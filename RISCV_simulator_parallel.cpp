#include <iostream>
#include <cstdio>
#include <typeinfo>
using namespace std;

enum Stage {IF, ID, EX, MEM, WB};

bool stall;
bool stall_lock;
bool ret_flag;

unsigned sgnext(unsigned imm, int hi) {
    if (imm & (1 << hi))
        imm |= 0xFFFFFFFF >> hi << hi;
    return imm;
}

class Memory {
private:
    unsigned char storage[0x400000];
public:
    void init() {
        unsigned addr = 0;
        while (!cin.eof()) {
            unsigned byte;
            while (cin >> hex >> byte) {
                storage[addr] = byte;
                ++addr;
            }
            cin.clear();
            cin.get();
            cin >> addr;
        }
    }
    int read_dword(unsigned addr) {
        int ret;
        for (int i = 3; i >= 0; --i)
            ret = (ret << 8) + storage[addr + i];
        return ret;
    }
    unsigned read_word(unsigned addr) {
        unsigned ret;
        for (int i = 1; i >= 0; --i)
            ret = (ret << 8) + storage[addr + i];
        return ret;
    }
    unsigned read(unsigned addr) {
        return storage[addr];
    }
    void write_dword(unsigned addr, unsigned data) {
        for (int i = 0; i < 4; ++i) {
            storage[addr + i] = data;
            data >>= 8;
        }
    }
    void write_word(unsigned addr, unsigned data) {
        for (int i = 0; i < 2; ++i) {
            storage[addr + i] = data;
            data >>= 8;
        }
    }
    void write(unsigned addr, unsigned data) {
        storage[addr] = data;
    }
};

Memory mem;

template <typename DataType>
class Register {
private:
    DataType input;
    DataType output;
    bool zero;
    bool lts;
public:
    bool lock;
    Register():
        zero(false), lts(false), lock(false) {}
    DataType read() {
        if (zero)
            return 0;
        return output;
    }
    DataType read_input() {
        return input;
    }
    void write(DataType data) {
        if (!lock)
            input = data;
    }
    void write_sel(DataType data) {
        input = data;
        lock = true;
    }
    void update() {
        if (!lts || !stall)
            output = input;
        lock = false;
    }
    void set_zero() {
        zero = true;
    }
    void set_lts() {
        lts = true;
    }
};

Register<unsigned> reg[32];
Register<unsigned> pc;
Register<unsigned> IFID_pc;
Register<unsigned> IFID_predict;
Register<unsigned> IDEX_pc;
Register<unsigned> IDEX_predict;

class Inst {
public:
    virtual void inst_fetch() {
        unsigned addr = pc.read();
        pc.write(addr + 4);
        IFID_predict.write(addr + 4);
    }
    virtual void inst_decode() {}
    virtual void execute() {}
    virtual void mem_access() {}
    virtual void write_back() {}
    virtual bool fwd(Stage stage, unsigned src, Register<unsigned> & reg) {}
    void bubble();
    virtual ~Inst() {}
    static Inst * parse(unsigned code);
};

Register<Inst *> IFID_inst;
Register<Inst *> IDEX_inst;
Register<Inst *> EXMEM_inst;
Register<Inst *> MEMWB_inst;

Register<unsigned> IDEX_rval1, IDEX_rval2;
Register<unsigned> EXMEM_data;
Register<unsigned> EXMEM_addr;
Register<unsigned> MEMWB_data;

class OneSrcInst: public Inst {
protected:
    unsigned src;
    void get_fwd() {
        stall = false;
        if (src == 0) {
            IDEX_rval1.write(0);
            return;
        }
        bool fwd_flag = IDEX_inst.read()->fwd(EX, src, IDEX_rval1);
        if (!fwd_flag)
            fwd_flag = EXMEM_inst.read()->fwd(MEM, src, IDEX_rval1);
        if (!fwd_flag)
            MEMWB_inst.read()->fwd(WB, src, IDEX_rval1);
    }
};

class TwoSrcInst: public Inst {
protected:
    unsigned src1, src2;
    void get_fwd() {
        stall = false;
        if (src1 == 0) {
            IDEX_rval1.write(0);
        } else {
            bool fwd_flag =  IDEX_inst.read()->fwd(EX, src1, IDEX_rval1);
            if (!fwd_flag)
                fwd_flag = EXMEM_inst.read()->fwd(MEM, src1, IDEX_rval1);
            if (!fwd_flag)
                MEMWB_inst.read()->fwd(WB, src1, IDEX_rval1);
        }
        if (src2 == 0) {
            IDEX_rval2.write(0);
        } else {
            bool fwd_flag = IDEX_inst.read()->fwd(EX, src2, IDEX_rval2);
            if (!fwd_flag)
                fwd_flag = EXMEM_inst.read()->fwd(MEM, src2, IDEX_rval2);
            if (!fwd_flag)
                MEMWB_inst.read()->fwd(WB, src2, IDEX_rval2);
        }
    }
};

class RTypeInst: public TwoSrcInst {
protected:
    unsigned dest;
public:
    static Inst * parse(unsigned code);
    void inst_decode() {
        IDEX_rval1.write(reg[src1].read());
        IDEX_rval2.write(reg[src2].read());
        get_fwd();
    }
    void mem_access() {
        MEMWB_data.write(EXMEM_data.read());
    }
    void write_back() {
        reg[dest].write(MEMWB_data.read());
        reg[dest].update();
    }
    bool fwd(Stage stage, unsigned src, Register<unsigned> & reg) {
        if (src == dest) {
            switch (stage) {
                case EX: reg.write(EXMEM_data.read_input()); break;
                case MEM: reg.write(EXMEM_data.read()); break;
                case WB: reg.write(MEMWB_data.read()); break;
                default: break;
            }
            return true;
        }
        return false;
    }
    void set(unsigned src1, unsigned src2, unsigned dest) {
        this->src1 = src1;
        this->src2 = src2;
        this->dest = dest;
    }
};

class ADD: public RTypeInst {
public:
    void execute() {
        EXMEM_data.write(IDEX_rval1.read() + IDEX_rval2.read());
    }
};

class SUB: public RTypeInst {
public:
    void execute() {
        EXMEM_data.write(IDEX_rval1.read() - IDEX_rval2.read());
    }
};

class SLL: public RTypeInst {
public:
    void execute() {
        EXMEM_data.write(IDEX_rval1.read() << (IDEX_rval2.read() & 0x1F));
    }
};

class SLT: public RTypeInst {
public:
    void execute() {
        EXMEM_data.write((int) IDEX_rval1.read() < (int) IDEX_rval2.read());
    }
};

class SLTU: public RTypeInst {
public:
    void execute() {
        EXMEM_data.write(IDEX_rval1.read() < IDEX_rval2.read());
    }
};

class XOR: public RTypeInst {
public:
    void execute() {
        EXMEM_data.write(IDEX_rval1.read() ^ IDEX_rval2.read());
    }
};

class SRL: public RTypeInst {
public:
    void execute() {
        EXMEM_data.write(IDEX_rval1.read() >> (IDEX_rval2.read() & 0x1F));
    }
};

class SRA: public RTypeInst {
public:
    void execute() {
        EXMEM_data.write((int) IDEX_rval1.read() >> (IDEX_rval2.read() & 0x1F));
    }
};

class OR: public RTypeInst {
public:
    void execute() {
        EXMEM_data.write(IDEX_rval1.read() | IDEX_rval2.read());
    }
};

class AND: public RTypeInst {
public:
    void execute() {
        EXMEM_data.write(IDEX_rval1.read() & IDEX_rval2.read());
    }
};

class ITypeInst: public OneSrcInst {
protected:
    unsigned imm;
    unsigned dest, shamt;
public:
    static Inst * parse(unsigned code);
    void inst_decode() {
        IDEX_rval1.write(reg[src].read());
        get_fwd();
    }
    bool fwd(Stage stage, unsigned src, Register<unsigned> & reg) {
        if (src == dest) {
            switch (stage) {
                case EX: reg.write(EXMEM_data.read_input()); break;
                case MEM: reg.write(EXMEM_data.read()); break;
                case WB: reg.write(MEMWB_data.read()); break;
                default: break;
            }
            return true;
        }
        return false;
    }
    void set(unsigned imm, unsigned src, unsigned dest, unsigned shamt) {
        this->imm = imm;
        this->src = src;
        this->dest = dest;
        this->shamt = shamt;
    }
};

class JALR: public ITypeInst {
public:
    void execute() {
        pc.write_sel((IDEX_rval1.read() + imm) & 0xFFFFFFFE);
        bubble();
        EXMEM_data.write(IDEX_pc.read() + 4);
    }
    void mem_access() {
        MEMWB_data.write(EXMEM_data.read());
    }
    void write_back() {
        reg[dest].write(MEMWB_data.read());
        reg[dest].update();
    }
};

class ITypeCalcInst: public ITypeInst {
public:
    void mem_access() {
        MEMWB_data.write(EXMEM_data.read());
    }
    void write_back() {
        reg[dest].write(MEMWB_data.read());
        reg[dest].update();
    }
};

class ADDI: public ITypeCalcInst {
public:
    void execute() {
        EXMEM_data.write(IDEX_rval1.read() + imm);
    }
};

class NOP: public ADDI {
public:
    NOP() {
        set(0, 0, 0, 0);
    }
    bool fwd(Stage stage, unsigned src, Register<unsigned> & reg) {
        return false;
    }
};

void Inst::bubble() {
    IFID_inst.write_sel(new NOP);
    IDEX_inst.write_sel(new NOP);
    stall = false;
    stall_lock = true;
}

class SLTI: public ITypeCalcInst {
public:
    void execute() {
        EXMEM_data.write((int) IDEX_rval1.read() < (int) imm);
    }
};

class SLTIU: public ITypeCalcInst {
public:
    void execute() {
        EXMEM_data.write(IDEX_rval1.read() < imm);
    }
};

class XORI: public ITypeCalcInst {
public:
    void execute() {
        EXMEM_data.write(IDEX_rval1.read() ^ imm);
    }
};

class ORI: public ITypeCalcInst {
public:
    void execute() {
        EXMEM_data.write(IDEX_rval1.read() | imm);
    }
};

class ANDI: public ITypeCalcInst {
public:
    void execute() {
        EXMEM_data.write(IDEX_rval1.read() & imm);
    }
};

class SLLI: public ITypeCalcInst {
public:
    void execute() {
        EXMEM_data.write(IDEX_rval1.read() << shamt);
    }
};

class SRLI: public ITypeCalcInst {
public:
    void execute() {
        EXMEM_data.write(IDEX_rval1.read() >> shamt);
    }
};

class SRAI: public ITypeCalcInst {
public:
    void execute() {
        EXMEM_data.write((int) IDEX_rval1.read() >> shamt);
    }
};

class LoadInst: public ITypeInst {
public:
    void execute() {
        EXMEM_addr.write(IDEX_rval1.read() + imm);
    }
    void write_back() {
        reg[dest].write(MEMWB_data.read());
        reg[dest].update();
    }
    bool fwd(Stage stage, unsigned src, Register<unsigned> & reg) {
        if (src == dest) {
            switch (stage) {
                case EX:
                    if (!stall_lock)
                        stall = true;
                    IDEX_inst.write_sel(new NOP);
                    break;
                case MEM: reg.write(MEMWB_data.read_input()); break;
                case WB: reg.write(MEMWB_data.read()); break;
                default: break;
            }
            return true;
        }
        return false;
    }
};

class LB: public LoadInst {
public:
    void mem_access() {
        MEMWB_data.write(sgnext(mem.read(EXMEM_addr.read()), 7));
    }
};

class LH: public LoadInst {
public:
    void mem_access() {
        MEMWB_data.write(sgnext(mem.read_word(EXMEM_addr.read()), 15));
    }
};

class LW: public LoadInst {
public:
    void mem_access() {
        MEMWB_data.write(mem.read_dword(EXMEM_addr.read()));
    }
};

class LBU: public LoadInst {
public:
    void mem_access() {
        MEMWB_data.write(mem.read(EXMEM_addr.read()));
    }
};

class LHU: public LoadInst {
public:
    void mem_access() {
        MEMWB_data.write(mem.read_word(EXMEM_addr.read()));
    }
};

class STypeInst: public TwoSrcInst {
protected:
    unsigned imm;
public:
    static Inst * parse(unsigned code);
    void inst_decode() {
        IDEX_rval1.write(reg[src1].read());
        IDEX_rval2.write(reg[src2].read());
        get_fwd();
    }
    void execute() {
        EXMEM_addr.write(IDEX_rval1.read() + imm);
        EXMEM_data.write(IDEX_rval2.read());
        if (EXMEM_addr.read_input() == 0x30004)
            ret_flag = true;
    }
    void set(unsigned imm, unsigned src1, unsigned src2) {
        this->imm = imm;
        this->src1 = src1;
        this->src2 = src2;
    }
};

class SB: public STypeInst {
public:
    void mem_access() {
        mem.write(EXMEM_addr.read(), EXMEM_data.read());
    }
};

class SH: public STypeInst {
public:
    void mem_access() {
        mem.write_word(EXMEM_addr.read(), EXMEM_data.read());
    }
};

class SW: public STypeInst {
public:
    void mem_access() {
        mem.write_dword(EXMEM_addr.read(), EXMEM_data.read());
    }
};

class BTypeInst: public TwoSrcInst {
protected:
    unsigned imm;
    virtual bool judge(unsigned lhs, unsigned rhs) {
        return true;
    }
public:
    static Inst * parse(unsigned code);
    void inst_decode() {
        IDEX_rval1.write(reg[src1].read());
        IDEX_rval2.write(reg[src2].read());
        get_fwd();
    }
    void set(unsigned imm, unsigned src1, unsigned src2) {
        this->imm = imm;
        this->src1 = src1;
        this->src2 = src2;
    }
    void execute() {
        unsigned next_pc;
        if (judge(IDEX_rval1.read(), IDEX_rval2.read()))
            next_pc = IDEX_pc.read() + imm;
        else
            next_pc = IDEX_pc.read() + 4;
        if (IDEX_predict.read() != next_pc) {
            pc.write_sel(next_pc);
            bubble();
        }
    }
};

class BEQ: public BTypeInst {
private:
    bool judge(unsigned lhs, unsigned rhs) {
        return lhs == rhs;
    }
};

class BNE: public BTypeInst {
private:
    bool judge(unsigned lhs, unsigned rhs) {
        return lhs != rhs;
    }
};

class BLT: public BTypeInst {
private:
    bool judge(unsigned lhs, unsigned rhs) {
        return (int) lhs < (int) rhs;
    }
};

class BGE: public BTypeInst {
private:
    bool judge(unsigned lhs, unsigned rhs) {
        return (int) lhs >= (int) rhs;
    }
};

class BLTU: public BTypeInst {
private:
    bool judge(unsigned lhs, unsigned rhs) {
        return lhs < rhs;
    }
};

class BGEU: public BTypeInst {
private:
    bool judge(unsigned lhs, unsigned rhs) {
        return lhs >= rhs;
    }
};

class UTypeInst: public Inst {
protected:
    unsigned imm;
    unsigned dest;
public:
    static Inst * parse(unsigned code);
    void set(unsigned imm, unsigned dest) {
        this->imm = imm;
        this->dest = dest;
    }
};

class LUI: public UTypeInst {
public:
    void write_back() {
        reg[dest].write(imm);
        reg[dest].update();
    }
    bool fwd(Stage stage, unsigned src, Register<unsigned> & reg) {
        if (src == dest) {
            reg.write(imm);
            return true;
        }
        return false;
    }
};

class AUIPC: public UTypeInst {
public:
    void execute() {
        EXMEM_data.write(IDEX_pc.read() + imm);
    }
    void mem_access() {
        MEMWB_data.write(EXMEM_data.read());
    }
    void write_back() {
        reg[dest].write(MEMWB_data.read());
        reg[dest].update();
    }
    bool fwd(Stage stage, unsigned src, Register<unsigned> & reg) {
        if (src == dest) {
            switch (stage) {
                case EX: reg.write(EXMEM_data.read_input()); break;
                case MEM: reg.write(EXMEM_data.read()); break;
                case WB: reg.write(MEMWB_data.read()); break;
                default: break;
            }
            return true;
        }
        return false;
    }
};

class JTypeInst: public Inst {
protected:
    unsigned imm;
    unsigned dest;
public:
    static Inst * parse(unsigned code);
    void set(unsigned imm, unsigned dest) {
        this->imm = imm;
        this->dest = dest;
    }
    bool fwd(Stage stage, unsigned src, Register<unsigned> & reg) {
        if (src == dest) {
            switch (stage) {
                case EX: reg.write(EXMEM_data.read_input()); break;
                case MEM: reg.write(EXMEM_data.read()); break;
                case WB: reg.write(MEMWB_data.read()); break;
                default: break;
            }
            return true;
        }
        return false;
    }
};

class JAL: public JTypeInst {
public:
    void inst_fetch() {
        unsigned addr = pc.read();
        pc.write(addr + imm);
        IFID_predict.write(addr + 4);
    }
    void execute() {
        EXMEM_data.write(IDEX_pc.read() + 4);
    }
    void mem_access() {
        MEMWB_data.write(EXMEM_data.read());
    }
    void write_back() {
        reg[dest].write(MEMWB_data.read());
        reg[dest].update();
    }
};

Inst * Inst::parse(unsigned code) {
    unsigned opcode = code & 0x7F;
    if (opcode == 0x33)
        return RTypeInst::parse(code);
    else if (opcode == 0x67 || opcode == 0x3 || opcode == 0x13)
        return ITypeInst::parse(code);
    else if (opcode == 0x23)
        return STypeInst::parse(code);
    else if (opcode == 0x63)
        return BTypeInst::parse(code);
    else if (opcode == 0x37 || opcode == 0x17)
        return UTypeInst::parse(code);
    else if (opcode = 0x6F)
        return JTypeInst::parse(code);
}

Inst * RTypeInst::parse(unsigned code) {
    RTypeInst * ret;
    unsigned funct3 = code >> 12 & 0x7;
    unsigned funct7 = code >> 25;
    if (funct3 == 0 && funct7 == 0)
        ret = new ADD;
    else if (funct3 == 0 && funct7 == 0x20)
        ret = new SUB;
    else if (funct3 == 0x1)
        ret = new SLL;
    else if (funct3 == 0x2)
        ret = new SLT;
    else if (funct3 == 0x3)
        ret = new SLTU;
    else if (funct3 == 0x4)
        ret = new XOR;
    else if (funct3 == 0x5 && funct7 == 0)
        ret = new SRL;
    else if (funct3 == 0x5 && funct7 == 0x20)
        ret = new SRA;
    else if (funct3 == 0x6)
        ret = new OR;
    else if (funct3 == 0x7)
        ret = new AND;
    unsigned src1 = code >> 15 & 0x1F;
    unsigned src2 = code >> 20 & 0x1F;
    unsigned dest = code >> 7 & 0x1F;
    ret->set(src1, src2, dest);
    return (Inst *) ret;
}

Inst * ITypeInst::parse(unsigned code) {
    ITypeInst * ret;
    unsigned opcode = code & 0x7F;
    unsigned funct3 = code >> 12 & 0x7;
    unsigned funct7 = code >> 25;
    if (opcode == 0x67)
        ret = new JALR;
    else if (opcode == 0x3) {
        if (funct3 == 0)
            ret = new LB;
        else if (funct3 == 0x1)
            ret = new LH;
        else if (funct3 == 0x2)
            ret = new LW;
        else if (funct3 == 0x4)
            ret = new LBU;
        else if (funct3 == 0x5)
            ret = new LHU;
    } else if (opcode == 0x13) {
        if (funct3 == 0)
            ret = new ADDI;
        else if (funct3 == 0x2)
            ret = new SLTI;
        else if (funct3 == 0x3)
            ret = new SLTIU;
        else if (funct3 == 0x4)
            ret = new XORI;
        else if (funct3 == 0x6)
            ret = new ORI;
        else if (funct3 == 0x7)
            ret = new ANDI;
        else if (funct3 == 0x1)
            ret = new SLLI;
        else if (funct3 == 0x5 && funct7 == 0)
            ret = new SRLI;
        else if (funct3 == 0x5 && funct7 == 0x20)
            ret = new SRAI;
    }
    unsigned imm = sgnext(code >> 20, 11);
    unsigned src = code >> 15 & 0x1F;
    unsigned dest = code >> 7 & 0x1F;
    unsigned shamt = imm & 0x1F;
    ret->set(imm, src, dest, shamt);
    return (Inst *) ret;
}

Inst * STypeInst::parse(unsigned code) {
    STypeInst * ret;
    unsigned funct3 = code >> 12 & 0x7;
    if (funct3 == 0)
        ret = new SB;
    else if (funct3 == 0x1)
        ret = new SH;
    else if (funct3 == 0x2)
        ret = new SW;
    unsigned imm = sgnext((code >> 7 & 0x1F) + ((code >> 25 & 0x7F) << 5), 11);
    unsigned src1 = code >> 15 & 0x1F;
    unsigned src2 = code >> 20 & 0x1F;
    ret->set(imm, src1, src2);
    return (Inst *) ret;
}

Inst * BTypeInst::parse(unsigned code) {
    BTypeInst * ret;
    unsigned funct3 = code >> 12 & 0x7;
    if (funct3 == 0)
        ret = new BEQ;
    else if (funct3 == 0x1)
        ret = new BNE;
    else if (funct3 == 0x4)
        ret = new BLT;
    else if (funct3 == 0x5)
        ret = new BGE;
    else if (funct3 == 0x6)
        ret = new BLTU;
    else if (funct3 == 0x7)
        ret = new BGEU;
    unsigned imm = sgnext(((code >> 8 & 0xF) << 1) + ((code >> 25 & 0x3F) << 5) +
        ((code >> 7 & 0x1) << 11) + ((code >> 31 & 1) << 12), 12);
    unsigned src1 = code >> 15 & 0x1F;
    unsigned src2 = code >> 20 & 0x1F;
    ret->set(imm, src1, src2);
    return (Inst *) ret;
}

Inst * UTypeInst::parse(unsigned code) {
    UTypeInst * ret;
    unsigned opcode = code & 0x7F;
    if (opcode == 0x37)
        ret = new LUI;
    else if (opcode == 0x17)
        ret = new AUIPC;
    unsigned imm = code & 0xFFFFF000;
    unsigned dest = code >> 7 & 0x1F;
    ret->set(imm, dest);
    return (Inst *) ret;
}

Inst * JTypeInst::parse(unsigned code) {
    JTypeInst * ret = new JAL;
    unsigned imm = sgnext(((code >> 21 & 0x3FF) << 1) + ((code >> 20 & 0x1) << 11) +
        ((code >> 12 & 0xFF) << 12) + ((code >> 31 & 0x1) << 20), 20);
    unsigned dest = code >> 7 & 0x1F;
    ret->set(imm, dest);
    return (Inst *) ret;
}

class InstFetch {
public:
    void work() {
        unsigned addr = pc.read();
        // cout << hex << addr << " ";
        IFID_pc.write(addr);
        unsigned code = mem.read_dword(addr);
        Inst * inst = Inst::parse(code);
        // cout << "IF: " << typeid(*inst).name() << " ";
        inst->inst_fetch();
        if (!IFID_inst.lock && !stall)
            IFID_inst.write(inst);
        else
            delete inst;
    }
};

class InstDecode {
public:
    void work() {
        Inst * inst = IFID_inst.read();
        // cout << "ID: " << typeid(*inst).name() << " ";
        inst->inst_decode();
        if (!IDEX_inst.lock)
            IDEX_inst.write(inst);
        else if (!stall)
            delete inst;
        IDEX_pc.write(IFID_pc.read());
        IDEX_predict.write(IFID_predict.read());
    }
};

class Execute {
public:
    void work() {
        Inst * inst = IDEX_inst.read();
        // cout << "EX: " << typeid(*inst).name() << " ";
        inst->execute();
        EXMEM_inst.write(inst);
    }
};

class MemoryAccess {
public:
    void work() {
        Inst * inst = EXMEM_inst.read();
        // cout << "MEM: " << typeid(*inst).name() << " ";
        inst->mem_access();
        MEMWB_inst.write(inst);
    }
};

class WriteBack {
public:
    void work() {
        Inst * inst = MEMWB_inst.read();
        // cout << "WB: " << typeid(*inst).name() << endl;;
        inst->write_back();
        delete inst;
    }
};

InstFetch inst_fetch;
InstDecode inst_decode;
Execute execute;
MemoryAccess mem_access;
WriteBack write_back;

void upd_reg() {
    // for (int i = 0; i < 32; ++i)
    //     reg[i].update();
    pc.update();
    IFID_inst.update();
    IFID_pc.update();
    IFID_predict.update();
    IDEX_inst.update();
    IDEX_pc.update();
    IDEX_predict.update();
    IDEX_rval1.update();
    IDEX_rval2.update();
    EXMEM_addr.update();
    EXMEM_data.update();
    EXMEM_inst.update();
    MEMWB_data.update();
    MEMWB_inst.update();
}

void work() {
    execute.work();
    mem_access.work();
    inst_decode.work();
    inst_fetch.work();
    write_back.work();
}

int main() {
    // freopen("test.data", "r", stdin);
    // freopen("check2", "w", stdout);

    mem.init();
    reg[0].set_zero();
    pc.set_lts();
    IFID_inst.set_lts();
    IFID_pc.set_lts();
    IFID_predict.set_lts();
    pc.write(0);
    IFID_inst.write_sel(new NOP);
    IDEX_inst.write_sel(new NOP);
    EXMEM_inst.write_sel(new NOP);
    MEMWB_inst.write_sel(new NOP);
    stall = false;
    ret_flag = false;

    while (!ret_flag) {
        stall_lock = false;
        upd_reg();
        work();
    }
    stall_lock = false;
    upd_reg();
    work();
    upd_reg();
    cout << dec << (reg[10].read() & 0xFF) << endl;
    delete IFID_inst.read();
    delete IDEX_inst.read();
    delete EXMEM_inst.read();
    delete MEMWB_inst.read();
    
    return 0;
}