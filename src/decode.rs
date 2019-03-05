use super::address::AM::{self, *};

#[derive(Debug, Clone, Copy)]
#[allow(non_camel_case_types)]
pub enum OP {
    /// *Load & Storw*
    /// 由存储器取数送入累加器A    M -> A
    LDA,
    /// 由存储器取数送入寄存器X    M -> X
    LDX,
    /// 由存储器取数送入寄存器Y    M -> Y
    LDY,
    /// 将累加器A的数送入存储器    A -> M
    STA,
    /// 将寄存器X的数送入存储器    X -> M
    STX,
    /// 将寄存器Y的数送入存储器    Y -> M
    STY,
    /// *transfer*
    /// 将累加器A的内容送入变址寄存器X
    TAX,
    /// 将累加器A的内容送入变址寄存器Y
    TAY,
    /// 将变址寄存器X的内容送入累加器A
    TXA,
    /// 将变址寄存器Y的内容送入累加器A
    TYA,
    /// 堆栈指针S的内容送入变址寄存器X
    TSX,
    /// 变址寄存器X的内容送入堆栈指针S
    TXS,
    /// *Stack*
    /// 累加器进栈
    PHA,
    /// 标志寄存器P进栈
    PHP,
    /// 累加器出栈
    PLA,
    /// 标志寄存器P出栈
    PLP,
    /// *Arithmetic*
    /// 累加器,存储器,进位标志C相加,结果送累加器A  A+M+C -> A
    ADC,
    /// 从累加器减去存储器和进位标志C取反,结果送累加器 A-M-(1-C) -> A
    SBC,
    /// *Inc & Dec*
    /// X寄存器+1 X+1 -> X
    INX,
    /// Y寄存器+1 Y+1 -> Y
    INY,
    /// X寄存器-1 X-1 -> X
    DEX,
    /// Y寄存器-1 Y-1 -> Y
    DEY,
    /// 存储器单元内容增1  M+1 -> M
    INC,
    /// 存储器单元内容减1  M-1 -> M
    DEC,
    /// *logical*
    /// 存储器与累加器相与,结果送累加器  A∧M -> A
    AND,
    /// 存储器与累加器相或,结果送累加器  A∨M -> A
    ORA,
    /// 存储器与累加器异或,结果送累加器  A≮M -> A
    EOR,
    /// 位测试
    BIT,
    /// 累加器和存储器比较
    CMP,
    /// 寄存器X的内容和存储器比较
    CPX,
    /// 寄存器Y的内容和存储器比较
    CPY,
    /// *bit shift*
    /// 算术左移 储存器
    ASL,
    /// 算术右移 储存器
    LSR,
    /// 循环算术左移 储存器
    ROL,
    /// 循环算术右移 储存器
    ROR,
    /// *flags*
    /// 清除进位标志C         0 -> C
    CLC,
    /// 清除十进标志D         0 -> D
    CLD,
    /// 清除中断禁止I         0 -> I
    CLI,
    /// 清除溢出标志V         0 -> V
    CLV,
    /// 设置进位标志C         1 -> C
    SEC,
    /// 设置十进标志D         1 -> D
    SED,
    /// 设置中断禁止V         1 -> I
    SEI,
    /// *Jump & Branch*
    /// 无条件跳转
    JMP,
    /// 跳转到子程序
    JSR,
    /// 返回到主程序
    RTS,
    /// 从中断返回
    RTI,
    /// 强制中断
    BRK,
    /// 如果标志位Z = 1则转移，否则继续
    BEQ,
    /// 如果标志位Z = 0则转移，否则继续
    BNE,
    /// 如果标志位C = 1则转移，否则继续
    BCS,
    /// 如果标志位C = 0则转移，否则继续
    BCC,
    /// 如果标志位V = 1则转移，否则继续
    BVS,
    /// 如果标志位V = 0则转移，否则继续
    BVC,
    /// 如果标志位N = 1则转移，否则继续
    BMI,
    /// 如果标志位N = 0则转移，否则继续
    BPL,
    /// 无操作
    NOP,
    /// *unoffical OpCode*
    /// AND+LSR,'And' then Logical Shift Right
    ALR,
    /// 'And' then copy N to C
    ANC,
    /// 'AND' then Rotate Right
    ARR,
    /// A 'And' X, then Subtract memory, to X
    AXS,
    /// Load to A and X
    LAX,
    /// Store A 'And' X
    SAX,
    /// DEC + CMP
    DCP,
    /// INC + SBC
    ISC,
    /// Rotate Left then 'And' - ROL + AND
    RLA,
    /// Rotate Right then Add with Carry - ROR + ADC
    RRA,
    /// Shift Left then 'Or' - ASL + ORA
    SLO,
    /// Shift Right then "Exclusive-Or" - LSR + EOR
    SRE,
    // TODO: Unimplemented Unoffical Opecode
    SHY,
    SHX,
    STP,
    XXA,
    AHX,
    TAS,
    LAS,
}
use self::OP::*;
#[rustfmt::skip]///(OPCODE,Address Mode,CPU Cycle)
pub static DECODE: [(OP, AM, u8); 256] = [
/*0x00*/(BRK, IMP, 7),(ORA, ZIX, 6),(STP, NON, 2),(SLO, ZIX, 8),(NOP, ZPG, 3),(ORA, ZPG, 3),(ASL, ZPG, 5),(SLO, ZPG, 5),
/*0x08*/(PHP, IMP, 3),(ORA, IMM, 2),(ASL, ACC, 2),(ANC, IMM, 2),(NOP, ABS, 4),(ORA, ABS, 4),(ASL, ABS, 6),(SLO, ABS, 6),
/*0x10*/(BPL, REL, 2),(ORA, ZIY, 5),(STP, NON, 2),(SLO, ZIY, 8),(NOP, ZPX, 4),(ORA, ZPX, 4),(ASL, ZPX, 6),(SLO, ZPX, 6),
/*0x18*/(CLC, IMP, 2),(ORA, aby, 4),(NOP, IMP, 2),(SLO, ABY, 7),(NOP, abx, 4),(ORA, abx, 4),(ASL, ABX, 7),(SLO, ABX, 7),
/*0x20*/(JSR, ABS, 6),(AND, ZIX, 6),(STP, NON, 2),(RLA, ZIX, 8),(BIT, ZPG, 3),(AND, ZPG, 3),(ROL, ZPG, 5),(RLA, ZPG, 5),
/*0x28*/(PLP, IMP, 4),(AND, IMM, 2),(ROL, ACC, 2),(ANC, IMM, 2),(BIT, ABS, 4),(AND, ABS, 4),(ROL, ABS, 6),(RLA, ABS, 6),
/*0x30*/(BMI, REL, 2),(AND, ZIY, 5),(STP, NON, 2),(RLA, ZIY, 8),(NOP, ZPX, 4),(AND, ZPX, 4),(ROL, ZPX, 6),(RLA, ZPX, 6),
/*0x38*/(SEC, IMP, 2),(AND, aby, 4),(NOP, IMP, 2),(RLA, ABY, 7),(NOP, abx, 4),(AND, abx, 4),(ROL, ABX, 7),(RLA, ABX, 7),
/*0x40*/(RTI, IMP, 6),(EOR, ZIX, 6),(STP, NON, 2),(SRE, ZIX, 8),(NOP, ZPG, 3),(EOR, ZPG, 3),(LSR, ZPG, 5),(SRE, ZPG, 5),
/*0x48*/(PHA, IMP, 3),(EOR, IMM, 2),(LSR, ACC, 2),(ALR, IMM, 2),(JMP, ABS, 3),(EOR, ABS, 4),(LSR, ABS, 6),(SRE, ABS, 6),
/*0x50*/(BVC, REL, 2),(EOR, ZIY, 5),(STP, NON, 2),(SRE, ZIY, 8),(NOP, ZPX, 4),(EOR, ZPX, 4),(LSR, ZPX, 6),(SRE, ZPX, 6),
/*0x58*/(CLI, IMP, 2),(EOR, aby, 4),(NOP, IMP, 2),(SRE, ABY, 7),(NOP, abx, 4),(EOR, abx, 4),(LSR, ABX, 7),(SRE, ABX, 7),
/*0x60*/(RTS, IMP, 6),(ADC, ZIX, 6),(STP, NON, 2),(RRA, ZIX, 8),(NOP, ZPG, 3),(ADC, ZPG, 3),(ROR, ZPG, 5),(RRA, ZPG, 5),
/*0x68*/(PLA, IMP, 4),(ADC, IMM, 2),(ROR, ACC, 2),(ARR, IMM, 2),(JMP, IND, 5),(ADC, ABS, 4),(ROR, ABS, 6),(RRA, ABS, 6),
/*0x70*/(BVS, REL, 2),(ADC, ziy, 5),(STP, NON, 2),(RRA, ZIY, 8),(NOP, ZPX, 4),(ADC, ZPX, 4),(ROR, ZPX, 6),(RRA, ZPX, 6),
/*0x78*/(SEI, IMP, 2),(ADC, aby, 4),(NOP, IMP, 2),(RRA, ABY, 7),(NOP, abx, 4),(ADC, abx, 4),(ROR, ABX, 7),(RRA, ABX, 7),
/*0x80*/(NOP, IMM, 2),(STA, ZIX, 6),(NOP, IMM, 2),(SAX, ZIX, 6),(STY, ZPG, 3),(STA, ZPG, 3),(STX, ZPG, 3),(SAX, ZPG, 3),
/*0x88*/(DEY, IMP, 2),(NOP, IMM, 2),(TXA, IMP, 2),(XXA, IMM, 2),(STY, ABS, 4),(STA, ABS, 4),(STX, ABS, 4),(SAX, ABS, 4),
/*0x90*/(BCC, REL, 2),(STA, ZIY, 6),(STP, NON, 2),(AHX, ZIY, 6),(STY, ZPX, 4),(STA, ZPX, 4),(STX, ZPY, 4),(SAX, ZPY, 4),
/*0x98*/(TYA, IMP, 2),(STA, ABY, 5),(TXS, IMP, 2),(TAS, ABY, 5),(SHY, ABX, 5),(STA, ABX, 5),(SHX, ABY, 5),(AHX, ABY, 5),
/*0xA0*/(LDY, IMM, 2),(LDA, ZIX, 6),(LDX, IMM, 2),(LAX, ZIX, 6),(LDY, ZPG, 3),(LDA, ZPG, 3),(LDX, ZPG, 3),(LAX, ZPG, 3),
/*0xA8*/(TAY, IMP, 2),(LDA, IMM, 2),(TAX, IMP, 2),(LAX, IMM, 2),(LDY, ABS, 4),(LDA, ABS, 4),(LDX, ABS, 4),(LAX, ABS, 4),
/*0xB0*/(BCS, REL, 2),(LDA, ziy, 5),(STP, NON, 2),(LAX, ziy, 5),(LDY, ZPX, 4),(LDA, ZPX, 4),(LDX, ZPY, 4),(LAX, ZPY, 4),
/*0xB8*/(CLV, IMP, 2),(LDA, aby, 4),(TSX, IMP, 2),(LAS, ABY, 4),(LDY, abx, 4),(LDA, abx, 4),(LDX, aby, 4),(LAX, aby, 4),
/*0xC0*/(CPY, IMM, 2),(CMP, ZIX, 6),(NOP, IMM, 2),(DCP, ZIX, 8),(CPY, ZPG, 3),(CMP, ZPG, 3),(DEC, ZPG, 5),(DCP, ZPG, 5),
/*0xC8*/(INY, IMP, 2),(CMP, IMM, 2),(DEX, IMP, 2),(AXS, IMM, 2),(CPY, ABS, 4),(CMP, ABS, 4),(DEC, ABS, 6),(DCP, ABS, 6),
/*0xD0*/(BNE, REL, 2),(CMP, ziy, 5),(STP, NON, 2),(DCP, ZIY, 8),(NOP, ZPX, 4),(CMP, ZPX, 4),(DEC, ZPX, 6),(DCP, ZPX, 6),
/*0xD8*/(CLD, IMP, 2),(CMP, aby, 4),(NOP, IMP, 2),(DCP, ABY, 7),(NOP, abx, 4),(CMP, abx, 4),(DEC, ABX, 7),(DCP, ABX, 7),
/*0xE0*/(CPX, IMM, 2),(SBC, ZIX, 6),(NOP, IMM, 3),(ISC, ZIX, 8),(CPX, ZPG, 3),(SBC, ZPG, 3),(INC, ZPG, 5),(ISC, ZPG, 5),
/*0xE8*/(INX, IMP, 2),(SBC, IMM, 2),(NOP, IMP, 2),(SBC, IMM, 2),(CPX, ABS, 4),(SBC, ABS, 4),(INC, ABS, 6),(ISC, ABS, 6),
/*0xF0*/(BEQ, REL, 2),(SBC, ZIY, 5),(STP, NON, 2),(ISC, ZIY, 8),(NOP, ZPX, 4),(SBC, ZPX, 4),(INC, ZPX, 6),(ISC, ZPX, 6),
/*0xF8*/(SED, IMP, 2),(SBC, aby, 4),(NOP, IMP, 2),(ISC, ABY, 7),(NOP, abx, 4),(SBC, abx, 4),(INC, ABX, 7),(ISC, ABX, 7),
];
