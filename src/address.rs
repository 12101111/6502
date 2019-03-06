// AddressMode
#[derive(Debug, Clone, Copy, PartialEq)]
#[allow(non_camel_case_types)]
pub enum AM {
    ///隐含寻址Implied,单字节指令
    IMP,
    ///Immediate 立即寻址,双字节指令 #v
    IMM,
    ///Accumulator 累加器A寻址,单字节指令 A
    ACC,
    ///Absolute 绝对寻址,三字节指令 a
    ABS,
    ///ZeroPage 零页寻址,双字节指令 d
    ZPG,
    ///AbsoluteX 绝对X变址,三字节指令 a,x
    ABX,
    ///AbsoluteX 绝对X变址,三字节指令 a,x 跨页时周期加1
    abx,
    ///AbsoluteY 绝对Y变址,三字节指令 a,y
    ABY,
    ///AbsoluteY 绝对Y变址,三字节指令 a,y 跨页时周期加1
    aby,
    ///Relative相对寻址,双字节指令 *+d
    REL,
    ///ZeroPageX零页X变址,双字节指令 d,x
    ZPX,
    ///ZeroPageY零页Y变址,双字节指令 d,y
    ZPY,
    ///ZeroPageIndexIndirectX零页间接X变址,双字节指令 (d,x)
    ZIX,
    ///ZeroPageIndexIndirectY零页间接Y变址,双字节指令 (d),y
    ZIY,
    ///ZeroPageIndexIndirectY零页间接Y变址,双字节指令 (d),y
    ziy,
    //AbsoluteIndexIndirect相对寻址,双字节指令 (a) 跨页时周期加1
    IND,
    // 未知
    NON,
}
use self::AM::*;

pub(crate) fn ins_size(am: &AM) -> u16 {
    match *am {
        IMP => 0,
        IMM => 1,
        ACC => 0,
        ABS => 2,
        ZPG => 1,
        ABX => 2,
        abx => 2,
        ABY => 2,
        aby => 2,
        REL => 1,
        ZPX => 1,
        ZPY => 1,
        ZIX => 1,
        ZIY => 1,
        ziy => 1,
        IND => 2,
        NON => 0,
    }
}
