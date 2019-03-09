# mos6502

A Interpreter/Disassembler for MOS 6502.

## status

Currently mainly supports Ricoh 2A03,and support for BCD is missing.

Can pass the test of [nestest.nes](http://nickmass.com/images/nestest.nes)

![test1](./doc/test1.jpg)
![test2](./doc/test2.jpg)

Need more test on non-nes target

Disassembler is incomplete. Currently only the execution log is displayed in the terminal (by enable `disasm` feature).

## no_std

This crate supports executing in `no_std`.