// This file is part of AsmJit project <https://asmjit.com>
//
// See asmjit.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#include "../core/api-build_p.h"
#if !defined(ASMJIT_NO_AARCH64)

#include "../core/codeholder.h"
#include "../core/support.h"
#include "../arm/a64instdb_p.h"
#include "../arm/a64operand.h"

ASMJIT_BEGIN_SUB_NAMESPACE(a64)

namespace InstDB {

// a64::InstDB - InstInfoTable
// ===========================

// Defines an ARM/AArch64 instruction.
#define INST(id, opcodeEncoding, opcodeData, rwInfoIndex, flags, opcodeDataIndex) { \
  uint32_t(kEncoding##opcodeEncoding),      \
  uint32_t(opcodeDataIndex),                \
  0,                                        \
  uint16_t(rwInfoIndex),                    \
  uint16_t(flags)                           \
}

#define F(flag) kInstFlag##flag

// TODO: [ARM] Missing Instructions:
/*
BLRAA, BLRAAZ, BLRAB, BLRABZ: Branch with Link to Register, with pointer authentication.
BRAA, BRAAZ, BRAB, BRABZ: Branch to Register, with pointer authentication.

CFP: Control Flow Prediction Restriction by Context: an alias of SYS.
CPP: Cache Prefetch Prediction Restriction by Context: an alias of SYS.
DVP: Data Value Prediction Restriction by Context: an alias of SYS.
PSB CSYNC: Profiling Synchronization Barrier.

ERETAA, ERETAB: Exception Return, with pointer authentication.
LDAPxxx
PACIA, PACIA1716, PACIASP, PACIAZ, PACIZA: Pointer Authentication Code for Instruction address, using key A.
PACIB, PACIB1716, PACIBSP, PACIBZ, PACIZB: Pointer Authentication Code for Instruction address, using key B.
PRFM (immediate): Prefetch Memory (immediate).
PRFM (literal): Prefetch Memory (literal).
PRFM (register): Prefetch Memory (register).
PRFUM: Prefetch Memory (unscaled offset).
RETAA, RETAB: Return from subroutine, with pointer authentication.
RMIF: Rotate, Mask Insert Flags.
SYSL
IRG: Insert Random Tag.
INST_(Irg              , BaseRRR            , (0b1001101011000000000100, kX , kSP, kX , kSP, kX , kZR, true)                        , kRWI_W    , 0                         , 0  , 1   ), // #1
*/
const InstInfo _instInfoTable[] = {
  // +------------------+---------------------+--------------------------------------------------------------------------------------+-----------+---------------------------+----+
  // | Instruction Id   | Encoding            | Opcode Data                                                                          | RW Info   | Instruction Flags         |DatX|
  // +------------------+---------------------+--------------------------------------------------------------------------------------+-----------+---------------------------+----+
  // ${InstInfo:Begin}
  INST(None             , None               , (_)                                                                                   , 0         , 0                         , 0  ), // #0
  INST(Adc              , BaseRRR            , (0b0001101000000000000000, kWX, kZR, kWX, kZR, kWX, kZR, true)                        , kRWI_W    , 0                         , 0  ), // #1
  INST(Adcs             , BaseRRR            , (0b0011101000000000000000, kWX, kZR, kWX, kZR, kWX, kZR, true)                        , kRWI_W    , 0                         , 1  ), // #2
  INST(Add              , BaseAddSub         , (0b0001011000, 0b0001011001, 0b0010001)                                               , kRWI_W    , 0                         , 0  ), // #3
  INST(Addg             , BaseRRII           , (0b1001000110000000000000, kX, kSP, kX, kSP, 6, 4, 16, 4, 0, 10)                      , kRWI_W    , 0                         , 0  ), // #4
  INST(Adds             , BaseAddSub         , (0b0101011000, 0b0101011001, 0b0110001)                                               , kRWI_W    , 0                         , 1  ), // #5
  INST(Adr              , BaseAdr            , (0b0001000000000000000000, OffsetType::kAArch64_ADR)                                  , kRWI_W    , 0                         , 0  ), // #6
  INST(Adrp             , BaseAdr            , (0b1001000000000000000000, OffsetType::kAArch64_ADRP)                                 , kRWI_W    , 0                         , 1  ), // #7
  INST(And              , BaseLogical        , (0b0001010000, 0b00100100, 0)                                                         , kRWI_W    , 0                         , 0  ), // #8
  INST(Ands             , BaseLogical        , (0b1101010000, 0b11100100, 0)                                                         , kRWI_W    , 0                         , 1  ), // #9
  INST(Asr              , BaseShift          , (0b0001101011000000001010, 0b0001001100000000011111, 0)                               , kRWI_W    , 0                         , 0  ), // #10
  INST(Asrv             , BaseShift          , (0b0001101011000000001010, 0b0000000000000000000000, 0)                               , kRWI_W    , 0                         , 1  ), // #11
  INST(At               , BaseAtDcIcTlbi     , (0b00011111110000, 0b00001111000000, true)                                            , kRWI_RX   , 0                         , 0  ), // #12
  INST(Autda            , BaseRR             , (0b11011010110000010001100000000000, kX, kZR, 0, kX, kSP, 5, true)                    , kRWI_X    , 0                         , 0  ), // #13
  INST(Autdza           , BaseR              , (0b11011010110000010011101111100000, kX, kZR, 0)                                      , kRWI_X    , 0                         , 0  ), // #14
  INST(Autdb            , BaseRR             , (0b11011010110000010001110000000000, kX, kZR, 0, kX, kSP, 5, true)                    , kRWI_X    , 0                         , 1  ), // #15
  INST(Autdzb           , BaseR              , (0b11011010110000010011111111100000, kX, kZR, 0)                                      , kRWI_X    , 0                         , 1  ), // #16
  INST(Autia            , BaseRR             , (0b11011010110000010001000000000000, kX, kZR, 0, kX, kSP, 5, true)                    , kRWI_X    , 0                         , 2  ), // #17
  INST(Autia1716        , BaseOp             , (0b11010101000000110010000110011111)                                                  , 0         , 0                         , 0  ), // #18
  INST(Autiasp          , BaseOp             , (0b11010101000000110010001110111111)                                                  , 0         , 0                         , 1  ), // #19
  INST(Autiaz           , BaseOp             , (0b11010101000000110010001110011111)                                                  , 0         , 0                         , 2  ), // #20
  INST(Autib            , BaseRR             , (0b11011010110000010001010000000000, kX, kZR, 0, kX, kSP, 5, true)                    , kRWI_X    , 0                         , 3  ), // #21
  INST(Autib1716        , BaseOp             , (0b11010101000000110010000111011111)                                                  , 0         , 0                         , 3  ), // #22
  INST(Autibsp          , BaseOp             , (0b11010101000000110010001111111111)                                                  , 0         , 0                         , 4  ), // #23
  INST(Autibz           , BaseOp             , (0b11010101000000110010001111011111)                                                  , 0         , 0                         , 5  ), // #24
  INST(Autiza           , BaseR              , (0b11011010110000010011001111100000, kX, kZR, 0)                                      , kRWI_X    , 0                         , 2  ), // #25
  INST(Autizb           , BaseR              , (0b11011010110000010011011111100000, kX, kZR, 0)                                      , kRWI_X    , 0                         , 3  ), // #26
  INST(Axflag           , BaseOp             , (0b11010101000000000100000001011111)                                                  , 0         , 0                         , 6  ), // #27
  INST(B                , BaseBranchRel      , (0b00010100000000000000000000000000)                                                  , 0         , F(Cond)                   , 0  ), // #28
  INST(Bfc              , BaseBfc            , (0b00110011000000000000001111100000)                                                  , kRWI_X    , 0                         , 0  ), // #29
  INST(Bfi              , BaseBfi            , (0b00110011000000000000000000000000)                                                  , kRWI_X    , 0                         , 0  ), // #30
  INST(Bfm              , BaseBfm            , (0b00110011000000000000000000000000)                                                  , kRWI_X    , 0                         , 0  ), // #31
  INST(Bfxil            , BaseBfx            , (0b00110011000000000000000000000000)                                                  , kRWI_X    , 0                         , 0  ), // #32
  INST(Bic              , BaseLogical        , (0b0001010001, 0b00100100, 1)                                                         , kRWI_W    , 0                         , 2  ), // #33
  INST(Bics             , BaseLogical        , (0b1101010001, 0b11100100, 1)                                                         , kRWI_W    , 0                         , 3  ), // #34
  INST(Bl               , BaseBranchRel      , (0b10010100000000000000000000000000)                                                  , 0         , 0                         , 1  ), // #35
  INST(Blr              , BaseBranchReg      , (0b11010110001111110000000000000000)                                                  , kRWI_R    , 0                         , 0  ), // #36
  INST(Br               , BaseBranchReg      , (0b11010110000111110000000000000000)                                                  , kRWI_R    , 0                         , 1  ), // #37
  INST(Brk              , BaseOpImm          , (0b11010100001000000000000000000000, 16, 5)                                           , 0         , 0                         , 0  ), // #38
  INST(Cas              , BaseAtomicOp       , (0b1000100010100000011111, kWX, 30, 0)                                                , kRWI_XRX  , 0                         , 0  ), // #39
  INST(Casa             , BaseAtomicOp       , (0b1000100011100000011111, kWX, 30, 1)                                                , kRWI_XRX  , 0                         , 1  ), // #40
  INST(Casab            , BaseAtomicOp       , (0b0000100011100000011111, kW , 0 , 1)                                                , kRWI_XRX  , 0                         , 2  ), // #41
  INST(Casah            , BaseAtomicOp       , (0b0100100011100000011111, kW , 0 , 1)                                                , kRWI_XRX  , 0                         , 3  ), // #42
  INST(Casal            , BaseAtomicOp       , (0b1000100011100000111111, kWX, 30, 1)                                                , kRWI_XRX  , 0                         , 4  ), // #43
  INST(Casalb           , BaseAtomicOp       , (0b0000100011100000111111, kW , 0 , 1)                                                , kRWI_XRX  , 0                         , 5  ), // #44
  INST(Casalh           , BaseAtomicOp       , (0b0100100011100000111111, kW , 0 , 1)                                                , kRWI_XRX  , 0                         , 6  ), // #45
  INST(Casb             , BaseAtomicOp       , (0b0000100010100000011111, kW , 0 , 0)                                                , kRWI_XRX  , 0                         , 7  ), // #46
  INST(Cash             , BaseAtomicOp       , (0b0100100010100000011111, kW , 0 , 0)                                                , kRWI_XRX  , 0                         , 8  ), // #47
  INST(Casl             , BaseAtomicOp       , (0b1000100010100000111111, kWX, 30, 0)                                                , kRWI_XRX  , 0                         , 9  ), // #48
  INST(Caslb            , BaseAtomicOp       , (0b0000100010100000111111, kW , 0 , 0)                                                , kRWI_XRX  , 0                         , 10 ), // #49
  INST(Caslh            , BaseAtomicOp       , (0b0100100010100000111111, kW , 0 , 0)                                                , kRWI_XRX  , 0                         , 11 ), // #50
  INST(Casp             , BaseAtomicCasp     , (0b0000100000100000011111, kWX, 30)                                                   , kRWI_XXRRX, 0                         , 0  ), // #51
  INST(Caspa            , BaseAtomicCasp     , (0b0000100001100000011111, kWX, 30)                                                   , kRWI_XXRRX, 0                         , 1  ), // #52
  INST(Caspal           , BaseAtomicCasp     , (0b0000100001100000111111, kWX, 30)                                                   , kRWI_XXRRX, 0                         , 2  ), // #53
  INST(Caspl            , BaseAtomicCasp     , (0b0000100000100000111111, kWX, 30)                                                   , kRWI_XXRRX, 0                         , 3  ), // #54
  INST(Cbnz             , BaseBranchCmp      , (0b00110101000000000000000000000000)                                                  , kRWI_R    , 0                         , 0  ), // #55
  INST(Cbz              , BaseBranchCmp      , (0b00110100000000000000000000000000)                                                  , kRWI_R    , 0                         , 1  ), // #56
  INST(Ccmn             , BaseCCmp           , (0b00111010010000000000000000000000)                                                  , kRWI_R    , 0                         , 0  ), // #57
  INST(Ccmp             , BaseCCmp           , (0b01111010010000000000000000000000)                                                  , kRWI_R    , 0                         , 1  ), // #58
  INST(Cfinv            , BaseOp             , (0b11010101000000000100000000011111)                                                  , 0         , 0                         , 7  ), // #59
  INST(Cinc             , BaseCInc           , (0b00011010100000000000010000000000)                                                  , kRWI_W    , 0                         , 0  ), // #60
  INST(Cinv             , BaseCInc           , (0b01011010100000000000000000000000)                                                  , kRWI_W    , 0                         , 1  ), // #61
  INST(Clrex            , BaseOpImm          , (0b11010101000000110011000001011111, 4, 8)                                            , 0         , 0                         , 1  ), // #62
  INST(Cls              , BaseRR             , (0b01011010110000000001010000000000, kWX, kZR, 0, kWX, kZR, 5, true)                  , kRWI_W    , 0                         , 4  ), // #63
  INST(Clz              , BaseRR             , (0b01011010110000000001000000000000, kWX, kZR, 0, kWX, kZR, 5, true)                  , kRWI_W    , 0                         , 5  ), // #64
  INST(Cmn              , BaseCmpCmn         , (0b0101011000, 0b0101011001, 0b0110001)                                               , kRWI_R    , 0                         , 0  ), // #65
  INST(Cmp              , BaseCmpCmn         , (0b1101011000, 0b1101011001, 0b1110001)                                               , kRWI_R    , 0                         , 1  ), // #66
  INST(Cmpp             , BaseRR             , (0b10111010110000000000000000011111, kX, kSP, 5, kX, kSP, 16, true)                   , kRWI_R    , 0                         , 6  ), // #67
  INST(Cneg             , BaseCInc           , (0b01011010100000000000010000000000)                                                  , kRWI_W    , 0                         , 2  ), // #68
  INST(Crc32b           , BaseRRR            , (0b0001101011000000010000, kW, kZR, kW, kZR, kW, kZR, false)                          , kRWI_W    , 0                         , 2  ), // #69
  INST(Crc32cb          , BaseRRR            , (0b0001101011000000010100, kW, kZR, kW, kZR, kW, kZR, false)                          , kRWI_W    , 0                         , 3  ), // #70
  INST(Crc32ch          , BaseRRR            , (0b0001101011000000010101, kW, kZR, kW, kZR, kW, kZR, false)                          , kRWI_W    , 0                         , 4  ), // #71
  INST(Crc32cw          , BaseRRR            , (0b0001101011000000010110, kW, kZR, kW, kZR, kW, kZR, false)                          , kRWI_W    , 0                         , 5  ), // #72
  INST(Crc32cx          , BaseRRR            , (0b1001101011000000010111, kW, kZR, kW, kZR, kX, kZR, false)                          , kRWI_W    , 0                         , 6  ), // #73
  INST(Crc32h           , BaseRRR            , (0b0001101011000000010001, kW, kZR, kW, kZR, kW, kZR, false)                          , kRWI_W    , 0                         , 7  ), // #74
  INST(Crc32w           , BaseRRR            , (0b0001101011000000010010, kW, kZR, kW, kZR, kW, kZR, false)                          , kRWI_W    , 0                         , 8  ), // #75
  INST(Crc32x           , BaseRRR            , (0b1001101011000000010011, kW, kZR, kW, kZR, kX, kZR, false)                          , kRWI_W    , 0                         , 9  ), // #76
  INST(Csdb             , BaseOp             , (0b11010101000000110010001010011111)                                                  , 0         , 0                         , 8  ), // #77
  INST(Csel             , BaseCSel           , (0b00011010100000000000000000000000)                                                  , kRWI_W    , 0                         , 0  ), // #78
  INST(Cset             , BaseCSet           , (0b00011010100111110000011111100000)                                                  , kRWI_W    , 0                         , 0  ), // #79
  INST(Csetm            , BaseCSet           , (0b01011010100111110000001111100000)                                                  , kRWI_W    , 0                         , 1  ), // #80
  INST(Csinc            , BaseCSel           , (0b00011010100000000000010000000000)                                                  , kRWI_W    , 0                         , 1  ), // #81
  INST(Csinv            , BaseCSel           , (0b01011010100000000000000000000000)                                                  , kRWI_W    , 0                         , 2  ), // #82
  INST(Csneg            , BaseCSel           , (0b01011010100000000000010000000000)                                                  , kRWI_W    , 0                         , 3  ), // #83
  INST(Dc               , BaseAtDcIcTlbi     , (0b00011110000000, 0b00001110000000, true)                                            , kRWI_RX   , 0                         , 1  ), // #84
  INST(Dcps1            , BaseOpImm          , (0b11010100101000000000000000000001, 16, 5)                                           , 0         , 0                         , 2  ), // #85
  INST(Dcps2            , BaseOpImm          , (0b11010100101000000000000000000010, 16, 5)                                           , 0         , 0                         , 3  ), // #86
  INST(Dcps3            , BaseOpImm          , (0b11010100101000000000000000000011, 16, 5)                                           , 0         , 0                         , 4  ), // #87
  INST(Dgh              , BaseOp             , (0b11010101000000110010000011011111)                                                  , 0         , 0                         , 9  ), // #88
  INST(Dmb              , BaseOpImm          , (0b11010101000000110011000010111111, 4, 8)                                            , 0         , 0                         , 5  ), // #89
  INST(Drps             , BaseOp             , (0b11010110101111110000001111100000)                                                  , 0         , 0                         , 10 ), // #90
  INST(Dsb              , BaseOpImm          , (0b11010101000000110011000010011111, 4, 8)                                            , 0         , 0                         , 6  ), // #91
  INST(Eon              , BaseLogical        , (0b1001010001, 0b10100100, 1)                                                         , kRWI_W    , 0                         , 4  ), // #92
  INST(Eor              , BaseLogical        , (0b1001010000, 0b10100100, 0)                                                         , kRWI_W    , 0                         , 5  ), // #93
  INST(Esb              , BaseOp             , (0b11010101000000110010001000011111)                                                  , 0         , 0                         , 11 ), // #94
  INST(Extr             , BaseExtract        , (0b00010011100000000000000000000000)                                                  , kRWI_W    , 0                         , 0  ), // #95
  INST(Eret             , BaseOp             , (0b11010110100111110000001111100000)                                                  , 0         , 0                         , 12 ), // #96
  INST(Gmi              , BaseRRR            , (0b1001101011000000000101, kX , kZR, kX , kSP, kX , kZR, true)                        , kRWI_W    , 0                         , 10 ), // #97
  INST(Hint             , BaseOpImm          , (0b11010101000000110010000000011111, 7, 5)                                            , 0         , 0                         , 7  ), // #98
  INST(Hlt              , BaseOpImm          , (0b11010100010000000000000000000000, 16, 5)                                           , 0         , 0                         , 8  ), // #99
  INST(Hvc              , BaseOpImm          , (0b11010100000000000000000000000010, 16, 5)                                           , 0         , 0                         , 9  ), // #100
  INST(Ic               , BaseAtDcIcTlbi     , (0b00011110000000, 0b00001110000000, false)                                           , kRWI_RX   , 0                         , 2  ), // #101
  INST(Isb              , BaseOpImm          , (0b11010101000000110011000011011111, 4, 8)                                            , 0         , 0                         , 10 ), // #102
  INST(Ldadd            , BaseAtomicOp       , (0b1011100000100000000000, kWX, 30, 0)                                                , kRWI_WRX  , 0                         , 12 ), // #103
  INST(Ldadda           , BaseAtomicOp       , (0b1011100010100000000000, kWX, 30, 1)                                                , kRWI_WRX  , 0                         , 13 ), // #104
  INST(Ldaddab          , BaseAtomicOp       , (0b0011100010100000000000, kW , 0 , 1)                                                , kRWI_WRX  , 0                         , 14 ), // #105
  INST(Ldaddah          , BaseAtomicOp       , (0b0111100010100000000000, kW , 0 , 1)                                                , kRWI_WRX  , 0                         , 15 ), // #106
  INST(Ldaddal          , BaseAtomicOp       , (0b1011100011100000000000, kWX, 30, 1)                                                , kRWI_WRX  , 0                         , 16 ), // #107
  INST(Ldaddalb         , BaseAtomicOp       , (0b0011100011100000000000, kW , 0 , 1)                                                , kRWI_WRX  , 0                         , 17 ), // #108
  INST(Ldaddalh         , BaseAtomicOp       , (0b0111100011100000000000, kW , 0 , 1)                                                , kRWI_WRX  , 0                         , 18 ), // #109
  INST(Ldaddb           , BaseAtomicOp       , (0b0011100000100000000000, kW , 0 , 0)                                                , kRWI_WRX  , 0                         , 19 ), // #110
  INST(Ldaddh           , BaseAtomicOp       , (0b0111100000100000000000, kW , 0 , 0)                                                , kRWI_WRX  , 0                         , 20 ), // #111
  INST(Ldaddl           , BaseAtomicOp       , (0b1011100001100000000000, kWX, 30, 0)                                                , kRWI_WRX  , 0                         , 21 ), // #112
  INST(Ldaddlb          , BaseAtomicOp       , (0b0011100001100000000000, kW , 0 , 0)                                                , kRWI_WRX  , 0                         , 22 ), // #113
  INST(Ldaddlh          , BaseAtomicOp       , (0b0111100001100000000000, kW , 0 , 0)                                                , kRWI_WRX  , 0                         , 23 ), // #114
  INST(Ldar             , BaseRM_NoImm       , (0b1000100011011111111111, kWX, kZR, 30)                                              , kRWI_W    , 0                         , 0  ), // #115
  INST(Ldarb            , BaseRM_NoImm       , (0b0000100011011111111111, kW , kZR, 0 )                                              , kRWI_W    , 0                         , 1  ), // #116
  INST(Ldarh            , BaseRM_NoImm       , (0b0100100011011111111111, kW , kZR, 0 )                                              , kRWI_W    , 0                         , 2  ), // #117
  INST(Ldaxp            , BaseLdxp           , (0b1000100001111111100000, kWX, 30)                                                   , kRWI_WW   , 0                         , 0  ), // #118
  INST(Ldaxr            , BaseRM_NoImm       , (0b1000100001011111111111, kWX, kZR, 30)                                              , kRWI_W    , 0                         , 3  ), // #119
  INST(Ldaxrb           , BaseRM_NoImm       , (0b0000100001011111111111, kW , kZR, 0 )                                              , kRWI_W    , 0                         , 4  ), // #120
  INST(Ldaxrh           , BaseRM_NoImm       , (0b0100100001011111111111, kW , kZR, 0 )                                              , kRWI_W    , 0                         , 5  ), // #121
  INST(Ldclr            , BaseAtomicOp       , (0b1011100000100000000100, kWX, 30, 0)                                                , kRWI_WRX  , 0                         , 24 ), // #122
  INST(Ldclra           , BaseAtomicOp       , (0b1011100010100000000100, kWX, 30, 1)                                                , kRWI_WRX  , 0                         , 25 ), // #123
  INST(Ldclrab          , BaseAtomicOp       , (0b0011100010100000000100, kW , 0 , 1)                                                , kRWI_WRX  , 0                         , 26 ), // #124
  INST(Ldclrah          , BaseAtomicOp       , (0b0111100010100000000100, kW , 0 , 1)                                                , kRWI_WRX  , 0                         , 27 ), // #125
  INST(Ldclral          , BaseAtomicOp       , (0b1011100011100000000100, kWX, 30, 1)                                                , kRWI_WRX  , 0                         , 28 ), // #126
  INST(Ldclralb         , BaseAtomicOp       , (0b0011100011100000000100, kW , 0 , 1)                                                , kRWI_WRX  , 0                         , 29 ), // #127
  INST(Ldclralh         , BaseAtomicOp       , (0b0111100011100000000100, kW , 0 , 1)                                                , kRWI_WRX  , 0                         , 30 ), // #128
  INST(Ldclrb           , BaseAtomicOp       , (0b0011100000100000000100, kW , 0 , 0)                                                , kRWI_WRX  , 0                         , 31 ), // #129
  INST(Ldclrh           , BaseAtomicOp       , (0b0111100000100000000100, kW , 0 , 0)                                                , kRWI_WRX  , 0                         , 32 ), // #130
  INST(Ldclrl           , BaseAtomicOp       , (0b1011100001100000000100, kWX, 30, 0)                                                , kRWI_WRX  , 0                         , 33 ), // #131
  INST(Ldclrlb          , BaseAtomicOp       , (0b0011100001100000000100, kW , 0 , 0)                                                , kRWI_WRX  , 0                         , 34 ), // #132
  INST(Ldclrlh          , BaseAtomicOp       , (0b0111100001100000000100, kW , 0 , 0)                                                , kRWI_WRX  , 0                         , 35 ), // #133
  INST(Ldeor            , BaseAtomicOp       , (0b1011100000100000001000, kWX, 30, 0)                                                , kRWI_WRX  , 0                         , 36 ), // #134
  INST(Ldeora           , BaseAtomicOp       , (0b1011100010100000001000, kWX, 30, 1)                                                , kRWI_WRX  , 0                         , 37 ), // #135
  INST(Ldeorab          , BaseAtomicOp       , (0b0011100010100000001000, kW , 0 , 1)                                                , kRWI_WRX  , 0                         , 38 ), // #136
  INST(Ldeorah          , BaseAtomicOp       , (0b0111100010100000001000, kW , 0 , 1)                                                , kRWI_WRX  , 0                         , 39 ), // #137
  INST(Ldeoral          , BaseAtomicOp       , (0b1011100011100000001000, kWX, 30, 1)                                                , kRWI_WRX  , 0                         , 40 ), // #138
  INST(Ldeoralb         , BaseAtomicOp       , (0b0011100011100000001000, kW , 0 , 1)                                                , kRWI_WRX  , 0                         , 41 ), // #139
  INST(Ldeoralh         , BaseAtomicOp       , (0b0111100011100000001000, kW , 0 , 1)                                                , kRWI_WRX  , 0                         , 42 ), // #140
  INST(Ldeorb           , BaseAtomicOp       , (0b0011100000100000001000, kW , 0 , 0)                                                , kRWI_WRX  , 0                         , 43 ), // #141
  INST(Ldeorh           , BaseAtomicOp       , (0b0111100000100000001000, kW , 0 , 0)                                                , kRWI_WRX  , 0                         , 44 ), // #142
  INST(Ldeorl           , BaseAtomicOp       , (0b1011100001100000001000, kWX, 30, 0)                                                , kRWI_WRX  , 0                         , 45 ), // #143
  INST(Ldeorlb          , BaseAtomicOp       , (0b0011100001100000001000, kW , 0 , 0)                                                , kRWI_WRX  , 0                         , 46 ), // #144
  INST(Ldeorlh          , BaseAtomicOp       , (0b0111100001100000001000, kW , 0 , 0)                                                , kRWI_WRX  , 0                         , 47 ), // #145
  INST(Ldg              , BaseRM_SImm9       , (0b1101100101100000000000, 0b0000000000000000000000, kX , kZR, 0, 4)                  , kRWI_W    , 0                         , 0  ), // #146
  INST(Ldgm             , BaseRM_NoImm       , (0b1101100111100000000000, kX , kZR, 0 )                                              , kRWI_W    , 0                         , 6  ), // #147
  INST(Ldlar            , BaseRM_NoImm       , (0b1000100011011111011111, kWX, kZR, 30)                                              , kRWI_W    , 0                         , 7  ), // #148
  INST(Ldlarb           , BaseRM_NoImm       , (0b0000100011011111011111, kW , kZR, 0 )                                              , kRWI_W    , 0                         , 8  ), // #149
  INST(Ldlarh           , BaseRM_NoImm       , (0b0100100011011111011111, kW , kZR, 0 )                                              , kRWI_W    , 0                         , 9  ), // #150
  INST(Ldnp             , BaseLdpStp         , (0b0010100001, 0           , kWX, 31, 2)                                              , kRWI_WW   , 0                         , 0  ), // #151
  INST(Ldp              , BaseLdpStp         , (0b0010100101, 0b0010100011, kWX, 31, 2)                                              , kRWI_W    , 0                         , 1  ), // #152
  INST(Ldpsw            , BaseLdpStp         , (0b0110100101, 0b0110100011, kX , 0 , 2)                                              , kRWI_WW   , 0                         , 2  ), // #153
  INST(Ldr              , BaseLdSt           , (0b1011100101, 0b10111000010, 0b10111000011, 0b00011000, kWX, 30, 2, Inst::kIdLdur)   , kRWI_W    , 0                         , 0  ), // #154
  INST(Ldraa            , BaseRM_SImm10      , (0b1111100000100000000001, kX , kZR, 0, 3)                                            , kRWI_W    , 0                         , 0  ), // #155
  INST(Ldrab            , BaseRM_SImm10      , (0b1111100010100000000001, kX , kZR, 0, 3)                                            , kRWI_W    , 0                         , 1  ), // #156
  INST(Ldrb             , BaseLdSt           , (0b0011100101, 0b00111000010, 0b00111000011, 0         , kW , 0 , 0, Inst::kIdLdurb)  , kRWI_W    , 0                         , 1  ), // #157
  INST(Ldrh             , BaseLdSt           , (0b0111100101, 0b01111000010, 0b01111000011, 0         , kW , 0 , 1, Inst::kIdLdurh)  , kRWI_W    , 0                         , 2  ), // #158
  INST(Ldrsb            , BaseLdSt           , (0b0011100111, 0b00111000100, 0b00111000111, 0         , kWX, 22, 0, Inst::kIdLdursb) , kRWI_W    , 0                         , 3  ), // #159
  INST(Ldrsh            , BaseLdSt           , (0b0111100111, 0b01111000100, 0b01111000111, 0         , kWX, 22, 1, Inst::kIdLdursh) , kRWI_W    , 0                         , 4  ), // #160
  INST(Ldrsw            , BaseLdSt           , (0b1011100110, 0b10111000100, 0b10111000101, 0b10011000, kX , 0 , 2, Inst::kIdLdursw) , kRWI_W    , 0                         , 5  ), // #161
  INST(Ldset            , BaseAtomicOp       , (0b1011100000100000001100, kWX, 30, 0)                                                , kRWI_WRX  , 0                         , 48 ), // #162
  INST(Ldseta           , BaseAtomicOp       , (0b1011100010100000001100, kWX, 30, 1)                                                , kRWI_WRX  , 0                         , 49 ), // #163
  INST(Ldsetab          , BaseAtomicOp       , (0b0011100010100000001100, kW , 0 , 1)                                                , kRWI_WRX  , 0                         , 50 ), // #164
  INST(Ldsetah          , BaseAtomicOp       , (0b0111100010100000001100, kW , 0 , 1)                                                , kRWI_WRX  , 0                         , 51 ), // #165
  INST(Ldsetal          , BaseAtomicOp       , (0b1011100011100000001100, kWX, 30, 1)                                                , kRWI_WRX  , 0                         , 52 ), // #166
  INST(Ldsetalb         , BaseAtomicOp       , (0b0011100011100000001100, kW , 0 , 1)                                                , kRWI_WRX  , 0                         , 53 ), // #167
  INST(Ldsetalh         , BaseAtomicOp       , (0b0111100011100000001100, kW , 0 , 1)                                                , kRWI_WRX  , 0                         , 54 ), // #168
  INST(Ldsetb           , BaseAtomicOp       , (0b0011100000100000001100, kW , 0 , 0)                                                , kRWI_WRX  , 0                         , 55 ), // #169
  INST(Ldseth           , BaseAtomicOp       , (0b0111100000100000001100, kW , 0 , 0)                                                , kRWI_WRX  , 0                         , 56 ), // #170
  INST(Ldsetl           , BaseAtomicOp       , (0b1011100001100000001100, kWX, 30, 0)                                                , kRWI_WRX  , 0                         , 57 ), // #171
  INST(Ldsetlb          , BaseAtomicOp       , (0b0011100001100000001100, kW , 0 , 0)                                                , kRWI_WRX  , 0                         , 58 ), // #172
  INST(Ldsetlh          , BaseAtomicOp       , (0b0111100001100000001100, kW , 0 , 0)                                                , kRWI_WRX  , 0                         , 59 ), // #173
  INST(Ldsmax           , BaseAtomicOp       , (0b1011100000100000010000, kWX, 30, 0)                                                , kRWI_WRX  , 0                         , 60 ), // #174
  INST(Ldsmaxa          , BaseAtomicOp       , (0b1011100010100000010000, kWX, 30, 1)                                                , kRWI_WRX  , 0                         , 61 ), // #175
  INST(Ldsmaxab         , BaseAtomicOp       , (0b0011100010100000010000, kW , 0 , 1)                                                , kRWI_WRX  , 0                         , 62 ), // #176
  INST(Ldsmaxah         , BaseAtomicOp       , (0b0111100010100000010000, kW , 0 , 1)                                                , kRWI_WRX  , 0                         , 63 ), // #177
  INST(Ldsmaxal         , BaseAtomicOp       , (0b1011100011100000010000, kWX, 30, 1)                                                , kRWI_WRX  , 0                         , 64 ), // #178
  INST(Ldsmaxalb        , BaseAtomicOp       , (0b0011100011100000010000, kW , 0 , 1)                                                , kRWI_WRX  , 0                         , 65 ), // #179
  INST(Ldsmaxalh        , BaseAtomicOp       , (0b0111100011100000010000, kW , 0 , 1)                                                , kRWI_WRX  , 0                         , 66 ), // #180
  INST(Ldsmaxb          , BaseAtomicOp       , (0b0011100000100000010000, kW , 0 , 0)                                                , kRWI_WRX  , 0                         , 67 ), // #181
  INST(Ldsmaxh          , BaseAtomicOp       , (0b0111100000100000010000, kW , 0 , 0)                                                , kRWI_WRX  , 0                         , 68 ), // #182
  INST(Ldsmaxl          , BaseAtomicOp       , (0b1011100001100000010000, kWX, 30, 0)                                                , kRWI_WRX  , 0                         , 69 ), // #183
  INST(Ldsmaxlb         , BaseAtomicOp       , (0b0011100001100000010000, kW , 0 , 0)                                                , kRWI_WRX  , 0                         , 70 ), // #184
  INST(Ldsmaxlh         , BaseAtomicOp       , (0b0111100001100000010000, kW , 0 , 0)                                                , kRWI_WRX  , 0                         , 71 ), // #185
  INST(Ldsmin           , BaseAtomicOp       , (0b1011100000100000010100, kWX, 30, 0)                                                , kRWI_WRX  , 0                         , 72 ), // #186
  INST(Ldsmina          , BaseAtomicOp       , (0b1011100010100000010100, kWX, 30, 1)                                                , kRWI_WRX  , 0                         , 73 ), // #187
  INST(Ldsminab         , BaseAtomicOp       , (0b0011100010100000010100, kW , 0 , 1)                                                , kRWI_WRX  , 0                         , 74 ), // #188
  INST(Ldsminah         , BaseAtomicOp       , (0b0111100010100000010100, kW , 0 , 1)                                                , kRWI_WRX  , 0                         , 75 ), // #189
  INST(Ldsminal         , BaseAtomicOp       , (0b1011100011100000010100, kWX, 30, 1)                                                , kRWI_WRX  , 0                         , 76 ), // #190
  INST(Ldsminalb        , BaseAtomicOp       , (0b0011100011100000010100, kW , 0 , 1)                                                , kRWI_WRX  , 0                         , 77 ), // #191
  INST(Ldsminalh        , BaseAtomicOp       , (0b0111100011100000010100, kW , 0 , 1)                                                , kRWI_WRX  , 0                         , 78 ), // #192
  INST(Ldsminb          , BaseAtomicOp       , (0b0011100000100000010100, kW , 0 , 0)                                                , kRWI_WRX  , 0                         , 79 ), // #193
  INST(Ldsminh          , BaseAtomicOp       , (0b0111100000100000010100, kW , 0 , 0)                                                , kRWI_WRX  , 0                         , 80 ), // #194
  INST(Ldsminl          , BaseAtomicOp       , (0b1011100001100000010100, kWX, 30, 0)                                                , kRWI_WRX  , 0                         , 81 ), // #195
  INST(Ldsminlb         , BaseAtomicOp       , (0b0011100001100000010100, kW , 0 , 0)                                                , kRWI_WRX  , 0                         , 82 ), // #196
  INST(Ldsminlh         , BaseAtomicOp       , (0b0111100001100000010100, kW , 0 , 0)                                                , kRWI_WRX  , 0                         , 83 ), // #197
  INST(Ldtr             , BaseRM_SImm9       , (0b1011100001000000000010, 0b0000000000000000000000, kWX, kZR, 30, 0)                 , kRWI_W    , 0                         , 1  ), // #198
  INST(Ldtrb            , BaseRM_SImm9       , (0b0011100001000000000010, 0b0000000000000000000000, kW , kZR, 0 , 0)                 , kRWI_W    , 0                         , 2  ), // #199
  INST(Ldtrh            , BaseRM_SImm9       , (0b0111100001000000000010, 0b0000000000000000000000, kW , kZR, 0 , 0)                 , kRWI_W    , 0                         , 3  ), // #200
  INST(Ldtrsb           , BaseRM_SImm9       , (0b0011100011000000000010, 0b0000000000000000000000, kWX, kZR, 22, 0)                 , kRWI_W    , 0                         , 4  ), // #201
  INST(Ldtrsh           , BaseRM_SImm9       , (0b0111100011000000000010, 0b0000000000000000000000, kWX, kZR, 22, 0)                 , kRWI_W    , 0                         , 5  ), // #202
  INST(Ldtrsw           , BaseRM_SImm9       , (0b1011100010000000000010, 0b0000000000000000000000, kX , kZR, 0 , 0)                 , kRWI_W    , 0                         , 6  ), // #203
  INST(Ldumax           , BaseAtomicOp       , (0b1011100000100000011000, kWX, 30, 0)                                                , kRWI_WRX  , 0                         , 84 ), // #204
  INST(Ldumaxa          , BaseAtomicOp       , (0b1011100010100000011000, kWX, 30, 1)                                                , kRWI_WRX  , 0                         , 85 ), // #205
  INST(Ldumaxab         , BaseAtomicOp       , (0b0011100010100000011000, kW , 0 , 1)                                                , kRWI_WRX  , 0                         , 86 ), // #206
  INST(Ldumaxah         , BaseAtomicOp       , (0b0111100010100000011000, kW , 0 , 1)                                                , kRWI_WRX  , 0                         , 87 ), // #207
  INST(Ldumaxal         , BaseAtomicOp       , (0b1011100011100000011000, kWX, 30, 1)                                                , kRWI_WRX  , 0                         , 88 ), // #208
  INST(Ldumaxalb        , BaseAtomicOp       , (0b0011100011100000011000, kW , 0 , 1)                                                , kRWI_WRX  , 0                         , 89 ), // #209
  INST(Ldumaxalh        , BaseAtomicOp       , (0b0111100011100000011000, kW , 0 , 1)                                                , kRWI_WRX  , 0                         , 90 ), // #210
  INST(Ldumaxb          , BaseAtomicOp       , (0b0011100000100000011000, kW , 0 , 0)                                                , kRWI_WRX  , 0                         , 91 ), // #211
  INST(Ldumaxh          , BaseAtomicOp       , (0b0111100000100000011000, kW , 0 , 0)                                                , kRWI_WRX  , 0                         , 92 ), // #212
  INST(Ldumaxl          , BaseAtomicOp       , (0b1011100001100000011000, kWX, 30, 0)                                                , kRWI_WRX  , 0                         , 93 ), // #213
  INST(Ldumaxlb         , BaseAtomicOp       , (0b0011100001100000011000, kW , 0 , 0)                                                , kRWI_WRX  , 0                         , 94 ), // #214
  INST(Ldumaxlh         , BaseAtomicOp       , (0b0111100001100000011000, kW , 0 , 0)                                                , kRWI_WRX  , 0                         , 95 ), // #215
  INST(Ldumin           , BaseAtomicOp       , (0b1011100000100000011100, kWX, 30, 0)                                                , kRWI_WRX  , 0                         , 96 ), // #216
  INST(Ldumina          , BaseAtomicOp       , (0b1011100010100000011100, kWX, 30, 1)                                                , kRWI_WRX  , 0                         , 97 ), // #217
  INST(Lduminab         , BaseAtomicOp       , (0b0011100010100000011100, kW , 0 , 1)                                                , kRWI_WRX  , 0                         , 98 ), // #218
  INST(Lduminah         , BaseAtomicOp       , (0b0111100010100000011100, kW , 0 , 1)                                                , kRWI_WRX  , 0                         , 99 ), // #219
  INST(Lduminal         , BaseAtomicOp       , (0b1011100011100000011100, kWX, 30, 1)                                                , kRWI_WRX  , 0                         , 100), // #220
  INST(Lduminalb        , BaseAtomicOp       , (0b0011100011100000011100, kW , 0 , 1)                                                , kRWI_WRX  , 0                         , 101), // #221
  INST(Lduminalh        , BaseAtomicOp       , (0b0111100011100000011100, kW , 0 , 1)                                                , kRWI_WRX  , 0                         , 102), // #222
  INST(Lduminb          , BaseAtomicOp       , (0b0011100000100000011100, kW , 0 , 0)                                                , kRWI_WRX  , 0                         , 103), // #223
  INST(Lduminh          , BaseAtomicOp       , (0b0111100000100000011100, kW , 0 , 0)                                                , kRWI_WRX  , 0                         , 104), // #224
  INST(Lduminl          , BaseAtomicOp       , (0b1011100001100000011100, kWX, 30, 0)                                                , kRWI_WRX  , 0                         , 105), // #225
  INST(Lduminlb         , BaseAtomicOp       , (0b0011100001100000011100, kW , 0 , 0)                                                , kRWI_WRX  , 0                         , 106), // #226
  INST(Lduminlh         , BaseAtomicOp       , (0b0111100001100000011100, kW , 0 , 0)                                                , kRWI_WRX  , 0                         , 107), // #227
  INST(Ldur             , BaseRM_SImm9       , (0b1011100001000000000000, 0b0000000000000000000000, kWX, kZR, 30, 0)                 , kRWI_W    , 0                         , 7  ), // #228
  INST(Ldurb            , BaseRM_SImm9       , (0b0011100001000000000000, 0b0000000000000000000000, kW , kZR, 0 , 0)                 , kRWI_W    , 0                         , 8  ), // #229
  INST(Ldurh            , BaseRM_SImm9       , (0b0111100001000000000000, 0b0000000000000000000000, kW , kZR, 0 , 0)                 , kRWI_W    , 0                         , 9  ), // #230
  INST(Ldursb           , BaseRM_SImm9       , (0b0011100011000000000000, 0b0000000000000000000000, kWX, kZR, 22, 0)                 , kRWI_W    , 0                         , 10 ), // #231
  INST(Ldursh           , BaseRM_SImm9       , (0b0111100011000000000000, 0b0000000000000000000000, kWX, kZR, 22, 0)                 , kRWI_W    , 0                         , 11 ), // #232
  INST(Ldursw           , BaseRM_SImm9       , (0b1011100010000000000000, 0b0000000000000000000000, kX , kZR, 0 , 0)                 , kRWI_W    , 0                         , 12 ), // #233
  INST(Ldxp             , BaseLdxp           , (0b1000100001111111000000, kWX, 30)                                                   , kRWI_WW   , 0                         , 1  ), // #234
  INST(Ldxr             , BaseRM_NoImm       , (0b1000100001011111011111, kWX, kZR, 30)                                              , kRWI_W    , 0                         , 10 ), // #235
  INST(Ldxrb            , BaseRM_NoImm       , (0b0000100001011111011111, kW , kZR, 0 )                                              , kRWI_W    , 0                         , 11 ), // #236
  INST(Ldxrh            , BaseRM_NoImm       , (0b0100100001011111011111, kW , kZR, 0 )                                              , kRWI_W    , 0                         , 12 ), // #237
  INST(Lsl              , BaseShift          , (0b0001101011000000001000, 0b0101001100000000000000, 0)                               , kRWI_W    , 0                         , 2  ), // #238
  INST(Lslv             , BaseShift          , (0b0001101011000000001000, 0b0000000000000000000000, 0)                               , kRWI_W    , 0                         , 3  ), // #239
  INST(Lsr              , BaseShift          , (0b0001101011000000001001, 0b0101001100000000011111, 0)                               , kRWI_W    , 0                         , 4  ), // #240
  INST(Lsrv             , BaseShift          , (0b0001101011000000001001, 0b0000000000000000000000, 0)                               , kRWI_W    , 0                         , 5  ), // #241
  INST(Madd             , BaseRRRR           , (0b0001101100000000000000, kWX, kZR, kWX, kZR, kWX, kZR, kWX, kZR, true)              , kRWI_W    , 0                         , 0  ), // #242
  INST(Mneg             , BaseRRR            , (0b0001101100000000111111, kWX, kZR, kWX, kZR, kWX, kZR, true)                        , kRWI_W    , 0                         , 11 ), // #243
  INST(Mov              , BaseMov            , (_)                                                                                   , kRWI_W    , 0                         , 0  ), // #244
  INST(Movk             , BaseMovKNZ         , (0b01110010100000000000000000000000)                                                  , kRWI_X    , 0                         , 0  ), // #245
  INST(Movn             , BaseMovKNZ         , (0b00010010100000000000000000000000)                                                  , kRWI_W    , 0                         , 1  ), // #246
  INST(Movz             , BaseMovKNZ         , (0b01010010100000000000000000000000)                                                  , kRWI_W    , 0                         , 2  ), // #247
  INST(Mrs              , BaseMrs            , (_)                                                                                   , kRWI_W    , 0                         , 0  ), // #248
  INST(Msr              , BaseMsr            , (_)                                                                                   , kRWI_W    , 0                         , 0  ), // #249
  INST(Msub             , BaseRRRR           , (0b0001101100000000100000, kWX, kZR, kWX, kZR, kWX, kZR, kWX, kZR, true)              , kRWI_W    , 0                         , 1  ), // #250
  INST(Mul              , BaseRRR            , (0b0001101100000000011111, kWX, kZR, kWX, kZR, kWX, kZR, true)                        , kRWI_W    , 0                         , 12 ), // #251
  INST(Mvn              , BaseMvnNeg         , (0b00101010001000000000001111100000)                                                  , kRWI_W    , 0                         , 0  ), // #252
  INST(Neg              , BaseMvnNeg         , (0b01001011000000000000001111100000)                                                  , kRWI_W    , 0                         , 1  ), // #253
  INST(Negs             , BaseMvnNeg         , (0b01101011000000000000001111100000)                                                  , kRWI_W    , 0                         , 2  ), // #254
  INST(Ngc              , BaseRR             , (0b01011010000000000000001111100000, kWX, kZR, 0, kWX, kZR, 16, true)                 , kRWI_W    , 0                         , 7  ), // #255
  INST(Ngcs             , BaseRR             , (0b01111010000000000000001111100000, kWX, kZR, 0, kWX, kZR, 16, true)                 , kRWI_W    , 0                         , 8  ), // #256
  INST(Nop              , BaseOp             , (0b11010101000000110010000000011111)                                                  , 0         , 0                         , 13 ), // #257
  INST(Orn              , BaseLogical        , (0b0101010001, 0b01100100, 1)                                                         , kRWI_W    , 0                         , 6  ), // #258
  INST(Orr              , BaseLogical        , (0b0101010000, 0b01100100, 0)                                                         , kRWI_W    , 0                         , 7  ), // #259
  INST(Pacda            , BaseRR             , (0b11011010110000010000100000000000, kX, kZR, 0, kX, kSP, 5, true)                    , kRWI_X    , 0                         , 9  ), // #260
  INST(Pacdb            , BaseRR             , (0b11011010110000010000110000000000, kX, kZR, 0, kX, kSP, 5, true)                    , kRWI_X    , 0                         , 10 ), // #261
  INST(Pacdza           , BaseR              , (0b11011010110000010010101111100000, kX, kZR, 0)                                      , kRWI_X    , 0                         , 4  ), // #262
  INST(Pacdzb           , BaseR              , (0b11011010110000010010111111100000, kX, kZR, 0)                                      , kRWI_X    , 0                         , 5  ), // #263
  INST(Pacga            , BaseRRR            , (0b1001101011000000001100, kX, kZR, kX, kZR, kX, kSP, false)                          , kRWI_W    , 0                         , 13 ), // #264
  INST(Pssbb            , BaseOp             , (0b11010101000000110011010010011111)                                                  , 0         , 0                         , 14 ), // #265
  INST(Rbit             , BaseRR             , (0b01011010110000000000000000000000, kWX, kZR, 0, kWX, kZR, 5, true)                  , kRWI_W    , 0                         , 11 ), // #266
  INST(Ret              , BaseBranchReg      , (0b11010110010111110000000000000000)                                                  , kRWI_R    , 0                         , 2  ), // #267
  INST(Rev              , BaseRev            , (_)                                                                                   , kRWI_W    , 0                         , 0  ), // #268
  INST(Rev16            , BaseRR             , (0b01011010110000000000010000000000, kWX, kZR, 0, kWX, kZR, 5, true)                  , kRWI_W    , 0                         , 12 ), // #269
  INST(Rev32            , BaseRR             , (0b11011010110000000000100000000000, kWX, kZR, 0, kWX, kZR, 5, true)                  , kRWI_W    , 0                         , 13 ), // #270
  INST(Rev64            , BaseRR             , (0b11011010110000000000110000000000, kWX, kZR, 0, kWX, kZR, 5, true)                  , kRWI_W    , 0                         , 14 ), // #271
  INST(Ror              , BaseShift          , (0b0001101011000000001011, 0b0001001110000000000000, 1)                               , kRWI_W    , 0                         , 6  ), // #272
  INST(Rorv             , BaseShift          , (0b0001101011000000001011, 0b0000000000000000000000, 1)                               , kRWI_W    , 0                         , 7  ), // #273
  INST(Sbc              , BaseRRR            , (0b0101101000000000000000, kWX, kZR, kWX, kZR, kWX, kZR, true)                        , kRWI_W    , 0                         , 14 ), // #274
  INST(Sbcs             , BaseRRR            , (0b0111101000000000000000, kWX, kZR, kWX, kZR, kWX, kZR, true)                        , kRWI_W    , 0                         , 15 ), // #275
  INST(Sbfiz            , BaseBfi            , (0b00010011000000000000000000000000)                                                  , kRWI_W    , 0                         , 1  ), // #276
  INST(Sbfm             , BaseBfm            , (0b00010011000000000000000000000000)                                                  , kRWI_W    , 0                         , 1  ), // #277
  INST(Sbfx             , BaseBfx            , (0b00010011000000000000000000000000)                                                  , kRWI_W    , 0                         , 1  ), // #278
  INST(Sdiv             , BaseRRR            , (0b0001101011000000000011, kWX, kZR, kWX, kZR, kWX, kZR, true)                        , kRWI_W    , 0                         , 16 ), // #279
  INST(Setf8            , BaseR              , (0b00111010000000000000100000001101, kW, kZR, 5)                                      , 0         , 0                         , 6  ), // #280
  INST(Setf16           , BaseR              , (0b00111010000000000100100000001101, kW, kZR, 5)                                      , 0         , 0                         , 7  ), // #281
  INST(Sev              , BaseOp             , (0b11010101000000110010000010011111)                                                  , 0         , 0                         , 15 ), // #282
  INST(Sevl             , BaseOp             , (0b11010101000000110010000010111111)                                                  , 0         , 0                         , 16 ), // #283
  INST(Smaddl           , BaseRRRR           , (0b1001101100100000000000, kX , kZR, kW , kZR, kW , kZR, kX , kZR, false)             , kRWI_W    , 0                         , 2  ), // #284
  INST(Smc              , BaseOpImm          , (0b11010100000000000000000000000011, 16, 5)                                           , 0         , 0                         , 11 ), // #285
  INST(Smnegl           , BaseRRR            , (0b1001101100100000111111, kX , kZR, kW , kZR, kW , kZR, false)                       , kRWI_W    , 0                         , 17 ), // #286
  INST(Smsubl           , BaseRRRR           , (0b1001101100100000100000, kX , kZR, kW , kZR, kW , kZR, kX , kZR, false)             , kRWI_W    , 0                         , 3  ), // #287
  INST(Smulh            , BaseRRR            , (0b1001101101000000011111, kX , kZR, kX , kZR, kX , kZR, true)                        , kRWI_W    , 0                         , 18 ), // #288
  INST(Smull            , BaseRRR            , (0b1001101100100000011111, kX , kZR, kW , kZR, kW , kZR, false)                       , kRWI_W    , 0                         , 19 ), // #289
  INST(Ssbb             , BaseOp             , (0b11010101000000110011000010011111)                                                  , 0         , 0                         , 17 ), // #290
  INST(St2g             , BaseRM_SImm9       , (0b1101100110100000000010, 0b1101100110100000000001, kX, kSP, 0, 4)                   , kRWI_RW   , 0                         , 13 ), // #291
  INST(Stadd            , BaseAtomicSt       , (0b1011100000100000000000, kWX, 30)                                                   , kRWI_RX   , 0                         , 0  ), // #292
  INST(Staddl           , BaseAtomicSt       , (0b1011100001100000000000, kWX, 30)                                                   , kRWI_RX   , 0                         , 1  ), // #293
  INST(Staddb           , BaseAtomicSt       , (0b0011100000100000000000, kW , 0 )                                                   , kRWI_RX   , 0                         , 2  ), // #294
  INST(Staddlb          , BaseAtomicSt       , (0b0011100001100000000000, kW , 0 )                                                   , kRWI_RX   , 0                         , 3  ), // #295
  INST(Staddh           , BaseAtomicSt       , (0b0111100000100000000000, kW , 0 )                                                   , kRWI_RX   , 0                         , 4  ), // #296
  INST(Staddlh          , BaseAtomicSt       , (0b0111100001100000000000, kW , 0 )                                                   , kRWI_RX   , 0                         , 5  ), // #297
  INST(Stclr            , BaseAtomicSt       , (0b1011100000100000000100, kWX, 30)                                                   , kRWI_RX   , 0                         , 6  ), // #298
  INST(Stclrl           , BaseAtomicSt       , (0b1011100001100000000100, kWX, 30)                                                   , kRWI_RX   , 0                         , 7  ), // #299
  INST(Stclrb           , BaseAtomicSt       , (0b0011100000100000000100, kW , 0 )                                                   , kRWI_RX   , 0                         , 8  ), // #300
  INST(Stclrlb          , BaseAtomicSt       , (0b0011100001100000000100, kW , 0 )                                                   , kRWI_RX   , 0                         , 9  ), // #301
  INST(Stclrh           , BaseAtomicSt       , (0b0111100000100000000100, kW , 0 )                                                   , kRWI_RX   , 0                         , 10 ), // #302
  INST(Stclrlh          , BaseAtomicSt       , (0b0111100001100000000100, kW , 0 )                                                   , kRWI_RX   , 0                         , 11 ), // #303
  INST(Steor            , BaseAtomicSt       , (0b1011100000100000001000, kWX, 30)                                                   , kRWI_RX   , 0                         , 12 ), // #304
  INST(Steorl           , BaseAtomicSt       , (0b1011100001100000001000, kWX, 30)                                                   , kRWI_RX   , 0                         , 13 ), // #305
  INST(Steorb           , BaseAtomicSt       , (0b0011100000100000001000, kW , 0 )                                                   , kRWI_RX   , 0                         , 14 ), // #306
  INST(Steorlb          , BaseAtomicSt       , (0b0011100001100000001000, kW , 0 )                                                   , kRWI_RX   , 0                         , 15 ), // #307
  INST(Steorh           , BaseAtomicSt       , (0b0111100000100000001000, kW , 0 )                                                   , kRWI_RX   , 0                         , 16 ), // #308
  INST(Steorlh          , BaseAtomicSt       , (0b0111100001100000001000, kW , 0 )                                                   , kRWI_RX   , 0                         , 17 ), // #309
  INST(Stg              , BaseRM_SImm9       , (0b1101100100100000000010, 0b1101100100100000000001, kX, kSP, 0, 4)                   , kRWI_RW   , 0                         , 14 ), // #310
  INST(Stgm             , BaseRM_NoImm       , (0b1101100110100000000000, kX , kZR, 0 )                                              , kRWI_RW   , 0                         , 13 ), // #311
  INST(Stgp             , BaseLdpStp         , (0b0110100100, 0b0110100010, kX, 0, 4)                                                , kRWI_RRW  , 0                         , 3  ), // #312
  INST(Stllr            , BaseRM_NoImm       , (0b1000100010011111011111, kWX, kZR, 30)                                              , kRWI_RW   , 0                         , 14 ), // #313
  INST(Stllrb           , BaseRM_NoImm       , (0b0000100010011111011111, kW , kZR, 0 )                                              , kRWI_RW   , 0                         , 15 ), // #314
  INST(Stllrh           , BaseRM_NoImm       , (0b0100100010011111011111, kW , kZR, 0 )                                              , kRWI_RW   , 0                         , 16 ), // #315
  INST(Stlr             , BaseRM_NoImm       , (0b1000100010011111111111, kWX, kZR, 30)                                              , kRWI_RW   , 0                         , 17 ), // #316
  INST(Stlrb            , BaseRM_NoImm       , (0b0000100010011111111111, kW , kZR, 0 )                                              , kRWI_RW   , 0                         , 18 ), // #317
  INST(Stlrh            , BaseRM_NoImm       , (0b0100100010011111111111, kW , kZR, 0 )                                              , kRWI_RW   , 0                         , 19 ), // #318
  INST(Stlxp            , BaseStxp           , (0b1000100000100000100000, kWX, 30)                                                   , kRWI_WRRX , 0                         , 0  ), // #319
  INST(Stlxr            , BaseAtomicOp       , (0b1000100000000000111111, kWX, 30, 1)                                                , kRWI_WRX  , 0                         , 108), // #320
  INST(Stlxrb           , BaseAtomicOp       , (0b0000100000000000111111, kW , 0 , 1)                                                , kRWI_WRX  , 0                         , 109), // #321
  INST(Stlxrh           , BaseAtomicOp       , (0b0100100000000000111111, kW , 0 , 1)                                                , kRWI_WRX  , 0                         , 110), // #322
  INST(Stnp             , BaseLdpStp         , (0b0010100000, 0           , kWX, 31, 2)                                              , kRWI_RRW  , 0                         , 4  ), // #323
  INST(Stp              , BaseLdpStp         , (0b0010100100, 0b0010100010, kWX, 31, 2)                                              , kRWI_RRW  , 0                         , 5  ), // #324
  INST(Str              , BaseLdSt           , (0b1011100100, 0b10111000000, 0b10111000001, 0         , kWX, 30, 2, Inst::kIdStur)   , kRWI_RW   , 0                         , 6  ), // #325
  INST(Strb             , BaseLdSt           , (0b0011100100, 0b00111000000, 0b00111000001, 0         , kW , 30, 0, Inst::kIdSturb)  , kRWI_RW   , 0                         , 7  ), // #326
  INST(Strh             , BaseLdSt           , (0b0111100100, 0b01111000000, 0b01111000001, 0         , kWX, 30, 1, Inst::kIdSturh)  , kRWI_RW   , 0                         , 8  ), // #327
  INST(Stset            , BaseAtomicSt       , (0b1011100000100000001100, kWX, 30)                                                   , kRWI_RX   , 0                         , 18 ), // #328
  INST(Stsetl           , BaseAtomicSt       , (0b1011100001100000001100, kWX, 30)                                                   , kRWI_RX   , 0                         , 19 ), // #329
  INST(Stsetb           , BaseAtomicSt       , (0b0011100000100000001100, kW , 0 )                                                   , kRWI_RX   , 0                         , 20 ), // #330
  INST(Stsetlb          , BaseAtomicSt       , (0b0011100001100000001100, kW , 0 )                                                   , kRWI_RX   , 0                         , 21 ), // #331
  INST(Stseth           , BaseAtomicSt       , (0b0111100000100000001100, kW , 0 )                                                   , kRWI_RX   , 0                         , 22 ), // #332
  INST(Stsetlh          , BaseAtomicSt       , (0b0111100001100000001100, kW , 0 )                                                   , kRWI_RX   , 0                         , 23 ), // #333
  INST(Stsmax           , BaseAtomicSt       , (0b1011100000100000010000, kWX, 30)                                                   , kRWI_RX   , 0                         , 24 ), // #334
  INST(Stsmaxl          , BaseAtomicSt       , (0b1011100001100000010000, kWX, 30)                                                   , kRWI_RX   , 0                         , 25 ), // #335
  INST(Stsmaxb          , BaseAtomicSt       , (0b0011100000100000010000, kW , 0 )                                                   , kRWI_RX   , 0                         , 26 ), // #336
  INST(Stsmaxlb         , BaseAtomicSt       , (0b0011100001100000010000, kW , 0 )                                                   , kRWI_RX   , 0                         , 27 ), // #337
  INST(Stsmaxh          , BaseAtomicSt       , (0b0111100000100000010000, kW , 0 )                                                   , kRWI_RX   , 0                         , 28 ), // #338
  INST(Stsmaxlh         , BaseAtomicSt       , (0b0111100001100000010000, kW , 0 )                                                   , kRWI_RX   , 0                         , 29 ), // #339
  INST(Stsmin           , BaseAtomicSt       , (0b1011100000100000010100, kWX, 30)                                                   , kRWI_RX   , 0                         , 30 ), // #340
  INST(Stsminl          , BaseAtomicSt       , (0b1011100001100000010100, kWX, 30)                                                   , kRWI_RX   , 0                         , 31 ), // #341
  INST(Stsminb          , BaseAtomicSt       , (0b0011100000100000010100, kW , 0 )                                                   , kRWI_RX   , 0                         , 32 ), // #342
  INST(Stsminlb         , BaseAtomicSt       , (0b0011100001100000010100, kW , 0 )                                                   , kRWI_RX   , 0                         , 33 ), // #343
  INST(Stsminh          , BaseAtomicSt       , (0b0111100000100000010100, kW , 0 )                                                   , kRWI_RX   , 0                         , 34 ), // #344
  INST(Stsminlh         , BaseAtomicSt       , (0b0111100001100000010100, kW , 0 )                                                   , kRWI_RX   , 0                         , 35 ), // #345
  INST(Sttr             , BaseRM_SImm9       , (0b1011100000000000000010, 0b0000000000000000000000, kWX, kZR, 30, 0)                 , kRWI_RW   , 0                         , 15 ), // #346
  INST(Sttrb            , BaseRM_SImm9       , (0b0011100000000000000010, 0b0000000000000000000000, kW , kZR, 0 , 0)                 , kRWI_RW   , 0                         , 16 ), // #347
  INST(Sttrh            , BaseRM_SImm9       , (0b0111100000000000000010, 0b0000000000000000000000, kW , kZR, 0 , 0)                 , kRWI_RW   , 0                         , 17 ), // #348
  INST(Stumax           , BaseAtomicSt       , (0b1011100000100000011000, kWX, 30)                                                   , kRWI_RX   , 0                         , 36 ), // #349
  INST(Stumaxl          , BaseAtomicSt       , (0b1011100001100000011000, kWX, 30)                                                   , kRWI_RX   , 0                         , 37 ), // #350
  INST(Stumaxb          , BaseAtomicSt       , (0b0011100000100000011000, kW , 0 )                                                   , kRWI_RX   , 0                         , 38 ), // #351
  INST(Stumaxlb         , BaseAtomicSt       , (0b0011100001100000011000, kW , 0 )                                                   , kRWI_RX   , 0                         , 39 ), // #352
  INST(Stumaxh          , BaseAtomicSt       , (0b0111100000100000011000, kW , 0 )                                                   , kRWI_RX   , 0                         , 40 ), // #353
  INST(Stumaxlh         , BaseAtomicSt       , (0b0111100001100000011000, kW , 0 )                                                   , kRWI_RX   , 0                         , 41 ), // #354
  INST(Stumin           , BaseAtomicSt       , (0b1011100000100000011100, kWX, 30)                                                   , kRWI_RX   , 0                         , 42 ), // #355
  INST(Stuminl          , BaseAtomicSt       , (0b1011100001100000011100, kWX, 30)                                                   , kRWI_RX   , 0                         , 43 ), // #356
  INST(Stuminb          , BaseAtomicSt       , (0b0011100000100000011100, kW , 0 )                                                   , kRWI_RX   , 0                         , 44 ), // #357
  INST(Stuminlb         , BaseAtomicSt       , (0b0011100001100000011100, kW , 0 )                                                   , kRWI_RX   , 0                         , 45 ), // #358
  INST(Stuminh          , BaseAtomicSt       , (0b0111100000100000011100, kW , 0 )                                                   , kRWI_RX   , 0                         , 46 ), // #359
  INST(Stuminlh         , BaseAtomicSt       , (0b0111100001100000011100, kW , 0 )                                                   , kRWI_RX   , 0                         , 47 ), // #360
  INST(Stur             , BaseRM_SImm9       , (0b1011100000000000000000, 0b0000000000000000000000, kWX, kZR, 30, 0)                 , kRWI_RW   , 0                         , 18 ), // #361
  INST(Sturb            , BaseRM_SImm9       , (0b0011100000000000000000, 0b0000000000000000000000, kW , kZR, 0 , 0)                 , kRWI_RW   , 0                         , 19 ), // #362
  INST(Sturh            , BaseRM_SImm9       , (0b0111100000000000000000, 0b0000000000000000000000, kW , kZR, 0 , 0)                 , kRWI_RW   , 0                         , 20 ), // #363
  INST(Stxp             , BaseStxp           , (0b1000100000100000000000, kWX, 30)                                                   , kRWI_WRRW , 0                         , 1  ), // #364
  INST(Stxr             , BaseStx            , (0b1000100000000000011111, kWX, 30)                                                   , kRWI_WRW  , 0                         , 0  ), // #365
  INST(Stxrb            , BaseStx            , (0b0000100000000000011111, kW , 0 )                                                   , kRWI_WRW  , 0                         , 1  ), // #366
  INST(Stxrh            , BaseStx            , (0b0100100000000000011111, kW , 0 )                                                   , kRWI_WRW  , 0                         , 2  ), // #367
  INST(Stz2g            , BaseRM_SImm9       , (0b1101100111100000000010, 0b1101100111100000000001, kX , kSP, 0, 4)                  , kRWI_RW   , 0                         , 21 ), // #368
  INST(Stzg             , BaseRM_SImm9       , (0b1101100101100000000010, 0b1101100101100000000001, kX , kSP, 0, 4)                  , kRWI_RW   , 0                         , 22 ), // #369
  INST(Stzgm            , BaseRM_NoImm       , (0b1101100100100000000000, kX , kZR, 0)                                               , kRWI_RW   , 0                         , 20 ), // #370
  INST(Sub              , BaseAddSub         , (0b1001011000, 0b1001011001, 0b1010001)                                               , kRWI_X    , 0                         , 2  ), // #371
  INST(Subg             , BaseRRII           , (0b1101000110000000000000, kX, kSP, kX, kSP, 6, 4, 16, 4, 0, 10)                      , kRWI_W    , 0                         , 1  ), // #372
  INST(Subp             , BaseRRR            , (0b1001101011000000000000, kX, kZR, kX, kSP, kX, kSP, false)                          , kRWI_W    , 0                         , 20 ), // #373
  INST(Subps            , BaseRRR            , (0b1011101011000000000000, kX, kZR, kX, kSP, kX, kSP, false)                          , kRWI_W    , 0                         , 21 ), // #374
  INST(Subs             , BaseAddSub         , (0b1101011000, 0b1101011001, 0b1110001)                                               , kRWI_X    , 0                         , 3  ), // #375
  INST(Svc              , BaseOpImm          , (0b11010100000000000000000000000001, 16, 5)                                           , 0         , 0                         , 12 ), // #376
  INST(Swp              , BaseAtomicOp       , (0b1011100000100000100000, kWX, 30, 1)                                                , kRWI_RWX  , 0                         , 111), // #377
  INST(Swpa             , BaseAtomicOp       , (0b1011100010100000100000, kWX, 30, 1)                                                , kRWI_RWX  , 0                         , 112), // #378
  INST(Swpab            , BaseAtomicOp       , (0b0011100010100000100000, kW , 0 , 1)                                                , kRWI_RWX  , 0                         , 113), // #379
  INST(Swpah            , BaseAtomicOp       , (0b0111100010100000100000, kW , 0 , 1)                                                , kRWI_RWX  , 0                         , 114), // #380
  INST(Swpal            , BaseAtomicOp       , (0b1011100011100000100000, kWX, 30, 1)                                                , kRWI_RWX  , 0                         , 115), // #381
  INST(Swpalb           , BaseAtomicOp       , (0b0011100011100000100000, kW , 0 , 1)                                                , kRWI_RWX  , 0                         , 116), // #382
  INST(Swpalh           , BaseAtomicOp       , (0b0111100011100000100000, kW , 0 , 1)                                                , kRWI_RWX  , 0                         , 117), // #383
  INST(Swpb             , BaseAtomicOp       , (0b0011100000100000100000, kW , 0 , 1)                                                , kRWI_RWX  , 0                         , 118), // #384
  INST(Swph             , BaseAtomicOp       , (0b0111100000100000100000, kW , 0 , 1)                                                , kRWI_RWX  , 0                         , 119), // #385
  INST(Swpl             , BaseAtomicOp       , (0b1011100001100000100000, kWX, 30, 1)                                                , kRWI_RWX  , 0                         , 120), // #386
  INST(Swplb            , BaseAtomicOp       , (0b0011100001100000100000, kW , 0 , 1)                                                , kRWI_RWX  , 0                         , 121), // #387
  INST(Swplh            , BaseAtomicOp       , (0b0111100001100000100000, kW , 0 , 1)                                                , kRWI_RWX  , 0                         , 122), // #388
  INST(Sxtb             , BaseExtend         , (0b0001001100000000000111, kWX, 0)                                                    , kRWI_W    , 0                         , 0  ), // #389
  INST(Sxth             , BaseExtend         , (0b0001001100000000001111, kWX, 0)                                                    , kRWI_W    , 0                         , 1  ), // #390
  INST(Sxtw             , BaseExtend         , (0b1001001101000000011111, kX , 0)                                                    , kRWI_W    , 0                         , 2  ), // #391
  INST(Sys              , BaseSys            , (_)                                                                                   , kRWI_W    , 0                         , 0  ), // #392
  INST(Tlbi             , BaseAtDcIcTlbi     , (0b00011110000000, 0b00010000000000, false)                                           , kRWI_RX   , 0                         , 3  ), // #393
  INST(Tst              , BaseTst            , (0b1101010000, 0b111001000)                                                           , kRWI_R    , 0                         , 0  ), // #394
  INST(Tbnz             , BaseBranchTst      , (0b00110111000000000000000000000000)                                                  , kRWI_R    , 0                         , 0  ), // #395
  INST(Tbz              , BaseBranchTst      , (0b00110110000000000000000000000000)                                                  , kRWI_R    , 0                         , 1  ), // #396
  INST(Ubfiz            , BaseBfi            , (0b01010011000000000000000000000000)                                                  , kRWI_W    , 0                         , 2  ), // #397
  INST(Ubfm             , BaseBfm            , (0b01010011000000000000000000000000)                                                  , kRWI_W    , 0                         , 2  ), // #398
  INST(Ubfx             , BaseBfx            , (0b01010011000000000000000000000000)                                                  , kRWI_W    , 0                         , 2  ), // #399
  INST(Udf              , BaseOpImm          , (0b00000000000000000000000000000000, 16, 0)                                           , 0         , 0                         , 13 ), // #400
  INST(Udiv             , BaseRRR            , (0b0001101011000000000010, kWX, kZR, kWX, kZR, kWX, kZR, true)                        , kRWI_W    , 0                         , 22 ), // #401
  INST(Umaddl           , BaseRRRR           , (0b1001101110100000000000, kX , kZR, kW , kZR, kW , kZR, kX , kZR, false)             , kRWI_W    , 0                         , 4  ), // #402
  INST(Umnegl           , BaseRRR            , (0b1001101110100000111111, kX , kZR, kW , kZR, kW , kZR, false)                       , kRWI_W    , 0                         , 23 ), // #403
  INST(Umull            , BaseRRR            , (0b1001101110100000011111, kX , kZR, kW , kZR, kW , kZR, false)                       , kRWI_W    , 0                         , 24 ), // #404
  INST(Umulh            , BaseRRR            , (0b1001101111000000011111, kX , kZR, kX , kZR, kX , kZR, false)                       , kRWI_W    , 0                         , 25 ), // #405
  INST(Umsubl           , BaseRRRR           , (0b1001101110100000100000, kX , kZR, kW , kZR, kW , kZR, kX , kZR, false)             , kRWI_W    , 0                         , 5  ), // #406
  INST(Uxtb             , BaseExtend         , (0b0101001100000000000111, kW, 1)                                                     , kRWI_W    , 0                         , 3  ), // #407
  INST(Uxth             , BaseExtend         , (0b0101001100000000001111, kW, 1)                                                     , kRWI_W    , 0                         , 4  ), // #408
  INST(Wfe              , BaseOp             , (0b11010101000000110010000001011111)                                                  , 0         , 0                         , 18 ), // #409
  INST(Wfi              , BaseOp             , (0b11010101000000110010000001111111)                                                  , 0         , 0                         , 19 ), // #410
  INST(Xaflag           , BaseOp             , (0b11010101000000000100000000111111)                                                  , 0         , 0                         , 20 ), // #411
  INST(Xpacd            , BaseR              , (0b11011010110000010100011111100000, kX, kZR, 0)                                      , kRWI_X    , 0                         , 8  ), // #412
  INST(Xpaci            , BaseR              , (0b11011010110000010100001111100000, kX, kZR, 0)                                      , kRWI_X    , 0                         , 9  ), // #413
  INST(Xpaclri          , BaseOp             , (0b11010101000000110010000011111111)                                                  , kRWI_X    , 0                         , 21 ), // #414
  INST(Yield            , BaseOp             , (0b11010101000000110010000000111111)                                                  , 0         , 0                         , 22 ), // #415
  INST(Abs_v            , ISimdVV            , (0b0000111000100000101110, kVO_V_Any)                                                 , kRWI_W    , 0                         , 0  ), // #416
  INST(Add_v            , ISimdVVV           , (0b0000111000100000100001, kVO_V_Any)                                                 , kRWI_W    , 0                         , 0  ), // #417
  INST(Addhn_v          , ISimdVVV           , (0b0000111000100000010000, kVO_V_B8H4S2)                                              , kRWI_W    , F(Narrow)                 , 1  ), // #418
  INST(Addhn2_v         , ISimdVVV           , (0b0100111000100000010000, kVO_V_B16H8S4)                                             , kRWI_W    , F(Narrow)                 , 2  ), // #419
  INST(Addp_v           , ISimdPair          , (0b0101111000110001101110, 0b0000111000100000101111, kVO_V_Any)                       , kRWI_W    , F(Pair)                   , 0  ), // #420
  INST(Addv_v           , ISimdSV            , (0b0000111000110001101110, kVO_V_BH_4S)                                               , kRWI_W    , 0                         , 0  ), // #421
  INST(Aesd_v           , ISimdVVx           , (0b0100111000101000010110, kOp_V16B, kOp_V16B)                                        , kRWI_X    , 0                         , 0  ), // #422
  INST(Aese_v           , ISimdVVx           , (0b0100111000101000010010, kOp_V16B, kOp_V16B)                                        , kRWI_X    , 0                         , 1  ), // #423
  INST(Aesimc_v         , ISimdVVx           , (0b0100111000101000011110, kOp_V16B, kOp_V16B)                                        , kRWI_W    , 0                         , 2  ), // #424
  INST(Aesmc_v          , ISimdVVx           , (0b0100111000101000011010, kOp_V16B, kOp_V16B)                                        , kRWI_W    , 0                         , 3  ), // #425
  INST(And_v            , ISimdVVV           , (0b0000111000100000000111, kVO_V_B)                                                   , kRWI_W    , 0                         , 3  ), // #426
  INST(Bcax_v           , ISimdVVVV          , (0b1100111000100000000000, kVO_V_B16)                                                 , kRWI_W    , 0                         , 0  ), // #427
  INST(Bfcvt_v          , ISimdVVx           , (0b0001111001100011010000, kOp_H, kOp_S)                                              , kRWI_W    , 0                         , 4  ), // #428
  INST(Bfcvtn_v         , ISimdVVx           , (0b0000111010100001011010, kOp_V4H, kOp_V4S)                                          , kRWI_W    , F(Narrow)                 , 5  ), // #429
  INST(Bfcvtn2_v        , ISimdVVx           , (0b0100111010100001011010, kOp_V8H, kOp_V4S)                                          , kRWI_W    , F(Narrow)                 , 6  ), // #430
  INST(Bfdot_v          , SimdDot            , (0b0010111001000000111111, 0b0000111101000000111100, kET_S, kET_H, kET_2H)            , kRWI_X    , 0                         , 0  ), // #431
  INST(Bfmlalb_v        , SimdFmlal          , (0b0010111011000000111111, 0b0000111111000000111100, 0, kET_S, kET_H, kET_H)          , kRWI_X    , F(VH0_15)                 , 0  ), // #432
  INST(Bfmlalt_v        , SimdFmlal          , (0b0110111011000000111111, 0b0100111111000000111100, 0, kET_S, kET_H, kET_H)          , kRWI_X    , F(VH0_15)                 , 1  ), // #433
  INST(Bfmmla_v         , ISimdVVVx          , (0b0110111001000000111011, kOp_V4S, kOp_V8H, kOp_V8H)                                 , kRWI_X    , F(Long)                   , 0  ), // #434
  INST(Bic_v            , SimdBicOrr         , (0b0000111001100000000111, 0b0010111100000000000001)                                  , kRWI_W    , 0                         , 0  ), // #435
  INST(Bif_v            , ISimdVVV           , (0b0010111011100000000111, kVO_V_B)                                                   , kRWI_X    , 0                         , 4  ), // #436
  INST(Bit_v            , ISimdVVV           , (0b0010111010100000000111, kVO_V_B)                                                   , kRWI_X    , 0                         , 5  ), // #437
  INST(Bsl_v            , ISimdVVV           , (0b0010111001100000000111, kVO_V_B)                                                   , kRWI_X    , 0                         , 6  ), // #438
  INST(Cls_v            , ISimdVV            , (0b0000111000100000010010, kVO_V_BHS)                                                 , kRWI_W    , 0                         , 1  ), // #439
  INST(Clz_v            , ISimdVV            , (0b0010111000100000010010, kVO_V_BHS)                                                 , kRWI_W    , 0                         , 2  ), // #440
  INST(Cmeq_v           , SimdCmp            , (0b0010111000100000100011, 0b0000111000100000100110, kVO_V_Any)                       , kRWI_W    , 0                         , 0  ), // #441
  INST(Cmge_v           , SimdCmp            , (0b0000111000100000001111, 0b0010111000100000100010, kVO_V_Any)                       , kRWI_W    , 0                         , 1  ), // #442
  INST(Cmgt_v           , SimdCmp            , (0b0000111000100000001101, 0b0000111000100000100010, kVO_V_Any)                       , kRWI_W    , 0                         , 2  ), // #443
  INST(Cmhi_v           , SimdCmp            , (0b0010111000100000001101, 0b0000000000000000000000, kVO_V_Any)                       , kRWI_W    , 0                         , 3  ), // #444
  INST(Cmhs_v           , SimdCmp            , (0b0010111000100000001111, 0b0000000000000000000000, kVO_V_Any)                       , kRWI_W    , 0                         , 4  ), // #445
  INST(Cmle_v           , SimdCmp            , (0b0000000000000000000000, 0b0010111000100000100110, kVO_V_Any)                       , kRWI_W    , 0                         , 5  ), // #446
  INST(Cmlt_v           , SimdCmp            , (0b0000000000000000000000, 0b0000111000100000101010, kVO_V_Any)                       , kRWI_W    , 0                         , 6  ), // #447
  INST(Cmtst_v          , ISimdVVV           , (0b0000111000100000100011, kVO_V_Any)                                                 , kRWI_W    , 0                         , 7  ), // #448
  INST(Cnt_v            , ISimdVV            , (0b0000111000100000010110, kVO_V_B)                                                   , kRWI_W    , 0                         , 3  ), // #449
  INST(Dup_v            , SimdDup            , (_)                                                                                   , kRWI_W    , 0                         , 0  ), // #450
  INST(Eor_v            , ISimdVVV           , (0b0010111000100000000111, kVO_V_B)                                                   , kRWI_W    , 0                         , 8  ), // #451
  INST(Eor3_v           , ISimdVVVV          , (0b1100111000000000000000, kVO_V_B16)                                                 , kRWI_W    , 0                         , 1  ), // #452
  INST(Ext_v            , ISimdVVVI          , (0b0010111000000000000000, kVO_V_B, 4, 11, 1)                                         , kRWI_W    , 0                         , 0  ), // #453
  INST(Fabd_v           , FSimdVVV           , (0b0111111010100000110101, kHF_C, 0b0010111010100000110101, kHF_C)                    , kRWI_W    , 0                         , 0  ), // #454
  INST(Fabs_v           , FSimdVV            , (0b0001111000100000110000, kHF_A, 0b0000111010100000111110, kHF_B)                    , kRWI_W    , 0                         , 0  ), // #455
  INST(Facge_v          , FSimdVVV           , (0b0111111000100000111011, kHF_C, 0b0010111000100000111011, kHF_C)                    , kRWI_W    , 0                         , 1  ), // #456
  INST(Facgt_v          , FSimdVVV           , (0b0111111010100000111011, kHF_C, 0b0010111010100000111011, kHF_C)                    , kRWI_W    , 0                         , 2  ), // #457
  INST(Fadd_v           , FSimdVVV           , (0b0001111000100000001010, kHF_A, 0b0000111000100000110101, kHF_C)                    , kRWI_W    , 0                         , 3  ), // #458
  INST(Faddp_v          , FSimdPair          , (0b0111111000110000110110, 0b0010111000100000110101)                                  , kRWI_W    , 0                         , 0  ), // #459
  INST(Fcadd_v          , SimdFcadd          , (0b0010111000000000111001)                                                            , kRWI_W    , 0                         , 0  ), // #460
  INST(Fccmp_v          , SimdFccmpFccmpe    , (0b00011110001000000000010000000000)                                                  , kRWI_R    , 0                         , 0  ), // #461
  INST(Fccmpe_v         , SimdFccmpFccmpe    , (0b00011110001000000000010000010000)                                                  , kRWI_R    , 0                         , 1  ), // #462
  INST(Fcmeq_v          , SimdFcm            , (0b0000111000100000111001, kHF_C, 0b0000111010100000110110)                           , kRWI_W    , 0                         , 0  ), // #463
  INST(Fcmge_v          , SimdFcm            , (0b0010111000100000111001, kHF_C, 0b0010111010100000110010)                           , kRWI_W    , 0                         , 1  ), // #464
  INST(Fcmgt_v          , SimdFcm            , (0b0010111010100000111001, kHF_C, 0b0000111010100000110010)                           , kRWI_W    , 0                         , 2  ), // #465
  INST(Fcmla_v          , SimdFcmla          , (0b0010111000000000110001, 0b0010111100000000000100)                                  , kRWI_X    , 0                         , 0  ), // #466
  INST(Fcmle_v          , SimdFcm            , (0b0000000000000000000000, kHF_C, 0b0010111010100000110110)                           , kRWI_W    , 0                         , 3  ), // #467
  INST(Fcmlt_v          , SimdFcm            , (0b0000000000000000000000, kHF_C, 0b0000111010100000111010)                           , kRWI_W    , 0                         , 4  ), // #468
  INST(Fcmp_v           , SimdFcmpFcmpe      , (0b00011110001000000010000000000000)                                                  , kRWI_R    , 0                         , 0  ), // #469
  INST(Fcmpe_v          , SimdFcmpFcmpe      , (0b00011110001000000010000000010000)                                                  , kRWI_R    , 0                         , 1  ), // #470
  INST(Fcsel_v          , SimdFcsel          , (_)                                                                                   , kRWI_W    , 0                         , 0  ), // #471
  INST(Fcvt_v           , SimdFcvt           , (_)                                                                                   , kRWI_W    , 0                         , 0  ), // #472
  INST(Fcvtas_v         , SimdFcvtSV         , (0b0000111000100001110010, 0b0000000000000000000000, 0b0001111000100100000000, 1)     , kRWI_W    , 0                         , 0  ), // #473
  INST(Fcvtau_v         , SimdFcvtSV         , (0b0010111000100001110010, 0b0000000000000000000000, 0b0001111000100101000000, 1)     , kRWI_W    , 0                         , 1  ), // #474
  INST(Fcvtl_v          , SimdFcvtLN         , (0b0000111000100001011110, 0, 0)                                                      , kRWI_W    , F(Long)                   , 0  ), // #475
  INST(Fcvtl2_v         , SimdFcvtLN         , (0b0100111000100001011110, 0, 0)                                                      , kRWI_W    , F(Long)                   , 1  ), // #476
  INST(Fcvtms_v         , SimdFcvtSV         , (0b0000111000100001101110, 0b0000000000000000000000, 0b0001111000110000000000, 1)     , kRWI_W    , 0                         , 2  ), // #477
  INST(Fcvtmu_v         , SimdFcvtSV         , (0b0010111000100001101110, 0b0000000000000000000000, 0b0001111000110001000000, 1)     , kRWI_W    , 0                         , 3  ), // #478
  INST(Fcvtn_v          , SimdFcvtLN         , (0b0000111000100001011010, 0, 0)                                                      , kRWI_W    , F(Narrow)                 , 2  ), // #479
  INST(Fcvtn2_v         , SimdFcvtLN         , (0b0100111000100001011010, 0, 0)                                                      , kRWI_X    , F(Narrow)                 , 3  ), // #480
  INST(Fcvtns_v         , SimdFcvtSV         , (0b0000111000100001101010, 0b0000000000000000000000, 0b0001111000100000000000, 1)     , kRWI_W    , 0                         , 4  ), // #481
  INST(Fcvtnu_v         , SimdFcvtSV         , (0b0010111000100001101010, 0b0000000000000000000000, 0b0001111000100001000000, 1)     , kRWI_W    , 0                         , 5  ), // #482
  INST(Fcvtps_v         , SimdFcvtSV         , (0b0000111010100001101010, 0b0000000000000000000000, 0b0001111000101000000000, 1)     , kRWI_W    , 0                         , 6  ), // #483
  INST(Fcvtpu_v         , SimdFcvtSV         , (0b0010111010100001101010, 0b0000000000000000000000, 0b0001111000101001000000, 1)     , kRWI_W    , 0                         , 7  ), // #484
  INST(Fcvtxn_v         , SimdFcvtLN         , (0b0010111000100001011010, 1, 1)                                                      , kRWI_W    , F(Narrow)                 , 4  ), // #485
  INST(Fcvtxn2_v        , SimdFcvtLN         , (0b0110111000100001011010, 1, 0)                                                      , kRWI_X    , F(Narrow)                 , 5  ), // #486
  INST(Fcvtzs_v         , SimdFcvtSV         , (0b0000111010100001101110, 0b0000111100000000111111, 0b0001111000111000000000, 1)     , kRWI_W    , 0                         , 8  ), // #487
  INST(Fcvtzu_v         , SimdFcvtSV         , (0b0010111010100001101110, 0b0010111100000000111111, 0b0001111000111001000000, 1)     , kRWI_W    , 0                         , 9  ), // #488
  INST(Fdiv_v           , FSimdVVV           , (0b0001111000100000000110, kHF_A, 0b0010111000100000111111, kHF_C)                    , kRWI_W    , 0                         , 4  ), // #489
  INST(Fjcvtzs_v        , ISimdVVx           , (0b0001111001111110000000, kOp_GpW, kOp_D)                                            , kRWI_W    , 0                         , 7  ), // #490
  INST(Fmadd_v          , FSimdVVVV          , (0b0001111100000000000000, kHF_A, 0b0000000000000000000000, kHF_N)                    , kRWI_W    , 0                         , 0  ), // #491
  INST(Fmax_v           , FSimdVVV           , (0b0001111000100000010010, kHF_A, 0b0000111000100000111101, kHF_C)                    , kRWI_W    , 0                         , 5  ), // #492
  INST(Fmaxnm_v         , FSimdVVV           , (0b0001111000100000011010, kHF_A, 0b0000111000100000110001, kHF_C)                    , kRWI_W    , 0                         , 6  ), // #493
  INST(Fmaxnmp_v        , FSimdPair          , (0b0111111000110000110010, 0b0010111000100000110001)                                  , kRWI_W    , 0                         , 1  ), // #494
  INST(Fmaxnmv_v        , FSimdSV            , (0b0010111000110000110010)                                                            , kRWI_W    , 0                         , 0  ), // #495
  INST(Fmaxp_v          , FSimdPair          , (0b0111111000110000111110, 0b0010111000100000111101)                                  , kRWI_W    , 0                         , 2  ), // #496
  INST(Fmaxv_v          , FSimdSV            , (0b0010111000110000111110)                                                            , kRWI_W    , 0                         , 1  ), // #497
  INST(Fmin_v           , FSimdVVV           , (0b0001111000100000010110, kHF_A, 0b0000111010100000111101, kHF_C)                    , kRWI_W    , 0                         , 7  ), // #498
  INST(Fminnm_v         , FSimdVVV           , (0b0001111000100000011110, kHF_A, 0b0000111010100000110001, kHF_C)                    , kRWI_W    , 0                         , 8  ), // #499
  INST(Fminnmp_v        , FSimdPair          , (0b0111111010110000110010, 0b0010111010100000110001)                                  , kRWI_W    , 0                         , 3  ), // #500
  INST(Fminnmv_v        , FSimdSV            , (0b0010111010110000110010)                                                            , kRWI_W    , 0                         , 2  ), // #501
  INST(Fminp_v          , FSimdPair          , (0b0111111010110000111110, 0b0010111010100000111101)                                  , kRWI_W    , 0                         , 4  ), // #502
  INST(Fminv_v          , FSimdSV            , (0b0010111010110000111110)                                                            , kRWI_W    , 0                         , 3  ), // #503
  INST(Fmla_v           , FSimdVVVe          , (0b0000000000000000000000, kHF_N, 0b0000111000100000110011, 0b0000111110000000000100) , kRWI_X    , F(VH0_15)                 , 0  ), // #504
  INST(Fmlal_v          , SimdFmlal          , (0b0000111000100000111011, 0b0000111110000000000000, 1, kET_S, kET_H, kET_H)          , kRWI_X    , F(VH0_15)                 , 2  ), // #505
  INST(Fmlal2_v         , SimdFmlal          , (0b0010111000100000110011, 0b0010111110000000100000, 1, kET_S, kET_H, kET_H)          , kRWI_X    , F(VH0_15)                 , 3  ), // #506
  INST(Fmls_v           , FSimdVVVe          , (0b0000000000000000000000, kHF_N, 0b0000111010100000110011, 0b0000111110000000010100) , kRWI_X    , F(VH0_15)                 , 1  ), // #507
  INST(Fmlsl_v          , SimdFmlal          , (0b0000111010100000111011, 0b0000111110000000010000, 1, kET_S, kET_H, kET_H)          , kRWI_X    , F(VH0_15)                 , 4  ), // #508
  INST(Fmlsl2_v         , SimdFmlal          , (0b0010111010100000110011, 0b0010111110000000110000, 1, kET_S, kET_H, kET_H)          , kRWI_X    , F(VH0_15)                 , 5  ), // #509
  INST(Fmov_v           , SimdFmov           , (_)                                                                                   , kRWI_W    , 0                         , 0  ), // #510
  INST(Fmsub_v          , FSimdVVVV          , (0b0001111100000000100000, kHF_A, 0b0000000000000000000000, kHF_N)                    , kRWI_W    , 0                         , 1  ), // #511
  INST(Fmul_v           , FSimdVVVe          , (0b0001111000100000000010, kHF_A, 0b0010111000100000110111, 0b0000111110000000100100) , kRWI_W    , F(VH0_15)                 , 2  ), // #512
  INST(Fmulx_v          , FSimdVVVe          , (0b0101111000100000110111, kHF_C, 0b0000111000100000110111, 0b0010111110000000100100) , kRWI_W    , F(VH0_15)                 , 3  ), // #513
  INST(Fneg_v           , FSimdVV            , (0b0001111000100001010000, kHF_A, 0b0010111010100000111110, kHF_B)                    , kRWI_W    , 0                         , 1  ), // #514
  INST(Fnmadd_v         , FSimdVVVV          , (0b0001111100100000000000, kHF_A, 0b0000000000000000000000, kHF_N)                    , kRWI_W    , 0                         , 2  ), // #515
  INST(Fnmsub_v         , FSimdVVVV          , (0b0001111100100000100000, kHF_A, 0b0000000000000000000000, kHF_N)                    , kRWI_W    , 0                         , 3  ), // #516
  INST(Fnmul_v          , FSimdVVV           , (0b0001111000100000100010, kHF_A, 0b0000000000000000000000, kHF_N)                    , kRWI_W    , 0                         , 9  ), // #517
  INST(Frecpe_v         , FSimdVV            , (0b0101111010100001110110, kHF_B, 0b0000111010100001110110, kHF_B)                    , kRWI_W    , 0                         , 2  ), // #518
  INST(Frecps_v         , FSimdVVV           , (0b0101111000100000111111, kHF_C, 0b0000111000100000111111, kHF_C)                    , kRWI_W    , 0                         , 10 ), // #519
  INST(Frecpx_v         , FSimdVV            , (0b0101111010100001111110, kHF_B, 0b0000000000000000000000, kHF_N)                    , kRWI_W    , 0                         , 3  ), // #520
  INST(Frint32x_v       , FSimdVV            , (0b0001111000101000110000, kHF_N, 0b0010111000100001111010, kHF_N)                    , kRWI_W    , 0                         , 4  ), // #521
  INST(Frint32z_v       , FSimdVV            , (0b0001111000101000010000, kHF_N, 0b0000111000100001111010, kHF_N)                    , kRWI_W    , 0                         , 5  ), // #522
  INST(Frint64x_v       , FSimdVV            , (0b0001111000101001110000, kHF_N, 0b0010111000100001111110, kHF_N)                    , kRWI_W    , 0                         , 6  ), // #523
  INST(Frint64z_v       , FSimdVV            , (0b0001111000101001010000, kHF_N, 0b0000111000100001111110, kHF_N)                    , kRWI_W    , 0                         , 7  ), // #524
  INST(Frinta_v         , FSimdVV            , (0b0001111000100110010000, kHF_A, 0b0010111000100001100010, kHF_B)                    , kRWI_W    , 0                         , 8  ), // #525
  INST(Frinti_v         , FSimdVV            , (0b0001111000100111110000, kHF_A, 0b0010111010100001100110, kHF_B)                    , kRWI_W    , 0                         , 9  ), // #526
  INST(Frintm_v         , FSimdVV            , (0b0001111000100101010000, kHF_A, 0b0000111000100001100110, kHF_B)                    , kRWI_W    , 0                         , 10 ), // #527
  INST(Frintn_v         , FSimdVV            , (0b0001111000100100010000, kHF_A, 0b0000111000100001100010, kHF_B)                    , kRWI_W    , 0                         , 11 ), // #528
  INST(Frintp_v         , FSimdVV            , (0b0001111000100100110000, kHF_A, 0b0000111010100001100010, kHF_B)                    , kRWI_W    , 0                         , 12 ), // #529
  INST(Frintx_v         , FSimdVV            , (0b0001111000100111010000, kHF_A, 0b0010111000100001100110, kHF_B)                    , kRWI_W    , 0                         , 13 ), // #530
  INST(Frintz_v         , FSimdVV            , (0b0001111000100101110000, kHF_A, 0b0000111010100001100110, kHF_B)                    , kRWI_W    , 0                         , 14 ), // #531
  INST(Frsqrte_v        , FSimdVV            , (0b0111111010100001110110, kHF_B, 0b0010111010100001110110, kHF_B)                    , kRWI_W    , 0                         , 15 ), // #532
  INST(Frsqrts_v        , FSimdVVV           , (0b0101111010100000111111, kHF_C, 0b0000111010100000111111, kHF_C)                    , kRWI_W    , 0                         , 11 ), // #533
  INST(Fsqrt_v          , FSimdVV            , (0b0001111000100001110000, kHF_A, 0b0010111010100001111110, kHF_B)                    , kRWI_W    , 0                         , 16 ), // #534
  INST(Fsub_v           , FSimdVVV           , (0b0001111000100000001110, kHF_A, 0b0000111010100000110101, kHF_C)                    , kRWI_W    , 0                         , 12 ), // #535
  INST(Ins_v            , SimdIns            , (_)                                                                                   , kRWI_X    , 0                         , 0  ), // #536
  INST(Ld1_v            , SimdLdNStN         , (0b0000110101000000000000, 0b0000110001000000001000, 1, 0)                            , kRWI_LDn  , F(Consecutive)            , 0  ), // #537
  INST(Ld1r_v           , SimdLdNStN         , (0b0000110101000000110000, 0b0000000000000000000000, 1, 1)                            , kRWI_LDn  , F(Consecutive)            , 1  ), // #538
  INST(Ld2_v            , SimdLdNStN         , (0b0000110101100000000000, 0b0000110001000000100000, 2, 0)                            , kRWI_LDn  , F(Consecutive)            , 2  ), // #539
  INST(Ld2r_v           , SimdLdNStN         , (0b0000110101100000110000, 0b0000000000000000000000, 2, 1)                            , kRWI_LDn  , F(Consecutive)            , 3  ), // #540
  INST(Ld3_v            , SimdLdNStN         , (0b0000110101000000001000, 0b0000110001000000010000, 3, 0)                            , kRWI_LDn  , F(Consecutive)            , 4  ), // #541
  INST(Ld3r_v           , SimdLdNStN         , (0b0000110101000000111000, 0b0000000000000000000000, 3, 1)                            , kRWI_LDn  , F(Consecutive)            , 5  ), // #542
  INST(Ld4_v            , SimdLdNStN         , (0b0000110101100000001000, 0b0000110001000000000000, 4, 0)                            , kRWI_LDn  , F(Consecutive)            , 6  ), // #543
  INST(Ld4r_v           , SimdLdNStN         , (0b0000110101100000111000, 0b0000000000000000000000, 4, 1)                            , kRWI_LDn  , F(Consecutive)            , 7  ), // #544
  INST(Ldnp_v           , SimdLdpStp         , (0b0010110001, 0b0000000000)                                                          , kRWI_WW   , 0                         , 0  ), // #545
  INST(Ldp_v            , SimdLdpStp         , (0b0010110101, 0b0010110011)                                                          , kRWI_WW   , 0                         , 1  ), // #546
  INST(Ldr_v            , SimdLdSt           , (0b0011110101, 0b00111100010, 0b00111100011, 0b00011100, Inst::kIdLdur_v)             , kRWI_W    , 0                         , 0  ), // #547
  INST(Ldur_v           , SimdLdurStur       , (0b0011110001000000000000)                                                            , kRWI_W    , 0                         , 0  ), // #548
  INST(Mla_v            , ISimdVVVe          , (0b0000111000100000100101, kVO_V_BHS, 0b0010111100000000000000, kVO_V_HS)             , kRWI_X    , F(VH0_15)                 , 0  ), // #549
  INST(Mls_v            , ISimdVVVe          , (0b0010111000100000100101, kVO_V_BHS, 0b0010111100000000010000, kVO_V_HS)             , kRWI_X    , F(VH0_15)                 , 1  ), // #550
  INST(Mov_v            , SimdMov            , (_)                                                                                   , kRWI_W    , 0                         , 0  ), // #551
  INST(Movi_v           , SimdMoviMvni       , (0b0000111100000000000001, 0)                                                         , kRWI_W    , 0                         , 0  ), // #552
  INST(Mul_v            , ISimdVVVe          , (0b0000111000100000100111, kVO_V_BHS, 0b0000111100000000100000, kVO_V_HS)             , kRWI_W    , F(VH0_15)                 , 2  ), // #553
  INST(Mvn_v            , ISimdVV            , (0b0010111000100000010110, kVO_V_B)                                                   , kRWI_W    , 0                         , 4  ), // #554
  INST(Mvni_v           , SimdMoviMvni       , (0b0000111100000000000001, 1)                                                         , kRWI_W    , 0                         , 1  ), // #555
  INST(Neg_v            , ISimdVV            , (0b0010111000100000101110, kVO_V_Any)                                                 , kRWI_W    , 0                         , 5  ), // #556
  INST(Not_v            , ISimdVV            , (0b0010111000100000010110, kVO_V_B)                                                   , kRWI_W    , 0                         , 6  ), // #557
  INST(Orn_v            , ISimdVVV           , (0b0000111011100000000111, kVO_V_B)                                                   , kRWI_W    , 0                         , 9  ), // #558
  INST(Orr_v            , SimdBicOrr         , (0b0000111010100000000111, 0b0000111100000000000001)                                  , kRWI_W    , 0                         , 1  ), // #559
  INST(Pmul_v           , ISimdVVV           , (0b0010111000100000100111, kVO_V_B)                                                   , kRWI_W    , 0                         , 10 ), // #560
  INST(Pmull_v          , ISimdVVV           , (0b0000111000100000111000, kVO_V_B8D1)                                                , kRWI_W    , F(Long)                   , 11 ), // #561
  INST(Pmull2_v         , ISimdVVV           , (0b0100111000100000111000, kVO_V_B16D2)                                               , kRWI_W    , F(Long)                   , 12 ), // #562
  INST(Raddhn_v         , ISimdVVV           , (0b0010111000100000010000, kVO_V_B8H4S2)                                              , kRWI_W    , F(Narrow)                 , 13 ), // #563
  INST(Raddhn2_v        , ISimdVVV           , (0b0110111000100000010000, kVO_V_B16H8S4)                                             , kRWI_X    , F(Narrow)                 , 14 ), // #564
  INST(Rax1_v           , ISimdVVV           , (0b1100111001100000100011, kVO_V_D2)                                                  , kRWI_W    , 0                         , 15 ), // #565
  INST(Rbit_v           , ISimdVV            , (0b0010111001100000010110, kVO_V_B)                                                   , kRWI_W    , 0                         , 7  ), // #566
  INST(Rev16_v          , ISimdVV            , (0b0000111000100000000110, kVO_V_B)                                                   , kRWI_W    , 0                         , 8  ), // #567
  INST(Rev32_v          , ISimdVV            , (0b0010111000100000000010, kVO_V_BH)                                                  , kRWI_W    , 0                         , 9  ), // #568
  INST(Rev64_v          , ISimdVV            , (0b0000111000100000000010, kVO_V_BHS)                                                 , kRWI_W    , 0                         , 10 ), // #569
  INST(Rshrn_v          , SimdShift          , (0b0000000000000000000000, 0b0000111100000000100011, 1, kVO_V_B8H4S2)                 , kRWI_W    , F(Narrow)                 , 0  ), // #570
  INST(Rshrn2_v         , SimdShift          , (0b0000000000000000000000, 0b0100111100000000100011, 1, kVO_V_B16H8S4)                , kRWI_X    , F(Narrow)                 , 1  ), // #571
  INST(Rsubhn_v         , ISimdVVV           , (0b0010111000100000011000, kVO_V_B8H4S2)                                              , kRWI_W    , F(Narrow)                 , 16 ), // #572
  INST(Rsubhn2_v        , ISimdVVV           , (0b0110111000100000011000, kVO_V_B16H8S4)                                             , kRWI_X    , F(Narrow)                 , 17 ), // #573
  INST(Saba_v           , ISimdVVV           , (0b0000111000100000011111, kVO_V_BHS)                                                 , kRWI_X    , 0                         , 18 ), // #574
  INST(Sabal_v          , ISimdVVV           , (0b0000111000100000010100, kVO_V_B8H4S2)                                              , kRWI_X    , F(Long)                   , 19 ), // #575
  INST(Sabal2_v         , ISimdVVV           , (0b0100111000100000010100, kVO_V_B16H8S4)                                             , kRWI_X    , F(Long)                   , 20 ), // #576
  INST(Sabd_v           , ISimdVVV           , (0b0000111000100000011101, kVO_V_BHS)                                                 , kRWI_W    , 0                         , 21 ), // #577
  INST(Sabdl_v          , ISimdVVV           , (0b0000111000100000011100, kVO_V_B8H4S2)                                              , kRWI_W    , F(Long)                   , 22 ), // #578
  INST(Sabdl2_v         , ISimdVVV           , (0b0100111000100000011100, kVO_V_B16H8S4)                                             , kRWI_W    , F(Long)                   , 23 ), // #579
  INST(Sadalp_v         , ISimdVV            , (0b0000111000100000011010, kVO_V_BHS)                                                 , kRWI_X    , F(Long) | F(Pair)         , 11 ), // #580
  INST(Saddl_v          , ISimdVVV           , (0b0000111000100000000000, kVO_V_B8H4S2)                                              , kRWI_W    , F(Long)                   , 24 ), // #581
  INST(Saddl2_v         , ISimdVVV           , (0b0100111000100000000000, kVO_V_B16H8S4)                                             , kRWI_W    , F(Long)                   , 25 ), // #582
  INST(Saddlp_v         , ISimdVV            , (0b0000111000100000001010, kVO_V_BHS)                                                 , kRWI_W    , F(Long) | F(Pair)         , 12 ), // #583
  INST(Saddlv_v         , ISimdSV            , (0b0000111000110000001110, kVO_V_BH_4S)                                               , kRWI_W    , F(Long)                   , 1  ), // #584
  INST(Saddw_v          , ISimdWWV           , (0b0000111000100000000100, kVO_V_B8H4S2)                                              , kRWI_W    , 0                         , 0  ), // #585
  INST(Saddw2_v         , ISimdWWV           , (0b0000111000100000000100, kVO_V_B16H8S4)                                             , kRWI_W    , 0                         , 1  ), // #586
  INST(Scvtf_v          , SimdFcvtSV         , (0b0000111000100001110110, 0b0000111100000000111001, 0b0001111000100010000000, 0)     , kRWI_W    , 0                         , 10 ), // #587
  INST(Sdot_v           , SimdDot            , (0b0000111010000000100101, 0b0000111110000000111000, kET_S, kET_B, kET_4B)            , kRWI_X    , 0                         , 1  ), // #588
  INST(Sha1c_v          , ISimdVVVx          , (0b0101111000000000000000, kOp_Q, kOp_S, kOp_V4S)                                     , kRWI_X    , 0                         , 1  ), // #589
  INST(Sha1h_v          , ISimdVVx           , (0b0101111000101000000010, kOp_S, kOp_S)                                              , kRWI_W    , 0                         , 8  ), // #590
  INST(Sha1m_v          , ISimdVVVx          , (0b0101111000000000001000, kOp_Q, kOp_S, kOp_V4S)                                     , kRWI_X    , 0                         , 2  ), // #591
  INST(Sha1p_v          , ISimdVVVx          , (0b0101111000000000000100, kOp_Q, kOp_S, kOp_V4S)                                     , kRWI_X    , 0                         , 3  ), // #592
  INST(Sha1su0_v        , ISimdVVVx          , (0b0101111000000000001100, kOp_V4S, kOp_V4S, kOp_V4S)                                 , kRWI_X    , 0                         , 4  ), // #593
  INST(Sha1su1_v        , ISimdVVx           , (0b0101111000101000000110, kOp_V4S, kOp_V4S)                                          , kRWI_X    , 0                         , 9  ), // #594
  INST(Sha256h_v        , ISimdVVVx          , (0b0101111000000000010000, kOp_Q, kOp_Q, kOp_V4S)                                     , kRWI_X    , 0                         , 5  ), // #595
  INST(Sha256h2_v       , ISimdVVVx          , (0b0101111000000000010100, kOp_Q, kOp_Q, kOp_V4S)                                     , kRWI_X    , 0                         , 6  ), // #596
  INST(Sha256su0_v      , ISimdVVx           , (0b0101111000101000001010, kOp_V4S, kOp_V4S)                                          , kRWI_X    , 0                         , 10 ), // #597
  INST(Sha256su1_v      , ISimdVVVx          , (0b0101111000000000011000, kOp_V4S, kOp_V4S, kOp_V4S)                                 , kRWI_X    , 0                         , 7  ), // #598
  INST(Sha512h_v        , ISimdVVVx          , (0b1100111001100000100000, kOp_Q, kOp_Q, kOp_V2D)                                     , kRWI_X    , 0                         , 8  ), // #599
  INST(Sha512h2_v       , ISimdVVVx          , (0b1100111001100000100001, kOp_Q, kOp_Q, kOp_V2D)                                     , kRWI_X    , 0                         , 9  ), // #600
  INST(Sha512su0_v      , ISimdVVx           , (0b1100111011000000100000, kOp_V2D, kOp_V2D)                                          , kRWI_X    , 0                         , 11 ), // #601
  INST(Sha512su1_v      , ISimdVVVx          , (0b1100111001100000100010, kOp_V2D, kOp_V2D, kOp_V2D)                                 , kRWI_X    , 0                         , 10 ), // #602
  INST(Shadd_v          , ISimdVVV           , (0b0000111000100000000001, kVO_V_BHS)                                                 , kRWI_W    , 0                         , 26 ), // #603
  INST(Shl_v            , SimdShift          , (0b0000000000000000000000, 0b0000111100000000010101, 0, kVO_V_Any)                    , kRWI_W    , 0                         , 2  ), // #604
  INST(Shll_v           , SimdShiftES        , (0b0010111000100001001110, kVO_V_B8H4S2)                                              , kRWI_W    , F(Long)                   , 0  ), // #605
  INST(Shll2_v          , SimdShiftES        , (0b0110111000100001001110, kVO_V_B16H8S4)                                             , kRWI_W    , F(Long)                   , 1  ), // #606
  INST(Shrn_v           , SimdShift          , (0b0000000000000000000000, 0b0000111100000000100001, 1, kVO_V_B8H4S2)                 , kRWI_W    , F(Narrow)                 , 3  ), // #607
  INST(Shrn2_v          , SimdShift          , (0b0000000000000000000000, 0b0100111100000000100001, 1, kVO_V_B16H8S4)                , kRWI_X    , F(Narrow)                 , 4  ), // #608
  INST(Shsub_v          , ISimdVVV           , (0b0000111000100000001001, kVO_V_BHS)                                                 , kRWI_W    , 0                         , 27 ), // #609
  INST(Sli_v            , SimdShift          , (0b0000000000000000000000, 0b0010111100000000010101, 0, kVO_V_Any)                    , kRWI_X    , 0                         , 5  ), // #610
  INST(Sm3partw1_v      , ISimdVVVx          , (0b1100111001100000110000, kOp_V4S, kOp_V4S, kOp_V4S)                                 , kRWI_X    , 0                         , 11 ), // #611
  INST(Sm3partw2_v      , ISimdVVVx          , (0b1100111001100000110001, kOp_V4S, kOp_V4S, kOp_V4S)                                 , kRWI_X    , 0                         , 12 ), // #612
  INST(Sm3ss1_v         , ISimdVVVVx         , (0b1100111001000000000000, kOp_V4S, kOp_V4S, kOp_V4S, kOp_V4S)                        , kRWI_W    , 0                         , 0  ), // #613
  INST(Sm3tt1a_v        , SimdSm3tt          , (0b1100111001000000100000)                                                            , kRWI_X    , 0                         , 0  ), // #614
  INST(Sm3tt1b_v        , SimdSm3tt          , (0b1100111001000000100001)                                                            , kRWI_X    , 0                         , 1  ), // #615
  INST(Sm3tt2a_v        , SimdSm3tt          , (0b1100111001000000100010)                                                            , kRWI_X    , 0                         , 2  ), // #616
  INST(Sm3tt2b_v        , SimdSm3tt          , (0b1100111001000000100011)                                                            , kRWI_X    , 0                         , 3  ), // #617
  INST(Sm4e_v           , ISimdVVx           , (0b1100111011000000100001, kOp_V4S, kOp_V4S)                                          , kRWI_X    , 0                         , 12 ), // #618
  INST(Sm4ekey_v        , ISimdVVVx          , (0b1100111001100000110010, kOp_V4S, kOp_V4S, kOp_V4S)                                 , kRWI_X    , 0                         , 13 ), // #619
  INST(Smax_v           , ISimdVVV           , (0b0000111000100000011001, kVO_V_BHS)                                                 , kRWI_W    , 0                         , 28 ), // #620
  INST(Smaxp_v          , ISimdVVV           , (0b0000111000100000101001, kVO_V_BHS)                                                 , kRWI_W    , 0                         , 29 ), // #621
  INST(Smaxv_v          , ISimdSV            , (0b0000111000110000101010, kVO_V_BH_4S)                                               , kRWI_W    , 0                         , 2  ), // #622
  INST(Smin_v           , ISimdVVV           , (0b0000111000100000011011, kVO_V_BHS)                                                 , kRWI_W    , 0                         , 30 ), // #623
  INST(Sminp_v          , ISimdVVV           , (0b0000111000100000101011, kVO_V_BHS)                                                 , kRWI_W    , 0                         , 31 ), // #624
  INST(Sminv_v          , ISimdSV            , (0b0000111000110001101010, kVO_V_BH_4S)                                               , kRWI_W    , 0                         , 3  ), // #625
  INST(Smlal_v          , ISimdVVVe          , (0b0000111000100000100000, kVO_V_B8H4S2, 0b0000111100000000001000, kVO_V_H4S2)        , kRWI_X    , F(Long) | F(VH0_15)       , 3  ), // #626
  INST(Smlal2_v         , ISimdVVVe          , (0b0100111000100000100000, kVO_V_B16H8S4, 0b0100111100000000001000, kVO_V_H8S4)       , kRWI_X    , F(Long) | F(VH0_15)       , 4  ), // #627
  INST(Smlsl_v          , ISimdVVVe          , (0b0000111000100000101000, kVO_V_B8H4S2, 0b0000111100000000011000, kVO_V_H4S2)        , kRWI_X    , F(Long) | F(VH0_15)       , 5  ), // #628
  INST(Smlsl2_v         , ISimdVVVe          , (0b0100111000100000101000, kVO_V_B16H8S4, 0b0100111100000000011000, kVO_V_H8S4)       , kRWI_X    , F(Long) | F(VH0_15)       , 6  ), // #629
  INST(Smmla_v          , ISimdVVVx          , (0b0100111010000000101001, kOp_V4S, kOp_V16B, kOp_V16B)                               , kRWI_X    , 0                         , 14 ), // #630
  INST(Smov_v           , SimdSmovUmov       , (0b0000111000000000001011, kVO_V_BHS, 1)                                              , kRWI_W    , 0                         , 0  ), // #631
  INST(Smull_v          , ISimdVVVe          , (0b0000111000100000110000, kVO_V_B8H4S2, 0b0000111100000000101000, kVO_V_H4S2)        , kRWI_W    , F(Long) | F(VH0_15)       , 7  ), // #632
  INST(Smull2_v         , ISimdVVVe          , (0b0100111000100000110000, kVO_V_B16H8S4, 0b0100111100000000101000, kVO_V_H8S4)       , kRWI_W    , F(Long) | F(VH0_15)       , 8  ), // #633
  INST(Sqabs_v          , ISimdVV            , (0b0000111000100000011110, kVO_SV_Any)                                                , kRWI_W    , 0                         , 13 ), // #634
  INST(Sqadd_v          , ISimdVVV           , (0b0000111000100000000011, kVO_SV_Any)                                                , kRWI_W    , 0                         , 32 ), // #635
  INST(Sqdmlal_v        , ISimdVVVe          , (0b0000111000100000100100, kVO_SV_BHS, 0b0000111100000000001100, kVO_V_H4S2)          , kRWI_X    , F(Long) | F(VH0_15)       , 9  ), // #636
  INST(Sqdmlal2_v       , ISimdVVVe          , (0b0100111000100000100100, kVO_V_B16H8S4, 0b0100111100000000001100, kVO_V_H8S4)       , kRWI_X    , F(Long) | F(VH0_15)       , 10 ), // #637
  INST(Sqdmlsl_v        , ISimdVVVe          , (0b0000111000100000101100, kVO_SV_BHS, 0b0000111100000000011100, kVO_V_H4S2)          , kRWI_X    , F(Long) | F(VH0_15)       , 11 ), // #638
  INST(Sqdmlsl2_v       , ISimdVVVe          , (0b0100111000100000101100, kVO_V_B16H8S4, 0b0100111100000000011100, kVO_V_H8S4)       , kRWI_X    , F(Long) | F(VH0_15)       , 12 ), // #639
  INST(Sqdmulh_v        , ISimdVVVe          , (0b0000111000100000101101, kVO_SV_HS, 0b0000111100000000110000, kVO_SV_HS)            , kRWI_W    , F(VH0_15)                 , 13 ), // #640
  INST(Sqdmull_v        , ISimdVVVe          , (0b0000111000100000110100, kVO_SV_BHS, 0b0000111100000000101100, kVO_V_H4S2)          , kRWI_W    , F(Long) | F(VH0_15)       , 14 ), // #641
  INST(Sqdmull2_v       , ISimdVVVe          , (0b0100111000100000110100, kVO_V_B16H8S4, 0b0100111100000000101100, kVO_V_H8S4)       , kRWI_W    , F(Long) | F(VH0_15)       , 15 ), // #642
  INST(Sqneg_v          , ISimdVV            , (0b0010111000100000011110, kVO_SV_Any)                                                , kRWI_W    , 0                         , 14 ), // #643
  INST(Sqrdmlah_v       , ISimdVVVe          , (0b0010111000000000100001, kVO_SV_HS, 0b0010111100000000110100, kVO_SV_HS)            , kRWI_X    , F(VH0_15)                 , 16 ), // #644
  INST(Sqrdmlsh_v       , ISimdVVVe          , (0b0010111000000000100011, kVO_SV_HS, 0b0010111100000000111100, kVO_SV_HS)            , kRWI_X    , F(VH0_15)                 , 17 ), // #645
  INST(Sqrdmulh_v       , ISimdVVVe          , (0b0010111000100000101101, kVO_SV_HS, 0b0000111100000000110100, kVO_SV_HS)            , kRWI_W    , F(VH0_15)                 , 18 ), // #646
  INST(Sqrshl_v         , SimdShift          , (0b0000111000100000010111, 0b0000000000000000000000, 1, kVO_SV_Any)                   , kRWI_W    , 0                         , 6  ), // #647
  INST(Sqrshrn_v        , SimdShift          , (0b0000000000000000000000, 0b0000111100000000100111, 1, kVO_SV_B8H4S2)                , kRWI_W    , F(Narrow)                 , 7  ), // #648
  INST(Sqrshrn2_v       , SimdShift          , (0b0000000000000000000000, 0b0100111100000000100111, 1, kVO_V_B16H8S4)                , kRWI_X    , F(Narrow)                 , 8  ), // #649
  INST(Sqrshrun_v       , SimdShift          , (0b0000000000000000000000, 0b0010111100000000100011, 1, kVO_SV_B8H4S2)                , kRWI_W    , F(Narrow)                 , 9  ), // #650
  INST(Sqrshrun2_v      , SimdShift          , (0b0000000000000000000000, 0b0110111100000000100011, 1, kVO_V_B16H8S4)                , kRWI_X    , F(Narrow)                 , 10 ), // #651
  INST(Sqshl_v          , SimdShift          , (0b0000111000100000010011, 0b0000111100000000011101, 0, kVO_SV_Any)                   , kRWI_W    , 0                         , 11 ), // #652
  INST(Sqshlu_v         , SimdShift          , (0b0000000000000000000000, 0b0010111100000000011001, 0, kVO_SV_Any)                   , kRWI_W    , 0                         , 12 ), // #653
  INST(Sqshrn_v         , SimdShift          , (0b0000000000000000000000, 0b0000111100000000100101, 1, kVO_SV_B8H4S2)                , kRWI_W    , F(Narrow)                 , 13 ), // #654
  INST(Sqshrn2_v        , SimdShift          , (0b0000000000000000000000, 0b0100111100000000100101, 1, kVO_V_B16H8S4)                , kRWI_X    , F(Narrow)                 , 14 ), // #655
  INST(Sqshrun_v        , SimdShift          , (0b0000000000000000000000, 0b0010111100000000100001, 1, kVO_SV_B8H4S2)                , kRWI_W    , F(Narrow)                 , 15 ), // #656
  INST(Sqshrun2_v       , SimdShift          , (0b0000000000000000000000, 0b0110111100000000100001, 1, kVO_V_B16H8S4)                , kRWI_X    , F(Narrow)                 , 16 ), // #657
  INST(Sqsub_v          , ISimdVVV           , (0b0000111000100000001011, kVO_SV_Any)                                                , kRWI_W    , 0                         , 33 ), // #658
  INST(Sqxtn_v          , ISimdVV            , (0b0000111000100001010010, kVO_SV_B8H4S2)                                             , kRWI_W    , F(Narrow)                 , 15 ), // #659
  INST(Sqxtn2_v         , ISimdVV            , (0b0100111000100001010010, kVO_V_B16H8S4)                                             , kRWI_X    , F(Narrow)                 , 16 ), // #660
  INST(Sqxtun_v         , ISimdVV            , (0b0010111000100001001010, kVO_SV_B8H4S2)                                             , kRWI_W    , F(Narrow)                 , 17 ), // #661
  INST(Sqxtun2_v        , ISimdVV            , (0b0110111000100001001010, kVO_V_B16H8S4)                                             , kRWI_X    , F(Narrow)                 , 18 ), // #662
  INST(Srhadd_v         , ISimdVVV           , (0b0000111000100000000101, kVO_V_BHS)                                                 , kRWI_W    , 0                         , 34 ), // #663
  INST(Sri_v            , SimdShift          , (0b0000000000000000000000, 0b0010111100000000010001, 1, kVO_V_Any)                    , kRWI_W    , 0                         , 17 ), // #664
  INST(Srshl_v          , SimdShift          , (0b0000111000100000010101, 0b0000000000000000000000, 0, kVO_V_Any)                    , kRWI_W    , 0                         , 18 ), // #665
  INST(Srshr_v          , SimdShift          , (0b0000000000000000000000, 0b0000111100000000001001, 1, kVO_V_Any)                    , kRWI_W    , 0                         , 19 ), // #666
  INST(Srsra_v          , SimdShift          , (0b0000000000000000000000, 0b0000111100000000001101, 1, kVO_V_Any)                    , kRWI_X    , 0                         , 20 ), // #667
  INST(Sshl_v           , SimdShift          , (0b0000111000100000010001, 0b0000000000000000000000, 0, kVO_V_Any)                    , kRWI_W    , 0                         , 21 ), // #668
  INST(Sshll_v          , SimdShift          , (0b0000000000000000000000, 0b0000111100000000101001, 0, kVO_V_B8H4S2)                 , kRWI_W    , F(Long)                   , 22 ), // #669
  INST(Sshll2_v         , SimdShift          , (0b0000000000000000000000, 0b0100111100000000101001, 0, kVO_V_B16H8S4)                , kRWI_W    , F(Long)                   , 23 ), // #670
  INST(Sshr_v           , SimdShift          , (0b0000000000000000000000, 0b0000111100000000000001, 1, kVO_V_Any)                    , kRWI_W    , 0                         , 24 ), // #671
  INST(Ssra_v           , SimdShift          , (0b0000000000000000000000, 0b0000111100000000000101, 1, kVO_V_Any)                    , kRWI_X    , 0                         , 25 ), // #672
  INST(Ssubl_v          , ISimdVVV           , (0b0000111000100000001000, kVO_V_B8H4S2)                                              , kRWI_W    , F(Long)                   , 35 ), // #673
  INST(Ssubl2_v         , ISimdVVV           , (0b0100111000100000001000, kVO_V_B16H8S4)                                             , kRWI_W    , F(Long)                   , 36 ), // #674
  INST(Ssubw_v          , ISimdWWV           , (0b0000111000100000001100, kVO_V_B8H4S2)                                              , kRWI_W    , 0                         , 2  ), // #675
  INST(Ssubw2_v         , ISimdWWV           , (0b0000111000100000001100, kVO_V_B16H8S4)                                             , kRWI_X    , 0                         , 3  ), // #676
  INST(St1_v            , SimdLdNStN         , (0b0000110100000000000000, 0b0000110000000000001000, 1, 0)                            , kRWI_STn  , F(Consecutive)            , 8  ), // #677
  INST(St2_v            , SimdLdNStN         , (0b0000110100100000000000, 0b0000110000000000100000, 2, 0)                            , kRWI_STn  , F(Consecutive)            , 9  ), // #678
  INST(St3_v            , SimdLdNStN         , (0b0000110100000000001000, 0b0000110000000000010000, 3, 0)                            , kRWI_STn  , F(Consecutive)            , 10 ), // #679
  INST(St4_v            , SimdLdNStN         , (0b0000110100100000001000, 0b0000110000000000000000, 4, 0)                            , kRWI_STn  , F(Consecutive)            , 11 ), // #680
  INST(Stnp_v           , SimdLdpStp         , (0b0010110000, 0b0000000000)                                                          , kRWI_RRW  , 0                         , 2  ), // #681
  INST(Stp_v            , SimdLdpStp         , (0b0010110100, 0b0010110010)                                                          , kRWI_RRW  , 0                         , 3  ), // #682
  INST(Str_v            , SimdLdSt           , (0b0011110100, 0b00111100000, 0b00111100001, 0b00000000, Inst::kIdStur_v)             , kRWI_RW   , 0                         , 1  ), // #683
  INST(Stur_v           , SimdLdurStur       , (0b0011110000000000000000)                                                            , kRWI_RW   , 0                         , 1  ), // #684
  INST(Sub_v            , ISimdVVV           , (0b0010111000100000100001, kVO_V_Any)                                                 , kRWI_W    , 0                         , 37 ), // #685
  INST(Subhn_v          , ISimdVVV           , (0b0000111000100000011000, kVO_V_B8H4S2)                                              , kRWI_W    , F(Narrow)                 , 38 ), // #686
  INST(Subhn2_v         , ISimdVVV           , (0b0000111000100000011000, kVO_V_B16H8S4)                                             , kRWI_X    , F(Narrow)                 , 39 ), // #687
  INST(Sudot_v          , SimdDot            , (0b0000000000000000000000, 0b0000111100000000111100, kET_S, kET_B, kET_4B)            , kRWI_X    , 0                         , 2  ), // #688
  INST(Suqadd_v         , ISimdVV            , (0b0000111000100000001110, kVO_SV_Any)                                                , kRWI_X    , 0                         , 19 ), // #689
  INST(Sxtl_v           , SimdSxtlUxtl       , (0b0000111100000000101001, kVO_V_B8H4S2)                                              , kRWI_W    , F(Long)                   , 0  ), // #690
  INST(Sxtl2_v          , SimdSxtlUxtl       , (0b0100111100000000101001, kVO_V_B16H8S4)                                             , kRWI_W    , F(Long)                   , 1  ), // #691
  INST(Tbl_v            , SimdTblTbx         , (0b0000111000000000000000)                                                            , kRWI_W    , 0                         , 0  ), // #692
  INST(Tbx_v            , SimdTblTbx         , (0b0000111000000000000100)                                                            , kRWI_W    , 0                         , 1  ), // #693
  INST(Trn1_v           , ISimdVVV           , (0b0000111000000000001010, kVO_V_BHS_D2)                                              , kRWI_W    , 0                         , 40 ), // #694
  INST(Trn2_v           , ISimdVVV           , (0b0000111000000000011010, kVO_V_BHS_D2)                                              , kRWI_W    , 0                         , 41 ), // #695
  INST(Uaba_v           , ISimdVVV           , (0b0010111000100000011111, kVO_V_BHS)                                                 , kRWI_X    , 0                         , 42 ), // #696
  INST(Uabal_v          , ISimdVVV           , (0b0010111000100000010100, kVO_V_B8H4S2)                                              , kRWI_X    , F(Long)                   , 43 ), // #697
  INST(Uabal2_v         , ISimdVVV           , (0b0110111000100000010100, kVO_V_B16H8S4)                                             , kRWI_X    , F(Long)                   , 44 ), // #698
  INST(Uabd_v           , ISimdVVV           , (0b0010111000100000011101, kVO_V_BHS)                                                 , kRWI_W    , 0                         , 45 ), // #699
  INST(Uabdl_v          , ISimdVVV           , (0b0010111000100000011100, kVO_V_B8H4S2)                                              , kRWI_W    , F(Long)                   , 46 ), // #700
  INST(Uabdl2_v         , ISimdVVV           , (0b0110111000100000011100, kVO_V_B16H8S4)                                             , kRWI_W    , F(Long)                   , 47 ), // #701
  INST(Uadalp_v         , ISimdVV            , (0b0010111000100000011010, kVO_V_BHS)                                                 , kRWI_X    , F(Long) | F(Pair)         , 20 ), // #702
  INST(Uaddl_v          , ISimdVVV           , (0b0010111000100000000000, kVO_V_B8H4S2)                                              , kRWI_W    , F(Long)                   , 48 ), // #703
  INST(Uaddl2_v         , ISimdVVV           , (0b0110111000100000000000, kVO_V_B16H8S4)                                             , kRWI_W    , F(Long)                   , 49 ), // #704
  INST(Uaddlp_v         , ISimdVV            , (0b0010111000100000001010, kVO_V_BHS)                                                 , kRWI_W    , F(Long) | F(Pair)         , 21 ), // #705
  INST(Uaddlv_v         , ISimdSV            , (0b0010111000110000001110, kVO_V_BH_4S)                                               , kRWI_W    , F(Long)                   , 4  ), // #706
  INST(Uaddw_v          , ISimdWWV           , (0b0010111000100000000100, kVO_V_B8H4S2)                                              , kRWI_W    , 0                         , 4  ), // #707
  INST(Uaddw2_v         , ISimdWWV           , (0b0010111000100000000100, kVO_V_B16H8S4)                                             , kRWI_W    , 0                         , 5  ), // #708
  INST(Ucvtf_v          , SimdFcvtSV         , (0b0010111000100001110110, 0b0010111100000000111001, 0b0001111000100011000000, 0)     , kRWI_W    , 0                         , 11 ), // #709
  INST(Udot_v           , SimdDot            , (0b0010111010000000100101, 0b0010111110000000111000, kET_S, kET_B, kET_4B)            , kRWI_X    , 0                         , 3  ), // #710
  INST(Uhadd_v          , ISimdVVV           , (0b0010111000100000000001, kVO_V_BHS)                                                 , kRWI_W    , 0                         , 50 ), // #711
  INST(Uhsub_v          , ISimdVVV           , (0b0010111000100000001001, kVO_V_BHS)                                                 , kRWI_W    , 0                         , 51 ), // #712
  INST(Umax_v           , ISimdVVV           , (0b0010111000100000011001, kVO_V_BHS)                                                 , kRWI_W    , 0                         , 52 ), // #713
  INST(Umaxp_v          , ISimdVVV           , (0b0010111000100000101001, kVO_V_BHS)                                                 , kRWI_W    , 0                         , 53 ), // #714
  INST(Umaxv_v          , ISimdSV            , (0b0010111000110000101010, kVO_V_BH_4S)                                               , kRWI_W    , 0                         , 5  ), // #715
  INST(Umin_v           , ISimdVVV           , (0b0010111000100000011011, kVO_V_BHS)                                                 , kRWI_W    , 0                         , 54 ), // #716
  INST(Uminp_v          , ISimdVVV           , (0b0010111000100000101011, kVO_V_BHS)                                                 , kRWI_W    , 0                         , 55 ), // #717
  INST(Uminv_v          , ISimdSV            , (0b0010111000110001101010, kVO_V_BH_4S)                                               , kRWI_W    , 0                         , 6  ), // #718
  INST(Umlal_v          , ISimdVVVe          , (0b0010111000100000100000, kVO_V_B8H4S2, 0b0010111100000000001000, kVO_V_H4S2)        , kRWI_X    , F(Long) | F(VH0_15)       , 19 ), // #719
  INST(Umlal2_v         , ISimdVVVe          , (0b0110111000100000100000, kVO_V_B16H8S4, 0b0010111100000000001000, kVO_V_H8S4)       , kRWI_X    , F(Long) | F(VH0_15)       , 20 ), // #720
  INST(Umlsl_v          , ISimdVVVe          , (0b0010111000100000101000, kVO_V_B8H4S2, 0b0010111100000000011000, kVO_V_H4S2)        , kRWI_X    , F(Long) | F(VH0_15)       , 21 ), // #721
  INST(Umlsl2_v         , ISimdVVVe          , (0b0110111000100000101000, kVO_V_B16H8S4, 0b0110111100000000011000, kVO_V_H8S4)       , kRWI_X    , F(Long) | F(VH0_15)       , 22 ), // #722
  INST(Ummla_v          , ISimdVVVx          , (0b0110111010000000101001, kOp_V4S, kOp_V16B, kOp_V16B)                               , kRWI_X    , 0                         , 15 ), // #723
  INST(Umov_v           , SimdSmovUmov       , (0b0000111000000000001111, kVO_V_Any, 0)                                              , kRWI_W    , 0                         , 1  ), // #724
  INST(Umull_v          , ISimdVVVe          , (0b0010111000100000110000, kVO_V_B8H4S2, 0b0010111100000000101000, kVO_V_H4S2)        , kRWI_W    , F(Long) | F(VH0_15)       , 23 ), // #725
  INST(Umull2_v         , ISimdVVVe          , (0b0110111000100000110000, kVO_V_B16H8S4, 0b0110111100000000101000, kVO_V_H8S4)       , kRWI_W    , F(Long) | F(VH0_15)       , 24 ), // #726
  INST(Uqadd_v          , ISimdVVV           , (0b0010111000100000000011, kVO_SV_Any)                                                , kRWI_W    , 0                         , 56 ), // #727
  INST(Uqrshl_v         , SimdShift          , (0b0010111000100000010111, 0b0000000000000000000000, 0, kVO_SV_Any)                   , kRWI_W    , 0                         , 26 ), // #728
  INST(Uqrshrn_v        , SimdShift          , (0b0000000000000000000000, 0b0010111100000000100111, 1, kVO_SV_B8H4S2)                , kRWI_W    , F(Narrow)                 , 27 ), // #729
  INST(Uqrshrn2_v       , SimdShift          , (0b0000000000000000000000, 0b0110111100000000100111, 1, kVO_V_B16H8S4)                , kRWI_X    , F(Narrow)                 , 28 ), // #730
  INST(Uqshl_v          , SimdShift          , (0b0010111000100000010011, 0b0010111100000000011101, 0, kVO_SV_Any)                   , kRWI_W    , 0                         , 29 ), // #731
  INST(Uqshrn_v         , SimdShift          , (0b0000000000000000000000, 0b0010111100000000100101, 1, kVO_SV_B8H4S2)                , kRWI_W    , F(Narrow)                 , 30 ), // #732
  INST(Uqshrn2_v        , SimdShift          , (0b0000000000000000000000, 0b0110111100000000100101, 1, kVO_V_B16H8S4)                , kRWI_X    , F(Narrow)                 , 31 ), // #733
  INST(Uqsub_v          , ISimdVVV           , (0b0010111000100000001011, kVO_SV_Any)                                                , kRWI_W    , 0                         , 57 ), // #734
  INST(Uqxtn_v          , ISimdVV            , (0b0010111000100001010010, kVO_SV_B8H4S2)                                             , kRWI_W    , F(Narrow)                 , 22 ), // #735
  INST(Uqxtn2_v         , ISimdVV            , (0b0110111000100001010010, kVO_V_B16H8S4)                                             , kRWI_X    , F(Narrow)                 , 23 ), // #736
  INST(Urecpe_v         , ISimdVV            , (0b0000111010100001110010, kVO_V_S)                                                   , kRWI_W    , 0                         , 24 ), // #737
  INST(Urhadd_v         , ISimdVVV           , (0b0010111000100000000101, kVO_V_BHS)                                                 , kRWI_W    , 0                         , 58 ), // #738
  INST(Urshl_v          , SimdShift          , (0b0010111000100000010101, 0b0000000000000000000000, 0, kVO_V_Any)                    , kRWI_W    , 0                         , 32 ), // #739
  INST(Urshr_v          , SimdShift          , (0b0000000000000000000000, 0b0010111100000000001001, 1, kVO_V_Any)                    , kRWI_W    , 0                         , 33 ), // #740
  INST(Ursqrte_v        , ISimdVV            , (0b0010111010100001110010, kVO_V_S)                                                   , kRWI_W    , 0                         , 25 ), // #741
  INST(Ursra_v          , SimdShift          , (0b0000000000000000000000, 0b0010111100000000001101, 1, kVO_V_Any)                    , kRWI_X    , 0                         , 34 ), // #742
  INST(Usdot_v          , SimdDot            , (0b0000111010000000100111, 0b0000111110000000111100, kET_S, kET_B, kET_4B)            , kRWI_X    , 0                         , 4  ), // #743
  INST(Ushl_v           , SimdShift          , (0b0010111000100000010001, 0b0000000000000000000000, 0, kVO_V_Any)                    , kRWI_W    , 0                         , 35 ), // #744
  INST(Ushll_v          , SimdShift          , (0b0000000000000000000000, 0b0010111100000000101001, 0, kVO_V_B8H4S2)                 , kRWI_W    , F(Long)                   , 36 ), // #745
  INST(Ushll2_v         , SimdShift          , (0b0000000000000000000000, 0b0110111100000000101001, 0, kVO_V_B16H8S4)                , kRWI_W    , F(Long)                   , 37 ), // #746
  INST(Ushr_v           , SimdShift          , (0b0000000000000000000000, 0b0010111100000000000001, 1, kVO_V_Any)                    , kRWI_W    , 0                         , 38 ), // #747
  INST(Usmmla_v         , ISimdVVVx          , (0b0100111010000000101011, kOp_V4S, kOp_V16B, kOp_V16B)                               , kRWI_X    , 0                         , 16 ), // #748
  INST(Usqadd_v         , ISimdVV            , (0b0010111000100000001110, kVO_SV_Any)                                                , kRWI_X    , 0                         , 26 ), // #749
  INST(Usra_v           , SimdShift          , (0b0000000000000000000000, 0b0010111100000000000101, 1, kVO_V_Any)                    , kRWI_X    , 0                         , 39 ), // #750
  INST(Usubl_v          , ISimdVVV           , (0b0010111000100000001000, kVO_V_B8H4S2)                                              , kRWI_W    , F(Long)                   , 59 ), // #751
  INST(Usubl2_v         , ISimdVVV           , (0b0110111000100000001000, kVO_V_B16H8S4)                                             , kRWI_W    , F(Long)                   , 60 ), // #752
  INST(Usubw_v          , ISimdWWV           , (0b0010111000100000001100, kVO_V_B8H4S2)                                              , kRWI_W    , 0                         , 6  ), // #753
  INST(Usubw2_v         , ISimdWWV           , (0b0010111000100000001100, kVO_V_B16H8S4)                                             , kRWI_W    , 0                         , 7  ), // #754
  INST(Uxtl_v           , SimdSxtlUxtl       , (0b0010111100000000101001, kVO_V_B8H4S2)                                              , kRWI_W    , F(Long)                   , 2  ), // #755
  INST(Uxtl2_v          , SimdSxtlUxtl       , (0b0110111100000000101001, kVO_V_B16H8S4)                                             , kRWI_W    , F(Long)                   , 3  ), // #756
  INST(Uzp1_v           , ISimdVVV           , (0b0000111000000000000110, kVO_V_BHS_D2)                                              , kRWI_W    , 0                         , 61 ), // #757
  INST(Uzp2_v           , ISimdVVV           , (0b0000111000000000010110, kVO_V_BHS_D2)                                              , kRWI_W    , 0                         , 62 ), // #758
  INST(Xar_v            , ISimdVVVI          , (0b1100111001100000100011, kVO_V_D2, 6, 10, 0)                                        , kRWI_W    , 0                         , 1  ), // #759
  INST(Xtn_v            , ISimdVV            , (0b0000111000100001001010, kVO_V_B8H4S2)                                              , kRWI_W    , F(Narrow)                 , 27 ), // #760
  INST(Xtn2_v           , ISimdVV            , (0b0100111000100001001010, kVO_V_B16H8S4)                                             , kRWI_X    , F(Narrow)                 , 28 ), // #761
  INST(Zip1_v           , ISimdVVV           , (0b0000111000000000001110, kVO_V_BHS_D2)                                              , kRWI_W    , 0                         , 63 ), // #762
  INST(Zip2_v           , ISimdVVV           , (0b0000111000000000011110, kVO_V_BHS_D2)                                              , kRWI_W    , 0                         , 64 )  // #763
  // ${InstInfo:End}
};

#undef F
#undef INST
#undef NAME_DATA_INDEX

namespace EncodingData {

// ${EncodingData:Begin}
// ------------------- Automatically generated, do not edit -------------------
const BaseAddSub baseAddSub[4] = {
  { 0b0001011000, 0b0001011001, 0b0010001 }, // add
  { 0b0101011000, 0b0101011001, 0b0110001 }, // adds
  { 0b1001011000, 0b1001011001, 0b1010001 }, // sub
  { 0b1101011000, 0b1101011001, 0b1110001 }  // subs
};

const BaseAdr baseAdr[2] = {
  { 0b0001000000000000000000, OffsetType::kAArch64_ADR }, // adr
  { 0b1001000000000000000000, OffsetType::kAArch64_ADRP }  // adrp
};

const BaseAtDcIcTlbi baseAtDcIcTlbi[4] = {
  { 0b00011111110000, 0b00001111000000, true }, // at
  { 0b00011110000000, 0b00001110000000, true }, // dc
  { 0b00011110000000, 0b00001110000000, false }, // ic
  { 0b00011110000000, 0b00010000000000, false }  // tlbi
};

const BaseAtomicCasp baseAtomicCasp[4] = {
  { 0b0000100000100000011111, kWX, 30 }, // casp
  { 0b0000100001100000011111, kWX, 30 }, // caspa
  { 0b0000100001100000111111, kWX, 30 }, // caspal
  { 0b0000100000100000111111, kWX, 30 }  // caspl
};

const BaseAtomicOp baseAtomicOp[123] = {
  { 0b1000100010100000011111, kWX, 30, 0 }, // cas
  { 0b1000100011100000011111, kWX, 30, 1 }, // casa
  { 0b0000100011100000011111, kW , 0 , 1 }, // casab
  { 0b0100100011100000011111, kW , 0 , 1 }, // casah
  { 0b1000100011100000111111, kWX, 30, 1 }, // casal
  { 0b0000100011100000111111, kW , 0 , 1 }, // casalb
  { 0b0100100011100000111111, kW , 0 , 1 }, // casalh
  { 0b0000100010100000011111, kW , 0 , 0 }, // casb
  { 0b0100100010100000011111, kW , 0 , 0 }, // cash
  { 0b1000100010100000111111, kWX, 30, 0 }, // casl
  { 0b0000100010100000111111, kW , 0 , 0 }, // caslb
  { 0b0100100010100000111111, kW , 0 , 0 }, // caslh
  { 0b1011100000100000000000, kWX, 30, 0 }, // ldadd
  { 0b1011100010100000000000, kWX, 30, 1 }, // ldadda
  { 0b0011100010100000000000, kW , 0 , 1 }, // ldaddab
  { 0b0111100010100000000000, kW , 0 , 1 }, // ldaddah
  { 0b1011100011100000000000, kWX, 30, 1 }, // ldaddal
  { 0b0011100011100000000000, kW , 0 , 1 }, // ldaddalb
  { 0b0111100011100000000000, kW , 0 , 1 }, // ldaddalh
  { 0b0011100000100000000000, kW , 0 , 0 }, // ldaddb
  { 0b0111100000100000000000, kW , 0 , 0 }, // ldaddh
  { 0b1011100001100000000000, kWX, 30, 0 }, // ldaddl
  { 0b0011100001100000000000, kW , 0 , 0 }, // ldaddlb
  { 0b0111100001100000000000, kW , 0 , 0 }, // ldaddlh
  { 0b1011100000100000000100, kWX, 30, 0 }, // ldclr
  { 0b1011100010100000000100, kWX, 30, 1 }, // ldclra
  { 0b0011100010100000000100, kW , 0 , 1 }, // ldclrab
  { 0b0111100010100000000100, kW , 0 , 1 }, // ldclrah
  { 0b1011100011100000000100, kWX, 30, 1 }, // ldclral
  { 0b0011100011100000000100, kW , 0 , 1 }, // ldclralb
  { 0b0111100011100000000100, kW , 0 , 1 }, // ldclralh
  { 0b0011100000100000000100, kW , 0 , 0 }, // ldclrb
  { 0b0111100000100000000100, kW , 0 , 0 }, // ldclrh
  { 0b1011100001100000000100, kWX, 30, 0 }, // ldclrl
  { 0b0011100001100000000100, kW , 0 , 0 }, // ldclrlb
  { 0b0111100001100000000100, kW , 0 , 0 }, // ldclrlh
  { 0b1011100000100000001000, kWX, 30, 0 }, // ldeor
  { 0b1011100010100000001000, kWX, 30, 1 }, // ldeora
  { 0b0011100010100000001000, kW , 0 , 1 }, // ldeorab
  { 0b0111100010100000001000, kW , 0 , 1 }, // ldeorah
  { 0b1011100011100000001000, kWX, 30, 1 }, // ldeoral
  { 0b0011100011100000001000, kW , 0 , 1 }, // ldeoralb
  { 0b0111100011100000001000, kW , 0 , 1 }, // ldeoralh
  { 0b0011100000100000001000, kW , 0 , 0 }, // ldeorb
  { 0b0111100000100000001000, kW , 0 , 0 }, // ldeorh
  { 0b1011100001100000001000, kWX, 30, 0 }, // ldeorl
  { 0b0011100001100000001000, kW , 0 , 0 }, // ldeorlb
  { 0b0111100001100000001000, kW , 0 , 0 }, // ldeorlh
  { 0b1011100000100000001100, kWX, 30, 0 }, // ldset
  { 0b1011100010100000001100, kWX, 30, 1 }, // ldseta
  { 0b0011100010100000001100, kW , 0 , 1 }, // ldsetab
  { 0b0111100010100000001100, kW , 0 , 1 }, // ldsetah
  { 0b1011100011100000001100, kWX, 30, 1 }, // ldsetal
  { 0b0011100011100000001100, kW , 0 , 1 }, // ldsetalb
  { 0b0111100011100000001100, kW , 0 , 1 }, // ldsetalh
  { 0b0011100000100000001100, kW , 0 , 0 }, // ldsetb
  { 0b0111100000100000001100, kW , 0 , 0 }, // ldseth
  { 0b1011100001100000001100, kWX, 30, 0 }, // ldsetl
  { 0b0011100001100000001100, kW , 0 , 0 }, // ldsetlb
  { 0b0111100001100000001100, kW , 0 , 0 }, // ldsetlh
  { 0b1011100000100000010000, kWX, 30, 0 }, // ldsmax
  { 0b1011100010100000010000, kWX, 30, 1 }, // ldsmaxa
  { 0b0011100010100000010000, kW , 0 , 1 }, // ldsmaxab
  { 0b0111100010100000010000, kW , 0 , 1 }, // ldsmaxah
  { 0b1011100011100000010000, kWX, 30, 1 }, // ldsmaxal
  { 0b0011100011100000010000, kW , 0 , 1 }, // ldsmaxalb
  { 0b0111100011100000010000, kW , 0 , 1 }, // ldsmaxalh
  { 0b0011100000100000010000, kW , 0 , 0 }, // ldsmaxb
  { 0b0111100000100000010000, kW , 0 , 0 }, // ldsmaxh
  { 0b1011100001100000010000, kWX, 30, 0 }, // ldsmaxl
  { 0b0011100001100000010000, kW , 0 , 0 }, // ldsmaxlb
  { 0b0111100001100000010000, kW , 0 , 0 }, // ldsmaxlh
  { 0b1011100000100000010100, kWX, 30, 0 }, // ldsmin
  { 0b1011100010100000010100, kWX, 30, 1 }, // ldsmina
  { 0b0011100010100000010100, kW , 0 , 1 }, // ldsminab
  { 0b0111100010100000010100, kW , 0 , 1 }, // ldsminah
  { 0b1011100011100000010100, kWX, 30, 1 }, // ldsminal
  { 0b0011100011100000010100, kW , 0 , 1 }, // ldsminalb
  { 0b0111100011100000010100, kW , 0 , 1 }, // ldsminalh
  { 0b0011100000100000010100, kW , 0 , 0 }, // ldsminb
  { 0b0111100000100000010100, kW , 0 , 0 }, // ldsminh
  { 0b1011100001100000010100, kWX, 30, 0 }, // ldsminl
  { 0b0011100001100000010100, kW , 0 , 0 }, // ldsminlb
  { 0b0111100001100000010100, kW , 0 , 0 }, // ldsminlh
  { 0b1011100000100000011000, kWX, 30, 0 }, // ldumax
  { 0b1011100010100000011000, kWX, 30, 1 }, // ldumaxa
  { 0b0011100010100000011000, kW , 0 , 1 }, // ldumaxab
  { 0b0111100010100000011000, kW , 0 , 1 }, // ldumaxah
  { 0b1011100011100000011000, kWX, 30, 1 }, // ldumaxal
  { 0b0011100011100000011000, kW , 0 , 1 }, // ldumaxalb
  { 0b0111100011100000011000, kW , 0 , 1 }, // ldumaxalh
  { 0b0011100000100000011000, kW , 0 , 0 }, // ldumaxb
  { 0b0111100000100000011000, kW , 0 , 0 }, // ldumaxh
  { 0b1011100001100000011000, kWX, 30, 0 }, // ldumaxl
  { 0b0011100001100000011000, kW , 0 , 0 }, // ldumaxlb
  { 0b0111100001100000011000, kW , 0 , 0 }, // ldumaxlh
  { 0b1011100000100000011100, kWX, 30, 0 }, // ldumin
  { 0b1011100010100000011100, kWX, 30, 1 }, // ldumina
  { 0b0011100010100000011100, kW , 0 , 1 }, // lduminab
  { 0b0111100010100000011100, kW , 0 , 1 }, // lduminah
  { 0b1011100011100000011100, kWX, 30, 1 }, // lduminal
  { 0b0011100011100000011100, kW , 0 , 1 }, // lduminalb
  { 0b0111100011100000011100, kW , 0 , 1 }, // lduminalh
  { 0b0011100000100000011100, kW , 0 , 0 }, // lduminb
  { 0b0111100000100000011100, kW , 0 , 0 }, // lduminh
  { 0b1011100001100000011100, kWX, 30, 0 }, // lduminl
  { 0b0011100001100000011100, kW , 0 , 0 }, // lduminlb
  { 0b0111100001100000011100, kW , 0 , 0 }, // lduminlh
  { 0b1000100000000000111111, kWX, 30, 1 }, // stlxr
  { 0b0000100000000000111111, kW , 0 , 1 }, // stlxrb
  { 0b0100100000000000111111, kW , 0 , 1 }, // stlxrh
  { 0b1011100000100000100000, kWX, 30, 1 }, // swp
  { 0b1011100010100000100000, kWX, 30, 1 }, // swpa
  { 0b0011100010100000100000, kW , 0 , 1 }, // swpab
  { 0b0111100010100000100000, kW , 0 , 1 }, // swpah
  { 0b1011100011100000100000, kWX, 30, 1 }, // swpal
  { 0b0011100011100000100000, kW , 0 , 1 }, // swpalb
  { 0b0111100011100000100000, kW , 0 , 1 }, // swpalh
  { 0b0011100000100000100000, kW , 0 , 1 }, // swpb
  { 0b0111100000100000100000, kW , 0 , 1 }, // swph
  { 0b1011100001100000100000, kWX, 30, 1 }, // swpl
  { 0b0011100001100000100000, kW , 0 , 1 }, // swplb
  { 0b0111100001100000100000, kW , 0 , 1 }  // swplh
};

const BaseAtomicSt baseAtomicSt[48] = {
  { 0b1011100000100000000000, kWX, 30 }, // stadd
  { 0b1011100001100000000000, kWX, 30 }, // staddl
  { 0b0011100000100000000000, kW , 0  }, // staddb
  { 0b0011100001100000000000, kW , 0  }, // staddlb
  { 0b0111100000100000000000, kW , 0  }, // staddh
  { 0b0111100001100000000000, kW , 0  }, // staddlh
  { 0b1011100000100000000100, kWX, 30 }, // stclr
  { 0b1011100001100000000100, kWX, 30 }, // stclrl
  { 0b0011100000100000000100, kW , 0  }, // stclrb
  { 0b0011100001100000000100, kW , 0  }, // stclrlb
  { 0b0111100000100000000100, kW , 0  }, // stclrh
  { 0b0111100001100000000100, kW , 0  }, // stclrlh
  { 0b1011100000100000001000, kWX, 30 }, // steor
  { 0b1011100001100000001000, kWX, 30 }, // steorl
  { 0b0011100000100000001000, kW , 0  }, // steorb
  { 0b0011100001100000001000, kW , 0  }, // steorlb
  { 0b0111100000100000001000, kW , 0  }, // steorh
  { 0b0111100001100000001000, kW , 0  }, // steorlh
  { 0b1011100000100000001100, kWX, 30 }, // stset
  { 0b1011100001100000001100, kWX, 30 }, // stsetl
  { 0b0011100000100000001100, kW , 0  }, // stsetb
  { 0b0011100001100000001100, kW , 0  }, // stsetlb
  { 0b0111100000100000001100, kW , 0  }, // stseth
  { 0b0111100001100000001100, kW , 0  }, // stsetlh
  { 0b1011100000100000010000, kWX, 30 }, // stsmax
  { 0b1011100001100000010000, kWX, 30 }, // stsmaxl
  { 0b0011100000100000010000, kW , 0  }, // stsmaxb
  { 0b0011100001100000010000, kW , 0  }, // stsmaxlb
  { 0b0111100000100000010000, kW , 0  }, // stsmaxh
  { 0b0111100001100000010000, kW , 0  }, // stsmaxlh
  { 0b1011100000100000010100, kWX, 30 }, // stsmin
  { 0b1011100001100000010100, kWX, 30 }, // stsminl
  { 0b0011100000100000010100, kW , 0  }, // stsminb
  { 0b0011100001100000010100, kW , 0  }, // stsminlb
  { 0b0111100000100000010100, kW , 0  }, // stsminh
  { 0b0111100001100000010100, kW , 0  }, // stsminlh
  { 0b1011100000100000011000, kWX, 30 }, // stumax
  { 0b1011100001100000011000, kWX, 30 }, // stumaxl
  { 0b0011100000100000011000, kW , 0  }, // stumaxb
  { 0b0011100001100000011000, kW , 0  }, // stumaxlb
  { 0b0111100000100000011000, kW , 0  }, // stumaxh
  { 0b0111100001100000011000, kW , 0  }, // stumaxlh
  { 0b1011100000100000011100, kWX, 30 }, // stumin
  { 0b1011100001100000011100, kWX, 30 }, // stuminl
  { 0b0011100000100000011100, kW , 0  }, // stuminb
  { 0b0011100001100000011100, kW , 0  }, // stuminlb
  { 0b0111100000100000011100, kW , 0  }, // stuminh
  { 0b0111100001100000011100, kW , 0  }  // stuminlh
};

const BaseBfc baseBfc[1] = {
  { 0b00110011000000000000001111100000 }  // bfc
};

const BaseBfi baseBfi[3] = {
  { 0b00110011000000000000000000000000 }, // bfi
  { 0b00010011000000000000000000000000 }, // sbfiz
  { 0b01010011000000000000000000000000 }  // ubfiz
};

const BaseBfm baseBfm[3] = {
  { 0b00110011000000000000000000000000 }, // bfm
  { 0b00010011000000000000000000000000 }, // sbfm
  { 0b01010011000000000000000000000000 }  // ubfm
};

const BaseBfx baseBfx[3] = {
  { 0b00110011000000000000000000000000 }, // bfxil
  { 0b00010011000000000000000000000000 }, // sbfx
  { 0b01010011000000000000000000000000 }  // ubfx
};

const BaseBranchCmp baseBranchCmp[2] = {
  { 0b00110101000000000000000000000000 }, // cbnz
  { 0b00110100000000000000000000000000 }  // cbz
};

const BaseBranchReg baseBranchReg[3] = {
  { 0b11010110001111110000000000000000 }, // blr
  { 0b11010110000111110000000000000000 }, // br
  { 0b11010110010111110000000000000000 }  // ret
};

const BaseBranchRel baseBranchRel[2] = {
  { 0b00010100000000000000000000000000 }, // b
  { 0b10010100000000000000000000000000 }  // bl
};

const BaseBranchTst baseBranchTst[2] = {
  { 0b00110111000000000000000000000000 }, // tbnz
  { 0b00110110000000000000000000000000 }  // tbz
};

const BaseCCmp baseCCmp[2] = {
  { 0b00111010010000000000000000000000 }, // ccmn
  { 0b01111010010000000000000000000000 }  // ccmp
};

const BaseCInc baseCInc[3] = {
  { 0b00011010100000000000010000000000 }, // cinc
  { 0b01011010100000000000000000000000 }, // cinv
  { 0b01011010100000000000010000000000 }  // cneg
};

const BaseCSel baseCSel[4] = {
  { 0b00011010100000000000000000000000 }, // csel
  { 0b00011010100000000000010000000000 }, // csinc
  { 0b01011010100000000000000000000000 }, // csinv
  { 0b01011010100000000000010000000000 }  // csneg
};

const BaseCSet baseCSet[2] = {
  { 0b00011010100111110000011111100000 }, // cset
  { 0b01011010100111110000001111100000 }  // csetm
};

const BaseCmpCmn baseCmpCmn[2] = {
  { 0b0101011000, 0b0101011001, 0b0110001 }, // cmn
  { 0b1101011000, 0b1101011001, 0b1110001 }  // cmp
};

const BaseExtend baseExtend[5] = {
  { 0b0001001100000000000111, kWX, 0 }, // sxtb
  { 0b0001001100000000001111, kWX, 0 }, // sxth
  { 0b1001001101000000011111, kX , 0 }, // sxtw
  { 0b0101001100000000000111, kW, 1 }, // uxtb
  { 0b0101001100000000001111, kW, 1 }  // uxth
};

const BaseExtract baseExtract[1] = {
  { 0b00010011100000000000000000000000 }  // extr
};

const BaseLdSt baseLdSt[9] = {
  { 0b1011100101, 0b10111000010, 0b10111000011, 0b00011000, kWX, 30, 2, Inst::kIdLdur }, // ldr
  { 0b0011100101, 0b00111000010, 0b00111000011, 0         , kW , 0 , 0, Inst::kIdLdurb }, // ldrb
  { 0b0111100101, 0b01111000010, 0b01111000011, 0         , kW , 0 , 1, Inst::kIdLdurh }, // ldrh
  { 0b0011100111, 0b00111000100, 0b00111000111, 0         , kWX, 22, 0, Inst::kIdLdursb }, // ldrsb
  { 0b0111100111, 0b01111000100, 0b01111000111, 0         , kWX, 22, 1, Inst::kIdLdursh }, // ldrsh
  { 0b1011100110, 0b10111000100, 0b10111000101, 0b10011000, kX , 0 , 2, Inst::kIdLdursw }, // ldrsw
  { 0b1011100100, 0b10111000000, 0b10111000001, 0         , kWX, 30, 2, Inst::kIdStur }, // str
  { 0b0011100100, 0b00111000000, 0b00111000001, 0         , kW , 30, 0, Inst::kIdSturb }, // strb
  { 0b0111100100, 0b01111000000, 0b01111000001, 0         , kWX, 30, 1, Inst::kIdSturh }  // strh
};

const BaseLdpStp baseLdpStp[6] = {
  { 0b0010100001, 0           , kWX, 31, 2 }, // ldnp
  { 0b0010100101, 0b0010100011, kWX, 31, 2 }, // ldp
  { 0b0110100101, 0b0110100011, kX , 0 , 2 }, // ldpsw
  { 0b0110100100, 0b0110100010, kX, 0, 4 }, // stgp
  { 0b0010100000, 0           , kWX, 31, 2 }, // stnp
  { 0b0010100100, 0b0010100010, kWX, 31, 2 }  // stp
};

const BaseLdxp baseLdxp[2] = {
  { 0b1000100001111111100000, kWX, 30 }, // ldaxp
  { 0b1000100001111111000000, kWX, 30 }  // ldxp
};

const BaseLogical baseLogical[8] = {
  { 0b0001010000, 0b00100100, 0 }, // and
  { 0b1101010000, 0b11100100, 0 }, // ands
  { 0b0001010001, 0b00100100, 1 }, // bic
  { 0b1101010001, 0b11100100, 1 }, // bics
  { 0b1001010001, 0b10100100, 1 }, // eon
  { 0b1001010000, 0b10100100, 0 }, // eor
  { 0b0101010001, 0b01100100, 1 }, // orn
  { 0b0101010000, 0b01100100, 0 }  // orr
};

const BaseMovKNZ baseMovKNZ[3] = {
  { 0b01110010100000000000000000000000 }, // movk
  { 0b00010010100000000000000000000000 }, // movn
  { 0b01010010100000000000000000000000 }  // movz
};

const BaseMvnNeg baseMvnNeg[3] = {
  { 0b00101010001000000000001111100000 }, // mvn
  { 0b01001011000000000000001111100000 }, // neg
  { 0b01101011000000000000001111100000 }  // negs
};

const BaseOp baseOp[23] = {
  { 0b11010101000000110010000110011111 }, // autia1716
  { 0b11010101000000110010001110111111 }, // autiasp
  { 0b11010101000000110010001110011111 }, // autiaz
  { 0b11010101000000110010000111011111 }, // autib1716
  { 0b11010101000000110010001111111111 }, // autibsp
  { 0b11010101000000110010001111011111 }, // autibz
  { 0b11010101000000000100000001011111 }, // axflag
  { 0b11010101000000000100000000011111 }, // cfinv
  { 0b11010101000000110010001010011111 }, // csdb
  { 0b11010101000000110010000011011111 }, // dgh
  { 0b11010110101111110000001111100000 }, // drps
  { 0b11010101000000110010001000011111 }, // esb
  { 0b11010110100111110000001111100000 }, // eret
  { 0b11010101000000110010000000011111 }, // nop
  { 0b11010101000000110011010010011111 }, // pssbb
  { 0b11010101000000110010000010011111 }, // sev
  { 0b11010101000000110010000010111111 }, // sevl
  { 0b11010101000000110011000010011111 }, // ssbb
  { 0b11010101000000110010000001011111 }, // wfe
  { 0b11010101000000110010000001111111 }, // wfi
  { 0b11010101000000000100000000111111 }, // xaflag
  { 0b11010101000000110010000011111111 }, // xpaclri
  { 0b11010101000000110010000000111111 }  // yield
};

const BaseOpImm baseOpImm[14] = {
  { 0b11010100001000000000000000000000, 16, 5 }, // brk
  { 0b11010101000000110011000001011111, 4, 8 }, // clrex
  { 0b11010100101000000000000000000001, 16, 5 }, // dcps1
  { 0b11010100101000000000000000000010, 16, 5 }, // dcps2
  { 0b11010100101000000000000000000011, 16, 5 }, // dcps3
  { 0b11010101000000110011000010111111, 4, 8 }, // dmb
  { 0b11010101000000110011000010011111, 4, 8 }, // dsb
  { 0b11010101000000110010000000011111, 7, 5 }, // hint
  { 0b11010100010000000000000000000000, 16, 5 }, // hlt
  { 0b11010100000000000000000000000010, 16, 5 }, // hvc
  { 0b11010101000000110011000011011111, 4, 8 }, // isb
  { 0b11010100000000000000000000000011, 16, 5 }, // smc
  { 0b11010100000000000000000000000001, 16, 5 }, // svc
  { 0b00000000000000000000000000000000, 16, 0 }  // udf
};

const BaseR baseR[10] = {
  { 0b11011010110000010011101111100000, kX, kZR, 0 }, // autdza
  { 0b11011010110000010011111111100000, kX, kZR, 0 }, // autdzb
  { 0b11011010110000010011001111100000, kX, kZR, 0 }, // autiza
  { 0b11011010110000010011011111100000, kX, kZR, 0 }, // autizb
  { 0b11011010110000010010101111100000, kX, kZR, 0 }, // pacdza
  { 0b11011010110000010010111111100000, kX, kZR, 0 }, // pacdzb
  { 0b00111010000000000000100000001101, kW, kZR, 5 }, // setf8
  { 0b00111010000000000100100000001101, kW, kZR, 5 }, // setf16
  { 0b11011010110000010100011111100000, kX, kZR, 0 }, // xpacd
  { 0b11011010110000010100001111100000, kX, kZR, 0 }  // xpaci
};

const BaseRM_NoImm baseRM_NoImm[21] = {
  { 0b1000100011011111111111, kWX, kZR, 30 }, // ldar
  { 0b0000100011011111111111, kW , kZR, 0  }, // ldarb
  { 0b0100100011011111111111, kW , kZR, 0  }, // ldarh
  { 0b1000100001011111111111, kWX, kZR, 30 }, // ldaxr
  { 0b0000100001011111111111, kW , kZR, 0  }, // ldaxrb
  { 0b0100100001011111111111, kW , kZR, 0  }, // ldaxrh
  { 0b1101100111100000000000, kX , kZR, 0  }, // ldgm
  { 0b1000100011011111011111, kWX, kZR, 30 }, // ldlar
  { 0b0000100011011111011111, kW , kZR, 0  }, // ldlarb
  { 0b0100100011011111011111, kW , kZR, 0  }, // ldlarh
  { 0b1000100001011111011111, kWX, kZR, 30 }, // ldxr
  { 0b0000100001011111011111, kW , kZR, 0  }, // ldxrb
  { 0b0100100001011111011111, kW , kZR, 0  }, // ldxrh
  { 0b1101100110100000000000, kX , kZR, 0  }, // stgm
  { 0b1000100010011111011111, kWX, kZR, 30 }, // stllr
  { 0b0000100010011111011111, kW , kZR, 0  }, // stllrb
  { 0b0100100010011111011111, kW , kZR, 0  }, // stllrh
  { 0b1000100010011111111111, kWX, kZR, 30 }, // stlr
  { 0b0000100010011111111111, kW , kZR, 0  }, // stlrb
  { 0b0100100010011111111111, kW , kZR, 0  }, // stlrh
  { 0b1101100100100000000000, kX , kZR, 0 }  // stzgm
};

const BaseRM_SImm10 baseRM_SImm10[2] = {
  { 0b1111100000100000000001, kX , kZR, 0, 3 }, // ldraa
  { 0b1111100010100000000001, kX , kZR, 0, 3 }  // ldrab
};

const BaseRM_SImm9 baseRM_SImm9[23] = {
  { 0b1101100101100000000000, 0b0000000000000000000000, kX , kZR, 0, 4 }, // ldg
  { 0b1011100001000000000010, 0b0000000000000000000000, kWX, kZR, 30, 0 }, // ldtr
  { 0b0011100001000000000010, 0b0000000000000000000000, kW , kZR, 0 , 0 }, // ldtrb
  { 0b0111100001000000000010, 0b0000000000000000000000, kW , kZR, 0 , 0 }, // ldtrh
  { 0b0011100011000000000010, 0b0000000000000000000000, kWX, kZR, 22, 0 }, // ldtrsb
  { 0b0111100011000000000010, 0b0000000000000000000000, kWX, kZR, 22, 0 }, // ldtrsh
  { 0b1011100010000000000010, 0b0000000000000000000000, kX , kZR, 0 , 0 }, // ldtrsw
  { 0b1011100001000000000000, 0b0000000000000000000000, kWX, kZR, 30, 0 }, // ldur
  { 0b0011100001000000000000, 0b0000000000000000000000, kW , kZR, 0 , 0 }, // ldurb
  { 0b0111100001000000000000, 0b0000000000000000000000, kW , kZR, 0 , 0 }, // ldurh
  { 0b0011100011000000000000, 0b0000000000000000000000, kWX, kZR, 22, 0 }, // ldursb
  { 0b0111100011000000000000, 0b0000000000000000000000, kWX, kZR, 22, 0 }, // ldursh
  { 0b1011100010000000000000, 0b0000000000000000000000, kX,  kZR, 0 , 0 }, // ldursw
  { 0b1101100110100000000010, 0b1101100110100000000001, kX, kSP, 0, 4 }, // st2g
  { 0b1101100100100000000010, 0b1101100100100000000001, kX, kSP, 0, 4 }, // stg
  { 0b1011100000000000000010, 0b0000000000000000000000, kWX, kZR, 30, 0 }, // sttr
  { 0b0011100000000000000010, 0b0000000000000000000000, kW , kZR, 0 , 0 }, // sttrb
  { 0b0111100000000000000010, 0b0000000000000000000000, kW , kZR, 0 , 0 }, // sttrh
  { 0b1011100000000000000000, 0b0000000000000000000000, kWX, kZR, 30, 0 }, // stur
  { 0b0011100000000000000000, 0b0000000000000000000000, kW , kZR, 0 , 0 }, // sturb
  { 0b0111100000000000000000, 0b0000000000000000000000, kW , kZR, 0 , 0 }, // sturh
  { 0b1101100111100000000010, 0b1101100111100000000001, kX , kSP, 0, 4 }, // stz2g
  { 0b1101100101100000000010, 0b1101100101100000000001, kX , kSP, 0, 4 }  // stzg
};

const BaseRR baseRR[15] = {
  { 0b11011010110000010001100000000000, kX, kZR, 0, kX, kSP, 5, true }, // autda
  { 0b11011010110000010001110000000000, kX, kZR, 0, kX, kSP, 5, true }, // autdb
  { 0b11011010110000010001000000000000, kX, kZR, 0, kX, kSP, 5, true }, // autia
  { 0b11011010110000010001010000000000, kX, kZR, 0, kX, kSP, 5, true }, // autib
  { 0b01011010110000000001010000000000, kWX, kZR, 0, kWX, kZR, 5, true }, // cls
  { 0b01011010110000000001000000000000, kWX, kZR, 0, kWX, kZR, 5, true }, // clz
  { 0b10111010110000000000000000011111, kX, kSP, 5, kX, kSP, 16, true }, // cmpp
  { 0b01011010000000000000001111100000, kWX, kZR, 0, kWX, kZR, 16, true }, // ngc
  { 0b01111010000000000000001111100000, kWX, kZR, 0, kWX, kZR, 16, true }, // ngcs
  { 0b11011010110000010000100000000000, kX, kZR, 0, kX, kSP, 5, true }, // pacda
  { 0b11011010110000010000110000000000, kX, kZR, 0, kX, kSP, 5, true }, // pacdb
  { 0b01011010110000000000000000000000, kWX, kZR, 0, kWX, kZR, 5, true }, // rbit
  { 0b01011010110000000000010000000000, kWX, kZR, 0, kWX, kZR, 5, true }, // rev16
  { 0b11011010110000000000100000000000, kWX, kZR, 0, kWX, kZR, 5, true }, // rev32
  { 0b11011010110000000000110000000000, kWX, kZR, 0, kWX, kZR, 5, true }  // rev64
};

const BaseRRII baseRRII[2] = {
  { 0b1001000110000000000000, kX, kSP, kX, kSP, 6, 4, 16, 4, 0, 10 }, // addg
  { 0b1101000110000000000000, kX, kSP, kX, kSP, 6, 4, 16, 4, 0, 10 }  // subg
};

const BaseRRR baseRRR[26] = {
  { 0b0001101000000000000000, kWX, kZR, kWX, kZR, kWX, kZR, true }, // adc
  { 0b0011101000000000000000, kWX, kZR, kWX, kZR, kWX, kZR, true }, // adcs
  { 0b0001101011000000010000, kW, kZR, kW, kZR, kW, kZR, false }, // crc32b
  { 0b0001101011000000010100, kW, kZR, kW, kZR, kW, kZR, false }, // crc32cb
  { 0b0001101011000000010101, kW, kZR, kW, kZR, kW, kZR, false }, // crc32ch
  { 0b0001101011000000010110, kW, kZR, kW, kZR, kW, kZR, false }, // crc32cw
  { 0b1001101011000000010111, kW, kZR, kW, kZR, kX, kZR, false }, // crc32cx
  { 0b0001101011000000010001, kW, kZR, kW, kZR, kW, kZR, false }, // crc32h
  { 0b0001101011000000010010, kW, kZR, kW, kZR, kW, kZR, false }, // crc32w
  { 0b1001101011000000010011, kW, kZR, kW, kZR, kX, kZR, false }, // crc32x
  { 0b1001101011000000000101, kX , kZR, kX , kSP, kX , kZR, true }, // gmi
  { 0b0001101100000000111111, kWX, kZR, kWX, kZR, kWX, kZR, true }, // mneg
  { 0b0001101100000000011111, kWX, kZR, kWX, kZR, kWX, kZR, true }, // mul
  { 0b1001101011000000001100, kX, kZR, kX, kZR, kX, kSP, false }, // pacga
  { 0b0101101000000000000000, kWX, kZR, kWX, kZR, kWX, kZR, true }, // sbc
  { 0b0111101000000000000000, kWX, kZR, kWX, kZR, kWX, kZR, true }, // sbcs
  { 0b0001101011000000000011, kWX, kZR, kWX, kZR, kWX, kZR, true }, // sdiv
  { 0b1001101100100000111111, kX , kZR, kW , kZR, kW , kZR, false }, // smnegl
  { 0b1001101101000000011111, kX , kZR, kX , kZR, kX , kZR, true }, // smulh
  { 0b1001101100100000011111, kX , kZR, kW , kZR, kW , kZR, false }, // smull
  { 0b1001101011000000000000, kX, kZR, kX, kSP, kX, kSP, false }, // subp
  { 0b1011101011000000000000, kX, kZR, kX, kSP, kX, kSP, false }, // subps
  { 0b0001101011000000000010, kWX, kZR, kWX, kZR, kWX, kZR, true }, // udiv
  { 0b1001101110100000111111, kX , kZR, kW , kZR, kW , kZR, false }, // umnegl
  { 0b1001101110100000011111, kX , kZR, kW , kZR, kW , kZR, false }, // umull
  { 0b1001101111000000011111, kX , kZR, kX , kZR, kX , kZR, false }  // umulh
};

const BaseRRRR baseRRRR[6] = {
  { 0b0001101100000000000000, kWX, kZR, kWX, kZR, kWX, kZR, kWX, kZR, true }, // madd
  { 0b0001101100000000100000, kWX, kZR, kWX, kZR, kWX, kZR, kWX, kZR, true }, // msub
  { 0b1001101100100000000000, kX , kZR, kW , kZR, kW , kZR, kX , kZR, false }, // smaddl
  { 0b1001101100100000100000, kX , kZR, kW , kZR, kW , kZR, kX , kZR, false }, // smsubl
  { 0b1001101110100000000000, kX , kZR, kW , kZR, kW , kZR, kX , kZR, false }, // umaddl
  { 0b1001101110100000100000, kX , kZR, kW , kZR, kW , kZR, kX , kZR, false }  // umsubl
};

const BaseShift baseShift[8] = {
  { 0b0001101011000000001010, 0b0001001100000000011111, 0 }, // asr
  { 0b0001101011000000001010, 0b0000000000000000000000, 0 }, // asrv
  { 0b0001101011000000001000, 0b0101001100000000000000, 0 }, // lsl
  { 0b0001101011000000001000, 0b0000000000000000000000, 0 }, // lslv
  { 0b0001101011000000001001, 0b0101001100000000011111, 0 }, // lsr
  { 0b0001101011000000001001, 0b0000000000000000000000, 0 }, // lsrv
  { 0b0001101011000000001011, 0b0001001110000000000000, 1 }, // ror
  { 0b0001101011000000001011, 0b0000000000000000000000, 1 }  // rorv
};

const BaseStx baseStx[3] = {
  { 0b1000100000000000011111, kWX, 30 }, // stxr
  { 0b0000100000000000011111, kW , 0  }, // stxrb
  { 0b0100100000000000011111, kW , 0  }  // stxrh
};

const BaseStxp baseStxp[2] = {
  { 0b1000100000100000100000, kWX, 30 }, // stlxp
  { 0b1000100000100000000000, kWX, 30 }  // stxp
};

const BaseTst baseTst[1] = {
  { 0b1101010000, 0b111001000 }  // tst
};

const FSimdPair fSimdPair[5] = {
  { 0b0111111000110000110110, 0b0010111000100000110101 }, // faddp_v
  { 0b0111111000110000110010, 0b0010111000100000110001 }, // fmaxnmp_v
  { 0b0111111000110000111110, 0b0010111000100000111101 }, // fmaxp_v
  { 0b0111111010110000110010, 0b0010111010100000110001 }, // fminnmp_v
  { 0b0111111010110000111110, 0b0010111010100000111101 }  // fminp_v
};

const FSimdSV fSimdSV[4] = {
  { 0b0010111000110000110010 }, // fmaxnmv_v
  { 0b0010111000110000111110 }, // fmaxv_v
  { 0b0010111010110000110010 }, // fminnmv_v
  { 0b0010111010110000111110 }  // fminv_v
};

const FSimdVV fSimdVV[17] = {
  { 0b0001111000100000110000, kHF_A, 0b0000111010100000111110, kHF_B }, // fabs_v
  { 0b0001111000100001010000, kHF_A, 0b0010111010100000111110, kHF_B }, // fneg_v
  { 0b0101111010100001110110, kHF_B, 0b0000111010100001110110, kHF_B }, // frecpe_v
  { 0b0101111010100001111110, kHF_B, 0b0000000000000000000000, kHF_N }, // frecpx_v
  { 0b0001111000101000110000, kHF_N, 0b0010111000100001111010, kHF_N }, // frint32x_v
  { 0b0001111000101000010000, kHF_N, 0b0000111000100001111010, kHF_N }, // frint32z_v
  { 0b0001111000101001110000, kHF_N, 0b0010111000100001111110, kHF_N }, // frint64x_v
  { 0b0001111000101001010000, kHF_N, 0b0000111000100001111110, kHF_N }, // frint64z_v
  { 0b0001111000100110010000, kHF_A, 0b0010111000100001100010, kHF_B }, // frinta_v
  { 0b0001111000100111110000, kHF_A, 0b0010111010100001100110, kHF_B }, // frinti_v
  { 0b0001111000100101010000, kHF_A, 0b0000111000100001100110, kHF_B }, // frintm_v
  { 0b0001111000100100010000, kHF_A, 0b0000111000100001100010, kHF_B }, // frintn_v
  { 0b0001111000100100110000, kHF_A, 0b0000111010100001100010, kHF_B }, // frintp_v
  { 0b0001111000100111010000, kHF_A, 0b0010111000100001100110, kHF_B }, // frintx_v
  { 0b0001111000100101110000, kHF_A, 0b0000111010100001100110, kHF_B }, // frintz_v
  { 0b0111111010100001110110, kHF_B, 0b0010111010100001110110, kHF_B }, // frsqrte_v
  { 0b0001111000100001110000, kHF_A, 0b0010111010100001111110, kHF_B }  // fsqrt_v
};

const FSimdVVV fSimdVVV[13] = {
  { 0b0111111010100000110101, kHF_C, 0b0010111010100000110101, kHF_C }, // fabd_v
  { 0b0111111000100000111011, kHF_C, 0b0010111000100000111011, kHF_C }, // facge_v
  { 0b0111111010100000111011, kHF_C, 0b0010111010100000111011, kHF_C }, // facgt_v
  { 0b0001111000100000001010, kHF_A, 0b0000111000100000110101, kHF_C }, // fadd_v
  { 0b0001111000100000000110, kHF_A, 0b0010111000100000111111, kHF_C }, // fdiv_v
  { 0b0001111000100000010010, kHF_A, 0b0000111000100000111101, kHF_C }, // fmax_v
  { 0b0001111000100000011010, kHF_A, 0b0000111000100000110001, kHF_C }, // fmaxnm_v
  { 0b0001111000100000010110, kHF_A, 0b0000111010100000111101, kHF_C }, // fmin_v
  { 0b0001111000100000011110, kHF_A, 0b0000111010100000110001, kHF_C }, // fminnm_v
  { 0b0001111000100000100010, kHF_A, 0b0000000000000000000000, kHF_N }, // fnmul_v
  { 0b0101111000100000111111, kHF_C, 0b0000111000100000111111, kHF_C }, // frecps_v
  { 0b0101111010100000111111, kHF_C, 0b0000111010100000111111, kHF_C }, // frsqrts_v
  { 0b0001111000100000001110, kHF_A, 0b0000111010100000110101, kHF_C }  // fsub_v
};

const FSimdVVVV fSimdVVVV[4] = {
  { 0b0001111100000000000000, kHF_A, 0b0000000000000000000000, kHF_N }, // fmadd_v
  { 0b0001111100000000100000, kHF_A, 0b0000000000000000000000, kHF_N }, // fmsub_v
  { 0b0001111100100000000000, kHF_A, 0b0000000000000000000000, kHF_N }, // fnmadd_v
  { 0b0001111100100000100000, kHF_A, 0b0000000000000000000000, kHF_N }  // fnmsub_v
};

const FSimdVVVe fSimdVVVe[4] = {
  { 0b0000000000000000000000, kHF_N, 0b0000111000100000110011, 0b0000111110000000000100 }, // fmla_v
  { 0b0000000000000000000000, kHF_N, 0b0000111010100000110011, 0b0000111110000000010100 }, // fmls_v
  { 0b0001111000100000000010, kHF_A, 0b0010111000100000110111, 0b0000111110000000100100 }, // fmul_v
  { 0b0101111000100000110111, kHF_C, 0b0000111000100000110111, 0b0010111110000000100100 }  // fmulx_v
};

const ISimdPair iSimdPair[1] = {
  { 0b0101111000110001101110, 0b0000111000100000101111, kVO_V_Any }  // addp_v
};

const ISimdSV iSimdSV[7] = {
  { 0b0000111000110001101110, kVO_V_BH_4S }, // addv_v
  { 0b0000111000110000001110, kVO_V_BH_4S }, // saddlv_v
  { 0b0000111000110000101010, kVO_V_BH_4S }, // smaxv_v
  { 0b0000111000110001101010, kVO_V_BH_4S }, // sminv_v
  { 0b0010111000110000001110, kVO_V_BH_4S }, // uaddlv_v
  { 0b0010111000110000101010, kVO_V_BH_4S }, // umaxv_v
  { 0b0010111000110001101010, kVO_V_BH_4S }  // uminv_v
};

const ISimdVV iSimdVV[29] = {
  { 0b0000111000100000101110, kVO_V_Any }, // abs_v
  { 0b0000111000100000010010, kVO_V_BHS }, // cls_v
  { 0b0010111000100000010010, kVO_V_BHS }, // clz_v
  { 0b0000111000100000010110, kVO_V_B }, // cnt_v
  { 0b0010111000100000010110, kVO_V_B }, // mvn_v
  { 0b0010111000100000101110, kVO_V_Any }, // neg_v
  { 0b0010111000100000010110, kVO_V_B }, // not_v
  { 0b0010111001100000010110, kVO_V_B }, // rbit_v
  { 0b0000111000100000000110, kVO_V_B }, // rev16_v
  { 0b0010111000100000000010, kVO_V_BH }, // rev32_v
  { 0b0000111000100000000010, kVO_V_BHS }, // rev64_v
  { 0b0000111000100000011010, kVO_V_BHS }, // sadalp_v
  { 0b0000111000100000001010, kVO_V_BHS }, // saddlp_v
  { 0b0000111000100000011110, kVO_SV_Any }, // sqabs_v
  { 0b0010111000100000011110, kVO_SV_Any }, // sqneg_v
  { 0b0000111000100001010010, kVO_SV_B8H4S2 }, // sqxtn_v
  { 0b0100111000100001010010, kVO_V_B16H8S4 }, // sqxtn2_v
  { 0b0010111000100001001010, kVO_SV_B8H4S2 }, // sqxtun_v
  { 0b0110111000100001001010, kVO_V_B16H8S4 }, // sqxtun2_v
  { 0b0000111000100000001110, kVO_SV_Any }, // suqadd_v
  { 0b0010111000100000011010, kVO_V_BHS }, // uadalp_v
  { 0b0010111000100000001010, kVO_V_BHS }, // uaddlp_v
  { 0b0010111000100001010010, kVO_SV_B8H4S2 }, // uqxtn_v
  { 0b0110111000100001010010, kVO_V_B16H8S4 }, // uqxtn2_v
  { 0b0000111010100001110010, kVO_V_S }, // urecpe_v
  { 0b0010111010100001110010, kVO_V_S }, // ursqrte_v
  { 0b0010111000100000001110, kVO_SV_Any }, // usqadd_v
  { 0b0000111000100001001010, kVO_V_B8H4S2 }, // xtn_v
  { 0b0100111000100001001010, kVO_V_B16H8S4 }  // xtn2_v
};

const ISimdVVV iSimdVVV[65] = {
  { 0b0000111000100000100001, kVO_V_Any }, // add_v
  { 0b0000111000100000010000, kVO_V_B8H4S2 }, // addhn_v
  { 0b0100111000100000010000, kVO_V_B16H8S4 }, // addhn2_v
  { 0b0000111000100000000111, kVO_V_B }, // and_v
  { 0b0010111011100000000111, kVO_V_B }, // bif_v
  { 0b0010111010100000000111, kVO_V_B }, // bit_v
  { 0b0010111001100000000111, kVO_V_B }, // bsl_v
  { 0b0000111000100000100011, kVO_V_Any }, // cmtst_v
  { 0b0010111000100000000111, kVO_V_B }, // eor_v
  { 0b0000111011100000000111, kVO_V_B }, // orn_v
  { 0b0010111000100000100111, kVO_V_B }, // pmul_v
  { 0b0000111000100000111000, kVO_V_B8D1 }, // pmull_v
  { 0b0100111000100000111000, kVO_V_B16D2 }, // pmull2_v
  { 0b0010111000100000010000, kVO_V_B8H4S2 }, // raddhn_v
  { 0b0110111000100000010000, kVO_V_B16H8S4 }, // raddhn2_v
  { 0b1100111001100000100011, kVO_V_D2 }, // rax1_v
  { 0b0010111000100000011000, kVO_V_B8H4S2 }, // rsubhn_v
  { 0b0110111000100000011000, kVO_V_B16H8S4 }, // rsubhn2_v
  { 0b0000111000100000011111, kVO_V_BHS }, // saba_v
  { 0b0000111000100000010100, kVO_V_B8H4S2 }, // sabal_v
  { 0b0100111000100000010100, kVO_V_B16H8S4 }, // sabal2_v
  { 0b0000111000100000011101, kVO_V_BHS }, // sabd_v
  { 0b0000111000100000011100, kVO_V_B8H4S2 }, // sabdl_v
  { 0b0100111000100000011100, kVO_V_B16H8S4 }, // sabdl2_v
  { 0b0000111000100000000000, kVO_V_B8H4S2 }, // saddl_v
  { 0b0100111000100000000000, kVO_V_B16H8S4 }, // saddl2_v
  { 0b0000111000100000000001, kVO_V_BHS }, // shadd_v
  { 0b0000111000100000001001, kVO_V_BHS }, // shsub_v
  { 0b0000111000100000011001, kVO_V_BHS }, // smax_v
  { 0b0000111000100000101001, kVO_V_BHS }, // smaxp_v
  { 0b0000111000100000011011, kVO_V_BHS }, // smin_v
  { 0b0000111000100000101011, kVO_V_BHS }, // sminp_v
  { 0b0000111000100000000011, kVO_SV_Any }, // sqadd_v
  { 0b0000111000100000001011, kVO_SV_Any }, // sqsub_v
  { 0b0000111000100000000101, kVO_V_BHS }, // srhadd_v
  { 0b0000111000100000001000, kVO_V_B8H4S2 }, // ssubl_v
  { 0b0100111000100000001000, kVO_V_B16H8S4 }, // ssubl2_v
  { 0b0010111000100000100001, kVO_V_Any }, // sub_v
  { 0b0000111000100000011000, kVO_V_B8H4S2 }, // subhn_v
  { 0b0000111000100000011000, kVO_V_B16H8S4 }, // subhn2_v
  { 0b0000111000000000001010, kVO_V_BHS_D2 }, // trn1_v
  { 0b0000111000000000011010, kVO_V_BHS_D2 }, // trn2_v
  { 0b0010111000100000011111, kVO_V_BHS }, // uaba_v
  { 0b0010111000100000010100, kVO_V_B8H4S2 }, // uabal_v
  { 0b0110111000100000010100, kVO_V_B16H8S4 }, // uabal2_v
  { 0b0010111000100000011101, kVO_V_BHS }, // uabd_v
  { 0b0010111000100000011100, kVO_V_B8H4S2 }, // uabdl_v
  { 0b0110111000100000011100, kVO_V_B16H8S4 }, // uabdl2_v
  { 0b0010111000100000000000, kVO_V_B8H4S2 }, // uaddl_v
  { 0b0110111000100000000000, kVO_V_B16H8S4 }, // uaddl2_v
  { 0b0010111000100000000001, kVO_V_BHS }, // uhadd_v
  { 0b0010111000100000001001, kVO_V_BHS }, // uhsub_v
  { 0b0010111000100000011001, kVO_V_BHS }, // umax_v
  { 0b0010111000100000101001, kVO_V_BHS }, // umaxp_v
  { 0b0010111000100000011011, kVO_V_BHS }, // umin_v
  { 0b0010111000100000101011, kVO_V_BHS }, // uminp_v
  { 0b0010111000100000000011, kVO_SV_Any }, // uqadd_v
  { 0b0010111000100000001011, kVO_SV_Any }, // uqsub_v
  { 0b0010111000100000000101, kVO_V_BHS }, // urhadd_v
  { 0b0010111000100000001000, kVO_V_B8H4S2 }, // usubl_v
  { 0b0110111000100000001000, kVO_V_B16H8S4 }, // usubl2_v
  { 0b0000111000000000000110, kVO_V_BHS_D2 }, // uzp1_v
  { 0b0000111000000000010110, kVO_V_BHS_D2 }, // uzp2_v
  { 0b0000111000000000001110, kVO_V_BHS_D2 }, // zip1_v
  { 0b0000111000000000011110, kVO_V_BHS_D2 }  // zip2_v
};

const ISimdVVVI iSimdVVVI[2] = {
  { 0b0010111000000000000000, kVO_V_B, 4, 11, 1 }, // ext_v
  { 0b1100111001100000100011, kVO_V_D2, 6, 10, 0 }  // xar_v
};

const ISimdVVVV iSimdVVVV[2] = {
  { 0b1100111000100000000000, kVO_V_B16 }, // bcax_v
  { 0b1100111000000000000000, kVO_V_B16 }  // eor3_v
};

const ISimdVVVVx iSimdVVVVx[1] = {
  { 0b1100111001000000000000, kOp_V4S, kOp_V4S, kOp_V4S, kOp_V4S }  // sm3ss1_v
};

const ISimdVVVe iSimdVVVe[25] = {
  { 0b0000111000100000100101, kVO_V_BHS, 0b0010111100000000000000, kVO_V_HS }, // mla_v
  { 0b0010111000100000100101, kVO_V_BHS, 0b0010111100000000010000, kVO_V_HS }, // mls_v
  { 0b0000111000100000100111, kVO_V_BHS, 0b0000111100000000100000, kVO_V_HS }, // mul_v
  { 0b0000111000100000100000, kVO_V_B8H4S2, 0b0000111100000000001000, kVO_V_H4S2 }, // smlal_v
  { 0b0100111000100000100000, kVO_V_B16H8S4, 0b0100111100000000001000, kVO_V_H8S4 }, // smlal2_v
  { 0b0000111000100000101000, kVO_V_B8H4S2, 0b0000111100000000011000, kVO_V_H4S2 }, // smlsl_v
  { 0b0100111000100000101000, kVO_V_B16H8S4, 0b0100111100000000011000, kVO_V_H8S4 }, // smlsl2_v
  { 0b0000111000100000110000, kVO_V_B8H4S2, 0b0000111100000000101000, kVO_V_H4S2 }, // smull_v
  { 0b0100111000100000110000, kVO_V_B16H8S4, 0b0100111100000000101000, kVO_V_H8S4 }, // smull2_v
  { 0b0000111000100000100100, kVO_SV_BHS, 0b0000111100000000001100, kVO_V_H4S2 }, // sqdmlal_v
  { 0b0100111000100000100100, kVO_V_B16H8S4, 0b0100111100000000001100, kVO_V_H8S4 }, // sqdmlal2_v
  { 0b0000111000100000101100, kVO_SV_BHS, 0b0000111100000000011100, kVO_V_H4S2 }, // sqdmlsl_v
  { 0b0100111000100000101100, kVO_V_B16H8S4, 0b0100111100000000011100, kVO_V_H8S4 }, // sqdmlsl2_v
  { 0b0000111000100000101101, kVO_SV_HS, 0b0000111100000000110000, kVO_SV_HS }, // sqdmulh_v
  { 0b0000111000100000110100, kVO_SV_BHS, 0b0000111100000000101100, kVO_V_H4S2 }, // sqdmull_v
  { 0b0100111000100000110100, kVO_V_B16H8S4, 0b0100111100000000101100, kVO_V_H8S4 }, // sqdmull2_v
  { 0b0010111000000000100001, kVO_SV_HS, 0b0010111100000000110100, kVO_SV_HS }, // sqrdmlah_v
  { 0b0010111000000000100011, kVO_SV_HS, 0b0010111100000000111100, kVO_SV_HS }, // sqrdmlsh_v
  { 0b0010111000100000101101, kVO_SV_HS, 0b0000111100000000110100, kVO_SV_HS }, // sqrdmulh_v
  { 0b0010111000100000100000, kVO_V_B8H4S2, 0b0010111100000000001000, kVO_V_H4S2 }, // umlal_v
  { 0b0110111000100000100000, kVO_V_B16H8S4, 0b0010111100000000001000, kVO_V_H8S4 }, // umlal2_v
  { 0b0010111000100000101000, kVO_V_B8H4S2, 0b0010111100000000011000, kVO_V_H4S2 }, // umlsl_v
  { 0b0110111000100000101000, kVO_V_B16H8S4, 0b0110111100000000011000, kVO_V_H8S4 }, // umlsl2_v
  { 0b0010111000100000110000, kVO_V_B8H4S2, 0b0010111100000000101000, kVO_V_H4S2 }, // umull_v
  { 0b0110111000100000110000, kVO_V_B16H8S4, 0b0110111100000000101000, kVO_V_H8S4 }  // umull2_v
};

const ISimdVVVx iSimdVVVx[17] = {
  { 0b0110111001000000111011, kOp_V4S, kOp_V8H, kOp_V8H }, // bfmmla_v
  { 0b0101111000000000000000, kOp_Q, kOp_S, kOp_V4S }, // sha1c_v
  { 0b0101111000000000001000, kOp_Q, kOp_S, kOp_V4S }, // sha1m_v
  { 0b0101111000000000000100, kOp_Q, kOp_S, kOp_V4S }, // sha1p_v
  { 0b0101111000000000001100, kOp_V4S, kOp_V4S, kOp_V4S }, // sha1su0_v
  { 0b0101111000000000010000, kOp_Q, kOp_Q, kOp_V4S }, // sha256h_v
  { 0b0101111000000000010100, kOp_Q, kOp_Q, kOp_V4S }, // sha256h2_v
  { 0b0101111000000000011000, kOp_V4S, kOp_V4S, kOp_V4S }, // sha256su1_v
  { 0b1100111001100000100000, kOp_Q, kOp_Q, kOp_V2D }, // sha512h_v
  { 0b1100111001100000100001, kOp_Q, kOp_Q, kOp_V2D }, // sha512h2_v
  { 0b1100111001100000100010, kOp_V2D, kOp_V2D, kOp_V2D }, // sha512su1_v
  { 0b1100111001100000110000, kOp_V4S, kOp_V4S, kOp_V4S }, // sm3partw1_v
  { 0b1100111001100000110001, kOp_V4S, kOp_V4S, kOp_V4S }, // sm3partw2_v
  { 0b1100111001100000110010, kOp_V4S, kOp_V4S, kOp_V4S }, // sm4ekey_v
  { 0b0100111010000000101001, kOp_V4S, kOp_V16B, kOp_V16B }, // smmla_v
  { 0b0110111010000000101001, kOp_V4S, kOp_V16B, kOp_V16B }, // ummla_v
  { 0b0100111010000000101011, kOp_V4S, kOp_V16B, kOp_V16B }  // usmmla_v
};

const ISimdVVx iSimdVVx[13] = {
  { 0b0100111000101000010110, kOp_V16B, kOp_V16B }, // aesd_v
  { 0b0100111000101000010010, kOp_V16B, kOp_V16B }, // aese_v
  { 0b0100111000101000011110, kOp_V16B, kOp_V16B }, // aesimc_v
  { 0b0100111000101000011010, kOp_V16B, kOp_V16B }, // aesmc_v
  { 0b0001111001100011010000, kOp_H, kOp_S }, // bfcvt_v
  { 0b0000111010100001011010, kOp_V4H, kOp_V4S }, // bfcvtn_v
  { 0b0100111010100001011010, kOp_V8H, kOp_V4S }, // bfcvtn2_v
  { 0b0001111001111110000000, kOp_GpW, kOp_D }, // fjcvtzs_v
  { 0b0101111000101000000010, kOp_S, kOp_S }, // sha1h_v
  { 0b0101111000101000000110, kOp_V4S, kOp_V4S }, // sha1su1_v
  { 0b0101111000101000001010, kOp_V4S, kOp_V4S }, // sha256su0_v
  { 0b1100111011000000100000, kOp_V2D, kOp_V2D }, // sha512su0_v
  { 0b1100111011000000100001, kOp_V4S, kOp_V4S }  // sm4e_v
};

const ISimdWWV iSimdWWV[8] = {
  { 0b0000111000100000000100, kVO_V_B8H4S2 }, // saddw_v
  { 0b0000111000100000000100, kVO_V_B16H8S4 }, // saddw2_v
  { 0b0000111000100000001100, kVO_V_B8H4S2 }, // ssubw_v
  { 0b0000111000100000001100, kVO_V_B16H8S4 }, // ssubw2_v
  { 0b0010111000100000000100, kVO_V_B8H4S2 }, // uaddw_v
  { 0b0010111000100000000100, kVO_V_B16H8S4 }, // uaddw2_v
  { 0b0010111000100000001100, kVO_V_B8H4S2 }, // usubw_v
  { 0b0010111000100000001100, kVO_V_B16H8S4 }  // usubw2_v
};

const SimdBicOrr simdBicOrr[2] = {
  { 0b0000111001100000000111, 0b0010111100000000000001 }, // bic_v
  { 0b0000111010100000000111, 0b0000111100000000000001 }  // orr_v
};

const SimdCmp simdCmp[7] = {
  { 0b0010111000100000100011, 0b0000111000100000100110, kVO_V_Any }, // cmeq_v
  { 0b0000111000100000001111, 0b0010111000100000100010, kVO_V_Any }, // cmge_v
  { 0b0000111000100000001101, 0b0000111000100000100010, kVO_V_Any }, // cmgt_v
  { 0b0010111000100000001101, 0b0000000000000000000000, kVO_V_Any }, // cmhi_v
  { 0b0010111000100000001111, 0b0000000000000000000000, kVO_V_Any }, // cmhs_v
  { 0b0000000000000000000000, 0b0010111000100000100110, kVO_V_Any }, // cmle_v
  { 0b0000000000000000000000, 0b0000111000100000101010, kVO_V_Any }  // cmlt_v
};

const SimdDot simdDot[5] = {
  { 0b0010111001000000111111, 0b0000111101000000111100, kET_S, kET_H, kET_2H }, // bfdot_v
  { 0b0000111010000000100101, 0b0000111110000000111000, kET_S, kET_B, kET_4B }, // sdot_v
  { 0b0000000000000000000000, 0b0000111100000000111100, kET_S, kET_B, kET_4B }, // sudot_v
  { 0b0010111010000000100101, 0b0010111110000000111000, kET_S, kET_B, kET_4B }, // udot_v
  { 0b0000111010000000100111, 0b0000111110000000111100, kET_S, kET_B, kET_4B }  // usdot_v
};

const SimdFcadd simdFcadd[1] = {
  { 0b0010111000000000111001 }  // fcadd_v
};

const SimdFccmpFccmpe simdFccmpFccmpe[2] = {
  { 0b00011110001000000000010000000000 }, // fccmp_v
  { 0b00011110001000000000010000010000 }  // fccmpe_v
};

const SimdFcm simdFcm[5] = {
  { 0b0000111000100000111001, kHF_C, 0b0000111010100000110110 }, // fcmeq_v
  { 0b0010111000100000111001, kHF_C, 0b0010111010100000110010 }, // fcmge_v
  { 0b0010111010100000111001, kHF_C, 0b0000111010100000110010 }, // fcmgt_v
  { 0b0000000000000000000000, kHF_C, 0b0010111010100000110110 }, // fcmle_v
  { 0b0000000000000000000000, kHF_C, 0b0000111010100000111010 }  // fcmlt_v
};

const SimdFcmla simdFcmla[1] = {
  { 0b0010111000000000110001, 0b0010111100000000000100 }  // fcmla_v
};

const SimdFcmpFcmpe simdFcmpFcmpe[2] = {
  { 0b00011110001000000010000000000000 }, // fcmp_v
  { 0b00011110001000000010000000010000 }  // fcmpe_v
};

const SimdFcvtLN simdFcvtLN[6] = {
  { 0b0000111000100001011110, 0, 0 }, // fcvtl_v
  { 0b0100111000100001011110, 0, 0 }, // fcvtl2_v
  { 0b0000111000100001011010, 0, 0 }, // fcvtn_v
  { 0b0100111000100001011010, 0, 0 }, // fcvtn2_v
  { 0b0010111000100001011010, 1, 1 }, // fcvtxn_v
  { 0b0110111000100001011010, 1, 0 }  // fcvtxn2_v
};

const SimdFcvtSV simdFcvtSV[12] = {
  { 0b0000111000100001110010, 0b0000000000000000000000, 0b0001111000100100000000, 1 }, // fcvtas_v
  { 0b0010111000100001110010, 0b0000000000000000000000, 0b0001111000100101000000, 1 }, // fcvtau_v
  { 0b0000111000100001101110, 0b0000000000000000000000, 0b0001111000110000000000, 1 }, // fcvtms_v
  { 0b0010111000100001101110, 0b0000000000000000000000, 0b0001111000110001000000, 1 }, // fcvtmu_v
  { 0b0000111000100001101010, 0b0000000000000000000000, 0b0001111000100000000000, 1 }, // fcvtns_v
  { 0b0010111000100001101010, 0b0000000000000000000000, 0b0001111000100001000000, 1 }, // fcvtnu_v
  { 0b0000111010100001101010, 0b0000000000000000000000, 0b0001111000101000000000, 1 }, // fcvtps_v
  { 0b0010111010100001101010, 0b0000000000000000000000, 0b0001111000101001000000, 1 }, // fcvtpu_v
  { 0b0000111010100001101110, 0b0000111100000000111111, 0b0001111000111000000000, 1 }, // fcvtzs_v
  { 0b0010111010100001101110, 0b0010111100000000111111, 0b0001111000111001000000, 1 }, // fcvtzu_v
  { 0b0000111000100001110110, 0b0000111100000000111001, 0b0001111000100010000000, 0 }, // scvtf_v
  { 0b0010111000100001110110, 0b0010111100000000111001, 0b0001111000100011000000, 0 }  // ucvtf_v
};

const SimdFmlal simdFmlal[6] = {
  { 0b0010111011000000111111, 0b0000111111000000111100, 0, kET_S, kET_H, kET_H }, // bfmlalb_v
  { 0b0110111011000000111111, 0b0100111111000000111100, 0, kET_S, kET_H, kET_H }, // bfmlalt_v
  { 0b0000111000100000111011, 0b0000111110000000000000, 1, kET_S, kET_H, kET_H }, // fmlal_v
  { 0b0010111000100000110011, 0b0010111110000000100000, 1, kET_S, kET_H, kET_H }, // fmlal2_v
  { 0b0000111010100000111011, 0b0000111110000000010000, 1, kET_S, kET_H, kET_H }, // fmlsl_v
  { 0b0010111010100000110011, 0b0010111110000000110000, 1, kET_S, kET_H, kET_H }  // fmlsl2_v
};

const SimdLdNStN simdLdNStN[12] = {
  { 0b0000110101000000000000, 0b0000110001000000001000, 1, 0 }, // ld1_v
  { 0b0000110101000000110000, 0b0000000000000000000000, 1, 1 }, // ld1r_v
  { 0b0000110101100000000000, 0b0000110001000000100000, 2, 0 }, // ld2_v
  { 0b0000110101100000110000, 0b0000000000000000000000, 2, 1 }, // ld2r_v
  { 0b0000110101000000001000, 0b0000110001000000010000, 3, 0 }, // ld3_v
  { 0b0000110101000000111000, 0b0000000000000000000000, 3, 1 }, // ld3r_v
  { 0b0000110101100000001000, 0b0000110001000000000000, 4, 0 }, // ld4_v
  { 0b0000110101100000111000, 0b0000000000000000000000, 4, 1 }, // ld4r_v
  { 0b0000110100000000000000, 0b0000110000000000001000, 1, 0 }, // st1_v
  { 0b0000110100100000000000, 0b0000110000000000100000, 2, 0 }, // st2_v
  { 0b0000110100000000001000, 0b0000110000000000010000, 3, 0 }, // st3_v
  { 0b0000110100100000001000, 0b0000110000000000000000, 4, 0 }  // st4_v
};

const SimdLdSt simdLdSt[2] = {
  { 0b0011110101, 0b00111100010, 0b00111100011, 0b00011100, Inst::kIdLdur_v }, // ldr_v
  { 0b0011110100, 0b00111100000, 0b00111100001, 0b00000000, Inst::kIdStur_v }  // str_v
};

const SimdLdpStp simdLdpStp[4] = {
  { 0b0010110001, 0b0000000000 }, // ldnp_v
  { 0b0010110101, 0b0010110011 }, // ldp_v
  { 0b0010110000, 0b0000000000 }, // stnp_v
  { 0b0010110100, 0b0010110010 }  // stp_v
};

const SimdLdurStur simdLdurStur[2] = {
  { 0b0011110001000000000000 }, // ldur_v
  { 0b0011110000000000000000 }  // stur_v
};

const SimdMoviMvni simdMoviMvni[2] = {
  { 0b0000111100000000000001, 0 }, // movi_v
  { 0b0000111100000000000001, 1 }  // mvni_v
};

const SimdShift simdShift[40] = {
  { 0b0000000000000000000000, 0b0000111100000000100011, 1, kVO_V_B8H4S2 }, // rshrn_v
  { 0b0000000000000000000000, 0b0100111100000000100011, 1, kVO_V_B16H8S4 }, // rshrn2_v
  { 0b0000000000000000000000, 0b0000111100000000010101, 0, kVO_V_Any }, // shl_v
  { 0b0000000000000000000000, 0b0000111100000000100001, 1, kVO_V_B8H4S2 }, // shrn_v
  { 0b0000000000000000000000, 0b0100111100000000100001, 1, kVO_V_B16H8S4 }, // shrn2_v
  { 0b0000000000000000000000, 0b0010111100000000010101, 0, kVO_V_Any }, // sli_v
  { 0b0000111000100000010111, 0b0000000000000000000000, 1, kVO_SV_Any }, // sqrshl_v
  { 0b0000000000000000000000, 0b0000111100000000100111, 1, kVO_SV_B8H4S2 }, // sqrshrn_v
  { 0b0000000000000000000000, 0b0100111100000000100111, 1, kVO_V_B16H8S4 }, // sqrshrn2_v
  { 0b0000000000000000000000, 0b0010111100000000100011, 1, kVO_SV_B8H4S2 }, // sqrshrun_v
  { 0b0000000000000000000000, 0b0110111100000000100011, 1, kVO_V_B16H8S4 }, // sqrshrun2_v
  { 0b0000111000100000010011, 0b0000111100000000011101, 0, kVO_SV_Any }, // sqshl_v
  { 0b0000000000000000000000, 0b0010111100000000011001, 0, kVO_SV_Any }, // sqshlu_v
  { 0b0000000000000000000000, 0b0000111100000000100101, 1, kVO_SV_B8H4S2 }, // sqshrn_v
  { 0b0000000000000000000000, 0b0100111100000000100101, 1, kVO_V_B16H8S4 }, // sqshrn2_v
  { 0b0000000000000000000000, 0b0010111100000000100001, 1, kVO_SV_B8H4S2 }, // sqshrun_v
  { 0b0000000000000000000000, 0b0110111100000000100001, 1, kVO_V_B16H8S4 }, // sqshrun2_v
  { 0b0000000000000000000000, 0b0010111100000000010001, 1, kVO_V_Any }, // sri_v
  { 0b0000111000100000010101, 0b0000000000000000000000, 0, kVO_V_Any }, // srshl_v
  { 0b0000000000000000000000, 0b0000111100000000001001, 1, kVO_V_Any }, // srshr_v
  { 0b0000000000000000000000, 0b0000111100000000001101, 1, kVO_V_Any }, // srsra_v
  { 0b0000111000100000010001, 0b0000000000000000000000, 0, kVO_V_Any }, // sshl_v
  { 0b0000000000000000000000, 0b0000111100000000101001, 0, kVO_V_B8H4S2 }, // sshll_v
  { 0b0000000000000000000000, 0b0100111100000000101001, 0, kVO_V_B16H8S4 }, // sshll2_v
  { 0b0000000000000000000000, 0b0000111100000000000001, 1, kVO_V_Any }, // sshr_v
  { 0b0000000000000000000000, 0b0000111100000000000101, 1, kVO_V_Any }, // ssra_v
  { 0b0010111000100000010111, 0b0000000000000000000000, 0, kVO_SV_Any }, // uqrshl_v
  { 0b0000000000000000000000, 0b0010111100000000100111, 1, kVO_SV_B8H4S2 }, // uqrshrn_v
  { 0b0000000000000000000000, 0b0110111100000000100111, 1, kVO_V_B16H8S4 }, // uqrshrn2_v
  { 0b0010111000100000010011, 0b0010111100000000011101, 0, kVO_SV_Any }, // uqshl_v
  { 0b0000000000000000000000, 0b0010111100000000100101, 1, kVO_SV_B8H4S2 }, // uqshrn_v
  { 0b0000000000000000000000, 0b0110111100000000100101, 1, kVO_V_B16H8S4 }, // uqshrn2_v
  { 0b0010111000100000010101, 0b0000000000000000000000, 0, kVO_V_Any }, // urshl_v
  { 0b0000000000000000000000, 0b0010111100000000001001, 1, kVO_V_Any }, // urshr_v
  { 0b0000000000000000000000, 0b0010111100000000001101, 1, kVO_V_Any }, // ursra_v
  { 0b0010111000100000010001, 0b0000000000000000000000, 0, kVO_V_Any }, // ushl_v
  { 0b0000000000000000000000, 0b0010111100000000101001, 0, kVO_V_B8H4S2 }, // ushll_v
  { 0b0000000000000000000000, 0b0110111100000000101001, 0, kVO_V_B16H8S4 }, // ushll2_v
  { 0b0000000000000000000000, 0b0010111100000000000001, 1, kVO_V_Any }, // ushr_v
  { 0b0000000000000000000000, 0b0010111100000000000101, 1, kVO_V_Any }  // usra_v
};

const SimdShiftES simdShiftES[2] = {
  { 0b0010111000100001001110, kVO_V_B8H4S2 }, // shll_v
  { 0b0110111000100001001110, kVO_V_B16H8S4 }  // shll2_v
};

const SimdSm3tt simdSm3tt[4] = {
  { 0b1100111001000000100000 }, // sm3tt1a_v
  { 0b1100111001000000100001 }, // sm3tt1b_v
  { 0b1100111001000000100010 }, // sm3tt2a_v
  { 0b1100111001000000100011 }  // sm3tt2b_v
};

const SimdSmovUmov simdSmovUmov[2] = {
  { 0b0000111000000000001011, kVO_V_BHS, 1 }, // smov_v
  { 0b0000111000000000001111, kVO_V_Any, 0 }  // umov_v
};

const SimdSxtlUxtl simdSxtlUxtl[4] = {
  { 0b0000111100000000101001, kVO_V_B8H4S2 }, // sxtl_v
  { 0b0100111100000000101001, kVO_V_B16H8S4 }, // sxtl2_v
  { 0b0010111100000000101001, kVO_V_B8H4S2 }, // uxtl_v
  { 0b0110111100000000101001, kVO_V_B16H8S4 }  // uxtl2_v
};

const SimdTblTbx simdTblTbx[2] = {
  { 0b0000111000000000000000 }, // tbl_v
  { 0b0000111000000000000100 }  // tbx_v
};
// ----------------------------------------------------------------------------
// ${EncodingData:End}

} // {EncodingData}
} // {InstDB}

/*
// ${CommonData:Begin}
// ------------------- Automatically generated, do not edit -------------------
const InstDB::CommonInfo InstDB::commonData[] = {
  { 0}  // #0 [ref=440x]
};
// ----------------------------------------------------------------------------
// ${CommonData:End}
*/

// ArmUtil - Id <-> Name
// =====================

#ifndef ASMJIT_DISABLE_TEXT
// ${NameData:Begin}
// ------------------- Automatically generated, do not edit -------------------
const uint32_t InstDB::_instNameIndexTable[] = {
  0x80000000, // Small ''.
  0x80000C81, // Small 'adc'.
  0x80098C81, // Small 'adcs'.
  0x80001081, // Small 'add'.
  0x80039081, // Small 'addg'.
  0x80099081, // Small 'adds'.
  0x80004881, // Small 'adr'.
  0x80084881, // Small 'adrp'.
  0x800011C1, // Small 'and'.
  0x800991C1, // Small 'ands'.
  0x80004A61, // Small 'asr'.
  0x800B4A61, // Small 'asrv'.
  0x80000281, // Small 'at'.
  0x801252A1, // Small 'autda'.
  0x83A252A1, // Small 'autdza'.
  0x802252A1, // Small 'autdb'.
  0x85A252A1, // Small 'autdzb'.
  0x8014D2A1, // Small 'autia'.
  0x00009000, // Large 'autia1716'.
  0x20BF5000, // Large 'autia|sp'.
  0xB414D2A1, // Small 'autiaz'.
  0x8024D2A1, // Small 'autib'.
  0x40055009, // Large 'autib|1716'.
  0x20BF5009, // Large 'autib|sp'.
  0xB424D2A1, // Small 'autibz'.
  0x83A4D2A1, // Small 'autiza'.
  0x85A4D2A1, // Small 'autizb'.
  0x8E161B01, // Small 'axflag'.
  0x80000002, // Small 'b'.
  0x80000CC2, // Small 'bfc'.
  0x800024C2, // Small 'bfi'.
  0x800034C2, // Small 'bfm'.
  0x80C4E0C2, // Small 'bfxil'.
  0x80000D22, // Small 'bic'.
  0x80098D22, // Small 'bics'.
  0x80000182, // Small 'bl'.
  0x80004982, // Small 'blr'.
  0x80000242, // Small 'br'.
  0x80002E42, // Small 'brk'.
  0x80004C23, // Small 'cas'.
  0x8000CC23, // Small 'casa'.
  0x8020CC23, // Small 'casab'.
  0x8080CC23, // Small 'casah'.
  0x80C0CC23, // Small 'casal'.
  0x84C0CC23, // Small 'casalb'.
  0x90C0CC23, // Small 'casalh'.
  0x80014C23, // Small 'casb'.
  0x80044C23, // Small 'cash'.
  0x80064C23, // Small 'casl'.
  0x80264C23, // Small 'caslb'.
  0x80864C23, // Small 'caslh'.
  0x80084C23, // Small 'casp'.
  0x80184C23, // Small 'caspa'.
  0x98184C23, // Small 'caspal'.
  0x80C84C23, // Small 'caspl'.
  0x800D3843, // Small 'cbnz'.
  0x80006843, // Small 'cbz'.
  0x80073463, // Small 'ccmn'.
  0x80083463, // Small 'ccmp'.
  0x816724C3, // Small 'cfinv'.
  0x8001B923, // Small 'cinc'.
  0x800B3923, // Small 'cinv'.
  0x8182C983, // Small 'clrex'.
  0x80004D83, // Small 'cls'.
  0x80006983, // Small 'clz'.
  0x800039A3, // Small 'cmn'.
  0x800041A3, // Small 'cmp'.
  0x800841A3, // Small 'cmpp'.
  0x800395C3, // Small 'cneg'.
  0x85DF0E43, // Small 'crc32b'.
  0x100D60C1, // Large 'crc32c|b'.
  0x101660C1, // Large 'crc32c|h'.
  0x104860C1, // Large 'crc32c|w'.
  0x101360C1, // Large 'crc32c|x'.
  0x91DF0E43, // Small 'crc32h'.
  0xAFDF0E43, // Small 'crc32w'.
  0xB1DF0E43, // Small 'crc32x'.
  0x80011263, // Small 'csdb'.
  0x80061663, // Small 'csel'.
  0x800A1663, // Small 'cset'.
  0x80DA1663, // Small 'csetm'.
  0x80372663, // Small 'csinc'.
  0x81672663, // Small 'csinv'.
  0x8072BA63, // Small 'csneg'.
  0x80000064, // Small 'dc'.
  0x81C9C064, // Small 'dcps1'.
  0x81D9C064, // Small 'dcps2'.
  0x81E9C064, // Small 'dcps3'.
  0x800020E4, // Small 'dgh'.
  0x800009A4, // Small 'dmb'.
  0x8009C244, // Small 'drps'.
  0x80000A64, // Small 'dsb'.
  0x800039E5, // Small 'eon'.
  0x800049E5, // Small 'eor'.
  0x80000A65, // Small 'esb'.
  0x80095305, // Small 'extr'.
  0x800A1645, // Small 'eret'.
  0x800025A7, // Small 'gmi'.
  0x800A3928, // Small 'hint'.
  0x80005188, // Small 'hlt'.
  0x80000EC8, // Small 'hvc'.
  0x80000069, // Small 'ic'.
  0x80000A69, // Small 'isb'.
  0x8042048C, // Small 'ldadd'.
  0x8242048C, // Small 'ldadda'.
  0x100D6051, // Large 'ldadda|b'.
  0x10166051, // Large 'ldadda|h'.
  0x00007051, // Large 'ldaddal'.
  0x100D7051, // Large 'ldaddal|b'.
  0x10167051, // Large 'ldaddal|h'.
  0x8442048C, // Small 'ldaddb'.
  0x9042048C, // Small 'ldaddh'.
  0x9842048C, // Small 'ldaddl'.
  0x206D5051, // Large 'ldadd|lb'.
  0x20155051, // Large 'ldadd|lh'.
  0x8009048C, // Small 'ldar'.
  0x8029048C, // Small 'ldarb'.
  0x8089048C, // Small 'ldarh'.
  0x810C048C, // Small 'ldaxp'.
  0x812C048C, // Small 'ldaxr'.
  0x852C048C, // Small 'ldaxrb'.
  0x912C048C, // Small 'ldaxrh'.
  0x81260C8C, // Small 'ldclr'.
  0x83260C8C, // Small 'ldclra'.
  0x100D6058, // Large 'ldclra|b'.
  0x10166058, // Large 'ldclra|h'.
  0x00007058, // Large 'ldclral'.
  0x100D7058, // Large 'ldclral|b'.
  0x10167058, // Large 'ldclral|h'.
  0x85260C8C, // Small 'ldclrb'.
  0x91260C8C, // Small 'ldclrh'.
  0x99260C8C, // Small 'ldclrl'.
  0x206D5058, // Large 'ldclr|lb'.
  0x20155058, // Large 'ldclr|lh'.
  0x8127948C, // Small 'ldeor'.
  0x8327948C, // Small 'ldeora'.
  0x100D605F, // Large 'ldeora|b'.
  0x1016605F, // Large 'ldeora|h'.
  0x0000705F, // Large 'ldeoral'.
  0x100D705F, // Large 'ldeoral|b'.
  0x1016705F, // Large 'ldeoral|h'.
  0x8527948C, // Small 'ldeorb'.
  0x9127948C, // Small 'ldeorh'.
  0x9927948C, // Small 'ldeorl'.
  0x206D505F, // Large 'ldeor|lb'.
  0x2015505F, // Large 'ldeor|lh'.
  0x80001C8C, // Small 'ldg'.
  0x80069C8C, // Small 'ldgm'.
  0x8120B08C, // Small 'ldlar'.
  0x8520B08C, // Small 'ldlarb'.
  0x9120B08C, // Small 'ldlarh'.
  0x8008388C, // Small 'ldnp'.
  0x8000408C, // Small 'ldp'.
  0x8179C08C, // Small 'ldpsw'.
  0x8000488C, // Small 'ldr'.
  0x8010C88C, // Small 'ldraa'.
  0x8020C88C, // Small 'ldrab'.
  0x8001488C, // Small 'ldrb'.
  0x8004488C, // Small 'ldrh'.
  0x8029C88C, // Small 'ldrsb'.
  0x8089C88C, // Small 'ldrsh'.
  0x8179C88C, // Small 'ldrsw'.
  0x8142CC8C, // Small 'ldset'.
  0x8342CC8C, // Small 'ldseta'.
  0x100D6066, // Large 'ldseta|b'.
  0x10166066, // Large 'ldseta|h'.
  0x00007066, // Large 'ldsetal'.
  0x100D7066, // Large 'ldsetal|b'.
  0x10167066, // Large 'ldsetal|h'.
  0x8542CC8C, // Small 'ldsetb'.
  0x9142CC8C, // Small 'ldseth'.
  0x9942CC8C, // Small 'ldsetl'.
  0x206D5066, // Large 'ldset|lb'.
  0x20155066, // Large 'ldset|lh'.
  0xB016CC8C, // Small 'ldsmax'.
  0x0000700E, // Large 'ldsmaxa'.
  0x100D700E, // Large 'ldsmaxa|b'.
  0x1016700E, // Large 'ldsmaxa|h'.
  0x0000800E, // Large 'ldsmaxal'.
  0x100D800E, // Large 'ldsmaxal|b'.
  0x1016800E, // Large 'ldsmaxal|h'.
  0x100D600E, // Large 'ldsmax|b'.
  0x1016600E, // Large 'ldsmax|h'.
  0x100E600E, // Large 'ldsmax|l'.
  0x206D600E, // Large 'ldsmax|lb'.
  0x2015600E, // Large 'ldsmax|lh'.
  0x9C96CC8C, // Small 'ldsmin'.
  0x00007017, // Large 'ldsmina'.
  0x100D7017, // Large 'ldsmina|b'.
  0x10167017, // Large 'ldsmina|h'.
  0x00008017, // Large 'ldsminal'.
  0x100D8017, // Large 'ldsminal|b'.
  0x10168017, // Large 'ldsminal|h'.
  0x100D6017, // Large 'ldsmin|b'.
  0x10166017, // Large 'ldsmin|h'.
  0x100E6017, // Large 'ldsmin|l'.
  0x206D6017, // Large 'ldsmin|lb'.
  0x20156017, // Large 'ldsmin|lh'.
  0x8009508C, // Small 'ldtr'.
  0x8029508C, // Small 'ldtrb'.
  0x8089508C, // Small 'ldtrh'.
  0x8539508C, // Small 'ldtrsb'.
  0x9139508C, // Small 'ldtrsh'.
  0xAF39508C, // Small 'ldtrsw'.
  0xB016D48C, // Small 'ldumax'.
  0x0000701F, // Large 'ldumaxa'.
  0x100D701F, // Large 'ldumaxa|b'.
  0x1016701F, // Large 'ldumaxa|h'.
  0x0000801F, // Large 'ldumaxal'.
  0x100D801F, // Large 'ldumaxal|b'.
  0x1016801F, // Large 'ldumaxal|h'.
  0x100D601F, // Large 'ldumax|b'.
  0x1016601F, // Large 'ldumax|h'.
  0x100E601F, // Large 'ldumax|l'.
  0x206D601F, // Large 'ldumax|lb'.
  0x2015601F, // Large 'ldumax|lh'.
  0x9C96D48C, // Small 'ldumin'.
  0x00007027, // Large 'ldumina'.
  0x100D7027, // Large 'ldumina|b'.
  0x10167027, // Large 'ldumina|h'.
  0x00008027, // Large 'lduminal'.
  0x100D8027, // Large 'lduminal|b'.
  0x10168027, // Large 'lduminal|h'.
  0x100D6027, // Large 'ldumin|b'.
  0x10166027, // Large 'ldumin|h'.
  0x100E6027, // Large 'ldumin|l'.
  0x206D6027, // Large 'ldumin|lb'.
  0x20156027, // Large 'ldumin|lh'.
  0x8009548C, // Small 'ldur'.
  0x8029548C, // Small 'ldurb'.
  0x8089548C, // Small 'ldurh'.
  0x8539548C, // Small 'ldursb'.
  0x9139548C, // Small 'ldursh'.
  0xAF39548C, // Small 'ldursw'.
  0x8008608C, // Small 'ldxp'.
  0x8009608C, // Small 'ldxr'.
  0x8029608C, // Small 'ldxrb'.
  0x8089608C, // Small 'ldxrh'.
  0x8000326C, // Small 'lsl'.
  0x800B326C, // Small 'lslv'.
  0x80004A6C, // Small 'lsr'.
  0x800B4A6C, // Small 'lsrv'.
  0x8002102D, // Small 'madd'.
  0x800395CD, // Small 'mneg'.
  0x800059ED, // Small 'mov'.
  0x8005D9ED, // Small 'movk'.
  0x800759ED, // Small 'movn'.
  0x800D59ED, // Small 'movz'.
  0x80004E4D, // Small 'mrs'.
  0x80004A6D, // Small 'msr'.
  0x8001566D, // Small 'msub'.
  0x800032AD, // Small 'mul'.
  0x80003ACD, // Small 'mvn'.
  0x80001CAE, // Small 'neg'.
  0x80099CAE, // Small 'negs'.
  0x80000CEE, // Small 'ngc'.
  0x80098CEE, // Small 'ngcs'.
  0x800041EE, // Small 'nop'.
  0x80003A4F, // Small 'orn'.
  0x80004A4F, // Small 'orr'.
  0x80120C30, // Small 'pacda'.
  0x80220C30, // Small 'pacdb'.
  0x83A20C30, // Small 'pacdza'.
  0x85A20C30, // Small 'pacdzb'.
  0x80138C30, // Small 'pacga'.
  0x80214E70, // Small 'pssbb'.
  0x800A2452, // Small 'rbit'.
  0x800050B2, // Small 'ret'.
  0x800058B2, // Small 'rev'.
  0x20073138, // Large 'rev|16'.
  0x81DF58B2, // Small 'rev32'.
  0x208F3138, // Large 'rev|64'.
  0x800049F2, // Small 'ror'.
  0x800B49F2, // Small 'rorv'.
  0x80000C53, // Small 'sbc'.
  0x80098C53, // Small 'sbcs'.
  0x81A49853, // Small 'sbfiz'.
  0x80069853, // Small 'sbfm'.
  0x800C1853, // Small 'sbfx'.
  0x800B2493, // Small 'sdiv'.
  0x113B4134, // Large 'setf|8'.
  0x20074134, // Large 'setf|16'.
  0x800058B3, // Small 'sev'.
  0x800658B3, // Small 'sevl'.
  0x984205B3, // Small 'smaddl'.
  0x80000DB3, // Small 'smc'.
  0x9872B9B3, // Small 'smnegl'.
  0x982ACDB3, // Small 'smsubl'.
  0x808655B3, // Small 'smulh'.
  0x80C655B3, // Small 'smull'.
  0x80010A73, // Small 'ssbb'.
  0x8003F693, // Small 'st2g'.
  0x80420693, // Small 'stadd'.
  0x98420693, // Small 'staddl'.
  0x84420693, // Small 'staddb'.
  0x206D50C7, // Large 'stadd|lb'.
  0x90420693, // Small 'staddh'.
  0x201550C7, // Large 'stadd|lh'.
  0x81260E93, // Small 'stclr'.
  0x99260E93, // Small 'stclrl'.
  0x85260E93, // Small 'stclrb'.
  0x206D50CC, // Large 'stclr|lb'.
  0x91260E93, // Small 'stclrh'.
  0x201550CC, // Large 'stclr|lh'.
  0x81279693, // Small 'steor'.
  0x99279693, // Small 'steorl'.
  0x85279693, // Small 'steorb'.
  0x206D50D1, // Large 'steor|lb'.
  0x91279693, // Small 'steorh'.
  0x201550D1, // Large 'steor|lh'.
  0x80001E93, // Small 'stg'.
  0x80069E93, // Small 'stgm'.
  0x80081E93, // Small 'stgp'.
  0x81263293, // Small 'stllr'.
  0x85263293, // Small 'stllrb'.
  0x91263293, // Small 'stllrh'.
  0x80093293, // Small 'stlr'.
  0x80293293, // Small 'stlrb'.
  0x80893293, // Small 'stlrh'.
  0x810C3293, // Small 'stlxp'.
  0x812C3293, // Small 'stlxr'.
  0x852C3293, // Small 'stlxrb'.
  0x912C3293, // Small 'stlxrh'.
  0x80083A93, // Small 'stnp'.
  0x80004293, // Small 'stp'.
  0x80004A93, // Small 'str'.
  0x80014A93, // Small 'strb'.
  0x80044A93, // Small 'strh'.
  0x8142CE93, // Small 'stset'.
  0x9942CE93, // Small 'stsetl'.
  0x8542CE93, // Small 'stsetb'.
  0x206D50D6, // Large 'stset|lb'.
  0x9142CE93, // Small 'stseth'.
  0x201550D6, // Large 'stset|lh'.
  0xB016CE93, // Small 'stsmax'.
  0x100E606F, // Large 'stsmax|l'.
  0x100D606F, // Large 'stsmax|b'.
  0x206D606F, // Large 'stsmax|lb'.
  0x1016606F, // Large 'stsmax|h'.
  0x2015606F, // Large 'stsmax|lh'.
  0x9C96CE93, // Small 'stsmin'.
  0x100E6075, // Large 'stsmin|l'.
  0x100D6075, // Large 'stsmin|b'.
  0x206D6075, // Large 'stsmin|lb'.
  0x10166075, // Large 'stsmin|h'.
  0x20156075, // Large 'stsmin|lh'.
  0x80095293, // Small 'sttr'.
  0x80295293, // Small 'sttrb'.
  0x80895293, // Small 'sttrh'.
  0xB016D693, // Small 'stumax'.
  0x100E607B, // Large 'stumax|l'.
  0x100D607B, // Large 'stumax|b'.
  0x206D607B, // Large 'stumax|lb'.
  0x1016607B, // Large 'stumax|h'.
  0x2015607B, // Large 'stumax|lh'.
  0x9C96D693, // Small 'stumin'.
  0x100E6081, // Large 'stumin|l'.
  0x100D6081, // Large 'stumin|b'.
  0x206D6081, // Large 'stumin|lb'.
  0x10166081, // Large 'stumin|h'.
  0x20156081, // Large 'stumin|lh'.
  0x80095693, // Small 'stur'.
  0x80295693, // Small 'sturb'.
  0x80895693, // Small 'sturh'.
  0x80086293, // Small 'stxp'.
  0x80096293, // Small 'stxr'.
  0x80296293, // Small 'stxrb'.
  0x80896293, // Small 'stxrh'.
  0x807EEA93, // Small 'stz2g'.
  0x8003EA93, // Small 'stzg'.
  0x80D3EA93, // Small 'stzgm'.
  0x80000AB3, // Small 'sub'.
  0x80038AB3, // Small 'subg'.
  0x80080AB3, // Small 'subp'.
  0x81380AB3, // Small 'subps'.
  0x80098AB3, // Small 'subs'.
  0x80000ED3, // Small 'svc'.
  0x800042F3, // Small 'swp'.
  0x8000C2F3, // Small 'swpa'.
  0x8020C2F3, // Small 'swpab'.
  0x8080C2F3, // Small 'swpah'.
  0x80C0C2F3, // Small 'swpal'.
  0x84C0C2F3, // Small 'swpalb'.
  0x90C0C2F3, // Small 'swpalh'.
  0x800142F3, // Small 'swpb'.
  0x800442F3, // Small 'swph'.
  0x800642F3, // Small 'swpl'.
  0x802642F3, // Small 'swplb'.
  0x808642F3, // Small 'swplh'.
  0x80015313, // Small 'sxtb'.
  0x80045313, // Small 'sxth'.
  0x800BD313, // Small 'sxtw'.
  0x80004F33, // Small 'sys'.
  0x80048994, // Small 'tlbi'.
  0x80005274, // Small 'tst'.
  0x800D3854, // Small 'tbnz'.
  0x80006854, // Small 'tbz'.
  0x81A49855, // Small 'ubfiz'.
  0x80069855, // Small 'ubfm'.
  0x800C1855, // Small 'ubfx'.
  0x80001895, // Small 'udf'.
  0x800B2495, // Small 'udiv'.
  0x984205B5, // Small 'umaddl'.
  0x9872B9B5, // Small 'umnegl'.
  0x80C655B5, // Small 'umull'.
  0x808655B5, // Small 'umulh'.
  0x982ACDB5, // Small 'umsubl'.
  0x80015315, // Small 'uxtb'.
  0x80045315, // Small 'uxth'.
  0x800014D7, // Small 'wfe'.
  0x800024D7, // Small 'wfi'.
  0x8E161838, // Small 'xaflag'.
  0x80418618, // Small 'xpacd'.
  0x80918618, // Small 'xpaci'.
  0x208850DB, // Large 'xpacl|ri'.
  0x80461539, // Small 'yield'.
  0x80004C41, // Small 'abs'.
  0x80001081, // Small 'add'.
  0x80E41081, // Small 'addhn'.
  0xBAE41081, // Small 'addhn2'.
  0x80081081, // Small 'addp'.
  0x800B1081, // Small 'addv'.
  0x80024CA1, // Small 'aesd'.
  0x8002CCA1, // Small 'aese'.
  0x86D4CCA1, // Small 'aesimc'.
  0x8036CCA1, // Small 'aesmc'.
  0x800011C1, // Small 'and'.
  0x800C0462, // Small 'bcax'.
  0x814B0CC2, // Small 'bfcvt'.
  0x9D4B0CC2, // Small 'bfcvtn'.
  0x20B150E0, // Large 'bfcvt|n2'.
  0x814790C2, // Small 'bfdot'.
  0x206D50E5, // Large 'bfmla|lb'.
  0x20EA50E5, // Large 'bfmla|lt'.
  0x82C6B4C2, // Small 'bfmmla'.
  0x80000D22, // Small 'bic'.
  0x80001922, // Small 'bif'.
  0x80005122, // Small 'bit'.
  0x80003262, // Small 'bsl'.
  0x80004D83, // Small 'cls'.
  0x80006983, // Small 'clz'.
  0x800895A3, // Small 'cmeq'.
  0x80029DA3, // Small 'cmge'.
  0x800A1DA3, // Small 'cmgt'.
  0x8004A1A3, // Small 'cmhi'.
  0x8009A1A3, // Small 'cmhs'.
  0x8002B1A3, // Small 'cmle'.
  0x800A31A3, // Small 'cmlt'.
  0x8149D1A3, // Small 'cmtst'.
  0x800051C3, // Small 'cnt'.
  0x800042A4, // Small 'dup'.
  0x800049E5, // Small 'eor'.
  0x800F49E5, // Small 'eor3'.
  0x80005305, // Small 'ext'.
  0x80020826, // Small 'fabd'.
  0x80098826, // Small 'fabs'.
  0x80538C26, // Small 'facge'.
  0x81438C26, // Small 'facgt'.
  0x80021026, // Small 'fadd'.
  0x81021026, // Small 'faddp'.
  0x80420466, // Small 'fcadd'.
  0x81068C66, // Small 'fccmp'.
  0x8B068C66, // Small 'fccmpe'.
  0x8112B466, // Small 'fcmeq'.
  0x8053B466, // Small 'fcmge'.
  0x8143B466, // Small 'fcmgt'.
  0x80163466, // Small 'fcmla'.
  0x80563466, // Small 'fcmle'.
  0x81463466, // Small 'fcmlt'.
  0x80083466, // Small 'fcmp'.
  0x80583466, // Small 'fcmpe'.
  0x80C2CC66, // Small 'fcsel'.
  0x800A5866, // Small 'fcvt'.
  0xA61A5866, // Small 'fcvtas'.
  0xAA1A5866, // Small 'fcvtau'.
  0x80CA5866, // Small 'fcvtl'.
  0xBACA5866, // Small 'fcvtl2'.
  0xA6DA5866, // Small 'fcvtms'.
  0xAADA5866, // Small 'fcvtmu'.
  0x80EA5866, // Small 'fcvtn'.
  0xBAEA5866, // Small 'fcvtn2'.
  0xA6EA5866, // Small 'fcvtns'.
  0xAAEA5866, // Small 'fcvtnu'.
  0xA70A5866, // Small 'fcvtps'.
  0xAB0A5866, // Small 'fcvtpu'.
  0x9D8A5866, // Small 'fcvtxn'.
  0x20B150EC, // Large 'fcvtx|n2'.
  0xA7AA5866, // Small 'fcvtzs'.
  0xABAA5866, // Small 'fcvtzu'.
  0x800B2486, // Small 'fdiv'.
  0x101060F1, // Large 'fjcvtz|s'.
  0x804205A6, // Small 'fmadd'.
  0x800C05A6, // Small 'fmax'.
  0x9AEC05A6, // Small 'fmaxnm'.
  0x104460F7, // Large 'fmaxnm|p'.
  0x10E360F7, // Large 'fmaxnm|v'.
  0x810C05A6, // Small 'fmaxp'.
  0x816C05A6, // Small 'fmaxv'.
  0x800725A6, // Small 'fmin'.
  0x9AE725A6, // Small 'fminnm'.
  0x104460FD, // Large 'fminnm|p'.
  0x10E360FD, // Large 'fminnm|v'.
  0x810725A6, // Small 'fminp'.
  0x816725A6, // Small 'fminv'.
  0x8000B1A6, // Small 'fmla'.
  0x80C0B1A6, // Small 'fmlal'.
  0xBAC0B1A6, // Small 'fmlal2'.
  0x8009B1A6, // Small 'fmls'.
  0x80C9B1A6, // Small 'fmlsl'.
  0xBAC9B1A6, // Small 'fmlsl2'.
  0x800B3DA6, // Small 'fmov'.
  0x802ACDA6, // Small 'fmsub'.
  0x800655A6, // Small 'fmul'.
  0x818655A6, // Small 'fmulx'.
  0x800395C6, // Small 'fneg'.
  0x8840B5C6, // Small 'fnmadd'.
  0x8559B5C6, // Small 'fnmsub'.
  0x80CAB5C6, // Small 'fnmul'.
  0x8B019646, // Small 'frecpe'.
  0xA7019646, // Small 'frecps'.
  0xB1019646, // Small 'frecpx'.
  0x10137087, // Large 'frint32|x'.
  0x108E7087, // Large 'frint32|z'.
  0x308F5087, // Large 'frint|64x'.
  0x30925087, // Large 'frint|64z'.
  0x83472646, // Small 'frinta'.
  0x93472646, // Small 'frinti'.
  0x9B472646, // Small 'frintm'.
  0x9D472646, // Small 'frintn'.
  0xA1472646, // Small 'frintp'.
  0xB1472646, // Small 'frintx'.
  0xB5472646, // Small 'frintz'.
  0x20D25103, // Large 'frsqr|te'.
  0x20705103, // Large 'frsqr|ts'.
  0x81494666, // Small 'fsqrt'.
  0x80015666, // Small 'fsub'.
  0x80004DC9, // Small 'ins'.
  0x8000708C, // Small 'ld1'.
  0x8009708C, // Small 'ld1r'.
  0x8000748C, // Small 'ld2'.
  0x8009748C, // Small 'ld2r'.
  0x8000788C, // Small 'ld3'.
  0x8009788C, // Small 'ld3r'.
  0x80007C8C, // Small 'ld4'.
  0x80097C8C, // Small 'ld4r'.
  0x8008388C, // Small 'ldnp'.
  0x8000408C, // Small 'ldp'.
  0x8000488C, // Small 'ldr'.
  0x8009548C, // Small 'ldur'.
  0x8000058D, // Small 'mla'.
  0x80004D8D, // Small 'mls'.
  0x800059ED, // Small 'mov'.
  0x8004D9ED, // Small 'movi'.
  0x800032AD, // Small 'mul'.
  0x80003ACD, // Small 'mvn'.
  0x8004BACD, // Small 'mvni'.
  0x80001CAE, // Small 'neg'.
  0x800051EE, // Small 'not'.
  0x80003A4F, // Small 'orn'.
  0x80004A4F, // Small 'orr'.
  0x800655B0, // Small 'pmul'.
  0x80C655B0, // Small 'pmull'.
  0xBAC655B0, // Small 'pmull2'.
  0x9C821032, // Small 'raddhn'.
  0x30B04108, // Large 'radd|hn2'.
  0x800E6032, // Small 'rax1'.
  0x800A2452, // Small 'rbit'.
  0x20073138, // Large 'rev|16'.
  0x81DF58B2, // Small 'rev32'.
  0x208F3138, // Large 'rev|64'.
  0x80E92272, // Small 'rshrn'.
  0xBAE92272, // Small 'rshrn2'.
  0x9C815672, // Small 'rsubhn'.
  0x30B0410C, // Large 'rsub|hn2'.
  0x80008833, // Small 'saba'.
  0x80C08833, // Small 'sabal'.
  0xBAC08833, // Small 'sabal2'.
  0x80020833, // Small 'sabd'.
  0x80C20833, // Small 'sabdl'.
  0xBAC20833, // Small 'sabdl2'.
  0xA0C09033, // Small 'sadalp'.
  0x80C21033, // Small 'saddl'.
  0xBAC21033, // Small 'saddl2'.
  0xA0C21033, // Small 'saddlp'.
  0xACC21033, // Small 'saddlv'.
  0x81721033, // Small 'saddw'.
  0xBB721033, // Small 'saddw2'.
  0x806A5873, // Small 'scvtf'.
  0x800A3C93, // Small 'sdot'.
  0x803E0513, // Small 'sha1c'.
  0x808E0513, // Small 'sha1h'.
  0x80DE0513, // Small 'sha1m'.
  0x810E0513, // Small 'sha1p'.
  0x30354110, // Large 'sha1|su0'.
  0x303E4110, // Large 'sha1|su1'.
  0x1016602F, // Large 'sha256|h'.
  0x2095602F, // Large 'sha256|h2'.
  0x0000902F, // Large 'sha256su0'.
  0x1005802F, // Large 'sha256su|1'.
  0x10166038, // Large 'sha512|h'.
  0x20956038, // Large 'sha512|h2'.
  0x30356038, // Large 'sha512|su0'.
  0x303E6038, // Large 'sha512|su1'.
  0x80420513, // Small 'shadd'.
  0x80003113, // Small 'shl'.
  0x80063113, // Small 'shll'.
  0x81D63113, // Small 'shll2'.
  0x80074913, // Small 'shrn'.
  0x81D74913, // Small 'shrn2'.
  0x802ACD13, // Small 'shsub'.
  0x80002593, // Small 'sli'.
  0x10058041, // Large 'sm3partw|1'.
  0x10328041, // Large 'sm3partw|2'.
  0xB939F9B3, // Small 'sm3ss1'.
  0x10006114, // Large 'sm3tt1|a'.
  0x100D6114, // Large 'sm3tt1|b'.
  0x211A5114, // Large 'sm3tt|2a'.
  0x211C5114, // Large 'sm3tt|2b'.
  0x8002FDB3, // Small 'sm4e'.
  0x0000711E, // Large 'sm4ekey'.
  0x800C05B3, // Small 'smax'.
  0x810C05B3, // Small 'smaxp'.
  0x816C05B3, // Small 'smaxv'.
  0x800725B3, // Small 'smin'.
  0x810725B3, // Small 'sminp'.
  0x816725B3, // Small 'sminv'.
  0x80C0B1B3, // Small 'smlal'.
  0xBAC0B1B3, // Small 'smlal2'.
  0x80C9B1B3, // Small 'smlsl'.
  0xBAC9B1B3, // Small 'smlsl2'.
  0x801635B3, // Small 'smmla'.
  0x800B3DB3, // Small 'smov'.
  0x80C655B3, // Small 'smull'.
  0xBAC655B3, // Small 'smull2'.
  0x81310633, // Small 'sqabs'.
  0x80420633, // Small 'sqadd'.
  0x00007097, // Large 'sqdmlal'.
  0x10327097, // Large 'sqdmlal|2'.
  0x209E5097, // Large 'sqdml|sl'.
  0x309E5097, // Large 'sqdml|sl2'.
  0x101660A1, // Large 'sqdmul|h'.
  0x100E60A1, // Large 'sqdmul|l'.
  0x209F60A1, // Large 'sqdmul|l2'.
  0x8072BA33, // Small 'sqneg'.
  0x101670A7, // Large 'sqrdmla|h'.
  0x202F60A7, // Large 'sqrdml|sh'.
  0x30AE50A7, // Large 'sqrdm|ulh'.
  0x9889CA33, // Small 'sqrshl'.
  0x101C6049, // Large 'sqrshr|n'.
  0x20B16049, // Large 'sqrshr|n2'.
  0x00008049, // Large 'sqrshrun'.
  0x10328049, // Large 'sqrshrun|2'.
  0x80C44E33, // Small 'sqshl'.
  0xAAC44E33, // Small 'sqshlu'.
  0x9D244E33, // Small 'sqshrn'.
  0x20B150B3, // Large 'sqshr|n2'.
  0x101C60B3, // Large 'sqshru|n'.
  0x20B160B3, // Large 'sqshru|n2'.
  0x802ACE33, // Small 'sqsub'.
  0x80EA6233, // Small 'sqxtn'.
  0xBAEA6233, // Small 'sqxtn2'.
  0x9D5A6233, // Small 'sqxtun'.
  0x20B15125, // Large 'sqxtu|n2'.
  0x8840A253, // Small 'srhadd'.
  0x80002653, // Small 'sri'.
  0x80C44E53, // Small 'srshl'.
  0x81244E53, // Small 'srshr'.
  0x80194E53, // Small 'srsra'.
  0x80062273, // Small 'sshl'.
  0x80C62273, // Small 'sshll'.
  0xBAC62273, // Small 'sshll2'.
  0x80092273, // Small 'sshr'.
  0x8000CA73, // Small 'ssra'.
  0x80C15673, // Small 'ssubl'.
  0xBAC15673, // Small 'ssubl2'.
  0x81715673, // Small 'ssubw'.
  0xBB715673, // Small 'ssubw2'.
  0x80007293, // Small 'st1'.
  0x80007693, // Small 'st2'.
  0x80007A93, // Small 'st3'.
  0x80007E93, // Small 'st4'.
  0x80083A93, // Small 'stnp'.
  0x80004293, // Small 'stp'.
  0x80004A93, // Small 'str'.
  0x80095693, // Small 'stur'.
  0x80000AB3, // Small 'sub'.
  0x80E40AB3, // Small 'subhn'.
  0xBAE40AB3, // Small 'subhn2'.
  0x814792B3, // Small 'sudot'.
  0x8840C6B3, // Small 'suqadd'.
  0x80065313, // Small 'sxtl'.
  0x81D65313, // Small 'sxtl2'.
  0x80003054, // Small 'tbl'.
  0x80006054, // Small 'tbx'.
  0x800E3A54, // Small 'trn1'.
  0x800EBA54, // Small 'trn2'.
  0x80008835, // Small 'uaba'.
  0x80C08835, // Small 'uabal'.
  0xBAC08835, // Small 'uabal2'.
  0x80020835, // Small 'uabd'.
  0x80C20835, // Small 'uabdl'.
  0xBAC20835, // Small 'uabdl2'.
  0xA0C09035, // Small 'uadalp'.
  0x80C21035, // Small 'uaddl'.
  0xBAC21035, // Small 'uaddl2'.
  0xA0C21035, // Small 'uaddlp'.
  0xACC21035, // Small 'uaddlv'.
  0x81721035, // Small 'uaddw'.
  0xBB721035, // Small 'uaddw2'.
  0x806A5875, // Small 'ucvtf'.
  0x800A3C95, // Small 'udot'.
  0x80420515, // Small 'uhadd'.
  0x802ACD15, // Small 'uhsub'.
  0x800C05B5, // Small 'umax'.
  0x810C05B5, // Small 'umaxp'.
  0x816C05B5, // Small 'umaxv'.
  0x800725B5, // Small 'umin'.
  0x810725B5, // Small 'uminp'.
  0x816725B5, // Small 'uminv'.
  0x80C0B1B5, // Small 'umlal'.
  0xBAC0B1B5, // Small 'umlal2'.
  0x80C9B1B5, // Small 'umlsl'.
  0xBAC9B1B5, // Small 'umlsl2'.
  0x801635B5, // Small 'ummla'.
  0x800B3DB5, // Small 'umov'.
  0x80C655B5, // Small 'umull'.
  0xBAC655B5, // Small 'umull2'.
  0x80420635, // Small 'uqadd'.
  0x9889CA35, // Small 'uqrshl'.
  0x101C60B9, // Large 'uqrshr|n'.
  0x20B160B9, // Large 'uqrshr|n2'.
  0x80C44E35, // Small 'uqshl'.
  0x9D244E35, // Small 'uqshrn'.
  0x20B1512A, // Large 'uqshr|n2'.
  0x802ACE35, // Small 'uqsub'.
  0x80EA6235, // Small 'uqxtn'.
  0xBAEA6235, // Small 'uqxtn2'.
  0x8B019655, // Small 'urecpe'.
  0x8840A255, // Small 'urhadd'.
  0x80C44E55, // Small 'urshl'.
  0x81244E55, // Small 'urshr'.
  0x20D2512F, // Large 'ursqr|te'.
  0x80194E55, // Small 'ursra'.
  0x81479275, // Small 'usdot'.
  0x80062275, // Small 'ushl'.
  0x80C62275, // Small 'ushll'.
  0xBAC62275, // Small 'ushll2'.
  0x80092275, // Small 'ushr'.
  0x82C6B675, // Small 'usmmla'.
  0x8840C675, // Small 'usqadd'.
  0x8000CA75, // Small 'usra'.
  0x80C15675, // Small 'usubl'.
  0xBAC15675, // Small 'usubl2'.
  0x81715675, // Small 'usubw'.
  0xBB715675, // Small 'usubw2'.
  0x80065315, // Small 'uxtl'.
  0x81D65315, // Small 'uxtl2'.
  0x800E4355, // Small 'uzp1'.
  0x800EC355, // Small 'uzp2'.
  0x80004838, // Small 'xar'.
  0x80003A98, // Small 'xtn'.
  0x800EBA98, // Small 'xtn2'.
  0x800E413A, // Small 'zip1'.
  0x800EC13A  // Small 'zip2'.
};

const char InstDB::_instNameStringTable[] =
  "autia1716autibldsmaxalhldsminalldumaxallduminalsha256su0sha512su1sm3partwsqrshru"
  "nldaddalldclralldeoralldsetallbstsmaxstsminstumaxstuminfrint32z64x64zh2sqdmlalsl"
  "2sqdmulsqrdmlaulhn2sqshruuqrshrspcrc32cstaddstclrsteorstsetxpaclbfcvtbfmlaltfcvt"
  "xfjcvtzfmaxnmfminnmfrsqrraddrsubsha1sm3tt12a2bsm4ekeysqxtuuqshrursqrsetfrev8";


const InstDB::InstNameIndex InstDB::instNameIndex[26] = {
  { Inst::kIdAdc          , Inst::kIdAnd_v         + 1 },
  { Inst::kIdB            , Inst::kIdBsl_v         + 1 },
  { Inst::kIdCas          , Inst::kIdCnt_v         + 1 },
  { Inst::kIdDc           , Inst::kIdDup_v         + 1 },
  { Inst::kIdEon          , Inst::kIdExt_v         + 1 },
  { Inst::kIdFabd_v       , Inst::kIdFsub_v        + 1 },
  { Inst::kIdGmi          , Inst::kIdGmi           + 1 },
  { Inst::kIdHint         , Inst::kIdHvc           + 1 },
  { Inst::kIdIc           , Inst::kIdIns_v         + 1 },
  { Inst::kIdNone         , Inst::kIdNone          + 1 },
  { Inst::kIdNone         , Inst::kIdNone          + 1 },
  { Inst::kIdLdadd        , Inst::kIdLdur_v        + 1 },
  { Inst::kIdMadd         , Inst::kIdMvni_v        + 1 },
  { Inst::kIdNeg          , Inst::kIdNot_v         + 1 },
  { Inst::kIdOrn          , Inst::kIdOrr_v         + 1 },
  { Inst::kIdPacda        , Inst::kIdPmull2_v      + 1 },
  { Inst::kIdNone         , Inst::kIdNone          + 1 },
  { Inst::kIdRbit         , Inst::kIdRsubhn2_v     + 1 },
  { Inst::kIdSbc          , Inst::kIdSxtl2_v       + 1 },
  { Inst::kIdTlbi         , Inst::kIdTrn2_v        + 1 },
  { Inst::kIdUbfiz        , Inst::kIdUzp2_v        + 1 },
  { Inst::kIdNone         , Inst::kIdNone          + 1 },
  { Inst::kIdWfe          , Inst::kIdWfi           + 1 },
  { Inst::kIdXaflag       , Inst::kIdXtn2_v        + 1 },
  { Inst::kIdYield        , Inst::kIdYield         + 1 },
  { Inst::kIdZip1_v       , Inst::kIdZip2_v        + 1 }
};
// ----------------------------------------------------------------------------
// ${NameData:End}
#endif

ASMJIT_END_SUB_NAMESPACE

#endif // !ASMJIT_NO_AARCH64
