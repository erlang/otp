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

// Don't store `_nameDataIndex` if instruction names are disabled. Since some
// APIs can use `_nameDataIndex` it's much safer if it's zero if it's not used.
#if defined(ASMJIT_NO_TEXT)
  #define NAME_DATA_INDEX(x) 0
#else
  #define NAME_DATA_INDEX(x) x
#endif

// Defines an ARM/AArch64 instruction.
#define INST(id, opcodeEncoding, opcodeData, rwInfoIndex, flags, opcodeDataIndex, nameDataIndex) { \
  uint32_t(kEncoding##opcodeEncoding),      \
  uint32_t(opcodeDataIndex),                \
  0,                                        \
  uint32_t(NAME_DATA_INDEX(nameDataIndex)), \
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
  // +------------------+---------------------+--------------------------------------------------------------------------------------+-----------+---------------------------+----+-----+
  // | Instruction Id   | Encoding            | Opcode Data                                                                          | RW Info   | Instruction Flags         |DatX|NameX|
  // +------------------+---------------------+--------------------------------------------------------------------------------------+-----------+---------------------------+----+-----+
  // ${InstInfo:Begin}
  INST(None             , None               , (_)                                                                                   , 0         , 0                         , 0  , 0   ), // #0
  INST(Adc              , BaseRRR            , (0b0001101000000000000000, kWX, kZR, kWX, kZR, kWX, kZR, true)                        , kRWI_W    , 0                         , 0  , 1   ), // #1
  INST(Adcs             , BaseRRR            , (0b0011101000000000000000, kWX, kZR, kWX, kZR, kWX, kZR, true)                        , kRWI_W    , 0                         , 1  , 5   ), // #2
  INST(Add              , BaseAddSub         , (0b0001011000, 0b0001011001, 0b0010001)                                               , kRWI_W    , 0                         , 0  , 978 ), // #3
  INST(Addg             , BaseRRII           , (0b1001000110000000000000, kX, kSP, kX, kSP, 6, 4, 16, 4, 0, 10)                      , kRWI_W    , 0                         , 0  , 10  ), // #4
  INST(Adds             , BaseAddSub         , (0b0101011000, 0b0101011001, 0b0110001)                                               , kRWI_W    , 0                         , 1  , 15  ), // #5
  INST(Adr              , BaseAdr            , (0b0001000000000000000000, OffsetType::kAArch64_ADR)                                  , kRWI_W    , 0                         , 0  , 25  ), // #6
  INST(Adrp             , BaseAdr            , (0b1001000000000000000000, OffsetType::kAArch64_ADRP)                                 , kRWI_W    , 0                         , 1  , 29  ), // #7
  INST(And              , BaseLogical        , (0b0001010000, 0b00100100, 0)                                                         , kRWI_W    , 0                         , 0  , 57  ), // #8
  INST(Ands             , BaseLogical        , (0b1101010000, 0b11100100, 0)                                                         , kRWI_W    , 0                         , 1  , 61  ), // #9
  INST(Asr              , BaseShift          , (0b0001101011000000001010, 0b0001001100000000011111, 0)                               , kRWI_W    , 0                         , 0  , 66  ), // #10
  INST(Asrv             , BaseShift          , (0b0001101011000000001010, 0b0000000000000000000000, 0)                               , kRWI_W    , 0                         , 1  , 70  ), // #11
  INST(At               , BaseAtDcIcTlbi     , (0b00011111110000, 0b00001111000000, true)                                            , kRWI_RX   , 0                         , 0  , 75  ), // #12
  INST(Autda            , BaseRR             , (0b11011010110000010001100000000000, kX, kZR, 0, kX, kSP, 5, true)                    , kRWI_X    , 0                         , 0  , 78  ), // #13
  INST(Autdza           , BaseR              , (0b11011010110000010011101111100000, kX, kZR, 0)                                      , kRWI_X    , 0                         , 0  , 90  ), // #14
  INST(Autdb            , BaseRR             , (0b11011010110000010001110000000000, kX, kZR, 0, kX, kSP, 5, true)                    , kRWI_X    , 0                         , 1  , 84  ), // #15
  INST(Autdzb           , BaseR              , (0b11011010110000010011111111100000, kX, kZR, 0)                                      , kRWI_X    , 0                         , 1  , 97  ), // #16
  INST(Autia            , BaseRR             , (0b11011010110000010001000000000000, kX, kZR, 0, kX, kSP, 5, true)                    , kRWI_X    , 0                         , 2  , 104 ), // #17
  INST(Autia1716        , BaseOp             , (0b11010101000000110010000110011111)                                                  , 0         , 0                         , 0  , 110 ), // #18
  INST(Autiasp          , BaseOp             , (0b11010101000000110010001110111111)                                                  , 0         , 0                         , 1  , 120 ), // #19
  INST(Autiaz           , BaseOp             , (0b11010101000000110010001110011111)                                                  , 0         , 0                         , 2  , 128 ), // #20
  INST(Autib            , BaseRR             , (0b11011010110000010001010000000000, kX, kZR, 0, kX, kSP, 5, true)                    , kRWI_X    , 0                         , 3  , 135 ), // #21
  INST(Autib1716        , BaseOp             , (0b11010101000000110010000111011111)                                                  , 0         , 0                         , 3  , 141 ), // #22
  INST(Autibsp          , BaseOp             , (0b11010101000000110010001111111111)                                                  , 0         , 0                         , 4  , 151 ), // #23
  INST(Autibz           , BaseOp             , (0b11010101000000110010001111011111)                                                  , 0         , 0                         , 5  , 159 ), // #24
  INST(Autiza           , BaseR              , (0b11011010110000010011001111100000, kX, kZR, 0)                                      , kRWI_X    , 0                         , 2  , 166 ), // #25
  INST(Autizb           , BaseR              , (0b11011010110000010011011111100000, kX, kZR, 0)                                      , kRWI_X    , 0                         , 3  , 173 ), // #26
  INST(Axflag           , BaseOp             , (0b11010101000000000100000001011111)                                                  , 0         , 0                         , 6  , 180 ), // #27
  INST(B                , BaseBranchRel      , (0b00010100000000000000000000000000)                                                  , 0         , F(Cond)                   , 0  , 1738), // #28
  INST(Bfc              , BaseBfc            , (0b00110011000000000000001111100000)                                                  , kRWI_X    , 0                         , 0  , 192 ), // #29
  INST(Bfi              , BaseBfi            , (0b00110011000000000000000000000000)                                                  , kRWI_X    , 0                         , 0  , 223 ), // #30
  INST(Bfm              , BaseBfm            , (0b00110011000000000000000000000000)                                                  , kRWI_X    , 0                         , 0  , 2514), // #31
  INST(Bfxil            , BaseBfx            , (0b00110011000000000000000000000000)                                                  , kRWI_X    , 0                         , 0  , 250 ), // #32
  INST(Bic              , BaseLogical        , (0b0001010001, 0b00100100, 1)                                                         , kRWI_W    , 0                         , 2  , 256 ), // #33
  INST(Bics             , BaseLogical        , (0b1101010001, 0b11100100, 1)                                                         , kRWI_W    , 0                         , 3  , 260 ), // #34
  INST(Bl               , BaseBranchRel      , (0b10010100000000000000000000000000)                                                  , 0         , 0                         , 1  , 2831), // #35
  INST(Blr              , BaseBranchReg      , (0b11010110001111110000000000000000)                                                  , kRWI_R    , 0                         , 0  , 269 ), // #36
  INST(Br               , BaseBranchReg      , (0b11010110000111110000000000000000)                                                  , kRWI_R    , 0                         , 1  , 273 ), // #37
  INST(Brk              , BaseOpImm          , (0b11010100001000000000000000000000, 16, 5)                                           , 0         , 0                         , 0  , 276 ), // #38
  INST(Cas              , BaseAtomicOp       , (0b1000100010100000011111, kWX, 30, 0)                                                , kRWI_XRX  , 0                         , 0  , 284 ), // #39
  INST(Casa             , BaseAtomicOp       , (0b1000100011100000011111, kWX, 30, 1)                                                , kRWI_XRX  , 0                         , 1  , 288 ), // #40
  INST(Casab            , BaseAtomicOp       , (0b0000100011100000011111, kW , 0 , 1)                                                , kRWI_XRX  , 0                         , 2  , 293 ), // #41
  INST(Casah            , BaseAtomicOp       , (0b0100100011100000011111, kW , 0 , 1)                                                , kRWI_XRX  , 0                         , 3  , 299 ), // #42
  INST(Casal            , BaseAtomicOp       , (0b1000100011100000111111, kWX, 30, 1)                                                , kRWI_XRX  , 0                         , 4  , 305 ), // #43
  INST(Casalb           , BaseAtomicOp       , (0b0000100011100000111111, kW , 0 , 1)                                                , kRWI_XRX  , 0                         , 5  , 311 ), // #44
  INST(Casalh           , BaseAtomicOp       , (0b0100100011100000111111, kW , 0 , 1)                                                , kRWI_XRX  , 0                         , 6  , 318 ), // #45
  INST(Casb             , BaseAtomicOp       , (0b0000100010100000011111, kW , 0 , 0)                                                , kRWI_XRX  , 0                         , 7  , 325 ), // #46
  INST(Cash             , BaseAtomicOp       , (0b0100100010100000011111, kW , 0 , 0)                                                , kRWI_XRX  , 0                         , 8  , 330 ), // #47
  INST(Casl             , BaseAtomicOp       , (0b1000100010100000111111, kWX, 30, 0)                                                , kRWI_XRX  , 0                         , 9  , 335 ), // #48
  INST(Caslb            , BaseAtomicOp       , (0b0000100010100000111111, kW , 0 , 0)                                                , kRWI_XRX  , 0                         , 10 , 340 ), // #49
  INST(Caslh            , BaseAtomicOp       , (0b0100100010100000111111, kW , 0 , 0)                                                , kRWI_XRX  , 0                         , 11 , 346 ), // #50
  INST(Casp             , BaseAtomicCasp     , (0b0000100000100000011111, kWX, 30)                                                   , kRWI_XXRRX, 0                         , 0  , 352 ), // #51
  INST(Caspa            , BaseAtomicCasp     , (0b0000100001100000011111, kWX, 30)                                                   , kRWI_XXRRX, 0                         , 1  , 357 ), // #52
  INST(Caspal           , BaseAtomicCasp     , (0b0000100001100000111111, kWX, 30)                                                   , kRWI_XXRRX, 0                         , 2  , 363 ), // #53
  INST(Caspl            , BaseAtomicCasp     , (0b0000100000100000111111, kWX, 30)                                                   , kRWI_XXRRX, 0                         , 3  , 370 ), // #54
  INST(Cbnz             , BaseBranchCmp      , (0b00110101000000000000000000000000)                                                  , kRWI_R    , 0                         , 0  , 376 ), // #55
  INST(Cbz              , BaseBranchCmp      , (0b00110100000000000000000000000000)                                                  , kRWI_R    , 0                         , 1  , 381 ), // #56
  INST(Ccmn             , BaseCCmp           , (0b00111010010000000000000000000000)                                                  , kRWI_R    , 0                         , 0  , 385 ), // #57
  INST(Ccmp             , BaseCCmp           , (0b01111010010000000000000000000000)                                                  , kRWI_R    , 0                         , 1  , 650 ), // #58
  INST(Cfinv            , BaseOp             , (0b11010101000000000100000000011111)                                                  , 0         , 0                         , 7  , 390 ), // #59
  INST(Cinc             , BaseCInc           , (0b00011010100000000000010000000000)                                                  , kRWI_W    , 0                         , 0  , 396 ), // #60
  INST(Cinv             , BaseCInc           , (0b01011010100000000000000000000000)                                                  , kRWI_W    , 0                         , 1  , 401 ), // #61
  INST(Clrex            , BaseOpImm          , (0b11010101000000110011000001011111, 4, 8)                                            , 0         , 0                         , 1  , 406 ), // #62
  INST(Cls              , BaseRR             , (0b01011010110000000001010000000000, kWX, kZR, 0, kWX, kZR, 5, true)                  , kRWI_W    , 0                         , 4  , 412 ), // #63
  INST(Clz              , BaseRR             , (0b01011010110000000001000000000000, kWX, kZR, 0, kWX, kZR, 5, true)                  , kRWI_W    , 0                         , 5  , 416 ), // #64
  INST(Cmn              , BaseCmpCmn         , (0b0101011000, 0b0101011001, 0b0110001)                                               , kRWI_R    , 0                         , 0  , 386 ), // #65
  INST(Cmp              , BaseCmpCmn         , (0b1101011000, 0b1101011001, 0b1110001)                                               , kRWI_R    , 0                         , 1  , 651 ), // #66
  INST(Cmpp             , BaseRR             , (0b10111010110000000000000000011111, kX, kSP, 5, kX, kSP, 16, true)                   , kRWI_R    , 0                         , 6  , 430 ), // #67
  INST(Cneg             , BaseCInc           , (0b01011010100000000000010000000000)                                                  , kRWI_W    , 0                         , 2  , 441 ), // #68
  INST(Crc32b           , BaseRRR            , (0b0001101011000000010000, kW, kZR, kW, kZR, kW, kZR, false)                          , kRWI_W    , 0                         , 2  , 450 ), // #69
  INST(Crc32cb          , BaseRRR            , (0b0001101011000000010100, kW, kZR, kW, kZR, kW, kZR, false)                          , kRWI_W    , 0                         , 3  , 457 ), // #70
  INST(Crc32ch          , BaseRRR            , (0b0001101011000000010101, kW, kZR, kW, kZR, kW, kZR, false)                          , kRWI_W    , 0                         , 4  , 465 ), // #71
  INST(Crc32cw          , BaseRRR            , (0b0001101011000000010110, kW, kZR, kW, kZR, kW, kZR, false)                          , kRWI_W    , 0                         , 5  , 473 ), // #72
  INST(Crc32cx          , BaseRRR            , (0b1001101011000000010111, kW, kZR, kW, kZR, kX, kZR, false)                          , kRWI_W    , 0                         , 6  , 481 ), // #73
  INST(Crc32h           , BaseRRR            , (0b0001101011000000010001, kW, kZR, kW, kZR, kW, kZR, false)                          , kRWI_W    , 0                         , 7  , 489 ), // #74
  INST(Crc32w           , BaseRRR            , (0b0001101011000000010010, kW, kZR, kW, kZR, kW, kZR, false)                          , kRWI_W    , 0                         , 8  , 496 ), // #75
  INST(Crc32x           , BaseRRR            , (0b1001101011000000010011, kW, kZR, kW, kZR, kX, kZR, false)                          , kRWI_W    , 0                         , 9  , 503 ), // #76
  INST(Csdb             , BaseOp             , (0b11010101000000110010001010011111)                                                  , 0         , 0                         , 8  , 510 ), // #77
  INST(Csel             , BaseCSel           , (0b00011010100000000000000000000000)                                                  , kRWI_W    , 0                         , 0  , 710 ), // #78
  INST(Cset             , BaseCSet           , (0b00011010100111110000011111100000)                                                  , kRWI_W    , 0                         , 0  , 515 ), // #79
  INST(Csetm            , BaseCSet           , (0b01011010100111110000001111100000)                                                  , kRWI_W    , 0                         , 1  , 520 ), // #80
  INST(Csinc            , BaseCSel           , (0b00011010100000000000010000000000)                                                  , kRWI_W    , 0                         , 1  , 526 ), // #81
  INST(Csinv            , BaseCSel           , (0b01011010100000000000000000000000)                                                  , kRWI_W    , 0                         , 2  , 532 ), // #82
  INST(Csneg            , BaseCSel           , (0b01011010100000000000010000000000)                                                  , kRWI_W    , 0                         , 3  , 538 ), // #83
  INST(Dc               , BaseAtDcIcTlbi     , (0b00011110000000, 0b00001110000000, true)                                            , kRWI_RX   , 0                         , 1  , 2   ), // #84
  INST(Dcps1            , BaseOpImm          , (0b11010100101000000000000000000001, 16, 5)                                           , 0         , 0                         , 2  , 544 ), // #85
  INST(Dcps2            , BaseOpImm          , (0b11010100101000000000000000000010, 16, 5)                                           , 0         , 0                         , 3  , 550 ), // #86
  INST(Dcps3            , BaseOpImm          , (0b11010100101000000000000000000011, 16, 5)                                           , 0         , 0                         , 4  , 556 ), // #87
  INST(Dgh              , BaseOp             , (0b11010101000000110010000011011111)                                                  , 0         , 0                         , 9  , 562 ), // #88
  INST(Dmb              , BaseOpImm          , (0b11010101000000110011000010111111, 4, 8)                                            , 0         , 0                         , 5  , 566 ), // #89
  INST(Drps             , BaseOp             , (0b11010110101111110000001111100000)                                                  , 0         , 0                         , 10 , 570 ), // #90
  INST(Dsb              , BaseOpImm          , (0b11010101000000110011000010011111, 4, 8)                                            , 0         , 0                         , 6  , 575 ), // #91
  INST(Eon              , BaseLogical        , (0b1001010001, 0b10100100, 1)                                                         , kRWI_W    , 0                         , 4  , 583 ), // #92
  INST(Eor              , BaseLogical        , (0b1001010000, 0b10100100, 0)                                                         , kRWI_W    , 0                         , 5  , 1418), // #93
  INST(Esb              , BaseOp             , (0b11010101000000110010001000011111)                                                  , 0         , 0                         , 11 , 597 ), // #94
  INST(Extr             , BaseExtract        , (0b00010011100000000000000000000000)                                                  , kRWI_W    , 0                         , 0  , 605 ), // #95
  INST(Eret             , BaseOp             , (0b11010110100111110000001111100000)                                                  , 0         , 0                         , 12 , 592 ), // #96
  INST(Gmi              , BaseRRR            , (0b1001101011000000000101, kX , kZR, kX , kSP, kX , kZR, true)                        , kRWI_W    , 0                         , 10 , 1128), // #97
  INST(Hint             , BaseOpImm          , (0b11010101000000110010000000011111, 7, 5)                                            , 0         , 0                         , 7  , 1132), // #98
  INST(Hlt              , BaseOpImm          , (0b11010100010000000000000000000000, 16, 5)                                           , 0         , 0                         , 8  , 1137), // #99
  INST(Hvc              , BaseOpImm          , (0b11010100000000000000000000000010, 16, 5)                                           , 0         , 0                         , 9  , 1141), // #100
  INST(Ic               , BaseAtDcIcTlbi     , (0b00011110000000, 0b00001110000000, false)                                           , kRWI_RX   , 0                         , 2  , 257 ), // #101
  INST(Isb              , BaseOpImm          , (0b11010101000000110011000011011111, 4, 8)                                            , 0         , 0                         , 10 , 1149), // #102
  INST(Ldadd            , BaseAtomicOp       , (0b1011100000100000000000, kWX, 30, 0)                                                , kRWI_WRX  , 0                         , 12 , 1189), // #103
  INST(Ldadda           , BaseAtomicOp       , (0b1011100010100000000000, kWX, 30, 1)                                                , kRWI_WRX  , 0                         , 13 , 1195), // #104
  INST(Ldaddab          , BaseAtomicOp       , (0b0011100010100000000000, kW , 0 , 1)                                                , kRWI_WRX  , 0                         , 14 , 1202), // #105
  INST(Ldaddah          , BaseAtomicOp       , (0b0111100010100000000000, kW , 0 , 1)                                                , kRWI_WRX  , 0                         , 15 , 1210), // #106
  INST(Ldaddal          , BaseAtomicOp       , (0b1011100011100000000000, kWX, 30, 1)                                                , kRWI_WRX  , 0                         , 16 , 1218), // #107
  INST(Ldaddalb         , BaseAtomicOp       , (0b0011100011100000000000, kW , 0 , 1)                                                , kRWI_WRX  , 0                         , 17 , 1226), // #108
  INST(Ldaddalh         , BaseAtomicOp       , (0b0111100011100000000000, kW , 0 , 1)                                                , kRWI_WRX  , 0                         , 18 , 1235), // #109
  INST(Ldaddb           , BaseAtomicOp       , (0b0011100000100000000000, kW , 0 , 0)                                                , kRWI_WRX  , 0                         , 19 , 1244), // #110
  INST(Ldaddh           , BaseAtomicOp       , (0b0111100000100000000000, kW , 0 , 0)                                                , kRWI_WRX  , 0                         , 20 , 1251), // #111
  INST(Ldaddl           , BaseAtomicOp       , (0b1011100001100000000000, kWX, 30, 0)                                                , kRWI_WRX  , 0                         , 21 , 1258), // #112
  INST(Ldaddlb          , BaseAtomicOp       , (0b0011100001100000000000, kW , 0 , 0)                                                , kRWI_WRX  , 0                         , 22 , 1265), // #113
  INST(Ldaddlh          , BaseAtomicOp       , (0b0111100001100000000000, kW , 0 , 0)                                                , kRWI_WRX  , 0                         , 23 , 1273), // #114
  INST(Ldar             , BaseRM_NoImm       , (0b1000100011011111111111, kWX, kZR, 30)                                              , kRWI_W    , 0                         , 0  , 1281), // #115
  INST(Ldarb            , BaseRM_NoImm       , (0b0000100011011111111111, kW , kZR, 0 )                                              , kRWI_W    , 0                         , 1  , 1286), // #116
  INST(Ldarh            , BaseRM_NoImm       , (0b0100100011011111111111, kW , kZR, 0 )                                              , kRWI_W    , 0                         , 2  , 1292), // #117
  INST(Ldaxp            , BaseLdxp           , (0b1000100001111111100000, kWX, 30)                                                   , kRWI_WW   , 0                         , 0  , 1298), // #118
  INST(Ldaxr            , BaseRM_NoImm       , (0b1000100001011111111111, kWX, kZR, 30)                                              , kRWI_W    , 0                         , 3  , 1304), // #119
  INST(Ldaxrb           , BaseRM_NoImm       , (0b0000100001011111111111, kW , kZR, 0 )                                              , kRWI_W    , 0                         , 4  , 1310), // #120
  INST(Ldaxrh           , BaseRM_NoImm       , (0b0100100001011111111111, kW , kZR, 0 )                                              , kRWI_W    , 0                         , 5  , 1317), // #121
  INST(Ldclr            , BaseAtomicOp       , (0b1011100000100000000100, kWX, 30, 0)                                                , kRWI_WRX  , 0                         , 24 , 1324), // #122
  INST(Ldclra           , BaseAtomicOp       , (0b1011100010100000000100, kWX, 30, 1)                                                , kRWI_WRX  , 0                         , 25 , 1330), // #123
  INST(Ldclrab          , BaseAtomicOp       , (0b0011100010100000000100, kW , 0 , 1)                                                , kRWI_WRX  , 0                         , 26 , 1337), // #124
  INST(Ldclrah          , BaseAtomicOp       , (0b0111100010100000000100, kW , 0 , 1)                                                , kRWI_WRX  , 0                         , 27 , 1345), // #125
  INST(Ldclral          , BaseAtomicOp       , (0b1011100011100000000100, kWX, 30, 1)                                                , kRWI_WRX  , 0                         , 28 , 1353), // #126
  INST(Ldclralb         , BaseAtomicOp       , (0b0011100011100000000100, kW , 0 , 1)                                                , kRWI_WRX  , 0                         , 29 , 1361), // #127
  INST(Ldclralh         , BaseAtomicOp       , (0b0111100011100000000100, kW , 0 , 1)                                                , kRWI_WRX  , 0                         , 30 , 1370), // #128
  INST(Ldclrb           , BaseAtomicOp       , (0b0011100000100000000100, kW , 0 , 0)                                                , kRWI_WRX  , 0                         , 31 , 1379), // #129
  INST(Ldclrh           , BaseAtomicOp       , (0b0111100000100000000100, kW , 0 , 0)                                                , kRWI_WRX  , 0                         , 32 , 1386), // #130
  INST(Ldclrl           , BaseAtomicOp       , (0b1011100001100000000100, kWX, 30, 0)                                                , kRWI_WRX  , 0                         , 33 , 1393), // #131
  INST(Ldclrlb          , BaseAtomicOp       , (0b0011100001100000000100, kW , 0 , 0)                                                , kRWI_WRX  , 0                         , 34 , 1400), // #132
  INST(Ldclrlh          , BaseAtomicOp       , (0b0111100001100000000100, kW , 0 , 0)                                                , kRWI_WRX  , 0                         , 35 , 1408), // #133
  INST(Ldeor            , BaseAtomicOp       , (0b1011100000100000001000, kWX, 30, 0)                                                , kRWI_WRX  , 0                         , 36 , 1416), // #134
  INST(Ldeora           , BaseAtomicOp       , (0b1011100010100000001000, kWX, 30, 1)                                                , kRWI_WRX  , 0                         , 37 , 1422), // #135
  INST(Ldeorab          , BaseAtomicOp       , (0b0011100010100000001000, kW , 0 , 1)                                                , kRWI_WRX  , 0                         , 38 , 1429), // #136
  INST(Ldeorah          , BaseAtomicOp       , (0b0111100010100000001000, kW , 0 , 1)                                                , kRWI_WRX  , 0                         , 39 , 1437), // #137
  INST(Ldeoral          , BaseAtomicOp       , (0b1011100011100000001000, kWX, 30, 1)                                                , kRWI_WRX  , 0                         , 40 , 1445), // #138
  INST(Ldeoralb         , BaseAtomicOp       , (0b0011100011100000001000, kW , 0 , 1)                                                , kRWI_WRX  , 0                         , 41 , 1453), // #139
  INST(Ldeoralh         , BaseAtomicOp       , (0b0111100011100000001000, kW , 0 , 1)                                                , kRWI_WRX  , 0                         , 42 , 1462), // #140
  INST(Ldeorb           , BaseAtomicOp       , (0b0011100000100000001000, kW , 0 , 0)                                                , kRWI_WRX  , 0                         , 43 , 1471), // #141
  INST(Ldeorh           , BaseAtomicOp       , (0b0111100000100000001000, kW , 0 , 0)                                                , kRWI_WRX  , 0                         , 44 , 1478), // #142
  INST(Ldeorl           , BaseAtomicOp       , (0b1011100001100000001000, kWX, 30, 0)                                                , kRWI_WRX  , 0                         , 45 , 1485), // #143
  INST(Ldeorlb          , BaseAtomicOp       , (0b0011100001100000001000, kW , 0 , 0)                                                , kRWI_WRX  , 0                         , 46 , 1492), // #144
  INST(Ldeorlh          , BaseAtomicOp       , (0b0111100001100000001000, kW , 0 , 0)                                                , kRWI_WRX  , 0                         , 47 , 1500), // #145
  INST(Ldg              , BaseRM_SImm9       , (0b1101100101100000000000, 0b0000000000000000000000, kX , kZR, 0, 4)                  , kRWI_W    , 0                         , 0  , 1508), // #146
  INST(Ldgm             , BaseRM_NoImm       , (0b1101100111100000000000, kX , kZR, 0 )                                              , kRWI_W    , 0                         , 6  , 1512), // #147
  INST(Ldlar            , BaseRM_NoImm       , (0b1000100011011111011111, kWX, kZR, 30)                                              , kRWI_W    , 0                         , 7  , 1517), // #148
  INST(Ldlarb           , BaseRM_NoImm       , (0b0000100011011111011111, kW , kZR, 0 )                                              , kRWI_W    , 0                         , 8  , 1523), // #149
  INST(Ldlarh           , BaseRM_NoImm       , (0b0100100011011111011111, kW , kZR, 0 )                                              , kRWI_W    , 0                         , 9  , 1530), // #150
  INST(Ldnp             , BaseLdpStp         , (0b0010100001, 0           , kWX, 31, 2)                                              , kRWI_WW   , 0                         , 0  , 1537), // #151
  INST(Ldp              , BaseLdpStp         , (0b0010100101, 0b0010100011, kWX, 31, 2)                                              , kRWI_W    , 0                         , 1  , 1542), // #152
  INST(Ldpsw            , BaseLdpStp         , (0b0110100101, 0b0110100011, kX , 0 , 2)                                              , kRWI_WW   , 0                         , 2  , 1546), // #153
  INST(Ldr              , BaseLdSt           , (0b1011100101, 0b10111000010, 0b10111000011, 0b00011000, kWX, 30, 2, Inst::kIdLdur)   , kRWI_W    , 0                         , 0  , 1552), // #154
  INST(Ldraa            , BaseRM_SImm10      , (0b1111100000100000000001, kX , kZR, 0, 3)                                            , kRWI_W    , 0                         , 0  , 1556), // #155
  INST(Ldrab            , BaseRM_SImm10      , (0b1111100010100000000001, kX , kZR, 0, 3)                                            , kRWI_W    , 0                         , 1  , 1562), // #156
  INST(Ldrb             , BaseLdSt           , (0b0011100101, 0b00111000010, 0b00111000011, 0         , kW , 0 , 0, Inst::kIdLdurb)  , kRWI_W    , 0                         , 1  , 1568), // #157
  INST(Ldrh             , BaseLdSt           , (0b0111100101, 0b01111000010, 0b01111000011, 0         , kW , 0 , 1, Inst::kIdLdurh)  , kRWI_W    , 0                         , 2  , 1573), // #158
  INST(Ldrsb            , BaseLdSt           , (0b0011100111, 0b00111000100, 0b00111000101, 0         , kWX, 22, 0, Inst::kIdLdursb) , kRWI_W    , 0                         , 3  , 1578), // #159
  INST(Ldrsh            , BaseLdSt           , (0b0111100110, 0b01111000100, 0b01111000101, 0         , kWX, 22, 1, Inst::kIdLdursh) , kRWI_W    , 0                         , 4  , 1584), // #160
  INST(Ldrsw            , BaseLdSt           , (0b1011100110, 0b10111000100, 0b10111000101, 0b10011000, kX , 0 , 2, Inst::kIdLdursw) , kRWI_W    , 0                         , 5  , 1590), // #161
  INST(Ldset            , BaseAtomicOp       , (0b1011100000100000001100, kWX, 30, 0)                                                , kRWI_WRX  , 0                         , 48 , 1596), // #162
  INST(Ldseta           , BaseAtomicOp       , (0b1011100010100000001100, kWX, 30, 1)                                                , kRWI_WRX  , 0                         , 49 , 1602), // #163
  INST(Ldsetab          , BaseAtomicOp       , (0b0011100010100000001100, kW , 0 , 1)                                                , kRWI_WRX  , 0                         , 50 , 1609), // #164
  INST(Ldsetah          , BaseAtomicOp       , (0b0111100010100000001100, kW , 0 , 1)                                                , kRWI_WRX  , 0                         , 51 , 1617), // #165
  INST(Ldsetal          , BaseAtomicOp       , (0b1011100011100000001100, kWX, 30, 1)                                                , kRWI_WRX  , 0                         , 52 , 1625), // #166
  INST(Ldsetalb         , BaseAtomicOp       , (0b0011100011100000001100, kW , 0 , 1)                                                , kRWI_WRX  , 0                         , 53 , 1633), // #167
  INST(Ldsetalh         , BaseAtomicOp       , (0b0111100011100000001100, kW , 0 , 1)                                                , kRWI_WRX  , 0                         , 54 , 1642), // #168
  INST(Ldsetb           , BaseAtomicOp       , (0b0011100000100000001100, kW , 0 , 0)                                                , kRWI_WRX  , 0                         , 55 , 1651), // #169
  INST(Ldseth           , BaseAtomicOp       , (0b0111100000100000001100, kW , 0 , 0)                                                , kRWI_WRX  , 0                         , 56 , 1658), // #170
  INST(Ldsetl           , BaseAtomicOp       , (0b1011100001100000001100, kWX, 30, 0)                                                , kRWI_WRX  , 0                         , 57 , 1665), // #171
  INST(Ldsetlb          , BaseAtomicOp       , (0b0011100001100000001100, kW , 0 , 0)                                                , kRWI_WRX  , 0                         , 58 , 1672), // #172
  INST(Ldsetlh          , BaseAtomicOp       , (0b0111100001100000001100, kW , 0 , 0)                                                , kRWI_WRX  , 0                         , 59 , 1680), // #173
  INST(Ldsmax           , BaseAtomicOp       , (0b1011100000100000010000, kWX, 30, 0)                                                , kRWI_WRX  , 0                         , 60 , 1688), // #174
  INST(Ldsmaxa          , BaseAtomicOp       , (0b1011100010100000010000, kWX, 30, 1)                                                , kRWI_WRX  , 0                         , 61 , 1695), // #175
  INST(Ldsmaxab         , BaseAtomicOp       , (0b0011100010100000010000, kW , 0 , 1)                                                , kRWI_WRX  , 0                         , 62 , 1703), // #176
  INST(Ldsmaxah         , BaseAtomicOp       , (0b0111100010100000010000, kW , 0 , 1)                                                , kRWI_WRX  , 0                         , 63 , 1712), // #177
  INST(Ldsmaxal         , BaseAtomicOp       , (0b1011100011100000010000, kWX, 30, 1)                                                , kRWI_WRX  , 0                         , 64 , 1721), // #178
  INST(Ldsmaxalb        , BaseAtomicOp       , (0b0011100011100000010000, kW , 0 , 1)                                                , kRWI_WRX  , 0                         , 65 , 1730), // #179
  INST(Ldsmaxalh        , BaseAtomicOp       , (0b0111100011100000010000, kW , 0 , 1)                                                , kRWI_WRX  , 0                         , 66 , 1740), // #180
  INST(Ldsmaxb          , BaseAtomicOp       , (0b0011100000100000010000, kW , 0 , 0)                                                , kRWI_WRX  , 0                         , 67 , 1750), // #181
  INST(Ldsmaxh          , BaseAtomicOp       , (0b0111100000100000010000, kW , 0 , 0)                                                , kRWI_WRX  , 0                         , 68 , 1758), // #182
  INST(Ldsmaxl          , BaseAtomicOp       , (0b1011100001100000010000, kWX, 30, 0)                                                , kRWI_WRX  , 0                         , 69 , 1766), // #183
  INST(Ldsmaxlb         , BaseAtomicOp       , (0b0011100001100000010000, kW , 0 , 0)                                                , kRWI_WRX  , 0                         , 70 , 1774), // #184
  INST(Ldsmaxlh         , BaseAtomicOp       , (0b0111100001100000010000, kW , 0 , 0)                                                , kRWI_WRX  , 0                         , 71 , 1783), // #185
  INST(Ldsmin           , BaseAtomicOp       , (0b1011100000100000010100, kWX, 30, 0)                                                , kRWI_WRX  , 0                         , 72 , 1792), // #186
  INST(Ldsmina          , BaseAtomicOp       , (0b1011100010100000010100, kWX, 30, 1)                                                , kRWI_WRX  , 0                         , 73 , 1799), // #187
  INST(Ldsminab         , BaseAtomicOp       , (0b0011100010100000010100, kW , 0 , 1)                                                , kRWI_WRX  , 0                         , 74 , 1807), // #188
  INST(Ldsminah         , BaseAtomicOp       , (0b0111100010100000010100, kW , 0 , 1)                                                , kRWI_WRX  , 0                         , 75 , 1816), // #189
  INST(Ldsminal         , BaseAtomicOp       , (0b1011100011100000010100, kWX, 30, 1)                                                , kRWI_WRX  , 0                         , 76 , 1825), // #190
  INST(Ldsminalb        , BaseAtomicOp       , (0b0011100011100000010100, kW , 0 , 1)                                                , kRWI_WRX  , 0                         , 77 , 1834), // #191
  INST(Ldsminalh        , BaseAtomicOp       , (0b0111100011100000010100, kW , 0 , 1)                                                , kRWI_WRX  , 0                         , 78 , 1844), // #192
  INST(Ldsminb          , BaseAtomicOp       , (0b0011100000100000010100, kW , 0 , 0)                                                , kRWI_WRX  , 0                         , 79 , 1854), // #193
  INST(Ldsminh          , BaseAtomicOp       , (0b0111100000100000010100, kW , 0 , 0)                                                , kRWI_WRX  , 0                         , 80 , 1862), // #194
  INST(Ldsminl          , BaseAtomicOp       , (0b1011100001100000010100, kWX, 30, 0)                                                , kRWI_WRX  , 0                         , 81 , 1870), // #195
  INST(Ldsminlb         , BaseAtomicOp       , (0b0011100001100000010100, kW , 0 , 0)                                                , kRWI_WRX  , 0                         , 82 , 1878), // #196
  INST(Ldsminlh         , BaseAtomicOp       , (0b0111100001100000010100, kW , 0 , 0)                                                , kRWI_WRX  , 0                         , 83 , 1887), // #197
  INST(Ldtr             , BaseRM_SImm9       , (0b1011100001000000000010, 0b0000000000000000000000, kWX, kZR, 30, 0)                 , kRWI_W    , 0                         , 1  , 1896), // #198
  INST(Ldtrb            , BaseRM_SImm9       , (0b0011100001000000000010, 0b0000000000000000000000, kW , kZR, 0 , 0)                 , kRWI_W    , 0                         , 2  , 1901), // #199
  INST(Ldtrh            , BaseRM_SImm9       , (0b0111100001000000000010, 0b0000000000000000000000, kW , kZR, 0 , 0)                 , kRWI_W    , 0                         , 3  , 1907), // #200
  INST(Ldtrsb           , BaseRM_SImm9       , (0b0011100011000000000010, 0b0000000000000000000000, kWX, kZR, 22, 0)                 , kRWI_W    , 0                         , 4  , 1913), // #201
  INST(Ldtrsh           , BaseRM_SImm9       , (0b0111100011000000000010, 0b0000000000000000000000, kWX, kZR, 22, 0)                 , kRWI_W    , 0                         , 5  , 1920), // #202
  INST(Ldtrsw           , BaseRM_SImm9       , (0b1011100010000000000010, 0b0000000000000000000000, kX , kZR, 0 , 0)                 , kRWI_W    , 0                         , 6  , 1927), // #203
  INST(Ldumax           , BaseAtomicOp       , (0b1011100000100000011000, kWX, 30, 0)                                                , kRWI_WRX  , 0                         , 84 , 1934), // #204
  INST(Ldumaxa          , BaseAtomicOp       , (0b1011100010100000011000, kWX, 30, 1)                                                , kRWI_WRX  , 0                         , 85 , 1941), // #205
  INST(Ldumaxab         , BaseAtomicOp       , (0b0011100010100000011000, kW , 0 , 1)                                                , kRWI_WRX  , 0                         , 86 , 1949), // #206
  INST(Ldumaxah         , BaseAtomicOp       , (0b0111100010100000011000, kW , 0 , 1)                                                , kRWI_WRX  , 0                         , 87 , 1958), // #207
  INST(Ldumaxal         , BaseAtomicOp       , (0b1011100011100000011000, kWX, 30, 1)                                                , kRWI_WRX  , 0                         , 88 , 1967), // #208
  INST(Ldumaxalb        , BaseAtomicOp       , (0b0011100011100000011000, kW , 0 , 1)                                                , kRWI_WRX  , 0                         , 89 , 1976), // #209
  INST(Ldumaxalh        , BaseAtomicOp       , (0b0111100011100000011000, kW , 0 , 1)                                                , kRWI_WRX  , 0                         , 90 , 1986), // #210
  INST(Ldumaxb          , BaseAtomicOp       , (0b0011100000100000011000, kW , 0 , 0)                                                , kRWI_WRX  , 0                         , 91 , 1996), // #211
  INST(Ldumaxh          , BaseAtomicOp       , (0b0111100000100000011000, kW , 0 , 0)                                                , kRWI_WRX  , 0                         , 92 , 2004), // #212
  INST(Ldumaxl          , BaseAtomicOp       , (0b1011100001100000011000, kWX, 30, 0)                                                , kRWI_WRX  , 0                         , 93 , 2012), // #213
  INST(Ldumaxlb         , BaseAtomicOp       , (0b0011100001100000011000, kW , 0 , 0)                                                , kRWI_WRX  , 0                         , 94 , 2020), // #214
  INST(Ldumaxlh         , BaseAtomicOp       , (0b0111100001100000011000, kW , 0 , 0)                                                , kRWI_WRX  , 0                         , 95 , 2029), // #215
  INST(Ldumin           , BaseAtomicOp       , (0b1011100000100000011100, kWX, 30, 0)                                                , kRWI_WRX  , 0                         , 96 , 2038), // #216
  INST(Ldumina          , BaseAtomicOp       , (0b1011100010100000011100, kWX, 30, 1)                                                , kRWI_WRX  , 0                         , 97 , 2045), // #217
  INST(Lduminab         , BaseAtomicOp       , (0b0011100010100000011100, kW , 0 , 1)                                                , kRWI_WRX  , 0                         , 98 , 2053), // #218
  INST(Lduminah         , BaseAtomicOp       , (0b0111100010100000011100, kW , 0 , 1)                                                , kRWI_WRX  , 0                         , 99 , 2062), // #219
  INST(Lduminal         , BaseAtomicOp       , (0b1011100011100000011100, kWX, 30, 1)                                                , kRWI_WRX  , 0                         , 100, 2071), // #220
  INST(Lduminalb        , BaseAtomicOp       , (0b0011100011100000011100, kW , 0 , 1)                                                , kRWI_WRX  , 0                         , 101, 2080), // #221
  INST(Lduminalh        , BaseAtomicOp       , (0b0111100011100000011100, kW , 0 , 1)                                                , kRWI_WRX  , 0                         , 102, 2090), // #222
  INST(Lduminb          , BaseAtomicOp       , (0b0011100000100000011100, kW , 0 , 0)                                                , kRWI_WRX  , 0                         , 103, 2100), // #223
  INST(Lduminh          , BaseAtomicOp       , (0b0111100000100000011100, kW , 0 , 0)                                                , kRWI_WRX  , 0                         , 104, 2108), // #224
  INST(Lduminl          , BaseAtomicOp       , (0b1011100001100000011100, kWX, 30, 0)                                                , kRWI_WRX  , 0                         , 105, 2116), // #225
  INST(Lduminlb         , BaseAtomicOp       , (0b0011100001100000011100, kW , 0 , 0)                                                , kRWI_WRX  , 0                         , 106, 2124), // #226
  INST(Lduminlh         , BaseAtomicOp       , (0b0111100001100000011100, kW , 0 , 0)                                                , kRWI_WRX  , 0                         , 107, 2133), // #227
  INST(Ldur             , BaseRM_SImm9       , (0b1011100001000000000000, 0b0000000000000000000000, kWX, kZR, 30, 0)                 , kRWI_W    , 0                         , 7  , 2142), // #228
  INST(Ldurb            , BaseRM_SImm9       , (0b0011100001000000000000, 0b0000000000000000000000, kW , kZR, 0 , 0)                 , kRWI_W    , 0                         , 8  , 2147), // #229
  INST(Ldurh            , BaseRM_SImm9       , (0b0111100001000000000000, 0b0000000000000000000000, kW , kZR, 0 , 0)                 , kRWI_W    , 0                         , 9  , 2153), // #230
  INST(Ldursb           , BaseRM_SImm9       , (0b0011100011000000000000, 0b0000000000000000000000, kWX, kZR, 22, 0)                 , kRWI_W    , 0                         , 10 , 2159), // #231
  INST(Ldursh           , BaseRM_SImm9       , (0b0111100011000000000000, 0b0000000000000000000000, kWX, kZR, 22, 0)                 , kRWI_W    , 0                         , 11 , 2166), // #232
  INST(Ldursw           , BaseRM_SImm9       , (0b1011100010000000000000, 0b0000000000000000000000, kWX, kZR, 0 , 0)                 , kRWI_W    , 0                         , 12 , 2173), // #233
  INST(Ldxp             , BaseLdxp           , (0b1000100001111111000000, kWX, 30)                                                   , kRWI_WW   , 0                         , 1  , 2180), // #234
  INST(Ldxr             , BaseRM_NoImm       , (0b1000100001011111011111, kWX, kZR, 30)                                              , kRWI_W    , 0                         , 10 , 2185), // #235
  INST(Ldxrb            , BaseRM_NoImm       , (0b0000100001011111011111, kW , kZR, 0 )                                              , kRWI_W    , 0                         , 11 , 2190), // #236
  INST(Ldxrh            , BaseRM_NoImm       , (0b0100100001011111011111, kW , kZR, 0 )                                              , kRWI_W    , 0                         , 12 , 2196), // #237
  INST(Lsl              , BaseShift          , (0b0001101011000000001000, 0b0101001100000000000000, 0)                               , kRWI_W    , 0                         , 2  , 2880), // #238
  INST(Lslv             , BaseShift          , (0b0001101011000000001000, 0b0000000000000000000000, 0)                               , kRWI_W    , 0                         , 3  , 2202), // #239
  INST(Lsr              , BaseShift          , (0b0001101011000000001001, 0b0101001100000000011111, 0)                               , kRWI_W    , 0                         , 4  , 2207), // #240
  INST(Lsrv             , BaseShift          , (0b0001101011000000001001, 0b0000000000000000000000, 0)                               , kRWI_W    , 0                         , 5  , 2211), // #241
  INST(Madd             , BaseRRRR           , (0b0001101100000000000000, kWX, kZR, kWX, kZR, kWX, kZR, kWX, kZR, true)              , kRWI_W    , 0                         , 0  , 977 ), // #242
  INST(Mneg             , BaseRRR            , (0b0001101100000000111111, kWX, kZR, kWX, kZR, kWX, kZR, true)                        , kRWI_W    , 0                         , 11 , 2216), // #243
  INST(Mov              , BaseMov            , (_)                                                                                   , kRWI_W    , 0                         , 0  , 949 ), // #244
  INST(Movk             , BaseMovKNZ         , (0b01110010100000000000000000000000)                                                  , kRWI_X    , 0                         , 0  , 2226), // #245
  INST(Movn             , BaseMovKNZ         , (0b00010010100000000000000000000000)                                                  , kRWI_W    , 0                         , 1  , 2231), // #246
  INST(Movz             , BaseMovKNZ         , (0b01010010100000000000000000000000)                                                  , kRWI_W    , 0                         , 2  , 2236), // #247
  INST(Mrs              , BaseMrs            , (_)                                                                                   , kRWI_W    , 0                         , 0  , 2241), // #248
  INST(Msr              , BaseMsr            , (_)                                                                                   , kRWI_W    , 0                         , 0  , 2245), // #249
  INST(Msub             , BaseRRRR           , (0b0001101100000000100000, kWX, kZR, kWX, kZR, kWX, kZR, kWX, kZR, true)              , kRWI_W    , 0                         , 1  , 984 ), // #250
  INST(Mul              , BaseRRR            , (0b0001101100000000011111, kWX, kZR, kWX, kZR, kWX, kZR, true)                        , kRWI_W    , 0                         , 12 , 991 ), // #251
  INST(Mvn              , BaseMvnNeg         , (0b00101010001000000000001111100000)                                                  , kRWI_W    , 0                         , 0  , 2249), // #252
  INST(Neg              , BaseMvnNeg         , (0b01001011000000000000001111100000)                                                  , kRWI_W    , 0                         , 1  , 540 ), // #253
  INST(Negs             , BaseMvnNeg         , (0b01101011000000000000001111100000)                                                  , kRWI_W    , 0                         , 2  , 2258), // #254
  INST(Ngc              , BaseRR             , (0b01011010000000000000001111100000, kWX, kZR, 0, kWX, kZR, 16, true)                 , kRWI_W    , 0                         , 7  , 2263), // #255
  INST(Ngcs             , BaseRR             , (0b01111010000000000000001111100000, kWX, kZR, 0, kWX, kZR, 16, true)                 , kRWI_W    , 0                         , 8  , 2267), // #256
  INST(Nop              , BaseOp             , (0b11010101000000110010000000011111)                                                  , 0         , 0                         , 13 , 2272), // #257
  INST(Orn              , BaseLogical        , (0b0101010001, 0b01100100, 1)                                                         , kRWI_W    , 0                         , 6  , 2280), // #258
  INST(Orr              , BaseLogical        , (0b0101010000, 0b01100100, 0)                                                         , kRWI_W    , 0                         , 7  , 2284), // #259
  INST(Pacda            , BaseRR             , (0b11011010110000010000100000000000, kX, kZR, 0, kX, kSP, 5, true)                    , kRWI_X    , 0                         , 9  , 2288), // #260
  INST(Pacdb            , BaseRR             , (0b11011010110000010000110000000000, kX, kZR, 0, kX, kSP, 5, true)                    , kRWI_X    , 0                         , 10 , 2294), // #261
  INST(Pacdza           , BaseR              , (0b11011010110000010010101111100000, kX, kZR, 0)                                      , kRWI_X    , 0                         , 4  , 2300), // #262
  INST(Pacdzb           , BaseR              , (0b11011010110000010010111111100000, kX, kZR, 0)                                      , kRWI_X    , 0                         , 5  , 2307), // #263
  INST(Pacga            , BaseRRR            , (0b1001101011000000001100, kX, kZR, kX, kZR, kX, kSP, false)                          , kRWI_W    , 0                         , 13 , 2314), // #264
  INST(Pssbb            , BaseOp             , (0b11010101000000110011010010011111)                                                  , 0         , 0                         , 14 , 2338), // #265
  INST(Rbit             , BaseRR             , (0b01011010110000000000000000000000, kWX, kZR, 0, kWX, kZR, 5, true)                  , kRWI_W    , 0                         , 11 , 2364), // #266
  INST(Ret              , BaseBranchReg      , (0b11010110010111110000000000000000)                                                  , kRWI_R    , 0                         , 2  , 593 ), // #267
  INST(Rev              , BaseRev            , (_)                                                                                   , kRWI_W    , 0                         , 0  , 2369), // #268
  INST(Rev16            , BaseRR             , (0b01011010110000000000010000000000, kWX, kZR, 0, kWX, kZR, 5, true)                  , kRWI_W    , 0                         , 12 , 2373), // #269
  INST(Rev32            , BaseRR             , (0b11011010110000000000100000000000, kWX, kZR, 0, kWX, kZR, 5, true)                  , kRWI_W    , 0                         , 13 , 2379), // #270
  INST(Rev64            , BaseRR             , (0b11011010110000000000110000000000, kWX, kZR, 0, kWX, kZR, 5, true)                  , kRWI_W    , 0                         , 14 , 2385), // #271
  INST(Ror              , BaseShift          , (0b0001101011000000001011, 0b0001001110000000000000, 1)                               , kRWI_W    , 0                         , 6  , 2391), // #272
  INST(Rorv             , BaseShift          , (0b0001101011000000001011, 0b0000000000000000000000, 1)                               , kRWI_W    , 0                         , 7  , 2395), // #273
  INST(Sbc              , BaseRRR            , (0b0101101000000000000000, kWX, kZR, kWX, kZR, kWX, kZR, true)                        , kRWI_W    , 0                         , 14 , 2498), // #274
  INST(Sbcs             , BaseRRR            , (0b0111101000000000000000, kWX, kZR, kWX, kZR, kWX, kZR, true)                        , kRWI_W    , 0                         , 15 , 2502), // #275
  INST(Sbfiz            , BaseBfi            , (0b00010011000000000000000000000000)                                                  , kRWI_W    , 0                         , 1  , 2507), // #276
  INST(Sbfm             , BaseBfm            , (0b00010011000000000000000000000000)                                                  , kRWI_W    , 0                         , 1  , 2513), // #277
  INST(Sbfx             , BaseBfx            , (0b00010011000000000000000000000000)                                                  , kRWI_W    , 0                         , 1  , 2518), // #278
  INST(Sdiv             , BaseRRR            , (0b0001101011000000000011, kWX, kZR, kWX, kZR, kWX, kZR, true)                        , kRWI_W    , 0                         , 16 , 2529), // #279
  INST(Setf8            , BaseR              , (0b00111010000000000000100000001101, kW, kZR, 5)                                      , 0         , 0                         , 6  , 2541), // #280
  INST(Setf16           , BaseR              , (0b00111010000000000100100000001101, kW, kZR, 5)                                      , 0         , 0                         , 7  , 2534), // #281
  INST(Sev              , BaseOp             , (0b11010101000000110010000010011111)                                                  , 0         , 0                         , 15 , 2547), // #282
  INST(Sevl             , BaseOp             , (0b11010101000000110010000010111111)                                                  , 0         , 0                         , 16 , 2551), // #283
  INST(Smaddl           , BaseRRRR           , (0b1001101100100000000000, kX , kZR, kW , kZR, kW , kZR, kX , kZR, false)             , kRWI_W    , 0                         , 2  , 2758), // #284
  INST(Smc              , BaseOpImm          , (0b11010100000000000000000000000011, 16, 5)                                           , 0         , 0                         , 11 , 53  ), // #285
  INST(Smnegl           , BaseRRR            , (0b1001101100100000111111, kX , kZR, kW , kZR, kW , kZR, false)                       , kRWI_W    , 0                         , 17 , 2815), // #286
  INST(Smsubl           , BaseRRRR           , (0b1001101100100000100000, kX , kZR, kW , kZR, kW , kZR, kX , kZR, false)             , kRWI_W    , 0                         , 3  , 2827), // #287
  INST(Smulh            , BaseRRR            , (0b1001101101000000011111, kX , kZR, kX , kZR, kX , kZR, true)                        , kRWI_W    , 0                         , 18 , 2834), // #288
  INST(Smull            , BaseRRR            , (0b1001101100100000011111, kX , kZR, kW , kZR, kW , kZR, false)                       , kRWI_W    , 0                         , 19 , 2840), // #289
  INST(Ssbb             , BaseOp             , (0b11010101000000110011000010011111)                                                  , 0         , 0                         , 17 , 2339), // #290
  INST(St2g             , BaseRM_SImm9       , (0b1101100110100000000010, 0b1101100110100000000001, kX, kSP, 0, 4)                   , kRWI_RW   , 0                         , 13 , 3164), // #291
  INST(Stadd            , BaseAtomicSt       , (0b1011100000100000000000, kWX, 30)                                                   , kRWI_RX   , 0                         , 0  , 3177), // #292
  INST(Staddl           , BaseAtomicSt       , (0b1011100001100000000000, kWX, 30)                                                   , kRWI_RX   , 0                         , 1  , 3197), // #293
  INST(Staddb           , BaseAtomicSt       , (0b0011100000100000000000, kW , 0 )                                                   , kRWI_RX   , 0                         , 2  , 3183), // #294
  INST(Staddlb          , BaseAtomicSt       , (0b0011100001100000000000, kW , 0 )                                                   , kRWI_RX   , 0                         , 3  , 3204), // #295
  INST(Staddh           , BaseAtomicSt       , (0b0111100000100000000000, kW , 0 )                                                   , kRWI_RX   , 0                         , 4  , 3190), // #296
  INST(Staddlh          , BaseAtomicSt       , (0b0111100001100000000000, kW , 0 )                                                   , kRWI_RX   , 0                         , 5  , 3212), // #297
  INST(Stclr            , BaseAtomicSt       , (0b1011100000100000000100, kWX, 30)                                                   , kRWI_RX   , 0                         , 6  , 3220), // #298
  INST(Stclrl           , BaseAtomicSt       , (0b1011100001100000000100, kWX, 30)                                                   , kRWI_RX   , 0                         , 7  , 3240), // #299
  INST(Stclrb           , BaseAtomicSt       , (0b0011100000100000000100, kW , 0 )                                                   , kRWI_RX   , 0                         , 8  , 3226), // #300
  INST(Stclrlb          , BaseAtomicSt       , (0b0011100001100000000100, kW , 0 )                                                   , kRWI_RX   , 0                         , 9  , 3247), // #301
  INST(Stclrh           , BaseAtomicSt       , (0b0111100000100000000100, kW , 0 )                                                   , kRWI_RX   , 0                         , 10 , 3233), // #302
  INST(Stclrlh          , BaseAtomicSt       , (0b0111100001100000000100, kW , 0 )                                                   , kRWI_RX   , 0                         , 11 , 3255), // #303
  INST(Steor            , BaseAtomicSt       , (0b1011100000100000001000, kWX, 30)                                                   , kRWI_RX   , 0                         , 12 , 3263), // #304
  INST(Steorl           , BaseAtomicSt       , (0b1011100001100000001000, kWX, 30)                                                   , kRWI_RX   , 0                         , 13 , 3283), // #305
  INST(Steorb           , BaseAtomicSt       , (0b0011100000100000001000, kW , 0 )                                                   , kRWI_RX   , 0                         , 14 , 3269), // #306
  INST(Steorlb          , BaseAtomicSt       , (0b0011100001100000001000, kW , 0 )                                                   , kRWI_RX   , 0                         , 15 , 3290), // #307
  INST(Steorh           , BaseAtomicSt       , (0b0111100000100000001000, kW , 0 )                                                   , kRWI_RX   , 0                         , 16 , 3276), // #308
  INST(Steorlh          , BaseAtomicSt       , (0b0111100001100000001000, kW , 0 )                                                   , kRWI_RX   , 0                         , 17 , 3298), // #309
  INST(Stg              , BaseRM_SImm9       , (0b1101100100100000000010, 0b1101100100100000000001, kX, kSP, 0, 4)                   , kRWI_RW   , 0                         , 14 , 3306), // #310
  INST(Stgm             , BaseRM_NoImm       , (0b1101100110100000000000, kX , kZR, 0 )                                              , kRWI_RW   , 0                         , 13 , 3310), // #311
  INST(Stgp             , BaseLdpStp         , (0b0110100100, 0b0110100010, kX, 0, 4)                                                , kRWI_RRW  , 0                         , 3  , 3315), // #312
  INST(Stllr            , BaseRM_NoImm       , (0b1000100010011111011111, kWX, kZR, 30)                                              , kRWI_RW   , 0                         , 14 , 3320), // #313
  INST(Stllrb           , BaseRM_NoImm       , (0b0000100010011111011111, kW , kZR, 0 )                                              , kRWI_RW   , 0                         , 15 , 3326), // #314
  INST(Stllrh           , BaseRM_NoImm       , (0b0100100010011111011111, kW , kZR, 0 )                                              , kRWI_RW   , 0                         , 16 , 3333), // #315
  INST(Stlr             , BaseRM_NoImm       , (0b1000100010011111111111, kWX, kZR, 30)                                              , kRWI_RW   , 0                         , 17 , 3340), // #316
  INST(Stlrb            , BaseRM_NoImm       , (0b0000100010011111111111, kW , kZR, 0 )                                              , kRWI_RW   , 0                         , 18 , 3345), // #317
  INST(Stlrh            , BaseRM_NoImm       , (0b0100100010011111111111, kW , kZR, 0 )                                              , kRWI_RW   , 0                         , 19 , 3351), // #318
  INST(Stlxp            , BaseStxp           , (0b1000100000100000100000, kWX, 30)                                                   , kRWI_WRRX , 0                         , 0  , 3357), // #319
  INST(Stlxr            , BaseAtomicOp       , (0b1000100000000000111111, kWX, 30, 1)                                                , kRWI_WRX  , 0                         , 108, 3363), // #320
  INST(Stlxrb           , BaseAtomicOp       , (0b0000100000000000111111, kW , 0 , 1)                                                , kRWI_WRX  , 0                         , 109, 3369), // #321
  INST(Stlxrh           , BaseAtomicOp       , (0b0100100000000000111111, kW , 0 , 1)                                                , kRWI_WRX  , 0                         , 110, 3376), // #322
  INST(Stnp             , BaseLdpStp         , (0b0010100000, 0           , kWX, 31, 2)                                              , kRWI_RRW  , 0                         , 4  , 3383), // #323
  INST(Stp              , BaseLdpStp         , (0b0010100100, 0b0010100010, kWX, 31, 2)                                              , kRWI_RRW  , 0                         , 5  , 3388), // #324
  INST(Str              , BaseLdSt           , (0b1011100100, 0b10111000000, 0b10111000001, 0         , kWX, 30, 2, Inst::kIdStur)   , kRWI_RW   , 0                         , 6  , 3392), // #325
  INST(Strb             , BaseLdSt           , (0b0011100100, 0b00111000000, 0b00111000001, 0         , kW , 30, 0, Inst::kIdSturb)  , kRWI_RW   , 0                         , 7  , 3396), // #326
  INST(Strh             , BaseLdSt           , (0b0111100100, 0b01111000000, 0b01111000001, 0         , kWX, 30, 1, Inst::kIdSturh)  , kRWI_RW   , 0                         , 8  , 3401), // #327
  INST(Stset            , BaseAtomicSt       , (0b1011100000100000001100, kWX, 30)                                                   , kRWI_RX   , 0                         , 18 , 3406), // #328
  INST(Stsetl           , BaseAtomicSt       , (0b1011100001100000001100, kWX, 30)                                                   , kRWI_RX   , 0                         , 19 , 3426), // #329
  INST(Stsetb           , BaseAtomicSt       , (0b0011100000100000001100, kW , 0 )                                                   , kRWI_RX   , 0                         , 20 , 3412), // #330
  INST(Stsetlb          , BaseAtomicSt       , (0b0011100001100000001100, kW , 0 )                                                   , kRWI_RX   , 0                         , 21 , 3433), // #331
  INST(Stseth           , BaseAtomicSt       , (0b0111100000100000001100, kW , 0 )                                                   , kRWI_RX   , 0                         , 22 , 3419), // #332
  INST(Stsetlh          , BaseAtomicSt       , (0b0111100001100000001100, kW , 0 )                                                   , kRWI_RX   , 0                         , 23 , 3441), // #333
  INST(Stsmax           , BaseAtomicSt       , (0b1011100000100000010000, kWX, 30)                                                   , kRWI_RX   , 0                         , 24 , 3449), // #334
  INST(Stsmaxl          , BaseAtomicSt       , (0b1011100001100000010000, kWX, 30)                                                   , kRWI_RX   , 0                         , 25 , 3472), // #335
  INST(Stsmaxb          , BaseAtomicSt       , (0b0011100000100000010000, kW , 0 )                                                   , kRWI_RX   , 0                         , 26 , 3456), // #336
  INST(Stsmaxlb         , BaseAtomicSt       , (0b0011100001100000010000, kW , 0 )                                                   , kRWI_RX   , 0                         , 27 , 3480), // #337
  INST(Stsmaxh          , BaseAtomicSt       , (0b0111100000100000010000, kW , 0 )                                                   , kRWI_RX   , 0                         , 28 , 3464), // #338
  INST(Stsmaxlh         , BaseAtomicSt       , (0b0111100001100000010000, kW , 0 )                                                   , kRWI_RX   , 0                         , 29 , 3489), // #339
  INST(Stsmin           , BaseAtomicSt       , (0b1011100000100000010100, kWX, 30)                                                   , kRWI_RX   , 0                         , 30 , 3498), // #340
  INST(Stsminl          , BaseAtomicSt       , (0b1011100001100000010100, kWX, 30)                                                   , kRWI_RX   , 0                         , 31 , 3521), // #341
  INST(Stsminb          , BaseAtomicSt       , (0b0011100000100000010100, kW , 0 )                                                   , kRWI_RX   , 0                         , 32 , 3505), // #342
  INST(Stsminlb         , BaseAtomicSt       , (0b0011100001100000010100, kW , 0 )                                                   , kRWI_RX   , 0                         , 33 , 3529), // #343
  INST(Stsminh          , BaseAtomicSt       , (0b0111100000100000010100, kW , 0 )                                                   , kRWI_RX   , 0                         , 34 , 3513), // #344
  INST(Stsminlh         , BaseAtomicSt       , (0b0111100001100000010100, kW , 0 )                                                   , kRWI_RX   , 0                         , 35 , 3538), // #345
  INST(Sttr             , BaseRM_SImm9       , (0b1011100000000000000010, 0b0000000000000000000000, kWX, kZR, 30, 0)                 , kRWI_RW   , 0                         , 15 , 3547), // #346
  INST(Sttrb            , BaseRM_SImm9       , (0b0011100000000000000010, 0b0000000000000000000000, kW , kZR, 0 , 0)                 , kRWI_RW   , 0                         , 16 , 3552), // #347
  INST(Sttrh            , BaseRM_SImm9       , (0b0111100000000000000010, 0b0000000000000000000000, kW , kZR, 0 , 0)                 , kRWI_RW   , 0                         , 17 , 3558), // #348
  INST(Stumax           , BaseAtomicSt       , (0b1011100000100000011000, kWX, 30)                                                   , kRWI_RX   , 0                         , 36 , 3564), // #349
  INST(Stumaxl          , BaseAtomicSt       , (0b1011100001100000011000, kWX, 30)                                                   , kRWI_RX   , 0                         , 37 , 3587), // #350
  INST(Stumaxb          , BaseAtomicSt       , (0b0011100000100000011000, kW , 0 )                                                   , kRWI_RX   , 0                         , 38 , 3571), // #351
  INST(Stumaxlb         , BaseAtomicSt       , (0b0011100001100000011000, kW , 0 )                                                   , kRWI_RX   , 0                         , 39 , 3595), // #352
  INST(Stumaxh          , BaseAtomicSt       , (0b0111100000100000011000, kW , 0 )                                                   , kRWI_RX   , 0                         , 40 , 3579), // #353
  INST(Stumaxlh         , BaseAtomicSt       , (0b0111100001100000011000, kW , 0 )                                                   , kRWI_RX   , 0                         , 41 , 3604), // #354
  INST(Stumin           , BaseAtomicSt       , (0b1011100000100000011100, kWX, 30)                                                   , kRWI_RX   , 0                         , 42 , 3613), // #355
  INST(Stuminl          , BaseAtomicSt       , (0b1011100001100000011100, kWX, 30)                                                   , kRWI_RX   , 0                         , 43 , 3636), // #356
  INST(Stuminb          , BaseAtomicSt       , (0b0011100000100000011100, kW , 0 )                                                   , kRWI_RX   , 0                         , 44 , 3620), // #357
  INST(Stuminlb         , BaseAtomicSt       , (0b0011100001100000011100, kW , 0 )                                                   , kRWI_RX   , 0                         , 45 , 3644), // #358
  INST(Stuminh          , BaseAtomicSt       , (0b0111100000100000011100, kW , 0 )                                                   , kRWI_RX   , 0                         , 46 , 3628), // #359
  INST(Stuminlh         , BaseAtomicSt       , (0b0111100001100000011100, kW , 0 )                                                   , kRWI_RX   , 0                         , 47 , 3653), // #360
  INST(Stur             , BaseRM_SImm9       , (0b1011100000000000000000, 0b0000000000000000000000, kWX, kZR, 30, 0)                 , kRWI_RW   , 0                         , 18 , 3662), // #361
  INST(Sturb            , BaseRM_SImm9       , (0b0011100000000000000000, 0b0000000000000000000000, kW , kZR, 0 , 0)                 , kRWI_RW   , 0                         , 19 , 3667), // #362
  INST(Sturh            , BaseRM_SImm9       , (0b0111100000000000000000, 0b0000000000000000000000, kW , kZR, 0 , 0)                 , kRWI_RW   , 0                         , 20 , 3673), // #363
  INST(Stxp             , BaseStxp           , (0b1000100000100000000000, kWX, 30)                                                   , kRWI_WRRW , 0                         , 1  , 3679), // #364
  INST(Stxr             , BaseStx            , (0b1000100000000000011111, kWX, 30)                                                   , kRWI_WRW  , 0                         , 0  , 3684), // #365
  INST(Stxrb            , BaseStx            , (0b0000100000000000011111, kW , 0 )                                                   , kRWI_WRW  , 0                         , 1  , 3689), // #366
  INST(Stxrh            , BaseStx            , (0b0100100000000000011111, kW , 0 )                                                   , kRWI_WRW  , 0                         , 2  , 3695), // #367
  INST(Stz2g            , BaseRM_SImm9       , (0b1101100111100000000010, 0b1101100111100000000001, kX , kSP, 0, 4)                  , kRWI_RW   , 0                         , 21 , 3701), // #368
  INST(Stzg             , BaseRM_SImm9       , (0b1101100101100000000010, 0b1101100101100000000001, kX , kSP, 0, 4)                  , kRWI_RW   , 0                         , 22 , 3707), // #369
  INST(Stzgm            , BaseRM_NoImm       , (0b1101100100100000000000, kX , kZR, 0)                                               , kRWI_RW   , 0                         , 20 , 3712), // #370
  INST(Sub              , BaseAddSub         , (0b1001011000, 0b1001011001, 0b1010001)                                               , kRWI_X    , 0                         , 2  , 985 ), // #371
  INST(Subg             , BaseRRII           , (0b1101000110000000000000, kX, kSP, kX, kSP, 6, 4, 16, 4, 0, 10)                      , kRWI_W    , 0                         , 1  , 3718), // #372
  INST(Subp             , BaseRRR            , (0b1001101011000000000000, kX, kZR, kX, kSP, kX, kSP, false)                          , kRWI_W    , 0                         , 20 , 3723), // #373
  INST(Subps            , BaseRRR            , (0b1011101011000000000000, kX, kZR, kX, kSP, kX, kSP, false)                          , kRWI_W    , 0                         , 21 , 3728), // #374
  INST(Subs             , BaseAddSub         , (0b1101011000, 0b1101011001, 0b1110001)                                               , kRWI_X    , 0                         , 3  , 3734), // #375
  INST(Svc              , BaseOpImm          , (0b11010100000000000000000000000001, 16, 5)                                           , 0         , 0                         , 12 , 3752), // #376
  INST(Swp              , BaseAtomicOp       , (0b1011100000100000100000, kWX, 30, 1)                                                , kRWI_RWX  , 0                         , 111, 3756), // #377
  INST(Swpa             , BaseAtomicOp       , (0b1011100010100000100000, kWX, 30, 1)                                                , kRWI_RWX  , 0                         , 112, 3760), // #378
  INST(Swpab            , BaseAtomicOp       , (0b0011100010100000100000, kW , 0 , 1)                                                , kRWI_RWX  , 0                         , 113, 3765), // #379
  INST(Swpah            , BaseAtomicOp       , (0b0111100010100000100000, kW , 0 , 1)                                                , kRWI_RWX  , 0                         , 114, 3771), // #380
  INST(Swpal            , BaseAtomicOp       , (0b1011100011100000100000, kWX, 30, 1)                                                , kRWI_RWX  , 0                         , 115, 3777), // #381
  INST(Swpalb           , BaseAtomicOp       , (0b0011100011100000100000, kW , 0 , 1)                                                , kRWI_RWX  , 0                         , 116, 3783), // #382
  INST(Swpalh           , BaseAtomicOp       , (0b0111100011100000100000, kW , 0 , 1)                                                , kRWI_RWX  , 0                         , 117, 3790), // #383
  INST(Swpb             , BaseAtomicOp       , (0b0011100000100000100000, kW , 0 , 1)                                                , kRWI_RWX  , 0                         , 118, 3797), // #384
  INST(Swph             , BaseAtomicOp       , (0b0111100000100000100000, kW , 0 , 1)                                                , kRWI_RWX  , 0                         , 119, 3802), // #385
  INST(Swpl             , BaseAtomicOp       , (0b1011100001100000100000, kWX, 30, 1)                                                , kRWI_RWX  , 0                         , 120, 3807), // #386
  INST(Swplb            , BaseAtomicOp       , (0b0011100001100000100000, kW , 0 , 1)                                                , kRWI_RWX  , 0                         , 121, 3812), // #387
  INST(Swplh            , BaseAtomicOp       , (0b0111100001100000100000, kW , 0 , 1)                                                , kRWI_RWX  , 0                         , 122, 3818), // #388
  INST(Sxtb             , BaseExtend         , (0b0001001100000000000111, kWX, 0)                                                    , kRWI_W    , 0                         , 0  , 3824), // #389
  INST(Sxth             , BaseExtend         , (0b0001001100000000001111, kWX, 0)                                                    , kRWI_W    , 0                         , 1  , 3829), // #390
  INST(Sxtw             , BaseExtend         , (0b1001001101000000011111, kX , 0)                                                    , kRWI_W    , 0                         , 2  , 3845), // #391
  INST(Sys              , BaseSys            , (_)                                                                                   , kRWI_W    , 0                         , 0  , 3850), // #392
  INST(Tlbi             , BaseAtDcIcTlbi     , (0b00011110000000, 0b00010000000000, false)                                           , kRWI_RX   , 0                         , 3  , 3871), // #393
  INST(Tst              , BaseTst            , (0b1101010000, 0b111001000)                                                           , kRWI_R    , 0                         , 0  , 437 ), // #394
  INST(Tbnz             , BaseBranchTst      , (0b00110111000000000000000000000000)                                                  , kRWI_R    , 0                         , 0  , 3858), // #395
  INST(Tbz              , BaseBranchTst      , (0b00110110000000000000000000000000)                                                  , kRWI_R    , 0                         , 1  , 3867), // #396
  INST(Ubfiz            , BaseBfi            , (0b01010011000000000000000000000000)                                                  , kRWI_W    , 0                         , 2  , 3969), // #397
  INST(Ubfm             , BaseBfm            , (0b01010011000000000000000000000000)                                                  , kRWI_W    , 0                         , 2  , 3975), // #398
  INST(Ubfx             , BaseBfx            , (0b01010011000000000000000000000000)                                                  , kRWI_W    , 0                         , 2  , 3980), // #399
  INST(Udf              , BaseOpImm          , (0b00000000000000000000000000000000, 16, 0)                                           , 0         , 0                         , 13 , 3991), // #400
  INST(Udiv             , BaseRRR            , (0b0001101011000000000010, kWX, kZR, kWX, kZR, kWX, kZR, true)                        , kRWI_W    , 0                         , 22 , 3995), // #401
  INST(Umaddl           , BaseRRRR           , (0b1001101110100000000000, kX , kZR, kW , kZR, kW , kZR, kX , kZR, false)             , kRWI_W    , 0                         , 4  , 4012), // #402
  INST(Umnegl           , BaseRRR            , (0b1001101110100000111111, kX , kZR, kW , kZR, kW , kZR, false)                       , kRWI_W    , 0                         , 23 , 4075), // #403
  INST(Umull            , BaseRRR            , (0b1001101110100000011111, kX , kZR, kW , kZR, kW , kZR, false)                       , kRWI_W    , 0                         , 24 , 4100), // #404
  INST(Umulh            , BaseRRR            , (0b1001101111000000011111, kX , kZR, kX , kZR, kX , kZR, false)                       , kRWI_W    , 0                         , 25 , 4094), // #405
  INST(Umsubl           , BaseRRRR           , (0b1001101110100000100000, kX , kZR, kW , kZR, kW , kZR, kX , kZR, false)             , kRWI_W    , 0                         , 5  , 4087), // #406
  INST(Uxtb             , BaseExtend         , (0b0101001100000000000111, kW, 1)                                                     , kRWI_W    , 0                         , 3  , 4291), // #407
  INST(Uxth             , BaseExtend         , (0b0101001100000000001111, kW, 1)                                                     , kRWI_W    , 0                         , 4  , 4296), // #408
  INST(Wfe              , BaseOp             , (0b11010101000000110010000001011111)                                                  , 0         , 0                         , 18 , 4322), // #409
  INST(Wfi              , BaseOp             , (0b11010101000000110010000001111111)                                                  , 0         , 0                         , 19 , 4326), // #410
  INST(Xaflag           , BaseOp             , (0b11010101000000000100000000111111)                                                  , 0         , 0                         , 20 , 4330), // #411
  INST(Xpacd            , BaseR              , (0b11011010110000010100011111100000, kX, kZR, 0)                                      , kRWI_X    , 0                         , 8  , 4341), // #412
  INST(Xpaci            , BaseR              , (0b11011010110000010100001111100000, kX, kZR, 0)                                      , kRWI_X    , 0                         , 9  , 4347), // #413
  INST(Xpaclri          , BaseOp             , (0b11010101000000110010000011111111)                                                  , kRWI_X    , 0                         , 21 , 4353), // #414
  INST(Yield            , BaseOp             , (0b11010101000000110010000000111111)                                                  , 0         , 0                         , 22 , 4361), // #415
  INST(Abs_v            , ISimdVV            , (0b0000111000100000101110, kVO_V_Any)                                                 , kRWI_W    , 0                         , 0  , 2855), // #416
  INST(Add_v            , ISimdVVV           , (0b0000111000100000100001, kVO_V_Any)                                                 , kRWI_W    , 0                         , 0  , 978 ), // #417
  INST(Addhn_v          , ISimdVVV           , (0b0000111000100000010000, kVO_V_B8H4S2)                                              , kRWI_W    , F(Narrow)                 , 1  , 2345), // #418
  INST(Addhn2_v         , ISimdVVV           , (0b0100111000100000010000, kVO_V_B16H8S4)                                             , kRWI_W    , F(Narrow)                 , 2  , 2352), // #419
  INST(Addp_v           , ISimdPair          , (0b0101111000110001101110, 0b0000111000100000101111, kVO_V_Any)                       , kRWI_W    , F(Pair)                   , 0  , 638 ), // #420
  INST(Addv_v           , ISimdSV            , (0b0000111000110001101110, kVO_V_BH_4S)                                               , kRWI_W    , 0                         , 0  , 20  ), // #421
  INST(Aesd_v           , ISimdVVx           , (0b0100111000101000010110, kOp_V16B, kOp_V16B)                                        , kRWI_X    , 0                         , 0  , 34  ), // #422
  INST(Aese_v           , ISimdVVx           , (0b0100111000101000010010, kOp_V16B, kOp_V16B)                                        , kRWI_X    , 0                         , 1  , 39  ), // #423
  INST(Aesimc_v         , ISimdVVx           , (0b0100111000101000011110, kOp_V16B, kOp_V16B)                                        , kRWI_W    , 0                         , 2  , 44  ), // #424
  INST(Aesmc_v          , ISimdVVx           , (0b0100111000101000011010, kOp_V16B, kOp_V16B)                                        , kRWI_W    , 0                         , 3  , 51  ), // #425
  INST(And_v            , ISimdVVV           , (0b0000111000100000000111, kVO_V_B)                                                   , kRWI_W    , 0                         , 3  , 57  ), // #426
  INST(Bcax_v           , ISimdVVVV          , (0b1100111000100000000000, kVO_V_B16)                                                 , kRWI_W    , 0                         , 0  , 187 ), // #427
  INST(Bfcvt_v          , ISimdVVx           , (0b0001111001100011010000, kOp_H, kOp_S)                                              , kRWI_W    , 0                         , 4  , 196 ), // #428
  INST(Bfcvtn_v         , ISimdVVx           , (0b0000111010100001011010, kOp_V4H, kOp_V4S)                                          , kRWI_W    , F(Narrow)                 , 5  , 202 ), // #429
  INST(Bfcvtn2_v        , ISimdVVx           , (0b0100111010100001011010, kOp_V8H, kOp_V4S)                                          , kRWI_W    , F(Narrow)                 , 6  , 209 ), // #430
  INST(Bfdot_v          , SimdDot            , (0b0010111001000000111111, 0b0000111101000000111100, kET_S, kET_H, kET_2H)            , kRWI_X    , 0                         , 0  , 217 ), // #431
  INST(Bfmlalb_v        , SimdFmlal          , (0b0010111011000000111111, 0b0000111111000000111100, 0, kET_S, kET_H, kET_H)          , kRWI_X    , F(VH0_15)                 , 0  , 227 ), // #432
  INST(Bfmlalt_v        , SimdFmlal          , (0b0110111011000000111111, 0b0100111111000000111100, 0, kET_S, kET_H, kET_H)          , kRWI_X    , F(VH0_15)                 , 1  , 235 ), // #433
  INST(Bfmmla_v         , ISimdVVVx          , (0b0110111001000000111011, kOp_V4S, kOp_V8H, kOp_V8H)                                 , kRWI_X    , F(Long)                   , 0  , 243 ), // #434
  INST(Bic_v            , SimdBicOrr         , (0b0000111001100000000111, 0b0010111100000000000001)                                  , kRWI_W    , 0                         , 0  , 256 ), // #435
  INST(Bif_v            , ISimdVVV           , (0b0010111011100000000111, kVO_V_B)                                                   , kRWI_X    , 0                         , 4  , 265 ), // #436
  INST(Bit_v            , ISimdVVV           , (0b0010111010100000000111, kVO_V_B)                                                   , kRWI_X    , 0                         , 5  , 2365), // #437
  INST(Bsl_v            , ISimdVVV           , (0b0010111001100000000111, kVO_V_B)                                                   , kRWI_X    , 0                         , 6  , 280 ), // #438
  INST(Cls_v            , ISimdVV            , (0b0000111000100000010010, kVO_V_BHS)                                                 , kRWI_W    , 0                         , 1  , 412 ), // #439
  INST(Clz_v            , ISimdVV            , (0b0010111000100000010010, kVO_V_BHS)                                                 , kRWI_W    , 0                         , 2  , 416 ), // #440
  INST(Cmeq_v           , SimdCmp            , (0b0010111000100000100011, 0b0000111000100000100110, kVO_V_Any)                       , kRWI_W    , 0                         , 0  , 663 ), // #441
  INST(Cmge_v           , SimdCmp            , (0b0000111000100000001111, 0b0010111000100000100010, kVO_V_Any)                       , kRWI_W    , 0                         , 1  , 669 ), // #442
  INST(Cmgt_v           , SimdCmp            , (0b0000111000100000001101, 0b0000111000100000100010, kVO_V_Any)                       , kRWI_W    , 0                         , 2  , 675 ), // #443
  INST(Cmhi_v           , SimdCmp            , (0b0010111000100000001101, 0b0000000000000000000000, kVO_V_Any)                       , kRWI_W    , 0                         , 3  , 420 ), // #444
  INST(Cmhs_v           , SimdCmp            , (0b0010111000100000001111, 0b0000000000000000000000, kVO_V_Any)                       , kRWI_W    , 0                         , 4  , 425 ), // #445
  INST(Cmle_v           , SimdCmp            , (0b0000000000000000000000, 0b0010111000100000100110, kVO_V_Any)                       , kRWI_W    , 0                         , 5  , 687 ), // #446
  INST(Cmlt_v           , SimdCmp            , (0b0000000000000000000000, 0b0000111000100000101010, kVO_V_Any)                       , kRWI_W    , 0                         , 6  , 693 ), // #447
  INST(Cmtst_v          , ISimdVVV           , (0b0000111000100000100011, kVO_V_Any)                                                 , kRWI_W    , 0                         , 7  , 435 ), // #448
  INST(Cnt_v            , ISimdVV            , (0b0000111000100000010110, kVO_V_B)                                                   , kRWI_W    , 0                         , 3  , 446 ), // #449
  INST(Dup_v            , SimdDup            , (_)                                                                                   , kRWI_W    , 0                         , 0  , 579 ), // #450
  INST(Eor_v            , ISimdVVV           , (0b0010111000100000000111, kVO_V_B)                                                   , kRWI_W    , 0                         , 8  , 1418), // #451
  INST(Eor3_v           , ISimdVVVV          , (0b1100111000000000000000, kVO_V_B16)                                                 , kRWI_W    , 0                         , 1  , 587 ), // #452
  INST(Ext_v            , ISimdVVVI          , (0b0010111000000000000000, kVO_V_B, 4, 11, 1)                                         , kRWI_W    , 0                         , 0  , 601 ), // #453
  INST(Fabd_v           , FSimdVVV           , (0b0111111010100000110101, kHF_C, 0b0010111010100000110101, kHF_C)                    , kRWI_W    , 0                         , 0  , 610 ), // #454
  INST(Fabs_v           , FSimdVV            , (0b0001111000100000110000, kHF_A, 0b0000111010100000111110, kHF_B)                    , kRWI_W    , 0                         , 0  , 615 ), // #455
  INST(Facge_v          , FSimdVVV           , (0b0111111000100000111011, kHF_C, 0b0010111000100000111011, kHF_C)                    , kRWI_W    , 0                         , 1  , 620 ), // #456
  INST(Facgt_v          , FSimdVVV           , (0b0111111010100000111011, kHF_C, 0b0010111010100000111011, kHF_C)                    , kRWI_W    , 0                         , 2  , 626 ), // #457
  INST(Fadd_v           , FSimdVVV           , (0b0001111000100000001010, kHF_A, 0b0000111000100000110101, kHF_C)                    , kRWI_W    , 0                         , 3  , 632 ), // #458
  INST(Faddp_v          , FSimdPair          , (0b0111111000110000110110, 0b0010111000100000110101)                                  , kRWI_W    , 0                         , 0  , 637 ), // #459
  INST(Fcadd_v          , SimdFcadd          , (0b0010111000000000111001)                                                            , kRWI_W    , 0                         , 0  , 643 ), // #460
  INST(Fccmp_v          , SimdFccmpFccmpe    , (0b00011110001000000000010000000000)                                                  , kRWI_R    , 0                         , 0  , 649 ), // #461
  INST(Fccmpe_v         , SimdFccmpFccmpe    , (0b00011110001000000000010000010000)                                                  , kRWI_R    , 0                         , 1  , 655 ), // #462
  INST(Fcmeq_v          , SimdFcm            , (0b0000111000100000111001, kHF_C, 0b0000111010100000110110)                           , kRWI_W    , 0                         , 0  , 662 ), // #463
  INST(Fcmge_v          , SimdFcm            , (0b0010111000100000111001, kHF_C, 0b0010111010100000110010)                           , kRWI_W    , 0                         , 1  , 668 ), // #464
  INST(Fcmgt_v          , SimdFcm            , (0b0010111010100000111001, kHF_C, 0b0000111010100000110010)                           , kRWI_W    , 0                         , 2  , 674 ), // #465
  INST(Fcmla_v          , SimdFcmla          , (0b0010111000000000110001, 0b0010111100000000000100)                                  , kRWI_X    , 0                         , 0  , 680 ), // #466
  INST(Fcmle_v          , SimdFcm            , (0b0000000000000000000000, kHF_C, 0b0010111010100000110110)                           , kRWI_W    , 0                         , 3  , 686 ), // #467
  INST(Fcmlt_v          , SimdFcm            , (0b0000000000000000000000, kHF_C, 0b0000111010100000111010)                           , kRWI_W    , 0                         , 4  , 692 ), // #468
  INST(Fcmp_v           , SimdFcmpFcmpe      , (0b00011110001000000010000000000000)                                                  , kRWI_R    , 0                         , 0  , 698 ), // #469
  INST(Fcmpe_v          , SimdFcmpFcmpe      , (0b00011110001000000010000000010000)                                                  , kRWI_R    , 0                         , 1  , 703 ), // #470
  INST(Fcsel_v          , SimdFcsel          , (_)                                                                                   , kRWI_W    , 0                         , 0  , 709 ), // #471
  INST(Fcvt_v           , SimdFcvt           , (_)                                                                                   , kRWI_W    , 0                         , 0  , 197 ), // #472
  INST(Fcvtas_v         , SimdFcvtSV         , (0b0000111000100001110010, 0b0000000000000000000000, 0b0001111000100100000000, 1)     , kRWI_W    , 0                         , 0  , 715 ), // #473
  INST(Fcvtau_v         , SimdFcvtSV         , (0b0010111000100001110010, 0b0000000000000000000000, 0b0001111000100101000000, 1)     , kRWI_W    , 0                         , 1  , 722 ), // #474
  INST(Fcvtl_v          , SimdFcvtLN         , (0b0000111000100001011110, 0, 0)                                                      , kRWI_W    , F(Long)                   , 0  , 729 ), // #475
  INST(Fcvtl2_v         , SimdFcvtLN         , (0b0100111000100001011110, 0, 0)                                                      , kRWI_W    , F(Long)                   , 1  , 735 ), // #476
  INST(Fcvtms_v         , SimdFcvtSV         , (0b0000111000100001101110, 0b0000000000000000000000, 0b0001111000110000000000, 1)     , kRWI_W    , 0                         , 2  , 742 ), // #477
  INST(Fcvtmu_v         , SimdFcvtSV         , (0b0010111000100001101110, 0b0000000000000000000000, 0b0001111000110001000000, 1)     , kRWI_W    , 0                         , 3  , 749 ), // #478
  INST(Fcvtn_v          , SimdFcvtLN         , (0b0000111000100001011010, 0, 0)                                                      , kRWI_W    , F(Narrow)                 , 2  , 203 ), // #479
  INST(Fcvtn2_v         , SimdFcvtLN         , (0b0100111000100001011010, 0, 0)                                                      , kRWI_X    , F(Narrow)                 , 3  , 210 ), // #480
  INST(Fcvtns_v         , SimdFcvtSV         , (0b0000111000100001101010, 0b0000000000000000000000, 0b0001111000100000000000, 1)     , kRWI_W    , 0                         , 4  , 756 ), // #481
  INST(Fcvtnu_v         , SimdFcvtSV         , (0b0010111000100001101010, 0b0000000000000000000000, 0b0001111000100001000000, 1)     , kRWI_W    , 0                         , 5  , 763 ), // #482
  INST(Fcvtps_v         , SimdFcvtSV         , (0b0000111010100001101010, 0b0000000000000000000000, 0b0001111000101000000000, 1)     , kRWI_W    , 0                         , 6  , 770 ), // #483
  INST(Fcvtpu_v         , SimdFcvtSV         , (0b0010111010100001101010, 0b0000000000000000000000, 0b0001111000101001000000, 1)     , kRWI_W    , 0                         , 7  , 777 ), // #484
  INST(Fcvtxn_v         , SimdFcvtLN         , (0b0010111000100001011010, 1, 1)                                                      , kRWI_W    , F(Narrow)                 , 4  , 784 ), // #485
  INST(Fcvtxn2_v        , SimdFcvtLN         , (0b0110111000100001011010, 1, 0)                                                      , kRWI_X    , F(Narrow)                 , 5  , 791 ), // #486
  INST(Fcvtzs_v         , SimdFcvtSV         , (0b0000111010100001101110, 0b0000111100000000111111, 0b0001111000111000000000, 1)     , kRWI_W    , 0                         , 8  , 799 ), // #487
  INST(Fcvtzu_v         , SimdFcvtSV         , (0b0010111010100001101110, 0b0010111100000000111111, 0b0001111000111001000000, 1)     , kRWI_W    , 0                         , 9  , 806 ), // #488
  INST(Fdiv_v           , FSimdVVV           , (0b0001111000100000000110, kHF_A, 0b0010111000100000111111, kHF_C)                    , kRWI_W    , 0                         , 4  , 813 ), // #489
  INST(Fjcvtzs_v        , ISimdVVx           , (0b0001111001111110000000, kOp_GpW, kOp_D)                                            , kRWI_W    , 0                         , 7  , 818 ), // #490
  INST(Fmadd_v          , FSimdVVVV          , (0b0001111100000000000000, kHF_A, 0b0000000000000000000000, kHF_N)                    , kRWI_W    , 0                         , 0  , 826 ), // #491
  INST(Fmax_v           , FSimdVVV           , (0b0001111000100000010010, kHF_A, 0b0000111000100000111101, kHF_C)                    , kRWI_W    , 0                         , 5  , 832 ), // #492
  INST(Fmaxnm_v         , FSimdVVV           , (0b0001111000100000011010, kHF_A, 0b0000111000100000110001, kHF_C)                    , kRWI_W    , 0                         , 6  , 837 ), // #493
  INST(Fmaxnmp_v        , FSimdPair          , (0b0111111000110000110010, 0b0010111000100000110001)                                  , kRWI_W    , 0                         , 1  , 844 ), // #494
  INST(Fmaxnmv_v        , FSimdSV            , (0b0010111000110000110010)                                                            , kRWI_W    , 0                         , 0  , 852 ), // #495
  INST(Fmaxp_v          , FSimdPair          , (0b0111111000110000111110, 0b0010111000100000111101)                                  , kRWI_W    , 0                         , 2  , 860 ), // #496
  INST(Fmaxv_v          , FSimdSV            , (0b0010111000110000111110)                                                            , kRWI_W    , 0                         , 1  , 866 ), // #497
  INST(Fmin_v           , FSimdVVV           , (0b0001111000100000010110, kHF_A, 0b0000111010100000111101, kHF_C)                    , kRWI_W    , 0                         , 7  , 872 ), // #498
  INST(Fminnm_v         , FSimdVVV           , (0b0001111000100000011110, kHF_A, 0b0000111010100000110001, kHF_C)                    , kRWI_W    , 0                         , 8  , 877 ), // #499
  INST(Fminnmp_v        , FSimdPair          , (0b0111111010110000110010, 0b0010111010100000110001)                                  , kRWI_W    , 0                         , 3  , 884 ), // #500
  INST(Fminnmv_v        , FSimdSV            , (0b0010111010110000110010)                                                            , kRWI_W    , 0                         , 2  , 892 ), // #501
  INST(Fminp_v          , FSimdPair          , (0b0111111010110000111110, 0b0010111010100000111101)                                  , kRWI_W    , 0                         , 4  , 900 ), // #502
  INST(Fminv_v          , FSimdSV            , (0b0010111010110000111110)                                                            , kRWI_W    , 0                         , 3  , 906 ), // #503
  INST(Fmla_v           , FSimdVVVe          , (0b0000000000000000000000, kHF_N, 0b0000111000100000110011, 0b0000111110000000000100) , kRWI_X    , F(VH0_15)                 , 0  , 912 ), // #504
  INST(Fmlal_v          , SimdFmlal          , (0b0000111000100000111011, 0b0000111110000000000000, 1, kET_S, kET_H, kET_H)          , kRWI_X    , F(VH0_15)                 , 2  , 917 ), // #505
  INST(Fmlal2_v         , SimdFmlal          , (0b0010111000100000110011, 0b0010111110000000100000, 1, kET_S, kET_H, kET_H)          , kRWI_X    , F(VH0_15)                 , 3  , 923 ), // #506
  INST(Fmls_v           , FSimdVVVe          , (0b0000000000000000000000, kHF_N, 0b0000111010100000110011, 0b0000111110000000010100) , kRWI_X    , F(VH0_15)                 , 1  , 930 ), // #507
  INST(Fmlsl_v          , SimdFmlal          , (0b0000111010100000111011, 0b0000111110000000010000, 1, kET_S, kET_H, kET_H)          , kRWI_X    , F(VH0_15)                 , 4  , 935 ), // #508
  INST(Fmlsl2_v         , SimdFmlal          , (0b0010111010100000110011, 0b0010111110000000110000, 1, kET_S, kET_H, kET_H)          , kRWI_X    , F(VH0_15)                 , 5  , 941 ), // #509
  INST(Fmov_v           , SimdFmov           , (_)                                                                                   , kRWI_W    , 0                         , 0  , 948 ), // #510
  INST(Fmsub_v          , FSimdVVVV          , (0b0001111100000000100000, kHF_A, 0b0000000000000000000000, kHF_N)                    , kRWI_W    , 0                         , 1  , 953 ), // #511
  INST(Fmul_v           , FSimdVVVe          , (0b0001111000100000000010, kHF_A, 0b0010111000100000110111, 0b0000111110000000100100) , kRWI_W    , F(VH0_15)                 , 2  , 959 ), // #512
  INST(Fmulx_v          , FSimdVVVe          , (0b0101111000100000110111, kHF_C, 0b0000111000100000110111, 0b0010111110000000100100) , kRWI_W    , F(VH0_15)                 , 3  , 964 ), // #513
  INST(Fneg_v           , FSimdVV            , (0b0001111000100001010000, kHF_A, 0b0010111010100000111110, kHF_B)                    , kRWI_W    , 0                         , 1  , 970 ), // #514
  INST(Fnmadd_v         , FSimdVVVV          , (0b0001111100100000000000, kHF_A, 0b0000000000000000000000, kHF_N)                    , kRWI_W    , 0                         , 2  , 975 ), // #515
  INST(Fnmsub_v         , FSimdVVVV          , (0b0001111100100000100000, kHF_A, 0b0000000000000000000000, kHF_N)                    , kRWI_W    , 0                         , 3  , 982 ), // #516
  INST(Fnmul_v          , FSimdVVV           , (0b0001111000100000100010, kHF_A, 0b0000000000000000000000, kHF_N)                    , kRWI_W    , 0                         , 9  , 989 ), // #517
  INST(Frecpe_v         , FSimdVV            , (0b0101111010100001110110, kHF_B, 0b0000111010100001110110, kHF_B)                    , kRWI_W    , 0                         , 2  , 995 ), // #518
  INST(Frecps_v         , FSimdVVV           , (0b0101111000100000111111, kHF_C, 0b0000111000100000111111, kHF_C)                    , kRWI_W    , 0                         , 10 , 1002), // #519
  INST(Frecpx_v         , FSimdVV            , (0b0101111010100001111110, kHF_B, 0b0000000000000000000000, kHF_N)                    , kRWI_W    , 0                         , 3  , 1009), // #520
  INST(Frint32x_v       , FSimdVV            , (0b0001111000101000110000, kHF_N, 0b0010111000100001111010, kHF_N)                    , kRWI_W    , 0                         , 4  , 1016), // #521
  INST(Frint32z_v       , FSimdVV            , (0b0001111000101000010000, kHF_N, 0b0000111000100001111010, kHF_N)                    , kRWI_W    , 0                         , 5  , 1025), // #522
  INST(Frint64x_v       , FSimdVV            , (0b0001111000101001110000, kHF_N, 0b0010111000100001111110, kHF_N)                    , kRWI_W    , 0                         , 6  , 1034), // #523
  INST(Frint64z_v       , FSimdVV            , (0b0001111000101001010000, kHF_N, 0b0000111000100001111110, kHF_N)                    , kRWI_W    , 0                         , 7  , 1043), // #524
  INST(Frinta_v         , FSimdVV            , (0b0001111000100110010000, kHF_A, 0b0010111000100001100010, kHF_B)                    , kRWI_W    , 0                         , 8  , 1052), // #525
  INST(Frinti_v         , FSimdVV            , (0b0001111000100111110000, kHF_A, 0b0010111010100001100110, kHF_B)                    , kRWI_W    , 0                         , 9  , 1059), // #526
  INST(Frintm_v         , FSimdVV            , (0b0001111000100101010000, kHF_A, 0b0000111000100001100110, kHF_B)                    , kRWI_W    , 0                         , 10 , 1066), // #527
  INST(Frintn_v         , FSimdVV            , (0b0001111000100100010000, kHF_A, 0b0000111000100001100010, kHF_B)                    , kRWI_W    , 0                         , 11 , 1073), // #528
  INST(Frintp_v         , FSimdVV            , (0b0001111000100100110000, kHF_A, 0b0000111010100001100010, kHF_B)                    , kRWI_W    , 0                         , 12 , 1080), // #529
  INST(Frintx_v         , FSimdVV            , (0b0001111000100111010000, kHF_A, 0b0010111000100001100110, kHF_B)                    , kRWI_W    , 0                         , 13 , 1087), // #530
  INST(Frintz_v         , FSimdVV            , (0b0001111000100101110000, kHF_A, 0b0000111010100001100110, kHF_B)                    , kRWI_W    , 0                         , 14 , 1094), // #531
  INST(Frsqrte_v        , FSimdVV            , (0b0111111010100001110110, kHF_B, 0b0010111010100001110110, kHF_B)                    , kRWI_W    , 0                         , 15 , 1101), // #532
  INST(Frsqrts_v        , FSimdVVV           , (0b0101111010100000111111, kHF_C, 0b0000111010100000111111, kHF_C)                    , kRWI_W    , 0                         , 11 , 1109), // #533
  INST(Fsqrt_v          , FSimdVV            , (0b0001111000100001110000, kHF_A, 0b0010111010100001111110, kHF_B)                    , kRWI_W    , 0                         , 16 , 1117), // #534
  INST(Fsub_v           , FSimdVVV           , (0b0001111000100000001110, kHF_A, 0b0000111010100000110101, kHF_C)                    , kRWI_W    , 0                         , 12 , 1123), // #535
  INST(Ins_v            , SimdIns            , (_)                                                                                   , kRWI_X    , 0                         , 0  , 1145), // #536
  INST(Ld1_v            , SimdLdNStN         , (0b0000110101000000000000, 0b0000110001000000001000, 1, 0)                            , kRWI_LDn  , F(Consecutive)            , 0  , 1153), // #537
  INST(Ld1r_v           , SimdLdNStN         , (0b0000110101000000110000, 0b0000000000000000000000, 1, 1)                            , kRWI_LDn  , F(Consecutive)            , 1  , 1157), // #538
  INST(Ld2_v            , SimdLdNStN         , (0b0000110101100000000000, 0b0000110001000000100000, 2, 0)                            , kRWI_LDn  , F(Consecutive)            , 2  , 1162), // #539
  INST(Ld2r_v           , SimdLdNStN         , (0b0000110101100000110000, 0b0000000000000000000000, 2, 1)                            , kRWI_LDn  , F(Consecutive)            , 3  , 1166), // #540
  INST(Ld3_v            , SimdLdNStN         , (0b0000110101000000001000, 0b0000110001000000010000, 3, 0)                            , kRWI_LDn  , F(Consecutive)            , 4  , 1171), // #541
  INST(Ld3r_v           , SimdLdNStN         , (0b0000110101000000111000, 0b0000000000000000000000, 3, 1)                            , kRWI_LDn  , F(Consecutive)            , 5  , 1175), // #542
  INST(Ld4_v            , SimdLdNStN         , (0b0000110101100000001000, 0b0000110001000000000000, 4, 0)                            , kRWI_LDn  , F(Consecutive)            , 6  , 1180), // #543
  INST(Ld4r_v           , SimdLdNStN         , (0b0000110101100000111000, 0b0000000000000000000000, 4, 1)                            , kRWI_LDn  , F(Consecutive)            , 7  , 1184), // #544
  INST(Ldnp_v           , SimdLdpStp         , (0b0010110001, 0b0000000000)                                                          , kRWI_WW   , 0                         , 0  , 1537), // #545
  INST(Ldp_v            , SimdLdpStp         , (0b0010110101, 0b0010110011)                                                          , kRWI_WW   , 0                         , 1  , 1542), // #546
  INST(Ldr_v            , SimdLdSt           , (0b0011110101, 0b00111100010, 0b00111100011, 0b00011100, Inst::kIdLdur_v)             , kRWI_W    , 0                         , 0  , 1552), // #547
  INST(Ldur_v           , SimdLdurStur       , (0b0011110001000000000000)                                                            , kRWI_W    , 0                         , 0  , 2142), // #548
  INST(Mla_v            , ISimdVVVe          , (0b0000111000100000100101, kVO_V_BHS, 0b0010111100000000000000, kVO_V_HS)             , kRWI_X    , F(VH0_15)                 , 0  , 246 ), // #549
  INST(Mls_v            , ISimdVVVe          , (0b0010111000100000100101, kVO_V_BHS, 0b0010111100000000010000, kVO_V_HS)             , kRWI_X    , F(VH0_15)                 , 1  , 931 ), // #550
  INST(Mov_v            , SimdMov            , (_)                                                                                   , kRWI_W    , 0                         , 0  , 949 ), // #551
  INST(Movi_v           , SimdMoviMvni       , (0b0000111100000000000001, 0)                                                         , kRWI_W    , 0                         , 0  , 2221), // #552
  INST(Mul_v            , ISimdVVVe          , (0b0000111000100000100111, kVO_V_BHS, 0b0000111100000000100000, kVO_V_HS)             , kRWI_W    , F(VH0_15)                 , 2  , 991 ), // #553
  INST(Mvn_v            , ISimdVV            , (0b0010111000100000010110, kVO_V_B)                                                   , kRWI_W    , 0                         , 4  , 2249), // #554
  INST(Mvni_v           , SimdMoviMvni       , (0b0000111100000000000001, 1)                                                         , kRWI_W    , 0                         , 1  , 2253), // #555
  INST(Neg_v            , ISimdVV            , (0b0010111000100000101110, kVO_V_Any)                                                 , kRWI_W    , 0                         , 5  , 540 ), // #556
  INST(Not_v            , ISimdVV            , (0b0010111000100000010110, kVO_V_B)                                                   , kRWI_W    , 0                         , 6  , 2276), // #557
  INST(Orn_v            , ISimdVVV           , (0b0000111011100000000111, kVO_V_B)                                                   , kRWI_W    , 0                         , 9  , 2280), // #558
  INST(Orr_v            , SimdBicOrr         , (0b0000111010100000000111, 0b0000111100000000000001)                                  , kRWI_W    , 0                         , 1  , 2284), // #559
  INST(Pmul_v           , ISimdVVV           , (0b0010111000100000100111, kVO_V_B)                                                   , kRWI_W    , 0                         , 10 , 2320), // #560
  INST(Pmull_v          , ISimdVVV           , (0b0000111000100000111000, kVO_V_B8D1)                                                , kRWI_W    , F(Long)                   , 11 , 2325), // #561
  INST(Pmull2_v         , ISimdVVV           , (0b0100111000100000111000, kVO_V_B16D2)                                               , kRWI_W    , F(Long)                   , 12 , 2331), // #562
  INST(Raddhn_v         , ISimdVVV           , (0b0010111000100000010000, kVO_V_B8H4S2)                                              , kRWI_W    , F(Narrow)                 , 13 , 2344), // #563
  INST(Raddhn2_v        , ISimdVVV           , (0b0110111000100000010000, kVO_V_B16H8S4)                                             , kRWI_X    , F(Narrow)                 , 14 , 2351), // #564
  INST(Rax1_v           , ISimdVVV           , (0b1100111001100000100011, kVO_V_D2)                                                  , kRWI_W    , 0                         , 15 , 2359), // #565
  INST(Rbit_v           , ISimdVV            , (0b0010111001100000010110, kVO_V_B)                                                   , kRWI_W    , 0                         , 7  , 2364), // #566
  INST(Rev16_v          , ISimdVV            , (0b0000111000100000000110, kVO_V_B)                                                   , kRWI_W    , 0                         , 8  , 2373), // #567
  INST(Rev32_v          , ISimdVV            , (0b0010111000100000000010, kVO_V_BH)                                                  , kRWI_W    , 0                         , 9  , 2379), // #568
  INST(Rev64_v          , ISimdVV            , (0b0000111000100000000010, kVO_V_BHS)                                                 , kRWI_W    , 0                         , 10 , 2385), // #569
  INST(Rshrn_v          , SimdShift          , (0b0000000000000000000000, 0b0000111100000000100011, 1, kVO_V_B8H4S2)                 , kRWI_W    , F(Narrow)                 , 0  , 2960), // #570
  INST(Rshrn2_v         , SimdShift          , (0b0000000000000000000000, 0b0100111100000000100011, 1, kVO_V_B16H8S4)                , kRWI_X    , F(Narrow)                 , 1  , 2968), // #571
  INST(Rsubhn_v         , ISimdVVV           , (0b0010111000100000011000, kVO_V_B8H4S2)                                              , kRWI_W    , F(Narrow)                 , 16 , 2400), // #572
  INST(Rsubhn2_v        , ISimdVVV           , (0b0110111000100000011000, kVO_V_B16H8S4)                                             , kRWI_X    , F(Narrow)                 , 17 , 2407), // #573
  INST(Saba_v           , ISimdVVV           , (0b0000111000100000011111, kVO_V_BHS)                                                 , kRWI_X    , 0                         , 18 , 2415), // #574
  INST(Sabal_v          , ISimdVVV           , (0b0000111000100000010100, kVO_V_B8H4S2)                                              , kRWI_X    , F(Long)                   , 19 , 2420), // #575
  INST(Sabal2_v         , ISimdVVV           , (0b0100111000100000010100, kVO_V_B16H8S4)                                             , kRWI_X    , F(Long)                   , 20 , 2426), // #576
  INST(Sabd_v           , ISimdVVV           , (0b0000111000100000011101, kVO_V_BHS)                                                 , kRWI_W    , 0                         , 21 , 2433), // #577
  INST(Sabdl_v          , ISimdVVV           , (0b0000111000100000011100, kVO_V_B8H4S2)                                              , kRWI_W    , F(Long)                   , 22 , 2438), // #578
  INST(Sabdl2_v         , ISimdVVV           , (0b0100111000100000011100, kVO_V_B16H8S4)                                             , kRWI_W    , F(Long)                   , 23 , 2444), // #579
  INST(Sadalp_v         , ISimdVV            , (0b0000111000100000011010, kVO_V_BHS)                                                 , kRWI_X    , F(Long) | F(Pair)         , 11 , 2451), // #580
  INST(Saddl_v          , ISimdVVV           , (0b0000111000100000000000, kVO_V_B8H4S2)                                              , kRWI_W    , F(Long)                   , 24 , 2458), // #581
  INST(Saddl2_v         , ISimdVVV           , (0b0100111000100000000000, kVO_V_B16H8S4)                                             , kRWI_W    , F(Long)                   , 25 , 2464), // #582
  INST(Saddlp_v         , ISimdVV            , (0b0000111000100000001010, kVO_V_BHS)                                                 , kRWI_W    , F(Long) | F(Pair)         , 12 , 2471), // #583
  INST(Saddlv_v         , ISimdSV            , (0b0000111000110000001110, kVO_V_BH_4S)                                               , kRWI_W    , F(Long)                   , 1  , 2478), // #584
  INST(Saddw_v          , ISimdWWV           , (0b0000111000100000000100, kVO_V_B8H4S2)                                              , kRWI_W    , 0                         , 0  , 2485), // #585
  INST(Saddw2_v         , ISimdWWV           , (0b0000111000100000000100, kVO_V_B16H8S4)                                             , kRWI_W    , 0                         , 1  , 2491), // #586
  INST(Scvtf_v          , SimdFcvtSV         , (0b0000111000100001110110, 0b0000111100000000111001, 0b0001111000100010000000, 0)     , kRWI_W    , 0                         , 10 , 2523), // #587
  INST(Sdot_v           , SimdDot            , (0b0000111010000000100101, 0b0000111110000000111000, kET_S, kET_B, kET_4B)            , kRWI_X    , 0                         , 1  , 4218), // #588
  INST(Sha1c_v          , ISimdVVVx          , (0b0101111000000000000000, kOp_Q, kOp_S, kOp_V4S)                                     , kRWI_X    , 0                         , 1  , 2556), // #589
  INST(Sha1h_v          , ISimdVVx           , (0b0101111000101000000010, kOp_S, kOp_S)                                              , kRWI_W    , 0                         , 8  , 2562), // #590
  INST(Sha1m_v          , ISimdVVVx          , (0b0101111000000000001000, kOp_Q, kOp_S, kOp_V4S)                                     , kRWI_X    , 0                         , 2  , 2568), // #591
  INST(Sha1p_v          , ISimdVVVx          , (0b0101111000000000000100, kOp_Q, kOp_S, kOp_V4S)                                     , kRWI_X    , 0                         , 3  , 2574), // #592
  INST(Sha1su0_v        , ISimdVVVx          , (0b0101111000000000001100, kOp_V4S, kOp_V4S, kOp_V4S)                                 , kRWI_X    , 0                         , 4  , 2580), // #593
  INST(Sha1su1_v        , ISimdVVx           , (0b0101111000101000000110, kOp_V4S, kOp_V4S)                                          , kRWI_X    , 0                         , 9  , 2588), // #594
  INST(Sha256h_v        , ISimdVVVx          , (0b0101111000000000010000, kOp_Q, kOp_Q, kOp_V4S)                                     , kRWI_X    , 0                         , 5  , 2596), // #595
  INST(Sha256h2_v       , ISimdVVVx          , (0b0101111000000000010100, kOp_Q, kOp_Q, kOp_V4S)                                     , kRWI_X    , 0                         , 6  , 2604), // #596
  INST(Sha256su0_v      , ISimdVVx           , (0b0101111000101000001010, kOp_V4S, kOp_V4S)                                          , kRWI_X    , 0                         , 10 , 2613), // #597
  INST(Sha256su1_v      , ISimdVVVx          , (0b0101111000000000011000, kOp_V4S, kOp_V4S, kOp_V4S)                                 , kRWI_X    , 0                         , 7  , 2623), // #598
  INST(Sha512h_v        , ISimdVVVx          , (0b1100111001100000100000, kOp_Q, kOp_Q, kOp_V2D)                                     , kRWI_X    , 0                         , 8  , 2633), // #599
  INST(Sha512h2_v       , ISimdVVVx          , (0b1100111001100000100001, kOp_Q, kOp_Q, kOp_V2D)                                     , kRWI_X    , 0                         , 9  , 2641), // #600
  INST(Sha512su0_v      , ISimdVVx           , (0b1100111011000000100000, kOp_V2D, kOp_V2D)                                          , kRWI_X    , 0                         , 11 , 2650), // #601
  INST(Sha512su1_v      , ISimdVVVx          , (0b1100111001100000100010, kOp_V2D, kOp_V2D, kOp_V2D)                                 , kRWI_X    , 0                         , 10 , 2660), // #602
  INST(Shadd_v          , ISimdVVV           , (0b0000111000100000000001, kVO_V_BHS)                                                 , kRWI_W    , 0                         , 26 , 2670), // #603
  INST(Shl_v            , SimdShift          , (0b0000000000000000000000, 0b0000111100000000010101, 0, kVO_V_Any)                    , kRWI_W    , 0                         , 2  , 2954), // #604
  INST(Shll_v           , SimdShiftES        , (0b0010111000100001001110, kVO_V_B8H4S2)                                              , kRWI_W    , F(Long)                   , 0  , 3108), // #605
  INST(Shll2_v          , SimdShiftES        , (0b0110111000100001001110, kVO_V_B16H8S4)                                             , kRWI_W    , F(Long)                   , 1  , 3114), // #606
  INST(Shrn_v           , SimdShift          , (0b0000000000000000000000, 0b0000111100000000100001, 1, kVO_V_B8H4S2)                 , kRWI_W    , F(Narrow)                 , 3  , 2961), // #607
  INST(Shrn2_v          , SimdShift          , (0b0000000000000000000000, 0b0100111100000000100001, 1, kVO_V_B16H8S4)                , kRWI_X    , F(Narrow)                 , 4  , 2969), // #608
  INST(Shsub_v          , ISimdVVV           , (0b0000111000100000001001, kVO_V_BHS)                                                 , kRWI_W    , 0                         , 27 , 2676), // #609
  INST(Sli_v            , SimdShift          , (0b0000000000000000000000, 0b0010111100000000010101, 0, kVO_V_Any)                    , kRWI_X    , 0                         , 5  , 2682), // #610
  INST(Sm3partw1_v      , ISimdVVVx          , (0b1100111001100000110000, kOp_V4S, kOp_V4S, kOp_V4S)                                 , kRWI_X    , 0                         , 11 , 2686), // #611
  INST(Sm3partw2_v      , ISimdVVVx          , (0b1100111001100000110001, kOp_V4S, kOp_V4S, kOp_V4S)                                 , kRWI_X    , 0                         , 12 , 2696), // #612
  INST(Sm3ss1_v         , ISimdVVVVx         , (0b1100111001000000000000, kOp_V4S, kOp_V4S, kOp_V4S, kOp_V4S)                        , kRWI_W    , 0                         , 0  , 2706), // #613
  INST(Sm3tt1a_v        , SimdSm3tt          , (0b1100111001000000100000)                                                            , kRWI_X    , 0                         , 0  , 2713), // #614
  INST(Sm3tt1b_v        , SimdSm3tt          , (0b1100111001000000100001)                                                            , kRWI_X    , 0                         , 1  , 2721), // #615
  INST(Sm3tt2a_v        , SimdSm3tt          , (0b1100111001000000100010)                                                            , kRWI_X    , 0                         , 2  , 2729), // #616
  INST(Sm3tt2b_v        , SimdSm3tt          , (0b1100111001000000100011)                                                            , kRWI_X    , 0                         , 3  , 2737), // #617
  INST(Sm4e_v           , ISimdVVx           , (0b1100111011000000100001, kOp_V4S, kOp_V4S)                                          , kRWI_X    , 0                         , 12 , 2745), // #618
  INST(Sm4ekey_v        , ISimdVVVx          , (0b1100111001100000110010, kOp_V4S, kOp_V4S, kOp_V4S)                                 , kRWI_X    , 0                         , 13 , 2750), // #619
  INST(Smax_v           , ISimdVVV           , (0b0000111000100000011001, kVO_V_BHS)                                                 , kRWI_W    , 0                         , 28 , 1690), // #620
  INST(Smaxp_v          , ISimdVVV           , (0b0000111000100000101001, kVO_V_BHS)                                                 , kRWI_W    , 0                         , 29 , 2765), // #621
  INST(Smaxv_v          , ISimdSV            , (0b0000111000110000101010, kVO_V_BH_4S)                                               , kRWI_W    , 0                         , 2  , 2771), // #622
  INST(Smin_v           , ISimdVVV           , (0b0000111000100000011011, kVO_V_BHS)                                                 , kRWI_W    , 0                         , 30 , 1794), // #623
  INST(Sminp_v          , ISimdVVV           , (0b0000111000100000101011, kVO_V_BHS)                                                 , kRWI_W    , 0                         , 31 , 2777), // #624
  INST(Sminv_v          , ISimdSV            , (0b0000111000110001101010, kVO_V_BH_4S)                                               , kRWI_W    , 0                         , 3  , 2783), // #625
  INST(Smlal_v          , ISimdVVVe          , (0b0000111000100000100000, kVO_V_B8H4S2, 0b0000111100000000001000, kVO_V_H4S2)        , kRWI_X    , F(Long) | F(VH0_15)       , 3  , 2789), // #626
  INST(Smlal2_v         , ISimdVVVe          , (0b0100111000100000100000, kVO_V_B16H8S4, 0b0100111100000000001000, kVO_V_H8S4)       , kRWI_X    , F(Long) | F(VH0_15)       , 4  , 2795), // #627
  INST(Smlsl_v          , ISimdVVVe          , (0b0000111000100000101000, kVO_V_B8H4S2, 0b0000111100000000011000, kVO_V_H4S2)        , kRWI_X    , F(Long) | F(VH0_15)       , 5  , 2802), // #628
  INST(Smlsl2_v         , ISimdVVVe          , (0b0100111000100000101000, kVO_V_B16H8S4, 0b0100111100000000011000, kVO_V_H8S4)       , kRWI_X    , F(Long) | F(VH0_15)       , 6  , 2808), // #629
  INST(Smmla_v          , ISimdVVVx          , (0b0100111010000000101001, kOp_V4S, kOp_V16B, kOp_V16B)                               , kRWI_X    , 0                         , 14 , 4247), // #630
  INST(Smov_v           , SimdSmovUmov       , (0b0000111000000000001011, kVO_V_BHS, 1)                                              , kRWI_W    , 0                         , 0  , 2822), // #631
  INST(Smull_v          , ISimdVVVe          , (0b0000111000100000110000, kVO_V_B8H4S2, 0b0000111100000000101000, kVO_V_H4S2)        , kRWI_W    , F(Long) | F(VH0_15)       , 7  , 2840), // #632
  INST(Smull2_v         , ISimdVVVe          , (0b0100111000100000110000, kVO_V_B16H8S4, 0b0100111100000000101000, kVO_V_H8S4)       , kRWI_W    , F(Long) | F(VH0_15)       , 8  , 2846), // #633
  INST(Sqabs_v          , ISimdVV            , (0b0000111000100000011110, kVO_SV_Any)                                                , kRWI_W    , 0                         , 13 , 2853), // #634
  INST(Sqadd_v          , ISimdVVV           , (0b0000111000100000000011, kVO_SV_Any)                                                , kRWI_W    , 0                         , 32 , 4254), // #635
  INST(Sqdmlal_v        , ISimdVVVe          , (0b0000111000100000100100, kVO_SV_BHS, 0b0000111100000000001100, kVO_V_H4S2)          , kRWI_X    , F(Long) | F(VH0_15)       , 9  , 2859), // #636
  INST(Sqdmlal2_v       , ISimdVVVe          , (0b0100111000100000100100, kVO_V_B16H8S4, 0b0100111100000000001100, kVO_V_H8S4)       , kRWI_X    , F(Long) | F(VH0_15)       , 10 , 2867), // #637
  INST(Sqdmlsl_v        , ISimdVVVe          , (0b0000111000100000101100, kVO_SV_BHS, 0b0000111100000000011100, kVO_V_H4S2)          , kRWI_X    , F(Long) | F(VH0_15)       , 11 , 2876), // #638
  INST(Sqdmlsl2_v       , ISimdVVVe          , (0b0100111000100000101100, kVO_V_B16H8S4, 0b0100111100000000011100, kVO_V_H8S4)       , kRWI_X    , F(Long) | F(VH0_15)       , 12 , 2884), // #639
  INST(Sqdmulh_v        , ISimdVVVe          , (0b0000111000100000101101, kVO_SV_HS, 0b0000111100000000110000, kVO_SV_HS)            , kRWI_W    , F(VH0_15)                 , 13 , 2893), // #640
  INST(Sqdmull_v        , ISimdVVVe          , (0b0000111000100000110100, kVO_SV_BHS, 0b0000111100000000101100, kVO_V_H4S2)          , kRWI_W    , F(Long) | F(VH0_15)       , 14 , 2901), // #641
  INST(Sqdmull2_v       , ISimdVVVe          , (0b0100111000100000110100, kVO_V_B16H8S4, 0b0100111100000000101100, kVO_V_H8S4)       , kRWI_W    , F(Long) | F(VH0_15)       , 15 , 2909), // #642
  INST(Sqneg_v          , ISimdVV            , (0b0010111000100000011110, kVO_SV_Any)                                                , kRWI_W    , 0                         , 14 , 2918), // #643
  INST(Sqrdmlah_v       , ISimdVVVe          , (0b0010111000000000100001, kVO_SV_HS, 0b0010111100000000110100, kVO_SV_HS)            , kRWI_X    , F(VH0_15)                 , 16 , 2924), // #644
  INST(Sqrdmlsh_v       , ISimdVVVe          , (0b0010111000000000100011, kVO_SV_HS, 0b0010111100000000111100, kVO_SV_HS)            , kRWI_X    , F(VH0_15)                 , 17 , 2933), // #645
  INST(Sqrdmulh_v       , ISimdVVVe          , (0b0010111000100000101101, kVO_SV_HS, 0b0000111100000000110100, kVO_SV_HS)            , kRWI_W    , F(VH0_15)                 , 18 , 2942), // #646
  INST(Sqrshl_v         , SimdShift          , (0b0000111000100000010111, 0b0000000000000000000000, 1, kVO_SV_Any)                   , kRWI_W    , 0                         , 6  , 2951), // #647
  INST(Sqrshrn_v        , SimdShift          , (0b0000000000000000000000, 0b0000111100000000100111, 1, kVO_SV_B8H4S2)                , kRWI_W    , F(Narrow)                 , 7  , 2958), // #648
  INST(Sqrshrn2_v       , SimdShift          , (0b0000000000000000000000, 0b0100111100000000100111, 1, kVO_V_B16H8S4)                , kRWI_X    , F(Narrow)                 , 8  , 2966), // #649
  INST(Sqrshrun_v       , SimdShift          , (0b0000000000000000000000, 0b0010111100000000100011, 1, kVO_SV_B8H4S2)                , kRWI_W    , F(Narrow)                 , 9  , 2975), // #650
  INST(Sqrshrun2_v      , SimdShift          , (0b0000000000000000000000, 0b0110111100000000100011, 1, kVO_V_B16H8S4)                , kRWI_X    , F(Narrow)                 , 10 , 2984), // #651
  INST(Sqshl_v          , SimdShift          , (0b0000111000100000010011, 0b0000111100000000011101, 0, kVO_SV_Any)                   , kRWI_W    , 0                         , 11 , 2994), // #652
  INST(Sqshlu_v         , SimdShift          , (0b0000000000000000000000, 0b0010111100000000011001, 0, kVO_SV_Any)                   , kRWI_W    , 0                         , 12 , 3000), // #653
  INST(Sqshrn_v         , SimdShift          , (0b0000000000000000000000, 0b0000111100000000100101, 1, kVO_SV_B8H4S2)                , kRWI_W    , F(Narrow)                 , 13 , 3007), // #654
  INST(Sqshrn2_v        , SimdShift          , (0b0000000000000000000000, 0b0100111100000000100101, 1, kVO_V_B16H8S4)                , kRWI_X    , F(Narrow)                 , 14 , 3014), // #655
  INST(Sqshrun_v        , SimdShift          , (0b0000000000000000000000, 0b0010111100000000100001, 1, kVO_SV_B8H4S2)                , kRWI_W    , F(Narrow)                 , 15 , 3022), // #656
  INST(Sqshrun2_v       , SimdShift          , (0b0000000000000000000000, 0b0110111100000000100001, 1, kVO_V_B16H8S4)                , kRWI_X    , F(Narrow)                 , 16 , 3030), // #657
  INST(Sqsub_v          , ISimdVVV           , (0b0000111000100000001011, kVO_SV_Any)                                                , kRWI_W    , 0                         , 33 , 3039), // #658
  INST(Sqxtn_v          , ISimdVV            , (0b0000111000100001010010, kVO_SV_B8H4S2)                                             , kRWI_W    , F(Narrow)                 , 15 , 3045), // #659
  INST(Sqxtn2_v         , ISimdVV            , (0b0100111000100001010010, kVO_V_B16H8S4)                                             , kRWI_X    , F(Narrow)                 , 16 , 3051), // #660
  INST(Sqxtun_v         , ISimdVV            , (0b0010111000100001001010, kVO_SV_B8H4S2)                                             , kRWI_W    , F(Narrow)                 , 17 , 3058), // #661
  INST(Sqxtun2_v        , ISimdVV            , (0b0110111000100001001010, kVO_V_B16H8S4)                                             , kRWI_X    , F(Narrow)                 , 18 , 3065), // #662
  INST(Srhadd_v         , ISimdVVV           , (0b0000111000100000000101, kVO_V_BHS)                                                 , kRWI_W    , 0                         , 34 , 3073), // #663
  INST(Sri_v            , SimdShift          , (0b0000000000000000000000, 0b0010111100000000010001, 1, kVO_V_Any)                    , kRWI_W    , 0                         , 17 , 3080), // #664
  INST(Srshl_v          , SimdShift          , (0b0000111000100000010101, 0b0000000000000000000000, 0, kVO_V_Any)                    , kRWI_W    , 0                         , 18 , 3084), // #665
  INST(Srshr_v          , SimdShift          , (0b0000000000000000000000, 0b0000111100000000001001, 1, kVO_V_Any)                    , kRWI_W    , 0                         , 19 , 3090), // #666
  INST(Srsra_v          , SimdShift          , (0b0000000000000000000000, 0b0000111100000000001101, 1, kVO_V_Any)                    , kRWI_X    , 0                         , 20 , 3096), // #667
  INST(Sshl_v           , SimdShift          , (0b0000111000100000010001, 0b0000000000000000000000, 0, kVO_V_Any)                    , kRWI_W    , 0                         , 21 , 3102), // #668
  INST(Sshll_v          , SimdShift          , (0b0000000000000000000000, 0b0000111100000000101001, 0, kVO_V_B8H4S2)                 , kRWI_W    , F(Long)                   , 22 , 3107), // #669
  INST(Sshll2_v         , SimdShift          , (0b0000000000000000000000, 0b0100111100000000101001, 0, kVO_V_B16H8S4)                , kRWI_W    , F(Long)                   , 23 , 3113), // #670
  INST(Sshr_v           , SimdShift          , (0b0000000000000000000000, 0b0000111100000000000001, 1, kVO_V_Any)                    , kRWI_W    , 0                         , 24 , 3120), // #671
  INST(Ssra_v           , SimdShift          , (0b0000000000000000000000, 0b0000111100000000000101, 1, kVO_V_Any)                    , kRWI_X    , 0                         , 25 , 3125), // #672
  INST(Ssubl_v          , ISimdVVV           , (0b0000111000100000001000, kVO_V_B8H4S2)                                              , kRWI_W    , F(Long)                   , 35 , 3130), // #673
  INST(Ssubl2_v         , ISimdVVV           , (0b0100111000100000001000, kVO_V_B16H8S4)                                             , kRWI_W    , F(Long)                   , 36 , 3136), // #674
  INST(Ssubw_v          , ISimdWWV           , (0b0000111000100000001100, kVO_V_B8H4S2)                                              , kRWI_W    , 0                         , 2  , 3143), // #675
  INST(Ssubw2_v         , ISimdWWV           , (0b0000111000100000001100, kVO_V_B16H8S4)                                             , kRWI_X    , 0                         , 3  , 3149), // #676
  INST(St1_v            , SimdLdNStN         , (0b0000110100000000000000, 0b0000110000000000001000, 1, 0)                            , kRWI_STn  , F(Consecutive)            , 8  , 3156), // #677
  INST(St2_v            , SimdLdNStN         , (0b0000110100100000000000, 0b0000110000000000100000, 2, 0)                            , kRWI_STn  , F(Consecutive)            , 9  , 3160), // #678
  INST(St3_v            , SimdLdNStN         , (0b0000110100000000001000, 0b0000110000000000010000, 3, 0)                            , kRWI_STn  , F(Consecutive)            , 10 , 3169), // #679
  INST(St4_v            , SimdLdNStN         , (0b0000110100100000001000, 0b0000110000000000000000, 4, 0)                            , kRWI_STn  , F(Consecutive)            , 11 , 3173), // #680
  INST(Stnp_v           , SimdLdpStp         , (0b0010110000, 0b0000000000)                                                          , kRWI_RRW  , 0                         , 2  , 3383), // #681
  INST(Stp_v            , SimdLdpStp         , (0b0010110100, 0b0010110010)                                                          , kRWI_RRW  , 0                         , 3  , 3388), // #682
  INST(Str_v            , SimdLdSt           , (0b0011110100, 0b00111100000, 0b00111100001, 0b00000000, Inst::kIdStur_v)             , kRWI_RW   , 0                         , 1  , 3392), // #683
  INST(Stur_v           , SimdLdurStur       , (0b0011110000000000000000)                                                            , kRWI_RW   , 0                         , 1  , 3662), // #684
  INST(Sub_v            , ISimdVVV           , (0b0010111000100000100001, kVO_V_Any)                                                 , kRWI_W    , 0                         , 37 , 985 ), // #685
  INST(Subhn_v          , ISimdVVV           , (0b0000111000100000011000, kVO_V_B8H4S2)                                              , kRWI_W    , F(Narrow)                 , 38 , 2401), // #686
  INST(Subhn2_v         , ISimdVVV           , (0b0000111000100000011000, kVO_V_B16H8S4)                                             , kRWI_X    , F(Narrow)                 , 39 , 2408), // #687
  INST(Sudot_v          , SimdDot            , (0b0000000000000000000000, 0b0000111100000000111100, kET_S, kET_B, kET_4B)            , kRWI_X    , 0                         , 2  , 3739), // #688
  INST(Suqadd_v         , ISimdVV            , (0b0000111000100000001110, kVO_SV_Any)                                                , kRWI_X    , 0                         , 19 , 3745), // #689
  INST(Sxtl_v           , SimdSxtlUxtl       , (0b0000111100000000101001, kVO_V_B8H4S2)                                              , kRWI_W    , F(Long)                   , 0  , 3834), // #690
  INST(Sxtl2_v          , SimdSxtlUxtl       , (0b0100111100000000101001, kVO_V_B16H8S4)                                             , kRWI_W    , F(Long)                   , 1  , 3839), // #691
  INST(Tbl_v            , SimdTblTbx         , (0b0000111000000000000000)                                                            , kRWI_W    , 0                         , 0  , 3854), // #692
  INST(Tbx_v            , SimdTblTbx         , (0b0000111000000000000100)                                                            , kRWI_W    , 0                         , 1  , 3863), // #693
  INST(Trn1_v           , ISimdVVV           , (0b0000111000000000001010, kVO_V_BHS_D2)                                              , kRWI_W    , 0                         , 40 , 3876), // #694
  INST(Trn2_v           , ISimdVVV           , (0b0000111000000000011010, kVO_V_BHS_D2)                                              , kRWI_W    , 0                         , 41 , 3881), // #695
  INST(Uaba_v           , ISimdVVV           , (0b0010111000100000011111, kVO_V_BHS)                                                 , kRWI_X    , 0                         , 42 , 3886), // #696
  INST(Uabal_v          , ISimdVVV           , (0b0010111000100000010100, kVO_V_B8H4S2)                                              , kRWI_X    , F(Long)                   , 43 , 3891), // #697
  INST(Uabal2_v         , ISimdVVV           , (0b0110111000100000010100, kVO_V_B16H8S4)                                             , kRWI_X    , F(Long)                   , 44 , 3897), // #698
  INST(Uabd_v           , ISimdVVV           , (0b0010111000100000011101, kVO_V_BHS)                                                 , kRWI_W    , 0                         , 45 , 3904), // #699
  INST(Uabdl_v          , ISimdVVV           , (0b0010111000100000011100, kVO_V_B8H4S2)                                              , kRWI_W    , F(Long)                   , 46 , 3909), // #700
  INST(Uabdl2_v         , ISimdVVV           , (0b0110111000100000011100, kVO_V_B16H8S4)                                             , kRWI_W    , F(Long)                   , 47 , 3915), // #701
  INST(Uadalp_v         , ISimdVV            , (0b0010111000100000011010, kVO_V_BHS)                                                 , kRWI_X    , F(Long) | F(Pair)         , 20 , 3922), // #702
  INST(Uaddl_v          , ISimdVVV           , (0b0010111000100000000000, kVO_V_B8H4S2)                                              , kRWI_W    , F(Long)                   , 48 , 3929), // #703
  INST(Uaddl2_v         , ISimdVVV           , (0b0110111000100000000000, kVO_V_B16H8S4)                                             , kRWI_W    , F(Long)                   , 49 , 3935), // #704
  INST(Uaddlp_v         , ISimdVV            , (0b0010111000100000001010, kVO_V_BHS)                                                 , kRWI_W    , F(Long) | F(Pair)         , 21 , 3942), // #705
  INST(Uaddlv_v         , ISimdSV            , (0b0010111000110000001110, kVO_V_BH_4S)                                               , kRWI_W    , F(Long)                   , 4  , 3949), // #706
  INST(Uaddw_v          , ISimdWWV           , (0b0010111000100000000100, kVO_V_B8H4S2)                                              , kRWI_W    , 0                         , 4  , 3956), // #707
  INST(Uaddw2_v         , ISimdWWV           , (0b0010111000100000000100, kVO_V_B16H8S4)                                             , kRWI_W    , 0                         , 5  , 3962), // #708
  INST(Ucvtf_v          , SimdFcvtSV         , (0b0010111000100001110110, 0b0010111100000000111001, 0b0001111000100011000000, 0)     , kRWI_W    , 0                         , 11 , 3985), // #709
  INST(Udot_v           , SimdDot            , (0b0010111010000000100101, 0b0010111110000000111000, kET_S, kET_B, kET_4B)            , kRWI_X    , 0                         , 3  , 3740), // #710
  INST(Uhadd_v          , ISimdVVV           , (0b0010111000100000000001, kVO_V_BHS)                                                 , kRWI_W    , 0                         , 50 , 4000), // #711
  INST(Uhsub_v          , ISimdVVV           , (0b0010111000100000001001, kVO_V_BHS)                                                 , kRWI_W    , 0                         , 51 , 4006), // #712
  INST(Umax_v           , ISimdVVV           , (0b0010111000100000011001, kVO_V_BHS)                                                 , kRWI_W    , 0                         , 52 , 1936), // #713
  INST(Umaxp_v          , ISimdVVV           , (0b0010111000100000101001, kVO_V_BHS)                                                 , kRWI_W    , 0                         , 53 , 4019), // #714
  INST(Umaxv_v          , ISimdSV            , (0b0010111000110000101010, kVO_V_BH_4S)                                               , kRWI_W    , 0                         , 5  , 4025), // #715
  INST(Umin_v           , ISimdVVV           , (0b0010111000100000011011, kVO_V_BHS)                                                 , kRWI_W    , 0                         , 54 , 2040), // #716
  INST(Uminp_v          , ISimdVVV           , (0b0010111000100000101011, kVO_V_BHS)                                                 , kRWI_W    , 0                         , 55 , 4031), // #717
  INST(Uminv_v          , ISimdSV            , (0b0010111000110001101010, kVO_V_BH_4S)                                               , kRWI_W    , 0                         , 6  , 4037), // #718
  INST(Umlal_v          , ISimdVVVe          , (0b0010111000100000100000, kVO_V_B8H4S2, 0b0010111100000000001000, kVO_V_H4S2)        , kRWI_X    , F(Long) | F(VH0_15)       , 19 , 4043), // #719
  INST(Umlal2_v         , ISimdVVVe          , (0b0110111000100000100000, kVO_V_B16H8S4, 0b0010111100000000001000, kVO_V_H8S4)       , kRWI_X    , F(Long) | F(VH0_15)       , 20 , 4049), // #720
  INST(Umlsl_v          , ISimdVVVe          , (0b0010111000100000101000, kVO_V_B8H4S2, 0b0010111100000000011000, kVO_V_H4S2)        , kRWI_X    , F(Long) | F(VH0_15)       , 21 , 4056), // #721
  INST(Umlsl2_v         , ISimdVVVe          , (0b0110111000100000101000, kVO_V_B16H8S4, 0b0110111100000000011000, kVO_V_H8S4)       , kRWI_X    , F(Long) | F(VH0_15)       , 22 , 4062), // #722
  INST(Ummla_v          , ISimdVVVx          , (0b0110111010000000101001, kOp_V4S, kOp_V16B, kOp_V16B)                               , kRWI_X    , 0                         , 15 , 4069), // #723
  INST(Umov_v           , SimdSmovUmov       , (0b0000111000000000001111, kVO_V_Any, 0)                                              , kRWI_W    , 0                         , 1  , 4082), // #724
  INST(Umull_v          , ISimdVVVe          , (0b0010111000100000110000, kVO_V_B8H4S2, 0b0010111100000000101000, kVO_V_H4S2)        , kRWI_W    , F(Long) | F(VH0_15)       , 23 , 4100), // #725
  INST(Umull2_v         , ISimdVVVe          , (0b0110111000100000110000, kVO_V_B16H8S4, 0b0110111100000000101000, kVO_V_H8S4)       , kRWI_W    , F(Long) | F(VH0_15)       , 24 , 4106), // #726
  INST(Uqadd_v          , ISimdVVV           , (0b0010111000100000000011, kVO_SV_Any)                                                , kRWI_W    , 0                         , 56 , 3746), // #727
  INST(Uqrshl_v         , SimdShift          , (0b0010111000100000010111, 0b0000000000000000000000, 0, kVO_SV_Any)                   , kRWI_W    , 0                         , 26 , 4113), // #728
  INST(Uqrshrn_v        , SimdShift          , (0b0000000000000000000000, 0b0010111100000000100111, 1, kVO_SV_B8H4S2)                , kRWI_W    , F(Narrow)                 , 27 , 4120), // #729
  INST(Uqrshrn2_v       , SimdShift          , (0b0000000000000000000000, 0b0110111100000000100111, 1, kVO_V_B16H8S4)                , kRWI_X    , F(Narrow)                 , 28 , 4128), // #730
  INST(Uqshl_v          , SimdShift          , (0b0010111000100000010011, 0b0010111100000000011101, 0, kVO_SV_Any)                   , kRWI_W    , 0                         , 29 , 4137), // #731
  INST(Uqshrn_v         , SimdShift          , (0b0000000000000000000000, 0b0010111100000000100101, 1, kVO_SV_B8H4S2)                , kRWI_W    , F(Narrow)                 , 30 , 4143), // #732
  INST(Uqshrn2_v        , SimdShift          , (0b0000000000000000000000, 0b0110111100000000100101, 1, kVO_V_B16H8S4)                , kRWI_X    , F(Narrow)                 , 31 , 4150), // #733
  INST(Uqsub_v          , ISimdVVV           , (0b0010111000100000001011, kVO_SV_Any)                                                , kRWI_W    , 0                         , 57 , 4158), // #734
  INST(Uqxtn_v          , ISimdVV            , (0b0010111000100001010010, kVO_SV_B8H4S2)                                             , kRWI_W    , F(Narrow)                 , 22 , 4164), // #735
  INST(Uqxtn2_v         , ISimdVV            , (0b0110111000100001010010, kVO_V_B16H8S4)                                             , kRWI_X    , F(Narrow)                 , 23 , 4170), // #736
  INST(Urecpe_v         , ISimdVV            , (0b0000111010100001110010, kVO_V_S)                                                   , kRWI_W    , 0                         , 24 , 4177), // #737
  INST(Urhadd_v         , ISimdVVV           , (0b0010111000100000000101, kVO_V_BHS)                                                 , kRWI_W    , 0                         , 58 , 4184), // #738
  INST(Urshl_v          , SimdShift          , (0b0010111000100000010101, 0b0000000000000000000000, 0, kVO_V_Any)                    , kRWI_W    , 0                         , 32 , 4191), // #739
  INST(Urshr_v          , SimdShift          , (0b0000000000000000000000, 0b0010111100000000001001, 1, kVO_V_Any)                    , kRWI_W    , 0                         , 33 , 4197), // #740
  INST(Ursqrte_v        , ISimdVV            , (0b0010111010100001110010, kVO_V_S)                                                   , kRWI_W    , 0                         , 25 , 4203), // #741
  INST(Ursra_v          , SimdShift          , (0b0000000000000000000000, 0b0010111100000000001101, 1, kVO_V_Any)                    , kRWI_X    , 0                         , 34 , 4211), // #742
  INST(Usdot_v          , SimdDot            , (0b0000111010000000100111, 0b0000111110000000111100, kET_S, kET_B, kET_4B)            , kRWI_X    , 0                         , 4  , 4217), // #743
  INST(Ushl_v           , SimdShift          , (0b0010111000100000010001, 0b0000000000000000000000, 0, kVO_V_Any)                    , kRWI_W    , 0                         , 35 , 4223), // #744
  INST(Ushll_v          , SimdShift          , (0b0000000000000000000000, 0b0010111100000000101001, 0, kVO_V_B8H4S2)                 , kRWI_W    , F(Long)                   , 36 , 4228), // #745
  INST(Ushll2_v         , SimdShift          , (0b0000000000000000000000, 0b0110111100000000101001, 0, kVO_V_B16H8S4)                , kRWI_W    , F(Long)                   , 37 , 4234), // #746
  INST(Ushr_v           , SimdShift          , (0b0000000000000000000000, 0b0010111100000000000001, 1, kVO_V_Any)                    , kRWI_W    , 0                         , 38 , 4241), // #747
  INST(Usmmla_v         , ISimdVVVx          , (0b0100111010000000101011, kOp_V4S, kOp_V16B, kOp_V16B)                               , kRWI_X    , 0                         , 16 , 4246), // #748
  INST(Usqadd_v         , ISimdVV            , (0b0010111000100000001110, kVO_SV_Any)                                                , kRWI_X    , 0                         , 26 , 4253), // #749
  INST(Usra_v           , SimdShift          , (0b0000000000000000000000, 0b0010111100000000000101, 1, kVO_V_Any)                    , kRWI_X    , 0                         , 39 , 4260), // #750
  INST(Usubl_v          , ISimdVVV           , (0b0010111000100000001000, kVO_V_B8H4S2)                                              , kRWI_W    , F(Long)                   , 59 , 4265), // #751
  INST(Usubl2_v         , ISimdVVV           , (0b0110111000100000001000, kVO_V_B16H8S4)                                             , kRWI_W    , F(Long)                   , 60 , 4271), // #752
  INST(Usubw_v          , ISimdWWV           , (0b0010111000100000001100, kVO_V_B8H4S2)                                              , kRWI_W    , 0                         , 6  , 4278), // #753
  INST(Usubw2_v         , ISimdWWV           , (0b0010111000100000001100, kVO_V_B16H8S4)                                             , kRWI_W    , 0                         , 7  , 4284), // #754
  INST(Uxtl_v           , SimdSxtlUxtl       , (0b0010111100000000101001, kVO_V_B8H4S2)                                              , kRWI_W    , F(Long)                   , 2  , 4301), // #755
  INST(Uxtl2_v          , SimdSxtlUxtl       , (0b0110111100000000101001, kVO_V_B16H8S4)                                             , kRWI_W    , F(Long)                   , 3  , 4306), // #756
  INST(Uzp1_v           , ISimdVVV           , (0b0000111000000000000110, kVO_V_BHS_D2)                                              , kRWI_W    , 0                         , 61 , 4312), // #757
  INST(Uzp2_v           , ISimdVVV           , (0b0000111000000000010110, kVO_V_BHS_D2)                                              , kRWI_W    , 0                         , 62 , 4317), // #758
  INST(Xar_v            , ISimdVVVI          , (0b1100111001100000100011, kVO_V_D2, 6, 10, 0)                                        , kRWI_W    , 0                         , 1  , 4337), // #759
  INST(Xtn_v            , ISimdVV            , (0b0000111000100001001010, kVO_V_B8H4S2)                                              , kRWI_W    , F(Narrow)                 , 27 , 3047), // #760
  INST(Xtn2_v           , ISimdVV            , (0b0100111000100001001010, kVO_V_B16H8S4)                                             , kRWI_X    , F(Narrow)                 , 28 , 3053), // #761
  INST(Zip1_v           , ISimdVVV           , (0b0000111000000000001110, kVO_V_BHS_D2)                                              , kRWI_W    , 0                         , 63 , 4367), // #762
  INST(Zip2_v           , ISimdVVV           , (0b0000111000000000011110, kVO_V_BHS_D2)                                              , kRWI_W    , 0                         , 64 , 4372)  // #763
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
  { 0b0011100111, 0b00111000100, 0b00111000101, 0         , kWX, 22, 0, Inst::kIdLdursb }, // ldrsb
  { 0b0111100110, 0b01111000100, 0b01111000101, 0         , kWX, 22, 1, Inst::kIdLdursh }, // ldrsh
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
  { 0b1011100010000000000000, 0b0000000000000000000000, kWX, kZR, 0 , 0 }, // ldursw
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
const char InstDB::_nameData[] =
  "\0" "adc\0" "adcs\0" "addg\0" "adds\0" "addv\0" "adr\0" "adrp\0" "aesd\0" "aese\0" "aesimc\0" "aesmc\0" "and\0"
  "ands\0" "asr\0" "asrv\0" "at\0" "autda\0" "autdb\0" "autdza\0" "autdzb\0" "autia\0" "autia1716\0" "autiasp\0"
  "autiaz\0" "autib\0" "autib1716\0" "autibsp\0" "autibz\0" "autiza\0" "autizb\0" "axflag\0" "bcax\0" "bfc\0" "bfcvt\0"
  "bfcvtn\0" "bfcvtn2\0" "bfdot\0" "bfi\0" "bfmlalb\0" "bfmlalt\0" "bfmmla\0" "bfxil\0" "bic\0" "bics\0" "bif\0"
  "blr\0" "br\0" "brk\0" "bsl\0" "cas\0" "casa\0" "casab\0" "casah\0" "casal\0" "casalb\0" "casalh\0" "casb\0" "cash\0"
  "casl\0" "caslb\0" "caslh\0" "casp\0" "caspa\0" "caspal\0" "caspl\0" "cbnz\0" "cbz\0" "ccmn\0" "cfinv\0" "cinc\0"
  "cinv\0" "clrex\0" "cls\0" "clz\0" "cmhi\0" "cmhs\0" "cmpp\0" "cmtst\0" "cneg\0" "cnt\0" "crc32b\0" "crc32cb\0"
  "crc32ch\0" "crc32cw\0" "crc32cx\0" "crc32h\0" "crc32w\0" "crc32x\0" "csdb\0" "cset\0" "csetm\0" "csinc\0" "csinv\0"
  "csneg\0" "dcps1\0" "dcps2\0" "dcps3\0" "dgh\0" "dmb\0" "drps\0" "dsb\0" "dup\0" "eon\0" "eor3\0" "eret\0" "esb\0"
  "ext\0" "extr\0" "fabd\0" "fabs\0" "facge\0" "facgt\0" "fadd\0" "faddp\0" "fcadd\0" "fccmp\0" "fccmpe\0" "fcmeq\0"
  "fcmge\0" "fcmgt\0" "fcmla\0" "fcmle\0" "fcmlt\0" "fcmp\0" "fcmpe\0" "fcsel\0" "fcvtas\0" "fcvtau\0" "fcvtl\0"
  "fcvtl2\0" "fcvtms\0" "fcvtmu\0" "fcvtns\0" "fcvtnu\0" "fcvtps\0" "fcvtpu\0" "fcvtxn\0" "fcvtxn2\0" "fcvtzs\0"
  "fcvtzu\0" "fdiv\0" "fjcvtzs\0" "fmadd\0" "fmax\0" "fmaxnm\0" "fmaxnmp\0" "fmaxnmv\0" "fmaxp\0" "fmaxv\0" "fmin\0"
  "fminnm\0" "fminnmp\0" "fminnmv\0" "fminp\0" "fminv\0" "fmla\0" "fmlal\0" "fmlal2\0" "fmls\0" "fmlsl\0" "fmlsl2\0"
  "fmov\0" "fmsub\0" "fmul\0" "fmulx\0" "fneg\0" "fnmadd\0" "fnmsub\0" "fnmul\0" "frecpe\0" "frecps\0" "frecpx\0"
  "frint32x\0" "frint32z\0" "frint64x\0" "frint64z\0" "frinta\0" "frinti\0" "frintm\0" "frintn\0" "frintp\0" "frintx\0"
  "frintz\0" "frsqrte\0" "frsqrts\0" "fsqrt\0" "fsub\0" "gmi\0" "hint\0" "hlt\0" "hvc\0" "ins\0" "isb\0" "ld1\0"
  "ld1r\0" "ld2\0" "ld2r\0" "ld3\0" "ld3r\0" "ld4\0" "ld4r\0" "ldadd\0" "ldadda\0" "ldaddab\0" "ldaddah\0" "ldaddal\0"
  "ldaddalb\0" "ldaddalh\0" "ldaddb\0" "ldaddh\0" "ldaddl\0" "ldaddlb\0" "ldaddlh\0" "ldar\0" "ldarb\0" "ldarh\0"
  "ldaxp\0" "ldaxr\0" "ldaxrb\0" "ldaxrh\0" "ldclr\0" "ldclra\0" "ldclrab\0" "ldclrah\0" "ldclral\0" "ldclralb\0"
  "ldclralh\0" "ldclrb\0" "ldclrh\0" "ldclrl\0" "ldclrlb\0" "ldclrlh\0" "ldeor\0" "ldeora\0" "ldeorab\0" "ldeorah\0"
  "ldeoral\0" "ldeoralb\0" "ldeoralh\0" "ldeorb\0" "ldeorh\0" "ldeorl\0" "ldeorlb\0" "ldeorlh\0" "ldg\0" "ldgm\0"
  "ldlar\0" "ldlarb\0" "ldlarh\0" "ldnp\0" "ldp\0" "ldpsw\0" "ldr\0" "ldraa\0" "ldrab\0" "ldrb\0" "ldrh\0" "ldrsb\0"
  "ldrsh\0" "ldrsw\0" "ldset\0" "ldseta\0" "ldsetab\0" "ldsetah\0" "ldsetal\0" "ldsetalb\0" "ldsetalh\0" "ldsetb\0"
  "ldseth\0" "ldsetl\0" "ldsetlb\0" "ldsetlh\0" "ldsmax\0" "ldsmaxa\0" "ldsmaxab\0" "ldsmaxah\0" "ldsmaxal\0"
  "ldsmaxalb\0" "ldsmaxalh\0" "ldsmaxb\0" "ldsmaxh\0" "ldsmaxl\0" "ldsmaxlb\0" "ldsmaxlh\0" "ldsmin\0" "ldsmina\0"
  "ldsminab\0" "ldsminah\0" "ldsminal\0" "ldsminalb\0" "ldsminalh\0" "ldsminb\0" "ldsminh\0" "ldsminl\0" "ldsminlb\0"
  "ldsminlh\0" "ldtr\0" "ldtrb\0" "ldtrh\0" "ldtrsb\0" "ldtrsh\0" "ldtrsw\0" "ldumax\0" "ldumaxa\0" "ldumaxab\0"
  "ldumaxah\0" "ldumaxal\0" "ldumaxalb\0" "ldumaxalh\0" "ldumaxb\0" "ldumaxh\0" "ldumaxl\0" "ldumaxlb\0" "ldumaxlh\0"
  "ldumin\0" "ldumina\0" "lduminab\0" "lduminah\0" "lduminal\0" "lduminalb\0" "lduminalh\0" "lduminb\0" "lduminh\0"
  "lduminl\0" "lduminlb\0" "lduminlh\0" "ldur\0" "ldurb\0" "ldurh\0" "ldursb\0" "ldursh\0" "ldursw\0" "ldxp\0" "ldxr\0"
  "ldxrb\0" "ldxrh\0" "lslv\0" "lsr\0" "lsrv\0" "mneg\0" "movi\0" "movk\0" "movn\0" "movz\0" "mrs\0" "msr\0" "mvn\0"
  "mvni\0" "negs\0" "ngc\0" "ngcs\0" "nop\0" "not\0" "orn\0" "orr\0" "pacda\0" "pacdb\0" "pacdza\0" "pacdzb\0"
  "pacga\0" "pmul\0" "pmull\0" "pmull2\0" "pssbb\0" "raddhn\0" "raddhn2\0" "rax1\0" "rbit\0" "rev\0" "rev16\0"
  "rev32\0" "rev64\0" "ror\0" "rorv\0" "rsubhn\0" "rsubhn2\0" "saba\0" "sabal\0" "sabal2\0" "sabd\0" "sabdl\0"
  "sabdl2\0" "sadalp\0" "saddl\0" "saddl2\0" "saddlp\0" "saddlv\0" "saddw\0" "saddw2\0" "sbc\0" "sbcs\0" "sbfiz\0"
  "sbfm\0" "sbfx\0" "scvtf\0" "sdiv\0" "setf16\0" "setf8\0" "sev\0" "sevl\0" "sha1c\0" "sha1h\0" "sha1m\0" "sha1p\0"
  "sha1su0\0" "sha1su1\0" "sha256h\0" "sha256h2\0" "sha256su0\0" "sha256su1\0" "sha512h\0" "sha512h2\0" "sha512su0\0"
  "sha512su1\0" "shadd\0" "shsub\0" "sli\0" "sm3partw1\0" "sm3partw2\0" "sm3ss1\0" "sm3tt1a\0" "sm3tt1b\0" "sm3tt2a\0"
  "sm3tt2b\0" "sm4e\0" "sm4ekey\0" "smaddl\0" "smaxp\0" "smaxv\0" "sminp\0" "sminv\0" "smlal\0" "smlal2\0" "smlsl\0"
  "smlsl2\0" "smnegl\0" "smov\0" "smsubl\0" "smulh\0" "smull\0" "smull2\0" "sqabs\0" "sqdmlal\0" "sqdmlal2\0"
  "sqdmlsl\0" "sqdmlsl2\0" "sqdmulh\0" "sqdmull\0" "sqdmull2\0" "sqneg\0" "sqrdmlah\0" "sqrdmlsh\0" "sqrdmulh\0"
  "sqrshl\0" "sqrshrn\0" "sqrshrn2\0" "sqrshrun\0" "sqrshrun2\0" "sqshl\0" "sqshlu\0" "sqshrn\0" "sqshrn2\0"
  "sqshrun\0" "sqshrun2\0" "sqsub\0" "sqxtn\0" "sqxtn2\0" "sqxtun\0" "sqxtun2\0" "srhadd\0" "sri\0" "srshl\0" "srshr\0"
  "srsra\0" "sshl\0" "sshll\0" "sshll2\0" "sshr\0" "ssra\0" "ssubl\0" "ssubl2\0" "ssubw\0" "ssubw2\0" "st1\0" "st2\0"
  "st2g\0" "st3\0" "st4\0" "stadd\0" "staddb\0" "staddh\0" "staddl\0" "staddlb\0" "staddlh\0" "stclr\0" "stclrb\0"
  "stclrh\0" "stclrl\0" "stclrlb\0" "stclrlh\0" "steor\0" "steorb\0" "steorh\0" "steorl\0" "steorlb\0" "steorlh\0"
  "stg\0" "stgm\0" "stgp\0" "stllr\0" "stllrb\0" "stllrh\0" "stlr\0" "stlrb\0" "stlrh\0" "stlxp\0" "stlxr\0" "stlxrb\0"
  "stlxrh\0" "stnp\0" "stp\0" "str\0" "strb\0" "strh\0" "stset\0" "stsetb\0" "stseth\0" "stsetl\0" "stsetlb\0"
  "stsetlh\0" "stsmax\0" "stsmaxb\0" "stsmaxh\0" "stsmaxl\0" "stsmaxlb\0" "stsmaxlh\0" "stsmin\0" "stsminb\0"
  "stsminh\0" "stsminl\0" "stsminlb\0" "stsminlh\0" "sttr\0" "sttrb\0" "sttrh\0" "stumax\0" "stumaxb\0" "stumaxh\0"
  "stumaxl\0" "stumaxlb\0" "stumaxlh\0" "stumin\0" "stuminb\0" "stuminh\0" "stuminl\0" "stuminlb\0" "stuminlh\0"
  "stur\0" "sturb\0" "sturh\0" "stxp\0" "stxr\0" "stxrb\0" "stxrh\0" "stz2g\0" "stzg\0" "stzgm\0" "subg\0" "subp\0"
  "subps\0" "subs\0" "sudot\0" "suqadd\0" "svc\0" "swp\0" "swpa\0" "swpab\0" "swpah\0" "swpal\0" "swpalb\0" "swpalh\0"
  "swpb\0" "swph\0" "swpl\0" "swplb\0" "swplh\0" "sxtb\0" "sxth\0" "sxtl\0" "sxtl2\0" "sxtw\0" "sys\0" "tbl\0" "tbnz\0"
  "tbx\0" "tbz\0" "tlbi\0" "trn1\0" "trn2\0" "uaba\0" "uabal\0" "uabal2\0" "uabd\0" "uabdl\0" "uabdl2\0" "uadalp\0"
  "uaddl\0" "uaddl2\0" "uaddlp\0" "uaddlv\0" "uaddw\0" "uaddw2\0" "ubfiz\0" "ubfm\0" "ubfx\0" "ucvtf\0" "udf\0"
  "udiv\0" "uhadd\0" "uhsub\0" "umaddl\0" "umaxp\0" "umaxv\0" "uminp\0" "uminv\0" "umlal\0" "umlal2\0" "umlsl\0"
  "umlsl2\0" "ummla\0" "umnegl\0" "umov\0" "umsubl\0" "umulh\0" "umull\0" "umull2\0" "uqrshl\0" "uqrshrn\0"
  "uqrshrn2\0" "uqshl\0" "uqshrn\0" "uqshrn2\0" "uqsub\0" "uqxtn\0" "uqxtn2\0" "urecpe\0" "urhadd\0" "urshl\0"
  "urshr\0" "ursqrte\0" "ursra\0" "usdot\0" "ushl\0" "ushll\0" "ushll2\0" "ushr\0" "usmmla\0" "usqadd\0" "usra\0"
  "usubl\0" "usubl2\0" "usubw\0" "usubw2\0" "uxtb\0" "uxth\0" "uxtl\0" "uxtl2\0" "uzp1\0" "uzp2\0" "wfe\0" "wfi\0"
  "xaflag\0" "xar\0" "xpacd\0" "xpaci\0" "xpaclri\0" "yield\0" "zip1\0" "zip2";

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
