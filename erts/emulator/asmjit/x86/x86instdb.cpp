// This file is part of AsmJit project <https://asmjit.com>
//
// See asmjit.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

// ----------------------------------------------------------------------------
// IMPORTANT: AsmJit now uses an external instruction database to populate
// static tables within this file. Perform the following steps to regenerate
// all tables enclosed by ${...}:
//
//   1. Install node.js environment <https://nodejs.org>
//   2. Go to asmjit/tools directory
//   3. Get the latest asmdb from <https://github.com/asmjit/asmdb> and
//      copy/link the `asmdb` directory to `asmjit/tools/asmdb`.
//   4. Execute `node tablegen-x86.js`
//
// Instruction encoding and opcodes were added to the `x86inst.cpp` database
// manually in the past and they are not updated by the script as it became
// tricky. However, everything else is updated including instruction operands
// and tables required to validate them, instruction read/write information
// (including registers and flags), and all indexes to all tables.
// ----------------------------------------------------------------------------

#include "../core/api-build_p.h"
#if !defined(ASMJIT_NO_X86)

#include "../core/cpuinfo.h"
#include "../core/misc_p.h"
#include "../core/support.h"
#include "../x86/x86instdb_p.h"
#include "../x86/x86opcode_p.h"
#include "../x86/x86operand.h"

ASMJIT_BEGIN_SUB_NAMESPACE(x86)

// x86::InstDB - InstInfo
// ======================

// Instruction opcode definitions:
//   - `O` encodes X86|MMX|SSE instructions.
//   - `V` encodes VEX|XOP|EVEX instructions.
//   - `E` encodes EVEX instructions only.
#define O_ENCODE(PREFIX, OPCODE, O, L, W, EvexW, N, TT) ((PREFIX) | (OPCODE) | (O) | (L) | (W) | (EvexW) | (N) | (TT))

#define O(PREFIX, OPCODE, ModO, LL, W, EvexW, N, ModRM) (O_ENCODE(Opcode::k##PREFIX, 0x##OPCODE, Opcode::kModO_##ModO, Opcode::kLL_##LL, Opcode::kW_##W, Opcode::kEvex_W_##EvexW, Opcode::kCDSHL_##N, Opcode::kModRM_##ModRM))
#define V(PREFIX, OPCODE, ModO, LL, W, EvexW, N, TT) (O_ENCODE(Opcode::k##PREFIX, 0x##OPCODE, Opcode::kModO_##ModO, Opcode::kLL_##LL, Opcode::kW_##W, Opcode::kEvex_W_##EvexW, Opcode::kCDSHL_##N, Opcode::kCDTT_##TT))
#define E(PREFIX, OPCODE, ModO, LL, W, EvexW, N, TT) (O_ENCODE(Opcode::k##PREFIX, 0x##OPCODE, Opcode::kModO_##ModO, Opcode::kLL_##LL, Opcode::kW_##W, Opcode::kEvex_W_##EvexW, Opcode::kCDSHL_##N, Opcode::kCDTT_##TT) | Opcode::kMM_ForceEvex)
#define O_FPU(PREFIX, OPCODE, ModO) (Opcode::kFPU_##PREFIX | (0x##OPCODE & 0xFFu) | ((0x##OPCODE >> 8) << Opcode::kFPU_2B_Shift) | Opcode::kModO_##ModO)

// Don't store `_nameDataIndex` if instruction names are disabled. Since some
// APIs can use `_nameDataIndex` it's much safer if it's zero if it's not defined.
#ifndef ASMJIT_NO_TEXT
  #define NAME_DATA_INDEX(Index) Index
#else
  #define NAME_DATA_INDEX(Index) 0
#endif

// Defines an X86 instruction.
#define INST(id, encoding, opcode0, opcode1, mainOpcodeIndex, altOpcodeIndex, nameDataIndex, commomInfoIndex, additionalInfoIndex) { \
  uint32_t(NAME_DATA_INDEX(nameDataIndex)), \
  uint32_t(commomInfoIndex),                \
  uint32_t(additionalInfoIndex),            \
  uint8_t(InstDB::kEncoding##encoding),     \
  uint8_t((opcode0) & 0xFFu),               \
  uint8_t(mainOpcodeIndex),                 \
  uint8_t(altOpcodeIndex)                   \
}

const InstDB::InstInfo InstDB::_instInfoTable[] = {
  /*--------------------+--------------------+------------------+--------+------------------+--------+----+----+------+----+----+
  |    Instruction      |    Instruction     |    Main Opcode   |  EVEX  |Alternative Opcode|  EVEX  |Op0X|Op1X|Name-X|IdxA|IdxB|
  |     Id & Name       |      Encoding      |  (pp+mmm|op/o|L|w|W|N|TT.)|--(pp+mmm|op/o|L|w|W|N|TT.)|     (auto-generated)     |
  +---------------------+--------------------+---------+----+-+-+-+-+----+---------+----+-+-+-+-+----+----+----+------+----+---*/
  // ${InstInfo:Begin}
  INST(None             , None               , 0                         , 0                         , 0  , 0  , 0    , 0  , 0  ), // #0
  INST(Aaa              , X86Op_xAX          , O(000000,37,_,_,_,_,_,_  ), 0                         , 0  , 0  , 1    , 1  , 1  ), // #1
  INST(Aad              , X86I_xAX           , O(000000,D5,_,_,_,_,_,_  ), 0                         , 0  , 0  , 5    , 2  , 1  ), // #2
  INST(Aam              , X86I_xAX           , O(000000,D4,_,_,_,_,_,_  ), 0                         , 0  , 0  , 9    , 2  , 1  ), // #3
  INST(Aas              , X86Op_xAX          , O(000000,3F,_,_,_,_,_,_  ), 0                         , 0  , 0  , 13   , 1  , 1  ), // #4
  INST(Adc              , X86Arith           , O(000000,10,2,_,x,_,_,_  ), 0                         , 1  , 0  , 17   , 3  , 2  ), // #5
  INST(Adcx             , X86Rm              , O(660F38,F6,_,_,x,_,_,_  ), 0                         , 2  , 0  , 21   , 4  , 3  ), // #6
  INST(Add              , X86Arith           , O(000000,00,0,_,x,_,_,_  ), 0                         , 0  , 0  , 3146 , 3  , 1  ), // #7
  INST(Addpd            , ExtRm              , O(660F00,58,_,_,_,_,_,_  ), 0                         , 3  , 0  , 5788 , 5  , 4  ), // #8
  INST(Addps            , ExtRm              , O(000F00,58,_,_,_,_,_,_  ), 0                         , 4  , 0  , 5800 , 5  , 5  ), // #9
  INST(Addsd            , ExtRm              , O(F20F00,58,_,_,_,_,_,_  ), 0                         , 5  , 0  , 6118 , 6  , 4  ), // #10
  INST(Addss            , ExtRm              , O(F30F00,58,_,_,_,_,_,_  ), 0                         , 6  , 0  , 3283 , 7  , 5  ), // #11
  INST(Addsubpd         , ExtRm              , O(660F00,D0,_,_,_,_,_,_  ), 0                         , 3  , 0  , 5410 , 5  , 6  ), // #12
  INST(Addsubps         , ExtRm              , O(F20F00,D0,_,_,_,_,_,_  ), 0                         , 5  , 0  , 5422 , 5  , 6  ), // #13
  INST(Adox             , X86Rm              , O(F30F38,F6,_,_,x,_,_,_  ), 0                         , 7  , 0  , 26   , 4  , 7  ), // #14
  INST(Aesdec           , ExtRm              , O(660F38,DE,_,_,_,_,_,_  ), 0                         , 2  , 0  , 3352 , 5  , 8  ), // #15
  INST(Aesdeclast       , ExtRm              , O(660F38,DF,_,_,_,_,_,_  ), 0                         , 2  , 0  , 3360 , 5  , 8  ), // #16
  INST(Aesenc           , ExtRm              , O(660F38,DC,_,_,_,_,_,_  ), 0                         , 2  , 0  , 3372 , 5  , 8  ), // #17
  INST(Aesenclast       , ExtRm              , O(660F38,DD,_,_,_,_,_,_  ), 0                         , 2  , 0  , 3380 , 5  , 8  ), // #18
  INST(Aesimc           , ExtRm              , O(660F38,DB,_,_,_,_,_,_  ), 0                         , 2  , 0  , 3392 , 5  , 8  ), // #19
  INST(Aeskeygenassist  , ExtRmi             , O(660F3A,DF,_,_,_,_,_,_  ), 0                         , 8  , 0  , 3400 , 8  , 8  ), // #20
  INST(And              , X86Arith           , O(000000,20,4,_,x,_,_,_  ), 0                         , 9  , 0  , 2525 , 9  , 1  ), // #21
  INST(Andn             , VexRvm_Wx          , V(000F38,F2,_,0,x,_,_,_  ), 0                         , 10 , 0  , 7789 , 10 , 9  ), // #22
  INST(Andnpd           , ExtRm              , O(660F00,55,_,_,_,_,_,_  ), 0                         , 3  , 0  , 3433 , 5  , 4  ), // #23
  INST(Andnps           , ExtRm              , O(000F00,55,_,_,_,_,_,_  ), 0                         , 4  , 0  , 3441 , 5  , 5  ), // #24
  INST(Andpd            , ExtRm              , O(660F00,54,_,_,_,_,_,_  ), 0                         , 3  , 0  , 4745 , 11 , 4  ), // #25
  INST(Andps            , ExtRm              , O(000F00,54,_,_,_,_,_,_  ), 0                         , 4  , 0  , 4755 , 11 , 5  ), // #26
  INST(Arpl             , X86Mr_NoSize       , O(000000,63,_,_,_,_,_,_  ), 0                         , 0  , 0  , 31   , 12 , 10 ), // #27
  INST(Bextr            , VexRmv_Wx          , V(000F38,F7,_,0,x,_,_,_  ), 0                         , 10 , 0  , 36   , 13 , 9  ), // #28
  INST(Blcfill          , VexVm_Wx           , V(XOP_M9,01,1,0,x,_,_,_  ), 0                         , 11 , 0  , 42   , 14 , 11 ), // #29
  INST(Blci             , VexVm_Wx           , V(XOP_M9,02,6,0,x,_,_,_  ), 0                         , 12 , 0  , 50   , 14 , 11 ), // #30
  INST(Blcic            , VexVm_Wx           , V(XOP_M9,01,5,0,x,_,_,_  ), 0                         , 13 , 0  , 55   , 14 , 11 ), // #31
  INST(Blcmsk           , VexVm_Wx           , V(XOP_M9,02,1,0,x,_,_,_  ), 0                         , 11 , 0  , 61   , 14 , 11 ), // #32
  INST(Blcs             , VexVm_Wx           , V(XOP_M9,01,3,0,x,_,_,_  ), 0                         , 14 , 0  , 68   , 14 , 11 ), // #33
  INST(Blendpd          , ExtRmi             , O(660F3A,0D,_,_,_,_,_,_  ), 0                         , 8  , 0  , 3483 , 8  , 12 ), // #34
  INST(Blendps          , ExtRmi             , O(660F3A,0C,_,_,_,_,_,_  ), 0                         , 8  , 0  , 3492 , 8  , 12 ), // #35
  INST(Blendvpd         , ExtRm_XMM0         , O(660F38,15,_,_,_,_,_,_  ), 0                         , 2  , 0  , 3501 , 15 , 12 ), // #36
  INST(Blendvps         , ExtRm_XMM0         , O(660F38,14,_,_,_,_,_,_  ), 0                         , 2  , 0  , 3511 , 15 , 12 ), // #37
  INST(Blsfill          , VexVm_Wx           , V(XOP_M9,01,2,0,x,_,_,_  ), 0                         , 15 , 0  , 73   , 14 , 11 ), // #38
  INST(Blsi             , VexVm_Wx           , V(000F38,F3,3,0,x,_,_,_  ), 0                         , 16 , 0  , 81   , 14 , 9  ), // #39
  INST(Blsic            , VexVm_Wx           , V(XOP_M9,01,6,0,x,_,_,_  ), 0                         , 12 , 0  , 86   , 14 , 11 ), // #40
  INST(Blsmsk           , VexVm_Wx           , V(000F38,F3,2,0,x,_,_,_  ), 0                         , 17 , 0  , 92   , 14 , 9  ), // #41
  INST(Blsr             , VexVm_Wx           , V(000F38,F3,1,0,x,_,_,_  ), 0                         , 18 , 0  , 99   , 14 , 9  ), // #42
  INST(Bndcl            , X86Rm              , O(F30F00,1A,_,_,_,_,_,_  ), 0                         , 6  , 0  , 104  , 16 , 13 ), // #43
  INST(Bndcn            , X86Rm              , O(F20F00,1B,_,_,_,_,_,_  ), 0                         , 5  , 0  , 110  , 16 , 13 ), // #44
  INST(Bndcu            , X86Rm              , O(F20F00,1A,_,_,_,_,_,_  ), 0                         , 5  , 0  , 116  , 16 , 13 ), // #45
  INST(Bndldx           , X86Rm              , O(000F00,1A,_,_,_,_,_,_  ), 0                         , 4  , 0  , 122  , 17 , 13 ), // #46
  INST(Bndmk            , X86Rm              , O(F30F00,1B,_,_,_,_,_,_  ), 0                         , 6  , 0  , 129  , 18 , 13 ), // #47
  INST(Bndmov           , X86Bndmov          , O(660F00,1A,_,_,_,_,_,_  ), O(660F00,1B,_,_,_,_,_,_  ), 3  , 1  , 135  , 19 , 13 ), // #48
  INST(Bndstx           , X86Mr              , O(000F00,1B,_,_,_,_,_,_  ), 0                         , 4  , 0  , 142  , 20 , 13 ), // #49
  INST(Bound            , X86Rm              , O(000000,62,_,_,_,_,_,_  ), 0                         , 0  , 0  , 149  , 21 , 0  ), // #50
  INST(Bsf              , X86Rm              , O(000F00,BC,_,_,x,_,_,_  ), 0                         , 4  , 0  , 155  , 22 , 1  ), // #51
  INST(Bsr              , X86Rm              , O(000F00,BD,_,_,x,_,_,_  ), 0                         , 4  , 0  , 159  , 22 , 1  ), // #52
  INST(Bswap            , X86Bswap           , O(000F00,C8,_,_,x,_,_,_  ), 0                         , 4  , 0  , 163  , 23 , 0  ), // #53
  INST(Bt               , X86Bt              , O(000F00,A3,_,_,x,_,_,_  ), O(000F00,BA,4,_,x,_,_,_  ), 4  , 2  , 169  , 24 , 14 ), // #54
  INST(Btc              , X86Bt              , O(000F00,BB,_,_,x,_,_,_  ), O(000F00,BA,7,_,x,_,_,_  ), 4  , 3  , 172  , 25 , 14 ), // #55
  INST(Btr              , X86Bt              , O(000F00,B3,_,_,x,_,_,_  ), O(000F00,BA,6,_,x,_,_,_  ), 4  , 4  , 176  , 25 , 14 ), // #56
  INST(Bts              , X86Bt              , O(000F00,AB,_,_,x,_,_,_  ), O(000F00,BA,5,_,x,_,_,_  ), 4  , 5  , 180  , 25 , 14 ), // #57
  INST(Bzhi             , VexRmv_Wx          , V(000F38,F5,_,0,x,_,_,_  ), 0                         , 10 , 0  , 184  , 13 , 15 ), // #58
  INST(Call             , X86Call            , O(000000,FF,2,_,_,_,_,_  ), 0                         , 1  , 0  , 3038 , 26 , 1  ), // #59
  INST(Cbw              , X86Op_xAX          , O(660000,98,_,_,_,_,_,_  ), 0                         , 19 , 0  , 189  , 27 , 0  ), // #60
  INST(Cdq              , X86Op_xDX_xAX      , O(000000,99,_,_,_,_,_,_  ), 0                         , 0  , 0  , 193  , 28 , 0  ), // #61
  INST(Cdqe             , X86Op_xAX          , O(000000,98,_,_,1,_,_,_  ), 0                         , 20 , 0  , 197  , 29 , 0  ), // #62
  INST(Clac             , X86Op              , O(000F01,CA,_,_,_,_,_,_  ), 0                         , 21 , 0  , 202  , 30 , 16 ), // #63
  INST(Clc              , X86Op              , O(000000,F8,_,_,_,_,_,_  ), 0                         , 0  , 0  , 207  , 30 , 17 ), // #64
  INST(Cld              , X86Op              , O(000000,FC,_,_,_,_,_,_  ), 0                         , 0  , 0  , 211  , 30 , 18 ), // #65
  INST(Cldemote         , X86M_Only          , O(000F00,1C,0,_,_,_,_,_  ), 0                         , 4  , 0  , 215  , 31 , 19 ), // #66
  INST(Clflush          , X86M_Only          , O(000F00,AE,7,_,_,_,_,_  ), 0                         , 22 , 0  , 224  , 31 , 20 ), // #67
  INST(Clflushopt       , X86M_Only          , O(660F00,AE,7,_,_,_,_,_  ), 0                         , 23 , 0  , 232  , 31 , 21 ), // #68
  INST(Clgi             , X86Op              , O(000F01,DD,_,_,_,_,_,_  ), 0                         , 21 , 0  , 243  , 30 , 22 ), // #69
  INST(Cli              , X86Op              , O(000000,FA,_,_,_,_,_,_  ), 0                         , 0  , 0  , 248  , 30 , 23 ), // #70
  INST(Clrssbsy         , X86M_Only          , O(F30F00,AE,6,_,_,_,_,_  ), 0                         , 24 , 0  , 252  , 32 , 24 ), // #71
  INST(Clts             , X86Op              , O(000F00,06,_,_,_,_,_,_  ), 0                         , 4  , 0  , 261  , 30 , 0  ), // #72
  INST(Clui             , X86Op              , O(F30F01,EE,_,_,_,_,_,_  ), 0                         , 25 , 0  , 266  , 33 , 25 ), // #73
  INST(Clwb             , X86M_Only          , O(660F00,AE,6,_,_,_,_,_  ), 0                         , 26 , 0  , 271  , 31 , 26 ), // #74
  INST(Clzero           , X86Op_MemZAX       , O(000F01,FC,_,_,_,_,_,_  ), 0                         , 21 , 0  , 276  , 34 , 27 ), // #75
  INST(Cmc              , X86Op              , O(000000,F5,_,_,_,_,_,_  ), 0                         , 0  , 0  , 283  , 30 , 28 ), // #76
  INST(Cmova            , X86Rm              , O(000F00,47,_,_,x,_,_,_  ), 0                         , 4  , 0  , 287  , 22 , 29 ), // #77
  INST(Cmovae           , X86Rm              , O(000F00,43,_,_,x,_,_,_  ), 0                         , 4  , 0  , 293  , 22 , 30 ), // #78
  INST(Cmovb            , X86Rm              , O(000F00,42,_,_,x,_,_,_  ), 0                         , 4  , 0  , 648  , 22 , 30 ), // #79
  INST(Cmovbe           , X86Rm              , O(000F00,46,_,_,x,_,_,_  ), 0                         , 4  , 0  , 655  , 22 , 29 ), // #80
  INST(Cmovc            , X86Rm              , O(000F00,42,_,_,x,_,_,_  ), 0                         , 4  , 0  , 300  , 22 , 30 ), // #81
  INST(Cmove            , X86Rm              , O(000F00,44,_,_,x,_,_,_  ), 0                         , 4  , 0  , 663  , 22 , 31 ), // #82
  INST(Cmovg            , X86Rm              , O(000F00,4F,_,_,x,_,_,_  ), 0                         , 4  , 0  , 306  , 22 , 32 ), // #83
  INST(Cmovge           , X86Rm              , O(000F00,4D,_,_,x,_,_,_  ), 0                         , 4  , 0  , 312  , 22 , 33 ), // #84
  INST(Cmovl            , X86Rm              , O(000F00,4C,_,_,x,_,_,_  ), 0                         , 4  , 0  , 319  , 22 , 33 ), // #85
  INST(Cmovle           , X86Rm              , O(000F00,4E,_,_,x,_,_,_  ), 0                         , 4  , 0  , 325  , 22 , 32 ), // #86
  INST(Cmovna           , X86Rm              , O(000F00,46,_,_,x,_,_,_  ), 0                         , 4  , 0  , 332  , 22 , 29 ), // #87
  INST(Cmovnae          , X86Rm              , O(000F00,42,_,_,x,_,_,_  ), 0                         , 4  , 0  , 339  , 22 , 30 ), // #88
  INST(Cmovnb           , X86Rm              , O(000F00,43,_,_,x,_,_,_  ), 0                         , 4  , 0  , 670  , 22 , 30 ), // #89
  INST(Cmovnbe          , X86Rm              , O(000F00,47,_,_,x,_,_,_  ), 0                         , 4  , 0  , 678  , 22 , 29 ), // #90
  INST(Cmovnc           , X86Rm              , O(000F00,43,_,_,x,_,_,_  ), 0                         , 4  , 0  , 347  , 22 , 30 ), // #91
  INST(Cmovne           , X86Rm              , O(000F00,45,_,_,x,_,_,_  ), 0                         , 4  , 0  , 687  , 22 , 31 ), // #92
  INST(Cmovng           , X86Rm              , O(000F00,4E,_,_,x,_,_,_  ), 0                         , 4  , 0  , 354  , 22 , 32 ), // #93
  INST(Cmovnge          , X86Rm              , O(000F00,4C,_,_,x,_,_,_  ), 0                         , 4  , 0  , 361  , 22 , 33 ), // #94
  INST(Cmovnl           , X86Rm              , O(000F00,4D,_,_,x,_,_,_  ), 0                         , 4  , 0  , 369  , 22 , 33 ), // #95
  INST(Cmovnle          , X86Rm              , O(000F00,4F,_,_,x,_,_,_  ), 0                         , 4  , 0  , 376  , 22 , 32 ), // #96
  INST(Cmovno           , X86Rm              , O(000F00,41,_,_,x,_,_,_  ), 0                         , 4  , 0  , 384  , 22 , 34 ), // #97
  INST(Cmovnp           , X86Rm              , O(000F00,4B,_,_,x,_,_,_  ), 0                         , 4  , 0  , 391  , 22 , 35 ), // #98
  INST(Cmovns           , X86Rm              , O(000F00,49,_,_,x,_,_,_  ), 0                         , 4  , 0  , 398  , 22 , 36 ), // #99
  INST(Cmovnz           , X86Rm              , O(000F00,45,_,_,x,_,_,_  ), 0                         , 4  , 0  , 405  , 22 , 31 ), // #100
  INST(Cmovo            , X86Rm              , O(000F00,40,_,_,x,_,_,_  ), 0                         , 4  , 0  , 412  , 22 , 34 ), // #101
  INST(Cmovp            , X86Rm              , O(000F00,4A,_,_,x,_,_,_  ), 0                         , 4  , 0  , 418  , 22 , 35 ), // #102
  INST(Cmovpe           , X86Rm              , O(000F00,4A,_,_,x,_,_,_  ), 0                         , 4  , 0  , 424  , 22 , 35 ), // #103
  INST(Cmovpo           , X86Rm              , O(000F00,4B,_,_,x,_,_,_  ), 0                         , 4  , 0  , 431  , 22 , 35 ), // #104
  INST(Cmovs            , X86Rm              , O(000F00,48,_,_,x,_,_,_  ), 0                         , 4  , 0  , 438  , 22 , 36 ), // #105
  INST(Cmovz            , X86Rm              , O(000F00,44,_,_,x,_,_,_  ), 0                         , 4  , 0  , 444  , 22 , 31 ), // #106
  INST(Cmp              , X86Arith           , O(000000,38,7,_,x,_,_,_  ), 0                         , 27 , 0  , 450  , 35 , 1  ), // #107
  INST(Cmppd            , ExtRmi             , O(660F00,C2,_,_,_,_,_,_  ), 0                         , 3  , 0  , 3737 , 8  , 4  ), // #108
  INST(Cmpps            , ExtRmi             , O(000F00,C2,_,_,_,_,_,_  ), 0                         , 4  , 0  , 3751 , 8  , 5  ), // #109
  INST(Cmps             , X86StrMm           , O(000000,A6,_,_,_,_,_,_  ), 0                         , 0  , 0  , 454  , 36 , 37 ), // #110
  INST(Cmpsd            , ExtRmi             , O(F20F00,C2,_,_,_,_,_,_  ), 0                         , 5  , 0  , 3758 , 37 , 4  ), // #111
  INST(Cmpss            , ExtRmi             , O(F30F00,C2,_,_,_,_,_,_  ), 0                         , 6  , 0  , 3772 , 38 , 5  ), // #112
  INST(Cmpxchg          , X86Cmpxchg         , O(000F00,B0,_,_,x,_,_,_  ), 0                         , 4  , 0  , 459  , 39 , 38 ), // #113
  INST(Cmpxchg16b       , X86Cmpxchg8b_16b   , O(000F00,C7,1,_,1,_,_,_  ), 0                         , 28 , 0  , 467  , 40 , 39 ), // #114
  INST(Cmpxchg8b        , X86Cmpxchg8b_16b   , O(000F00,C7,1,_,_,_,_,_  ), 0                         , 29 , 0  , 478  , 41 , 40 ), // #115
  INST(Comisd           , ExtRm              , O(660F00,2F,_,_,_,_,_,_  ), 0                         , 3  , 0  , 11391, 6  , 41 ), // #116
  INST(Comiss           , ExtRm              , O(000F00,2F,_,_,_,_,_,_  ), 0                         , 4  , 0  , 11409, 7  , 42 ), // #117
  INST(Cpuid            , X86Op              , O(000F00,A2,_,_,_,_,_,_  ), 0                         , 4  , 0  , 488  , 42 , 43 ), // #118
  INST(Cqo              , X86Op_xDX_xAX      , O(000000,99,_,_,1,_,_,_  ), 0                         , 20 , 0  , 494  , 43 , 0  ), // #119
  INST(Crc32            , X86Crc             , O(F20F38,F0,_,_,x,_,_,_  ), 0                         , 30 , 0  , 498  , 44 , 44 ), // #120
  INST(Cvtdq2pd         , ExtRm              , O(F30F00,E6,_,_,_,_,_,_  ), 0                         , 6  , 0  , 3827 , 6  , 4  ), // #121
  INST(Cvtdq2ps         , ExtRm              , O(000F00,5B,_,_,_,_,_,_  ), 0                         , 4  , 0  , 3847 , 5  , 4  ), // #122
  INST(Cvtpd2dq         , ExtRm              , O(F20F00,E6,_,_,_,_,_,_  ), 0                         , 5  , 0  , 3886 , 5  , 4  ), // #123
  INST(Cvtpd2pi         , ExtRm              , O(660F00,2D,_,_,_,_,_,_  ), 0                         , 3  , 0  , 504  , 45 , 4  ), // #124
  INST(Cvtpd2ps         , ExtRm              , O(660F00,5A,_,_,_,_,_,_  ), 0                         , 3  , 0  , 3906 , 5  , 4  ), // #125
  INST(Cvtpi2pd         , ExtRm              , O(660F00,2A,_,_,_,_,_,_  ), 0                         , 3  , 0  , 513  , 46 , 4  ), // #126
  INST(Cvtpi2ps         , ExtRm              , O(000F00,2A,_,_,_,_,_,_  ), 0                         , 4  , 0  , 522  , 46 , 5  ), // #127
  INST(Cvtps2dq         , ExtRm              , O(660F00,5B,_,_,_,_,_,_  ), 0                         , 3  , 0  , 4040 , 5  , 4  ), // #128
  INST(Cvtps2pd         , ExtRm              , O(000F00,5A,_,_,_,_,_,_  ), 0                         , 4  , 0  , 4050 , 6  , 4  ), // #129
  INST(Cvtps2pi         , ExtRm              , O(000F00,2D,_,_,_,_,_,_  ), 0                         , 4  , 0  , 531  , 47 , 5  ), // #130
  INST(Cvtsd2si         , ExtRm_Wx_GpqOnly   , O(F20F00,2D,_,_,x,_,_,_  ), 0                         , 5  , 0  , 4153 , 48 , 4  ), // #131
  INST(Cvtsd2ss         , ExtRm              , O(F20F00,5A,_,_,_,_,_,_  ), 0                         , 5  , 0  , 4163 , 6  , 4  ), // #132
  INST(Cvtsi2sd         , ExtRm_Wx           , O(F20F00,2A,_,_,x,_,_,_  ), 0                         , 5  , 0  , 4225 , 49 , 4  ), // #133
  INST(Cvtsi2ss         , ExtRm_Wx           , O(F30F00,2A,_,_,x,_,_,_  ), 0                         , 6  , 0  , 4245 , 49 , 5  ), // #134
  INST(Cvtss2sd         , ExtRm              , O(F30F00,5A,_,_,_,_,_,_  ), 0                         , 6  , 0  , 4255 , 7  , 4  ), // #135
  INST(Cvtss2si         , ExtRm_Wx_GpqOnly   , O(F30F00,2D,_,_,x,_,_,_  ), 0                         , 6  , 0  , 4275 , 50 , 5  ), // #136
  INST(Cvttpd2dq        , ExtRm              , O(660F00,E6,_,_,_,_,_,_  ), 0                         , 3  , 0  , 4296 , 5  , 4  ), // #137
  INST(Cvttpd2pi        , ExtRm              , O(660F00,2C,_,_,_,_,_,_  ), 0                         , 3  , 0  , 540  , 45 , 4  ), // #138
  INST(Cvttps2dq        , ExtRm              , O(F30F00,5B,_,_,_,_,_,_  ), 0                         , 6  , 0  , 4409 , 5  , 4  ), // #139
  INST(Cvttps2pi        , ExtRm              , O(000F00,2C,_,_,_,_,_,_  ), 0                         , 4  , 0  , 550  , 47 , 5  ), // #140
  INST(Cvttsd2si        , ExtRm_Wx_GpqOnly   , O(F20F00,2C,_,_,x,_,_,_  ), 0                         , 5  , 0  , 4455 , 48 , 4  ), // #141
  INST(Cvttss2si        , ExtRm_Wx_GpqOnly   , O(F30F00,2C,_,_,x,_,_,_  ), 0                         , 6  , 0  , 4501 , 50 , 5  ), // #142
  INST(Cwd              , X86Op_xDX_xAX      , O(660000,99,_,_,_,_,_,_  ), 0                         , 19 , 0  , 560  , 51 , 0  ), // #143
  INST(Cwde             , X86Op_xAX          , O(000000,98,_,_,_,_,_,_  ), 0                         , 0  , 0  , 564  , 52 , 0  ), // #144
  INST(Daa              , X86Op              , O(000000,27,_,_,_,_,_,_  ), 0                         , 0  , 0  , 569  , 1  , 1  ), // #145
  INST(Das              , X86Op              , O(000000,2F,_,_,_,_,_,_  ), 0                         , 0  , 0  , 573  , 1  , 1  ), // #146
  INST(Dec              , X86IncDec          , O(000000,FE,1,_,x,_,_,_  ), O(000000,48,_,_,x,_,_,_  ), 31 , 6  , 3355 , 53 , 45 ), // #147
  INST(Div              , X86M_GPB_MulDiv    , O(000000,F6,6,_,x,_,_,_  ), 0                         , 32 , 0  , 810  , 54 , 1  ), // #148
  INST(Divpd            , ExtRm              , O(660F00,5E,_,_,_,_,_,_  ), 0                         , 3  , 0  , 4652 , 5  , 4  ), // #149
  INST(Divps            , ExtRm              , O(000F00,5E,_,_,_,_,_,_  ), 0                         , 4  , 0  , 4666 , 5  , 5  ), // #150
  INST(Divsd            , ExtRm              , O(F20F00,5E,_,_,_,_,_,_  ), 0                         , 5  , 0  , 4673 , 6  , 4  ), // #151
  INST(Divss            , ExtRm              , O(F30F00,5E,_,_,_,_,_,_  ), 0                         , 6  , 0  , 4687 , 7  , 5  ), // #152
  INST(Dppd             , ExtRmi             , O(660F3A,41,_,_,_,_,_,_  ), 0                         , 8  , 0  , 4704 , 8  , 12 ), // #153
  INST(Dpps             , ExtRmi             , O(660F3A,40,_,_,_,_,_,_  ), 0                         , 8  , 0  , 4710 , 8  , 12 ), // #154
  INST(Emms             , X86Op              , O(000F00,77,_,_,_,_,_,_  ), 0                         , 4  , 0  , 778  , 55 , 46 ), // #155
  INST(Endbr32          , X86Op_Mod11RM      , O(F30F00,1E,7,_,_,_,_,3  ), 0                         , 33 , 0  , 577  , 30 , 47 ), // #156
  INST(Endbr64          , X86Op_Mod11RM      , O(F30F00,1E,7,_,_,_,_,2  ), 0                         , 34 , 0  , 585  , 30 , 47 ), // #157
  INST(Enqcmd           , X86EnqcmdMovdir64b , O(F20F38,F8,_,_,_,_,_,_  ), 0                         , 30 , 0  , 593  , 56 , 48 ), // #158
  INST(Enqcmds          , X86EnqcmdMovdir64b , O(F30F38,F8,_,_,_,_,_,_  ), 0                         , 7  , 0  , 600  , 56 , 48 ), // #159
  INST(Enter            , X86Enter           , O(000000,C8,_,_,_,_,_,_  ), 0                         , 0  , 0  , 3046 , 57 , 0  ), // #160
  INST(Extractps        , ExtExtract         , O(660F3A,17,_,_,_,_,_,_  ), 0                         , 8  , 0  , 4900 , 58 , 12 ), // #161
  INST(Extrq            , ExtExtrq           , O(660F00,79,_,_,_,_,_,_  ), O(660F00,78,0,_,_,_,_,_  ), 3  , 7  , 8625 , 59 , 49 ), // #162
  INST(F2xm1            , FpuOp              , O_FPU(00,D9F0,_)          , 0                         , 35 , 0  , 608  , 30 , 0  ), // #163
  INST(Fabs             , FpuOp              , O_FPU(00,D9E1,_)          , 0                         , 35 , 0  , 614  , 30 , 0  ), // #164
  INST(Fadd             , FpuArith           , O_FPU(00,C0C0,0)          , 0                         , 36 , 0  , 2121 , 60 , 0  ), // #165
  INST(Faddp            , FpuRDef            , O_FPU(00,DEC0,_)          , 0                         , 37 , 0  , 619  , 61 , 0  ), // #166
  INST(Fbld             , X86M_Only          , O_FPU(00,00DF,4)          , 0                         , 38 , 0  , 625  , 62 , 0  ), // #167
  INST(Fbstp            , X86M_Only          , O_FPU(00,00DF,6)          , 0                         , 39 , 0  , 630  , 62 , 0  ), // #168
  INST(Fchs             , FpuOp              , O_FPU(00,D9E0,_)          , 0                         , 35 , 0  , 636  , 30 , 0  ), // #169
  INST(Fclex            , FpuOp              , O_FPU(9B,DBE2,_)          , 0                         , 40 , 0  , 641  , 30 , 0  ), // #170
  INST(Fcmovb           , FpuR               , O_FPU(00,DAC0,_)          , 0                         , 41 , 0  , 647  , 63 , 30 ), // #171
  INST(Fcmovbe          , FpuR               , O_FPU(00,DAD0,_)          , 0                         , 41 , 0  , 654  , 63 , 29 ), // #172
  INST(Fcmove           , FpuR               , O_FPU(00,DAC8,_)          , 0                         , 41 , 0  , 662  , 63 , 31 ), // #173
  INST(Fcmovnb          , FpuR               , O_FPU(00,DBC0,_)          , 0                         , 42 , 0  , 669  , 63 , 30 ), // #174
  INST(Fcmovnbe         , FpuR               , O_FPU(00,DBD0,_)          , 0                         , 42 , 0  , 677  , 63 , 29 ), // #175
  INST(Fcmovne          , FpuR               , O_FPU(00,DBC8,_)          , 0                         , 42 , 0  , 686  , 63 , 31 ), // #176
  INST(Fcmovnu          , FpuR               , O_FPU(00,DBD8,_)          , 0                         , 42 , 0  , 694  , 63 , 35 ), // #177
  INST(Fcmovu           , FpuR               , O_FPU(00,DAD8,_)          , 0                         , 41 , 0  , 702  , 63 , 35 ), // #178
  INST(Fcom             , FpuCom             , O_FPU(00,D0D0,2)          , 0                         , 43 , 0  , 709  , 64 , 0  ), // #179
  INST(Fcomi            , FpuR               , O_FPU(00,DBF0,_)          , 0                         , 42 , 0  , 714  , 63 , 50 ), // #180
  INST(Fcomip           , FpuR               , O_FPU(00,DFF0,_)          , 0                         , 44 , 0  , 720  , 63 , 50 ), // #181
  INST(Fcomp            , FpuCom             , O_FPU(00,D8D8,3)          , 0                         , 45 , 0  , 727  , 64 , 0  ), // #182
  INST(Fcompp           , FpuOp              , O_FPU(00,DED9,_)          , 0                         , 37 , 0  , 733  , 30 , 0  ), // #183
  INST(Fcos             , FpuOp              , O_FPU(00,D9FF,_)          , 0                         , 35 , 0  , 740  , 30 , 0  ), // #184
  INST(Fdecstp          , FpuOp              , O_FPU(00,D9F6,_)          , 0                         , 35 , 0  , 745  , 30 , 0  ), // #185
  INST(Fdiv             , FpuArith           , O_FPU(00,F0F8,6)          , 0                         , 46 , 0  , 753  , 60 , 0  ), // #186
  INST(Fdivp            , FpuRDef            , O_FPU(00,DEF8,_)          , 0                         , 37 , 0  , 758  , 61 , 0  ), // #187
  INST(Fdivr            , FpuArith           , O_FPU(00,F8F0,7)          , 0                         , 47 , 0  , 764  , 60 , 0  ), // #188
  INST(Fdivrp           , FpuRDef            , O_FPU(00,DEF0,_)          , 0                         , 37 , 0  , 770  , 61 , 0  ), // #189
  INST(Femms            , X86Op              , O(000F00,0E,_,_,_,_,_,_  ), 0                         , 4  , 0  , 777  , 30 , 51 ), // #190
  INST(Ffree            , FpuR               , O_FPU(00,DDC0,_)          , 0                         , 48 , 0  , 783  , 63 , 0  ), // #191
  INST(Fiadd            , FpuM               , O_FPU(00,00DA,0)          , 0                         , 49 , 0  , 789  , 65 , 0  ), // #192
  INST(Ficom            , FpuM               , O_FPU(00,00DA,2)          , 0                         , 50 , 0  , 795  , 65 , 0  ), // #193
  INST(Ficomp           , FpuM               , O_FPU(00,00DA,3)          , 0                         , 51 , 0  , 801  , 65 , 0  ), // #194
  INST(Fidiv            , FpuM               , O_FPU(00,00DA,6)          , 0                         , 39 , 0  , 808  , 65 , 0  ), // #195
  INST(Fidivr           , FpuM               , O_FPU(00,00DA,7)          , 0                         , 52 , 0  , 814  , 65 , 0  ), // #196
  INST(Fild             , FpuM               , O_FPU(00,00DB,0)          , O_FPU(00,00DF,5)          , 49 , 8  , 821  , 66 , 0  ), // #197
  INST(Fimul            , FpuM               , O_FPU(00,00DA,1)          , 0                         , 53 , 0  , 826  , 65 , 0  ), // #198
  INST(Fincstp          , FpuOp              , O_FPU(00,D9F7,_)          , 0                         , 35 , 0  , 832  , 30 , 0  ), // #199
  INST(Finit            , FpuOp              , O_FPU(9B,DBE3,_)          , 0                         , 40 , 0  , 840  , 30 , 0  ), // #200
  INST(Fist             , FpuM               , O_FPU(00,00DB,2)          , 0                         , 50 , 0  , 846  , 65 , 0  ), // #201
  INST(Fistp            , FpuM               , O_FPU(00,00DB,3)          , O_FPU(00,00DF,7)          , 51 , 9  , 851  , 66 , 0  ), // #202
  INST(Fisttp           , FpuM               , O_FPU(00,00DB,1)          , O_FPU(00,00DD,1)          , 53 , 10 , 857  , 66 , 6  ), // #203
  INST(Fisub            , FpuM               , O_FPU(00,00DA,4)          , 0                         , 38 , 0  , 864  , 65 , 0  ), // #204
  INST(Fisubr           , FpuM               , O_FPU(00,00DA,5)          , 0                         , 54 , 0  , 870  , 65 , 0  ), // #205
  INST(Fld              , FpuFldFst          , O_FPU(00,00D9,0)          , O_FPU(00,00DB,5)          , 49 , 11 , 877  , 67 , 0  ), // #206
  INST(Fld1             , FpuOp              , O_FPU(00,D9E8,_)          , 0                         , 35 , 0  , 881  , 30 , 0  ), // #207
  INST(Fldcw            , X86M_Only          , O_FPU(00,00D9,5)          , 0                         , 54 , 0  , 886  , 68 , 0  ), // #208
  INST(Fldenv           , X86M_Only          , O_FPU(00,00D9,4)          , 0                         , 38 , 0  , 892  , 69 , 0  ), // #209
  INST(Fldl2e           , FpuOp              , O_FPU(00,D9EA,_)          , 0                         , 35 , 0  , 899  , 30 , 0  ), // #210
  INST(Fldl2t           , FpuOp              , O_FPU(00,D9E9,_)          , 0                         , 35 , 0  , 906  , 30 , 0  ), // #211
  INST(Fldlg2           , FpuOp              , O_FPU(00,D9EC,_)          , 0                         , 35 , 0  , 913  , 30 , 0  ), // #212
  INST(Fldln2           , FpuOp              , O_FPU(00,D9ED,_)          , 0                         , 35 , 0  , 920  , 30 , 0  ), // #213
  INST(Fldpi            , FpuOp              , O_FPU(00,D9EB,_)          , 0                         , 35 , 0  , 927  , 30 , 0  ), // #214
  INST(Fldz             , FpuOp              , O_FPU(00,D9EE,_)          , 0                         , 35 , 0  , 933  , 30 , 0  ), // #215
  INST(Fmul             , FpuArith           , O_FPU(00,C8C8,1)          , 0                         , 55 , 0  , 2163 , 60 , 0  ), // #216
  INST(Fmulp            , FpuRDef            , O_FPU(00,DEC8,_)          , 0                         , 37 , 0  , 938  , 61 , 0  ), // #217
  INST(Fnclex           , FpuOp              , O_FPU(00,DBE2,_)          , 0                         , 42 , 0  , 944  , 30 , 0  ), // #218
  INST(Fninit           , FpuOp              , O_FPU(00,DBE3,_)          , 0                         , 42 , 0  , 951  , 30 , 0  ), // #219
  INST(Fnop             , FpuOp              , O_FPU(00,D9D0,_)          , 0                         , 35 , 0  , 958  , 30 , 0  ), // #220
  INST(Fnsave           , X86M_Only          , O_FPU(00,00DD,6)          , 0                         , 39 , 0  , 963  , 69 , 0  ), // #221
  INST(Fnstcw           , X86M_Only          , O_FPU(00,00D9,7)          , 0                         , 52 , 0  , 970  , 68 , 0  ), // #222
  INST(Fnstenv          , X86M_Only          , O_FPU(00,00D9,6)          , 0                         , 39 , 0  , 977  , 69 , 0  ), // #223
  INST(Fnstsw           , FpuStsw            , O_FPU(00,00DD,7)          , O_FPU(00,DFE0,_)          , 52 , 12 , 985  , 70 , 0  ), // #224
  INST(Fpatan           , FpuOp              , O_FPU(00,D9F3,_)          , 0                         , 35 , 0  , 992  , 30 , 0  ), // #225
  INST(Fprem            , FpuOp              , O_FPU(00,D9F8,_)          , 0                         , 35 , 0  , 999  , 30 , 0  ), // #226
  INST(Fprem1           , FpuOp              , O_FPU(00,D9F5,_)          , 0                         , 35 , 0  , 1005 , 30 , 0  ), // #227
  INST(Fptan            , FpuOp              , O_FPU(00,D9F2,_)          , 0                         , 35 , 0  , 1012 , 30 , 0  ), // #228
  INST(Frndint          , FpuOp              , O_FPU(00,D9FC,_)          , 0                         , 35 , 0  , 1018 , 30 , 0  ), // #229
  INST(Frstor           , X86M_Only          , O_FPU(00,00DD,4)          , 0                         , 38 , 0  , 1026 , 69 , 0  ), // #230
  INST(Fsave            , X86M_Only          , O_FPU(9B,00DD,6)          , 0                         , 56 , 0  , 1033 , 69 , 0  ), // #231
  INST(Fscale           , FpuOp              , O_FPU(00,D9FD,_)          , 0                         , 35 , 0  , 1039 , 30 , 0  ), // #232
  INST(Fsin             , FpuOp              , O_FPU(00,D9FE,_)          , 0                         , 35 , 0  , 1046 , 30 , 0  ), // #233
  INST(Fsincos          , FpuOp              , O_FPU(00,D9FB,_)          , 0                         , 35 , 0  , 1051 , 30 , 0  ), // #234
  INST(Fsqrt            , FpuOp              , O_FPU(00,D9FA,_)          , 0                         , 35 , 0  , 1059 , 30 , 0  ), // #235
  INST(Fst              , FpuFldFst          , O_FPU(00,00D9,2)          , 0                         , 50 , 0  , 1065 , 71 , 0  ), // #236
  INST(Fstcw            , X86M_Only          , O_FPU(9B,00D9,7)          , 0                         , 57 , 0  , 1069 , 68 , 0  ), // #237
  INST(Fstenv           , X86M_Only          , O_FPU(9B,00D9,6)          , 0                         , 56 , 0  , 1075 , 69 , 0  ), // #238
  INST(Fstp             , FpuFldFst          , O_FPU(00,00D9,3)          , O(000000,DB,7,_,_,_,_,_  ), 51 , 13 , 1082 , 67 , 0  ), // #239
  INST(Fstsw            , FpuStsw            , O_FPU(9B,00DD,7)          , O_FPU(9B,DFE0,_)          , 57 , 14 , 1087 , 70 , 0  ), // #240
  INST(Fsub             , FpuArith           , O_FPU(00,E0E8,4)          , 0                         , 58 , 0  , 2241 , 60 , 0  ), // #241
  INST(Fsubp            , FpuRDef            , O_FPU(00,DEE8,_)          , 0                         , 37 , 0  , 1093 , 61 , 0  ), // #242
  INST(Fsubr            , FpuArith           , O_FPU(00,E8E0,5)          , 0                         , 59 , 0  , 2247 , 60 , 0  ), // #243
  INST(Fsubrp           , FpuRDef            , O_FPU(00,DEE0,_)          , 0                         , 37 , 0  , 1099 , 61 , 0  ), // #244
  INST(Ftst             , FpuOp              , O_FPU(00,D9E4,_)          , 0                         , 35 , 0  , 1106 , 30 , 0  ), // #245
  INST(Fucom            , FpuRDef            , O_FPU(00,DDE0,_)          , 0                         , 48 , 0  , 1111 , 61 , 0  ), // #246
  INST(Fucomi           , FpuR               , O_FPU(00,DBE8,_)          , 0                         , 42 , 0  , 1117 , 63 , 50 ), // #247
  INST(Fucomip          , FpuR               , O_FPU(00,DFE8,_)          , 0                         , 44 , 0  , 1124 , 63 , 50 ), // #248
  INST(Fucomp           , FpuRDef            , O_FPU(00,DDE8,_)          , 0                         , 48 , 0  , 1132 , 61 , 0  ), // #249
  INST(Fucompp          , FpuOp              , O_FPU(00,DAE9,_)          , 0                         , 41 , 0  , 1139 , 30 , 0  ), // #250
  INST(Fwait            , X86Op              , O_FPU(00,009B,_)          , 0                         , 49 , 0  , 1147 , 30 , 0  ), // #251
  INST(Fxam             , FpuOp              , O_FPU(00,D9E5,_)          , 0                         , 35 , 0  , 1153 , 30 , 0  ), // #252
  INST(Fxch             , FpuR               , O_FPU(00,D9C8,_)          , 0                         , 35 , 0  , 1158 , 61 , 0  ), // #253
  INST(Fxrstor          , X86M_Only          , O(000F00,AE,1,_,_,_,_,_  ), 0                         , 29 , 0  , 1163 , 69 , 52 ), // #254
  INST(Fxrstor64        , X86M_Only          , O(000F00,AE,1,_,1,_,_,_  ), 0                         , 28 , 0  , 1171 , 72 , 52 ), // #255
  INST(Fxsave           , X86M_Only          , O(000F00,AE,0,_,_,_,_,_  ), 0                         , 4  , 0  , 1181 , 69 , 52 ), // #256
  INST(Fxsave64         , X86M_Only          , O(000F00,AE,0,_,1,_,_,_  ), 0                         , 60 , 0  , 1188 , 72 , 52 ), // #257
  INST(Fxtract          , FpuOp              , O_FPU(00,D9F4,_)          , 0                         , 35 , 0  , 1197 , 30 , 0  ), // #258
  INST(Fyl2x            , FpuOp              , O_FPU(00,D9F1,_)          , 0                         , 35 , 0  , 1205 , 30 , 0  ), // #259
  INST(Fyl2xp1          , FpuOp              , O_FPU(00,D9F9,_)          , 0                         , 35 , 0  , 1211 , 30 , 0  ), // #260
  INST(Getsec           , X86Op              , O(000F00,37,_,_,_,_,_,_  ), 0                         , 4  , 0  , 1219 , 30 , 53 ), // #261
  INST(Gf2p8affineinvqb , ExtRmi             , O(660F3A,CF,_,_,_,_,_,_  ), 0                         , 8  , 0  , 6789 , 8  , 54 ), // #262
  INST(Gf2p8affineqb    , ExtRmi             , O(660F3A,CE,_,_,_,_,_,_  ), 0                         , 8  , 0  , 6807 , 8  , 54 ), // #263
  INST(Gf2p8mulb        , ExtRm              , O(660F38,CF,_,_,_,_,_,_  ), 0                         , 2  , 0  , 6822 , 5  , 54 ), // #264
  INST(Haddpd           , ExtRm              , O(660F00,7C,_,_,_,_,_,_  ), 0                         , 3  , 0  , 6833 , 5  , 6  ), // #265
  INST(Haddps           , ExtRm              , O(F20F00,7C,_,_,_,_,_,_  ), 0                         , 5  , 0  , 6841 , 5  , 6  ), // #266
  INST(Hlt              , X86Op              , O(000000,F4,_,_,_,_,_,_  ), 0                         , 0  , 0  , 1226 , 30 , 0  ), // #267
  INST(Hreset           , X86Op_Mod11RM_I8   , O(F30F3A,F0,0,_,_,_,_,_  ), 0                         , 61 , 0  , 1230 , 73 , 55 ), // #268
  INST(Hsubpd           , ExtRm              , O(660F00,7D,_,_,_,_,_,_  ), 0                         , 3  , 0  , 6849 , 5  , 6  ), // #269
  INST(Hsubps           , ExtRm              , O(F20F00,7D,_,_,_,_,_,_  ), 0                         , 5  , 0  , 6857 , 5  , 6  ), // #270
  INST(Idiv             , X86M_GPB_MulDiv    , O(000000,F6,7,_,x,_,_,_  ), 0                         , 27 , 0  , 809  , 54 , 1  ), // #271
  INST(Imul             , X86Imul            , O(000000,F6,5,_,x,_,_,_  ), 0                         , 62 , 0  , 827  , 74 , 1  ), // #272
  INST(In               , X86In              , O(000000,EC,_,_,_,_,_,_  ), O(000000,E4,_,_,_,_,_,_  ), 0  , 15 , 11572, 75 , 0  ), // #273
  INST(Inc              , X86IncDec          , O(000000,FE,0,_,x,_,_,_  ), O(000000,40,_,_,x,_,_,_  ), 0  , 16 , 1237 , 53 , 45 ), // #274
  INST(Incsspd          , X86M               , O(F30F00,AE,5,_,0,_,_,_  ), 0                         , 63 , 0  , 1241 , 76 , 56 ), // #275
  INST(Incsspq          , X86M               , O(F30F00,AE,5,_,1,_,_,_  ), 0                         , 64 , 0  , 1249 , 77 , 56 ), // #276
  INST(Ins              , X86Ins             , O(000000,6C,_,_,_,_,_,_  ), 0                         , 0  , 0  , 1916 , 78 , 0  ), // #277
  INST(Insertps         , ExtRmi             , O(660F3A,21,_,_,_,_,_,_  ), 0                         , 8  , 0  , 6993 , 38 , 12 ), // #278
  INST(Insertq          , ExtInsertq         , O(F20F00,79,_,_,_,_,_,_  ), O(F20F00,78,_,_,_,_,_,_  ), 5  , 17 , 1257 , 79 , 49 ), // #279
  INST(Int              , X86Int             , O(000000,CD,_,_,_,_,_,_  ), 0                         , 0  , 0  , 1022 , 80 , 0  ), // #280
  INST(Int3             , X86Op              , O(000000,CC,_,_,_,_,_,_  ), 0                         , 0  , 0  , 1265 , 30 , 0  ), // #281
  INST(Into             , X86Op              , O(000000,CE,_,_,_,_,_,_  ), 0                         , 0  , 0  , 1270 , 81 , 57 ), // #282
  INST(Invd             , X86Op              , O(000F00,08,_,_,_,_,_,_  ), 0                         , 4  , 0  , 11501, 30 , 43 ), // #283
  INST(Invept           , X86Rm_NoSize       , O(660F38,80,_,_,_,_,_,_  ), 0                         , 2  , 0  , 1275 , 82 , 58 ), // #284
  INST(Invlpg           , X86M_Only          , O(000F00,01,7,_,_,_,_,_  ), 0                         , 22 , 0  , 1282 , 69 , 43 ), // #285
  INST(Invlpga          , X86Op_xAddr        , O(000F01,DF,_,_,_,_,_,_  ), 0                         , 21 , 0  , 1289 , 83 , 22 ), // #286
  INST(Invpcid          , X86Rm_NoSize       , O(660F38,82,_,_,_,_,_,_  ), 0                         , 2  , 0  , 1297 , 82 , 43 ), // #287
  INST(Invvpid          , X86Rm_NoSize       , O(660F38,81,_,_,_,_,_,_  ), 0                         , 2  , 0  , 1305 , 82 , 58 ), // #288
  INST(Iret             , X86Op              , O(660000,CF,_,_,_,_,_,_  ), 0                         , 19 , 0  , 3226 , 84 , 1  ), // #289
  INST(Iretd            , X86Op              , O(000000,CF,_,_,_,_,_,_  ), 0                         , 0  , 0  , 1313 , 84 , 1  ), // #290
  INST(Iretq            , X86Op              , O(000000,CF,_,_,1,_,_,_  ), 0                         , 20 , 0  , 1319 , 85 , 1  ), // #291
  INST(Ja               , X86Jcc             , O(000F00,87,_,_,_,_,_,_  ), O(000000,77,_,_,_,_,_,_  ), 4  , 18 , 1325 , 86 , 59 ), // #292
  INST(Jae              , X86Jcc             , O(000F00,83,_,_,_,_,_,_  ), O(000000,73,_,_,_,_,_,_  ), 4  , 19 , 1328 , 86 , 60 ), // #293
  INST(Jb               , X86Jcc             , O(000F00,82,_,_,_,_,_,_  ), O(000000,72,_,_,_,_,_,_  ), 4  , 20 , 1332 , 86 , 60 ), // #294
  INST(Jbe              , X86Jcc             , O(000F00,86,_,_,_,_,_,_  ), O(000000,76,_,_,_,_,_,_  ), 4  , 21 , 1335 , 86 , 59 ), // #295
  INST(Jc               , X86Jcc             , O(000F00,82,_,_,_,_,_,_  ), O(000000,72,_,_,_,_,_,_  ), 4  , 20 , 1339 , 86 , 60 ), // #296
  INST(Je               , X86Jcc             , O(000F00,84,_,_,_,_,_,_  ), O(000000,74,_,_,_,_,_,_  ), 4  , 22 , 1342 , 86 , 61 ), // #297
  INST(Jecxz            , X86JecxzLoop       , 0                         , O(000000,E3,_,_,_,_,_,_  ), 0  , 23 , 1345 , 87 , 0  ), // #298
  INST(Jg               , X86Jcc             , O(000F00,8F,_,_,_,_,_,_  ), O(000000,7F,_,_,_,_,_,_  ), 4  , 24 , 1351 , 86 , 62 ), // #299
  INST(Jge              , X86Jcc             , O(000F00,8D,_,_,_,_,_,_  ), O(000000,7D,_,_,_,_,_,_  ), 4  , 25 , 1354 , 86 , 63 ), // #300
  INST(Jl               , X86Jcc             , O(000F00,8C,_,_,_,_,_,_  ), O(000000,7C,_,_,_,_,_,_  ), 4  , 26 , 1358 , 86 , 63 ), // #301
  INST(Jle              , X86Jcc             , O(000F00,8E,_,_,_,_,_,_  ), O(000000,7E,_,_,_,_,_,_  ), 4  , 27 , 1361 , 86 , 62 ), // #302
  INST(Jmp              , X86Jmp             , O(000000,FF,4,_,_,_,_,_  ), O(000000,EB,_,_,_,_,_,_  ), 9  , 28 , 1861 , 88 , 0  ), // #303
  INST(Jna              , X86Jcc             , O(000F00,86,_,_,_,_,_,_  ), O(000000,76,_,_,_,_,_,_  ), 4  , 21 , 1365 , 86 , 59 ), // #304
  INST(Jnae             , X86Jcc             , O(000F00,82,_,_,_,_,_,_  ), O(000000,72,_,_,_,_,_,_  ), 4  , 20 , 1369 , 86 , 60 ), // #305
  INST(Jnb              , X86Jcc             , O(000F00,83,_,_,_,_,_,_  ), O(000000,73,_,_,_,_,_,_  ), 4  , 19 , 1374 , 86 , 60 ), // #306
  INST(Jnbe             , X86Jcc             , O(000F00,87,_,_,_,_,_,_  ), O(000000,77,_,_,_,_,_,_  ), 4  , 18 , 1378 , 86 , 59 ), // #307
  INST(Jnc              , X86Jcc             , O(000F00,83,_,_,_,_,_,_  ), O(000000,73,_,_,_,_,_,_  ), 4  , 19 , 1383 , 86 , 60 ), // #308
  INST(Jne              , X86Jcc             , O(000F00,85,_,_,_,_,_,_  ), O(000000,75,_,_,_,_,_,_  ), 4  , 29 , 1387 , 86 , 61 ), // #309
  INST(Jng              , X86Jcc             , O(000F00,8E,_,_,_,_,_,_  ), O(000000,7E,_,_,_,_,_,_  ), 4  , 27 , 1391 , 86 , 62 ), // #310
  INST(Jnge             , X86Jcc             , O(000F00,8C,_,_,_,_,_,_  ), O(000000,7C,_,_,_,_,_,_  ), 4  , 26 , 1395 , 86 , 63 ), // #311
  INST(Jnl              , X86Jcc             , O(000F00,8D,_,_,_,_,_,_  ), O(000000,7D,_,_,_,_,_,_  ), 4  , 25 , 1400 , 86 , 63 ), // #312
  INST(Jnle             , X86Jcc             , O(000F00,8F,_,_,_,_,_,_  ), O(000000,7F,_,_,_,_,_,_  ), 4  , 24 , 1404 , 86 , 62 ), // #313
  INST(Jno              , X86Jcc             , O(000F00,81,_,_,_,_,_,_  ), O(000000,71,_,_,_,_,_,_  ), 4  , 30 , 1409 , 86 , 57 ), // #314
  INST(Jnp              , X86Jcc             , O(000F00,8B,_,_,_,_,_,_  ), O(000000,7B,_,_,_,_,_,_  ), 4  , 31 , 1413 , 86 , 64 ), // #315
  INST(Jns              , X86Jcc             , O(000F00,89,_,_,_,_,_,_  ), O(000000,79,_,_,_,_,_,_  ), 4  , 32 , 1417 , 86 , 65 ), // #316
  INST(Jnz              , X86Jcc             , O(000F00,85,_,_,_,_,_,_  ), O(000000,75,_,_,_,_,_,_  ), 4  , 29 , 1421 , 86 , 61 ), // #317
  INST(Jo               , X86Jcc             , O(000F00,80,_,_,_,_,_,_  ), O(000000,70,_,_,_,_,_,_  ), 4  , 33 , 1425 , 86 , 57 ), // #318
  INST(Jp               , X86Jcc             , O(000F00,8A,_,_,_,_,_,_  ), O(000000,7A,_,_,_,_,_,_  ), 4  , 34 , 1428 , 86 , 64 ), // #319
  INST(Jpe              , X86Jcc             , O(000F00,8A,_,_,_,_,_,_  ), O(000000,7A,_,_,_,_,_,_  ), 4  , 34 , 1431 , 86 , 64 ), // #320
  INST(Jpo              , X86Jcc             , O(000F00,8B,_,_,_,_,_,_  ), O(000000,7B,_,_,_,_,_,_  ), 4  , 31 , 1435 , 86 , 64 ), // #321
  INST(Js               , X86Jcc             , O(000F00,88,_,_,_,_,_,_  ), O(000000,78,_,_,_,_,_,_  ), 4  , 35 , 1439 , 86 , 65 ), // #322
  INST(Jz               , X86Jcc             , O(000F00,84,_,_,_,_,_,_  ), O(000000,74,_,_,_,_,_,_  ), 4  , 22 , 1442 , 86 , 61 ), // #323
  INST(Kaddb            , VexRvm             , V(660F00,4A,_,1,0,_,_,_  ), 0                         , 65 , 0  , 1445 , 89 , 66 ), // #324
  INST(Kaddd            , VexRvm             , V(660F00,4A,_,1,1,_,_,_  ), 0                         , 66 , 0  , 1451 , 89 , 67 ), // #325
  INST(Kaddq            , VexRvm             , V(000F00,4A,_,1,1,_,_,_  ), 0                         , 67 , 0  , 1457 , 89 , 67 ), // #326
  INST(Kaddw            , VexRvm             , V(000F00,4A,_,1,0,_,_,_  ), 0                         , 68 , 0  , 1463 , 89 , 66 ), // #327
  INST(Kandb            , VexRvm             , V(660F00,41,_,1,0,_,_,_  ), 0                         , 65 , 0  , 1469 , 89 , 66 ), // #328
  INST(Kandd            , VexRvm             , V(660F00,41,_,1,1,_,_,_  ), 0                         , 66 , 0  , 1475 , 89 , 67 ), // #329
  INST(Kandnb           , VexRvm             , V(660F00,42,_,1,0,_,_,_  ), 0                         , 65 , 0  , 1481 , 89 , 66 ), // #330
  INST(Kandnd           , VexRvm             , V(660F00,42,_,1,1,_,_,_  ), 0                         , 66 , 0  , 1488 , 89 , 67 ), // #331
  INST(Kandnq           , VexRvm             , V(000F00,42,_,1,1,_,_,_  ), 0                         , 67 , 0  , 1495 , 89 , 67 ), // #332
  INST(Kandnw           , VexRvm             , V(000F00,42,_,1,0,_,_,_  ), 0                         , 68 , 0  , 1502 , 89 , 68 ), // #333
  INST(Kandq            , VexRvm             , V(000F00,41,_,1,1,_,_,_  ), 0                         , 67 , 0  , 1509 , 89 , 67 ), // #334
  INST(Kandw            , VexRvm             , V(000F00,41,_,1,0,_,_,_  ), 0                         , 68 , 0  , 1515 , 89 , 68 ), // #335
  INST(Kmovb            , VexKmov            , V(660F00,90,_,0,0,_,_,_  ), V(660F00,92,_,0,0,_,_,_  ), 69 , 36 , 1521 , 90 , 66 ), // #336
  INST(Kmovd            , VexKmov            , V(660F00,90,_,0,1,_,_,_  ), V(F20F00,92,_,0,0,_,_,_  ), 70 , 37 , 9105 , 91 , 67 ), // #337
  INST(Kmovq            , VexKmov            , V(000F00,90,_,0,1,_,_,_  ), V(F20F00,92,_,0,1,_,_,_  ), 71 , 38 , 9116 , 92 , 67 ), // #338
  INST(Kmovw            , VexKmov            , V(000F00,90,_,0,0,_,_,_  ), V(000F00,92,_,0,0,_,_,_  ), 72 , 39 , 1527 , 93 , 68 ), // #339
  INST(Knotb            , VexRm              , V(660F00,44,_,0,0,_,_,_  ), 0                         , 69 , 0  , 1533 , 94 , 66 ), // #340
  INST(Knotd            , VexRm              , V(660F00,44,_,0,1,_,_,_  ), 0                         , 70 , 0  , 1539 , 94 , 67 ), // #341
  INST(Knotq            , VexRm              , V(000F00,44,_,0,1,_,_,_  ), 0                         , 71 , 0  , 1545 , 94 , 67 ), // #342
  INST(Knotw            , VexRm              , V(000F00,44,_,0,0,_,_,_  ), 0                         , 72 , 0  , 1551 , 94 , 68 ), // #343
  INST(Korb             , VexRvm             , V(660F00,45,_,1,0,_,_,_  ), 0                         , 65 , 0  , 1557 , 89 , 66 ), // #344
  INST(Kord             , VexRvm             , V(660F00,45,_,1,1,_,_,_  ), 0                         , 66 , 0  , 1562 , 89 , 67 ), // #345
  INST(Korq             , VexRvm             , V(000F00,45,_,1,1,_,_,_  ), 0                         , 67 , 0  , 1567 , 89 , 67 ), // #346
  INST(Kortestb         , VexRm              , V(660F00,98,_,0,0,_,_,_  ), 0                         , 69 , 0  , 1572 , 94 , 69 ), // #347
  INST(Kortestd         , VexRm              , V(660F00,98,_,0,1,_,_,_  ), 0                         , 70 , 0  , 1581 , 94 , 70 ), // #348
  INST(Kortestq         , VexRm              , V(000F00,98,_,0,1,_,_,_  ), 0                         , 71 , 0  , 1590 , 94 , 70 ), // #349
  INST(Kortestw         , VexRm              , V(000F00,98,_,0,0,_,_,_  ), 0                         , 72 , 0  , 1599 , 94 , 71 ), // #350
  INST(Korw             , VexRvm             , V(000F00,45,_,1,0,_,_,_  ), 0                         , 68 , 0  , 1608 , 89 , 68 ), // #351
  INST(Kshiftlb         , VexRmi             , V(660F3A,32,_,0,0,_,_,_  ), 0                         , 73 , 0  , 1613 , 95 , 66 ), // #352
  INST(Kshiftld         , VexRmi             , V(660F3A,33,_,0,0,_,_,_  ), 0                         , 73 , 0  , 1622 , 95 , 67 ), // #353
  INST(Kshiftlq         , VexRmi             , V(660F3A,33,_,0,1,_,_,_  ), 0                         , 74 , 0  , 1631 , 95 , 67 ), // #354
  INST(Kshiftlw         , VexRmi             , V(660F3A,32,_,0,1,_,_,_  ), 0                         , 74 , 0  , 1640 , 95 , 68 ), // #355
  INST(Kshiftrb         , VexRmi             , V(660F3A,30,_,0,0,_,_,_  ), 0                         , 73 , 0  , 1649 , 95 , 66 ), // #356
  INST(Kshiftrd         , VexRmi             , V(660F3A,31,_,0,0,_,_,_  ), 0                         , 73 , 0  , 1658 , 95 , 67 ), // #357
  INST(Kshiftrq         , VexRmi             , V(660F3A,31,_,0,1,_,_,_  ), 0                         , 74 , 0  , 1667 , 95 , 67 ), // #358
  INST(Kshiftrw         , VexRmi             , V(660F3A,30,_,0,1,_,_,_  ), 0                         , 74 , 0  , 1676 , 95 , 68 ), // #359
  INST(Ktestb           , VexRm              , V(660F00,99,_,0,0,_,_,_  ), 0                         , 69 , 0  , 1685 , 94 , 69 ), // #360
  INST(Ktestd           , VexRm              , V(660F00,99,_,0,1,_,_,_  ), 0                         , 70 , 0  , 1692 , 94 , 70 ), // #361
  INST(Ktestq           , VexRm              , V(000F00,99,_,0,1,_,_,_  ), 0                         , 71 , 0  , 1699 , 94 , 70 ), // #362
  INST(Ktestw           , VexRm              , V(000F00,99,_,0,0,_,_,_  ), 0                         , 72 , 0  , 1706 , 94 , 69 ), // #363
  INST(Kunpckbw         , VexRvm             , V(660F00,4B,_,1,0,_,_,_  ), 0                         , 65 , 0  , 1713 , 89 , 68 ), // #364
  INST(Kunpckdq         , VexRvm             , V(000F00,4B,_,1,1,_,_,_  ), 0                         , 67 , 0  , 1722 , 89 , 67 ), // #365
  INST(Kunpckwd         , VexRvm             , V(000F00,4B,_,1,0,_,_,_  ), 0                         , 68 , 0  , 1731 , 89 , 67 ), // #366
  INST(Kxnorb           , VexRvm             , V(660F00,46,_,1,0,_,_,_  ), 0                         , 65 , 0  , 1740 , 96 , 66 ), // #367
  INST(Kxnord           , VexRvm             , V(660F00,46,_,1,1,_,_,_  ), 0                         , 66 , 0  , 1747 , 96 , 67 ), // #368
  INST(Kxnorq           , VexRvm             , V(000F00,46,_,1,1,_,_,_  ), 0                         , 67 , 0  , 1754 , 96 , 67 ), // #369
  INST(Kxnorw           , VexRvm             , V(000F00,46,_,1,0,_,_,_  ), 0                         , 68 , 0  , 1761 , 96 , 68 ), // #370
  INST(Kxorb            , VexRvm             , V(660F00,47,_,1,0,_,_,_  ), 0                         , 65 , 0  , 1768 , 96 , 66 ), // #371
  INST(Kxord            , VexRvm             , V(660F00,47,_,1,1,_,_,_  ), 0                         , 66 , 0  , 1774 , 96 , 67 ), // #372
  INST(Kxorq            , VexRvm             , V(000F00,47,_,1,1,_,_,_  ), 0                         , 67 , 0  , 1780 , 96 , 67 ), // #373
  INST(Kxorw            , VexRvm             , V(000F00,47,_,1,0,_,_,_  ), 0                         , 68 , 0  , 1786 , 96 , 68 ), // #374
  INST(Lahf             , X86Op              , O(000000,9F,_,_,_,_,_,_  ), 0                         , 0  , 0  , 1792 , 97 , 72 ), // #375
  INST(Lar              , X86Rm              , O(000F00,02,_,_,_,_,_,_  ), 0                         , 4  , 0  , 1797 , 98 , 10 ), // #376
  INST(Lcall            , X86LcallLjmp       , O(000000,FF,3,_,_,_,_,_  ), O(000000,9A,_,_,_,_,_,_  ), 75 , 40 , 1801 , 99 , 1  ), // #377
  INST(Lddqu            , ExtRm              , O(F20F00,F0,_,_,_,_,_,_  ), 0                         , 5  , 0  , 7003 , 100, 6  ), // #378
  INST(Ldmxcsr          , X86M_Only          , O(000F00,AE,2,_,_,_,_,_  ), 0                         , 76 , 0  , 7010 , 101, 5  ), // #379
  INST(Lds              , X86Rm              , O(000000,C5,_,_,_,_,_,_  ), 0                         , 0  , 0  , 1807 , 102, 0  ), // #380
  INST(Ldtilecfg        , AmxCfg             , V(000F38,49,_,0,0,_,_,_  ), 0                         , 10 , 0  , 1811 , 103, 73 ), // #381
  INST(Lea              , X86Lea             , O(000000,8D,_,_,x,_,_,_  ), 0                         , 0  , 0  , 1821 , 104, 0  ), // #382
  INST(Leave            , X86Op              , O(000000,C9,_,_,_,_,_,_  ), 0                         , 0  , 0  , 1825 , 30 , 0  ), // #383
  INST(Les              , X86Rm              , O(000000,C4,_,_,_,_,_,_  ), 0                         , 0  , 0  , 1831 , 102, 0  ), // #384
  INST(Lfence           , X86Fence           , O(000F00,AE,5,_,_,_,_,_  ), 0                         , 77 , 0  , 1835 , 30 , 4  ), // #385
  INST(Lfs              , X86Rm              , O(000F00,B4,_,_,_,_,_,_  ), 0                         , 4  , 0  , 1842 , 105, 0  ), // #386
  INST(Lgdt             , X86M_Only          , O(000F00,01,2,_,_,_,_,_  ), 0                         , 76 , 0  , 1846 , 69 , 0  ), // #387
  INST(Lgs              , X86Rm              , O(000F00,B5,_,_,_,_,_,_  ), 0                         , 4  , 0  , 1851 , 105, 0  ), // #388
  INST(Lidt             , X86M_Only          , O(000F00,01,3,_,_,_,_,_  ), 0                         , 78 , 0  , 1855 , 69 , 0  ), // #389
  INST(Ljmp             , X86LcallLjmp       , O(000000,FF,5,_,_,_,_,_  ), O(000000,EA,_,_,_,_,_,_  ), 62 , 41 , 1860 , 106, 0  ), // #390
  INST(Lldt             , X86M_NoSize        , O(000F00,00,2,_,_,_,_,_  ), 0                         , 76 , 0  , 1865 , 107, 0  ), // #391
  INST(Llwpcb           , VexR_Wx            , V(XOP_M9,12,0,0,x,_,_,_  ), 0                         , 79 , 0  , 1870 , 108, 74 ), // #392
  INST(Lmsw             , X86M_NoSize        , O(000F00,01,6,_,_,_,_,_  ), 0                         , 80 , 0  , 1877 , 107, 0  ), // #393
  INST(Lods             , X86StrRm           , O(000000,AC,_,_,_,_,_,_  ), 0                         , 0  , 0  , 1882 , 109, 75 ), // #394
  INST(Loop             , X86JecxzLoop       , 0                         , O(000000,E2,_,_,_,_,_,_  ), 0  , 42 , 1887 , 110, 0  ), // #395
  INST(Loope            , X86JecxzLoop       , 0                         , O(000000,E1,_,_,_,_,_,_  ), 0  , 43 , 1892 , 110, 61 ), // #396
  INST(Loopne           , X86JecxzLoop       , 0                         , O(000000,E0,_,_,_,_,_,_  ), 0  , 44 , 1898 , 110, 61 ), // #397
  INST(Lsl              , X86Rm              , O(000F00,03,_,_,_,_,_,_  ), 0                         , 4  , 0  , 1905 , 111, 10 ), // #398
  INST(Lss              , X86Rm              , O(000F00,B2,_,_,_,_,_,_  ), 0                         , 4  , 0  , 7556 , 105, 0  ), // #399
  INST(Ltr              , X86M_NoSize        , O(000F00,00,3,_,_,_,_,_  ), 0                         , 78 , 0  , 1909 , 107, 0  ), // #400
  INST(Lwpins           , VexVmi4_Wx         , V(XOP_MA,12,0,0,x,_,_,_  ), 0                         , 81 , 0  , 1913 , 112, 74 ), // #401
  INST(Lwpval           , VexVmi4_Wx         , V(XOP_MA,12,1,0,x,_,_,_  ), 0                         , 82 , 0  , 1920 , 112, 74 ), // #402
  INST(Lzcnt            , X86Rm_Raw66H       , O(F30F00,BD,_,_,x,_,_,_  ), 0                         , 6  , 0  , 1927 , 22 , 76 ), // #403
  INST(Maskmovdqu       , ExtRm_ZDI          , O(660F00,F7,_,_,_,_,_,_  ), 0                         , 3  , 0  , 7019 , 113, 4  ), // #404
  INST(Maskmovq         , ExtRm_ZDI          , O(000F00,F7,_,_,_,_,_,_  ), 0                         , 4  , 0  , 9113 , 114, 77 ), // #405
  INST(Maxpd            , ExtRm              , O(660F00,5F,_,_,_,_,_,_  ), 0                         , 3  , 0  , 7053 , 5  , 4  ), // #406
  INST(Maxps            , ExtRm              , O(000F00,5F,_,_,_,_,_,_  ), 0                         , 4  , 0  , 7067 , 5  , 5  ), // #407
  INST(Maxsd            , ExtRm              , O(F20F00,5F,_,_,_,_,_,_  ), 0                         , 5  , 0  , 9132 , 6  , 4  ), // #408
  INST(Maxss            , ExtRm              , O(F30F00,5F,_,_,_,_,_,_  ), 0                         , 6  , 0  , 7088 , 7  , 5  ), // #409
  INST(Mcommit          , X86Op              , O(F30F01,FA,_,_,_,_,_,_  ), 0                         , 25 , 0  , 1933 , 30 , 78 ), // #410
  INST(Mfence           , X86Fence           , O(000F00,AE,6,_,_,_,_,_  ), 0                         , 80 , 0  , 1941 , 30 , 4  ), // #411
  INST(Minpd            , ExtRm              , O(660F00,5D,_,_,_,_,_,_  ), 0                         , 3  , 0  , 7117 , 5  , 4  ), // #412
  INST(Minps            , ExtRm              , O(000F00,5D,_,_,_,_,_,_  ), 0                         , 4  , 0  , 7131 , 5  , 5  ), // #413
  INST(Minsd            , ExtRm              , O(F20F00,5D,_,_,_,_,_,_  ), 0                         , 5  , 0  , 9196 , 6  , 4  ), // #414
  INST(Minss            , ExtRm              , O(F30F00,5D,_,_,_,_,_,_  ), 0                         , 6  , 0  , 7152 , 7  , 5  ), // #415
  INST(Monitor          , X86Op              , O(000F01,C8,_,_,_,_,_,_  ), 0                         , 21 , 0  , 3232 , 115, 79 ), // #416
  INST(Monitorx         , X86Op              , O(000F01,FA,_,_,_,_,_,_  ), 0                         , 21 , 0  , 1948 , 115, 80 ), // #417
  INST(Mov              , X86Mov             , 0                         , 0                         , 0  , 0  , 138  , 116, 0  ), // #418
  INST(Movabs           , X86Movabs          , 0                         , 0                         , 0  , 0  , 1957 , 117, 0  ), // #419
  INST(Movapd           , ExtMov             , O(660F00,28,_,_,_,_,_,_  ), O(660F00,29,_,_,_,_,_,_  ), 3  , 45 , 7183 , 118, 4  ), // #420
  INST(Movaps           , ExtMov             , O(000F00,28,_,_,_,_,_,_  ), O(000F00,29,_,_,_,_,_,_  ), 4  , 46 , 7191 , 118, 5  ), // #421
  INST(Movbe            , ExtMovbe           , O(000F38,F0,_,_,x,_,_,_  ), O(000F38,F1,_,_,x,_,_,_  ), 83 , 47 , 656  , 119, 81 ), // #422
  INST(Movd             , ExtMovd            , O(000F00,6E,_,_,_,_,_,_  ), O(000F00,7E,_,_,_,_,_,_  ), 4  , 48 , 9106 , 120, 82 ), // #423
  INST(Movddup          , ExtMov             , O(F20F00,12,_,_,_,_,_,_  ), 0                         , 5  , 0  , 7205 , 6  , 6  ), // #424
  INST(Movdir64b        , X86EnqcmdMovdir64b , O(660F38,F8,_,_,_,_,_,_  ), 0                         , 2  , 0  , 1964 , 121, 83 ), // #425
  INST(Movdiri          , X86MovntiMovdiri   , O(000F38,F9,_,_,_,_,_,_  ), 0                         , 83 , 0  , 1974 , 122, 84 ), // #426
  INST(Movdq2q          , ExtMov             , O(F20F00,D6,_,_,_,_,_,_  ), 0                         , 5  , 0  , 1982 , 123, 4  ), // #427
  INST(Movdqa           , ExtMov             , O(660F00,6F,_,_,_,_,_,_  ), O(660F00,7F,_,_,_,_,_,_  ), 3  , 49 , 7214 , 118, 4  ), // #428
  INST(Movdqu           , ExtMov             , O(F30F00,6F,_,_,_,_,_,_  ), O(F30F00,7F,_,_,_,_,_,_  ), 6  , 50 , 7023 , 118, 4  ), // #429
  INST(Movhlps          , ExtMov             , O(000F00,12,_,_,_,_,_,_  ), 0                         , 4  , 0  , 7289 , 124, 5  ), // #430
  INST(Movhpd           , ExtMov             , O(660F00,16,_,_,_,_,_,_  ), O(660F00,17,_,_,_,_,_,_  ), 3  , 51 , 7298 , 125, 4  ), // #431
  INST(Movhps           , ExtMov             , O(000F00,16,_,_,_,_,_,_  ), O(000F00,17,_,_,_,_,_,_  ), 4  , 52 , 7306 , 125, 5  ), // #432
  INST(Movlhps          , ExtMov             , O(000F00,16,_,_,_,_,_,_  ), 0                         , 4  , 0  , 7314 , 124, 5  ), // #433
  INST(Movlpd           , ExtMov             , O(660F00,12,_,_,_,_,_,_  ), O(660F00,13,_,_,_,_,_,_  ), 3  , 53 , 7323 , 125, 4  ), // #434
  INST(Movlps           , ExtMov             , O(000F00,12,_,_,_,_,_,_  ), O(000F00,13,_,_,_,_,_,_  ), 4  , 54 , 7331 , 125, 5  ), // #435
  INST(Movmskpd         , ExtMov             , O(660F00,50,_,_,_,_,_,_  ), 0                         , 3  , 0  , 7339 , 126, 4  ), // #436
  INST(Movmskps         , ExtMov             , O(000F00,50,_,_,_,_,_,_  ), 0                         , 4  , 0  , 7349 , 126, 5  ), // #437
  INST(Movntdq          , ExtMov             , 0                         , O(660F00,E7,_,_,_,_,_,_  ), 0  , 55 , 7359 , 127, 4  ), // #438
  INST(Movntdqa         , ExtMov             , O(660F38,2A,_,_,_,_,_,_  ), 0                         , 2  , 0  , 7368 , 100, 12 ), // #439
  INST(Movnti           , X86MovntiMovdiri   , O(000F00,C3,_,_,x,_,_,_  ), 0                         , 4  , 0  , 1990 , 122, 4  ), // #440
  INST(Movntpd          , ExtMov             , 0                         , O(660F00,2B,_,_,_,_,_,_  ), 0  , 56 , 7378 , 127, 4  ), // #441
  INST(Movntps          , ExtMov             , 0                         , O(000F00,2B,_,_,_,_,_,_  ), 0  , 57 , 7387 , 127, 5  ), // #442
  INST(Movntq           , ExtMov             , 0                         , O(000F00,E7,_,_,_,_,_,_  ), 0  , 58 , 1997 , 128, 77 ), // #443
  INST(Movntsd          , ExtMov             , 0                         , O(F20F00,2B,_,_,_,_,_,_  ), 0  , 59 , 2004 , 129, 49 ), // #444
  INST(Movntss          , ExtMov             , 0                         , O(F30F00,2B,_,_,_,_,_,_  ), 0  , 60 , 2012 , 130, 49 ), // #445
  INST(Movq             , ExtMovq            , O(000F00,6E,_,_,x,_,_,_  ), O(000F00,7E,_,_,x,_,_,_  ), 4  , 48 , 9117 , 131, 82 ), // #446
  INST(Movq2dq          , ExtRm              , O(F30F00,D6,_,_,_,_,_,_  ), 0                         , 6  , 0  , 2020 , 132, 4  ), // #447
  INST(Movs             , X86StrMm           , O(000000,A4,_,_,_,_,_,_  ), 0                         , 0  , 0  , 439  , 133, 75 ), // #448
  INST(Movsd            , ExtMov             , O(F20F00,10,_,_,_,_,_,_  ), O(F20F00,11,_,_,_,_,_,_  ), 5  , 61 , 7402 , 134, 4  ), // #449
  INST(Movshdup         , ExtRm              , O(F30F00,16,_,_,_,_,_,_  ), 0                         , 6  , 0  , 7416 , 5  , 6  ), // #450
  INST(Movsldup         , ExtRm              , O(F30F00,12,_,_,_,_,_,_  ), 0                         , 6  , 0  , 7426 , 5  , 6  ), // #451
  INST(Movss            , ExtMov             , O(F30F00,10,_,_,_,_,_,_  ), O(F30F00,11,_,_,_,_,_,_  ), 6  , 62 , 7436 , 135, 5  ), // #452
  INST(Movsx            , X86MovsxMovzx      , O(000F00,BE,_,_,x,_,_,_  ), 0                         , 4  , 0  , 2028 , 136, 0  ), // #453
  INST(Movsxd           , X86Rm              , O(000000,63,_,_,x,_,_,_  ), 0                         , 0  , 0  , 2034 , 137, 0  ), // #454
  INST(Movupd           , ExtMov             , O(660F00,10,_,_,_,_,_,_  ), O(660F00,11,_,_,_,_,_,_  ), 3  , 63 , 7443 , 118, 4  ), // #455
  INST(Movups           , ExtMov             , O(000F00,10,_,_,_,_,_,_  ), O(000F00,11,_,_,_,_,_,_  ), 4  , 64 , 7451 , 118, 5  ), // #456
  INST(Movzx            , X86MovsxMovzx      , O(000F00,B6,_,_,x,_,_,_  ), 0                         , 4  , 0  , 2041 , 136, 0  ), // #457
  INST(Mpsadbw          , ExtRmi             , O(660F3A,42,_,_,_,_,_,_  ), 0                         , 8  , 0  , 7465 , 8  , 12 ), // #458
  INST(Mul              , X86M_GPB_MulDiv    , O(000000,F6,4,_,x,_,_,_  ), 0                         , 9  , 0  , 828  , 54 , 1  ), // #459
  INST(Mulpd            , ExtRm              , O(660F00,59,_,_,_,_,_,_  ), 0                         , 3  , 0  , 7519 , 5  , 4  ), // #460
  INST(Mulps            , ExtRm              , O(000F00,59,_,_,_,_,_,_  ), 0                         , 4  , 0  , 7533 , 5  , 5  ), // #461
  INST(Mulsd            , ExtRm              , O(F20F00,59,_,_,_,_,_,_  ), 0                         , 5  , 0  , 7540 , 6  , 4  ), // #462
  INST(Mulss            , ExtRm              , O(F30F00,59,_,_,_,_,_,_  ), 0                         , 6  , 0  , 7554 , 7  , 5  ), // #463
  INST(Mulx             , VexRvm_ZDX_Wx      , V(F20F38,F6,_,0,x,_,_,_  ), 0                         , 84 , 0  , 2047 , 138, 85 ), // #464
  INST(Mwait            , X86Op              , O(000F01,C9,_,_,_,_,_,_  ), 0                         , 21 , 0  , 3241 , 139, 79 ), // #465
  INST(Mwaitx           , X86Op              , O(000F01,FB,_,_,_,_,_,_  ), 0                         , 21 , 0  , 2052 , 140, 80 ), // #466
  INST(Neg              , X86M_GPB           , O(000000,F6,3,_,x,_,_,_  ), 0                         , 75 , 0  , 2059 , 141, 1  ), // #467
  INST(Nop              , X86M_Nop           , O(000000,90,_,_,_,_,_,_  ), 0                         , 0  , 0  , 959  , 142, 0  ), // #468
  INST(Not              , X86M_GPB           , O(000000,F6,2,_,x,_,_,_  ), 0                         , 1  , 0  , 2063 , 141, 0  ), // #469
  INST(Or               , X86Arith           , O(000000,08,1,_,x,_,_,_  ), 0                         , 31 , 0  , 3237 , 143, 1  ), // #470
  INST(Orpd             , ExtRm              , O(660F00,56,_,_,_,_,_,_  ), 0                         , 3  , 0  , 11458, 11 , 4  ), // #471
  INST(Orps             , ExtRm              , O(000F00,56,_,_,_,_,_,_  ), 0                         , 4  , 0  , 11465, 11 , 5  ), // #472
  INST(Out              , X86Out             , O(000000,EE,_,_,_,_,_,_  ), O(000000,E6,_,_,_,_,_,_  ), 0  , 65 , 2067 , 144, 0  ), // #473
  INST(Outs             , X86Outs            , O(000000,6E,_,_,_,_,_,_  ), 0                         , 0  , 0  , 2071 , 145, 0  ), // #474
  INST(Pabsb            , ExtRm_P            , O(000F38,1C,_,_,_,_,_,_  ), 0                         , 83 , 0  , 7636 , 146, 86 ), // #475
  INST(Pabsd            , ExtRm_P            , O(000F38,1E,_,_,_,_,_,_  ), 0                         , 83 , 0  , 7643 , 146, 86 ), // #476
  INST(Pabsw            , ExtRm_P            , O(000F38,1D,_,_,_,_,_,_  ), 0                         , 83 , 0  , 7657 , 146, 86 ), // #477
  INST(Packssdw         , ExtRm_P            , O(000F00,6B,_,_,_,_,_,_  ), 0                         , 4  , 0  , 7664 , 146, 82 ), // #478
  INST(Packsswb         , ExtRm_P            , O(000F00,63,_,_,_,_,_,_  ), 0                         , 4  , 0  , 7674 , 146, 82 ), // #479
  INST(Packusdw         , ExtRm              , O(660F38,2B,_,_,_,_,_,_  ), 0                         , 2  , 0  , 7684 , 5  , 12 ), // #480
  INST(Packuswb         , ExtRm_P            , O(000F00,67,_,_,_,_,_,_  ), 0                         , 4  , 0  , 7694 , 146, 82 ), // #481
  INST(Paddb            , ExtRm_P            , O(000F00,FC,_,_,_,_,_,_  ), 0                         , 4  , 0  , 7704 , 146, 82 ), // #482
  INST(Paddd            , ExtRm_P            , O(000F00,FE,_,_,_,_,_,_  ), 0                         , 4  , 0  , 7711 , 146, 82 ), // #483
  INST(Paddq            , ExtRm_P            , O(000F00,D4,_,_,_,_,_,_  ), 0                         , 4  , 0  , 7718 , 146, 4  ), // #484
  INST(Paddsb           , ExtRm_P            , O(000F00,EC,_,_,_,_,_,_  ), 0                         , 4  , 0  , 7725 , 146, 82 ), // #485
  INST(Paddsw           , ExtRm_P            , O(000F00,ED,_,_,_,_,_,_  ), 0                         , 4  , 0  , 7733 , 146, 82 ), // #486
  INST(Paddusb          , ExtRm_P            , O(000F00,DC,_,_,_,_,_,_  ), 0                         , 4  , 0  , 7741 , 146, 82 ), // #487
  INST(Paddusw          , ExtRm_P            , O(000F00,DD,_,_,_,_,_,_  ), 0                         , 4  , 0  , 7750 , 146, 82 ), // #488
  INST(Paddw            , ExtRm_P            , O(000F00,FD,_,_,_,_,_,_  ), 0                         , 4  , 0  , 7759 , 146, 82 ), // #489
  INST(Palignr          , ExtRmi_P           , O(000F3A,0F,_,_,_,_,_,_  ), 0                         , 85 , 0  , 7766 , 147, 6  ), // #490
  INST(Pand             , ExtRm_P            , O(000F00,DB,_,_,_,_,_,_  ), 0                         , 4  , 0  , 7775 , 148, 82 ), // #491
  INST(Pandn            , ExtRm_P            , O(000F00,DF,_,_,_,_,_,_  ), 0                         , 4  , 0  , 7788 , 149, 82 ), // #492
  INST(Pause            , X86Op              , O(F30000,90,_,_,_,_,_,_  ), 0                         , 86 , 0  , 3195 , 30 , 0  ), // #493
  INST(Pavgb            , ExtRm_P            , O(000F00,E0,_,_,_,_,_,_  ), 0                         , 4  , 0  , 7818 , 146, 87 ), // #494
  INST(Pavgusb          , Ext3dNow           , O(000F0F,BF,_,_,_,_,_,_  ), 0                         , 87 , 0  , 2076 , 150, 51 ), // #495
  INST(Pavgw            , ExtRm_P            , O(000F00,E3,_,_,_,_,_,_  ), 0                         , 4  , 0  , 7825 , 146, 87 ), // #496
  INST(Pblendvb         , ExtRm_XMM0         , O(660F38,10,_,_,_,_,_,_  ), 0                         , 2  , 0  , 7881 , 15 , 12 ), // #497
  INST(Pblendw          , ExtRmi             , O(660F3A,0E,_,_,_,_,_,_  ), 0                         , 8  , 0  , 7891 , 8  , 12 ), // #498
  INST(Pclmulqdq        , ExtRmi             , O(660F3A,44,_,_,_,_,_,_  ), 0                         , 8  , 0  , 7984 , 8  , 88 ), // #499
  INST(Pcmpeqb          , ExtRm_P            , O(000F00,74,_,_,_,_,_,_  ), 0                         , 4  , 0  , 8016 , 149, 82 ), // #500
  INST(Pcmpeqd          , ExtRm_P            , O(000F00,76,_,_,_,_,_,_  ), 0                         , 4  , 0  , 8025 , 149, 82 ), // #501
  INST(Pcmpeqq          , ExtRm              , O(660F38,29,_,_,_,_,_,_  ), 0                         , 2  , 0  , 8034 , 151, 12 ), // #502
  INST(Pcmpeqw          , ExtRm_P            , O(000F00,75,_,_,_,_,_,_  ), 0                         , 4  , 0  , 8043 , 149, 82 ), // #503
  INST(Pcmpestri        , ExtRmi             , O(660F3A,61,_,_,_,_,_,_  ), 0                         , 8  , 0  , 8052 , 152, 89 ), // #504
  INST(Pcmpestrm        , ExtRmi             , O(660F3A,60,_,_,_,_,_,_  ), 0                         , 8  , 0  , 8063 , 153, 89 ), // #505
  INST(Pcmpgtb          , ExtRm_P            , O(000F00,64,_,_,_,_,_,_  ), 0                         , 4  , 0  , 8074 , 149, 82 ), // #506
  INST(Pcmpgtd          , ExtRm_P            , O(000F00,66,_,_,_,_,_,_  ), 0                         , 4  , 0  , 8083 , 149, 82 ), // #507
  INST(Pcmpgtq          , ExtRm              , O(660F38,37,_,_,_,_,_,_  ), 0                         , 2  , 0  , 8092 , 151, 44 ), // #508
  INST(Pcmpgtw          , ExtRm_P            , O(000F00,65,_,_,_,_,_,_  ), 0                         , 4  , 0  , 8101 , 149, 82 ), // #509
  INST(Pcmpistri        , ExtRmi             , O(660F3A,63,_,_,_,_,_,_  ), 0                         , 8  , 0  , 8110 , 154, 89 ), // #510
  INST(Pcmpistrm        , ExtRmi             , O(660F3A,62,_,_,_,_,_,_  ), 0                         , 8  , 0  , 8121 , 155, 89 ), // #511
  INST(Pconfig          , X86Op              , O(000F01,C5,_,_,_,_,_,_  ), 0                         , 21 , 0  , 2084 , 30 , 90 ), // #512
  INST(Pdep             , VexRvm_Wx          , V(F20F38,F5,_,0,x,_,_,_  ), 0                         , 84 , 0  , 2092 , 10 , 85 ), // #513
  INST(Pext             , VexRvm_Wx          , V(F30F38,F5,_,0,x,_,_,_  ), 0                         , 88 , 0  , 2097 , 10 , 85 ), // #514
  INST(Pextrb           , ExtExtract         , O(000F3A,14,_,_,_,_,_,_  ), 0                         , 85 , 0  , 8608 , 156, 12 ), // #515
  INST(Pextrd           , ExtExtract         , O(000F3A,16,_,_,_,_,_,_  ), 0                         , 85 , 0  , 8616 , 58 , 12 ), // #516
  INST(Pextrq           , ExtExtract         , O(000F3A,16,_,_,1,_,_,_  ), 0                         , 89 , 0  , 8624 , 157, 12 ), // #517
  INST(Pextrw           , ExtPextrw          , O(000F00,C5,_,_,_,_,_,_  ), O(000F3A,15,_,_,_,_,_,_  ), 4  , 66 , 8632 , 158, 91 ), // #518
  INST(Pf2id            , Ext3dNow           , O(000F0F,1D,_,_,_,_,_,_  ), 0                         , 87 , 0  , 2102 , 150, 51 ), // #519
  INST(Pf2iw            , Ext3dNow           , O(000F0F,1C,_,_,_,_,_,_  ), 0                         , 87 , 0  , 2108 , 150, 92 ), // #520
  INST(Pfacc            , Ext3dNow           , O(000F0F,AE,_,_,_,_,_,_  ), 0                         , 87 , 0  , 2114 , 150, 51 ), // #521
  INST(Pfadd            , Ext3dNow           , O(000F0F,9E,_,_,_,_,_,_  ), 0                         , 87 , 0  , 2120 , 150, 51 ), // #522
  INST(Pfcmpeq          , Ext3dNow           , O(000F0F,B0,_,_,_,_,_,_  ), 0                         , 87 , 0  , 2126 , 150, 51 ), // #523
  INST(Pfcmpge          , Ext3dNow           , O(000F0F,90,_,_,_,_,_,_  ), 0                         , 87 , 0  , 2134 , 150, 51 ), // #524
  INST(Pfcmpgt          , Ext3dNow           , O(000F0F,A0,_,_,_,_,_,_  ), 0                         , 87 , 0  , 2142 , 150, 51 ), // #525
  INST(Pfmax            , Ext3dNow           , O(000F0F,A4,_,_,_,_,_,_  ), 0                         , 87 , 0  , 2150 , 150, 51 ), // #526
  INST(Pfmin            , Ext3dNow           , O(000F0F,94,_,_,_,_,_,_  ), 0                         , 87 , 0  , 2156 , 150, 51 ), // #527
  INST(Pfmul            , Ext3dNow           , O(000F0F,B4,_,_,_,_,_,_  ), 0                         , 87 , 0  , 2162 , 150, 51 ), // #528
  INST(Pfnacc           , Ext3dNow           , O(000F0F,8A,_,_,_,_,_,_  ), 0                         , 87 , 0  , 2168 , 150, 92 ), // #529
  INST(Pfpnacc          , Ext3dNow           , O(000F0F,8E,_,_,_,_,_,_  ), 0                         , 87 , 0  , 2175 , 150, 92 ), // #530
  INST(Pfrcp            , Ext3dNow           , O(000F0F,96,_,_,_,_,_,_  ), 0                         , 87 , 0  , 2183 , 150, 51 ), // #531
  INST(Pfrcpit1         , Ext3dNow           , O(000F0F,A6,_,_,_,_,_,_  ), 0                         , 87 , 0  , 2189 , 150, 51 ), // #532
  INST(Pfrcpit2         , Ext3dNow           , O(000F0F,B6,_,_,_,_,_,_  ), 0                         , 87 , 0  , 2198 , 150, 51 ), // #533
  INST(Pfrcpv           , Ext3dNow           , O(000F0F,86,_,_,_,_,_,_  ), 0                         , 87 , 0  , 2207 , 150, 93 ), // #534
  INST(Pfrsqit1         , Ext3dNow           , O(000F0F,A7,_,_,_,_,_,_  ), 0                         , 87 , 0  , 2214 , 150, 51 ), // #535
  INST(Pfrsqrt          , Ext3dNow           , O(000F0F,97,_,_,_,_,_,_  ), 0                         , 87 , 0  , 2223 , 150, 51 ), // #536
  INST(Pfrsqrtv         , Ext3dNow           , O(000F0F,87,_,_,_,_,_,_  ), 0                         , 87 , 0  , 2231 , 150, 93 ), // #537
  INST(Pfsub            , Ext3dNow           , O(000F0F,9A,_,_,_,_,_,_  ), 0                         , 87 , 0  , 2240 , 150, 51 ), // #538
  INST(Pfsubr           , Ext3dNow           , O(000F0F,AA,_,_,_,_,_,_  ), 0                         , 87 , 0  , 2246 , 150, 51 ), // #539
  INST(Phaddd           , ExtRm_P            , O(000F38,02,_,_,_,_,_,_  ), 0                         , 83 , 0  , 8711 , 146, 86 ), // #540
  INST(Phaddsw          , ExtRm_P            , O(000F38,03,_,_,_,_,_,_  ), 0                         , 83 , 0  , 8728 , 146, 86 ), // #541
  INST(Phaddw           , ExtRm_P            , O(000F38,01,_,_,_,_,_,_  ), 0                         , 83 , 0  , 8797 , 146, 86 ), // #542
  INST(Phminposuw       , ExtRm              , O(660F38,41,_,_,_,_,_,_  ), 0                         , 2  , 0  , 8823 , 5  , 12 ), // #543
  INST(Phsubd           , ExtRm_P            , O(000F38,06,_,_,_,_,_,_  ), 0                         , 83 , 0  , 8844 , 146, 86 ), // #544
  INST(Phsubsw          , ExtRm_P            , O(000F38,07,_,_,_,_,_,_  ), 0                         , 83 , 0  , 8861 , 146, 86 ), // #545
  INST(Phsubw           , ExtRm_P            , O(000F38,05,_,_,_,_,_,_  ), 0                         , 83 , 0  , 8870 , 146, 86 ), // #546
  INST(Pi2fd            , Ext3dNow           , O(000F0F,0D,_,_,_,_,_,_  ), 0                         , 87 , 0  , 2253 , 150, 51 ), // #547
  INST(Pi2fw            , Ext3dNow           , O(000F0F,0C,_,_,_,_,_,_  ), 0                         , 87 , 0  , 2259 , 150, 92 ), // #548
  INST(Pinsrb           , ExtRmi             , O(660F3A,20,_,_,_,_,_,_  ), 0                         , 8  , 0  , 8887 , 159, 12 ), // #549
  INST(Pinsrd           , ExtRmi             , O(660F3A,22,_,_,_,_,_,_  ), 0                         , 8  , 0  , 8895 , 160, 12 ), // #550
  INST(Pinsrq           , ExtRmi             , O(660F3A,22,_,_,1,_,_,_  ), 0                         , 90 , 0  , 8903 , 161, 12 ), // #551
  INST(Pinsrw           , ExtRmi_P           , O(000F00,C4,_,_,_,_,_,_  ), 0                         , 4  , 0  , 8911 , 162, 87 ), // #552
  INST(Pmaddubsw        , ExtRm_P            , O(000F38,04,_,_,_,_,_,_  ), 0                         , 83 , 0  , 9081 , 146, 86 ), // #553
  INST(Pmaddwd          , ExtRm_P            , O(000F00,F5,_,_,_,_,_,_  ), 0                         , 4  , 0  , 9092 , 146, 82 ), // #554
  INST(Pmaxsb           , ExtRm              , O(660F38,3C,_,_,_,_,_,_  ), 0                         , 2  , 0  , 9123 , 11 , 12 ), // #555
  INST(Pmaxsd           , ExtRm              , O(660F38,3D,_,_,_,_,_,_  ), 0                         , 2  , 0  , 9131 , 11 , 12 ), // #556
  INST(Pmaxsw           , ExtRm_P            , O(000F00,EE,_,_,_,_,_,_  ), 0                         , 4  , 0  , 9147 , 148, 87 ), // #557
  INST(Pmaxub           , ExtRm_P            , O(000F00,DE,_,_,_,_,_,_  ), 0                         , 4  , 0  , 9155 , 148, 87 ), // #558
  INST(Pmaxud           , ExtRm              , O(660F38,3F,_,_,_,_,_,_  ), 0                         , 2  , 0  , 9163 , 11 , 12 ), // #559
  INST(Pmaxuw           , ExtRm              , O(660F38,3E,_,_,_,_,_,_  ), 0                         , 2  , 0  , 9179 , 11 , 12 ), // #560
  INST(Pminsb           , ExtRm              , O(660F38,38,_,_,_,_,_,_  ), 0                         , 2  , 0  , 9187 , 11 , 12 ), // #561
  INST(Pminsd           , ExtRm              , O(660F38,39,_,_,_,_,_,_  ), 0                         , 2  , 0  , 9195 , 11 , 12 ), // #562
  INST(Pminsw           , ExtRm_P            , O(000F00,EA,_,_,_,_,_,_  ), 0                         , 4  , 0  , 9211 , 148, 87 ), // #563
  INST(Pminub           , ExtRm_P            , O(000F00,DA,_,_,_,_,_,_  ), 0                         , 4  , 0  , 9219 , 148, 87 ), // #564
  INST(Pminud           , ExtRm              , O(660F38,3B,_,_,_,_,_,_  ), 0                         , 2  , 0  , 9227 , 11 , 12 ), // #565
  INST(Pminuw           , ExtRm              , O(660F38,3A,_,_,_,_,_,_  ), 0                         , 2  , 0  , 9243 , 11 , 12 ), // #566
  INST(Pmovmskb         , ExtRm_P            , O(000F00,D7,_,_,_,_,_,_  ), 0                         , 4  , 0  , 9321 , 163, 87 ), // #567
  INST(Pmovsxbd         , ExtRm              , O(660F38,21,_,_,_,_,_,_  ), 0                         , 2  , 0  , 9418 , 7  , 12 ), // #568
  INST(Pmovsxbq         , ExtRm              , O(660F38,22,_,_,_,_,_,_  ), 0                         , 2  , 0  , 9428 , 164, 12 ), // #569
  INST(Pmovsxbw         , ExtRm              , O(660F38,20,_,_,_,_,_,_  ), 0                         , 2  , 0  , 9438 , 6  , 12 ), // #570
  INST(Pmovsxdq         , ExtRm              , O(660F38,25,_,_,_,_,_,_  ), 0                         , 2  , 0  , 9448 , 6  , 12 ), // #571
  INST(Pmovsxwd         , ExtRm              , O(660F38,23,_,_,_,_,_,_  ), 0                         , 2  , 0  , 9458 , 6  , 12 ), // #572
  INST(Pmovsxwq         , ExtRm              , O(660F38,24,_,_,_,_,_,_  ), 0                         , 2  , 0  , 9468 , 7  , 12 ), // #573
  INST(Pmovzxbd         , ExtRm              , O(660F38,31,_,_,_,_,_,_  ), 0                         , 2  , 0  , 9555 , 7  , 12 ), // #574
  INST(Pmovzxbq         , ExtRm              , O(660F38,32,_,_,_,_,_,_  ), 0                         , 2  , 0  , 9565 , 164, 12 ), // #575
  INST(Pmovzxbw         , ExtRm              , O(660F38,30,_,_,_,_,_,_  ), 0                         , 2  , 0  , 9575 , 6  , 12 ), // #576
  INST(Pmovzxdq         , ExtRm              , O(660F38,35,_,_,_,_,_,_  ), 0                         , 2  , 0  , 9585 , 6  , 12 ), // #577
  INST(Pmovzxwd         , ExtRm              , O(660F38,33,_,_,_,_,_,_  ), 0                         , 2  , 0  , 9595 , 6  , 12 ), // #578
  INST(Pmovzxwq         , ExtRm              , O(660F38,34,_,_,_,_,_,_  ), 0                         , 2  , 0  , 9605 , 7  , 12 ), // #579
  INST(Pmuldq           , ExtRm              , O(660F38,28,_,_,_,_,_,_  ), 0                         , 2  , 0  , 9615 , 5  , 12 ), // #580
  INST(Pmulhrsw         , ExtRm_P            , O(000F38,0B,_,_,_,_,_,_  ), 0                         , 83 , 0  , 9623 , 146, 86 ), // #581
  INST(Pmulhrw          , Ext3dNow           , O(000F0F,B7,_,_,_,_,_,_  ), 0                         , 87 , 0  , 2265 , 150, 51 ), // #582
  INST(Pmulhuw          , ExtRm_P            , O(000F00,E4,_,_,_,_,_,_  ), 0                         , 4  , 0  , 9633 , 146, 87 ), // #583
  INST(Pmulhw           , ExtRm_P            , O(000F00,E5,_,_,_,_,_,_  ), 0                         , 4  , 0  , 9642 , 146, 82 ), // #584
  INST(Pmulld           , ExtRm              , O(660F38,40,_,_,_,_,_,_  ), 0                         , 2  , 0  , 9650 , 5  , 12 ), // #585
  INST(Pmullw           , ExtRm_P            , O(000F00,D5,_,_,_,_,_,_  ), 0                         , 4  , 0  , 9666 , 146, 82 ), // #586
  INST(Pmuludq          , ExtRm_P            , O(000F00,F4,_,_,_,_,_,_  ), 0                         , 4  , 0  , 9689 , 146, 4  ), // #587
  INST(Pop              , X86Pop             , O(000000,8F,0,_,_,_,_,_  ), O(000000,58,_,_,_,_,_,_  ), 0  , 67 , 2273 , 165, 0  ), // #588
  INST(Popa             , X86Op              , O(660000,61,_,_,_,_,_,_  ), 0                         , 19 , 0  , 2277 , 81 , 0  ), // #589
  INST(Popad            , X86Op              , O(000000,61,_,_,_,_,_,_  ), 0                         , 0  , 0  , 2282 , 81 , 0  ), // #590
  INST(Popcnt           , X86Rm_Raw66H       , O(F30F00,B8,_,_,x,_,_,_  ), 0                         , 6  , 0  , 2288 , 22 , 94 ), // #591
  INST(Popf             , X86Op              , O(660000,9D,_,_,_,_,_,_  ), 0                         , 19 , 0  , 2295 , 30 , 95 ), // #592
  INST(Popfd            , X86Op              , O(000000,9D,_,_,_,_,_,_  ), 0                         , 0  , 0  , 2300 , 81 , 95 ), // #593
  INST(Popfq            , X86Op              , O(000000,9D,_,_,_,_,_,_  ), 0                         , 0  , 0  , 2306 , 33 , 95 ), // #594
  INST(Por              , ExtRm_P            , O(000F00,EB,_,_,_,_,_,_  ), 0                         , 4  , 0  , 9734 , 148, 82 ), // #595
  INST(Prefetch         , X86M_Only          , O(000F00,0D,0,_,_,_,_,_  ), 0                         , 4  , 0  , 2312 , 31 , 51 ), // #596
  INST(Prefetchnta      , X86M_Only          , O(000F00,18,0,_,_,_,_,_  ), 0                         , 4  , 0  , 2321 , 31 , 77 ), // #597
  INST(Prefetcht0       , X86M_Only          , O(000F00,18,1,_,_,_,_,_  ), 0                         , 29 , 0  , 2333 , 31 , 77 ), // #598
  INST(Prefetcht1       , X86M_Only          , O(000F00,18,2,_,_,_,_,_  ), 0                         , 76 , 0  , 2344 , 31 , 77 ), // #599
  INST(Prefetcht2       , X86M_Only          , O(000F00,18,3,_,_,_,_,_  ), 0                         , 78 , 0  , 2355 , 31 , 77 ), // #600
  INST(Prefetchw        , X86M_Only          , O(000F00,0D,1,_,_,_,_,_  ), 0                         , 29 , 0  , 2366 , 31 , 96 ), // #601
  INST(Prefetchwt1      , X86M_Only          , O(000F00,0D,2,_,_,_,_,_  ), 0                         , 76 , 0  , 2376 , 31 , 97 ), // #602
  INST(Psadbw           , ExtRm_P            , O(000F00,F6,_,_,_,_,_,_  ), 0                         , 4  , 0  , 4644 , 146, 87 ), // #603
  INST(Pshufb           , ExtRm_P            , O(000F38,00,_,_,_,_,_,_  ), 0                         , 83 , 0  , 10060, 146, 86 ), // #604
  INST(Pshufd           , ExtRmi             , O(660F00,70,_,_,_,_,_,_  ), 0                         , 3  , 0  , 10081, 8  , 4  ), // #605
  INST(Pshufhw          , ExtRmi             , O(F30F00,70,_,_,_,_,_,_  ), 0                         , 6  , 0  , 10089, 8  , 4  ), // #606
  INST(Pshuflw          , ExtRmi             , O(F20F00,70,_,_,_,_,_,_  ), 0                         , 5  , 0  , 10098, 8  , 4  ), // #607
  INST(Pshufw           , ExtRmi_P           , O(000F00,70,_,_,_,_,_,_  ), 0                         , 4  , 0  , 2388 , 166, 77 ), // #608
  INST(Psignb           , ExtRm_P            , O(000F38,08,_,_,_,_,_,_  ), 0                         , 83 , 0  , 10107, 146, 86 ), // #609
  INST(Psignd           , ExtRm_P            , O(000F38,0A,_,_,_,_,_,_  ), 0                         , 83 , 0  , 10115, 146, 86 ), // #610
  INST(Psignw           , ExtRm_P            , O(000F38,09,_,_,_,_,_,_  ), 0                         , 83 , 0  , 10123, 146, 86 ), // #611
  INST(Pslld            , ExtRmRi_P          , O(000F00,F2,_,_,_,_,_,_  ), O(000F00,72,6,_,_,_,_,_  ), 4  , 68 , 10131, 167, 82 ), // #612
  INST(Pslldq           , ExtRmRi            , 0                         , O(660F00,73,7,_,_,_,_,_  ), 0  , 69 , 10138, 168, 4  ), // #613
  INST(Psllq            , ExtRmRi_P          , O(000F00,F3,_,_,_,_,_,_  ), O(000F00,73,6,_,_,_,_,_  ), 4  , 70 , 10146, 167, 82 ), // #614
  INST(Psllw            , ExtRmRi_P          , O(000F00,F1,_,_,_,_,_,_  ), O(000F00,71,6,_,_,_,_,_  ), 4  , 71 , 10177, 167, 82 ), // #615
  INST(Psmash           , X86Op              , O(F30F01,FF,_,_,_,_,_,_  ), 0                         , 25 , 0  , 2395 , 33 , 98 ), // #616
  INST(Psrad            , ExtRmRi_P          , O(000F00,E2,_,_,_,_,_,_  ), O(000F00,72,4,_,_,_,_,_  ), 4  , 72 , 10184, 167, 82 ), // #617
  INST(Psraw            , ExtRmRi_P          , O(000F00,E1,_,_,_,_,_,_  ), O(000F00,71,4,_,_,_,_,_  ), 4  , 73 , 10222, 167, 82 ), // #618
  INST(Psrld            , ExtRmRi_P          , O(000F00,D2,_,_,_,_,_,_  ), O(000F00,72,2,_,_,_,_,_  ), 4  , 74 , 10229, 167, 82 ), // #619
  INST(Psrldq           , ExtRmRi            , 0                         , O(660F00,73,3,_,_,_,_,_  ), 0  , 75 , 10236, 168, 4  ), // #620
  INST(Psrlq            , ExtRmRi_P          , O(000F00,D3,_,_,_,_,_,_  ), O(000F00,73,2,_,_,_,_,_  ), 4  , 76 , 10244, 167, 82 ), // #621
  INST(Psrlw            , ExtRmRi_P          , O(000F00,D1,_,_,_,_,_,_  ), O(000F00,71,2,_,_,_,_,_  ), 4  , 77 , 10275, 167, 82 ), // #622
  INST(Psubb            , ExtRm_P            , O(000F00,F8,_,_,_,_,_,_  ), 0                         , 4  , 0  , 10282, 149, 82 ), // #623
  INST(Psubd            , ExtRm_P            , O(000F00,FA,_,_,_,_,_,_  ), 0                         , 4  , 0  , 10289, 149, 82 ), // #624
  INST(Psubq            , ExtRm_P            , O(000F00,FB,_,_,_,_,_,_  ), 0                         , 4  , 0  , 10296, 149, 4  ), // #625
  INST(Psubsb           , ExtRm_P            , O(000F00,E8,_,_,_,_,_,_  ), 0                         , 4  , 0  , 10303, 149, 82 ), // #626
  INST(Psubsw           , ExtRm_P            , O(000F00,E9,_,_,_,_,_,_  ), 0                         , 4  , 0  , 10311, 149, 82 ), // #627
  INST(Psubusb          , ExtRm_P            , O(000F00,D8,_,_,_,_,_,_  ), 0                         , 4  , 0  , 10319, 149, 82 ), // #628
  INST(Psubusw          , ExtRm_P            , O(000F00,D9,_,_,_,_,_,_  ), 0                         , 4  , 0  , 10328, 149, 82 ), // #629
  INST(Psubw            , ExtRm_P            , O(000F00,F9,_,_,_,_,_,_  ), 0                         , 4  , 0  , 10337, 149, 82 ), // #630
  INST(Pswapd           , Ext3dNow           , O(000F0F,BB,_,_,_,_,_,_  ), 0                         , 87 , 0  , 2402 , 150, 92 ), // #631
  INST(Ptest            , ExtRm              , O(660F38,17,_,_,_,_,_,_  ), 0                         , 2  , 0  , 10366, 5  , 99 ), // #632
  INST(Ptwrite          , X86M               , O(F30F00,AE,4,_,_,_,_,_  ), 0                         , 91 , 0  , 2409 , 169, 100), // #633
  INST(Punpckhbw        , ExtRm_P            , O(000F00,68,_,_,_,_,_,_  ), 0                         , 4  , 0  , 10449, 146, 82 ), // #634
  INST(Punpckhdq        , ExtRm_P            , O(000F00,6A,_,_,_,_,_,_  ), 0                         , 4  , 0  , 10460, 146, 82 ), // #635
  INST(Punpckhqdq       , ExtRm              , O(660F00,6D,_,_,_,_,_,_  ), 0                         , 3  , 0  , 10471, 5  , 4  ), // #636
  INST(Punpckhwd        , ExtRm_P            , O(000F00,69,_,_,_,_,_,_  ), 0                         , 4  , 0  , 10483, 146, 82 ), // #637
  INST(Punpcklbw        , ExtRm_P            , O(000F00,60,_,_,_,_,_,_  ), 0                         , 4  , 0  , 10494, 170, 82 ), // #638
  INST(Punpckldq        , ExtRm_P            , O(000F00,62,_,_,_,_,_,_  ), 0                         , 4  , 0  , 10505, 170, 82 ), // #639
  INST(Punpcklqdq       , ExtRm              , O(660F00,6C,_,_,_,_,_,_  ), 0                         , 3  , 0  , 10516, 5  , 4  ), // #640
  INST(Punpcklwd        , ExtRm_P            , O(000F00,61,_,_,_,_,_,_  ), 0                         , 4  , 0  , 10528, 170, 82 ), // #641
  INST(Push             , X86Push            , O(000000,FF,6,_,_,_,_,_  ), O(000000,50,_,_,_,_,_,_  ), 32 , 78 , 2417 , 171, 0  ), // #642
  INST(Pusha            , X86Op              , O(660000,60,_,_,_,_,_,_  ), 0                         , 19 , 0  , 2422 , 81 , 0  ), // #643
  INST(Pushad           , X86Op              , O(000000,60,_,_,_,_,_,_  ), 0                         , 0  , 0  , 2428 , 81 , 0  ), // #644
  INST(Pushf            , X86Op              , O(660000,9C,_,_,_,_,_,_  ), 0                         , 19 , 0  , 2435 , 30 , 101), // #645
  INST(Pushfd           , X86Op              , O(000000,9C,_,_,_,_,_,_  ), 0                         , 0  , 0  , 2441 , 81 , 101), // #646
  INST(Pushfq           , X86Op              , O(000000,9C,_,_,_,_,_,_  ), 0                         , 0  , 0  , 2448 , 33 , 101), // #647
  INST(Pvalidate        , X86Op              , O(F20F01,FF,_,_,_,_,_,_  ), 0                         , 92 , 0  , 2455 , 30 , 102), // #648
  INST(Pxor             , ExtRm_P            , O(000F00,EF,_,_,_,_,_,_  ), 0                         , 4  , 0  , 10539, 149, 82 ), // #649
  INST(Rcl              , X86Rot             , O(000000,D0,2,_,x,_,_,_  ), 0                         , 1  , 0  , 2465 , 172, 103), // #650
  INST(Rcpps            , ExtRm              , O(000F00,53,_,_,_,_,_,_  ), 0                         , 4  , 0  , 10674, 5  , 5  ), // #651
  INST(Rcpss            , ExtRm              , O(F30F00,53,_,_,_,_,_,_  ), 0                         , 6  , 0  , 10688, 7  , 5  ), // #652
  INST(Rcr              , X86Rot             , O(000000,D0,3,_,x,_,_,_  ), 0                         , 75 , 0  , 2469 , 172, 103), // #653
  INST(Rdfsbase         , X86M               , O(F30F00,AE,0,_,x,_,_,_  ), 0                         , 6  , 0  , 2473 , 173, 104), // #654
  INST(Rdgsbase         , X86M               , O(F30F00,AE,1,_,x,_,_,_  ), 0                         , 93 , 0  , 2482 , 173, 104), // #655
  INST(Rdmsr            , X86Op              , O(000F00,32,_,_,_,_,_,_  ), 0                         , 4  , 0  , 2491 , 174, 105), // #656
  INST(Rdpid            , X86R_Native        , O(F30F00,C7,7,_,_,_,_,_  ), 0                         , 94 , 0  , 2497 , 175, 106), // #657
  INST(Rdpkru           , X86Op              , O(000F01,EE,_,_,_,_,_,_  ), 0                         , 21 , 0  , 2503 , 174, 107), // #658
  INST(Rdpmc            , X86Op              , O(000F00,33,_,_,_,_,_,_  ), 0                         , 4  , 0  , 2510 , 174, 0  ), // #659
  INST(Rdpru            , X86Op              , O(000F01,FD,_,_,_,_,_,_  ), 0                         , 21 , 0  , 2516 , 174, 108), // #660
  INST(Rdrand           , X86M               , O(000F00,C7,6,_,x,_,_,_  ), 0                         , 80 , 0  , 2522 , 23 , 109), // #661
  INST(Rdseed           , X86M               , O(000F00,C7,7,_,x,_,_,_  ), 0                         , 22 , 0  , 2529 , 23 , 110), // #662
  INST(Rdsspd           , X86M               , O(F30F00,1E,1,_,_,_,_,_  ), 0                         , 93 , 0  , 2536 , 76 , 56 ), // #663
  INST(Rdsspq           , X86M               , O(F30F00,1E,1,_,_,_,_,_  ), 0                         , 93 , 0  , 2543 , 77 , 56 ), // #664
  INST(Rdtsc            , X86Op              , O(000F00,31,_,_,_,_,_,_  ), 0                         , 4  , 0  , 2550 , 28 , 111), // #665
  INST(Rdtscp           , X86Op              , O(000F01,F9,_,_,_,_,_,_  ), 0                         , 21 , 0  , 2556 , 174, 112), // #666
  INST(Ret              , X86Ret             , O(000000,C2,_,_,_,_,_,_  ), 0                         , 0  , 0  , 3072 , 176, 0  ), // #667
  INST(Retf             , X86Ret             , O(000000,CA,_,_,x,_,_,_  ), 0                         , 0  , 0  , 2563 , 177, 0  ), // #668
  INST(Rmpadjust        , X86Op              , O(F30F01,FE,_,_,_,_,_,_  ), 0                         , 25 , 0  , 2568 , 33 , 98 ), // #669
  INST(Rmpupdate        , X86Op              , O(F20F01,FE,_,_,_,_,_,_  ), 0                         , 92 , 0  , 2578 , 33 , 98 ), // #670
  INST(Rol              , X86Rot             , O(000000,D0,0,_,x,_,_,_  ), 0                         , 0  , 0  , 2588 , 172, 113), // #671
  INST(Ror              , X86Rot             , O(000000,D0,1,_,x,_,_,_  ), 0                         , 31 , 0  , 2592 , 172, 113), // #672
  INST(Rorx             , VexRmi_Wx          , V(F20F3A,F0,_,0,x,_,_,_  ), 0                         , 95 , 0  , 2596 , 178, 85 ), // #673
  INST(Roundpd          , ExtRmi             , O(660F3A,09,_,_,_,_,_,_  ), 0                         , 8  , 0  , 10827, 8  , 12 ), // #674
  INST(Roundps          , ExtRmi             , O(660F3A,08,_,_,_,_,_,_  ), 0                         , 8  , 0  , 10836, 8  , 12 ), // #675
  INST(Roundsd          , ExtRmi             , O(660F3A,0B,_,_,_,_,_,_  ), 0                         , 8  , 0  , 10845, 37 , 12 ), // #676
  INST(Roundss          , ExtRmi             , O(660F3A,0A,_,_,_,_,_,_  ), 0                         , 8  , 0  , 10854, 38 , 12 ), // #677
  INST(Rsm              , X86Op              , O(000F00,AA,_,_,_,_,_,_  ), 0                         , 4  , 0  , 2601 , 81 , 1  ), // #678
  INST(Rsqrtps          , ExtRm              , O(000F00,52,_,_,_,_,_,_  ), 0                         , 4  , 0  , 10960, 5  , 5  ), // #679
  INST(Rsqrtss          , ExtRm              , O(F30F00,52,_,_,_,_,_,_  ), 0                         , 6  , 0  , 10978, 7  , 5  ), // #680
  INST(Rstorssp         , X86M_Only          , O(F30F00,01,5,_,_,_,_,_  ), 0                         , 63 , 0  , 2605 , 32 , 24 ), // #681
  INST(Sahf             , X86Op              , O(000000,9E,_,_,_,_,_,_  ), 0                         , 0  , 0  , 2614 , 97 , 114), // #682
  INST(Sal              , X86Rot             , O(000000,D0,4,_,x,_,_,_  ), 0                         , 9  , 0  , 2619 , 172, 1  ), // #683
  INST(Sar              , X86Rot             , O(000000,D0,7,_,x,_,_,_  ), 0                         , 27 , 0  , 2623 , 172, 1  ), // #684
  INST(Sarx             , VexRmv_Wx          , V(F30F38,F7,_,0,x,_,_,_  ), 0                         , 88 , 0  , 2627 , 13 , 85 ), // #685
  INST(Saveprevssp      , X86Op              , O(F30F01,EA,_,_,_,_,_,_  ), 0                         , 25 , 0  , 2632 , 30 , 24 ), // #686
  INST(Sbb              , X86Arith           , O(000000,18,3,_,x,_,_,_  ), 0                         , 75 , 0  , 2644 , 179, 2  ), // #687
  INST(Scas             , X86StrRm           , O(000000,AE,_,_,_,_,_,_  ), 0                         , 0  , 0  , 2648 , 180, 37 ), // #688
  INST(Senduipi         , X86M_NoSize        , O(F30F00,C7,6,_,_,_,_,_  ), 0                         , 24 , 0  , 2653 , 77 , 25 ), // #689
  INST(Serialize        , X86Op              , O(000F01,E8,_,_,_,_,_,_  ), 0                         , 21 , 0  , 2662 , 30 , 115), // #690
  INST(Seta             , X86Set             , O(000F00,97,_,_,_,_,_,_  ), 0                         , 4  , 0  , 2672 , 181, 59 ), // #691
  INST(Setae            , X86Set             , O(000F00,93,_,_,_,_,_,_  ), 0                         , 4  , 0  , 2677 , 181, 60 ), // #692
  INST(Setb             , X86Set             , O(000F00,92,_,_,_,_,_,_  ), 0                         , 4  , 0  , 2683 , 181, 60 ), // #693
  INST(Setbe            , X86Set             , O(000F00,96,_,_,_,_,_,_  ), 0                         , 4  , 0  , 2688 , 181, 59 ), // #694
  INST(Setc             , X86Set             , O(000F00,92,_,_,_,_,_,_  ), 0                         , 4  , 0  , 2694 , 181, 60 ), // #695
  INST(Sete             , X86Set             , O(000F00,94,_,_,_,_,_,_  ), 0                         , 4  , 0  , 2699 , 181, 61 ), // #696
  INST(Setg             , X86Set             , O(000F00,9F,_,_,_,_,_,_  ), 0                         , 4  , 0  , 2704 , 181, 62 ), // #697
  INST(Setge            , X86Set             , O(000F00,9D,_,_,_,_,_,_  ), 0                         , 4  , 0  , 2709 , 181, 63 ), // #698
  INST(Setl             , X86Set             , O(000F00,9C,_,_,_,_,_,_  ), 0                         , 4  , 0  , 2715 , 181, 63 ), // #699
  INST(Setle            , X86Set             , O(000F00,9E,_,_,_,_,_,_  ), 0                         , 4  , 0  , 2720 , 181, 62 ), // #700
  INST(Setna            , X86Set             , O(000F00,96,_,_,_,_,_,_  ), 0                         , 4  , 0  , 2726 , 181, 59 ), // #701
  INST(Setnae           , X86Set             , O(000F00,92,_,_,_,_,_,_  ), 0                         , 4  , 0  , 2732 , 181, 60 ), // #702
  INST(Setnb            , X86Set             , O(000F00,93,_,_,_,_,_,_  ), 0                         , 4  , 0  , 2739 , 181, 60 ), // #703
  INST(Setnbe           , X86Set             , O(000F00,97,_,_,_,_,_,_  ), 0                         , 4  , 0  , 2745 , 181, 59 ), // #704
  INST(Setnc            , X86Set             , O(000F00,93,_,_,_,_,_,_  ), 0                         , 4  , 0  , 2752 , 181, 60 ), // #705
  INST(Setne            , X86Set             , O(000F00,95,_,_,_,_,_,_  ), 0                         , 4  , 0  , 2758 , 181, 61 ), // #706
  INST(Setng            , X86Set             , O(000F00,9E,_,_,_,_,_,_  ), 0                         , 4  , 0  , 2764 , 181, 62 ), // #707
  INST(Setnge           , X86Set             , O(000F00,9C,_,_,_,_,_,_  ), 0                         , 4  , 0  , 2770 , 181, 63 ), // #708
  INST(Setnl            , X86Set             , O(000F00,9D,_,_,_,_,_,_  ), 0                         , 4  , 0  , 2777 , 181, 63 ), // #709
  INST(Setnle           , X86Set             , O(000F00,9F,_,_,_,_,_,_  ), 0                         , 4  , 0  , 2783 , 181, 62 ), // #710
  INST(Setno            , X86Set             , O(000F00,91,_,_,_,_,_,_  ), 0                         , 4  , 0  , 2790 , 181, 57 ), // #711
  INST(Setnp            , X86Set             , O(000F00,9B,_,_,_,_,_,_  ), 0                         , 4  , 0  , 2796 , 181, 64 ), // #712
  INST(Setns            , X86Set             , O(000F00,99,_,_,_,_,_,_  ), 0                         , 4  , 0  , 2802 , 181, 65 ), // #713
  INST(Setnz            , X86Set             , O(000F00,95,_,_,_,_,_,_  ), 0                         , 4  , 0  , 2808 , 181, 61 ), // #714
  INST(Seto             , X86Set             , O(000F00,90,_,_,_,_,_,_  ), 0                         , 4  , 0  , 2814 , 181, 57 ), // #715
  INST(Setp             , X86Set             , O(000F00,9A,_,_,_,_,_,_  ), 0                         , 4  , 0  , 2819 , 181, 64 ), // #716
  INST(Setpe            , X86Set             , O(000F00,9A,_,_,_,_,_,_  ), 0                         , 4  , 0  , 2824 , 181, 64 ), // #717
  INST(Setpo            , X86Set             , O(000F00,9B,_,_,_,_,_,_  ), 0                         , 4  , 0  , 2830 , 181, 64 ), // #718
  INST(Sets             , X86Set             , O(000F00,98,_,_,_,_,_,_  ), 0                         , 4  , 0  , 2836 , 181, 65 ), // #719
  INST(Setssbsy         , X86Op              , O(F30F01,E8,_,_,_,_,_,_  ), 0                         , 25 , 0  , 2841 , 30 , 56 ), // #720
  INST(Setz             , X86Set             , O(000F00,94,_,_,_,_,_,_  ), 0                         , 4  , 0  , 2850 , 181, 61 ), // #721
  INST(Sfence           , X86Fence           , O(000F00,AE,7,_,_,_,_,_  ), 0                         , 22 , 0  , 2855 , 30 , 77 ), // #722
  INST(Sgdt             , X86M_Only          , O(000F00,01,0,_,_,_,_,_  ), 0                         , 4  , 0  , 2862 , 69 , 0  ), // #723
  INST(Sha1msg1         , ExtRm              , O(000F38,C9,_,_,_,_,_,_  ), 0                         , 83 , 0  , 2867 , 5  , 116), // #724
  INST(Sha1msg2         , ExtRm              , O(000F38,CA,_,_,_,_,_,_  ), 0                         , 83 , 0  , 2876 , 5  , 116), // #725
  INST(Sha1nexte        , ExtRm              , O(000F38,C8,_,_,_,_,_,_  ), 0                         , 83 , 0  , 2885 , 5  , 116), // #726
  INST(Sha1rnds4        , ExtRmi             , O(000F3A,CC,_,_,_,_,_,_  ), 0                         , 85 , 0  , 2895 , 8  , 116), // #727
  INST(Sha256msg1       , ExtRm              , O(000F38,CC,_,_,_,_,_,_  ), 0                         , 83 , 0  , 2905 , 5  , 116), // #728
  INST(Sha256msg2       , ExtRm              , O(000F38,CD,_,_,_,_,_,_  ), 0                         , 83 , 0  , 2916 , 5  , 116), // #729
  INST(Sha256rnds2      , ExtRm_XMM0         , O(000F38,CB,_,_,_,_,_,_  ), 0                         , 83 , 0  , 2927 , 15 , 116), // #730
  INST(Shl              , X86Rot             , O(000000,D0,4,_,x,_,_,_  ), 0                         , 9  , 0  , 2939 , 172, 1  ), // #731
  INST(Shld             , X86ShldShrd        , O(000F00,A4,_,_,x,_,_,_  ), 0                         , 4  , 0  , 9938 , 182, 1  ), // #732
  INST(Shlx             , VexRmv_Wx          , V(660F38,F7,_,0,x,_,_,_  ), 0                         , 96 , 0  , 2943 , 13 , 85 ), // #733
  INST(Shr              , X86Rot             , O(000000,D0,5,_,x,_,_,_  ), 0                         , 62 , 0  , 2948 , 172, 1  ), // #734
  INST(Shrd             , X86ShldShrd        , O(000F00,AC,_,_,x,_,_,_  ), 0                         , 4  , 0  , 2952 , 182, 1  ), // #735
  INST(Shrx             , VexRmv_Wx          , V(F20F38,F7,_,0,x,_,_,_  ), 0                         , 84 , 0  , 2957 , 13 , 85 ), // #736
  INST(Shufpd           , ExtRmi             , O(660F00,C6,_,_,_,_,_,_  ), 0                         , 3  , 0  , 11259, 8  , 4  ), // #737
  INST(Shufps           , ExtRmi             , O(000F00,C6,_,_,_,_,_,_  ), 0                         , 4  , 0  , 11267, 8  , 5  ), // #738
  INST(Sidt             , X86M_Only          , O(000F00,01,1,_,_,_,_,_  ), 0                         , 29 , 0  , 2962 , 69 , 0  ), // #739
  INST(Skinit           , X86Op_xAX          , O(000F01,DE,_,_,_,_,_,_  ), 0                         , 21 , 0  , 2967 , 52 , 117), // #740
  INST(Sldt             , X86M_NoMemSize     , O(000F00,00,0,_,_,_,_,_  ), 0                         , 4  , 0  , 2974 , 183, 0  ), // #741
  INST(Slwpcb           , VexR_Wx            , V(XOP_M9,12,1,0,x,_,_,_  ), 0                         , 11 , 0  , 2979 , 108, 74 ), // #742
  INST(Smsw             , X86M_NoMemSize     , O(000F00,01,4,_,_,_,_,_  ), 0                         , 97 , 0  , 2986 , 183, 0  ), // #743
  INST(Sqrtpd           , ExtRm              , O(660F00,51,_,_,_,_,_,_  ), 0                         , 3  , 0  , 11275, 5  , 4  ), // #744
  INST(Sqrtps           , ExtRm              , O(000F00,51,_,_,_,_,_,_  ), 0                         , 4  , 0  , 10961, 5  , 5  ), // #745
  INST(Sqrtsd           , ExtRm              , O(F20F00,51,_,_,_,_,_,_  ), 0                         , 5  , 0  , 11299, 6  , 4  ), // #746
  INST(Sqrtss           , ExtRm              , O(F30F00,51,_,_,_,_,_,_  ), 0                         , 6  , 0  , 10979, 7  , 5  ), // #747
  INST(Stac             , X86Op              , O(000F01,CB,_,_,_,_,_,_  ), 0                         , 21 , 0  , 2991 , 30 , 16 ), // #748
  INST(Stc              , X86Op              , O(000000,F9,_,_,_,_,_,_  ), 0                         , 0  , 0  , 2996 , 30 , 17 ), // #749
  INST(Std              , X86Op              , O(000000,FD,_,_,_,_,_,_  ), 0                         , 0  , 0  , 7921 , 30 , 18 ), // #750
  INST(Stgi             , X86Op              , O(000F01,DC,_,_,_,_,_,_  ), 0                         , 21 , 0  , 3000 , 30 , 117), // #751
  INST(Sti              , X86Op              , O(000000,FB,_,_,_,_,_,_  ), 0                         , 0  , 0  , 3005 , 30 , 23 ), // #752
  INST(Stmxcsr          , X86M_Only          , O(000F00,AE,3,_,_,_,_,_  ), 0                         , 78 , 0  , 11323, 101, 5  ), // #753
  INST(Stos             , X86StrMr           , O(000000,AA,_,_,_,_,_,_  ), 0                         , 0  , 0  , 3009 , 184, 75 ), // #754
  INST(Str              , X86M_NoMemSize     , O(000F00,00,1,_,_,_,_,_  ), 0                         , 29 , 0  , 3014 , 183, 0  ), // #755
  INST(Sttilecfg        , AmxCfg             , V(660F38,49,_,0,0,_,_,_  ), 0                         , 96 , 0  , 3018 , 103, 73 ), // #756
  INST(Stui             , X86Op              , O(F30F01,EF,_,_,_,_,_,_  ), 0                         , 25 , 0  , 3135 , 33 , 25 ), // #757
  INST(Sub              , X86Arith           , O(000000,28,5,_,x,_,_,_  ), 0                         , 62 , 0  , 866  , 179, 1  ), // #758
  INST(Subpd            , ExtRm              , O(660F00,5C,_,_,_,_,_,_  ), 0                         , 3  , 0  , 5413 , 5  , 4  ), // #759
  INST(Subps            , ExtRm              , O(000F00,5C,_,_,_,_,_,_  ), 0                         , 4  , 0  , 5425 , 5  , 5  ), // #760
  INST(Subsd            , ExtRm              , O(F20F00,5C,_,_,_,_,_,_  ), 0                         , 5  , 0  , 6392 , 6  , 4  ), // #761
  INST(Subss            , ExtRm              , O(F30F00,5C,_,_,_,_,_,_  ), 0                         , 6  , 0  , 6402 , 7  , 5  ), // #762
  INST(Swapgs           , X86Op              , O(000F01,F8,_,_,_,_,_,_  ), 0                         , 21 , 0  , 3028 , 33 , 0  ), // #763
  INST(Syscall          , X86Op              , O(000F00,05,_,_,_,_,_,_  ), 0                         , 4  , 0  , 3035 , 33 , 0  ), // #764
  INST(Sysenter         , X86Op              , O(000F00,34,_,_,_,_,_,_  ), 0                         , 4  , 0  , 3043 , 30 , 0  ), // #765
  INST(Sysexit          , X86Op              , O(000F00,35,_,_,_,_,_,_  ), 0                         , 4  , 0  , 3052 , 30 , 0  ), // #766
  INST(Sysexitq         , X86Op              , O(000F00,35,_,_,1,_,_,_  ), 0                         , 60 , 0  , 3060 , 30 , 0  ), // #767
  INST(Sysret           , X86Op              , O(000F00,07,_,_,_,_,_,_  ), 0                         , 4  , 0  , 3069 , 33 , 0  ), // #768
  INST(Sysretq          , X86Op              , O(000F00,07,_,_,1,_,_,_  ), 0                         , 60 , 0  , 3076 , 33 , 0  ), // #769
  INST(T1mskc           , VexVm_Wx           , V(XOP_M9,01,7,0,x,_,_,_  ), 0                         , 98 , 0  , 3084 , 14 , 11 ), // #770
  INST(Tdpbf16ps        , AmxRmv             , V(F30F38,5C,_,0,0,_,_,_  ), 0                         , 88 , 0  , 3091 , 185, 118), // #771
  INST(Tdpbssd          , AmxRmv             , V(F20F38,5E,_,0,0,_,_,_  ), 0                         , 84 , 0  , 3101 , 185, 119), // #772
  INST(Tdpbsud          , AmxRmv             , V(F30F38,5E,_,0,0,_,_,_  ), 0                         , 88 , 0  , 3109 , 185, 119), // #773
  INST(Tdpbusd          , AmxRmv             , V(660F38,5E,_,0,0,_,_,_  ), 0                         , 96 , 0  , 3117 , 185, 119), // #774
  INST(Tdpbuud          , AmxRmv             , V(000F38,5E,_,0,0,_,_,_  ), 0                         , 10 , 0  , 3125 , 185, 119), // #775
  INST(Test             , X86Test            , O(000000,84,_,_,x,_,_,_  ), O(000000,F6,_,_,x,_,_,_  ), 0  , 79 , 10367, 186, 1  ), // #776
  INST(Testui           , X86Op              , O(F30F01,ED,_,_,_,_,_,_  ), 0                         , 25 , 0  , 3133 , 33 , 120), // #777
  INST(Tileloadd        , AmxRm              , V(F20F38,4B,_,0,0,_,_,_  ), 0                         , 84 , 0  , 3140 , 187, 73 ), // #778
  INST(Tileloaddt1      , AmxRm              , V(660F38,4B,_,0,0,_,_,_  ), 0                         , 96 , 0  , 3150 , 187, 73 ), // #779
  INST(Tilerelease      , VexOpMod           , V(000F38,49,0,0,0,_,_,_  ), 0                         , 10 , 0  , 3162 , 188, 73 ), // #780
  INST(Tilestored       , AmxMr              , V(F30F38,4B,_,0,0,_,_,_  ), 0                         , 88 , 0  , 3174 , 189, 73 ), // #781
  INST(Tilezero         , AmxR               , V(F20F38,49,_,0,0,_,_,_  ), 0                         , 84 , 0  , 3185 , 190, 73 ), // #782
  INST(Tpause           , X86R32_EDX_EAX     , O(660F00,AE,6,_,_,_,_,_  ), 0                         , 26 , 0  , 3194 , 191, 121), // #783
  INST(Tzcnt            , X86Rm_Raw66H       , O(F30F00,BC,_,_,x,_,_,_  ), 0                         , 6  , 0  , 3201 , 22 , 9  ), // #784
  INST(Tzmsk            , VexVm_Wx           , V(XOP_M9,01,4,0,x,_,_,_  ), 0                         , 99 , 0  , 3207 , 14 , 11 ), // #785
  INST(Ucomisd          , ExtRm              , O(660F00,2E,_,_,_,_,_,_  ), 0                         , 3  , 0  , 11390, 6  , 41 ), // #786
  INST(Ucomiss          , ExtRm              , O(000F00,2E,_,_,_,_,_,_  ), 0                         , 4  , 0  , 11408, 7  , 42 ), // #787
  INST(Ud0              , X86Rm              , O(000F00,FF,_,_,_,_,_,_  ), 0                         , 4  , 0  , 3213 , 192, 0  ), // #788
  INST(Ud1              , X86Rm              , O(000F00,B9,_,_,_,_,_,_  ), 0                         , 4  , 0  , 3217 , 192, 0  ), // #789
  INST(Ud2              , X86Op              , O(000F00,0B,_,_,_,_,_,_  ), 0                         , 4  , 0  , 3221 , 30 , 0  ), // #790
  INST(Uiret            , X86Op              , O(F30F01,EC,_,_,_,_,_,_  ), 0                         , 25 , 0  , 3225 , 33 , 25 ), // #791
  INST(Umonitor         , X86R_FromM         , O(F30F00,AE,6,_,_,_,_,_  ), 0                         , 24 , 0  , 3231 , 193, 122), // #792
  INST(Umwait           , X86R32_EDX_EAX     , O(F20F00,AE,6,_,_,_,_,_  ), 0                         , 100, 0  , 3240 , 191, 121), // #793
  INST(Unpckhpd         , ExtRm              , O(660F00,15,_,_,_,_,_,_  ), 0                         , 3  , 0  , 11417, 5  , 4  ), // #794
  INST(Unpckhps         , ExtRm              , O(000F00,15,_,_,_,_,_,_  ), 0                         , 4  , 0  , 11427, 5  , 5  ), // #795
  INST(Unpcklpd         , ExtRm              , O(660F00,14,_,_,_,_,_,_  ), 0                         , 3  , 0  , 11437, 5  , 4  ), // #796
  INST(Unpcklps         , ExtRm              , O(000F00,14,_,_,_,_,_,_  ), 0                         , 4  , 0  , 11447, 5  , 5  ), // #797
  INST(V4fmaddps        , VexRm_T1_4X        , E(F20F38,9A,_,2,_,0,4,T4X), 0                         , 101, 0  , 3247 , 194, 123), // #798
  INST(V4fmaddss        , VexRm_T1_4X        , E(F20F38,9B,_,0,_,0,4,T4X), 0                         , 102, 0  , 3257 , 195, 123), // #799
  INST(V4fnmaddps       , VexRm_T1_4X        , E(F20F38,AA,_,2,_,0,4,T4X), 0                         , 101, 0  , 3267 , 194, 123), // #800
  INST(V4fnmaddss       , VexRm_T1_4X        , E(F20F38,AB,_,0,_,0,4,T4X), 0                         , 102, 0  , 3278 , 195, 123), // #801
  INST(Vaddpd           , VexRvm_Lx          , V(660F00,58,_,x,I,1,4,FV ), 0                         , 103, 0  , 3289 , 196, 124), // #802
  INST(Vaddph           , VexRvm_Lx          , E(00MAP5,58,_,_,_,0,4,FV ), 0                         , 104, 0  , 3296 , 197, 125), // #803
  INST(Vaddps           , VexRvm_Lx          , V(000F00,58,_,x,I,0,4,FV ), 0                         , 105, 0  , 3303 , 198, 124), // #804
  INST(Vaddsd           , VexRvm             , V(F20F00,58,_,I,I,1,3,T1S), 0                         , 106, 0  , 3310 , 199, 126), // #805
  INST(Vaddsh           , VexRvm             , E(F3MAP5,58,_,_,_,0,1,T1S), 0                         , 107, 0  , 3317 , 200, 127), // #806
  INST(Vaddss           , VexRvm             , V(F30F00,58,_,I,I,0,2,T1S), 0                         , 108, 0  , 3324 , 201, 126), // #807
  INST(Vaddsubpd        , VexRvm_Lx          , V(660F00,D0,_,x,I,_,_,_  ), 0                         , 69 , 0  , 3331 , 202, 128), // #808
  INST(Vaddsubps        , VexRvm_Lx          , V(F20F00,D0,_,x,I,_,_,_  ), 0                         , 109, 0  , 3341 , 202, 128), // #809
  INST(Vaesdec          , VexRvm_Lx          , V(660F38,DE,_,x,I,_,4,FVM), 0                         , 110, 0  , 3351 , 203, 129), // #810
  INST(Vaesdeclast      , VexRvm_Lx          , V(660F38,DF,_,x,I,_,4,FVM), 0                         , 110, 0  , 3359 , 203, 129), // #811
  INST(Vaesenc          , VexRvm_Lx          , V(660F38,DC,_,x,I,_,4,FVM), 0                         , 110, 0  , 3371 , 203, 129), // #812
  INST(Vaesenclast      , VexRvm_Lx          , V(660F38,DD,_,x,I,_,4,FVM), 0                         , 110, 0  , 3379 , 203, 129), // #813
  INST(Vaesimc          , VexRm              , V(660F38,DB,_,0,I,_,_,_  ), 0                         , 96 , 0  , 3391 , 204, 130), // #814
  INST(Vaeskeygenassist , VexRmi             , V(660F3A,DF,_,0,I,_,_,_  ), 0                         , 73 , 0  , 3399 , 205, 130), // #815
  INST(Valignd          , VexRvmi_Lx         , E(660F3A,03,_,x,_,0,4,FV ), 0                         , 111, 0  , 3416 , 206, 131), // #816
  INST(Valignq          , VexRvmi_Lx         , E(660F3A,03,_,x,_,1,4,FV ), 0                         , 112, 0  , 3424 , 207, 131), // #817
  INST(Vandnpd          , VexRvm_Lx          , V(660F00,55,_,x,I,1,4,FV ), 0                         , 103, 0  , 3432 , 208, 132), // #818
  INST(Vandnps          , VexRvm_Lx          , V(000F00,55,_,x,I,0,4,FV ), 0                         , 105, 0  , 3440 , 209, 132), // #819
  INST(Vandpd           , VexRvm_Lx          , V(660F00,54,_,x,I,1,4,FV ), 0                         , 103, 0  , 3448 , 210, 132), // #820
  INST(Vandps           , VexRvm_Lx          , V(000F00,54,_,x,I,0,4,FV ), 0                         , 105, 0  , 3455 , 211, 132), // #821
  INST(Vblendmpd        , VexRvm_Lx          , E(660F38,65,_,x,_,1,4,FV ), 0                         , 113, 0  , 3462 , 212, 131), // #822
  INST(Vblendmps        , VexRvm_Lx          , E(660F38,65,_,x,_,0,4,FV ), 0                         , 114, 0  , 3472 , 213, 131), // #823
  INST(Vblendpd         , VexRvmi_Lx         , V(660F3A,0D,_,x,I,_,_,_  ), 0                         , 73 , 0  , 3482 , 214, 128), // #824
  INST(Vblendps         , VexRvmi_Lx         , V(660F3A,0C,_,x,I,_,_,_  ), 0                         , 73 , 0  , 3491 , 214, 128), // #825
  INST(Vblendvpd        , VexRvmr_Lx         , V(660F3A,4B,_,x,0,_,_,_  ), 0                         , 73 , 0  , 3500 , 215, 128), // #826
  INST(Vblendvps        , VexRvmr_Lx         , V(660F3A,4A,_,x,0,_,_,_  ), 0                         , 73 , 0  , 3510 , 215, 128), // #827
  INST(Vbroadcastf128   , VexRm              , V(660F38,1A,_,1,0,_,_,_  ), 0                         , 115, 0  , 3520 , 216, 128), // #828
  INST(Vbroadcastf32x2  , VexRm_Lx           , E(660F38,19,_,x,_,0,3,T2 ), 0                         , 116, 0  , 3535 , 217, 133), // #829
  INST(Vbroadcastf32x4  , VexRm_Lx           , E(660F38,1A,_,x,_,0,4,T4 ), 0                         , 117, 0  , 3551 , 218, 68 ), // #830
  INST(Vbroadcastf32x8  , VexRm              , E(660F38,1B,_,2,_,0,5,T8 ), 0                         , 118, 0  , 3567 , 219, 66 ), // #831
  INST(Vbroadcastf64x2  , VexRm_Lx           , E(660F38,1A,_,x,_,1,4,T2 ), 0                         , 119, 0  , 3583 , 218, 133), // #832
  INST(Vbroadcastf64x4  , VexRm              , E(660F38,1B,_,2,_,1,5,T4 ), 0                         , 120, 0  , 3599 , 219, 68 ), // #833
  INST(Vbroadcasti128   , VexRm              , V(660F38,5A,_,1,0,_,_,_  ), 0                         , 115, 0  , 3615 , 216, 134), // #834
  INST(Vbroadcasti32x2  , VexRm_Lx           , E(660F38,59,_,x,_,0,3,T2 ), 0                         , 116, 0  , 3630 , 220, 133), // #835
  INST(Vbroadcasti32x4  , VexRm_Lx           , E(660F38,5A,_,x,_,0,4,T4 ), 0                         , 117, 0  , 3646 , 218, 131), // #836
  INST(Vbroadcasti32x8  , VexRm              , E(660F38,5B,_,2,_,0,5,T8 ), 0                         , 118, 0  , 3662 , 219, 66 ), // #837
  INST(Vbroadcasti64x2  , VexRm_Lx           , E(660F38,5A,_,x,_,1,4,T2 ), 0                         , 119, 0  , 3678 , 218, 133), // #838
  INST(Vbroadcasti64x4  , VexRm              , E(660F38,5B,_,2,_,1,5,T4 ), 0                         , 120, 0  , 3694 , 219, 68 ), // #839
  INST(Vbroadcastsd     , VexRm_Lx           , V(660F38,19,_,x,0,1,3,T1S), 0                         , 121, 0  , 3710 , 221, 135), // #840
  INST(Vbroadcastss     , VexRm_Lx           , V(660F38,18,_,x,0,0,2,T1S), 0                         , 122, 0  , 3723 , 222, 135), // #841
  INST(Vcmppd           , VexRvmi_Lx_KEvex   , V(660F00,C2,_,x,I,1,4,FV ), 0                         , 103, 0  , 3736 , 223, 124), // #842
  INST(Vcmpph           , VexRvmi_Lx_KEvex   , E(000F3A,C2,_,_,_,0,4,FV ), 0                         , 123, 0  , 3743 , 224, 125), // #843
  INST(Vcmpps           , VexRvmi_Lx_KEvex   , V(000F00,C2,_,x,I,0,4,FV ), 0                         , 105, 0  , 3750 , 225, 124), // #844
  INST(Vcmpsd           , VexRvmi_KEvex      , V(F20F00,C2,_,I,I,1,3,T1S), 0                         , 106, 0  , 3757 , 226, 126), // #845
  INST(Vcmpsh           , VexRvmi_KEvex      , E(F30F3A,C2,_,_,_,0,1,T1S), 0                         , 124, 0  , 3764 , 227, 127), // #846
  INST(Vcmpss           , VexRvmi_KEvex      , V(F30F00,C2,_,I,I,0,2,T1S), 0                         , 108, 0  , 3771 , 228, 126), // #847
  INST(Vcomisd          , VexRm              , V(660F00,2F,_,I,I,1,3,T1S), 0                         , 125, 0  , 3778 , 229, 136), // #848
  INST(Vcomish          , VexRm              , E(00MAP5,2F,_,_,_,0,1,T1S), 0                         , 126, 0  , 3786 , 230, 127), // #849
  INST(Vcomiss          , VexRm              , V(000F00,2F,_,I,I,0,2,T1S), 0                         , 127, 0  , 3794 , 231, 136), // #850
  INST(Vcompresspd      , VexMr_Lx           , E(660F38,8A,_,x,_,1,3,T1S), 0                         , 128, 0  , 3802 , 232, 131), // #851
  INST(Vcompressps      , VexMr_Lx           , E(660F38,8A,_,x,_,0,2,T1S), 0                         , 129, 0  , 3814 , 232, 131), // #852
  INST(Vcvtdq2pd        , VexRm_Lx           , V(F30F00,E6,_,x,I,0,3,HV ), 0                         , 130, 0  , 3826 , 233, 124), // #853
  INST(Vcvtdq2ph        , VexRm_Lx           , E(00MAP5,5B,_,_,_,0,4,FV ), 0                         , 104, 0  , 3836 , 234, 125), // #854
  INST(Vcvtdq2ps        , VexRm_Lx           , V(000F00,5B,_,x,I,0,4,FV ), 0                         , 105, 0  , 3846 , 235, 124), // #855
  INST(Vcvtne2ps2bf16   , VexRvm_Lx          , E(F20F38,72,_,_,_,0,4,FV ), 0                         , 131, 0  , 3856 , 213, 137), // #856
  INST(Vcvtneps2bf16    , VexRm_Lx_Narrow    , E(F30F38,72,_,_,_,0,4,FV ), 0                         , 132, 0  , 3871 , 236, 137), // #857
  INST(Vcvtpd2dq        , VexRm_Lx_Narrow    , V(F20F00,E6,_,x,I,1,4,FV ), 0                         , 133, 0  , 3885 , 237, 124), // #858
  INST(Vcvtpd2ph        , VexRm_Lx           , E(66MAP5,5A,_,_,_,1,4,FV ), 0                         , 134, 0  , 3895 , 238, 125), // #859
  INST(Vcvtpd2ps        , VexRm_Lx_Narrow    , V(660F00,5A,_,x,I,1,4,FV ), 0                         , 103, 0  , 3905 , 237, 124), // #860
  INST(Vcvtpd2qq        , VexRm_Lx           , E(660F00,7B,_,x,_,1,4,FV ), 0                         , 135, 0  , 3915 , 239, 133), // #861
  INST(Vcvtpd2udq       , VexRm_Lx_Narrow    , E(000F00,79,_,x,_,1,4,FV ), 0                         , 136, 0  , 3925 , 240, 131), // #862
  INST(Vcvtpd2uqq       , VexRm_Lx           , E(660F00,79,_,x,_,1,4,FV ), 0                         , 135, 0  , 3936 , 239, 133), // #863
  INST(Vcvtph2dq        , VexRm_Lx           , E(66MAP5,5B,_,_,_,0,3,HV ), 0                         , 137, 0  , 3947 , 241, 125), // #864
  INST(Vcvtph2pd        , VexRm_Lx           , E(00MAP5,5A,_,_,_,0,2,QV ), 0                         , 138, 0  , 3957 , 242, 125), // #865
  INST(Vcvtph2ps        , VexRm_Lx           , V(660F38,13,_,x,0,0,3,HVM), 0                         , 139, 0  , 3967 , 243, 138), // #866
  INST(Vcvtph2psx       , VexRm_Lx           , E(66MAP6,13,_,_,_,0,3,HV ), 0                         , 140, 0  , 3977 , 244, 125), // #867
  INST(Vcvtph2qq        , VexRm_Lx           , E(66MAP5,7B,_,_,_,0,2,QV ), 0                         , 141, 0  , 3988 , 245, 125), // #868
  INST(Vcvtph2udq       , VexRm_Lx           , E(00MAP5,79,_,_,_,0,3,HV ), 0                         , 142, 0  , 3998 , 241, 125), // #869
  INST(Vcvtph2uqq       , VexRm_Lx           , E(66MAP5,79,_,_,_,0,2,QV ), 0                         , 141, 0  , 4009 , 245, 125), // #870
  INST(Vcvtph2uw        , VexRm_Lx           , E(00MAP5,7D,_,_,_,0,4,FV ), 0                         , 104, 0  , 4020 , 246, 125), // #871
  INST(Vcvtph2w         , VexRm_Lx           , E(66MAP5,7D,_,_,_,0,4,FV ), 0                         , 143, 0  , 4030 , 246, 125), // #872
  INST(Vcvtps2dq        , VexRm_Lx           , V(660F00,5B,_,x,I,0,4,FV ), 0                         , 144, 0  , 4039 , 235, 124), // #873
  INST(Vcvtps2pd        , VexRm_Lx           , V(000F00,5A,_,x,I,0,3,HV ), 0                         , 145, 0  , 4049 , 247, 124), // #874
  INST(Vcvtps2ph        , VexMri_Lx          , V(660F3A,1D,_,x,0,0,3,HVM), 0                         , 146, 0  , 4059 , 248, 138), // #875
  INST(Vcvtps2phx       , VexRm_Lx           , E(66MAP5,1D,_,_,_,0,4,FV ), 0                         , 143, 0  , 4069 , 234, 125), // #876
  INST(Vcvtps2qq        , VexRm_Lx           , E(660F00,7B,_,x,_,0,3,HV ), 0                         , 147, 0  , 4080 , 249, 133), // #877
  INST(Vcvtps2udq       , VexRm_Lx           , E(000F00,79,_,x,_,0,4,FV ), 0                         , 148, 0  , 4090 , 250, 131), // #878
  INST(Vcvtps2uqq       , VexRm_Lx           , E(660F00,79,_,x,_,0,3,HV ), 0                         , 147, 0  , 4101 , 249, 133), // #879
  INST(Vcvtqq2pd        , VexRm_Lx           , E(F30F00,E6,_,x,_,1,4,FV ), 0                         , 149, 0  , 4112 , 239, 133), // #880
  INST(Vcvtqq2ph        , VexRm_Lx           , E(00MAP5,5B,_,_,_,1,4,FV ), 0                         , 150, 0  , 4122 , 238, 125), // #881
  INST(Vcvtqq2ps        , VexRm_Lx_Narrow    , E(000F00,5B,_,x,_,1,4,FV ), 0                         , 136, 0  , 4132 , 240, 133), // #882
  INST(Vcvtsd2sh        , VexRvm             , E(F2MAP5,5A,_,_,_,1,3,T1S), 0                         , 151, 0  , 4142 , 251, 127), // #883
  INST(Vcvtsd2si        , VexRm_Wx           , V(F20F00,2D,_,I,x,x,3,T1F), 0                         , 152, 0  , 4152 , 252, 126), // #884
  INST(Vcvtsd2ss        , VexRvm             , V(F20F00,5A,_,I,I,1,3,T1S), 0                         , 106, 0  , 4162 , 199, 126), // #885
  INST(Vcvtsd2usi       , VexRm_Wx           , E(F20F00,79,_,I,_,x,3,T1F), 0                         , 153, 0  , 4172 , 253, 68 ), // #886
  INST(Vcvtsh2sd        , VexRvm             , E(F3MAP5,5A,_,_,_,0,1,T1S), 0                         , 107, 0  , 4183 , 254, 127), // #887
  INST(Vcvtsh2si        , VexRm_Wx           , E(F3MAP5,2D,_,_,_,x,1,T1S), 0                         , 107, 0  , 4193 , 255, 127), // #888
  INST(Vcvtsh2ss        , VexRvm             , E(00MAP6,13,_,_,_,0,1,T1S), 0                         , 154, 0  , 4203 , 254, 127), // #889
  INST(Vcvtsh2usi       , VexRm_Wx           , E(F3MAP5,79,_,_,_,x,1,T1S), 0                         , 107, 0  , 4213 , 255, 127), // #890
  INST(Vcvtsi2sd        , VexRvm_Wx          , V(F20F00,2A,_,I,x,x,2,T1W), 0                         , 155, 0  , 4224 , 256, 126), // #891
  INST(Vcvtsi2sh        , VexRvm_Wx          , E(F3MAP5,2A,_,_,_,x,2,T1W), 0                         , 156, 0  , 4234 , 257, 127), // #892
  INST(Vcvtsi2ss        , VexRvm_Wx          , V(F30F00,2A,_,I,x,x,2,T1W), 0                         , 157, 0  , 4244 , 256, 126), // #893
  INST(Vcvtss2sd        , VexRvm             , V(F30F00,5A,_,I,I,0,2,T1S), 0                         , 108, 0  , 4254 , 258, 126), // #894
  INST(Vcvtss2sh        , VexRvm             , E(00MAP5,1D,_,_,_,0,2,T1S), 0                         , 158, 0  , 4264 , 259, 127), // #895
  INST(Vcvtss2si        , VexRm_Wx           , V(F30F00,2D,_,I,x,x,2,T1F), 0                         , 108, 0  , 4274 , 260, 126), // #896
  INST(Vcvtss2usi       , VexRm_Wx           , E(F30F00,79,_,I,_,x,2,T1F), 0                         , 159, 0  , 4284 , 261, 68 ), // #897
  INST(Vcvttpd2dq       , VexRm_Lx_Narrow    , V(660F00,E6,_,x,I,1,4,FV ), 0                         , 103, 0  , 4295 , 262, 124), // #898
  INST(Vcvttpd2qq       , VexRm_Lx           , E(660F00,7A,_,x,_,1,4,FV ), 0                         , 135, 0  , 4306 , 263, 131), // #899
  INST(Vcvttpd2udq      , VexRm_Lx_Narrow    , E(000F00,78,_,x,_,1,4,FV ), 0                         , 136, 0  , 4317 , 264, 131), // #900
  INST(Vcvttpd2uqq      , VexRm_Lx           , E(660F00,78,_,x,_,1,4,FV ), 0                         , 135, 0  , 4329 , 263, 133), // #901
  INST(Vcvttph2dq       , VexRm_Lx           , E(F3MAP5,5B,_,_,_,0,3,HV ), 0                         , 160, 0  , 4341 , 244, 125), // #902
  INST(Vcvttph2qq       , VexRm_Lx           , E(66MAP5,7A,_,_,_,0,2,QV ), 0                         , 141, 0  , 4352 , 242, 125), // #903
  INST(Vcvttph2udq      , VexRm_Lx           , E(00MAP5,78,_,_,_,0,3,HV ), 0                         , 142, 0  , 4363 , 244, 125), // #904
  INST(Vcvttph2uqq      , VexRm_Lx           , E(66MAP5,78,_,_,_,0,2,QV ), 0                         , 141, 0  , 4375 , 242, 125), // #905
  INST(Vcvttph2uw       , VexRm_Lx           , E(00MAP5,7C,_,_,_,0,4,FV ), 0                         , 104, 0  , 4387 , 265, 125), // #906
  INST(Vcvttph2w        , VexRm_Lx           , E(66MAP5,7C,_,_,_,0,4,FV ), 0                         , 143, 0  , 4398 , 265, 125), // #907
  INST(Vcvttps2dq       , VexRm_Lx           , V(F30F00,5B,_,x,I,0,4,FV ), 0                         , 161, 0  , 4408 , 266, 124), // #908
  INST(Vcvttps2qq       , VexRm_Lx           , E(660F00,7A,_,x,_,0,3,HV ), 0                         , 147, 0  , 4419 , 267, 133), // #909
  INST(Vcvttps2udq      , VexRm_Lx           , E(000F00,78,_,x,_,0,4,FV ), 0                         , 148, 0  , 4430 , 268, 131), // #910
  INST(Vcvttps2uqq      , VexRm_Lx           , E(660F00,78,_,x,_,0,3,HV ), 0                         , 147, 0  , 4442 , 267, 133), // #911
  INST(Vcvttsd2si       , VexRm_Wx           , V(F20F00,2C,_,I,x,x,3,T1F), 0                         , 152, 0  , 4454 , 269, 126), // #912
  INST(Vcvttsd2usi      , VexRm_Wx           , E(F20F00,78,_,I,_,x,3,T1F), 0                         , 153, 0  , 4465 , 270, 68 ), // #913
  INST(Vcvttsh2si       , VexRm_Wx           , E(F3MAP5,2C,_,_,_,x,1,T1S), 0                         , 107, 0  , 4477 , 271, 127), // #914
  INST(Vcvttsh2usi      , VexRm_Wx           , E(F3MAP5,78,_,_,_,x,1,T1S), 0                         , 107, 0  , 4488 , 271, 127), // #915
  INST(Vcvttss2si       , VexRm_Wx           , V(F30F00,2C,_,I,x,x,2,T1F), 0                         , 108, 0  , 4500 , 272, 126), // #916
  INST(Vcvttss2usi      , VexRm_Wx           , E(F30F00,78,_,I,_,x,2,T1F), 0                         , 159, 0  , 4511 , 273, 68 ), // #917
  INST(Vcvtudq2pd       , VexRm_Lx           , E(F30F00,7A,_,x,_,0,3,HV ), 0                         , 162, 0  , 4523 , 274, 131), // #918
  INST(Vcvtudq2ph       , VexRm_Lx           , E(F2MAP5,7A,_,_,_,0,4,FV ), 0                         , 163, 0  , 4534 , 234, 125), // #919
  INST(Vcvtudq2ps       , VexRm_Lx           , E(F20F00,7A,_,x,_,0,4,FV ), 0                         , 164, 0  , 4545 , 250, 131), // #920
  INST(Vcvtuqq2pd       , VexRm_Lx           , E(F30F00,7A,_,x,_,1,4,FV ), 0                         , 149, 0  , 4556 , 239, 133), // #921
  INST(Vcvtuqq2ph       , VexRm_Lx           , E(F2MAP5,7A,_,_,_,1,4,FV ), 0                         , 165, 0  , 4567 , 238, 125), // #922
  INST(Vcvtuqq2ps       , VexRm_Lx_Narrow    , E(F20F00,7A,_,x,_,1,4,FV ), 0                         , 166, 0  , 4578 , 240, 133), // #923
  INST(Vcvtusi2sd       , VexRvm_Wx          , E(F20F00,7B,_,I,_,x,2,T1W), 0                         , 167, 0  , 4589 , 257, 68 ), // #924
  INST(Vcvtusi2sh       , VexRvm_Wx          , E(F3MAP5,7B,_,_,_,x,2,T1W), 0                         , 156, 0  , 4600 , 257, 127), // #925
  INST(Vcvtusi2ss       , VexRvm_Wx          , E(F30F00,7B,_,I,_,x,2,T1W), 0                         , 168, 0  , 4611 , 257, 68 ), // #926
  INST(Vcvtuw2ph        , VexRm_Lx           , E(F2MAP5,7D,_,_,_,0,4,FV ), 0                         , 163, 0  , 4622 , 246, 125), // #927
  INST(Vcvtw2ph         , VexRm_Lx           , E(F3MAP5,7D,_,_,_,0,4,FV ), 0                         , 169, 0  , 4632 , 246, 125), // #928
  INST(Vdbpsadbw        , VexRvmi_Lx         , E(660F3A,42,_,x,_,0,4,FVM), 0                         , 111, 0  , 4641 , 275, 139), // #929
  INST(Vdivpd           , VexRvm_Lx          , V(660F00,5E,_,x,I,1,4,FV ), 0                         , 103, 0  , 4651 , 196, 124), // #930
  INST(Vdivph           , VexRvm_Lx          , E(00MAP5,5E,_,_,_,0,4,FV ), 0                         , 104, 0  , 4658 , 197, 125), // #931
  INST(Vdivps           , VexRvm_Lx          , V(000F00,5E,_,x,I,0,4,FV ), 0                         , 105, 0  , 4665 , 198, 124), // #932
  INST(Vdivsd           , VexRvm             , V(F20F00,5E,_,I,I,1,3,T1S), 0                         , 106, 0  , 4672 , 199, 126), // #933
  INST(Vdivsh           , VexRvm             , E(F3MAP5,5E,_,_,_,0,1,T1S), 0                         , 107, 0  , 4679 , 200, 127), // #934
  INST(Vdivss           , VexRvm             , V(F30F00,5E,_,I,I,0,2,T1S), 0                         , 108, 0  , 4686 , 201, 126), // #935
  INST(Vdpbf16ps        , VexRvm_Lx          , E(F30F38,52,_,_,_,0,4,FV ), 0                         , 132, 0  , 4693 , 213, 137), // #936
  INST(Vdppd            , VexRvmi_Lx         , V(660F3A,41,_,x,I,_,_,_  ), 0                         , 73 , 0  , 4703 , 276, 128), // #937
  INST(Vdpps            , VexRvmi_Lx         , V(660F3A,40,_,x,I,_,_,_  ), 0                         , 73 , 0  , 4709 , 214, 128), // #938
  INST(Verr             , X86M_NoSize        , O(000F00,00,4,_,_,_,_,_  ), 0                         , 97 , 0  , 4715 , 107, 10 ), // #939
  INST(Verw             , X86M_NoSize        , O(000F00,00,5,_,_,_,_,_  ), 0                         , 77 , 0  , 4720 , 107, 10 ), // #940
  INST(Vexp2pd          , VexRm              , E(660F38,C8,_,2,_,1,4,FV ), 0                         , 170, 0  , 4725 , 277, 140), // #941
  INST(Vexp2ps          , VexRm              , E(660F38,C8,_,2,_,0,4,FV ), 0                         , 171, 0  , 4733 , 278, 140), // #942
  INST(Vexpandpd        , VexRm_Lx           , E(660F38,88,_,x,_,1,3,T1S), 0                         , 128, 0  , 4741 , 279, 131), // #943
  INST(Vexpandps        , VexRm_Lx           , E(660F38,88,_,x,_,0,2,T1S), 0                         , 129, 0  , 4751 , 279, 131), // #944
  INST(Vextractf128     , VexMri             , V(660F3A,19,_,1,0,_,_,_  ), 0                         , 172, 0  , 4761 , 280, 128), // #945
  INST(Vextractf32x4    , VexMri_Lx          , E(660F3A,19,_,x,_,0,4,T4 ), 0                         , 173, 0  , 4774 , 281, 131), // #946
  INST(Vextractf32x8    , VexMri             , E(660F3A,1B,_,2,_,0,5,T8 ), 0                         , 174, 0  , 4788 , 282, 66 ), // #947
  INST(Vextractf64x2    , VexMri_Lx          , E(660F3A,19,_,x,_,1,4,T2 ), 0                         , 175, 0  , 4802 , 281, 133), // #948
  INST(Vextractf64x4    , VexMri             , E(660F3A,1B,_,2,_,1,5,T4 ), 0                         , 176, 0  , 4816 , 282, 68 ), // #949
  INST(Vextracti128     , VexMri             , V(660F3A,39,_,1,0,_,_,_  ), 0                         , 172, 0  , 4830 , 280, 134), // #950
  INST(Vextracti32x4    , VexMri_Lx          , E(660F3A,39,_,x,_,0,4,T4 ), 0                         , 173, 0  , 4843 , 281, 131), // #951
  INST(Vextracti32x8    , VexMri             , E(660F3A,3B,_,2,_,0,5,T8 ), 0                         , 174, 0  , 4857 , 282, 66 ), // #952
  INST(Vextracti64x2    , VexMri_Lx          , E(660F3A,39,_,x,_,1,4,T2 ), 0                         , 175, 0  , 4871 , 281, 133), // #953
  INST(Vextracti64x4    , VexMri             , E(660F3A,3B,_,2,_,1,5,T4 ), 0                         , 176, 0  , 4885 , 282, 68 ), // #954
  INST(Vextractps       , VexMri             , V(660F3A,17,_,0,I,I,2,T1S), 0                         , 177, 0  , 4899 , 283, 126), // #955
  INST(Vfcmaddcph       , VexRvm_Lx          , E(F2MAP6,56,_,_,_,0,4,FV ), 0                         , 178, 0  , 4910 , 284, 125), // #956
  INST(Vfcmaddcsh       , VexRvm             , E(F2MAP6,57,_,_,_,0,2,T1S), 0                         , 179, 0  , 4921 , 259, 125), // #957
  INST(Vfcmulcph        , VexRvm_Lx          , E(F2MAP6,D6,_,_,_,0,4,FV ), 0                         , 178, 0  , 4932 , 284, 125), // #958
  INST(Vfcmulcsh        , VexRvm             , E(F2MAP6,D7,_,_,_,0,2,T1S), 0                         , 179, 0  , 4942 , 259, 125), // #959
  INST(Vfixupimmpd      , VexRvmi_Lx         , E(660F3A,54,_,x,_,1,4,FV ), 0                         , 112, 0  , 4952 , 285, 131), // #960
  INST(Vfixupimmps      , VexRvmi_Lx         , E(660F3A,54,_,x,_,0,4,FV ), 0                         , 111, 0  , 4964 , 286, 131), // #961
  INST(Vfixupimmsd      , VexRvmi            , E(660F3A,55,_,I,_,1,3,T1S), 0                         , 180, 0  , 4976 , 287, 68 ), // #962
  INST(Vfixupimmss      , VexRvmi            , E(660F3A,55,_,I,_,0,2,T1S), 0                         , 181, 0  , 4988 , 288, 68 ), // #963
  INST(Vfmadd132pd      , VexRvm_Lx          , V(660F38,98,_,x,1,1,4,FV ), 0                         , 182, 0  , 5000 , 196, 141), // #964
  INST(Vfmadd132ph      , VexRvm_Lx          , E(66MAP6,98,_,_,_,0,4,FV ), 0                         , 183, 0  , 5012 , 197, 125), // #965
  INST(Vfmadd132ps      , VexRvm_Lx          , V(660F38,98,_,x,0,0,4,FV ), 0                         , 110, 0  , 5024 , 198, 141), // #966
  INST(Vfmadd132sd      , VexRvm             , V(660F38,99,_,I,1,1,3,T1S), 0                         , 184, 0  , 5036 , 199, 142), // #967
  INST(Vfmadd132sh      , VexRvm             , E(66MAP6,99,_,_,_,0,1,T1S), 0                         , 185, 0  , 5048 , 200, 127), // #968
  INST(Vfmadd132ss      , VexRvm             , V(660F38,99,_,I,0,0,2,T1S), 0                         , 122, 0  , 5060 , 201, 142), // #969
  INST(Vfmadd213pd      , VexRvm_Lx          , V(660F38,A8,_,x,1,1,4,FV ), 0                         , 182, 0  , 5072 , 196, 141), // #970
  INST(Vfmadd213ph      , VexRvm_Lx          , E(66MAP6,A8,_,_,_,0,4,FV ), 0                         , 183, 0  , 5084 , 197, 125), // #971
  INST(Vfmadd213ps      , VexRvm_Lx          , V(660F38,A8,_,x,0,0,4,FV ), 0                         , 110, 0  , 5096 , 198, 141), // #972
  INST(Vfmadd213sd      , VexRvm             , V(660F38,A9,_,I,1,1,3,T1S), 0                         , 184, 0  , 5108 , 199, 142), // #973
  INST(Vfmadd213sh      , VexRvm             , E(66MAP6,A9,_,_,_,0,1,T1S), 0                         , 185, 0  , 5120 , 200, 127), // #974
  INST(Vfmadd213ss      , VexRvm             , V(660F38,A9,_,I,0,0,2,T1S), 0                         , 122, 0  , 5132 , 201, 142), // #975
  INST(Vfmadd231pd      , VexRvm_Lx          , V(660F38,B8,_,x,1,1,4,FV ), 0                         , 182, 0  , 5144 , 196, 141), // #976
  INST(Vfmadd231ph      , VexRvm_Lx          , E(66MAP6,B8,_,_,_,0,4,FV ), 0                         , 183, 0  , 5156 , 197, 125), // #977
  INST(Vfmadd231ps      , VexRvm_Lx          , V(660F38,B8,_,x,0,0,4,FV ), 0                         , 110, 0  , 5168 , 198, 141), // #978
  INST(Vfmadd231sd      , VexRvm             , V(660F38,B9,_,I,1,1,3,T1S), 0                         , 184, 0  , 5180 , 199, 142), // #979
  INST(Vfmadd231sh      , VexRvm             , E(66MAP6,B9,_,_,_,0,1,T1S), 0                         , 185, 0  , 5192 , 200, 127), // #980
  INST(Vfmadd231ss      , VexRvm             , V(660F38,B9,_,I,0,0,2,T1S), 0                         , 122, 0  , 5204 , 201, 142), // #981
  INST(Vfmaddcph        , VexRvm_Lx          , E(F3MAP6,56,_,_,_,0,4,FV ), 0                         , 186, 0  , 5216 , 284, 125), // #982
  INST(Vfmaddcsh        , VexRvm             , E(F3MAP6,57,_,_,_,0,2,T1S), 0                         , 187, 0  , 5226 , 259, 125), // #983
  INST(Vfmaddpd         , Fma4_Lx            , V(660F3A,69,_,x,x,_,_,_  ), 0                         , 73 , 0  , 5236 , 289, 143), // #984
  INST(Vfmaddps         , Fma4_Lx            , V(660F3A,68,_,x,x,_,_,_  ), 0                         , 73 , 0  , 5245 , 289, 143), // #985
  INST(Vfmaddsd         , Fma4               , V(660F3A,6B,_,0,x,_,_,_  ), 0                         , 73 , 0  , 5254 , 290, 143), // #986
  INST(Vfmaddss         , Fma4               , V(660F3A,6A,_,0,x,_,_,_  ), 0                         , 73 , 0  , 5263 , 291, 143), // #987
  INST(Vfmaddsub132pd   , VexRvm_Lx          , V(660F38,96,_,x,1,1,4,FV ), 0                         , 182, 0  , 5272 , 196, 141), // #988
  INST(Vfmaddsub132ph   , VexRvm_Lx          , E(66MAP6,96,_,_,_,0,4,FV ), 0                         , 183, 0  , 5287 , 197, 125), // #989
  INST(Vfmaddsub132ps   , VexRvm_Lx          , V(660F38,96,_,x,0,0,4,FV ), 0                         , 110, 0  , 5302 , 198, 141), // #990
  INST(Vfmaddsub213pd   , VexRvm_Lx          , V(660F38,A6,_,x,1,1,4,FV ), 0                         , 182, 0  , 5317 , 196, 141), // #991
  INST(Vfmaddsub213ph   , VexRvm_Lx          , E(66MAP6,A6,_,_,_,0,4,FV ), 0                         , 183, 0  , 5332 , 197, 125), // #992
  INST(Vfmaddsub213ps   , VexRvm_Lx          , V(660F38,A6,_,x,0,0,4,FV ), 0                         , 110, 0  , 5347 , 198, 141), // #993
  INST(Vfmaddsub231pd   , VexRvm_Lx          , V(660F38,B6,_,x,1,1,4,FV ), 0                         , 182, 0  , 5362 , 196, 141), // #994
  INST(Vfmaddsub231ph   , VexRvm_Lx          , E(66MAP6,B6,_,_,_,0,4,FV ), 0                         , 183, 0  , 5377 , 197, 125), // #995
  INST(Vfmaddsub231ps   , VexRvm_Lx          , V(660F38,B6,_,x,0,0,4,FV ), 0                         , 110, 0  , 5392 , 198, 141), // #996
  INST(Vfmaddsubpd      , Fma4_Lx            , V(660F3A,5D,_,x,x,_,_,_  ), 0                         , 73 , 0  , 5407 , 289, 143), // #997
  INST(Vfmaddsubps      , Fma4_Lx            , V(660F3A,5C,_,x,x,_,_,_  ), 0                         , 73 , 0  , 5419 , 289, 143), // #998
  INST(Vfmsub132pd      , VexRvm_Lx          , V(660F38,9A,_,x,1,1,4,FV ), 0                         , 182, 0  , 5431 , 196, 141), // #999
  INST(Vfmsub132ph      , VexRvm_Lx          , E(66MAP6,9A,_,_,_,0,4,FV ), 0                         , 183, 0  , 5443 , 197, 125), // #1000
  INST(Vfmsub132ps      , VexRvm_Lx          , V(660F38,9A,_,x,0,0,4,FV ), 0                         , 110, 0  , 5455 , 198, 141), // #1001
  INST(Vfmsub132sd      , VexRvm             , V(660F38,9B,_,I,1,1,3,T1S), 0                         , 184, 0  , 5467 , 199, 142), // #1002
  INST(Vfmsub132sh      , VexRvm             , E(66MAP6,9B,_,_,_,0,1,T1S), 0                         , 185, 0  , 5479 , 200, 127), // #1003
  INST(Vfmsub132ss      , VexRvm             , V(660F38,9B,_,I,0,0,2,T1S), 0                         , 122, 0  , 5491 , 201, 142), // #1004
  INST(Vfmsub213pd      , VexRvm_Lx          , V(660F38,AA,_,x,1,1,4,FV ), 0                         , 182, 0  , 5503 , 196, 141), // #1005
  INST(Vfmsub213ph      , VexRvm_Lx          , E(66MAP6,AA,_,_,_,0,4,FV ), 0                         , 183, 0  , 5515 , 197, 125), // #1006
  INST(Vfmsub213ps      , VexRvm_Lx          , V(660F38,AA,_,x,0,0,4,FV ), 0                         , 110, 0  , 5527 , 198, 141), // #1007
  INST(Vfmsub213sd      , VexRvm             , V(660F38,AB,_,I,1,1,3,T1S), 0                         , 184, 0  , 5539 , 199, 142), // #1008
  INST(Vfmsub213sh      , VexRvm             , E(66MAP6,AB,_,_,_,0,1,T1S), 0                         , 185, 0  , 5551 , 200, 127), // #1009
  INST(Vfmsub213ss      , VexRvm             , V(660F38,AB,_,I,0,0,2,T1S), 0                         , 122, 0  , 5563 , 201, 142), // #1010
  INST(Vfmsub231pd      , VexRvm_Lx          , V(660F38,BA,_,x,1,1,4,FV ), 0                         , 182, 0  , 5575 , 196, 141), // #1011
  INST(Vfmsub231ph      , VexRvm_Lx          , E(66MAP6,BA,_,_,_,0,4,FV ), 0                         , 183, 0  , 5587 , 197, 125), // #1012
  INST(Vfmsub231ps      , VexRvm_Lx          , V(660F38,BA,_,x,0,0,4,FV ), 0                         , 110, 0  , 5599 , 198, 141), // #1013
  INST(Vfmsub231sd      , VexRvm             , V(660F38,BB,_,I,1,1,3,T1S), 0                         , 184, 0  , 5611 , 199, 142), // #1014
  INST(Vfmsub231sh      , VexRvm             , E(66MAP6,BB,_,_,_,0,1,T1S), 0                         , 185, 0  , 5623 , 200, 127), // #1015
  INST(Vfmsub231ss      , VexRvm             , V(660F38,BB,_,I,0,0,2,T1S), 0                         , 122, 0  , 5635 , 201, 142), // #1016
  INST(Vfmsubadd132pd   , VexRvm_Lx          , V(660F38,97,_,x,1,1,4,FV ), 0                         , 182, 0  , 5647 , 196, 141), // #1017
  INST(Vfmsubadd132ph   , VexRvm_Lx          , E(66MAP6,97,_,_,_,0,4,FV ), 0                         , 183, 0  , 5662 , 197, 125), // #1018
  INST(Vfmsubadd132ps   , VexRvm_Lx          , V(660F38,97,_,x,0,0,4,FV ), 0                         , 110, 0  , 5677 , 198, 141), // #1019
  INST(Vfmsubadd213pd   , VexRvm_Lx          , V(660F38,A7,_,x,1,1,4,FV ), 0                         , 182, 0  , 5692 , 196, 141), // #1020
  INST(Vfmsubadd213ph   , VexRvm_Lx          , E(66MAP6,A7,_,_,_,0,4,FV ), 0                         , 183, 0  , 5707 , 197, 125), // #1021
  INST(Vfmsubadd213ps   , VexRvm_Lx          , V(660F38,A7,_,x,0,0,4,FV ), 0                         , 110, 0  , 5722 , 198, 141), // #1022
  INST(Vfmsubadd231pd   , VexRvm_Lx          , V(660F38,B7,_,x,1,1,4,FV ), 0                         , 182, 0  , 5737 , 196, 141), // #1023
  INST(Vfmsubadd231ph   , VexRvm_Lx          , E(66MAP6,B7,_,_,_,0,4,FV ), 0                         , 183, 0  , 5752 , 197, 125), // #1024
  INST(Vfmsubadd231ps   , VexRvm_Lx          , V(660F38,B7,_,x,0,0,4,FV ), 0                         , 110, 0  , 5767 , 198, 141), // #1025
  INST(Vfmsubaddpd      , Fma4_Lx            , V(660F3A,5F,_,x,x,_,_,_  ), 0                         , 73 , 0  , 5782 , 289, 143), // #1026
  INST(Vfmsubaddps      , Fma4_Lx            , V(660F3A,5E,_,x,x,_,_,_  ), 0                         , 73 , 0  , 5794 , 289, 143), // #1027
  INST(Vfmsubpd         , Fma4_Lx            , V(660F3A,6D,_,x,x,_,_,_  ), 0                         , 73 , 0  , 5806 , 289, 143), // #1028
  INST(Vfmsubps         , Fma4_Lx            , V(660F3A,6C,_,x,x,_,_,_  ), 0                         , 73 , 0  , 5815 , 289, 143), // #1029
  INST(Vfmsubsd         , Fma4               , V(660F3A,6F,_,0,x,_,_,_  ), 0                         , 73 , 0  , 5824 , 290, 143), // #1030
  INST(Vfmsubss         , Fma4               , V(660F3A,6E,_,0,x,_,_,_  ), 0                         , 73 , 0  , 5833 , 291, 143), // #1031
  INST(Vfmulcph         , VexRvm_Lx          , E(F3MAP6,D6,_,_,_,0,4,FV ), 0                         , 186, 0  , 5842 , 284, 125), // #1032
  INST(Vfmulcsh         , VexRvm             , E(F3MAP6,D7,_,_,_,0,2,T1S), 0                         , 187, 0  , 5851 , 259, 125), // #1033
  INST(Vfnmadd132pd     , VexRvm_Lx          , V(660F38,9C,_,x,1,1,4,FV ), 0                         , 182, 0  , 5860 , 196, 141), // #1034
  INST(Vfnmadd132ph     , VexRvm_Lx          , E(66MAP6,9C,_,_,_,0,4,FV ), 0                         , 183, 0  , 5873 , 197, 125), // #1035
  INST(Vfnmadd132ps     , VexRvm_Lx          , V(660F38,9C,_,x,0,0,4,FV ), 0                         , 110, 0  , 5886 , 198, 141), // #1036
  INST(Vfnmadd132sd     , VexRvm             , V(660F38,9D,_,I,1,1,3,T1S), 0                         , 184, 0  , 5899 , 199, 142), // #1037
  INST(Vfnmadd132sh     , VexRvm             , E(66MAP6,9D,_,_,_,0,1,T1S), 0                         , 185, 0  , 5912 , 200, 127), // #1038
  INST(Vfnmadd132ss     , VexRvm             , V(660F38,9D,_,I,0,0,2,T1S), 0                         , 122, 0  , 5925 , 201, 142), // #1039
  INST(Vfnmadd213pd     , VexRvm_Lx          , V(660F38,AC,_,x,1,1,4,FV ), 0                         , 182, 0  , 5938 , 196, 141), // #1040
  INST(Vfnmadd213ph     , VexRvm_Lx          , E(66MAP6,AC,_,_,_,0,4,FV ), 0                         , 183, 0  , 5951 , 197, 125), // #1041
  INST(Vfnmadd213ps     , VexRvm_Lx          , V(660F38,AC,_,x,0,0,4,FV ), 0                         , 110, 0  , 5964 , 198, 141), // #1042
  INST(Vfnmadd213sd     , VexRvm             , V(660F38,AD,_,I,1,1,3,T1S), 0                         , 184, 0  , 5977 , 199, 142), // #1043
  INST(Vfnmadd213sh     , VexRvm             , E(66MAP6,AD,_,_,_,0,1,T1S), 0                         , 185, 0  , 5990 , 200, 127), // #1044
  INST(Vfnmadd213ss     , VexRvm             , V(660F38,AD,_,I,0,0,2,T1S), 0                         , 122, 0  , 6003 , 201, 142), // #1045
  INST(Vfnmadd231pd     , VexRvm_Lx          , V(660F38,BC,_,x,1,1,4,FV ), 0                         , 182, 0  , 6016 , 196, 141), // #1046
  INST(Vfnmadd231ph     , VexRvm_Lx          , E(66MAP6,BC,_,_,_,0,4,FV ), 0                         , 183, 0  , 6029 , 197, 125), // #1047
  INST(Vfnmadd231ps     , VexRvm_Lx          , V(660F38,BC,_,x,0,0,4,FV ), 0                         , 110, 0  , 6042 , 198, 141), // #1048
  INST(Vfnmadd231sd     , VexRvm             , V(660F38,BD,_,I,1,1,3,T1S), 0                         , 184, 0  , 6055 , 199, 142), // #1049
  INST(Vfnmadd231sh     , VexRvm             , E(66MAP6,BD,_,_,_,0,1,T1S), 0                         , 185, 0  , 6068 , 200, 127), // #1050
  INST(Vfnmadd231ss     , VexRvm             , V(660F38,BD,_,I,0,0,2,T1S), 0                         , 122, 0  , 6081 , 201, 142), // #1051
  INST(Vfnmaddpd        , Fma4_Lx            , V(660F3A,79,_,x,x,_,_,_  ), 0                         , 73 , 0  , 6094 , 289, 143), // #1052
  INST(Vfnmaddps        , Fma4_Lx            , V(660F3A,78,_,x,x,_,_,_  ), 0                         , 73 , 0  , 6104 , 289, 143), // #1053
  INST(Vfnmaddsd        , Fma4               , V(660F3A,7B,_,0,x,_,_,_  ), 0                         , 73 , 0  , 6114 , 290, 143), // #1054
  INST(Vfnmaddss        , Fma4               , V(660F3A,7A,_,0,x,_,_,_  ), 0                         , 73 , 0  , 6124 , 291, 143), // #1055
  INST(Vfnmsub132pd     , VexRvm_Lx          , V(660F38,9E,_,x,1,1,4,FV ), 0                         , 182, 0  , 6134 , 196, 141), // #1056
  INST(Vfnmsub132ph     , VexRvm_Lx          , E(66MAP6,9E,_,_,_,0,4,FV ), 0                         , 183, 0  , 6147 , 197, 125), // #1057
  INST(Vfnmsub132ps     , VexRvm_Lx          , V(660F38,9E,_,x,0,0,4,FV ), 0                         , 110, 0  , 6160 , 198, 141), // #1058
  INST(Vfnmsub132sd     , VexRvm             , V(660F38,9F,_,I,1,1,3,T1S), 0                         , 184, 0  , 6173 , 199, 142), // #1059
  INST(Vfnmsub132sh     , VexRvm             , E(66MAP6,9F,_,_,_,0,1,T1S), 0                         , 185, 0  , 6186 , 200, 127), // #1060
  INST(Vfnmsub132ss     , VexRvm             , V(660F38,9F,_,I,0,0,2,T1S), 0                         , 122, 0  , 6199 , 201, 142), // #1061
  INST(Vfnmsub213pd     , VexRvm_Lx          , V(660F38,AE,_,x,1,1,4,FV ), 0                         , 182, 0  , 6212 , 196, 141), // #1062
  INST(Vfnmsub213ph     , VexRvm_Lx          , E(66MAP6,AE,_,_,_,0,4,FV ), 0                         , 183, 0  , 6225 , 197, 125), // #1063
  INST(Vfnmsub213ps     , VexRvm_Lx          , V(660F38,AE,_,x,0,0,4,FV ), 0                         , 110, 0  , 6238 , 198, 141), // #1064
  INST(Vfnmsub213sd     , VexRvm             , V(660F38,AF,_,I,1,1,3,T1S), 0                         , 184, 0  , 6251 , 199, 142), // #1065
  INST(Vfnmsub213sh     , VexRvm             , E(66MAP6,AF,_,_,_,0,1,T1S), 0                         , 185, 0  , 6264 , 200, 127), // #1066
  INST(Vfnmsub213ss     , VexRvm             , V(660F38,AF,_,I,0,0,2,T1S), 0                         , 122, 0  , 6277 , 201, 142), // #1067
  INST(Vfnmsub231pd     , VexRvm_Lx          , V(660F38,BE,_,x,1,1,4,FV ), 0                         , 182, 0  , 6290 , 196, 141), // #1068
  INST(Vfnmsub231ph     , VexRvm_Lx          , E(66MAP6,BE,_,_,_,0,4,FV ), 0                         , 183, 0  , 6303 , 197, 125), // #1069
  INST(Vfnmsub231ps     , VexRvm_Lx          , V(660F38,BE,_,x,0,0,4,FV ), 0                         , 110, 0  , 6316 , 198, 141), // #1070
  INST(Vfnmsub231sd     , VexRvm             , V(660F38,BF,_,I,1,1,3,T1S), 0                         , 184, 0  , 6329 , 199, 142), // #1071
  INST(Vfnmsub231sh     , VexRvm             , E(66MAP6,BF,_,_,_,0,1,T1S), 0                         , 185, 0  , 6342 , 200, 127), // #1072
  INST(Vfnmsub231ss     , VexRvm             , V(660F38,BF,_,I,0,0,2,T1S), 0                         , 122, 0  , 6355 , 201, 142), // #1073
  INST(Vfnmsubpd        , Fma4_Lx            , V(660F3A,7D,_,x,x,_,_,_  ), 0                         , 73 , 0  , 6368 , 289, 143), // #1074
  INST(Vfnmsubps        , Fma4_Lx            , V(660F3A,7C,_,x,x,_,_,_  ), 0                         , 73 , 0  , 6378 , 289, 143), // #1075
  INST(Vfnmsubsd        , Fma4               , V(660F3A,7F,_,0,x,_,_,_  ), 0                         , 73 , 0  , 6388 , 290, 143), // #1076
  INST(Vfnmsubss        , Fma4               , V(660F3A,7E,_,0,x,_,_,_  ), 0                         , 73 , 0  , 6398 , 291, 143), // #1077
  INST(Vfpclasspd       , VexRmi_Lx          , E(660F3A,66,_,x,_,1,4,FV ), 0                         , 112, 0  , 6408 , 292, 133), // #1078
  INST(Vfpclassph       , VexRmi_Lx          , E(000F3A,66,_,_,_,0,4,FV ), 0                         , 123, 0  , 6419 , 293, 125), // #1079
  INST(Vfpclassps       , VexRmi_Lx          , E(660F3A,66,_,x,_,0,4,FV ), 0                         , 111, 0  , 6430 , 294, 133), // #1080
  INST(Vfpclasssd       , VexRmi             , E(660F3A,67,_,I,_,1,3,T1S), 0                         , 180, 0  , 6441 , 295, 66 ), // #1081
  INST(Vfpclasssh       , VexRmi             , E(000F3A,67,_,_,_,0,1,T1S), 0                         , 188, 0  , 6452 , 296, 127), // #1082
  INST(Vfpclassss       , VexRmi             , E(660F3A,67,_,I,_,0,2,T1S), 0                         , 181, 0  , 6463 , 297, 66 ), // #1083
  INST(Vfrczpd          , VexRm_Lx           , V(XOP_M9,81,_,x,0,_,_,_  ), 0                         , 79 , 0  , 6474 , 298, 144), // #1084
  INST(Vfrczps          , VexRm_Lx           , V(XOP_M9,80,_,x,0,_,_,_  ), 0                         , 79 , 0  , 6482 , 298, 144), // #1085
  INST(Vfrczsd          , VexRm              , V(XOP_M9,83,_,0,0,_,_,_  ), 0                         , 79 , 0  , 6490 , 299, 144), // #1086
  INST(Vfrczss          , VexRm              , V(XOP_M9,82,_,0,0,_,_,_  ), 0                         , 79 , 0  , 6498 , 300, 144), // #1087
  INST(Vgatherdpd       , VexRmvRm_VM        , V(660F38,92,_,x,1,_,_,_  ), E(660F38,92,_,x,_,1,3,T1S), 189, 80 , 6506 , 301, 145), // #1088
  INST(Vgatherdps       , VexRmvRm_VM        , V(660F38,92,_,x,0,_,_,_  ), E(660F38,92,_,x,_,0,2,T1S), 96 , 81 , 6517 , 302, 145), // #1089
  INST(Vgatherpf0dpd    , VexM_VM            , E(660F38,C6,1,2,_,1,3,T1S), 0                         , 190, 0  , 6528 , 303, 146), // #1090
  INST(Vgatherpf0dps    , VexM_VM            , E(660F38,C6,1,2,_,0,2,T1S), 0                         , 191, 0  , 6542 , 304, 146), // #1091
  INST(Vgatherpf0qpd    , VexM_VM            , E(660F38,C7,1,2,_,1,3,T1S), 0                         , 190, 0  , 6556 , 305, 146), // #1092
  INST(Vgatherpf0qps    , VexM_VM            , E(660F38,C7,1,2,_,0,2,T1S), 0                         , 191, 0  , 6570 , 305, 146), // #1093
  INST(Vgatherpf1dpd    , VexM_VM            , E(660F38,C6,2,2,_,1,3,T1S), 0                         , 192, 0  , 6584 , 303, 146), // #1094
  INST(Vgatherpf1dps    , VexM_VM            , E(660F38,C6,2,2,_,0,2,T1S), 0                         , 193, 0  , 6598 , 304, 146), // #1095
  INST(Vgatherpf1qpd    , VexM_VM            , E(660F38,C7,2,2,_,1,3,T1S), 0                         , 192, 0  , 6612 , 305, 146), // #1096
  INST(Vgatherpf1qps    , VexM_VM            , E(660F38,C7,2,2,_,0,2,T1S), 0                         , 193, 0  , 6626 , 305, 146), // #1097
  INST(Vgatherqpd       , VexRmvRm_VM        , V(660F38,93,_,x,1,_,_,_  ), E(660F38,93,_,x,_,1,3,T1S), 189, 82 , 6640 , 306, 145), // #1098
  INST(Vgatherqps       , VexRmvRm_VM        , V(660F38,93,_,x,0,_,_,_  ), E(660F38,93,_,x,_,0,2,T1S), 96 , 83 , 6651 , 307, 145), // #1099
  INST(Vgetexppd        , VexRm_Lx           , E(660F38,42,_,x,_,1,4,FV ), 0                         , 113, 0  , 6662 , 263, 131), // #1100
  INST(Vgetexpph        , VexRm_Lx           , E(66MAP6,42,_,_,_,0,4,FV ), 0                         , 183, 0  , 6672 , 265, 125), // #1101
  INST(Vgetexpps        , VexRm_Lx           , E(660F38,42,_,x,_,0,4,FV ), 0                         , 114, 0  , 6682 , 268, 131), // #1102
  INST(Vgetexpsd        , VexRvm             , E(660F38,43,_,I,_,1,3,T1S), 0                         , 128, 0  , 6692 , 308, 68 ), // #1103
  INST(Vgetexpsh        , VexRvm             , E(66MAP6,43,_,_,_,0,1,T1S), 0                         , 185, 0  , 6702 , 254, 127), // #1104
  INST(Vgetexpss        , VexRvm             , E(660F38,43,_,I,_,0,2,T1S), 0                         , 129, 0  , 6712 , 309, 68 ), // #1105
  INST(Vgetmantpd       , VexRmi_Lx          , E(660F3A,26,_,x,_,1,4,FV ), 0                         , 112, 0  , 6722 , 310, 131), // #1106
  INST(Vgetmantph       , VexRmi_Lx          , E(000F3A,26,_,_,_,0,4,FV ), 0                         , 123, 0  , 6733 , 311, 125), // #1107
  INST(Vgetmantps       , VexRmi_Lx          , E(660F3A,26,_,x,_,0,4,FV ), 0                         , 111, 0  , 6744 , 312, 131), // #1108
  INST(Vgetmantsd       , VexRvmi            , E(660F3A,27,_,I,_,1,3,T1S), 0                         , 180, 0  , 6755 , 287, 68 ), // #1109
  INST(Vgetmantsh       , VexRvmi            , E(000F3A,27,_,_,_,0,1,T1S), 0                         , 188, 0  , 6766 , 313, 127), // #1110
  INST(Vgetmantss       , VexRvmi            , E(660F3A,27,_,I,_,0,2,T1S), 0                         , 181, 0  , 6777 , 288, 68 ), // #1111
  INST(Vgf2p8affineinvqb, VexRvmi_Lx         , V(660F3A,CF,_,x,1,1,4,FV ), 0                         , 194, 0  , 6788 , 314, 147), // #1112
  INST(Vgf2p8affineqb   , VexRvmi_Lx         , V(660F3A,CE,_,x,1,1,4,FV ), 0                         , 194, 0  , 6806 , 314, 147), // #1113
  INST(Vgf2p8mulb       , VexRvm_Lx          , V(660F38,CF,_,x,0,0,4,FV ), 0                         , 110, 0  , 6821 , 315, 147), // #1114
  INST(Vhaddpd          , VexRvm_Lx          , V(660F00,7C,_,x,I,_,_,_  ), 0                         , 69 , 0  , 6832 , 202, 128), // #1115
  INST(Vhaddps          , VexRvm_Lx          , V(F20F00,7C,_,x,I,_,_,_  ), 0                         , 109, 0  , 6840 , 202, 128), // #1116
  INST(Vhsubpd          , VexRvm_Lx          , V(660F00,7D,_,x,I,_,_,_  ), 0                         , 69 , 0  , 6848 , 202, 128), // #1117
  INST(Vhsubps          , VexRvm_Lx          , V(F20F00,7D,_,x,I,_,_,_  ), 0                         , 109, 0  , 6856 , 202, 128), // #1118
  INST(Vinsertf128      , VexRvmi            , V(660F3A,18,_,1,0,_,_,_  ), 0                         , 172, 0  , 6864 , 316, 128), // #1119
  INST(Vinsertf32x4     , VexRvmi_Lx         , E(660F3A,18,_,x,_,0,4,T4 ), 0                         , 173, 0  , 6876 , 317, 131), // #1120
  INST(Vinsertf32x8     , VexRvmi            , E(660F3A,1A,_,2,_,0,5,T8 ), 0                         , 174, 0  , 6889 , 318, 66 ), // #1121
  INST(Vinsertf64x2     , VexRvmi_Lx         , E(660F3A,18,_,x,_,1,4,T2 ), 0                         , 175, 0  , 6902 , 317, 133), // #1122
  INST(Vinsertf64x4     , VexRvmi            , E(660F3A,1A,_,2,_,1,5,T4 ), 0                         , 176, 0  , 6915 , 318, 68 ), // #1123
  INST(Vinserti128      , VexRvmi            , V(660F3A,38,_,1,0,_,_,_  ), 0                         , 172, 0  , 6928 , 316, 134), // #1124
  INST(Vinserti32x4     , VexRvmi_Lx         , E(660F3A,38,_,x,_,0,4,T4 ), 0                         , 173, 0  , 6940 , 317, 131), // #1125
  INST(Vinserti32x8     , VexRvmi            , E(660F3A,3A,_,2,_,0,5,T8 ), 0                         , 174, 0  , 6953 , 318, 66 ), // #1126
  INST(Vinserti64x2     , VexRvmi_Lx         , E(660F3A,38,_,x,_,1,4,T2 ), 0                         , 175, 0  , 6966 , 317, 133), // #1127
  INST(Vinserti64x4     , VexRvmi            , E(660F3A,3A,_,2,_,1,5,T4 ), 0                         , 176, 0  , 6979 , 318, 68 ), // #1128
  INST(Vinsertps        , VexRvmi            , V(660F3A,21,_,0,I,0,2,T1S), 0                         , 177, 0  , 6992 , 319, 126), // #1129
  INST(Vlddqu           , VexRm_Lx           , V(F20F00,F0,_,x,I,_,_,_  ), 0                         , 109, 0  , 7002 , 320, 128), // #1130
  INST(Vldmxcsr         , VexM               , V(000F00,AE,2,0,I,_,_,_  ), 0                         , 195, 0  , 7009 , 321, 128), // #1131
  INST(Vmaskmovdqu      , VexRm_ZDI          , V(660F00,F7,_,0,I,_,_,_  ), 0                         , 69 , 0  , 7018 , 322, 128), // #1132
  INST(Vmaskmovpd       , VexRvmMvr_Lx       , V(660F38,2D,_,x,0,_,_,_  ), V(660F38,2F,_,x,0,_,_,_  ), 96 , 84 , 7030 , 323, 128), // #1133
  INST(Vmaskmovps       , VexRvmMvr_Lx       , V(660F38,2C,_,x,0,_,_,_  ), V(660F38,2E,_,x,0,_,_,_  ), 96 , 85 , 7041 , 323, 128), // #1134
  INST(Vmaxpd           , VexRvm_Lx          , V(660F00,5F,_,x,I,1,4,FV ), 0                         , 103, 0  , 7052 , 324, 124), // #1135
  INST(Vmaxph           , VexRvm_Lx          , E(00MAP5,5F,_,_,_,0,4,FV ), 0                         , 104, 0  , 7059 , 325, 125), // #1136
  INST(Vmaxps           , VexRvm_Lx          , V(000F00,5F,_,x,I,0,4,FV ), 0                         , 105, 0  , 7066 , 326, 124), // #1137
  INST(Vmaxsd           , VexRvm             , V(F20F00,5F,_,I,I,1,3,T1S), 0                         , 106, 0  , 7073 , 327, 124), // #1138
  INST(Vmaxsh           , VexRvm             , E(F3MAP5,5F,_,_,_,0,1,T1S), 0                         , 107, 0  , 7080 , 254, 127), // #1139
  INST(Vmaxss           , VexRvm             , V(F30F00,5F,_,I,I,0,2,T1S), 0                         , 108, 0  , 7087 , 258, 124), // #1140
  INST(Vmcall           , X86Op              , O(000F01,C1,_,_,_,_,_,_  ), 0                         , 21 , 0  , 7094 , 30 , 58 ), // #1141
  INST(Vmclear          , X86M_Only          , O(660F00,C7,6,_,_,_,_,_  ), 0                         , 26 , 0  , 7101 , 32 , 58 ), // #1142
  INST(Vmfunc           , X86Op              , O(000F01,D4,_,_,_,_,_,_  ), 0                         , 21 , 0  , 7109 , 30 , 58 ), // #1143
  INST(Vminpd           , VexRvm_Lx          , V(660F00,5D,_,x,I,1,4,FV ), 0                         , 103, 0  , 7116 , 324, 124), // #1144
  INST(Vminph           , VexRvm_Lx          , E(00MAP5,5D,_,_,_,0,4,FV ), 0                         , 104, 0  , 7123 , 325, 125), // #1145
  INST(Vminps           , VexRvm_Lx          , V(000F00,5D,_,x,I,0,4,FV ), 0                         , 105, 0  , 7130 , 326, 124), // #1146
  INST(Vminsd           , VexRvm             , V(F20F00,5D,_,I,I,1,3,T1S), 0                         , 106, 0  , 7137 , 327, 124), // #1147
  INST(Vminsh           , VexRvm             , E(F3MAP5,5D,_,_,_,0,1,T1S), 0                         , 107, 0  , 7144 , 254, 127), // #1148
  INST(Vminss           , VexRvm             , V(F30F00,5D,_,I,I,0,2,T1S), 0                         , 108, 0  , 7151 , 258, 124), // #1149
  INST(Vmlaunch         , X86Op              , O(000F01,C2,_,_,_,_,_,_  ), 0                         , 21 , 0  , 7158 , 30 , 58 ), // #1150
  INST(Vmload           , X86Op_xAX          , O(000F01,DA,_,_,_,_,_,_  ), 0                         , 21 , 0  , 7167 , 328, 22 ), // #1151
  INST(Vmmcall          , X86Op              , O(000F01,D9,_,_,_,_,_,_  ), 0                         , 21 , 0  , 7174 , 30 , 22 ), // #1152
  INST(Vmovapd          , VexRmMr_Lx         , V(660F00,28,_,x,I,1,4,FVM), V(660F00,29,_,x,I,1,4,FVM), 103, 86 , 7182 , 329, 124), // #1153
  INST(Vmovaps          , VexRmMr_Lx         , V(000F00,28,_,x,I,0,4,FVM), V(000F00,29,_,x,I,0,4,FVM), 105, 87 , 7190 , 329, 124), // #1154
  INST(Vmovd            , VexMovdMovq        , V(660F00,6E,_,0,0,0,2,T1S), V(660F00,7E,_,0,0,0,2,T1S), 196, 88 , 7198 , 330, 126), // #1155
  INST(Vmovddup         , VexRm_Lx           , V(F20F00,12,_,x,I,1,3,DUP), 0                         , 197, 0  , 7204 , 331, 124), // #1156
  INST(Vmovdqa          , VexRmMr_Lx         , V(660F00,6F,_,x,I,_,_,_  ), V(660F00,7F,_,x,I,_,_,_  ), 69 , 89 , 7213 , 332, 128), // #1157
  INST(Vmovdqa32        , VexRmMr_Lx         , E(660F00,6F,_,x,_,0,4,FVM), E(660F00,7F,_,x,_,0,4,FVM), 198, 90 , 7221 , 333, 131), // #1158
  INST(Vmovdqa64        , VexRmMr_Lx         , E(660F00,6F,_,x,_,1,4,FVM), E(660F00,7F,_,x,_,1,4,FVM), 135, 91 , 7231 , 333, 131), // #1159
  INST(Vmovdqu          , VexRmMr_Lx         , V(F30F00,6F,_,x,I,_,_,_  ), V(F30F00,7F,_,x,I,_,_,_  ), 199, 92 , 7241 , 332, 128), // #1160
  INST(Vmovdqu16        , VexRmMr_Lx         , E(F20F00,6F,_,x,_,1,4,FVM), E(F20F00,7F,_,x,_,1,4,FVM), 166, 93 , 7249 , 333, 139), // #1161
  INST(Vmovdqu32        , VexRmMr_Lx         , E(F30F00,6F,_,x,_,0,4,FVM), E(F30F00,7F,_,x,_,0,4,FVM), 200, 94 , 7259 , 333, 131), // #1162
  INST(Vmovdqu64        , VexRmMr_Lx         , E(F30F00,6F,_,x,_,1,4,FVM), E(F30F00,7F,_,x,_,1,4,FVM), 149, 95 , 7269 , 333, 131), // #1163
  INST(Vmovdqu8         , VexRmMr_Lx         , E(F20F00,6F,_,x,_,0,4,FVM), E(F20F00,7F,_,x,_,0,4,FVM), 164, 96 , 7279 , 333, 139), // #1164
  INST(Vmovhlps         , VexRvm             , V(000F00,12,_,0,I,0,_,_  ), 0                         , 72 , 0  , 7288 , 334, 126), // #1165
  INST(Vmovhpd          , VexRvmMr           , V(660F00,16,_,0,I,1,3,T1S), V(660F00,17,_,0,I,1,3,T1S), 125, 97 , 7297 , 335, 126), // #1166
  INST(Vmovhps          , VexRvmMr           , V(000F00,16,_,0,I,0,3,T2 ), V(000F00,17,_,0,I,0,3,T2 ), 201, 98 , 7305 , 335, 126), // #1167
  INST(Vmovlhps         , VexRvm             , V(000F00,16,_,0,I,0,_,_  ), 0                         , 72 , 0  , 7313 , 334, 126), // #1168
  INST(Vmovlpd          , VexRvmMr           , V(660F00,12,_,0,I,1,3,T1S), V(660F00,13,_,0,I,1,3,T1S), 125, 99 , 7322 , 335, 126), // #1169
  INST(Vmovlps          , VexRvmMr           , V(000F00,12,_,0,I,0,3,T2 ), V(000F00,13,_,0,I,0,3,T2 ), 201, 100, 7330 , 335, 126), // #1170
  INST(Vmovmskpd        , VexRm_Lx           , V(660F00,50,_,x,I,_,_,_  ), 0                         , 69 , 0  , 7338 , 336, 128), // #1171
  INST(Vmovmskps        , VexRm_Lx           , V(000F00,50,_,x,I,_,_,_  ), 0                         , 72 , 0  , 7348 , 336, 128), // #1172
  INST(Vmovntdq         , VexMr_Lx           , V(660F00,E7,_,x,I,0,4,FVM), 0                         , 144, 0  , 7358 , 337, 124), // #1173
  INST(Vmovntdqa        , VexRm_Lx           , V(660F38,2A,_,x,I,0,4,FVM), 0                         , 110, 0  , 7367 , 338, 135), // #1174
  INST(Vmovntpd         , VexMr_Lx           , V(660F00,2B,_,x,I,1,4,FVM), 0                         , 103, 0  , 7377 , 337, 124), // #1175
  INST(Vmovntps         , VexMr_Lx           , V(000F00,2B,_,x,I,0,4,FVM), 0                         , 105, 0  , 7386 , 337, 124), // #1176
  INST(Vmovq            , VexMovdMovq        , V(660F00,6E,_,0,I,1,3,T1S), V(660F00,7E,_,0,I,1,3,T1S), 125, 101, 7395 , 339, 126), // #1177
  INST(Vmovsd           , VexMovssMovsd      , V(F20F00,10,_,I,I,1,3,T1S), V(F20F00,11,_,I,I,1,3,T1S), 106, 102, 7401 , 340, 126), // #1178
  INST(Vmovsh           , VexMovssMovsd      , E(F3MAP5,10,_,I,_,0,1,T1S), E(F3MAP5,11,_,I,_,0,1,T1S), 107, 103, 7408 , 341, 127), // #1179
  INST(Vmovshdup        , VexRm_Lx           , V(F30F00,16,_,x,I,0,4,FVM), 0                         , 161, 0  , 7415 , 342, 124), // #1180
  INST(Vmovsldup        , VexRm_Lx           , V(F30F00,12,_,x,I,0,4,FVM), 0                         , 161, 0  , 7425 , 342, 124), // #1181
  INST(Vmovss           , VexMovssMovsd      , V(F30F00,10,_,I,I,0,2,T1S), V(F30F00,11,_,I,I,0,2,T1S), 108, 104, 7435 , 343, 126), // #1182
  INST(Vmovupd          , VexRmMr_Lx         , V(660F00,10,_,x,I,1,4,FVM), V(660F00,11,_,x,I,1,4,FVM), 103, 105, 7442 , 329, 124), // #1183
  INST(Vmovups          , VexRmMr_Lx         , V(000F00,10,_,x,I,0,4,FVM), V(000F00,11,_,x,I,0,4,FVM), 105, 106, 7450 , 329, 124), // #1184
  INST(Vmovw            , VexMovdMovq        , E(66MAP5,6E,_,0,_,I,1,T1S), E(66MAP5,7E,_,0,_,I,1,T1S), 202, 107, 7458 , 344, 127), // #1185
  INST(Vmpsadbw         , VexRvmi_Lx         , V(660F3A,42,_,x,I,_,_,_  ), 0                         , 73 , 0  , 7464 , 214, 148), // #1186
  INST(Vmptrld          , X86M_Only          , O(000F00,C7,6,_,_,_,_,_  ), 0                         , 80 , 0  , 7473 , 32 , 58 ), // #1187
  INST(Vmptrst          , X86M_Only          , O(000F00,C7,7,_,_,_,_,_  ), 0                         , 22 , 0  , 7481 , 32 , 58 ), // #1188
  INST(Vmread           , X86Mr_NoSize       , O(000F00,78,_,_,_,_,_,_  ), 0                         , 4  , 0  , 7489 , 345, 58 ), // #1189
  INST(Vmresume         , X86Op              , O(000F01,C3,_,_,_,_,_,_  ), 0                         , 21 , 0  , 7496 , 30 , 58 ), // #1190
  INST(Vmrun            , X86Op_xAX          , O(000F01,D8,_,_,_,_,_,_  ), 0                         , 21 , 0  , 7505 , 328, 22 ), // #1191
  INST(Vmsave           , X86Op_xAX          , O(000F01,DB,_,_,_,_,_,_  ), 0                         , 21 , 0  , 7511 , 328, 22 ), // #1192
  INST(Vmulpd           , VexRvm_Lx          , V(660F00,59,_,x,I,1,4,FV ), 0                         , 103, 0  , 7518 , 196, 124), // #1193
  INST(Vmulph           , VexRvm_Lx          , E(00MAP5,59,_,_,_,0,4,FV ), 0                         , 104, 0  , 7525 , 197, 125), // #1194
  INST(Vmulps           , VexRvm_Lx          , V(000F00,59,_,x,I,0,4,FV ), 0                         , 105, 0  , 7532 , 198, 124), // #1195
  INST(Vmulsd           , VexRvm             , V(F20F00,59,_,I,I,1,3,T1S), 0                         , 106, 0  , 7539 , 199, 126), // #1196
  INST(Vmulsh           , VexRvm             , E(F3MAP5,59,_,_,_,0,1,T1S), 0                         , 107, 0  , 7546 , 200, 127), // #1197
  INST(Vmulss           , VexRvm             , V(F30F00,59,_,I,I,0,2,T1S), 0                         , 108, 0  , 7553 , 201, 126), // #1198
  INST(Vmwrite          , X86Rm_NoSize       , O(000F00,79,_,_,_,_,_,_  ), 0                         , 4  , 0  , 7560 , 346, 58 ), // #1199
  INST(Vmxon            , X86M_Only          , O(F30F00,C7,6,_,_,_,_,_  ), 0                         , 24 , 0  , 7568 , 32 , 58 ), // #1200
  INST(Vorpd            , VexRvm_Lx          , V(660F00,56,_,x,I,1,4,FV ), 0                         , 103, 0  , 7574 , 210, 132), // #1201
  INST(Vorps            , VexRvm_Lx          , V(000F00,56,_,x,I,0,4,FV ), 0                         , 105, 0  , 7580 , 211, 132), // #1202
  INST(Vp2intersectd    , VexRvm_Lx_2xK      , E(F20F38,68,_,_,_,0,4,FV ), 0                         , 131, 0  , 7586 , 347, 149), // #1203
  INST(Vp2intersectq    , VexRvm_Lx_2xK      , E(F20F38,68,_,_,_,1,4,FV ), 0                         , 203, 0  , 7600 , 348, 149), // #1204
  INST(Vp4dpwssd        , VexRm_T1_4X        , E(F20F38,52,_,2,_,0,4,T4X), 0                         , 101, 0  , 7614 , 194, 150), // #1205
  INST(Vp4dpwssds       , VexRm_T1_4X        , E(F20F38,53,_,2,_,0,4,T4X), 0                         , 101, 0  , 7624 , 194, 150), // #1206
  INST(Vpabsb           , VexRm_Lx           , V(660F38,1C,_,x,I,_,4,FVM), 0                         , 110, 0  , 7635 , 342, 151), // #1207
  INST(Vpabsd           , VexRm_Lx           , V(660F38,1E,_,x,I,0,4,FV ), 0                         , 110, 0  , 7642 , 349, 135), // #1208
  INST(Vpabsq           , VexRm_Lx           , E(660F38,1F,_,x,_,1,4,FV ), 0                         , 113, 0  , 7649 , 350, 131), // #1209
  INST(Vpabsw           , VexRm_Lx           , V(660F38,1D,_,x,I,_,4,FVM), 0                         , 110, 0  , 7656 , 342, 151), // #1210
  INST(Vpackssdw        , VexRvm_Lx          , V(660F00,6B,_,x,I,0,4,FV ), 0                         , 144, 0  , 7663 , 209, 151), // #1211
  INST(Vpacksswb        , VexRvm_Lx          , V(660F00,63,_,x,I,I,4,FVM), 0                         , 144, 0  , 7673 , 315, 151), // #1212
  INST(Vpackusdw        , VexRvm_Lx          , V(660F38,2B,_,x,I,0,4,FV ), 0                         , 110, 0  , 7683 , 209, 151), // #1213
  INST(Vpackuswb        , VexRvm_Lx          , V(660F00,67,_,x,I,I,4,FVM), 0                         , 144, 0  , 7693 , 315, 151), // #1214
  INST(Vpaddb           , VexRvm_Lx          , V(660F00,FC,_,x,I,I,4,FVM), 0                         , 144, 0  , 7703 , 315, 151), // #1215
  INST(Vpaddd           , VexRvm_Lx          , V(660F00,FE,_,x,I,0,4,FV ), 0                         , 144, 0  , 7710 , 209, 135), // #1216
  INST(Vpaddq           , VexRvm_Lx          , V(660F00,D4,_,x,I,1,4,FV ), 0                         , 103, 0  , 7717 , 208, 135), // #1217
  INST(Vpaddsb          , VexRvm_Lx          , V(660F00,EC,_,x,I,I,4,FVM), 0                         , 144, 0  , 7724 , 315, 151), // #1218
  INST(Vpaddsw          , VexRvm_Lx          , V(660F00,ED,_,x,I,I,4,FVM), 0                         , 144, 0  , 7732 , 315, 151), // #1219
  INST(Vpaddusb         , VexRvm_Lx          , V(660F00,DC,_,x,I,I,4,FVM), 0                         , 144, 0  , 7740 , 315, 151), // #1220
  INST(Vpaddusw         , VexRvm_Lx          , V(660F00,DD,_,x,I,I,4,FVM), 0                         , 144, 0  , 7749 , 315, 151), // #1221
  INST(Vpaddw           , VexRvm_Lx          , V(660F00,FD,_,x,I,I,4,FVM), 0                         , 144, 0  , 7758 , 315, 151), // #1222
  INST(Vpalignr         , VexRvmi_Lx         , V(660F3A,0F,_,x,I,I,4,FVM), 0                         , 204, 0  , 7765 , 314, 151), // #1223
  INST(Vpand            , VexRvm_Lx          , V(660F00,DB,_,x,I,_,_,_  ), 0                         , 69 , 0  , 7774 , 351, 148), // #1224
  INST(Vpandd           , VexRvm_Lx          , E(660F00,DB,_,x,_,0,4,FV ), 0                         , 198, 0  , 7780 , 352, 131), // #1225
  INST(Vpandn           , VexRvm_Lx          , V(660F00,DF,_,x,I,_,_,_  ), 0                         , 69 , 0  , 7787 , 353, 148), // #1226
  INST(Vpandnd          , VexRvm_Lx          , E(660F00,DF,_,x,_,0,4,FV ), 0                         , 198, 0  , 7794 , 354, 131), // #1227
  INST(Vpandnq          , VexRvm_Lx          , E(660F00,DF,_,x,_,1,4,FV ), 0                         , 135, 0  , 7802 , 355, 131), // #1228
  INST(Vpandq           , VexRvm_Lx          , E(660F00,DB,_,x,_,1,4,FV ), 0                         , 135, 0  , 7810 , 356, 131), // #1229
  INST(Vpavgb           , VexRvm_Lx          , V(660F00,E0,_,x,I,I,4,FVM), 0                         , 144, 0  , 7817 , 315, 151), // #1230
  INST(Vpavgw           , VexRvm_Lx          , V(660F00,E3,_,x,I,I,4,FVM), 0                         , 144, 0  , 7824 , 315, 151), // #1231
  INST(Vpblendd         , VexRvmi_Lx         , V(660F3A,02,_,x,0,_,_,_  ), 0                         , 73 , 0  , 7831 , 214, 134), // #1232
  INST(Vpblendmb        , VexRvm_Lx          , E(660F38,66,_,x,_,0,4,FVM), 0                         , 114, 0  , 7840 , 357, 139), // #1233
  INST(Vpblendmd        , VexRvm_Lx          , E(660F38,64,_,x,_,0,4,FV ), 0                         , 114, 0  , 7850 , 213, 131), // #1234
  INST(Vpblendmq        , VexRvm_Lx          , E(660F38,64,_,x,_,1,4,FV ), 0                         , 113, 0  , 7860 , 212, 131), // #1235
  INST(Vpblendmw        , VexRvm_Lx          , E(660F38,66,_,x,_,1,4,FVM), 0                         , 113, 0  , 7870 , 357, 139), // #1236
  INST(Vpblendvb        , VexRvmr_Lx         , V(660F3A,4C,_,x,0,_,_,_  ), 0                         , 73 , 0  , 7880 , 215, 148), // #1237
  INST(Vpblendw         , VexRvmi_Lx         , V(660F3A,0E,_,x,I,_,_,_  ), 0                         , 73 , 0  , 7890 , 214, 148), // #1238
  INST(Vpbroadcastb     , VexRm_Lx_Bcst      , V(660F38,78,_,x,0,0,0,T1S), E(660F38,7A,_,x,0,0,0,T1S), 96 , 108, 7899 , 358, 152), // #1239
  INST(Vpbroadcastd     , VexRm_Lx_Bcst      , V(660F38,58,_,x,0,0,2,T1S), E(660F38,7C,_,x,0,0,0,T1S), 122, 109, 7912 , 359, 145), // #1240
  INST(Vpbroadcastmb2q  , VexRm_Lx           , E(F30F38,2A,_,x,_,1,_,_  ), 0                         , 205, 0  , 7925 , 360, 153), // #1241
  INST(Vpbroadcastmw2d  , VexRm_Lx           , E(F30F38,3A,_,x,_,0,_,_  ), 0                         , 206, 0  , 7941 , 360, 153), // #1242
  INST(Vpbroadcastq     , VexRm_Lx_Bcst      , V(660F38,59,_,x,0,1,3,T1S), E(660F38,7C,_,x,0,1,0,T1S), 121, 110, 7957 , 361, 145), // #1243
  INST(Vpbroadcastw     , VexRm_Lx_Bcst      , V(660F38,79,_,x,0,0,1,T1S), E(660F38,7B,_,x,0,0,0,T1S), 207, 111, 7970 , 362, 152), // #1244
  INST(Vpclmulqdq       , VexRvmi_Lx         , V(660F3A,44,_,x,I,_,4,FVM), 0                         , 204, 0  , 7983 , 363, 154), // #1245
  INST(Vpcmov           , VexRvrmRvmr_Lx     , V(XOP_M8,A2,_,x,x,_,_,_  ), 0                         , 208, 0  , 7994 , 289, 144), // #1246
  INST(Vpcmpb           , VexRvmi_Lx         , E(660F3A,3F,_,x,_,0,4,FVM), 0                         , 111, 0  , 8001 , 364, 139), // #1247
  INST(Vpcmpd           , VexRvmi_Lx         , E(660F3A,1F,_,x,_,0,4,FV ), 0                         , 111, 0  , 8008 , 365, 131), // #1248
  INST(Vpcmpeqb         , VexRvm_Lx_KEvex    , V(660F00,74,_,x,I,I,4,FV ), 0                         , 144, 0  , 8015 , 366, 151), // #1249
  INST(Vpcmpeqd         , VexRvm_Lx_KEvex    , V(660F00,76,_,x,I,0,4,FVM), 0                         , 144, 0  , 8024 , 367, 135), // #1250
  INST(Vpcmpeqq         , VexRvm_Lx_KEvex    , V(660F38,29,_,x,I,1,4,FVM), 0                         , 209, 0  , 8033 , 368, 135), // #1251
  INST(Vpcmpeqw         , VexRvm_Lx_KEvex    , V(660F00,75,_,x,I,I,4,FV ), 0                         , 144, 0  , 8042 , 366, 151), // #1252
  INST(Vpcmpestri       , VexRmi             , V(660F3A,61,_,0,I,_,_,_  ), 0                         , 73 , 0  , 8051 , 369, 155), // #1253
  INST(Vpcmpestrm       , VexRmi             , V(660F3A,60,_,0,I,_,_,_  ), 0                         , 73 , 0  , 8062 , 370, 155), // #1254
  INST(Vpcmpgtb         , VexRvm_Lx_KEvex    , V(660F00,64,_,x,I,I,4,FV ), 0                         , 144, 0  , 8073 , 366, 151), // #1255
  INST(Vpcmpgtd         , VexRvm_Lx_KEvex    , V(660F00,66,_,x,I,0,4,FVM), 0                         , 144, 0  , 8082 , 367, 135), // #1256
  INST(Vpcmpgtq         , VexRvm_Lx_KEvex    , V(660F38,37,_,x,I,1,4,FVM), 0                         , 209, 0  , 8091 , 368, 135), // #1257
  INST(Vpcmpgtw         , VexRvm_Lx_KEvex    , V(660F00,65,_,x,I,I,4,FV ), 0                         , 144, 0  , 8100 , 366, 151), // #1258
  INST(Vpcmpistri       , VexRmi             , V(660F3A,63,_,0,I,_,_,_  ), 0                         , 73 , 0  , 8109 , 371, 155), // #1259
  INST(Vpcmpistrm       , VexRmi             , V(660F3A,62,_,0,I,_,_,_  ), 0                         , 73 , 0  , 8120 , 372, 155), // #1260
  INST(Vpcmpq           , VexRvmi_Lx         , E(660F3A,1F,_,x,_,1,4,FV ), 0                         , 112, 0  , 8131 , 373, 131), // #1261
  INST(Vpcmpub          , VexRvmi_Lx         , E(660F3A,3E,_,x,_,0,4,FVM), 0                         , 111, 0  , 8138 , 364, 139), // #1262
  INST(Vpcmpud          , VexRvmi_Lx         , E(660F3A,1E,_,x,_,0,4,FV ), 0                         , 111, 0  , 8146 , 365, 131), // #1263
  INST(Vpcmpuq          , VexRvmi_Lx         , E(660F3A,1E,_,x,_,1,4,FV ), 0                         , 112, 0  , 8154 , 373, 131), // #1264
  INST(Vpcmpuw          , VexRvmi_Lx         , E(660F3A,3E,_,x,_,1,4,FVM), 0                         , 112, 0  , 8162 , 373, 139), // #1265
  INST(Vpcmpw           , VexRvmi_Lx         , E(660F3A,3F,_,x,_,1,4,FVM), 0                         , 112, 0  , 8170 , 373, 139), // #1266
  INST(Vpcomb           , VexRvmi            , V(XOP_M8,CC,_,0,0,_,_,_  ), 0                         , 208, 0  , 8177 , 276, 144), // #1267
  INST(Vpcomd           , VexRvmi            , V(XOP_M8,CE,_,0,0,_,_,_  ), 0                         , 208, 0  , 8184 , 276, 144), // #1268
  INST(Vpcompressb      , VexMr_Lx           , E(660F38,63,_,x,_,0,0,T1S), 0                         , 210, 0  , 8191 , 232, 156), // #1269
  INST(Vpcompressd      , VexMr_Lx           , E(660F38,8B,_,x,_,0,2,T1S), 0                         , 129, 0  , 8203 , 232, 131), // #1270
  INST(Vpcompressq      , VexMr_Lx           , E(660F38,8B,_,x,_,1,3,T1S), 0                         , 128, 0  , 8215 , 232, 131), // #1271
  INST(Vpcompressw      , VexMr_Lx           , E(660F38,63,_,x,_,1,1,T1S), 0                         , 211, 0  , 8227 , 232, 156), // #1272
  INST(Vpcomq           , VexRvmi            , V(XOP_M8,CF,_,0,0,_,_,_  ), 0                         , 208, 0  , 8239 , 276, 144), // #1273
  INST(Vpcomub          , VexRvmi            , V(XOP_M8,EC,_,0,0,_,_,_  ), 0                         , 208, 0  , 8246 , 276, 144), // #1274
  INST(Vpcomud          , VexRvmi            , V(XOP_M8,EE,_,0,0,_,_,_  ), 0                         , 208, 0  , 8254 , 276, 144), // #1275
  INST(Vpcomuq          , VexRvmi            , V(XOP_M8,EF,_,0,0,_,_,_  ), 0                         , 208, 0  , 8262 , 276, 144), // #1276
  INST(Vpcomuw          , VexRvmi            , V(XOP_M8,ED,_,0,0,_,_,_  ), 0                         , 208, 0  , 8270 , 276, 144), // #1277
  INST(Vpcomw           , VexRvmi            , V(XOP_M8,CD,_,0,0,_,_,_  ), 0                         , 208, 0  , 8278 , 276, 144), // #1278
  INST(Vpconflictd      , VexRm_Lx           , E(660F38,C4,_,x,_,0,4,FV ), 0                         , 114, 0  , 8285 , 374, 153), // #1279
  INST(Vpconflictq      , VexRm_Lx           , E(660F38,C4,_,x,_,1,4,FV ), 0                         , 113, 0  , 8297 , 374, 153), // #1280
  INST(Vpdpbusd         , VexRvm_Lx          , V(660F38,50,_,x,_,0,4,FV ), 0                         , 110, 0  , 8309 , 375, 157), // #1281
  INST(Vpdpbusds        , VexRvm_Lx          , V(660F38,51,_,x,_,0,4,FV ), 0                         , 110, 0  , 8318 , 375, 157), // #1282
  INST(Vpdpwssd         , VexRvm_Lx          , V(660F38,52,_,x,_,0,4,FV ), 0                         , 110, 0  , 8328 , 375, 157), // #1283
  INST(Vpdpwssds        , VexRvm_Lx          , V(660F38,53,_,x,_,0,4,FV ), 0                         , 110, 0  , 8337 , 375, 157), // #1284
  INST(Vperm2f128       , VexRvmi            , V(660F3A,06,_,1,0,_,_,_  ), 0                         , 172, 0  , 8347 , 376, 128), // #1285
  INST(Vperm2i128       , VexRvmi            , V(660F3A,46,_,1,0,_,_,_  ), 0                         , 172, 0  , 8358 , 376, 134), // #1286
  INST(Vpermb           , VexRvm_Lx          , E(660F38,8D,_,x,_,0,4,FVM), 0                         , 114, 0  , 8369 , 357, 158), // #1287
  INST(Vpermd           , VexRvm_Lx          , V(660F38,36,_,x,0,0,4,FV ), 0                         , 110, 0  , 8376 , 377, 145), // #1288
  INST(Vpermi2b         , VexRvm_Lx          , E(660F38,75,_,x,_,0,4,FVM), 0                         , 114, 0  , 8383 , 357, 158), // #1289
  INST(Vpermi2d         , VexRvm_Lx          , E(660F38,76,_,x,_,0,4,FV ), 0                         , 114, 0  , 8392 , 213, 131), // #1290
  INST(Vpermi2pd        , VexRvm_Lx          , E(660F38,77,_,x,_,1,4,FV ), 0                         , 113, 0  , 8401 , 212, 131), // #1291
  INST(Vpermi2ps        , VexRvm_Lx          , E(660F38,77,_,x,_,0,4,FV ), 0                         , 114, 0  , 8411 , 213, 131), // #1292
  INST(Vpermi2q         , VexRvm_Lx          , E(660F38,76,_,x,_,1,4,FV ), 0                         , 113, 0  , 8421 , 212, 131), // #1293
  INST(Vpermi2w         , VexRvm_Lx          , E(660F38,75,_,x,_,1,4,FVM), 0                         , 113, 0  , 8430 , 357, 139), // #1294
  INST(Vpermil2pd       , VexRvrmiRvmri_Lx   , V(660F3A,49,_,x,x,_,_,_  ), 0                         , 73 , 0  , 8439 , 378, 144), // #1295
  INST(Vpermil2ps       , VexRvrmiRvmri_Lx   , V(660F3A,48,_,x,x,_,_,_  ), 0                         , 73 , 0  , 8450 , 378, 144), // #1296
  INST(Vpermilpd        , VexRvmRmi_Lx       , V(660F38,0D,_,x,0,1,4,FV ), V(660F3A,05,_,x,0,1,4,FV ), 209, 112, 8461 , 379, 124), // #1297
  INST(Vpermilps        , VexRvmRmi_Lx       , V(660F38,0C,_,x,0,0,4,FV ), V(660F3A,04,_,x,0,0,4,FV ), 110, 113, 8471 , 380, 124), // #1298
  INST(Vpermpd          , VexRvmRmi_Lx       , E(660F38,16,_,x,1,1,4,FV ), V(660F3A,01,_,x,1,1,4,FV ), 212, 114, 8481 , 381, 145), // #1299
  INST(Vpermps          , VexRvm_Lx          , V(660F38,16,_,x,0,0,4,FV ), 0                         , 110, 0  , 8489 , 377, 145), // #1300
  INST(Vpermq           , VexRvmRmi_Lx       , E(660F38,36,_,x,_,1,4,FV ), V(660F3A,00,_,x,1,1,4,FV ), 113, 115, 8497 , 381, 145), // #1301
  INST(Vpermt2b         , VexRvm_Lx          , E(660F38,7D,_,x,_,0,4,FVM), 0                         , 114, 0  , 8504 , 357, 158), // #1302
  INST(Vpermt2d         , VexRvm_Lx          , E(660F38,7E,_,x,_,0,4,FV ), 0                         , 114, 0  , 8513 , 213, 131), // #1303
  INST(Vpermt2pd        , VexRvm_Lx          , E(660F38,7F,_,x,_,1,4,FV ), 0                         , 113, 0  , 8522 , 212, 131), // #1304
  INST(Vpermt2ps        , VexRvm_Lx          , E(660F38,7F,_,x,_,0,4,FV ), 0                         , 114, 0  , 8532 , 213, 131), // #1305
  INST(Vpermt2q         , VexRvm_Lx          , E(660F38,7E,_,x,_,1,4,FV ), 0                         , 113, 0  , 8542 , 212, 131), // #1306
  INST(Vpermt2w         , VexRvm_Lx          , E(660F38,7D,_,x,_,1,4,FVM), 0                         , 113, 0  , 8551 , 357, 139), // #1307
  INST(Vpermw           , VexRvm_Lx          , E(660F38,8D,_,x,_,1,4,FVM), 0                         , 113, 0  , 8560 , 357, 139), // #1308
  INST(Vpexpandb        , VexRm_Lx           , E(660F38,62,_,x,_,0,0,T1S), 0                         , 210, 0  , 8567 , 279, 156), // #1309
  INST(Vpexpandd        , VexRm_Lx           , E(660F38,89,_,x,_,0,2,T1S), 0                         , 129, 0  , 8577 , 279, 131), // #1310
  INST(Vpexpandq        , VexRm_Lx           , E(660F38,89,_,x,_,1,3,T1S), 0                         , 128, 0  , 8587 , 279, 131), // #1311
  INST(Vpexpandw        , VexRm_Lx           , E(660F38,62,_,x,_,1,1,T1S), 0                         , 211, 0  , 8597 , 279, 156), // #1312
  INST(Vpextrb          , VexMri             , V(660F3A,14,_,0,0,I,0,T1S), 0                         , 73 , 0  , 8607 , 382, 159), // #1313
  INST(Vpextrd          , VexMri             , V(660F3A,16,_,0,0,0,2,T1S), 0                         , 177, 0  , 8615 , 283, 160), // #1314
  INST(Vpextrq          , VexMri             , V(660F3A,16,_,0,1,1,3,T1S), 0                         , 213, 0  , 8623 , 383, 160), // #1315
  INST(Vpextrw          , VexMri_Vpextrw     , V(660F3A,15,_,0,0,I,1,T1S), 0                         , 214, 0  , 8631 , 384, 159), // #1316
  INST(Vpgatherdd       , VexRmvRm_VM        , V(660F38,90,_,x,0,_,_,_  ), E(660F38,90,_,x,_,0,2,T1S), 96 , 116, 8639 , 302, 145), // #1317
  INST(Vpgatherdq       , VexRmvRm_VM        , V(660F38,90,_,x,1,_,_,_  ), E(660F38,90,_,x,_,1,3,T1S), 189, 117, 8650 , 301, 145), // #1318
  INST(Vpgatherqd       , VexRmvRm_VM        , V(660F38,91,_,x,0,_,_,_  ), E(660F38,91,_,x,_,0,2,T1S), 96 , 118, 8661 , 307, 145), // #1319
  INST(Vpgatherqq       , VexRmvRm_VM        , V(660F38,91,_,x,1,_,_,_  ), E(660F38,91,_,x,_,1,3,T1S), 189, 119, 8672 , 306, 145), // #1320
  INST(Vphaddbd         , VexRm              , V(XOP_M9,C2,_,0,0,_,_,_  ), 0                         , 79 , 0  , 8683 , 204, 144), // #1321
  INST(Vphaddbq         , VexRm              , V(XOP_M9,C3,_,0,0,_,_,_  ), 0                         , 79 , 0  , 8692 , 204, 144), // #1322
  INST(Vphaddbw         , VexRm              , V(XOP_M9,C1,_,0,0,_,_,_  ), 0                         , 79 , 0  , 8701 , 204, 144), // #1323
  INST(Vphaddd          , VexRvm_Lx          , V(660F38,02,_,x,I,_,_,_  ), 0                         , 96 , 0  , 8710 , 202, 148), // #1324
  INST(Vphadddq         , VexRm              , V(XOP_M9,CB,_,0,0,_,_,_  ), 0                         , 79 , 0  , 8718 , 204, 144), // #1325
  INST(Vphaddsw         , VexRvm_Lx          , V(660F38,03,_,x,I,_,_,_  ), 0                         , 96 , 0  , 8727 , 202, 148), // #1326
  INST(Vphaddubd        , VexRm              , V(XOP_M9,D2,_,0,0,_,_,_  ), 0                         , 79 , 0  , 8736 , 204, 144), // #1327
  INST(Vphaddubq        , VexRm              , V(XOP_M9,D3,_,0,0,_,_,_  ), 0                         , 79 , 0  , 8746 , 204, 144), // #1328
  INST(Vphaddubw        , VexRm              , V(XOP_M9,D1,_,0,0,_,_,_  ), 0                         , 79 , 0  , 8756 , 204, 144), // #1329
  INST(Vphaddudq        , VexRm              , V(XOP_M9,DB,_,0,0,_,_,_  ), 0                         , 79 , 0  , 8766 , 204, 144), // #1330
  INST(Vphadduwd        , VexRm              , V(XOP_M9,D6,_,0,0,_,_,_  ), 0                         , 79 , 0  , 8776 , 204, 144), // #1331
  INST(Vphadduwq        , VexRm              , V(XOP_M9,D7,_,0,0,_,_,_  ), 0                         , 79 , 0  , 8786 , 204, 144), // #1332
  INST(Vphaddw          , VexRvm_Lx          , V(660F38,01,_,x,I,_,_,_  ), 0                         , 96 , 0  , 8796 , 202, 148), // #1333
  INST(Vphaddwd         , VexRm              , V(XOP_M9,C6,_,0,0,_,_,_  ), 0                         , 79 , 0  , 8804 , 204, 144), // #1334
  INST(Vphaddwq         , VexRm              , V(XOP_M9,C7,_,0,0,_,_,_  ), 0                         , 79 , 0  , 8813 , 204, 144), // #1335
  INST(Vphminposuw      , VexRm              , V(660F38,41,_,0,I,_,_,_  ), 0                         , 96 , 0  , 8822 , 204, 128), // #1336
  INST(Vphsubbw         , VexRm              , V(XOP_M9,E1,_,0,0,_,_,_  ), 0                         , 79 , 0  , 8834 , 204, 144), // #1337
  INST(Vphsubd          , VexRvm_Lx          , V(660F38,06,_,x,I,_,_,_  ), 0                         , 96 , 0  , 8843 , 202, 148), // #1338
  INST(Vphsubdq         , VexRm              , V(XOP_M9,E3,_,0,0,_,_,_  ), 0                         , 79 , 0  , 8851 , 204, 144), // #1339
  INST(Vphsubsw         , VexRvm_Lx          , V(660F38,07,_,x,I,_,_,_  ), 0                         , 96 , 0  , 8860 , 202, 148), // #1340
  INST(Vphsubw          , VexRvm_Lx          , V(660F38,05,_,x,I,_,_,_  ), 0                         , 96 , 0  , 8869 , 202, 148), // #1341
  INST(Vphsubwd         , VexRm              , V(XOP_M9,E2,_,0,0,_,_,_  ), 0                         , 79 , 0  , 8877 , 204, 144), // #1342
  INST(Vpinsrb          , VexRvmi            , V(660F3A,20,_,0,0,I,0,T1S), 0                         , 73 , 0  , 8886 , 385, 159), // #1343
  INST(Vpinsrd          , VexRvmi            , V(660F3A,22,_,0,0,0,2,T1S), 0                         , 177, 0  , 8894 , 386, 160), // #1344
  INST(Vpinsrq          , VexRvmi            , V(660F3A,22,_,0,1,1,3,T1S), 0                         , 213, 0  , 8902 , 387, 160), // #1345
  INST(Vpinsrw          , VexRvmi            , V(660F00,C4,_,0,0,I,1,T1S), 0                         , 215, 0  , 8910 , 388, 159), // #1346
  INST(Vplzcntd         , VexRm_Lx           , E(660F38,44,_,x,_,0,4,FV ), 0                         , 114, 0  , 8918 , 374, 153), // #1347
  INST(Vplzcntq         , VexRm_Lx           , E(660F38,44,_,x,_,1,4,FV ), 0                         , 113, 0  , 8927 , 350, 153), // #1348
  INST(Vpmacsdd         , VexRvmr            , V(XOP_M8,9E,_,0,0,_,_,_  ), 0                         , 208, 0  , 8936 , 389, 144), // #1349
  INST(Vpmacsdqh        , VexRvmr            , V(XOP_M8,9F,_,0,0,_,_,_  ), 0                         , 208, 0  , 8945 , 389, 144), // #1350
  INST(Vpmacsdql        , VexRvmr            , V(XOP_M8,97,_,0,0,_,_,_  ), 0                         , 208, 0  , 8955 , 389, 144), // #1351
  INST(Vpmacssdd        , VexRvmr            , V(XOP_M8,8E,_,0,0,_,_,_  ), 0                         , 208, 0  , 8965 , 389, 144), // #1352
  INST(Vpmacssdqh       , VexRvmr            , V(XOP_M8,8F,_,0,0,_,_,_  ), 0                         , 208, 0  , 8975 , 389, 144), // #1353
  INST(Vpmacssdql       , VexRvmr            , V(XOP_M8,87,_,0,0,_,_,_  ), 0                         , 208, 0  , 8986 , 389, 144), // #1354
  INST(Vpmacsswd        , VexRvmr            , V(XOP_M8,86,_,0,0,_,_,_  ), 0                         , 208, 0  , 8997 , 389, 144), // #1355
  INST(Vpmacssww        , VexRvmr            , V(XOP_M8,85,_,0,0,_,_,_  ), 0                         , 208, 0  , 9007 , 389, 144), // #1356
  INST(Vpmacswd         , VexRvmr            , V(XOP_M8,96,_,0,0,_,_,_  ), 0                         , 208, 0  , 9017 , 389, 144), // #1357
  INST(Vpmacsww         , VexRvmr            , V(XOP_M8,95,_,0,0,_,_,_  ), 0                         , 208, 0  , 9026 , 389, 144), // #1358
  INST(Vpmadcsswd       , VexRvmr            , V(XOP_M8,A6,_,0,0,_,_,_  ), 0                         , 208, 0  , 9035 , 389, 144), // #1359
  INST(Vpmadcswd        , VexRvmr            , V(XOP_M8,B6,_,0,0,_,_,_  ), 0                         , 208, 0  , 9046 , 389, 144), // #1360
  INST(Vpmadd52huq      , VexRvm_Lx          , E(660F38,B5,_,x,_,1,4,FV ), 0                         , 113, 0  , 9056 , 212, 161), // #1361
  INST(Vpmadd52luq      , VexRvm_Lx          , E(660F38,B4,_,x,_,1,4,FV ), 0                         , 113, 0  , 9068 , 212, 161), // #1362
  INST(Vpmaddubsw       , VexRvm_Lx          , V(660F38,04,_,x,I,I,4,FVM), 0                         , 110, 0  , 9080 , 315, 151), // #1363
  INST(Vpmaddwd         , VexRvm_Lx          , V(660F00,F5,_,x,I,I,4,FVM), 0                         , 144, 0  , 9091 , 315, 151), // #1364
  INST(Vpmaskmovd       , VexRvmMvr_Lx       , V(660F38,8C,_,x,0,_,_,_  ), V(660F38,8E,_,x,0,_,_,_  ), 96 , 120, 9100 , 323, 134), // #1365
  INST(Vpmaskmovq       , VexRvmMvr_Lx       , V(660F38,8C,_,x,1,_,_,_  ), V(660F38,8E,_,x,1,_,_,_  ), 189, 121, 9111 , 323, 134), // #1366
  INST(Vpmaxsb          , VexRvm_Lx          , V(660F38,3C,_,x,I,I,4,FVM), 0                         , 110, 0  , 9122 , 390, 151), // #1367
  INST(Vpmaxsd          , VexRvm_Lx          , V(660F38,3D,_,x,I,0,4,FV ), 0                         , 110, 0  , 9130 , 211, 135), // #1368
  INST(Vpmaxsq          , VexRvm_Lx          , E(660F38,3D,_,x,_,1,4,FV ), 0                         , 113, 0  , 9138 , 212, 131), // #1369
  INST(Vpmaxsw          , VexRvm_Lx          , V(660F00,EE,_,x,I,I,4,FVM), 0                         , 144, 0  , 9146 , 390, 151), // #1370
  INST(Vpmaxub          , VexRvm_Lx          , V(660F00,DE,_,x,I,I,4,FVM), 0                         , 144, 0  , 9154 , 390, 151), // #1371
  INST(Vpmaxud          , VexRvm_Lx          , V(660F38,3F,_,x,I,0,4,FV ), 0                         , 110, 0  , 9162 , 211, 135), // #1372
  INST(Vpmaxuq          , VexRvm_Lx          , E(660F38,3F,_,x,_,1,4,FV ), 0                         , 113, 0  , 9170 , 212, 131), // #1373
  INST(Vpmaxuw          , VexRvm_Lx          , V(660F38,3E,_,x,I,I,4,FVM), 0                         , 110, 0  , 9178 , 390, 151), // #1374
  INST(Vpminsb          , VexRvm_Lx          , V(660F38,38,_,x,I,I,4,FVM), 0                         , 110, 0  , 9186 , 390, 151), // #1375
  INST(Vpminsd          , VexRvm_Lx          , V(660F38,39,_,x,I,0,4,FV ), 0                         , 110, 0  , 9194 , 211, 135), // #1376
  INST(Vpminsq          , VexRvm_Lx          , E(660F38,39,_,x,_,1,4,FV ), 0                         , 113, 0  , 9202 , 212, 131), // #1377
  INST(Vpminsw          , VexRvm_Lx          , V(660F00,EA,_,x,I,I,4,FVM), 0                         , 144, 0  , 9210 , 390, 151), // #1378
  INST(Vpminub          , VexRvm_Lx          , V(660F00,DA,_,x,I,_,4,FVM), 0                         , 144, 0  , 9218 , 390, 151), // #1379
  INST(Vpminud          , VexRvm_Lx          , V(660F38,3B,_,x,I,0,4,FV ), 0                         , 110, 0  , 9226 , 211, 135), // #1380
  INST(Vpminuq          , VexRvm_Lx          , E(660F38,3B,_,x,_,1,4,FV ), 0                         , 113, 0  , 9234 , 212, 131), // #1381
  INST(Vpminuw          , VexRvm_Lx          , V(660F38,3A,_,x,I,_,4,FVM), 0                         , 110, 0  , 9242 , 390, 151), // #1382
  INST(Vpmovb2m         , VexRm_Lx           , E(F30F38,29,_,x,_,0,_,_  ), 0                         , 206, 0  , 9250 , 391, 139), // #1383
  INST(Vpmovd2m         , VexRm_Lx           , E(F30F38,39,_,x,_,0,_,_  ), 0                         , 206, 0  , 9259 , 391, 133), // #1384
  INST(Vpmovdb          , VexMr_Lx           , E(F30F38,31,_,x,_,0,2,QVM), 0                         , 216, 0  , 9268 , 392, 131), // #1385
  INST(Vpmovdw          , VexMr_Lx           , E(F30F38,33,_,x,_,0,3,HVM), 0                         , 217, 0  , 9276 , 393, 131), // #1386
  INST(Vpmovm2b         , VexRm_Lx           , E(F30F38,28,_,x,_,0,_,_  ), 0                         , 206, 0  , 9284 , 360, 139), // #1387
  INST(Vpmovm2d         , VexRm_Lx           , E(F30F38,38,_,x,_,0,_,_  ), 0                         , 206, 0  , 9293 , 360, 133), // #1388
  INST(Vpmovm2q         , VexRm_Lx           , E(F30F38,38,_,x,_,1,_,_  ), 0                         , 205, 0  , 9302 , 360, 133), // #1389
  INST(Vpmovm2w         , VexRm_Lx           , E(F30F38,28,_,x,_,1,_,_  ), 0                         , 205, 0  , 9311 , 360, 139), // #1390
  INST(Vpmovmskb        , VexRm_Lx           , V(660F00,D7,_,x,I,_,_,_  ), 0                         , 69 , 0  , 9320 , 336, 148), // #1391
  INST(Vpmovq2m         , VexRm_Lx           , E(F30F38,39,_,x,_,1,_,_  ), 0                         , 205, 0  , 9330 , 391, 133), // #1392
  INST(Vpmovqb          , VexMr_Lx           , E(F30F38,32,_,x,_,0,1,OVM), 0                         , 218, 0  , 9339 , 394, 131), // #1393
  INST(Vpmovqd          , VexMr_Lx           , E(F30F38,35,_,x,_,0,3,HVM), 0                         , 217, 0  , 9347 , 393, 131), // #1394
  INST(Vpmovqw          , VexMr_Lx           , E(F30F38,34,_,x,_,0,2,QVM), 0                         , 216, 0  , 9355 , 392, 131), // #1395
  INST(Vpmovsdb         , VexMr_Lx           , E(F30F38,21,_,x,_,0,2,QVM), 0                         , 216, 0  , 9363 , 392, 131), // #1396
  INST(Vpmovsdw         , VexMr_Lx           , E(F30F38,23,_,x,_,0,3,HVM), 0                         , 217, 0  , 9372 , 393, 131), // #1397
  INST(Vpmovsqb         , VexMr_Lx           , E(F30F38,22,_,x,_,0,1,OVM), 0                         , 218, 0  , 9381 , 394, 131), // #1398
  INST(Vpmovsqd         , VexMr_Lx           , E(F30F38,25,_,x,_,0,3,HVM), 0                         , 217, 0  , 9390 , 393, 131), // #1399
  INST(Vpmovsqw         , VexMr_Lx           , E(F30F38,24,_,x,_,0,2,QVM), 0                         , 216, 0  , 9399 , 392, 131), // #1400
  INST(Vpmovswb         , VexMr_Lx           , E(F30F38,20,_,x,_,0,3,HVM), 0                         , 217, 0  , 9408 , 393, 139), // #1401
  INST(Vpmovsxbd        , VexRm_Lx           , V(660F38,21,_,x,I,I,2,QVM), 0                         , 219, 0  , 9417 , 395, 135), // #1402
  INST(Vpmovsxbq        , VexRm_Lx           , V(660F38,22,_,x,I,I,1,OVM), 0                         , 220, 0  , 9427 , 396, 135), // #1403
  INST(Vpmovsxbw        , VexRm_Lx           , V(660F38,20,_,x,I,I,3,HVM), 0                         , 139, 0  , 9437 , 397, 151), // #1404
  INST(Vpmovsxdq        , VexRm_Lx           , V(660F38,25,_,x,I,0,3,HVM), 0                         , 139, 0  , 9447 , 397, 135), // #1405
  INST(Vpmovsxwd        , VexRm_Lx           , V(660F38,23,_,x,I,I,3,HVM), 0                         , 139, 0  , 9457 , 397, 135), // #1406
  INST(Vpmovsxwq        , VexRm_Lx           , V(660F38,24,_,x,I,I,2,QVM), 0                         , 219, 0  , 9467 , 395, 135), // #1407
  INST(Vpmovusdb        , VexMr_Lx           , E(F30F38,11,_,x,_,0,2,QVM), 0                         , 216, 0  , 9477 , 392, 131), // #1408
  INST(Vpmovusdw        , VexMr_Lx           , E(F30F38,13,_,x,_,0,3,HVM), 0                         , 217, 0  , 9487 , 393, 131), // #1409
  INST(Vpmovusqb        , VexMr_Lx           , E(F30F38,12,_,x,_,0,1,OVM), 0                         , 218, 0  , 9497 , 394, 131), // #1410
  INST(Vpmovusqd        , VexMr_Lx           , E(F30F38,15,_,x,_,0,3,HVM), 0                         , 217, 0  , 9507 , 393, 131), // #1411
  INST(Vpmovusqw        , VexMr_Lx           , E(F30F38,14,_,x,_,0,2,QVM), 0                         , 216, 0  , 9517 , 392, 131), // #1412
  INST(Vpmovuswb        , VexMr_Lx           , E(F30F38,10,_,x,_,0,3,HVM), 0                         , 217, 0  , 9527 , 393, 139), // #1413
  INST(Vpmovw2m         , VexRm_Lx           , E(F30F38,29,_,x,_,1,_,_  ), 0                         , 205, 0  , 9537 , 391, 139), // #1414
  INST(Vpmovwb          , VexMr_Lx           , E(F30F38,30,_,x,_,0,3,HVM), 0                         , 217, 0  , 9546 , 393, 139), // #1415
  INST(Vpmovzxbd        , VexRm_Lx           , V(660F38,31,_,x,I,I,2,QVM), 0                         , 219, 0  , 9554 , 395, 135), // #1416
  INST(Vpmovzxbq        , VexRm_Lx           , V(660F38,32,_,x,I,I,1,OVM), 0                         , 220, 0  , 9564 , 396, 135), // #1417
  INST(Vpmovzxbw        , VexRm_Lx           , V(660F38,30,_,x,I,I,3,HVM), 0                         , 139, 0  , 9574 , 397, 151), // #1418
  INST(Vpmovzxdq        , VexRm_Lx           , V(660F38,35,_,x,I,0,3,HVM), 0                         , 139, 0  , 9584 , 397, 135), // #1419
  INST(Vpmovzxwd        , VexRm_Lx           , V(660F38,33,_,x,I,I,3,HVM), 0                         , 139, 0  , 9594 , 397, 135), // #1420
  INST(Vpmovzxwq        , VexRm_Lx           , V(660F38,34,_,x,I,I,2,QVM), 0                         , 219, 0  , 9604 , 395, 135), // #1421
  INST(Vpmuldq          , VexRvm_Lx          , V(660F38,28,_,x,I,1,4,FV ), 0                         , 209, 0  , 9614 , 208, 135), // #1422
  INST(Vpmulhrsw        , VexRvm_Lx          , V(660F38,0B,_,x,I,I,4,FVM), 0                         , 110, 0  , 9622 , 315, 151), // #1423
  INST(Vpmulhuw         , VexRvm_Lx          , V(660F00,E4,_,x,I,I,4,FVM), 0                         , 144, 0  , 9632 , 315, 151), // #1424
  INST(Vpmulhw          , VexRvm_Lx          , V(660F00,E5,_,x,I,I,4,FVM), 0                         , 144, 0  , 9641 , 315, 151), // #1425
  INST(Vpmulld          , VexRvm_Lx          , V(660F38,40,_,x,I,0,4,FV ), 0                         , 110, 0  , 9649 , 209, 135), // #1426
  INST(Vpmullq          , VexRvm_Lx          , E(660F38,40,_,x,_,1,4,FV ), 0                         , 113, 0  , 9657 , 212, 133), // #1427
  INST(Vpmullw          , VexRvm_Lx          , V(660F00,D5,_,x,I,I,4,FVM), 0                         , 144, 0  , 9665 , 315, 151), // #1428
  INST(Vpmultishiftqb   , VexRvm_Lx          , E(660F38,83,_,x,_,1,4,FV ), 0                         , 113, 0  , 9673 , 212, 158), // #1429
  INST(Vpmuludq         , VexRvm_Lx          , V(660F00,F4,_,x,I,1,4,FV ), 0                         , 103, 0  , 9688 , 208, 135), // #1430
  INST(Vpopcntb         , VexRm_Lx           , E(660F38,54,_,x,_,0,4,FV ), 0                         , 114, 0  , 9697 , 279, 162), // #1431
  INST(Vpopcntd         , VexRm_Lx           , E(660F38,55,_,x,_,0,4,FVM), 0                         , 114, 0  , 9706 , 374, 163), // #1432
  INST(Vpopcntq         , VexRm_Lx           , E(660F38,55,_,x,_,1,4,FVM), 0                         , 113, 0  , 9715 , 350, 163), // #1433
  INST(Vpopcntw         , VexRm_Lx           , E(660F38,54,_,x,_,1,4,FV ), 0                         , 113, 0  , 9724 , 279, 162), // #1434
  INST(Vpor             , VexRvm_Lx          , V(660F00,EB,_,x,I,_,_,_  ), 0                         , 69 , 0  , 9733 , 351, 148), // #1435
  INST(Vpord            , VexRvm_Lx          , E(660F00,EB,_,x,_,0,4,FV ), 0                         , 198, 0  , 9738 , 352, 131), // #1436
  INST(Vporq            , VexRvm_Lx          , E(660F00,EB,_,x,_,1,4,FV ), 0                         , 135, 0  , 9744 , 356, 131), // #1437
  INST(Vpperm           , VexRvrmRvmr        , V(XOP_M8,A3,_,0,x,_,_,_  ), 0                         , 208, 0  , 9750 , 398, 144), // #1438
  INST(Vprold           , VexVmi_Lx          , E(660F00,72,1,x,_,0,4,FV ), 0                         , 221, 0  , 9757 , 399, 131), // #1439
  INST(Vprolq           , VexVmi_Lx          , E(660F00,72,1,x,_,1,4,FV ), 0                         , 222, 0  , 9764 , 400, 131), // #1440
  INST(Vprolvd          , VexRvm_Lx          , E(660F38,15,_,x,_,0,4,FV ), 0                         , 114, 0  , 9771 , 213, 131), // #1441
  INST(Vprolvq          , VexRvm_Lx          , E(660F38,15,_,x,_,1,4,FV ), 0                         , 113, 0  , 9779 , 212, 131), // #1442
  INST(Vprord           , VexVmi_Lx          , E(660F00,72,0,x,_,0,4,FV ), 0                         , 198, 0  , 9787 , 399, 131), // #1443
  INST(Vprorq           , VexVmi_Lx          , E(660F00,72,0,x,_,1,4,FV ), 0                         , 135, 0  , 9794 , 400, 131), // #1444
  INST(Vprorvd          , VexRvm_Lx          , E(660F38,14,_,x,_,0,4,FV ), 0                         , 114, 0  , 9801 , 213, 131), // #1445
  INST(Vprorvq          , VexRvm_Lx          , E(660F38,14,_,x,_,1,4,FV ), 0                         , 113, 0  , 9809 , 212, 131), // #1446
  INST(Vprotb           , VexRvmRmvRmi       , V(XOP_M9,90,_,0,x,_,_,_  ), V(XOP_M8,C0,_,0,x,_,_,_  ), 79 , 122, 9817 , 401, 144), // #1447
  INST(Vprotd           , VexRvmRmvRmi       , V(XOP_M9,92,_,0,x,_,_,_  ), V(XOP_M8,C2,_,0,x,_,_,_  ), 79 , 123, 9824 , 401, 144), // #1448
  INST(Vprotq           , VexRvmRmvRmi       , V(XOP_M9,93,_,0,x,_,_,_  ), V(XOP_M8,C3,_,0,x,_,_,_  ), 79 , 124, 9831 , 401, 144), // #1449
  INST(Vprotw           , VexRvmRmvRmi       , V(XOP_M9,91,_,0,x,_,_,_  ), V(XOP_M8,C1,_,0,x,_,_,_  ), 79 , 125, 9838 , 401, 144), // #1450
  INST(Vpsadbw          , VexRvm_Lx          , V(660F00,F6,_,x,I,I,4,FVM), 0                         , 144, 0  , 9845 , 203, 151), // #1451
  INST(Vpscatterdd      , VexMr_VM           , E(660F38,A0,_,x,_,0,2,T1S), 0                         , 129, 0  , 9853 , 402, 131), // #1452
  INST(Vpscatterdq      , VexMr_VM           , E(660F38,A0,_,x,_,1,3,T1S), 0                         , 128, 0  , 9865 , 403, 131), // #1453
  INST(Vpscatterqd      , VexMr_VM           , E(660F38,A1,_,x,_,0,2,T1S), 0                         , 129, 0  , 9877 , 404, 131), // #1454
  INST(Vpscatterqq      , VexMr_VM           , E(660F38,A1,_,x,_,1,3,T1S), 0                         , 128, 0  , 9889 , 405, 131), // #1455
  INST(Vpshab           , VexRvmRmv          , V(XOP_M9,98,_,0,x,_,_,_  ), 0                         , 79 , 0  , 9901 , 406, 144), // #1456
  INST(Vpshad           , VexRvmRmv          , V(XOP_M9,9A,_,0,x,_,_,_  ), 0                         , 79 , 0  , 9908 , 406, 144), // #1457
  INST(Vpshaq           , VexRvmRmv          , V(XOP_M9,9B,_,0,x,_,_,_  ), 0                         , 79 , 0  , 9915 , 406, 144), // #1458
  INST(Vpshaw           , VexRvmRmv          , V(XOP_M9,99,_,0,x,_,_,_  ), 0                         , 79 , 0  , 9922 , 406, 144), // #1459
  INST(Vpshlb           , VexRvmRmv          , V(XOP_M9,94,_,0,x,_,_,_  ), 0                         , 79 , 0  , 9929 , 406, 144), // #1460
  INST(Vpshld           , VexRvmRmv          , V(XOP_M9,96,_,0,x,_,_,_  ), 0                         , 79 , 0  , 9936 , 406, 144), // #1461
  INST(Vpshldd          , VexRvmi_Lx         , E(660F3A,71,_,x,_,0,4,FV ), 0                         , 111, 0  , 9943 , 206, 156), // #1462
  INST(Vpshldq          , VexRvmi_Lx         , E(660F3A,71,_,x,_,1,4,FV ), 0                         , 112, 0  , 9951 , 207, 156), // #1463
  INST(Vpshldvd         , VexRvm_Lx          , E(660F38,71,_,x,_,0,4,FV ), 0                         , 114, 0  , 9959 , 213, 156), // #1464
  INST(Vpshldvq         , VexRvm_Lx          , E(660F38,71,_,x,_,1,4,FV ), 0                         , 113, 0  , 9968 , 212, 156), // #1465
  INST(Vpshldvw         , VexRvm_Lx          , E(660F38,70,_,x,_,1,4,FVM), 0                         , 113, 0  , 9977 , 357, 156), // #1466
  INST(Vpshldw          , VexRvmi_Lx         , E(660F3A,70,_,x,_,1,4,FVM), 0                         , 112, 0  , 9986 , 275, 156), // #1467
  INST(Vpshlq           , VexRvmRmv          , V(XOP_M9,97,_,0,x,_,_,_  ), 0                         , 79 , 0  , 9994 , 406, 144), // #1468
  INST(Vpshlw           , VexRvmRmv          , V(XOP_M9,95,_,0,x,_,_,_  ), 0                         , 79 , 0  , 10001, 406, 144), // #1469
  INST(Vpshrdd          , VexRvmi_Lx         , E(660F3A,73,_,x,_,0,4,FV ), 0                         , 111, 0  , 10008, 206, 156), // #1470
  INST(Vpshrdq          , VexRvmi_Lx         , E(660F3A,73,_,x,_,1,4,FV ), 0                         , 112, 0  , 10016, 207, 156), // #1471
  INST(Vpshrdvd         , VexRvm_Lx          , E(660F38,73,_,x,_,0,4,FV ), 0                         , 114, 0  , 10024, 213, 156), // #1472
  INST(Vpshrdvq         , VexRvm_Lx          , E(660F38,73,_,x,_,1,4,FV ), 0                         , 113, 0  , 10033, 212, 156), // #1473
  INST(Vpshrdvw         , VexRvm_Lx          , E(660F38,72,_,x,_,1,4,FVM), 0                         , 113, 0  , 10042, 357, 156), // #1474
  INST(Vpshrdw          , VexRvmi_Lx         , E(660F3A,72,_,x,_,1,4,FVM), 0                         , 112, 0  , 10051, 275, 156), // #1475
  INST(Vpshufb          , VexRvm_Lx          , V(660F38,00,_,x,I,I,4,FVM), 0                         , 110, 0  , 10059, 315, 151), // #1476
  INST(Vpshufbitqmb     , VexRvm_Lx          , E(660F38,8F,_,x,0,0,4,FVM), 0                         , 114, 0  , 10067, 407, 162), // #1477
  INST(Vpshufd          , VexRmi_Lx          , V(660F00,70,_,x,I,0,4,FV ), 0                         , 144, 0  , 10080, 408, 135), // #1478
  INST(Vpshufhw         , VexRmi_Lx          , V(F30F00,70,_,x,I,I,4,FVM), 0                         , 161, 0  , 10088, 409, 151), // #1479
  INST(Vpshuflw         , VexRmi_Lx          , V(F20F00,70,_,x,I,I,4,FVM), 0                         , 223, 0  , 10097, 409, 151), // #1480
  INST(Vpsignb          , VexRvm_Lx          , V(660F38,08,_,x,I,_,_,_  ), 0                         , 96 , 0  , 10106, 202, 148), // #1481
  INST(Vpsignd          , VexRvm_Lx          , V(660F38,0A,_,x,I,_,_,_  ), 0                         , 96 , 0  , 10114, 202, 148), // #1482
  INST(Vpsignw          , VexRvm_Lx          , V(660F38,09,_,x,I,_,_,_  ), 0                         , 96 , 0  , 10122, 202, 148), // #1483
  INST(Vpslld           , VexRvmVmi_Lx_MEvex , V(660F00,F2,_,x,I,0,4,128), V(660F00,72,6,x,I,0,4,FV ), 224, 126, 10130, 410, 135), // #1484
  INST(Vpslldq          , VexVmi_Lx_MEvex    , V(660F00,73,7,x,I,I,4,FVM), 0                         , 225, 0  , 10137, 411, 151), // #1485
  INST(Vpsllq           , VexRvmVmi_Lx_MEvex , V(660F00,F3,_,x,I,1,4,128), V(660F00,73,6,x,I,1,4,FV ), 226, 127, 10145, 412, 135), // #1486
  INST(Vpsllvd          , VexRvm_Lx          , V(660F38,47,_,x,0,0,4,FV ), 0                         , 110, 0  , 10152, 209, 145), // #1487
  INST(Vpsllvq          , VexRvm_Lx          , V(660F38,47,_,x,1,1,4,FV ), 0                         , 182, 0  , 10160, 208, 145), // #1488
  INST(Vpsllvw          , VexRvm_Lx          , E(660F38,12,_,x,_,1,4,FVM), 0                         , 113, 0  , 10168, 357, 139), // #1489
  INST(Vpsllw           , VexRvmVmi_Lx_MEvex , V(660F00,F1,_,x,I,I,4,128), V(660F00,71,6,x,I,I,4,FVM), 224, 128, 10176, 413, 151), // #1490
  INST(Vpsrad           , VexRvmVmi_Lx_MEvex , V(660F00,E2,_,x,I,0,4,128), V(660F00,72,4,x,I,0,4,FV ), 224, 129, 10183, 410, 135), // #1491
  INST(Vpsraq           , VexRvmVmi_Lx_MEvex , E(660F00,E2,_,x,_,1,4,128), E(660F00,72,4,x,_,1,4,FV ), 227, 130, 10190, 414, 131), // #1492
  INST(Vpsravd          , VexRvm_Lx          , V(660F38,46,_,x,0,0,4,FV ), 0                         , 110, 0  , 10197, 209, 145), // #1493
  INST(Vpsravq          , VexRvm_Lx          , E(660F38,46,_,x,_,1,4,FV ), 0                         , 113, 0  , 10205, 212, 131), // #1494
  INST(Vpsravw          , VexRvm_Lx          , E(660F38,11,_,x,_,1,4,FVM), 0                         , 113, 0  , 10213, 357, 139), // #1495
  INST(Vpsraw           , VexRvmVmi_Lx_MEvex , V(660F00,E1,_,x,I,I,4,128), V(660F00,71,4,x,I,I,4,FVM), 224, 131, 10221, 413, 151), // #1496
  INST(Vpsrld           , VexRvmVmi_Lx_MEvex , V(660F00,D2,_,x,I,0,4,128), V(660F00,72,2,x,I,0,4,FV ), 224, 132, 10228, 410, 135), // #1497
  INST(Vpsrldq          , VexVmi_Lx_MEvex    , V(660F00,73,3,x,I,I,4,FVM), 0                         , 228, 0  , 10235, 411, 151), // #1498
  INST(Vpsrlq           , VexRvmVmi_Lx_MEvex , V(660F00,D3,_,x,I,1,4,128), V(660F00,73,2,x,I,1,4,FV ), 226, 133, 10243, 412, 135), // #1499
  INST(Vpsrlvd          , VexRvm_Lx          , V(660F38,45,_,x,0,0,4,FV ), 0                         , 110, 0  , 10250, 209, 145), // #1500
  INST(Vpsrlvq          , VexRvm_Lx          , V(660F38,45,_,x,1,1,4,FV ), 0                         , 182, 0  , 10258, 208, 145), // #1501
  INST(Vpsrlvw          , VexRvm_Lx          , E(660F38,10,_,x,_,1,4,FVM), 0                         , 113, 0  , 10266, 357, 139), // #1502
  INST(Vpsrlw           , VexRvmVmi_Lx_MEvex , V(660F00,D1,_,x,I,I,4,128), V(660F00,71,2,x,I,I,4,FVM), 224, 134, 10274, 413, 151), // #1503
  INST(Vpsubb           , VexRvm_Lx          , V(660F00,F8,_,x,I,I,4,FVM), 0                         , 144, 0  , 10281, 415, 151), // #1504
  INST(Vpsubd           , VexRvm_Lx          , V(660F00,FA,_,x,I,0,4,FV ), 0                         , 144, 0  , 10288, 416, 135), // #1505
  INST(Vpsubq           , VexRvm_Lx          , V(660F00,FB,_,x,I,1,4,FV ), 0                         , 103, 0  , 10295, 417, 135), // #1506
  INST(Vpsubsb          , VexRvm_Lx          , V(660F00,E8,_,x,I,I,4,FVM), 0                         , 144, 0  , 10302, 415, 151), // #1507
  INST(Vpsubsw          , VexRvm_Lx          , V(660F00,E9,_,x,I,I,4,FVM), 0                         , 144, 0  , 10310, 415, 151), // #1508
  INST(Vpsubusb         , VexRvm_Lx          , V(660F00,D8,_,x,I,I,4,FVM), 0                         , 144, 0  , 10318, 415, 151), // #1509
  INST(Vpsubusw         , VexRvm_Lx          , V(660F00,D9,_,x,I,I,4,FVM), 0                         , 144, 0  , 10327, 415, 151), // #1510
  INST(Vpsubw           , VexRvm_Lx          , V(660F00,F9,_,x,I,I,4,FVM), 0                         , 144, 0  , 10336, 415, 151), // #1511
  INST(Vpternlogd       , VexRvmi_Lx         , E(660F3A,25,_,x,_,0,4,FV ), 0                         , 111, 0  , 10343, 206, 131), // #1512
  INST(Vpternlogq       , VexRvmi_Lx         , E(660F3A,25,_,x,_,1,4,FV ), 0                         , 112, 0  , 10354, 207, 131), // #1513
  INST(Vptest           , VexRm_Lx           , V(660F38,17,_,x,I,_,_,_  ), 0                         , 96 , 0  , 10365, 298, 155), // #1514
  INST(Vptestmb         , VexRvm_Lx          , E(660F38,26,_,x,_,0,4,FVM), 0                         , 114, 0  , 10372, 407, 139), // #1515
  INST(Vptestmd         , VexRvm_Lx          , E(660F38,27,_,x,_,0,4,FV ), 0                         , 114, 0  , 10381, 418, 131), // #1516
  INST(Vptestmq         , VexRvm_Lx          , E(660F38,27,_,x,_,1,4,FV ), 0                         , 113, 0  , 10390, 419, 131), // #1517
  INST(Vptestmw         , VexRvm_Lx          , E(660F38,26,_,x,_,1,4,FVM), 0                         , 113, 0  , 10399, 407, 139), // #1518
  INST(Vptestnmb        , VexRvm_Lx          , E(F30F38,26,_,x,_,0,4,FVM), 0                         , 132, 0  , 10408, 407, 139), // #1519
  INST(Vptestnmd        , VexRvm_Lx          , E(F30F38,27,_,x,_,0,4,FV ), 0                         , 132, 0  , 10418, 418, 131), // #1520
  INST(Vptestnmq        , VexRvm_Lx          , E(F30F38,27,_,x,_,1,4,FV ), 0                         , 229, 0  , 10428, 419, 131), // #1521
  INST(Vptestnmw        , VexRvm_Lx          , E(F30F38,26,_,x,_,1,4,FVM), 0                         , 229, 0  , 10438, 407, 139), // #1522
  INST(Vpunpckhbw       , VexRvm_Lx          , V(660F00,68,_,x,I,I,4,FVM), 0                         , 144, 0  , 10448, 315, 151), // #1523
  INST(Vpunpckhdq       , VexRvm_Lx          , V(660F00,6A,_,x,I,0,4,FV ), 0                         , 144, 0  , 10459, 209, 135), // #1524
  INST(Vpunpckhqdq      , VexRvm_Lx          , V(660F00,6D,_,x,I,1,4,FV ), 0                         , 103, 0  , 10470, 208, 135), // #1525
  INST(Vpunpckhwd       , VexRvm_Lx          , V(660F00,69,_,x,I,I,4,FVM), 0                         , 144, 0  , 10482, 315, 151), // #1526
  INST(Vpunpcklbw       , VexRvm_Lx          , V(660F00,60,_,x,I,I,4,FVM), 0                         , 144, 0  , 10493, 315, 151), // #1527
  INST(Vpunpckldq       , VexRvm_Lx          , V(660F00,62,_,x,I,0,4,FV ), 0                         , 144, 0  , 10504, 209, 135), // #1528
  INST(Vpunpcklqdq      , VexRvm_Lx          , V(660F00,6C,_,x,I,1,4,FV ), 0                         , 103, 0  , 10515, 208, 135), // #1529
  INST(Vpunpcklwd       , VexRvm_Lx          , V(660F00,61,_,x,I,I,4,FVM), 0                         , 144, 0  , 10527, 315, 151), // #1530
  INST(Vpxor            , VexRvm_Lx          , V(660F00,EF,_,x,I,_,_,_  ), 0                         , 69 , 0  , 10538, 353, 148), // #1531
  INST(Vpxord           , VexRvm_Lx          , E(660F00,EF,_,x,_,0,4,FV ), 0                         , 198, 0  , 10544, 354, 131), // #1532
  INST(Vpxorq           , VexRvm_Lx          , E(660F00,EF,_,x,_,1,4,FV ), 0                         , 135, 0  , 10551, 355, 131), // #1533
  INST(Vrangepd         , VexRvmi_Lx         , E(660F3A,50,_,x,_,1,4,FV ), 0                         , 112, 0  , 10558, 285, 133), // #1534
  INST(Vrangeps         , VexRvmi_Lx         , E(660F3A,50,_,x,_,0,4,FV ), 0                         , 111, 0  , 10567, 286, 133), // #1535
  INST(Vrangesd         , VexRvmi            , E(660F3A,51,_,I,_,1,3,T1S), 0                         , 180, 0  , 10576, 287, 66 ), // #1536
  INST(Vrangess         , VexRvmi            , E(660F3A,51,_,I,_,0,2,T1S), 0                         , 181, 0  , 10585, 288, 66 ), // #1537
  INST(Vrcp14pd         , VexRm_Lx           , E(660F38,4C,_,x,_,1,4,FV ), 0                         , 113, 0  , 10594, 350, 131), // #1538
  INST(Vrcp14ps         , VexRm_Lx           , E(660F38,4C,_,x,_,0,4,FV ), 0                         , 114, 0  , 10603, 374, 131), // #1539
  INST(Vrcp14sd         , VexRvm             , E(660F38,4D,_,I,_,1,3,T1S), 0                         , 128, 0  , 10612, 420, 68 ), // #1540
  INST(Vrcp14ss         , VexRvm             , E(660F38,4D,_,I,_,0,2,T1S), 0                         , 129, 0  , 10621, 421, 68 ), // #1541
  INST(Vrcp28pd         , VexRm              , E(660F38,CA,_,2,_,1,4,FV ), 0                         , 170, 0  , 10630, 277, 140), // #1542
  INST(Vrcp28ps         , VexRm              , E(660F38,CA,_,2,_,0,4,FV ), 0                         , 171, 0  , 10639, 278, 140), // #1543
  INST(Vrcp28sd         , VexRvm             , E(660F38,CB,_,I,_,1,3,T1S), 0                         , 128, 0  , 10648, 308, 140), // #1544
  INST(Vrcp28ss         , VexRvm             , E(660F38,CB,_,I,_,0,2,T1S), 0                         , 129, 0  , 10657, 309, 140), // #1545
  INST(Vrcpph           , VexRm_Lx           , E(66MAP6,4C,_,_,_,0,4,FV ), 0                         , 183, 0  , 10666, 422, 127), // #1546
  INST(Vrcpps           , VexRm_Lx           , V(000F00,53,_,x,I,_,_,_  ), 0                         , 72 , 0  , 10673, 298, 128), // #1547
  INST(Vrcpsh           , VexRvm             , E(66MAP6,4D,_,_,_,0,1,T1S), 0                         , 185, 0  , 10680, 423, 127), // #1548
  INST(Vrcpss           , VexRvm             , V(F30F00,53,_,I,I,_,_,_  ), 0                         , 199, 0  , 10687, 424, 128), // #1549
  INST(Vreducepd        , VexRmi_Lx          , E(660F3A,56,_,x,_,1,4,FV ), 0                         , 112, 0  , 10694, 400, 133), // #1550
  INST(Vreduceph        , VexRmi_Lx          , E(000F3A,56,_,_,_,0,4,FV ), 0                         , 123, 0  , 10704, 311, 125), // #1551
  INST(Vreduceps        , VexRmi_Lx          , E(660F3A,56,_,x,_,0,4,FV ), 0                         , 111, 0  , 10714, 399, 133), // #1552
  INST(Vreducesd        , VexRvmi            , E(660F3A,57,_,I,_,1,3,T1S), 0                         , 180, 0  , 10724, 425, 66 ), // #1553
  INST(Vreducesh        , VexRvmi            , E(000F3A,57,_,_,_,0,1,T1S), 0                         , 188, 0  , 10734, 313, 127), // #1554
  INST(Vreducess        , VexRvmi            , E(660F3A,57,_,I,_,0,2,T1S), 0                         , 181, 0  , 10744, 426, 66 ), // #1555
  INST(Vrndscalepd      , VexRmi_Lx          , E(660F3A,09,_,x,_,1,4,FV ), 0                         , 112, 0  , 10754, 310, 131), // #1556
  INST(Vrndscaleph      , VexRmi_Lx          , E(000F3A,08,_,_,_,0,4,FV ), 0                         , 123, 0  , 10766, 311, 125), // #1557
  INST(Vrndscaleps      , VexRmi_Lx          , E(660F3A,08,_,x,_,0,4,FV ), 0                         , 111, 0  , 10778, 312, 131), // #1558
  INST(Vrndscalesd      , VexRvmi            , E(660F3A,0B,_,I,_,1,3,T1S), 0                         , 180, 0  , 10790, 287, 68 ), // #1559
  INST(Vrndscalesh      , VexRvmi            , E(000F3A,0A,_,_,_,0,1,T1S), 0                         , 188, 0  , 10802, 313, 127), // #1560
  INST(Vrndscaless      , VexRvmi            , E(660F3A,0A,_,I,_,0,2,T1S), 0                         , 181, 0  , 10814, 288, 68 ), // #1561
  INST(Vroundpd         , VexRmi_Lx          , V(660F3A,09,_,x,I,_,_,_  ), 0                         , 73 , 0  , 10826, 427, 128), // #1562
  INST(Vroundps         , VexRmi_Lx          , V(660F3A,08,_,x,I,_,_,_  ), 0                         , 73 , 0  , 10835, 427, 128), // #1563
  INST(Vroundsd         , VexRvmi            , V(660F3A,0B,_,I,I,_,_,_  ), 0                         , 73 , 0  , 10844, 428, 128), // #1564
  INST(Vroundss         , VexRvmi            , V(660F3A,0A,_,I,I,_,_,_  ), 0                         , 73 , 0  , 10853, 429, 128), // #1565
  INST(Vrsqrt14pd       , VexRm_Lx           , E(660F38,4E,_,x,_,1,4,FV ), 0                         , 113, 0  , 10862, 350, 131), // #1566
  INST(Vrsqrt14ps       , VexRm_Lx           , E(660F38,4E,_,x,_,0,4,FV ), 0                         , 114, 0  , 10873, 374, 131), // #1567
  INST(Vrsqrt14sd       , VexRvm             , E(660F38,4F,_,I,_,1,3,T1S), 0                         , 128, 0  , 10884, 420, 68 ), // #1568
  INST(Vrsqrt14ss       , VexRvm             , E(660F38,4F,_,I,_,0,2,T1S), 0                         , 129, 0  , 10895, 421, 68 ), // #1569
  INST(Vrsqrt28pd       , VexRm              , E(660F38,CC,_,2,_,1,4,FV ), 0                         , 170, 0  , 10906, 277, 140), // #1570
  INST(Vrsqrt28ps       , VexRm              , E(660F38,CC,_,2,_,0,4,FV ), 0                         , 171, 0  , 10917, 278, 140), // #1571
  INST(Vrsqrt28sd       , VexRvm             , E(660F38,CD,_,I,_,1,3,T1S), 0                         , 128, 0  , 10928, 308, 140), // #1572
  INST(Vrsqrt28ss       , VexRvm             , E(660F38,CD,_,I,_,0,2,T1S), 0                         , 129, 0  , 10939, 309, 140), // #1573
  INST(Vrsqrtph         , VexRm_Lx           , E(66MAP6,4E,_,_,_,0,4,FV ), 0                         , 183, 0  , 10950, 422, 125), // #1574
  INST(Vrsqrtps         , VexRm_Lx           , V(000F00,52,_,x,I,_,_,_  ), 0                         , 72 , 0  , 10959, 298, 128), // #1575
  INST(Vrsqrtsh         , VexRvm             , E(66MAP6,4F,_,_,_,0,1,T1S), 0                         , 185, 0  , 10968, 423, 127), // #1576
  INST(Vrsqrtss         , VexRvm             , V(F30F00,52,_,I,I,_,_,_  ), 0                         , 199, 0  , 10977, 424, 128), // #1577
  INST(Vscalefpd        , VexRvm_Lx          , E(660F38,2C,_,x,_,1,4,FV ), 0                         , 113, 0  , 10986, 430, 131), // #1578
  INST(Vscalefph        , VexRvm_Lx          , E(66MAP6,2C,_,_,_,0,4,FV ), 0                         , 183, 0  , 10996, 197, 125), // #1579
  INST(Vscalefps        , VexRvm_Lx          , E(660F38,2C,_,x,_,0,4,FV ), 0                         , 114, 0  , 11006, 284, 131), // #1580
  INST(Vscalefsd        , VexRvm             , E(660F38,2D,_,I,_,1,3,T1S), 0                         , 128, 0  , 11016, 251, 68 ), // #1581
  INST(Vscalefsh        , VexRvm             , E(66MAP6,2D,_,_,_,0,1,T1S), 0                         , 185, 0  , 11026, 200, 127), // #1582
  INST(Vscalefss        , VexRvm             , E(660F38,2D,_,I,_,0,2,T1S), 0                         , 129, 0  , 11036, 259, 68 ), // #1583
  INST(Vscatterdpd      , VexMr_VM           , E(660F38,A2,_,x,_,1,3,T1S), 0                         , 128, 0  , 11046, 403, 131), // #1584
  INST(Vscatterdps      , VexMr_VM           , E(660F38,A2,_,x,_,0,2,T1S), 0                         , 129, 0  , 11058, 402, 131), // #1585
  INST(Vscatterpf0dpd   , VexM_VM            , E(660F38,C6,5,2,_,1,3,T1S), 0                         , 230, 0  , 11070, 303, 146), // #1586
  INST(Vscatterpf0dps   , VexM_VM            , E(660F38,C6,5,2,_,0,2,T1S), 0                         , 231, 0  , 11085, 304, 146), // #1587
  INST(Vscatterpf0qpd   , VexM_VM            , E(660F38,C7,5,2,_,1,3,T1S), 0                         , 230, 0  , 11100, 305, 146), // #1588
  INST(Vscatterpf0qps   , VexM_VM            , E(660F38,C7,5,2,_,0,2,T1S), 0                         , 231, 0  , 11115, 305, 146), // #1589
  INST(Vscatterpf1dpd   , VexM_VM            , E(660F38,C6,6,2,_,1,3,T1S), 0                         , 232, 0  , 11130, 303, 146), // #1590
  INST(Vscatterpf1dps   , VexM_VM            , E(660F38,C6,6,2,_,0,2,T1S), 0                         , 233, 0  , 11145, 304, 146), // #1591
  INST(Vscatterpf1qpd   , VexM_VM            , E(660F38,C7,6,2,_,1,3,T1S), 0                         , 232, 0  , 11160, 305, 146), // #1592
  INST(Vscatterpf1qps   , VexM_VM            , E(660F38,C7,6,2,_,0,2,T1S), 0                         , 233, 0  , 11175, 305, 146), // #1593
  INST(Vscatterqpd      , VexMr_VM           , E(660F38,A3,_,x,_,1,3,T1S), 0                         , 128, 0  , 11190, 405, 131), // #1594
  INST(Vscatterqps      , VexMr_VM           , E(660F38,A3,_,x,_,0,2,T1S), 0                         , 129, 0  , 11202, 404, 131), // #1595
  INST(Vshuff32x4       , VexRvmi_Lx         , E(660F3A,23,_,x,_,0,4,FV ), 0                         , 111, 0  , 11214, 431, 131), // #1596
  INST(Vshuff64x2       , VexRvmi_Lx         , E(660F3A,23,_,x,_,1,4,FV ), 0                         , 112, 0  , 11225, 432, 131), // #1597
  INST(Vshufi32x4       , VexRvmi_Lx         , E(660F3A,43,_,x,_,0,4,FV ), 0                         , 111, 0  , 11236, 431, 131), // #1598
  INST(Vshufi64x2       , VexRvmi_Lx         , E(660F3A,43,_,x,_,1,4,FV ), 0                         , 112, 0  , 11247, 432, 131), // #1599
  INST(Vshufpd          , VexRvmi_Lx         , V(660F00,C6,_,x,I,1,4,FV ), 0                         , 103, 0  , 11258, 433, 124), // #1600
  INST(Vshufps          , VexRvmi_Lx         , V(000F00,C6,_,x,I,0,4,FV ), 0                         , 105, 0  , 11266, 434, 124), // #1601
  INST(Vsqrtpd          , VexRm_Lx           , V(660F00,51,_,x,I,1,4,FV ), 0                         , 103, 0  , 11274, 435, 124), // #1602
  INST(Vsqrtph          , VexRm_Lx           , E(00MAP5,51,_,_,_,0,4,FV ), 0                         , 104, 0  , 11282, 246, 125), // #1603
  INST(Vsqrtps          , VexRm_Lx           , V(000F00,51,_,x,I,0,4,FV ), 0                         , 105, 0  , 11290, 235, 124), // #1604
  INST(Vsqrtsd          , VexRvm             , V(F20F00,51,_,I,I,1,3,T1S), 0                         , 106, 0  , 11298, 199, 126), // #1605
  INST(Vsqrtsh          , VexRvm             , E(F3MAP5,51,_,_,_,0,1,T1S), 0                         , 107, 0  , 11306, 200, 127), // #1606
  INST(Vsqrtss          , VexRvm             , V(F30F00,51,_,I,I,0,2,T1S), 0                         , 108, 0  , 11314, 201, 126), // #1607
  INST(Vstmxcsr         , VexM               , V(000F00,AE,3,0,I,_,_,_  ), 0                         , 234, 0  , 11322, 321, 128), // #1608
  INST(Vsubpd           , VexRvm_Lx          , V(660F00,5C,_,x,I,1,4,FV ), 0                         , 103, 0  , 11331, 196, 124), // #1609
  INST(Vsubph           , VexRvm_Lx          , E(00MAP5,5C,_,_,_,0,4,FV ), 0                         , 104, 0  , 11338, 197, 125), // #1610
  INST(Vsubps           , VexRvm_Lx          , V(000F00,5C,_,x,I,0,4,FV ), 0                         , 105, 0  , 11345, 198, 124), // #1611
  INST(Vsubsd           , VexRvm             , V(F20F00,5C,_,I,I,1,3,T1S), 0                         , 106, 0  , 11352, 199, 126), // #1612
  INST(Vsubsh           , VexRvm             , E(F3MAP5,5C,_,_,_,0,1,T1S), 0                         , 107, 0  , 11359, 200, 127), // #1613
  INST(Vsubss           , VexRvm             , V(F30F00,5C,_,I,I,0,2,T1S), 0                         , 108, 0  , 11366, 201, 126), // #1614
  INST(Vtestpd          , VexRm_Lx           , V(660F38,0F,_,x,0,_,_,_  ), 0                         , 96 , 0  , 11373, 298, 155), // #1615
  INST(Vtestps          , VexRm_Lx           , V(660F38,0E,_,x,0,_,_,_  ), 0                         , 96 , 0  , 11381, 298, 155), // #1616
  INST(Vucomisd         , VexRm              , V(660F00,2E,_,I,I,1,3,T1S), 0                         , 125, 0  , 11389, 229, 136), // #1617
  INST(Vucomish         , VexRm              , E(00MAP5,2E,_,_,_,0,1,T1S), 0                         , 126, 0  , 11398, 230, 127), // #1618
  INST(Vucomiss         , VexRm              , V(000F00,2E,_,I,I,0,2,T1S), 0                         , 127, 0  , 11407, 231, 136), // #1619
  INST(Vunpckhpd        , VexRvm_Lx          , V(660F00,15,_,x,I,1,4,FV ), 0                         , 103, 0  , 11416, 208, 124), // #1620
  INST(Vunpckhps        , VexRvm_Lx          , V(000F00,15,_,x,I,0,4,FV ), 0                         , 105, 0  , 11426, 209, 124), // #1621
  INST(Vunpcklpd        , VexRvm_Lx          , V(660F00,14,_,x,I,1,4,FV ), 0                         , 103, 0  , 11436, 208, 124), // #1622
  INST(Vunpcklps        , VexRvm_Lx          , V(000F00,14,_,x,I,0,4,FV ), 0                         , 105, 0  , 11446, 209, 124), // #1623
  INST(Vxorpd           , VexRvm_Lx          , V(660F00,57,_,x,I,1,4,FV ), 0                         , 103, 0  , 11456, 417, 132), // #1624
  INST(Vxorps           , VexRvm_Lx          , V(000F00,57,_,x,I,0,4,FV ), 0                         , 105, 0  , 11463, 416, 132), // #1625
  INST(Vzeroall         , VexOp              , V(000F00,77,_,1,I,_,_,_  ), 0                         , 68 , 0  , 11470, 436, 128), // #1626
  INST(Vzeroupper       , VexOp              , V(000F00,77,_,0,I,_,_,_  ), 0                         , 72 , 0  , 11479, 436, 128), // #1627
  INST(Wbinvd           , X86Op              , O(000F00,09,_,_,_,_,_,_  ), 0                         , 4  , 0  , 11490, 30 , 0  ), // #1628
  INST(Wbnoinvd         , X86Op              , O(F30F00,09,_,_,_,_,_,_  ), 0                         , 6  , 0  , 11497, 30 , 164), // #1629
  INST(Wrfsbase         , X86M               , O(F30F00,AE,2,_,x,_,_,_  ), 0                         , 235, 0  , 11506, 173, 104), // #1630
  INST(Wrgsbase         , X86M               , O(F30F00,AE,3,_,x,_,_,_  ), 0                         , 236, 0  , 11515, 173, 104), // #1631
  INST(Wrmsr            , X86Op              , O(000F00,30,_,_,_,_,_,_  ), 0                         , 4  , 0  , 11524, 174, 105), // #1632
  INST(Wrssd            , X86Mr              , O(000F38,F6,_,_,_,_,_,_  ), 0                         , 83 , 0  , 11530, 437, 56 ), // #1633
  INST(Wrssq            , X86Mr              , O(000F38,F6,_,_,1,_,_,_  ), 0                         , 237, 0  , 11536, 438, 56 ), // #1634
  INST(Wrussd           , X86Mr              , O(660F38,F5,_,_,_,_,_,_  ), 0                         , 2  , 0  , 11542, 437, 56 ), // #1635
  INST(Wrussq           , X86Mr              , O(660F38,F5,_,_,1,_,_,_  ), 0                         , 238, 0  , 11549, 438, 56 ), // #1636
  INST(Xabort           , X86Op_Mod11RM_I8   , O(000000,C6,7,_,_,_,_,_  ), 0                         , 27 , 0  , 11556, 80 , 165), // #1637
  INST(Xadd             , X86Xadd            , O(000F00,C0,_,_,x,_,_,_  ), 0                         , 4  , 0  , 11563, 439, 38 ), // #1638
  INST(Xbegin           , X86JmpRel          , O(000000,C7,7,_,_,_,_,_  ), 0                         , 27 , 0  , 11568, 440, 165), // #1639
  INST(Xchg             , X86Xchg            , O(000000,86,_,_,x,_,_,_  ), 0                         , 0  , 0  , 462  , 441, 0  ), // #1640
  INST(Xend             , X86Op              , O(000F01,D5,_,_,_,_,_,_  ), 0                         , 21 , 0  , 11575, 30 , 165), // #1641
  INST(Xgetbv           , X86Op              , O(000F01,D0,_,_,_,_,_,_  ), 0                         , 21 , 0  , 11580, 174, 166), // #1642
  INST(Xlatb            , X86Op              , O(000000,D7,_,_,_,_,_,_  ), 0                         , 0  , 0  , 11587, 30 , 0  ), // #1643
  INST(Xor              , X86Arith           , O(000000,30,6,_,x,_,_,_  ), 0                         , 32 , 0  , 10540, 179, 1  ), // #1644
  INST(Xorpd            , ExtRm              , O(660F00,57,_,_,_,_,_,_  ), 0                         , 3  , 0  , 11457, 151, 4  ), // #1645
  INST(Xorps            , ExtRm              , O(000F00,57,_,_,_,_,_,_  ), 0                         , 4  , 0  , 11464, 151, 5  ), // #1646
  INST(Xresldtrk        , X86Op              , O(F20F01,E9,_,_,_,_,_,_  ), 0                         , 92 , 0  , 11593, 30 , 167), // #1647
  INST(Xrstor           , X86M_Only_EDX_EAX  , O(000F00,AE,5,_,_,_,_,_  ), 0                         , 77 , 0  , 1164 , 442, 166), // #1648
  INST(Xrstor64         , X86M_Only_EDX_EAX  , O(000F00,AE,5,_,1,_,_,_  ), 0                         , 239, 0  , 1172 , 443, 166), // #1649
  INST(Xrstors          , X86M_Only_EDX_EAX  , O(000F00,C7,3,_,_,_,_,_  ), 0                         , 78 , 0  , 11603, 442, 168), // #1650
  INST(Xrstors64        , X86M_Only_EDX_EAX  , O(000F00,C7,3,_,1,_,_,_  ), 0                         , 240, 0  , 11611, 443, 168), // #1651
  INST(Xsave            , X86M_Only_EDX_EAX  , O(000F00,AE,4,_,_,_,_,_  ), 0                         , 97 , 0  , 1182 , 442, 166), // #1652
  INST(Xsave64          , X86M_Only_EDX_EAX  , O(000F00,AE,4,_,1,_,_,_  ), 0                         , 241, 0  , 1189 , 443, 166), // #1653
  INST(Xsavec           , X86M_Only_EDX_EAX  , O(000F00,C7,4,_,_,_,_,_  ), 0                         , 97 , 0  , 11621, 442, 169), // #1654
  INST(Xsavec64         , X86M_Only_EDX_EAX  , O(000F00,C7,4,_,1,_,_,_  ), 0                         , 241, 0  , 11628, 443, 169), // #1655
  INST(Xsaveopt         , X86M_Only_EDX_EAX  , O(000F00,AE,6,_,_,_,_,_  ), 0                         , 80 , 0  , 11637, 442, 170), // #1656
  INST(Xsaveopt64       , X86M_Only_EDX_EAX  , O(000F00,AE,6,_,1,_,_,_  ), 0                         , 242, 0  , 11646, 443, 170), // #1657
  INST(Xsaves           , X86M_Only_EDX_EAX  , O(000F00,C7,5,_,_,_,_,_  ), 0                         , 77 , 0  , 11657, 442, 168), // #1658
  INST(Xsaves64         , X86M_Only_EDX_EAX  , O(000F00,C7,5,_,1,_,_,_  ), 0                         , 239, 0  , 11664, 443, 168), // #1659
  INST(Xsetbv           , X86Op              , O(000F01,D1,_,_,_,_,_,_  ), 0                         , 21 , 0  , 11673, 174, 166), // #1660
  INST(Xsusldtrk        , X86Op              , O(F20F01,E8,_,_,_,_,_,_  ), 0                         , 92 , 0  , 11680, 30 , 167), // #1661
  INST(Xtest            , X86Op              , O(000F01,D6,_,_,_,_,_,_  ), 0                         , 21 , 0  , 11690, 30 , 171)  // #1662
  // ${InstInfo:End}
};
#undef NAME_DATA_INDEX
#undef INST

// x86::InstDB - Opcode Tables
// ===========================

// ${MainOpcodeTable:Begin}
// ------------------- Automatically generated, do not edit -------------------
const uint32_t InstDB::_mainOpcodeTable[] = {
  O(000000,00,0,0,0,0,0,0   ), // #0 [ref=56x]
  O(000000,00,2,0,0,0,0,0   ), // #1 [ref=4x]
  O(660F38,00,0,0,0,0,0,0   ), // #2 [ref=43x]
  O(660F00,00,0,0,0,0,0,0   ), // #3 [ref=38x]
  O(000F00,00,0,0,0,0,0,0   ), // #4 [ref=231x]
  O(F20F00,00,0,0,0,0,0,0   ), // #5 [ref=24x]
  O(F30F00,00,0,0,0,0,0,0   ), // #6 [ref=29x]
  O(F30F38,00,0,0,0,0,0,0   ), // #7 [ref=2x]
  O(660F3A,00,0,0,0,0,0,0   ), // #8 [ref=22x]
  O(000000,00,4,0,0,0,0,0   ), // #9 [ref=5x]
  V(000F38,00,0,0,0,0,0,None), // #10 [ref=6x]
  V(XOP_M9,00,1,0,0,0,0,None), // #11 [ref=3x]
  V(XOP_M9,00,6,0,0,0,0,None), // #12 [ref=2x]
  V(XOP_M9,00,5,0,0,0,0,None), // #13 [ref=1x]
  V(XOP_M9,00,3,0,0,0,0,None), // #14 [ref=1x]
  V(XOP_M9,00,2,0,0,0,0,None), // #15 [ref=1x]
  V(000F38,00,3,0,0,0,0,None), // #16 [ref=1x]
  V(000F38,00,2,0,0,0,0,None), // #17 [ref=1x]
  V(000F38,00,1,0,0,0,0,None), // #18 [ref=1x]
  O(660000,00,0,0,0,0,0,0   ), // #19 [ref=7x]
  O(000000,00,0,0,1,0,0,0   ), // #20 [ref=3x]
  O(000F01,00,0,0,0,0,0,0   ), // #21 [ref=29x]
  O(000F00,00,7,0,0,0,0,0   ), // #22 [ref=5x]
  O(660F00,00,7,0,0,0,0,0   ), // #23 [ref=1x]
  O(F30F00,00,6,0,0,0,0,0   ), // #24 [ref=4x]
  O(F30F01,00,0,0,0,0,0,0   ), // #25 [ref=9x]
  O(660F00,00,6,0,0,0,0,0   ), // #26 [ref=3x]
  O(000000,00,7,0,0,0,0,0   ), // #27 [ref=5x]
  O(000F00,00,1,0,1,0,0,0   ), // #28 [ref=2x]
  O(000F00,00,1,0,0,0,0,0   ), // #29 [ref=6x]
  O(F20F38,00,0,0,0,0,0,0   ), // #30 [ref=2x]
  O(000000,00,1,0,0,0,0,0   ), // #31 [ref=3x]
  O(000000,00,6,0,0,0,0,0   ), // #32 [ref=3x]
  O(F30F00,00,7,0,0,0,0,3   ), // #33 [ref=1x]
  O(F30F00,00,7,0,0,0,0,2   ), // #34 [ref=1x]
  O_FPU(00,D900,0)           , // #35 [ref=29x]
  O_FPU(00,C000,0)           , // #36 [ref=1x]
  O_FPU(00,DE00,0)           , // #37 [ref=7x]
  O_FPU(00,0000,4)           , // #38 [ref=4x]
  O_FPU(00,0000,6)           , // #39 [ref=4x]
  O_FPU(9B,DB00,0)           , // #40 [ref=2x]
  O_FPU(00,DA00,0)           , // #41 [ref=5x]
  O_FPU(00,DB00,0)           , // #42 [ref=8x]
  O_FPU(00,D000,2)           , // #43 [ref=1x]
  O_FPU(00,DF00,0)           , // #44 [ref=2x]
  O_FPU(00,D800,3)           , // #45 [ref=1x]
  O_FPU(00,F000,6)           , // #46 [ref=1x]
  O_FPU(00,F800,7)           , // #47 [ref=1x]
  O_FPU(00,DD00,0)           , // #48 [ref=3x]
  O_FPU(00,0000,0)           , // #49 [ref=4x]
  O_FPU(00,0000,2)           , // #50 [ref=3x]
  O_FPU(00,0000,3)           , // #51 [ref=3x]
  O_FPU(00,0000,7)           , // #52 [ref=3x]
  O_FPU(00,0000,1)           , // #53 [ref=2x]
  O_FPU(00,0000,5)           , // #54 [ref=2x]
  O_FPU(00,C800,1)           , // #55 [ref=1x]
  O_FPU(9B,0000,6)           , // #56 [ref=2x]
  O_FPU(9B,0000,7)           , // #57 [ref=2x]
  O_FPU(00,E000,4)           , // #58 [ref=1x]
  O_FPU(00,E800,5)           , // #59 [ref=1x]
  O(000F00,00,0,0,1,0,0,0   ), // #60 [ref=3x]
  O(F30F3A,00,0,0,0,0,0,0   ), // #61 [ref=1x]
  O(000000,00,5,0,0,0,0,0   ), // #62 [ref=4x]
  O(F30F00,00,5,0,0,0,0,0   ), // #63 [ref=2x]
  O(F30F00,00,5,0,1,0,0,0   ), // #64 [ref=1x]
  V(660F00,00,0,1,0,0,0,None), // #65 [ref=7x]
  V(660F00,00,0,1,1,0,0,None), // #66 [ref=6x]
  V(000F00,00,0,1,1,0,0,None), // #67 [ref=7x]
  V(000F00,00,0,1,0,0,0,None), // #68 [ref=8x]
  V(660F00,00,0,0,0,0,0,None), // #69 [ref=15x]
  V(660F00,00,0,0,1,0,0,None), // #70 [ref=4x]
  V(000F00,00,0,0,1,0,0,None), // #71 [ref=4x]
  V(000F00,00,0,0,0,0,0,None), // #72 [ref=10x]
  V(660F3A,00,0,0,0,0,0,None), // #73 [ref=47x]
  V(660F3A,00,0,0,1,0,0,None), // #74 [ref=4x]
  O(000000,00,3,0,0,0,0,0   ), // #75 [ref=4x]
  O(000F00,00,2,0,0,0,0,0   ), // #76 [ref=5x]
  O(000F00,00,5,0,0,0,0,0   ), // #77 [ref=4x]
  O(000F00,00,3,0,0,0,0,0   ), // #78 [ref=5x]
  V(XOP_M9,00,0,0,0,0,0,None), // #79 [ref=32x]
  O(000F00,00,6,0,0,0,0,0   ), // #80 [ref=5x]
  V(XOP_MA,00,0,0,0,0,0,None), // #81 [ref=1x]
  V(XOP_MA,00,1,0,0,0,0,None), // #82 [ref=1x]
  O(000F38,00,0,0,0,0,0,0   ), // #83 [ref=24x]
  V(F20F38,00,0,0,0,0,0,None), // #84 [ref=6x]
  O(000F3A,00,0,0,0,0,0,0   ), // #85 [ref=4x]
  O(F30000,00,0,0,0,0,0,0   ), // #86 [ref=1x]
  O(000F0F,00,0,0,0,0,0,0   ), // #87 [ref=26x]
  V(F30F38,00,0,0,0,0,0,None), // #88 [ref=5x]
  O(000F3A,00,0,0,1,0,0,0   ), // #89 [ref=1x]
  O(660F3A,00,0,0,1,0,0,0   ), // #90 [ref=1x]
  O(F30F00,00,4,0,0,0,0,0   ), // #91 [ref=1x]
  O(F20F01,00,0,0,0,0,0,0   ), // #92 [ref=4x]
  O(F30F00,00,1,0,0,0,0,0   ), // #93 [ref=3x]
  O(F30F00,00,7,0,0,0,0,0   ), // #94 [ref=1x]
  V(F20F3A,00,0,0,0,0,0,None), // #95 [ref=1x]
  V(660F38,00,0,0,0,0,0,None), // #96 [ref=26x]
  O(000F00,00,4,0,0,0,0,0   ), // #97 [ref=4x]
  V(XOP_M9,00,7,0,0,0,0,None), // #98 [ref=1x]
  V(XOP_M9,00,4,0,0,0,0,None), // #99 [ref=1x]
  O(F20F00,00,6,0,0,0,0,0   ), // #100 [ref=1x]
  E(F20F38,00,0,2,0,0,4,None), // #101 [ref=4x]
  E(F20F38,00,0,0,0,0,4,None), // #102 [ref=2x]
  V(660F00,00,0,0,0,1,4,ByLL), // #103 [ref=25x]
  E(00MAP5,00,0,0,0,0,4,ByLL), // #104 [ref=10x]
  V(000F00,00,0,0,0,0,4,ByLL), // #105 [ref=19x]
  V(F20F00,00,0,0,0,1,3,None), // #106 [ref=10x]
  E(F3MAP5,00,0,0,0,0,1,None), // #107 [ref=13x]
  V(F30F00,00,0,0,0,0,2,None), // #108 [ref=12x]
  V(F20F00,00,0,0,0,0,0,None), // #109 [ref=4x]
  V(660F38,00,0,0,0,0,4,ByLL), // #110 [ref=50x]
  E(660F3A,00,0,0,0,0,4,ByLL), // #111 [ref=17x]
  E(660F3A,00,0,0,0,1,4,ByLL), // #112 [ref=18x]
  E(660F38,00,0,0,0,1,4,ByLL), // #113 [ref=40x]
  E(660F38,00,0,0,0,0,4,ByLL), // #114 [ref=25x]
  V(660F38,00,0,1,0,0,0,None), // #115 [ref=2x]
  E(660F38,00,0,0,0,0,3,None), // #116 [ref=2x]
  E(660F38,00,0,0,0,0,4,None), // #117 [ref=2x]
  E(660F38,00,0,2,0,0,5,None), // #118 [ref=2x]
  E(660F38,00,0,0,0,1,4,None), // #119 [ref=2x]
  E(660F38,00,0,2,0,1,5,None), // #120 [ref=2x]
  V(660F38,00,0,0,0,1,3,None), // #121 [ref=2x]
  V(660F38,00,0,0,0,0,2,None), // #122 [ref=14x]
  E(000F3A,00,0,0,0,0,4,ByLL), // #123 [ref=5x]
  E(F30F3A,00,0,0,0,0,1,None), // #124 [ref=1x]
  V(660F00,00,0,0,0,1,3,None), // #125 [ref=5x]
  E(00MAP5,00,0,0,0,0,1,None), // #126 [ref=2x]
  V(000F00,00,0,0,0,0,2,None), // #127 [ref=2x]
  E(660F38,00,0,0,0,1,3,None), // #128 [ref=14x]
  E(660F38,00,0,0,0,0,2,None), // #129 [ref=14x]
  V(F30F00,00,0,0,0,0,3,ByLL), // #130 [ref=1x]
  E(F20F38,00,0,0,0,0,4,ByLL), // #131 [ref=2x]
  E(F30F38,00,0,0,0,0,4,ByLL), // #132 [ref=4x]
  V(F20F00,00,0,0,0,1,4,ByLL), // #133 [ref=1x]
  E(66MAP5,00,0,0,0,1,4,ByLL), // #134 [ref=1x]
  E(660F00,00,0,0,0,1,4,ByLL), // #135 [ref=10x]
  E(000F00,00,0,0,0,1,4,ByLL), // #136 [ref=3x]
  E(66MAP5,00,0,0,0,0,3,ByLL), // #137 [ref=1x]
  E(00MAP5,00,0,0,0,0,2,ByLL), // #138 [ref=1x]
  V(660F38,00,0,0,0,0,3,ByLL), // #139 [ref=7x]
  E(66MAP6,00,0,0,0,0,3,ByLL), // #140 [ref=1x]
  E(66MAP5,00,0,0,0,0,2,ByLL), // #141 [ref=4x]
  E(00MAP5,00,0,0,0,0,3,ByLL), // #142 [ref=2x]
  E(66MAP5,00,0,0,0,0,4,ByLL), // #143 [ref=3x]
  V(660F00,00,0,0,0,0,4,ByLL), // #144 [ref=43x]
  V(000F00,00,0,0,0,0,3,ByLL), // #145 [ref=1x]
  V(660F3A,00,0,0,0,0,3,ByLL), // #146 [ref=1x]
  E(660F00,00,0,0,0,0,3,ByLL), // #147 [ref=4x]
  E(000F00,00,0,0,0,0,4,ByLL), // #148 [ref=2x]
  E(F30F00,00,0,0,0,1,4,ByLL), // #149 [ref=3x]
  E(00MAP5,00,0,0,0,1,4,ByLL), // #150 [ref=1x]
  E(F2MAP5,00,0,0,0,1,3,None), // #151 [ref=1x]
  V(F20F00,00,0,0,0,0,3,None), // #152 [ref=2x]
  E(F20F00,00,0,0,0,0,3,None), // #153 [ref=2x]
  E(00MAP6,00,0,0,0,0,1,None), // #154 [ref=1x]
  V(F20F00,00,0,0,0,0,2,T1W ), // #155 [ref=1x]
  E(F3MAP5,00,0,0,0,0,2,T1W ), // #156 [ref=2x]
  V(F30F00,00,0,0,0,0,2,T1W ), // #157 [ref=1x]
  E(00MAP5,00,0,0,0,0,2,None), // #158 [ref=1x]
  E(F30F00,00,0,0,0,0,2,None), // #159 [ref=2x]
  E(F3MAP5,00,0,0,0,0,3,ByLL), // #160 [ref=1x]
  V(F30F00,00,0,0,0,0,4,ByLL), // #161 [ref=4x]
  E(F30F00,00,0,0,0,0,3,ByLL), // #162 [ref=1x]
  E(F2MAP5,00,0,0,0,0,4,ByLL), // #163 [ref=2x]
  E(F20F00,00,0,0,0,0,4,ByLL), // #164 [ref=2x]
  E(F2MAP5,00,0,0,0,1,4,ByLL), // #165 [ref=1x]
  E(F20F00,00,0,0,0,1,4,ByLL), // #166 [ref=2x]
  E(F20F00,00,0,0,0,0,2,T1W ), // #167 [ref=1x]
  E(F30F00,00,0,0,0,0,2,T1W ), // #168 [ref=1x]
  E(F3MAP5,00,0,0,0,0,4,ByLL), // #169 [ref=1x]
  E(660F38,00,0,2,0,1,4,ByLL), // #170 [ref=3x]
  E(660F38,00,0,2,0,0,4,ByLL), // #171 [ref=3x]
  V(660F3A,00,0,1,0,0,0,None), // #172 [ref=6x]
  E(660F3A,00,0,0,0,0,4,None), // #173 [ref=4x]
  E(660F3A,00,0,2,0,0,5,None), // #174 [ref=4x]
  E(660F3A,00,0,0,0,1,4,None), // #175 [ref=4x]
  E(660F3A,00,0,2,0,1,5,None), // #176 [ref=4x]
  V(660F3A,00,0,0,0,0,2,None), // #177 [ref=4x]
  E(F2MAP6,00,0,0,0,0,4,ByLL), // #178 [ref=2x]
  E(F2MAP6,00,0,0,0,0,2,None), // #179 [ref=2x]
  E(660F3A,00,0,0,0,1,3,None), // #180 [ref=6x]
  E(660F3A,00,0,0,0,0,2,None), // #181 [ref=6x]
  V(660F38,00,0,0,1,1,4,ByLL), // #182 [ref=20x]
  E(66MAP6,00,0,0,0,0,4,ByLL), // #183 [ref=22x]
  V(660F38,00,0,0,1,1,3,None), // #184 [ref=12x]
  E(66MAP6,00,0,0,0,0,1,None), // #185 [ref=16x]
  E(F3MAP6,00,0,0,0,0,4,ByLL), // #186 [ref=2x]
  E(F3MAP6,00,0,0,0,0,2,None), // #187 [ref=2x]
  E(000F3A,00,0,0,0,0,1,None), // #188 [ref=4x]
  V(660F38,00,0,0,1,0,0,None), // #189 [ref=5x]
  E(660F38,00,1,2,0,1,3,None), // #190 [ref=2x]
  E(660F38,00,1,2,0,0,2,None), // #191 [ref=2x]
  E(660F38,00,2,2,0,1,3,None), // #192 [ref=2x]
  E(660F38,00,2,2,0,0,2,None), // #193 [ref=2x]
  V(660F3A,00,0,0,1,1,4,ByLL), // #194 [ref=2x]
  V(000F00,00,2,0,0,0,0,None), // #195 [ref=1x]
  V(660F00,00,0,0,0,0,2,None), // #196 [ref=1x]
  V(F20F00,00,0,0,0,1,3,DUP ), // #197 [ref=1x]
  E(660F00,00,0,0,0,0,4,ByLL), // #198 [ref=6x]
  V(F30F00,00,0,0,0,0,0,None), // #199 [ref=3x]
  E(F30F00,00,0,0,0,0,4,ByLL), // #200 [ref=1x]
  V(000F00,00,0,0,0,0,3,None), // #201 [ref=2x]
  E(66MAP5,00,0,0,0,0,1,None), // #202 [ref=1x]
  E(F20F38,00,0,0,0,1,4,ByLL), // #203 [ref=1x]
  V(660F3A,00,0,0,0,0,4,ByLL), // #204 [ref=2x]
  E(F30F38,00,0,0,0,1,0,None), // #205 [ref=5x]
  E(F30F38,00,0,0,0,0,0,None), // #206 [ref=5x]
  V(660F38,00,0,0,0,0,1,None), // #207 [ref=1x]
  V(XOP_M8,00,0,0,0,0,0,None), // #208 [ref=22x]
  V(660F38,00,0,0,0,1,4,ByLL), // #209 [ref=4x]
  E(660F38,00,0,0,0,0,0,None), // #210 [ref=2x]
  E(660F38,00,0,0,0,1,1,None), // #211 [ref=2x]
  E(660F38,00,0,0,1,1,4,ByLL), // #212 [ref=1x]
  V(660F3A,00,0,0,1,1,3,None), // #213 [ref=2x]
  V(660F3A,00,0,0,0,0,1,None), // #214 [ref=1x]
  V(660F00,00,0,0,0,0,1,None), // #215 [ref=1x]
  E(F30F38,00,0,0,0,0,2,ByLL), // #216 [ref=6x]
  E(F30F38,00,0,0,0,0,3,ByLL), // #217 [ref=9x]
  E(F30F38,00,0,0,0,0,1,ByLL), // #218 [ref=3x]
  V(660F38,00,0,0,0,0,2,ByLL), // #219 [ref=4x]
  V(660F38,00,0,0,0,0,1,ByLL), // #220 [ref=2x]
  E(660F00,00,1,0,0,0,4,ByLL), // #221 [ref=1x]
  E(660F00,00,1,0,0,1,4,ByLL), // #222 [ref=1x]
  V(F20F00,00,0,0,0,0,4,ByLL), // #223 [ref=1x]
  V(660F00,00,0,0,0,0,4,None), // #224 [ref=6x]
  V(660F00,00,7,0,0,0,4,ByLL), // #225 [ref=1x]
  V(660F00,00,0,0,0,1,4,None), // #226 [ref=2x]
  E(660F00,00,0,0,0,1,4,None), // #227 [ref=1x]
  V(660F00,00,3,0,0,0,4,ByLL), // #228 [ref=1x]
  E(F30F38,00,0,0,0,1,4,ByLL), // #229 [ref=2x]
  E(660F38,00,5,2,0,1,3,None), // #230 [ref=2x]
  E(660F38,00,5,2,0,0,2,None), // #231 [ref=2x]
  E(660F38,00,6,2,0,1,3,None), // #232 [ref=2x]
  E(660F38,00,6,2,0,0,2,None), // #233 [ref=2x]
  V(000F00,00,3,0,0,0,0,None), // #234 [ref=1x]
  O(F30F00,00,2,0,0,0,0,0   ), // #235 [ref=1x]
  O(F30F00,00,3,0,0,0,0,0   ), // #236 [ref=1x]
  O(000F38,00,0,0,1,0,0,0   ), // #237 [ref=1x]
  O(660F38,00,0,0,1,0,0,0   ), // #238 [ref=1x]
  O(000F00,00,5,0,1,0,0,0   ), // #239 [ref=2x]
  O(000F00,00,3,0,1,0,0,0   ), // #240 [ref=1x]
  O(000F00,00,4,0,1,0,0,0   ), // #241 [ref=2x]
  O(000F00,00,6,0,1,0,0,0   )  // #242 [ref=1x]
};
// ----------------------------------------------------------------------------
// ${MainOpcodeTable:End}

// ${AltOpcodeTable:Begin}
// ------------------- Automatically generated, do not edit -------------------
const uint32_t InstDB::_altOpcodeTable[] = {
  O(000000,00,0,0,0,0,0,0   ), // #0 [ref=1514x]
  O(660F00,1B,0,0,0,0,0,0   ), // #1 [ref=1x]
  O(000F00,BA,4,0,0,0,0,0   ), // #2 [ref=1x]
  O(000F00,BA,7,0,0,0,0,0   ), // #3 [ref=1x]
  O(000F00,BA,6,0,0,0,0,0   ), // #4 [ref=1x]
  O(000F00,BA,5,0,0,0,0,0   ), // #5 [ref=1x]
  O(000000,48,0,0,0,0,0,0   ), // #6 [ref=1x]
  O(660F00,78,0,0,0,0,0,0   ), // #7 [ref=1x]
  O_FPU(00,00DF,5)           , // #8 [ref=1x]
  O_FPU(00,00DF,7)           , // #9 [ref=1x]
  O_FPU(00,00DD,1)           , // #10 [ref=1x]
  O_FPU(00,00DB,5)           , // #11 [ref=1x]
  O_FPU(00,DFE0,0)           , // #12 [ref=1x]
  O(000000,DB,7,0,0,0,0,0   ), // #13 [ref=1x]
  O_FPU(9B,DFE0,0)           , // #14 [ref=1x]
  O(000000,E4,0,0,0,0,0,0   ), // #15 [ref=1x]
  O(000000,40,0,0,0,0,0,0   ), // #16 [ref=1x]
  O(F20F00,78,0,0,0,0,0,0   ), // #17 [ref=1x]
  O(000000,77,0,0,0,0,0,0   ), // #18 [ref=2x]
  O(000000,73,0,0,0,0,0,0   ), // #19 [ref=3x]
  O(000000,72,0,0,0,0,0,0   ), // #20 [ref=3x]
  O(000000,76,0,0,0,0,0,0   ), // #21 [ref=2x]
  O(000000,74,0,0,0,0,0,0   ), // #22 [ref=2x]
  O(000000,E3,0,0,0,0,0,0   ), // #23 [ref=1x]
  O(000000,7F,0,0,0,0,0,0   ), // #24 [ref=2x]
  O(000000,7D,0,0,0,0,0,0   ), // #25 [ref=2x]
  O(000000,7C,0,0,0,0,0,0   ), // #26 [ref=2x]
  O(000000,7E,0,0,0,0,0,0   ), // #27 [ref=2x]
  O(000000,EB,0,0,0,0,0,0   ), // #28 [ref=1x]
  O(000000,75,0,0,0,0,0,0   ), // #29 [ref=2x]
  O(000000,71,0,0,0,0,0,0   ), // #30 [ref=1x]
  O(000000,7B,0,0,0,0,0,0   ), // #31 [ref=2x]
  O(000000,79,0,0,0,0,0,0   ), // #32 [ref=1x]
  O(000000,70,0,0,0,0,0,0   ), // #33 [ref=1x]
  O(000000,7A,0,0,0,0,0,0   ), // #34 [ref=2x]
  O(000000,78,0,0,0,0,0,0   ), // #35 [ref=1x]
  V(660F00,92,0,0,0,0,0,None), // #36 [ref=1x]
  V(F20F00,92,0,0,0,0,0,None), // #37 [ref=1x]
  V(F20F00,92,0,0,1,0,0,None), // #38 [ref=1x]
  V(000F00,92,0,0,0,0,0,None), // #39 [ref=1x]
  O(000000,9A,0,0,0,0,0,0   ), // #40 [ref=1x]
  O(000000,EA,0,0,0,0,0,0   ), // #41 [ref=1x]
  O(000000,E2,0,0,0,0,0,0   ), // #42 [ref=1x]
  O(000000,E1,0,0,0,0,0,0   ), // #43 [ref=1x]
  O(000000,E0,0,0,0,0,0,0   ), // #44 [ref=1x]
  O(660F00,29,0,0,0,0,0,0   ), // #45 [ref=1x]
  O(000F00,29,0,0,0,0,0,0   ), // #46 [ref=1x]
  O(000F38,F1,0,0,0,0,0,0   ), // #47 [ref=1x]
  O(000F00,7E,0,0,0,0,0,0   ), // #48 [ref=2x]
  O(660F00,7F,0,0,0,0,0,0   ), // #49 [ref=1x]
  O(F30F00,7F,0,0,0,0,0,0   ), // #50 [ref=1x]
  O(660F00,17,0,0,0,0,0,0   ), // #51 [ref=1x]
  O(000F00,17,0,0,0,0,0,0   ), // #52 [ref=1x]
  O(660F00,13,0,0,0,0,0,0   ), // #53 [ref=1x]
  O(000F00,13,0,0,0,0,0,0   ), // #54 [ref=1x]
  O(660F00,E7,0,0,0,0,0,0   ), // #55 [ref=1x]
  O(660F00,2B,0,0,0,0,0,0   ), // #56 [ref=1x]
  O(000F00,2B,0,0,0,0,0,0   ), // #57 [ref=1x]
  O(000F00,E7,0,0,0,0,0,0   ), // #58 [ref=1x]
  O(F20F00,2B,0,0,0,0,0,0   ), // #59 [ref=1x]
  O(F30F00,2B,0,0,0,0,0,0   ), // #60 [ref=1x]
  O(F20F00,11,0,0,0,0,0,0   ), // #61 [ref=1x]
  O(F30F00,11,0,0,0,0,0,0   ), // #62 [ref=1x]
  O(660F00,11,0,0,0,0,0,0   ), // #63 [ref=1x]
  O(000F00,11,0,0,0,0,0,0   ), // #64 [ref=1x]
  O(000000,E6,0,0,0,0,0,0   ), // #65 [ref=1x]
  O(000F3A,15,0,0,0,0,0,0   ), // #66 [ref=1x]
  O(000000,58,0,0,0,0,0,0   ), // #67 [ref=1x]
  O(000F00,72,6,0,0,0,0,0   ), // #68 [ref=1x]
  O(660F00,73,7,0,0,0,0,0   ), // #69 [ref=1x]
  O(000F00,73,6,0,0,0,0,0   ), // #70 [ref=1x]
  O(000F00,71,6,0,0,0,0,0   ), // #71 [ref=1x]
  O(000F00,72,4,0,0,0,0,0   ), // #72 [ref=1x]
  O(000F00,71,4,0,0,0,0,0   ), // #73 [ref=1x]
  O(000F00,72,2,0,0,0,0,0   ), // #74 [ref=1x]
  O(660F00,73,3,0,0,0,0,0   ), // #75 [ref=1x]
  O(000F00,73,2,0,0,0,0,0   ), // #76 [ref=1x]
  O(000F00,71,2,0,0,0,0,0   ), // #77 [ref=1x]
  O(000000,50,0,0,0,0,0,0   ), // #78 [ref=1x]
  O(000000,F6,0,0,0,0,0,0   ), // #79 [ref=1x]
  E(660F38,92,0,0,0,1,3,None), // #80 [ref=1x]
  E(660F38,92,0,0,0,0,2,None), // #81 [ref=1x]
  E(660F38,93,0,0,0,1,3,None), // #82 [ref=1x]
  E(660F38,93,0,0,0,0,2,None), // #83 [ref=1x]
  V(660F38,2F,0,0,0,0,0,None), // #84 [ref=1x]
  V(660F38,2E,0,0,0,0,0,None), // #85 [ref=1x]
  V(660F00,29,0,0,0,1,4,ByLL), // #86 [ref=1x]
  V(000F00,29,0,0,0,0,4,ByLL), // #87 [ref=1x]
  V(660F00,7E,0,0,0,0,2,None), // #88 [ref=1x]
  V(660F00,7F,0,0,0,0,0,None), // #89 [ref=1x]
  E(660F00,7F,0,0,0,0,4,ByLL), // #90 [ref=1x]
  E(660F00,7F,0,0,0,1,4,ByLL), // #91 [ref=1x]
  V(F30F00,7F,0,0,0,0,0,None), // #92 [ref=1x]
  E(F20F00,7F,0,0,0,1,4,ByLL), // #93 [ref=1x]
  E(F30F00,7F,0,0,0,0,4,ByLL), // #94 [ref=1x]
  E(F30F00,7F,0,0,0,1,4,ByLL), // #95 [ref=1x]
  E(F20F00,7F,0,0,0,0,4,ByLL), // #96 [ref=1x]
  V(660F00,17,0,0,0,1,3,None), // #97 [ref=1x]
  V(000F00,17,0,0,0,0,3,None), // #98 [ref=1x]
  V(660F00,13,0,0,0,1,3,None), // #99 [ref=1x]
  V(000F00,13,0,0,0,0,3,None), // #100 [ref=1x]
  V(660F00,7E,0,0,0,1,3,None), // #101 [ref=1x]
  V(F20F00,11,0,0,0,1,3,None), // #102 [ref=1x]
  E(F3MAP5,11,0,0,0,0,1,None), // #103 [ref=1x]
  V(F30F00,11,0,0,0,0,2,None), // #104 [ref=1x]
  V(660F00,11,0,0,0,1,4,ByLL), // #105 [ref=1x]
  V(000F00,11,0,0,0,0,4,ByLL), // #106 [ref=1x]
  E(66MAP5,7E,0,0,0,0,1,None), // #107 [ref=1x]
  E(660F38,7A,0,0,0,0,0,None), // #108 [ref=1x]
  E(660F38,7C,0,0,0,0,0,None), // #109 [ref=1x]
  E(660F38,7C,0,0,0,1,0,None), // #110 [ref=1x]
  E(660F38,7B,0,0,0,0,0,None), // #111 [ref=1x]
  V(660F3A,05,0,0,0,1,4,ByLL), // #112 [ref=1x]
  V(660F3A,04,0,0,0,0,4,ByLL), // #113 [ref=1x]
  V(660F3A,01,0,0,1,1,4,ByLL), // #114 [ref=1x]
  V(660F3A,00,0,0,1,1,4,ByLL), // #115 [ref=1x]
  E(660F38,90,0,0,0,0,2,None), // #116 [ref=1x]
  E(660F38,90,0,0,0,1,3,None), // #117 [ref=1x]
  E(660F38,91,0,0,0,0,2,None), // #118 [ref=1x]
  E(660F38,91,0,0,0,1,3,None), // #119 [ref=1x]
  V(660F38,8E,0,0,0,0,0,None), // #120 [ref=1x]
  V(660F38,8E,0,0,1,0,0,None), // #121 [ref=1x]
  V(XOP_M8,C0,0,0,0,0,0,None), // #122 [ref=1x]
  V(XOP_M8,C2,0,0,0,0,0,None), // #123 [ref=1x]
  V(XOP_M8,C3,0,0,0,0,0,None), // #124 [ref=1x]
  V(XOP_M8,C1,0,0,0,0,0,None), // #125 [ref=1x]
  V(660F00,72,6,0,0,0,4,ByLL), // #126 [ref=1x]
  V(660F00,73,6,0,0,1,4,ByLL), // #127 [ref=1x]
  V(660F00,71,6,0,0,0,4,ByLL), // #128 [ref=1x]
  V(660F00,72,4,0,0,0,4,ByLL), // #129 [ref=1x]
  E(660F00,72,4,0,0,1,4,ByLL), // #130 [ref=1x]
  V(660F00,71,4,0,0,0,4,ByLL), // #131 [ref=1x]
  V(660F00,72,2,0,0,0,4,ByLL), // #132 [ref=1x]
  V(660F00,73,2,0,0,1,4,ByLL), // #133 [ref=1x]
  V(660F00,71,2,0,0,0,4,ByLL)  // #134 [ref=1x]
};
// ----------------------------------------------------------------------------
// ${AltOpcodeTable:End}

#undef O
#undef V
#undef E
#undef O_FPU

// x86::InstDB - CommonInfoTable
// =============================

// ${InstCommonTable:Begin}
// ------------------- Automatically generated, do not edit -------------------
#define F(VAL) uint32_t(InstDB::InstFlags::k##VAL)
#define X(VAL) uint32_t(InstDB::Avx512Flags::k##VAL)
#define CONTROL_FLOW(VAL) uint8_t(InstControlFlow::k##VAL)
#define SAME_REG_HINT(VAL) uint8_t(InstSameRegHint::k##VAL)
const InstDB::CommonInfo InstDB::_commonInfoTable[] = {
  { 0                                                 , 0                             , 0  , 0 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #0 [ref=1x]
  { 0                                                 , 0                             , 383, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #1 [ref=4x]
  { 0                                                 , 0                             , 384, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #2 [ref=2x]
  { F(Lock)|F(XAcquire)|F(XRelease)                   , 0                             , 16 , 12, CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #3 [ref=2x]
  { 0                                                 , 0                             , 180, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #4 [ref=2x]
  { F(Vec)                                            , 0                             , 79 , 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #5 [ref=54x]
  { F(Vec)                                            , 0                             , 106, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #6 [ref=19x]
  { F(Vec)                                            , 0                             , 212, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #7 [ref=16x]
  { F(Vec)                                            , 0                             , 221, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #8 [ref=20x]
  { F(Lock)|F(XAcquire)|F(XRelease)                   , 0                             , 28 , 11, CONTROL_FLOW(Regular), SAME_REG_HINT(RO)}, // #9 [ref=1x]
  { F(Vex)                                            , 0                             , 275, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #10 [ref=3x]
  { F(Vec)                                            , 0                             , 79 , 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(RO)}, // #11 [ref=12x]
  { 0                                                 , 0                             , 385, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #12 [ref=1x]
  { F(Vex)                                            , 0                             , 277, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #13 [ref=5x]
  { F(Vex)                                            , 0                             , 180, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #14 [ref=12x]
  { F(Vec)                                            , 0                             , 386, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #15 [ref=4x]
  { 0                                                 , 0                             , 279, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #16 [ref=3x]
  { F(Mib)                                            , 0                             , 387, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #17 [ref=1x]
  { 0                                                 , 0                             , 388, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #18 [ref=1x]
  { 0                                                 , 0                             , 281, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #19 [ref=1x]
  { F(Mib)                                            , 0                             , 389, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #20 [ref=1x]
  { 0                                                 , 0                             , 283, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #21 [ref=1x]
  { 0                                                 , 0                             , 179, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #22 [ref=35x]
  { 0                                                 , 0                             , 390, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #23 [ref=3x]
  { 0                                                 , 0                             , 123, 4 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #24 [ref=1x]
  { F(Lock)|F(XAcquire)|F(XRelease)                   , 0                             , 123, 4 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #25 [ref=3x]
  { F(Rep)|F(RepIgnored)                              , 0                             , 285, 2 , CONTROL_FLOW(Call), SAME_REG_HINT(None)}, // #26 [ref=1x]
  { 0                                                 , 0                             , 391, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #27 [ref=1x]
  { 0                                                 , 0                             , 392, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #28 [ref=2x]
  { 0                                                 , 0                             , 364, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #29 [ref=1x]
  { 0                                                 , 0                             , 108, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #30 [ref=83x]
  { 0                                                 , 0                             , 393, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #31 [ref=11x]
  { 0                                                 , 0                             , 394, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #32 [ref=6x]
  { 0                                                 , 0                             , 395, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #33 [ref=13x]
  { 0                                                 , 0                             , 396, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #34 [ref=1x]
  { 0                                                 , 0                             , 16 , 12, CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #35 [ref=1x]
  { F(Rep)                                            , 0                             , 127, 4 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #36 [ref=1x]
  { F(Vec)                                            , 0                             , 397, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #37 [ref=2x]
  { F(Vec)                                            , 0                             , 398, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #38 [ref=3x]
  { F(Lock)|F(XAcquire)|F(XRelease)                   , 0                             , 131, 4 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #39 [ref=1x]
  { F(Lock)|F(XAcquire)|F(XRelease)                   , 0                             , 399, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #40 [ref=1x]
  { F(Lock)|F(XAcquire)|F(XRelease)                   , 0                             , 400, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #41 [ref=1x]
  { 0                                                 , 0                             , 401, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #42 [ref=1x]
  { 0                                                 , 0                             , 402, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #43 [ref=1x]
  { 0                                                 , 0                             , 287, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #44 [ref=1x]
  { F(Mmx)|F(Vec)                                     , 0                             , 403, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #45 [ref=2x]
  { F(Mmx)|F(Vec)                                     , 0                             , 404, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #46 [ref=2x]
  { F(Mmx)|F(Vec)                                     , 0                             , 405, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #47 [ref=2x]
  { F(Vec)                                            , 0                             , 406, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #48 [ref=2x]
  { F(Vec)                                            , 0                             , 407, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #49 [ref=2x]
  { F(Vec)                                            , 0                             , 408, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #50 [ref=2x]
  { 0                                                 , 0                             , 409, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #51 [ref=1x]
  { 0                                                 , 0                             , 410, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #52 [ref=2x]
  { F(Lock)|F(XAcquire)|F(XRelease)                   , 0                             , 289, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #53 [ref=2x]
  { 0                                                 , 0                             , 39 , 4 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #54 [ref=3x]
  { F(Mmx)                                            , 0                             , 108, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #55 [ref=1x]
  { 0                                                 , 0                             , 291, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #56 [ref=2x]
  { 0                                                 , 0                             , 411, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #57 [ref=1x]
  { F(Vec)                                            , 0                             , 412, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #58 [ref=2x]
  { F(Vec)                                            , 0                             , 293, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #59 [ref=1x]
  { F(FpuM32)|F(FpuM64)                               , 0                             , 182, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #60 [ref=6x]
  { 0                                                 , 0                             , 295, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #61 [ref=9x]
  { F(FpuM80)                                         , 0                             , 413, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #62 [ref=2x]
  { 0                                                 , 0                             , 296, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #63 [ref=13x]
  { F(FpuM32)|F(FpuM64)                               , 0                             , 297, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #64 [ref=2x]
  { F(FpuM16)|F(FpuM32)                               , 0                             , 414, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #65 [ref=9x]
  { F(FpuM16)|F(FpuM32)|F(FpuM64)                     , 0                             , 415, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #66 [ref=3x]
  { F(FpuM32)|F(FpuM64)|F(FpuM80)                     , 0                             , 416, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #67 [ref=2x]
  { F(FpuM16)                                         , 0                             , 417, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #68 [ref=3x]
  { 0                                                 , 0                             , 418, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #69 [ref=13x]
  { F(FpuM16)                                         , 0                             , 419, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #70 [ref=2x]
  { F(FpuM32)|F(FpuM64)                               , 0                             , 298, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #71 [ref=1x]
  { 0                                                 , 0                             , 420, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #72 [ref=2x]
  { 0                                                 , 0                             , 421, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #73 [ref=1x]
  { 0                                                 , 0                             , 39 , 10, CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #74 [ref=1x]
  { 0                                                 , 0                             , 422, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #75 [ref=1x]
  { 0                                                 , 0                             , 423, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #76 [ref=2x]
  { 0                                                 , 0                             , 348, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #77 [ref=3x]
  { F(Rep)                                            , 0                             , 424, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #78 [ref=1x]
  { F(Vec)                                            , 0                             , 299, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #79 [ref=1x]
  { 0                                                 , 0                             , 425, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #80 [ref=2x]
  { 0                                                 , 0                             , 426, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #81 [ref=8x]
  { 0                                                 , 0                             , 301, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #82 [ref=3x]
  { 0                                                 , 0                             , 303, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #83 [ref=1x]
  { 0                                                 , 0                             , 108, 1 , CONTROL_FLOW(Return), SAME_REG_HINT(None)}, // #84 [ref=2x]
  { 0                                                 , 0                             , 395, 1 , CONTROL_FLOW(Return), SAME_REG_HINT(None)}, // #85 [ref=1x]
  { F(Rep)|F(RepIgnored)                              , 0                             , 305, 2 , CONTROL_FLOW(Branch), SAME_REG_HINT(None)}, // #86 [ref=30x]
  { F(Rep)|F(RepIgnored)                              , 0                             , 307, 2 , CONTROL_FLOW(Branch), SAME_REG_HINT(None)}, // #87 [ref=1x]
  { F(Rep)|F(RepIgnored)                              , 0                             , 309, 2 , CONTROL_FLOW(Jump), SAME_REG_HINT(None)}, // #88 [ref=1x]
  { F(Vex)                                            , 0                             , 427, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #89 [ref=19x]
  { F(Vex)                                            , 0                             , 311, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #90 [ref=1x]
  { F(Vex)                                            , 0                             , 313, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #91 [ref=1x]
  { F(Vex)                                            , 0                             , 315, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #92 [ref=1x]
  { F(Vex)                                            , 0                             , 317, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #93 [ref=1x]
  { F(Vex)                                            , 0                             , 428, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #94 [ref=12x]
  { F(Vex)                                            , 0                             , 429, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #95 [ref=8x]
  { F(Vex)                                            , 0                             , 427, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(WO)}, // #96 [ref=8x]
  { 0                                                 , 0                             , 430, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #97 [ref=2x]
  { 0                                                 , 0                             , 319, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #98 [ref=1x]
  { 0                                                 , 0                             , 321, 2 , CONTROL_FLOW(Call), SAME_REG_HINT(None)}, // #99 [ref=1x]
  { F(Vec)                                            , 0                             , 230, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #100 [ref=2x]
  { 0                                                 , 0                             , 431, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #101 [ref=2x]
  { 0                                                 , 0                             , 323, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #102 [ref=2x]
  { F(Vex)                                            , 0                             , 432, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #103 [ref=2x]
  { 0                                                 , 0                             , 433, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #104 [ref=1x]
  { 0                                                 , 0                             , 185, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #105 [ref=3x]
  { 0                                                 , 0                             , 321, 2 , CONTROL_FLOW(Jump), SAME_REG_HINT(None)}, // #106 [ref=1x]
  { 0                                                 , 0                             , 434, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #107 [ref=5x]
  { F(Vex)                                            , 0                             , 435, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #108 [ref=2x]
  { F(Rep)                                            , 0                             , 135, 4 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #109 [ref=1x]
  { 0                                                 , 0                             , 307, 2 , CONTROL_FLOW(Branch), SAME_REG_HINT(None)}, // #110 [ref=3x]
  { 0                                                 , 0                             , 325, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #111 [ref=1x]
  { F(Vex)                                            , 0                             , 436, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #112 [ref=2x]
  { F(Vec)                                            , 0                             , 437, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #113 [ref=1x]
  { F(Mmx)                                            , 0                             , 438, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #114 [ref=1x]
  { 0                                                 , 0                             , 439, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #115 [ref=2x]
  { F(XRelease)                                       , 0                             , 0  , 16, CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #116 [ref=1x]
  { 0                                                 , 0                             , 49 , 9 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #117 [ref=1x]
  { F(Vec)                                            , 0                             , 79 , 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #118 [ref=6x]
  { 0                                                 , 0                             , 73 , 6 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #119 [ref=1x]
  { F(Mmx)|F(Vec)                                     , 0                             , 327, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #120 [ref=1x]
  { 0                                                 , 0                             , 440, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #121 [ref=1x]
  { 0                                                 , 0                             , 77 , 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #122 [ref=2x]
  { F(Mmx)|F(Vec)                                     , 0                             , 441, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #123 [ref=1x]
  { F(Vec)                                            , 0                             , 294, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #124 [ref=2x]
  { F(Vec)                                            , 0                             , 236, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #125 [ref=4x]
  { F(Vec)                                            , 0                             , 442, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #126 [ref=2x]
  { F(Vec)                                            , 0                             , 80 , 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #127 [ref=3x]
  { F(Mmx)                                            , 0                             , 443, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #128 [ref=1x]
  { F(Vec)                                            , 0                             , 107, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #129 [ref=1x]
  { F(Vec)                                            , 0                             , 242, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #130 [ref=1x]
  { F(Mmx)|F(Vec)                                     , 0                             , 103, 5 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #131 [ref=1x]
  { F(Mmx)|F(Vec)                                     , 0                             , 444, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #132 [ref=1x]
  { F(Rep)                                            , 0                             , 139, 4 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #133 [ref=1x]
  { F(Vec)                                            , 0                             , 106, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #134 [ref=1x]
  { F(Vec)                                            , 0                             , 329, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #135 [ref=1x]
  { 0                                                 , 0                             , 331, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #136 [ref=2x]
  { 0                                                 , 0                             , 333, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #137 [ref=1x]
  { F(Vex)                                            , 0                             , 335, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #138 [ref=1x]
  { 0                                                 , 0                             , 445, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #139 [ref=1x]
  { 0                                                 , 0                             , 446, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #140 [ref=1x]
  { F(Lock)|F(XAcquire)|F(XRelease)                   , 0                             , 290, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #141 [ref=2x]
  { 0                                                 , 0                             , 108, 5 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #142 [ref=1x]
  { F(Lock)|F(XAcquire)|F(XRelease)                   , 0                             , 16 , 12, CONTROL_FLOW(Regular), SAME_REG_HINT(RO)}, // #143 [ref=1x]
  { 0                                                 , 0                             , 447, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #144 [ref=1x]
  { F(Rep)                                            , 0                             , 448, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #145 [ref=1x]
  { F(Mmx)|F(Vec)                                     , 0                             , 337, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #146 [ref=37x]
  { F(Mmx)|F(Vec)                                     , 0                             , 339, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #147 [ref=1x]
  { F(Mmx)|F(Vec)                                     , 0                             , 337, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(RO)}, // #148 [ref=6x]
  { F(Mmx)|F(Vec)                                     , 0                             , 337, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(WO)}, // #149 [ref=16x]
  { F(Mmx)                                            , 0                             , 337, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #150 [ref=26x]
  { F(Vec)                                            , 0                             , 79 , 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(WO)}, // #151 [ref=4x]
  { F(Vec)                                            , 0                             , 449, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #152 [ref=1x]
  { F(Vec)                                            , 0                             , 450, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #153 [ref=1x]
  { F(Vec)                                            , 0                             , 451, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #154 [ref=1x]
  { F(Vec)                                            , 0                             , 452, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #155 [ref=1x]
  { F(Vec)                                            , 0                             , 453, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #156 [ref=1x]
  { F(Vec)                                            , 0                             , 454, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #157 [ref=1x]
  { F(Mmx)|F(Vec)                                     , 0                             , 341, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #158 [ref=1x]
  { F(Vec)                                            , 0                             , 455, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #159 [ref=1x]
  { F(Vec)                                            , 0                             , 456, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #160 [ref=1x]
  { F(Vec)                                            , 0                             , 457, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #161 [ref=1x]
  { F(Mmx)|F(Vec)                                     , 0                             , 458, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #162 [ref=1x]
  { F(Mmx)|F(Vec)                                     , 0                             , 459, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #163 [ref=1x]
  { F(Vec)                                            , 0                             , 263, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #164 [ref=2x]
  { 0                                                 , 0                             , 143, 4 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #165 [ref=1x]
  { F(Mmx)                                            , 0                             , 339, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #166 [ref=1x]
  { F(Mmx)|F(Vec)                                     , 0                             , 343, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #167 [ref=8x]
  { F(Vec)                                            , 0                             , 460, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #168 [ref=2x]
  { 0                                                 , 0                             , 461, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #169 [ref=1x]
  { F(Mmx)|F(Vec)                                     , 0                             , 345, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #170 [ref=3x]
  { 0                                                 , 0                             , 147, 4 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #171 [ref=1x]
  { 0                                                 , 0                             , 462, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #172 [ref=8x]
  { 0                                                 , 0                             , 463, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #173 [ref=4x]
  { 0                                                 , 0                             , 464, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #174 [ref=8x]
  { 0                                                 , 0                             , 347, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #175 [ref=1x]
  { F(Rep)|F(RepIgnored)                              , 0                             , 349, 2 , CONTROL_FLOW(Return), SAME_REG_HINT(None)}, // #176 [ref=1x]
  { 0                                                 , 0                             , 349, 2 , CONTROL_FLOW(Return), SAME_REG_HINT(None)}, // #177 [ref=1x]
  { F(Vex)                                            , 0                             , 351, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #178 [ref=1x]
  { F(Lock)|F(XAcquire)|F(XRelease)                   , 0                             , 16 , 12, CONTROL_FLOW(Regular), SAME_REG_HINT(WO)}, // #179 [ref=3x]
  { F(Rep)                                            , 0                             , 151, 4 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #180 [ref=1x]
  { 0                                                 , 0                             , 465, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #181 [ref=30x]
  { 0                                                 , 0                             , 188, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #182 [ref=2x]
  { 0                                                 , 0                             , 466, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #183 [ref=3x]
  { F(Rep)                                            , 0                             , 155, 4 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #184 [ref=1x]
  { F(Vex)                                            , 0                             , 467, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #185 [ref=5x]
  { 0                                                 , 0                             , 66 , 7 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #186 [ref=1x]
  { F(Tsib)|F(Vex)                                    , 0                             , 468, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #187 [ref=2x]
  { F(Vex)                                            , 0                             , 395, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #188 [ref=1x]
  { F(Tsib)|F(Vex)                                    , 0                             , 469, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #189 [ref=1x]
  { F(Vex)                                            , 0                             , 470, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #190 [ref=1x]
  { 0                                                 , 0                             , 471, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #191 [ref=2x]
  { 0                                                 , 0                             , 180, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #192 [ref=2x]
  { 0                                                 , 0                             , 472, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #193 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(K)|X(T4X)|X(Z)              , 473, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #194 [ref=4x]
  { F(Evex)|F(Vec)                                    , X(K)|X(T4X)|X(Z)              , 474, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #195 [ref=2x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(B64)|X(ER)|X(K)|X(SAE)|X(Z) , 191, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #196 [ref=22x]
  { F(Evex)|F(Vec)                                    , X(B16)|X(ER)|X(K)|X(SAE)|X(Z) , 191, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #197 [ref=23x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(B32)|X(ER)|X(K)|X(SAE)|X(Z) , 191, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #198 [ref=22x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(ER)|X(K)|X(SAE)|X(Z)        , 475, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #199 [ref=18x]
  { F(Evex)|F(Vec)                                    , X(ER)|X(K)|X(SAE)|X(Z)        , 476, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #200 [ref=18x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(ER)|X(K)|X(SAE)|X(Z)        , 477, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #201 [ref=17x]
  { F(Vec)|F(Vex)                                     , 0                             , 191, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #202 [ref=15x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , 0                             , 191, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #203 [ref=5x]
  { F(Vec)|F(Vex)                                     , 0                             , 79 , 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #204 [ref=17x]
  { F(Vec)|F(Vex)                                     , 0                             , 221, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #205 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(B32)|X(K)|X(Z)              , 194, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #206 [ref=4x]
  { F(Evex)|F(Vec)                                    , X(B64)|X(K)|X(Z)              , 194, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #207 [ref=4x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(B64)|X(K)|X(Z)              , 191, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #208 [ref=10x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(B32)|X(K)|X(Z)              , 191, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #209 [ref=12x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(B64)|X(K)|X(Z)              , 191, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(RO)}, // #210 [ref=2x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(B32)|X(K)|X(Z)              , 191, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(RO)}, // #211 [ref=6x]
  { F(Evex)|F(Vec)                                    , X(B64)|X(K)|X(Z)              , 191, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #212 [ref=19x]
  { F(Evex)|F(Vec)                                    , X(B32)|X(K)|X(Z)              , 191, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #213 [ref=12x]
  { F(Vec)|F(Vex)                                     , 0                             , 194, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #214 [ref=6x]
  { F(Vec)|F(Vex)                                     , 0                             , 353, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #215 [ref=3x]
  { F(EvexTransformable)|F(Vec)|F(Vex)                , 0                             , 478, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #216 [ref=2x]
  { F(Evex)|F(Vec)                                    , X(K)|X(Z)                     , 479, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #217 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(K)|X(Z)                     , 480, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #218 [ref=4x]
  { F(Evex)|F(Vec)                                    , X(K)|X(Z)                     , 481, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #219 [ref=4x]
  { F(Evex)|F(Vec)                                    , X(K)|X(Z)                     , 482, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #220 [ref=1x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(K)|X(Z)                     , 479, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #221 [ref=1x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(K)|X(Z)                     , 483, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #222 [ref=1x]
  { F(Evex)|F(EvexKReg)|F(Vec)|F(Vex)                 , X(B64)|X(ImplicitZ)|X(K)|X(SAE), 197, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #223 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(B16)|X(ImplicitZ)|X(K)|X(SAE), 200, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #224 [ref=1x]
  { F(Evex)|F(EvexKReg)|F(Vec)|F(Vex)                 , X(B32)|X(ImplicitZ)|X(K)|X(SAE), 197, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #225 [ref=1x]
  { F(Evex)|F(EvexKReg)|F(Vec)|F(Vex)                 , X(ImplicitZ)|X(K)|X(SAE)      , 484, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #226 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(ImplicitZ)|X(K)|X(SAE)      , 485, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #227 [ref=1x]
  { F(Evex)|F(EvexKReg)|F(Vec)|F(Vex)                 , X(ImplicitZ)|X(K)|X(SAE)      , 486, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #228 [ref=1x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(SAE)                        , 106, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #229 [ref=2x]
  { F(Evex)|F(Vec)                                    , X(SAE)                        , 263, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #230 [ref=2x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(SAE)                        , 212, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #231 [ref=2x]
  { F(Evex)|F(Vec)                                    , X(K)|X(Z)                     , 203, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #232 [ref=6x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(B32)|X(K)|X(Z)              , 206, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #233 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(B32)|X(ER)|X(K)|X(SAE)|X(Z) , 355, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #234 [ref=3x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(B32)|X(ER)|X(K)|X(SAE)|X(Z) , 209, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #235 [ref=3x]
  { F(Evex)|F(Vec)                                    , X(B32)|X(K)|X(Z)              , 355, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #236 [ref=1x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(B64)|X(ER)|X(K)|X(SAE)|X(Z) , 355, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #237 [ref=2x]
  { F(Evex)|F(Vec)                                    , X(B64)|X(ER)|X(K)|X(SAE)|X(Z) , 487, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #238 [ref=3x]
  { F(Evex)|F(Vec)                                    , X(B64)|X(ER)|X(K)|X(SAE)|X(Z) , 209, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #239 [ref=4x]
  { F(Evex)|F(Vec)                                    , X(B64)|X(ER)|X(K)|X(SAE)|X(Z) , 355, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #240 [ref=3x]
  { F(Evex)|F(Vec)                                    , X(B16)|X(ER)|X(K)|X(SAE)|X(Z) , 206, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #241 [ref=2x]
  { F(Evex)|F(Vec)                                    , X(B16)|X(K)|X(SAE)|X(Z)       , 212, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #242 [ref=3x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(K)|X(SAE)|X(Z)              , 206, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #243 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(B16)|X(K)|X(SAE)|X(Z)       , 206, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #244 [ref=3x]
  { F(Evex)|F(Vec)                                    , X(B16)|X(ER)|X(K)|X(SAE)|X(Z) , 212, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #245 [ref=2x]
  { F(Evex)|F(Vec)                                    , X(B16)|X(ER)|X(K)|X(SAE)|X(Z) , 209, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #246 [ref=5x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(B32)|X(ER)|X(K)|X(SAE)|X(Z) , 206, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #247 [ref=1x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(K)|X(SAE)|X(Z)              , 215, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #248 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(B32)|X(ER)|X(K)|X(SAE)|X(Z) , 206, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #249 [ref=2x]
  { F(Evex)|F(Vec)                                    , X(B32)|X(ER)|X(K)|X(SAE)|X(Z) , 209, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #250 [ref=2x]
  { F(Evex)|F(Vec)                                    , X(ER)|X(K)|X(SAE)|X(Z)        , 475, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #251 [ref=2x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(ER)|X(SAE)                  , 406, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #252 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(ER)|X(SAE)                  , 406, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #253 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(K)|X(SAE)|X(Z)              , 476, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #254 [ref=5x]
  { F(Evex)|F(Vec)                                    , X(ER)|X(SAE)                  , 488, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #255 [ref=2x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(ER)|X(SAE)                  , 489, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #256 [ref=2x]
  { F(Evex)|F(Vec)                                    , X(ER)|X(SAE)                  , 489, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #257 [ref=4x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(K)|X(SAE)|X(Z)              , 477, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #258 [ref=3x]
  { F(Evex)|F(Vec)                                    , X(ER)|X(K)|X(SAE)|X(Z)        , 477, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #259 [ref=6x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(ER)|X(SAE)                  , 408, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #260 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(ER)|X(SAE)                  , 408, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #261 [ref=1x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(B64)|X(K)|X(SAE)|X(Z)       , 355, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #262 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(B64)|X(K)|X(SAE)|X(Z)       , 209, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #263 [ref=3x]
  { F(Evex)|F(Vec)                                    , X(B64)|X(K)|X(SAE)|X(Z)       , 355, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #264 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(B16)|X(K)|X(SAE)|X(Z)       , 209, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #265 [ref=3x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(B32)|X(K)|X(SAE)|X(Z)       , 209, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #266 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(B32)|X(K)|X(SAE)|X(Z)       , 206, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #267 [ref=2x]
  { F(Evex)|F(Vec)                                    , X(B32)|X(K)|X(SAE)|X(Z)       , 209, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #268 [ref=2x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(SAE)                        , 406, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #269 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(SAE)                        , 406, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #270 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(SAE)                        , 488, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #271 [ref=2x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(SAE)                        , 408, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #272 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(SAE)                        , 408, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #273 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(B32)|X(K)|X(Z)              , 206, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #274 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(K)|X(Z)                     , 194, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #275 [ref=3x]
  { F(Vec)|F(Vex)                                     , 0                             , 194, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #276 [ref=9x]
  { F(Evex)|F(Vec)                                    , X(B64)|X(K)|X(SAE)|X(Z)       , 83 , 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #277 [ref=3x]
  { F(Evex)|F(Vec)                                    , X(B32)|X(K)|X(SAE)|X(Z)       , 83 , 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #278 [ref=3x]
  { F(Evex)|F(Vec)                                    , X(K)|X(Z)                     , 209, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #279 [ref=8x]
  { F(EvexTransformable)|F(Vec)|F(Vex)                , 0                             , 216, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #280 [ref=2x]
  { F(Evex)|F(Vec)                                    , X(K)|X(Z)                     , 490, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #281 [ref=4x]
  { F(Evex)|F(Vec)                                    , X(K)|X(Z)                     , 217, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #282 [ref=4x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , 0                             , 412, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #283 [ref=2x]
  { F(Evex)|F(Vec)                                    , X(B32)|X(ER)|X(K)|X(SAE)|X(Z) , 191, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #284 [ref=5x]
  { F(Evex)|F(Vec)                                    , X(B64)|X(K)|X(SAE)|X(Z)       , 194, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #285 [ref=2x]
  { F(Evex)|F(Vec)                                    , X(B32)|X(K)|X(SAE)|X(Z)       , 194, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #286 [ref=2x]
  { F(Evex)|F(Vec)                                    , X(K)|X(SAE)|X(Z)              , 491, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #287 [ref=4x]
  { F(Evex)|F(Vec)                                    , X(K)|X(SAE)|X(Z)              , 492, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #288 [ref=4x]
  { F(Vec)|F(Vex)                                     , 0                             , 159, 4 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #289 [ref=13x]
  { F(Vec)|F(Vex)                                     , 0                             , 357, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #290 [ref=4x]
  { F(Vec)|F(Vex)                                     , 0                             , 359, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #291 [ref=4x]
  { F(Evex)|F(Vec)                                    , X(B64)|X(ImplicitZ)|X(K)      , 493, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #292 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(B16)|X(K)                   , 493, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #293 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(B32)|X(ImplicitZ)|X(K)      , 493, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #294 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(ImplicitZ)|X(K)             , 494, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #295 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(K)                          , 495, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #296 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(ImplicitZ)|X(K)             , 496, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #297 [ref=1x]
  { F(Vec)|F(Vex)                                     , 0                             , 209, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #298 [ref=7x]
  { F(Vec)|F(Vex)                                     , 0                             , 106, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #299 [ref=1x]
  { F(Vec)|F(Vex)                                     , 0                             , 212, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #300 [ref=1x]
  { F(Evex)|F(EvexTwoOp)|F(Vec)|F(Vex)|F(Vsib)        , X(K)                          , 163, 4 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #301 [ref=2x]
  { F(Evex)|F(EvexTwoOp)|F(Vec)|F(Vex)|F(Vsib)        , X(K)                          , 113, 5 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #302 [ref=2x]
  { F(Evex)|F(Vsib)                                   , X(K)                          , 497, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #303 [ref=4x]
  { F(Evex)|F(Vsib)                                   , X(K)                          , 498, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #304 [ref=4x]
  { F(Evex)|F(Vsib)                                   , X(K)                          , 499, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #305 [ref=8x]
  { F(Evex)|F(EvexTwoOp)|F(Vec)|F(Vex)|F(Vsib)        , X(K)                          , 118, 5 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #306 [ref=2x]
  { F(Evex)|F(EvexTwoOp)|F(Vec)|F(Vex)|F(Vsib)        , X(K)                          , 218, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #307 [ref=2x]
  { F(Evex)|F(Vec)                                    , X(K)|X(SAE)|X(Z)              , 475, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #308 [ref=3x]
  { F(Evex)|F(Vec)                                    , X(K)|X(SAE)|X(Z)              , 477, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #309 [ref=3x]
  { F(Evex)|F(Vec)                                    , X(B64)|X(K)|X(SAE)|X(Z)       , 221, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #310 [ref=2x]
  { F(Evex)|F(Vec)                                    , X(B16)|X(K)|X(SAE)|X(Z)       , 221, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #311 [ref=3x]
  { F(Evex)|F(Vec)                                    , X(B32)|X(K)|X(SAE)|X(Z)       , 221, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #312 [ref=2x]
  { F(Evex)|F(Vec)                                    , X(K)|X(SAE)|X(Z)              , 500, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #313 [ref=3x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(K)|X(Z)                     , 194, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #314 [ref=3x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(K)|X(Z)                     , 191, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #315 [ref=22x]
  { F(EvexTransformable)|F(Vec)|F(Vex)                , 0                             , 361, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #316 [ref=2x]
  { F(Evex)|F(Vec)                                    , X(K)|X(Z)                     , 361, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #317 [ref=4x]
  { F(Evex)|F(Vec)                                    , X(K)|X(Z)                     , 501, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #318 [ref=4x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , 0                             , 492, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #319 [ref=1x]
  { F(Vec)|F(Vex)                                     , 0                             , 230, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #320 [ref=1x]
  { F(Vex)                                            , 0                             , 431, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #321 [ref=2x]
  { F(Vec)|F(Vex)                                     , 0                             , 437, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #322 [ref=1x]
  { F(Vec)|F(Vex)                                     , 0                             , 167, 4 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #323 [ref=4x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(B64)|X(K)|X(SAE)|X(Z)       , 191, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #324 [ref=2x]
  { F(Evex)|F(Vec)                                    , X(B16)|X(K)|X(SAE)|X(Z)       , 191, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #325 [ref=2x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(B32)|X(K)|X(SAE)|X(Z)       , 191, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #326 [ref=2x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(K)|X(SAE)|X(Z)              , 475, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #327 [ref=2x]
  { 0                                                 , 0                             , 363, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #328 [ref=3x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(K)|X(Z)                     , 79 , 6 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #329 [ref=4x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , 0                             , 365, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #330 [ref=1x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(K)|X(Z)                     , 224, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #331 [ref=1x]
  { F(EvexTransformable)|F(Vec)|F(Vex)                , 0                             , 79 , 4 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #332 [ref=2x]
  { F(Evex)|F(Vec)                                    , X(K)|X(Z)                     , 79 , 6 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #333 [ref=6x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , 0                             , 238, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #334 [ref=2x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , 0                             , 367, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #335 [ref=4x]
  { F(Vec)|F(Vex)                                     , 0                             , 502, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #336 [ref=3x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , 0                             , 227, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #337 [ref=3x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , 0                             , 230, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #338 [ref=1x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , 0                             , 233, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #339 [ref=1x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(K)|X(Z)                     , 236, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #340 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(K)|X(Z)                     , 239, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #341 [ref=1x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(K)|X(Z)                     , 209, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #342 [ref=4x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(K)|X(Z)                     , 242, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #343 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(K)|X(Z)                     , 369, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #344 [ref=1x]
  { 0                                                 , 0                             , 371, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #345 [ref=1x]
  { 0                                                 , 0                             , 373, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #346 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(B32)                        , 245, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #347 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(B64)                        , 245, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #348 [ref=1x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(B32)|X(K)|X(Z)              , 209, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #349 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(B64)|X(K)|X(Z)              , 209, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #350 [ref=5x]
  { F(EvexTransformable)|F(Vec)|F(Vex)                , 0                             , 191, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(RO)}, // #351 [ref=2x]
  { F(Evex)|F(Vec)                                    , X(B32)|X(K)|X(Z)              , 191, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(RO)}, // #352 [ref=2x]
  { F(EvexTransformable)|F(Vec)|F(Vex)                , 0                             , 191, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(WO)}, // #353 [ref=2x]
  { F(Evex)|F(Vec)                                    , X(B32)|X(K)|X(Z)              , 191, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(WO)}, // #354 [ref=2x]
  { F(Evex)|F(Vec)                                    , X(B64)|X(K)|X(Z)              , 191, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(WO)}, // #355 [ref=2x]
  { F(Evex)|F(Vec)                                    , X(B64)|X(K)|X(Z)              , 191, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(RO)}, // #356 [ref=2x]
  { F(Evex)|F(Vec)                                    , X(K)|X(Z)                     , 191, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #357 [ref=13x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(K)|X(Z)                     , 503, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #358 [ref=1x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(K)|X(Z)                     , 504, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #359 [ref=1x]
  { F(Evex)|F(Vec)                                    , 0                             , 505, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #360 [ref=6x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(K)|X(Z)                     , 248, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #361 [ref=1x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(K)|X(Z)                     , 506, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #362 [ref=1x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , 0                             , 194, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #363 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(ImplicitZ)|X(K)             , 200, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(WO)}, // #364 [ref=2x]
  { F(Evex)|F(Vec)                                    , X(B32)|X(ImplicitZ)|X(K)      , 200, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(WO)}, // #365 [ref=2x]
  { F(Evex)|F(EvexKReg)|F(Vec)|F(Vex)                 , X(ImplicitZ)|X(K)             , 251, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(WO)}, // #366 [ref=4x]
  { F(Evex)|F(EvexKReg)|F(Vec)|F(Vex)                 , X(B32)|X(ImplicitZ)|X(K)      , 251, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(WO)}, // #367 [ref=2x]
  { F(Evex)|F(EvexKReg)|F(Vec)|F(Vex)                 , X(B64)|X(ImplicitZ)|X(K)      , 251, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(WO)}, // #368 [ref=2x]
  { F(Vec)|F(Vex)                                     , 0                             , 449, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #369 [ref=1x]
  { F(Vec)|F(Vex)                                     , 0                             , 450, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #370 [ref=1x]
  { F(Vec)|F(Vex)                                     , 0                             , 451, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #371 [ref=1x]
  { F(Vec)|F(Vex)                                     , 0                             , 452, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #372 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(B64)|X(ImplicitZ)|X(K)      , 200, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(WO)}, // #373 [ref=4x]
  { F(Evex)|F(Vec)                                    , X(B32)|X(K)|X(Z)              , 209, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #374 [ref=6x]
  { F(Evex)|F(EvexCompat)|F(PreferEvex)|F(Vec)|F(Vex) , X(B32)|X(K)|X(Z)              , 191, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #375 [ref=4x]
  { F(Vec)|F(Vex)                                     , 0                             , 195, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #376 [ref=2x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(B32)|X(K)|X(Z)              , 192, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #377 [ref=2x]
  { F(Vec)|F(Vex)                                     , 0                             , 171, 4 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #378 [ref=2x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(B64)|X(K)|X(Z)              , 85 , 6 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #379 [ref=1x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(B32)|X(K)|X(Z)              , 85 , 6 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #380 [ref=1x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(B64)|X(K)|X(Z)              , 175, 4 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #381 [ref=2x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , 0                             , 453, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #382 [ref=1x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , 0                             , 454, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #383 [ref=1x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , 0                             , 507, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #384 [ref=1x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(K)|X(Z)                     , 508, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #385 [ref=1x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(K)|X(Z)                     , 509, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #386 [ref=1x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(K)|X(Z)                     , 510, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #387 [ref=1x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(K)|X(Z)                     , 511, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #388 [ref=1x]
  { F(Vec)|F(Vex)                                     , 0                             , 353, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #389 [ref=12x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(K)|X(Z)                     , 191, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(RO)}, // #390 [ref=8x]
  { F(Evex)|F(Vec)                                    , 0                             , 512, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #391 [ref=4x]
  { F(Evex)|F(Vec)                                    , X(K)|X(Z)                     , 254, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #392 [ref=6x]
  { F(Evex)|F(Vec)                                    , X(K)|X(Z)                     , 257, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #393 [ref=9x]
  { F(Evex)|F(Vec)                                    , X(K)|X(Z)                     , 260, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #394 [ref=3x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(K)|X(Z)                     , 212, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #395 [ref=4x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(K)|X(Z)                     , 263, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #396 [ref=2x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(K)|X(Z)                     , 206, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #397 [ref=6x]
  { F(Vec)|F(Vex)                                     , 0                             , 159, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #398 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(B32)|X(K)|X(Z)              , 221, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #399 [ref=3x]
  { F(Evex)|F(Vec)                                    , X(B64)|X(K)|X(Z)              , 221, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #400 [ref=3x]
  { F(Vec)|F(Vex)                                     , 0                             , 375, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #401 [ref=4x]
  { F(Evex)|F(Vec)|F(Vsib)                            , X(K)                          , 266, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #402 [ref=2x]
  { F(Evex)|F(Vec)|F(Vsib)                            , X(K)                          , 377, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #403 [ref=2x]
  { F(Evex)|F(Vec)|F(Vsib)                            , X(K)                          , 379, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #404 [ref=2x]
  { F(Evex)|F(Vec)|F(Vsib)                            , X(K)                          , 269, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #405 [ref=2x]
  { F(Vec)|F(Vex)                                     , 0                             , 381, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #406 [ref=8x]
  { F(Evex)|F(Vec)                                    , X(ImplicitZ)|X(K)             , 272, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #407 [ref=5x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(B32)|X(K)|X(Z)              , 221, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #408 [ref=1x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(K)|X(Z)                     , 221, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #409 [ref=2x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(B32)|X(K)|X(Z)              , 91 , 6 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #410 [ref=3x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , 0                             , 221, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #411 [ref=2x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(B64)|X(K)|X(Z)              , 91 , 6 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #412 [ref=2x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(K)|X(Z)                     , 91 , 6 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #413 [ref=3x]
  { F(Evex)|F(Vec)                                    , X(B64)|X(K)|X(Z)              , 97 , 6 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #414 [ref=1x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(K)|X(Z)                     , 191, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(WO)}, // #415 [ref=6x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(B32)|X(K)|X(Z)              , 191, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(WO)}, // #416 [ref=2x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(B64)|X(K)|X(Z)              , 191, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(WO)}, // #417 [ref=2x]
  { F(Evex)|F(Vec)                                    , X(B32)|X(ImplicitZ)|X(K)      , 272, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #418 [ref=2x]
  { F(Evex)|F(Vec)                                    , X(B64)|X(ImplicitZ)|X(K)      , 272, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #419 [ref=2x]
  { F(Evex)|F(Vec)                                    , X(K)|X(Z)                     , 475, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #420 [ref=2x]
  { F(Evex)|F(Vec)                                    , X(K)|X(Z)                     , 477, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #421 [ref=2x]
  { F(Evex)|F(Vec)                                    , X(B16)|X(K)|X(Z)              , 209, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #422 [ref=2x]
  { F(Evex)|F(Vec)                                    , X(K)|X(Z)                     , 476, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #423 [ref=2x]
  { F(Vec)|F(Vex)                                     , 0                             , 477, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #424 [ref=2x]
  { F(Evex)|F(Vec)                                    , X(K)|X(Z)                     , 491, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #425 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(K)|X(Z)                     , 492, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #426 [ref=1x]
  { F(EvexTransformable)|F(Vec)|F(Vex)                , 0                             , 221, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #427 [ref=2x]
  { F(EvexTransformable)|F(Vec)|F(Vex)                , 0                             , 491, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #428 [ref=1x]
  { F(EvexTransformable)|F(Vec)|F(Vex)                , 0                             , 492, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #429 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(B64)|X(ER)|X(K)|X(SAE)|X(Z) , 191, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #430 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(B32)|X(K)|X(Z)              , 195, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #431 [ref=2x]
  { F(Evex)|F(Vec)                                    , X(B64)|X(K)|X(Z)              , 195, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #432 [ref=2x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(B64)|X(K)|X(Z)              , 194, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #433 [ref=1x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(B32)|X(K)|X(Z)              , 194, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #434 [ref=1x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(B64)|X(ER)|X(K)|X(SAE)|X(Z) , 209, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #435 [ref=1x]
  { F(Vec)|F(Vex)                                     , 0                             , 108, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #436 [ref=2x]
  { 0                                                 , 0                             , 23 , 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #437 [ref=2x]
  { 0                                                 , 0                             , 61 , 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #438 [ref=2x]
  { F(Lock)|F(XAcquire)|F(XRelease)                   , 0                             , 58 , 4 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #439 [ref=1x]
  { 0                                                 , 0                             , 513, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #440 [ref=1x]
  { F(Lock)|F(XAcquire)                               , 0                             , 58 , 8 , CONTROL_FLOW(Regular), SAME_REG_HINT(RO)}, // #441 [ref=1x]
  { 0                                                 , 0                             , 514, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #442 [ref=6x]
  { 0                                                 , 0                             , 515, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}  // #443 [ref=6x]
};
#undef SAME_REG_HINT
#undef CONTROL_FLOW
#undef X
#undef F
// ----------------------------------------------------------------------------
// ${InstCommonTable:End}

// x86::InstDB - AdditionalInfoTable
// =================================

// ${AdditionalInfoTable:Begin}
// ------------------- Automatically generated, do not edit -------------------
#define EXT(VAL) uint32_t(CpuFeatures::X86::k##VAL)
const InstDB::AdditionalInfo InstDB::_additionalInfoTable[] = {
  { { 0 }, 0, 0 }, // #0 [ref=149x]
  { { 0 }, 1, 0 }, // #1 [ref=32x]
  { { 0 }, 2, 0 }, // #2 [ref=2x]
  { { EXT(ADX) }, 3, 0 }, // #3 [ref=1x]
  { { EXT(SSE2) }, 0, 0 }, // #4 [ref=65x]
  { { EXT(SSE) }, 0, 0 }, // #5 [ref=44x]
  { { EXT(SSE3) }, 0, 0 }, // #6 [ref=12x]
  { { EXT(ADX) }, 4, 0 }, // #7 [ref=1x]
  { { EXT(AESNI) }, 0, 0 }, // #8 [ref=6x]
  { { EXT(BMI) }, 1, 0 }, // #9 [ref=6x]
  { { 0 }, 5, 0 }, // #10 [ref=5x]
  { { EXT(TBM) }, 0, 0 }, // #11 [ref=9x]
  { { EXT(SSE4_1) }, 0, 0 }, // #12 [ref=47x]
  { { EXT(MPX) }, 0, 0 }, // #13 [ref=7x]
  { { 0 }, 6, 0 }, // #14 [ref=4x]
  { { EXT(BMI2) }, 1, 0 }, // #15 [ref=1x]
  { { EXT(SMAP) }, 7, 0 }, // #16 [ref=2x]
  { { 0 }, 8, 0 }, // #17 [ref=2x]
  { { 0 }, 9, 0 }, // #18 [ref=2x]
  { { EXT(CLDEMOTE) }, 0, 0 }, // #19 [ref=1x]
  { { EXT(CLFLUSH) }, 0, 0 }, // #20 [ref=1x]
  { { EXT(CLFLUSHOPT) }, 0, 0 }, // #21 [ref=1x]
  { { EXT(SVM) }, 0, 0 }, // #22 [ref=6x]
  { { 0 }, 10, 0 }, // #23 [ref=2x]
  { { EXT(CET_SS) }, 1, 0 }, // #24 [ref=3x]
  { { EXT(UINTR) }, 0, 0 }, // #25 [ref=4x]
  { { EXT(CLWB) }, 0, 0 }, // #26 [ref=1x]
  { { EXT(CLZERO) }, 0, 0 }, // #27 [ref=1x]
  { { 0 }, 3, 0 }, // #28 [ref=1x]
  { { EXT(CMOV) }, 11, 0 }, // #29 [ref=6x]
  { { EXT(CMOV) }, 12, 0 }, // #30 [ref=8x]
  { { EXT(CMOV) }, 13, 0 }, // #31 [ref=6x]
  { { EXT(CMOV) }, 14, 0 }, // #32 [ref=4x]
  { { EXT(CMOV) }, 15, 0 }, // #33 [ref=4x]
  { { EXT(CMOV) }, 16, 0 }, // #34 [ref=2x]
  { { EXT(CMOV) }, 17, 0 }, // #35 [ref=6x]
  { { EXT(CMOV) }, 18, 0 }, // #36 [ref=2x]
  { { 0 }, 19, 0 }, // #37 [ref=2x]
  { { EXT(I486) }, 1, 0 }, // #38 [ref=2x]
  { { EXT(CMPXCHG16B) }, 5, 0 }, // #39 [ref=1x]
  { { EXT(CMPXCHG8B) }, 5, 0 }, // #40 [ref=1x]
  { { EXT(SSE2) }, 1, 0 }, // #41 [ref=2x]
  { { EXT(SSE) }, 1, 0 }, // #42 [ref=2x]
  { { EXT(I486) }, 0, 0 }, // #43 [ref=4x]
  { { EXT(SSE4_2) }, 0, 0 }, // #44 [ref=2x]
  { { 0 }, 20, 0 }, // #45 [ref=2x]
  { { EXT(MMX) }, 0, 0 }, // #46 [ref=1x]
  { { EXT(CET_IBT) }, 0, 0 }, // #47 [ref=2x]
  { { EXT(ENQCMD) }, 0, 0 }, // #48 [ref=2x]
  { { EXT(SSE4A) }, 0, 0 }, // #49 [ref=4x]
  { { 0 }, 21, 0 }, // #50 [ref=4x]
  { { EXT(3DNOW) }, 0, 0 }, // #51 [ref=21x]
  { { EXT(FXSR) }, 0, 0 }, // #52 [ref=4x]
  { { EXT(SMX) }, 0, 0 }, // #53 [ref=1x]
  { { EXT(GFNI) }, 0, 0 }, // #54 [ref=3x]
  { { EXT(HRESET) }, 0, 0 }, // #55 [ref=1x]
  { { EXT(CET_SS) }, 0, 0 }, // #56 [ref=9x]
  { { 0 }, 16, 0 }, // #57 [ref=5x]
  { { EXT(VMX) }, 0, 0 }, // #58 [ref=12x]
  { { 0 }, 11, 0 }, // #59 [ref=8x]
  { { 0 }, 12, 0 }, // #60 [ref=12x]
  { { 0 }, 13, 0 }, // #61 [ref=10x]
  { { 0 }, 14, 0 }, // #62 [ref=8x]
  { { 0 }, 15, 0 }, // #63 [ref=8x]
  { { 0 }, 17, 0 }, // #64 [ref=8x]
  { { 0 }, 18, 0 }, // #65 [ref=4x]
  { { EXT(AVX512_DQ) }, 0, 0 }, // #66 [ref=23x]
  { { EXT(AVX512_BW) }, 0, 0 }, // #67 [ref=22x]
  { { EXT(AVX512_F) }, 0, 0 }, // #68 [ref=37x]
  { { EXT(AVX512_DQ) }, 1, 0 }, // #69 [ref=3x]
  { { EXT(AVX512_BW) }, 1, 0 }, // #70 [ref=4x]
  { { EXT(AVX512_F) }, 1, 0 }, // #71 [ref=1x]
  { { EXT(LAHFSAHF) }, 22, 0 }, // #72 [ref=1x]
  { { EXT(AMX_TILE) }, 0, 0 }, // #73 [ref=7x]
  { { EXT(LWP) }, 0, 0 }, // #74 [ref=4x]
  { { 0 }, 23, 0 }, // #75 [ref=3x]
  { { EXT(LZCNT) }, 1, 0 }, // #76 [ref=1x]
  { { EXT(MMX2) }, 0, 0 }, // #77 [ref=8x]
  { { EXT(MCOMMIT) }, 1, 0 }, // #78 [ref=1x]
  { { EXT(MONITOR) }, 0, 0 }, // #79 [ref=2x]
  { { EXT(MONITORX) }, 0, 0 }, // #80 [ref=2x]
  { { EXT(MOVBE) }, 0, 0 }, // #81 [ref=1x]
  { { EXT(MMX), EXT(SSE2) }, 0, 0 }, // #82 [ref=46x]
  { { EXT(MOVDIR64B) }, 0, 0 }, // #83 [ref=1x]
  { { EXT(MOVDIRI) }, 0, 0 }, // #84 [ref=1x]
  { { EXT(BMI2) }, 0, 0 }, // #85 [ref=7x]
  { { EXT(SSSE3) }, 0, 0 }, // #86 [ref=15x]
  { { EXT(MMX2), EXT(SSE2) }, 0, 0 }, // #87 [ref=10x]
  { { EXT(PCLMULQDQ) }, 0, 0 }, // #88 [ref=1x]
  { { EXT(SSE4_2) }, 1, 0 }, // #89 [ref=4x]
  { { EXT(PCONFIG) }, 0, 0 }, // #90 [ref=1x]
  { { EXT(MMX2), EXT(SSE2), EXT(SSE4_1) }, 0, 0 }, // #91 [ref=1x]
  { { EXT(3DNOW2) }, 0, 0 }, // #92 [ref=5x]
  { { EXT(GEODE) }, 0, 0 }, // #93 [ref=2x]
  { { EXT(POPCNT) }, 1, 0 }, // #94 [ref=1x]
  { { 0 }, 24, 0 }, // #95 [ref=3x]
  { { EXT(PREFETCHW) }, 1, 0 }, // #96 [ref=1x]
  { { EXT(PREFETCHWT1) }, 1, 0 }, // #97 [ref=1x]
  { { EXT(SNP) }, 20, 0 }, // #98 [ref=3x]
  { { EXT(SSE4_1) }, 1, 0 }, // #99 [ref=1x]
  { { EXT(PTWRITE) }, 0, 0 }, // #100 [ref=1x]
  { { 0 }, 25, 0 }, // #101 [ref=3x]
  { { EXT(SNP) }, 1, 0 }, // #102 [ref=1x]
  { { 0 }, 26, 0 }, // #103 [ref=2x]
  { { EXT(FSGSBASE) }, 0, 0 }, // #104 [ref=4x]
  { { EXT(MSR) }, 0, 0 }, // #105 [ref=2x]
  { { EXT(RDPID) }, 0, 0 }, // #106 [ref=1x]
  { { EXT(OSPKE) }, 0, 0 }, // #107 [ref=1x]
  { { EXT(RDPRU) }, 0, 0 }, // #108 [ref=1x]
  { { EXT(RDRAND) }, 1, 0 }, // #109 [ref=1x]
  { { EXT(RDSEED) }, 1, 0 }, // #110 [ref=1x]
  { { EXT(RDTSC) }, 0, 0 }, // #111 [ref=1x]
  { { EXT(RDTSCP) }, 0, 0 }, // #112 [ref=1x]
  { { 0 }, 27, 0 }, // #113 [ref=2x]
  { { EXT(LAHFSAHF) }, 28, 0 }, // #114 [ref=1x]
  { { EXT(SERIALIZE) }, 0, 0 }, // #115 [ref=1x]
  { { EXT(SHA) }, 0, 0 }, // #116 [ref=7x]
  { { EXT(SKINIT) }, 0, 0 }, // #117 [ref=2x]
  { { EXT(AMX_BF16) }, 0, 0 }, // #118 [ref=1x]
  { { EXT(AMX_INT8) }, 0, 0 }, // #119 [ref=4x]
  { { EXT(UINTR) }, 1, 0 }, // #120 [ref=1x]
  { { EXT(WAITPKG) }, 1, 0 }, // #121 [ref=2x]
  { { EXT(WAITPKG) }, 0, 0 }, // #122 [ref=1x]
  { { EXT(AVX512_4FMAPS) }, 0, 0 }, // #123 [ref=4x]
  { { EXT(AVX), EXT(AVX512_F), EXT(AVX512_VL) }, 0, 0 }, // #124 [ref=46x]
  { { EXT(AVX512_FP16), EXT(AVX512_VL) }, 0, 0 }, // #125 [ref=63x]
  { { EXT(AVX), EXT(AVX512_F) }, 0, 0 }, // #126 [ref=32x]
  { { EXT(AVX512_FP16) }, 0, 0 }, // #127 [ref=43x]
  { { EXT(AVX) }, 0, 0 }, // #128 [ref=37x]
  { { EXT(AESNI), EXT(AVX), EXT(AVX512_F), EXT(AVX512_VL), EXT(VAES) }, 0, 0 }, // #129 [ref=4x]
  { { EXT(AESNI), EXT(AVX) }, 0, 0 }, // #130 [ref=2x]
  { { EXT(AVX512_F), EXT(AVX512_VL) }, 0, 0 }, // #131 [ref=112x]
  { { EXT(AVX), EXT(AVX512_DQ), EXT(AVX512_VL) }, 0, 0 }, // #132 [ref=8x]
  { { EXT(AVX512_DQ), EXT(AVX512_VL) }, 0, 0 }, // #133 [ref=30x]
  { { EXT(AVX2) }, 0, 0 }, // #134 [ref=7x]
  { { EXT(AVX), EXT(AVX2), EXT(AVX512_F), EXT(AVX512_VL) }, 0, 0 }, // #135 [ref=39x]
  { { EXT(AVX), EXT(AVX512_F) }, 1, 0 }, // #136 [ref=4x]
  { { EXT(AVX512_BF16), EXT(AVX512_VL) }, 0, 0 }, // #137 [ref=3x]
  { { EXT(AVX512_F), EXT(AVX512_VL), EXT(F16C) }, 0, 0 }, // #138 [ref=2x]
  { { EXT(AVX512_BW), EXT(AVX512_VL) }, 0, 0 }, // #139 [ref=26x]
  { { EXT(AVX512_ERI) }, 0, 0 }, // #140 [ref=10x]
  { { EXT(AVX512_F), EXT(AVX512_VL), EXT(FMA) }, 0, 0 }, // #141 [ref=36x]
  { { EXT(AVX512_F), EXT(FMA) }, 0, 0 }, // #142 [ref=24x]
  { { EXT(FMA4) }, 0, 0 }, // #143 [ref=20x]
  { { EXT(XOP) }, 0, 0 }, // #144 [ref=55x]
  { { EXT(AVX2), EXT(AVX512_F), EXT(AVX512_VL) }, 0, 0 }, // #145 [ref=19x]
  { { EXT(AVX512_PFI) }, 0, 0 }, // #146 [ref=16x]
  { { EXT(AVX), EXT(AVX512_F), EXT(AVX512_VL), EXT(GFNI) }, 0, 0 }, // #147 [ref=3x]
  { { EXT(AVX), EXT(AVX2) }, 0, 0 }, // #148 [ref=17x]
  { { EXT(AVX512_VP2INTERSECT) }, 0, 0 }, // #149 [ref=2x]
  { { EXT(AVX512_4VNNIW) }, 0, 0 }, // #150 [ref=2x]
  { { EXT(AVX), EXT(AVX2), EXT(AVX512_BW), EXT(AVX512_VL) }, 0, 0 }, // #151 [ref=54x]
  { { EXT(AVX2), EXT(AVX512_BW), EXT(AVX512_VL) }, 0, 0 }, // #152 [ref=2x]
  { { EXT(AVX512_CDI), EXT(AVX512_VL) }, 0, 0 }, // #153 [ref=6x]
  { { EXT(AVX), EXT(AVX512_F), EXT(AVX512_VL), EXT(PCLMULQDQ), EXT(VPCLMULQDQ) }, 0, 0 }, // #154 [ref=1x]
  { { EXT(AVX) }, 1, 0 }, // #155 [ref=7x]
  { { EXT(AVX512_VBMI2), EXT(AVX512_VL) }, 0, 0 }, // #156 [ref=16x]
  { { EXT(AVX512_VL), EXT(AVX512_VNNI), EXT(AVX_VNNI) }, 0, 0 }, // #157 [ref=4x]
  { { EXT(AVX512_VBMI), EXT(AVX512_VL) }, 0, 0 }, // #158 [ref=4x]
  { { EXT(AVX), EXT(AVX512_BW) }, 0, 0 }, // #159 [ref=4x]
  { { EXT(AVX), EXT(AVX512_DQ) }, 0, 0 }, // #160 [ref=4x]
  { { EXT(AVX512_IFMA), EXT(AVX512_VL) }, 0, 0 }, // #161 [ref=2x]
  { { EXT(AVX512_BITALG), EXT(AVX512_VL) }, 0, 0 }, // #162 [ref=3x]
  { { EXT(AVX512_VL), EXT(AVX512_VPOPCNTDQ) }, 0, 0 }, // #163 [ref=2x]
  { { EXT(WBNOINVD) }, 0, 0 }, // #164 [ref=1x]
  { { EXT(RTM) }, 0, 0 }, // #165 [ref=3x]
  { { EXT(XSAVE) }, 0, 0 }, // #166 [ref=6x]
  { { EXT(TSXLDTRK) }, 0, 0 }, // #167 [ref=2x]
  { { EXT(XSAVES) }, 0, 0 }, // #168 [ref=4x]
  { { EXT(XSAVEC) }, 0, 0 }, // #169 [ref=2x]
  { { EXT(XSAVEOPT) }, 0, 0 }, // #170 [ref=2x]
  { { EXT(TSX) }, 1, 0 }  // #171 [ref=1x]
};
#undef EXT

#define FLAG(VAL) uint32_t(CpuRWFlags::kX86_##VAL)
const InstDB::RWFlagsInfoTable InstDB::_rwFlagsInfoTable[] = {
  { 0, 0 }, // #0 [ref=1429x]
  { 0, FLAG(AF) | FLAG(CF) | FLAG(OF) | FLAG(PF) | FLAG(SF) | FLAG(ZF) }, // #1 [ref=84x]
  { FLAG(CF), FLAG(AF) | FLAG(CF) | FLAG(OF) | FLAG(PF) | FLAG(SF) | FLAG(ZF) }, // #2 [ref=2x]
  { FLAG(CF), FLAG(CF) }, // #3 [ref=2x]
  { FLAG(OF), FLAG(OF) }, // #4 [ref=1x]
  { 0, FLAG(ZF) }, // #5 [ref=7x]
  { 0, FLAG(AF) | FLAG(CF) | FLAG(OF) | FLAG(PF) | FLAG(SF) }, // #6 [ref=4x]
  { 0, FLAG(AC) }, // #7 [ref=2x]
  { 0, FLAG(CF) }, // #8 [ref=2x]
  { 0, FLAG(DF) }, // #9 [ref=2x]
  { 0, FLAG(IF) }, // #10 [ref=2x]
  { FLAG(CF) | FLAG(ZF), 0 }, // #11 [ref=14x]
  { FLAG(CF), 0 }, // #12 [ref=20x]
  { FLAG(ZF), 0 }, // #13 [ref=16x]
  { FLAG(OF) | FLAG(SF) | FLAG(ZF), 0 }, // #14 [ref=12x]
  { FLAG(OF) | FLAG(SF), 0 }, // #15 [ref=12x]
  { FLAG(OF), 0 }, // #16 [ref=7x]
  { FLAG(PF), 0 }, // #17 [ref=14x]
  { FLAG(SF), 0 }, // #18 [ref=6x]
  { FLAG(DF), FLAG(AF) | FLAG(CF) | FLAG(OF) | FLAG(PF) | FLAG(SF) | FLAG(ZF) }, // #19 [ref=2x]
  { 0, FLAG(AF) | FLAG(OF) | FLAG(PF) | FLAG(SF) | FLAG(ZF) }, // #20 [ref=5x]
  { 0, FLAG(CF) | FLAG(PF) | FLAG(ZF) }, // #21 [ref=4x]
  { FLAG(AF) | FLAG(CF) | FLAG(PF) | FLAG(SF) | FLAG(ZF), 0 }, // #22 [ref=1x]
  { FLAG(DF), 0 }, // #23 [ref=3x]
  { 0, FLAG(AF) | FLAG(CF) | FLAG(DF) | FLAG(IF) | FLAG(OF) | FLAG(PF) | FLAG(SF) | FLAG(ZF) }, // #24 [ref=3x]
  { FLAG(AF) | FLAG(CF) | FLAG(DF) | FLAG(IF) | FLAG(OF) | FLAG(PF) | FLAG(SF) | FLAG(ZF), 0 }, // #25 [ref=3x]
  { FLAG(CF) | FLAG(OF), FLAG(CF) | FLAG(OF) }, // #26 [ref=2x]
  { 0, FLAG(CF) | FLAG(OF) }, // #27 [ref=2x]
  { 0, FLAG(AF) | FLAG(CF) | FLAG(PF) | FLAG(SF) | FLAG(ZF) }  // #28 [ref=1x]
};
#undef FLAG
// ----------------------------------------------------------------------------
// ${AdditionalInfoTable:End}

// Inst - NameData
// ===============

#ifndef ASMJIT_NO_TEXT
// ${NameData:Begin}
// ------------------- Automatically generated, do not edit -------------------
const char InstDB::_nameData[] =
  "\0" "aaa\0" "aad\0" "aam\0" "aas\0" "adc\0" "adcx\0" "adox\0" "arpl\0" "bextr\0" "blcfill\0" "blci\0" "blcic\0"
  "blcmsk\0" "blcs\0" "blsfill\0" "blsi\0" "blsic\0" "blsmsk\0" "blsr\0" "bndcl\0" "bndcn\0" "bndcu\0" "bndldx\0"
  "bndmk\0" "bndmov\0" "bndstx\0" "bound\0" "bsf\0" "bsr\0" "bswap\0" "bt\0" "btc\0" "btr\0" "bts\0" "bzhi\0" "cbw\0"
  "cdq\0" "cdqe\0" "clac\0" "clc\0" "cld\0" "cldemote\0" "clflush\0" "clflushopt\0" "clgi\0" "cli\0" "clrssbsy\0"
  "clts\0" "clui\0" "clwb\0" "clzero\0" "cmc\0" "cmova\0" "cmovae\0" "cmovc\0" "cmovg\0" "cmovge\0" "cmovl\0"
  "cmovle\0" "cmovna\0" "cmovnae\0" "cmovnc\0" "cmovng\0" "cmovnge\0" "cmovnl\0" "cmovnle\0" "cmovno\0" "cmovnp\0"
  "cmovns\0" "cmovnz\0" "cmovo\0" "cmovp\0" "cmovpe\0" "cmovpo\0" "cmovs\0" "cmovz\0" "cmp\0" "cmps\0" "cmpxchg\0"
  "cmpxchg16b\0" "cmpxchg8b\0" "cpuid\0" "cqo\0" "crc32\0" "cvtpd2pi\0" "cvtpi2pd\0" "cvtpi2ps\0" "cvtps2pi\0"
  "cvttpd2pi\0" "cvttps2pi\0" "cwd\0" "cwde\0" "daa\0" "das\0" "endbr32\0" "endbr64\0" "enqcmd\0" "enqcmds\0" "f2xm1\0"
  "fabs\0" "faddp\0" "fbld\0" "fbstp\0" "fchs\0" "fclex\0" "fcmovb\0" "fcmovbe\0" "fcmove\0" "fcmovnb\0" "fcmovnbe\0"
  "fcmovne\0" "fcmovnu\0" "fcmovu\0" "fcom\0" "fcomi\0" "fcomip\0" "fcomp\0" "fcompp\0" "fcos\0" "fdecstp\0" "fdiv\0"
  "fdivp\0" "fdivr\0" "fdivrp\0" "femms\0" "ffree\0" "fiadd\0" "ficom\0" "ficomp\0" "fidiv\0" "fidivr\0" "fild\0"
  "fimul\0" "fincstp\0" "finit\0" "fist\0" "fistp\0" "fisttp\0" "fisub\0" "fisubr\0" "fld\0" "fld1\0" "fldcw\0"
  "fldenv\0" "fldl2e\0" "fldl2t\0" "fldlg2\0" "fldln2\0" "fldpi\0" "fldz\0" "fmulp\0" "fnclex\0" "fninit\0" "fnop\0"
  "fnsave\0" "fnstcw\0" "fnstenv\0" "fnstsw\0" "fpatan\0" "fprem\0" "fprem1\0" "fptan\0" "frndint\0" "frstor\0"
  "fsave\0" "fscale\0" "fsin\0" "fsincos\0" "fsqrt\0" "fst\0" "fstcw\0" "fstenv\0" "fstp\0" "fstsw\0" "fsubp\0"
  "fsubrp\0" "ftst\0" "fucom\0" "fucomi\0" "fucomip\0" "fucomp\0" "fucompp\0" "fwait\0" "fxam\0" "fxch\0" "fxrstor\0"
  "fxrstor64\0" "fxsave\0" "fxsave64\0" "fxtract\0" "fyl2x\0" "fyl2xp1\0" "getsec\0" "hlt\0" "hreset\0" "inc\0"
  "incsspd\0" "incsspq\0" "insertq\0" "int3\0" "into\0" "invept\0" "invlpg\0" "invlpga\0" "invpcid\0" "invvpid\0"
  "iretd\0" "iretq\0" "ja\0" "jae\0" "jb\0" "jbe\0" "jc\0" "je\0" "jecxz\0" "jg\0" "jge\0" "jl\0" "jle\0" "jna\0"
  "jnae\0" "jnb\0" "jnbe\0" "jnc\0" "jne\0" "jng\0" "jnge\0" "jnl\0" "jnle\0" "jno\0" "jnp\0" "jns\0" "jnz\0" "jo\0"
  "jp\0" "jpe\0" "jpo\0" "js\0" "jz\0" "kaddb\0" "kaddd\0" "kaddq\0" "kaddw\0" "kandb\0" "kandd\0" "kandnb\0"
  "kandnd\0" "kandnq\0" "kandnw\0" "kandq\0" "kandw\0" "kmovb\0" "kmovw\0" "knotb\0" "knotd\0" "knotq\0" "knotw\0"
  "korb\0" "kord\0" "korq\0" "kortestb\0" "kortestd\0" "kortestq\0" "kortestw\0" "korw\0" "kshiftlb\0" "kshiftld\0"
  "kshiftlq\0" "kshiftlw\0" "kshiftrb\0" "kshiftrd\0" "kshiftrq\0" "kshiftrw\0" "ktestb\0" "ktestd\0" "ktestq\0"
  "ktestw\0" "kunpckbw\0" "kunpckdq\0" "kunpckwd\0" "kxnorb\0" "kxnord\0" "kxnorq\0" "kxnorw\0" "kxorb\0" "kxord\0"
  "kxorq\0" "kxorw\0" "lahf\0" "lar\0" "lcall\0" "lds\0" "ldtilecfg\0" "lea\0" "leave\0" "les\0" "lfence\0" "lfs\0"
  "lgdt\0" "lgs\0" "lidt\0" "ljmp\0" "lldt\0" "llwpcb\0" "lmsw\0" "lods\0" "loop\0" "loope\0" "loopne\0" "lsl\0"
  "ltr\0" "lwpins\0" "lwpval\0" "lzcnt\0" "mcommit\0" "mfence\0" "monitorx\0" "movabs\0" "movdir64b\0" "movdiri\0"
  "movdq2q\0" "movnti\0" "movntq\0" "movntsd\0" "movntss\0" "movq2dq\0" "movsx\0" "movsxd\0" "movzx\0" "mulx\0"
  "mwaitx\0" "neg\0" "not\0" "out\0" "outs\0" "pavgusb\0" "pconfig\0" "pdep\0" "pext\0" "pf2id\0" "pf2iw\0" "pfacc\0"
  "pfadd\0" "pfcmpeq\0" "pfcmpge\0" "pfcmpgt\0" "pfmax\0" "pfmin\0" "pfmul\0" "pfnacc\0" "pfpnacc\0" "pfrcp\0"
  "pfrcpit1\0" "pfrcpit2\0" "pfrcpv\0" "pfrsqit1\0" "pfrsqrt\0" "pfrsqrtv\0" "pfsub\0" "pfsubr\0" "pi2fd\0" "pi2fw\0"
  "pmulhrw\0" "pop\0" "popa\0" "popad\0" "popcnt\0" "popf\0" "popfd\0" "popfq\0" "prefetch\0" "prefetchnta\0"
  "prefetcht0\0" "prefetcht1\0" "prefetcht2\0" "prefetchw\0" "prefetchwt1\0" "pshufw\0" "psmash\0" "pswapd\0"
  "ptwrite\0" "push\0" "pusha\0" "pushad\0" "pushf\0" "pushfd\0" "pushfq\0" "pvalidate\0" "rcl\0" "rcr\0" "rdfsbase\0"
  "rdgsbase\0" "rdmsr\0" "rdpid\0" "rdpkru\0" "rdpmc\0" "rdpru\0" "rdrand\0" "rdseed\0" "rdsspd\0" "rdsspq\0" "rdtsc\0"
  "rdtscp\0" "retf\0" "rmpadjust\0" "rmpupdate\0" "rol\0" "ror\0" "rorx\0" "rsm\0" "rstorssp\0" "sahf\0" "sal\0"
  "sar\0" "sarx\0" "saveprevssp\0" "sbb\0" "scas\0" "senduipi\0" "serialize\0" "seta\0" "setae\0" "setb\0" "setbe\0"
  "setc\0" "sete\0" "setg\0" "setge\0" "setl\0" "setle\0" "setna\0" "setnae\0" "setnb\0" "setnbe\0" "setnc\0" "setne\0"
  "setng\0" "setnge\0" "setnl\0" "setnle\0" "setno\0" "setnp\0" "setns\0" "setnz\0" "seto\0" "setp\0" "setpe\0"
  "setpo\0" "sets\0" "setssbsy\0" "setz\0" "sfence\0" "sgdt\0" "sha1msg1\0" "sha1msg2\0" "sha1nexte\0" "sha1rnds4\0"
  "sha256msg1\0" "sha256msg2\0" "sha256rnds2\0" "shl\0" "shlx\0" "shr\0" "shrd\0" "shrx\0" "sidt\0" "skinit\0" "sldt\0"
  "slwpcb\0" "smsw\0" "stac\0" "stc\0" "stgi\0" "sti\0" "stos\0" "str\0" "sttilecfg\0" "swapgs\0" "syscall\0"
  "sysenter\0" "sysexit\0" "sysexitq\0" "sysret\0" "sysretq\0" "t1mskc\0" "tdpbf16ps\0" "tdpbssd\0" "tdpbsud\0"
  "tdpbusd\0" "tdpbuud\0" "testui\0" "tileloadd\0" "tileloaddt1\0" "tilerelease\0" "tilestored\0" "tilezero\0"
  "tpause\0" "tzcnt\0" "tzmsk\0" "ud0\0" "ud1\0" "ud2\0" "uiret\0" "umonitor\0" "umwait\0" "v4fmaddps\0" "v4fmaddss\0"
  "v4fnmaddps\0" "v4fnmaddss\0" "vaddpd\0" "vaddph\0" "vaddps\0" "vaddsd\0" "vaddsh\0" "vaddss\0" "vaddsubpd\0"
  "vaddsubps\0" "vaesdec\0" "vaesdeclast\0" "vaesenc\0" "vaesenclast\0" "vaesimc\0" "vaeskeygenassist\0" "valignd\0"
  "valignq\0" "vandnpd\0" "vandnps\0" "vandpd\0" "vandps\0" "vblendmpd\0" "vblendmps\0" "vblendpd\0" "vblendps\0"
  "vblendvpd\0" "vblendvps\0" "vbroadcastf128\0" "vbroadcastf32x2\0" "vbroadcastf32x4\0" "vbroadcastf32x8\0"
  "vbroadcastf64x2\0" "vbroadcastf64x4\0" "vbroadcasti128\0" "vbroadcasti32x2\0" "vbroadcasti32x4\0"
  "vbroadcasti32x8\0" "vbroadcasti64x2\0" "vbroadcasti64x4\0" "vbroadcastsd\0" "vbroadcastss\0" "vcmppd\0" "vcmpph\0"
  "vcmpps\0" "vcmpsd\0" "vcmpsh\0" "vcmpss\0" "vcomisd\0" "vcomish\0" "vcomiss\0" "vcompresspd\0" "vcompressps\0"
  "vcvtdq2pd\0" "vcvtdq2ph\0" "vcvtdq2ps\0" "vcvtne2ps2bf16\0" "vcvtneps2bf16\0" "vcvtpd2dq\0" "vcvtpd2ph\0"
  "vcvtpd2ps\0" "vcvtpd2qq\0" "vcvtpd2udq\0" "vcvtpd2uqq\0" "vcvtph2dq\0" "vcvtph2pd\0" "vcvtph2ps\0" "vcvtph2psx\0"
  "vcvtph2qq\0" "vcvtph2udq\0" "vcvtph2uqq\0" "vcvtph2uw\0" "vcvtph2w\0" "vcvtps2dq\0" "vcvtps2pd\0" "vcvtps2ph\0"
  "vcvtps2phx\0" "vcvtps2qq\0" "vcvtps2udq\0" "vcvtps2uqq\0" "vcvtqq2pd\0" "vcvtqq2ph\0" "vcvtqq2ps\0" "vcvtsd2sh\0"
  "vcvtsd2si\0" "vcvtsd2ss\0" "vcvtsd2usi\0" "vcvtsh2sd\0" "vcvtsh2si\0" "vcvtsh2ss\0" "vcvtsh2usi\0" "vcvtsi2sd\0"
  "vcvtsi2sh\0" "vcvtsi2ss\0" "vcvtss2sd\0" "vcvtss2sh\0" "vcvtss2si\0" "vcvtss2usi\0" "vcvttpd2dq\0" "vcvttpd2qq\0"
  "vcvttpd2udq\0" "vcvttpd2uqq\0" "vcvttph2dq\0" "vcvttph2qq\0" "vcvttph2udq\0" "vcvttph2uqq\0" "vcvttph2uw\0"
  "vcvttph2w\0" "vcvttps2dq\0" "vcvttps2qq\0" "vcvttps2udq\0" "vcvttps2uqq\0" "vcvttsd2si\0" "vcvttsd2usi\0"
  "vcvttsh2si\0" "vcvttsh2usi\0" "vcvttss2si\0" "vcvttss2usi\0" "vcvtudq2pd\0" "vcvtudq2ph\0" "vcvtudq2ps\0"
  "vcvtuqq2pd\0" "vcvtuqq2ph\0" "vcvtuqq2ps\0" "vcvtusi2sd\0" "vcvtusi2sh\0" "vcvtusi2ss\0" "vcvtuw2ph\0" "vcvtw2ph\0"
  "vdbpsadbw\0" "vdivpd\0" "vdivph\0" "vdivps\0" "vdivsd\0" "vdivsh\0" "vdivss\0" "vdpbf16ps\0" "vdppd\0" "vdpps\0"
  "verr\0" "verw\0" "vexp2pd\0" "vexp2ps\0" "vexpandpd\0" "vexpandps\0" "vextractf128\0" "vextractf32x4\0"
  "vextractf32x8\0" "vextractf64x2\0" "vextractf64x4\0" "vextracti128\0" "vextracti32x4\0" "vextracti32x8\0"
  "vextracti64x2\0" "vextracti64x4\0" "vextractps\0" "vfcmaddcph\0" "vfcmaddcsh\0" "vfcmulcph\0" "vfcmulcsh\0"
  "vfixupimmpd\0" "vfixupimmps\0" "vfixupimmsd\0" "vfixupimmss\0" "vfmadd132pd\0" "vfmadd132ph\0" "vfmadd132ps\0"
  "vfmadd132sd\0" "vfmadd132sh\0" "vfmadd132ss\0" "vfmadd213pd\0" "vfmadd213ph\0" "vfmadd213ps\0" "vfmadd213sd\0"
  "vfmadd213sh\0" "vfmadd213ss\0" "vfmadd231pd\0" "vfmadd231ph\0" "vfmadd231ps\0" "vfmadd231sd\0" "vfmadd231sh\0"
  "vfmadd231ss\0" "vfmaddcph\0" "vfmaddcsh\0" "vfmaddpd\0" "vfmaddps\0" "vfmaddsd\0" "vfmaddss\0" "vfmaddsub132pd\0"
  "vfmaddsub132ph\0" "vfmaddsub132ps\0" "vfmaddsub213pd\0" "vfmaddsub213ph\0" "vfmaddsub213ps\0" "vfmaddsub231pd\0"
  "vfmaddsub231ph\0" "vfmaddsub231ps\0" "vfmaddsubpd\0" "vfmaddsubps\0" "vfmsub132pd\0" "vfmsub132ph\0" "vfmsub132ps\0"
  "vfmsub132sd\0" "vfmsub132sh\0" "vfmsub132ss\0" "vfmsub213pd\0" "vfmsub213ph\0" "vfmsub213ps\0" "vfmsub213sd\0"
  "vfmsub213sh\0" "vfmsub213ss\0" "vfmsub231pd\0" "vfmsub231ph\0" "vfmsub231ps\0" "vfmsub231sd\0" "vfmsub231sh\0"
  "vfmsub231ss\0" "vfmsubadd132pd\0" "vfmsubadd132ph\0" "vfmsubadd132ps\0" "vfmsubadd213pd\0" "vfmsubadd213ph\0"
  "vfmsubadd213ps\0" "vfmsubadd231pd\0" "vfmsubadd231ph\0" "vfmsubadd231ps\0" "vfmsubaddpd\0" "vfmsubaddps\0"
  "vfmsubpd\0" "vfmsubps\0" "vfmsubsd\0" "vfmsubss\0" "vfmulcph\0" "vfmulcsh\0" "vfnmadd132pd\0" "vfnmadd132ph\0"
  "vfnmadd132ps\0" "vfnmadd132sd\0" "vfnmadd132sh\0" "vfnmadd132ss\0" "vfnmadd213pd\0" "vfnmadd213ph\0"
  "vfnmadd213ps\0" "vfnmadd213sd\0" "vfnmadd213sh\0" "vfnmadd213ss\0" "vfnmadd231pd\0" "vfnmadd231ph\0"
  "vfnmadd231ps\0" "vfnmadd231sd\0" "vfnmadd231sh\0" "vfnmadd231ss\0" "vfnmaddpd\0" "vfnmaddps\0" "vfnmaddsd\0"
  "vfnmaddss\0" "vfnmsub132pd\0" "vfnmsub132ph\0" "vfnmsub132ps\0" "vfnmsub132sd\0" "vfnmsub132sh\0" "vfnmsub132ss\0"
  "vfnmsub213pd\0" "vfnmsub213ph\0" "vfnmsub213ps\0" "vfnmsub213sd\0" "vfnmsub213sh\0" "vfnmsub213ss\0"
  "vfnmsub231pd\0" "vfnmsub231ph\0" "vfnmsub231ps\0" "vfnmsub231sd\0" "vfnmsub231sh\0" "vfnmsub231ss\0" "vfnmsubpd\0"
  "vfnmsubps\0" "vfnmsubsd\0" "vfnmsubss\0" "vfpclasspd\0" "vfpclassph\0" "vfpclassps\0" "vfpclasssd\0" "vfpclasssh\0"
  "vfpclassss\0" "vfrczpd\0" "vfrczps\0" "vfrczsd\0" "vfrczss\0" "vgatherdpd\0" "vgatherdps\0" "vgatherpf0dpd\0"
  "vgatherpf0dps\0" "vgatherpf0qpd\0" "vgatherpf0qps\0" "vgatherpf1dpd\0" "vgatherpf1dps\0" "vgatherpf1qpd\0"
  "vgatherpf1qps\0" "vgatherqpd\0" "vgatherqps\0" "vgetexppd\0" "vgetexpph\0" "vgetexpps\0" "vgetexpsd\0" "vgetexpsh\0"
  "vgetexpss\0" "vgetmantpd\0" "vgetmantph\0" "vgetmantps\0" "vgetmantsd\0" "vgetmantsh\0" "vgetmantss\0"
  "vgf2p8affineinvqb\0" "vgf2p8affineqb\0" "vgf2p8mulb\0" "vhaddpd\0" "vhaddps\0" "vhsubpd\0" "vhsubps\0"
  "vinsertf128\0" "vinsertf32x4\0" "vinsertf32x8\0" "vinsertf64x2\0" "vinsertf64x4\0" "vinserti128\0" "vinserti32x4\0"
  "vinserti32x8\0" "vinserti64x2\0" "vinserti64x4\0" "vinsertps\0" "vlddqu\0" "vldmxcsr\0" "vmaskmovdqu\0"
  "vmaskmovpd\0" "vmaskmovps\0" "vmaxpd\0" "vmaxph\0" "vmaxps\0" "vmaxsd\0" "vmaxsh\0" "vmaxss\0" "vmcall\0"
  "vmclear\0" "vmfunc\0" "vminpd\0" "vminph\0" "vminps\0" "vminsd\0" "vminsh\0" "vminss\0" "vmlaunch\0" "vmload\0"
  "vmmcall\0" "vmovapd\0" "vmovaps\0" "vmovd\0" "vmovddup\0" "vmovdqa\0" "vmovdqa32\0" "vmovdqa64\0" "vmovdqu\0"
  "vmovdqu16\0" "vmovdqu32\0" "vmovdqu64\0" "vmovdqu8\0" "vmovhlps\0" "vmovhpd\0" "vmovhps\0" "vmovlhps\0" "vmovlpd\0"
  "vmovlps\0" "vmovmskpd\0" "vmovmskps\0" "vmovntdq\0" "vmovntdqa\0" "vmovntpd\0" "vmovntps\0" "vmovq\0" "vmovsd\0"
  "vmovsh\0" "vmovshdup\0" "vmovsldup\0" "vmovss\0" "vmovupd\0" "vmovups\0" "vmovw\0" "vmpsadbw\0" "vmptrld\0"
  "vmptrst\0" "vmread\0" "vmresume\0" "vmrun\0" "vmsave\0" "vmulpd\0" "vmulph\0" "vmulps\0" "vmulsd\0" "vmulsh\0"
  "vmulss\0" "vmwrite\0" "vmxon\0" "vorpd\0" "vorps\0" "vp2intersectd\0" "vp2intersectq\0" "vp4dpwssd\0" "vp4dpwssds\0"
  "vpabsb\0" "vpabsd\0" "vpabsq\0" "vpabsw\0" "vpackssdw\0" "vpacksswb\0" "vpackusdw\0" "vpackuswb\0" "vpaddb\0"
  "vpaddd\0" "vpaddq\0" "vpaddsb\0" "vpaddsw\0" "vpaddusb\0" "vpaddusw\0" "vpaddw\0" "vpalignr\0" "vpand\0" "vpandd\0"
  "vpandn\0" "vpandnd\0" "vpandnq\0" "vpandq\0" "vpavgb\0" "vpavgw\0" "vpblendd\0" "vpblendmb\0" "vpblendmd\0"
  "vpblendmq\0" "vpblendmw\0" "vpblendvb\0" "vpblendw\0" "vpbroadcastb\0" "vpbroadcastd\0" "vpbroadcastmb2q\0"
  "vpbroadcastmw2d\0" "vpbroadcastq\0" "vpbroadcastw\0" "vpclmulqdq\0" "vpcmov\0" "vpcmpb\0" "vpcmpd\0" "vpcmpeqb\0"
  "vpcmpeqd\0" "vpcmpeqq\0" "vpcmpeqw\0" "vpcmpestri\0" "vpcmpestrm\0" "vpcmpgtb\0" "vpcmpgtd\0" "vpcmpgtq\0"
  "vpcmpgtw\0" "vpcmpistri\0" "vpcmpistrm\0" "vpcmpq\0" "vpcmpub\0" "vpcmpud\0" "vpcmpuq\0" "vpcmpuw\0" "vpcmpw\0"
  "vpcomb\0" "vpcomd\0" "vpcompressb\0" "vpcompressd\0" "vpcompressq\0" "vpcompressw\0" "vpcomq\0" "vpcomub\0"
  "vpcomud\0" "vpcomuq\0" "vpcomuw\0" "vpcomw\0" "vpconflictd\0" "vpconflictq\0" "vpdpbusd\0" "vpdpbusds\0"
  "vpdpwssd\0" "vpdpwssds\0" "vperm2f128\0" "vperm2i128\0" "vpermb\0" "vpermd\0" "vpermi2b\0" "vpermi2d\0"
  "vpermi2pd\0" "vpermi2ps\0" "vpermi2q\0" "vpermi2w\0" "vpermil2pd\0" "vpermil2ps\0" "vpermilpd\0" "vpermilps\0"
  "vpermpd\0" "vpermps\0" "vpermq\0" "vpermt2b\0" "vpermt2d\0" "vpermt2pd\0" "vpermt2ps\0" "vpermt2q\0" "vpermt2w\0"
  "vpermw\0" "vpexpandb\0" "vpexpandd\0" "vpexpandq\0" "vpexpandw\0" "vpextrb\0" "vpextrd\0" "vpextrq\0" "vpextrw\0"
  "vpgatherdd\0" "vpgatherdq\0" "vpgatherqd\0" "vpgatherqq\0" "vphaddbd\0" "vphaddbq\0" "vphaddbw\0" "vphaddd\0"
  "vphadddq\0" "vphaddsw\0" "vphaddubd\0" "vphaddubq\0" "vphaddubw\0" "vphaddudq\0" "vphadduwd\0" "vphadduwq\0"
  "vphaddw\0" "vphaddwd\0" "vphaddwq\0" "vphminposuw\0" "vphsubbw\0" "vphsubd\0" "vphsubdq\0" "vphsubsw\0" "vphsubw\0"
  "vphsubwd\0" "vpinsrb\0" "vpinsrd\0" "vpinsrq\0" "vpinsrw\0" "vplzcntd\0" "vplzcntq\0" "vpmacsdd\0" "vpmacsdqh\0"
  "vpmacsdql\0" "vpmacssdd\0" "vpmacssdqh\0" "vpmacssdql\0" "vpmacsswd\0" "vpmacssww\0" "vpmacswd\0" "vpmacsww\0"
  "vpmadcsswd\0" "vpmadcswd\0" "vpmadd52huq\0" "vpmadd52luq\0" "vpmaddubsw\0" "vpmaddwd\0" "vpmaskmovd\0"
  "vpmaskmovq\0" "vpmaxsb\0" "vpmaxsd\0" "vpmaxsq\0" "vpmaxsw\0" "vpmaxub\0" "vpmaxud\0" "vpmaxuq\0" "vpmaxuw\0"
  "vpminsb\0" "vpminsd\0" "vpminsq\0" "vpminsw\0" "vpminub\0" "vpminud\0" "vpminuq\0" "vpminuw\0" "vpmovb2m\0"
  "vpmovd2m\0" "vpmovdb\0" "vpmovdw\0" "vpmovm2b\0" "vpmovm2d\0" "vpmovm2q\0" "vpmovm2w\0" "vpmovmskb\0" "vpmovq2m\0"
  "vpmovqb\0" "vpmovqd\0" "vpmovqw\0" "vpmovsdb\0" "vpmovsdw\0" "vpmovsqb\0" "vpmovsqd\0" "vpmovsqw\0" "vpmovswb\0"
  "vpmovsxbd\0" "vpmovsxbq\0" "vpmovsxbw\0" "vpmovsxdq\0" "vpmovsxwd\0" "vpmovsxwq\0" "vpmovusdb\0" "vpmovusdw\0"
  "vpmovusqb\0" "vpmovusqd\0" "vpmovusqw\0" "vpmovuswb\0" "vpmovw2m\0" "vpmovwb\0" "vpmovzxbd\0" "vpmovzxbq\0"
  "vpmovzxbw\0" "vpmovzxdq\0" "vpmovzxwd\0" "vpmovzxwq\0" "vpmuldq\0" "vpmulhrsw\0" "vpmulhuw\0" "vpmulhw\0"
  "vpmulld\0" "vpmullq\0" "vpmullw\0" "vpmultishiftqb\0" "vpmuludq\0" "vpopcntb\0" "vpopcntd\0" "vpopcntq\0"
  "vpopcntw\0" "vpor\0" "vpord\0" "vporq\0" "vpperm\0" "vprold\0" "vprolq\0" "vprolvd\0" "vprolvq\0" "vprord\0"
  "vprorq\0" "vprorvd\0" "vprorvq\0" "vprotb\0" "vprotd\0" "vprotq\0" "vprotw\0" "vpsadbw\0" "vpscatterdd\0"
  "vpscatterdq\0" "vpscatterqd\0" "vpscatterqq\0" "vpshab\0" "vpshad\0" "vpshaq\0" "vpshaw\0" "vpshlb\0" "vpshld\0"
  "vpshldd\0" "vpshldq\0" "vpshldvd\0" "vpshldvq\0" "vpshldvw\0" "vpshldw\0" "vpshlq\0" "vpshlw\0" "vpshrdd\0"
  "vpshrdq\0" "vpshrdvd\0" "vpshrdvq\0" "vpshrdvw\0" "vpshrdw\0" "vpshufb\0" "vpshufbitqmb\0" "vpshufd\0" "vpshufhw\0"
  "vpshuflw\0" "vpsignb\0" "vpsignd\0" "vpsignw\0" "vpslld\0" "vpslldq\0" "vpsllq\0" "vpsllvd\0" "vpsllvq\0"
  "vpsllvw\0" "vpsllw\0" "vpsrad\0" "vpsraq\0" "vpsravd\0" "vpsravq\0" "vpsravw\0" "vpsraw\0" "vpsrld\0" "vpsrldq\0"
  "vpsrlq\0" "vpsrlvd\0" "vpsrlvq\0" "vpsrlvw\0" "vpsrlw\0" "vpsubb\0" "vpsubd\0" "vpsubq\0" "vpsubsb\0" "vpsubsw\0"
  "vpsubusb\0" "vpsubusw\0" "vpsubw\0" "vpternlogd\0" "vpternlogq\0" "vptest\0" "vptestmb\0" "vptestmd\0" "vptestmq\0"
  "vptestmw\0" "vptestnmb\0" "vptestnmd\0" "vptestnmq\0" "vptestnmw\0" "vpunpckhbw\0" "vpunpckhdq\0" "vpunpckhqdq\0"
  "vpunpckhwd\0" "vpunpcklbw\0" "vpunpckldq\0" "vpunpcklqdq\0" "vpunpcklwd\0" "vpxor\0" "vpxord\0" "vpxorq\0"
  "vrangepd\0" "vrangeps\0" "vrangesd\0" "vrangess\0" "vrcp14pd\0" "vrcp14ps\0" "vrcp14sd\0" "vrcp14ss\0" "vrcp28pd\0"
  "vrcp28ps\0" "vrcp28sd\0" "vrcp28ss\0" "vrcpph\0" "vrcpps\0" "vrcpsh\0" "vrcpss\0" "vreducepd\0" "vreduceph\0"
  "vreduceps\0" "vreducesd\0" "vreducesh\0" "vreducess\0" "vrndscalepd\0" "vrndscaleph\0" "vrndscaleps\0"
  "vrndscalesd\0" "vrndscalesh\0" "vrndscaless\0" "vroundpd\0" "vroundps\0" "vroundsd\0" "vroundss\0" "vrsqrt14pd\0"
  "vrsqrt14ps\0" "vrsqrt14sd\0" "vrsqrt14ss\0" "vrsqrt28pd\0" "vrsqrt28ps\0" "vrsqrt28sd\0" "vrsqrt28ss\0" "vrsqrtph\0"
  "vrsqrtps\0" "vrsqrtsh\0" "vrsqrtss\0" "vscalefpd\0" "vscalefph\0" "vscalefps\0" "vscalefsd\0" "vscalefsh\0"
  "vscalefss\0" "vscatterdpd\0" "vscatterdps\0" "vscatterpf0dpd\0" "vscatterpf0dps\0" "vscatterpf0qpd\0"
  "vscatterpf0qps\0" "vscatterpf1dpd\0" "vscatterpf1dps\0" "vscatterpf1qpd\0" "vscatterpf1qps\0" "vscatterqpd\0"
  "vscatterqps\0" "vshuff32x4\0" "vshuff64x2\0" "vshufi32x4\0" "vshufi64x2\0" "vshufpd\0" "vshufps\0" "vsqrtpd\0"
  "vsqrtph\0" "vsqrtps\0" "vsqrtsd\0" "vsqrtsh\0" "vsqrtss\0" "vstmxcsr\0" "vsubpd\0" "vsubph\0" "vsubps\0" "vsubsd\0"
  "vsubsh\0" "vsubss\0" "vtestpd\0" "vtestps\0" "vucomisd\0" "vucomish\0" "vucomiss\0" "vunpckhpd\0" "vunpckhps\0"
  "vunpcklpd\0" "vunpcklps\0" "vxorpd\0" "vxorps\0" "vzeroall\0" "vzeroupper\0" "wbinvd\0" "wbnoinvd\0" "wrfsbase\0"
  "wrgsbase\0" "wrmsr\0" "wrssd\0" "wrssq\0" "wrussd\0" "wrussq\0" "xabort\0" "xadd\0" "xbegin\0" "xend\0" "xgetbv\0"
  "xlatb\0" "xresldtrk\0" "xrstors\0" "xrstors64\0" "xsavec\0" "xsavec64\0" "xsaveopt\0" "xsaveopt64\0" "xsaves\0"
  "xsaves64\0" "xsetbv\0" "xsusldtrk\0" "xtest";

const InstDB::InstNameIndex InstDB::instNameIndex[26] = {
  { Inst::kIdAaa          , Inst::kIdArpl          + 1 },
  { Inst::kIdBextr        , Inst::kIdBzhi          + 1 },
  { Inst::kIdCall         , Inst::kIdCwde          + 1 },
  { Inst::kIdDaa          , Inst::kIdDpps          + 1 },
  { Inst::kIdEmms         , Inst::kIdExtrq         + 1 },
  { Inst::kIdF2xm1        , Inst::kIdFyl2xp1       + 1 },
  { Inst::kIdGetsec       , Inst::kIdGf2p8mulb     + 1 },
  { Inst::kIdHaddpd       , Inst::kIdHsubps        + 1 },
  { Inst::kIdIdiv         , Inst::kIdIretq         + 1 },
  { Inst::kIdJa           , Inst::kIdJz            + 1 },
  { Inst::kIdKaddb        , Inst::kIdKxorw         + 1 },
  { Inst::kIdLahf         , Inst::kIdLzcnt         + 1 },
  { Inst::kIdMaskmovdqu   , Inst::kIdMwaitx        + 1 },
  { Inst::kIdNeg          , Inst::kIdNot           + 1 },
  { Inst::kIdOr           , Inst::kIdOuts          + 1 },
  { Inst::kIdPabsb        , Inst::kIdPxor          + 1 },
  { Inst::kIdNone         , Inst::kIdNone          + 1 },
  { Inst::kIdRcl          , Inst::kIdRstorssp      + 1 },
  { Inst::kIdSahf         , Inst::kIdSysretq       + 1 },
  { Inst::kIdT1mskc       , Inst::kIdTzmsk         + 1 },
  { Inst::kIdUcomisd      , Inst::kIdUnpcklps      + 1 },
  { Inst::kIdV4fmaddps    , Inst::kIdVzeroupper    + 1 },
  { Inst::kIdWbinvd       , Inst::kIdWrussq        + 1 },
  { Inst::kIdXabort       , Inst::kIdXtest         + 1 },
  { Inst::kIdNone         , Inst::kIdNone          + 1 },
  { Inst::kIdNone         , Inst::kIdNone          + 1 }
};
// ----------------------------------------------------------------------------
// ${NameData:End}
#endif // !ASMJIT_NO_TEXT

// x86::InstDB - InstSignature & OpSignature
// =========================================

#ifndef ASMJIT_NO_VALIDATION
// ${InstSignatureTable:Begin}
// ------------------- Automatically generated, do not edit -------------------
#define ROW(count, x86, x64, implicit, o0, o1, o2, o3, o4, o5)       \
  { count, uint8_t(x86 ? uint8_t(InstDB::Mode::kX86) : uint8_t(0)) | \
                  (x64 ? uint8_t(InstDB::Mode::kX64) : uint8_t(0)) , \
    implicit,                                                        \
    0,                                                               \
    { o0, o1, o2, o3, o4, o5 }                                       \
  }
const InstDB::InstSignature InstDB::_instSignatureTable[] = {
  ROW(2, 1, 1, 0, 1  , 2  , 0  , 0  , 0  , 0  ), // #0   {r8lo|r8hi|m8|mem, r8lo|r8hi}
  ROW(2, 1, 1, 0, 3  , 4  , 0  , 0  , 0  , 0  ), //      {r16|m16|mem|sreg, r16}
  ROW(2, 1, 1, 0, 5  , 6  , 0  , 0  , 0  , 0  ), //      {r32|m32|mem|sreg, r32}
  ROW(2, 0, 1, 0, 7  , 8  , 0  , 0  , 0  , 0  ), //      {r64|m64|mem|sreg|creg|dreg, r64}
  ROW(2, 1, 1, 0, 9  , 10 , 0  , 0  , 0  , 0  ), //      {r8lo|r8hi|m8, i8|u8}
  ROW(2, 1, 1, 0, 11 , 12 , 0  , 0  , 0  , 0  ), //      {r16|m16, i16|u16}
  ROW(2, 1, 1, 0, 13 , 14 , 0  , 0  , 0  , 0  ), //      {r32|m32, i32|u32}
  ROW(2, 0, 1, 0, 15 , 16 , 0  , 0  , 0  , 0  ), //      {r64|m64|mem, i32}
  ROW(2, 0, 1, 0, 8  , 17 , 0  , 0  , 0  , 0  ), //      {r64, i64|u64|m64|mem|sreg|creg|dreg}
  ROW(2, 1, 1, 0, 2  , 18 , 0  , 0  , 0  , 0  ), //      {r8lo|r8hi, m8|mem}
  ROW(2, 1, 1, 0, 4  , 19 , 0  , 0  , 0  , 0  ), //      {r16, m16|mem|sreg}
  ROW(2, 1, 1, 0, 6  , 20 , 0  , 0  , 0  , 0  ), //      {r32, m32|mem|sreg}
  ROW(2, 1, 1, 0, 21 , 22 , 0  , 0  , 0  , 0  ), //      {m16|mem, sreg}
  ROW(2, 1, 1, 0, 22 , 21 , 0  , 0  , 0  , 0  ), //      {sreg, m16|mem}
  ROW(2, 1, 0, 0, 6  , 23 , 0  , 0  , 0  , 0  ), //      {r32, creg|dreg}
  ROW(2, 1, 0, 0, 23 , 6  , 0  , 0  , 0  , 0  ), //      {creg|dreg, r32}
  ROW(2, 1, 1, 0, 9  , 10 , 0  , 0  , 0  , 0  ), // #16  {r8lo|r8hi|m8, i8|u8}
  ROW(2, 1, 1, 0, 11 , 12 , 0  , 0  , 0  , 0  ), //      {r16|m16, i16|u16}
  ROW(2, 1, 1, 0, 13 , 14 , 0  , 0  , 0  , 0  ), //      {r32|m32, i32|u32}
  ROW(2, 0, 1, 0, 15 , 24 , 0  , 0  , 0  , 0  ), //      {r64|m64|mem, i32|r64}
  ROW(2, 1, 1, 0, 25 , 26 , 0  , 0  , 0  , 0  ), //      {r16|m16|r32|m32|r64|m64|mem, i8}
  ROW(2, 1, 1, 0, 1  , 2  , 0  , 0  , 0  , 0  ), //      {r8lo|r8hi|m8|mem, r8lo|r8hi}
  ROW(2, 1, 1, 0, 27 , 4  , 0  , 0  , 0  , 0  ), //      {r16|m16|mem, r16}
  ROW(2, 1, 1, 0, 28 , 6  , 0  , 0  , 0  , 0  ), // #23  {r32|m32|mem, r32}
  ROW(2, 1, 1, 0, 2  , 18 , 0  , 0  , 0  , 0  ), //      {r8lo|r8hi, m8|mem}
  ROW(2, 1, 1, 0, 4  , 21 , 0  , 0  , 0  , 0  ), //      {r16, m16|mem}
  ROW(2, 1, 1, 0, 6  , 29 , 0  , 0  , 0  , 0  ), //      {r32, m32|mem}
  ROW(2, 0, 1, 0, 8  , 30 , 0  , 0  , 0  , 0  ), //      {r64, m64|mem}
  ROW(2, 1, 1, 0, 31 , 10 , 0  , 0  , 0  , 0  ), // #28  {r8lo|r8hi|m8|r16|m16|r32|m32|r64|m64|mem, i8|u8}
  ROW(2, 1, 1, 0, 11 , 12 , 0  , 0  , 0  , 0  ), //      {r16|m16, i16|u16}
  ROW(2, 1, 1, 0, 13 , 14 , 0  , 0  , 0  , 0  ), //      {r32|m32, i32|u32}
  ROW(2, 0, 1, 0, 8  , 32 , 0  , 0  , 0  , 0  ), //      {r64, u32|i32|r64|m64|mem}
  ROW(2, 0, 1, 0, 30 , 24 , 0  , 0  , 0  , 0  ), //      {m64|mem, i32|r64}
  ROW(2, 1, 1, 0, 1  , 2  , 0  , 0  , 0  , 0  ), //      {r8lo|r8hi|m8|mem, r8lo|r8hi}
  ROW(2, 1, 1, 0, 27 , 4  , 0  , 0  , 0  , 0  ), //      {r16|m16|mem, r16}
  ROW(2, 1, 1, 0, 28 , 6  , 0  , 0  , 0  , 0  ), //      {r32|m32|mem, r32}
  ROW(2, 1, 1, 0, 2  , 18 , 0  , 0  , 0  , 0  ), //      {r8lo|r8hi, m8|mem}
  ROW(2, 1, 1, 0, 4  , 21 , 0  , 0  , 0  , 0  ), //      {r16, m16|mem}
  ROW(2, 1, 1, 0, 6  , 29 , 0  , 0  , 0  , 0  ), //      {r32, m32|mem}
  ROW(2, 1, 1, 1, 33 , 1  , 0  , 0  , 0  , 0  ), // #39  {<ax>, r8lo|r8hi|m8|mem}
  ROW(3, 1, 1, 2, 34 , 33 , 27 , 0  , 0  , 0  ), //      {<dx>, <ax>, r16|m16|mem}
  ROW(3, 1, 1, 2, 35 , 36 , 28 , 0  , 0  , 0  ), //      {<edx>, <eax>, r32|m32|mem}
  ROW(3, 0, 1, 2, 37 , 38 , 15 , 0  , 0  , 0  ), //      {<rdx>, <rax>, r64|m64|mem}
  ROW(2, 1, 1, 0, 4  , 39 , 0  , 0  , 0  , 0  ), //      {r16, r16|m16|mem|i8|i16}
  ROW(2, 1, 1, 0, 6  , 40 , 0  , 0  , 0  , 0  ), //      {r32, r32|m32|mem|i8|i32}
  ROW(2, 0, 1, 0, 8  , 41 , 0  , 0  , 0  , 0  ), //      {r64, r64|m64|mem|i8|i32}
  ROW(3, 1, 1, 0, 4  , 27 , 42 , 0  , 0  , 0  ), //      {r16, r16|m16|mem, i8|i16|u16}
  ROW(3, 1, 1, 0, 6  , 28 , 43 , 0  , 0  , 0  ), //      {r32, r32|m32|mem, i8|i32|u32}
  ROW(3, 0, 1, 0, 8  , 15 , 44 , 0  , 0  , 0  ), //      {r64, r64|m64|mem, i8|i32}
  ROW(2, 0, 1, 0, 8  , 45 , 0  , 0  , 0  , 0  ), // #49  {r64, i64|u64}
  ROW(2, 0, 1, 0, 46 , 18 , 0  , 0  , 0  , 0  ), //      {al, m8|mem}
  ROW(2, 0, 1, 0, 47 , 21 , 0  , 0  , 0  , 0  ), //      {ax, m16|mem}
  ROW(2, 0, 1, 0, 48 , 29 , 0  , 0  , 0  , 0  ), //      {eax, m32|mem}
  ROW(2, 0, 1, 0, 49 , 30 , 0  , 0  , 0  , 0  ), //      {rax, m64|mem}
  ROW(2, 0, 1, 0, 18 , 46 , 0  , 0  , 0  , 0  ), //      {m8|mem, al}
  ROW(2, 0, 1, 0, 21 , 47 , 0  , 0  , 0  , 0  ), //      {m16|mem, ax}
  ROW(2, 0, 1, 0, 29 , 48 , 0  , 0  , 0  , 0  ), //      {m32|mem, eax}
  ROW(2, 0, 1, 0, 30 , 49 , 0  , 0  , 0  , 0  ), //      {m64|mem, rax}
  ROW(2, 1, 1, 0, 1  , 2  , 0  , 0  , 0  , 0  ), // #58  {r8lo|r8hi|m8|mem, r8lo|r8hi}
  ROW(2, 1, 1, 0, 27 , 4  , 0  , 0  , 0  , 0  ), //      {r16|m16|mem, r16}
  ROW(2, 1, 1, 0, 28 , 6  , 0  , 0  , 0  , 0  ), //      {r32|m32|mem, r32}
  ROW(2, 0, 1, 0, 15 , 8  , 0  , 0  , 0  , 0  ), // #61  {r64|m64|mem, r64}
  ROW(2, 1, 1, 0, 2  , 18 , 0  , 0  , 0  , 0  ), //      {r8lo|r8hi, m8|mem}
  ROW(2, 1, 1, 0, 4  , 21 , 0  , 0  , 0  , 0  ), //      {r16, m16|mem}
  ROW(2, 1, 1, 0, 6  , 29 , 0  , 0  , 0  , 0  ), //      {r32, m32|mem}
  ROW(2, 0, 1, 0, 8  , 30 , 0  , 0  , 0  , 0  ), //      {r64, m64|mem}
  ROW(2, 1, 1, 0, 9  , 10 , 0  , 0  , 0  , 0  ), // #66  {r8lo|r8hi|m8, i8|u8}
  ROW(2, 1, 1, 0, 11 , 12 , 0  , 0  , 0  , 0  ), //      {r16|m16, i16|u16}
  ROW(2, 1, 1, 0, 13 , 14 , 0  , 0  , 0  , 0  ), //      {r32|m32, i32|u32}
  ROW(2, 0, 1, 0, 15 , 24 , 0  , 0  , 0  , 0  ), //      {r64|m64|mem, i32|r64}
  ROW(2, 1, 1, 0, 1  , 2  , 0  , 0  , 0  , 0  ), //      {r8lo|r8hi|m8|mem, r8lo|r8hi}
  ROW(2, 1, 1, 0, 27 , 4  , 0  , 0  , 0  , 0  ), //      {r16|m16|mem, r16}
  ROW(2, 1, 1, 0, 28 , 6  , 0  , 0  , 0  , 0  ), //      {r32|m32|mem, r32}
  ROW(2, 1, 1, 0, 4  , 21 , 0  , 0  , 0  , 0  ), // #73  {r16, m16|mem}
  ROW(2, 1, 1, 0, 6  , 29 , 0  , 0  , 0  , 0  ), //      {r32, m32|mem}
  ROW(2, 0, 1, 0, 8  , 30 , 0  , 0  , 0  , 0  ), //      {r64, m64|mem}
  ROW(2, 1, 1, 0, 21 , 4  , 0  , 0  , 0  , 0  ), //      {m16|mem, r16}
  ROW(2, 1, 1, 0, 29 , 6  , 0  , 0  , 0  , 0  ), // #77  {m32|mem, r32}
  ROW(2, 0, 1, 0, 30 , 8  , 0  , 0  , 0  , 0  ), //      {m64|mem, r64}
  ROW(2, 1, 1, 0, 50 , 51 , 0  , 0  , 0  , 0  ), // #79  {xmm, xmm|m128|mem}
  ROW(2, 1, 1, 0, 52 , 50 , 0  , 0  , 0  , 0  ), // #80  {m128|mem, xmm}
  ROW(2, 1, 1, 0, 53 , 54 , 0  , 0  , 0  , 0  ), //      {ymm, ymm|m256|mem}
  ROW(2, 1, 1, 0, 55 , 53 , 0  , 0  , 0  , 0  ), //      {m256|mem, ymm}
  ROW(2, 1, 1, 0, 56 , 57 , 0  , 0  , 0  , 0  ), // #83  {zmm, zmm|m512|mem}
  ROW(2, 1, 1, 0, 58 , 56 , 0  , 0  , 0  , 0  ), //      {m512|mem, zmm}
  ROW(3, 1, 1, 0, 50 , 50 , 59 , 0  , 0  , 0  ), // #85  {xmm, xmm, xmm|m128|mem|i8|u8}
  ROW(3, 1, 1, 0, 50 , 52 , 10 , 0  , 0  , 0  ), //      {xmm, m128|mem, i8|u8}
  ROW(3, 1, 1, 0, 53 , 53 , 60 , 0  , 0  , 0  ), //      {ymm, ymm, ymm|m256|mem|i8|u8}
  ROW(3, 1, 1, 0, 53 , 55 , 10 , 0  , 0  , 0  ), //      {ymm, m256|mem, i8|u8}
  ROW(3, 1, 1, 0, 56 , 56 , 61 , 0  , 0  , 0  ), //      {zmm, zmm, zmm|m512|mem|i8|u8}
  ROW(3, 1, 1, 0, 56 , 58 , 10 , 0  , 0  , 0  ), //      {zmm, m512|mem, i8|u8}
  ROW(3, 1, 1, 0, 50 , 50 , 59 , 0  , 0  , 0  ), // #91  {xmm, xmm, i8|u8|xmm|m128|mem}
  ROW(3, 1, 1, 0, 53 , 53 , 59 , 0  , 0  , 0  ), //      {ymm, ymm, i8|u8|xmm|m128|mem}
  ROW(3, 1, 1, 0, 50 , 52 , 10 , 0  , 0  , 0  ), //      {xmm, m128|mem, i8|u8}
  ROW(3, 1, 1, 0, 53 , 55 , 10 , 0  , 0  , 0  ), //      {ymm, m256|mem, i8|u8}
  ROW(3, 1, 1, 0, 56 , 56 , 59 , 0  , 0  , 0  ), //      {zmm, zmm, xmm|m128|mem|i8|u8}
  ROW(3, 1, 1, 0, 56 , 58 , 10 , 0  , 0  , 0  ), //      {zmm, m512|mem, i8|u8}
  ROW(3, 1, 1, 0, 50 , 50 , 59 , 0  , 0  , 0  ), // #97  {xmm, xmm, xmm|m128|mem|i8|u8}
  ROW(3, 1, 1, 0, 50 , 52 , 10 , 0  , 0  , 0  ), //      {xmm, m128|mem, i8|u8}
  ROW(3, 1, 1, 0, 53 , 53 , 59 , 0  , 0  , 0  ), //      {ymm, ymm, xmm|m128|mem|i8|u8}
  ROW(3, 1, 1, 0, 53 , 55 , 10 , 0  , 0  , 0  ), //      {ymm, m256|mem, i8|u8}
  ROW(3, 1, 1, 0, 56 , 56 , 59 , 0  , 0  , 0  ), //      {zmm, zmm, xmm|m128|mem|i8|u8}
  ROW(3, 1, 1, 0, 56 , 58 , 10 , 0  , 0  , 0  ), //      {zmm, m512|mem, i8|u8}
  ROW(2, 1, 1, 0, 62 , 63 , 0  , 0  , 0  , 0  ), // #103 {mm, mm|m64|mem|r64}
  ROW(2, 1, 1, 0, 15 , 64 , 0  , 0  , 0  , 0  ), //      {m64|mem|r64, mm|xmm}
  ROW(2, 0, 1, 0, 50 , 15 , 0  , 0  , 0  , 0  ), //      {xmm, r64|m64|mem}
  ROW(2, 1, 1, 0, 50 , 65 , 0  , 0  , 0  , 0  ), // #106 {xmm, xmm|m64|mem}
  ROW(2, 1, 1, 0, 30 , 50 , 0  , 0  , 0  , 0  ), // #107 {m64|mem, xmm}
  ROW(0, 1, 1, 0, 0  , 0  , 0  , 0  , 0  , 0  ), // #108 {}
  ROW(1, 1, 1, 0, 66 , 0  , 0  , 0  , 0  , 0  ), //      {r16|m16|r32|m32|r64|m64}
  ROW(2, 1, 1, 0, 27 , 4  , 0  , 0  , 0  , 0  ), //      {r16|m16|mem, r16}
  ROW(2, 1, 1, 0, 28 , 6  , 0  , 0  , 0  , 0  ), //      {r32|m32|mem, r32}
  ROW(2, 1, 1, 0, 15 , 8  , 0  , 0  , 0  , 0  ), //      {r64|m64|mem, r64}
  ROW(3, 1, 1, 0, 50 , 67 , 50 , 0  , 0  , 0  ), // #113 {xmm, vm32x, xmm}
  ROW(3, 1, 1, 0, 53 , 68 , 53 , 0  , 0  , 0  ), //      {ymm, vm32y, ymm}
  ROW(2, 1, 1, 0, 50 , 67 , 0  , 0  , 0  , 0  ), //      {xmm, vm32x}
  ROW(2, 1, 1, 0, 53 , 68 , 0  , 0  , 0  , 0  ), //      {ymm, vm32y}
  ROW(2, 1, 1, 0, 56 , 69 , 0  , 0  , 0  , 0  ), //      {zmm, vm32z}
  ROW(3, 1, 1, 0, 50 , 70 , 50 , 0  , 0  , 0  ), // #118 {xmm, vm64x, xmm}
  ROW(3, 1, 1, 0, 53 , 71 , 53 , 0  , 0  , 0  ), //      {ymm, vm64y, ymm}
  ROW(2, 1, 1, 0, 50 , 70 , 0  , 0  , 0  , 0  ), //      {xmm, vm64x}
  ROW(2, 1, 1, 0, 53 , 71 , 0  , 0  , 0  , 0  ), //      {ymm, vm64y}
  ROW(2, 1, 1, 0, 56 , 72 , 0  , 0  , 0  , 0  ), //      {zmm, vm64z}
  ROW(2, 1, 1, 0, 25 , 10 , 0  , 0  , 0  , 0  ), // #123 {r16|m16|r32|m32|r64|m64|mem, i8|u8}
  ROW(2, 1, 1, 0, 27 , 4  , 0  , 0  , 0  , 0  ), //      {r16|m16|mem, r16}
  ROW(2, 1, 1, 0, 28 , 6  , 0  , 0  , 0  , 0  ), //      {r32|m32|mem, r32}
  ROW(2, 0, 1, 0, 15 , 8  , 0  , 0  , 0  , 0  ), //      {r64|m64|mem, r64}
  ROW(2, 1, 1, 2, 73 , 74 , 0  , 0  , 0  , 0  ), // #127 {<ds:[m8|memBase|zsi]>, <es:[m8|memBase|zdi]>}
  ROW(2, 1, 1, 2, 75 , 76 , 0  , 0  , 0  , 0  ), //      {<ds:[m16|memBase|zsi]>, <es:[m16|memBase|zdi]>}
  ROW(2, 1, 1, 2, 77 , 78 , 0  , 0  , 0  , 0  ), //      {<ds:[m32|memBase|zsi]>, <es:[m32|memBase|zdi]>}
  ROW(2, 0, 1, 2, 79 , 80 , 0  , 0  , 0  , 0  ), //      {<ds:[m64|memBase|zsi]>, <es:[m64|memBase|zdi]>}
  ROW(3, 1, 1, 1, 1  , 2  , 81 , 0  , 0  , 0  ), // #131 {r8lo|r8hi|m8|mem, r8lo|r8hi, <al>}
  ROW(3, 1, 1, 1, 27 , 4  , 33 , 0  , 0  , 0  ), //      {r16|m16|mem, r16, <ax>}
  ROW(3, 1, 1, 1, 28 , 6  , 36 , 0  , 0  , 0  ), //      {r32|m32|mem, r32, <eax>}
  ROW(3, 0, 1, 1, 15 , 8  , 38 , 0  , 0  , 0  ), //      {r64|m64|mem, r64, <rax>}
  ROW(2, 1, 1, 2, 81 , 82 , 0  , 0  , 0  , 0  ), // #135 {<al>, <ds:[m8|memBase|zsi|mem]>}
  ROW(2, 1, 1, 2, 33 , 83 , 0  , 0  , 0  , 0  ), //      {<ax>, <ds:[m16|memBase|zsi|mem]>}
  ROW(2, 1, 1, 2, 36 , 84 , 0  , 0  , 0  , 0  ), //      {<eax>, <ds:[m32|memBase|zsi|mem]>}
  ROW(2, 0, 1, 2, 38 , 85 , 0  , 0  , 0  , 0  ), //      {<rax>, <ds:[m64|memBase|zsi|mem]>}
  ROW(2, 1, 1, 2, 74 , 73 , 0  , 0  , 0  , 0  ), // #139 {<es:[m8|memBase|zdi]>, <ds:[m8|memBase|zsi]>}
  ROW(2, 1, 1, 2, 76 , 75 , 0  , 0  , 0  , 0  ), //      {<es:[m16|memBase|zdi]>, <ds:[m16|memBase|zsi]>}
  ROW(2, 1, 1, 2, 78 , 77 , 0  , 0  , 0  , 0  ), //      {<es:[m32|memBase|zdi]>, <ds:[m32|memBase|zsi]>}
  ROW(2, 0, 1, 2, 80 , 79 , 0  , 0  , 0  , 0  ), //      {<es:[m64|memBase|zdi]>, <ds:[m64|memBase|zsi]>}
  ROW(1, 1, 1, 0, 86 , 0  , 0  , 0  , 0  , 0  ), // #143 {r16|m16|r64|m64}
  ROW(1, 1, 0, 0, 13 , 0  , 0  , 0  , 0  , 0  ), //      {r32|m32}
  ROW(1, 1, 0, 0, 87 , 0  , 0  , 0  , 0  , 0  ), //      {ds|es|ss}
  ROW(1, 1, 1, 0, 88 , 0  , 0  , 0  , 0  , 0  ), //      {fs|gs}
  ROW(1, 1, 1, 0, 89 , 0  , 0  , 0  , 0  , 0  ), // #147 {r16|m16|r64|m64|i8|i16|i32}
  ROW(1, 1, 0, 0, 90 , 0  , 0  , 0  , 0  , 0  ), //      {r32|m32|i32|u32}
  ROW(1, 1, 0, 0, 91 , 0  , 0  , 0  , 0  , 0  ), //      {cs|ss|ds|es}
  ROW(1, 1, 1, 0, 88 , 0  , 0  , 0  , 0  , 0  ), //      {fs|gs}
  ROW(2, 1, 1, 2, 81 , 92 , 0  , 0  , 0  , 0  ), // #151 {<al>, <es:[m8|memBase|zdi|mem]>}
  ROW(2, 1, 1, 2, 33 , 93 , 0  , 0  , 0  , 0  ), //      {<ax>, <es:[m16|memBase|zdi|mem]>}
  ROW(2, 1, 1, 2, 36 , 94 , 0  , 0  , 0  , 0  ), //      {<eax>, <es:[m32|memBase|zdi|mem]>}
  ROW(2, 0, 1, 2, 38 , 95 , 0  , 0  , 0  , 0  ), //      {<rax>, <es:[m64|memBase|zdi|mem]>}
  ROW(2, 1, 1, 2, 92 , 81 , 0  , 0  , 0  , 0  ), // #155 {<es:[m8|memBase|zdi|mem]>, <al>}
  ROW(2, 1, 1, 2, 93 , 33 , 0  , 0  , 0  , 0  ), //      {<es:[m16|memBase|zdi|mem]>, <ax>}
  ROW(2, 1, 1, 2, 94 , 36 , 0  , 0  , 0  , 0  ), //      {<es:[m32|memBase|zdi|mem]>, <eax>}
  ROW(2, 0, 1, 2, 95 , 38 , 0  , 0  , 0  , 0  ), //      {<es:[m64|memBase|zdi|mem]>, <rax>}
  ROW(4, 1, 1, 0, 50 , 50 , 50 , 51 , 0  , 0  ), // #159 {xmm, xmm, xmm, xmm|m128|mem}
  ROW(4, 1, 1, 0, 50 , 50 , 52 , 50 , 0  , 0  ), //      {xmm, xmm, m128|mem, xmm}
  ROW(4, 1, 1, 0, 53 , 53 , 53 , 54 , 0  , 0  ), //      {ymm, ymm, ymm, ymm|m256|mem}
  ROW(4, 1, 1, 0, 53 , 53 , 55 , 53 , 0  , 0  ), //      {ymm, ymm, m256|mem, ymm}
  ROW(3, 1, 1, 0, 50 , 67 , 50 , 0  , 0  , 0  ), // #163 {xmm, vm32x, xmm}
  ROW(3, 1, 1, 0, 53 , 67 , 53 , 0  , 0  , 0  ), //      {ymm, vm32x, ymm}
  ROW(2, 1, 1, 0, 96 , 67 , 0  , 0  , 0  , 0  ), //      {xmm|ymm, vm32x}
  ROW(2, 1, 1, 0, 56 , 68 , 0  , 0  , 0  , 0  ), //      {zmm, vm32y}
  ROW(3, 1, 1, 0, 52 , 50 , 50 , 0  , 0  , 0  ), // #167 {m128|mem, xmm, xmm}
  ROW(3, 1, 1, 0, 55 , 53 , 53 , 0  , 0  , 0  ), //      {m256|mem, ymm, ymm}
  ROW(3, 1, 1, 0, 50 , 50 , 52 , 0  , 0  , 0  ), //      {xmm, xmm, m128|mem}
  ROW(3, 1, 1, 0, 53 , 53 , 55 , 0  , 0  , 0  ), //      {ymm, ymm, m256|mem}
  ROW(5, 1, 1, 0, 50 , 50 , 51 , 50 , 97 , 0  ), // #171 {xmm, xmm, xmm|m128|mem, xmm, i4|u4}
  ROW(5, 1, 1, 0, 50 , 50 , 50 , 52 , 97 , 0  ), //      {xmm, xmm, xmm, m128|mem, i4|u4}
  ROW(5, 1, 1, 0, 53 , 53 , 54 , 53 , 97 , 0  ), //      {ymm, ymm, ymm|m256|mem, ymm, i4|u4}
  ROW(5, 1, 1, 0, 53 , 53 , 53 , 55 , 97 , 0  ), //      {ymm, ymm, ymm, m256|mem, i4|u4}
  ROW(3, 1, 1, 0, 53 , 54 , 10 , 0  , 0  , 0  ), // #175 {ymm, ymm|m256|mem, i8|u8}
  ROW(3, 1, 1, 0, 53 , 53 , 54 , 0  , 0  , 0  ), //      {ymm, ymm, ymm|m256|mem}
  ROW(3, 1, 1, 0, 56 , 56 , 61 , 0  , 0  , 0  ), //      {zmm, zmm, zmm|m512|mem|i8|u8}
  ROW(3, 1, 1, 0, 56 , 58 , 10 , 0  , 0  , 0  ), //      {zmm, m512|mem, i8|u8}
  ROW(2, 1, 1, 0, 4  , 27 , 0  , 0  , 0  , 0  ), // #179 {r16, r16|m16|mem}
  ROW(2, 1, 1, 0, 6  , 28 , 0  , 0  , 0  , 0  ), // #180 {r32, r32|m32|mem}
  ROW(2, 0, 1, 0, 8  , 15 , 0  , 0  , 0  , 0  ), //      {r64, r64|m64|mem}
  ROW(1, 1, 1, 0, 98 , 0  , 0  , 0  , 0  , 0  ), // #182 {m32|m64}
  ROW(2, 1, 1, 0, 99 , 100, 0  , 0  , 0  , 0  ), //      {st0, st}
  ROW(2, 1, 1, 0, 100, 99 , 0  , 0  , 0  , 0  ), //      {st, st0}
  ROW(2, 1, 1, 0, 4  , 29 , 0  , 0  , 0  , 0  ), // #185 {r16, m32|mem}
  ROW(2, 1, 1, 0, 6  , 101, 0  , 0  , 0  , 0  ), //      {r32, m48|mem}
  ROW(2, 0, 1, 0, 8  , 102, 0  , 0  , 0  , 0  ), //      {r64, m80|mem}
  ROW(3, 1, 1, 0, 27 , 4  , 103, 0  , 0  , 0  ), // #188 {r16|m16|mem, r16, cl|i8|u8}
  ROW(3, 1, 1, 0, 28 , 6  , 103, 0  , 0  , 0  ), //      {r32|m32|mem, r32, cl|i8|u8}
  ROW(3, 0, 1, 0, 15 , 8  , 103, 0  , 0  , 0  ), //      {r64|m64|mem, r64, cl|i8|u8}
  ROW(3, 1, 1, 0, 50 , 50 , 51 , 0  , 0  , 0  ), // #191 {xmm, xmm, xmm|m128|mem}
  ROW(3, 1, 1, 0, 53 , 53 , 54 , 0  , 0  , 0  ), // #192 {ymm, ymm, ymm|m256|mem}
  ROW(3, 1, 1, 0, 56 , 56 , 57 , 0  , 0  , 0  ), //      {zmm, zmm, zmm|m512|mem}
  ROW(4, 1, 1, 0, 50 , 50 , 51 , 10 , 0  , 0  ), // #194 {xmm, xmm, xmm|m128|mem, i8|u8}
  ROW(4, 1, 1, 0, 53 , 53 , 54 , 10 , 0  , 0  ), // #195 {ymm, ymm, ymm|m256|mem, i8|u8}
  ROW(4, 1, 1, 0, 56 , 56 , 57 , 10 , 0  , 0  ), //      {zmm, zmm, zmm|m512|mem, i8|u8}
  ROW(4, 1, 1, 0, 104, 50 , 51 , 10 , 0  , 0  ), // #197 {xmm|k, xmm, xmm|m128|mem, i8|u8}
  ROW(4, 1, 1, 0, 105, 53 , 54 , 10 , 0  , 0  ), //      {ymm|k, ymm, ymm|m256|mem, i8|u8}
  ROW(4, 1, 1, 0, 106, 56 , 57 , 10 , 0  , 0  ), //      {k, zmm, zmm|m512|mem, i8|u8}
  ROW(4, 1, 1, 0, 106, 50 , 51 , 10 , 0  , 0  ), // #200 {k, xmm, xmm|m128|mem, i8|u8}
  ROW(4, 1, 1, 0, 106, 53 , 54 , 10 , 0  , 0  ), //      {k, ymm, ymm|m256|mem, i8|u8}
  ROW(4, 1, 1, 0, 106, 56 , 57 , 10 , 0  , 0  ), //      {k, zmm, zmm|m512|mem, i8|u8}
  ROW(2, 1, 1, 0, 51 , 50 , 0  , 0  , 0  , 0  ), // #203 {xmm|m128|mem, xmm}
  ROW(2, 1, 1, 0, 54 , 53 , 0  , 0  , 0  , 0  ), //      {ymm|m256|mem, ymm}
  ROW(2, 1, 1, 0, 57 , 56 , 0  , 0  , 0  , 0  ), //      {zmm|m512|mem, zmm}
  ROW(2, 1, 1, 0, 50 , 65 , 0  , 0  , 0  , 0  ), // #206 {xmm, xmm|m64|mem}
  ROW(2, 1, 1, 0, 53 , 51 , 0  , 0  , 0  , 0  ), //      {ymm, xmm|m128|mem}
  ROW(2, 1, 1, 0, 56 , 54 , 0  , 0  , 0  , 0  ), //      {zmm, ymm|m256|mem}
  ROW(2, 1, 1, 0, 50 , 51 , 0  , 0  , 0  , 0  ), // #209 {xmm, xmm|m128|mem}
  ROW(2, 1, 1, 0, 53 , 54 , 0  , 0  , 0  , 0  ), //      {ymm, ymm|m256|mem}
  ROW(2, 1, 1, 0, 56 , 57 , 0  , 0  , 0  , 0  ), //      {zmm, zmm|m512|mem}
  ROW(2, 1, 1, 0, 50 , 107, 0  , 0  , 0  , 0  ), // #212 {xmm, xmm|m32|mem}
  ROW(2, 1, 1, 0, 53 , 65 , 0  , 0  , 0  , 0  ), //      {ymm, xmm|m64|mem}
  ROW(2, 1, 1, 0, 56 , 51 , 0  , 0  , 0  , 0  ), //      {zmm, xmm|m128|mem}
  ROW(3, 1, 1, 0, 65 , 50 , 10 , 0  , 0  , 0  ), // #215 {xmm|m64|mem, xmm, i8|u8}
  ROW(3, 1, 1, 0, 51 , 53 , 10 , 0  , 0  , 0  ), // #216 {xmm|m128|mem, ymm, i8|u8}
  ROW(3, 1, 1, 0, 54 , 56 , 10 , 0  , 0  , 0  ), // #217 {ymm|m256|mem, zmm, i8|u8}
  ROW(3, 1, 1, 0, 50 , 108, 50 , 0  , 0  , 0  ), // #218 {xmm, vm64x|vm64y, xmm}
  ROW(2, 1, 1, 0, 50 , 108, 0  , 0  , 0  , 0  ), //      {xmm, vm64x|vm64y}
  ROW(2, 1, 1, 0, 53 , 72 , 0  , 0  , 0  , 0  ), //      {ymm, vm64z}
  ROW(3, 1, 1, 0, 50 , 51 , 10 , 0  , 0  , 0  ), // #221 {xmm, xmm|m128|mem, i8|u8}
  ROW(3, 1, 1, 0, 53 , 54 , 10 , 0  , 0  , 0  ), //      {ymm, ymm|m256|mem, i8|u8}
  ROW(3, 1, 1, 0, 56 , 57 , 10 , 0  , 0  , 0  ), //      {zmm, zmm|m512|mem, i8|u8}
  ROW(2, 1, 1, 0, 50 , 65 , 0  , 0  , 0  , 0  ), // #224 {xmm, xmm|m64|mem}
  ROW(2, 1, 1, 0, 53 , 54 , 0  , 0  , 0  , 0  ), //      {ymm, ymm|m256|mem}
  ROW(2, 1, 1, 0, 56 , 57 , 0  , 0  , 0  , 0  ), //      {zmm, zmm|m512|mem}
  ROW(2, 1, 1, 0, 52 , 50 , 0  , 0  , 0  , 0  ), // #227 {m128|mem, xmm}
  ROW(2, 1, 1, 0, 55 , 53 , 0  , 0  , 0  , 0  ), //      {m256|mem, ymm}
  ROW(2, 1, 1, 0, 58 , 56 , 0  , 0  , 0  , 0  ), //      {m512|mem, zmm}
  ROW(2, 1, 1, 0, 50 , 52 , 0  , 0  , 0  , 0  ), // #230 {xmm, m128|mem}
  ROW(2, 1, 1, 0, 53 , 55 , 0  , 0  , 0  , 0  ), //      {ymm, m256|mem}
  ROW(2, 1, 1, 0, 56 , 58 , 0  , 0  , 0  , 0  ), //      {zmm, m512|mem}
  ROW(2, 0, 1, 0, 15 , 50 , 0  , 0  , 0  , 0  ), // #233 {r64|m64|mem, xmm}
  ROW(2, 1, 1, 0, 50 , 109, 0  , 0  , 0  , 0  ), //      {xmm, xmm|m64|mem|r64}
  ROW(2, 1, 1, 0, 30 , 50 , 0  , 0  , 0  , 0  ), //      {m64|mem, xmm}
  ROW(2, 1, 1, 0, 30 , 50 , 0  , 0  , 0  , 0  ), // #236 {m64|mem, xmm}
  ROW(2, 1, 1, 0, 50 , 30 , 0  , 0  , 0  , 0  ), //      {xmm, m64|mem}
  ROW(3, 1, 1, 0, 50 , 50 , 50 , 0  , 0  , 0  ), // #238 {xmm, xmm, xmm}
  ROW(2, 1, 1, 0, 21 , 50 , 0  , 0  , 0  , 0  ), // #239 {m16|mem, xmm}
  ROW(2, 1, 1, 0, 50 , 21 , 0  , 0  , 0  , 0  ), //      {xmm, m16|mem}
  ROW(3, 1, 1, 0, 50 , 50 , 50 , 0  , 0  , 0  ), //      {xmm, xmm, xmm}
  ROW(2, 1, 1, 0, 29 , 50 , 0  , 0  , 0  , 0  ), // #242 {m32|mem, xmm}
  ROW(2, 1, 1, 0, 50 , 29 , 0  , 0  , 0  , 0  ), //      {xmm, m32|mem}
  ROW(3, 1, 1, 0, 50 , 50 , 50 , 0  , 0  , 0  ), //      {xmm, xmm, xmm}
  ROW(4, 1, 1, 0, 106, 106, 50 , 51 , 0  , 0  ), // #245 {k, k, xmm, xmm|m128|mem}
  ROW(4, 1, 1, 0, 106, 106, 53 , 54 , 0  , 0  ), //      {k, k, ymm, ymm|m256|mem}
  ROW(4, 1, 1, 0, 106, 106, 56 , 57 , 0  , 0  ), //      {k, k, zmm, zmm|m512|mem}
  ROW(2, 1, 1, 0, 96 , 109, 0  , 0  , 0  , 0  ), // #248 {xmm|ymm, xmm|m64|mem|r64}
  ROW(2, 0, 1, 0, 56 , 8  , 0  , 0  , 0  , 0  ), //      {zmm, r64}
  ROW(2, 1, 1, 0, 56 , 65 , 0  , 0  , 0  , 0  ), //      {zmm, xmm|m64|mem}
  ROW(3, 1, 1, 0, 104, 50 , 51 , 0  , 0  , 0  ), // #251 {xmm|k, xmm, xmm|m128|mem}
  ROW(3, 1, 1, 0, 105, 53 , 54 , 0  , 0  , 0  ), //      {ymm|k, ymm, ymm|m256|mem}
  ROW(3, 1, 1, 0, 106, 56 , 57 , 0  , 0  , 0  ), //      {k, zmm, zmm|m512|mem}
  ROW(2, 1, 1, 0, 107, 50 , 0  , 0  , 0  , 0  ), // #254 {xmm|m32|mem, xmm}
  ROW(2, 1, 1, 0, 65 , 53 , 0  , 0  , 0  , 0  ), //      {xmm|m64|mem, ymm}
  ROW(2, 1, 1, 0, 51 , 56 , 0  , 0  , 0  , 0  ), //      {xmm|m128|mem, zmm}
  ROW(2, 1, 1, 0, 65 , 50 , 0  , 0  , 0  , 0  ), // #257 {xmm|m64|mem, xmm}
  ROW(2, 1, 1, 0, 51 , 53 , 0  , 0  , 0  , 0  ), //      {xmm|m128|mem, ymm}
  ROW(2, 1, 1, 0, 54 , 56 , 0  , 0  , 0  , 0  ), //      {ymm|m256|mem, zmm}
  ROW(2, 1, 1, 0, 110, 50 , 0  , 0  , 0  , 0  ), // #260 {xmm|m16|mem, xmm}
  ROW(2, 1, 1, 0, 107, 53 , 0  , 0  , 0  , 0  ), //      {xmm|m32|mem, ymm}
  ROW(2, 1, 1, 0, 65 , 56 , 0  , 0  , 0  , 0  ), //      {xmm|m64|mem, zmm}
  ROW(2, 1, 1, 0, 50 , 110, 0  , 0  , 0  , 0  ), // #263 {xmm, xmm|m16|mem}
  ROW(2, 1, 1, 0, 53 , 107, 0  , 0  , 0  , 0  ), //      {ymm, xmm|m32|mem}
  ROW(2, 1, 1, 0, 56 , 65 , 0  , 0  , 0  , 0  ), //      {zmm, xmm|m64|mem}
  ROW(2, 1, 1, 0, 67 , 50 , 0  , 0  , 0  , 0  ), // #266 {vm32x, xmm}
  ROW(2, 1, 1, 0, 68 , 53 , 0  , 0  , 0  , 0  ), //      {vm32y, ymm}
  ROW(2, 1, 1, 0, 69 , 56 , 0  , 0  , 0  , 0  ), //      {vm32z, zmm}
  ROW(2, 1, 1, 0, 70 , 50 , 0  , 0  , 0  , 0  ), // #269 {vm64x, xmm}
  ROW(2, 1, 1, 0, 71 , 53 , 0  , 0  , 0  , 0  ), //      {vm64y, ymm}
  ROW(2, 1, 1, 0, 72 , 56 , 0  , 0  , 0  , 0  ), //      {vm64z, zmm}
  ROW(3, 1, 1, 0, 106, 50 , 51 , 0  , 0  , 0  ), // #272 {k, xmm, xmm|m128|mem}
  ROW(3, 1, 1, 0, 106, 53 , 54 , 0  , 0  , 0  ), //      {k, ymm, ymm|m256|mem}
  ROW(3, 1, 1, 0, 106, 56 , 57 , 0  , 0  , 0  ), //      {k, zmm, zmm|m512|mem}
  ROW(3, 1, 1, 0, 6  , 6  , 28 , 0  , 0  , 0  ), // #275 {r32, r32, r32|m32|mem}
  ROW(3, 0, 1, 0, 8  , 8  , 15 , 0  , 0  , 0  ), //      {r64, r64, r64|m64|mem}
  ROW(3, 1, 1, 0, 6  , 28 , 6  , 0  , 0  , 0  ), // #277 {r32, r32|m32|mem, r32}
  ROW(3, 0, 1, 0, 8  , 15 , 8  , 0  , 0  , 0  ), //      {r64, r64|m64|mem, r64}
  ROW(2, 1, 0, 0, 111, 28 , 0  , 0  , 0  , 0  ), // #279 {bnd, r32|m32|mem}
  ROW(2, 0, 1, 0, 111, 15 , 0  , 0  , 0  , 0  ), //      {bnd, r64|m64|mem}
  ROW(2, 1, 1, 0, 111, 112, 0  , 0  , 0  , 0  ), // #281 {bnd, bnd|mem}
  ROW(2, 1, 1, 0, 113, 111, 0  , 0  , 0  , 0  ), //      {mem, bnd}
  ROW(2, 1, 0, 0, 4  , 29 , 0  , 0  , 0  , 0  ), // #283 {r16, m32|mem}
  ROW(2, 1, 0, 0, 6  , 30 , 0  , 0  , 0  , 0  ), //      {r32, m64|mem}
  ROW(1, 1, 0, 0, 114, 0  , 0  , 0  , 0  , 0  ), // #285 {rel16|r16|m16|r32|m32}
  ROW(1, 1, 1, 0, 115, 0  , 0  , 0  , 0  , 0  ), //      {rel32|r64|m64|mem}
  ROW(2, 1, 1, 0, 6  , 116, 0  , 0  , 0  , 0  ), // #287 {r32, r8lo|r8hi|m8|r16|m16|r32|m32}
  ROW(2, 0, 1, 0, 8  , 117, 0  , 0  , 0  , 0  ), //      {r64, r8lo|r8hi|m8|r64|m64}
  ROW(1, 1, 0, 0, 118, 0  , 0  , 0  , 0  , 0  ), // #289 {r16|r32}
  ROW(1, 1, 1, 0, 31 , 0  , 0  , 0  , 0  , 0  ), // #290 {r8lo|r8hi|m8|r16|m16|r32|m32|r64|m64|mem}
  ROW(2, 1, 0, 0, 119, 58 , 0  , 0  , 0  , 0  ), // #291 {es:[mem|m512|memBase], m512|mem}
  ROW(2, 0, 1, 0, 119, 58 , 0  , 0  , 0  , 0  ), //      {es:[mem|m512|memBase], m512|mem}
  ROW(3, 1, 1, 0, 50 , 10 , 10 , 0  , 0  , 0  ), // #293 {xmm, i8|u8, i8|u8}
  ROW(2, 1, 1, 0, 50 , 50 , 0  , 0  , 0  , 0  ), // #294 {xmm, xmm}
  ROW(0, 1, 1, 0, 0  , 0  , 0  , 0  , 0  , 0  ), // #295 {}
  ROW(1, 1, 1, 0, 100, 0  , 0  , 0  , 0  , 0  ), // #296 {st}
  ROW(0, 1, 1, 0, 0  , 0  , 0  , 0  , 0  , 0  ), // #297 {}
  ROW(1, 1, 1, 0, 120, 0  , 0  , 0  , 0  , 0  ), // #298 {m32|m64|st}
  ROW(2, 1, 1, 0, 50 , 50 , 0  , 0  , 0  , 0  ), // #299 {xmm, xmm}
  ROW(4, 1, 1, 0, 50 , 50 , 10 , 10 , 0  , 0  ), //      {xmm, xmm, i8|u8, i8|u8}
  ROW(2, 1, 0, 0, 6  , 52 , 0  , 0  , 0  , 0  ), // #301 {r32, m128|mem}
  ROW(2, 0, 1, 0, 8  , 52 , 0  , 0  , 0  , 0  ), //      {r64, m128|mem}
  ROW(2, 1, 0, 2, 36 , 121, 0  , 0  , 0  , 0  ), // #303 {<eax>, <ecx>}
  ROW(2, 0, 1, 2, 122, 121, 0  , 0  , 0  , 0  ), //      {<eax|rax>, <ecx>}
  ROW(1, 1, 1, 0, 123, 0  , 0  , 0  , 0  , 0  ), // #305 {rel8|rel32}
  ROW(1, 1, 0, 0, 124, 0  , 0  , 0  , 0  , 0  ), //      {rel16}
  ROW(2, 1, 0, 1, 125, 126, 0  , 0  , 0  , 0  ), // #307 {<cx|ecx>, rel8}
  ROW(2, 0, 1, 1, 127, 126, 0  , 0  , 0  , 0  ), //      {<ecx|rcx>, rel8}
  ROW(1, 1, 1, 0, 128, 0  , 0  , 0  , 0  , 0  ), // #309 {rel8|rel32|r64|m64|mem}
  ROW(1, 1, 0, 0, 129, 0  , 0  , 0  , 0  , 0  ), //      {rel16|r32|m32|mem}
  ROW(2, 1, 1, 0, 106, 130, 0  , 0  , 0  , 0  ), // #311 {k, k|m8|mem|r32}
  ROW(2, 1, 1, 0, 131, 106, 0  , 0  , 0  , 0  ), //      {m8|mem|r32, k}
  ROW(2, 1, 1, 0, 106, 132, 0  , 0  , 0  , 0  ), // #313 {k, k|m32|mem|r32}
  ROW(2, 1, 1, 0, 28 , 106, 0  , 0  , 0  , 0  ), //      {m32|mem|r32, k}
  ROW(2, 1, 1, 0, 106, 133, 0  , 0  , 0  , 0  ), // #315 {k, k|m64|mem|r64}
  ROW(2, 1, 1, 0, 15 , 106, 0  , 0  , 0  , 0  ), //      {m64|mem|r64, k}
  ROW(2, 1, 1, 0, 106, 134, 0  , 0  , 0  , 0  ), // #317 {k, k|m16|mem|r32}
  ROW(2, 1, 1, 0, 135, 106, 0  , 0  , 0  , 0  ), //      {m16|mem|r32, k}
  ROW(2, 1, 1, 0, 4  , 27 , 0  , 0  , 0  , 0  ), // #319 {r16, r16|m16|mem}
  ROW(2, 1, 1, 0, 6  , 135, 0  , 0  , 0  , 0  ), //      {r32, r32|m16|mem}
  ROW(2, 1, 0, 0, 136, 137, 0  , 0  , 0  , 0  ), // #321 {i16, i16|i32}
  ROW(1, 1, 1, 0, 138, 0  , 0  , 0  , 0  , 0  ), //      {m32|m48|m80|mem}
  ROW(2, 1, 0, 0, 4  , 29 , 0  , 0  , 0  , 0  ), // #323 {r16, m32|mem}
  ROW(2, 1, 0, 0, 6  , 101, 0  , 0  , 0  , 0  ), //      {r32, m48|mem}
  ROW(2, 1, 1, 0, 4  , 27 , 0  , 0  , 0  , 0  ), // #325 {r16, r16|m16|mem}
  ROW(2, 1, 1, 0, 139, 135, 0  , 0  , 0  , 0  ), //      {r32|r64, r32|m16|mem}
  ROW(2, 1, 1, 0, 64 , 28 , 0  , 0  , 0  , 0  ), // #327 {mm|xmm, r32|m32|mem}
  ROW(2, 1, 1, 0, 28 , 64 , 0  , 0  , 0  , 0  ), //      {r32|m32|mem, mm|xmm}
  ROW(2, 1, 1, 0, 50 , 107, 0  , 0  , 0  , 0  ), // #329 {xmm, xmm|m32|mem}
  ROW(2, 1, 1, 0, 29 , 50 , 0  , 0  , 0  , 0  ), //      {m32|mem, xmm}
  ROW(2, 1, 1, 0, 4  , 9  , 0  , 0  , 0  , 0  ), // #331 {r16, r8lo|r8hi|m8}
  ROW(2, 1, 1, 0, 139, 140, 0  , 0  , 0  , 0  ), //      {r32|r64, r8lo|r8hi|m8|r16|m16}
  ROW(2, 0, 1, 0, 4  , 27 , 0  , 0  , 0  , 0  ), // #333 {r16, r16|m16|mem}
  ROW(2, 0, 1, 0, 139, 28 , 0  , 0  , 0  , 0  ), //      {r32|r64, r32|m32|mem}
  ROW(4, 1, 1, 1, 6  , 6  , 28 , 35 , 0  , 0  ), // #335 {r32, r32, r32|m32|mem, <edx>}
  ROW(4, 0, 1, 1, 8  , 8  , 15 , 37 , 0  , 0  ), //      {r64, r64, r64|m64|mem, <rdx>}
  ROW(2, 1, 1, 0, 62 , 141, 0  , 0  , 0  , 0  ), // #337 {mm, mm|m64|mem}
  ROW(2, 1, 1, 0, 50 , 51 , 0  , 0  , 0  , 0  ), //      {xmm, xmm|m128|mem}
  ROW(3, 1, 1, 0, 62 , 141, 10 , 0  , 0  , 0  ), // #339 {mm, mm|m64|mem, i8|u8}
  ROW(3, 1, 1, 0, 50 , 51 , 10 , 0  , 0  , 0  ), //      {xmm, xmm|m128|mem, i8|u8}
  ROW(3, 1, 1, 0, 6  , 64 , 10 , 0  , 0  , 0  ), // #341 {r32, mm|xmm, i8|u8}
  ROW(3, 1, 1, 0, 21 , 50 , 10 , 0  , 0  , 0  ), //      {m16|mem, xmm, i8|u8}
  ROW(2, 1, 1, 0, 62 , 142, 0  , 0  , 0  , 0  ), // #343 {mm, i8|u8|mm|m64|mem}
  ROW(2, 1, 1, 0, 50 , 59 , 0  , 0  , 0  , 0  ), //      {xmm, i8|u8|xmm|m128|mem}
  ROW(2, 1, 1, 0, 62 , 143, 0  , 0  , 0  , 0  ), // #345 {mm, mm|m32|mem}
  ROW(2, 1, 1, 0, 50 , 51 , 0  , 0  , 0  , 0  ), //      {xmm, xmm|m128|mem}
  ROW(1, 1, 0, 0, 6  , 0  , 0  , 0  , 0  , 0  ), // #347 {r32}
  ROW(1, 0, 1, 0, 8  , 0  , 0  , 0  , 0  , 0  ), // #348 {r64}
  ROW(0, 1, 1, 0, 0  , 0  , 0  , 0  , 0  , 0  ), // #349 {}
  ROW(1, 1, 1, 0, 144, 0  , 0  , 0  , 0  , 0  ), //      {u16}
  ROW(3, 1, 1, 0, 6  , 28 , 10 , 0  , 0  , 0  ), // #351 {r32, r32|m32|mem, i8|u8}
  ROW(3, 0, 1, 0, 8  , 15 , 10 , 0  , 0  , 0  ), //      {r64, r64|m64|mem, i8|u8}
  ROW(4, 1, 1, 0, 50 , 50 , 51 , 50 , 0  , 0  ), // #353 {xmm, xmm, xmm|m128|mem, xmm}
  ROW(4, 1, 1, 0, 53 , 53 , 54 , 53 , 0  , 0  ), //      {ymm, ymm, ymm|m256|mem, ymm}
  ROW(2, 1, 1, 0, 50 , 145, 0  , 0  , 0  , 0  ), // #355 {xmm, xmm|m128|ymm|m256}
  ROW(2, 1, 1, 0, 53 , 57 , 0  , 0  , 0  , 0  ), //      {ymm, zmm|m512|mem}
  ROW(4, 1, 1, 0, 50 , 50 , 50 , 65 , 0  , 0  ), // #357 {xmm, xmm, xmm, xmm|m64|mem}
  ROW(4, 1, 1, 0, 50 , 50 , 30 , 50 , 0  , 0  ), //      {xmm, xmm, m64|mem, xmm}
  ROW(4, 1, 1, 0, 50 , 50 , 50 , 107, 0  , 0  ), // #359 {xmm, xmm, xmm, xmm|m32|mem}
  ROW(4, 1, 1, 0, 50 , 50 , 29 , 50 , 0  , 0  ), //      {xmm, xmm, m32|mem, xmm}
  ROW(4, 1, 1, 0, 53 , 53 , 51 , 10 , 0  , 0  ), // #361 {ymm, ymm, xmm|m128|mem, i8|u8}
  ROW(4, 1, 1, 0, 56 , 56 , 51 , 10 , 0  , 0  ), //      {zmm, zmm, xmm|m128|mem, i8|u8}
  ROW(1, 1, 0, 1, 36 , 0  , 0  , 0  , 0  , 0  ), // #363 {<eax>}
  ROW(1, 0, 1, 1, 38 , 0  , 0  , 0  , 0  , 0  ), // #364 {<rax>}
  ROW(2, 1, 1, 0, 28 , 50 , 0  , 0  , 0  , 0  ), // #365 {r32|m32|mem, xmm}
  ROW(2, 1, 1, 0, 50 , 28 , 0  , 0  , 0  , 0  ), //      {xmm, r32|m32|mem}
  ROW(2, 1, 1, 0, 30 , 50 , 0  , 0  , 0  , 0  ), // #367 {m64|mem, xmm}
  ROW(3, 1, 1, 0, 50 , 50 , 30 , 0  , 0  , 0  ), //      {xmm, xmm, m64|mem}
  ROW(2, 1, 1, 0, 135, 50 , 0  , 0  , 0  , 0  ), // #369 {r32|m16|mem, xmm}
  ROW(2, 1, 1, 0, 50 , 135, 0  , 0  , 0  , 0  ), //      {xmm, r32|m16|mem}
  ROW(2, 1, 0, 0, 28 , 6  , 0  , 0  , 0  , 0  ), // #371 {r32|m32|mem, r32}
  ROW(2, 0, 1, 0, 15 , 8  , 0  , 0  , 0  , 0  ), //      {r64|m64|mem, r64}
  ROW(2, 1, 0, 0, 6  , 28 , 0  , 0  , 0  , 0  ), // #373 {r32, r32|m32|mem}
  ROW(2, 0, 1, 0, 8  , 15 , 0  , 0  , 0  , 0  ), //      {r64, r64|m64|mem}
  ROW(3, 1, 1, 0, 50 , 50 , 59 , 0  , 0  , 0  ), // #375 {xmm, xmm, xmm|m128|mem|i8|u8}
  ROW(3, 1, 1, 0, 50 , 52 , 146, 0  , 0  , 0  ), //      {xmm, m128|mem, i8|u8|xmm}
  ROW(2, 1, 1, 0, 67 , 96 , 0  , 0  , 0  , 0  ), // #377 {vm32x, xmm|ymm}
  ROW(2, 1, 1, 0, 68 , 56 , 0  , 0  , 0  , 0  ), //      {vm32y, zmm}
  ROW(2, 1, 1, 0, 108, 50 , 0  , 0  , 0  , 0  ), // #379 {vm64x|vm64y, xmm}
  ROW(2, 1, 1, 0, 72 , 53 , 0  , 0  , 0  , 0  ), //      {vm64z, ymm}
  ROW(3, 1, 1, 0, 50 , 50 , 51 , 0  , 0  , 0  ), // #381 {xmm, xmm, xmm|m128|mem}
  ROW(3, 1, 1, 0, 50 , 52 , 50 , 0  , 0  , 0  ), //      {xmm, m128|mem, xmm}
  ROW(1, 1, 0, 1, 33 , 0  , 0  , 0  , 0  , 0  ), // #383 {<ax>}
  ROW(2, 1, 0, 1, 33 , 10 , 0  , 0  , 0  , 0  ), // #384 {<ax>, i8|u8}
  ROW(2, 1, 0, 0, 27 , 4  , 0  , 0  , 0  , 0  ), // #385 {r16|m16|mem, r16}
  ROW(3, 1, 1, 1, 50 , 51 , 147, 0  , 0  , 0  ), // #386 {xmm, xmm|m128|mem, <xmm0>}
  ROW(2, 1, 1, 0, 111, 148, 0  , 0  , 0  , 0  ), // #387 {bnd, mib}
  ROW(2, 1, 1, 0, 111, 113, 0  , 0  , 0  , 0  ), // #388 {bnd, mem}
  ROW(2, 1, 1, 0, 148, 111, 0  , 0  , 0  , 0  ), // #389 {mib, bnd}
  ROW(1, 1, 1, 0, 149, 0  , 0  , 0  , 0  , 0  ), // #390 {r16|r32|r64}
  ROW(1, 1, 1, 1, 33 , 0  , 0  , 0  , 0  , 0  ), // #391 {<ax>}
  ROW(2, 1, 1, 2, 35 , 36 , 0  , 0  , 0  , 0  ), // #392 {<edx>, <eax>}
  ROW(1, 1, 1, 0, 150, 0  , 0  , 0  , 0  , 0  ), // #393 {mem|m8|m16|m32|m48|m64|m80|m128|m256|m512|m1024}
  ROW(1, 1, 1, 0, 30 , 0  , 0  , 0  , 0  , 0  ), // #394 {m64|mem}
  ROW(0, 0, 1, 0, 0  , 0  , 0  , 0  , 0  , 0  ), // #395 {}
  ROW(1, 1, 1, 1, 151, 0  , 0  , 0  , 0  , 0  ), // #396 {<ds:[mem|m512|memBase|zax]>}
  ROW(3, 1, 1, 0, 50 , 65 , 10 , 0  , 0  , 0  ), // #397 {xmm, xmm|m64|mem, i8|u8}
  ROW(3, 1, 1, 0, 50 , 107, 10 , 0  , 0  , 0  ), // #398 {xmm, xmm|m32|mem, i8|u8}
  ROW(5, 0, 1, 4, 52 , 37 , 38 , 152, 153, 0  ), // #399 {m128|mem, <rdx>, <rax>, <rcx>, <rbx>}
  ROW(5, 1, 1, 4, 30 , 35 , 36 , 121, 154, 0  ), // #400 {m64|mem, <edx>, <eax>, <ecx>, <ebx>}
  ROW(4, 1, 1, 4, 36 , 154, 121, 35 , 0  , 0  ), // #401 {<eax>, <ebx>, <ecx>, <edx>}
  ROW(2, 0, 1, 2, 37 , 38 , 0  , 0  , 0  , 0  ), // #402 {<rdx>, <rax>}
  ROW(2, 1, 1, 0, 62 , 51 , 0  , 0  , 0  , 0  ), // #403 {mm, xmm|m128|mem}
  ROW(2, 1, 1, 0, 50 , 141, 0  , 0  , 0  , 0  ), // #404 {xmm, mm|m64|mem}
  ROW(2, 1, 1, 0, 62 , 65 , 0  , 0  , 0  , 0  ), // #405 {mm, xmm|m64|mem}
  ROW(2, 1, 1, 0, 139, 65 , 0  , 0  , 0  , 0  ), // #406 {r32|r64, xmm|m64|mem}
  ROW(2, 1, 1, 0, 50 , 155, 0  , 0  , 0  , 0  ), // #407 {xmm, r32|m32|mem|r64|m64}
  ROW(2, 1, 1, 0, 139, 107, 0  , 0  , 0  , 0  ), // #408 {r32|r64, xmm|m32|mem}
  ROW(2, 1, 1, 2, 34 , 33 , 0  , 0  , 0  , 0  ), // #409 {<dx>, <ax>}
  ROW(1, 1, 1, 1, 36 , 0  , 0  , 0  , 0  , 0  ), // #410 {<eax>}
  ROW(2, 1, 1, 0, 12 , 10 , 0  , 0  , 0  , 0  ), // #411 {i16|u16, i8|u8}
  ROW(3, 1, 1, 0, 28 , 50 , 10 , 0  , 0  , 0  ), // #412 {r32|m32|mem, xmm, i8|u8}
  ROW(1, 1, 1, 0, 102, 0  , 0  , 0  , 0  , 0  ), // #413 {m80|mem}
  ROW(1, 1, 1, 0, 156, 0  , 0  , 0  , 0  , 0  ), // #414 {m16|m32}
  ROW(1, 1, 1, 0, 157, 0  , 0  , 0  , 0  , 0  ), // #415 {m16|m32|m64}
  ROW(1, 1, 1, 0, 158, 0  , 0  , 0  , 0  , 0  ), // #416 {m32|m64|m80|st}
  ROW(1, 1, 1, 0, 21 , 0  , 0  , 0  , 0  , 0  ), // #417 {m16|mem}
  ROW(1, 1, 1, 0, 113, 0  , 0  , 0  , 0  , 0  ), // #418 {mem}
  ROW(1, 1, 1, 0, 159, 0  , 0  , 0  , 0  , 0  ), // #419 {ax|m16|mem}
  ROW(1, 0, 1, 0, 113, 0  , 0  , 0  , 0  , 0  ), // #420 {mem}
  ROW(2, 1, 1, 1, 10 , 36 , 0  , 0  , 0  , 0  ), // #421 {i8|u8, <eax>}
  ROW(2, 1, 1, 0, 160, 161, 0  , 0  , 0  , 0  ), // #422 {al|ax|eax, i8|u8|dx}
  ROW(1, 1, 1, 0, 6  , 0  , 0  , 0  , 0  , 0  ), // #423 {r32}
  ROW(2, 1, 1, 0, 162, 163, 0  , 0  , 0  , 0  ), // #424 {es:[m8|memBase|zdi|m16|m32], dx}
  ROW(1, 1, 1, 0, 10 , 0  , 0  , 0  , 0  , 0  ), // #425 {i8|u8}
  ROW(0, 1, 0, 0, 0  , 0  , 0  , 0  , 0  , 0  ), // #426 {}
  ROW(3, 1, 1, 0, 106, 106, 106, 0  , 0  , 0  ), // #427 {k, k, k}
  ROW(2, 1, 1, 0, 106, 106, 0  , 0  , 0  , 0  ), // #428 {k, k}
  ROW(3, 1, 1, 0, 106, 106, 10 , 0  , 0  , 0  ), // #429 {k, k, i8|u8}
  ROW(1, 1, 1, 1, 164, 0  , 0  , 0  , 0  , 0  ), // #430 {<ah>}
  ROW(1, 1, 1, 0, 29 , 0  , 0  , 0  , 0  , 0  ), // #431 {m32|mem}
  ROW(1, 0, 1, 0, 58 , 0  , 0  , 0  , 0  , 0  ), // #432 {m512|mem}
  ROW(2, 1, 1, 0, 149, 150, 0  , 0  , 0  , 0  ), // #433 {r16|r32|r64, mem|m8|m16|m32|m48|m64|m80|m128|m256|m512|m1024}
  ROW(1, 1, 1, 0, 27 , 0  , 0  , 0  , 0  , 0  ), // #434 {r16|m16|mem}
  ROW(1, 1, 1, 0, 139, 0  , 0  , 0  , 0  , 0  ), // #435 {r32|r64}
  ROW(3, 1, 1, 0, 139, 28 , 14 , 0  , 0  , 0  ), // #436 {r32|r64, r32|m32|mem, i32|u32}
  ROW(3, 1, 1, 1, 50 , 50 , 165, 0  , 0  , 0  ), // #437 {xmm, xmm, <ds:[mem|m128|memBase|zdi]>}
  ROW(3, 1, 1, 1, 62 , 62 , 166, 0  , 0  , 0  ), // #438 {mm, mm, <ds:[mem|m64|memBase|zdi]>}
  ROW(3, 1, 1, 3, 167, 121, 35 , 0  , 0  , 0  ), // #439 {<ds:[mem|memBase|zax]>, <ecx>, <edx>}
  ROW(2, 1, 1, 0, 119, 58 , 0  , 0  , 0  , 0  ), // #440 {es:[mem|m512|memBase], m512|mem}
  ROW(2, 1, 1, 0, 62 , 50 , 0  , 0  , 0  , 0  ), // #441 {mm, xmm}
  ROW(2, 1, 1, 0, 6  , 50 , 0  , 0  , 0  , 0  ), // #442 {r32, xmm}
  ROW(2, 1, 1, 0, 30 , 62 , 0  , 0  , 0  , 0  ), // #443 {m64|mem, mm}
  ROW(2, 1, 1, 0, 50 , 62 , 0  , 0  , 0  , 0  ), // #444 {xmm, mm}
  ROW(2, 1, 1, 2, 36 , 121, 0  , 0  , 0  , 0  ), // #445 {<eax>, <ecx>}
  ROW(3, 1, 1, 3, 36 , 121, 154, 0  , 0  , 0  ), // #446 {<eax>, <ecx>, <ebx>}
  ROW(2, 1, 1, 0, 168, 160, 0  , 0  , 0  , 0  ), // #447 {u8|dx, al|ax|eax}
  ROW(2, 1, 1, 0, 163, 169, 0  , 0  , 0  , 0  ), // #448 {dx, ds:[m8|memBase|zsi|m16|m32]}
  ROW(6, 1, 1, 3, 50 , 51 , 10 , 121, 36 , 35 ), // #449 {xmm, xmm|m128|mem, i8|u8, <ecx>, <eax>, <edx>}
  ROW(6, 1, 1, 3, 50 , 51 , 10 , 147, 36 , 35 ), // #450 {xmm, xmm|m128|mem, i8|u8, <xmm0>, <eax>, <edx>}
  ROW(4, 1, 1, 1, 50 , 51 , 10 , 121, 0  , 0  ), // #451 {xmm, xmm|m128|mem, i8|u8, <ecx>}
  ROW(4, 1, 1, 1, 50 , 51 , 10 , 147, 0  , 0  ), // #452 {xmm, xmm|m128|mem, i8|u8, <xmm0>}
  ROW(3, 1, 1, 0, 131, 50 , 10 , 0  , 0  , 0  ), // #453 {r32|m8|mem, xmm, i8|u8}
  ROW(3, 0, 1, 0, 15 , 50 , 10 , 0  , 0  , 0  ), // #454 {r64|m64|mem, xmm, i8|u8}
  ROW(3, 1, 1, 0, 50 , 131, 10 , 0  , 0  , 0  ), // #455 {xmm, r32|m8|mem, i8|u8}
  ROW(3, 1, 1, 0, 50 , 28 , 10 , 0  , 0  , 0  ), // #456 {xmm, r32|m32|mem, i8|u8}
  ROW(3, 0, 1, 0, 50 , 15 , 10 , 0  , 0  , 0  ), // #457 {xmm, r64|m64|mem, i8|u8}
  ROW(3, 1, 1, 0, 64 , 135, 10 , 0  , 0  , 0  ), // #458 {mm|xmm, r32|m16|mem, i8|u8}
  ROW(2, 1, 1, 0, 6  , 64 , 0  , 0  , 0  , 0  ), // #459 {r32, mm|xmm}
  ROW(2, 1, 1, 0, 50 , 10 , 0  , 0  , 0  , 0  ), // #460 {xmm, i8|u8}
  ROW(1, 1, 1, 0, 155, 0  , 0  , 0  , 0  , 0  ), // #461 {r32|m32|mem|r64|m64}
  ROW(2, 1, 1, 0, 31 , 103, 0  , 0  , 0  , 0  ), // #462 {r8lo|r8hi|m8|r16|m16|r32|m32|r64|m64|mem, cl|i8|u8}
  ROW(1, 0, 1, 0, 139, 0  , 0  , 0  , 0  , 0  ), // #463 {r32|r64}
  ROW(3, 1, 1, 3, 35 , 36 , 121, 0  , 0  , 0  ), // #464 {<edx>, <eax>, <ecx>}
  ROW(1, 1, 1, 0, 1  , 0  , 0  , 0  , 0  , 0  ), // #465 {r8lo|r8hi|m8|mem}
  ROW(1, 1, 1, 0, 170, 0  , 0  , 0  , 0  , 0  ), // #466 {r16|m16|mem|r32|r64}
  ROW(3, 0, 1, 0, 171, 171, 171, 0  , 0  , 0  ), // #467 {tmm, tmm, tmm}
  ROW(2, 0, 1, 0, 171, 172, 0  , 0  , 0  , 0  ), // #468 {tmm, tmem}
  ROW(2, 0, 1, 0, 172, 171, 0  , 0  , 0  , 0  ), // #469 {tmem, tmm}
  ROW(1, 0, 1, 0, 171, 0  , 0  , 0  , 0  , 0  ), // #470 {tmm}
  ROW(3, 1, 1, 2, 6  , 35 , 36 , 0  , 0  , 0  ), // #471 {r32, <edx>, <eax>}
  ROW(1, 1, 1, 0, 173, 0  , 0  , 0  , 0  , 0  ), // #472 {ds:[mem|memBase]}
  ROW(6, 1, 1, 0, 56 , 56 , 56 , 56 , 56 , 52 ), // #473 {zmm, zmm, zmm, zmm, zmm, m128|mem}
  ROW(6, 1, 1, 0, 50 , 50 , 50 , 50 , 50 , 52 ), // #474 {xmm, xmm, xmm, xmm, xmm, m128|mem}
  ROW(3, 1, 1, 0, 50 , 50 , 65 , 0  , 0  , 0  ), // #475 {xmm, xmm, xmm|m64|mem}
  ROW(3, 1, 1, 0, 50 , 50 , 110, 0  , 0  , 0  ), // #476 {xmm, xmm, xmm|m16|mem}
  ROW(3, 1, 1, 0, 50 , 50 , 107, 0  , 0  , 0  ), // #477 {xmm, xmm, xmm|m32|mem}
  ROW(2, 1, 1, 0, 53 , 52 , 0  , 0  , 0  , 0  ), // #478 {ymm, m128|mem}
  ROW(2, 1, 1, 0, 174, 65 , 0  , 0  , 0  , 0  ), // #479 {ymm|zmm, xmm|m64|mem}
  ROW(2, 1, 1, 0, 174, 52 , 0  , 0  , 0  , 0  ), // #480 {ymm|zmm, m128|mem}
  ROW(2, 1, 1, 0, 56 , 55 , 0  , 0  , 0  , 0  ), // #481 {zmm, m256|mem}
  ROW(2, 1, 1, 0, 175, 65 , 0  , 0  , 0  , 0  ), // #482 {xmm|ymm|zmm, xmm|m64|mem}
  ROW(2, 1, 1, 0, 175, 107, 0  , 0  , 0  , 0  ), // #483 {xmm|ymm|zmm, m32|mem|xmm}
  ROW(4, 1, 1, 0, 104, 50 , 65 , 10 , 0  , 0  ), // #484 {xmm|k, xmm, xmm|m64|mem, i8|u8}
  ROW(4, 1, 1, 0, 106, 50 , 110, 10 , 0  , 0  ), // #485 {k, xmm, xmm|m16|mem, i8|u8}
  ROW(4, 1, 1, 0, 104, 50 , 107, 10 , 0  , 0  ), // #486 {xmm|k, xmm, xmm|m32|mem, i8|u8}
  ROW(2, 1, 1, 0, 50 , 176, 0  , 0  , 0  , 0  ), // #487 {xmm, xmm|m128|ymm|m256|zmm|m512}
  ROW(2, 1, 1, 0, 139, 110, 0  , 0  , 0  , 0  ), // #488 {r32|r64, xmm|m16|mem}
  ROW(3, 1, 1, 0, 50 , 50 , 155, 0  , 0  , 0  ), // #489 {xmm, xmm, r32|m32|mem|r64|m64}
  ROW(3, 1, 1, 0, 51 , 174, 10 , 0  , 0  , 0  ), // #490 {xmm|m128|mem, ymm|zmm, i8|u8}
  ROW(4, 1, 1, 0, 50 , 50 , 65 , 10 , 0  , 0  ), // #491 {xmm, xmm, xmm|m64|mem, i8|u8}
  ROW(4, 1, 1, 0, 50 , 50 , 107, 10 , 0  , 0  ), // #492 {xmm, xmm, xmm|m32|mem, i8|u8}
  ROW(3, 1, 1, 0, 106, 176, 10 , 0  , 0  , 0  ), // #493 {k, xmm|m128|ymm|m256|zmm|m512, i8|u8}
  ROW(3, 1, 1, 0, 106, 65 , 10 , 0  , 0  , 0  ), // #494 {k, xmm|m64|mem, i8|u8}
  ROW(3, 1, 1, 0, 106, 110, 10 , 0  , 0  , 0  ), // #495 {k, xmm|m16|mem, i8|u8}
  ROW(3, 1, 1, 0, 106, 107, 10 , 0  , 0  , 0  ), // #496 {k, xmm|m32|mem, i8|u8}
  ROW(1, 1, 1, 0, 68 , 0  , 0  , 0  , 0  , 0  ), // #497 {vm32y}
  ROW(1, 1, 1, 0, 69 , 0  , 0  , 0  , 0  , 0  ), // #498 {vm32z}
  ROW(1, 1, 1, 0, 72 , 0  , 0  , 0  , 0  , 0  ), // #499 {vm64z}
  ROW(4, 1, 1, 0, 50 , 50 , 110, 10 , 0  , 0  ), // #500 {xmm, xmm, xmm|m16|mem, i8|u8}
  ROW(4, 1, 1, 0, 56 , 56 , 54 , 10 , 0  , 0  ), // #501 {zmm, zmm, ymm|m256|mem, i8|u8}
  ROW(2, 1, 1, 0, 6  , 96 , 0  , 0  , 0  , 0  ), // #502 {r32, xmm|ymm}
  ROW(2, 1, 1, 0, 175, 177, 0  , 0  , 0  , 0  ), // #503 {xmm|ymm|zmm, xmm|m8|mem|r32}
  ROW(2, 1, 1, 0, 175, 178, 0  , 0  , 0  , 0  ), // #504 {xmm|ymm|zmm, xmm|m32|mem|r32}
  ROW(2, 1, 1, 0, 175, 106, 0  , 0  , 0  , 0  ), // #505 {xmm|ymm|zmm, k}
  ROW(2, 1, 1, 0, 175, 179, 0  , 0  , 0  , 0  ), // #506 {xmm|ymm|zmm, xmm|m16|mem|r32}
  ROW(3, 1, 1, 0, 135, 50 , 10 , 0  , 0  , 0  ), // #507 {r32|m16|mem, xmm, i8|u8}
  ROW(4, 1, 1, 0, 50 , 50 , 131, 10 , 0  , 0  ), // #508 {xmm, xmm, r32|m8|mem, i8|u8}
  ROW(4, 1, 1, 0, 50 , 50 , 28 , 10 , 0  , 0  ), // #509 {xmm, xmm, r32|m32|mem, i8|u8}
  ROW(4, 0, 1, 0, 50 , 50 , 15 , 10 , 0  , 0  ), // #510 {xmm, xmm, r64|m64|mem, i8|u8}
  ROW(4, 1, 1, 0, 50 , 50 , 135, 10 , 0  , 0  ), // #511 {xmm, xmm, r32|m16|mem, i8|u8}
  ROW(2, 1, 1, 0, 106, 175, 0  , 0  , 0  , 0  ), // #512 {k, xmm|ymm|zmm}
  ROW(1, 1, 1, 0, 124, 0  , 0  , 0  , 0  , 0  ), // #513 {rel16|rel32}
  ROW(3, 1, 1, 2, 113, 35 , 36 , 0  , 0  , 0  ), // #514 {mem, <edx>, <eax>}
  ROW(3, 0, 1, 2, 113, 35 , 36 , 0  , 0  , 0  )  // #515 {mem, <edx>, <eax>}
};
#undef ROW

#define ROW(opFlags, regId) { opFlags, uint8_t(regId) }
#define F(VAL) uint64_t(InstDB::OpFlags::k##VAL)
const InstDB::OpSignature InstDB::_opSignatureTable[] = {
  ROW(0, 0xFF),
  ROW(F(RegGpbLo) | F(RegGpbHi) | F(MemUnspecified) | F(Mem8), 0x00),
  ROW(F(RegGpbLo) | F(RegGpbHi), 0x00),
  ROW(F(RegGpw) | F(RegSReg) | F(MemUnspecified) | F(Mem16), 0x00),
  ROW(F(RegGpw), 0x00),
  ROW(F(RegGpd) | F(RegSReg) | F(MemUnspecified) | F(Mem32), 0x00),
  ROW(F(RegGpd), 0x00),
  ROW(F(RegGpq) | F(RegSReg) | F(RegCReg) | F(RegDReg) | F(MemUnspecified) | F(Mem64), 0x00),
  ROW(F(RegGpq), 0x00),
  ROW(F(RegGpbLo) | F(RegGpbHi) | F(Mem8), 0x00),
  ROW(F(ImmI8) | F(ImmU8), 0x00),
  ROW(F(RegGpw) | F(Mem16), 0x00),
  ROW(F(ImmI16) | F(ImmU16), 0x00),
  ROW(F(RegGpd) | F(Mem32), 0x00),
  ROW(F(ImmI32) | F(ImmU32), 0x00),
  ROW(F(RegGpq) | F(MemUnspecified) | F(Mem64), 0x00),
  ROW(F(ImmI32), 0x00),
  ROW(F(RegSReg) | F(RegCReg) | F(RegDReg) | F(MemUnspecified) | F(Mem64) | F(ImmI64) | F(ImmU64), 0x00),
  ROW(F(MemUnspecified) | F(Mem8), 0x00),
  ROW(F(RegSReg) | F(MemUnspecified) | F(Mem16), 0x00),
  ROW(F(RegSReg) | F(MemUnspecified) | F(Mem32), 0x00),
  ROW(F(MemUnspecified) | F(Mem16), 0x00),
  ROW(F(RegSReg), 0x00),
  ROW(F(RegCReg) | F(RegDReg), 0x00),
  ROW(F(RegGpq) | F(ImmI32), 0x00),
  ROW(F(RegGpw) | F(RegGpd) | F(RegGpq) | F(MemUnspecified) | F(Mem16) | F(Mem32) | F(Mem64), 0x00),
  ROW(F(ImmI8), 0x00),
  ROW(F(RegGpw) | F(MemUnspecified) | F(Mem16), 0x00),
  ROW(F(RegGpd) | F(MemUnspecified) | F(Mem32), 0x00),
  ROW(F(MemUnspecified) | F(Mem32), 0x00),
  ROW(F(MemUnspecified) | F(Mem64), 0x00),
  ROW(F(RegGpbLo) | F(RegGpbHi) | F(RegGpw) | F(RegGpd) | F(RegGpq) | F(MemUnspecified) | F(Mem8) | F(Mem16) | F(Mem32) | F(Mem64), 0x00),
  ROW(F(RegGpq) | F(MemUnspecified) | F(Mem64) | F(ImmI32) | F(ImmU32), 0x00),
  ROW(F(RegGpw) | F(FlagImplicit), 0x01),
  ROW(F(RegGpw) | F(FlagImplicit), 0x04),
  ROW(F(RegGpd) | F(FlagImplicit), 0x04),
  ROW(F(RegGpd) | F(FlagImplicit), 0x01),
  ROW(F(RegGpq) | F(FlagImplicit), 0x04),
  ROW(F(RegGpq) | F(FlagImplicit), 0x01),
  ROW(F(RegGpw) | F(MemUnspecified) | F(Mem16) | F(ImmI8) | F(ImmI16), 0x00),
  ROW(F(RegGpd) | F(MemUnspecified) | F(Mem32) | F(ImmI8) | F(ImmI32), 0x00),
  ROW(F(RegGpq) | F(MemUnspecified) | F(Mem64) | F(ImmI8) | F(ImmI32), 0x00),
  ROW(F(ImmI8) | F(ImmI16) | F(ImmU16), 0x00),
  ROW(F(ImmI8) | F(ImmI32) | F(ImmU32), 0x00),
  ROW(F(ImmI8) | F(ImmI32), 0x00),
  ROW(F(ImmI64) | F(ImmU64), 0x00),
  ROW(F(RegGpbLo), 0x01),
  ROW(F(RegGpw), 0x01),
  ROW(F(RegGpd), 0x01),
  ROW(F(RegGpq), 0x01),
  ROW(F(RegXmm), 0x00),
  ROW(F(RegXmm) | F(MemUnspecified) | F(Mem128), 0x00),
  ROW(F(MemUnspecified) | F(Mem128), 0x00),
  ROW(F(RegYmm), 0x00),
  ROW(F(RegYmm) | F(MemUnspecified) | F(Mem256), 0x00),
  ROW(F(MemUnspecified) | F(Mem256), 0x00),
  ROW(F(RegZmm), 0x00),
  ROW(F(RegZmm) | F(MemUnspecified) | F(Mem512), 0x00),
  ROW(F(MemUnspecified) | F(Mem512), 0x00),
  ROW(F(RegXmm) | F(MemUnspecified) | F(Mem128) | F(ImmI8) | F(ImmU8), 0x00),
  ROW(F(RegYmm) | F(MemUnspecified) | F(Mem256) | F(ImmI8) | F(ImmU8), 0x00),
  ROW(F(RegZmm) | F(MemUnspecified) | F(Mem512) | F(ImmI8) | F(ImmU8), 0x00),
  ROW(F(RegMm), 0x00),
  ROW(F(RegGpq) | F(RegMm) | F(MemUnspecified) | F(Mem64), 0x00),
  ROW(F(RegXmm) | F(RegMm), 0x00),
  ROW(F(RegXmm) | F(MemUnspecified) | F(Mem64), 0x00),
  ROW(F(RegGpw) | F(RegGpd) | F(RegGpq) | F(Mem16) | F(Mem32) | F(Mem64), 0x00),
  ROW(F(Vm32x), 0x00),
  ROW(F(Vm32y), 0x00),
  ROW(F(Vm32z), 0x00),
  ROW(F(Vm64x), 0x00),
  ROW(F(Vm64y), 0x00),
  ROW(F(Vm64z), 0x00),
  ROW(F(Mem8) | F(FlagMemBase) | F(FlagMemDs) | F(FlagImplicit), 0x40),
  ROW(F(Mem8) | F(FlagMemBase) | F(FlagMemEs) | F(FlagImplicit), 0x80),
  ROW(F(Mem16) | F(FlagMemBase) | F(FlagMemDs) | F(FlagImplicit), 0x40),
  ROW(F(Mem16) | F(FlagMemBase) | F(FlagMemEs) | F(FlagImplicit), 0x80),
  ROW(F(Mem32) | F(FlagMemBase) | F(FlagMemDs) | F(FlagImplicit), 0x40),
  ROW(F(Mem32) | F(FlagMemBase) | F(FlagMemEs) | F(FlagImplicit), 0x80),
  ROW(F(Mem64) | F(FlagMemBase) | F(FlagMemDs) | F(FlagImplicit), 0x40),
  ROW(F(Mem64) | F(FlagMemBase) | F(FlagMemEs) | F(FlagImplicit), 0x80),
  ROW(F(RegGpbLo) | F(FlagImplicit), 0x01),
  ROW(F(MemUnspecified) | F(Mem8) | F(FlagMemBase) | F(FlagMemDs) | F(FlagImplicit), 0x40),
  ROW(F(MemUnspecified) | F(Mem16) | F(FlagMemBase) | F(FlagMemDs) | F(FlagImplicit), 0x40),
  ROW(F(MemUnspecified) | F(Mem32) | F(FlagMemBase) | F(FlagMemDs) | F(FlagImplicit), 0x40),
  ROW(F(MemUnspecified) | F(Mem64) | F(FlagMemBase) | F(FlagMemDs) | F(FlagImplicit), 0x40),
  ROW(F(RegGpw) | F(RegGpq) | F(Mem16) | F(Mem64), 0x00),
  ROW(F(RegSReg), 0x1A),
  ROW(F(RegSReg), 0x60),
  ROW(F(RegGpw) | F(RegGpq) | F(Mem16) | F(Mem64) | F(ImmI8) | F(ImmI16) | F(ImmI32), 0x00),
  ROW(F(RegGpd) | F(Mem32) | F(ImmI32) | F(ImmU32), 0x00),
  ROW(F(RegSReg), 0x1E),
  ROW(F(MemUnspecified) | F(Mem8) | F(FlagMemBase) | F(FlagMemEs) | F(FlagImplicit), 0x80),
  ROW(F(MemUnspecified) | F(Mem16) | F(FlagMemBase) | F(FlagMemEs) | F(FlagImplicit), 0x80),
  ROW(F(MemUnspecified) | F(Mem32) | F(FlagMemBase) | F(FlagMemEs) | F(FlagImplicit), 0x80),
  ROW(F(MemUnspecified) | F(Mem64) | F(FlagMemBase) | F(FlagMemEs) | F(FlagImplicit), 0x80),
  ROW(F(RegXmm) | F(RegYmm), 0x00),
  ROW(F(ImmI4) | F(ImmU4), 0x00),
  ROW(F(Mem32) | F(Mem64), 0x00),
  ROW(F(RegSt), 0x01),
  ROW(F(RegSt), 0x00),
  ROW(F(MemUnspecified) | F(Mem48), 0x00),
  ROW(F(MemUnspecified) | F(Mem80), 0x00),
  ROW(F(RegGpbLo) | F(ImmI8) | F(ImmU8), 0x02),
  ROW(F(RegXmm) | F(RegKReg), 0x00),
  ROW(F(RegYmm) | F(RegKReg), 0x00),
  ROW(F(RegKReg), 0x00),
  ROW(F(RegXmm) | F(MemUnspecified) | F(Mem32), 0x00),
  ROW(F(Vm64x) | F(Vm64y), 0x00),
  ROW(F(RegGpq) | F(RegXmm) | F(MemUnspecified) | F(Mem64), 0x00),
  ROW(F(RegXmm) | F(MemUnspecified) | F(Mem16), 0x00),
  ROW(F(RegBnd), 0x00),
  ROW(F(RegBnd) | F(MemUnspecified), 0x00),
  ROW(F(MemUnspecified), 0x00),
  ROW(F(RegGpw) | F(RegGpd) | F(Mem16) | F(Mem32) | F(ImmI32) | F(ImmI64) | F(Rel32), 0x00),
  ROW(F(RegGpq) | F(MemUnspecified) | F(Mem64) | F(ImmI32) | F(ImmI64) | F(Rel32), 0x00),
  ROW(F(RegGpbLo) | F(RegGpbHi) | F(RegGpw) | F(RegGpd) | F(Mem8) | F(Mem16) | F(Mem32), 0x00),
  ROW(F(RegGpbLo) | F(RegGpbHi) | F(RegGpq) | F(Mem8) | F(Mem64), 0x00),
  ROW(F(RegGpw) | F(RegGpd), 0x00),
  ROW(F(MemUnspecified) | F(Mem512) | F(FlagMemBase) | F(FlagMemEs), 0x00),
  ROW(F(RegSt) | F(Mem32) | F(Mem64), 0x00),
  ROW(F(RegGpd) | F(FlagImplicit), 0x02),
  ROW(F(RegGpd) | F(RegGpq) | F(FlagImplicit), 0x01),
  ROW(F(ImmI32) | F(ImmI64) | F(Rel8) | F(Rel32), 0x00),
  ROW(F(ImmI32) | F(ImmI64) | F(Rel32), 0x00),
  ROW(F(RegGpw) | F(RegGpd) | F(FlagImplicit), 0x02),
  ROW(F(ImmI32) | F(ImmI64) | F(Rel8), 0x00),
  ROW(F(RegGpd) | F(RegGpq) | F(FlagImplicit), 0x02),
  ROW(F(RegGpq) | F(MemUnspecified) | F(Mem64) | F(ImmI32) | F(ImmI64) | F(Rel8) | F(Rel32), 0x00),
  ROW(F(RegGpd) | F(MemUnspecified) | F(Mem32) | F(ImmI32) | F(ImmI64) | F(Rel32), 0x00),
  ROW(F(RegGpd) | F(RegKReg) | F(MemUnspecified) | F(Mem8), 0x00),
  ROW(F(RegGpd) | F(MemUnspecified) | F(Mem8), 0x00),
  ROW(F(RegGpd) | F(RegKReg) | F(MemUnspecified) | F(Mem32), 0x00),
  ROW(F(RegGpq) | F(RegKReg) | F(MemUnspecified) | F(Mem64), 0x00),
  ROW(F(RegGpd) | F(RegKReg) | F(MemUnspecified) | F(Mem16), 0x00),
  ROW(F(RegGpd) | F(MemUnspecified) | F(Mem16), 0x00),
  ROW(F(ImmI16), 0x00),
  ROW(F(ImmI16) | F(ImmI32), 0x00),
  ROW(F(MemUnspecified) | F(Mem32) | F(Mem48) | F(Mem80), 0x00),
  ROW(F(RegGpd) | F(RegGpq), 0x00),
  ROW(F(RegGpbLo) | F(RegGpbHi) | F(RegGpw) | F(Mem8) | F(Mem16), 0x00),
  ROW(F(RegMm) | F(MemUnspecified) | F(Mem64), 0x00),
  ROW(F(RegMm) | F(MemUnspecified) | F(Mem64) | F(ImmI8) | F(ImmU8), 0x00),
  ROW(F(RegMm) | F(MemUnspecified) | F(Mem32), 0x00),
  ROW(F(ImmU16), 0x00),
  ROW(F(RegXmm) | F(RegYmm) | F(Mem128) | F(Mem256), 0x00),
  ROW(F(RegXmm) | F(ImmI8) | F(ImmU8), 0x00),
  ROW(F(RegXmm) | F(FlagImplicit), 0x01),
  ROW(F(MemUnspecified) | F(FlagMib), 0x00),
  ROW(F(RegGpw) | F(RegGpd) | F(RegGpq), 0x00),
  ROW(F(MemUnspecified) | F(Mem8) | F(Mem16) | F(Mem32) | F(Mem48) | F(Mem64) | F(Mem80) | F(Mem128) | F(Mem256) | F(Mem512) | F(Mem1024), 0x00),
  ROW(F(MemUnspecified) | F(Mem512) | F(FlagMemBase) | F(FlagMemDs) | F(FlagImplicit), 0x01),
  ROW(F(RegGpq) | F(FlagImplicit), 0x02),
  ROW(F(RegGpq) | F(FlagImplicit), 0x08),
  ROW(F(RegGpd) | F(FlagImplicit), 0x08),
  ROW(F(RegGpd) | F(RegGpq) | F(MemUnspecified) | F(Mem32) | F(Mem64), 0x00),
  ROW(F(Mem16) | F(Mem32), 0x00),
  ROW(F(Mem16) | F(Mem32) | F(Mem64), 0x00),
  ROW(F(RegSt) | F(Mem32) | F(Mem64) | F(Mem80), 0x00),
  ROW(F(RegGpw) | F(MemUnspecified) | F(Mem16), 0x01),
  ROW(F(RegGpbLo) | F(RegGpw) | F(RegGpd), 0x01),
  ROW(F(RegGpw) | F(ImmI8) | F(ImmU8), 0x04),
  ROW(F(Mem8) | F(Mem16) | F(Mem32) | F(FlagMemBase) | F(FlagMemEs), 0x80),
  ROW(F(RegGpw), 0x04),
  ROW(F(RegGpbHi) | F(FlagImplicit), 0x01),
  ROW(F(MemUnspecified) | F(Mem128) | F(FlagMemBase) | F(FlagMemDs) | F(FlagImplicit), 0x80),
  ROW(F(MemUnspecified) | F(Mem64) | F(FlagMemBase) | F(FlagMemDs) | F(FlagImplicit), 0x80),
  ROW(F(MemUnspecified) | F(FlagMemBase) | F(FlagMemDs) | F(FlagImplicit), 0x01),
  ROW(F(RegGpw) | F(ImmU8), 0x04),
  ROW(F(Mem8) | F(Mem16) | F(Mem32) | F(FlagMemBase) | F(FlagMemDs), 0x40),
  ROW(F(RegGpw) | F(RegGpd) | F(RegGpq) | F(MemUnspecified) | F(Mem16), 0x00),
  ROW(F(RegTmm), 0x00),
  ROW(F(MemUnspecified) | F(FlagTMem), 0x00),
  ROW(F(MemUnspecified) | F(FlagMemBase) | F(FlagMemDs), 0x00),
  ROW(F(RegYmm) | F(RegZmm), 0x00),
  ROW(F(RegXmm) | F(RegYmm) | F(RegZmm), 0x00),
  ROW(F(RegXmm) | F(RegYmm) | F(RegZmm) | F(Mem128) | F(Mem256) | F(Mem512), 0x00),
  ROW(F(RegGpd) | F(RegXmm) | F(MemUnspecified) | F(Mem8), 0x00),
  ROW(F(RegGpd) | F(RegXmm) | F(MemUnspecified) | F(Mem32), 0x00),
  ROW(F(RegGpd) | F(RegXmm) | F(MemUnspecified) | F(Mem16), 0x00)
};
#undef F
#undef ROW
// ----------------------------------------------------------------------------
// ${InstSignatureTable:End}
#endif // !ASMJIT_NO_VALIDATION

// x86::InstInternal - QueryRWInfo
// ===============================

// ${InstRWInfoTable:Begin}
// ------------------- Automatically generated, do not edit -------------------
const uint8_t InstDB::rwInfoIndexA[Inst::_kIdCount] = {
  0, 0, 1, 1, 0, 2, 3, 2, 4, 4, 5, 6, 4, 4, 3, 4, 4, 4, 4, 7, 0, 2, 0, 4, 4, 4,
  4, 8, 0, 9, 9, 9, 9, 9, 0, 0, 0, 0, 9, 9, 9, 9, 9, 10, 10, 10, 11, 11, 12, 13,
  14, 9, 9, 0, 15, 16, 16, 16, 0, 0, 0, 17, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
  3, 3, 3, 3, 3, 3, 3, 18, 0, 0, 19, 0, 0, 0, 0, 0, 20, 21, 0, 22, 23, 24, 7, 25,
  25, 25, 24, 26, 7, 24, 27, 28, 29, 30, 31, 32, 33, 25, 25, 7, 27, 28, 33, 34,
  0, 0, 0, 0, 35, 4, 4, 5, 6, 0, 0, 0, 0, 0, 36, 36, 0, 0, 37, 0, 0, 38, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 38, 0, 38, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 38, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 38, 0, 38,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4, 4, 4, 0, 39, 4,
  4, 35, 40, 41, 0, 0, 0, 42, 0, 37, 0, 0, 0, 0, 43, 0, 44, 43, 43, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 45, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 46, 47, 48, 49, 50, 51,
  52, 53, 0, 0, 0, 54, 55, 56, 57, 0, 0, 0, 0, 0, 0, 0, 0, 0, 54, 55, 56, 57, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 58, 0, 59, 0, 60, 0, 61, 0, 60, 0, 60, 0, 60,
  0, 0, 0, 0, 0, 62, 63, 63, 63, 58, 60, 0, 0, 0, 9, 0, 0, 4, 4, 5, 6, 0, 0, 4,
  4, 5, 6, 0, 0, 64, 65, 66, 66, 67, 47, 24, 36, 67, 52, 66, 66, 68, 69, 69, 70,
  71, 71, 72, 72, 59, 59, 67, 59, 59, 71, 71, 73, 48, 52, 74, 48, 7, 7, 47, 75,
  9, 66, 66, 75, 0, 35, 4, 4, 5, 6, 0, 76, 0, 0, 77, 0, 2, 4, 4, 78, 79, 9, 9,
  9, 3, 3, 4, 3, 3, 3, 3, 3, 3, 3, 3, 3, 0, 3, 3, 0, 3, 80, 3, 0, 0, 0, 3, 3,
  4, 3, 0, 0, 3, 3, 4, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 27, 27, 80, 80, 80, 80, 80,
  80, 80, 80, 80, 80, 27, 80, 80, 80, 27, 27, 80, 80, 80, 3, 3, 3, 81, 3, 3, 3,
  27, 27, 0, 0, 0, 0, 3, 3, 4, 4, 3, 3, 4, 4, 4, 4, 3, 3, 4, 4, 82, 83, 84, 24,
  24, 24, 83, 83, 84, 24, 24, 24, 83, 4, 3, 80, 3, 3, 4, 3, 3, 0, 0, 0, 9, 0, 0,
  0, 3, 0, 0, 0, 0, 0, 0, 0, 3, 3, 0, 0, 0, 0, 3, 3, 3, 3, 85, 3, 3, 0, 3, 3,
  3, 85, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 27, 86, 0, 3, 3, 4, 3, 87, 87, 4, 87, 0,
  0, 0, 0, 0, 0, 0, 3, 88, 7, 89, 88, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 90, 0, 0,
  0, 0, 0, 88, 88, 0, 0, 0, 0, 0, 0, 7, 89, 0, 0, 88, 88, 0, 0, 2, 91, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 4, 4, 4, 0, 4, 4, 0, 88, 0, 0, 88, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 7, 7, 26, 89, 0, 0, 0, 0, 0, 0, 92, 0, 0, 0, 2, 4, 4, 5, 6, 0, 0, 0, 0, 0,
  0, 0, 9, 0, 0, 0, 0, 0, 15, 0, 93, 93, 0, 94, 0, 0, 9, 9, 20, 21, 95, 95, 0, 0,
  0, 0, 4, 4, 4, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 7, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 96, 28, 97, 98, 97, 98, 96, 28, 97, 98, 97, 98,
  99, 100, 0, 0, 0, 0, 0, 0, 20, 101, 21, 102, 102, 103, 75, 9, 0, 75, 104, 105,
  104, 9, 104, 9, 106, 107, 103, 106, 107, 106, 107, 9, 9, 9, 103, 0, 75, 103,
  9, 103, 9, 105, 104, 0, 28, 0, 28, 0, 108, 0, 108, 0, 0, 0, 0, 0, 33, 33, 104,
  9, 104, 9, 106, 107, 106, 107, 9, 9, 9, 103, 9, 103, 28, 28, 108, 108, 33,
  33, 103, 75, 9, 9, 105, 104, 0, 0, 0, 9, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  109, 109, 9, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 9, 9, 27, 110, 60, 60, 0, 0, 0, 0,
  0, 0, 0, 0, 60, 111, 9, 9, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 67, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 112, 112, 47, 113, 112, 112, 112, 112, 112, 112, 112,
  112, 0, 114, 114, 0, 71, 71, 115, 116, 67, 67, 67, 67, 117, 71, 118, 9, 9,
  73, 112, 112, 49, 0, 0, 0, 102, 0, 0, 0, 0, 0, 0, 0, 0, 0, 119, 0, 0, 0, 0, 0,
  0, 0, 9, 9, 9, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 120, 33, 121, 121, 28, 122, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 102, 102, 102, 102, 0, 0, 0, 0,
  0, 0, 9, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 9, 9, 9, 9, 0, 0, 0, 0, 60, 60, 111, 60, 7, 7, 7, 0, 7, 0,
  7, 7, 7, 7, 7, 7, 0, 7, 7, 81, 7, 0, 7, 0, 0, 7, 0, 0, 0, 0, 9, 9, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 123, 123, 124, 125, 121, 121, 121, 121, 82, 123, 126, 125, 124, 124,
  125, 126, 125, 124, 125, 127, 128, 103, 103, 103, 127, 124, 125, 126, 125,
  124, 125, 123, 125, 127, 128, 103, 103, 103, 127, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  9, 9, 9, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 67, 67, 129, 67,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 119, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 9, 9, 0, 0, 109, 109, 0, 0, 9, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 9, 9, 0, 0, 109, 109, 0, 0, 9, 9, 0, 0, 0, 0, 0, 0,
  0, 0, 67, 67, 0, 0, 0, 0, 0, 0, 0, 0, 67, 129, 0, 0, 0, 0, 0, 0, 9, 9, 9, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 119, 119, 20, 101, 21, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 130, 131, 130, 131, 0, 132, 0, 133, 0, 0, 0, 2, 4, 4, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
};

const uint8_t InstDB::rwInfoIndexB[Inst::_kIdCount] = {
  0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 3, 0, 0, 0,
  0, 0, 4, 0, 0, 0, 0, 0, 5, 5, 6, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 7, 0, 0, 0, 0, 4, 8, 1, 0, 9, 0, 0, 0, 10, 10, 10, 0, 0, 11, 0, 0, 10, 12,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 5, 5, 0, 13, 14, 15, 16, 17, 0, 0, 18, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 19, 1, 1, 20, 21, 0, 0,
  0, 0, 5, 5, 0, 0, 0, 0, 0, 0, 22, 23, 0, 0, 24, 25, 26, 27, 0, 0, 25, 25, 25,
  25, 25, 25, 25, 25, 28, 29, 29, 28, 0, 0, 0, 24, 25, 24, 25, 0, 25, 24, 24, 24,
  24, 24, 24, 24, 0, 0, 30, 30, 30, 24, 24, 28, 0, 31, 10, 0, 0, 0, 0, 0, 0, 24,
  25, 0, 0, 0, 32, 33, 32, 34, 0, 0, 0, 0, 0, 10, 32, 0, 0, 0, 0, 35, 33, 32,
  35, 34, 24, 25, 24, 25, 0, 29, 29, 29, 29, 0, 0, 0, 25, 10, 10, 32, 32, 0, 0,
  0, 0, 5, 5, 0, 0, 0, 0, 0, 0, 0, 21, 36, 0, 20, 37, 38, 0, 39, 40, 0, 0, 0, 0,
  0, 10, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 8, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 41, 42, 43, 44, 41, 42, 41, 42, 43,
  44, 43, 44, 0, 0, 0, 0, 0, 0, 0, 0, 41, 42, 43, 0, 0, 0, 0, 44, 45, 46, 47,
  48, 45, 46, 47, 48, 0, 0, 0, 0, 49, 50, 51, 41, 42, 43, 44, 41, 42, 43, 44, 52,
  0, 24, 0, 53, 0, 54, 0, 0, 0, 0, 0, 10, 0, 10, 24, 55, 56, 55, 0, 0, 0, 0,
  0, 0, 55, 57, 57, 0, 58, 59, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 60, 60, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5, 61, 0, 0, 0, 0, 62, 0, 63, 20, 64, 20, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 65, 0, 0, 0, 0, 0, 0, 6,
  5, 5, 0, 0, 0, 0, 66, 67, 0, 0, 0, 0, 68, 69, 0, 3, 3, 70, 22, 71, 72, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 73, 39, 74, 75, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 76, 0, 0, 0, 0, 0, 0, 0, 10,
  10, 10, 10, 10, 10, 10, 0, 0, 2, 2, 2, 77, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 64, 0, 0, 0, 0, 0, 0, 0, 0, 78, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 79, 79, 80, 79, 80, 80, 80, 79, 79, 81, 82, 0, 83, 0,
  0, 0, 0, 0, 0, 84, 2, 2, 85, 86, 0, 0, 0, 11, 87, 0, 0, 4, 0, 0, 0, 88, 0, 89,
  89, 89, 89, 89, 89, 89, 89, 89, 89, 89, 89, 89, 89, 89, 89, 89, 89, 89, 89,
  89, 89, 89, 89, 89, 89, 89, 89, 89, 0, 89, 0, 32, 0, 0, 0, 5, 0, 0, 6, 0, 90,
  4, 0, 90, 4, 5, 5, 32, 19, 91, 79, 91, 0, 0, 0, 0, 0, 0, 0, 0, 0, 92, 0, 91, 93,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 94, 94, 94, 94, 94, 0, 0, 0, 0, 0,
  0, 95, 96, 0, 0, 0, 0, 0, 0, 0, 0, 56, 96, 0, 0, 0, 0, 97, 98, 97, 98, 3, 3,
  3, 99, 100, 101, 3, 3, 3, 3, 3, 3, 0, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 102, 102,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 103, 3, 104, 105, 106, 0, 0,
  0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 107,
  0, 0, 0, 0, 0, 0, 0, 108, 0, 109, 0, 110, 0, 110, 0, 111, 112, 113, 114, 115,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 111, 112, 113, 0, 0, 3, 3, 3, 3, 99, 110, 101, 3, 116, 3, 55, 55, 0, 0,
  0, 0, 117, 118, 119, 118, 119, 117, 118, 119, 118, 119, 22, 120, 121, 120, 121,
  120, 120, 122, 123, 120, 120, 120, 124, 125, 126, 120, 120, 120, 124, 125,
  126, 120, 120, 120, 124, 125, 126, 120, 121, 127, 127, 128, 129, 120, 120, 120,
  120, 120, 120, 120, 120, 120, 127, 127, 120, 120, 120, 124, 130, 126, 120,
  120, 120, 124, 130, 126, 120, 120, 120, 124, 130, 126, 120, 120, 120, 120, 120,
  120, 120, 120, 120, 127, 127, 127, 127, 128, 129, 120, 121, 120, 120, 120, 124,
  125, 126, 120, 120, 120, 124, 125, 126, 120, 120, 120, 124, 125, 126, 127,
  127, 128, 129, 120, 120, 120, 124, 130, 126, 120, 120, 120, 124, 130, 126, 120,
  120, 120, 131, 130, 132, 127, 127, 128, 129, 133, 133, 133, 77, 134, 135, 0,
  0, 0, 0, 136, 137, 10, 10, 10, 10, 10, 10, 10, 10, 137, 138, 0, 0, 0, 139, 140,
  141, 84, 84, 84, 139, 140, 141, 3, 3, 3, 3, 3, 3, 3, 142, 143, 144, 143, 144,
  142, 143, 144, 143, 144, 101, 0, 53, 58, 145, 145, 3, 3, 3, 99, 100, 101,
  0, 146, 0, 3, 3, 3, 99, 100, 101, 0, 147, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 148, 149, 149, 150, 151, 151, 0, 0, 0, 0, 0, 0, 0, 152, 153, 0, 0, 154, 0,
  0, 0, 3, 11, 146, 0, 0, 155, 147, 3, 3, 3, 99, 100, 101, 0, 11, 3, 3, 156, 156,
  157, 157, 0, 0, 0, 0, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
  3, 3, 3, 3, 3, 3, 3, 3, 102, 3, 0, 0, 0, 0, 0, 0, 3, 127, 103, 103, 3, 3, 3, 3,
  66, 67, 3, 3, 3, 3, 68, 69, 103, 103, 103, 103, 103, 103, 116, 116, 0, 0, 0,
  0, 116, 116, 116, 116, 116, 116, 0, 0, 120, 120, 120, 120, 158, 158, 3, 3, 3,
  120, 3, 3, 120, 120, 127, 127, 159, 159, 159, 3, 159, 3, 120, 120, 120, 120,
  120, 3, 0, 0, 0, 0, 70, 22, 71, 160, 137, 136, 138, 137, 0, 0, 0, 3, 0, 3, 0,
  0, 0, 0, 0, 0, 3, 0, 0, 0, 0, 3, 0, 3, 3, 0, 161, 101, 99, 100, 0, 0, 162, 162,
  162, 162, 162, 162, 162, 162, 162, 162, 162, 162, 120, 120, 3, 3, 145, 145,
  3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 3, 3, 3, 3, 3, 3, 3, 3, 3, 0, 0, 0, 0, 3, 3, 3, 163, 84, 84, 3, 3, 84,
  84, 3, 3, 164, 164, 164, 164, 3, 0, 0, 0, 0, 164, 164, 164, 164, 164, 164, 3,
  3, 120, 120, 120, 3, 164, 164, 3, 3, 120, 120, 120, 3, 3, 103, 84, 84, 84, 3,
  3, 3, 165, 166, 165, 3, 3, 3, 165, 165, 165, 3, 3, 3, 165, 165, 166, 165, 3,
  3, 3, 165, 3, 3, 3, 3, 3, 3, 3, 3, 120, 120, 0, 103, 103, 103, 103, 103, 103,
  103, 103, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 139, 141, 0, 0, 139, 141, 0,
  0, 139, 141, 0, 0, 140, 141, 84, 84, 84, 139, 140, 141, 84, 84, 84, 139, 140,
  141, 84, 84, 139, 141, 0, 0, 139, 141, 0, 0, 139, 141, 0, 0, 140, 141, 3, 3,
  3, 99, 100, 101, 0, 0, 10, 10, 10, 10, 10, 10, 10, 10, 0, 0, 3, 3, 3, 3, 3, 3,
  0, 0, 0, 139, 140, 141, 92, 3, 3, 3, 99, 100, 101, 0, 0, 0, 0, 0, 3, 3, 3, 3,
  3, 3, 0, 0, 0, 0, 56, 56, 167, 0, 0, 0, 0, 0, 0, 0, 0, 0, 80, 0, 0, 0, 0, 0,
  168, 168, 168, 168, 169, 169, 169, 169, 169, 169, 169, 169, 167, 0, 0
};

const InstDB::RWInfo InstDB::rwInfoA[] = {
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 0 , 0 , 0 , 0 , 0 , 0  } }, // #0 [ref=1008x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 1 , 0 , 0 , 0 , 0 , 0  } }, // #1 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 1 , { 2 , 3 , 0 , 0 , 0 , 0  } }, // #2 [ref=7x]
  { InstDB::RWInfo::kCategoryGeneric   , 2 , { 2 , 3 , 0 , 0 , 0 , 0  } }, // #3 [ref=96x]
  { InstDB::RWInfo::kCategoryGeneric   , 3 , { 4 , 5 , 0 , 0 , 0 , 0  } }, // #4 [ref=55x]
  { InstDB::RWInfo::kCategoryGeneric   , 4 , { 6 , 7 , 0 , 0 , 0 , 0  } }, // #5 [ref=6x]
  { InstDB::RWInfo::kCategoryGeneric   , 5 , { 8 , 9 , 0 , 0 , 0 , 0  } }, // #6 [ref=6x]
  { InstDB::RWInfo::kCategoryGeneric   , 3 , { 10, 5 , 0 , 0 , 0 , 0  } }, // #7 [ref=26x]
  { InstDB::RWInfo::kCategoryGeneric   , 7 , { 12, 13, 0 , 0 , 0 , 0  } }, // #8 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 2 , { 11, 3 , 0 , 0 , 0 , 0  } }, // #9 [ref=75x]
  { InstDB::RWInfo::kCategoryGeneric   , 2 , { 5 , 3 , 0 , 0 , 0 , 0  } }, // #10 [ref=3x]
  { InstDB::RWInfo::kCategoryGeneric   , 8 , { 10, 3 , 0 , 0 , 0 , 0  } }, // #11 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 9 , { 10, 5 , 0 , 0 , 0 , 0  } }, // #12 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 8 , { 15, 5 , 0 , 0 , 0 , 0  } }, // #13 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 3 , 3 , 0 , 0 , 0 , 0  } }, // #14 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 10, { 3 , 3 , 0 , 0 , 0 , 0  } }, // #15 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 10, { 2 , 3 , 0 , 0 , 0 , 0  } }, // #16 [ref=3x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 16, 17, 0 , 0 , 0 , 0  } }, // #17 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 1 , { 3 , 3 , 0 , 0 , 0 , 0  } }, // #18 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 20, 21, 0 , 0 , 0 , 0  } }, // #19 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 4 , { 7 , 7 , 0 , 0 , 0 , 0  } }, // #20 [ref=4x]
  { InstDB::RWInfo::kCategoryGeneric   , 5 , { 9 , 9 , 0 , 0 , 0 , 0  } }, // #21 [ref=4x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 33, 34, 0 , 0 , 0 , 0  } }, // #22 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 14, { 2 , 3 , 0 , 0 , 0 , 0  } }, // #23 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 4 , { 10, 7 , 0 , 0 , 0 , 0  } }, // #24 [ref=10x]
  { InstDB::RWInfo::kCategoryGeneric   , 3 , { 35, 5 , 0 , 0 , 0 , 0  } }, // #25 [ref=5x]
  { InstDB::RWInfo::kCategoryGeneric   , 4 , { 36, 7 , 0 , 0 , 0 , 0  } }, // #26 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 4 , { 35, 7 , 0 , 0 , 0 , 0  } }, // #27 [ref=11x]
  { InstDB::RWInfo::kCategoryGeneric   , 4 , { 11, 7 , 0 , 0 , 0 , 0  } }, // #28 [ref=9x]
  { InstDB::RWInfo::kCategoryGeneric   , 4 , { 37, 7 , 0 , 0 , 0 , 0  } }, // #29 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 14, { 36, 3 , 0 , 0 , 0 , 0  } }, // #30 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 14, { 37, 3 , 0 , 0 , 0 , 0  } }, // #31 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 5 , { 36, 9 , 0 , 0 , 0 , 0  } }, // #32 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 5 , { 11, 9 , 0 , 0 , 0 , 0  } }, // #33 [ref=7x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 38, 39, 0 , 0 , 0 , 0  } }, // #34 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 15, { 1 , 40, 0 , 0 , 0 , 0  } }, // #35 [ref=3x]
  { InstDB::RWInfo::kCategoryGeneric   , 16, { 11, 43, 0 , 0 , 0 , 0  } }, // #36 [ref=3x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 4 , 5 , 0 , 0 , 0 , 0  } }, // #37 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 45, 46, 0 , 0 , 0 , 0  } }, // #38 [ref=6x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 0 , 50, 0 , 0 , 0 , 0  } }, // #39 [ref=1x]
  { InstDB::RWInfo::kCategoryImul      , 2 , { 0 , 0 , 0 , 0 , 0 , 0  } }, // #40 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 51, 52, 0 , 0 , 0 , 0  } }, // #41 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 54, 52, 0 , 0 , 0 , 0  } }, // #42 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 13, { 3 , 5 , 0 , 0 , 0 , 0  } }, // #43 [ref=3x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 22, 29, 0 , 0 , 0 , 0  } }, // #44 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 55, 0 , 0 , 0 , 0 , 0  } }, // #45 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 23, { 56, 40, 0 , 0 , 0 , 0  } }, // #46 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 24, { 44, 9 , 0 , 0 , 0 , 0  } }, // #47 [ref=4x]
  { InstDB::RWInfo::kCategoryGeneric   , 25, { 35, 7 , 0 , 0 , 0 , 0  } }, // #48 [ref=3x]
  { InstDB::RWInfo::kCategoryGeneric   , 26, { 48, 13, 0 , 0 , 0 , 0  } }, // #49 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 56, 40, 0 , 0 , 0 , 0  } }, // #50 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 44, 9 , 0 , 0 , 0 , 0  } }, // #51 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 35, 7 , 0 , 0 , 0 , 0  } }, // #52 [ref=3x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 48, 13, 0 , 0 , 0 , 0  } }, // #53 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 40, 40, 0 , 0 , 0 , 0  } }, // #54 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 9 , 9 , 0 , 0 , 0 , 0  } }, // #55 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 7 , 7 , 0 , 0 , 0 , 0  } }, // #56 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 13, 13, 0 , 0 , 0 , 0  } }, // #57 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 27, { 11, 3 , 0 , 0 , 0 , 0  } }, // #58 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 13, { 10, 5 , 0 , 0 , 0 , 0  } }, // #59 [ref=5x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 2 , 3 , 0 , 0 , 0 , 0  } }, // #60 [ref=11x]
  { InstDB::RWInfo::kCategoryGeneric   , 8 , { 11, 3 , 0 , 0 , 0 , 0  } }, // #61 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 51, 20, 0 , 0 , 0 , 0  } }, // #62 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 58, 0 , 0 , 0 , 0 , 0  } }, // #63 [ref=3x]
  { InstDB::RWInfo::kCategoryMov       , 29, { 0 , 0 , 0 , 0 , 0 , 0  } }, // #64 [ref=1x]
  { InstDB::RWInfo::kCategoryMovabs    , 0 , { 0 , 0 , 0 , 0 , 0 , 0  } }, // #65 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 30, { 10, 5 , 0 , 0 , 0 , 0  } }, // #66 [ref=6x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 11, 3 , 0 , 0 , 0 , 0  } }, // #67 [ref=14x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 36, 61, 0 , 0 , 0 , 0  } }, // #68 [ref=1x]
  { InstDB::RWInfo::kCategoryMovh64    , 12, { 0 , 0 , 0 , 0 , 0 , 0  } }, // #69 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 62, 7 , 0 , 0 , 0 , 0  } }, // #70 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 12, { 35, 7 , 0 , 0 , 0 , 0  } }, // #71 [ref=7x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 56, 5 , 0 , 0 , 0 , 0  } }, // #72 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 28, { 44, 9 , 0 , 0 , 0 , 0  } }, // #73 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 63, 20, 0 , 0 , 0 , 0  } }, // #74 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 14, { 11, 3 , 0 , 0 , 0 , 0  } }, // #75 [ref=6x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 17, 29, 0 , 0 , 0 , 0  } }, // #76 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 11, { 3 , 3 , 0 , 0 , 0 , 0  } }, // #77 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 52, 22, 0 , 0 , 0 , 0  } }, // #78 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 52, 66, 0 , 0 , 0 , 0  } }, // #79 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 4 , { 26, 7 , 0 , 0 , 0 , 0  } }, // #80 [ref=18x]
  { InstDB::RWInfo::kCategoryGeneric   , 3 , { 69, 5 , 0 , 0 , 0 , 0  } }, // #81 [ref=2x]
  { InstDB::RWInfo::kCategoryVmov1_8   , 0 , { 0 , 0 , 0 , 0 , 0 , 0  } }, // #82 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 5 , { 10, 9 , 0 , 0 , 0 , 0  } }, // #83 [ref=4x]
  { InstDB::RWInfo::kCategoryGeneric   , 27, { 10, 13, 0 , 0 , 0 , 0  } }, // #84 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 4 , 0 , 0 , 0 , 0 , 0  } }, // #85 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 3 , { 5 , 5 , 0 , 0 , 0 , 0  } }, // #86 [ref=1x]
  { InstDB::RWInfo::kCategoryPunpcklxx , 34, { 0 , 0 , 0 , 0 , 0 , 0  } }, // #87 [ref=3x]
  { InstDB::RWInfo::kCategoryGeneric   , 10, { 2 , 71, 0 , 0 , 0 , 0  } }, // #88 [ref=8x]
  { InstDB::RWInfo::kCategoryGeneric   , 5 , { 37, 9 , 0 , 0 , 0 , 0  } }, // #89 [ref=3x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 16, 50, 0 , 0 , 0 , 0  } }, // #90 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 22, 21, 0 , 0 , 0 , 0  } }, // #91 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 63, 22, 0 , 0 , 0 , 0  } }, // #92 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 8 , { 74, 3 , 0 , 0 , 0 , 0  } }, // #93 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 8 , { 11, 43, 0 , 0 , 0 , 0  } }, // #94 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 5 , { 53, 9 , 0 , 0 , 0 , 0  } }, // #95 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 13, { 80, 5 , 0 , 0 , 0 , 0  } }, // #96 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 13, { 11, 5 , 0 , 0 , 0 , 0  } }, // #97 [ref=4x]
  { InstDB::RWInfo::kCategoryGeneric   , 39, { 74, 81, 0 , 0 , 0 , 0  } }, // #98 [ref=4x]
  { InstDB::RWInfo::kCategoryGeneric   , 40, { 11, 7 , 0 , 0 , 0 , 0  } }, // #99 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 41, { 11, 9 , 0 , 0 , 0 , 0  } }, // #100 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 27, { 13, 13, 0 , 0 , 0 , 0  } }, // #101 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 11, { 11, 3 , 0 , 0 , 0 , 0  } }, // #102 [ref=7x]
  { InstDB::RWInfo::kCategoryVmov2_1   , 42, { 0 , 0 , 0 , 0 , 0 , 0  } }, // #103 [ref=14x]
  { InstDB::RWInfo::kCategoryVmov1_2   , 14, { 0 , 0 , 0 , 0 , 0 , 0  } }, // #104 [ref=7x]
  { InstDB::RWInfo::kCategoryGeneric   , 14, { 10, 3 , 0 , 0 , 0 , 0  } }, // #105 [ref=3x]
  { InstDB::RWInfo::kCategoryGeneric   , 42, { 11, 3 , 0 , 0 , 0 , 0  } }, // #106 [ref=5x]
  { InstDB::RWInfo::kCategoryGeneric   , 43, { 11, 5 , 0 , 0 , 0 , 0  } }, // #107 [ref=5x]
  { InstDB::RWInfo::kCategoryGeneric   , 27, { 11, 5 , 0 , 0 , 0 , 0  } }, // #108 [ref=4x]
  { InstDB::RWInfo::kCategoryGeneric   , 47, { 74, 43, 0 , 0 , 0 , 0  } }, // #109 [ref=6x]
  { InstDB::RWInfo::kCategoryGeneric   , 5 , { 44, 9 , 0 , 0 , 0 , 0  } }, // #110 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 18, { 2 , 3 , 0 , 0 , 0 , 0  } }, // #111 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 54, { 11, 3 , 0 , 0 , 0 , 0  } }, // #112 [ref=12x]
  { InstDB::RWInfo::kCategoryVmovddup  , 34, { 0 , 0 , 0 , 0 , 0 , 0  } }, // #113 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 12, { 35, 61, 0 , 0 , 0 , 0  } }, // #114 [ref=2x]
  { InstDB::RWInfo::kCategoryVmovmskpd , 0 , { 0 , 0 , 0 , 0 , 0 , 0  } }, // #115 [ref=1x]
  { InstDB::RWInfo::kCategoryVmovmskps , 0 , { 0 , 0 , 0 , 0 , 0 , 0  } }, // #116 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 55, { 35, 7 , 0 , 0 , 0 , 0  } }, // #117 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 21, { 48, 13, 0 , 0 , 0 , 0  } }, // #118 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 2 , { 3 , 3 , 0 , 0 , 0 , 0  } }, // #119 [ref=4x]
  { InstDB::RWInfo::kCategoryGeneric   , 15, { 11, 40, 0 , 0 , 0 , 0  } }, // #120 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 11, 7 , 0 , 0 , 0 , 0  } }, // #121 [ref=6x]
  { InstDB::RWInfo::kCategoryGeneric   , 27, { 11, 13, 0 , 0 , 0 , 0  } }, // #122 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 35, 3 , 0 , 0 , 0 , 0  } }, // #123 [ref=4x]
  { InstDB::RWInfo::kCategoryVmov1_4   , 58, { 0 , 0 , 0 , 0 , 0 , 0  } }, // #124 [ref=6x]
  { InstDB::RWInfo::kCategoryVmov1_2   , 44, { 0 , 0 , 0 , 0 , 0 , 0  } }, // #125 [ref=9x]
  { InstDB::RWInfo::kCategoryVmov1_8   , 59, { 0 , 0 , 0 , 0 , 0 , 0  } }, // #126 [ref=3x]
  { InstDB::RWInfo::kCategoryVmov4_1   , 43, { 0 , 0 , 0 , 0 , 0 , 0  } }, // #127 [ref=4x]
  { InstDB::RWInfo::kCategoryVmov8_1   , 60, { 0 , 0 , 0 , 0 , 0 , 0  } }, // #128 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 18, { 11, 3 , 0 , 0 , 0 , 0  } }, // #129 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 17, { 44, 9 , 0 , 0 , 0 , 0  } }, // #130 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 32, { 35, 7 , 0 , 0 , 0 , 0  } }, // #131 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 11, { 2 , 2 , 0 , 0 , 0 , 0  } }, // #132 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 54, { 2 , 2 , 0 , 0 , 0 , 0  } }  // #133 [ref=1x]
};

const InstDB::RWInfo InstDB::rwInfoB[] = {
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 0 , 0 , 0 , 0 , 0 , 0  } }, // #0 [ref=775x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 1 , 0 , 0 , 0 , 0 , 0  } }, // #1 [ref=5x]
  { InstDB::RWInfo::kCategoryGeneric   , 3 , { 10, 5 , 0 , 0 , 0 , 0  } }, // #2 [ref=7x]
  { InstDB::RWInfo::kCategoryGeneric   , 6 , { 11, 3 , 3 , 0 , 0 , 0  } }, // #3 [ref=193x]
  { InstDB::RWInfo::kCategoryGeneric   , 2 , { 11, 3 , 3 , 0 , 0 , 0  } }, // #4 [ref=5x]
  { InstDB::RWInfo::kCategoryGeneric   , 3 , { 4 , 5 , 0 , 0 , 0 , 0  } }, // #5 [ref=14x]
  { InstDB::RWInfo::kCategoryGeneric   , 3 , { 4 , 5 , 14, 0 , 0 , 0  } }, // #6 [ref=4x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 2 , 0 , 0 , 0 , 0 , 0  } }, // #7 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 11, { 3 , 0 , 0 , 0 , 0 , 0  } }, // #8 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 18, 0 , 0 , 0 , 0 , 0  } }, // #9 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 8 , { 3 , 0 , 0 , 0 , 0 , 0  } }, // #10 [ref=34x]
  { InstDB::RWInfo::kCategoryGeneric   , 12, { 7 , 0 , 0 , 0 , 0 , 0  } }, // #11 [ref=4x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 19, 0 , 0 , 0 , 0 , 0  } }, // #12 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 4 , { 6 , 7 , 0 , 0 , 0 , 0  } }, // #13 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 5 , { 8 , 9 , 0 , 0 , 0 , 0  } }, // #14 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 11, { 2 , 3 , 22, 0 , 0 , 0  } }, // #15 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 13, { 4 , 23, 18, 24, 25, 0  } }, // #16 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 12, { 26, 27, 28, 29, 30, 0  } }, // #17 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 28, 31, 32, 16, 0 , 0  } }, // #18 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 28, 0 , 0 , 0 , 0 , 0  } }, // #19 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 10, { 2 , 0 , 0 , 0 , 0 , 0  } }, // #20 [ref=4x]
  { InstDB::RWInfo::kCategoryGeneric   , 6 , { 41, 42, 3 , 0 , 0 , 0  } }, // #21 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 17, { 44, 5 , 0 , 0 , 0 , 0  } }, // #22 [ref=4x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 4 , 0 , 0 , 0 , 0 , 0  } }, // #23 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 18, { 3 , 0 , 0 , 0 , 0 , 0  } }, // #24 [ref=17x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 45, 0 , 0 , 0 , 0 , 0  } }, // #25 [ref=16x]
  { InstDB::RWInfo::kCategoryGeneric   , 19, { 46, 0 , 0 , 0 , 0 , 0  } }, // #26 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 19, { 47, 0 , 0 , 0 , 0 , 0  } }, // #27 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 20, { 3 , 0 , 0 , 0 , 0 , 0  } }, // #28 [ref=3x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 46, 0 , 0 , 0 , 0 , 0  } }, // #29 [ref=6x]
  { InstDB::RWInfo::kCategoryGeneric   , 18, { 11, 0 , 0 , 0 , 0 , 0  } }, // #30 [ref=3x]
  { InstDB::RWInfo::kCategoryGeneric   , 21, { 13, 0 , 0 , 0 , 0 , 0  } }, // #31 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 8 , { 11, 0 , 0 , 0 , 0 , 0  } }, // #32 [ref=8x]
  { InstDB::RWInfo::kCategoryGeneric   , 21, { 48, 0 , 0 , 0 , 0 , 0  } }, // #33 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 7 , { 49, 0 , 0 , 0 , 0 , 0  } }, // #34 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 20, { 11, 0 , 0 , 0 , 0 , 0  } }, // #35 [ref=2x]
  { InstDB::RWInfo::kCategoryImul      , 22, { 0 , 0 , 0 , 0 , 0 , 0  } }, // #36 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 53, 0 , 0 , 0 , 0 , 0  } }, // #37 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 26, 0 , 0 , 0 , 0 , 0  } }, // #38 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 5 , { 4 , 9 , 0 , 0 , 0 , 0  } }, // #39 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 4 , 5 , 0 , 0 , 0 , 0  } }, // #40 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 56, 40, 40, 0 , 0 , 0  } }, // #41 [ref=6x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 44, 9 , 9 , 0 , 0 , 0  } }, // #42 [ref=6x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 35, 7 , 7 , 0 , 0 , 0  } }, // #43 [ref=6x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 48, 13, 13, 0 , 0 , 0  } }, // #44 [ref=6x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 56, 40, 0 , 0 , 0 , 0  } }, // #45 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 44, 9 , 0 , 0 , 0 , 0  } }, // #46 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 35, 7 , 0 , 0 , 0 , 0  } }, // #47 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 48, 13, 0 , 0 , 0 , 0  } }, // #48 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 48, 40, 40, 0 , 0 , 0  } }, // #49 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 35, 9 , 9 , 0 , 0 , 0  } }, // #50 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 44, 13, 13, 0 , 0 , 0  } }, // #51 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 57, 0 , 0 , 0 , 0 , 0  } }, // #52 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 28, { 9 , 0 , 0 , 0 , 0 , 0  } }, // #53 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 16, { 43, 0 , 0 , 0 , 0 , 0  } }, // #54 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 7 , { 13, 0 , 0 , 0 , 0 , 0  } }, // #55 [ref=5x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 3 , 0 , 0 , 0 , 0 , 0  } }, // #56 [ref=4x]
  { InstDB::RWInfo::kCategoryGeneric   , 5 , { 3 , 9 , 0 , 0 , 0 , 0  } }, // #57 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 5 , 5 , 59, 0 , 0 , 0  } }, // #58 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 7 , 7 , 59, 0 , 0 , 0  } }, // #59 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 19, 29, 60, 0 , 0 , 0  } }, // #60 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 6 , { 64, 42, 3 , 0 , 0 , 0  } }, // #61 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 6 , { 11, 11, 3 , 65, 0 , 0  } }, // #62 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 17, 29, 30, 0 , 0 , 0  } }, // #63 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 10, { 3 , 0 , 0 , 0 , 0 , 0  } }, // #64 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 2 , { 2 , 3 , 0 , 0 , 0 , 0  } }, // #65 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 3 , { 5 , 5 , 0 , 67, 17, 60 } }, // #66 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 3 , { 5 , 5 , 0 , 68, 17, 60 } }, // #67 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 3 , { 5 , 5 , 0 , 67, 0 , 0  } }, // #68 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 3 , { 5 , 5 , 0 , 68, 0 , 0  } }, // #69 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 31, { 56, 5 , 0 , 0 , 0 , 0  } }, // #70 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 32, { 35, 5 , 0 , 0 , 0 , 0  } }, // #71 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 33, { 48, 3 , 0 , 0 , 0 , 0  } }, // #72 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 15, { 4 , 40, 0 , 0 , 0 , 0  } }, // #73 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 4 , { 4 , 7 , 0 , 0 , 0 , 0  } }, // #74 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 27, { 2 , 13, 0 , 0 , 0 , 0  } }, // #75 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 10, { 70, 0 , 0 , 0 , 0 , 0  } }, // #76 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 4 , { 35, 7 , 0 , 0 , 0 , 0  } }, // #77 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 10, { 65, 0 , 0 , 0 , 0 , 0  } }, // #78 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 11, 0 , 0 , 0 , 0 , 0  } }, // #79 [ref=6x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 16, 50, 29, 0 , 0 , 0  } }, // #80 [ref=5x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 44, 0 , 0 , 0 , 0 , 0  } }, // #81 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 35, 0 , 0 , 0 , 0 , 0  } }, // #82 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 16, 50, 67, 0 , 0 , 0  } }, // #83 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 2 , { 11, 3 , 0 , 0 , 0 , 0  } }, // #84 [ref=19x]
  { InstDB::RWInfo::kCategoryGeneric   , 4 , { 36, 7 , 0 , 0 , 0 , 0  } }, // #85 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 5 , { 37, 9 , 0 , 0 , 0 , 0  } }, // #86 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 72, 0 , 0 , 0 , 0 , 0  } }, // #87 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 7 , 0 , 0 , 0 , 0 , 0  } }, // #88 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 31, { 73, 0 , 0 , 0 , 0 , 0  } }, // #89 [ref=30x]
  { InstDB::RWInfo::kCategoryGeneric   , 11, { 2 , 3 , 71, 0 , 0 , 0  } }, // #90 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 35, { 11, 0 , 0 , 0 , 0 , 0  } }, // #91 [ref=3x]
  { InstDB::RWInfo::kCategoryGeneric   , 28, { 44, 0 , 0 , 0 , 0 , 0  } }, // #92 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 16, { 74, 0 , 0 , 0 , 0 , 0  } }, // #93 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 75, 43, 43, 0 , 0 , 0  } }, // #94 [ref=5x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 74, 0 , 0 , 0 , 0 , 0  } }, // #95 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 9 , 60, 17, 0 , 0 , 0  } }, // #96 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 13, { 75, 76, 77, 77, 77, 5  } }, // #97 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 13, { 4 , 78, 79, 79, 79, 5  } }, // #98 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 36, { 10, 5 , 7 , 0 , 0 , 0  } }, // #99 [ref=8x]
  { InstDB::RWInfo::kCategoryGeneric   , 37, { 10, 5 , 13, 0 , 0 , 0  } }, // #100 [ref=7x]
  { InstDB::RWInfo::kCategoryGeneric   , 38, { 10, 5 , 9 , 0 , 0 , 0  } }, // #101 [ref=9x]
  { InstDB::RWInfo::kCategoryGeneric   , 6 , { 11, 3 , 3 , 3 , 0 , 0  } }, // #102 [ref=3x]
  { InstDB::RWInfo::kCategoryGeneric   , 6 , { 35, 3 , 3 , 0 , 0 , 0  } }, // #103 [ref=18x]
  { InstDB::RWInfo::kCategoryGeneric   , 36, { 11, 5 , 7 , 0 , 0 , 0  } }, // #104 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 37, { 35, 13, 13, 0 , 0 , 0  } }, // #105 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 38, { 11, 5 , 9 , 0 , 0 , 0  } }, // #106 [ref=1x]
  { InstDB::RWInfo::kCategoryVmov1_2   , 44, { 0 , 0 , 0 , 0 , 0 , 0  } }, // #107 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 36, { 10, 5 , 5 , 0 , 0 , 0  } }, // #108 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 36, { 10, 82, 7 , 0 , 0 , 0  } }, // #109 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 37, { 10, 5 , 5 , 0 , 0 , 0  } }, // #110 [ref=3x]
  { InstDB::RWInfo::kCategoryGeneric   , 45, { 10, 61, 3 , 0 , 0 , 0  } }, // #111 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 45, { 10, 3 , 3 , 0 , 0 , 0  } }, // #112 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 45, { 10, 82, 3 , 0 , 0 , 0  } }, // #113 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 38, { 10, 61, 9 , 0 , 0 , 0  } }, // #114 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 38, { 10, 5 , 5 , 0 , 0 , 0  } }, // #115 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 46, { 10, 5 , 5 , 0 , 0 , 0  } }, // #116 [ref=9x]
  { InstDB::RWInfo::kCategoryGeneric   , 48, { 10, 81, 0 , 0 , 0 , 0  } }, // #117 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 48, { 10, 3 , 0 , 0 , 0 , 0  } }, // #118 [ref=4x]
  { InstDB::RWInfo::kCategoryGeneric   , 49, { 80, 43, 0 , 0 , 0 , 0  } }, // #119 [ref=4x]
  { InstDB::RWInfo::kCategoryGeneric   , 6 , { 2 , 3 , 3 , 0 , 0 , 0  } }, // #120 [ref=82x]
  { InstDB::RWInfo::kCategoryGeneric   , 38, { 4 , 5 , 5 , 0 , 0 , 0  } }, // #121 [ref=4x]
  { InstDB::RWInfo::kCategoryGeneric   , 36, { 4 , 61, 7 , 0 , 0 , 0  } }, // #122 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 38, { 4 , 82, 9 , 0 , 0 , 0  } }, // #123 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 36, { 6 , 7 , 7 , 0 , 0 , 0  } }, // #124 [ref=11x]
  { InstDB::RWInfo::kCategoryGeneric   , 37, { 4 , 5 , 5 , 0 , 0 , 0  } }, // #125 [ref=6x]
  { InstDB::RWInfo::kCategoryGeneric   , 38, { 8 , 9 , 9 , 0 , 0 , 0  } }, // #126 [ref=11x]
  { InstDB::RWInfo::kCategoryGeneric   , 50, { 11, 3 , 3 , 3 , 0 , 0  } }, // #127 [ref=15x]
  { InstDB::RWInfo::kCategoryGeneric   , 51, { 35, 7 , 7 , 7 , 0 , 0  } }, // #128 [ref=4x]
  { InstDB::RWInfo::kCategoryGeneric   , 52, { 44, 9 , 9 , 9 , 0 , 0  } }, // #129 [ref=4x]
  { InstDB::RWInfo::kCategoryGeneric   , 37, { 4 , 5 , 13, 0 , 0 , 0  } }, // #130 [ref=6x]
  { InstDB::RWInfo::kCategoryGeneric   , 36, { 26, 7 , 7 , 0 , 0 , 0  } }, // #131 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 38, { 53, 9 , 9 , 0 , 0 , 0  } }, // #132 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 14, { 35, 3 , 0 , 0 , 0 , 0  } }, // #133 [ref=3x]
  { InstDB::RWInfo::kCategoryGeneric   , 27, { 35, 13, 0 , 0 , 0 , 0  } }, // #134 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 5 , { 35, 9 , 0 , 0 , 0 , 0  } }, // #135 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 8 , { 2 , 3 , 2 , 0 , 0 , 0  } }, // #136 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 2 , 3 , 2 , 0 , 0 , 0  } }, // #137 [ref=4x]
  { InstDB::RWInfo::kCategoryGeneric   , 18, { 4 , 3 , 4 , 0 , 0 , 0  } }, // #138 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 36, { 10, 61, 7 , 0 , 0 , 0  } }, // #139 [ref=11x]
  { InstDB::RWInfo::kCategoryGeneric   , 37, { 10, 83, 13, 0 , 0 , 0  } }, // #140 [ref=7x]
  { InstDB::RWInfo::kCategoryGeneric   , 38, { 10, 82, 9 , 0 , 0 , 0  } }, // #141 [ref=13x]
  { InstDB::RWInfo::kCategoryGeneric   , 46, { 80, 81, 5 , 0 , 0 , 0  } }, // #142 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 46, { 11, 3 , 5 , 0 , 0 , 0  } }, // #143 [ref=4x]
  { InstDB::RWInfo::kCategoryGeneric   , 53, { 74, 43, 81, 0 , 0 , 0  } }, // #144 [ref=4x]
  { InstDB::RWInfo::kCategoryVmaskmov  , 0 , { 0 , 0 , 0 , 0 , 0 , 0  } }, // #145 [ref=4x]
  { InstDB::RWInfo::kCategoryGeneric   , 12, { 35, 0 , 0 , 0 , 0 , 0  } }, // #146 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 22, 0 , 0 , 0 , 0 , 0  } }, // #147 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 10, 61, 61, 0 , 0 , 0  } }, // #148 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 12, { 10, 7 , 7 , 0 , 0 , 0  } }, // #149 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 10, 7 , 7 , 0 , 0 , 0  } }, // #150 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 12, { 10, 61, 7 , 0 , 0 , 0  } }, // #151 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 10, 61, 7 , 0 , 0 , 0  } }, // #152 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 10, 83, 13, 0 , 0 , 0  } }, // #153 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 10, 82, 9 , 0 , 0 , 0  } }, // #154 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 84, 0 , 0 , 0 , 0 , 0  } }, // #155 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 56, { 85, 86, 3 , 3 , 0 , 0  } }, // #156 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 13, { 74, 76, 77, 77, 77, 5  } }, // #157 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 53, { 80, 81, 81, 0 , 0 , 0  } }, // #158 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 22, { 11, 3 , 3 , 0 , 0 , 0  } }, // #159 [ref=4x]
  { InstDB::RWInfo::kCategoryGeneric   , 7 , { 48, 5 , 0 , 0 , 0 , 0  } }, // #160 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 57, { 10, 5 , 40, 0 , 0 , 0  } }, // #161 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 46, { 10, 5 , 5 , 5 , 0 , 0  } }, // #162 [ref=12x]
  { InstDB::RWInfo::kCategoryGeneric   , 61, { 10, 5 , 5 , 5 , 0 , 0  } }, // #163 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 62, { 10, 5 , 5 , 0 , 0 , 0  } }, // #164 [ref=12x]
  { InstDB::RWInfo::kCategoryGeneric   , 22, { 11, 3 , 5 , 0 , 0 , 0  } }, // #165 [ref=9x]
  { InstDB::RWInfo::kCategoryGeneric   , 63, { 11, 3 , 0 , 0 , 0 , 0  } }, // #166 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 60, 17, 29, 0 , 0 , 0  } }, // #167 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 8 , { 3 , 60, 17, 0 , 0 , 0  } }, // #168 [ref=4x]
  { InstDB::RWInfo::kCategoryGeneric   , 8 , { 11, 60, 17, 0 , 0 , 0  } }  // #169 [ref=8x]
};

const InstDB::RWInfoOp InstDB::rwInfoOp[] = {
  { 0x0000000000000000u, 0x0000000000000000u, 0xFF, 0, { 0 }, OpRWFlags::kNone }, // #0 [ref=16519x]
  { 0x0000000000000003u, 0x0000000000000003u, 0x00, 0, { 0 }, OpRWFlags::kRW | OpRWFlags::kRegPhysId }, // #1 [ref=10x]
  { 0x0000000000000000u, 0x0000000000000000u, 0xFF, 0, { 0 }, OpRWFlags::kRW | OpRWFlags::kZExt }, // #2 [ref=236x]
  { 0x0000000000000000u, 0x0000000000000000u, 0xFF, 0, { 0 }, OpRWFlags::kRead }, // #3 [ref=1077x]
  { 0x000000000000FFFFu, 0x000000000000FFFFu, 0xFF, 0, { 0 }, OpRWFlags::kRW | OpRWFlags::kZExt }, // #4 [ref=108x]
  { 0x000000000000FFFFu, 0x0000000000000000u, 0xFF, 0, { 0 }, OpRWFlags::kRead }, // #5 [ref=348x]
  { 0x00000000000000FFu, 0x00000000000000FFu, 0xFF, 0, { 0 }, OpRWFlags::kRW }, // #6 [ref=18x]
  { 0x00000000000000FFu, 0x0000000000000000u, 0xFF, 0, { 0 }, OpRWFlags::kRead }, // #7 [ref=186x]
  { 0x000000000000000Fu, 0x000000000000000Fu, 0xFF, 0, { 0 }, OpRWFlags::kRW }, // #8 [ref=18x]
  { 0x000000000000000Fu, 0x0000000000000000u, 0xFF, 0, { 0 }, OpRWFlags::kRead }, // #9 [ref=135x]
  { 0x0000000000000000u, 0x000000000000FFFFu, 0xFF, 0, { 0 }, OpRWFlags::kWrite | OpRWFlags::kZExt }, // #10 [ref=184x]
  { 0x0000000000000000u, 0x0000000000000000u, 0xFF, 0, { 0 }, OpRWFlags::kWrite | OpRWFlags::kZExt }, // #11 [ref=455x]
  { 0x0000000000000003u, 0x0000000000000003u, 0xFF, 0, { 0 }, OpRWFlags::kRW }, // #12 [ref=1x]
  { 0x0000000000000003u, 0x0000000000000000u, 0xFF, 0, { 0 }, OpRWFlags::kRead }, // #13 [ref=63x]
  { 0x000000000000FFFFu, 0x0000000000000000u, 0x00, 0, { 0 }, OpRWFlags::kRead | OpRWFlags::kRegPhysId }, // #14 [ref=4x]
  { 0x0000000000000000u, 0x0000000000000000u, 0xFF, 0, { 0 }, OpRWFlags::kWrite | OpRWFlags::kZExt | OpRWFlags::kMemBaseWrite | OpRWFlags::kMemIndexWrite }, // #15 [ref=1x]
  { 0x0000000000000000u, 0x000000000000000Fu, 0x02, 0, { 0 }, OpRWFlags::kWrite | OpRWFlags::kZExt | OpRWFlags::kRegPhysId }, // #16 [ref=9x]
  { 0x000000000000000Fu, 0x0000000000000000u, 0x00, 0, { 0 }, OpRWFlags::kRead | OpRWFlags::kRegPhysId }, // #17 [ref=23x]
  { 0x00000000000000FFu, 0x00000000000000FFu, 0x00, 0, { 0 }, OpRWFlags::kRW | OpRWFlags::kZExt | OpRWFlags::kRegPhysId }, // #18 [ref=2x]
  { 0x0000000000000000u, 0x0000000000000000u, 0x00, 0, { 0 }, OpRWFlags::kRead | OpRWFlags::kMemPhysId }, // #19 [ref=3x]
  { 0x0000000000000000u, 0x0000000000000000u, 0x06, 0, { 0 }, OpRWFlags::kRead | OpRWFlags::kMemBaseRW | OpRWFlags::kMemBasePostModify | OpRWFlags::kMemPhysId }, // #20 [ref=3x]
  { 0x0000000000000000u, 0x0000000000000000u, 0x07, 0, { 0 }, OpRWFlags::kRead | OpRWFlags::kMemBaseRW | OpRWFlags::kMemBasePostModify | OpRWFlags::kMemPhysId }, // #21 [ref=2x]
  { 0x0000000000000000u, 0x0000000000000000u, 0x00, 0, { 0 }, OpRWFlags::kRead | OpRWFlags::kRegPhysId }, // #22 [ref=7x]
  { 0x00000000000000FFu, 0x00000000000000FFu, 0x02, 0, { 0 }, OpRWFlags::kRW | OpRWFlags::kZExt | OpRWFlags::kRegPhysId }, // #23 [ref=1x]
  { 0x00000000000000FFu, 0x0000000000000000u, 0x01, 0, { 0 }, OpRWFlags::kRead | OpRWFlags::kRegPhysId }, // #24 [ref=1x]
  { 0x00000000000000FFu, 0x0000000000000000u, 0x03, 0, { 0 }, OpRWFlags::kRead | OpRWFlags::kRegPhysId }, // #25 [ref=1x]
  { 0x00000000000000FFu, 0x00000000000000FFu, 0xFF, 0, { 0 }, OpRWFlags::kRW | OpRWFlags::kZExt }, // #26 [ref=21x]
  { 0x000000000000000Fu, 0x000000000000000Fu, 0x02, 0, { 0 }, OpRWFlags::kRW | OpRWFlags::kZExt | OpRWFlags::kRegPhysId }, // #27 [ref=1x]
  { 0x000000000000000Fu, 0x000000000000000Fu, 0x00, 0, { 0 }, OpRWFlags::kRW | OpRWFlags::kZExt | OpRWFlags::kRegPhysId }, // #28 [ref=4x]
  { 0x000000000000000Fu, 0x0000000000000000u, 0x01, 0, { 0 }, OpRWFlags::kRead | OpRWFlags::kRegPhysId }, // #29 [ref=13x]
  { 0x000000000000000Fu, 0x0000000000000000u, 0x03, 0, { 0 }, OpRWFlags::kRead | OpRWFlags::kRegPhysId }, // #30 [ref=2x]
  { 0x0000000000000000u, 0x000000000000000Fu, 0x03, 0, { 0 }, OpRWFlags::kWrite | OpRWFlags::kZExt | OpRWFlags::kRegPhysId }, // #31 [ref=1x]
  { 0x000000000000000Fu, 0x000000000000000Fu, 0x01, 0, { 0 }, OpRWFlags::kRW | OpRWFlags::kZExt | OpRWFlags::kRegPhysId }, // #32 [ref=1x]
  { 0x0000000000000000u, 0x00000000000000FFu, 0x02, 0, { 0 }, OpRWFlags::kWrite | OpRWFlags::kZExt | OpRWFlags::kRegPhysId }, // #33 [ref=1x]
  { 0x00000000000000FFu, 0x0000000000000000u, 0x00, 0, { 0 }, OpRWFlags::kRead | OpRWFlags::kRegPhysId }, // #34 [ref=1x]
  { 0x0000000000000000u, 0x00000000000000FFu, 0xFF, 0, { 0 }, OpRWFlags::kWrite | OpRWFlags::kZExt }, // #35 [ref=82x]
  { 0x0000000000000000u, 0x00000000000000FFu, 0xFF, 0, { 0 }, OpRWFlags::kWrite }, // #36 [ref=6x]
  { 0x0000000000000000u, 0x000000000000000Fu, 0xFF, 0, { 0 }, OpRWFlags::kWrite }, // #37 [ref=6x]
  { 0x0000000000000000u, 0x0000000000000003u, 0x02, 0, { 0 }, OpRWFlags::kWrite | OpRWFlags::kRegPhysId }, // #38 [ref=1x]
  { 0x0000000000000003u, 0x0000000000000000u, 0x00, 0, { 0 }, OpRWFlags::kRead | OpRWFlags::kRegPhysId }, // #39 [ref=1x]
  { 0x0000000000000001u, 0x0000000000000000u, 0xFF, 0, { 0 }, OpRWFlags::kRead }, // #40 [ref=28x]
  { 0x0000000000000000u, 0x0000000000000000u, 0x02, 0, { 0 }, OpRWFlags::kRW | OpRWFlags::kRegPhysId | OpRWFlags::kZExt }, // #41 [ref=2x]
  { 0x0000000000000000u, 0x0000000000000000u, 0x00, 0, { 0 }, OpRWFlags::kRW | OpRWFlags::kRegPhysId | OpRWFlags::kZExt }, // #42 [ref=3x]
  { 0xFFFFFFFFFFFFFFFFu, 0x0000000000000000u, 0xFF, 0, { 0 }, OpRWFlags::kRead }, // #43 [ref=29x]
  { 0x0000000000000000u, 0x000000000000000Fu, 0xFF, 0, { 0 }, OpRWFlags::kWrite | OpRWFlags::kZExt }, // #44 [ref=30x]
  { 0x00000000000003FFu, 0x00000000000003FFu, 0xFF, 0, { 0 }, OpRWFlags::kRW | OpRWFlags::kZExt }, // #45 [ref=22x]
  { 0x00000000000003FFu, 0x0000000000000000u, 0xFF, 0, { 0 }, OpRWFlags::kRead }, // #46 [ref=13x]
  { 0x0000000000000000u, 0x00000000000003FFu, 0xFF, 0, { 0 }, OpRWFlags::kWrite | OpRWFlags::kZExt }, // #47 [ref=1x]
  { 0x0000000000000000u, 0x0000000000000003u, 0xFF, 0, { 0 }, OpRWFlags::kWrite | OpRWFlags::kZExt }, // #48 [ref=17x]
  { 0x0000000000000000u, 0x0000000000000003u, 0x00, 0, { 0 }, OpRWFlags::kWrite | OpRWFlags::kRegPhysId | OpRWFlags::kZExt }, // #49 [ref=2x]
  { 0x0000000000000000u, 0x000000000000000Fu, 0x00, 0, { 0 }, OpRWFlags::kWrite | OpRWFlags::kZExt | OpRWFlags::kRegPhysId }, // #50 [ref=8x]
  { 0x0000000000000000u, 0x0000000000000000u, 0x00, 0, { 0 }, OpRWFlags::kWrite | OpRWFlags::kRegPhysId | OpRWFlags::kZExt }, // #51 [ref=2x]
  { 0x0000000000000003u, 0x0000000000000000u, 0x02, 0, { 0 }, OpRWFlags::kRead | OpRWFlags::kRegPhysId }, // #52 [ref=4x]
  { 0x000000000000000Fu, 0x000000000000000Fu, 0xFF, 0, { 0 }, OpRWFlags::kRW | OpRWFlags::kZExt }, // #53 [ref=4x]
  { 0x0000000000000000u, 0x0000000000000000u, 0x07, 0, { 0 }, OpRWFlags::kWrite | OpRWFlags::kZExt | OpRWFlags::kMemPhysId }, // #54 [ref=1x]
  { 0x0000000000000000u, 0x0000000000000000u, 0x01, 0, { 0 }, OpRWFlags::kRead | OpRWFlags::kRegPhysId }, // #55 [ref=1x]
  { 0x0000000000000000u, 0x0000000000000001u, 0xFF, 0, { 0 }, OpRWFlags::kWrite | OpRWFlags::kZExt }, // #56 [ref=14x]
  { 0x0000000000000000u, 0x0000000000000001u, 0x00, 0, { 0 }, OpRWFlags::kWrite | OpRWFlags::kRegPhysId }, // #57 [ref=1x]
  { 0x0000000000000000u, 0x0000000000000000u, 0x01, 0, { 0 }, OpRWFlags::kRW | OpRWFlags::kRegPhysId | OpRWFlags::kZExt }, // #58 [ref=3x]
  { 0x0000000000000000u, 0x0000000000000000u, 0x07, 0, { 0 }, OpRWFlags::kRW | OpRWFlags::kZExt | OpRWFlags::kMemPhysId }, // #59 [ref=3x]
  { 0x000000000000000Fu, 0x0000000000000000u, 0x02, 0, { 0 }, OpRWFlags::kRead | OpRWFlags::kRegPhysId }, // #60 [ref=22x]
  { 0x000000000000FF00u, 0x0000000000000000u, 0xFF, 0, { 0 }, OpRWFlags::kRead }, // #61 [ref=23x]
  { 0x0000000000000000u, 0x000000000000FF00u, 0xFF, 0, { 0 }, OpRWFlags::kWrite }, // #62 [ref=1x]
  { 0x0000000000000000u, 0x0000000000000000u, 0x07, 0, { 0 }, OpRWFlags::kWrite | OpRWFlags::kZExt | OpRWFlags::kMemBaseRW | OpRWFlags::kMemBasePostModify | OpRWFlags::kMemPhysId }, // #63 [ref=2x]
  { 0x0000000000000000u, 0x0000000000000000u, 0x02, 0, { 0 }, OpRWFlags::kWrite | OpRWFlags::kRegPhysId | OpRWFlags::kZExt }, // #64 [ref=1x]
  { 0x0000000000000000u, 0x0000000000000000u, 0x02, 0, { 0 }, OpRWFlags::kRead | OpRWFlags::kRegPhysId }, // #65 [ref=2x]
  { 0x0000000000000000u, 0x0000000000000000u, 0x06, 0, { 0 }, OpRWFlags::kRead | OpRWFlags::kMemPhysId }, // #66 [ref=1x]
  { 0x0000000000000000u, 0x000000000000000Fu, 0x01, 0, { 0 }, OpRWFlags::kWrite | OpRWFlags::kZExt | OpRWFlags::kRegPhysId }, // #67 [ref=5x]
  { 0x0000000000000000u, 0x000000000000FFFFu, 0x00, 0, { 0 }, OpRWFlags::kWrite | OpRWFlags::kZExt | OpRWFlags::kRegPhysId }, // #68 [ref=4x]
  { 0x0000000000000000u, 0x0000000000000007u, 0xFF, 0, { 0 }, OpRWFlags::kWrite | OpRWFlags::kZExt }, // #69 [ref=2x]
  { 0x0000000000000000u, 0x0000000000000000u, 0x04, 0, { 0 }, OpRWFlags::kWrite | OpRWFlags::kZExt | OpRWFlags::kRegPhysId }, // #70 [ref=1x]
  { 0x0000000000000001u, 0x0000000000000000u, 0x01, 0, { 0 }, OpRWFlags::kRead | OpRWFlags::kRegPhysId }, // #71 [ref=10x]
  { 0x0000000000000001u, 0x0000000000000000u, 0x00, 0, { 0 }, OpRWFlags::kRead | OpRWFlags::kRegPhysId }, // #72 [ref=1x]
  { 0x0000000000000000u, 0x0000000000000001u, 0xFF, 0, { 0 }, OpRWFlags::kWrite }, // #73 [ref=30x]
  { 0x0000000000000000u, 0xFFFFFFFFFFFFFFFFu, 0xFF, 0, { 0 }, OpRWFlags::kWrite | OpRWFlags::kZExt }, // #74 [ref=20x]
  { 0xFFFFFFFFFFFFFFFFu, 0xFFFFFFFFFFFFFFFFu, 0xFF, 0, { 0 }, OpRWFlags::kRW | OpRWFlags::kZExt }, // #75 [ref=7x]
  { 0xFFFFFFFFFFFFFFFFu, 0x0000000000000000u, 0xFF, 4, { 0 }, OpRWFlags::kRead }, // #76 [ref=4x]
  { 0xFFFFFFFFFFFFFFFFu, 0x0000000000000000u, 0xFF, 0, { 0 }, OpRWFlags::kRead | OpRWFlags::kConsecutive }, // #77 [ref=12x]
  { 0x000000000000FFFFu, 0x0000000000000000u, 0xFF, 4, { 0 }, OpRWFlags::kRead }, // #78 [ref=2x]
  { 0x000000000000FFFFu, 0x0000000000000000u, 0xFF, 0, { 0 }, OpRWFlags::kRead | OpRWFlags::kConsecutive }, // #79 [ref=6x]
  { 0x0000000000000000u, 0x00000000FFFFFFFFu, 0xFF, 0, { 0 }, OpRWFlags::kWrite | OpRWFlags::kZExt }, // #80 [ref=10x]
  { 0x00000000FFFFFFFFu, 0x0000000000000000u, 0xFF, 0, { 0 }, OpRWFlags::kRead }, // #81 [ref=16x]
  { 0x000000000000FFF0u, 0x0000000000000000u, 0xFF, 0, { 0 }, OpRWFlags::kRead }, // #82 [ref=18x]
  { 0x000000000000FFFCu, 0x0000000000000000u, 0xFF, 0, { 0 }, OpRWFlags::kRead }, // #83 [ref=8x]
  { 0x0000000000000000u, 0x0000000000000000u, 0x00, 0, { 0 }, OpRWFlags::kRW | OpRWFlags::kZExt | OpRWFlags::kRegPhysId }, // #84 [ref=1x]
  { 0x0000000000000000u, 0x00000000000000FFu, 0xFF, 2, { 0 }, OpRWFlags::kWrite | OpRWFlags::kZExt }, // #85 [ref=2x]
  { 0x0000000000000000u, 0x0000000000000000u, 0xFF, 0, { 0 }, OpRWFlags::kWrite | OpRWFlags::kZExt | OpRWFlags::kConsecutive }  // #86 [ref=2x]
};

const InstDB::RWInfoRm InstDB::rwInfoRm[] = {
  { InstDB::RWInfoRm::kCategoryNone      , 0x00, 0 , 0, 0 }, // #0 [ref=2000x]
  { InstDB::RWInfoRm::kCategoryConsistent, 0x03, 0 , InstDB::RWInfoRm::kFlagAmbiguous, 0 }, // #1 [ref=8x]
  { InstDB::RWInfoRm::kCategoryConsistent, 0x02, 0 , 0, 0 }, // #2 [ref=204x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x02, 16, 0, 0 }, // #3 [ref=122x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x02, 8 , 0, 0 }, // #4 [ref=66x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x02, 4 , 0, 0 }, // #5 [ref=35x]
  { InstDB::RWInfoRm::kCategoryConsistent, 0x04, 0 , 0, 0 }, // #6 [ref=300x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x01, 2 , 0, 0 }, // #7 [ref=9x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x00, 0 , 0, 0 }, // #8 [ref=63x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x03, 0 , 0, 0 }, // #9 [ref=1x]
  { InstDB::RWInfoRm::kCategoryConsistent, 0x01, 0 , InstDB::RWInfoRm::kFlagAmbiguous, 0 }, // #10 [ref=21x]
  { InstDB::RWInfoRm::kCategoryConsistent, 0x01, 0 , 0, 0 }, // #11 [ref=14x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x00, 8 , 0, 0 }, // #12 [ref=22x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x00, 16, 0, 0 }, // #13 [ref=21x]
  { InstDB::RWInfoRm::kCategoryConsistent, 0x02, 0 , InstDB::RWInfoRm::kFlagAmbiguous, 0 }, // #14 [ref=22x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x02, 1 , 0, 0 }, // #15 [ref=5x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x00, 64, 0, 0 }, // #16 [ref=5x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x01, 4 , 0, 0 }, // #17 [ref=6x]
  { InstDB::RWInfoRm::kCategoryNone      , 0x00, 0 , InstDB::RWInfoRm::kFlagAmbiguous, 0 }, // #18 [ref=26x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x00, 10, 0, 0 }, // #19 [ref=2x]
  { InstDB::RWInfoRm::kCategoryNone      , 0x01, 0 , InstDB::RWInfoRm::kFlagAmbiguous, 0 }, // #20 [ref=5x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x00, 2 , 0, 0 }, // #21 [ref=4x]
  { InstDB::RWInfoRm::kCategoryConsistent, 0x06, 0 , 0, 0 }, // #22 [ref=14x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x03, 1 , 0, 0 }, // #23 [ref=1x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x03, 4 , 0, 0 }, // #24 [ref=4x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x03, 8 , 0, 0 }, // #25 [ref=3x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x03, 2 , 0, 0 }, // #26 [ref=2x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x02, 2 , 0, 0 }, // #27 [ref=13x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x00, 4 , 0, 0 }, // #28 [ref=6x]
  { InstDB::RWInfoRm::kCategoryNone      , 0x03, 0 , InstDB::RWInfoRm::kFlagAmbiguous, 0 }, // #29 [ref=1x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x03, 16, 0, 0 }, // #30 [ref=6x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x01, 1 , 0, 0 }, // #31 [ref=32x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x01, 8 , 0, 0 }, // #32 [ref=4x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x01, 2 , 0, uint32_t(CpuFeatures::X86::kSSE4_1) }, // #33 [ref=1x]
  { InstDB::RWInfoRm::kCategoryNone      , 0x02, 0 , 0, 0 }, // #34 [ref=4x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x01, 2 , InstDB::RWInfoRm::kFlagAmbiguous, 0 }, // #35 [ref=3x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x04, 8 , 0, 0 }, // #36 [ref=35x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x04, 2 , 0, 0 }, // #37 [ref=30x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x04, 4 , 0, 0 }, // #38 [ref=42x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x00, 32, 0, 0 }, // #39 [ref=4x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x02, 8 , InstDB::RWInfoRm::kFlagAmbiguous, 0 }, // #40 [ref=1x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x02, 4 , InstDB::RWInfoRm::kFlagAmbiguous, 0 }, // #41 [ref=1x]
  { InstDB::RWInfoRm::kCategoryHalf      , 0x02, 0 , 0, 0 }, // #42 [ref=19x]
  { InstDB::RWInfoRm::kCategoryQuarter   , 0x02, 0 , 0, 0 }, // #43 [ref=9x]
  { InstDB::RWInfoRm::kCategoryHalf      , 0x01, 0 , 0, 0 }, // #44 [ref=10x]
  { InstDB::RWInfoRm::kCategoryConsistent, 0x04, 0 , InstDB::RWInfoRm::kFlagAmbiguous, 0 }, // #45 [ref=6x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x04, 16, 0, 0 }, // #46 [ref=27x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x02, 64, 0, 0 }, // #47 [ref=6x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x01, 16, 0, 0 }, // #48 [ref=6x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x01, 32, 0, 0 }, // #49 [ref=4x]
  { InstDB::RWInfoRm::kCategoryConsistent, 0x0C, 0 , 0, 0 }, // #50 [ref=15x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x0C, 8 , 0, 0 }, // #51 [ref=4x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x0C, 4 , 0, 0 }, // #52 [ref=4x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x04, 32, 0, 0 }, // #53 [ref=6x]
  { InstDB::RWInfoRm::kCategoryConsistent, 0x03, 0 , 0, 0 }, // #54 [ref=13x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x03, 8 , InstDB::RWInfoRm::kFlagAmbiguous, 0 }, // #55 [ref=1x]
  { InstDB::RWInfoRm::kCategoryConsistent, 0x08, 0 , 0, 0 }, // #56 [ref=2x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x04, 1 , 0, 0 }, // #57 [ref=1x]
  { InstDB::RWInfoRm::kCategoryQuarter   , 0x01, 0 , 0, 0 }, // #58 [ref=6x]
  { InstDB::RWInfoRm::kCategoryEighth    , 0x01, 0 , 0, 0 }, // #59 [ref=3x]
  { InstDB::RWInfoRm::kCategoryEighth    , 0x02, 0 , 0, 0 }, // #60 [ref=2x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x0C, 16, 0, 0 }, // #61 [ref=1x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x06, 16, 0, 0 }, // #62 [ref=12x]
  { InstDB::RWInfoRm::kCategoryConsistent, 0x02, 0 , 0, uint32_t(CpuFeatures::X86::kAVX512_BW) }  // #63 [ref=2x]
};
// ----------------------------------------------------------------------------
// ${InstRWInfoTable:End}

// x86::InstDB - Tests
// ===================

#if defined(ASMJIT_TEST)
UNIT(x86_inst_db) {
  INFO("Checking validity of Inst enums");

  // Cross-validate prefixes.
  EXPECT(uint32_t(InstOptions::kX86_Rex ) == 0x40000000u, "REX prefix must be at 0x40000000");
  EXPECT(uint32_t(InstOptions::kX86_Evex) == 0x00001000u, "EVEX prefix must be at 0x00001000");

  // These could be combined together to form a valid REX prefix, they must match.
  EXPECT(uint32_t(InstOptions::kX86_OpCodeB) == uint32_t(Opcode::kB), "Opcode::kB must match InstOptions::kX86_OpCodeB");
  EXPECT(uint32_t(InstOptions::kX86_OpCodeX) == uint32_t(Opcode::kX), "Opcode::kX must match InstOptions::kX86_OpCodeX");
  EXPECT(uint32_t(InstOptions::kX86_OpCodeR) == uint32_t(Opcode::kR), "Opcode::kR must match InstOptions::kX86_OpCodeR");
  EXPECT(uint32_t(InstOptions::kX86_OpCodeW) == uint32_t(Opcode::kW), "Opcode::kW must match InstOptions::kX86_OpCodeW");

  uint32_t rex_rb = (Opcode::kR >> Opcode::kREX_Shift) | (Opcode::kB >> Opcode::kREX_Shift) | 0x40;
  uint32_t rex_rw = (Opcode::kR >> Opcode::kREX_Shift) | (Opcode::kW >> Opcode::kREX_Shift) | 0x40;

  EXPECT(rex_rb == 0x45, "Opcode::kR|B must form a valid REX prefix (0x45) if combined with 0x40");
  EXPECT(rex_rw == 0x4C, "Opcode::kR|W must form a valid REX prefix (0x4C) if combined with 0x40");
}
#endif

ASMJIT_END_SUB_NAMESPACE

#endif // !ASMJIT_NO_X86
