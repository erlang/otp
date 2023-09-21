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

// Defines an X86 instruction.
#define INST(id, encoding, opcode0, opcode1, mainOpcodeIndex, altOpcodeIndex, commomInfoIndex, additionalInfoIndex) { \
  uint32_t(0),                              \
  uint32_t(commomInfoIndex),                \
  uint32_t(additionalInfoIndex),            \
  uint8_t(InstDB::kEncoding##encoding),     \
  uint8_t((opcode0) & 0xFFu),               \
  uint8_t(mainOpcodeIndex),                 \
  uint8_t(altOpcodeIndex)                   \
}

const InstDB::InstInfo InstDB::_instInfoTable[] = {
  /*--------------------+--------------------+------------------+--------+------------------+--------+----+----+----+----+
  |    Instruction      |    Instruction     |    Main Opcode   |  EVEX  |Alternative Opcode|  EVEX  |Op0X|Op1X|IdxA|IdxB|
  |     Id & Name       |      Encoding      |  (pp+mmm|op/o|L|w|W|N|TT.)|--(pp+mmm|op/o|L|w|W|N|TT.)| (auto-generated)  |
  +---------------------+--------------------+---------+----+-+-+-+-+----+---------+----+-+-+-+-+----+----+----+----+---*/
  // ${InstInfo:Begin}
  INST(None             , None               , 0                         , 0                         , 0  , 0  , 0  , 0  ), // #0
  INST(Aaa              , X86Op_xAX          , O(000000,37,_,_,_,_,_,_  ), 0                         , 0  , 0  , 1  , 1  ), // #1
  INST(Aad              , X86I_xAX           , O(000000,D5,_,_,_,_,_,_  ), 0                         , 0  , 0  , 2  , 1  ), // #2
  INST(Aam              , X86I_xAX           , O(000000,D4,_,_,_,_,_,_  ), 0                         , 0  , 0  , 2  , 1  ), // #3
  INST(Aas              , X86Op_xAX          , O(000000,3F,_,_,_,_,_,_  ), 0                         , 0  , 0  , 1  , 1  ), // #4
  INST(Adc              , X86Arith           , O(000000,10,2,_,x,_,_,_  ), 0                         , 1  , 0  , 3  , 2  ), // #5
  INST(Adcx             , X86Rm              , O(660F38,F6,_,_,x,_,_,_  ), 0                         , 2  , 0  , 4  , 3  ), // #6
  INST(Add              , X86Arith           , O(000000,00,0,_,x,_,_,_  ), 0                         , 0  , 0  , 3  , 1  ), // #7
  INST(Addpd            , ExtRm              , O(660F00,58,_,_,_,_,_,_  ), 0                         , 3  , 0  , 5  , 4  ), // #8
  INST(Addps            , ExtRm              , O(000F00,58,_,_,_,_,_,_  ), 0                         , 4  , 0  , 5  , 5  ), // #9
  INST(Addsd            , ExtRm              , O(F20F00,58,_,_,_,_,_,_  ), 0                         , 5  , 0  , 6  , 4  ), // #10
  INST(Addss            , ExtRm              , O(F30F00,58,_,_,_,_,_,_  ), 0                         , 6  , 0  , 7  , 5  ), // #11
  INST(Addsubpd         , ExtRm              , O(660F00,D0,_,_,_,_,_,_  ), 0                         , 3  , 0  , 5  , 6  ), // #12
  INST(Addsubps         , ExtRm              , O(F20F00,D0,_,_,_,_,_,_  ), 0                         , 5  , 0  , 5  , 6  ), // #13
  INST(Adox             , X86Rm              , O(F30F38,F6,_,_,x,_,_,_  ), 0                         , 7  , 0  , 4  , 7  ), // #14
  INST(Aesdec           , ExtRm              , O(660F38,DE,_,_,_,_,_,_  ), 0                         , 2  , 0  , 5  , 8  ), // #15
  INST(Aesdeclast       , ExtRm              , O(660F38,DF,_,_,_,_,_,_  ), 0                         , 2  , 0  , 5  , 8  ), // #16
  INST(Aesenc           , ExtRm              , O(660F38,DC,_,_,_,_,_,_  ), 0                         , 2  , 0  , 5  , 8  ), // #17
  INST(Aesenclast       , ExtRm              , O(660F38,DD,_,_,_,_,_,_  ), 0                         , 2  , 0  , 5  , 8  ), // #18
  INST(Aesimc           , ExtRm              , O(660F38,DB,_,_,_,_,_,_  ), 0                         , 2  , 0  , 5  , 8  ), // #19
  INST(Aeskeygenassist  , ExtRmi             , O(660F3A,DF,_,_,_,_,_,_  ), 0                         , 8  , 0  , 8  , 8  ), // #20
  INST(And              , X86Arith           , O(000000,20,4,_,x,_,_,_  ), 0                         , 9  , 0  , 9  , 1  ), // #21
  INST(Andn             , VexRvm_Wx          , V(000F38,F2,_,0,x,_,_,_  ), 0                         , 10 , 0  , 10 , 9  ), // #22
  INST(Andnpd           , ExtRm              , O(660F00,55,_,_,_,_,_,_  ), 0                         , 3  , 0  , 5  , 4  ), // #23
  INST(Andnps           , ExtRm              , O(000F00,55,_,_,_,_,_,_  ), 0                         , 4  , 0  , 5  , 5  ), // #24
  INST(Andpd            , ExtRm              , O(660F00,54,_,_,_,_,_,_  ), 0                         , 3  , 0  , 11 , 4  ), // #25
  INST(Andps            , ExtRm              , O(000F00,54,_,_,_,_,_,_  ), 0                         , 4  , 0  , 11 , 5  ), // #26
  INST(Arpl             , X86Mr_NoSize       , O(000000,63,_,_,_,_,_,_  ), 0                         , 0  , 0  , 12 , 10 ), // #27
  INST(Bextr            , VexRmv_Wx          , V(000F38,F7,_,0,x,_,_,_  ), 0                         , 10 , 0  , 13 , 9  ), // #28
  INST(Blcfill          , VexVm_Wx           , V(XOP_M9,01,1,0,x,_,_,_  ), 0                         , 11 , 0  , 14 , 11 ), // #29
  INST(Blci             , VexVm_Wx           , V(XOP_M9,02,6,0,x,_,_,_  ), 0                         , 12 , 0  , 14 , 11 ), // #30
  INST(Blcic            , VexVm_Wx           , V(XOP_M9,01,5,0,x,_,_,_  ), 0                         , 13 , 0  , 14 , 11 ), // #31
  INST(Blcmsk           , VexVm_Wx           , V(XOP_M9,02,1,0,x,_,_,_  ), 0                         , 11 , 0  , 14 , 11 ), // #32
  INST(Blcs             , VexVm_Wx           , V(XOP_M9,01,3,0,x,_,_,_  ), 0                         , 14 , 0  , 14 , 11 ), // #33
  INST(Blendpd          , ExtRmi             , O(660F3A,0D,_,_,_,_,_,_  ), 0                         , 8  , 0  , 8  , 12 ), // #34
  INST(Blendps          , ExtRmi             , O(660F3A,0C,_,_,_,_,_,_  ), 0                         , 8  , 0  , 8  , 12 ), // #35
  INST(Blendvpd         , ExtRm_XMM0         , O(660F38,15,_,_,_,_,_,_  ), 0                         , 2  , 0  , 15 , 12 ), // #36
  INST(Blendvps         , ExtRm_XMM0         , O(660F38,14,_,_,_,_,_,_  ), 0                         , 2  , 0  , 15 , 12 ), // #37
  INST(Blsfill          , VexVm_Wx           , V(XOP_M9,01,2,0,x,_,_,_  ), 0                         , 15 , 0  , 14 , 11 ), // #38
  INST(Blsi             , VexVm_Wx           , V(000F38,F3,3,0,x,_,_,_  ), 0                         , 16 , 0  , 14 , 9  ), // #39
  INST(Blsic            , VexVm_Wx           , V(XOP_M9,01,6,0,x,_,_,_  ), 0                         , 12 , 0  , 14 , 11 ), // #40
  INST(Blsmsk           , VexVm_Wx           , V(000F38,F3,2,0,x,_,_,_  ), 0                         , 17 , 0  , 14 , 9  ), // #41
  INST(Blsr             , VexVm_Wx           , V(000F38,F3,1,0,x,_,_,_  ), 0                         , 18 , 0  , 14 , 9  ), // #42
  INST(Bndcl            , X86Rm              , O(F30F00,1A,_,_,_,_,_,_  ), 0                         , 6  , 0  , 16 , 13 ), // #43
  INST(Bndcn            , X86Rm              , O(F20F00,1B,_,_,_,_,_,_  ), 0                         , 5  , 0  , 16 , 13 ), // #44
  INST(Bndcu            , X86Rm              , O(F20F00,1A,_,_,_,_,_,_  ), 0                         , 5  , 0  , 16 , 13 ), // #45
  INST(Bndldx           , X86Rm              , O(000F00,1A,_,_,_,_,_,_  ), 0                         , 4  , 0  , 17 , 13 ), // #46
  INST(Bndmk            , X86Rm              , O(F30F00,1B,_,_,_,_,_,_  ), 0                         , 6  , 0  , 18 , 13 ), // #47
  INST(Bndmov           , X86Bndmov          , O(660F00,1A,_,_,_,_,_,_  ), O(660F00,1B,_,_,_,_,_,_  ), 3  , 1  , 19 , 13 ), // #48
  INST(Bndstx           , X86Mr              , O(000F00,1B,_,_,_,_,_,_  ), 0                         , 4  , 0  , 20 , 13 ), // #49
  INST(Bound            , X86Rm              , O(000000,62,_,_,_,_,_,_  ), 0                         , 0  , 0  , 21 , 0  ), // #50
  INST(Bsf              , X86Rm              , O(000F00,BC,_,_,x,_,_,_  ), 0                         , 4  , 0  , 22 , 1  ), // #51
  INST(Bsr              , X86Rm              , O(000F00,BD,_,_,x,_,_,_  ), 0                         , 4  , 0  , 22 , 1  ), // #52
  INST(Bswap            , X86Bswap           , O(000F00,C8,_,_,x,_,_,_  ), 0                         , 4  , 0  , 23 , 0  ), // #53
  INST(Bt               , X86Bt              , O(000F00,A3,_,_,x,_,_,_  ), O(000F00,BA,4,_,x,_,_,_  ), 4  , 2  , 24 , 14 ), // #54
  INST(Btc              , X86Bt              , O(000F00,BB,_,_,x,_,_,_  ), O(000F00,BA,7,_,x,_,_,_  ), 4  , 3  , 25 , 14 ), // #55
  INST(Btr              , X86Bt              , O(000F00,B3,_,_,x,_,_,_  ), O(000F00,BA,6,_,x,_,_,_  ), 4  , 4  , 25 , 14 ), // #56
  INST(Bts              , X86Bt              , O(000F00,AB,_,_,x,_,_,_  ), O(000F00,BA,5,_,x,_,_,_  ), 4  , 5  , 25 , 14 ), // #57
  INST(Bzhi             , VexRmv_Wx          , V(000F38,F5,_,0,x,_,_,_  ), 0                         , 10 , 0  , 13 , 15 ), // #58
  INST(Call             , X86Call            , O(000000,FF,2,_,_,_,_,_  ), 0                         , 1  , 0  , 26 , 1  ), // #59
  INST(Cbw              , X86Op_xAX          , O(660000,98,_,_,_,_,_,_  ), 0                         , 19 , 0  , 27 , 0  ), // #60
  INST(Cdq              , X86Op_xDX_xAX      , O(000000,99,_,_,_,_,_,_  ), 0                         , 0  , 0  , 28 , 0  ), // #61
  INST(Cdqe             , X86Op_xAX          , O(000000,98,_,_,1,_,_,_  ), 0                         , 20 , 0  , 29 , 0  ), // #62
  INST(Clac             , X86Op              , O(000F01,CA,_,_,_,_,_,_  ), 0                         , 21 , 0  , 30 , 16 ), // #63
  INST(Clc              , X86Op              , O(000000,F8,_,_,_,_,_,_  ), 0                         , 0  , 0  , 30 , 17 ), // #64
  INST(Cld              , X86Op              , O(000000,FC,_,_,_,_,_,_  ), 0                         , 0  , 0  , 30 , 18 ), // #65
  INST(Cldemote         , X86M_Only          , O(000F00,1C,0,_,_,_,_,_  ), 0                         , 4  , 0  , 31 , 19 ), // #66
  INST(Clflush          , X86M_Only          , O(000F00,AE,7,_,_,_,_,_  ), 0                         , 22 , 0  , 31 , 20 ), // #67
  INST(Clflushopt       , X86M_Only          , O(660F00,AE,7,_,_,_,_,_  ), 0                         , 23 , 0  , 31 , 21 ), // #68
  INST(Clgi             , X86Op              , O(000F01,DD,_,_,_,_,_,_  ), 0                         , 21 , 0  , 30 , 22 ), // #69
  INST(Cli              , X86Op              , O(000000,FA,_,_,_,_,_,_  ), 0                         , 0  , 0  , 30 , 23 ), // #70
  INST(Clrssbsy         , X86M_Only          , O(F30F00,AE,6,_,_,_,_,_  ), 0                         , 24 , 0  , 32 , 24 ), // #71
  INST(Clts             , X86Op              , O(000F00,06,_,_,_,_,_,_  ), 0                         , 4  , 0  , 30 , 0  ), // #72
  INST(Clui             , X86Op              , O(F30F01,EE,_,_,_,_,_,_  ), 0                         , 25 , 0  , 33 , 25 ), // #73
  INST(Clwb             , X86M_Only          , O(660F00,AE,6,_,_,_,_,_  ), 0                         , 26 , 0  , 31 , 26 ), // #74
  INST(Clzero           , X86Op_MemZAX       , O(000F01,FC,_,_,_,_,_,_  ), 0                         , 21 , 0  , 34 , 27 ), // #75
  INST(Cmc              , X86Op              , O(000000,F5,_,_,_,_,_,_  ), 0                         , 0  , 0  , 30 , 28 ), // #76
  INST(Cmova            , X86Rm              , O(000F00,47,_,_,x,_,_,_  ), 0                         , 4  , 0  , 22 , 29 ), // #77
  INST(Cmovae           , X86Rm              , O(000F00,43,_,_,x,_,_,_  ), 0                         , 4  , 0  , 22 , 30 ), // #78
  INST(Cmovb            , X86Rm              , O(000F00,42,_,_,x,_,_,_  ), 0                         , 4  , 0  , 22 , 30 ), // #79
  INST(Cmovbe           , X86Rm              , O(000F00,46,_,_,x,_,_,_  ), 0                         , 4  , 0  , 22 , 29 ), // #80
  INST(Cmovc            , X86Rm              , O(000F00,42,_,_,x,_,_,_  ), 0                         , 4  , 0  , 22 , 30 ), // #81
  INST(Cmove            , X86Rm              , O(000F00,44,_,_,x,_,_,_  ), 0                         , 4  , 0  , 22 , 31 ), // #82
  INST(Cmovg            , X86Rm              , O(000F00,4F,_,_,x,_,_,_  ), 0                         , 4  , 0  , 22 , 32 ), // #83
  INST(Cmovge           , X86Rm              , O(000F00,4D,_,_,x,_,_,_  ), 0                         , 4  , 0  , 22 , 33 ), // #84
  INST(Cmovl            , X86Rm              , O(000F00,4C,_,_,x,_,_,_  ), 0                         , 4  , 0  , 22 , 33 ), // #85
  INST(Cmovle           , X86Rm              , O(000F00,4E,_,_,x,_,_,_  ), 0                         , 4  , 0  , 22 , 32 ), // #86
  INST(Cmovna           , X86Rm              , O(000F00,46,_,_,x,_,_,_  ), 0                         , 4  , 0  , 22 , 29 ), // #87
  INST(Cmovnae          , X86Rm              , O(000F00,42,_,_,x,_,_,_  ), 0                         , 4  , 0  , 22 , 30 ), // #88
  INST(Cmovnb           , X86Rm              , O(000F00,43,_,_,x,_,_,_  ), 0                         , 4  , 0  , 22 , 30 ), // #89
  INST(Cmovnbe          , X86Rm              , O(000F00,47,_,_,x,_,_,_  ), 0                         , 4  , 0  , 22 , 29 ), // #90
  INST(Cmovnc           , X86Rm              , O(000F00,43,_,_,x,_,_,_  ), 0                         , 4  , 0  , 22 , 30 ), // #91
  INST(Cmovne           , X86Rm              , O(000F00,45,_,_,x,_,_,_  ), 0                         , 4  , 0  , 22 , 31 ), // #92
  INST(Cmovng           , X86Rm              , O(000F00,4E,_,_,x,_,_,_  ), 0                         , 4  , 0  , 22 , 32 ), // #93
  INST(Cmovnge          , X86Rm              , O(000F00,4C,_,_,x,_,_,_  ), 0                         , 4  , 0  , 22 , 33 ), // #94
  INST(Cmovnl           , X86Rm              , O(000F00,4D,_,_,x,_,_,_  ), 0                         , 4  , 0  , 22 , 33 ), // #95
  INST(Cmovnle          , X86Rm              , O(000F00,4F,_,_,x,_,_,_  ), 0                         , 4  , 0  , 22 , 32 ), // #96
  INST(Cmovno           , X86Rm              , O(000F00,41,_,_,x,_,_,_  ), 0                         , 4  , 0  , 22 , 34 ), // #97
  INST(Cmovnp           , X86Rm              , O(000F00,4B,_,_,x,_,_,_  ), 0                         , 4  , 0  , 22 , 35 ), // #98
  INST(Cmovns           , X86Rm              , O(000F00,49,_,_,x,_,_,_  ), 0                         , 4  , 0  , 22 , 36 ), // #99
  INST(Cmovnz           , X86Rm              , O(000F00,45,_,_,x,_,_,_  ), 0                         , 4  , 0  , 22 , 31 ), // #100
  INST(Cmovo            , X86Rm              , O(000F00,40,_,_,x,_,_,_  ), 0                         , 4  , 0  , 22 , 34 ), // #101
  INST(Cmovp            , X86Rm              , O(000F00,4A,_,_,x,_,_,_  ), 0                         , 4  , 0  , 22 , 35 ), // #102
  INST(Cmovpe           , X86Rm              , O(000F00,4A,_,_,x,_,_,_  ), 0                         , 4  , 0  , 22 , 35 ), // #103
  INST(Cmovpo           , X86Rm              , O(000F00,4B,_,_,x,_,_,_  ), 0                         , 4  , 0  , 22 , 35 ), // #104
  INST(Cmovs            , X86Rm              , O(000F00,48,_,_,x,_,_,_  ), 0                         , 4  , 0  , 22 , 36 ), // #105
  INST(Cmovz            , X86Rm              , O(000F00,44,_,_,x,_,_,_  ), 0                         , 4  , 0  , 22 , 31 ), // #106
  INST(Cmp              , X86Arith           , O(000000,38,7,_,x,_,_,_  ), 0                         , 27 , 0  , 35 , 1  ), // #107
  INST(Cmppd            , ExtRmi             , O(660F00,C2,_,_,_,_,_,_  ), 0                         , 3  , 0  , 8  , 4  ), // #108
  INST(Cmpps            , ExtRmi             , O(000F00,C2,_,_,_,_,_,_  ), 0                         , 4  , 0  , 8  , 5  ), // #109
  INST(Cmps             , X86StrMm           , O(000000,A6,_,_,_,_,_,_  ), 0                         , 0  , 0  , 36 , 37 ), // #110
  INST(Cmpsd            , ExtRmi             , O(F20F00,C2,_,_,_,_,_,_  ), 0                         , 5  , 0  , 37 , 4  ), // #111
  INST(Cmpss            , ExtRmi             , O(F30F00,C2,_,_,_,_,_,_  ), 0                         , 6  , 0  , 38 , 5  ), // #112
  INST(Cmpxchg          , X86Cmpxchg         , O(000F00,B0,_,_,x,_,_,_  ), 0                         , 4  , 0  , 39 , 38 ), // #113
  INST(Cmpxchg16b       , X86Cmpxchg8b_16b   , O(000F00,C7,1,_,1,_,_,_  ), 0                         , 28 , 0  , 40 , 39 ), // #114
  INST(Cmpxchg8b        , X86Cmpxchg8b_16b   , O(000F00,C7,1,_,_,_,_,_  ), 0                         , 29 , 0  , 41 , 40 ), // #115
  INST(Comisd           , ExtRm              , O(660F00,2F,_,_,_,_,_,_  ), 0                         , 3  , 0  , 6  , 41 ), // #116
  INST(Comiss           , ExtRm              , O(000F00,2F,_,_,_,_,_,_  ), 0                         , 4  , 0  , 7  , 42 ), // #117
  INST(Cpuid            , X86Op              , O(000F00,A2,_,_,_,_,_,_  ), 0                         , 4  , 0  , 42 , 43 ), // #118
  INST(Cqo              , X86Op_xDX_xAX      , O(000000,99,_,_,1,_,_,_  ), 0                         , 20 , 0  , 43 , 0  ), // #119
  INST(Crc32            , X86Crc             , O(F20F38,F0,_,_,x,_,_,_  ), 0                         , 30 , 0  , 44 , 44 ), // #120
  INST(Cvtdq2pd         , ExtRm              , O(F30F00,E6,_,_,_,_,_,_  ), 0                         , 6  , 0  , 6  , 4  ), // #121
  INST(Cvtdq2ps         , ExtRm              , O(000F00,5B,_,_,_,_,_,_  ), 0                         , 4  , 0  , 5  , 4  ), // #122
  INST(Cvtpd2dq         , ExtRm              , O(F20F00,E6,_,_,_,_,_,_  ), 0                         , 5  , 0  , 5  , 4  ), // #123
  INST(Cvtpd2pi         , ExtRm              , O(660F00,2D,_,_,_,_,_,_  ), 0                         , 3  , 0  , 45 , 4  ), // #124
  INST(Cvtpd2ps         , ExtRm              , O(660F00,5A,_,_,_,_,_,_  ), 0                         , 3  , 0  , 5  , 4  ), // #125
  INST(Cvtpi2pd         , ExtRm              , O(660F00,2A,_,_,_,_,_,_  ), 0                         , 3  , 0  , 46 , 4  ), // #126
  INST(Cvtpi2ps         , ExtRm              , O(000F00,2A,_,_,_,_,_,_  ), 0                         , 4  , 0  , 46 , 5  ), // #127
  INST(Cvtps2dq         , ExtRm              , O(660F00,5B,_,_,_,_,_,_  ), 0                         , 3  , 0  , 5  , 4  ), // #128
  INST(Cvtps2pd         , ExtRm              , O(000F00,5A,_,_,_,_,_,_  ), 0                         , 4  , 0  , 6  , 4  ), // #129
  INST(Cvtps2pi         , ExtRm              , O(000F00,2D,_,_,_,_,_,_  ), 0                         , 4  , 0  , 47 , 5  ), // #130
  INST(Cvtsd2si         , ExtRm_Wx_GpqOnly   , O(F20F00,2D,_,_,x,_,_,_  ), 0                         , 5  , 0  , 48 , 4  ), // #131
  INST(Cvtsd2ss         , ExtRm              , O(F20F00,5A,_,_,_,_,_,_  ), 0                         , 5  , 0  , 6  , 4  ), // #132
  INST(Cvtsi2sd         , ExtRm_Wx           , O(F20F00,2A,_,_,x,_,_,_  ), 0                         , 5  , 0  , 49 , 4  ), // #133
  INST(Cvtsi2ss         , ExtRm_Wx           , O(F30F00,2A,_,_,x,_,_,_  ), 0                         , 6  , 0  , 49 , 5  ), // #134
  INST(Cvtss2sd         , ExtRm              , O(F30F00,5A,_,_,_,_,_,_  ), 0                         , 6  , 0  , 7  , 4  ), // #135
  INST(Cvtss2si         , ExtRm_Wx_GpqOnly   , O(F30F00,2D,_,_,x,_,_,_  ), 0                         , 6  , 0  , 50 , 5  ), // #136
  INST(Cvttpd2dq        , ExtRm              , O(660F00,E6,_,_,_,_,_,_  ), 0                         , 3  , 0  , 5  , 4  ), // #137
  INST(Cvttpd2pi        , ExtRm              , O(660F00,2C,_,_,_,_,_,_  ), 0                         , 3  , 0  , 45 , 4  ), // #138
  INST(Cvttps2dq        , ExtRm              , O(F30F00,5B,_,_,_,_,_,_  ), 0                         , 6  , 0  , 5  , 4  ), // #139
  INST(Cvttps2pi        , ExtRm              , O(000F00,2C,_,_,_,_,_,_  ), 0                         , 4  , 0  , 47 , 5  ), // #140
  INST(Cvttsd2si        , ExtRm_Wx_GpqOnly   , O(F20F00,2C,_,_,x,_,_,_  ), 0                         , 5  , 0  , 48 , 4  ), // #141
  INST(Cvttss2si        , ExtRm_Wx_GpqOnly   , O(F30F00,2C,_,_,x,_,_,_  ), 0                         , 6  , 0  , 50 , 5  ), // #142
  INST(Cwd              , X86Op_xDX_xAX      , O(660000,99,_,_,_,_,_,_  ), 0                         , 19 , 0  , 51 , 0  ), // #143
  INST(Cwde             , X86Op_xAX          , O(000000,98,_,_,_,_,_,_  ), 0                         , 0  , 0  , 52 , 0  ), // #144
  INST(Daa              , X86Op              , O(000000,27,_,_,_,_,_,_  ), 0                         , 0  , 0  , 1  , 1  ), // #145
  INST(Das              , X86Op              , O(000000,2F,_,_,_,_,_,_  ), 0                         , 0  , 0  , 1  , 1  ), // #146
  INST(Dec              , X86IncDec          , O(000000,FE,1,_,x,_,_,_  ), O(000000,48,_,_,x,_,_,_  ), 31 , 6  , 53 , 45 ), // #147
  INST(Div              , X86M_GPB_MulDiv    , O(000000,F6,6,_,x,_,_,_  ), 0                         , 32 , 0  , 54 , 1  ), // #148
  INST(Divpd            , ExtRm              , O(660F00,5E,_,_,_,_,_,_  ), 0                         , 3  , 0  , 5  , 4  ), // #149
  INST(Divps            , ExtRm              , O(000F00,5E,_,_,_,_,_,_  ), 0                         , 4  , 0  , 5  , 5  ), // #150
  INST(Divsd            , ExtRm              , O(F20F00,5E,_,_,_,_,_,_  ), 0                         , 5  , 0  , 6  , 4  ), // #151
  INST(Divss            , ExtRm              , O(F30F00,5E,_,_,_,_,_,_  ), 0                         , 6  , 0  , 7  , 5  ), // #152
  INST(Dppd             , ExtRmi             , O(660F3A,41,_,_,_,_,_,_  ), 0                         , 8  , 0  , 8  , 12 ), // #153
  INST(Dpps             , ExtRmi             , O(660F3A,40,_,_,_,_,_,_  ), 0                         , 8  , 0  , 8  , 12 ), // #154
  INST(Emms             , X86Op              , O(000F00,77,_,_,_,_,_,_  ), 0                         , 4  , 0  , 55 , 46 ), // #155
  INST(Endbr32          , X86Op_Mod11RM      , O(F30F00,1E,7,_,_,_,_,3  ), 0                         , 33 , 0  , 30 , 47 ), // #156
  INST(Endbr64          , X86Op_Mod11RM      , O(F30F00,1E,7,_,_,_,_,2  ), 0                         , 34 , 0  , 30 , 47 ), // #157
  INST(Enqcmd           , X86EnqcmdMovdir64b , O(F20F38,F8,_,_,_,_,_,_  ), 0                         , 30 , 0  , 56 , 48 ), // #158
  INST(Enqcmds          , X86EnqcmdMovdir64b , O(F30F38,F8,_,_,_,_,_,_  ), 0                         , 7  , 0  , 56 , 48 ), // #159
  INST(Enter            , X86Enter           , O(000000,C8,_,_,_,_,_,_  ), 0                         , 0  , 0  , 57 , 0  ), // #160
  INST(Extractps        , ExtExtract         , O(660F3A,17,_,_,_,_,_,_  ), 0                         , 8  , 0  , 58 , 12 ), // #161
  INST(Extrq            , ExtExtrq           , O(660F00,79,_,_,_,_,_,_  ), O(660F00,78,0,_,_,_,_,_  ), 3  , 7  , 59 , 49 ), // #162
  INST(F2xm1            , FpuOp              , O_FPU(00,D9F0,_)          , 0                         , 35 , 0  , 30 , 0  ), // #163
  INST(Fabs             , FpuOp              , O_FPU(00,D9E1,_)          , 0                         , 35 , 0  , 30 , 0  ), // #164
  INST(Fadd             , FpuArith           , O_FPU(00,C0C0,0)          , 0                         , 36 , 0  , 60 , 0  ), // #165
  INST(Faddp            , FpuRDef            , O_FPU(00,DEC0,_)          , 0                         , 37 , 0  , 61 , 0  ), // #166
  INST(Fbld             , X86M_Only          , O_FPU(00,00DF,4)          , 0                         , 38 , 0  , 62 , 0  ), // #167
  INST(Fbstp            , X86M_Only          , O_FPU(00,00DF,6)          , 0                         , 39 , 0  , 62 , 0  ), // #168
  INST(Fchs             , FpuOp              , O_FPU(00,D9E0,_)          , 0                         , 35 , 0  , 30 , 0  ), // #169
  INST(Fclex            , FpuOp              , O_FPU(9B,DBE2,_)          , 0                         , 40 , 0  , 30 , 0  ), // #170
  INST(Fcmovb           , FpuR               , O_FPU(00,DAC0,_)          , 0                         , 41 , 0  , 63 , 30 ), // #171
  INST(Fcmovbe          , FpuR               , O_FPU(00,DAD0,_)          , 0                         , 41 , 0  , 63 , 29 ), // #172
  INST(Fcmove           , FpuR               , O_FPU(00,DAC8,_)          , 0                         , 41 , 0  , 63 , 31 ), // #173
  INST(Fcmovnb          , FpuR               , O_FPU(00,DBC0,_)          , 0                         , 42 , 0  , 63 , 30 ), // #174
  INST(Fcmovnbe         , FpuR               , O_FPU(00,DBD0,_)          , 0                         , 42 , 0  , 63 , 29 ), // #175
  INST(Fcmovne          , FpuR               , O_FPU(00,DBC8,_)          , 0                         , 42 , 0  , 63 , 31 ), // #176
  INST(Fcmovnu          , FpuR               , O_FPU(00,DBD8,_)          , 0                         , 42 , 0  , 63 , 35 ), // #177
  INST(Fcmovu           , FpuR               , O_FPU(00,DAD8,_)          , 0                         , 41 , 0  , 63 , 35 ), // #178
  INST(Fcom             , FpuCom             , O_FPU(00,D0D0,2)          , 0                         , 43 , 0  , 64 , 0  ), // #179
  INST(Fcomi            , FpuR               , O_FPU(00,DBF0,_)          , 0                         , 42 , 0  , 63 , 50 ), // #180
  INST(Fcomip           , FpuR               , O_FPU(00,DFF0,_)          , 0                         , 44 , 0  , 63 , 50 ), // #181
  INST(Fcomp            , FpuCom             , O_FPU(00,D8D8,3)          , 0                         , 45 , 0  , 64 , 0  ), // #182
  INST(Fcompp           , FpuOp              , O_FPU(00,DED9,_)          , 0                         , 37 , 0  , 30 , 0  ), // #183
  INST(Fcos             , FpuOp              , O_FPU(00,D9FF,_)          , 0                         , 35 , 0  , 30 , 0  ), // #184
  INST(Fdecstp          , FpuOp              , O_FPU(00,D9F6,_)          , 0                         , 35 , 0  , 30 , 0  ), // #185
  INST(Fdiv             , FpuArith           , O_FPU(00,F0F8,6)          , 0                         , 46 , 0  , 60 , 0  ), // #186
  INST(Fdivp            , FpuRDef            , O_FPU(00,DEF8,_)          , 0                         , 37 , 0  , 61 , 0  ), // #187
  INST(Fdivr            , FpuArith           , O_FPU(00,F8F0,7)          , 0                         , 47 , 0  , 60 , 0  ), // #188
  INST(Fdivrp           , FpuRDef            , O_FPU(00,DEF0,_)          , 0                         , 37 , 0  , 61 , 0  ), // #189
  INST(Femms            , X86Op              , O(000F00,0E,_,_,_,_,_,_  ), 0                         , 4  , 0  , 30 , 51 ), // #190
  INST(Ffree            , FpuR               , O_FPU(00,DDC0,_)          , 0                         , 48 , 0  , 63 , 0  ), // #191
  INST(Fiadd            , FpuM               , O_FPU(00,00DA,0)          , 0                         , 49 , 0  , 65 , 0  ), // #192
  INST(Ficom            , FpuM               , O_FPU(00,00DA,2)          , 0                         , 50 , 0  , 65 , 0  ), // #193
  INST(Ficomp           , FpuM               , O_FPU(00,00DA,3)          , 0                         , 51 , 0  , 65 , 0  ), // #194
  INST(Fidiv            , FpuM               , O_FPU(00,00DA,6)          , 0                         , 39 , 0  , 65 , 0  ), // #195
  INST(Fidivr           , FpuM               , O_FPU(00,00DA,7)          , 0                         , 52 , 0  , 65 , 0  ), // #196
  INST(Fild             , FpuM               , O_FPU(00,00DB,0)          , O_FPU(00,00DF,5)          , 49 , 8  , 66 , 0  ), // #197
  INST(Fimul            , FpuM               , O_FPU(00,00DA,1)          , 0                         , 53 , 0  , 65 , 0  ), // #198
  INST(Fincstp          , FpuOp              , O_FPU(00,D9F7,_)          , 0                         , 35 , 0  , 30 , 0  ), // #199
  INST(Finit            , FpuOp              , O_FPU(9B,DBE3,_)          , 0                         , 40 , 0  , 30 , 0  ), // #200
  INST(Fist             , FpuM               , O_FPU(00,00DB,2)          , 0                         , 50 , 0  , 65 , 0  ), // #201
  INST(Fistp            , FpuM               , O_FPU(00,00DB,3)          , O_FPU(00,00DF,7)          , 51 , 9  , 66 , 0  ), // #202
  INST(Fisttp           , FpuM               , O_FPU(00,00DB,1)          , O_FPU(00,00DD,1)          , 53 , 10 , 66 , 6  ), // #203
  INST(Fisub            , FpuM               , O_FPU(00,00DA,4)          , 0                         , 38 , 0  , 65 , 0  ), // #204
  INST(Fisubr           , FpuM               , O_FPU(00,00DA,5)          , 0                         , 54 , 0  , 65 , 0  ), // #205
  INST(Fld              , FpuFldFst          , O_FPU(00,00D9,0)          , O_FPU(00,00DB,5)          , 49 , 11 , 67 , 0  ), // #206
  INST(Fld1             , FpuOp              , O_FPU(00,D9E8,_)          , 0                         , 35 , 0  , 30 , 0  ), // #207
  INST(Fldcw            , X86M_Only          , O_FPU(00,00D9,5)          , 0                         , 54 , 0  , 68 , 0  ), // #208
  INST(Fldenv           , X86M_Only          , O_FPU(00,00D9,4)          , 0                         , 38 , 0  , 69 , 0  ), // #209
  INST(Fldl2e           , FpuOp              , O_FPU(00,D9EA,_)          , 0                         , 35 , 0  , 30 , 0  ), // #210
  INST(Fldl2t           , FpuOp              , O_FPU(00,D9E9,_)          , 0                         , 35 , 0  , 30 , 0  ), // #211
  INST(Fldlg2           , FpuOp              , O_FPU(00,D9EC,_)          , 0                         , 35 , 0  , 30 , 0  ), // #212
  INST(Fldln2           , FpuOp              , O_FPU(00,D9ED,_)          , 0                         , 35 , 0  , 30 , 0  ), // #213
  INST(Fldpi            , FpuOp              , O_FPU(00,D9EB,_)          , 0                         , 35 , 0  , 30 , 0  ), // #214
  INST(Fldz             , FpuOp              , O_FPU(00,D9EE,_)          , 0                         , 35 , 0  , 30 , 0  ), // #215
  INST(Fmul             , FpuArith           , O_FPU(00,C8C8,1)          , 0                         , 55 , 0  , 60 , 0  ), // #216
  INST(Fmulp            , FpuRDef            , O_FPU(00,DEC8,_)          , 0                         , 37 , 0  , 61 , 0  ), // #217
  INST(Fnclex           , FpuOp              , O_FPU(00,DBE2,_)          , 0                         , 42 , 0  , 30 , 0  ), // #218
  INST(Fninit           , FpuOp              , O_FPU(00,DBE3,_)          , 0                         , 42 , 0  , 30 , 0  ), // #219
  INST(Fnop             , FpuOp              , O_FPU(00,D9D0,_)          , 0                         , 35 , 0  , 30 , 0  ), // #220
  INST(Fnsave           , X86M_Only          , O_FPU(00,00DD,6)          , 0                         , 39 , 0  , 69 , 0  ), // #221
  INST(Fnstcw           , X86M_Only          , O_FPU(00,00D9,7)          , 0                         , 52 , 0  , 68 , 0  ), // #222
  INST(Fnstenv          , X86M_Only          , O_FPU(00,00D9,6)          , 0                         , 39 , 0  , 69 , 0  ), // #223
  INST(Fnstsw           , FpuStsw            , O_FPU(00,00DD,7)          , O_FPU(00,DFE0,_)          , 52 , 12 , 70 , 0  ), // #224
  INST(Fpatan           , FpuOp              , O_FPU(00,D9F3,_)          , 0                         , 35 , 0  , 30 , 0  ), // #225
  INST(Fprem            , FpuOp              , O_FPU(00,D9F8,_)          , 0                         , 35 , 0  , 30 , 0  ), // #226
  INST(Fprem1           , FpuOp              , O_FPU(00,D9F5,_)          , 0                         , 35 , 0  , 30 , 0  ), // #227
  INST(Fptan            , FpuOp              , O_FPU(00,D9F2,_)          , 0                         , 35 , 0  , 30 , 0  ), // #228
  INST(Frndint          , FpuOp              , O_FPU(00,D9FC,_)          , 0                         , 35 , 0  , 30 , 0  ), // #229
  INST(Frstor           , X86M_Only          , O_FPU(00,00DD,4)          , 0                         , 38 , 0  , 69 , 0  ), // #230
  INST(Fsave            , X86M_Only          , O_FPU(9B,00DD,6)          , 0                         , 56 , 0  , 69 , 0  ), // #231
  INST(Fscale           , FpuOp              , O_FPU(00,D9FD,_)          , 0                         , 35 , 0  , 30 , 0  ), // #232
  INST(Fsin             , FpuOp              , O_FPU(00,D9FE,_)          , 0                         , 35 , 0  , 30 , 0  ), // #233
  INST(Fsincos          , FpuOp              , O_FPU(00,D9FB,_)          , 0                         , 35 , 0  , 30 , 0  ), // #234
  INST(Fsqrt            , FpuOp              , O_FPU(00,D9FA,_)          , 0                         , 35 , 0  , 30 , 0  ), // #235
  INST(Fst              , FpuFldFst          , O_FPU(00,00D9,2)          , 0                         , 50 , 0  , 71 , 0  ), // #236
  INST(Fstcw            , X86M_Only          , O_FPU(9B,00D9,7)          , 0                         , 57 , 0  , 68 , 0  ), // #237
  INST(Fstenv           , X86M_Only          , O_FPU(9B,00D9,6)          , 0                         , 56 , 0  , 69 , 0  ), // #238
  INST(Fstp             , FpuFldFst          , O_FPU(00,00D9,3)          , O(000000,DB,7,_,_,_,_,_  ), 51 , 13 , 67 , 0  ), // #239
  INST(Fstsw            , FpuStsw            , O_FPU(9B,00DD,7)          , O_FPU(9B,DFE0,_)          , 57 , 14 , 70 , 0  ), // #240
  INST(Fsub             , FpuArith           , O_FPU(00,E0E8,4)          , 0                         , 58 , 0  , 60 , 0  ), // #241
  INST(Fsubp            , FpuRDef            , O_FPU(00,DEE8,_)          , 0                         , 37 , 0  , 61 , 0  ), // #242
  INST(Fsubr            , FpuArith           , O_FPU(00,E8E0,5)          , 0                         , 59 , 0  , 60 , 0  ), // #243
  INST(Fsubrp           , FpuRDef            , O_FPU(00,DEE0,_)          , 0                         , 37 , 0  , 61 , 0  ), // #244
  INST(Ftst             , FpuOp              , O_FPU(00,D9E4,_)          , 0                         , 35 , 0  , 30 , 0  ), // #245
  INST(Fucom            , FpuRDef            , O_FPU(00,DDE0,_)          , 0                         , 48 , 0  , 61 , 0  ), // #246
  INST(Fucomi           , FpuR               , O_FPU(00,DBE8,_)          , 0                         , 42 , 0  , 63 , 50 ), // #247
  INST(Fucomip          , FpuR               , O_FPU(00,DFE8,_)          , 0                         , 44 , 0  , 63 , 50 ), // #248
  INST(Fucomp           , FpuRDef            , O_FPU(00,DDE8,_)          , 0                         , 48 , 0  , 61 , 0  ), // #249
  INST(Fucompp          , FpuOp              , O_FPU(00,DAE9,_)          , 0                         , 41 , 0  , 30 , 0  ), // #250
  INST(Fwait            , X86Op              , O_FPU(00,009B,_)          , 0                         , 49 , 0  , 30 , 0  ), // #251
  INST(Fxam             , FpuOp              , O_FPU(00,D9E5,_)          , 0                         , 35 , 0  , 30 , 0  ), // #252
  INST(Fxch             , FpuR               , O_FPU(00,D9C8,_)          , 0                         , 35 , 0  , 61 , 0  ), // #253
  INST(Fxrstor          , X86M_Only          , O(000F00,AE,1,_,_,_,_,_  ), 0                         , 29 , 0  , 69 , 52 ), // #254
  INST(Fxrstor64        , X86M_Only          , O(000F00,AE,1,_,1,_,_,_  ), 0                         , 28 , 0  , 72 , 52 ), // #255
  INST(Fxsave           , X86M_Only          , O(000F00,AE,0,_,_,_,_,_  ), 0                         , 4  , 0  , 69 , 52 ), // #256
  INST(Fxsave64         , X86M_Only          , O(000F00,AE,0,_,1,_,_,_  ), 0                         , 60 , 0  , 72 , 52 ), // #257
  INST(Fxtract          , FpuOp              , O_FPU(00,D9F4,_)          , 0                         , 35 , 0  , 30 , 0  ), // #258
  INST(Fyl2x            , FpuOp              , O_FPU(00,D9F1,_)          , 0                         , 35 , 0  , 30 , 0  ), // #259
  INST(Fyl2xp1          , FpuOp              , O_FPU(00,D9F9,_)          , 0                         , 35 , 0  , 30 , 0  ), // #260
  INST(Getsec           , X86Op              , O(000F00,37,_,_,_,_,_,_  ), 0                         , 4  , 0  , 30 , 53 ), // #261
  INST(Gf2p8affineinvqb , ExtRmi             , O(660F3A,CF,_,_,_,_,_,_  ), 0                         , 8  , 0  , 8  , 54 ), // #262
  INST(Gf2p8affineqb    , ExtRmi             , O(660F3A,CE,_,_,_,_,_,_  ), 0                         , 8  , 0  , 8  , 54 ), // #263
  INST(Gf2p8mulb        , ExtRm              , O(660F38,CF,_,_,_,_,_,_  ), 0                         , 2  , 0  , 5  , 54 ), // #264
  INST(Haddpd           , ExtRm              , O(660F00,7C,_,_,_,_,_,_  ), 0                         , 3  , 0  , 5  , 6  ), // #265
  INST(Haddps           , ExtRm              , O(F20F00,7C,_,_,_,_,_,_  ), 0                         , 5  , 0  , 5  , 6  ), // #266
  INST(Hlt              , X86Op              , O(000000,F4,_,_,_,_,_,_  ), 0                         , 0  , 0  , 30 , 0  ), // #267
  INST(Hreset           , X86Op_Mod11RM_I8   , O(F30F3A,F0,0,_,_,_,_,_  ), 0                         , 61 , 0  , 73 , 55 ), // #268
  INST(Hsubpd           , ExtRm              , O(660F00,7D,_,_,_,_,_,_  ), 0                         , 3  , 0  , 5  , 6  ), // #269
  INST(Hsubps           , ExtRm              , O(F20F00,7D,_,_,_,_,_,_  ), 0                         , 5  , 0  , 5  , 6  ), // #270
  INST(Idiv             , X86M_GPB_MulDiv    , O(000000,F6,7,_,x,_,_,_  ), 0                         , 27 , 0  , 54 , 1  ), // #271
  INST(Imul             , X86Imul            , O(000000,F6,5,_,x,_,_,_  ), 0                         , 62 , 0  , 74 , 1  ), // #272
  INST(In               , X86In              , O(000000,EC,_,_,_,_,_,_  ), O(000000,E4,_,_,_,_,_,_  ), 0  , 15 , 75 , 0  ), // #273
  INST(Inc              , X86IncDec          , O(000000,FE,0,_,x,_,_,_  ), O(000000,40,_,_,x,_,_,_  ), 0  , 16 , 53 , 45 ), // #274
  INST(Incsspd          , X86M               , O(F30F00,AE,5,_,0,_,_,_  ), 0                         , 63 , 0  , 76 , 56 ), // #275
  INST(Incsspq          , X86M               , O(F30F00,AE,5,_,1,_,_,_  ), 0                         , 64 , 0  , 77 , 56 ), // #276
  INST(Ins              , X86Ins             , O(000000,6C,_,_,_,_,_,_  ), 0                         , 0  , 0  , 78 , 0  ), // #277
  INST(Insertps         , ExtRmi             , O(660F3A,21,_,_,_,_,_,_  ), 0                         , 8  , 0  , 38 , 12 ), // #278
  INST(Insertq          , ExtInsertq         , O(F20F00,79,_,_,_,_,_,_  ), O(F20F00,78,_,_,_,_,_,_  ), 5  , 17 , 79 , 49 ), // #279
  INST(Int              , X86Int             , O(000000,CD,_,_,_,_,_,_  ), 0                         , 0  , 0  , 80 , 0  ), // #280
  INST(Int3             , X86Op              , O(000000,CC,_,_,_,_,_,_  ), 0                         , 0  , 0  , 30 , 0  ), // #281
  INST(Into             , X86Op              , O(000000,CE,_,_,_,_,_,_  ), 0                         , 0  , 0  , 81 , 57 ), // #282
  INST(Invd             , X86Op              , O(000F00,08,_,_,_,_,_,_  ), 0                         , 4  , 0  , 30 , 43 ), // #283
  INST(Invept           , X86Rm_NoSize       , O(660F38,80,_,_,_,_,_,_  ), 0                         , 2  , 0  , 82 , 58 ), // #284
  INST(Invlpg           , X86M_Only          , O(000F00,01,7,_,_,_,_,_  ), 0                         , 22 , 0  , 69 , 43 ), // #285
  INST(Invlpga          , X86Op_xAddr        , O(000F01,DF,_,_,_,_,_,_  ), 0                         , 21 , 0  , 83 , 22 ), // #286
  INST(Invpcid          , X86Rm_NoSize       , O(660F38,82,_,_,_,_,_,_  ), 0                         , 2  , 0  , 82 , 43 ), // #287
  INST(Invvpid          , X86Rm_NoSize       , O(660F38,81,_,_,_,_,_,_  ), 0                         , 2  , 0  , 82 , 58 ), // #288
  INST(Iret             , X86Op              , O(660000,CF,_,_,_,_,_,_  ), 0                         , 19 , 0  , 84 , 1  ), // #289
  INST(Iretd            , X86Op              , O(000000,CF,_,_,_,_,_,_  ), 0                         , 0  , 0  , 84 , 1  ), // #290
  INST(Iretq            , X86Op              , O(000000,CF,_,_,1,_,_,_  ), 0                         , 20 , 0  , 85 , 1  ), // #291
  INST(Ja               , X86Jcc             , O(000F00,87,_,_,_,_,_,_  ), O(000000,77,_,_,_,_,_,_  ), 4  , 18 , 86 , 59 ), // #292
  INST(Jae              , X86Jcc             , O(000F00,83,_,_,_,_,_,_  ), O(000000,73,_,_,_,_,_,_  ), 4  , 19 , 86 , 60 ), // #293
  INST(Jb               , X86Jcc             , O(000F00,82,_,_,_,_,_,_  ), O(000000,72,_,_,_,_,_,_  ), 4  , 20 , 86 , 60 ), // #294
  INST(Jbe              , X86Jcc             , O(000F00,86,_,_,_,_,_,_  ), O(000000,76,_,_,_,_,_,_  ), 4  , 21 , 86 , 59 ), // #295
  INST(Jc               , X86Jcc             , O(000F00,82,_,_,_,_,_,_  ), O(000000,72,_,_,_,_,_,_  ), 4  , 20 , 86 , 60 ), // #296
  INST(Je               , X86Jcc             , O(000F00,84,_,_,_,_,_,_  ), O(000000,74,_,_,_,_,_,_  ), 4  , 22 , 86 , 61 ), // #297
  INST(Jecxz            , X86JecxzLoop       , 0                         , O(000000,E3,_,_,_,_,_,_  ), 0  , 23 , 87 , 0  ), // #298
  INST(Jg               , X86Jcc             , O(000F00,8F,_,_,_,_,_,_  ), O(000000,7F,_,_,_,_,_,_  ), 4  , 24 , 86 , 62 ), // #299
  INST(Jge              , X86Jcc             , O(000F00,8D,_,_,_,_,_,_  ), O(000000,7D,_,_,_,_,_,_  ), 4  , 25 , 86 , 63 ), // #300
  INST(Jl               , X86Jcc             , O(000F00,8C,_,_,_,_,_,_  ), O(000000,7C,_,_,_,_,_,_  ), 4  , 26 , 86 , 63 ), // #301
  INST(Jle              , X86Jcc             , O(000F00,8E,_,_,_,_,_,_  ), O(000000,7E,_,_,_,_,_,_  ), 4  , 27 , 86 , 62 ), // #302
  INST(Jmp              , X86Jmp             , O(000000,FF,4,_,_,_,_,_  ), O(000000,EB,_,_,_,_,_,_  ), 9  , 28 , 88 , 0  ), // #303
  INST(Jna              , X86Jcc             , O(000F00,86,_,_,_,_,_,_  ), O(000000,76,_,_,_,_,_,_  ), 4  , 21 , 86 , 59 ), // #304
  INST(Jnae             , X86Jcc             , O(000F00,82,_,_,_,_,_,_  ), O(000000,72,_,_,_,_,_,_  ), 4  , 20 , 86 , 60 ), // #305
  INST(Jnb              , X86Jcc             , O(000F00,83,_,_,_,_,_,_  ), O(000000,73,_,_,_,_,_,_  ), 4  , 19 , 86 , 60 ), // #306
  INST(Jnbe             , X86Jcc             , O(000F00,87,_,_,_,_,_,_  ), O(000000,77,_,_,_,_,_,_  ), 4  , 18 , 86 , 59 ), // #307
  INST(Jnc              , X86Jcc             , O(000F00,83,_,_,_,_,_,_  ), O(000000,73,_,_,_,_,_,_  ), 4  , 19 , 86 , 60 ), // #308
  INST(Jne              , X86Jcc             , O(000F00,85,_,_,_,_,_,_  ), O(000000,75,_,_,_,_,_,_  ), 4  , 29 , 86 , 61 ), // #309
  INST(Jng              , X86Jcc             , O(000F00,8E,_,_,_,_,_,_  ), O(000000,7E,_,_,_,_,_,_  ), 4  , 27 , 86 , 62 ), // #310
  INST(Jnge             , X86Jcc             , O(000F00,8C,_,_,_,_,_,_  ), O(000000,7C,_,_,_,_,_,_  ), 4  , 26 , 86 , 63 ), // #311
  INST(Jnl              , X86Jcc             , O(000F00,8D,_,_,_,_,_,_  ), O(000000,7D,_,_,_,_,_,_  ), 4  , 25 , 86 , 63 ), // #312
  INST(Jnle             , X86Jcc             , O(000F00,8F,_,_,_,_,_,_  ), O(000000,7F,_,_,_,_,_,_  ), 4  , 24 , 86 , 62 ), // #313
  INST(Jno              , X86Jcc             , O(000F00,81,_,_,_,_,_,_  ), O(000000,71,_,_,_,_,_,_  ), 4  , 30 , 86 , 57 ), // #314
  INST(Jnp              , X86Jcc             , O(000F00,8B,_,_,_,_,_,_  ), O(000000,7B,_,_,_,_,_,_  ), 4  , 31 , 86 , 64 ), // #315
  INST(Jns              , X86Jcc             , O(000F00,89,_,_,_,_,_,_  ), O(000000,79,_,_,_,_,_,_  ), 4  , 32 , 86 , 65 ), // #316
  INST(Jnz              , X86Jcc             , O(000F00,85,_,_,_,_,_,_  ), O(000000,75,_,_,_,_,_,_  ), 4  , 29 , 86 , 61 ), // #317
  INST(Jo               , X86Jcc             , O(000F00,80,_,_,_,_,_,_  ), O(000000,70,_,_,_,_,_,_  ), 4  , 33 , 86 , 57 ), // #318
  INST(Jp               , X86Jcc             , O(000F00,8A,_,_,_,_,_,_  ), O(000000,7A,_,_,_,_,_,_  ), 4  , 34 , 86 , 64 ), // #319
  INST(Jpe              , X86Jcc             , O(000F00,8A,_,_,_,_,_,_  ), O(000000,7A,_,_,_,_,_,_  ), 4  , 34 , 86 , 64 ), // #320
  INST(Jpo              , X86Jcc             , O(000F00,8B,_,_,_,_,_,_  ), O(000000,7B,_,_,_,_,_,_  ), 4  , 31 , 86 , 64 ), // #321
  INST(Js               , X86Jcc             , O(000F00,88,_,_,_,_,_,_  ), O(000000,78,_,_,_,_,_,_  ), 4  , 35 , 86 , 65 ), // #322
  INST(Jz               , X86Jcc             , O(000F00,84,_,_,_,_,_,_  ), O(000000,74,_,_,_,_,_,_  ), 4  , 22 , 86 , 61 ), // #323
  INST(Kaddb            , VexRvm             , V(660F00,4A,_,1,0,_,_,_  ), 0                         , 65 , 0  , 89 , 66 ), // #324
  INST(Kaddd            , VexRvm             , V(660F00,4A,_,1,1,_,_,_  ), 0                         , 66 , 0  , 89 , 67 ), // #325
  INST(Kaddq            , VexRvm             , V(000F00,4A,_,1,1,_,_,_  ), 0                         , 67 , 0  , 89 , 67 ), // #326
  INST(Kaddw            , VexRvm             , V(000F00,4A,_,1,0,_,_,_  ), 0                         , 68 , 0  , 89 , 66 ), // #327
  INST(Kandb            , VexRvm             , V(660F00,41,_,1,0,_,_,_  ), 0                         , 65 , 0  , 89 , 66 ), // #328
  INST(Kandd            , VexRvm             , V(660F00,41,_,1,1,_,_,_  ), 0                         , 66 , 0  , 89 , 67 ), // #329
  INST(Kandnb           , VexRvm             , V(660F00,42,_,1,0,_,_,_  ), 0                         , 65 , 0  , 89 , 66 ), // #330
  INST(Kandnd           , VexRvm             , V(660F00,42,_,1,1,_,_,_  ), 0                         , 66 , 0  , 89 , 67 ), // #331
  INST(Kandnq           , VexRvm             , V(000F00,42,_,1,1,_,_,_  ), 0                         , 67 , 0  , 89 , 67 ), // #332
  INST(Kandnw           , VexRvm             , V(000F00,42,_,1,0,_,_,_  ), 0                         , 68 , 0  , 89 , 68 ), // #333
  INST(Kandq            , VexRvm             , V(000F00,41,_,1,1,_,_,_  ), 0                         , 67 , 0  , 89 , 67 ), // #334
  INST(Kandw            , VexRvm             , V(000F00,41,_,1,0,_,_,_  ), 0                         , 68 , 0  , 89 , 68 ), // #335
  INST(Kmovb            , VexKmov            , V(660F00,90,_,0,0,_,_,_  ), V(660F00,92,_,0,0,_,_,_  ), 69 , 36 , 90 , 69 ), // #336
  INST(Kmovd            , VexKmov            , V(660F00,90,_,0,1,_,_,_  ), V(F20F00,92,_,0,0,_,_,_  ), 70 , 37 , 91 , 70 ), // #337
  INST(Kmovq            , VexKmov            , V(000F00,90,_,0,1,_,_,_  ), V(F20F00,92,_,0,1,_,_,_  ), 71 , 38 , 92 , 70 ), // #338
  INST(Kmovw            , VexKmov            , V(000F00,90,_,0,0,_,_,_  ), V(000F00,92,_,0,0,_,_,_  ), 72 , 39 , 93 , 71 ), // #339
  INST(Knotb            , VexRm              , V(660F00,44,_,0,0,_,_,_  ), 0                         , 69 , 0  , 94 , 66 ), // #340
  INST(Knotd            , VexRm              , V(660F00,44,_,0,1,_,_,_  ), 0                         , 70 , 0  , 94 , 67 ), // #341
  INST(Knotq            , VexRm              , V(000F00,44,_,0,1,_,_,_  ), 0                         , 71 , 0  , 94 , 67 ), // #342
  INST(Knotw            , VexRm              , V(000F00,44,_,0,0,_,_,_  ), 0                         , 72 , 0  , 94 , 68 ), // #343
  INST(Korb             , VexRvm             , V(660F00,45,_,1,0,_,_,_  ), 0                         , 65 , 0  , 89 , 66 ), // #344
  INST(Kord             , VexRvm             , V(660F00,45,_,1,1,_,_,_  ), 0                         , 66 , 0  , 89 , 67 ), // #345
  INST(Korq             , VexRvm             , V(000F00,45,_,1,1,_,_,_  ), 0                         , 67 , 0  , 89 , 67 ), // #346
  INST(Kortestb         , VexRm              , V(660F00,98,_,0,0,_,_,_  ), 0                         , 69 , 0  , 94 , 72 ), // #347
  INST(Kortestd         , VexRm              , V(660F00,98,_,0,1,_,_,_  ), 0                         , 70 , 0  , 94 , 73 ), // #348
  INST(Kortestq         , VexRm              , V(000F00,98,_,0,1,_,_,_  ), 0                         , 71 , 0  , 94 , 73 ), // #349
  INST(Kortestw         , VexRm              , V(000F00,98,_,0,0,_,_,_  ), 0                         , 72 , 0  , 94 , 74 ), // #350
  INST(Korw             , VexRvm             , V(000F00,45,_,1,0,_,_,_  ), 0                         , 68 , 0  , 89 , 68 ), // #351
  INST(Kshiftlb         , VexRmi             , V(660F3A,32,_,0,0,_,_,_  ), 0                         , 73 , 0  , 95 , 66 ), // #352
  INST(Kshiftld         , VexRmi             , V(660F3A,33,_,0,0,_,_,_  ), 0                         , 73 , 0  , 95 , 67 ), // #353
  INST(Kshiftlq         , VexRmi             , V(660F3A,33,_,0,1,_,_,_  ), 0                         , 74 , 0  , 95 , 67 ), // #354
  INST(Kshiftlw         , VexRmi             , V(660F3A,32,_,0,1,_,_,_  ), 0                         , 74 , 0  , 95 , 68 ), // #355
  INST(Kshiftrb         , VexRmi             , V(660F3A,30,_,0,0,_,_,_  ), 0                         , 73 , 0  , 95 , 66 ), // #356
  INST(Kshiftrd         , VexRmi             , V(660F3A,31,_,0,0,_,_,_  ), 0                         , 73 , 0  , 95 , 67 ), // #357
  INST(Kshiftrq         , VexRmi             , V(660F3A,31,_,0,1,_,_,_  ), 0                         , 74 , 0  , 95 , 67 ), // #358
  INST(Kshiftrw         , VexRmi             , V(660F3A,30,_,0,1,_,_,_  ), 0                         , 74 , 0  , 95 , 68 ), // #359
  INST(Ktestb           , VexRm              , V(660F00,99,_,0,0,_,_,_  ), 0                         , 69 , 0  , 94 , 72 ), // #360
  INST(Ktestd           , VexRm              , V(660F00,99,_,0,1,_,_,_  ), 0                         , 70 , 0  , 94 , 73 ), // #361
  INST(Ktestq           , VexRm              , V(000F00,99,_,0,1,_,_,_  ), 0                         , 71 , 0  , 94 , 73 ), // #362
  INST(Ktestw           , VexRm              , V(000F00,99,_,0,0,_,_,_  ), 0                         , 72 , 0  , 94 , 72 ), // #363
  INST(Kunpckbw         , VexRvm             , V(660F00,4B,_,1,0,_,_,_  ), 0                         , 65 , 0  , 89 , 68 ), // #364
  INST(Kunpckdq         , VexRvm             , V(000F00,4B,_,1,1,_,_,_  ), 0                         , 67 , 0  , 89 , 67 ), // #365
  INST(Kunpckwd         , VexRvm             , V(000F00,4B,_,1,0,_,_,_  ), 0                         , 68 , 0  , 89 , 67 ), // #366
  INST(Kxnorb           , VexRvm             , V(660F00,46,_,1,0,_,_,_  ), 0                         , 65 , 0  , 96 , 66 ), // #367
  INST(Kxnord           , VexRvm             , V(660F00,46,_,1,1,_,_,_  ), 0                         , 66 , 0  , 96 , 67 ), // #368
  INST(Kxnorq           , VexRvm             , V(000F00,46,_,1,1,_,_,_  ), 0                         , 67 , 0  , 96 , 67 ), // #369
  INST(Kxnorw           , VexRvm             , V(000F00,46,_,1,0,_,_,_  ), 0                         , 68 , 0  , 96 , 68 ), // #370
  INST(Kxorb            , VexRvm             , V(660F00,47,_,1,0,_,_,_  ), 0                         , 65 , 0  , 96 , 66 ), // #371
  INST(Kxord            , VexRvm             , V(660F00,47,_,1,1,_,_,_  ), 0                         , 66 , 0  , 96 , 67 ), // #372
  INST(Kxorq            , VexRvm             , V(000F00,47,_,1,1,_,_,_  ), 0                         , 67 , 0  , 96 , 67 ), // #373
  INST(Kxorw            , VexRvm             , V(000F00,47,_,1,0,_,_,_  ), 0                         , 68 , 0  , 96 , 68 ), // #374
  INST(Lahf             , X86Op              , O(000000,9F,_,_,_,_,_,_  ), 0                         , 0  , 0  , 97 , 75 ), // #375
  INST(Lar              , X86Rm              , O(000F00,02,_,_,_,_,_,_  ), 0                         , 4  , 0  , 98 , 10 ), // #376
  INST(Lcall            , X86LcallLjmp       , O(000000,FF,3,_,_,_,_,_  ), O(000000,9A,_,_,_,_,_,_  ), 75 , 40 , 99 , 1  ), // #377
  INST(Lddqu            , ExtRm              , O(F20F00,F0,_,_,_,_,_,_  ), 0                         , 5  , 0  , 100, 6  ), // #378
  INST(Ldmxcsr          , X86M_Only          , O(000F00,AE,2,_,_,_,_,_  ), 0                         , 76 , 0  , 101, 5  ), // #379
  INST(Lds              , X86Rm              , O(000000,C5,_,_,_,_,_,_  ), 0                         , 0  , 0  , 102, 0  ), // #380
  INST(Ldtilecfg        , AmxCfg             , V(000F38,49,_,0,0,_,_,_  ), 0                         , 10 , 0  , 103, 76 ), // #381
  INST(Lea              , X86Lea             , O(000000,8D,_,_,x,_,_,_  ), 0                         , 0  , 0  , 104, 0  ), // #382
  INST(Leave            , X86Op              , O(000000,C9,_,_,_,_,_,_  ), 0                         , 0  , 0  , 30 , 0  ), // #383
  INST(Les              , X86Rm              , O(000000,C4,_,_,_,_,_,_  ), 0                         , 0  , 0  , 102, 0  ), // #384
  INST(Lfence           , X86Fence           , O(000F00,AE,5,_,_,_,_,_  ), 0                         , 77 , 0  , 30 , 4  ), // #385
  INST(Lfs              , X86Rm              , O(000F00,B4,_,_,_,_,_,_  ), 0                         , 4  , 0  , 105, 0  ), // #386
  INST(Lgdt             , X86M_Only          , O(000F00,01,2,_,_,_,_,_  ), 0                         , 76 , 0  , 69 , 0  ), // #387
  INST(Lgs              , X86Rm              , O(000F00,B5,_,_,_,_,_,_  ), 0                         , 4  , 0  , 105, 0  ), // #388
  INST(Lidt             , X86M_Only          , O(000F00,01,3,_,_,_,_,_  ), 0                         , 78 , 0  , 69 , 0  ), // #389
  INST(Ljmp             , X86LcallLjmp       , O(000000,FF,5,_,_,_,_,_  ), O(000000,EA,_,_,_,_,_,_  ), 62 , 41 , 106, 0  ), // #390
  INST(Lldt             , X86M_NoSize        , O(000F00,00,2,_,_,_,_,_  ), 0                         , 76 , 0  , 107, 0  ), // #391
  INST(Llwpcb           , VexR_Wx            , V(XOP_M9,12,0,0,x,_,_,_  ), 0                         , 79 , 0  , 108, 77 ), // #392
  INST(Lmsw             , X86M_NoSize        , O(000F00,01,6,_,_,_,_,_  ), 0                         , 80 , 0  , 107, 0  ), // #393
  INST(Lods             , X86StrRm           , O(000000,AC,_,_,_,_,_,_  ), 0                         , 0  , 0  , 109, 78 ), // #394
  INST(Loop             , X86JecxzLoop       , 0                         , O(000000,E2,_,_,_,_,_,_  ), 0  , 42 , 110, 0  ), // #395
  INST(Loope            , X86JecxzLoop       , 0                         , O(000000,E1,_,_,_,_,_,_  ), 0  , 43 , 110, 61 ), // #396
  INST(Loopne           , X86JecxzLoop       , 0                         , O(000000,E0,_,_,_,_,_,_  ), 0  , 44 , 110, 61 ), // #397
  INST(Lsl              , X86Rm              , O(000F00,03,_,_,_,_,_,_  ), 0                         , 4  , 0  , 111, 10 ), // #398
  INST(Lss              , X86Rm              , O(000F00,B2,_,_,_,_,_,_  ), 0                         , 4  , 0  , 105, 0  ), // #399
  INST(Ltr              , X86M_NoSize        , O(000F00,00,3,_,_,_,_,_  ), 0                         , 78 , 0  , 107, 0  ), // #400
  INST(Lwpins           , VexVmi4_Wx         , V(XOP_MA,12,0,0,x,_,_,_  ), 0                         , 81 , 0  , 112, 77 ), // #401
  INST(Lwpval           , VexVmi4_Wx         , V(XOP_MA,12,1,0,x,_,_,_  ), 0                         , 82 , 0  , 112, 77 ), // #402
  INST(Lzcnt            , X86Rm_Raw66H       , O(F30F00,BD,_,_,x,_,_,_  ), 0                         , 6  , 0  , 22 , 79 ), // #403
  INST(Maskmovdqu       , ExtRm_ZDI          , O(660F00,F7,_,_,_,_,_,_  ), 0                         , 3  , 0  , 113, 4  ), // #404
  INST(Maskmovq         , ExtRm_ZDI          , O(000F00,F7,_,_,_,_,_,_  ), 0                         , 4  , 0  , 114, 80 ), // #405
  INST(Maxpd            , ExtRm              , O(660F00,5F,_,_,_,_,_,_  ), 0                         , 3  , 0  , 5  , 4  ), // #406
  INST(Maxps            , ExtRm              , O(000F00,5F,_,_,_,_,_,_  ), 0                         , 4  , 0  , 5  , 5  ), // #407
  INST(Maxsd            , ExtRm              , O(F20F00,5F,_,_,_,_,_,_  ), 0                         , 5  , 0  , 6  , 4  ), // #408
  INST(Maxss            , ExtRm              , O(F30F00,5F,_,_,_,_,_,_  ), 0                         , 6  , 0  , 7  , 5  ), // #409
  INST(Mcommit          , X86Op              , O(F30F01,FA,_,_,_,_,_,_  ), 0                         , 25 , 0  , 30 , 81 ), // #410
  INST(Mfence           , X86Fence           , O(000F00,AE,6,_,_,_,_,_  ), 0                         , 80 , 0  , 30 , 4  ), // #411
  INST(Minpd            , ExtRm              , O(660F00,5D,_,_,_,_,_,_  ), 0                         , 3  , 0  , 5  , 4  ), // #412
  INST(Minps            , ExtRm              , O(000F00,5D,_,_,_,_,_,_  ), 0                         , 4  , 0  , 5  , 5  ), // #413
  INST(Minsd            , ExtRm              , O(F20F00,5D,_,_,_,_,_,_  ), 0                         , 5  , 0  , 6  , 4  ), // #414
  INST(Minss            , ExtRm              , O(F30F00,5D,_,_,_,_,_,_  ), 0                         , 6  , 0  , 7  , 5  ), // #415
  INST(Monitor          , X86Op              , O(000F01,C8,_,_,_,_,_,_  ), 0                         , 21 , 0  , 115, 82 ), // #416
  INST(Monitorx         , X86Op              , O(000F01,FA,_,_,_,_,_,_  ), 0                         , 21 , 0  , 115, 83 ), // #417
  INST(Mov              , X86Mov             , 0                         , 0                         , 0  , 0  , 116, 84 ), // #418
  INST(Movabs           , X86Movabs          , 0                         , 0                         , 0  , 0  , 117, 0  ), // #419
  INST(Movapd           , ExtMov             , O(660F00,28,_,_,_,_,_,_  ), O(660F00,29,_,_,_,_,_,_  ), 3  , 45 , 118, 85 ), // #420
  INST(Movaps           , ExtMov             , O(000F00,28,_,_,_,_,_,_  ), O(000F00,29,_,_,_,_,_,_  ), 4  , 46 , 118, 86 ), // #421
  INST(Movbe            , ExtMovbe           , O(000F38,F0,_,_,x,_,_,_  ), O(000F38,F1,_,_,x,_,_,_  ), 83 , 47 , 119, 87 ), // #422
  INST(Movd             , ExtMovd            , O(000F00,6E,_,_,_,_,_,_  ), O(000F00,7E,_,_,_,_,_,_  ), 4  , 48 , 120, 88 ), // #423
  INST(Movddup          , ExtMov             , O(F20F00,12,_,_,_,_,_,_  ), 0                         , 5  , 0  , 6  , 6  ), // #424
  INST(Movdir64b        , X86EnqcmdMovdir64b , O(660F38,F8,_,_,_,_,_,_  ), 0                         , 2  , 0  , 121, 89 ), // #425
  INST(Movdiri          , X86MovntiMovdiri   , O(000F38,F9,_,_,_,_,_,_  ), 0                         , 83 , 0  , 122, 90 ), // #426
  INST(Movdq2q          , ExtMov             , O(F20F00,D6,_,_,_,_,_,_  ), 0                         , 5  , 0  , 123, 4  ), // #427
  INST(Movdqa           , ExtMov             , O(660F00,6F,_,_,_,_,_,_  ), O(660F00,7F,_,_,_,_,_,_  ), 3  , 49 , 118, 85 ), // #428
  INST(Movdqu           , ExtMov             , O(F30F00,6F,_,_,_,_,_,_  ), O(F30F00,7F,_,_,_,_,_,_  ), 6  , 50 , 118, 85 ), // #429
  INST(Movhlps          , ExtMov             , O(000F00,12,_,_,_,_,_,_  ), 0                         , 4  , 0  , 124, 5  ), // #430
  INST(Movhpd           , ExtMov             , O(660F00,16,_,_,_,_,_,_  ), O(660F00,17,_,_,_,_,_,_  ), 3  , 51 , 125, 4  ), // #431
  INST(Movhps           , ExtMov             , O(000F00,16,_,_,_,_,_,_  ), O(000F00,17,_,_,_,_,_,_  ), 4  , 52 , 125, 5  ), // #432
  INST(Movlhps          , ExtMov             , O(000F00,16,_,_,_,_,_,_  ), 0                         , 4  , 0  , 124, 5  ), // #433
  INST(Movlpd           , ExtMov             , O(660F00,12,_,_,_,_,_,_  ), O(660F00,13,_,_,_,_,_,_  ), 3  , 53 , 125, 4  ), // #434
  INST(Movlps           , ExtMov             , O(000F00,12,_,_,_,_,_,_  ), O(000F00,13,_,_,_,_,_,_  ), 4  , 54 , 125, 5  ), // #435
  INST(Movmskpd         , ExtMov             , O(660F00,50,_,_,_,_,_,_  ), 0                         , 3  , 0  , 126, 4  ), // #436
  INST(Movmskps         , ExtMov             , O(000F00,50,_,_,_,_,_,_  ), 0                         , 4  , 0  , 126, 5  ), // #437
  INST(Movntdq          , ExtMov             , 0                         , O(660F00,E7,_,_,_,_,_,_  ), 0  , 55 , 127, 4  ), // #438
  INST(Movntdqa         , ExtMov             , O(660F38,2A,_,_,_,_,_,_  ), 0                         , 2  , 0  , 100, 12 ), // #439
  INST(Movnti           , X86MovntiMovdiri   , O(000F00,C3,_,_,x,_,_,_  ), 0                         , 4  , 0  , 122, 4  ), // #440
  INST(Movntpd          , ExtMov             , 0                         , O(660F00,2B,_,_,_,_,_,_  ), 0  , 56 , 127, 4  ), // #441
  INST(Movntps          , ExtMov             , 0                         , O(000F00,2B,_,_,_,_,_,_  ), 0  , 57 , 127, 5  ), // #442
  INST(Movntq           , ExtMov             , 0                         , O(000F00,E7,_,_,_,_,_,_  ), 0  , 58 , 128, 80 ), // #443
  INST(Movntsd          , ExtMov             , 0                         , O(F20F00,2B,_,_,_,_,_,_  ), 0  , 59 , 129, 49 ), // #444
  INST(Movntss          , ExtMov             , 0                         , O(F30F00,2B,_,_,_,_,_,_  ), 0  , 60 , 130, 49 ), // #445
  INST(Movq             , ExtMovq            , O(000F00,6E,_,_,x,_,_,_  ), O(000F00,7E,_,_,x,_,_,_  ), 4  , 48 , 131, 91 ), // #446
  INST(Movq2dq          , ExtRm              , O(F30F00,D6,_,_,_,_,_,_  ), 0                         , 6  , 0  , 132, 4  ), // #447
  INST(Movs             , X86StrMm           , O(000000,A4,_,_,_,_,_,_  ), 0                         , 0  , 0  , 133, 78 ), // #448
  INST(Movsd            , ExtMov             , O(F20F00,10,_,_,_,_,_,_  ), O(F20F00,11,_,_,_,_,_,_  ), 5  , 61 , 134, 85 ), // #449
  INST(Movshdup         , ExtRm              , O(F30F00,16,_,_,_,_,_,_  ), 0                         , 6  , 0  , 5  , 6  ), // #450
  INST(Movsldup         , ExtRm              , O(F30F00,12,_,_,_,_,_,_  ), 0                         , 6  , 0  , 5  , 6  ), // #451
  INST(Movss            , ExtMov             , O(F30F00,10,_,_,_,_,_,_  ), O(F30F00,11,_,_,_,_,_,_  ), 6  , 62 , 135, 86 ), // #452
  INST(Movsx            , X86MovsxMovzx      , O(000F00,BE,_,_,x,_,_,_  ), 0                         , 4  , 0  , 136, 0  ), // #453
  INST(Movsxd           , X86Rm              , O(000000,63,_,_,x,_,_,_  ), 0                         , 0  , 0  , 137, 0  ), // #454
  INST(Movupd           , ExtMov             , O(660F00,10,_,_,_,_,_,_  ), O(660F00,11,_,_,_,_,_,_  ), 3  , 63 , 118, 85 ), // #455
  INST(Movups           , ExtMov             , O(000F00,10,_,_,_,_,_,_  ), O(000F00,11,_,_,_,_,_,_  ), 4  , 64 , 118, 86 ), // #456
  INST(Movzx            , X86MovsxMovzx      , O(000F00,B6,_,_,x,_,_,_  ), 0                         , 4  , 0  , 136, 0  ), // #457
  INST(Mpsadbw          , ExtRmi             , O(660F3A,42,_,_,_,_,_,_  ), 0                         , 8  , 0  , 8  , 12 ), // #458
  INST(Mul              , X86M_GPB_MulDiv    , O(000000,F6,4,_,x,_,_,_  ), 0                         , 9  , 0  , 54 , 1  ), // #459
  INST(Mulpd            , ExtRm              , O(660F00,59,_,_,_,_,_,_  ), 0                         , 3  , 0  , 5  , 4  ), // #460
  INST(Mulps            , ExtRm              , O(000F00,59,_,_,_,_,_,_  ), 0                         , 4  , 0  , 5  , 5  ), // #461
  INST(Mulsd            , ExtRm              , O(F20F00,59,_,_,_,_,_,_  ), 0                         , 5  , 0  , 6  , 4  ), // #462
  INST(Mulss            , ExtRm              , O(F30F00,59,_,_,_,_,_,_  ), 0                         , 6  , 0  , 7  , 5  ), // #463
  INST(Mulx             , VexRvm_ZDX_Wx      , V(F20F38,F6,_,0,x,_,_,_  ), 0                         , 84 , 0  , 138, 92 ), // #464
  INST(Mwait            , X86Op              , O(000F01,C9,_,_,_,_,_,_  ), 0                         , 21 , 0  , 139, 82 ), // #465
  INST(Mwaitx           , X86Op              , O(000F01,FB,_,_,_,_,_,_  ), 0                         , 21 , 0  , 140, 83 ), // #466
  INST(Neg              , X86M_GPB           , O(000000,F6,3,_,x,_,_,_  ), 0                         , 75 , 0  , 141, 1  ), // #467
  INST(Nop              , X86M_Nop           , O(000000,90,_,_,_,_,_,_  ), 0                         , 0  , 0  , 142, 0  ), // #468
  INST(Not              , X86M_GPB           , O(000000,F6,2,_,x,_,_,_  ), 0                         , 1  , 0  , 141, 0  ), // #469
  INST(Or               , X86Arith           , O(000000,08,1,_,x,_,_,_  ), 0                         , 31 , 0  , 143, 1  ), // #470
  INST(Orpd             , ExtRm              , O(660F00,56,_,_,_,_,_,_  ), 0                         , 3  , 0  , 11 , 4  ), // #471
  INST(Orps             , ExtRm              , O(000F00,56,_,_,_,_,_,_  ), 0                         , 4  , 0  , 11 , 5  ), // #472
  INST(Out              , X86Out             , O(000000,EE,_,_,_,_,_,_  ), O(000000,E6,_,_,_,_,_,_  ), 0  , 65 , 144, 0  ), // #473
  INST(Outs             , X86Outs            , O(000000,6E,_,_,_,_,_,_  ), 0                         , 0  , 0  , 145, 0  ), // #474
  INST(Pabsb            , ExtRm_P            , O(000F38,1C,_,_,_,_,_,_  ), 0                         , 83 , 0  , 146, 93 ), // #475
  INST(Pabsd            , ExtRm_P            , O(000F38,1E,_,_,_,_,_,_  ), 0                         , 83 , 0  , 146, 93 ), // #476
  INST(Pabsw            , ExtRm_P            , O(000F38,1D,_,_,_,_,_,_  ), 0                         , 83 , 0  , 146, 93 ), // #477
  INST(Packssdw         , ExtRm_P            , O(000F00,6B,_,_,_,_,_,_  ), 0                         , 4  , 0  , 146, 88 ), // #478
  INST(Packsswb         , ExtRm_P            , O(000F00,63,_,_,_,_,_,_  ), 0                         , 4  , 0  , 146, 88 ), // #479
  INST(Packusdw         , ExtRm              , O(660F38,2B,_,_,_,_,_,_  ), 0                         , 2  , 0  , 5  , 12 ), // #480
  INST(Packuswb         , ExtRm_P            , O(000F00,67,_,_,_,_,_,_  ), 0                         , 4  , 0  , 146, 88 ), // #481
  INST(Paddb            , ExtRm_P            , O(000F00,FC,_,_,_,_,_,_  ), 0                         , 4  , 0  , 146, 88 ), // #482
  INST(Paddd            , ExtRm_P            , O(000F00,FE,_,_,_,_,_,_  ), 0                         , 4  , 0  , 146, 88 ), // #483
  INST(Paddq            , ExtRm_P            , O(000F00,D4,_,_,_,_,_,_  ), 0                         , 4  , 0  , 146, 4  ), // #484
  INST(Paddsb           , ExtRm_P            , O(000F00,EC,_,_,_,_,_,_  ), 0                         , 4  , 0  , 146, 88 ), // #485
  INST(Paddsw           , ExtRm_P            , O(000F00,ED,_,_,_,_,_,_  ), 0                         , 4  , 0  , 146, 88 ), // #486
  INST(Paddusb          , ExtRm_P            , O(000F00,DC,_,_,_,_,_,_  ), 0                         , 4  , 0  , 146, 88 ), // #487
  INST(Paddusw          , ExtRm_P            , O(000F00,DD,_,_,_,_,_,_  ), 0                         , 4  , 0  , 146, 88 ), // #488
  INST(Paddw            , ExtRm_P            , O(000F00,FD,_,_,_,_,_,_  ), 0                         , 4  , 0  , 146, 88 ), // #489
  INST(Palignr          , ExtRmi_P           , O(000F3A,0F,_,_,_,_,_,_  ), 0                         , 85 , 0  , 147, 6  ), // #490
  INST(Pand             , ExtRm_P            , O(000F00,DB,_,_,_,_,_,_  ), 0                         , 4  , 0  , 148, 88 ), // #491
  INST(Pandn            , ExtRm_P            , O(000F00,DF,_,_,_,_,_,_  ), 0                         , 4  , 0  , 149, 88 ), // #492
  INST(Pause            , X86Op              , O(F30000,90,_,_,_,_,_,_  ), 0                         , 86 , 0  , 30 , 0  ), // #493
  INST(Pavgb            , ExtRm_P            , O(000F00,E0,_,_,_,_,_,_  ), 0                         , 4  , 0  , 146, 94 ), // #494
  INST(Pavgusb          , Ext3dNow           , O(000F0F,BF,_,_,_,_,_,_  ), 0                         , 87 , 0  , 150, 51 ), // #495
  INST(Pavgw            , ExtRm_P            , O(000F00,E3,_,_,_,_,_,_  ), 0                         , 4  , 0  , 146, 94 ), // #496
  INST(Pblendvb         , ExtRm_XMM0         , O(660F38,10,_,_,_,_,_,_  ), 0                         , 2  , 0  , 15 , 12 ), // #497
  INST(Pblendw          , ExtRmi             , O(660F3A,0E,_,_,_,_,_,_  ), 0                         , 8  , 0  , 8  , 12 ), // #498
  INST(Pclmulqdq        , ExtRmi             , O(660F3A,44,_,_,_,_,_,_  ), 0                         , 8  , 0  , 8  , 95 ), // #499
  INST(Pcmpeqb          , ExtRm_P            , O(000F00,74,_,_,_,_,_,_  ), 0                         , 4  , 0  , 149, 88 ), // #500
  INST(Pcmpeqd          , ExtRm_P            , O(000F00,76,_,_,_,_,_,_  ), 0                         , 4  , 0  , 149, 88 ), // #501
  INST(Pcmpeqq          , ExtRm              , O(660F38,29,_,_,_,_,_,_  ), 0                         , 2  , 0  , 151, 12 ), // #502
  INST(Pcmpeqw          , ExtRm_P            , O(000F00,75,_,_,_,_,_,_  ), 0                         , 4  , 0  , 149, 88 ), // #503
  INST(Pcmpestri        , ExtRmi             , O(660F3A,61,_,_,_,_,_,_  ), 0                         , 8  , 0  , 152, 96 ), // #504
  INST(Pcmpestrm        , ExtRmi             , O(660F3A,60,_,_,_,_,_,_  ), 0                         , 8  , 0  , 153, 96 ), // #505
  INST(Pcmpgtb          , ExtRm_P            , O(000F00,64,_,_,_,_,_,_  ), 0                         , 4  , 0  , 149, 88 ), // #506
  INST(Pcmpgtd          , ExtRm_P            , O(000F00,66,_,_,_,_,_,_  ), 0                         , 4  , 0  , 149, 88 ), // #507
  INST(Pcmpgtq          , ExtRm              , O(660F38,37,_,_,_,_,_,_  ), 0                         , 2  , 0  , 151, 44 ), // #508
  INST(Pcmpgtw          , ExtRm_P            , O(000F00,65,_,_,_,_,_,_  ), 0                         , 4  , 0  , 149, 88 ), // #509
  INST(Pcmpistri        , ExtRmi             , O(660F3A,63,_,_,_,_,_,_  ), 0                         , 8  , 0  , 154, 96 ), // #510
  INST(Pcmpistrm        , ExtRmi             , O(660F3A,62,_,_,_,_,_,_  ), 0                         , 8  , 0  , 155, 96 ), // #511
  INST(Pconfig          , X86Op              , O(000F01,C5,_,_,_,_,_,_  ), 0                         , 21 , 0  , 30 , 97 ), // #512
  INST(Pdep             , VexRvm_Wx          , V(F20F38,F5,_,0,x,_,_,_  ), 0                         , 84 , 0  , 10 , 92 ), // #513
  INST(Pext             , VexRvm_Wx          , V(F30F38,F5,_,0,x,_,_,_  ), 0                         , 88 , 0  , 10 , 92 ), // #514
  INST(Pextrb           , ExtExtract         , O(000F3A,14,_,_,_,_,_,_  ), 0                         , 85 , 0  , 156, 12 ), // #515
  INST(Pextrd           , ExtExtract         , O(000F3A,16,_,_,_,_,_,_  ), 0                         , 85 , 0  , 58 , 12 ), // #516
  INST(Pextrq           , ExtExtract         , O(000F3A,16,_,_,1,_,_,_  ), 0                         , 89 , 0  , 157, 12 ), // #517
  INST(Pextrw           , ExtPextrw          , O(000F00,C5,_,_,_,_,_,_  ), O(000F3A,15,_,_,_,_,_,_  ), 4  , 66 , 158, 98 ), // #518
  INST(Pf2id            , Ext3dNow           , O(000F0F,1D,_,_,_,_,_,_  ), 0                         , 87 , 0  , 150, 51 ), // #519
  INST(Pf2iw            , Ext3dNow           , O(000F0F,1C,_,_,_,_,_,_  ), 0                         , 87 , 0  , 150, 99 ), // #520
  INST(Pfacc            , Ext3dNow           , O(000F0F,AE,_,_,_,_,_,_  ), 0                         , 87 , 0  , 150, 51 ), // #521
  INST(Pfadd            , Ext3dNow           , O(000F0F,9E,_,_,_,_,_,_  ), 0                         , 87 , 0  , 150, 51 ), // #522
  INST(Pfcmpeq          , Ext3dNow           , O(000F0F,B0,_,_,_,_,_,_  ), 0                         , 87 , 0  , 150, 51 ), // #523
  INST(Pfcmpge          , Ext3dNow           , O(000F0F,90,_,_,_,_,_,_  ), 0                         , 87 , 0  , 150, 51 ), // #524
  INST(Pfcmpgt          , Ext3dNow           , O(000F0F,A0,_,_,_,_,_,_  ), 0                         , 87 , 0  , 150, 51 ), // #525
  INST(Pfmax            , Ext3dNow           , O(000F0F,A4,_,_,_,_,_,_  ), 0                         , 87 , 0  , 150, 51 ), // #526
  INST(Pfmin            , Ext3dNow           , O(000F0F,94,_,_,_,_,_,_  ), 0                         , 87 , 0  , 150, 51 ), // #527
  INST(Pfmul            , Ext3dNow           , O(000F0F,B4,_,_,_,_,_,_  ), 0                         , 87 , 0  , 150, 51 ), // #528
  INST(Pfnacc           , Ext3dNow           , O(000F0F,8A,_,_,_,_,_,_  ), 0                         , 87 , 0  , 150, 99 ), // #529
  INST(Pfpnacc          , Ext3dNow           , O(000F0F,8E,_,_,_,_,_,_  ), 0                         , 87 , 0  , 150, 99 ), // #530
  INST(Pfrcp            , Ext3dNow           , O(000F0F,96,_,_,_,_,_,_  ), 0                         , 87 , 0  , 150, 51 ), // #531
  INST(Pfrcpit1         , Ext3dNow           , O(000F0F,A6,_,_,_,_,_,_  ), 0                         , 87 , 0  , 150, 51 ), // #532
  INST(Pfrcpit2         , Ext3dNow           , O(000F0F,B6,_,_,_,_,_,_  ), 0                         , 87 , 0  , 150, 51 ), // #533
  INST(Pfrcpv           , Ext3dNow           , O(000F0F,86,_,_,_,_,_,_  ), 0                         , 87 , 0  , 150, 100), // #534
  INST(Pfrsqit1         , Ext3dNow           , O(000F0F,A7,_,_,_,_,_,_  ), 0                         , 87 , 0  , 150, 51 ), // #535
  INST(Pfrsqrt          , Ext3dNow           , O(000F0F,97,_,_,_,_,_,_  ), 0                         , 87 , 0  , 150, 51 ), // #536
  INST(Pfrsqrtv         , Ext3dNow           , O(000F0F,87,_,_,_,_,_,_  ), 0                         , 87 , 0  , 150, 100), // #537
  INST(Pfsub            , Ext3dNow           , O(000F0F,9A,_,_,_,_,_,_  ), 0                         , 87 , 0  , 150, 51 ), // #538
  INST(Pfsubr           , Ext3dNow           , O(000F0F,AA,_,_,_,_,_,_  ), 0                         , 87 , 0  , 150, 51 ), // #539
  INST(Phaddd           , ExtRm_P            , O(000F38,02,_,_,_,_,_,_  ), 0                         , 83 , 0  , 146, 93 ), // #540
  INST(Phaddsw          , ExtRm_P            , O(000F38,03,_,_,_,_,_,_  ), 0                         , 83 , 0  , 146, 93 ), // #541
  INST(Phaddw           , ExtRm_P            , O(000F38,01,_,_,_,_,_,_  ), 0                         , 83 , 0  , 146, 93 ), // #542
  INST(Phminposuw       , ExtRm              , O(660F38,41,_,_,_,_,_,_  ), 0                         , 2  , 0  , 5  , 12 ), // #543
  INST(Phsubd           , ExtRm_P            , O(000F38,06,_,_,_,_,_,_  ), 0                         , 83 , 0  , 146, 93 ), // #544
  INST(Phsubsw          , ExtRm_P            , O(000F38,07,_,_,_,_,_,_  ), 0                         , 83 , 0  , 146, 93 ), // #545
  INST(Phsubw           , ExtRm_P            , O(000F38,05,_,_,_,_,_,_  ), 0                         , 83 , 0  , 146, 93 ), // #546
  INST(Pi2fd            , Ext3dNow           , O(000F0F,0D,_,_,_,_,_,_  ), 0                         , 87 , 0  , 150, 51 ), // #547
  INST(Pi2fw            , Ext3dNow           , O(000F0F,0C,_,_,_,_,_,_  ), 0                         , 87 , 0  , 150, 99 ), // #548
  INST(Pinsrb           , ExtRmi             , O(660F3A,20,_,_,_,_,_,_  ), 0                         , 8  , 0  , 159, 12 ), // #549
  INST(Pinsrd           , ExtRmi             , O(660F3A,22,_,_,_,_,_,_  ), 0                         , 8  , 0  , 160, 12 ), // #550
  INST(Pinsrq           , ExtRmi             , O(660F3A,22,_,_,1,_,_,_  ), 0                         , 90 , 0  , 161, 12 ), // #551
  INST(Pinsrw           , ExtRmi_P           , O(000F00,C4,_,_,_,_,_,_  ), 0                         , 4  , 0  , 162, 94 ), // #552
  INST(Pmaddubsw        , ExtRm_P            , O(000F38,04,_,_,_,_,_,_  ), 0                         , 83 , 0  , 146, 93 ), // #553
  INST(Pmaddwd          , ExtRm_P            , O(000F00,F5,_,_,_,_,_,_  ), 0                         , 4  , 0  , 146, 88 ), // #554
  INST(Pmaxsb           , ExtRm              , O(660F38,3C,_,_,_,_,_,_  ), 0                         , 2  , 0  , 11 , 12 ), // #555
  INST(Pmaxsd           , ExtRm              , O(660F38,3D,_,_,_,_,_,_  ), 0                         , 2  , 0  , 11 , 12 ), // #556
  INST(Pmaxsw           , ExtRm_P            , O(000F00,EE,_,_,_,_,_,_  ), 0                         , 4  , 0  , 148, 94 ), // #557
  INST(Pmaxub           , ExtRm_P            , O(000F00,DE,_,_,_,_,_,_  ), 0                         , 4  , 0  , 148, 94 ), // #558
  INST(Pmaxud           , ExtRm              , O(660F38,3F,_,_,_,_,_,_  ), 0                         , 2  , 0  , 11 , 12 ), // #559
  INST(Pmaxuw           , ExtRm              , O(660F38,3E,_,_,_,_,_,_  ), 0                         , 2  , 0  , 11 , 12 ), // #560
  INST(Pminsb           , ExtRm              , O(660F38,38,_,_,_,_,_,_  ), 0                         , 2  , 0  , 11 , 12 ), // #561
  INST(Pminsd           , ExtRm              , O(660F38,39,_,_,_,_,_,_  ), 0                         , 2  , 0  , 11 , 12 ), // #562
  INST(Pminsw           , ExtRm_P            , O(000F00,EA,_,_,_,_,_,_  ), 0                         , 4  , 0  , 148, 94 ), // #563
  INST(Pminub           , ExtRm_P            , O(000F00,DA,_,_,_,_,_,_  ), 0                         , 4  , 0  , 148, 94 ), // #564
  INST(Pminud           , ExtRm              , O(660F38,3B,_,_,_,_,_,_  ), 0                         , 2  , 0  , 11 , 12 ), // #565
  INST(Pminuw           , ExtRm              , O(660F38,3A,_,_,_,_,_,_  ), 0                         , 2  , 0  , 11 , 12 ), // #566
  INST(Pmovmskb         , ExtRm_P            , O(000F00,D7,_,_,_,_,_,_  ), 0                         , 4  , 0  , 163, 94 ), // #567
  INST(Pmovsxbd         , ExtRm              , O(660F38,21,_,_,_,_,_,_  ), 0                         , 2  , 0  , 7  , 12 ), // #568
  INST(Pmovsxbq         , ExtRm              , O(660F38,22,_,_,_,_,_,_  ), 0                         , 2  , 0  , 164, 12 ), // #569
  INST(Pmovsxbw         , ExtRm              , O(660F38,20,_,_,_,_,_,_  ), 0                         , 2  , 0  , 6  , 12 ), // #570
  INST(Pmovsxdq         , ExtRm              , O(660F38,25,_,_,_,_,_,_  ), 0                         , 2  , 0  , 6  , 12 ), // #571
  INST(Pmovsxwd         , ExtRm              , O(660F38,23,_,_,_,_,_,_  ), 0                         , 2  , 0  , 6  , 12 ), // #572
  INST(Pmovsxwq         , ExtRm              , O(660F38,24,_,_,_,_,_,_  ), 0                         , 2  , 0  , 7  , 12 ), // #573
  INST(Pmovzxbd         , ExtRm              , O(660F38,31,_,_,_,_,_,_  ), 0                         , 2  , 0  , 7  , 12 ), // #574
  INST(Pmovzxbq         , ExtRm              , O(660F38,32,_,_,_,_,_,_  ), 0                         , 2  , 0  , 164, 12 ), // #575
  INST(Pmovzxbw         , ExtRm              , O(660F38,30,_,_,_,_,_,_  ), 0                         , 2  , 0  , 6  , 12 ), // #576
  INST(Pmovzxdq         , ExtRm              , O(660F38,35,_,_,_,_,_,_  ), 0                         , 2  , 0  , 6  , 12 ), // #577
  INST(Pmovzxwd         , ExtRm              , O(660F38,33,_,_,_,_,_,_  ), 0                         , 2  , 0  , 6  , 12 ), // #578
  INST(Pmovzxwq         , ExtRm              , O(660F38,34,_,_,_,_,_,_  ), 0                         , 2  , 0  , 7  , 12 ), // #579
  INST(Pmuldq           , ExtRm              , O(660F38,28,_,_,_,_,_,_  ), 0                         , 2  , 0  , 5  , 12 ), // #580
  INST(Pmulhrsw         , ExtRm_P            , O(000F38,0B,_,_,_,_,_,_  ), 0                         , 83 , 0  , 146, 93 ), // #581
  INST(Pmulhrw          , Ext3dNow           , O(000F0F,B7,_,_,_,_,_,_  ), 0                         , 87 , 0  , 150, 51 ), // #582
  INST(Pmulhuw          , ExtRm_P            , O(000F00,E4,_,_,_,_,_,_  ), 0                         , 4  , 0  , 146, 94 ), // #583
  INST(Pmulhw           , ExtRm_P            , O(000F00,E5,_,_,_,_,_,_  ), 0                         , 4  , 0  , 146, 88 ), // #584
  INST(Pmulld           , ExtRm              , O(660F38,40,_,_,_,_,_,_  ), 0                         , 2  , 0  , 5  , 12 ), // #585
  INST(Pmullw           , ExtRm_P            , O(000F00,D5,_,_,_,_,_,_  ), 0                         , 4  , 0  , 146, 88 ), // #586
  INST(Pmuludq          , ExtRm_P            , O(000F00,F4,_,_,_,_,_,_  ), 0                         , 4  , 0  , 146, 4  ), // #587
  INST(Pop              , X86Pop             , O(000000,8F,0,_,_,_,_,_  ), O(000000,58,_,_,_,_,_,_  ), 0  , 67 , 165, 0  ), // #588
  INST(Popa             , X86Op              , O(660000,61,_,_,_,_,_,_  ), 0                         , 19 , 0  , 81 , 0  ), // #589
  INST(Popad            , X86Op              , O(000000,61,_,_,_,_,_,_  ), 0                         , 0  , 0  , 81 , 0  ), // #590
  INST(Popcnt           , X86Rm_Raw66H       , O(F30F00,B8,_,_,x,_,_,_  ), 0                         , 6  , 0  , 22 , 101), // #591
  INST(Popf             , X86Op              , O(660000,9D,_,_,_,_,_,_  ), 0                         , 19 , 0  , 30 , 102), // #592
  INST(Popfd            , X86Op              , O(000000,9D,_,_,_,_,_,_  ), 0                         , 0  , 0  , 81 , 102), // #593
  INST(Popfq            , X86Op              , O(000000,9D,_,_,_,_,_,_  ), 0                         , 0  , 0  , 33 , 102), // #594
  INST(Por              , ExtRm_P            , O(000F00,EB,_,_,_,_,_,_  ), 0                         , 4  , 0  , 148, 88 ), // #595
  INST(Prefetch         , X86M_Only          , O(000F00,0D,0,_,_,_,_,_  ), 0                         , 4  , 0  , 31 , 51 ), // #596
  INST(Prefetchnta      , X86M_Only          , O(000F00,18,0,_,_,_,_,_  ), 0                         , 4  , 0  , 31 , 80 ), // #597
  INST(Prefetcht0       , X86M_Only          , O(000F00,18,1,_,_,_,_,_  ), 0                         , 29 , 0  , 31 , 80 ), // #598
  INST(Prefetcht1       , X86M_Only          , O(000F00,18,2,_,_,_,_,_  ), 0                         , 76 , 0  , 31 , 80 ), // #599
  INST(Prefetcht2       , X86M_Only          , O(000F00,18,3,_,_,_,_,_  ), 0                         , 78 , 0  , 31 , 80 ), // #600
  INST(Prefetchw        , X86M_Only          , O(000F00,0D,1,_,_,_,_,_  ), 0                         , 29 , 0  , 31 , 103), // #601
  INST(Prefetchwt1      , X86M_Only          , O(000F00,0D,2,_,_,_,_,_  ), 0                         , 76 , 0  , 31 , 104), // #602
  INST(Psadbw           , ExtRm_P            , O(000F00,F6,_,_,_,_,_,_  ), 0                         , 4  , 0  , 146, 94 ), // #603
  INST(Pshufb           , ExtRm_P            , O(000F38,00,_,_,_,_,_,_  ), 0                         , 83 , 0  , 146, 93 ), // #604
  INST(Pshufd           , ExtRmi             , O(660F00,70,_,_,_,_,_,_  ), 0                         , 3  , 0  , 8  , 4  ), // #605
  INST(Pshufhw          , ExtRmi             , O(F30F00,70,_,_,_,_,_,_  ), 0                         , 6  , 0  , 8  , 4  ), // #606
  INST(Pshuflw          , ExtRmi             , O(F20F00,70,_,_,_,_,_,_  ), 0                         , 5  , 0  , 8  , 4  ), // #607
  INST(Pshufw           , ExtRmi_P           , O(000F00,70,_,_,_,_,_,_  ), 0                         , 4  , 0  , 166, 80 ), // #608
  INST(Psignb           , ExtRm_P            , O(000F38,08,_,_,_,_,_,_  ), 0                         , 83 , 0  , 146, 93 ), // #609
  INST(Psignd           , ExtRm_P            , O(000F38,0A,_,_,_,_,_,_  ), 0                         , 83 , 0  , 146, 93 ), // #610
  INST(Psignw           , ExtRm_P            , O(000F38,09,_,_,_,_,_,_  ), 0                         , 83 , 0  , 146, 93 ), // #611
  INST(Pslld            , ExtRmRi_P          , O(000F00,F2,_,_,_,_,_,_  ), O(000F00,72,6,_,_,_,_,_  ), 4  , 68 , 167, 88 ), // #612
  INST(Pslldq           , ExtRmRi            , 0                         , O(660F00,73,7,_,_,_,_,_  ), 0  , 69 , 168, 4  ), // #613
  INST(Psllq            , ExtRmRi_P          , O(000F00,F3,_,_,_,_,_,_  ), O(000F00,73,6,_,_,_,_,_  ), 4  , 70 , 167, 88 ), // #614
  INST(Psllw            , ExtRmRi_P          , O(000F00,F1,_,_,_,_,_,_  ), O(000F00,71,6,_,_,_,_,_  ), 4  , 71 , 167, 88 ), // #615
  INST(Psmash           , X86Op              , O(F30F01,FF,_,_,_,_,_,_  ), 0                         , 25 , 0  , 33 , 105), // #616
  INST(Psrad            , ExtRmRi_P          , O(000F00,E2,_,_,_,_,_,_  ), O(000F00,72,4,_,_,_,_,_  ), 4  , 72 , 167, 88 ), // #617
  INST(Psraw            , ExtRmRi_P          , O(000F00,E1,_,_,_,_,_,_  ), O(000F00,71,4,_,_,_,_,_  ), 4  , 73 , 167, 88 ), // #618
  INST(Psrld            , ExtRmRi_P          , O(000F00,D2,_,_,_,_,_,_  ), O(000F00,72,2,_,_,_,_,_  ), 4  , 74 , 167, 88 ), // #619
  INST(Psrldq           , ExtRmRi            , 0                         , O(660F00,73,3,_,_,_,_,_  ), 0  , 75 , 168, 4  ), // #620
  INST(Psrlq            , ExtRmRi_P          , O(000F00,D3,_,_,_,_,_,_  ), O(000F00,73,2,_,_,_,_,_  ), 4  , 76 , 167, 88 ), // #621
  INST(Psrlw            , ExtRmRi_P          , O(000F00,D1,_,_,_,_,_,_  ), O(000F00,71,2,_,_,_,_,_  ), 4  , 77 , 167, 88 ), // #622
  INST(Psubb            , ExtRm_P            , O(000F00,F8,_,_,_,_,_,_  ), 0                         , 4  , 0  , 149, 88 ), // #623
  INST(Psubd            , ExtRm_P            , O(000F00,FA,_,_,_,_,_,_  ), 0                         , 4  , 0  , 149, 88 ), // #624
  INST(Psubq            , ExtRm_P            , O(000F00,FB,_,_,_,_,_,_  ), 0                         , 4  , 0  , 149, 4  ), // #625
  INST(Psubsb           , ExtRm_P            , O(000F00,E8,_,_,_,_,_,_  ), 0                         , 4  , 0  , 149, 88 ), // #626
  INST(Psubsw           , ExtRm_P            , O(000F00,E9,_,_,_,_,_,_  ), 0                         , 4  , 0  , 149, 88 ), // #627
  INST(Psubusb          , ExtRm_P            , O(000F00,D8,_,_,_,_,_,_  ), 0                         , 4  , 0  , 149, 88 ), // #628
  INST(Psubusw          , ExtRm_P            , O(000F00,D9,_,_,_,_,_,_  ), 0                         , 4  , 0  , 149, 88 ), // #629
  INST(Psubw            , ExtRm_P            , O(000F00,F9,_,_,_,_,_,_  ), 0                         , 4  , 0  , 149, 88 ), // #630
  INST(Pswapd           , Ext3dNow           , O(000F0F,BB,_,_,_,_,_,_  ), 0                         , 87 , 0  , 150, 99 ), // #631
  INST(Ptest            , ExtRm              , O(660F38,17,_,_,_,_,_,_  ), 0                         , 2  , 0  , 5  , 106), // #632
  INST(Ptwrite          , X86M               , O(F30F00,AE,4,_,_,_,_,_  ), 0                         , 91 , 0  , 169, 107), // #633
  INST(Punpckhbw        , ExtRm_P            , O(000F00,68,_,_,_,_,_,_  ), 0                         , 4  , 0  , 146, 88 ), // #634
  INST(Punpckhdq        , ExtRm_P            , O(000F00,6A,_,_,_,_,_,_  ), 0                         , 4  , 0  , 146, 88 ), // #635
  INST(Punpckhqdq       , ExtRm              , O(660F00,6D,_,_,_,_,_,_  ), 0                         , 3  , 0  , 5  , 4  ), // #636
  INST(Punpckhwd        , ExtRm_P            , O(000F00,69,_,_,_,_,_,_  ), 0                         , 4  , 0  , 146, 88 ), // #637
  INST(Punpcklbw        , ExtRm_P            , O(000F00,60,_,_,_,_,_,_  ), 0                         , 4  , 0  , 170, 88 ), // #638
  INST(Punpckldq        , ExtRm_P            , O(000F00,62,_,_,_,_,_,_  ), 0                         , 4  , 0  , 170, 88 ), // #639
  INST(Punpcklqdq       , ExtRm              , O(660F00,6C,_,_,_,_,_,_  ), 0                         , 3  , 0  , 5  , 4  ), // #640
  INST(Punpcklwd        , ExtRm_P            , O(000F00,61,_,_,_,_,_,_  ), 0                         , 4  , 0  , 170, 88 ), // #641
  INST(Push             , X86Push            , O(000000,FF,6,_,_,_,_,_  ), O(000000,50,_,_,_,_,_,_  ), 32 , 78 , 171, 0  ), // #642
  INST(Pusha            , X86Op              , O(660000,60,_,_,_,_,_,_  ), 0                         , 19 , 0  , 81 , 0  ), // #643
  INST(Pushad           , X86Op              , O(000000,60,_,_,_,_,_,_  ), 0                         , 0  , 0  , 81 , 0  ), // #644
  INST(Pushf            , X86Op              , O(660000,9C,_,_,_,_,_,_  ), 0                         , 19 , 0  , 30 , 108), // #645
  INST(Pushfd           , X86Op              , O(000000,9C,_,_,_,_,_,_  ), 0                         , 0  , 0  , 81 , 108), // #646
  INST(Pushfq           , X86Op              , O(000000,9C,_,_,_,_,_,_  ), 0                         , 0  , 0  , 33 , 108), // #647
  INST(Pvalidate        , X86Op              , O(F20F01,FF,_,_,_,_,_,_  ), 0                         , 92 , 0  , 30 , 109), // #648
  INST(Pxor             , ExtRm_P            , O(000F00,EF,_,_,_,_,_,_  ), 0                         , 4  , 0  , 149, 88 ), // #649
  INST(Rcl              , X86Rot             , O(000000,D0,2,_,x,_,_,_  ), 0                         , 1  , 0  , 172, 110), // #650
  INST(Rcpps            , ExtRm              , O(000F00,53,_,_,_,_,_,_  ), 0                         , 4  , 0  , 5  , 5  ), // #651
  INST(Rcpss            , ExtRm              , O(F30F00,53,_,_,_,_,_,_  ), 0                         , 6  , 0  , 7  , 5  ), // #652
  INST(Rcr              , X86Rot             , O(000000,D0,3,_,x,_,_,_  ), 0                         , 75 , 0  , 172, 110), // #653
  INST(Rdfsbase         , X86M               , O(F30F00,AE,0,_,x,_,_,_  ), 0                         , 6  , 0  , 173, 111), // #654
  INST(Rdgsbase         , X86M               , O(F30F00,AE,1,_,x,_,_,_  ), 0                         , 93 , 0  , 173, 111), // #655
  INST(Rdmsr            , X86Op              , O(000F00,32,_,_,_,_,_,_  ), 0                         , 4  , 0  , 174, 112), // #656
  INST(Rdpid            , X86R_Native        , O(F30F00,C7,7,_,_,_,_,_  ), 0                         , 94 , 0  , 175, 113), // #657
  INST(Rdpkru           , X86Op              , O(000F01,EE,_,_,_,_,_,_  ), 0                         , 21 , 0  , 174, 114), // #658
  INST(Rdpmc            , X86Op              , O(000F00,33,_,_,_,_,_,_  ), 0                         , 4  , 0  , 174, 0  ), // #659
  INST(Rdpru            , X86Op              , O(000F01,FD,_,_,_,_,_,_  ), 0                         , 21 , 0  , 174, 115), // #660
  INST(Rdrand           , X86M               , O(000F00,C7,6,_,x,_,_,_  ), 0                         , 80 , 0  , 23 , 116), // #661
  INST(Rdseed           , X86M               , O(000F00,C7,7,_,x,_,_,_  ), 0                         , 22 , 0  , 23 , 117), // #662
  INST(Rdsspd           , X86M               , O(F30F00,1E,1,_,_,_,_,_  ), 0                         , 93 , 0  , 76 , 56 ), // #663
  INST(Rdsspq           , X86M               , O(F30F00,1E,1,_,_,_,_,_  ), 0                         , 93 , 0  , 77 , 56 ), // #664
  INST(Rdtsc            , X86Op              , O(000F00,31,_,_,_,_,_,_  ), 0                         , 4  , 0  , 28 , 118), // #665
  INST(Rdtscp           , X86Op              , O(000F01,F9,_,_,_,_,_,_  ), 0                         , 21 , 0  , 174, 119), // #666
  INST(Ret              , X86Ret             , O(000000,C2,_,_,_,_,_,_  ), 0                         , 0  , 0  , 176, 0  ), // #667
  INST(Retf             , X86Ret             , O(000000,CA,_,_,x,_,_,_  ), 0                         , 0  , 0  , 177, 0  ), // #668
  INST(Rmpadjust        , X86Op              , O(F30F01,FE,_,_,_,_,_,_  ), 0                         , 25 , 0  , 33 , 105), // #669
  INST(Rmpupdate        , X86Op              , O(F20F01,FE,_,_,_,_,_,_  ), 0                         , 92 , 0  , 33 , 105), // #670
  INST(Rol              , X86Rot             , O(000000,D0,0,_,x,_,_,_  ), 0                         , 0  , 0  , 172, 120), // #671
  INST(Ror              , X86Rot             , O(000000,D0,1,_,x,_,_,_  ), 0                         , 31 , 0  , 172, 120), // #672
  INST(Rorx             , VexRmi_Wx          , V(F20F3A,F0,_,0,x,_,_,_  ), 0                         , 95 , 0  , 178, 92 ), // #673
  INST(Roundpd          , ExtRmi             , O(660F3A,09,_,_,_,_,_,_  ), 0                         , 8  , 0  , 8  , 12 ), // #674
  INST(Roundps          , ExtRmi             , O(660F3A,08,_,_,_,_,_,_  ), 0                         , 8  , 0  , 8  , 12 ), // #675
  INST(Roundsd          , ExtRmi             , O(660F3A,0B,_,_,_,_,_,_  ), 0                         , 8  , 0  , 37 , 12 ), // #676
  INST(Roundss          , ExtRmi             , O(660F3A,0A,_,_,_,_,_,_  ), 0                         , 8  , 0  , 38 , 12 ), // #677
  INST(Rsm              , X86Op              , O(000F00,AA,_,_,_,_,_,_  ), 0                         , 4  , 0  , 81 , 1  ), // #678
  INST(Rsqrtps          , ExtRm              , O(000F00,52,_,_,_,_,_,_  ), 0                         , 4  , 0  , 5  , 5  ), // #679
  INST(Rsqrtss          , ExtRm              , O(F30F00,52,_,_,_,_,_,_  ), 0                         , 6  , 0  , 7  , 5  ), // #680
  INST(Rstorssp         , X86M_Only          , O(F30F00,01,5,_,_,_,_,_  ), 0                         , 63 , 0  , 32 , 24 ), // #681
  INST(Sahf             , X86Op              , O(000000,9E,_,_,_,_,_,_  ), 0                         , 0  , 0  , 97 , 121), // #682
  INST(Sal              , X86Rot             , O(000000,D0,4,_,x,_,_,_  ), 0                         , 9  , 0  , 172, 1  ), // #683
  INST(Sar              , X86Rot             , O(000000,D0,7,_,x,_,_,_  ), 0                         , 27 , 0  , 172, 1  ), // #684
  INST(Sarx             , VexRmv_Wx          , V(F30F38,F7,_,0,x,_,_,_  ), 0                         , 88 , 0  , 13 , 92 ), // #685
  INST(Saveprevssp      , X86Op              , O(F30F01,EA,_,_,_,_,_,_  ), 0                         , 25 , 0  , 30 , 24 ), // #686
  INST(Sbb              , X86Arith           , O(000000,18,3,_,x,_,_,_  ), 0                         , 75 , 0  , 179, 2  ), // #687
  INST(Scas             , X86StrRm           , O(000000,AE,_,_,_,_,_,_  ), 0                         , 0  , 0  , 180, 37 ), // #688
  INST(Senduipi         , X86M_NoSize        , O(F30F00,C7,6,_,_,_,_,_  ), 0                         , 24 , 0  , 77 , 25 ), // #689
  INST(Serialize        , X86Op              , O(000F01,E8,_,_,_,_,_,_  ), 0                         , 21 , 0  , 30 , 122), // #690
  INST(Seta             , X86Set             , O(000F00,97,_,_,_,_,_,_  ), 0                         , 4  , 0  , 181, 59 ), // #691
  INST(Setae            , X86Set             , O(000F00,93,_,_,_,_,_,_  ), 0                         , 4  , 0  , 181, 60 ), // #692
  INST(Setb             , X86Set             , O(000F00,92,_,_,_,_,_,_  ), 0                         , 4  , 0  , 181, 60 ), // #693
  INST(Setbe            , X86Set             , O(000F00,96,_,_,_,_,_,_  ), 0                         , 4  , 0  , 181, 59 ), // #694
  INST(Setc             , X86Set             , O(000F00,92,_,_,_,_,_,_  ), 0                         , 4  , 0  , 181, 60 ), // #695
  INST(Sete             , X86Set             , O(000F00,94,_,_,_,_,_,_  ), 0                         , 4  , 0  , 181, 61 ), // #696
  INST(Setg             , X86Set             , O(000F00,9F,_,_,_,_,_,_  ), 0                         , 4  , 0  , 181, 62 ), // #697
  INST(Setge            , X86Set             , O(000F00,9D,_,_,_,_,_,_  ), 0                         , 4  , 0  , 181, 63 ), // #698
  INST(Setl             , X86Set             , O(000F00,9C,_,_,_,_,_,_  ), 0                         , 4  , 0  , 181, 63 ), // #699
  INST(Setle            , X86Set             , O(000F00,9E,_,_,_,_,_,_  ), 0                         , 4  , 0  , 181, 62 ), // #700
  INST(Setna            , X86Set             , O(000F00,96,_,_,_,_,_,_  ), 0                         , 4  , 0  , 181, 59 ), // #701
  INST(Setnae           , X86Set             , O(000F00,92,_,_,_,_,_,_  ), 0                         , 4  , 0  , 181, 60 ), // #702
  INST(Setnb            , X86Set             , O(000F00,93,_,_,_,_,_,_  ), 0                         , 4  , 0  , 181, 60 ), // #703
  INST(Setnbe           , X86Set             , O(000F00,97,_,_,_,_,_,_  ), 0                         , 4  , 0  , 181, 59 ), // #704
  INST(Setnc            , X86Set             , O(000F00,93,_,_,_,_,_,_  ), 0                         , 4  , 0  , 181, 60 ), // #705
  INST(Setne            , X86Set             , O(000F00,95,_,_,_,_,_,_  ), 0                         , 4  , 0  , 181, 61 ), // #706
  INST(Setng            , X86Set             , O(000F00,9E,_,_,_,_,_,_  ), 0                         , 4  , 0  , 181, 62 ), // #707
  INST(Setnge           , X86Set             , O(000F00,9C,_,_,_,_,_,_  ), 0                         , 4  , 0  , 181, 63 ), // #708
  INST(Setnl            , X86Set             , O(000F00,9D,_,_,_,_,_,_  ), 0                         , 4  , 0  , 181, 63 ), // #709
  INST(Setnle           , X86Set             , O(000F00,9F,_,_,_,_,_,_  ), 0                         , 4  , 0  , 181, 62 ), // #710
  INST(Setno            , X86Set             , O(000F00,91,_,_,_,_,_,_  ), 0                         , 4  , 0  , 181, 57 ), // #711
  INST(Setnp            , X86Set             , O(000F00,9B,_,_,_,_,_,_  ), 0                         , 4  , 0  , 181, 64 ), // #712
  INST(Setns            , X86Set             , O(000F00,99,_,_,_,_,_,_  ), 0                         , 4  , 0  , 181, 65 ), // #713
  INST(Setnz            , X86Set             , O(000F00,95,_,_,_,_,_,_  ), 0                         , 4  , 0  , 181, 61 ), // #714
  INST(Seto             , X86Set             , O(000F00,90,_,_,_,_,_,_  ), 0                         , 4  , 0  , 181, 57 ), // #715
  INST(Setp             , X86Set             , O(000F00,9A,_,_,_,_,_,_  ), 0                         , 4  , 0  , 181, 64 ), // #716
  INST(Setpe            , X86Set             , O(000F00,9A,_,_,_,_,_,_  ), 0                         , 4  , 0  , 181, 64 ), // #717
  INST(Setpo            , X86Set             , O(000F00,9B,_,_,_,_,_,_  ), 0                         , 4  , 0  , 181, 64 ), // #718
  INST(Sets             , X86Set             , O(000F00,98,_,_,_,_,_,_  ), 0                         , 4  , 0  , 181, 65 ), // #719
  INST(Setssbsy         , X86Op              , O(F30F01,E8,_,_,_,_,_,_  ), 0                         , 25 , 0  , 30 , 56 ), // #720
  INST(Setz             , X86Set             , O(000F00,94,_,_,_,_,_,_  ), 0                         , 4  , 0  , 181, 61 ), // #721
  INST(Sfence           , X86Fence           , O(000F00,AE,7,_,_,_,_,_  ), 0                         , 22 , 0  , 30 , 80 ), // #722
  INST(Sgdt             , X86M_Only          , O(000F00,01,0,_,_,_,_,_  ), 0                         , 4  , 0  , 69 , 0  ), // #723
  INST(Sha1msg1         , ExtRm              , O(000F38,C9,_,_,_,_,_,_  ), 0                         , 83 , 0  , 5  , 123), // #724
  INST(Sha1msg2         , ExtRm              , O(000F38,CA,_,_,_,_,_,_  ), 0                         , 83 , 0  , 5  , 123), // #725
  INST(Sha1nexte        , ExtRm              , O(000F38,C8,_,_,_,_,_,_  ), 0                         , 83 , 0  , 5  , 123), // #726
  INST(Sha1rnds4        , ExtRmi             , O(000F3A,CC,_,_,_,_,_,_  ), 0                         , 85 , 0  , 8  , 123), // #727
  INST(Sha256msg1       , ExtRm              , O(000F38,CC,_,_,_,_,_,_  ), 0                         , 83 , 0  , 5  , 123), // #728
  INST(Sha256msg2       , ExtRm              , O(000F38,CD,_,_,_,_,_,_  ), 0                         , 83 , 0  , 5  , 123), // #729
  INST(Sha256rnds2      , ExtRm_XMM0         , O(000F38,CB,_,_,_,_,_,_  ), 0                         , 83 , 0  , 15 , 123), // #730
  INST(Shl              , X86Rot             , O(000000,D0,4,_,x,_,_,_  ), 0                         , 9  , 0  , 172, 1  ), // #731
  INST(Shld             , X86ShldShrd        , O(000F00,A4,_,_,x,_,_,_  ), 0                         , 4  , 0  , 182, 1  ), // #732
  INST(Shlx             , VexRmv_Wx          , V(660F38,F7,_,0,x,_,_,_  ), 0                         , 96 , 0  , 13 , 92 ), // #733
  INST(Shr              , X86Rot             , O(000000,D0,5,_,x,_,_,_  ), 0                         , 62 , 0  , 172, 1  ), // #734
  INST(Shrd             , X86ShldShrd        , O(000F00,AC,_,_,x,_,_,_  ), 0                         , 4  , 0  , 182, 1  ), // #735
  INST(Shrx             , VexRmv_Wx          , V(F20F38,F7,_,0,x,_,_,_  ), 0                         , 84 , 0  , 13 , 92 ), // #736
  INST(Shufpd           , ExtRmi             , O(660F00,C6,_,_,_,_,_,_  ), 0                         , 3  , 0  , 8  , 4  ), // #737
  INST(Shufps           , ExtRmi             , O(000F00,C6,_,_,_,_,_,_  ), 0                         , 4  , 0  , 8  , 5  ), // #738
  INST(Sidt             , X86M_Only          , O(000F00,01,1,_,_,_,_,_  ), 0                         , 29 , 0  , 69 , 0  ), // #739
  INST(Skinit           , X86Op_xAX          , O(000F01,DE,_,_,_,_,_,_  ), 0                         , 21 , 0  , 52 , 124), // #740
  INST(Sldt             , X86M_NoMemSize     , O(000F00,00,0,_,_,_,_,_  ), 0                         , 4  , 0  , 183, 0  ), // #741
  INST(Slwpcb           , VexR_Wx            , V(XOP_M9,12,1,0,x,_,_,_  ), 0                         , 11 , 0  , 108, 77 ), // #742
  INST(Smsw             , X86M_NoMemSize     , O(000F00,01,4,_,_,_,_,_  ), 0                         , 97 , 0  , 183, 0  ), // #743
  INST(Sqrtpd           , ExtRm              , O(660F00,51,_,_,_,_,_,_  ), 0                         , 3  , 0  , 5  , 4  ), // #744
  INST(Sqrtps           , ExtRm              , O(000F00,51,_,_,_,_,_,_  ), 0                         , 4  , 0  , 5  , 5  ), // #745
  INST(Sqrtsd           , ExtRm              , O(F20F00,51,_,_,_,_,_,_  ), 0                         , 5  , 0  , 6  , 4  ), // #746
  INST(Sqrtss           , ExtRm              , O(F30F00,51,_,_,_,_,_,_  ), 0                         , 6  , 0  , 7  , 5  ), // #747
  INST(Stac             , X86Op              , O(000F01,CB,_,_,_,_,_,_  ), 0                         , 21 , 0  , 30 , 16 ), // #748
  INST(Stc              , X86Op              , O(000000,F9,_,_,_,_,_,_  ), 0                         , 0  , 0  , 30 , 17 ), // #749
  INST(Std              , X86Op              , O(000000,FD,_,_,_,_,_,_  ), 0                         , 0  , 0  , 30 , 18 ), // #750
  INST(Stgi             , X86Op              , O(000F01,DC,_,_,_,_,_,_  ), 0                         , 21 , 0  , 30 , 124), // #751
  INST(Sti              , X86Op              , O(000000,FB,_,_,_,_,_,_  ), 0                         , 0  , 0  , 30 , 23 ), // #752
  INST(Stmxcsr          , X86M_Only          , O(000F00,AE,3,_,_,_,_,_  ), 0                         , 78 , 0  , 101, 5  ), // #753
  INST(Stos             , X86StrMr           , O(000000,AA,_,_,_,_,_,_  ), 0                         , 0  , 0  , 184, 78 ), // #754
  INST(Str              , X86M_NoMemSize     , O(000F00,00,1,_,_,_,_,_  ), 0                         , 29 , 0  , 183, 0  ), // #755
  INST(Sttilecfg        , AmxCfg             , V(660F38,49,_,0,0,_,_,_  ), 0                         , 96 , 0  , 103, 76 ), // #756
  INST(Stui             , X86Op              , O(F30F01,EF,_,_,_,_,_,_  ), 0                         , 25 , 0  , 33 , 25 ), // #757
  INST(Sub              , X86Arith           , O(000000,28,5,_,x,_,_,_  ), 0                         , 62 , 0  , 179, 1  ), // #758
  INST(Subpd            , ExtRm              , O(660F00,5C,_,_,_,_,_,_  ), 0                         , 3  , 0  , 5  , 4  ), // #759
  INST(Subps            , ExtRm              , O(000F00,5C,_,_,_,_,_,_  ), 0                         , 4  , 0  , 5  , 5  ), // #760
  INST(Subsd            , ExtRm              , O(F20F00,5C,_,_,_,_,_,_  ), 0                         , 5  , 0  , 6  , 4  ), // #761
  INST(Subss            , ExtRm              , O(F30F00,5C,_,_,_,_,_,_  ), 0                         , 6  , 0  , 7  , 5  ), // #762
  INST(Swapgs           , X86Op              , O(000F01,F8,_,_,_,_,_,_  ), 0                         , 21 , 0  , 33 , 0  ), // #763
  INST(Syscall          , X86Op              , O(000F00,05,_,_,_,_,_,_  ), 0                         , 4  , 0  , 33 , 0  ), // #764
  INST(Sysenter         , X86Op              , O(000F00,34,_,_,_,_,_,_  ), 0                         , 4  , 0  , 30 , 0  ), // #765
  INST(Sysexit          , X86Op              , O(000F00,35,_,_,_,_,_,_  ), 0                         , 4  , 0  , 30 , 0  ), // #766
  INST(Sysexitq         , X86Op              , O(000F00,35,_,_,1,_,_,_  ), 0                         , 60 , 0  , 30 , 0  ), // #767
  INST(Sysret           , X86Op              , O(000F00,07,_,_,_,_,_,_  ), 0                         , 4  , 0  , 33 , 0  ), // #768
  INST(Sysretq          , X86Op              , O(000F00,07,_,_,1,_,_,_  ), 0                         , 60 , 0  , 33 , 0  ), // #769
  INST(T1mskc           , VexVm_Wx           , V(XOP_M9,01,7,0,x,_,_,_  ), 0                         , 98 , 0  , 14 , 11 ), // #770
  INST(Tdpbf16ps        , AmxRmv             , V(F30F38,5C,_,0,0,_,_,_  ), 0                         , 88 , 0  , 185, 125), // #771
  INST(Tdpbssd          , AmxRmv             , V(F20F38,5E,_,0,0,_,_,_  ), 0                         , 84 , 0  , 185, 126), // #772
  INST(Tdpbsud          , AmxRmv             , V(F30F38,5E,_,0,0,_,_,_  ), 0                         , 88 , 0  , 185, 126), // #773
  INST(Tdpbusd          , AmxRmv             , V(660F38,5E,_,0,0,_,_,_  ), 0                         , 96 , 0  , 185, 126), // #774
  INST(Tdpbuud          , AmxRmv             , V(000F38,5E,_,0,0,_,_,_  ), 0                         , 10 , 0  , 185, 126), // #775
  INST(Test             , X86Test            , O(000000,84,_,_,x,_,_,_  ), O(000000,F6,_,_,x,_,_,_  ), 0  , 79 , 186, 1  ), // #776
  INST(Testui           , X86Op              , O(F30F01,ED,_,_,_,_,_,_  ), 0                         , 25 , 0  , 33 , 127), // #777
  INST(Tileloadd        , AmxRm              , V(F20F38,4B,_,0,0,_,_,_  ), 0                         , 84 , 0  , 187, 76 ), // #778
  INST(Tileloaddt1      , AmxRm              , V(660F38,4B,_,0,0,_,_,_  ), 0                         , 96 , 0  , 187, 76 ), // #779
  INST(Tilerelease      , VexOpMod           , V(000F38,49,0,0,0,_,_,_  ), 0                         , 10 , 0  , 188, 76 ), // #780
  INST(Tilestored       , AmxMr              , V(F30F38,4B,_,0,0,_,_,_  ), 0                         , 88 , 0  , 189, 76 ), // #781
  INST(Tilezero         , AmxR               , V(F20F38,49,_,0,0,_,_,_  ), 0                         , 84 , 0  , 190, 76 ), // #782
  INST(Tpause           , X86R32_EDX_EAX     , O(660F00,AE,6,_,_,_,_,_  ), 0                         , 26 , 0  , 191, 128), // #783
  INST(Tzcnt            , X86Rm_Raw66H       , O(F30F00,BC,_,_,x,_,_,_  ), 0                         , 6  , 0  , 22 , 9  ), // #784
  INST(Tzmsk            , VexVm_Wx           , V(XOP_M9,01,4,0,x,_,_,_  ), 0                         , 99 , 0  , 14 , 11 ), // #785
  INST(Ucomisd          , ExtRm              , O(660F00,2E,_,_,_,_,_,_  ), 0                         , 3  , 0  , 6  , 41 ), // #786
  INST(Ucomiss          , ExtRm              , O(000F00,2E,_,_,_,_,_,_  ), 0                         , 4  , 0  , 7  , 42 ), // #787
  INST(Ud0              , X86Rm              , O(000F00,FF,_,_,_,_,_,_  ), 0                         , 4  , 0  , 192, 0  ), // #788
  INST(Ud1              , X86Rm              , O(000F00,B9,_,_,_,_,_,_  ), 0                         , 4  , 0  , 192, 0  ), // #789
  INST(Ud2              , X86Op              , O(000F00,0B,_,_,_,_,_,_  ), 0                         , 4  , 0  , 30 , 0  ), // #790
  INST(Uiret            , X86Op              , O(F30F01,EC,_,_,_,_,_,_  ), 0                         , 25 , 0  , 33 , 25 ), // #791
  INST(Umonitor         , X86R_FromM         , O(F30F00,AE,6,_,_,_,_,_  ), 0                         , 24 , 0  , 193, 129), // #792
  INST(Umwait           , X86R32_EDX_EAX     , O(F20F00,AE,6,_,_,_,_,_  ), 0                         , 100, 0  , 191, 128), // #793
  INST(Unpckhpd         , ExtRm              , O(660F00,15,_,_,_,_,_,_  ), 0                         , 3  , 0  , 5  , 4  ), // #794
  INST(Unpckhps         , ExtRm              , O(000F00,15,_,_,_,_,_,_  ), 0                         , 4  , 0  , 5  , 5  ), // #795
  INST(Unpcklpd         , ExtRm              , O(660F00,14,_,_,_,_,_,_  ), 0                         , 3  , 0  , 5  , 4  ), // #796
  INST(Unpcklps         , ExtRm              , O(000F00,14,_,_,_,_,_,_  ), 0                         , 4  , 0  , 5  , 5  ), // #797
  INST(V4fmaddps        , VexRm_T1_4X        , E(F20F38,9A,_,2,_,0,4,T4X), 0                         , 101, 0  , 194, 130), // #798
  INST(V4fmaddss        , VexRm_T1_4X        , E(F20F38,9B,_,0,_,0,4,T4X), 0                         , 102, 0  , 195, 130), // #799
  INST(V4fnmaddps       , VexRm_T1_4X        , E(F20F38,AA,_,2,_,0,4,T4X), 0                         , 101, 0  , 194, 130), // #800
  INST(V4fnmaddss       , VexRm_T1_4X        , E(F20F38,AB,_,0,_,0,4,T4X), 0                         , 102, 0  , 195, 130), // #801
  INST(Vaddpd           , VexRvm_Lx          , V(660F00,58,_,x,I,1,4,FV ), 0                         , 103, 0  , 196, 131), // #802
  INST(Vaddph           , VexRvm_Lx          , E(00MAP5,58,_,_,_,0,4,FV ), 0                         , 104, 0  , 197, 132), // #803
  INST(Vaddps           , VexRvm_Lx          , V(000F00,58,_,x,I,0,4,FV ), 0                         , 105, 0  , 198, 131), // #804
  INST(Vaddsd           , VexRvm             , V(F20F00,58,_,I,I,1,3,T1S), 0                         , 106, 0  , 199, 133), // #805
  INST(Vaddsh           , VexRvm             , E(F3MAP5,58,_,_,_,0,1,T1S), 0                         , 107, 0  , 200, 134), // #806
  INST(Vaddss           , VexRvm             , V(F30F00,58,_,I,I,0,2,T1S), 0                         , 108, 0  , 201, 133), // #807
  INST(Vaddsubpd        , VexRvm_Lx          , V(660F00,D0,_,x,I,_,_,_  ), 0                         , 69 , 0  , 202, 135), // #808
  INST(Vaddsubps        , VexRvm_Lx          , V(F20F00,D0,_,x,I,_,_,_  ), 0                         , 109, 0  , 202, 135), // #809
  INST(Vaesdec          , VexRvm_Lx          , V(660F38,DE,_,x,I,_,4,FVM), 0                         , 110, 0  , 203, 136), // #810
  INST(Vaesdeclast      , VexRvm_Lx          , V(660F38,DF,_,x,I,_,4,FVM), 0                         , 110, 0  , 203, 136), // #811
  INST(Vaesenc          , VexRvm_Lx          , V(660F38,DC,_,x,I,_,4,FVM), 0                         , 110, 0  , 203, 136), // #812
  INST(Vaesenclast      , VexRvm_Lx          , V(660F38,DD,_,x,I,_,4,FVM), 0                         , 110, 0  , 203, 136), // #813
  INST(Vaesimc          , VexRm              , V(660F38,DB,_,0,I,_,_,_  ), 0                         , 96 , 0  , 204, 137), // #814
  INST(Vaeskeygenassist , VexRmi             , V(660F3A,DF,_,0,I,_,_,_  ), 0                         , 73 , 0  , 205, 137), // #815
  INST(Valignd          , VexRvmi_Lx         , E(660F3A,03,_,x,_,0,4,FV ), 0                         , 111, 0  , 206, 138), // #816
  INST(Valignq          , VexRvmi_Lx         , E(660F3A,03,_,x,_,1,4,FV ), 0                         , 112, 0  , 207, 138), // #817
  INST(Vandnpd          , VexRvm_Lx          , V(660F00,55,_,x,I,1,4,FV ), 0                         , 103, 0  , 208, 139), // #818
  INST(Vandnps          , VexRvm_Lx          , V(000F00,55,_,x,I,0,4,FV ), 0                         , 105, 0  , 209, 139), // #819
  INST(Vandpd           , VexRvm_Lx          , V(660F00,54,_,x,I,1,4,FV ), 0                         , 103, 0  , 210, 139), // #820
  INST(Vandps           , VexRvm_Lx          , V(000F00,54,_,x,I,0,4,FV ), 0                         , 105, 0  , 211, 139), // #821
  INST(Vblendmpd        , VexRvm_Lx          , E(660F38,65,_,x,_,1,4,FV ), 0                         , 113, 0  , 212, 138), // #822
  INST(Vblendmps        , VexRvm_Lx          , E(660F38,65,_,x,_,0,4,FV ), 0                         , 114, 0  , 213, 138), // #823
  INST(Vblendpd         , VexRvmi_Lx         , V(660F3A,0D,_,x,I,_,_,_  ), 0                         , 73 , 0  , 214, 135), // #824
  INST(Vblendps         , VexRvmi_Lx         , V(660F3A,0C,_,x,I,_,_,_  ), 0                         , 73 , 0  , 214, 135), // #825
  INST(Vblendvpd        , VexRvmr_Lx         , V(660F3A,4B,_,x,0,_,_,_  ), 0                         , 73 , 0  , 215, 135), // #826
  INST(Vblendvps        , VexRvmr_Lx         , V(660F3A,4A,_,x,0,_,_,_  ), 0                         , 73 , 0  , 215, 135), // #827
  INST(Vbroadcastf128   , VexRm              , V(660F38,1A,_,1,0,_,_,_  ), 0                         , 115, 0  , 216, 135), // #828
  INST(Vbroadcastf32x2  , VexRm_Lx           , E(660F38,19,_,x,_,0,3,T2 ), 0                         , 116, 0  , 217, 140), // #829
  INST(Vbroadcastf32x4  , VexRm_Lx           , E(660F38,1A,_,x,_,0,4,T4 ), 0                         , 117, 0  , 218, 68 ), // #830
  INST(Vbroadcastf32x8  , VexRm              , E(660F38,1B,_,2,_,0,5,T8 ), 0                         , 118, 0  , 219, 66 ), // #831
  INST(Vbroadcastf64x2  , VexRm_Lx           , E(660F38,1A,_,x,_,1,4,T2 ), 0                         , 119, 0  , 218, 140), // #832
  INST(Vbroadcastf64x4  , VexRm              , E(660F38,1B,_,2,_,1,5,T4 ), 0                         , 120, 0  , 219, 68 ), // #833
  INST(Vbroadcasti128   , VexRm              , V(660F38,5A,_,1,0,_,_,_  ), 0                         , 115, 0  , 216, 141), // #834
  INST(Vbroadcasti32x2  , VexRm_Lx           , E(660F38,59,_,x,_,0,3,T2 ), 0                         , 116, 0  , 220, 140), // #835
  INST(Vbroadcasti32x4  , VexRm_Lx           , E(660F38,5A,_,x,_,0,4,T4 ), 0                         , 117, 0  , 218, 138), // #836
  INST(Vbroadcasti32x8  , VexRm              , E(660F38,5B,_,2,_,0,5,T8 ), 0                         , 118, 0  , 219, 66 ), // #837
  INST(Vbroadcasti64x2  , VexRm_Lx           , E(660F38,5A,_,x,_,1,4,T2 ), 0                         , 119, 0  , 218, 140), // #838
  INST(Vbroadcasti64x4  , VexRm              , E(660F38,5B,_,2,_,1,5,T4 ), 0                         , 120, 0  , 219, 68 ), // #839
  INST(Vbroadcastsd     , VexRm_Lx           , V(660F38,19,_,x,0,1,3,T1S), 0                         , 121, 0  , 221, 142), // #840
  INST(Vbroadcastss     , VexRm_Lx           , V(660F38,18,_,x,0,0,2,T1S), 0                         , 122, 0  , 222, 142), // #841
  INST(Vcmppd           , VexRvmi_Lx_KEvex   , V(660F00,C2,_,x,I,1,4,FV ), 0                         , 103, 0  , 223, 131), // #842
  INST(Vcmpph           , VexRvmi_Lx_KEvex   , E(000F3A,C2,_,_,_,0,4,FV ), 0                         , 123, 0  , 224, 132), // #843
  INST(Vcmpps           , VexRvmi_Lx_KEvex   , V(000F00,C2,_,x,I,0,4,FV ), 0                         , 105, 0  , 225, 131), // #844
  INST(Vcmpsd           , VexRvmi_KEvex      , V(F20F00,C2,_,I,I,1,3,T1S), 0                         , 106, 0  , 226, 133), // #845
  INST(Vcmpsh           , VexRvmi_KEvex      , E(F30F3A,C2,_,_,_,0,1,T1S), 0                         , 124, 0  , 227, 134), // #846
  INST(Vcmpss           , VexRvmi_KEvex      , V(F30F00,C2,_,I,I,0,2,T1S), 0                         , 108, 0  , 228, 133), // #847
  INST(Vcomisd          , VexRm              , V(660F00,2F,_,I,I,1,3,T1S), 0                         , 125, 0  , 229, 143), // #848
  INST(Vcomish          , VexRm              , E(00MAP5,2F,_,_,_,0,1,T1S), 0                         , 126, 0  , 230, 134), // #849
  INST(Vcomiss          , VexRm              , V(000F00,2F,_,I,I,0,2,T1S), 0                         , 127, 0  , 231, 143), // #850
  INST(Vcompresspd      , VexMr_Lx           , E(660F38,8A,_,x,_,1,3,T1S), 0                         , 128, 0  , 232, 138), // #851
  INST(Vcompressps      , VexMr_Lx           , E(660F38,8A,_,x,_,0,2,T1S), 0                         , 129, 0  , 232, 138), // #852
  INST(Vcvtdq2pd        , VexRm_Lx           , V(F30F00,E6,_,x,I,0,3,HV ), 0                         , 130, 0  , 233, 131), // #853
  INST(Vcvtdq2ph        , VexRm_Lx           , E(00MAP5,5B,_,_,_,0,4,FV ), 0                         , 104, 0  , 234, 132), // #854
  INST(Vcvtdq2ps        , VexRm_Lx           , V(000F00,5B,_,x,I,0,4,FV ), 0                         , 105, 0  , 235, 131), // #855
  INST(Vcvtne2ps2bf16   , VexRvm_Lx          , E(F20F38,72,_,_,_,0,4,FV ), 0                         , 131, 0  , 213, 144), // #856
  INST(Vcvtneps2bf16    , VexRm_Lx_Narrow    , E(F30F38,72,_,_,_,0,4,FV ), 0                         , 132, 0  , 236, 144), // #857
  INST(Vcvtpd2dq        , VexRm_Lx_Narrow    , V(F20F00,E6,_,x,I,1,4,FV ), 0                         , 133, 0  , 237, 131), // #858
  INST(Vcvtpd2ph        , VexRm_Lx           , E(66MAP5,5A,_,_,_,1,4,FV ), 0                         , 134, 0  , 238, 132), // #859
  INST(Vcvtpd2ps        , VexRm_Lx_Narrow    , V(660F00,5A,_,x,I,1,4,FV ), 0                         , 103, 0  , 237, 131), // #860
  INST(Vcvtpd2qq        , VexRm_Lx           , E(660F00,7B,_,x,_,1,4,FV ), 0                         , 135, 0  , 239, 140), // #861
  INST(Vcvtpd2udq       , VexRm_Lx_Narrow    , E(000F00,79,_,x,_,1,4,FV ), 0                         , 136, 0  , 240, 138), // #862
  INST(Vcvtpd2uqq       , VexRm_Lx           , E(660F00,79,_,x,_,1,4,FV ), 0                         , 135, 0  , 239, 140), // #863
  INST(Vcvtph2dq        , VexRm_Lx           , E(66MAP5,5B,_,_,_,0,3,HV ), 0                         , 137, 0  , 241, 132), // #864
  INST(Vcvtph2pd        , VexRm_Lx           , E(00MAP5,5A,_,_,_,0,2,QV ), 0                         , 138, 0  , 242, 132), // #865
  INST(Vcvtph2ps        , VexRm_Lx           , V(660F38,13,_,x,0,0,3,HVM), 0                         , 139, 0  , 243, 145), // #866
  INST(Vcvtph2psx       , VexRm_Lx           , E(66MAP6,13,_,_,_,0,3,HV ), 0                         , 140, 0  , 244, 132), // #867
  INST(Vcvtph2qq        , VexRm_Lx           , E(66MAP5,7B,_,_,_,0,2,QV ), 0                         , 141, 0  , 245, 132), // #868
  INST(Vcvtph2udq       , VexRm_Lx           , E(00MAP5,79,_,_,_,0,3,HV ), 0                         , 142, 0  , 241, 132), // #869
  INST(Vcvtph2uqq       , VexRm_Lx           , E(66MAP5,79,_,_,_,0,2,QV ), 0                         , 141, 0  , 245, 132), // #870
  INST(Vcvtph2uw        , VexRm_Lx           , E(00MAP5,7D,_,_,_,0,4,FV ), 0                         , 104, 0  , 246, 132), // #871
  INST(Vcvtph2w         , VexRm_Lx           , E(66MAP5,7D,_,_,_,0,4,FV ), 0                         , 143, 0  , 246, 132), // #872
  INST(Vcvtps2dq        , VexRm_Lx           , V(660F00,5B,_,x,I,0,4,FV ), 0                         , 144, 0  , 235, 131), // #873
  INST(Vcvtps2pd        , VexRm_Lx           , V(000F00,5A,_,x,I,0,3,HV ), 0                         , 145, 0  , 247, 131), // #874
  INST(Vcvtps2ph        , VexMri_Lx          , V(660F3A,1D,_,x,0,0,3,HVM), 0                         , 146, 0  , 248, 145), // #875
  INST(Vcvtps2phx       , VexRm_Lx           , E(66MAP5,1D,_,_,_,0,4,FV ), 0                         , 143, 0  , 234, 132), // #876
  INST(Vcvtps2qq        , VexRm_Lx           , E(660F00,7B,_,x,_,0,3,HV ), 0                         , 147, 0  , 249, 140), // #877
  INST(Vcvtps2udq       , VexRm_Lx           , E(000F00,79,_,x,_,0,4,FV ), 0                         , 148, 0  , 250, 138), // #878
  INST(Vcvtps2uqq       , VexRm_Lx           , E(660F00,79,_,x,_,0,3,HV ), 0                         , 147, 0  , 249, 140), // #879
  INST(Vcvtqq2pd        , VexRm_Lx           , E(F30F00,E6,_,x,_,1,4,FV ), 0                         , 149, 0  , 239, 140), // #880
  INST(Vcvtqq2ph        , VexRm_Lx           , E(00MAP5,5B,_,_,_,1,4,FV ), 0                         , 150, 0  , 238, 132), // #881
  INST(Vcvtqq2ps        , VexRm_Lx_Narrow    , E(000F00,5B,_,x,_,1,4,FV ), 0                         , 136, 0  , 240, 140), // #882
  INST(Vcvtsd2sh        , VexRvm             , E(F2MAP5,5A,_,_,_,1,3,T1S), 0                         , 151, 0  , 251, 134), // #883
  INST(Vcvtsd2si        , VexRm_Wx           , V(F20F00,2D,_,I,x,x,3,T1F), 0                         , 152, 0  , 252, 133), // #884
  INST(Vcvtsd2ss        , VexRvm             , V(F20F00,5A,_,I,I,1,3,T1S), 0                         , 106, 0  , 199, 133), // #885
  INST(Vcvtsd2usi       , VexRm_Wx           , E(F20F00,79,_,I,_,x,3,T1F), 0                         , 153, 0  , 253, 68 ), // #886
  INST(Vcvtsh2sd        , VexRvm             , E(F3MAP5,5A,_,_,_,0,1,T1S), 0                         , 107, 0  , 254, 134), // #887
  INST(Vcvtsh2si        , VexRm_Wx           , E(F3MAP5,2D,_,_,_,x,1,T1S), 0                         , 107, 0  , 255, 134), // #888
  INST(Vcvtsh2ss        , VexRvm             , E(00MAP6,13,_,_,_,0,1,T1S), 0                         , 154, 0  , 254, 134), // #889
  INST(Vcvtsh2usi       , VexRm_Wx           , E(F3MAP5,79,_,_,_,x,1,T1S), 0                         , 107, 0  , 255, 134), // #890
  INST(Vcvtsi2sd        , VexRvm_Wx          , V(F20F00,2A,_,I,x,x,2,T1W), 0                         , 155, 0  , 256, 133), // #891
  INST(Vcvtsi2sh        , VexRvm_Wx          , E(F3MAP5,2A,_,_,_,x,2,T1W), 0                         , 156, 0  , 257, 134), // #892
  INST(Vcvtsi2ss        , VexRvm_Wx          , V(F30F00,2A,_,I,x,x,2,T1W), 0                         , 157, 0  , 256, 133), // #893
  INST(Vcvtss2sd        , VexRvm             , V(F30F00,5A,_,I,I,0,2,T1S), 0                         , 108, 0  , 258, 133), // #894
  INST(Vcvtss2sh        , VexRvm             , E(00MAP5,1D,_,_,_,0,2,T1S), 0                         , 158, 0  , 259, 134), // #895
  INST(Vcvtss2si        , VexRm_Wx           , V(F30F00,2D,_,I,x,x,2,T1F), 0                         , 108, 0  , 260, 133), // #896
  INST(Vcvtss2usi       , VexRm_Wx           , E(F30F00,79,_,I,_,x,2,T1F), 0                         , 159, 0  , 261, 68 ), // #897
  INST(Vcvttpd2dq       , VexRm_Lx_Narrow    , V(660F00,E6,_,x,I,1,4,FV ), 0                         , 103, 0  , 262, 131), // #898
  INST(Vcvttpd2qq       , VexRm_Lx           , E(660F00,7A,_,x,_,1,4,FV ), 0                         , 135, 0  , 263, 138), // #899
  INST(Vcvttpd2udq      , VexRm_Lx_Narrow    , E(000F00,78,_,x,_,1,4,FV ), 0                         , 136, 0  , 264, 138), // #900
  INST(Vcvttpd2uqq      , VexRm_Lx           , E(660F00,78,_,x,_,1,4,FV ), 0                         , 135, 0  , 263, 140), // #901
  INST(Vcvttph2dq       , VexRm_Lx           , E(F3MAP5,5B,_,_,_,0,3,HV ), 0                         , 160, 0  , 244, 132), // #902
  INST(Vcvttph2qq       , VexRm_Lx           , E(66MAP5,7A,_,_,_,0,2,QV ), 0                         , 141, 0  , 242, 132), // #903
  INST(Vcvttph2udq      , VexRm_Lx           , E(00MAP5,78,_,_,_,0,3,HV ), 0                         , 142, 0  , 244, 132), // #904
  INST(Vcvttph2uqq      , VexRm_Lx           , E(66MAP5,78,_,_,_,0,2,QV ), 0                         , 141, 0  , 242, 132), // #905
  INST(Vcvttph2uw       , VexRm_Lx           , E(00MAP5,7C,_,_,_,0,4,FV ), 0                         , 104, 0  , 265, 132), // #906
  INST(Vcvttph2w        , VexRm_Lx           , E(66MAP5,7C,_,_,_,0,4,FV ), 0                         , 143, 0  , 265, 132), // #907
  INST(Vcvttps2dq       , VexRm_Lx           , V(F30F00,5B,_,x,I,0,4,FV ), 0                         , 161, 0  , 266, 131), // #908
  INST(Vcvttps2qq       , VexRm_Lx           , E(660F00,7A,_,x,_,0,3,HV ), 0                         , 147, 0  , 267, 140), // #909
  INST(Vcvttps2udq      , VexRm_Lx           , E(000F00,78,_,x,_,0,4,FV ), 0                         , 148, 0  , 268, 138), // #910
  INST(Vcvttps2uqq      , VexRm_Lx           , E(660F00,78,_,x,_,0,3,HV ), 0                         , 147, 0  , 267, 140), // #911
  INST(Vcvttsd2si       , VexRm_Wx           , V(F20F00,2C,_,I,x,x,3,T1F), 0                         , 152, 0  , 269, 133), // #912
  INST(Vcvttsd2usi      , VexRm_Wx           , E(F20F00,78,_,I,_,x,3,T1F), 0                         , 153, 0  , 270, 68 ), // #913
  INST(Vcvttsh2si       , VexRm_Wx           , E(F3MAP5,2C,_,_,_,x,1,T1S), 0                         , 107, 0  , 271, 134), // #914
  INST(Vcvttsh2usi      , VexRm_Wx           , E(F3MAP5,78,_,_,_,x,1,T1S), 0                         , 107, 0  , 271, 134), // #915
  INST(Vcvttss2si       , VexRm_Wx           , V(F30F00,2C,_,I,x,x,2,T1F), 0                         , 108, 0  , 272, 133), // #916
  INST(Vcvttss2usi      , VexRm_Wx           , E(F30F00,78,_,I,_,x,2,T1F), 0                         , 159, 0  , 273, 68 ), // #917
  INST(Vcvtudq2pd       , VexRm_Lx           , E(F30F00,7A,_,x,_,0,3,HV ), 0                         , 162, 0  , 274, 138), // #918
  INST(Vcvtudq2ph       , VexRm_Lx           , E(F2MAP5,7A,_,_,_,0,4,FV ), 0                         , 163, 0  , 234, 132), // #919
  INST(Vcvtudq2ps       , VexRm_Lx           , E(F20F00,7A,_,x,_,0,4,FV ), 0                         , 164, 0  , 250, 138), // #920
  INST(Vcvtuqq2pd       , VexRm_Lx           , E(F30F00,7A,_,x,_,1,4,FV ), 0                         , 149, 0  , 239, 140), // #921
  INST(Vcvtuqq2ph       , VexRm_Lx           , E(F2MAP5,7A,_,_,_,1,4,FV ), 0                         , 165, 0  , 238, 132), // #922
  INST(Vcvtuqq2ps       , VexRm_Lx_Narrow    , E(F20F00,7A,_,x,_,1,4,FV ), 0                         , 166, 0  , 240, 140), // #923
  INST(Vcvtusi2sd       , VexRvm_Wx          , E(F20F00,7B,_,I,_,x,2,T1W), 0                         , 167, 0  , 257, 68 ), // #924
  INST(Vcvtusi2sh       , VexRvm_Wx          , E(F3MAP5,7B,_,_,_,x,2,T1W), 0                         , 156, 0  , 257, 134), // #925
  INST(Vcvtusi2ss       , VexRvm_Wx          , E(F30F00,7B,_,I,_,x,2,T1W), 0                         , 168, 0  , 257, 68 ), // #926
  INST(Vcvtuw2ph        , VexRm_Lx           , E(F2MAP5,7D,_,_,_,0,4,FV ), 0                         , 163, 0  , 246, 132), // #927
  INST(Vcvtw2ph         , VexRm_Lx           , E(F3MAP5,7D,_,_,_,0,4,FV ), 0                         , 169, 0  , 246, 132), // #928
  INST(Vdbpsadbw        , VexRvmi_Lx         , E(660F3A,42,_,x,_,0,4,FVM), 0                         , 111, 0  , 275, 146), // #929
  INST(Vdivpd           , VexRvm_Lx          , V(660F00,5E,_,x,I,1,4,FV ), 0                         , 103, 0  , 196, 131), // #930
  INST(Vdivph           , VexRvm_Lx          , E(00MAP5,5E,_,_,_,0,4,FV ), 0                         , 104, 0  , 197, 132), // #931
  INST(Vdivps           , VexRvm_Lx          , V(000F00,5E,_,x,I,0,4,FV ), 0                         , 105, 0  , 198, 131), // #932
  INST(Vdivsd           , VexRvm             , V(F20F00,5E,_,I,I,1,3,T1S), 0                         , 106, 0  , 199, 133), // #933
  INST(Vdivsh           , VexRvm             , E(F3MAP5,5E,_,_,_,0,1,T1S), 0                         , 107, 0  , 200, 134), // #934
  INST(Vdivss           , VexRvm             , V(F30F00,5E,_,I,I,0,2,T1S), 0                         , 108, 0  , 201, 133), // #935
  INST(Vdpbf16ps        , VexRvm_Lx          , E(F30F38,52,_,_,_,0,4,FV ), 0                         , 132, 0  , 213, 144), // #936
  INST(Vdppd            , VexRvmi_Lx         , V(660F3A,41,_,x,I,_,_,_  ), 0                         , 73 , 0  , 276, 135), // #937
  INST(Vdpps            , VexRvmi_Lx         , V(660F3A,40,_,x,I,_,_,_  ), 0                         , 73 , 0  , 214, 135), // #938
  INST(Verr             , X86M_NoSize        , O(000F00,00,4,_,_,_,_,_  ), 0                         , 97 , 0  , 107, 10 ), // #939
  INST(Verw             , X86M_NoSize        , O(000F00,00,5,_,_,_,_,_  ), 0                         , 77 , 0  , 107, 10 ), // #940
  INST(Vexp2pd          , VexRm              , E(660F38,C8,_,2,_,1,4,FV ), 0                         , 170, 0  , 277, 147), // #941
  INST(Vexp2ps          , VexRm              , E(660F38,C8,_,2,_,0,4,FV ), 0                         , 171, 0  , 278, 147), // #942
  INST(Vexpandpd        , VexRm_Lx           , E(660F38,88,_,x,_,1,3,T1S), 0                         , 128, 0  , 279, 138), // #943
  INST(Vexpandps        , VexRm_Lx           , E(660F38,88,_,x,_,0,2,T1S), 0                         , 129, 0  , 279, 138), // #944
  INST(Vextractf128     , VexMri             , V(660F3A,19,_,1,0,_,_,_  ), 0                         , 172, 0  , 280, 135), // #945
  INST(Vextractf32x4    , VexMri_Lx          , E(660F3A,19,_,x,_,0,4,T4 ), 0                         , 173, 0  , 281, 138), // #946
  INST(Vextractf32x8    , VexMri             , E(660F3A,1B,_,2,_,0,5,T8 ), 0                         , 174, 0  , 282, 66 ), // #947
  INST(Vextractf64x2    , VexMri_Lx          , E(660F3A,19,_,x,_,1,4,T2 ), 0                         , 175, 0  , 281, 140), // #948
  INST(Vextractf64x4    , VexMri             , E(660F3A,1B,_,2,_,1,5,T4 ), 0                         , 176, 0  , 282, 68 ), // #949
  INST(Vextracti128     , VexMri             , V(660F3A,39,_,1,0,_,_,_  ), 0                         , 172, 0  , 280, 141), // #950
  INST(Vextracti32x4    , VexMri_Lx          , E(660F3A,39,_,x,_,0,4,T4 ), 0                         , 173, 0  , 281, 138), // #951
  INST(Vextracti32x8    , VexMri             , E(660F3A,3B,_,2,_,0,5,T8 ), 0                         , 174, 0  , 282, 66 ), // #952
  INST(Vextracti64x2    , VexMri_Lx          , E(660F3A,39,_,x,_,1,4,T2 ), 0                         , 175, 0  , 281, 140), // #953
  INST(Vextracti64x4    , VexMri             , E(660F3A,3B,_,2,_,1,5,T4 ), 0                         , 176, 0  , 282, 68 ), // #954
  INST(Vextractps       , VexMri             , V(660F3A,17,_,0,I,I,2,T1S), 0                         , 177, 0  , 283, 133), // #955
  INST(Vfcmaddcph       , VexRvm_Lx          , E(F2MAP6,56,_,_,_,0,4,FV ), 0                         , 178, 0  , 284, 132), // #956
  INST(Vfcmaddcsh       , VexRvm             , E(F2MAP6,57,_,_,_,0,2,T1S), 0                         , 179, 0  , 259, 132), // #957
  INST(Vfcmulcph        , VexRvm_Lx          , E(F2MAP6,D6,_,_,_,0,4,FV ), 0                         , 178, 0  , 284, 132), // #958
  INST(Vfcmulcsh        , VexRvm             , E(F2MAP6,D7,_,_,_,0,2,T1S), 0                         , 179, 0  , 259, 132), // #959
  INST(Vfixupimmpd      , VexRvmi_Lx         , E(660F3A,54,_,x,_,1,4,FV ), 0                         , 112, 0  , 285, 138), // #960
  INST(Vfixupimmps      , VexRvmi_Lx         , E(660F3A,54,_,x,_,0,4,FV ), 0                         , 111, 0  , 286, 138), // #961
  INST(Vfixupimmsd      , VexRvmi            , E(660F3A,55,_,I,_,1,3,T1S), 0                         , 180, 0  , 287, 68 ), // #962
  INST(Vfixupimmss      , VexRvmi            , E(660F3A,55,_,I,_,0,2,T1S), 0                         , 181, 0  , 288, 68 ), // #963
  INST(Vfmadd132pd      , VexRvm_Lx          , V(660F38,98,_,x,1,1,4,FV ), 0                         , 182, 0  , 196, 148), // #964
  INST(Vfmadd132ph      , VexRvm_Lx          , E(66MAP6,98,_,_,_,0,4,FV ), 0                         , 183, 0  , 197, 132), // #965
  INST(Vfmadd132ps      , VexRvm_Lx          , V(660F38,98,_,x,0,0,4,FV ), 0                         , 110, 0  , 198, 148), // #966
  INST(Vfmadd132sd      , VexRvm             , V(660F38,99,_,I,1,1,3,T1S), 0                         , 184, 0  , 199, 149), // #967
  INST(Vfmadd132sh      , VexRvm             , E(66MAP6,99,_,_,_,0,1,T1S), 0                         , 185, 0  , 200, 134), // #968
  INST(Vfmadd132ss      , VexRvm             , V(660F38,99,_,I,0,0,2,T1S), 0                         , 122, 0  , 201, 149), // #969
  INST(Vfmadd213pd      , VexRvm_Lx          , V(660F38,A8,_,x,1,1,4,FV ), 0                         , 182, 0  , 196, 148), // #970
  INST(Vfmadd213ph      , VexRvm_Lx          , E(66MAP6,A8,_,_,_,0,4,FV ), 0                         , 183, 0  , 197, 132), // #971
  INST(Vfmadd213ps      , VexRvm_Lx          , V(660F38,A8,_,x,0,0,4,FV ), 0                         , 110, 0  , 198, 148), // #972
  INST(Vfmadd213sd      , VexRvm             , V(660F38,A9,_,I,1,1,3,T1S), 0                         , 184, 0  , 199, 149), // #973
  INST(Vfmadd213sh      , VexRvm             , E(66MAP6,A9,_,_,_,0,1,T1S), 0                         , 185, 0  , 200, 134), // #974
  INST(Vfmadd213ss      , VexRvm             , V(660F38,A9,_,I,0,0,2,T1S), 0                         , 122, 0  , 201, 149), // #975
  INST(Vfmadd231pd      , VexRvm_Lx          , V(660F38,B8,_,x,1,1,4,FV ), 0                         , 182, 0  , 196, 148), // #976
  INST(Vfmadd231ph      , VexRvm_Lx          , E(66MAP6,B8,_,_,_,0,4,FV ), 0                         , 183, 0  , 197, 132), // #977
  INST(Vfmadd231ps      , VexRvm_Lx          , V(660F38,B8,_,x,0,0,4,FV ), 0                         , 110, 0  , 198, 148), // #978
  INST(Vfmadd231sd      , VexRvm             , V(660F38,B9,_,I,1,1,3,T1S), 0                         , 184, 0  , 199, 149), // #979
  INST(Vfmadd231sh      , VexRvm             , E(66MAP6,B9,_,_,_,0,1,T1S), 0                         , 185, 0  , 200, 134), // #980
  INST(Vfmadd231ss      , VexRvm             , V(660F38,B9,_,I,0,0,2,T1S), 0                         , 122, 0  , 201, 149), // #981
  INST(Vfmaddcph        , VexRvm_Lx          , E(F3MAP6,56,_,_,_,0,4,FV ), 0                         , 186, 0  , 284, 132), // #982
  INST(Vfmaddcsh        , VexRvm             , E(F3MAP6,57,_,_,_,0,2,T1S), 0                         , 187, 0  , 259, 132), // #983
  INST(Vfmaddpd         , Fma4_Lx            , V(660F3A,69,_,x,x,_,_,_  ), 0                         , 73 , 0  , 289, 150), // #984
  INST(Vfmaddps         , Fma4_Lx            , V(660F3A,68,_,x,x,_,_,_  ), 0                         , 73 , 0  , 289, 150), // #985
  INST(Vfmaddsd         , Fma4               , V(660F3A,6B,_,0,x,_,_,_  ), 0                         , 73 , 0  , 290, 150), // #986
  INST(Vfmaddss         , Fma4               , V(660F3A,6A,_,0,x,_,_,_  ), 0                         , 73 , 0  , 291, 150), // #987
  INST(Vfmaddsub132pd   , VexRvm_Lx          , V(660F38,96,_,x,1,1,4,FV ), 0                         , 182, 0  , 196, 148), // #988
  INST(Vfmaddsub132ph   , VexRvm_Lx          , E(66MAP6,96,_,_,_,0,4,FV ), 0                         , 183, 0  , 197, 132), // #989
  INST(Vfmaddsub132ps   , VexRvm_Lx          , V(660F38,96,_,x,0,0,4,FV ), 0                         , 110, 0  , 198, 148), // #990
  INST(Vfmaddsub213pd   , VexRvm_Lx          , V(660F38,A6,_,x,1,1,4,FV ), 0                         , 182, 0  , 196, 148), // #991
  INST(Vfmaddsub213ph   , VexRvm_Lx          , E(66MAP6,A6,_,_,_,0,4,FV ), 0                         , 183, 0  , 197, 132), // #992
  INST(Vfmaddsub213ps   , VexRvm_Lx          , V(660F38,A6,_,x,0,0,4,FV ), 0                         , 110, 0  , 198, 148), // #993
  INST(Vfmaddsub231pd   , VexRvm_Lx          , V(660F38,B6,_,x,1,1,4,FV ), 0                         , 182, 0  , 196, 148), // #994
  INST(Vfmaddsub231ph   , VexRvm_Lx          , E(66MAP6,B6,_,_,_,0,4,FV ), 0                         , 183, 0  , 197, 132), // #995
  INST(Vfmaddsub231ps   , VexRvm_Lx          , V(660F38,B6,_,x,0,0,4,FV ), 0                         , 110, 0  , 198, 148), // #996
  INST(Vfmaddsubpd      , Fma4_Lx            , V(660F3A,5D,_,x,x,_,_,_  ), 0                         , 73 , 0  , 289, 150), // #997
  INST(Vfmaddsubps      , Fma4_Lx            , V(660F3A,5C,_,x,x,_,_,_  ), 0                         , 73 , 0  , 289, 150), // #998
  INST(Vfmsub132pd      , VexRvm_Lx          , V(660F38,9A,_,x,1,1,4,FV ), 0                         , 182, 0  , 196, 148), // #999
  INST(Vfmsub132ph      , VexRvm_Lx          , E(66MAP6,9A,_,_,_,0,4,FV ), 0                         , 183, 0  , 197, 132), // #1000
  INST(Vfmsub132ps      , VexRvm_Lx          , V(660F38,9A,_,x,0,0,4,FV ), 0                         , 110, 0  , 198, 148), // #1001
  INST(Vfmsub132sd      , VexRvm             , V(660F38,9B,_,I,1,1,3,T1S), 0                         , 184, 0  , 199, 149), // #1002
  INST(Vfmsub132sh      , VexRvm             , E(66MAP6,9B,_,_,_,0,1,T1S), 0                         , 185, 0  , 200, 134), // #1003
  INST(Vfmsub132ss      , VexRvm             , V(660F38,9B,_,I,0,0,2,T1S), 0                         , 122, 0  , 201, 149), // #1004
  INST(Vfmsub213pd      , VexRvm_Lx          , V(660F38,AA,_,x,1,1,4,FV ), 0                         , 182, 0  , 196, 148), // #1005
  INST(Vfmsub213ph      , VexRvm_Lx          , E(66MAP6,AA,_,_,_,0,4,FV ), 0                         , 183, 0  , 197, 132), // #1006
  INST(Vfmsub213ps      , VexRvm_Lx          , V(660F38,AA,_,x,0,0,4,FV ), 0                         , 110, 0  , 198, 148), // #1007
  INST(Vfmsub213sd      , VexRvm             , V(660F38,AB,_,I,1,1,3,T1S), 0                         , 184, 0  , 199, 149), // #1008
  INST(Vfmsub213sh      , VexRvm             , E(66MAP6,AB,_,_,_,0,1,T1S), 0                         , 185, 0  , 200, 134), // #1009
  INST(Vfmsub213ss      , VexRvm             , V(660F38,AB,_,I,0,0,2,T1S), 0                         , 122, 0  , 201, 149), // #1010
  INST(Vfmsub231pd      , VexRvm_Lx          , V(660F38,BA,_,x,1,1,4,FV ), 0                         , 182, 0  , 196, 148), // #1011
  INST(Vfmsub231ph      , VexRvm_Lx          , E(66MAP6,BA,_,_,_,0,4,FV ), 0                         , 183, 0  , 197, 132), // #1012
  INST(Vfmsub231ps      , VexRvm_Lx          , V(660F38,BA,_,x,0,0,4,FV ), 0                         , 110, 0  , 198, 148), // #1013
  INST(Vfmsub231sd      , VexRvm             , V(660F38,BB,_,I,1,1,3,T1S), 0                         , 184, 0  , 199, 149), // #1014
  INST(Vfmsub231sh      , VexRvm             , E(66MAP6,BB,_,_,_,0,1,T1S), 0                         , 185, 0  , 200, 134), // #1015
  INST(Vfmsub231ss      , VexRvm             , V(660F38,BB,_,I,0,0,2,T1S), 0                         , 122, 0  , 201, 149), // #1016
  INST(Vfmsubadd132pd   , VexRvm_Lx          , V(660F38,97,_,x,1,1,4,FV ), 0                         , 182, 0  , 196, 148), // #1017
  INST(Vfmsubadd132ph   , VexRvm_Lx          , E(66MAP6,97,_,_,_,0,4,FV ), 0                         , 183, 0  , 197, 132), // #1018
  INST(Vfmsubadd132ps   , VexRvm_Lx          , V(660F38,97,_,x,0,0,4,FV ), 0                         , 110, 0  , 198, 148), // #1019
  INST(Vfmsubadd213pd   , VexRvm_Lx          , V(660F38,A7,_,x,1,1,4,FV ), 0                         , 182, 0  , 196, 148), // #1020
  INST(Vfmsubadd213ph   , VexRvm_Lx          , E(66MAP6,A7,_,_,_,0,4,FV ), 0                         , 183, 0  , 197, 132), // #1021
  INST(Vfmsubadd213ps   , VexRvm_Lx          , V(660F38,A7,_,x,0,0,4,FV ), 0                         , 110, 0  , 198, 148), // #1022
  INST(Vfmsubadd231pd   , VexRvm_Lx          , V(660F38,B7,_,x,1,1,4,FV ), 0                         , 182, 0  , 196, 148), // #1023
  INST(Vfmsubadd231ph   , VexRvm_Lx          , E(66MAP6,B7,_,_,_,0,4,FV ), 0                         , 183, 0  , 197, 132), // #1024
  INST(Vfmsubadd231ps   , VexRvm_Lx          , V(660F38,B7,_,x,0,0,4,FV ), 0                         , 110, 0  , 198, 148), // #1025
  INST(Vfmsubaddpd      , Fma4_Lx            , V(660F3A,5F,_,x,x,_,_,_  ), 0                         , 73 , 0  , 289, 150), // #1026
  INST(Vfmsubaddps      , Fma4_Lx            , V(660F3A,5E,_,x,x,_,_,_  ), 0                         , 73 , 0  , 289, 150), // #1027
  INST(Vfmsubpd         , Fma4_Lx            , V(660F3A,6D,_,x,x,_,_,_  ), 0                         , 73 , 0  , 289, 150), // #1028
  INST(Vfmsubps         , Fma4_Lx            , V(660F3A,6C,_,x,x,_,_,_  ), 0                         , 73 , 0  , 289, 150), // #1029
  INST(Vfmsubsd         , Fma4               , V(660F3A,6F,_,0,x,_,_,_  ), 0                         , 73 , 0  , 290, 150), // #1030
  INST(Vfmsubss         , Fma4               , V(660F3A,6E,_,0,x,_,_,_  ), 0                         , 73 , 0  , 291, 150), // #1031
  INST(Vfmulcph         , VexRvm_Lx          , E(F3MAP6,D6,_,_,_,0,4,FV ), 0                         , 186, 0  , 284, 132), // #1032
  INST(Vfmulcsh         , VexRvm             , E(F3MAP6,D7,_,_,_,0,2,T1S), 0                         , 187, 0  , 259, 132), // #1033
  INST(Vfnmadd132pd     , VexRvm_Lx          , V(660F38,9C,_,x,1,1,4,FV ), 0                         , 182, 0  , 196, 148), // #1034
  INST(Vfnmadd132ph     , VexRvm_Lx          , E(66MAP6,9C,_,_,_,0,4,FV ), 0                         , 183, 0  , 197, 132), // #1035
  INST(Vfnmadd132ps     , VexRvm_Lx          , V(660F38,9C,_,x,0,0,4,FV ), 0                         , 110, 0  , 198, 148), // #1036
  INST(Vfnmadd132sd     , VexRvm             , V(660F38,9D,_,I,1,1,3,T1S), 0                         , 184, 0  , 199, 149), // #1037
  INST(Vfnmadd132sh     , VexRvm             , E(66MAP6,9D,_,_,_,0,1,T1S), 0                         , 185, 0  , 200, 134), // #1038
  INST(Vfnmadd132ss     , VexRvm             , V(660F38,9D,_,I,0,0,2,T1S), 0                         , 122, 0  , 201, 149), // #1039
  INST(Vfnmadd213pd     , VexRvm_Lx          , V(660F38,AC,_,x,1,1,4,FV ), 0                         , 182, 0  , 196, 148), // #1040
  INST(Vfnmadd213ph     , VexRvm_Lx          , E(66MAP6,AC,_,_,_,0,4,FV ), 0                         , 183, 0  , 197, 132), // #1041
  INST(Vfnmadd213ps     , VexRvm_Lx          , V(660F38,AC,_,x,0,0,4,FV ), 0                         , 110, 0  , 198, 148), // #1042
  INST(Vfnmadd213sd     , VexRvm             , V(660F38,AD,_,I,1,1,3,T1S), 0                         , 184, 0  , 199, 149), // #1043
  INST(Vfnmadd213sh     , VexRvm             , E(66MAP6,AD,_,_,_,0,1,T1S), 0                         , 185, 0  , 200, 134), // #1044
  INST(Vfnmadd213ss     , VexRvm             , V(660F38,AD,_,I,0,0,2,T1S), 0                         , 122, 0  , 201, 149), // #1045
  INST(Vfnmadd231pd     , VexRvm_Lx          , V(660F38,BC,_,x,1,1,4,FV ), 0                         , 182, 0  , 196, 148), // #1046
  INST(Vfnmadd231ph     , VexRvm_Lx          , E(66MAP6,BC,_,_,_,0,4,FV ), 0                         , 183, 0  , 197, 132), // #1047
  INST(Vfnmadd231ps     , VexRvm_Lx          , V(660F38,BC,_,x,0,0,4,FV ), 0                         , 110, 0  , 198, 148), // #1048
  INST(Vfnmadd231sd     , VexRvm             , V(660F38,BD,_,I,1,1,3,T1S), 0                         , 184, 0  , 199, 149), // #1049
  INST(Vfnmadd231sh     , VexRvm             , E(66MAP6,BD,_,_,_,0,1,T1S), 0                         , 185, 0  , 200, 134), // #1050
  INST(Vfnmadd231ss     , VexRvm             , V(660F38,BD,_,I,0,0,2,T1S), 0                         , 122, 0  , 201, 149), // #1051
  INST(Vfnmaddpd        , Fma4_Lx            , V(660F3A,79,_,x,x,_,_,_  ), 0                         , 73 , 0  , 289, 150), // #1052
  INST(Vfnmaddps        , Fma4_Lx            , V(660F3A,78,_,x,x,_,_,_  ), 0                         , 73 , 0  , 289, 150), // #1053
  INST(Vfnmaddsd        , Fma4               , V(660F3A,7B,_,0,x,_,_,_  ), 0                         , 73 , 0  , 290, 150), // #1054
  INST(Vfnmaddss        , Fma4               , V(660F3A,7A,_,0,x,_,_,_  ), 0                         , 73 , 0  , 291, 150), // #1055
  INST(Vfnmsub132pd     , VexRvm_Lx          , V(660F38,9E,_,x,1,1,4,FV ), 0                         , 182, 0  , 196, 148), // #1056
  INST(Vfnmsub132ph     , VexRvm_Lx          , E(66MAP6,9E,_,_,_,0,4,FV ), 0                         , 183, 0  , 197, 132), // #1057
  INST(Vfnmsub132ps     , VexRvm_Lx          , V(660F38,9E,_,x,0,0,4,FV ), 0                         , 110, 0  , 198, 148), // #1058
  INST(Vfnmsub132sd     , VexRvm             , V(660F38,9F,_,I,1,1,3,T1S), 0                         , 184, 0  , 199, 149), // #1059
  INST(Vfnmsub132sh     , VexRvm             , E(66MAP6,9F,_,_,_,0,1,T1S), 0                         , 185, 0  , 200, 134), // #1060
  INST(Vfnmsub132ss     , VexRvm             , V(660F38,9F,_,I,0,0,2,T1S), 0                         , 122, 0  , 201, 149), // #1061
  INST(Vfnmsub213pd     , VexRvm_Lx          , V(660F38,AE,_,x,1,1,4,FV ), 0                         , 182, 0  , 196, 148), // #1062
  INST(Vfnmsub213ph     , VexRvm_Lx          , E(66MAP6,AE,_,_,_,0,4,FV ), 0                         , 183, 0  , 197, 132), // #1063
  INST(Vfnmsub213ps     , VexRvm_Lx          , V(660F38,AE,_,x,0,0,4,FV ), 0                         , 110, 0  , 198, 148), // #1064
  INST(Vfnmsub213sd     , VexRvm             , V(660F38,AF,_,I,1,1,3,T1S), 0                         , 184, 0  , 199, 149), // #1065
  INST(Vfnmsub213sh     , VexRvm             , E(66MAP6,AF,_,_,_,0,1,T1S), 0                         , 185, 0  , 200, 134), // #1066
  INST(Vfnmsub213ss     , VexRvm             , V(660F38,AF,_,I,0,0,2,T1S), 0                         , 122, 0  , 201, 149), // #1067
  INST(Vfnmsub231pd     , VexRvm_Lx          , V(660F38,BE,_,x,1,1,4,FV ), 0                         , 182, 0  , 196, 148), // #1068
  INST(Vfnmsub231ph     , VexRvm_Lx          , E(66MAP6,BE,_,_,_,0,4,FV ), 0                         , 183, 0  , 197, 132), // #1069
  INST(Vfnmsub231ps     , VexRvm_Lx          , V(660F38,BE,_,x,0,0,4,FV ), 0                         , 110, 0  , 198, 148), // #1070
  INST(Vfnmsub231sd     , VexRvm             , V(660F38,BF,_,I,1,1,3,T1S), 0                         , 184, 0  , 199, 149), // #1071
  INST(Vfnmsub231sh     , VexRvm             , E(66MAP6,BF,_,_,_,0,1,T1S), 0                         , 185, 0  , 200, 134), // #1072
  INST(Vfnmsub231ss     , VexRvm             , V(660F38,BF,_,I,0,0,2,T1S), 0                         , 122, 0  , 201, 149), // #1073
  INST(Vfnmsubpd        , Fma4_Lx            , V(660F3A,7D,_,x,x,_,_,_  ), 0                         , 73 , 0  , 289, 150), // #1074
  INST(Vfnmsubps        , Fma4_Lx            , V(660F3A,7C,_,x,x,_,_,_  ), 0                         , 73 , 0  , 289, 150), // #1075
  INST(Vfnmsubsd        , Fma4               , V(660F3A,7F,_,0,x,_,_,_  ), 0                         , 73 , 0  , 290, 150), // #1076
  INST(Vfnmsubss        , Fma4               , V(660F3A,7E,_,0,x,_,_,_  ), 0                         , 73 , 0  , 291, 150), // #1077
  INST(Vfpclasspd       , VexRmi_Lx          , E(660F3A,66,_,x,_,1,4,FV ), 0                         , 112, 0  , 292, 140), // #1078
  INST(Vfpclassph       , VexRmi_Lx          , E(000F3A,66,_,_,_,0,4,FV ), 0                         , 123, 0  , 293, 132), // #1079
  INST(Vfpclassps       , VexRmi_Lx          , E(660F3A,66,_,x,_,0,4,FV ), 0                         , 111, 0  , 294, 140), // #1080
  INST(Vfpclasssd       , VexRmi             , E(660F3A,67,_,I,_,1,3,T1S), 0                         , 180, 0  , 295, 66 ), // #1081
  INST(Vfpclasssh       , VexRmi             , E(000F3A,67,_,_,_,0,1,T1S), 0                         , 188, 0  , 296, 134), // #1082
  INST(Vfpclassss       , VexRmi             , E(660F3A,67,_,I,_,0,2,T1S), 0                         , 181, 0  , 297, 66 ), // #1083
  INST(Vfrczpd          , VexRm_Lx           , V(XOP_M9,81,_,x,0,_,_,_  ), 0                         , 79 , 0  , 298, 151), // #1084
  INST(Vfrczps          , VexRm_Lx           , V(XOP_M9,80,_,x,0,_,_,_  ), 0                         , 79 , 0  , 298, 151), // #1085
  INST(Vfrczsd          , VexRm              , V(XOP_M9,83,_,0,0,_,_,_  ), 0                         , 79 , 0  , 299, 151), // #1086
  INST(Vfrczss          , VexRm              , V(XOP_M9,82,_,0,0,_,_,_  ), 0                         , 79 , 0  , 300, 151), // #1087
  INST(Vgatherdpd       , VexRmvRm_VM        , V(660F38,92,_,x,1,_,_,_  ), E(660F38,92,_,x,_,1,3,T1S), 189, 80 , 301, 152), // #1088
  INST(Vgatherdps       , VexRmvRm_VM        , V(660F38,92,_,x,0,_,_,_  ), E(660F38,92,_,x,_,0,2,T1S), 96 , 81 , 302, 152), // #1089
  INST(Vgatherpf0dpd    , VexM_VM            , E(660F38,C6,1,2,_,1,3,T1S), 0                         , 190, 0  , 303, 153), // #1090
  INST(Vgatherpf0dps    , VexM_VM            , E(660F38,C6,1,2,_,0,2,T1S), 0                         , 191, 0  , 304, 153), // #1091
  INST(Vgatherpf0qpd    , VexM_VM            , E(660F38,C7,1,2,_,1,3,T1S), 0                         , 190, 0  , 305, 153), // #1092
  INST(Vgatherpf0qps    , VexM_VM            , E(660F38,C7,1,2,_,0,2,T1S), 0                         , 191, 0  , 305, 153), // #1093
  INST(Vgatherpf1dpd    , VexM_VM            , E(660F38,C6,2,2,_,1,3,T1S), 0                         , 192, 0  , 303, 153), // #1094
  INST(Vgatherpf1dps    , VexM_VM            , E(660F38,C6,2,2,_,0,2,T1S), 0                         , 193, 0  , 304, 153), // #1095
  INST(Vgatherpf1qpd    , VexM_VM            , E(660F38,C7,2,2,_,1,3,T1S), 0                         , 192, 0  , 305, 153), // #1096
  INST(Vgatherpf1qps    , VexM_VM            , E(660F38,C7,2,2,_,0,2,T1S), 0                         , 193, 0  , 305, 153), // #1097
  INST(Vgatherqpd       , VexRmvRm_VM        , V(660F38,93,_,x,1,_,_,_  ), E(660F38,93,_,x,_,1,3,T1S), 189, 82 , 306, 152), // #1098
  INST(Vgatherqps       , VexRmvRm_VM        , V(660F38,93,_,x,0,_,_,_  ), E(660F38,93,_,x,_,0,2,T1S), 96 , 83 , 307, 152), // #1099
  INST(Vgetexppd        , VexRm_Lx           , E(660F38,42,_,x,_,1,4,FV ), 0                         , 113, 0  , 263, 138), // #1100
  INST(Vgetexpph        , VexRm_Lx           , E(66MAP6,42,_,_,_,0,4,FV ), 0                         , 183, 0  , 265, 132), // #1101
  INST(Vgetexpps        , VexRm_Lx           , E(660F38,42,_,x,_,0,4,FV ), 0                         , 114, 0  , 268, 138), // #1102
  INST(Vgetexpsd        , VexRvm             , E(660F38,43,_,I,_,1,3,T1S), 0                         , 128, 0  , 308, 68 ), // #1103
  INST(Vgetexpsh        , VexRvm             , E(66MAP6,43,_,_,_,0,1,T1S), 0                         , 185, 0  , 254, 134), // #1104
  INST(Vgetexpss        , VexRvm             , E(660F38,43,_,I,_,0,2,T1S), 0                         , 129, 0  , 309, 68 ), // #1105
  INST(Vgetmantpd       , VexRmi_Lx          , E(660F3A,26,_,x,_,1,4,FV ), 0                         , 112, 0  , 310, 138), // #1106
  INST(Vgetmantph       , VexRmi_Lx          , E(000F3A,26,_,_,_,0,4,FV ), 0                         , 123, 0  , 311, 132), // #1107
  INST(Vgetmantps       , VexRmi_Lx          , E(660F3A,26,_,x,_,0,4,FV ), 0                         , 111, 0  , 312, 138), // #1108
  INST(Vgetmantsd       , VexRvmi            , E(660F3A,27,_,I,_,1,3,T1S), 0                         , 180, 0  , 287, 68 ), // #1109
  INST(Vgetmantsh       , VexRvmi            , E(000F3A,27,_,_,_,0,1,T1S), 0                         , 188, 0  , 313, 134), // #1110
  INST(Vgetmantss       , VexRvmi            , E(660F3A,27,_,I,_,0,2,T1S), 0                         , 181, 0  , 288, 68 ), // #1111
  INST(Vgf2p8affineinvqb, VexRvmi_Lx         , V(660F3A,CF,_,x,1,1,4,FV ), 0                         , 194, 0  , 314, 154), // #1112
  INST(Vgf2p8affineqb   , VexRvmi_Lx         , V(660F3A,CE,_,x,1,1,4,FV ), 0                         , 194, 0  , 314, 154), // #1113
  INST(Vgf2p8mulb       , VexRvm_Lx          , V(660F38,CF,_,x,0,0,4,FV ), 0                         , 110, 0  , 315, 154), // #1114
  INST(Vhaddpd          , VexRvm_Lx          , V(660F00,7C,_,x,I,_,_,_  ), 0                         , 69 , 0  , 202, 135), // #1115
  INST(Vhaddps          , VexRvm_Lx          , V(F20F00,7C,_,x,I,_,_,_  ), 0                         , 109, 0  , 202, 135), // #1116
  INST(Vhsubpd          , VexRvm_Lx          , V(660F00,7D,_,x,I,_,_,_  ), 0                         , 69 , 0  , 202, 135), // #1117
  INST(Vhsubps          , VexRvm_Lx          , V(F20F00,7D,_,x,I,_,_,_  ), 0                         , 109, 0  , 202, 135), // #1118
  INST(Vinsertf128      , VexRvmi            , V(660F3A,18,_,1,0,_,_,_  ), 0                         , 172, 0  , 316, 135), // #1119
  INST(Vinsertf32x4     , VexRvmi_Lx         , E(660F3A,18,_,x,_,0,4,T4 ), 0                         , 173, 0  , 317, 138), // #1120
  INST(Vinsertf32x8     , VexRvmi            , E(660F3A,1A,_,2,_,0,5,T8 ), 0                         , 174, 0  , 318, 66 ), // #1121
  INST(Vinsertf64x2     , VexRvmi_Lx         , E(660F3A,18,_,x,_,1,4,T2 ), 0                         , 175, 0  , 317, 140), // #1122
  INST(Vinsertf64x4     , VexRvmi            , E(660F3A,1A,_,2,_,1,5,T4 ), 0                         , 176, 0  , 318, 68 ), // #1123
  INST(Vinserti128      , VexRvmi            , V(660F3A,38,_,1,0,_,_,_  ), 0                         , 172, 0  , 316, 141), // #1124
  INST(Vinserti32x4     , VexRvmi_Lx         , E(660F3A,38,_,x,_,0,4,T4 ), 0                         , 173, 0  , 317, 138), // #1125
  INST(Vinserti32x8     , VexRvmi            , E(660F3A,3A,_,2,_,0,5,T8 ), 0                         , 174, 0  , 318, 66 ), // #1126
  INST(Vinserti64x2     , VexRvmi_Lx         , E(660F3A,38,_,x,_,1,4,T2 ), 0                         , 175, 0  , 317, 140), // #1127
  INST(Vinserti64x4     , VexRvmi            , E(660F3A,3A,_,2,_,1,5,T4 ), 0                         , 176, 0  , 318, 68 ), // #1128
  INST(Vinsertps        , VexRvmi            , V(660F3A,21,_,0,I,0,2,T1S), 0                         , 177, 0  , 319, 133), // #1129
  INST(Vlddqu           , VexRm_Lx           , V(F20F00,F0,_,x,I,_,_,_  ), 0                         , 109, 0  , 320, 135), // #1130
  INST(Vldmxcsr         , VexM               , V(000F00,AE,2,0,I,_,_,_  ), 0                         , 195, 0  , 321, 135), // #1131
  INST(Vmaskmovdqu      , VexRm_ZDI          , V(660F00,F7,_,0,I,_,_,_  ), 0                         , 69 , 0  , 322, 135), // #1132
  INST(Vmaskmovpd       , VexRvmMvr_Lx       , V(660F38,2D,_,x,0,_,_,_  ), V(660F38,2F,_,x,0,_,_,_  ), 96 , 84 , 323, 135), // #1133
  INST(Vmaskmovps       , VexRvmMvr_Lx       , V(660F38,2C,_,x,0,_,_,_  ), V(660F38,2E,_,x,0,_,_,_  ), 96 , 85 , 323, 135), // #1134
  INST(Vmaxpd           , VexRvm_Lx          , V(660F00,5F,_,x,I,1,4,FV ), 0                         , 103, 0  , 324, 131), // #1135
  INST(Vmaxph           , VexRvm_Lx          , E(00MAP5,5F,_,_,_,0,4,FV ), 0                         , 104, 0  , 325, 132), // #1136
  INST(Vmaxps           , VexRvm_Lx          , V(000F00,5F,_,x,I,0,4,FV ), 0                         , 105, 0  , 326, 131), // #1137
  INST(Vmaxsd           , VexRvm             , V(F20F00,5F,_,I,I,1,3,T1S), 0                         , 106, 0  , 327, 131), // #1138
  INST(Vmaxsh           , VexRvm             , E(F3MAP5,5F,_,_,_,0,1,T1S), 0                         , 107, 0  , 254, 134), // #1139
  INST(Vmaxss           , VexRvm             , V(F30F00,5F,_,I,I,0,2,T1S), 0                         , 108, 0  , 258, 131), // #1140
  INST(Vmcall           , X86Op              , O(000F01,C1,_,_,_,_,_,_  ), 0                         , 21 , 0  , 30 , 58 ), // #1141
  INST(Vmclear          , X86M_Only          , O(660F00,C7,6,_,_,_,_,_  ), 0                         , 26 , 0  , 32 , 58 ), // #1142
  INST(Vmfunc           , X86Op              , O(000F01,D4,_,_,_,_,_,_  ), 0                         , 21 , 0  , 30 , 58 ), // #1143
  INST(Vminpd           , VexRvm_Lx          , V(660F00,5D,_,x,I,1,4,FV ), 0                         , 103, 0  , 324, 131), // #1144
  INST(Vminph           , VexRvm_Lx          , E(00MAP5,5D,_,_,_,0,4,FV ), 0                         , 104, 0  , 325, 132), // #1145
  INST(Vminps           , VexRvm_Lx          , V(000F00,5D,_,x,I,0,4,FV ), 0                         , 105, 0  , 326, 131), // #1146
  INST(Vminsd           , VexRvm             , V(F20F00,5D,_,I,I,1,3,T1S), 0                         , 106, 0  , 327, 131), // #1147
  INST(Vminsh           , VexRvm             , E(F3MAP5,5D,_,_,_,0,1,T1S), 0                         , 107, 0  , 254, 134), // #1148
  INST(Vminss           , VexRvm             , V(F30F00,5D,_,I,I,0,2,T1S), 0                         , 108, 0  , 258, 131), // #1149
  INST(Vmlaunch         , X86Op              , O(000F01,C2,_,_,_,_,_,_  ), 0                         , 21 , 0  , 30 , 58 ), // #1150
  INST(Vmload           , X86Op_xAX          , O(000F01,DA,_,_,_,_,_,_  ), 0                         , 21 , 0  , 328, 22 ), // #1151
  INST(Vmmcall          , X86Op              , O(000F01,D9,_,_,_,_,_,_  ), 0                         , 21 , 0  , 30 , 22 ), // #1152
  INST(Vmovapd          , VexRmMr_Lx         , V(660F00,28,_,x,I,1,4,FVM), V(660F00,29,_,x,I,1,4,FVM), 103, 86 , 329, 155), // #1153
  INST(Vmovaps          , VexRmMr_Lx         , V(000F00,28,_,x,I,0,4,FVM), V(000F00,29,_,x,I,0,4,FVM), 105, 87 , 329, 155), // #1154
  INST(Vmovd            , VexMovdMovq        , V(660F00,6E,_,0,0,0,2,T1S), V(660F00,7E,_,0,0,0,2,T1S), 196, 88 , 330, 133), // #1155
  INST(Vmovddup         , VexRm_Lx           , V(F20F00,12,_,x,I,1,3,DUP), 0                         , 197, 0  , 331, 131), // #1156
  INST(Vmovdqa          , VexRmMr_Lx         , V(660F00,6F,_,x,I,_,_,_  ), V(660F00,7F,_,x,I,_,_,_  ), 69 , 89 , 332, 156), // #1157
  INST(Vmovdqa32        , VexRmMr_Lx         , E(660F00,6F,_,x,_,0,4,FVM), E(660F00,7F,_,x,_,0,4,FVM), 198, 90 , 333, 157), // #1158
  INST(Vmovdqa64        , VexRmMr_Lx         , E(660F00,6F,_,x,_,1,4,FVM), E(660F00,7F,_,x,_,1,4,FVM), 135, 91 , 333, 157), // #1159
  INST(Vmovdqu          , VexRmMr_Lx         , V(F30F00,6F,_,x,I,_,_,_  ), V(F30F00,7F,_,x,I,_,_,_  ), 199, 92 , 332, 156), // #1160
  INST(Vmovdqu16        , VexRmMr_Lx         , E(F20F00,6F,_,x,_,1,4,FVM), E(F20F00,7F,_,x,_,1,4,FVM), 166, 93 , 333, 158), // #1161
  INST(Vmovdqu32        , VexRmMr_Lx         , E(F30F00,6F,_,x,_,0,4,FVM), E(F30F00,7F,_,x,_,0,4,FVM), 200, 94 , 333, 157), // #1162
  INST(Vmovdqu64        , VexRmMr_Lx         , E(F30F00,6F,_,x,_,1,4,FVM), E(F30F00,7F,_,x,_,1,4,FVM), 149, 95 , 333, 157), // #1163
  INST(Vmovdqu8         , VexRmMr_Lx         , E(F20F00,6F,_,x,_,0,4,FVM), E(F20F00,7F,_,x,_,0,4,FVM), 164, 96 , 333, 158), // #1164
  INST(Vmovhlps         , VexRvm             , V(000F00,12,_,0,I,0,_,_  ), 0                         , 72 , 0  , 334, 133), // #1165
  INST(Vmovhpd          , VexRvmMr           , V(660F00,16,_,0,I,1,3,T1S), V(660F00,17,_,0,I,1,3,T1S), 125, 97 , 335, 133), // #1166
  INST(Vmovhps          , VexRvmMr           , V(000F00,16,_,0,I,0,3,T2 ), V(000F00,17,_,0,I,0,3,T2 ), 201, 98 , 335, 133), // #1167
  INST(Vmovlhps         , VexRvm             , V(000F00,16,_,0,I,0,_,_  ), 0                         , 72 , 0  , 334, 133), // #1168
  INST(Vmovlpd          , VexRvmMr           , V(660F00,12,_,0,I,1,3,T1S), V(660F00,13,_,0,I,1,3,T1S), 125, 99 , 335, 133), // #1169
  INST(Vmovlps          , VexRvmMr           , V(000F00,12,_,0,I,0,3,T2 ), V(000F00,13,_,0,I,0,3,T2 ), 201, 100, 335, 133), // #1170
  INST(Vmovmskpd        , VexRm_Lx           , V(660F00,50,_,x,I,_,_,_  ), 0                         , 69 , 0  , 336, 135), // #1171
  INST(Vmovmskps        , VexRm_Lx           , V(000F00,50,_,x,I,_,_,_  ), 0                         , 72 , 0  , 336, 135), // #1172
  INST(Vmovntdq         , VexMr_Lx           , V(660F00,E7,_,x,I,0,4,FVM), 0                         , 144, 0  , 337, 131), // #1173
  INST(Vmovntdqa        , VexRm_Lx           , V(660F38,2A,_,x,I,0,4,FVM), 0                         , 110, 0  , 338, 142), // #1174
  INST(Vmovntpd         , VexMr_Lx           , V(660F00,2B,_,x,I,1,4,FVM), 0                         , 103, 0  , 337, 131), // #1175
  INST(Vmovntps         , VexMr_Lx           , V(000F00,2B,_,x,I,0,4,FVM), 0                         , 105, 0  , 337, 131), // #1176
  INST(Vmovq            , VexMovdMovq        , V(660F00,6E,_,0,I,1,3,T1S), V(660F00,7E,_,0,I,1,3,T1S), 125, 101, 339, 159), // #1177
  INST(Vmovsd           , VexMovssMovsd      , V(F20F00,10,_,I,I,1,3,T1S), V(F20F00,11,_,I,I,1,3,T1S), 106, 102, 340, 159), // #1178
  INST(Vmovsh           , VexMovssMovsd      , E(F3MAP5,10,_,I,_,0,1,T1S), E(F3MAP5,11,_,I,_,0,1,T1S), 107, 103, 341, 134), // #1179
  INST(Vmovshdup        , VexRm_Lx           , V(F30F00,16,_,x,I,0,4,FVM), 0                         , 161, 0  , 342, 131), // #1180
  INST(Vmovsldup        , VexRm_Lx           , V(F30F00,12,_,x,I,0,4,FVM), 0                         , 161, 0  , 342, 131), // #1181
  INST(Vmovss           , VexMovssMovsd      , V(F30F00,10,_,I,I,0,2,T1S), V(F30F00,11,_,I,I,0,2,T1S), 108, 104, 343, 159), // #1182
  INST(Vmovupd          , VexRmMr_Lx         , V(660F00,10,_,x,I,1,4,FVM), V(660F00,11,_,x,I,1,4,FVM), 103, 105, 329, 155), // #1183
  INST(Vmovups          , VexRmMr_Lx         , V(000F00,10,_,x,I,0,4,FVM), V(000F00,11,_,x,I,0,4,FVM), 105, 106, 329, 155), // #1184
  INST(Vmovw            , VexMovdMovq        , E(66MAP5,6E,_,0,_,I,1,T1S), E(66MAP5,7E,_,0,_,I,1,T1S), 202, 107, 344, 134), // #1185
  INST(Vmpsadbw         , VexRvmi_Lx         , V(660F3A,42,_,x,I,_,_,_  ), 0                         , 73 , 0  , 214, 160), // #1186
  INST(Vmptrld          , X86M_Only          , O(000F00,C7,6,_,_,_,_,_  ), 0                         , 80 , 0  , 32 , 58 ), // #1187
  INST(Vmptrst          , X86M_Only          , O(000F00,C7,7,_,_,_,_,_  ), 0                         , 22 , 0  , 32 , 58 ), // #1188
  INST(Vmread           , X86Mr_NoSize       , O(000F00,78,_,_,_,_,_,_  ), 0                         , 4  , 0  , 345, 58 ), // #1189
  INST(Vmresume         , X86Op              , O(000F01,C3,_,_,_,_,_,_  ), 0                         , 21 , 0  , 30 , 58 ), // #1190
  INST(Vmrun            , X86Op_xAX          , O(000F01,D8,_,_,_,_,_,_  ), 0                         , 21 , 0  , 328, 22 ), // #1191
  INST(Vmsave           , X86Op_xAX          , O(000F01,DB,_,_,_,_,_,_  ), 0                         , 21 , 0  , 328, 22 ), // #1192
  INST(Vmulpd           , VexRvm_Lx          , V(660F00,59,_,x,I,1,4,FV ), 0                         , 103, 0  , 196, 131), // #1193
  INST(Vmulph           , VexRvm_Lx          , E(00MAP5,59,_,_,_,0,4,FV ), 0                         , 104, 0  , 197, 132), // #1194
  INST(Vmulps           , VexRvm_Lx          , V(000F00,59,_,x,I,0,4,FV ), 0                         , 105, 0  , 198, 131), // #1195
  INST(Vmulsd           , VexRvm             , V(F20F00,59,_,I,I,1,3,T1S), 0                         , 106, 0  , 199, 133), // #1196
  INST(Vmulsh           , VexRvm             , E(F3MAP5,59,_,_,_,0,1,T1S), 0                         , 107, 0  , 200, 134), // #1197
  INST(Vmulss           , VexRvm             , V(F30F00,59,_,I,I,0,2,T1S), 0                         , 108, 0  , 201, 133), // #1198
  INST(Vmwrite          , X86Rm_NoSize       , O(000F00,79,_,_,_,_,_,_  ), 0                         , 4  , 0  , 346, 58 ), // #1199
  INST(Vmxon            , X86M_Only          , O(F30F00,C7,6,_,_,_,_,_  ), 0                         , 24 , 0  , 32 , 58 ), // #1200
  INST(Vorpd            , VexRvm_Lx          , V(660F00,56,_,x,I,1,4,FV ), 0                         , 103, 0  , 210, 139), // #1201
  INST(Vorps            , VexRvm_Lx          , V(000F00,56,_,x,I,0,4,FV ), 0                         , 105, 0  , 211, 139), // #1202
  INST(Vp2intersectd    , VexRvm_Lx_2xK      , E(F20F38,68,_,_,_,0,4,FV ), 0                         , 131, 0  , 347, 161), // #1203
  INST(Vp2intersectq    , VexRvm_Lx_2xK      , E(F20F38,68,_,_,_,1,4,FV ), 0                         , 203, 0  , 348, 161), // #1204
  INST(Vp4dpwssd        , VexRm_T1_4X        , E(F20F38,52,_,2,_,0,4,T4X), 0                         , 101, 0  , 194, 162), // #1205
  INST(Vp4dpwssds       , VexRm_T1_4X        , E(F20F38,53,_,2,_,0,4,T4X), 0                         , 101, 0  , 194, 162), // #1206
  INST(Vpabsb           , VexRm_Lx           , V(660F38,1C,_,x,I,_,4,FVM), 0                         , 110, 0  , 342, 163), // #1207
  INST(Vpabsd           , VexRm_Lx           , V(660F38,1E,_,x,I,0,4,FV ), 0                         , 110, 0  , 349, 142), // #1208
  INST(Vpabsq           , VexRm_Lx           , E(660F38,1F,_,x,_,1,4,FV ), 0                         , 113, 0  , 350, 138), // #1209
  INST(Vpabsw           , VexRm_Lx           , V(660F38,1D,_,x,I,_,4,FVM), 0                         , 110, 0  , 342, 163), // #1210
  INST(Vpackssdw        , VexRvm_Lx          , V(660F00,6B,_,x,I,0,4,FV ), 0                         , 144, 0  , 209, 163), // #1211
  INST(Vpacksswb        , VexRvm_Lx          , V(660F00,63,_,x,I,I,4,FVM), 0                         , 144, 0  , 315, 163), // #1212
  INST(Vpackusdw        , VexRvm_Lx          , V(660F38,2B,_,x,I,0,4,FV ), 0                         , 110, 0  , 209, 163), // #1213
  INST(Vpackuswb        , VexRvm_Lx          , V(660F00,67,_,x,I,I,4,FVM), 0                         , 144, 0  , 315, 163), // #1214
  INST(Vpaddb           , VexRvm_Lx          , V(660F00,FC,_,x,I,I,4,FVM), 0                         , 144, 0  , 315, 163), // #1215
  INST(Vpaddd           , VexRvm_Lx          , V(660F00,FE,_,x,I,0,4,FV ), 0                         , 144, 0  , 209, 142), // #1216
  INST(Vpaddq           , VexRvm_Lx          , V(660F00,D4,_,x,I,1,4,FV ), 0                         , 103, 0  , 208, 142), // #1217
  INST(Vpaddsb          , VexRvm_Lx          , V(660F00,EC,_,x,I,I,4,FVM), 0                         , 144, 0  , 315, 163), // #1218
  INST(Vpaddsw          , VexRvm_Lx          , V(660F00,ED,_,x,I,I,4,FVM), 0                         , 144, 0  , 315, 163), // #1219
  INST(Vpaddusb         , VexRvm_Lx          , V(660F00,DC,_,x,I,I,4,FVM), 0                         , 144, 0  , 315, 163), // #1220
  INST(Vpaddusw         , VexRvm_Lx          , V(660F00,DD,_,x,I,I,4,FVM), 0                         , 144, 0  , 315, 163), // #1221
  INST(Vpaddw           , VexRvm_Lx          , V(660F00,FD,_,x,I,I,4,FVM), 0                         , 144, 0  , 315, 163), // #1222
  INST(Vpalignr         , VexRvmi_Lx         , V(660F3A,0F,_,x,I,I,4,FVM), 0                         , 204, 0  , 314, 163), // #1223
  INST(Vpand            , VexRvm_Lx          , V(660F00,DB,_,x,I,_,_,_  ), 0                         , 69 , 0  , 351, 160), // #1224
  INST(Vpandd           , VexRvm_Lx          , E(660F00,DB,_,x,_,0,4,FV ), 0                         , 198, 0  , 352, 138), // #1225
  INST(Vpandn           , VexRvm_Lx          , V(660F00,DF,_,x,I,_,_,_  ), 0                         , 69 , 0  , 353, 160), // #1226
  INST(Vpandnd          , VexRvm_Lx          , E(660F00,DF,_,x,_,0,4,FV ), 0                         , 198, 0  , 354, 138), // #1227
  INST(Vpandnq          , VexRvm_Lx          , E(660F00,DF,_,x,_,1,4,FV ), 0                         , 135, 0  , 355, 138), // #1228
  INST(Vpandq           , VexRvm_Lx          , E(660F00,DB,_,x,_,1,4,FV ), 0                         , 135, 0  , 356, 138), // #1229
  INST(Vpavgb           , VexRvm_Lx          , V(660F00,E0,_,x,I,I,4,FVM), 0                         , 144, 0  , 315, 163), // #1230
  INST(Vpavgw           , VexRvm_Lx          , V(660F00,E3,_,x,I,I,4,FVM), 0                         , 144, 0  , 315, 163), // #1231
  INST(Vpblendd         , VexRvmi_Lx         , V(660F3A,02,_,x,0,_,_,_  ), 0                         , 73 , 0  , 214, 141), // #1232
  INST(Vpblendmb        , VexRvm_Lx          , E(660F38,66,_,x,_,0,4,FVM), 0                         , 114, 0  , 357, 146), // #1233
  INST(Vpblendmd        , VexRvm_Lx          , E(660F38,64,_,x,_,0,4,FV ), 0                         , 114, 0  , 213, 138), // #1234
  INST(Vpblendmq        , VexRvm_Lx          , E(660F38,64,_,x,_,1,4,FV ), 0                         , 113, 0  , 212, 138), // #1235
  INST(Vpblendmw        , VexRvm_Lx          , E(660F38,66,_,x,_,1,4,FVM), 0                         , 113, 0  , 357, 146), // #1236
  INST(Vpblendvb        , VexRvmr_Lx         , V(660F3A,4C,_,x,0,_,_,_  ), 0                         , 73 , 0  , 215, 160), // #1237
  INST(Vpblendw         , VexRvmi_Lx         , V(660F3A,0E,_,x,I,_,_,_  ), 0                         , 73 , 0  , 214, 160), // #1238
  INST(Vpbroadcastb     , VexRm_Lx_Bcst      , V(660F38,78,_,x,0,0,0,T1S), E(660F38,7A,_,x,0,0,0,T1S), 96 , 108, 358, 164), // #1239
  INST(Vpbroadcastd     , VexRm_Lx_Bcst      , V(660F38,58,_,x,0,0,2,T1S), E(660F38,7C,_,x,0,0,0,T1S), 122, 109, 359, 152), // #1240
  INST(Vpbroadcastmb2q  , VexRm_Lx           , E(F30F38,2A,_,x,_,1,_,_  ), 0                         , 205, 0  , 360, 165), // #1241
  INST(Vpbroadcastmw2d  , VexRm_Lx           , E(F30F38,3A,_,x,_,0,_,_  ), 0                         , 206, 0  , 360, 165), // #1242
  INST(Vpbroadcastq     , VexRm_Lx_Bcst      , V(660F38,59,_,x,0,1,3,T1S), E(660F38,7C,_,x,0,1,0,T1S), 121, 110, 361, 152), // #1243
  INST(Vpbroadcastw     , VexRm_Lx_Bcst      , V(660F38,79,_,x,0,0,1,T1S), E(660F38,7B,_,x,0,0,0,T1S), 207, 111, 362, 164), // #1244
  INST(Vpclmulqdq       , VexRvmi_Lx         , V(660F3A,44,_,x,I,_,4,FVM), 0                         , 204, 0  , 363, 166), // #1245
  INST(Vpcmov           , VexRvrmRvmr_Lx     , V(XOP_M8,A2,_,x,x,_,_,_  ), 0                         , 208, 0  , 289, 151), // #1246
  INST(Vpcmpb           , VexRvmi_Lx         , E(660F3A,3F,_,x,_,0,4,FVM), 0                         , 111, 0  , 364, 146), // #1247
  INST(Vpcmpd           , VexRvmi_Lx         , E(660F3A,1F,_,x,_,0,4,FV ), 0                         , 111, 0  , 365, 138), // #1248
  INST(Vpcmpeqb         , VexRvm_Lx_KEvex    , V(660F00,74,_,x,I,I,4,FV ), 0                         , 144, 0  , 366, 163), // #1249
  INST(Vpcmpeqd         , VexRvm_Lx_KEvex    , V(660F00,76,_,x,I,0,4,FVM), 0                         , 144, 0  , 367, 142), // #1250
  INST(Vpcmpeqq         , VexRvm_Lx_KEvex    , V(660F38,29,_,x,I,1,4,FVM), 0                         , 209, 0  , 368, 142), // #1251
  INST(Vpcmpeqw         , VexRvm_Lx_KEvex    , V(660F00,75,_,x,I,I,4,FV ), 0                         , 144, 0  , 366, 163), // #1252
  INST(Vpcmpestri       , VexRmi             , V(660F3A,61,_,0,I,_,_,_  ), 0                         , 73 , 0  , 369, 167), // #1253
  INST(Vpcmpestrm       , VexRmi             , V(660F3A,60,_,0,I,_,_,_  ), 0                         , 73 , 0  , 370, 167), // #1254
  INST(Vpcmpgtb         , VexRvm_Lx_KEvex    , V(660F00,64,_,x,I,I,4,FV ), 0                         , 144, 0  , 366, 163), // #1255
  INST(Vpcmpgtd         , VexRvm_Lx_KEvex    , V(660F00,66,_,x,I,0,4,FVM), 0                         , 144, 0  , 367, 142), // #1256
  INST(Vpcmpgtq         , VexRvm_Lx_KEvex    , V(660F38,37,_,x,I,1,4,FVM), 0                         , 209, 0  , 368, 142), // #1257
  INST(Vpcmpgtw         , VexRvm_Lx_KEvex    , V(660F00,65,_,x,I,I,4,FV ), 0                         , 144, 0  , 366, 163), // #1258
  INST(Vpcmpistri       , VexRmi             , V(660F3A,63,_,0,I,_,_,_  ), 0                         , 73 , 0  , 371, 167), // #1259
  INST(Vpcmpistrm       , VexRmi             , V(660F3A,62,_,0,I,_,_,_  ), 0                         , 73 , 0  , 372, 167), // #1260
  INST(Vpcmpq           , VexRvmi_Lx         , E(660F3A,1F,_,x,_,1,4,FV ), 0                         , 112, 0  , 373, 138), // #1261
  INST(Vpcmpub          , VexRvmi_Lx         , E(660F3A,3E,_,x,_,0,4,FVM), 0                         , 111, 0  , 364, 146), // #1262
  INST(Vpcmpud          , VexRvmi_Lx         , E(660F3A,1E,_,x,_,0,4,FV ), 0                         , 111, 0  , 365, 138), // #1263
  INST(Vpcmpuq          , VexRvmi_Lx         , E(660F3A,1E,_,x,_,1,4,FV ), 0                         , 112, 0  , 373, 138), // #1264
  INST(Vpcmpuw          , VexRvmi_Lx         , E(660F3A,3E,_,x,_,1,4,FVM), 0                         , 112, 0  , 373, 146), // #1265
  INST(Vpcmpw           , VexRvmi_Lx         , E(660F3A,3F,_,x,_,1,4,FVM), 0                         , 112, 0  , 373, 146), // #1266
  INST(Vpcomb           , VexRvmi            , V(XOP_M8,CC,_,0,0,_,_,_  ), 0                         , 208, 0  , 276, 151), // #1267
  INST(Vpcomd           , VexRvmi            , V(XOP_M8,CE,_,0,0,_,_,_  ), 0                         , 208, 0  , 276, 151), // #1268
  INST(Vpcompressb      , VexMr_Lx           , E(660F38,63,_,x,_,0,0,T1S), 0                         , 210, 0  , 232, 168), // #1269
  INST(Vpcompressd      , VexMr_Lx           , E(660F38,8B,_,x,_,0,2,T1S), 0                         , 129, 0  , 232, 138), // #1270
  INST(Vpcompressq      , VexMr_Lx           , E(660F38,8B,_,x,_,1,3,T1S), 0                         , 128, 0  , 232, 138), // #1271
  INST(Vpcompressw      , VexMr_Lx           , E(660F38,63,_,x,_,1,1,T1S), 0                         , 211, 0  , 232, 168), // #1272
  INST(Vpcomq           , VexRvmi            , V(XOP_M8,CF,_,0,0,_,_,_  ), 0                         , 208, 0  , 276, 151), // #1273
  INST(Vpcomub          , VexRvmi            , V(XOP_M8,EC,_,0,0,_,_,_  ), 0                         , 208, 0  , 276, 151), // #1274
  INST(Vpcomud          , VexRvmi            , V(XOP_M8,EE,_,0,0,_,_,_  ), 0                         , 208, 0  , 276, 151), // #1275
  INST(Vpcomuq          , VexRvmi            , V(XOP_M8,EF,_,0,0,_,_,_  ), 0                         , 208, 0  , 276, 151), // #1276
  INST(Vpcomuw          , VexRvmi            , V(XOP_M8,ED,_,0,0,_,_,_  ), 0                         , 208, 0  , 276, 151), // #1277
  INST(Vpcomw           , VexRvmi            , V(XOP_M8,CD,_,0,0,_,_,_  ), 0                         , 208, 0  , 276, 151), // #1278
  INST(Vpconflictd      , VexRm_Lx           , E(660F38,C4,_,x,_,0,4,FV ), 0                         , 114, 0  , 374, 165), // #1279
  INST(Vpconflictq      , VexRm_Lx           , E(660F38,C4,_,x,_,1,4,FV ), 0                         , 113, 0  , 374, 165), // #1280
  INST(Vpdpbusd         , VexRvm_Lx          , V(660F38,50,_,x,_,0,4,FV ), 0                         , 110, 0  , 375, 169), // #1281
  INST(Vpdpbusds        , VexRvm_Lx          , V(660F38,51,_,x,_,0,4,FV ), 0                         , 110, 0  , 375, 169), // #1282
  INST(Vpdpwssd         , VexRvm_Lx          , V(660F38,52,_,x,_,0,4,FV ), 0                         , 110, 0  , 375, 169), // #1283
  INST(Vpdpwssds        , VexRvm_Lx          , V(660F38,53,_,x,_,0,4,FV ), 0                         , 110, 0  , 375, 169), // #1284
  INST(Vperm2f128       , VexRvmi            , V(660F3A,06,_,1,0,_,_,_  ), 0                         , 172, 0  , 376, 135), // #1285
  INST(Vperm2i128       , VexRvmi            , V(660F3A,46,_,1,0,_,_,_  ), 0                         , 172, 0  , 376, 141), // #1286
  INST(Vpermb           , VexRvm_Lx          , E(660F38,8D,_,x,_,0,4,FVM), 0                         , 114, 0  , 357, 170), // #1287
  INST(Vpermd           , VexRvm_Lx          , V(660F38,36,_,x,0,0,4,FV ), 0                         , 110, 0  , 377, 152), // #1288
  INST(Vpermi2b         , VexRvm_Lx          , E(660F38,75,_,x,_,0,4,FVM), 0                         , 114, 0  , 357, 170), // #1289
  INST(Vpermi2d         , VexRvm_Lx          , E(660F38,76,_,x,_,0,4,FV ), 0                         , 114, 0  , 213, 138), // #1290
  INST(Vpermi2pd        , VexRvm_Lx          , E(660F38,77,_,x,_,1,4,FV ), 0                         , 113, 0  , 212, 138), // #1291
  INST(Vpermi2ps        , VexRvm_Lx          , E(660F38,77,_,x,_,0,4,FV ), 0                         , 114, 0  , 213, 138), // #1292
  INST(Vpermi2q         , VexRvm_Lx          , E(660F38,76,_,x,_,1,4,FV ), 0                         , 113, 0  , 212, 138), // #1293
  INST(Vpermi2w         , VexRvm_Lx          , E(660F38,75,_,x,_,1,4,FVM), 0                         , 113, 0  , 357, 146), // #1294
  INST(Vpermil2pd       , VexRvrmiRvmri_Lx   , V(660F3A,49,_,x,x,_,_,_  ), 0                         , 73 , 0  , 378, 151), // #1295
  INST(Vpermil2ps       , VexRvrmiRvmri_Lx   , V(660F3A,48,_,x,x,_,_,_  ), 0                         , 73 , 0  , 378, 151), // #1296
  INST(Vpermilpd        , VexRvmRmi_Lx       , V(660F38,0D,_,x,0,1,4,FV ), V(660F3A,05,_,x,0,1,4,FV ), 209, 112, 379, 131), // #1297
  INST(Vpermilps        , VexRvmRmi_Lx       , V(660F38,0C,_,x,0,0,4,FV ), V(660F3A,04,_,x,0,0,4,FV ), 110, 113, 380, 131), // #1298
  INST(Vpermpd          , VexRvmRmi_Lx       , E(660F38,16,_,x,1,1,4,FV ), V(660F3A,01,_,x,1,1,4,FV ), 212, 114, 381, 152), // #1299
  INST(Vpermps          , VexRvm_Lx          , V(660F38,16,_,x,0,0,4,FV ), 0                         , 110, 0  , 377, 152), // #1300
  INST(Vpermq           , VexRvmRmi_Lx       , E(660F38,36,_,x,_,1,4,FV ), V(660F3A,00,_,x,1,1,4,FV ), 113, 115, 381, 152), // #1301
  INST(Vpermt2b         , VexRvm_Lx          , E(660F38,7D,_,x,_,0,4,FVM), 0                         , 114, 0  , 357, 170), // #1302
  INST(Vpermt2d         , VexRvm_Lx          , E(660F38,7E,_,x,_,0,4,FV ), 0                         , 114, 0  , 213, 138), // #1303
  INST(Vpermt2pd        , VexRvm_Lx          , E(660F38,7F,_,x,_,1,4,FV ), 0                         , 113, 0  , 212, 138), // #1304
  INST(Vpermt2ps        , VexRvm_Lx          , E(660F38,7F,_,x,_,0,4,FV ), 0                         , 114, 0  , 213, 138), // #1305
  INST(Vpermt2q         , VexRvm_Lx          , E(660F38,7E,_,x,_,1,4,FV ), 0                         , 113, 0  , 212, 138), // #1306
  INST(Vpermt2w         , VexRvm_Lx          , E(660F38,7D,_,x,_,1,4,FVM), 0                         , 113, 0  , 357, 146), // #1307
  INST(Vpermw           , VexRvm_Lx          , E(660F38,8D,_,x,_,1,4,FVM), 0                         , 113, 0  , 357, 146), // #1308
  INST(Vpexpandb        , VexRm_Lx           , E(660F38,62,_,x,_,0,0,T1S), 0                         , 210, 0  , 279, 168), // #1309
  INST(Vpexpandd        , VexRm_Lx           , E(660F38,89,_,x,_,0,2,T1S), 0                         , 129, 0  , 279, 138), // #1310
  INST(Vpexpandq        , VexRm_Lx           , E(660F38,89,_,x,_,1,3,T1S), 0                         , 128, 0  , 279, 138), // #1311
  INST(Vpexpandw        , VexRm_Lx           , E(660F38,62,_,x,_,1,1,T1S), 0                         , 211, 0  , 279, 168), // #1312
  INST(Vpextrb          , VexMri             , V(660F3A,14,_,0,0,I,0,T1S), 0                         , 73 , 0  , 382, 171), // #1313
  INST(Vpextrd          , VexMri             , V(660F3A,16,_,0,0,0,2,T1S), 0                         , 177, 0  , 283, 172), // #1314
  INST(Vpextrq          , VexMri             , V(660F3A,16,_,0,1,1,3,T1S), 0                         , 213, 0  , 383, 172), // #1315
  INST(Vpextrw          , VexMri_Vpextrw     , V(660F3A,15,_,0,0,I,1,T1S), 0                         , 214, 0  , 384, 171), // #1316
  INST(Vpgatherdd       , VexRmvRm_VM        , V(660F38,90,_,x,0,_,_,_  ), E(660F38,90,_,x,_,0,2,T1S), 96 , 116, 302, 152), // #1317
  INST(Vpgatherdq       , VexRmvRm_VM        , V(660F38,90,_,x,1,_,_,_  ), E(660F38,90,_,x,_,1,3,T1S), 189, 117, 301, 152), // #1318
  INST(Vpgatherqd       , VexRmvRm_VM        , V(660F38,91,_,x,0,_,_,_  ), E(660F38,91,_,x,_,0,2,T1S), 96 , 118, 307, 152), // #1319
  INST(Vpgatherqq       , VexRmvRm_VM        , V(660F38,91,_,x,1,_,_,_  ), E(660F38,91,_,x,_,1,3,T1S), 189, 119, 306, 152), // #1320
  INST(Vphaddbd         , VexRm              , V(XOP_M9,C2,_,0,0,_,_,_  ), 0                         , 79 , 0  , 204, 151), // #1321
  INST(Vphaddbq         , VexRm              , V(XOP_M9,C3,_,0,0,_,_,_  ), 0                         , 79 , 0  , 204, 151), // #1322
  INST(Vphaddbw         , VexRm              , V(XOP_M9,C1,_,0,0,_,_,_  ), 0                         , 79 , 0  , 204, 151), // #1323
  INST(Vphaddd          , VexRvm_Lx          , V(660F38,02,_,x,I,_,_,_  ), 0                         , 96 , 0  , 202, 160), // #1324
  INST(Vphadddq         , VexRm              , V(XOP_M9,CB,_,0,0,_,_,_  ), 0                         , 79 , 0  , 204, 151), // #1325
  INST(Vphaddsw         , VexRvm_Lx          , V(660F38,03,_,x,I,_,_,_  ), 0                         , 96 , 0  , 202, 160), // #1326
  INST(Vphaddubd        , VexRm              , V(XOP_M9,D2,_,0,0,_,_,_  ), 0                         , 79 , 0  , 204, 151), // #1327
  INST(Vphaddubq        , VexRm              , V(XOP_M9,D3,_,0,0,_,_,_  ), 0                         , 79 , 0  , 204, 151), // #1328
  INST(Vphaddubw        , VexRm              , V(XOP_M9,D1,_,0,0,_,_,_  ), 0                         , 79 , 0  , 204, 151), // #1329
  INST(Vphaddudq        , VexRm              , V(XOP_M9,DB,_,0,0,_,_,_  ), 0                         , 79 , 0  , 204, 151), // #1330
  INST(Vphadduwd        , VexRm              , V(XOP_M9,D6,_,0,0,_,_,_  ), 0                         , 79 , 0  , 204, 151), // #1331
  INST(Vphadduwq        , VexRm              , V(XOP_M9,D7,_,0,0,_,_,_  ), 0                         , 79 , 0  , 204, 151), // #1332
  INST(Vphaddw          , VexRvm_Lx          , V(660F38,01,_,x,I,_,_,_  ), 0                         , 96 , 0  , 202, 160), // #1333
  INST(Vphaddwd         , VexRm              , V(XOP_M9,C6,_,0,0,_,_,_  ), 0                         , 79 , 0  , 204, 151), // #1334
  INST(Vphaddwq         , VexRm              , V(XOP_M9,C7,_,0,0,_,_,_  ), 0                         , 79 , 0  , 204, 151), // #1335
  INST(Vphminposuw      , VexRm              , V(660F38,41,_,0,I,_,_,_  ), 0                         , 96 , 0  , 204, 135), // #1336
  INST(Vphsubbw         , VexRm              , V(XOP_M9,E1,_,0,0,_,_,_  ), 0                         , 79 , 0  , 204, 151), // #1337
  INST(Vphsubd          , VexRvm_Lx          , V(660F38,06,_,x,I,_,_,_  ), 0                         , 96 , 0  , 202, 160), // #1338
  INST(Vphsubdq         , VexRm              , V(XOP_M9,E3,_,0,0,_,_,_  ), 0                         , 79 , 0  , 204, 151), // #1339
  INST(Vphsubsw         , VexRvm_Lx          , V(660F38,07,_,x,I,_,_,_  ), 0                         , 96 , 0  , 202, 160), // #1340
  INST(Vphsubw          , VexRvm_Lx          , V(660F38,05,_,x,I,_,_,_  ), 0                         , 96 , 0  , 202, 160), // #1341
  INST(Vphsubwd         , VexRm              , V(XOP_M9,E2,_,0,0,_,_,_  ), 0                         , 79 , 0  , 204, 151), // #1342
  INST(Vpinsrb          , VexRvmi            , V(660F3A,20,_,0,0,I,0,T1S), 0                         , 73 , 0  , 385, 171), // #1343
  INST(Vpinsrd          , VexRvmi            , V(660F3A,22,_,0,0,0,2,T1S), 0                         , 177, 0  , 386, 172), // #1344
  INST(Vpinsrq          , VexRvmi            , V(660F3A,22,_,0,1,1,3,T1S), 0                         , 213, 0  , 387, 172), // #1345
  INST(Vpinsrw          , VexRvmi            , V(660F00,C4,_,0,0,I,1,T1S), 0                         , 215, 0  , 388, 171), // #1346
  INST(Vplzcntd         , VexRm_Lx           , E(660F38,44,_,x,_,0,4,FV ), 0                         , 114, 0  , 374, 165), // #1347
  INST(Vplzcntq         , VexRm_Lx           , E(660F38,44,_,x,_,1,4,FV ), 0                         , 113, 0  , 350, 165), // #1348
  INST(Vpmacsdd         , VexRvmr            , V(XOP_M8,9E,_,0,0,_,_,_  ), 0                         , 208, 0  , 389, 151), // #1349
  INST(Vpmacsdqh        , VexRvmr            , V(XOP_M8,9F,_,0,0,_,_,_  ), 0                         , 208, 0  , 389, 151), // #1350
  INST(Vpmacsdql        , VexRvmr            , V(XOP_M8,97,_,0,0,_,_,_  ), 0                         , 208, 0  , 389, 151), // #1351
  INST(Vpmacssdd        , VexRvmr            , V(XOP_M8,8E,_,0,0,_,_,_  ), 0                         , 208, 0  , 389, 151), // #1352
  INST(Vpmacssdqh       , VexRvmr            , V(XOP_M8,8F,_,0,0,_,_,_  ), 0                         , 208, 0  , 389, 151), // #1353
  INST(Vpmacssdql       , VexRvmr            , V(XOP_M8,87,_,0,0,_,_,_  ), 0                         , 208, 0  , 389, 151), // #1354
  INST(Vpmacsswd        , VexRvmr            , V(XOP_M8,86,_,0,0,_,_,_  ), 0                         , 208, 0  , 389, 151), // #1355
  INST(Vpmacssww        , VexRvmr            , V(XOP_M8,85,_,0,0,_,_,_  ), 0                         , 208, 0  , 389, 151), // #1356
  INST(Vpmacswd         , VexRvmr            , V(XOP_M8,96,_,0,0,_,_,_  ), 0                         , 208, 0  , 389, 151), // #1357
  INST(Vpmacsww         , VexRvmr            , V(XOP_M8,95,_,0,0,_,_,_  ), 0                         , 208, 0  , 389, 151), // #1358
  INST(Vpmadcsswd       , VexRvmr            , V(XOP_M8,A6,_,0,0,_,_,_  ), 0                         , 208, 0  , 389, 151), // #1359
  INST(Vpmadcswd        , VexRvmr            , V(XOP_M8,B6,_,0,0,_,_,_  ), 0                         , 208, 0  , 389, 151), // #1360
  INST(Vpmadd52huq      , VexRvm_Lx          , E(660F38,B5,_,x,_,1,4,FV ), 0                         , 113, 0  , 212, 173), // #1361
  INST(Vpmadd52luq      , VexRvm_Lx          , E(660F38,B4,_,x,_,1,4,FV ), 0                         , 113, 0  , 212, 173), // #1362
  INST(Vpmaddubsw       , VexRvm_Lx          , V(660F38,04,_,x,I,I,4,FVM), 0                         , 110, 0  , 315, 163), // #1363
  INST(Vpmaddwd         , VexRvm_Lx          , V(660F00,F5,_,x,I,I,4,FVM), 0                         , 144, 0  , 315, 163), // #1364
  INST(Vpmaskmovd       , VexRvmMvr_Lx       , V(660F38,8C,_,x,0,_,_,_  ), V(660F38,8E,_,x,0,_,_,_  ), 96 , 120, 323, 141), // #1365
  INST(Vpmaskmovq       , VexRvmMvr_Lx       , V(660F38,8C,_,x,1,_,_,_  ), V(660F38,8E,_,x,1,_,_,_  ), 189, 121, 323, 141), // #1366
  INST(Vpmaxsb          , VexRvm_Lx          , V(660F38,3C,_,x,I,I,4,FVM), 0                         , 110, 0  , 390, 163), // #1367
  INST(Vpmaxsd          , VexRvm_Lx          , V(660F38,3D,_,x,I,0,4,FV ), 0                         , 110, 0  , 211, 142), // #1368
  INST(Vpmaxsq          , VexRvm_Lx          , E(660F38,3D,_,x,_,1,4,FV ), 0                         , 113, 0  , 212, 138), // #1369
  INST(Vpmaxsw          , VexRvm_Lx          , V(660F00,EE,_,x,I,I,4,FVM), 0                         , 144, 0  , 390, 163), // #1370
  INST(Vpmaxub          , VexRvm_Lx          , V(660F00,DE,_,x,I,I,4,FVM), 0                         , 144, 0  , 390, 163), // #1371
  INST(Vpmaxud          , VexRvm_Lx          , V(660F38,3F,_,x,I,0,4,FV ), 0                         , 110, 0  , 211, 142), // #1372
  INST(Vpmaxuq          , VexRvm_Lx          , E(660F38,3F,_,x,_,1,4,FV ), 0                         , 113, 0  , 212, 138), // #1373
  INST(Vpmaxuw          , VexRvm_Lx          , V(660F38,3E,_,x,I,I,4,FVM), 0                         , 110, 0  , 390, 163), // #1374
  INST(Vpminsb          , VexRvm_Lx          , V(660F38,38,_,x,I,I,4,FVM), 0                         , 110, 0  , 390, 163), // #1375
  INST(Vpminsd          , VexRvm_Lx          , V(660F38,39,_,x,I,0,4,FV ), 0                         , 110, 0  , 211, 142), // #1376
  INST(Vpminsq          , VexRvm_Lx          , E(660F38,39,_,x,_,1,4,FV ), 0                         , 113, 0  , 212, 138), // #1377
  INST(Vpminsw          , VexRvm_Lx          , V(660F00,EA,_,x,I,I,4,FVM), 0                         , 144, 0  , 390, 163), // #1378
  INST(Vpminub          , VexRvm_Lx          , V(660F00,DA,_,x,I,_,4,FVM), 0                         , 144, 0  , 390, 163), // #1379
  INST(Vpminud          , VexRvm_Lx          , V(660F38,3B,_,x,I,0,4,FV ), 0                         , 110, 0  , 211, 142), // #1380
  INST(Vpminuq          , VexRvm_Lx          , E(660F38,3B,_,x,_,1,4,FV ), 0                         , 113, 0  , 212, 138), // #1381
  INST(Vpminuw          , VexRvm_Lx          , V(660F38,3A,_,x,I,_,4,FVM), 0                         , 110, 0  , 390, 163), // #1382
  INST(Vpmovb2m         , VexRm_Lx           , E(F30F38,29,_,x,_,0,_,_  ), 0                         , 206, 0  , 391, 146), // #1383
  INST(Vpmovd2m         , VexRm_Lx           , E(F30F38,39,_,x,_,0,_,_  ), 0                         , 206, 0  , 391, 140), // #1384
  INST(Vpmovdb          , VexMr_Lx           , E(F30F38,31,_,x,_,0,2,QVM), 0                         , 216, 0  , 392, 138), // #1385
  INST(Vpmovdw          , VexMr_Lx           , E(F30F38,33,_,x,_,0,3,HVM), 0                         , 217, 0  , 393, 138), // #1386
  INST(Vpmovm2b         , VexRm_Lx           , E(F30F38,28,_,x,_,0,_,_  ), 0                         , 206, 0  , 360, 146), // #1387
  INST(Vpmovm2d         , VexRm_Lx           , E(F30F38,38,_,x,_,0,_,_  ), 0                         , 206, 0  , 360, 140), // #1388
  INST(Vpmovm2q         , VexRm_Lx           , E(F30F38,38,_,x,_,1,_,_  ), 0                         , 205, 0  , 360, 140), // #1389
  INST(Vpmovm2w         , VexRm_Lx           , E(F30F38,28,_,x,_,1,_,_  ), 0                         , 205, 0  , 360, 146), // #1390
  INST(Vpmovmskb        , VexRm_Lx           , V(660F00,D7,_,x,I,_,_,_  ), 0                         , 69 , 0  , 336, 160), // #1391
  INST(Vpmovq2m         , VexRm_Lx           , E(F30F38,39,_,x,_,1,_,_  ), 0                         , 205, 0  , 391, 140), // #1392
  INST(Vpmovqb          , VexMr_Lx           , E(F30F38,32,_,x,_,0,1,OVM), 0                         , 218, 0  , 394, 138), // #1393
  INST(Vpmovqd          , VexMr_Lx           , E(F30F38,35,_,x,_,0,3,HVM), 0                         , 217, 0  , 393, 138), // #1394
  INST(Vpmovqw          , VexMr_Lx           , E(F30F38,34,_,x,_,0,2,QVM), 0                         , 216, 0  , 392, 138), // #1395
  INST(Vpmovsdb         , VexMr_Lx           , E(F30F38,21,_,x,_,0,2,QVM), 0                         , 216, 0  , 392, 138), // #1396
  INST(Vpmovsdw         , VexMr_Lx           , E(F30F38,23,_,x,_,0,3,HVM), 0                         , 217, 0  , 393, 138), // #1397
  INST(Vpmovsqb         , VexMr_Lx           , E(F30F38,22,_,x,_,0,1,OVM), 0                         , 218, 0  , 394, 138), // #1398
  INST(Vpmovsqd         , VexMr_Lx           , E(F30F38,25,_,x,_,0,3,HVM), 0                         , 217, 0  , 393, 138), // #1399
  INST(Vpmovsqw         , VexMr_Lx           , E(F30F38,24,_,x,_,0,2,QVM), 0                         , 216, 0  , 392, 138), // #1400
  INST(Vpmovswb         , VexMr_Lx           , E(F30F38,20,_,x,_,0,3,HVM), 0                         , 217, 0  , 393, 146), // #1401
  INST(Vpmovsxbd        , VexRm_Lx           , V(660F38,21,_,x,I,I,2,QVM), 0                         , 219, 0  , 395, 142), // #1402
  INST(Vpmovsxbq        , VexRm_Lx           , V(660F38,22,_,x,I,I,1,OVM), 0                         , 220, 0  , 396, 142), // #1403
  INST(Vpmovsxbw        , VexRm_Lx           , V(660F38,20,_,x,I,I,3,HVM), 0                         , 139, 0  , 397, 163), // #1404
  INST(Vpmovsxdq        , VexRm_Lx           , V(660F38,25,_,x,I,0,3,HVM), 0                         , 139, 0  , 397, 142), // #1405
  INST(Vpmovsxwd        , VexRm_Lx           , V(660F38,23,_,x,I,I,3,HVM), 0                         , 139, 0  , 397, 142), // #1406
  INST(Vpmovsxwq        , VexRm_Lx           , V(660F38,24,_,x,I,I,2,QVM), 0                         , 219, 0  , 395, 142), // #1407
  INST(Vpmovusdb        , VexMr_Lx           , E(F30F38,11,_,x,_,0,2,QVM), 0                         , 216, 0  , 392, 138), // #1408
  INST(Vpmovusdw        , VexMr_Lx           , E(F30F38,13,_,x,_,0,3,HVM), 0                         , 217, 0  , 393, 138), // #1409
  INST(Vpmovusqb        , VexMr_Lx           , E(F30F38,12,_,x,_,0,1,OVM), 0                         , 218, 0  , 394, 138), // #1410
  INST(Vpmovusqd        , VexMr_Lx           , E(F30F38,15,_,x,_,0,3,HVM), 0                         , 217, 0  , 393, 138), // #1411
  INST(Vpmovusqw        , VexMr_Lx           , E(F30F38,14,_,x,_,0,2,QVM), 0                         , 216, 0  , 392, 138), // #1412
  INST(Vpmovuswb        , VexMr_Lx           , E(F30F38,10,_,x,_,0,3,HVM), 0                         , 217, 0  , 393, 146), // #1413
  INST(Vpmovw2m         , VexRm_Lx           , E(F30F38,29,_,x,_,1,_,_  ), 0                         , 205, 0  , 391, 146), // #1414
  INST(Vpmovwb          , VexMr_Lx           , E(F30F38,30,_,x,_,0,3,HVM), 0                         , 217, 0  , 393, 146), // #1415
  INST(Vpmovzxbd        , VexRm_Lx           , V(660F38,31,_,x,I,I,2,QVM), 0                         , 219, 0  , 395, 142), // #1416
  INST(Vpmovzxbq        , VexRm_Lx           , V(660F38,32,_,x,I,I,1,OVM), 0                         , 220, 0  , 396, 142), // #1417
  INST(Vpmovzxbw        , VexRm_Lx           , V(660F38,30,_,x,I,I,3,HVM), 0                         , 139, 0  , 397, 163), // #1418
  INST(Vpmovzxdq        , VexRm_Lx           , V(660F38,35,_,x,I,0,3,HVM), 0                         , 139, 0  , 397, 142), // #1419
  INST(Vpmovzxwd        , VexRm_Lx           , V(660F38,33,_,x,I,I,3,HVM), 0                         , 139, 0  , 397, 142), // #1420
  INST(Vpmovzxwq        , VexRm_Lx           , V(660F38,34,_,x,I,I,2,QVM), 0                         , 219, 0  , 395, 142), // #1421
  INST(Vpmuldq          , VexRvm_Lx          , V(660F38,28,_,x,I,1,4,FV ), 0                         , 209, 0  , 208, 142), // #1422
  INST(Vpmulhrsw        , VexRvm_Lx          , V(660F38,0B,_,x,I,I,4,FVM), 0                         , 110, 0  , 315, 163), // #1423
  INST(Vpmulhuw         , VexRvm_Lx          , V(660F00,E4,_,x,I,I,4,FVM), 0                         , 144, 0  , 315, 163), // #1424
  INST(Vpmulhw          , VexRvm_Lx          , V(660F00,E5,_,x,I,I,4,FVM), 0                         , 144, 0  , 315, 163), // #1425
  INST(Vpmulld          , VexRvm_Lx          , V(660F38,40,_,x,I,0,4,FV ), 0                         , 110, 0  , 209, 142), // #1426
  INST(Vpmullq          , VexRvm_Lx          , E(660F38,40,_,x,_,1,4,FV ), 0                         , 113, 0  , 212, 140), // #1427
  INST(Vpmullw          , VexRvm_Lx          , V(660F00,D5,_,x,I,I,4,FVM), 0                         , 144, 0  , 315, 163), // #1428
  INST(Vpmultishiftqb   , VexRvm_Lx          , E(660F38,83,_,x,_,1,4,FV ), 0                         , 113, 0  , 212, 170), // #1429
  INST(Vpmuludq         , VexRvm_Lx          , V(660F00,F4,_,x,I,1,4,FV ), 0                         , 103, 0  , 208, 142), // #1430
  INST(Vpopcntb         , VexRm_Lx           , E(660F38,54,_,x,_,0,4,FV ), 0                         , 114, 0  , 279, 174), // #1431
  INST(Vpopcntd         , VexRm_Lx           , E(660F38,55,_,x,_,0,4,FVM), 0                         , 114, 0  , 374, 175), // #1432
  INST(Vpopcntq         , VexRm_Lx           , E(660F38,55,_,x,_,1,4,FVM), 0                         , 113, 0  , 350, 175), // #1433
  INST(Vpopcntw         , VexRm_Lx           , E(660F38,54,_,x,_,1,4,FV ), 0                         , 113, 0  , 279, 174), // #1434
  INST(Vpor             , VexRvm_Lx          , V(660F00,EB,_,x,I,_,_,_  ), 0                         , 69 , 0  , 351, 160), // #1435
  INST(Vpord            , VexRvm_Lx          , E(660F00,EB,_,x,_,0,4,FV ), 0                         , 198, 0  , 352, 138), // #1436
  INST(Vporq            , VexRvm_Lx          , E(660F00,EB,_,x,_,1,4,FV ), 0                         , 135, 0  , 356, 138), // #1437
  INST(Vpperm           , VexRvrmRvmr        , V(XOP_M8,A3,_,0,x,_,_,_  ), 0                         , 208, 0  , 398, 151), // #1438
  INST(Vprold           , VexVmi_Lx          , E(660F00,72,1,x,_,0,4,FV ), 0                         , 221, 0  , 399, 138), // #1439
  INST(Vprolq           , VexVmi_Lx          , E(660F00,72,1,x,_,1,4,FV ), 0                         , 222, 0  , 400, 138), // #1440
  INST(Vprolvd          , VexRvm_Lx          , E(660F38,15,_,x,_,0,4,FV ), 0                         , 114, 0  , 213, 138), // #1441
  INST(Vprolvq          , VexRvm_Lx          , E(660F38,15,_,x,_,1,4,FV ), 0                         , 113, 0  , 212, 138), // #1442
  INST(Vprord           , VexVmi_Lx          , E(660F00,72,0,x,_,0,4,FV ), 0                         , 198, 0  , 399, 138), // #1443
  INST(Vprorq           , VexVmi_Lx          , E(660F00,72,0,x,_,1,4,FV ), 0                         , 135, 0  , 400, 138), // #1444
  INST(Vprorvd          , VexRvm_Lx          , E(660F38,14,_,x,_,0,4,FV ), 0                         , 114, 0  , 213, 138), // #1445
  INST(Vprorvq          , VexRvm_Lx          , E(660F38,14,_,x,_,1,4,FV ), 0                         , 113, 0  , 212, 138), // #1446
  INST(Vprotb           , VexRvmRmvRmi       , V(XOP_M9,90,_,0,x,_,_,_  ), V(XOP_M8,C0,_,0,x,_,_,_  ), 79 , 122, 401, 151), // #1447
  INST(Vprotd           , VexRvmRmvRmi       , V(XOP_M9,92,_,0,x,_,_,_  ), V(XOP_M8,C2,_,0,x,_,_,_  ), 79 , 123, 401, 151), // #1448
  INST(Vprotq           , VexRvmRmvRmi       , V(XOP_M9,93,_,0,x,_,_,_  ), V(XOP_M8,C3,_,0,x,_,_,_  ), 79 , 124, 401, 151), // #1449
  INST(Vprotw           , VexRvmRmvRmi       , V(XOP_M9,91,_,0,x,_,_,_  ), V(XOP_M8,C1,_,0,x,_,_,_  ), 79 , 125, 401, 151), // #1450
  INST(Vpsadbw          , VexRvm_Lx          , V(660F00,F6,_,x,I,I,4,FVM), 0                         , 144, 0  , 203, 163), // #1451
  INST(Vpscatterdd      , VexMr_VM           , E(660F38,A0,_,x,_,0,2,T1S), 0                         , 129, 0  , 402, 138), // #1452
  INST(Vpscatterdq      , VexMr_VM           , E(660F38,A0,_,x,_,1,3,T1S), 0                         , 128, 0  , 403, 138), // #1453
  INST(Vpscatterqd      , VexMr_VM           , E(660F38,A1,_,x,_,0,2,T1S), 0                         , 129, 0  , 404, 138), // #1454
  INST(Vpscatterqq      , VexMr_VM           , E(660F38,A1,_,x,_,1,3,T1S), 0                         , 128, 0  , 405, 138), // #1455
  INST(Vpshab           , VexRvmRmv          , V(XOP_M9,98,_,0,x,_,_,_  ), 0                         , 79 , 0  , 406, 151), // #1456
  INST(Vpshad           , VexRvmRmv          , V(XOP_M9,9A,_,0,x,_,_,_  ), 0                         , 79 , 0  , 406, 151), // #1457
  INST(Vpshaq           , VexRvmRmv          , V(XOP_M9,9B,_,0,x,_,_,_  ), 0                         , 79 , 0  , 406, 151), // #1458
  INST(Vpshaw           , VexRvmRmv          , V(XOP_M9,99,_,0,x,_,_,_  ), 0                         , 79 , 0  , 406, 151), // #1459
  INST(Vpshlb           , VexRvmRmv          , V(XOP_M9,94,_,0,x,_,_,_  ), 0                         , 79 , 0  , 406, 151), // #1460
  INST(Vpshld           , VexRvmRmv          , V(XOP_M9,96,_,0,x,_,_,_  ), 0                         , 79 , 0  , 406, 151), // #1461
  INST(Vpshldd          , VexRvmi_Lx         , E(660F3A,71,_,x,_,0,4,FV ), 0                         , 111, 0  , 206, 168), // #1462
  INST(Vpshldq          , VexRvmi_Lx         , E(660F3A,71,_,x,_,1,4,FV ), 0                         , 112, 0  , 207, 168), // #1463
  INST(Vpshldvd         , VexRvm_Lx          , E(660F38,71,_,x,_,0,4,FV ), 0                         , 114, 0  , 213, 168), // #1464
  INST(Vpshldvq         , VexRvm_Lx          , E(660F38,71,_,x,_,1,4,FV ), 0                         , 113, 0  , 212, 168), // #1465
  INST(Vpshldvw         , VexRvm_Lx          , E(660F38,70,_,x,_,1,4,FVM), 0                         , 113, 0  , 357, 168), // #1466
  INST(Vpshldw          , VexRvmi_Lx         , E(660F3A,70,_,x,_,1,4,FVM), 0                         , 112, 0  , 275, 168), // #1467
  INST(Vpshlq           , VexRvmRmv          , V(XOP_M9,97,_,0,x,_,_,_  ), 0                         , 79 , 0  , 406, 151), // #1468
  INST(Vpshlw           , VexRvmRmv          , V(XOP_M9,95,_,0,x,_,_,_  ), 0                         , 79 , 0  , 406, 151), // #1469
  INST(Vpshrdd          , VexRvmi_Lx         , E(660F3A,73,_,x,_,0,4,FV ), 0                         , 111, 0  , 206, 168), // #1470
  INST(Vpshrdq          , VexRvmi_Lx         , E(660F3A,73,_,x,_,1,4,FV ), 0                         , 112, 0  , 207, 168), // #1471
  INST(Vpshrdvd         , VexRvm_Lx          , E(660F38,73,_,x,_,0,4,FV ), 0                         , 114, 0  , 213, 168), // #1472
  INST(Vpshrdvq         , VexRvm_Lx          , E(660F38,73,_,x,_,1,4,FV ), 0                         , 113, 0  , 212, 168), // #1473
  INST(Vpshrdvw         , VexRvm_Lx          , E(660F38,72,_,x,_,1,4,FVM), 0                         , 113, 0  , 357, 168), // #1474
  INST(Vpshrdw          , VexRvmi_Lx         , E(660F3A,72,_,x,_,1,4,FVM), 0                         , 112, 0  , 275, 168), // #1475
  INST(Vpshufb          , VexRvm_Lx          , V(660F38,00,_,x,I,I,4,FVM), 0                         , 110, 0  , 315, 163), // #1476
  INST(Vpshufbitqmb     , VexRvm_Lx          , E(660F38,8F,_,x,0,0,4,FVM), 0                         , 114, 0  , 407, 174), // #1477
  INST(Vpshufd          , VexRmi_Lx          , V(660F00,70,_,x,I,0,4,FV ), 0                         , 144, 0  , 408, 142), // #1478
  INST(Vpshufhw         , VexRmi_Lx          , V(F30F00,70,_,x,I,I,4,FVM), 0                         , 161, 0  , 409, 163), // #1479
  INST(Vpshuflw         , VexRmi_Lx          , V(F20F00,70,_,x,I,I,4,FVM), 0                         , 223, 0  , 409, 163), // #1480
  INST(Vpsignb          , VexRvm_Lx          , V(660F38,08,_,x,I,_,_,_  ), 0                         , 96 , 0  , 202, 160), // #1481
  INST(Vpsignd          , VexRvm_Lx          , V(660F38,0A,_,x,I,_,_,_  ), 0                         , 96 , 0  , 202, 160), // #1482
  INST(Vpsignw          , VexRvm_Lx          , V(660F38,09,_,x,I,_,_,_  ), 0                         , 96 , 0  , 202, 160), // #1483
  INST(Vpslld           , VexRvmVmi_Lx_MEvex , V(660F00,F2,_,x,I,0,4,128), V(660F00,72,6,x,I,0,4,FV ), 224, 126, 410, 142), // #1484
  INST(Vpslldq          , VexVmi_Lx_MEvex    , V(660F00,73,7,x,I,I,4,FVM), 0                         , 225, 0  , 411, 163), // #1485
  INST(Vpsllq           , VexRvmVmi_Lx_MEvex , V(660F00,F3,_,x,I,1,4,128), V(660F00,73,6,x,I,1,4,FV ), 226, 127, 412, 142), // #1486
  INST(Vpsllvd          , VexRvm_Lx          , V(660F38,47,_,x,0,0,4,FV ), 0                         , 110, 0  , 209, 152), // #1487
  INST(Vpsllvq          , VexRvm_Lx          , V(660F38,47,_,x,1,1,4,FV ), 0                         , 182, 0  , 208, 152), // #1488
  INST(Vpsllvw          , VexRvm_Lx          , E(660F38,12,_,x,_,1,4,FVM), 0                         , 113, 0  , 357, 146), // #1489
  INST(Vpsllw           , VexRvmVmi_Lx_MEvex , V(660F00,F1,_,x,I,I,4,128), V(660F00,71,6,x,I,I,4,FVM), 224, 128, 413, 163), // #1490
  INST(Vpsrad           , VexRvmVmi_Lx_MEvex , V(660F00,E2,_,x,I,0,4,128), V(660F00,72,4,x,I,0,4,FV ), 224, 129, 410, 142), // #1491
  INST(Vpsraq           , VexRvmVmi_Lx_MEvex , E(660F00,E2,_,x,_,1,4,128), E(660F00,72,4,x,_,1,4,FV ), 227, 130, 414, 138), // #1492
  INST(Vpsravd          , VexRvm_Lx          , V(660F38,46,_,x,0,0,4,FV ), 0                         , 110, 0  , 209, 152), // #1493
  INST(Vpsravq          , VexRvm_Lx          , E(660F38,46,_,x,_,1,4,FV ), 0                         , 113, 0  , 212, 138), // #1494
  INST(Vpsravw          , VexRvm_Lx          , E(660F38,11,_,x,_,1,4,FVM), 0                         , 113, 0  , 357, 146), // #1495
  INST(Vpsraw           , VexRvmVmi_Lx_MEvex , V(660F00,E1,_,x,I,I,4,128), V(660F00,71,4,x,I,I,4,FVM), 224, 131, 413, 163), // #1496
  INST(Vpsrld           , VexRvmVmi_Lx_MEvex , V(660F00,D2,_,x,I,0,4,128), V(660F00,72,2,x,I,0,4,FV ), 224, 132, 410, 142), // #1497
  INST(Vpsrldq          , VexVmi_Lx_MEvex    , V(660F00,73,3,x,I,I,4,FVM), 0                         , 228, 0  , 411, 163), // #1498
  INST(Vpsrlq           , VexRvmVmi_Lx_MEvex , V(660F00,D3,_,x,I,1,4,128), V(660F00,73,2,x,I,1,4,FV ), 226, 133, 412, 142), // #1499
  INST(Vpsrlvd          , VexRvm_Lx          , V(660F38,45,_,x,0,0,4,FV ), 0                         , 110, 0  , 209, 152), // #1500
  INST(Vpsrlvq          , VexRvm_Lx          , V(660F38,45,_,x,1,1,4,FV ), 0                         , 182, 0  , 208, 152), // #1501
  INST(Vpsrlvw          , VexRvm_Lx          , E(660F38,10,_,x,_,1,4,FVM), 0                         , 113, 0  , 357, 146), // #1502
  INST(Vpsrlw           , VexRvmVmi_Lx_MEvex , V(660F00,D1,_,x,I,I,4,128), V(660F00,71,2,x,I,I,4,FVM), 224, 134, 413, 163), // #1503
  INST(Vpsubb           , VexRvm_Lx          , V(660F00,F8,_,x,I,I,4,FVM), 0                         , 144, 0  , 415, 163), // #1504
  INST(Vpsubd           , VexRvm_Lx          , V(660F00,FA,_,x,I,0,4,FV ), 0                         , 144, 0  , 416, 142), // #1505
  INST(Vpsubq           , VexRvm_Lx          , V(660F00,FB,_,x,I,1,4,FV ), 0                         , 103, 0  , 417, 142), // #1506
  INST(Vpsubsb          , VexRvm_Lx          , V(660F00,E8,_,x,I,I,4,FVM), 0                         , 144, 0  , 415, 163), // #1507
  INST(Vpsubsw          , VexRvm_Lx          , V(660F00,E9,_,x,I,I,4,FVM), 0                         , 144, 0  , 415, 163), // #1508
  INST(Vpsubusb         , VexRvm_Lx          , V(660F00,D8,_,x,I,I,4,FVM), 0                         , 144, 0  , 415, 163), // #1509
  INST(Vpsubusw         , VexRvm_Lx          , V(660F00,D9,_,x,I,I,4,FVM), 0                         , 144, 0  , 415, 163), // #1510
  INST(Vpsubw           , VexRvm_Lx          , V(660F00,F9,_,x,I,I,4,FVM), 0                         , 144, 0  , 415, 163), // #1511
  INST(Vpternlogd       , VexRvmi_Lx         , E(660F3A,25,_,x,_,0,4,FV ), 0                         , 111, 0  , 206, 138), // #1512
  INST(Vpternlogq       , VexRvmi_Lx         , E(660F3A,25,_,x,_,1,4,FV ), 0                         , 112, 0  , 207, 138), // #1513
  INST(Vptest           , VexRm_Lx           , V(660F38,17,_,x,I,_,_,_  ), 0                         , 96 , 0  , 298, 167), // #1514
  INST(Vptestmb         , VexRvm_Lx          , E(660F38,26,_,x,_,0,4,FVM), 0                         , 114, 0  , 407, 146), // #1515
  INST(Vptestmd         , VexRvm_Lx          , E(660F38,27,_,x,_,0,4,FV ), 0                         , 114, 0  , 418, 138), // #1516
  INST(Vptestmq         , VexRvm_Lx          , E(660F38,27,_,x,_,1,4,FV ), 0                         , 113, 0  , 419, 138), // #1517
  INST(Vptestmw         , VexRvm_Lx          , E(660F38,26,_,x,_,1,4,FVM), 0                         , 113, 0  , 407, 146), // #1518
  INST(Vptestnmb        , VexRvm_Lx          , E(F30F38,26,_,x,_,0,4,FVM), 0                         , 132, 0  , 407, 146), // #1519
  INST(Vptestnmd        , VexRvm_Lx          , E(F30F38,27,_,x,_,0,4,FV ), 0                         , 132, 0  , 418, 138), // #1520
  INST(Vptestnmq        , VexRvm_Lx          , E(F30F38,27,_,x,_,1,4,FV ), 0                         , 229, 0  , 419, 138), // #1521
  INST(Vptestnmw        , VexRvm_Lx          , E(F30F38,26,_,x,_,1,4,FVM), 0                         , 229, 0  , 407, 146), // #1522
  INST(Vpunpckhbw       , VexRvm_Lx          , V(660F00,68,_,x,I,I,4,FVM), 0                         , 144, 0  , 315, 163), // #1523
  INST(Vpunpckhdq       , VexRvm_Lx          , V(660F00,6A,_,x,I,0,4,FV ), 0                         , 144, 0  , 209, 142), // #1524
  INST(Vpunpckhqdq      , VexRvm_Lx          , V(660F00,6D,_,x,I,1,4,FV ), 0                         , 103, 0  , 208, 142), // #1525
  INST(Vpunpckhwd       , VexRvm_Lx          , V(660F00,69,_,x,I,I,4,FVM), 0                         , 144, 0  , 315, 163), // #1526
  INST(Vpunpcklbw       , VexRvm_Lx          , V(660F00,60,_,x,I,I,4,FVM), 0                         , 144, 0  , 315, 163), // #1527
  INST(Vpunpckldq       , VexRvm_Lx          , V(660F00,62,_,x,I,0,4,FV ), 0                         , 144, 0  , 209, 142), // #1528
  INST(Vpunpcklqdq      , VexRvm_Lx          , V(660F00,6C,_,x,I,1,4,FV ), 0                         , 103, 0  , 208, 142), // #1529
  INST(Vpunpcklwd       , VexRvm_Lx          , V(660F00,61,_,x,I,I,4,FVM), 0                         , 144, 0  , 315, 163), // #1530
  INST(Vpxor            , VexRvm_Lx          , V(660F00,EF,_,x,I,_,_,_  ), 0                         , 69 , 0  , 353, 160), // #1531
  INST(Vpxord           , VexRvm_Lx          , E(660F00,EF,_,x,_,0,4,FV ), 0                         , 198, 0  , 354, 138), // #1532
  INST(Vpxorq           , VexRvm_Lx          , E(660F00,EF,_,x,_,1,4,FV ), 0                         , 135, 0  , 355, 138), // #1533
  INST(Vrangepd         , VexRvmi_Lx         , E(660F3A,50,_,x,_,1,4,FV ), 0                         , 112, 0  , 285, 140), // #1534
  INST(Vrangeps         , VexRvmi_Lx         , E(660F3A,50,_,x,_,0,4,FV ), 0                         , 111, 0  , 286, 140), // #1535
  INST(Vrangesd         , VexRvmi            , E(660F3A,51,_,I,_,1,3,T1S), 0                         , 180, 0  , 287, 66 ), // #1536
  INST(Vrangess         , VexRvmi            , E(660F3A,51,_,I,_,0,2,T1S), 0                         , 181, 0  , 288, 66 ), // #1537
  INST(Vrcp14pd         , VexRm_Lx           , E(660F38,4C,_,x,_,1,4,FV ), 0                         , 113, 0  , 350, 138), // #1538
  INST(Vrcp14ps         , VexRm_Lx           , E(660F38,4C,_,x,_,0,4,FV ), 0                         , 114, 0  , 374, 138), // #1539
  INST(Vrcp14sd         , VexRvm             , E(660F38,4D,_,I,_,1,3,T1S), 0                         , 128, 0  , 420, 68 ), // #1540
  INST(Vrcp14ss         , VexRvm             , E(660F38,4D,_,I,_,0,2,T1S), 0                         , 129, 0  , 421, 68 ), // #1541
  INST(Vrcp28pd         , VexRm              , E(660F38,CA,_,2,_,1,4,FV ), 0                         , 170, 0  , 277, 147), // #1542
  INST(Vrcp28ps         , VexRm              , E(660F38,CA,_,2,_,0,4,FV ), 0                         , 171, 0  , 278, 147), // #1543
  INST(Vrcp28sd         , VexRvm             , E(660F38,CB,_,I,_,1,3,T1S), 0                         , 128, 0  , 308, 147), // #1544
  INST(Vrcp28ss         , VexRvm             , E(660F38,CB,_,I,_,0,2,T1S), 0                         , 129, 0  , 309, 147), // #1545
  INST(Vrcpph           , VexRm_Lx           , E(66MAP6,4C,_,_,_,0,4,FV ), 0                         , 183, 0  , 422, 134), // #1546
  INST(Vrcpps           , VexRm_Lx           , V(000F00,53,_,x,I,_,_,_  ), 0                         , 72 , 0  , 298, 135), // #1547
  INST(Vrcpsh           , VexRvm             , E(66MAP6,4D,_,_,_,0,1,T1S), 0                         , 185, 0  , 423, 134), // #1548
  INST(Vrcpss           , VexRvm             , V(F30F00,53,_,I,I,_,_,_  ), 0                         , 199, 0  , 424, 135), // #1549
  INST(Vreducepd        , VexRmi_Lx          , E(660F3A,56,_,x,_,1,4,FV ), 0                         , 112, 0  , 400, 140), // #1550
  INST(Vreduceph        , VexRmi_Lx          , E(000F3A,56,_,_,_,0,4,FV ), 0                         , 123, 0  , 311, 132), // #1551
  INST(Vreduceps        , VexRmi_Lx          , E(660F3A,56,_,x,_,0,4,FV ), 0                         , 111, 0  , 399, 140), // #1552
  INST(Vreducesd        , VexRvmi            , E(660F3A,57,_,I,_,1,3,T1S), 0                         , 180, 0  , 425, 66 ), // #1553
  INST(Vreducesh        , VexRvmi            , E(000F3A,57,_,_,_,0,1,T1S), 0                         , 188, 0  , 313, 134), // #1554
  INST(Vreducess        , VexRvmi            , E(660F3A,57,_,I,_,0,2,T1S), 0                         , 181, 0  , 426, 66 ), // #1555
  INST(Vrndscalepd      , VexRmi_Lx          , E(660F3A,09,_,x,_,1,4,FV ), 0                         , 112, 0  , 310, 138), // #1556
  INST(Vrndscaleph      , VexRmi_Lx          , E(000F3A,08,_,_,_,0,4,FV ), 0                         , 123, 0  , 311, 132), // #1557
  INST(Vrndscaleps      , VexRmi_Lx          , E(660F3A,08,_,x,_,0,4,FV ), 0                         , 111, 0  , 312, 138), // #1558
  INST(Vrndscalesd      , VexRvmi            , E(660F3A,0B,_,I,_,1,3,T1S), 0                         , 180, 0  , 287, 68 ), // #1559
  INST(Vrndscalesh      , VexRvmi            , E(000F3A,0A,_,_,_,0,1,T1S), 0                         , 188, 0  , 313, 134), // #1560
  INST(Vrndscaless      , VexRvmi            , E(660F3A,0A,_,I,_,0,2,T1S), 0                         , 181, 0  , 288, 68 ), // #1561
  INST(Vroundpd         , VexRmi_Lx          , V(660F3A,09,_,x,I,_,_,_  ), 0                         , 73 , 0  , 427, 135), // #1562
  INST(Vroundps         , VexRmi_Lx          , V(660F3A,08,_,x,I,_,_,_  ), 0                         , 73 , 0  , 427, 135), // #1563
  INST(Vroundsd         , VexRvmi            , V(660F3A,0B,_,I,I,_,_,_  ), 0                         , 73 , 0  , 428, 135), // #1564
  INST(Vroundss         , VexRvmi            , V(660F3A,0A,_,I,I,_,_,_  ), 0                         , 73 , 0  , 429, 135), // #1565
  INST(Vrsqrt14pd       , VexRm_Lx           , E(660F38,4E,_,x,_,1,4,FV ), 0                         , 113, 0  , 350, 138), // #1566
  INST(Vrsqrt14ps       , VexRm_Lx           , E(660F38,4E,_,x,_,0,4,FV ), 0                         , 114, 0  , 374, 138), // #1567
  INST(Vrsqrt14sd       , VexRvm             , E(660F38,4F,_,I,_,1,3,T1S), 0                         , 128, 0  , 420, 68 ), // #1568
  INST(Vrsqrt14ss       , VexRvm             , E(660F38,4F,_,I,_,0,2,T1S), 0                         , 129, 0  , 421, 68 ), // #1569
  INST(Vrsqrt28pd       , VexRm              , E(660F38,CC,_,2,_,1,4,FV ), 0                         , 170, 0  , 277, 147), // #1570
  INST(Vrsqrt28ps       , VexRm              , E(660F38,CC,_,2,_,0,4,FV ), 0                         , 171, 0  , 278, 147), // #1571
  INST(Vrsqrt28sd       , VexRvm             , E(660F38,CD,_,I,_,1,3,T1S), 0                         , 128, 0  , 308, 147), // #1572
  INST(Vrsqrt28ss       , VexRvm             , E(660F38,CD,_,I,_,0,2,T1S), 0                         , 129, 0  , 309, 147), // #1573
  INST(Vrsqrtph         , VexRm_Lx           , E(66MAP6,4E,_,_,_,0,4,FV ), 0                         , 183, 0  , 422, 132), // #1574
  INST(Vrsqrtps         , VexRm_Lx           , V(000F00,52,_,x,I,_,_,_  ), 0                         , 72 , 0  , 298, 135), // #1575
  INST(Vrsqrtsh         , VexRvm             , E(66MAP6,4F,_,_,_,0,1,T1S), 0                         , 185, 0  , 423, 134), // #1576
  INST(Vrsqrtss         , VexRvm             , V(F30F00,52,_,I,I,_,_,_  ), 0                         , 199, 0  , 424, 135), // #1577
  INST(Vscalefpd        , VexRvm_Lx          , E(660F38,2C,_,x,_,1,4,FV ), 0                         , 113, 0  , 430, 138), // #1578
  INST(Vscalefph        , VexRvm_Lx          , E(66MAP6,2C,_,_,_,0,4,FV ), 0                         , 183, 0  , 197, 132), // #1579
  INST(Vscalefps        , VexRvm_Lx          , E(660F38,2C,_,x,_,0,4,FV ), 0                         , 114, 0  , 284, 138), // #1580
  INST(Vscalefsd        , VexRvm             , E(660F38,2D,_,I,_,1,3,T1S), 0                         , 128, 0  , 251, 68 ), // #1581
  INST(Vscalefsh        , VexRvm             , E(66MAP6,2D,_,_,_,0,1,T1S), 0                         , 185, 0  , 200, 134), // #1582
  INST(Vscalefss        , VexRvm             , E(660F38,2D,_,I,_,0,2,T1S), 0                         , 129, 0  , 259, 68 ), // #1583
  INST(Vscatterdpd      , VexMr_VM           , E(660F38,A2,_,x,_,1,3,T1S), 0                         , 128, 0  , 403, 138), // #1584
  INST(Vscatterdps      , VexMr_VM           , E(660F38,A2,_,x,_,0,2,T1S), 0                         , 129, 0  , 402, 138), // #1585
  INST(Vscatterpf0dpd   , VexM_VM            , E(660F38,C6,5,2,_,1,3,T1S), 0                         , 230, 0  , 303, 153), // #1586
  INST(Vscatterpf0dps   , VexM_VM            , E(660F38,C6,5,2,_,0,2,T1S), 0                         , 231, 0  , 304, 153), // #1587
  INST(Vscatterpf0qpd   , VexM_VM            , E(660F38,C7,5,2,_,1,3,T1S), 0                         , 230, 0  , 305, 153), // #1588
  INST(Vscatterpf0qps   , VexM_VM            , E(660F38,C7,5,2,_,0,2,T1S), 0                         , 231, 0  , 305, 153), // #1589
  INST(Vscatterpf1dpd   , VexM_VM            , E(660F38,C6,6,2,_,1,3,T1S), 0                         , 232, 0  , 303, 153), // #1590
  INST(Vscatterpf1dps   , VexM_VM            , E(660F38,C6,6,2,_,0,2,T1S), 0                         , 233, 0  , 304, 153), // #1591
  INST(Vscatterpf1qpd   , VexM_VM            , E(660F38,C7,6,2,_,1,3,T1S), 0                         , 232, 0  , 305, 153), // #1592
  INST(Vscatterpf1qps   , VexM_VM            , E(660F38,C7,6,2,_,0,2,T1S), 0                         , 233, 0  , 305, 153), // #1593
  INST(Vscatterqpd      , VexMr_VM           , E(660F38,A3,_,x,_,1,3,T1S), 0                         , 128, 0  , 405, 138), // #1594
  INST(Vscatterqps      , VexMr_VM           , E(660F38,A3,_,x,_,0,2,T1S), 0                         , 129, 0  , 404, 138), // #1595
  INST(Vshuff32x4       , VexRvmi_Lx         , E(660F3A,23,_,x,_,0,4,FV ), 0                         , 111, 0  , 431, 138), // #1596
  INST(Vshuff64x2       , VexRvmi_Lx         , E(660F3A,23,_,x,_,1,4,FV ), 0                         , 112, 0  , 432, 138), // #1597
  INST(Vshufi32x4       , VexRvmi_Lx         , E(660F3A,43,_,x,_,0,4,FV ), 0                         , 111, 0  , 431, 138), // #1598
  INST(Vshufi64x2       , VexRvmi_Lx         , E(660F3A,43,_,x,_,1,4,FV ), 0                         , 112, 0  , 432, 138), // #1599
  INST(Vshufpd          , VexRvmi_Lx         , V(660F00,C6,_,x,I,1,4,FV ), 0                         , 103, 0  , 433, 131), // #1600
  INST(Vshufps          , VexRvmi_Lx         , V(000F00,C6,_,x,I,0,4,FV ), 0                         , 105, 0  , 434, 131), // #1601
  INST(Vsqrtpd          , VexRm_Lx           , V(660F00,51,_,x,I,1,4,FV ), 0                         , 103, 0  , 435, 131), // #1602
  INST(Vsqrtph          , VexRm_Lx           , E(00MAP5,51,_,_,_,0,4,FV ), 0                         , 104, 0  , 246, 132), // #1603
  INST(Vsqrtps          , VexRm_Lx           , V(000F00,51,_,x,I,0,4,FV ), 0                         , 105, 0  , 235, 131), // #1604
  INST(Vsqrtsd          , VexRvm             , V(F20F00,51,_,I,I,1,3,T1S), 0                         , 106, 0  , 199, 133), // #1605
  INST(Vsqrtsh          , VexRvm             , E(F3MAP5,51,_,_,_,0,1,T1S), 0                         , 107, 0  , 200, 134), // #1606
  INST(Vsqrtss          , VexRvm             , V(F30F00,51,_,I,I,0,2,T1S), 0                         , 108, 0  , 201, 133), // #1607
  INST(Vstmxcsr         , VexM               , V(000F00,AE,3,0,I,_,_,_  ), 0                         , 234, 0  , 321, 135), // #1608
  INST(Vsubpd           , VexRvm_Lx          , V(660F00,5C,_,x,I,1,4,FV ), 0                         , 103, 0  , 196, 131), // #1609
  INST(Vsubph           , VexRvm_Lx          , E(00MAP5,5C,_,_,_,0,4,FV ), 0                         , 104, 0  , 197, 132), // #1610
  INST(Vsubps           , VexRvm_Lx          , V(000F00,5C,_,x,I,0,4,FV ), 0                         , 105, 0  , 198, 131), // #1611
  INST(Vsubsd           , VexRvm             , V(F20F00,5C,_,I,I,1,3,T1S), 0                         , 106, 0  , 199, 133), // #1612
  INST(Vsubsh           , VexRvm             , E(F3MAP5,5C,_,_,_,0,1,T1S), 0                         , 107, 0  , 200, 134), // #1613
  INST(Vsubss           , VexRvm             , V(F30F00,5C,_,I,I,0,2,T1S), 0                         , 108, 0  , 201, 133), // #1614
  INST(Vtestpd          , VexRm_Lx           , V(660F38,0F,_,x,0,_,_,_  ), 0                         , 96 , 0  , 298, 167), // #1615
  INST(Vtestps          , VexRm_Lx           , V(660F38,0E,_,x,0,_,_,_  ), 0                         , 96 , 0  , 298, 167), // #1616
  INST(Vucomisd         , VexRm              , V(660F00,2E,_,I,I,1,3,T1S), 0                         , 125, 0  , 229, 143), // #1617
  INST(Vucomish         , VexRm              , E(00MAP5,2E,_,_,_,0,1,T1S), 0                         , 126, 0  , 230, 134), // #1618
  INST(Vucomiss         , VexRm              , V(000F00,2E,_,I,I,0,2,T1S), 0                         , 127, 0  , 231, 143), // #1619
  INST(Vunpckhpd        , VexRvm_Lx          , V(660F00,15,_,x,I,1,4,FV ), 0                         , 103, 0  , 208, 131), // #1620
  INST(Vunpckhps        , VexRvm_Lx          , V(000F00,15,_,x,I,0,4,FV ), 0                         , 105, 0  , 209, 131), // #1621
  INST(Vunpcklpd        , VexRvm_Lx          , V(660F00,14,_,x,I,1,4,FV ), 0                         , 103, 0  , 208, 131), // #1622
  INST(Vunpcklps        , VexRvm_Lx          , V(000F00,14,_,x,I,0,4,FV ), 0                         , 105, 0  , 209, 131), // #1623
  INST(Vxorpd           , VexRvm_Lx          , V(660F00,57,_,x,I,1,4,FV ), 0                         , 103, 0  , 417, 139), // #1624
  INST(Vxorps           , VexRvm_Lx          , V(000F00,57,_,x,I,0,4,FV ), 0                         , 105, 0  , 416, 139), // #1625
  INST(Vzeroall         , VexOp              , V(000F00,77,_,1,I,_,_,_  ), 0                         , 68 , 0  , 436, 135), // #1626
  INST(Vzeroupper       , VexOp              , V(000F00,77,_,0,I,_,_,_  ), 0                         , 72 , 0  , 436, 135), // #1627
  INST(Wbinvd           , X86Op              , O(000F00,09,_,_,_,_,_,_  ), 0                         , 4  , 0  , 30 , 0  ), // #1628
  INST(Wbnoinvd         , X86Op              , O(F30F00,09,_,_,_,_,_,_  ), 0                         , 6  , 0  , 30 , 176), // #1629
  INST(Wrfsbase         , X86M               , O(F30F00,AE,2,_,x,_,_,_  ), 0                         , 235, 0  , 173, 111), // #1630
  INST(Wrgsbase         , X86M               , O(F30F00,AE,3,_,x,_,_,_  ), 0                         , 236, 0  , 173, 111), // #1631
  INST(Wrmsr            , X86Op              , O(000F00,30,_,_,_,_,_,_  ), 0                         , 4  , 0  , 174, 112), // #1632
  INST(Wrssd            , X86Mr              , O(000F38,F6,_,_,_,_,_,_  ), 0                         , 83 , 0  , 437, 56 ), // #1633
  INST(Wrssq            , X86Mr              , O(000F38,F6,_,_,1,_,_,_  ), 0                         , 237, 0  , 438, 56 ), // #1634
  INST(Wrussd           , X86Mr              , O(660F38,F5,_,_,_,_,_,_  ), 0                         , 2  , 0  , 437, 56 ), // #1635
  INST(Wrussq           , X86Mr              , O(660F38,F5,_,_,1,_,_,_  ), 0                         , 238, 0  , 438, 56 ), // #1636
  INST(Xabort           , X86Op_Mod11RM_I8   , O(000000,C6,7,_,_,_,_,_  ), 0                         , 27 , 0  , 80 , 177), // #1637
  INST(Xadd             , X86Xadd            , O(000F00,C0,_,_,x,_,_,_  ), 0                         , 4  , 0  , 439, 38 ), // #1638
  INST(Xbegin           , X86JmpRel          , O(000000,C7,7,_,_,_,_,_  ), 0                         , 27 , 0  , 440, 177), // #1639
  INST(Xchg             , X86Xchg            , O(000000,86,_,_,x,_,_,_  ), 0                         , 0  , 0  , 441, 0  ), // #1640
  INST(Xend             , X86Op              , O(000F01,D5,_,_,_,_,_,_  ), 0                         , 21 , 0  , 30 , 177), // #1641
  INST(Xgetbv           , X86Op              , O(000F01,D0,_,_,_,_,_,_  ), 0                         , 21 , 0  , 174, 178), // #1642
  INST(Xlatb            , X86Op              , O(000000,D7,_,_,_,_,_,_  ), 0                         , 0  , 0  , 30 , 0  ), // #1643
  INST(Xor              , X86Arith           , O(000000,30,6,_,x,_,_,_  ), 0                         , 32 , 0  , 179, 1  ), // #1644
  INST(Xorpd            , ExtRm              , O(660F00,57,_,_,_,_,_,_  ), 0                         , 3  , 0  , 151, 4  ), // #1645
  INST(Xorps            , ExtRm              , O(000F00,57,_,_,_,_,_,_  ), 0                         , 4  , 0  , 151, 5  ), // #1646
  INST(Xresldtrk        , X86Op              , O(F20F01,E9,_,_,_,_,_,_  ), 0                         , 92 , 0  , 30 , 179), // #1647
  INST(Xrstor           , X86M_Only_EDX_EAX  , O(000F00,AE,5,_,_,_,_,_  ), 0                         , 77 , 0  , 442, 178), // #1648
  INST(Xrstor64         , X86M_Only_EDX_EAX  , O(000F00,AE,5,_,1,_,_,_  ), 0                         , 239, 0  , 443, 178), // #1649
  INST(Xrstors          , X86M_Only_EDX_EAX  , O(000F00,C7,3,_,_,_,_,_  ), 0                         , 78 , 0  , 442, 180), // #1650
  INST(Xrstors64        , X86M_Only_EDX_EAX  , O(000F00,C7,3,_,1,_,_,_  ), 0                         , 240, 0  , 443, 180), // #1651
  INST(Xsave            , X86M_Only_EDX_EAX  , O(000F00,AE,4,_,_,_,_,_  ), 0                         , 97 , 0  , 442, 178), // #1652
  INST(Xsave64          , X86M_Only_EDX_EAX  , O(000F00,AE,4,_,1,_,_,_  ), 0                         , 241, 0  , 443, 178), // #1653
  INST(Xsavec           , X86M_Only_EDX_EAX  , O(000F00,C7,4,_,_,_,_,_  ), 0                         , 97 , 0  , 442, 181), // #1654
  INST(Xsavec64         , X86M_Only_EDX_EAX  , O(000F00,C7,4,_,1,_,_,_  ), 0                         , 241, 0  , 443, 181), // #1655
  INST(Xsaveopt         , X86M_Only_EDX_EAX  , O(000F00,AE,6,_,_,_,_,_  ), 0                         , 80 , 0  , 442, 182), // #1656
  INST(Xsaveopt64       , X86M_Only_EDX_EAX  , O(000F00,AE,6,_,1,_,_,_  ), 0                         , 242, 0  , 443, 182), // #1657
  INST(Xsaves           , X86M_Only_EDX_EAX  , O(000F00,C7,5,_,_,_,_,_  ), 0                         , 77 , 0  , 442, 180), // #1658
  INST(Xsaves64         , X86M_Only_EDX_EAX  , O(000F00,C7,5,_,1,_,_,_  ), 0                         , 239, 0  , 443, 180), // #1659
  INST(Xsetbv           , X86Op              , O(000F01,D1,_,_,_,_,_,_  ), 0                         , 21 , 0  , 174, 178), // #1660
  INST(Xsusldtrk        , X86Op              , O(F20F01,E8,_,_,_,_,_,_  ), 0                         , 92 , 0  , 30 , 179), // #1661
  INST(Xtest            , X86Op              , O(000F01,D6,_,_,_,_,_,_  ), 0                         , 21 , 0  , 30 , 183)  // #1662
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
  { F(Evex)|F(EvexKReg)|F(Vec)|F(Vex)                 , X(B64)|X(K)|X(SAE)|X(Z)       , 197, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #223 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(B16)|X(K)|X(SAE)            , 200, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #224 [ref=1x]
  { F(Evex)|F(EvexKReg)|F(Vec)|F(Vex)                 , X(B32)|X(K)|X(SAE)|X(Z)       , 197, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #225 [ref=1x]
  { F(Evex)|F(EvexKReg)|F(Vec)|F(Vex)                 , X(K)|X(SAE)|X(Z)              , 484, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #226 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(K)|X(SAE)                   , 485, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #227 [ref=1x]
  { F(Evex)|F(EvexKReg)|F(Vec)|F(Vex)                 , X(K)|X(SAE)|X(Z)              , 486, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #228 [ref=1x]
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
  { F(Evex)|F(Vec)                                    , X(B64)|X(K)                   , 493, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #292 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(B16)|X(K)                   , 493, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #293 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(B32)|X(K)                   , 493, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #294 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(K)                          , 494, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #295 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(K)                          , 495, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #296 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(K)                          , 496, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #297 [ref=1x]
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
  { F(Evex)|F(Vec)                                    , X(K)                          , 200, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(WO)}, // #364 [ref=2x]
  { F(Evex)|F(Vec)                                    , X(B32)|X(K)                   , 200, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(WO)}, // #365 [ref=2x]
  { F(Evex)|F(EvexKReg)|F(Vec)|F(Vex)                 , X(K)                          , 251, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(WO)}, // #366 [ref=4x]
  { F(Evex)|F(EvexKReg)|F(Vec)|F(Vex)                 , X(B32)|X(K)                   , 251, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(WO)}, // #367 [ref=2x]
  { F(Evex)|F(EvexKReg)|F(Vec)|F(Vex)                 , X(B64)|X(K)                   , 251, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(WO)}, // #368 [ref=2x]
  { F(Vec)|F(Vex)                                     , 0                             , 449, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #369 [ref=1x]
  { F(Vec)|F(Vex)                                     , 0                             , 450, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #370 [ref=1x]
  { F(Vec)|F(Vex)                                     , 0                             , 451, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #371 [ref=1x]
  { F(Vec)|F(Vex)                                     , 0                             , 452, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #372 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(B64)|X(K)                   , 200, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(WO)}, // #373 [ref=4x]
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
  { F(Evex)|F(Vec)                                    , X(K)                          , 272, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #407 [ref=5x]
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
  { F(Evex)|F(Vec)                                    , X(B32)|X(K)                   , 272, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #418 [ref=2x]
  { F(Evex)|F(Vec)                                    , X(B64)|X(K)                   , 272, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #419 [ref=2x]
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
  { 0, 0, { 0 } }, // #0 [ref=148x]
  { 0, 1, { 0 } }, // #1 [ref=32x]
  { 0, 2, { 0 } }, // #2 [ref=2x]
  { 0, 3, { EXT(ADX) } }, // #3 [ref=1x]
  { 0, 0, { EXT(SSE2) } }, // #4 [ref=60x]
  { 0, 0, { EXT(SSE) } }, // #5 [ref=41x]
  { 0, 0, { EXT(SSE3) } }, // #6 [ref=12x]
  { 0, 4, { EXT(ADX) } }, // #7 [ref=1x]
  { 0, 0, { EXT(AESNI) } }, // #8 [ref=6x]
  { 0, 1, { EXT(BMI) } }, // #9 [ref=6x]
  { 0, 5, { 0 } }, // #10 [ref=5x]
  { 0, 0, { EXT(TBM) } }, // #11 [ref=9x]
  { 0, 0, { EXT(SSE4_1) } }, // #12 [ref=47x]
  { 0, 0, { EXT(MPX) } }, // #13 [ref=7x]
  { 0, 6, { 0 } }, // #14 [ref=4x]
  { 0, 1, { EXT(BMI2) } }, // #15 [ref=1x]
  { 0, 7, { EXT(SMAP) } }, // #16 [ref=2x]
  { 0, 8, { 0 } }, // #17 [ref=2x]
  { 0, 9, { 0 } }, // #18 [ref=2x]
  { 0, 0, { EXT(CLDEMOTE) } }, // #19 [ref=1x]
  { 0, 0, { EXT(CLFLUSH) } }, // #20 [ref=1x]
  { 0, 0, { EXT(CLFLUSHOPT) } }, // #21 [ref=1x]
  { 0, 0, { EXT(SVM) } }, // #22 [ref=6x]
  { 0, 10, { 0 } }, // #23 [ref=2x]
  { 0, 1, { EXT(CET_SS) } }, // #24 [ref=3x]
  { 0, 0, { EXT(UINTR) } }, // #25 [ref=4x]
  { 0, 0, { EXT(CLWB) } }, // #26 [ref=1x]
  { 0, 0, { EXT(CLZERO) } }, // #27 [ref=1x]
  { 0, 3, { 0 } }, // #28 [ref=1x]
  { 0, 11, { EXT(CMOV) } }, // #29 [ref=6x]
  { 0, 12, { EXT(CMOV) } }, // #30 [ref=8x]
  { 0, 13, { EXT(CMOV) } }, // #31 [ref=6x]
  { 0, 14, { EXT(CMOV) } }, // #32 [ref=4x]
  { 0, 15, { EXT(CMOV) } }, // #33 [ref=4x]
  { 0, 16, { EXT(CMOV) } }, // #34 [ref=2x]
  { 0, 17, { EXT(CMOV) } }, // #35 [ref=6x]
  { 0, 18, { EXT(CMOV) } }, // #36 [ref=2x]
  { 0, 19, { 0 } }, // #37 [ref=2x]
  { 0, 1, { EXT(I486) } }, // #38 [ref=2x]
  { 0, 5, { EXT(CMPXCHG16B) } }, // #39 [ref=1x]
  { 0, 5, { EXT(CMPXCHG8B) } }, // #40 [ref=1x]
  { 0, 1, { EXT(SSE2) } }, // #41 [ref=2x]
  { 0, 1, { EXT(SSE) } }, // #42 [ref=2x]
  { 0, 0, { EXT(I486) } }, // #43 [ref=4x]
  { 0, 0, { EXT(SSE4_2) } }, // #44 [ref=2x]
  { 0, 20, { 0 } }, // #45 [ref=2x]
  { 0, 0, { EXT(MMX) } }, // #46 [ref=1x]
  { 0, 0, { EXT(CET_IBT) } }, // #47 [ref=2x]
  { 0, 0, { EXT(ENQCMD) } }, // #48 [ref=2x]
  { 0, 0, { EXT(SSE4A) } }, // #49 [ref=4x]
  { 0, 21, { 0 } }, // #50 [ref=4x]
  { 0, 0, { EXT(3DNOW) } }, // #51 [ref=21x]
  { 0, 0, { EXT(FXSR) } }, // #52 [ref=4x]
  { 0, 0, { EXT(SMX) } }, // #53 [ref=1x]
  { 0, 0, { EXT(GFNI) } }, // #54 [ref=3x]
  { 0, 0, { EXT(HRESET) } }, // #55 [ref=1x]
  { 0, 0, { EXT(CET_SS) } }, // #56 [ref=9x]
  { 0, 16, { 0 } }, // #57 [ref=5x]
  { 0, 0, { EXT(VMX) } }, // #58 [ref=12x]
  { 0, 11, { 0 } }, // #59 [ref=8x]
  { 0, 12, { 0 } }, // #60 [ref=12x]
  { 0, 13, { 0 } }, // #61 [ref=10x]
  { 0, 14, { 0 } }, // #62 [ref=8x]
  { 0, 15, { 0 } }, // #63 [ref=8x]
  { 0, 17, { 0 } }, // #64 [ref=8x]
  { 0, 18, { 0 } }, // #65 [ref=4x]
  { 0, 0, { EXT(AVX512_DQ) } }, // #66 [ref=22x]
  { 0, 0, { EXT(AVX512_BW) } }, // #67 [ref=20x]
  { 0, 0, { EXT(AVX512_F) } }, // #68 [ref=36x]
  { 1, 0, { EXT(AVX512_DQ) } }, // #69 [ref=1x]
  { 1, 0, { EXT(AVX512_BW) } }, // #70 [ref=2x]
  { 1, 0, { EXT(AVX512_F) } }, // #71 [ref=1x]
  { 0, 1, { EXT(AVX512_DQ) } }, // #72 [ref=3x]
  { 0, 1, { EXT(AVX512_BW) } }, // #73 [ref=4x]
  { 0, 1, { EXT(AVX512_F) } }, // #74 [ref=1x]
  { 0, 22, { EXT(LAHFSAHF) } }, // #75 [ref=1x]
  { 0, 0, { EXT(AMX_TILE) } }, // #76 [ref=7x]
  { 0, 0, { EXT(LWP) } }, // #77 [ref=4x]
  { 0, 23, { 0 } }, // #78 [ref=3x]
  { 0, 1, { EXT(LZCNT) } }, // #79 [ref=1x]
  { 0, 0, { EXT(MMX2) } }, // #80 [ref=8x]
  { 0, 1, { EXT(MCOMMIT) } }, // #81 [ref=1x]
  { 0, 0, { EXT(MONITOR) } }, // #82 [ref=2x]
  { 0, 0, { EXT(MONITORX) } }, // #83 [ref=2x]
  { 1, 0, { 0 } }, // #84 [ref=1x]
  { 1, 0, { EXT(SSE2) } }, // #85 [ref=5x]
  { 1, 0, { EXT(SSE) } }, // #86 [ref=3x]
  { 0, 0, { EXT(MOVBE) } }, // #87 [ref=1x]
  { 0, 0, { EXT(MMX), EXT(SSE2) } }, // #88 [ref=45x]
  { 0, 0, { EXT(MOVDIR64B) } }, // #89 [ref=1x]
  { 0, 0, { EXT(MOVDIRI) } }, // #90 [ref=1x]
  { 1, 0, { EXT(MMX), EXT(SSE2) } }, // #91 [ref=1x]
  { 0, 0, { EXT(BMI2) } }, // #92 [ref=7x]
  { 0, 0, { EXT(SSSE3) } }, // #93 [ref=15x]
  { 0, 0, { EXT(MMX2), EXT(SSE2) } }, // #94 [ref=10x]
  { 0, 0, { EXT(PCLMULQDQ) } }, // #95 [ref=1x]
  { 0, 1, { EXT(SSE4_2) } }, // #96 [ref=4x]
  { 0, 0, { EXT(PCONFIG) } }, // #97 [ref=1x]
  { 0, 0, { EXT(MMX2), EXT(SSE2), EXT(SSE4_1) } }, // #98 [ref=1x]
  { 0, 0, { EXT(3DNOW2) } }, // #99 [ref=5x]
  { 0, 0, { EXT(GEODE) } }, // #100 [ref=2x]
  { 0, 1, { EXT(POPCNT) } }, // #101 [ref=1x]
  { 0, 24, { 0 } }, // #102 [ref=3x]
  { 0, 1, { EXT(PREFETCHW) } }, // #103 [ref=1x]
  { 0, 1, { EXT(PREFETCHWT1) } }, // #104 [ref=1x]
  { 0, 20, { EXT(SNP) } }, // #105 [ref=3x]
  { 0, 1, { EXT(SSE4_1) } }, // #106 [ref=1x]
  { 0, 0, { EXT(PTWRITE) } }, // #107 [ref=1x]
  { 0, 25, { 0 } }, // #108 [ref=3x]
  { 0, 1, { EXT(SNP) } }, // #109 [ref=1x]
  { 0, 26, { 0 } }, // #110 [ref=2x]
  { 0, 0, { EXT(FSGSBASE) } }, // #111 [ref=4x]
  { 0, 0, { EXT(MSR) } }, // #112 [ref=2x]
  { 0, 0, { EXT(RDPID) } }, // #113 [ref=1x]
  { 0, 0, { EXT(OSPKE) } }, // #114 [ref=1x]
  { 0, 0, { EXT(RDPRU) } }, // #115 [ref=1x]
  { 0, 1, { EXT(RDRAND) } }, // #116 [ref=1x]
  { 0, 1, { EXT(RDSEED) } }, // #117 [ref=1x]
  { 0, 0, { EXT(RDTSC) } }, // #118 [ref=1x]
  { 0, 0, { EXT(RDTSCP) } }, // #119 [ref=1x]
  { 0, 27, { 0 } }, // #120 [ref=2x]
  { 0, 28, { EXT(LAHFSAHF) } }, // #121 [ref=1x]
  { 0, 0, { EXT(SERIALIZE) } }, // #122 [ref=1x]
  { 0, 0, { EXT(SHA) } }, // #123 [ref=7x]
  { 0, 0, { EXT(SKINIT) } }, // #124 [ref=2x]
  { 0, 0, { EXT(AMX_BF16) } }, // #125 [ref=1x]
  { 0, 0, { EXT(AMX_INT8) } }, // #126 [ref=4x]
  { 0, 1, { EXT(UINTR) } }, // #127 [ref=1x]
  { 0, 1, { EXT(WAITPKG) } }, // #128 [ref=2x]
  { 0, 0, { EXT(WAITPKG) } }, // #129 [ref=1x]
  { 0, 0, { EXT(AVX512_4FMAPS) } }, // #130 [ref=4x]
  { 0, 0, { EXT(AVX), EXT(AVX512_F), EXT(AVX512_VL) } }, // #131 [ref=42x]
  { 0, 0, { EXT(AVX512_FP16), EXT(AVX512_VL) } }, // #132 [ref=63x]
  { 0, 0, { EXT(AVX), EXT(AVX512_F) } }, // #133 [ref=29x]
  { 0, 0, { EXT(AVX512_FP16) } }, // #134 [ref=43x]
  { 0, 0, { EXT(AVX) } }, // #135 [ref=35x]
  { 0, 0, { EXT(AESNI), EXT(AVX), EXT(AVX512_F), EXT(AVX512_VL), EXT(VAES) } }, // #136 [ref=4x]
  { 0, 0, { EXT(AESNI), EXT(AVX) } }, // #137 [ref=2x]
  { 0, 0, { EXT(AVX512_F), EXT(AVX512_VL) } }, // #138 [ref=108x]
  { 0, 0, { EXT(AVX), EXT(AVX512_DQ), EXT(AVX512_VL) } }, // #139 [ref=8x]
  { 0, 0, { EXT(AVX512_DQ), EXT(AVX512_VL) } }, // #140 [ref=30x]
  { 0, 0, { EXT(AVX2) } }, // #141 [ref=7x]
  { 0, 0, { EXT(AVX), EXT(AVX2), EXT(AVX512_F), EXT(AVX512_VL) } }, // #142 [ref=39x]
  { 0, 1, { EXT(AVX), EXT(AVX512_F) } }, // #143 [ref=4x]
  { 0, 0, { EXT(AVX512_BF16), EXT(AVX512_VL) } }, // #144 [ref=3x]
  { 0, 0, { EXT(AVX512_F), EXT(AVX512_VL), EXT(F16C) } }, // #145 [ref=2x]
  { 0, 0, { EXT(AVX512_BW), EXT(AVX512_VL) } }, // #146 [ref=24x]
  { 0, 0, { EXT(AVX512_ERI) } }, // #147 [ref=10x]
  { 0, 0, { EXT(AVX512_F), EXT(AVX512_VL), EXT(FMA) } }, // #148 [ref=36x]
  { 0, 0, { EXT(AVX512_F), EXT(FMA) } }, // #149 [ref=24x]
  { 0, 0, { EXT(FMA4) } }, // #150 [ref=20x]
  { 0, 0, { EXT(XOP) } }, // #151 [ref=55x]
  { 0, 0, { EXT(AVX2), EXT(AVX512_F), EXT(AVX512_VL) } }, // #152 [ref=19x]
  { 0, 0, { EXT(AVX512_PFI) } }, // #153 [ref=16x]
  { 0, 0, { EXT(AVX), EXT(AVX512_F), EXT(AVX512_VL), EXT(GFNI) } }, // #154 [ref=3x]
  { 1, 0, { EXT(AVX), EXT(AVX512_F), EXT(AVX512_VL) } }, // #155 [ref=4x]
  { 1, 0, { EXT(AVX) } }, // #156 [ref=2x]
  { 1, 0, { EXT(AVX512_F), EXT(AVX512_VL) } }, // #157 [ref=4x]
  { 1, 0, { EXT(AVX512_BW), EXT(AVX512_VL) } }, // #158 [ref=2x]
  { 1, 0, { EXT(AVX), EXT(AVX512_F) } }, // #159 [ref=3x]
  { 0, 0, { EXT(AVX), EXT(AVX2) } }, // #160 [ref=17x]
  { 0, 0, { EXT(AVX512_VP2INTERSECT) } }, // #161 [ref=2x]
  { 0, 0, { EXT(AVX512_4VNNIW) } }, // #162 [ref=2x]
  { 0, 0, { EXT(AVX), EXT(AVX2), EXT(AVX512_BW), EXT(AVX512_VL) } }, // #163 [ref=54x]
  { 0, 0, { EXT(AVX2), EXT(AVX512_BW), EXT(AVX512_VL) } }, // #164 [ref=2x]
  { 0, 0, { EXT(AVX512_CDI), EXT(AVX512_VL) } }, // #165 [ref=6x]
  { 0, 0, { EXT(AVX), EXT(AVX512_F), EXT(AVX512_VL), EXT(PCLMULQDQ), EXT(VPCLMULQDQ) } }, // #166 [ref=1x]
  { 0, 1, { EXT(AVX) } }, // #167 [ref=7x]
  { 0, 0, { EXT(AVX512_VBMI2), EXT(AVX512_VL) } }, // #168 [ref=16x]
  { 0, 0, { EXT(AVX512_VL), EXT(AVX512_VNNI), EXT(AVX_VNNI) } }, // #169 [ref=4x]
  { 0, 0, { EXT(AVX512_VBMI), EXT(AVX512_VL) } }, // #170 [ref=4x]
  { 0, 0, { EXT(AVX), EXT(AVX512_BW) } }, // #171 [ref=4x]
  { 0, 0, { EXT(AVX), EXT(AVX512_DQ) } }, // #172 [ref=4x]
  { 0, 0, { EXT(AVX512_IFMA), EXT(AVX512_VL) } }, // #173 [ref=2x]
  { 0, 0, { EXT(AVX512_BITALG), EXT(AVX512_VL) } }, // #174 [ref=3x]
  { 0, 0, { EXT(AVX512_VL), EXT(AVX512_VPOPCNTDQ) } }, // #175 [ref=2x]
  { 0, 0, { EXT(WBNOINVD) } }, // #176 [ref=1x]
  { 0, 0, { EXT(RTM) } }, // #177 [ref=3x]
  { 0, 0, { EXT(XSAVE) } }, // #178 [ref=6x]
  { 0, 0, { EXT(TSXLDTRK) } }, // #179 [ref=2x]
  { 0, 0, { EXT(XSAVES) } }, // #180 [ref=4x]
  { 0, 0, { EXT(XSAVEC) } }, // #181 [ref=2x]
  { 0, 0, { EXT(XSAVEOPT) } }, // #182 [ref=2x]
  { 0, 1, { EXT(TSX) } }  // #183 [ref=1x]
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

#define FLAG(VAL) uint32_t(InstRWFlags::k##VAL)
const InstRWFlags InstDB::_instFlagsTable[] = {
  InstRWFlags(FLAG(None)), // #0 [ref=1634x]
  InstRWFlags(FLAG(MovOp))  // #1 [ref=29x]
};
#undef FLAG
// ----------------------------------------------------------------------------
// ${AdditionalInfoTable:End}

// Inst - NameData
// ===============

#ifndef ASMJIT_NO_TEXT
// ${NameData:Begin}
// ------------------- Automatically generated, do not edit -------------------
const uint32_t InstDB::_instNameIndexTable[] = {
  0x80000000, // Small ''.
  0x80000421, // Small 'aaa'.
  0x80001021, // Small 'aad'.
  0x80003421, // Small 'aam'.
  0x80004C21, // Small 'aas'.
  0x80000C81, // Small 'adc'.
  0x800C0C81, // Small 'adcx'.
  0x80001081, // Small 'add'.
  0x80481081, // Small 'addpd'.
  0x81381081, // Small 'addps'.
  0x80499081, // Small 'addsd'.
  0x81399081, // Small 'addss'.
  0x20876079, // Large 'addsub|pd'.
  0x20706079, // Large 'addsub|ps'.
  0x800C3C81, // Small 'adox'.
  0x86524CA1, // Small 'aesdec'.
  0x3028718D, // Large 'aesdecl|ast'.
  0x86E2CCA1, // Small 'aesenc'.
  0x30287195, // Large 'aesencl|ast'.
  0x86D4CCA1, // Small 'aesimc'.
  0x0000F012, // Large 'aeskeygenassist'.
  0x800011C1, // Small 'and'.
  0x800711C1, // Small 'andn'.
  0x890711C1, // Small 'andnpd'.
  0xA70711C1, // Small 'andnps'.
  0x804811C1, // Small 'andpd'.
  0x813811C1, // Small 'andps'.
  0x80064241, // Small 'arpl'.
  0x812A60A2, // Small 'bextr'.
  0x261B5630, // Large 'blcfi|ll'.
  0x80048D82, // Small 'blci'.
  0x80348D82, // Small 'blcic'.
  0x97368D82, // Small 'blcmsk'.
  0x80098D82, // Small 'blcs'.
  0x208753E4, // Large 'blend|pd'.
  0x207053E4, // Large 'blend|ps'.
  0x33EA53E4, // Large 'blend|vpd'.
  0x315053E4, // Large 'blend|vps'.
  0x261B5635, // Large 'blsfi|ll'.
  0x8004CD82, // Small 'blsi'.
  0x8034CD82, // Small 'blsic'.
  0x9736CD82, // Small 'blsmsk'.
  0x80094D82, // Small 'blsr'.
  0x80C191C2, // Small 'bndcl'.
  0x80E191C2, // Small 'bndcn'.
  0x815191C2, // Small 'bndcu'.
  0xB04611C2, // Small 'bndldx'.
  0x80B691C2, // Small 'bndmk'.
  0xACF691C2, // Small 'bndmov'.
  0xB14991C2, // Small 'bndstx'.
  0x804755E2, // Small 'bound'.
  0x80001A62, // Small 'bsf'.
  0x80004A62, // Small 'bsr'.
  0x8100DE62, // Small 'bswap'.
  0x80000282, // Small 'bt'.
  0x80000E82, // Small 'btc'.
  0x80004A82, // Small 'btr'.
  0x80004E82, // Small 'bts'.
  0x8004A342, // Small 'bzhi'.
  0x80063023, // Small 'call'.
  0x80005C43, // Small 'cbw'.
  0x80004483, // Small 'cdq'.
  0x8002C483, // Small 'cdqe'.
  0x80018583, // Small 'clac'.
  0x80000D83, // Small 'clc'.
  0x80001183, // Small 'cld'.
  0x20BF6503, // Large 'cldemo|te'.
  0x0000724D, // Large 'clflush'.
  0x1020924D, // Large 'clflushop|t'.
  0x80049D83, // Small 'clgi'.
  0x80002583, // Small 'cli'.
  0x10177509, // Large 'clrssbs|y'.
  0x8009D183, // Small 'clts'.
  0x8004D583, // Small 'clui'.
  0x80015D83, // Small 'clwb'.
  0x9F22E983, // Small 'clzero'.
  0x80000DA3, // Small 'cmc'.
  0x801B3DA3, // Small 'cmova'.
  0x8A1B3DA3, // Small 'cmovae'.
  0x802B3DA3, // Small 'cmovb'.
  0x8A2B3DA3, // Small 'cmovbe'.
  0x803B3DA3, // Small 'cmovc'.
  0x805B3DA3, // Small 'cmove'.
  0x807B3DA3, // Small 'cmovg'.
  0x8A7B3DA3, // Small 'cmovge'.
  0x80CB3DA3, // Small 'cmovl'.
  0x8ACB3DA3, // Small 'cmovle'.
  0x82EB3DA3, // Small 'cmovna'.
  0x20125516, // Large 'cmovn|ae'.
  0x84EB3DA3, // Small 'cmovnb'.
  0x100B6516, // Large 'cmovnb|e'.
  0x86EB3DA3, // Small 'cmovnc'.
  0x8AEB3DA3, // Small 'cmovne'.
  0x8EEB3DA3, // Small 'cmovng'.
  0x20185516, // Large 'cmovn|ge'.
  0x98EB3DA3, // Small 'cmovnl'.
  0x217C5516, // Large 'cmovn|le'.
  0x9EEB3DA3, // Small 'cmovno'.
  0xA0EB3DA3, // Small 'cmovnp'.
  0xA6EB3DA3, // Small 'cmovns'.
  0xB4EB3DA3, // Small 'cmovnz'.
  0x80FB3DA3, // Small 'cmovo'.
  0x810B3DA3, // Small 'cmovp'.
  0x8B0B3DA3, // Small 'cmovpe'.
  0x9F0B3DA3, // Small 'cmovpo'.
  0x813B3DA3, // Small 'cmovs'.
  0x81AB3DA3, // Small 'cmovz'.
  0x800041A3, // Small 'cmp'.
  0x804841A3, // Small 'cmppd'.
  0x813841A3, // Small 'cmpps'.
  0x8009C1A3, // Small 'cmps'.
  0x8049C1A3, // Small 'cmpsd'.
  0x8139C1A3, // Small 'cmpss'.
  0x00007256, // Large 'cmpxchg'.
  0x10109256, // Large 'cmpxchg16|b'.
  0x23837256, // Large 'cmpxchg|8b'.
  0x8934B5E3, // Small 'comisd'.
  0xA734B5E3, // Small 'comiss'.
  0x8044D603, // Small 'cpuid'.
  0x80003E23, // Small 'cqo'.
  0x81DF0E43, // Small 'crc32'.
  0x208763EE, // Large 'cvtdq2|pd'.
  0x207063EE, // Large 'cvtdq2|ps'.
  0x20C5627F, // Large 'cvtpd2|dq'.
  0x21E2627F, // Large 'cvtpd2|pi'.
  0x2070627F, // Large 'cvtpd2|ps'.
  0x34875510, // Large 'cvtpi|2pd'.
  0x306F5510, // Large 'cvtpi|2ps'.
  0x20C56293, // Large 'cvtps2|dq'.
  0x10267293, // Large 'cvtps2p|d'.
  0x10097293, // Large 'cvtps2p|i'.
  0x201D629C, // Large 'cvtsd2|si'.
  0x201C629C, // Large 'cvtsd2|ss'.
  0x210E63FE, // Large 'cvtsi2|sd'.
  0x201C63FE, // Large 'cvtsi2|ss'.
  0x210E62AC, // Large 'cvtss2|sd'.
  0x201D62AC, // Large 'cvtss2|si'.
  0x20C571A6, // Large 'cvttpd2|dq'.
  0x21E271A6, // Large 'cvttpd2|pi'.
  0x20C571BA, // Large 'cvttps2|dq'.
  0x21E271BA, // Large 'cvttps2|pi'.
  0x201D71C3, // Large 'cvttsd2|si'.
  0x201D71D5, // Large 'cvttss2|si'.
  0x800012E3, // Small 'cwd'.
  0x800292E3, // Small 'cwde'.
  0x80000424, // Small 'daa'.
  0x80004C24, // Small 'das'.
  0x80000CA4, // Small 'dec'.
  0x80005924, // Small 'div'.
  0x80485924, // Small 'divpd'.
  0x81385924, // Small 'divps'.
  0x8049D924, // Small 'divsd'.
  0x8139D924, // Small 'divss'.
  0x80024204, // Small 'dppd'.
  0x8009C204, // Small 'dpps'.
  0x8009B5A5, // Small 'emms'.
  0x202C563A, // Large 'endbr|32'.
  0x2030563A, // Large 'endbr|64'.
  0x88D1C5C5, // Small 'enqcmd'.
  0x207B563F, // Large 'enqcm|ds'.
  0x8122D1C5, // Small 'enter'.
  0x207070F0, // Large 'extract|ps'.
  0x81195305, // Small 'extrq'.
  0x81C6E3A6, // Small 'f2xm1'.
  0x80098826, // Small 'fabs'.
  0x80021026, // Small 'fadd'.
  0x81021026, // Small 'faddp'.
  0x80023046, // Small 'fbld'.
  0x810A4C46, // Small 'fbstp'.
  0x8009A066, // Small 'fchs'.
  0x8182B066, // Small 'fclex'.
  0x8567B466, // Small 'fcmovb'.
  0x26445515, // Large 'fcmov|be'.
  0x8B67B466, // Small 'fcmove'.
  0x00007515, // Large 'fcmovnb'.
  0x100B7515, // Large 'fcmovnb|e'.
  0x100B6515, // Large 'fcmovn|e'.
  0x107D6515, // Large 'fcmovn|u'.
  0xAB67B466, // Small 'fcmovu'.
  0x8006BC66, // Small 'fcom'.
  0x8096BC66, // Small 'fcomi'.
  0xA096BC66, // Small 'fcomip'.
  0x8106BC66, // Small 'fcomp'.
  0xA106BC66, // Small 'fcompp'.
  0x8009BC66, // Small 'fcos'.
  0x21A95646, // Large 'fdecs|tp'.
  0x800B2486, // Small 'fdiv'.
  0x810B2486, // Small 'fdivp'.
  0x812B2486, // Small 'fdivr'.
  0xA12B2486, // Small 'fdivrp'.
  0x8136B4A6, // Small 'femms'.
  0x8052C8C6, // Small 'ffree'.
  0x80420526, // Small 'fiadd'.
  0x80D78D26, // Small 'ficom'.
  0xA0D78D26, // Small 'ficomp'.
  0x81649126, // Small 'fidiv'.
  0xA5649126, // Small 'fidivr'.
  0x80023126, // Small 'fild'.
  0x80CAB526, // Small 'fimul'.
  0x21A9564B, // Large 'fincs|tp'.
  0x8144B926, // Small 'finit'.
  0x800A4D26, // Small 'fist'.
  0x810A4D26, // Small 'fistp'.
  0xA14A4D26, // Small 'fisttp'.
  0x802ACD26, // Small 'fisub'.
  0xA42ACD26, // Small 'fisubr'.
  0x80001186, // Small 'fld'.
  0x800E1186, // Small 'fld1'.
  0x81719186, // Small 'fldcw'.
  0xACE29186, // Small 'fldenv'.
  0x8BD61186, // Small 'fldl2e'.
  0xA9D61186, // Small 'fldl2t'.
  0xBA761186, // Small 'fldlg2'.
  0xBAE61186, // Small 'fldln2'.
  0x80981186, // Small 'fldpi'.
  0x800D1186, // Small 'fldz'.
  0x800655A6, // Small 'fmul'.
  0x810655A6, // Small 'fmulp'.
  0xB0560DC6, // Small 'fnclex'.
  0xA89725C6, // Small 'fninit'.
  0x80083DC6, // Small 'fnop'.
  0x8B60CDC6, // Small 'fnsave'.
  0xAE3A4DC6, // Small 'fnstcw'.
  0x200D5650, // Large 'fnste|nv'.
  0xAF3A4DC6, // Small 'fnstsw'.
  0x9C1A0606, // Small 'fpatan'.
  0x80D2CA06, // Small 'fprem'.
  0xB8D2CA06, // Small 'fprem1'.
  0x80E0D206, // Small 'fptan'.
  0x31054655, // Large 'frnd|int'.
  0xA4FA4E46, // Small 'frstor'.
  0x805B0666, // Small 'fsave'.
  0x8AC08E66, // Small 'fscale'.
  0x80072666, // Small 'fsin'.
  0x221D5659, // Large 'fsinc|os'.
  0x81494666, // Small 'fsqrt'.
  0x80005266, // Small 'fst'.
  0x8171D266, // Small 'fstcw'.
  0xACE2D266, // Small 'fstenv'.
  0x80085266, // Small 'fstp'.
  0x8179D266, // Small 'fstsw'.
  0x80015666, // Small 'fsub'.
  0x81015666, // Small 'fsubp'.
  0x81215666, // Small 'fsubr'.
  0xA1215666, // Small 'fsubrp'.
  0x800A4E86, // Small 'ftst'.
  0x80D78EA6, // Small 'fucom'.
  0x92D78EA6, // Small 'fucomi'.
  0x2543565E, // Large 'fucom|ip'.
  0xA0D78EA6, // Small 'fucomp'.
  0x2663565E, // Large 'fucom|pp'.
  0x814486E6, // Small 'fwait'.
  0x80068706, // Small 'fxam'.
  0x80040F06, // Small 'fxch'.
  0x00007385, // Large 'fxrstor'.
  0x20307385, // Large 'fxrstor|64'.
  0x8B60CF06, // Small 'fxsave'.
  0x2030651C, // Large 'fxsave|64'.
  0x50F22385, // Large 'fx|tract'.
  0x818EB326, // Small 'fyl2x'.
  0x26025665, // Large 'fyl2x|p1'.
  0x8659D0A7, // Small 'getsec'.
  0x1010F001, // Large 'gf2p8affineinvq|b'.
  0x200FB001, // Large 'gf2p8affine|qb'.
  0x42E25001, // Large 'gf2p8|mulb'.
  0x89021028, // Small 'haddpd'.
  0xA7021028, // Small 'haddps'.
  0x80005188, // Small 'hlt'.
  0xA8599648, // Small 'hreset'.
  0x89015668, // Small 'hsubpd'.
  0xA7015668, // Small 'hsubps'.
  0x800B2489, // Small 'idiv'.
  0x800655A9, // Small 'imul'.
  0x800001C9, // Small 'in'.
  0x80000DC9, // Small 'inc'.
  0x2087566A, // Large 'incss|pd'.
  0x266F566A, // Large 'incss|pq'.
  0x80004DC9, // Small 'ins'.
  0x20706149, // Large 'insert|ps'.
  0x100F6149, // Large 'insert|q'.
  0x800051C9, // Small 'int'.
  0x800F51C9, // Small 'int3'.
  0x8007D1C9, // Small 'into'.
  0x800259C9, // Small 'invd'.
  0xA902D9C9, // Small 'invept'.
  0x8F0659C9, // Small 'invlpg'.
  0x33164671, // Large 'invl|pga'.
  0x23A05675, // Large 'invpc|id'.
  0x23A0567A, // Large 'invvp|id'.
  0x800A1649, // Small 'iret'.
  0x804A1649, // Small 'iretd'.
  0x811A1649, // Small 'iretq'.
  0x8000002A, // Small 'ja'.
  0x8000142A, // Small 'jae'.
  0x8000004A, // Small 'jb'.
  0x8000144A, // Small 'jbe'.
  0x8000006A, // Small 'jc'.
  0x800000AA, // Small 'je'.
  0x81AC0CAA, // Small 'jecxz'.
  0x800000EA, // Small 'jg'.
  0x800014EA, // Small 'jge'.
  0x8000018A, // Small 'jl'.
  0x8000158A, // Small 'jle'.
  0x800041AA, // Small 'jmp'.
  0x800005CA, // Small 'jna'.
  0x800285CA, // Small 'jnae'.
  0x800009CA, // Small 'jnb'.
  0x800289CA, // Small 'jnbe'.
  0x80000DCA, // Small 'jnc'.
  0x800015CA, // Small 'jne'.
  0x80001DCA, // Small 'jng'.
  0x80029DCA, // Small 'jnge'.
  0x800031CA, // Small 'jnl'.
  0x8002B1CA, // Small 'jnle'.
  0x80003DCA, // Small 'jno'.
  0x800041CA, // Small 'jnp'.
  0x80004DCA, // Small 'jns'.
  0x800069CA, // Small 'jnz'.
  0x800001EA, // Small 'jo'.
  0x8000020A, // Small 'jp'.
  0x8000160A, // Small 'jpe'.
  0x80003E0A, // Small 'jpo'.
  0x8000026A, // Small 'js'.
  0x8000034A, // Small 'jz'.
  0x8022102B, // Small 'kaddb'.
  0x8042102B, // Small 'kaddd'.
  0x8112102B, // Small 'kaddq'.
  0x8172102B, // Small 'kaddw'.
  0x8022382B, // Small 'kandb'.
  0x8042382B, // Small 'kandd'.
  0x84E2382B, // Small 'kandnb'.
  0x88E2382B, // Small 'kandnd'.
  0xA2E2382B, // Small 'kandnq'.
  0xAEE2382B, // Small 'kandnw'.
  0x8112382B, // Small 'kandq'.
  0x8172382B, // Small 'kandw'.
  0x802B3DAB, // Small 'kmovb'.
  0x804B3DAB, // Small 'kmovd'.
  0x811B3DAB, // Small 'kmovq'.
  0x817B3DAB, // Small 'kmovw'.
  0x802A3DCB, // Small 'knotb'.
  0x804A3DCB, // Small 'knotd'.
  0x811A3DCB, // Small 'knotq'.
  0x817A3DCB, // Small 'knotw'.
  0x800149EB, // Small 'korb'.
  0x800249EB, // Small 'kord'.
  0x8008C9EB, // Small 'korq'.
  0x10107522, // Large 'kortest|b'.
  0x10267522, // Large 'kortest|d'.
  0x100F7522, // Large 'kortest|q'.
  0x105F7522, // Large 'kortest|w'.
  0x800BC9EB, // Small 'korw'.
  0x22E46529, // Large 'kshift|lb'.
  0x234B6529, // Large 'kshift|ld'.
  0x22406529, // Large 'kshift|lq'.
  0x234E6529, // Large 'kshift|lw'.
  0x252F6529, // Large 'kshift|rb'.
  0x10267529, // Large 'kshiftr|d'.
  0x100F7529, // Large 'kshiftr|q'.
  0x105F7529, // Large 'kshiftr|w'.
  0x8549968B, // Small 'ktestb'.
  0x8949968B, // Small 'ktestd'.
  0xA349968B, // Small 'ktestq'.
  0xAF49968B, // Small 'ktestw'.
  0x23446531, // Large 'kunpck|bw'.
  0x20C56531, // Large 'kunpck|dq'.
  0x23466531, // Large 'kunpck|wd'.
  0x8527BB0B, // Small 'kxnorb'.
  0x8927BB0B, // Small 'kxnord'.
  0xA327BB0B, // Small 'kxnorq'.
  0xAF27BB0B, // Small 'kxnorw'.
  0x80293F0B, // Small 'kxorb'.
  0x80493F0B, // Small 'kxord'.
  0x81193F0B, // Small 'kxorq'.
  0x81793F0B, // Small 'kxorw'.
  0x8003202C, // Small 'lahf'.
  0x8000482C, // Small 'lar'.
  0x80C6046C, // Small 'lcall'.
  0x8158908C, // Small 'lddqu'.
  0x1023657B, // Large 'ldmxcs|r'.
  0x80004C8C, // Small 'lds'.
  0x1001838C, // Large 'ldtilecf|g'.
  0x800004AC, // Small 'lea'.
  0x805B04AC, // Small 'leave'.
  0x80004CAC, // Small 'les'.
  0x8A3714CC, // Small 'lfence'.
  0x80004CCC, // Small 'lfs'.
  0x800A10EC, // Small 'lgdt'.
  0x80004CEC, // Small 'lgs'.
  0x800A112C, // Small 'lidt'.
  0x8008354C, // Small 'ljmp'.
  0x800A118C, // Small 'lldt'.
  0x84385D8C, // Small 'llwpcb'.
  0x800BCDAC, // Small 'lmsw'.
  0x800991EC, // Small 'lods'.
  0x80083DEC, // Small 'loop'.
  0x80583DEC, // Small 'loope'.
  0x8AE83DEC, // Small 'loopne'.
  0x8000326C, // Small 'lsl'.
  0x80004E6C, // Small 'lss'.
  0x80004A8C, // Small 'ltr'.
  0xA6E4C2EC, // Small 'lwpins'.
  0x981B42EC, // Small 'lwpval'.
  0x81470F4C, // Small 'lzcnt'.
  0x107D91F9, // Large 'maskmovdq|u'.
  0x100F71F9, // Large 'maskmov|q'.
  0x8048602D, // Small 'maxpd'.
  0x8138602D, // Small 'maxps'.
  0x8049E02D, // Small 'maxsd'.
  0x8139E02D, // Small 'maxss'.
  0x2157567F, // Large 'mcomm|it'.
  0x8A3714CD, // Small 'mfence'.
  0x8048392D, // Small 'minpd'.
  0x8138392D, // Small 'minps'.
  0x8049B92D, // Small 'minsd'.
  0x8139B92D, // Small 'minss'.
  0x00007537, // Large 'monitor'.
  0x102E7537, // Large 'monitor|x'.
  0x800059ED, // Small 'mov'.
  0xA620D9ED, // Small 'movabs'.
  0x8900D9ED, // Small 'movapd'.
  0xA700D9ED, // Small 'movaps'.
  0x805159ED, // Small 'movbe'.
  0x800259ED, // Small 'movd'.
  0x358741FD, // Large 'movd|dup'.
  0x10108394, // Large 'movdir64|b'.
  0x10096394, // Large 'movdir|i'.
  0x25BD51FD, // Large 'movdq|2q'.
  0x831259ED, // Small 'movdqa'.
  0xAB1259ED, // Small 'movdqu'.
  0x34ED458D, // Large 'movh|lps'.
  0x890459ED, // Small 'movhpd'.
  0xA70459ED, // Small 'movhps'.
  0x20705592, // Large 'movlh|ps'.
  0x890659ED, // Small 'movlpd'.
  0xA70659ED, // Small 'movlps'.
  0x20876443, // Large 'movmsk|pd'.
  0x20706443, // Large 'movmsk|ps'.
  0x20C5544A, // Large 'movnt|dq'.
  0x3436544A, // Large 'movnt|dqa'.
  0x934759ED, // Small 'movnti'.
  0x2087544A, // Large 'movnt|pd'.
  0x2070544A, // Large 'movnt|ps'.
  0xA34759ED, // Small 'movntq'.
  0x210E544A, // Large 'movnt|sd'.
  0x201C544A, // Large 'movnt|ss'.
  0x8008D9ED, // Small 'movq'.
  0x20C55684, // Large 'movq2|dq'.
  0x8009D9ED, // Small 'movs'.
  0x8049D9ED, // Small 'movsd'.
  0x21E16450, // Large 'movshd|up'.
  0x21E16457, // Large 'movsld|up'.
  0x8139D9ED, // Small 'movss'.
  0x8189D9ED, // Small 'movsx'.
  0x8989D9ED, // Small 'movsxd'.
  0x890AD9ED, // Small 'movupd'.
  0xA70AD9ED, // Small 'movups'.
  0x818D59ED, // Small 'movzx'.
  0x23445598, // Large 'mpsad|bw'.
  0x800032AD, // Small 'mul'.
  0x804832AD, // Small 'mulpd'.
  0x813832AD, // Small 'mulps'.
  0x8049B2AD, // Small 'mulsd'.
  0x8139B2AD, // Small 'mulss'.
  0x800C32AD, // Small 'mulx'.
  0x814486ED, // Small 'mwait'.
  0xB14486ED, // Small 'mwaitx'.
  0x80001CAE, // Small 'neg'.
  0x800041EE, // Small 'nop'.
  0x800051EE, // Small 'not'.
  0x8000024F, // Small 'or'.
  0x8002424F, // Small 'orpd'.
  0x8009C24F, // Small 'orps'.
  0x800052AF, // Small 'out'.
  0x8009D2AF, // Small 'outs'.
  0x80298830, // Small 'pabsb'.
  0x80498830, // Small 'pabsd'.
  0x81798830, // Small 'pabsw'.
  0x0000845E, // Large 'packssdw'.
  0x2465645E, // Large 'packss|wb'.
  0x24646468, // Large 'packus|dw'.
  0x00008468, // Large 'packuswb'.
  0x80221030, // Small 'paddb'.
  0x80421030, // Small 'paddd'.
  0x81121030, // Small 'paddq'.
  0x85321030, // Small 'paddsb'.
  0xAF321030, // Small 'paddsw'.
  0x250D55A5, // Large 'paddu|sb'.
  0x232D55A5, // Large 'paddu|sw'.
  0x81721030, // Small 'paddw'.
  0x102365AB, // Large 'palign|r'.
  0x80023830, // Small 'pand'.
  0x80E23830, // Small 'pandn'.
  0x8059D430, // Small 'pause'.
  0x8023D830, // Small 'pavgb'.
  0x250D5689, // Large 'pavgu|sb'.
  0x8173D830, // Small 'pavgw'.
  0x20216471, // Large 'pblend|vb'.
  0x105F6471, // Large 'pblend|w'.
  0x424052EF, // Large 'pclmu|lqdq'.
  0x200F52F5, // Large 'pcmpe|qb'.
  0x223552F5, // Large 'pcmpe|qd'.
  0x21AE52F5, // Large 'pcmpe|qq'.
  0x24BB52F5, // Large 'pcmpe|qw'.
  0x100982F5, // Large 'pcmpestr|i'.
  0x105C82F5, // Large 'pcmpestr|m'.
  0x35B14255, // Large 'pcmp|gtb'.
  0x35B44255, // Large 'pcmp|gtd'.
  0x35B74255, // Large 'pcmp|gtq'.
  0x35BA4255, // Large 'pcmp|gtw'.
  0x100982FE, // Large 'pcmpistr|i'.
  0x105C82FE, // Large 'pcmpistr|m'.
  0x25AE520D, // Large 'pconf|ig'.
  0x80081490, // Small 'pdep'.
  0x800A60B0, // Small 'pext'.
  0x852A60B0, // Small 'pextrb'.
  0x892A60B0, // Small 'pextrd'.
  0xA32A60B0, // Small 'pextrq'.
  0xAF2A60B0, // Small 'pextrw'.
  0x8044F4D0, // Small 'pf2id'.
  0x8174F4D0, // Small 'pf2iw'.
  0x803184D0, // Small 'pfacc'.
  0x804204D0, // Small 'pfadd'.
  0x100F668E, // Large 'pfcmpe|q'.
  0x2018568E, // Large 'pfcmp|ge'.
  0x25B1568E, // Large 'pfcmp|gt'.
  0x8180B4D0, // Small 'pfmax'.
  0x80E4B4D0, // Small 'pfmin'.
  0x80CAB4D0, // Small 'pfmul'.
  0x8630B8D0, // Small 'pfnacc'.
  0x24245694, // Large 'pfpna|cc'.
  0x8101C8D0, // Small 'pfrcp'.
  0x2165653E, // Large 'pfrcpi|t1'.
  0x2261653E, // Large 'pfrcpi|t2'.
  0xAD01C8D0, // Small 'pfrcpv'.
  0x21656544, // Large 'pfrsqi|t1'.
  0x214D5544, // Large 'pfrsq|rt'.
  0x354A5544, // Large 'pfrsq|rtv'.
  0x802ACCD0, // Small 'pfsub'.
  0xA42ACCD0, // Small 'pfsubr'.
  0x88420510, // Small 'phaddd'.
  0x232D5498, // Large 'phadd|sw'.
  0xAE420510, // Small 'phaddw'.
  0x105F9217, // Large 'phminposu|w'.
  0x882ACD10, // Small 'phsubd'.
  0x232D55C4, // Large 'phsub|sw'.
  0xAE2ACD10, // Small 'phsubw'.
  0x80437530, // Small 'pi2fd'.
  0x81737530, // Small 'pi2fw'.
  0x8529B930, // Small 'pinsrb'.
  0x8929B930, // Small 'pinsrd'.
  0xA329B930, // Small 'pinsrq'.
  0xAF29B930, // Small 'pinsrw'.
  0x432F5221, // Large 'pmadd|ubsw'.
  0x23465221, // Large 'pmadd|wd'.
  0x853C05B0, // Small 'pmaxsb'.
  0x893C05B0, // Small 'pmaxsd'.
  0xAF3C05B0, // Small 'pmaxsw'.
  0x855C05B0, // Small 'pmaxub'.
  0x895C05B0, // Small 'pmaxud'.
  0xAF5C05B0, // Small 'pmaxuw'.
  0x853725B0, // Small 'pminsb'.
  0x893725B0, // Small 'pminsd'.
  0xAF3725B0, // Small 'pminsw'.
  0x855725B0, // Small 'pminub'.
  0x895725B0, // Small 'pminud'.
  0xAF5725B0, // Small 'pminuw'.
  0x101074A5, // Large 'pmovmsk|b'.
  0x102674AD, // Large 'pmovsxb|d'.
  0x100F74AD, // Large 'pmovsxb|q'.
  0x105F74AD, // Large 'pmovsxb|w'.
  0x20C564AD, // Large 'pmovsx|dq'.
  0x234664AD, // Large 'pmovsx|wd'.
  0x249F64AD, // Large 'pmovsx|wq'.
  0x102674BE, // Large 'pmovzxb|d'.
  0x100F74BE, // Large 'pmovzxb|q'.
  0x105F74BE, // Large 'pmovzxb|w'.
  0x20C564BE, // Large 'pmovzx|dq'.
  0x234664BE, // Large 'pmovzx|wd'.
  0x249F64BE, // Large 'pmovzx|wq'.
  0xA24655B0, // Small 'pmuldq'.
  0x232D64C6, // Large 'pmulhr|sw'.
  0x105F64C6, // Large 'pmulhr|w'.
  0x23F454C6, // Large 'pmulh|uw'.
  0xAE8655B0, // Small 'pmulhw'.
  0x88C655B0, // Small 'pmulld'.
  0xAEC655B0, // Small 'pmullw'.
  0x328F40AF, // Large 'pmul|udq'.
  0x800041F0, // Small 'pop'.
  0x8000C1F0, // Small 'popa'.
  0x8040C1F0, // Small 'popad'.
  0xA8E1C1F0, // Small 'popcnt'.
  0x800341F0, // Small 'popf'.
  0x804341F0, // Small 'popfd'.
  0x811341F0, // Small 'popfq'.
  0x800049F0, // Small 'por'.
  0x0000815A, // Large 'prefetch'.
  0x1006A15A, // Large 'prefetchnt|a'.
  0x225F815A, // Large 'prefetch|t0'.
  0x2165815A, // Large 'prefetch|t1'.
  0x2261815A, // Large 'prefetch|t2'.
  0x105F815A, // Large 'prefetch|w'.
  0x3164815A, // Large 'prefetch|wt1'.
  0xAE220670, // Small 'psadbw'.
  0x846AA270, // Small 'pshufb'.
  0x886AA270, // Small 'pshufd'.
  0x25F15151, // Large 'pshuf|hw'.
  0x234E5151, // Large 'pshuf|lw'.
  0xAE6AA270, // Small 'pshufw'.
  0x84E3A670, // Small 'psignb'.
  0x88E3A670, // Small 'psignd'.
  0xAEE3A670, // Small 'psignw'.
  0x80463270, // Small 'pslld'.
  0xA2463270, // Small 'pslldq'.
  0x81163270, // Small 'psllq'.
  0x81763270, // Small 'psllw'.
  0x9130B670, // Small 'psmash'.
  0x8040CA70, // Small 'psrad'.
  0x8170CA70, // Small 'psraw'.
  0x80464A70, // Small 'psrld'.
  0xA2464A70, // Small 'psrldq'.
  0x81164A70, // Small 'psrlq'.
  0x81764A70, // Small 'psrlw'.
  0x80215670, // Small 'psubb'.
  0x80415670, // Small 'psubd'.
  0x81115670, // Small 'psubq'.
  0x85315670, // Small 'psubsb'.
  0xAF315670, // Small 'psubsw'.
  0x250D55F4, // Large 'psubu|sb'.
  0x232D55F4, // Large 'psubu|sw'.
  0x81715670, // Small 'psubw'.
  0x8900DE70, // Small 'pswapd'.
  0x81499690, // Small 'ptest'.
  0x20BF5699, // Large 'ptwri|te'.
  0x23447238, // Large 'punpckh|bw'.
  0x20C57238, // Large 'punpckh|dq'.
  0x20C58238, // Large 'punpckhq|dq'.
  0x23467238, // Large 'punpckh|wd'.
  0x33486238, // Large 'punpck|lbw'.
  0x334B6238, // Large 'punpck|ldq'.
  0x42406238, // Large 'punpck|lqdq'.
  0x334E6238, // Large 'punpck|lwd'.
  0x80044EB0, // Small 'push'.
  0x80144EB0, // Small 'pusha'.
  0x88144EB0, // Small 'pushad'.
  0x80644EB0, // Small 'pushf'.
  0x88644EB0, // Small 'pushfd'.
  0xA2644EB0, // Small 'pushfq'.
  0x20BF739C, // Large 'pvalida|te'.
  0x80093F10, // Small 'pxor'.
  0x80003072, // Small 'rcl'.
  0x81384072, // Small 'rcpps'.
  0x8139C072, // Small 'rcpss'.
  0x80004872, // Small 'rcr'.
  0x33B0554D, // Large 'rdfsb|ase'.
  0x33B05552, // Large 'rdgsb|ase'.
  0x8129B492, // Small 'rdmsr'.
  0x8044C092, // Small 'rdpid'.
  0xAB25C092, // Small 'rdpkru'.
  0x8036C092, // Small 'rdpmc'.
  0x81594092, // Small 'rdpru'.
  0x88E0C892, // Small 'rdrand'.
  0x8852CC92, // Small 'rdseed'.
  0x8909CC92, // Small 'rdsspd'.
  0xA309CC92, // Small 'rdsspq'.
  0x8039D092, // Small 'rdtsc'.
  0xA039D092, // Small 'rdtscp'.
  0x800050B2, // Small 'ret'.
  0x800350B2, // Small 'retf'.
  0x201F73A3, // Large 'rmpadju|st'.
  0x20BF73AA, // Large 'rmpupda|te'.
  0x800031F2, // Small 'rol'.
  0x800049F2, // Small 'ror'.
  0x800C49F2, // Small 'rorx'.
  0x20875606, // Large 'round|pd'.
  0x20705606, // Large 'round|ps'.
  0x00007606, // Large 'roundsd'.
  0x10146606, // Large 'rounds|s'.
  0x80003672, // Small 'rsm'.
  0x20705352, // Large 'rsqrt|ps'.
  0x201C5352, // Large 'rsqrt|ss'.
  0x35575387, // Large 'rstor|ssp'.
  0x80032033, // Small 'sahf'.
  0x80003033, // Small 'sal'.
  0x80004833, // Small 'sar'.
  0x800C4833, // Small 'sarx'.
  0x1004A167, // Large 'saveprevss|p'.
  0x80000853, // Small 'sbb'.
  0x80098473, // Small 'scas'.
  0x21E2655A, // Large 'sendui|pi'.
  0x237573B1, // Large 'seriali|ze'.
  0x8000D0B3, // Small 'seta'.
  0x8050D0B3, // Small 'setae'.
  0x800150B3, // Small 'setb'.
  0x805150B3, // Small 'setbe'.
  0x8001D0B3, // Small 'setc'.
  0x8002D0B3, // Small 'sete'.
  0x8003D0B3, // Small 'setg'.
  0x8053D0B3, // Small 'setge'.
  0x800650B3, // Small 'setl'.
  0x805650B3, // Small 'setle'.
  0x801750B3, // Small 'setna'.
  0x8A1750B3, // Small 'setnae'.
  0x802750B3, // Small 'setnb'.
  0x8A2750B3, // Small 'setnbe'.
  0x803750B3, // Small 'setnc'.
  0x805750B3, // Small 'setne'.
  0x807750B3, // Small 'setng'.
  0x8A7750B3, // Small 'setnge'.
  0x80C750B3, // Small 'setnl'.
  0x8AC750B3, // Small 'setnle'.
  0x80F750B3, // Small 'setno'.
  0x810750B3, // Small 'setnp'.
  0x813750B3, // Small 'setns'.
  0x81A750B3, // Small 'setnz'.
  0x8007D0B3, // Small 'seto'.
  0x800850B3, // Small 'setp'.
  0x805850B3, // Small 'setpe'.
  0x80F850B3, // Small 'setpo'.
  0x8009D0B3, // Small 'sets'.
  0x10177560, // Large 'setssbs|y'.
  0x800D50B3, // Small 'setz'.
  0x8A3714D3, // Small 'sfence'.
  0x800A10F3, // Small 'sgdt'.
  0x426343B8, // Large 'sha1|msg1'.
  0x426743B8, // Large 'sha1|msg2'.
  0x20BF73B8, // Large 'sha1nex|te'.
  0x102F83BF, // Large 'sha1rnds|4'.
  0x42636171, // Large 'sha256|msg1'.
  0x42676171, // Large 'sha256|msg2'.
  0x20719171, // Large 'sha256rnd|s2'.
  0x80003113, // Small 'shl'.
  0x80023113, // Small 'shld'.
  0x800C3113, // Small 'shlx'.
  0x80004913, // Small 'shr'.
  0x80024913, // Small 'shrd'.
  0x800C4913, // Small 'shrx'.
  0x89035513, // Small 'shufpd'.
  0xA7035513, // Small 'shufps'.
  0x800A1133, // Small 'sidt'.
  0xA8972573, // Small 'skinit'.
  0x800A1193, // Small 'sldt'.
  0x84385D93, // Small 'slwpcb'.
  0x800BCDB3, // Small 'smsw'.
  0x890A4A33, // Small 'sqrtpd'.
  0xA70A4A33, // Small 'sqrtps'.
  0x893A4A33, // Small 'sqrtsd'.
  0xA73A4A33, // Small 'sqrtss'.
  0x80018693, // Small 'stac'.
  0x80000E93, // Small 'stc'.
  0x80001293, // Small 'std'.
  0x80049E93, // Small 'stgi'.
  0x80002693, // Small 'sti'.
  0x1023660E, // Large 'stmxcs|r'.
  0x8009BE93, // Small 'stos'.
  0x80004A93, // Small 'str'.
  0x100183C7, // Large 'sttilecf|g'.
  0x8004D693, // Small 'stui'.
  0x80000AB3, // Small 'sub'.
  0x80480AB3, // Small 'subpd'.
  0x81380AB3, // Small 'subps'.
  0x80498AB3, // Small 'subsd'.
  0x81398AB3, // Small 'subss'.
  0xA67806F3, // Small 'swapgs'.
  0x361A469E, // Large 'sysc|all'.
  0x41064567, // Large 'syse|nter'.
  0x2157556B, // Large 'sysex|it'.
  0x3157556B, // Large 'sysex|itq'.
  0xA8594F33, // Small 'sysret'.
  0x215856A2, // Large 'sysre|tq'.
  0x86B9B794, // Small 't1mskc'.
  0x207073CF, // Large 'tdpbf16|ps'.
  0x332243CF, // Large 'tdpb|ssd'.
  0x328E43CF, // Large 'tdpb|sud'.
  0x210E56A7, // Large 'tdpbu|sd'.
  0x228F56A7, // Large 'tdpbu|ud'.
  0x800A4CB4, // Small 'test'.
  0x935A4CB4, // Small 'testui'.
  0x0000917A, // Large 'tileloadd'.
  0x2165917A, // Large 'tileloadd|t1'.
  0x210A9183, // Large 'tilerelea|se'.
  0x1026926B, // Large 'tilestore|d'.
  0x4375417A, // Large 'tile|zero'.
  0x8B3A8614, // Small 'tpause'.
  0x81470F54, // Small 'tzcnt'.
  0x80B9B754, // Small 'tzmsk'.
  0x210E5615, // Large 'ucomi|sd'.
  0x201C5615, // Large 'ucomi|ss'.
  0x80006C95, // Small 'ud0'.
  0x80007095, // Small 'ud1'.
  0x80007495, // Small 'ud2'.
  0x8142C935, // Small 'uiret'.
  0x7537107D, // Large 'u|monitor'.
  0xA890DDB5, // Small 'umwait'.
  0x20876239, // Large 'unpckh|pd'.
  0x20706239, // Large 'unpckh|ps'.
  0x34EA5239, // Large 'unpck|lpd'.
  0x34ED5239, // Large 'unpck|lps'.
  0x30D163D6, // Large 'v4fmad|dps'.
  0x327B63D6, // Large 'v4fmad|dss'.
  0x30D17274, // Large 'v4fnmad|dps'.
  0x327B7274, // Large 'v4fnmad|dss'.
  0x89021036, // Small 'vaddpd'.
  0x91021036, // Small 'vaddph'.
  0xA7021036, // Small 'vaddps'.
  0x89321036, // Small 'vaddsd'.
  0x91321036, // Small 'vaddsh'.
  0xA7321036, // Small 'vaddss'.
  0x208773DC, // Large 'vaddsub|pd'.
  0x207073DC, // Large 'vaddsub|ps'.
  0x0000718C, // Large 'vaesdec'.
  0x3028818C, // Large 'vaesdecl|ast'.
  0x00007194, // Large 'vaesenc'.
  0x30288194, // Large 'vaesencl|ast'.
  0x267F56AC, // Large 'vaesi|mc'.
  0x1020F011, // Large 'vaeskeygenassis|t'.
  0x217856B1, // Large 'valig|nd'.
  0x264056B1, // Large 'valig|nq'.
  0x208756B6, // Large 'vandn|pd'.
  0x207056B6, // Large 'vandn|ps'.
  0x89023836, // Small 'vandpd'.
  0xA7023836, // Small 'vandps'.
  0x208773E3, // Large 'vblendm|pd'.
  0x207073E3, // Large 'vblendm|ps'.
  0x208763E3, // Large 'vblend|pd'.
  0x207063E3, // Large 'vblend|ps'.
  0x33EA63E3, // Large 'vblend|vpd'.
  0x315063E3, // Large 'vblend|vps'.
  0x3062B021, // Large 'vbroadcastf|128'.
  0x1003E021, // Large 'vbroadcastf32x|2'.
  0x102FE021, // Large 'vbroadcastf32x|4'.
  0x1005E021, // Large 'vbroadcastf32x|8'.
  0x4030B021, // Large 'vbroadcastf|64x2'.
  0x4034B021, // Large 'vbroadcastf|64x4'.
  0x4065A021, // Large 'vbroadcast|i128'.
  0x5038A021, // Large 'vbroadcast|i32x2'.
  0x503DA021, // Large 'vbroadcast|i32x4'.
  0x5042A021, // Large 'vbroadcast|i32x8'.
  0x5047A021, // Large 'vbroadcast|i64x2'.
  0x504CA021, // Large 'vbroadcast|i64x4'.
  0x210EA021, // Large 'vbroadcast|sd'.
  0x201CA021, // Large 'vbroadcast|ss'.
  0x89083476, // Small 'vcmppd'.
  0x91083476, // Small 'vcmpph'.
  0xA7083476, // Small 'vcmpps'.
  0x89383476, // Small 'vcmpsd'.
  0x91383476, // Small 'vcmpsh'.
  0xA7383476, // Small 'vcmpss'.
  0x210E56BB, // Large 'vcomi|sd'.
  0x20B556BB, // Large 'vcomi|sh'.
  0x201C56BB, // Large 'vcomi|ss'.
  0x2087919C, // Large 'vcompress|pd'.
  0x2070919C, // Large 'vcompress|ps'.
  0x208773ED, // Large 'vcvtdq2|pd'.
  0x208273ED, // Large 'vcvtdq2|ph'.
  0x207073ED, // Large 'vcvtdq2|ps'.
  0x1030D069, // Large 'vcvtne2ps2bf1|6'.
  0x1030C0DC, // Large 'vcvtneps2bf1|6'.
  0x20C5727E, // Large 'vcvtpd2|dq'.
  0x2082727E, // Large 'vcvtpd2|ph'.
  0x2070727E, // Large 'vcvtpd2|ps'.
  0x21AE727E, // Large 'vcvtpd2|qq'.
  0x20C5827E, // Large 'vcvtpd2u|dq'.
  0x21AE827E, // Large 'vcvtpd2u|qq'.
  0x20C57286, // Large 'vcvtph2|dq'.
  0x10268286, // Large 'vcvtph2p|d'.
  0x00009286, // Large 'vcvtph2ps'.
  0x102E9286, // Large 'vcvtph2ps|x'.
  0x21AE7286, // Large 'vcvtph2|qq'.
  0x328F7286, // Large 'vcvtph2|udq'.
  0x31AD7286, // Large 'vcvtph2|uqq'.
  0x23F47286, // Large 'vcvtph2|uw'.
  0x105F7286, // Large 'vcvtph2|w'.
  0x20C57292, // Large 'vcvtps2|dq'.
  0x10268292, // Large 'vcvtps2p|d'.
  0x00009292, // Large 'vcvtps2ph'.
  0x102E9292, // Large 'vcvtps2ph|x'.
  0x21AE7292, // Large 'vcvtps2|qq'.
  0x328F7292, // Large 'vcvtps2|udq'.
  0x31AD7292, // Large 'vcvtps2|uqq'.
  0x208773F6, // Large 'vcvtqq2|pd'.
  0x208273F6, // Large 'vcvtqq2|ph'.
  0x207073F6, // Large 'vcvtqq2|ps'.
  0x20B5729B, // Large 'vcvtsd2|sh'.
  0x201D729B, // Large 'vcvtsd2|si'.
  0x201C729B, // Large 'vcvtsd2|ss'.
  0x201D829B, // Large 'vcvtsd2u|si'.
  0x210E72A3, // Large 'vcvtsh2|sd'.
  0x201D72A3, // Large 'vcvtsh2|si'.
  0x201C72A3, // Large 'vcvtsh2|ss'.
  0x201D82A3, // Large 'vcvtsh2u|si'.
  0x210E73FD, // Large 'vcvtsi2|sd'.
  0x20B573FD, // Large 'vcvtsi2|sh'.
  0x201C73FD, // Large 'vcvtsi2|ss'.
  0x210E72AB, // Large 'vcvtss2|sd'.
  0x20B572AB, // Large 'vcvtss2|sh'.
  0x201D72AB, // Large 'vcvtss2|si'.
  0x201D82AB, // Large 'vcvtss2u|si'.
  0x20C581A5, // Large 'vcvttpd2|dq'.
  0x21AE81A5, // Large 'vcvttpd2|qq'.
  0x20C591A5, // Large 'vcvttpd2u|dq'.
  0x21AE91A5, // Large 'vcvttpd2u|qq'.
  0x20C581B0, // Large 'vcvttph2|dq'.
  0x21AE81B0, // Large 'vcvttph2|qq'.
  0x20C591B0, // Large 'vcvttph2u|dq'.
  0x21AE91B0, // Large 'vcvttph2u|qq'.
  0x105F91B0, // Large 'vcvttph2u|w'.
  0x105F81B0, // Large 'vcvttph2|w'.
  0x20C581B9, // Large 'vcvttps2|dq'.
  0x21AE81B9, // Large 'vcvttps2|qq'.
  0x20C591B9, // Large 'vcvttps2u|dq'.
  0x21AE91B9, // Large 'vcvttps2u|qq'.
  0x201D81C2, // Large 'vcvttsd2|si'.
  0x201D91C2, // Large 'vcvttsd2u|si'.
  0x201D81CB, // Large 'vcvttsh2|si'.
  0x201D91CB, // Large 'vcvttsh2u|si'.
  0x201D81D4, // Large 'vcvttss2|si'.
  0x201D91D4, // Large 'vcvttss2u|si'.
  0x208782B3, // Large 'vcvtudq2|pd'.
  0x208282B3, // Large 'vcvtudq2|ph'.
  0x207082B3, // Large 'vcvtudq2|ps'.
  0x208782BB, // Large 'vcvtuqq2|pd'.
  0x208282BB, // Large 'vcvtuqq2|ph'.
  0x207082BB, // Large 'vcvtuqq2|ps'.
  0x210E82C3, // Large 'vcvtusi2|sd'.
  0x20B582C3, // Large 'vcvtusi2|sh'.
  0x201C82C3, // Large 'vcvtusi2|ss'.
  0x30816404, // Large 'vcvtuw|2ph'.
  0x30815570, // Large 'vcvtw|2ph'.
  0x2344740A, // Large 'vdbpsad|bw'.
  0x890B2496, // Small 'vdivpd'.
  0x910B2496, // Small 'vdivph'.
  0xA70B2496, // Small 'vdivps'.
  0x893B2496, // Small 'vdivsd'.
  0x913B2496, // Small 'vdivsh'.
  0xA73B2496, // Small 'vdivss'.
  0x20707411, // Large 'vdpbf16|ps'.
  0x80484096, // Small 'vdppd'.
  0x81384096, // Small 'vdpps'.
  0x800948B6, // Small 'verr'.
  0x800BC8B6, // Small 'verw'.
  0x34874418, // Large 'vexp|2pd'.
  0x306F4418, // Large 'vexp|2ps'.
  0x30CD6418, // Large 'vexpan|dpd'.
  0x30D16418, // Large 'vexpan|dps'.
  0x306290EF, // Large 'vextractf|128'.
  0x602A70E8, // Large 'vextrac|tf32x4'.
  0x404390EF, // Large 'vextractf|32x8'.
  0x403090EF, // Large 'vextractf|64x2'.
  0x403490EF, // Large 'vextractf|64x4'.
  0x406580EF, // Large 'vextract|i128'.
  0x503D80EF, // Large 'vextract|i32x4'.
  0x504280EF, // Large 'vextract|i32x8'.
  0x504780EF, // Large 'vextract|i64x2'.
  0x504C80EF, // Large 'vextract|i64x4'.
  0x207080EF, // Large 'vextract|ps'.
  0x208282CB, // Large 'vfcmaddc|ph'.
  0x20B582CB, // Large 'vfcmaddc|sh'.
  0x2082741E, // Large 'vfcmulc|ph'.
  0x20B5741E, // Large 'vfcmulc|sh'.
  0x208791DD, // Large 'vfixupimm|pd'.
  0x207091DD, // Large 'vfixupimm|ps'.
  0x210E91DD, // Large 'vfixupimm|sd'.
  0x201C91DD, // Large 'vfixupimm|ss'.
  0x208791E6, // Large 'vfmadd132|pd'.
  0x208291E6, // Large 'vfmadd132|ph'.
  0x207091E6, // Large 'vfmadd132|ps'.
  0x210E91E6, // Large 'vfmadd132|sd'.
  0x20B591E6, // Large 'vfmadd132|sh'.
  0x201C91E6, // Large 'vfmadd132|ss'.
  0x50846076, // Large 'vfmadd|213pd'.
  0x50896076, // Large 'vfmadd|213ph'.
  0x508E6076, // Large 'vfmadd|213ps'.
  0x511A6076, // Large 'vfmadd|213sd'.
  0x511F6076, // Large 'vfmadd|213sh'.
  0x51246076, // Large 'vfmadd|213ss'.
  0x50936076, // Large 'vfmadd|231pd'.
  0x50986076, // Large 'vfmadd|231ph'.
  0x509D6076, // Large 'vfmadd|231ps'.
  0x51296076, // Large 'vfmadd|231sd'.
  0x512E6076, // Large 'vfmadd|231sh'.
  0x51336076, // Large 'vfmadd|231ss'.
  0x34256076, // Large 'vfmadd|cph'.
  0x34286076, // Large 'vfmadd|csh'.
  0x20876076, // Large 'vfmadd|pd'.
  0x20706076, // Large 'vfmadd|ps'.
  0x10267076, // Large 'vfmadds|d'.
  0x10147076, // Large 'vfmadds|s'.
  0x1026D076, // Large 'vfmaddsub132p|d'.
  0x1083D076, // Large 'vfmaddsub132p|h'.
  0x1014D076, // Large 'vfmaddsub132p|s'.
  0x50849076, // Large 'vfmaddsub|213pd'.
  0x50899076, // Large 'vfmaddsub|213ph'.
  0x508E9076, // Large 'vfmaddsub|213ps'.
  0x50939076, // Large 'vfmaddsub|231pd'.
  0x50989076, // Large 'vfmaddsub|231ph'.
  0x509D9076, // Large 'vfmaddsub|231ps'.
  0x20879076, // Large 'vfmaddsub|pd'.
  0x20709076, // Large 'vfmaddsub|ps'.
  0x208791EF, // Large 'vfmsub132|pd'.
  0x208291EF, // Large 'vfmsub132|ph'.
  0x207091EF, // Large 'vfmsub132|ps'.
  0x210E91EF, // Large 'vfmsub132|sd'.
  0x20B591EF, // Large 'vfmsub132|sh'.
  0x201C91EF, // Large 'vfmsub132|ss'.
  0x508460A2, // Large 'vfmsub|213pd'.
  0x508960A2, // Large 'vfmsub|213ph'.
  0x508E60A2, // Large 'vfmsub|213ps'.
  0x511A60A2, // Large 'vfmsub|213sd'.
  0x511F60A2, // Large 'vfmsub|213sh'.
  0x512460A2, // Large 'vfmsub|213ss'.
  0x509360A2, // Large 'vfmsub|231pd'.
  0x509860A2, // Large 'vfmsub|231ph'.
  0x509D60A2, // Large 'vfmsub|231ps'.
  0x512960A2, // Large 'vfmsub|231sd'.
  0x512E60A2, // Large 'vfmsub|231sh'.
  0x513360A2, // Large 'vfmsub|231ss'.
  0x2087C0A2, // Large 'vfmsubadd132|pd'.
  0x2082C0A2, // Large 'vfmsubadd132|ph'.
  0x2070C0A2, // Large 'vfmsubadd132|ps'.
  0x508490A2, // Large 'vfmsubadd|213pd'.
  0x508990A2, // Large 'vfmsubadd|213ph'.
  0x508E90A2, // Large 'vfmsubadd|213ps'.
  0x509390A2, // Large 'vfmsubadd|231pd'.
  0x509890A2, // Large 'vfmsubadd|231ph'.
  0x509D90A2, // Large 'vfmsubadd|231ps'.
  0x208790A2, // Large 'vfmsubadd|pd'.
  0x207090A2, // Large 'vfmsubadd|ps'.
  0x208760A2, // Large 'vfmsub|pd'.
  0x207060A2, // Large 'vfmsub|ps'.
  0x210E60A2, // Large 'vfmsub|sd'.
  0x201C60A2, // Large 'vfmsub|ss'.
  0x34255575, // Large 'vfmul|cph'.
  0x34285575, // Large 'vfmul|csh'.
  0x2087A110, // Large 'vfnmadd132|pd'.
  0x2082A110, // Large 'vfnmadd132|ph'.
  0x2070A110, // Large 'vfnmadd132|ps'.
  0x210EA110, // Large 'vfnmadd132|sd'.
  0x20B5A110, // Large 'vfnmadd132|sh'.
  0x201CA110, // Large 'vfnmadd132|ss'.
  0x50847110, // Large 'vfnmadd|213pd'.
  0x50897110, // Large 'vfnmadd|213ph'.
  0x508E7110, // Large 'vfnmadd|213ps'.
  0x511A7110, // Large 'vfnmadd|213sd'.
  0x511F7110, // Large 'vfnmadd|213sh'.
  0x51247110, // Large 'vfnmadd|213ss'.
  0x50937110, // Large 'vfnmadd|231pd'.
  0x50987110, // Large 'vfnmadd|231ph'.
  0x509D7110, // Large 'vfnmadd|231ps'.
  0x51297110, // Large 'vfnmadd|231sd'.
  0x512E7110, // Large 'vfnmadd|231sh'.
  0x51337110, // Large 'vfnmadd|231ss'.
  0x20877110, // Large 'vfnmadd|pd'.
  0x20707110, // Large 'vfnmadd|ps'.
  0x210E7110, // Large 'vfnmadd|sd'.
  0x201C7110, // Large 'vfnmadd|ss'.
  0x2087A138, // Large 'vfnmsub132|pd'.
  0x2082A138, // Large 'vfnmsub132|ph'.
  0x2070A138, // Large 'vfnmsub132|ps'.
  0x210EA138, // Large 'vfnmsub132|sd'.
  0x20B5A138, // Large 'vfnmsub132|sh'.
  0x201CA138, // Large 'vfnmsub132|ss'.
  0x50847138, // Large 'vfnmsub|213pd'.
  0x50897138, // Large 'vfnmsub|213ph'.
  0x508E7138, // Large 'vfnmsub|213ps'.
  0x511A7138, // Large 'vfnmsub|213sd'.
  0x511F7138, // Large 'vfnmsub|213sh'.
  0x51247138, // Large 'vfnmsub|213ss'.
  0x50937138, // Large 'vfnmsub|231pd'.
  0x50987138, // Large 'vfnmsub|231ph'.
  0x509D7138, // Large 'vfnmsub|231ps'.
  0x51297138, // Large 'vfnmsub|231sd'.
  0x512E7138, // Large 'vfnmsub|231sh'.
  0x51337138, // Large 'vfnmsub|231ss'.
  0x20877138, // Large 'vfnmsub|pd'.
  0x20707138, // Large 'vfnmsub|ps'.
  0x210E7138, // Large 'vfnmsub|sd'.
  0x201C7138, // Large 'vfnmsub|ss'.
  0x208782D3, // Large 'vfpclass|pd'.
  0x208282D3, // Large 'vfpclass|ph'.
  0x207082D3, // Large 'vfpclass|ps'.
  0x210E82D3, // Large 'vfpclass|sd'.
  0x20B582D3, // Large 'vfpclass|sh'.
  0x201C82D3, // Large 'vfpclass|ss'.
  0x208756C0, // Large 'vfrcz|pd'.
  0x207056C0, // Large 'vfrcz|ps'.
  0x210E56C0, // Large 'vfrcz|sd'.
  0x201C56C0, // Large 'vfrcz|ss'.
  0x30CD70F8, // Large 'vgather|dpd'.
  0x30D170F8, // Large 'vgather|dps'.
  0x30CDA0F8, // Large 'vgatherpf0|dpd'.
  0x30D1A0F8, // Large 'vgatherpf0|dps'.
  0x30C6A0F8, // Large 'vgatherpf0|qpd'.
  0x30C9A0F8, // Large 'vgatherpf0|qps'.
  0x40CC90F8, // Large 'vgatherpf|1dpd'.
  0x40D090F8, // Large 'vgatherpf|1dps'.
  0x40D490F8, // Large 'vgatherpf|1qpd'.
  0x40D890F8, // Large 'vgatherpf|1qps'.
  0x30C670F8, // Large 'vgather|qpd'.
  0x30C970F8, // Large 'vgather|qps'.
  0x2087742B, // Large 'vgetexp|pd'.
  0x2082742B, // Large 'vgetexp|ph'.
  0x2070742B, // Large 'vgetexp|ps'.
  0x210E742B, // Large 'vgetexp|sd'.
  0x20B5742B, // Large 'vgetexp|sh'.
  0x201C742B, // Large 'vgetexp|ss'.
  0x31A972DB, // Large 'vgetman|tpd'.
  0x31B472DB, // Large 'vgetman|tph'.
  0x31BD72DB, // Large 'vgetman|tps'.
  0x310D72DB, // Large 'vgetman|tsd'.
  0x31CF72DB, // Large 'vgetman|tsh'.
  0x31D872DB, // Large 'vgetman|tss'.
  0x200FF000, // Large 'vgf2p8affineinv|qb'.
  0x200FC000, // Large 'vgf2p8affine|qb'.
  0x42E26000, // Large 'vgf2p8|mulb'.
  0x30CD46C5, // Large 'vhad|dpd'.
  0x30D146C5, // Large 'vhad|dps'.
  0x208756C9, // Large 'vhsub|pd'.
  0x207056C9, // Large 'vhsub|ps'.
  0x30628148, // Large 'vinsertf|128'.
  0x602A6142, // Large 'vinser|tf32x4'.
  0x40438148, // Large 'vinsertf|32x8'.
  0x40308148, // Large 'vinsertf|64x2'.
  0x40348148, // Large 'vinsertf|64x4'.
  0x40657148, // Large 'vinsert|i128'.
  0x503D7148, // Large 'vinsert|i32x4'.
  0x50427148, // Large 'vinsert|i32x8'.
  0x50477148, // Large 'vinsert|i64x2'.
  0x504C7148, // Large 'vinsert|i64x4'.
  0x20707148, // Large 'vinsert|ps'.
  0xAB121196, // Small 'vlddqu'.
  0x1023757A, // Large 'vldmxcs|r'.
  0x107DA1F8, // Large 'vmaskmovdq|u'.
  0x208781F8, // Large 'vmaskmov|pd'.
  0x207081F8, // Large 'vmaskmov|ps'.
  0x890C05B6, // Small 'vmaxpd'.
  0x910C05B6, // Small 'vmaxph'.
  0xA70C05B6, // Small 'vmaxps'.
  0x893C05B6, // Small 'vmaxsd'.
  0x913C05B6, // Small 'vmaxsh'.
  0xA73C05B6, // Small 'vmaxss'.
  0x98C08DB6, // Small 'vmcall'.
  0x23A256CE, // Large 'vmcle|ar'.
  0x86EA99B6, // Small 'vmfunc'.
  0x890725B6, // Small 'vminpd'.
  0x910725B6, // Small 'vminph'.
  0xA70725B6, // Small 'vminps'.
  0x893725B6, // Small 'vminsd'.
  0x913725B6, // Small 'vminsh'.
  0xA73725B6, // Small 'vminss'.
  0x21606581, // Large 'vmlaun|ch'.
  0x8817B1B6, // Small 'vmload'.
  0x361A46D3, // Large 'vmmc|all'.
  0x208756D7, // Large 'vmova|pd'.
  0x207056D7, // Large 'vmova|ps'.
  0x804B3DB6, // Small 'vmovd'.
  0x35875432, // Large 'vmovd|dup'.
  0x00007432, // Large 'vmovdqa'.
  0x202C7432, // Large 'vmovdqa|32'.
  0x20307432, // Large 'vmovdqa|64'.
  0x107D6432, // Large 'vmovdq|u'.
  0x34396432, // Large 'vmovdq|u16'.
  0x343C6432, // Large 'vmovdq|u32'.
  0x343F6432, // Large 'vmovdq|u64'.
  0x258A6432, // Large 'vmovdq|u8'.
  0x34ED558C, // Large 'vmovh|lps'.
  0x2087558C, // Large 'vmovh|pd'.
  0x2070558C, // Large 'vmovh|ps'.
  0x20706591, // Large 'vmovlh|ps'.
  0x20875591, // Large 'vmovl|pd'.
  0x20705591, // Large 'vmovl|ps'.
  0x20877442, // Large 'vmovmsk|pd'.
  0x20707442, // Large 'vmovmsk|ps'.
  0x20C56449, // Large 'vmovnt|dq'.
  0x34366449, // Large 'vmovnt|dqa'.
  0x20876449, // Large 'vmovnt|pd'.
  0x20706449, // Large 'vmovnt|ps'.
  0x811B3DB6, // Small 'vmovq'.
  0x893B3DB6, // Small 'vmovsd'.
  0x913B3DB6, // Small 'vmovsh'.
  0x21E1744F, // Large 'vmovshd|up'.
  0x21E17456, // Large 'vmovsld|up'.
  0xA73B3DB6, // Small 'vmovss'.
  0x33AD4432, // Large 'vmov|upd'.
  0x207056DC, // Large 'vmovu|ps'.
  0x817B3DB6, // Small 'vmovw'.
  0x23446597, // Large 'vmpsad|bw'.
  0x338B46E1, // Large 'vmpt|rld'.
  0x338746E1, // Large 'vmpt|rst'.
  0x8812C9B6, // Small 'vmread'.
  0x100B759D, // Large 'vmresum|e'.
  0x80EAC9B6, // Small 'vmrun'.
  0x8B60CDB6, // Small 'vmsave'.
  0x890655B6, // Small 'vmulpd'.
  0x910655B6, // Small 'vmulph'.
  0xA70655B6, // Small 'vmulps'.
  0x893655B6, // Small 'vmulsd'.
  0x913655B6, // Small 'vmulsh'.
  0xA73655B6, // Small 'vmulss'.
  0x20BF56E5, // Large 'vmwri|te'.
  0x80E7E1B6, // Small 'vmxon'.
  0x804849F6, // Small 'vorpd'.
  0x813849F6, // Small 'vorps'.
  0x1026C102, // Large 'vp2intersect|d'.
  0x100FC102, // Large 'vp2intersect|q'.
  0x102682E6, // Large 'vp4dpwss|d'.
  0x207B82E6, // Large 'vp4dpwss|ds'.
  0x85310616, // Small 'vpabsb'.
  0x89310616, // Small 'vpabsd'.
  0xA3310616, // Small 'vpabsq'.
  0xAF310616, // Small 'vpabsw'.
  0x105F845D, // Large 'vpackssd|w'.
  0x2465745D, // Large 'vpackss|wb'.
  0x34636467, // Large 'vpacku|sdw'.
  0x346D6467, // Large 'vpacku|swb'.
  0x84420616, // Small 'vpaddb'.
  0x88420616, // Small 'vpaddd'.
  0xA2420616, // Small 'vpaddq'.
  0x250D55A4, // Large 'vpadd|sb'.
  0x232D55A4, // Large 'vpadd|sw'.
  0x250D65A4, // Large 'vpaddu|sb'.
  0x232D65A4, // Large 'vpaddu|sw'.
  0xAE420616, // Small 'vpaddw'.
  0x102375AA, // Large 'vpalign|r'.
  0x80470616, // Small 'vpand'.
  0x88470616, // Small 'vpandd'.
  0x9C470616, // Small 'vpandn'.
  0x217856EA, // Large 'vpand|nd'.
  0x264056EA, // Large 'vpand|nq'.
  0xA2470616, // Small 'vpandq'.
  0x847B0616, // Small 'vpavgb'.
  0xAE7B0616, // Small 'vpavgw'.
  0x10267470, // Large 'vpblend|d'.
  0x205C7470, // Large 'vpblend|mb'.
  0x24777470, // Large 'vpblend|md'.
  0x100F8470, // Large 'vpblendm|q'.
  0x105F8470, // Large 'vpblendm|w'.
  0x20217470, // Large 'vpblend|vb'.
  0x105F7470, // Large 'vpblend|w'.
  0x1010B051, // Large 'vpbroadcast|b'.
  0x1026B051, // Large 'vpbroadcast|d'.
  0x100FE051, // Large 'vpbroadcastmb2|q'.
  0x305FC051, // Large 'vpbroadcastm|w2d'.
  0x100FB051, // Large 'vpbroadcast|q'.
  0x105FB051, // Large 'vpbroadcast|w'.
  0x424062EE, // Large 'vpclmu|lqdq'.
  0xACF68E16, // Small 'vpcmov'.
  0x85068E16, // Small 'vpcmpb'.
  0x89068E16, // Small 'vpcmpd'.
  0x200F62F4, // Large 'vpcmpe|qb'.
  0x223562F4, // Large 'vpcmpe|qd'.
  0x21AE62F4, // Large 'vpcmpe|qq'.
  0x24BB62F4, // Large 'vpcmpe|qw'.
  0x100992F4, // Large 'vpcmpestr|i'.
  0x105C92F4, // Large 'vpcmpestr|m'.
  0x35B152F4, // Large 'vpcmp|gtb'.
  0x35B452F4, // Large 'vpcmp|gtd'.
  0x35B752F4, // Large 'vpcmp|gtq'.
  0x35BA52F4, // Large 'vpcmp|gtw'.
  0x100992FD, // Large 'vpcmpistr|i'.
  0x105C92FD, // Large 'vpcmpistr|m'.
  0xA3068E16, // Small 'vpcmpq'.
  0x207D52F4, // Large 'vpcmp|ub'.
  0x228F52F4, // Large 'vpcmp|ud'.
  0x21AD52F4, // Large 'vpcmp|uq'.
  0x23F452F4, // Large 'vpcmp|uw'.
  0xAF068E16, // Small 'vpcmpw'.
  0x84D78E16, // Small 'vpcomb'.
  0x88D78E16, // Small 'vpcomd'.
  0x1010A202, // Large 'vpcompress|b'.
  0x1026A202, // Large 'vpcompress|d'.
  0x100FA202, // Large 'vpcompress|q'.
  0x105FA202, // Large 'vpcompress|w'.
  0xA2D78E16, // Small 'vpcomq'.
  0x207D5202, // Large 'vpcom|ub'.
  0x228F5202, // Large 'vpcom|ud'.
  0x21AD5202, // Large 'vpcom|uq'.
  0x23F45202, // Large 'vpcom|uw'.
  0xAED78E16, // Small 'vpcomw'.
  0x1026A20C, // Large 'vpconflict|d'.
  0x100FA20C, // Large 'vpconflict|q'.
  0x10267479, // Large 'vpdpbus|d'.
  0x207B7479, // Large 'vpdpbus|ds'.
  0x10267480, // Large 'vpdpwss|d'.
  0x207B7480, // Large 'vpdpwss|ds'.
  0x30627306, // Large 'vperm2f|128'.
  0x40656306, // Large 'vperm2|i128'.
  0x84D91616, // Small 'vpermb'.
  0x88D91616, // Small 'vpermd'.
  0x2072630D, // Large 'vpermi|2b'.
  0x2060630D, // Large 'vpermi|2d'.
  0x3487630D, // Large 'vpermi|2pd'.
  0x306F630D, // Large 'vpermi|2ps'.
  0x25BD630D, // Large 'vpermi|2q'.
  0x205E630D, // Large 'vpermi|2w'.
  0x2087830D, // Large 'vpermil2|pd'.
  0x2070830D, // Large 'vpermil2|ps'.
  0x2087730D, // Large 'vpermil|pd'.
  0x2070730D, // Large 'vpermil|ps'.
  0x20875306, // Large 'vperm|pd'.
  0x20705306, // Large 'vperm|ps'.
  0xA2D91616, // Small 'vpermq'.
  0x2072648A, // Large 'vpermt|2b'.
  0x2060648A, // Large 'vpermt|2d'.
  0x3487648A, // Large 'vpermt|2pd'.
  0x306F648A, // Large 'vpermt|2ps'.
  0x25BD648A, // Large 'vpermt|2q'.
  0x205E648A, // Large 'vpermt|2w'.
  0xAED91616, // Small 'vpermw'.
  0x240B7490, // Large 'vpexpan|db'.
  0x207A7490, // Large 'vpexpan|dd'.
  0x20C57490, // Large 'vpexpan|dq'.
  0x24647490, // Large 'vpexpan|dw'.
  0x352E4490, // Large 'vpex|trb'.
  0x254D56EF, // Large 'vpext|rd'.
  0x223456EF, // Large 'vpext|rq'.
  0x26F456EF, // Large 'vpext|rw'.
  0x207A8315, // Large 'vpgather|dd'.
  0x20C58315, // Large 'vpgather|dq'.
  0x22358315, // Large 'vpgather|qd'.
  0x21AE8315, // Large 'vpgather|qq'.
  0x25BF6497, // Large 'vphadd|bd'.
  0x25C16497, // Large 'vphadd|bq'.
  0x23446497, // Large 'vphadd|bw'.
  0x10266497, // Large 'vphadd|d'.
  0x20C56497, // Large 'vphadd|dq'.
  0x232D6497, // Large 'vphadd|sw'.
  0x10268497, // Large 'vphaddub|d'.
  0x100F8497, // Large 'vphaddub|q'.
  0x105F8497, // Large 'vphaddub|w'.
  0x20C57497, // Large 'vphaddu|dq'.
  0x23467497, // Large 'vphaddu|wd'.
  0x249F7497, // Large 'vphaddu|wq'.
  0x105F6497, // Large 'vphadd|w'.
  0x23466497, // Large 'vphadd|wd'.
  0x249F6497, // Large 'vphadd|wq'.
  0x105FA216, // Large 'vphminposu|w'.
  0x234465C3, // Large 'vphsub|bw'.
  0x102665C3, // Large 'vphsub|d'.
  0x20C565C3, // Large 'vphsub|dq'.
  0x232D65C3, // Large 'vphsub|sw'.
  0x105F65C3, // Large 'vphsub|w'.
  0x234665C3, // Large 'vphsub|wd'.
  0x252F56F6, // Large 'vpins|rb'.
  0x254D56F6, // Large 'vpins|rd'.
  0x223456F6, // Large 'vpins|rq'.
  0x26F456F6, // Large 'vpins|rw'.
  0x23CF65C9, // Large 'vplzcn|td'.
  0x215865C9, // Large 'vplzcn|tq'.
  0x207A631D, // Large 'vpmacs|dd'.
  0x34A1631D, // Large 'vpmacs|dqh'.
  0x334C631D, // Large 'vpmacs|dql'.
  0x1026831D, // Large 'vpmacssd|d'.
  0x1083931D, // Large 'vpmacssdq|h'.
  0x10B2931D, // Large 'vpmacssdq|l'.
  0x2346731D, // Large 'vpmacss|wd'.
  0x2345731D, // Large 'vpmacss|ww'.
  0x2346631D, // Large 'vpmacs|wd'.
  0x2345631D, // Large 'vpmacs|ww'.
  0x10269326, // Large 'vpmadcssw|d'.
  0x23467326, // Large 'vpmadcs|wd'.
  0x21AD9220, // Large 'vpmadd52h|uq'.
  0x32298220, // Large 'vpmadd52|luq'.
  0x432F6220, // Large 'vpmadd|ubsw'.
  0x23466220, // Large 'vpmadd|wd'.
  0x61FB4220, // Large 'vpma|skmovd'.
  0x200E8333, // Large 'vpmaskmo|vq'.
  0x250D56FB, // Large 'vpmax|sb'.
  0x210E56FB, // Large 'vpmax|sd'.
  0x235356FB, // Large 'vpmax|sq'.
  0x232D56FB, // Large 'vpmax|sw'.
  0x207D56FB, // Large 'vpmax|ub'.
  0x228F56FB, // Large 'vpmax|ud'.
  0x21AD56FB, // Large 'vpmax|uq'.
  0x23F456FB, // Large 'vpmax|uw'.
  0x250D5700, // Large 'vpmin|sb'.
  0x210E5700, // Large 'vpmin|sd'.
  0x23535700, // Large 'vpmin|sq'.
  0x232D5700, // Large 'vpmin|sw'.
  0x207D5700, // Large 'vpmin|ub'.
  0x228F5700, // Large 'vpmin|ud'.
  0x21AD5700, // Large 'vpmin|uq'.
  0x23F45700, // Large 'vpmin|uw'.
  0x35CF54A4, // Large 'vpmov|b2m'.
  0x35D254A4, // Large 'vpmov|d2m'.
  0x240B54A4, // Large 'vpmov|db'.
  0x246454A4, // Large 'vpmov|dw'.
  0x207264A4, // Large 'vpmovm|2b'.
  0x206064A4, // Large 'vpmovm|2d'.
  0x25BD64A4, // Large 'vpmovm|2q'.
  0x205E64A4, // Large 'vpmovm|2w'.
  0x101084A4, // Large 'vpmovmsk|b'.
  0x35D554A4, // Large 'vpmov|q2m'.
  0x200F54A4, // Large 'vpmov|qb'.
  0x223554A4, // Large 'vpmov|qd'.
  0x24BB54A4, // Large 'vpmov|qw'.
  0x240B64AC, // Large 'vpmovs|db'.
  0x246464AC, // Large 'vpmovs|dw'.
  0x200F64AC, // Large 'vpmovs|qb'.
  0x223564AC, // Large 'vpmovs|qd'.
  0x24BB64AC, // Large 'vpmovs|qw'.
  0x246564AC, // Large 'vpmovs|wb'.
  0x102684AC, // Large 'vpmovsxb|d'.
  0x100F84AC, // Large 'vpmovsxb|q'.
  0x105F84AC, // Large 'vpmovsxb|w'.
  0x20C574AC, // Large 'vpmovsx|dq'.
  0x234674AC, // Large 'vpmovsx|wd'.
  0x249F74AC, // Large 'vpmovsx|wq'.
  0x240B74B4, // Large 'vpmovus|db'.
  0x246474B4, // Large 'vpmovus|dw'.
  0x200F74B4, // Large 'vpmovus|qb'.
  0x223574B4, // Large 'vpmovus|qd'.
  0x24BB74B4, // Large 'vpmovus|qw'.
  0x246574B4, // Large 'vpmovus|wb'.
  0x35D854A4, // Large 'vpmov|w2m'.
  0x246554A4, // Large 'vpmov|wb'.
  0x102684BD, // Large 'vpmovzxb|d'.
  0x100F84BD, // Large 'vpmovzxb|q'.
  0x105F84BD, // Large 'vpmovzxb|w'.
  0x20C574BD, // Large 'vpmovzx|dq'.
  0x234674BD, // Large 'vpmovzx|wd'.
  0x249F74BD, // Large 'vpmovzx|wq'.
  0x20C550AE, // Large 'vpmul|dq'.
  0x232D74C5, // Large 'vpmulhr|sw'.
  0x23F464C5, // Large 'vpmulh|uw'.
  0x105F64C5, // Large 'vpmulh|w'.
  0x234B50AE, // Large 'vpmul|ld'.
  0x224050AE, // Large 'vpmul|lq'.
  0x234E50AE, // Large 'vpmul|lw'.
  0x200FC0AE, // Large 'vpmultishift|qb'.
  0x328F50AE, // Large 'vpmul|udq'.
  0x25B265DB, // Large 'vpopcn|tb'.
  0x23CF65DB, // Large 'vpopcn|td'.
  0x215865DB, // Large 'vpopcn|tq'.
  0x216365DB, // Large 'vpopcn|tw'.
  0x80093E16, // Small 'vpor'.
  0x80493E16, // Small 'vpord'.
  0x81193E16, // Small 'vporq'.
  0x9B22C216, // Small 'vpperm'.
  0x88C7CA16, // Small 'vprold'.
  0xA2C7CA16, // Small 'vprolq'.
  0x21FF5705, // Large 'vprol|vd'.
  0x200E5705, // Large 'vprol|vq'.
  0x8927CA16, // Small 'vprord'.
  0xA327CA16, // Small 'vprorq'.
  0x21FF570A, // Large 'vpror|vd'.
  0x200E570A, // Large 'vpror|vq'.
  0x8547CA16, // Small 'vprotb'.
  0x8947CA16, // Small 'vprotd'.
  0xA347CA16, // Small 'vprotq'.
  0xAF47CA16, // Small 'vprotw'.
  0x2344570F, // Large 'vpsad|bw'.
  0x207A922C, // Large 'vpscatter|dd'.
  0x20C5922C, // Large 'vpscatter|dq'.
  0x2235922C, // Large 'vpscatter|qd'.
  0x100FA22C, // Large 'vpscatterq|q'.
  0x84144E16, // Small 'vpshab'.
  0x88144E16, // Small 'vpshad'.
  0xA2144E16, // Small 'vpshaq'.
  0xAE144E16, // Small 'vpshaw'.
  0x84C44E16, // Small 'vpshlb'.
  0x88C44E16, // Small 'vpshld'.
  0x102665E1, // Large 'vpshld|d'.
  0x100F65E1, // Large 'vpshld|q'.
  0x341055E1, // Large 'vpshl|dvd'.
  0x35E655E1, // Large 'vpshl|dvq'.
  0x105F75E1, // Large 'vpshldv|w'.
  0x105F65E1, // Large 'vpshld|w'.
  0xA2C44E16, // Small 'vpshlq'.
  0xAEC44E16, // Small 'vpshlw'.
  0x102665E9, // Large 'vpshrd|d'.
  0x100F65E9, // Large 'vpshrd|q'.
  0x341055E9, // Large 'vpshr|dvd'.
  0x35E655E9, // Large 'vpshr|dvq'.
  0x35EE55E9, // Large 'vpshr|dvw'.
  0x105F65E9, // Large 'vpshrd|w'.
  0x00007150, // Large 'vpshufb'.
  0x205CA150, // Large 'vpshufbitq|mb'.
  0x10266150, // Large 'vpshuf|d'.
  0x25F16150, // Large 'vpshuf|hw'.
  0x234E6150, // Large 'vpshuf|lw'.
  0x251A5714, // Large 'vpsig|nb'.
  0x21785714, // Large 'vpsig|nd'.
  0x26225714, // Large 'vpsig|nw'.
  0x88C64E16, // Small 'vpslld'.
  0x334B4719, // Large 'vpsl|ldq'.
  0xA2C64E16, // Small 'vpsllq'.
  0x21FF571D, // Large 'vpsll|vd'.
  0x200E571D, // Large 'vpsll|vq'.
  0x25EF571D, // Large 'vpsll|vw'.
  0xAEC64E16, // Small 'vpsllw'.
  0x88194E16, // Small 'vpsrad'.
  0xA2194E16, // Small 'vpsraq'.
  0x21FF5722, // Large 'vpsra|vd'.
  0x200E5722, // Large 'vpsra|vq'.
  0x25EF5722, // Large 'vpsra|vw'.
  0xAE194E16, // Small 'vpsraw'.
  0x88C94E16, // Small 'vpsrld'.
  0x334B4722, // Large 'vpsr|ldq'.
  0xA2C94E16, // Small 'vpsrlq'.
  0x21FF5727, // Large 'vpsrl|vd'.
  0x200E5727, // Large 'vpsrl|vq'.
  0x25EF5727, // Large 'vpsrl|vw'.
  0xAEC94E16, // Small 'vpsrlw'.
  0x842ACE16, // Small 'vpsubb'.
  0x882ACE16, // Small 'vpsubd'.
  0xA22ACE16, // Small 'vpsubq'.
  0x250D55F3, // Large 'vpsub|sb'.
  0x232D55F3, // Large 'vpsub|sw'.
  0x250D65F3, // Large 'vpsubu|sb'.
  0x232D65F3, // Large 'vpsubu|sw'.
  0xAE2ACE16, // Small 'vpsubw'.
  0x1026933B, // Large 'vpternlog|d'.
  0x100F933B, // Large 'vpternlog|q'.
  0xA932D216, // Small 'vptest'.
  0x205C64CC, // Large 'vptest|mb'.
  0x247764CC, // Large 'vptest|md'.
  0x24D364CC, // Large 'vptest|mq'.
  0x25D764CC, // Large 'vptest|mw'.
  0x205C74CC, // Large 'vptestn|mb'.
  0x247774CC, // Large 'vptestn|md'.
  0x24D374CC, // Large 'vptestn|mq'.
  0x105F84CC, // Large 'vptestnm|w'.
  0x23448237, // Large 'vpunpckh|bw'.
  0x20C58237, // Large 'vpunpckh|dq'.
  0x20C59237, // Large 'vpunpckhq|dq'.
  0x23468237, // Large 'vpunpckh|wd'.
  0x33487237, // Large 'vpunpck|lbw'.
  0x334B7237, // Large 'vpunpck|ldq'.
  0x42407237, // Large 'vpunpck|lqdq'.
  0x334E7237, // Large 'vpunpck|lwd'.
  0x8127E216, // Small 'vpxor'.
  0x8927E216, // Small 'vpxord'.
  0xA327E216, // Small 'vpxorq'.
  0x208765F9, // Large 'vrange|pd'.
  0x207065F9, // Large 'vrange|ps'.
  0x210E65F9, // Large 'vrange|sd'.
  0x201C65F9, // Large 'vrange|ss'.
  0x208765FF, // Large 'vrcp14|pd'.
  0x207065FF, // Large 'vrcp14|ps'.
  0x210E65FF, // Large 'vrcp14|sd'.
  0x201C65FF, // Large 'vrcp14|ss'.
  0x435945FF, // Large 'vrcp|28pd'.
  0x435D45FF, // Large 'vrcp|28ps'.
  0x436145FF, // Large 'vrcp|28sd'.
  0x436545FF, // Large 'vrcp|28ss'.
  0x91080E56, // Small 'vrcpph'.
  0xA7080E56, // Small 'vrcpps'.
  0x91380E56, // Small 'vrcpsh'.
  0xA7380E56, // Small 'vrcpss'.
  0x208774D5, // Large 'vreduce|pd'.
  0x208274D5, // Large 'vreduce|ph'.
  0x207074D5, // Large 'vreduce|ps'.
  0x210E74D5, // Large 'vreduce|sd'.
  0x20B574D5, // Large 'vreduce|sh'.
  0x201C74D5, // Large 'vreduce|ss'.
  0x20879244, // Large 'vrndscale|pd'.
  0x20829244, // Large 'vrndscale|ph'.
  0x20709244, // Large 'vrndscale|ps'.
  0x210E9244, // Large 'vrndscale|sd'.
  0x20B59244, // Large 'vrndscale|sh'.
  0x201C9244, // Large 'vrndscale|ss'.
  0x30CD5605, // Large 'vroun|dpd'.
  0x30D15605, // Large 'vroun|dps'.
  0x360A5605, // Large 'vroun|dsd'.
  0x10147605, // Large 'vrounds|s'.
  0x20878351, // Large 'vrsqrt14|pd'.
  0x20708351, // Large 'vrsqrt14|ps'.
  0x210E8351, // Large 'vrsqrt14|sd'.
  0x201C8351, // Large 'vrsqrt14|ss'.
  0x43596351, // Large 'vrsqrt|28pd'.
  0x435D6351, // Large 'vrsqrt|28ps'.
  0x43616351, // Large 'vrsqrt|28sd'.
  0x43656351, // Large 'vrsqrt|28ss'.
  0x20826351, // Large 'vrsqrt|ph'.
  0x20706351, // Large 'vrsqrt|ps'.
  0x20B56351, // Large 'vrsqrt|sh'.
  0x201C6351, // Large 'vrsqrt|ss'.
  0x208774DC, // Large 'vscalef|pd'.
  0x208274DC, // Large 'vscalef|ph'.
  0x207074DC, // Large 'vscalef|ps'.
  0x210E74DC, // Large 'vscalef|sd'.
  0x20B574DC, // Large 'vscalef|sh'.
  0x201C74DC, // Large 'vscalef|ss'.
  0x30CD80BA, // Large 'vscatter|dpd'.
  0x30D180BA, // Large 'vscatter|dps'.
  0x2087C0BA, // Large 'vscatterpf0d|pd'.
  0x2070C0BA, // Large 'vscatterpf0d|ps'.
  0x30C6B0BA, // Large 'vscatterpf0|qpd'.
  0x30C9B0BA, // Large 'vscatterpf0|qps'.
  0x40CCA0BA, // Large 'vscatterpf|1dpd'.
  0x40D0A0BA, // Large 'vscatterpf|1dps'.
  0x40D4A0BA, // Large 'vscatterpf|1qpd'.
  0x40D8A0BA, // Large 'vscatterpf|1qps'.
  0x30C680BA, // Large 'vscatter|qpd'.
  0x30C980BA, // Large 'vscatter|qps'.
  0x502B5369, // Large 'vshuf|f32x4'.
  0x4030636E, // Large 'vshuff|64x2'.
  0x503D5369, // Large 'vshuf|i32x4'.
  0x50475369, // Large 'vshuf|i64x2'.
  0x20875369, // Large 'vshuf|pd'.
  0x20705369, // Large 'vshuf|ps'.
  0x31A9472C, // Large 'vsqr|tpd'.
  0x31B4472C, // Large 'vsqr|tph'.
  0x31BD472C, // Large 'vsqr|tps'.
  0x310D472C, // Large 'vsqr|tsd'.
  0x31CF472C, // Large 'vsqr|tsh'.
  0x31D8472C, // Large 'vsqr|tss'.
  0x1023760D, // Large 'vstmxcs|r'.
  0x89015676, // Small 'vsubpd'.
  0x91015676, // Small 'vsubph'.
  0xA7015676, // Small 'vsubps'.
  0x89315676, // Small 'vsubsd'.
  0x91315676, // Small 'vsubsh'.
  0xA7315676, // Small 'vsubss'.
  0x31A94730, // Large 'vtes|tpd'.
  0x31BD4730, // Large 'vtes|tps'.
  0x210E6614, // Large 'vucomi|sd'.
  0x20B56614, // Large 'vucomi|sh'.
  0x201C6614, // Large 'vucomi|ss'.
  0x208774E3, // Large 'vunpckh|pd'.
  0x207074E3, // Large 'vunpckh|ps'.
  0x34EA64E3, // Large 'vunpck|lpd'.
  0x34ED64E3, // Large 'vunpck|lps'.
  0x89093F16, // Small 'vxorpd'.
  0xA7093F16, // Small 'vxorps'.
  0x361A5374, // Large 'vzero|all'.
  0x33077374, // Large 'vzeroup|per'.
  0x89672457, // Small 'wbinvd'.
  0x21FF661D, // Large 'wbnoin|vd'.
  0x33B05623, // Large 'wrfsb|ase'.
  0x33B05628, // Large 'wrgsb|ase'.
  0x8129B657, // Small 'wrmsr'.
  0x8049CE57, // Small 'wrssd'.
  0x8119CE57, // Small 'wrssq'.
  0x8939D657, // Small 'wrussd'.
  0xA339D657, // Small 'wrussq'.
  0xA9278838, // Small 'xabort'.
  0x80021038, // Small 'xadd'.
  0x9C939458, // Small 'xbegin'.
  0x8003A078, // Small 'xchg'.
  0x800238B8, // Small 'xend'.
  0xAC2A14F8, // Small 'xgetbv'.
  0x802A0598, // Small 'xlatb'.
  0x800049F8, // Small 'xor'.
  0x804849F8, // Small 'xorpd'.
  0x813849F8, // Small 'xorps'.
  0x101584F0, // Large 'xresldtr|k'.
  0xA4FA4E58, // Small 'xrstor'.
  0x20306386, // Large 'xrstor|64'.
  0x10146386, // Large 'xrstor|s'.
  0x34F86386, // Large 'xrstor|s64'.
  0x805B0678, // Small 'xsave'.
  0x2030537B, // Large 'xsave|64'.
  0x865B0678, // Small 'xsavec'.
  0x362D537B, // Large 'xsave|c64'.
  0x0000837B, // Large 'xsaveopt'.
  0x2030837B, // Large 'xsaveopt|64'.
  0xA65B0678, // Small 'xsaves'.
  0x34F8537B, // Large 'xsave|s64'.
  0xAC2A1678, // Small 'xsetbv'.
  0x101584FB, // Large 'xsusldtr|k'.
  0x81499698  // Small 'xtest'.
};

const char InstDB::_instNameStringTable[] =
  "vgf2p8affineinvqbvaeskeygenassistvbroadcastf32x464x264x4i32x2i32x4i32x8i64x2i64x"
  "4vpbroadcastmb2w2d128i128vcvtne2ps2bf1vfmaddsub132ph213pd213ph213ps231pd231ph231"
  "psvfmsubadd132vpmultishiftvscatterpf0dqpdqps1dpd1dps1qpd1qpsvcvtneps2bf1vextracv"
  "extractfvgatherpf0vp2intersectsdvfnmadd132213sd213sh213ss231sd231sh231ssvfnmsub1"
  "32vinservinsertfvpshufbitqprefetchntwt1saveprevsssha256rndtileloaddtilereleavaes"
  "declvaesenclvcompressvcvttpd2uqqvcvttph2uvcvttps2uvcvttsd2uvcvttsh2uvcvttss2uvfi"
  "xupimmvfmadd132vfmsub132vmaskmovdqvpcompressvpconflictvphminposuvpmadd52hluqvpsc"
  "atterqdvpunpckhqlqdqvrndscaleclflushopcmpxchg16t0t2msg1msg2tilestorev4fnmaddssvc"
  "vtpd2uvcvtph2psudqvcvtps2phvcvtsd2uvcvtsh2uvcvtss2uvcvtudq2vcvtuqq2vcvtusi2vfcma"
  "ddcvfpclassvgetmanmulbvp4dpwssvpclmuvpcmpestrvpcmpistrvperm2fvpermil2vpgathervpm"
  "acssdqvpmadcsswubswvpmaskmovpternlogbwwdlbwldqlwdvrsqrt1428pd28ps28sd28ssvshufvs"
  "huffvzeroupxsaveopt8bfxrstorldtilecfmovdir64pvalidarmpadjurmpupdaserialisha1nexs"
  "ha1rndssttilecftdpbf16v4fmadvaddsubvblendmvpdvcvtdq2uwvcvtqq2vcvtsi2vcvtuwvdbpsa"
  "dvdpbf16vexpanvfcmulccphcshvgetexpvmovdqau16u32u64vmovmskvmovntvmovshdvmovsldvpa"
  "ckssdwbvpackuswbvpblendmdvpdpbusvpdpwss2pdvpermtvpexpanvphaddubwqdqhvpmovmskvpmo"
  "vsxbvpmovusqwvpmovzxbvpmulhrvptestnmqvreducevscalefvunpckhlpdlpsxresldtrs64xsusl"
  "dtrcldemoclrssbscvtpifcmovnbfxsavekortestkshiftrbkunpckmonitorpfrcpipfrsqirtvrdf"
  "sbrdgsbsspsenduisetssbssysesysexvcvtwvfmulvldmxcsvmlaundupu8vmovhvmovlhvmpsadvmr"
  "esumvpadduvpaligngtbgtdgtqgtw2qbdbqvphsubvplzcnb2md2mq2mw2mvpopcnvpshldvqvpshrdv"
  "whwvpsubuvrangevrcp14vroundsdvstmxcsvucomiallwbnoinwrfsbwrgsbc64blcfiblsfiendbre"
  "nqcmbefdecsfincsfnstefrndfsincfucomppfyl2xincsspqinvlinvpcinvvpmcommmovq2pavgupf"
  "cmpepfpnaptwrisyscsysretdpbuvaesivaligvandnvcomivfrczvhadvhsubvmclevmmcvmovavmov"
  "uvmptvmwrivpandvpextrwvpinsvpmaxvpminvprolvprorvpsadvpsigvpslvpsllvpsravpsrlvsqr"
  "vtes";


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
  ROW(2, 1, 1, 0, 4  , 39 , 0  , 0  , 0  , 0  ), //      {r16, r16|m16|mem|i8|i16|u16}
  ROW(2, 1, 1, 0, 6  , 40 , 0  , 0  , 0  , 0  ), //      {r32, r32|m32|mem|i8|i32|u32}
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
  ROW(F(RegGpw) | F(MemUnspecified) | F(Mem16) | F(ImmI8) | F(ImmI16) | F(ImmU16), 0x00),
  ROW(F(RegGpd) | F(MemUnspecified) | F(Mem32) | F(ImmI8) | F(ImmI32) | F(ImmU32), 0x00),
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
  71, 71, 72, 72, 59, 59, 67, 59, 59, 71, 71, 73, 48, 52, 74, 75, 7, 7, 76, 77,
  9, 66, 66, 77, 0, 35, 4, 4, 5, 6, 0, 78, 0, 0, 79, 0, 2, 4, 4, 80, 81, 9, 9,
  9, 3, 3, 4, 3, 3, 3, 3, 3, 3, 3, 3, 3, 0, 3, 3, 0, 3, 82, 3, 0, 0, 0, 3, 3,
  4, 3, 0, 0, 3, 3, 4, 3, 0, 0, 0, 0, 0, 0, 0, 0, 83, 27, 27, 82, 82, 82, 82, 82,
  82, 82, 82, 82, 82, 27, 82, 82, 82, 27, 27, 82, 82, 82, 3, 3, 3, 84, 3, 3, 3,
  27, 27, 0, 0, 0, 0, 3, 3, 4, 4, 3, 3, 4, 4, 4, 4, 3, 3, 4, 4, 85, 86, 87, 24,
  24, 24, 86, 86, 87, 24, 24, 24, 86, 4, 3, 82, 3, 3, 4, 3, 3, 0, 0, 0, 9, 0,
  0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 3, 3, 0, 0, 0, 0, 3, 3, 3, 3, 88, 3, 3, 0, 3, 3,
  3, 88, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 27, 89, 0, 3, 3, 4, 3, 90, 90, 4, 90, 0,
  0, 0, 0, 0, 0, 0, 3, 91, 7, 92, 91, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 93, 0, 0,
  0, 0, 0, 91, 91, 0, 0, 0, 0, 0, 0, 7, 92, 0, 0, 91, 91, 0, 0, 2, 94, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 4, 4, 4, 0, 4, 4, 0, 91, 0, 0, 91, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 7, 7, 26, 92, 0, 0, 0, 0, 0, 0, 95, 0, 0, 0, 2, 4, 4, 5, 6, 0, 0, 0, 0, 0,
  0, 0, 9, 0, 0, 0, 0, 0, 15, 0, 96, 96, 0, 97, 0, 0, 9, 9, 20, 21, 98, 98, 0,
  0, 0, 0, 4, 4, 4, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 7, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 99, 28, 100, 101, 100, 101, 99, 28, 100, 101,
  100, 101, 102, 103, 0, 0, 0, 0, 0, 0, 20, 104, 21, 105, 105, 106, 77, 9, 0, 77,
  107, 108, 107, 9, 107, 9, 109, 110, 106, 109, 110, 109, 110, 9, 9, 9, 106,
  0, 77, 106, 9, 106, 9, 108, 107, 0, 28, 0, 28, 0, 111, 0, 111, 0, 0, 0, 0, 0,
  33, 33, 107, 9, 107, 9, 109, 110, 109, 110, 9, 9, 9, 106, 9, 106, 28, 28, 111,
  111, 33, 33, 106, 77, 9, 9, 108, 107, 0, 0, 0, 9, 9, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 112, 112, 9, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 9, 9, 27, 113, 60, 60,
  0, 0, 0, 0, 0, 0, 0, 0, 60, 114, 9, 9, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 67, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 115, 115, 47, 116, 115, 115, 115, 115, 115,
  115, 115, 115, 0, 117, 117, 0, 71, 71, 118, 119, 67, 67, 67, 67, 120, 71, 121,
  9, 9, 73, 115, 115, 49, 0, 0, 0, 105, 0, 0, 0, 0, 0, 0, 0, 0, 0, 122, 0, 0,
  0, 0, 0, 0, 0, 9, 9, 9, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 123, 33, 124, 124, 28, 125, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 105, 105, 105, 105, 0,
  0, 0, 0, 0, 0, 9, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 9, 9, 9, 9, 0, 0, 0, 0, 60, 60, 114, 60, 7, 7, 7,
  0, 7, 0, 7, 7, 7, 7, 7, 7, 0, 7, 7, 84, 7, 0, 7, 0, 0, 7, 0, 0, 0, 0, 9, 9, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 126, 126, 127, 128, 124, 124, 124, 124, 85, 126, 129, 128,
  127, 127, 128, 129, 128, 127, 128, 130, 131, 106, 106, 106, 130, 127, 128,
  129, 128, 127, 128, 126, 128, 130, 131, 106, 106, 106, 130, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 9, 9, 9, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 67, 67,
  132, 67, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 122, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 9, 9, 0, 0, 112, 112, 0, 0, 9, 9, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 9, 9, 0, 0, 112, 112, 0, 0, 9, 9, 0, 0, 0,
  0, 0, 0, 0, 0, 67, 67, 0, 0, 0, 0, 0, 0, 0, 0, 67, 132, 0, 0, 0, 0, 0, 0, 9,
  9, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 122, 122, 20, 104, 21, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 133, 134, 133, 134, 0, 135, 0, 136, 0, 0, 0, 2, 4, 4, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
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
  0, 61, 0, 0, 61, 0, 0, 0, 0, 0, 5, 62, 0, 0, 0, 0, 63, 0, 64, 20, 65, 20, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 66, 0, 0, 0, 0, 0, 0,
  6, 5, 5, 0, 0, 0, 0, 67, 68, 0, 0, 0, 0, 69, 70, 0, 3, 3, 71, 22, 72, 73, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 74, 39, 75, 76, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 77, 0, 0, 0, 0, 0, 0, 0, 10,
  10, 10, 10, 10, 10, 10, 0, 0, 2, 2, 2, 78, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 65, 0, 0, 0, 0, 0, 0, 0, 0, 65, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 79, 79, 80, 79, 80, 80, 80, 79, 79, 81, 82, 0, 83,
  0, 0, 0, 0, 0, 0, 84, 2, 2, 85, 86, 0, 0, 0, 11, 87, 0, 0, 4, 0, 0, 0, 88, 0,
  89, 89, 89, 89, 89, 89, 89, 89, 89, 89, 89, 89, 89, 89, 89, 89, 89, 89, 89, 89,
  89, 89, 89, 89, 89, 89, 89, 89, 89, 0, 89, 0, 32, 0, 0, 0, 5, 0, 0, 6, 0, 90,
  4, 0, 90, 4, 5, 5, 32, 19, 91, 79, 91, 0, 0, 0, 0, 0, 0, 0, 0, 0, 92, 0, 91,
  93, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 94, 94, 94, 94, 94, 0, 0, 0, 0,
  0, 0, 95, 96, 0, 0, 0, 0, 0, 0, 0, 0, 56, 96, 0, 0, 0, 0, 97, 98, 97, 98, 3,
  3, 3, 99, 100, 101, 3, 3, 3, 3, 3, 3, 0, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 102,
  102, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 103, 3, 104, 105, 106, 0, 0,
  0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 107,
  0, 0, 0, 0, 0, 0, 0, 108, 0, 109, 0, 110, 0, 110, 0, 111, 112, 113, 114, 115,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 111, 112, 113, 0, 0, 3, 3, 3, 3, 99, 110, 101, 3, 116, 3, 55, 55, 0,
  0, 0, 0, 117, 118, 119, 118, 119, 117, 118, 119, 118, 119, 22, 120, 121, 120,
  121, 120, 120, 122, 123, 120, 120, 120, 124, 125, 126, 120, 120, 120, 124, 125,
  126, 120, 120, 120, 124, 125, 126, 120, 121, 127, 127, 128, 129, 120, 120,
  120, 120, 120, 120, 120, 120, 120, 127, 127, 120, 120, 120, 124, 130, 126, 120,
  120, 120, 124, 130, 126, 120, 120, 120, 124, 130, 126, 120, 120, 120, 120, 120,
  120, 120, 120, 120, 127, 127, 127, 127, 128, 129, 120, 121, 120, 120, 120,
  124, 125, 126, 120, 120, 120, 124, 125, 126, 120, 120, 120, 124, 125, 126, 127,
  127, 128, 129, 120, 120, 120, 124, 130, 126, 120, 120, 120, 124, 130, 126,
  120, 120, 120, 131, 130, 132, 127, 127, 128, 129, 133, 133, 133, 78, 134, 135,
  0, 0, 0, 0, 136, 137, 10, 10, 10, 10, 10, 10, 10, 10, 137, 138, 0, 0, 0, 139,
  140, 141, 84, 84, 84, 139, 140, 141, 3, 3, 3, 3, 3, 3, 3, 142, 143, 144, 143,
  144, 142, 143, 144, 143, 144, 101, 0, 53, 58, 145, 145, 3, 3, 3, 99, 100, 101,
  0, 146, 0, 3, 3, 3, 99, 100, 101, 0, 147, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 148, 149, 149, 150, 151, 151, 0, 0, 0, 0, 0, 0, 0, 152, 153, 0, 0, 154, 0,
  0, 0, 3, 11, 146, 0, 0, 155, 147, 3, 3, 3, 99, 100, 101, 0, 11, 3, 3, 156, 156,
  157, 157, 0, 0, 0, 0, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
  3, 3, 3, 3, 3, 3, 3, 3, 102, 3, 0, 0, 0, 0, 0, 0, 3, 127, 103, 103, 3, 3, 3,
  3, 67, 68, 3, 3, 3, 3, 69, 70, 103, 103, 103, 103, 103, 103, 116, 116, 0, 0,
  0, 0, 116, 116, 116, 116, 116, 116, 0, 0, 120, 120, 120, 120, 158, 158, 3, 3,
  3, 120, 3, 3, 120, 120, 127, 127, 159, 159, 159, 3, 159, 3, 120, 120, 120, 120,
  120, 3, 0, 0, 0, 0, 71, 22, 72, 160, 137, 136, 138, 137, 0, 0, 0, 3, 0, 3, 0,
  0, 0, 0, 0, 0, 3, 0, 0, 0, 0, 3, 0, 3, 3, 0, 161, 101, 99, 100, 0, 0, 162, 162,
  162, 162, 162, 162, 162, 162, 162, 162, 162, 162, 120, 120, 3, 3, 145, 145,
  3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 3, 3, 3, 3, 3, 3, 3, 3, 3, 0, 0, 0, 0, 3, 3, 3, 163, 84, 84, 3, 3,
  84, 84, 3, 3, 164, 164, 164, 164, 3, 0, 0, 0, 0, 164, 164, 164, 164, 164, 164,
  3, 3, 120, 120, 120, 3, 164, 164, 3, 3, 120, 120, 120, 3, 3, 103, 84, 84, 84,
  3, 3, 3, 165, 166, 165, 3, 3, 3, 167, 165, 168, 3, 3, 3, 167, 165, 166, 165,
  3, 3, 3, 167, 3, 3, 3, 3, 3, 3, 3, 3, 120, 120, 0, 103, 103, 103, 103, 103, 103,
  103, 103, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 139, 141, 0, 0, 139, 141,
  0, 0, 139, 141, 0, 0, 140, 141, 84, 84, 84, 139, 140, 141, 84, 84, 84, 139, 140,
  141, 84, 84, 139, 141, 0, 0, 139, 141, 0, 0, 139, 141, 0, 0, 140, 141, 3, 3,
  3, 99, 100, 101, 0, 0, 10, 10, 10, 10, 10, 10, 10, 10, 0, 0, 3, 3, 3, 3, 3,
  3, 0, 0, 0, 139, 140, 141, 92, 3, 3, 3, 99, 100, 101, 0, 0, 0, 0, 0, 3, 3, 3,
  3, 3, 3, 0, 0, 0, 0, 56, 56, 169, 0, 0, 0, 0, 0, 0, 0, 0, 0, 80, 0, 0, 0, 0, 0,
  170, 170, 170, 170, 171, 171, 171, 171, 171, 171, 171, 171, 169, 0, 0
};

const InstDB::RWInfo InstDB::rwInfoA[] = {
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 0 , 0 , 0 , 0 , 0 , 0  } }, // #0 [ref=1007x]
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
  { InstDB::RWInfo::kCategoryGeneric   , 24, { 44, 9 , 0 , 0 , 0 , 0  } }, // #47 [ref=3x]
  { InstDB::RWInfo::kCategoryGeneric   , 25, { 35, 7 , 0 , 0 , 0 , 0  } }, // #48 [ref=2x]
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
  { InstDB::RWInfo::kCategoryGeneric   , 31, { 35, 7 , 0 , 0 , 0 , 0  } }, // #75 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 33, { 44, 9 , 0 , 0 , 0 , 0  } }, // #76 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 14, { 11, 3 , 0 , 0 , 0 , 0  } }, // #77 [ref=6x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 17, 29, 0 , 0 , 0 , 0  } }, // #78 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 11, { 3 , 3 , 0 , 0 , 0 , 0  } }, // #79 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 52, 22, 0 , 0 , 0 , 0  } }, // #80 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 52, 66, 0 , 0 , 0 , 0  } }, // #81 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 4 , { 26, 7 , 0 , 0 , 0 , 0  } }, // #82 [ref=18x]
  { InstDB::RWInfo::kCategoryGeneric   , 36, { 0 , 0 , 0 , 0 , 0 , 0  } }, // #83 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 3 , { 69, 5 , 0 , 0 , 0 , 0  } }, // #84 [ref=2x]
  { InstDB::RWInfo::kCategoryVmov1_8   , 0 , { 0 , 0 , 0 , 0 , 0 , 0  } }, // #85 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 5 , { 10, 9 , 0 , 0 , 0 , 0  } }, // #86 [ref=4x]
  { InstDB::RWInfo::kCategoryGeneric   , 27, { 10, 13, 0 , 0 , 0 , 0  } }, // #87 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 4 , 0 , 0 , 0 , 0 , 0  } }, // #88 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 3 , { 5 , 5 , 0 , 0 , 0 , 0  } }, // #89 [ref=1x]
  { InstDB::RWInfo::kCategoryPunpcklxx , 38, { 0 , 0 , 0 , 0 , 0 , 0  } }, // #90 [ref=3x]
  { InstDB::RWInfo::kCategoryGeneric   , 10, { 2 , 70, 0 , 0 , 0 , 0  } }, // #91 [ref=8x]
  { InstDB::RWInfo::kCategoryGeneric   , 5 , { 37, 9 , 0 , 0 , 0 , 0  } }, // #92 [ref=3x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 16, 50, 0 , 0 , 0 , 0  } }, // #93 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 22, 21, 0 , 0 , 0 , 0  } }, // #94 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 63, 22, 0 , 0 , 0 , 0  } }, // #95 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 8 , { 73, 3 , 0 , 0 , 0 , 0  } }, // #96 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 8 , { 11, 43, 0 , 0 , 0 , 0  } }, // #97 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 5 , { 53, 9 , 0 , 0 , 0 , 0  } }, // #98 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 13, { 79, 5 , 0 , 0 , 0 , 0  } }, // #99 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 13, { 11, 5 , 0 , 0 , 0 , 0  } }, // #100 [ref=4x]
  { InstDB::RWInfo::kCategoryGeneric   , 43, { 73, 80, 0 , 0 , 0 , 0  } }, // #101 [ref=4x]
  { InstDB::RWInfo::kCategoryGeneric   , 44, { 11, 7 , 0 , 0 , 0 , 0  } }, // #102 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 45, { 11, 9 , 0 , 0 , 0 , 0  } }, // #103 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 27, { 13, 13, 0 , 0 , 0 , 0  } }, // #104 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 11, { 11, 3 , 0 , 0 , 0 , 0  } }, // #105 [ref=7x]
  { InstDB::RWInfo::kCategoryVmov2_1   , 46, { 0 , 0 , 0 , 0 , 0 , 0  } }, // #106 [ref=14x]
  { InstDB::RWInfo::kCategoryVmov1_2   , 14, { 0 , 0 , 0 , 0 , 0 , 0  } }, // #107 [ref=7x]
  { InstDB::RWInfo::kCategoryGeneric   , 14, { 10, 3 , 0 , 0 , 0 , 0  } }, // #108 [ref=3x]
  { InstDB::RWInfo::kCategoryGeneric   , 46, { 11, 3 , 0 , 0 , 0 , 0  } }, // #109 [ref=5x]
  { InstDB::RWInfo::kCategoryGeneric   , 47, { 11, 5 , 0 , 0 , 0 , 0  } }, // #110 [ref=5x]
  { InstDB::RWInfo::kCategoryGeneric   , 27, { 11, 5 , 0 , 0 , 0 , 0  } }, // #111 [ref=4x]
  { InstDB::RWInfo::kCategoryGeneric   , 51, { 73, 43, 0 , 0 , 0 , 0  } }, // #112 [ref=6x]
  { InstDB::RWInfo::kCategoryGeneric   , 5 , { 44, 9 , 0 , 0 , 0 , 0  } }, // #113 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 18, { 2 , 3 , 0 , 0 , 0 , 0  } }, // #114 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 58, { 11, 3 , 0 , 0 , 0 , 0  } }, // #115 [ref=12x]
  { InstDB::RWInfo::kCategoryVmovddup  , 38, { 0 , 0 , 0 , 0 , 0 , 0  } }, // #116 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 12, { 35, 61, 0 , 0 , 0 , 0  } }, // #117 [ref=2x]
  { InstDB::RWInfo::kCategoryVmovmskpd , 0 , { 0 , 0 , 0 , 0 , 0 , 0  } }, // #118 [ref=1x]
  { InstDB::RWInfo::kCategoryVmovmskps , 0 , { 0 , 0 , 0 , 0 , 0 , 0  } }, // #119 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 59, { 35, 7 , 0 , 0 , 0 , 0  } }, // #120 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 21, { 48, 13, 0 , 0 , 0 , 0  } }, // #121 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 2 , { 3 , 3 , 0 , 0 , 0 , 0  } }, // #122 [ref=4x]
  { InstDB::RWInfo::kCategoryGeneric   , 15, { 11, 40, 0 , 0 , 0 , 0  } }, // #123 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 11, 7 , 0 , 0 , 0 , 0  } }, // #124 [ref=6x]
  { InstDB::RWInfo::kCategoryGeneric   , 27, { 11, 13, 0 , 0 , 0 , 0  } }, // #125 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 35, 3 , 0 , 0 , 0 , 0  } }, // #126 [ref=4x]
  { InstDB::RWInfo::kCategoryVmov1_4   , 62, { 0 , 0 , 0 , 0 , 0 , 0  } }, // #127 [ref=6x]
  { InstDB::RWInfo::kCategoryVmov1_2   , 48, { 0 , 0 , 0 , 0 , 0 , 0  } }, // #128 [ref=9x]
  { InstDB::RWInfo::kCategoryVmov1_8   , 63, { 0 , 0 , 0 , 0 , 0 , 0  } }, // #129 [ref=3x]
  { InstDB::RWInfo::kCategoryVmov4_1   , 47, { 0 , 0 , 0 , 0 , 0 , 0  } }, // #130 [ref=4x]
  { InstDB::RWInfo::kCategoryVmov8_1   , 64, { 0 , 0 , 0 , 0 , 0 , 0  } }, // #131 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 18, { 11, 3 , 0 , 0 , 0 , 0  } }, // #132 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 17, { 44, 9 , 0 , 0 , 0 , 0  } }, // #133 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 35, { 35, 7 , 0 , 0 , 0 , 0  } }, // #134 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 11, { 2 , 2 , 0 , 0 , 0 , 0  } }, // #135 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 58, { 2 , 2 , 0 , 0 , 0 , 0  } }  // #136 [ref=1x]
};

const InstDB::RWInfo InstDB::rwInfoB[] = {
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 0 , 0 , 0 , 0 , 0 , 0  } }, // #0 [ref=773x]
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
  { InstDB::RWInfo::kCategoryGeneric   , 32, { 0 , 0 , 0 , 0 , 0 , 0  } }, // #61 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 6 , { 64, 42, 3 , 0 , 0 , 0  } }, // #62 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 6 , { 11, 11, 3 , 65, 0 , 0  } }, // #63 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 17, 29, 30, 0 , 0 , 0  } }, // #64 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 10, { 3 , 0 , 0 , 0 , 0 , 0  } }, // #65 [ref=3x]
  { InstDB::RWInfo::kCategoryGeneric   , 2 , { 2 , 3 , 0 , 0 , 0 , 0  } }, // #66 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 3 , { 5 , 5 , 0 , 67, 17, 60 } }, // #67 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 3 , { 5 , 5 , 0 , 68, 17, 60 } }, // #68 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 3 , { 5 , 5 , 0 , 67, 0 , 0  } }, // #69 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 3 , { 5 , 5 , 0 , 68, 0 , 0  } }, // #70 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 34, { 56, 5 , 0 , 0 , 0 , 0  } }, // #71 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 35, { 35, 5 , 0 , 0 , 0 , 0  } }, // #72 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 37, { 48, 3 , 0 , 0 , 0 , 0  } }, // #73 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 15, { 4 , 40, 0 , 0 , 0 , 0  } }, // #74 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 4 , { 4 , 7 , 0 , 0 , 0 , 0  } }, // #75 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 27, { 2 , 13, 0 , 0 , 0 , 0  } }, // #76 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 10, { 11, 0 , 0 , 0 , 0 , 0  } }, // #77 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 4 , { 35, 7 , 0 , 0 , 0 , 0  } }, // #78 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 11, 0 , 0 , 0 , 0 , 0  } }, // #79 [ref=6x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 16, 50, 29, 0 , 0 , 0  } }, // #80 [ref=5x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 44, 0 , 0 , 0 , 0 , 0  } }, // #81 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 35, 0 , 0 , 0 , 0 , 0  } }, // #82 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 16, 50, 67, 0 , 0 , 0  } }, // #83 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 2 , { 11, 3 , 0 , 0 , 0 , 0  } }, // #84 [ref=19x]
  { InstDB::RWInfo::kCategoryGeneric   , 4 , { 36, 7 , 0 , 0 , 0 , 0  } }, // #85 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 5 , { 37, 9 , 0 , 0 , 0 , 0  } }, // #86 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 71, 0 , 0 , 0 , 0 , 0  } }, // #87 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 7 , 0 , 0 , 0 , 0 , 0  } }, // #88 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 34, { 72, 0 , 0 , 0 , 0 , 0  } }, // #89 [ref=30x]
  { InstDB::RWInfo::kCategoryGeneric   , 11, { 2 , 3 , 70, 0 , 0 , 0  } }, // #90 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 39, { 11, 0 , 0 , 0 , 0 , 0  } }, // #91 [ref=3x]
  { InstDB::RWInfo::kCategoryGeneric   , 28, { 44, 0 , 0 , 0 , 0 , 0  } }, // #92 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 16, { 73, 0 , 0 , 0 , 0 , 0  } }, // #93 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 74, 43, 43, 0 , 0 , 0  } }, // #94 [ref=5x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 73, 0 , 0 , 0 , 0 , 0  } }, // #95 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 9 , 60, 17, 0 , 0 , 0  } }, // #96 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 13, { 74, 75, 76, 76, 76, 5  } }, // #97 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 13, { 4 , 77, 78, 78, 78, 5  } }, // #98 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 40, { 10, 5 , 7 , 0 , 0 , 0  } }, // #99 [ref=8x]
  { InstDB::RWInfo::kCategoryGeneric   , 41, { 10, 5 , 13, 0 , 0 , 0  } }, // #100 [ref=7x]
  { InstDB::RWInfo::kCategoryGeneric   , 42, { 10, 5 , 9 , 0 , 0 , 0  } }, // #101 [ref=9x]
  { InstDB::RWInfo::kCategoryGeneric   , 6 , { 11, 3 , 3 , 3 , 0 , 0  } }, // #102 [ref=3x]
  { InstDB::RWInfo::kCategoryGeneric   , 6 , { 35, 3 , 3 , 0 , 0 , 0  } }, // #103 [ref=18x]
  { InstDB::RWInfo::kCategoryGeneric   , 40, { 11, 5 , 7 , 0 , 0 , 0  } }, // #104 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 41, { 35, 13, 13, 0 , 0 , 0  } }, // #105 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 42, { 11, 5 , 9 , 0 , 0 , 0  } }, // #106 [ref=1x]
  { InstDB::RWInfo::kCategoryVmov1_2   , 48, { 0 , 0 , 0 , 0 , 0 , 0  } }, // #107 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 40, { 10, 5 , 5 , 0 , 0 , 0  } }, // #108 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 40, { 10, 81, 7 , 0 , 0 , 0  } }, // #109 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 41, { 10, 5 , 5 , 0 , 0 , 0  } }, // #110 [ref=3x]
  { InstDB::RWInfo::kCategoryGeneric   , 49, { 10, 61, 3 , 0 , 0 , 0  } }, // #111 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 49, { 10, 3 , 3 , 0 , 0 , 0  } }, // #112 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 49, { 10, 81, 3 , 0 , 0 , 0  } }, // #113 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 42, { 10, 61, 9 , 0 , 0 , 0  } }, // #114 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 42, { 10, 5 , 5 , 0 , 0 , 0  } }, // #115 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 50, { 10, 5 , 5 , 0 , 0 , 0  } }, // #116 [ref=9x]
  { InstDB::RWInfo::kCategoryGeneric   , 52, { 10, 80, 0 , 0 , 0 , 0  } }, // #117 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 52, { 10, 3 , 0 , 0 , 0 , 0  } }, // #118 [ref=4x]
  { InstDB::RWInfo::kCategoryGeneric   , 53, { 79, 43, 0 , 0 , 0 , 0  } }, // #119 [ref=4x]
  { InstDB::RWInfo::kCategoryGeneric   , 6 , { 2 , 3 , 3 , 0 , 0 , 0  } }, // #120 [ref=82x]
  { InstDB::RWInfo::kCategoryGeneric   , 42, { 4 , 5 , 5 , 0 , 0 , 0  } }, // #121 [ref=4x]
  { InstDB::RWInfo::kCategoryGeneric   , 40, { 4 , 61, 7 , 0 , 0 , 0  } }, // #122 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 42, { 4 , 81, 9 , 0 , 0 , 0  } }, // #123 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 40, { 6 , 7 , 7 , 0 , 0 , 0  } }, // #124 [ref=11x]
  { InstDB::RWInfo::kCategoryGeneric   , 41, { 4 , 5 , 5 , 0 , 0 , 0  } }, // #125 [ref=6x]
  { InstDB::RWInfo::kCategoryGeneric   , 42, { 8 , 9 , 9 , 0 , 0 , 0  } }, // #126 [ref=11x]
  { InstDB::RWInfo::kCategoryGeneric   , 54, { 11, 3 , 3 , 3 , 0 , 0  } }, // #127 [ref=15x]
  { InstDB::RWInfo::kCategoryGeneric   , 55, { 35, 7 , 7 , 7 , 0 , 0  } }, // #128 [ref=4x]
  { InstDB::RWInfo::kCategoryGeneric   , 56, { 44, 9 , 9 , 9 , 0 , 0  } }, // #129 [ref=4x]
  { InstDB::RWInfo::kCategoryGeneric   , 41, { 4 , 5 , 13, 0 , 0 , 0  } }, // #130 [ref=6x]
  { InstDB::RWInfo::kCategoryGeneric   , 40, { 26, 7 , 7 , 0 , 0 , 0  } }, // #131 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 42, { 53, 9 , 9 , 0 , 0 , 0  } }, // #132 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 14, { 35, 3 , 0 , 0 , 0 , 0  } }, // #133 [ref=3x]
  { InstDB::RWInfo::kCategoryGeneric   , 27, { 35, 13, 0 , 0 , 0 , 0  } }, // #134 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 5 , { 35, 9 , 0 , 0 , 0 , 0  } }, // #135 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 8 , { 2 , 3 , 2 , 0 , 0 , 0  } }, // #136 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 2 , 3 , 2 , 0 , 0 , 0  } }, // #137 [ref=4x]
  { InstDB::RWInfo::kCategoryGeneric   , 18, { 4 , 3 , 4 , 0 , 0 , 0  } }, // #138 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 40, { 10, 61, 7 , 0 , 0 , 0  } }, // #139 [ref=11x]
  { InstDB::RWInfo::kCategoryGeneric   , 41, { 10, 82, 13, 0 , 0 , 0  } }, // #140 [ref=7x]
  { InstDB::RWInfo::kCategoryGeneric   , 42, { 10, 81, 9 , 0 , 0 , 0  } }, // #141 [ref=13x]
  { InstDB::RWInfo::kCategoryGeneric   , 50, { 79, 80, 5 , 0 , 0 , 0  } }, // #142 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 50, { 11, 3 , 5 , 0 , 0 , 0  } }, // #143 [ref=4x]
  { InstDB::RWInfo::kCategoryGeneric   , 57, { 73, 43, 80, 0 , 0 , 0  } }, // #144 [ref=4x]
  { InstDB::RWInfo::kCategoryVmaskmov  , 0 , { 0 , 0 , 0 , 0 , 0 , 0  } }, // #145 [ref=4x]
  { InstDB::RWInfo::kCategoryGeneric   , 12, { 35, 0 , 0 , 0 , 0 , 0  } }, // #146 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 22, 0 , 0 , 0 , 0 , 0  } }, // #147 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 10, 61, 61, 0 , 0 , 0  } }, // #148 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 12, { 10, 7 , 7 , 0 , 0 , 0  } }, // #149 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 10, 7 , 7 , 0 , 0 , 0  } }, // #150 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 12, { 10, 61, 7 , 0 , 0 , 0  } }, // #151 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 10, 61, 7 , 0 , 0 , 0  } }, // #152 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 10, 82, 13, 0 , 0 , 0  } }, // #153 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 10, 81, 9 , 0 , 0 , 0  } }, // #154 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 83, 0 , 0 , 0 , 0 , 0  } }, // #155 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 60, { 84, 85, 3 , 3 , 0 , 0  } }, // #156 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 13, { 73, 75, 76, 76, 76, 5  } }, // #157 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 57, { 79, 80, 80, 0 , 0 , 0  } }, // #158 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 22, { 11, 3 , 3 , 0 , 0 , 0  } }, // #159 [ref=4x]
  { InstDB::RWInfo::kCategoryGeneric   , 7 , { 48, 5 , 0 , 0 , 0 , 0  } }, // #160 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 61, { 10, 5 , 40, 0 , 0 , 0  } }, // #161 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 50, { 10, 5 , 5 , 5 , 0 , 0  } }, // #162 [ref=12x]
  { InstDB::RWInfo::kCategoryGeneric   , 65, { 10, 5 , 5 , 5 , 0 , 0  } }, // #163 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 66, { 10, 5 , 5 , 0 , 0 , 0  } }, // #164 [ref=12x]
  { InstDB::RWInfo::kCategoryGeneric   , 67, { 11, 3 , 5 , 0 , 0 , 0  } }, // #165 [ref=5x]
  { InstDB::RWInfo::kCategoryGeneric   , 68, { 11, 3 , 0 , 0 , 0 , 0  } }, // #166 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 69, { 11, 3 , 5 , 0 , 0 , 0  } }, // #167 [ref=3x]
  { InstDB::RWInfo::kCategoryGeneric   , 22, { 11, 3 , 5 , 0 , 0 , 0  } }, // #168 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 60, 17, 29, 0 , 0 , 0  } }, // #169 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 8 , { 3 , 60, 17, 0 , 0 , 0  } }, // #170 [ref=4x]
  { InstDB::RWInfo::kCategoryGeneric   , 8 , { 11, 60, 17, 0 , 0 , 0  } }  // #171 [ref=8x]
};

const InstDB::RWInfoOp InstDB::rwInfoOp[] = {
  { 0x0000000000000000u, 0x0000000000000000u, 0xFF, 0, { 0 }, OpRWFlags::kNone }, // #0 [ref=16519x]
  { 0x0000000000000003u, 0x0000000000000003u, 0x00, 0, { 0 }, OpRWFlags::kRW | OpRWFlags::kRegPhysId }, // #1 [ref=10x]
  { 0x0000000000000000u, 0x0000000000000000u, 0xFF, 0, { 0 }, OpRWFlags::kRW | OpRWFlags::kZExt }, // #2 [ref=236x]
  { 0x0000000000000000u, 0x0000000000000000u, 0xFF, 0, { 0 }, OpRWFlags::kRead }, // #3 [ref=1078x]
  { 0x000000000000FFFFu, 0x000000000000FFFFu, 0xFF, 0, { 0 }, OpRWFlags::kRW | OpRWFlags::kZExt }, // #4 [ref=108x]
  { 0x000000000000FFFFu, 0x0000000000000000u, 0xFF, 0, { 0 }, OpRWFlags::kRead }, // #5 [ref=348x]
  { 0x00000000000000FFu, 0x00000000000000FFu, 0xFF, 0, { 0 }, OpRWFlags::kRW }, // #6 [ref=18x]
  { 0x00000000000000FFu, 0x0000000000000000u, 0xFF, 0, { 0 }, OpRWFlags::kRead }, // #7 [ref=186x]
  { 0x000000000000000Fu, 0x000000000000000Fu, 0xFF, 0, { 0 }, OpRWFlags::kRW }, // #8 [ref=18x]
  { 0x000000000000000Fu, 0x0000000000000000u, 0xFF, 0, { 0 }, OpRWFlags::kRead }, // #9 [ref=135x]
  { 0x0000000000000000u, 0x000000000000FFFFu, 0xFF, 0, { 0 }, OpRWFlags::kWrite | OpRWFlags::kZExt }, // #10 [ref=184x]
  { 0x0000000000000000u, 0x0000000000000000u, 0xFF, 0, { 0 }, OpRWFlags::kWrite | OpRWFlags::kZExt }, // #11 [ref=456x]
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
  { 0x0000000000000000u, 0x0000000000000000u, 0x02, 0, { 0 }, OpRWFlags::kRead | OpRWFlags::kRegPhysId }, // #65 [ref=1x]
  { 0x0000000000000000u, 0x0000000000000000u, 0x06, 0, { 0 }, OpRWFlags::kRead | OpRWFlags::kMemPhysId }, // #66 [ref=1x]
  { 0x0000000000000000u, 0x000000000000000Fu, 0x01, 0, { 0 }, OpRWFlags::kWrite | OpRWFlags::kZExt | OpRWFlags::kRegPhysId }, // #67 [ref=5x]
  { 0x0000000000000000u, 0x000000000000FFFFu, 0x00, 0, { 0 }, OpRWFlags::kWrite | OpRWFlags::kZExt | OpRWFlags::kRegPhysId }, // #68 [ref=4x]
  { 0x0000000000000000u, 0x0000000000000007u, 0xFF, 0, { 0 }, OpRWFlags::kWrite | OpRWFlags::kZExt }, // #69 [ref=2x]
  { 0x0000000000000001u, 0x0000000000000000u, 0x01, 0, { 0 }, OpRWFlags::kRead | OpRWFlags::kRegPhysId }, // #70 [ref=10x]
  { 0x0000000000000001u, 0x0000000000000000u, 0x00, 0, { 0 }, OpRWFlags::kRead | OpRWFlags::kRegPhysId }, // #71 [ref=1x]
  { 0x0000000000000000u, 0x0000000000000001u, 0xFF, 0, { 0 }, OpRWFlags::kWrite }, // #72 [ref=30x]
  { 0x0000000000000000u, 0xFFFFFFFFFFFFFFFFu, 0xFF, 0, { 0 }, OpRWFlags::kWrite | OpRWFlags::kZExt }, // #73 [ref=20x]
  { 0xFFFFFFFFFFFFFFFFu, 0xFFFFFFFFFFFFFFFFu, 0xFF, 0, { 0 }, OpRWFlags::kRW | OpRWFlags::kZExt }, // #74 [ref=7x]
  { 0xFFFFFFFFFFFFFFFFu, 0x0000000000000000u, 0xFF, 4, { 0 }, OpRWFlags::kRead }, // #75 [ref=4x]
  { 0xFFFFFFFFFFFFFFFFu, 0x0000000000000000u, 0xFF, 0, { 0 }, OpRWFlags::kRead | OpRWFlags::kConsecutive }, // #76 [ref=12x]
  { 0x000000000000FFFFu, 0x0000000000000000u, 0xFF, 4, { 0 }, OpRWFlags::kRead }, // #77 [ref=2x]
  { 0x000000000000FFFFu, 0x0000000000000000u, 0xFF, 0, { 0 }, OpRWFlags::kRead | OpRWFlags::kConsecutive }, // #78 [ref=6x]
  { 0x0000000000000000u, 0x00000000FFFFFFFFu, 0xFF, 0, { 0 }, OpRWFlags::kWrite | OpRWFlags::kZExt }, // #79 [ref=10x]
  { 0x00000000FFFFFFFFu, 0x0000000000000000u, 0xFF, 0, { 0 }, OpRWFlags::kRead }, // #80 [ref=16x]
  { 0x000000000000FFF0u, 0x0000000000000000u, 0xFF, 0, { 0 }, OpRWFlags::kRead }, // #81 [ref=18x]
  { 0x000000000000FFFCu, 0x0000000000000000u, 0xFF, 0, { 0 }, OpRWFlags::kRead }, // #82 [ref=8x]
  { 0x0000000000000000u, 0x0000000000000000u, 0x00, 0, { 0 }, OpRWFlags::kRW | OpRWFlags::kZExt | OpRWFlags::kRegPhysId }, // #83 [ref=1x]
  { 0x0000000000000000u, 0x00000000000000FFu, 0xFF, 2, { 0 }, OpRWFlags::kWrite | OpRWFlags::kZExt }, // #84 [ref=2x]
  { 0x0000000000000000u, 0x0000000000000000u, 0xFF, 0, { 0 }, OpRWFlags::kWrite | OpRWFlags::kZExt | OpRWFlags::kConsecutive }  // #85 [ref=2x]
};

const InstDB::RWInfoRm InstDB::rwInfoRm[] = {
  { InstDB::RWInfoRm::kCategoryNone      , 0x00, 0 , 0, 0 }, // #0 [ref=1997x]
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
  { InstDB::RWInfoRm::kCategoryConsistent, 0x06, 0 , 0, 0 }, // #22 [ref=6x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x03, 1 , 0, 0 }, // #23 [ref=1x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x03, 4 , 0, 0 }, // #24 [ref=3x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x03, 8 , 0, 0 }, // #25 [ref=2x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x03, 2 , 0, 0 }, // #26 [ref=2x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x02, 2 , 0, 0 }, // #27 [ref=13x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x00, 4 , 0, 0 }, // #28 [ref=6x]
  { InstDB::RWInfoRm::kCategoryNone      , 0x03, 0 , InstDB::RWInfoRm::kFlagAmbiguous, 0 }, // #29 [ref=1x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x03, 16, 0, 0 }, // #30 [ref=6x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x03, 8 , InstDB::RWInfoRm::kFlagMovssMovsd, 0 }, // #31 [ref=1x]
  { InstDB::RWInfoRm::kCategoryNone      , 0x00, 0 , InstDB::RWInfoRm::kFlagMovssMovsd, 0 }, // #32 [ref=2x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x03, 4 , InstDB::RWInfoRm::kFlagMovssMovsd, 0 }, // #33 [ref=1x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x01, 1 , 0, 0 }, // #34 [ref=32x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x01, 8 , 0, 0 }, // #35 [ref=4x]
  { InstDB::RWInfoRm::kCategoryNone      , 0x00, 0 , InstDB::RWInfoRm::kFlagPextrw, 0 }, // #36 [ref=1x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x01, 2 , InstDB::RWInfoRm::kFlagPextrw, uint32_t(CpuFeatures::X86::kSSE4_1) }, // #37 [ref=1x]
  { InstDB::RWInfoRm::kCategoryNone      , 0x02, 0 , 0, 0 }, // #38 [ref=4x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x01, 2 , InstDB::RWInfoRm::kFlagAmbiguous, 0 }, // #39 [ref=3x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x04, 8 , 0, 0 }, // #40 [ref=35x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x04, 2 , 0, 0 }, // #41 [ref=30x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x04, 4 , 0, 0 }, // #42 [ref=42x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x00, 32, 0, 0 }, // #43 [ref=4x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x02, 8 , InstDB::RWInfoRm::kFlagAmbiguous, 0 }, // #44 [ref=1x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x02, 4 , InstDB::RWInfoRm::kFlagAmbiguous, 0 }, // #45 [ref=1x]
  { InstDB::RWInfoRm::kCategoryHalf      , 0x02, 0 , 0, 0 }, // #46 [ref=19x]
  { InstDB::RWInfoRm::kCategoryQuarter   , 0x02, 0 , 0, 0 }, // #47 [ref=9x]
  { InstDB::RWInfoRm::kCategoryHalf      , 0x01, 0 , 0, 0 }, // #48 [ref=10x]
  { InstDB::RWInfoRm::kCategoryConsistent, 0x04, 0 , InstDB::RWInfoRm::kFlagAmbiguous, 0 }, // #49 [ref=6x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x04, 16, 0, 0 }, // #50 [ref=27x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x02, 64, 0, 0 }, // #51 [ref=6x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x01, 16, 0, 0 }, // #52 [ref=6x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x01, 32, 0, 0 }, // #53 [ref=4x]
  { InstDB::RWInfoRm::kCategoryConsistent, 0x0C, 0 , 0, 0 }, // #54 [ref=15x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x0C, 8 , 0, 0 }, // #55 [ref=4x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x0C, 4 , 0, 0 }, // #56 [ref=4x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x04, 32, 0, 0 }, // #57 [ref=6x]
  { InstDB::RWInfoRm::kCategoryConsistent, 0x03, 0 , 0, 0 }, // #58 [ref=13x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x03, 8 , InstDB::RWInfoRm::kFlagAmbiguous, 0 }, // #59 [ref=1x]
  { InstDB::RWInfoRm::kCategoryConsistent, 0x08, 0 , 0, 0 }, // #60 [ref=2x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x04, 1 , 0, 0 }, // #61 [ref=1x]
  { InstDB::RWInfoRm::kCategoryQuarter   , 0x01, 0 , 0, 0 }, // #62 [ref=6x]
  { InstDB::RWInfoRm::kCategoryEighth    , 0x01, 0 , 0, 0 }, // #63 [ref=3x]
  { InstDB::RWInfoRm::kCategoryEighth    , 0x02, 0 , 0, 0 }, // #64 [ref=2x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x0C, 16, 0, 0 }, // #65 [ref=1x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x06, 16, 0, 0 }, // #66 [ref=12x]
  { InstDB::RWInfoRm::kCategoryConsistent, 0x06, 0 , InstDB::RWInfoRm::kFlagFeatureIfRMI, uint32_t(CpuFeatures::X86::kAVX512_F) }, // #67 [ref=5x]
  { InstDB::RWInfoRm::kCategoryConsistent, 0x02, 0 , InstDB::RWInfoRm::kFlagFeatureIfRMI, uint32_t(CpuFeatures::X86::kAVX512_BW) }, // #68 [ref=2x]
  { InstDB::RWInfoRm::kCategoryConsistent, 0x06, 0 , InstDB::RWInfoRm::kFlagFeatureIfRMI, uint32_t(CpuFeatures::X86::kAVX512_BW) }  // #69 [ref=3x]
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
