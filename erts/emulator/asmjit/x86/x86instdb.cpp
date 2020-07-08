// AsmJit - Machine code generation for C++
//
//  * Official AsmJit Home Page: https://asmjit.com
//  * Official Github Repository: https://github.com/asmjit/asmjit
//
// Copyright (c) 2008-2020 The AsmJit Authors
//
// This software is provided 'as-is', without any express or implied
// warranty. In no event will the authors be held liable for any damages
// arising from the use of this software.
//
// Permission is granted to anyone to use this software for any purpose,
// including commercial applications, and to alter it and redistribute it
// freely, subject to the following restrictions:
//
// 1. The origin of this software must not be misrepresented; you must not
//    claim that you wrote the original software. If you use this software
//    in a product, an acknowledgment in the product documentation would be
//    appreciated but is not required.
// 2. Altered source versions must be plainly marked as such, and must not be
//    misrepresented as being the original software.
// 3. This notice may not be removed or altered from any source distribution.

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
#ifdef ASMJIT_BUILD_X86

#include "../core/cpuinfo.h"
#include "../core/misc_p.h"
#include "../core/support.h"
#include "../x86/x86features.h"
#include "../x86/x86instdb_p.h"
#include "../x86/x86opcode_p.h"
#include "../x86/x86operand.h"

ASMJIT_BEGIN_SUB_NAMESPACE(x86)

// ============================================================================
// [asmjit::x86::InstDB - InstInfo]
// ============================================================================

// Instruction opcode definitions:
//   - `O` encodes X86|MMX|SSE instructions.
//   - `V` encodes VEX|XOP|EVEX instructions.
//   - `E` encodes EVEX instructions only.
#define O_ENCODE(VEX, PREFIX, OPCODE, O, L, W, EvexW, N, TT) \
  ((PREFIX) | (OPCODE) | (O) | (L) | (W) | (EvexW) | (N) | (TT) | \
   (VEX && ((PREFIX) & Opcode::kMM_Mask) != Opcode::kMM_0F ? int(Opcode::kMM_ForceVex3) : 0))

#define O(PREFIX, OPCODE, ModO, LL, W, EvexW, N, ModRM) (O_ENCODE(0, Opcode::k##PREFIX, 0x##OPCODE, Opcode::kModO_##ModO, Opcode::kLL_##LL, Opcode::kW_##W, Opcode::kEvex_W_##EvexW, Opcode::kCDSHL_##N, Opcode::kModRM_##ModRM))
#define V(PREFIX, OPCODE, ModO, LL, W, EvexW, N, TT) (O_ENCODE(1, Opcode::k##PREFIX, 0x##OPCODE, Opcode::kModO_##ModO, Opcode::kLL_##LL, Opcode::kW_##W, Opcode::kEvex_W_##EvexW, Opcode::kCDSHL_##N, Opcode::kCDTT_##TT))
#define E(PREFIX, OPCODE, ModO, LL, W, EvexW, N, TT) (O_ENCODE(1, Opcode::k##PREFIX, 0x##OPCODE, Opcode::kModO_##ModO, Opcode::kLL_##LL, Opcode::kW_##W, Opcode::kEvex_W_##EvexW, Opcode::kCDSHL_##N, Opcode::kCDTT_##TT) | Opcode::kMM_ForceEvex)
#define O_FPU(PREFIX, OPCODE, ModO) (Opcode::kFPU_##PREFIX | (0x##OPCODE & 0xFFu) | ((0x##OPCODE >> 8) << Opcode::kFPU_2B_Shift) | Opcode::kModO_##ModO)

// Don't store `_nameDataIndex` if instruction names are disabled. Since some
// APIs can use `_nameDataIndex` it's much safer if it's zero if it's not defined.
#ifndef ASMJIT_NO_TEXT
  #define NAME_DATA_INDEX(Index) Index
#else
  #define NAME_DATA_INDEX(Index) 0
#endif

// Defines an X86 instruction.
#define INST(id, encoding, opcode0, opcode1, mainOpcodeIndex, altOpcodeIndex, nameDataIndex, commomInfoIndexA, commomInfoIndexB) { \
  uint32_t(NAME_DATA_INDEX(nameDataIndex)), \
  uint32_t(commomInfoIndexA),               \
  uint32_t(commomInfoIndexB),               \
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
  INST(Add              , X86Arith           , O(000000,00,0,_,x,_,_,_  ), 0                         , 0  , 0  , 3112 , 3  , 1  ), // #7
  INST(Addpd            , ExtRm              , O(660F00,58,_,_,_,_,_,_  ), 0                         , 3  , 0  , 5102 , 5  , 4  ), // #8
  INST(Addps            , ExtRm              , O(000F00,58,_,_,_,_,_,_  ), 0                         , 4  , 0  , 5114 , 5  , 5  ), // #9
  INST(Addsd            , ExtRm              , O(F20F00,58,_,_,_,_,_,_  ), 0                         , 5  , 0  , 5336 , 6  , 4  ), // #10
  INST(Addss            , ExtRm              , O(F30F00,58,_,_,_,_,_,_  ), 0                         , 6  , 0  , 3243 , 7  , 5  ), // #11
  INST(Addsubpd         , ExtRm              , O(660F00,D0,_,_,_,_,_,_  ), 0                         , 3  , 0  , 4841 , 5  , 6  ), // #12
  INST(Addsubps         , ExtRm              , O(F20F00,D0,_,_,_,_,_,_  ), 0                         , 5  , 0  , 4853 , 5  , 6  ), // #13
  INST(Adox             , X86Rm              , O(F30F38,F6,_,_,x,_,_,_  ), 0                         , 7  , 0  , 26   , 4  , 7  ), // #14
  INST(Aesdec           , ExtRm              , O(660F38,DE,_,_,_,_,_,_  ), 0                         , 2  , 0  , 3298 , 5  , 8  ), // #15
  INST(Aesdeclast       , ExtRm              , O(660F38,DF,_,_,_,_,_,_  ), 0                         , 2  , 0  , 3306 , 5  , 8  ), // #16
  INST(Aesenc           , ExtRm              , O(660F38,DC,_,_,_,_,_,_  ), 0                         , 2  , 0  , 3318 , 5  , 8  ), // #17
  INST(Aesenclast       , ExtRm              , O(660F38,DD,_,_,_,_,_,_  ), 0                         , 2  , 0  , 3326 , 5  , 8  ), // #18
  INST(Aesimc           , ExtRm              , O(660F38,DB,_,_,_,_,_,_  ), 0                         , 2  , 0  , 3338 , 5  , 8  ), // #19
  INST(Aeskeygenassist  , ExtRmi             , O(660F3A,DF,_,_,_,_,_,_  ), 0                         , 8  , 0  , 3346 , 8  , 8  ), // #20
  INST(And              , X86Arith           , O(000000,20,4,_,x,_,_,_  ), 0                         , 9  , 0  , 2510 , 9  , 1  ), // #21
  INST(Andn             , VexRvm_Wx          , V(000F38,F2,_,0,x,_,_,_  ), 0                         , 10 , 0  , 6810 , 10 , 9  ), // #22
  INST(Andnpd           , ExtRm              , O(660F00,55,_,_,_,_,_,_  ), 0                         , 3  , 0  , 3379 , 5  , 4  ), // #23
  INST(Andnps           , ExtRm              , O(000F00,55,_,_,_,_,_,_  ), 0                         , 4  , 0  , 3387 , 5  , 5  ), // #24
  INST(Andpd            , ExtRm              , O(660F00,54,_,_,_,_,_,_  ), 0                         , 3  , 0  , 4355 , 11 , 4  ), // #25
  INST(Andps            , ExtRm              , O(000F00,54,_,_,_,_,_,_  ), 0                         , 4  , 0  , 4365 , 11 , 5  ), // #26
  INST(Arpl             , X86Mr_NoSize       , O(000000,63,_,_,_,_,_,_  ), 0                         , 0  , 0  , 31   , 12 , 10 ), // #27
  INST(Bextr            , VexRmv_Wx          , V(000F38,F7,_,0,x,_,_,_  ), 0                         , 10 , 0  , 36   , 13 , 9  ), // #28
  INST(Blcfill          , VexVm_Wx           , V(XOP_M9,01,1,0,x,_,_,_  ), 0                         , 11 , 0  , 42   , 14 , 11 ), // #29
  INST(Blci             , VexVm_Wx           , V(XOP_M9,02,6,0,x,_,_,_  ), 0                         , 12 , 0  , 50   , 14 , 11 ), // #30
  INST(Blcic            , VexVm_Wx           , V(XOP_M9,01,5,0,x,_,_,_  ), 0                         , 13 , 0  , 55   , 14 , 11 ), // #31
  INST(Blcmsk           , VexVm_Wx           , V(XOP_M9,02,1,0,x,_,_,_  ), 0                         , 11 , 0  , 61   , 14 , 11 ), // #32
  INST(Blcs             , VexVm_Wx           , V(XOP_M9,01,3,0,x,_,_,_  ), 0                         , 14 , 0  , 68   , 14 , 11 ), // #33
  INST(Blendpd          , ExtRmi             , O(660F3A,0D,_,_,_,_,_,_  ), 0                         , 8  , 0  , 3465 , 8  , 12 ), // #34
  INST(Blendps          , ExtRmi             , O(660F3A,0C,_,_,_,_,_,_  ), 0                         , 8  , 0  , 3474 , 8  , 12 ), // #35
  INST(Blendvpd         , ExtRm_XMM0         , O(660F38,15,_,_,_,_,_,_  ), 0                         , 2  , 0  , 3483 , 15 , 12 ), // #36
  INST(Blendvps         , ExtRm_XMM0         , O(660F38,14,_,_,_,_,_,_  ), 0                         , 2  , 0  , 3493 , 15 , 12 ), // #37
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
  INST(Call             , X86Call            , O(000000,FF,2,_,_,_,_,_  ), 0                         , 1  , 0  , 3009 , 26 , 1  ), // #59
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
  INST(Clrssbsy         , X86M               , O(F30F00,AE,6,_,_,_,_,_  ), 0                         , 24 , 0  , 252  , 32 , 24 ), // #71
  INST(Clts             , X86Op              , O(000F00,06,_,_,_,_,_,_  ), 0                         , 4  , 0  , 261  , 30 , 0  ), // #72
  INST(Clwb             , X86M_Only          , O(660F00,AE,6,_,_,_,_,_  ), 0                         , 25 , 0  , 266  , 31 , 25 ), // #73
  INST(Clzero           , X86Op_MemZAX       , O(000F01,FC,_,_,_,_,_,_  ), 0                         , 21 , 0  , 271  , 33 , 26 ), // #74
  INST(Cmc              , X86Op              , O(000000,F5,_,_,_,_,_,_  ), 0                         , 0  , 0  , 278  , 30 , 27 ), // #75
  INST(Cmova            , X86Rm              , O(000F00,47,_,_,x,_,_,_  ), 0                         , 4  , 0  , 282  , 22 , 28 ), // #76
  INST(Cmovae           , X86Rm              , O(000F00,43,_,_,x,_,_,_  ), 0                         , 4  , 0  , 288  , 22 , 29 ), // #77
  INST(Cmovb            , X86Rm              , O(000F00,42,_,_,x,_,_,_  ), 0                         , 4  , 0  , 643  , 22 , 29 ), // #78
  INST(Cmovbe           , X86Rm              , O(000F00,46,_,_,x,_,_,_  ), 0                         , 4  , 0  , 650  , 22 , 28 ), // #79
  INST(Cmovc            , X86Rm              , O(000F00,42,_,_,x,_,_,_  ), 0                         , 4  , 0  , 295  , 22 , 29 ), // #80
  INST(Cmove            , X86Rm              , O(000F00,44,_,_,x,_,_,_  ), 0                         , 4  , 0  , 658  , 22 , 30 ), // #81
  INST(Cmovg            , X86Rm              , O(000F00,4F,_,_,x,_,_,_  ), 0                         , 4  , 0  , 301  , 22 , 31 ), // #82
  INST(Cmovge           , X86Rm              , O(000F00,4D,_,_,x,_,_,_  ), 0                         , 4  , 0  , 307  , 22 , 32 ), // #83
  INST(Cmovl            , X86Rm              , O(000F00,4C,_,_,x,_,_,_  ), 0                         , 4  , 0  , 314  , 22 , 32 ), // #84
  INST(Cmovle           , X86Rm              , O(000F00,4E,_,_,x,_,_,_  ), 0                         , 4  , 0  , 320  , 22 , 31 ), // #85
  INST(Cmovna           , X86Rm              , O(000F00,46,_,_,x,_,_,_  ), 0                         , 4  , 0  , 327  , 22 , 28 ), // #86
  INST(Cmovnae          , X86Rm              , O(000F00,42,_,_,x,_,_,_  ), 0                         , 4  , 0  , 334  , 22 , 29 ), // #87
  INST(Cmovnb           , X86Rm              , O(000F00,43,_,_,x,_,_,_  ), 0                         , 4  , 0  , 665  , 22 , 29 ), // #88
  INST(Cmovnbe          , X86Rm              , O(000F00,47,_,_,x,_,_,_  ), 0                         , 4  , 0  , 673  , 22 , 28 ), // #89
  INST(Cmovnc           , X86Rm              , O(000F00,43,_,_,x,_,_,_  ), 0                         , 4  , 0  , 342  , 22 , 29 ), // #90
  INST(Cmovne           , X86Rm              , O(000F00,45,_,_,x,_,_,_  ), 0                         , 4  , 0  , 682  , 22 , 30 ), // #91
  INST(Cmovng           , X86Rm              , O(000F00,4E,_,_,x,_,_,_  ), 0                         , 4  , 0  , 349  , 22 , 31 ), // #92
  INST(Cmovnge          , X86Rm              , O(000F00,4C,_,_,x,_,_,_  ), 0                         , 4  , 0  , 356  , 22 , 32 ), // #93
  INST(Cmovnl           , X86Rm              , O(000F00,4D,_,_,x,_,_,_  ), 0                         , 4  , 0  , 364  , 22 , 32 ), // #94
  INST(Cmovnle          , X86Rm              , O(000F00,4F,_,_,x,_,_,_  ), 0                         , 4  , 0  , 371  , 22 , 31 ), // #95
  INST(Cmovno           , X86Rm              , O(000F00,41,_,_,x,_,_,_  ), 0                         , 4  , 0  , 379  , 22 , 33 ), // #96
  INST(Cmovnp           , X86Rm              , O(000F00,4B,_,_,x,_,_,_  ), 0                         , 4  , 0  , 386  , 22 , 34 ), // #97
  INST(Cmovns           , X86Rm              , O(000F00,49,_,_,x,_,_,_  ), 0                         , 4  , 0  , 393  , 22 , 35 ), // #98
  INST(Cmovnz           , X86Rm              , O(000F00,45,_,_,x,_,_,_  ), 0                         , 4  , 0  , 400  , 22 , 30 ), // #99
  INST(Cmovo            , X86Rm              , O(000F00,40,_,_,x,_,_,_  ), 0                         , 4  , 0  , 407  , 22 , 33 ), // #100
  INST(Cmovp            , X86Rm              , O(000F00,4A,_,_,x,_,_,_  ), 0                         , 4  , 0  , 413  , 22 , 34 ), // #101
  INST(Cmovpe           , X86Rm              , O(000F00,4A,_,_,x,_,_,_  ), 0                         , 4  , 0  , 419  , 22 , 34 ), // #102
  INST(Cmovpo           , X86Rm              , O(000F00,4B,_,_,x,_,_,_  ), 0                         , 4  , 0  , 426  , 22 , 34 ), // #103
  INST(Cmovs            , X86Rm              , O(000F00,48,_,_,x,_,_,_  ), 0                         , 4  , 0  , 433  , 22 , 35 ), // #104
  INST(Cmovz            , X86Rm              , O(000F00,44,_,_,x,_,_,_  ), 0                         , 4  , 0  , 439  , 22 , 30 ), // #105
  INST(Cmp              , X86Arith           , O(000000,38,7,_,x,_,_,_  ), 0                         , 26 , 0  , 445  , 34 , 1  ), // #106
  INST(Cmppd            , ExtRmi             , O(660F00,C2,_,_,_,_,_,_  ), 0                         , 3  , 0  , 3719 , 8  , 4  ), // #107
  INST(Cmpps            , ExtRmi             , O(000F00,C2,_,_,_,_,_,_  ), 0                         , 4  , 0  , 3726 , 8  , 5  ), // #108
  INST(Cmps             , X86StrMm           , O(000000,A6,_,_,_,_,_,_  ), 0                         , 0  , 0  , 449  , 35 , 36 ), // #109
  INST(Cmpsd            , ExtRmi             , O(F20F00,C2,_,_,_,_,_,_  ), 0                         , 5  , 0  , 3733 , 36 , 4  ), // #110
  INST(Cmpss            , ExtRmi             , O(F30F00,C2,_,_,_,_,_,_  ), 0                         , 6  , 0  , 3740 , 37 , 5  ), // #111
  INST(Cmpxchg          , X86Cmpxchg         , O(000F00,B0,_,_,x,_,_,_  ), 0                         , 4  , 0  , 454  , 38 , 37 ), // #112
  INST(Cmpxchg16b       , X86Cmpxchg8b_16b   , O(000F00,C7,1,_,1,_,_,_  ), 0                         , 27 , 0  , 462  , 39 , 38 ), // #113
  INST(Cmpxchg8b        , X86Cmpxchg8b_16b   , O(000F00,C7,1,_,_,_,_,_  ), 0                         , 28 , 0  , 473  , 40 , 39 ), // #114
  INST(Comisd           , ExtRm              , O(660F00,2F,_,_,_,_,_,_  ), 0                         , 3  , 0  , 10246, 6  , 40 ), // #115
  INST(Comiss           , ExtRm              , O(000F00,2F,_,_,_,_,_,_  ), 0                         , 4  , 0  , 10255, 7  , 41 ), // #116
  INST(Cpuid            , X86Op              , O(000F00,A2,_,_,_,_,_,_  ), 0                         , 4  , 0  , 483  , 41 , 42 ), // #117
  INST(Cqo              , X86Op_xDX_xAX      , O(000000,99,_,_,1,_,_,_  ), 0                         , 20 , 0  , 489  , 42 , 0  ), // #118
  INST(Crc32            , X86Crc             , O(F20F38,F0,_,_,x,_,_,_  ), 0                         , 29 , 0  , 493  , 43 , 43 ), // #119
  INST(Cvtdq2pd         , ExtRm              , O(F30F00,E6,_,_,_,_,_,_  ), 0                         , 6  , 0  , 3787 , 6  , 4  ), // #120
  INST(Cvtdq2ps         , ExtRm              , O(000F00,5B,_,_,_,_,_,_  ), 0                         , 4  , 0  , 3797 , 5  , 4  ), // #121
  INST(Cvtpd2dq         , ExtRm              , O(F20F00,E6,_,_,_,_,_,_  ), 0                         , 5  , 0  , 3836 , 5  , 4  ), // #122
  INST(Cvtpd2pi         , ExtRm              , O(660F00,2D,_,_,_,_,_,_  ), 0                         , 3  , 0  , 499  , 44 , 4  ), // #123
  INST(Cvtpd2ps         , ExtRm              , O(660F00,5A,_,_,_,_,_,_  ), 0                         , 3  , 0  , 3846 , 5  , 4  ), // #124
  INST(Cvtpi2pd         , ExtRm              , O(660F00,2A,_,_,_,_,_,_  ), 0                         , 3  , 0  , 508  , 45 , 4  ), // #125
  INST(Cvtpi2ps         , ExtRm              , O(000F00,2A,_,_,_,_,_,_  ), 0                         , 4  , 0  , 517  , 45 , 5  ), // #126
  INST(Cvtps2dq         , ExtRm              , O(660F00,5B,_,_,_,_,_,_  ), 0                         , 3  , 0  , 3898 , 5  , 4  ), // #127
  INST(Cvtps2pd         , ExtRm              , O(000F00,5A,_,_,_,_,_,_  ), 0                         , 4  , 0  , 3908 , 6  , 4  ), // #128
  INST(Cvtps2pi         , ExtRm              , O(000F00,2D,_,_,_,_,_,_  ), 0                         , 4  , 0  , 526  , 46 , 5  ), // #129
  INST(Cvtsd2si         , ExtRm_Wx           , O(F20F00,2D,_,_,x,_,_,_  ), 0                         , 5  , 0  , 3980 , 47 , 4  ), // #130
  INST(Cvtsd2ss         , ExtRm              , O(F20F00,5A,_,_,_,_,_,_  ), 0                         , 5  , 0  , 3990 , 6  , 4  ), // #131
  INST(Cvtsi2sd         , ExtRm_Wx           , O(F20F00,2A,_,_,x,_,_,_  ), 0                         , 5  , 0  , 4011 , 48 , 4  ), // #132
  INST(Cvtsi2ss         , ExtRm_Wx           , O(F30F00,2A,_,_,x,_,_,_  ), 0                         , 6  , 0  , 4021 , 48 , 5  ), // #133
  INST(Cvtss2sd         , ExtRm              , O(F30F00,5A,_,_,_,_,_,_  ), 0                         , 6  , 0  , 4031 , 7  , 4  ), // #134
  INST(Cvtss2si         , ExtRm_Wx           , O(F30F00,2D,_,_,x,_,_,_  ), 0                         , 6  , 0  , 4041 , 49 , 5  ), // #135
  INST(Cvttpd2dq        , ExtRm              , O(660F00,E6,_,_,_,_,_,_  ), 0                         , 3  , 0  , 4062 , 5  , 4  ), // #136
  INST(Cvttpd2pi        , ExtRm              , O(660F00,2C,_,_,_,_,_,_  ), 0                         , 3  , 0  , 535  , 44 , 4  ), // #137
  INST(Cvttps2dq        , ExtRm              , O(F30F00,5B,_,_,_,_,_,_  ), 0                         , 6  , 0  , 4108 , 5  , 4  ), // #138
  INST(Cvttps2pi        , ExtRm              , O(000F00,2C,_,_,_,_,_,_  ), 0                         , 4  , 0  , 545  , 46 , 5  ), // #139
  INST(Cvttsd2si        , ExtRm_Wx           , O(F20F00,2C,_,_,x,_,_,_  ), 0                         , 5  , 0  , 4154 , 47 , 4  ), // #140
  INST(Cvttss2si        , ExtRm_Wx           , O(F30F00,2C,_,_,x,_,_,_  ), 0                         , 6  , 0  , 4177 , 49 , 5  ), // #141
  INST(Cwd              , X86Op_xDX_xAX      , O(660000,99,_,_,_,_,_,_  ), 0                         , 19 , 0  , 555  , 50 , 0  ), // #142
  INST(Cwde             , X86Op_xAX          , O(000000,98,_,_,_,_,_,_  ), 0                         , 0  , 0  , 559  , 51 , 0  ), // #143
  INST(Daa              , X86Op              , O(000000,27,_,_,_,_,_,_  ), 0                         , 0  , 0  , 564  , 1  , 1  ), // #144
  INST(Das              , X86Op              , O(000000,2F,_,_,_,_,_,_  ), 0                         , 0  , 0  , 568  , 1  , 1  ), // #145
  INST(Dec              , X86IncDec          , O(000000,FE,1,_,x,_,_,_  ), O(000000,48,_,_,x,_,_,_  ), 30 , 6  , 3301 , 52 , 44 ), // #146
  INST(Div              , X86M_GPB_MulDiv    , O(000000,F6,6,_,x,_,_,_  ), 0                         , 31 , 0  , 805  , 53 , 1  ), // #147
  INST(Divpd            , ExtRm              , O(660F00,5E,_,_,_,_,_,_  ), 0                         , 3  , 0  , 4276 , 5  , 4  ), // #148
  INST(Divps            , ExtRm              , O(000F00,5E,_,_,_,_,_,_  ), 0                         , 4  , 0  , 4283 , 5  , 5  ), // #149
  INST(Divsd            , ExtRm              , O(F20F00,5E,_,_,_,_,_,_  ), 0                         , 5  , 0  , 4290 , 6  , 4  ), // #150
  INST(Divss            , ExtRm              , O(F30F00,5E,_,_,_,_,_,_  ), 0                         , 6  , 0  , 4297 , 7  , 5  ), // #151
  INST(Dppd             , ExtRmi             , O(660F3A,41,_,_,_,_,_,_  ), 0                         , 8  , 0  , 4314 , 8  , 12 ), // #152
  INST(Dpps             , ExtRmi             , O(660F3A,40,_,_,_,_,_,_  ), 0                         , 8  , 0  , 4320 , 8  , 12 ), // #153
  INST(Emms             , X86Op              , O(000F00,77,_,_,_,_,_,_  ), 0                         , 4  , 0  , 773  , 54 , 45 ), // #154
  INST(Endbr32          , X86Op_Mod11RM      , O(F30F00,FB,7,_,_,_,_,3  ), 0                         , 32 , 0  , 572  , 30 , 46 ), // #155
  INST(Endbr64          , X86Op_Mod11RM      , O(F30F00,FA,7,_,_,_,_,2  ), 0                         , 33 , 0  , 580  , 30 , 46 ), // #156
  INST(Enqcmd           , X86EnqcmdMovdir64b , O(F20F38,F8,_,_,_,_,_,_  ), 0                         , 29 , 0  , 588  , 55 , 47 ), // #157
  INST(Enqcmds          , X86EnqcmdMovdir64b , O(F30F38,F8,_,_,_,_,_,_  ), 0                         , 7  , 0  , 595  , 55 , 47 ), // #158
  INST(Enter            , X86Enter           , O(000000,C8,_,_,_,_,_,_  ), 0                         , 0  , 0  , 3017 , 56 , 0  ), // #159
  INST(Extractps        , ExtExtract         , O(660F3A,17,_,_,_,_,_,_  ), 0                         , 8  , 0  , 4510 , 57 , 12 ), // #160
  INST(Extrq            , ExtExtrq           , O(660F00,79,_,_,_,_,_,_  ), O(660F00,78,0,_,_,_,_,_  ), 3  , 7  , 7606 , 58 , 48 ), // #161
  INST(F2xm1            , FpuOp              , O_FPU(00,D9F0,_)          , 0                         , 34 , 0  , 603  , 30 , 0  ), // #162
  INST(Fabs             , FpuOp              , O_FPU(00,D9E1,_)          , 0                         , 34 , 0  , 609  , 30 , 0  ), // #163
  INST(Fadd             , FpuArith           , O_FPU(00,C0C0,0)          , 0                         , 35 , 0  , 2106 , 59 , 0  ), // #164
  INST(Faddp            , FpuRDef            , O_FPU(00,DEC0,_)          , 0                         , 36 , 0  , 614  , 60 , 0  ), // #165
  INST(Fbld             , X86M_Only          , O_FPU(00,00DF,4)          , 0                         , 37 , 0  , 620  , 61 , 0  ), // #166
  INST(Fbstp            , X86M_Only          , O_FPU(00,00DF,6)          , 0                         , 38 , 0  , 625  , 61 , 0  ), // #167
  INST(Fchs             , FpuOp              , O_FPU(00,D9E0,_)          , 0                         , 34 , 0  , 631  , 30 , 0  ), // #168
  INST(Fclex            , FpuOp              , O_FPU(9B,DBE2,_)          , 0                         , 39 , 0  , 636  , 30 , 0  ), // #169
  INST(Fcmovb           , FpuR               , O_FPU(00,DAC0,_)          , 0                         , 40 , 0  , 642  , 62 , 29 ), // #170
  INST(Fcmovbe          , FpuR               , O_FPU(00,DAD0,_)          , 0                         , 40 , 0  , 649  , 62 , 28 ), // #171
  INST(Fcmove           , FpuR               , O_FPU(00,DAC8,_)          , 0                         , 40 , 0  , 657  , 62 , 30 ), // #172
  INST(Fcmovnb          , FpuR               , O_FPU(00,DBC0,_)          , 0                         , 41 , 0  , 664  , 62 , 29 ), // #173
  INST(Fcmovnbe         , FpuR               , O_FPU(00,DBD0,_)          , 0                         , 41 , 0  , 672  , 62 , 28 ), // #174
  INST(Fcmovne          , FpuR               , O_FPU(00,DBC8,_)          , 0                         , 41 , 0  , 681  , 62 , 30 ), // #175
  INST(Fcmovnu          , FpuR               , O_FPU(00,DBD8,_)          , 0                         , 41 , 0  , 689  , 62 , 34 ), // #176
  INST(Fcmovu           , FpuR               , O_FPU(00,DAD8,_)          , 0                         , 40 , 0  , 697  , 62 , 34 ), // #177
  INST(Fcom             , FpuCom             , O_FPU(00,D0D0,2)          , 0                         , 42 , 0  , 704  , 63 , 0  ), // #178
  INST(Fcomi            , FpuR               , O_FPU(00,DBF0,_)          , 0                         , 41 , 0  , 709  , 62 , 49 ), // #179
  INST(Fcomip           , FpuR               , O_FPU(00,DFF0,_)          , 0                         , 43 , 0  , 715  , 62 , 49 ), // #180
  INST(Fcomp            , FpuCom             , O_FPU(00,D8D8,3)          , 0                         , 44 , 0  , 722  , 63 , 0  ), // #181
  INST(Fcompp           , FpuOp              , O_FPU(00,DED9,_)          , 0                         , 36 , 0  , 728  , 30 , 0  ), // #182
  INST(Fcos             , FpuOp              , O_FPU(00,D9FF,_)          , 0                         , 34 , 0  , 735  , 30 , 0  ), // #183
  INST(Fdecstp          , FpuOp              , O_FPU(00,D9F6,_)          , 0                         , 34 , 0  , 740  , 30 , 0  ), // #184
  INST(Fdiv             , FpuArith           , O_FPU(00,F0F8,6)          , 0                         , 45 , 0  , 748  , 59 , 0  ), // #185
  INST(Fdivp            , FpuRDef            , O_FPU(00,DEF8,_)          , 0                         , 36 , 0  , 753  , 60 , 0  ), // #186
  INST(Fdivr            , FpuArith           , O_FPU(00,F8F0,7)          , 0                         , 46 , 0  , 759  , 59 , 0  ), // #187
  INST(Fdivrp           , FpuRDef            , O_FPU(00,DEF0,_)          , 0                         , 36 , 0  , 765  , 60 , 0  ), // #188
  INST(Femms            , X86Op              , O(000F00,0E,_,_,_,_,_,_  ), 0                         , 4  , 0  , 772  , 30 , 50 ), // #189
  INST(Ffree            , FpuR               , O_FPU(00,DDC0,_)          , 0                         , 47 , 0  , 778  , 62 , 0  ), // #190
  INST(Fiadd            , FpuM               , O_FPU(00,00DA,0)          , 0                         , 48 , 0  , 784  , 64 , 0  ), // #191
  INST(Ficom            , FpuM               , O_FPU(00,00DA,2)          , 0                         , 49 , 0  , 790  , 64 , 0  ), // #192
  INST(Ficomp           , FpuM               , O_FPU(00,00DA,3)          , 0                         , 50 , 0  , 796  , 64 , 0  ), // #193
  INST(Fidiv            , FpuM               , O_FPU(00,00DA,6)          , 0                         , 38 , 0  , 803  , 64 , 0  ), // #194
  INST(Fidivr           , FpuM               , O_FPU(00,00DA,7)          , 0                         , 51 , 0  , 809  , 64 , 0  ), // #195
  INST(Fild             , FpuM               , O_FPU(00,00DB,0)          , O_FPU(00,00DF,5)          , 48 , 8  , 816  , 65 , 0  ), // #196
  INST(Fimul            , FpuM               , O_FPU(00,00DA,1)          , 0                         , 52 , 0  , 821  , 64 , 0  ), // #197
  INST(Fincstp          , FpuOp              , O_FPU(00,D9F7,_)          , 0                         , 34 , 0  , 827  , 30 , 0  ), // #198
  INST(Finit            , FpuOp              , O_FPU(9B,DBE3,_)          , 0                         , 39 , 0  , 835  , 30 , 0  ), // #199
  INST(Fist             , FpuM               , O_FPU(00,00DB,2)          , 0                         , 49 , 0  , 841  , 64 , 0  ), // #200
  INST(Fistp            , FpuM               , O_FPU(00,00DB,3)          , O_FPU(00,00DF,7)          , 50 , 9  , 846  , 65 , 0  ), // #201
  INST(Fisttp           , FpuM               , O_FPU(00,00DB,1)          , O_FPU(00,00DD,1)          , 52 , 10 , 852  , 65 , 6  ), // #202
  INST(Fisub            , FpuM               , O_FPU(00,00DA,4)          , 0                         , 37 , 0  , 859  , 64 , 0  ), // #203
  INST(Fisubr           , FpuM               , O_FPU(00,00DA,5)          , 0                         , 53 , 0  , 865  , 64 , 0  ), // #204
  INST(Fld              , FpuFldFst          , O_FPU(00,00D9,0)          , O_FPU(00,00DB,5)          , 48 , 11 , 872  , 66 , 0  ), // #205
  INST(Fld1             , FpuOp              , O_FPU(00,D9E8,_)          , 0                         , 34 , 0  , 876  , 30 , 0  ), // #206
  INST(Fldcw            , X86M_Only          , O_FPU(00,00D9,5)          , 0                         , 53 , 0  , 881  , 67 , 0  ), // #207
  INST(Fldenv           , X86M_Only          , O_FPU(00,00D9,4)          , 0                         , 37 , 0  , 887  , 31 , 0  ), // #208
  INST(Fldl2e           , FpuOp              , O_FPU(00,D9EA,_)          , 0                         , 34 , 0  , 894  , 30 , 0  ), // #209
  INST(Fldl2t           , FpuOp              , O_FPU(00,D9E9,_)          , 0                         , 34 , 0  , 901  , 30 , 0  ), // #210
  INST(Fldlg2           , FpuOp              , O_FPU(00,D9EC,_)          , 0                         , 34 , 0  , 908  , 30 , 0  ), // #211
  INST(Fldln2           , FpuOp              , O_FPU(00,D9ED,_)          , 0                         , 34 , 0  , 915  , 30 , 0  ), // #212
  INST(Fldpi            , FpuOp              , O_FPU(00,D9EB,_)          , 0                         , 34 , 0  , 922  , 30 , 0  ), // #213
  INST(Fldz             , FpuOp              , O_FPU(00,D9EE,_)          , 0                         , 34 , 0  , 928  , 30 , 0  ), // #214
  INST(Fmul             , FpuArith           , O_FPU(00,C8C8,1)          , 0                         , 54 , 0  , 2148 , 59 , 0  ), // #215
  INST(Fmulp            , FpuRDef            , O_FPU(00,DEC8,_)          , 0                         , 36 , 0  , 933  , 60 , 0  ), // #216
  INST(Fnclex           , FpuOp              , O_FPU(00,DBE2,_)          , 0                         , 41 , 0  , 939  , 30 , 0  ), // #217
  INST(Fninit           , FpuOp              , O_FPU(00,DBE3,_)          , 0                         , 41 , 0  , 946  , 30 , 0  ), // #218
  INST(Fnop             , FpuOp              , O_FPU(00,D9D0,_)          , 0                         , 34 , 0  , 953  , 30 , 0  ), // #219
  INST(Fnsave           , X86M_Only          , O_FPU(00,00DD,6)          , 0                         , 38 , 0  , 958  , 31 , 0  ), // #220
  INST(Fnstcw           , X86M_Only          , O_FPU(00,00D9,7)          , 0                         , 51 , 0  , 965  , 67 , 0  ), // #221
  INST(Fnstenv          , X86M_Only          , O_FPU(00,00D9,6)          , 0                         , 38 , 0  , 972  , 31 , 0  ), // #222
  INST(Fnstsw           , FpuStsw            , O_FPU(00,00DD,7)          , O_FPU(00,DFE0,_)          , 51 , 12 , 980  , 68 , 0  ), // #223
  INST(Fpatan           , FpuOp              , O_FPU(00,D9F3,_)          , 0                         , 34 , 0  , 987  , 30 , 0  ), // #224
  INST(Fprem            , FpuOp              , O_FPU(00,D9F8,_)          , 0                         , 34 , 0  , 994  , 30 , 0  ), // #225
  INST(Fprem1           , FpuOp              , O_FPU(00,D9F5,_)          , 0                         , 34 , 0  , 1000 , 30 , 0  ), // #226
  INST(Fptan            , FpuOp              , O_FPU(00,D9F2,_)          , 0                         , 34 , 0  , 1007 , 30 , 0  ), // #227
  INST(Frndint          , FpuOp              , O_FPU(00,D9FC,_)          , 0                         , 34 , 0  , 1013 , 30 , 0  ), // #228
  INST(Frstor           , X86M_Only          , O_FPU(00,00DD,4)          , 0                         , 37 , 0  , 1021 , 31 , 0  ), // #229
  INST(Fsave            , X86M_Only          , O_FPU(9B,00DD,6)          , 0                         , 55 , 0  , 1028 , 31 , 0  ), // #230
  INST(Fscale           , FpuOp              , O_FPU(00,D9FD,_)          , 0                         , 34 , 0  , 1034 , 30 , 0  ), // #231
  INST(Fsin             , FpuOp              , O_FPU(00,D9FE,_)          , 0                         , 34 , 0  , 1041 , 30 , 0  ), // #232
  INST(Fsincos          , FpuOp              , O_FPU(00,D9FB,_)          , 0                         , 34 , 0  , 1046 , 30 , 0  ), // #233
  INST(Fsqrt            , FpuOp              , O_FPU(00,D9FA,_)          , 0                         , 34 , 0  , 1054 , 30 , 0  ), // #234
  INST(Fst              , FpuFldFst          , O_FPU(00,00D9,2)          , 0                         , 49 , 0  , 1060 , 69 , 0  ), // #235
  INST(Fstcw            , X86M_Only          , O_FPU(9B,00D9,7)          , 0                         , 56 , 0  , 1064 , 67 , 0  ), // #236
  INST(Fstenv           , X86M_Only          , O_FPU(9B,00D9,6)          , 0                         , 55 , 0  , 1070 , 31 , 0  ), // #237
  INST(Fstp             , FpuFldFst          , O_FPU(00,00D9,3)          , O(000000,DB,7,_,_,_,_,_  ), 50 , 13 , 1077 , 66 , 0  ), // #238
  INST(Fstsw            , FpuStsw            , O_FPU(9B,00DD,7)          , O_FPU(9B,DFE0,_)          , 56 , 14 , 1082 , 68 , 0  ), // #239
  INST(Fsub             , FpuArith           , O_FPU(00,E0E8,4)          , 0                         , 57 , 0  , 2226 , 59 , 0  ), // #240
  INST(Fsubp            , FpuRDef            , O_FPU(00,DEE8,_)          , 0                         , 36 , 0  , 1088 , 60 , 0  ), // #241
  INST(Fsubr            , FpuArith           , O_FPU(00,E8E0,5)          , 0                         , 58 , 0  , 2232 , 59 , 0  ), // #242
  INST(Fsubrp           , FpuRDef            , O_FPU(00,DEE0,_)          , 0                         , 36 , 0  , 1094 , 60 , 0  ), // #243
  INST(Ftst             , FpuOp              , O_FPU(00,D9E4,_)          , 0                         , 34 , 0  , 1101 , 30 , 0  ), // #244
  INST(Fucom            , FpuRDef            , O_FPU(00,DDE0,_)          , 0                         , 47 , 0  , 1106 , 60 , 0  ), // #245
  INST(Fucomi           , FpuR               , O_FPU(00,DBE8,_)          , 0                         , 41 , 0  , 1112 , 62 , 49 ), // #246
  INST(Fucomip          , FpuR               , O_FPU(00,DFE8,_)          , 0                         , 43 , 0  , 1119 , 62 , 49 ), // #247
  INST(Fucomp           , FpuRDef            , O_FPU(00,DDE8,_)          , 0                         , 47 , 0  , 1127 , 60 , 0  ), // #248
  INST(Fucompp          , FpuOp              , O_FPU(00,DAE9,_)          , 0                         , 40 , 0  , 1134 , 30 , 0  ), // #249
  INST(Fwait            , X86Op              , O_FPU(00,009B,_)          , 0                         , 59 , 0  , 1142 , 30 , 0  ), // #250
  INST(Fxam             , FpuOp              , O_FPU(00,D9E5,_)          , 0                         , 34 , 0  , 1148 , 30 , 0  ), // #251
  INST(Fxch             , FpuR               , O_FPU(00,D9C8,_)          , 0                         , 34 , 0  , 1153 , 60 , 0  ), // #252
  INST(Fxrstor          , X86M_Only          , O(000F00,AE,1,_,_,_,_,_  ), 0                         , 28 , 0  , 1158 , 31 , 51 ), // #253
  INST(Fxrstor64        , X86M_Only          , O(000F00,AE,1,_,1,_,_,_  ), 0                         , 27 , 0  , 1166 , 70 , 51 ), // #254
  INST(Fxsave           , X86M_Only          , O(000F00,AE,0,_,_,_,_,_  ), 0                         , 4  , 0  , 1176 , 31 , 51 ), // #255
  INST(Fxsave64         , X86M_Only          , O(000F00,AE,0,_,1,_,_,_  ), 0                         , 60 , 0  , 1183 , 70 , 51 ), // #256
  INST(Fxtract          , FpuOp              , O_FPU(00,D9F4,_)          , 0                         , 34 , 0  , 1192 , 30 , 0  ), // #257
  INST(Fyl2x            , FpuOp              , O_FPU(00,D9F1,_)          , 0                         , 34 , 0  , 1200 , 30 , 0  ), // #258
  INST(Fyl2xp1          , FpuOp              , O_FPU(00,D9F9,_)          , 0                         , 34 , 0  , 1206 , 30 , 0  ), // #259
  INST(Getsec           , X86Op              , O(000F00,37,_,_,_,_,_,_  ), 0                         , 4  , 0  , 1214 , 30 , 52 ), // #260
  INST(Gf2p8affineinvqb , ExtRmi             , O(660F3A,CF,_,_,_,_,_,_  ), 0                         , 8  , 0  , 5865 , 8  , 53 ), // #261
  INST(Gf2p8affineqb    , ExtRmi             , O(660F3A,CE,_,_,_,_,_,_  ), 0                         , 8  , 0  , 5883 , 8  , 53 ), // #262
  INST(Gf2p8mulb        , ExtRm              , O(660F38,CF,_,_,_,_,_,_  ), 0                         , 2  , 0  , 5898 , 5  , 53 ), // #263
  INST(Haddpd           , ExtRm              , O(660F00,7C,_,_,_,_,_,_  ), 0                         , 3  , 0  , 5909 , 5  , 6  ), // #264
  INST(Haddps           , ExtRm              , O(F20F00,7C,_,_,_,_,_,_  ), 0                         , 5  , 0  , 5917 , 5  , 6  ), // #265
  INST(Hlt              , X86Op              , O(000000,F4,_,_,_,_,_,_  ), 0                         , 0  , 0  , 1221 , 30 , 0  ), // #266
  INST(Hsubpd           , ExtRm              , O(660F00,7D,_,_,_,_,_,_  ), 0                         , 3  , 0  , 5925 , 5  , 6  ), // #267
  INST(Hsubps           , ExtRm              , O(F20F00,7D,_,_,_,_,_,_  ), 0                         , 5  , 0  , 5933 , 5  , 6  ), // #268
  INST(Idiv             , X86M_GPB_MulDiv    , O(000000,F6,7,_,x,_,_,_  ), 0                         , 26 , 0  , 804  , 53 , 1  ), // #269
  INST(Imul             , X86Imul            , O(000000,F6,5,_,x,_,_,_  ), 0                         , 61 , 0  , 822  , 71 , 1  ), // #270
  INST(In               , X86In              , O(000000,EC,_,_,_,_,_,_  ), O(000000,E4,_,_,_,_,_,_  ), 0  , 15 , 10418, 72 , 0  ), // #271
  INST(Inc              , X86IncDec          , O(000000,FE,0,_,x,_,_,_  ), O(000000,40,_,_,x,_,_,_  ), 0  , 16 , 1225 , 52 , 44 ), // #272
  INST(Incsspd          , X86M               , O(F30F00,AE,5,_,0,_,_,_  ), 0                         , 62 , 0  , 1229 , 73 , 54 ), // #273
  INST(Incsspq          , X86M               , O(F30F00,AE,5,_,1,_,_,_  ), 0                         , 63 , 0  , 1237 , 74 , 54 ), // #274
  INST(Ins              , X86Ins             , O(000000,6C,_,_,_,_,_,_  ), 0                         , 0  , 0  , 1908 , 75 , 0  ), // #275
  INST(Insertps         , ExtRmi             , O(660F3A,21,_,_,_,_,_,_  ), 0                         , 8  , 0  , 6069 , 37 , 12 ), // #276
  INST(Insertq          , ExtInsertq         , O(F20F00,79,_,_,_,_,_,_  ), O(F20F00,78,_,_,_,_,_,_  ), 5  , 17 , 1245 , 76 , 48 ), // #277
  INST(Int              , X86Int             , O(000000,CD,_,_,_,_,_,_  ), 0                         , 0  , 0  , 1017 , 77 , 0  ), // #278
  INST(Int3             , X86Op              , O(000000,CC,_,_,_,_,_,_  ), 0                         , 0  , 0  , 1253 , 30 , 0  ), // #279
  INST(Into             , X86Op              , O(000000,CE,_,_,_,_,_,_  ), 0                         , 0  , 0  , 1258 , 78 , 55 ), // #280
  INST(Invd             , X86Op              , O(000F00,08,_,_,_,_,_,_  ), 0                         , 4  , 0  , 10347, 30 , 42 ), // #281
  INST(Invept           , X86Rm_NoSize       , O(660F38,80,_,_,_,_,_,_  ), 0                         , 2  , 0  , 1263 , 79 , 56 ), // #282
  INST(Invlpg           , X86M_Only          , O(000F00,01,7,_,_,_,_,_  ), 0                         , 22 , 0  , 1270 , 31 , 42 ), // #283
  INST(Invlpga          , X86Op_xAddr        , O(000F01,DF,_,_,_,_,_,_  ), 0                         , 21 , 0  , 1277 , 80 , 22 ), // #284
  INST(Invpcid          , X86Rm_NoSize       , O(660F38,82,_,_,_,_,_,_  ), 0                         , 2  , 0  , 1285 , 79 , 42 ), // #285
  INST(Invvpid          , X86Rm_NoSize       , O(660F38,81,_,_,_,_,_,_  ), 0                         , 2  , 0  , 1293 , 79 , 56 ), // #286
  INST(Iret             , X86Op              , O(000000,CF,_,_,_,_,_,_  ), 0                         , 0  , 0  , 1301 , 81 , 1  ), // #287
  INST(Iretd            , X86Op              , O(000000,CF,_,_,_,_,_,_  ), 0                         , 0  , 0  , 1306 , 81 , 1  ), // #288
  INST(Iretq            , X86Op              , O(000000,CF,_,_,1,_,_,_  ), 0                         , 20 , 0  , 1312 , 82 , 1  ), // #289
  INST(Iretw            , X86Op              , O(660000,CF,_,_,_,_,_,_  ), 0                         , 19 , 0  , 1318 , 81 , 1  ), // #290
  INST(Ja               , X86Jcc             , O(000F00,87,_,_,_,_,_,_  ), O(000000,77,_,_,_,_,_,_  ), 4  , 18 , 1324 , 83 , 57 ), // #291
  INST(Jae              , X86Jcc             , O(000F00,83,_,_,_,_,_,_  ), O(000000,73,_,_,_,_,_,_  ), 4  , 19 , 1327 , 83 , 58 ), // #292
  INST(Jb               , X86Jcc             , O(000F00,82,_,_,_,_,_,_  ), O(000000,72,_,_,_,_,_,_  ), 4  , 20 , 1331 , 83 , 58 ), // #293
  INST(Jbe              , X86Jcc             , O(000F00,86,_,_,_,_,_,_  ), O(000000,76,_,_,_,_,_,_  ), 4  , 21 , 1334 , 83 , 57 ), // #294
  INST(Jc               , X86Jcc             , O(000F00,82,_,_,_,_,_,_  ), O(000000,72,_,_,_,_,_,_  ), 4  , 20 , 1338 , 83 , 58 ), // #295
  INST(Je               , X86Jcc             , O(000F00,84,_,_,_,_,_,_  ), O(000000,74,_,_,_,_,_,_  ), 4  , 22 , 1341 , 83 , 59 ), // #296
  INST(Jecxz            , X86JecxzLoop       , 0                         , O(000000,E3,_,_,_,_,_,_  ), 0  , 23 , 1344 , 84 , 0  ), // #297
  INST(Jg               , X86Jcc             , O(000F00,8F,_,_,_,_,_,_  ), O(000000,7F,_,_,_,_,_,_  ), 4  , 24 , 1350 , 83 , 60 ), // #298
  INST(Jge              , X86Jcc             , O(000F00,8D,_,_,_,_,_,_  ), O(000000,7D,_,_,_,_,_,_  ), 4  , 25 , 1353 , 83 , 61 ), // #299
  INST(Jl               , X86Jcc             , O(000F00,8C,_,_,_,_,_,_  ), O(000000,7C,_,_,_,_,_,_  ), 4  , 26 , 1357 , 83 , 61 ), // #300
  INST(Jle              , X86Jcc             , O(000F00,8E,_,_,_,_,_,_  ), O(000000,7E,_,_,_,_,_,_  ), 4  , 27 , 1360 , 83 , 60 ), // #301
  INST(Jmp              , X86Jmp             , O(000000,FF,4,_,_,_,_,_  ), O(000000,EB,_,_,_,_,_,_  ), 9  , 28 , 1364 , 85 , 0  ), // #302
  INST(Jna              , X86Jcc             , O(000F00,86,_,_,_,_,_,_  ), O(000000,76,_,_,_,_,_,_  ), 4  , 21 , 1368 , 83 , 57 ), // #303
  INST(Jnae             , X86Jcc             , O(000F00,82,_,_,_,_,_,_  ), O(000000,72,_,_,_,_,_,_  ), 4  , 20 , 1372 , 83 , 58 ), // #304
  INST(Jnb              , X86Jcc             , O(000F00,83,_,_,_,_,_,_  ), O(000000,73,_,_,_,_,_,_  ), 4  , 19 , 1377 , 83 , 58 ), // #305
  INST(Jnbe             , X86Jcc             , O(000F00,87,_,_,_,_,_,_  ), O(000000,77,_,_,_,_,_,_  ), 4  , 18 , 1381 , 83 , 57 ), // #306
  INST(Jnc              , X86Jcc             , O(000F00,83,_,_,_,_,_,_  ), O(000000,73,_,_,_,_,_,_  ), 4  , 19 , 1386 , 83 , 58 ), // #307
  INST(Jne              , X86Jcc             , O(000F00,85,_,_,_,_,_,_  ), O(000000,75,_,_,_,_,_,_  ), 4  , 29 , 1390 , 83 , 59 ), // #308
  INST(Jng              , X86Jcc             , O(000F00,8E,_,_,_,_,_,_  ), O(000000,7E,_,_,_,_,_,_  ), 4  , 27 , 1394 , 83 , 60 ), // #309
  INST(Jnge             , X86Jcc             , O(000F00,8C,_,_,_,_,_,_  ), O(000000,7C,_,_,_,_,_,_  ), 4  , 26 , 1398 , 83 , 61 ), // #310
  INST(Jnl              , X86Jcc             , O(000F00,8D,_,_,_,_,_,_  ), O(000000,7D,_,_,_,_,_,_  ), 4  , 25 , 1403 , 83 , 61 ), // #311
  INST(Jnle             , X86Jcc             , O(000F00,8F,_,_,_,_,_,_  ), O(000000,7F,_,_,_,_,_,_  ), 4  , 24 , 1407 , 83 , 60 ), // #312
  INST(Jno              , X86Jcc             , O(000F00,81,_,_,_,_,_,_  ), O(000000,71,_,_,_,_,_,_  ), 4  , 30 , 1412 , 83 , 55 ), // #313
  INST(Jnp              , X86Jcc             , O(000F00,8B,_,_,_,_,_,_  ), O(000000,7B,_,_,_,_,_,_  ), 4  , 31 , 1416 , 83 , 62 ), // #314
  INST(Jns              , X86Jcc             , O(000F00,89,_,_,_,_,_,_  ), O(000000,79,_,_,_,_,_,_  ), 4  , 32 , 1420 , 83 , 63 ), // #315
  INST(Jnz              , X86Jcc             , O(000F00,85,_,_,_,_,_,_  ), O(000000,75,_,_,_,_,_,_  ), 4  , 29 , 1424 , 83 , 59 ), // #316
  INST(Jo               , X86Jcc             , O(000F00,80,_,_,_,_,_,_  ), O(000000,70,_,_,_,_,_,_  ), 4  , 33 , 1428 , 83 , 55 ), // #317
  INST(Jp               , X86Jcc             , O(000F00,8A,_,_,_,_,_,_  ), O(000000,7A,_,_,_,_,_,_  ), 4  , 34 , 1431 , 83 , 62 ), // #318
  INST(Jpe              , X86Jcc             , O(000F00,8A,_,_,_,_,_,_  ), O(000000,7A,_,_,_,_,_,_  ), 4  , 34 , 1434 , 83 , 62 ), // #319
  INST(Jpo              , X86Jcc             , O(000F00,8B,_,_,_,_,_,_  ), O(000000,7B,_,_,_,_,_,_  ), 4  , 31 , 1438 , 83 , 62 ), // #320
  INST(Js               , X86Jcc             , O(000F00,88,_,_,_,_,_,_  ), O(000000,78,_,_,_,_,_,_  ), 4  , 35 , 1442 , 83 , 63 ), // #321
  INST(Jz               , X86Jcc             , O(000F00,84,_,_,_,_,_,_  ), O(000000,74,_,_,_,_,_,_  ), 4  , 22 , 1445 , 83 , 59 ), // #322
  INST(Kaddb            , VexRvm             , V(660F00,4A,_,1,0,_,_,_  ), 0                         , 64 , 0  , 1448 , 86 , 64 ), // #323
  INST(Kaddd            , VexRvm             , V(660F00,4A,_,1,1,_,_,_  ), 0                         , 65 , 0  , 1454 , 86 , 65 ), // #324
  INST(Kaddq            , VexRvm             , V(000F00,4A,_,1,1,_,_,_  ), 0                         , 66 , 0  , 1460 , 86 , 65 ), // #325
  INST(Kaddw            , VexRvm             , V(000F00,4A,_,1,0,_,_,_  ), 0                         , 67 , 0  , 1466 , 86 , 64 ), // #326
  INST(Kandb            , VexRvm             , V(660F00,41,_,1,0,_,_,_  ), 0                         , 64 , 0  , 1472 , 86 , 64 ), // #327
  INST(Kandd            , VexRvm             , V(660F00,41,_,1,1,_,_,_  ), 0                         , 65 , 0  , 1478 , 86 , 65 ), // #328
  INST(Kandnb           , VexRvm             , V(660F00,42,_,1,0,_,_,_  ), 0                         , 64 , 0  , 1484 , 86 , 64 ), // #329
  INST(Kandnd           , VexRvm             , V(660F00,42,_,1,1,_,_,_  ), 0                         , 65 , 0  , 1491 , 86 , 65 ), // #330
  INST(Kandnq           , VexRvm             , V(000F00,42,_,1,1,_,_,_  ), 0                         , 66 , 0  , 1498 , 86 , 65 ), // #331
  INST(Kandnw           , VexRvm             , V(000F00,42,_,1,0,_,_,_  ), 0                         , 67 , 0  , 1505 , 86 , 66 ), // #332
  INST(Kandq            , VexRvm             , V(000F00,41,_,1,1,_,_,_  ), 0                         , 66 , 0  , 1512 , 86 , 65 ), // #333
  INST(Kandw            , VexRvm             , V(000F00,41,_,1,0,_,_,_  ), 0                         , 67 , 0  , 1518 , 86 , 66 ), // #334
  INST(Kmovb            , VexKmov            , V(660F00,90,_,0,0,_,_,_  ), V(660F00,92,_,0,0,_,_,_  ), 68 , 36 , 1524 , 87 , 64 ), // #335
  INST(Kmovd            , VexKmov            , V(660F00,90,_,0,1,_,_,_  ), V(F20F00,92,_,0,0,_,_,_  ), 69 , 37 , 8086 , 88 , 65 ), // #336
  INST(Kmovq            , VexKmov            , V(000F00,90,_,0,1,_,_,_  ), V(F20F00,92,_,0,1,_,_,_  ), 70 , 38 , 8097 , 89 , 65 ), // #337
  INST(Kmovw            , VexKmov            , V(000F00,90,_,0,0,_,_,_  ), V(000F00,92,_,0,0,_,_,_  ), 71 , 39 , 1530 , 90 , 66 ), // #338
  INST(Knotb            , VexRm              , V(660F00,44,_,0,0,_,_,_  ), 0                         , 68 , 0  , 1536 , 91 , 64 ), // #339
  INST(Knotd            , VexRm              , V(660F00,44,_,0,1,_,_,_  ), 0                         , 69 , 0  , 1542 , 91 , 65 ), // #340
  INST(Knotq            , VexRm              , V(000F00,44,_,0,1,_,_,_  ), 0                         , 70 , 0  , 1548 , 91 , 65 ), // #341
  INST(Knotw            , VexRm              , V(000F00,44,_,0,0,_,_,_  ), 0                         , 71 , 0  , 1554 , 91 , 66 ), // #342
  INST(Korb             , VexRvm             , V(660F00,45,_,1,0,_,_,_  ), 0                         , 64 , 0  , 1560 , 86 , 64 ), // #343
  INST(Kord             , VexRvm             , V(660F00,45,_,1,1,_,_,_  ), 0                         , 65 , 0  , 1565 , 86 , 65 ), // #344
  INST(Korq             , VexRvm             , V(000F00,45,_,1,1,_,_,_  ), 0                         , 66 , 0  , 1570 , 86 , 65 ), // #345
  INST(Kortestb         , VexRm              , V(660F00,98,_,0,0,_,_,_  ), 0                         , 68 , 0  , 1575 , 91 , 67 ), // #346
  INST(Kortestd         , VexRm              , V(660F00,98,_,0,1,_,_,_  ), 0                         , 69 , 0  , 1584 , 91 , 68 ), // #347
  INST(Kortestq         , VexRm              , V(000F00,98,_,0,1,_,_,_  ), 0                         , 70 , 0  , 1593 , 91 , 68 ), // #348
  INST(Kortestw         , VexRm              , V(000F00,98,_,0,0,_,_,_  ), 0                         , 71 , 0  , 1602 , 91 , 69 ), // #349
  INST(Korw             , VexRvm             , V(000F00,45,_,1,0,_,_,_  ), 0                         , 67 , 0  , 1611 , 86 , 66 ), // #350
  INST(Kshiftlb         , VexRmi             , V(660F3A,32,_,0,0,_,_,_  ), 0                         , 72 , 0  , 1616 , 92 , 64 ), // #351
  INST(Kshiftld         , VexRmi             , V(660F3A,33,_,0,0,_,_,_  ), 0                         , 72 , 0  , 1625 , 92 , 65 ), // #352
  INST(Kshiftlq         , VexRmi             , V(660F3A,33,_,0,1,_,_,_  ), 0                         , 73 , 0  , 1634 , 92 , 65 ), // #353
  INST(Kshiftlw         , VexRmi             , V(660F3A,32,_,0,1,_,_,_  ), 0                         , 73 , 0  , 1643 , 92 , 66 ), // #354
  INST(Kshiftrb         , VexRmi             , V(660F3A,30,_,0,0,_,_,_  ), 0                         , 72 , 0  , 1652 , 92 , 64 ), // #355
  INST(Kshiftrd         , VexRmi             , V(660F3A,31,_,0,0,_,_,_  ), 0                         , 72 , 0  , 1661 , 92 , 65 ), // #356
  INST(Kshiftrq         , VexRmi             , V(660F3A,31,_,0,1,_,_,_  ), 0                         , 73 , 0  , 1670 , 92 , 65 ), // #357
  INST(Kshiftrw         , VexRmi             , V(660F3A,30,_,0,1,_,_,_  ), 0                         , 73 , 0  , 1679 , 92 , 66 ), // #358
  INST(Ktestb           , VexRm              , V(660F00,99,_,0,0,_,_,_  ), 0                         , 68 , 0  , 1688 , 91 , 67 ), // #359
  INST(Ktestd           , VexRm              , V(660F00,99,_,0,1,_,_,_  ), 0                         , 69 , 0  , 1695 , 91 , 68 ), // #360
  INST(Ktestq           , VexRm              , V(000F00,99,_,0,1,_,_,_  ), 0                         , 70 , 0  , 1702 , 91 , 68 ), // #361
  INST(Ktestw           , VexRm              , V(000F00,99,_,0,0,_,_,_  ), 0                         , 71 , 0  , 1709 , 91 , 67 ), // #362
  INST(Kunpckbw         , VexRvm             , V(660F00,4B,_,1,0,_,_,_  ), 0                         , 64 , 0  , 1716 , 86 , 66 ), // #363
  INST(Kunpckdq         , VexRvm             , V(000F00,4B,_,1,1,_,_,_  ), 0                         , 66 , 0  , 1725 , 86 , 65 ), // #364
  INST(Kunpckwd         , VexRvm             , V(000F00,4B,_,1,0,_,_,_  ), 0                         , 67 , 0  , 1734 , 86 , 65 ), // #365
  INST(Kxnorb           , VexRvm             , V(660F00,46,_,1,0,_,_,_  ), 0                         , 64 , 0  , 1743 , 86 , 64 ), // #366
  INST(Kxnord           , VexRvm             , V(660F00,46,_,1,1,_,_,_  ), 0                         , 65 , 0  , 1750 , 86 , 65 ), // #367
  INST(Kxnorq           , VexRvm             , V(000F00,46,_,1,1,_,_,_  ), 0                         , 66 , 0  , 1757 , 86 , 65 ), // #368
  INST(Kxnorw           , VexRvm             , V(000F00,46,_,1,0,_,_,_  ), 0                         , 67 , 0  , 1764 , 86 , 66 ), // #369
  INST(Kxorb            , VexRvm             , V(660F00,47,_,1,0,_,_,_  ), 0                         , 64 , 0  , 1771 , 86 , 64 ), // #370
  INST(Kxord            , VexRvm             , V(660F00,47,_,1,1,_,_,_  ), 0                         , 65 , 0  , 1777 , 86 , 65 ), // #371
  INST(Kxorq            , VexRvm             , V(000F00,47,_,1,1,_,_,_  ), 0                         , 66 , 0  , 1783 , 86 , 65 ), // #372
  INST(Kxorw            , VexRvm             , V(000F00,47,_,1,0,_,_,_  ), 0                         , 67 , 0  , 1789 , 86 , 66 ), // #373
  INST(Lahf             , X86Op              , O(000000,9F,_,_,_,_,_,_  ), 0                         , 0  , 0  , 1795 , 93 , 70 ), // #374
  INST(Lar              , X86Rm              , O(000F00,02,_,_,_,_,_,_  ), 0                         , 4  , 0  , 1800 , 94 , 10 ), // #375
  INST(Lddqu            , ExtRm              , O(F20F00,F0,_,_,_,_,_,_  ), 0                         , 5  , 0  , 6079 , 95 , 6  ), // #376
  INST(Ldmxcsr          , X86M_Only          , O(000F00,AE,2,_,_,_,_,_  ), 0                         , 74 , 0  , 6086 , 96 , 5  ), // #377
  INST(Lds              , X86Rm              , O(000000,C5,_,_,_,_,_,_  ), 0                         , 0  , 0  , 1804 , 97 , 0  ), // #378
  INST(Ldtilecfg        , AmxCfg             , V(000F38,49,_,0,0,_,_,_  ), 0                         , 10 , 0  , 1808 , 98 , 71 ), // #379
  INST(Lea              , X86Lea             , O(000000,8D,_,_,x,_,_,_  ), 0                         , 0  , 0  , 1818 , 99 , 0  ), // #380
  INST(Leave            , X86Op              , O(000000,C9,_,_,_,_,_,_  ), 0                         , 0  , 0  , 1822 , 30 , 0  ), // #381
  INST(Les              , X86Rm              , O(000000,C4,_,_,_,_,_,_  ), 0                         , 0  , 0  , 1828 , 97 , 0  ), // #382
  INST(Lfence           , X86Fence           , O(000F00,AE,5,_,_,_,_,_  ), 0                         , 75 , 0  , 1832 , 30 , 4  ), // #383
  INST(Lfs              , X86Rm              , O(000F00,B4,_,_,_,_,_,_  ), 0                         , 4  , 0  , 1839 , 100, 0  ), // #384
  INST(Lgdt             , X86M_Only          , O(000F00,01,2,_,_,_,_,_  ), 0                         , 74 , 0  , 1843 , 31 , 0  ), // #385
  INST(Lgs              , X86Rm              , O(000F00,B5,_,_,_,_,_,_  ), 0                         , 4  , 0  , 1848 , 100, 0  ), // #386
  INST(Lidt             , X86M_Only          , O(000F00,01,3,_,_,_,_,_  ), 0                         , 76 , 0  , 1852 , 31 , 0  ), // #387
  INST(Lldt             , X86M_NoSize        , O(000F00,00,2,_,_,_,_,_  ), 0                         , 74 , 0  , 1857 , 101, 0  ), // #388
  INST(Llwpcb           , VexR_Wx            , V(XOP_M9,12,0,0,x,_,_,_  ), 0                         , 77 , 0  , 1862 , 102, 72 ), // #389
  INST(Lmsw             , X86M_NoSize        , O(000F00,01,6,_,_,_,_,_  ), 0                         , 78 , 0  , 1869 , 101, 0  ), // #390
  INST(Lods             , X86StrRm           , O(000000,AC,_,_,_,_,_,_  ), 0                         , 0  , 0  , 1874 , 103, 73 ), // #391
  INST(Loop             , X86JecxzLoop       , 0                         , O(000000,E2,_,_,_,_,_,_  ), 0  , 40 , 1879 , 104, 0  ), // #392
  INST(Loope            , X86JecxzLoop       , 0                         , O(000000,E1,_,_,_,_,_,_  ), 0  , 41 , 1884 , 104, 59 ), // #393
  INST(Loopne           , X86JecxzLoop       , 0                         , O(000000,E0,_,_,_,_,_,_  ), 0  , 42 , 1890 , 104, 59 ), // #394
  INST(Lsl              , X86Rm              , O(000F00,03,_,_,_,_,_,_  ), 0                         , 4  , 0  , 1897 , 105, 10 ), // #395
  INST(Lss              , X86Rm              , O(000F00,B2,_,_,_,_,_,_  ), 0                         , 4  , 0  , 6577 , 100, 0  ), // #396
  INST(Ltr              , X86M_NoSize        , O(000F00,00,3,_,_,_,_,_  ), 0                         , 76 , 0  , 1901 , 101, 0  ), // #397
  INST(Lwpins           , VexVmi4_Wx         , V(XOP_MA,12,0,0,x,_,_,_  ), 0                         , 79 , 0  , 1905 , 106, 72 ), // #398
  INST(Lwpval           , VexVmi4_Wx         , V(XOP_MA,12,1,0,x,_,_,_  ), 0                         , 80 , 0  , 1912 , 106, 72 ), // #399
  INST(Lzcnt            , X86Rm_Raw66H       , O(F30F00,BD,_,_,x,_,_,_  ), 0                         , 6  , 0  , 1919 , 22 , 74 ), // #400
  INST(Maskmovdqu       , ExtRm_ZDI          , O(660F00,57,_,_,_,_,_,_  ), 0                         , 3  , 0  , 6095 , 107, 4  ), // #401
  INST(Maskmovq         , ExtRm_ZDI          , O(000F00,F7,_,_,_,_,_,_  ), 0                         , 4  , 0  , 8094 , 108, 75 ), // #402
  INST(Maxpd            , ExtRm              , O(660F00,5F,_,_,_,_,_,_  ), 0                         , 3  , 0  , 6129 , 5  , 4  ), // #403
  INST(Maxps            , ExtRm              , O(000F00,5F,_,_,_,_,_,_  ), 0                         , 4  , 0  , 6136 , 5  , 5  ), // #404
  INST(Maxsd            , ExtRm              , O(F20F00,5F,_,_,_,_,_,_  ), 0                         , 5  , 0  , 8113 , 6  , 4  ), // #405
  INST(Maxss            , ExtRm              , O(F30F00,5F,_,_,_,_,_,_  ), 0                         , 6  , 0  , 6150 , 7  , 5  ), // #406
  INST(Mcommit          , X86Op              , O(F30F01,FA,_,_,_,_,_,_  ), 0                         , 81 , 0  , 1925 , 30 , 76 ), // #407
  INST(Mfence           , X86Fence           , O(000F00,AE,6,_,_,_,_,_  ), 0                         , 78 , 0  , 1933 , 30 , 4  ), // #408
  INST(Minpd            , ExtRm              , O(660F00,5D,_,_,_,_,_,_  ), 0                         , 3  , 0  , 6179 , 5  , 4  ), // #409
  INST(Minps            , ExtRm              , O(000F00,5D,_,_,_,_,_,_  ), 0                         , 4  , 0  , 6186 , 5  , 5  ), // #410
  INST(Minsd            , ExtRm              , O(F20F00,5D,_,_,_,_,_,_  ), 0                         , 5  , 0  , 8177 , 6  , 4  ), // #411
  INST(Minss            , ExtRm              , O(F30F00,5D,_,_,_,_,_,_  ), 0                         , 6  , 0  , 6200 , 7  , 5  ), // #412
  INST(Monitor          , X86Op              , O(000F01,C8,_,_,_,_,_,_  ), 0                         , 21 , 0  , 3192 , 109, 77 ), // #413
  INST(Monitorx         , X86Op              , O(000F01,FA,_,_,_,_,_,_  ), 0                         , 21 , 0  , 1940 , 109, 78 ), // #414
  INST(Mov              , X86Mov             , 0                         , 0                         , 0  , 0  , 138  , 110, 0  ), // #415
  INST(Movapd           , ExtMov             , O(660F00,28,_,_,_,_,_,_  ), O(660F00,29,_,_,_,_,_,_  ), 3  , 43 , 6231 , 111, 4  ), // #416
  INST(Movaps           , ExtMov             , O(000F00,28,_,_,_,_,_,_  ), O(000F00,29,_,_,_,_,_,_  ), 4  , 44 , 6239 , 111, 5  ), // #417
  INST(Movbe            , ExtMovbe           , O(000F38,F0,_,_,x,_,_,_  ), O(000F38,F1,_,_,x,_,_,_  ), 82 , 45 , 651  , 112, 79 ), // #418
  INST(Movd             , ExtMovd            , O(000F00,6E,_,_,_,_,_,_  ), O(000F00,7E,_,_,_,_,_,_  ), 4  , 46 , 8087 , 113, 80 ), // #419
  INST(Movddup          , ExtMov             , O(F20F00,12,_,_,_,_,_,_  ), 0                         , 5  , 0  , 6253 , 6  , 6  ), // #420
  INST(Movdir64b        , X86EnqcmdMovdir64b , O(660F38,F8,_,_,_,_,_,_  ), 0                         , 2  , 0  , 1949 , 114, 81 ), // #421
  INST(Movdiri          , X86MovntiMovdiri   , O(000F38,F9,_,_,_,_,_,_  ), 0                         , 82 , 0  , 1959 , 115, 82 ), // #422
  INST(Movdq2q          , ExtMov             , O(F20F00,D6,_,_,_,_,_,_  ), 0                         , 5  , 0  , 1967 , 116, 4  ), // #423
  INST(Movdqa           , ExtMov             , O(660F00,6F,_,_,_,_,_,_  ), O(660F00,7F,_,_,_,_,_,_  ), 3  , 47 , 6262 , 111, 4  ), // #424
  INST(Movdqu           , ExtMov             , O(F30F00,6F,_,_,_,_,_,_  ), O(F30F00,7F,_,_,_,_,_,_  ), 6  , 48 , 6099 , 111, 4  ), // #425
  INST(Movhlps          , ExtMov             , O(000F00,12,_,_,_,_,_,_  ), 0                         , 4  , 0  , 6337 , 117, 5  ), // #426
  INST(Movhpd           , ExtMov             , O(660F00,16,_,_,_,_,_,_  ), O(660F00,17,_,_,_,_,_,_  ), 3  , 49 , 6346 , 118, 4  ), // #427
  INST(Movhps           , ExtMov             , O(000F00,16,_,_,_,_,_,_  ), O(000F00,17,_,_,_,_,_,_  ), 4  , 50 , 6354 , 118, 5  ), // #428
  INST(Movlhps          , ExtMov             , O(000F00,16,_,_,_,_,_,_  ), 0                         , 4  , 0  , 6362 , 117, 5  ), // #429
  INST(Movlpd           , ExtMov             , O(660F00,12,_,_,_,_,_,_  ), O(660F00,13,_,_,_,_,_,_  ), 3  , 51 , 6371 , 118, 4  ), // #430
  INST(Movlps           , ExtMov             , O(000F00,12,_,_,_,_,_,_  ), O(000F00,13,_,_,_,_,_,_  ), 4  , 52 , 6379 , 118, 5  ), // #431
  INST(Movmskpd         , ExtMov             , O(660F00,50,_,_,_,_,_,_  ), 0                         , 3  , 0  , 6387 , 119, 4  ), // #432
  INST(Movmskps         , ExtMov             , O(000F00,50,_,_,_,_,_,_  ), 0                         , 4  , 0  , 6397 , 119, 5  ), // #433
  INST(Movntdq          , ExtMov             , 0                         , O(660F00,E7,_,_,_,_,_,_  ), 0  , 53 , 6407 , 120, 4  ), // #434
  INST(Movntdqa         , ExtMov             , O(660F38,2A,_,_,_,_,_,_  ), 0                         , 2  , 0  , 6416 , 95 , 12 ), // #435
  INST(Movnti           , X86MovntiMovdiri   , O(000F00,C3,_,_,x,_,_,_  ), 0                         , 4  , 0  , 1975 , 115, 4  ), // #436
  INST(Movntpd          , ExtMov             , 0                         , O(660F00,2B,_,_,_,_,_,_  ), 0  , 54 , 6426 , 120, 4  ), // #437
  INST(Movntps          , ExtMov             , 0                         , O(000F00,2B,_,_,_,_,_,_  ), 0  , 55 , 6435 , 120, 5  ), // #438
  INST(Movntq           , ExtMov             , 0                         , O(000F00,E7,_,_,_,_,_,_  ), 0  , 56 , 1982 , 121, 75 ), // #439
  INST(Movntsd          , ExtMov             , 0                         , O(F20F00,2B,_,_,_,_,_,_  ), 0  , 57 , 1989 , 122, 48 ), // #440
  INST(Movntss          , ExtMov             , 0                         , O(F30F00,2B,_,_,_,_,_,_  ), 0  , 58 , 1997 , 123, 48 ), // #441
  INST(Movq             , ExtMovq            , O(000F00,6E,_,_,x,_,_,_  ), O(000F00,7E,_,_,x,_,_,_  ), 4  , 59 , 8098 , 124, 80 ), // #442
  INST(Movq2dq          , ExtRm              , O(F30F00,D6,_,_,_,_,_,_  ), 0                         , 6  , 0  , 2005 , 125, 4  ), // #443
  INST(Movs             , X86StrMm           , O(000000,A4,_,_,_,_,_,_  ), 0                         , 0  , 0  , 434  , 126, 73 ), // #444
  INST(Movsd            , ExtMov             , O(F20F00,10,_,_,_,_,_,_  ), O(F20F00,11,_,_,_,_,_,_  ), 5  , 60 , 6450 , 127, 4  ), // #445
  INST(Movshdup         , ExtRm              , O(F30F00,16,_,_,_,_,_,_  ), 0                         , 6  , 0  , 6457 , 5  , 6  ), // #446
  INST(Movsldup         , ExtRm              , O(F30F00,12,_,_,_,_,_,_  ), 0                         , 6  , 0  , 6467 , 5  , 6  ), // #447
  INST(Movss            , ExtMov             , O(F30F00,10,_,_,_,_,_,_  ), O(F30F00,11,_,_,_,_,_,_  ), 6  , 61 , 6477 , 128, 5  ), // #448
  INST(Movsx            , X86MovsxMovzx      , O(000F00,BE,_,_,x,_,_,_  ), 0                         , 4  , 0  , 2013 , 129, 0  ), // #449
  INST(Movsxd           , X86Rm              , O(000000,63,_,_,x,_,_,_  ), 0                         , 0  , 0  , 2019 , 130, 0  ), // #450
  INST(Movupd           , ExtMov             , O(660F00,10,_,_,_,_,_,_  ), O(660F00,11,_,_,_,_,_,_  ), 3  , 62 , 6484 , 111, 4  ), // #451
  INST(Movups           , ExtMov             , O(000F00,10,_,_,_,_,_,_  ), O(000F00,11,_,_,_,_,_,_  ), 4  , 63 , 6492 , 111, 5  ), // #452
  INST(Movzx            , X86MovsxMovzx      , O(000F00,B6,_,_,x,_,_,_  ), 0                         , 4  , 0  , 2026 , 129, 0  ), // #453
  INST(Mpsadbw          , ExtRmi             , O(660F3A,42,_,_,_,_,_,_  ), 0                         , 8  , 0  , 6500 , 8  , 12 ), // #454
  INST(Mul              , X86M_GPB_MulDiv    , O(000000,F6,4,_,x,_,_,_  ), 0                         , 9  , 0  , 823  , 53 , 1  ), // #455
  INST(Mulpd            , ExtRm              , O(660F00,59,_,_,_,_,_,_  ), 0                         , 3  , 0  , 6554 , 5  , 4  ), // #456
  INST(Mulps            , ExtRm              , O(000F00,59,_,_,_,_,_,_  ), 0                         , 4  , 0  , 6561 , 5  , 5  ), // #457
  INST(Mulsd            , ExtRm              , O(F20F00,59,_,_,_,_,_,_  ), 0                         , 5  , 0  , 6568 , 6  , 4  ), // #458
  INST(Mulss            , ExtRm              , O(F30F00,59,_,_,_,_,_,_  ), 0                         , 6  , 0  , 6575 , 7  , 5  ), // #459
  INST(Mulx             , VexRvm_ZDX_Wx      , V(F20F38,F6,_,0,x,_,_,_  ), 0                         , 83 , 0  , 2032 , 131, 83 ), // #460
  INST(Mwait            , X86Op              , O(000F01,C9,_,_,_,_,_,_  ), 0                         , 21 , 0  , 3201 , 132, 77 ), // #461
  INST(Mwaitx           , X86Op              , O(000F01,FB,_,_,_,_,_,_  ), 0                         , 21 , 0  , 2037 , 133, 78 ), // #462
  INST(Neg              , X86M_GPB           , O(000000,F6,3,_,x,_,_,_  ), 0                         , 84 , 0  , 2044 , 134, 1  ), // #463
  INST(Nop              , X86M_Nop           , O(000000,90,_,_,_,_,_,_  ), 0                         , 0  , 0  , 954  , 135, 0  ), // #464
  INST(Not              , X86M_GPB           , O(000000,F6,2,_,x,_,_,_  ), 0                         , 1  , 0  , 2048 , 134, 0  ), // #465
  INST(Or               , X86Arith           , O(000000,08,1,_,x,_,_,_  ), 0                         , 30 , 0  , 3197 , 136, 1  ), // #466
  INST(Orpd             , ExtRm              , O(660F00,56,_,_,_,_,_,_  ), 0                         , 3  , 0  , 10304, 11 , 4  ), // #467
  INST(Orps             , ExtRm              , O(000F00,56,_,_,_,_,_,_  ), 0                         , 4  , 0  , 10311, 11 , 5  ), // #468
  INST(Out              , X86Out             , O(000000,EE,_,_,_,_,_,_  ), O(000000,E6,_,_,_,_,_,_  ), 0  , 64 , 2052 , 137, 0  ), // #469
  INST(Outs             , X86Outs            , O(000000,6E,_,_,_,_,_,_  ), 0                         , 0  , 0  , 2056 , 138, 0  ), // #470
  INST(Pabsb            , ExtRm_P            , O(000F38,1C,_,_,_,_,_,_  ), 0                         , 82 , 0  , 6657 , 139, 84 ), // #471
  INST(Pabsd            , ExtRm_P            , O(000F38,1E,_,_,_,_,_,_  ), 0                         , 82 , 0  , 6664 , 139, 84 ), // #472
  INST(Pabsw            , ExtRm_P            , O(000F38,1D,_,_,_,_,_,_  ), 0                         , 82 , 0  , 6678 , 139, 84 ), // #473
  INST(Packssdw         , ExtRm_P            , O(000F00,6B,_,_,_,_,_,_  ), 0                         , 4  , 0  , 6685 , 139, 80 ), // #474
  INST(Packsswb         , ExtRm_P            , O(000F00,63,_,_,_,_,_,_  ), 0                         , 4  , 0  , 6695 , 139, 80 ), // #475
  INST(Packusdw         , ExtRm              , O(660F38,2B,_,_,_,_,_,_  ), 0                         , 2  , 0  , 6705 , 5  , 12 ), // #476
  INST(Packuswb         , ExtRm_P            , O(000F00,67,_,_,_,_,_,_  ), 0                         , 4  , 0  , 6715 , 139, 80 ), // #477
  INST(Paddb            , ExtRm_P            , O(000F00,FC,_,_,_,_,_,_  ), 0                         , 4  , 0  , 6725 , 139, 80 ), // #478
  INST(Paddd            , ExtRm_P            , O(000F00,FE,_,_,_,_,_,_  ), 0                         , 4  , 0  , 6732 , 139, 80 ), // #479
  INST(Paddq            , ExtRm_P            , O(000F00,D4,_,_,_,_,_,_  ), 0                         , 4  , 0  , 6739 , 139, 4  ), // #480
  INST(Paddsb           , ExtRm_P            , O(000F00,EC,_,_,_,_,_,_  ), 0                         , 4  , 0  , 6746 , 139, 80 ), // #481
  INST(Paddsw           , ExtRm_P            , O(000F00,ED,_,_,_,_,_,_  ), 0                         , 4  , 0  , 6754 , 139, 80 ), // #482
  INST(Paddusb          , ExtRm_P            , O(000F00,DC,_,_,_,_,_,_  ), 0                         , 4  , 0  , 6762 , 139, 80 ), // #483
  INST(Paddusw          , ExtRm_P            , O(000F00,DD,_,_,_,_,_,_  ), 0                         , 4  , 0  , 6771 , 139, 80 ), // #484
  INST(Paddw            , ExtRm_P            , O(000F00,FD,_,_,_,_,_,_  ), 0                         , 4  , 0  , 6780 , 139, 80 ), // #485
  INST(Palignr          , ExtRmi_P           , O(000F3A,0F,_,_,_,_,_,_  ), 0                         , 85 , 0  , 6787 , 140, 6  ), // #486
  INST(Pand             , ExtRm_P            , O(000F00,DB,_,_,_,_,_,_  ), 0                         , 4  , 0  , 6796 , 141, 80 ), // #487
  INST(Pandn            , ExtRm_P            , O(000F00,DF,_,_,_,_,_,_  ), 0                         , 4  , 0  , 6809 , 142, 80 ), // #488
  INST(Pause            , X86Op              , O(F30000,90,_,_,_,_,_,_  ), 0                         , 86 , 0  , 3161 , 30 , 0  ), // #489
  INST(Pavgb            , ExtRm_P            , O(000F00,E0,_,_,_,_,_,_  ), 0                         , 4  , 0  , 6839 , 139, 85 ), // #490
  INST(Pavgusb          , Ext3dNow           , O(000F0F,BF,_,_,_,_,_,_  ), 0                         , 87 , 0  , 2061 , 143, 50 ), // #491
  INST(Pavgw            , ExtRm_P            , O(000F00,E3,_,_,_,_,_,_  ), 0                         , 4  , 0  , 6846 , 139, 85 ), // #492
  INST(Pblendvb         , ExtRm_XMM0         , O(660F38,10,_,_,_,_,_,_  ), 0                         , 2  , 0  , 6862 , 15 , 12 ), // #493
  INST(Pblendw          , ExtRmi             , O(660F3A,0E,_,_,_,_,_,_  ), 0                         , 8  , 0  , 6872 , 8  , 12 ), // #494
  INST(Pclmulqdq        , ExtRmi             , O(660F3A,44,_,_,_,_,_,_  ), 0                         , 8  , 0  , 6965 , 8  , 86 ), // #495
  INST(Pcmpeqb          , ExtRm_P            , O(000F00,74,_,_,_,_,_,_  ), 0                         , 4  , 0  , 6997 , 142, 80 ), // #496
  INST(Pcmpeqd          , ExtRm_P            , O(000F00,76,_,_,_,_,_,_  ), 0                         , 4  , 0  , 7006 , 142, 80 ), // #497
  INST(Pcmpeqq          , ExtRm              , O(660F38,29,_,_,_,_,_,_  ), 0                         , 2  , 0  , 7015 , 144, 12 ), // #498
  INST(Pcmpeqw          , ExtRm_P            , O(000F00,75,_,_,_,_,_,_  ), 0                         , 4  , 0  , 7024 , 142, 80 ), // #499
  INST(Pcmpestri        , ExtRmi             , O(660F3A,61,_,_,_,_,_,_  ), 0                         , 8  , 0  , 7033 , 145, 87 ), // #500
  INST(Pcmpestrm        , ExtRmi             , O(660F3A,60,_,_,_,_,_,_  ), 0                         , 8  , 0  , 7044 , 146, 87 ), // #501
  INST(Pcmpgtb          , ExtRm_P            , O(000F00,64,_,_,_,_,_,_  ), 0                         , 4  , 0  , 7055 , 142, 80 ), // #502
  INST(Pcmpgtd          , ExtRm_P            , O(000F00,66,_,_,_,_,_,_  ), 0                         , 4  , 0  , 7064 , 142, 80 ), // #503
  INST(Pcmpgtq          , ExtRm              , O(660F38,37,_,_,_,_,_,_  ), 0                         , 2  , 0  , 7073 , 144, 43 ), // #504
  INST(Pcmpgtw          , ExtRm_P            , O(000F00,65,_,_,_,_,_,_  ), 0                         , 4  , 0  , 7082 , 142, 80 ), // #505
  INST(Pcmpistri        , ExtRmi             , O(660F3A,63,_,_,_,_,_,_  ), 0                         , 8  , 0  , 7091 , 147, 87 ), // #506
  INST(Pcmpistrm        , ExtRmi             , O(660F3A,62,_,_,_,_,_,_  ), 0                         , 8  , 0  , 7102 , 148, 87 ), // #507
  INST(Pconfig          , X86Op              , O(000F01,C5,_,_,_,_,_,_  ), 0                         , 21 , 0  , 2069 , 30 , 88 ), // #508
  INST(Pdep             , VexRvm_Wx          , V(F20F38,F5,_,0,x,_,_,_  ), 0                         , 83 , 0  , 2077 , 10 , 83 ), // #509
  INST(Pext             , VexRvm_Wx          , V(F30F38,F5,_,0,x,_,_,_  ), 0                         , 88 , 0  , 2082 , 10 , 83 ), // #510
  INST(Pextrb           , ExtExtract         , O(000F3A,14,_,_,_,_,_,_  ), 0                         , 85 , 0  , 7589 , 149, 12 ), // #511
  INST(Pextrd           , ExtExtract         , O(000F3A,16,_,_,_,_,_,_  ), 0                         , 85 , 0  , 7597 , 57 , 12 ), // #512
  INST(Pextrq           , ExtExtract         , O(000F3A,16,_,_,1,_,_,_  ), 0                         , 89 , 0  , 7605 , 150, 12 ), // #513
  INST(Pextrw           , ExtPextrw          , O(000F00,C5,_,_,_,_,_,_  ), O(000F3A,15,_,_,_,_,_,_  ), 4  , 65 , 7613 , 151, 89 ), // #514
  INST(Pf2id            , Ext3dNow           , O(000F0F,1D,_,_,_,_,_,_  ), 0                         , 87 , 0  , 2087 , 143, 50 ), // #515
  INST(Pf2iw            , Ext3dNow           , O(000F0F,1C,_,_,_,_,_,_  ), 0                         , 87 , 0  , 2093 , 143, 90 ), // #516
  INST(Pfacc            , Ext3dNow           , O(000F0F,AE,_,_,_,_,_,_  ), 0                         , 87 , 0  , 2099 , 143, 50 ), // #517
  INST(Pfadd            , Ext3dNow           , O(000F0F,9E,_,_,_,_,_,_  ), 0                         , 87 , 0  , 2105 , 143, 50 ), // #518
  INST(Pfcmpeq          , Ext3dNow           , O(000F0F,B0,_,_,_,_,_,_  ), 0                         , 87 , 0  , 2111 , 143, 50 ), // #519
  INST(Pfcmpge          , Ext3dNow           , O(000F0F,90,_,_,_,_,_,_  ), 0                         , 87 , 0  , 2119 , 143, 50 ), // #520
  INST(Pfcmpgt          , Ext3dNow           , O(000F0F,A0,_,_,_,_,_,_  ), 0                         , 87 , 0  , 2127 , 143, 50 ), // #521
  INST(Pfmax            , Ext3dNow           , O(000F0F,A4,_,_,_,_,_,_  ), 0                         , 87 , 0  , 2135 , 143, 50 ), // #522
  INST(Pfmin            , Ext3dNow           , O(000F0F,94,_,_,_,_,_,_  ), 0                         , 87 , 0  , 2141 , 143, 50 ), // #523
  INST(Pfmul            , Ext3dNow           , O(000F0F,B4,_,_,_,_,_,_  ), 0                         , 87 , 0  , 2147 , 143, 50 ), // #524
  INST(Pfnacc           , Ext3dNow           , O(000F0F,8A,_,_,_,_,_,_  ), 0                         , 87 , 0  , 2153 , 143, 90 ), // #525
  INST(Pfpnacc          , Ext3dNow           , O(000F0F,8E,_,_,_,_,_,_  ), 0                         , 87 , 0  , 2160 , 143, 90 ), // #526
  INST(Pfrcp            , Ext3dNow           , O(000F0F,96,_,_,_,_,_,_  ), 0                         , 87 , 0  , 2168 , 143, 50 ), // #527
  INST(Pfrcpit1         , Ext3dNow           , O(000F0F,A6,_,_,_,_,_,_  ), 0                         , 87 , 0  , 2174 , 143, 50 ), // #528
  INST(Pfrcpit2         , Ext3dNow           , O(000F0F,B6,_,_,_,_,_,_  ), 0                         , 87 , 0  , 2183 , 143, 50 ), // #529
  INST(Pfrcpv           , Ext3dNow           , O(000F0F,86,_,_,_,_,_,_  ), 0                         , 87 , 0  , 2192 , 143, 91 ), // #530
  INST(Pfrsqit1         , Ext3dNow           , O(000F0F,A7,_,_,_,_,_,_  ), 0                         , 87 , 0  , 2199 , 143, 50 ), // #531
  INST(Pfrsqrt          , Ext3dNow           , O(000F0F,97,_,_,_,_,_,_  ), 0                         , 87 , 0  , 2208 , 143, 50 ), // #532
  INST(Pfrsqrtv         , Ext3dNow           , O(000F0F,87,_,_,_,_,_,_  ), 0                         , 87 , 0  , 2216 , 143, 91 ), // #533
  INST(Pfsub            , Ext3dNow           , O(000F0F,9A,_,_,_,_,_,_  ), 0                         , 87 , 0  , 2225 , 143, 50 ), // #534
  INST(Pfsubr           , Ext3dNow           , O(000F0F,AA,_,_,_,_,_,_  ), 0                         , 87 , 0  , 2231 , 143, 50 ), // #535
  INST(Phaddd           , ExtRm_P            , O(000F38,02,_,_,_,_,_,_  ), 0                         , 82 , 0  , 7692 , 139, 84 ), // #536
  INST(Phaddsw          , ExtRm_P            , O(000F38,03,_,_,_,_,_,_  ), 0                         , 82 , 0  , 7709 , 139, 84 ), // #537
  INST(Phaddw           , ExtRm_P            , O(000F38,01,_,_,_,_,_,_  ), 0                         , 82 , 0  , 7778 , 139, 84 ), // #538
  INST(Phminposuw       , ExtRm              , O(660F38,41,_,_,_,_,_,_  ), 0                         , 2  , 0  , 7804 , 5  , 12 ), // #539
  INST(Phsubd           , ExtRm_P            , O(000F38,06,_,_,_,_,_,_  ), 0                         , 82 , 0  , 7825 , 139, 84 ), // #540
  INST(Phsubsw          , ExtRm_P            , O(000F38,07,_,_,_,_,_,_  ), 0                         , 82 , 0  , 7842 , 139, 84 ), // #541
  INST(Phsubw           , ExtRm_P            , O(000F38,05,_,_,_,_,_,_  ), 0                         , 82 , 0  , 7851 , 139, 84 ), // #542
  INST(Pi2fd            , Ext3dNow           , O(000F0F,0D,_,_,_,_,_,_  ), 0                         , 87 , 0  , 2238 , 143, 50 ), // #543
  INST(Pi2fw            , Ext3dNow           , O(000F0F,0C,_,_,_,_,_,_  ), 0                         , 87 , 0  , 2244 , 143, 90 ), // #544
  INST(Pinsrb           , ExtRmi             , O(660F3A,20,_,_,_,_,_,_  ), 0                         , 8  , 0  , 7868 , 152, 12 ), // #545
  INST(Pinsrd           , ExtRmi             , O(660F3A,22,_,_,_,_,_,_  ), 0                         , 8  , 0  , 7876 , 153, 12 ), // #546
  INST(Pinsrq           , ExtRmi             , O(660F3A,22,_,_,1,_,_,_  ), 0                         , 90 , 0  , 7884 , 154, 12 ), // #547
  INST(Pinsrw           , ExtRmi_P           , O(000F00,C4,_,_,_,_,_,_  ), 0                         , 4  , 0  , 7892 , 155, 85 ), // #548
  INST(Pmaddubsw        , ExtRm_P            , O(000F38,04,_,_,_,_,_,_  ), 0                         , 82 , 0  , 8062 , 139, 84 ), // #549
  INST(Pmaddwd          , ExtRm_P            , O(000F00,F5,_,_,_,_,_,_  ), 0                         , 4  , 0  , 8073 , 139, 80 ), // #550
  INST(Pmaxsb           , ExtRm              , O(660F38,3C,_,_,_,_,_,_  ), 0                         , 2  , 0  , 8104 , 11 , 12 ), // #551
  INST(Pmaxsd           , ExtRm              , O(660F38,3D,_,_,_,_,_,_  ), 0                         , 2  , 0  , 8112 , 11 , 12 ), // #552
  INST(Pmaxsw           , ExtRm_P            , O(000F00,EE,_,_,_,_,_,_  ), 0                         , 4  , 0  , 8128 , 141, 85 ), // #553
  INST(Pmaxub           , ExtRm_P            , O(000F00,DE,_,_,_,_,_,_  ), 0                         , 4  , 0  , 8136 , 141, 85 ), // #554
  INST(Pmaxud           , ExtRm              , O(660F38,3F,_,_,_,_,_,_  ), 0                         , 2  , 0  , 8144 , 11 , 12 ), // #555
  INST(Pmaxuw           , ExtRm              , O(660F38,3E,_,_,_,_,_,_  ), 0                         , 2  , 0  , 8160 , 11 , 12 ), // #556
  INST(Pminsb           , ExtRm              , O(660F38,38,_,_,_,_,_,_  ), 0                         , 2  , 0  , 8168 , 11 , 12 ), // #557
  INST(Pminsd           , ExtRm              , O(660F38,39,_,_,_,_,_,_  ), 0                         , 2  , 0  , 8176 , 11 , 12 ), // #558
  INST(Pminsw           , ExtRm_P            , O(000F00,EA,_,_,_,_,_,_  ), 0                         , 4  , 0  , 8192 , 141, 85 ), // #559
  INST(Pminub           , ExtRm_P            , O(000F00,DA,_,_,_,_,_,_  ), 0                         , 4  , 0  , 8200 , 141, 85 ), // #560
  INST(Pminud           , ExtRm              , O(660F38,3B,_,_,_,_,_,_  ), 0                         , 2  , 0  , 8208 , 11 , 12 ), // #561
  INST(Pminuw           , ExtRm              , O(660F38,3A,_,_,_,_,_,_  ), 0                         , 2  , 0  , 8224 , 11 , 12 ), // #562
  INST(Pmovmskb         , ExtRm_P            , O(000F00,D7,_,_,_,_,_,_  ), 0                         , 4  , 0  , 8302 , 156, 85 ), // #563
  INST(Pmovsxbd         , ExtRm              , O(660F38,21,_,_,_,_,_,_  ), 0                         , 2  , 0  , 8399 , 7  , 12 ), // #564
  INST(Pmovsxbq         , ExtRm              , O(660F38,22,_,_,_,_,_,_  ), 0                         , 2  , 0  , 8409 , 157, 12 ), // #565
  INST(Pmovsxbw         , ExtRm              , O(660F38,20,_,_,_,_,_,_  ), 0                         , 2  , 0  , 8419 , 6  , 12 ), // #566
  INST(Pmovsxdq         , ExtRm              , O(660F38,25,_,_,_,_,_,_  ), 0                         , 2  , 0  , 8429 , 6  , 12 ), // #567
  INST(Pmovsxwd         , ExtRm              , O(660F38,23,_,_,_,_,_,_  ), 0                         , 2  , 0  , 8439 , 6  , 12 ), // #568
  INST(Pmovsxwq         , ExtRm              , O(660F38,24,_,_,_,_,_,_  ), 0                         , 2  , 0  , 8449 , 7  , 12 ), // #569
  INST(Pmovzxbd         , ExtRm              , O(660F38,31,_,_,_,_,_,_  ), 0                         , 2  , 0  , 8536 , 7  , 12 ), // #570
  INST(Pmovzxbq         , ExtRm              , O(660F38,32,_,_,_,_,_,_  ), 0                         , 2  , 0  , 8546 , 157, 12 ), // #571
  INST(Pmovzxbw         , ExtRm              , O(660F38,30,_,_,_,_,_,_  ), 0                         , 2  , 0  , 8556 , 6  , 12 ), // #572
  INST(Pmovzxdq         , ExtRm              , O(660F38,35,_,_,_,_,_,_  ), 0                         , 2  , 0  , 8566 , 6  , 12 ), // #573
  INST(Pmovzxwd         , ExtRm              , O(660F38,33,_,_,_,_,_,_  ), 0                         , 2  , 0  , 8576 , 6  , 12 ), // #574
  INST(Pmovzxwq         , ExtRm              , O(660F38,34,_,_,_,_,_,_  ), 0                         , 2  , 0  , 8586 , 7  , 12 ), // #575
  INST(Pmuldq           , ExtRm              , O(660F38,28,_,_,_,_,_,_  ), 0                         , 2  , 0  , 8596 , 5  , 12 ), // #576
  INST(Pmulhrsw         , ExtRm_P            , O(000F38,0B,_,_,_,_,_,_  ), 0                         , 82 , 0  , 8604 , 139, 84 ), // #577
  INST(Pmulhrw          , Ext3dNow           , O(000F0F,B7,_,_,_,_,_,_  ), 0                         , 87 , 0  , 2250 , 143, 50 ), // #578
  INST(Pmulhuw          , ExtRm_P            , O(000F00,E4,_,_,_,_,_,_  ), 0                         , 4  , 0  , 8614 , 139, 85 ), // #579
  INST(Pmulhw           , ExtRm_P            , O(000F00,E5,_,_,_,_,_,_  ), 0                         , 4  , 0  , 8623 , 139, 80 ), // #580
  INST(Pmulld           , ExtRm              , O(660F38,40,_,_,_,_,_,_  ), 0                         , 2  , 0  , 8631 , 5  , 12 ), // #581
  INST(Pmullw           , ExtRm_P            , O(000F00,D5,_,_,_,_,_,_  ), 0                         , 4  , 0  , 8647 , 139, 80 ), // #582
  INST(Pmuludq          , ExtRm_P            , O(000F00,F4,_,_,_,_,_,_  ), 0                         , 4  , 0  , 8670 , 139, 4  ), // #583
  INST(Pop              , X86Pop             , O(000000,8F,0,_,_,_,_,_  ), O(000000,58,_,_,_,_,_,_  ), 0  , 66 , 2258 , 158, 0  ), // #584
  INST(Popa             , X86Op              , O(660000,61,_,_,_,_,_,_  ), 0                         , 19 , 0  , 2262 , 78 , 0  ), // #585
  INST(Popad            , X86Op              , O(000000,61,_,_,_,_,_,_  ), 0                         , 0  , 0  , 2267 , 78 , 0  ), // #586
  INST(Popcnt           , X86Rm_Raw66H       , O(F30F00,B8,_,_,x,_,_,_  ), 0                         , 6  , 0  , 2273 , 22 , 92 ), // #587
  INST(Popf             , X86Op              , O(660000,9D,_,_,_,_,_,_  ), 0                         , 19 , 0  , 2280 , 30 , 93 ), // #588
  INST(Popfd            , X86Op              , O(000000,9D,_,_,_,_,_,_  ), 0                         , 0  , 0  , 2285 , 78 , 93 ), // #589
  INST(Popfq            , X86Op              , O(000000,9D,_,_,_,_,_,_  ), 0                         , 0  , 0  , 2291 , 159, 93 ), // #590
  INST(Por              , ExtRm_P            , O(000F00,EB,_,_,_,_,_,_  ), 0                         , 4  , 0  , 8715 , 141, 80 ), // #591
  INST(Prefetch         , X86M_Only          , O(000F00,0D,0,_,_,_,_,_  ), 0                         , 4  , 0  , 2297 , 31 , 50 ), // #592
  INST(Prefetchnta      , X86M_Only          , O(000F00,18,0,_,_,_,_,_  ), 0                         , 4  , 0  , 2306 , 31 , 75 ), // #593
  INST(Prefetcht0       , X86M_Only          , O(000F00,18,1,_,_,_,_,_  ), 0                         , 28 , 0  , 2318 , 31 , 75 ), // #594
  INST(Prefetcht1       , X86M_Only          , O(000F00,18,2,_,_,_,_,_  ), 0                         , 74 , 0  , 2329 , 31 , 75 ), // #595
  INST(Prefetcht2       , X86M_Only          , O(000F00,18,3,_,_,_,_,_  ), 0                         , 76 , 0  , 2340 , 31 , 75 ), // #596
  INST(Prefetchw        , X86M_Only          , O(000F00,0D,1,_,_,_,_,_  ), 0                         , 28 , 0  , 2351 , 31 , 94 ), // #597
  INST(Prefetchwt1      , X86M_Only          , O(000F00,0D,2,_,_,_,_,_  ), 0                         , 74 , 0  , 2361 , 31 , 95 ), // #598
  INST(Psadbw           , ExtRm_P            , O(000F00,F6,_,_,_,_,_,_  ), 0                         , 4  , 0  , 4268 , 139, 85 ), // #599
  INST(Pshufb           , ExtRm_P            , O(000F38,00,_,_,_,_,_,_  ), 0                         , 82 , 0  , 9041 , 139, 84 ), // #600
  INST(Pshufd           , ExtRmi             , O(660F00,70,_,_,_,_,_,_  ), 0                         , 3  , 0  , 9062 , 8  , 4  ), // #601
  INST(Pshufhw          , ExtRmi             , O(F30F00,70,_,_,_,_,_,_  ), 0                         , 6  , 0  , 9070 , 8  , 4  ), // #602
  INST(Pshuflw          , ExtRmi             , O(F20F00,70,_,_,_,_,_,_  ), 0                         , 5  , 0  , 9079 , 8  , 4  ), // #603
  INST(Pshufw           , ExtRmi_P           , O(000F00,70,_,_,_,_,_,_  ), 0                         , 4  , 0  , 2373 , 160, 75 ), // #604
  INST(Psignb           , ExtRm_P            , O(000F38,08,_,_,_,_,_,_  ), 0                         , 82 , 0  , 9088 , 139, 84 ), // #605
  INST(Psignd           , ExtRm_P            , O(000F38,0A,_,_,_,_,_,_  ), 0                         , 82 , 0  , 9096 , 139, 84 ), // #606
  INST(Psignw           , ExtRm_P            , O(000F38,09,_,_,_,_,_,_  ), 0                         , 82 , 0  , 9104 , 139, 84 ), // #607
  INST(Pslld            , ExtRmRi_P          , O(000F00,F2,_,_,_,_,_,_  ), O(000F00,72,6,_,_,_,_,_  ), 4  , 67 , 9112 , 161, 80 ), // #608
  INST(Pslldq           , ExtRmRi            , 0                         , O(660F00,73,7,_,_,_,_,_  ), 0  , 68 , 9119 , 162, 4  ), // #609
  INST(Psllq            , ExtRmRi_P          , O(000F00,F3,_,_,_,_,_,_  ), O(000F00,73,6,_,_,_,_,_  ), 4  , 69 , 9127 , 161, 80 ), // #610
  INST(Psllw            , ExtRmRi_P          , O(000F00,F1,_,_,_,_,_,_  ), O(000F00,71,6,_,_,_,_,_  ), 4  , 70 , 9158 , 161, 80 ), // #611
  INST(Psmash           , X86Op              , O(F30F01,FF,_,_,_,_,_,_  ), 0                         , 81 , 0  , 2380 , 159, 96 ), // #612
  INST(Psrad            , ExtRmRi_P          , O(000F00,E2,_,_,_,_,_,_  ), O(000F00,72,4,_,_,_,_,_  ), 4  , 71 , 9165 , 161, 80 ), // #613
  INST(Psraw            , ExtRmRi_P          , O(000F00,E1,_,_,_,_,_,_  ), O(000F00,71,4,_,_,_,_,_  ), 4  , 72 , 9203 , 161, 80 ), // #614
  INST(Psrld            , ExtRmRi_P          , O(000F00,D2,_,_,_,_,_,_  ), O(000F00,72,2,_,_,_,_,_  ), 4  , 73 , 9210 , 161, 80 ), // #615
  INST(Psrldq           , ExtRmRi            , 0                         , O(660F00,73,3,_,_,_,_,_  ), 0  , 74 , 9217 , 162, 4  ), // #616
  INST(Psrlq            , ExtRmRi_P          , O(000F00,D3,_,_,_,_,_,_  ), O(000F00,73,2,_,_,_,_,_  ), 4  , 75 , 9225 , 161, 80 ), // #617
  INST(Psrlw            , ExtRmRi_P          , O(000F00,D1,_,_,_,_,_,_  ), O(000F00,71,2,_,_,_,_,_  ), 4  , 76 , 9256 , 161, 80 ), // #618
  INST(Psubb            , ExtRm_P            , O(000F00,F8,_,_,_,_,_,_  ), 0                         , 4  , 0  , 9263 , 142, 80 ), // #619
  INST(Psubd            , ExtRm_P            , O(000F00,FA,_,_,_,_,_,_  ), 0                         , 4  , 0  , 9270 , 142, 80 ), // #620
  INST(Psubq            , ExtRm_P            , O(000F00,FB,_,_,_,_,_,_  ), 0                         , 4  , 0  , 9277 , 142, 4  ), // #621
  INST(Psubsb           , ExtRm_P            , O(000F00,E8,_,_,_,_,_,_  ), 0                         , 4  , 0  , 9284 , 142, 80 ), // #622
  INST(Psubsw           , ExtRm_P            , O(000F00,E9,_,_,_,_,_,_  ), 0                         , 4  , 0  , 9292 , 142, 80 ), // #623
  INST(Psubusb          , ExtRm_P            , O(000F00,D8,_,_,_,_,_,_  ), 0                         , 4  , 0  , 9300 , 142, 80 ), // #624
  INST(Psubusw          , ExtRm_P            , O(000F00,D9,_,_,_,_,_,_  ), 0                         , 4  , 0  , 9309 , 142, 80 ), // #625
  INST(Psubw            , ExtRm_P            , O(000F00,F9,_,_,_,_,_,_  ), 0                         , 4  , 0  , 9318 , 142, 80 ), // #626
  INST(Pswapd           , Ext3dNow           , O(000F0F,BB,_,_,_,_,_,_  ), 0                         , 87 , 0  , 2387 , 143, 90 ), // #627
  INST(Ptest            , ExtRm              , O(660F38,17,_,_,_,_,_,_  ), 0                         , 2  , 0  , 9347 , 5  , 97 ), // #628
  INST(Ptwrite          , X86M               , O(F30F00,AE,4,_,_,_,_,_  ), 0                         , 91 , 0  , 2394 , 163, 98 ), // #629
  INST(Punpckhbw        , ExtRm_P            , O(000F00,68,_,_,_,_,_,_  ), 0                         , 4  , 0  , 9430 , 139, 80 ), // #630
  INST(Punpckhdq        , ExtRm_P            , O(000F00,6A,_,_,_,_,_,_  ), 0                         , 4  , 0  , 9441 , 139, 80 ), // #631
  INST(Punpckhqdq       , ExtRm              , O(660F00,6D,_,_,_,_,_,_  ), 0                         , 3  , 0  , 9452 , 5  , 4  ), // #632
  INST(Punpckhwd        , ExtRm_P            , O(000F00,69,_,_,_,_,_,_  ), 0                         , 4  , 0  , 9464 , 139, 80 ), // #633
  INST(Punpcklbw        , ExtRm_P            , O(000F00,60,_,_,_,_,_,_  ), 0                         , 4  , 0  , 9475 , 139, 80 ), // #634
  INST(Punpckldq        , ExtRm_P            , O(000F00,62,_,_,_,_,_,_  ), 0                         , 4  , 0  , 9486 , 139, 80 ), // #635
  INST(Punpcklqdq       , ExtRm              , O(660F00,6C,_,_,_,_,_,_  ), 0                         , 3  , 0  , 9497 , 5  , 4  ), // #636
  INST(Punpcklwd        , ExtRm_P            , O(000F00,61,_,_,_,_,_,_  ), 0                         , 4  , 0  , 9509 , 139, 80 ), // #637
  INST(Push             , X86Push            , O(000000,FF,6,_,_,_,_,_  ), O(000000,50,_,_,_,_,_,_  ), 31 , 77 , 2402 , 164, 0  ), // #638
  INST(Pusha            , X86Op              , O(660000,60,_,_,_,_,_,_  ), 0                         , 19 , 0  , 2407 , 78 , 0  ), // #639
  INST(Pushad           , X86Op              , O(000000,60,_,_,_,_,_,_  ), 0                         , 0  , 0  , 2413 , 78 , 0  ), // #640
  INST(Pushf            , X86Op              , O(660000,9C,_,_,_,_,_,_  ), 0                         , 19 , 0  , 2420 , 30 , 99 ), // #641
  INST(Pushfd           , X86Op              , O(000000,9C,_,_,_,_,_,_  ), 0                         , 0  , 0  , 2426 , 78 , 99 ), // #642
  INST(Pushfq           , X86Op              , O(000000,9C,_,_,_,_,_,_  ), 0                         , 0  , 0  , 2433 , 159, 99 ), // #643
  INST(Pvalidate        , X86Op              , O(F20F01,FF,_,_,_,_,_,_  ), 0                         , 92 , 0  , 2440 , 30 , 100), // #644
  INST(Pxor             , ExtRm_P            , O(000F00,EF,_,_,_,_,_,_  ), 0                         , 4  , 0  , 9520 , 142, 80 ), // #645
  INST(Rcl              , X86Rot             , O(000000,D0,2,_,x,_,_,_  ), 0                         , 1  , 0  , 2450 , 165, 101), // #646
  INST(Rcpps            , ExtRm              , O(000F00,53,_,_,_,_,_,_  ), 0                         , 4  , 0  , 9648 , 5  , 5  ), // #647
  INST(Rcpss            , ExtRm              , O(F30F00,53,_,_,_,_,_,_  ), 0                         , 6  , 0  , 9655 , 7  , 5  ), // #648
  INST(Rcr              , X86Rot             , O(000000,D0,3,_,x,_,_,_  ), 0                         , 84 , 0  , 2454 , 165, 101), // #649
  INST(Rdfsbase         , X86M               , O(F30F00,AE,0,_,x,_,_,_  ), 0                         , 6  , 0  , 2458 , 166, 102), // #650
  INST(Rdgsbase         , X86M               , O(F30F00,AE,1,_,x,_,_,_  ), 0                         , 93 , 0  , 2467 , 166, 102), // #651
  INST(Rdmsr            , X86Op              , O(000F00,32,_,_,_,_,_,_  ), 0                         , 4  , 0  , 2476 , 167, 103), // #652
  INST(Rdpid            , X86R_Native        , O(F30F00,C7,7,_,_,_,_,_  ), 0                         , 94 , 0  , 2482 , 168, 104), // #653
  INST(Rdpkru           , X86Op              , O(000F01,EE,_,_,_,_,_,_  ), 0                         , 21 , 0  , 2488 , 167, 105), // #654
  INST(Rdpmc            , X86Op              , O(000F00,33,_,_,_,_,_,_  ), 0                         , 4  , 0  , 2495 , 167, 0  ), // #655
  INST(Rdpru            , X86Op              , O(000F01,FD,_,_,_,_,_,_  ), 0                         , 21 , 0  , 2501 , 167, 106), // #656
  INST(Rdrand           , X86M               , O(000F00,C7,6,_,x,_,_,_  ), 0                         , 78 , 0  , 2507 , 23 , 107), // #657
  INST(Rdseed           , X86M               , O(000F00,C7,7,_,x,_,_,_  ), 0                         , 22 , 0  , 2514 , 23 , 108), // #658
  INST(Rdsspd           , X86M               , O(F30F00,1E,1,_,_,_,_,_  ), 0                         , 93 , 0  , 2521 , 73 , 54 ), // #659
  INST(Rdsspq           , X86M               , O(F30F00,1E,1,_,_,_,_,_  ), 0                         , 93 , 0  , 2528 , 74 , 54 ), // #660
  INST(Rdtsc            , X86Op              , O(000F00,31,_,_,_,_,_,_  ), 0                         , 4  , 0  , 2535 , 28 , 109), // #661
  INST(Rdtscp           , X86Op              , O(000F01,F9,_,_,_,_,_,_  ), 0                         , 21 , 0  , 2541 , 167, 110), // #662
  INST(Ret              , X86Ret             , O(000000,C2,_,_,_,_,_,_  ), 0                         , 0  , 0  , 3044 , 169, 0  ), // #663
  INST(Rmpadjust        , X86Op              , O(F30F01,FE,_,_,_,_,_,_  ), 0                         , 81 , 0  , 2548 , 159, 96 ), // #664
  INST(Rmpupdate        , X86Op              , O(F20F01,FE,_,_,_,_,_,_  ), 0                         , 92 , 0  , 2558 , 159, 96 ), // #665
  INST(Rol              , X86Rot             , O(000000,D0,0,_,x,_,_,_  ), 0                         , 0  , 0  , 2568 , 165, 111), // #666
  INST(Ror              , X86Rot             , O(000000,D0,1,_,x,_,_,_  ), 0                         , 30 , 0  , 2572 , 165, 111), // #667
  INST(Rorx             , VexRmi_Wx          , V(F20F3A,F0,_,0,x,_,_,_  ), 0                         , 95 , 0  , 2576 , 170, 83 ), // #668
  INST(Roundpd          , ExtRmi             , O(660F3A,09,_,_,_,_,_,_  ), 0                         , 8  , 0  , 9750 , 8  , 12 ), // #669
  INST(Roundps          , ExtRmi             , O(660F3A,08,_,_,_,_,_,_  ), 0                         , 8  , 0  , 9759 , 8  , 12 ), // #670
  INST(Roundsd          , ExtRmi             , O(660F3A,0B,_,_,_,_,_,_  ), 0                         , 8  , 0  , 9768 , 36 , 12 ), // #671
  INST(Roundss          , ExtRmi             , O(660F3A,0A,_,_,_,_,_,_  ), 0                         , 8  , 0  , 9777 , 37 , 12 ), // #672
  INST(Rsm              , X86Op              , O(000F00,AA,_,_,_,_,_,_  ), 0                         , 4  , 0  , 2581 , 78 , 1  ), // #673
  INST(Rsqrtps          , ExtRm              , O(000F00,52,_,_,_,_,_,_  ), 0                         , 4  , 0  , 9874 , 5  , 5  ), // #674
  INST(Rsqrtss          , ExtRm              , O(F30F00,52,_,_,_,_,_,_  ), 0                         , 6  , 0  , 9883 , 7  , 5  ), // #675
  INST(Rstorssp         , X86M               , O(F30F00,01,5,_,_,_,_,_  ), 0                         , 62 , 0  , 2585 , 32 , 24 ), // #676
  INST(Sahf             , X86Op              , O(000000,9E,_,_,_,_,_,_  ), 0                         , 0  , 0  , 2594 , 93 , 112), // #677
  INST(Sal              , X86Rot             , O(000000,D0,4,_,x,_,_,_  ), 0                         , 9  , 0  , 2599 , 165, 1  ), // #678
  INST(Sar              , X86Rot             , O(000000,D0,7,_,x,_,_,_  ), 0                         , 26 , 0  , 2603 , 165, 1  ), // #679
  INST(Sarx             , VexRmv_Wx          , V(F30F38,F7,_,0,x,_,_,_  ), 0                         , 88 , 0  , 2607 , 13 , 83 ), // #680
  INST(Saveprevssp      , X86Op              , O(F30F01,EA,_,_,_,_,_,_  ), 0                         , 81 , 0  , 2612 , 30 , 24 ), // #681
  INST(Sbb              , X86Arith           , O(000000,18,3,_,x,_,_,_  ), 0                         , 84 , 0  , 2624 , 171, 2  ), // #682
  INST(Scas             , X86StrRm           , O(000000,AE,_,_,_,_,_,_  ), 0                         , 0  , 0  , 2628 , 172, 36 ), // #683
  INST(Serialize        , X86Op              , O(000F01,E8,_,_,_,_,_,_  ), 0                         , 21 , 0  , 2633 , 30 , 113), // #684
  INST(Seta             , X86Set             , O(000F00,97,_,_,_,_,_,_  ), 0                         , 4  , 0  , 2643 , 173, 57 ), // #685
  INST(Setae            , X86Set             , O(000F00,93,_,_,_,_,_,_  ), 0                         , 4  , 0  , 2648 , 173, 58 ), // #686
  INST(Setb             , X86Set             , O(000F00,92,_,_,_,_,_,_  ), 0                         , 4  , 0  , 2654 , 173, 58 ), // #687
  INST(Setbe            , X86Set             , O(000F00,96,_,_,_,_,_,_  ), 0                         , 4  , 0  , 2659 , 173, 57 ), // #688
  INST(Setc             , X86Set             , O(000F00,92,_,_,_,_,_,_  ), 0                         , 4  , 0  , 2665 , 173, 58 ), // #689
  INST(Sete             , X86Set             , O(000F00,94,_,_,_,_,_,_  ), 0                         , 4  , 0  , 2670 , 173, 59 ), // #690
  INST(Setg             , X86Set             , O(000F00,9F,_,_,_,_,_,_  ), 0                         , 4  , 0  , 2675 , 173, 60 ), // #691
  INST(Setge            , X86Set             , O(000F00,9D,_,_,_,_,_,_  ), 0                         , 4  , 0  , 2680 , 173, 61 ), // #692
  INST(Setl             , X86Set             , O(000F00,9C,_,_,_,_,_,_  ), 0                         , 4  , 0  , 2686 , 173, 61 ), // #693
  INST(Setle            , X86Set             , O(000F00,9E,_,_,_,_,_,_  ), 0                         , 4  , 0  , 2691 , 173, 60 ), // #694
  INST(Setna            , X86Set             , O(000F00,96,_,_,_,_,_,_  ), 0                         , 4  , 0  , 2697 , 173, 57 ), // #695
  INST(Setnae           , X86Set             , O(000F00,92,_,_,_,_,_,_  ), 0                         , 4  , 0  , 2703 , 173, 58 ), // #696
  INST(Setnb            , X86Set             , O(000F00,93,_,_,_,_,_,_  ), 0                         , 4  , 0  , 2710 , 173, 58 ), // #697
  INST(Setnbe           , X86Set             , O(000F00,97,_,_,_,_,_,_  ), 0                         , 4  , 0  , 2716 , 173, 57 ), // #698
  INST(Setnc            , X86Set             , O(000F00,93,_,_,_,_,_,_  ), 0                         , 4  , 0  , 2723 , 173, 58 ), // #699
  INST(Setne            , X86Set             , O(000F00,95,_,_,_,_,_,_  ), 0                         , 4  , 0  , 2729 , 173, 59 ), // #700
  INST(Setng            , X86Set             , O(000F00,9E,_,_,_,_,_,_  ), 0                         , 4  , 0  , 2735 , 173, 60 ), // #701
  INST(Setnge           , X86Set             , O(000F00,9C,_,_,_,_,_,_  ), 0                         , 4  , 0  , 2741 , 173, 61 ), // #702
  INST(Setnl            , X86Set             , O(000F00,9D,_,_,_,_,_,_  ), 0                         , 4  , 0  , 2748 , 173, 61 ), // #703
  INST(Setnle           , X86Set             , O(000F00,9F,_,_,_,_,_,_  ), 0                         , 4  , 0  , 2754 , 173, 60 ), // #704
  INST(Setno            , X86Set             , O(000F00,91,_,_,_,_,_,_  ), 0                         , 4  , 0  , 2761 , 173, 55 ), // #705
  INST(Setnp            , X86Set             , O(000F00,9B,_,_,_,_,_,_  ), 0                         , 4  , 0  , 2767 , 173, 62 ), // #706
  INST(Setns            , X86Set             , O(000F00,99,_,_,_,_,_,_  ), 0                         , 4  , 0  , 2773 , 173, 63 ), // #707
  INST(Setnz            , X86Set             , O(000F00,95,_,_,_,_,_,_  ), 0                         , 4  , 0  , 2779 , 173, 59 ), // #708
  INST(Seto             , X86Set             , O(000F00,90,_,_,_,_,_,_  ), 0                         , 4  , 0  , 2785 , 173, 55 ), // #709
  INST(Setp             , X86Set             , O(000F00,9A,_,_,_,_,_,_  ), 0                         , 4  , 0  , 2790 , 173, 62 ), // #710
  INST(Setpe            , X86Set             , O(000F00,9A,_,_,_,_,_,_  ), 0                         , 4  , 0  , 2795 , 173, 62 ), // #711
  INST(Setpo            , X86Set             , O(000F00,9B,_,_,_,_,_,_  ), 0                         , 4  , 0  , 2801 , 173, 62 ), // #712
  INST(Sets             , X86Set             , O(000F00,98,_,_,_,_,_,_  ), 0                         , 4  , 0  , 2807 , 173, 63 ), // #713
  INST(Setssbsy         , X86Op              , O(F30F01,E8,_,_,_,_,_,_  ), 0                         , 81 , 0  , 2812 , 30 , 54 ), // #714
  INST(Setz             , X86Set             , O(000F00,94,_,_,_,_,_,_  ), 0                         , 4  , 0  , 2821 , 173, 59 ), // #715
  INST(Sfence           , X86Fence           , O(000F00,AE,7,_,_,_,_,_  ), 0                         , 22 , 0  , 2826 , 30 , 75 ), // #716
  INST(Sgdt             , X86M_Only          , O(000F00,01,0,_,_,_,_,_  ), 0                         , 4  , 0  , 2833 , 31 , 0  ), // #717
  INST(Sha1msg1         , ExtRm              , O(000F38,C9,_,_,_,_,_,_  ), 0                         , 82 , 0  , 2838 , 5  , 114), // #718
  INST(Sha1msg2         , ExtRm              , O(000F38,CA,_,_,_,_,_,_  ), 0                         , 82 , 0  , 2847 , 5  , 114), // #719
  INST(Sha1nexte        , ExtRm              , O(000F38,C8,_,_,_,_,_,_  ), 0                         , 82 , 0  , 2856 , 5  , 114), // #720
  INST(Sha1rnds4        , ExtRmi             , O(000F3A,CC,_,_,_,_,_,_  ), 0                         , 85 , 0  , 2866 , 8  , 114), // #721
  INST(Sha256msg1       , ExtRm              , O(000F38,CC,_,_,_,_,_,_  ), 0                         , 82 , 0  , 2876 , 5  , 114), // #722
  INST(Sha256msg2       , ExtRm              , O(000F38,CD,_,_,_,_,_,_  ), 0                         , 82 , 0  , 2887 , 5  , 114), // #723
  INST(Sha256rnds2      , ExtRm_XMM0         , O(000F38,CB,_,_,_,_,_,_  ), 0                         , 82 , 0  , 2898 , 15 , 114), // #724
  INST(Shl              , X86Rot             , O(000000,D0,4,_,x,_,_,_  ), 0                         , 9  , 0  , 2910 , 165, 1  ), // #725
  INST(Shld             , X86ShldShrd        , O(000F00,A4,_,_,x,_,_,_  ), 0                         , 4  , 0  , 8919 , 174, 1  ), // #726
  INST(Shlx             , VexRmv_Wx          , V(660F38,F7,_,0,x,_,_,_  ), 0                         , 96 , 0  , 2914 , 13 , 83 ), // #727
  INST(Shr              , X86Rot             , O(000000,D0,5,_,x,_,_,_  ), 0                         , 61 , 0  , 2919 , 165, 1  ), // #728
  INST(Shrd             , X86ShldShrd        , O(000F00,AC,_,_,x,_,_,_  ), 0                         , 4  , 0  , 2923 , 174, 1  ), // #729
  INST(Shrx             , VexRmv_Wx          , V(F20F38,F7,_,0,x,_,_,_  ), 0                         , 83 , 0  , 2928 , 13 , 83 ), // #730
  INST(Shufpd           , ExtRmi             , O(660F00,C6,_,_,_,_,_,_  ), 0                         , 3  , 0  , 10144, 8  , 4  ), // #731
  INST(Shufps           , ExtRmi             , O(000F00,C6,_,_,_,_,_,_  ), 0                         , 4  , 0  , 10152, 8  , 5  ), // #732
  INST(Sidt             , X86M_Only          , O(000F00,01,1,_,_,_,_,_  ), 0                         , 28 , 0  , 2933 , 31 , 0  ), // #733
  INST(Skinit           , X86Op_xAX          , O(000F01,DE,_,_,_,_,_,_  ), 0                         , 21 , 0  , 2938 , 51 , 115), // #734
  INST(Sldt             , X86M               , O(000F00,00,0,_,_,_,_,_  ), 0                         , 4  , 0  , 2945 , 175, 0  ), // #735
  INST(Slwpcb           , VexR_Wx            , V(XOP_M9,12,1,0,x,_,_,_  ), 0                         , 11 , 0  , 2950 , 102, 72 ), // #736
  INST(Smsw             , X86M               , O(000F00,01,4,_,_,_,_,_  ), 0                         , 97 , 0  , 2957 , 175, 0  ), // #737
  INST(Sqrtpd           , ExtRm              , O(660F00,51,_,_,_,_,_,_  ), 0                         , 3  , 0  , 10160, 5  , 4  ), // #738
  INST(Sqrtps           , ExtRm              , O(000F00,51,_,_,_,_,_,_  ), 0                         , 4  , 0  , 9875 , 5  , 5  ), // #739
  INST(Sqrtsd           , ExtRm              , O(F20F00,51,_,_,_,_,_,_  ), 0                         , 5  , 0  , 10176, 6  , 4  ), // #740
  INST(Sqrtss           , ExtRm              , O(F30F00,51,_,_,_,_,_,_  ), 0                         , 6  , 0  , 9884 , 7  , 5  ), // #741
  INST(Stac             , X86Op              , O(000F01,CB,_,_,_,_,_,_  ), 0                         , 21 , 0  , 2962 , 30 , 16 ), // #742
  INST(Stc              , X86Op              , O(000000,F9,_,_,_,_,_,_  ), 0                         , 0  , 0  , 2967 , 30 , 17 ), // #743
  INST(Std              , X86Op              , O(000000,FD,_,_,_,_,_,_  ), 0                         , 0  , 0  , 6902 , 30 , 18 ), // #744
  INST(Stgi             , X86Op              , O(000F01,DC,_,_,_,_,_,_  ), 0                         , 21 , 0  , 2971 , 30 , 115), // #745
  INST(Sti              , X86Op              , O(000000,FB,_,_,_,_,_,_  ), 0                         , 0  , 0  , 2976 , 30 , 23 ), // #746
  INST(Stmxcsr          , X86M_Only          , O(000F00,AE,3,_,_,_,_,_  ), 0                         , 76 , 0  , 10192, 96 , 5  ), // #747
  INST(Stos             , X86StrMr           , O(000000,AA,_,_,_,_,_,_  ), 0                         , 0  , 0  , 2980 , 176, 73 ), // #748
  INST(Str              , X86M               , O(000F00,00,1,_,_,_,_,_  ), 0                         , 28 , 0  , 2985 , 175, 0  ), // #749
  INST(Sttilecfg        , AmxCfg             , V(660F38,49,_,0,0,_,_,_  ), 0                         , 96 , 0  , 2989 , 98 , 71 ), // #750
  INST(Sub              , X86Arith           , O(000000,28,5,_,x,_,_,_  ), 0                         , 61 , 0  , 861  , 171, 1  ), // #751
  INST(Subpd            , ExtRm              , O(660F00,5C,_,_,_,_,_,_  ), 0                         , 3  , 0  , 4844 , 5  , 4  ), // #752
  INST(Subps            , ExtRm              , O(000F00,5C,_,_,_,_,_,_  ), 0                         , 4  , 0  , 4856 , 5  , 5  ), // #753
  INST(Subsd            , ExtRm              , O(F20F00,5C,_,_,_,_,_,_  ), 0                         , 5  , 0  , 5532 , 6  , 4  ), // #754
  INST(Subss            , ExtRm              , O(F30F00,5C,_,_,_,_,_,_  ), 0                         , 6  , 0  , 5542 , 7  , 5  ), // #755
  INST(Swapgs           , X86Op              , O(000F01,F8,_,_,_,_,_,_  ), 0                         , 21 , 0  , 2999 , 159, 0  ), // #756
  INST(Syscall          , X86Op              , O(000F00,05,_,_,_,_,_,_  ), 0                         , 4  , 0  , 3006 , 159, 0  ), // #757
  INST(Sysenter         , X86Op              , O(000F00,34,_,_,_,_,_,_  ), 0                         , 4  , 0  , 3014 , 30 , 0  ), // #758
  INST(Sysexit          , X86Op              , O(000F00,35,_,_,_,_,_,_  ), 0                         , 4  , 0  , 3023 , 30 , 0  ), // #759
  INST(Sysexit64        , X86Op              , O(000F00,35,_,_,_,_,_,_  ), 0                         , 4  , 0  , 3031 , 30 , 0  ), // #760
  INST(Sysret           , X86Op              , O(000F00,07,_,_,_,_,_,_  ), 0                         , 4  , 0  , 3041 , 159, 0  ), // #761
  INST(Sysret64         , X86Op              , O(000F00,07,_,_,_,_,_,_  ), 0                         , 4  , 0  , 3048 , 159, 0  ), // #762
  INST(T1mskc           , VexVm_Wx           , V(XOP_M9,01,7,0,x,_,_,_  ), 0                         , 98 , 0  , 3057 , 14 , 11 ), // #763
  INST(Tdpbf16ps        , AmxRmv             , V(F30F38,5C,_,0,0,_,_,_  ), 0                         , 88 , 0  , 3064 , 177, 116), // #764
  INST(Tdpbssd          , AmxRmv             , V(F20F38,5E,_,0,0,_,_,_  ), 0                         , 83 , 0  , 3074 , 177, 117), // #765
  INST(Tdpbsud          , AmxRmv             , V(F30F38,5E,_,0,0,_,_,_  ), 0                         , 88 , 0  , 3082 , 177, 117), // #766
  INST(Tdpbusd          , AmxRmv             , V(660F38,5E,_,0,0,_,_,_  ), 0                         , 96 , 0  , 3090 , 177, 117), // #767
  INST(Tdpbuud          , AmxRmv             , V(000F38,5E,_,0,0,_,_,_  ), 0                         , 10 , 0  , 3098 , 177, 117), // #768
  INST(Test             , X86Test            , O(000000,84,_,_,x,_,_,_  ), O(000000,F6,_,_,x,_,_,_  ), 0  , 78 , 9348 , 178, 1  ), // #769
  INST(Tileloadd        , AmxRm              , V(F20F38,4B,_,0,0,_,_,_  ), 0                         , 83 , 0  , 3106 , 179, 71 ), // #770
  INST(Tileloaddt1      , AmxRm              , V(660F38,4B,_,0,0,_,_,_  ), 0                         , 96 , 0  , 3116 , 179, 71 ), // #771
  INST(Tilerelease      , VexOpMod           , V(000F38,49,0,0,0,_,_,_  ), 0                         , 10 , 0  , 3128 , 180, 71 ), // #772
  INST(Tilestored       , AmxMr              , V(F30F38,4B,_,0,0,_,_,_  ), 0                         , 88 , 0  , 3140 , 181, 71 ), // #773
  INST(Tilezero         , AmxR               , V(F20F38,49,_,0,0,_,_,_  ), 0                         , 83 , 0  , 3151 , 182, 71 ), // #774
  INST(Tpause           , X86R32_EDX_EAX     , O(660F00,AE,6,_,_,_,_,_  ), 0                         , 25 , 0  , 3160 , 183, 118), // #775
  INST(Tzcnt            , X86Rm_Raw66H       , O(F30F00,BC,_,_,x,_,_,_  ), 0                         , 6  , 0  , 3167 , 22 , 9  ), // #776
  INST(Tzmsk            , VexVm_Wx           , V(XOP_M9,01,4,0,x,_,_,_  ), 0                         , 99 , 0  , 3173 , 14 , 11 ), // #777
  INST(Ucomisd          , ExtRm              , O(660F00,2E,_,_,_,_,_,_  ), 0                         , 3  , 0  , 10245, 6  , 40 ), // #778
  INST(Ucomiss          , ExtRm              , O(000F00,2E,_,_,_,_,_,_  ), 0                         , 4  , 0  , 10254, 7  , 41 ), // #779
  INST(Ud0              , X86M               , O(000F00,FF,_,_,_,_,_,_  ), 0                         , 4  , 0  , 3179 , 184, 0  ), // #780
  INST(Ud1              , X86M               , O(000F00,B9,_,_,_,_,_,_  ), 0                         , 4  , 0  , 3183 , 184, 0  ), // #781
  INST(Ud2              , X86Op              , O(000F00,0B,_,_,_,_,_,_  ), 0                         , 4  , 0  , 3187 , 30 , 0  ), // #782
  INST(Umonitor         , X86R_FromM         , O(F30F00,AE,6,_,_,_,_,_  ), 0                         , 24 , 0  , 3191 , 185, 119), // #783
  INST(Umwait           , X86R32_EDX_EAX     , O(F20F00,AE,6,_,_,_,_,_  ), 0                         , 100, 0  , 3200 , 183, 118), // #784
  INST(Unpckhpd         , ExtRm              , O(660F00,15,_,_,_,_,_,_  ), 0                         , 3  , 0  , 10263, 5  , 4  ), // #785
  INST(Unpckhps         , ExtRm              , O(000F00,15,_,_,_,_,_,_  ), 0                         , 4  , 0  , 10273, 5  , 5  ), // #786
  INST(Unpcklpd         , ExtRm              , O(660F00,14,_,_,_,_,_,_  ), 0                         , 3  , 0  , 10283, 5  , 4  ), // #787
  INST(Unpcklps         , ExtRm              , O(000F00,14,_,_,_,_,_,_  ), 0                         , 4  , 0  , 10293, 5  , 5  ), // #788
  INST(V4fmaddps        , VexRm_T1_4X        , E(F20F38,9A,_,2,_,0,2,T4X), 0                         , 101, 0  , 3207 , 186, 120), // #789
  INST(V4fmaddss        , VexRm_T1_4X        , E(F20F38,9B,_,2,_,0,2,T4X), 0                         , 101, 0  , 3217 , 187, 120), // #790
  INST(V4fnmaddps       , VexRm_T1_4X        , E(F20F38,AA,_,2,_,0,2,T4X), 0                         , 101, 0  , 3227 , 186, 120), // #791
  INST(V4fnmaddss       , VexRm_T1_4X        , E(F20F38,AB,_,2,_,0,2,T4X), 0                         , 101, 0  , 3238 , 187, 120), // #792
  INST(Vaddpd           , VexRvm_Lx          , V(660F00,58,_,x,I,1,4,FV ), 0                         , 102, 0  , 3249 , 188, 121), // #793
  INST(Vaddps           , VexRvm_Lx          , V(000F00,58,_,x,I,0,4,FV ), 0                         , 103, 0  , 3256 , 189, 121), // #794
  INST(Vaddsd           , VexRvm             , V(F20F00,58,_,I,I,1,3,T1S), 0                         , 104, 0  , 3263 , 190, 122), // #795
  INST(Vaddss           , VexRvm             , V(F30F00,58,_,I,I,0,2,T1S), 0                         , 105, 0  , 3270 , 191, 122), // #796
  INST(Vaddsubpd        , VexRvm_Lx          , V(660F00,D0,_,x,I,_,_,_  ), 0                         , 68 , 0  , 3277 , 192, 123), // #797
  INST(Vaddsubps        , VexRvm_Lx          , V(F20F00,D0,_,x,I,_,_,_  ), 0                         , 106, 0  , 3287 , 192, 123), // #798
  INST(Vaesdec          , VexRvm_Lx          , V(660F38,DE,_,x,I,_,4,FVM), 0                         , 107, 0  , 3297 , 193, 124), // #799
  INST(Vaesdeclast      , VexRvm_Lx          , V(660F38,DF,_,x,I,_,4,FVM), 0                         , 107, 0  , 3305 , 193, 124), // #800
  INST(Vaesenc          , VexRvm_Lx          , V(660F38,DC,_,x,I,_,4,FVM), 0                         , 107, 0  , 3317 , 193, 124), // #801
  INST(Vaesenclast      , VexRvm_Lx          , V(660F38,DD,_,x,I,_,4,FVM), 0                         , 107, 0  , 3325 , 193, 124), // #802
  INST(Vaesimc          , VexRm              , V(660F38,DB,_,0,I,_,_,_  ), 0                         , 96 , 0  , 3337 , 194, 125), // #803
  INST(Vaeskeygenassist , VexRmi             , V(660F3A,DF,_,0,I,_,_,_  ), 0                         , 72 , 0  , 3345 , 195, 125), // #804
  INST(Valignd          , VexRvmi_Lx         , E(660F3A,03,_,x,_,0,4,FV ), 0                         , 108, 0  , 3362 , 196, 126), // #805
  INST(Valignq          , VexRvmi_Lx         , E(660F3A,03,_,x,_,1,4,FV ), 0                         , 109, 0  , 3370 , 197, 126), // #806
  INST(Vandnpd          , VexRvm_Lx          , V(660F00,55,_,x,I,1,4,FV ), 0                         , 102, 0  , 3378 , 198, 127), // #807
  INST(Vandnps          , VexRvm_Lx          , V(000F00,55,_,x,I,0,4,FV ), 0                         , 103, 0  , 3386 , 199, 127), // #808
  INST(Vandpd           , VexRvm_Lx          , V(660F00,54,_,x,I,1,4,FV ), 0                         , 102, 0  , 3394 , 200, 127), // #809
  INST(Vandps           , VexRvm_Lx          , V(000F00,54,_,x,I,0,4,FV ), 0                         , 103, 0  , 3401 , 201, 127), // #810
  INST(Vblendmb         , VexRvm_Lx          , E(660F38,66,_,x,_,0,4,FVM), 0                         , 110, 0  , 3408 , 202, 128), // #811
  INST(Vblendmd         , VexRvm_Lx          , E(660F38,64,_,x,_,0,4,FV ), 0                         , 111, 0  , 3417 , 203, 126), // #812
  INST(Vblendmpd        , VexRvm_Lx          , E(660F38,65,_,x,_,1,4,FV ), 0                         , 112, 0  , 3426 , 204, 126), // #813
  INST(Vblendmps        , VexRvm_Lx          , E(660F38,65,_,x,_,0,4,FV ), 0                         , 111, 0  , 3436 , 203, 126), // #814
  INST(Vblendmq         , VexRvm_Lx          , E(660F38,64,_,x,_,1,4,FV ), 0                         , 112, 0  , 3446 , 204, 126), // #815
  INST(Vblendmw         , VexRvm_Lx          , E(660F38,66,_,x,_,1,4,FVM), 0                         , 113, 0  , 3455 , 202, 128), // #816
  INST(Vblendpd         , VexRvmi_Lx         , V(660F3A,0D,_,x,I,_,_,_  ), 0                         , 72 , 0  , 3464 , 205, 123), // #817
  INST(Vblendps         , VexRvmi_Lx         , V(660F3A,0C,_,x,I,_,_,_  ), 0                         , 72 , 0  , 3473 , 205, 123), // #818
  INST(Vblendvpd        , VexRvmr_Lx         , V(660F3A,4B,_,x,0,_,_,_  ), 0                         , 72 , 0  , 3482 , 206, 123), // #819
  INST(Vblendvps        , VexRvmr_Lx         , V(660F3A,4A,_,x,0,_,_,_  ), 0                         , 72 , 0  , 3492 , 206, 123), // #820
  INST(Vbroadcastf128   , VexRm              , V(660F38,1A,_,1,0,_,_,_  ), 0                         , 114, 0  , 3502 , 207, 123), // #821
  INST(Vbroadcastf32x2  , VexRm_Lx           , E(660F38,19,_,x,_,0,3,T2 ), 0                         , 115, 0  , 3517 , 208, 129), // #822
  INST(Vbroadcastf32x4  , VexRm_Lx           , E(660F38,1A,_,x,_,0,4,T4 ), 0                         , 116, 0  , 3533 , 209, 66 ), // #823
  INST(Vbroadcastf32x8  , VexRm              , E(660F38,1B,_,2,_,0,5,T8 ), 0                         , 117, 0  , 3549 , 210, 64 ), // #824
  INST(Vbroadcastf64x2  , VexRm_Lx           , E(660F38,1A,_,x,_,1,4,T2 ), 0                         , 118, 0  , 3565 , 209, 129), // #825
  INST(Vbroadcastf64x4  , VexRm              , E(660F38,1B,_,2,_,1,5,T4 ), 0                         , 119, 0  , 3581 , 210, 66 ), // #826
  INST(Vbroadcasti128   , VexRm              , V(660F38,5A,_,1,0,_,_,_  ), 0                         , 114, 0  , 3597 , 207, 130), // #827
  INST(Vbroadcasti32x2  , VexRm_Lx           , E(660F38,59,_,x,_,0,3,T2 ), 0                         , 115, 0  , 3612 , 211, 129), // #828
  INST(Vbroadcasti32x4  , VexRm_Lx           , E(660F38,5A,_,x,_,0,4,T4 ), 0                         , 116, 0  , 3628 , 209, 126), // #829
  INST(Vbroadcasti32x8  , VexRm              , E(660F38,5B,_,2,_,0,5,T8 ), 0                         , 117, 0  , 3644 , 210, 64 ), // #830
  INST(Vbroadcasti64x2  , VexRm_Lx           , E(660F38,5A,_,x,_,1,4,T2 ), 0                         , 118, 0  , 3660 , 209, 129), // #831
  INST(Vbroadcasti64x4  , VexRm              , E(660F38,5B,_,2,_,1,5,T4 ), 0                         , 119, 0  , 3676 , 210, 66 ), // #832
  INST(Vbroadcastsd     , VexRm_Lx           , V(660F38,19,_,x,0,1,3,T1S), 0                         , 120, 0  , 3692 , 212, 131), // #833
  INST(Vbroadcastss     , VexRm_Lx           , V(660F38,18,_,x,0,0,2,T1S), 0                         , 121, 0  , 3705 , 213, 131), // #834
  INST(Vcmppd           , VexRvmi_Lx         , V(660F00,C2,_,x,I,1,4,FV ), 0                         , 102, 0  , 3718 , 214, 121), // #835
  INST(Vcmpps           , VexRvmi_Lx         , V(000F00,C2,_,x,I,0,4,FV ), 0                         , 103, 0  , 3725 , 215, 121), // #836
  INST(Vcmpsd           , VexRvmi            , V(F20F00,C2,_,I,I,1,3,T1S), 0                         , 104, 0  , 3732 , 216, 122), // #837
  INST(Vcmpss           , VexRvmi            , V(F30F00,C2,_,I,I,0,2,T1S), 0                         , 105, 0  , 3739 , 217, 122), // #838
  INST(Vcomisd          , VexRm              , V(660F00,2F,_,I,I,1,3,T1S), 0                         , 122, 0  , 3746 , 218, 132), // #839
  INST(Vcomiss          , VexRm              , V(000F00,2F,_,I,I,0,2,T1S), 0                         , 123, 0  , 3754 , 219, 132), // #840
  INST(Vcompresspd      , VexMr_Lx           , E(660F38,8A,_,x,_,1,3,T1S), 0                         , 124, 0  , 3762 , 220, 126), // #841
  INST(Vcompressps      , VexMr_Lx           , E(660F38,8A,_,x,_,0,2,T1S), 0                         , 125, 0  , 3774 , 220, 126), // #842
  INST(Vcvtdq2pd        , VexRm_Lx           , V(F30F00,E6,_,x,I,0,3,HV ), 0                         , 126, 0  , 3786 , 221, 121), // #843
  INST(Vcvtdq2ps        , VexRm_Lx           , V(000F00,5B,_,x,I,0,4,FV ), 0                         , 103, 0  , 3796 , 222, 121), // #844
  INST(Vcvtne2ps2bf16   , VexRvm             , E(F20F38,72,_,_,_,0,_,_  ), 0                         , 127, 0  , 3806 , 203, 133), // #845
  INST(Vcvtneps2bf16    , VexRm              , E(F30F38,72,_,_,_,0,_,_  ), 0                         , 128, 0  , 3821 , 223, 133), // #846
  INST(Vcvtpd2dq        , VexRm_Lx           , V(F20F00,E6,_,x,I,1,4,FV ), 0                         , 129, 0  , 3835 , 224, 121), // #847
  INST(Vcvtpd2ps        , VexRm_Lx           , V(660F00,5A,_,x,I,1,4,FV ), 0                         , 102, 0  , 3845 , 224, 121), // #848
  INST(Vcvtpd2qq        , VexRm_Lx           , E(660F00,7B,_,x,_,1,4,FV ), 0                         , 130, 0  , 3855 , 225, 129), // #849
  INST(Vcvtpd2udq       , VexRm_Lx           , E(000F00,79,_,x,_,1,4,FV ), 0                         , 131, 0  , 3865 , 226, 126), // #850
  INST(Vcvtpd2uqq       , VexRm_Lx           , E(660F00,79,_,x,_,1,4,FV ), 0                         , 130, 0  , 3876 , 225, 129), // #851
  INST(Vcvtph2ps        , VexRm_Lx           , V(660F38,13,_,x,0,0,3,HVM), 0                         , 132, 0  , 3887 , 227, 134), // #852
  INST(Vcvtps2dq        , VexRm_Lx           , V(660F00,5B,_,x,I,0,4,FV ), 0                         , 133, 0  , 3897 , 222, 121), // #853
  INST(Vcvtps2pd        , VexRm_Lx           , V(000F00,5A,_,x,I,0,4,HV ), 0                         , 134, 0  , 3907 , 228, 121), // #854
  INST(Vcvtps2ph        , VexMri_Lx          , V(660F3A,1D,_,x,0,0,3,HVM), 0                         , 135, 0  , 3917 , 229, 134), // #855
  INST(Vcvtps2qq        , VexRm_Lx           , E(660F00,7B,_,x,_,0,3,HV ), 0                         , 136, 0  , 3927 , 230, 129), // #856
  INST(Vcvtps2udq       , VexRm_Lx           , E(000F00,79,_,x,_,0,4,FV ), 0                         , 137, 0  , 3937 , 231, 126), // #857
  INST(Vcvtps2uqq       , VexRm_Lx           , E(660F00,79,_,x,_,0,3,HV ), 0                         , 136, 0  , 3948 , 230, 129), // #858
  INST(Vcvtqq2pd        , VexRm_Lx           , E(F30F00,E6,_,x,_,1,4,FV ), 0                         , 138, 0  , 3959 , 225, 129), // #859
  INST(Vcvtqq2ps        , VexRm_Lx           , E(000F00,5B,_,x,_,1,4,FV ), 0                         , 131, 0  , 3969 , 226, 129), // #860
  INST(Vcvtsd2si        , VexRm_Wx           , V(F20F00,2D,_,I,x,x,3,T1F), 0                         , 139, 0  , 3979 , 232, 122), // #861
  INST(Vcvtsd2ss        , VexRvm             , V(F20F00,5A,_,I,I,1,3,T1S), 0                         , 104, 0  , 3989 , 190, 122), // #862
  INST(Vcvtsd2usi       , VexRm_Wx           , E(F20F00,79,_,I,_,x,3,T1F), 0                         , 140, 0  , 3999 , 233, 66 ), // #863
  INST(Vcvtsi2sd        , VexRvm_Wx          , V(F20F00,2A,_,I,x,x,2,T1W), 0                         , 141, 0  , 4010 , 234, 122), // #864
  INST(Vcvtsi2ss        , VexRvm_Wx          , V(F30F00,2A,_,I,x,x,2,T1W), 0                         , 142, 0  , 4020 , 234, 122), // #865
  INST(Vcvtss2sd        , VexRvm             , V(F30F00,5A,_,I,I,0,2,T1S), 0                         , 105, 0  , 4030 , 235, 122), // #866
  INST(Vcvtss2si        , VexRm_Wx           , V(F30F00,2D,_,I,x,x,2,T1F), 0                         , 143, 0  , 4040 , 236, 122), // #867
  INST(Vcvtss2usi       , VexRm_Wx           , E(F30F00,79,_,I,_,x,2,T1F), 0                         , 144, 0  , 4050 , 237, 66 ), // #868
  INST(Vcvttpd2dq       , VexRm_Lx           , V(660F00,E6,_,x,I,1,4,FV ), 0                         , 102, 0  , 4061 , 238, 121), // #869
  INST(Vcvttpd2qq       , VexRm_Lx           , E(660F00,7A,_,x,_,1,4,FV ), 0                         , 130, 0  , 4072 , 239, 126), // #870
  INST(Vcvttpd2udq      , VexRm_Lx           , E(000F00,78,_,x,_,1,4,FV ), 0                         , 131, 0  , 4083 , 240, 126), // #871
  INST(Vcvttpd2uqq      , VexRm_Lx           , E(660F00,78,_,x,_,1,4,FV ), 0                         , 130, 0  , 4095 , 239, 129), // #872
  INST(Vcvttps2dq       , VexRm_Lx           , V(F30F00,5B,_,x,I,0,4,FV ), 0                         , 145, 0  , 4107 , 241, 121), // #873
  INST(Vcvttps2qq       , VexRm_Lx           , E(660F00,7A,_,x,_,0,3,HV ), 0                         , 136, 0  , 4118 , 242, 129), // #874
  INST(Vcvttps2udq      , VexRm_Lx           , E(000F00,78,_,x,_,0,4,FV ), 0                         , 137, 0  , 4129 , 243, 126), // #875
  INST(Vcvttps2uqq      , VexRm_Lx           , E(660F00,78,_,x,_,0,3,HV ), 0                         , 136, 0  , 4141 , 242, 129), // #876
  INST(Vcvttsd2si       , VexRm_Wx           , V(F20F00,2C,_,I,x,x,3,T1F), 0                         , 139, 0  , 4153 , 244, 122), // #877
  INST(Vcvttsd2usi      , VexRm_Wx           , E(F20F00,78,_,I,_,x,3,T1F), 0                         , 140, 0  , 4164 , 245, 66 ), // #878
  INST(Vcvttss2si       , VexRm_Wx           , V(F30F00,2C,_,I,x,x,2,T1F), 0                         , 143, 0  , 4176 , 246, 122), // #879
  INST(Vcvttss2usi      , VexRm_Wx           , E(F30F00,78,_,I,_,x,2,T1F), 0                         , 144, 0  , 4187 , 247, 66 ), // #880
  INST(Vcvtudq2pd       , VexRm_Lx           , E(F30F00,7A,_,x,_,0,3,HV ), 0                         , 146, 0  , 4199 , 248, 126), // #881
  INST(Vcvtudq2ps       , VexRm_Lx           , E(F20F00,7A,_,x,_,0,4,FV ), 0                         , 147, 0  , 4210 , 231, 126), // #882
  INST(Vcvtuqq2pd       , VexRm_Lx           , E(F30F00,7A,_,x,_,1,4,FV ), 0                         , 138, 0  , 4221 , 225, 129), // #883
  INST(Vcvtuqq2ps       , VexRm_Lx           , E(F20F00,7A,_,x,_,1,4,FV ), 0                         , 148, 0  , 4232 , 226, 129), // #884
  INST(Vcvtusi2sd       , VexRvm_Wx          , E(F20F00,7B,_,I,_,x,2,T1W), 0                         , 149, 0  , 4243 , 249, 66 ), // #885
  INST(Vcvtusi2ss       , VexRvm_Wx          , E(F30F00,7B,_,I,_,x,2,T1W), 0                         , 150, 0  , 4254 , 249, 66 ), // #886
  INST(Vdbpsadbw        , VexRvmi_Lx         , E(660F3A,42,_,x,_,0,4,FVM), 0                         , 151, 0  , 4265 , 250, 128), // #887
  INST(Vdivpd           , VexRvm_Lx          , V(660F00,5E,_,x,I,1,4,FV ), 0                         , 102, 0  , 4275 , 188, 121), // #888
  INST(Vdivps           , VexRvm_Lx          , V(000F00,5E,_,x,I,0,4,FV ), 0                         , 103, 0  , 4282 , 189, 121), // #889
  INST(Vdivsd           , VexRvm             , V(F20F00,5E,_,I,I,1,3,T1S), 0                         , 104, 0  , 4289 , 190, 122), // #890
  INST(Vdivss           , VexRvm             , V(F30F00,5E,_,I,I,0,2,T1S), 0                         , 105, 0  , 4296 , 191, 122), // #891
  INST(Vdpbf16ps        , VexRvm             , E(F30F38,52,_,_,_,0,_,_  ), 0                         , 128, 0  , 4303 , 203, 133), // #892
  INST(Vdppd            , VexRvmi_Lx         , V(660F3A,41,_,x,I,_,_,_  ), 0                         , 72 , 0  , 4313 , 251, 123), // #893
  INST(Vdpps            , VexRvmi_Lx         , V(660F3A,40,_,x,I,_,_,_  ), 0                         , 72 , 0  , 4319 , 205, 123), // #894
  INST(Verr             , X86M_NoSize        , O(000F00,00,4,_,_,_,_,_  ), 0                         , 97 , 0  , 4325 , 101, 10 ), // #895
  INST(Verw             , X86M_NoSize        , O(000F00,00,5,_,_,_,_,_  ), 0                         , 75 , 0  , 4330 , 101, 10 ), // #896
  INST(Vexp2pd          , VexRm              , E(660F38,C8,_,2,_,1,4,FV ), 0                         , 152, 0  , 4335 , 252, 135), // #897
  INST(Vexp2ps          , VexRm              , E(660F38,C8,_,2,_,0,4,FV ), 0                         , 153, 0  , 4343 , 253, 135), // #898
  INST(Vexpandpd        , VexRm_Lx           , E(660F38,88,_,x,_,1,3,T1S), 0                         , 124, 0  , 4351 , 254, 126), // #899
  INST(Vexpandps        , VexRm_Lx           , E(660F38,88,_,x,_,0,2,T1S), 0                         , 125, 0  , 4361 , 254, 126), // #900
  INST(Vextractf128     , VexMri             , V(660F3A,19,_,1,0,_,_,_  ), 0                         , 154, 0  , 4371 , 255, 123), // #901
  INST(Vextractf32x4    , VexMri_Lx          , E(660F3A,19,_,x,_,0,4,T4 ), 0                         , 155, 0  , 4384 , 256, 126), // #902
  INST(Vextractf32x8    , VexMri             , E(660F3A,1B,_,2,_,0,5,T8 ), 0                         , 156, 0  , 4398 , 257, 64 ), // #903
  INST(Vextractf64x2    , VexMri_Lx          , E(660F3A,19,_,x,_,1,4,T2 ), 0                         , 157, 0  , 4412 , 256, 129), // #904
  INST(Vextractf64x4    , VexMri             , E(660F3A,1B,_,2,_,1,5,T4 ), 0                         , 158, 0  , 4426 , 257, 66 ), // #905
  INST(Vextracti128     , VexMri             , V(660F3A,39,_,1,0,_,_,_  ), 0                         , 154, 0  , 4440 , 255, 130), // #906
  INST(Vextracti32x4    , VexMri_Lx          , E(660F3A,39,_,x,_,0,4,T4 ), 0                         , 155, 0  , 4453 , 256, 126), // #907
  INST(Vextracti32x8    , VexMri             , E(660F3A,3B,_,2,_,0,5,T8 ), 0                         , 156, 0  , 4467 , 257, 64 ), // #908
  INST(Vextracti64x2    , VexMri_Lx          , E(660F3A,39,_,x,_,1,4,T2 ), 0                         , 157, 0  , 4481 , 256, 129), // #909
  INST(Vextracti64x4    , VexMri             , E(660F3A,3B,_,2,_,1,5,T4 ), 0                         , 158, 0  , 4495 , 257, 66 ), // #910
  INST(Vextractps       , VexMri             , V(660F3A,17,_,0,I,I,2,T1S), 0                         , 159, 0  , 4509 , 258, 122), // #911
  INST(Vfixupimmpd      , VexRvmi_Lx         , E(660F3A,54,_,x,_,1,4,FV ), 0                         , 109, 0  , 4520 , 259, 126), // #912
  INST(Vfixupimmps      , VexRvmi_Lx         , E(660F3A,54,_,x,_,0,4,FV ), 0                         , 108, 0  , 4532 , 260, 126), // #913
  INST(Vfixupimmsd      , VexRvmi            , E(660F3A,55,_,I,_,1,3,T1S), 0                         , 160, 0  , 4544 , 261, 66 ), // #914
  INST(Vfixupimmss      , VexRvmi            , E(660F3A,55,_,I,_,0,2,T1S), 0                         , 161, 0  , 4556 , 262, 66 ), // #915
  INST(Vfmadd132pd      , VexRvm_Lx          , V(660F38,98,_,x,1,1,4,FV ), 0                         , 162, 0  , 4568 , 188, 136), // #916
  INST(Vfmadd132ps      , VexRvm_Lx          , V(660F38,98,_,x,0,0,4,FV ), 0                         , 163, 0  , 4580 , 189, 136), // #917
  INST(Vfmadd132sd      , VexRvm             , V(660F38,99,_,I,1,1,3,T1S), 0                         , 164, 0  , 4592 , 190, 137), // #918
  INST(Vfmadd132ss      , VexRvm             , V(660F38,99,_,I,0,0,2,T1S), 0                         , 121, 0  , 4604 , 191, 137), // #919
  INST(Vfmadd213pd      , VexRvm_Lx          , V(660F38,A8,_,x,1,1,4,FV ), 0                         , 162, 0  , 4616 , 188, 136), // #920
  INST(Vfmadd213ps      , VexRvm_Lx          , V(660F38,A8,_,x,0,0,4,FV ), 0                         , 163, 0  , 4628 , 189, 136), // #921
  INST(Vfmadd213sd      , VexRvm             , V(660F38,A9,_,I,1,1,3,T1S), 0                         , 164, 0  , 4640 , 190, 137), // #922
  INST(Vfmadd213ss      , VexRvm             , V(660F38,A9,_,I,0,0,2,T1S), 0                         , 121, 0  , 4652 , 191, 137), // #923
  INST(Vfmadd231pd      , VexRvm_Lx          , V(660F38,B8,_,x,1,1,4,FV ), 0                         , 162, 0  , 4664 , 188, 136), // #924
  INST(Vfmadd231ps      , VexRvm_Lx          , V(660F38,B8,_,x,0,0,4,FV ), 0                         , 163, 0  , 4676 , 189, 136), // #925
  INST(Vfmadd231sd      , VexRvm             , V(660F38,B9,_,I,1,1,3,T1S), 0                         , 164, 0  , 4688 , 190, 137), // #926
  INST(Vfmadd231ss      , VexRvm             , V(660F38,B9,_,I,0,0,2,T1S), 0                         , 121, 0  , 4700 , 191, 137), // #927
  INST(Vfmaddpd         , Fma4_Lx            , V(660F3A,69,_,x,x,_,_,_  ), 0                         , 72 , 0  , 4712 , 263, 138), // #928
  INST(Vfmaddps         , Fma4_Lx            , V(660F3A,68,_,x,x,_,_,_  ), 0                         , 72 , 0  , 4721 , 263, 138), // #929
  INST(Vfmaddsd         , Fma4               , V(660F3A,6B,_,0,x,_,_,_  ), 0                         , 72 , 0  , 4730 , 264, 138), // #930
  INST(Vfmaddss         , Fma4               , V(660F3A,6A,_,0,x,_,_,_  ), 0                         , 72 , 0  , 4739 , 265, 138), // #931
  INST(Vfmaddsub132pd   , VexRvm_Lx          , V(660F38,96,_,x,1,1,4,FV ), 0                         , 162, 0  , 4748 , 188, 136), // #932
  INST(Vfmaddsub132ps   , VexRvm_Lx          , V(660F38,96,_,x,0,0,4,FV ), 0                         , 163, 0  , 4763 , 189, 136), // #933
  INST(Vfmaddsub213pd   , VexRvm_Lx          , V(660F38,A6,_,x,1,1,4,FV ), 0                         , 162, 0  , 4778 , 188, 136), // #934
  INST(Vfmaddsub213ps   , VexRvm_Lx          , V(660F38,A6,_,x,0,0,4,FV ), 0                         , 163, 0  , 4793 , 189, 136), // #935
  INST(Vfmaddsub231pd   , VexRvm_Lx          , V(660F38,B6,_,x,1,1,4,FV ), 0                         , 162, 0  , 4808 , 188, 136), // #936
  INST(Vfmaddsub231ps   , VexRvm_Lx          , V(660F38,B6,_,x,0,0,4,FV ), 0                         , 163, 0  , 4823 , 189, 136), // #937
  INST(Vfmaddsubpd      , Fma4_Lx            , V(660F3A,5D,_,x,x,_,_,_  ), 0                         , 72 , 0  , 4838 , 263, 138), // #938
  INST(Vfmaddsubps      , Fma4_Lx            , V(660F3A,5C,_,x,x,_,_,_  ), 0                         , 72 , 0  , 4850 , 263, 138), // #939
  INST(Vfmsub132pd      , VexRvm_Lx          , V(660F38,9A,_,x,1,1,4,FV ), 0                         , 162, 0  , 4862 , 188, 136), // #940
  INST(Vfmsub132ps      , VexRvm_Lx          , V(660F38,9A,_,x,0,0,4,FV ), 0                         , 163, 0  , 4874 , 189, 136), // #941
  INST(Vfmsub132sd      , VexRvm             , V(660F38,9B,_,I,1,1,3,T1S), 0                         , 164, 0  , 4886 , 190, 137), // #942
  INST(Vfmsub132ss      , VexRvm             , V(660F38,9B,_,I,0,0,2,T1S), 0                         , 121, 0  , 4898 , 191, 137), // #943
  INST(Vfmsub213pd      , VexRvm_Lx          , V(660F38,AA,_,x,1,1,4,FV ), 0                         , 162, 0  , 4910 , 188, 136), // #944
  INST(Vfmsub213ps      , VexRvm_Lx          , V(660F38,AA,_,x,0,0,4,FV ), 0                         , 163, 0  , 4922 , 189, 136), // #945
  INST(Vfmsub213sd      , VexRvm             , V(660F38,AB,_,I,1,1,3,T1S), 0                         , 164, 0  , 4934 , 190, 137), // #946
  INST(Vfmsub213ss      , VexRvm             , V(660F38,AB,_,I,0,0,2,T1S), 0                         , 121, 0  , 4946 , 191, 137), // #947
  INST(Vfmsub231pd      , VexRvm_Lx          , V(660F38,BA,_,x,1,1,4,FV ), 0                         , 162, 0  , 4958 , 188, 136), // #948
  INST(Vfmsub231ps      , VexRvm_Lx          , V(660F38,BA,_,x,0,0,4,FV ), 0                         , 163, 0  , 4970 , 189, 136), // #949
  INST(Vfmsub231sd      , VexRvm             , V(660F38,BB,_,I,1,1,3,T1S), 0                         , 164, 0  , 4982 , 190, 137), // #950
  INST(Vfmsub231ss      , VexRvm             , V(660F38,BB,_,I,0,0,2,T1S), 0                         , 121, 0  , 4994 , 191, 137), // #951
  INST(Vfmsubadd132pd   , VexRvm_Lx          , V(660F38,97,_,x,1,1,4,FV ), 0                         , 162, 0  , 5006 , 188, 136), // #952
  INST(Vfmsubadd132ps   , VexRvm_Lx          , V(660F38,97,_,x,0,0,4,FV ), 0                         , 163, 0  , 5021 , 189, 136), // #953
  INST(Vfmsubadd213pd   , VexRvm_Lx          , V(660F38,A7,_,x,1,1,4,FV ), 0                         , 162, 0  , 5036 , 188, 136), // #954
  INST(Vfmsubadd213ps   , VexRvm_Lx          , V(660F38,A7,_,x,0,0,4,FV ), 0                         , 163, 0  , 5051 , 189, 136), // #955
  INST(Vfmsubadd231pd   , VexRvm_Lx          , V(660F38,B7,_,x,1,1,4,FV ), 0                         , 162, 0  , 5066 , 188, 136), // #956
  INST(Vfmsubadd231ps   , VexRvm_Lx          , V(660F38,B7,_,x,0,0,4,FV ), 0                         , 163, 0  , 5081 , 189, 136), // #957
  INST(Vfmsubaddpd      , Fma4_Lx            , V(660F3A,5F,_,x,x,_,_,_  ), 0                         , 72 , 0  , 5096 , 263, 138), // #958
  INST(Vfmsubaddps      , Fma4_Lx            , V(660F3A,5E,_,x,x,_,_,_  ), 0                         , 72 , 0  , 5108 , 263, 138), // #959
  INST(Vfmsubpd         , Fma4_Lx            , V(660F3A,6D,_,x,x,_,_,_  ), 0                         , 72 , 0  , 5120 , 263, 138), // #960
  INST(Vfmsubps         , Fma4_Lx            , V(660F3A,6C,_,x,x,_,_,_  ), 0                         , 72 , 0  , 5129 , 263, 138), // #961
  INST(Vfmsubsd         , Fma4               , V(660F3A,6F,_,0,x,_,_,_  ), 0                         , 72 , 0  , 5138 , 264, 138), // #962
  INST(Vfmsubss         , Fma4               , V(660F3A,6E,_,0,x,_,_,_  ), 0                         , 72 , 0  , 5147 , 265, 138), // #963
  INST(Vfnmadd132pd     , VexRvm_Lx          , V(660F38,9C,_,x,1,1,4,FV ), 0                         , 162, 0  , 5156 , 188, 136), // #964
  INST(Vfnmadd132ps     , VexRvm_Lx          , V(660F38,9C,_,x,0,0,4,FV ), 0                         , 163, 0  , 5169 , 189, 136), // #965
  INST(Vfnmadd132sd     , VexRvm             , V(660F38,9D,_,I,1,1,3,T1S), 0                         , 164, 0  , 5182 , 190, 137), // #966
  INST(Vfnmadd132ss     , VexRvm             , V(660F38,9D,_,I,0,0,2,T1S), 0                         , 121, 0  , 5195 , 191, 137), // #967
  INST(Vfnmadd213pd     , VexRvm_Lx          , V(660F38,AC,_,x,1,1,4,FV ), 0                         , 162, 0  , 5208 , 188, 136), // #968
  INST(Vfnmadd213ps     , VexRvm_Lx          , V(660F38,AC,_,x,0,0,4,FV ), 0                         , 163, 0  , 5221 , 189, 136), // #969
  INST(Vfnmadd213sd     , VexRvm             , V(660F38,AD,_,I,1,1,3,T1S), 0                         , 164, 0  , 5234 , 190, 137), // #970
  INST(Vfnmadd213ss     , VexRvm             , V(660F38,AD,_,I,0,0,2,T1S), 0                         , 121, 0  , 5247 , 191, 137), // #971
  INST(Vfnmadd231pd     , VexRvm_Lx          , V(660F38,BC,_,x,1,1,4,FV ), 0                         , 162, 0  , 5260 , 188, 136), // #972
  INST(Vfnmadd231ps     , VexRvm_Lx          , V(660F38,BC,_,x,0,0,4,FV ), 0                         , 163, 0  , 5273 , 189, 136), // #973
  INST(Vfnmadd231sd     , VexRvm             , V(660F38,BC,_,I,1,1,3,T1S), 0                         , 164, 0  , 5286 , 190, 137), // #974
  INST(Vfnmadd231ss     , VexRvm             , V(660F38,BC,_,I,0,0,2,T1S), 0                         , 121, 0  , 5299 , 191, 137), // #975
  INST(Vfnmaddpd        , Fma4_Lx            , V(660F3A,79,_,x,x,_,_,_  ), 0                         , 72 , 0  , 5312 , 263, 138), // #976
  INST(Vfnmaddps        , Fma4_Lx            , V(660F3A,78,_,x,x,_,_,_  ), 0                         , 72 , 0  , 5322 , 263, 138), // #977
  INST(Vfnmaddsd        , Fma4               , V(660F3A,7B,_,0,x,_,_,_  ), 0                         , 72 , 0  , 5332 , 264, 138), // #978
  INST(Vfnmaddss        , Fma4               , V(660F3A,7A,_,0,x,_,_,_  ), 0                         , 72 , 0  , 5342 , 265, 138), // #979
  INST(Vfnmsub132pd     , VexRvm_Lx          , V(660F38,9E,_,x,1,1,4,FV ), 0                         , 162, 0  , 5352 , 188, 136), // #980
  INST(Vfnmsub132ps     , VexRvm_Lx          , V(660F38,9E,_,x,0,0,4,FV ), 0                         , 163, 0  , 5365 , 189, 136), // #981
  INST(Vfnmsub132sd     , VexRvm             , V(660F38,9F,_,I,1,1,3,T1S), 0                         , 164, 0  , 5378 , 190, 137), // #982
  INST(Vfnmsub132ss     , VexRvm             , V(660F38,9F,_,I,0,0,2,T1S), 0                         , 121, 0  , 5391 , 191, 137), // #983
  INST(Vfnmsub213pd     , VexRvm_Lx          , V(660F38,AE,_,x,1,1,4,FV ), 0                         , 162, 0  , 5404 , 188, 136), // #984
  INST(Vfnmsub213ps     , VexRvm_Lx          , V(660F38,AE,_,x,0,0,4,FV ), 0                         , 163, 0  , 5417 , 189, 136), // #985
  INST(Vfnmsub213sd     , VexRvm             , V(660F38,AF,_,I,1,1,3,T1S), 0                         , 164, 0  , 5430 , 190, 137), // #986
  INST(Vfnmsub213ss     , VexRvm             , V(660F38,AF,_,I,0,0,2,T1S), 0                         , 121, 0  , 5443 , 191, 137), // #987
  INST(Vfnmsub231pd     , VexRvm_Lx          , V(660F38,BE,_,x,1,1,4,FV ), 0                         , 162, 0  , 5456 , 188, 136), // #988
  INST(Vfnmsub231ps     , VexRvm_Lx          , V(660F38,BE,_,x,0,0,4,FV ), 0                         , 163, 0  , 5469 , 189, 136), // #989
  INST(Vfnmsub231sd     , VexRvm             , V(660F38,BF,_,I,1,1,3,T1S), 0                         , 164, 0  , 5482 , 190, 137), // #990
  INST(Vfnmsub231ss     , VexRvm             , V(660F38,BF,_,I,0,0,2,T1S), 0                         , 121, 0  , 5495 , 191, 137), // #991
  INST(Vfnmsubpd        , Fma4_Lx            , V(660F3A,7D,_,x,x,_,_,_  ), 0                         , 72 , 0  , 5508 , 263, 138), // #992
  INST(Vfnmsubps        , Fma4_Lx            , V(660F3A,7C,_,x,x,_,_,_  ), 0                         , 72 , 0  , 5518 , 263, 138), // #993
  INST(Vfnmsubsd        , Fma4               , V(660F3A,7F,_,0,x,_,_,_  ), 0                         , 72 , 0  , 5528 , 264, 138), // #994
  INST(Vfnmsubss        , Fma4               , V(660F3A,7E,_,0,x,_,_,_  ), 0                         , 72 , 0  , 5538 , 265, 138), // #995
  INST(Vfpclasspd       , VexRmi_Lx          , E(660F3A,66,_,x,_,1,4,FV ), 0                         , 109, 0  , 5548 , 266, 129), // #996
  INST(Vfpclassps       , VexRmi_Lx          , E(660F3A,66,_,x,_,0,4,FV ), 0                         , 108, 0  , 5559 , 267, 129), // #997
  INST(Vfpclasssd       , VexRmi_Lx          , E(660F3A,67,_,I,_,1,3,T1S), 0                         , 160, 0  , 5570 , 268, 64 ), // #998
  INST(Vfpclassss       , VexRmi_Lx          , E(660F3A,67,_,I,_,0,2,T1S), 0                         , 161, 0  , 5581 , 269, 64 ), // #999
  INST(Vfrczpd          , VexRm_Lx           , V(XOP_M9,81,_,x,0,_,_,_  ), 0                         , 77 , 0  , 5592 , 270, 139), // #1000
  INST(Vfrczps          , VexRm_Lx           , V(XOP_M9,80,_,x,0,_,_,_  ), 0                         , 77 , 0  , 5600 , 270, 139), // #1001
  INST(Vfrczsd          , VexRm              , V(XOP_M9,83,_,0,0,_,_,_  ), 0                         , 77 , 0  , 5608 , 271, 139), // #1002
  INST(Vfrczss          , VexRm              , V(XOP_M9,82,_,0,0,_,_,_  ), 0                         , 77 , 0  , 5616 , 272, 139), // #1003
  INST(Vgatherdpd       , VexRmvRm_VM        , V(660F38,92,_,x,1,_,_,_  ), V(660F38,92,_,x,_,1,3,T1S), 165, 79 , 5624 , 273, 140), // #1004
  INST(Vgatherdps       , VexRmvRm_VM        , V(660F38,92,_,x,0,_,_,_  ), V(660F38,92,_,x,_,0,2,T1S), 96 , 80 , 5635 , 274, 140), // #1005
  INST(Vgatherpf0dpd    , VexM_VM            , E(660F38,C6,1,2,_,1,3,T1S), 0                         , 166, 0  , 5646 , 275, 141), // #1006
  INST(Vgatherpf0dps    , VexM_VM            , E(660F38,C6,1,2,_,0,2,T1S), 0                         , 167, 0  , 5660 , 276, 141), // #1007
  INST(Vgatherpf0qpd    , VexM_VM            , E(660F38,C7,1,2,_,1,3,T1S), 0                         , 166, 0  , 5674 , 277, 141), // #1008
  INST(Vgatherpf0qps    , VexM_VM            , E(660F38,C7,1,2,_,0,2,T1S), 0                         , 167, 0  , 5688 , 277, 141), // #1009
  INST(Vgatherpf1dpd    , VexM_VM            , E(660F38,C6,2,2,_,1,3,T1S), 0                         , 168, 0  , 5702 , 275, 141), // #1010
  INST(Vgatherpf1dps    , VexM_VM            , E(660F38,C6,2,2,_,0,2,T1S), 0                         , 169, 0  , 5716 , 276, 141), // #1011
  INST(Vgatherpf1qpd    , VexM_VM            , E(660F38,C7,2,2,_,1,3,T1S), 0                         , 168, 0  , 5730 , 277, 141), // #1012
  INST(Vgatherpf1qps    , VexM_VM            , E(660F38,C7,2,2,_,0,2,T1S), 0                         , 169, 0  , 5744 , 277, 141), // #1013
  INST(Vgatherqpd       , VexRmvRm_VM        , V(660F38,93,_,x,1,_,_,_  ), V(660F38,93,_,x,_,1,3,T1S), 165, 81 , 5758 , 278, 140), // #1014
  INST(Vgatherqps       , VexRmvRm_VM        , V(660F38,93,_,x,0,_,_,_  ), V(660F38,93,_,x,_,0,2,T1S), 96 , 82 , 5769 , 279, 140), // #1015
  INST(Vgetexppd        , VexRm_Lx           , E(660F38,42,_,x,_,1,4,FV ), 0                         , 112, 0  , 5780 , 239, 126), // #1016
  INST(Vgetexpps        , VexRm_Lx           , E(660F38,42,_,x,_,0,4,FV ), 0                         , 111, 0  , 5790 , 243, 126), // #1017
  INST(Vgetexpsd        , VexRvm             , E(660F38,43,_,I,_,1,3,T1S), 0                         , 124, 0  , 5800 , 280, 66 ), // #1018
  INST(Vgetexpss        , VexRvm             , E(660F38,43,_,I,_,0,2,T1S), 0                         , 125, 0  , 5810 , 281, 66 ), // #1019
  INST(Vgetmantpd       , VexRmi_Lx          , E(660F3A,26,_,x,_,1,4,FV ), 0                         , 109, 0  , 5820 , 282, 126), // #1020
  INST(Vgetmantps       , VexRmi_Lx          , E(660F3A,26,_,x,_,0,4,FV ), 0                         , 108, 0  , 5831 , 283, 126), // #1021
  INST(Vgetmantsd       , VexRvmi            , E(660F3A,27,_,I,_,1,3,T1S), 0                         , 160, 0  , 5842 , 261, 66 ), // #1022
  INST(Vgetmantss       , VexRvmi            , E(660F3A,27,_,I,_,0,2,T1S), 0                         , 161, 0  , 5853 , 262, 66 ), // #1023
  INST(Vgf2p8affineinvqb, VexRvmi_Lx         , V(660F3A,CF,_,x,1,1,4,FV ), 0                         , 170, 0  , 5864 , 284, 142), // #1024
  INST(Vgf2p8affineqb   , VexRvmi_Lx         , V(660F3A,CE,_,x,1,1,4,FV ), 0                         , 170, 0  , 5882 , 284, 142), // #1025
  INST(Vgf2p8mulb       , VexRvm_Lx          , V(660F38,CF,_,x,0,0,4,FV ), 0                         , 163, 0  , 5897 , 285, 142), // #1026
  INST(Vhaddpd          , VexRvm_Lx          , V(660F00,7C,_,x,I,_,_,_  ), 0                         , 68 , 0  , 5908 , 192, 123), // #1027
  INST(Vhaddps          , VexRvm_Lx          , V(F20F00,7C,_,x,I,_,_,_  ), 0                         , 106, 0  , 5916 , 192, 123), // #1028
  INST(Vhsubpd          , VexRvm_Lx          , V(660F00,7D,_,x,I,_,_,_  ), 0                         , 68 , 0  , 5924 , 192, 123), // #1029
  INST(Vhsubps          , VexRvm_Lx          , V(F20F00,7D,_,x,I,_,_,_  ), 0                         , 106, 0  , 5932 , 192, 123), // #1030
  INST(Vinsertf128      , VexRvmi            , V(660F3A,18,_,1,0,_,_,_  ), 0                         , 154, 0  , 5940 , 286, 123), // #1031
  INST(Vinsertf32x4     , VexRvmi_Lx         , E(660F3A,18,_,x,_,0,4,T4 ), 0                         , 155, 0  , 5952 , 287, 126), // #1032
  INST(Vinsertf32x8     , VexRvmi            , E(660F3A,1A,_,2,_,0,5,T8 ), 0                         , 156, 0  , 5965 , 288, 64 ), // #1033
  INST(Vinsertf64x2     , VexRvmi_Lx         , E(660F3A,18,_,x,_,1,4,T2 ), 0                         , 157, 0  , 5978 , 287, 129), // #1034
  INST(Vinsertf64x4     , VexRvmi            , E(660F3A,1A,_,2,_,1,5,T4 ), 0                         , 158, 0  , 5991 , 288, 66 ), // #1035
  INST(Vinserti128      , VexRvmi            , V(660F3A,38,_,1,0,_,_,_  ), 0                         , 154, 0  , 6004 , 286, 130), // #1036
  INST(Vinserti32x4     , VexRvmi_Lx         , E(660F3A,38,_,x,_,0,4,T4 ), 0                         , 155, 0  , 6016 , 287, 126), // #1037
  INST(Vinserti32x8     , VexRvmi            , E(660F3A,3A,_,2,_,0,5,T8 ), 0                         , 156, 0  , 6029 , 288, 64 ), // #1038
  INST(Vinserti64x2     , VexRvmi_Lx         , E(660F3A,38,_,x,_,1,4,T2 ), 0                         , 157, 0  , 6042 , 287, 129), // #1039
  INST(Vinserti64x4     , VexRvmi            , E(660F3A,3A,_,2,_,1,5,T4 ), 0                         , 158, 0  , 6055 , 288, 66 ), // #1040
  INST(Vinsertps        , VexRvmi            , V(660F3A,21,_,0,I,0,2,T1S), 0                         , 159, 0  , 6068 , 289, 122), // #1041
  INST(Vlddqu           , VexRm_Lx           , V(F20F00,F0,_,x,I,_,_,_  ), 0                         , 106, 0  , 6078 , 290, 123), // #1042
  INST(Vldmxcsr         , VexM               , V(000F00,AE,2,0,I,_,_,_  ), 0                         , 171, 0  , 6085 , 291, 123), // #1043
  INST(Vmaskmovdqu      , VexRm_ZDI          , V(660F00,F7,_,0,I,_,_,_  ), 0                         , 68 , 0  , 6094 , 292, 123), // #1044
  INST(Vmaskmovpd       , VexRvmMvr_Lx       , V(660F38,2D,_,x,0,_,_,_  ), V(660F38,2F,_,x,0,_,_,_  ), 96 , 83 , 6106 , 293, 123), // #1045
  INST(Vmaskmovps       , VexRvmMvr_Lx       , V(660F38,2C,_,x,0,_,_,_  ), V(660F38,2E,_,x,0,_,_,_  ), 96 , 84 , 6117 , 293, 123), // #1046
  INST(Vmaxpd           , VexRvm_Lx          , V(660F00,5F,_,x,I,1,4,FV ), 0                         , 102, 0  , 6128 , 294, 121), // #1047
  INST(Vmaxps           , VexRvm_Lx          , V(000F00,5F,_,x,I,0,4,FV ), 0                         , 103, 0  , 6135 , 295, 121), // #1048
  INST(Vmaxsd           , VexRvm             , V(F20F00,5F,_,I,I,1,3,T1S), 0                         , 104, 0  , 6142 , 296, 121), // #1049
  INST(Vmaxss           , VexRvm             , V(F30F00,5F,_,I,I,0,2,T1S), 0                         , 105, 0  , 6149 , 235, 121), // #1050
  INST(Vmcall           , X86Op              , O(000F01,C1,_,_,_,_,_,_  ), 0                         , 21 , 0  , 6156 , 30 , 56 ), // #1051
  INST(Vmclear          , X86M_Only          , O(660F00,C7,6,_,_,_,_,_  ), 0                         , 25 , 0  , 6163 , 32 , 56 ), // #1052
  INST(Vmfunc           , X86Op              , O(000F01,D4,_,_,_,_,_,_  ), 0                         , 21 , 0  , 6171 , 30 , 56 ), // #1053
  INST(Vminpd           , VexRvm_Lx          , V(660F00,5D,_,x,I,1,4,FV ), 0                         , 102, 0  , 6178 , 294, 121), // #1054
  INST(Vminps           , VexRvm_Lx          , V(000F00,5D,_,x,I,0,4,FV ), 0                         , 103, 0  , 6185 , 295, 121), // #1055
  INST(Vminsd           , VexRvm             , V(F20F00,5D,_,I,I,1,3,T1S), 0                         , 104, 0  , 6192 , 296, 121), // #1056
  INST(Vminss           , VexRvm             , V(F30F00,5D,_,I,I,0,2,T1S), 0                         , 105, 0  , 6199 , 235, 121), // #1057
  INST(Vmlaunch         , X86Op              , O(000F01,C2,_,_,_,_,_,_  ), 0                         , 21 , 0  , 6206 , 30 , 56 ), // #1058
  INST(Vmload           , X86Op_xAX          , O(000F01,DA,_,_,_,_,_,_  ), 0                         , 21 , 0  , 6215 , 297, 22 ), // #1059
  INST(Vmmcall          , X86Op              , O(000F01,D9,_,_,_,_,_,_  ), 0                         , 21 , 0  , 6222 , 30 , 22 ), // #1060
  INST(Vmovapd          , VexRmMr_Lx         , V(660F00,28,_,x,I,1,4,FVM), V(660F00,29,_,x,I,1,4,FVM), 172, 85 , 6230 , 298, 121), // #1061
  INST(Vmovaps          , VexRmMr_Lx         , V(000F00,28,_,x,I,0,4,FVM), V(000F00,29,_,x,I,0,4,FVM), 173, 86 , 6238 , 298, 121), // #1062
  INST(Vmovd            , VexMovdMovq        , V(660F00,6E,_,0,0,0,2,T1S), V(660F00,7E,_,0,0,0,2,T1S), 174, 87 , 6246 , 299, 122), // #1063
  INST(Vmovddup         , VexRm_Lx           , V(F20F00,12,_,x,I,1,3,DUP), 0                         , 175, 0  , 6252 , 300, 121), // #1064
  INST(Vmovdqa          , VexRmMr_Lx         , V(660F00,6F,_,x,I,_,_,_  ), V(660F00,7F,_,x,I,_,_,_  ), 68 , 88 , 6261 , 301, 123), // #1065
  INST(Vmovdqa32        , VexRmMr_Lx         , E(660F00,6F,_,x,_,0,4,FVM), E(660F00,7F,_,x,_,0,4,FVM), 176, 89 , 6269 , 302, 126), // #1066
  INST(Vmovdqa64        , VexRmMr_Lx         , E(660F00,6F,_,x,_,1,4,FVM), E(660F00,7F,_,x,_,1,4,FVM), 177, 90 , 6279 , 302, 126), // #1067
  INST(Vmovdqu          , VexRmMr_Lx         , V(F30F00,6F,_,x,I,_,_,_  ), V(F30F00,7F,_,x,I,_,_,_  ), 178, 91 , 6289 , 301, 123), // #1068
  INST(Vmovdqu16        , VexRmMr_Lx         , E(F20F00,6F,_,x,_,1,4,FVM), E(F20F00,7F,_,x,_,1,4,FVM), 179, 92 , 6297 , 302, 128), // #1069
  INST(Vmovdqu32        , VexRmMr_Lx         , E(F30F00,6F,_,x,_,0,4,FVM), E(F30F00,7F,_,x,_,0,4,FVM), 180, 93 , 6307 , 302, 126), // #1070
  INST(Vmovdqu64        , VexRmMr_Lx         , E(F30F00,6F,_,x,_,1,4,FVM), E(F30F00,7F,_,x,_,1,4,FVM), 181, 94 , 6317 , 302, 126), // #1071
  INST(Vmovdqu8         , VexRmMr_Lx         , E(F20F00,6F,_,x,_,0,4,FVM), E(F20F00,7F,_,x,_,0,4,FVM), 182, 95 , 6327 , 302, 128), // #1072
  INST(Vmovhlps         , VexRvm             , V(000F00,12,_,0,I,0,_,_  ), 0                         , 71 , 0  , 6336 , 303, 122), // #1073
  INST(Vmovhpd          , VexRvmMr           , V(660F00,16,_,0,I,1,3,T1S), V(660F00,17,_,0,I,1,3,T1S), 122, 96 , 6345 , 304, 122), // #1074
  INST(Vmovhps          , VexRvmMr           , V(000F00,16,_,0,I,0,3,T2 ), V(000F00,17,_,0,I,0,3,T2 ), 183, 97 , 6353 , 304, 122), // #1075
  INST(Vmovlhps         , VexRvm             , V(000F00,16,_,0,I,0,_,_  ), 0                         , 71 , 0  , 6361 , 303, 122), // #1076
  INST(Vmovlpd          , VexRvmMr           , V(660F00,12,_,0,I,1,3,T1S), V(660F00,13,_,0,I,1,3,T1S), 122, 98 , 6370 , 304, 122), // #1077
  INST(Vmovlps          , VexRvmMr           , V(000F00,12,_,0,I,0,3,T2 ), V(000F00,13,_,0,I,0,3,T2 ), 183, 99 , 6378 , 304, 122), // #1078
  INST(Vmovmskpd        , VexRm_Lx           , V(660F00,50,_,x,I,_,_,_  ), 0                         , 68 , 0  , 6386 , 305, 123), // #1079
  INST(Vmovmskps        , VexRm_Lx           , V(000F00,50,_,x,I,_,_,_  ), 0                         , 71 , 0  , 6396 , 305, 123), // #1080
  INST(Vmovntdq         , VexMr_Lx           , V(660F00,E7,_,x,I,0,4,FVM), 0                         , 184, 0  , 6406 , 306, 121), // #1081
  INST(Vmovntdqa        , VexRm_Lx           , V(660F38,2A,_,x,I,0,4,FVM), 0                         , 107, 0  , 6415 , 307, 131), // #1082
  INST(Vmovntpd         , VexMr_Lx           , V(660F00,2B,_,x,I,1,4,FVM), 0                         , 172, 0  , 6425 , 306, 121), // #1083
  INST(Vmovntps         , VexMr_Lx           , V(000F00,2B,_,x,I,0,4,FVM), 0                         , 173, 0  , 6434 , 306, 121), // #1084
  INST(Vmovq            , VexMovdMovq        , V(660F00,6E,_,0,I,1,3,T1S), V(660F00,7E,_,0,I,1,3,T1S), 122, 100, 6443 , 308, 122), // #1085
  INST(Vmovsd           , VexMovssMovsd      , V(F20F00,10,_,I,I,1,3,T1S), V(F20F00,11,_,I,I,1,3,T1S), 104, 101, 6449 , 309, 122), // #1086
  INST(Vmovshdup        , VexRm_Lx           , V(F30F00,16,_,x,I,0,4,FVM), 0                         , 185, 0  , 6456 , 310, 121), // #1087
  INST(Vmovsldup        , VexRm_Lx           , V(F30F00,12,_,x,I,0,4,FVM), 0                         , 185, 0  , 6466 , 310, 121), // #1088
  INST(Vmovss           , VexMovssMovsd      , V(F30F00,10,_,I,I,0,2,T1S), V(F30F00,11,_,I,I,0,2,T1S), 105, 102, 6476 , 311, 122), // #1089
  INST(Vmovupd          , VexRmMr_Lx         , V(660F00,10,_,x,I,1,4,FVM), V(660F00,11,_,x,I,1,4,FVM), 172, 103, 6483 , 298, 121), // #1090
  INST(Vmovups          , VexRmMr_Lx         , V(000F00,10,_,x,I,0,4,FVM), V(000F00,11,_,x,I,0,4,FVM), 173, 104, 6491 , 298, 121), // #1091
  INST(Vmpsadbw         , VexRvmi_Lx         , V(660F3A,42,_,x,I,_,_,_  ), 0                         , 72 , 0  , 6499 , 205, 143), // #1092
  INST(Vmptrld          , X86M_Only          , O(000F00,C7,6,_,_,_,_,_  ), 0                         , 78 , 0  , 6508 , 32 , 56 ), // #1093
  INST(Vmptrst          , X86M_Only          , O(000F00,C7,7,_,_,_,_,_  ), 0                         , 22 , 0  , 6516 , 32 , 56 ), // #1094
  INST(Vmread           , X86Mr_NoSize       , O(000F00,78,_,_,_,_,_,_  ), 0                         , 4  , 0  , 6524 , 312, 56 ), // #1095
  INST(Vmresume         , X86Op              , O(000F01,C3,_,_,_,_,_,_  ), 0                         , 21 , 0  , 6531 , 30 , 56 ), // #1096
  INST(Vmrun            , X86Op_xAX          , O(000F01,D8,_,_,_,_,_,_  ), 0                         , 21 , 0  , 6540 , 297, 22 ), // #1097
  INST(Vmsave           , X86Op_xAX          , O(000F01,DB,_,_,_,_,_,_  ), 0                         , 21 , 0  , 6546 , 297, 22 ), // #1098
  INST(Vmulpd           , VexRvm_Lx          , V(660F00,59,_,x,I,1,4,FV ), 0                         , 102, 0  , 6553 , 188, 121), // #1099
  INST(Vmulps           , VexRvm_Lx          , V(000F00,59,_,x,I,0,4,FV ), 0                         , 103, 0  , 6560 , 189, 121), // #1100
  INST(Vmulsd           , VexRvm_Lx          , V(F20F00,59,_,I,I,1,3,T1S), 0                         , 104, 0  , 6567 , 190, 122), // #1101
  INST(Vmulss           , VexRvm_Lx          , V(F30F00,59,_,I,I,0,2,T1S), 0                         , 105, 0  , 6574 , 191, 122), // #1102
  INST(Vmwrite          , X86Rm_NoSize       , O(000F00,79,_,_,_,_,_,_  ), 0                         , 4  , 0  , 6581 , 313, 56 ), // #1103
  INST(Vmxon            , X86M_Only          , O(F30F00,C7,6,_,_,_,_,_  ), 0                         , 24 , 0  , 6589 , 32 , 56 ), // #1104
  INST(Vorpd            , VexRvm_Lx          , V(660F00,56,_,x,I,1,4,FV ), 0                         , 102, 0  , 6595 , 200, 127), // #1105
  INST(Vorps            , VexRvm_Lx          , V(000F00,56,_,x,I,0,4,FV ), 0                         , 103, 0  , 6601 , 201, 127), // #1106
  INST(Vp2intersectd    , VexRvm_Lx_2xK      , E(F20F38,68,_,_,_,0,4,FV ), 0                         , 186, 0  , 6607 , 314, 144), // #1107
  INST(Vp2intersectq    , VexRvm_Lx_2xK      , E(F20F38,68,_,_,_,1,4,FV ), 0                         , 187, 0  , 6621 , 315, 144), // #1108
  INST(Vp4dpwssd        , VexRm_T1_4X        , E(F20F38,52,_,2,_,0,2,T4X), 0                         , 101, 0  , 6635 , 186, 145), // #1109
  INST(Vp4dpwssds       , VexRm_T1_4X        , E(F20F38,53,_,2,_,0,2,T4X), 0                         , 101, 0  , 6645 , 186, 145), // #1110
  INST(Vpabsb           , VexRm_Lx           , V(660F38,1C,_,x,I,_,4,FVM), 0                         , 107, 0  , 6656 , 310, 146), // #1111
  INST(Vpabsd           , VexRm_Lx           , V(660F38,1E,_,x,I,0,4,FV ), 0                         , 163, 0  , 6663 , 310, 131), // #1112
  INST(Vpabsq           , VexRm_Lx           , E(660F38,1F,_,x,_,1,4,FV ), 0                         , 112, 0  , 6670 , 254, 126), // #1113
  INST(Vpabsw           , VexRm_Lx           , V(660F38,1D,_,x,I,_,4,FVM), 0                         , 107, 0  , 6677 , 310, 146), // #1114
  INST(Vpackssdw        , VexRvm_Lx          , V(660F00,6B,_,x,I,0,4,FV ), 0                         , 133, 0  , 6684 , 199, 146), // #1115
  INST(Vpacksswb        , VexRvm_Lx          , V(660F00,63,_,x,I,I,4,FVM), 0                         , 184, 0  , 6694 , 285, 146), // #1116
  INST(Vpackusdw        , VexRvm_Lx          , V(660F38,2B,_,x,I,0,4,FV ), 0                         , 163, 0  , 6704 , 199, 146), // #1117
  INST(Vpackuswb        , VexRvm_Lx          , V(660F00,67,_,x,I,I,4,FVM), 0                         , 184, 0  , 6714 , 285, 146), // #1118
  INST(Vpaddb           , VexRvm_Lx          , V(660F00,FC,_,x,I,I,4,FVM), 0                         , 184, 0  , 6724 , 285, 146), // #1119
  INST(Vpaddd           , VexRvm_Lx          , V(660F00,FE,_,x,I,0,4,FV ), 0                         , 133, 0  , 6731 , 199, 131), // #1120
  INST(Vpaddq           , VexRvm_Lx          , V(660F00,D4,_,x,I,1,4,FV ), 0                         , 102, 0  , 6738 , 198, 131), // #1121
  INST(Vpaddsb          , VexRvm_Lx          , V(660F00,EC,_,x,I,I,4,FVM), 0                         , 184, 0  , 6745 , 285, 146), // #1122
  INST(Vpaddsw          , VexRvm_Lx          , V(660F00,ED,_,x,I,I,4,FVM), 0                         , 184, 0  , 6753 , 285, 146), // #1123
  INST(Vpaddusb         , VexRvm_Lx          , V(660F00,DC,_,x,I,I,4,FVM), 0                         , 184, 0  , 6761 , 285, 146), // #1124
  INST(Vpaddusw         , VexRvm_Lx          , V(660F00,DD,_,x,I,I,4,FVM), 0                         , 184, 0  , 6770 , 285, 146), // #1125
  INST(Vpaddw           , VexRvm_Lx          , V(660F00,FD,_,x,I,I,4,FVM), 0                         , 184, 0  , 6779 , 285, 146), // #1126
  INST(Vpalignr         , VexRvmi_Lx         , V(660F3A,0F,_,x,I,I,4,FVM), 0                         , 188, 0  , 6786 , 284, 146), // #1127
  INST(Vpand            , VexRvm_Lx          , V(660F00,DB,_,x,I,_,_,_  ), 0                         , 68 , 0  , 6795 , 316, 143), // #1128
  INST(Vpandd           , VexRvm_Lx          , E(660F00,DB,_,x,_,0,4,FV ), 0                         , 189, 0  , 6801 , 317, 126), // #1129
  INST(Vpandn           , VexRvm_Lx          , V(660F00,DF,_,x,I,_,_,_  ), 0                         , 68 , 0  , 6808 , 318, 143), // #1130
  INST(Vpandnd          , VexRvm_Lx          , E(660F00,DF,_,x,_,0,4,FV ), 0                         , 189, 0  , 6815 , 319, 126), // #1131
  INST(Vpandnq          , VexRvm_Lx          , E(660F00,DF,_,x,_,1,4,FV ), 0                         , 130, 0  , 6823 , 320, 126), // #1132
  INST(Vpandq           , VexRvm_Lx          , E(660F00,DB,_,x,_,1,4,FV ), 0                         , 130, 0  , 6831 , 321, 126), // #1133
  INST(Vpavgb           , VexRvm_Lx          , V(660F00,E0,_,x,I,I,4,FVM), 0                         , 184, 0  , 6838 , 285, 146), // #1134
  INST(Vpavgw           , VexRvm_Lx          , V(660F00,E3,_,x,I,I,4,FVM), 0                         , 184, 0  , 6845 , 285, 146), // #1135
  INST(Vpblendd         , VexRvmi_Lx         , V(660F3A,02,_,x,0,_,_,_  ), 0                         , 72 , 0  , 6852 , 205, 130), // #1136
  INST(Vpblendvb        , VexRvmr            , V(660F3A,4C,_,x,0,_,_,_  ), 0                         , 72 , 0  , 6861 , 206, 143), // #1137
  INST(Vpblendw         , VexRvmi_Lx         , V(660F3A,0E,_,x,I,_,_,_  ), 0                         , 72 , 0  , 6871 , 205, 143), // #1138
  INST(Vpbroadcastb     , VexRm_Lx_Bcst      , V(660F38,78,_,x,0,0,0,T1S), E(660F38,7A,_,x,0,0,0,T1S), 190, 105, 6880 , 322, 147), // #1139
  INST(Vpbroadcastd     , VexRm_Lx_Bcst      , V(660F38,58,_,x,0,0,2,T1S), E(660F38,7C,_,x,0,0,0,T1S), 121, 106, 6893 , 323, 140), // #1140
  INST(Vpbroadcastmb2d  , VexRm_Lx           , E(F30F38,3A,_,x,_,0,_,_  ), 0                         , 128, 0  , 6906 , 324, 148), // #1141
  INST(Vpbroadcastmb2q  , VexRm_Lx           , E(F30F38,2A,_,x,_,1,_,_  ), 0                         , 191, 0  , 6922 , 324, 148), // #1142
  INST(Vpbroadcastq     , VexRm_Lx_Bcst      , V(660F38,59,_,x,0,1,3,T1S), E(660F38,7C,_,x,0,1,0,T1S), 120, 107, 6938 , 325, 140), // #1143
  INST(Vpbroadcastw     , VexRm_Lx_Bcst      , V(660F38,79,_,x,0,0,1,T1S), E(660F38,7B,_,x,0,0,0,T1S), 192, 108, 6951 , 326, 147), // #1144
  INST(Vpclmulqdq       , VexRvmi_Lx         , V(660F3A,44,_,x,I,_,4,FVM), 0                         , 188, 0  , 6964 , 327, 149), // #1145
  INST(Vpcmov           , VexRvrmRvmr_Lx     , V(XOP_M8,A2,_,x,x,_,_,_  ), 0                         , 193, 0  , 6975 , 263, 139), // #1146
  INST(Vpcmpb           , VexRvmi_Lx         , E(660F3A,3F,_,x,_,0,4,FVM), 0                         , 151, 0  , 6982 , 328, 128), // #1147
  INST(Vpcmpd           , VexRvmi_Lx         , E(660F3A,1F,_,x,_,0,4,FV ), 0                         , 108, 0  , 6989 , 329, 126), // #1148
  INST(Vpcmpeqb         , VexRvm_Lx          , V(660F00,74,_,x,I,I,4,FV ), 0                         , 133, 0  , 6996 , 330, 146), // #1149
  INST(Vpcmpeqd         , VexRvm_Lx          , V(660F00,76,_,x,I,0,4,FVM), 0                         , 184, 0  , 7005 , 331, 131), // #1150
  INST(Vpcmpeqq         , VexRvm_Lx          , V(660F38,29,_,x,I,1,4,FVM), 0                         , 194, 0  , 7014 , 332, 131), // #1151
  INST(Vpcmpeqw         , VexRvm_Lx          , V(660F00,75,_,x,I,I,4,FV ), 0                         , 133, 0  , 7023 , 330, 146), // #1152
  INST(Vpcmpestri       , VexRmi             , V(660F3A,61,_,0,I,_,_,_  ), 0                         , 72 , 0  , 7032 , 333, 150), // #1153
  INST(Vpcmpestrm       , VexRmi             , V(660F3A,60,_,0,I,_,_,_  ), 0                         , 72 , 0  , 7043 , 334, 150), // #1154
  INST(Vpcmpgtb         , VexRvm_Lx          , V(660F00,64,_,x,I,I,4,FV ), 0                         , 133, 0  , 7054 , 330, 146), // #1155
  INST(Vpcmpgtd         , VexRvm_Lx          , V(660F00,66,_,x,I,0,4,FVM), 0                         , 184, 0  , 7063 , 331, 131), // #1156
  INST(Vpcmpgtq         , VexRvm_Lx          , V(660F38,37,_,x,I,1,4,FVM), 0                         , 194, 0  , 7072 , 332, 131), // #1157
  INST(Vpcmpgtw         , VexRvm_Lx          , V(660F00,65,_,x,I,I,4,FV ), 0                         , 133, 0  , 7081 , 330, 146), // #1158
  INST(Vpcmpistri       , VexRmi             , V(660F3A,63,_,0,I,_,_,_  ), 0                         , 72 , 0  , 7090 , 335, 150), // #1159
  INST(Vpcmpistrm       , VexRmi             , V(660F3A,62,_,0,I,_,_,_  ), 0                         , 72 , 0  , 7101 , 336, 150), // #1160
  INST(Vpcmpq           , VexRvmi_Lx         , E(660F3A,1F,_,x,_,1,4,FV ), 0                         , 109, 0  , 7112 , 337, 126), // #1161
  INST(Vpcmpub          , VexRvmi_Lx         , E(660F3A,3E,_,x,_,0,4,FVM), 0                         , 151, 0  , 7119 , 328, 128), // #1162
  INST(Vpcmpud          , VexRvmi_Lx         , E(660F3A,1E,_,x,_,0,4,FV ), 0                         , 108, 0  , 7127 , 329, 126), // #1163
  INST(Vpcmpuq          , VexRvmi_Lx         , E(660F3A,1E,_,x,_,1,4,FV ), 0                         , 109, 0  , 7135 , 337, 126), // #1164
  INST(Vpcmpuw          , VexRvmi_Lx         , E(660F3A,3E,_,x,_,1,4,FVM), 0                         , 195, 0  , 7143 , 337, 128), // #1165
  INST(Vpcmpw           , VexRvmi_Lx         , E(660F3A,3F,_,x,_,1,4,FVM), 0                         , 195, 0  , 7151 , 337, 128), // #1166
  INST(Vpcomb           , VexRvmi            , V(XOP_M8,CC,_,0,0,_,_,_  ), 0                         , 193, 0  , 7158 , 251, 139), // #1167
  INST(Vpcomd           , VexRvmi            , V(XOP_M8,CE,_,0,0,_,_,_  ), 0                         , 193, 0  , 7165 , 251, 139), // #1168
  INST(Vpcompressb      , VexMr_Lx           , E(660F38,63,_,x,_,0,0,T1S), 0                         , 196, 0  , 7172 , 220, 151), // #1169
  INST(Vpcompressd      , VexMr_Lx           , E(660F38,8B,_,x,_,0,2,T1S), 0                         , 125, 0  , 7184 , 220, 126), // #1170
  INST(Vpcompressq      , VexMr_Lx           , E(660F38,8B,_,x,_,1,3,T1S), 0                         , 124, 0  , 7196 , 220, 126), // #1171
  INST(Vpcompressw      , VexMr_Lx           , E(660F38,63,_,x,_,1,1,T1S), 0                         , 197, 0  , 7208 , 220, 151), // #1172
  INST(Vpcomq           , VexRvmi            , V(XOP_M8,CF,_,0,0,_,_,_  ), 0                         , 193, 0  , 7220 , 251, 139), // #1173
  INST(Vpcomub          , VexRvmi            , V(XOP_M8,EC,_,0,0,_,_,_  ), 0                         , 193, 0  , 7227 , 251, 139), // #1174
  INST(Vpcomud          , VexRvmi            , V(XOP_M8,EE,_,0,0,_,_,_  ), 0                         , 193, 0  , 7235 , 251, 139), // #1175
  INST(Vpcomuq          , VexRvmi            , V(XOP_M8,EF,_,0,0,_,_,_  ), 0                         , 193, 0  , 7243 , 251, 139), // #1176
  INST(Vpcomuw          , VexRvmi            , V(XOP_M8,ED,_,0,0,_,_,_  ), 0                         , 193, 0  , 7251 , 251, 139), // #1177
  INST(Vpcomw           , VexRvmi            , V(XOP_M8,CD,_,0,0,_,_,_  ), 0                         , 193, 0  , 7259 , 251, 139), // #1178
  INST(Vpconflictd      , VexRm_Lx           , E(660F38,C4,_,x,_,0,4,FV ), 0                         , 111, 0  , 7266 , 338, 148), // #1179
  INST(Vpconflictq      , VexRm_Lx           , E(660F38,C4,_,x,_,1,4,FV ), 0                         , 112, 0  , 7278 , 338, 148), // #1180
  INST(Vpdpbusd         , VexRvm_Lx          , E(660F38,50,_,x,_,0,4,FV ), 0                         , 111, 0  , 7290 , 203, 152), // #1181
  INST(Vpdpbusds        , VexRvm_Lx          , E(660F38,51,_,x,_,0,4,FV ), 0                         , 111, 0  , 7299 , 203, 152), // #1182
  INST(Vpdpwssd         , VexRvm_Lx          , E(660F38,52,_,x,_,0,4,FV ), 0                         , 111, 0  , 7309 , 203, 152), // #1183
  INST(Vpdpwssds        , VexRvm_Lx          , E(660F38,53,_,x,_,0,4,FV ), 0                         , 111, 0  , 7318 , 203, 152), // #1184
  INST(Vperm2f128       , VexRvmi            , V(660F3A,06,_,1,0,_,_,_  ), 0                         , 154, 0  , 7328 , 339, 123), // #1185
  INST(Vperm2i128       , VexRvmi            , V(660F3A,46,_,1,0,_,_,_  ), 0                         , 154, 0  , 7339 , 339, 130), // #1186
  INST(Vpermb           , VexRvm_Lx          , E(660F38,8D,_,x,_,0,4,FVM), 0                         , 110, 0  , 7350 , 202, 153), // #1187
  INST(Vpermd           , VexRvm_Lx          , V(660F38,36,_,x,0,0,4,FV ), 0                         , 163, 0  , 7357 , 340, 140), // #1188
  INST(Vpermi2b         , VexRvm_Lx          , E(660F38,75,_,x,_,0,4,FVM), 0                         , 110, 0  , 7364 , 202, 153), // #1189
  INST(Vpermi2d         , VexRvm_Lx          , E(660F38,76,_,x,_,0,4,FV ), 0                         , 111, 0  , 7373 , 203, 126), // #1190
  INST(Vpermi2pd        , VexRvm_Lx          , E(660F38,77,_,x,_,1,4,FV ), 0                         , 112, 0  , 7382 , 204, 126), // #1191
  INST(Vpermi2ps        , VexRvm_Lx          , E(660F38,77,_,x,_,0,4,FV ), 0                         , 111, 0  , 7392 , 203, 126), // #1192
  INST(Vpermi2q         , VexRvm_Lx          , E(660F38,76,_,x,_,1,4,FV ), 0                         , 112, 0  , 7402 , 204, 126), // #1193
  INST(Vpermi2w         , VexRvm_Lx          , E(660F38,75,_,x,_,1,4,FVM), 0                         , 113, 0  , 7411 , 202, 128), // #1194
  INST(Vpermil2pd       , VexRvrmiRvmri_Lx   , V(660F3A,49,_,x,x,_,_,_  ), 0                         , 72 , 0  , 7420 , 341, 139), // #1195
  INST(Vpermil2ps       , VexRvrmiRvmri_Lx   , V(660F3A,48,_,x,x,_,_,_  ), 0                         , 72 , 0  , 7431 , 341, 139), // #1196
  INST(Vpermilpd        , VexRvmRmi_Lx       , V(660F38,0D,_,x,0,1,4,FV ), V(660F3A,05,_,x,0,1,4,FV ), 198, 109, 7442 , 342, 121), // #1197
  INST(Vpermilps        , VexRvmRmi_Lx       , V(660F38,0C,_,x,0,0,4,FV ), V(660F3A,04,_,x,0,0,4,FV ), 163, 110, 7452 , 342, 121), // #1198
  INST(Vpermpd          , VexRvmRmi_Lx       , E(660F38,16,_,x,1,1,4,FV ), V(660F3A,01,_,x,1,1,4,FV ), 199, 111, 7462 , 343, 140), // #1199
  INST(Vpermps          , VexRvm_Lx          , V(660F38,16,_,x,0,0,4,FV ), 0                         , 163, 0  , 7470 , 340, 140), // #1200
  INST(Vpermq           , VexRvmRmi_Lx       , V(660F38,36,_,x,_,1,4,FV ), V(660F3A,00,_,x,1,1,4,FV ), 198, 112, 7478 , 343, 140), // #1201
  INST(Vpermt2b         , VexRvm_Lx          , E(660F38,7D,_,x,_,0,4,FVM), 0                         , 110, 0  , 7485 , 202, 153), // #1202
  INST(Vpermt2d         , VexRvm_Lx          , E(660F38,7E,_,x,_,0,4,FV ), 0                         , 111, 0  , 7494 , 203, 126), // #1203
  INST(Vpermt2pd        , VexRvm_Lx          , E(660F38,7F,_,x,_,1,4,FV ), 0                         , 112, 0  , 7503 , 204, 126), // #1204
  INST(Vpermt2ps        , VexRvm_Lx          , E(660F38,7F,_,x,_,0,4,FV ), 0                         , 111, 0  , 7513 , 203, 126), // #1205
  INST(Vpermt2q         , VexRvm_Lx          , E(660F38,7E,_,x,_,1,4,FV ), 0                         , 112, 0  , 7523 , 204, 126), // #1206
  INST(Vpermt2w         , VexRvm_Lx          , E(660F38,7D,_,x,_,1,4,FVM), 0                         , 113, 0  , 7532 , 202, 128), // #1207
  INST(Vpermw           , VexRvm_Lx          , E(660F38,8D,_,x,_,1,4,FVM), 0                         , 113, 0  , 7541 , 202, 128), // #1208
  INST(Vpexpandb        , VexRm_Lx           , E(660F38,62,_,x,_,0,0,T1S), 0                         , 196, 0  , 7548 , 254, 151), // #1209
  INST(Vpexpandd        , VexRm_Lx           , E(660F38,89,_,x,_,0,2,T1S), 0                         , 125, 0  , 7558 , 254, 126), // #1210
  INST(Vpexpandq        , VexRm_Lx           , E(660F38,89,_,x,_,1,3,T1S), 0                         , 124, 0  , 7568 , 254, 126), // #1211
  INST(Vpexpandw        , VexRm_Lx           , E(660F38,62,_,x,_,1,1,T1S), 0                         , 197, 0  , 7578 , 254, 151), // #1212
  INST(Vpextrb          , VexMri             , V(660F3A,14,_,0,0,I,0,T1S), 0                         , 200, 0  , 7588 , 344, 154), // #1213
  INST(Vpextrd          , VexMri             , V(660F3A,16,_,0,0,0,2,T1S), 0                         , 159, 0  , 7596 , 258, 155), // #1214
  INST(Vpextrq          , VexMri             , V(660F3A,16,_,0,1,1,3,T1S), 0                         , 201, 0  , 7604 , 345, 155), // #1215
  INST(Vpextrw          , VexMri             , V(660F3A,15,_,0,0,I,1,T1S), 0                         , 202, 0  , 7612 , 346, 154), // #1216
  INST(Vpgatherdd       , VexRmvRm_VM        , V(660F38,90,_,x,0,_,_,_  ), V(660F38,90,_,x,_,0,2,T1S), 96 , 113, 7620 , 274, 140), // #1217
  INST(Vpgatherdq       , VexRmvRm_VM        , V(660F38,90,_,x,1,_,_,_  ), V(660F38,90,_,x,_,1,3,T1S), 165, 114, 7631 , 273, 140), // #1218
  INST(Vpgatherqd       , VexRmvRm_VM        , V(660F38,91,_,x,0,_,_,_  ), V(660F38,91,_,x,_,0,2,T1S), 96 , 115, 7642 , 279, 140), // #1219
  INST(Vpgatherqq       , VexRmvRm_VM        , V(660F38,91,_,x,1,_,_,_  ), V(660F38,91,_,x,_,1,3,T1S), 165, 116, 7653 , 278, 140), // #1220
  INST(Vphaddbd         , VexRm              , V(XOP_M9,C2,_,0,0,_,_,_  ), 0                         , 77 , 0  , 7664 , 194, 139), // #1221
  INST(Vphaddbq         , VexRm              , V(XOP_M9,C3,_,0,0,_,_,_  ), 0                         , 77 , 0  , 7673 , 194, 139), // #1222
  INST(Vphaddbw         , VexRm              , V(XOP_M9,C1,_,0,0,_,_,_  ), 0                         , 77 , 0  , 7682 , 194, 139), // #1223
  INST(Vphaddd          , VexRvm_Lx          , V(660F38,02,_,x,I,_,_,_  ), 0                         , 96 , 0  , 7691 , 192, 143), // #1224
  INST(Vphadddq         , VexRm              , V(XOP_M9,CB,_,0,0,_,_,_  ), 0                         , 77 , 0  , 7699 , 194, 139), // #1225
  INST(Vphaddsw         , VexRvm_Lx          , V(660F38,03,_,x,I,_,_,_  ), 0                         , 96 , 0  , 7708 , 192, 143), // #1226
  INST(Vphaddubd        , VexRm              , V(XOP_M9,D2,_,0,0,_,_,_  ), 0                         , 77 , 0  , 7717 , 194, 139), // #1227
  INST(Vphaddubq        , VexRm              , V(XOP_M9,D3,_,0,0,_,_,_  ), 0                         , 77 , 0  , 7727 , 194, 139), // #1228
  INST(Vphaddubw        , VexRm              , V(XOP_M9,D1,_,0,0,_,_,_  ), 0                         , 77 , 0  , 7737 , 194, 139), // #1229
  INST(Vphaddudq        , VexRm              , V(XOP_M9,DB,_,0,0,_,_,_  ), 0                         , 77 , 0  , 7747 , 194, 139), // #1230
  INST(Vphadduwd        , VexRm              , V(XOP_M9,D6,_,0,0,_,_,_  ), 0                         , 77 , 0  , 7757 , 194, 139), // #1231
  INST(Vphadduwq        , VexRm              , V(XOP_M9,D7,_,0,0,_,_,_  ), 0                         , 77 , 0  , 7767 , 194, 139), // #1232
  INST(Vphaddw          , VexRvm_Lx          , V(660F38,01,_,x,I,_,_,_  ), 0                         , 96 , 0  , 7777 , 192, 143), // #1233
  INST(Vphaddwd         , VexRm              , V(XOP_M9,C6,_,0,0,_,_,_  ), 0                         , 77 , 0  , 7785 , 194, 139), // #1234
  INST(Vphaddwq         , VexRm              , V(XOP_M9,C7,_,0,0,_,_,_  ), 0                         , 77 , 0  , 7794 , 194, 139), // #1235
  INST(Vphminposuw      , VexRm              , V(660F38,41,_,0,I,_,_,_  ), 0                         , 96 , 0  , 7803 , 194, 123), // #1236
  INST(Vphsubbw         , VexRm              , V(XOP_M9,E1,_,0,0,_,_,_  ), 0                         , 77 , 0  , 7815 , 194, 139), // #1237
  INST(Vphsubd          , VexRvm_Lx          , V(660F38,06,_,x,I,_,_,_  ), 0                         , 96 , 0  , 7824 , 192, 143), // #1238
  INST(Vphsubdq         , VexRm              , V(XOP_M9,E3,_,0,0,_,_,_  ), 0                         , 77 , 0  , 7832 , 194, 139), // #1239
  INST(Vphsubsw         , VexRvm_Lx          , V(660F38,07,_,x,I,_,_,_  ), 0                         , 96 , 0  , 7841 , 192, 143), // #1240
  INST(Vphsubw          , VexRvm_Lx          , V(660F38,05,_,x,I,_,_,_  ), 0                         , 96 , 0  , 7850 , 192, 143), // #1241
  INST(Vphsubwd         , VexRm              , V(XOP_M9,E2,_,0,0,_,_,_  ), 0                         , 77 , 0  , 7858 , 194, 139), // #1242
  INST(Vpinsrb          , VexRvmi            , V(660F3A,20,_,0,0,I,0,T1S), 0                         , 200, 0  , 7867 , 347, 154), // #1243
  INST(Vpinsrd          , VexRvmi            , V(660F3A,22,_,0,0,0,2,T1S), 0                         , 159, 0  , 7875 , 348, 155), // #1244
  INST(Vpinsrq          , VexRvmi            , V(660F3A,22,_,0,1,1,3,T1S), 0                         , 201, 0  , 7883 , 349, 155), // #1245
  INST(Vpinsrw          , VexRvmi            , V(660F00,C4,_,0,0,I,1,T1S), 0                         , 203, 0  , 7891 , 350, 154), // #1246
  INST(Vplzcntd         , VexRm_Lx           , E(660F38,44,_,x,_,0,4,FV ), 0                         , 111, 0  , 7899 , 338, 148), // #1247
  INST(Vplzcntq         , VexRm_Lx           , E(660F38,44,_,x,_,1,4,FV ), 0                         , 112, 0  , 7908 , 351, 148), // #1248
  INST(Vpmacsdd         , VexRvmr            , V(XOP_M8,9E,_,0,0,_,_,_  ), 0                         , 193, 0  , 7917 , 352, 139), // #1249
  INST(Vpmacsdqh        , VexRvmr            , V(XOP_M8,9F,_,0,0,_,_,_  ), 0                         , 193, 0  , 7926 , 352, 139), // #1250
  INST(Vpmacsdql        , VexRvmr            , V(XOP_M8,97,_,0,0,_,_,_  ), 0                         , 193, 0  , 7936 , 352, 139), // #1251
  INST(Vpmacssdd        , VexRvmr            , V(XOP_M8,8E,_,0,0,_,_,_  ), 0                         , 193, 0  , 7946 , 352, 139), // #1252
  INST(Vpmacssdqh       , VexRvmr            , V(XOP_M8,8F,_,0,0,_,_,_  ), 0                         , 193, 0  , 7956 , 352, 139), // #1253
  INST(Vpmacssdql       , VexRvmr            , V(XOP_M8,87,_,0,0,_,_,_  ), 0                         , 193, 0  , 7967 , 352, 139), // #1254
  INST(Vpmacsswd        , VexRvmr            , V(XOP_M8,86,_,0,0,_,_,_  ), 0                         , 193, 0  , 7978 , 352, 139), // #1255
  INST(Vpmacssww        , VexRvmr            , V(XOP_M8,85,_,0,0,_,_,_  ), 0                         , 193, 0  , 7988 , 352, 139), // #1256
  INST(Vpmacswd         , VexRvmr            , V(XOP_M8,96,_,0,0,_,_,_  ), 0                         , 193, 0  , 7998 , 352, 139), // #1257
  INST(Vpmacsww         , VexRvmr            , V(XOP_M8,95,_,0,0,_,_,_  ), 0                         , 193, 0  , 8007 , 352, 139), // #1258
  INST(Vpmadcsswd       , VexRvmr            , V(XOP_M8,A6,_,0,0,_,_,_  ), 0                         , 193, 0  , 8016 , 352, 139), // #1259
  INST(Vpmadcswd        , VexRvmr            , V(XOP_M8,B6,_,0,0,_,_,_  ), 0                         , 193, 0  , 8027 , 352, 139), // #1260
  INST(Vpmadd52huq      , VexRvm_Lx          , E(660F38,B5,_,x,_,1,4,FV ), 0                         , 112, 0  , 8037 , 204, 156), // #1261
  INST(Vpmadd52luq      , VexRvm_Lx          , E(660F38,B4,_,x,_,1,4,FV ), 0                         , 112, 0  , 8049 , 204, 156), // #1262
  INST(Vpmaddubsw       , VexRvm_Lx          , V(660F38,04,_,x,I,I,4,FVM), 0                         , 107, 0  , 8061 , 285, 146), // #1263
  INST(Vpmaddwd         , VexRvm_Lx          , V(660F00,F5,_,x,I,I,4,FVM), 0                         , 184, 0  , 8072 , 285, 146), // #1264
  INST(Vpmaskmovd       , VexRvmMvr_Lx       , V(660F38,8C,_,x,0,_,_,_  ), V(660F38,8E,_,x,0,_,_,_  ), 96 , 117, 8081 , 293, 130), // #1265
  INST(Vpmaskmovq       , VexRvmMvr_Lx       , V(660F38,8C,_,x,1,_,_,_  ), V(660F38,8E,_,x,1,_,_,_  ), 165, 118, 8092 , 293, 130), // #1266
  INST(Vpmaxsb          , VexRvm_Lx          , V(660F38,3C,_,x,I,I,4,FVM), 0                         , 107, 0  , 8103 , 353, 146), // #1267
  INST(Vpmaxsd          , VexRvm_Lx          , V(660F38,3D,_,x,I,0,4,FV ), 0                         , 163, 0  , 8111 , 201, 131), // #1268
  INST(Vpmaxsq          , VexRvm_Lx          , E(660F38,3D,_,x,_,1,4,FV ), 0                         , 112, 0  , 8119 , 204, 126), // #1269
  INST(Vpmaxsw          , VexRvm_Lx          , V(660F00,EE,_,x,I,I,4,FVM), 0                         , 184, 0  , 8127 , 353, 146), // #1270
  INST(Vpmaxub          , VexRvm_Lx          , V(660F00,DE,_,x,I,I,4,FVM), 0                         , 184, 0  , 8135 , 353, 146), // #1271
  INST(Vpmaxud          , VexRvm_Lx          , V(660F38,3F,_,x,I,0,4,FV ), 0                         , 163, 0  , 8143 , 201, 131), // #1272
  INST(Vpmaxuq          , VexRvm_Lx          , E(660F38,3F,_,x,_,1,4,FV ), 0                         , 112, 0  , 8151 , 204, 126), // #1273
  INST(Vpmaxuw          , VexRvm_Lx          , V(660F38,3E,_,x,I,I,4,FVM), 0                         , 107, 0  , 8159 , 353, 146), // #1274
  INST(Vpminsb          , VexRvm_Lx          , V(660F38,38,_,x,I,I,4,FVM), 0                         , 107, 0  , 8167 , 353, 146), // #1275
  INST(Vpminsd          , VexRvm_Lx          , V(660F38,39,_,x,I,0,4,FV ), 0                         , 163, 0  , 8175 , 201, 131), // #1276
  INST(Vpminsq          , VexRvm_Lx          , E(660F38,39,_,x,_,1,4,FV ), 0                         , 112, 0  , 8183 , 204, 126), // #1277
  INST(Vpminsw          , VexRvm_Lx          , V(660F00,EA,_,x,I,I,4,FVM), 0                         , 184, 0  , 8191 , 353, 146), // #1278
  INST(Vpminub          , VexRvm_Lx          , V(660F00,DA,_,x,I,_,4,FVM), 0                         , 184, 0  , 8199 , 353, 146), // #1279
  INST(Vpminud          , VexRvm_Lx          , V(660F38,3B,_,x,I,0,4,FV ), 0                         , 163, 0  , 8207 , 201, 131), // #1280
  INST(Vpminuq          , VexRvm_Lx          , E(660F38,3B,_,x,_,1,4,FV ), 0                         , 112, 0  , 8215 , 204, 126), // #1281
  INST(Vpminuw          , VexRvm_Lx          , V(660F38,3A,_,x,I,_,4,FVM), 0                         , 107, 0  , 8223 , 353, 146), // #1282
  INST(Vpmovb2m         , VexRm_Lx           , E(F30F38,29,_,x,_,0,_,_  ), 0                         , 128, 0  , 8231 , 354, 128), // #1283
  INST(Vpmovd2m         , VexRm_Lx           , E(F30F38,39,_,x,_,0,_,_  ), 0                         , 128, 0  , 8240 , 354, 129), // #1284
  INST(Vpmovdb          , VexMr_Lx           , E(F30F38,31,_,x,_,0,2,QVM), 0                         , 204, 0  , 8249 , 355, 126), // #1285
  INST(Vpmovdw          , VexMr_Lx           , E(F30F38,33,_,x,_,0,3,HVM), 0                         , 205, 0  , 8257 , 356, 126), // #1286
  INST(Vpmovm2b         , VexRm_Lx           , E(F30F38,28,_,x,_,0,_,_  ), 0                         , 128, 0  , 8265 , 324, 128), // #1287
  INST(Vpmovm2d         , VexRm_Lx           , E(F30F38,38,_,x,_,0,_,_  ), 0                         , 128, 0  , 8274 , 324, 129), // #1288
  INST(Vpmovm2q         , VexRm_Lx           , E(F30F38,38,_,x,_,1,_,_  ), 0                         , 191, 0  , 8283 , 324, 129), // #1289
  INST(Vpmovm2w         , VexRm_Lx           , E(F30F38,28,_,x,_,1,_,_  ), 0                         , 191, 0  , 8292 , 324, 128), // #1290
  INST(Vpmovmskb        , VexRm_Lx           , V(660F00,D7,_,x,I,_,_,_  ), 0                         , 68 , 0  , 8301 , 305, 143), // #1291
  INST(Vpmovq2m         , VexRm_Lx           , E(F30F38,39,_,x,_,1,_,_  ), 0                         , 191, 0  , 8311 , 354, 129), // #1292
  INST(Vpmovqb          , VexMr_Lx           , E(F30F38,32,_,x,_,0,1,OVM), 0                         , 206, 0  , 8320 , 357, 126), // #1293
  INST(Vpmovqd          , VexMr_Lx           , E(F30F38,35,_,x,_,0,3,HVM), 0                         , 205, 0  , 8328 , 356, 126), // #1294
  INST(Vpmovqw          , VexMr_Lx           , E(F30F38,34,_,x,_,0,2,QVM), 0                         , 204, 0  , 8336 , 355, 126), // #1295
  INST(Vpmovsdb         , VexMr_Lx           , E(F30F38,21,_,x,_,0,2,QVM), 0                         , 204, 0  , 8344 , 355, 126), // #1296
  INST(Vpmovsdw         , VexMr_Lx           , E(F30F38,23,_,x,_,0,3,HVM), 0                         , 205, 0  , 8353 , 356, 126), // #1297
  INST(Vpmovsqb         , VexMr_Lx           , E(F30F38,22,_,x,_,0,1,OVM), 0                         , 206, 0  , 8362 , 357, 126), // #1298
  INST(Vpmovsqd         , VexMr_Lx           , E(F30F38,25,_,x,_,0,3,HVM), 0                         , 205, 0  , 8371 , 356, 126), // #1299
  INST(Vpmovsqw         , VexMr_Lx           , E(F30F38,24,_,x,_,0,2,QVM), 0                         , 204, 0  , 8380 , 355, 126), // #1300
  INST(Vpmovswb         , VexMr_Lx           , E(F30F38,20,_,x,_,0,3,HVM), 0                         , 205, 0  , 8389 , 356, 128), // #1301
  INST(Vpmovsxbd        , VexRm_Lx           , V(660F38,21,_,x,I,I,2,QVM), 0                         , 207, 0  , 8398 , 358, 131), // #1302
  INST(Vpmovsxbq        , VexRm_Lx           , V(660F38,22,_,x,I,I,1,OVM), 0                         , 208, 0  , 8408 , 359, 131), // #1303
  INST(Vpmovsxbw        , VexRm_Lx           , V(660F38,20,_,x,I,I,3,HVM), 0                         , 132, 0  , 8418 , 360, 146), // #1304
  INST(Vpmovsxdq        , VexRm_Lx           , V(660F38,25,_,x,I,0,3,HVM), 0                         , 132, 0  , 8428 , 360, 131), // #1305
  INST(Vpmovsxwd        , VexRm_Lx           , V(660F38,23,_,x,I,I,3,HVM), 0                         , 132, 0  , 8438 , 360, 131), // #1306
  INST(Vpmovsxwq        , VexRm_Lx           , V(660F38,24,_,x,I,I,2,QVM), 0                         , 207, 0  , 8448 , 358, 131), // #1307
  INST(Vpmovusdb        , VexMr_Lx           , E(F30F38,11,_,x,_,0,2,QVM), 0                         , 204, 0  , 8458 , 355, 126), // #1308
  INST(Vpmovusdw        , VexMr_Lx           , E(F30F38,13,_,x,_,0,3,HVM), 0                         , 205, 0  , 8468 , 356, 126), // #1309
  INST(Vpmovusqb        , VexMr_Lx           , E(F30F38,12,_,x,_,0,1,OVM), 0                         , 206, 0  , 8478 , 357, 126), // #1310
  INST(Vpmovusqd        , VexMr_Lx           , E(F30F38,15,_,x,_,0,3,HVM), 0                         , 205, 0  , 8488 , 356, 126), // #1311
  INST(Vpmovusqw        , VexMr_Lx           , E(F30F38,14,_,x,_,0,2,QVM), 0                         , 204, 0  , 8498 , 355, 126), // #1312
  INST(Vpmovuswb        , VexMr_Lx           , E(F30F38,10,_,x,_,0,3,HVM), 0                         , 205, 0  , 8508 , 356, 128), // #1313
  INST(Vpmovw2m         , VexRm_Lx           , E(F30F38,29,_,x,_,1,_,_  ), 0                         , 191, 0  , 8518 , 354, 128), // #1314
  INST(Vpmovwb          , VexMr_Lx           , E(F30F38,30,_,x,_,0,3,HVM), 0                         , 205, 0  , 8527 , 356, 128), // #1315
  INST(Vpmovzxbd        , VexRm_Lx           , V(660F38,31,_,x,I,I,2,QVM), 0                         , 207, 0  , 8535 , 358, 131), // #1316
  INST(Vpmovzxbq        , VexRm_Lx           , V(660F38,32,_,x,I,I,1,OVM), 0                         , 208, 0  , 8545 , 359, 131), // #1317
  INST(Vpmovzxbw        , VexRm_Lx           , V(660F38,30,_,x,I,I,3,HVM), 0                         , 132, 0  , 8555 , 360, 146), // #1318
  INST(Vpmovzxdq        , VexRm_Lx           , V(660F38,35,_,x,I,0,3,HVM), 0                         , 132, 0  , 8565 , 360, 131), // #1319
  INST(Vpmovzxwd        , VexRm_Lx           , V(660F38,33,_,x,I,I,3,HVM), 0                         , 132, 0  , 8575 , 360, 131), // #1320
  INST(Vpmovzxwq        , VexRm_Lx           , V(660F38,34,_,x,I,I,2,QVM), 0                         , 207, 0  , 8585 , 358, 131), // #1321
  INST(Vpmuldq          , VexRvm_Lx          , V(660F38,28,_,x,I,1,4,FV ), 0                         , 198, 0  , 8595 , 198, 131), // #1322
  INST(Vpmulhrsw        , VexRvm_Lx          , V(660F38,0B,_,x,I,I,4,FVM), 0                         , 107, 0  , 8603 , 285, 146), // #1323
  INST(Vpmulhuw         , VexRvm_Lx          , V(660F00,E4,_,x,I,I,4,FVM), 0                         , 184, 0  , 8613 , 285, 146), // #1324
  INST(Vpmulhw          , VexRvm_Lx          , V(660F00,E5,_,x,I,I,4,FVM), 0                         , 184, 0  , 8622 , 285, 146), // #1325
  INST(Vpmulld          , VexRvm_Lx          , V(660F38,40,_,x,I,0,4,FV ), 0                         , 163, 0  , 8630 , 199, 131), // #1326
  INST(Vpmullq          , VexRvm_Lx          , E(660F38,40,_,x,_,1,4,FV ), 0                         , 112, 0  , 8638 , 204, 129), // #1327
  INST(Vpmullw          , VexRvm_Lx          , V(660F00,D5,_,x,I,I,4,FVM), 0                         , 184, 0  , 8646 , 285, 146), // #1328
  INST(Vpmultishiftqb   , VexRvm_Lx          , E(660F38,83,_,x,_,1,4,FV ), 0                         , 112, 0  , 8654 , 204, 153), // #1329
  INST(Vpmuludq         , VexRvm_Lx          , V(660F00,F4,_,x,I,1,4,FV ), 0                         , 102, 0  , 8669 , 198, 131), // #1330
  INST(Vpopcntb         , VexRm_Lx           , E(660F38,54,_,x,_,0,4,FV ), 0                         , 111, 0  , 8678 , 254, 157), // #1331
  INST(Vpopcntd         , VexRm_Lx           , E(660F38,55,_,x,_,0,4,FVM), 0                         , 110, 0  , 8687 , 338, 158), // #1332
  INST(Vpopcntq         , VexRm_Lx           , E(660F38,55,_,x,_,1,4,FVM), 0                         , 113, 0  , 8696 , 351, 158), // #1333
  INST(Vpopcntw         , VexRm_Lx           , E(660F38,54,_,x,_,1,4,FV ), 0                         , 112, 0  , 8705 , 254, 157), // #1334
  INST(Vpor             , VexRvm_Lx          , V(660F00,EB,_,x,I,_,_,_  ), 0                         , 68 , 0  , 8714 , 316, 143), // #1335
  INST(Vpord            , VexRvm_Lx          , E(660F00,EB,_,x,_,0,4,FV ), 0                         , 189, 0  , 8719 , 317, 126), // #1336
  INST(Vporq            , VexRvm_Lx          , E(660F00,EB,_,x,_,1,4,FV ), 0                         , 130, 0  , 8725 , 321, 126), // #1337
  INST(Vpperm           , VexRvrmRvmr        , V(XOP_M8,A3,_,0,x,_,_,_  ), 0                         , 193, 0  , 8731 , 361, 139), // #1338
  INST(Vprold           , VexVmi_Lx          , E(660F00,72,1,x,_,0,4,FV ), 0                         , 209, 0  , 8738 , 362, 126), // #1339
  INST(Vprolq           , VexVmi_Lx          , E(660F00,72,1,x,_,1,4,FV ), 0                         , 210, 0  , 8745 , 363, 126), // #1340
  INST(Vprolvd          , VexRvm_Lx          , E(660F38,15,_,x,_,0,4,FV ), 0                         , 111, 0  , 8752 , 203, 126), // #1341
  INST(Vprolvq          , VexRvm_Lx          , E(660F38,15,_,x,_,1,4,FV ), 0                         , 112, 0  , 8760 , 204, 126), // #1342
  INST(Vprord           , VexVmi_Lx          , E(660F00,72,0,x,_,0,4,FV ), 0                         , 189, 0  , 8768 , 362, 126), // #1343
  INST(Vprorq           , VexVmi_Lx          , E(660F00,72,0,x,_,1,4,FV ), 0                         , 130, 0  , 8775 , 363, 126), // #1344
  INST(Vprorvd          , VexRvm_Lx          , E(660F38,14,_,x,_,0,4,FV ), 0                         , 111, 0  , 8782 , 203, 126), // #1345
  INST(Vprorvq          , VexRvm_Lx          , E(660F38,14,_,x,_,1,4,FV ), 0                         , 112, 0  , 8790 , 204, 126), // #1346
  INST(Vprotb           , VexRvmRmvRmi       , V(XOP_M9,90,_,0,x,_,_,_  ), V(XOP_M8,C0,_,0,x,_,_,_  ), 77 , 119, 8798 , 364, 139), // #1347
  INST(Vprotd           , VexRvmRmvRmi       , V(XOP_M9,92,_,0,x,_,_,_  ), V(XOP_M8,C2,_,0,x,_,_,_  ), 77 , 120, 8805 , 364, 139), // #1348
  INST(Vprotq           , VexRvmRmvRmi       , V(XOP_M9,93,_,0,x,_,_,_  ), V(XOP_M8,C3,_,0,x,_,_,_  ), 77 , 121, 8812 , 364, 139), // #1349
  INST(Vprotw           , VexRvmRmvRmi       , V(XOP_M9,91,_,0,x,_,_,_  ), V(XOP_M8,C1,_,0,x,_,_,_  ), 77 , 122, 8819 , 364, 139), // #1350
  INST(Vpsadbw          , VexRvm_Lx          , V(660F00,F6,_,x,I,I,4,FVM), 0                         , 184, 0  , 8826 , 193, 146), // #1351
  INST(Vpscatterdd      , VexMr_VM           , E(660F38,A0,_,x,_,0,2,T1S), 0                         , 125, 0  , 8834 , 365, 126), // #1352
  INST(Vpscatterdq      , VexMr_VM           , E(660F38,A0,_,x,_,1,3,T1S), 0                         , 124, 0  , 8846 , 365, 126), // #1353
  INST(Vpscatterqd      , VexMr_VM           , E(660F38,A1,_,x,_,0,2,T1S), 0                         , 125, 0  , 8858 , 366, 126), // #1354
  INST(Vpscatterqq      , VexMr_VM           , E(660F38,A1,_,x,_,1,3,T1S), 0                         , 124, 0  , 8870 , 367, 126), // #1355
  INST(Vpshab           , VexRvmRmv          , V(XOP_M9,98,_,0,x,_,_,_  ), 0                         , 77 , 0  , 8882 , 368, 139), // #1356
  INST(Vpshad           , VexRvmRmv          , V(XOP_M9,9A,_,0,x,_,_,_  ), 0                         , 77 , 0  , 8889 , 368, 139), // #1357
  INST(Vpshaq           , VexRvmRmv          , V(XOP_M9,9B,_,0,x,_,_,_  ), 0                         , 77 , 0  , 8896 , 368, 139), // #1358
  INST(Vpshaw           , VexRvmRmv          , V(XOP_M9,99,_,0,x,_,_,_  ), 0                         , 77 , 0  , 8903 , 368, 139), // #1359
  INST(Vpshlb           , VexRvmRmv          , V(XOP_M9,94,_,0,x,_,_,_  ), 0                         , 77 , 0  , 8910 , 368, 139), // #1360
  INST(Vpshld           , VexRvmRmv          , V(XOP_M9,96,_,0,x,_,_,_  ), 0                         , 77 , 0  , 8917 , 368, 139), // #1361
  INST(Vpshldd          , VexRvmi_Lx         , E(660F3A,71,_,x,_,0,4,FV ), 0                         , 108, 0  , 8924 , 196, 151), // #1362
  INST(Vpshldq          , VexRvmi_Lx         , E(660F3A,71,_,x,_,1,4,FV ), 0                         , 109, 0  , 8932 , 197, 151), // #1363
  INST(Vpshldvd         , VexRvm_Lx          , E(660F38,71,_,x,_,0,4,FV ), 0                         , 111, 0  , 8940 , 203, 151), // #1364
  INST(Vpshldvq         , VexRvm_Lx          , E(660F38,71,_,x,_,1,4,FV ), 0                         , 112, 0  , 8949 , 204, 151), // #1365
  INST(Vpshldvw         , VexRvm_Lx          , E(660F38,70,_,x,_,0,4,FVM), 0                         , 110, 0  , 8958 , 202, 151), // #1366
  INST(Vpshldw          , VexRvmi_Lx         , E(660F3A,70,_,x,_,0,4,FVM), 0                         , 151, 0  , 8967 , 250, 151), // #1367
  INST(Vpshlq           , VexRvmRmv          , V(XOP_M9,97,_,0,x,_,_,_  ), 0                         , 77 , 0  , 8975 , 368, 139), // #1368
  INST(Vpshlw           , VexRvmRmv          , V(XOP_M9,95,_,0,x,_,_,_  ), 0                         , 77 , 0  , 8982 , 368, 139), // #1369
  INST(Vpshrdd          , VexRvmi_Lx         , E(660F3A,73,_,x,_,0,4,FV ), 0                         , 108, 0  , 8989 , 196, 151), // #1370
  INST(Vpshrdq          , VexRvmi_Lx         , E(660F3A,73,_,x,_,1,4,FV ), 0                         , 109, 0  , 8997 , 197, 151), // #1371
  INST(Vpshrdvd         , VexRvm_Lx          , E(660F38,73,_,x,_,0,4,FV ), 0                         , 111, 0  , 9005 , 203, 151), // #1372
  INST(Vpshrdvq         , VexRvm_Lx          , E(660F38,73,_,x,_,1,4,FV ), 0                         , 112, 0  , 9014 , 204, 151), // #1373
  INST(Vpshrdvw         , VexRvm_Lx          , E(660F38,72,_,x,_,0,4,FVM), 0                         , 110, 0  , 9023 , 202, 151), // #1374
  INST(Vpshrdw          , VexRvmi_Lx         , E(660F3A,72,_,x,_,0,4,FVM), 0                         , 151, 0  , 9032 , 250, 151), // #1375
  INST(Vpshufb          , VexRvm_Lx          , V(660F38,00,_,x,I,I,4,FVM), 0                         , 107, 0  , 9040 , 285, 146), // #1376
  INST(Vpshufbitqmb     , VexRvm_Lx          , E(660F38,8F,_,x,0,0,4,FVM), 0                         , 110, 0  , 9048 , 369, 157), // #1377
  INST(Vpshufd          , VexRmi_Lx          , V(660F00,70,_,x,I,0,4,FV ), 0                         , 133, 0  , 9061 , 370, 131), // #1378
  INST(Vpshufhw         , VexRmi_Lx          , V(F30F00,70,_,x,I,I,4,FVM), 0                         , 185, 0  , 9069 , 371, 146), // #1379
  INST(Vpshuflw         , VexRmi_Lx          , V(F20F00,70,_,x,I,I,4,FVM), 0                         , 211, 0  , 9078 , 371, 146), // #1380
  INST(Vpsignb          , VexRvm_Lx          , V(660F38,08,_,x,I,_,_,_  ), 0                         , 96 , 0  , 9087 , 192, 143), // #1381
  INST(Vpsignd          , VexRvm_Lx          , V(660F38,0A,_,x,I,_,_,_  ), 0                         , 96 , 0  , 9095 , 192, 143), // #1382
  INST(Vpsignw          , VexRvm_Lx          , V(660F38,09,_,x,I,_,_,_  ), 0                         , 96 , 0  , 9103 , 192, 143), // #1383
  INST(Vpslld           , VexRvmVmi_Lx       , V(660F00,F2,_,x,I,0,4,128), V(660F00,72,6,x,I,0,4,FV ), 212, 123, 9111 , 372, 131), // #1384
  INST(Vpslldq          , VexEvexVmi_Lx      , V(660F00,73,7,x,I,I,4,FVM), 0                         , 213, 0  , 9118 , 373, 146), // #1385
  INST(Vpsllq           , VexRvmVmi_Lx       , V(660F00,F3,_,x,I,1,4,128), V(660F00,73,6,x,I,1,4,FV ), 214, 124, 9126 , 374, 131), // #1386
  INST(Vpsllvd          , VexRvm_Lx          , V(660F38,47,_,x,0,0,4,FV ), 0                         , 163, 0  , 9133 , 199, 140), // #1387
  INST(Vpsllvq          , VexRvm_Lx          , V(660F38,47,_,x,1,1,4,FV ), 0                         , 162, 0  , 9141 , 198, 140), // #1388
  INST(Vpsllvw          , VexRvm_Lx          , E(660F38,12,_,x,_,1,4,FVM), 0                         , 113, 0  , 9149 , 202, 128), // #1389
  INST(Vpsllw           , VexRvmVmi_Lx       , V(660F00,F1,_,x,I,I,4,FVM), V(660F00,71,6,x,I,I,4,FVM), 184, 125, 9157 , 375, 146), // #1390
  INST(Vpsrad           , VexRvmVmi_Lx       , V(660F00,E2,_,x,I,0,4,128), V(660F00,72,4,x,I,0,4,FV ), 212, 126, 9164 , 372, 131), // #1391
  INST(Vpsraq           , VexRvmVmi_Lx       , E(660F00,E2,_,x,_,1,4,128), E(660F00,72,4,x,_,1,4,FV ), 215, 127, 9171 , 376, 126), // #1392
  INST(Vpsravd          , VexRvm_Lx          , V(660F38,46,_,x,0,0,4,FV ), 0                         , 163, 0  , 9178 , 199, 140), // #1393
  INST(Vpsravq          , VexRvm_Lx          , E(660F38,46,_,x,_,1,4,FV ), 0                         , 112, 0  , 9186 , 204, 126), // #1394
  INST(Vpsravw          , VexRvm_Lx          , E(660F38,11,_,x,_,1,4,FVM), 0                         , 113, 0  , 9194 , 202, 128), // #1395
  INST(Vpsraw           , VexRvmVmi_Lx       , V(660F00,E1,_,x,I,I,4,128), V(660F00,71,4,x,I,I,4,FVM), 212, 128, 9202 , 375, 146), // #1396
  INST(Vpsrld           , VexRvmVmi_Lx       , V(660F00,D2,_,x,I,0,4,128), V(660F00,72,2,x,I,0,4,FV ), 212, 129, 9209 , 372, 131), // #1397
  INST(Vpsrldq          , VexEvexVmi_Lx      , V(660F00,73,3,x,I,I,4,FVM), 0                         , 216, 0  , 9216 , 373, 146), // #1398
  INST(Vpsrlq           , VexRvmVmi_Lx       , V(660F00,D3,_,x,I,1,4,128), V(660F00,73,2,x,I,1,4,FV ), 214, 130, 9224 , 374, 131), // #1399
  INST(Vpsrlvd          , VexRvm_Lx          , V(660F38,45,_,x,0,0,4,FV ), 0                         , 163, 0  , 9231 , 199, 140), // #1400
  INST(Vpsrlvq          , VexRvm_Lx          , V(660F38,45,_,x,1,1,4,FV ), 0                         , 162, 0  , 9239 , 198, 140), // #1401
  INST(Vpsrlvw          , VexRvm_Lx          , E(660F38,10,_,x,_,1,4,FVM), 0                         , 113, 0  , 9247 , 202, 128), // #1402
  INST(Vpsrlw           , VexRvmVmi_Lx       , V(660F00,D1,_,x,I,I,4,128), V(660F00,71,2,x,I,I,4,FVM), 212, 131, 9255 , 375, 146), // #1403
  INST(Vpsubb           , VexRvm_Lx          , V(660F00,F8,_,x,I,I,4,FVM), 0                         , 184, 0  , 9262 , 377, 146), // #1404
  INST(Vpsubd           , VexRvm_Lx          , V(660F00,FA,_,x,I,0,4,FV ), 0                         , 133, 0  , 9269 , 378, 131), // #1405
  INST(Vpsubq           , VexRvm_Lx          , V(660F00,FB,_,x,I,1,4,FV ), 0                         , 102, 0  , 9276 , 379, 131), // #1406
  INST(Vpsubsb          , VexRvm_Lx          , V(660F00,E8,_,x,I,I,4,FVM), 0                         , 184, 0  , 9283 , 377, 146), // #1407
  INST(Vpsubsw          , VexRvm_Lx          , V(660F00,E9,_,x,I,I,4,FVM), 0                         , 184, 0  , 9291 , 377, 146), // #1408
  INST(Vpsubusb         , VexRvm_Lx          , V(660F00,D8,_,x,I,I,4,FVM), 0                         , 184, 0  , 9299 , 377, 146), // #1409
  INST(Vpsubusw         , VexRvm_Lx          , V(660F00,D9,_,x,I,I,4,FVM), 0                         , 184, 0  , 9308 , 377, 146), // #1410
  INST(Vpsubw           , VexRvm_Lx          , V(660F00,F9,_,x,I,I,4,FVM), 0                         , 184, 0  , 9317 , 377, 146), // #1411
  INST(Vpternlogd       , VexRvmi_Lx         , E(660F3A,25,_,x,_,0,4,FV ), 0                         , 108, 0  , 9324 , 196, 126), // #1412
  INST(Vpternlogq       , VexRvmi_Lx         , E(660F3A,25,_,x,_,1,4,FV ), 0                         , 109, 0  , 9335 , 197, 126), // #1413
  INST(Vptest           , VexRm_Lx           , V(660F38,17,_,x,I,_,_,_  ), 0                         , 96 , 0  , 9346 , 270, 150), // #1414
  INST(Vptestmb         , VexRvm_Lx          , E(660F38,26,_,x,_,0,4,FVM), 0                         , 110, 0  , 9353 , 369, 128), // #1415
  INST(Vptestmd         , VexRvm_Lx          , E(660F38,27,_,x,_,0,4,FV ), 0                         , 111, 0  , 9362 , 380, 126), // #1416
  INST(Vptestmq         , VexRvm_Lx          , E(660F38,27,_,x,_,1,4,FV ), 0                         , 112, 0  , 9371 , 381, 126), // #1417
  INST(Vptestmw         , VexRvm_Lx          , E(660F38,26,_,x,_,1,4,FVM), 0                         , 113, 0  , 9380 , 369, 128), // #1418
  INST(Vptestnmb        , VexRvm_Lx          , E(F30F38,26,_,x,_,0,4,FVM), 0                         , 217, 0  , 9389 , 369, 128), // #1419
  INST(Vptestnmd        , VexRvm_Lx          , E(F30F38,27,_,x,_,0,4,FV ), 0                         , 218, 0  , 9399 , 380, 126), // #1420
  INST(Vptestnmq        , VexRvm_Lx          , E(F30F38,27,_,x,_,1,4,FV ), 0                         , 219, 0  , 9409 , 381, 126), // #1421
  INST(Vptestnmw        , VexRvm_Lx          , E(F30F38,26,_,x,_,1,4,FVM), 0                         , 220, 0  , 9419 , 369, 128), // #1422
  INST(Vpunpckhbw       , VexRvm_Lx          , V(660F00,68,_,x,I,I,4,FVM), 0                         , 184, 0  , 9429 , 285, 146), // #1423
  INST(Vpunpckhdq       , VexRvm_Lx          , V(660F00,6A,_,x,I,0,4,FV ), 0                         , 133, 0  , 9440 , 199, 131), // #1424
  INST(Vpunpckhqdq      , VexRvm_Lx          , V(660F00,6D,_,x,I,1,4,FV ), 0                         , 102, 0  , 9451 , 198, 131), // #1425
  INST(Vpunpckhwd       , VexRvm_Lx          , V(660F00,69,_,x,I,I,4,FVM), 0                         , 184, 0  , 9463 , 285, 146), // #1426
  INST(Vpunpcklbw       , VexRvm_Lx          , V(660F00,60,_,x,I,I,4,FVM), 0                         , 184, 0  , 9474 , 285, 146), // #1427
  INST(Vpunpckldq       , VexRvm_Lx          , V(660F00,62,_,x,I,0,4,FV ), 0                         , 133, 0  , 9485 , 199, 131), // #1428
  INST(Vpunpcklqdq      , VexRvm_Lx          , V(660F00,6C,_,x,I,1,4,FV ), 0                         , 102, 0  , 9496 , 198, 131), // #1429
  INST(Vpunpcklwd       , VexRvm_Lx          , V(660F00,61,_,x,I,I,4,FVM), 0                         , 184, 0  , 9508 , 285, 146), // #1430
  INST(Vpxor            , VexRvm_Lx          , V(660F00,EF,_,x,I,_,_,_  ), 0                         , 68 , 0  , 9519 , 318, 143), // #1431
  INST(Vpxord           , VexRvm_Lx          , E(660F00,EF,_,x,_,0,4,FV ), 0                         , 189, 0  , 9525 , 319, 126), // #1432
  INST(Vpxorq           , VexRvm_Lx          , E(660F00,EF,_,x,_,1,4,FV ), 0                         , 130, 0  , 9532 , 320, 126), // #1433
  INST(Vrangepd         , VexRvmi_Lx         , E(660F3A,50,_,x,_,1,4,FV ), 0                         , 109, 0  , 9539 , 259, 129), // #1434
  INST(Vrangeps         , VexRvmi_Lx         , E(660F3A,50,_,x,_,0,4,FV ), 0                         , 108, 0  , 9548 , 260, 129), // #1435
  INST(Vrangesd         , VexRvmi            , E(660F3A,51,_,I,_,1,3,T1S), 0                         , 160, 0  , 9557 , 261, 64 ), // #1436
  INST(Vrangess         , VexRvmi            , E(660F3A,51,_,I,_,0,2,T1S), 0                         , 161, 0  , 9566 , 262, 64 ), // #1437
  INST(Vrcp14pd         , VexRm_Lx           , E(660F38,4C,_,x,_,1,4,FV ), 0                         , 112, 0  , 9575 , 351, 126), // #1438
  INST(Vrcp14ps         , VexRm_Lx           , E(660F38,4C,_,x,_,0,4,FV ), 0                         , 111, 0  , 9584 , 338, 126), // #1439
  INST(Vrcp14sd         , VexRvm             , E(660F38,4D,_,I,_,1,3,T1S), 0                         , 124, 0  , 9593 , 382, 66 ), // #1440
  INST(Vrcp14ss         , VexRvm             , E(660F38,4D,_,I,_,0,2,T1S), 0                         , 125, 0  , 9602 , 383, 66 ), // #1441
  INST(Vrcp28pd         , VexRm              , E(660F38,CA,_,2,_,1,4,FV ), 0                         , 152, 0  , 9611 , 252, 135), // #1442
  INST(Vrcp28ps         , VexRm              , E(660F38,CA,_,2,_,0,4,FV ), 0                         , 153, 0  , 9620 , 253, 135), // #1443
  INST(Vrcp28sd         , VexRvm             , E(660F38,CB,_,I,_,1,3,T1S), 0                         , 124, 0  , 9629 , 280, 135), // #1444
  INST(Vrcp28ss         , VexRvm             , E(660F38,CB,_,I,_,0,2,T1S), 0                         , 125, 0  , 9638 , 281, 135), // #1445
  INST(Vrcpps           , VexRm_Lx           , V(000F00,53,_,x,I,_,_,_  ), 0                         , 71 , 0  , 9647 , 270, 123), // #1446
  INST(Vrcpss           , VexRvm             , V(F30F00,53,_,I,I,_,_,_  ), 0                         , 178, 0  , 9654 , 384, 123), // #1447
  INST(Vreducepd        , VexRmi_Lx          , E(660F3A,56,_,x,_,1,4,FV ), 0                         , 109, 0  , 9661 , 363, 129), // #1448
  INST(Vreduceps        , VexRmi_Lx          , E(660F3A,56,_,x,_,0,4,FV ), 0                         , 108, 0  , 9671 , 362, 129), // #1449
  INST(Vreducesd        , VexRvmi            , E(660F3A,57,_,I,_,1,3,T1S), 0                         , 160, 0  , 9681 , 385, 64 ), // #1450
  INST(Vreducess        , VexRvmi            , E(660F3A,57,_,I,_,0,2,T1S), 0                         , 161, 0  , 9691 , 386, 64 ), // #1451
  INST(Vrndscalepd      , VexRmi_Lx          , E(660F3A,09,_,x,_,1,4,FV ), 0                         , 109, 0  , 9701 , 282, 126), // #1452
  INST(Vrndscaleps      , VexRmi_Lx          , E(660F3A,08,_,x,_,0,4,FV ), 0                         , 108, 0  , 9713 , 283, 126), // #1453
  INST(Vrndscalesd      , VexRvmi            , E(660F3A,0B,_,I,_,1,3,T1S), 0                         , 160, 0  , 9725 , 261, 66 ), // #1454
  INST(Vrndscaless      , VexRvmi            , E(660F3A,0A,_,I,_,0,2,T1S), 0                         , 161, 0  , 9737 , 262, 66 ), // #1455
  INST(Vroundpd         , VexRmi_Lx          , V(660F3A,09,_,x,I,_,_,_  ), 0                         , 72 , 0  , 9749 , 387, 123), // #1456
  INST(Vroundps         , VexRmi_Lx          , V(660F3A,08,_,x,I,_,_,_  ), 0                         , 72 , 0  , 9758 , 387, 123), // #1457
  INST(Vroundsd         , VexRvmi            , V(660F3A,0B,_,I,I,_,_,_  ), 0                         , 72 , 0  , 9767 , 388, 123), // #1458
  INST(Vroundss         , VexRvmi            , V(660F3A,0A,_,I,I,_,_,_  ), 0                         , 72 , 0  , 9776 , 389, 123), // #1459
  INST(Vrsqrt14pd       , VexRm_Lx           , E(660F38,4E,_,x,_,1,4,FV ), 0                         , 112, 0  , 9785 , 351, 126), // #1460
  INST(Vrsqrt14ps       , VexRm_Lx           , E(660F38,4E,_,x,_,0,4,FV ), 0                         , 111, 0  , 9796 , 338, 126), // #1461
  INST(Vrsqrt14sd       , VexRvm             , E(660F38,4F,_,I,_,1,3,T1S), 0                         , 124, 0  , 9807 , 382, 66 ), // #1462
  INST(Vrsqrt14ss       , VexRvm             , E(660F38,4F,_,I,_,0,2,T1S), 0                         , 125, 0  , 9818 , 383, 66 ), // #1463
  INST(Vrsqrt28pd       , VexRm              , E(660F38,CC,_,2,_,1,4,FV ), 0                         , 152, 0  , 9829 , 252, 135), // #1464
  INST(Vrsqrt28ps       , VexRm              , E(660F38,CC,_,2,_,0,4,FV ), 0                         , 153, 0  , 9840 , 253, 135), // #1465
  INST(Vrsqrt28sd       , VexRvm             , E(660F38,CD,_,I,_,1,3,T1S), 0                         , 124, 0  , 9851 , 280, 135), // #1466
  INST(Vrsqrt28ss       , VexRvm             , E(660F38,CD,_,I,_,0,2,T1S), 0                         , 125, 0  , 9862 , 281, 135), // #1467
  INST(Vrsqrtps         , VexRm_Lx           , V(000F00,52,_,x,I,_,_,_  ), 0                         , 71 , 0  , 9873 , 270, 123), // #1468
  INST(Vrsqrtss         , VexRvm             , V(F30F00,52,_,I,I,_,_,_  ), 0                         , 178, 0  , 9882 , 384, 123), // #1469
  INST(Vscalefpd        , VexRvm_Lx          , E(660F38,2C,_,x,_,1,4,FV ), 0                         , 112, 0  , 9891 , 390, 126), // #1470
  INST(Vscalefps        , VexRvm_Lx          , E(660F38,2C,_,x,_,0,4,FV ), 0                         , 111, 0  , 9901 , 391, 126), // #1471
  INST(Vscalefsd        , VexRvm             , E(660F38,2D,_,I,_,1,3,T1S), 0                         , 124, 0  , 9911 , 392, 66 ), // #1472
  INST(Vscalefss        , VexRvm             , E(660F38,2D,_,I,_,0,2,T1S), 0                         , 125, 0  , 9921 , 393, 66 ), // #1473
  INST(Vscatterdpd      , VexMr_Lx           , E(660F38,A2,_,x,_,1,3,T1S), 0                         , 124, 0  , 9931 , 394, 126), // #1474
  INST(Vscatterdps      , VexMr_Lx           , E(660F38,A2,_,x,_,0,2,T1S), 0                         , 125, 0  , 9943 , 365, 126), // #1475
  INST(Vscatterpf0dpd   , VexM_VM            , E(660F38,C6,5,2,_,1,3,T1S), 0                         , 221, 0  , 9955 , 275, 141), // #1476
  INST(Vscatterpf0dps   , VexM_VM            , E(660F38,C6,5,2,_,0,2,T1S), 0                         , 222, 0  , 9970 , 276, 141), // #1477
  INST(Vscatterpf0qpd   , VexM_VM            , E(660F38,C7,5,2,_,1,3,T1S), 0                         , 221, 0  , 9985 , 277, 141), // #1478
  INST(Vscatterpf0qps   , VexM_VM            , E(660F38,C7,5,2,_,0,2,T1S), 0                         , 222, 0  , 10000, 277, 141), // #1479
  INST(Vscatterpf1dpd   , VexM_VM            , E(660F38,C6,6,2,_,1,3,T1S), 0                         , 223, 0  , 10015, 275, 141), // #1480
  INST(Vscatterpf1dps   , VexM_VM            , E(660F38,C6,6,2,_,0,2,T1S), 0                         , 224, 0  , 10030, 276, 141), // #1481
  INST(Vscatterpf1qpd   , VexM_VM            , E(660F38,C7,6,2,_,1,3,T1S), 0                         , 223, 0  , 10045, 277, 141), // #1482
  INST(Vscatterpf1qps   , VexM_VM            , E(660F38,C7,6,2,_,0,2,T1S), 0                         , 224, 0  , 10060, 277, 141), // #1483
  INST(Vscatterqpd      , VexMr_Lx           , E(660F38,A3,_,x,_,1,3,T1S), 0                         , 124, 0  , 10075, 367, 126), // #1484
  INST(Vscatterqps      , VexMr_Lx           , E(660F38,A3,_,x,_,0,2,T1S), 0                         , 125, 0  , 10087, 366, 126), // #1485
  INST(Vshuff32x4       , VexRvmi_Lx         , E(660F3A,23,_,x,_,0,4,FV ), 0                         , 108, 0  , 10099, 395, 126), // #1486
  INST(Vshuff64x2       , VexRvmi_Lx         , E(660F3A,23,_,x,_,1,4,FV ), 0                         , 109, 0  , 10110, 396, 126), // #1487
  INST(Vshufi32x4       , VexRvmi_Lx         , E(660F3A,43,_,x,_,0,4,FV ), 0                         , 108, 0  , 10121, 395, 126), // #1488
  INST(Vshufi64x2       , VexRvmi_Lx         , E(660F3A,43,_,x,_,1,4,FV ), 0                         , 109, 0  , 10132, 396, 126), // #1489
  INST(Vshufpd          , VexRvmi_Lx         , V(660F00,C6,_,x,I,1,4,FV ), 0                         , 102, 0  , 10143, 397, 121), // #1490
  INST(Vshufps          , VexRvmi_Lx         , V(000F00,C6,_,x,I,0,4,FV ), 0                         , 103, 0  , 10151, 398, 121), // #1491
  INST(Vsqrtpd          , VexRm_Lx           , V(660F00,51,_,x,I,1,4,FV ), 0                         , 102, 0  , 10159, 399, 121), // #1492
  INST(Vsqrtps          , VexRm_Lx           , V(000F00,51,_,x,I,0,4,FV ), 0                         , 103, 0  , 10167, 222, 121), // #1493
  INST(Vsqrtsd          , VexRvm             , V(F20F00,51,_,I,I,1,3,T1S), 0                         , 104, 0  , 10175, 190, 122), // #1494
  INST(Vsqrtss          , VexRvm             , V(F30F00,51,_,I,I,0,2,T1S), 0                         , 105, 0  , 10183, 191, 122), // #1495
  INST(Vstmxcsr         , VexM               , V(000F00,AE,3,0,I,_,_,_  ), 0                         , 225, 0  , 10191, 291, 123), // #1496
  INST(Vsubpd           , VexRvm_Lx          , V(660F00,5C,_,x,I,1,4,FV ), 0                         , 102, 0  , 10200, 188, 121), // #1497
  INST(Vsubps           , VexRvm_Lx          , V(000F00,5C,_,x,I,0,4,FV ), 0                         , 103, 0  , 10207, 189, 121), // #1498
  INST(Vsubsd           , VexRvm             , V(F20F00,5C,_,I,I,1,3,T1S), 0                         , 104, 0  , 10214, 190, 122), // #1499
  INST(Vsubss           , VexRvm             , V(F30F00,5C,_,I,I,0,2,T1S), 0                         , 105, 0  , 10221, 191, 122), // #1500
  INST(Vtestpd          , VexRm_Lx           , V(660F38,0F,_,x,0,_,_,_  ), 0                         , 96 , 0  , 10228, 270, 150), // #1501
  INST(Vtestps          , VexRm_Lx           , V(660F38,0E,_,x,0,_,_,_  ), 0                         , 96 , 0  , 10236, 270, 150), // #1502
  INST(Vucomisd         , VexRm              , V(660F00,2E,_,I,I,1,3,T1S), 0                         , 122, 0  , 10244, 218, 132), // #1503
  INST(Vucomiss         , VexRm              , V(000F00,2E,_,I,I,0,2,T1S), 0                         , 123, 0  , 10253, 219, 132), // #1504
  INST(Vunpckhpd        , VexRvm_Lx          , V(660F00,15,_,x,I,1,4,FV ), 0                         , 102, 0  , 10262, 198, 121), // #1505
  INST(Vunpckhps        , VexRvm_Lx          , V(000F00,15,_,x,I,0,4,FV ), 0                         , 103, 0  , 10272, 199, 121), // #1506
  INST(Vunpcklpd        , VexRvm_Lx          , V(660F00,14,_,x,I,1,4,FV ), 0                         , 102, 0  , 10282, 198, 121), // #1507
  INST(Vunpcklps        , VexRvm_Lx          , V(000F00,14,_,x,I,0,4,FV ), 0                         , 103, 0  , 10292, 199, 121), // #1508
  INST(Vxorpd           , VexRvm_Lx          , V(660F00,57,_,x,I,1,4,FV ), 0                         , 102, 0  , 10302, 379, 127), // #1509
  INST(Vxorps           , VexRvm_Lx          , V(000F00,57,_,x,I,0,4,FV ), 0                         , 103, 0  , 10309, 378, 127), // #1510
  INST(Vzeroall         , VexOp              , V(000F00,77,_,1,I,_,_,_  ), 0                         , 67 , 0  , 10316, 400, 123), // #1511
  INST(Vzeroupper       , VexOp              , V(000F00,77,_,0,I,_,_,_  ), 0                         , 71 , 0  , 10325, 400, 123), // #1512
  INST(Wbinvd           , X86Op              , O(000F00,09,_,_,_,_,_,_  ), 0                         , 4  , 0  , 10336, 30 , 0  ), // #1513
  INST(Wbnoinvd         , X86Op              , O(F30F00,09,_,_,_,_,_,_  ), 0                         , 6  , 0  , 10343, 30 , 159), // #1514
  INST(Wrfsbase         , X86M               , O(F30F00,AE,2,_,x,_,_,_  ), 0                         , 226, 0  , 10352, 166, 102), // #1515
  INST(Wrgsbase         , X86M               , O(F30F00,AE,3,_,x,_,_,_  ), 0                         , 227, 0  , 10361, 166, 102), // #1516
  INST(Wrmsr            , X86Op              , O(000F00,30,_,_,_,_,_,_  ), 0                         , 4  , 0  , 10370, 167, 103), // #1517
  INST(Wrssd            , X86Mr              , O(000F38,F6,_,_,_,_,_,_  ), 0                         , 82 , 0  , 10376, 401, 54 ), // #1518
  INST(Wrssq            , X86Mr              , O(000F38,F6,_,_,1,_,_,_  ), 0                         , 228, 0  , 10382, 402, 54 ), // #1519
  INST(Wrussd           , X86Mr              , O(660F38,F5,_,_,_,_,_,_  ), 0                         , 2  , 0  , 10388, 401, 54 ), // #1520
  INST(Wrussq           , X86Mr              , O(660F38,F5,_,_,1,_,_,_  ), 0                         , 229, 0  , 10395, 402, 54 ), // #1521
  INST(Xabort           , X86Op_Mod11RM_I8   , O(000000,C6,7,_,_,_,_,_  ), 0                         , 26 , 0  , 10402, 77 , 160), // #1522
  INST(Xadd             , X86Xadd            , O(000F00,C0,_,_,x,_,_,_  ), 0                         , 4  , 0  , 10409, 403, 37 ), // #1523
  INST(Xbegin           , X86JmpRel          , O(000000,C7,7,_,_,_,_,_  ), 0                         , 26 , 0  , 10414, 404, 160), // #1524
  INST(Xchg             , X86Xchg            , O(000000,86,_,_,x,_,_,_  ), 0                         , 0  , 0  , 457  , 405, 0  ), // #1525
  INST(Xend             , X86Op              , O(000F01,D5,_,_,_,_,_,_  ), 0                         , 21 , 0  , 10421, 30 , 160), // #1526
  INST(Xgetbv           , X86Op              , O(000F01,D0,_,_,_,_,_,_  ), 0                         , 21 , 0  , 10426, 167, 161), // #1527
  INST(Xlatb            , X86Op              , O(000000,D7,_,_,_,_,_,_  ), 0                         , 0  , 0  , 10433, 30 , 0  ), // #1528
  INST(Xor              , X86Arith           , O(000000,30,6,_,x,_,_,_  ), 0                         , 31 , 0  , 9521 , 171, 1  ), // #1529
  INST(Xorpd            , ExtRm              , O(660F00,57,_,_,_,_,_,_  ), 0                         , 3  , 0  , 10303, 144, 4  ), // #1530
  INST(Xorps            , ExtRm              , O(000F00,57,_,_,_,_,_,_  ), 0                         , 4  , 0  , 10310, 144, 5  ), // #1531
  INST(Xresldtrk        , X86Op              , O(F20F01,E9,_,_,_,_,_,_  ), 0                         , 92 , 0  , 10439, 30 , 162), // #1532
  INST(Xrstor           , X86M_Only          , O(000F00,AE,5,_,_,_,_,_  ), 0                         , 75 , 0  , 1159 , 406, 161), // #1533
  INST(Xrstor64         , X86M_Only          , O(000F00,AE,5,_,1,_,_,_  ), 0                         , 230, 0  , 1167 , 407, 161), // #1534
  INST(Xrstors          , X86M_Only          , O(000F00,C7,3,_,_,_,_,_  ), 0                         , 76 , 0  , 10449, 406, 163), // #1535
  INST(Xrstors64        , X86M_Only          , O(000F00,C7,3,_,1,_,_,_  ), 0                         , 231, 0  , 10457, 407, 163), // #1536
  INST(Xsave            , X86M_Only          , O(000F00,AE,4,_,_,_,_,_  ), 0                         , 97 , 0  , 1177 , 406, 161), // #1537
  INST(Xsave64          , X86M_Only          , O(000F00,AE,4,_,1,_,_,_  ), 0                         , 232, 0  , 1184 , 407, 161), // #1538
  INST(Xsavec           , X86M_Only          , O(000F00,C7,4,_,_,_,_,_  ), 0                         , 97 , 0  , 10467, 406, 164), // #1539
  INST(Xsavec64         , X86M_Only          , O(000F00,C7,4,_,1,_,_,_  ), 0                         , 232, 0  , 10474, 407, 164), // #1540
  INST(Xsaveopt         , X86M_Only          , O(000F00,AE,6,_,_,_,_,_  ), 0                         , 78 , 0  , 10483, 406, 165), // #1541
  INST(Xsaveopt64       , X86M_Only          , O(000F00,AE,6,_,1,_,_,_  ), 0                         , 233, 0  , 10492, 407, 165), // #1542
  INST(Xsaves           , X86M_Only          , O(000F00,C7,5,_,_,_,_,_  ), 0                         , 75 , 0  , 10503, 406, 163), // #1543
  INST(Xsaves64         , X86M_Only          , O(000F00,C7,5,_,1,_,_,_  ), 0                         , 230, 0  , 10510, 407, 163), // #1544
  INST(Xsetbv           , X86Op              , O(000F01,D1,_,_,_,_,_,_  ), 0                         , 21 , 0  , 10519, 167, 161), // #1545
  INST(Xsusldtrk        , X86Op              , O(F20F01,E8,_,_,_,_,_,_  ), 0                         , 92 , 0  , 10526, 30 , 162), // #1546
  INST(Xtest            , X86Op              , O(000F01,D6,_,_,_,_,_,_  ), 0                         , 21 , 0  , 10536, 30 , 166)  // #1547
  // ${InstInfo:End}
};
#undef NAME_DATA_INDEX
#undef INST

// ============================================================================
// [asmjit::x86::InstDB - Opcode Tables]
// ============================================================================

// ${MainOpcodeTable:Begin}
// ------------------- Automatically generated, do not edit -------------------
const uint32_t InstDB::_mainOpcodeTable[] = {
  O(000000,00,0,0,0,0,0,_  ), // #0 [ref=56x]
  O(000000,00,2,0,0,0,0,_  ), // #1 [ref=4x]
  O(660F38,00,0,0,0,0,0,_  ), // #2 [ref=43x]
  O(660F00,00,0,0,0,0,0,_  ), // #3 [ref=38x]
  O(000F00,00,0,0,0,0,0,_  ), // #4 [ref=233x]
  O(F20F00,00,0,0,0,0,0,_  ), // #5 [ref=24x]
  O(F30F00,00,0,0,0,0,0,_  ), // #6 [ref=29x]
  O(F30F38,00,0,0,0,0,0,_  ), // #7 [ref=2x]
  O(660F3A,00,0,0,0,0,0,_  ), // #8 [ref=22x]
  O(000000,00,4,0,0,0,0,_  ), // #9 [ref=5x]
  V(000F38,00,0,0,0,0,0,_  ), // #10 [ref=6x]
  V(XOP_M9,00,1,0,0,0,0,_  ), // #11 [ref=3x]
  V(XOP_M9,00,6,0,0,0,0,_  ), // #12 [ref=2x]
  V(XOP_M9,00,5,0,0,0,0,_  ), // #13 [ref=1x]
  V(XOP_M9,00,3,0,0,0,0,_  ), // #14 [ref=1x]
  V(XOP_M9,00,2,0,0,0,0,_  ), // #15 [ref=1x]
  V(000F38,00,3,0,0,0,0,_  ), // #16 [ref=1x]
  V(000F38,00,2,0,0,0,0,_  ), // #17 [ref=1x]
  V(000F38,00,1,0,0,0,0,_  ), // #18 [ref=1x]
  O(660000,00,0,0,0,0,0,_  ), // #19 [ref=7x]
  O(000000,00,0,0,1,0,0,_  ), // #20 [ref=3x]
  O(000F01,00,0,0,0,0,0,_  ), // #21 [ref=29x]
  O(000F00,00,7,0,0,0,0,_  ), // #22 [ref=5x]
  O(660F00,00,7,0,0,0,0,_  ), // #23 [ref=1x]
  O(F30F00,00,6,0,0,0,0,_  ), // #24 [ref=3x]
  O(660F00,00,6,0,0,0,0,_  ), // #25 [ref=3x]
  O(000000,00,7,0,0,0,0,_  ), // #26 [ref=5x]
  O(000F00,00,1,0,1,0,0,_  ), // #27 [ref=2x]
  O(000F00,00,1,0,0,0,0,_  ), // #28 [ref=6x]
  O(F20F38,00,0,0,0,0,0,_  ), // #29 [ref=2x]
  O(000000,00,1,0,0,0,0,_  ), // #30 [ref=3x]
  O(000000,00,6,0,0,0,0,_  ), // #31 [ref=3x]
  O(F30F00,00,7,0,0,0,0,3  ), // #32 [ref=1x]
  O(F30F00,00,7,0,0,0,0,2  ), // #33 [ref=1x]
  O_FPU(00,D900,_)          , // #34 [ref=29x]
  O_FPU(00,C000,0)          , // #35 [ref=1x]
  O_FPU(00,DE00,_)          , // #36 [ref=7x]
  O_FPU(00,0000,4)          , // #37 [ref=4x]
  O_FPU(00,0000,6)          , // #38 [ref=4x]
  O_FPU(9B,DB00,_)          , // #39 [ref=2x]
  O_FPU(00,DA00,_)          , // #40 [ref=5x]
  O_FPU(00,DB00,_)          , // #41 [ref=8x]
  O_FPU(00,D000,2)          , // #42 [ref=1x]
  O_FPU(00,DF00,_)          , // #43 [ref=2x]
  O_FPU(00,D800,3)          , // #44 [ref=1x]
  O_FPU(00,F000,6)          , // #45 [ref=1x]
  O_FPU(00,F800,7)          , // #46 [ref=1x]
  O_FPU(00,DD00,_)          , // #47 [ref=3x]
  O_FPU(00,0000,0)          , // #48 [ref=3x]
  O_FPU(00,0000,2)          , // #49 [ref=3x]
  O_FPU(00,0000,3)          , // #50 [ref=3x]
  O_FPU(00,0000,7)          , // #51 [ref=3x]
  O_FPU(00,0000,1)          , // #52 [ref=2x]
  O_FPU(00,0000,5)          , // #53 [ref=2x]
  O_FPU(00,C800,1)          , // #54 [ref=1x]
  O_FPU(9B,0000,6)          , // #55 [ref=2x]
  O_FPU(9B,0000,7)          , // #56 [ref=2x]
  O_FPU(00,E000,4)          , // #57 [ref=1x]
  O_FPU(00,E800,5)          , // #58 [ref=1x]
  O_FPU(00,0000,_)          , // #59 [ref=1x]
  O(000F00,00,0,0,1,0,0,_  ), // #60 [ref=1x]
  O(000000,00,5,0,0,0,0,_  ), // #61 [ref=3x]
  O(F30F00,00,5,0,0,0,0,_  ), // #62 [ref=2x]
  O(F30F00,00,5,0,1,0,0,_  ), // #63 [ref=1x]
  V(660F00,00,0,1,0,0,0,_  ), // #64 [ref=7x]
  V(660F00,00,0,1,1,0,0,_  ), // #65 [ref=6x]
  V(000F00,00,0,1,1,0,0,_  ), // #66 [ref=7x]
  V(000F00,00,0,1,0,0,0,_  ), // #67 [ref=8x]
  V(660F00,00,0,0,0,0,0,_  ), // #68 [ref=15x]
  V(660F00,00,0,0,1,0,0,_  ), // #69 [ref=4x]
  V(000F00,00,0,0,1,0,0,_  ), // #70 [ref=4x]
  V(000F00,00,0,0,0,0,0,_  ), // #71 [ref=10x]
  V(660F3A,00,0,0,0,0,0,_  ), // #72 [ref=45x]
  V(660F3A,00,0,0,1,0,0,_  ), // #73 [ref=4x]
  O(000F00,00,2,0,0,0,0,_  ), // #74 [ref=5x]
  O(000F00,00,5,0,0,0,0,_  ), // #75 [ref=4x]
  O(000F00,00,3,0,0,0,0,_  ), // #76 [ref=5x]
  V(XOP_M9,00,0,0,0,0,0,_  ), // #77 [ref=32x]
  O(000F00,00,6,0,0,0,0,_  ), // #78 [ref=5x]
  V(XOP_MA,00,0,0,0,0,0,_  ), // #79 [ref=1x]
  V(XOP_MA,00,1,0,0,0,0,_  ), // #80 [ref=1x]
  O(F30F01,00,0,0,0,0,0,_  ), // #81 [ref=5x]
  O(000F38,00,0,0,0,0,0,_  ), // #82 [ref=24x]
  V(F20F38,00,0,0,0,0,0,_  ), // #83 [ref=6x]
  O(000000,00,3,0,0,0,0,_  ), // #84 [ref=3x]
  O(000F3A,00,0,0,0,0,0,_  ), // #85 [ref=4x]
  O(F30000,00,0,0,0,0,0,_  ), // #86 [ref=1x]
  O(000F0F,00,0,0,0,0,0,_  ), // #87 [ref=26x]
  V(F30F38,00,0,0,0,0,0,_  ), // #88 [ref=5x]
  O(000F3A,00,0,0,1,0,0,_  ), // #89 [ref=1x]
  O(660F3A,00,0,0,1,0,0,_  ), // #90 [ref=1x]
  O(F30F00,00,4,0,0,0,0,_  ), // #91 [ref=1x]
  O(F20F01,00,0,0,0,0,0,_  ), // #92 [ref=4x]
  O(F30F00,00,1,0,0,0,0,_  ), // #93 [ref=3x]
  O(F30F00,00,7,0,0,0,0,_  ), // #94 [ref=1x]
  V(F20F3A,00,0,0,0,0,0,_  ), // #95 [ref=1x]
  V(660F38,00,0,0,0,0,0,_  ), // #96 [ref=25x]
  O(000F00,00,4,0,0,0,0,_  ), // #97 [ref=4x]
  V(XOP_M9,00,7,0,0,0,0,_  ), // #98 [ref=1x]
  V(XOP_M9,00,4,0,0,0,0,_  ), // #99 [ref=1x]
  O(F20F00,00,6,0,0,0,0,_  ), // #100 [ref=1x]
  E(F20F38,00,0,2,0,0,2,T4X), // #101 [ref=6x]
  V(660F00,00,0,0,0,1,4,FV ), // #102 [ref=22x]
  V(000F00,00,0,0,0,0,4,FV ), // #103 [ref=16x]
  V(F20F00,00,0,0,0,1,3,T1S), // #104 [ref=10x]
  V(F30F00,00,0,0,0,0,2,T1S), // #105 [ref=10x]
  V(F20F00,00,0,0,0,0,0,_  ), // #106 [ref=4x]
  V(660F38,00,0,0,0,0,4,FVM), // #107 [ref=14x]
  E(660F3A,00,0,0,0,0,4,FV ), // #108 [ref=14x]
  E(660F3A,00,0,0,0,1,4,FV ), // #109 [ref=14x]
  E(660F38,00,0,0,0,0,4,FVM), // #110 [ref=9x]
  E(660F38,00,0,0,0,0,4,FV ), // #111 [ref=22x]
  E(660F38,00,0,0,0,1,4,FV ), // #112 [ref=28x]
  E(660F38,00,0,0,0,1,4,FVM), // #113 [ref=9x]
  V(660F38,00,0,1,0,0,0,_  ), // #114 [ref=2x]
  E(660F38,00,0,0,0,0,3,T2 ), // #115 [ref=2x]
  E(660F38,00,0,0,0,0,4,T4 ), // #116 [ref=2x]
  E(660F38,00,0,2,0,0,5,T8 ), // #117 [ref=2x]
  E(660F38,00,0,0,0,1,4,T2 ), // #118 [ref=2x]
  E(660F38,00,0,2,0,1,5,T4 ), // #119 [ref=2x]
  V(660F38,00,0,0,0,1,3,T1S), // #120 [ref=2x]
  V(660F38,00,0,0,0,0,2,T1S), // #121 [ref=14x]
  V(660F00,00,0,0,0,1,3,T1S), // #122 [ref=5x]
  V(000F00,00,0,0,0,0,2,T1S), // #123 [ref=2x]
  E(660F38,00,0,0,0,1,3,T1S), // #124 [ref=14x]
  E(660F38,00,0,0,0,0,2,T1S), // #125 [ref=14x]
  V(F30F00,00,0,0,0,0,3,HV ), // #126 [ref=1x]
  E(F20F38,00,0,0,0,0,0,_  ), // #127 [ref=1x]
  E(F30F38,00,0,0,0,0,0,_  ), // #128 [ref=7x]
  V(F20F00,00,0,0,0,1,4,FV ), // #129 [ref=1x]
  E(660F00,00,0,0,0,1,4,FV ), // #130 [ref=9x]
  E(000F00,00,0,0,0,1,4,FV ), // #131 [ref=3x]
  V(660F38,00,0,0,0,0,3,HVM), // #132 [ref=7x]
  V(660F00,00,0,0,0,0,4,FV ), // #133 [ref=11x]
  V(000F00,00,0,0,0,0,4,HV ), // #134 [ref=1x]
  V(660F3A,00,0,0,0,0,3,HVM), // #135 [ref=1x]
  E(660F00,00,0,0,0,0,3,HV ), // #136 [ref=4x]
  E(000F00,00,0,0,0,0,4,FV ), // #137 [ref=2x]
  E(F30F00,00,0,0,0,1,4,FV ), // #138 [ref=2x]
  V(F20F00,00,0,0,0,0,3,T1F), // #139 [ref=2x]
  E(F20F00,00,0,0,0,0,3,T1F), // #140 [ref=2x]
  V(F20F00,00,0,0,0,0,2,T1W), // #141 [ref=1x]
  V(F30F00,00,0,0,0,0,2,T1W), // #142 [ref=1x]
  V(F30F00,00,0,0,0,0,2,T1F), // #143 [ref=2x]
  E(F30F00,00,0,0,0,0,2,T1F), // #144 [ref=2x]
  V(F30F00,00,0,0,0,0,4,FV ), // #145 [ref=1x]
  E(F30F00,00,0,0,0,0,3,HV ), // #146 [ref=1x]
  E(F20F00,00,0,0,0,0,4,FV ), // #147 [ref=1x]
  E(F20F00,00,0,0,0,1,4,FV ), // #148 [ref=1x]
  E(F20F00,00,0,0,0,0,2,T1W), // #149 [ref=1x]
  E(F30F00,00,0,0,0,0,2,T1W), // #150 [ref=1x]
  E(660F3A,00,0,0,0,0,4,FVM), // #151 [ref=5x]
  E(660F38,00,0,2,0,1,4,FV ), // #152 [ref=3x]
  E(660F38,00,0,2,0,0,4,FV ), // #153 [ref=3x]
  V(660F3A,00,0,1,0,0,0,_  ), // #154 [ref=6x]
  E(660F3A,00,0,0,0,0,4,T4 ), // #155 [ref=4x]
  E(660F3A,00,0,2,0,0,5,T8 ), // #156 [ref=4x]
  E(660F3A,00,0,0,0,1,4,T2 ), // #157 [ref=4x]
  E(660F3A,00,0,2,0,1,5,T4 ), // #158 [ref=4x]
  V(660F3A,00,0,0,0,0,2,T1S), // #159 [ref=4x]
  E(660F3A,00,0,0,0,1,3,T1S), // #160 [ref=6x]
  E(660F3A,00,0,0,0,0,2,T1S), // #161 [ref=6x]
  V(660F38,00,0,0,1,1,4,FV ), // #162 [ref=20x]
  V(660F38,00,0,0,0,0,4,FV ), // #163 [ref=32x]
  V(660F38,00,0,0,1,1,3,T1S), // #164 [ref=12x]
  V(660F38,00,0,0,1,0,0,_  ), // #165 [ref=5x]
  E(660F38,00,1,2,0,1,3,T1S), // #166 [ref=2x]
  E(660F38,00,1,2,0,0,2,T1S), // #167 [ref=2x]
  E(660F38,00,2,2,0,1,3,T1S), // #168 [ref=2x]
  E(660F38,00,2,2,0,0,2,T1S), // #169 [ref=2x]
  V(660F3A,00,0,0,1,1,4,FV ), // #170 [ref=2x]
  V(000F00,00,2,0,0,0,0,_  ), // #171 [ref=1x]
  V(660F00,00,0,0,0,1,4,FVM), // #172 [ref=3x]
  V(000F00,00,0,0,0,0,4,FVM), // #173 [ref=3x]
  V(660F00,00,0,0,0,0,2,T1S), // #174 [ref=1x]
  V(F20F00,00,0,0,0,1,3,DUP), // #175 [ref=1x]
  E(660F00,00,0,0,0,0,4,FVM), // #176 [ref=1x]
  E(660F00,00,0,0,0,1,4,FVM), // #177 [ref=1x]
  V(F30F00,00,0,0,0,0,0,_  ), // #178 [ref=3x]
  E(F20F00,00,0,0,0,1,4,FVM), // #179 [ref=1x]
  E(F30F00,00,0,0,0,0,4,FVM), // #180 [ref=1x]
  E(F30F00,00,0,0,0,1,4,FVM), // #181 [ref=1x]
  E(F20F00,00,0,0,0,0,4,FVM), // #182 [ref=1x]
  V(000F00,00,0,0,0,0,3,T2 ), // #183 [ref=2x]
  V(660F00,00,0,0,0,0,4,FVM), // #184 [ref=33x]
  V(F30F00,00,0,0,0,0,4,FVM), // #185 [ref=3x]
  E(F20F38,00,0,0,0,0,4,FV ), // #186 [ref=1x]
  E(F20F38,00,0,0,0,1,4,FV ), // #187 [ref=1x]
  V(660F3A,00,0,0,0,0,4,FVM), // #188 [ref=2x]
  E(660F00,00,0,0,0,0,4,FV ), // #189 [ref=5x]
  V(660F38,00,0,0,0,0,0,T1S), // #190 [ref=1x]
  E(F30F38,00,0,0,0,1,0,_  ), // #191 [ref=5x]
  V(660F38,00,0,0,0,0,1,T1S), // #192 [ref=1x]
  V(XOP_M8,00,0,0,0,0,0,_  ), // #193 [ref=22x]
  V(660F38,00,0,0,0,1,4,FVM), // #194 [ref=2x]
  E(660F3A,00,0,0,0,1,4,FVM), // #195 [ref=2x]
  E(660F38,00,0,0,0,0,0,T1S), // #196 [ref=2x]
  E(660F38,00,0,0,0,1,1,T1S), // #197 [ref=2x]
  V(660F38,00,0,0,0,1,4,FV ), // #198 [ref=3x]
  E(660F38,00,0,0,1,1,4,FV ), // #199 [ref=1x]
  V(660F3A,00,0,0,0,0,0,T1S), // #200 [ref=2x]
  V(660F3A,00,0,0,1,1,3,T1S), // #201 [ref=2x]
  V(660F3A,00,0,0,0,0,1,T1S), // #202 [ref=1x]
  V(660F00,00,0,0,0,0,1,T1S), // #203 [ref=1x]
  E(F30F38,00,0,0,0,0,2,QVM), // #204 [ref=6x]
  E(F30F38,00,0,0,0,0,3,HVM), // #205 [ref=9x]
  E(F30F38,00,0,0,0,0,1,OVM), // #206 [ref=3x]
  V(660F38,00,0,0,0,0,2,QVM), // #207 [ref=4x]
  V(660F38,00,0,0,0,0,1,OVM), // #208 [ref=2x]
  E(660F00,00,1,0,0,0,4,FV ), // #209 [ref=1x]
  E(660F00,00,1,0,0,1,4,FV ), // #210 [ref=1x]
  V(F20F00,00,0,0,0,0,4,FVM), // #211 [ref=1x]
  V(660F00,00,0,0,0,0,4,128), // #212 [ref=5x]
  V(660F00,00,7,0,0,0,4,FVM), // #213 [ref=1x]
  V(660F00,00,0,0,0,1,4,128), // #214 [ref=2x]
  E(660F00,00,0,0,0,1,4,128), // #215 [ref=1x]
  V(660F00,00,3,0,0,0,4,FVM), // #216 [ref=1x]
  E(F30F38,00,0,0,0,0,4,FVM), // #217 [ref=1x]
  E(F30F38,00,0,0,0,0,4,FV ), // #218 [ref=1x]
  E(F30F38,00,0,0,0,1,4,FV ), // #219 [ref=1x]
  E(F30F38,00,0,0,0,1,4,FVM), // #220 [ref=1x]
  E(660F38,00,5,2,0,1,3,T1S), // #221 [ref=2x]
  E(660F38,00,5,2,0,0,2,T1S), // #222 [ref=2x]
  E(660F38,00,6,2,0,1,3,T1S), // #223 [ref=2x]
  E(660F38,00,6,2,0,0,2,T1S), // #224 [ref=2x]
  V(000F00,00,3,0,0,0,0,_  ), // #225 [ref=1x]
  O(F30F00,00,2,0,0,0,0,_  ), // #226 [ref=1x]
  O(F30F00,00,3,0,0,0,0,_  ), // #227 [ref=1x]
  O(000F38,00,0,0,1,0,0,_  ), // #228 [ref=1x]
  O(660F38,00,0,0,1,0,0,_  ), // #229 [ref=1x]
  O(000F00,00,5,0,1,0,0,_  ), // #230 [ref=2x]
  O(000F00,00,3,0,1,0,0,_  ), // #231 [ref=1x]
  O(000F00,00,4,0,1,0,0,_  ), // #232 [ref=2x]
  O(000F00,00,6,0,1,0,0,_  )  // #233 [ref=1x]
};
// ----------------------------------------------------------------------------
// ${MainOpcodeTable:End}

// ${AltOpcodeTable:Begin}
// ------------------- Automatically generated, do not edit -------------------
const uint32_t InstDB::_altOpcodeTable[] = {
  0                         , // #0 [ref=1403x]
  O(660F00,1B,_,_,_,_,_,_  ), // #1 [ref=1x]
  O(000F00,BA,4,_,x,_,_,_  ), // #2 [ref=1x]
  O(000F00,BA,7,_,x,_,_,_  ), // #3 [ref=1x]
  O(000F00,BA,6,_,x,_,_,_  ), // #4 [ref=1x]
  O(000F00,BA,5,_,x,_,_,_  ), // #5 [ref=1x]
  O(000000,48,_,_,x,_,_,_  ), // #6 [ref=1x]
  O(660F00,78,0,_,_,_,_,_  ), // #7 [ref=1x]
  O_FPU(00,00DF,5)          , // #8 [ref=1x]
  O_FPU(00,00DF,7)          , // #9 [ref=1x]
  O_FPU(00,00DD,1)          , // #10 [ref=1x]
  O_FPU(00,00DB,5)          , // #11 [ref=1x]
  O_FPU(00,DFE0,_)          , // #12 [ref=1x]
  O(000000,DB,7,_,_,_,_,_  ), // #13 [ref=1x]
  O_FPU(9B,DFE0,_)          , // #14 [ref=1x]
  O(000000,E4,_,_,_,_,_,_  ), // #15 [ref=1x]
  O(000000,40,_,_,x,_,_,_  ), // #16 [ref=1x]
  O(F20F00,78,_,_,_,_,_,_  ), // #17 [ref=1x]
  O(000000,77,_,_,_,_,_,_  ), // #18 [ref=2x]
  O(000000,73,_,_,_,_,_,_  ), // #19 [ref=3x]
  O(000000,72,_,_,_,_,_,_  ), // #20 [ref=3x]
  O(000000,76,_,_,_,_,_,_  ), // #21 [ref=2x]
  O(000000,74,_,_,_,_,_,_  ), // #22 [ref=2x]
  O(000000,E3,_,_,_,_,_,_  ), // #23 [ref=1x]
  O(000000,7F,_,_,_,_,_,_  ), // #24 [ref=2x]
  O(000000,7D,_,_,_,_,_,_  ), // #25 [ref=2x]
  O(000000,7C,_,_,_,_,_,_  ), // #26 [ref=2x]
  O(000000,7E,_,_,_,_,_,_  ), // #27 [ref=2x]
  O(000000,EB,_,_,_,_,_,_  ), // #28 [ref=1x]
  O(000000,75,_,_,_,_,_,_  ), // #29 [ref=2x]
  O(000000,71,_,_,_,_,_,_  ), // #30 [ref=1x]
  O(000000,7B,_,_,_,_,_,_  ), // #31 [ref=2x]
  O(000000,79,_,_,_,_,_,_  ), // #32 [ref=1x]
  O(000000,70,_,_,_,_,_,_  ), // #33 [ref=1x]
  O(000000,7A,_,_,_,_,_,_  ), // #34 [ref=2x]
  O(000000,78,_,_,_,_,_,_  ), // #35 [ref=1x]
  V(660F00,92,_,0,0,_,_,_  ), // #36 [ref=1x]
  V(F20F00,92,_,0,0,_,_,_  ), // #37 [ref=1x]
  V(F20F00,92,_,0,1,_,_,_  ), // #38 [ref=1x]
  V(000F00,92,_,0,0,_,_,_  ), // #39 [ref=1x]
  O(000000,E2,_,_,_,_,_,_  ), // #40 [ref=1x]
  O(000000,E1,_,_,_,_,_,_  ), // #41 [ref=1x]
  O(000000,E0,_,_,_,_,_,_  ), // #42 [ref=1x]
  O(660F00,29,_,_,_,_,_,_  ), // #43 [ref=1x]
  O(000F00,29,_,_,_,_,_,_  ), // #44 [ref=1x]
  O(000F38,F1,_,_,x,_,_,_  ), // #45 [ref=1x]
  O(000F00,7E,_,_,_,_,_,_  ), // #46 [ref=1x]
  O(660F00,7F,_,_,_,_,_,_  ), // #47 [ref=1x]
  O(F30F00,7F,_,_,_,_,_,_  ), // #48 [ref=1x]
  O(660F00,17,_,_,_,_,_,_  ), // #49 [ref=1x]
  O(000F00,17,_,_,_,_,_,_  ), // #50 [ref=1x]
  O(660F00,13,_,_,_,_,_,_  ), // #51 [ref=1x]
  O(000F00,13,_,_,_,_,_,_  ), // #52 [ref=1x]
  O(660F00,E7,_,_,_,_,_,_  ), // #53 [ref=1x]
  O(660F00,2B,_,_,_,_,_,_  ), // #54 [ref=1x]
  O(000F00,2B,_,_,_,_,_,_  ), // #55 [ref=1x]
  O(000F00,E7,_,_,_,_,_,_  ), // #56 [ref=1x]
  O(F20F00,2B,_,_,_,_,_,_  ), // #57 [ref=1x]
  O(F30F00,2B,_,_,_,_,_,_  ), // #58 [ref=1x]
  O(000F00,7E,_,_,x,_,_,_  ), // #59 [ref=1x]
  O(F20F00,11,_,_,_,_,_,_  ), // #60 [ref=1x]
  O(F30F00,11,_,_,_,_,_,_  ), // #61 [ref=1x]
  O(660F00,11,_,_,_,_,_,_  ), // #62 [ref=1x]
  O(000F00,11,_,_,_,_,_,_  ), // #63 [ref=1x]
  O(000000,E6,_,_,_,_,_,_  ), // #64 [ref=1x]
  O(000F3A,15,_,_,_,_,_,_  ), // #65 [ref=1x]
  O(000000,58,_,_,_,_,_,_  ), // #66 [ref=1x]
  O(000F00,72,6,_,_,_,_,_  ), // #67 [ref=1x]
  O(660F00,73,7,_,_,_,_,_  ), // #68 [ref=1x]
  O(000F00,73,6,_,_,_,_,_  ), // #69 [ref=1x]
  O(000F00,71,6,_,_,_,_,_  ), // #70 [ref=1x]
  O(000F00,72,4,_,_,_,_,_  ), // #71 [ref=1x]
  O(000F00,71,4,_,_,_,_,_  ), // #72 [ref=1x]
  O(000F00,72,2,_,_,_,_,_  ), // #73 [ref=1x]
  O(660F00,73,3,_,_,_,_,_  ), // #74 [ref=1x]
  O(000F00,73,2,_,_,_,_,_  ), // #75 [ref=1x]
  O(000F00,71,2,_,_,_,_,_  ), // #76 [ref=1x]
  O(000000,50,_,_,_,_,_,_  ), // #77 [ref=1x]
  O(000000,F6,_,_,x,_,_,_  ), // #78 [ref=1x]
  V(660F38,92,_,x,_,1,3,T1S), // #79 [ref=1x]
  V(660F38,92,_,x,_,0,2,T1S), // #80 [ref=1x]
  V(660F38,93,_,x,_,1,3,T1S), // #81 [ref=1x]
  V(660F38,93,_,x,_,0,2,T1S), // #82 [ref=1x]
  V(660F38,2F,_,x,0,_,_,_  ), // #83 [ref=1x]
  V(660F38,2E,_,x,0,_,_,_  ), // #84 [ref=1x]
  V(660F00,29,_,x,I,1,4,FVM), // #85 [ref=1x]
  V(000F00,29,_,x,I,0,4,FVM), // #86 [ref=1x]
  V(660F00,7E,_,0,0,0,2,T1S), // #87 [ref=1x]
  V(660F00,7F,_,x,I,_,_,_  ), // #88 [ref=1x]
  E(660F00,7F,_,x,_,0,4,FVM), // #89 [ref=1x]
  E(660F00,7F,_,x,_,1,4,FVM), // #90 [ref=1x]
  V(F30F00,7F,_,x,I,_,_,_  ), // #91 [ref=1x]
  E(F20F00,7F,_,x,_,1,4,FVM), // #92 [ref=1x]
  E(F30F00,7F,_,x,_,0,4,FVM), // #93 [ref=1x]
  E(F30F00,7F,_,x,_,1,4,FVM), // #94 [ref=1x]
  E(F20F00,7F,_,x,_,0,4,FVM), // #95 [ref=1x]
  V(660F00,17,_,0,I,1,3,T1S), // #96 [ref=1x]
  V(000F00,17,_,0,I,0,3,T2 ), // #97 [ref=1x]
  V(660F00,13,_,0,I,1,3,T1S), // #98 [ref=1x]
  V(000F00,13,_,0,I,0,3,T2 ), // #99 [ref=1x]
  V(660F00,7E,_,0,I,1,3,T1S), // #100 [ref=1x]
  V(F20F00,11,_,I,I,1,3,T1S), // #101 [ref=1x]
  V(F30F00,11,_,I,I,0,2,T1S), // #102 [ref=1x]
  V(660F00,11,_,x,I,1,4,FVM), // #103 [ref=1x]
  V(000F00,11,_,x,I,0,4,FVM), // #104 [ref=1x]
  E(660F38,7A,_,x,0,0,0,T1S), // #105 [ref=1x]
  E(660F38,7C,_,x,0,0,0,T1S), // #106 [ref=1x]
  E(660F38,7C,_,x,0,1,0,T1S), // #107 [ref=1x]
  E(660F38,7B,_,x,0,0,0,T1S), // #108 [ref=1x]
  V(660F3A,05,_,x,0,1,4,FV ), // #109 [ref=1x]
  V(660F3A,04,_,x,0,0,4,FV ), // #110 [ref=1x]
  V(660F3A,01,_,x,1,1,4,FV ), // #111 [ref=1x]
  V(660F3A,00,_,x,1,1,4,FV ), // #112 [ref=1x]
  V(660F38,90,_,x,_,0,2,T1S), // #113 [ref=1x]
  V(660F38,90,_,x,_,1,3,T1S), // #114 [ref=1x]
  V(660F38,91,_,x,_,0,2,T1S), // #115 [ref=1x]
  V(660F38,91,_,x,_,1,3,T1S), // #116 [ref=1x]
  V(660F38,8E,_,x,0,_,_,_  ), // #117 [ref=1x]
  V(660F38,8E,_,x,1,_,_,_  ), // #118 [ref=1x]
  V(XOP_M8,C0,_,0,x,_,_,_  ), // #119 [ref=1x]
  V(XOP_M8,C2,_,0,x,_,_,_  ), // #120 [ref=1x]
  V(XOP_M8,C3,_,0,x,_,_,_  ), // #121 [ref=1x]
  V(XOP_M8,C1,_,0,x,_,_,_  ), // #122 [ref=1x]
  V(660F00,72,6,x,I,0,4,FV ), // #123 [ref=1x]
  V(660F00,73,6,x,I,1,4,FV ), // #124 [ref=1x]
  V(660F00,71,6,x,I,I,4,FVM), // #125 [ref=1x]
  V(660F00,72,4,x,I,0,4,FV ), // #126 [ref=1x]
  E(660F00,72,4,x,_,1,4,FV ), // #127 [ref=1x]
  V(660F00,71,4,x,I,I,4,FVM), // #128 [ref=1x]
  V(660F00,72,2,x,I,0,4,FV ), // #129 [ref=1x]
  V(660F00,73,2,x,I,1,4,FV ), // #130 [ref=1x]
  V(660F00,71,2,x,I,I,4,FVM)  // #131 [ref=1x]
};
// ----------------------------------------------------------------------------
// ${AltOpcodeTable:End}

#undef O
#undef V
#undef E
#undef O_FPU

// ============================================================================
// [asmjit::x86::InstDB - CommonInfoTableA]
// ============================================================================

// ${InstCommonTable:Begin}
// ------------------- Automatically generated, do not edit -------------------
#define F(VAL) InstDB::kFlag##VAL
#define CONTROL(VAL) Inst::kControl##VAL
#define SINGLE_REG(VAL) InstDB::kSingleReg##VAL
const InstDB::CommonInfo InstDB::_commonInfoTable[] = {
  { 0                                                     , 0  , 0 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #0 [ref=1x]
  { 0                                                     , 347, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #1 [ref=4x]
  { 0                                                     , 348, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #2 [ref=2x]
  { F(Lock)|F(XAcquire)|F(XRelease)                       , 16 , 12, CONTROL(None)   , SINGLE_REG(None), 0 }, // #3 [ref=2x]
  { 0                                                     , 156, 2 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #4 [ref=2x]
  { F(Vec)                                                , 70 , 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #5 [ref=54x]
  { F(Vec)                                                , 97 , 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #6 [ref=19x]
  { F(Vec)                                                , 230, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #7 [ref=16x]
  { F(Vec)                                                , 188, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #8 [ref=20x]
  { F(Lock)|F(XAcquire)|F(XRelease)                       , 28 , 11, CONTROL(None)   , SINGLE_REG(RO)  , 0 }, // #9 [ref=1x]
  { F(Vex)                                                , 245, 2 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #10 [ref=3x]
  { F(Vec)                                                , 70 , 1 , CONTROL(None)   , SINGLE_REG(RO)  , 0 }, // #11 [ref=12x]
  { 0                                                     , 349, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #12 [ref=1x]
  { F(Vex)                                                , 247, 2 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #13 [ref=5x]
  { F(Vex)                                                , 156, 2 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #14 [ref=12x]
  { F(Vec)                                                , 350, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #15 [ref=4x]
  { 0                                                     , 249, 2 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #16 [ref=3x]
  { F(Mib)                                                , 351, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #17 [ref=1x]
  { 0                                                     , 352, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #18 [ref=1x]
  { 0                                                     , 251, 2 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #19 [ref=1x]
  { F(Mib)                                                , 353, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #20 [ref=1x]
  { 0                                                     , 253, 2 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #21 [ref=1x]
  { 0                                                     , 155, 3 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #22 [ref=35x]
  { 0                                                     , 354, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #23 [ref=3x]
  { 0                                                     , 119, 4 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #24 [ref=1x]
  { F(Lock)|F(XAcquire)|F(XRelease)                       , 119, 4 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #25 [ref=3x]
  { F(Rep)|F(RepIgnored)                                  , 255, 2 , CONTROL(Call)   , SINGLE_REG(None), 0 }, // #26 [ref=1x]
  { 0                                                     , 355, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #27 [ref=1x]
  { 0                                                     , 356, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #28 [ref=2x]
  { 0                                                     , 330, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #29 [ref=1x]
  { 0                                                     , 99 , 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #30 [ref=83x]
  { 0                                                     , 357, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #31 [ref=24x]
  { 0                                                     , 358, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #32 [ref=6x]
  { 0                                                     , 359, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #33 [ref=1x]
  { 0                                                     , 16 , 12, CONTROL(None)   , SINGLE_REG(None), 0 }, // #34 [ref=1x]
  { F(Rep)                                                , 360, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #35 [ref=1x]
  { F(Vec)                                                , 361, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #36 [ref=2x]
  { F(Vec)                                                , 362, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #37 [ref=3x]
  { F(Lock)|F(XAcquire)|F(XRelease)                       , 123, 4 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #38 [ref=1x]
  { F(Lock)|F(XAcquire)|F(XRelease)                       , 363, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #39 [ref=1x]
  { F(Lock)|F(XAcquire)|F(XRelease)                       , 364, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #40 [ref=1x]
  { 0                                                     , 365, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #41 [ref=1x]
  { 0                                                     , 366, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #42 [ref=1x]
  { 0                                                     , 257, 2 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #43 [ref=1x]
  { F(Mmx)|F(Vec)                                         , 367, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #44 [ref=2x]
  { F(Mmx)|F(Vec)                                         , 368, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #45 [ref=2x]
  { F(Mmx)|F(Vec)                                         , 369, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #46 [ref=2x]
  { F(Vec)                                                , 370, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #47 [ref=2x]
  { F(Vec)                                                , 371, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #48 [ref=2x]
  { F(Vec)                                                , 372, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #49 [ref=2x]
  { 0                                                     , 373, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #50 [ref=1x]
  { 0                                                     , 374, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #51 [ref=2x]
  { F(Lock)|F(XAcquire)|F(XRelease)                       , 259, 2 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #52 [ref=2x]
  { 0                                                     , 39 , 4 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #53 [ref=3x]
  { F(Mmx)                                                , 99 , 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #54 [ref=1x]
  { 0                                                     , 261, 2 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #55 [ref=2x]
  { 0                                                     , 375, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #56 [ref=1x]
  { F(Vec)                                                , 376, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #57 [ref=2x]
  { F(Vec)                                                , 263, 2 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #58 [ref=1x]
  { F(FpuM32)|F(FpuM64)                                   , 158, 3 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #59 [ref=6x]
  { 0                                                     , 265, 2 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #60 [ref=9x]
  { F(FpuM80)                                             , 377, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #61 [ref=2x]
  { 0                                                     , 266, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #62 [ref=13x]
  { F(FpuM32)|F(FpuM64)                                   , 267, 2 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #63 [ref=2x]
  { F(FpuM16)|F(FpuM32)                                   , 378, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #64 [ref=9x]
  { F(FpuM16)|F(FpuM32)|F(FpuM64)                         , 379, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #65 [ref=3x]
  { F(FpuM32)|F(FpuM64)|F(FpuM80)                         , 380, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #66 [ref=2x]
  { F(FpuM16)                                             , 381, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #67 [ref=3x]
  { F(FpuM16)                                             , 382, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #68 [ref=2x]
  { F(FpuM32)|F(FpuM64)                                   , 268, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #69 [ref=1x]
  { 0                                                     , 383, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #70 [ref=2x]
  { 0                                                     , 39 , 10, CONTROL(None)   , SINGLE_REG(None), 0 }, // #71 [ref=1x]
  { 0                                                     , 384, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #72 [ref=1x]
  { 0                                                     , 385, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #73 [ref=2x]
  { 0                                                     , 314, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #74 [ref=2x]
  { F(Rep)                                                , 386, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #75 [ref=1x]
  { F(Vec)                                                , 269, 2 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #76 [ref=1x]
  { 0                                                     , 387, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #77 [ref=2x]
  { 0                                                     , 388, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #78 [ref=8x]
  { 0                                                     , 271, 2 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #79 [ref=3x]
  { 0                                                     , 273, 2 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #80 [ref=1x]
  { 0                                                     , 99 , 1 , CONTROL(Return) , SINGLE_REG(None), 0 }, // #81 [ref=3x]
  { 0                                                     , 389, 1 , CONTROL(Return) , SINGLE_REG(None), 0 }, // #82 [ref=1x]
  { F(Rep)|F(RepIgnored)                                  , 275, 2 , CONTROL(Branch) , SINGLE_REG(None), 0 }, // #83 [ref=30x]
  { F(Rep)|F(RepIgnored)                                  , 277, 2 , CONTROL(Branch) , SINGLE_REG(None), 0 }, // #84 [ref=1x]
  { F(Rep)|F(RepIgnored)                                  , 279, 2 , CONTROL(Jump)   , SINGLE_REG(None), 0 }, // #85 [ref=1x]
  { F(Vec)|F(Vex)                                         , 390, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #86 [ref=27x]
  { F(Vec)|F(Vex)                                         , 281, 2 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #87 [ref=1x]
  { F(Vec)|F(Vex)                                         , 283, 2 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #88 [ref=1x]
  { F(Vec)|F(Vex)                                         , 285, 2 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #89 [ref=1x]
  { F(Vec)|F(Vex)                                         , 287, 2 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #90 [ref=1x]
  { F(Vec)|F(Vex)                                         , 391, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #91 [ref=12x]
  { F(Vec)|F(Vex)                                         , 392, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #92 [ref=8x]
  { 0                                                     , 393, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #93 [ref=2x]
  { 0                                                     , 289, 2 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #94 [ref=1x]
  { F(Vec)                                                , 197, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #95 [ref=2x]
  { 0                                                     , 394, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #96 [ref=2x]
  { 0                                                     , 291, 2 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #97 [ref=2x]
  { F(Vex)                                                , 395, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #98 [ref=2x]
  { 0                                                     , 396, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #99 [ref=1x]
  { 0                                                     , 161, 3 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #100 [ref=3x]
  { 0                                                     , 397, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #101 [ref=5x]
  { F(Vex)                                                , 398, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #102 [ref=2x]
  { F(Rep)                                                , 399, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #103 [ref=1x]
  { 0                                                     , 277, 2 , CONTROL(Branch) , SINGLE_REG(None), 0 }, // #104 [ref=3x]
  { 0                                                     , 293, 2 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #105 [ref=1x]
  { F(Vex)                                                , 400, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #106 [ref=2x]
  { F(Vec)                                                , 401, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #107 [ref=1x]
  { F(Mmx)                                                , 402, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #108 [ref=1x]
  { 0                                                     , 403, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #109 [ref=2x]
  { F(XRelease)                                           , 0  , 16, CONTROL(None)   , SINGLE_REG(None), 0 }, // #110 [ref=1x]
  { F(Vec)                                                , 70 , 2 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #111 [ref=6x]
  { 0                                                     , 64 , 6 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #112 [ref=1x]
  { F(Mmx)|F(Vec)                                         , 295, 2 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #113 [ref=1x]
  { 0                                                     , 404, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #114 [ref=1x]
  { 0                                                     , 68 , 2 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #115 [ref=2x]
  { F(Mmx)|F(Vec)                                         , 405, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #116 [ref=1x]
  { F(Vec)                                                , 264, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #117 [ref=2x]
  { F(Vec)                                                , 203, 2 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #118 [ref=4x]
  { F(Vec)                                                , 406, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #119 [ref=2x]
  { F(Vec)                                                , 71 , 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #120 [ref=3x]
  { F(Mmx)                                                , 407, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #121 [ref=1x]
  { F(Vec)                                                , 98 , 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #122 [ref=1x]
  { F(Vec)                                                , 206, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #123 [ref=1x]
  { F(Mmx)|F(Vec)                                         , 94 , 5 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #124 [ref=1x]
  { F(Mmx)|F(Vec)                                         , 408, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #125 [ref=1x]
  { F(Rep)                                                , 409, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #126 [ref=1x]
  { F(Vec)                                                , 97 , 2 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #127 [ref=1x]
  { F(Vec)                                                , 297, 2 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #128 [ref=1x]
  { 0                                                     , 299, 2 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #129 [ref=2x]
  { 0                                                     , 301, 2 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #130 [ref=1x]
  { F(Vex)                                                , 303, 2 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #131 [ref=1x]
  { 0                                                     , 410, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #132 [ref=1x]
  { 0                                                     , 411, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #133 [ref=1x]
  { F(Lock)|F(XAcquire)|F(XRelease)                       , 260, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #134 [ref=2x]
  { 0                                                     , 99 , 5 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #135 [ref=1x]
  { F(Lock)|F(XAcquire)|F(XRelease)                       , 16 , 12, CONTROL(None)   , SINGLE_REG(RO)  , 0 }, // #136 [ref=1x]
  { 0                                                     , 412, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #137 [ref=1x]
  { F(Rep)                                                , 413, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #138 [ref=1x]
  { F(Mmx)|F(Vec)                                         , 305, 2 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #139 [ref=40x]
  { F(Mmx)|F(Vec)                                         , 307, 2 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #140 [ref=1x]
  { F(Mmx)|F(Vec)                                         , 305, 2 , CONTROL(None)   , SINGLE_REG(RO)  , 0 }, // #141 [ref=6x]
  { F(Mmx)|F(Vec)                                         , 305, 2 , CONTROL(None)   , SINGLE_REG(WO)  , 0 }, // #142 [ref=16x]
  { F(Mmx)                                                , 305, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #143 [ref=26x]
  { F(Vec)                                                , 70 , 1 , CONTROL(None)   , SINGLE_REG(WO)  , 0 }, // #144 [ref=4x]
  { F(Vec)                                                , 414, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #145 [ref=1x]
  { F(Vec)                                                , 415, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #146 [ref=1x]
  { F(Vec)                                                , 416, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #147 [ref=1x]
  { F(Vec)                                                , 417, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #148 [ref=1x]
  { F(Vec)                                                , 418, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #149 [ref=1x]
  { F(Vec)                                                , 419, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #150 [ref=1x]
  { F(Mmx)|F(Vec)                                         , 309, 2 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #151 [ref=1x]
  { F(Vec)                                                , 420, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #152 [ref=1x]
  { F(Vec)                                                , 421, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #153 [ref=1x]
  { F(Vec)                                                , 422, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #154 [ref=1x]
  { F(Mmx)|F(Vec)                                         , 423, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #155 [ref=1x]
  { F(Mmx)|F(Vec)                                         , 424, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #156 [ref=1x]
  { F(Vec)                                                , 233, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #157 [ref=2x]
  { 0                                                     , 127, 4 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #158 [ref=1x]
  { 0                                                     , 389, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #159 [ref=9x]
  { F(Mmx)                                                , 307, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #160 [ref=1x]
  { F(Mmx)|F(Vec)                                         , 311, 2 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #161 [ref=8x]
  { F(Vec)                                                , 425, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #162 [ref=2x]
  { 0                                                     , 426, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #163 [ref=1x]
  { 0                                                     , 131, 4 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #164 [ref=1x]
  { 0                                                     , 427, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #165 [ref=8x]
  { 0                                                     , 428, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #166 [ref=4x]
  { 0                                                     , 429, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #167 [ref=8x]
  { 0                                                     , 313, 2 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #168 [ref=1x]
  { F(Rep)|F(RepIgnored)                                  , 315, 2 , CONTROL(Return) , SINGLE_REG(None), 0 }, // #169 [ref=1x]
  { F(Vex)                                                , 317, 2 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #170 [ref=1x]
  { F(Lock)|F(XAcquire)|F(XRelease)                       , 16 , 12, CONTROL(None)   , SINGLE_REG(WO)  , 0 }, // #171 [ref=3x]
  { F(Rep)                                                , 430, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #172 [ref=1x]
  { 0                                                     , 431, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #173 [ref=30x]
  { 0                                                     , 164, 3 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #174 [ref=2x]
  { 0                                                     , 432, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #175 [ref=3x]
  { F(Rep)                                                , 433, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #176 [ref=1x]
  { F(Vex)                                                , 434, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #177 [ref=5x]
  { 0                                                     , 57 , 7 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #178 [ref=1x]
  { F(Tsib)|F(Vex)                                        , 435, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #179 [ref=2x]
  { F(Vex)                                                , 389, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #180 [ref=1x]
  { F(Tsib)|F(Vex)                                        , 436, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #181 [ref=1x]
  { F(Vex)                                                , 437, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #182 [ref=1x]
  { 0                                                     , 438, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #183 [ref=2x]
  { 0                                                     , 439, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #184 [ref=2x]
  { 0                                                     , 440, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #185 [ref=1x]
  { F(Vec)|F(Evex)|F(Avx512T4X)|F(Avx512KZ)               , 441, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #186 [ref=4x]
  { F(Vec)|F(Evex)|F(Avx512T4X)|F(Avx512KZ)               , 442, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #187 [ref=2x]
  { F(Vec)|F(Vex)|F(Evex)|F(Avx512KZ_ER_SAE_B64)          , 167, 3 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #188 [ref=22x]
  { F(Vec)|F(Vex)|F(Evex)|F(Avx512KZ_ER_SAE_B32)          , 167, 3 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #189 [ref=22x]
  { F(Vec)|F(Vex)|F(Evex)|F(Avx512KZ_ER_SAE)              , 443, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #190 [ref=18x]
  { F(Vec)|F(Vex)|F(Evex)|F(Avx512KZ_ER_SAE)              , 444, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #191 [ref=17x]
  { F(Vec)|F(Vex)                                         , 167, 2 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #192 [ref=15x]
  { F(Vec)|F(Vex)|F(Evex)                                 , 167, 3 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #193 [ref=5x]
  { F(Vec)|F(Vex)                                         , 70 , 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #194 [ref=17x]
  { F(Vec)|F(Vex)                                         , 188, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #195 [ref=1x]
  { F(Vec)|F(Evex)|F(Avx512KZ_B32)                        , 170, 3 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #196 [ref=4x]
  { F(Vec)|F(Evex)|F(Avx512KZ_B64)                        , 170, 3 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #197 [ref=4x]
  { F(Vec)|F(Vex)|F(Evex)|F(Avx512KZ_B64)                 , 167, 3 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #198 [ref=10x]
  { F(Vec)|F(Vex)|F(Evex)|F(Avx512KZ_B32)                 , 167, 3 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #199 [ref=12x]
  { F(Vec)|F(Vex)|F(Evex)|F(Avx512KZ_B64)                 , 167, 3 , CONTROL(None)   , SINGLE_REG(RO)  , 0 }, // #200 [ref=2x]
  { F(Vec)|F(Vex)|F(Evex)|F(Avx512KZ_B32)                 , 167, 3 , CONTROL(None)   , SINGLE_REG(RO)  , 0 }, // #201 [ref=6x]
  { F(Vec)|F(Evex)|F(Avx512KZ)                            , 167, 3 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #202 [ref=13x]
  { F(Vec)|F(Evex)|F(Avx512KZ_B32)                        , 167, 3 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #203 [ref=16x]
  { F(Vec)|F(Evex)|F(Avx512KZ_B64)                        , 167, 3 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #204 [ref=19x]
  { F(Vec)|F(Vex)                                         , 170, 2 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #205 [ref=6x]
  { F(Vec)|F(Vex)                                         , 319, 2 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #206 [ref=3x]
  { F(Vec)|F(Vex)                                         , 445, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #207 [ref=2x]
  { F(Vec)|F(Evex)|F(Avx512KZ)                            , 446, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #208 [ref=1x]
  { F(Vec)|F(Evex)|F(Avx512KZ)                            , 447, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #209 [ref=4x]
  { F(Vec)|F(Evex)|F(Avx512KZ)                            , 448, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #210 [ref=4x]
  { F(Vec)|F(Evex)|F(Avx512KZ)                            , 449, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #211 [ref=1x]
  { F(Vec)|F(Vex)|F(Evex)|F(Avx512KZ)                     , 446, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #212 [ref=1x]
  { F(Vec)|F(Vex)|F(Evex)|F(Avx512KZ)                     , 450, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #213 [ref=1x]
  { F(Vec)|F(Vex)|F(Evex)|F(Avx512KZ_SAE_B64)             , 173, 3 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #214 [ref=1x]
  { F(Vec)|F(Vex)|F(Evex)|F(Avx512KZ_SAE_B32)             , 173, 3 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #215 [ref=1x]
  { F(Vec)|F(Vex)|F(Evex)|F(Avx512KZ_SAE)                 , 451, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #216 [ref=1x]
  { F(Vec)|F(Vex)|F(Evex)|F(Avx512KZ_SAE)                 , 452, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #217 [ref=1x]
  { F(Vec)|F(Vex)|F(Evex)|F(Avx512SAE)                    , 97 , 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #218 [ref=2x]
  { F(Vec)|F(Vex)|F(Evex)|F(Avx512SAE)                    , 230, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #219 [ref=2x]
  { F(Vec)|F(Evex)|F(Avx512KZ)                            , 176, 3 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #220 [ref=6x]
  { F(Vec)|F(Vex)|F(Evex)|F(Avx512KZ_B32)                 , 179, 3 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #221 [ref=1x]
  { F(Vec)|F(Vex)|F(Evex)|F(Avx512KZ_ER_SAE_B32)          , 182, 3 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #222 [ref=3x]
  { F(Vec)|F(Evex)|F(Avx512KZ_B32)                        , 321, 2 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #223 [ref=1x]
  { F(Vec)|F(Vex)|F(Evex)|F(Avx512KZ_ER_SAE_B64)          , 321, 2 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #224 [ref=2x]
  { F(Vec)|F(Evex)|F(Avx512KZ_ER_SAE_B64)                 , 182, 3 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #225 [ref=4x]
  { F(Vec)|F(Evex)|F(Avx512KZ_ER_SAE_B64)                 , 321, 2 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #226 [ref=3x]
  { F(Vec)|F(Vex)|F(Evex)|F(Avx512KZ_SAE)                 , 179, 3 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #227 [ref=1x]
  { F(Vec)|F(Vex)|F(Evex)|F(Avx512KZ_ER_SAE_B32)          , 179, 3 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #228 [ref=1x]
  { F(Vec)|F(Vex)|F(Evex)|F(Avx512KZ_SAE)                 , 185, 3 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #229 [ref=1x]
  { F(Vec)|F(Evex)|F(Avx512KZ_ER_SAE_B32)                 , 179, 3 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #230 [ref=2x]
  { F(Vec)|F(Evex)|F(Avx512KZ_ER_SAE_B32)                 , 182, 3 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #231 [ref=2x]
  { F(Vec)|F(Vex)|F(Evex)|F(Avx512ER_SAE)                 , 370, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #232 [ref=1x]
  { F(Vec)|F(Evex)|F(Avx512ER_SAE)                        , 370, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #233 [ref=1x]
  { F(Vec)|F(Vex)|F(Evex)|F(Avx512ER_SAE)                 , 453, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #234 [ref=2x]
  { F(Vec)|F(Vex)|F(Evex)|F(Avx512KZ_SAE)                 , 444, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #235 [ref=3x]
  { F(Vec)|F(Vex)|F(Evex)|F(Avx512ER_SAE)                 , 372, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #236 [ref=1x]
  { F(Vec)|F(Evex)|F(Avx512ER_SAE)                        , 372, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #237 [ref=1x]
  { F(Vec)|F(Vex)|F(Evex)|F(Avx512KZ_SAE_B64)             , 321, 2 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #238 [ref=1x]
  { F(Vec)|F(Evex)|F(Avx512KZ_SAE_B64)                    , 182, 3 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #239 [ref=3x]
  { F(Vec)|F(Evex)|F(Avx512KZ_SAE_B64)                    , 321, 2 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #240 [ref=1x]
  { F(Vec)|F(Vex)|F(Evex)|F(Avx512KZ_SAE_B32)             , 182, 3 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #241 [ref=1x]
  { F(Vec)|F(Evex)|F(Avx512KZ_SAE_B32)                    , 179, 3 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #242 [ref=2x]
  { F(Vec)|F(Evex)|F(Avx512KZ_SAE_B32)                    , 182, 3 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #243 [ref=2x]
  { F(Vec)|F(Vex)|F(Evex)|F(Avx512SAE)                    , 370, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #244 [ref=1x]
  { F(Vec)|F(Evex)|F(Avx512SAE)                           , 370, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #245 [ref=1x]
  { F(Vec)|F(Vex)|F(Evex)|F(Avx512SAE)                    , 372, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #246 [ref=1x]
  { F(Vec)|F(Evex)|F(Avx512SAE)                           , 372, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #247 [ref=1x]
  { F(Vec)|F(Evex)|F(Avx512KZ_B32)                        , 179, 3 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #248 [ref=1x]
  { F(Vec)|F(Evex)|F(Avx512ER_SAE)                        , 453, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #249 [ref=2x]
  { F(Vec)|F(Evex)|F(Avx512KZ)                            , 170, 3 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #250 [ref=3x]
  { F(Vec)|F(Vex)                                         , 170, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #251 [ref=9x]
  { F(Vec)|F(Evex)|F(Avx512KZ_SAE_B64)                    , 74 , 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #252 [ref=3x]
  { F(Vec)|F(Evex)|F(Avx512KZ_SAE_B32)                    , 74 , 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #253 [ref=3x]
  { F(Vec)|F(Evex)|F(Avx512KZ)                            , 182, 3 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #254 [ref=9x]
  { F(Vec)|F(Vex)                                         , 186, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #255 [ref=2x]
  { F(Vec)|F(Evex)|F(Avx512KZ)                            , 454, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #256 [ref=4x]
  { F(Vec)|F(Evex)|F(Avx512KZ)                            , 187, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #257 [ref=4x]
  { F(Vec)|F(Vex)|F(Evex)                                 , 376, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #258 [ref=2x]
  { F(Vec)|F(Evex)|F(Avx512KZ_SAE_B64)                    , 170, 3 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #259 [ref=2x]
  { F(Vec)|F(Evex)|F(Avx512KZ_SAE_B32)                    , 170, 3 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #260 [ref=2x]
  { F(Vec)|F(Evex)|F(Avx512KZ_SAE)                        , 455, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #261 [ref=4x]
  { F(Vec)|F(Evex)|F(Avx512KZ_SAE)                        , 456, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #262 [ref=4x]
  { F(Vec)|F(Vex)                                         , 135, 4 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #263 [ref=13x]
  { F(Vec)|F(Vex)                                         , 323, 2 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #264 [ref=4x]
  { F(Vec)|F(Vex)                                         , 325, 2 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #265 [ref=4x]
  { F(Vec)|F(Evex)|F(Avx512K_B64)                         , 457, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #266 [ref=1x]
  { F(Vec)|F(Evex)|F(Avx512K_B32)                         , 457, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #267 [ref=1x]
  { F(Vec)|F(Evex)|F(Avx512K)                             , 458, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #268 [ref=1x]
  { F(Vec)|F(Evex)|F(Avx512K)                             , 459, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #269 [ref=1x]
  { F(Vec)|F(Vex)                                         , 182, 2 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #270 [ref=7x]
  { F(Vec)|F(Vex)                                         , 97 , 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #271 [ref=1x]
  { F(Vec)|F(Vex)                                         , 230, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #272 [ref=1x]
  { F(Vec)|F(Vsib)|F(Vex)|F(Evex)|F(Avx512K)              , 104, 5 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #273 [ref=2x]
  { F(Vec)|F(Vsib)|F(Vex)|F(Evex)|F(Avx512K)              , 109, 5 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #274 [ref=2x]
  { F(Vsib)|F(Evex)|F(Avx512K)                            , 460, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #275 [ref=4x]
  { F(Vsib)|F(Evex)|F(Avx512K)                            , 461, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #276 [ref=4x]
  { F(Vsib)|F(Evex)|F(Avx512K)                            , 462, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #277 [ref=8x]
  { F(Vec)|F(Vsib)|F(Vex)|F(Evex)|F(Avx512K)              , 114, 5 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #278 [ref=2x]
  { F(Vec)|F(Vsib)|F(Vex)|F(Evex)|F(Avx512K)              , 139, 4 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #279 [ref=2x]
  { F(Vec)|F(Evex)|F(Avx512KZ_SAE)                        , 443, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #280 [ref=3x]
  { F(Vec)|F(Evex)|F(Avx512KZ_SAE)                        , 444, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #281 [ref=3x]
  { F(Vec)|F(Evex)|F(Avx512KZ_SAE_B64)                    , 188, 3 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #282 [ref=2x]
  { F(Vec)|F(Evex)|F(Avx512KZ_SAE_B32)                    , 188, 3 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #283 [ref=2x]
  { F(Vec)|F(Vex)|F(Evex)|F(Avx512KZ)                     , 170, 3 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #284 [ref=3x]
  { F(Vec)|F(Vex)|F(Evex)|F(Avx512KZ)                     , 167, 3 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #285 [ref=22x]
  { F(Vec)|F(Vex)                                         , 327, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #286 [ref=2x]
  { F(Vec)|F(Evex)|F(Avx512KZ)                            , 327, 2 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #287 [ref=4x]
  { F(Vec)|F(Evex)|F(Avx512KZ)                            , 463, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #288 [ref=4x]
  { F(Vec)|F(Vex)|F(Evex)                                 , 456, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #289 [ref=1x]
  { F(Vec)|F(Vex)                                         , 197, 2 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #290 [ref=1x]
  { F(Vex)                                                , 394, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #291 [ref=2x]
  { F(Vec)|F(Vex)                                         , 401, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #292 [ref=1x]
  { F(Vec)|F(Vex)                                         , 143, 4 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #293 [ref=4x]
  { F(Vec)|F(Vex)|F(Evex)|F(Avx512KZ_SAE_B64)             , 167, 3 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #294 [ref=2x]
  { F(Vec)|F(Vex)|F(Evex)|F(Avx512KZ_SAE_B32)             , 167, 3 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #295 [ref=2x]
  { F(Vec)|F(Vex)|F(Evex)|F(Avx512KZ_SAE)                 , 443, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #296 [ref=2x]
  { 0                                                     , 329, 2 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #297 [ref=3x]
  { F(Vec)|F(Vex)|F(Evex)|F(Avx512KZ)                     , 70 , 6 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #298 [ref=4x]
  { F(Vec)|F(Vex)|F(Evex)                                 , 331, 2 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #299 [ref=1x]
  { F(Vec)|F(Vex)|F(Evex)|F(Avx512KZ)                     , 191, 3 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #300 [ref=1x]
  { F(Vec)|F(Vex)                                         , 70 , 4 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #301 [ref=2x]
  { F(Vec)|F(Evex)|F(Avx512KZ)                            , 70 , 6 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #302 [ref=6x]
  { F(Vec)|F(Vex)|F(Evex)                                 , 205, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #303 [ref=2x]
  { F(Vec)|F(Vex)|F(Evex)                                 , 333, 2 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #304 [ref=4x]
  { F(Vec)|F(Vex)                                         , 464, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #305 [ref=3x]
  { F(Vec)|F(Vex)|F(Evex)                                 , 194, 3 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #306 [ref=3x]
  { F(Vec)|F(Vex)|F(Evex)                                 , 197, 3 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #307 [ref=1x]
  { F(Vec)|F(Vex)|F(Evex)                                 , 200, 3 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #308 [ref=1x]
  { F(Vec)|F(Vex)|F(Evex)|F(Avx512KZ)                     , 203, 3 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #309 [ref=1x]
  { F(Vec)|F(Vex)|F(Evex)|F(Avx512KZ)                     , 182, 3 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #310 [ref=5x]
  { F(Vec)|F(Vex)|F(Evex)|F(Avx512KZ)                     , 206, 3 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #311 [ref=1x]
  { 0                                                     , 335, 2 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #312 [ref=1x]
  { 0                                                     , 337, 2 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #313 [ref=1x]
  { F(Vec)|F(Evex)|F(Avx512B32)                           , 209, 3 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #314 [ref=1x]
  { F(Vec)|F(Evex)|F(Avx512B64)                           , 209, 3 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #315 [ref=1x]
  { F(Vec)|F(Vex)                                         , 167, 2 , CONTROL(None)   , SINGLE_REG(RO)  , 0 }, // #316 [ref=2x]
  { F(Vec)|F(Evex)|F(Avx512KZ_B32)                        , 167, 3 , CONTROL(None)   , SINGLE_REG(RO)  , 0 }, // #317 [ref=2x]
  { F(Vec)|F(Vex)                                         , 167, 2 , CONTROL(None)   , SINGLE_REG(WO)  , 0 }, // #318 [ref=2x]
  { F(Vec)|F(Evex)|F(Avx512KZ_B32)                        , 167, 3 , CONTROL(None)   , SINGLE_REG(WO)  , 0 }, // #319 [ref=2x]
  { F(Vec)|F(Evex)|F(Avx512KZ_B64)                        , 167, 3 , CONTROL(None)   , SINGLE_REG(WO)  , 0 }, // #320 [ref=2x]
  { F(Vec)|F(Evex)|F(Avx512KZ_B64)                        , 167, 3 , CONTROL(None)   , SINGLE_REG(RO)  , 0 }, // #321 [ref=2x]
  { F(Vec)|F(Vex)|F(Evex)|F(Avx512KZ)                     , 465, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #322 [ref=1x]
  { F(Vec)|F(Vex)|F(Evex)|F(Avx512KZ)                     , 466, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #323 [ref=1x]
  { F(Vec)|F(Evex)                                        , 467, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #324 [ref=6x]
  { F(Vec)|F(Vex)|F(Evex)|F(Avx512KZ)                     , 212, 3 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #325 [ref=1x]
  { F(Vec)|F(Vex)|F(Evex)|F(Avx512KZ)                     , 468, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #326 [ref=1x]
  { F(Vec)|F(Vex)|F(Evex)                                 , 170, 3 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #327 [ref=1x]
  { F(Vec)|F(Evex)|F(Avx512K)                             , 215, 3 , CONTROL(None)   , SINGLE_REG(WO)  , 0 }, // #328 [ref=2x]
  { F(Vec)|F(Evex)|F(Avx512K_B32)                         , 215, 3 , CONTROL(None)   , SINGLE_REG(WO)  , 0 }, // #329 [ref=2x]
  { F(Vec)|F(Vex)|F(Evex)|F(Avx512K)                      , 218, 3 , CONTROL(None)   , SINGLE_REG(WO)  , 0 }, // #330 [ref=4x]
  { F(Vec)|F(Vex)|F(Evex)|F(Avx512K_B32)                  , 218, 3 , CONTROL(None)   , SINGLE_REG(WO)  , 0 }, // #331 [ref=2x]
  { F(Vec)|F(Vex)|F(Evex)|F(Avx512K_B64)                  , 218, 3 , CONTROL(None)   , SINGLE_REG(WO)  , 0 }, // #332 [ref=2x]
  { F(Vec)|F(Vex)                                         , 414, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #333 [ref=1x]
  { F(Vec)|F(Vex)                                         , 415, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #334 [ref=1x]
  { F(Vec)|F(Vex)                                         , 416, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #335 [ref=1x]
  { F(Vec)|F(Vex)                                         , 417, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #336 [ref=1x]
  { F(Vec)|F(Evex)|F(Avx512K_B64)                         , 215, 3 , CONTROL(None)   , SINGLE_REG(WO)  , 0 }, // #337 [ref=4x]
  { F(Vec)|F(Evex)|F(Avx512KZ_B32)                        , 182, 3 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #338 [ref=6x]
  { F(Vec)|F(Vex)                                         , 171, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #339 [ref=2x]
  { F(Vec)|F(Vex)|F(Evex)|F(Avx512KZ_B32)                 , 168, 2 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #340 [ref=2x]
  { F(Vec)|F(Vex)                                         , 147, 4 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #341 [ref=2x]
  { F(Vec)|F(Vex)|F(Evex)|F(Avx512KZ_B64)                 , 76 , 6 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #342 [ref=2x]
  { F(Vec)|F(Vex)|F(Evex)|F(Avx512KZ_B64)                 , 151, 4 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #343 [ref=2x]
  { F(Vec)|F(Vex)|F(Evex)                                 , 418, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #344 [ref=1x]
  { F(Vec)|F(Vex)|F(Evex)                                 , 419, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #345 [ref=1x]
  { F(Vec)|F(Vex)|F(Evex)                                 , 469, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #346 [ref=1x]
  { F(Vec)|F(Vex)|F(Evex)|F(Avx512KZ)                     , 470, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #347 [ref=1x]
  { F(Vec)|F(Vex)|F(Evex)|F(Avx512KZ)                     , 471, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #348 [ref=1x]
  { F(Vec)|F(Vex)|F(Evex)|F(Avx512KZ)                     , 472, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #349 [ref=1x]
  { F(Vec)|F(Vex)|F(Evex)|F(Avx512KZ)                     , 473, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #350 [ref=1x]
  { F(Vec)|F(Evex)|F(Avx512KZ_B64)                        , 182, 3 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #351 [ref=4x]
  { F(Vec)|F(Vex)                                         , 319, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #352 [ref=12x]
  { F(Vec)|F(Vex)|F(Evex)|F(Avx512KZ)                     , 167, 3 , CONTROL(None)   , SINGLE_REG(RO)  , 0 }, // #353 [ref=8x]
  { F(Vec)|F(Evex)                                        , 474, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #354 [ref=4x]
  { F(Vec)|F(Evex)|F(Avx512KZ)                            , 221, 3 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #355 [ref=6x]
  { F(Vec)|F(Evex)|F(Avx512KZ)                            , 224, 3 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #356 [ref=9x]
  { F(Vec)|F(Evex)|F(Avx512KZ)                            , 227, 3 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #357 [ref=3x]
  { F(Vec)|F(Vex)|F(Evex)|F(Avx512KZ)                     , 230, 3 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #358 [ref=4x]
  { F(Vec)|F(Vex)|F(Evex)|F(Avx512KZ)                     , 233, 3 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #359 [ref=2x]
  { F(Vec)|F(Vex)|F(Evex)|F(Avx512KZ)                     , 179, 3 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #360 [ref=6x]
  { F(Vec)|F(Vex)                                         , 135, 2 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #361 [ref=1x]
  { F(Vec)|F(Evex)|F(Avx512KZ_B32)                        , 188, 3 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #362 [ref=3x]
  { F(Vec)|F(Evex)|F(Avx512KZ_B64)                        , 188, 3 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #363 [ref=3x]
  { F(Vec)|F(Vex)                                         , 339, 2 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #364 [ref=4x]
  { F(Vec)|F(Vsib)|F(Evex)|F(Avx512K)                     , 236, 3 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #365 [ref=3x]
  { F(Vec)|F(Vsib)|F(Evex)|F(Avx512K)                     , 341, 2 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #366 [ref=2x]
  { F(Vec)|F(Vsib)|F(Evex)|F(Avx512K)                     , 239, 3 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #367 [ref=2x]
  { F(Vec)|F(Vex)                                         , 343, 2 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #368 [ref=8x]
  { F(Vec)|F(Evex)|F(Avx512K)                             , 242, 3 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #369 [ref=5x]
  { F(Vec)|F(Vex)|F(Evex)|F(Avx512KZ_B32)                 , 188, 3 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #370 [ref=1x]
  { F(Vec)|F(Vex)|F(Evex)|F(Avx512KZ)                     , 188, 3 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #371 [ref=2x]
  { F(Vec)|F(Vex)|F(Evex)|F(Avx512KZ_B32)                 , 82 , 6 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #372 [ref=3x]
  { F(Vec)|F(Vex)|F(Evex)                                 , 188, 3 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #373 [ref=2x]
  { F(Vec)|F(Vex)|F(Evex)|F(Avx512KZ_B64)                 , 82 , 6 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #374 [ref=2x]
  { F(Vec)|F(Vex)|F(Evex)|F(Avx512KZ)                     , 82 , 6 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #375 [ref=3x]
  { F(Vec)|F(Evex)|F(Avx512KZ_B64)                        , 88 , 6 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #376 [ref=1x]
  { F(Vec)|F(Vex)|F(Evex)|F(Avx512KZ)                     , 167, 3 , CONTROL(None)   , SINGLE_REG(WO)  , 0 }, // #377 [ref=6x]
  { F(Vec)|F(Vex)|F(Evex)|F(Avx512KZ_B32)                 , 167, 3 , CONTROL(None)   , SINGLE_REG(WO)  , 0 }, // #378 [ref=2x]
  { F(Vec)|F(Vex)|F(Evex)|F(Avx512KZ_B64)                 , 167, 3 , CONTROL(None)   , SINGLE_REG(WO)  , 0 }, // #379 [ref=2x]
  { F(Vec)|F(Evex)|F(Avx512K_B32)                         , 242, 3 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #380 [ref=2x]
  { F(Vec)|F(Evex)|F(Avx512K_B64)                         , 242, 3 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #381 [ref=2x]
  { F(Vec)|F(Evex)|F(Avx512KZ)                            , 443, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #382 [ref=2x]
  { F(Vec)|F(Evex)|F(Avx512KZ)                            , 444, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #383 [ref=2x]
  { F(Vec)|F(Vex)                                         , 444, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #384 [ref=2x]
  { F(Vec)|F(Evex)|F(Avx512KZ)                            , 455, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #385 [ref=1x]
  { F(Vec)|F(Evex)|F(Avx512KZ)                            , 456, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #386 [ref=1x]
  { F(Vec)|F(Vex)                                         , 188, 2 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #387 [ref=2x]
  { F(Vec)|F(Vex)                                         , 455, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #388 [ref=1x]
  { F(Vec)|F(Vex)                                         , 456, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #389 [ref=1x]
  { F(Vec)|F(Evex)|F(Avx512KZ_ER_SAE_B64)                 , 167, 3 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #390 [ref=1x]
  { F(Vec)|F(Evex)|F(Avx512KZ_ER_SAE_B32)                 , 167, 3 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #391 [ref=1x]
  { F(Vec)|F(Evex)|F(Avx512KZ_ER_SAE)                     , 443, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #392 [ref=1x]
  { F(Vec)|F(Evex)|F(Avx512KZ_ER_SAE)                     , 444, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #393 [ref=1x]
  { F(Vec)|F(Vsib)|F(Evex)|F(Avx512K)                     , 345, 2 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #394 [ref=1x]
  { F(Vec)|F(Evex)|F(Avx512KZ_B32)                        , 171, 2 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #395 [ref=2x]
  { F(Vec)|F(Evex)|F(Avx512KZ_B64)                        , 171, 2 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #396 [ref=2x]
  { F(Vec)|F(Vex)|F(Evex)|F(Avx512KZ_B32)                 , 170, 3 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #397 [ref=1x]
  { F(Vec)|F(Vex)|F(Evex)|F(Avx512KZ_B64)                 , 170, 3 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #398 [ref=1x]
  { F(Vec)|F(Vex)|F(Evex)|F(Avx512KZ_ER_SAE_B64)          , 182, 3 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #399 [ref=1x]
  { F(Vec)|F(Vex)                                         , 99 , 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #400 [ref=2x]
  { 0                                                     , 23 , 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #401 [ref=2x]
  { 0                                                     , 52 , 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #402 [ref=2x]
  { F(Lock)|F(XAcquire)|F(XRelease)                       , 49 , 4 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #403 [ref=1x]
  { 0                                                     , 475, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #404 [ref=1x]
  { F(Lock)|F(XAcquire)                                   , 49 , 8 , CONTROL(None)   , SINGLE_REG(RO)  , 0 }, // #405 [ref=1x]
  { 0                                                     , 476, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }, // #406 [ref=6x]
  { 0                                                     , 477, 1 , CONTROL(None)   , SINGLE_REG(None), 0 }  // #407 [ref=6x]
};
#undef SINGLE_REG
#undef CONTROL
#undef F
// ----------------------------------------------------------------------------
// ${InstCommonTable:End}

// ============================================================================
// [asmjit::x86::InstDB - CommonInfoTableB]
// ============================================================================

// ${InstCommonInfoTableB:Begin}
// ------------------- Automatically generated, do not edit -------------------
#define EXT(VAL) uint32_t(Features::k##VAL)
const InstDB::CommonInfoTableB InstDB::_commonInfoTableB[] = {
  { { 0 }, 0, 0 }, // #0 [ref=146x]
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
  { { EXT(CLWB) }, 0, 0 }, // #25 [ref=1x]
  { { EXT(CLZERO) }, 0, 0 }, // #26 [ref=1x]
  { { 0 }, 3, 0 }, // #27 [ref=1x]
  { { EXT(CMOV) }, 11, 0 }, // #28 [ref=6x]
  { { EXT(CMOV) }, 12, 0 }, // #29 [ref=8x]
  { { EXT(CMOV) }, 13, 0 }, // #30 [ref=6x]
  { { EXT(CMOV) }, 14, 0 }, // #31 [ref=4x]
  { { EXT(CMOV) }, 15, 0 }, // #32 [ref=4x]
  { { EXT(CMOV) }, 16, 0 }, // #33 [ref=2x]
  { { EXT(CMOV) }, 17, 0 }, // #34 [ref=6x]
  { { EXT(CMOV) }, 18, 0 }, // #35 [ref=2x]
  { { 0 }, 19, 0 }, // #36 [ref=2x]
  { { EXT(I486) }, 1, 0 }, // #37 [ref=2x]
  { { EXT(CMPXCHG16B) }, 5, 0 }, // #38 [ref=1x]
  { { EXT(CMPXCHG8B) }, 5, 0 }, // #39 [ref=1x]
  { { EXT(SSE2) }, 1, 0 }, // #40 [ref=2x]
  { { EXT(SSE) }, 1, 0 }, // #41 [ref=2x]
  { { EXT(I486) }, 0, 0 }, // #42 [ref=4x]
  { { EXT(SSE4_2) }, 0, 0 }, // #43 [ref=2x]
  { { 0 }, 20, 0 }, // #44 [ref=2x]
  { { EXT(MMX) }, 0, 0 }, // #45 [ref=1x]
  { { EXT(CET_IBT) }, 0, 0 }, // #46 [ref=2x]
  { { EXT(ENQCMD) }, 0, 0 }, // #47 [ref=2x]
  { { EXT(SSE4A) }, 0, 0 }, // #48 [ref=4x]
  { { 0 }, 21, 0 }, // #49 [ref=4x]
  { { EXT(3DNOW) }, 0, 0 }, // #50 [ref=21x]
  { { EXT(FXSR) }, 0, 0 }, // #51 [ref=4x]
  { { EXT(SMX) }, 0, 0 }, // #52 [ref=1x]
  { { EXT(GFNI) }, 0, 0 }, // #53 [ref=3x]
  { { EXT(CET_SS) }, 0, 0 }, // #54 [ref=9x]
  { { 0 }, 16, 0 }, // #55 [ref=5x]
  { { EXT(VMX) }, 0, 0 }, // #56 [ref=12x]
  { { 0 }, 11, 0 }, // #57 [ref=8x]
  { { 0 }, 12, 0 }, // #58 [ref=12x]
  { { 0 }, 13, 0 }, // #59 [ref=10x]
  { { 0 }, 14, 0 }, // #60 [ref=8x]
  { { 0 }, 15, 0 }, // #61 [ref=8x]
  { { 0 }, 17, 0 }, // #62 [ref=8x]
  { { 0 }, 18, 0 }, // #63 [ref=4x]
  { { EXT(AVX512_DQ) }, 0, 0 }, // #64 [ref=23x]
  { { EXT(AVX512_BW) }, 0, 0 }, // #65 [ref=22x]
  { { EXT(AVX512_F) }, 0, 0 }, // #66 [ref=37x]
  { { EXT(AVX512_DQ) }, 1, 0 }, // #67 [ref=3x]
  { { EXT(AVX512_BW) }, 1, 0 }, // #68 [ref=4x]
  { { EXT(AVX512_F) }, 1, 0 }, // #69 [ref=1x]
  { { EXT(LAHFSAHF) }, 22, 0 }, // #70 [ref=1x]
  { { EXT(AMX_TILE) }, 0, 0 }, // #71 [ref=7x]
  { { EXT(LWP) }, 0, 0 }, // #72 [ref=4x]
  { { 0 }, 23, 0 }, // #73 [ref=3x]
  { { EXT(LZCNT) }, 1, 0 }, // #74 [ref=1x]
  { { EXT(MMX2) }, 0, 0 }, // #75 [ref=8x]
  { { EXT(MCOMMIT) }, 1, 0 }, // #76 [ref=1x]
  { { EXT(MONITOR) }, 0, 0 }, // #77 [ref=2x]
  { { EXT(MONITORX) }, 0, 0 }, // #78 [ref=2x]
  { { EXT(MOVBE) }, 0, 0 }, // #79 [ref=1x]
  { { EXT(MMX), EXT(SSE2) }, 0, 0 }, // #80 [ref=46x]
  { { EXT(MOVDIR64B) }, 0, 0 }, // #81 [ref=1x]
  { { EXT(MOVDIRI) }, 0, 0 }, // #82 [ref=1x]
  { { EXT(BMI2) }, 0, 0 }, // #83 [ref=7x]
  { { EXT(SSSE3) }, 0, 0 }, // #84 [ref=15x]
  { { EXT(MMX2), EXT(SSE2) }, 0, 0 }, // #85 [ref=10x]
  { { EXT(PCLMULQDQ) }, 0, 0 }, // #86 [ref=1x]
  { { EXT(SSE4_2) }, 1, 0 }, // #87 [ref=4x]
  { { EXT(PCONFIG) }, 0, 0 }, // #88 [ref=1x]
  { { EXT(MMX2), EXT(SSE2), EXT(SSE4_1) }, 0, 0 }, // #89 [ref=1x]
  { { EXT(3DNOW2) }, 0, 0 }, // #90 [ref=5x]
  { { EXT(GEODE) }, 0, 0 }, // #91 [ref=2x]
  { { EXT(POPCNT) }, 1, 0 }, // #92 [ref=1x]
  { { 0 }, 24, 0 }, // #93 [ref=3x]
  { { EXT(PREFETCHW) }, 1, 0 }, // #94 [ref=1x]
  { { EXT(PREFETCHWT1) }, 1, 0 }, // #95 [ref=1x]
  { { EXT(SNP) }, 20, 0 }, // #96 [ref=3x]
  { { EXT(SSE4_1) }, 1, 0 }, // #97 [ref=1x]
  { { EXT(PTWRITE) }, 0, 0 }, // #98 [ref=1x]
  { { 0 }, 25, 0 }, // #99 [ref=3x]
  { { EXT(SNP) }, 1, 0 }, // #100 [ref=1x]
  { { 0 }, 26, 0 }, // #101 [ref=2x]
  { { EXT(FSGSBASE) }, 0, 0 }, // #102 [ref=4x]
  { { EXT(MSR) }, 0, 0 }, // #103 [ref=2x]
  { { EXT(RDPID) }, 0, 0 }, // #104 [ref=1x]
  { { EXT(OSPKE) }, 0, 0 }, // #105 [ref=1x]
  { { EXT(RDPRU) }, 0, 0 }, // #106 [ref=1x]
  { { EXT(RDRAND) }, 1, 0 }, // #107 [ref=1x]
  { { EXT(RDSEED) }, 1, 0 }, // #108 [ref=1x]
  { { EXT(RDTSC) }, 0, 0 }, // #109 [ref=1x]
  { { EXT(RDTSCP) }, 0, 0 }, // #110 [ref=1x]
  { { 0 }, 27, 0 }, // #111 [ref=2x]
  { { EXT(LAHFSAHF) }, 28, 0 }, // #112 [ref=1x]
  { { EXT(SERIALIZE) }, 0, 0 }, // #113 [ref=1x]
  { { EXT(SHA) }, 0, 0 }, // #114 [ref=7x]
  { { EXT(SKINIT) }, 0, 0 }, // #115 [ref=2x]
  { { EXT(AMX_BF16) }, 0, 0 }, // #116 [ref=1x]
  { { EXT(AMX_INT8) }, 0, 0 }, // #117 [ref=4x]
  { { EXT(WAITPKG) }, 1, 0 }, // #118 [ref=2x]
  { { EXT(WAITPKG) }, 0, 0 }, // #119 [ref=1x]
  { { EXT(AVX512_4FMAPS) }, 0, 0 }, // #120 [ref=4x]
  { { EXT(AVX), EXT(AVX512_F), EXT(AVX512_VL) }, 0, 0 }, // #121 [ref=46x]
  { { EXT(AVX), EXT(AVX512_F) }, 0, 0 }, // #122 [ref=32x]
  { { EXT(AVX) }, 0, 0 }, // #123 [ref=37x]
  { { EXT(AESNI), EXT(AVX), EXT(AVX512_F), EXT(AVX512_VL), EXT(VAES) }, 0, 0 }, // #124 [ref=4x]
  { { EXT(AESNI), EXT(AVX) }, 0, 0 }, // #125 [ref=2x]
  { { EXT(AVX512_F), EXT(AVX512_VL) }, 0, 0 }, // #126 [ref=112x]
  { { EXT(AVX), EXT(AVX512_DQ), EXT(AVX512_VL) }, 0, 0 }, // #127 [ref=8x]
  { { EXT(AVX512_BW), EXT(AVX512_VL) }, 0, 0 }, // #128 [ref=26x]
  { { EXT(AVX512_DQ), EXT(AVX512_VL) }, 0, 0 }, // #129 [ref=30x]
  { { EXT(AVX2) }, 0, 0 }, // #130 [ref=7x]
  { { EXT(AVX), EXT(AVX2), EXT(AVX512_F), EXT(AVX512_VL) }, 0, 0 }, // #131 [ref=39x]
  { { EXT(AVX), EXT(AVX512_F) }, 1, 0 }, // #132 [ref=4x]
  { { EXT(AVX512_BF16), EXT(AVX512_VL) }, 0, 0 }, // #133 [ref=3x]
  { { EXT(AVX512_F), EXT(AVX512_VL), EXT(F16C) }, 0, 0 }, // #134 [ref=2x]
  { { EXT(AVX512_ERI) }, 0, 0 }, // #135 [ref=10x]
  { { EXT(AVX512_F), EXT(AVX512_VL), EXT(FMA) }, 0, 0 }, // #136 [ref=36x]
  { { EXT(AVX512_F), EXT(FMA) }, 0, 0 }, // #137 [ref=24x]
  { { EXT(FMA4) }, 0, 0 }, // #138 [ref=20x]
  { { EXT(XOP) }, 0, 0 }, // #139 [ref=55x]
  { { EXT(AVX2), EXT(AVX512_F), EXT(AVX512_VL) }, 0, 0 }, // #140 [ref=19x]
  { { EXT(AVX512_PFI) }, 0, 0 }, // #141 [ref=16x]
  { { EXT(AVX), EXT(AVX512_F), EXT(AVX512_VL), EXT(GFNI) }, 0, 0 }, // #142 [ref=3x]
  { { EXT(AVX), EXT(AVX2) }, 0, 0 }, // #143 [ref=17x]
  { { EXT(AVX512_VP2INTERSECT) }, 0, 0 }, // #144 [ref=2x]
  { { EXT(AVX512_4VNNIW) }, 0, 0 }, // #145 [ref=2x]
  { { EXT(AVX), EXT(AVX2), EXT(AVX512_BW), EXT(AVX512_VL) }, 0, 0 }, // #146 [ref=54x]
  { { EXT(AVX2), EXT(AVX512_BW), EXT(AVX512_VL) }, 0, 0 }, // #147 [ref=2x]
  { { EXT(AVX512_CDI), EXT(AVX512_VL) }, 0, 0 }, // #148 [ref=6x]
  { { EXT(AVX), EXT(AVX512_F), EXT(AVX512_VL), EXT(PCLMULQDQ), EXT(VPCLMULQDQ) }, 0, 0 }, // #149 [ref=1x]
  { { EXT(AVX) }, 1, 0 }, // #150 [ref=7x]
  { { EXT(AVX512_VBMI2), EXT(AVX512_VL) }, 0, 0 }, // #151 [ref=16x]
  { { EXT(AVX512_VL), EXT(AVX512_VNNI) }, 0, 0 }, // #152 [ref=4x]
  { { EXT(AVX512_VBMI), EXT(AVX512_VL) }, 0, 0 }, // #153 [ref=4x]
  { { EXT(AVX), EXT(AVX512_BW) }, 0, 0 }, // #154 [ref=4x]
  { { EXT(AVX), EXT(AVX512_DQ) }, 0, 0 }, // #155 [ref=4x]
  { { EXT(AVX512_IFMA), EXT(AVX512_VL) }, 0, 0 }, // #156 [ref=2x]
  { { EXT(AVX512_BITALG), EXT(AVX512_VL) }, 0, 0 }, // #157 [ref=3x]
  { { EXT(AVX512_VL), EXT(AVX512_VPOPCNTDQ) }, 0, 0 }, // #158 [ref=2x]
  { { EXT(WBNOINVD) }, 0, 0 }, // #159 [ref=1x]
  { { EXT(RTM) }, 0, 0 }, // #160 [ref=3x]
  { { EXT(XSAVE) }, 0, 0 }, // #161 [ref=6x]
  { { EXT(TSXLDTRK) }, 0, 0 }, // #162 [ref=2x]
  { { EXT(XSAVES) }, 0, 0 }, // #163 [ref=4x]
  { { EXT(XSAVEC) }, 0, 0 }, // #164 [ref=2x]
  { { EXT(XSAVEOPT) }, 0, 0 }, // #165 [ref=2x]
  { { EXT(TSX) }, 1, 0 }  // #166 [ref=1x]
};
#undef EXT

#define FLAG(VAL) uint32_t(Status::k##VAL)
const InstDB::RWFlagsInfoTable InstDB::_rwFlagsInfoTable[] = {
  { 0, 0 }, // #0 [ref=1315x]
  { 0, FLAG(AF) | FLAG(CF) | FLAG(OF) | FLAG(PF) | FLAG(SF) | FLAG(ZF) }, // #1 [ref=83x]
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
// ${InstCommonInfoTableB:End}

// ============================================================================
// [asmjit::Inst - NameData]
// ============================================================================

#ifndef ASMJIT_NO_TEXT
// ${NameData:Begin}
// ------------------- Automatically generated, do not edit -------------------
const char InstDB::_nameData[] =
  "\0" "aaa\0" "aad\0" "aam\0" "aas\0" "adc\0" "adcx\0" "adox\0" "arpl\0" "bextr\0" "blcfill\0" "blci\0" "blcic\0"
  "blcmsk\0" "blcs\0" "blsfill\0" "blsi\0" "blsic\0" "blsmsk\0" "blsr\0" "bndcl\0" "bndcn\0" "bndcu\0" "bndldx\0"
  "bndmk\0" "bndmov\0" "bndstx\0" "bound\0" "bsf\0" "bsr\0" "bswap\0" "bt\0" "btc\0" "btr\0" "bts\0" "bzhi\0" "cbw\0"
  "cdq\0" "cdqe\0" "clac\0" "clc\0" "cld\0" "cldemote\0" "clflush\0" "clflushopt\0" "clgi\0" "cli\0" "clrssbsy\0"
  "clts\0" "clwb\0" "clzero\0" "cmc\0" "cmova\0" "cmovae\0" "cmovc\0" "cmovg\0" "cmovge\0" "cmovl\0" "cmovle\0"
  "cmovna\0" "cmovnae\0" "cmovnc\0" "cmovng\0" "cmovnge\0" "cmovnl\0" "cmovnle\0" "cmovno\0" "cmovnp\0" "cmovns\0"
  "cmovnz\0" "cmovo\0" "cmovp\0" "cmovpe\0" "cmovpo\0" "cmovs\0" "cmovz\0" "cmp\0" "cmps\0" "cmpxchg\0" "cmpxchg16b\0"
  "cmpxchg8b\0" "cpuid\0" "cqo\0" "crc32\0" "cvtpd2pi\0" "cvtpi2pd\0" "cvtpi2ps\0" "cvtps2pi\0" "cvttpd2pi\0"
  "cvttps2pi\0" "cwd\0" "cwde\0" "daa\0" "das\0" "endbr32\0" "endbr64\0" "enqcmd\0" "enqcmds\0" "f2xm1\0" "fabs\0"
  "faddp\0" "fbld\0" "fbstp\0" "fchs\0" "fclex\0" "fcmovb\0" "fcmovbe\0" "fcmove\0" "fcmovnb\0" "fcmovnbe\0"
  "fcmovne\0" "fcmovnu\0" "fcmovu\0" "fcom\0" "fcomi\0" "fcomip\0" "fcomp\0" "fcompp\0" "fcos\0" "fdecstp\0" "fdiv\0"
  "fdivp\0" "fdivr\0" "fdivrp\0" "femms\0" "ffree\0" "fiadd\0" "ficom\0" "ficomp\0" "fidiv\0" "fidivr\0" "fild\0"
  "fimul\0" "fincstp\0" "finit\0" "fist\0" "fistp\0" "fisttp\0" "fisub\0" "fisubr\0" "fld\0" "fld1\0" "fldcw\0"
  "fldenv\0" "fldl2e\0" "fldl2t\0" "fldlg2\0" "fldln2\0" "fldpi\0" "fldz\0" "fmulp\0" "fnclex\0" "fninit\0" "fnop\0"
  "fnsave\0" "fnstcw\0" "fnstenv\0" "fnstsw\0" "fpatan\0" "fprem\0" "fprem1\0" "fptan\0" "frndint\0" "frstor\0"
  "fsave\0" "fscale\0" "fsin\0" "fsincos\0" "fsqrt\0" "fst\0" "fstcw\0" "fstenv\0" "fstp\0" "fstsw\0" "fsubp\0"
  "fsubrp\0" "ftst\0" "fucom\0" "fucomi\0" "fucomip\0" "fucomp\0" "fucompp\0" "fwait\0" "fxam\0" "fxch\0" "fxrstor\0"
  "fxrstor64\0" "fxsave\0" "fxsave64\0" "fxtract\0" "fyl2x\0" "fyl2xp1\0" "getsec\0" "hlt\0" "inc\0" "incsspd\0"
  "incsspq\0" "insertq\0" "int3\0" "into\0" "invept\0" "invlpg\0" "invlpga\0" "invpcid\0" "invvpid\0" "iret\0"
  "iretd\0" "iretq\0" "iretw\0" "ja\0" "jae\0" "jb\0" "jbe\0" "jc\0" "je\0" "jecxz\0" "jg\0" "jge\0" "jl\0" "jle\0"
  "jmp\0" "jna\0" "jnae\0" "jnb\0" "jnbe\0" "jnc\0" "jne\0" "jng\0" "jnge\0" "jnl\0" "jnle\0" "jno\0" "jnp\0" "jns\0"
  "jnz\0" "jo\0" "jp\0" "jpe\0" "jpo\0" "js\0" "jz\0" "kaddb\0" "kaddd\0" "kaddq\0" "kaddw\0" "kandb\0" "kandd\0"
  "kandnb\0" "kandnd\0" "kandnq\0" "kandnw\0" "kandq\0" "kandw\0" "kmovb\0" "kmovw\0" "knotb\0" "knotd\0" "knotq\0"
  "knotw\0" "korb\0" "kord\0" "korq\0" "kortestb\0" "kortestd\0" "kortestq\0" "kortestw\0" "korw\0" "kshiftlb\0"
  "kshiftld\0" "kshiftlq\0" "kshiftlw\0" "kshiftrb\0" "kshiftrd\0" "kshiftrq\0" "kshiftrw\0" "ktestb\0" "ktestd\0"
  "ktestq\0" "ktestw\0" "kunpckbw\0" "kunpckdq\0" "kunpckwd\0" "kxnorb\0" "kxnord\0" "kxnorq\0" "kxnorw\0" "kxorb\0"
  "kxord\0" "kxorq\0" "kxorw\0" "lahf\0" "lar\0" "lds\0" "ldtilecfg\0" "lea\0" "leave\0" "les\0" "lfence\0" "lfs\0"
  "lgdt\0" "lgs\0" "lidt\0" "lldt\0" "llwpcb\0" "lmsw\0" "lods\0" "loop\0" "loope\0" "loopne\0" "lsl\0" "ltr\0"
  "lwpins\0" "lwpval\0" "lzcnt\0" "mcommit\0" "mfence\0" "monitorx\0" "movdir64b\0" "movdiri\0" "movdq2q\0" "movnti\0"
  "movntq\0" "movntsd\0" "movntss\0" "movq2dq\0" "movsx\0" "movsxd\0" "movzx\0" "mulx\0" "mwaitx\0" "neg\0" "not\0"
  "out\0" "outs\0" "pavgusb\0" "pconfig\0" "pdep\0" "pext\0" "pf2id\0" "pf2iw\0" "pfacc\0" "pfadd\0" "pfcmpeq\0"
  "pfcmpge\0" "pfcmpgt\0" "pfmax\0" "pfmin\0" "pfmul\0" "pfnacc\0" "pfpnacc\0" "pfrcp\0" "pfrcpit1\0" "pfrcpit2\0"
  "pfrcpv\0" "pfrsqit1\0" "pfrsqrt\0" "pfrsqrtv\0" "pfsub\0" "pfsubr\0" "pi2fd\0" "pi2fw\0" "pmulhrw\0" "pop\0"
  "popa\0" "popad\0" "popcnt\0" "popf\0" "popfd\0" "popfq\0" "prefetch\0" "prefetchnta\0" "prefetcht0\0" "prefetcht1\0"
  "prefetcht2\0" "prefetchw\0" "prefetchwt1\0" "pshufw\0" "psmash\0" "pswapd\0" "ptwrite\0" "push\0" "pusha\0"
  "pushad\0" "pushf\0" "pushfd\0" "pushfq\0" "pvalidate\0" "rcl\0" "rcr\0" "rdfsbase\0" "rdgsbase\0" "rdmsr\0"
  "rdpid\0" "rdpkru\0" "rdpmc\0" "rdpru\0" "rdrand\0" "rdseed\0" "rdsspd\0" "rdsspq\0" "rdtsc\0" "rdtscp\0"
  "rmpadjust\0" "rmpupdate\0" "rol\0" "ror\0" "rorx\0" "rsm\0" "rstorssp\0" "sahf\0" "sal\0" "sar\0" "sarx\0"
  "saveprevssp\0" "sbb\0" "scas\0" "serialize\0" "seta\0" "setae\0" "setb\0" "setbe\0" "setc\0" "sete\0" "setg\0"
  "setge\0" "setl\0" "setle\0" "setna\0" "setnae\0" "setnb\0" "setnbe\0" "setnc\0" "setne\0" "setng\0" "setnge\0"
  "setnl\0" "setnle\0" "setno\0" "setnp\0" "setns\0" "setnz\0" "seto\0" "setp\0" "setpe\0" "setpo\0" "sets\0"
  "setssbsy\0" "setz\0" "sfence\0" "sgdt\0" "sha1msg1\0" "sha1msg2\0" "sha1nexte\0" "sha1rnds4\0" "sha256msg1\0"
  "sha256msg2\0" "sha256rnds2\0" "shl\0" "shlx\0" "shr\0" "shrd\0" "shrx\0" "sidt\0" "skinit\0" "sldt\0" "slwpcb\0"
  "smsw\0" "stac\0" "stc\0" "stgi\0" "sti\0" "stos\0" "str\0" "sttilecfg\0" "swapgs\0" "syscall\0" "sysenter\0"
  "sysexit\0" "sysexit64\0" "sysret\0" "sysret64\0" "t1mskc\0" "tdpbf16ps\0" "tdpbssd\0" "tdpbsud\0" "tdpbusd\0"
  "tdpbuud\0" "tileloadd\0" "tileloaddt1\0" "tilerelease\0" "tilestored\0" "tilezero\0" "tpause\0" "tzcnt\0" "tzmsk\0"
  "ud0\0" "ud1\0" "ud2\0" "umonitor\0" "umwait\0" "v4fmaddps\0" "v4fmaddss\0" "v4fnmaddps\0" "v4fnmaddss\0" "vaddpd\0"
  "vaddps\0" "vaddsd\0" "vaddss\0" "vaddsubpd\0" "vaddsubps\0" "vaesdec\0" "vaesdeclast\0" "vaesenc\0" "vaesenclast\0"
  "vaesimc\0" "vaeskeygenassist\0" "valignd\0" "valignq\0" "vandnpd\0" "vandnps\0" "vandpd\0" "vandps\0" "vblendmb\0"
  "vblendmd\0" "vblendmpd\0" "vblendmps\0" "vblendmq\0" "vblendmw\0" "vblendpd\0" "vblendps\0" "vblendvpd\0"
  "vblendvps\0" "vbroadcastf128\0" "vbroadcastf32x2\0" "vbroadcastf32x4\0" "vbroadcastf32x8\0" "vbroadcastf64x2\0"
  "vbroadcastf64x4\0" "vbroadcasti128\0" "vbroadcasti32x2\0" "vbroadcasti32x4\0" "vbroadcasti32x8\0"
  "vbroadcasti64x2\0" "vbroadcasti64x4\0" "vbroadcastsd\0" "vbroadcastss\0" "vcmppd\0" "vcmpps\0" "vcmpsd\0" "vcmpss\0"
  "vcomisd\0" "vcomiss\0" "vcompresspd\0" "vcompressps\0" "vcvtdq2pd\0" "vcvtdq2ps\0" "vcvtne2ps2bf16\0"
  "vcvtneps2bf16\0" "vcvtpd2dq\0" "vcvtpd2ps\0" "vcvtpd2qq\0" "vcvtpd2udq\0" "vcvtpd2uqq\0" "vcvtph2ps\0" "vcvtps2dq\0"
  "vcvtps2pd\0" "vcvtps2ph\0" "vcvtps2qq\0" "vcvtps2udq\0" "vcvtps2uqq\0" "vcvtqq2pd\0" "vcvtqq2ps\0" "vcvtsd2si\0"
  "vcvtsd2ss\0" "vcvtsd2usi\0" "vcvtsi2sd\0" "vcvtsi2ss\0" "vcvtss2sd\0" "vcvtss2si\0" "vcvtss2usi\0" "vcvttpd2dq\0"
  "vcvttpd2qq\0" "vcvttpd2udq\0" "vcvttpd2uqq\0" "vcvttps2dq\0" "vcvttps2qq\0" "vcvttps2udq\0" "vcvttps2uqq\0"
  "vcvttsd2si\0" "vcvttsd2usi\0" "vcvttss2si\0" "vcvttss2usi\0" "vcvtudq2pd\0" "vcvtudq2ps\0" "vcvtuqq2pd\0"
  "vcvtuqq2ps\0" "vcvtusi2sd\0" "vcvtusi2ss\0" "vdbpsadbw\0" "vdivpd\0" "vdivps\0" "vdivsd\0" "vdivss\0" "vdpbf16ps\0"
  "vdppd\0" "vdpps\0" "verr\0" "verw\0" "vexp2pd\0" "vexp2ps\0" "vexpandpd\0" "vexpandps\0" "vextractf128\0"
  "vextractf32x4\0" "vextractf32x8\0" "vextractf64x2\0" "vextractf64x4\0" "vextracti128\0" "vextracti32x4\0"
  "vextracti32x8\0" "vextracti64x2\0" "vextracti64x4\0" "vextractps\0" "vfixupimmpd\0" "vfixupimmps\0" "vfixupimmsd\0"
  "vfixupimmss\0" "vfmadd132pd\0" "vfmadd132ps\0" "vfmadd132sd\0" "vfmadd132ss\0" "vfmadd213pd\0" "vfmadd213ps\0"
  "vfmadd213sd\0" "vfmadd213ss\0" "vfmadd231pd\0" "vfmadd231ps\0" "vfmadd231sd\0" "vfmadd231ss\0" "vfmaddpd\0"
  "vfmaddps\0" "vfmaddsd\0" "vfmaddss\0" "vfmaddsub132pd\0" "vfmaddsub132ps\0" "vfmaddsub213pd\0" "vfmaddsub213ps\0"
  "vfmaddsub231pd\0" "vfmaddsub231ps\0" "vfmaddsubpd\0" "vfmaddsubps\0" "vfmsub132pd\0" "vfmsub132ps\0" "vfmsub132sd\0"
  "vfmsub132ss\0" "vfmsub213pd\0" "vfmsub213ps\0" "vfmsub213sd\0" "vfmsub213ss\0" "vfmsub231pd\0" "vfmsub231ps\0"
  "vfmsub231sd\0" "vfmsub231ss\0" "vfmsubadd132pd\0" "vfmsubadd132ps\0" "vfmsubadd213pd\0" "vfmsubadd213ps\0"
  "vfmsubadd231pd\0" "vfmsubadd231ps\0" "vfmsubaddpd\0" "vfmsubaddps\0" "vfmsubpd\0" "vfmsubps\0" "vfmsubsd\0"
  "vfmsubss\0" "vfnmadd132pd\0" "vfnmadd132ps\0" "vfnmadd132sd\0" "vfnmadd132ss\0" "vfnmadd213pd\0" "vfnmadd213ps\0"
  "vfnmadd213sd\0" "vfnmadd213ss\0" "vfnmadd231pd\0" "vfnmadd231ps\0" "vfnmadd231sd\0" "vfnmadd231ss\0" "vfnmaddpd\0"
  "vfnmaddps\0" "vfnmaddsd\0" "vfnmaddss\0" "vfnmsub132pd\0" "vfnmsub132ps\0" "vfnmsub132sd\0" "vfnmsub132ss\0"
  "vfnmsub213pd\0" "vfnmsub213ps\0" "vfnmsub213sd\0" "vfnmsub213ss\0" "vfnmsub231pd\0" "vfnmsub231ps\0"
  "vfnmsub231sd\0" "vfnmsub231ss\0" "vfnmsubpd\0" "vfnmsubps\0" "vfnmsubsd\0" "vfnmsubss\0" "vfpclasspd\0"
  "vfpclassps\0" "vfpclasssd\0" "vfpclassss\0" "vfrczpd\0" "vfrczps\0" "vfrczsd\0" "vfrczss\0" "vgatherdpd\0"
  "vgatherdps\0" "vgatherpf0dpd\0" "vgatherpf0dps\0" "vgatherpf0qpd\0" "vgatherpf0qps\0" "vgatherpf1dpd\0"
  "vgatherpf1dps\0" "vgatherpf1qpd\0" "vgatherpf1qps\0" "vgatherqpd\0" "vgatherqps\0" "vgetexppd\0" "vgetexpps\0"
  "vgetexpsd\0" "vgetexpss\0" "vgetmantpd\0" "vgetmantps\0" "vgetmantsd\0" "vgetmantss\0" "vgf2p8affineinvqb\0"
  "vgf2p8affineqb\0" "vgf2p8mulb\0" "vhaddpd\0" "vhaddps\0" "vhsubpd\0" "vhsubps\0" "vinsertf128\0" "vinsertf32x4\0"
  "vinsertf32x8\0" "vinsertf64x2\0" "vinsertf64x4\0" "vinserti128\0" "vinserti32x4\0" "vinserti32x8\0" "vinserti64x2\0"
  "vinserti64x4\0" "vinsertps\0" "vlddqu\0" "vldmxcsr\0" "vmaskmovdqu\0" "vmaskmovpd\0" "vmaskmovps\0" "vmaxpd\0"
  "vmaxps\0" "vmaxsd\0" "vmaxss\0" "vmcall\0" "vmclear\0" "vmfunc\0" "vminpd\0" "vminps\0" "vminsd\0" "vminss\0"
  "vmlaunch\0" "vmload\0" "vmmcall\0" "vmovapd\0" "vmovaps\0" "vmovd\0" "vmovddup\0" "vmovdqa\0" "vmovdqa32\0"
  "vmovdqa64\0" "vmovdqu\0" "vmovdqu16\0" "vmovdqu32\0" "vmovdqu64\0" "vmovdqu8\0" "vmovhlps\0" "vmovhpd\0" "vmovhps\0"
  "vmovlhps\0" "vmovlpd\0" "vmovlps\0" "vmovmskpd\0" "vmovmskps\0" "vmovntdq\0" "vmovntdqa\0" "vmovntpd\0" "vmovntps\0"
  "vmovq\0" "vmovsd\0" "vmovshdup\0" "vmovsldup\0" "vmovss\0" "vmovupd\0" "vmovups\0" "vmpsadbw\0" "vmptrld\0"
  "vmptrst\0" "vmread\0" "vmresume\0" "vmrun\0" "vmsave\0" "vmulpd\0" "vmulps\0" "vmulsd\0" "vmulss\0" "vmwrite\0"
  "vmxon\0" "vorpd\0" "vorps\0" "vp2intersectd\0" "vp2intersectq\0" "vp4dpwssd\0" "vp4dpwssds\0" "vpabsb\0" "vpabsd\0"
  "vpabsq\0" "vpabsw\0" "vpackssdw\0" "vpacksswb\0" "vpackusdw\0" "vpackuswb\0" "vpaddb\0" "vpaddd\0" "vpaddq\0"
  "vpaddsb\0" "vpaddsw\0" "vpaddusb\0" "vpaddusw\0" "vpaddw\0" "vpalignr\0" "vpand\0" "vpandd\0" "vpandn\0" "vpandnd\0"
  "vpandnq\0" "vpandq\0" "vpavgb\0" "vpavgw\0" "vpblendd\0" "vpblendvb\0" "vpblendw\0" "vpbroadcastb\0"
  "vpbroadcastd\0" "vpbroadcastmb2d\0" "vpbroadcastmb2q\0" "vpbroadcastq\0" "vpbroadcastw\0" "vpclmulqdq\0" "vpcmov\0"
  "vpcmpb\0" "vpcmpd\0" "vpcmpeqb\0" "vpcmpeqd\0" "vpcmpeqq\0" "vpcmpeqw\0" "vpcmpestri\0" "vpcmpestrm\0" "vpcmpgtb\0"
  "vpcmpgtd\0" "vpcmpgtq\0" "vpcmpgtw\0" "vpcmpistri\0" "vpcmpistrm\0" "vpcmpq\0" "vpcmpub\0" "vpcmpud\0" "vpcmpuq\0"
  "vpcmpuw\0" "vpcmpw\0" "vpcomb\0" "vpcomd\0" "vpcompressb\0" "vpcompressd\0" "vpcompressq\0" "vpcompressw\0"
  "vpcomq\0" "vpcomub\0" "vpcomud\0" "vpcomuq\0" "vpcomuw\0" "vpcomw\0" "vpconflictd\0" "vpconflictq\0" "vpdpbusd\0"
  "vpdpbusds\0" "vpdpwssd\0" "vpdpwssds\0" "vperm2f128\0" "vperm2i128\0" "vpermb\0" "vpermd\0" "vpermi2b\0"
  "vpermi2d\0" "vpermi2pd\0" "vpermi2ps\0" "vpermi2q\0" "vpermi2w\0" "vpermil2pd\0" "vpermil2ps\0" "vpermilpd\0"
  "vpermilps\0" "vpermpd\0" "vpermps\0" "vpermq\0" "vpermt2b\0" "vpermt2d\0" "vpermt2pd\0" "vpermt2ps\0" "vpermt2q\0"
  "vpermt2w\0" "vpermw\0" "vpexpandb\0" "vpexpandd\0" "vpexpandq\0" "vpexpandw\0" "vpextrb\0" "vpextrd\0" "vpextrq\0"
  "vpextrw\0" "vpgatherdd\0" "vpgatherdq\0" "vpgatherqd\0" "vpgatherqq\0" "vphaddbd\0" "vphaddbq\0" "vphaddbw\0"
  "vphaddd\0" "vphadddq\0" "vphaddsw\0" "vphaddubd\0" "vphaddubq\0" "vphaddubw\0" "vphaddudq\0" "vphadduwd\0"
  "vphadduwq\0" "vphaddw\0" "vphaddwd\0" "vphaddwq\0" "vphminposuw\0" "vphsubbw\0" "vphsubd\0" "vphsubdq\0"
  "vphsubsw\0" "vphsubw\0" "vphsubwd\0" "vpinsrb\0" "vpinsrd\0" "vpinsrq\0" "vpinsrw\0" "vplzcntd\0" "vplzcntq\0"
  "vpmacsdd\0" "vpmacsdqh\0" "vpmacsdql\0" "vpmacssdd\0" "vpmacssdqh\0" "vpmacssdql\0" "vpmacsswd\0" "vpmacssww\0"
  "vpmacswd\0" "vpmacsww\0" "vpmadcsswd\0" "vpmadcswd\0" "vpmadd52huq\0" "vpmadd52luq\0" "vpmaddubsw\0" "vpmaddwd\0"
  "vpmaskmovd\0" "vpmaskmovq\0" "vpmaxsb\0" "vpmaxsd\0" "vpmaxsq\0" "vpmaxsw\0" "vpmaxub\0" "vpmaxud\0" "vpmaxuq\0"
  "vpmaxuw\0" "vpminsb\0" "vpminsd\0" "vpminsq\0" "vpminsw\0" "vpminub\0" "vpminud\0" "vpminuq\0" "vpminuw\0"
  "vpmovb2m\0" "vpmovd2m\0" "vpmovdb\0" "vpmovdw\0" "vpmovm2b\0" "vpmovm2d\0" "vpmovm2q\0" "vpmovm2w\0" "vpmovmskb\0"
  "vpmovq2m\0" "vpmovqb\0" "vpmovqd\0" "vpmovqw\0" "vpmovsdb\0" "vpmovsdw\0" "vpmovsqb\0" "vpmovsqd\0" "vpmovsqw\0"
  "vpmovswb\0" "vpmovsxbd\0" "vpmovsxbq\0" "vpmovsxbw\0" "vpmovsxdq\0" "vpmovsxwd\0" "vpmovsxwq\0" "vpmovusdb\0"
  "vpmovusdw\0" "vpmovusqb\0" "vpmovusqd\0" "vpmovusqw\0" "vpmovuswb\0" "vpmovw2m\0" "vpmovwb\0" "vpmovzxbd\0"
  "vpmovzxbq\0" "vpmovzxbw\0" "vpmovzxdq\0" "vpmovzxwd\0" "vpmovzxwq\0" "vpmuldq\0" "vpmulhrsw\0" "vpmulhuw\0"
  "vpmulhw\0" "vpmulld\0" "vpmullq\0" "vpmullw\0" "vpmultishiftqb\0" "vpmuludq\0" "vpopcntb\0" "vpopcntd\0"
  "vpopcntq\0" "vpopcntw\0" "vpor\0" "vpord\0" "vporq\0" "vpperm\0" "vprold\0" "vprolq\0" "vprolvd\0" "vprolvq\0"
  "vprord\0" "vprorq\0" "vprorvd\0" "vprorvq\0" "vprotb\0" "vprotd\0" "vprotq\0" "vprotw\0" "vpsadbw\0" "vpscatterdd\0"
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
  "vrcp28ps\0" "vrcp28sd\0" "vrcp28ss\0" "vrcpps\0" "vrcpss\0" "vreducepd\0" "vreduceps\0" "vreducesd\0" "vreducess\0"
  "vrndscalepd\0" "vrndscaleps\0" "vrndscalesd\0" "vrndscaless\0" "vroundpd\0" "vroundps\0" "vroundsd\0" "vroundss\0"
  "vrsqrt14pd\0" "vrsqrt14ps\0" "vrsqrt14sd\0" "vrsqrt14ss\0" "vrsqrt28pd\0" "vrsqrt28ps\0" "vrsqrt28sd\0"
  "vrsqrt28ss\0" "vrsqrtps\0" "vrsqrtss\0" "vscalefpd\0" "vscalefps\0" "vscalefsd\0" "vscalefss\0" "vscatterdpd\0"
  "vscatterdps\0" "vscatterpf0dpd\0" "vscatterpf0dps\0" "vscatterpf0qpd\0" "vscatterpf0qps\0" "vscatterpf1dpd\0"
  "vscatterpf1dps\0" "vscatterpf1qpd\0" "vscatterpf1qps\0" "vscatterqpd\0" "vscatterqps\0" "vshuff32x4\0"
  "vshuff64x2\0" "vshufi32x4\0" "vshufi64x2\0" "vshufpd\0" "vshufps\0" "vsqrtpd\0" "vsqrtps\0" "vsqrtsd\0" "vsqrtss\0"
  "vstmxcsr\0" "vsubpd\0" "vsubps\0" "vsubsd\0" "vsubss\0" "vtestpd\0" "vtestps\0" "vucomisd\0" "vucomiss\0"
  "vunpckhpd\0" "vunpckhps\0" "vunpcklpd\0" "vunpcklps\0" "vxorpd\0" "vxorps\0" "vzeroall\0" "vzeroupper\0" "wbinvd\0"
  "wbnoinvd\0" "wrfsbase\0" "wrgsbase\0" "wrmsr\0" "wrssd\0" "wrssq\0" "wrussd\0" "wrussq\0" "xabort\0" "xadd\0"
  "xbegin\0" "xend\0" "xgetbv\0" "xlatb\0" "xresldtrk\0" "xrstors\0" "xrstors64\0" "xsavec\0" "xsavec64\0" "xsaveopt\0"
  "xsaveopt64\0" "xsaves\0" "xsaves64\0" "xsetbv\0" "xsusldtrk\0" "xtest";

const InstDB::InstNameIndex InstDB::instNameIndex[26] = {
  { Inst::kIdAaa          , Inst::kIdArpl          + 1 },
  { Inst::kIdBextr        , Inst::kIdBzhi          + 1 },
  { Inst::kIdCall         , Inst::kIdCwde          + 1 },
  { Inst::kIdDaa          , Inst::kIdDpps          + 1 },
  { Inst::kIdEmms         , Inst::kIdExtrq         + 1 },
  { Inst::kIdF2xm1        , Inst::kIdFyl2xp1       + 1 },
  { Inst::kIdGetsec       , Inst::kIdGf2p8mulb     + 1 },
  { Inst::kIdHaddpd       , Inst::kIdHsubps        + 1 },
  { Inst::kIdIdiv         , Inst::kIdIretw         + 1 },
  { Inst::kIdJa           , Inst::kIdJz            + 1 },
  { Inst::kIdKaddb        , Inst::kIdKxorw         + 1 },
  { Inst::kIdLahf         , Inst::kIdLzcnt         + 1 },
  { Inst::kIdMaskmovdqu   , Inst::kIdMwaitx        + 1 },
  { Inst::kIdNeg          , Inst::kIdNot           + 1 },
  { Inst::kIdOr           , Inst::kIdOuts          + 1 },
  { Inst::kIdPabsb        , Inst::kIdPxor          + 1 },
  { Inst::kIdNone         , Inst::kIdNone          + 1 },
  { Inst::kIdRcl          , Inst::kIdRstorssp      + 1 },
  { Inst::kIdSahf         , Inst::kIdSysret64      + 1 },
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

// ============================================================================
// [asmjit::x86::InstDB - InstSignature / OpSignature]
// ============================================================================

#ifndef ASMJIT_NO_VALIDATION
// ${InstSignatureTable:Begin}
// ------------------- Automatically generated, do not edit -------------------
#define ROW(count, x86, x64, implicit, o0, o1, o2, o3, o4, o5)  \
  { count, (x86 ? uint8_t(InstDB::kModeX86) : uint8_t(0)) |     \
           (x64 ? uint8_t(InstDB::kModeX64) : uint8_t(0)) ,     \
    implicit,                                                   \
    0,                                                          \
    { o0, o1, o2, o3, o4, o5 }                                  \
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
  ROW(2, 1, 1, 0, 1  , 2  , 0  , 0  , 0  , 0  ), // #49  {r8lo|r8hi|m8|mem, r8lo|r8hi}
  ROW(2, 1, 1, 0, 27 , 4  , 0  , 0  , 0  , 0  ), //      {r16|m16|mem, r16}
  ROW(2, 1, 1, 0, 28 , 6  , 0  , 0  , 0  , 0  ), //      {r32|m32|mem, r32}
  ROW(2, 0, 1, 0, 15 , 8  , 0  , 0  , 0  , 0  ), // #52  {r64|m64|mem, r64}
  ROW(2, 1, 1, 0, 2  , 18 , 0  , 0  , 0  , 0  ), //      {r8lo|r8hi, m8|mem}
  ROW(2, 1, 1, 0, 4  , 21 , 0  , 0  , 0  , 0  ), //      {r16, m16|mem}
  ROW(2, 1, 1, 0, 6  , 29 , 0  , 0  , 0  , 0  ), //      {r32, m32|mem}
  ROW(2, 0, 1, 0, 8  , 30 , 0  , 0  , 0  , 0  ), //      {r64, m64|mem}
  ROW(2, 1, 1, 0, 9  , 10 , 0  , 0  , 0  , 0  ), // #57  {r8lo|r8hi|m8, i8|u8}
  ROW(2, 1, 1, 0, 11 , 12 , 0  , 0  , 0  , 0  ), //      {r16|m16, i16|u16}
  ROW(2, 1, 1, 0, 13 , 14 , 0  , 0  , 0  , 0  ), //      {r32|m32, i32|u32}
  ROW(2, 0, 1, 0, 15 , 24 , 0  , 0  , 0  , 0  ), //      {r64|m64|mem, i32|r64}
  ROW(2, 1, 1, 0, 1  , 2  , 0  , 0  , 0  , 0  ), //      {r8lo|r8hi|m8|mem, r8lo|r8hi}
  ROW(2, 1, 1, 0, 27 , 4  , 0  , 0  , 0  , 0  ), //      {r16|m16|mem, r16}
  ROW(2, 1, 1, 0, 28 , 6  , 0  , 0  , 0  , 0  ), //      {r32|m32|mem, r32}
  ROW(2, 1, 1, 0, 4  , 21 , 0  , 0  , 0  , 0  ), // #64  {r16, m16|mem}
  ROW(2, 1, 1, 0, 6  , 29 , 0  , 0  , 0  , 0  ), //      {r32, m32|mem}
  ROW(2, 0, 1, 0, 8  , 30 , 0  , 0  , 0  , 0  ), //      {r64, m64|mem}
  ROW(2, 1, 1, 0, 21 , 4  , 0  , 0  , 0  , 0  ), //      {m16|mem, r16}
  ROW(2, 1, 1, 0, 29 , 6  , 0  , 0  , 0  , 0  ), // #68  {m32|mem, r32}
  ROW(2, 0, 1, 0, 30 , 8  , 0  , 0  , 0  , 0  ), //      {m64|mem, r64}
  ROW(2, 1, 1, 0, 45 , 46 , 0  , 0  , 0  , 0  ), // #70  {xmm, xmm|m128|mem}
  ROW(2, 1, 1, 0, 47 , 45 , 0  , 0  , 0  , 0  ), // #71  {m128|mem, xmm}
  ROW(2, 1, 1, 0, 48 , 49 , 0  , 0  , 0  , 0  ), //      {ymm, ymm|m256|mem}
  ROW(2, 1, 1, 0, 50 , 48 , 0  , 0  , 0  , 0  ), //      {m256|mem, ymm}
  ROW(2, 1, 1, 0, 51 , 52 , 0  , 0  , 0  , 0  ), // #74  {zmm, zmm|m512|mem}
  ROW(2, 1, 1, 0, 53 , 51 , 0  , 0  , 0  , 0  ), //      {m512|mem, zmm}
  ROW(3, 1, 1, 0, 45 , 45 , 54 , 0  , 0  , 0  ), // #76  {xmm, xmm, xmm|m128|mem|i8|u8}
  ROW(3, 1, 1, 0, 45 , 47 , 10 , 0  , 0  , 0  ), //      {xmm, m128|mem, i8|u8}
  ROW(3, 1, 1, 0, 48 , 48 , 55 , 0  , 0  , 0  ), //      {ymm, ymm, ymm|m256|mem|i8|u8}
  ROW(3, 1, 1, 0, 48 , 50 , 10 , 0  , 0  , 0  ), //      {ymm, m256|mem, i8|u8}
  ROW(3, 1, 1, 0, 51 , 51 , 56 , 0  , 0  , 0  ), //      {zmm, zmm, zmm|m512|mem|i8|u8}
  ROW(3, 1, 1, 0, 51 , 53 , 10 , 0  , 0  , 0  ), //      {zmm, m512|mem, i8|u8}
  ROW(3, 1, 1, 0, 45 , 45 , 54 , 0  , 0  , 0  ), // #82  {xmm, xmm, i8|u8|xmm|m128|mem}
  ROW(3, 1, 1, 0, 48 , 48 , 54 , 0  , 0  , 0  ), //      {ymm, ymm, i8|u8|xmm|m128|mem}
  ROW(3, 1, 1, 0, 45 , 47 , 10 , 0  , 0  , 0  ), //      {xmm, m128|mem, i8|u8}
  ROW(3, 1, 1, 0, 48 , 50 , 10 , 0  , 0  , 0  ), //      {ymm, m256|mem, i8|u8}
  ROW(3, 1, 1, 0, 51 , 51 , 54 , 0  , 0  , 0  ), //      {zmm, zmm, xmm|m128|mem|i8|u8}
  ROW(3, 1, 1, 0, 51 , 53 , 10 , 0  , 0  , 0  ), //      {zmm, m512|mem, i8|u8}
  ROW(3, 1, 1, 0, 45 , 45 , 54 , 0  , 0  , 0  ), // #88  {xmm, xmm, xmm|m128|mem|i8|u8}
  ROW(3, 1, 1, 0, 45 , 47 , 10 , 0  , 0  , 0  ), //      {xmm, m128|mem, i8|u8}
  ROW(3, 1, 1, 0, 48 , 48 , 54 , 0  , 0  , 0  ), //      {ymm, ymm, xmm|m128|mem|i8|u8}
  ROW(3, 1, 1, 0, 48 , 50 , 10 , 0  , 0  , 0  ), //      {ymm, m256|mem, i8|u8}
  ROW(3, 1, 1, 0, 51 , 51 , 54 , 0  , 0  , 0  ), //      {zmm, zmm, xmm|m128|mem|i8|u8}
  ROW(3, 1, 1, 0, 51 , 53 , 10 , 0  , 0  , 0  ), //      {zmm, m512|mem, i8|u8}
  ROW(2, 1, 1, 0, 57 , 58 , 0  , 0  , 0  , 0  ), // #94  {mm, mm|m64|mem|r64}
  ROW(2, 1, 1, 0, 15 , 59 , 0  , 0  , 0  , 0  ), //      {m64|mem|r64, mm|xmm}
  ROW(2, 0, 1, 0, 45 , 15 , 0  , 0  , 0  , 0  ), //      {xmm, r64|m64|mem}
  ROW(2, 1, 1, 0, 45 , 60 , 0  , 0  , 0  , 0  ), // #97  {xmm, xmm|m64|mem}
  ROW(2, 1, 1, 0, 30 , 45 , 0  , 0  , 0  , 0  ), // #98  {m64|mem, xmm}
  ROW(0, 1, 1, 0, 0  , 0  , 0  , 0  , 0  , 0  ), // #99  {}
  ROW(1, 1, 1, 0, 61 , 0  , 0  , 0  , 0  , 0  ), //      {r16|m16|r32|m32|r64|m64}
  ROW(2, 1, 1, 0, 27 , 4  , 0  , 0  , 0  , 0  ), //      {r16|m16|mem, r16}
  ROW(2, 1, 1, 0, 28 , 6  , 0  , 0  , 0  , 0  ), //      {r32|m32|mem, r32}
  ROW(2, 1, 1, 0, 15 , 8  , 0  , 0  , 0  , 0  ), //      {r64|m64|mem, r64}
  ROW(3, 1, 1, 0, 45 , 62 , 45 , 0  , 0  , 0  ), // #104 {xmm, vm32x, xmm}
  ROW(3, 1, 1, 0, 48 , 62 , 48 , 0  , 0  , 0  ), //      {ymm, vm32x, ymm}
  ROW(2, 1, 1, 0, 45 , 62 , 0  , 0  , 0  , 0  ), //      {xmm, vm32x}
  ROW(2, 1, 1, 0, 48 , 63 , 0  , 0  , 0  , 0  ), //      {ymm, vm32y}
  ROW(2, 1, 1, 0, 51 , 64 , 0  , 0  , 0  , 0  ), //      {zmm, vm32z}
  ROW(3, 1, 1, 0, 45 , 62 , 45 , 0  , 0  , 0  ), // #109 {xmm, vm32x, xmm}
  ROW(3, 1, 1, 0, 48 , 63 , 48 , 0  , 0  , 0  ), //      {ymm, vm32y, ymm}
  ROW(2, 1, 1, 0, 45 , 62 , 0  , 0  , 0  , 0  ), //      {xmm, vm32x}
  ROW(2, 1, 1, 0, 48 , 63 , 0  , 0  , 0  , 0  ), //      {ymm, vm32y}
  ROW(2, 1, 1, 0, 51 , 64 , 0  , 0  , 0  , 0  ), //      {zmm, vm32z}
  ROW(3, 1, 1, 0, 45 , 65 , 45 , 0  , 0  , 0  ), // #114 {xmm, vm64x, xmm}
  ROW(3, 1, 1, 0, 48 , 66 , 48 , 0  , 0  , 0  ), //      {ymm, vm64y, ymm}
  ROW(2, 1, 1, 0, 45 , 65 , 0  , 0  , 0  , 0  ), //      {xmm, vm64x}
  ROW(2, 1, 1, 0, 48 , 66 , 0  , 0  , 0  , 0  ), //      {ymm, vm64y}
  ROW(2, 1, 1, 0, 51 , 67 , 0  , 0  , 0  , 0  ), //      {zmm, vm64z}
  ROW(2, 1, 1, 0, 25 , 10 , 0  , 0  , 0  , 0  ), // #119 {r16|m16|r32|m32|r64|m64|mem, i8|u8}
  ROW(2, 1, 1, 0, 27 , 4  , 0  , 0  , 0  , 0  ), //      {r16|m16|mem, r16}
  ROW(2, 1, 1, 0, 28 , 6  , 0  , 0  , 0  , 0  ), //      {r32|m32|mem, r32}
  ROW(2, 0, 1, 0, 15 , 8  , 0  , 0  , 0  , 0  ), //      {r64|m64|mem, r64}
  ROW(3, 1, 1, 1, 1  , 2  , 68 , 0  , 0  , 0  ), // #123 {r8lo|r8hi|m8|mem, r8lo|r8hi, <al>}
  ROW(3, 1, 1, 1, 27 , 4  , 33 , 0  , 0  , 0  ), //      {r16|m16|mem, r16, <ax>}
  ROW(3, 1, 1, 1, 28 , 6  , 36 , 0  , 0  , 0  ), //      {r32|m32|mem, r32, <eax>}
  ROW(3, 0, 1, 1, 15 , 8  , 38 , 0  , 0  , 0  ), //      {r64|m64|mem, r64, <rax>}
  ROW(1, 1, 1, 0, 69 , 0  , 0  , 0  , 0  , 0  ), // #127 {r16|m16|r64|m64}
  ROW(1, 1, 0, 0, 13 , 0  , 0  , 0  , 0  , 0  ), //      {r32|m32}
  ROW(1, 1, 0, 0, 70 , 0  , 0  , 0  , 0  , 0  ), //      {ds|es|ss}
  ROW(1, 1, 1, 0, 71 , 0  , 0  , 0  , 0  , 0  ), //      {fs|gs}
  ROW(1, 1, 1, 0, 72 , 0  , 0  , 0  , 0  , 0  ), // #131 {r16|m16|r64|m64|i8|i16|i32}
  ROW(1, 1, 0, 0, 73 , 0  , 0  , 0  , 0  , 0  ), //      {r32|m32|i32|u32}
  ROW(1, 1, 0, 0, 74 , 0  , 0  , 0  , 0  , 0  ), //      {cs|ss|ds|es}
  ROW(1, 1, 1, 0, 71 , 0  , 0  , 0  , 0  , 0  ), //      {fs|gs}
  ROW(4, 1, 1, 0, 45 , 45 , 45 , 46 , 0  , 0  ), // #135 {xmm, xmm, xmm, xmm|m128|mem}
  ROW(4, 1, 1, 0, 45 , 45 , 47 , 45 , 0  , 0  ), //      {xmm, xmm, m128|mem, xmm}
  ROW(4, 1, 1, 0, 48 , 48 , 48 , 49 , 0  , 0  ), //      {ymm, ymm, ymm, ymm|m256|mem}
  ROW(4, 1, 1, 0, 48 , 48 , 50 , 48 , 0  , 0  ), //      {ymm, ymm, m256|mem, ymm}
  ROW(3, 1, 1, 0, 45 , 75 , 45 , 0  , 0  , 0  ), // #139 {xmm, vm64x|vm64y, xmm}
  ROW(2, 1, 1, 0, 45 , 65 , 0  , 0  , 0  , 0  ), //      {xmm, vm64x}
  ROW(2, 1, 1, 0, 48 , 66 , 0  , 0  , 0  , 0  ), //      {ymm, vm64y}
  ROW(2, 1, 1, 0, 51 , 67 , 0  , 0  , 0  , 0  ), //      {zmm, vm64z}
  ROW(3, 1, 1, 0, 47 , 45 , 45 , 0  , 0  , 0  ), // #143 {m128|mem, xmm, xmm}
  ROW(3, 1, 1, 0, 50 , 48 , 48 , 0  , 0  , 0  ), //      {m256|mem, ymm, ymm}
  ROW(3, 1, 1, 0, 45 , 45 , 47 , 0  , 0  , 0  ), //      {xmm, xmm, m128|mem}
  ROW(3, 1, 1, 0, 48 , 48 , 50 , 0  , 0  , 0  ), //      {ymm, ymm, m256|mem}
  ROW(5, 1, 1, 0, 45 , 45 , 46 , 45 , 76 , 0  ), // #147 {xmm, xmm, xmm|m128|mem, xmm, i4|u4}
  ROW(5, 1, 1, 0, 45 , 45 , 45 , 47 , 76 , 0  ), //      {xmm, xmm, xmm, m128|mem, i4|u4}
  ROW(5, 1, 1, 0, 48 , 48 , 49 , 48 , 76 , 0  ), //      {ymm, ymm, ymm|m256|mem, ymm, i4|u4}
  ROW(5, 1, 1, 0, 48 , 48 , 48 , 50 , 76 , 0  ), //      {ymm, ymm, ymm, m256|mem, i4|u4}
  ROW(3, 1, 1, 0, 48 , 49 , 10 , 0  , 0  , 0  ), // #151 {ymm, ymm|m256|mem, i8|u8}
  ROW(3, 1, 1, 0, 48 , 48 , 49 , 0  , 0  , 0  ), //      {ymm, ymm, ymm|m256|mem}
  ROW(3, 1, 1, 0, 51 , 51 , 56 , 0  , 0  , 0  ), //      {zmm, zmm, zmm|m512|mem|i8|u8}
  ROW(3, 1, 1, 0, 51 , 53 , 10 , 0  , 0  , 0  ), //      {zmm, m512|mem, i8|u8}
  ROW(2, 1, 1, 0, 4  , 27 , 0  , 0  , 0  , 0  ), // #155 {r16, r16|m16|mem}
  ROW(2, 1, 1, 0, 6  , 28 , 0  , 0  , 0  , 0  ), // #156 {r32, r32|m32|mem}
  ROW(2, 0, 1, 0, 8  , 15 , 0  , 0  , 0  , 0  ), //      {r64, r64|m64|mem}
  ROW(1, 1, 1, 0, 77 , 0  , 0  , 0  , 0  , 0  ), // #158 {m32|m64}
  ROW(2, 1, 1, 0, 78 , 79 , 0  , 0  , 0  , 0  ), //      {st0, st}
  ROW(2, 1, 1, 0, 79 , 78 , 0  , 0  , 0  , 0  ), //      {st, st0}
  ROW(2, 1, 1, 0, 4  , 29 , 0  , 0  , 0  , 0  ), // #161 {r16, m32|mem}
  ROW(2, 1, 1, 0, 6  , 80 , 0  , 0  , 0  , 0  ), //      {r32, m48|mem}
  ROW(2, 0, 1, 0, 8  , 81 , 0  , 0  , 0  , 0  ), //      {r64, m80|mem}
  ROW(3, 1, 1, 0, 27 , 4  , 82 , 0  , 0  , 0  ), // #164 {r16|m16|mem, r16, cl|i8|u8}
  ROW(3, 1, 1, 0, 28 , 6  , 82 , 0  , 0  , 0  ), //      {r32|m32|mem, r32, cl|i8|u8}
  ROW(3, 0, 1, 0, 15 , 8  , 82 , 0  , 0  , 0  ), //      {r64|m64|mem, r64, cl|i8|u8}
  ROW(3, 1, 1, 0, 45 , 45 , 46 , 0  , 0  , 0  ), // #167 {xmm, xmm, xmm|m128|mem}
  ROW(3, 1, 1, 0, 48 , 48 , 49 , 0  , 0  , 0  ), // #168 {ymm, ymm, ymm|m256|mem}
  ROW(3, 1, 1, 0, 51 , 51 , 52 , 0  , 0  , 0  ), //      {zmm, zmm, zmm|m512|mem}
  ROW(4, 1, 1, 0, 45 , 45 , 46 , 10 , 0  , 0  ), // #170 {xmm, xmm, xmm|m128|mem, i8|u8}
  ROW(4, 1, 1, 0, 48 , 48 , 49 , 10 , 0  , 0  ), // #171 {ymm, ymm, ymm|m256|mem, i8|u8}
  ROW(4, 1, 1, 0, 51 , 51 , 52 , 10 , 0  , 0  ), //      {zmm, zmm, zmm|m512|mem, i8|u8}
  ROW(4, 1, 1, 0, 83 , 45 , 46 , 10 , 0  , 0  ), // #173 {xmm|k, xmm, xmm|m128|mem, i8|u8}
  ROW(4, 1, 1, 0, 84 , 48 , 49 , 10 , 0  , 0  ), //      {ymm|k, ymm, ymm|m256|mem, i8|u8}
  ROW(4, 1, 1, 0, 85 , 51 , 52 , 10 , 0  , 0  ), //      {k, zmm, zmm|m512|mem, i8|u8}
  ROW(2, 1, 1, 0, 46 , 45 , 0  , 0  , 0  , 0  ), // #176 {xmm|m128|mem, xmm}
  ROW(2, 1, 1, 0, 49 , 48 , 0  , 0  , 0  , 0  ), //      {ymm|m256|mem, ymm}
  ROW(2, 1, 1, 0, 52 , 51 , 0  , 0  , 0  , 0  ), //      {zmm|m512|mem, zmm}
  ROW(2, 1, 1, 0, 45 , 60 , 0  , 0  , 0  , 0  ), // #179 {xmm, xmm|m64|mem}
  ROW(2, 1, 1, 0, 48 , 46 , 0  , 0  , 0  , 0  ), //      {ymm, xmm|m128|mem}
  ROW(2, 1, 1, 0, 51 , 49 , 0  , 0  , 0  , 0  ), //      {zmm, ymm|m256|mem}
  ROW(2, 1, 1, 0, 45 , 46 , 0  , 0  , 0  , 0  ), // #182 {xmm, xmm|m128|mem}
  ROW(2, 1, 1, 0, 48 , 49 , 0  , 0  , 0  , 0  ), //      {ymm, ymm|m256|mem}
  ROW(2, 1, 1, 0, 51 , 52 , 0  , 0  , 0  , 0  ), //      {zmm, zmm|m512|mem}
  ROW(3, 1, 1, 0, 60 , 45 , 10 , 0  , 0  , 0  ), // #185 {xmm|m64|mem, xmm, i8|u8}
  ROW(3, 1, 1, 0, 46 , 48 , 10 , 0  , 0  , 0  ), // #186 {xmm|m128|mem, ymm, i8|u8}
  ROW(3, 1, 1, 0, 49 , 51 , 10 , 0  , 0  , 0  ), // #187 {ymm|m256|mem, zmm, i8|u8}
  ROW(3, 1, 1, 0, 45 , 46 , 10 , 0  , 0  , 0  ), // #188 {xmm, xmm|m128|mem, i8|u8}
  ROW(3, 1, 1, 0, 48 , 49 , 10 , 0  , 0  , 0  ), //      {ymm, ymm|m256|mem, i8|u8}
  ROW(3, 1, 1, 0, 51 , 52 , 10 , 0  , 0  , 0  ), //      {zmm, zmm|m512|mem, i8|u8}
  ROW(2, 1, 1, 0, 45 , 60 , 0  , 0  , 0  , 0  ), // #191 {xmm, xmm|m64|mem}
  ROW(2, 1, 1, 0, 48 , 49 , 0  , 0  , 0  , 0  ), //      {ymm, ymm|m256|mem}
  ROW(2, 1, 1, 0, 51 , 52 , 0  , 0  , 0  , 0  ), //      {zmm, zmm|m512|mem}
  ROW(2, 1, 1, 0, 47 , 45 , 0  , 0  , 0  , 0  ), // #194 {m128|mem, xmm}
  ROW(2, 1, 1, 0, 50 , 48 , 0  , 0  , 0  , 0  ), //      {m256|mem, ymm}
  ROW(2, 1, 1, 0, 53 , 51 , 0  , 0  , 0  , 0  ), //      {m512|mem, zmm}
  ROW(2, 1, 1, 0, 45 , 47 , 0  , 0  , 0  , 0  ), // #197 {xmm, m128|mem}
  ROW(2, 1, 1, 0, 48 , 50 , 0  , 0  , 0  , 0  ), //      {ymm, m256|mem}
  ROW(2, 1, 1, 0, 51 , 53 , 0  , 0  , 0  , 0  ), //      {zmm, m512|mem}
  ROW(2, 0, 1, 0, 15 , 45 , 0  , 0  , 0  , 0  ), // #200 {r64|m64|mem, xmm}
  ROW(2, 1, 1, 0, 45 , 86 , 0  , 0  , 0  , 0  ), //      {xmm, xmm|m64|mem|r64}
  ROW(2, 1, 1, 0, 30 , 45 , 0  , 0  , 0  , 0  ), //      {m64|mem, xmm}
  ROW(2, 1, 1, 0, 30 , 45 , 0  , 0  , 0  , 0  ), // #203 {m64|mem, xmm}
  ROW(2, 1, 1, 0, 45 , 30 , 0  , 0  , 0  , 0  ), //      {xmm, m64|mem}
  ROW(3, 1, 1, 0, 45 , 45 , 45 , 0  , 0  , 0  ), // #205 {xmm, xmm, xmm}
  ROW(2, 1, 1, 0, 29 , 45 , 0  , 0  , 0  , 0  ), // #206 {m32|mem, xmm}
  ROW(2, 1, 1, 0, 45 , 29 , 0  , 0  , 0  , 0  ), //      {xmm, m32|mem}
  ROW(3, 1, 1, 0, 45 , 45 , 45 , 0  , 0  , 0  ), //      {xmm, xmm, xmm}
  ROW(4, 1, 1, 0, 85 , 85 , 45 , 46 , 0  , 0  ), // #209 {k, k, xmm, xmm|m128|mem}
  ROW(4, 1, 1, 0, 85 , 85 , 48 , 49 , 0  , 0  ), //      {k, k, ymm, ymm|m256|mem}
  ROW(4, 1, 1, 0, 85 , 85 , 51 , 52 , 0  , 0  ), //      {k, k, zmm, zmm|m512|mem}
  ROW(2, 1, 1, 0, 87 , 86 , 0  , 0  , 0  , 0  ), // #212 {xmm|ymm, xmm|m64|mem|r64}
  ROW(2, 0, 1, 0, 51 , 8  , 0  , 0  , 0  , 0  ), //      {zmm, r64}
  ROW(2, 1, 1, 0, 51 , 60 , 0  , 0  , 0  , 0  ), //      {zmm, xmm|m64|mem}
  ROW(4, 1, 1, 0, 85 , 45 , 46 , 10 , 0  , 0  ), // #215 {k, xmm, xmm|m128|mem, i8|u8}
  ROW(4, 1, 1, 0, 85 , 48 , 49 , 10 , 0  , 0  ), //      {k, ymm, ymm|m256|mem, i8|u8}
  ROW(4, 1, 1, 0, 85 , 51 , 52 , 10 , 0  , 0  ), //      {k, zmm, zmm|m512|mem, i8|u8}
  ROW(3, 1, 1, 0, 83 , 45 , 46 , 0  , 0  , 0  ), // #218 {xmm|k, xmm, xmm|m128|mem}
  ROW(3, 1, 1, 0, 84 , 48 , 49 , 0  , 0  , 0  ), //      {ymm|k, ymm, ymm|m256|mem}
  ROW(3, 1, 1, 0, 85 , 51 , 52 , 0  , 0  , 0  ), //      {k, zmm, zmm|m512|mem}
  ROW(2, 1, 1, 0, 88 , 45 , 0  , 0  , 0  , 0  ), // #221 {xmm|m32|mem, xmm}
  ROW(2, 1, 1, 0, 60 , 48 , 0  , 0  , 0  , 0  ), //      {xmm|m64|mem, ymm}
  ROW(2, 1, 1, 0, 46 , 51 , 0  , 0  , 0  , 0  ), //      {xmm|m128|mem, zmm}
  ROW(2, 1, 1, 0, 60 , 45 , 0  , 0  , 0  , 0  ), // #224 {xmm|m64|mem, xmm}
  ROW(2, 1, 1, 0, 46 , 48 , 0  , 0  , 0  , 0  ), //      {xmm|m128|mem, ymm}
  ROW(2, 1, 1, 0, 49 , 51 , 0  , 0  , 0  , 0  ), //      {ymm|m256|mem, zmm}
  ROW(2, 1, 1, 0, 89 , 45 , 0  , 0  , 0  , 0  ), // #227 {xmm|m16|mem, xmm}
  ROW(2, 1, 1, 0, 88 , 48 , 0  , 0  , 0  , 0  ), //      {xmm|m32|mem, ymm}
  ROW(2, 1, 1, 0, 60 , 51 , 0  , 0  , 0  , 0  ), //      {xmm|m64|mem, zmm}
  ROW(2, 1, 1, 0, 45 , 88 , 0  , 0  , 0  , 0  ), // #230 {xmm, xmm|m32|mem}
  ROW(2, 1, 1, 0, 48 , 60 , 0  , 0  , 0  , 0  ), //      {ymm, xmm|m64|mem}
  ROW(2, 1, 1, 0, 51 , 46 , 0  , 0  , 0  , 0  ), //      {zmm, xmm|m128|mem}
  ROW(2, 1, 1, 0, 45 , 89 , 0  , 0  , 0  , 0  ), // #233 {xmm, xmm|m16|mem}
  ROW(2, 1, 1, 0, 48 , 88 , 0  , 0  , 0  , 0  ), //      {ymm, xmm|m32|mem}
  ROW(2, 1, 1, 0, 51 , 60 , 0  , 0  , 0  , 0  ), //      {zmm, xmm|m64|mem}
  ROW(2, 1, 1, 0, 62 , 45 , 0  , 0  , 0  , 0  ), // #236 {vm32x, xmm}
  ROW(2, 1, 1, 0, 63 , 48 , 0  , 0  , 0  , 0  ), //      {vm32y, ymm}
  ROW(2, 1, 1, 0, 64 , 51 , 0  , 0  , 0  , 0  ), //      {vm32z, zmm}
  ROW(2, 1, 1, 0, 65 , 45 , 0  , 0  , 0  , 0  ), // #239 {vm64x, xmm}
  ROW(2, 1, 1, 0, 66 , 48 , 0  , 0  , 0  , 0  ), //      {vm64y, ymm}
  ROW(2, 1, 1, 0, 67 , 51 , 0  , 0  , 0  , 0  ), //      {vm64z, zmm}
  ROW(3, 1, 1, 0, 85 , 45 , 46 , 0  , 0  , 0  ), // #242 {k, xmm, xmm|m128|mem}
  ROW(3, 1, 1, 0, 85 , 48 , 49 , 0  , 0  , 0  ), //      {k, ymm, ymm|m256|mem}
  ROW(3, 1, 1, 0, 85 , 51 , 52 , 0  , 0  , 0  ), //      {k, zmm, zmm|m512|mem}
  ROW(3, 1, 1, 0, 6  , 6  , 28 , 0  , 0  , 0  ), // #245 {r32, r32, r32|m32|mem}
  ROW(3, 0, 1, 0, 8  , 8  , 15 , 0  , 0  , 0  ), //      {r64, r64, r64|m64|mem}
  ROW(3, 1, 1, 0, 6  , 28 , 6  , 0  , 0  , 0  ), // #247 {r32, r32|m32|mem, r32}
  ROW(3, 0, 1, 0, 8  , 15 , 8  , 0  , 0  , 0  ), //      {r64, r64|m64|mem, r64}
  ROW(2, 1, 0, 0, 90 , 28 , 0  , 0  , 0  , 0  ), // #249 {bnd, r32|m32|mem}
  ROW(2, 0, 1, 0, 90 , 15 , 0  , 0  , 0  , 0  ), //      {bnd, r64|m64|mem}
  ROW(2, 1, 1, 0, 90 , 91 , 0  , 0  , 0  , 0  ), // #251 {bnd, bnd|mem}
  ROW(2, 1, 1, 0, 92 , 90 , 0  , 0  , 0  , 0  ), //      {mem, bnd}
  ROW(2, 1, 0, 0, 4  , 29 , 0  , 0  , 0  , 0  ), // #253 {r16, m32|mem}
  ROW(2, 1, 0, 0, 6  , 30 , 0  , 0  , 0  , 0  ), //      {r32, m64|mem}
  ROW(1, 1, 0, 0, 93 , 0  , 0  , 0  , 0  , 0  ), // #255 {rel16|r16|m16|r32|m32}
  ROW(1, 1, 1, 0, 94 , 0  , 0  , 0  , 0  , 0  ), //      {rel32|r64|m64|mem}
  ROW(2, 1, 1, 0, 6  , 95 , 0  , 0  , 0  , 0  ), // #257 {r32, r8lo|r8hi|m8|r16|m16|r32|m32}
  ROW(2, 0, 1, 0, 8  , 96 , 0  , 0  , 0  , 0  ), //      {r64, r8lo|r8hi|m8|r64|m64}
  ROW(1, 1, 0, 0, 97 , 0  , 0  , 0  , 0  , 0  ), // #259 {r16|r32}
  ROW(1, 1, 1, 0, 31 , 0  , 0  , 0  , 0  , 0  ), // #260 {r8lo|r8hi|m8|r16|m16|r32|m32|r64|m64|mem}
  ROW(2, 1, 0, 0, 98 , 53 , 0  , 0  , 0  , 0  ), // #261 {es:[memBase], m512|mem}
  ROW(2, 0, 1, 0, 98 , 53 , 0  , 0  , 0  , 0  ), //      {es:[memBase], m512|mem}
  ROW(3, 1, 1, 0, 45 , 10 , 10 , 0  , 0  , 0  ), // #263 {xmm, i8|u8, i8|u8}
  ROW(2, 1, 1, 0, 45 , 45 , 0  , 0  , 0  , 0  ), // #264 {xmm, xmm}
  ROW(0, 1, 1, 0, 0  , 0  , 0  , 0  , 0  , 0  ), // #265 {}
  ROW(1, 1, 1, 0, 79 , 0  , 0  , 0  , 0  , 0  ), // #266 {st}
  ROW(0, 1, 1, 0, 0  , 0  , 0  , 0  , 0  , 0  ), // #267 {}
  ROW(1, 1, 1, 0, 99 , 0  , 0  , 0  , 0  , 0  ), // #268 {m32|m64|st}
  ROW(2, 1, 1, 0, 45 , 45 , 0  , 0  , 0  , 0  ), // #269 {xmm, xmm}
  ROW(4, 1, 1, 0, 45 , 45 , 10 , 10 , 0  , 0  ), //      {xmm, xmm, i8|u8, i8|u8}
  ROW(2, 1, 0, 0, 6  , 47 , 0  , 0  , 0  , 0  ), // #271 {r32, m128|mem}
  ROW(2, 0, 1, 0, 8  , 47 , 0  , 0  , 0  , 0  ), //      {r64, m128|mem}
  ROW(2, 1, 0, 2, 36 , 100, 0  , 0  , 0  , 0  ), // #273 {<eax>, <ecx>}
  ROW(2, 0, 1, 2, 101, 100, 0  , 0  , 0  , 0  ), //      {<eax|rax>, <ecx>}
  ROW(1, 1, 1, 0, 102, 0  , 0  , 0  , 0  , 0  ), // #275 {rel8|rel32}
  ROW(1, 1, 0, 0, 103, 0  , 0  , 0  , 0  , 0  ), //      {rel16}
  ROW(2, 1, 0, 1, 104, 105, 0  , 0  , 0  , 0  ), // #277 {<cx|ecx>, rel8}
  ROW(2, 0, 1, 1, 106, 105, 0  , 0  , 0  , 0  ), //      {<ecx|rcx>, rel8}
  ROW(1, 1, 1, 0, 107, 0  , 0  , 0  , 0  , 0  ), // #279 {rel8|rel32|r64|m64|mem}
  ROW(1, 1, 0, 0, 108, 0  , 0  , 0  , 0  , 0  ), //      {rel16|r32|m32|mem}
  ROW(2, 1, 1, 0, 85 , 109, 0  , 0  , 0  , 0  ), // #281 {k, k|m8|mem|r32|r8lo|r8hi|r16}
  ROW(2, 1, 1, 0, 110, 85 , 0  , 0  , 0  , 0  ), //      {m8|mem|r32|r8lo|r8hi|r16, k}
  ROW(2, 1, 1, 0, 85 , 111, 0  , 0  , 0  , 0  ), // #283 {k, k|m32|mem|r32}
  ROW(2, 1, 1, 0, 28 , 85 , 0  , 0  , 0  , 0  ), //      {m32|mem|r32, k}
  ROW(2, 1, 1, 0, 85 , 112, 0  , 0  , 0  , 0  ), // #285 {k, k|m64|mem|r64}
  ROW(2, 1, 1, 0, 15 , 85 , 0  , 0  , 0  , 0  ), //      {m64|mem|r64, k}
  ROW(2, 1, 1, 0, 85 , 113, 0  , 0  , 0  , 0  ), // #287 {k, k|m16|mem|r32|r16}
  ROW(2, 1, 1, 0, 114, 85 , 0  , 0  , 0  , 0  ), //      {m16|mem|r32|r16, k}
  ROW(2, 1, 1, 0, 4  , 27 , 0  , 0  , 0  , 0  ), // #289 {r16, r16|m16|mem}
  ROW(2, 1, 1, 0, 6  , 114, 0  , 0  , 0  , 0  ), //      {r32, r32|m16|mem|r16}
  ROW(2, 1, 0, 0, 4  , 29 , 0  , 0  , 0  , 0  ), // #291 {r16, m32|mem}
  ROW(2, 1, 0, 0, 6  , 80 , 0  , 0  , 0  , 0  ), //      {r32, m48|mem}
  ROW(2, 1, 1, 0, 4  , 27 , 0  , 0  , 0  , 0  ), // #293 {r16, r16|m16|mem}
  ROW(2, 1, 1, 0, 115, 114, 0  , 0  , 0  , 0  ), //      {r32|r64, r32|m16|mem|r16}
  ROW(2, 1, 1, 0, 59 , 28 , 0  , 0  , 0  , 0  ), // #295 {mm|xmm, r32|m32|mem}
  ROW(2, 1, 1, 0, 28 , 59 , 0  , 0  , 0  , 0  ), //      {r32|m32|mem, mm|xmm}
  ROW(2, 1, 1, 0, 45 , 88 , 0  , 0  , 0  , 0  ), // #297 {xmm, xmm|m32|mem}
  ROW(2, 1, 1, 0, 29 , 45 , 0  , 0  , 0  , 0  ), //      {m32|mem, xmm}
  ROW(2, 1, 1, 0, 4  , 9  , 0  , 0  , 0  , 0  ), // #299 {r16, r8lo|r8hi|m8}
  ROW(2, 1, 1, 0, 115, 116, 0  , 0  , 0  , 0  ), //      {r32|r64, r8lo|r8hi|m8|r16|m16}
  ROW(2, 0, 1, 0, 4  , 27 , 0  , 0  , 0  , 0  ), // #301 {r16, r16|m16|mem}
  ROW(2, 0, 1, 0, 115, 28 , 0  , 0  , 0  , 0  ), //      {r32|r64, r32|m32|mem}
  ROW(4, 1, 1, 1, 6  , 6  , 28 , 35 , 0  , 0  ), // #303 {r32, r32, r32|m32|mem, <edx>}
  ROW(4, 0, 1, 1, 8  , 8  , 15 , 37 , 0  , 0  ), //      {r64, r64, r64|m64|mem, <rdx>}
  ROW(2, 1, 1, 0, 57 , 117, 0  , 0  , 0  , 0  ), // #305 {mm, mm|m64|mem}
  ROW(2, 1, 1, 0, 45 , 46 , 0  , 0  , 0  , 0  ), //      {xmm, xmm|m128|mem}
  ROW(3, 1, 1, 0, 57 , 117, 10 , 0  , 0  , 0  ), // #307 {mm, mm|m64|mem, i8|u8}
  ROW(3, 1, 1, 0, 45 , 46 , 10 , 0  , 0  , 0  ), //      {xmm, xmm|m128|mem, i8|u8}
  ROW(3, 1, 1, 0, 6  , 59 , 10 , 0  , 0  , 0  ), // #309 {r32, mm|xmm, i8|u8}
  ROW(3, 1, 1, 0, 21 , 45 , 10 , 0  , 0  , 0  ), //      {m16|mem, xmm, i8|u8}
  ROW(2, 1, 1, 0, 57 , 118, 0  , 0  , 0  , 0  ), // #311 {mm, i8|u8|mm|m64|mem}
  ROW(2, 1, 1, 0, 45 , 54 , 0  , 0  , 0  , 0  ), //      {xmm, i8|u8|xmm|m128|mem}
  ROW(1, 1, 0, 0, 6  , 0  , 0  , 0  , 0  , 0  ), // #313 {r32}
  ROW(1, 0, 1, 0, 8  , 0  , 0  , 0  , 0  , 0  ), // #314 {r64}
  ROW(0, 1, 1, 0, 0  , 0  , 0  , 0  , 0  , 0  ), // #315 {}
  ROW(1, 1, 1, 0, 119, 0  , 0  , 0  , 0  , 0  ), //      {u16}
  ROW(3, 1, 1, 0, 6  , 28 , 10 , 0  , 0  , 0  ), // #317 {r32, r32|m32|mem, i8|u8}
  ROW(3, 0, 1, 0, 8  , 15 , 10 , 0  , 0  , 0  ), //      {r64, r64|m64|mem, i8|u8}
  ROW(4, 1, 1, 0, 45 , 45 , 46 , 45 , 0  , 0  ), // #319 {xmm, xmm, xmm|m128|mem, xmm}
  ROW(4, 1, 1, 0, 48 , 48 , 49 , 48 , 0  , 0  ), //      {ymm, ymm, ymm|m256|mem, ymm}
  ROW(2, 1, 1, 0, 45 , 120, 0  , 0  , 0  , 0  ), // #321 {xmm, xmm|m128|ymm|m256}
  ROW(2, 1, 1, 0, 48 , 52 , 0  , 0  , 0  , 0  ), //      {ymm, zmm|m512|mem}
  ROW(4, 1, 1, 0, 45 , 45 , 45 , 60 , 0  , 0  ), // #323 {xmm, xmm, xmm, xmm|m64|mem}
  ROW(4, 1, 1, 0, 45 , 45 , 30 , 45 , 0  , 0  ), //      {xmm, xmm, m64|mem, xmm}
  ROW(4, 1, 1, 0, 45 , 45 , 45 , 88 , 0  , 0  ), // #325 {xmm, xmm, xmm, xmm|m32|mem}
  ROW(4, 1, 1, 0, 45 , 45 , 29 , 45 , 0  , 0  ), //      {xmm, xmm, m32|mem, xmm}
  ROW(4, 1, 1, 0, 48 , 48 , 46 , 10 , 0  , 0  ), // #327 {ymm, ymm, xmm|m128|mem, i8|u8}
  ROW(4, 1, 1, 0, 51 , 51 , 46 , 10 , 0  , 0  ), //      {zmm, zmm, xmm|m128|mem, i8|u8}
  ROW(1, 1, 0, 1, 36 , 0  , 0  , 0  , 0  , 0  ), // #329 {<eax>}
  ROW(1, 0, 1, 1, 38 , 0  , 0  , 0  , 0  , 0  ), // #330 {<rax>}
  ROW(2, 1, 1, 0, 28 , 45 , 0  , 0  , 0  , 0  ), // #331 {r32|m32|mem, xmm}
  ROW(2, 1, 1, 0, 45 , 28 , 0  , 0  , 0  , 0  ), //      {xmm, r32|m32|mem}
  ROW(2, 1, 1, 0, 30 , 45 , 0  , 0  , 0  , 0  ), // #333 {m64|mem, xmm}
  ROW(3, 1, 1, 0, 45 , 45 , 30 , 0  , 0  , 0  ), //      {xmm, xmm, m64|mem}
  ROW(2, 1, 0, 0, 28 , 6  , 0  , 0  , 0  , 0  ), // #335 {r32|m32|mem, r32}
  ROW(2, 0, 1, 0, 15 , 8  , 0  , 0  , 0  , 0  ), //      {r64|m64|mem, r64}
  ROW(2, 1, 0, 0, 6  , 28 , 0  , 0  , 0  , 0  ), // #337 {r32, r32|m32|mem}
  ROW(2, 0, 1, 0, 8  , 15 , 0  , 0  , 0  , 0  ), //      {r64, r64|m64|mem}
  ROW(3, 1, 1, 0, 45 , 45 , 54 , 0  , 0  , 0  ), // #339 {xmm, xmm, xmm|m128|mem|i8|u8}
  ROW(3, 1, 1, 0, 45 , 47 , 121, 0  , 0  , 0  ), //      {xmm, m128|mem, i8|u8|xmm}
  ROW(2, 1, 1, 0, 75 , 45 , 0  , 0  , 0  , 0  ), // #341 {vm64x|vm64y, xmm}
  ROW(2, 1, 1, 0, 67 , 48 , 0  , 0  , 0  , 0  ), //      {vm64z, ymm}
  ROW(3, 1, 1, 0, 45 , 45 , 46 , 0  , 0  , 0  ), // #343 {xmm, xmm, xmm|m128|mem}
  ROW(3, 1, 1, 0, 45 , 47 , 45 , 0  , 0  , 0  ), //      {xmm, m128|mem, xmm}
  ROW(2, 1, 1, 0, 62 , 87 , 0  , 0  , 0  , 0  ), // #345 {vm32x, xmm|ymm}
  ROW(2, 1, 1, 0, 63 , 51 , 0  , 0  , 0  , 0  ), //      {vm32y, zmm}
  ROW(1, 1, 0, 1, 33 , 0  , 0  , 0  , 0  , 0  ), // #347 {<ax>}
  ROW(2, 1, 0, 1, 33 , 10 , 0  , 0  , 0  , 0  ), // #348 {<ax>, i8|u8}
  ROW(2, 1, 0, 0, 27 , 4  , 0  , 0  , 0  , 0  ), // #349 {r16|m16|mem, r16}
  ROW(3, 1, 1, 1, 45 , 46 , 122, 0  , 0  , 0  ), // #350 {xmm, xmm|m128|mem, <xmm0>}
  ROW(2, 1, 1, 0, 90 , 123, 0  , 0  , 0  , 0  ), // #351 {bnd, mib}
  ROW(2, 1, 1, 0, 90 , 92 , 0  , 0  , 0  , 0  ), // #352 {bnd, mem}
  ROW(2, 1, 1, 0, 123, 90 , 0  , 0  , 0  , 0  ), // #353 {mib, bnd}
  ROW(1, 1, 1, 0, 124, 0  , 0  , 0  , 0  , 0  ), // #354 {r16|r32|r64}
  ROW(1, 1, 1, 1, 33 , 0  , 0  , 0  , 0  , 0  ), // #355 {<ax>}
  ROW(2, 1, 1, 2, 35 , 36 , 0  , 0  , 0  , 0  ), // #356 {<edx>, <eax>}
  ROW(1, 1, 1, 0, 92 , 0  , 0  , 0  , 0  , 0  ), // #357 {mem}
  ROW(1, 1, 1, 0, 30 , 0  , 0  , 0  , 0  , 0  ), // #358 {m64|mem}
  ROW(1, 1, 1, 1, 125, 0  , 0  , 0  , 0  , 0  ), // #359 {<ds:[memBase|zax]>}
  ROW(2, 1, 1, 2, 126, 127, 0  , 0  , 0  , 0  ), // #360 {<ds:[memBase|zsi]>, <es:[memBase|zdi]>}
  ROW(3, 1, 1, 0, 45 , 60 , 10 , 0  , 0  , 0  ), // #361 {xmm, xmm|m64|mem, i8|u8}
  ROW(3, 1, 1, 0, 45 , 88 , 10 , 0  , 0  , 0  ), // #362 {xmm, xmm|m32|mem, i8|u8}
  ROW(5, 0, 1, 4, 47 , 37 , 38 , 128, 129, 0  ), // #363 {m128|mem, <rdx>, <rax>, <rcx>, <rbx>}
  ROW(5, 1, 1, 4, 30 , 35 , 36 , 100, 130, 0  ), // #364 {m64|mem, <edx>, <eax>, <ecx>, <ebx>}
  ROW(4, 1, 1, 4, 36 , 130, 100, 35 , 0  , 0  ), // #365 {<eax>, <ebx>, <ecx>, <edx>}
  ROW(2, 0, 1, 2, 37 , 38 , 0  , 0  , 0  , 0  ), // #366 {<rdx>, <rax>}
  ROW(2, 1, 1, 0, 57 , 46 , 0  , 0  , 0  , 0  ), // #367 {mm, xmm|m128|mem}
  ROW(2, 1, 1, 0, 45 , 117, 0  , 0  , 0  , 0  ), // #368 {xmm, mm|m64|mem}
  ROW(2, 1, 1, 0, 57 , 60 , 0  , 0  , 0  , 0  ), // #369 {mm, xmm|m64|mem}
  ROW(2, 1, 1, 0, 115, 60 , 0  , 0  , 0  , 0  ), // #370 {r32|r64, xmm|m64|mem}
  ROW(2, 1, 1, 0, 45 , 131, 0  , 0  , 0  , 0  ), // #371 {xmm, r32|m32|mem|r64|m64}
  ROW(2, 1, 1, 0, 115, 88 , 0  , 0  , 0  , 0  ), // #372 {r32|r64, xmm|m32|mem}
  ROW(2, 1, 1, 2, 34 , 33 , 0  , 0  , 0  , 0  ), // #373 {<dx>, <ax>}
  ROW(1, 1, 1, 1, 36 , 0  , 0  , 0  , 0  , 0  ), // #374 {<eax>}
  ROW(2, 1, 1, 0, 12 , 10 , 0  , 0  , 0  , 0  ), // #375 {i16|u16, i8|u8}
  ROW(3, 1, 1, 0, 28 , 45 , 10 , 0  , 0  , 0  ), // #376 {r32|m32|mem, xmm, i8|u8}
  ROW(1, 1, 1, 0, 81 , 0  , 0  , 0  , 0  , 0  ), // #377 {m80|mem}
  ROW(1, 1, 1, 0, 132, 0  , 0  , 0  , 0  , 0  ), // #378 {m16|m32}
  ROW(1, 1, 1, 0, 133, 0  , 0  , 0  , 0  , 0  ), // #379 {m16|m32|m64}
  ROW(1, 1, 1, 0, 134, 0  , 0  , 0  , 0  , 0  ), // #380 {m32|m64|m80|st}
  ROW(1, 1, 1, 0, 21 , 0  , 0  , 0  , 0  , 0  ), // #381 {m16|mem}
  ROW(1, 1, 1, 0, 135, 0  , 0  , 0  , 0  , 0  ), // #382 {ax|m16|mem}
  ROW(1, 0, 1, 0, 92 , 0  , 0  , 0  , 0  , 0  ), // #383 {mem}
  ROW(2, 1, 1, 0, 136, 137, 0  , 0  , 0  , 0  ), // #384 {al|ax|eax, i8|u8|dx}
  ROW(1, 1, 1, 0, 6  , 0  , 0  , 0  , 0  , 0  ), // #385 {r32}
  ROW(2, 1, 1, 0, 138, 139, 0  , 0  , 0  , 0  ), // #386 {es:[memBase|zdi], dx}
  ROW(1, 1, 1, 0, 10 , 0  , 0  , 0  , 0  , 0  ), // #387 {i8|u8}
  ROW(0, 1, 0, 0, 0  , 0  , 0  , 0  , 0  , 0  ), // #388 {}
  ROW(0, 0, 1, 0, 0  , 0  , 0  , 0  , 0  , 0  ), // #389 {}
  ROW(3, 1, 1, 0, 85 , 85 , 85 , 0  , 0  , 0  ), // #390 {k, k, k}
  ROW(2, 1, 1, 0, 85 , 85 , 0  , 0  , 0  , 0  ), // #391 {k, k}
  ROW(3, 1, 1, 0, 85 , 85 , 10 , 0  , 0  , 0  ), // #392 {k, k, i8|u8}
  ROW(1, 1, 1, 1, 140, 0  , 0  , 0  , 0  , 0  ), // #393 {<ah>}
  ROW(1, 1, 1, 0, 29 , 0  , 0  , 0  , 0  , 0  ), // #394 {m32|mem}
  ROW(1, 0, 1, 0, 53 , 0  , 0  , 0  , 0  , 0  ), // #395 {m512|mem}
  ROW(2, 1, 1, 0, 124, 141, 0  , 0  , 0  , 0  ), // #396 {r16|r32|r64, mem|m8|m16|m32|m48|m64|m80|m128|m256|m512|m1024}
  ROW(1, 1, 1, 0, 27 , 0  , 0  , 0  , 0  , 0  ), // #397 {r16|m16|mem}
  ROW(1, 1, 1, 0, 115, 0  , 0  , 0  , 0  , 0  ), // #398 {r32|r64}
  ROW(2, 1, 1, 2, 142, 126, 0  , 0  , 0  , 0  ), // #399 {<al|ax|eax|rax>, <ds:[memBase|zsi]>}
  ROW(3, 1, 1, 0, 115, 28 , 14 , 0  , 0  , 0  ), // #400 {r32|r64, r32|m32|mem, i32|u32}
  ROW(3, 1, 1, 1, 45 , 45 , 143, 0  , 0  , 0  ), // #401 {xmm, xmm, <ds:[memBase|zdi]>}
  ROW(3, 1, 1, 1, 57 , 57 , 143, 0  , 0  , 0  ), // #402 {mm, mm, <ds:[memBase|zdi]>}
  ROW(3, 1, 1, 3, 125, 100, 35 , 0  , 0  , 0  ), // #403 {<ds:[memBase|zax]>, <ecx>, <edx>}
  ROW(2, 1, 1, 0, 98 , 53 , 0  , 0  , 0  , 0  ), // #404 {es:[memBase], m512|mem}
  ROW(2, 1, 1, 0, 57 , 45 , 0  , 0  , 0  , 0  ), // #405 {mm, xmm}
  ROW(2, 1, 1, 0, 6  , 45 , 0  , 0  , 0  , 0  ), // #406 {r32, xmm}
  ROW(2, 1, 1, 0, 30 , 57 , 0  , 0  , 0  , 0  ), // #407 {m64|mem, mm}
  ROW(2, 1, 1, 0, 45 , 57 , 0  , 0  , 0  , 0  ), // #408 {xmm, mm}
  ROW(2, 1, 1, 2, 127, 126, 0  , 0  , 0  , 0  ), // #409 {<es:[memBase|zdi]>, <ds:[memBase|zsi]>}
  ROW(2, 1, 1, 2, 36 , 100, 0  , 0  , 0  , 0  ), // #410 {<eax>, <ecx>}
  ROW(3, 1, 1, 3, 36 , 100, 130, 0  , 0  , 0  ), // #411 {<eax>, <ecx>, <ebx>}
  ROW(2, 1, 1, 0, 144, 136, 0  , 0  , 0  , 0  ), // #412 {u8|dx, al|ax|eax}
  ROW(2, 1, 1, 0, 139, 145, 0  , 0  , 0  , 0  ), // #413 {dx, ds:[memBase|zsi]}
  ROW(6, 1, 1, 3, 45 , 46 , 10 , 100, 36 , 35 ), // #414 {xmm, xmm|m128|mem, i8|u8, <ecx>, <eax>, <edx>}
  ROW(6, 1, 1, 3, 45 , 46 , 10 , 122, 36 , 35 ), // #415 {xmm, xmm|m128|mem, i8|u8, <xmm0>, <eax>, <edx>}
  ROW(4, 1, 1, 1, 45 , 46 , 10 , 100, 0  , 0  ), // #416 {xmm, xmm|m128|mem, i8|u8, <ecx>}
  ROW(4, 1, 1, 1, 45 , 46 , 10 , 122, 0  , 0  ), // #417 {xmm, xmm|m128|mem, i8|u8, <xmm0>}
  ROW(3, 1, 1, 0, 110, 45 , 10 , 0  , 0  , 0  ), // #418 {r32|m8|mem|r8lo|r8hi|r16, xmm, i8|u8}
  ROW(3, 0, 1, 0, 15 , 45 , 10 , 0  , 0  , 0  ), // #419 {r64|m64|mem, xmm, i8|u8}
  ROW(3, 1, 1, 0, 45 , 110, 10 , 0  , 0  , 0  ), // #420 {xmm, r32|m8|mem|r8lo|r8hi|r16, i8|u8}
  ROW(3, 1, 1, 0, 45 , 28 , 10 , 0  , 0  , 0  ), // #421 {xmm, r32|m32|mem, i8|u8}
  ROW(3, 0, 1, 0, 45 , 15 , 10 , 0  , 0  , 0  ), // #422 {xmm, r64|m64|mem, i8|u8}
  ROW(3, 1, 1, 0, 59 , 114, 10 , 0  , 0  , 0  ), // #423 {mm|xmm, r32|m16|mem|r16, i8|u8}
  ROW(2, 1, 1, 0, 6  , 59 , 0  , 0  , 0  , 0  ), // #424 {r32, mm|xmm}
  ROW(2, 1, 1, 0, 45 , 10 , 0  , 0  , 0  , 0  ), // #425 {xmm, i8|u8}
  ROW(1, 1, 1, 0, 131, 0  , 0  , 0  , 0  , 0  ), // #426 {r32|m32|mem|r64|m64}
  ROW(2, 1, 1, 0, 31 , 82 , 0  , 0  , 0  , 0  ), // #427 {r8lo|r8hi|m8|r16|m16|r32|m32|r64|m64|mem, cl|i8|u8}
  ROW(1, 0, 1, 0, 115, 0  , 0  , 0  , 0  , 0  ), // #428 {r32|r64}
  ROW(3, 1, 1, 3, 35 , 36 , 100, 0  , 0  , 0  ), // #429 {<edx>, <eax>, <ecx>}
  ROW(2, 1, 1, 2, 142, 127, 0  , 0  , 0  , 0  ), // #430 {<al|ax|eax|rax>, <es:[memBase|zdi]>}
  ROW(1, 1, 1, 0, 1  , 0  , 0  , 0  , 0  , 0  ), // #431 {r8lo|r8hi|m8|mem}
  ROW(1, 1, 1, 0, 146, 0  , 0  , 0  , 0  , 0  ), // #432 {r16|m16|mem|r32|r64}
  ROW(2, 1, 1, 2, 127, 142, 0  , 0  , 0  , 0  ), // #433 {<es:[memBase|zdi]>, <al|ax|eax|rax>}
  ROW(3, 0, 1, 0, 147, 147, 147, 0  , 0  , 0  ), // #434 {tmm, tmm, tmm}
  ROW(2, 0, 1, 0, 147, 92 , 0  , 0  , 0  , 0  ), // #435 {tmm, tmem}
  ROW(2, 0, 1, 0, 92 , 147, 0  , 0  , 0  , 0  ), // #436 {tmem, tmm}
  ROW(1, 0, 1, 0, 147, 0  , 0  , 0  , 0  , 0  ), // #437 {tmm}
  ROW(3, 1, 1, 2, 6  , 35 , 36 , 0  , 0  , 0  ), // #438 {r32, <edx>, <eax>}
  ROW(1, 1, 1, 0, 28 , 0  , 0  , 0  , 0  , 0  ), // #439 {r32|m32|mem}
  ROW(1, 1, 1, 0, 148, 0  , 0  , 0  , 0  , 0  ), // #440 {ds:[memBase]}
  ROW(6, 1, 1, 0, 51 , 51 , 51 , 51 , 51 , 47 ), // #441 {zmm, zmm, zmm, zmm, zmm, m128|mem}
  ROW(6, 1, 1, 0, 45 , 45 , 45 , 45 , 45 , 47 ), // #442 {xmm, xmm, xmm, xmm, xmm, m128|mem}
  ROW(3, 1, 1, 0, 45 , 45 , 60 , 0  , 0  , 0  ), // #443 {xmm, xmm, xmm|m64|mem}
  ROW(3, 1, 1, 0, 45 , 45 , 88 , 0  , 0  , 0  ), // #444 {xmm, xmm, xmm|m32|mem}
  ROW(2, 1, 1, 0, 48 , 47 , 0  , 0  , 0  , 0  ), // #445 {ymm, m128|mem}
  ROW(2, 1, 1, 0, 149, 60 , 0  , 0  , 0  , 0  ), // #446 {ymm|zmm, xmm|m64|mem}
  ROW(2, 1, 1, 0, 149, 47 , 0  , 0  , 0  , 0  ), // #447 {ymm|zmm, m128|mem}
  ROW(2, 1, 1, 0, 51 , 50 , 0  , 0  , 0  , 0  ), // #448 {zmm, m256|mem}
  ROW(2, 1, 1, 0, 150, 60 , 0  , 0  , 0  , 0  ), // #449 {xmm|ymm|zmm, xmm|m64|mem}
  ROW(2, 1, 1, 0, 150, 88 , 0  , 0  , 0  , 0  ), // #450 {xmm|ymm|zmm, m32|mem|xmm}
  ROW(4, 1, 1, 0, 83 , 45 , 60 , 10 , 0  , 0  ), // #451 {xmm|k, xmm, xmm|m64|mem, i8|u8}
  ROW(4, 1, 1, 0, 83 , 45 , 88 , 10 , 0  , 0  ), // #452 {xmm|k, xmm, xmm|m32|mem, i8|u8}
  ROW(3, 1, 1, 0, 45 , 45 , 131, 0  , 0  , 0  ), // #453 {xmm, xmm, r32|m32|mem|r64|m64}
  ROW(3, 1, 1, 0, 46 , 149, 10 , 0  , 0  , 0  ), // #454 {xmm|m128|mem, ymm|zmm, i8|u8}
  ROW(4, 1, 1, 0, 45 , 45 , 60 , 10 , 0  , 0  ), // #455 {xmm, xmm, xmm|m64|mem, i8|u8}
  ROW(4, 1, 1, 0, 45 , 45 , 88 , 10 , 0  , 0  ), // #456 {xmm, xmm, xmm|m32|mem, i8|u8}
  ROW(3, 1, 1, 0, 85 , 151, 10 , 0  , 0  , 0  ), // #457 {k, xmm|m128|ymm|m256|zmm|m512, i8|u8}
  ROW(3, 1, 1, 0, 85 , 60 , 10 , 0  , 0  , 0  ), // #458 {k, xmm|m64|mem, i8|u8}
  ROW(3, 1, 1, 0, 85 , 88 , 10 , 0  , 0  , 0  ), // #459 {k, xmm|m32|mem, i8|u8}
  ROW(1, 1, 1, 0, 63 , 0  , 0  , 0  , 0  , 0  ), // #460 {vm32y}
  ROW(1, 1, 1, 0, 64 , 0  , 0  , 0  , 0  , 0  ), // #461 {vm32z}
  ROW(1, 1, 1, 0, 67 , 0  , 0  , 0  , 0  , 0  ), // #462 {vm64z}
  ROW(4, 1, 1, 0, 51 , 51 , 49 , 10 , 0  , 0  ), // #463 {zmm, zmm, ymm|m256|mem, i8|u8}
  ROW(2, 1, 1, 0, 6  , 87 , 0  , 0  , 0  , 0  ), // #464 {r32, xmm|ymm}
  ROW(2, 1, 1, 0, 150, 152, 0  , 0  , 0  , 0  ), // #465 {xmm|ymm|zmm, xmm|m8|mem|r32|r8lo|r8hi|r16}
  ROW(2, 1, 1, 0, 150, 153, 0  , 0  , 0  , 0  ), // #466 {xmm|ymm|zmm, xmm|m32|mem|r32}
  ROW(2, 1, 1, 0, 150, 85 , 0  , 0  , 0  , 0  ), // #467 {xmm|ymm|zmm, k}
  ROW(2, 1, 1, 0, 150, 154, 0  , 0  , 0  , 0  ), // #468 {xmm|ymm|zmm, xmm|m16|mem|r32|r16}
  ROW(3, 1, 1, 0, 114, 45 , 10 , 0  , 0  , 0  ), // #469 {r32|m16|mem|r16, xmm, i8|u8}
  ROW(4, 1, 1, 0, 45 , 45 , 110, 10 , 0  , 0  ), // #470 {xmm, xmm, r32|m8|mem|r8lo|r8hi|r16, i8|u8}
  ROW(4, 1, 1, 0, 45 , 45 , 28 , 10 , 0  , 0  ), // #471 {xmm, xmm, r32|m32|mem, i8|u8}
  ROW(4, 0, 1, 0, 45 , 45 , 15 , 10 , 0  , 0  ), // #472 {xmm, xmm, r64|m64|mem, i8|u8}
  ROW(4, 1, 1, 0, 45 , 45 , 114, 10 , 0  , 0  ), // #473 {xmm, xmm, r32|m16|mem|r16, i8|u8}
  ROW(2, 1, 1, 0, 85 , 150, 0  , 0  , 0  , 0  ), // #474 {k, xmm|ymm|zmm}
  ROW(1, 1, 1, 0, 103, 0  , 0  , 0  , 0  , 0  ), // #475 {rel16|rel32}
  ROW(3, 1, 1, 2, 92 , 35 , 36 , 0  , 0  , 0  ), // #476 {mem, <edx>, <eax>}
  ROW(3, 0, 1, 2, 92 , 35 , 36 , 0  , 0  , 0  )  // #477 {mem, <edx>, <eax>}
};
#undef ROW

#define ROW(flags, mFlags, extFlags, regId) { uint32_t(flags), uint16_t(mFlags), uint8_t(extFlags), uint8_t(regId) }
#define F(VAL) InstDB::kOp##VAL
#define M(VAL) InstDB::kMemOp##VAL
const InstDB::OpSignature InstDB::_opSignatureTable[] = {
  ROW(0, 0, 0, 0xFF),
  ROW(F(GpbLo) | F(GpbHi) | F(Mem), M(M8) | M(Any), 0, 0x00),
  ROW(F(GpbLo) | F(GpbHi), 0, 0, 0x00),
  ROW(F(Gpw) | F(SReg) | F(Mem), M(M16) | M(Any), 0, 0x00),
  ROW(F(Gpw), 0, 0, 0x00),
  ROW(F(Gpd) | F(SReg) | F(Mem), M(M32) | M(Any), 0, 0x00),
  ROW(F(Gpd), 0, 0, 0x00),
  ROW(F(Gpq) | F(SReg) | F(CReg) | F(DReg) | F(Mem), M(M64) | M(Any), 0, 0x00),
  ROW(F(Gpq), 0, 0, 0x00),
  ROW(F(GpbLo) | F(GpbHi) | F(Mem), M(M8), 0, 0x00),
  ROW(F(I8) | F(U8), 0, 0, 0x00),
  ROW(F(Gpw) | F(Mem), M(M16), 0, 0x00),
  ROW(F(I16) | F(U16), 0, 0, 0x00),
  ROW(F(Gpd) | F(Mem), M(M32), 0, 0x00),
  ROW(F(I32) | F(U32), 0, 0, 0x00),
  ROW(F(Gpq) | F(Mem), M(M64) | M(Any), 0, 0x00),
  ROW(F(I32), 0, 0, 0x00),
  ROW(F(SReg) | F(CReg) | F(DReg) | F(Mem) | F(I64) | F(U64), M(M64) | M(Any), 0, 0x00),
  ROW(F(Mem), M(M8) | M(Any), 0, 0x00),
  ROW(F(SReg) | F(Mem), M(M16) | M(Any), 0, 0x00),
  ROW(F(SReg) | F(Mem), M(M32) | M(Any), 0, 0x00),
  ROW(F(Mem), M(M16) | M(Any), 0, 0x00),
  ROW(F(SReg), 0, 0, 0x00),
  ROW(F(CReg) | F(DReg), 0, 0, 0x00),
  ROW(F(Gpq) | F(I32), 0, 0, 0x00),
  ROW(F(Gpw) | F(Gpd) | F(Gpq) | F(Mem), M(M16) | M(M32) | M(M64) | M(Any), 0, 0x00),
  ROW(F(I8), 0, 0, 0x00),
  ROW(F(Gpw) | F(Mem), M(M16) | M(Any), 0, 0x00),
  ROW(F(Gpd) | F(Mem), M(M32) | M(Any), 0, 0x00),
  ROW(F(Mem), M(M32) | M(Any), 0, 0x00),
  ROW(F(Mem), M(M64) | M(Any), 0, 0x00),
  ROW(F(GpbLo) | F(GpbHi) | F(Gpw) | F(Gpd) | F(Gpq) | F(Mem), M(M8) | M(M16) | M(M32) | M(M64) | M(Any), 0, 0x00),
  ROW(F(Gpq) | F(Mem) | F(I32) | F(U32), M(M64) | M(Any), 0, 0x00),
  ROW(F(Gpw) | F(Implicit), 0, 0, 0x01),
  ROW(F(Gpw) | F(Implicit), 0, 0, 0x04),
  ROW(F(Gpd) | F(Implicit), 0, 0, 0x04),
  ROW(F(Gpd) | F(Implicit), 0, 0, 0x01),
  ROW(F(Gpq) | F(Implicit), 0, 0, 0x04),
  ROW(F(Gpq) | F(Implicit), 0, 0, 0x01),
  ROW(F(Gpw) | F(Mem) | F(I8) | F(I16), M(M16) | M(Any), 0, 0x00),
  ROW(F(Gpd) | F(Mem) | F(I8) | F(I32), M(M32) | M(Any), 0, 0x00),
  ROW(F(Gpq) | F(Mem) | F(I8) | F(I32), M(M64) | M(Any), 0, 0x00),
  ROW(F(I8) | F(I16) | F(U16), 0, 0, 0x00),
  ROW(F(I8) | F(I32) | F(U32), 0, 0, 0x00),
  ROW(F(I8) | F(I32), 0, 0, 0x00),
  ROW(F(Xmm), 0, 0, 0x00),
  ROW(F(Xmm) | F(Mem), M(M128) | M(Any), 0, 0x00),
  ROW(F(Mem), M(M128) | M(Any), 0, 0x00),
  ROW(F(Ymm), 0, 0, 0x00),
  ROW(F(Ymm) | F(Mem), M(M256) | M(Any), 0, 0x00),
  ROW(F(Mem), M(M256) | M(Any), 0, 0x00),
  ROW(F(Zmm), 0, 0, 0x00),
  ROW(F(Zmm) | F(Mem), M(M512) | M(Any), 0, 0x00),
  ROW(F(Mem), M(M512) | M(Any), 0, 0x00),
  ROW(F(Xmm) | F(Mem) | F(I8) | F(U8), M(M128) | M(Any), 0, 0x00),
  ROW(F(Ymm) | F(Mem) | F(I8) | F(U8), M(M256) | M(Any), 0, 0x00),
  ROW(F(Zmm) | F(Mem) | F(I8) | F(U8), M(M512) | M(Any), 0, 0x00),
  ROW(F(Mm), 0, 0, 0x00),
  ROW(F(Gpq) | F(Mm) | F(Mem), M(M64) | M(Any), 0, 0x00),
  ROW(F(Xmm) | F(Mm), 0, 0, 0x00),
  ROW(F(Xmm) | F(Mem), M(M64) | M(Any), 0, 0x00),
  ROW(F(Gpw) | F(Gpd) | F(Gpq) | F(Mem), M(M16) | M(M32) | M(M64), 0, 0x00),
  ROW(F(Vm), M(Vm32x), 0, 0x00),
  ROW(F(Vm), M(Vm32y), 0, 0x00),
  ROW(F(Vm), M(Vm32z), 0, 0x00),
  ROW(F(Vm), M(Vm64x), 0, 0x00),
  ROW(F(Vm), M(Vm64y), 0, 0x00),
  ROW(F(Vm), M(Vm64z), 0, 0x00),
  ROW(F(GpbLo) | F(Implicit), 0, 0, 0x01),
  ROW(F(Gpw) | F(Gpq) | F(Mem), M(M16) | M(M64), 0, 0x00),
  ROW(F(SReg), 0, 0, 0x1A),
  ROW(F(SReg), 0, 0, 0x60),
  ROW(F(Gpw) | F(Gpq) | F(Mem) | F(I8) | F(I16) | F(I32), M(M16) | M(M64), 0, 0x00),
  ROW(F(Gpd) | F(Mem) | F(I32) | F(U32), M(M32), 0, 0x00),
  ROW(F(SReg), 0, 0, 0x1E),
  ROW(F(Vm), M(Vm64x) | M(Vm64y), 0, 0x00),
  ROW(F(I4) | F(U4), 0, 0, 0x00),
  ROW(F(Mem), M(M32) | M(M64), 0, 0x00),
  ROW(F(St), 0, 0, 0x01),
  ROW(F(St), 0, 0, 0x00),
  ROW(F(Mem), M(M48) | M(Any), 0, 0x00),
  ROW(F(Mem), M(M80) | M(Any), 0, 0x00),
  ROW(F(GpbLo) | F(I8) | F(U8), 0, 0, 0x02),
  ROW(F(Xmm) | F(KReg), 0, 0, 0x00),
  ROW(F(Ymm) | F(KReg), 0, 0, 0x00),
  ROW(F(KReg), 0, 0, 0x00),
  ROW(F(Gpq) | F(Xmm) | F(Mem), M(M64) | M(Any), 0, 0x00),
  ROW(F(Xmm) | F(Ymm), 0, 0, 0x00),
  ROW(F(Xmm) | F(Mem), M(M32) | M(Any), 0, 0x00),
  ROW(F(Xmm) | F(Mem), M(M16) | M(Any), 0, 0x00),
  ROW(F(Bnd), 0, 0, 0x00),
  ROW(F(Bnd) | F(Mem), M(Any), 0, 0x00),
  ROW(F(Mem), M(Any), 0, 0x00),
  ROW(F(Gpw) | F(Gpd) | F(Mem) | F(I32) | F(I64) | F(Rel32), M(M16) | M(M32), 0, 0x00),
  ROW(F(Gpq) | F(Mem) | F(I32) | F(I64) | F(Rel32), M(M64) | M(Any), 0, 0x00),
  ROW(F(GpbLo) | F(GpbHi) | F(Gpw) | F(Gpd) | F(Mem), M(M8) | M(M16) | M(M32), 0, 0x00),
  ROW(F(GpbLo) | F(GpbHi) | F(Gpq) | F(Mem), M(M8) | M(M64), 0, 0x00),
  ROW(F(Gpw) | F(Gpd), 0, 0, 0x00),
  ROW(F(Mem), M(BaseOnly) | M(Es), 0, 0x00),
  ROW(F(St) | F(Mem), M(M32) | M(M64), 0, 0x00),
  ROW(F(Gpd) | F(Implicit), 0, 0, 0x02),
  ROW(F(Gpd) | F(Gpq) | F(Implicit), 0, 0, 0x01),
  ROW(F(I32) | F(I64) | F(Rel8) | F(Rel32), 0, 0, 0x00),
  ROW(F(I32) | F(I64) | F(Rel32), 0, 0, 0x00),
  ROW(F(Gpw) | F(Gpd) | F(Implicit), 0, 0, 0x02),
  ROW(F(I32) | F(I64) | F(Rel8), 0, 0, 0x00),
  ROW(F(Gpd) | F(Gpq) | F(Implicit), 0, 0, 0x02),
  ROW(F(Gpq) | F(Mem) | F(I32) | F(I64) | F(Rel8) | F(Rel32), M(M64) | M(Any), 0, 0x00),
  ROW(F(Gpd) | F(Mem) | F(I32) | F(I64) | F(Rel32), M(M32) | M(Any), 0, 0x00),
  ROW(F(GpbLo) | F(GpbHi) | F(Gpw) | F(Gpd) | F(KReg) | F(Mem), M(M8) | M(Any), 0, 0x00),
  ROW(F(GpbLo) | F(GpbHi) | F(Gpw) | F(Gpd) | F(Mem), M(M8) | M(Any), 0, 0x00),
  ROW(F(Gpd) | F(KReg) | F(Mem), M(M32) | M(Any), 0, 0x00),
  ROW(F(Gpq) | F(KReg) | F(Mem), M(M64) | M(Any), 0, 0x00),
  ROW(F(Gpw) | F(Gpd) | F(KReg) | F(Mem), M(M16) | M(Any), 0, 0x00),
  ROW(F(Gpw) | F(Gpd) | F(Mem), M(M16) | M(Any), 0, 0x00),
  ROW(F(Gpd) | F(Gpq), 0, 0, 0x00),
  ROW(F(GpbLo) | F(GpbHi) | F(Gpw) | F(Mem), M(M8) | M(M16), 0, 0x00),
  ROW(F(Mm) | F(Mem), M(M64) | M(Any), 0, 0x00),
  ROW(F(Mm) | F(Mem) | F(I8) | F(U8), M(M64) | M(Any), 0, 0x00),
  ROW(F(U16), 0, 0, 0x00),
  ROW(F(Xmm) | F(Ymm) | F(Mem), M(M128) | M(M256), 0, 0x00),
  ROW(F(Xmm) | F(I8) | F(U8), 0, 0, 0x00),
  ROW(F(Xmm) | F(Implicit), 0, 0, 0x01),
  ROW(F(Mem), M(Mib), 0, 0x00),
  ROW(F(Gpw) | F(Gpd) | F(Gpq), 0, 0, 0x00),
  ROW(F(Mem) | F(Implicit), M(BaseOnly) | M(Ds), 0, 0x01),
  ROW(F(Mem) | F(Implicit), M(BaseOnly) | M(Ds), 0, 0x40),
  ROW(F(Mem) | F(Implicit), M(BaseOnly) | M(Es), 0, 0x80),
  ROW(F(Gpq) | F(Implicit), 0, 0, 0x02),
  ROW(F(Gpq) | F(Implicit), 0, 0, 0x08),
  ROW(F(Gpd) | F(Implicit), 0, 0, 0x08),
  ROW(F(Gpd) | F(Gpq) | F(Mem), M(M32) | M(M64) | M(Any), 0, 0x00),
  ROW(F(Mem), M(M16) | M(M32), 0, 0x00),
  ROW(F(Mem), M(M16) | M(M32) | M(M64), 0, 0x00),
  ROW(F(St) | F(Mem), M(M32) | M(M64) | M(M80), 0, 0x00),
  ROW(F(Gpw) | F(Mem), M(M16) | M(Any), 0, 0x01),
  ROW(F(GpbLo) | F(Gpw) | F(Gpd), 0, 0, 0x01),
  ROW(F(Gpw) | F(I8) | F(U8), 0, 0, 0x04),
  ROW(F(Mem), M(BaseOnly) | M(Es), 0, 0x80),
  ROW(F(Gpw), 0, 0, 0x04),
  ROW(F(GpbHi) | F(Implicit), 0, 0, 0x01),
  ROW(F(Mem), M(M8) | M(M16) | M(M32) | M(M48) | M(M64) | M(M80) | M(M128) | M(M256) | M(M512) | M(M1024) | M(Any), 0, 0x00),
  ROW(F(GpbLo) | F(Gpw) | F(Gpd) | F(Gpq) | F(Implicit), 0, 0, 0x01),
  ROW(F(Mem) | F(Implicit), M(BaseOnly) | M(Ds), 0, 0x80),
  ROW(F(Gpw) | F(U8), 0, 0, 0x04),
  ROW(F(Mem), M(BaseOnly) | M(Ds), 0, 0x40),
  ROW(F(Gpw) | F(Gpd) | F(Gpq) | F(Mem), M(M16) | M(Any), 0, 0x00),
  ROW(F(Tmm), 0, 0, 0x00),
  ROW(F(Mem), M(BaseOnly) | M(Ds), 0, 0x00),
  ROW(F(Ymm) | F(Zmm), 0, 0, 0x00),
  ROW(F(Xmm) | F(Ymm) | F(Zmm), 0, 0, 0x00),
  ROW(F(Xmm) | F(Ymm) | F(Zmm) | F(Mem), M(M128) | M(M256) | M(M512), 0, 0x00),
  ROW(F(GpbLo) | F(GpbHi) | F(Gpw) | F(Gpd) | F(Xmm) | F(Mem), M(M8) | M(Any), 0, 0x00),
  ROW(F(Gpd) | F(Xmm) | F(Mem), M(M32) | M(Any), 0, 0x00),
  ROW(F(Gpw) | F(Gpd) | F(Xmm) | F(Mem), M(M16) | M(Any), 0, 0x00)
};
#undef M
#undef F
#undef ROW
// ----------------------------------------------------------------------------
// ${InstSignatureTable:End}
#endif // !ASMJIT_NO_VALIDATION

// ============================================================================
// [asmjit::x86::InstInternal - QueryRWInfo]
// ============================================================================

// ${InstRWInfoTable:Begin}
// ------------------- Automatically generated, do not edit -------------------
const uint8_t InstDB::rwInfoIndexA[Inst::_kIdCount] = {
  0, 0, 1, 1, 0, 2, 3, 2, 4, 4, 5, 6, 4, 4, 3, 4, 4, 4, 4, 7, 0, 2, 0, 4, 4, 4,
  4, 8, 0, 9, 9, 9, 9, 9, 0, 0, 0, 0, 9, 9, 9, 9, 9, 10, 10, 10, 11, 11, 12, 13,
  14, 9, 9, 0, 15, 16, 16, 16, 0, 0, 0, 17, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
  3, 3, 3, 3, 3, 3, 18, 0, 0, 19, 0, 0, 0, 0, 0, 20, 21, 0, 22, 23, 24, 7, 25,
  25, 25, 24, 26, 7, 24, 27, 28, 29, 30, 31, 32, 33, 25, 25, 7, 27, 28, 33, 34,
  0, 0, 0, 0, 35, 4, 4, 5, 6, 0, 0, 0, 0, 0, 36, 36, 0, 0, 37, 0, 0, 38, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 38, 0, 38, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 38, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 38, 0, 38, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4, 4, 4, 0, 4, 4, 35,
  39, 40, 0, 0, 0, 41, 0, 37, 0, 0, 0, 0, 42, 0, 43, 42, 42, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 44, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 45, 46, 47, 48, 49, 50, 51,
  52, 0, 0, 0, 53, 54, 55, 56, 0, 0, 0, 0, 0, 0, 0, 0, 0, 53, 54, 55, 56, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 57, 58, 0, 59, 0, 60, 0, 59, 0, 59, 0, 59, 0, 0,
  0, 0, 61, 62, 62, 62, 57, 59, 0, 0, 0, 9, 0, 0, 4, 4, 5, 6, 0, 0, 4, 4, 5, 6,
  0, 0, 63, 64, 64, 65, 46, 24, 36, 65, 51, 64, 64, 66, 67, 67, 68, 69, 69, 70,
  70, 58, 58, 65, 58, 58, 69, 69, 71, 47, 51, 72, 47, 7, 7, 46, 73, 9, 64, 64,
  73, 0, 35, 4, 4, 5, 6, 0, 74, 0, 0, 75, 0, 2, 4, 4, 76, 77, 9, 9, 9, 3, 3, 4,
  3, 3, 3, 3, 3, 3, 3, 3, 3, 0, 3, 3, 0, 3, 78, 3, 0, 0, 0, 3, 3, 4, 3, 0, 0, 3,
  3, 4, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 27, 27, 78, 78, 78, 78, 78, 78, 78, 78, 78,
  78, 27, 78, 78, 78, 27, 27, 78, 78, 78, 3, 3, 3, 79, 3, 3, 3, 27, 27, 0, 0,
  0, 0, 3, 3, 4, 4, 3, 3, 4, 4, 4, 4, 3, 3, 4, 4, 80, 81, 82, 24, 24, 24, 81, 81,
  82, 24, 24, 24, 81, 4, 3, 78, 3, 3, 4, 3, 3, 0, 0, 0, 9, 0, 0, 0, 3, 0, 0,
  0, 0, 0, 0, 0, 3, 3, 0, 0, 0, 0, 3, 3, 3, 3, 83, 3, 3, 0, 3, 3, 3, 83, 3, 3, 3,
  3, 3, 3, 3, 3, 3, 3, 27, 84, 0, 3, 3, 4, 3, 3, 3, 4, 3, 0, 0, 0, 0, 0, 0, 0,
  3, 85, 7, 86, 85, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 87, 0, 0, 0, 0, 85, 85, 0,
  0, 0, 0, 0, 0, 7, 86, 0, 0, 85, 85, 0, 0, 2, 88, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4, 4,
  4, 0, 4, 4, 0, 85, 0, 0, 85, 0, 0, 0, 0, 0, 0, 0, 0, 0, 7, 7, 26, 86, 0, 0,
  0, 0, 0, 0, 89, 0, 0, 2, 4, 4, 5, 6, 0, 0, 0, 0, 0, 0, 0, 9, 0, 0, 0, 0, 0, 15,
  90, 90, 0, 91, 0, 0, 9, 9, 20, 21, 0, 0, 0, 0, 0, 4, 4, 4, 4, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 7, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 92, 28, 93, 94, 93, 94, 92, 28, 93, 94, 93, 94, 95, 96, 0, 0, 0, 0, 20, 21,
  97, 97, 98, 9, 0, 73, 99, 99, 9, 99, 9, 98, 9, 98, 0, 98, 9, 98, 9, 99, 28,
  0, 28, 0, 0, 0, 33, 33, 99, 9, 99, 9, 9, 98, 9, 98, 28, 28, 33, 33, 98, 9, 9,
  99, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 100, 100, 9, 9, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 9, 9, 27, 101, 59, 59, 0, 0, 0, 0, 0,
  0, 0, 0, 59, 59, 9, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 65, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 102,
  102, 46, 103, 102, 102, 102, 102, 102, 102, 102, 102, 0, 104, 104, 0, 69, 69,
  105, 106, 65, 65, 65, 65, 107, 69, 9, 9, 71, 102, 102, 0, 0, 0, 97, 0, 0, 0,
  0, 0, 0, 0, 108, 0, 0, 0, 0, 0, 0, 0, 9, 9, 9, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 109, 33, 110, 110, 28, 111, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 97, 97, 97,
  97, 0, 0, 0, 0, 0, 0, 9, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 9, 9, 9, 9, 0, 0, 0, 0, 59, 59, 59, 59, 7,
  7, 7, 0, 7, 0, 7, 7, 7, 7, 7, 7, 0, 7, 7, 79, 7, 0, 7, 0, 0, 7, 0, 0, 0, 0, 9,
  9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 112, 112, 113, 114, 110, 110, 110, 110, 80, 112,
  115, 114, 113, 113, 114, 115, 114, 113, 114, 116, 117, 98, 98, 98, 116, 113, 114,
  115, 114, 113, 114, 112, 114, 116, 117, 98, 98, 98, 116, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 9, 9, 9, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 65, 65,
  118, 65, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 108, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 9, 9, 0, 0, 100, 100, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 9, 9, 0, 0, 100, 100, 0, 0, 9, 0, 0, 0, 0, 0, 65, 65, 0, 0,
  0, 0, 0, 0, 0, 0, 65, 118, 0, 0, 0, 0, 0, 0, 9, 9, 0, 0, 0, 0, 0, 0, 0, 108, 108,
  20, 21, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 119, 120, 119, 120, 0, 121,
  0, 122, 0, 0, 0, 2, 4, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
};

const uint8_t InstDB::rwInfoIndexB[Inst::_kIdCount] = {
  0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 3, 0, 0, 0,
  0, 0, 4, 0, 0, 0, 0, 0, 5, 5, 6, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 7, 0, 0, 0, 0, 4, 8, 1, 0, 9, 0, 0, 0, 10, 10, 10, 0, 0, 11, 0, 10, 12, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 5, 5, 0, 13, 14, 15, 16, 17, 0, 0, 18, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 19, 1, 1, 20, 21, 0, 0, 0,
  0, 5, 5, 0, 0, 0, 0, 0, 0, 22, 23, 0, 0, 24, 25, 26, 27, 0, 0, 25, 25, 25, 25,
  25, 25, 25, 25, 28, 29, 29, 28, 0, 0, 0, 24, 25, 24, 25, 0, 25, 24, 24, 24, 24,
  24, 24, 24, 0, 0, 30, 30, 30, 24, 24, 28, 0, 31, 10, 0, 0, 0, 0, 0, 0, 24,
  25, 0, 0, 0, 32, 33, 32, 34, 0, 0, 0, 0, 0, 10, 32, 0, 0, 0, 0, 35, 33, 32, 35,
  34, 24, 25, 24, 25, 0, 29, 29, 29, 29, 0, 0, 0, 25, 10, 10, 32, 32, 0, 0, 0,
  0, 5, 5, 0, 0, 0, 0, 0, 0, 21, 36, 0, 20, 37, 38, 0, 39, 40, 0, 0, 0, 0, 0, 10,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 8, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 41, 42, 43, 44, 41, 42, 41, 42, 43,
  44, 43, 44, 0, 0, 0, 0, 0, 0, 0, 0, 41, 42, 43, 0, 0, 0, 0, 44, 45, 46, 47, 48,
  45, 46, 47, 48, 0, 0, 0, 0, 49, 50, 51, 41, 42, 43, 44, 41, 42, 43, 44, 52,
  0, 0, 53, 0, 54, 0, 0, 0, 0, 0, 10, 0, 10, 55, 56, 55, 0, 0, 0, 0, 0, 0, 55, 57,
  57, 0, 58, 59, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 60, 60, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 5, 61, 0, 0, 0, 0, 62, 0, 63, 20, 64, 20, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 65, 0, 0, 0, 0, 0, 0, 6, 5, 5, 0, 0,
  0, 0, 66, 67, 0, 0, 0, 0, 68, 69, 0, 3, 3, 70, 22, 71, 72, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 73, 39,
  74, 75, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 76, 0, 0, 0, 0, 0, 0, 0, 10, 10, 10, 10, 10,
  10, 10, 0, 0, 2, 2, 2, 77, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 64, 0, 0, 0, 0, 0, 0, 0, 0, 78, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 79, 79, 80, 79, 80, 80, 80, 79, 79, 81, 82, 0, 83, 0, 0, 0, 0, 0, 84,
  2, 2, 85, 86, 0, 0, 0, 11, 87, 0, 0, 4, 0, 0, 0, 0, 88, 88, 88, 88, 88, 88,
  88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88,
  88, 88, 88, 0, 88, 0, 32, 0, 0, 0, 5, 0, 0, 6, 0, 89, 4, 0, 89, 4, 5, 5, 32,
  19, 90, 79, 90, 0, 0, 0, 0, 0, 0, 0, 0, 0, 91, 0, 90, 92, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 93, 93, 93, 93, 93, 0, 0, 0, 0, 0, 94, 95, 0, 0, 0, 0, 96,
  96, 0, 56, 95, 0, 0, 0, 0, 97, 98, 97, 98, 3, 3, 99, 100, 3, 3, 3, 3, 3, 3,
  0, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 101, 101, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 3, 3, 102, 103, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 104, 0, 0, 0, 0, 0, 0, 105, 0, 106, 107, 108, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 106, 107, 3, 3, 3, 99, 100, 3, 109, 3, 55, 55, 0,
  0, 0, 0, 110, 111, 112, 111, 112, 110, 111, 112, 111, 112, 22, 113, 113, 114,
  115, 113, 113, 116, 117, 113, 113, 116, 117, 113, 113, 116, 117, 118, 118, 119,
  120, 113, 113, 113, 113, 113, 113, 118, 118, 113, 113, 116, 117, 113, 113,
  116, 117, 113, 113, 116, 117, 113, 113, 113, 113, 113, 113, 118, 118, 118, 118,
  119, 120, 113, 113, 116, 117, 113, 113, 116, 117, 113, 113, 116, 117, 118,
  118, 119, 120, 113, 113, 116, 117, 113, 113, 116, 117, 113, 113, 121, 122, 118,
  118, 119, 120, 123, 123, 77, 124, 0, 0, 0, 0, 125, 126, 10, 10, 10, 10, 10,
  10, 10, 10, 126, 127, 0, 0, 128, 129, 84, 84, 128, 129, 3, 3, 3, 3, 3, 3, 3, 130,
  131, 132, 131, 132, 130, 131, 132, 131, 132, 100, 0, 53, 58, 133, 133, 3,
  3, 99, 100, 0, 134, 0, 3, 3, 99, 100, 0, 135, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 136, 137, 137, 138, 139, 139, 0, 0, 0, 0, 0, 0, 0, 140, 0, 0, 141, 0, 0,
  3, 11, 134, 0, 0, 142, 135, 3, 3, 99, 100, 0, 11, 3, 3, 143, 143, 144, 144,
  0, 0, 0, 0, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
  101, 3, 0, 0, 0, 0, 0, 0, 3, 118, 145, 145, 3, 3, 3, 3, 66, 67, 3, 3, 3, 3, 68,
  69, 145, 145, 145, 145, 145, 145, 109, 109, 0, 0, 0, 0, 109, 109, 109, 109,
  109, 109, 0, 0, 113, 113, 113, 113, 146, 146, 3, 3, 3, 113, 3, 3, 113, 113, 118,
  118, 147, 147, 147, 3, 147, 3, 113, 113, 113, 113, 113, 3, 0, 0, 0, 0, 70,
  22, 71, 148, 126, 125, 127, 126, 0, 0, 0, 3, 0, 3, 0, 0, 0, 0, 0, 0, 3, 0, 0,
  0, 0, 3, 0, 3, 3, 0, 149, 100, 99, 150, 0, 0, 151, 151, 151, 151, 151, 151, 151,
  151, 151, 151, 151, 151, 113, 113, 3, 3, 133, 133, 3, 3, 3, 3, 3, 3, 3, 3,
  3, 3, 3, 3, 3, 3, 3, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 3, 3, 3, 3,
  3, 3, 3, 3, 0, 0, 0, 0, 3, 3, 3, 152, 84, 84, 3, 3, 84, 84, 3, 3, 153, 153, 153,
  153, 3, 0, 0, 0, 0, 153, 153, 153, 153, 153, 153, 3, 3, 113, 113, 113, 3, 153,
  153, 3, 3, 113, 113, 113, 3, 3, 145, 84, 84, 84, 3, 3, 3, 154, 155, 154, 3,
  3, 3, 154, 154, 154, 3, 3, 3, 154, 154, 155, 154, 3, 3, 3, 154, 3, 3, 3, 3,
  3, 3, 3, 3, 113, 113, 0, 145, 145, 145, 145, 145, 145, 145, 145, 3, 3, 3, 3, 3,
  3, 3, 3, 3, 3, 3, 3, 3, 128, 129, 0, 0, 128, 129, 0, 0, 128, 129, 0, 129, 84,
  84, 128, 129, 84, 84, 128, 129, 84, 84, 128, 129, 0, 0, 128, 129, 0, 0, 128,
  129, 0, 129, 3, 3, 99, 100, 0, 0, 10, 10, 10, 10, 10, 10, 10, 10, 0, 0, 3, 3,
  3, 3, 3, 3, 0, 0, 128, 129, 91, 3, 3, 99, 100, 0, 0, 0, 0, 3, 3, 3, 3, 3, 3,
  0, 0, 0, 0, 56, 56, 156, 0, 0, 0, 0, 0, 0, 0, 0, 0, 80, 0, 0, 0, 0, 0, 157, 157,
  157, 157, 158, 158, 158, 158, 158, 158, 158, 158, 156, 0, 0
};

const InstDB::RWInfo InstDB::rwInfoA[] = {
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 0 , 0 , 0 , 0 , 0 , 0  } }, // #0 [ref=931x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 1 , 0 , 0 , 0 , 0 , 0  } }, // #1 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 1 , { 2 , 3 , 0 , 0 , 0 , 0  } }, // #2 [ref=7x]
  { InstDB::RWInfo::kCategoryGeneric   , 2 , { 2 , 3 , 0 , 0 , 0 , 0  } }, // #3 [ref=99x]
  { InstDB::RWInfo::kCategoryGeneric   , 3 , { 4 , 5 , 0 , 0 , 0 , 0  } }, // #4 [ref=55x]
  { InstDB::RWInfo::kCategoryGeneric   , 4 , { 6 , 7 , 0 , 0 , 0 , 0  } }, // #5 [ref=6x]
  { InstDB::RWInfo::kCategoryGeneric   , 5 , { 8 , 9 , 0 , 0 , 0 , 0  } }, // #6 [ref=6x]
  { InstDB::RWInfo::kCategoryGeneric   , 3 , { 10, 5 , 0 , 0 , 0 , 0  } }, // #7 [ref=26x]
  { InstDB::RWInfo::kCategoryGeneric   , 7 , { 12, 13, 0 , 0 , 0 , 0  } }, // #8 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 2 , { 11, 3 , 0 , 0 , 0 , 0  } }, // #9 [ref=65x]
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
  { InstDB::RWInfo::kCategoryImul      , 2 , { 0 , 0 , 0 , 0 , 0 , 0  } }, // #39 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 50, 51, 0 , 0 , 0 , 0  } }, // #40 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 53, 51, 0 , 0 , 0 , 0  } }, // #41 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 13, { 3 , 5 , 0 , 0 , 0 , 0  } }, // #42 [ref=3x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 22, 29, 0 , 0 , 0 , 0  } }, // #43 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 54, 0 , 0 , 0 , 0 , 0  } }, // #44 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 23, { 55, 40, 0 , 0 , 0 , 0  } }, // #45 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 24, { 44, 9 , 0 , 0 , 0 , 0  } }, // #46 [ref=4x]
  { InstDB::RWInfo::kCategoryGeneric   , 25, { 35, 7 , 0 , 0 , 0 , 0  } }, // #47 [ref=3x]
  { InstDB::RWInfo::kCategoryGeneric   , 26, { 48, 13, 0 , 0 , 0 , 0  } }, // #48 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 55, 40, 0 , 0 , 0 , 0  } }, // #49 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 44, 9 , 0 , 0 , 0 , 0  } }, // #50 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 35, 7 , 0 , 0 , 0 , 0  } }, // #51 [ref=3x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 48, 13, 0 , 0 , 0 , 0  } }, // #52 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 40, 40, 0 , 0 , 0 , 0  } }, // #53 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 9 , 9 , 0 , 0 , 0 , 0  } }, // #54 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 7 , 7 , 0 , 0 , 0 , 0  } }, // #55 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 13, 13, 0 , 0 , 0 , 0  } }, // #56 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 27, { 11, 3 , 0 , 0 , 0 , 0  } }, // #57 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 13, { 10, 5 , 0 , 0 , 0 , 0  } }, // #58 [ref=5x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 2 , 3 , 0 , 0 , 0 , 0  } }, // #59 [ref=13x]
  { InstDB::RWInfo::kCategoryGeneric   , 8 , { 11, 3 , 0 , 0 , 0 , 0  } }, // #60 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 50, 20, 0 , 0 , 0 , 0  } }, // #61 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 57, 0 , 0 , 0 , 0 , 0  } }, // #62 [ref=3x]
  { InstDB::RWInfo::kCategoryMov       , 29, { 0 , 0 , 0 , 0 , 0 , 0  } }, // #63 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 30, { 10, 5 , 0 , 0 , 0 , 0  } }, // #64 [ref=6x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 11, 3 , 0 , 0 , 0 , 0  } }, // #65 [ref=14x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 36, 60, 0 , 0 , 0 , 0  } }, // #66 [ref=1x]
  { InstDB::RWInfo::kCategoryMovh64    , 12, { 0 , 0 , 0 , 0 , 0 , 0  } }, // #67 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 61, 7 , 0 , 0 , 0 , 0  } }, // #68 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 12, { 35, 7 , 0 , 0 , 0 , 0  } }, // #69 [ref=7x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 55, 5 , 0 , 0 , 0 , 0  } }, // #70 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 28, { 44, 9 , 0 , 0 , 0 , 0  } }, // #71 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 62, 20, 0 , 0 , 0 , 0  } }, // #72 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 14, { 11, 3 , 0 , 0 , 0 , 0  } }, // #73 [ref=3x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 17, 29, 0 , 0 , 0 , 0  } }, // #74 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 11, { 3 , 3 , 0 , 0 , 0 , 0  } }, // #75 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 51, 22, 0 , 0 , 0 , 0  } }, // #76 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 51, 65, 0 , 0 , 0 , 0  } }, // #77 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 4 , { 26, 7 , 0 , 0 , 0 , 0  } }, // #78 [ref=18x]
  { InstDB::RWInfo::kCategoryGeneric   , 3 , { 68, 5 , 0 , 0 , 0 , 0  } }, // #79 [ref=2x]
  { InstDB::RWInfo::kCategoryVmov1_8   , 0 , { 0 , 0 , 0 , 0 , 0 , 0  } }, // #80 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 5 , { 10, 9 , 0 , 0 , 0 , 0  } }, // #81 [ref=4x]
  { InstDB::RWInfo::kCategoryGeneric   , 27, { 10, 13, 0 , 0 , 0 , 0  } }, // #82 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 4 , 0 , 0 , 0 , 0 , 0  } }, // #83 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 3 , { 5 , 5 , 0 , 0 , 0 , 0  } }, // #84 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 10, { 2 , 70, 0 , 0 , 0 , 0  } }, // #85 [ref=8x]
  { InstDB::RWInfo::kCategoryGeneric   , 5 , { 37, 9 , 0 , 0 , 0 , 0  } }, // #86 [ref=3x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 16, 71, 0 , 0 , 0 , 0  } }, // #87 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 22, 21, 0 , 0 , 0 , 0  } }, // #88 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 62, 22, 0 , 0 , 0 , 0  } }, // #89 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 8 , { 74, 3 , 0 , 0 , 0 , 0  } }, // #90 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 8 , { 11, 43, 0 , 0 , 0 , 0  } }, // #91 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 13, { 76, 5 , 0 , 0 , 0 , 0  } }, // #92 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 13, { 11, 5 , 0 , 0 , 0 , 0  } }, // #93 [ref=4x]
  { InstDB::RWInfo::kCategoryGeneric   , 37, { 74, 77, 0 , 0 , 0 , 0  } }, // #94 [ref=4x]
  { InstDB::RWInfo::kCategoryGeneric   , 38, { 11, 7 , 0 , 0 , 0 , 0  } }, // #95 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 39, { 11, 9 , 0 , 0 , 0 , 0  } }, // #96 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 11, { 11, 3 , 0 , 0 , 0 , 0  } }, // #97 [ref=7x]
  { InstDB::RWInfo::kCategoryVmov2_1   , 40, { 0 , 0 , 0 , 0 , 0 , 0  } }, // #98 [ref=14x]
  { InstDB::RWInfo::kCategoryVmov1_2   , 14, { 0 , 0 , 0 , 0 , 0 , 0  } }, // #99 [ref=7x]
  { InstDB::RWInfo::kCategoryGeneric   , 44, { 74, 43, 0 , 0 , 0 , 0  } }, // #100 [ref=6x]
  { InstDB::RWInfo::kCategoryGeneric   , 5 , { 44, 9 , 0 , 0 , 0 , 0  } }, // #101 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 51, { 11, 3 , 0 , 0 , 0 , 0  } }, // #102 [ref=12x]
  { InstDB::RWInfo::kCategoryVmovddup  , 52, { 0 , 0 , 0 , 0 , 0 , 0  } }, // #103 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 12, { 35, 60, 0 , 0 , 0 , 0  } }, // #104 [ref=2x]
  { InstDB::RWInfo::kCategoryVmovmskpd , 0 , { 0 , 0 , 0 , 0 , 0 , 0  } }, // #105 [ref=1x]
  { InstDB::RWInfo::kCategoryVmovmskps , 0 , { 0 , 0 , 0 , 0 , 0 , 0  } }, // #106 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 53, { 35, 7 , 0 , 0 , 0 , 0  } }, // #107 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 2 , { 3 , 3 , 0 , 0 , 0 , 0  } }, // #108 [ref=4x]
  { InstDB::RWInfo::kCategoryGeneric   , 15, { 11, 40, 0 , 0 , 0 , 0  } }, // #109 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 11, 7 , 0 , 0 , 0 , 0  } }, // #110 [ref=6x]
  { InstDB::RWInfo::kCategoryGeneric   , 27, { 11, 13, 0 , 0 , 0 , 0  } }, // #111 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 35, 3 , 0 , 0 , 0 , 0  } }, // #112 [ref=4x]
  { InstDB::RWInfo::kCategoryVmov1_4   , 57, { 0 , 0 , 0 , 0 , 0 , 0  } }, // #113 [ref=6x]
  { InstDB::RWInfo::kCategoryVmov1_2   , 41, { 0 , 0 , 0 , 0 , 0 , 0  } }, // #114 [ref=9x]
  { InstDB::RWInfo::kCategoryVmov1_8   , 58, { 0 , 0 , 0 , 0 , 0 , 0  } }, // #115 [ref=3x]
  { InstDB::RWInfo::kCategoryVmov4_1   , 59, { 0 , 0 , 0 , 0 , 0 , 0  } }, // #116 [ref=4x]
  { InstDB::RWInfo::kCategoryVmov8_1   , 60, { 0 , 0 , 0 , 0 , 0 , 0  } }, // #117 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 18, { 11, 3 , 0 , 0 , 0 , 0  } }, // #118 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 17, { 44, 9 , 0 , 0 , 0 , 0  } }, // #119 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 32, { 35, 7 , 0 , 0 , 0 , 0  } }, // #120 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 11, { 2 , 2 , 0 , 0 , 0 , 0  } }, // #121 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 51, { 2 , 2 , 0 , 0 , 0 , 0  } }  // #122 [ref=1x]
};

const InstDB::RWInfo InstDB::rwInfoB[] = {
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 0 , 0 , 0 , 0 , 0 , 0  } }, // #0 [ref=734x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 1 , 0 , 0 , 0 , 0 , 0  } }, // #1 [ref=5x]
  { InstDB::RWInfo::kCategoryGeneric   , 3 , { 10, 5 , 0 , 0 , 0 , 0  } }, // #2 [ref=7x]
  { InstDB::RWInfo::kCategoryGeneric   , 6 , { 11, 3 , 3 , 0 , 0 , 0  } }, // #3 [ref=186x]
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
  { InstDB::RWInfo::kCategoryGeneric   , 18, { 3 , 0 , 0 , 0 , 0 , 0  } }, // #24 [ref=15x]
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
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 52, 0 , 0 , 0 , 0 , 0  } }, // #37 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 26, 0 , 0 , 0 , 0 , 0  } }, // #38 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 5 , { 4 , 9 , 0 , 0 , 0 , 0  } }, // #39 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 4 , 5 , 0 , 0 , 0 , 0  } }, // #40 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 55, 40, 40, 0 , 0 , 0  } }, // #41 [ref=6x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 44, 9 , 9 , 0 , 0 , 0  } }, // #42 [ref=6x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 35, 7 , 7 , 0 , 0 , 0  } }, // #43 [ref=6x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 48, 13, 13, 0 , 0 , 0  } }, // #44 [ref=6x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 55, 40, 0 , 0 , 0 , 0  } }, // #45 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 44, 9 , 0 , 0 , 0 , 0  } }, // #46 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 35, 7 , 0 , 0 , 0 , 0  } }, // #47 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 48, 13, 0 , 0 , 0 , 0  } }, // #48 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 48, 40, 40, 0 , 0 , 0  } }, // #49 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 35, 9 , 9 , 0 , 0 , 0  } }, // #50 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 44, 13, 13, 0 , 0 , 0  } }, // #51 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 56, 0 , 0 , 0 , 0 , 0  } }, // #52 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 28, { 9 , 0 , 0 , 0 , 0 , 0  } }, // #53 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 16, { 43, 0 , 0 , 0 , 0 , 0  } }, // #54 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 7 , { 13, 0 , 0 , 0 , 0 , 0  } }, // #55 [ref=5x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 3 , 0 , 0 , 0 , 0 , 0  } }, // #56 [ref=4x]
  { InstDB::RWInfo::kCategoryGeneric   , 5 , { 3 , 9 , 0 , 0 , 0 , 0  } }, // #57 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 5 , 5 , 58, 0 , 0 , 0  } }, // #58 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 7 , 7 , 58, 0 , 0 , 0  } }, // #59 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 19, 29, 59, 0 , 0 , 0  } }, // #60 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 6 , { 63, 42, 3 , 0 , 0 , 0  } }, // #61 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 6 , { 11, 11, 3 , 64, 0 , 0  } }, // #62 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 17, 29, 30, 0 , 0 , 0  } }, // #63 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 10, { 3 , 0 , 0 , 0 , 0 , 0  } }, // #64 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 2 , { 2 , 3 , 0 , 0 , 0 , 0  } }, // #65 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 3 , { 5 , 5 , 0 , 66, 17, 59 } }, // #66 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 3 , { 5 , 5 , 0 , 67, 17, 59 } }, // #67 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 3 , { 5 , 5 , 0 , 66, 0 , 0  } }, // #68 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 3 , { 5 , 5 , 0 , 67, 0 , 0  } }, // #69 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 31, { 55, 5 , 0 , 0 , 0 , 0  } }, // #70 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 32, { 35, 5 , 0 , 0 , 0 , 0  } }, // #71 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 33, { 48, 3 , 0 , 0 , 0 , 0  } }, // #72 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 15, { 4 , 40, 0 , 0 , 0 , 0  } }, // #73 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 4 , { 4 , 7 , 0 , 0 , 0 , 0  } }, // #74 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 27, { 2 , 13, 0 , 0 , 0 , 0  } }, // #75 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 10, { 69, 0 , 0 , 0 , 0 , 0  } }, // #76 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 4 , { 35, 7 , 0 , 0 , 0 , 0  } }, // #77 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 10, { 64, 0 , 0 , 0 , 0 , 0  } }, // #78 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 11, 0 , 0 , 0 , 0 , 0  } }, // #79 [ref=6x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 16, 71, 29, 0 , 0 , 0  } }, // #80 [ref=5x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 44, 0 , 0 , 0 , 0 , 0  } }, // #81 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 35, 0 , 0 , 0 , 0 , 0  } }, // #82 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 16, 71, 66, 0 , 0 , 0  } }, // #83 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 2 , { 11, 3 , 0 , 0 , 0 , 0  } }, // #84 [ref=16x]
  { InstDB::RWInfo::kCategoryGeneric   , 4 , { 36, 7 , 0 , 0 , 0 , 0  } }, // #85 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 5 , { 37, 9 , 0 , 0 , 0 , 0  } }, // #86 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 72, 0 , 0 , 0 , 0 , 0  } }, // #87 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 31, { 73, 0 , 0 , 0 , 0 , 0  } }, // #88 [ref=30x]
  { InstDB::RWInfo::kCategoryGeneric   , 11, { 2 , 3 , 70, 0 , 0 , 0  } }, // #89 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 34, { 11, 0 , 0 , 0 , 0 , 0  } }, // #90 [ref=3x]
  { InstDB::RWInfo::kCategoryGeneric   , 28, { 44, 0 , 0 , 0 , 0 , 0  } }, // #91 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 16, { 74, 0 , 0 , 0 , 0 , 0  } }, // #92 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 75, 43, 43, 0 , 0 , 0  } }, // #93 [ref=5x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 74, 0 , 0 , 0 , 0 , 0  } }, // #94 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 9 , 59, 17, 0 , 0 , 0  } }, // #95 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 17, { 52, 0 , 0 , 0 , 0 , 0  } }, // #96 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 13, { 75, 43, 43, 43, 43, 5  } }, // #97 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 13, { 4 , 5 , 5 , 5 , 5 , 5  } }, // #98 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 35, { 10, 5 , 7 , 0 , 0 , 0  } }, // #99 [ref=8x]
  { InstDB::RWInfo::kCategoryGeneric   , 36, { 10, 5 , 9 , 0 , 0 , 0  } }, // #100 [ref=9x]
  { InstDB::RWInfo::kCategoryGeneric   , 6 , { 11, 3 , 3 , 3 , 0 , 0  } }, // #101 [ref=3x]
  { InstDB::RWInfo::kCategoryGeneric   , 35, { 11, 5 , 7 , 0 , 0 , 0  } }, // #102 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 36, { 11, 5 , 9 , 0 , 0 , 0  } }, // #103 [ref=1x]
  { InstDB::RWInfo::kCategoryVmov1_2   , 41, { 0 , 0 , 0 , 0 , 0 , 0  } }, // #104 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 35, { 10, 78, 7 , 0 , 0 , 0  } }, // #105 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 42, { 10, 60, 3 , 0 , 0 , 0  } }, // #106 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 42, { 10, 78, 3 , 0 , 0 , 0  } }, // #107 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 36, { 10, 60, 9 , 0 , 0 , 0  } }, // #108 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 43, { 10, 5 , 5 , 0 , 0 , 0  } }, // #109 [ref=9x]
  { InstDB::RWInfo::kCategoryGeneric   , 45, { 10, 77, 0 , 0 , 0 , 0  } }, // #110 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 45, { 10, 3 , 0 , 0 , 0 , 0  } }, // #111 [ref=4x]
  { InstDB::RWInfo::kCategoryGeneric   , 46, { 76, 43, 0 , 0 , 0 , 0  } }, // #112 [ref=4x]
  { InstDB::RWInfo::kCategoryGeneric   , 6 , { 2 , 3 , 3 , 0 , 0 , 0  } }, // #113 [ref=60x]
  { InstDB::RWInfo::kCategoryGeneric   , 35, { 4 , 60, 7 , 0 , 0 , 0  } }, // #114 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 36, { 4 , 78, 9 , 0 , 0 , 0  } }, // #115 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 35, { 6 , 7 , 7 , 0 , 0 , 0  } }, // #116 [ref=11x]
  { InstDB::RWInfo::kCategoryGeneric   , 36, { 8 , 9 , 9 , 0 , 0 , 0  } }, // #117 [ref=11x]
  { InstDB::RWInfo::kCategoryGeneric   , 47, { 11, 3 , 3 , 3 , 0 , 0  } }, // #118 [ref=15x]
  { InstDB::RWInfo::kCategoryGeneric   , 48, { 35, 7 , 7 , 7 , 0 , 0  } }, // #119 [ref=4x]
  { InstDB::RWInfo::kCategoryGeneric   , 49, { 44, 9 , 9 , 9 , 0 , 0  } }, // #120 [ref=4x]
  { InstDB::RWInfo::kCategoryGeneric   , 35, { 26, 7 , 7 , 0 , 0 , 0  } }, // #121 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 36, { 52, 9 , 9 , 0 , 0 , 0  } }, // #122 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 14, { 35, 3 , 0 , 0 , 0 , 0  } }, // #123 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 5 , { 35, 9 , 0 , 0 , 0 , 0  } }, // #124 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 8 , { 2 , 3 , 2 , 0 , 0 , 0  } }, // #125 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 2 , 3 , 2 , 0 , 0 , 0  } }, // #126 [ref=4x]
  { InstDB::RWInfo::kCategoryGeneric   , 18, { 4 , 3 , 4 , 0 , 0 , 0  } }, // #127 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 35, { 10, 60, 7 , 0 , 0 , 0  } }, // #128 [ref=11x]
  { InstDB::RWInfo::kCategoryGeneric   , 36, { 10, 78, 9 , 0 , 0 , 0  } }, // #129 [ref=13x]
  { InstDB::RWInfo::kCategoryGeneric   , 43, { 76, 77, 5 , 0 , 0 , 0  } }, // #130 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 43, { 11, 3 , 5 , 0 , 0 , 0  } }, // #131 [ref=4x]
  { InstDB::RWInfo::kCategoryGeneric   , 50, { 74, 43, 77, 0 , 0 , 0  } }, // #132 [ref=4x]
  { InstDB::RWInfo::kCategoryVmaskmov  , 0 , { 0 , 0 , 0 , 0 , 0 , 0  } }, // #133 [ref=4x]
  { InstDB::RWInfo::kCategoryGeneric   , 12, { 35, 0 , 0 , 0 , 0 , 0  } }, // #134 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 22, 0 , 0 , 0 , 0 , 0  } }, // #135 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 10, 60, 60, 0 , 0 , 0  } }, // #136 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 12, { 10, 7 , 7 , 0 , 0 , 0  } }, // #137 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 10, 7 , 7 , 0 , 0 , 0  } }, // #138 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 12, { 10, 60, 7 , 0 , 0 , 0  } }, // #139 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 10, 60, 7 , 0 , 0 , 0  } }, // #140 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 10, 78, 9 , 0 , 0 , 0  } }, // #141 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 79, 0 , 0 , 0 , 0 , 0  } }, // #142 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 54, { 35, 11, 3 , 3 , 0 , 0  } }, // #143 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 13, { 74, 43, 43, 43, 43, 5  } }, // #144 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 6 , { 35, 3 , 3 , 0 , 0 , 0  } }, // #145 [ref=17x]
  { InstDB::RWInfo::kCategoryGeneric   , 50, { 76, 77, 77, 0 , 0 , 0  } }, // #146 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 22, { 11, 3 , 3 , 0 , 0 , 0  } }, // #147 [ref=4x]
  { InstDB::RWInfo::kCategoryGeneric   , 7 , { 48, 5 , 0 , 0 , 0 , 0  } }, // #148 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 55, { 10, 5 , 40, 0 , 0 , 0  } }, // #149 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 56, { 10, 5 , 13, 0 , 0 , 0  } }, // #150 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 43, { 10, 5 , 5 , 5 , 0 , 0  } }, // #151 [ref=12x]
  { InstDB::RWInfo::kCategoryGeneric   , 61, { 10, 5 , 5 , 5 , 0 , 0  } }, // #152 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 62, { 10, 5 , 5 , 0 , 0 , 0  } }, // #153 [ref=12x]
  { InstDB::RWInfo::kCategoryGeneric   , 22, { 11, 3 , 5 , 0 , 0 , 0  } }, // #154 [ref=9x]
  { InstDB::RWInfo::kCategoryGeneric   , 63, { 11, 3 , 0 , 0 , 0 , 0  } }, // #155 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 59, 17, 29, 0 , 0 , 0  } }, // #156 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 8 , { 3 , 59, 17, 0 , 0 , 0  } }, // #157 [ref=4x]
  { InstDB::RWInfo::kCategoryGeneric   , 8 , { 11, 59, 17, 0 , 0 , 0  } }  // #158 [ref=8x]
};

const InstDB::RWInfoOp InstDB::rwInfoOp[] = {
  { 0x0000000000000000u, 0x0000000000000000u, 0xFF, { 0 }, 0 }, // #0 [ref=15421x]
  { 0x0000000000000003u, 0x0000000000000003u, 0x00, { 0 }, OpRWInfo::kRW | OpRWInfo::kRegPhysId }, // #1 [ref=10x]
  { 0x0000000000000000u, 0x0000000000000000u, 0xFF, { 0 }, OpRWInfo::kRW | OpRWInfo::kZExt }, // #2 [ref=217x]
  { 0x0000000000000000u, 0x0000000000000000u, 0xFF, { 0 }, OpRWInfo::kRead }, // #3 [ref=989x]
  { 0x000000000000FFFFu, 0x000000000000FFFFu, 0xFF, { 0 }, OpRWInfo::kRW | OpRWInfo::kZExt }, // #4 [ref=92x]
  { 0x000000000000FFFFu, 0x0000000000000000u, 0xFF, { 0 }, OpRWInfo::kRead }, // #5 [ref=305x]
  { 0x00000000000000FFu, 0x00000000000000FFu, 0xFF, { 0 }, OpRWInfo::kRW }, // #6 [ref=18x]
  { 0x00000000000000FFu, 0x0000000000000000u, 0xFF, { 0 }, OpRWInfo::kRead }, // #7 [ref=185x]
  { 0x000000000000000Fu, 0x000000000000000Fu, 0xFF, { 0 }, OpRWInfo::kRW }, // #8 [ref=18x]
  { 0x000000000000000Fu, 0x0000000000000000u, 0xFF, { 0 }, OpRWInfo::kRead }, // #9 [ref=133x]
  { 0x0000000000000000u, 0x000000000000FFFFu, 0xFF, { 0 }, OpRWInfo::kWrite | OpRWInfo::kZExt }, // #10 [ref=160x]
  { 0x0000000000000000u, 0x0000000000000000u, 0xFF, { 0 }, OpRWInfo::kWrite | OpRWInfo::kZExt }, // #11 [ref=420x]
  { 0x0000000000000003u, 0x0000000000000003u, 0xFF, { 0 }, OpRWInfo::kRW }, // #12 [ref=1x]
  { 0x0000000000000003u, 0x0000000000000000u, 0xFF, { 0 }, OpRWInfo::kRead }, // #13 [ref=34x]
  { 0x000000000000FFFFu, 0x0000000000000000u, 0x00, { 0 }, OpRWInfo::kRead | OpRWInfo::kRegPhysId }, // #14 [ref=4x]
  { 0x0000000000000000u, 0x0000000000000000u, 0xFF, { 0 }, OpRWInfo::kWrite | OpRWInfo::kZExt | OpRWInfo::kMemBaseWrite | OpRWInfo::kMemIndexWrite }, // #15 [ref=1x]
  { 0x0000000000000000u, 0x000000000000000Fu, 0x02, { 0 }, OpRWInfo::kWrite | OpRWInfo::kZExt | OpRWInfo::kRegPhysId }, // #16 [ref=9x]
  { 0x000000000000000Fu, 0x0000000000000000u, 0x00, { 0 }, OpRWInfo::kRead | OpRWInfo::kRegPhysId }, // #17 [ref=23x]
  { 0x00000000000000FFu, 0x00000000000000FFu, 0x00, { 0 }, OpRWInfo::kRW | OpRWInfo::kZExt | OpRWInfo::kRegPhysId }, // #18 [ref=2x]
  { 0x0000000000000000u, 0x0000000000000000u, 0x00, { 0 }, OpRWInfo::kRead | OpRWInfo::kMemPhysId }, // #19 [ref=3x]
  { 0x0000000000000000u, 0x0000000000000000u, 0x06, { 0 }, OpRWInfo::kRead | OpRWInfo::kMemBaseRW | OpRWInfo::kMemBasePostModify | OpRWInfo::kMemPhysId }, // #20 [ref=3x]
  { 0x0000000000000000u, 0x0000000000000000u, 0x07, { 0 }, OpRWInfo::kRead | OpRWInfo::kMemBaseRW | OpRWInfo::kMemBasePostModify | OpRWInfo::kMemPhysId }, // #21 [ref=2x]
  { 0x0000000000000000u, 0x0000000000000000u, 0x00, { 0 }, OpRWInfo::kRead | OpRWInfo::kRegPhysId }, // #22 [ref=7x]
  { 0x00000000000000FFu, 0x00000000000000FFu, 0x02, { 0 }, OpRWInfo::kRW | OpRWInfo::kZExt | OpRWInfo::kRegPhysId }, // #23 [ref=1x]
  { 0x00000000000000FFu, 0x0000000000000000u, 0x01, { 0 }, OpRWInfo::kRead | OpRWInfo::kRegPhysId }, // #24 [ref=1x]
  { 0x00000000000000FFu, 0x0000000000000000u, 0x03, { 0 }, OpRWInfo::kRead | OpRWInfo::kRegPhysId }, // #25 [ref=1x]
  { 0x00000000000000FFu, 0x00000000000000FFu, 0xFF, { 0 }, OpRWInfo::kRW | OpRWInfo::kZExt }, // #26 [ref=21x]
  { 0x000000000000000Fu, 0x000000000000000Fu, 0x02, { 0 }, OpRWInfo::kRW | OpRWInfo::kZExt | OpRWInfo::kRegPhysId }, // #27 [ref=1x]
  { 0x000000000000000Fu, 0x000000000000000Fu, 0x00, { 0 }, OpRWInfo::kRW | OpRWInfo::kZExt | OpRWInfo::kRegPhysId }, // #28 [ref=4x]
  { 0x000000000000000Fu, 0x0000000000000000u, 0x01, { 0 }, OpRWInfo::kRead | OpRWInfo::kRegPhysId }, // #29 [ref=13x]
  { 0x000000000000000Fu, 0x0000000000000000u, 0x03, { 0 }, OpRWInfo::kRead | OpRWInfo::kRegPhysId }, // #30 [ref=2x]
  { 0x0000000000000000u, 0x000000000000000Fu, 0x03, { 0 }, OpRWInfo::kWrite | OpRWInfo::kZExt | OpRWInfo::kRegPhysId }, // #31 [ref=1x]
  { 0x000000000000000Fu, 0x000000000000000Fu, 0x01, { 0 }, OpRWInfo::kRW | OpRWInfo::kZExt | OpRWInfo::kRegPhysId }, // #32 [ref=1x]
  { 0x0000000000000000u, 0x00000000000000FFu, 0x02, { 0 }, OpRWInfo::kWrite | OpRWInfo::kZExt | OpRWInfo::kRegPhysId }, // #33 [ref=1x]
  { 0x00000000000000FFu, 0x0000000000000000u, 0x00, { 0 }, OpRWInfo::kRead | OpRWInfo::kRegPhysId }, // #34 [ref=1x]
  { 0x0000000000000000u, 0x00000000000000FFu, 0xFF, { 0 }, OpRWInfo::kWrite | OpRWInfo::kZExt }, // #35 [ref=80x]
  { 0x0000000000000000u, 0x00000000000000FFu, 0xFF, { 0 }, OpRWInfo::kWrite }, // #36 [ref=6x]
  { 0x0000000000000000u, 0x000000000000000Fu, 0xFF, { 0 }, OpRWInfo::kWrite }, // #37 [ref=6x]
  { 0x0000000000000000u, 0x0000000000000003u, 0x02, { 0 }, OpRWInfo::kWrite | OpRWInfo::kRegPhysId }, // #38 [ref=1x]
  { 0x0000000000000003u, 0x0000000000000000u, 0x00, { 0 }, OpRWInfo::kRead | OpRWInfo::kRegPhysId }, // #39 [ref=1x]
  { 0x0000000000000001u, 0x0000000000000000u, 0xFF, { 0 }, OpRWInfo::kRead }, // #40 [ref=28x]
  { 0x0000000000000000u, 0x0000000000000000u, 0x02, { 0 }, OpRWInfo::kRW | OpRWInfo::kRegPhysId | OpRWInfo::kZExt }, // #41 [ref=2x]
  { 0x0000000000000000u, 0x0000000000000000u, 0x00, { 0 }, OpRWInfo::kRW | OpRWInfo::kRegPhysId | OpRWInfo::kZExt }, // #42 [ref=3x]
  { 0xFFFFFFFFFFFFFFFFu, 0x0000000000000000u, 0xFF, { 0 }, OpRWInfo::kRead }, // #43 [ref=45x]
  { 0x0000000000000000u, 0x000000000000000Fu, 0xFF, { 0 }, OpRWInfo::kWrite | OpRWInfo::kZExt }, // #44 [ref=30x]
  { 0x00000000000003FFu, 0x00000000000003FFu, 0xFF, { 0 }, OpRWInfo::kRW | OpRWInfo::kZExt }, // #45 [ref=22x]
  { 0x00000000000003FFu, 0x0000000000000000u, 0xFF, { 0 }, OpRWInfo::kRead }, // #46 [ref=13x]
  { 0x0000000000000000u, 0x00000000000003FFu, 0xFF, { 0 }, OpRWInfo::kWrite | OpRWInfo::kZExt }, // #47 [ref=1x]
  { 0x0000000000000000u, 0x0000000000000003u, 0xFF, { 0 }, OpRWInfo::kWrite | OpRWInfo::kZExt }, // #48 [ref=15x]
  { 0x0000000000000000u, 0x0000000000000003u, 0x00, { 0 }, OpRWInfo::kWrite | OpRWInfo::kRegPhysId | OpRWInfo::kZExt }, // #49 [ref=2x]
  { 0x0000000000000000u, 0x0000000000000000u, 0x00, { 0 }, OpRWInfo::kWrite | OpRWInfo::kRegPhysId | OpRWInfo::kZExt }, // #50 [ref=2x]
  { 0x0000000000000003u, 0x0000000000000000u, 0x02, { 0 }, OpRWInfo::kRead | OpRWInfo::kRegPhysId }, // #51 [ref=4x]
  { 0x000000000000000Fu, 0x000000000000000Fu, 0xFF, { 0 }, OpRWInfo::kRW | OpRWInfo::kZExt }, // #52 [ref=4x]
  { 0x0000000000000000u, 0x0000000000000000u, 0x07, { 0 }, OpRWInfo::kWrite | OpRWInfo::kZExt | OpRWInfo::kMemPhysId }, // #53 [ref=1x]
  { 0x0000000000000000u, 0x0000000000000000u, 0x01, { 0 }, OpRWInfo::kRead | OpRWInfo::kRegPhysId }, // #54 [ref=1x]
  { 0x0000000000000000u, 0x0000000000000001u, 0xFF, { 0 }, OpRWInfo::kWrite | OpRWInfo::kZExt }, // #55 [ref=14x]
  { 0x0000000000000000u, 0x0000000000000001u, 0x00, { 0 }, OpRWInfo::kWrite | OpRWInfo::kRegPhysId }, // #56 [ref=1x]
  { 0x0000000000000000u, 0x0000000000000000u, 0x01, { 0 }, OpRWInfo::kRW | OpRWInfo::kRegPhysId | OpRWInfo::kZExt }, // #57 [ref=3x]
  { 0x0000000000000000u, 0x0000000000000000u, 0x07, { 0 }, OpRWInfo::kRW | OpRWInfo::kZExt | OpRWInfo::kMemPhysId }, // #58 [ref=3x]
  { 0x000000000000000Fu, 0x0000000000000000u, 0x02, { 0 }, OpRWInfo::kRead | OpRWInfo::kRegPhysId }, // #59 [ref=22x]
  { 0x000000000000FF00u, 0x0000000000000000u, 0xFF, { 0 }, OpRWInfo::kRead }, // #60 [ref=23x]
  { 0x0000000000000000u, 0x000000000000FF00u, 0xFF, { 0 }, OpRWInfo::kWrite }, // #61 [ref=1x]
  { 0x0000000000000000u, 0x0000000000000000u, 0x07, { 0 }, OpRWInfo::kWrite | OpRWInfo::kZExt | OpRWInfo::kMemBaseRW | OpRWInfo::kMemBasePostModify | OpRWInfo::kMemPhysId }, // #62 [ref=2x]
  { 0x0000000000000000u, 0x0000000000000000u, 0x02, { 0 }, OpRWInfo::kWrite | OpRWInfo::kRegPhysId | OpRWInfo::kZExt }, // #63 [ref=1x]
  { 0x0000000000000000u, 0x0000000000000000u, 0x02, { 0 }, OpRWInfo::kRead | OpRWInfo::kRegPhysId }, // #64 [ref=2x]
  { 0x0000000000000000u, 0x0000000000000000u, 0x06, { 0 }, OpRWInfo::kRead | OpRWInfo::kMemPhysId }, // #65 [ref=1x]
  { 0x0000000000000000u, 0x000000000000000Fu, 0x01, { 0 }, OpRWInfo::kWrite | OpRWInfo::kZExt | OpRWInfo::kRegPhysId }, // #66 [ref=5x]
  { 0x0000000000000000u, 0x000000000000FFFFu, 0x00, { 0 }, OpRWInfo::kWrite | OpRWInfo::kZExt | OpRWInfo::kRegPhysId }, // #67 [ref=4x]
  { 0x0000000000000000u, 0x0000000000000007u, 0xFF, { 0 }, OpRWInfo::kWrite | OpRWInfo::kZExt }, // #68 [ref=2x]
  { 0x0000000000000000u, 0x0000000000000000u, 0x04, { 0 }, OpRWInfo::kWrite | OpRWInfo::kZExt | OpRWInfo::kRegPhysId }, // #69 [ref=1x]
  { 0x0000000000000001u, 0x0000000000000000u, 0x01, { 0 }, OpRWInfo::kRead | OpRWInfo::kRegPhysId }, // #70 [ref=10x]
  { 0x0000000000000000u, 0x000000000000000Fu, 0x00, { 0 }, OpRWInfo::kWrite | OpRWInfo::kZExt | OpRWInfo::kRegPhysId }, // #71 [ref=7x]
  { 0x0000000000000001u, 0x0000000000000000u, 0x00, { 0 }, OpRWInfo::kRead | OpRWInfo::kRegPhysId }, // #72 [ref=1x]
  { 0x0000000000000000u, 0x0000000000000001u, 0xFF, { 0 }, OpRWInfo::kWrite }, // #73 [ref=30x]
  { 0x0000000000000000u, 0xFFFFFFFFFFFFFFFFu, 0xFF, { 0 }, OpRWInfo::kWrite | OpRWInfo::kZExt }, // #74 [ref=20x]
  { 0xFFFFFFFFFFFFFFFFu, 0xFFFFFFFFFFFFFFFFu, 0xFF, { 0 }, OpRWInfo::kRW | OpRWInfo::kZExt }, // #75 [ref=7x]
  { 0x0000000000000000u, 0x00000000FFFFFFFFu, 0xFF, { 0 }, OpRWInfo::kWrite | OpRWInfo::kZExt }, // #76 [ref=10x]
  { 0x00000000FFFFFFFFu, 0x0000000000000000u, 0xFF, { 0 }, OpRWInfo::kRead }, // #77 [ref=16x]
  { 0x000000000000FFF0u, 0x0000000000000000u, 0xFF, { 0 }, OpRWInfo::kRead }, // #78 [ref=18x]
  { 0x0000000000000000u, 0x0000000000000000u, 0x00, { 0 }, OpRWInfo::kRW | OpRWInfo::kZExt | OpRWInfo::kRegPhysId }  // #79 [ref=1x]
};

const InstDB::RWInfoRm InstDB::rwInfoRm[] = {
  { InstDB::RWInfoRm::kCategoryNone      , 0x00, 0 , 0, 0 }, // #0 [ref=1880x]
  { InstDB::RWInfoRm::kCategoryConsistent, 0x03, 0 , InstDB::RWInfoRm::kFlagAmbiguous, 0 }, // #1 [ref=8x]
  { InstDB::RWInfoRm::kCategoryConsistent, 0x02, 0 , 0, 0 }, // #2 [ref=194x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x02, 16, 0, 0 }, // #3 [ref=122x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x02, 8 , 0, 0 }, // #4 [ref=66x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x02, 4 , 0, 0 }, // #5 [ref=33x]
  { InstDB::RWInfoRm::kCategoryConsistent, 0x04, 0 , 0, 0 }, // #6 [ref=270x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x01, 2 , 0, 0 }, // #7 [ref=9x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x00, 0 , 0, 0 }, // #8 [ref=63x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x03, 0 , 0, 0 }, // #9 [ref=1x]
  { InstDB::RWInfoRm::kCategoryConsistent, 0x01, 0 , InstDB::RWInfoRm::kFlagAmbiguous, 0 }, // #10 [ref=21x]
  { InstDB::RWInfoRm::kCategoryConsistent, 0x01, 0 , 0, 0 }, // #11 [ref=14x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x00, 8 , 0, 0 }, // #12 [ref=22x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x00, 16, 0, 0 }, // #13 [ref=21x]
  { InstDB::RWInfoRm::kCategoryConsistent, 0x02, 0 , InstDB::RWInfoRm::kFlagAmbiguous, 0 }, // #14 [ref=15x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x02, 1 , 0, 0 }, // #15 [ref=5x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x00, 64, 0, 0 }, // #16 [ref=5x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x01, 4 , 0, 0 }, // #17 [ref=8x]
  { InstDB::RWInfoRm::kCategoryNone      , 0x00, 0 , InstDB::RWInfoRm::kFlagAmbiguous, 0 }, // #18 [ref=22x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x00, 10, 0, 0 }, // #19 [ref=2x]
  { InstDB::RWInfoRm::kCategoryNone      , 0x01, 0 , InstDB::RWInfoRm::kFlagAmbiguous, 0 }, // #20 [ref=5x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x00, 2 , 0, 0 }, // #21 [ref=3x]
  { InstDB::RWInfoRm::kCategoryConsistent, 0x06, 0 , 0, 0 }, // #22 [ref=14x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x03, 1 , 0, 0 }, // #23 [ref=1x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x03, 4 , 0, 0 }, // #24 [ref=4x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x03, 8 , 0, 0 }, // #25 [ref=3x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x03, 2 , 0, 0 }, // #26 [ref=1x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x02, 2 , 0, 0 }, // #27 [ref=6x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x00, 4 , 0, 0 }, // #28 [ref=6x]
  { InstDB::RWInfoRm::kCategoryNone      , 0x03, 0 , InstDB::RWInfoRm::kFlagAmbiguous, 0 }, // #29 [ref=1x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x03, 16, 0, 0 }, // #30 [ref=6x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x01, 1 , 0, 0 }, // #31 [ref=32x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x01, 8 , 0, 0 }, // #32 [ref=4x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x01, 2 , 0, Features::kSSE4_1 }, // #33 [ref=1x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x01, 2 , InstDB::RWInfoRm::kFlagAmbiguous, 0 }, // #34 [ref=3x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x04, 8 , 0, 0 }, // #35 [ref=34x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x04, 4 , 0, 0 }, // #36 [ref=37x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x00, 32, 0, 0 }, // #37 [ref=4x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x02, 8 , InstDB::RWInfoRm::kFlagAmbiguous, 0 }, // #38 [ref=1x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x02, 4 , InstDB::RWInfoRm::kFlagAmbiguous, 0 }, // #39 [ref=1x]
  { InstDB::RWInfoRm::kCategoryHalf      , 0x02, 0 , 0, 0 }, // #40 [ref=14x]
  { InstDB::RWInfoRm::kCategoryHalf      , 0x01, 0 , 0, 0 }, // #41 [ref=10x]
  { InstDB::RWInfoRm::kCategoryConsistent, 0x04, 0 , InstDB::RWInfoRm::kFlagAmbiguous, 0 }, // #42 [ref=4x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x04, 16, 0, 0 }, // #43 [ref=27x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x02, 64, 0, 0 }, // #44 [ref=6x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x01, 16, 0, 0 }, // #45 [ref=6x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x01, 32, 0, 0 }, // #46 [ref=4x]
  { InstDB::RWInfoRm::kCategoryConsistent, 0x0C, 0 , 0, 0 }, // #47 [ref=15x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x0C, 8 , 0, 0 }, // #48 [ref=4x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x0C, 4 , 0, 0 }, // #49 [ref=4x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x04, 32, 0, 0 }, // #50 [ref=6x]
  { InstDB::RWInfoRm::kCategoryConsistent, 0x03, 0 , 0, 0 }, // #51 [ref=13x]
  { InstDB::RWInfoRm::kCategoryNone      , 0x02, 0 , 0, 0 }, // #52 [ref=1x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x03, 8 , InstDB::RWInfoRm::kFlagAmbiguous, 0 }, // #53 [ref=1x]
  { InstDB::RWInfoRm::kCategoryConsistent, 0x08, 0 , 0, 0 }, // #54 [ref=2x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x04, 1 , 0, 0 }, // #55 [ref=1x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x04, 2 , 0, 0 }, // #56 [ref=1x]
  { InstDB::RWInfoRm::kCategoryQuarter   , 0x01, 0 , 0, 0 }, // #57 [ref=6x]
  { InstDB::RWInfoRm::kCategoryEighth    , 0x01, 0 , 0, 0 }, // #58 [ref=3x]
  { InstDB::RWInfoRm::kCategoryQuarter   , 0x02, 0 , 0, 0 }, // #59 [ref=4x]
  { InstDB::RWInfoRm::kCategoryEighth    , 0x02, 0 , 0, 0 }, // #60 [ref=2x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x0C, 16, 0, 0 }, // #61 [ref=1x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x06, 16, 0, 0 }, // #62 [ref=12x]
  { InstDB::RWInfoRm::kCategoryConsistent, 0x02, 0 , 0, Features::kAVX512_BW }  // #63 [ref=2x]
};
// ----------------------------------------------------------------------------
// ${InstRWInfoTable:End}

// ============================================================================
// [asmjit::x86::InstDB - Unit]
// ============================================================================

#if defined(ASMJIT_TEST)
UNIT(x86_inst_db) {
  INFO("Checking validity of Inst enums");

  // Cross-validate prefixes.
  EXPECT(Inst::kOptionRex  == 0x40000000u, "REX prefix must be at 0x40000000");
  EXPECT(Inst::kOptionVex3 == 0x00000400u, "VEX3 prefix must be at 0x00000400");
  EXPECT(Inst::kOptionEvex == 0x00001000u, "EVEX prefix must be at 0x00001000");

  // These could be combined together to form a valid REX prefix, they must match.
  EXPECT(uint32_t(Inst::kOptionOpCodeB) == uint32_t(Opcode::kB), "Opcode::kB must match Inst::kOptionOpCodeB");
  EXPECT(uint32_t(Inst::kOptionOpCodeX) == uint32_t(Opcode::kX), "Opcode::kX must match Inst::kOptionOpCodeX");
  EXPECT(uint32_t(Inst::kOptionOpCodeR) == uint32_t(Opcode::kR), "Opcode::kR must match Inst::kOptionOpCodeR");
  EXPECT(uint32_t(Inst::kOptionOpCodeW) == uint32_t(Opcode::kW), "Opcode::kW must match Inst::kOptionOpCodeW");

  uint32_t rex_rb = (Opcode::kR >> Opcode::kREX_Shift) | (Opcode::kB >> Opcode::kREX_Shift) | 0x40;
  uint32_t rex_rw = (Opcode::kR >> Opcode::kREX_Shift) | (Opcode::kW >> Opcode::kREX_Shift) | 0x40;

  EXPECT(rex_rb == 0x45, "Opcode::kR|B must form a valid REX prefix (0x45) if combined with 0x40");
  EXPECT(rex_rw == 0x4C, "Opcode::kR|W must form a valid REX prefix (0x4C) if combined with 0x40");
}
#endif

ASMJIT_END_SUB_NAMESPACE

#endif // ASMJIT_BUILD_X86
