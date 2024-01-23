// This file is part of AsmJit project <https://asmjit.com>
//
// See asmjit.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

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
  INST(Aadd             , X86Mr              , O(000F38,FC,_,_,_,_,_,_  ), 0                         , 1  , 0  , 3  , 2  ), // #3
  INST(Aam              , X86I_xAX           , O(000000,D4,_,_,_,_,_,_  ), 0                         , 0  , 0  , 2  , 1  ), // #4
  INST(Aand             , X86Mr              , O(660F38,FC,_,_,_,_,_,_  ), 0                         , 2  , 0  , 3  , 2  ), // #5
  INST(Aas              , X86Op_xAX          , O(000000,3F,_,_,_,_,_,_  ), 0                         , 0  , 0  , 1  , 1  ), // #6
  INST(Adc              , X86Arith           , O(000000,10,2,_,x,_,_,_  ), 0                         , 3  , 0  , 4  , 3  ), // #7
  INST(Adcx             , X86Rm              , O(660F38,F6,_,_,x,_,_,_  ), 0                         , 2  , 0  , 5  , 4  ), // #8
  INST(Add              , X86Arith           , O(000000,00,0,_,x,_,_,_  ), 0                         , 0  , 0  , 4  , 1  ), // #9
  INST(Addpd            , ExtRm              , O(660F00,58,_,_,_,_,_,_  ), 0                         , 4  , 0  , 6  , 5  ), // #10
  INST(Addps            , ExtRm              , O(000F00,58,_,_,_,_,_,_  ), 0                         , 5  , 0  , 6  , 6  ), // #11
  INST(Addsd            , ExtRm              , O(F20F00,58,_,_,_,_,_,_  ), 0                         , 6  , 0  , 7  , 5  ), // #12
  INST(Addss            , ExtRm              , O(F30F00,58,_,_,_,_,_,_  ), 0                         , 7  , 0  , 8  , 6  ), // #13
  INST(Addsubpd         , ExtRm              , O(660F00,D0,_,_,_,_,_,_  ), 0                         , 4  , 0  , 6  , 7  ), // #14
  INST(Addsubps         , ExtRm              , O(F20F00,D0,_,_,_,_,_,_  ), 0                         , 6  , 0  , 6  , 7  ), // #15
  INST(Adox             , X86Rm              , O(F30F38,F6,_,_,x,_,_,_  ), 0                         , 8  , 0  , 5  , 8  ), // #16
  INST(Aesdec           , ExtRm              , O(660F38,DE,_,_,_,_,_,_  ), 0                         , 2  , 0  , 6  , 9  ), // #17
  INST(Aesdeclast       , ExtRm              , O(660F38,DF,_,_,_,_,_,_  ), 0                         , 2  , 0  , 6  , 9  ), // #18
  INST(Aesenc           , ExtRm              , O(660F38,DC,_,_,_,_,_,_  ), 0                         , 2  , 0  , 6  , 9  ), // #19
  INST(Aesenclast       , ExtRm              , O(660F38,DD,_,_,_,_,_,_  ), 0                         , 2  , 0  , 6  , 9  ), // #20
  INST(Aesimc           , ExtRm              , O(660F38,DB,_,_,_,_,_,_  ), 0                         , 2  , 0  , 6  , 9  ), // #21
  INST(Aeskeygenassist  , ExtRmi             , O(660F3A,DF,_,_,_,_,_,_  ), 0                         , 9  , 0  , 9  , 9  ), // #22
  INST(And              , X86Arith           , O(000000,20,4,_,x,_,_,_  ), 0                         , 10 , 0  , 10 , 1  ), // #23
  INST(Andn             , VexRvm_Wx          , V(000F38,F2,_,0,x,_,_,_  ), 0                         , 11 , 0  , 11 , 10 ), // #24
  INST(Andnpd           , ExtRm              , O(660F00,55,_,_,_,_,_,_  ), 0                         , 4  , 0  , 6  , 5  ), // #25
  INST(Andnps           , ExtRm              , O(000F00,55,_,_,_,_,_,_  ), 0                         , 5  , 0  , 6  , 6  ), // #26
  INST(Andpd            , ExtRm              , O(660F00,54,_,_,_,_,_,_  ), 0                         , 4  , 0  , 12 , 5  ), // #27
  INST(Andps            , ExtRm              , O(000F00,54,_,_,_,_,_,_  ), 0                         , 5  , 0  , 12 , 6  ), // #28
  INST(Aor              , X86Mr              , O(F20F38,FC,_,_,_,_,_,_  ), 0                         , 12 , 0  , 3  , 2  ), // #29
  INST(Arpl             , X86Mr_NoSize       , O(000000,63,_,_,_,_,_,_  ), 0                         , 0  , 0  , 13 , 11 ), // #30
  INST(Axor             , X86Mr              , O(F30F38,FC,_,_,_,_,_,_  ), 0                         , 8  , 0  , 3  , 2  ), // #31
  INST(Bextr            , VexRmv_Wx          , V(000F38,F7,_,0,x,_,_,_  ), 0                         , 11 , 0  , 14 , 10 ), // #32
  INST(Blcfill          , VexVm_Wx           , V(XOP_M9,01,1,0,x,_,_,_  ), 0                         , 13 , 0  , 15 , 12 ), // #33
  INST(Blci             , VexVm_Wx           , V(XOP_M9,02,6,0,x,_,_,_  ), 0                         , 14 , 0  , 15 , 12 ), // #34
  INST(Blcic            , VexVm_Wx           , V(XOP_M9,01,5,0,x,_,_,_  ), 0                         , 15 , 0  , 15 , 12 ), // #35
  INST(Blcmsk           , VexVm_Wx           , V(XOP_M9,02,1,0,x,_,_,_  ), 0                         , 13 , 0  , 15 , 12 ), // #36
  INST(Blcs             , VexVm_Wx           , V(XOP_M9,01,3,0,x,_,_,_  ), 0                         , 16 , 0  , 15 , 12 ), // #37
  INST(Blendpd          , ExtRmi             , O(660F3A,0D,_,_,_,_,_,_  ), 0                         , 9  , 0  , 9  , 13 ), // #38
  INST(Blendps          , ExtRmi             , O(660F3A,0C,_,_,_,_,_,_  ), 0                         , 9  , 0  , 9  , 13 ), // #39
  INST(Blendvpd         , ExtRm_XMM0         , O(660F38,15,_,_,_,_,_,_  ), 0                         , 2  , 0  , 16 , 13 ), // #40
  INST(Blendvps         , ExtRm_XMM0         , O(660F38,14,_,_,_,_,_,_  ), 0                         , 2  , 0  , 16 , 13 ), // #41
  INST(Blsfill          , VexVm_Wx           , V(XOP_M9,01,2,0,x,_,_,_  ), 0                         , 17 , 0  , 15 , 12 ), // #42
  INST(Blsi             , VexVm_Wx           , V(000F38,F3,3,0,x,_,_,_  ), 0                         , 18 , 0  , 15 , 10 ), // #43
  INST(Blsic            , VexVm_Wx           , V(XOP_M9,01,6,0,x,_,_,_  ), 0                         , 14 , 0  , 15 , 12 ), // #44
  INST(Blsmsk           , VexVm_Wx           , V(000F38,F3,2,0,x,_,_,_  ), 0                         , 19 , 0  , 15 , 10 ), // #45
  INST(Blsr             , VexVm_Wx           , V(000F38,F3,1,0,x,_,_,_  ), 0                         , 20 , 0  , 15 , 10 ), // #46
  INST(Bndcl            , X86Rm              , O(F30F00,1A,_,_,_,_,_,_  ), 0                         , 7  , 0  , 17 , 14 ), // #47
  INST(Bndcn            , X86Rm              , O(F20F00,1B,_,_,_,_,_,_  ), 0                         , 6  , 0  , 17 , 14 ), // #48
  INST(Bndcu            , X86Rm              , O(F20F00,1A,_,_,_,_,_,_  ), 0                         , 6  , 0  , 17 , 14 ), // #49
  INST(Bndldx           , X86Rm              , O(000F00,1A,_,_,_,_,_,_  ), 0                         , 5  , 0  , 18 , 14 ), // #50
  INST(Bndmk            , X86Rm              , O(F30F00,1B,_,_,_,_,_,_  ), 0                         , 7  , 0  , 19 , 14 ), // #51
  INST(Bndmov           , X86Bndmov          , O(660F00,1A,_,_,_,_,_,_  ), O(660F00,1B,_,_,_,_,_,_  ), 4  , 1  , 20 , 14 ), // #52
  INST(Bndstx           , X86Mr              , O(000F00,1B,_,_,_,_,_,_  ), 0                         , 5  , 0  , 21 , 14 ), // #53
  INST(Bound            , X86Rm              , O(000000,62,_,_,_,_,_,_  ), 0                         , 0  , 0  , 22 , 0  ), // #54
  INST(Bsf              , X86Rm              , O(000F00,BC,_,_,x,_,_,_  ), 0                         , 5  , 0  , 23 , 1  ), // #55
  INST(Bsr              , X86Rm              , O(000F00,BD,_,_,x,_,_,_  ), 0                         , 5  , 0  , 23 , 1  ), // #56
  INST(Bswap            , X86Bswap           , O(000F00,C8,_,_,x,_,_,_  ), 0                         , 5  , 0  , 24 , 0  ), // #57
  INST(Bt               , X86Bt              , O(000F00,A3,_,_,x,_,_,_  ), O(000F00,BA,4,_,x,_,_,_  ), 5  , 2  , 25 , 15 ), // #58
  INST(Btc              , X86Bt              , O(000F00,BB,_,_,x,_,_,_  ), O(000F00,BA,7,_,x,_,_,_  ), 5  , 3  , 26 , 15 ), // #59
  INST(Btr              , X86Bt              , O(000F00,B3,_,_,x,_,_,_  ), O(000F00,BA,6,_,x,_,_,_  ), 5  , 4  , 26 , 15 ), // #60
  INST(Bts              , X86Bt              , O(000F00,AB,_,_,x,_,_,_  ), O(000F00,BA,5,_,x,_,_,_  ), 5  , 5  , 26 , 15 ), // #61
  INST(Bzhi             , VexRmv_Wx          , V(000F38,F5,_,0,x,_,_,_  ), 0                         , 11 , 0  , 14 , 16 ), // #62
  INST(Call             , X86Call            , O(000000,FF,2,_,_,_,_,_  ), 0                         , 3  , 0  , 27 , 1  ), // #63
  INST(Cbw              , X86Op_xAX          , O(660000,98,_,_,_,_,_,_  ), 0                         , 21 , 0  , 28 , 0  ), // #64
  INST(Cdq              , X86Op_xDX_xAX      , O(000000,99,_,_,_,_,_,_  ), 0                         , 0  , 0  , 29 , 0  ), // #65
  INST(Cdqe             , X86Op_xAX          , O(000000,98,_,_,1,_,_,_  ), 0                         , 22 , 0  , 30 , 0  ), // #66
  INST(Clac             , X86Op              , O(000F01,CA,_,_,_,_,_,_  ), 0                         , 23 , 0  , 31 , 17 ), // #67
  INST(Clc              , X86Op              , O(000000,F8,_,_,_,_,_,_  ), 0                         , 0  , 0  , 31 , 18 ), // #68
  INST(Cld              , X86Op              , O(000000,FC,_,_,_,_,_,_  ), 0                         , 0  , 0  , 31 , 19 ), // #69
  INST(Cldemote         , X86M_Only          , O(000F00,1C,0,_,_,_,_,_  ), 0                         , 5  , 0  , 32 , 20 ), // #70
  INST(Clflush          , X86M_Only          , O(000F00,AE,7,_,_,_,_,_  ), 0                         , 24 , 0  , 32 , 21 ), // #71
  INST(Clflushopt       , X86M_Only          , O(660F00,AE,7,_,_,_,_,_  ), 0                         , 25 , 0  , 32 , 22 ), // #72
  INST(Clgi             , X86Op              , O(000F01,DD,_,_,_,_,_,_  ), 0                         , 23 , 0  , 31 , 23 ), // #73
  INST(Cli              , X86Op              , O(000000,FA,_,_,_,_,_,_  ), 0                         , 0  , 0  , 31 , 24 ), // #74
  INST(Clrssbsy         , X86M_Only          , O(F30F00,AE,6,_,_,_,_,_  ), 0                         , 26 , 0  , 33 , 25 ), // #75
  INST(Clts             , X86Op              , O(000F00,06,_,_,_,_,_,_  ), 0                         , 5  , 0  , 31 , 0  ), // #76
  INST(Clui             , X86Op              , O(F30F01,EE,_,_,_,_,_,_  ), 0                         , 27 , 0  , 34 , 26 ), // #77
  INST(Clwb             , X86M_Only          , O(660F00,AE,6,_,_,_,_,_  ), 0                         , 28 , 0  , 32 , 27 ), // #78
  INST(Clzero           , X86Op_MemZAX       , O(000F01,FC,_,_,_,_,_,_  ), 0                         , 23 , 0  , 35 , 28 ), // #79
  INST(Cmc              , X86Op              , O(000000,F5,_,_,_,_,_,_  ), 0                         , 0  , 0  , 31 , 29 ), // #80
  INST(Cmova            , X86Rm              , O(000F00,47,_,_,x,_,_,_  ), 0                         , 5  , 0  , 23 , 30 ), // #81
  INST(Cmovae           , X86Rm              , O(000F00,43,_,_,x,_,_,_  ), 0                         , 5  , 0  , 23 , 31 ), // #82
  INST(Cmovb            , X86Rm              , O(000F00,42,_,_,x,_,_,_  ), 0                         , 5  , 0  , 23 , 31 ), // #83
  INST(Cmovbe           , X86Rm              , O(000F00,46,_,_,x,_,_,_  ), 0                         , 5  , 0  , 23 , 30 ), // #84
  INST(Cmovc            , X86Rm              , O(000F00,42,_,_,x,_,_,_  ), 0                         , 5  , 0  , 23 , 31 ), // #85
  INST(Cmove            , X86Rm              , O(000F00,44,_,_,x,_,_,_  ), 0                         , 5  , 0  , 23 , 32 ), // #86
  INST(Cmovg            , X86Rm              , O(000F00,4F,_,_,x,_,_,_  ), 0                         , 5  , 0  , 23 , 33 ), // #87
  INST(Cmovge           , X86Rm              , O(000F00,4D,_,_,x,_,_,_  ), 0                         , 5  , 0  , 23 , 34 ), // #88
  INST(Cmovl            , X86Rm              , O(000F00,4C,_,_,x,_,_,_  ), 0                         , 5  , 0  , 23 , 34 ), // #89
  INST(Cmovle           , X86Rm              , O(000F00,4E,_,_,x,_,_,_  ), 0                         , 5  , 0  , 23 , 33 ), // #90
  INST(Cmovna           , X86Rm              , O(000F00,46,_,_,x,_,_,_  ), 0                         , 5  , 0  , 23 , 30 ), // #91
  INST(Cmovnae          , X86Rm              , O(000F00,42,_,_,x,_,_,_  ), 0                         , 5  , 0  , 23 , 31 ), // #92
  INST(Cmovnb           , X86Rm              , O(000F00,43,_,_,x,_,_,_  ), 0                         , 5  , 0  , 23 , 31 ), // #93
  INST(Cmovnbe          , X86Rm              , O(000F00,47,_,_,x,_,_,_  ), 0                         , 5  , 0  , 23 , 30 ), // #94
  INST(Cmovnc           , X86Rm              , O(000F00,43,_,_,x,_,_,_  ), 0                         , 5  , 0  , 23 , 31 ), // #95
  INST(Cmovne           , X86Rm              , O(000F00,45,_,_,x,_,_,_  ), 0                         , 5  , 0  , 23 , 32 ), // #96
  INST(Cmovng           , X86Rm              , O(000F00,4E,_,_,x,_,_,_  ), 0                         , 5  , 0  , 23 , 33 ), // #97
  INST(Cmovnge          , X86Rm              , O(000F00,4C,_,_,x,_,_,_  ), 0                         , 5  , 0  , 23 , 34 ), // #98
  INST(Cmovnl           , X86Rm              , O(000F00,4D,_,_,x,_,_,_  ), 0                         , 5  , 0  , 23 , 34 ), // #99
  INST(Cmovnle          , X86Rm              , O(000F00,4F,_,_,x,_,_,_  ), 0                         , 5  , 0  , 23 , 33 ), // #100
  INST(Cmovno           , X86Rm              , O(000F00,41,_,_,x,_,_,_  ), 0                         , 5  , 0  , 23 , 35 ), // #101
  INST(Cmovnp           , X86Rm              , O(000F00,4B,_,_,x,_,_,_  ), 0                         , 5  , 0  , 23 , 36 ), // #102
  INST(Cmovns           , X86Rm              , O(000F00,49,_,_,x,_,_,_  ), 0                         , 5  , 0  , 23 , 37 ), // #103
  INST(Cmovnz           , X86Rm              , O(000F00,45,_,_,x,_,_,_  ), 0                         , 5  , 0  , 23 , 32 ), // #104
  INST(Cmovo            , X86Rm              , O(000F00,40,_,_,x,_,_,_  ), 0                         , 5  , 0  , 23 , 35 ), // #105
  INST(Cmovp            , X86Rm              , O(000F00,4A,_,_,x,_,_,_  ), 0                         , 5  , 0  , 23 , 36 ), // #106
  INST(Cmovpe           , X86Rm              , O(000F00,4A,_,_,x,_,_,_  ), 0                         , 5  , 0  , 23 , 36 ), // #107
  INST(Cmovpo           , X86Rm              , O(000F00,4B,_,_,x,_,_,_  ), 0                         , 5  , 0  , 23 , 36 ), // #108
  INST(Cmovs            , X86Rm              , O(000F00,48,_,_,x,_,_,_  ), 0                         , 5  , 0  , 23 , 37 ), // #109
  INST(Cmovz            , X86Rm              , O(000F00,44,_,_,x,_,_,_  ), 0                         , 5  , 0  , 23 , 32 ), // #110
  INST(Cmp              , X86Arith           , O(000000,38,7,_,x,_,_,_  ), 0                         , 29 , 0  , 36 , 1  ), // #111
  INST(Cmpbexadd        , VexMvr_Wx          , V(660F38,E6,_,0,x,_,_,_  ), 0                         , 30 , 0  , 37 , 38 ), // #112
  INST(Cmpbxadd         , VexMvr_Wx          , V(660F38,E2,_,0,x,_,_,_  ), 0                         , 30 , 0  , 37 , 38 ), // #113
  INST(Cmplexadd        , VexMvr_Wx          , V(660F38,EE,_,0,x,_,_,_  ), 0                         , 30 , 0  , 37 , 38 ), // #114
  INST(Cmplxadd         , VexMvr_Wx          , V(660F38,EC,_,0,x,_,_,_  ), 0                         , 30 , 0  , 37 , 38 ), // #115
  INST(Cmpnbexadd       , VexMvr_Wx          , V(660F38,E7,_,0,x,_,_,_  ), 0                         , 30 , 0  , 37 , 38 ), // #116
  INST(Cmpnbxadd        , VexMvr_Wx          , V(660F38,E3,_,0,x,_,_,_  ), 0                         , 30 , 0  , 37 , 38 ), // #117
  INST(Cmpnlexadd       , VexMvr_Wx          , V(660F38,EF,_,0,x,_,_,_  ), 0                         , 30 , 0  , 37 , 38 ), // #118
  INST(Cmpnlxadd        , VexMvr_Wx          , V(660F38,ED,_,0,x,_,_,_  ), 0                         , 30 , 0  , 37 , 38 ), // #119
  INST(Cmpnoxadd        , VexMvr_Wx          , V(660F38,E1,_,0,x,_,_,_  ), 0                         , 30 , 0  , 37 , 38 ), // #120
  INST(Cmpnpxadd        , VexMvr_Wx          , V(660F38,EB,_,0,x,_,_,_  ), 0                         , 30 , 0  , 37 , 38 ), // #121
  INST(Cmpnsxadd        , VexMvr_Wx          , V(660F38,E9,_,0,x,_,_,_  ), 0                         , 30 , 0  , 37 , 38 ), // #122
  INST(Cmpnzxadd        , VexMvr_Wx          , V(660F38,E5,_,0,x,_,_,_  ), 0                         , 30 , 0  , 37 , 38 ), // #123
  INST(Cmpoxadd         , VexMvr_Wx          , V(660F38,E0,_,0,x,_,_,_  ), 0                         , 30 , 0  , 37 , 38 ), // #124
  INST(Cmppd            , ExtRmi             , O(660F00,C2,_,_,_,_,_,_  ), 0                         , 4  , 0  , 9  , 5  ), // #125
  INST(Cmpps            , ExtRmi             , O(000F00,C2,_,_,_,_,_,_  ), 0                         , 5  , 0  , 9  , 6  ), // #126
  INST(Cmppxadd         , VexMvr_Wx          , V(660F38,EA,_,0,x,_,_,_  ), 0                         , 30 , 0  , 37 , 38 ), // #127
  INST(Cmps             , X86StrMm           , O(000000,A6,_,_,_,_,_,_  ), 0                         , 0  , 0  , 38 , 39 ), // #128
  INST(Cmpsd            , ExtRmi             , O(F20F00,C2,_,_,_,_,_,_  ), 0                         , 6  , 0  , 39 , 5  ), // #129
  INST(Cmpss            , ExtRmi             , O(F30F00,C2,_,_,_,_,_,_  ), 0                         , 7  , 0  , 40 , 6  ), // #130
  INST(Cmpsxadd         , VexMvr_Wx          , V(660F38,E8,_,0,x,_,_,_  ), 0                         , 30 , 0  , 37 , 38 ), // #131
  INST(Cmpxchg          , X86Cmpxchg         , O(000F00,B0,_,_,x,_,_,_  ), 0                         , 5  , 0  , 41 , 40 ), // #132
  INST(Cmpxchg16b       , X86Cmpxchg8b_16b   , O(000F00,C7,1,_,1,_,_,_  ), 0                         , 31 , 0  , 42 , 41 ), // #133
  INST(Cmpxchg8b        , X86Cmpxchg8b_16b   , O(000F00,C7,1,_,_,_,_,_  ), 0                         , 32 , 0  , 43 , 42 ), // #134
  INST(Cmpzxadd         , VexMvr_Wx          , V(660F38,E4,_,0,x,_,_,_  ), 0                         , 30 , 0  , 37 , 38 ), // #135
  INST(Comisd           , ExtRm              , O(660F00,2F,_,_,_,_,_,_  ), 0                         , 4  , 0  , 7  , 43 ), // #136
  INST(Comiss           , ExtRm              , O(000F00,2F,_,_,_,_,_,_  ), 0                         , 5  , 0  , 8  , 44 ), // #137
  INST(Cpuid            , X86Op              , O(000F00,A2,_,_,_,_,_,_  ), 0                         , 5  , 0  , 44 , 45 ), // #138
  INST(Cqo              , X86Op_xDX_xAX      , O(000000,99,_,_,1,_,_,_  ), 0                         , 22 , 0  , 45 , 0  ), // #139
  INST(Crc32            , X86Crc             , O(F20F38,F0,_,_,x,_,_,_  ), 0                         , 12 , 0  , 46 , 46 ), // #140
  INST(Cvtdq2pd         , ExtRm              , O(F30F00,E6,_,_,_,_,_,_  ), 0                         , 7  , 0  , 7  , 5  ), // #141
  INST(Cvtdq2ps         , ExtRm              , O(000F00,5B,_,_,_,_,_,_  ), 0                         , 5  , 0  , 6  , 5  ), // #142
  INST(Cvtpd2dq         , ExtRm              , O(F20F00,E6,_,_,_,_,_,_  ), 0                         , 6  , 0  , 6  , 5  ), // #143
  INST(Cvtpd2pi         , ExtRm              , O(660F00,2D,_,_,_,_,_,_  ), 0                         , 4  , 0  , 47 , 5  ), // #144
  INST(Cvtpd2ps         , ExtRm              , O(660F00,5A,_,_,_,_,_,_  ), 0                         , 4  , 0  , 6  , 5  ), // #145
  INST(Cvtpi2pd         , ExtRm              , O(660F00,2A,_,_,_,_,_,_  ), 0                         , 4  , 0  , 48 , 5  ), // #146
  INST(Cvtpi2ps         , ExtRm              , O(000F00,2A,_,_,_,_,_,_  ), 0                         , 5  , 0  , 48 , 6  ), // #147
  INST(Cvtps2dq         , ExtRm              , O(660F00,5B,_,_,_,_,_,_  ), 0                         , 4  , 0  , 6  , 5  ), // #148
  INST(Cvtps2pd         , ExtRm              , O(000F00,5A,_,_,_,_,_,_  ), 0                         , 5  , 0  , 7  , 5  ), // #149
  INST(Cvtps2pi         , ExtRm              , O(000F00,2D,_,_,_,_,_,_  ), 0                         , 5  , 0  , 49 , 6  ), // #150
  INST(Cvtsd2si         , ExtRm_Wx_GpqOnly   , O(F20F00,2D,_,_,x,_,_,_  ), 0                         , 6  , 0  , 50 , 5  ), // #151
  INST(Cvtsd2ss         , ExtRm              , O(F20F00,5A,_,_,_,_,_,_  ), 0                         , 6  , 0  , 7  , 5  ), // #152
  INST(Cvtsi2sd         , ExtRm_Wx           , O(F20F00,2A,_,_,x,_,_,_  ), 0                         , 6  , 0  , 51 , 5  ), // #153
  INST(Cvtsi2ss         , ExtRm_Wx           , O(F30F00,2A,_,_,x,_,_,_  ), 0                         , 7  , 0  , 52 , 6  ), // #154
  INST(Cvtss2sd         , ExtRm              , O(F30F00,5A,_,_,_,_,_,_  ), 0                         , 7  , 0  , 8  , 5  ), // #155
  INST(Cvtss2si         , ExtRm_Wx_GpqOnly   , O(F30F00,2D,_,_,x,_,_,_  ), 0                         , 7  , 0  , 53 , 6  ), // #156
  INST(Cvttpd2dq        , ExtRm              , O(660F00,E6,_,_,_,_,_,_  ), 0                         , 4  , 0  , 6  , 5  ), // #157
  INST(Cvttpd2pi        , ExtRm              , O(660F00,2C,_,_,_,_,_,_  ), 0                         , 4  , 0  , 47 , 5  ), // #158
  INST(Cvttps2dq        , ExtRm              , O(F30F00,5B,_,_,_,_,_,_  ), 0                         , 7  , 0  , 6  , 5  ), // #159
  INST(Cvttps2pi        , ExtRm              , O(000F00,2C,_,_,_,_,_,_  ), 0                         , 5  , 0  , 49 , 6  ), // #160
  INST(Cvttsd2si        , ExtRm_Wx_GpqOnly   , O(F20F00,2C,_,_,x,_,_,_  ), 0                         , 6  , 0  , 50 , 5  ), // #161
  INST(Cvttss2si        , ExtRm_Wx_GpqOnly   , O(F30F00,2C,_,_,x,_,_,_  ), 0                         , 7  , 0  , 54 , 6  ), // #162
  INST(Cwd              , X86Op_xDX_xAX      , O(660000,99,_,_,_,_,_,_  ), 0                         , 21 , 0  , 55 , 0  ), // #163
  INST(Cwde             , X86Op_xAX          , O(000000,98,_,_,_,_,_,_  ), 0                         , 0  , 0  , 56 , 0  ), // #164
  INST(Daa              , X86Op              , O(000000,27,_,_,_,_,_,_  ), 0                         , 0  , 0  , 1  , 1  ), // #165
  INST(Das              , X86Op              , O(000000,2F,_,_,_,_,_,_  ), 0                         , 0  , 0  , 1  , 1  ), // #166
  INST(Dec              , X86IncDec          , O(000000,FE,1,_,x,_,_,_  ), O(000000,48,_,_,x,_,_,_  ), 33 , 6  , 57 , 47 ), // #167
  INST(Div              , X86M_GPB_MulDiv    , O(000000,F6,6,_,x,_,_,_  ), 0                         , 34 , 0  , 58 , 1  ), // #168
  INST(Divpd            , ExtRm              , O(660F00,5E,_,_,_,_,_,_  ), 0                         , 4  , 0  , 6  , 5  ), // #169
  INST(Divps            , ExtRm              , O(000F00,5E,_,_,_,_,_,_  ), 0                         , 5  , 0  , 6  , 6  ), // #170
  INST(Divsd            , ExtRm              , O(F20F00,5E,_,_,_,_,_,_  ), 0                         , 6  , 0  , 7  , 5  ), // #171
  INST(Divss            , ExtRm              , O(F30F00,5E,_,_,_,_,_,_  ), 0                         , 7  , 0  , 8  , 6  ), // #172
  INST(Dppd             , ExtRmi             , O(660F3A,41,_,_,_,_,_,_  ), 0                         , 9  , 0  , 9  , 13 ), // #173
  INST(Dpps             , ExtRmi             , O(660F3A,40,_,_,_,_,_,_  ), 0                         , 9  , 0  , 9  , 13 ), // #174
  INST(Emms             , X86Op              , O(000F00,77,_,_,_,_,_,_  ), 0                         , 5  , 0  , 59 , 48 ), // #175
  INST(Endbr32          , X86Op_Mod11RM      , O(F30F00,1E,7,_,_,_,_,3  ), 0                         , 35 , 0  , 31 , 49 ), // #176
  INST(Endbr64          , X86Op_Mod11RM      , O(F30F00,1E,7,_,_,_,_,2  ), 0                         , 36 , 0  , 31 , 49 ), // #177
  INST(Enqcmd           , X86EnqcmdMovdir64b , O(F20F38,F8,_,_,_,_,_,_  ), 0                         , 12 , 0  , 60 , 50 ), // #178
  INST(Enqcmds          , X86EnqcmdMovdir64b , O(F30F38,F8,_,_,_,_,_,_  ), 0                         , 8  , 0  , 60 , 50 ), // #179
  INST(Enter            , X86Enter           , O(000000,C8,_,_,_,_,_,_  ), 0                         , 0  , 0  , 61 , 0  ), // #180
  INST(Extractps        , ExtExtract         , O(660F3A,17,_,_,_,_,_,_  ), 0                         , 9  , 0  , 62 , 13 ), // #181
  INST(Extrq            , ExtExtrq           , O(660F00,79,_,_,_,_,_,_  ), O(660F00,78,0,_,_,_,_,_  ), 4  , 7  , 63 , 51 ), // #182
  INST(F2xm1            , FpuOp              , O_FPU(00,D9F0,_)          , 0                         , 37 , 0  , 31 , 52 ), // #183
  INST(Fabs             , FpuOp              , O_FPU(00,D9E1,_)          , 0                         , 37 , 0  , 31 , 52 ), // #184
  INST(Fadd             , FpuArith           , O_FPU(00,C0C0,0)          , 0                         , 38 , 0  , 64 , 52 ), // #185
  INST(Faddp            , FpuRDef            , O_FPU(00,DEC0,_)          , 0                         , 39 , 0  , 65 , 52 ), // #186
  INST(Fbld             , X86M_Only          , O_FPU(00,00DF,4)          , 0                         , 40 , 0  , 66 , 52 ), // #187
  INST(Fbstp            , X86M_Only          , O_FPU(00,00DF,6)          , 0                         , 41 , 0  , 66 , 52 ), // #188
  INST(Fchs             , FpuOp              , O_FPU(00,D9E0,_)          , 0                         , 37 , 0  , 31 , 52 ), // #189
  INST(Fclex            , FpuOp              , O_FPU(9B,DBE2,_)          , 0                         , 42 , 0  , 31 , 52 ), // #190
  INST(Fcmovb           , FpuR               , O_FPU(00,DAC0,_)          , 0                         , 43 , 0  , 67 , 53 ), // #191
  INST(Fcmovbe          , FpuR               , O_FPU(00,DAD0,_)          , 0                         , 43 , 0  , 67 , 54 ), // #192
  INST(Fcmove           , FpuR               , O_FPU(00,DAC8,_)          , 0                         , 43 , 0  , 67 , 55 ), // #193
  INST(Fcmovnb          , FpuR               , O_FPU(00,DBC0,_)          , 0                         , 44 , 0  , 67 , 53 ), // #194
  INST(Fcmovnbe         , FpuR               , O_FPU(00,DBD0,_)          , 0                         , 44 , 0  , 67 , 54 ), // #195
  INST(Fcmovne          , FpuR               , O_FPU(00,DBC8,_)          , 0                         , 44 , 0  , 67 , 55 ), // #196
  INST(Fcmovnu          , FpuR               , O_FPU(00,DBD8,_)          , 0                         , 44 , 0  , 67 , 56 ), // #197
  INST(Fcmovu           , FpuR               , O_FPU(00,DAD8,_)          , 0                         , 43 , 0  , 67 , 56 ), // #198
  INST(Fcom             , FpuCom             , O_FPU(00,D0D0,2)          , 0                         , 45 , 0  , 68 , 52 ), // #199
  INST(Fcomi            , FpuR               , O_FPU(00,DBF0,_)          , 0                         , 44 , 0  , 67 , 57 ), // #200
  INST(Fcomip           , FpuR               , O_FPU(00,DFF0,_)          , 0                         , 46 , 0  , 67 , 57 ), // #201
  INST(Fcomp            , FpuCom             , O_FPU(00,D8D8,3)          , 0                         , 47 , 0  , 68 , 52 ), // #202
  INST(Fcompp           , FpuOp              , O_FPU(00,DED9,_)          , 0                         , 39 , 0  , 31 , 52 ), // #203
  INST(Fcos             , FpuOp              , O_FPU(00,D9FF,_)          , 0                         , 37 , 0  , 31 , 52 ), // #204
  INST(Fdecstp          , FpuOp              , O_FPU(00,D9F6,_)          , 0                         , 37 , 0  , 31 , 52 ), // #205
  INST(Fdiv             , FpuArith           , O_FPU(00,F0F8,6)          , 0                         , 48 , 0  , 64 , 52 ), // #206
  INST(Fdivp            , FpuRDef            , O_FPU(00,DEF8,_)          , 0                         , 39 , 0  , 65 , 52 ), // #207
  INST(Fdivr            , FpuArith           , O_FPU(00,F8F0,7)          , 0                         , 49 , 0  , 64 , 52 ), // #208
  INST(Fdivrp           , FpuRDef            , O_FPU(00,DEF0,_)          , 0                         , 39 , 0  , 65 , 52 ), // #209
  INST(Femms            , X86Op              , O(000F00,0E,_,_,_,_,_,_  ), 0                         , 5  , 0  , 31 , 58 ), // #210
  INST(Ffree            , FpuR               , O_FPU(00,DDC0,_)          , 0                         , 50 , 0  , 67 , 52 ), // #211
  INST(Fiadd            , FpuM               , O_FPU(00,00DA,0)          , 0                         , 51 , 0  , 69 , 52 ), // #212
  INST(Ficom            , FpuM               , O_FPU(00,00DA,2)          , 0                         , 52 , 0  , 69 , 52 ), // #213
  INST(Ficomp           , FpuM               , O_FPU(00,00DA,3)          , 0                         , 53 , 0  , 69 , 52 ), // #214
  INST(Fidiv            , FpuM               , O_FPU(00,00DA,6)          , 0                         , 41 , 0  , 69 , 52 ), // #215
  INST(Fidivr           , FpuM               , O_FPU(00,00DA,7)          , 0                         , 54 , 0  , 69 , 52 ), // #216
  INST(Fild             , FpuM               , O_FPU(00,00DB,0)          , O_FPU(00,00DF,5)          , 51 , 8  , 70 , 52 ), // #217
  INST(Fimul            , FpuM               , O_FPU(00,00DA,1)          , 0                         , 55 , 0  , 69 , 52 ), // #218
  INST(Fincstp          , FpuOp              , O_FPU(00,D9F7,_)          , 0                         , 37 , 0  , 31 , 52 ), // #219
  INST(Finit            , FpuOp              , O_FPU(9B,DBE3,_)          , 0                         , 42 , 0  , 31 , 52 ), // #220
  INST(Fist             , FpuM               , O_FPU(00,00DB,2)          , 0                         , 52 , 0  , 69 , 52 ), // #221
  INST(Fistp            , FpuM               , O_FPU(00,00DB,3)          , O_FPU(00,00DF,7)          , 53 , 9  , 70 , 52 ), // #222
  INST(Fisttp           , FpuM               , O_FPU(00,00DB,1)          , O_FPU(00,00DD,1)          , 55 , 10 , 70 , 59 ), // #223
  INST(Fisub            , FpuM               , O_FPU(00,00DA,4)          , 0                         , 40 , 0  , 69 , 52 ), // #224
  INST(Fisubr           , FpuM               , O_FPU(00,00DA,5)          , 0                         , 56 , 0  , 69 , 52 ), // #225
  INST(Fld              , FpuFldFst          , O_FPU(00,00D9,0)          , O_FPU(00,00DB,5)          , 51 , 11 , 71 , 52 ), // #226
  INST(Fld1             , FpuOp              , O_FPU(00,D9E8,_)          , 0                         , 37 , 0  , 31 , 52 ), // #227
  INST(Fldcw            , X86M_Only          , O_FPU(00,00D9,5)          , 0                         , 56 , 0  , 72 , 52 ), // #228
  INST(Fldenv           , X86M_Only          , O_FPU(00,00D9,4)          , 0                         , 40 , 0  , 32 , 52 ), // #229
  INST(Fldl2e           , FpuOp              , O_FPU(00,D9EA,_)          , 0                         , 37 , 0  , 31 , 52 ), // #230
  INST(Fldl2t           , FpuOp              , O_FPU(00,D9E9,_)          , 0                         , 37 , 0  , 31 , 52 ), // #231
  INST(Fldlg2           , FpuOp              , O_FPU(00,D9EC,_)          , 0                         , 37 , 0  , 31 , 52 ), // #232
  INST(Fldln2           , FpuOp              , O_FPU(00,D9ED,_)          , 0                         , 37 , 0  , 31 , 52 ), // #233
  INST(Fldpi            , FpuOp              , O_FPU(00,D9EB,_)          , 0                         , 37 , 0  , 31 , 52 ), // #234
  INST(Fldz             , FpuOp              , O_FPU(00,D9EE,_)          , 0                         , 37 , 0  , 31 , 52 ), // #235
  INST(Fmul             , FpuArith           , O_FPU(00,C8C8,1)          , 0                         , 57 , 0  , 64 , 52 ), // #236
  INST(Fmulp            , FpuRDef            , O_FPU(00,DEC8,_)          , 0                         , 39 , 0  , 65 , 52 ), // #237
  INST(Fnclex           , FpuOp              , O_FPU(00,DBE2,_)          , 0                         , 44 , 0  , 31 , 52 ), // #238
  INST(Fninit           , FpuOp              , O_FPU(00,DBE3,_)          , 0                         , 44 , 0  , 31 , 52 ), // #239
  INST(Fnop             , FpuOp              , O_FPU(00,D9D0,_)          , 0                         , 37 , 0  , 31 , 52 ), // #240
  INST(Fnsave           , X86M_Only          , O_FPU(00,00DD,6)          , 0                         , 41 , 0  , 32 , 52 ), // #241
  INST(Fnstcw           , X86M_Only          , O_FPU(00,00D9,7)          , 0                         , 54 , 0  , 72 , 52 ), // #242
  INST(Fnstenv          , X86M_Only          , O_FPU(00,00D9,6)          , 0                         , 41 , 0  , 32 , 52 ), // #243
  INST(Fnstsw           , FpuStsw            , O_FPU(00,00DD,7)          , O_FPU(00,DFE0,_)          , 54 , 12 , 73 , 52 ), // #244
  INST(Fpatan           , FpuOp              , O_FPU(00,D9F3,_)          , 0                         , 37 , 0  , 31 , 52 ), // #245
  INST(Fprem            , FpuOp              , O_FPU(00,D9F8,_)          , 0                         , 37 , 0  , 31 , 52 ), // #246
  INST(Fprem1           , FpuOp              , O_FPU(00,D9F5,_)          , 0                         , 37 , 0  , 31 , 52 ), // #247
  INST(Fptan            , FpuOp              , O_FPU(00,D9F2,_)          , 0                         , 37 , 0  , 31 , 52 ), // #248
  INST(Frndint          , FpuOp              , O_FPU(00,D9FC,_)          , 0                         , 37 , 0  , 31 , 52 ), // #249
  INST(Frstor           , X86M_Only          , O_FPU(00,00DD,4)          , 0                         , 40 , 0  , 32 , 52 ), // #250
  INST(Fsave            , X86M_Only          , O_FPU(9B,00DD,6)          , 0                         , 58 , 0  , 32 , 52 ), // #251
  INST(Fscale           , FpuOp              , O_FPU(00,D9FD,_)          , 0                         , 37 , 0  , 31 , 52 ), // #252
  INST(Fsin             , FpuOp              , O_FPU(00,D9FE,_)          , 0                         , 37 , 0  , 31 , 52 ), // #253
  INST(Fsincos          , FpuOp              , O_FPU(00,D9FB,_)          , 0                         , 37 , 0  , 31 , 52 ), // #254
  INST(Fsqrt            , FpuOp              , O_FPU(00,D9FA,_)          , 0                         , 37 , 0  , 31 , 52 ), // #255
  INST(Fst              , FpuFldFst          , O_FPU(00,00D9,2)          , 0                         , 52 , 0  , 74 , 52 ), // #256
  INST(Fstcw            , X86M_Only          , O_FPU(9B,00D9,7)          , 0                         , 59 , 0  , 72 , 52 ), // #257
  INST(Fstenv           , X86M_Only          , O_FPU(9B,00D9,6)          , 0                         , 58 , 0  , 32 , 52 ), // #258
  INST(Fstp             , FpuFldFst          , O_FPU(00,00D9,3)          , O(000000,DB,7,_,_,_,_,_  ), 53 , 13 , 71 , 52 ), // #259
  INST(Fstsw            , FpuStsw            , O_FPU(9B,00DD,7)          , O_FPU(9B,DFE0,_)          , 59 , 14 , 73 , 52 ), // #260
  INST(Fsub             , FpuArith           , O_FPU(00,E0E8,4)          , 0                         , 60 , 0  , 64 , 52 ), // #261
  INST(Fsubp            , FpuRDef            , O_FPU(00,DEE8,_)          , 0                         , 39 , 0  , 65 , 52 ), // #262
  INST(Fsubr            , FpuArith           , O_FPU(00,E8E0,5)          , 0                         , 61 , 0  , 64 , 52 ), // #263
  INST(Fsubrp           , FpuRDef            , O_FPU(00,DEE0,_)          , 0                         , 39 , 0  , 65 , 52 ), // #264
  INST(Ftst             , FpuOp              , O_FPU(00,D9E4,_)          , 0                         , 37 , 0  , 31 , 52 ), // #265
  INST(Fucom            , FpuRDef            , O_FPU(00,DDE0,_)          , 0                         , 50 , 0  , 65 , 52 ), // #266
  INST(Fucomi           , FpuR               , O_FPU(00,DBE8,_)          , 0                         , 44 , 0  , 67 , 57 ), // #267
  INST(Fucomip          , FpuR               , O_FPU(00,DFE8,_)          , 0                         , 46 , 0  , 67 , 57 ), // #268
  INST(Fucomp           , FpuRDef            , O_FPU(00,DDE8,_)          , 0                         , 50 , 0  , 65 , 52 ), // #269
  INST(Fucompp          , FpuOp              , O_FPU(00,DAE9,_)          , 0                         , 43 , 0  , 31 , 52 ), // #270
  INST(Fwait            , X86Op              , O_FPU(00,009B,_)          , 0                         , 51 , 0  , 31 , 52 ), // #271
  INST(Fxam             , FpuOp              , O_FPU(00,D9E5,_)          , 0                         , 37 , 0  , 31 , 52 ), // #272
  INST(Fxch             , FpuR               , O_FPU(00,D9C8,_)          , 0                         , 37 , 0  , 65 , 52 ), // #273
  INST(Fxrstor          , X86M_Only          , O(000F00,AE,1,_,_,_,_,_  ), 0                         , 32 , 0  , 32 , 60 ), // #274
  INST(Fxrstor64        , X86M_Only          , O(000F00,AE,1,_,1,_,_,_  ), 0                         , 31 , 0  , 75 , 60 ), // #275
  INST(Fxsave           , X86M_Only          , O(000F00,AE,0,_,_,_,_,_  ), 0                         , 5  , 0  , 32 , 61 ), // #276
  INST(Fxsave64         , X86M_Only          , O(000F00,AE,0,_,1,_,_,_  ), 0                         , 62 , 0  , 75 , 61 ), // #277
  INST(Fxtract          , FpuOp              , O_FPU(00,D9F4,_)          , 0                         , 37 , 0  , 31 , 52 ), // #278
  INST(Fyl2x            , FpuOp              , O_FPU(00,D9F1,_)          , 0                         , 37 , 0  , 31 , 52 ), // #279
  INST(Fyl2xp1          , FpuOp              , O_FPU(00,D9F9,_)          , 0                         , 37 , 0  , 31 , 52 ), // #280
  INST(Getsec           , X86Op              , O(000F00,37,_,_,_,_,_,_  ), 0                         , 5  , 0  , 56 , 62 ), // #281
  INST(Gf2p8affineinvqb , ExtRmi             , O(660F3A,CF,_,_,_,_,_,_  ), 0                         , 9  , 0  , 9  , 63 ), // #282
  INST(Gf2p8affineqb    , ExtRmi             , O(660F3A,CE,_,_,_,_,_,_  ), 0                         , 9  , 0  , 9  , 63 ), // #283
  INST(Gf2p8mulb        , ExtRm              , O(660F38,CF,_,_,_,_,_,_  ), 0                         , 2  , 0  , 6  , 63 ), // #284
  INST(Haddpd           , ExtRm              , O(660F00,7C,_,_,_,_,_,_  ), 0                         , 4  , 0  , 6  , 7  ), // #285
  INST(Haddps           , ExtRm              , O(F20F00,7C,_,_,_,_,_,_  ), 0                         , 6  , 0  , 6  , 7  ), // #286
  INST(Hlt              , X86Op              , O(000000,F4,_,_,_,_,_,_  ), 0                         , 0  , 0  , 31 , 0  ), // #287
  INST(Hreset           , X86Op_Mod11RM_I8   , O(F30F3A,F0,0,_,_,_,_,_  ), 0                         , 63 , 0  , 76 , 64 ), // #288
  INST(Hsubpd           , ExtRm              , O(660F00,7D,_,_,_,_,_,_  ), 0                         , 4  , 0  , 6  , 7  ), // #289
  INST(Hsubps           , ExtRm              , O(F20F00,7D,_,_,_,_,_,_  ), 0                         , 6  , 0  , 6  , 7  ), // #290
  INST(Idiv             , X86M_GPB_MulDiv    , O(000000,F6,7,_,x,_,_,_  ), 0                         , 29 , 0  , 58 , 1  ), // #291
  INST(Imul             , X86Imul            , O(000000,F6,5,_,x,_,_,_  ), 0                         , 64 , 0  , 77 , 1  ), // #292
  INST(In               , X86In              , O(000000,EC,_,_,_,_,_,_  ), O(000000,E4,_,_,_,_,_,_  ), 0  , 15 , 78 , 0  ), // #293
  INST(Inc              , X86IncDec          , O(000000,FE,0,_,x,_,_,_  ), O(000000,40,_,_,x,_,_,_  ), 0  , 16 , 79 , 47 ), // #294
  INST(Incsspd          , X86M               , O(F30F00,AE,5,_,0,_,_,_  ), 0                         , 65 , 0  , 80 , 65 ), // #295
  INST(Incsspq          , X86M               , O(F30F00,AE,5,_,1,_,_,_  ), 0                         , 66 , 0  , 81 , 65 ), // #296
  INST(Ins              , X86Ins             , O(000000,6C,_,_,_,_,_,_  ), 0                         , 0  , 0  , 82 , 0  ), // #297
  INST(Insertps         , ExtRmi             , O(660F3A,21,_,_,_,_,_,_  ), 0                         , 9  , 0  , 40 , 13 ), // #298
  INST(Insertq          , ExtInsertq         , O(F20F00,79,_,_,_,_,_,_  ), O(F20F00,78,_,_,_,_,_,_  ), 6  , 17 , 83 , 51 ), // #299
  INST(Int              , X86Int             , O(000000,CD,_,_,_,_,_,_  ), 0                         , 0  , 0  , 84 , 0  ), // #300
  INST(Int3             , X86Op              , O(000000,CC,_,_,_,_,_,_  ), 0                         , 0  , 0  , 31 , 0  ), // #301
  INST(Into             , X86Op              , O(000000,CE,_,_,_,_,_,_  ), 0                         , 0  , 0  , 85 , 66 ), // #302
  INST(Invd             , X86Op              , O(000F00,08,_,_,_,_,_,_  ), 0                         , 5  , 0  , 31 , 45 ), // #303
  INST(Invept           , X86Rm_NoSize       , O(660F38,80,_,_,_,_,_,_  ), 0                         , 2  , 0  , 86 , 67 ), // #304
  INST(Invlpg           , X86M_Only          , O(000F00,01,7,_,_,_,_,_  ), 0                         , 24 , 0  , 32 , 45 ), // #305
  INST(Invlpga          , X86Op_xAddr        , O(000F01,DF,_,_,_,_,_,_  ), 0                         , 23 , 0  , 87 , 23 ), // #306
  INST(Invlpgb          , X86Op              , O(000F01,FE,_,_,_,_,_,_  ), 0                         , 23 , 0  , 88 , 68 ), // #307
  INST(Invpcid          , X86Rm_NoSize       , O(660F38,82,_,_,_,_,_,_  ), 0                         , 2  , 0  , 86 , 45 ), // #308
  INST(Invvpid          , X86Rm_NoSize       , O(660F38,81,_,_,_,_,_,_  ), 0                         , 2  , 0  , 86 , 67 ), // #309
  INST(Iret             , X86Op              , O(660000,CF,_,_,_,_,_,_  ), 0                         , 21 , 0  , 89 , 1  ), // #310
  INST(Iretd            , X86Op              , O(000000,CF,_,_,_,_,_,_  ), 0                         , 0  , 0  , 89 , 1  ), // #311
  INST(Iretq            , X86Op              , O(000000,CF,_,_,1,_,_,_  ), 0                         , 22 , 0  , 90 , 1  ), // #312
  INST(Ja               , X86Jcc             , O(000F00,87,_,_,_,_,_,_  ), O(000000,77,_,_,_,_,_,_  ), 5  , 18 , 91 , 69 ), // #313
  INST(Jae              , X86Jcc             , O(000F00,83,_,_,_,_,_,_  ), O(000000,73,_,_,_,_,_,_  ), 5  , 19 , 91 , 70 ), // #314
  INST(Jb               , X86Jcc             , O(000F00,82,_,_,_,_,_,_  ), O(000000,72,_,_,_,_,_,_  ), 5  , 20 , 91 , 70 ), // #315
  INST(Jbe              , X86Jcc             , O(000F00,86,_,_,_,_,_,_  ), O(000000,76,_,_,_,_,_,_  ), 5  , 21 , 91 , 69 ), // #316
  INST(Jc               , X86Jcc             , O(000F00,82,_,_,_,_,_,_  ), O(000000,72,_,_,_,_,_,_  ), 5  , 20 , 91 , 70 ), // #317
  INST(Je               , X86Jcc             , O(000F00,84,_,_,_,_,_,_  ), O(000000,74,_,_,_,_,_,_  ), 5  , 22 , 91 , 71 ), // #318
  INST(Jecxz            , X86JecxzLoop       , 0                         , O(000000,E3,_,_,_,_,_,_  ), 0  , 23 , 92 , 0  ), // #319
  INST(Jg               , X86Jcc             , O(000F00,8F,_,_,_,_,_,_  ), O(000000,7F,_,_,_,_,_,_  ), 5  , 24 , 91 , 72 ), // #320
  INST(Jge              , X86Jcc             , O(000F00,8D,_,_,_,_,_,_  ), O(000000,7D,_,_,_,_,_,_  ), 5  , 25 , 91 , 73 ), // #321
  INST(Jl               , X86Jcc             , O(000F00,8C,_,_,_,_,_,_  ), O(000000,7C,_,_,_,_,_,_  ), 5  , 26 , 91 , 73 ), // #322
  INST(Jle              , X86Jcc             , O(000F00,8E,_,_,_,_,_,_  ), O(000000,7E,_,_,_,_,_,_  ), 5  , 27 , 91 , 72 ), // #323
  INST(Jmp              , X86Jmp             , O(000000,FF,4,_,_,_,_,_  ), O(000000,EB,_,_,_,_,_,_  ), 10 , 28 , 93 , 0  ), // #324
  INST(Jna              , X86Jcc             , O(000F00,86,_,_,_,_,_,_  ), O(000000,76,_,_,_,_,_,_  ), 5  , 21 , 91 , 69 ), // #325
  INST(Jnae             , X86Jcc             , O(000F00,82,_,_,_,_,_,_  ), O(000000,72,_,_,_,_,_,_  ), 5  , 20 , 91 , 70 ), // #326
  INST(Jnb              , X86Jcc             , O(000F00,83,_,_,_,_,_,_  ), O(000000,73,_,_,_,_,_,_  ), 5  , 19 , 91 , 70 ), // #327
  INST(Jnbe             , X86Jcc             , O(000F00,87,_,_,_,_,_,_  ), O(000000,77,_,_,_,_,_,_  ), 5  , 18 , 91 , 69 ), // #328
  INST(Jnc              , X86Jcc             , O(000F00,83,_,_,_,_,_,_  ), O(000000,73,_,_,_,_,_,_  ), 5  , 19 , 91 , 70 ), // #329
  INST(Jne              , X86Jcc             , O(000F00,85,_,_,_,_,_,_  ), O(000000,75,_,_,_,_,_,_  ), 5  , 29 , 91 , 71 ), // #330
  INST(Jng              , X86Jcc             , O(000F00,8E,_,_,_,_,_,_  ), O(000000,7E,_,_,_,_,_,_  ), 5  , 27 , 91 , 72 ), // #331
  INST(Jnge             , X86Jcc             , O(000F00,8C,_,_,_,_,_,_  ), O(000000,7C,_,_,_,_,_,_  ), 5  , 26 , 91 , 73 ), // #332
  INST(Jnl              , X86Jcc             , O(000F00,8D,_,_,_,_,_,_  ), O(000000,7D,_,_,_,_,_,_  ), 5  , 25 , 91 , 73 ), // #333
  INST(Jnle             , X86Jcc             , O(000F00,8F,_,_,_,_,_,_  ), O(000000,7F,_,_,_,_,_,_  ), 5  , 24 , 91 , 72 ), // #334
  INST(Jno              , X86Jcc             , O(000F00,81,_,_,_,_,_,_  ), O(000000,71,_,_,_,_,_,_  ), 5  , 30 , 91 , 66 ), // #335
  INST(Jnp              , X86Jcc             , O(000F00,8B,_,_,_,_,_,_  ), O(000000,7B,_,_,_,_,_,_  ), 5  , 31 , 91 , 74 ), // #336
  INST(Jns              , X86Jcc             , O(000F00,89,_,_,_,_,_,_  ), O(000000,79,_,_,_,_,_,_  ), 5  , 32 , 91 , 75 ), // #337
  INST(Jnz              , X86Jcc             , O(000F00,85,_,_,_,_,_,_  ), O(000000,75,_,_,_,_,_,_  ), 5  , 29 , 91 , 71 ), // #338
  INST(Jo               , X86Jcc             , O(000F00,80,_,_,_,_,_,_  ), O(000000,70,_,_,_,_,_,_  ), 5  , 33 , 91 , 66 ), // #339
  INST(Jp               , X86Jcc             , O(000F00,8A,_,_,_,_,_,_  ), O(000000,7A,_,_,_,_,_,_  ), 5  , 34 , 91 , 74 ), // #340
  INST(Jpe              , X86Jcc             , O(000F00,8A,_,_,_,_,_,_  ), O(000000,7A,_,_,_,_,_,_  ), 5  , 34 , 91 , 74 ), // #341
  INST(Jpo              , X86Jcc             , O(000F00,8B,_,_,_,_,_,_  ), O(000000,7B,_,_,_,_,_,_  ), 5  , 31 , 91 , 74 ), // #342
  INST(Js               , X86Jcc             , O(000F00,88,_,_,_,_,_,_  ), O(000000,78,_,_,_,_,_,_  ), 5  , 35 , 91 , 75 ), // #343
  INST(Jz               , X86Jcc             , O(000F00,84,_,_,_,_,_,_  ), O(000000,74,_,_,_,_,_,_  ), 5  , 22 , 91 , 71 ), // #344
  INST(Kaddb            , VexRvm             , V(660F00,4A,_,1,0,_,_,_  ), 0                         , 67 , 0  , 94 , 76 ), // #345
  INST(Kaddd            , VexRvm             , V(660F00,4A,_,1,1,_,_,_  ), 0                         , 68 , 0  , 94 , 77 ), // #346
  INST(Kaddq            , VexRvm             , V(000F00,4A,_,1,1,_,_,_  ), 0                         , 69 , 0  , 94 , 77 ), // #347
  INST(Kaddw            , VexRvm             , V(000F00,4A,_,1,0,_,_,_  ), 0                         , 70 , 0  , 94 , 76 ), // #348
  INST(Kandb            , VexRvm             , V(660F00,41,_,1,0,_,_,_  ), 0                         , 67 , 0  , 94 , 76 ), // #349
  INST(Kandd            , VexRvm             , V(660F00,41,_,1,1,_,_,_  ), 0                         , 68 , 0  , 94 , 77 ), // #350
  INST(Kandnb           , VexRvm             , V(660F00,42,_,1,0,_,_,_  ), 0                         , 67 , 0  , 94 , 76 ), // #351
  INST(Kandnd           , VexRvm             , V(660F00,42,_,1,1,_,_,_  ), 0                         , 68 , 0  , 94 , 77 ), // #352
  INST(Kandnq           , VexRvm             , V(000F00,42,_,1,1,_,_,_  ), 0                         , 69 , 0  , 94 , 77 ), // #353
  INST(Kandnw           , VexRvm             , V(000F00,42,_,1,0,_,_,_  ), 0                         , 70 , 0  , 94 , 78 ), // #354
  INST(Kandq            , VexRvm             , V(000F00,41,_,1,1,_,_,_  ), 0                         , 69 , 0  , 94 , 77 ), // #355
  INST(Kandw            , VexRvm             , V(000F00,41,_,1,0,_,_,_  ), 0                         , 70 , 0  , 94 , 78 ), // #356
  INST(Kmovb            , VexKmov            , V(660F00,90,_,0,0,_,_,_  ), V(660F00,92,_,0,0,_,_,_  ), 71 , 36 , 95 , 79 ), // #357
  INST(Kmovd            , VexKmov            , V(660F00,90,_,0,1,_,_,_  ), V(F20F00,92,_,0,0,_,_,_  ), 72 , 37 , 96 , 80 ), // #358
  INST(Kmovq            , VexKmov            , V(000F00,90,_,0,1,_,_,_  ), V(F20F00,92,_,0,1,_,_,_  ), 73 , 38 , 97 , 80 ), // #359
  INST(Kmovw            , VexKmov            , V(000F00,90,_,0,0,_,_,_  ), V(000F00,92,_,0,0,_,_,_  ), 74 , 39 , 98 , 81 ), // #360
  INST(Knotb            , VexRm              , V(660F00,44,_,0,0,_,_,_  ), 0                         , 71 , 0  , 99 , 76 ), // #361
  INST(Knotd            , VexRm              , V(660F00,44,_,0,1,_,_,_  ), 0                         , 72 , 0  , 99 , 77 ), // #362
  INST(Knotq            , VexRm              , V(000F00,44,_,0,1,_,_,_  ), 0                         , 73 , 0  , 99 , 77 ), // #363
  INST(Knotw            , VexRm              , V(000F00,44,_,0,0,_,_,_  ), 0                         , 74 , 0  , 99 , 78 ), // #364
  INST(Korb             , VexRvm             , V(660F00,45,_,1,0,_,_,_  ), 0                         , 67 , 0  , 94 , 76 ), // #365
  INST(Kord             , VexRvm             , V(660F00,45,_,1,1,_,_,_  ), 0                         , 68 , 0  , 94 , 77 ), // #366
  INST(Korq             , VexRvm             , V(000F00,45,_,1,1,_,_,_  ), 0                         , 69 , 0  , 94 , 77 ), // #367
  INST(Kortestb         , VexRm              , V(660F00,98,_,0,0,_,_,_  ), 0                         , 71 , 0  , 99 , 82 ), // #368
  INST(Kortestd         , VexRm              , V(660F00,98,_,0,1,_,_,_  ), 0                         , 72 , 0  , 99 , 83 ), // #369
  INST(Kortestq         , VexRm              , V(000F00,98,_,0,1,_,_,_  ), 0                         , 73 , 0  , 99 , 83 ), // #370
  INST(Kortestw         , VexRm              , V(000F00,98,_,0,0,_,_,_  ), 0                         , 74 , 0  , 99 , 84 ), // #371
  INST(Korw             , VexRvm             , V(000F00,45,_,1,0,_,_,_  ), 0                         , 70 , 0  , 94 , 78 ), // #372
  INST(Kshiftlb         , VexRmi             , V(660F3A,32,_,0,0,_,_,_  ), 0                         , 75 , 0  , 100, 76 ), // #373
  INST(Kshiftld         , VexRmi             , V(660F3A,33,_,0,0,_,_,_  ), 0                         , 75 , 0  , 100, 77 ), // #374
  INST(Kshiftlq         , VexRmi             , V(660F3A,33,_,0,1,_,_,_  ), 0                         , 76 , 0  , 100, 77 ), // #375
  INST(Kshiftlw         , VexRmi             , V(660F3A,32,_,0,1,_,_,_  ), 0                         , 76 , 0  , 100, 78 ), // #376
  INST(Kshiftrb         , VexRmi             , V(660F3A,30,_,0,0,_,_,_  ), 0                         , 75 , 0  , 100, 76 ), // #377
  INST(Kshiftrd         , VexRmi             , V(660F3A,31,_,0,0,_,_,_  ), 0                         , 75 , 0  , 100, 77 ), // #378
  INST(Kshiftrq         , VexRmi             , V(660F3A,31,_,0,1,_,_,_  ), 0                         , 76 , 0  , 100, 77 ), // #379
  INST(Kshiftrw         , VexRmi             , V(660F3A,30,_,0,1,_,_,_  ), 0                         , 76 , 0  , 100, 78 ), // #380
  INST(Ktestb           , VexRm              , V(660F00,99,_,0,0,_,_,_  ), 0                         , 71 , 0  , 99 , 82 ), // #381
  INST(Ktestd           , VexRm              , V(660F00,99,_,0,1,_,_,_  ), 0                         , 72 , 0  , 99 , 83 ), // #382
  INST(Ktestq           , VexRm              , V(000F00,99,_,0,1,_,_,_  ), 0                         , 73 , 0  , 99 , 83 ), // #383
  INST(Ktestw           , VexRm              , V(000F00,99,_,0,0,_,_,_  ), 0                         , 74 , 0  , 99 , 82 ), // #384
  INST(Kunpckbw         , VexRvm             , V(660F00,4B,_,1,0,_,_,_  ), 0                         , 67 , 0  , 94 , 78 ), // #385
  INST(Kunpckdq         , VexRvm             , V(000F00,4B,_,1,1,_,_,_  ), 0                         , 69 , 0  , 94 , 77 ), // #386
  INST(Kunpckwd         , VexRvm             , V(000F00,4B,_,1,0,_,_,_  ), 0                         , 70 , 0  , 94 , 77 ), // #387
  INST(Kxnorb           , VexRvm             , V(660F00,46,_,1,0,_,_,_  ), 0                         , 67 , 0  , 101, 76 ), // #388
  INST(Kxnord           , VexRvm             , V(660F00,46,_,1,1,_,_,_  ), 0                         , 68 , 0  , 101, 77 ), // #389
  INST(Kxnorq           , VexRvm             , V(000F00,46,_,1,1,_,_,_  ), 0                         , 69 , 0  , 101, 77 ), // #390
  INST(Kxnorw           , VexRvm             , V(000F00,46,_,1,0,_,_,_  ), 0                         , 70 , 0  , 101, 78 ), // #391
  INST(Kxorb            , VexRvm             , V(660F00,47,_,1,0,_,_,_  ), 0                         , 67 , 0  , 101, 76 ), // #392
  INST(Kxord            , VexRvm             , V(660F00,47,_,1,1,_,_,_  ), 0                         , 68 , 0  , 101, 77 ), // #393
  INST(Kxorq            , VexRvm             , V(000F00,47,_,1,1,_,_,_  ), 0                         , 69 , 0  , 101, 77 ), // #394
  INST(Kxorw            , VexRvm             , V(000F00,47,_,1,0,_,_,_  ), 0                         , 70 , 0  , 101, 78 ), // #395
  INST(Lahf             , X86Op              , O(000000,9F,_,_,_,_,_,_  ), 0                         , 0  , 0  , 102, 85 ), // #396
  INST(Lar              , X86Rm              , O(000F00,02,_,_,_,_,_,_  ), 0                         , 5  , 0  , 103, 11 ), // #397
  INST(Lcall            , X86LcallLjmp       , O(000000,FF,3,_,_,_,_,_  ), O(000000,9A,_,_,_,_,_,_  ), 77 , 40 , 104, 1  ), // #398
  INST(Lddqu            , ExtRm              , O(F20F00,F0,_,_,_,_,_,_  ), 0                         , 6  , 0  , 105, 7  ), // #399
  INST(Ldmxcsr          , X86M_Only          , O(000F00,AE,2,_,_,_,_,_  ), 0                         , 78 , 0  , 106, 6  ), // #400
  INST(Lds              , X86Rm              , O(000000,C5,_,_,_,_,_,_  ), 0                         , 0  , 0  , 107, 0  ), // #401
  INST(Ldtilecfg        , AmxCfg             , V(000F38,49,_,0,0,_,_,_  ), 0                         , 11 , 0  , 108, 86 ), // #402
  INST(Lea              , X86Lea             , O(000000,8D,_,_,x,_,_,_  ), 0                         , 0  , 0  , 109, 0  ), // #403
  INST(Leave            , X86Op              , O(000000,C9,_,_,_,_,_,_  ), 0                         , 0  , 0  , 31 , 0  ), // #404
  INST(Les              , X86Rm              , O(000000,C4,_,_,_,_,_,_  ), 0                         , 0  , 0  , 107, 0  ), // #405
  INST(Lfence           , X86Fence           , O(000F00,AE,5,_,_,_,_,_  ), 0                         , 79 , 0  , 31 , 5  ), // #406
  INST(Lfs              , X86Rm              , O(000F00,B4,_,_,_,_,_,_  ), 0                         , 5  , 0  , 110, 0  ), // #407
  INST(Lgdt             , X86M_Only          , O(000F00,01,2,_,_,_,_,_  ), 0                         , 78 , 0  , 32 , 0  ), // #408
  INST(Lgs              , X86Rm              , O(000F00,B5,_,_,_,_,_,_  ), 0                         , 5  , 0  , 110, 0  ), // #409
  INST(Lidt             , X86M_Only          , O(000F00,01,3,_,_,_,_,_  ), 0                         , 80 , 0  , 32 , 0  ), // #410
  INST(Ljmp             , X86LcallLjmp       , O(000000,FF,5,_,_,_,_,_  ), O(000000,EA,_,_,_,_,_,_  ), 64 , 41 , 111, 0  ), // #411
  INST(Lldt             , X86M_NoSize        , O(000F00,00,2,_,_,_,_,_  ), 0                         , 78 , 0  , 112, 0  ), // #412
  INST(Llwpcb           , VexR_Wx            , V(XOP_M9,12,0,0,x,_,_,_  ), 0                         , 81 , 0  , 113, 87 ), // #413
  INST(Lmsw             , X86M_NoSize        , O(000F00,01,6,_,_,_,_,_  ), 0                         , 82 , 0  , 112, 0  ), // #414
  INST(Lods             , X86StrRm           , O(000000,AC,_,_,_,_,_,_  ), 0                         , 0  , 0  , 114, 88 ), // #415
  INST(Loop             , X86JecxzLoop       , 0                         , O(000000,E2,_,_,_,_,_,_  ), 0  , 42 , 115, 0  ), // #416
  INST(Loope            , X86JecxzLoop       , 0                         , O(000000,E1,_,_,_,_,_,_  ), 0  , 43 , 115, 71 ), // #417
  INST(Loopne           , X86JecxzLoop       , 0                         , O(000000,E0,_,_,_,_,_,_  ), 0  , 44 , 115, 71 ), // #418
  INST(Lsl              , X86Rm              , O(000F00,03,_,_,_,_,_,_  ), 0                         , 5  , 0  , 116, 11 ), // #419
  INST(Lss              , X86Rm              , O(000F00,B2,_,_,_,_,_,_  ), 0                         , 5  , 0  , 110, 0  ), // #420
  INST(Ltr              , X86M_NoSize        , O(000F00,00,3,_,_,_,_,_  ), 0                         , 80 , 0  , 112, 0  ), // #421
  INST(Lwpins           , VexVmi4_Wx         , V(XOP_MA,12,0,0,x,_,_,_  ), 0                         , 83 , 0  , 117, 87 ), // #422
  INST(Lwpval           , VexVmi4_Wx         , V(XOP_MA,12,1,0,x,_,_,_  ), 0                         , 84 , 0  , 117, 87 ), // #423
  INST(Lzcnt            , X86Rm_Raw66H       , O(F30F00,BD,_,_,x,_,_,_  ), 0                         , 7  , 0  , 23 , 89 ), // #424
  INST(Maskmovdqu       , ExtRm_ZDI          , O(660F00,F7,_,_,_,_,_,_  ), 0                         , 4  , 0  , 118, 5  ), // #425
  INST(Maskmovq         , ExtRm_ZDI          , O(000F00,F7,_,_,_,_,_,_  ), 0                         , 5  , 0  , 119, 90 ), // #426
  INST(Maxpd            , ExtRm              , O(660F00,5F,_,_,_,_,_,_  ), 0                         , 4  , 0  , 6  , 5  ), // #427
  INST(Maxps            , ExtRm              , O(000F00,5F,_,_,_,_,_,_  ), 0                         , 5  , 0  , 6  , 6  ), // #428
  INST(Maxsd            , ExtRm              , O(F20F00,5F,_,_,_,_,_,_  ), 0                         , 6  , 0  , 7  , 5  ), // #429
  INST(Maxss            , ExtRm              , O(F30F00,5F,_,_,_,_,_,_  ), 0                         , 7  , 0  , 8  , 6  ), // #430
  INST(Mcommit          , X86Op              , O(F30F01,FA,_,_,_,_,_,_  ), 0                         , 27 , 0  , 31 , 91 ), // #431
  INST(Mfence           , X86Fence           , O(000F00,AE,6,_,_,_,_,_  ), 0                         , 82 , 0  , 31 , 5  ), // #432
  INST(Minpd            , ExtRm              , O(660F00,5D,_,_,_,_,_,_  ), 0                         , 4  , 0  , 6  , 5  ), // #433
  INST(Minps            , ExtRm              , O(000F00,5D,_,_,_,_,_,_  ), 0                         , 5  , 0  , 6  , 6  ), // #434
  INST(Minsd            , ExtRm              , O(F20F00,5D,_,_,_,_,_,_  ), 0                         , 6  , 0  , 7  , 5  ), // #435
  INST(Minss            , ExtRm              , O(F30F00,5D,_,_,_,_,_,_  ), 0                         , 7  , 0  , 8  , 6  ), // #436
  INST(Monitor          , X86Op              , O(000F01,C8,_,_,_,_,_,_  ), 0                         , 23 , 0  , 120, 92 ), // #437
  INST(Monitorx         , X86Op              , O(000F01,FA,_,_,_,_,_,_  ), 0                         , 23 , 0  , 120, 93 ), // #438
  INST(Mov              , X86Mov             , 0                         , 0                         , 0  , 0  , 121, 94 ), // #439
  INST(Movabs           , X86Movabs          , 0                         , 0                         , 0  , 0  , 122, 0  ), // #440
  INST(Movapd           , ExtMov             , O(660F00,28,_,_,_,_,_,_  ), O(660F00,29,_,_,_,_,_,_  ), 4  , 45 , 123, 95 ), // #441
  INST(Movaps           , ExtMov             , O(000F00,28,_,_,_,_,_,_  ), O(000F00,29,_,_,_,_,_,_  ), 5  , 46 , 123, 96 ), // #442
  INST(Movbe            , ExtMovbe           , O(000F38,F0,_,_,x,_,_,_  ), O(000F38,F1,_,_,x,_,_,_  ), 1  , 47 , 124, 97 ), // #443
  INST(Movd             , ExtMovd            , O(000F00,6E,_,_,_,_,_,_  ), O(000F00,7E,_,_,_,_,_,_  ), 5  , 48 , 125, 98 ), // #444
  INST(Movddup          , ExtMov             , O(F20F00,12,_,_,_,_,_,_  ), 0                         , 6  , 0  , 7  , 7  ), // #445
  INST(Movdir64b        , X86EnqcmdMovdir64b , O(660F38,F8,_,_,_,_,_,_  ), 0                         , 2  , 0  , 126, 99 ), // #446
  INST(Movdiri          , X86MovntiMovdiri   , O(000F38,F9,_,_,_,_,_,_  ), 0                         , 1  , 0  , 3  , 100), // #447
  INST(Movdq2q          , ExtMov             , O(F20F00,D6,_,_,_,_,_,_  ), 0                         , 6  , 0  , 127, 5  ), // #448
  INST(Movdqa           , ExtMov             , O(660F00,6F,_,_,_,_,_,_  ), O(660F00,7F,_,_,_,_,_,_  ), 4  , 49 , 123, 95 ), // #449
  INST(Movdqu           , ExtMov             , O(F30F00,6F,_,_,_,_,_,_  ), O(F30F00,7F,_,_,_,_,_,_  ), 7  , 50 , 123, 95 ), // #450
  INST(Movhlps          , ExtMov             , O(000F00,12,_,_,_,_,_,_  ), 0                         , 5  , 0  , 128, 6  ), // #451
  INST(Movhpd           , ExtMov             , O(660F00,16,_,_,_,_,_,_  ), O(660F00,17,_,_,_,_,_,_  ), 4  , 51 , 129, 5  ), // #452
  INST(Movhps           , ExtMov             , O(000F00,16,_,_,_,_,_,_  ), O(000F00,17,_,_,_,_,_,_  ), 5  , 52 , 129, 6  ), // #453
  INST(Movlhps          , ExtMov             , O(000F00,16,_,_,_,_,_,_  ), 0                         , 5  , 0  , 128, 6  ), // #454
  INST(Movlpd           , ExtMov             , O(660F00,12,_,_,_,_,_,_  ), O(660F00,13,_,_,_,_,_,_  ), 4  , 53 , 129, 5  ), // #455
  INST(Movlps           , ExtMov             , O(000F00,12,_,_,_,_,_,_  ), O(000F00,13,_,_,_,_,_,_  ), 5  , 54 , 129, 6  ), // #456
  INST(Movmskpd         , ExtMov             , O(660F00,50,_,_,_,_,_,_  ), 0                         , 4  , 0  , 130, 5  ), // #457
  INST(Movmskps         , ExtMov             , O(000F00,50,_,_,_,_,_,_  ), 0                         , 5  , 0  , 130, 6  ), // #458
  INST(Movntdq          , ExtMov             , 0                         , O(660F00,E7,_,_,_,_,_,_  ), 0  , 55 , 131, 5  ), // #459
  INST(Movntdqa         , ExtMov             , O(660F38,2A,_,_,_,_,_,_  ), 0                         , 2  , 0  , 105, 13 ), // #460
  INST(Movnti           , X86MovntiMovdiri   , O(000F00,C3,_,_,x,_,_,_  ), 0                         , 5  , 0  , 3  , 5  ), // #461
  INST(Movntpd          , ExtMov             , 0                         , O(660F00,2B,_,_,_,_,_,_  ), 0  , 56 , 131, 5  ), // #462
  INST(Movntps          , ExtMov             , 0                         , O(000F00,2B,_,_,_,_,_,_  ), 0  , 57 , 131, 6  ), // #463
  INST(Movntq           , ExtMov             , 0                         , O(000F00,E7,_,_,_,_,_,_  ), 0  , 58 , 132, 90 ), // #464
  INST(Movntsd          , ExtMov             , 0                         , O(F20F00,2B,_,_,_,_,_,_  ), 0  , 59 , 133, 51 ), // #465
  INST(Movntss          , ExtMov             , 0                         , O(F30F00,2B,_,_,_,_,_,_  ), 0  , 60 , 134, 51 ), // #466
  INST(Movq             , ExtMovq            , O(000F00,6E,_,_,x,_,_,_  ), O(000F00,7E,_,_,x,_,_,_  ), 5  , 48 , 135, 101), // #467
  INST(Movq2dq          , ExtRm              , O(F30F00,D6,_,_,_,_,_,_  ), 0                         , 7  , 0  , 136, 5  ), // #468
  INST(Movs             , X86StrMm           , O(000000,A4,_,_,_,_,_,_  ), 0                         , 0  , 0  , 137, 88 ), // #469
  INST(Movsd            , ExtMov             , O(F20F00,10,_,_,_,_,_,_  ), O(F20F00,11,_,_,_,_,_,_  ), 6  , 61 , 138, 95 ), // #470
  INST(Movshdup         , ExtRm              , O(F30F00,16,_,_,_,_,_,_  ), 0                         , 7  , 0  , 6  , 7  ), // #471
  INST(Movsldup         , ExtRm              , O(F30F00,12,_,_,_,_,_,_  ), 0                         , 7  , 0  , 6  , 7  ), // #472
  INST(Movss            , ExtMov             , O(F30F00,10,_,_,_,_,_,_  ), O(F30F00,11,_,_,_,_,_,_  ), 7  , 62 , 139, 96 ), // #473
  INST(Movsx            , X86MovsxMovzx      , O(000F00,BE,_,_,x,_,_,_  ), 0                         , 5  , 0  , 140, 0  ), // #474
  INST(Movsxd           , X86Rm              , O(000000,63,_,_,x,_,_,_  ), 0                         , 0  , 0  , 141, 0  ), // #475
  INST(Movupd           , ExtMov             , O(660F00,10,_,_,_,_,_,_  ), O(660F00,11,_,_,_,_,_,_  ), 4  , 63 , 123, 95 ), // #476
  INST(Movups           , ExtMov             , O(000F00,10,_,_,_,_,_,_  ), O(000F00,11,_,_,_,_,_,_  ), 5  , 64 , 123, 96 ), // #477
  INST(Movzx            , X86MovsxMovzx      , O(000F00,B6,_,_,x,_,_,_  ), 0                         , 5  , 0  , 140, 0  ), // #478
  INST(Mpsadbw          , ExtRmi             , O(660F3A,42,_,_,_,_,_,_  ), 0                         , 9  , 0  , 9  , 13 ), // #479
  INST(Mul              , X86M_GPB_MulDiv    , O(000000,F6,4,_,x,_,_,_  ), 0                         , 10 , 0  , 58 , 1  ), // #480
  INST(Mulpd            , ExtRm              , O(660F00,59,_,_,_,_,_,_  ), 0                         , 4  , 0  , 6  , 5  ), // #481
  INST(Mulps            , ExtRm              , O(000F00,59,_,_,_,_,_,_  ), 0                         , 5  , 0  , 6  , 6  ), // #482
  INST(Mulsd            , ExtRm              , O(F20F00,59,_,_,_,_,_,_  ), 0                         , 6  , 0  , 7  , 5  ), // #483
  INST(Mulss            , ExtRm              , O(F30F00,59,_,_,_,_,_,_  ), 0                         , 7  , 0  , 8  , 6  ), // #484
  INST(Mulx             , VexRvm_ZDX_Wx      , V(F20F38,F6,_,0,x,_,_,_  ), 0                         , 85 , 0  , 142, 102), // #485
  INST(Mwait            , X86Op              , O(000F01,C9,_,_,_,_,_,_  ), 0                         , 23 , 0  , 143, 92 ), // #486
  INST(Mwaitx           , X86Op              , O(000F01,FB,_,_,_,_,_,_  ), 0                         , 23 , 0  , 144, 93 ), // #487
  INST(Neg              , X86M_GPB           , O(000000,F6,3,_,x,_,_,_  ), 0                         , 77 , 0  , 145, 1  ), // #488
  INST(Nop              , X86M_Nop           , O(000000,90,_,_,_,_,_,_  ), 0                         , 0  , 0  , 146, 0  ), // #489
  INST(Not              , X86M_GPB           , O(000000,F6,2,_,x,_,_,_  ), 0                         , 3  , 0  , 145, 0  ), // #490
  INST(Or               , X86Arith           , O(000000,08,1,_,x,_,_,_  ), 0                         , 33 , 0  , 147, 1  ), // #491
  INST(Orpd             , ExtRm              , O(660F00,56,_,_,_,_,_,_  ), 0                         , 4  , 0  , 12 , 5  ), // #492
  INST(Orps             , ExtRm              , O(000F00,56,_,_,_,_,_,_  ), 0                         , 5  , 0  , 12 , 6  ), // #493
  INST(Out              , X86Out             , O(000000,EE,_,_,_,_,_,_  ), O(000000,E6,_,_,_,_,_,_  ), 0  , 65 , 148, 0  ), // #494
  INST(Outs             , X86Outs            , O(000000,6E,_,_,_,_,_,_  ), 0                         , 0  , 0  , 149, 0  ), // #495
  INST(Pabsb            , ExtRm_P            , O(000F38,1C,_,_,_,_,_,_  ), 0                         , 1  , 0  , 150, 103), // #496
  INST(Pabsd            , ExtRm_P            , O(000F38,1E,_,_,_,_,_,_  ), 0                         , 1  , 0  , 150, 103), // #497
  INST(Pabsw            , ExtRm_P            , O(000F38,1D,_,_,_,_,_,_  ), 0                         , 1  , 0  , 150, 103), // #498
  INST(Packssdw         , ExtRm_P            , O(000F00,6B,_,_,_,_,_,_  ), 0                         , 5  , 0  , 150, 98 ), // #499
  INST(Packsswb         , ExtRm_P            , O(000F00,63,_,_,_,_,_,_  ), 0                         , 5  , 0  , 150, 98 ), // #500
  INST(Packusdw         , ExtRm              , O(660F38,2B,_,_,_,_,_,_  ), 0                         , 2  , 0  , 6  , 13 ), // #501
  INST(Packuswb         , ExtRm_P            , O(000F00,67,_,_,_,_,_,_  ), 0                         , 5  , 0  , 150, 98 ), // #502
  INST(Paddb            , ExtRm_P            , O(000F00,FC,_,_,_,_,_,_  ), 0                         , 5  , 0  , 150, 98 ), // #503
  INST(Paddd            , ExtRm_P            , O(000F00,FE,_,_,_,_,_,_  ), 0                         , 5  , 0  , 150, 98 ), // #504
  INST(Paddq            , ExtRm_P            , O(000F00,D4,_,_,_,_,_,_  ), 0                         , 5  , 0  , 150, 5  ), // #505
  INST(Paddsb           , ExtRm_P            , O(000F00,EC,_,_,_,_,_,_  ), 0                         , 5  , 0  , 150, 98 ), // #506
  INST(Paddsw           , ExtRm_P            , O(000F00,ED,_,_,_,_,_,_  ), 0                         , 5  , 0  , 150, 98 ), // #507
  INST(Paddusb          , ExtRm_P            , O(000F00,DC,_,_,_,_,_,_  ), 0                         , 5  , 0  , 150, 98 ), // #508
  INST(Paddusw          , ExtRm_P            , O(000F00,DD,_,_,_,_,_,_  ), 0                         , 5  , 0  , 150, 98 ), // #509
  INST(Paddw            , ExtRm_P            , O(000F00,FD,_,_,_,_,_,_  ), 0                         , 5  , 0  , 150, 98 ), // #510
  INST(Palignr          , ExtRmi_P           , O(000F3A,0F,_,_,_,_,_,_  ), 0                         , 86 , 0  , 151, 103), // #511
  INST(Pand             , ExtRm_P            , O(000F00,DB,_,_,_,_,_,_  ), 0                         , 5  , 0  , 152, 98 ), // #512
  INST(Pandn            , ExtRm_P            , O(000F00,DF,_,_,_,_,_,_  ), 0                         , 5  , 0  , 153, 98 ), // #513
  INST(Pause            , X86Op              , O(F30000,90,_,_,_,_,_,_  ), 0                         , 87 , 0  , 31 , 0  ), // #514
  INST(Pavgb            , ExtRm_P            , O(000F00,E0,_,_,_,_,_,_  ), 0                         , 5  , 0  , 150, 104), // #515
  INST(Pavgusb          , Ext3dNow           , O(000F0F,BF,_,_,_,_,_,_  ), 0                         , 88 , 0  , 154, 58 ), // #516
  INST(Pavgw            , ExtRm_P            , O(000F00,E3,_,_,_,_,_,_  ), 0                         , 5  , 0  , 150, 104), // #517
  INST(Pblendvb         , ExtRm_XMM0         , O(660F38,10,_,_,_,_,_,_  ), 0                         , 2  , 0  , 16 , 13 ), // #518
  INST(Pblendw          , ExtRmi             , O(660F3A,0E,_,_,_,_,_,_  ), 0                         , 9  , 0  , 9  , 13 ), // #519
  INST(Pclmulqdq        , ExtRmi             , O(660F3A,44,_,_,_,_,_,_  ), 0                         , 9  , 0  , 9  , 105), // #520
  INST(Pcmpeqb          , ExtRm_P            , O(000F00,74,_,_,_,_,_,_  ), 0                         , 5  , 0  , 153, 98 ), // #521
  INST(Pcmpeqd          , ExtRm_P            , O(000F00,76,_,_,_,_,_,_  ), 0                         , 5  , 0  , 153, 98 ), // #522
  INST(Pcmpeqq          , ExtRm              , O(660F38,29,_,_,_,_,_,_  ), 0                         , 2  , 0  , 155, 13 ), // #523
  INST(Pcmpeqw          , ExtRm_P            , O(000F00,75,_,_,_,_,_,_  ), 0                         , 5  , 0  , 153, 98 ), // #524
  INST(Pcmpestri        , ExtRmi             , O(660F3A,61,_,_,_,_,_,_  ), 0                         , 9  , 0  , 156, 106), // #525
  INST(Pcmpestrm        , ExtRmi             , O(660F3A,60,_,_,_,_,_,_  ), 0                         , 9  , 0  , 157, 106), // #526
  INST(Pcmpgtb          , ExtRm_P            , O(000F00,64,_,_,_,_,_,_  ), 0                         , 5  , 0  , 153, 98 ), // #527
  INST(Pcmpgtd          , ExtRm_P            , O(000F00,66,_,_,_,_,_,_  ), 0                         , 5  , 0  , 153, 98 ), // #528
  INST(Pcmpgtq          , ExtRm              , O(660F38,37,_,_,_,_,_,_  ), 0                         , 2  , 0  , 155, 46 ), // #529
  INST(Pcmpgtw          , ExtRm_P            , O(000F00,65,_,_,_,_,_,_  ), 0                         , 5  , 0  , 153, 98 ), // #530
  INST(Pcmpistri        , ExtRmi             , O(660F3A,63,_,_,_,_,_,_  ), 0                         , 9  , 0  , 158, 106), // #531
  INST(Pcmpistrm        , ExtRmi             , O(660F3A,62,_,_,_,_,_,_  ), 0                         , 9  , 0  , 159, 106), // #532
  INST(Pconfig          , X86Op              , O(000F01,C5,_,_,_,_,_,_  ), 0                         , 23 , 0  , 31 , 107), // #533
  INST(Pdep             , VexRvm_Wx          , V(F20F38,F5,_,0,x,_,_,_  ), 0                         , 85 , 0  , 11 , 102), // #534
  INST(Pext             , VexRvm_Wx          , V(F30F38,F5,_,0,x,_,_,_  ), 0                         , 89 , 0  , 11 , 102), // #535
  INST(Pextrb           , ExtExtract         , O(000F3A,14,_,_,_,_,_,_  ), 0                         , 86 , 0  , 160, 13 ), // #536
  INST(Pextrd           , ExtExtract         , O(000F3A,16,_,_,_,_,_,_  ), 0                         , 86 , 0  , 62 , 13 ), // #537
  INST(Pextrq           , ExtExtract         , O(000F3A,16,_,_,1,_,_,_  ), 0                         , 90 , 0  , 161, 13 ), // #538
  INST(Pextrw           , ExtPextrw          , O(000F00,C5,_,_,_,_,_,_  ), O(000F3A,15,_,_,_,_,_,_  ), 5  , 66 , 162, 108), // #539
  INST(Pf2id            , Ext3dNow           , O(000F0F,1D,_,_,_,_,_,_  ), 0                         , 88 , 0  , 154, 58 ), // #540
  INST(Pf2iw            , Ext3dNow           , O(000F0F,1C,_,_,_,_,_,_  ), 0                         , 88 , 0  , 154, 109), // #541
  INST(Pfacc            , Ext3dNow           , O(000F0F,AE,_,_,_,_,_,_  ), 0                         , 88 , 0  , 154, 58 ), // #542
  INST(Pfadd            , Ext3dNow           , O(000F0F,9E,_,_,_,_,_,_  ), 0                         , 88 , 0  , 154, 58 ), // #543
  INST(Pfcmpeq          , Ext3dNow           , O(000F0F,B0,_,_,_,_,_,_  ), 0                         , 88 , 0  , 154, 58 ), // #544
  INST(Pfcmpge          , Ext3dNow           , O(000F0F,90,_,_,_,_,_,_  ), 0                         , 88 , 0  , 154, 58 ), // #545
  INST(Pfcmpgt          , Ext3dNow           , O(000F0F,A0,_,_,_,_,_,_  ), 0                         , 88 , 0  , 154, 58 ), // #546
  INST(Pfmax            , Ext3dNow           , O(000F0F,A4,_,_,_,_,_,_  ), 0                         , 88 , 0  , 154, 58 ), // #547
  INST(Pfmin            , Ext3dNow           , O(000F0F,94,_,_,_,_,_,_  ), 0                         , 88 , 0  , 154, 58 ), // #548
  INST(Pfmul            , Ext3dNow           , O(000F0F,B4,_,_,_,_,_,_  ), 0                         , 88 , 0  , 154, 58 ), // #549
  INST(Pfnacc           , Ext3dNow           , O(000F0F,8A,_,_,_,_,_,_  ), 0                         , 88 , 0  , 154, 109), // #550
  INST(Pfpnacc          , Ext3dNow           , O(000F0F,8E,_,_,_,_,_,_  ), 0                         , 88 , 0  , 154, 109), // #551
  INST(Pfrcp            , Ext3dNow           , O(000F0F,96,_,_,_,_,_,_  ), 0                         , 88 , 0  , 154, 58 ), // #552
  INST(Pfrcpit1         , Ext3dNow           , O(000F0F,A6,_,_,_,_,_,_  ), 0                         , 88 , 0  , 154, 58 ), // #553
  INST(Pfrcpit2         , Ext3dNow           , O(000F0F,B6,_,_,_,_,_,_  ), 0                         , 88 , 0  , 154, 58 ), // #554
  INST(Pfrcpv           , Ext3dNow           , O(000F0F,86,_,_,_,_,_,_  ), 0                         , 88 , 0  , 154, 110), // #555
  INST(Pfrsqit1         , Ext3dNow           , O(000F0F,A7,_,_,_,_,_,_  ), 0                         , 88 , 0  , 154, 58 ), // #556
  INST(Pfrsqrt          , Ext3dNow           , O(000F0F,97,_,_,_,_,_,_  ), 0                         , 88 , 0  , 154, 58 ), // #557
  INST(Pfrsqrtv         , Ext3dNow           , O(000F0F,87,_,_,_,_,_,_  ), 0                         , 88 , 0  , 154, 110), // #558
  INST(Pfsub            , Ext3dNow           , O(000F0F,9A,_,_,_,_,_,_  ), 0                         , 88 , 0  , 154, 58 ), // #559
  INST(Pfsubr           , Ext3dNow           , O(000F0F,AA,_,_,_,_,_,_  ), 0                         , 88 , 0  , 154, 58 ), // #560
  INST(Phaddd           , ExtRm_P            , O(000F38,02,_,_,_,_,_,_  ), 0                         , 1  , 0  , 150, 103), // #561
  INST(Phaddsw          , ExtRm_P            , O(000F38,03,_,_,_,_,_,_  ), 0                         , 1  , 0  , 150, 103), // #562
  INST(Phaddw           , ExtRm_P            , O(000F38,01,_,_,_,_,_,_  ), 0                         , 1  , 0  , 150, 103), // #563
  INST(Phminposuw       , ExtRm              , O(660F38,41,_,_,_,_,_,_  ), 0                         , 2  , 0  , 6  , 13 ), // #564
  INST(Phsubd           , ExtRm_P            , O(000F38,06,_,_,_,_,_,_  ), 0                         , 1  , 0  , 150, 103), // #565
  INST(Phsubsw          , ExtRm_P            , O(000F38,07,_,_,_,_,_,_  ), 0                         , 1  , 0  , 150, 103), // #566
  INST(Phsubw           , ExtRm_P            , O(000F38,05,_,_,_,_,_,_  ), 0                         , 1  , 0  , 150, 103), // #567
  INST(Pi2fd            , Ext3dNow           , O(000F0F,0D,_,_,_,_,_,_  ), 0                         , 88 , 0  , 154, 58 ), // #568
  INST(Pi2fw            , Ext3dNow           , O(000F0F,0C,_,_,_,_,_,_  ), 0                         , 88 , 0  , 154, 109), // #569
  INST(Pinsrb           , ExtRmi             , O(660F3A,20,_,_,_,_,_,_  ), 0                         , 9  , 0  , 163, 13 ), // #570
  INST(Pinsrd           , ExtRmi             , O(660F3A,22,_,_,_,_,_,_  ), 0                         , 9  , 0  , 164, 13 ), // #571
  INST(Pinsrq           , ExtRmi             , O(660F3A,22,_,_,1,_,_,_  ), 0                         , 91 , 0  , 165, 13 ), // #572
  INST(Pinsrw           , ExtRmi_P           , O(000F00,C4,_,_,_,_,_,_  ), 0                         , 5  , 0  , 166, 104), // #573
  INST(Pmaddubsw        , ExtRm_P            , O(000F38,04,_,_,_,_,_,_  ), 0                         , 1  , 0  , 150, 103), // #574
  INST(Pmaddwd          , ExtRm_P            , O(000F00,F5,_,_,_,_,_,_  ), 0                         , 5  , 0  , 150, 98 ), // #575
  INST(Pmaxsb           , ExtRm              , O(660F38,3C,_,_,_,_,_,_  ), 0                         , 2  , 0  , 12 , 13 ), // #576
  INST(Pmaxsd           , ExtRm              , O(660F38,3D,_,_,_,_,_,_  ), 0                         , 2  , 0  , 12 , 13 ), // #577
  INST(Pmaxsw           , ExtRm_P            , O(000F00,EE,_,_,_,_,_,_  ), 0                         , 5  , 0  , 152, 104), // #578
  INST(Pmaxub           , ExtRm_P            , O(000F00,DE,_,_,_,_,_,_  ), 0                         , 5  , 0  , 152, 104), // #579
  INST(Pmaxud           , ExtRm              , O(660F38,3F,_,_,_,_,_,_  ), 0                         , 2  , 0  , 12 , 13 ), // #580
  INST(Pmaxuw           , ExtRm              , O(660F38,3E,_,_,_,_,_,_  ), 0                         , 2  , 0  , 12 , 13 ), // #581
  INST(Pminsb           , ExtRm              , O(660F38,38,_,_,_,_,_,_  ), 0                         , 2  , 0  , 12 , 13 ), // #582
  INST(Pminsd           , ExtRm              , O(660F38,39,_,_,_,_,_,_  ), 0                         , 2  , 0  , 12 , 13 ), // #583
  INST(Pminsw           , ExtRm_P            , O(000F00,EA,_,_,_,_,_,_  ), 0                         , 5  , 0  , 152, 104), // #584
  INST(Pminub           , ExtRm_P            , O(000F00,DA,_,_,_,_,_,_  ), 0                         , 5  , 0  , 152, 104), // #585
  INST(Pminud           , ExtRm              , O(660F38,3B,_,_,_,_,_,_  ), 0                         , 2  , 0  , 12 , 13 ), // #586
  INST(Pminuw           , ExtRm              , O(660F38,3A,_,_,_,_,_,_  ), 0                         , 2  , 0  , 12 , 13 ), // #587
  INST(Pmovmskb         , ExtRm_P            , O(000F00,D7,_,_,_,_,_,_  ), 0                         , 5  , 0  , 167, 104), // #588
  INST(Pmovsxbd         , ExtRm              , O(660F38,21,_,_,_,_,_,_  ), 0                         , 2  , 0  , 8  , 13 ), // #589
  INST(Pmovsxbq         , ExtRm              , O(660F38,22,_,_,_,_,_,_  ), 0                         , 2  , 0  , 168, 13 ), // #590
  INST(Pmovsxbw         , ExtRm              , O(660F38,20,_,_,_,_,_,_  ), 0                         , 2  , 0  , 7  , 13 ), // #591
  INST(Pmovsxdq         , ExtRm              , O(660F38,25,_,_,_,_,_,_  ), 0                         , 2  , 0  , 7  , 13 ), // #592
  INST(Pmovsxwd         , ExtRm              , O(660F38,23,_,_,_,_,_,_  ), 0                         , 2  , 0  , 7  , 13 ), // #593
  INST(Pmovsxwq         , ExtRm              , O(660F38,24,_,_,_,_,_,_  ), 0                         , 2  , 0  , 8  , 13 ), // #594
  INST(Pmovzxbd         , ExtRm              , O(660F38,31,_,_,_,_,_,_  ), 0                         , 2  , 0  , 8  , 13 ), // #595
  INST(Pmovzxbq         , ExtRm              , O(660F38,32,_,_,_,_,_,_  ), 0                         , 2  , 0  , 168, 13 ), // #596
  INST(Pmovzxbw         , ExtRm              , O(660F38,30,_,_,_,_,_,_  ), 0                         , 2  , 0  , 7  , 13 ), // #597
  INST(Pmovzxdq         , ExtRm              , O(660F38,35,_,_,_,_,_,_  ), 0                         , 2  , 0  , 7  , 13 ), // #598
  INST(Pmovzxwd         , ExtRm              , O(660F38,33,_,_,_,_,_,_  ), 0                         , 2  , 0  , 7  , 13 ), // #599
  INST(Pmovzxwq         , ExtRm              , O(660F38,34,_,_,_,_,_,_  ), 0                         , 2  , 0  , 8  , 13 ), // #600
  INST(Pmuldq           , ExtRm              , O(660F38,28,_,_,_,_,_,_  ), 0                         , 2  , 0  , 6  , 13 ), // #601
  INST(Pmulhrsw         , ExtRm_P            , O(000F38,0B,_,_,_,_,_,_  ), 0                         , 1  , 0  , 150, 103), // #602
  INST(Pmulhrw          , Ext3dNow           , O(000F0F,B7,_,_,_,_,_,_  ), 0                         , 88 , 0  , 154, 58 ), // #603
  INST(Pmulhuw          , ExtRm_P            , O(000F00,E4,_,_,_,_,_,_  ), 0                         , 5  , 0  , 150, 104), // #604
  INST(Pmulhw           , ExtRm_P            , O(000F00,E5,_,_,_,_,_,_  ), 0                         , 5  , 0  , 150, 98 ), // #605
  INST(Pmulld           , ExtRm              , O(660F38,40,_,_,_,_,_,_  ), 0                         , 2  , 0  , 6  , 13 ), // #606
  INST(Pmullw           , ExtRm_P            , O(000F00,D5,_,_,_,_,_,_  ), 0                         , 5  , 0  , 150, 98 ), // #607
  INST(Pmuludq          , ExtRm_P            , O(000F00,F4,_,_,_,_,_,_  ), 0                         , 5  , 0  , 150, 5  ), // #608
  INST(Pop              , X86Pop             , O(000000,8F,0,_,_,_,_,_  ), O(000000,58,_,_,_,_,_,_  ), 0  , 67 , 169, 0  ), // #609
  INST(Popa             , X86Op              , O(660000,61,_,_,_,_,_,_  ), 0                         , 21 , 0  , 85 , 0  ), // #610
  INST(Popad            , X86Op              , O(000000,61,_,_,_,_,_,_  ), 0                         , 0  , 0  , 85 , 0  ), // #611
  INST(Popcnt           , X86Rm_Raw66H       , O(F30F00,B8,_,_,x,_,_,_  ), 0                         , 7  , 0  , 23 , 111), // #612
  INST(Popf             , X86Op              , O(660000,9D,_,_,_,_,_,_  ), 0                         , 21 , 0  , 31 , 112), // #613
  INST(Popfd            , X86Op              , O(000000,9D,_,_,_,_,_,_  ), 0                         , 0  , 0  , 85 , 112), // #614
  INST(Popfq            , X86Op              , O(000000,9D,_,_,_,_,_,_  ), 0                         , 0  , 0  , 34 , 112), // #615
  INST(Por              , ExtRm_P            , O(000F00,EB,_,_,_,_,_,_  ), 0                         , 5  , 0  , 152, 98 ), // #616
  INST(Prefetch         , X86M_Only          , O(000F00,0D,0,_,_,_,_,_  ), 0                         , 5  , 0  , 32 , 58 ), // #617
  INST(Prefetchit0      , X86M_Only          , O(000F00,18,7,_,_,_,_,_  ), 0                         , 24 , 0  , 75 , 113), // #618
  INST(Prefetchit1      , X86M_Only          , O(000F00,18,6,_,_,_,_,_  ), 0                         , 82 , 0  , 75 , 113), // #619
  INST(Prefetchnta      , X86M_Only          , O(000F00,18,0,_,_,_,_,_  ), 0                         , 5  , 0  , 32 , 6  ), // #620
  INST(Prefetcht0       , X86M_Only          , O(000F00,18,1,_,_,_,_,_  ), 0                         , 32 , 0  , 32 , 6  ), // #621
  INST(Prefetcht1       , X86M_Only          , O(000F00,18,2,_,_,_,_,_  ), 0                         , 78 , 0  , 32 , 6  ), // #622
  INST(Prefetcht2       , X86M_Only          , O(000F00,18,3,_,_,_,_,_  ), 0                         , 80 , 0  , 32 , 6  ), // #623
  INST(Prefetchw        , X86M_Only          , O(000F00,0D,1,_,_,_,_,_  ), 0                         , 32 , 0  , 32 , 114), // #624
  INST(Prefetchwt1      , X86M_Only          , O(000F00,0D,2,_,_,_,_,_  ), 0                         , 78 , 0  , 32 , 115), // #625
  INST(Psadbw           , ExtRm_P            , O(000F00,F6,_,_,_,_,_,_  ), 0                         , 5  , 0  , 150, 104), // #626
  INST(Pshufb           , ExtRm_P            , O(000F38,00,_,_,_,_,_,_  ), 0                         , 1  , 0  , 150, 103), // #627
  INST(Pshufd           , ExtRmi             , O(660F00,70,_,_,_,_,_,_  ), 0                         , 4  , 0  , 9  , 5  ), // #628
  INST(Pshufhw          , ExtRmi             , O(F30F00,70,_,_,_,_,_,_  ), 0                         , 7  , 0  , 9  , 5  ), // #629
  INST(Pshuflw          , ExtRmi             , O(F20F00,70,_,_,_,_,_,_  ), 0                         , 6  , 0  , 9  , 5  ), // #630
  INST(Pshufw           , ExtRmi_P           , O(000F00,70,_,_,_,_,_,_  ), 0                         , 5  , 0  , 170, 90 ), // #631
  INST(Psignb           , ExtRm_P            , O(000F38,08,_,_,_,_,_,_  ), 0                         , 1  , 0  , 150, 103), // #632
  INST(Psignd           , ExtRm_P            , O(000F38,0A,_,_,_,_,_,_  ), 0                         , 1  , 0  , 150, 103), // #633
  INST(Psignw           , ExtRm_P            , O(000F38,09,_,_,_,_,_,_  ), 0                         , 1  , 0  , 150, 103), // #634
  INST(Pslld            , ExtRmRi_P          , O(000F00,F2,_,_,_,_,_,_  ), O(000F00,72,6,_,_,_,_,_  ), 5  , 68 , 171, 98 ), // #635
  INST(Pslldq           , ExtRmRi            , 0                         , O(660F00,73,7,_,_,_,_,_  ), 0  , 69 , 172, 5  ), // #636
  INST(Psllq            , ExtRmRi_P          , O(000F00,F3,_,_,_,_,_,_  ), O(000F00,73,6,_,_,_,_,_  ), 5  , 70 , 171, 98 ), // #637
  INST(Psllw            , ExtRmRi_P          , O(000F00,F1,_,_,_,_,_,_  ), O(000F00,71,6,_,_,_,_,_  ), 5  , 71 , 171, 98 ), // #638
  INST(Psmash           , X86Op              , O(F30F01,FF,_,_,_,_,_,_  ), 0                         , 27 , 0  , 34 , 116), // #639
  INST(Psrad            , ExtRmRi_P          , O(000F00,E2,_,_,_,_,_,_  ), O(000F00,72,4,_,_,_,_,_  ), 5  , 72 , 171, 98 ), // #640
  INST(Psraw            , ExtRmRi_P          , O(000F00,E1,_,_,_,_,_,_  ), O(000F00,71,4,_,_,_,_,_  ), 5  , 73 , 171, 98 ), // #641
  INST(Psrld            , ExtRmRi_P          , O(000F00,D2,_,_,_,_,_,_  ), O(000F00,72,2,_,_,_,_,_  ), 5  , 74 , 171, 98 ), // #642
  INST(Psrldq           , ExtRmRi            , 0                         , O(660F00,73,3,_,_,_,_,_  ), 0  , 75 , 172, 5  ), // #643
  INST(Psrlq            , ExtRmRi_P          , O(000F00,D3,_,_,_,_,_,_  ), O(000F00,73,2,_,_,_,_,_  ), 5  , 76 , 171, 98 ), // #644
  INST(Psrlw            , ExtRmRi_P          , O(000F00,D1,_,_,_,_,_,_  ), O(000F00,71,2,_,_,_,_,_  ), 5  , 77 , 171, 98 ), // #645
  INST(Psubb            , ExtRm_P            , O(000F00,F8,_,_,_,_,_,_  ), 0                         , 5  , 0  , 153, 98 ), // #646
  INST(Psubd            , ExtRm_P            , O(000F00,FA,_,_,_,_,_,_  ), 0                         , 5  , 0  , 153, 98 ), // #647
  INST(Psubq            , ExtRm_P            , O(000F00,FB,_,_,_,_,_,_  ), 0                         , 5  , 0  , 153, 5  ), // #648
  INST(Psubsb           , ExtRm_P            , O(000F00,E8,_,_,_,_,_,_  ), 0                         , 5  , 0  , 153, 98 ), // #649
  INST(Psubsw           , ExtRm_P            , O(000F00,E9,_,_,_,_,_,_  ), 0                         , 5  , 0  , 153, 98 ), // #650
  INST(Psubusb          , ExtRm_P            , O(000F00,D8,_,_,_,_,_,_  ), 0                         , 5  , 0  , 153, 98 ), // #651
  INST(Psubusw          , ExtRm_P            , O(000F00,D9,_,_,_,_,_,_  ), 0                         , 5  , 0  , 153, 98 ), // #652
  INST(Psubw            , ExtRm_P            , O(000F00,F9,_,_,_,_,_,_  ), 0                         , 5  , 0  , 153, 98 ), // #653
  INST(Pswapd           , Ext3dNow           , O(000F0F,BB,_,_,_,_,_,_  ), 0                         , 88 , 0  , 154, 109), // #654
  INST(Ptest            , ExtRm              , O(660F38,17,_,_,_,_,_,_  ), 0                         , 2  , 0  , 6  , 117), // #655
  INST(Ptwrite          , X86M               , O(F30F00,AE,4,_,_,_,_,_  ), 0                         , 92 , 0  , 173, 118), // #656
  INST(Punpckhbw        , ExtRm_P            , O(000F00,68,_,_,_,_,_,_  ), 0                         , 5  , 0  , 150, 98 ), // #657
  INST(Punpckhdq        , ExtRm_P            , O(000F00,6A,_,_,_,_,_,_  ), 0                         , 5  , 0  , 150, 98 ), // #658
  INST(Punpckhqdq       , ExtRm              , O(660F00,6D,_,_,_,_,_,_  ), 0                         , 4  , 0  , 6  , 5  ), // #659
  INST(Punpckhwd        , ExtRm_P            , O(000F00,69,_,_,_,_,_,_  ), 0                         , 5  , 0  , 150, 98 ), // #660
  INST(Punpcklbw        , ExtRm_P            , O(000F00,60,_,_,_,_,_,_  ), 0                         , 5  , 0  , 174, 98 ), // #661
  INST(Punpckldq        , ExtRm_P            , O(000F00,62,_,_,_,_,_,_  ), 0                         , 5  , 0  , 174, 98 ), // #662
  INST(Punpcklqdq       , ExtRm              , O(660F00,6C,_,_,_,_,_,_  ), 0                         , 4  , 0  , 6  , 5  ), // #663
  INST(Punpcklwd        , ExtRm_P            , O(000F00,61,_,_,_,_,_,_  ), 0                         , 5  , 0  , 174, 98 ), // #664
  INST(Push             , X86Push            , O(000000,FF,6,_,_,_,_,_  ), O(000000,50,_,_,_,_,_,_  ), 34 , 78 , 175, 0  ), // #665
  INST(Pusha            , X86Op              , O(660000,60,_,_,_,_,_,_  ), 0                         , 21 , 0  , 85 , 0  ), // #666
  INST(Pushad           , X86Op              , O(000000,60,_,_,_,_,_,_  ), 0                         , 0  , 0  , 85 , 0  ), // #667
  INST(Pushf            , X86Op              , O(660000,9C,_,_,_,_,_,_  ), 0                         , 21 , 0  , 31 , 119), // #668
  INST(Pushfd           , X86Op              , O(000000,9C,_,_,_,_,_,_  ), 0                         , 0  , 0  , 85 , 119), // #669
  INST(Pushfq           , X86Op              , O(000000,9C,_,_,_,_,_,_  ), 0                         , 0  , 0  , 34 , 119), // #670
  INST(Pvalidate        , X86Op              , O(F20F01,FF,_,_,_,_,_,_  ), 0                         , 93 , 0  , 31 , 120), // #671
  INST(Pxor             , ExtRm_P            , O(000F00,EF,_,_,_,_,_,_  ), 0                         , 5  , 0  , 153, 98 ), // #672
  INST(Rcl              , X86Rot             , O(000000,D0,2,_,x,_,_,_  ), 0                         , 3  , 0  , 176, 121), // #673
  INST(Rcpps            , ExtRm              , O(000F00,53,_,_,_,_,_,_  ), 0                         , 5  , 0  , 6  , 6  ), // #674
  INST(Rcpss            , ExtRm              , O(F30F00,53,_,_,_,_,_,_  ), 0                         , 7  , 0  , 8  , 6  ), // #675
  INST(Rcr              , X86Rot             , O(000000,D0,3,_,x,_,_,_  ), 0                         , 77 , 0  , 176, 121), // #676
  INST(Rdfsbase         , X86M               , O(F30F00,AE,0,_,x,_,_,_  ), 0                         , 7  , 0  , 177, 122), // #677
  INST(Rdgsbase         , X86M               , O(F30F00,AE,1,_,x,_,_,_  ), 0                         , 94 , 0  , 177, 122), // #678
  INST(Rdmsr            , X86Op              , O(000F00,32,_,_,_,_,_,_  ), 0                         , 5  , 0  , 178, 123), // #679
  INST(Rdpid            , X86R_Native        , O(F30F00,C7,7,_,_,_,_,_  ), 0                         , 95 , 0  , 179, 124), // #680
  INST(Rdpkru           , X86Op              , O(000F01,EE,_,_,_,_,_,_  ), 0                         , 23 , 0  , 178, 125), // #681
  INST(Rdpmc            , X86Op              , O(000F00,33,_,_,_,_,_,_  ), 0                         , 5  , 0  , 178, 0  ), // #682
  INST(Rdpru            , X86Op              , O(000F01,FD,_,_,_,_,_,_  ), 0                         , 23 , 0  , 178, 126), // #683
  INST(Rdrand           , X86M               , O(000F00,C7,6,_,x,_,_,_  ), 0                         , 82 , 0  , 24 , 127), // #684
  INST(Rdseed           , X86M               , O(000F00,C7,7,_,x,_,_,_  ), 0                         , 24 , 0  , 24 , 128), // #685
  INST(Rdsspd           , X86M               , O(F30F00,1E,1,_,_,_,_,_  ), 0                         , 94 , 0  , 80 , 65 ), // #686
  INST(Rdsspq           , X86M               , O(F30F00,1E,1,_,_,_,_,_  ), 0                         , 94 , 0  , 81 , 65 ), // #687
  INST(Rdtsc            , X86Op              , O(000F00,31,_,_,_,_,_,_  ), 0                         , 5  , 0  , 29 , 129), // #688
  INST(Rdtscp           , X86Op              , O(000F01,F9,_,_,_,_,_,_  ), 0                         , 23 , 0  , 178, 130), // #689
  INST(Ret              , X86Ret             , O(000000,C2,_,_,_,_,_,_  ), 0                         , 0  , 0  , 180, 0  ), // #690
  INST(Retf             , X86Ret             , O(000000,CA,_,_,x,_,_,_  ), 0                         , 0  , 0  , 181, 0  ), // #691
  INST(Rmpadjust        , X86Op              , O(F30F01,FE,_,_,_,_,_,_  ), 0                         , 27 , 0  , 34 , 116), // #692
  INST(Rmpupdate        , X86Op              , O(F20F01,FE,_,_,_,_,_,_  ), 0                         , 93 , 0  , 34 , 116), // #693
  INST(Rol              , X86Rot             , O(000000,D0,0,_,x,_,_,_  ), 0                         , 0  , 0  , 176, 131), // #694
  INST(Ror              , X86Rot             , O(000000,D0,1,_,x,_,_,_  ), 0                         , 33 , 0  , 176, 131), // #695
  INST(Rorx             , VexRmi_Wx          , V(F20F3A,F0,_,0,x,_,_,_  ), 0                         , 96 , 0  , 182, 102), // #696
  INST(Roundpd          , ExtRmi             , O(660F3A,09,_,_,_,_,_,_  ), 0                         , 9  , 0  , 9  , 13 ), // #697
  INST(Roundps          , ExtRmi             , O(660F3A,08,_,_,_,_,_,_  ), 0                         , 9  , 0  , 9  , 13 ), // #698
  INST(Roundsd          , ExtRmi             , O(660F3A,0B,_,_,_,_,_,_  ), 0                         , 9  , 0  , 39 , 13 ), // #699
  INST(Roundss          , ExtRmi             , O(660F3A,0A,_,_,_,_,_,_  ), 0                         , 9  , 0  , 40 , 13 ), // #700
  INST(Rsm              , X86Op              , O(000F00,AA,_,_,_,_,_,_  ), 0                         , 5  , 0  , 85 , 1  ), // #701
  INST(Rsqrtps          , ExtRm              , O(000F00,52,_,_,_,_,_,_  ), 0                         , 5  , 0  , 6  , 6  ), // #702
  INST(Rsqrtss          , ExtRm              , O(F30F00,52,_,_,_,_,_,_  ), 0                         , 7  , 0  , 8  , 6  ), // #703
  INST(Rstorssp         , X86M_Only          , O(F30F00,01,5,_,_,_,_,_  ), 0                         , 65 , 0  , 33 , 25 ), // #704
  INST(Sahf             , X86Op              , O(000000,9E,_,_,_,_,_,_  ), 0                         , 0  , 0  , 102, 132), // #705
  INST(Sal              , X86Rot             , O(000000,D0,4,_,x,_,_,_  ), 0                         , 10 , 0  , 176, 1  ), // #706
  INST(Sar              , X86Rot             , O(000000,D0,7,_,x,_,_,_  ), 0                         , 29 , 0  , 176, 1  ), // #707
  INST(Sarx             , VexRmv_Wx          , V(F30F38,F7,_,0,x,_,_,_  ), 0                         , 89 , 0  , 14 , 102), // #708
  INST(Saveprevssp      , X86Op              , O(F30F01,EA,_,_,_,_,_,_  ), 0                         , 27 , 0  , 31 , 25 ), // #709
  INST(Sbb              , X86Arith           , O(000000,18,3,_,x,_,_,_  ), 0                         , 77 , 0  , 183, 3  ), // #710
  INST(Scas             , X86StrRm           , O(000000,AE,_,_,_,_,_,_  ), 0                         , 0  , 0  , 184, 39 ), // #711
  INST(Seamcall         , X86Op              , O(660F01,CF,_,_,_,_,_,_  ), 0                         , 97 , 0  , 31 , 133), // #712
  INST(Seamops          , X86Op              , O(660F01,CE,_,_,_,_,_,_  ), 0                         , 97 , 0  , 31 , 133), // #713
  INST(Seamret          , X86Op              , O(660F01,CD,_,_,_,_,_,_  ), 0                         , 97 , 0  , 31 , 133), // #714
  INST(Senduipi         , X86M_NoSize        , O(F30F00,C7,6,_,_,_,_,_  ), 0                         , 26 , 0  , 81 , 26 ), // #715
  INST(Serialize        , X86Op              , O(000F01,E8,_,_,_,_,_,_  ), 0                         , 23 , 0  , 31 , 134), // #716
  INST(Seta             , X86Set             , O(000F00,97,_,_,_,_,_,_  ), 0                         , 5  , 0  , 185, 69 ), // #717
  INST(Setae            , X86Set             , O(000F00,93,_,_,_,_,_,_  ), 0                         , 5  , 0  , 185, 70 ), // #718
  INST(Setb             , X86Set             , O(000F00,92,_,_,_,_,_,_  ), 0                         , 5  , 0  , 185, 70 ), // #719
  INST(Setbe            , X86Set             , O(000F00,96,_,_,_,_,_,_  ), 0                         , 5  , 0  , 185, 69 ), // #720
  INST(Setc             , X86Set             , O(000F00,92,_,_,_,_,_,_  ), 0                         , 5  , 0  , 185, 70 ), // #721
  INST(Sete             , X86Set             , O(000F00,94,_,_,_,_,_,_  ), 0                         , 5  , 0  , 185, 71 ), // #722
  INST(Setg             , X86Set             , O(000F00,9F,_,_,_,_,_,_  ), 0                         , 5  , 0  , 185, 72 ), // #723
  INST(Setge            , X86Set             , O(000F00,9D,_,_,_,_,_,_  ), 0                         , 5  , 0  , 185, 73 ), // #724
  INST(Setl             , X86Set             , O(000F00,9C,_,_,_,_,_,_  ), 0                         , 5  , 0  , 185, 73 ), // #725
  INST(Setle            , X86Set             , O(000F00,9E,_,_,_,_,_,_  ), 0                         , 5  , 0  , 185, 72 ), // #726
  INST(Setna            , X86Set             , O(000F00,96,_,_,_,_,_,_  ), 0                         , 5  , 0  , 185, 69 ), // #727
  INST(Setnae           , X86Set             , O(000F00,92,_,_,_,_,_,_  ), 0                         , 5  , 0  , 185, 70 ), // #728
  INST(Setnb            , X86Set             , O(000F00,93,_,_,_,_,_,_  ), 0                         , 5  , 0  , 185, 70 ), // #729
  INST(Setnbe           , X86Set             , O(000F00,97,_,_,_,_,_,_  ), 0                         , 5  , 0  , 185, 69 ), // #730
  INST(Setnc            , X86Set             , O(000F00,93,_,_,_,_,_,_  ), 0                         , 5  , 0  , 185, 70 ), // #731
  INST(Setne            , X86Set             , O(000F00,95,_,_,_,_,_,_  ), 0                         , 5  , 0  , 185, 71 ), // #732
  INST(Setng            , X86Set             , O(000F00,9E,_,_,_,_,_,_  ), 0                         , 5  , 0  , 185, 72 ), // #733
  INST(Setnge           , X86Set             , O(000F00,9C,_,_,_,_,_,_  ), 0                         , 5  , 0  , 185, 73 ), // #734
  INST(Setnl            , X86Set             , O(000F00,9D,_,_,_,_,_,_  ), 0                         , 5  , 0  , 185, 73 ), // #735
  INST(Setnle           , X86Set             , O(000F00,9F,_,_,_,_,_,_  ), 0                         , 5  , 0  , 185, 72 ), // #736
  INST(Setno            , X86Set             , O(000F00,91,_,_,_,_,_,_  ), 0                         , 5  , 0  , 185, 66 ), // #737
  INST(Setnp            , X86Set             , O(000F00,9B,_,_,_,_,_,_  ), 0                         , 5  , 0  , 185, 74 ), // #738
  INST(Setns            , X86Set             , O(000F00,99,_,_,_,_,_,_  ), 0                         , 5  , 0  , 185, 75 ), // #739
  INST(Setnz            , X86Set             , O(000F00,95,_,_,_,_,_,_  ), 0                         , 5  , 0  , 185, 71 ), // #740
  INST(Seto             , X86Set             , O(000F00,90,_,_,_,_,_,_  ), 0                         , 5  , 0  , 185, 66 ), // #741
  INST(Setp             , X86Set             , O(000F00,9A,_,_,_,_,_,_  ), 0                         , 5  , 0  , 185, 74 ), // #742
  INST(Setpe            , X86Set             , O(000F00,9A,_,_,_,_,_,_  ), 0                         , 5  , 0  , 185, 74 ), // #743
  INST(Setpo            , X86Set             , O(000F00,9B,_,_,_,_,_,_  ), 0                         , 5  , 0  , 185, 74 ), // #744
  INST(Sets             , X86Set             , O(000F00,98,_,_,_,_,_,_  ), 0                         , 5  , 0  , 185, 75 ), // #745
  INST(Setssbsy         , X86Op              , O(F30F01,E8,_,_,_,_,_,_  ), 0                         , 27 , 0  , 31 , 65 ), // #746
  INST(Setz             , X86Set             , O(000F00,94,_,_,_,_,_,_  ), 0                         , 5  , 0  , 185, 71 ), // #747
  INST(Sfence           , X86Fence           , O(000F00,AE,7,_,_,_,_,_  ), 0                         , 24 , 0  , 31 , 6  ), // #748
  INST(Sgdt             , X86M_Only          , O(000F00,01,0,_,_,_,_,_  ), 0                         , 5  , 0  , 32 , 0  ), // #749
  INST(Sha1msg1         , ExtRm              , O(000F38,C9,_,_,_,_,_,_  ), 0                         , 1  , 0  , 6  , 135), // #750
  INST(Sha1msg2         , ExtRm              , O(000F38,CA,_,_,_,_,_,_  ), 0                         , 1  , 0  , 6  , 135), // #751
  INST(Sha1nexte        , ExtRm              , O(000F38,C8,_,_,_,_,_,_  ), 0                         , 1  , 0  , 6  , 135), // #752
  INST(Sha1rnds4        , ExtRmi             , O(000F3A,CC,_,_,_,_,_,_  ), 0                         , 86 , 0  , 9  , 135), // #753
  INST(Sha256msg1       , ExtRm              , O(000F38,CC,_,_,_,_,_,_  ), 0                         , 1  , 0  , 6  , 135), // #754
  INST(Sha256msg2       , ExtRm              , O(000F38,CD,_,_,_,_,_,_  ), 0                         , 1  , 0  , 6  , 135), // #755
  INST(Sha256rnds2      , ExtRm_XMM0         , O(000F38,CB,_,_,_,_,_,_  ), 0                         , 1  , 0  , 16 , 135), // #756
  INST(Shl              , X86Rot             , O(000000,D0,4,_,x,_,_,_  ), 0                         , 10 , 0  , 176, 1  ), // #757
  INST(Shld             , X86ShldShrd        , O(000F00,A4,_,_,x,_,_,_  ), 0                         , 5  , 0  , 186, 1  ), // #758
  INST(Shlx             , VexRmv_Wx          , V(660F38,F7,_,0,x,_,_,_  ), 0                         , 30 , 0  , 14 , 102), // #759
  INST(Shr              , X86Rot             , O(000000,D0,5,_,x,_,_,_  ), 0                         , 64 , 0  , 176, 1  ), // #760
  INST(Shrd             , X86ShldShrd        , O(000F00,AC,_,_,x,_,_,_  ), 0                         , 5  , 0  , 186, 1  ), // #761
  INST(Shrx             , VexRmv_Wx          , V(F20F38,F7,_,0,x,_,_,_  ), 0                         , 85 , 0  , 14 , 102), // #762
  INST(Shufpd           , ExtRmi             , O(660F00,C6,_,_,_,_,_,_  ), 0                         , 4  , 0  , 9  , 5  ), // #763
  INST(Shufps           , ExtRmi             , O(000F00,C6,_,_,_,_,_,_  ), 0                         , 5  , 0  , 9  , 6  ), // #764
  INST(Sidt             , X86M_Only          , O(000F00,01,1,_,_,_,_,_  ), 0                         , 32 , 0  , 32 , 0  ), // #765
  INST(Skinit           , X86Op_xAX          , O(000F01,DE,_,_,_,_,_,_  ), 0                         , 23 , 0  , 56 , 136), // #766
  INST(Sldt             , X86M_NoMemSize     , O(000F00,00,0,_,_,_,_,_  ), 0                         , 5  , 0  , 187, 0  ), // #767
  INST(Slwpcb           , VexR_Wx            , V(XOP_M9,12,1,0,x,_,_,_  ), 0                         , 13 , 0  , 113, 87 ), // #768
  INST(Smsw             , X86M_NoMemSize     , O(000F00,01,4,_,_,_,_,_  ), 0                         , 98 , 0  , 187, 0  ), // #769
  INST(Sqrtpd           , ExtRm              , O(660F00,51,_,_,_,_,_,_  ), 0                         , 4  , 0  , 6  , 5  ), // #770
  INST(Sqrtps           , ExtRm              , O(000F00,51,_,_,_,_,_,_  ), 0                         , 5  , 0  , 6  , 6  ), // #771
  INST(Sqrtsd           , ExtRm              , O(F20F00,51,_,_,_,_,_,_  ), 0                         , 6  , 0  , 7  , 5  ), // #772
  INST(Sqrtss           , ExtRm              , O(F30F00,51,_,_,_,_,_,_  ), 0                         , 7  , 0  , 8  , 6  ), // #773
  INST(Stac             , X86Op              , O(000F01,CB,_,_,_,_,_,_  ), 0                         , 23 , 0  , 31 , 17 ), // #774
  INST(Stc              , X86Op              , O(000000,F9,_,_,_,_,_,_  ), 0                         , 0  , 0  , 31 , 18 ), // #775
  INST(Std              , X86Op              , O(000000,FD,_,_,_,_,_,_  ), 0                         , 0  , 0  , 31 , 19 ), // #776
  INST(Stgi             , X86Op              , O(000F01,DC,_,_,_,_,_,_  ), 0                         , 23 , 0  , 31 , 136), // #777
  INST(Sti              , X86Op              , O(000000,FB,_,_,_,_,_,_  ), 0                         , 0  , 0  , 31 , 24 ), // #778
  INST(Stmxcsr          , X86M_Only          , O(000F00,AE,3,_,_,_,_,_  ), 0                         , 80 , 0  , 106, 6  ), // #779
  INST(Stos             , X86StrMr           , O(000000,AA,_,_,_,_,_,_  ), 0                         , 0  , 0  , 188, 88 ), // #780
  INST(Str              , X86M_NoMemSize     , O(000F00,00,1,_,_,_,_,_  ), 0                         , 32 , 0  , 187, 0  ), // #781
  INST(Sttilecfg        , AmxCfg             , V(660F38,49,_,0,0,_,_,_  ), 0                         , 30 , 0  , 108, 86 ), // #782
  INST(Stui             , X86Op              , O(F30F01,EF,_,_,_,_,_,_  ), 0                         , 27 , 0  , 34 , 26 ), // #783
  INST(Sub              , X86Arith           , O(000000,28,5,_,x,_,_,_  ), 0                         , 64 , 0  , 183, 1  ), // #784
  INST(Subpd            , ExtRm              , O(660F00,5C,_,_,_,_,_,_  ), 0                         , 4  , 0  , 6  , 5  ), // #785
  INST(Subps            , ExtRm              , O(000F00,5C,_,_,_,_,_,_  ), 0                         , 5  , 0  , 6  , 6  ), // #786
  INST(Subsd            , ExtRm              , O(F20F00,5C,_,_,_,_,_,_  ), 0                         , 6  , 0  , 7  , 5  ), // #787
  INST(Subss            , ExtRm              , O(F30F00,5C,_,_,_,_,_,_  ), 0                         , 7  , 0  , 8  , 6  ), // #788
  INST(Swapgs           , X86Op              , O(000F01,F8,_,_,_,_,_,_  ), 0                         , 23 , 0  , 34 , 0  ), // #789
  INST(Syscall          , X86Op              , O(000F00,05,_,_,_,_,_,_  ), 0                         , 5  , 0  , 34 , 0  ), // #790
  INST(Sysenter         , X86Op              , O(000F00,34,_,_,_,_,_,_  ), 0                         , 5  , 0  , 31 , 0  ), // #791
  INST(Sysexit          , X86Op              , O(000F00,35,_,_,_,_,_,_  ), 0                         , 5  , 0  , 31 , 0  ), // #792
  INST(Sysexitq         , X86Op              , O(000F00,35,_,_,1,_,_,_  ), 0                         , 62 , 0  , 34 , 0  ), // #793
  INST(Sysret           , X86Op              , O(000F00,07,_,_,_,_,_,_  ), 0                         , 5  , 0  , 34 , 0  ), // #794
  INST(Sysretq          , X86Op              , O(000F00,07,_,_,1,_,_,_  ), 0                         , 62 , 0  , 34 , 0  ), // #795
  INST(T1mskc           , VexVm_Wx           , V(XOP_M9,01,7,0,x,_,_,_  ), 0                         , 99 , 0  , 15 , 12 ), // #796
  INST(Tcmmimfp16ps     , AmxRmv             , V(660F38,6C,_,0,0,_,_,_  ), 0                         , 30 , 0  , 189, 137), // #797
  INST(Tcmmrlfp16ps     , AmxRmv             , V(000F38,6C,_,0,0,_,_,_  ), 0                         , 11 , 0  , 189, 137), // #798
  INST(Tdcall           , X86Op              , O(660F01,CC,_,_,_,_,_,_  ), 0                         , 97 , 0  , 31 , 133), // #799
  INST(Tdpbf16ps        , AmxRmv             , V(F30F38,5C,_,0,0,_,_,_  ), 0                         , 89 , 0  , 189, 138), // #800
  INST(Tdpbssd          , AmxRmv             , V(F20F38,5E,_,0,0,_,_,_  ), 0                         , 85 , 0  , 189, 139), // #801
  INST(Tdpbsud          , AmxRmv             , V(F30F38,5E,_,0,0,_,_,_  ), 0                         , 89 , 0  , 189, 139), // #802
  INST(Tdpbusd          , AmxRmv             , V(660F38,5E,_,0,0,_,_,_  ), 0                         , 30 , 0  , 189, 139), // #803
  INST(Tdpbuud          , AmxRmv             , V(000F38,5E,_,0,0,_,_,_  ), 0                         , 11 , 0  , 189, 139), // #804
  INST(Tdpfp16ps        , AmxRmv             , V(F20F38,5C,_,0,0,_,_,_  ), 0                         , 85 , 0  , 189, 140), // #805
  INST(Test             , X86Test            , O(000000,84,_,_,x,_,_,_  ), O(000000,F6,_,_,x,_,_,_  ), 0  , 79 , 190, 1  ), // #806
  INST(Testui           , X86Op              , O(F30F01,ED,_,_,_,_,_,_  ), 0                         , 27 , 0  , 34 , 141), // #807
  INST(Tileloadd        , AmxRm              , V(F20F38,4B,_,0,0,_,_,_  ), 0                         , 85 , 0  , 191, 86 ), // #808
  INST(Tileloaddt1      , AmxRm              , V(660F38,4B,_,0,0,_,_,_  ), 0                         , 30 , 0  , 191, 86 ), // #809
  INST(Tilerelease      , VexOpMod           , V(000F38,49,0,0,0,_,_,_  ), 0                         , 11 , 0  , 192, 86 ), // #810
  INST(Tilestored       , AmxMr              , V(F30F38,4B,_,0,0,_,_,_  ), 0                         , 89 , 0  , 193, 86 ), // #811
  INST(Tilezero         , AmxR               , V(F20F38,49,_,0,0,_,_,_  ), 0                         , 85 , 0  , 194, 86 ), // #812
  INST(Tlbsync          , X86Op              , O(000F01,FF,_,_,_,_,_,_  ), 0                         , 23 , 0  , 31 , 68 ), // #813
  INST(Tpause           , X86R32_EDX_EAX     , O(660F00,AE,6,_,_,_,_,_  ), 0                         , 28 , 0  , 195, 142), // #814
  INST(Tzcnt            , X86Rm_Raw66H       , O(F30F00,BC,_,_,x,_,_,_  ), 0                         , 7  , 0  , 23 , 10 ), // #815
  INST(Tzmsk            , VexVm_Wx           , V(XOP_M9,01,4,0,x,_,_,_  ), 0                         , 100, 0  , 15 , 12 ), // #816
  INST(Ucomisd          , ExtRm              , O(660F00,2E,_,_,_,_,_,_  ), 0                         , 4  , 0  , 7  , 43 ), // #817
  INST(Ucomiss          , ExtRm              , O(000F00,2E,_,_,_,_,_,_  ), 0                         , 5  , 0  , 8  , 44 ), // #818
  INST(Ud0              , X86Rm              , O(000F00,FF,_,_,_,_,_,_  ), 0                         , 5  , 0  , 196, 0  ), // #819
  INST(Ud1              , X86Rm              , O(000F00,B9,_,_,_,_,_,_  ), 0                         , 5  , 0  , 196, 0  ), // #820
  INST(Ud2              , X86Op              , O(000F00,0B,_,_,_,_,_,_  ), 0                         , 5  , 0  , 31 , 0  ), // #821
  INST(Uiret            , X86Op              , O(F30F01,EC,_,_,_,_,_,_  ), 0                         , 27 , 0  , 34 , 26 ), // #822
  INST(Umonitor         , X86R_FromM         , O(F30F00,AE,6,_,_,_,_,_  ), 0                         , 26 , 0  , 197, 143), // #823
  INST(Umwait           , X86R32_EDX_EAX     , O(F20F00,AE,6,_,_,_,_,_  ), 0                         , 101, 0  , 195, 142), // #824
  INST(Unpckhpd         , ExtRm              , O(660F00,15,_,_,_,_,_,_  ), 0                         , 4  , 0  , 6  , 5  ), // #825
  INST(Unpckhps         , ExtRm              , O(000F00,15,_,_,_,_,_,_  ), 0                         , 5  , 0  , 6  , 6  ), // #826
  INST(Unpcklpd         , ExtRm              , O(660F00,14,_,_,_,_,_,_  ), 0                         , 4  , 0  , 6  , 5  ), // #827
  INST(Unpcklps         , ExtRm              , O(000F00,14,_,_,_,_,_,_  ), 0                         , 5  , 0  , 6  , 6  ), // #828
  INST(V4fmaddps        , VexRm_T1_4X        , E(F20F38,9A,_,2,_,0,4,T4X), 0                         , 102, 0  , 198, 144), // #829
  INST(V4fmaddss        , VexRm_T1_4X        , E(F20F38,9B,_,0,_,0,4,T4X), 0                         , 103, 0  , 199, 144), // #830
  INST(V4fnmaddps       , VexRm_T1_4X        , E(F20F38,AA,_,2,_,0,4,T4X), 0                         , 102, 0  , 198, 144), // #831
  INST(V4fnmaddss       , VexRm_T1_4X        , E(F20F38,AB,_,0,_,0,4,T4X), 0                         , 103, 0  , 199, 144), // #832
  INST(Vaddpd           , VexRvm_Lx          , V(660F00,58,_,x,I,1,4,FV ), 0                         , 104, 0  , 200, 145), // #833
  INST(Vaddph           , VexRvm_Lx          , E(00MAP5,58,_,_,_,0,4,FV ), 0                         , 105, 0  , 201, 146), // #834
  INST(Vaddps           , VexRvm_Lx          , V(000F00,58,_,x,I,0,4,FV ), 0                         , 106, 0  , 202, 145), // #835
  INST(Vaddsd           , VexRvm             , V(F20F00,58,_,I,I,1,3,T1S), 0                         , 107, 0  , 203, 147), // #836
  INST(Vaddsh           , VexRvm             , E(F3MAP5,58,_,_,_,0,1,T1S), 0                         , 108, 0  , 204, 148), // #837
  INST(Vaddss           , VexRvm             , V(F30F00,58,_,I,I,0,2,T1S), 0                         , 109, 0  , 205, 147), // #838
  INST(Vaddsubpd        , VexRvm_Lx          , V(660F00,D0,_,x,I,_,_,_  ), 0                         , 71 , 0  , 206, 149), // #839
  INST(Vaddsubps        , VexRvm_Lx          , V(F20F00,D0,_,x,I,_,_,_  ), 0                         , 110, 0  , 206, 149), // #840
  INST(Vaesdec          , VexRvm_Lx          , V(660F38,DE,_,x,I,_,4,FVM), 0                         , 111, 0  , 207, 150), // #841
  INST(Vaesdeclast      , VexRvm_Lx          , V(660F38,DF,_,x,I,_,4,FVM), 0                         , 111, 0  , 207, 150), // #842
  INST(Vaesenc          , VexRvm_Lx          , V(660F38,DC,_,x,I,_,4,FVM), 0                         , 111, 0  , 207, 150), // #843
  INST(Vaesenclast      , VexRvm_Lx          , V(660F38,DD,_,x,I,_,4,FVM), 0                         , 111, 0  , 207, 150), // #844
  INST(Vaesimc          , VexRm              , V(660F38,DB,_,0,I,_,_,_  ), 0                         , 30 , 0  , 208, 151), // #845
  INST(Vaeskeygenassist , VexRmi             , V(660F3A,DF,_,0,I,_,_,_  ), 0                         , 75 , 0  , 209, 151), // #846
  INST(Valignd          , VexRvmi_Lx         , E(660F3A,03,_,x,_,0,4,FV ), 0                         , 112, 0  , 210, 152), // #847
  INST(Valignq          , VexRvmi_Lx         , E(660F3A,03,_,x,_,1,4,FV ), 0                         , 113, 0  , 211, 152), // #848
  INST(Vandnpd          , VexRvm_Lx          , V(660F00,55,_,x,I,1,4,FV ), 0                         , 104, 0  , 212, 153), // #849
  INST(Vandnps          , VexRvm_Lx          , V(000F00,55,_,x,I,0,4,FV ), 0                         , 106, 0  , 213, 153), // #850
  INST(Vandpd           , VexRvm_Lx          , V(660F00,54,_,x,I,1,4,FV ), 0                         , 104, 0  , 214, 153), // #851
  INST(Vandps           , VexRvm_Lx          , V(000F00,54,_,x,I,0,4,FV ), 0                         , 106, 0  , 215, 153), // #852
  INST(Vbcstnebf162ps   , VexRm_Lx           , V(F30F38,B1,_,x,0,_,_,_  ), 0                         , 89 , 0  , 216, 154), // #853
  INST(Vbcstnesh2ps     , VexRm_Lx           , V(660F38,B1,_,x,0,_,_,_  ), 0                         , 30 , 0  , 216, 154), // #854
  INST(Vblendmpd        , VexRvm_Lx          , E(660F38,65,_,x,_,1,4,FV ), 0                         , 114, 0  , 217, 152), // #855
  INST(Vblendmps        , VexRvm_Lx          , E(660F38,65,_,x,_,0,4,FV ), 0                         , 115, 0  , 218, 152), // #856
  INST(Vblendpd         , VexRvmi_Lx         , V(660F3A,0D,_,x,I,_,_,_  ), 0                         , 75 , 0  , 219, 149), // #857
  INST(Vblendps         , VexRvmi_Lx         , V(660F3A,0C,_,x,I,_,_,_  ), 0                         , 75 , 0  , 219, 149), // #858
  INST(Vblendvpd        , VexRvmr_Lx         , V(660F3A,4B,_,x,0,_,_,_  ), 0                         , 75 , 0  , 220, 149), // #859
  INST(Vblendvps        , VexRvmr_Lx         , V(660F3A,4A,_,x,0,_,_,_  ), 0                         , 75 , 0  , 220, 149), // #860
  INST(Vbroadcastf128   , VexRm              , V(660F38,1A,_,1,0,_,_,_  ), 0                         , 116, 0  , 221, 149), // #861
  INST(Vbroadcastf32x2  , VexRm_Lx           , E(660F38,19,_,x,_,0,3,T2 ), 0                         , 117, 0  , 222, 155), // #862
  INST(Vbroadcastf32x4  , VexRm_Lx           , E(660F38,1A,_,x,_,0,4,T4 ), 0                         , 118, 0  , 223, 78 ), // #863
  INST(Vbroadcastf32x8  , VexRm              , E(660F38,1B,_,2,_,0,5,T8 ), 0                         , 119, 0  , 224, 76 ), // #864
  INST(Vbroadcastf64x2  , VexRm_Lx           , E(660F38,1A,_,x,_,1,4,T2 ), 0                         , 120, 0  , 223, 155), // #865
  INST(Vbroadcastf64x4  , VexRm              , E(660F38,1B,_,2,_,1,5,T4 ), 0                         , 121, 0  , 224, 78 ), // #866
  INST(Vbroadcasti128   , VexRm              , V(660F38,5A,_,1,0,_,_,_  ), 0                         , 116, 0  , 221, 156), // #867
  INST(Vbroadcasti32x2  , VexRm_Lx           , E(660F38,59,_,x,_,0,3,T2 ), 0                         , 117, 0  , 225, 155), // #868
  INST(Vbroadcasti32x4  , VexRm_Lx           , E(660F38,5A,_,x,_,0,4,T4 ), 0                         , 118, 0  , 223, 152), // #869
  INST(Vbroadcasti32x8  , VexRm              , E(660F38,5B,_,2,_,0,5,T8 ), 0                         , 119, 0  , 224, 76 ), // #870
  INST(Vbroadcasti64x2  , VexRm_Lx           , E(660F38,5A,_,x,_,1,4,T2 ), 0                         , 120, 0  , 223, 155), // #871
  INST(Vbroadcasti64x4  , VexRm              , E(660F38,5B,_,2,_,1,5,T4 ), 0                         , 121, 0  , 224, 78 ), // #872
  INST(Vbroadcastsd     , VexRm_Lx           , V(660F38,19,_,x,0,1,3,T1S), 0                         , 122, 0  , 226, 157), // #873
  INST(Vbroadcastss     , VexRm_Lx           , V(660F38,18,_,x,0,0,2,T1S), 0                         , 123, 0  , 227, 157), // #874
  INST(Vcmppd           , VexRvmi_Lx_KEvex   , V(660F00,C2,_,x,I,1,4,FV ), 0                         , 104, 0  , 228, 145), // #875
  INST(Vcmpph           , VexRvmi_Lx_KEvex   , E(000F3A,C2,_,_,_,0,4,FV ), 0                         , 124, 0  , 229, 146), // #876
  INST(Vcmpps           , VexRvmi_Lx_KEvex   , V(000F00,C2,_,x,I,0,4,FV ), 0                         , 106, 0  , 230, 145), // #877
  INST(Vcmpsd           , VexRvmi_KEvex      , V(F20F00,C2,_,I,I,1,3,T1S), 0                         , 107, 0  , 231, 147), // #878
  INST(Vcmpsh           , VexRvmi_KEvex      , E(F30F3A,C2,_,_,_,0,1,T1S), 0                         , 125, 0  , 232, 148), // #879
  INST(Vcmpss           , VexRvmi_KEvex      , V(F30F00,C2,_,I,I,0,2,T1S), 0                         , 109, 0  , 233, 147), // #880
  INST(Vcomisd          , VexRm              , V(660F00,2F,_,I,I,1,3,T1S), 0                         , 126, 0  , 234, 158), // #881
  INST(Vcomish          , VexRm              , E(00MAP5,2F,_,_,_,0,1,T1S), 0                         , 127, 0  , 235, 159), // #882
  INST(Vcomiss          , VexRm              , V(000F00,2F,_,I,I,0,2,T1S), 0                         , 128, 0  , 236, 158), // #883
  INST(Vcompresspd      , VexMr_Lx           , E(660F38,8A,_,x,_,1,3,T1S), 0                         , 129, 0  , 237, 152), // #884
  INST(Vcompressps      , VexMr_Lx           , E(660F38,8A,_,x,_,0,2,T1S), 0                         , 130, 0  , 237, 152), // #885
  INST(Vcvtdq2pd        , VexRm_Lx           , V(F30F00,E6,_,x,I,0,3,HV ), 0                         , 131, 0  , 238, 145), // #886
  INST(Vcvtdq2ph        , VexRm_Lx_Narrow    , E(00MAP5,5B,_,x,0,0,4,FV ), 0                         , 105, 0  , 239, 146), // #887
  INST(Vcvtdq2ps        , VexRm_Lx           , V(000F00,5B,_,x,I,0,4,FV ), 0                         , 106, 0  , 240, 145), // #888
  INST(Vcvtne2ps2bf16   , VexRvm_Lx          , E(F20F38,72,_,_,_,0,4,FV ), 0                         , 132, 0  , 218, 160), // #889
  INST(Vcvtneebf162ps   , VexRm_Lx           , V(F30F38,B0,_,x,0,_,_,_  ), 0                         , 89 , 0  , 241, 154), // #890
  INST(Vcvtneeph2ps     , VexRm_Lx           , V(660F38,B0,_,x,0,_,_,_  ), 0                         , 30 , 0  , 241, 154), // #891
  INST(Vcvtneobf162ps   , VexRm_Lx           , V(F20F38,B0,_,x,0,_,_,_  ), 0                         , 85 , 0  , 241, 154), // #892
  INST(Vcvtneoph2ps     , VexRm_Lx           , V(000F38,B0,_,x,0,_,_,_  ), 0                         , 11 , 0  , 241, 154), // #893
  INST(Vcvtneps2bf16    , VexRm_Lx_Narrow    , V(F30F38,72,_,_,_,0,4,FV ), 0                         , 133, 0  , 242, 161), // #894
  INST(Vcvtpd2dq        , VexRm_Lx_Narrow    , V(F20F00,E6,_,x,I,1,4,FV ), 0                         , 134, 0  , 243, 145), // #895
  INST(Vcvtpd2ph        , VexRm_Lx           , E(66MAP5,5A,_,_,_,1,4,FV ), 0                         , 135, 0  , 244, 146), // #896
  INST(Vcvtpd2ps        , VexRm_Lx_Narrow    , V(660F00,5A,_,x,I,1,4,FV ), 0                         , 104, 0  , 243, 145), // #897
  INST(Vcvtpd2qq        , VexRm_Lx           , E(660F00,7B,_,x,_,1,4,FV ), 0                         , 136, 0  , 245, 155), // #898
  INST(Vcvtpd2udq       , VexRm_Lx_Narrow    , E(000F00,79,_,x,_,1,4,FV ), 0                         , 137, 0  , 246, 152), // #899
  INST(Vcvtpd2uqq       , VexRm_Lx           , E(660F00,79,_,x,_,1,4,FV ), 0                         , 136, 0  , 245, 155), // #900
  INST(Vcvtph2dq        , VexRm_Lx           , E(66MAP5,5B,_,_,_,0,3,HV ), 0                         , 138, 0  , 247, 146), // #901
  INST(Vcvtph2pd        , VexRm_Lx           , E(00MAP5,5A,_,_,_,0,2,QV ), 0                         , 139, 0  , 248, 146), // #902
  INST(Vcvtph2ps        , VexRm_Lx           , V(660F38,13,_,x,0,0,3,HVM), 0                         , 140, 0  , 249, 162), // #903
  INST(Vcvtph2psx       , VexRm_Lx           , E(66MAP6,13,_,_,_,0,3,HV ), 0                         , 141, 0  , 250, 146), // #904
  INST(Vcvtph2qq        , VexRm_Lx           , E(66MAP5,7B,_,_,_,0,2,QV ), 0                         , 142, 0  , 251, 146), // #905
  INST(Vcvtph2udq       , VexRm_Lx           , E(00MAP5,79,_,_,_,0,3,HV ), 0                         , 143, 0  , 247, 146), // #906
  INST(Vcvtph2uqq       , VexRm_Lx           , E(66MAP5,79,_,_,_,0,2,QV ), 0                         , 142, 0  , 251, 146), // #907
  INST(Vcvtph2uw        , VexRm_Lx           , E(00MAP5,7D,_,_,_,0,4,FV ), 0                         , 105, 0  , 252, 146), // #908
  INST(Vcvtph2w         , VexRm_Lx           , E(66MAP5,7D,_,_,_,0,4,FV ), 0                         , 144, 0  , 252, 146), // #909
  INST(Vcvtps2dq        , VexRm_Lx           , V(660F00,5B,_,x,I,0,4,FV ), 0                         , 145, 0  , 240, 145), // #910
  INST(Vcvtps2pd        , VexRm_Lx           , V(000F00,5A,_,x,I,0,3,HV ), 0                         , 146, 0  , 253, 145), // #911
  INST(Vcvtps2ph        , VexMri_Lx          , V(660F3A,1D,_,x,0,0,3,HVM), 0                         , 147, 0  , 254, 162), // #912
  INST(Vcvtps2phx       , VexRm_Lx_Narrow    , E(66MAP5,1D,_,_,_,0,4,FV ), 0                         , 144, 0  , 239, 146), // #913
  INST(Vcvtps2qq        , VexRm_Lx           , E(660F00,7B,_,x,_,0,3,HV ), 0                         , 148, 0  , 255, 155), // #914
  INST(Vcvtps2udq       , VexRm_Lx           , E(000F00,79,_,x,_,0,4,FV ), 0                         , 149, 0  , 256, 152), // #915
  INST(Vcvtps2uqq       , VexRm_Lx           , E(660F00,79,_,x,_,0,3,HV ), 0                         , 148, 0  , 255, 155), // #916
  INST(Vcvtqq2pd        , VexRm_Lx           , E(F30F00,E6,_,x,_,1,4,FV ), 0                         , 150, 0  , 245, 155), // #917
  INST(Vcvtqq2ph        , VexRm_Lx           , E(00MAP5,5B,_,_,_,1,4,FV ), 0                         , 151, 0  , 244, 146), // #918
  INST(Vcvtqq2ps        , VexRm_Lx_Narrow    , E(000F00,5B,_,x,_,1,4,FV ), 0                         , 137, 0  , 246, 155), // #919
  INST(Vcvtsd2sh        , VexRvm             , E(F2MAP5,5A,_,_,_,1,3,T1S), 0                         , 152, 0  , 257, 148), // #920
  INST(Vcvtsd2si        , VexRm_Wx           , V(F20F00,2D,_,I,x,x,3,T1F), 0                         , 153, 0  , 258, 147), // #921
  INST(Vcvtsd2ss        , VexRvm             , V(F20F00,5A,_,I,I,1,3,T1S), 0                         , 107, 0  , 203, 147), // #922
  INST(Vcvtsd2usi       , VexRm_Wx           , E(F20F00,79,_,I,_,x,3,T1F), 0                         , 154, 0  , 259, 78 ), // #923
  INST(Vcvtsh2sd        , VexRvm             , E(F3MAP5,5A,_,_,_,0,1,T1S), 0                         , 108, 0  , 260, 148), // #924
  INST(Vcvtsh2si        , VexRm_Wx           , E(F3MAP5,2D,_,_,_,x,1,T1S), 0                         , 108, 0  , 261, 148), // #925
  INST(Vcvtsh2ss        , VexRvm             , E(00MAP6,13,_,_,_,0,1,T1S), 0                         , 155, 0  , 260, 148), // #926
  INST(Vcvtsh2usi       , VexRm_Wx           , E(F3MAP5,79,_,_,_,x,1,T1S), 0                         , 108, 0  , 261, 148), // #927
  INST(Vcvtsi2sd        , VexRvm_Wx          , V(F20F00,2A,_,I,x,x,2,T1W), 0                         , 156, 0  , 262, 147), // #928
  INST(Vcvtsi2sh        , VexRvm_Wx          , E(F3MAP5,2A,_,_,_,x,2,T1W), 0                         , 157, 0  , 263, 148), // #929
  INST(Vcvtsi2ss        , VexRvm_Wx          , V(F30F00,2A,_,I,x,x,2,T1W), 0                         , 158, 0  , 262, 147), // #930
  INST(Vcvtss2sd        , VexRvm             , V(F30F00,5A,_,I,I,0,2,T1S), 0                         , 109, 0  , 264, 147), // #931
  INST(Vcvtss2sh        , VexRvm             , E(00MAP5,1D,_,_,_,0,2,T1S), 0                         , 159, 0  , 265, 148), // #932
  INST(Vcvtss2si        , VexRm_Wx           , V(F30F00,2D,_,I,x,x,2,T1F), 0                         , 109, 0  , 266, 147), // #933
  INST(Vcvtss2usi       , VexRm_Wx           , E(F30F00,79,_,I,_,x,2,T1F), 0                         , 160, 0  , 267, 78 ), // #934
  INST(Vcvttpd2dq       , VexRm_Lx_Narrow    , V(660F00,E6,_,x,I,1,4,FV ), 0                         , 104, 0  , 268, 145), // #935
  INST(Vcvttpd2qq       , VexRm_Lx           , E(660F00,7A,_,x,_,1,4,FV ), 0                         , 136, 0  , 269, 152), // #936
  INST(Vcvttpd2udq      , VexRm_Lx_Narrow    , E(000F00,78,_,x,_,1,4,FV ), 0                         , 137, 0  , 270, 152), // #937
  INST(Vcvttpd2uqq      , VexRm_Lx           , E(660F00,78,_,x,_,1,4,FV ), 0                         , 136, 0  , 269, 155), // #938
  INST(Vcvttph2dq       , VexRm_Lx           , E(F3MAP5,5B,_,_,_,0,3,HV ), 0                         , 161, 0  , 250, 146), // #939
  INST(Vcvttph2qq       , VexRm_Lx           , E(66MAP5,7A,_,_,_,0,2,QV ), 0                         , 142, 0  , 248, 146), // #940
  INST(Vcvttph2udq      , VexRm_Lx           , E(00MAP5,78,_,_,_,0,3,HV ), 0                         , 143, 0  , 250, 146), // #941
  INST(Vcvttph2uqq      , VexRm_Lx           , E(66MAP5,78,_,_,_,0,2,QV ), 0                         , 142, 0  , 248, 146), // #942
  INST(Vcvttph2uw       , VexRm_Lx           , E(00MAP5,7C,_,_,_,0,4,FV ), 0                         , 105, 0  , 271, 146), // #943
  INST(Vcvttph2w        , VexRm_Lx           , E(66MAP5,7C,_,_,_,0,4,FV ), 0                         , 144, 0  , 271, 146), // #944
  INST(Vcvttps2dq       , VexRm_Lx           , V(F30F00,5B,_,x,I,0,4,FV ), 0                         , 162, 0  , 272, 145), // #945
  INST(Vcvttps2qq       , VexRm_Lx           , E(660F00,7A,_,x,_,0,3,HV ), 0                         , 148, 0  , 273, 155), // #946
  INST(Vcvttps2udq      , VexRm_Lx           , E(000F00,78,_,x,_,0,4,FV ), 0                         , 149, 0  , 274, 152), // #947
  INST(Vcvttps2uqq      , VexRm_Lx           , E(660F00,78,_,x,_,0,3,HV ), 0                         , 148, 0  , 273, 155), // #948
  INST(Vcvttsd2si       , VexRm_Wx           , V(F20F00,2C,_,I,x,x,3,T1F), 0                         , 153, 0  , 275, 147), // #949
  INST(Vcvttsd2usi      , VexRm_Wx           , E(F20F00,78,_,I,_,x,3,T1F), 0                         , 154, 0  , 276, 78 ), // #950
  INST(Vcvttsh2si       , VexRm_Wx           , E(F3MAP5,2C,_,_,_,x,1,T1S), 0                         , 108, 0  , 277, 148), // #951
  INST(Vcvttsh2usi      , VexRm_Wx           , E(F3MAP5,78,_,_,_,x,1,T1S), 0                         , 108, 0  , 277, 148), // #952
  INST(Vcvttss2si       , VexRm_Wx           , V(F30F00,2C,_,I,x,x,2,T1F), 0                         , 109, 0  , 278, 147), // #953
  INST(Vcvttss2usi      , VexRm_Wx           , E(F30F00,78,_,I,_,x,2,T1F), 0                         , 160, 0  , 279, 78 ), // #954
  INST(Vcvtudq2pd       , VexRm_Lx           , E(F30F00,7A,_,x,_,0,3,HV ), 0                         , 163, 0  , 280, 152), // #955
  INST(Vcvtudq2ph       , VexRm_Lx_Narrow    , E(F2MAP5,7A,_,_,_,0,4,FV ), 0                         , 164, 0  , 239, 146), // #956
  INST(Vcvtudq2ps       , VexRm_Lx           , E(F20F00,7A,_,x,_,0,4,FV ), 0                         , 165, 0  , 256, 152), // #957
  INST(Vcvtuqq2pd       , VexRm_Lx           , E(F30F00,7A,_,x,_,1,4,FV ), 0                         , 150, 0  , 245, 155), // #958
  INST(Vcvtuqq2ph       , VexRm_Lx           , E(F2MAP5,7A,_,_,_,1,4,FV ), 0                         , 166, 0  , 244, 146), // #959
  INST(Vcvtuqq2ps       , VexRm_Lx_Narrow    , E(F20F00,7A,_,x,_,1,4,FV ), 0                         , 167, 0  , 246, 155), // #960
  INST(Vcvtusi2sd       , VexRvm_Wx          , E(F20F00,7B,_,I,_,x,2,T1W), 0                         , 168, 0  , 281, 78 ), // #961
  INST(Vcvtusi2sh       , VexRvm_Wx          , E(F3MAP5,7B,_,_,_,x,2,T1W), 0                         , 157, 0  , 263, 148), // #962
  INST(Vcvtusi2ss       , VexRvm_Wx          , E(F30F00,7B,_,I,_,x,2,T1W), 0                         , 169, 0  , 281, 78 ), // #963
  INST(Vcvtuw2ph        , VexRm_Lx           , E(F2MAP5,7D,_,_,_,0,4,FV ), 0                         , 164, 0  , 252, 146), // #964
  INST(Vcvtw2ph         , VexRm_Lx           , E(F3MAP5,7D,_,_,_,0,4,FV ), 0                         , 170, 0  , 252, 146), // #965
  INST(Vdbpsadbw        , VexRvmi_Lx         , E(660F3A,42,_,x,_,0,4,FVM), 0                         , 112, 0  , 282, 163), // #966
  INST(Vdivpd           , VexRvm_Lx          , V(660F00,5E,_,x,I,1,4,FV ), 0                         , 104, 0  , 200, 145), // #967
  INST(Vdivph           , VexRvm_Lx          , E(00MAP5,5E,_,_,_,0,4,FV ), 0                         , 105, 0  , 201, 146), // #968
  INST(Vdivps           , VexRvm_Lx          , V(000F00,5E,_,x,I,0,4,FV ), 0                         , 106, 0  , 202, 145), // #969
  INST(Vdivsd           , VexRvm             , V(F20F00,5E,_,I,I,1,3,T1S), 0                         , 107, 0  , 203, 147), // #970
  INST(Vdivsh           , VexRvm             , E(F3MAP5,5E,_,_,_,0,1,T1S), 0                         , 108, 0  , 204, 148), // #971
  INST(Vdivss           , VexRvm             , V(F30F00,5E,_,I,I,0,2,T1S), 0                         , 109, 0  , 205, 147), // #972
  INST(Vdpbf16ps        , VexRvm_Lx          , E(F30F38,52,_,_,_,0,4,FV ), 0                         , 171, 0  , 218, 160), // #973
  INST(Vdppd            , VexRvmi_Lx         , V(660F3A,41,_,x,I,_,_,_  ), 0                         , 75 , 0  , 283, 149), // #974
  INST(Vdpps            , VexRvmi_Lx         , V(660F3A,40,_,x,I,_,_,_  ), 0                         , 75 , 0  , 219, 149), // #975
  INST(Verr             , X86M_NoSize        , O(000F00,00,4,_,_,_,_,_  ), 0                         , 98 , 0  , 112, 11 ), // #976
  INST(Verw             , X86M_NoSize        , O(000F00,00,5,_,_,_,_,_  ), 0                         , 79 , 0  , 112, 11 ), // #977
  INST(Vexp2pd          , VexRm              , E(660F38,C8,_,2,_,1,4,FV ), 0                         , 172, 0  , 284, 164), // #978
  INST(Vexp2ps          , VexRm              , E(660F38,C8,_,2,_,0,4,FV ), 0                         , 173, 0  , 285, 164), // #979
  INST(Vexpandpd        , VexRm_Lx           , E(660F38,88,_,x,_,1,3,T1S), 0                         , 129, 0  , 286, 152), // #980
  INST(Vexpandps        , VexRm_Lx           , E(660F38,88,_,x,_,0,2,T1S), 0                         , 130, 0  , 286, 152), // #981
  INST(Vextractf128     , VexMri             , V(660F3A,19,_,1,0,_,_,_  ), 0                         , 174, 0  , 287, 149), // #982
  INST(Vextractf32x4    , VexMri_Lx          , E(660F3A,19,_,x,_,0,4,T4 ), 0                         , 175, 0  , 288, 152), // #983
  INST(Vextractf32x8    , VexMri             , E(660F3A,1B,_,2,_,0,5,T8 ), 0                         , 176, 0  , 289, 76 ), // #984
  INST(Vextractf64x2    , VexMri_Lx          , E(660F3A,19,_,x,_,1,4,T2 ), 0                         , 177, 0  , 288, 155), // #985
  INST(Vextractf64x4    , VexMri             , E(660F3A,1B,_,2,_,1,5,T4 ), 0                         , 178, 0  , 289, 78 ), // #986
  INST(Vextracti128     , VexMri             , V(660F3A,39,_,1,0,_,_,_  ), 0                         , 174, 0  , 287, 156), // #987
  INST(Vextracti32x4    , VexMri_Lx          , E(660F3A,39,_,x,_,0,4,T4 ), 0                         , 175, 0  , 288, 152), // #988
  INST(Vextracti32x8    , VexMri             , E(660F3A,3B,_,2,_,0,5,T8 ), 0                         , 176, 0  , 289, 76 ), // #989
  INST(Vextracti64x2    , VexMri_Lx          , E(660F3A,39,_,x,_,1,4,T2 ), 0                         , 177, 0  , 288, 155), // #990
  INST(Vextracti64x4    , VexMri             , E(660F3A,3B,_,2,_,1,5,T4 ), 0                         , 178, 0  , 289, 78 ), // #991
  INST(Vextractps       , VexMri             , V(660F3A,17,_,0,I,I,2,T1S), 0                         , 179, 0  , 290, 147), // #992
  INST(Vfcmaddcph       , VexRvm_Lx          , E(F2MAP6,56,_,_,_,0,4,FV ), 0                         , 180, 0  , 291, 146), // #993
  INST(Vfcmaddcsh       , VexRvm             , E(F2MAP6,57,_,_,_,0,2,T1S), 0                         , 181, 0  , 265, 148), // #994
  INST(Vfcmulcph        , VexRvm_Lx          , E(F2MAP6,D6,_,_,_,0,4,FV ), 0                         , 180, 0  , 291, 146), // #995
  INST(Vfcmulcsh        , VexRvm             , E(F2MAP6,D7,_,_,_,0,2,T1S), 0                         , 181, 0  , 265, 148), // #996
  INST(Vfixupimmpd      , VexRvmi_Lx         , E(660F3A,54,_,x,_,1,4,FV ), 0                         , 113, 0  , 292, 152), // #997
  INST(Vfixupimmps      , VexRvmi_Lx         , E(660F3A,54,_,x,_,0,4,FV ), 0                         , 112, 0  , 293, 152), // #998
  INST(Vfixupimmsd      , VexRvmi            , E(660F3A,55,_,I,_,1,3,T1S), 0                         , 182, 0  , 294, 78 ), // #999
  INST(Vfixupimmss      , VexRvmi            , E(660F3A,55,_,I,_,0,2,T1S), 0                         , 183, 0  , 295, 78 ), // #1000
  INST(Vfmadd132pd      , VexRvm_Lx          , V(660F38,98,_,x,1,1,4,FV ), 0                         , 184, 0  , 200, 165), // #1001
  INST(Vfmadd132ph      , VexRvm_Lx          , E(66MAP6,98,_,_,_,0,4,FV ), 0                         , 185, 0  , 201, 146), // #1002
  INST(Vfmadd132ps      , VexRvm_Lx          , V(660F38,98,_,x,0,0,4,FV ), 0                         , 111, 0  , 202, 165), // #1003
  INST(Vfmadd132sd      , VexRvm             , V(660F38,99,_,I,1,1,3,T1S), 0                         , 186, 0  , 203, 166), // #1004
  INST(Vfmadd132sh      , VexRvm             , E(66MAP6,99,_,_,_,0,1,T1S), 0                         , 187, 0  , 204, 148), // #1005
  INST(Vfmadd132ss      , VexRvm             , V(660F38,99,_,I,0,0,2,T1S), 0                         , 123, 0  , 205, 166), // #1006
  INST(Vfmadd213pd      , VexRvm_Lx          , V(660F38,A8,_,x,1,1,4,FV ), 0                         , 184, 0  , 200, 165), // #1007
  INST(Vfmadd213ph      , VexRvm_Lx          , E(66MAP6,A8,_,_,_,0,4,FV ), 0                         , 185, 0  , 201, 146), // #1008
  INST(Vfmadd213ps      , VexRvm_Lx          , V(660F38,A8,_,x,0,0,4,FV ), 0                         , 111, 0  , 202, 165), // #1009
  INST(Vfmadd213sd      , VexRvm             , V(660F38,A9,_,I,1,1,3,T1S), 0                         , 186, 0  , 203, 166), // #1010
  INST(Vfmadd213sh      , VexRvm             , E(66MAP6,A9,_,_,_,0,1,T1S), 0                         , 187, 0  , 204, 148), // #1011
  INST(Vfmadd213ss      , VexRvm             , V(660F38,A9,_,I,0,0,2,T1S), 0                         , 123, 0  , 205, 166), // #1012
  INST(Vfmadd231pd      , VexRvm_Lx          , V(660F38,B8,_,x,1,1,4,FV ), 0                         , 184, 0  , 200, 165), // #1013
  INST(Vfmadd231ph      , VexRvm_Lx          , E(66MAP6,B8,_,_,_,0,4,FV ), 0                         , 185, 0  , 201, 146), // #1014
  INST(Vfmadd231ps      , VexRvm_Lx          , V(660F38,B8,_,x,0,0,4,FV ), 0                         , 111, 0  , 202, 165), // #1015
  INST(Vfmadd231sd      , VexRvm             , V(660F38,B9,_,I,1,1,3,T1S), 0                         , 186, 0  , 203, 166), // #1016
  INST(Vfmadd231sh      , VexRvm             , E(66MAP6,B9,_,_,_,0,1,T1S), 0                         , 187, 0  , 204, 148), // #1017
  INST(Vfmadd231ss      , VexRvm             , V(660F38,B9,_,I,0,0,2,T1S), 0                         , 123, 0  , 205, 166), // #1018
  INST(Vfmaddcph        , VexRvm_Lx          , E(F3MAP6,56,_,_,_,0,4,FV ), 0                         , 188, 0  , 291, 146), // #1019
  INST(Vfmaddcsh        , VexRvm             , E(F3MAP6,57,_,_,_,0,2,T1S), 0                         , 189, 0  , 265, 148), // #1020
  INST(Vfmaddpd         , Fma4_Lx            , V(660F3A,69,_,x,x,_,_,_  ), 0                         , 75 , 0  , 296, 167), // #1021
  INST(Vfmaddps         , Fma4_Lx            , V(660F3A,68,_,x,x,_,_,_  ), 0                         , 75 , 0  , 296, 167), // #1022
  INST(Vfmaddsd         , Fma4               , V(660F3A,6B,_,0,x,_,_,_  ), 0                         , 75 , 0  , 297, 167), // #1023
  INST(Vfmaddss         , Fma4               , V(660F3A,6A,_,0,x,_,_,_  ), 0                         , 75 , 0  , 298, 167), // #1024
  INST(Vfmaddsub132pd   , VexRvm_Lx          , V(660F38,96,_,x,1,1,4,FV ), 0                         , 184, 0  , 200, 165), // #1025
  INST(Vfmaddsub132ph   , VexRvm_Lx          , E(66MAP6,96,_,_,_,0,4,FV ), 0                         , 185, 0  , 201, 146), // #1026
  INST(Vfmaddsub132ps   , VexRvm_Lx          , V(660F38,96,_,x,0,0,4,FV ), 0                         , 111, 0  , 202, 165), // #1027
  INST(Vfmaddsub213pd   , VexRvm_Lx          , V(660F38,A6,_,x,1,1,4,FV ), 0                         , 184, 0  , 200, 165), // #1028
  INST(Vfmaddsub213ph   , VexRvm_Lx          , E(66MAP6,A6,_,_,_,0,4,FV ), 0                         , 185, 0  , 201, 146), // #1029
  INST(Vfmaddsub213ps   , VexRvm_Lx          , V(660F38,A6,_,x,0,0,4,FV ), 0                         , 111, 0  , 202, 165), // #1030
  INST(Vfmaddsub231pd   , VexRvm_Lx          , V(660F38,B6,_,x,1,1,4,FV ), 0                         , 184, 0  , 200, 165), // #1031
  INST(Vfmaddsub231ph   , VexRvm_Lx          , E(66MAP6,B6,_,_,_,0,4,FV ), 0                         , 185, 0  , 201, 146), // #1032
  INST(Vfmaddsub231ps   , VexRvm_Lx          , V(660F38,B6,_,x,0,0,4,FV ), 0                         , 111, 0  , 202, 165), // #1033
  INST(Vfmaddsubpd      , Fma4_Lx            , V(660F3A,5D,_,x,x,_,_,_  ), 0                         , 75 , 0  , 296, 167), // #1034
  INST(Vfmaddsubps      , Fma4_Lx            , V(660F3A,5C,_,x,x,_,_,_  ), 0                         , 75 , 0  , 296, 167), // #1035
  INST(Vfmsub132pd      , VexRvm_Lx          , V(660F38,9A,_,x,1,1,4,FV ), 0                         , 184, 0  , 200, 165), // #1036
  INST(Vfmsub132ph      , VexRvm_Lx          , E(66MAP6,9A,_,_,_,0,4,FV ), 0                         , 185, 0  , 201, 146), // #1037
  INST(Vfmsub132ps      , VexRvm_Lx          , V(660F38,9A,_,x,0,0,4,FV ), 0                         , 111, 0  , 202, 165), // #1038
  INST(Vfmsub132sd      , VexRvm             , V(660F38,9B,_,I,1,1,3,T1S), 0                         , 186, 0  , 203, 166), // #1039
  INST(Vfmsub132sh      , VexRvm             , E(66MAP6,9B,_,_,_,0,1,T1S), 0                         , 187, 0  , 204, 148), // #1040
  INST(Vfmsub132ss      , VexRvm             , V(660F38,9B,_,I,0,0,2,T1S), 0                         , 123, 0  , 205, 166), // #1041
  INST(Vfmsub213pd      , VexRvm_Lx          , V(660F38,AA,_,x,1,1,4,FV ), 0                         , 184, 0  , 200, 165), // #1042
  INST(Vfmsub213ph      , VexRvm_Lx          , E(66MAP6,AA,_,_,_,0,4,FV ), 0                         , 185, 0  , 201, 146), // #1043
  INST(Vfmsub213ps      , VexRvm_Lx          , V(660F38,AA,_,x,0,0,4,FV ), 0                         , 111, 0  , 202, 165), // #1044
  INST(Vfmsub213sd      , VexRvm             , V(660F38,AB,_,I,1,1,3,T1S), 0                         , 186, 0  , 203, 166), // #1045
  INST(Vfmsub213sh      , VexRvm             , E(66MAP6,AB,_,_,_,0,1,T1S), 0                         , 187, 0  , 204, 148), // #1046
  INST(Vfmsub213ss      , VexRvm             , V(660F38,AB,_,I,0,0,2,T1S), 0                         , 123, 0  , 205, 166), // #1047
  INST(Vfmsub231pd      , VexRvm_Lx          , V(660F38,BA,_,x,1,1,4,FV ), 0                         , 184, 0  , 200, 165), // #1048
  INST(Vfmsub231ph      , VexRvm_Lx          , E(66MAP6,BA,_,_,_,0,4,FV ), 0                         , 185, 0  , 201, 146), // #1049
  INST(Vfmsub231ps      , VexRvm_Lx          , V(660F38,BA,_,x,0,0,4,FV ), 0                         , 111, 0  , 202, 165), // #1050
  INST(Vfmsub231sd      , VexRvm             , V(660F38,BB,_,I,1,1,3,T1S), 0                         , 186, 0  , 203, 166), // #1051
  INST(Vfmsub231sh      , VexRvm             , E(66MAP6,BB,_,_,_,0,1,T1S), 0                         , 187, 0  , 204, 148), // #1052
  INST(Vfmsub231ss      , VexRvm             , V(660F38,BB,_,I,0,0,2,T1S), 0                         , 123, 0  , 205, 166), // #1053
  INST(Vfmsubadd132pd   , VexRvm_Lx          , V(660F38,97,_,x,1,1,4,FV ), 0                         , 184, 0  , 200, 165), // #1054
  INST(Vfmsubadd132ph   , VexRvm_Lx          , E(66MAP6,97,_,_,_,0,4,FV ), 0                         , 185, 0  , 201, 146), // #1055
  INST(Vfmsubadd132ps   , VexRvm_Lx          , V(660F38,97,_,x,0,0,4,FV ), 0                         , 111, 0  , 202, 165), // #1056
  INST(Vfmsubadd213pd   , VexRvm_Lx          , V(660F38,A7,_,x,1,1,4,FV ), 0                         , 184, 0  , 200, 165), // #1057
  INST(Vfmsubadd213ph   , VexRvm_Lx          , E(66MAP6,A7,_,_,_,0,4,FV ), 0                         , 185, 0  , 201, 146), // #1058
  INST(Vfmsubadd213ps   , VexRvm_Lx          , V(660F38,A7,_,x,0,0,4,FV ), 0                         , 111, 0  , 202, 165), // #1059
  INST(Vfmsubadd231pd   , VexRvm_Lx          , V(660F38,B7,_,x,1,1,4,FV ), 0                         , 184, 0  , 200, 165), // #1060
  INST(Vfmsubadd231ph   , VexRvm_Lx          , E(66MAP6,B7,_,_,_,0,4,FV ), 0                         , 185, 0  , 201, 146), // #1061
  INST(Vfmsubadd231ps   , VexRvm_Lx          , V(660F38,B7,_,x,0,0,4,FV ), 0                         , 111, 0  , 202, 165), // #1062
  INST(Vfmsubaddpd      , Fma4_Lx            , V(660F3A,5F,_,x,x,_,_,_  ), 0                         , 75 , 0  , 296, 167), // #1063
  INST(Vfmsubaddps      , Fma4_Lx            , V(660F3A,5E,_,x,x,_,_,_  ), 0                         , 75 , 0  , 296, 167), // #1064
  INST(Vfmsubpd         , Fma4_Lx            , V(660F3A,6D,_,x,x,_,_,_  ), 0                         , 75 , 0  , 296, 167), // #1065
  INST(Vfmsubps         , Fma4_Lx            , V(660F3A,6C,_,x,x,_,_,_  ), 0                         , 75 , 0  , 296, 167), // #1066
  INST(Vfmsubsd         , Fma4               , V(660F3A,6F,_,0,x,_,_,_  ), 0                         , 75 , 0  , 297, 167), // #1067
  INST(Vfmsubss         , Fma4               , V(660F3A,6E,_,0,x,_,_,_  ), 0                         , 75 , 0  , 298, 167), // #1068
  INST(Vfmulcph         , VexRvm_Lx          , E(F3MAP6,D6,_,_,_,0,4,FV ), 0                         , 188, 0  , 291, 146), // #1069
  INST(Vfmulcsh         , VexRvm             , E(F3MAP6,D7,_,_,_,0,2,T1S), 0                         , 189, 0  , 265, 146), // #1070
  INST(Vfnmadd132pd     , VexRvm_Lx          , V(660F38,9C,_,x,1,1,4,FV ), 0                         , 184, 0  , 200, 165), // #1071
  INST(Vfnmadd132ph     , VexRvm_Lx          , E(66MAP6,9C,_,_,_,0,4,FV ), 0                         , 185, 0  , 201, 146), // #1072
  INST(Vfnmadd132ps     , VexRvm_Lx          , V(660F38,9C,_,x,0,0,4,FV ), 0                         , 111, 0  , 202, 165), // #1073
  INST(Vfnmadd132sd     , VexRvm             , V(660F38,9D,_,I,1,1,3,T1S), 0                         , 186, 0  , 203, 166), // #1074
  INST(Vfnmadd132sh     , VexRvm             , E(66MAP6,9D,_,_,_,0,1,T1S), 0                         , 187, 0  , 204, 148), // #1075
  INST(Vfnmadd132ss     , VexRvm             , V(660F38,9D,_,I,0,0,2,T1S), 0                         , 123, 0  , 205, 166), // #1076
  INST(Vfnmadd213pd     , VexRvm_Lx          , V(660F38,AC,_,x,1,1,4,FV ), 0                         , 184, 0  , 200, 165), // #1077
  INST(Vfnmadd213ph     , VexRvm_Lx          , E(66MAP6,AC,_,_,_,0,4,FV ), 0                         , 185, 0  , 201, 146), // #1078
  INST(Vfnmadd213ps     , VexRvm_Lx          , V(660F38,AC,_,x,0,0,4,FV ), 0                         , 111, 0  , 202, 165), // #1079
  INST(Vfnmadd213sd     , VexRvm             , V(660F38,AD,_,I,1,1,3,T1S), 0                         , 186, 0  , 203, 166), // #1080
  INST(Vfnmadd213sh     , VexRvm             , E(66MAP6,AD,_,_,_,0,1,T1S), 0                         , 187, 0  , 204, 148), // #1081
  INST(Vfnmadd213ss     , VexRvm             , V(660F38,AD,_,I,0,0,2,T1S), 0                         , 123, 0  , 205, 166), // #1082
  INST(Vfnmadd231pd     , VexRvm_Lx          , V(660F38,BC,_,x,1,1,4,FV ), 0                         , 184, 0  , 200, 165), // #1083
  INST(Vfnmadd231ph     , VexRvm_Lx          , E(66MAP6,BC,_,_,_,0,4,FV ), 0                         , 185, 0  , 201, 146), // #1084
  INST(Vfnmadd231ps     , VexRvm_Lx          , V(660F38,BC,_,x,0,0,4,FV ), 0                         , 111, 0  , 202, 165), // #1085
  INST(Vfnmadd231sd     , VexRvm             , V(660F38,BD,_,I,1,1,3,T1S), 0                         , 186, 0  , 203, 166), // #1086
  INST(Vfnmadd231sh     , VexRvm             , E(66MAP6,BD,_,_,_,0,1,T1S), 0                         , 187, 0  , 204, 148), // #1087
  INST(Vfnmadd231ss     , VexRvm             , V(660F38,BD,_,I,0,0,2,T1S), 0                         , 123, 0  , 205, 166), // #1088
  INST(Vfnmaddpd        , Fma4_Lx            , V(660F3A,79,_,x,x,_,_,_  ), 0                         , 75 , 0  , 296, 167), // #1089
  INST(Vfnmaddps        , Fma4_Lx            , V(660F3A,78,_,x,x,_,_,_  ), 0                         , 75 , 0  , 296, 167), // #1090
  INST(Vfnmaddsd        , Fma4               , V(660F3A,7B,_,0,x,_,_,_  ), 0                         , 75 , 0  , 297, 167), // #1091
  INST(Vfnmaddss        , Fma4               , V(660F3A,7A,_,0,x,_,_,_  ), 0                         , 75 , 0  , 298, 167), // #1092
  INST(Vfnmsub132pd     , VexRvm_Lx          , V(660F38,9E,_,x,1,1,4,FV ), 0                         , 184, 0  , 200, 165), // #1093
  INST(Vfnmsub132ph     , VexRvm_Lx          , E(66MAP6,9E,_,_,_,0,4,FV ), 0                         , 185, 0  , 201, 146), // #1094
  INST(Vfnmsub132ps     , VexRvm_Lx          , V(660F38,9E,_,x,0,0,4,FV ), 0                         , 111, 0  , 202, 165), // #1095
  INST(Vfnmsub132sd     , VexRvm             , V(660F38,9F,_,I,1,1,3,T1S), 0                         , 186, 0  , 203, 166), // #1096
  INST(Vfnmsub132sh     , VexRvm             , E(66MAP6,9F,_,_,_,0,1,T1S), 0                         , 187, 0  , 204, 148), // #1097
  INST(Vfnmsub132ss     , VexRvm             , V(660F38,9F,_,I,0,0,2,T1S), 0                         , 123, 0  , 205, 166), // #1098
  INST(Vfnmsub213pd     , VexRvm_Lx          , V(660F38,AE,_,x,1,1,4,FV ), 0                         , 184, 0  , 200, 165), // #1099
  INST(Vfnmsub213ph     , VexRvm_Lx          , E(66MAP6,AE,_,_,_,0,4,FV ), 0                         , 185, 0  , 201, 146), // #1100
  INST(Vfnmsub213ps     , VexRvm_Lx          , V(660F38,AE,_,x,0,0,4,FV ), 0                         , 111, 0  , 202, 165), // #1101
  INST(Vfnmsub213sd     , VexRvm             , V(660F38,AF,_,I,1,1,3,T1S), 0                         , 186, 0  , 203, 166), // #1102
  INST(Vfnmsub213sh     , VexRvm             , E(66MAP6,AF,_,_,_,0,1,T1S), 0                         , 187, 0  , 204, 148), // #1103
  INST(Vfnmsub213ss     , VexRvm             , V(660F38,AF,_,I,0,0,2,T1S), 0                         , 123, 0  , 205, 166), // #1104
  INST(Vfnmsub231pd     , VexRvm_Lx          , V(660F38,BE,_,x,1,1,4,FV ), 0                         , 184, 0  , 200, 165), // #1105
  INST(Vfnmsub231ph     , VexRvm_Lx          , E(66MAP6,BE,_,_,_,0,4,FV ), 0                         , 185, 0  , 201, 146), // #1106
  INST(Vfnmsub231ps     , VexRvm_Lx          , V(660F38,BE,_,x,0,0,4,FV ), 0                         , 111, 0  , 202, 165), // #1107
  INST(Vfnmsub231sd     , VexRvm             , V(660F38,BF,_,I,1,1,3,T1S), 0                         , 186, 0  , 203, 166), // #1108
  INST(Vfnmsub231sh     , VexRvm             , E(66MAP6,BF,_,_,_,0,1,T1S), 0                         , 187, 0  , 204, 148), // #1109
  INST(Vfnmsub231ss     , VexRvm             , V(660F38,BF,_,I,0,0,2,T1S), 0                         , 123, 0  , 205, 166), // #1110
  INST(Vfnmsubpd        , Fma4_Lx            , V(660F3A,7D,_,x,x,_,_,_  ), 0                         , 75 , 0  , 296, 167), // #1111
  INST(Vfnmsubps        , Fma4_Lx            , V(660F3A,7C,_,x,x,_,_,_  ), 0                         , 75 , 0  , 296, 167), // #1112
  INST(Vfnmsubsd        , Fma4               , V(660F3A,7F,_,0,x,_,_,_  ), 0                         , 75 , 0  , 297, 167), // #1113
  INST(Vfnmsubss        , Fma4               , V(660F3A,7E,_,0,x,_,_,_  ), 0                         , 75 , 0  , 298, 167), // #1114
  INST(Vfpclasspd       , VexRmi_Lx          , E(660F3A,66,_,x,_,1,4,FV ), 0                         , 113, 0  , 299, 155), // #1115
  INST(Vfpclassph       , VexRmi_Lx          , E(000F3A,66,_,_,_,0,4,FV ), 0                         , 124, 0  , 300, 146), // #1116
  INST(Vfpclassps       , VexRmi_Lx          , E(660F3A,66,_,x,_,0,4,FV ), 0                         , 112, 0  , 301, 155), // #1117
  INST(Vfpclasssd       , VexRmi             , E(660F3A,67,_,I,_,1,3,T1S), 0                         , 182, 0  , 302, 76 ), // #1118
  INST(Vfpclasssh       , VexRmi             , E(000F3A,67,_,_,_,0,1,T1S), 0                         , 190, 0  , 303, 148), // #1119
  INST(Vfpclassss       , VexRmi             , E(660F3A,67,_,I,_,0,2,T1S), 0                         , 183, 0  , 304, 76 ), // #1120
  INST(Vfrczpd          , VexRm_Lx           , V(XOP_M9,81,_,x,0,_,_,_  ), 0                         , 81 , 0  , 305, 168), // #1121
  INST(Vfrczps          , VexRm_Lx           , V(XOP_M9,80,_,x,0,_,_,_  ), 0                         , 81 , 0  , 305, 168), // #1122
  INST(Vfrczsd          , VexRm              , V(XOP_M9,83,_,0,0,_,_,_  ), 0                         , 81 , 0  , 306, 168), // #1123
  INST(Vfrczss          , VexRm              , V(XOP_M9,82,_,0,0,_,_,_  ), 0                         , 81 , 0  , 307, 168), // #1124
  INST(Vgatherdpd       , VexRmvRm_VM        , V(660F38,92,_,x,1,_,_,_  ), E(660F38,92,_,x,_,1,3,T1S), 191, 80 , 308, 169), // #1125
  INST(Vgatherdps       , VexRmvRm_VM        , V(660F38,92,_,x,0,_,_,_  ), E(660F38,92,_,x,_,0,2,T1S), 30 , 81 , 309, 169), // #1126
  INST(Vgatherpf0dpd    , VexM_VM            , E(660F38,C6,1,2,_,1,3,T1S), 0                         , 192, 0  , 310, 170), // #1127
  INST(Vgatherpf0dps    , VexM_VM            , E(660F38,C6,1,2,_,0,2,T1S), 0                         , 193, 0  , 311, 170), // #1128
  INST(Vgatherpf0qpd    , VexM_VM            , E(660F38,C7,1,2,_,1,3,T1S), 0                         , 192, 0  , 312, 170), // #1129
  INST(Vgatherpf0qps    , VexM_VM            , E(660F38,C7,1,2,_,0,2,T1S), 0                         , 193, 0  , 312, 170), // #1130
  INST(Vgatherpf1dpd    , VexM_VM            , E(660F38,C6,2,2,_,1,3,T1S), 0                         , 194, 0  , 310, 170), // #1131
  INST(Vgatherpf1dps    , VexM_VM            , E(660F38,C6,2,2,_,0,2,T1S), 0                         , 195, 0  , 311, 170), // #1132
  INST(Vgatherpf1qpd    , VexM_VM            , E(660F38,C7,2,2,_,1,3,T1S), 0                         , 194, 0  , 312, 170), // #1133
  INST(Vgatherpf1qps    , VexM_VM            , E(660F38,C7,2,2,_,0,2,T1S), 0                         , 195, 0  , 312, 170), // #1134
  INST(Vgatherqpd       , VexRmvRm_VM        , V(660F38,93,_,x,1,_,_,_  ), E(660F38,93,_,x,_,1,3,T1S), 191, 82 , 313, 169), // #1135
  INST(Vgatherqps       , VexRmvRm_VM        , V(660F38,93,_,x,0,_,_,_  ), E(660F38,93,_,x,_,0,2,T1S), 30 , 83 , 314, 169), // #1136
  INST(Vgetexppd        , VexRm_Lx           , E(660F38,42,_,x,_,1,4,FV ), 0                         , 114, 0  , 269, 152), // #1137
  INST(Vgetexpph        , VexRm_Lx           , E(66MAP6,42,_,_,_,0,4,FV ), 0                         , 185, 0  , 271, 146), // #1138
  INST(Vgetexpps        , VexRm_Lx           , E(660F38,42,_,x,_,0,4,FV ), 0                         , 115, 0  , 274, 152), // #1139
  INST(Vgetexpsd        , VexRvm             , E(660F38,43,_,I,_,1,3,T1S), 0                         , 129, 0  , 315, 78 ), // #1140
  INST(Vgetexpsh        , VexRvm             , E(66MAP6,43,_,_,_,0,1,T1S), 0                         , 187, 0  , 260, 148), // #1141
  INST(Vgetexpss        , VexRvm             , E(660F38,43,_,I,_,0,2,T1S), 0                         , 130, 0  , 316, 78 ), // #1142
  INST(Vgetmantpd       , VexRmi_Lx          , E(660F3A,26,_,x,_,1,4,FV ), 0                         , 113, 0  , 317, 152), // #1143
  INST(Vgetmantph       , VexRmi_Lx          , E(000F3A,26,_,_,_,0,4,FV ), 0                         , 124, 0  , 318, 146), // #1144
  INST(Vgetmantps       , VexRmi_Lx          , E(660F3A,26,_,x,_,0,4,FV ), 0                         , 112, 0  , 319, 152), // #1145
  INST(Vgetmantsd       , VexRvmi            , E(660F3A,27,_,I,_,1,3,T1S), 0                         , 182, 0  , 294, 78 ), // #1146
  INST(Vgetmantsh       , VexRvmi            , E(000F3A,27,_,_,_,0,1,T1S), 0                         , 190, 0  , 320, 148), // #1147
  INST(Vgetmantss       , VexRvmi            , E(660F3A,27,_,I,_,0,2,T1S), 0                         , 183, 0  , 295, 78 ), // #1148
  INST(Vgf2p8affineinvqb, VexRvmi_Lx         , V(660F3A,CF,_,x,1,1,4,FV ), 0                         , 196, 0  , 321, 171), // #1149
  INST(Vgf2p8affineqb   , VexRvmi_Lx         , V(660F3A,CE,_,x,1,1,4,FV ), 0                         , 196, 0  , 321, 171), // #1150
  INST(Vgf2p8mulb       , VexRvm_Lx          , V(660F38,CF,_,x,0,0,4,FV ), 0                         , 111, 0  , 322, 171), // #1151
  INST(Vhaddpd          , VexRvm_Lx          , V(660F00,7C,_,x,I,_,_,_  ), 0                         , 71 , 0  , 206, 149), // #1152
  INST(Vhaddps          , VexRvm_Lx          , V(F20F00,7C,_,x,I,_,_,_  ), 0                         , 110, 0  , 206, 149), // #1153
  INST(Vhsubpd          , VexRvm_Lx          , V(660F00,7D,_,x,I,_,_,_  ), 0                         , 71 , 0  , 206, 149), // #1154
  INST(Vhsubps          , VexRvm_Lx          , V(F20F00,7D,_,x,I,_,_,_  ), 0                         , 110, 0  , 206, 149), // #1155
  INST(Vinsertf128      , VexRvmi            , V(660F3A,18,_,1,0,_,_,_  ), 0                         , 174, 0  , 323, 149), // #1156
  INST(Vinsertf32x4     , VexRvmi_Lx         , E(660F3A,18,_,x,_,0,4,T4 ), 0                         , 175, 0  , 324, 152), // #1157
  INST(Vinsertf32x8     , VexRvmi            , E(660F3A,1A,_,2,_,0,5,T8 ), 0                         , 176, 0  , 325, 76 ), // #1158
  INST(Vinsertf64x2     , VexRvmi_Lx         , E(660F3A,18,_,x,_,1,4,T2 ), 0                         , 177, 0  , 324, 155), // #1159
  INST(Vinsertf64x4     , VexRvmi            , E(660F3A,1A,_,2,_,1,5,T4 ), 0                         , 178, 0  , 325, 78 ), // #1160
  INST(Vinserti128      , VexRvmi            , V(660F3A,38,_,1,0,_,_,_  ), 0                         , 174, 0  , 323, 156), // #1161
  INST(Vinserti32x4     , VexRvmi_Lx         , E(660F3A,38,_,x,_,0,4,T4 ), 0                         , 175, 0  , 324, 152), // #1162
  INST(Vinserti32x8     , VexRvmi            , E(660F3A,3A,_,2,_,0,5,T8 ), 0                         , 176, 0  , 325, 76 ), // #1163
  INST(Vinserti64x2     , VexRvmi_Lx         , E(660F3A,38,_,x,_,1,4,T2 ), 0                         , 177, 0  , 324, 155), // #1164
  INST(Vinserti64x4     , VexRvmi            , E(660F3A,3A,_,2,_,1,5,T4 ), 0                         , 178, 0  , 325, 78 ), // #1165
  INST(Vinsertps        , VexRvmi            , V(660F3A,21,_,0,I,0,2,T1S), 0                         , 179, 0  , 326, 147), // #1166
  INST(Vlddqu           , VexRm_Lx           , V(F20F00,F0,_,x,I,_,_,_  ), 0                         , 110, 0  , 241, 149), // #1167
  INST(Vldmxcsr         , VexM               , V(000F00,AE,2,0,I,_,_,_  ), 0                         , 197, 0  , 327, 149), // #1168
  INST(Vmaskmovdqu      , VexRm_ZDI          , V(660F00,F7,_,0,I,_,_,_  ), 0                         , 71 , 0  , 328, 149), // #1169
  INST(Vmaskmovpd       , VexRvmMvr_Lx       , V(660F38,2D,_,x,0,_,_,_  ), V(660F38,2F,_,x,0,_,_,_  ), 30 , 84 , 329, 149), // #1170
  INST(Vmaskmovps       , VexRvmMvr_Lx       , V(660F38,2C,_,x,0,_,_,_  ), V(660F38,2E,_,x,0,_,_,_  ), 30 , 85 , 329, 149), // #1171
  INST(Vmaxpd           , VexRvm_Lx          , V(660F00,5F,_,x,I,1,4,FV ), 0                         , 104, 0  , 330, 145), // #1172
  INST(Vmaxph           , VexRvm_Lx          , E(00MAP5,5F,_,_,_,0,4,FV ), 0                         , 105, 0  , 331, 146), // #1173
  INST(Vmaxps           , VexRvm_Lx          , V(000F00,5F,_,x,I,0,4,FV ), 0                         , 106, 0  , 332, 145), // #1174
  INST(Vmaxsd           , VexRvm             , V(F20F00,5F,_,I,I,1,3,T1S), 0                         , 107, 0  , 333, 147), // #1175
  INST(Vmaxsh           , VexRvm             , E(F3MAP5,5F,_,_,_,0,1,T1S), 0                         , 108, 0  , 260, 148), // #1176
  INST(Vmaxss           , VexRvm             , V(F30F00,5F,_,I,I,0,2,T1S), 0                         , 109, 0  , 264, 147), // #1177
  INST(Vmcall           , X86Op              , O(000F01,C1,_,_,_,_,_,_  ), 0                         , 23 , 0  , 31 , 67 ), // #1178
  INST(Vmclear          , X86M_Only          , O(660F00,C7,6,_,_,_,_,_  ), 0                         , 28 , 0  , 33 , 67 ), // #1179
  INST(Vmfunc           , X86Op              , O(000F01,D4,_,_,_,_,_,_  ), 0                         , 23 , 0  , 31 , 67 ), // #1180
  INST(Vmgexit          , X86Op              , O(F20F01,D9,_,_,_,_,_,_  ), 0                         , 93 , 0  , 31 , 172), // #1181
  INST(Vminpd           , VexRvm_Lx          , V(660F00,5D,_,x,I,1,4,FV ), 0                         , 104, 0  , 330, 145), // #1182
  INST(Vminph           , VexRvm_Lx          , E(00MAP5,5D,_,_,_,0,4,FV ), 0                         , 105, 0  , 331, 146), // #1183
  INST(Vminps           , VexRvm_Lx          , V(000F00,5D,_,x,I,0,4,FV ), 0                         , 106, 0  , 332, 145), // #1184
  INST(Vminsd           , VexRvm             , V(F20F00,5D,_,I,I,1,3,T1S), 0                         , 107, 0  , 333, 147), // #1185
  INST(Vminsh           , VexRvm             , E(F3MAP5,5D,_,_,_,0,1,T1S), 0                         , 108, 0  , 260, 148), // #1186
  INST(Vminss           , VexRvm             , V(F30F00,5D,_,I,I,0,2,T1S), 0                         , 109, 0  , 264, 147), // #1187
  INST(Vmlaunch         , X86Op              , O(000F01,C2,_,_,_,_,_,_  ), 0                         , 23 , 0  , 31 , 67 ), // #1188
  INST(Vmload           , X86Op_xAX          , O(000F01,DA,_,_,_,_,_,_  ), 0                         , 23 , 0  , 334, 23 ), // #1189
  INST(Vmmcall          , X86Op              , O(000F01,D9,_,_,_,_,_,_  ), 0                         , 23 , 0  , 335, 23 ), // #1190
  INST(Vmovapd          , VexRmMr_Lx         , V(660F00,28,_,x,I,1,4,FVM), V(660F00,29,_,x,I,1,4,FVM), 104, 86 , 336, 173), // #1191
  INST(Vmovaps          , VexRmMr_Lx         , V(000F00,28,_,x,I,0,4,FVM), V(000F00,29,_,x,I,0,4,FVM), 106, 87 , 336, 173), // #1192
  INST(Vmovd            , VexMovdMovq        , V(660F00,6E,_,0,0,0,2,T1S), V(660F00,7E,_,0,0,0,2,T1S), 198, 88 , 337, 147), // #1193
  INST(Vmovddup         , VexRm_Lx           , V(F20F00,12,_,x,I,1,3,DUP), 0                         , 199, 0  , 338, 145), // #1194
  INST(Vmovdqa          , VexRmMr_Lx         , V(660F00,6F,_,x,I,_,_,_  ), V(660F00,7F,_,x,I,_,_,_  ), 71 , 89 , 339, 174), // #1195
  INST(Vmovdqa32        , VexRmMr_Lx         , E(660F00,6F,_,x,_,0,4,FVM), E(660F00,7F,_,x,_,0,4,FVM), 200, 90 , 340, 175), // #1196
  INST(Vmovdqa64        , VexRmMr_Lx         , E(660F00,6F,_,x,_,1,4,FVM), E(660F00,7F,_,x,_,1,4,FVM), 136, 91 , 340, 175), // #1197
  INST(Vmovdqu          , VexRmMr_Lx         , V(F30F00,6F,_,x,I,_,_,_  ), V(F30F00,7F,_,x,I,_,_,_  ), 201, 92 , 339, 174), // #1198
  INST(Vmovdqu16        , VexRmMr_Lx         , E(F20F00,6F,_,x,_,1,4,FVM), E(F20F00,7F,_,x,_,1,4,FVM), 167, 93 , 340, 176), // #1199
  INST(Vmovdqu32        , VexRmMr_Lx         , E(F30F00,6F,_,x,_,0,4,FVM), E(F30F00,7F,_,x,_,0,4,FVM), 202, 94 , 340, 175), // #1200
  INST(Vmovdqu64        , VexRmMr_Lx         , E(F30F00,6F,_,x,_,1,4,FVM), E(F30F00,7F,_,x,_,1,4,FVM), 150, 95 , 340, 175), // #1201
  INST(Vmovdqu8         , VexRmMr_Lx         , E(F20F00,6F,_,x,_,0,4,FVM), E(F20F00,7F,_,x,_,0,4,FVM), 165, 96 , 340, 176), // #1202
  INST(Vmovhlps         , VexRvm             , V(000F00,12,_,0,I,0,_,_  ), 0                         , 74 , 0  , 341, 147), // #1203
  INST(Vmovhpd          , VexRvmMr           , V(660F00,16,_,0,I,1,3,T1S), V(660F00,17,_,0,I,1,3,T1S), 126, 97 , 342, 147), // #1204
  INST(Vmovhps          , VexRvmMr           , V(000F00,16,_,0,I,0,3,T2 ), V(000F00,17,_,0,I,0,3,T2 ), 203, 98 , 342, 147), // #1205
  INST(Vmovlhps         , VexRvm             , V(000F00,16,_,0,I,0,_,_  ), 0                         , 74 , 0  , 341, 147), // #1206
  INST(Vmovlpd          , VexRvmMr           , V(660F00,12,_,0,I,1,3,T1S), V(660F00,13,_,0,I,1,3,T1S), 126, 99 , 342, 147), // #1207
  INST(Vmovlps          , VexRvmMr           , V(000F00,12,_,0,I,0,3,T2 ), V(000F00,13,_,0,I,0,3,T2 ), 203, 100, 342, 147), // #1208
  INST(Vmovmskpd        , VexRm_Lx           , V(660F00,50,_,x,I,_,_,_  ), 0                         , 71 , 0  , 343, 149), // #1209
  INST(Vmovmskps        , VexRm_Lx           , V(000F00,50,_,x,I,_,_,_  ), 0                         , 74 , 0  , 343, 149), // #1210
  INST(Vmovntdq         , VexMr_Lx           , V(660F00,E7,_,x,I,0,4,FVM), 0                         , 145, 0  , 344, 145), // #1211
  INST(Vmovntdqa        , VexRm_Lx           , V(660F38,2A,_,x,I,0,4,FVM), 0                         , 111, 0  , 345, 157), // #1212
  INST(Vmovntpd         , VexMr_Lx           , V(660F00,2B,_,x,I,1,4,FVM), 0                         , 104, 0  , 344, 145), // #1213
  INST(Vmovntps         , VexMr_Lx           , V(000F00,2B,_,x,I,0,4,FVM), 0                         , 106, 0  , 344, 145), // #1214
  INST(Vmovq            , VexMovdMovq        , V(660F00,6E,_,0,I,1,3,T1S), V(660F00,7E,_,0,I,1,3,T1S), 126, 101, 346, 177), // #1215
  INST(Vmovsd           , VexMovssMovsd      , V(F20F00,10,_,I,I,1,3,T1S), V(F20F00,11,_,I,I,1,3,T1S), 107, 102, 347, 177), // #1216
  INST(Vmovsh           , VexMovssMovsd      , E(F3MAP5,10,_,I,_,0,1,T1S), E(F3MAP5,11,_,I,_,0,1,T1S), 108, 103, 348, 148), // #1217
  INST(Vmovshdup        , VexRm_Lx           , V(F30F00,16,_,x,I,0,4,FVM), 0                         , 162, 0  , 349, 145), // #1218
  INST(Vmovsldup        , VexRm_Lx           , V(F30F00,12,_,x,I,0,4,FVM), 0                         , 162, 0  , 349, 145), // #1219
  INST(Vmovss           , VexMovssMovsd      , V(F30F00,10,_,I,I,0,2,T1S), V(F30F00,11,_,I,I,0,2,T1S), 109, 104, 350, 177), // #1220
  INST(Vmovupd          , VexRmMr_Lx         , V(660F00,10,_,x,I,1,4,FVM), V(660F00,11,_,x,I,1,4,FVM), 104, 105, 336, 173), // #1221
  INST(Vmovups          , VexRmMr_Lx         , V(000F00,10,_,x,I,0,4,FVM), V(000F00,11,_,x,I,0,4,FVM), 106, 106, 336, 173), // #1222
  INST(Vmovw            , VexMovdMovq        , E(66MAP5,6E,_,0,_,I,1,T1S), E(66MAP5,7E,_,0,_,I,1,T1S), 204, 107, 351, 148), // #1223
  INST(Vmpsadbw         , VexRvmi_Lx         , V(660F3A,42,_,x,I,_,_,_  ), 0                         , 75 , 0  , 219, 178), // #1224
  INST(Vmptrld          , X86M_Only          , O(000F00,C7,6,_,_,_,_,_  ), 0                         , 82 , 0  , 33 , 67 ), // #1225
  INST(Vmptrst          , X86M_Only          , O(000F00,C7,7,_,_,_,_,_  ), 0                         , 24 , 0  , 33 , 67 ), // #1226
  INST(Vmread           , X86Mr_NoSize       , O(000F00,78,_,_,_,_,_,_  ), 0                         , 5  , 0  , 352, 67 ), // #1227
  INST(Vmresume         , X86Op              , O(000F01,C3,_,_,_,_,_,_  ), 0                         , 23 , 0  , 31 , 67 ), // #1228
  INST(Vmrun            , X86Op_xAX          , O(000F01,D8,_,_,_,_,_,_  ), 0                         , 23 , 0  , 334, 23 ), // #1229
  INST(Vmsave           , X86Op_xAX          , O(000F01,DB,_,_,_,_,_,_  ), 0                         , 23 , 0  , 334, 23 ), // #1230
  INST(Vmulpd           , VexRvm_Lx          , V(660F00,59,_,x,I,1,4,FV ), 0                         , 104, 0  , 200, 145), // #1231
  INST(Vmulph           , VexRvm_Lx          , E(00MAP5,59,_,_,_,0,4,FV ), 0                         , 105, 0  , 201, 146), // #1232
  INST(Vmulps           , VexRvm_Lx          , V(000F00,59,_,x,I,0,4,FV ), 0                         , 106, 0  , 202, 145), // #1233
  INST(Vmulsd           , VexRvm             , V(F20F00,59,_,I,I,1,3,T1S), 0                         , 107, 0  , 203, 147), // #1234
  INST(Vmulsh           , VexRvm             , E(F3MAP5,59,_,_,_,0,1,T1S), 0                         , 108, 0  , 204, 148), // #1235
  INST(Vmulss           , VexRvm             , V(F30F00,59,_,I,I,0,2,T1S), 0                         , 109, 0  , 205, 147), // #1236
  INST(Vmwrite          , X86Rm_NoSize       , O(000F00,79,_,_,_,_,_,_  ), 0                         , 5  , 0  , 353, 67 ), // #1237
  INST(Vmxoff           , X86Op              , O(000F01,C4,_,_,_,_,_,_  ), 0                         , 23 , 0  , 31 , 67 ), // #1238
  INST(Vmxon            , X86M_Only          , O(F30F00,C7,6,_,_,_,_,_  ), 0                         , 26 , 0  , 33 , 67 ), // #1239
  INST(Vorpd            , VexRvm_Lx          , V(660F00,56,_,x,I,1,4,FV ), 0                         , 104, 0  , 214, 153), // #1240
  INST(Vorps            , VexRvm_Lx          , V(000F00,56,_,x,I,0,4,FV ), 0                         , 106, 0  , 215, 153), // #1241
  INST(Vp2intersectd    , VexRvm_Lx_2xK      , E(F20F38,68,_,_,_,0,4,FV ), 0                         , 132, 0  , 354, 179), // #1242
  INST(Vp2intersectq    , VexRvm_Lx_2xK      , E(F20F38,68,_,_,_,1,4,FV ), 0                         , 205, 0  , 355, 179), // #1243
  INST(Vp4dpwssd        , VexRm_T1_4X        , E(F20F38,52,_,2,_,0,4,T4X), 0                         , 102, 0  , 198, 180), // #1244
  INST(Vp4dpwssds       , VexRm_T1_4X        , E(F20F38,53,_,2,_,0,4,T4X), 0                         , 102, 0  , 198, 180), // #1245
  INST(Vpabsb           , VexRm_Lx           , V(660F38,1C,_,x,I,_,4,FVM), 0                         , 111, 0  , 349, 181), // #1246
  INST(Vpabsd           , VexRm_Lx           , V(660F38,1E,_,x,I,0,4,FV ), 0                         , 111, 0  , 356, 157), // #1247
  INST(Vpabsq           , VexRm_Lx           , E(660F38,1F,_,x,_,1,4,FV ), 0                         , 114, 0  , 357, 152), // #1248
  INST(Vpabsw           , VexRm_Lx           , V(660F38,1D,_,x,I,_,4,FVM), 0                         , 111, 0  , 349, 181), // #1249
  INST(Vpackssdw        , VexRvm_Lx          , V(660F00,6B,_,x,I,0,4,FV ), 0                         , 145, 0  , 213, 181), // #1250
  INST(Vpacksswb        , VexRvm_Lx          , V(660F00,63,_,x,I,I,4,FVM), 0                         , 145, 0  , 322, 181), // #1251
  INST(Vpackusdw        , VexRvm_Lx          , V(660F38,2B,_,x,I,0,4,FV ), 0                         , 111, 0  , 213, 181), // #1252
  INST(Vpackuswb        , VexRvm_Lx          , V(660F00,67,_,x,I,I,4,FVM), 0                         , 145, 0  , 322, 181), // #1253
  INST(Vpaddb           , VexRvm_Lx          , V(660F00,FC,_,x,I,I,4,FVM), 0                         , 145, 0  , 322, 181), // #1254
  INST(Vpaddd           , VexRvm_Lx          , V(660F00,FE,_,x,I,0,4,FV ), 0                         , 145, 0  , 213, 157), // #1255
  INST(Vpaddq           , VexRvm_Lx          , V(660F00,D4,_,x,I,1,4,FV ), 0                         , 104, 0  , 212, 157), // #1256
  INST(Vpaddsb          , VexRvm_Lx          , V(660F00,EC,_,x,I,I,4,FVM), 0                         , 145, 0  , 322, 181), // #1257
  INST(Vpaddsw          , VexRvm_Lx          , V(660F00,ED,_,x,I,I,4,FVM), 0                         , 145, 0  , 322, 181), // #1258
  INST(Vpaddusb         , VexRvm_Lx          , V(660F00,DC,_,x,I,I,4,FVM), 0                         , 145, 0  , 322, 181), // #1259
  INST(Vpaddusw         , VexRvm_Lx          , V(660F00,DD,_,x,I,I,4,FVM), 0                         , 145, 0  , 322, 181), // #1260
  INST(Vpaddw           , VexRvm_Lx          , V(660F00,FD,_,x,I,I,4,FVM), 0                         , 145, 0  , 322, 181), // #1261
  INST(Vpalignr         , VexRvmi_Lx         , V(660F3A,0F,_,x,I,I,4,FVM), 0                         , 206, 0  , 321, 181), // #1262
  INST(Vpand            , VexRvm_Lx          , V(660F00,DB,_,x,I,_,_,_  ), 0                         , 71 , 0  , 358, 178), // #1263
  INST(Vpandd           , VexRvm_Lx          , E(660F00,DB,_,x,_,0,4,FV ), 0                         , 200, 0  , 359, 152), // #1264
  INST(Vpandn           , VexRvm_Lx          , V(660F00,DF,_,x,I,_,_,_  ), 0                         , 71 , 0  , 360, 178), // #1265
  INST(Vpandnd          , VexRvm_Lx          , E(660F00,DF,_,x,_,0,4,FV ), 0                         , 200, 0  , 361, 152), // #1266
  INST(Vpandnq          , VexRvm_Lx          , E(660F00,DF,_,x,_,1,4,FV ), 0                         , 136, 0  , 362, 152), // #1267
  INST(Vpandq           , VexRvm_Lx          , E(660F00,DB,_,x,_,1,4,FV ), 0                         , 136, 0  , 363, 152), // #1268
  INST(Vpavgb           , VexRvm_Lx          , V(660F00,E0,_,x,I,I,4,FVM), 0                         , 145, 0  , 322, 181), // #1269
  INST(Vpavgw           , VexRvm_Lx          , V(660F00,E3,_,x,I,I,4,FVM), 0                         , 145, 0  , 322, 181), // #1270
  INST(Vpblendd         , VexRvmi_Lx         , V(660F3A,02,_,x,0,_,_,_  ), 0                         , 75 , 0  , 219, 156), // #1271
  INST(Vpblendmb        , VexRvm_Lx          , E(660F38,66,_,x,_,0,4,FVM), 0                         , 115, 0  , 364, 163), // #1272
  INST(Vpblendmd        , VexRvm_Lx          , E(660F38,64,_,x,_,0,4,FV ), 0                         , 115, 0  , 218, 152), // #1273
  INST(Vpblendmq        , VexRvm_Lx          , E(660F38,64,_,x,_,1,4,FV ), 0                         , 114, 0  , 217, 152), // #1274
  INST(Vpblendmw        , VexRvm_Lx          , E(660F38,66,_,x,_,1,4,FVM), 0                         , 114, 0  , 364, 163), // #1275
  INST(Vpblendvb        , VexRvmr_Lx         , V(660F3A,4C,_,x,0,_,_,_  ), 0                         , 75 , 0  , 220, 178), // #1276
  INST(Vpblendw         , VexRvmi_Lx         , V(660F3A,0E,_,x,I,_,_,_  ), 0                         , 75 , 0  , 219, 178), // #1277
  INST(Vpbroadcastb     , VexRm_Lx_Bcst      , V(660F38,78,_,x,0,0,0,T1S), E(660F38,7A,_,x,0,0,0,T1S), 30 , 108, 365, 182), // #1278
  INST(Vpbroadcastd     , VexRm_Lx_Bcst      , V(660F38,58,_,x,0,0,2,T1S), E(660F38,7C,_,x,0,0,0,T1S), 123, 109, 366, 169), // #1279
  INST(Vpbroadcastmb2q  , VexRm_Lx           , E(F30F38,2A,_,x,_,1,_,_  ), 0                         , 207, 0  , 367, 183), // #1280
  INST(Vpbroadcastmw2d  , VexRm_Lx           , E(F30F38,3A,_,x,_,0,_,_  ), 0                         , 208, 0  , 367, 183), // #1281
  INST(Vpbroadcastq     , VexRm_Lx_Bcst      , V(660F38,59,_,x,0,1,3,T1S), E(660F38,7C,_,x,0,1,0,T1S), 122, 110, 368, 169), // #1282
  INST(Vpbroadcastw     , VexRm_Lx_Bcst      , V(660F38,79,_,x,0,0,1,T1S), E(660F38,7B,_,x,0,0,0,T1S), 209, 111, 369, 182), // #1283
  INST(Vpclmulqdq       , VexRvmi_Lx         , V(660F3A,44,_,x,I,_,4,FVM), 0                         , 206, 0  , 370, 184), // #1284
  INST(Vpcmov           , VexRvrmRvmr_Lx     , V(XOP_M8,A2,_,x,x,_,_,_  ), 0                         , 210, 0  , 296, 168), // #1285
  INST(Vpcmpb           , VexRvmi_Lx         , E(660F3A,3F,_,x,_,0,4,FVM), 0                         , 112, 0  , 371, 163), // #1286
  INST(Vpcmpd           , VexRvmi_Lx         , E(660F3A,1F,_,x,_,0,4,FV ), 0                         , 112, 0  , 372, 152), // #1287
  INST(Vpcmpeqb         , VexRvm_Lx_KEvex    , V(660F00,74,_,x,I,I,4,FV ), 0                         , 145, 0  , 373, 181), // #1288
  INST(Vpcmpeqd         , VexRvm_Lx_KEvex    , V(660F00,76,_,x,I,0,4,FVM), 0                         , 145, 0  , 374, 157), // #1289
  INST(Vpcmpeqq         , VexRvm_Lx_KEvex    , V(660F38,29,_,x,I,1,4,FVM), 0                         , 211, 0  , 375, 157), // #1290
  INST(Vpcmpeqw         , VexRvm_Lx_KEvex    , V(660F00,75,_,x,I,I,4,FV ), 0                         , 145, 0  , 373, 181), // #1291
  INST(Vpcmpestri       , VexRmi             , V(660F3A,61,_,0,I,_,_,_  ), 0                         , 75 , 0  , 376, 185), // #1292
  INST(Vpcmpestrm       , VexRmi             , V(660F3A,60,_,0,I,_,_,_  ), 0                         , 75 , 0  , 377, 185), // #1293
  INST(Vpcmpgtb         , VexRvm_Lx_KEvex    , V(660F00,64,_,x,I,I,4,FV ), 0                         , 145, 0  , 373, 181), // #1294
  INST(Vpcmpgtd         , VexRvm_Lx_KEvex    , V(660F00,66,_,x,I,0,4,FVM), 0                         , 145, 0  , 374, 157), // #1295
  INST(Vpcmpgtq         , VexRvm_Lx_KEvex    , V(660F38,37,_,x,I,1,4,FVM), 0                         , 211, 0  , 375, 157), // #1296
  INST(Vpcmpgtw         , VexRvm_Lx_KEvex    , V(660F00,65,_,x,I,I,4,FV ), 0                         , 145, 0  , 373, 181), // #1297
  INST(Vpcmpistri       , VexRmi             , V(660F3A,63,_,0,I,_,_,_  ), 0                         , 75 , 0  , 378, 185), // #1298
  INST(Vpcmpistrm       , VexRmi             , V(660F3A,62,_,0,I,_,_,_  ), 0                         , 75 , 0  , 379, 185), // #1299
  INST(Vpcmpq           , VexRvmi_Lx         , E(660F3A,1F,_,x,_,1,4,FV ), 0                         , 113, 0  , 380, 152), // #1300
  INST(Vpcmpub          , VexRvmi_Lx         , E(660F3A,3E,_,x,_,0,4,FVM), 0                         , 112, 0  , 371, 163), // #1301
  INST(Vpcmpud          , VexRvmi_Lx         , E(660F3A,1E,_,x,_,0,4,FV ), 0                         , 112, 0  , 372, 152), // #1302
  INST(Vpcmpuq          , VexRvmi_Lx         , E(660F3A,1E,_,x,_,1,4,FV ), 0                         , 113, 0  , 380, 152), // #1303
  INST(Vpcmpuw          , VexRvmi_Lx         , E(660F3A,3E,_,x,_,1,4,FVM), 0                         , 113, 0  , 380, 163), // #1304
  INST(Vpcmpw           , VexRvmi_Lx         , E(660F3A,3F,_,x,_,1,4,FVM), 0                         , 113, 0  , 380, 163), // #1305
  INST(Vpcomb           , VexRvmi            , V(XOP_M8,CC,_,0,0,_,_,_  ), 0                         , 210, 0  , 283, 168), // #1306
  INST(Vpcomd           , VexRvmi            , V(XOP_M8,CE,_,0,0,_,_,_  ), 0                         , 210, 0  , 283, 168), // #1307
  INST(Vpcompressb      , VexMr_Lx           , E(660F38,63,_,x,_,0,0,T1S), 0                         , 212, 0  , 237, 186), // #1308
  INST(Vpcompressd      , VexMr_Lx           , E(660F38,8B,_,x,_,0,2,T1S), 0                         , 130, 0  , 237, 152), // #1309
  INST(Vpcompressq      , VexMr_Lx           , E(660F38,8B,_,x,_,1,3,T1S), 0                         , 129, 0  , 237, 152), // #1310
  INST(Vpcompressw      , VexMr_Lx           , E(660F38,63,_,x,_,1,1,T1S), 0                         , 213, 0  , 237, 186), // #1311
  INST(Vpcomq           , VexRvmi            , V(XOP_M8,CF,_,0,0,_,_,_  ), 0                         , 210, 0  , 283, 168), // #1312
  INST(Vpcomub          , VexRvmi            , V(XOP_M8,EC,_,0,0,_,_,_  ), 0                         , 210, 0  , 283, 168), // #1313
  INST(Vpcomud          , VexRvmi            , V(XOP_M8,EE,_,0,0,_,_,_  ), 0                         , 210, 0  , 283, 168), // #1314
  INST(Vpcomuq          , VexRvmi            , V(XOP_M8,EF,_,0,0,_,_,_  ), 0                         , 210, 0  , 283, 168), // #1315
  INST(Vpcomuw          , VexRvmi            , V(XOP_M8,ED,_,0,0,_,_,_  ), 0                         , 210, 0  , 283, 168), // #1316
  INST(Vpcomw           , VexRvmi            , V(XOP_M8,CD,_,0,0,_,_,_  ), 0                         , 210, 0  , 283, 168), // #1317
  INST(Vpconflictd      , VexRm_Lx           , E(660F38,C4,_,x,_,0,4,FV ), 0                         , 115, 0  , 381, 183), // #1318
  INST(Vpconflictq      , VexRm_Lx           , E(660F38,C4,_,x,_,1,4,FV ), 0                         , 114, 0  , 381, 183), // #1319
  INST(Vpdpbssd         , VexRvm_Lx          , V(F20F38,50,_,x,0,_,_,_  ), 0                         , 85 , 0  , 206, 187), // #1320
  INST(Vpdpbssds        , VexRvm_Lx          , V(F20F38,51,_,x,0,_,_,_  ), 0                         , 85 , 0  , 206, 187), // #1321
  INST(Vpdpbsud         , VexRvm_Lx          , V(F30F38,50,_,x,0,_,_,_  ), 0                         , 89 , 0  , 206, 187), // #1322
  INST(Vpdpbsuds        , VexRvm_Lx          , V(F30F38,51,_,x,0,_,_,_  ), 0                         , 89 , 0  , 206, 187), // #1323
  INST(Vpdpbusd         , VexRvm_Lx          , V(660F38,50,_,x,_,0,4,FV ), 0                         , 111, 0  , 382, 188), // #1324
  INST(Vpdpbusds        , VexRvm_Lx          , V(660F38,51,_,x,_,0,4,FV ), 0                         , 111, 0  , 382, 188), // #1325
  INST(Vpdpbuud         , VexRvm_Lx          , V(000F38,50,_,x,0,_,_,_  ), 0                         , 11 , 0  , 206, 187), // #1326
  INST(Vpdpbuuds        , VexRvm_Lx          , V(000F38,51,_,x,0,_,_,_  ), 0                         , 11 , 0  , 206, 187), // #1327
  INST(Vpdpwssd         , VexRvm_Lx          , V(660F38,52,_,x,_,0,4,FV ), 0                         , 111, 0  , 382, 188), // #1328
  INST(Vpdpwssds        , VexRvm_Lx          , V(660F38,53,_,x,_,0,4,FV ), 0                         , 111, 0  , 382, 188), // #1329
  INST(Vpdpwsud         , VexRvm_Lx          , V(F30F38,D2,_,x,0,_,_,_  ), 0                         , 89 , 0  , 206, 189), // #1330
  INST(Vpdpwsuds        , VexRvm_Lx          , V(F30F38,D3,_,x,0,_,_,_  ), 0                         , 89 , 0  , 206, 189), // #1331
  INST(Vpdpwusd         , VexRvm_Lx          , V(660F38,D2,_,x,0,_,_,_  ), 0                         , 30 , 0  , 206, 189), // #1332
  INST(Vpdpwusds        , VexRvm_Lx          , V(660F38,D3,_,x,0,_,_,_  ), 0                         , 30 , 0  , 206, 189), // #1333
  INST(Vpdpwuud         , VexRvm_Lx          , V(000F38,D2,_,x,0,_,_,_  ), 0                         , 11 , 0  , 206, 189), // #1334
  INST(Vpdpwuuds        , VexRvm_Lx          , V(000F38,D3,_,x,0,_,_,_  ), 0                         , 11 , 0  , 206, 189), // #1335
  INST(Vperm2f128       , VexRvmi            , V(660F3A,06,_,1,0,_,_,_  ), 0                         , 174, 0  , 383, 149), // #1336
  INST(Vperm2i128       , VexRvmi            , V(660F3A,46,_,1,0,_,_,_  ), 0                         , 174, 0  , 383, 156), // #1337
  INST(Vpermb           , VexRvm_Lx          , E(660F38,8D,_,x,_,0,4,FVM), 0                         , 115, 0  , 364, 190), // #1338
  INST(Vpermd           , VexRvm_Lx          , V(660F38,36,_,x,0,0,4,FV ), 0                         , 111, 0  , 384, 169), // #1339
  INST(Vpermi2b         , VexRvm_Lx          , E(660F38,75,_,x,_,0,4,FVM), 0                         , 115, 0  , 364, 190), // #1340
  INST(Vpermi2d         , VexRvm_Lx          , E(660F38,76,_,x,_,0,4,FV ), 0                         , 115, 0  , 218, 152), // #1341
  INST(Vpermi2pd        , VexRvm_Lx          , E(660F38,77,_,x,_,1,4,FV ), 0                         , 114, 0  , 217, 152), // #1342
  INST(Vpermi2ps        , VexRvm_Lx          , E(660F38,77,_,x,_,0,4,FV ), 0                         , 115, 0  , 218, 152), // #1343
  INST(Vpermi2q         , VexRvm_Lx          , E(660F38,76,_,x,_,1,4,FV ), 0                         , 114, 0  , 217, 152), // #1344
  INST(Vpermi2w         , VexRvm_Lx          , E(660F38,75,_,x,_,1,4,FVM), 0                         , 114, 0  , 364, 163), // #1345
  INST(Vpermil2pd       , VexRvrmiRvmri_Lx   , V(660F3A,49,_,x,x,_,_,_  ), 0                         , 75 , 0  , 385, 168), // #1346
  INST(Vpermil2ps       , VexRvrmiRvmri_Lx   , V(660F3A,48,_,x,x,_,_,_  ), 0                         , 75 , 0  , 385, 168), // #1347
  INST(Vpermilpd        , VexRvmRmi_Lx       , V(660F38,0D,_,x,0,1,4,FV ), V(660F3A,05,_,x,0,1,4,FV ), 211, 112, 386, 145), // #1348
  INST(Vpermilps        , VexRvmRmi_Lx       , V(660F38,0C,_,x,0,0,4,FV ), V(660F3A,04,_,x,0,0,4,FV ), 111, 113, 387, 145), // #1349
  INST(Vpermpd          , VexRvmRmi_Lx       , E(660F38,16,_,x,1,1,4,FV ), V(660F3A,01,_,x,1,1,4,FV ), 214, 114, 388, 169), // #1350
  INST(Vpermps          , VexRvm_Lx          , V(660F38,16,_,x,0,0,4,FV ), 0                         , 111, 0  , 384, 169), // #1351
  INST(Vpermq           , VexRvmRmi_Lx       , E(660F38,36,_,x,_,1,4,FV ), V(660F3A,00,_,x,1,1,4,FV ), 114, 115, 388, 169), // #1352
  INST(Vpermt2b         , VexRvm_Lx          , E(660F38,7D,_,x,_,0,4,FVM), 0                         , 115, 0  , 364, 190), // #1353
  INST(Vpermt2d         , VexRvm_Lx          , E(660F38,7E,_,x,_,0,4,FV ), 0                         , 115, 0  , 218, 152), // #1354
  INST(Vpermt2pd        , VexRvm_Lx          , E(660F38,7F,_,x,_,1,4,FV ), 0                         , 114, 0  , 217, 152), // #1355
  INST(Vpermt2ps        , VexRvm_Lx          , E(660F38,7F,_,x,_,0,4,FV ), 0                         , 115, 0  , 218, 152), // #1356
  INST(Vpermt2q         , VexRvm_Lx          , E(660F38,7E,_,x,_,1,4,FV ), 0                         , 114, 0  , 217, 152), // #1357
  INST(Vpermt2w         , VexRvm_Lx          , E(660F38,7D,_,x,_,1,4,FVM), 0                         , 114, 0  , 364, 163), // #1358
  INST(Vpermw           , VexRvm_Lx          , E(660F38,8D,_,x,_,1,4,FVM), 0                         , 114, 0  , 364, 163), // #1359
  INST(Vpexpandb        , VexRm_Lx           , E(660F38,62,_,x,_,0,0,T1S), 0                         , 212, 0  , 286, 186), // #1360
  INST(Vpexpandd        , VexRm_Lx           , E(660F38,89,_,x,_,0,2,T1S), 0                         , 130, 0  , 286, 152), // #1361
  INST(Vpexpandq        , VexRm_Lx           , E(660F38,89,_,x,_,1,3,T1S), 0                         , 129, 0  , 286, 152), // #1362
  INST(Vpexpandw        , VexRm_Lx           , E(660F38,62,_,x,_,1,1,T1S), 0                         , 213, 0  , 286, 186), // #1363
  INST(Vpextrb          , VexMri             , V(660F3A,14,_,0,0,I,0,T1S), 0                         , 75 , 0  , 389, 191), // #1364
  INST(Vpextrd          , VexMri             , V(660F3A,16,_,0,0,0,2,T1S), 0                         , 179, 0  , 290, 192), // #1365
  INST(Vpextrq          , VexMri             , V(660F3A,16,_,0,1,1,3,T1S), 0                         , 215, 0  , 390, 192), // #1366
  INST(Vpextrw          , VexMri_Vpextrw     , V(660F3A,15,_,0,0,I,1,T1S), 0                         , 216, 0  , 391, 191), // #1367
  INST(Vpgatherdd       , VexRmvRm_VM        , V(660F38,90,_,x,0,_,_,_  ), E(660F38,90,_,x,_,0,2,T1S), 30 , 116, 309, 169), // #1368
  INST(Vpgatherdq       , VexRmvRm_VM        , V(660F38,90,_,x,1,_,_,_  ), E(660F38,90,_,x,_,1,3,T1S), 191, 117, 308, 169), // #1369
  INST(Vpgatherqd       , VexRmvRm_VM        , V(660F38,91,_,x,0,_,_,_  ), E(660F38,91,_,x,_,0,2,T1S), 30 , 118, 314, 169), // #1370
  INST(Vpgatherqq       , VexRmvRm_VM        , V(660F38,91,_,x,1,_,_,_  ), E(660F38,91,_,x,_,1,3,T1S), 191, 119, 313, 169), // #1371
  INST(Vphaddbd         , VexRm              , V(XOP_M9,C2,_,0,0,_,_,_  ), 0                         , 81 , 0  , 208, 168), // #1372
  INST(Vphaddbq         , VexRm              , V(XOP_M9,C3,_,0,0,_,_,_  ), 0                         , 81 , 0  , 208, 168), // #1373
  INST(Vphaddbw         , VexRm              , V(XOP_M9,C1,_,0,0,_,_,_  ), 0                         , 81 , 0  , 208, 168), // #1374
  INST(Vphaddd          , VexRvm_Lx          , V(660F38,02,_,x,I,_,_,_  ), 0                         , 30 , 0  , 206, 178), // #1375
  INST(Vphadddq         , VexRm              , V(XOP_M9,CB,_,0,0,_,_,_  ), 0                         , 81 , 0  , 208, 168), // #1376
  INST(Vphaddsw         , VexRvm_Lx          , V(660F38,03,_,x,I,_,_,_  ), 0                         , 30 , 0  , 206, 178), // #1377
  INST(Vphaddubd        , VexRm              , V(XOP_M9,D2,_,0,0,_,_,_  ), 0                         , 81 , 0  , 208, 168), // #1378
  INST(Vphaddubq        , VexRm              , V(XOP_M9,D3,_,0,0,_,_,_  ), 0                         , 81 , 0  , 208, 168), // #1379
  INST(Vphaddubw        , VexRm              , V(XOP_M9,D1,_,0,0,_,_,_  ), 0                         , 81 , 0  , 208, 168), // #1380
  INST(Vphaddudq        , VexRm              , V(XOP_M9,DB,_,0,0,_,_,_  ), 0                         , 81 , 0  , 208, 168), // #1381
  INST(Vphadduwd        , VexRm              , V(XOP_M9,D6,_,0,0,_,_,_  ), 0                         , 81 , 0  , 208, 168), // #1382
  INST(Vphadduwq        , VexRm              , V(XOP_M9,D7,_,0,0,_,_,_  ), 0                         , 81 , 0  , 208, 168), // #1383
  INST(Vphaddw          , VexRvm_Lx          , V(660F38,01,_,x,I,_,_,_  ), 0                         , 30 , 0  , 206, 178), // #1384
  INST(Vphaddwd         , VexRm              , V(XOP_M9,C6,_,0,0,_,_,_  ), 0                         , 81 , 0  , 208, 168), // #1385
  INST(Vphaddwq         , VexRm              , V(XOP_M9,C7,_,0,0,_,_,_  ), 0                         , 81 , 0  , 208, 168), // #1386
  INST(Vphminposuw      , VexRm              , V(660F38,41,_,0,I,_,_,_  ), 0                         , 30 , 0  , 208, 149), // #1387
  INST(Vphsubbw         , VexRm              , V(XOP_M9,E1,_,0,0,_,_,_  ), 0                         , 81 , 0  , 208, 168), // #1388
  INST(Vphsubd          , VexRvm_Lx          , V(660F38,06,_,x,I,_,_,_  ), 0                         , 30 , 0  , 206, 178), // #1389
  INST(Vphsubdq         , VexRm              , V(XOP_M9,E3,_,0,0,_,_,_  ), 0                         , 81 , 0  , 208, 168), // #1390
  INST(Vphsubsw         , VexRvm_Lx          , V(660F38,07,_,x,I,_,_,_  ), 0                         , 30 , 0  , 206, 178), // #1391
  INST(Vphsubw          , VexRvm_Lx          , V(660F38,05,_,x,I,_,_,_  ), 0                         , 30 , 0  , 206, 178), // #1392
  INST(Vphsubwd         , VexRm              , V(XOP_M9,E2,_,0,0,_,_,_  ), 0                         , 81 , 0  , 208, 168), // #1393
  INST(Vpinsrb          , VexRvmi            , V(660F3A,20,_,0,0,I,0,T1S), 0                         , 75 , 0  , 392, 191), // #1394
  INST(Vpinsrd          , VexRvmi            , V(660F3A,22,_,0,0,0,2,T1S), 0                         , 179, 0  , 393, 192), // #1395
  INST(Vpinsrq          , VexRvmi            , V(660F3A,22,_,0,1,1,3,T1S), 0                         , 215, 0  , 394, 192), // #1396
  INST(Vpinsrw          , VexRvmi            , V(660F00,C4,_,0,0,I,1,T1S), 0                         , 217, 0  , 395, 191), // #1397
  INST(Vplzcntd         , VexRm_Lx           , E(660F38,44,_,x,_,0,4,FV ), 0                         , 115, 0  , 381, 183), // #1398
  INST(Vplzcntq         , VexRm_Lx           , E(660F38,44,_,x,_,1,4,FV ), 0                         , 114, 0  , 357, 183), // #1399
  INST(Vpmacsdd         , VexRvmr            , V(XOP_M8,9E,_,0,0,_,_,_  ), 0                         , 210, 0  , 396, 168), // #1400
  INST(Vpmacsdqh        , VexRvmr            , V(XOP_M8,9F,_,0,0,_,_,_  ), 0                         , 210, 0  , 396, 168), // #1401
  INST(Vpmacsdql        , VexRvmr            , V(XOP_M8,97,_,0,0,_,_,_  ), 0                         , 210, 0  , 396, 168), // #1402
  INST(Vpmacssdd        , VexRvmr            , V(XOP_M8,8E,_,0,0,_,_,_  ), 0                         , 210, 0  , 396, 168), // #1403
  INST(Vpmacssdqh       , VexRvmr            , V(XOP_M8,8F,_,0,0,_,_,_  ), 0                         , 210, 0  , 396, 168), // #1404
  INST(Vpmacssdql       , VexRvmr            , V(XOP_M8,87,_,0,0,_,_,_  ), 0                         , 210, 0  , 396, 168), // #1405
  INST(Vpmacsswd        , VexRvmr            , V(XOP_M8,86,_,0,0,_,_,_  ), 0                         , 210, 0  , 396, 168), // #1406
  INST(Vpmacssww        , VexRvmr            , V(XOP_M8,85,_,0,0,_,_,_  ), 0                         , 210, 0  , 396, 168), // #1407
  INST(Vpmacswd         , VexRvmr            , V(XOP_M8,96,_,0,0,_,_,_  ), 0                         , 210, 0  , 396, 168), // #1408
  INST(Vpmacsww         , VexRvmr            , V(XOP_M8,95,_,0,0,_,_,_  ), 0                         , 210, 0  , 396, 168), // #1409
  INST(Vpmadcsswd       , VexRvmr            , V(XOP_M8,A6,_,0,0,_,_,_  ), 0                         , 210, 0  , 396, 168), // #1410
  INST(Vpmadcswd        , VexRvmr            , V(XOP_M8,B6,_,0,0,_,_,_  ), 0                         , 210, 0  , 396, 168), // #1411
  INST(Vpmadd52huq      , VexRvm_Lx          , V(660F38,B5,_,x,1,1,4,FV ), 0                         , 184, 0  , 397, 193), // #1412
  INST(Vpmadd52luq      , VexRvm_Lx          , V(660F38,B4,_,x,1,1,4,FV ), 0                         , 184, 0  , 397, 193), // #1413
  INST(Vpmaddubsw       , VexRvm_Lx          , V(660F38,04,_,x,I,I,4,FVM), 0                         , 111, 0  , 322, 181), // #1414
  INST(Vpmaddwd         , VexRvm_Lx          , V(660F00,F5,_,x,I,I,4,FVM), 0                         , 145, 0  , 322, 181), // #1415
  INST(Vpmaskmovd       , VexRvmMvr_Lx       , V(660F38,8C,_,x,0,_,_,_  ), V(660F38,8E,_,x,0,_,_,_  ), 30 , 120, 329, 156), // #1416
  INST(Vpmaskmovq       , VexRvmMvr_Lx       , V(660F38,8C,_,x,1,_,_,_  ), V(660F38,8E,_,x,1,_,_,_  ), 191, 121, 329, 156), // #1417
  INST(Vpmaxsb          , VexRvm_Lx          , V(660F38,3C,_,x,I,I,4,FVM), 0                         , 111, 0  , 398, 181), // #1418
  INST(Vpmaxsd          , VexRvm_Lx          , V(660F38,3D,_,x,I,0,4,FV ), 0                         , 111, 0  , 215, 157), // #1419
  INST(Vpmaxsq          , VexRvm_Lx          , E(660F38,3D,_,x,_,1,4,FV ), 0                         , 114, 0  , 217, 152), // #1420
  INST(Vpmaxsw          , VexRvm_Lx          , V(660F00,EE,_,x,I,I,4,FVM), 0                         , 145, 0  , 398, 181), // #1421
  INST(Vpmaxub          , VexRvm_Lx          , V(660F00,DE,_,x,I,I,4,FVM), 0                         , 145, 0  , 398, 181), // #1422
  INST(Vpmaxud          , VexRvm_Lx          , V(660F38,3F,_,x,I,0,4,FV ), 0                         , 111, 0  , 215, 157), // #1423
  INST(Vpmaxuq          , VexRvm_Lx          , E(660F38,3F,_,x,_,1,4,FV ), 0                         , 114, 0  , 217, 152), // #1424
  INST(Vpmaxuw          , VexRvm_Lx          , V(660F38,3E,_,x,I,I,4,FVM), 0                         , 111, 0  , 398, 181), // #1425
  INST(Vpminsb          , VexRvm_Lx          , V(660F38,38,_,x,I,I,4,FVM), 0                         , 111, 0  , 398, 181), // #1426
  INST(Vpminsd          , VexRvm_Lx          , V(660F38,39,_,x,I,0,4,FV ), 0                         , 111, 0  , 215, 157), // #1427
  INST(Vpminsq          , VexRvm_Lx          , E(660F38,39,_,x,_,1,4,FV ), 0                         , 114, 0  , 217, 152), // #1428
  INST(Vpminsw          , VexRvm_Lx          , V(660F00,EA,_,x,I,I,4,FVM), 0                         , 145, 0  , 398, 181), // #1429
  INST(Vpminub          , VexRvm_Lx          , V(660F00,DA,_,x,I,_,4,FVM), 0                         , 145, 0  , 398, 181), // #1430
  INST(Vpminud          , VexRvm_Lx          , V(660F38,3B,_,x,I,0,4,FV ), 0                         , 111, 0  , 215, 157), // #1431
  INST(Vpminuq          , VexRvm_Lx          , E(660F38,3B,_,x,_,1,4,FV ), 0                         , 114, 0  , 217, 152), // #1432
  INST(Vpminuw          , VexRvm_Lx          , V(660F38,3A,_,x,I,_,4,FVM), 0                         , 111, 0  , 398, 181), // #1433
  INST(Vpmovb2m         , VexRm_Lx           , E(F30F38,29,_,x,_,0,_,_  ), 0                         , 208, 0  , 399, 163), // #1434
  INST(Vpmovd2m         , VexRm_Lx           , E(F30F38,39,_,x,_,0,_,_  ), 0                         , 208, 0  , 399, 155), // #1435
  INST(Vpmovdb          , VexMr_Lx           , E(F30F38,31,_,x,_,0,2,QVM), 0                         , 218, 0  , 400, 152), // #1436
  INST(Vpmovdw          , VexMr_Lx           , E(F30F38,33,_,x,_,0,3,HVM), 0                         , 219, 0  , 401, 152), // #1437
  INST(Vpmovm2b         , VexRm_Lx           , E(F30F38,28,_,x,_,0,_,_  ), 0                         , 208, 0  , 367, 163), // #1438
  INST(Vpmovm2d         , VexRm_Lx           , E(F30F38,38,_,x,_,0,_,_  ), 0                         , 208, 0  , 367, 155), // #1439
  INST(Vpmovm2q         , VexRm_Lx           , E(F30F38,38,_,x,_,1,_,_  ), 0                         , 207, 0  , 367, 155), // #1440
  INST(Vpmovm2w         , VexRm_Lx           , E(F30F38,28,_,x,_,1,_,_  ), 0                         , 207, 0  , 367, 163), // #1441
  INST(Vpmovmskb        , VexRm_Lx           , V(660F00,D7,_,x,I,_,_,_  ), 0                         , 71 , 0  , 343, 178), // #1442
  INST(Vpmovq2m         , VexRm_Lx           , E(F30F38,39,_,x,_,1,_,_  ), 0                         , 207, 0  , 399, 155), // #1443
  INST(Vpmovqb          , VexMr_Lx           , E(F30F38,32,_,x,_,0,1,OVM), 0                         , 220, 0  , 402, 152), // #1444
  INST(Vpmovqd          , VexMr_Lx           , E(F30F38,35,_,x,_,0,3,HVM), 0                         , 219, 0  , 401, 152), // #1445
  INST(Vpmovqw          , VexMr_Lx           , E(F30F38,34,_,x,_,0,2,QVM), 0                         , 218, 0  , 400, 152), // #1446
  INST(Vpmovsdb         , VexMr_Lx           , E(F30F38,21,_,x,_,0,2,QVM), 0                         , 218, 0  , 400, 152), // #1447
  INST(Vpmovsdw         , VexMr_Lx           , E(F30F38,23,_,x,_,0,3,HVM), 0                         , 219, 0  , 401, 152), // #1448
  INST(Vpmovsqb         , VexMr_Lx           , E(F30F38,22,_,x,_,0,1,OVM), 0                         , 220, 0  , 402, 152), // #1449
  INST(Vpmovsqd         , VexMr_Lx           , E(F30F38,25,_,x,_,0,3,HVM), 0                         , 219, 0  , 401, 152), // #1450
  INST(Vpmovsqw         , VexMr_Lx           , E(F30F38,24,_,x,_,0,2,QVM), 0                         , 218, 0  , 400, 152), // #1451
  INST(Vpmovswb         , VexMr_Lx           , E(F30F38,20,_,x,_,0,3,HVM), 0                         , 219, 0  , 401, 163), // #1452
  INST(Vpmovsxbd        , VexRm_Lx           , V(660F38,21,_,x,I,I,2,QVM), 0                         , 221, 0  , 403, 157), // #1453
  INST(Vpmovsxbq        , VexRm_Lx           , V(660F38,22,_,x,I,I,1,OVM), 0                         , 222, 0  , 404, 157), // #1454
  INST(Vpmovsxbw        , VexRm_Lx           , V(660F38,20,_,x,I,I,3,HVM), 0                         , 140, 0  , 405, 181), // #1455
  INST(Vpmovsxdq        , VexRm_Lx           , V(660F38,25,_,x,I,0,3,HVM), 0                         , 140, 0  , 405, 157), // #1456
  INST(Vpmovsxwd        , VexRm_Lx           , V(660F38,23,_,x,I,I,3,HVM), 0                         , 140, 0  , 405, 157), // #1457
  INST(Vpmovsxwq        , VexRm_Lx           , V(660F38,24,_,x,I,I,2,QVM), 0                         , 221, 0  , 403, 157), // #1458
  INST(Vpmovusdb        , VexMr_Lx           , E(F30F38,11,_,x,_,0,2,QVM), 0                         , 218, 0  , 400, 152), // #1459
  INST(Vpmovusdw        , VexMr_Lx           , E(F30F38,13,_,x,_,0,3,HVM), 0                         , 219, 0  , 401, 152), // #1460
  INST(Vpmovusqb        , VexMr_Lx           , E(F30F38,12,_,x,_,0,1,OVM), 0                         , 220, 0  , 402, 152), // #1461
  INST(Vpmovusqd        , VexMr_Lx           , E(F30F38,15,_,x,_,0,3,HVM), 0                         , 219, 0  , 401, 152), // #1462
  INST(Vpmovusqw        , VexMr_Lx           , E(F30F38,14,_,x,_,0,2,QVM), 0                         , 218, 0  , 400, 152), // #1463
  INST(Vpmovuswb        , VexMr_Lx           , E(F30F38,10,_,x,_,0,3,HVM), 0                         , 219, 0  , 401, 163), // #1464
  INST(Vpmovw2m         , VexRm_Lx           , E(F30F38,29,_,x,_,1,_,_  ), 0                         , 207, 0  , 399, 163), // #1465
  INST(Vpmovwb          , VexMr_Lx           , E(F30F38,30,_,x,_,0,3,HVM), 0                         , 219, 0  , 401, 163), // #1466
  INST(Vpmovzxbd        , VexRm_Lx           , V(660F38,31,_,x,I,I,2,QVM), 0                         , 221, 0  , 403, 157), // #1467
  INST(Vpmovzxbq        , VexRm_Lx           , V(660F38,32,_,x,I,I,1,OVM), 0                         , 222, 0  , 404, 157), // #1468
  INST(Vpmovzxbw        , VexRm_Lx           , V(660F38,30,_,x,I,I,3,HVM), 0                         , 140, 0  , 405, 181), // #1469
  INST(Vpmovzxdq        , VexRm_Lx           , V(660F38,35,_,x,I,0,3,HVM), 0                         , 140, 0  , 405, 157), // #1470
  INST(Vpmovzxwd        , VexRm_Lx           , V(660F38,33,_,x,I,I,3,HVM), 0                         , 140, 0  , 405, 157), // #1471
  INST(Vpmovzxwq        , VexRm_Lx           , V(660F38,34,_,x,I,I,2,QVM), 0                         , 221, 0  , 403, 157), // #1472
  INST(Vpmuldq          , VexRvm_Lx          , V(660F38,28,_,x,I,1,4,FV ), 0                         , 211, 0  , 212, 157), // #1473
  INST(Vpmulhrsw        , VexRvm_Lx          , V(660F38,0B,_,x,I,I,4,FVM), 0                         , 111, 0  , 322, 181), // #1474
  INST(Vpmulhuw         , VexRvm_Lx          , V(660F00,E4,_,x,I,I,4,FVM), 0                         , 145, 0  , 322, 181), // #1475
  INST(Vpmulhw          , VexRvm_Lx          , V(660F00,E5,_,x,I,I,4,FVM), 0                         , 145, 0  , 322, 181), // #1476
  INST(Vpmulld          , VexRvm_Lx          , V(660F38,40,_,x,I,0,4,FV ), 0                         , 111, 0  , 213, 157), // #1477
  INST(Vpmullq          , VexRvm_Lx          , E(660F38,40,_,x,_,1,4,FV ), 0                         , 114, 0  , 217, 155), // #1478
  INST(Vpmullw          , VexRvm_Lx          , V(660F00,D5,_,x,I,I,4,FVM), 0                         , 145, 0  , 322, 181), // #1479
  INST(Vpmultishiftqb   , VexRvm_Lx          , E(660F38,83,_,x,_,1,4,FV ), 0                         , 114, 0  , 217, 190), // #1480
  INST(Vpmuludq         , VexRvm_Lx          , V(660F00,F4,_,x,I,1,4,FV ), 0                         , 104, 0  , 212, 157), // #1481
  INST(Vpopcntb         , VexRm_Lx           , E(660F38,54,_,x,_,0,4,FV ), 0                         , 115, 0  , 286, 194), // #1482
  INST(Vpopcntd         , VexRm_Lx           , E(660F38,55,_,x,_,0,4,FVM), 0                         , 115, 0  , 381, 195), // #1483
  INST(Vpopcntq         , VexRm_Lx           , E(660F38,55,_,x,_,1,4,FVM), 0                         , 114, 0  , 357, 195), // #1484
  INST(Vpopcntw         , VexRm_Lx           , E(660F38,54,_,x,_,1,4,FV ), 0                         , 114, 0  , 286, 194), // #1485
  INST(Vpor             , VexRvm_Lx          , V(660F00,EB,_,x,I,_,_,_  ), 0                         , 71 , 0  , 358, 178), // #1486
  INST(Vpord            , VexRvm_Lx          , E(660F00,EB,_,x,_,0,4,FV ), 0                         , 200, 0  , 359, 152), // #1487
  INST(Vporq            , VexRvm_Lx          , E(660F00,EB,_,x,_,1,4,FV ), 0                         , 136, 0  , 363, 152), // #1488
  INST(Vpperm           , VexRvrmRvmr        , V(XOP_M8,A3,_,0,x,_,_,_  ), 0                         , 210, 0  , 406, 168), // #1489
  INST(Vprold           , VexVmi_Lx          , E(660F00,72,1,x,_,0,4,FV ), 0                         , 223, 0  , 407, 152), // #1490
  INST(Vprolq           , VexVmi_Lx          , E(660F00,72,1,x,_,1,4,FV ), 0                         , 224, 0  , 408, 152), // #1491
  INST(Vprolvd          , VexRvm_Lx          , E(660F38,15,_,x,_,0,4,FV ), 0                         , 115, 0  , 218, 152), // #1492
  INST(Vprolvq          , VexRvm_Lx          , E(660F38,15,_,x,_,1,4,FV ), 0                         , 114, 0  , 217, 152), // #1493
  INST(Vprord           , VexVmi_Lx          , E(660F00,72,0,x,_,0,4,FV ), 0                         , 200, 0  , 407, 152), // #1494
  INST(Vprorq           , VexVmi_Lx          , E(660F00,72,0,x,_,1,4,FV ), 0                         , 136, 0  , 408, 152), // #1495
  INST(Vprorvd          , VexRvm_Lx          , E(660F38,14,_,x,_,0,4,FV ), 0                         , 115, 0  , 218, 152), // #1496
  INST(Vprorvq          , VexRvm_Lx          , E(660F38,14,_,x,_,1,4,FV ), 0                         , 114, 0  , 217, 152), // #1497
  INST(Vprotb           , VexRvmRmvRmi       , V(XOP_M9,90,_,0,x,_,_,_  ), V(XOP_M8,C0,_,0,x,_,_,_  ), 81 , 122, 409, 168), // #1498
  INST(Vprotd           , VexRvmRmvRmi       , V(XOP_M9,92,_,0,x,_,_,_  ), V(XOP_M8,C2,_,0,x,_,_,_  ), 81 , 123, 409, 168), // #1499
  INST(Vprotq           , VexRvmRmvRmi       , V(XOP_M9,93,_,0,x,_,_,_  ), V(XOP_M8,C3,_,0,x,_,_,_  ), 81 , 124, 409, 168), // #1500
  INST(Vprotw           , VexRvmRmvRmi       , V(XOP_M9,91,_,0,x,_,_,_  ), V(XOP_M8,C1,_,0,x,_,_,_  ), 81 , 125, 409, 168), // #1501
  INST(Vpsadbw          , VexRvm_Lx          , V(660F00,F6,_,x,I,I,4,FVM), 0                         , 145, 0  , 207, 181), // #1502
  INST(Vpscatterdd      , VexMr_VM           , E(660F38,A0,_,x,_,0,2,T1S), 0                         , 130, 0  , 410, 152), // #1503
  INST(Vpscatterdq      , VexMr_VM           , E(660F38,A0,_,x,_,1,3,T1S), 0                         , 129, 0  , 411, 152), // #1504
  INST(Vpscatterqd      , VexMr_VM           , E(660F38,A1,_,x,_,0,2,T1S), 0                         , 130, 0  , 412, 152), // #1505
  INST(Vpscatterqq      , VexMr_VM           , E(660F38,A1,_,x,_,1,3,T1S), 0                         , 129, 0  , 413, 152), // #1506
  INST(Vpshab           , VexRvmRmv          , V(XOP_M9,98,_,0,x,_,_,_  ), 0                         , 81 , 0  , 414, 168), // #1507
  INST(Vpshad           , VexRvmRmv          , V(XOP_M9,9A,_,0,x,_,_,_  ), 0                         , 81 , 0  , 414, 168), // #1508
  INST(Vpshaq           , VexRvmRmv          , V(XOP_M9,9B,_,0,x,_,_,_  ), 0                         , 81 , 0  , 414, 168), // #1509
  INST(Vpshaw           , VexRvmRmv          , V(XOP_M9,99,_,0,x,_,_,_  ), 0                         , 81 , 0  , 414, 168), // #1510
  INST(Vpshlb           , VexRvmRmv          , V(XOP_M9,94,_,0,x,_,_,_  ), 0                         , 81 , 0  , 414, 168), // #1511
  INST(Vpshld           , VexRvmRmv          , V(XOP_M9,96,_,0,x,_,_,_  ), 0                         , 81 , 0  , 414, 168), // #1512
  INST(Vpshldd          , VexRvmi_Lx         , E(660F3A,71,_,x,_,0,4,FV ), 0                         , 112, 0  , 210, 186), // #1513
  INST(Vpshldq          , VexRvmi_Lx         , E(660F3A,71,_,x,_,1,4,FV ), 0                         , 113, 0  , 211, 186), // #1514
  INST(Vpshldvd         , VexRvm_Lx          , E(660F38,71,_,x,_,0,4,FV ), 0                         , 115, 0  , 218, 186), // #1515
  INST(Vpshldvq         , VexRvm_Lx          , E(660F38,71,_,x,_,1,4,FV ), 0                         , 114, 0  , 217, 186), // #1516
  INST(Vpshldvw         , VexRvm_Lx          , E(660F38,70,_,x,_,1,4,FVM), 0                         , 114, 0  , 364, 186), // #1517
  INST(Vpshldw          , VexRvmi_Lx         , E(660F3A,70,_,x,_,1,4,FVM), 0                         , 113, 0  , 282, 186), // #1518
  INST(Vpshlq           , VexRvmRmv          , V(XOP_M9,97,_,0,x,_,_,_  ), 0                         , 81 , 0  , 414, 168), // #1519
  INST(Vpshlw           , VexRvmRmv          , V(XOP_M9,95,_,0,x,_,_,_  ), 0                         , 81 , 0  , 414, 168), // #1520
  INST(Vpshrdd          , VexRvmi_Lx         , E(660F3A,73,_,x,_,0,4,FV ), 0                         , 112, 0  , 210, 186), // #1521
  INST(Vpshrdq          , VexRvmi_Lx         , E(660F3A,73,_,x,_,1,4,FV ), 0                         , 113, 0  , 211, 186), // #1522
  INST(Vpshrdvd         , VexRvm_Lx          , E(660F38,73,_,x,_,0,4,FV ), 0                         , 115, 0  , 218, 186), // #1523
  INST(Vpshrdvq         , VexRvm_Lx          , E(660F38,73,_,x,_,1,4,FV ), 0                         , 114, 0  , 217, 186), // #1524
  INST(Vpshrdvw         , VexRvm_Lx          , E(660F38,72,_,x,_,1,4,FVM), 0                         , 114, 0  , 364, 186), // #1525
  INST(Vpshrdw          , VexRvmi_Lx         , E(660F3A,72,_,x,_,1,4,FVM), 0                         , 113, 0  , 282, 186), // #1526
  INST(Vpshufb          , VexRvm_Lx          , V(660F38,00,_,x,I,I,4,FVM), 0                         , 111, 0  , 322, 181), // #1527
  INST(Vpshufbitqmb     , VexRvm_Lx          , E(660F38,8F,_,x,0,0,4,FVM), 0                         , 115, 0  , 415, 194), // #1528
  INST(Vpshufd          , VexRmi_Lx          , V(660F00,70,_,x,I,0,4,FV ), 0                         , 145, 0  , 416, 157), // #1529
  INST(Vpshufhw         , VexRmi_Lx          , V(F30F00,70,_,x,I,I,4,FVM), 0                         , 162, 0  , 417, 181), // #1530
  INST(Vpshuflw         , VexRmi_Lx          , V(F20F00,70,_,x,I,I,4,FVM), 0                         , 225, 0  , 417, 181), // #1531
  INST(Vpsignb          , VexRvm_Lx          , V(660F38,08,_,x,I,_,_,_  ), 0                         , 30 , 0  , 206, 178), // #1532
  INST(Vpsignd          , VexRvm_Lx          , V(660F38,0A,_,x,I,_,_,_  ), 0                         , 30 , 0  , 206, 178), // #1533
  INST(Vpsignw          , VexRvm_Lx          , V(660F38,09,_,x,I,_,_,_  ), 0                         , 30 , 0  , 206, 178), // #1534
  INST(Vpslld           , VexRvmVmi_Lx_MEvex , V(660F00,F2,_,x,I,0,4,128), V(660F00,72,6,x,I,0,4,FV ), 226, 126, 418, 157), // #1535
  INST(Vpslldq          , VexVmi_Lx_MEvex    , V(660F00,73,7,x,I,I,4,FVM), 0                         , 227, 0  , 419, 181), // #1536
  INST(Vpsllq           , VexRvmVmi_Lx_MEvex , V(660F00,F3,_,x,I,1,4,128), V(660F00,73,6,x,I,1,4,FV ), 228, 127, 420, 157), // #1537
  INST(Vpsllvd          , VexRvm_Lx          , V(660F38,47,_,x,0,0,4,FV ), 0                         , 111, 0  , 213, 169), // #1538
  INST(Vpsllvq          , VexRvm_Lx          , V(660F38,47,_,x,1,1,4,FV ), 0                         , 184, 0  , 212, 169), // #1539
  INST(Vpsllvw          , VexRvm_Lx          , E(660F38,12,_,x,_,1,4,FVM), 0                         , 114, 0  , 364, 163), // #1540
  INST(Vpsllw           , VexRvmVmi_Lx_MEvex , V(660F00,F1,_,x,I,I,4,128), V(660F00,71,6,x,I,I,4,FVM), 226, 128, 421, 181), // #1541
  INST(Vpsrad           , VexRvmVmi_Lx_MEvex , V(660F00,E2,_,x,I,0,4,128), V(660F00,72,4,x,I,0,4,FV ), 226, 129, 418, 157), // #1542
  INST(Vpsraq           , VexRvmVmi_Lx_MEvex , E(660F00,E2,_,x,_,1,4,128), E(660F00,72,4,x,_,1,4,FV ), 229, 130, 422, 152), // #1543
  INST(Vpsravd          , VexRvm_Lx          , V(660F38,46,_,x,0,0,4,FV ), 0                         , 111, 0  , 213, 169), // #1544
  INST(Vpsravq          , VexRvm_Lx          , E(660F38,46,_,x,_,1,4,FV ), 0                         , 114, 0  , 217, 152), // #1545
  INST(Vpsravw          , VexRvm_Lx          , E(660F38,11,_,x,_,1,4,FVM), 0                         , 114, 0  , 364, 163), // #1546
  INST(Vpsraw           , VexRvmVmi_Lx_MEvex , V(660F00,E1,_,x,I,I,4,128), V(660F00,71,4,x,I,I,4,FVM), 226, 131, 421, 181), // #1547
  INST(Vpsrld           , VexRvmVmi_Lx_MEvex , V(660F00,D2,_,x,I,0,4,128), V(660F00,72,2,x,I,0,4,FV ), 226, 132, 418, 157), // #1548
  INST(Vpsrldq          , VexVmi_Lx_MEvex    , V(660F00,73,3,x,I,I,4,FVM), 0                         , 230, 0  , 419, 181), // #1549
  INST(Vpsrlq           , VexRvmVmi_Lx_MEvex , V(660F00,D3,_,x,I,1,4,128), V(660F00,73,2,x,I,1,4,FV ), 228, 133, 420, 157), // #1550
  INST(Vpsrlvd          , VexRvm_Lx          , V(660F38,45,_,x,0,0,4,FV ), 0                         , 111, 0  , 213, 169), // #1551
  INST(Vpsrlvq          , VexRvm_Lx          , V(660F38,45,_,x,1,1,4,FV ), 0                         , 184, 0  , 212, 169), // #1552
  INST(Vpsrlvw          , VexRvm_Lx          , E(660F38,10,_,x,_,1,4,FVM), 0                         , 114, 0  , 364, 163), // #1553
  INST(Vpsrlw           , VexRvmVmi_Lx_MEvex , V(660F00,D1,_,x,I,I,4,128), V(660F00,71,2,x,I,I,4,FVM), 226, 134, 421, 181), // #1554
  INST(Vpsubb           , VexRvm_Lx          , V(660F00,F8,_,x,I,I,4,FVM), 0                         , 145, 0  , 423, 181), // #1555
  INST(Vpsubd           , VexRvm_Lx          , V(660F00,FA,_,x,I,0,4,FV ), 0                         , 145, 0  , 424, 157), // #1556
  INST(Vpsubq           , VexRvm_Lx          , V(660F00,FB,_,x,I,1,4,FV ), 0                         , 104, 0  , 425, 157), // #1557
  INST(Vpsubsb          , VexRvm_Lx          , V(660F00,E8,_,x,I,I,4,FVM), 0                         , 145, 0  , 423, 181), // #1558
  INST(Vpsubsw          , VexRvm_Lx          , V(660F00,E9,_,x,I,I,4,FVM), 0                         , 145, 0  , 423, 181), // #1559
  INST(Vpsubusb         , VexRvm_Lx          , V(660F00,D8,_,x,I,I,4,FVM), 0                         , 145, 0  , 423, 181), // #1560
  INST(Vpsubusw         , VexRvm_Lx          , V(660F00,D9,_,x,I,I,4,FVM), 0                         , 145, 0  , 423, 181), // #1561
  INST(Vpsubw           , VexRvm_Lx          , V(660F00,F9,_,x,I,I,4,FVM), 0                         , 145, 0  , 423, 181), // #1562
  INST(Vpternlogd       , VexRvmi_Lx         , E(660F3A,25,_,x,_,0,4,FV ), 0                         , 112, 0  , 210, 152), // #1563
  INST(Vpternlogq       , VexRvmi_Lx         , E(660F3A,25,_,x,_,1,4,FV ), 0                         , 113, 0  , 211, 152), // #1564
  INST(Vptest           , VexRm_Lx           , V(660F38,17,_,x,I,_,_,_  ), 0                         , 30 , 0  , 305, 185), // #1565
  INST(Vptestmb         , VexRvm_Lx          , E(660F38,26,_,x,_,0,4,FVM), 0                         , 115, 0  , 415, 163), // #1566
  INST(Vptestmd         , VexRvm_Lx          , E(660F38,27,_,x,_,0,4,FV ), 0                         , 115, 0  , 426, 152), // #1567
  INST(Vptestmq         , VexRvm_Lx          , E(660F38,27,_,x,_,1,4,FV ), 0                         , 114, 0  , 427, 152), // #1568
  INST(Vptestmw         , VexRvm_Lx          , E(660F38,26,_,x,_,1,4,FVM), 0                         , 114, 0  , 415, 163), // #1569
  INST(Vptestnmb        , VexRvm_Lx          , E(F30F38,26,_,x,_,0,4,FVM), 0                         , 171, 0  , 415, 163), // #1570
  INST(Vptestnmd        , VexRvm_Lx          , E(F30F38,27,_,x,_,0,4,FV ), 0                         , 171, 0  , 426, 152), // #1571
  INST(Vptestnmq        , VexRvm_Lx          , E(F30F38,27,_,x,_,1,4,FV ), 0                         , 231, 0  , 427, 152), // #1572
  INST(Vptestnmw        , VexRvm_Lx          , E(F30F38,26,_,x,_,1,4,FVM), 0                         , 231, 0  , 415, 163), // #1573
  INST(Vpunpckhbw       , VexRvm_Lx          , V(660F00,68,_,x,I,I,4,FVM), 0                         , 145, 0  , 322, 181), // #1574
  INST(Vpunpckhdq       , VexRvm_Lx          , V(660F00,6A,_,x,I,0,4,FV ), 0                         , 145, 0  , 213, 157), // #1575
  INST(Vpunpckhqdq      , VexRvm_Lx          , V(660F00,6D,_,x,I,1,4,FV ), 0                         , 104, 0  , 212, 157), // #1576
  INST(Vpunpckhwd       , VexRvm_Lx          , V(660F00,69,_,x,I,I,4,FVM), 0                         , 145, 0  , 322, 181), // #1577
  INST(Vpunpcklbw       , VexRvm_Lx          , V(660F00,60,_,x,I,I,4,FVM), 0                         , 145, 0  , 322, 181), // #1578
  INST(Vpunpckldq       , VexRvm_Lx          , V(660F00,62,_,x,I,0,4,FV ), 0                         , 145, 0  , 213, 157), // #1579
  INST(Vpunpcklqdq      , VexRvm_Lx          , V(660F00,6C,_,x,I,1,4,FV ), 0                         , 104, 0  , 212, 157), // #1580
  INST(Vpunpcklwd       , VexRvm_Lx          , V(660F00,61,_,x,I,I,4,FVM), 0                         , 145, 0  , 322, 181), // #1581
  INST(Vpxor            , VexRvm_Lx          , V(660F00,EF,_,x,I,_,_,_  ), 0                         , 71 , 0  , 360, 178), // #1582
  INST(Vpxord           , VexRvm_Lx          , E(660F00,EF,_,x,_,0,4,FV ), 0                         , 200, 0  , 361, 152), // #1583
  INST(Vpxorq           , VexRvm_Lx          , E(660F00,EF,_,x,_,1,4,FV ), 0                         , 136, 0  , 362, 152), // #1584
  INST(Vrangepd         , VexRvmi_Lx         , E(660F3A,50,_,x,_,1,4,FV ), 0                         , 113, 0  , 292, 155), // #1585
  INST(Vrangeps         , VexRvmi_Lx         , E(660F3A,50,_,x,_,0,4,FV ), 0                         , 112, 0  , 293, 155), // #1586
  INST(Vrangesd         , VexRvmi            , E(660F3A,51,_,I,_,1,3,T1S), 0                         , 182, 0  , 294, 76 ), // #1587
  INST(Vrangess         , VexRvmi            , E(660F3A,51,_,I,_,0,2,T1S), 0                         , 183, 0  , 295, 76 ), // #1588
  INST(Vrcp14pd         , VexRm_Lx           , E(660F38,4C,_,x,_,1,4,FV ), 0                         , 114, 0  , 357, 152), // #1589
  INST(Vrcp14ps         , VexRm_Lx           , E(660F38,4C,_,x,_,0,4,FV ), 0                         , 115, 0  , 381, 152), // #1590
  INST(Vrcp14sd         , VexRvm             , E(660F38,4D,_,I,_,1,3,T1S), 0                         , 129, 0  , 428, 78 ), // #1591
  INST(Vrcp14ss         , VexRvm             , E(660F38,4D,_,I,_,0,2,T1S), 0                         , 130, 0  , 429, 78 ), // #1592
  INST(Vrcp28pd         , VexRm              , E(660F38,CA,_,2,_,1,4,FV ), 0                         , 172, 0  , 284, 164), // #1593
  INST(Vrcp28ps         , VexRm              , E(660F38,CA,_,2,_,0,4,FV ), 0                         , 173, 0  , 285, 164), // #1594
  INST(Vrcp28sd         , VexRvm             , E(660F38,CB,_,I,_,1,3,T1S), 0                         , 129, 0  , 315, 164), // #1595
  INST(Vrcp28ss         , VexRvm             , E(660F38,CB,_,I,_,0,2,T1S), 0                         , 130, 0  , 316, 164), // #1596
  INST(Vrcpph           , VexRm_Lx           , E(66MAP6,4C,_,_,_,0,4,FV ), 0                         , 185, 0  , 430, 148), // #1597
  INST(Vrcpps           , VexRm_Lx           , V(000F00,53,_,x,I,_,_,_  ), 0                         , 74 , 0  , 305, 149), // #1598
  INST(Vrcpsh           , VexRvm             , E(66MAP6,4D,_,_,_,0,1,T1S), 0                         , 187, 0  , 431, 148), // #1599
  INST(Vrcpss           , VexRvm             , V(F30F00,53,_,I,I,_,_,_  ), 0                         , 201, 0  , 432, 149), // #1600
  INST(Vreducepd        , VexRmi_Lx          , E(660F3A,56,_,x,_,1,4,FV ), 0                         , 113, 0  , 408, 155), // #1601
  INST(Vreduceph        , VexRmi_Lx          , E(000F3A,56,_,_,_,0,4,FV ), 0                         , 124, 0  , 318, 146), // #1602
  INST(Vreduceps        , VexRmi_Lx          , E(660F3A,56,_,x,_,0,4,FV ), 0                         , 112, 0  , 407, 155), // #1603
  INST(Vreducesd        , VexRvmi            , E(660F3A,57,_,I,_,1,3,T1S), 0                         , 182, 0  , 433, 76 ), // #1604
  INST(Vreducesh        , VexRvmi            , E(000F3A,57,_,_,_,0,1,T1S), 0                         , 190, 0  , 320, 148), // #1605
  INST(Vreducess        , VexRvmi            , E(660F3A,57,_,I,_,0,2,T1S), 0                         , 183, 0  , 434, 76 ), // #1606
  INST(Vrndscalepd      , VexRmi_Lx          , E(660F3A,09,_,x,_,1,4,FV ), 0                         , 113, 0  , 317, 152), // #1607
  INST(Vrndscaleph      , VexRmi_Lx          , E(000F3A,08,_,_,_,0,4,FV ), 0                         , 124, 0  , 318, 146), // #1608
  INST(Vrndscaleps      , VexRmi_Lx          , E(660F3A,08,_,x,_,0,4,FV ), 0                         , 112, 0  , 319, 152), // #1609
  INST(Vrndscalesd      , VexRvmi            , E(660F3A,0B,_,I,_,1,3,T1S), 0                         , 182, 0  , 294, 78 ), // #1610
  INST(Vrndscalesh      , VexRvmi            , E(000F3A,0A,_,_,_,0,1,T1S), 0                         , 190, 0  , 320, 148), // #1611
  INST(Vrndscaless      , VexRvmi            , E(660F3A,0A,_,I,_,0,2,T1S), 0                         , 183, 0  , 295, 78 ), // #1612
  INST(Vroundpd         , VexRmi_Lx          , V(660F3A,09,_,x,I,_,_,_  ), 0                         , 75 , 0  , 435, 149), // #1613
  INST(Vroundps         , VexRmi_Lx          , V(660F3A,08,_,x,I,_,_,_  ), 0                         , 75 , 0  , 435, 149), // #1614
  INST(Vroundsd         , VexRvmi            , V(660F3A,0B,_,I,I,_,_,_  ), 0                         , 75 , 0  , 436, 149), // #1615
  INST(Vroundss         , VexRvmi            , V(660F3A,0A,_,I,I,_,_,_  ), 0                         , 75 , 0  , 437, 149), // #1616
  INST(Vrsqrt14pd       , VexRm_Lx           , E(660F38,4E,_,x,_,1,4,FV ), 0                         , 114, 0  , 357, 152), // #1617
  INST(Vrsqrt14ps       , VexRm_Lx           , E(660F38,4E,_,x,_,0,4,FV ), 0                         , 115, 0  , 381, 152), // #1618
  INST(Vrsqrt14sd       , VexRvm             , E(660F38,4F,_,I,_,1,3,T1S), 0                         , 129, 0  , 428, 78 ), // #1619
  INST(Vrsqrt14ss       , VexRvm             , E(660F38,4F,_,I,_,0,2,T1S), 0                         , 130, 0  , 429, 78 ), // #1620
  INST(Vrsqrt28pd       , VexRm              , E(660F38,CC,_,2,_,1,4,FV ), 0                         , 172, 0  , 284, 164), // #1621
  INST(Vrsqrt28ps       , VexRm              , E(660F38,CC,_,2,_,0,4,FV ), 0                         , 173, 0  , 285, 164), // #1622
  INST(Vrsqrt28sd       , VexRvm             , E(660F38,CD,_,I,_,1,3,T1S), 0                         , 129, 0  , 315, 164), // #1623
  INST(Vrsqrt28ss       , VexRvm             , E(660F38,CD,_,I,_,0,2,T1S), 0                         , 130, 0  , 316, 164), // #1624
  INST(Vrsqrtph         , VexRm_Lx           , E(66MAP6,4E,_,_,_,0,4,FV ), 0                         , 185, 0  , 430, 146), // #1625
  INST(Vrsqrtps         , VexRm_Lx           , V(000F00,52,_,x,I,_,_,_  ), 0                         , 74 , 0  , 305, 149), // #1626
  INST(Vrsqrtsh         , VexRvm             , E(66MAP6,4F,_,_,_,0,1,T1S), 0                         , 187, 0  , 431, 148), // #1627
  INST(Vrsqrtss         , VexRvm             , V(F30F00,52,_,I,I,_,_,_  ), 0                         , 201, 0  , 432, 149), // #1628
  INST(Vscalefpd        , VexRvm_Lx          , E(660F38,2C,_,x,_,1,4,FV ), 0                         , 114, 0  , 438, 152), // #1629
  INST(Vscalefph        , VexRvm_Lx          , E(66MAP6,2C,_,_,_,0,4,FV ), 0                         , 185, 0  , 201, 146), // #1630
  INST(Vscalefps        , VexRvm_Lx          , E(660F38,2C,_,x,_,0,4,FV ), 0                         , 115, 0  , 291, 152), // #1631
  INST(Vscalefsd        , VexRvm             , E(660F38,2D,_,I,_,1,3,T1S), 0                         , 129, 0  , 257, 78 ), // #1632
  INST(Vscalefsh        , VexRvm             , E(66MAP6,2D,_,_,_,0,1,T1S), 0                         , 187, 0  , 204, 148), // #1633
  INST(Vscalefss        , VexRvm             , E(660F38,2D,_,I,_,0,2,T1S), 0                         , 130, 0  , 265, 78 ), // #1634
  INST(Vscatterdpd      , VexMr_VM           , E(660F38,A2,_,x,_,1,3,T1S), 0                         , 129, 0  , 411, 152), // #1635
  INST(Vscatterdps      , VexMr_VM           , E(660F38,A2,_,x,_,0,2,T1S), 0                         , 130, 0  , 410, 152), // #1636
  INST(Vscatterpf0dpd   , VexM_VM            , E(660F38,C6,5,2,_,1,3,T1S), 0                         , 232, 0  , 310, 170), // #1637
  INST(Vscatterpf0dps   , VexM_VM            , E(660F38,C6,5,2,_,0,2,T1S), 0                         , 233, 0  , 311, 170), // #1638
  INST(Vscatterpf0qpd   , VexM_VM            , E(660F38,C7,5,2,_,1,3,T1S), 0                         , 232, 0  , 312, 170), // #1639
  INST(Vscatterpf0qps   , VexM_VM            , E(660F38,C7,5,2,_,0,2,T1S), 0                         , 233, 0  , 312, 170), // #1640
  INST(Vscatterpf1dpd   , VexM_VM            , E(660F38,C6,6,2,_,1,3,T1S), 0                         , 234, 0  , 310, 170), // #1641
  INST(Vscatterpf1dps   , VexM_VM            , E(660F38,C6,6,2,_,0,2,T1S), 0                         , 235, 0  , 311, 170), // #1642
  INST(Vscatterpf1qpd   , VexM_VM            , E(660F38,C7,6,2,_,1,3,T1S), 0                         , 234, 0  , 312, 170), // #1643
  INST(Vscatterpf1qps   , VexM_VM            , E(660F38,C7,6,2,_,0,2,T1S), 0                         , 235, 0  , 312, 170), // #1644
  INST(Vscatterqpd      , VexMr_VM           , E(660F38,A3,_,x,_,1,3,T1S), 0                         , 129, 0  , 413, 152), // #1645
  INST(Vscatterqps      , VexMr_VM           , E(660F38,A3,_,x,_,0,2,T1S), 0                         , 130, 0  , 412, 152), // #1646
  INST(Vsha512msg1      , VexRm              , V(F20F38,CC,_,1,0,_,_,_  ), 0                         , 236, 0  , 439, 196), // #1647
  INST(Vsha512msg2      , VexRm              , V(F20F38,CD,_,1,0,_,_,_  ), 0                         , 236, 0  , 440, 196), // #1648
  INST(Vsha512rnds2     , VexRvm             , V(F20F38,CB,_,1,0,_,_,_  ), 0                         , 236, 0  , 441, 196), // #1649
  INST(Vshuff32x4       , VexRvmi_Lx         , E(660F3A,23,_,x,_,0,4,FV ), 0                         , 112, 0  , 442, 152), // #1650
  INST(Vshuff64x2       , VexRvmi_Lx         , E(660F3A,23,_,x,_,1,4,FV ), 0                         , 113, 0  , 443, 152), // #1651
  INST(Vshufi32x4       , VexRvmi_Lx         , E(660F3A,43,_,x,_,0,4,FV ), 0                         , 112, 0  , 442, 152), // #1652
  INST(Vshufi64x2       , VexRvmi_Lx         , E(660F3A,43,_,x,_,1,4,FV ), 0                         , 113, 0  , 443, 152), // #1653
  INST(Vshufpd          , VexRvmi_Lx         , V(660F00,C6,_,x,I,1,4,FV ), 0                         , 104, 0  , 444, 145), // #1654
  INST(Vshufps          , VexRvmi_Lx         , V(000F00,C6,_,x,I,0,4,FV ), 0                         , 106, 0  , 445, 145), // #1655
  INST(Vsm3msg1         , VexRvm             , V(000F38,DA,_,0,0,_,_,_  ), 0                         , 11 , 0  , 446, 197), // #1656
  INST(Vsm3msg2         , VexRvm             , V(660F38,DA,_,0,0,_,_,_  ), 0                         , 30 , 0  , 446, 197), // #1657
  INST(Vsm3rnds2        , VexRvmi            , V(660F3A,DE,_,0,0,_,_,_  ), 0                         , 75 , 0  , 283, 197), // #1658
  INST(Vsm4key4         , VexRvm_Lx          , V(F30F38,DA,_,x,0,_,_,_  ), 0                         , 89 , 0  , 206, 198), // #1659
  INST(Vsm4rnds4        , VexRvm_Lx          , V(F20F38,DA,_,x,0,_,_,_  ), 0                         , 85 , 0  , 206, 198), // #1660
  INST(Vsqrtpd          , VexRm_Lx           , V(660F00,51,_,x,I,1,4,FV ), 0                         , 104, 0  , 447, 145), // #1661
  INST(Vsqrtph          , VexRm_Lx           , E(00MAP5,51,_,_,_,0,4,FV ), 0                         , 105, 0  , 252, 146), // #1662
  INST(Vsqrtps          , VexRm_Lx           , V(000F00,51,_,x,I,0,4,FV ), 0                         , 106, 0  , 240, 145), // #1663
  INST(Vsqrtsd          , VexRvm             , V(F20F00,51,_,I,I,1,3,T1S), 0                         , 107, 0  , 203, 147), // #1664
  INST(Vsqrtsh          , VexRvm             , E(F3MAP5,51,_,_,_,0,1,T1S), 0                         , 108, 0  , 204, 148), // #1665
  INST(Vsqrtss          , VexRvm             , V(F30F00,51,_,I,I,0,2,T1S), 0                         , 109, 0  , 205, 147), // #1666
  INST(Vstmxcsr         , VexM               , V(000F00,AE,3,0,I,_,_,_  ), 0                         , 237, 0  , 327, 149), // #1667
  INST(Vsubpd           , VexRvm_Lx          , V(660F00,5C,_,x,I,1,4,FV ), 0                         , 104, 0  , 200, 145), // #1668
  INST(Vsubph           , VexRvm_Lx          , E(00MAP5,5C,_,_,_,0,4,FV ), 0                         , 105, 0  , 201, 146), // #1669
  INST(Vsubps           , VexRvm_Lx          , V(000F00,5C,_,x,I,0,4,FV ), 0                         , 106, 0  , 202, 145), // #1670
  INST(Vsubsd           , VexRvm             , V(F20F00,5C,_,I,I,1,3,T1S), 0                         , 107, 0  , 203, 147), // #1671
  INST(Vsubsh           , VexRvm             , E(F3MAP5,5C,_,_,_,0,1,T1S), 0                         , 108, 0  , 204, 148), // #1672
  INST(Vsubss           , VexRvm             , V(F30F00,5C,_,I,I,0,2,T1S), 0                         , 109, 0  , 205, 147), // #1673
  INST(Vtestpd          , VexRm_Lx           , V(660F38,0F,_,x,0,_,_,_  ), 0                         , 30 , 0  , 305, 185), // #1674
  INST(Vtestps          , VexRm_Lx           , V(660F38,0E,_,x,0,_,_,_  ), 0                         , 30 , 0  , 305, 185), // #1675
  INST(Vucomisd         , VexRm              , V(660F00,2E,_,I,I,1,3,T1S), 0                         , 126, 0  , 234, 158), // #1676
  INST(Vucomish         , VexRm              , E(00MAP5,2E,_,_,_,0,1,T1S), 0                         , 127, 0  , 235, 159), // #1677
  INST(Vucomiss         , VexRm              , V(000F00,2E,_,I,I,0,2,T1S), 0                         , 128, 0  , 236, 158), // #1678
  INST(Vunpckhpd        , VexRvm_Lx          , V(660F00,15,_,x,I,1,4,FV ), 0                         , 104, 0  , 212, 145), // #1679
  INST(Vunpckhps        , VexRvm_Lx          , V(000F00,15,_,x,I,0,4,FV ), 0                         , 106, 0  , 213, 145), // #1680
  INST(Vunpcklpd        , VexRvm_Lx          , V(660F00,14,_,x,I,1,4,FV ), 0                         , 104, 0  , 212, 145), // #1681
  INST(Vunpcklps        , VexRvm_Lx          , V(000F00,14,_,x,I,0,4,FV ), 0                         , 106, 0  , 213, 145), // #1682
  INST(Vxorpd           , VexRvm_Lx          , V(660F00,57,_,x,I,1,4,FV ), 0                         , 104, 0  , 425, 153), // #1683
  INST(Vxorps           , VexRvm_Lx          , V(000F00,57,_,x,I,0,4,FV ), 0                         , 106, 0  , 424, 153), // #1684
  INST(Vzeroall         , VexOp              , V(000F00,77,_,1,I,_,_,_  ), 0                         , 70 , 0  , 448, 149), // #1685
  INST(Vzeroupper       , VexOp              , V(000F00,77,_,0,I,_,_,_  ), 0                         , 74 , 0  , 448, 149), // #1686
  INST(Wbinvd           , X86Op              , O(000F00,09,_,_,_,_,_,_  ), 0                         , 5  , 0  , 31 , 45 ), // #1687
  INST(Wbnoinvd         , X86Op              , O(F30F00,09,_,_,_,_,_,_  ), 0                         , 7  , 0  , 31 , 199), // #1688
  INST(Wrfsbase         , X86M               , O(F30F00,AE,2,_,x,_,_,_  ), 0                         , 238, 0  , 177, 122), // #1689
  INST(Wrgsbase         , X86M               , O(F30F00,AE,3,_,x,_,_,_  ), 0                         , 239, 0  , 177, 122), // #1690
  INST(Wrmsr            , X86Op              , O(000F00,30,_,_,_,_,_,_  ), 0                         , 5  , 0  , 178, 123), // #1691
  INST(Wrssd            , X86Mr              , O(000F38,F6,_,_,_,_,_,_  ), 0                         , 1  , 0  , 449, 65 ), // #1692
  INST(Wrssq            , X86Mr              , O(000F38,F6,_,_,1,_,_,_  ), 0                         , 240, 0  , 450, 65 ), // #1693
  INST(Wrussd           , X86Mr              , O(660F38,F5,_,_,_,_,_,_  ), 0                         , 2  , 0  , 449, 65 ), // #1694
  INST(Wrussq           , X86Mr              , O(660F38,F5,_,_,1,_,_,_  ), 0                         , 241, 0  , 450, 65 ), // #1695
  INST(Xabort           , X86Op_Mod11RM_I8   , O(000000,C6,7,_,_,_,_,_  ), 0                         , 29 , 0  , 84 , 200), // #1696
  INST(Xadd             , X86Xadd            , O(000F00,C0,_,_,x,_,_,_  ), 0                         , 5  , 0  , 451, 40 ), // #1697
  INST(Xbegin           , X86JmpRel          , O(000000,C7,7,_,_,_,_,_  ), 0                         , 29 , 0  , 452, 200), // #1698
  INST(Xchg             , X86Xchg            , O(000000,86,_,_,x,_,_,_  ), 0                         , 0  , 0  , 453, 0  ), // #1699
  INST(Xend             , X86Op              , O(000F01,D5,_,_,_,_,_,_  ), 0                         , 23 , 0  , 31 , 200), // #1700
  INST(Xgetbv           , X86Op              , O(000F01,D0,_,_,_,_,_,_  ), 0                         , 23 , 0  , 178, 201), // #1701
  INST(Xlatb            , X86Op              , O(000000,D7,_,_,_,_,_,_  ), 0                         , 0  , 0  , 31 , 0  ), // #1702
  INST(Xor              , X86Arith           , O(000000,30,6,_,x,_,_,_  ), 0                         , 34 , 0  , 183, 1  ), // #1703
  INST(Xorpd            , ExtRm              , O(660F00,57,_,_,_,_,_,_  ), 0                         , 4  , 0  , 155, 5  ), // #1704
  INST(Xorps            , ExtRm              , O(000F00,57,_,_,_,_,_,_  ), 0                         , 5  , 0  , 155, 6  ), // #1705
  INST(Xresldtrk        , X86Op              , O(F20F01,E9,_,_,_,_,_,_  ), 0                         , 93 , 0  , 31 , 202), // #1706
  INST(Xrstor           , X86M_Only_EDX_EAX  , O(000F00,AE,5,_,_,_,_,_  ), 0                         , 79 , 0  , 454, 201), // #1707
  INST(Xrstor64         , X86M_Only_EDX_EAX  , O(000F00,AE,5,_,1,_,_,_  ), 0                         , 242, 0  , 455, 201), // #1708
  INST(Xrstors          , X86M_Only_EDX_EAX  , O(000F00,C7,3,_,_,_,_,_  ), 0                         , 80 , 0  , 454, 203), // #1709
  INST(Xrstors64        , X86M_Only_EDX_EAX  , O(000F00,C7,3,_,1,_,_,_  ), 0                         , 243, 0  , 455, 203), // #1710
  INST(Xsave            , X86M_Only_EDX_EAX  , O(000F00,AE,4,_,_,_,_,_  ), 0                         , 98 , 0  , 454, 201), // #1711
  INST(Xsave64          , X86M_Only_EDX_EAX  , O(000F00,AE,4,_,1,_,_,_  ), 0                         , 244, 0  , 455, 201), // #1712
  INST(Xsavec           , X86M_Only_EDX_EAX  , O(000F00,C7,4,_,_,_,_,_  ), 0                         , 98 , 0  , 454, 204), // #1713
  INST(Xsavec64         , X86M_Only_EDX_EAX  , O(000F00,C7,4,_,1,_,_,_  ), 0                         , 244, 0  , 455, 204), // #1714
  INST(Xsaveopt         , X86M_Only_EDX_EAX  , O(000F00,AE,6,_,_,_,_,_  ), 0                         , 82 , 0  , 454, 205), // #1715
  INST(Xsaveopt64       , X86M_Only_EDX_EAX  , O(000F00,AE,6,_,1,_,_,_  ), 0                         , 245, 0  , 455, 205), // #1716
  INST(Xsaves           , X86M_Only_EDX_EAX  , O(000F00,C7,5,_,_,_,_,_  ), 0                         , 79 , 0  , 454, 203), // #1717
  INST(Xsaves64         , X86M_Only_EDX_EAX  , O(000F00,C7,5,_,1,_,_,_  ), 0                         , 242, 0  , 455, 203), // #1718
  INST(Xsetbv           , X86Op              , O(000F01,D1,_,_,_,_,_,_  ), 0                         , 23 , 0  , 178, 201), // #1719
  INST(Xsusldtrk        , X86Op              , O(F20F01,E8,_,_,_,_,_,_  ), 0                         , 93 , 0  , 31 , 202), // #1720
  INST(Xtest            , X86Op              , O(000F01,D6,_,_,_,_,_,_  ), 0                         , 23 , 0  , 31 , 206)  // #1721
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
  O(000F38,00,0,0,0,0,0,0   ), // #1 [ref=25x]
  O(660F38,00,0,0,0,0,0,0   ), // #2 [ref=44x]
  O(000000,00,2,0,0,0,0,0   ), // #3 [ref=4x]
  O(660F00,00,0,0,0,0,0,0   ), // #4 [ref=38x]
  O(000F00,00,0,0,0,0,0,0   ), // #5 [ref=231x]
  O(F20F00,00,0,0,0,0,0,0   ), // #6 [ref=24x]
  O(F30F00,00,0,0,0,0,0,0   ), // #7 [ref=29x]
  O(F30F38,00,0,0,0,0,0,0   ), // #8 [ref=3x]
  O(660F3A,00,0,0,0,0,0,0   ), // #9 [ref=22x]
  O(000000,00,4,0,0,0,0,0   ), // #10 [ref=5x]
  V(000F38,00,0,0,0,0,0,None), // #11 [ref=13x]
  O(F20F38,00,0,0,0,0,0,0   ), // #12 [ref=3x]
  V(XOP_M9,00,1,0,0,0,0,None), // #13 [ref=3x]
  V(XOP_M9,00,6,0,0,0,0,None), // #14 [ref=2x]
  V(XOP_M9,00,5,0,0,0,0,None), // #15 [ref=1x]
  V(XOP_M9,00,3,0,0,0,0,None), // #16 [ref=1x]
  V(XOP_M9,00,2,0,0,0,0,None), // #17 [ref=1x]
  V(000F38,00,3,0,0,0,0,None), // #18 [ref=1x]
  V(000F38,00,2,0,0,0,0,None), // #19 [ref=1x]
  V(000F38,00,1,0,0,0,0,None), // #20 [ref=1x]
  O(660000,00,0,0,0,0,0,0   ), // #21 [ref=7x]
  O(000000,00,0,0,1,0,0,0   ), // #22 [ref=3x]
  O(000F01,00,0,0,0,0,0,0   ), // #23 [ref=32x]
  O(000F00,00,7,0,0,0,0,0   ), // #24 [ref=6x]
  O(660F00,00,7,0,0,0,0,0   ), // #25 [ref=1x]
  O(F30F00,00,6,0,0,0,0,0   ), // #26 [ref=4x]
  O(F30F01,00,0,0,0,0,0,0   ), // #27 [ref=9x]
  O(660F00,00,6,0,0,0,0,0   ), // #28 [ref=3x]
  O(000000,00,7,0,0,0,0,0   ), // #29 [ref=5x]
  V(660F38,00,0,0,0,0,0,None), // #30 [ref=48x]
  O(000F00,00,1,0,1,0,0,0   ), // #31 [ref=2x]
  O(000F00,00,1,0,0,0,0,0   ), // #32 [ref=6x]
  O(000000,00,1,0,0,0,0,0   ), // #33 [ref=3x]
  O(000000,00,6,0,0,0,0,0   ), // #34 [ref=3x]
  O(F30F00,00,7,0,0,0,0,3   ), // #35 [ref=1x]
  O(F30F00,00,7,0,0,0,0,2   ), // #36 [ref=1x]
  O_FPU(00,D900,0)           , // #37 [ref=29x]
  O_FPU(00,C000,0)           , // #38 [ref=1x]
  O_FPU(00,DE00,0)           , // #39 [ref=7x]
  O_FPU(00,0000,4)           , // #40 [ref=4x]
  O_FPU(00,0000,6)           , // #41 [ref=4x]
  O_FPU(9B,DB00,0)           , // #42 [ref=2x]
  O_FPU(00,DA00,0)           , // #43 [ref=5x]
  O_FPU(00,DB00,0)           , // #44 [ref=8x]
  O_FPU(00,D000,2)           , // #45 [ref=1x]
  O_FPU(00,DF00,0)           , // #46 [ref=2x]
  O_FPU(00,D800,3)           , // #47 [ref=1x]
  O_FPU(00,F000,6)           , // #48 [ref=1x]
  O_FPU(00,F800,7)           , // #49 [ref=1x]
  O_FPU(00,DD00,0)           , // #50 [ref=3x]
  O_FPU(00,0000,0)           , // #51 [ref=4x]
  O_FPU(00,0000,2)           , // #52 [ref=3x]
  O_FPU(00,0000,3)           , // #53 [ref=3x]
  O_FPU(00,0000,7)           , // #54 [ref=3x]
  O_FPU(00,0000,1)           , // #55 [ref=2x]
  O_FPU(00,0000,5)           , // #56 [ref=2x]
  O_FPU(00,C800,1)           , // #57 [ref=1x]
  O_FPU(9B,0000,6)           , // #58 [ref=2x]
  O_FPU(9B,0000,7)           , // #59 [ref=2x]
  O_FPU(00,E000,4)           , // #60 [ref=1x]
  O_FPU(00,E800,5)           , // #61 [ref=1x]
  O(000F00,00,0,0,1,0,0,0   ), // #62 [ref=3x]
  O(F30F3A,00,0,0,0,0,0,0   ), // #63 [ref=1x]
  O(000000,00,5,0,0,0,0,0   ), // #64 [ref=4x]
  O(F30F00,00,5,0,0,0,0,0   ), // #65 [ref=2x]
  O(F30F00,00,5,0,1,0,0,0   ), // #66 [ref=1x]
  V(660F00,00,0,1,0,0,0,None), // #67 [ref=7x]
  V(660F00,00,0,1,1,0,0,None), // #68 [ref=6x]
  V(000F00,00,0,1,1,0,0,None), // #69 [ref=7x]
  V(000F00,00,0,1,0,0,0,None), // #70 [ref=8x]
  V(660F00,00,0,0,0,0,0,None), // #71 [ref=15x]
  V(660F00,00,0,0,1,0,0,None), // #72 [ref=4x]
  V(000F00,00,0,0,1,0,0,None), // #73 [ref=4x]
  V(000F00,00,0,0,0,0,0,None), // #74 [ref=10x]
  V(660F3A,00,0,0,0,0,0,None), // #75 [ref=48x]
  V(660F3A,00,0,0,1,0,0,None), // #76 [ref=4x]
  O(000000,00,3,0,0,0,0,0   ), // #77 [ref=4x]
  O(000F00,00,2,0,0,0,0,0   ), // #78 [ref=5x]
  O(000F00,00,5,0,0,0,0,0   ), // #79 [ref=4x]
  O(000F00,00,3,0,0,0,0,0   ), // #80 [ref=5x]
  V(XOP_M9,00,0,0,0,0,0,None), // #81 [ref=32x]
  O(000F00,00,6,0,0,0,0,0   ), // #82 [ref=6x]
  V(XOP_MA,00,0,0,0,0,0,None), // #83 [ref=1x]
  V(XOP_MA,00,1,0,0,0,0,None), // #84 [ref=1x]
  V(F20F38,00,0,0,0,0,0,None), // #85 [ref=11x]
  O(000F3A,00,0,0,0,0,0,0   ), // #86 [ref=4x]
  O(F30000,00,0,0,0,0,0,0   ), // #87 [ref=1x]
  O(000F0F,00,0,0,0,0,0,0   ), // #88 [ref=26x]
  V(F30F38,00,0,0,0,0,0,None), // #89 [ref=12x]
  O(000F3A,00,0,0,1,0,0,0   ), // #90 [ref=1x]
  O(660F3A,00,0,0,1,0,0,0   ), // #91 [ref=1x]
  O(F30F00,00,4,0,0,0,0,0   ), // #92 [ref=1x]
  O(F20F01,00,0,0,0,0,0,0   ), // #93 [ref=5x]
  O(F30F00,00,1,0,0,0,0,0   ), // #94 [ref=3x]
  O(F30F00,00,7,0,0,0,0,0   ), // #95 [ref=1x]
  V(F20F3A,00,0,0,0,0,0,None), // #96 [ref=1x]
  O(660F01,00,0,0,0,0,0,0   ), // #97 [ref=4x]
  O(000F00,00,4,0,0,0,0,0   ), // #98 [ref=4x]
  V(XOP_M9,00,7,0,0,0,0,None), // #99 [ref=1x]
  V(XOP_M9,00,4,0,0,0,0,None), // #100 [ref=1x]
  O(F20F00,00,6,0,0,0,0,0   ), // #101 [ref=1x]
  E(F20F38,00,0,2,0,0,4,None), // #102 [ref=4x]
  E(F20F38,00,0,0,0,0,4,None), // #103 [ref=2x]
  V(660F00,00,0,0,0,1,4,ByLL), // #104 [ref=25x]
  E(00MAP5,00,0,0,0,0,4,ByLL), // #105 [ref=10x]
  V(000F00,00,0,0,0,0,4,ByLL), // #106 [ref=19x]
  V(F20F00,00,0,0,0,1,3,None), // #107 [ref=10x]
  E(F3MAP5,00,0,0,0,0,1,None), // #108 [ref=13x]
  V(F30F00,00,0,0,0,0,2,None), // #109 [ref=12x]
  V(F20F00,00,0,0,0,0,0,None), // #110 [ref=4x]
  V(660F38,00,0,0,0,0,4,ByLL), // #111 [ref=50x]
  E(660F3A,00,0,0,0,0,4,ByLL), // #112 [ref=17x]
  E(660F3A,00,0,0,0,1,4,ByLL), // #113 [ref=18x]
  E(660F38,00,0,0,0,1,4,ByLL), // #114 [ref=38x]
  E(660F38,00,0,0,0,0,4,ByLL), // #115 [ref=25x]
  V(660F38,00,0,1,0,0,0,None), // #116 [ref=2x]
  E(660F38,00,0,0,0,0,3,None), // #117 [ref=2x]
  E(660F38,00,0,0,0,0,4,None), // #118 [ref=2x]
  E(660F38,00,0,2,0,0,5,None), // #119 [ref=2x]
  E(660F38,00,0,0,0,1,4,None), // #120 [ref=2x]
  E(660F38,00,0,2,0,1,5,None), // #121 [ref=2x]
  V(660F38,00,0,0,0,1,3,None), // #122 [ref=2x]
  V(660F38,00,0,0,0,0,2,None), // #123 [ref=14x]
  E(000F3A,00,0,0,0,0,4,ByLL), // #124 [ref=5x]
  E(F30F3A,00,0,0,0,0,1,None), // #125 [ref=1x]
  V(660F00,00,0,0,0,1,3,None), // #126 [ref=5x]
  E(00MAP5,00,0,0,0,0,1,None), // #127 [ref=2x]
  V(000F00,00,0,0,0,0,2,None), // #128 [ref=2x]
  E(660F38,00,0,0,0,1,3,None), // #129 [ref=14x]
  E(660F38,00,0,0,0,0,2,None), // #130 [ref=14x]
  V(F30F00,00,0,0,0,0,3,ByLL), // #131 [ref=1x]
  E(F20F38,00,0,0,0,0,4,ByLL), // #132 [ref=2x]
  V(F30F38,00,0,0,0,0,4,ByLL), // #133 [ref=1x]
  V(F20F00,00,0,0,0,1,4,ByLL), // #134 [ref=1x]
  E(66MAP5,00,0,0,0,1,4,ByLL), // #135 [ref=1x]
  E(660F00,00,0,0,0,1,4,ByLL), // #136 [ref=10x]
  E(000F00,00,0,0,0,1,4,ByLL), // #137 [ref=3x]
  E(66MAP5,00,0,0,0,0,3,ByLL), // #138 [ref=1x]
  E(00MAP5,00,0,0,0,0,2,ByLL), // #139 [ref=1x]
  V(660F38,00,0,0,0,0,3,ByLL), // #140 [ref=7x]
  E(66MAP6,00,0,0,0,0,3,ByLL), // #141 [ref=1x]
  E(66MAP5,00,0,0,0,0,2,ByLL), // #142 [ref=4x]
  E(00MAP5,00,0,0,0,0,3,ByLL), // #143 [ref=2x]
  E(66MAP5,00,0,0,0,0,4,ByLL), // #144 [ref=3x]
  V(660F00,00,0,0,0,0,4,ByLL), // #145 [ref=43x]
  V(000F00,00,0,0,0,0,3,ByLL), // #146 [ref=1x]
  V(660F3A,00,0,0,0,0,3,ByLL), // #147 [ref=1x]
  E(660F00,00,0,0,0,0,3,ByLL), // #148 [ref=4x]
  E(000F00,00,0,0,0,0,4,ByLL), // #149 [ref=2x]
  E(F30F00,00,0,0,0,1,4,ByLL), // #150 [ref=3x]
  E(00MAP5,00,0,0,0,1,4,ByLL), // #151 [ref=1x]
  E(F2MAP5,00,0,0,0,1,3,None), // #152 [ref=1x]
  V(F20F00,00,0,0,0,0,3,None), // #153 [ref=2x]
  E(F20F00,00,0,0,0,0,3,None), // #154 [ref=2x]
  E(00MAP6,00,0,0,0,0,1,None), // #155 [ref=1x]
  V(F20F00,00,0,0,0,0,2,T1W ), // #156 [ref=1x]
  E(F3MAP5,00,0,0,0,0,2,T1W ), // #157 [ref=2x]
  V(F30F00,00,0,0,0,0,2,T1W ), // #158 [ref=1x]
  E(00MAP5,00,0,0,0,0,2,None), // #159 [ref=1x]
  E(F30F00,00,0,0,0,0,2,None), // #160 [ref=2x]
  E(F3MAP5,00,0,0,0,0,3,ByLL), // #161 [ref=1x]
  V(F30F00,00,0,0,0,0,4,ByLL), // #162 [ref=4x]
  E(F30F00,00,0,0,0,0,3,ByLL), // #163 [ref=1x]
  E(F2MAP5,00,0,0,0,0,4,ByLL), // #164 [ref=2x]
  E(F20F00,00,0,0,0,0,4,ByLL), // #165 [ref=2x]
  E(F2MAP5,00,0,0,0,1,4,ByLL), // #166 [ref=1x]
  E(F20F00,00,0,0,0,1,4,ByLL), // #167 [ref=2x]
  E(F20F00,00,0,0,0,0,2,T1W ), // #168 [ref=1x]
  E(F30F00,00,0,0,0,0,2,T1W ), // #169 [ref=1x]
  E(F3MAP5,00,0,0,0,0,4,ByLL), // #170 [ref=1x]
  E(F30F38,00,0,0,0,0,4,ByLL), // #171 [ref=3x]
  E(660F38,00,0,2,0,1,4,ByLL), // #172 [ref=3x]
  E(660F38,00,0,2,0,0,4,ByLL), // #173 [ref=3x]
  V(660F3A,00,0,1,0,0,0,None), // #174 [ref=6x]
  E(660F3A,00,0,0,0,0,4,None), // #175 [ref=4x]
  E(660F3A,00,0,2,0,0,5,None), // #176 [ref=4x]
  E(660F3A,00,0,0,0,1,4,None), // #177 [ref=4x]
  E(660F3A,00,0,2,0,1,5,None), // #178 [ref=4x]
  V(660F3A,00,0,0,0,0,2,None), // #179 [ref=4x]
  E(F2MAP6,00,0,0,0,0,4,ByLL), // #180 [ref=2x]
  E(F2MAP6,00,0,0,0,0,2,None), // #181 [ref=2x]
  E(660F3A,00,0,0,0,1,3,None), // #182 [ref=6x]
  E(660F3A,00,0,0,0,0,2,None), // #183 [ref=6x]
  V(660F38,00,0,0,1,1,4,ByLL), // #184 [ref=22x]
  E(66MAP6,00,0,0,0,0,4,ByLL), // #185 [ref=22x]
  V(660F38,00,0,0,1,1,3,None), // #186 [ref=12x]
  E(66MAP6,00,0,0,0,0,1,None), // #187 [ref=16x]
  E(F3MAP6,00,0,0,0,0,4,ByLL), // #188 [ref=2x]
  E(F3MAP6,00,0,0,0,0,2,None), // #189 [ref=2x]
  E(000F3A,00,0,0,0,0,1,None), // #190 [ref=4x]
  V(660F38,00,0,0,1,0,0,None), // #191 [ref=5x]
  E(660F38,00,1,2,0,1,3,None), // #192 [ref=2x]
  E(660F38,00,1,2,0,0,2,None), // #193 [ref=2x]
  E(660F38,00,2,2,0,1,3,None), // #194 [ref=2x]
  E(660F38,00,2,2,0,0,2,None), // #195 [ref=2x]
  V(660F3A,00,0,0,1,1,4,ByLL), // #196 [ref=2x]
  V(000F00,00,2,0,0,0,0,None), // #197 [ref=1x]
  V(660F00,00,0,0,0,0,2,None), // #198 [ref=1x]
  V(F20F00,00,0,0,0,1,3,DUP ), // #199 [ref=1x]
  E(660F00,00,0,0,0,0,4,ByLL), // #200 [ref=6x]
  V(F30F00,00,0,0,0,0,0,None), // #201 [ref=3x]
  E(F30F00,00,0,0,0,0,4,ByLL), // #202 [ref=1x]
  V(000F00,00,0,0,0,0,3,None), // #203 [ref=2x]
  E(66MAP5,00,0,0,0,0,1,None), // #204 [ref=1x]
  E(F20F38,00,0,0,0,1,4,ByLL), // #205 [ref=1x]
  V(660F3A,00,0,0,0,0,4,ByLL), // #206 [ref=2x]
  E(F30F38,00,0,0,0,1,0,None), // #207 [ref=5x]
  E(F30F38,00,0,0,0,0,0,None), // #208 [ref=5x]
  V(660F38,00,0,0,0,0,1,None), // #209 [ref=1x]
  V(XOP_M8,00,0,0,0,0,0,None), // #210 [ref=22x]
  V(660F38,00,0,0,0,1,4,ByLL), // #211 [ref=4x]
  E(660F38,00,0,0,0,0,0,None), // #212 [ref=2x]
  E(660F38,00,0,0,0,1,1,None), // #213 [ref=2x]
  E(660F38,00,0,0,1,1,4,ByLL), // #214 [ref=1x]
  V(660F3A,00,0,0,1,1,3,None), // #215 [ref=2x]
  V(660F3A,00,0,0,0,0,1,None), // #216 [ref=1x]
  V(660F00,00,0,0,0,0,1,None), // #217 [ref=1x]
  E(F30F38,00,0,0,0,0,2,ByLL), // #218 [ref=6x]
  E(F30F38,00,0,0,0,0,3,ByLL), // #219 [ref=9x]
  E(F30F38,00,0,0,0,0,1,ByLL), // #220 [ref=3x]
  V(660F38,00,0,0,0,0,2,ByLL), // #221 [ref=4x]
  V(660F38,00,0,0,0,0,1,ByLL), // #222 [ref=2x]
  E(660F00,00,1,0,0,0,4,ByLL), // #223 [ref=1x]
  E(660F00,00,1,0,0,1,4,ByLL), // #224 [ref=1x]
  V(F20F00,00,0,0,0,0,4,ByLL), // #225 [ref=1x]
  V(660F00,00,0,0,0,0,4,None), // #226 [ref=6x]
  V(660F00,00,7,0,0,0,4,ByLL), // #227 [ref=1x]
  V(660F00,00,0,0,0,1,4,None), // #228 [ref=2x]
  E(660F00,00,0,0,0,1,4,None), // #229 [ref=1x]
  V(660F00,00,3,0,0,0,4,ByLL), // #230 [ref=1x]
  E(F30F38,00,0,0,0,1,4,ByLL), // #231 [ref=2x]
  E(660F38,00,5,2,0,1,3,None), // #232 [ref=2x]
  E(660F38,00,5,2,0,0,2,None), // #233 [ref=2x]
  E(660F38,00,6,2,0,1,3,None), // #234 [ref=2x]
  E(660F38,00,6,2,0,0,2,None), // #235 [ref=2x]
  V(F20F38,00,0,1,0,0,0,None), // #236 [ref=3x]
  V(000F00,00,3,0,0,0,0,None), // #237 [ref=1x]
  O(F30F00,00,2,0,0,0,0,0   ), // #238 [ref=1x]
  O(F30F00,00,3,0,0,0,0,0   ), // #239 [ref=1x]
  O(000F38,00,0,0,1,0,0,0   ), // #240 [ref=1x]
  O(660F38,00,0,0,1,0,0,0   ), // #241 [ref=1x]
  O(000F00,00,5,0,1,0,0,0   ), // #242 [ref=2x]
  O(000F00,00,3,0,1,0,0,0   ), // #243 [ref=1x]
  O(000F00,00,4,0,1,0,0,0   ), // #244 [ref=2x]
  O(000F00,00,6,0,1,0,0,0   )  // #245 [ref=1x]
};
// ----------------------------------------------------------------------------
// ${MainOpcodeTable:End}

// ${AltOpcodeTable:Begin}
// ------------------- Automatically generated, do not edit -------------------
const uint32_t InstDB::_altOpcodeTable[] = {
  O(000000,00,0,0,0,0,0,0   ), // #0 [ref=1573x]
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
  { 0                                                 , 0                             , 457, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #1 [ref=4x]
  { 0                                                 , 0                             , 458, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #2 [ref=2x]
  { 0                                                 , 0                             , 108, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #3 [ref=6x]
  { F(Lock)|F(XAcquire)|F(XRelease)                   , 0                             , 20 , 13, CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #4 [ref=2x]
  { 0                                                 , 0                             , 50 , 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #5 [ref=2x]
  { F(Vec)                                            , 0                             , 72 , 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #6 [ref=54x]
  { F(Vec)                                            , 0                             , 143, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #7 [ref=19x]
  { F(Vec)                                            , 0                             , 283, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #8 [ref=16x]
  { F(Vec)                                            , 0                             , 292, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #9 [ref=20x]
  { F(Lock)|F(XAcquire)|F(XRelease)                   , 0                             , 33 , 12, CONTROL_FLOW(Regular), SAME_REG_HINT(RO)}, // #10 [ref=1x]
  { F(Vex)                                            , 0                             , 325, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #11 [ref=3x]
  { F(Vec)                                            , 0                             , 72 , 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(RO)}, // #12 [ref=12x]
  { 0                                                 , 0                             , 459, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #13 [ref=1x]
  { F(Vex)                                            , 0                             , 327, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #14 [ref=5x]
  { F(Vex)                                            , 0                             , 50 , 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #15 [ref=12x]
  { F(Vec)                                            , 0                             , 460, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #16 [ref=4x]
  { 0                                                 , 0                             , 329, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #17 [ref=3x]
  { F(Mib)                                            , 0                             , 461, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #18 [ref=1x]
  { 0                                                 , 0                             , 462, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #19 [ref=1x]
  { 0                                                 , 0                             , 331, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #20 [ref=1x]
  { F(Mib)                                            , 0                             , 463, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #21 [ref=1x]
  { 0                                                 , 0                             , 333, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #22 [ref=1x]
  { 0                                                 , 0                             , 49 , 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #23 [ref=35x]
  { 0                                                 , 0                             , 335, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #24 [ref=3x]
  { 0                                                 , 0                             , 134, 5 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #25 [ref=1x]
  { F(Lock)|F(XAcquire)|F(XRelease)                   , 0                             , 134, 5 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #26 [ref=3x]
  { F(Rep)|F(RepIgnored)                              , 0                             , 235, 3 , CONTROL_FLOW(Call), SAME_REG_HINT(None)}, // #27 [ref=1x]
  { 0                                                 , 0                             , 464, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #28 [ref=1x]
  { 0                                                 , 0                             , 465, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #29 [ref=2x]
  { 0                                                 , 0                             , 436, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #30 [ref=1x]
  { 0                                                 , 0                             , 110, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #31 [ref=87x]
  { 0                                                 , 0                             , 466, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #32 [ref=24x]
  { 0                                                 , 0                             , 467, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #33 [ref=6x]
  { 0                                                 , 0                             , 468, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #34 [ref=14x]
  { 0                                                 , 0                             , 469, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #35 [ref=1x]
  { 0                                                 , 0                             , 20 , 13, CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #36 [ref=1x]
  { F(Vex)                                            , 0                             , 337, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #37 [ref=16x]
  { F(Rep)                                            , 0                             , 179, 4 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #38 [ref=1x]
  { F(Vec)                                            , 0                             , 470, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #39 [ref=2x]
  { F(Vec)                                            , 0                             , 471, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #40 [ref=3x]
  { F(Lock)|F(XAcquire)|F(XRelease)                   , 0                             , 183, 4 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #41 [ref=1x]
  { F(Lock)|F(XAcquire)|F(XRelease)                   , 0                             , 472, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #42 [ref=1x]
  { F(Lock)|F(XAcquire)|F(XRelease)                   , 0                             , 473, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #43 [ref=1x]
  { 0                                                 , 0                             , 474, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #44 [ref=1x]
  { 0                                                 , 0                             , 475, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #45 [ref=1x]
  { 0                                                 , 0                             , 339, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #46 [ref=1x]
  { F(Mmx)|F(Vec)                                     , 0                             , 476, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #47 [ref=2x]
  { F(Mmx)|F(Vec)                                     , 0                             , 477, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #48 [ref=2x]
  { F(Mmx)|F(Vec)                                     , 0                             , 478, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #49 [ref=2x]
  { F(Vec)                                            , 0                             , 341, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #50 [ref=2x]
  { F(Vec)                                            , 0                             , 343, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #51 [ref=1x]
  { F(Vec)                                            , 0                             , 345, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #52 [ref=1x]
  { F(Vec)                                            , 0                             , 347, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #53 [ref=1x]
  { F(Vec)                                            , 0                             , 349, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #54 [ref=1x]
  { 0                                                 , 0                             , 479, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #55 [ref=1x]
  { 0                                                 , 0                             , 480, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #56 [ref=3x]
  { F(Lock)|F(XAcquire)|F(XRelease)                   , 0                             , 238, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #57 [ref=1x]
  { 0                                                 , 0                             , 45 , 4 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #58 [ref=3x]
  { F(Mmx)                                            , 0                             , 110, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #59 [ref=1x]
  { 0                                                 , 0                             , 351, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #60 [ref=2x]
  { 0                                                 , 0                             , 481, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #61 [ref=1x]
  { F(Vec)                                            , 0                             , 482, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #62 [ref=2x]
  { F(Vec)                                            , 0                             , 353, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #63 [ref=1x]
  { F(FpuM32)|F(FpuM64)                               , 0                             , 241, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #64 [ref=6x]
  { 0                                                 , 0                             , 355, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #65 [ref=9x]
  { F(FpuM80)                                         , 0                             , 483, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #66 [ref=2x]
  { 0                                                 , 0                             , 356, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #67 [ref=13x]
  { F(FpuM32)|F(FpuM64)                               , 0                             , 357, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #68 [ref=2x]
  { F(FpuM16)|F(FpuM32)                               , 0                             , 484, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #69 [ref=9x]
  { F(FpuM16)|F(FpuM32)|F(FpuM64)                     , 0                             , 485, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #70 [ref=3x]
  { F(FpuM32)|F(FpuM64)|F(FpuM80)                     , 0                             , 486, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #71 [ref=2x]
  { F(FpuM16)                                         , 0                             , 487, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #72 [ref=3x]
  { F(FpuM16)                                         , 0                             , 488, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #73 [ref=2x]
  { F(FpuM32)|F(FpuM64)                               , 0                             , 358, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #74 [ref=1x]
  { 0                                                 , 0                             , 489, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #75 [ref=4x]
  { 0                                                 , 0                             , 490, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #76 [ref=1x]
  { 0                                                 , 0                             , 45 , 10, CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #77 [ref=1x]
  { 0                                                 , 0                             , 491, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #78 [ref=1x]
  { F(Lock)                                           , 0                             , 238, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #79 [ref=1x]
  { 0                                                 , 0                             , 379, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #80 [ref=2x]
  { 0                                                 , 0                             , 336, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #81 [ref=3x]
  { F(Rep)                                            , 0                             , 492, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #82 [ref=1x]
  { F(Vec)                                            , 0                             , 359, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #83 [ref=1x]
  { 0                                                 , 0                             , 493, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #84 [ref=2x]
  { 0                                                 , 0                             , 494, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #85 [ref=8x]
  { 0                                                 , 0                             , 361, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #86 [ref=3x]
  { 0                                                 , 0                             , 363, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #87 [ref=1x]
  { 0                                                 , 0                             , 365, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #88 [ref=1x]
  { 0                                                 , 0                             , 110, 1 , CONTROL_FLOW(Return), SAME_REG_HINT(None)}, // #89 [ref=2x]
  { 0                                                 , 0                             , 468, 1 , CONTROL_FLOW(Return), SAME_REG_HINT(None)}, // #90 [ref=1x]
  { F(Rep)                                            , 0                             , 244, 1 , CONTROL_FLOW(Branch), SAME_REG_HINT(None)}, // #91 [ref=30x]
  { F(Rep)                                            , 0                             , 367, 2 , CONTROL_FLOW(Branch), SAME_REG_HINT(None)}, // #92 [ref=1x]
  { F(Rep)                                            , 0                             , 244, 3 , CONTROL_FLOW(Jump), SAME_REG_HINT(None)}, // #93 [ref=1x]
  { F(Vex)                                            , 0                             , 495, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #94 [ref=19x]
  { F(Vex)                                            , 0                             , 369, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #95 [ref=1x]
  { F(Vex)                                            , 0                             , 371, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #96 [ref=1x]
  { F(Vex)                                            , 0                             , 187, 4 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #97 [ref=1x]
  { F(Vex)                                            , 0                             , 373, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #98 [ref=1x]
  { F(Vex)                                            , 0                             , 496, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #99 [ref=12x]
  { F(Vex)                                            , 0                             , 497, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #100 [ref=8x]
  { F(Vex)                                            , 0                             , 495, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(WO)}, // #101 [ref=8x]
  { 0                                                 , 0                             , 498, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #102 [ref=2x]
  { 0                                                 , 0                             , 253, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #103 [ref=1x]
  { 0                                                 , 0                             , 247, 3 , CONTROL_FLOW(Call), SAME_REG_HINT(None)}, // #104 [ref=1x]
  { F(Vec)                                            , 0                             , 169, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #105 [ref=2x]
  { 0                                                 , 0                             , 499, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #106 [ref=2x]
  { 0                                                 , 0                             , 375, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #107 [ref=2x]
  { F(Vex)                                            , 0                             , 500, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #108 [ref=2x]
  { 0                                                 , 0                             , 377, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #109 [ref=1x]
  { 0                                                 , 0                             , 250, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #110 [ref=3x]
  { 0                                                 , 0                             , 247, 3 , CONTROL_FLOW(Jump), SAME_REG_HINT(None)}, // #111 [ref=1x]
  { 0                                                 , 0                             , 501, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #112 [ref=5x]
  { F(Vex)                                            , 0                             , 379, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #113 [ref=2x]
  { F(Rep)                                            , 0                             , 191, 4 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #114 [ref=1x]
  { 0                                                 , 0                             , 367, 2 , CONTROL_FLOW(Branch), SAME_REG_HINT(None)}, // #115 [ref=3x]
  { 0                                                 , 0                             , 253, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #116 [ref=1x]
  { F(Vex)                                            , 0                             , 381, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #117 [ref=2x]
  { F(Vec)                                            , 0                             , 502, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #118 [ref=1x]
  { F(Mmx)                                            , 0                             , 503, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #119 [ref=1x]
  { 0                                                 , 0                             , 504, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #120 [ref=2x]
  { F(XRelease)                                       , 0                             , 0  , 20, CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #121 [ref=1x]
  { 0                                                 , 0                             , 55 , 9 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #122 [ref=1x]
  { F(Vec)                                            , 0                             , 72 , 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #123 [ref=6x]
  { 0                                                 , 0                             , 104, 6 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #124 [ref=1x]
  { F(Mmx)|F(Vec)                                     , 0                             , 383, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #125 [ref=1x]
  { 0                                                 , 0                             , 385, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #126 [ref=1x]
  { F(Mmx)|F(Vec)                                     , 0                             , 505, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #127 [ref=1x]
  { F(Vec)                                            , 0                             , 354, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #128 [ref=2x]
  { F(Vec)                                            , 0                             , 80 , 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #129 [ref=4x]
  { F(Vec)                                            , 0                             , 506, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #130 [ref=2x]
  { F(Vec)                                            , 0                             , 73 , 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #131 [ref=3x]
  { F(Mmx)                                            , 0                             , 507, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #132 [ref=1x]
  { F(Vec)                                            , 0                             , 80 , 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #133 [ref=1x]
  { F(Vec)                                            , 0                             , 88 , 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #134 [ref=1x]
  { F(Mmx)|F(Vec)                                     , 0                             , 139, 5 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #135 [ref=1x]
  { F(Mmx)|F(Vec)                                     , 0                             , 508, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #136 [ref=1x]
  { F(Rep)                                            , 0                             , 195, 4 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #137 [ref=1x]
  { F(Vec)                                            , 0                             , 387, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #138 [ref=1x]
  { F(Vec)                                            , 0                             , 389, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #139 [ref=1x]
  { 0                                                 , 0                             , 256, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #140 [ref=2x]
  { 0                                                 , 0                             , 391, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #141 [ref=1x]
  { F(Vex)                                            , 0                             , 393, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #142 [ref=1x]
  { 0                                                 , 0                             , 509, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #143 [ref=1x]
  { 0                                                 , 0                             , 510, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #144 [ref=1x]
  { F(Lock)|F(XAcquire)|F(XRelease)                   , 0                             , 239, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #145 [ref=2x]
  { 0                                                 , 0                             , 110, 6 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #146 [ref=1x]
  { F(Lock)|F(XAcquire)|F(XRelease)                   , 0                             , 20 , 13, CONTROL_FLOW(Regular), SAME_REG_HINT(RO)}, // #147 [ref=1x]
  { 0                                                 , 0                             , 511, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #148 [ref=1x]
  { F(Rep)                                            , 0                             , 512, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #149 [ref=1x]
  { F(Mmx)|F(Vec)                                     , 0                             , 395, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #150 [ref=37x]
  { F(Mmx)|F(Vec)                                     , 0                             , 397, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #151 [ref=1x]
  { F(Mmx)|F(Vec)                                     , 0                             , 395, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(RO)}, // #152 [ref=6x]
  { F(Mmx)|F(Vec)                                     , 0                             , 395, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(WO)}, // #153 [ref=16x]
  { F(Mmx)                                            , 0                             , 139, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #154 [ref=26x]
  { F(Vec)                                            , 0                             , 72 , 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(WO)}, // #155 [ref=4x]
  { F(Vec)                                            , 0                             , 513, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #156 [ref=1x]
  { F(Vec)                                            , 0                             , 514, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #157 [ref=1x]
  { F(Vec)                                            , 0                             , 515, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #158 [ref=1x]
  { F(Vec)                                            , 0                             , 516, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #159 [ref=1x]
  { F(Vec)                                            , 0                             , 517, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #160 [ref=1x]
  { F(Vec)                                            , 0                             , 518, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #161 [ref=1x]
  { F(Mmx)|F(Vec)                                     , 0                             , 399, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #162 [ref=1x]
  { F(Vec)                                            , 0                             , 519, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #163 [ref=1x]
  { F(Vec)                                            , 0                             , 520, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #164 [ref=1x]
  { F(Vec)                                            , 0                             , 521, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #165 [ref=1x]
  { F(Mmx)|F(Vec)                                     , 0                             , 522, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #166 [ref=1x]
  { F(Mmx)|F(Vec)                                     , 0                             , 523, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #167 [ref=1x]
  { F(Vec)                                            , 0                             , 313, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #168 [ref=2x]
  { 0                                                 , 0                             , 144, 5 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #169 [ref=1x]
  { F(Mmx)                                            , 0                             , 397, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #170 [ref=1x]
  { F(Mmx)|F(Vec)                                     , 0                             , 401, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #171 [ref=8x]
  { F(Vec)                                            , 0                             , 524, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #172 [ref=2x]
  { 0                                                 , 0                             , 403, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #173 [ref=1x]
  { F(Mmx)|F(Vec)                                     , 0                             , 405, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #174 [ref=3x]
  { 0                                                 , 0                             , 149, 5 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #175 [ref=1x]
  { 0                                                 , 0                             , 407, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #176 [ref=8x]
  { 0                                                 , 0                             , 525, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #177 [ref=4x]
  { 0                                                 , 0                             , 526, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #178 [ref=8x]
  { 0                                                 , 0                             , 409, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #179 [ref=1x]
  { F(Rep)|F(RepIgnored)                              , 0                             , 411, 2 , CONTROL_FLOW(Return), SAME_REG_HINT(None)}, // #180 [ref=1x]
  { 0                                                 , 0                             , 411, 2 , CONTROL_FLOW(Return), SAME_REG_HINT(None)}, // #181 [ref=1x]
  { F(Vex)                                            , 0                             , 413, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #182 [ref=1x]
  { F(Lock)|F(XAcquire)|F(XRelease)                   , 0                             , 20 , 13, CONTROL_FLOW(Regular), SAME_REG_HINT(WO)}, // #183 [ref=3x]
  { F(Rep)                                            , 0                             , 199, 4 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #184 [ref=1x]
  { 0                                                 , 0                             , 527, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #185 [ref=30x]
  { 0                                                 , 0                             , 259, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #186 [ref=2x]
  { 0                                                 , 0                             , 415, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #187 [ref=3x]
  { F(Rep)                                            , 0                             , 203, 4 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #188 [ref=1x]
  { F(Vex)                                            , 0                             , 528, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #189 [ref=8x]
  { 0                                                 , 0                             , 64 , 8 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #190 [ref=1x]
  { F(Tsib)|F(Vex)                                    , 0                             , 529, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #191 [ref=2x]
  { F(Vex)                                            , 0                             , 468, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #192 [ref=1x]
  { F(Tsib)|F(Vex)                                    , 0                             , 530, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #193 [ref=1x]
  { F(Vex)                                            , 0                             , 531, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #194 [ref=1x]
  { 0                                                 , 0                             , 532, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #195 [ref=2x]
  { 0                                                 , 0                             , 50 , 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #196 [ref=2x]
  { 0                                                 , 0                             , 417, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #197 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(K)|X(T4X)|X(Z)              , 533, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #198 [ref=4x]
  { F(Evex)|F(Vec)                                    , X(K)|X(T4X)|X(Z)              , 534, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #199 [ref=2x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(B64)|X(ER)|X(K)|X(SAE)|X(Z) , 262, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #200 [ref=22x]
  { F(Evex)|F(Vec)                                    , X(B16)|X(ER)|X(K)|X(SAE)|X(Z) , 262, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #201 [ref=23x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(B32)|X(ER)|X(K)|X(SAE)|X(Z) , 262, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #202 [ref=22x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(ER)|X(K)|X(SAE)|X(Z)        , 535, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #203 [ref=18x]
  { F(Evex)|F(Vec)                                    , X(ER)|X(K)|X(SAE)|X(Z)        , 536, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #204 [ref=18x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(ER)|X(K)|X(SAE)|X(Z)        , 537, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #205 [ref=17x]
  { F(Vec)|F(Vex)                                     , 0                             , 262, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #206 [ref=29x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , 0                             , 262, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #207 [ref=5x]
  { F(Vec)|F(Vex)                                     , 0                             , 72 , 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #208 [ref=17x]
  { F(Vec)|F(Vex)                                     , 0                             , 292, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #209 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(B32)|X(K)|X(Z)              , 265, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #210 [ref=4x]
  { F(Evex)|F(Vec)                                    , X(B64)|X(K)|X(Z)              , 265, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #211 [ref=4x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(B64)|X(K)|X(Z)              , 262, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #212 [ref=10x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(B32)|X(K)|X(Z)              , 262, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #213 [ref=12x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(B64)|X(K)|X(Z)              , 262, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(RO)}, // #214 [ref=2x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(B32)|X(K)|X(Z)              , 262, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(RO)}, // #215 [ref=6x]
  { F(Vec)|F(Vex)                                     , 0                             , 538, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #216 [ref=2x]
  { F(Evex)|F(Vec)                                    , X(B64)|X(K)|X(Z)              , 262, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #217 [ref=17x]
  { F(Evex)|F(Vec)                                    , X(B32)|X(K)|X(Z)              , 262, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #218 [ref=12x]
  { F(Vec)|F(Vex)                                     , 0                             , 265, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #219 [ref=6x]
  { F(Vec)|F(Vex)                                     , 0                             , 419, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #220 [ref=3x]
  { F(EvexTransformable)|F(Vec)|F(Vex)                , 0                             , 539, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #221 [ref=2x]
  { F(Evex)|F(Vec)                                    , X(K)|X(Z)                     , 540, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #222 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(K)|X(Z)                     , 541, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #223 [ref=4x]
  { F(Evex)|F(Vec)                                    , X(K)|X(Z)                     , 542, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #224 [ref=4x]
  { F(Evex)|F(Vec)                                    , X(K)|X(Z)                     , 447, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #225 [ref=1x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(K)|X(Z)                     , 540, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #226 [ref=1x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(K)|X(Z)                     , 543, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #227 [ref=1x]
  { F(Evex)|F(EvexKReg)|F(Vec)|F(Vex)                 , X(B64)|X(K)|X(SAE)            , 268, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #228 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(B16)|X(K)|X(SAE)            , 271, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #229 [ref=1x]
  { F(Evex)|F(EvexKReg)|F(Vec)|F(Vex)                 , X(B32)|X(K)|X(SAE)            , 268, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #230 [ref=1x]
  { F(Evex)|F(EvexKReg)|F(Vec)|F(Vex)                 , X(K)|X(SAE)                   , 544, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #231 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(K)|X(SAE)                   , 545, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #232 [ref=1x]
  { F(Evex)|F(EvexKReg)|F(Vec)|F(Vex)                 , X(K)|X(SAE)                   , 546, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #233 [ref=1x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(SAE)                        , 143, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #234 [ref=2x]
  { F(Evex)|F(Vec)                                    , X(SAE)                        , 313, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #235 [ref=2x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(SAE)                        , 283, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #236 [ref=2x]
  { F(Evex)|F(Vec)                                    , X(K)|X(Z)                     , 274, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #237 [ref=6x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(B32)|X(K)|X(Z)              , 277, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #238 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(B32)|X(ER)|X(K)|X(SAE)|X(Z) , 421, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #239 [ref=3x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(B32)|X(ER)|X(K)|X(SAE)|X(Z) , 280, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #240 [ref=3x]
  { F(Vec)|F(Vex)                                     , 0                             , 169, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #241 [ref=5x]
  { F(Evex)|F(EvexCompat)|F(PreferEvex)|F(Vec)|F(Vex) , X(B32)|X(K)|X(Z)              , 421, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #242 [ref=1x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(B64)|X(ER)|X(K)|X(SAE)|X(Z) , 421, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #243 [ref=2x]
  { F(Evex)|F(Vec)                                    , X(B64)|X(ER)|X(K)|X(SAE)|X(Z) , 547, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #244 [ref=3x]
  { F(Evex)|F(Vec)                                    , X(B64)|X(ER)|X(K)|X(SAE)|X(Z) , 280, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #245 [ref=4x]
  { F(Evex)|F(Vec)                                    , X(B64)|X(ER)|X(K)|X(SAE)|X(Z) , 421, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #246 [ref=3x]
  { F(Evex)|F(Vec)                                    , X(B16)|X(ER)|X(K)|X(SAE)|X(Z) , 277, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #247 [ref=2x]
  { F(Evex)|F(Vec)                                    , X(B16)|X(K)|X(SAE)|X(Z)       , 283, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #248 [ref=3x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(K)|X(SAE)|X(Z)              , 277, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #249 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(B16)|X(K)|X(SAE)|X(Z)       , 277, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #250 [ref=3x]
  { F(Evex)|F(Vec)                                    , X(B16)|X(ER)|X(K)|X(SAE)|X(Z) , 283, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #251 [ref=2x]
  { F(Evex)|F(Vec)                                    , X(B16)|X(ER)|X(K)|X(SAE)|X(Z) , 280, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #252 [ref=5x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(B32)|X(ER)|X(K)|X(SAE)|X(Z) , 277, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #253 [ref=1x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(K)|X(SAE)|X(Z)              , 286, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #254 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(B32)|X(ER)|X(K)|X(SAE)|X(Z) , 277, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #255 [ref=2x]
  { F(Evex)|F(Vec)                                    , X(B32)|X(ER)|X(K)|X(SAE)|X(Z) , 280, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #256 [ref=2x]
  { F(Evex)|F(Vec)                                    , X(ER)|X(K)|X(SAE)|X(Z)        , 535, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #257 [ref=2x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(ER)|X(SAE)                  , 341, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #258 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(ER)|X(SAE)                  , 341, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #259 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(K)|X(SAE)|X(Z)              , 536, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #260 [ref=5x]
  { F(Evex)|F(Vec)                                    , X(ER)|X(SAE)                  , 423, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #261 [ref=2x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(ER)|X(SAE)                  , 425, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #262 [ref=2x]
  { F(Evex)|F(Vec)                                    , X(ER)|X(SAE)                  , 427, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #263 [ref=2x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(K)|X(SAE)|X(Z)              , 537, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #264 [ref=3x]
  { F(Evex)|F(Vec)                                    , X(ER)|X(K)|X(SAE)|X(Z)        , 537, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #265 [ref=6x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(ER)|X(SAE)                  , 347, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #266 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(ER)|X(SAE)                  , 347, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #267 [ref=1x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(B64)|X(K)|X(SAE)|X(Z)       , 421, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #268 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(B64)|X(K)|X(SAE)|X(Z)       , 280, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #269 [ref=3x]
  { F(Evex)|F(Vec)                                    , X(B64)|X(K)|X(SAE)|X(Z)       , 421, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #270 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(B16)|X(K)|X(SAE)|X(Z)       , 280, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #271 [ref=3x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(B32)|X(K)|X(SAE)|X(Z)       , 280, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #272 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(B32)|X(K)|X(SAE)|X(Z)       , 277, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #273 [ref=2x]
  { F(Evex)|F(Vec)                                    , X(B32)|X(K)|X(SAE)|X(Z)       , 280, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #274 [ref=2x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(SAE)                        , 341, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #275 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(SAE)                        , 341, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #276 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(SAE)                        , 423, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #277 [ref=2x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(SAE)                        , 347, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #278 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(SAE)                        , 347, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #279 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(B32)|X(K)|X(Z)              , 277, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #280 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(ER)|X(SAE)                  , 425, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #281 [ref=2x]
  { F(Evex)|F(Vec)                                    , X(K)|X(Z)                     , 265, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #282 [ref=3x]
  { F(Vec)|F(Vex)                                     , 0                             , 265, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #283 [ref=10x]
  { F(Evex)|F(Vec)                                    , X(B64)|X(K)|X(SAE)|X(Z)       , 78 , 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #284 [ref=3x]
  { F(Evex)|F(Vec)                                    , X(B32)|X(K)|X(SAE)|X(Z)       , 78 , 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #285 [ref=3x]
  { F(Evex)|F(Vec)                                    , X(K)|X(Z)                     , 280, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #286 [ref=8x]
  { F(EvexTransformable)|F(Vec)|F(Vex)                , 0                             , 287, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #287 [ref=2x]
  { F(Evex)|F(Vec)                                    , X(K)|X(Z)                     , 548, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #288 [ref=4x]
  { F(Evex)|F(Vec)                                    , X(K)|X(Z)                     , 288, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #289 [ref=4x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , 0                             , 482, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #290 [ref=2x]
  { F(Evex)|F(Vec)                                    , X(B32)|X(ER)|X(K)|X(SAE)|X(Z) , 262, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #291 [ref=5x]
  { F(Evex)|F(Vec)                                    , X(B64)|X(K)|X(SAE)|X(Z)       , 265, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #292 [ref=2x]
  { F(Evex)|F(Vec)                                    , X(B32)|X(K)|X(SAE)|X(Z)       , 265, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #293 [ref=2x]
  { F(Evex)|F(Vec)                                    , X(K)|X(SAE)|X(Z)              , 549, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #294 [ref=4x]
  { F(Evex)|F(Vec)                                    , X(K)|X(SAE)|X(Z)              , 550, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #295 [ref=4x]
  { F(Vec)|F(Vex)                                     , 0                             , 207, 4 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #296 [ref=13x]
  { F(Vec)|F(Vex)                                     , 0                             , 429, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #297 [ref=4x]
  { F(Vec)|F(Vex)                                     , 0                             , 431, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #298 [ref=4x]
  { F(Evex)|F(Vec)                                    , X(B64)|X(K)                   , 551, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #299 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(B16)|X(K)                   , 551, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #300 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(B32)|X(K)                   , 551, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #301 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(K)                          , 552, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #302 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(K)                          , 553, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #303 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(K)                          , 554, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #304 [ref=1x]
  { F(Vec)|F(Vex)                                     , 0                             , 280, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #305 [ref=7x]
  { F(Vec)|F(Vex)                                     , 0                             , 143, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #306 [ref=1x]
  { F(Vec)|F(Vex)                                     , 0                             , 283, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #307 [ref=1x]
  { F(Evex)|F(EvexTwoOp)|F(Vec)|F(Vex)|F(Vsib)        , X(K)                          , 211, 4 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #308 [ref=2x]
  { F(Evex)|F(EvexTwoOp)|F(Vec)|F(Vex)|F(Vsib)        , X(K)                          , 154, 5 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #309 [ref=2x]
  { F(Evex)|F(Vsib)                                   , X(K)                          , 555, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #310 [ref=4x]
  { F(Evex)|F(Vsib)                                   , X(K)                          , 556, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #311 [ref=4x]
  { F(Evex)|F(Vsib)                                   , X(K)                          , 557, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #312 [ref=8x]
  { F(Evex)|F(EvexTwoOp)|F(Vec)|F(Vex)|F(Vsib)        , X(K)                          , 159, 5 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #313 [ref=2x]
  { F(Evex)|F(EvexTwoOp)|F(Vec)|F(Vex)|F(Vsib)        , X(K)                          , 289, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #314 [ref=2x]
  { F(Evex)|F(Vec)                                    , X(K)|X(SAE)|X(Z)              , 535, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #315 [ref=3x]
  { F(Evex)|F(Vec)                                    , X(K)|X(SAE)|X(Z)              , 537, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #316 [ref=3x]
  { F(Evex)|F(Vec)                                    , X(B64)|X(K)|X(SAE)|X(Z)       , 292, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #317 [ref=2x]
  { F(Evex)|F(Vec)                                    , X(B16)|X(K)|X(SAE)|X(Z)       , 292, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #318 [ref=3x]
  { F(Evex)|F(Vec)                                    , X(B32)|X(K)|X(SAE)|X(Z)       , 292, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #319 [ref=2x]
  { F(Evex)|F(Vec)                                    , X(K)|X(SAE)|X(Z)              , 558, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #320 [ref=3x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(K)|X(Z)                     , 265, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #321 [ref=3x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(K)|X(Z)                     , 262, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #322 [ref=22x]
  { F(EvexTransformable)|F(Vec)|F(Vex)                , 0                             , 433, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #323 [ref=2x]
  { F(Evex)|F(Vec)                                    , X(K)|X(Z)                     , 433, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #324 [ref=4x]
  { F(Evex)|F(Vec)                                    , X(K)|X(Z)                     , 559, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #325 [ref=4x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , 0                             , 550, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #326 [ref=1x]
  { F(Vex)                                            , 0                             , 499, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #327 [ref=2x]
  { F(Vec)|F(Vex)                                     , 0                             , 502, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #328 [ref=1x]
  { F(Vec)|F(Vex)                                     , 0                             , 215, 4 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #329 [ref=4x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(B64)|X(K)|X(SAE)|X(Z)       , 262, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #330 [ref=2x]
  { F(Evex)|F(Vec)                                    , X(B16)|X(K)|X(SAE)|X(Z)       , 262, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #331 [ref=2x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(B32)|X(K)|X(SAE)|X(Z)       , 262, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #332 [ref=2x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(K)|X(SAE)|X(Z)              , 535, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #333 [ref=2x]
  { 0                                                 , 0                             , 435, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #334 [ref=3x]
  { 0                                                 , 0                             , 437, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #335 [ref=1x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(K)|X(Z)                     , 72 , 8 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #336 [ref=4x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , 0                             , 439, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #337 [ref=1x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(K)|X(Z)                     , 295, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #338 [ref=1x]
  { F(EvexTransformable)|F(Vec)|F(Vex)                , 0                             , 72 , 4 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #339 [ref=2x]
  { F(Evex)|F(Vec)                                    , X(K)|X(Z)                     , 116, 6 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #340 [ref=6x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , 0                             , 82 , 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #341 [ref=2x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , 0                             , 219, 4 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #342 [ref=4x]
  { F(Vec)|F(Vex)                                     , 0                             , 560, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #343 [ref=3x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , 0                             , 164, 5 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #344 [ref=3x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , 0                             , 169, 5 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #345 [ref=1x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , 0                             , 174, 5 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #346 [ref=1x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(K)|X(Z)                     , 80 , 8 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #347 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(K)|X(Z)                     , 223, 4 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #348 [ref=1x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(K)|X(Z)                     , 280, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #349 [ref=4x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(K)|X(Z)                     , 88 , 8 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #350 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(K)|X(Z)                     , 441, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #351 [ref=1x]
  { 0                                                 , 0                             , 443, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #352 [ref=1x]
  { 0                                                 , 0                             , 445, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #353 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(B32)                        , 298, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #354 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(B64)                        , 298, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #355 [ref=1x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(B32)|X(K)|X(Z)              , 280, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #356 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(B64)|X(K)|X(Z)              , 280, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #357 [ref=5x]
  { F(EvexTransformable)|F(Vec)|F(Vex)                , 0                             , 262, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(RO)}, // #358 [ref=2x]
  { F(Evex)|F(Vec)                                    , X(B32)|X(K)|X(Z)              , 262, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(RO)}, // #359 [ref=2x]
  { F(EvexTransformable)|F(Vec)|F(Vex)                , 0                             , 262, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(WO)}, // #360 [ref=2x]
  { F(Evex)|F(Vec)                                    , X(B32)|X(K)|X(Z)              , 262, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(WO)}, // #361 [ref=2x]
  { F(Evex)|F(Vec)                                    , X(B64)|X(K)|X(Z)              , 262, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(WO)}, // #362 [ref=2x]
  { F(Evex)|F(Vec)                                    , X(B64)|X(K)|X(Z)              , 262, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(RO)}, // #363 [ref=2x]
  { F(Evex)|F(Vec)                                    , X(K)|X(Z)                     , 262, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #364 [ref=13x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(K)|X(Z)                     , 561, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #365 [ref=1x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(K)|X(Z)                     , 562, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #366 [ref=1x]
  { F(Evex)|F(Vec)                                    , 0                             , 563, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #367 [ref=6x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(K)|X(Z)                     , 447, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #368 [ref=1x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(K)|X(Z)                     , 564, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #369 [ref=1x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , 0                             , 265, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #370 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(K)                          , 271, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(WO)}, // #371 [ref=2x]
  { F(Evex)|F(Vec)                                    , X(B32)|X(K)                   , 271, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(WO)}, // #372 [ref=2x]
  { F(Evex)|F(EvexKReg)|F(Vec)|F(Vex)                 , X(K)                          , 301, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(WO)}, // #373 [ref=4x]
  { F(Evex)|F(EvexKReg)|F(Vec)|F(Vex)                 , X(B32)|X(K)                   , 301, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(WO)}, // #374 [ref=2x]
  { F(Evex)|F(EvexKReg)|F(Vec)|F(Vex)                 , X(B64)|X(K)                   , 301, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(WO)}, // #375 [ref=2x]
  { F(Vec)|F(Vex)                                     , 0                             , 513, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #376 [ref=1x]
  { F(Vec)|F(Vex)                                     , 0                             , 514, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #377 [ref=1x]
  { F(Vec)|F(Vex)                                     , 0                             , 515, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #378 [ref=1x]
  { F(Vec)|F(Vex)                                     , 0                             , 516, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #379 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(B64)|X(K)                   , 271, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(WO)}, // #380 [ref=4x]
  { F(Evex)|F(Vec)                                    , X(B32)|X(K)|X(Z)              , 280, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #381 [ref=6x]
  { F(Evex)|F(EvexCompat)|F(PreferEvex)|F(Vec)|F(Vex) , X(B32)|X(K)|X(Z)              , 262, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #382 [ref=4x]
  { F(Vec)|F(Vex)                                     , 0                             , 266, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #383 [ref=2x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(B32)|X(K)|X(Z)              , 263, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #384 [ref=2x]
  { F(Vec)|F(Vex)                                     , 0                             , 227, 4 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #385 [ref=2x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(B64)|X(K)|X(Z)              , 96 , 8 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #386 [ref=1x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(B32)|X(K)|X(Z)              , 96 , 8 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #387 [ref=1x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(B64)|X(K)|X(Z)              , 231, 4 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #388 [ref=2x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , 0                             , 517, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #389 [ref=1x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , 0                             , 518, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #390 [ref=1x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , 0                             , 565, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #391 [ref=1x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(K)|X(Z)                     , 566, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #392 [ref=1x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(K)|X(Z)                     , 567, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #393 [ref=1x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(K)|X(Z)                     , 568, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #394 [ref=1x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(K)|X(Z)                     , 569, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #395 [ref=1x]
  { F(Vec)|F(Vex)                                     , 0                             , 419, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #396 [ref=12x]
  { F(Evex)|F(EvexCompat)|F(PreferEvex)|F(Vec)|F(Vex) , X(B64)|X(K)|X(Z)              , 262, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #397 [ref=2x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(K)|X(Z)                     , 262, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(RO)}, // #398 [ref=8x]
  { F(Evex)|F(Vec)                                    , 0                             , 570, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #399 [ref=4x]
  { F(Evex)|F(Vec)                                    , X(K)|X(Z)                     , 304, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #400 [ref=6x]
  { F(Evex)|F(Vec)                                    , X(K)|X(Z)                     , 307, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #401 [ref=9x]
  { F(Evex)|F(Vec)                                    , X(K)|X(Z)                     , 310, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #402 [ref=3x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(K)|X(Z)                     , 283, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #403 [ref=4x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(K)|X(Z)                     , 313, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #404 [ref=2x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(K)|X(Z)                     , 277, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #405 [ref=6x]
  { F(Vec)|F(Vex)                                     , 0                             , 207, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #406 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(B32)|X(K)|X(Z)              , 292, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #407 [ref=3x]
  { F(Evex)|F(Vec)                                    , X(B64)|X(K)|X(Z)              , 292, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #408 [ref=3x]
  { F(Vec)|F(Vex)                                     , 0                             , 449, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #409 [ref=4x]
  { F(Evex)|F(Vec)|F(Vsib)                            , X(K)                          , 316, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #410 [ref=2x]
  { F(Evex)|F(Vec)|F(Vsib)                            , X(K)                          , 451, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #411 [ref=2x]
  { F(Evex)|F(Vec)|F(Vsib)                            , X(K)                          , 453, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #412 [ref=2x]
  { F(Evex)|F(Vec)|F(Vsib)                            , X(K)                          , 319, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #413 [ref=2x]
  { F(Vec)|F(Vex)                                     , 0                             , 455, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #414 [ref=8x]
  { F(Evex)|F(Vec)                                    , X(K)                          , 322, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #415 [ref=5x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(B32)|X(K)|X(Z)              , 292, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #416 [ref=1x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(K)|X(Z)                     , 292, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #417 [ref=2x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(B32)|X(K)|X(Z)              , 122, 6 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #418 [ref=3x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , 0                             , 292, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #419 [ref=2x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(B64)|X(K)|X(Z)              , 122, 6 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #420 [ref=2x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(K)|X(Z)                     , 122, 6 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #421 [ref=3x]
  { F(Evex)|F(Vec)                                    , X(B64)|X(K)|X(Z)              , 128, 6 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #422 [ref=1x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(K)|X(Z)                     , 262, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(WO)}, // #423 [ref=6x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(B32)|X(K)|X(Z)              , 262, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(WO)}, // #424 [ref=2x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(B64)|X(K)|X(Z)              , 262, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(WO)}, // #425 [ref=2x]
  { F(Evex)|F(Vec)                                    , X(B32)|X(K)                   , 322, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #426 [ref=2x]
  { F(Evex)|F(Vec)                                    , X(B64)|X(K)                   , 322, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #427 [ref=2x]
  { F(Evex)|F(Vec)                                    , X(K)|X(Z)                     , 535, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #428 [ref=2x]
  { F(Evex)|F(Vec)                                    , X(K)|X(Z)                     , 537, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #429 [ref=2x]
  { F(Evex)|F(Vec)                                    , X(B16)|X(K)|X(Z)              , 280, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #430 [ref=2x]
  { F(Evex)|F(Vec)                                    , X(K)|X(Z)                     , 536, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #431 [ref=2x]
  { F(Vec)|F(Vex)                                     , 0                             , 537, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #432 [ref=2x]
  { F(Evex)|F(Vec)                                    , X(K)|X(Z)                     , 549, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #433 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(K)|X(Z)                     , 550, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #434 [ref=1x]
  { F(EvexTransformable)|F(Vec)|F(Vex)                , 0                             , 292, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #435 [ref=2x]
  { F(EvexTransformable)|F(Vec)|F(Vex)                , 0                             , 549, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #436 [ref=1x]
  { F(EvexTransformable)|F(Vec)|F(Vex)                , 0                             , 550, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #437 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(B64)|X(ER)|X(K)|X(SAE)|X(Z) , 262, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #438 [ref=1x]
  { F(Vec)|F(Vex)                                     , 0                             , 571, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #439 [ref=1x]
  { F(Vec)|F(Vex)                                     , 0                             , 572, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #440 [ref=1x]
  { F(Vec)|F(Vex)                                     , 0                             , 573, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #441 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(B32)|X(K)|X(Z)              , 266, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #442 [ref=2x]
  { F(Evex)|F(Vec)                                    , X(B64)|X(K)|X(Z)              , 266, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #443 [ref=2x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(B64)|X(K)|X(Z)              , 265, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #444 [ref=1x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(B32)|X(K)|X(Z)              , 265, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #445 [ref=1x]
  { F(Vec)|F(Vex)                                     , 0                             , 262, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #446 [ref=2x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(B64)|X(ER)|X(K)|X(SAE)|X(Z) , 280, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #447 [ref=1x]
  { F(Vec)|F(Vex)                                     , 0                             , 110, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #448 [ref=2x]
  { 0                                                 , 0                             , 27 , 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #449 [ref=2x]
  { 0                                                 , 0                             , 28 , 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #450 [ref=2x]
  { F(Lock)|F(XAcquire)|F(XRelease)                   , 0                             , 25 , 4 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #451 [ref=1x]
  { 0                                                 , 0                             , 236, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #452 [ref=1x]
  { F(XAcquire)                                       , 0                             , 25 , 8 , CONTROL_FLOW(Regular), SAME_REG_HINT(RO)}, // #453 [ref=1x]
  { 0                                                 , 0                             , 574, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #454 [ref=6x]
  { 0                                                 , 0                             , 575, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}  // #455 [ref=6x]
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
  { 0, 0, { 0 } }, // #0 [ref=67x]
  { 0, 1, { 0 } }, // #1 [ref=32x]
  { 0, 0, { EXT(RAO_INT) } }, // #2 [ref=4x]
  { 0, 2, { 0 } }, // #3 [ref=2x]
  { 0, 3, { EXT(ADX) } }, // #4 [ref=1x]
  { 0, 0, { EXT(SSE2) } }, // #5 [ref=60x]
  { 0, 0, { EXT(SSE) } }, // #6 [ref=46x]
  { 0, 0, { EXT(SSE3) } }, // #7 [ref=10x]
  { 0, 4, { EXT(ADX) } }, // #8 [ref=1x]
  { 0, 0, { EXT(AESNI) } }, // #9 [ref=6x]
  { 0, 1, { EXT(BMI) } }, // #10 [ref=6x]
  { 0, 5, { 0 } }, // #11 [ref=5x]
  { 0, 0, { EXT(TBM) } }, // #12 [ref=9x]
  { 0, 0, { EXT(SSE4_1) } }, // #13 [ref=47x]
  { 0, 0, { EXT(MPX) } }, // #14 [ref=7x]
  { 0, 6, { 0 } }, // #15 [ref=4x]
  { 0, 1, { EXT(BMI2) } }, // #16 [ref=1x]
  { 0, 7, { EXT(SMAP) } }, // #17 [ref=2x]
  { 0, 8, { 0 } }, // #18 [ref=2x]
  { 0, 9, { 0 } }, // #19 [ref=2x]
  { 0, 0, { EXT(CLDEMOTE) } }, // #20 [ref=1x]
  { 0, 0, { EXT(CLFLUSH) } }, // #21 [ref=1x]
  { 0, 0, { EXT(CLFLUSHOPT) } }, // #22 [ref=1x]
  { 0, 0, { EXT(SVM) } }, // #23 [ref=6x]
  { 0, 10, { 0 } }, // #24 [ref=2x]
  { 0, 1, { EXT(CET_SS) } }, // #25 [ref=3x]
  { 0, 0, { EXT(UINTR) } }, // #26 [ref=4x]
  { 0, 0, { EXT(CLWB) } }, // #27 [ref=1x]
  { 0, 0, { EXT(CLZERO) } }, // #28 [ref=1x]
  { 0, 3, { 0 } }, // #29 [ref=1x]
  { 0, 11, { EXT(CMOV) } }, // #30 [ref=4x]
  { 0, 12, { EXT(CMOV) } }, // #31 [ref=6x]
  { 0, 13, { EXT(CMOV) } }, // #32 [ref=4x]
  { 0, 14, { EXT(CMOV) } }, // #33 [ref=4x]
  { 0, 15, { EXT(CMOV) } }, // #34 [ref=4x]
  { 0, 16, { EXT(CMOV) } }, // #35 [ref=2x]
  { 0, 17, { EXT(CMOV) } }, // #36 [ref=4x]
  { 0, 18, { EXT(CMOV) } }, // #37 [ref=2x]
  { 0, 1, { EXT(CMPCCXADD) } }, // #38 [ref=16x]
  { 0, 19, { 0 } }, // #39 [ref=2x]
  { 0, 1, { EXT(I486) } }, // #40 [ref=2x]
  { 0, 5, { EXT(CMPXCHG16B) } }, // #41 [ref=1x]
  { 0, 5, { EXT(CMPXCHG8B) } }, // #42 [ref=1x]
  { 0, 1, { EXT(SSE2) } }, // #43 [ref=2x]
  { 0, 1, { EXT(SSE) } }, // #44 [ref=2x]
  { 0, 0, { EXT(I486) } }, // #45 [ref=5x]
  { 0, 0, { EXT(SSE4_2) } }, // #46 [ref=2x]
  { 0, 20, { 0 } }, // #47 [ref=2x]
  { 0, 0, { EXT(MMX) } }, // #48 [ref=1x]
  { 0, 0, { EXT(CET_IBT) } }, // #49 [ref=2x]
  { 0, 1, { EXT(ENQCMD) } }, // #50 [ref=2x]
  { 0, 0, { EXT(SSE4A) } }, // #51 [ref=4x]
  { 0, 21, { EXT(FPU) } }, // #52 [ref=80x]
  { 0, 22, { EXT(CMOV), EXT(FPU) } }, // #53 [ref=2x]
  { 0, 23, { EXT(CMOV), EXT(FPU) } }, // #54 [ref=2x]
  { 0, 24, { EXT(CMOV), EXT(FPU) } }, // #55 [ref=2x]
  { 0, 25, { EXT(CMOV), EXT(FPU) } }, // #56 [ref=2x]
  { 0, 26, { EXT(FPU) } }, // #57 [ref=4x]
  { 0, 0, { EXT(3DNOW) } }, // #58 [ref=21x]
  { 0, 21, { EXT(SSE3), EXT(FPU) } }, // #59 [ref=1x]
  { 0, 21, { EXT(FXSR) } }, // #60 [ref=2x]
  { 0, 27, { EXT(FXSR) } }, // #61 [ref=2x]
  { 0, 0, { EXT(SMX) } }, // #62 [ref=1x]
  { 0, 0, { EXT(GFNI) } }, // #63 [ref=3x]
  { 0, 0, { EXT(HRESET) } }, // #64 [ref=1x]
  { 0, 0, { EXT(CET_SS) } }, // #65 [ref=9x]
  { 0, 16, { 0 } }, // #66 [ref=5x]
  { 0, 0, { EXT(VMX) } }, // #67 [ref=13x]
  { 0, 0, { EXT(INVLPGB) } }, // #68 [ref=2x]
  { 0, 11, { 0 } }, // #69 [ref=8x]
  { 0, 12, { 0 } }, // #70 [ref=12x]
  { 0, 13, { 0 } }, // #71 [ref=10x]
  { 0, 14, { 0 } }, // #72 [ref=8x]
  { 0, 15, { 0 } }, // #73 [ref=8x]
  { 0, 17, { 0 } }, // #74 [ref=8x]
  { 0, 18, { 0 } }, // #75 [ref=4x]
  { 0, 0, { EXT(AVX512_DQ) } }, // #76 [ref=22x]
  { 0, 0, { EXT(AVX512_BW) } }, // #77 [ref=20x]
  { 0, 0, { EXT(AVX512_F) } }, // #78 [ref=36x]
  { 1, 0, { EXT(AVX512_DQ) } }, // #79 [ref=1x]
  { 1, 0, { EXT(AVX512_BW) } }, // #80 [ref=2x]
  { 1, 0, { EXT(AVX512_F) } }, // #81 [ref=1x]
  { 0, 1, { EXT(AVX512_DQ) } }, // #82 [ref=3x]
  { 0, 1, { EXT(AVX512_BW) } }, // #83 [ref=4x]
  { 0, 1, { EXT(AVX512_F) } }, // #84 [ref=1x]
  { 0, 28, { EXT(LAHFSAHF) } }, // #85 [ref=1x]
  { 0, 0, { EXT(AMX_TILE) } }, // #86 [ref=7x]
  { 0, 0, { EXT(LWP) } }, // #87 [ref=4x]
  { 0, 29, { 0 } }, // #88 [ref=3x]
  { 0, 1, { EXT(LZCNT) } }, // #89 [ref=1x]
  { 0, 0, { EXT(MMX2) } }, // #90 [ref=3x]
  { 0, 1, { EXT(MCOMMIT) } }, // #91 [ref=1x]
  { 0, 0, { EXT(MONITOR) } }, // #92 [ref=2x]
  { 0, 0, { EXT(MONITORX) } }, // #93 [ref=2x]
  { 1, 0, { 0 } }, // #94 [ref=1x]
  { 1, 0, { EXT(SSE2) } }, // #95 [ref=5x]
  { 1, 0, { EXT(SSE) } }, // #96 [ref=3x]
  { 0, 0, { EXT(MOVBE) } }, // #97 [ref=1x]
  { 0, 0, { EXT(MMX), EXT(SSE2) } }, // #98 [ref=45x]
  { 0, 0, { EXT(MOVDIR64B) } }, // #99 [ref=1x]
  { 0, 0, { EXT(MOVDIRI) } }, // #100 [ref=1x]
  { 1, 0, { EXT(MMX), EXT(SSE2) } }, // #101 [ref=1x]
  { 0, 0, { EXT(BMI2) } }, // #102 [ref=7x]
  { 0, 0, { EXT(SSSE3) } }, // #103 [ref=16x]
  { 0, 0, { EXT(MMX2), EXT(SSE2) } }, // #104 [ref=10x]
  { 0, 0, { EXT(PCLMULQDQ) } }, // #105 [ref=1x]
  { 0, 1, { EXT(SSE4_2) } }, // #106 [ref=4x]
  { 0, 0, { EXT(PCONFIG) } }, // #107 [ref=1x]
  { 0, 0, { EXT(MMX2), EXT(SSE2), EXT(SSE4_1) } }, // #108 [ref=1x]
  { 0, 0, { EXT(3DNOW2) } }, // #109 [ref=5x]
  { 0, 0, { EXT(GEODE) } }, // #110 [ref=2x]
  { 0, 1, { EXT(POPCNT) } }, // #111 [ref=1x]
  { 0, 30, { 0 } }, // #112 [ref=3x]
  { 0, 0, { EXT(PREFETCHI) } }, // #113 [ref=2x]
  { 0, 1, { EXT(PREFETCHW) } }, // #114 [ref=1x]
  { 0, 1, { EXT(PREFETCHWT1) } }, // #115 [ref=1x]
  { 0, 20, { EXT(SEV_SNP) } }, // #116 [ref=3x]
  { 0, 1, { EXT(SSE4_1) } }, // #117 [ref=1x]
  { 0, 0, { EXT(PTWRITE) } }, // #118 [ref=1x]
  { 0, 31, { 0 } }, // #119 [ref=3x]
  { 0, 1, { EXT(SEV_SNP) } }, // #120 [ref=1x]
  { 0, 32, { 0 } }, // #121 [ref=2x]
  { 0, 0, { EXT(FSGSBASE) } }, // #122 [ref=4x]
  { 0, 0, { EXT(MSR) } }, // #123 [ref=2x]
  { 0, 0, { EXT(RDPID) } }, // #124 [ref=1x]
  { 0, 0, { EXT(OSPKE) } }, // #125 [ref=1x]
  { 0, 0, { EXT(RDPRU) } }, // #126 [ref=1x]
  { 0, 1, { EXT(RDRAND) } }, // #127 [ref=1x]
  { 0, 1, { EXT(RDSEED) } }, // #128 [ref=1x]
  { 0, 0, { EXT(RDTSC) } }, // #129 [ref=1x]
  { 0, 0, { EXT(RDTSCP) } }, // #130 [ref=1x]
  { 0, 33, { 0 } }, // #131 [ref=2x]
  { 0, 34, { EXT(LAHFSAHF) } }, // #132 [ref=1x]
  { 0, 0, { EXT(SEAM) } }, // #133 [ref=4x]
  { 0, 0, { EXT(SERIALIZE) } }, // #134 [ref=1x]
  { 0, 0, { EXT(SHA) } }, // #135 [ref=7x]
  { 0, 0, { EXT(SKINIT) } }, // #136 [ref=2x]
  { 0, 0, { EXT(AMX_COMPLEX) } }, // #137 [ref=2x]
  { 0, 0, { EXT(AMX_BF16) } }, // #138 [ref=1x]
  { 0, 0, { EXT(AMX_INT8) } }, // #139 [ref=4x]
  { 0, 0, { EXT(AMX_FP16) } }, // #140 [ref=1x]
  { 0, 1, { EXT(UINTR) } }, // #141 [ref=1x]
  { 0, 1, { EXT(WAITPKG) } }, // #142 [ref=2x]
  { 0, 0, { EXT(WAITPKG) } }, // #143 [ref=1x]
  { 0, 0, { EXT(AVX512_4FMAPS) } }, // #144 [ref=4x]
  { 0, 0, { EXT(AVX), EXT(AVX512_F), EXT(AVX512_VL) } }, // #145 [ref=38x]
  { 0, 0, { EXT(AVX512_FP16), EXT(AVX512_VL) } }, // #146 [ref=60x]
  { 0, 0, { EXT(AVX), EXT(AVX512_F) } }, // #147 [ref=33x]
  { 0, 0, { EXT(AVX512_FP16) } }, // #148 [ref=44x]
  { 0, 0, { EXT(AVX) } }, // #149 [ref=35x]
  { 0, 0, { EXT(AESNI), EXT(VAES), EXT(AVX), EXT(AVX512_F), EXT(AVX512_VL) } }, // #150 [ref=4x]
  { 0, 0, { EXT(AESNI), EXT(AVX) } }, // #151 [ref=2x]
  { 0, 0, { EXT(AVX512_F), EXT(AVX512_VL) } }, // #152 [ref=108x]
  { 0, 0, { EXT(AVX), EXT(AVX512_DQ), EXT(AVX512_VL) } }, // #153 [ref=8x]
  { 0, 0, { EXT(AVX_NE_CONVERT) } }, // #154 [ref=6x]
  { 0, 0, { EXT(AVX512_DQ), EXT(AVX512_VL) } }, // #155 [ref=30x]
  { 0, 0, { EXT(AVX2) } }, // #156 [ref=7x]
  { 0, 0, { EXT(AVX), EXT(AVX2), EXT(AVX512_F), EXT(AVX512_VL) } }, // #157 [ref=39x]
  { 0, 1, { EXT(AVX), EXT(AVX512_F) } }, // #158 [ref=4x]
  { 0, 1, { EXT(AVX512_FP16) } }, // #159 [ref=2x]
  { 0, 0, { EXT(AVX512_BF16), EXT(AVX512_VL) } }, // #160 [ref=2x]
  { 0, 0, { EXT(AVX_NE_CONVERT), EXT(AVX512_BF16), EXT(AVX512_VL) } }, // #161 [ref=1x]
  { 0, 0, { EXT(F16C), EXT(AVX512_F), EXT(AVX512_VL) } }, // #162 [ref=2x]
  { 0, 0, { EXT(AVX512_BW), EXT(AVX512_VL) } }, // #163 [ref=24x]
  { 0, 0, { EXT(AVX512_ER) } }, // #164 [ref=10x]
  { 0, 0, { EXT(FMA), EXT(AVX512_F), EXT(AVX512_VL) } }, // #165 [ref=36x]
  { 0, 0, { EXT(FMA), EXT(AVX512_F) } }, // #166 [ref=24x]
  { 0, 0, { EXT(FMA4) } }, // #167 [ref=20x]
  { 0, 0, { EXT(XOP) } }, // #168 [ref=55x]
  { 0, 0, { EXT(AVX2), EXT(AVX512_F), EXT(AVX512_VL) } }, // #169 [ref=19x]
  { 0, 0, { EXT(AVX512_PF) } }, // #170 [ref=16x]
  { 0, 0, { EXT(GFNI), EXT(AVX), EXT(AVX512_F), EXT(AVX512_VL) } }, // #171 [ref=3x]
  { 0, 0, { EXT(SEV_ES) } }, // #172 [ref=1x]
  { 1, 0, { EXT(AVX), EXT(AVX512_F), EXT(AVX512_VL) } }, // #173 [ref=4x]
  { 1, 0, { EXT(AVX) } }, // #174 [ref=2x]
  { 1, 0, { EXT(AVX512_F), EXT(AVX512_VL) } }, // #175 [ref=4x]
  { 1, 0, { EXT(AVX512_BW), EXT(AVX512_VL) } }, // #176 [ref=2x]
  { 1, 0, { EXT(AVX), EXT(AVX512_F) } }, // #177 [ref=3x]
  { 0, 0, { EXT(AVX), EXT(AVX2) } }, // #178 [ref=17x]
  { 0, 0, { EXT(AVX512_VL), EXT(AVX512_VP2INTERSECT) } }, // #179 [ref=2x]
  { 0, 0, { EXT(AVX512_4VNNIW) } }, // #180 [ref=2x]
  { 0, 0, { EXT(AVX), EXT(AVX2), EXT(AVX512_BW), EXT(AVX512_VL) } }, // #181 [ref=54x]
  { 0, 0, { EXT(AVX2), EXT(AVX512_BW), EXT(AVX512_VL) } }, // #182 [ref=2x]
  { 0, 0, { EXT(AVX512_CD), EXT(AVX512_VL) } }, // #183 [ref=6x]
  { 0, 0, { EXT(PCLMULQDQ), EXT(VPCLMULQDQ), EXT(AVX), EXT(AVX512_F), EXT(AVX512_VL) } }, // #184 [ref=1x]
  { 0, 1, { EXT(AVX) } }, // #185 [ref=7x]
  { 0, 0, { EXT(AVX512_VBMI2), EXT(AVX512_VL) } }, // #186 [ref=16x]
  { 0, 0, { EXT(AVX_VNNI_INT8) } }, // #187 [ref=6x]
  { 0, 0, { EXT(AVX_VNNI), EXT(AVX512_VL), EXT(AVX512_VNNI) } }, // #188 [ref=4x]
  { 0, 0, { EXT(AVX_VNNI_INT16) } }, // #189 [ref=6x]
  { 0, 0, { EXT(AVX512_VBMI), EXT(AVX512_VL) } }, // #190 [ref=4x]
  { 0, 0, { EXT(AVX), EXT(AVX512_BW) } }, // #191 [ref=4x]
  { 0, 0, { EXT(AVX), EXT(AVX512_DQ) } }, // #192 [ref=4x]
  { 0, 0, { EXT(AVX_IFMA), EXT(AVX512_IFMA), EXT(AVX512_VL) } }, // #193 [ref=2x]
  { 0, 0, { EXT(AVX512_BITALG), EXT(AVX512_VL) } }, // #194 [ref=3x]
  { 0, 0, { EXT(AVX512_VL), EXT(AVX512_VPOPCNTDQ) } }, // #195 [ref=2x]
  { 0, 0, { EXT(SHA512), EXT(AVX) } }, // #196 [ref=3x]
  { 0, 0, { EXT(SM3), EXT(AVX) } }, // #197 [ref=3x]
  { 0, 0, { EXT(SM4), EXT(AVX) } }, // #198 [ref=2x]
  { 0, 0, { EXT(WBNOINVD) } }, // #199 [ref=1x]
  { 0, 0, { EXT(RTM) } }, // #200 [ref=3x]
  { 0, 0, { EXT(XSAVE) } }, // #201 [ref=6x]
  { 0, 0, { EXT(TSXLDTRK) } }, // #202 [ref=2x]
  { 0, 0, { EXT(XSAVES) } }, // #203 [ref=4x]
  { 0, 0, { EXT(XSAVEC) } }, // #204 [ref=2x]
  { 0, 0, { EXT(XSAVEOPT) } }, // #205 [ref=2x]
  { 0, 1, { EXT(TSX) } }  // #206 [ref=1x]
};
#undef EXT

#define FLAG(VAL) uint32_t(CpuRWFlags::kX86_##VAL)
const InstDB::RWFlagsInfoTable InstDB::_rwFlagsInfoTable[] = {
  { 0, 0 }, // #0 [ref=1383x]
  { 0, FLAG(AF) | FLAG(CF) | FLAG(OF) | FLAG(PF) | FLAG(SF) | FLAG(ZF) }, // #1 [ref=104x]
  { FLAG(CF), FLAG(AF) | FLAG(CF) | FLAG(OF) | FLAG(PF) | FLAG(SF) | FLAG(ZF) }, // #2 [ref=2x]
  { FLAG(CF), FLAG(CF) }, // #3 [ref=2x]
  { FLAG(OF), FLAG(OF) }, // #4 [ref=1x]
  { 0, FLAG(ZF) }, // #5 [ref=7x]
  { 0, FLAG(AF) | FLAG(CF) | FLAG(OF) | FLAG(PF) | FLAG(SF) }, // #6 [ref=4x]
  { 0, FLAG(AC) }, // #7 [ref=2x]
  { 0, FLAG(CF) }, // #8 [ref=2x]
  { 0, FLAG(DF) }, // #9 [ref=2x]
  { 0, FLAG(IF) }, // #10 [ref=2x]
  { FLAG(CF) | FLAG(ZF), 0 }, // #11 [ref=12x]
  { FLAG(CF), 0 }, // #12 [ref=18x]
  { FLAG(ZF), 0 }, // #13 [ref=14x]
  { FLAG(OF) | FLAG(SF) | FLAG(ZF), 0 }, // #14 [ref=12x]
  { FLAG(OF) | FLAG(SF), 0 }, // #15 [ref=12x]
  { FLAG(OF), 0 }, // #16 [ref=7x]
  { FLAG(PF), 0 }, // #17 [ref=12x]
  { FLAG(SF), 0 }, // #18 [ref=6x]
  { FLAG(DF), FLAG(AF) | FLAG(CF) | FLAG(OF) | FLAG(PF) | FLAG(SF) | FLAG(ZF) }, // #19 [ref=2x]
  { 0, FLAG(AF) | FLAG(OF) | FLAG(PF) | FLAG(SF) | FLAG(ZF) }, // #20 [ref=5x]
  { 0, FLAG(C0) | FLAG(C1) | FLAG(C2) | FLAG(C3) }, // #21 [ref=83x]
  { FLAG(CF), FLAG(C0) | FLAG(C1) | FLAG(C2) | FLAG(C3) }, // #22 [ref=2x]
  { FLAG(CF) | FLAG(ZF), FLAG(C0) | FLAG(C1) | FLAG(C2) | FLAG(C3) }, // #23 [ref=2x]
  { FLAG(ZF), FLAG(C0) | FLAG(C1) | FLAG(C2) | FLAG(C3) }, // #24 [ref=2x]
  { FLAG(PF), FLAG(C0) | FLAG(C1) | FLAG(C2) | FLAG(C3) }, // #25 [ref=2x]
  { 0, FLAG(C1) | FLAG(CF) | FLAG(PF) | FLAG(ZF) }, // #26 [ref=4x]
  { FLAG(C0) | FLAG(C1) | FLAG(C2) | FLAG(C3), 0 }, // #27 [ref=2x]
  { FLAG(AF) | FLAG(CF) | FLAG(PF) | FLAG(SF) | FLAG(ZF), 0 }, // #28 [ref=1x]
  { FLAG(DF), 0 }, // #29 [ref=3x]
  { 0, FLAG(AF) | FLAG(CF) | FLAG(DF) | FLAG(IF) | FLAG(OF) | FLAG(PF) | FLAG(SF) | FLAG(ZF) }, // #30 [ref=3x]
  { FLAG(AF) | FLAG(CF) | FLAG(DF) | FLAG(IF) | FLAG(OF) | FLAG(PF) | FLAG(SF) | FLAG(ZF), 0 }, // #31 [ref=3x]
  { FLAG(CF) | FLAG(OF), FLAG(CF) | FLAG(OF) }, // #32 [ref=2x]
  { 0, FLAG(CF) | FLAG(OF) }, // #33 [ref=2x]
  { 0, FLAG(AF) | FLAG(CF) | FLAG(PF) | FLAG(SF) | FLAG(ZF) }  // #34 [ref=1x]
};
#undef FLAG

#define FLAG(VAL) uint32_t(InstRWFlags::k##VAL)
const InstRWFlags InstDB::_instFlagsTable[] = {
  InstRWFlags(FLAG(None)), // #0 [ref=1693x]
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
const InstNameIndex InstDB::instNameIndex = {{
  { Inst::kIdAaa          , Inst::kIdAxor          + 1 },
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
}, uint16_t(17)};

const char InstDB::_instNameStringTable[] =
  "vgf2p8affineinvqbvaeskeygenassistvbroadcastf32x464x264x4i32x2i32x4i32x8i64x2i64x"
  "4vpbroadcastmb2w2dvbcstnebf162p128i128vcvtne2ps2vcvtneebf16vcvtneobf16vfmaddsub1"
  "32ph213pd213ph213ps231pd231ph231psvfmsubadd132vpmultishiftvscatterpf0dqpdqps1dpd"
  "1dps1qpd1qpsvcvtneps2vextracvextractfvgatherpf0vp2intersecttcmmimfp16tcmmrlfp16s"
  "h2pssdph2psvfnmadd132213sd213sh213ss231sd231sh231ssvfnmsub132vinservinsertfvpshu"
  "fbitqvsha512rndprefetchitntawt1saveprevsssha256rndtileloaddtilereleavaesdeclvaes"
  "enclvcompressvcvttpd2uqqvcvttph2uvcvttps2uvcvttsd2uvcvttsh2uvcvttss2uvfixupimmvf"
  "madd132vfmsub132vmaskmovdqvpcompressvpconflictvphminposuvpmadd52hluqvpscatterqdv"
  "punpckhqlqdqvrndscalemsg1msg2clflushopcmpnbexcmpnlexcmpxchg16t0t2tilestorev4fnma"
  "ddssvcvtpd2uvcvtph2psudqvcvtps2phvcvtsd2uvcvtsh2uvcvtss2uvcvtudq2vcvtuqq2vcvtusi"
  "2vfcmaddcvfpclassvgetmanmulbvp4dpwssvpclmuvpcmpestrvpcmpistrvperm2fvpermil2vpgat"
  "hervpmacssdqvpmadcsswubswvpmaskmovpternlogbwwdlbwldqlwdvrsqrt1428pd28ps28sd28ssv"
  "shufvshuffvzeroupxsaveoptcmpbexcmplexcmpnbxcmpnlxcmpnoxcmpnpxcmpnsxcmpnzx8bfxrst"
  "orldtilecfmovdir64pvalidarmpadjurmpupdaserialisha1nexsha1rndssttilecftdpbf16tdpf"
  "p16v4fmadvaddsubvblendmvpdvcvtdq2uwvcvtqq2vcvtsi2vcvtuwvdbpsadvdpbf16vexpanvfcmu"
  "lccphcshvgetexpvmovdqau16u32u64vmovmskvmovntvmovshdvmovsldvpackssdwbvpackuswbvpb"
  "lendmdvpdpbssudsvpdpbusvpdpwssvpdpwus2pdvpermtvpexpanvphaddubwqdqhvpmovmskvpmovs"
  "xbvpmovusqwvpmovzxbvpmulhrvptestnmqvreducevscalefvsm3rndvsm4rndsvunpckhlpdlpsxre"
  "sldtrs64xsusldtrcldemoclrssbscmpbxcmplxcmpoxcmppxcmpsxcmpzxcvtpifcmovfxsavekorte"
  "stkshiftrbkunpckmonitorpfrcpipfrsqirtvrdfsbrdgsbsspseamcalsenduisetssbssysesysex"
  "vcvtwvfmulvldmxcsvmlaundupu8vmovhvmovlhvmpsadvmresumvpadduvpaligngtbgtdgtqgtw2b2"
  "qbdbqvphsubvplzcnb2md2mq2mw2mvpopcnvpshldvqvpshrdvwhwvpsubuvrangevrcp14vroundsdv"
  "sm4keyvstmxcsvucomiallwbnoinwrfsbwrgsbc64blcfiblsficmovnendbrenqcmnufdecsfincsfn"
  "stefrndfsincfucomfyl2xincsspqinvlinvlpinvpcinvvpmcommmovq2pavgupfcmpepfpnaptwris"
  "eamoseamrsyscsysretdpbutlbsyvaesivaligvandnvcomivfrczvhadvhsubvmclevmgexvmmcvmov"
  "avmovuvmptvmwrivpandvpextrwvpinsvpmaxvpminvprolvprorvpsadvpsigvpslvpsllvpsravpsr"
  "lvsqrvtes";


const uint32_t InstDB::_instNameIndexTable[] = {
  0x80000000, // Small ''.
  0x80000421, // Small 'aaa'.
  0x80001021, // Small 'aad'.
  0x80021021, // Small 'aadd'.
  0x80003421, // Small 'aam'.
  0x80023821, // Small 'aand'.
  0x80004C21, // Small 'aas'.
  0x80000C81, // Small 'adc'.
  0x800C0C81, // Small 'adcx'.
  0x80001081, // Small 'add'.
  0x80481081, // Small 'addpd'.
  0x81381081, // Small 'addps'.
  0x80499081, // Small 'addsd'.
  0x81399081, // Small 'addss'.
  0x20A76099, // Large 'addsub|pd'.
  0x207D6099, // Large 'addsub|ps'.
  0x800C3C81, // Small 'adox'.
  0x86524CA1, // Small 'aesdec'.
  0x302871D5, // Large 'aesdecl|ast'.
  0x86E2CCA1, // Small 'aesenc'.
  0x302871DD, // Large 'aesencl|ast'.
  0x86D4CCA1, // Small 'aesimc'.
  0x0000F012, // Large 'aeskeygenassist'.
  0x800011C1, // Small 'and'.
  0x800711C1, // Small 'andn'.
  0x890711C1, // Small 'andnpd'.
  0xA70711C1, // Small 'andnps'.
  0x804811C1, // Small 'andpd'.
  0x813811C1, // Small 'andps'.
  0x800049E1, // Small 'aor'.
  0x80064241, // Small 'arpl'.
  0x80093F01, // Small 'axor'.
  0x812A60A2, // Small 'bextr'.
  0x26F45709, // Large 'blcfi|ll'.
  0x80048D82, // Small 'blci'.
  0x80348D82, // Small 'blcic'.
  0x97368D82, // Small 'blcmsk'.
  0x80098D82, // Small 'blcs'.
  0x20A75471, // Large 'blend|pd'.
  0x207D5471, // Large 'blend|ps'.
  0x34775471, // Large 'blend|vpd'.
  0x318B5471, // Large 'blend|vps'.
  0x26F4570E, // Large 'blsfi|ll'.
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
  0x20DF65B0, // Large 'cldemo|te'.
  0x0000729D, // Large 'clflush'.
  0x1020929D, // Large 'clflushop|t'.
  0x80049D83, // Small 'clgi'.
  0x80002583, // Small 'cli'.
  0x101775B6, // Large 'clrssbs|y'.
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
  0x20125713, // Large 'cmovn|ae'.
  0x84EB3DA3, // Small 'cmovnb'.
  0x22AA5713, // Large 'cmovn|be'.
  0x86EB3DA3, // Small 'cmovnc'.
  0x8AEB3DA3, // Small 'cmovne'.
  0x8EEB3DA3, // Small 'cmovng'.
  0x20185713, // Large 'cmovn|ge'.
  0x98EB3DA3, // Small 'cmovnl'.
  0x21C45713, // Large 'cmovn|le'.
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
  0x309963D9, // Large 'cmpbex|add'.
  0x309955BD, // Large 'cmpbx|add'.
  0x309963DF, // Large 'cmplex|add'.
  0x309955C2, // Large 'cmplx|add'.
  0x309972A6, // Large 'cmpnbex|add'.
  0x309963E5, // Large 'cmpnbx|add'.
  0x309972AD, // Large 'cmpnlex|add'.
  0x309963EB, // Large 'cmpnlx|add'.
  0x309963F1, // Large 'cmpnox|add'.
  0x309963F7, // Large 'cmpnpx|add'.
  0x309963FD, // Large 'cmpnsx|add'.
  0x30996403, // Large 'cmpnzx|add'.
  0x309955C7, // Large 'cmpox|add'.
  0x804841A3, // Small 'cmppd'.
  0x813841A3, // Small 'cmpps'.
  0x309955CC, // Large 'cmppx|add'.
  0x8009C1A3, // Small 'cmps'.
  0x8049C1A3, // Small 'cmpsd'.
  0x8139C1A3, // Small 'cmpss'.
  0x309955D1, // Large 'cmpsx|add'.
  0x000072B4, // Large 'cmpxchg'.
  0x101092B4, // Large 'cmpxchg16|b'.
  0x240972B4, // Large 'cmpxchg|8b'.
  0x309955D6, // Large 'cmpzx|add'.
  0x8934B5E3, // Small 'comisd'.
  0xA734B5E3, // Small 'comiss'.
  0x8044D603, // Small 'cpuid'.
  0x80003E23, // Small 'cqo'.
  0x81DF0E43, // Small 'crc32'.
  0x20A7647B, // Large 'cvtdq2|pd'.
  0x207D647B, // Large 'cvtdq2|ps'.
  0x20E562D5, // Large 'cvtpd2|dq'.
  0x222A62D5, // Large 'cvtpd2|pi'.
  0x207D62D5, // Large 'cvtpd2|ps'.
  0x352555DB, // Large 'cvtpi|2pd'.
  0x307C55DB, // Large 'cvtpi|2ps'.
  0x20E562E9, // Large 'cvtps2|dq'.
  0x102672E9, // Large 'cvtps2p|d'.
  0x100972E9, // Large 'cvtps2p|i'.
  0x201D62F2, // Large 'cvtsd2|si'.
  0x201C62F2, // Large 'cvtsd2|ss'.
  0x2144648B, // Large 'cvtsi2|sd'.
  0x201C648B, // Large 'cvtsi2|ss'.
  0x21446302, // Large 'cvtss2|sd'.
  0x201D6302, // Large 'cvtss2|si'.
  0x20E571EE, // Large 'cvttpd2|dq'.
  0x222A71EE, // Large 'cvttpd2|pi'.
  0x20E57202, // Large 'cvttps2|dq'.
  0x222A7202, // Large 'cvttps2|pi'.
  0x201D720B, // Large 'cvttsd2|si'.
  0x201D721D, // Large 'cvttss2|si'.
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
  0x202C5718, // Large 'endbr|32'.
  0x20305718, // Large 'endbr|64'.
  0x88D1C5C5, // Small 'enqcmd'.
  0x209B571D, // Large 'enqcm|ds'.
  0x8122D1C5, // Small 'enter'.
  0x207D710D, // Large 'extract|ps'.
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
  0x22AA55E0, // Large 'fcmov|be'.
  0x8B67B466, // Small 'fcmove'.
  0x22A955E0, // Large 'fcmov|nb'.
  0x32A955E0, // Large 'fcmov|nbe'.
  0x200A55E0, // Large 'fcmov|ne'.
  0x272255E0, // Large 'fcmov|nu'.
  0xAB67B466, // Small 'fcmovu'.
  0x8006BC66, // Small 'fcom'.
  0x8096BC66, // Small 'fcomi'.
  0xA096BC66, // Small 'fcomip'.
  0x8106BC66, // Small 'fcomp'.
  0xA106BC66, // Small 'fcompp'.
  0x8009BC66, // Small 'fcos'.
  0x21F15724, // Large 'fdecs|tp'.
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
  0x21F15729, // Large 'fincs|tp'.
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
  0x200D572E, // Large 'fnste|nv'.
  0xAF3A4DC6, // Small 'fnstsw'.
  0x9C1A0606, // Small 'fpatan'.
  0x80D2CA06, // Small 'fprem'.
  0xB8D2CA06, // Small 'fprem1'.
  0x80E0D206, // Small 'fptan'.
  0x31224733, // Large 'frnd|int'.
  0xA4FA4E46, // Small 'frstor'.
  0x805B0666, // Small 'fsave'.
  0x8AC08E66, // Small 'fscale'.
  0x80072666, // Small 'fsin'.
  0x22655737, // Large 'fsinc|os'.
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
  0x260C573C, // Large 'fucom|ip'.
  0xA0D78EA6, // Small 'fucomp'.
  0x25CE573C, // Large 'fucom|pp'.
  0x814486E6, // Small 'fwait'.
  0x80068706, // Small 'fxam'.
  0x80040F06, // Small 'fxch'.
  0x0000740B, // Large 'fxrstor'.
  0x2030740B, // Large 'fxrstor|64'.
  0x8B60CF06, // Small 'fxsave'.
  0x203065E5, // Large 'fxsave|64'.
  0x510F240B, // Large 'fx|tract'.
  0x818EB326, // Small 'fyl2x'.
  0x206E5741, // Large 'fyl2x|p1'.
  0x8659D0A7, // Small 'getsec'.
  0x1010F001, // Large 'gf2p8affineinvq|b'.
  0x200FB001, // Large 'gf2p8affine|qb'.
  0x43385001, // Large 'gf2p8|mulb'.
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
  0x20A75746, // Large 'incss|pd'.
  0x274B5746, // Large 'incss|pq'.
  0x80004DC9, // Small 'ins'.
  0x207D6184, // Large 'insert|ps'.
  0x100F6184, // Large 'insert|q'.
  0x800051C9, // Small 'int'.
  0x800F51C9, // Small 'int3'.
  0x8007D1C9, // Small 'into'.
  0x800259C9, // Small 'invd'.
  0xA902D9C9, // Small 'invept'.
  0x8F0659C9, // Small 'invlpg'.
  0x336C474D, // Large 'invl|pga'.
  0x23995751, // Large 'invlp|gb'.
  0x24265756, // Large 'invpc|id'.
  0x2426575B, // Large 'invvp|id'.
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
  0x101075EB, // Large 'kortest|b'.
  0x102675EB, // Large 'kortest|d'.
  0x100F75EB, // Large 'kortest|q'.
  0x105F75EB, // Large 'kortest|w'.
  0x800BC9EB, // Small 'korw'.
  0x233A65F2, // Large 'kshift|lb'.
  0x23A165F2, // Large 'kshift|ld'.
  0x228865F2, // Large 'kshift|lq'.
  0x23A465F2, // Large 'kshift|lw'.
  0x25F865F2, // Large 'kshift|rb'.
  0x102675F2, // Large 'kshiftr|d'.
  0x100F75F2, // Large 'kshiftr|q'.
  0x105F75F2, // Large 'kshiftr|w'.
  0x8549968B, // Small 'ktestb'.
  0x8949968B, // Small 'ktestd'.
  0xA349968B, // Small 'ktestq'.
  0xAF49968B, // Small 'ktestw'.
  0x239A65FA, // Large 'kunpck|bw'.
  0x20E565FA, // Large 'kunpck|dq'.
  0x239C65FA, // Large 'kunpck|wd'.
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
  0x1023664B, // Large 'ldmxcs|r'.
  0x80004C8C, // Small 'lds'.
  0x10018412, // Large 'ldtilecf|g'.
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
  0x109D9241, // Large 'maskmovdq|u'.
  0x100F7241, // Large 'maskmov|q'.
  0x8048602D, // Small 'maxpd'.
  0x8138602D, // Small 'maxps'.
  0x8049E02D, // Small 'maxsd'.
  0x8139E02D, // Small 'maxss'.
  0x21925760, // Large 'mcomm|it'.
  0x8A3714CD, // Small 'mfence'.
  0x8048392D, // Small 'minpd'.
  0x8138392D, // Small 'minps'.
  0x8049B92D, // Small 'minsd'.
  0x8139B92D, // Small 'minss'.
  0x00007600, // Large 'monitor'.
  0x102E7600, // Large 'monitor|x'.
  0x800059ED, // Small 'mov'.
  0xA620D9ED, // Small 'movabs'.
  0x8900D9ED, // Small 'movapd'.
  0xA700D9ED, // Small 'movaps'.
  0x805159ED, // Small 'movbe'.
  0x800259ED, // Small 'movd'.
  0x36574245, // Large 'movd|dup'.
  0x1010841A, // Large 'movdir64|b'.
  0x1009641A, // Large 'movdir|i'.
  0x268F5245, // Large 'movdq|2q'.
  0x831259ED, // Small 'movdqa'.
  0xAB1259ED, // Small 'movdqu'.
  0x359A465D, // Large 'movh|lps'.
  0x890459ED, // Small 'movhpd'.
  0xA70459ED, // Small 'movhps'.
  0x207D5662, // Large 'movlh|ps'.
  0x890659ED, // Small 'movlpd'.
  0xA70659ED, // Small 'movlps'.
  0x20A764D0, // Large 'movmsk|pd'.
  0x207D64D0, // Large 'movmsk|ps'.
  0x20E554D7, // Large 'movnt|dq'.
  0x34C354D7, // Large 'movnt|dqa'.
  0x934759ED, // Small 'movnti'.
  0x20A754D7, // Large 'movnt|pd'.
  0x207D54D7, // Large 'movnt|ps'.
  0xA34759ED, // Small 'movntq'.
  0x214454D7, // Large 'movnt|sd'.
  0x201C54D7, // Large 'movnt|ss'.
  0x8008D9ED, // Small 'movq'.
  0x20E55765, // Large 'movq2|dq'.
  0x8009D9ED, // Small 'movs'.
  0x8049D9ED, // Small 'movsd'.
  0x222964DD, // Large 'movshd|up'.
  0x222964E4, // Large 'movsld|up'.
  0x8139D9ED, // Small 'movss'.
  0x8189D9ED, // Small 'movsx'.
  0x8989D9ED, // Small 'movsxd'.
  0x890AD9ED, // Small 'movupd'.
  0xA70AD9ED, // Small 'movups'.
  0x818D59ED, // Small 'movzx'.
  0x239A5668, // Large 'mpsad|bw'.
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
  0x000084EB, // Large 'packssdw'.
  0x24F264EB, // Large 'packss|wb'.
  0x24F164F5, // Large 'packus|dw'.
  0x000084F5, // Large 'packuswb'.
  0x80221030, // Small 'paddb'.
  0x80421030, // Small 'paddd'.
  0x81121030, // Small 'paddq'.
  0x85321030, // Small 'paddsb'.
  0xAF321030, // Small 'paddsw'.
  0x25BA5675, // Large 'paddu|sb'.
  0x23835675, // Large 'paddu|sw'.
  0x81721030, // Small 'paddw'.
  0x1023667B, // Large 'palign|r'.
  0x80023830, // Small 'pand'.
  0x80E23830, // Small 'pandn'.
  0x8059D430, // Small 'pause'.
  0x8023D830, // Small 'pavgb'.
  0x25BA576A, // Large 'pavgu|sb'.
  0x8173D830, // Small 'pavgw'.
  0x202164FE, // Large 'pblend|vb'.
  0x105F64FE, // Large 'pblend|w'.
  0x42885345, // Large 'pclmu|lqdq'.
  0x200F534B, // Large 'pcmpe|qb'.
  0x227D534B, // Large 'pcmpe|qd'.
  0x21F6534B, // Large 'pcmpe|qq'.
  0x2559534B, // Large 'pcmpe|qw'.
  0x1009834B, // Large 'pcmpestr|i'.
  0x105C834B, // Large 'pcmpestr|m'.
  0x368142A5, // Large 'pcmp|gtb'.
  0x368442A5, // Large 'pcmp|gtd'.
  0x368742A5, // Large 'pcmp|gtq'.
  0x368A42A5, // Large 'pcmp|gtw'.
  0x10098354, // Large 'pcmpistr|i'.
  0x105C8354, // Large 'pcmpistr|m'.
  0x267E5255, // Large 'pconf|ig'.
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
  0x100F676F, // Large 'pfcmpe|q'.
  0x2018576F, // Large 'pfcmp|ge'.
  0x2681576F, // Large 'pfcmp|gt'.
  0x8180B4D0, // Small 'pfmax'.
  0x80E4B4D0, // Small 'pfmin'.
  0x80CAB4D0, // Small 'pfmul'.
  0x8630B8D0, // Small 'pfnacc'.
  0x24B15775, // Large 'pfpna|cc'.
  0x8101C8D0, // Small 'pfrcp'.
  0x21AD6607, // Large 'pfrcpi|t1'.
  0x22BF6607, // Large 'pfrcpi|t2'.
  0xAD01C8D0, // Small 'pfrcpv'.
  0x21AD660D, // Large 'pfrsqi|t1'.
  0x2188560D, // Large 'pfrsq|rt'.
  0x3613560D, // Large 'pfrsq|rtv'.
  0x802ACCD0, // Small 'pfsub'.
  0xA42ACCD0, // Small 'pfsubr'.
  0x88420510, // Small 'phaddd'.
  0x23835536, // Large 'phadd|sw'.
  0xAE420510, // Small 'phaddw'.
  0x105F925F, // Large 'phminposu|w'.
  0x882ACD10, // Small 'phsubd'.
  0x23835696, // Large 'phsub|sw'.
  0xAE2ACD10, // Small 'phsubw'.
  0x80437530, // Small 'pi2fd'.
  0x81737530, // Small 'pi2fw'.
  0x8529B930, // Small 'pinsrb'.
  0x8929B930, // Small 'pinsrd'.
  0xA329B930, // Small 'pinsrq'.
  0xAF29B930, // Small 'pinsrw'.
  0x43855269, // Large 'pmadd|ubsw'.
  0x239C5269, // Large 'pmadd|wd'.
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
  0x10107543, // Large 'pmovmsk|b'.
  0x1026754B, // Large 'pmovsxb|d'.
  0x100F754B, // Large 'pmovsxb|q'.
  0x105F754B, // Large 'pmovsxb|w'.
  0x20E5654B, // Large 'pmovsx|dq'.
  0x239C654B, // Large 'pmovsx|wd'.
  0x253D654B, // Large 'pmovsx|wq'.
  0x1026755C, // Large 'pmovzxb|d'.
  0x100F755C, // Large 'pmovzxb|q'.
  0x105F755C, // Large 'pmovzxb|w'.
  0x20E5655C, // Large 'pmovzx|dq'.
  0x239C655C, // Large 'pmovzx|wd'.
  0x253D655C, // Large 'pmovzx|wq'.
  0xA24655B0, // Small 'pmuldq'.
  0x23836564, // Large 'pmulhr|sw'.
  0x105F6564, // Large 'pmulhr|w'.
  0x24815564, // Large 'pmulh|uw'.
  0xAE8655B0, // Small 'pmulhw'.
  0x88C655B0, // Small 'pmulld'.
  0xAEC655B0, // Small 'pmullw'.
  0x32E540CF, // Large 'pmul|udq'.
  0x800041F0, // Small 'pop'.
  0x8000C1F0, // Small 'popa'.
  0x8040C1F0, // Small 'popad'.
  0xA8E1C1F0, // Small 'popcnt'.
  0x800341F0, // Small 'popf'.
  0x804341F0, // Small 'popfd'.
  0x811341F0, // Small 'popfq'.
  0x800049F0, // Small 'por'.
  0x0000819F, // Large 'prefetch'.
  0x10E4A19F, // Large 'prefetchit|0'.
  0x106BA19F, // Large 'prefetchit|1'.
  0x31A9819F, // Large 'prefetch|nta'.
  0x22BD819F, // Large 'prefetch|t0'.
  0x21AD819F, // Large 'prefetch|t1'.
  0x22BF819F, // Large 'prefetch|t2'.
  0x105F819F, // Large 'prefetch|w'.
  0x31AC819F, // Large 'prefetch|wt1'.
  0xAE220670, // Small 'psadbw'.
  0x846AA270, // Small 'pshufb'.
  0x886AA270, // Small 'pshufd'.
  0x26C3518C, // Large 'pshuf|hw'.
  0x23A4518C, // Large 'pshuf|lw'.
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
  0x25BA56C6, // Large 'psubu|sb'.
  0x238356C6, // Large 'psubu|sw'.
  0x81715670, // Small 'psubw'.
  0x8900DE70, // Small 'pswapd'.
  0x81499690, // Small 'ptest'.
  0x20DF577A, // Large 'ptwri|te'.
  0x239A7280, // Large 'punpckh|bw'.
  0x20E57280, // Large 'punpckh|dq'.
  0x20E58280, // Large 'punpckhq|dq'.
  0x239C7280, // Large 'punpckh|wd'.
  0x339E6280, // Large 'punpck|lbw'.
  0x33A16280, // Large 'punpck|ldq'.
  0x42886280, // Large 'punpck|lqdq'.
  0x33A46280, // Large 'punpck|lwd'.
  0x80044EB0, // Small 'push'.
  0x80144EB0, // Small 'pusha'.
  0x88144EB0, // Small 'pushad'.
  0x80644EB0, // Small 'pushf'.
  0x88644EB0, // Small 'pushfd'.
  0xA2644EB0, // Small 'pushfq'.
  0x20DF7422, // Large 'pvalida|te'.
  0x80093F10, // Small 'pxor'.
  0x80003072, // Small 'rcl'.
  0x81384072, // Small 'rcpps'.
  0x8139C072, // Small 'rcpss'.
  0x80004872, // Small 'rcr'.
  0x34365616, // Large 'rdfsb|ase'.
  0x3436561B, // Large 'rdgsb|ase'.
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
  0x201F7429, // Large 'rmpadju|st'.
  0x20DF7430, // Large 'rmpupda|te'.
  0x800031F2, // Small 'rol'.
  0x800049F2, // Small 'ror'.
  0x800C49F2, // Small 'rorx'.
  0x20A756D8, // Large 'round|pd'.
  0x207D56D8, // Large 'round|ps'.
  0x000076D8, // Large 'roundsd'.
  0x101466D8, // Large 'rounds|s'.
  0x80003672, // Small 'rsm'.
  0x207D53A8, // Large 'rsqrt|ps'.
  0x201C53A8, // Large 'rsqrt|ss'.
  0x3620540D, // Large 'rstor|ssp'.
  0x80032033, // Small 'sahf'.
  0x80003033, // Small 'sal'.
  0x80004833, // Small 'sar'.
  0x800C4833, // Small 'sarx'.
  0x1004A1AF, // Large 'saveprevss|p'.
  0x80000853, // Small 'sbb'.
  0x80098473, // Small 'scas'.
  0x10D27623, // Large 'seamcal|l'.
  0x207D577F, // Large 'seamo|ps'.
  0x21A35784, // Large 'seamr|et'.
  0x222A662A, // Large 'sendui|pi'.
  0x23CB7437, // Large 'seriali|ze'.
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
  0x10177630, // Large 'setssbs|y'.
  0x800D50B3, // Small 'setz'.
  0x8A3714D3, // Small 'sfence'.
  0x800A10F3, // Small 'sgdt'.
  0x4295443E, // Large 'sha1|msg1'.
  0x4299443E, // Large 'sha1|msg2'.
  0x20DF743E, // Large 'sha1nex|te'.
  0x102F8445, // Large 'sha1rnds|4'.
  0x429561B9, // Large 'sha256|msg1'.
  0x429961B9, // Large 'sha256|msg2'.
  0x207E91B9, // Large 'sha256rnd|s2'.
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
  0x102366E7, // Large 'stmxcs|r'.
  0x8009BE93, // Small 'stos'.
  0x80004A93, // Small 'str'.
  0x1001844D, // Large 'sttilecf|g'.
  0x8004D693, // Small 'stui'.
  0x80000AB3, // Small 'sub'.
  0x80480AB3, // Small 'subpd'.
  0x81380AB3, // Small 'subps'.
  0x80498AB3, // Small 'subsd'.
  0x81398AB3, // Small 'subss'.
  0xA67806F3, // Small 'swapgs'.
  0x36F34789, // Large 'sysc|all'.
  0x41234637, // Large 'syse|nter'.
  0x2192563B, // Large 'sysex|it'.
  0x3192563B, // Large 'sysex|itq'.
  0xA8594F33, // Small 'sysret'.
  0x2193578D, // Large 'sysre|tq'.
  0x86B9B794, // Small 't1mskc'.
  0x207DA12B, // Large 'tcmmimfp16|ps'.
  0x207DA135, // Large 'tcmmrlfp16|ps'.
  0x98C08C94, // Small 'tdcall'.
  0x207D7455, // Large 'tdpbf16|ps'.
  0x31434455, // Large 'tdpb|ssd'.
  0x32E44455, // Large 'tdpb|sud'.
  0x21445792, // Large 'tdpbu|sd'.
  0x22E55792, // Large 'tdpbu|ud'.
  0x207D745C, // Large 'tdpfp16|ps'.
  0x800A4CB4, // Small 'test'.
  0x935A4CB4, // Small 'testui'.
  0x000091C2, // Large 'tileloadd'.
  0x21AD91C2, // Large 'tileloadd|t1'.
  0x212791CB, // Large 'tilerelea|se'.
  0x102692C1, // Large 'tilestore|d'.
  0x43CB41C2, // Large 'tile|zero'.
  0x21E15797, // Large 'tlbsy|nc'.
  0x8B3A8614, // Small 'tpause'.
  0x81470F54, // Small 'tzcnt'.
  0x80B9B754, // Small 'tzmsk'.
  0x214456EE, // Large 'ucomi|sd'.
  0x201C56EE, // Large 'ucomi|ss'.
  0x80006C95, // Small 'ud0'.
  0x80007095, // Small 'ud1'.
  0x80007495, // Small 'ud2'.
  0x8142C935, // Small 'uiret'.
  0x7600109D, // Large 'u|monitor'.
  0xA890DDB5, // Small 'umwait'.
  0x20A76281, // Large 'unpckh|pd'.
  0x207D6281, // Large 'unpckh|ps'.
  0x35975281, // Large 'unpck|lpd'.
  0x359A5281, // Large 'unpck|lps'.
  0x30F16463, // Large 'v4fmad|dps'.
  0x32D16463, // Large 'v4fmad|dss'.
  0x30F172CA, // Large 'v4fnmad|dps'.
  0x32D172CA, // Large 'v4fnmad|dss'.
  0x89021036, // Small 'vaddpd'.
  0x91021036, // Small 'vaddph'.
  0xA7021036, // Small 'vaddps'.
  0x89321036, // Small 'vaddsd'.
  0x91321036, // Small 'vaddsh'.
  0xA7321036, // Small 'vaddss'.
  0x20A77469, // Large 'vaddsub|pd'.
  0x207D7469, // Large 'vaddsub|ps'.
  0x000071D4, // Large 'vaesdec'.
  0x302881D4, // Large 'vaesdecl|ast'.
  0x000071DC, // Large 'vaesenc'.
  0x302881DC, // Large 'vaesencl|ast'.
  0x2626579C, // Large 'vaesi|mc'.
  0x1020F011, // Large 'vaeskeygenassis|t'.
  0x219D57A1, // Large 'valig|nd'.
  0x271E57A1, // Large 'valig|nq'.
  0x20A757A6, // Large 'vandn|pd'.
  0x207D57A6, // Large 'vandn|ps'.
  0x89023836, // Small 'vandpd'.
  0xA7023836, // Small 'vandps'.
  0x1014D062, // Large 'vbcstnebf162p|s'.
  0x513F7062, // Large 'vbcstne|sh2ps'.
  0x20A77470, // Large 'vblendm|pd'.
  0x207D7470, // Large 'vblendm|ps'.
  0x20A76470, // Large 'vblend|pd'.
  0x207D6470, // Large 'vblend|ps'.
  0x34776470, // Large 'vblend|vpd'.
  0x318B6470, // Large 'vblend|vps'.
  0x306FB021, // Large 'vbroadcastf|128'.
  0x1003E021, // Large 'vbroadcastf32x|2'.
  0x102FE021, // Large 'vbroadcastf32x|4'.
  0x1005E021, // Large 'vbroadcastf32x|8'.
  0x4030B021, // Large 'vbroadcastf|64x2'.
  0x4034B021, // Large 'vbroadcastf|64x4'.
  0x4072A021, // Large 'vbroadcast|i128'.
  0x5038A021, // Large 'vbroadcast|i32x2'.
  0x503DA021, // Large 'vbroadcast|i32x4'.
  0x5042A021, // Large 'vbroadcast|i32x8'.
  0x5047A021, // Large 'vbroadcast|i64x2'.
  0x504CA021, // Large 'vbroadcast|i64x4'.
  0x2144A021, // Large 'vbroadcast|sd'.
  0x201CA021, // Large 'vbroadcast|ss'.
  0x89083476, // Small 'vcmppd'.
  0x91083476, // Small 'vcmpph'.
  0xA7083476, // Small 'vcmpps'.
  0x89383476, // Small 'vcmpsd'.
  0x91383476, // Small 'vcmpsh'.
  0xA7383476, // Small 'vcmpss'.
  0x214457AB, // Large 'vcomi|sd'.
  0x20D557AB, // Large 'vcomi|sh'.
  0x201C57AB, // Large 'vcomi|ss'.
  0x20A791E4, // Large 'vcompress|pd'.
  0x207D91E4, // Large 'vcompress|ps'.
  0x20A7747A, // Large 'vcvtdq2|pd'.
  0x20A2747A, // Large 'vcvtdq2|ph'.
  0x207D747A, // Large 'vcvtdq2|ps'.
  0x4069A076, // Large 'vcvtne2ps2|bf16'.
  0x307CB080, // Large 'vcvtneebf16|2ps'.
  0x51467080, // Large 'vcvtnee|ph2ps'.
  0x307CB08B, // Large 'vcvtneobf16|2ps'.
  0x5146708B, // Large 'vcvtneo|ph2ps'.
  0x406990FC, // Large 'vcvtneps2|bf16'.
  0x20E572D4, // Large 'vcvtpd2|dq'.
  0x20A272D4, // Large 'vcvtpd2|ph'.
  0x207D72D4, // Large 'vcvtpd2|ps'.
  0x21F672D4, // Large 'vcvtpd2|qq'.
  0x20E582D4, // Large 'vcvtpd2u|dq'.
  0x21F682D4, // Large 'vcvtpd2u|qq'.
  0x20E572DC, // Large 'vcvtph2|dq'.
  0x102682DC, // Large 'vcvtph2p|d'.
  0x000092DC, // Large 'vcvtph2ps'.
  0x102E92DC, // Large 'vcvtph2ps|x'.
  0x21F672DC, // Large 'vcvtph2|qq'.
  0x32E572DC, // Large 'vcvtph2|udq'.
  0x31F572DC, // Large 'vcvtph2|uqq'.
  0x248172DC, // Large 'vcvtph2|uw'.
  0x105F72DC, // Large 'vcvtph2|w'.
  0x20E572E8, // Large 'vcvtps2|dq'.
  0x102682E8, // Large 'vcvtps2p|d'.
  0x000092E8, // Large 'vcvtps2ph'.
  0x102E92E8, // Large 'vcvtps2ph|x'.
  0x21F672E8, // Large 'vcvtps2|qq'.
  0x32E572E8, // Large 'vcvtps2|udq'.
  0x31F572E8, // Large 'vcvtps2|uqq'.
  0x20A77483, // Large 'vcvtqq2|pd'.
  0x20A27483, // Large 'vcvtqq2|ph'.
  0x207D7483, // Large 'vcvtqq2|ps'.
  0x20D572F1, // Large 'vcvtsd2|sh'.
  0x201D72F1, // Large 'vcvtsd2|si'.
  0x201C72F1, // Large 'vcvtsd2|ss'.
  0x201D82F1, // Large 'vcvtsd2u|si'.
  0x214472F9, // Large 'vcvtsh2|sd'.
  0x201D72F9, // Large 'vcvtsh2|si'.
  0x201C72F9, // Large 'vcvtsh2|ss'.
  0x201D82F9, // Large 'vcvtsh2u|si'.
  0x2144748A, // Large 'vcvtsi2|sd'.
  0x20D5748A, // Large 'vcvtsi2|sh'.
  0x201C748A, // Large 'vcvtsi2|ss'.
  0x21447301, // Large 'vcvtss2|sd'.
  0x20D57301, // Large 'vcvtss2|sh'.
  0x201D7301, // Large 'vcvtss2|si'.
  0x201D8301, // Large 'vcvtss2u|si'.
  0x20E581ED, // Large 'vcvttpd2|dq'.
  0x21F681ED, // Large 'vcvttpd2|qq'.
  0x20E591ED, // Large 'vcvttpd2u|dq'.
  0x21F691ED, // Large 'vcvttpd2u|qq'.
  0x20E581F8, // Large 'vcvttph2|dq'.
  0x21F681F8, // Large 'vcvttph2|qq'.
  0x20E591F8, // Large 'vcvttph2u|dq'.
  0x21F691F8, // Large 'vcvttph2u|qq'.
  0x105F91F8, // Large 'vcvttph2u|w'.
  0x105F81F8, // Large 'vcvttph2|w'.
  0x20E58201, // Large 'vcvttps2|dq'.
  0x21F68201, // Large 'vcvttps2|qq'.
  0x20E59201, // Large 'vcvttps2u|dq'.
  0x21F69201, // Large 'vcvttps2u|qq'.
  0x201D820A, // Large 'vcvttsd2|si'.
  0x201D920A, // Large 'vcvttsd2u|si'.
  0x201D8213, // Large 'vcvttsh2|si'.
  0x201D9213, // Large 'vcvttsh2u|si'.
  0x201D821C, // Large 'vcvttss2|si'.
  0x201D921C, // Large 'vcvttss2u|si'.
  0x20A78309, // Large 'vcvtudq2|pd'.
  0x20A28309, // Large 'vcvtudq2|ph'.
  0x207D8309, // Large 'vcvtudq2|ps'.
  0x20A78311, // Large 'vcvtuqq2|pd'.
  0x20A28311, // Large 'vcvtuqq2|ph'.
  0x207D8311, // Large 'vcvtuqq2|ps'.
  0x21448319, // Large 'vcvtusi2|sd'.
  0x20D58319, // Large 'vcvtusi2|sh'.
  0x201C8319, // Large 'vcvtusi2|ss'.
  0x30A16491, // Large 'vcvtuw|2ph'.
  0x30A15640, // Large 'vcvtw|2ph'.
  0x239A7497, // Large 'vdbpsad|bw'.
  0x890B2496, // Small 'vdivpd'.
  0x910B2496, // Small 'vdivph'.
  0xA70B2496, // Small 'vdivps'.
  0x893B2496, // Small 'vdivsd'.
  0x913B2496, // Small 'vdivsh'.
  0xA73B2496, // Small 'vdivss'.
  0x207D749E, // Large 'vdpbf16|ps'.
  0x80484096, // Small 'vdppd'.
  0x81384096, // Small 'vdpps'.
  0x800948B6, // Small 'verr'.
  0x800BC8B6, // Small 'verw'.
  0x352544A5, // Large 'vexp|2pd'.
  0x307C44A5, // Large 'vexp|2ps'.
  0x30ED64A5, // Large 'vexpan|dpd'.
  0x30F164A5, // Large 'vexpan|dps'.
  0x306F910C, // Large 'vextractf|128'.
  0x602A7105, // Large 'vextrac|tf32x4'.
  0x4043910C, // Large 'vextractf|32x8'.
  0x4030910C, // Large 'vextractf|64x2'.
  0x4034910C, // Large 'vextractf|64x4'.
  0x4072810C, // Large 'vextract|i128'.
  0x503D810C, // Large 'vextract|i32x4'.
  0x5042810C, // Large 'vextract|i32x8'.
  0x5047810C, // Large 'vextract|i64x2'.
  0x504C810C, // Large 'vextract|i64x4'.
  0x207D810C, // Large 'vextract|ps'.
  0x20A28321, // Large 'vfcmaddc|ph'.
  0x20D58321, // Large 'vfcmaddc|sh'.
  0x20A274AB, // Large 'vfcmulc|ph'.
  0x20D574AB, // Large 'vfcmulc|sh'.
  0x20A79225, // Large 'vfixupimm|pd'.
  0x207D9225, // Large 'vfixupimm|ps'.
  0x21449225, // Large 'vfixupimm|sd'.
  0x201C9225, // Large 'vfixupimm|ss'.
  0x20A7922E, // Large 'vfmadd132|pd'.
  0x20A2922E, // Large 'vfmadd132|ph'.
  0x207D922E, // Large 'vfmadd132|ps'.
  0x2144922E, // Large 'vfmadd132|sd'.
  0x20D5922E, // Large 'vfmadd132|sh'.
  0x201C922E, // Large 'vfmadd132|ss'.
  0x50A46096, // Large 'vfmadd|213pd'.
  0x50A96096, // Large 'vfmadd|213ph'.
  0x50AE6096, // Large 'vfmadd|213ps'.
  0x51556096, // Large 'vfmadd|213sd'.
  0x515A6096, // Large 'vfmadd|213sh'.
  0x515F6096, // Large 'vfmadd|213ss'.
  0x50B36096, // Large 'vfmadd|231pd'.
  0x50B86096, // Large 'vfmadd|231ph'.
  0x50BD6096, // Large 'vfmadd|231ps'.
  0x51646096, // Large 'vfmadd|231sd'.
  0x51696096, // Large 'vfmadd|231sh'.
  0x516E6096, // Large 'vfmadd|231ss'.
  0x34B26096, // Large 'vfmadd|cph'.
  0x34B56096, // Large 'vfmadd|csh'.
  0x20A76096, // Large 'vfmadd|pd'.
  0x207D6096, // Large 'vfmadd|ps'.
  0x10267096, // Large 'vfmadds|d'.
  0x10147096, // Large 'vfmadds|s'.
  0x1026D096, // Large 'vfmaddsub132p|d'.
  0x10A3D096, // Large 'vfmaddsub132p|h'.
  0x1014D096, // Large 'vfmaddsub132p|s'.
  0x50A49096, // Large 'vfmaddsub|213pd'.
  0x50A99096, // Large 'vfmaddsub|213ph'.
  0x50AE9096, // Large 'vfmaddsub|213ps'.
  0x50B39096, // Large 'vfmaddsub|231pd'.
  0x50B89096, // Large 'vfmaddsub|231ph'.
  0x50BD9096, // Large 'vfmaddsub|231ps'.
  0x20A79096, // Large 'vfmaddsub|pd'.
  0x207D9096, // Large 'vfmaddsub|ps'.
  0x20A79237, // Large 'vfmsub132|pd'.
  0x20A29237, // Large 'vfmsub132|ph'.
  0x207D9237, // Large 'vfmsub132|ps'.
  0x21449237, // Large 'vfmsub132|sd'.
  0x20D59237, // Large 'vfmsub132|sh'.
  0x201C9237, // Large 'vfmsub132|ss'.
  0x50A460C2, // Large 'vfmsub|213pd'.
  0x50A960C2, // Large 'vfmsub|213ph'.
  0x50AE60C2, // Large 'vfmsub|213ps'.
  0x515560C2, // Large 'vfmsub|213sd'.
  0x515A60C2, // Large 'vfmsub|213sh'.
  0x515F60C2, // Large 'vfmsub|213ss'.
  0x50B360C2, // Large 'vfmsub|231pd'.
  0x50B860C2, // Large 'vfmsub|231ph'.
  0x50BD60C2, // Large 'vfmsub|231ps'.
  0x516460C2, // Large 'vfmsub|231sd'.
  0x516960C2, // Large 'vfmsub|231sh'.
  0x516E60C2, // Large 'vfmsub|231ss'.
  0x20A7C0C2, // Large 'vfmsubadd132|pd'.
  0x20A2C0C2, // Large 'vfmsubadd132|ph'.
  0x207DC0C2, // Large 'vfmsubadd132|ps'.
  0x50A490C2, // Large 'vfmsubadd|213pd'.
  0x50A990C2, // Large 'vfmsubadd|213ph'.
  0x50AE90C2, // Large 'vfmsubadd|213ps'.
  0x50B390C2, // Large 'vfmsubadd|231pd'.
  0x50B890C2, // Large 'vfmsubadd|231ph'.
  0x50BD90C2, // Large 'vfmsubadd|231ps'.
  0x20A790C2, // Large 'vfmsubadd|pd'.
  0x207D90C2, // Large 'vfmsubadd|ps'.
  0x20A760C2, // Large 'vfmsub|pd'.
  0x207D60C2, // Large 'vfmsub|ps'.
  0x214460C2, // Large 'vfmsub|sd'.
  0x201C60C2, // Large 'vfmsub|ss'.
  0x34B25645, // Large 'vfmul|cph'.
  0x34B55645, // Large 'vfmul|csh'.
  0x20A7A14B, // Large 'vfnmadd132|pd'.
  0x20A2A14B, // Large 'vfnmadd132|ph'.
  0x207DA14B, // Large 'vfnmadd132|ps'.
  0x2144A14B, // Large 'vfnmadd132|sd'.
  0x20D5A14B, // Large 'vfnmadd132|sh'.
  0x201CA14B, // Large 'vfnmadd132|ss'.
  0x50A4714B, // Large 'vfnmadd|213pd'.
  0x50A9714B, // Large 'vfnmadd|213ph'.
  0x50AE714B, // Large 'vfnmadd|213ps'.
  0x5155714B, // Large 'vfnmadd|213sd'.
  0x515A714B, // Large 'vfnmadd|213sh'.
  0x515F714B, // Large 'vfnmadd|213ss'.
  0x50B3714B, // Large 'vfnmadd|231pd'.
  0x50B8714B, // Large 'vfnmadd|231ph'.
  0x50BD714B, // Large 'vfnmadd|231ps'.
  0x5164714B, // Large 'vfnmadd|231sd'.
  0x5169714B, // Large 'vfnmadd|231sh'.
  0x516E714B, // Large 'vfnmadd|231ss'.
  0x20A7714B, // Large 'vfnmadd|pd'.
  0x207D714B, // Large 'vfnmadd|ps'.
  0x2144714B, // Large 'vfnmadd|sd'.
  0x201C714B, // Large 'vfnmadd|ss'.
  0x20A7A173, // Large 'vfnmsub132|pd'.
  0x20A2A173, // Large 'vfnmsub132|ph'.
  0x207DA173, // Large 'vfnmsub132|ps'.
  0x2144A173, // Large 'vfnmsub132|sd'.
  0x20D5A173, // Large 'vfnmsub132|sh'.
  0x201CA173, // Large 'vfnmsub132|ss'.
  0x50A47173, // Large 'vfnmsub|213pd'.
  0x50A97173, // Large 'vfnmsub|213ph'.
  0x50AE7173, // Large 'vfnmsub|213ps'.
  0x51557173, // Large 'vfnmsub|213sd'.
  0x515A7173, // Large 'vfnmsub|213sh'.
  0x515F7173, // Large 'vfnmsub|213ss'.
  0x50B37173, // Large 'vfnmsub|231pd'.
  0x50B87173, // Large 'vfnmsub|231ph'.
  0x50BD7173, // Large 'vfnmsub|231ps'.
  0x51647173, // Large 'vfnmsub|231sd'.
  0x51697173, // Large 'vfnmsub|231sh'.
  0x516E7173, // Large 'vfnmsub|231ss'.
  0x20A77173, // Large 'vfnmsub|pd'.
  0x207D7173, // Large 'vfnmsub|ps'.
  0x21447173, // Large 'vfnmsub|sd'.
  0x201C7173, // Large 'vfnmsub|ss'.
  0x20A78329, // Large 'vfpclass|pd'.
  0x20A28329, // Large 'vfpclass|ph'.
  0x207D8329, // Large 'vfpclass|ps'.
  0x21448329, // Large 'vfpclass|sd'.
  0x20D58329, // Large 'vfpclass|sh'.
  0x201C8329, // Large 'vfpclass|ss'.
  0x20A757B0, // Large 'vfrcz|pd'.
  0x207D57B0, // Large 'vfrcz|ps'.
  0x214457B0, // Large 'vfrcz|sd'.
  0x201C57B0, // Large 'vfrcz|ss'.
  0x30ED7115, // Large 'vgather|dpd'.
  0x30F17115, // Large 'vgather|dps'.
  0x30EDA115, // Large 'vgatherpf0|dpd'.
  0x30F1A115, // Large 'vgatherpf0|dps'.
  0x30E6A115, // Large 'vgatherpf0|qpd'.
  0x30E9A115, // Large 'vgatherpf0|qps'.
  0x40EC9115, // Large 'vgatherpf|1dpd'.
  0x40F09115, // Large 'vgatherpf|1dps'.
  0x40F49115, // Large 'vgatherpf|1qpd'.
  0x40F89115, // Large 'vgatherpf|1qps'.
  0x30E67115, // Large 'vgather|qpd'.
  0x30E97115, // Large 'vgather|qps'.
  0x20A774B8, // Large 'vgetexp|pd'.
  0x20A274B8, // Large 'vgetexp|ph'.
  0x207D74B8, // Large 'vgetexp|ps'.
  0x214474B8, // Large 'vgetexp|sd'.
  0x20D574B8, // Large 'vgetexp|sh'.
  0x201C74B8, // Large 'vgetexp|ss'.
  0x31F17331, // Large 'vgetman|tpd'.
  0x31FC7331, // Large 'vgetman|tph'.
  0x32057331, // Large 'vgetman|tps'.
  0x320E7331, // Large 'vgetman|tsd'.
  0x32177331, // Large 'vgetman|tsh'.
  0x32207331, // Large 'vgetman|tss'.
  0x200FF000, // Large 'vgf2p8affineinv|qb'.
  0x200FC000, // Large 'vgf2p8affine|qb'.
  0x43386000, // Large 'vgf2p8|mulb'.
  0x30ED47B5, // Large 'vhad|dpd'.
  0x30F147B5, // Large 'vhad|dps'.
  0x20A757B9, // Large 'vhsub|pd'.
  0x207D57B9, // Large 'vhsub|ps'.
  0x306F8183, // Large 'vinsertf|128'.
  0x602A617D, // Large 'vinser|tf32x4'.
  0x40438183, // Large 'vinsertf|32x8'.
  0x40308183, // Large 'vinsertf|64x2'.
  0x40348183, // Large 'vinsertf|64x4'.
  0x40727183, // Large 'vinsert|i128'.
  0x503D7183, // Large 'vinsert|i32x4'.
  0x50427183, // Large 'vinsert|i32x8'.
  0x50477183, // Large 'vinsert|i64x2'.
  0x504C7183, // Large 'vinsert|i64x4'.
  0x207D7183, // Large 'vinsert|ps'.
  0xAB121196, // Small 'vlddqu'.
  0x1023764A, // Large 'vldmxcs|r'.
  0x109DA240, // Large 'vmaskmovdq|u'.
  0x20A78240, // Large 'vmaskmov|pd'.
  0x207D8240, // Large 'vmaskmov|ps'.
  0x890C05B6, // Small 'vmaxpd'.
  0x910C05B6, // Small 'vmaxph'.
  0xA70C05B6, // Small 'vmaxps'.
  0x893C05B6, // Small 'vmaxsd'.
  0x913C05B6, // Small 'vmaxsh'.
  0xA73C05B6, // Small 'vmaxss'.
  0x98C08DB6, // Small 'vmcall'.
  0x242857BE, // Large 'vmcle|ar'.
  0x86EA99B6, // Small 'vmfunc'.
  0x219257C3, // Large 'vmgex|it'.
  0x890725B6, // Small 'vminpd'.
  0x910725B6, // Small 'vminph'.
  0xA70725B6, // Small 'vminps'.
  0x893725B6, // Small 'vminsd'.
  0x913725B6, // Small 'vminsh'.
  0xA73725B6, // Small 'vminss'.
  0x21A56651, // Large 'vmlaun|ch'.
  0x8817B1B6, // Small 'vmload'.
  0x36F347C8, // Large 'vmmc|all'.
  0x20A757CC, // Large 'vmova|pd'.
  0x207D57CC, // Large 'vmova|ps'.
  0x804B3DB6, // Small 'vmovd'.
  0x365754BF, // Large 'vmovd|dup'.
  0x000074BF, // Large 'vmovdqa'.
  0x202C74BF, // Large 'vmovdqa|32'.
  0x203074BF, // Large 'vmovdqa|64'.
  0x109D64BF, // Large 'vmovdq|u'.
  0x34C664BF, // Large 'vmovdq|u16'.
  0x34C964BF, // Large 'vmovdq|u32'.
  0x34CC64BF, // Large 'vmovdq|u64'.
  0x265A64BF, // Large 'vmovdq|u8'.
  0x359A565C, // Large 'vmovh|lps'.
  0x20A7565C, // Large 'vmovh|pd'.
  0x207D565C, // Large 'vmovh|ps'.
  0x207D6661, // Large 'vmovlh|ps'.
  0x20A75661, // Large 'vmovl|pd'.
  0x207D5661, // Large 'vmovl|ps'.
  0x20A774CF, // Large 'vmovmsk|pd'.
  0x207D74CF, // Large 'vmovmsk|ps'.
  0x20E564D6, // Large 'vmovnt|dq'.
  0x34C364D6, // Large 'vmovnt|dqa'.
  0x20A764D6, // Large 'vmovnt|pd'.
  0x207D64D6, // Large 'vmovnt|ps'.
  0x811B3DB6, // Small 'vmovq'.
  0x893B3DB6, // Small 'vmovsd'.
  0x913B3DB6, // Small 'vmovsh'.
  0x222974DC, // Large 'vmovshd|up'.
  0x222974E3, // Large 'vmovsld|up'.
  0xA73B3DB6, // Small 'vmovss'.
  0x343344BF, // Large 'vmov|upd'.
  0x207D57D1, // Large 'vmovu|ps'.
  0x817B3DB6, // Small 'vmovw'.
  0x239A6667, // Large 'vmpsad|bw'.
  0x341147D6, // Large 'vmpt|rld'.
  0x340D47D6, // Large 'vmpt|rst'.
  0x8812C9B6, // Small 'vmread'.
  0x100B766D, // Large 'vmresum|e'.
  0x80EAC9B6, // Small 'vmrun'.
  0x8B60CDB6, // Small 'vmsave'.
  0x890655B6, // Small 'vmulpd'.
  0x910655B6, // Small 'vmulph'.
  0xA70655B6, // Small 'vmulps'.
  0x893655B6, // Small 'vmulsd'.
  0x913655B6, // Small 'vmulsh'.
  0xA73655B6, // Small 'vmulss'.
  0x20DF57DA, // Large 'vmwri|te'.
  0x8C67E1B6, // Small 'vmxoff'.
  0x80E7E1B6, // Small 'vmxon'.
  0x804849F6, // Small 'vorpd'.
  0x813849F6, // Small 'vorps'.
  0x1026C11F, // Large 'vp2intersect|d'.
  0x100FC11F, // Large 'vp2intersect|q'.
  0x1026833C, // Large 'vp4dpwss|d'.
  0x209B833C, // Large 'vp4dpwss|ds'.
  0x85310616, // Small 'vpabsb'.
  0x89310616, // Small 'vpabsd'.
  0xA3310616, // Small 'vpabsq'.
  0xAF310616, // Small 'vpabsw'.
  0x105F84EA, // Large 'vpackssd|w'.
  0x24F274EA, // Large 'vpackss|wb'.
  0x34F064F4, // Large 'vpacku|sdw'.
  0x34FA64F4, // Large 'vpacku|swb'.
  0x84420616, // Small 'vpaddb'.
  0x88420616, // Small 'vpaddd'.
  0xA2420616, // Small 'vpaddq'.
  0x25BA5674, // Large 'vpadd|sb'.
  0x23835674, // Large 'vpadd|sw'.
  0x25BA6674, // Large 'vpaddu|sb'.
  0x23836674, // Large 'vpaddu|sw'.
  0xAE420616, // Small 'vpaddw'.
  0x1023767A, // Large 'vpalign|r'.
  0x80470616, // Small 'vpand'.
  0x88470616, // Small 'vpandd'.
  0x9C470616, // Small 'vpandn'.
  0x219D57DF, // Large 'vpand|nd'.
  0x271E57DF, // Large 'vpand|nq'.
  0xA2470616, // Small 'vpandq'.
  0x847B0616, // Small 'vpavgb'.
  0xAE7B0616, // Small 'vpavgw'.
  0x102674FD, // Large 'vpblend|d'.
  0x205C74FD, // Large 'vpblend|mb'.
  0x250474FD, // Large 'vpblend|md'.
  0x100F84FD, // Large 'vpblendm|q'.
  0x105F84FD, // Large 'vpblendm|w'.
  0x202174FD, // Large 'vpblend|vb'.
  0x105F74FD, // Large 'vpblend|w'.
  0x1010B051, // Large 'vpbroadcast|b'.
  0x1026B051, // Large 'vpbroadcast|d'.
  0x100FE051, // Large 'vpbroadcastmb2|q'.
  0x305FC051, // Large 'vpbroadcastm|w2d'.
  0x100FB051, // Large 'vpbroadcast|q'.
  0x105FB051, // Large 'vpbroadcast|w'.
  0x42886344, // Large 'vpclmu|lqdq'.
  0xACF68E16, // Small 'vpcmov'.
  0x85068E16, // Small 'vpcmpb'.
  0x89068E16, // Small 'vpcmpd'.
  0x200F634A, // Large 'vpcmpe|qb'.
  0x227D634A, // Large 'vpcmpe|qd'.
  0x21F6634A, // Large 'vpcmpe|qq'.
  0x2559634A, // Large 'vpcmpe|qw'.
  0x1009934A, // Large 'vpcmpestr|i'.
  0x105C934A, // Large 'vpcmpestr|m'.
  0x3681534A, // Large 'vpcmp|gtb'.
  0x3684534A, // Large 'vpcmp|gtd'.
  0x3687534A, // Large 'vpcmp|gtq'.
  0x368A534A, // Large 'vpcmp|gtw'.
  0x10099353, // Large 'vpcmpistr|i'.
  0x105C9353, // Large 'vpcmpistr|m'.
  0xA3068E16, // Small 'vpcmpq'.
  0x209D534A, // Large 'vpcmp|ub'.
  0x22E5534A, // Large 'vpcmp|ud'.
  0x21F5534A, // Large 'vpcmp|uq'.
  0x2481534A, // Large 'vpcmp|uw'.
  0xAF068E16, // Small 'vpcmpw'.
  0x84D78E16, // Small 'vpcomb'.
  0x88D78E16, // Small 'vpcomd'.
  0x1010A24A, // Large 'vpcompress|b'.
  0x1026A24A, // Large 'vpcompress|d'.
  0x100FA24A, // Large 'vpcompress|q'.
  0x105FA24A, // Large 'vpcompress|w'.
  0xA2D78E16, // Small 'vpcomq'.
  0x209D524A, // Large 'vpcom|ub'.
  0x22E5524A, // Large 'vpcom|ud'.
  0x21F5524A, // Large 'vpcom|uq'.
  0x2481524A, // Large 'vpcom|uw'.
  0xAED78E16, // Small 'vpcomw'.
  0x1026A254, // Large 'vpconflict|d'.
  0x100FA254, // Large 'vpconflict|q'.
  0x10267506, // Large 'vpdpbss|d'.
  0x209B7506, // Large 'vpdpbss|ds'.
  0x22E56506, // Large 'vpdpbs|ud'.
  0x350D6506, // Large 'vpdpbs|uds'.
  0x10267510, // Large 'vpdpbus|d'.
  0x209B7510, // Large 'vpdpbus|ds'.
  0x22E56510, // Large 'vpdpbu|ud'.
  0x350D6510, // Large 'vpdpbu|uds'.
  0x10267517, // Large 'vpdpwss|d'.
  0x209B7517, // Large 'vpdpwss|ds'.
  0x22E56517, // Large 'vpdpws|ud'.
  0x350D6517, // Large 'vpdpws|uds'.
  0x1026751E, // Large 'vpdpwus|d'.
  0x209B751E, // Large 'vpdpwus|ds'.
  0x22E5651E, // Large 'vpdpwu|ud'.
  0x350D651E, // Large 'vpdpwu|uds'.
  0x306F735C, // Large 'vperm2f|128'.
  0x4072635C, // Large 'vperm2|i128'.
  0x84D91616, // Small 'vpermb'.
  0x88D91616, // Small 'vpermd'.
  0x268D6363, // Large 'vpermi|2b'.
  0x20606363, // Large 'vpermi|2d'.
  0x35256363, // Large 'vpermi|2pd'.
  0x307C6363, // Large 'vpermi|2ps'.
  0x268F6363, // Large 'vpermi|2q'.
  0x205E6363, // Large 'vpermi|2w'.
  0x20A78363, // Large 'vpermil2|pd'.
  0x207D8363, // Large 'vpermil2|ps'.
  0x20A77363, // Large 'vpermil|pd'.
  0x207D7363, // Large 'vpermil|ps'.
  0x20A7535C, // Large 'vperm|pd'.
  0x207D535C, // Large 'vperm|ps'.
  0xA2D91616, // Small 'vpermq'.
  0x268D6528, // Large 'vpermt|2b'.
  0x20606528, // Large 'vpermt|2d'.
  0x35256528, // Large 'vpermt|2pd'.
  0x307C6528, // Large 'vpermt|2ps'.
  0x268F6528, // Large 'vpermt|2q'.
  0x205E6528, // Large 'vpermt|2w'.
  0xAED91616, // Small 'vpermw'.
  0x2498752E, // Large 'vpexpan|db'.
  0x209A752E, // Large 'vpexpan|dd'.
  0x20E5752E, // Large 'vpexpan|dq'.
  0x24F1752E, // Large 'vpexpan|dw'.
  0x35F7452E, // Large 'vpex|trb'.
  0x261657E4, // Large 'vpext|rd'.
  0x227C57E4, // Large 'vpext|rq'.
  0x27E957E4, // Large 'vpext|rw'.
  0x209A836B, // Large 'vpgather|dd'.
  0x20E5836B, // Large 'vpgather|dq'.
  0x227D836B, // Large 'vpgather|qd'.
  0x21F6836B, // Large 'vpgather|qq'.
  0x26916535, // Large 'vphadd|bd'.
  0x26936535, // Large 'vphadd|bq'.
  0x239A6535, // Large 'vphadd|bw'.
  0x10266535, // Large 'vphadd|d'.
  0x20E56535, // Large 'vphadd|dq'.
  0x23836535, // Large 'vphadd|sw'.
  0x10268535, // Large 'vphaddub|d'.
  0x100F8535, // Large 'vphaddub|q'.
  0x105F8535, // Large 'vphaddub|w'.
  0x20E57535, // Large 'vphaddu|dq'.
  0x239C7535, // Large 'vphaddu|wd'.
  0x253D7535, // Large 'vphaddu|wq'.
  0x105F6535, // Large 'vphadd|w'.
  0x239C6535, // Large 'vphadd|wd'.
  0x253D6535, // Large 'vphadd|wq'.
  0x105FA25E, // Large 'vphminposu|w'.
  0x239A6695, // Large 'vphsub|bw'.
  0x10266695, // Large 'vphsub|d'.
  0x20E56695, // Large 'vphsub|dq'.
  0x23836695, // Large 'vphsub|sw'.
  0x105F6695, // Large 'vphsub|w'.
  0x239C6695, // Large 'vphsub|wd'.
  0x25F857EB, // Large 'vpins|rb'.
  0x261657EB, // Large 'vpins|rd'.
  0x227C57EB, // Large 'vpins|rq'.
  0x27E957EB, // Large 'vpins|rw'.
  0x2455669B, // Large 'vplzcn|td'.
  0x2193669B, // Large 'vplzcn|tq'.
  0x209A6373, // Large 'vpmacs|dd'.
  0x353F6373, // Large 'vpmacs|dqh'.
  0x33A26373, // Large 'vpmacs|dql'.
  0x10268373, // Large 'vpmacssd|d'.
  0x10A39373, // Large 'vpmacssdq|h'.
  0x10D29373, // Large 'vpmacssdq|l'.
  0x239C7373, // Large 'vpmacss|wd'.
  0x239B7373, // Large 'vpmacss|ww'.
  0x239C6373, // Large 'vpmacs|wd'.
  0x239B6373, // Large 'vpmacs|ww'.
  0x1026937C, // Large 'vpmadcssw|d'.
  0x239C737C, // Large 'vpmadcs|wd'.
  0x21F59268, // Large 'vpmadd52h|uq'.
  0x32718268, // Large 'vpmadd52|luq'.
  0x43856268, // Large 'vpmadd|ubsw'.
  0x239C6268, // Large 'vpmadd|wd'.
  0x62434268, // Large 'vpma|skmovd'.
  0x200E8389, // Large 'vpmaskmo|vq'.
  0x25BA57F0, // Large 'vpmax|sb'.
  0x214457F0, // Large 'vpmax|sd'.
  0x23A957F0, // Large 'vpmax|sq'.
  0x238357F0, // Large 'vpmax|sw'.
  0x209D57F0, // Large 'vpmax|ub'.
  0x22E557F0, // Large 'vpmax|ud'.
  0x21F557F0, // Large 'vpmax|uq'.
  0x248157F0, // Large 'vpmax|uw'.
  0x25BA57F5, // Large 'vpmin|sb'.
  0x214457F5, // Large 'vpmin|sd'.
  0x23A957F5, // Large 'vpmin|sq'.
  0x238357F5, // Large 'vpmin|sw'.
  0x209D57F5, // Large 'vpmin|ub'.
  0x22E557F5, // Large 'vpmin|ud'.
  0x21F557F5, // Large 'vpmin|uq'.
  0x248157F5, // Large 'vpmin|uw'.
  0x36A15542, // Large 'vpmov|b2m'.
  0x36A45542, // Large 'vpmov|d2m'.
  0x24985542, // Large 'vpmov|db'.
  0x24F15542, // Large 'vpmov|dw'.
  0x268D6542, // Large 'vpmovm|2b'.
  0x20606542, // Large 'vpmovm|2d'.
  0x268F6542, // Large 'vpmovm|2q'.
  0x205E6542, // Large 'vpmovm|2w'.
  0x10108542, // Large 'vpmovmsk|b'.
  0x36A75542, // Large 'vpmov|q2m'.
  0x200F5542, // Large 'vpmov|qb'.
  0x227D5542, // Large 'vpmov|qd'.
  0x25595542, // Large 'vpmov|qw'.
  0x2498654A, // Large 'vpmovs|db'.
  0x24F1654A, // Large 'vpmovs|dw'.
  0x200F654A, // Large 'vpmovs|qb'.
  0x227D654A, // Large 'vpmovs|qd'.
  0x2559654A, // Large 'vpmovs|qw'.
  0x24F2654A, // Large 'vpmovs|wb'.
  0x1026854A, // Large 'vpmovsxb|d'.
  0x100F854A, // Large 'vpmovsxb|q'.
  0x105F854A, // Large 'vpmovsxb|w'.
  0x20E5754A, // Large 'vpmovsx|dq'.
  0x239C754A, // Large 'vpmovsx|wd'.
  0x253D754A, // Large 'vpmovsx|wq'.
  0x24987552, // Large 'vpmovus|db'.
  0x24F17552, // Large 'vpmovus|dw'.
  0x200F7552, // Large 'vpmovus|qb'.
  0x227D7552, // Large 'vpmovus|qd'.
  0x25597552, // Large 'vpmovus|qw'.
  0x24F27552, // Large 'vpmovus|wb'.
  0x36AA5542, // Large 'vpmov|w2m'.
  0x24F25542, // Large 'vpmov|wb'.
  0x1026855B, // Large 'vpmovzxb|d'.
  0x100F855B, // Large 'vpmovzxb|q'.
  0x105F855B, // Large 'vpmovzxb|w'.
  0x20E5755B, // Large 'vpmovzx|dq'.
  0x239C755B, // Large 'vpmovzx|wd'.
  0x253D755B, // Large 'vpmovzx|wq'.
  0x20E550CE, // Large 'vpmul|dq'.
  0x23837563, // Large 'vpmulhr|sw'.
  0x24816563, // Large 'vpmulh|uw'.
  0x105F6563, // Large 'vpmulh|w'.
  0x23A150CE, // Large 'vpmul|ld'.
  0x228850CE, // Large 'vpmul|lq'.
  0x23A450CE, // Large 'vpmul|lw'.
  0x200FC0CE, // Large 'vpmultishift|qb'.
  0x32E550CE, // Large 'vpmul|udq'.
  0x268266AD, // Large 'vpopcn|tb'.
  0x245566AD, // Large 'vpopcn|td'.
  0x219366AD, // Large 'vpopcn|tq'.
  0x264366AD, // Large 'vpopcn|tw'.
  0x80093E16, // Small 'vpor'.
  0x80493E16, // Small 'vpord'.
  0x81193E16, // Small 'vporq'.
  0x9B22C216, // Small 'vpperm'.
  0x88C7CA16, // Small 'vprold'.
  0xA2C7CA16, // Small 'vprolq'.
  0x224757FA, // Large 'vprol|vd'.
  0x200E57FA, // Large 'vprol|vq'.
  0x8927CA16, // Small 'vprord'.
  0xA327CA16, // Small 'vprorq'.
  0x224757FF, // Large 'vpror|vd'.
  0x200E57FF, // Large 'vpror|vq'.
  0x8547CA16, // Small 'vprotb'.
  0x8947CA16, // Small 'vprotd'.
  0xA347CA16, // Small 'vprotq'.
  0xAF47CA16, // Small 'vprotw'.
  0x239A5804, // Large 'vpsad|bw'.
  0x209A9274, // Large 'vpscatter|dd'.
  0x20E59274, // Large 'vpscatter|dq'.
  0x227D9274, // Large 'vpscatter|qd'.
  0x100FA274, // Large 'vpscatterq|q'.
  0x84144E16, // Small 'vpshab'.
  0x88144E16, // Small 'vpshad'.
  0xA2144E16, // Small 'vpshaq'.
  0xAE144E16, // Small 'vpshaw'.
  0x84C44E16, // Small 'vpshlb'.
  0x88C44E16, // Small 'vpshld'.
  0x102666B3, // Large 'vpshld|d'.
  0x100F66B3, // Large 'vpshld|q'.
  0x349D56B3, // Large 'vpshl|dvd'.
  0x36B856B3, // Large 'vpshl|dvq'.
  0x105F76B3, // Large 'vpshldv|w'.
  0x105F66B3, // Large 'vpshld|w'.
  0xA2C44E16, // Small 'vpshlq'.
  0xAEC44E16, // Small 'vpshlw'.
  0x102666BB, // Large 'vpshrd|d'.
  0x100F66BB, // Large 'vpshrd|q'.
  0x349D56BB, // Large 'vpshr|dvd'.
  0x36B856BB, // Large 'vpshr|dvq'.
  0x36C056BB, // Large 'vpshr|dvw'.
  0x105F66BB, // Large 'vpshrd|w'.
  0x0000718B, // Large 'vpshufb'.
  0x205CA18B, // Large 'vpshufbitq|mb'.
  0x1026618B, // Large 'vpshuf|d'.
  0x26C3618B, // Large 'vpshuf|hw'.
  0x23A4618B, // Large 'vpshuf|lw'.
  0x22A95809, // Large 'vpsig|nb'.
  0x219D5809, // Large 'vpsig|nd'.
  0x26FB5809, // Large 'vpsig|nw'.
  0x88C64E16, // Small 'vpslld'.
  0x33A1480E, // Large 'vpsl|ldq'.
  0xA2C64E16, // Small 'vpsllq'.
  0x22475812, // Large 'vpsll|vd'.
  0x200E5812, // Large 'vpsll|vq'.
  0x26C15812, // Large 'vpsll|vw'.
  0xAEC64E16, // Small 'vpsllw'.
  0x88194E16, // Small 'vpsrad'.
  0xA2194E16, // Small 'vpsraq'.
  0x22475817, // Large 'vpsra|vd'.
  0x200E5817, // Large 'vpsra|vq'.
  0x26C15817, // Large 'vpsra|vw'.
  0xAE194E16, // Small 'vpsraw'.
  0x88C94E16, // Small 'vpsrld'.
  0x33A14817, // Large 'vpsr|ldq'.
  0xA2C94E16, // Small 'vpsrlq'.
  0x2247581C, // Large 'vpsrl|vd'.
  0x200E581C, // Large 'vpsrl|vq'.
  0x26C1581C, // Large 'vpsrl|vw'.
  0xAEC94E16, // Small 'vpsrlw'.
  0x842ACE16, // Small 'vpsubb'.
  0x882ACE16, // Small 'vpsubd'.
  0xA22ACE16, // Small 'vpsubq'.
  0x25BA56C5, // Large 'vpsub|sb'.
  0x238356C5, // Large 'vpsub|sw'.
  0x25BA66C5, // Large 'vpsubu|sb'.
  0x238366C5, // Large 'vpsubu|sw'.
  0xAE2ACE16, // Small 'vpsubw'.
  0x10269391, // Large 'vpternlog|d'.
  0x100F9391, // Large 'vpternlog|q'.
  0xA932D216, // Small 'vptest'.
  0x205C656A, // Large 'vptest|mb'.
  0x2504656A, // Large 'vptest|md'.
  0x2571656A, // Large 'vptest|mq'.
  0x26A9656A, // Large 'vptest|mw'.
  0x205C756A, // Large 'vptestn|mb'.
  0x2504756A, // Large 'vptestn|md'.
  0x2571756A, // Large 'vptestn|mq'.
  0x105F856A, // Large 'vptestnm|w'.
  0x239A827F, // Large 'vpunpckh|bw'.
  0x20E5827F, // Large 'vpunpckh|dq'.
  0x20E5927F, // Large 'vpunpckhq|dq'.
  0x239C827F, // Large 'vpunpckh|wd'.
  0x339E727F, // Large 'vpunpck|lbw'.
  0x33A1727F, // Large 'vpunpck|ldq'.
  0x4288727F, // Large 'vpunpck|lqdq'.
  0x33A4727F, // Large 'vpunpck|lwd'.
  0x8127E216, // Small 'vpxor'.
  0x8927E216, // Small 'vpxord'.
  0xA327E216, // Small 'vpxorq'.
  0x20A766CB, // Large 'vrange|pd'.
  0x207D66CB, // Large 'vrange|ps'.
  0x214466CB, // Large 'vrange|sd'.
  0x201C66CB, // Large 'vrange|ss'.
  0x20A766D1, // Large 'vrcp14|pd'.
  0x207D66D1, // Large 'vrcp14|ps'.
  0x214466D1, // Large 'vrcp14|sd'.
  0x201C66D1, // Large 'vrcp14|ss'.
  0x43AF46D1, // Large 'vrcp|28pd'.
  0x43B346D1, // Large 'vrcp|28ps'.
  0x43B746D1, // Large 'vrcp|28sd'.
  0x43BB46D1, // Large 'vrcp|28ss'.
  0x91080E56, // Small 'vrcpph'.
  0xA7080E56, // Small 'vrcpps'.
  0x91380E56, // Small 'vrcpsh'.
  0xA7380E56, // Small 'vrcpss'.
  0x20A77573, // Large 'vreduce|pd'.
  0x20A27573, // Large 'vreduce|ph'.
  0x207D7573, // Large 'vreduce|ps'.
  0x21447573, // Large 'vreduce|sd'.
  0x20D57573, // Large 'vreduce|sh'.
  0x201C7573, // Large 'vreduce|ss'.
  0x20A7928C, // Large 'vrndscale|pd'.
  0x20A2928C, // Large 'vrndscale|ph'.
  0x207D928C, // Large 'vrndscale|ps'.
  0x2144928C, // Large 'vrndscale|sd'.
  0x20D5928C, // Large 'vrndscale|sh'.
  0x201C928C, // Large 'vrndscale|ss'.
  0x30ED56D7, // Large 'vroun|dpd'.
  0x30F156D7, // Large 'vroun|dps'.
  0x36DC56D7, // Large 'vroun|dsd'.
  0x101476D7, // Large 'vrounds|s'.
  0x20A783A7, // Large 'vrsqrt14|pd'.
  0x207D83A7, // Large 'vrsqrt14|ps'.
  0x214483A7, // Large 'vrsqrt14|sd'.
  0x201C83A7, // Large 'vrsqrt14|ss'.
  0x43AF63A7, // Large 'vrsqrt|28pd'.
  0x43B363A7, // Large 'vrsqrt|28ps'.
  0x43B763A7, // Large 'vrsqrt|28sd'.
  0x43BB63A7, // Large 'vrsqrt|28ss'.
  0x20A263A7, // Large 'vrsqrt|ph'.
  0x207D63A7, // Large 'vrsqrt|ps'.
  0x20D563A7, // Large 'vrsqrt|sh'.
  0x201C63A7, // Large 'vrsqrt|ss'.
  0x20A7757A, // Large 'vscalef|pd'.
  0x20A2757A, // Large 'vscalef|ph'.
  0x207D757A, // Large 'vscalef|ps'.
  0x2144757A, // Large 'vscalef|sd'.
  0x20D5757A, // Large 'vscalef|sh'.
  0x201C757A, // Large 'vscalef|ss'.
  0x30ED80DA, // Large 'vscatter|dpd'.
  0x30F180DA, // Large 'vscatter|dps'.
  0x20A7C0DA, // Large 'vscatterpf0d|pd'.
  0x207DC0DA, // Large 'vscatterpf0d|ps'.
  0x30E6B0DA, // Large 'vscatterpf0|qpd'.
  0x30E9B0DA, // Large 'vscatterpf0|qps'.
  0x40ECA0DA, // Large 'vscatterpf|1dpd'.
  0x40F0A0DA, // Large 'vscatterpf|1dps'.
  0x40F4A0DA, // Large 'vscatterpf|1qpd'.
  0x40F8A0DA, // Large 'vscatterpf|1qps'.
  0x30E680DA, // Large 'vscatter|qpd'.
  0x30E980DA, // Large 'vscatter|qps'.
  0x42957195, // Large 'vsha512|msg1'.
  0x42997195, // Large 'vsha512|msg2'.
  0x207EA195, // Large 'vsha512rnd|s2'.
  0x502B53BF, // Large 'vshuf|f32x4'.
  0x403063C4, // Large 'vshuff|64x2'.
  0x503D53BF, // Large 'vshuf|i32x4'.
  0x504753BF, // Large 'vshuf|i64x2'.
  0x20A753BF, // Large 'vshuf|pd'.
  0x207D53BF, // Large 'vshuf|ps'.
  0x42954581, // Large 'vsm3|msg1'.
  0x42994581, // Large 'vsm3|msg2'.
  0x207E7581, // Large 'vsm3rnd|s2'.
  0x102F76DF, // Large 'vsm4key|4'.
  0x102F8588, // Large 'vsm4rnds|4'.
  0x31F14821, // Large 'vsqr|tpd'.
  0x31FC4821, // Large 'vsqr|tph'.
  0x32054821, // Large 'vsqr|tps'.
  0x320E4821, // Large 'vsqr|tsd'.
  0x32174821, // Large 'vsqr|tsh'.
  0x32204821, // Large 'vsqr|tss'.
  0x102376E6, // Large 'vstmxcs|r'.
  0x89015676, // Small 'vsubpd'.
  0x91015676, // Small 'vsubph'.
  0xA7015676, // Small 'vsubps'.
  0x89315676, // Small 'vsubsd'.
  0x91315676, // Small 'vsubsh'.
  0xA7315676, // Small 'vsubss'.
  0x31F14825, // Large 'vtes|tpd'.
  0x32054825, // Large 'vtes|tps'.
  0x214466ED, // Large 'vucomi|sd'.
  0x20D566ED, // Large 'vucomi|sh'.
  0x201C66ED, // Large 'vucomi|ss'.
  0x20A77590, // Large 'vunpckh|pd'.
  0x207D7590, // Large 'vunpckh|ps'.
  0x35976590, // Large 'vunpck|lpd'.
  0x359A6590, // Large 'vunpck|lps'.
  0x89093F16, // Small 'vxorpd'.
  0xA7093F16, // Small 'vxorps'.
  0x36F353CA, // Large 'vzero|all'.
  0x335D73CA, // Large 'vzeroup|per'.
  0x89672457, // Small 'wbinvd'.
  0x224766F6, // Large 'wbnoin|vd'.
  0x343656FC, // Large 'wrfsb|ase'.
  0x34365701, // Large 'wrgsb|ase'.
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
  0x1015859D, // Large 'xresldtr|k'.
  0xA4FA4E58, // Small 'xrstor'.
  0x2030640C, // Large 'xrstor|64'.
  0x1014640C, // Large 'xrstor|s'.
  0x35A5640C, // Large 'xrstor|s64'.
  0x805B0678, // Small 'xsave'.
  0x203053D1, // Large 'xsave|64'.
  0x865B0678, // Small 'xsavec'.
  0x370653D1, // Large 'xsave|c64'.
  0x000083D1, // Large 'xsaveopt'.
  0x203083D1, // Large 'xsaveopt|64'.
  0xA65B0678, // Small 'xsaves'.
  0x35A553D1, // Large 'xsave|s64'.
  0xAC2A1678, // Small 'xsetbv'.
  0x101585A8, // Large 'xsusldtr|k'.
  0x81499698  // Small 'xtest'.
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
  ROW(2, 0, 1, 0, 15 , 16 , 0  , 0  , 0  , 0  ), //      {r64|m64, i32}
  ROW(2, 0, 1, 0, 8  , 17 , 0  , 0  , 0  , 0  ), //      {r64, i64|u64|m64|mem|sreg|creg|dreg}
  ROW(2, 1, 1, 0, 2  , 18 , 0  , 0  , 0  , 0  ), //      {r8lo|r8hi, m8|mem}
  ROW(2, 1, 1, 0, 4  , 19 , 0  , 0  , 0  , 0  ), //      {r16, m16|mem|sreg}
  ROW(2, 1, 1, 0, 6  , 20 , 0  , 0  , 0  , 0  ), //      {r32, m32|mem|sreg}
  ROW(2, 1, 1, 0, 21 , 22 , 0  , 0  , 0  , 0  ), //      {m16|mem, sreg}
  ROW(2, 1, 1, 0, 21 , 22 , 0  , 0  , 0  , 0  ), //      {m16|mem, sreg}
  ROW(2, 0, 1, 0, 21 , 22 , 0  , 0  , 0  , 0  ), //      {m16|mem, sreg}
  ROW(2, 1, 1, 0, 22 , 21 , 0  , 0  , 0  , 0  ), //      {sreg, m16|mem}
  ROW(2, 1, 1, 0, 22 , 21 , 0  , 0  , 0  , 0  ), //      {sreg, m16|mem}
  ROW(2, 0, 1, 0, 22 , 21 , 0  , 0  , 0  , 0  ), //      {sreg, m16|mem}
  ROW(2, 1, 0, 0, 6  , 23 , 0  , 0  , 0  , 0  ), //      {r32, creg|dreg}
  ROW(2, 1, 0, 0, 23 , 6  , 0  , 0  , 0  , 0  ), //      {creg|dreg, r32}
  ROW(2, 1, 1, 0, 9  , 10 , 0  , 0  , 0  , 0  ), // #20  {r8lo|r8hi|m8, i8|u8}
  ROW(2, 1, 1, 0, 11 , 12 , 0  , 0  , 0  , 0  ), //      {r16|m16, i16|u16}
  ROW(2, 1, 1, 0, 13 , 14 , 0  , 0  , 0  , 0  ), //      {r32|m32, i32|u32}
  ROW(2, 0, 1, 0, 15 , 24 , 0  , 0  , 0  , 0  ), //      {r64|m64, i32|i8}
  ROW(2, 1, 1, 0, 25 , 26 , 0  , 0  , 0  , 0  ), //      {r16|m16|r32|m32, i8}
  ROW(2, 1, 1, 0, 1  , 2  , 0  , 0  , 0  , 0  ), // #25  {r8lo|r8hi|m8|mem, r8lo|r8hi}
  ROW(2, 1, 1, 0, 27 , 4  , 0  , 0  , 0  , 0  ), //      {r16|m16|mem, r16}
  ROW(2, 1, 1, 0, 28 , 6  , 0  , 0  , 0  , 0  ), // #27  {r32|m32|mem, r32}
  ROW(2, 0, 1, 0, 29 , 8  , 0  , 0  , 0  , 0  ), // #28  {r64|m64|mem, r64}
  ROW(2, 1, 1, 0, 2  , 18 , 0  , 0  , 0  , 0  ), //      {r8lo|r8hi, m8|mem}
  ROW(2, 1, 1, 0, 4  , 21 , 0  , 0  , 0  , 0  ), //      {r16, m16|mem}
  ROW(2, 1, 1, 0, 6  , 30 , 0  , 0  , 0  , 0  ), //      {r32, m32|mem}
  ROW(2, 0, 1, 0, 8  , 31 , 0  , 0  , 0  , 0  ), //      {r64, m64|mem}
  ROW(2, 1, 1, 0, 32 , 10 , 0  , 0  , 0  , 0  ), // #33  {r8lo|r8hi|m8|r16|m16|r32|m32, i8|u8}
  ROW(2, 1, 1, 0, 11 , 12 , 0  , 0  , 0  , 0  ), //      {r16|m16, i16|u16}
  ROW(2, 1, 1, 0, 13 , 14 , 0  , 0  , 0  , 0  ), //      {r32|m32, i32|u32}
  ROW(2, 0, 1, 0, 8  , 33 , 0  , 0  , 0  , 0  ), //      {r64, u32|i32|i8|u8|r64|m64|mem}
  ROW(2, 0, 1, 0, 34 , 35 , 0  , 0  , 0  , 0  ), //      {m64, i32|i8|u8}
  ROW(2, 1, 1, 0, 1  , 2  , 0  , 0  , 0  , 0  ), //      {r8lo|r8hi|m8|mem, r8lo|r8hi}
  ROW(2, 1, 1, 0, 27 , 4  , 0  , 0  , 0  , 0  ), //      {r16|m16|mem, r16}
  ROW(2, 1, 1, 0, 28 , 6  , 0  , 0  , 0  , 0  ), //      {r32|m32|mem, r32}
  ROW(2, 0, 1, 0, 31 , 8  , 0  , 0  , 0  , 0  ), //      {m64|mem, r64}
  ROW(2, 1, 1, 0, 2  , 18 , 0  , 0  , 0  , 0  ), //      {r8lo|r8hi, m8|mem}
  ROW(2, 1, 1, 0, 4  , 21 , 0  , 0  , 0  , 0  ), //      {r16, m16|mem}
  ROW(2, 1, 1, 0, 6  , 30 , 0  , 0  , 0  , 0  ), //      {r32, m32|mem}
  ROW(2, 1, 1, 1, 36 , 1  , 0  , 0  , 0  , 0  ), // #45  {<ax>, r8lo|r8hi|m8|mem}
  ROW(3, 1, 1, 2, 37 , 36 , 27 , 0  , 0  , 0  ), //      {<dx>, <ax>, r16|m16|mem}
  ROW(3, 1, 1, 2, 38 , 39 , 28 , 0  , 0  , 0  ), //      {<edx>, <eax>, r32|m32|mem}
  ROW(3, 0, 1, 2, 40 , 41 , 29 , 0  , 0  , 0  ), //      {<rdx>, <rax>, r64|m64|mem}
  ROW(2, 1, 1, 0, 4  , 27 , 0  , 0  , 0  , 0  ), // #49  {r16, r16|m16|mem}
  ROW(2, 1, 1, 0, 6  , 28 , 0  , 0  , 0  , 0  ), // #50  {r32, r32|m32|mem}
  ROW(2, 0, 1, 0, 8  , 29 , 0  , 0  , 0  , 0  ), //      {r64, r64|m64|mem}
  ROW(3, 1, 1, 0, 4  , 27 , 42 , 0  , 0  , 0  ), //      {r16, r16|m16|mem, i8|i16|u16}
  ROW(3, 1, 1, 0, 6  , 28 , 43 , 0  , 0  , 0  ), //      {r32, r32|m32|mem, i8|i32|u32}
  ROW(3, 0, 1, 0, 8  , 29 , 24 , 0  , 0  , 0  ), //      {r64, r64|m64|mem, i8|i32}
  ROW(2, 0, 1, 0, 8  , 44 , 0  , 0  , 0  , 0  ), // #55  {r64, i64|u64}
  ROW(2, 1, 1, 0, 45 , 18 , 0  , 0  , 0  , 0  ), //      {al, m8|mem}
  ROW(2, 1, 1, 0, 46 , 21 , 0  , 0  , 0  , 0  ), //      {ax, m16|mem}
  ROW(2, 1, 1, 0, 47 , 30 , 0  , 0  , 0  , 0  ), //      {eax, m32|mem}
  ROW(2, 0, 1, 0, 48 , 31 , 0  , 0  , 0  , 0  ), //      {rax, m64|mem}
  ROW(2, 1, 1, 0, 18 , 45 , 0  , 0  , 0  , 0  ), //      {m8|mem, al}
  ROW(2, 1, 1, 0, 21 , 46 , 0  , 0  , 0  , 0  ), //      {m16|mem, ax}
  ROW(2, 1, 1, 0, 30 , 47 , 0  , 0  , 0  , 0  ), //      {m32|mem, eax}
  ROW(2, 0, 1, 0, 31 , 48 , 0  , 0  , 0  , 0  ), //      {m64|mem, rax}
  ROW(2, 1, 1, 0, 9  , 10 , 0  , 0  , 0  , 0  ), // #64  {r8lo|r8hi|m8, i8|u8}
  ROW(2, 1, 1, 0, 11 , 12 , 0  , 0  , 0  , 0  ), //      {r16|m16, i16|u16}
  ROW(2, 1, 1, 0, 13 , 14 , 0  , 0  , 0  , 0  ), //      {r32|m32, i32|u32}
  ROW(2, 0, 1, 0, 15 , 16 , 0  , 0  , 0  , 0  ), //      {r64|m64, i32}
  ROW(2, 1, 1, 0, 1  , 2  , 0  , 0  , 0  , 0  ), //      {r8lo|r8hi|m8|mem, r8lo|r8hi}
  ROW(2, 1, 1, 0, 27 , 4  , 0  , 0  , 0  , 0  ), //      {r16|m16|mem, r16}
  ROW(2, 1, 1, 0, 28 , 6  , 0  , 0  , 0  , 0  ), //      {r32|m32|mem, r32}
  ROW(2, 0, 1, 0, 29 , 8  , 0  , 0  , 0  , 0  ), //      {r64|m64|mem, r64}
  ROW(2, 1, 1, 0, 49 , 50 , 0  , 0  , 0  , 0  ), // #72  {xmm, xmm|m128|mem}
  ROW(2, 1, 1, 0, 51 , 49 , 0  , 0  , 0  , 0  ), // #73  {m128|mem, xmm}
  ROW(2, 1, 1, 0, 52 , 53 , 0  , 0  , 0  , 0  ), //      {ymm, ymm|m256|mem}
  ROW(2, 1, 1, 0, 54 , 52 , 0  , 0  , 0  , 0  ), //      {m256|mem, ymm}
  ROW(2, 1, 1, 0, 51 , 49 , 0  , 0  , 0  , 0  ), //      {m128|mem, xmm}
  ROW(2, 1, 1, 0, 54 , 52 , 0  , 0  , 0  , 0  ), //      {m256|mem, ymm}
  ROW(2, 1, 1, 0, 55 , 56 , 0  , 0  , 0  , 0  ), // #78  {zmm, zmm|m512|mem}
  ROW(2, 1, 1, 0, 57 , 55 , 0  , 0  , 0  , 0  ), //      {m512|mem, zmm}
  ROW(2, 1, 1, 0, 31 , 49 , 0  , 0  , 0  , 0  ), // #80  {m64|mem, xmm}
  ROW(2, 1, 1, 0, 49 , 31 , 0  , 0  , 0  , 0  ), //      {xmm, m64|mem}
  ROW(3, 1, 1, 0, 49 , 49 , 49 , 0  , 0  , 0  ), // #82  {xmm, xmm, xmm}
  ROW(3, 1, 1, 0, 49 , 49 , 49 , 0  , 0  , 0  ), //      {xmm, xmm, xmm}
  ROW(2, 1, 1, 0, 31 , 49 , 0  , 0  , 0  , 0  ), //      {m64|mem, xmm}
  ROW(2, 1, 1, 0, 49 , 31 , 0  , 0  , 0  , 0  ), //      {xmm, m64|mem}
  ROW(3, 1, 1, 0, 49 , 49 , 49 , 0  , 0  , 0  ), //      {xmm, xmm, xmm}
  ROW(3, 1, 1, 0, 49 , 49 , 49 , 0  , 0  , 0  ), //      {xmm, xmm, xmm}
  ROW(2, 1, 1, 0, 30 , 49 , 0  , 0  , 0  , 0  ), // #88  {m32|mem, xmm}
  ROW(2, 1, 1, 0, 49 , 30 , 0  , 0  , 0  , 0  ), //      {xmm, m32|mem}
  ROW(3, 1, 1, 0, 49 , 49 , 49 , 0  , 0  , 0  ), //      {xmm, xmm, xmm}
  ROW(3, 1, 1, 0, 49 , 49 , 49 , 0  , 0  , 0  ), //      {xmm, xmm, xmm}
  ROW(2, 1, 1, 0, 30 , 49 , 0  , 0  , 0  , 0  ), //      {m32|mem, xmm}
  ROW(2, 1, 1, 0, 49 , 30 , 0  , 0  , 0  , 0  ), //      {xmm, m32|mem}
  ROW(3, 1, 1, 0, 49 , 49 , 49 , 0  , 0  , 0  ), //      {xmm, xmm, xmm}
  ROW(3, 1, 1, 0, 49 , 49 , 49 , 0  , 0  , 0  ), //      {xmm, xmm, xmm}
  ROW(3, 1, 1, 0, 49 , 49 , 58 , 0  , 0  , 0  ), // #96  {xmm, xmm, xmm|m128|mem|i8|u8}
  ROW(3, 1, 1, 0, 49 , 51 , 10 , 0  , 0  , 0  ), //      {xmm, m128|mem, i8|u8}
  ROW(3, 1, 1, 0, 52 , 52 , 59 , 0  , 0  , 0  ), //      {ymm, ymm, ymm|m256|mem|i8|u8}
  ROW(3, 1, 1, 0, 52 , 54 , 10 , 0  , 0  , 0  ), //      {ymm, m256|mem, i8|u8}
  ROW(3, 1, 1, 0, 55 , 55 , 60 , 0  , 0  , 0  ), //      {zmm, zmm, zmm|m512|mem|i8|u8}
  ROW(3, 1, 1, 0, 49 , 51 , 10 , 0  , 0  , 0  ), //      {xmm, m128|mem, i8|u8}
  ROW(3, 1, 1, 0, 52 , 54 , 10 , 0  , 0  , 0  ), //      {ymm, m256|mem, i8|u8}
  ROW(3, 1, 1, 0, 55 , 57 , 10 , 0  , 0  , 0  ), //      {zmm, m512|mem, i8|u8}
  ROW(2, 1, 1, 0, 4  , 21 , 0  , 0  , 0  , 0  ), // #104 {r16, m16|mem}
  ROW(2, 1, 1, 0, 6  , 30 , 0  , 0  , 0  , 0  ), //      {r32, m32|mem}
  ROW(2, 0, 1, 0, 8  , 31 , 0  , 0  , 0  , 0  ), //      {r64, m64|mem}
  ROW(2, 1, 1, 0, 21 , 4  , 0  , 0  , 0  , 0  ), //      {m16|mem, r16}
  ROW(2, 1, 1, 0, 30 , 6  , 0  , 0  , 0  , 0  ), // #108 {m32|mem, r32}
  ROW(2, 0, 1, 0, 31 , 8  , 0  , 0  , 0  , 0  ), //      {m64|mem, r64}
  ROW(0, 1, 1, 0, 0  , 0  , 0  , 0  , 0  , 0  ), // #110 {}
  ROW(1, 1, 1, 0, 25 , 0  , 0  , 0  , 0  , 0  ), //      {r16|m16|r32|m32}
  ROW(1, 0, 1, 0, 15 , 0  , 0  , 0  , 0  , 0  ), //      {r64|m64}
  ROW(2, 1, 1, 0, 27 , 4  , 0  , 0  , 0  , 0  ), //      {r16|m16|mem, r16}
  ROW(2, 1, 1, 0, 28 , 6  , 0  , 0  , 0  , 0  ), //      {r32|m32|mem, r32}
  ROW(2, 0, 1, 0, 29 , 8  , 0  , 0  , 0  , 0  ), //      {r64|m64|mem, r64}
  ROW(2, 1, 1, 0, 49 , 50 , 0  , 0  , 0  , 0  ), // #116 {xmm, xmm|m128|mem}
  ROW(2, 1, 1, 0, 51 , 49 , 0  , 0  , 0  , 0  ), //      {m128|mem, xmm}
  ROW(2, 1, 1, 0, 52 , 53 , 0  , 0  , 0  , 0  ), //      {ymm, ymm|m256|mem}
  ROW(2, 1, 1, 0, 54 , 52 , 0  , 0  , 0  , 0  ), //      {m256|mem, ymm}
  ROW(2, 1, 1, 0, 55 , 56 , 0  , 0  , 0  , 0  ), //      {zmm, zmm|m512|mem}
  ROW(2, 1, 1, 0, 57 , 55 , 0  , 0  , 0  , 0  ), //      {m512|mem, zmm}
  ROW(3, 1, 1, 0, 49 , 49 , 58 , 0  , 0  , 0  ), // #122 {xmm, xmm, i8|u8|xmm|m128|mem}
  ROW(3, 1, 1, 0, 52 , 52 , 58 , 0  , 0  , 0  ), //      {ymm, ymm, i8|u8|xmm|m128|mem}
  ROW(3, 1, 1, 0, 49 , 51 , 10 , 0  , 0  , 0  ), //      {xmm, m128|mem, i8|u8}
  ROW(3, 1, 1, 0, 52 , 54 , 10 , 0  , 0  , 0  ), //      {ymm, m256|mem, i8|u8}
  ROW(3, 1, 1, 0, 55 , 55 , 58 , 0  , 0  , 0  ), //      {zmm, zmm, xmm|m128|mem|i8|u8}
  ROW(3, 1, 1, 0, 55 , 57 , 10 , 0  , 0  , 0  ), //      {zmm, m512|mem, i8|u8}
  ROW(3, 1, 1, 0, 49 , 49 , 58 , 0  , 0  , 0  ), // #128 {xmm, xmm, xmm|m128|mem|i8|u8}
  ROW(3, 1, 1, 0, 49 , 51 , 10 , 0  , 0  , 0  ), //      {xmm, m128|mem, i8|u8}
  ROW(3, 1, 1, 0, 52 , 52 , 58 , 0  , 0  , 0  ), //      {ymm, ymm, xmm|m128|mem|i8|u8}
  ROW(3, 1, 1, 0, 52 , 54 , 10 , 0  , 0  , 0  ), //      {ymm, m256|mem, i8|u8}
  ROW(3, 1, 1, 0, 55 , 55 , 58 , 0  , 0  , 0  ), //      {zmm, zmm, xmm|m128|mem|i8|u8}
  ROW(3, 1, 1, 0, 55 , 57 , 10 , 0  , 0  , 0  ), //      {zmm, m512|mem, i8|u8}
  ROW(2, 1, 1, 0, 25 , 10 , 0  , 0  , 0  , 0  ), // #134 {r16|m16|r32|m32, i8|u8}
  ROW(2, 0, 1, 0, 15 , 10 , 0  , 0  , 0  , 0  ), //      {r64|m64, i8|u8}
  ROW(2, 1, 1, 0, 27 , 4  , 0  , 0  , 0  , 0  ), //      {r16|m16|mem, r16}
  ROW(2, 1, 1, 0, 28 , 6  , 0  , 0  , 0  , 0  ), //      {r32|m32|mem, r32}
  ROW(2, 0, 1, 0, 29 , 8  , 0  , 0  , 0  , 0  ), //      {r64|m64|mem, r64}
  ROW(2, 1, 1, 0, 61 , 62 , 0  , 0  , 0  , 0  ), // #139 {mm, mm|m64|mem}
  ROW(2, 0, 1, 0, 63 , 29 , 0  , 0  , 0  , 0  ), //      {mm|xmm, r64|m64|mem}
  ROW(2, 1, 1, 0, 31 , 63 , 0  , 0  , 0  , 0  ), //      {m64|mem, mm|xmm}
  ROW(2, 0, 1, 0, 29 , 63 , 0  , 0  , 0  , 0  ), //      {r64|m64|mem, mm|xmm}
  ROW(2, 1, 1, 0, 49 , 64 , 0  , 0  , 0  , 0  ), // #143 {xmm, xmm|m64|mem}
  ROW(1, 1, 1, 0, 11 , 0  , 0  , 0  , 0  , 0  ), // #144 {r16|m16}
  ROW(1, 1, 0, 0, 13 , 0  , 0  , 0  , 0  , 0  ), //      {r32|m32}
  ROW(1, 0, 1, 0, 15 , 0  , 0  , 0  , 0  , 0  ), //      {r64|m64}
  ROW(1, 1, 0, 0, 65 , 0  , 0  , 0  , 0  , 0  ), //      {ds|es|ss}
  ROW(1, 1, 1, 0, 66 , 0  , 0  , 0  , 0  , 0  ), //      {fs|gs}
  ROW(1, 1, 1, 0, 67 , 0  , 0  , 0  , 0  , 0  ), // #149 {r16|m16|i8|i16}
  ROW(1, 1, 0, 0, 68 , 0  , 0  , 0  , 0  , 0  ), //      {r32|m32|i32|u32}
  ROW(1, 0, 1, 0, 69 , 0  , 0  , 0  , 0  , 0  ), //      {r64|m64|i32}
  ROW(1, 1, 0, 0, 70 , 0  , 0  , 0  , 0  , 0  ), //      {cs|ss|ds|es}
  ROW(1, 1, 1, 0, 66 , 0  , 0  , 0  , 0  , 0  ), //      {fs|gs}
  ROW(3, 1, 1, 0, 49 , 71 , 49 , 0  , 0  , 0  ), // #154 {xmm, vm32x, xmm}
  ROW(3, 1, 1, 0, 52 , 72 , 52 , 0  , 0  , 0  ), //      {ymm, vm32y, ymm}
  ROW(2, 1, 1, 0, 49 , 71 , 0  , 0  , 0  , 0  ), //      {xmm, vm32x}
  ROW(2, 1, 1, 0, 52 , 72 , 0  , 0  , 0  , 0  ), //      {ymm, vm32y}
  ROW(2, 1, 1, 0, 55 , 73 , 0  , 0  , 0  , 0  ), //      {zmm, vm32z}
  ROW(3, 1, 1, 0, 49 , 74 , 49 , 0  , 0  , 0  ), // #159 {xmm, vm64x, xmm}
  ROW(3, 1, 1, 0, 52 , 75 , 52 , 0  , 0  , 0  ), //      {ymm, vm64y, ymm}
  ROW(2, 1, 1, 0, 49 , 74 , 0  , 0  , 0  , 0  ), //      {xmm, vm64x}
  ROW(2, 1, 1, 0, 52 , 75 , 0  , 0  , 0  , 0  ), //      {ymm, vm64y}
  ROW(2, 1, 1, 0, 55 , 76 , 0  , 0  , 0  , 0  ), //      {zmm, vm64z}
  ROW(2, 1, 1, 0, 51 , 49 , 0  , 0  , 0  , 0  ), // #164 {m128|mem, xmm}
  ROW(2, 1, 1, 0, 54 , 52 , 0  , 0  , 0  , 0  ), //      {m256|mem, ymm}
  ROW(2, 1, 1, 0, 51 , 49 , 0  , 0  , 0  , 0  ), //      {m128|mem, xmm}
  ROW(2, 1, 1, 0, 54 , 52 , 0  , 0  , 0  , 0  ), //      {m256|mem, ymm}
  ROW(2, 1, 1, 0, 57 , 55 , 0  , 0  , 0  , 0  ), //      {m512|mem, zmm}
  ROW(2, 1, 1, 0, 49 , 51 , 0  , 0  , 0  , 0  ), // #169 {xmm, m128|mem}
  ROW(2, 1, 1, 0, 52 , 54 , 0  , 0  , 0  , 0  ), //      {ymm, m256|mem}
  ROW(2, 1, 1, 0, 49 , 51 , 0  , 0  , 0  , 0  ), //      {xmm, m128|mem}
  ROW(2, 1, 1, 0, 52 , 54 , 0  , 0  , 0  , 0  ), //      {ymm, m256|mem}
  ROW(2, 1, 1, 0, 55 , 57 , 0  , 0  , 0  , 0  ), //      {zmm, m512|mem}
  ROW(2, 0, 1, 0, 29 , 49 , 0  , 0  , 0  , 0  ), // #174 {r64|m64|mem, xmm}
  ROW(2, 1, 1, 0, 49 , 64 , 0  , 0  , 0  , 0  ), //      {xmm, xmm|m64|mem}
  ROW(2, 0, 1, 0, 49 , 29 , 0  , 0  , 0  , 0  ), //      {xmm, r64|m64|mem}
  ROW(2, 1, 1, 0, 31 , 49 , 0  , 0  , 0  , 0  ), //      {m64|mem, xmm}
  ROW(2, 1, 1, 0, 31 , 49 , 0  , 0  , 0  , 0  ), //      {m64|mem, xmm}
  ROW(2, 1, 1, 0, 77 , 78 , 0  , 0  , 0  , 0  ), // #179 {ds:[memBase|zsi|m8], es:[memBase|zdi|m8]}
  ROW(2, 1, 1, 0, 79 , 80 , 0  , 0  , 0  , 0  ), //      {ds:[memBase|zsi|m16], es:[memBase|zdi|m16]}
  ROW(2, 1, 1, 0, 81 , 82 , 0  , 0  , 0  , 0  ), //      {ds:[memBase|zsi|m32], es:[memBase|zdi|m32]}
  ROW(2, 0, 1, 0, 83 , 84 , 0  , 0  , 0  , 0  ), //      {ds:[memBase|zsi|m64], es:[memBase|zdi|m64]}
  ROW(3, 1, 1, 1, 1  , 2  , 85 , 0  , 0  , 0  ), // #183 {r8lo|r8hi|m8|mem, r8lo|r8hi, <al>}
  ROW(3, 1, 1, 1, 27 , 4  , 36 , 0  , 0  , 0  ), //      {r16|m16|mem, r16, <ax>}
  ROW(3, 1, 1, 1, 28 , 6  , 39 , 0  , 0  , 0  ), //      {r32|m32|mem, r32, <eax>}
  ROW(3, 0, 1, 1, 29 , 8  , 41 , 0  , 0  , 0  ), //      {r64|m64|mem, r64, <rax>}
  ROW(2, 1, 1, 0, 86 , 87 , 0  , 0  , 0  , 0  ), // #187 {k, k|m64|mem}
  ROW(2, 0, 1, 0, 86 , 8  , 0  , 0  , 0  , 0  ), //      {k, r64}
  ROW(2, 1, 1, 0, 31 , 86 , 0  , 0  , 0  , 0  ), //      {m64|mem, k}
  ROW(2, 0, 1, 0, 8  , 86 , 0  , 0  , 0  , 0  ), //      {r64, k}
  ROW(2, 1, 1, 0, 45 , 88 , 0  , 0  , 0  , 0  ), // #191 {al, ds:[memBase|zsi|m8|mem]}
  ROW(2, 1, 1, 0, 46 , 89 , 0  , 0  , 0  , 0  ), //      {ax, ds:[memBase|zsi|m16|mem]}
  ROW(2, 1, 1, 0, 47 , 90 , 0  , 0  , 0  , 0  ), //      {eax, ds:[memBase|zsi|m32|mem]}
  ROW(2, 0, 1, 0, 48 , 91 , 0  , 0  , 0  , 0  ), //      {rax, ds:[memBase|zsi|m64|mem]}
  ROW(2, 1, 1, 0, 78 , 77 , 0  , 0  , 0  , 0  ), // #195 {es:[memBase|zdi|m8], ds:[memBase|zsi|m8]}
  ROW(2, 1, 1, 0, 80 , 79 , 0  , 0  , 0  , 0  ), //      {es:[memBase|zdi|m16], ds:[memBase|zsi|m16]}
  ROW(2, 1, 1, 0, 82 , 81 , 0  , 0  , 0  , 0  ), //      {es:[memBase|zdi|m32], ds:[memBase|zsi|m32]}
  ROW(2, 0, 1, 0, 84 , 83 , 0  , 0  , 0  , 0  ), //      {es:[memBase|zdi|m64], ds:[memBase|zsi|m64]}
  ROW(2, 1, 1, 0, 45 , 92 , 0  , 0  , 0  , 0  ), // #199 {al, es:[memBase|zdi|m8|mem]}
  ROW(2, 1, 1, 0, 46 , 93 , 0  , 0  , 0  , 0  ), //      {ax, es:[memBase|zdi|m16|mem]}
  ROW(2, 1, 1, 0, 47 , 94 , 0  , 0  , 0  , 0  ), //      {eax, es:[memBase|zdi|m32|mem]}
  ROW(2, 0, 1, 0, 48 , 95 , 0  , 0  , 0  , 0  ), //      {rax, es:[memBase|zdi|m64|mem]}
  ROW(2, 1, 1, 0, 92 , 45 , 0  , 0  , 0  , 0  ), // #203 {es:[memBase|zdi|m8|mem], al}
  ROW(2, 1, 1, 0, 93 , 46 , 0  , 0  , 0  , 0  ), //      {es:[memBase|zdi|m16|mem], ax}
  ROW(2, 1, 1, 0, 94 , 47 , 0  , 0  , 0  , 0  ), //      {es:[memBase|zdi|m32|mem], eax}
  ROW(2, 0, 1, 0, 95 , 48 , 0  , 0  , 0  , 0  ), //      {es:[memBase|zdi|m64|mem], rax}
  ROW(4, 1, 1, 0, 49 , 49 , 49 , 50 , 0  , 0  ), // #207 {xmm, xmm, xmm, xmm|m128|mem}
  ROW(4, 1, 1, 0, 49 , 49 , 51 , 49 , 0  , 0  ), //      {xmm, xmm, m128|mem, xmm}
  ROW(4, 1, 1, 0, 52 , 52 , 52 , 53 , 0  , 0  ), //      {ymm, ymm, ymm, ymm|m256|mem}
  ROW(4, 1, 1, 0, 52 , 52 , 54 , 52 , 0  , 0  ), //      {ymm, ymm, m256|mem, ymm}
  ROW(3, 1, 1, 0, 49 , 71 , 49 , 0  , 0  , 0  ), // #211 {xmm, vm32x, xmm}
  ROW(3, 1, 1, 0, 52 , 71 , 52 , 0  , 0  , 0  ), //      {ymm, vm32x, ymm}
  ROW(2, 1, 1, 0, 96 , 71 , 0  , 0  , 0  , 0  ), //      {xmm|ymm, vm32x}
  ROW(2, 1, 1, 0, 55 , 72 , 0  , 0  , 0  , 0  ), //      {zmm, vm32y}
  ROW(3, 1, 1, 0, 51 , 49 , 49 , 0  , 0  , 0  ), // #215 {m128|mem, xmm, xmm}
  ROW(3, 1, 1, 0, 54 , 52 , 52 , 0  , 0  , 0  ), //      {m256|mem, ymm, ymm}
  ROW(3, 1, 1, 0, 49 , 49 , 51 , 0  , 0  , 0  ), //      {xmm, xmm, m128|mem}
  ROW(3, 1, 1, 0, 52 , 52 , 54 , 0  , 0  , 0  ), //      {ymm, ymm, m256|mem}
  ROW(2, 1, 1, 0, 31 , 49 , 0  , 0  , 0  , 0  ), // #219 {m64|mem, xmm}
  ROW(3, 1, 1, 0, 49 , 49 , 31 , 0  , 0  , 0  ), //      {xmm, xmm, m64|mem}
  ROW(2, 1, 1, 0, 31 , 49 , 0  , 0  , 0  , 0  ), //      {m64|mem, xmm}
  ROW(3, 1, 1, 0, 49 , 49 , 31 , 0  , 0  , 0  ), //      {xmm, xmm, m64|mem}
  ROW(2, 1, 1, 0, 21 , 49 , 0  , 0  , 0  , 0  ), // #223 {m16|mem, xmm}
  ROW(2, 1, 1, 0, 49 , 21 , 0  , 0  , 0  , 0  ), //      {xmm, m16|mem}
  ROW(3, 1, 1, 0, 49 , 49 , 49 , 0  , 0  , 0  ), //      {xmm, xmm, xmm}
  ROW(3, 1, 1, 0, 49 , 49 , 49 , 0  , 0  , 0  ), //      {xmm, xmm, xmm}
  ROW(5, 1, 1, 0, 49 , 49 , 50 , 49 , 97 , 0  ), // #227 {xmm, xmm, xmm|m128|mem, xmm, i4|u4}
  ROW(5, 1, 1, 0, 49 , 49 , 49 , 51 , 97 , 0  ), //      {xmm, xmm, xmm, m128|mem, i4|u4}
  ROW(5, 1, 1, 0, 52 , 52 , 53 , 52 , 97 , 0  ), //      {ymm, ymm, ymm|m256|mem, ymm, i4|u4}
  ROW(5, 1, 1, 0, 52 , 52 , 52 , 54 , 97 , 0  ), //      {ymm, ymm, ymm, m256|mem, i4|u4}
  ROW(3, 1, 1, 0, 52 , 53 , 10 , 0  , 0  , 0  ), // #231 {ymm, ymm|m256|mem, i8|u8}
  ROW(3, 1, 1, 0, 52 , 52 , 53 , 0  , 0  , 0  ), //      {ymm, ymm, ymm|m256|mem}
  ROW(3, 1, 1, 0, 55 , 55 , 60 , 0  , 0  , 0  ), //      {zmm, zmm, zmm|m512|mem|i8|u8}
  ROW(3, 1, 1, 0, 55 , 57 , 10 , 0  , 0  , 0  ), //      {zmm, m512|mem, i8|u8}
  ROW(1, 1, 0, 0, 98 , 0  , 0  , 0  , 0  , 0  ), // #235 {rel16|r16|m16|mem|r32|m32}
  ROW(1, 1, 1, 0, 99 , 0  , 0  , 0  , 0  , 0  ), // #236 {rel32}
  ROW(1, 0, 1, 0, 29 , 0  , 0  , 0  , 0  , 0  ), //      {r64|m64|mem}
  ROW(1, 1, 0, 0, 100, 0  , 0  , 0  , 0  , 0  ), // #238 {r16|r32}
  ROW(1, 1, 1, 0, 32 , 0  , 0  , 0  , 0  , 0  ), // #239 {r8lo|r8hi|m8|r16|m16|r32|m32}
  ROW(1, 0, 1, 0, 15 , 0  , 0  , 0  , 0  , 0  ), //      {r64|m64}
  ROW(1, 1, 1, 0, 101, 0  , 0  , 0  , 0  , 0  ), // #241 {m32|m64}
  ROW(2, 1, 1, 0, 102, 103, 0  , 0  , 0  , 0  ), //      {st0, st}
  ROW(2, 1, 1, 0, 103, 102, 0  , 0  , 0  , 0  ), //      {st, st0}
  ROW(1, 1, 1, 0, 104, 0  , 0  , 0  , 0  , 0  ), // #244 {rel8|rel32}
  ROW(1, 1, 0, 0, 105, 0  , 0  , 0  , 0  , 0  ), //      {rel16|r32|m32}
  ROW(1, 0, 1, 0, 15 , 0  , 0  , 0  , 0  , 0  ), //      {r64|m64}
  ROW(2, 1, 0, 0, 106, 107, 0  , 0  , 0  , 0  ), // #247 {i16, i16|i32}
  ROW(1, 1, 1, 0, 108, 0  , 0  , 0  , 0  , 0  ), //      {m32|mem|m48}
  ROW(1, 0, 1, 0, 109, 0  , 0  , 0  , 0  , 0  ), //      {m80|mem}
  ROW(2, 1, 1, 0, 4  , 30 , 0  , 0  , 0  , 0  ), // #250 {r16, m32|mem}
  ROW(2, 1, 1, 0, 6  , 110, 0  , 0  , 0  , 0  ), //      {r32, m48|mem}
  ROW(2, 0, 1, 0, 8  , 109, 0  , 0  , 0  , 0  ), //      {r64, m80|mem}
  ROW(2, 1, 1, 0, 4  , 27 , 0  , 0  , 0  , 0  ), // #253 {r16, r16|m16|mem}
  ROW(2, 1, 1, 0, 6  , 111, 0  , 0  , 0  , 0  ), //      {r32, r32|m16|mem}
  ROW(2, 0, 1, 0, 8  , 111, 0  , 0  , 0  , 0  ), //      {r64, r32|m16|mem}
  ROW(2, 1, 1, 0, 4  , 9  , 0  , 0  , 0  , 0  ), // #256 {r16, r8lo|r8hi|m8}
  ROW(2, 1, 1, 0, 6  , 112, 0  , 0  , 0  , 0  ), //      {r32, r8lo|r8hi|m8|r16|m16}
  ROW(2, 0, 1, 0, 8  , 113, 0  , 0  , 0  , 0  ), //      {r64, r8lo|m8|r16|m16}
  ROW(3, 1, 1, 0, 27 , 4  , 114, 0  , 0  , 0  ), // #259 {r16|m16|mem, r16, cl|i8|u8}
  ROW(3, 1, 1, 0, 28 , 6  , 114, 0  , 0  , 0  ), //      {r32|m32|mem, r32, cl|i8|u8}
  ROW(3, 0, 1, 0, 29 , 8  , 114, 0  , 0  , 0  ), //      {r64|m64|mem, r64, cl|i8|u8}
  ROW(3, 1, 1, 0, 49 , 49 , 50 , 0  , 0  , 0  ), // #262 {xmm, xmm, xmm|m128|mem}
  ROW(3, 1, 1, 0, 52 , 52 , 53 , 0  , 0  , 0  ), // #263 {ymm, ymm, ymm|m256|mem}
  ROW(3, 1, 1, 0, 55 , 55 , 56 , 0  , 0  , 0  ), //      {zmm, zmm, zmm|m512|mem}
  ROW(4, 1, 1, 0, 49 , 49 , 50 , 10 , 0  , 0  ), // #265 {xmm, xmm, xmm|m128|mem, i8|u8}
  ROW(4, 1, 1, 0, 52 , 52 , 53 , 10 , 0  , 0  ), // #266 {ymm, ymm, ymm|m256|mem, i8|u8}
  ROW(4, 1, 1, 0, 55 , 55 , 56 , 10 , 0  , 0  ), //      {zmm, zmm, zmm|m512|mem, i8|u8}
  ROW(4, 1, 1, 0, 115, 49 , 50 , 10 , 0  , 0  ), // #268 {xmm|k, xmm, xmm|m128|mem, i8|u8}
  ROW(4, 1, 1, 0, 116, 52 , 53 , 10 , 0  , 0  ), //      {ymm|k, ymm, ymm|m256|mem, i8|u8}
  ROW(4, 1, 1, 0, 86 , 55 , 56 , 10 , 0  , 0  ), //      {k, zmm, zmm|m512|mem, i8|u8}
  ROW(4, 1, 1, 0, 86 , 49 , 50 , 10 , 0  , 0  ), // #271 {k, xmm, xmm|m128|mem, i8|u8}
  ROW(4, 1, 1, 0, 86 , 52 , 53 , 10 , 0  , 0  ), //      {k, ymm, ymm|m256|mem, i8|u8}
  ROW(4, 1, 1, 0, 86 , 55 , 56 , 10 , 0  , 0  ), //      {k, zmm, zmm|m512|mem, i8|u8}
  ROW(2, 1, 1, 0, 50 , 49 , 0  , 0  , 0  , 0  ), // #274 {xmm|m128|mem, xmm}
  ROW(2, 1, 1, 0, 53 , 52 , 0  , 0  , 0  , 0  ), //      {ymm|m256|mem, ymm}
  ROW(2, 1, 1, 0, 56 , 55 , 0  , 0  , 0  , 0  ), //      {zmm|m512|mem, zmm}
  ROW(2, 1, 1, 0, 49 , 64 , 0  , 0  , 0  , 0  ), // #277 {xmm, xmm|m64|mem}
  ROW(2, 1, 1, 0, 52 , 50 , 0  , 0  , 0  , 0  ), //      {ymm, xmm|m128|mem}
  ROW(2, 1, 1, 0, 55 , 53 , 0  , 0  , 0  , 0  ), //      {zmm, ymm|m256|mem}
  ROW(2, 1, 1, 0, 49 , 50 , 0  , 0  , 0  , 0  ), // #280 {xmm, xmm|m128|mem}
  ROW(2, 1, 1, 0, 52 , 53 , 0  , 0  , 0  , 0  ), //      {ymm, ymm|m256|mem}
  ROW(2, 1, 1, 0, 55 , 56 , 0  , 0  , 0  , 0  ), //      {zmm, zmm|m512|mem}
  ROW(2, 1, 1, 0, 49 , 117, 0  , 0  , 0  , 0  ), // #283 {xmm, xmm|m32|mem}
  ROW(2, 1, 1, 0, 52 , 64 , 0  , 0  , 0  , 0  ), //      {ymm, xmm|m64|mem}
  ROW(2, 1, 1, 0, 55 , 50 , 0  , 0  , 0  , 0  ), //      {zmm, xmm|m128|mem}
  ROW(3, 1, 1, 0, 64 , 49 , 10 , 0  , 0  , 0  ), // #286 {xmm|m64|mem, xmm, i8|u8}
  ROW(3, 1, 1, 0, 50 , 52 , 10 , 0  , 0  , 0  ), // #287 {xmm|m128|mem, ymm, i8|u8}
  ROW(3, 1, 1, 0, 53 , 55 , 10 , 0  , 0  , 0  ), // #288 {ymm|m256|mem, zmm, i8|u8}
  ROW(3, 1, 1, 0, 49 , 118, 49 , 0  , 0  , 0  ), // #289 {xmm, vm64x|vm64y, xmm}
  ROW(2, 1, 1, 0, 49 , 118, 0  , 0  , 0  , 0  ), //      {xmm, vm64x|vm64y}
  ROW(2, 1, 1, 0, 52 , 76 , 0  , 0  , 0  , 0  ), //      {ymm, vm64z}
  ROW(3, 1, 1, 0, 49 , 50 , 10 , 0  , 0  , 0  ), // #292 {xmm, xmm|m128|mem, i8|u8}
  ROW(3, 1, 1, 0, 52 , 53 , 10 , 0  , 0  , 0  ), //      {ymm, ymm|m256|mem, i8|u8}
  ROW(3, 1, 1, 0, 55 , 56 , 10 , 0  , 0  , 0  ), //      {zmm, zmm|m512|mem, i8|u8}
  ROW(2, 1, 1, 0, 49 , 64 , 0  , 0  , 0  , 0  ), // #295 {xmm, xmm|m64|mem}
  ROW(2, 1, 1, 0, 52 , 53 , 0  , 0  , 0  , 0  ), //      {ymm, ymm|m256|mem}
  ROW(2, 1, 1, 0, 55 , 56 , 0  , 0  , 0  , 0  ), //      {zmm, zmm|m512|mem}
  ROW(4, 1, 1, 0, 86 , 86 , 49 , 50 , 0  , 0  ), // #298 {k, k, xmm, xmm|m128|mem}
  ROW(4, 1, 1, 0, 86 , 86 , 52 , 53 , 0  , 0  ), //      {k, k, ymm, ymm|m256|mem}
  ROW(4, 1, 1, 0, 86 , 86 , 55 , 56 , 0  , 0  ), //      {k, k, zmm, zmm|m512|mem}
  ROW(3, 1, 1, 0, 115, 49 , 50 , 0  , 0  , 0  ), // #301 {xmm|k, xmm, xmm|m128|mem}
  ROW(3, 1, 1, 0, 116, 52 , 53 , 0  , 0  , 0  ), //      {ymm|k, ymm, ymm|m256|mem}
  ROW(3, 1, 1, 0, 86 , 55 , 56 , 0  , 0  , 0  ), //      {k, zmm, zmm|m512|mem}
  ROW(2, 1, 1, 0, 117, 49 , 0  , 0  , 0  , 0  ), // #304 {xmm|m32|mem, xmm}
  ROW(2, 1, 1, 0, 64 , 52 , 0  , 0  , 0  , 0  ), //      {xmm|m64|mem, ymm}
  ROW(2, 1, 1, 0, 50 , 55 , 0  , 0  , 0  , 0  ), //      {xmm|m128|mem, zmm}
  ROW(2, 1, 1, 0, 64 , 49 , 0  , 0  , 0  , 0  ), // #307 {xmm|m64|mem, xmm}
  ROW(2, 1, 1, 0, 50 , 52 , 0  , 0  , 0  , 0  ), //      {xmm|m128|mem, ymm}
  ROW(2, 1, 1, 0, 53 , 55 , 0  , 0  , 0  , 0  ), //      {ymm|m256|mem, zmm}
  ROW(2, 1, 1, 0, 119, 49 , 0  , 0  , 0  , 0  ), // #310 {xmm|m16|mem, xmm}
  ROW(2, 1, 1, 0, 117, 52 , 0  , 0  , 0  , 0  ), //      {xmm|m32|mem, ymm}
  ROW(2, 1, 1, 0, 64 , 55 , 0  , 0  , 0  , 0  ), //      {xmm|m64|mem, zmm}
  ROW(2, 1, 1, 0, 49 , 119, 0  , 0  , 0  , 0  ), // #313 {xmm, xmm|m16|mem}
  ROW(2, 1, 1, 0, 52 , 117, 0  , 0  , 0  , 0  ), //      {ymm, xmm|m32|mem}
  ROW(2, 1, 1, 0, 55 , 64 , 0  , 0  , 0  , 0  ), //      {zmm, xmm|m64|mem}
  ROW(2, 1, 1, 0, 71 , 49 , 0  , 0  , 0  , 0  ), // #316 {vm32x, xmm}
  ROW(2, 1, 1, 0, 72 , 52 , 0  , 0  , 0  , 0  ), //      {vm32y, ymm}
  ROW(2, 1, 1, 0, 73 , 55 , 0  , 0  , 0  , 0  ), //      {vm32z, zmm}
  ROW(2, 1, 1, 0, 74 , 49 , 0  , 0  , 0  , 0  ), // #319 {vm64x, xmm}
  ROW(2, 1, 1, 0, 75 , 52 , 0  , 0  , 0  , 0  ), //      {vm64y, ymm}
  ROW(2, 1, 1, 0, 76 , 55 , 0  , 0  , 0  , 0  ), //      {vm64z, zmm}
  ROW(3, 1, 1, 0, 86 , 49 , 50 , 0  , 0  , 0  ), // #322 {k, xmm, xmm|m128|mem}
  ROW(3, 1, 1, 0, 86 , 52 , 53 , 0  , 0  , 0  ), //      {k, ymm, ymm|m256|mem}
  ROW(3, 1, 1, 0, 86 , 55 , 56 , 0  , 0  , 0  ), //      {k, zmm, zmm|m512|mem}
  ROW(3, 1, 1, 0, 6  , 6  , 28 , 0  , 0  , 0  ), // #325 {r32, r32, r32|m32|mem}
  ROW(3, 0, 1, 0, 8  , 8  , 29 , 0  , 0  , 0  ), //      {r64, r64, r64|m64|mem}
  ROW(3, 1, 1, 0, 6  , 28 , 6  , 0  , 0  , 0  ), // #327 {r32, r32|m32|mem, r32}
  ROW(3, 0, 1, 0, 8  , 29 , 8  , 0  , 0  , 0  ), //      {r64, r64|m64|mem, r64}
  ROW(2, 1, 0, 0, 120, 28 , 0  , 0  , 0  , 0  ), // #329 {bnd, r32|m32|mem}
  ROW(2, 0, 1, 0, 120, 29 , 0  , 0  , 0  , 0  ), //      {bnd, r64|m64|mem}
  ROW(2, 1, 1, 0, 120, 121, 0  , 0  , 0  , 0  ), // #331 {bnd, bnd|mem}
  ROW(2, 1, 1, 0, 122, 120, 0  , 0  , 0  , 0  ), //      {mem, bnd}
  ROW(2, 1, 0, 0, 4  , 30 , 0  , 0  , 0  , 0  ), // #333 {r16, m32|mem}
  ROW(2, 1, 0, 0, 6  , 31 , 0  , 0  , 0  , 0  ), //      {r32, m64|mem}
  ROW(1, 1, 1, 0, 100, 0  , 0  , 0  , 0  , 0  ), // #335 {r16|r32}
  ROW(1, 0, 1, 0, 8  , 0  , 0  , 0  , 0  , 0  ), // #336 {r64}
  ROW(3, 1, 1, 0, 30 , 6  , 6  , 0  , 0  , 0  ), // #337 {m32|mem, r32, r32}
  ROW(3, 0, 1, 0, 31 , 8  , 8  , 0  , 0  , 0  ), //      {m64|mem, r64, r64}
  ROW(2, 1, 1, 0, 6  , 32 , 0  , 0  , 0  , 0  ), // #339 {r32, r8lo|r8hi|m8|r16|m16|r32|m32}
  ROW(2, 0, 1, 0, 8  , 123, 0  , 0  , 0  , 0  ), //      {r64, r8lo|m8|r64|m64}
  ROW(2, 1, 1, 0, 6  , 64 , 0  , 0  , 0  , 0  ), // #341 {r32, xmm|m64|mem}
  ROW(2, 0, 1, 0, 8  , 64 , 0  , 0  , 0  , 0  ), //      {r64, xmm|m64|mem}
  ROW(2, 1, 1, 0, 49 , 28 , 0  , 0  , 0  , 0  ), // #343 {xmm, r32|m32|mem}
  ROW(2, 0, 1, 0, 49 , 29 , 0  , 0  , 0  , 0  ), //      {xmm, r64|m64|mem}
  ROW(2, 0, 1, 0, 49 , 29 , 0  , 0  , 0  , 0  ), // #345 {xmm, r64|m64|mem}
  ROW(2, 1, 1, 0, 49 , 28 , 0  , 0  , 0  , 0  ), //      {xmm, r32|m32|mem}
  ROW(2, 1, 1, 0, 6  , 117, 0  , 0  , 0  , 0  ), // #347 {r32, xmm|m32|mem}
  ROW(2, 0, 1, 0, 8  , 117, 0  , 0  , 0  , 0  ), //      {r64, xmm|m32|mem}
  ROW(2, 0, 1, 0, 8  , 117, 0  , 0  , 0  , 0  ), // #349 {r64, xmm|m32|mem}
  ROW(2, 1, 1, 0, 6  , 117, 0  , 0  , 0  , 0  ), //      {r32, xmm|m32|mem}
  ROW(2, 1, 0, 0, 124, 57 , 0  , 0  , 0  , 0  ), // #351 {es:[mem|m512|memBase], m512|mem}
  ROW(2, 0, 1, 0, 124, 57 , 0  , 0  , 0  , 0  ), //      {es:[mem|m512|memBase], m512|mem}
  ROW(3, 1, 1, 0, 49 , 10 , 10 , 0  , 0  , 0  ), // #353 {xmm, i8|u8, i8|u8}
  ROW(2, 1, 1, 0, 49 , 49 , 0  , 0  , 0  , 0  ), // #354 {xmm, xmm}
  ROW(0, 1, 1, 0, 0  , 0  , 0  , 0  , 0  , 0  ), // #355 {}
  ROW(1, 1, 1, 0, 103, 0  , 0  , 0  , 0  , 0  ), // #356 {st}
  ROW(0, 1, 1, 0, 0  , 0  , 0  , 0  , 0  , 0  ), // #357 {}
  ROW(1, 1, 1, 0, 125, 0  , 0  , 0  , 0  , 0  ), // #358 {m32|m64|st}
  ROW(2, 1, 1, 0, 49 , 49 , 0  , 0  , 0  , 0  ), // #359 {xmm, xmm}
  ROW(4, 1, 1, 0, 49 , 49 , 10 , 10 , 0  , 0  ), //      {xmm, xmm, i8|u8, i8|u8}
  ROW(2, 1, 0, 0, 6  , 51 , 0  , 0  , 0  , 0  ), // #361 {r32, m128|mem}
  ROW(2, 0, 1, 0, 8  , 51 , 0  , 0  , 0  , 0  ), //      {r64, m128|mem}
  ROW(2, 1, 0, 2, 39 , 126, 0  , 0  , 0  , 0  ), // #363 {<eax>, <ecx>}
  ROW(2, 0, 1, 2, 127, 126, 0  , 0  , 0  , 0  ), //      {<eax|rax>, <ecx>}
  ROW(3, 1, 0, 3, 39 , 38 , 126, 0  , 0  , 0  ), // #365 {<eax>, <edx>, <ecx>}
  ROW(3, 0, 1, 3, 127, 38 , 126, 0  , 0  , 0  ), //      {<eax|rax>, <edx>, <ecx>}
  ROW(2, 1, 0, 1, 128, 129, 0  , 0  , 0  , 0  ), // #367 {<cx|ecx>, rel8}
  ROW(2, 0, 1, 1, 130, 129, 0  , 0  , 0  , 0  ), //      {<ecx|rcx>, rel8}
  ROW(2, 1, 1, 0, 86 , 131, 0  , 0  , 0  , 0  ), // #369 {k, k|m8|mem|r32}
  ROW(2, 1, 1, 0, 132, 86 , 0  , 0  , 0  , 0  ), //      {m8|mem|r32, k}
  ROW(2, 1, 1, 0, 86 , 133, 0  , 0  , 0  , 0  ), // #371 {k, k|m32|mem|r32}
  ROW(2, 1, 1, 0, 28 , 86 , 0  , 0  , 0  , 0  ), //      {m32|mem|r32, k}
  ROW(2, 1, 1, 0, 86 , 134, 0  , 0  , 0  , 0  ), // #373 {k, k|m16|mem|r32}
  ROW(2, 1, 1, 0, 111, 86 , 0  , 0  , 0  , 0  ), //      {m16|mem|r32, k}
  ROW(2, 1, 0, 0, 4  , 30 , 0  , 0  , 0  , 0  ), // #375 {r16, m32|mem}
  ROW(2, 1, 0, 0, 6  , 110, 0  , 0  , 0  , 0  ), //      {r32, m48|mem}
  ROW(2, 1, 1, 0, 100, 135, 0  , 0  , 0  , 0  ), // #377 {r16|r32, mem|m8|m16|m32|m48|m64|m80|m128|m256|m512|m1024}
  ROW(2, 0, 1, 0, 8  , 135, 0  , 0  , 0  , 0  ), //      {r64, mem|m8|m16|m32|m48|m64|m80|m128|m256|m512|m1024}
  ROW(1, 1, 1, 0, 6  , 0  , 0  , 0  , 0  , 0  ), // #379 {r32}
  ROW(1, 0, 1, 0, 8  , 0  , 0  , 0  , 0  , 0  ), //      {r64}
  ROW(3, 1, 1, 0, 6  , 28 , 14 , 0  , 0  , 0  ), // #381 {r32, r32|m32|mem, i32|u32}
  ROW(3, 0, 1, 0, 8  , 28 , 14 , 0  , 0  , 0  ), //      {r64, r32|m32|mem, i32|u32}
  ROW(2, 1, 1, 0, 63 , 28 , 0  , 0  , 0  , 0  ), // #383 {mm|xmm, r32|m32|mem}
  ROW(2, 1, 1, 0, 28 , 63 , 0  , 0  , 0  , 0  ), //      {r32|m32|mem, mm|xmm}
  ROW(2, 1, 1, 0, 124, 57 , 0  , 0  , 0  , 0  ), // #385 {es:[mem|m512|memBase], m512|mem}
  ROW(2, 1, 1, 0, 124, 57 , 0  , 0  , 0  , 0  ), //      {es:[mem|m512|memBase], m512|mem}
  ROW(2, 1, 1, 0, 49 , 64 , 0  , 0  , 0  , 0  ), // #387 {xmm, xmm|m64|mem}
  ROW(2, 1, 1, 0, 31 , 49 , 0  , 0  , 0  , 0  ), //      {m64|mem, xmm}
  ROW(2, 1, 1, 0, 49 , 117, 0  , 0  , 0  , 0  ), // #389 {xmm, xmm|m32|mem}
  ROW(2, 1, 1, 0, 30 , 49 , 0  , 0  , 0  , 0  ), //      {m32|mem, xmm}
  ROW(2, 0, 1, 0, 4  , 27 , 0  , 0  , 0  , 0  ), // #391 {r16, r16|m16|mem}
  ROW(2, 0, 1, 0, 136, 28 , 0  , 0  , 0  , 0  ), //      {r32|r64, r32|m32|mem}
  ROW(4, 1, 1, 1, 6  , 6  , 28 , 38 , 0  , 0  ), // #393 {r32, r32, r32|m32|mem, <edx>}
  ROW(4, 0, 1, 1, 8  , 8  , 29 , 40 , 0  , 0  ), //      {r64, r64, r64|m64|mem, <rdx>}
  ROW(2, 1, 1, 0, 61 , 62 , 0  , 0  , 0  , 0  ), // #395 {mm, mm|m64|mem}
  ROW(2, 1, 1, 0, 49 , 50 , 0  , 0  , 0  , 0  ), //      {xmm, xmm|m128|mem}
  ROW(3, 1, 1, 0, 61 , 62 , 10 , 0  , 0  , 0  ), // #397 {mm, mm|m64|mem, i8|u8}
  ROW(3, 1, 1, 0, 49 , 50 , 10 , 0  , 0  , 0  ), //      {xmm, xmm|m128|mem, i8|u8}
  ROW(3, 1, 1, 0, 6  , 63 , 10 , 0  , 0  , 0  ), // #399 {r32, mm|xmm, i8|u8}
  ROW(3, 1, 1, 0, 21 , 49 , 10 , 0  , 0  , 0  ), //      {m16|mem, xmm, i8|u8}
  ROW(2, 1, 1, 0, 61 , 137, 0  , 0  , 0  , 0  ), // #401 {mm, i8|u8|mm|m64|mem}
  ROW(2, 1, 1, 0, 49 , 58 , 0  , 0  , 0  , 0  ), //      {xmm, i8|u8|xmm|m128|mem}
  ROW(1, 1, 1, 0, 28 , 0  , 0  , 0  , 0  , 0  ), // #403 {r32|m32|mem}
  ROW(1, 0, 1, 0, 29 , 0  , 0  , 0  , 0  , 0  ), //      {r64|m64|mem}
  ROW(2, 1, 1, 0, 61 , 138, 0  , 0  , 0  , 0  ), // #405 {mm, mm|m32|mem}
  ROW(2, 1, 1, 0, 49 , 50 , 0  , 0  , 0  , 0  ), //      {xmm, xmm|m128|mem}
  ROW(2, 1, 1, 0, 32 , 114, 0  , 0  , 0  , 0  ), // #407 {r8lo|r8hi|m8|r16|m16|r32|m32, cl|i8|u8}
  ROW(2, 0, 1, 0, 15 , 114, 0  , 0  , 0  , 0  ), //      {r64|m64, cl|i8|u8}
  ROW(1, 1, 0, 0, 6  , 0  , 0  , 0  , 0  , 0  ), // #409 {r32}
  ROW(1, 0, 1, 0, 8  , 0  , 0  , 0  , 0  , 0  ), //      {r64}
  ROW(0, 1, 1, 0, 0  , 0  , 0  , 0  , 0  , 0  ), // #411 {}
  ROW(1, 1, 1, 0, 139, 0  , 0  , 0  , 0  , 0  ), //      {u16}
  ROW(3, 1, 1, 0, 6  , 28 , 10 , 0  , 0  , 0  ), // #413 {r32, r32|m32|mem, i8|u8}
  ROW(3, 0, 1, 0, 8  , 29 , 10 , 0  , 0  , 0  ), //      {r64, r64|m64|mem, i8|u8}
  ROW(1, 1, 1, 0, 140, 0  , 0  , 0  , 0  , 0  ), // #415 {r16|m16|mem|r32}
  ROW(1, 0, 1, 0, 141, 0  , 0  , 0  , 0  , 0  ), //      {r64|m16|mem}
  ROW(1, 1, 0, 0, 142, 0  , 0  , 0  , 0  , 0  ), // #417 {ds:[mem|memBase]}
  ROW(1, 0, 1, 0, 142, 0  , 0  , 0  , 0  , 0  ), //      {ds:[mem|memBase]}
  ROW(4, 1, 1, 0, 49 , 49 , 50 , 49 , 0  , 0  ), // #419 {xmm, xmm, xmm|m128|mem, xmm}
  ROW(4, 1, 1, 0, 52 , 52 , 53 , 52 , 0  , 0  ), //      {ymm, ymm, ymm|m256|mem, ymm}
  ROW(2, 1, 1, 0, 49 , 143, 0  , 0  , 0  , 0  ), // #421 {xmm, xmm|m128|ymm|m256}
  ROW(2, 1, 1, 0, 52 , 56 , 0  , 0  , 0  , 0  ), //      {ymm, zmm|m512|mem}
  ROW(2, 1, 1, 0, 6  , 119, 0  , 0  , 0  , 0  ), // #423 {r32, xmm|m16|mem}
  ROW(2, 0, 1, 0, 8  , 119, 0  , 0  , 0  , 0  ), //      {r64, xmm|m16|mem}
  ROW(3, 1, 1, 0, 49 , 49 , 28 , 0  , 0  , 0  ), // #425 {xmm, xmm, r32|m32|mem}
  ROW(3, 0, 1, 0, 49 , 49 , 29 , 0  , 0  , 0  ), //      {xmm, xmm, r64|m64|mem}
  ROW(3, 1, 1, 0, 49 , 49 , 13 , 0  , 0  , 0  ), // #427 {xmm, xmm, r32|m32}
  ROW(3, 0, 1, 0, 49 , 49 , 15 , 0  , 0  , 0  ), //      {xmm, xmm, r64|m64}
  ROW(4, 1, 1, 0, 49 , 49 , 49 , 64 , 0  , 0  ), // #429 {xmm, xmm, xmm, xmm|m64|mem}
  ROW(4, 1, 1, 0, 49 , 49 , 31 , 49 , 0  , 0  ), //      {xmm, xmm, m64|mem, xmm}
  ROW(4, 1, 1, 0, 49 , 49 , 49 , 117, 0  , 0  ), // #431 {xmm, xmm, xmm, xmm|m32|mem}
  ROW(4, 1, 1, 0, 49 , 49 , 30 , 49 , 0  , 0  ), //      {xmm, xmm, m32|mem, xmm}
  ROW(4, 1, 1, 0, 52 , 52 , 50 , 10 , 0  , 0  ), // #433 {ymm, ymm, xmm|m128|mem, i8|u8}
  ROW(4, 1, 1, 0, 55 , 55 , 50 , 10 , 0  , 0  ), //      {zmm, zmm, xmm|m128|mem, i8|u8}
  ROW(1, 1, 0, 1, 39 , 0  , 0  , 0  , 0  , 0  ), // #435 {<eax>}
  ROW(1, 0, 1, 1, 41 , 0  , 0  , 0  , 0  , 0  ), // #436 {<rax>}
  ROW(0, 1, 1, 0, 0  , 0  , 0  , 0  , 0  , 0  ), // #437 {}
  ROW(0, 1, 1, 0, 0  , 0  , 0  , 0  , 0  , 0  ), //      {}
  ROW(2, 1, 1, 0, 28 , 49 , 0  , 0  , 0  , 0  ), // #439 {r32|m32|mem, xmm}
  ROW(2, 1, 1, 0, 49 , 28 , 0  , 0  , 0  , 0  ), //      {xmm, r32|m32|mem}
  ROW(2, 1, 1, 0, 111, 49 , 0  , 0  , 0  , 0  ), // #441 {r32|m16|mem, xmm}
  ROW(2, 1, 1, 0, 49 , 111, 0  , 0  , 0  , 0  ), //      {xmm, r32|m16|mem}
  ROW(2, 1, 0, 0, 28 , 6  , 0  , 0  , 0  , 0  ), // #443 {r32|m32|mem, r32}
  ROW(2, 0, 1, 0, 29 , 8  , 0  , 0  , 0  , 0  ), //      {r64|m64|mem, r64}
  ROW(2, 1, 0, 0, 6  , 28 , 0  , 0  , 0  , 0  ), // #445 {r32, r32|m32|mem}
  ROW(2, 0, 1, 0, 8  , 29 , 0  , 0  , 0  , 0  ), //      {r64, r64|m64|mem}
  ROW(2, 1, 1, 0, 144, 64 , 0  , 0  , 0  , 0  ), // #447 {xmm|ymm|zmm, xmm|m64|mem}
  ROW(2, 0, 1, 0, 144, 8  , 0  , 0  , 0  , 0  ), //      {xmm|ymm|zmm, r64}
  ROW(3, 1, 1, 0, 49 , 49 , 58 , 0  , 0  , 0  ), // #449 {xmm, xmm, xmm|m128|mem|i8|u8}
  ROW(3, 1, 1, 0, 49 , 51 , 145, 0  , 0  , 0  ), //      {xmm, m128|mem, i8|u8|xmm}
  ROW(2, 1, 1, 0, 71 , 96 , 0  , 0  , 0  , 0  ), // #451 {vm32x, xmm|ymm}
  ROW(2, 1, 1, 0, 72 , 55 , 0  , 0  , 0  , 0  ), //      {vm32y, zmm}
  ROW(2, 1, 1, 0, 118, 49 , 0  , 0  , 0  , 0  ), // #453 {vm64x|vm64y, xmm}
  ROW(2, 1, 1, 0, 76 , 52 , 0  , 0  , 0  , 0  ), //      {vm64z, ymm}
  ROW(3, 1, 1, 0, 49 , 49 , 50 , 0  , 0  , 0  ), // #455 {xmm, xmm, xmm|m128|mem}
  ROW(3, 1, 1, 0, 49 , 51 , 49 , 0  , 0  , 0  ), //      {xmm, m128|mem, xmm}
  ROW(1, 1, 0, 1, 36 , 0  , 0  , 0  , 0  , 0  ), // #457 {<ax>}
  ROW(2, 1, 0, 1, 36 , 10 , 0  , 0  , 0  , 0  ), // #458 {<ax>, i8|u8}
  ROW(2, 1, 0, 0, 27 , 4  , 0  , 0  , 0  , 0  ), // #459 {r16|m16|mem, r16}
  ROW(3, 1, 1, 1, 49 , 50 , 146, 0  , 0  , 0  ), // #460 {xmm, xmm|m128|mem, <xmm0>}
  ROW(2, 1, 1, 0, 120, 147, 0  , 0  , 0  , 0  ), // #461 {bnd, mib}
  ROW(2, 1, 1, 0, 120, 122, 0  , 0  , 0  , 0  ), // #462 {bnd, mem}
  ROW(2, 1, 1, 0, 147, 120, 0  , 0  , 0  , 0  ), // #463 {mib, bnd}
  ROW(1, 1, 1, 1, 36 , 0  , 0  , 0  , 0  , 0  ), // #464 {<ax>}
  ROW(2, 1, 1, 2, 38 , 39 , 0  , 0  , 0  , 0  ), // #465 {<edx>, <eax>}
  ROW(1, 1, 1, 0, 122, 0  , 0  , 0  , 0  , 0  ), // #466 {mem}
  ROW(1, 1, 1, 0, 31 , 0  , 0  , 0  , 0  , 0  ), // #467 {m64|mem}
  ROW(0, 0, 1, 0, 0  , 0  , 0  , 0  , 0  , 0  ), // #468 {}
  ROW(1, 1, 1, 1, 148, 0  , 0  , 0  , 0  , 0  ), // #469 {<ds:[mem|m512|memBase|zax]>}
  ROW(3, 1, 1, 0, 49 , 64 , 10 , 0  , 0  , 0  ), // #470 {xmm, xmm|m64|mem, i8|u8}
  ROW(3, 1, 1, 0, 49 , 117, 10 , 0  , 0  , 0  ), // #471 {xmm, xmm|m32|mem, i8|u8}
  ROW(5, 0, 1, 4, 51 , 40 , 41 , 149, 150, 0  ), // #472 {m128|mem, <rdx>, <rax>, <rcx>, <rbx>}
  ROW(5, 1, 1, 4, 31 , 38 , 39 , 126, 151, 0  ), // #473 {m64|mem, <edx>, <eax>, <ecx>, <ebx>}
  ROW(4, 1, 1, 4, 39 , 151, 126, 38 , 0  , 0  ), // #474 {<eax>, <ebx>, <ecx>, <edx>}
  ROW(2, 0, 1, 2, 40 , 41 , 0  , 0  , 0  , 0  ), // #475 {<rdx>, <rax>}
  ROW(2, 1, 1, 0, 61 , 50 , 0  , 0  , 0  , 0  ), // #476 {mm, xmm|m128|mem}
  ROW(2, 1, 1, 0, 49 , 62 , 0  , 0  , 0  , 0  ), // #477 {xmm, mm|m64|mem}
  ROW(2, 1, 1, 0, 61 , 64 , 0  , 0  , 0  , 0  ), // #478 {mm, xmm|m64|mem}
  ROW(2, 1, 1, 2, 37 , 36 , 0  , 0  , 0  , 0  ), // #479 {<dx>, <ax>}
  ROW(1, 1, 1, 1, 39 , 0  , 0  , 0  , 0  , 0  ), // #480 {<eax>}
  ROW(2, 1, 1, 0, 12 , 10 , 0  , 0  , 0  , 0  ), // #481 {i16|u16, i8|u8}
  ROW(3, 1, 1, 0, 28 , 49 , 10 , 0  , 0  , 0  ), // #482 {r32|m32|mem, xmm, i8|u8}
  ROW(1, 1, 1, 0, 109, 0  , 0  , 0  , 0  , 0  ), // #483 {m80|mem}
  ROW(1, 1, 1, 0, 152, 0  , 0  , 0  , 0  , 0  ), // #484 {m16|m32}
  ROW(1, 1, 1, 0, 153, 0  , 0  , 0  , 0  , 0  ), // #485 {m16|m32|m64}
  ROW(1, 1, 1, 0, 154, 0  , 0  , 0  , 0  , 0  ), // #486 {m32|m64|m80|st}
  ROW(1, 1, 1, 0, 21 , 0  , 0  , 0  , 0  , 0  ), // #487 {m16|mem}
  ROW(1, 1, 1, 0, 155, 0  , 0  , 0  , 0  , 0  ), // #488 {ax|m16|mem}
  ROW(1, 0, 1, 0, 122, 0  , 0  , 0  , 0  , 0  ), // #489 {mem}
  ROW(2, 1, 1, 1, 10 , 39 , 0  , 0  , 0  , 0  ), // #490 {i8|u8, <eax>}
  ROW(2, 1, 1, 0, 156, 157, 0  , 0  , 0  , 0  ), // #491 {al|ax|eax, i8|u8|dx}
  ROW(2, 1, 1, 0, 158, 159, 0  , 0  , 0  , 0  ), // #492 {es:[memBase|zdi|m8|m16|m32], dx}
  ROW(1, 1, 1, 0, 10 , 0  , 0  , 0  , 0  , 0  ), // #493 {i8|u8}
  ROW(0, 1, 0, 0, 0  , 0  , 0  , 0  , 0  , 0  ), // #494 {}
  ROW(3, 1, 1, 0, 86 , 86 , 86 , 0  , 0  , 0  ), // #495 {k, k, k}
  ROW(2, 1, 1, 0, 86 , 86 , 0  , 0  , 0  , 0  ), // #496 {k, k}
  ROW(3, 1, 1, 0, 86 , 86 , 10 , 0  , 0  , 0  ), // #497 {k, k, i8|u8}
  ROW(1, 1, 1, 1, 160, 0  , 0  , 0  , 0  , 0  ), // #498 {<ah>}
  ROW(1, 1, 1, 0, 30 , 0  , 0  , 0  , 0  , 0  ), // #499 {m32|mem}
  ROW(1, 0, 1, 0, 57 , 0  , 0  , 0  , 0  , 0  ), // #500 {m512|mem}
  ROW(1, 1, 1, 0, 27 , 0  , 0  , 0  , 0  , 0  ), // #501 {r16|m16|mem}
  ROW(3, 1, 1, 1, 49 , 49 , 161, 0  , 0  , 0  ), // #502 {xmm, xmm, <ds:[mem|m128|memBase|zdi]>}
  ROW(3, 1, 1, 1, 61 , 61 , 162, 0  , 0  , 0  ), // #503 {mm, mm, <ds:[mem|m64|memBase|zdi]>}
  ROW(3, 1, 1, 3, 163, 126, 38 , 0  , 0  , 0  ), // #504 {<ds:[mem|memBase|zax]>, <ecx>, <edx>}
  ROW(2, 1, 1, 0, 61 , 49 , 0  , 0  , 0  , 0  ), // #505 {mm, xmm}
  ROW(2, 1, 1, 0, 6  , 49 , 0  , 0  , 0  , 0  ), // #506 {r32, xmm}
  ROW(2, 1, 1, 0, 31 , 61 , 0  , 0  , 0  , 0  ), // #507 {m64|mem, mm}
  ROW(2, 1, 1, 0, 49 , 61 , 0  , 0  , 0  , 0  ), // #508 {xmm, mm}
  ROW(2, 1, 1, 2, 39 , 126, 0  , 0  , 0  , 0  ), // #509 {<eax>, <ecx>}
  ROW(3, 1, 1, 3, 39 , 126, 151, 0  , 0  , 0  ), // #510 {<eax>, <ecx>, <ebx>}
  ROW(2, 1, 1, 0, 164, 156, 0  , 0  , 0  , 0  ), // #511 {u8|dx, al|ax|eax}
  ROW(2, 1, 1, 0, 159, 165, 0  , 0  , 0  , 0  ), // #512 {dx, ds:[memBase|zsi|m8|m16|m32]}
  ROW(6, 1, 1, 3, 49 , 50 , 10 , 126, 39 , 38 ), // #513 {xmm, xmm|m128|mem, i8|u8, <ecx>, <eax>, <edx>}
  ROW(6, 1, 1, 3, 49 , 50 , 10 , 146, 39 , 38 ), // #514 {xmm, xmm|m128|mem, i8|u8, <xmm0>, <eax>, <edx>}
  ROW(4, 1, 1, 1, 49 , 50 , 10 , 126, 0  , 0  ), // #515 {xmm, xmm|m128|mem, i8|u8, <ecx>}
  ROW(4, 1, 1, 1, 49 , 50 , 10 , 146, 0  , 0  ), // #516 {xmm, xmm|m128|mem, i8|u8, <xmm0>}
  ROW(3, 1, 1, 0, 132, 49 , 10 , 0  , 0  , 0  ), // #517 {r32|m8|mem, xmm, i8|u8}
  ROW(3, 0, 1, 0, 29 , 49 , 10 , 0  , 0  , 0  ), // #518 {r64|m64|mem, xmm, i8|u8}
  ROW(3, 1, 1, 0, 49 , 132, 10 , 0  , 0  , 0  ), // #519 {xmm, r32|m8|mem, i8|u8}
  ROW(3, 1, 1, 0, 49 , 28 , 10 , 0  , 0  , 0  ), // #520 {xmm, r32|m32|mem, i8|u8}
  ROW(3, 0, 1, 0, 49 , 29 , 10 , 0  , 0  , 0  ), // #521 {xmm, r64|m64|mem, i8|u8}
  ROW(3, 1, 1, 0, 63 , 111, 10 , 0  , 0  , 0  ), // #522 {mm|xmm, r32|m16|mem, i8|u8}
  ROW(2, 1, 1, 0, 6  , 63 , 0  , 0  , 0  , 0  ), // #523 {r32, mm|xmm}
  ROW(2, 1, 1, 0, 49 , 10 , 0  , 0  , 0  , 0  ), // #524 {xmm, i8|u8}
  ROW(1, 0, 1, 0, 136, 0  , 0  , 0  , 0  , 0  ), // #525 {r32|r64}
  ROW(3, 1, 1, 3, 38 , 39 , 126, 0  , 0  , 0  ), // #526 {<edx>, <eax>, <ecx>}
  ROW(1, 1, 1, 0, 1  , 0  , 0  , 0  , 0  , 0  ), // #527 {r8lo|r8hi|m8|mem}
  ROW(3, 0, 1, 0, 166, 166, 166, 0  , 0  , 0  ), // #528 {tmm, tmm, tmm}
  ROW(2, 0, 1, 0, 166, 167, 0  , 0  , 0  , 0  ), // #529 {tmm, tmem}
  ROW(2, 0, 1, 0, 167, 166, 0  , 0  , 0  , 0  ), // #530 {tmem, tmm}
  ROW(1, 0, 1, 0, 166, 0  , 0  , 0  , 0  , 0  ), // #531 {tmm}
  ROW(3, 1, 1, 2, 6  , 38 , 39 , 0  , 0  , 0  ), // #532 {r32, <edx>, <eax>}
  ROW(6, 1, 1, 0, 55 , 55 , 55 , 55 , 55 , 51 ), // #533 {zmm, zmm, zmm, zmm, zmm, m128|mem}
  ROW(6, 1, 1, 0, 49 , 49 , 49 , 49 , 49 , 51 ), // #534 {xmm, xmm, xmm, xmm, xmm, m128|mem}
  ROW(3, 1, 1, 0, 49 , 49 , 64 , 0  , 0  , 0  ), // #535 {xmm, xmm, xmm|m64|mem}
  ROW(3, 1, 1, 0, 49 , 49 , 119, 0  , 0  , 0  ), // #536 {xmm, xmm, xmm|m16|mem}
  ROW(3, 1, 1, 0, 49 , 49 , 117, 0  , 0  , 0  ), // #537 {xmm, xmm, xmm|m32|mem}
  ROW(2, 1, 1, 0, 96 , 21 , 0  , 0  , 0  , 0  ), // #538 {xmm|ymm, m16|mem}
  ROW(2, 1, 1, 0, 52 , 51 , 0  , 0  , 0  , 0  ), // #539 {ymm, m128|mem}
  ROW(2, 1, 1, 0, 168, 64 , 0  , 0  , 0  , 0  ), // #540 {ymm|zmm, xmm|m64|mem}
  ROW(2, 1, 1, 0, 168, 51 , 0  , 0  , 0  , 0  ), // #541 {ymm|zmm, m128|mem}
  ROW(2, 1, 1, 0, 55 , 54 , 0  , 0  , 0  , 0  ), // #542 {zmm, m256|mem}
  ROW(2, 1, 1, 0, 144, 117, 0  , 0  , 0  , 0  ), // #543 {xmm|ymm|zmm, m32|mem|xmm}
  ROW(4, 1, 1, 0, 115, 49 , 64 , 10 , 0  , 0  ), // #544 {xmm|k, xmm, xmm|m64|mem, i8|u8}
  ROW(4, 1, 1, 0, 86 , 49 , 119, 10 , 0  , 0  ), // #545 {k, xmm, xmm|m16|mem, i8|u8}
  ROW(4, 1, 1, 0, 115, 49 , 117, 10 , 0  , 0  ), // #546 {xmm|k, xmm, xmm|m32|mem, i8|u8}
  ROW(2, 1, 1, 0, 49 , 169, 0  , 0  , 0  , 0  ), // #547 {xmm, xmm|m128|ymm|m256|zmm|m512}
  ROW(3, 1, 1, 0, 50 , 168, 10 , 0  , 0  , 0  ), // #548 {xmm|m128|mem, ymm|zmm, i8|u8}
  ROW(4, 1, 1, 0, 49 , 49 , 64 , 10 , 0  , 0  ), // #549 {xmm, xmm, xmm|m64|mem, i8|u8}
  ROW(4, 1, 1, 0, 49 , 49 , 117, 10 , 0  , 0  ), // #550 {xmm, xmm, xmm|m32|mem, i8|u8}
  ROW(3, 1, 1, 0, 86 , 169, 10 , 0  , 0  , 0  ), // #551 {k, xmm|m128|ymm|m256|zmm|m512, i8|u8}
  ROW(3, 1, 1, 0, 86 , 64 , 10 , 0  , 0  , 0  ), // #552 {k, xmm|m64|mem, i8|u8}
  ROW(3, 1, 1, 0, 86 , 119, 10 , 0  , 0  , 0  ), // #553 {k, xmm|m16|mem, i8|u8}
  ROW(3, 1, 1, 0, 86 , 117, 10 , 0  , 0  , 0  ), // #554 {k, xmm|m32|mem, i8|u8}
  ROW(1, 1, 1, 0, 72 , 0  , 0  , 0  , 0  , 0  ), // #555 {vm32y}
  ROW(1, 1, 1, 0, 73 , 0  , 0  , 0  , 0  , 0  ), // #556 {vm32z}
  ROW(1, 1, 1, 0, 76 , 0  , 0  , 0  , 0  , 0  ), // #557 {vm64z}
  ROW(4, 1, 1, 0, 49 , 49 , 119, 10 , 0  , 0  ), // #558 {xmm, xmm, xmm|m16|mem, i8|u8}
  ROW(4, 1, 1, 0, 55 , 55 , 53 , 10 , 0  , 0  ), // #559 {zmm, zmm, ymm|m256|mem, i8|u8}
  ROW(2, 1, 1, 0, 6  , 96 , 0  , 0  , 0  , 0  ), // #560 {r32, xmm|ymm}
  ROW(2, 1, 1, 0, 144, 170, 0  , 0  , 0  , 0  ), // #561 {xmm|ymm|zmm, xmm|m8|mem|r32}
  ROW(2, 1, 1, 0, 144, 171, 0  , 0  , 0  , 0  ), // #562 {xmm|ymm|zmm, xmm|m32|mem|r32}
  ROW(2, 1, 1, 0, 144, 86 , 0  , 0  , 0  , 0  ), // #563 {xmm|ymm|zmm, k}
  ROW(2, 1, 1, 0, 144, 172, 0  , 0  , 0  , 0  ), // #564 {xmm|ymm|zmm, xmm|m16|mem|r32}
  ROW(3, 1, 1, 0, 111, 49 , 10 , 0  , 0  , 0  ), // #565 {r32|m16|mem, xmm, i8|u8}
  ROW(4, 1, 1, 0, 49 , 49 , 132, 10 , 0  , 0  ), // #566 {xmm, xmm, r32|m8|mem, i8|u8}
  ROW(4, 1, 1, 0, 49 , 49 , 28 , 10 , 0  , 0  ), // #567 {xmm, xmm, r32|m32|mem, i8|u8}
  ROW(4, 0, 1, 0, 49 , 49 , 29 , 10 , 0  , 0  ), // #568 {xmm, xmm, r64|m64|mem, i8|u8}
  ROW(4, 1, 1, 0, 49 , 49 , 111, 10 , 0  , 0  ), // #569 {xmm, xmm, r32|m16|mem, i8|u8}
  ROW(2, 1, 1, 0, 86 , 144, 0  , 0  , 0  , 0  ), // #570 {k, xmm|ymm|zmm}
  ROW(2, 1, 1, 0, 52 , 49 , 0  , 0  , 0  , 0  ), // #571 {ymm, xmm}
  ROW(2, 1, 1, 0, 52 , 52 , 0  , 0  , 0  , 0  ), // #572 {ymm, ymm}
  ROW(3, 1, 1, 0, 52 , 52 , 49 , 0  , 0  , 0  ), // #573 {ymm, ymm, xmm}
  ROW(3, 1, 1, 2, 122, 38 , 39 , 0  , 0  , 0  ), // #574 {mem, <edx>, <eax>}
  ROW(3, 0, 1, 2, 122, 38 , 39 , 0  , 0  , 0  )  // #575 {mem, <edx>, <eax>}
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
  ROW(F(RegGpq) | F(Mem64), 0x00),
  ROW(F(ImmI32), 0x00),
  ROW(F(RegSReg) | F(RegCReg) | F(RegDReg) | F(MemUnspecified) | F(Mem64) | F(ImmI64) | F(ImmU64), 0x00),
  ROW(F(MemUnspecified) | F(Mem8), 0x00),
  ROW(F(RegSReg) | F(MemUnspecified) | F(Mem16), 0x00),
  ROW(F(RegSReg) | F(MemUnspecified) | F(Mem32), 0x00),
  ROW(F(MemUnspecified) | F(Mem16), 0x00),
  ROW(F(RegSReg), 0x00),
  ROW(F(RegCReg) | F(RegDReg), 0x00),
  ROW(F(ImmI8) | F(ImmI32), 0x00),
  ROW(F(RegGpw) | F(RegGpd) | F(Mem16) | F(Mem32), 0x00),
  ROW(F(ImmI8), 0x00),
  ROW(F(RegGpw) | F(MemUnspecified) | F(Mem16), 0x00),
  ROW(F(RegGpd) | F(MemUnspecified) | F(Mem32), 0x00),
  ROW(F(RegGpq) | F(MemUnspecified) | F(Mem64), 0x00),
  ROW(F(MemUnspecified) | F(Mem32), 0x00),
  ROW(F(MemUnspecified) | F(Mem64), 0x00),
  ROW(F(RegGpbLo) | F(RegGpbHi) | F(RegGpw) | F(RegGpd) | F(Mem8) | F(Mem16) | F(Mem32), 0x00),
  ROW(F(RegGpq) | F(MemUnspecified) | F(Mem64) | F(ImmI8) | F(ImmU8) | F(ImmI32) | F(ImmU32), 0x00),
  ROW(F(Mem64), 0x00),
  ROW(F(ImmI8) | F(ImmU8) | F(ImmI32), 0x00),
  ROW(F(RegGpw) | F(FlagImplicit), 0x01),
  ROW(F(RegGpw) | F(FlagImplicit), 0x04),
  ROW(F(RegGpd) | F(FlagImplicit), 0x04),
  ROW(F(RegGpd) | F(FlagImplicit), 0x01),
  ROW(F(RegGpq) | F(FlagImplicit), 0x04),
  ROW(F(RegGpq) | F(FlagImplicit), 0x01),
  ROW(F(ImmI8) | F(ImmI16) | F(ImmU16), 0x00),
  ROW(F(ImmI8) | F(ImmI32) | F(ImmU32), 0x00),
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
  ROW(F(RegMm) | F(MemUnspecified) | F(Mem64), 0x00),
  ROW(F(RegXmm) | F(RegMm), 0x00),
  ROW(F(RegXmm) | F(MemUnspecified) | F(Mem64), 0x00),
  ROW(F(RegSReg), 0x1A),
  ROW(F(RegSReg), 0x60),
  ROW(F(RegGpw) | F(Mem16) | F(ImmI8) | F(ImmI16), 0x00),
  ROW(F(RegGpd) | F(Mem32) | F(ImmI32) | F(ImmU32), 0x00),
  ROW(F(RegGpq) | F(Mem64) | F(ImmI32), 0x00),
  ROW(F(RegSReg), 0x1E),
  ROW(F(Vm32x), 0x00),
  ROW(F(Vm32y), 0x00),
  ROW(F(Vm32z), 0x00),
  ROW(F(Vm64x), 0x00),
  ROW(F(Vm64y), 0x00),
  ROW(F(Vm64z), 0x00),
  ROW(F(Mem8) | F(FlagMemBase) | F(FlagMemDs), 0x40),
  ROW(F(Mem8) | F(FlagMemBase) | F(FlagMemEs), 0x80),
  ROW(F(Mem16) | F(FlagMemBase) | F(FlagMemDs), 0x40),
  ROW(F(Mem16) | F(FlagMemBase) | F(FlagMemEs), 0x80),
  ROW(F(Mem32) | F(FlagMemBase) | F(FlagMemDs), 0x40),
  ROW(F(Mem32) | F(FlagMemBase) | F(FlagMemEs), 0x80),
  ROW(F(Mem64) | F(FlagMemBase) | F(FlagMemDs), 0x40),
  ROW(F(Mem64) | F(FlagMemBase) | F(FlagMemEs), 0x80),
  ROW(F(RegGpbLo) | F(FlagImplicit), 0x01),
  ROW(F(RegKReg), 0x00),
  ROW(F(RegKReg) | F(MemUnspecified) | F(Mem64), 0x00),
  ROW(F(MemUnspecified) | F(Mem8) | F(FlagMemBase) | F(FlagMemDs), 0x40),
  ROW(F(MemUnspecified) | F(Mem16) | F(FlagMemBase) | F(FlagMemDs), 0x40),
  ROW(F(MemUnspecified) | F(Mem32) | F(FlagMemBase) | F(FlagMemDs), 0x40),
  ROW(F(MemUnspecified) | F(Mem64) | F(FlagMemBase) | F(FlagMemDs), 0x40),
  ROW(F(MemUnspecified) | F(Mem8) | F(FlagMemBase) | F(FlagMemEs), 0x80),
  ROW(F(MemUnspecified) | F(Mem16) | F(FlagMemBase) | F(FlagMemEs), 0x80),
  ROW(F(MemUnspecified) | F(Mem32) | F(FlagMemBase) | F(FlagMemEs), 0x80),
  ROW(F(MemUnspecified) | F(Mem64) | F(FlagMemBase) | F(FlagMemEs), 0x80),
  ROW(F(RegXmm) | F(RegYmm), 0x00),
  ROW(F(ImmI4) | F(ImmU4), 0x00),
  ROW(F(RegGpw) | F(RegGpd) | F(MemUnspecified) | F(Mem16) | F(Mem32) | F(ImmI32) | F(ImmI64) | F(Rel32), 0x00),
  ROW(F(ImmI32) | F(ImmI64) | F(Rel32), 0x00),
  ROW(F(RegGpw) | F(RegGpd), 0x00),
  ROW(F(Mem32) | F(Mem64), 0x00),
  ROW(F(RegSt), 0x01),
  ROW(F(RegSt), 0x00),
  ROW(F(ImmI32) | F(ImmI64) | F(Rel8) | F(Rel32), 0x00),
  ROW(F(RegGpd) | F(Mem32) | F(ImmI32) | F(ImmI64) | F(Rel32), 0x00),
  ROW(F(ImmI16), 0x00),
  ROW(F(ImmI16) | F(ImmI32), 0x00),
  ROW(F(MemUnspecified) | F(Mem32) | F(Mem48), 0x00),
  ROW(F(MemUnspecified) | F(Mem80), 0x00),
  ROW(F(MemUnspecified) | F(Mem48), 0x00),
  ROW(F(RegGpd) | F(MemUnspecified) | F(Mem16), 0x00),
  ROW(F(RegGpbLo) | F(RegGpbHi) | F(RegGpw) | F(Mem8) | F(Mem16), 0x00),
  ROW(F(RegGpbLo) | F(RegGpw) | F(Mem8) | F(Mem16), 0x00),
  ROW(F(RegGpbLo) | F(ImmI8) | F(ImmU8), 0x02),
  ROW(F(RegXmm) | F(RegKReg), 0x00),
  ROW(F(RegYmm) | F(RegKReg), 0x00),
  ROW(F(RegXmm) | F(MemUnspecified) | F(Mem32), 0x00),
  ROW(F(Vm64x) | F(Vm64y), 0x00),
  ROW(F(RegXmm) | F(MemUnspecified) | F(Mem16), 0x00),
  ROW(F(RegBnd), 0x00),
  ROW(F(RegBnd) | F(MemUnspecified), 0x00),
  ROW(F(MemUnspecified), 0x00),
  ROW(F(RegGpbLo) | F(RegGpq) | F(Mem8) | F(Mem64), 0x00),
  ROW(F(MemUnspecified) | F(Mem512) | F(FlagMemBase) | F(FlagMemEs), 0x00),
  ROW(F(RegSt) | F(Mem32) | F(Mem64), 0x00),
  ROW(F(RegGpd) | F(FlagImplicit), 0x02),
  ROW(F(RegGpd) | F(RegGpq) | F(FlagImplicit), 0x01),
  ROW(F(RegGpw) | F(RegGpd) | F(FlagImplicit), 0x02),
  ROW(F(ImmI32) | F(ImmI64) | F(Rel8), 0x00),
  ROW(F(RegGpd) | F(RegGpq) | F(FlagImplicit), 0x02),
  ROW(F(RegGpd) | F(RegKReg) | F(MemUnspecified) | F(Mem8), 0x00),
  ROW(F(RegGpd) | F(MemUnspecified) | F(Mem8), 0x00),
  ROW(F(RegGpd) | F(RegKReg) | F(MemUnspecified) | F(Mem32), 0x00),
  ROW(F(RegGpd) | F(RegKReg) | F(MemUnspecified) | F(Mem16), 0x00),
  ROW(F(MemUnspecified) | F(Mem8) | F(Mem16) | F(Mem32) | F(Mem48) | F(Mem64) | F(Mem80) | F(Mem128) | F(Mem256) | F(Mem512) | F(Mem1024), 0x00),
  ROW(F(RegGpd) | F(RegGpq), 0x00),
  ROW(F(RegMm) | F(MemUnspecified) | F(Mem64) | F(ImmI8) | F(ImmU8), 0x00),
  ROW(F(RegMm) | F(MemUnspecified) | F(Mem32), 0x00),
  ROW(F(ImmU16), 0x00),
  ROW(F(RegGpw) | F(RegGpd) | F(MemUnspecified) | F(Mem16), 0x00),
  ROW(F(RegGpq) | F(MemUnspecified) | F(Mem16), 0x00),
  ROW(F(MemUnspecified) | F(FlagMemBase) | F(FlagMemDs), 0x00),
  ROW(F(RegXmm) | F(RegYmm) | F(Mem128) | F(Mem256), 0x00),
  ROW(F(RegXmm) | F(RegYmm) | F(RegZmm), 0x00),
  ROW(F(RegXmm) | F(ImmI8) | F(ImmU8), 0x00),
  ROW(F(RegXmm) | F(FlagImplicit), 0x01),
  ROW(F(MemUnspecified) | F(FlagMib), 0x00),
  ROW(F(MemUnspecified) | F(Mem512) | F(FlagMemBase) | F(FlagMemDs) | F(FlagImplicit), 0x01),
  ROW(F(RegGpq) | F(FlagImplicit), 0x02),
  ROW(F(RegGpq) | F(FlagImplicit), 0x08),
  ROW(F(RegGpd) | F(FlagImplicit), 0x08),
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
  ROW(F(RegTmm), 0x00),
  ROW(F(MemUnspecified) | F(FlagTMem), 0x00),
  ROW(F(RegYmm) | F(RegZmm), 0x00),
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
  0, 0, 1, 2, 1, 2, 0, 3, 4, 3, 5, 5, 6, 7, 5, 5, 4, 5, 5, 5, 5, 8, 0, 3, 0, 5,
  5, 5, 5, 2, 9, 2, 0, 10, 10, 10, 10, 10, 0, 0, 0, 0, 10, 10, 10, 10, 10, 11, 11,
  11, 12, 12, 13, 14, 15, 10, 10, 0, 16, 17, 17, 17, 0, 0, 0, 18, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
  4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 19, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 20, 0, 0, 0, 0, 0, 0, 0, 21, 22, 0, 23, 24, 25, 8, 26, 26,
  26, 25, 27, 8, 25, 28, 29, 30, 31, 32, 33, 34, 26, 26, 8, 28, 29, 34, 35, 0,
  0, 0, 0, 36, 5, 5, 6, 7, 0, 0, 0, 0, 0, 37, 37, 0, 0, 38, 0, 0, 39, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 39, 0, 39, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 39, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 39, 0, 39, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5, 5, 5, 0, 40, 5, 5, 36,
  41, 42, 0, 0, 0, 43, 0, 38, 0, 0, 0, 0, 44, 0, 45, 0, 44, 44, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 46, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 47, 48, 49, 50, 51, 52, 53,
  54, 0, 0, 0, 55, 56, 57, 58, 0, 0, 0, 0, 0, 0, 0, 0, 0, 55, 56, 57, 58, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 59, 0, 60, 0, 2, 0, 61, 0, 2, 0, 2, 0, 2, 0, 0,
  0, 0, 0, 62, 63, 63, 63, 59, 2, 0, 0, 0, 10, 0, 0, 5, 5, 6, 7, 0, 0, 5, 5, 6,
  7, 0, 0, 64, 65, 66, 66, 67, 48, 25, 37, 67, 53, 66, 66, 68, 69, 69, 70, 71,
  71, 72, 72, 60, 60, 67, 60, 60, 71, 71, 73, 49, 53, 74, 75, 8, 8, 76, 77, 10,
  66, 66, 77, 0, 36, 5, 5, 6, 7, 0, 78, 0, 0, 79, 0, 3, 5, 5, 80, 81, 10, 10, 10,
  4, 4, 5, 4, 4, 4, 4, 4, 4, 4, 4, 4, 0, 4, 4, 0, 4, 82, 4, 0, 0, 0, 4, 4, 5,
  4, 0, 0, 4, 4, 5, 4, 0, 0, 0, 0, 0, 0, 0, 0, 83, 28, 28, 82, 82, 82, 82, 82, 82,
  82, 82, 82, 82, 28, 82, 82, 82, 28, 28, 82, 82, 82, 4, 4, 4, 84, 4, 4, 4, 28,
  28, 0, 0, 0, 0, 4, 4, 5, 5, 4, 4, 5, 5, 5, 5, 4, 4, 5, 5, 85, 86, 87, 25, 25,
  25, 86, 86, 87, 25, 25, 25, 86, 5, 4, 82, 4, 4, 5, 4, 4, 0, 0, 0, 10, 0, 0,
  0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4, 4, 0, 0, 0, 0, 4, 4, 4, 4, 88, 4, 4, 0, 4,
  4, 4, 88, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 28, 89, 0, 4, 4, 5, 4, 90, 90, 5, 90,
  0, 0, 0, 0, 0, 0, 0, 4, 91, 8, 92, 91, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 93,
  0, 0, 0, 0, 0, 91, 91, 0, 0, 0, 0, 0, 0, 8, 92, 0, 0, 91, 91, 0, 0, 3, 94, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5, 5, 5, 0, 5, 5, 0, 91, 0, 0, 91, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 8, 8, 27, 92, 0, 0, 0, 0, 0, 0, 95, 0, 0, 0, 3, 5, 5, 6, 7, 0,
  0, 0, 0, 0, 0, 0, 10, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 0, 96, 96, 0, 97, 0, 0,
  0, 10, 10, 21, 22, 98, 98, 0, 0, 0, 0, 5, 5, 5, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 8, 0, 0, 0, 0, 0, 0, 0, 99, 99, 0, 0, 0, 0, 0, 0, 100, 29,
  101, 102, 101, 102, 100, 29, 101, 102, 101, 102, 103, 104, 0, 0, 0, 0, 0, 0,
  21, 105, 22, 106, 106, 107, 77, 10, 0, 67, 67, 67, 67, 77, 108, 109, 108, 10,
  108, 10, 110, 111, 107, 110, 111, 110, 111, 10, 10, 10, 107, 0, 77, 107, 10,
  107, 10, 109, 108, 0, 29, 0, 29, 0, 112, 0, 112, 0, 0, 0, 0, 0, 34, 34, 108,
  10, 108, 10, 110, 111, 110, 111, 10, 10, 10, 107, 10, 107, 29, 29, 112, 112, 34,
  34, 107, 77, 10, 10, 109, 108, 0, 0, 0, 10, 10, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 113, 113, 10, 10, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10, 10, 28, 114, 2, 2, 0,
  0, 0, 0, 0, 0, 0, 0, 2, 115, 10, 10, 10, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 67, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 116, 116, 48, 117, 116, 116, 116, 116,
  116, 116, 116, 116, 0, 118, 118, 0, 71, 71, 119, 120, 67, 67, 67, 67, 121, 71,
  122, 10, 10, 73, 116, 116, 50, 0, 0, 0, 106, 0, 0, 0, 0, 0, 0, 0, 0, 0, 123,
  0, 0, 0, 0, 0, 0, 0, 0, 10, 10, 10, 10, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 124, 34, 125, 125, 29, 126, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 106, 106,
  106, 106, 0, 0, 0, 0, 0, 0, 10, 10, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  10, 10, 10, 10, 0, 0, 0, 0, 2, 2, 115, 2, 8, 8, 8, 0, 8, 0, 8, 8, 8, 8, 8, 8,
  0, 8, 8, 84, 8, 0, 8, 0, 0, 8, 0, 0, 0, 0, 10, 10, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 127,
  127, 128, 129, 125, 125, 125, 125, 85, 127, 130, 129, 128, 128, 129, 130,
  129, 128, 129, 131, 132, 107, 107, 107, 131, 128, 129, 130, 129, 128, 129, 127,
  129, 131, 132, 107, 107, 107, 131, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10, 10, 10, 10,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 67, 67, 133, 67, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 123, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 10, 10, 0, 0, 113, 113, 0, 0, 10, 10, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 10, 10, 0, 0, 113, 113, 0, 0, 10, 10, 0, 0, 0, 0, 0, 0, 0,
  0, 67, 67, 0, 0, 0, 0, 0, 0, 0, 0, 67, 133, 134, 135, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 10, 10, 10, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 123, 123, 21, 105, 22,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 136, 137, 136, 137, 0, 138, 0, 139, 0,
  0, 0, 3, 5, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
};

const uint8_t InstDB::rwInfoIndexB[Inst::_kIdCount] = {
  0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 3, 0,
  0, 0, 0, 0, 0, 0, 4, 0, 0, 0, 0, 0, 5, 5, 6, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 7, 0, 0, 0, 0, 4, 8, 1, 0, 9, 0, 0, 0, 10, 10, 10, 0, 0, 11, 0,
  0, 10, 12, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13,
  13, 5, 5, 13, 0, 14, 15, 13, 16, 17, 18, 13, 0, 0, 19, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 20, 1, 1, 21, 22, 0, 0, 0,
  0, 5, 5, 0, 0, 0, 0, 0, 0, 23, 24, 0, 0, 25, 26, 27, 28, 0, 0, 26, 26, 26, 26,
  26, 26, 26, 26, 29, 30, 30, 29, 0, 0, 0, 25, 26, 25, 26, 0, 26, 25, 25, 25,
  25, 25, 25, 25, 0, 0, 31, 31, 31, 25, 25, 29, 0, 32, 10, 0, 0, 0, 0, 0, 0, 25,
  26, 0, 0, 0, 33, 34, 33, 35, 0, 0, 0, 0, 0, 10, 33, 0, 0, 0, 0, 36, 34, 33, 36,
  35, 25, 26, 25, 26, 0, 30, 30, 30, 30, 0, 0, 0, 26, 10, 10, 33, 33, 0, 0, 0,
  20, 5, 5, 0, 0, 0, 0, 0, 0, 0, 22, 37, 0, 21, 38, 39, 0, 40, 41, 0, 0, 0, 0,
  0, 10, 0, 42, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 8, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 43, 44, 45, 46, 43, 44, 43, 44,
  45, 46, 45, 46, 0, 0, 0, 0, 0, 0, 0, 0, 43, 44, 45, 0, 0, 0, 0, 46, 47, 48,
  49, 50, 47, 48, 49, 50, 0, 0, 0, 0, 51, 52, 53, 43, 44, 45, 46, 43, 44, 45, 46,
  54, 0, 25, 0, 55, 0, 56, 0, 0, 0, 0, 0, 10, 0, 10, 25, 57, 58, 57, 0, 0, 0,
  0, 0, 0, 57, 59, 59, 0, 60, 61, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 62, 62, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 63, 0, 0, 63, 0, 0, 0, 0, 0, 5, 64, 0, 0, 0, 0, 65, 0, 66, 21, 67, 21,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 68, 0, 0, 0, 0, 0,
  0, 6, 5, 5, 0, 0, 0, 0, 69, 70, 0, 0, 0, 0, 71, 72, 0, 3, 3, 73, 23, 74, 75,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 76, 40, 77, 78, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 79, 0, 0, 0, 0, 0, 0, 0,
  10, 10, 10, 10, 10, 10, 10, 10, 10, 0, 0, 2, 2, 2, 80, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 67, 0, 0, 0, 0, 0, 0, 0, 0,
  67, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 81, 81, 82, 81, 82, 82, 82, 81, 81, 83,
  84, 0, 85, 0, 0, 0, 0, 0, 0, 86, 2, 2, 87, 88, 0, 0, 0, 11, 89, 0, 0, 4, 0, 0,
  0, 0, 0, 0, 90, 0, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91,
  91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 0, 91, 0, 33, 0, 0,
  0, 5, 0, 0, 6, 0, 92, 4, 0, 92, 4, 5, 5, 33, 20, 93, 81, 93, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 94, 0, 93, 95, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 96, 96, 0,
  96, 96, 96, 96, 96, 96, 0, 0, 0, 0, 0, 0, 97, 0, 98, 0, 0, 0, 0, 0, 0, 0, 0, 10,
  98, 0, 0, 0, 0, 99, 100, 99, 100, 3, 3, 3, 101, 102, 103, 3, 3, 3, 3, 3, 3,
  0, 2, 3, 3, 3, 3, 3, 3, 0, 0, 3, 3, 3, 3, 104, 104, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 3, 105, 3, 106, 107, 108, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 109, 0, 0, 0, 0, 0,
  0, 0, 110, 0, 111, 0, 112, 0, 112, 0, 113, 114, 115, 116, 117, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 113, 114,
  115, 0, 0, 3, 3, 3, 3, 101, 112, 103, 3, 118, 3, 57, 57, 0, 0, 0, 0, 119, 120,
  121, 120, 121, 119, 120, 121, 120, 121, 23, 122, 123, 122, 123, 124, 124, 125,
  126, 124, 124, 124, 127, 128, 129, 124, 124, 124, 127, 128, 129, 124, 124,
  124, 127, 128, 129, 122, 123, 130, 130, 131, 132, 124, 124, 124, 124, 124, 124,
  124, 124, 124, 130, 130, 124, 124, 124, 127, 133, 129, 124, 124, 124, 127, 133,
  129, 124, 124, 124, 127, 133, 129, 124, 124, 124, 124, 124, 124, 124, 124,
  124, 130, 130, 130, 130, 131, 132, 122, 123, 124, 124, 124, 127, 128, 129, 124,
  124, 124, 127, 128, 129, 124, 124, 124, 127, 128, 129, 130, 130, 131, 132,
  124, 124, 124, 127, 133, 129, 124, 124, 124, 127, 133, 129, 124, 124, 124, 134,
  133, 135, 130, 130, 131, 132, 136, 136, 136, 80, 137, 138, 0, 0, 0, 0, 139,
  140, 10, 10, 10, 10, 10, 10, 10, 10, 140, 141, 0, 0, 0, 142, 143, 144, 86, 86,
  86, 142, 143, 144, 3, 3, 3, 3, 3, 3, 3, 145, 146, 147, 146, 147, 145, 146, 147,
  146, 147, 103, 0, 55, 60, 148, 148, 3, 3, 3, 101, 102, 103, 0, 149, 0, 0, 3,
  3, 3, 101, 102, 103, 0, 150, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 151, 152,
  152, 153, 154, 154, 0, 0, 0, 0, 0, 0, 0, 155, 156, 0, 0, 157, 0, 0, 0, 3, 11,
  149, 0, 0, 158, 150, 3, 3, 3, 101, 102, 103, 0, 0, 11, 3, 3, 159, 159, 160,
  160, 0, 0, 0, 0, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
  3, 3, 3, 3, 3, 104, 3, 0, 0, 0, 0, 0, 0, 3, 130, 105, 105, 3, 3, 3, 3, 69, 70,
  3, 3, 3, 3, 71, 72, 105, 105, 105, 105, 105, 105, 118, 118, 0, 0, 0, 0, 118,
  118, 118, 118, 118, 118, 0, 0, 124, 124, 124, 124, 124, 124, 124, 124, 124,
  124, 124, 124, 124, 124, 124, 124, 161, 161, 3, 3, 3, 124, 3, 3, 124, 124, 130,
  130, 162, 162, 162, 3, 162, 3, 124, 124, 124, 124, 124, 3, 0, 0, 0, 0, 73, 23,
  74, 163, 140, 139, 141, 140, 0, 0, 0, 3, 0, 3, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0,
  0, 3, 0, 3, 3, 0, 164, 103, 101, 102, 0, 0, 165, 165, 165, 165, 165, 165, 165,
  165, 165, 165, 165, 165, 124, 124, 3, 3, 148, 148, 3, 3, 3, 3, 3, 3, 3, 3,
  3, 3, 3, 3, 3, 3, 3, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 3, 3, 3, 3,
  3, 3, 3, 3, 0, 0, 0, 0, 3, 3, 3, 166, 86, 86, 3, 3, 86, 86, 3, 3, 167, 167, 167,
  167, 3, 0, 0, 0, 0, 167, 167, 167, 167, 167, 167, 3, 3, 124, 124, 124, 3, 167,
  167, 3, 3, 124, 124, 124, 3, 3, 105, 86, 86, 86, 3, 3, 3, 168, 169, 168, 3,
  3, 3, 170, 168, 171, 3, 3, 3, 170, 168, 169, 168, 3, 3, 3, 170, 3, 3, 3, 3,
  3, 3, 3, 3, 172, 172, 0, 105, 105, 105, 105, 105, 105, 105, 105, 3, 3, 3, 3, 3,
  3, 3, 3, 3, 3, 3, 3, 3, 142, 144, 0, 0, 142, 144, 0, 0, 142, 144, 0, 0, 143,
  144, 86, 86, 86, 142, 143, 144, 86, 86, 86, 142, 143, 144, 86, 86, 142, 144,
  0, 0, 142, 144, 0, 0, 142, 144, 0, 0, 143, 144, 3, 3, 3, 101, 102, 103, 0, 0,
  10, 10, 10, 10, 10, 10, 10, 10, 0, 0, 0, 0, 173, 3, 3, 3, 3, 3, 3, 174, 174, 174,
  3, 3, 0, 0, 0, 142, 143, 144, 94, 3, 3, 3, 101, 102, 103, 0, 0, 0, 0, 0, 3,
  3, 3, 3, 3, 3, 0, 0, 0, 0, 58, 58, 175, 0, 0, 0, 0, 0, 0, 0, 0, 0, 82, 0, 0,
  0, 0, 0, 176, 176, 176, 176, 177, 177, 177, 177, 177, 177, 177, 177, 175, 0,
  0
};

const InstDB::RWInfo InstDB::rwInfoA[] = {
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 0 , 0 , 0 , 0 , 0 , 0  } }, // #0 [ref=1054x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 1 , 0 , 0 , 0 , 0 , 0  } }, // #1 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 2 , 3 , 0 , 0 , 0 , 0  } }, // #2 [ref=15x]
  { InstDB::RWInfo::kCategoryGeneric   , 1 , { 2 , 3 , 0 , 0 , 0 , 0  } }, // #3 [ref=7x]
  { InstDB::RWInfo::kCategoryGeneric   , 2 , { 2 , 3 , 0 , 0 , 0 , 0  } }, // #4 [ref=96x]
  { InstDB::RWInfo::kCategoryGeneric   , 3 , { 4 , 5 , 0 , 0 , 0 , 0  } }, // #5 [ref=55x]
  { InstDB::RWInfo::kCategoryGeneric   , 4 , { 6 , 7 , 0 , 0 , 0 , 0  } }, // #6 [ref=6x]
  { InstDB::RWInfo::kCategoryGeneric   , 5 , { 8 , 9 , 0 , 0 , 0 , 0  } }, // #7 [ref=6x]
  { InstDB::RWInfo::kCategoryGeneric   , 3 , { 10, 5 , 0 , 0 , 0 , 0  } }, // #8 [ref=26x]
  { InstDB::RWInfo::kCategoryGeneric   , 7 , { 12, 13, 0 , 0 , 0 , 0  } }, // #9 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 2 , { 11, 3 , 0 , 0 , 0 , 0  } }, // #10 [ref=75x]
  { InstDB::RWInfo::kCategoryGeneric   , 2 , { 5 , 3 , 0 , 0 , 0 , 0  } }, // #11 [ref=3x]
  { InstDB::RWInfo::kCategoryGeneric   , 8 , { 10, 3 , 0 , 0 , 0 , 0  } }, // #12 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 9 , { 10, 5 , 0 , 0 , 0 , 0  } }, // #13 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 8 , { 15, 5 , 0 , 0 , 0 , 0  } }, // #14 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 3 , 3 , 0 , 0 , 0 , 0  } }, // #15 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 10, { 3 , 3 , 0 , 0 , 0 , 0  } }, // #16 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 10, { 2 , 3 , 0 , 0 , 0 , 0  } }, // #17 [ref=3x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 16, 17, 0 , 0 , 0 , 0  } }, // #18 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 1 , { 3 , 3 , 0 , 0 , 0 , 0  } }, // #19 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 14, { 20, 21, 0 , 0 , 0 , 0  } }, // #20 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 4 , { 7 , 7 , 0 , 0 , 0 , 0  } }, // #21 [ref=4x]
  { InstDB::RWInfo::kCategoryGeneric   , 5 , { 9 , 9 , 0 , 0 , 0 , 0  } }, // #22 [ref=4x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 33, 34, 0 , 0 , 0 , 0  } }, // #23 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 16, { 2 , 3 , 0 , 0 , 0 , 0  } }, // #24 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 4 , { 10, 7 , 0 , 0 , 0 , 0  } }, // #25 [ref=10x]
  { InstDB::RWInfo::kCategoryGeneric   , 3 , { 35, 5 , 0 , 0 , 0 , 0  } }, // #26 [ref=5x]
  { InstDB::RWInfo::kCategoryGeneric   , 4 , { 36, 7 , 0 , 0 , 0 , 0  } }, // #27 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 4 , { 35, 7 , 0 , 0 , 0 , 0  } }, // #28 [ref=11x]
  { InstDB::RWInfo::kCategoryGeneric   , 4 , { 11, 7 , 0 , 0 , 0 , 0  } }, // #29 [ref=9x]
  { InstDB::RWInfo::kCategoryGeneric   , 4 , { 37, 7 , 0 , 0 , 0 , 0  } }, // #30 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 16, { 36, 3 , 0 , 0 , 0 , 0  } }, // #31 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 16, { 37, 3 , 0 , 0 , 0 , 0  } }, // #32 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 5 , { 36, 9 , 0 , 0 , 0 , 0  } }, // #33 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 5 , { 11, 9 , 0 , 0 , 0 , 0  } }, // #34 [ref=7x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 38, 39, 0 , 0 , 0 , 0  } }, // #35 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 17, { 1 , 40, 0 , 0 , 0 , 0  } }, // #36 [ref=3x]
  { InstDB::RWInfo::kCategoryGeneric   , 13, { 43, 44, 0 , 0 , 0 , 0  } }, // #37 [ref=3x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 4 , 5 , 0 , 0 , 0 , 0  } }, // #38 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 46, 47, 0 , 0 , 0 , 0  } }, // #39 [ref=6x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 0 , 51, 0 , 0 , 0 , 0  } }, // #40 [ref=1x]
  { InstDB::RWInfo::kCategoryImul      , 2 , { 0 , 0 , 0 , 0 , 0 , 0  } }, // #41 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 52, 53, 0 , 0 , 0 , 0  } }, // #42 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 14, { 55, 53, 0 , 0 , 0 , 0  } }, // #43 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 15, { 3 , 5 , 0 , 0 , 0 , 0  } }, // #44 [ref=3x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 22, 29, 0 , 0 , 0 , 0  } }, // #45 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 57, 0 , 0 , 0 , 0 , 0  } }, // #46 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 23, { 58, 40, 0 , 0 , 0 , 0  } }, // #47 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 24, { 45, 9 , 0 , 0 , 0 , 0  } }, // #48 [ref=3x]
  { InstDB::RWInfo::kCategoryGeneric   , 25, { 35, 7 , 0 , 0 , 0 , 0  } }, // #49 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 26, { 49, 13, 0 , 0 , 0 , 0  } }, // #50 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 58, 40, 0 , 0 , 0 , 0  } }, // #51 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 45, 9 , 0 , 0 , 0 , 0  } }, // #52 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 35, 7 , 0 , 0 , 0 , 0  } }, // #53 [ref=3x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 49, 13, 0 , 0 , 0 , 0  } }, // #54 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 40, 40, 0 , 0 , 0 , 0  } }, // #55 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 9 , 9 , 0 , 0 , 0 , 0  } }, // #56 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 7 , 7 , 0 , 0 , 0 , 0  } }, // #57 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 13, 13, 0 , 0 , 0 , 0  } }, // #58 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 27, { 11, 3 , 0 , 0 , 0 , 0  } }, // #59 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 15, { 10, 5 , 0 , 0 , 0 , 0  } }, // #60 [ref=5x]
  { InstDB::RWInfo::kCategoryGeneric   , 8 , { 11, 3 , 0 , 0 , 0 , 0  } }, // #61 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 52, 20, 0 , 0 , 0 , 0  } }, // #62 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 60, 0 , 0 , 0 , 0 , 0  } }, // #63 [ref=3x]
  { InstDB::RWInfo::kCategoryMov       , 29, { 0 , 0 , 0 , 0 , 0 , 0  } }, // #64 [ref=1x]
  { InstDB::RWInfo::kCategoryMovabs    , 0 , { 0 , 0 , 0 , 0 , 0 , 0  } }, // #65 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 30, { 10, 5 , 0 , 0 , 0 , 0  } }, // #66 [ref=6x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 11, 3 , 0 , 0 , 0 , 0  } }, // #67 [ref=18x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 36, 64, 0 , 0 , 0 , 0  } }, // #68 [ref=1x]
  { InstDB::RWInfo::kCategoryMovh64    , 12, { 0 , 0 , 0 , 0 , 0 , 0  } }, // #69 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 65, 7 , 0 , 0 , 0 , 0  } }, // #70 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 12, { 35, 7 , 0 , 0 , 0 , 0  } }, // #71 [ref=7x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 58, 5 , 0 , 0 , 0 , 0  } }, // #72 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 28, { 45, 9 , 0 , 0 , 0 , 0  } }, // #73 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 14, { 66, 20, 0 , 0 , 0 , 0  } }, // #74 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 31, { 35, 7 , 0 , 0 , 0 , 0  } }, // #75 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 33, { 45, 9 , 0 , 0 , 0 , 0  } }, // #76 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 16, { 11, 3 , 0 , 0 , 0 , 0  } }, // #77 [ref=6x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 17, 29, 0 , 0 , 0 , 0  } }, // #78 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 11, { 3 , 3 , 0 , 0 , 0 , 0  } }, // #79 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 53, 22, 0 , 0 , 0 , 0  } }, // #80 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 14, { 53, 69, 0 , 0 , 0 , 0  } }, // #81 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 4 , { 26, 7 , 0 , 0 , 0 , 0  } }, // #82 [ref=18x]
  { InstDB::RWInfo::kCategoryGeneric   , 36, { 0 , 0 , 0 , 0 , 0 , 0  } }, // #83 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 3 , { 72, 5 , 0 , 0 , 0 , 0  } }, // #84 [ref=2x]
  { InstDB::RWInfo::kCategoryVmov1_8   , 0 , { 0 , 0 , 0 , 0 , 0 , 0  } }, // #85 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 5 , { 10, 9 , 0 , 0 , 0 , 0  } }, // #86 [ref=4x]
  { InstDB::RWInfo::kCategoryGeneric   , 27, { 10, 13, 0 , 0 , 0 , 0  } }, // #87 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 4 , 0 , 0 , 0 , 0 , 0  } }, // #88 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 3 , { 5 , 5 , 0 , 0 , 0 , 0  } }, // #89 [ref=1x]
  { InstDB::RWInfo::kCategoryPunpcklxx , 38, { 0 , 0 , 0 , 0 , 0 , 0  } }, // #90 [ref=3x]
  { InstDB::RWInfo::kCategoryGeneric   , 10, { 2 , 73, 0 , 0 , 0 , 0  } }, // #91 [ref=8x]
  { InstDB::RWInfo::kCategoryGeneric   , 5 , { 37, 9 , 0 , 0 , 0 , 0  } }, // #92 [ref=3x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 16, 51, 0 , 0 , 0 , 0  } }, // #93 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 22, 21, 0 , 0 , 0 , 0  } }, // #94 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 66, 22, 0 , 0 , 0 , 0  } }, // #95 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 8 , { 43, 3 , 0 , 0 , 0 , 0  } }, // #96 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 8 , { 11, 44, 0 , 0 , 0 , 0  } }, // #97 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 5 , { 54, 9 , 0 , 0 , 0 , 0  } }, // #98 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 21, { 11, 13, 0 , 0 , 0 , 0  } }, // #99 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 15, { 81, 5 , 0 , 0 , 0 , 0  } }, // #100 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 15, { 11, 5 , 0 , 0 , 0 , 0  } }, // #101 [ref=4x]
  { InstDB::RWInfo::kCategoryGeneric   , 43, { 43, 82, 0 , 0 , 0 , 0  } }, // #102 [ref=4x]
  { InstDB::RWInfo::kCategoryGeneric   , 44, { 11, 7 , 0 , 0 , 0 , 0  } }, // #103 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 45, { 11, 9 , 0 , 0 , 0 , 0  } }, // #104 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 27, { 13, 13, 0 , 0 , 0 , 0  } }, // #105 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 11, { 11, 3 , 0 , 0 , 0 , 0  } }, // #106 [ref=7x]
  { InstDB::RWInfo::kCategoryVmov2_1   , 46, { 0 , 0 , 0 , 0 , 0 , 0  } }, // #107 [ref=14x]
  { InstDB::RWInfo::kCategoryVmov1_2   , 16, { 0 , 0 , 0 , 0 , 0 , 0  } }, // #108 [ref=7x]
  { InstDB::RWInfo::kCategoryGeneric   , 16, { 10, 3 , 0 , 0 , 0 , 0  } }, // #109 [ref=3x]
  { InstDB::RWInfo::kCategoryGeneric   , 46, { 11, 3 , 0 , 0 , 0 , 0  } }, // #110 [ref=5x]
  { InstDB::RWInfo::kCategoryGeneric   , 47, { 11, 5 , 0 , 0 , 0 , 0  } }, // #111 [ref=5x]
  { InstDB::RWInfo::kCategoryGeneric   , 27, { 11, 5 , 0 , 0 , 0 , 0  } }, // #112 [ref=4x]
  { InstDB::RWInfo::kCategoryGeneric   , 51, { 43, 44, 0 , 0 , 0 , 0  } }, // #113 [ref=6x]
  { InstDB::RWInfo::kCategoryGeneric   , 5 , { 45, 9 , 0 , 0 , 0 , 0  } }, // #114 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 14, { 2 , 3 , 0 , 0 , 0 , 0  } }, // #115 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 58, { 11, 3 , 0 , 0 , 0 , 0  } }, // #116 [ref=12x]
  { InstDB::RWInfo::kCategoryVmovddup  , 38, { 0 , 0 , 0 , 0 , 0 , 0  } }, // #117 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 12, { 35, 64, 0 , 0 , 0 , 0  } }, // #118 [ref=2x]
  { InstDB::RWInfo::kCategoryVmovmskpd , 0 , { 0 , 0 , 0 , 0 , 0 , 0  } }, // #119 [ref=1x]
  { InstDB::RWInfo::kCategoryVmovmskps , 0 , { 0 , 0 , 0 , 0 , 0 , 0  } }, // #120 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 59, { 35, 7 , 0 , 0 , 0 , 0  } }, // #121 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 21, { 49, 13, 0 , 0 , 0 , 0  } }, // #122 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 2 , { 3 , 3 , 0 , 0 , 0 , 0  } }, // #123 [ref=4x]
  { InstDB::RWInfo::kCategoryGeneric   , 17, { 11, 40, 0 , 0 , 0 , 0  } }, // #124 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 11, 7 , 0 , 0 , 0 , 0  } }, // #125 [ref=6x]
  { InstDB::RWInfo::kCategoryGeneric   , 27, { 11, 13, 0 , 0 , 0 , 0  } }, // #126 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 35, 3 , 0 , 0 , 0 , 0  } }, // #127 [ref=4x]
  { InstDB::RWInfo::kCategoryVmov1_4   , 62, { 0 , 0 , 0 , 0 , 0 , 0  } }, // #128 [ref=6x]
  { InstDB::RWInfo::kCategoryVmov1_2   , 48, { 0 , 0 , 0 , 0 , 0 , 0  } }, // #129 [ref=9x]
  { InstDB::RWInfo::kCategoryVmov1_8   , 63, { 0 , 0 , 0 , 0 , 0 , 0  } }, // #130 [ref=3x]
  { InstDB::RWInfo::kCategoryVmov4_1   , 47, { 0 , 0 , 0 , 0 , 0 , 0  } }, // #131 [ref=4x]
  { InstDB::RWInfo::kCategoryVmov8_1   , 64, { 0 , 0 , 0 , 0 , 0 , 0  } }, // #132 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 14, { 11, 3 , 0 , 0 , 0 , 0  } }, // #133 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 90, 5 , 0 , 0 , 0 , 0  } }, // #134 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 90, 82, 0 , 0 , 0 , 0  } }, // #135 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 18, { 45, 9 , 0 , 0 , 0 , 0  } }, // #136 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 35, { 35, 7 , 0 , 0 , 0 , 0  } }, // #137 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 11, { 2 , 2 , 0 , 0 , 0 , 0  } }, // #138 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 58, { 2 , 2 , 0 , 0 , 0 , 0  } }  // #139 [ref=1x]
};

const InstDB::RWInfo InstDB::rwInfoB[] = {
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 0 , 0 , 0 , 0 , 0 , 0  } }, // #0 [ref=791x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 1 , 0 , 0 , 0 , 0 , 0  } }, // #1 [ref=5x]
  { InstDB::RWInfo::kCategoryGeneric   , 3 , { 10, 5 , 0 , 0 , 0 , 0  } }, // #2 [ref=7x]
  { InstDB::RWInfo::kCategoryGeneric   , 6 , { 11, 3 , 3 , 0 , 0 , 0  } }, // #3 [ref=195x]
  { InstDB::RWInfo::kCategoryGeneric   , 2 , { 11, 3 , 3 , 0 , 0 , 0  } }, // #4 [ref=5x]
  { InstDB::RWInfo::kCategoryGeneric   , 3 , { 4 , 5 , 0 , 0 , 0 , 0  } }, // #5 [ref=14x]
  { InstDB::RWInfo::kCategoryGeneric   , 3 , { 4 , 5 , 14, 0 , 0 , 0  } }, // #6 [ref=4x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 2 , 0 , 0 , 0 , 0 , 0  } }, // #7 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 11, { 3 , 0 , 0 , 0 , 0 , 0  } }, // #8 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 18, 0 , 0 , 0 , 0 , 0  } }, // #9 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 8 , { 3 , 0 , 0 , 0 , 0 , 0  } }, // #10 [ref=37x]
  { InstDB::RWInfo::kCategoryGeneric   , 12, { 7 , 0 , 0 , 0 , 0 , 0  } }, // #11 [ref=4x]
  { InstDB::RWInfo::kCategoryGeneric   , 13, { 19, 0 , 0 , 0 , 0 , 0  } }, // #12 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 2 , 2 , 3 , 0 , 0 , 0  } }, // #13 [ref=16x]
  { InstDB::RWInfo::kCategoryGeneric   , 4 , { 6 , 7 , 0 , 0 , 0 , 0  } }, // #14 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 5 , { 8 , 9 , 0 , 0 , 0 , 0  } }, // #15 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 11, { 2 , 3 , 22, 0 , 0 , 0  } }, // #16 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 15, { 4 , 23, 18, 24, 25, 0  } }, // #17 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 12, { 26, 27, 28, 29, 30, 0  } }, // #18 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 28, 31, 32, 16, 0 , 0  } }, // #19 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 28, 0 , 0 , 0 , 0 , 0  } }, // #20 [ref=3x]
  { InstDB::RWInfo::kCategoryGeneric   , 10, { 2 , 0 , 0 , 0 , 0 , 0  } }, // #21 [ref=4x]
  { InstDB::RWInfo::kCategoryGeneric   , 6 , { 41, 42, 3 , 0 , 0 , 0  } }, // #22 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 18, { 45, 5 , 0 , 0 , 0 , 0  } }, // #23 [ref=4x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 4 , 0 , 0 , 0 , 0 , 0  } }, // #24 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 14, { 3 , 0 , 0 , 0 , 0 , 0  } }, // #25 [ref=17x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 46, 0 , 0 , 0 , 0 , 0  } }, // #26 [ref=16x]
  { InstDB::RWInfo::kCategoryGeneric   , 19, { 47, 0 , 0 , 0 , 0 , 0  } }, // #27 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 19, { 48, 0 , 0 , 0 , 0 , 0  } }, // #28 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 20, { 3 , 0 , 0 , 0 , 0 , 0  } }, // #29 [ref=3x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 47, 0 , 0 , 0 , 0 , 0  } }, // #30 [ref=6x]
  { InstDB::RWInfo::kCategoryGeneric   , 14, { 11, 0 , 0 , 0 , 0 , 0  } }, // #31 [ref=3x]
  { InstDB::RWInfo::kCategoryGeneric   , 21, { 13, 0 , 0 , 0 , 0 , 0  } }, // #32 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 8 , { 11, 0 , 0 , 0 , 0 , 0  } }, // #33 [ref=8x]
  { InstDB::RWInfo::kCategoryGeneric   , 21, { 49, 0 , 0 , 0 , 0 , 0  } }, // #34 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 7 , { 50, 0 , 0 , 0 , 0 , 0  } }, // #35 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 20, { 11, 0 , 0 , 0 , 0 , 0  } }, // #36 [ref=2x]
  { InstDB::RWInfo::kCategoryImul      , 22, { 0 , 0 , 0 , 0 , 0 , 0  } }, // #37 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 54, 0 , 0 , 0 , 0 , 0  } }, // #38 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 26, 0 , 0 , 0 , 0 , 0  } }, // #39 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 5 , { 4 , 9 , 0 , 0 , 0 , 0  } }, // #40 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 4 , 5 , 0 , 0 , 0 , 0  } }, // #41 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 22, 56, 57, 0 , 0 , 0  } }, // #42 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 58, 40, 40, 0 , 0 , 0  } }, // #43 [ref=6x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 45, 9 , 9 , 0 , 0 , 0  } }, // #44 [ref=6x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 35, 7 , 7 , 0 , 0 , 0  } }, // #45 [ref=6x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 49, 13, 13, 0 , 0 , 0  } }, // #46 [ref=6x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 58, 40, 0 , 0 , 0 , 0  } }, // #47 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 45, 9 , 0 , 0 , 0 , 0  } }, // #48 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 35, 7 , 0 , 0 , 0 , 0  } }, // #49 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 49, 13, 0 , 0 , 0 , 0  } }, // #50 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 49, 40, 40, 0 , 0 , 0  } }, // #51 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 35, 9 , 9 , 0 , 0 , 0  } }, // #52 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 45, 13, 13, 0 , 0 , 0  } }, // #53 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 59, 0 , 0 , 0 , 0 , 0  } }, // #54 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 28, { 9 , 0 , 0 , 0 , 0 , 0  } }, // #55 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 13, { 44, 0 , 0 , 0 , 0 , 0  } }, // #56 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 7 , { 13, 0 , 0 , 0 , 0 , 0  } }, // #57 [ref=5x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 3 , 0 , 0 , 0 , 0 , 0  } }, // #58 [ref=3x]
  { InstDB::RWInfo::kCategoryGeneric   , 5 , { 3 , 9 , 0 , 0 , 0 , 0  } }, // #59 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 15, { 5 , 5 , 61, 0 , 0 , 0  } }, // #60 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 12, { 7 , 7 , 62, 0 , 0 , 0  } }, // #61 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 8 , { 63, 29, 56, 0 , 0 , 0  } }, // #62 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 32, { 0 , 0 , 0 , 0 , 0 , 0  } }, // #63 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 6 , { 67, 42, 3 , 0 , 0 , 0  } }, // #64 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 6 , { 11, 11, 3 , 68, 0 , 0  } }, // #65 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 17, 29, 30, 0 , 0 , 0  } }, // #66 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 10, { 3 , 0 , 0 , 0 , 0 , 0  } }, // #67 [ref=3x]
  { InstDB::RWInfo::kCategoryGeneric   , 2 , { 2 , 3 , 0 , 0 , 0 , 0  } }, // #68 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 3 , { 5 , 5 , 0 , 70, 17, 56 } }, // #69 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 3 , { 5 , 5 , 0 , 71, 17, 56 } }, // #70 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 3 , { 5 , 5 , 0 , 70, 0 , 0  } }, // #71 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 3 , { 5 , 5 , 0 , 71, 0 , 0  } }, // #72 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 34, { 58, 5 , 0 , 0 , 0 , 0  } }, // #73 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 35, { 35, 5 , 0 , 0 , 0 , 0  } }, // #74 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 37, { 49, 3 , 0 , 0 , 0 , 0  } }, // #75 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 17, { 4 , 40, 0 , 0 , 0 , 0  } }, // #76 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 4 , { 4 , 7 , 0 , 0 , 0 , 0  } }, // #77 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 27, { 2 , 13, 0 , 0 , 0 , 0  } }, // #78 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 10, { 11, 0 , 0 , 0 , 0 , 0  } }, // #79 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 4 , { 35, 7 , 0 , 0 , 0 , 0  } }, // #80 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 11, 0 , 0 , 0 , 0 , 0  } }, // #81 [ref=6x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 16, 51, 29, 0 , 0 , 0  } }, // #82 [ref=5x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 45, 0 , 0 , 0 , 0 , 0  } }, // #83 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 35, 0 , 0 , 0 , 0 , 0  } }, // #84 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 16, 51, 70, 0 , 0 , 0  } }, // #85 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 2 , { 11, 3 , 0 , 0 , 0 , 0  } }, // #86 [ref=19x]
  { InstDB::RWInfo::kCategoryGeneric   , 4 , { 36, 7 , 0 , 0 , 0 , 0  } }, // #87 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 5 , { 37, 9 , 0 , 0 , 0 , 0  } }, // #88 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 74, 0 , 0 , 0 , 0 , 0  } }, // #89 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 7 , 0 , 0 , 0 , 0 , 0  } }, // #90 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 34, { 75, 0 , 0 , 0 , 0 , 0  } }, // #91 [ref=30x]
  { InstDB::RWInfo::kCategoryGeneric   , 11, { 2 , 3 , 73, 0 , 0 , 0  } }, // #92 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 39, { 11, 0 , 0 , 0 , 0 , 0  } }, // #93 [ref=3x]
  { InstDB::RWInfo::kCategoryGeneric   , 28, { 45, 0 , 0 , 0 , 0 , 0  } }, // #94 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 13, { 43, 0 , 0 , 0 , 0 , 0  } }, // #95 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 76, 44, 44, 0 , 0 , 0  } }, // #96 [ref=8x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 43, 0 , 0 , 0 , 0 , 0  } }, // #97 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 9 , 56, 17, 0 , 0 , 0  } }, // #98 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 15, { 76, 77, 78, 78, 78, 5  } }, // #99 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 15, { 4 , 79, 80, 80, 80, 5  } }, // #100 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 40, { 10, 5 , 7 , 0 , 0 , 0  } }, // #101 [ref=8x]
  { InstDB::RWInfo::kCategoryGeneric   , 41, { 10, 5 , 13, 0 , 0 , 0  } }, // #102 [ref=7x]
  { InstDB::RWInfo::kCategoryGeneric   , 42, { 10, 5 , 9 , 0 , 0 , 0  } }, // #103 [ref=9x]
  { InstDB::RWInfo::kCategoryGeneric   , 6 , { 11, 3 , 3 , 3 , 0 , 0  } }, // #104 [ref=3x]
  { InstDB::RWInfo::kCategoryGeneric   , 6 , { 35, 3 , 3 , 0 , 0 , 0  } }, // #105 [ref=18x]
  { InstDB::RWInfo::kCategoryGeneric   , 40, { 11, 5 , 7 , 0 , 0 , 0  } }, // #106 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 41, { 35, 13, 13, 0 , 0 , 0  } }, // #107 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 42, { 11, 5 , 9 , 0 , 0 , 0  } }, // #108 [ref=1x]
  { InstDB::RWInfo::kCategoryVmov1_2   , 48, { 0 , 0 , 0 , 0 , 0 , 0  } }, // #109 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 40, { 10, 5 , 5 , 0 , 0 , 0  } }, // #110 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 40, { 10, 83, 7 , 0 , 0 , 0  } }, // #111 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 41, { 10, 5 , 5 , 0 , 0 , 0  } }, // #112 [ref=3x]
  { InstDB::RWInfo::kCategoryGeneric   , 49, { 10, 64, 3 , 0 , 0 , 0  } }, // #113 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 49, { 10, 3 , 3 , 0 , 0 , 0  } }, // #114 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 49, { 10, 83, 3 , 0 , 0 , 0  } }, // #115 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 42, { 10, 64, 9 , 0 , 0 , 0  } }, // #116 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 42, { 10, 5 , 5 , 0 , 0 , 0  } }, // #117 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 50, { 10, 5 , 5 , 0 , 0 , 0  } }, // #118 [ref=9x]
  { InstDB::RWInfo::kCategoryGeneric   , 52, { 10, 82, 0 , 0 , 0 , 0  } }, // #119 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 52, { 10, 3 , 0 , 0 , 0 , 0  } }, // #120 [ref=4x]
  { InstDB::RWInfo::kCategoryGeneric   , 53, { 81, 44, 0 , 0 , 0 , 0  } }, // #121 [ref=4x]
  { InstDB::RWInfo::kCategoryGeneric   , 6 , { 84, 3 , 3 , 0 , 0 , 0  } }, // #122 [ref=4x]
  { InstDB::RWInfo::kCategoryGeneric   , 42, { 85, 5 , 5 , 0 , 0 , 0  } }, // #123 [ref=4x]
  { InstDB::RWInfo::kCategoryGeneric   , 6 , { 2 , 3 , 3 , 0 , 0 , 0  } }, // #124 [ref=88x]
  { InstDB::RWInfo::kCategoryGeneric   , 40, { 4 , 64, 7 , 0 , 0 , 0  } }, // #125 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 42, { 4 , 83, 9 , 0 , 0 , 0  } }, // #126 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 40, { 6 , 7 , 7 , 0 , 0 , 0  } }, // #127 [ref=11x]
  { InstDB::RWInfo::kCategoryGeneric   , 41, { 4 , 5 , 5 , 0 , 0 , 0  } }, // #128 [ref=6x]
  { InstDB::RWInfo::kCategoryGeneric   , 42, { 8 , 9 , 9 , 0 , 0 , 0  } }, // #129 [ref=11x]
  { InstDB::RWInfo::kCategoryGeneric   , 54, { 11, 3 , 3 , 3 , 0 , 0  } }, // #130 [ref=15x]
  { InstDB::RWInfo::kCategoryGeneric   , 55, { 35, 7 , 7 , 7 , 0 , 0  } }, // #131 [ref=4x]
  { InstDB::RWInfo::kCategoryGeneric   , 56, { 45, 9 , 9 , 9 , 0 , 0  } }, // #132 [ref=4x]
  { InstDB::RWInfo::kCategoryGeneric   , 41, { 4 , 5 , 13, 0 , 0 , 0  } }, // #133 [ref=6x]
  { InstDB::RWInfo::kCategoryGeneric   , 40, { 26, 7 , 7 , 0 , 0 , 0  } }, // #134 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 42, { 54, 9 , 9 , 0 , 0 , 0  } }, // #135 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 16, { 35, 3 , 0 , 0 , 0 , 0  } }, // #136 [ref=3x]
  { InstDB::RWInfo::kCategoryGeneric   , 27, { 35, 13, 0 , 0 , 0 , 0  } }, // #137 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 5 , { 35, 9 , 0 , 0 , 0 , 0  } }, // #138 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 8 , { 2 , 3 , 2 , 0 , 0 , 0  } }, // #139 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 2 , 3 , 2 , 0 , 0 , 0  } }, // #140 [ref=4x]
  { InstDB::RWInfo::kCategoryGeneric   , 14, { 4 , 3 , 4 , 0 , 0 , 0  } }, // #141 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 40, { 10, 64, 7 , 0 , 0 , 0  } }, // #142 [ref=11x]
  { InstDB::RWInfo::kCategoryGeneric   , 41, { 10, 86, 13, 0 , 0 , 0  } }, // #143 [ref=7x]
  { InstDB::RWInfo::kCategoryGeneric   , 42, { 10, 83, 9 , 0 , 0 , 0  } }, // #144 [ref=13x]
  { InstDB::RWInfo::kCategoryGeneric   , 50, { 81, 82, 5 , 0 , 0 , 0  } }, // #145 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 50, { 11, 3 , 5 , 0 , 0 , 0  } }, // #146 [ref=4x]
  { InstDB::RWInfo::kCategoryGeneric   , 57, { 43, 44, 82, 0 , 0 , 0  } }, // #147 [ref=4x]
  { InstDB::RWInfo::kCategoryVmaskmov  , 0 , { 0 , 0 , 0 , 0 , 0 , 0  } }, // #148 [ref=4x]
  { InstDB::RWInfo::kCategoryGeneric   , 12, { 35, 0 , 0 , 0 , 0 , 0  } }, // #149 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 22, 0 , 0 , 0 , 0 , 0  } }, // #150 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 10, 64, 64, 0 , 0 , 0  } }, // #151 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 12, { 10, 7 , 7 , 0 , 0 , 0  } }, // #152 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 10, 7 , 7 , 0 , 0 , 0  } }, // #153 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 12, { 10, 64, 7 , 0 , 0 , 0  } }, // #154 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 10, 64, 7 , 0 , 0 , 0  } }, // #155 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 10, 86, 13, 0 , 0 , 0  } }, // #156 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 10, 83, 9 , 0 , 0 , 0  } }, // #157 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 87, 0 , 0 , 0 , 0 , 0  } }, // #158 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 60, { 88, 89, 3 , 3 , 0 , 0  } }, // #159 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 15, { 43, 77, 78, 78, 78, 5  } }, // #160 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 57, { 81, 82, 82, 0 , 0 , 0  } }, // #161 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 22, { 11, 3 , 3 , 0 , 0 , 0  } }, // #162 [ref=4x]
  { InstDB::RWInfo::kCategoryGeneric   , 7 , { 49, 5 , 0 , 0 , 0 , 0  } }, // #163 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 61, { 10, 5 , 40, 0 , 0 , 0  } }, // #164 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 50, { 10, 5 , 5 , 5 , 0 , 0  } }, // #165 [ref=12x]
  { InstDB::RWInfo::kCategoryGeneric   , 65, { 10, 5 , 5 , 5 , 0 , 0  } }, // #166 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 66, { 10, 5 , 5 , 0 , 0 , 0  } }, // #167 [ref=12x]
  { InstDB::RWInfo::kCategoryGeneric   , 67, { 11, 3 , 5 , 0 , 0 , 0  } }, // #168 [ref=5x]
  { InstDB::RWInfo::kCategoryGeneric   , 68, { 11, 3 , 0 , 0 , 0 , 0  } }, // #169 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 69, { 11, 3 , 5 , 0 , 0 , 0  } }, // #170 [ref=3x]
  { InstDB::RWInfo::kCategoryGeneric   , 22, { 11, 3 , 5 , 0 , 0 , 0  } }, // #171 [ref=1x]
  { InstDB::RWInfo::kCategoryGenericEx , 6 , { 2 , 3 , 3 , 0 , 0 , 0  } }, // #172 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 90, 82, 5 , 0 , 0 , 0  } }, // #173 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 50, { 4 , 5 , 5 , 0 , 0 , 0  } }, // #174 [ref=3x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 56, 17, 29, 0 , 0 , 0  } }, // #175 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 8 , { 3 , 56, 17, 0 , 0 , 0  } }, // #176 [ref=4x]
  { InstDB::RWInfo::kCategoryGeneric   , 8 , { 11, 56, 17, 0 , 0 , 0  } }  // #177 [ref=8x]
};

const InstDB::RWInfoOp InstDB::rwInfoOp[] = {
  { 0x0000000000000000u, 0x0000000000000000u, 0xFF, 0, { 0 }, OpRWFlags::kNone }, // #0 [ref=17086x]
  { 0x0000000000000003u, 0x0000000000000003u, 0x00, 0, { 0 }, OpRWFlags::kRW | OpRWFlags::kRegPhysId }, // #1 [ref=10x]
  { 0x0000000000000000u, 0x0000000000000000u, 0xFF, 0, { 0 }, OpRWFlags::kRW | OpRWFlags::kZExt }, // #2 [ref=280x]
  { 0x0000000000000000u, 0x0000000000000000u, 0xFF, 0, { 0 }, OpRWFlags::kRead }, // #3 [ref=1132x]
  { 0x000000000000FFFFu, 0x000000000000FFFFu, 0xFF, 0, { 0 }, OpRWFlags::kRW | OpRWFlags::kZExt }, // #4 [ref=107x]
  { 0x000000000000FFFFu, 0x0000000000000000u, 0xFF, 0, { 0 }, OpRWFlags::kRead }, // #5 [ref=356x]
  { 0x00000000000000FFu, 0x00000000000000FFu, 0xFF, 0, { 0 }, OpRWFlags::kRW }, // #6 [ref=18x]
  { 0x00000000000000FFu, 0x0000000000000000u, 0xFF, 0, { 0 }, OpRWFlags::kRead }, // #7 [ref=186x]
  { 0x000000000000000Fu, 0x000000000000000Fu, 0xFF, 0, { 0 }, OpRWFlags::kRW }, // #8 [ref=18x]
  { 0x000000000000000Fu, 0x0000000000000000u, 0xFF, 0, { 0 }, OpRWFlags::kRead }, // #9 [ref=135x]
  { 0x0000000000000000u, 0x000000000000FFFFu, 0xFF, 0, { 0 }, OpRWFlags::kWrite | OpRWFlags::kZExt }, // #10 [ref=184x]
  { 0x0000000000000000u, 0x0000000000000000u, 0xFF, 0, { 0 }, OpRWFlags::kWrite | OpRWFlags::kZExt }, // #11 [ref=461x]
  { 0x0000000000000003u, 0x0000000000000003u, 0xFF, 0, { 0 }, OpRWFlags::kRW }, // #12 [ref=1x]
  { 0x0000000000000003u, 0x0000000000000000u, 0xFF, 0, { 0 }, OpRWFlags::kRead }, // #13 [ref=65x]
  { 0x000000000000FFFFu, 0x0000000000000000u, 0x00, 0, { 0 }, OpRWFlags::kRead | OpRWFlags::kRegPhysId }, // #14 [ref=4x]
  { 0x0000000000000000u, 0x0000000000000000u, 0xFF, 0, { 0 }, OpRWFlags::kWrite | OpRWFlags::kZExt | OpRWFlags::kMemBaseWrite | OpRWFlags::kMemIndexWrite }, // #15 [ref=1x]
  { 0x0000000000000000u, 0x000000000000000Fu, 0x02, 0, { 0 }, OpRWFlags::kWrite | OpRWFlags::kZExt | OpRWFlags::kRegPhysId }, // #16 [ref=9x]
  { 0x000000000000000Fu, 0x0000000000000000u, 0x00, 0, { 0 }, OpRWFlags::kRead | OpRWFlags::kRegPhysId }, // #17 [ref=23x]
  { 0x00000000000000FFu, 0x00000000000000FFu, 0x00, 0, { 0 }, OpRWFlags::kRW | OpRWFlags::kZExt | OpRWFlags::kRegPhysId }, // #18 [ref=2x]
  { 0xFFFFFFFFFFFFFFFFu, 0x0000000000000000u, 0x00, 0, { 0 }, OpRWFlags::kRead | OpRWFlags::kMemPhysId }, // #19 [ref=1x]
  { 0x0000000000000000u, 0x0000000000000000u, 0x06, 0, { 0 }, OpRWFlags::kRead | OpRWFlags::kMemBaseRW | OpRWFlags::kMemBasePostModify | OpRWFlags::kMemPhysId }, // #20 [ref=3x]
  { 0x0000000000000000u, 0x0000000000000000u, 0x07, 0, { 0 }, OpRWFlags::kRead | OpRWFlags::kMemBaseRW | OpRWFlags::kMemBasePostModify | OpRWFlags::kMemPhysId }, // #21 [ref=2x]
  { 0x0000000000000000u, 0x0000000000000000u, 0x00, 0, { 0 }, OpRWFlags::kRead | OpRWFlags::kRegPhysId }, // #22 [ref=8x]
  { 0x00000000000000FFu, 0x00000000000000FFu, 0x02, 0, { 0 }, OpRWFlags::kRW | OpRWFlags::kZExt | OpRWFlags::kRegPhysId }, // #23 [ref=1x]
  { 0x00000000000000FFu, 0x0000000000000000u, 0x01, 0, { 0 }, OpRWFlags::kRead | OpRWFlags::kRegPhysId }, // #24 [ref=1x]
  { 0x00000000000000FFu, 0x0000000000000000u, 0x03, 0, { 0 }, OpRWFlags::kRead | OpRWFlags::kRegPhysId }, // #25 [ref=1x]
  { 0x00000000000000FFu, 0x00000000000000FFu, 0xFF, 0, { 0 }, OpRWFlags::kRW | OpRWFlags::kZExt }, // #26 [ref=21x]
  { 0x000000000000000Fu, 0x000000000000000Fu, 0x02, 0, { 0 }, OpRWFlags::kRW | OpRWFlags::kZExt | OpRWFlags::kRegPhysId }, // #27 [ref=1x]
  { 0x000000000000000Fu, 0x000000000000000Fu, 0x00, 0, { 0 }, OpRWFlags::kRW | OpRWFlags::kZExt | OpRWFlags::kRegPhysId }, // #28 [ref=5x]
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
  { 0x0000000000000000u, 0xFFFFFFFFFFFFFFFFu, 0xFF, 0, { 0 }, OpRWFlags::kWrite | OpRWFlags::kZExt }, // #43 [ref=23x]
  { 0xFFFFFFFFFFFFFFFFu, 0x0000000000000000u, 0xFF, 0, { 0 }, OpRWFlags::kRead }, // #44 [ref=35x]
  { 0x0000000000000000u, 0x000000000000000Fu, 0xFF, 0, { 0 }, OpRWFlags::kWrite | OpRWFlags::kZExt }, // #45 [ref=30x]
  { 0x00000000000003FFu, 0x00000000000003FFu, 0xFF, 0, { 0 }, OpRWFlags::kRW | OpRWFlags::kZExt }, // #46 [ref=22x]
  { 0x00000000000003FFu, 0x0000000000000000u, 0xFF, 0, { 0 }, OpRWFlags::kRead }, // #47 [ref=13x]
  { 0x0000000000000000u, 0x00000000000003FFu, 0xFF, 0, { 0 }, OpRWFlags::kWrite | OpRWFlags::kZExt }, // #48 [ref=1x]
  { 0x0000000000000000u, 0x0000000000000003u, 0xFF, 0, { 0 }, OpRWFlags::kWrite | OpRWFlags::kZExt }, // #49 [ref=17x]
  { 0x0000000000000000u, 0x0000000000000003u, 0x00, 0, { 0 }, OpRWFlags::kWrite | OpRWFlags::kRegPhysId | OpRWFlags::kZExt }, // #50 [ref=2x]
  { 0x0000000000000000u, 0x000000000000000Fu, 0x00, 0, { 0 }, OpRWFlags::kWrite | OpRWFlags::kZExt | OpRWFlags::kRegPhysId }, // #51 [ref=8x]
  { 0x0000000000000000u, 0x0000000000000000u, 0x00, 0, { 0 }, OpRWFlags::kWrite | OpRWFlags::kRegPhysId | OpRWFlags::kZExt }, // #52 [ref=2x]
  { 0x0000000000000003u, 0x0000000000000000u, 0x02, 0, { 0 }, OpRWFlags::kRead | OpRWFlags::kRegPhysId }, // #53 [ref=4x]
  { 0x000000000000000Fu, 0x000000000000000Fu, 0xFF, 0, { 0 }, OpRWFlags::kRW | OpRWFlags::kZExt }, // #54 [ref=4x]
  { 0x0000000000000000u, 0x0000000000000000u, 0x07, 0, { 0 }, OpRWFlags::kWrite | OpRWFlags::kZExt | OpRWFlags::kMemPhysId }, // #55 [ref=1x]
  { 0x000000000000000Fu, 0x0000000000000000u, 0x02, 0, { 0 }, OpRWFlags::kRead | OpRWFlags::kRegPhysId }, // #56 [ref=23x]
  { 0x0000000000000000u, 0x0000000000000000u, 0x01, 0, { 0 }, OpRWFlags::kRead | OpRWFlags::kRegPhysId }, // #57 [ref=2x]
  { 0x0000000000000000u, 0x0000000000000001u, 0xFF, 0, { 0 }, OpRWFlags::kWrite | OpRWFlags::kZExt }, // #58 [ref=14x]
  { 0x0000000000000000u, 0x0000000000000001u, 0x00, 0, { 0 }, OpRWFlags::kWrite | OpRWFlags::kRegPhysId }, // #59 [ref=1x]
  { 0x0000000000000000u, 0x0000000000000000u, 0x01, 0, { 0 }, OpRWFlags::kRW | OpRWFlags::kRegPhysId | OpRWFlags::kZExt }, // #60 [ref=3x]
  { 0x000000000000FFFFu, 0x000000000000FFFFu, 0x07, 0, { 0 }, OpRWFlags::kRW | OpRWFlags::kZExt | OpRWFlags::kMemPhysId }, // #61 [ref=2x]
  { 0x00000000000000FFu, 0x00000000000000FFu, 0x07, 0, { 0 }, OpRWFlags::kRW | OpRWFlags::kZExt | OpRWFlags::kMemPhysId }, // #62 [ref=1x]
  { 0x0000000000000000u, 0x0000000000000000u, 0x00, 0, { 0 }, OpRWFlags::kRead | OpRWFlags::kMemPhysId }, // #63 [ref=2x]
  { 0x000000000000FF00u, 0x0000000000000000u, 0xFF, 0, { 0 }, OpRWFlags::kRead }, // #64 [ref=23x]
  { 0x0000000000000000u, 0x000000000000FF00u, 0xFF, 0, { 0 }, OpRWFlags::kWrite }, // #65 [ref=1x]
  { 0x0000000000000000u, 0x0000000000000000u, 0x07, 0, { 0 }, OpRWFlags::kWrite | OpRWFlags::kZExt | OpRWFlags::kMemBaseRW | OpRWFlags::kMemBasePostModify | OpRWFlags::kMemPhysId }, // #66 [ref=2x]
  { 0x0000000000000000u, 0x0000000000000000u, 0x02, 0, { 0 }, OpRWFlags::kWrite | OpRWFlags::kRegPhysId | OpRWFlags::kZExt }, // #67 [ref=1x]
  { 0x0000000000000000u, 0x0000000000000000u, 0x02, 0, { 0 }, OpRWFlags::kRead | OpRWFlags::kRegPhysId }, // #68 [ref=1x]
  { 0x0000000000000000u, 0x0000000000000000u, 0x06, 0, { 0 }, OpRWFlags::kRead | OpRWFlags::kMemPhysId }, // #69 [ref=1x]
  { 0x0000000000000000u, 0x000000000000000Fu, 0x01, 0, { 0 }, OpRWFlags::kWrite | OpRWFlags::kZExt | OpRWFlags::kRegPhysId }, // #70 [ref=5x]
  { 0x0000000000000000u, 0x000000000000FFFFu, 0x00, 0, { 0 }, OpRWFlags::kWrite | OpRWFlags::kZExt | OpRWFlags::kRegPhysId }, // #71 [ref=4x]
  { 0x0000000000000000u, 0x0000000000000007u, 0xFF, 0, { 0 }, OpRWFlags::kWrite | OpRWFlags::kZExt }, // #72 [ref=2x]
  { 0x0000000000000001u, 0x0000000000000000u, 0x01, 0, { 0 }, OpRWFlags::kRead | OpRWFlags::kRegPhysId }, // #73 [ref=10x]
  { 0x0000000000000001u, 0x0000000000000000u, 0x00, 0, { 0 }, OpRWFlags::kRead | OpRWFlags::kRegPhysId }, // #74 [ref=1x]
  { 0x0000000000000000u, 0x0000000000000001u, 0xFF, 0, { 0 }, OpRWFlags::kWrite }, // #75 [ref=30x]
  { 0xFFFFFFFFFFFFFFFFu, 0xFFFFFFFFFFFFFFFFu, 0xFF, 0, { 0 }, OpRWFlags::kRW | OpRWFlags::kZExt }, // #76 [ref=10x]
  { 0xFFFFFFFFFFFFFFFFu, 0x0000000000000000u, 0xFF, 4, { 0 }, OpRWFlags::kRead }, // #77 [ref=4x]
  { 0xFFFFFFFFFFFFFFFFu, 0x0000000000000000u, 0xFF, 0, { 0 }, OpRWFlags::kRead | OpRWFlags::kConsecutive }, // #78 [ref=12x]
  { 0x000000000000FFFFu, 0x0000000000000000u, 0xFF, 4, { 0 }, OpRWFlags::kRead }, // #79 [ref=2x]
  { 0x000000000000FFFFu, 0x0000000000000000u, 0xFF, 0, { 0 }, OpRWFlags::kRead | OpRWFlags::kConsecutive }, // #80 [ref=6x]
  { 0x0000000000000000u, 0x00000000FFFFFFFFu, 0xFF, 0, { 0 }, OpRWFlags::kWrite | OpRWFlags::kZExt }, // #81 [ref=10x]
  { 0x00000000FFFFFFFFu, 0x0000000000000000u, 0xFF, 0, { 0 }, OpRWFlags::kRead }, // #82 [ref=18x]
  { 0x000000000000FFF0u, 0x0000000000000000u, 0xFF, 0, { 0 }, OpRWFlags::kRead }, // #83 [ref=18x]
  { 0x0000000000000000u, 0x0000000000000000u, 0xFF, 0, { 0 }, OpRWFlags::kRW | OpRWFlags::kUnique | OpRWFlags::kZExt }, // #84 [ref=4x]
  { 0x000000000000FFFFu, 0x000000000000FFFFu, 0xFF, 0, { 0 }, OpRWFlags::kRW | OpRWFlags::kUnique | OpRWFlags::kZExt }, // #85 [ref=4x]
  { 0x000000000000FFFCu, 0x0000000000000000u, 0xFF, 0, { 0 }, OpRWFlags::kRead }, // #86 [ref=8x]
  { 0x0000000000000000u, 0x0000000000000000u, 0x00, 0, { 0 }, OpRWFlags::kRW | OpRWFlags::kZExt | OpRWFlags::kRegPhysId }, // #87 [ref=1x]
  { 0x0000000000000000u, 0x00000000000000FFu, 0xFF, 2, { 0 }, OpRWFlags::kWrite | OpRWFlags::kZExt }, // #88 [ref=2x]
  { 0x0000000000000000u, 0x0000000000000000u, 0xFF, 0, { 0 }, OpRWFlags::kWrite | OpRWFlags::kZExt | OpRWFlags::kConsecutive }, // #89 [ref=2x]
  { 0x00000000FFFFFFFFu, 0x00000000FFFFFFFFu, 0xFF, 0, { 0 }, OpRWFlags::kRW | OpRWFlags::kZExt }  // #90 [ref=3x]
};

const InstDB::RWInfoRm InstDB::rwInfoRm[] = {
  { InstDB::RWInfoRm::kCategoryNone      , 0x00, 0 , 0, 0 }, // #0 [ref=2083x]
  { InstDB::RWInfoRm::kCategoryConsistent, 0x03, 0 , InstDB::RWInfoRm::kFlagAmbiguous, 0 }, // #1 [ref=8x]
  { InstDB::RWInfoRm::kCategoryConsistent, 0x02, 0 , 0, 0 }, // #2 [ref=204x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x02, 16, 0, 0 }, // #3 [ref=122x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x02, 8 , 0, 0 }, // #4 [ref=66x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x02, 4 , 0, 0 }, // #5 [ref=35x]
  { InstDB::RWInfoRm::kCategoryConsistent, 0x04, 0 , 0, 0 }, // #6 [ref=314x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x01, 2 , 0, 0 }, // #7 [ref=9x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x00, 0 , 0, 0 }, // #8 [ref=68x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x03, 0 , 0, 0 }, // #9 [ref=1x]
  { InstDB::RWInfoRm::kCategoryConsistent, 0x01, 0 , InstDB::RWInfoRm::kFlagAmbiguous, 0 }, // #10 [ref=21x]
  { InstDB::RWInfoRm::kCategoryConsistent, 0x01, 0 , 0, 0 }, // #11 [ref=14x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x00, 8 , 0, 0 }, // #12 [ref=23x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x00, 64, 0, 0 }, // #13 [ref=6x]
  { InstDB::RWInfoRm::kCategoryNone      , 0x00, 0 , InstDB::RWInfoRm::kFlagAmbiguous, 0 }, // #14 [ref=30x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x00, 16, 0, 0 }, // #15 [ref=23x]
  { InstDB::RWInfoRm::kCategoryConsistent, 0x02, 0 , InstDB::RWInfoRm::kFlagAmbiguous, 0 }, // #16 [ref=22x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x02, 1 , 0, 0 }, // #17 [ref=5x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x01, 4 , 0, 0 }, // #18 [ref=6x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x00, 10, 0, 0 }, // #19 [ref=2x]
  { InstDB::RWInfoRm::kCategoryNone      , 0x01, 0 , InstDB::RWInfoRm::kFlagAmbiguous, 0 }, // #20 [ref=5x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x00, 2 , 0, 0 }, // #21 [ref=6x]
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
  { InstDB::RWInfoRm::kCategoryFixed     , 0x04, 16, 0, 0 }, // #50 [ref=30x]
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
  EXPECT_EQ(uint32_t(InstOptions::kX86_Rex ), 0x40000000u)
    .message("REX prefix must be at 0x40000000");

  EXPECT_EQ(uint32_t(InstOptions::kX86_Evex), 0x00001000u)
    .message("EVEX prefix must be at 0x00001000");

  // These could be combined together to form a valid REX prefix, they must match.
  EXPECT_EQ(uint32_t(InstOptions::kX86_OpCodeB), uint32_t(Opcode::kB))
    .message("Opcode::kB must match InstOptions::kX86_OpCodeB");

  EXPECT_EQ(uint32_t(InstOptions::kX86_OpCodeX), uint32_t(Opcode::kX))
    .message("Opcode::kX must match InstOptions::kX86_OpCodeX");

  EXPECT_EQ(uint32_t(InstOptions::kX86_OpCodeR), uint32_t(Opcode::kR))
    .message("Opcode::kR must match InstOptions::kX86_OpCodeR");

  EXPECT_EQ(uint32_t(InstOptions::kX86_OpCodeW), uint32_t(Opcode::kW))
    .message("Opcode::kW must match InstOptions::kX86_OpCodeW");

  uint32_t rex_rb = (Opcode::kR >> Opcode::kREX_Shift) | (Opcode::kB >> Opcode::kREX_Shift) | 0x40;
  uint32_t rex_rw = (Opcode::kR >> Opcode::kREX_Shift) | (Opcode::kW >> Opcode::kREX_Shift) | 0x40;

  EXPECT_EQ(rex_rb, 0x45u)
    .message("Opcode::kR|B must form a valid REX prefix (0x45) if combined with 0x40");

  EXPECT_EQ(rex_rw, 0x4Cu)
    .message("Opcode::kR|W must form a valid REX prefix (0x4C) if combined with 0x40");
}
#endif

ASMJIT_END_SUB_NAMESPACE

#endif // !ASMJIT_NO_X86
