// This file is part of AsmJit project <https://asmjit.com>
//
// See <asmjit/core.h> or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#include <asmjit/core/api_build_p.h>
#if !defined(ASMJIT_NO_X86)

#include <asmjit/core/cpu_info.h>
#include <asmjit/core/globals.h>
#include <asmjit/core/misc_p.h>
#include <asmjit/x86/x86_inst_db_p.h>
#include <asmjit/x86/x86_opcode_p.h>
#include <asmjit/x86/x86_operand.h>

ASMJIT_BEGIN_SUB_NAMESPACE(x86)

// x86::InstDB - InstInfo
// ======================

// Instruction opcode definitions:
//   - `O` encodes X86|MMX|SSE instructions.
//   - `V` encodes VEX|XOP|EVEX instructions.
//   - `E` encodes EVEX instructions only.
#define O_ENCODE(PREFIX, OPCODE, O, L, W, EvexW, N, TT) ((PREFIX) | (OPCODE) | (O) | (L) | (W) | (EvexW) | (N) | (TT))
#define O_FPU(PREFIX, OPCODE, ModO) (Opcode::kFPU_##PREFIX | (0x##OPCODE & 0xFFu) | ((0x##OPCODE >> 8) << Opcode::kFPU_2B_Shift) | Opcode::kModO_##ModO)
#define O(PREFIX, OPCODE, ModO, LL, W, EvexW, N, ModRM) (O_ENCODE(Opcode::k##PREFIX, 0x##OPCODE, Opcode::kModO_##ModO, Opcode::kLL_##LL, Opcode::kW_##W, Opcode::kEvex_W_##EvexW, Opcode::kCDSHL_##N, Opcode::kModRM_##ModRM))
#define V(PREFIX, OPCODE, ModO, LL, W, EvexW, N, TT) (O_ENCODE(Opcode::k##PREFIX, 0x##OPCODE, Opcode::kModO_##ModO, Opcode::kLL_##LL, Opcode::kW_##W, Opcode::kEvex_W_##EvexW, Opcode::kCDSHL_##N, Opcode::kCDTT_##TT))
#define E(PREFIX, OPCODE, ModO, LL, W, EvexW, N, TT) (O_ENCODE(Opcode::k##PREFIX, 0x##OPCODE, Opcode::kModO_##ModO, Opcode::kLL_##LL, Opcode::kW_##W, Opcode::kEvex_W_##EvexW, Opcode::kCDSHL_##N, Opcode::kCDTT_##TT) | Opcode::kMM_ForceEvex)

// Defines an X86 instruction.
#define INST(id, encoding, opcode0, opcode1, main_opcode_index, alt_opcode_index, common_info_index, additional_info_index) { \
  uint32_t(0),                              \
  uint32_t(common_info_index),              \
  uint32_t(additional_info_index),          \
  uint8_t(InstDB::kEncoding##encoding),     \
  uint8_t((opcode0) & 0xFFu),               \
  uint8_t(main_opcode_index),               \
  uint8_t(alt_opcode_index)                 \
}

const InstDB::InstInfo InstDB::_inst_info_table[] = {
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
  INST(Bound            , X86Rm              , O(000000,62,_,_,_,_,_,_  ), 0                         , 0  , 0  , 17 , 0  ), // #47
  INST(Bsf              , X86Rm              , O(000F00,BC,_,_,x,_,_,_  ), 0                         , 5  , 0  , 18 , 1  ), // #48
  INST(Bsr              , X86Rm              , O(000F00,BD,_,_,x,_,_,_  ), 0                         , 5  , 0  , 18 , 1  ), // #49
  INST(Bswap            , X86Bswap           , O(000F00,C8,_,_,x,_,_,_  ), 0                         , 5  , 0  , 19 , 0  ), // #50
  INST(Bt               , X86Bt              , O(000F00,A3,_,_,x,_,_,_  ), O(000F00,BA,4,_,x,_,_,_  ), 5  , 1  , 20 , 14 ), // #51
  INST(Btc              , X86Bt              , O(000F00,BB,_,_,x,_,_,_  ), O(000F00,BA,7,_,x,_,_,_  ), 5  , 2  , 21 , 14 ), // #52
  INST(Btr              , X86Bt              , O(000F00,B3,_,_,x,_,_,_  ), O(000F00,BA,6,_,x,_,_,_  ), 5  , 3  , 21 , 14 ), // #53
  INST(Bts              , X86Bt              , O(000F00,AB,_,_,x,_,_,_  ), O(000F00,BA,5,_,x,_,_,_  ), 5  , 4  , 21 , 14 ), // #54
  INST(Bzhi             , VexRmv_Wx          , V(000F38,F5,_,0,x,_,_,_  ), 0                         , 11 , 0  , 14 , 15 ), // #55
  INST(Call             , X86Call            , O(000000,FF,2,_,_,_,_,_  ), 0                         , 3  , 0  , 22 , 1  ), // #56
  INST(Cbw              , X86Op_xAX          , O(660000,98,_,_,_,_,_,_  ), 0                         , 21 , 0  , 23 , 0  ), // #57
  INST(Cdq              , X86Op_xDX_xAX      , O(000000,99,_,_,_,_,_,_  ), 0                         , 0  , 0  , 24 , 0  ), // #58
  INST(Cdqe             , X86Op_xAX          , O(000000,98,_,_,1,_,_,_  ), 0                         , 22 , 0  , 25 , 0  ), // #59
  INST(Clac             , X86Op              , O(000F01,CA,_,_,_,_,_,_  ), 0                         , 23 , 0  , 26 , 16 ), // #60
  INST(Clc              , X86Op              , O(000000,F8,_,_,_,_,_,_  ), 0                         , 0  , 0  , 26 , 17 ), // #61
  INST(Cld              , X86Op              , O(000000,FC,_,_,_,_,_,_  ), 0                         , 0  , 0  , 26 , 18 ), // #62
  INST(Cldemote         , X86M_Only          , O(000F00,1C,0,_,_,_,_,_  ), 0                         , 5  , 0  , 27 , 19 ), // #63
  INST(Clflush          , X86M_Only          , O(000F00,AE,7,_,_,_,_,_  ), 0                         , 24 , 0  , 27 , 20 ), // #64
  INST(Clflushopt       , X86M_Only          , O(660F00,AE,7,_,_,_,_,_  ), 0                         , 25 , 0  , 27 , 21 ), // #65
  INST(Clgi             , X86Op              , O(000F01,DD,_,_,_,_,_,_  ), 0                         , 23 , 0  , 26 , 22 ), // #66
  INST(Cli              , X86Op              , O(000000,FA,_,_,_,_,_,_  ), 0                         , 0  , 0  , 26 , 23 ), // #67
  INST(Clrssbsy         , X86M_Only          , O(F30F00,AE,6,_,_,_,_,_  ), 0                         , 26 , 0  , 28 , 24 ), // #68
  INST(Clts             , X86Op              , O(000F00,06,_,_,_,_,_,_  ), 0                         , 5  , 0  , 26 , 0  ), // #69
  INST(Clui             , X86Op              , O(F30F01,EE,_,_,_,_,_,_  ), 0                         , 27 , 0  , 29 , 25 ), // #70
  INST(Clwb             , X86M_Only          , O(660F00,AE,6,_,_,_,_,_  ), 0                         , 28 , 0  , 27 , 26 ), // #71
  INST(Clzero           , X86Op_MemZAX       , O(000F01,FC,_,_,_,_,_,_  ), 0                         , 23 , 0  , 30 , 27 ), // #72
  INST(Cmc              , X86Op              , O(000000,F5,_,_,_,_,_,_  ), 0                         , 0  , 0  , 26 , 28 ), // #73
  INST(Cmovb            , X86Rm              , O(000F00,42,_,_,x,_,_,_  ), 0                         , 5  , 0  , 18 , 29 ), // #74
  INST(Cmovbe           , X86Rm              , O(000F00,46,_,_,x,_,_,_  ), 0                         , 5  , 0  , 18 , 30 ), // #75
  INST(Cmovl            , X86Rm              , O(000F00,4C,_,_,x,_,_,_  ), 0                         , 5  , 0  , 18 , 31 ), // #76
  INST(Cmovle           , X86Rm              , O(000F00,4E,_,_,x,_,_,_  ), 0                         , 5  , 0  , 18 , 32 ), // #77
  INST(Cmovnb           , X86Rm              , O(000F00,43,_,_,x,_,_,_  ), 0                         , 5  , 0  , 18 , 29 ), // #78
  INST(Cmovnbe          , X86Rm              , O(000F00,47,_,_,x,_,_,_  ), 0                         , 5  , 0  , 18 , 30 ), // #79
  INST(Cmovnl           , X86Rm              , O(000F00,4D,_,_,x,_,_,_  ), 0                         , 5  , 0  , 18 , 31 ), // #80
  INST(Cmovnle          , X86Rm              , O(000F00,4F,_,_,x,_,_,_  ), 0                         , 5  , 0  , 18 , 32 ), // #81
  INST(Cmovno           , X86Rm              , O(000F00,41,_,_,x,_,_,_  ), 0                         , 5  , 0  , 18 , 33 ), // #82
  INST(Cmovnp           , X86Rm              , O(000F00,4B,_,_,x,_,_,_  ), 0                         , 5  , 0  , 18 , 34 ), // #83
  INST(Cmovns           , X86Rm              , O(000F00,49,_,_,x,_,_,_  ), 0                         , 5  , 0  , 18 , 35 ), // #84
  INST(Cmovnz           , X86Rm              , O(000F00,45,_,_,x,_,_,_  ), 0                         , 5  , 0  , 18 , 36 ), // #85
  INST(Cmovo            , X86Rm              , O(000F00,40,_,_,x,_,_,_  ), 0                         , 5  , 0  , 18 , 33 ), // #86
  INST(Cmovp            , X86Rm              , O(000F00,4A,_,_,x,_,_,_  ), 0                         , 5  , 0  , 18 , 34 ), // #87
  INST(Cmovs            , X86Rm              , O(000F00,48,_,_,x,_,_,_  ), 0                         , 5  , 0  , 18 , 35 ), // #88
  INST(Cmovz            , X86Rm              , O(000F00,44,_,_,x,_,_,_  ), 0                         , 5  , 0  , 18 , 36 ), // #89
  INST(Cmp              , X86Arith           , O(000000,38,7,_,x,_,_,_  ), 0                         , 29 , 0  , 31 , 1  ), // #90
  INST(Cmpbexadd        , VexMvr_Wx          , V(660F38,E6,_,0,x,_,_,_  ), 0                         , 30 , 0  , 32 , 37 ), // #91
  INST(Cmpbxadd         , VexMvr_Wx          , V(660F38,E2,_,0,x,_,_,_  ), 0                         , 30 , 0  , 32 , 37 ), // #92
  INST(Cmplexadd        , VexMvr_Wx          , V(660F38,EE,_,0,x,_,_,_  ), 0                         , 30 , 0  , 32 , 37 ), // #93
  INST(Cmplxadd         , VexMvr_Wx          , V(660F38,EC,_,0,x,_,_,_  ), 0                         , 30 , 0  , 32 , 37 ), // #94
  INST(Cmpnbexadd       , VexMvr_Wx          , V(660F38,E7,_,0,x,_,_,_  ), 0                         , 30 , 0  , 32 , 37 ), // #95
  INST(Cmpnbxadd        , VexMvr_Wx          , V(660F38,E3,_,0,x,_,_,_  ), 0                         , 30 , 0  , 32 , 37 ), // #96
  INST(Cmpnlexadd       , VexMvr_Wx          , V(660F38,EF,_,0,x,_,_,_  ), 0                         , 30 , 0  , 32 , 37 ), // #97
  INST(Cmpnlxadd        , VexMvr_Wx          , V(660F38,ED,_,0,x,_,_,_  ), 0                         , 30 , 0  , 32 , 37 ), // #98
  INST(Cmpnoxadd        , VexMvr_Wx          , V(660F38,E1,_,0,x,_,_,_  ), 0                         , 30 , 0  , 32 , 37 ), // #99
  INST(Cmpnpxadd        , VexMvr_Wx          , V(660F38,EB,_,0,x,_,_,_  ), 0                         , 30 , 0  , 32 , 37 ), // #100
  INST(Cmpnsxadd        , VexMvr_Wx          , V(660F38,E9,_,0,x,_,_,_  ), 0                         , 30 , 0  , 32 , 37 ), // #101
  INST(Cmpnzxadd        , VexMvr_Wx          , V(660F38,E5,_,0,x,_,_,_  ), 0                         , 30 , 0  , 32 , 37 ), // #102
  INST(Cmpoxadd         , VexMvr_Wx          , V(660F38,E0,_,0,x,_,_,_  ), 0                         , 30 , 0  , 32 , 37 ), // #103
  INST(Cmppd            , ExtRmi             , O(660F00,C2,_,_,_,_,_,_  ), 0                         , 4  , 0  , 9  , 5  ), // #104
  INST(Cmpps            , ExtRmi             , O(000F00,C2,_,_,_,_,_,_  ), 0                         , 5  , 0  , 9  , 6  ), // #105
  INST(Cmppxadd         , VexMvr_Wx          , V(660F38,EA,_,0,x,_,_,_  ), 0                         , 30 , 0  , 32 , 37 ), // #106
  INST(Cmps             , X86StrMm           , O(000000,A6,_,_,_,_,_,_  ), 0                         , 0  , 0  , 33 , 38 ), // #107
  INST(Cmpsd            , ExtRmi             , O(F20F00,C2,_,_,_,_,_,_  ), 0                         , 6  , 0  , 34 , 5  ), // #108
  INST(Cmpss            , ExtRmi             , O(F30F00,C2,_,_,_,_,_,_  ), 0                         , 7  , 0  , 35 , 6  ), // #109
  INST(Cmpsxadd         , VexMvr_Wx          , V(660F38,E8,_,0,x,_,_,_  ), 0                         , 30 , 0  , 32 , 37 ), // #110
  INST(Cmpxchg          , X86Cmpxchg         , O(000F00,B0,_,_,x,_,_,_  ), 0                         , 5  , 0  , 36 , 39 ), // #111
  INST(Cmpxchg16b       , X86Cmpxchg8b_16b   , O(000F00,C7,1,_,1,_,_,_  ), 0                         , 31 , 0  , 37 , 40 ), // #112
  INST(Cmpxchg8b        , X86Cmpxchg8b_16b   , O(000F00,C7,1,_,_,_,_,_  ), 0                         , 32 , 0  , 38 , 41 ), // #113
  INST(Cmpzxadd         , VexMvr_Wx          , V(660F38,E4,_,0,x,_,_,_  ), 0                         , 30 , 0  , 32 , 37 ), // #114
  INST(Comisd           , ExtRm              , O(660F00,2F,_,_,_,_,_,_  ), 0                         , 4  , 0  , 7  , 42 ), // #115
  INST(Comiss           , ExtRm              , O(000F00,2F,_,_,_,_,_,_  ), 0                         , 5  , 0  , 8  , 43 ), // #116
  INST(Cpuid            , X86Op              , O(000F00,A2,_,_,_,_,_,_  ), 0                         , 5  , 0  , 39 , 44 ), // #117
  INST(Cqo              , X86Op_xDX_xAX      , O(000000,99,_,_,1,_,_,_  ), 0                         , 22 , 0  , 40 , 0  ), // #118
  INST(Crc32            , X86Crc             , O(F20F38,F0,_,_,x,_,_,_  ), 0                         , 12 , 0  , 41 , 45 ), // #119
  INST(Cvtdq2pd         , ExtRm              , O(F30F00,E6,_,_,_,_,_,_  ), 0                         , 7  , 0  , 7  , 5  ), // #120
  INST(Cvtdq2ps         , ExtRm              , O(000F00,5B,_,_,_,_,_,_  ), 0                         , 5  , 0  , 6  , 5  ), // #121
  INST(Cvtpd2dq         , ExtRm              , O(F20F00,E6,_,_,_,_,_,_  ), 0                         , 6  , 0  , 6  , 5  ), // #122
  INST(Cvtpd2pi         , ExtRm              , O(660F00,2D,_,_,_,_,_,_  ), 0                         , 4  , 0  , 42 , 5  ), // #123
  INST(Cvtpd2ps         , ExtRm              , O(660F00,5A,_,_,_,_,_,_  ), 0                         , 4  , 0  , 6  , 5  ), // #124
  INST(Cvtpi2pd         , ExtRm              , O(660F00,2A,_,_,_,_,_,_  ), 0                         , 4  , 0  , 43 , 5  ), // #125
  INST(Cvtpi2ps         , ExtRm              , O(000F00,2A,_,_,_,_,_,_  ), 0                         , 5  , 0  , 43 , 6  ), // #126
  INST(Cvtps2dq         , ExtRm              , O(660F00,5B,_,_,_,_,_,_  ), 0                         , 4  , 0  , 6  , 5  ), // #127
  INST(Cvtps2pd         , ExtRm              , O(000F00,5A,_,_,_,_,_,_  ), 0                         , 5  , 0  , 7  , 5  ), // #128
  INST(Cvtps2pi         , ExtRm              , O(000F00,2D,_,_,_,_,_,_  ), 0                         , 5  , 0  , 44 , 6  ), // #129
  INST(Cvtsd2si         , ExtRm_Wx_GpqOnly   , O(F20F00,2D,_,_,x,_,_,_  ), 0                         , 6  , 0  , 45 , 5  ), // #130
  INST(Cvtsd2ss         , ExtRm              , O(F20F00,5A,_,_,_,_,_,_  ), 0                         , 6  , 0  , 7  , 5  ), // #131
  INST(Cvtsi2sd         , ExtRm_Wx           , O(F20F00,2A,_,_,x,_,_,_  ), 0                         , 6  , 0  , 46 , 5  ), // #132
  INST(Cvtsi2ss         , ExtRm_Wx           , O(F30F00,2A,_,_,x,_,_,_  ), 0                         , 7  , 0  , 46 , 6  ), // #133
  INST(Cvtss2sd         , ExtRm              , O(F30F00,5A,_,_,_,_,_,_  ), 0                         , 7  , 0  , 8  , 5  ), // #134
  INST(Cvtss2si         , ExtRm_Wx_GpqOnly   , O(F30F00,2D,_,_,x,_,_,_  ), 0                         , 7  , 0  , 47 , 6  ), // #135
  INST(Cvttpd2dq        , ExtRm              , O(660F00,E6,_,_,_,_,_,_  ), 0                         , 4  , 0  , 6  , 5  ), // #136
  INST(Cvttpd2pi        , ExtRm              , O(660F00,2C,_,_,_,_,_,_  ), 0                         , 4  , 0  , 42 , 5  ), // #137
  INST(Cvttps2dq        , ExtRm              , O(F30F00,5B,_,_,_,_,_,_  ), 0                         , 7  , 0  , 6  , 5  ), // #138
  INST(Cvttps2pi        , ExtRm              , O(000F00,2C,_,_,_,_,_,_  ), 0                         , 5  , 0  , 44 , 6  ), // #139
  INST(Cvttsd2si        , ExtRm_Wx_GpqOnly   , O(F20F00,2C,_,_,x,_,_,_  ), 0                         , 6  , 0  , 45 , 5  ), // #140
  INST(Cvttss2si        , ExtRm_Wx_GpqOnly   , O(F30F00,2C,_,_,x,_,_,_  ), 0                         , 7  , 0  , 47 , 6  ), // #141
  INST(Cwd              , X86Op_xDX_xAX      , O(660000,99,_,_,_,_,_,_  ), 0                         , 21 , 0  , 48 , 0  ), // #142
  INST(Cwde             , X86Op_xAX          , O(000000,98,_,_,_,_,_,_  ), 0                         , 0  , 0  , 49 , 0  ), // #143
  INST(Daa              , X86Op              , O(000000,27,_,_,_,_,_,_  ), 0                         , 0  , 0  , 1  , 1  ), // #144
  INST(Das              , X86Op              , O(000000,2F,_,_,_,_,_,_  ), 0                         , 0  , 0  , 1  , 1  ), // #145
  INST(Dec              , X86IncDec          , O(000000,FE,1,_,x,_,_,_  ), O(000000,48,_,_,x,_,_,_  ), 33 , 5  , 50 , 46 ), // #146
  INST(Div              , X86M_GPB_MulDiv    , O(000000,F6,6,_,x,_,_,_  ), 0                         , 34 , 0  , 51 , 1  ), // #147
  INST(Divpd            , ExtRm              , O(660F00,5E,_,_,_,_,_,_  ), 0                         , 4  , 0  , 6  , 5  ), // #148
  INST(Divps            , ExtRm              , O(000F00,5E,_,_,_,_,_,_  ), 0                         , 5  , 0  , 6  , 6  ), // #149
  INST(Divsd            , ExtRm              , O(F20F00,5E,_,_,_,_,_,_  ), 0                         , 6  , 0  , 7  , 5  ), // #150
  INST(Divss            , ExtRm              , O(F30F00,5E,_,_,_,_,_,_  ), 0                         , 7  , 0  , 8  , 6  ), // #151
  INST(Dppd             , ExtRmi             , O(660F3A,41,_,_,_,_,_,_  ), 0                         , 9  , 0  , 9  , 13 ), // #152
  INST(Dpps             , ExtRmi             , O(660F3A,40,_,_,_,_,_,_  ), 0                         , 9  , 0  , 9  , 13 ), // #153
  INST(Emms             , X86Op              , O(000F00,77,_,_,_,_,_,_  ), 0                         , 5  , 0  , 52 , 47 ), // #154
  INST(Endbr32          , X86Op_Mod11RM      , O(F30F00,1E,7,_,_,_,_,3  ), 0                         , 35 , 0  , 26 , 48 ), // #155
  INST(Endbr64          , X86Op_Mod11RM      , O(F30F00,1E,7,_,_,_,_,2  ), 0                         , 36 , 0  , 26 , 48 ), // #156
  INST(Enqcmd           , X86EnqcmdMovdir64b , O(F20F38,F8,_,_,_,_,_,_  ), 0                         , 12 , 0  , 53 , 49 ), // #157
  INST(Enqcmds          , X86EnqcmdMovdir64b , O(F30F38,F8,_,_,_,_,_,_  ), 0                         , 8  , 0  , 53 , 49 ), // #158
  INST(Enter            , X86Enter           , O(000000,C8,_,_,_,_,_,_  ), 0                         , 0  , 0  , 54 , 0  ), // #159
  INST(Extractps        , ExtExtract         , O(660F3A,17,_,_,_,_,_,_  ), 0                         , 9  , 0  , 55 , 13 ), // #160
  INST(Extrq            , ExtExtrq           , O(660F00,79,_,_,_,_,_,_  ), O(660F00,78,0,_,_,_,_,_  ), 4  , 6  , 56 , 50 ), // #161
  INST(F2xm1            , FpuOp              , O_FPU(00,D9F0,_)          , 0                         , 37 , 0  , 26 , 51 ), // #162
  INST(Fabs             , FpuOp              , O_FPU(00,D9E1,_)          , 0                         , 37 , 0  , 26 , 51 ), // #163
  INST(Fadd             , FpuArith           , O_FPU(00,C0C0,0)          , 0                         , 38 , 0  , 57 , 51 ), // #164
  INST(Faddp            , FpuRDef            , O_FPU(00,DEC0,_)          , 0                         , 39 , 0  , 58 , 51 ), // #165
  INST(Fbld             , X86M_Only          , O_FPU(00,00DF,4)          , 0                         , 40 , 0  , 59 , 51 ), // #166
  INST(Fbstp            , X86M_Only          , O_FPU(00,00DF,6)          , 0                         , 41 , 0  , 59 , 51 ), // #167
  INST(Fchs             , FpuOp              , O_FPU(00,D9E0,_)          , 0                         , 37 , 0  , 26 , 51 ), // #168
  INST(Fclex            , FpuOp              , O_FPU(9B,DBE2,_)          , 0                         , 42 , 0  , 26 , 51 ), // #169
  INST(Fcmovb           , FpuR               , O_FPU(00,DAC0,_)          , 0                         , 43 , 0  , 60 , 52 ), // #170
  INST(Fcmovbe          , FpuR               , O_FPU(00,DAD0,_)          , 0                         , 43 , 0  , 60 , 53 ), // #171
  INST(Fcmove           , FpuR               , O_FPU(00,DAC8,_)          , 0                         , 43 , 0  , 60 , 54 ), // #172
  INST(Fcmovnb          , FpuR               , O_FPU(00,DBC0,_)          , 0                         , 44 , 0  , 60 , 52 ), // #173
  INST(Fcmovnbe         , FpuR               , O_FPU(00,DBD0,_)          , 0                         , 44 , 0  , 60 , 53 ), // #174
  INST(Fcmovne          , FpuR               , O_FPU(00,DBC8,_)          , 0                         , 44 , 0  , 60 , 54 ), // #175
  INST(Fcmovnu          , FpuR               , O_FPU(00,DBD8,_)          , 0                         , 44 , 0  , 60 , 55 ), // #176
  INST(Fcmovu           , FpuR               , O_FPU(00,DAD8,_)          , 0                         , 43 , 0  , 60 , 55 ), // #177
  INST(Fcom             , FpuCom             , O_FPU(00,D0D0,2)          , 0                         , 45 , 0  , 61 , 51 ), // #178
  INST(Fcomi            , FpuR               , O_FPU(00,DBF0,_)          , 0                         , 44 , 0  , 60 , 56 ), // #179
  INST(Fcomip           , FpuR               , O_FPU(00,DFF0,_)          , 0                         , 46 , 0  , 60 , 56 ), // #180
  INST(Fcomp            , FpuCom             , O_FPU(00,D8D8,3)          , 0                         , 47 , 0  , 61 , 51 ), // #181
  INST(Fcompp           , FpuOp              , O_FPU(00,DED9,_)          , 0                         , 39 , 0  , 26 , 51 ), // #182
  INST(Fcos             , FpuOp              , O_FPU(00,D9FF,_)          , 0                         , 37 , 0  , 26 , 51 ), // #183
  INST(Fdecstp          , FpuOp              , O_FPU(00,D9F6,_)          , 0                         , 37 , 0  , 26 , 51 ), // #184
  INST(Fdiv             , FpuArith           , O_FPU(00,F0F8,6)          , 0                         , 48 , 0  , 57 , 51 ), // #185
  INST(Fdivp            , FpuRDef            , O_FPU(00,DEF8,_)          , 0                         , 39 , 0  , 58 , 51 ), // #186
  INST(Fdivr            , FpuArith           , O_FPU(00,F8F0,7)          , 0                         , 49 , 0  , 57 , 51 ), // #187
  INST(Fdivrp           , FpuRDef            , O_FPU(00,DEF0,_)          , 0                         , 39 , 0  , 58 , 51 ), // #188
  INST(Femms            , X86Op              , O(000F00,0E,_,_,_,_,_,_  ), 0                         , 5  , 0  , 26 , 57 ), // #189
  INST(Ffree            , FpuR               , O_FPU(00,DDC0,_)          , 0                         , 50 , 0  , 60 , 51 ), // #190
  INST(Fiadd            , FpuM               , O_FPU(00,00DA,0)          , 0                         , 51 , 0  , 62 , 51 ), // #191
  INST(Ficom            , FpuM               , O_FPU(00,00DA,2)          , 0                         , 52 , 0  , 62 , 51 ), // #192
  INST(Ficomp           , FpuM               , O_FPU(00,00DA,3)          , 0                         , 53 , 0  , 62 , 51 ), // #193
  INST(Fidiv            , FpuM               , O_FPU(00,00DA,6)          , 0                         , 41 , 0  , 62 , 51 ), // #194
  INST(Fidivr           , FpuM               , O_FPU(00,00DA,7)          , 0                         , 54 , 0  , 62 , 51 ), // #195
  INST(Fild             , FpuM               , O_FPU(00,00DB,0)          , O_FPU(00,00DF,5)          , 51 , 7  , 63 , 51 ), // #196
  INST(Fimul            , FpuM               , O_FPU(00,00DA,1)          , 0                         , 55 , 0  , 62 , 51 ), // #197
  INST(Fincstp          , FpuOp              , O_FPU(00,D9F7,_)          , 0                         , 37 , 0  , 26 , 51 ), // #198
  INST(Finit            , FpuOp              , O_FPU(9B,DBE3,_)          , 0                         , 42 , 0  , 26 , 51 ), // #199
  INST(Fist             , FpuM               , O_FPU(00,00DB,2)          , 0                         , 52 , 0  , 62 , 51 ), // #200
  INST(Fistp            , FpuM               , O_FPU(00,00DB,3)          , O_FPU(00,00DF,7)          , 53 , 8  , 63 , 51 ), // #201
  INST(Fisttp           , FpuM               , O_FPU(00,00DB,1)          , O_FPU(00,00DD,1)          , 55 , 9  , 63 , 58 ), // #202
  INST(Fisub            , FpuM               , O_FPU(00,00DA,4)          , 0                         , 40 , 0  , 62 , 51 ), // #203
  INST(Fisubr           , FpuM               , O_FPU(00,00DA,5)          , 0                         , 56 , 0  , 62 , 51 ), // #204
  INST(Fld              , FpuFldFst          , O_FPU(00,00D9,0)          , O_FPU(00,00DB,5)          , 51 , 10 , 64 , 51 ), // #205
  INST(Fld1             , FpuOp              , O_FPU(00,D9E8,_)          , 0                         , 37 , 0  , 26 , 51 ), // #206
  INST(Fldcw            , X86M_Only          , O_FPU(00,00D9,5)          , 0                         , 56 , 0  , 65 , 51 ), // #207
  INST(Fldenv           , X86M_Only          , O_FPU(00,00D9,4)          , 0                         , 40 , 0  , 27 , 51 ), // #208
  INST(Fldl2e           , FpuOp              , O_FPU(00,D9EA,_)          , 0                         , 37 , 0  , 26 , 51 ), // #209
  INST(Fldl2t           , FpuOp              , O_FPU(00,D9E9,_)          , 0                         , 37 , 0  , 26 , 51 ), // #210
  INST(Fldlg2           , FpuOp              , O_FPU(00,D9EC,_)          , 0                         , 37 , 0  , 26 , 51 ), // #211
  INST(Fldln2           , FpuOp              , O_FPU(00,D9ED,_)          , 0                         , 37 , 0  , 26 , 51 ), // #212
  INST(Fldpi            , FpuOp              , O_FPU(00,D9EB,_)          , 0                         , 37 , 0  , 26 , 51 ), // #213
  INST(Fldz             , FpuOp              , O_FPU(00,D9EE,_)          , 0                         , 37 , 0  , 26 , 51 ), // #214
  INST(Fmul             , FpuArith           , O_FPU(00,C8C8,1)          , 0                         , 57 , 0  , 57 , 51 ), // #215
  INST(Fmulp            , FpuRDef            , O_FPU(00,DEC8,_)          , 0                         , 39 , 0  , 58 , 51 ), // #216
  INST(Fnclex           , FpuOp              , O_FPU(00,DBE2,_)          , 0                         , 44 , 0  , 26 , 51 ), // #217
  INST(Fninit           , FpuOp              , O_FPU(00,DBE3,_)          , 0                         , 44 , 0  , 26 , 51 ), // #218
  INST(Fnop             , FpuOp              , O_FPU(00,D9D0,_)          , 0                         , 37 , 0  , 26 , 51 ), // #219
  INST(Fnsave           , X86M_Only          , O_FPU(00,00DD,6)          , 0                         , 41 , 0  , 27 , 51 ), // #220
  INST(Fnstcw           , X86M_Only          , O_FPU(00,00D9,7)          , 0                         , 54 , 0  , 65 , 51 ), // #221
  INST(Fnstenv          , X86M_Only          , O_FPU(00,00D9,6)          , 0                         , 41 , 0  , 27 , 51 ), // #222
  INST(Fnstsw           , FpuStsw            , O_FPU(00,00DD,7)          , O_FPU(00,DFE0,_)          , 54 , 11 , 66 , 51 ), // #223
  INST(Fpatan           , FpuOp              , O_FPU(00,D9F3,_)          , 0                         , 37 , 0  , 26 , 51 ), // #224
  INST(Fprem            , FpuOp              , O_FPU(00,D9F8,_)          , 0                         , 37 , 0  , 26 , 51 ), // #225
  INST(Fprem1           , FpuOp              , O_FPU(00,D9F5,_)          , 0                         , 37 , 0  , 26 , 51 ), // #226
  INST(Fptan            , FpuOp              , O_FPU(00,D9F2,_)          , 0                         , 37 , 0  , 26 , 51 ), // #227
  INST(Frndint          , FpuOp              , O_FPU(00,D9FC,_)          , 0                         , 37 , 0  , 26 , 51 ), // #228
  INST(Frstor           , X86M_Only          , O_FPU(00,00DD,4)          , 0                         , 40 , 0  , 27 , 51 ), // #229
  INST(Fsave            , X86M_Only          , O_FPU(9B,00DD,6)          , 0                         , 58 , 0  , 27 , 51 ), // #230
  INST(Fscale           , FpuOp              , O_FPU(00,D9FD,_)          , 0                         , 37 , 0  , 26 , 51 ), // #231
  INST(Fsin             , FpuOp              , O_FPU(00,D9FE,_)          , 0                         , 37 , 0  , 26 , 51 ), // #232
  INST(Fsincos          , FpuOp              , O_FPU(00,D9FB,_)          , 0                         , 37 , 0  , 26 , 51 ), // #233
  INST(Fsqrt            , FpuOp              , O_FPU(00,D9FA,_)          , 0                         , 37 , 0  , 26 , 51 ), // #234
  INST(Fst              , FpuFldFst          , O_FPU(00,00D9,2)          , 0                         , 52 , 0  , 67 , 51 ), // #235
  INST(Fstcw            , X86M_Only          , O_FPU(9B,00D9,7)          , 0                         , 59 , 0  , 65 , 51 ), // #236
  INST(Fstenv           , X86M_Only          , O_FPU(9B,00D9,6)          , 0                         , 58 , 0  , 27 , 51 ), // #237
  INST(Fstp             , FpuFldFst          , O_FPU(00,00D9,3)          , O(000000,DB,7,_,_,_,_,_  ), 53 , 12 , 64 , 51 ), // #238
  INST(Fstsw            , FpuStsw            , O_FPU(9B,00DD,7)          , O_FPU(9B,DFE0,_)          , 59 , 13 , 66 , 51 ), // #239
  INST(Fsub             , FpuArith           , O_FPU(00,E0E8,4)          , 0                         , 60 , 0  , 57 , 51 ), // #240
  INST(Fsubp            , FpuRDef            , O_FPU(00,DEE8,_)          , 0                         , 39 , 0  , 58 , 51 ), // #241
  INST(Fsubr            , FpuArith           , O_FPU(00,E8E0,5)          , 0                         , 61 , 0  , 57 , 51 ), // #242
  INST(Fsubrp           , FpuRDef            , O_FPU(00,DEE0,_)          , 0                         , 39 , 0  , 58 , 51 ), // #243
  INST(Ftst             , FpuOp              , O_FPU(00,D9E4,_)          , 0                         , 37 , 0  , 26 , 51 ), // #244
  INST(Fucom            , FpuRDef            , O_FPU(00,DDE0,_)          , 0                         , 50 , 0  , 58 , 51 ), // #245
  INST(Fucomi           , FpuR               , O_FPU(00,DBE8,_)          , 0                         , 44 , 0  , 60 , 56 ), // #246
  INST(Fucomip          , FpuR               , O_FPU(00,DFE8,_)          , 0                         , 46 , 0  , 60 , 56 ), // #247
  INST(Fucomp           , FpuRDef            , O_FPU(00,DDE8,_)          , 0                         , 50 , 0  , 58 , 51 ), // #248
  INST(Fucompp          , FpuOp              , O_FPU(00,DAE9,_)          , 0                         , 43 , 0  , 26 , 51 ), // #249
  INST(Fwait            , X86Op              , O_FPU(00,009B,_)          , 0                         , 51 , 0  , 26 , 51 ), // #250
  INST(Fxam             , FpuOp              , O_FPU(00,D9E5,_)          , 0                         , 37 , 0  , 26 , 51 ), // #251
  INST(Fxch             , FpuR               , O_FPU(00,D9C8,_)          , 0                         , 37 , 0  , 58 , 51 ), // #252
  INST(Fxrstor          , X86M_Only          , O(000F00,AE,1,_,_,_,_,_  ), 0                         , 32 , 0  , 27 , 59 ), // #253
  INST(Fxrstor64        , X86M_Only          , O(000F00,AE,1,_,1,_,_,_  ), 0                         , 31 , 0  , 68 , 59 ), // #254
  INST(Fxsave           , X86M_Only          , O(000F00,AE,0,_,_,_,_,_  ), 0                         , 5  , 0  , 27 , 60 ), // #255
  INST(Fxsave64         , X86M_Only          , O(000F00,AE,0,_,1,_,_,_  ), 0                         , 62 , 0  , 68 , 60 ), // #256
  INST(Fxtract          , FpuOp              , O_FPU(00,D9F4,_)          , 0                         , 37 , 0  , 26 , 51 ), // #257
  INST(Fyl2x            , FpuOp              , O_FPU(00,D9F1,_)          , 0                         , 37 , 0  , 26 , 51 ), // #258
  INST(Fyl2xp1          , FpuOp              , O_FPU(00,D9F9,_)          , 0                         , 37 , 0  , 26 , 51 ), // #259
  INST(Getsec           , X86Op              , O(000F00,37,_,_,_,_,_,_  ), 0                         , 5  , 0  , 69 , 61 ), // #260
  INST(Gf2p8affineinvqb , ExtRmi             , O(660F3A,CF,_,_,_,_,_,_  ), 0                         , 9  , 0  , 9  , 62 ), // #261
  INST(Gf2p8affineqb    , ExtRmi             , O(660F3A,CE,_,_,_,_,_,_  ), 0                         , 9  , 0  , 9  , 62 ), // #262
  INST(Gf2p8mulb        , ExtRm              , O(660F38,CF,_,_,_,_,_,_  ), 0                         , 2  , 0  , 6  , 62 ), // #263
  INST(Haddpd           , ExtRm              , O(660F00,7C,_,_,_,_,_,_  ), 0                         , 4  , 0  , 6  , 7  ), // #264
  INST(Haddps           , ExtRm              , O(F20F00,7C,_,_,_,_,_,_  ), 0                         , 6  , 0  , 6  , 7  ), // #265
  INST(Hlt              , X86Op              , O(000000,F4,_,_,_,_,_,_  ), 0                         , 0  , 0  , 26 , 0  ), // #266
  INST(Hreset           , X86Op_Mod11RM_I8   , O(F30F3A,F0,0,_,_,_,_,_  ), 0                         , 63 , 0  , 70 , 63 ), // #267
  INST(Hsubpd           , ExtRm              , O(660F00,7D,_,_,_,_,_,_  ), 0                         , 4  , 0  , 6  , 7  ), // #268
  INST(Hsubps           , ExtRm              , O(F20F00,7D,_,_,_,_,_,_  ), 0                         , 6  , 0  , 6  , 7  ), // #269
  INST(Idiv             , X86M_GPB_MulDiv    , O(000000,F6,7,_,x,_,_,_  ), 0                         , 29 , 0  , 51 , 1  ), // #270
  INST(Imul             , X86Imul            , O(000000,F6,5,_,x,_,_,_  ), 0                         , 64 , 0  , 71 , 1  ), // #271
  INST(In               , X86In              , O(000000,EC,_,_,_,_,_,_  ), O(000000,E4,_,_,_,_,_,_  ), 0  , 14 , 72 , 0  ), // #272
  INST(Inc              , X86IncDec          , O(000000,FE,0,_,x,_,_,_  ), O(000000,40,_,_,x,_,_,_  ), 0  , 15 , 50 , 46 ), // #273
  INST(Incsspd          , X86M               , O(F30F00,AE,5,_,0,_,_,_  ), 0                         , 65 , 0  , 73 , 64 ), // #274
  INST(Incsspq          , X86M               , O(F30F00,AE,5,_,1,_,_,_  ), 0                         , 66 , 0  , 74 , 64 ), // #275
  INST(Ins              , X86Ins             , O(000000,6C,_,_,_,_,_,_  ), 0                         , 0  , 0  , 75 , 0  ), // #276
  INST(Insertps         , ExtRmi             , O(660F3A,21,_,_,_,_,_,_  ), 0                         , 9  , 0  , 35 , 13 ), // #277
  INST(Insertq          , ExtInsertq         , O(F20F00,79,_,_,_,_,_,_  ), O(F20F00,78,_,_,_,_,_,_  ), 6  , 16 , 76 , 50 ), // #278
  INST(Int              , X86Int             , O(000000,CD,_,_,_,_,_,_  ), 0                         , 0  , 0  , 77 , 0  ), // #279
  INST(Int3             , X86Op              , O(000000,CC,_,_,_,_,_,_  ), 0                         , 0  , 0  , 26 , 0  ), // #280
  INST(Into             , X86Op              , O(000000,CE,_,_,_,_,_,_  ), 0                         , 0  , 0  , 78 , 65 ), // #281
  INST(Invd             , X86Op              , O(000F00,08,_,_,_,_,_,_  ), 0                         , 5  , 0  , 26 , 44 ), // #282
  INST(Invept           , X86Rm_NoSize       , O(660F38,80,_,_,_,_,_,_  ), 0                         , 2  , 0  , 79 , 66 ), // #283
  INST(Invlpg           , X86M_Only          , O(000F00,01,7,_,_,_,_,_  ), 0                         , 24 , 0  , 27 , 44 ), // #284
  INST(Invlpga          , X86Op_xAddr        , O(000F01,DF,_,_,_,_,_,_  ), 0                         , 23 , 0  , 80 , 22 ), // #285
  INST(Invlpgb          , X86Op              , O(000F01,FE,_,_,_,_,_,_  ), 0                         , 23 , 0  , 81 , 67 ), // #286
  INST(Invpcid          , X86Rm_NoSize       , O(660F38,82,_,_,_,_,_,_  ), 0                         , 2  , 0  , 79 , 44 ), // #287
  INST(Invvpid          , X86Rm_NoSize       , O(660F38,81,_,_,_,_,_,_  ), 0                         , 2  , 0  , 79 , 66 ), // #288
  INST(Iret             , X86Op              , O(660000,CF,_,_,_,_,_,_  ), 0                         , 21 , 0  , 82 , 1  ), // #289
  INST(Iretd            , X86Op              , O(000000,CF,_,_,_,_,_,_  ), 0                         , 0  , 0  , 82 , 1  ), // #290
  INST(Iretq            , X86Op              , O(000000,CF,_,_,1,_,_,_  ), 0                         , 22 , 0  , 83 , 1  ), // #291
  INST(Jb               , X86Jcc             , O(000F00,82,_,_,_,_,_,_  ), O(000000,72,_,_,_,_,_,_  ), 5  , 17 , 84 , 68 ), // #292
  INST(Jbe              , X86Jcc             , O(000F00,86,_,_,_,_,_,_  ), O(000000,76,_,_,_,_,_,_  ), 5  , 18 , 84 , 69 ), // #293
  INST(Jecxz            , X86JecxzLoop       , 0                         , O(000000,E3,_,_,_,_,_,_  ), 0  , 19 , 85 , 0  ), // #294
  INST(Jl               , X86Jcc             , O(000F00,8C,_,_,_,_,_,_  ), O(000000,7C,_,_,_,_,_,_  ), 5  , 20 , 84 , 70 ), // #295
  INST(Jle              , X86Jcc             , O(000F00,8E,_,_,_,_,_,_  ), O(000000,7E,_,_,_,_,_,_  ), 5  , 21 , 84 , 71 ), // #296
  INST(Jmp              , X86Jmp             , O(000000,FF,4,_,_,_,_,_  ), O(000000,EB,_,_,_,_,_,_  ), 10 , 22 , 86 , 0  ), // #297
  INST(Jnb              , X86Jcc             , O(000F00,83,_,_,_,_,_,_  ), O(000000,73,_,_,_,_,_,_  ), 5  , 23 , 84 , 68 ), // #298
  INST(Jnbe             , X86Jcc             , O(000F00,87,_,_,_,_,_,_  ), O(000000,77,_,_,_,_,_,_  ), 5  , 24 , 84 , 69 ), // #299
  INST(Jnl              , X86Jcc             , O(000F00,8D,_,_,_,_,_,_  ), O(000000,7D,_,_,_,_,_,_  ), 5  , 25 , 84 , 70 ), // #300
  INST(Jnle             , X86Jcc             , O(000F00,8F,_,_,_,_,_,_  ), O(000000,7F,_,_,_,_,_,_  ), 5  , 26 , 84 , 71 ), // #301
  INST(Jno              , X86Jcc             , O(000F00,81,_,_,_,_,_,_  ), O(000000,71,_,_,_,_,_,_  ), 5  , 27 , 84 , 65 ), // #302
  INST(Jnp              , X86Jcc             , O(000F00,8B,_,_,_,_,_,_  ), O(000000,7B,_,_,_,_,_,_  ), 5  , 28 , 84 , 72 ), // #303
  INST(Jns              , X86Jcc             , O(000F00,89,_,_,_,_,_,_  ), O(000000,79,_,_,_,_,_,_  ), 5  , 29 , 84 , 73 ), // #304
  INST(Jnz              , X86Jcc             , O(000F00,85,_,_,_,_,_,_  ), O(000000,75,_,_,_,_,_,_  ), 5  , 30 , 84 , 74 ), // #305
  INST(Jo               , X86Jcc             , O(000F00,80,_,_,_,_,_,_  ), O(000000,70,_,_,_,_,_,_  ), 5  , 31 , 84 , 65 ), // #306
  INST(Jp               , X86Jcc             , O(000F00,8A,_,_,_,_,_,_  ), O(000000,7A,_,_,_,_,_,_  ), 5  , 32 , 84 , 72 ), // #307
  INST(Js               , X86Jcc             , O(000F00,88,_,_,_,_,_,_  ), O(000000,78,_,_,_,_,_,_  ), 5  , 33 , 84 , 73 ), // #308
  INST(Jz               , X86Jcc             , O(000F00,84,_,_,_,_,_,_  ), O(000000,74,_,_,_,_,_,_  ), 5  , 34 , 84 , 74 ), // #309
  INST(Kaddb            , VexRvm             , V(660F00,4A,_,1,0,_,_,_  ), 0                         , 67 , 0  , 87 , 75 ), // #310
  INST(Kaddd            , VexRvm             , V(660F00,4A,_,1,1,_,_,_  ), 0                         , 68 , 0  , 87 , 76 ), // #311
  INST(Kaddq            , VexRvm             , V(000F00,4A,_,1,1,_,_,_  ), 0                         , 69 , 0  , 87 , 76 ), // #312
  INST(Kaddw            , VexRvm             , V(000F00,4A,_,1,0,_,_,_  ), 0                         , 70 , 0  , 87 , 75 ), // #313
  INST(Kandb            , VexRvm             , V(660F00,41,_,1,0,_,_,_  ), 0                         , 67 , 0  , 87 , 75 ), // #314
  INST(Kandd            , VexRvm             , V(660F00,41,_,1,1,_,_,_  ), 0                         , 68 , 0  , 87 , 76 ), // #315
  INST(Kandnb           , VexRvm             , V(660F00,42,_,1,0,_,_,_  ), 0                         , 67 , 0  , 87 , 75 ), // #316
  INST(Kandnd           , VexRvm             , V(660F00,42,_,1,1,_,_,_  ), 0                         , 68 , 0  , 87 , 76 ), // #317
  INST(Kandnq           , VexRvm             , V(000F00,42,_,1,1,_,_,_  ), 0                         , 69 , 0  , 87 , 76 ), // #318
  INST(Kandnw           , VexRvm             , V(000F00,42,_,1,0,_,_,_  ), 0                         , 70 , 0  , 87 , 77 ), // #319
  INST(Kandq            , VexRvm             , V(000F00,41,_,1,1,_,_,_  ), 0                         , 69 , 0  , 87 , 76 ), // #320
  INST(Kandw            , VexRvm             , V(000F00,41,_,1,0,_,_,_  ), 0                         , 70 , 0  , 87 , 77 ), // #321
  INST(Kmovb            , VexKmov            , V(660F00,90,_,0,0,_,_,_  ), V(660F00,92,_,0,0,_,_,_  ), 71 , 35 , 88 , 78 ), // #322
  INST(Kmovd            , VexKmov            , V(660F00,90,_,0,1,_,_,_  ), V(F20F00,92,_,0,0,_,_,_  ), 72 , 36 , 89 , 79 ), // #323
  INST(Kmovq            , VexKmov            , V(000F00,90,_,0,1,_,_,_  ), V(F20F00,92,_,0,1,_,_,_  ), 73 , 37 , 90 , 79 ), // #324
  INST(Kmovw            , VexKmov            , V(000F00,90,_,0,0,_,_,_  ), V(000F00,92,_,0,0,_,_,_  ), 74 , 38 , 91 , 80 ), // #325
  INST(Knotb            , VexRm              , V(660F00,44,_,0,0,_,_,_  ), 0                         , 71 , 0  , 92 , 75 ), // #326
  INST(Knotd            , VexRm              , V(660F00,44,_,0,1,_,_,_  ), 0                         , 72 , 0  , 92 , 76 ), // #327
  INST(Knotq            , VexRm              , V(000F00,44,_,0,1,_,_,_  ), 0                         , 73 , 0  , 92 , 76 ), // #328
  INST(Knotw            , VexRm              , V(000F00,44,_,0,0,_,_,_  ), 0                         , 74 , 0  , 92 , 77 ), // #329
  INST(Korb             , VexRvm             , V(660F00,45,_,1,0,_,_,_  ), 0                         , 67 , 0  , 87 , 75 ), // #330
  INST(Kord             , VexRvm             , V(660F00,45,_,1,1,_,_,_  ), 0                         , 68 , 0  , 87 , 76 ), // #331
  INST(Korq             , VexRvm             , V(000F00,45,_,1,1,_,_,_  ), 0                         , 69 , 0  , 87 , 76 ), // #332
  INST(Kortestb         , VexRm              , V(660F00,98,_,0,0,_,_,_  ), 0                         , 71 , 0  , 92 , 81 ), // #333
  INST(Kortestd         , VexRm              , V(660F00,98,_,0,1,_,_,_  ), 0                         , 72 , 0  , 92 , 82 ), // #334
  INST(Kortestq         , VexRm              , V(000F00,98,_,0,1,_,_,_  ), 0                         , 73 , 0  , 92 , 82 ), // #335
  INST(Kortestw         , VexRm              , V(000F00,98,_,0,0,_,_,_  ), 0                         , 74 , 0  , 92 , 83 ), // #336
  INST(Korw             , VexRvm             , V(000F00,45,_,1,0,_,_,_  ), 0                         , 70 , 0  , 87 , 77 ), // #337
  INST(Kshiftlb         , VexRmi             , V(660F3A,32,_,0,0,_,_,_  ), 0                         , 75 , 0  , 93 , 75 ), // #338
  INST(Kshiftld         , VexRmi             , V(660F3A,33,_,0,0,_,_,_  ), 0                         , 75 , 0  , 93 , 76 ), // #339
  INST(Kshiftlq         , VexRmi             , V(660F3A,33,_,0,1,_,_,_  ), 0                         , 76 , 0  , 93 , 76 ), // #340
  INST(Kshiftlw         , VexRmi             , V(660F3A,32,_,0,1,_,_,_  ), 0                         , 76 , 0  , 93 , 77 ), // #341
  INST(Kshiftrb         , VexRmi             , V(660F3A,30,_,0,0,_,_,_  ), 0                         , 75 , 0  , 93 , 75 ), // #342
  INST(Kshiftrd         , VexRmi             , V(660F3A,31,_,0,0,_,_,_  ), 0                         , 75 , 0  , 93 , 76 ), // #343
  INST(Kshiftrq         , VexRmi             , V(660F3A,31,_,0,1,_,_,_  ), 0                         , 76 , 0  , 93 , 76 ), // #344
  INST(Kshiftrw         , VexRmi             , V(660F3A,30,_,0,1,_,_,_  ), 0                         , 76 , 0  , 93 , 77 ), // #345
  INST(Ktestb           , VexRm              , V(660F00,99,_,0,0,_,_,_  ), 0                         , 71 , 0  , 92 , 81 ), // #346
  INST(Ktestd           , VexRm              , V(660F00,99,_,0,1,_,_,_  ), 0                         , 72 , 0  , 92 , 82 ), // #347
  INST(Ktestq           , VexRm              , V(000F00,99,_,0,1,_,_,_  ), 0                         , 73 , 0  , 92 , 82 ), // #348
  INST(Ktestw           , VexRm              , V(000F00,99,_,0,0,_,_,_  ), 0                         , 74 , 0  , 92 , 81 ), // #349
  INST(Kunpckbw         , VexRvm             , V(660F00,4B,_,1,0,_,_,_  ), 0                         , 67 , 0  , 87 , 77 ), // #350
  INST(Kunpckdq         , VexRvm             , V(000F00,4B,_,1,1,_,_,_  ), 0                         , 69 , 0  , 87 , 76 ), // #351
  INST(Kunpckwd         , VexRvm             , V(000F00,4B,_,1,0,_,_,_  ), 0                         , 70 , 0  , 87 , 76 ), // #352
  INST(Kxnorb           , VexRvm             , V(660F00,46,_,1,0,_,_,_  ), 0                         , 67 , 0  , 94 , 75 ), // #353
  INST(Kxnord           , VexRvm             , V(660F00,46,_,1,1,_,_,_  ), 0                         , 68 , 0  , 94 , 76 ), // #354
  INST(Kxnorq           , VexRvm             , V(000F00,46,_,1,1,_,_,_  ), 0                         , 69 , 0  , 94 , 76 ), // #355
  INST(Kxnorw           , VexRvm             , V(000F00,46,_,1,0,_,_,_  ), 0                         , 70 , 0  , 94 , 77 ), // #356
  INST(Kxorb            , VexRvm             , V(660F00,47,_,1,0,_,_,_  ), 0                         , 67 , 0  , 94 , 75 ), // #357
  INST(Kxord            , VexRvm             , V(660F00,47,_,1,1,_,_,_  ), 0                         , 68 , 0  , 94 , 76 ), // #358
  INST(Kxorq            , VexRvm             , V(000F00,47,_,1,1,_,_,_  ), 0                         , 69 , 0  , 94 , 76 ), // #359
  INST(Kxorw            , VexRvm             , V(000F00,47,_,1,0,_,_,_  ), 0                         , 70 , 0  , 94 , 77 ), // #360
  INST(Lahf             , X86Op              , O(000000,9F,_,_,_,_,_,_  ), 0                         , 0  , 0  , 95 , 84 ), // #361
  INST(Lar              , X86Rm              , O(000F00,02,_,_,_,_,_,_  ), 0                         , 5  , 0  , 96 , 11 ), // #362
  INST(Lcall            , X86LcallLjmp       , O(000000,FF,3,_,_,_,_,_  ), O(000000,9A,_,_,_,_,_,_  ), 77 , 39 , 97 , 1  ), // #363
  INST(Lddqu            , ExtRm              , O(F20F00,F0,_,_,_,_,_,_  ), 0                         , 6  , 0  , 98 , 7  ), // #364
  INST(Ldmxcsr          , X86M_Only          , O(000F00,AE,2,_,_,_,_,_  ), 0                         , 78 , 0  , 99 , 6  ), // #365
  INST(Lds              , X86Rm              , O(000000,C5,_,_,_,_,_,_  ), 0                         , 0  , 0  , 100, 0  ), // #366
  INST(Ldtilecfg        , AmxCfg             , V(000F38,49,_,0,0,_,_,_  ), 0                         , 11 , 0  , 101, 85 ), // #367
  INST(Lea              , X86Lea             , O(000000,8D,_,_,x,_,_,_  ), 0                         , 0  , 0  , 102, 0  ), // #368
  INST(Leave            , X86Op              , O(000000,C9,_,_,_,_,_,_  ), 0                         , 0  , 0  , 26 , 0  ), // #369
  INST(Les              , X86Rm              , O(000000,C4,_,_,_,_,_,_  ), 0                         , 0  , 0  , 100, 0  ), // #370
  INST(Lfence           , X86Fence           , O(000F00,AE,5,_,_,_,_,_  ), 0                         , 79 , 0  , 26 , 5  ), // #371
  INST(Lfs              , X86Rm              , O(000F00,B4,_,_,_,_,_,_  ), 0                         , 5  , 0  , 103, 0  ), // #372
  INST(Lgdt             , X86M_Only          , O(000F00,01,2,_,_,_,_,_  ), 0                         , 78 , 0  , 27 , 0  ), // #373
  INST(Lgs              , X86Rm              , O(000F00,B5,_,_,_,_,_,_  ), 0                         , 5  , 0  , 103, 0  ), // #374
  INST(Lidt             , X86M_Only          , O(000F00,01,3,_,_,_,_,_  ), 0                         , 80 , 0  , 27 , 0  ), // #375
  INST(Ljmp             , X86LcallLjmp       , O(000000,FF,5,_,_,_,_,_  ), O(000000,EA,_,_,_,_,_,_  ), 64 , 40 , 104, 0  ), // #376
  INST(Lldt             , X86M_NoSize        , O(000F00,00,2,_,_,_,_,_  ), 0                         , 78 , 0  , 105, 0  ), // #377
  INST(Llwpcb           , VexR_Wx            , V(XOP_M9,12,0,0,x,_,_,_  ), 0                         , 81 , 0  , 106, 86 ), // #378
  INST(Lmsw             , X86M_NoSize        , O(000F00,01,6,_,_,_,_,_  ), 0                         , 82 , 0  , 105, 0  ), // #379
  INST(Lods             , X86StrRm           , O(000000,AC,_,_,_,_,_,_  ), 0                         , 0  , 0  , 107, 87 ), // #380
  INST(Loop             , X86JecxzLoop       , 0                         , O(000000,E2,_,_,_,_,_,_  ), 0  , 41 , 108, 0  ), // #381
  INST(Loope            , X86JecxzLoop       , 0                         , O(000000,E1,_,_,_,_,_,_  ), 0  , 42 , 108, 74 ), // #382
  INST(Loopne           , X86JecxzLoop       , 0                         , O(000000,E0,_,_,_,_,_,_  ), 0  , 43 , 108, 74 ), // #383
  INST(Lsl              , X86Rm              , O(000F00,03,_,_,_,_,_,_  ), 0                         , 5  , 0  , 109, 11 ), // #384
  INST(Lss              , X86Rm              , O(000F00,B2,_,_,_,_,_,_  ), 0                         , 5  , 0  , 103, 0  ), // #385
  INST(Ltr              , X86M_NoSize        , O(000F00,00,3,_,_,_,_,_  ), 0                         , 80 , 0  , 105, 0  ), // #386
  INST(Lwpins           , VexVmi4_Wx         , V(XOP_MA,12,0,0,x,_,_,_  ), 0                         , 83 , 0  , 110, 86 ), // #387
  INST(Lwpval           , VexVmi4_Wx         , V(XOP_MA,12,1,0,x,_,_,_  ), 0                         , 84 , 0  , 110, 86 ), // #388
  INST(Lzcnt            , X86Rm_Raw66H       , O(F30F00,BD,_,_,x,_,_,_  ), 0                         , 7  , 0  , 18 , 88 ), // #389
  INST(Maskmovdqu       , ExtRm_ZDI          , O(660F00,F7,_,_,_,_,_,_  ), 0                         , 4  , 0  , 111, 5  ), // #390
  INST(Maskmovq         , ExtRm_ZDI          , O(000F00,F7,_,_,_,_,_,_  ), 0                         , 5  , 0  , 112, 89 ), // #391
  INST(Maxpd            , ExtRm              , O(660F00,5F,_,_,_,_,_,_  ), 0                         , 4  , 0  , 6  , 5  ), // #392
  INST(Maxps            , ExtRm              , O(000F00,5F,_,_,_,_,_,_  ), 0                         , 5  , 0  , 6  , 6  ), // #393
  INST(Maxsd            , ExtRm              , O(F20F00,5F,_,_,_,_,_,_  ), 0                         , 6  , 0  , 7  , 5  ), // #394
  INST(Maxss            , ExtRm              , O(F30F00,5F,_,_,_,_,_,_  ), 0                         , 7  , 0  , 8  , 6  ), // #395
  INST(Mcommit          , X86Op              , O(F30F01,FA,_,_,_,_,_,_  ), 0                         , 27 , 0  , 26 , 90 ), // #396
  INST(Mfence           , X86Fence           , O(000F00,AE,6,_,_,_,_,_  ), 0                         , 82 , 0  , 26 , 5  ), // #397
  INST(Minpd            , ExtRm              , O(660F00,5D,_,_,_,_,_,_  ), 0                         , 4  , 0  , 6  , 5  ), // #398
  INST(Minps            , ExtRm              , O(000F00,5D,_,_,_,_,_,_  ), 0                         , 5  , 0  , 6  , 6  ), // #399
  INST(Minsd            , ExtRm              , O(F20F00,5D,_,_,_,_,_,_  ), 0                         , 6  , 0  , 7  , 5  ), // #400
  INST(Minss            , ExtRm              , O(F30F00,5D,_,_,_,_,_,_  ), 0                         , 7  , 0  , 8  , 6  ), // #401
  INST(Monitor          , X86Op              , O(000F01,C8,_,_,_,_,_,_  ), 0                         , 23 , 0  , 113, 91 ), // #402
  INST(Monitorx         , X86Op              , O(000F01,FA,_,_,_,_,_,_  ), 0                         , 23 , 0  , 113, 92 ), // #403
  INST(Mov              , X86Mov             , 0                         , 0                         , 0  , 0  , 114, 93 ), // #404
  INST(Movabs           , X86Movabs          , 0                         , 0                         , 0  , 0  , 115, 0  ), // #405
  INST(Movapd           , ExtMov             , O(660F00,28,_,_,_,_,_,_  ), O(660F00,29,_,_,_,_,_,_  ), 4  , 44 , 116, 94 ), // #406
  INST(Movaps           , ExtMov             , O(000F00,28,_,_,_,_,_,_  ), O(000F00,29,_,_,_,_,_,_  ), 5  , 45 , 116, 95 ), // #407
  INST(Movbe            , ExtMovbe           , O(000F38,F0,_,_,x,_,_,_  ), O(000F38,F1,_,_,x,_,_,_  ), 1  , 46 , 117, 96 ), // #408
  INST(Movd             , ExtMovd            , O(000F00,6E,_,_,_,_,_,_  ), O(000F00,7E,_,_,_,_,_,_  ), 5  , 47 , 118, 97 ), // #409
  INST(Movddup          , ExtMov             , O(F20F00,12,_,_,_,_,_,_  ), 0                         , 6  , 0  , 7  , 7  ), // #410
  INST(Movdir64b        , X86EnqcmdMovdir64b , O(660F38,F8,_,_,_,_,_,_  ), 0                         , 2  , 0  , 119, 98 ), // #411
  INST(Movdiri          , X86MovntiMovdiri   , O(000F38,F9,_,_,_,_,_,_  ), 0                         , 1  , 0  , 3  , 99 ), // #412
  INST(Movdq2q          , ExtMov             , O(F20F00,D6,_,_,_,_,_,_  ), 0                         , 6  , 0  , 120, 5  ), // #413
  INST(Movdqa           , ExtMov             , O(660F00,6F,_,_,_,_,_,_  ), O(660F00,7F,_,_,_,_,_,_  ), 4  , 48 , 116, 94 ), // #414
  INST(Movdqu           , ExtMov             , O(F30F00,6F,_,_,_,_,_,_  ), O(F30F00,7F,_,_,_,_,_,_  ), 7  , 49 , 116, 94 ), // #415
  INST(Movhlps          , ExtMov             , O(000F00,12,_,_,_,_,_,_  ), 0                         , 5  , 0  , 121, 6  ), // #416
  INST(Movhpd           , ExtMov             , O(660F00,16,_,_,_,_,_,_  ), O(660F00,17,_,_,_,_,_,_  ), 4  , 50 , 122, 5  ), // #417
  INST(Movhps           , ExtMov             , O(000F00,16,_,_,_,_,_,_  ), O(000F00,17,_,_,_,_,_,_  ), 5  , 51 , 122, 6  ), // #418
  INST(Movlhps          , ExtMov             , O(000F00,16,_,_,_,_,_,_  ), 0                         , 5  , 0  , 121, 6  ), // #419
  INST(Movlpd           , ExtMov             , O(660F00,12,_,_,_,_,_,_  ), O(660F00,13,_,_,_,_,_,_  ), 4  , 52 , 122, 5  ), // #420
  INST(Movlps           , ExtMov             , O(000F00,12,_,_,_,_,_,_  ), O(000F00,13,_,_,_,_,_,_  ), 5  , 53 , 122, 6  ), // #421
  INST(Movmskpd         , ExtMov             , O(660F00,50,_,_,_,_,_,_  ), 0                         , 4  , 0  , 123, 5  ), // #422
  INST(Movmskps         , ExtMov             , O(000F00,50,_,_,_,_,_,_  ), 0                         , 5  , 0  , 123, 6  ), // #423
  INST(Movntdq          , ExtMov             , 0                         , O(660F00,E7,_,_,_,_,_,_  ), 0  , 54 , 124, 5  ), // #424
  INST(Movntdqa         , ExtMov             , O(660F38,2A,_,_,_,_,_,_  ), 0                         , 2  , 0  , 98 , 13 ), // #425
  INST(Movnti           , X86MovntiMovdiri   , O(000F00,C3,_,_,x,_,_,_  ), 0                         , 5  , 0  , 3  , 5  ), // #426
  INST(Movntpd          , ExtMov             , 0                         , O(660F00,2B,_,_,_,_,_,_  ), 0  , 55 , 124, 5  ), // #427
  INST(Movntps          , ExtMov             , 0                         , O(000F00,2B,_,_,_,_,_,_  ), 0  , 56 , 124, 6  ), // #428
  INST(Movntq           , ExtMov             , 0                         , O(000F00,E7,_,_,_,_,_,_  ), 0  , 57 , 125, 89 ), // #429
  INST(Movntsd          , ExtMov             , 0                         , O(F20F00,2B,_,_,_,_,_,_  ), 0  , 58 , 126, 50 ), // #430
  INST(Movntss          , ExtMov             , 0                         , O(F30F00,2B,_,_,_,_,_,_  ), 0  , 59 , 127, 50 ), // #431
  INST(Movq             , ExtMovq            , O(000F00,6E,_,_,x,_,_,_  ), O(000F00,7E,_,_,x,_,_,_  ), 5  , 47 , 128, 100), // #432
  INST(Movq2dq          , ExtRm              , O(F30F00,D6,_,_,_,_,_,_  ), 0                         , 7  , 0  , 129, 5  ), // #433
  INST(Movs             , X86StrMm           , O(000000,A4,_,_,_,_,_,_  ), 0                         , 0  , 0  , 130, 87 ), // #434
  INST(Movsd            , ExtMov             , O(F20F00,10,_,_,_,_,_,_  ), O(F20F00,11,_,_,_,_,_,_  ), 6  , 60 , 131, 94 ), // #435
  INST(Movshdup         , ExtRm              , O(F30F00,16,_,_,_,_,_,_  ), 0                         , 7  , 0  , 6  , 7  ), // #436
  INST(Movsldup         , ExtRm              , O(F30F00,12,_,_,_,_,_,_  ), 0                         , 7  , 0  , 6  , 7  ), // #437
  INST(Movss            , ExtMov             , O(F30F00,10,_,_,_,_,_,_  ), O(F30F00,11,_,_,_,_,_,_  ), 7  , 61 , 132, 95 ), // #438
  INST(Movsx            , X86MovsxMovzx      , O(000F00,BE,_,_,x,_,_,_  ), 0                         , 5  , 0  , 133, 0  ), // #439
  INST(Movsxd           , X86Rm              , O(000000,63,_,_,x,_,_,_  ), 0                         , 0  , 0  , 134, 0  ), // #440
  INST(Movupd           , ExtMov             , O(660F00,10,_,_,_,_,_,_  ), O(660F00,11,_,_,_,_,_,_  ), 4  , 62 , 116, 94 ), // #441
  INST(Movups           , ExtMov             , O(000F00,10,_,_,_,_,_,_  ), O(000F00,11,_,_,_,_,_,_  ), 5  , 63 , 116, 95 ), // #442
  INST(Movzx            , X86MovsxMovzx      , O(000F00,B6,_,_,x,_,_,_  ), 0                         , 5  , 0  , 133, 0  ), // #443
  INST(Mpsadbw          , ExtRmi             , O(660F3A,42,_,_,_,_,_,_  ), 0                         , 9  , 0  , 9  , 13 ), // #444
  INST(Mul              , X86M_GPB_MulDiv    , O(000000,F6,4,_,x,_,_,_  ), 0                         , 10 , 0  , 51 , 1  ), // #445
  INST(Mulpd            , ExtRm              , O(660F00,59,_,_,_,_,_,_  ), 0                         , 4  , 0  , 6  , 5  ), // #446
  INST(Mulps            , ExtRm              , O(000F00,59,_,_,_,_,_,_  ), 0                         , 5  , 0  , 6  , 6  ), // #447
  INST(Mulsd            , ExtRm              , O(F20F00,59,_,_,_,_,_,_  ), 0                         , 6  , 0  , 7  , 5  ), // #448
  INST(Mulss            , ExtRm              , O(F30F00,59,_,_,_,_,_,_  ), 0                         , 7  , 0  , 8  , 6  ), // #449
  INST(Mulx             , VexRvm_ZDX_Wx      , V(F20F38,F6,_,0,x,_,_,_  ), 0                         , 85 , 0  , 135, 101), // #450
  INST(Mwait            , X86Op              , O(000F01,C9,_,_,_,_,_,_  ), 0                         , 23 , 0  , 136, 91 ), // #451
  INST(Mwaitx           , X86Op              , O(000F01,FB,_,_,_,_,_,_  ), 0                         , 23 , 0  , 137, 92 ), // #452
  INST(Neg              , X86M_GPB           , O(000000,F6,3,_,x,_,_,_  ), 0                         , 77 , 0  , 138, 1  ), // #453
  INST(Nop              , X86M_Nop           , O(000000,90,_,_,_,_,_,_  ), 0                         , 0  , 0  , 139, 0  ), // #454
  INST(Not              , X86M_GPB           , O(000000,F6,2,_,x,_,_,_  ), 0                         , 3  , 0  , 138, 0  ), // #455
  INST(Or               , X86Arith           , O(000000,08,1,_,x,_,_,_  ), 0                         , 33 , 0  , 140, 1  ), // #456
  INST(Orpd             , ExtRm              , O(660F00,56,_,_,_,_,_,_  ), 0                         , 4  , 0  , 12 , 5  ), // #457
  INST(Orps             , ExtRm              , O(000F00,56,_,_,_,_,_,_  ), 0                         , 5  , 0  , 12 , 6  ), // #458
  INST(Out              , X86Out             , O(000000,EE,_,_,_,_,_,_  ), O(000000,E6,_,_,_,_,_,_  ), 0  , 64 , 141, 0  ), // #459
  INST(Outs             , X86Outs            , O(000000,6E,_,_,_,_,_,_  ), 0                         , 0  , 0  , 142, 0  ), // #460
  INST(Pabsb            , ExtRm_P            , O(000F38,1C,_,_,_,_,_,_  ), 0                         , 1  , 0  , 143, 102), // #461
  INST(Pabsd            , ExtRm_P            , O(000F38,1E,_,_,_,_,_,_  ), 0                         , 1  , 0  , 143, 102), // #462
  INST(Pabsw            , ExtRm_P            , O(000F38,1D,_,_,_,_,_,_  ), 0                         , 1  , 0  , 143, 102), // #463
  INST(Packssdw         , ExtRm_P            , O(000F00,6B,_,_,_,_,_,_  ), 0                         , 5  , 0  , 143, 97 ), // #464
  INST(Packsswb         , ExtRm_P            , O(000F00,63,_,_,_,_,_,_  ), 0                         , 5  , 0  , 143, 97 ), // #465
  INST(Packusdw         , ExtRm              , O(660F38,2B,_,_,_,_,_,_  ), 0                         , 2  , 0  , 6  , 13 ), // #466
  INST(Packuswb         , ExtRm_P            , O(000F00,67,_,_,_,_,_,_  ), 0                         , 5  , 0  , 143, 97 ), // #467
  INST(Paddb            , ExtRm_P            , O(000F00,FC,_,_,_,_,_,_  ), 0                         , 5  , 0  , 143, 97 ), // #468
  INST(Paddd            , ExtRm_P            , O(000F00,FE,_,_,_,_,_,_  ), 0                         , 5  , 0  , 143, 97 ), // #469
  INST(Paddq            , ExtRm_P            , O(000F00,D4,_,_,_,_,_,_  ), 0                         , 5  , 0  , 143, 5  ), // #470
  INST(Paddsb           , ExtRm_P            , O(000F00,EC,_,_,_,_,_,_  ), 0                         , 5  , 0  , 143, 97 ), // #471
  INST(Paddsw           , ExtRm_P            , O(000F00,ED,_,_,_,_,_,_  ), 0                         , 5  , 0  , 143, 97 ), // #472
  INST(Paddusb          , ExtRm_P            , O(000F00,DC,_,_,_,_,_,_  ), 0                         , 5  , 0  , 143, 97 ), // #473
  INST(Paddusw          , ExtRm_P            , O(000F00,DD,_,_,_,_,_,_  ), 0                         , 5  , 0  , 143, 97 ), // #474
  INST(Paddw            , ExtRm_P            , O(000F00,FD,_,_,_,_,_,_  ), 0                         , 5  , 0  , 143, 97 ), // #475
  INST(Palignr          , ExtRmi_P           , O(000F3A,0F,_,_,_,_,_,_  ), 0                         , 86 , 0  , 144, 102), // #476
  INST(Pand             , ExtRm_P            , O(000F00,DB,_,_,_,_,_,_  ), 0                         , 5  , 0  , 145, 97 ), // #477
  INST(Pandn            , ExtRm_P            , O(000F00,DF,_,_,_,_,_,_  ), 0                         , 5  , 0  , 146, 97 ), // #478
  INST(Pause            , X86Op              , O(F30000,90,_,_,_,_,_,_  ), 0                         , 87 , 0  , 26 , 0  ), // #479
  INST(Pavgb            , ExtRm_P            , O(000F00,E0,_,_,_,_,_,_  ), 0                         , 5  , 0  , 143, 103), // #480
  INST(Pavgusb          , Ext3dNow           , O(000F0F,BF,_,_,_,_,_,_  ), 0                         , 88 , 0  , 147, 57 ), // #481
  INST(Pavgw            , ExtRm_P            , O(000F00,E3,_,_,_,_,_,_  ), 0                         , 5  , 0  , 143, 103), // #482
  INST(Pblendvb         , ExtRm_XMM0         , O(660F38,10,_,_,_,_,_,_  ), 0                         , 2  , 0  , 16 , 13 ), // #483
  INST(Pblendw          , ExtRmi             , O(660F3A,0E,_,_,_,_,_,_  ), 0                         , 9  , 0  , 9  , 13 ), // #484
  INST(Pclmulqdq        , ExtRmi             , O(660F3A,44,_,_,_,_,_,_  ), 0                         , 9  , 0  , 9  , 104), // #485
  INST(Pcmpeqb          , ExtRm_P            , O(000F00,74,_,_,_,_,_,_  ), 0                         , 5  , 0  , 146, 97 ), // #486
  INST(Pcmpeqd          , ExtRm_P            , O(000F00,76,_,_,_,_,_,_  ), 0                         , 5  , 0  , 146, 97 ), // #487
  INST(Pcmpeqq          , ExtRm              , O(660F38,29,_,_,_,_,_,_  ), 0                         , 2  , 0  , 148, 13 ), // #488
  INST(Pcmpeqw          , ExtRm_P            , O(000F00,75,_,_,_,_,_,_  ), 0                         , 5  , 0  , 146, 97 ), // #489
  INST(Pcmpestri        , ExtRmi             , O(660F3A,61,_,_,_,_,_,_  ), 0                         , 9  , 0  , 149, 105), // #490
  INST(Pcmpestrm        , ExtRmi             , O(660F3A,60,_,_,_,_,_,_  ), 0                         , 9  , 0  , 150, 105), // #491
  INST(Pcmpgtb          , ExtRm_P            , O(000F00,64,_,_,_,_,_,_  ), 0                         , 5  , 0  , 146, 97 ), // #492
  INST(Pcmpgtd          , ExtRm_P            , O(000F00,66,_,_,_,_,_,_  ), 0                         , 5  , 0  , 146, 97 ), // #493
  INST(Pcmpgtq          , ExtRm              , O(660F38,37,_,_,_,_,_,_  ), 0                         , 2  , 0  , 148, 45 ), // #494
  INST(Pcmpgtw          , ExtRm_P            , O(000F00,65,_,_,_,_,_,_  ), 0                         , 5  , 0  , 146, 97 ), // #495
  INST(Pcmpistri        , ExtRmi             , O(660F3A,63,_,_,_,_,_,_  ), 0                         , 9  , 0  , 151, 105), // #496
  INST(Pcmpistrm        , ExtRmi             , O(660F3A,62,_,_,_,_,_,_  ), 0                         , 9  , 0  , 152, 105), // #497
  INST(Pconfig          , X86Op              , O(000F01,C5,_,_,_,_,_,_  ), 0                         , 23 , 0  , 26 , 106), // #498
  INST(Pdep             , VexRvm_Wx          , V(F20F38,F5,_,0,x,_,_,_  ), 0                         , 85 , 0  , 11 , 101), // #499
  INST(Pext             , VexRvm_Wx          , V(F30F38,F5,_,0,x,_,_,_  ), 0                         , 89 , 0  , 11 , 101), // #500
  INST(Pextrb           , ExtExtract         , O(000F3A,14,_,_,_,_,_,_  ), 0                         , 86 , 0  , 153, 13 ), // #501
  INST(Pextrd           , ExtExtract         , O(000F3A,16,_,_,_,_,_,_  ), 0                         , 86 , 0  , 55 , 13 ), // #502
  INST(Pextrq           , ExtExtract         , O(000F3A,16,_,_,1,_,_,_  ), 0                         , 90 , 0  , 154, 13 ), // #503
  INST(Pextrw           , ExtPextrw          , O(000F00,C5,_,_,_,_,_,_  ), O(000F3A,15,_,_,_,_,_,_  ), 5  , 65 , 155, 107), // #504
  INST(Pf2id            , Ext3dNow           , O(000F0F,1D,_,_,_,_,_,_  ), 0                         , 88 , 0  , 147, 57 ), // #505
  INST(Pf2iw            , Ext3dNow           , O(000F0F,1C,_,_,_,_,_,_  ), 0                         , 88 , 0  , 147, 108), // #506
  INST(Pfacc            , Ext3dNow           , O(000F0F,AE,_,_,_,_,_,_  ), 0                         , 88 , 0  , 147, 57 ), // #507
  INST(Pfadd            , Ext3dNow           , O(000F0F,9E,_,_,_,_,_,_  ), 0                         , 88 , 0  , 147, 57 ), // #508
  INST(Pfcmpeq          , Ext3dNow           , O(000F0F,B0,_,_,_,_,_,_  ), 0                         , 88 , 0  , 147, 57 ), // #509
  INST(Pfcmpge          , Ext3dNow           , O(000F0F,90,_,_,_,_,_,_  ), 0                         , 88 , 0  , 147, 57 ), // #510
  INST(Pfcmpgt          , Ext3dNow           , O(000F0F,A0,_,_,_,_,_,_  ), 0                         , 88 , 0  , 147, 57 ), // #511
  INST(Pfmax            , Ext3dNow           , O(000F0F,A4,_,_,_,_,_,_  ), 0                         , 88 , 0  , 147, 57 ), // #512
  INST(Pfmin            , Ext3dNow           , O(000F0F,94,_,_,_,_,_,_  ), 0                         , 88 , 0  , 147, 57 ), // #513
  INST(Pfmul            , Ext3dNow           , O(000F0F,B4,_,_,_,_,_,_  ), 0                         , 88 , 0  , 147, 57 ), // #514
  INST(Pfnacc           , Ext3dNow           , O(000F0F,8A,_,_,_,_,_,_  ), 0                         , 88 , 0  , 147, 108), // #515
  INST(Pfpnacc          , Ext3dNow           , O(000F0F,8E,_,_,_,_,_,_  ), 0                         , 88 , 0  , 147, 108), // #516
  INST(Pfrcp            , Ext3dNow           , O(000F0F,96,_,_,_,_,_,_  ), 0                         , 88 , 0  , 147, 57 ), // #517
  INST(Pfrcpit1         , Ext3dNow           , O(000F0F,A6,_,_,_,_,_,_  ), 0                         , 88 , 0  , 147, 57 ), // #518
  INST(Pfrcpit2         , Ext3dNow           , O(000F0F,B6,_,_,_,_,_,_  ), 0                         , 88 , 0  , 147, 57 ), // #519
  INST(Pfrcpv           , Ext3dNow           , O(000F0F,86,_,_,_,_,_,_  ), 0                         , 88 , 0  , 147, 109), // #520
  INST(Pfrsqit1         , Ext3dNow           , O(000F0F,A7,_,_,_,_,_,_  ), 0                         , 88 , 0  , 147, 57 ), // #521
  INST(Pfrsqrt          , Ext3dNow           , O(000F0F,97,_,_,_,_,_,_  ), 0                         , 88 , 0  , 147, 57 ), // #522
  INST(Pfrsqrtv         , Ext3dNow           , O(000F0F,87,_,_,_,_,_,_  ), 0                         , 88 , 0  , 147, 109), // #523
  INST(Pfsub            , Ext3dNow           , O(000F0F,9A,_,_,_,_,_,_  ), 0                         , 88 , 0  , 147, 57 ), // #524
  INST(Pfsubr           , Ext3dNow           , O(000F0F,AA,_,_,_,_,_,_  ), 0                         , 88 , 0  , 147, 57 ), // #525
  INST(Phaddd           , ExtRm_P            , O(000F38,02,_,_,_,_,_,_  ), 0                         , 1  , 0  , 143, 102), // #526
  INST(Phaddsw          , ExtRm_P            , O(000F38,03,_,_,_,_,_,_  ), 0                         , 1  , 0  , 143, 102), // #527
  INST(Phaddw           , ExtRm_P            , O(000F38,01,_,_,_,_,_,_  ), 0                         , 1  , 0  , 143, 102), // #528
  INST(Phminposuw       , ExtRm              , O(660F38,41,_,_,_,_,_,_  ), 0                         , 2  , 0  , 6  , 13 ), // #529
  INST(Phsubd           , ExtRm_P            , O(000F38,06,_,_,_,_,_,_  ), 0                         , 1  , 0  , 143, 102), // #530
  INST(Phsubsw          , ExtRm_P            , O(000F38,07,_,_,_,_,_,_  ), 0                         , 1  , 0  , 143, 102), // #531
  INST(Phsubw           , ExtRm_P            , O(000F38,05,_,_,_,_,_,_  ), 0                         , 1  , 0  , 143, 102), // #532
  INST(Pi2fd            , Ext3dNow           , O(000F0F,0D,_,_,_,_,_,_  ), 0                         , 88 , 0  , 147, 57 ), // #533
  INST(Pi2fw            , Ext3dNow           , O(000F0F,0C,_,_,_,_,_,_  ), 0                         , 88 , 0  , 147, 108), // #534
  INST(Pinsrb           , ExtRmi             , O(660F3A,20,_,_,_,_,_,_  ), 0                         , 9  , 0  , 156, 13 ), // #535
  INST(Pinsrd           , ExtRmi             , O(660F3A,22,_,_,_,_,_,_  ), 0                         , 9  , 0  , 157, 13 ), // #536
  INST(Pinsrq           , ExtRmi             , O(660F3A,22,_,_,1,_,_,_  ), 0                         , 91 , 0  , 158, 13 ), // #537
  INST(Pinsrw           , ExtRmi_P           , O(000F00,C4,_,_,_,_,_,_  ), 0                         , 5  , 0  , 159, 103), // #538
  INST(Pmaddubsw        , ExtRm_P            , O(000F38,04,_,_,_,_,_,_  ), 0                         , 1  , 0  , 143, 102), // #539
  INST(Pmaddwd          , ExtRm_P            , O(000F00,F5,_,_,_,_,_,_  ), 0                         , 5  , 0  , 143, 97 ), // #540
  INST(Pmaxsb           , ExtRm              , O(660F38,3C,_,_,_,_,_,_  ), 0                         , 2  , 0  , 12 , 13 ), // #541
  INST(Pmaxsd           , ExtRm              , O(660F38,3D,_,_,_,_,_,_  ), 0                         , 2  , 0  , 12 , 13 ), // #542
  INST(Pmaxsw           , ExtRm_P            , O(000F00,EE,_,_,_,_,_,_  ), 0                         , 5  , 0  , 145, 103), // #543
  INST(Pmaxub           , ExtRm_P            , O(000F00,DE,_,_,_,_,_,_  ), 0                         , 5  , 0  , 145, 103), // #544
  INST(Pmaxud           , ExtRm              , O(660F38,3F,_,_,_,_,_,_  ), 0                         , 2  , 0  , 12 , 13 ), // #545
  INST(Pmaxuw           , ExtRm              , O(660F38,3E,_,_,_,_,_,_  ), 0                         , 2  , 0  , 12 , 13 ), // #546
  INST(Pminsb           , ExtRm              , O(660F38,38,_,_,_,_,_,_  ), 0                         , 2  , 0  , 12 , 13 ), // #547
  INST(Pminsd           , ExtRm              , O(660F38,39,_,_,_,_,_,_  ), 0                         , 2  , 0  , 12 , 13 ), // #548
  INST(Pminsw           , ExtRm_P            , O(000F00,EA,_,_,_,_,_,_  ), 0                         , 5  , 0  , 145, 103), // #549
  INST(Pminub           , ExtRm_P            , O(000F00,DA,_,_,_,_,_,_  ), 0                         , 5  , 0  , 145, 103), // #550
  INST(Pminud           , ExtRm              , O(660F38,3B,_,_,_,_,_,_  ), 0                         , 2  , 0  , 12 , 13 ), // #551
  INST(Pminuw           , ExtRm              , O(660F38,3A,_,_,_,_,_,_  ), 0                         , 2  , 0  , 12 , 13 ), // #552
  INST(Pmovmskb         , ExtRm_P            , O(000F00,D7,_,_,_,_,_,_  ), 0                         , 5  , 0  , 160, 103), // #553
  INST(Pmovsxbd         , ExtRm              , O(660F38,21,_,_,_,_,_,_  ), 0                         , 2  , 0  , 8  , 13 ), // #554
  INST(Pmovsxbq         , ExtRm              , O(660F38,22,_,_,_,_,_,_  ), 0                         , 2  , 0  , 161, 13 ), // #555
  INST(Pmovsxbw         , ExtRm              , O(660F38,20,_,_,_,_,_,_  ), 0                         , 2  , 0  , 7  , 13 ), // #556
  INST(Pmovsxdq         , ExtRm              , O(660F38,25,_,_,_,_,_,_  ), 0                         , 2  , 0  , 7  , 13 ), // #557
  INST(Pmovsxwd         , ExtRm              , O(660F38,23,_,_,_,_,_,_  ), 0                         , 2  , 0  , 7  , 13 ), // #558
  INST(Pmovsxwq         , ExtRm              , O(660F38,24,_,_,_,_,_,_  ), 0                         , 2  , 0  , 8  , 13 ), // #559
  INST(Pmovzxbd         , ExtRm              , O(660F38,31,_,_,_,_,_,_  ), 0                         , 2  , 0  , 8  , 13 ), // #560
  INST(Pmovzxbq         , ExtRm              , O(660F38,32,_,_,_,_,_,_  ), 0                         , 2  , 0  , 161, 13 ), // #561
  INST(Pmovzxbw         , ExtRm              , O(660F38,30,_,_,_,_,_,_  ), 0                         , 2  , 0  , 7  , 13 ), // #562
  INST(Pmovzxdq         , ExtRm              , O(660F38,35,_,_,_,_,_,_  ), 0                         , 2  , 0  , 7  , 13 ), // #563
  INST(Pmovzxwd         , ExtRm              , O(660F38,33,_,_,_,_,_,_  ), 0                         , 2  , 0  , 7  , 13 ), // #564
  INST(Pmovzxwq         , ExtRm              , O(660F38,34,_,_,_,_,_,_  ), 0                         , 2  , 0  , 8  , 13 ), // #565
  INST(Pmuldq           , ExtRm              , O(660F38,28,_,_,_,_,_,_  ), 0                         , 2  , 0  , 6  , 13 ), // #566
  INST(Pmulhrsw         , ExtRm_P            , O(000F38,0B,_,_,_,_,_,_  ), 0                         , 1  , 0  , 143, 102), // #567
  INST(Pmulhrw          , Ext3dNow           , O(000F0F,B7,_,_,_,_,_,_  ), 0                         , 88 , 0  , 147, 57 ), // #568
  INST(Pmulhuw          , ExtRm_P            , O(000F00,E4,_,_,_,_,_,_  ), 0                         , 5  , 0  , 143, 103), // #569
  INST(Pmulhw           , ExtRm_P            , O(000F00,E5,_,_,_,_,_,_  ), 0                         , 5  , 0  , 143, 97 ), // #570
  INST(Pmulld           , ExtRm              , O(660F38,40,_,_,_,_,_,_  ), 0                         , 2  , 0  , 6  , 13 ), // #571
  INST(Pmullw           , ExtRm_P            , O(000F00,D5,_,_,_,_,_,_  ), 0                         , 5  , 0  , 143, 97 ), // #572
  INST(Pmuludq          , ExtRm_P            , O(000F00,F4,_,_,_,_,_,_  ), 0                         , 5  , 0  , 143, 5  ), // #573
  INST(Pop              , X86Pop             , O(000000,8F,0,_,_,_,_,_  ), O(000000,58,_,_,_,_,_,_  ), 0  , 66 , 162, 0  ), // #574
  INST(Popa             , X86Op              , O(660000,61,_,_,_,_,_,_  ), 0                         , 21 , 0  , 78 , 0  ), // #575
  INST(Popad            , X86Op              , O(000000,61,_,_,_,_,_,_  ), 0                         , 0  , 0  , 78 , 0  ), // #576
  INST(Popcnt           , X86Rm_Raw66H       , O(F30F00,B8,_,_,x,_,_,_  ), 0                         , 7  , 0  , 18 , 110), // #577
  INST(Popf             , X86Op              , O(660000,9D,_,_,_,_,_,_  ), 0                         , 21 , 0  , 26 , 111), // #578
  INST(Popfd            , X86Op              , O(000000,9D,_,_,_,_,_,_  ), 0                         , 0  , 0  , 78 , 111), // #579
  INST(Popfq            , X86Op              , O(000000,9D,_,_,_,_,_,_  ), 0                         , 0  , 0  , 29 , 111), // #580
  INST(Por              , ExtRm_P            , O(000F00,EB,_,_,_,_,_,_  ), 0                         , 5  , 0  , 145, 97 ), // #581
  INST(Prefetch         , X86M_Only          , O(000F00,0D,0,_,_,_,_,_  ), 0                         , 5  , 0  , 27 , 57 ), // #582
  INST(Prefetchit0      , X86M_Only          , O(000F00,18,7,_,_,_,_,_  ), 0                         , 24 , 0  , 68 , 112), // #583
  INST(Prefetchit1      , X86M_Only          , O(000F00,18,6,_,_,_,_,_  ), 0                         , 82 , 0  , 68 , 112), // #584
  INST(Prefetchnta      , X86M_Only          , O(000F00,18,0,_,_,_,_,_  ), 0                         , 5  , 0  , 27 , 6  ), // #585
  INST(Prefetcht0       , X86M_Only          , O(000F00,18,1,_,_,_,_,_  ), 0                         , 32 , 0  , 27 , 6  ), // #586
  INST(Prefetcht1       , X86M_Only          , O(000F00,18,2,_,_,_,_,_  ), 0                         , 78 , 0  , 27 , 6  ), // #587
  INST(Prefetcht2       , X86M_Only          , O(000F00,18,3,_,_,_,_,_  ), 0                         , 80 , 0  , 27 , 6  ), // #588
  INST(Prefetchw        , X86M_Only          , O(000F00,0D,1,_,_,_,_,_  ), 0                         , 32 , 0  , 27 , 113), // #589
  INST(Prefetchwt1      , X86M_Only          , O(000F00,0D,2,_,_,_,_,_  ), 0                         , 78 , 0  , 27 , 114), // #590
  INST(Psadbw           , ExtRm_P            , O(000F00,F6,_,_,_,_,_,_  ), 0                         , 5  , 0  , 143, 103), // #591
  INST(Pshufb           , ExtRm_P            , O(000F38,00,_,_,_,_,_,_  ), 0                         , 1  , 0  , 143, 102), // #592
  INST(Pshufd           , ExtRmi             , O(660F00,70,_,_,_,_,_,_  ), 0                         , 4  , 0  , 9  , 5  ), // #593
  INST(Pshufhw          , ExtRmi             , O(F30F00,70,_,_,_,_,_,_  ), 0                         , 7  , 0  , 9  , 5  ), // #594
  INST(Pshuflw          , ExtRmi             , O(F20F00,70,_,_,_,_,_,_  ), 0                         , 6  , 0  , 9  , 5  ), // #595
  INST(Pshufw           , ExtRmi_P           , O(000F00,70,_,_,_,_,_,_  ), 0                         , 5  , 0  , 163, 89 ), // #596
  INST(Psignb           , ExtRm_P            , O(000F38,08,_,_,_,_,_,_  ), 0                         , 1  , 0  , 143, 102), // #597
  INST(Psignd           , ExtRm_P            , O(000F38,0A,_,_,_,_,_,_  ), 0                         , 1  , 0  , 143, 102), // #598
  INST(Psignw           , ExtRm_P            , O(000F38,09,_,_,_,_,_,_  ), 0                         , 1  , 0  , 143, 102), // #599
  INST(Pslld            , ExtRmRi_P          , O(000F00,F2,_,_,_,_,_,_  ), O(000F00,72,6,_,_,_,_,_  ), 5  , 67 , 164, 97 ), // #600
  INST(Pslldq           , ExtRmRi            , 0                         , O(660F00,73,7,_,_,_,_,_  ), 0  , 68 , 165, 5  ), // #601
  INST(Psllq            , ExtRmRi_P          , O(000F00,F3,_,_,_,_,_,_  ), O(000F00,73,6,_,_,_,_,_  ), 5  , 69 , 164, 97 ), // #602
  INST(Psllw            , ExtRmRi_P          , O(000F00,F1,_,_,_,_,_,_  ), O(000F00,71,6,_,_,_,_,_  ), 5  , 70 , 164, 97 ), // #603
  INST(Psmash           , X86Op              , O(F30F01,FF,_,_,_,_,_,_  ), 0                         , 27 , 0  , 29 , 115), // #604
  INST(Psrad            , ExtRmRi_P          , O(000F00,E2,_,_,_,_,_,_  ), O(000F00,72,4,_,_,_,_,_  ), 5  , 71 , 164, 97 ), // #605
  INST(Psraw            , ExtRmRi_P          , O(000F00,E1,_,_,_,_,_,_  ), O(000F00,71,4,_,_,_,_,_  ), 5  , 72 , 164, 97 ), // #606
  INST(Psrld            , ExtRmRi_P          , O(000F00,D2,_,_,_,_,_,_  ), O(000F00,72,2,_,_,_,_,_  ), 5  , 73 , 164, 97 ), // #607
  INST(Psrldq           , ExtRmRi            , 0                         , O(660F00,73,3,_,_,_,_,_  ), 0  , 74 , 165, 5  ), // #608
  INST(Psrlq            , ExtRmRi_P          , O(000F00,D3,_,_,_,_,_,_  ), O(000F00,73,2,_,_,_,_,_  ), 5  , 75 , 164, 97 ), // #609
  INST(Psrlw            , ExtRmRi_P          , O(000F00,D1,_,_,_,_,_,_  ), O(000F00,71,2,_,_,_,_,_  ), 5  , 76 , 164, 97 ), // #610
  INST(Psubb            , ExtRm_P            , O(000F00,F8,_,_,_,_,_,_  ), 0                         , 5  , 0  , 146, 97 ), // #611
  INST(Psubd            , ExtRm_P            , O(000F00,FA,_,_,_,_,_,_  ), 0                         , 5  , 0  , 146, 97 ), // #612
  INST(Psubq            , ExtRm_P            , O(000F00,FB,_,_,_,_,_,_  ), 0                         , 5  , 0  , 146, 5  ), // #613
  INST(Psubsb           , ExtRm_P            , O(000F00,E8,_,_,_,_,_,_  ), 0                         , 5  , 0  , 146, 97 ), // #614
  INST(Psubsw           , ExtRm_P            , O(000F00,E9,_,_,_,_,_,_  ), 0                         , 5  , 0  , 146, 97 ), // #615
  INST(Psubusb          , ExtRm_P            , O(000F00,D8,_,_,_,_,_,_  ), 0                         , 5  , 0  , 146, 97 ), // #616
  INST(Psubusw          , ExtRm_P            , O(000F00,D9,_,_,_,_,_,_  ), 0                         , 5  , 0  , 146, 97 ), // #617
  INST(Psubw            , ExtRm_P            , O(000F00,F9,_,_,_,_,_,_  ), 0                         , 5  , 0  , 146, 97 ), // #618
  INST(Pswapd           , Ext3dNow           , O(000F0F,BB,_,_,_,_,_,_  ), 0                         , 88 , 0  , 147, 108), // #619
  INST(Ptest            , ExtRm              , O(660F38,17,_,_,_,_,_,_  ), 0                         , 2  , 0  , 6  , 116), // #620
  INST(Ptwrite          , X86M               , O(F30F00,AE,4,_,_,_,_,_  ), 0                         , 92 , 0  , 166, 117), // #621
  INST(Punpckhbw        , ExtRm_P            , O(000F00,68,_,_,_,_,_,_  ), 0                         , 5  , 0  , 143, 97 ), // #622
  INST(Punpckhdq        , ExtRm_P            , O(000F00,6A,_,_,_,_,_,_  ), 0                         , 5  , 0  , 143, 97 ), // #623
  INST(Punpckhqdq       , ExtRm              , O(660F00,6D,_,_,_,_,_,_  ), 0                         , 4  , 0  , 6  , 5  ), // #624
  INST(Punpckhwd        , ExtRm_P            , O(000F00,69,_,_,_,_,_,_  ), 0                         , 5  , 0  , 143, 97 ), // #625
  INST(Punpcklbw        , ExtRm_P            , O(000F00,60,_,_,_,_,_,_  ), 0                         , 5  , 0  , 167, 97 ), // #626
  INST(Punpckldq        , ExtRm_P            , O(000F00,62,_,_,_,_,_,_  ), 0                         , 5  , 0  , 167, 97 ), // #627
  INST(Punpcklqdq       , ExtRm              , O(660F00,6C,_,_,_,_,_,_  ), 0                         , 4  , 0  , 6  , 5  ), // #628
  INST(Punpcklwd        , ExtRm_P            , O(000F00,61,_,_,_,_,_,_  ), 0                         , 5  , 0  , 167, 97 ), // #629
  INST(Push             , X86Push            , O(000000,FF,6,_,_,_,_,_  ), O(000000,50,_,_,_,_,_,_  ), 34 , 77 , 168, 0  ), // #630
  INST(Pusha            , X86Op              , O(660000,60,_,_,_,_,_,_  ), 0                         , 21 , 0  , 78 , 0  ), // #631
  INST(Pushad           , X86Op              , O(000000,60,_,_,_,_,_,_  ), 0                         , 0  , 0  , 78 , 0  ), // #632
  INST(Pushf            , X86Op              , O(660000,9C,_,_,_,_,_,_  ), 0                         , 21 , 0  , 26 , 118), // #633
  INST(Pushfd           , X86Op              , O(000000,9C,_,_,_,_,_,_  ), 0                         , 0  , 0  , 78 , 118), // #634
  INST(Pushfq           , X86Op              , O(000000,9C,_,_,_,_,_,_  ), 0                         , 0  , 0  , 29 , 118), // #635
  INST(Pushw            , X86Pushw           , O(000000,FF,6,_,_,_,_,_  ), O(000000,50,_,_,_,_,_,_  ), 34 , 77 , 169, 0  ), // #636
  INST(Pvalidate        , X86Op              , O(F20F01,FF,_,_,_,_,_,_  ), 0                         , 93 , 0  , 26 , 119), // #637
  INST(Pxor             , ExtRm_P            , O(000F00,EF,_,_,_,_,_,_  ), 0                         , 5  , 0  , 146, 97 ), // #638
  INST(Rcl              , X86Rot             , O(000000,D0,2,_,x,_,_,_  ), 0                         , 3  , 0  , 170, 120), // #639
  INST(Rcpps            , ExtRm              , O(000F00,53,_,_,_,_,_,_  ), 0                         , 5  , 0  , 6  , 6  ), // #640
  INST(Rcpss            , ExtRm              , O(F30F00,53,_,_,_,_,_,_  ), 0                         , 7  , 0  , 8  , 6  ), // #641
  INST(Rcr              , X86Rot             , O(000000,D0,3,_,x,_,_,_  ), 0                         , 77 , 0  , 170, 120), // #642
  INST(Rdfsbase         , X86M               , O(F30F00,AE,0,_,x,_,_,_  ), 0                         , 7  , 0  , 171, 121), // #643
  INST(Rdgsbase         , X86M               , O(F30F00,AE,1,_,x,_,_,_  ), 0                         , 94 , 0  , 171, 121), // #644
  INST(Rdmsr            , X86Op              , O(000F00,32,_,_,_,_,_,_  ), 0                         , 5  , 0  , 172, 122), // #645
  INST(Rdpid            , X86R_Native        , O(F30F00,C7,7,_,_,_,_,_  ), 0                         , 95 , 0  , 173, 123), // #646
  INST(Rdpkru           , X86Op              , O(000F01,EE,_,_,_,_,_,_  ), 0                         , 23 , 0  , 174, 124), // #647
  INST(Rdpmc            , X86Op              , O(000F00,33,_,_,_,_,_,_  ), 0                         , 5  , 0  , 174, 0  ), // #648
  INST(Rdpru            , X86Op              , O(000F01,FD,_,_,_,_,_,_  ), 0                         , 23 , 0  , 174, 125), // #649
  INST(Rdrand           , X86M               , O(000F00,C7,6,_,x,_,_,_  ), 0                         , 82 , 0  , 19 , 126), // #650
  INST(Rdseed           , X86M               , O(000F00,C7,7,_,x,_,_,_  ), 0                         , 24 , 0  , 19 , 127), // #651
  INST(Rdsspd           , X86M               , O(F30F00,1E,1,_,_,_,_,_  ), 0                         , 94 , 0  , 73 , 64 ), // #652
  INST(Rdsspq           , X86M               , O(F30F00,1E,1,_,_,_,_,_  ), 0                         , 94 , 0  , 74 , 64 ), // #653
  INST(Rdtsc            , X86Op              , O(000F00,31,_,_,_,_,_,_  ), 0                         , 5  , 0  , 24 , 128), // #654
  INST(Rdtscp           , X86Op              , O(000F01,F9,_,_,_,_,_,_  ), 0                         , 23 , 0  , 174, 129), // #655
  INST(Ret              , X86Ret             , O(000000,C2,_,_,_,_,_,_  ), 0                         , 0  , 0  , 175, 0  ), // #656
  INST(Retf             , X86Ret             , O(000000,CA,_,_,x,_,_,_  ), 0                         , 0  , 0  , 176, 0  ), // #657
  INST(Rmpadjust        , X86Op              , O(F30F01,FE,_,_,_,_,_,_  ), 0                         , 27 , 0  , 29 , 115), // #658
  INST(Rmpupdate        , X86Op              , O(F20F01,FE,_,_,_,_,_,_  ), 0                         , 93 , 0  , 29 , 115), // #659
  INST(Rol              , X86Rot             , O(000000,D0,0,_,x,_,_,_  ), 0                         , 0  , 0  , 170, 130), // #660
  INST(Ror              , X86Rot             , O(000000,D0,1,_,x,_,_,_  ), 0                         , 33 , 0  , 170, 130), // #661
  INST(Rorx             , VexRmi_Wx          , V(F20F3A,F0,_,0,x,_,_,_  ), 0                         , 96 , 0  , 177, 101), // #662
  INST(Roundpd          , ExtRmi             , O(660F3A,09,_,_,_,_,_,_  ), 0                         , 9  , 0  , 9  , 13 ), // #663
  INST(Roundps          , ExtRmi             , O(660F3A,08,_,_,_,_,_,_  ), 0                         , 9  , 0  , 9  , 13 ), // #664
  INST(Roundsd          , ExtRmi             , O(660F3A,0B,_,_,_,_,_,_  ), 0                         , 9  , 0  , 34 , 13 ), // #665
  INST(Roundss          , ExtRmi             , O(660F3A,0A,_,_,_,_,_,_  ), 0                         , 9  , 0  , 35 , 13 ), // #666
  INST(Rsm              , X86Op              , O(000F00,AA,_,_,_,_,_,_  ), 0                         , 5  , 0  , 78 , 1  ), // #667
  INST(Rsqrtps          , ExtRm              , O(000F00,52,_,_,_,_,_,_  ), 0                         , 5  , 0  , 6  , 6  ), // #668
  INST(Rsqrtss          , ExtRm              , O(F30F00,52,_,_,_,_,_,_  ), 0                         , 7  , 0  , 8  , 6  ), // #669
  INST(Rstorssp         , X86M_Only          , O(F30F00,01,5,_,_,_,_,_  ), 0                         , 65 , 0  , 28 , 24 ), // #670
  INST(Sahf             , X86Op              , O(000000,9E,_,_,_,_,_,_  ), 0                         , 0  , 0  , 95 , 131), // #671
  INST(Sar              , X86Rot             , O(000000,D0,7,_,x,_,_,_  ), 0                         , 29 , 0  , 170, 1  ), // #672
  INST(Sarx             , VexRmv_Wx          , V(F30F38,F7,_,0,x,_,_,_  ), 0                         , 89 , 0  , 14 , 101), // #673
  INST(Saveprevssp      , X86Op              , O(F30F01,EA,_,_,_,_,_,_  ), 0                         , 27 , 0  , 26 , 24 ), // #674
  INST(Sbb              , X86Arith           , O(000000,18,3,_,x,_,_,_  ), 0                         , 77 , 0  , 178, 3  ), // #675
  INST(Scas             , X86StrRm           , O(000000,AE,_,_,_,_,_,_  ), 0                         , 0  , 0  , 179, 38 ), // #676
  INST(Seamcall         , X86Op              , O(660F01,CF,_,_,_,_,_,_  ), 0                         , 97 , 0  , 26 , 132), // #677
  INST(Seamops          , X86Op              , O(660F01,CE,_,_,_,_,_,_  ), 0                         , 97 , 0  , 26 , 132), // #678
  INST(Seamret          , X86Op              , O(660F01,CD,_,_,_,_,_,_  ), 0                         , 97 , 0  , 26 , 132), // #679
  INST(Senduipi         , X86M_NoSize        , O(F30F00,C7,6,_,_,_,_,_  ), 0                         , 26 , 0  , 74 , 25 ), // #680
  INST(Serialize        , X86Op              , O(000F01,E8,_,_,_,_,_,_  ), 0                         , 23 , 0  , 26 , 133), // #681
  INST(Setb             , X86Set             , O(000F00,92,_,_,_,_,_,_  ), 0                         , 5  , 0  , 180, 68 ), // #682
  INST(Setbe            , X86Set             , O(000F00,96,_,_,_,_,_,_  ), 0                         , 5  , 0  , 180, 69 ), // #683
  INST(Setl             , X86Set             , O(000F00,9C,_,_,_,_,_,_  ), 0                         , 5  , 0  , 180, 70 ), // #684
  INST(Setle            , X86Set             , O(000F00,9E,_,_,_,_,_,_  ), 0                         , 5  , 0  , 180, 71 ), // #685
  INST(Setnb            , X86Set             , O(000F00,93,_,_,_,_,_,_  ), 0                         , 5  , 0  , 180, 68 ), // #686
  INST(Setnbe           , X86Set             , O(000F00,97,_,_,_,_,_,_  ), 0                         , 5  , 0  , 180, 69 ), // #687
  INST(Setnl            , X86Set             , O(000F00,9D,_,_,_,_,_,_  ), 0                         , 5  , 0  , 180, 70 ), // #688
  INST(Setnle           , X86Set             , O(000F00,9F,_,_,_,_,_,_  ), 0                         , 5  , 0  , 180, 71 ), // #689
  INST(Setno            , X86Set             , O(000F00,91,_,_,_,_,_,_  ), 0                         , 5  , 0  , 180, 65 ), // #690
  INST(Setnp            , X86Set             , O(000F00,9B,_,_,_,_,_,_  ), 0                         , 5  , 0  , 180, 72 ), // #691
  INST(Setns            , X86Set             , O(000F00,99,_,_,_,_,_,_  ), 0                         , 5  , 0  , 180, 73 ), // #692
  INST(Setnz            , X86Set             , O(000F00,95,_,_,_,_,_,_  ), 0                         , 5  , 0  , 180, 74 ), // #693
  INST(Seto             , X86Set             , O(000F00,90,_,_,_,_,_,_  ), 0                         , 5  , 0  , 180, 65 ), // #694
  INST(Setp             , X86Set             , O(000F00,9A,_,_,_,_,_,_  ), 0                         , 5  , 0  , 180, 72 ), // #695
  INST(Sets             , X86Set             , O(000F00,98,_,_,_,_,_,_  ), 0                         , 5  , 0  , 180, 73 ), // #696
  INST(Setssbsy         , X86Op              , O(F30F01,E8,_,_,_,_,_,_  ), 0                         , 27 , 0  , 26 , 64 ), // #697
  INST(Setz             , X86Set             , O(000F00,94,_,_,_,_,_,_  ), 0                         , 5  , 0  , 180, 74 ), // #698
  INST(Sfence           , X86Fence           , O(000F00,AE,7,_,_,_,_,_  ), 0                         , 24 , 0  , 26 , 6  ), // #699
  INST(Sgdt             , X86M_Only          , O(000F00,01,0,_,_,_,_,_  ), 0                         , 5  , 0  , 27 , 0  ), // #700
  INST(Sha1msg1         , ExtRm              , O(000F38,C9,_,_,_,_,_,_  ), 0                         , 1  , 0  , 6  , 134), // #701
  INST(Sha1msg2         , ExtRm              , O(000F38,CA,_,_,_,_,_,_  ), 0                         , 1  , 0  , 6  , 134), // #702
  INST(Sha1nexte        , ExtRm              , O(000F38,C8,_,_,_,_,_,_  ), 0                         , 1  , 0  , 6  , 134), // #703
  INST(Sha1rnds4        , ExtRmi             , O(000F3A,CC,_,_,_,_,_,_  ), 0                         , 86 , 0  , 9  , 134), // #704
  INST(Sha256msg1       , ExtRm              , O(000F38,CC,_,_,_,_,_,_  ), 0                         , 1  , 0  , 6  , 134), // #705
  INST(Sha256msg2       , ExtRm              , O(000F38,CD,_,_,_,_,_,_  ), 0                         , 1  , 0  , 6  , 134), // #706
  INST(Sha256rnds2      , ExtRm_XMM0         , O(000F38,CB,_,_,_,_,_,_  ), 0                         , 1  , 0  , 16 , 134), // #707
  INST(Shl              , X86Rot             , O(000000,D0,4,_,x,_,_,_  ), 0                         , 10 , 0  , 170, 1  ), // #708
  INST(Shld             , X86ShldShrd        , O(000F00,A4,_,_,x,_,_,_  ), 0                         , 5  , 0  , 181, 1  ), // #709
  INST(Shlx             , VexRmv_Wx          , V(660F38,F7,_,0,x,_,_,_  ), 0                         , 30 , 0  , 14 , 101), // #710
  INST(Shr              , X86Rot             , O(000000,D0,5,_,x,_,_,_  ), 0                         , 64 , 0  , 170, 1  ), // #711
  INST(Shrd             , X86ShldShrd        , O(000F00,AC,_,_,x,_,_,_  ), 0                         , 5  , 0  , 181, 1  ), // #712
  INST(Shrx             , VexRmv_Wx          , V(F20F38,F7,_,0,x,_,_,_  ), 0                         , 85 , 0  , 14 , 101), // #713
  INST(Shufpd           , ExtRmi             , O(660F00,C6,_,_,_,_,_,_  ), 0                         , 4  , 0  , 9  , 5  ), // #714
  INST(Shufps           , ExtRmi             , O(000F00,C6,_,_,_,_,_,_  ), 0                         , 5  , 0  , 9  , 6  ), // #715
  INST(Sidt             , X86M_Only          , O(000F00,01,1,_,_,_,_,_  ), 0                         , 32 , 0  , 27 , 0  ), // #716
  INST(Skinit           , X86Op_xAX          , O(000F01,DE,_,_,_,_,_,_  ), 0                         , 23 , 0  , 49 , 135), // #717
  INST(Sldt             , X86M_NoMemSize     , O(000F00,00,0,_,_,_,_,_  ), 0                         , 5  , 0  , 182, 0  ), // #718
  INST(Slwpcb           , VexR_Wx            , V(XOP_M9,12,1,0,x,_,_,_  ), 0                         , 13 , 0  , 106, 86 ), // #719
  INST(Smsw             , X86M_NoMemSize     , O(000F00,01,4,_,_,_,_,_  ), 0                         , 98 , 0  , 182, 0  ), // #720
  INST(Sqrtpd           , ExtRm              , O(660F00,51,_,_,_,_,_,_  ), 0                         , 4  , 0  , 6  , 5  ), // #721
  INST(Sqrtps           , ExtRm              , O(000F00,51,_,_,_,_,_,_  ), 0                         , 5  , 0  , 6  , 6  ), // #722
  INST(Sqrtsd           , ExtRm              , O(F20F00,51,_,_,_,_,_,_  ), 0                         , 6  , 0  , 7  , 5  ), // #723
  INST(Sqrtss           , ExtRm              , O(F30F00,51,_,_,_,_,_,_  ), 0                         , 7  , 0  , 8  , 6  ), // #724
  INST(Stac             , X86Op              , O(000F01,CB,_,_,_,_,_,_  ), 0                         , 23 , 0  , 26 , 16 ), // #725
  INST(Stc              , X86Op              , O(000000,F9,_,_,_,_,_,_  ), 0                         , 0  , 0  , 26 , 17 ), // #726
  INST(Std              , X86Op              , O(000000,FD,_,_,_,_,_,_  ), 0                         , 0  , 0  , 26 , 18 ), // #727
  INST(Stgi             , X86Op              , O(000F01,DC,_,_,_,_,_,_  ), 0                         , 23 , 0  , 26 , 135), // #728
  INST(Sti              , X86Op              , O(000000,FB,_,_,_,_,_,_  ), 0                         , 0  , 0  , 26 , 23 ), // #729
  INST(Stmxcsr          , X86M_Only          , O(000F00,AE,3,_,_,_,_,_  ), 0                         , 80 , 0  , 99 , 6  ), // #730
  INST(Stos             , X86StrMr           , O(000000,AA,_,_,_,_,_,_  ), 0                         , 0  , 0  , 183, 87 ), // #731
  INST(Str              , X86M_NoMemSize     , O(000F00,00,1,_,_,_,_,_  ), 0                         , 32 , 0  , 182, 0  ), // #732
  INST(Sttilecfg        , AmxCfg             , V(660F38,49,_,0,0,_,_,_  ), 0                         , 30 , 0  , 101, 85 ), // #733
  INST(Stui             , X86Op              , O(F30F01,EF,_,_,_,_,_,_  ), 0                         , 27 , 0  , 29 , 25 ), // #734
  INST(Sub              , X86Arith           , O(000000,28,5,_,x,_,_,_  ), 0                         , 64 , 0  , 178, 1  ), // #735
  INST(Subpd            , ExtRm              , O(660F00,5C,_,_,_,_,_,_  ), 0                         , 4  , 0  , 6  , 5  ), // #736
  INST(Subps            , ExtRm              , O(000F00,5C,_,_,_,_,_,_  ), 0                         , 5  , 0  , 6  , 6  ), // #737
  INST(Subsd            , ExtRm              , O(F20F00,5C,_,_,_,_,_,_  ), 0                         , 6  , 0  , 7  , 5  ), // #738
  INST(Subss            , ExtRm              , O(F30F00,5C,_,_,_,_,_,_  ), 0                         , 7  , 0  , 8  , 6  ), // #739
  INST(Swapgs           , X86Op              , O(000F01,F8,_,_,_,_,_,_  ), 0                         , 23 , 0  , 29 , 0  ), // #740
  INST(Syscall          , X86Op              , O(000F00,05,_,_,_,_,_,_  ), 0                         , 5  , 0  , 29 , 0  ), // #741
  INST(Sysenter         , X86Op              , O(000F00,34,_,_,_,_,_,_  ), 0                         , 5  , 0  , 26 , 0  ), // #742
  INST(Sysexit          , X86Op              , O(000F00,35,_,_,_,_,_,_  ), 0                         , 5  , 0  , 26 , 0  ), // #743
  INST(Sysexitq         , X86Op              , O(000F00,35,_,_,1,_,_,_  ), 0                         , 62 , 0  , 29 , 0  ), // #744
  INST(Sysret           , X86Op              , O(000F00,07,_,_,_,_,_,_  ), 0                         , 5  , 0  , 29 , 0  ), // #745
  INST(Sysretq          , X86Op              , O(000F00,07,_,_,1,_,_,_  ), 0                         , 62 , 0  , 29 , 0  ), // #746
  INST(T1mskc           , VexVm_Wx           , V(XOP_M9,01,7,0,x,_,_,_  ), 0                         , 99 , 0  , 15 , 12 ), // #747
  INST(Tcmmimfp16ps     , AmxRmv             , V(660F38,6C,_,0,0,_,_,_  ), 0                         , 30 , 0  , 184, 136), // #748
  INST(Tcmmrlfp16ps     , AmxRmv             , V(000F38,6C,_,0,0,_,_,_  ), 0                         , 11 , 0  , 184, 136), // #749
  INST(Tdcall           , X86Op              , O(660F01,CC,_,_,_,_,_,_  ), 0                         , 97 , 0  , 26 , 132), // #750
  INST(Tdpbf16ps        , AmxRmv             , V(F30F38,5C,_,0,0,_,_,_  ), 0                         , 89 , 0  , 184, 137), // #751
  INST(Tdpbssd          , AmxRmv             , V(F20F38,5E,_,0,0,_,_,_  ), 0                         , 85 , 0  , 184, 138), // #752
  INST(Tdpbsud          , AmxRmv             , V(F30F38,5E,_,0,0,_,_,_  ), 0                         , 89 , 0  , 184, 138), // #753
  INST(Tdpbusd          , AmxRmv             , V(660F38,5E,_,0,0,_,_,_  ), 0                         , 30 , 0  , 184, 138), // #754
  INST(Tdpbuud          , AmxRmv             , V(000F38,5E,_,0,0,_,_,_  ), 0                         , 11 , 0  , 184, 138), // #755
  INST(Tdpfp16ps        , AmxRmv             , V(F20F38,5C,_,0,0,_,_,_  ), 0                         , 85 , 0  , 184, 139), // #756
  INST(Test             , X86Test            , O(000000,84,_,_,x,_,_,_  ), O(000000,F6,_,_,x,_,_,_  ), 0  , 78 , 185, 1  ), // #757
  INST(Testui           , X86Op              , O(F30F01,ED,_,_,_,_,_,_  ), 0                         , 27 , 0  , 29 , 140), // #758
  INST(Tileloadd        , AmxRm              , V(F20F38,4B,_,0,0,_,_,_  ), 0                         , 85 , 0  , 186, 85 ), // #759
  INST(Tileloaddt1      , AmxRm              , V(660F38,4B,_,0,0,_,_,_  ), 0                         , 30 , 0  , 186, 85 ), // #760
  INST(Tilerelease      , VexOpMod           , V(000F38,49,0,0,0,_,_,_  ), 0                         , 11 , 0  , 187, 85 ), // #761
  INST(Tilestored       , AmxMr              , V(F30F38,4B,_,0,0,_,_,_  ), 0                         , 89 , 0  , 188, 85 ), // #762
  INST(Tilezero         , AmxR               , V(F20F38,49,_,0,0,_,_,_  ), 0                         , 85 , 0  , 189, 85 ), // #763
  INST(Tlbsync          , X86Op              , O(000F01,FF,_,_,_,_,_,_  ), 0                         , 23 , 0  , 26 , 67 ), // #764
  INST(Tpause           , X86R32_EDX_EAX     , O(660F00,AE,6,_,_,_,_,_  ), 0                         , 28 , 0  , 190, 141), // #765
  INST(Tzcnt            , X86Rm_Raw66H       , O(F30F00,BC,_,_,x,_,_,_  ), 0                         , 7  , 0  , 18 , 10 ), // #766
  INST(Tzmsk            , VexVm_Wx           , V(XOP_M9,01,4,0,x,_,_,_  ), 0                         , 100, 0  , 15 , 12 ), // #767
  INST(Ucomisd          , ExtRm              , O(660F00,2E,_,_,_,_,_,_  ), 0                         , 4  , 0  , 7  , 42 ), // #768
  INST(Ucomiss          , ExtRm              , O(000F00,2E,_,_,_,_,_,_  ), 0                         , 5  , 0  , 8  , 43 ), // #769
  INST(Ud0              , X86Rm              , O(000F00,FF,_,_,_,_,_,_  ), 0                         , 5  , 0  , 191, 0  ), // #770
  INST(Ud1              , X86Rm              , O(000F00,B9,_,_,_,_,_,_  ), 0                         , 5  , 0  , 191, 0  ), // #771
  INST(Ud2              , X86Op              , O(000F00,0B,_,_,_,_,_,_  ), 0                         , 5  , 0  , 26 , 0  ), // #772
  INST(Uiret            , X86Op              , O(F30F01,EC,_,_,_,_,_,_  ), 0                         , 27 , 0  , 29 , 25 ), // #773
  INST(Umonitor         , X86R_FromM         , O(F30F00,AE,6,_,_,_,_,_  ), 0                         , 26 , 0  , 192, 142), // #774
  INST(Umwait           , X86R32_EDX_EAX     , O(F20F00,AE,6,_,_,_,_,_  ), 0                         , 101, 0  , 190, 141), // #775
  INST(Unpckhpd         , ExtRm              , O(660F00,15,_,_,_,_,_,_  ), 0                         , 4  , 0  , 6  , 5  ), // #776
  INST(Unpckhps         , ExtRm              , O(000F00,15,_,_,_,_,_,_  ), 0                         , 5  , 0  , 6  , 6  ), // #777
  INST(Unpcklpd         , ExtRm              , O(660F00,14,_,_,_,_,_,_  ), 0                         , 4  , 0  , 6  , 5  ), // #778
  INST(Unpcklps         , ExtRm              , O(000F00,14,_,_,_,_,_,_  ), 0                         , 5  , 0  , 6  , 6  ), // #779
  INST(Vaddpd           , VexRvm_Lx          , V(660F00,58,_,x,I,1,4,FV ), 0                         , 102, 0  , 193, 143), // #780
  INST(Vaddph           , VexRvm_Lx          , E(00MAP5,58,_,_,_,0,4,FV ), 0                         , 103, 0  , 194, 144), // #781
  INST(Vaddps           , VexRvm_Lx          , V(000F00,58,_,x,I,0,4,FV ), 0                         , 104, 0  , 195, 143), // #782
  INST(Vaddsd           , VexRvm             , V(F20F00,58,_,I,I,1,3,T1S), 0                         , 105, 0  , 196, 143), // #783
  INST(Vaddsh           , VexRvm             , E(F3MAP5,58,_,_,_,0,1,T1S), 0                         , 106, 0  , 197, 144), // #784
  INST(Vaddss           , VexRvm             , V(F30F00,58,_,I,I,0,2,T1S), 0                         , 107, 0  , 198, 143), // #785
  INST(Vaddsubpd        , VexRvm_Lx          , V(660F00,D0,_,x,I,_,_,_  ), 0                         , 71 , 0  , 199, 145), // #786
  INST(Vaddsubps        , VexRvm_Lx          , V(F20F00,D0,_,x,I,_,_,_  ), 0                         , 108, 0  , 199, 145), // #787
  INST(Vaesdec          , VexRvm_Lx          , V(660F38,DE,_,x,I,_,4,FVM), 0                         , 109, 0  , 200, 146), // #788
  INST(Vaesdeclast      , VexRvm_Lx          , V(660F38,DF,_,x,I,_,4,FVM), 0                         , 109, 0  , 200, 146), // #789
  INST(Vaesenc          , VexRvm_Lx          , V(660F38,DC,_,x,I,_,4,FVM), 0                         , 109, 0  , 200, 146), // #790
  INST(Vaesenclast      , VexRvm_Lx          , V(660F38,DD,_,x,I,_,4,FVM), 0                         , 109, 0  , 200, 146), // #791
  INST(Vaesimc          , VexRm              , V(660F38,DB,_,0,I,_,_,_  ), 0                         , 30 , 0  , 201, 147), // #792
  INST(Vaeskeygenassist , VexRmi             , V(660F3A,DF,_,0,I,_,_,_  ), 0                         , 75 , 0  , 202, 147), // #793
  INST(Valignd          , VexRvmi_Lx         , E(660F3A,03,_,x,_,0,4,FV ), 0                         , 110, 0  , 203, 148), // #794
  INST(Valignq          , VexRvmi_Lx         , E(660F3A,03,_,x,_,1,4,FV ), 0                         , 111, 0  , 204, 148), // #795
  INST(Vandnpd          , VexRvm_Lx          , V(660F00,55,_,x,I,1,4,FV ), 0                         , 102, 0  , 205, 149), // #796
  INST(Vandnps          , VexRvm_Lx          , V(000F00,55,_,x,I,0,4,FV ), 0                         , 104, 0  , 206, 149), // #797
  INST(Vandpd           , VexRvm_Lx          , V(660F00,54,_,x,I,1,4,FV ), 0                         , 102, 0  , 207, 149), // #798
  INST(Vandps           , VexRvm_Lx          , V(000F00,54,_,x,I,0,4,FV ), 0                         , 104, 0  , 208, 149), // #799
  INST(Vbcstnebf162ps   , VexRm_Lx           , V(F30F38,B1,_,x,0,_,_,_  ), 0                         , 89 , 0  , 209, 150), // #800
  INST(Vbcstnesh2ps     , VexRm_Lx           , V(660F38,B1,_,x,0,_,_,_  ), 0                         , 30 , 0  , 209, 150), // #801
  INST(Vblendmpd        , VexRvm_Lx          , E(660F38,65,_,x,_,1,4,FV ), 0                         , 112, 0  , 210, 148), // #802
  INST(Vblendmps        , VexRvm_Lx          , E(660F38,65,_,x,_,0,4,FV ), 0                         , 113, 0  , 211, 148), // #803
  INST(Vblendpd         , VexRvmi_Lx         , V(660F3A,0D,_,x,I,_,_,_  ), 0                         , 75 , 0  , 212, 145), // #804
  INST(Vblendps         , VexRvmi_Lx         , V(660F3A,0C,_,x,I,_,_,_  ), 0                         , 75 , 0  , 212, 145), // #805
  INST(Vblendvpd        , VexRvmr_Lx         , V(660F3A,4B,_,x,0,_,_,_  ), 0                         , 75 , 0  , 213, 145), // #806
  INST(Vblendvps        , VexRvmr_Lx         , V(660F3A,4A,_,x,0,_,_,_  ), 0                         , 75 , 0  , 213, 145), // #807
  INST(Vbroadcastf128   , VexRm              , V(660F38,1A,_,1,0,_,_,_  ), 0                         , 114, 0  , 214, 145), // #808
  INST(Vbroadcastf32x2  , VexRm_Lx           , E(660F38,19,_,x,_,0,3,T2 ), 0                         , 115, 0  , 215, 151), // #809
  INST(Vbroadcastf32x4  , VexRm_Lx           , E(660F38,1A,_,x,_,0,4,T4 ), 0                         , 116, 0  , 216, 148), // #810
  INST(Vbroadcastf32x8  , VexRm              , E(660F38,1B,_,2,_,0,5,T8 ), 0                         , 117, 0  , 217, 151), // #811
  INST(Vbroadcastf64x2  , VexRm_Lx           , E(660F38,1A,_,x,_,1,4,T2 ), 0                         , 118, 0  , 216, 151), // #812
  INST(Vbroadcastf64x4  , VexRm              , E(660F38,1B,_,2,_,1,5,T4 ), 0                         , 119, 0  , 217, 148), // #813
  INST(Vbroadcasti128   , VexRm              , V(660F38,5A,_,1,0,_,_,_  ), 0                         , 114, 0  , 214, 152), // #814
  INST(Vbroadcasti32x2  , VexRm_Lx           , E(660F38,59,_,x,_,0,3,T2 ), 0                         , 115, 0  , 218, 151), // #815
  INST(Vbroadcasti32x4  , VexRm_Lx           , E(660F38,5A,_,x,_,0,4,T4 ), 0                         , 116, 0  , 216, 148), // #816
  INST(Vbroadcasti32x8  , VexRm              , E(660F38,5B,_,2,_,0,5,T8 ), 0                         , 117, 0  , 217, 151), // #817
  INST(Vbroadcasti64x2  , VexRm_Lx           , E(660F38,5A,_,x,_,1,4,T2 ), 0                         , 118, 0  , 216, 151), // #818
  INST(Vbroadcasti64x4  , VexRm              , E(660F38,5B,_,2,_,1,5,T4 ), 0                         , 119, 0  , 217, 148), // #819
  INST(Vbroadcastsd     , VexRm_Lx           , V(660F38,19,_,x,0,1,3,T1S), 0                         , 120, 0  , 219, 153), // #820
  INST(Vbroadcastss     , VexRm_Lx           , V(660F38,18,_,x,0,0,2,T1S), 0                         , 121, 0  , 220, 153), // #821
  INST(Vcmppd           , VexRvmi_Lx_KEvex   , V(660F00,C2,_,x,I,1,4,FV ), 0                         , 102, 0  , 221, 143), // #822
  INST(Vcmpph           , VexRvmi_Lx_KEvex   , E(000F3A,C2,_,_,_,0,4,FV ), 0                         , 122, 0  , 222, 144), // #823
  INST(Vcmpps           , VexRvmi_Lx_KEvex   , V(000F00,C2,_,x,I,0,4,FV ), 0                         , 104, 0  , 223, 143), // #824
  INST(Vcmpsd           , VexRvmi_KEvex      , V(F20F00,C2,_,I,I,1,3,T1S), 0                         , 105, 0  , 224, 143), // #825
  INST(Vcmpsh           , VexRvmi_KEvex      , E(F30F3A,C2,_,_,_,0,1,T1S), 0                         , 123, 0  , 225, 144), // #826
  INST(Vcmpss           , VexRvmi_KEvex      , V(F30F00,C2,_,I,I,0,2,T1S), 0                         , 107, 0  , 226, 143), // #827
  INST(Vcomisd          , VexRm              , V(660F00,2F,_,I,I,1,3,T1S), 0                         , 124, 0  , 227, 154), // #828
  INST(Vcomish          , VexRm              , E(00MAP5,2F,_,_,_,0,1,T1S), 0                         , 125, 0  , 228, 155), // #829
  INST(Vcomiss          , VexRm              , V(000F00,2F,_,I,I,0,2,T1S), 0                         , 126, 0  , 229, 154), // #830
  INST(Vcompresspd      , VexMr_Lx           , E(660F38,8A,_,x,_,1,3,T1S), 0                         , 127, 0  , 230, 148), // #831
  INST(Vcompressps      , VexMr_Lx           , E(660F38,8A,_,x,_,0,2,T1S), 0                         , 128, 0  , 230, 148), // #832
  INST(Vcvtdq2pd        , VexRm_Lx           , V(F30F00,E6,_,x,I,0,3,HV ), 0                         , 129, 0  , 231, 143), // #833
  INST(Vcvtdq2ph        , VexRm_Lx_Narrow    , E(00MAP5,5B,_,x,0,0,4,FV ), 0                         , 103, 0  , 232, 144), // #834
  INST(Vcvtdq2ps        , VexRm_Lx           , V(000F00,5B,_,x,I,0,4,FV ), 0                         , 104, 0  , 233, 143), // #835
  INST(Vcvtne2ps2bf16   , VexRvm_Lx          , E(F20F38,72,_,_,_,0,4,FV ), 0                         , 130, 0  , 211, 156), // #836
  INST(Vcvtneebf162ps   , VexRm_Lx           , V(F30F38,B0,_,x,0,_,_,_  ), 0                         , 89 , 0  , 234, 150), // #837
  INST(Vcvtneeph2ps     , VexRm_Lx           , V(660F38,B0,_,x,0,_,_,_  ), 0                         , 30 , 0  , 234, 150), // #838
  INST(Vcvtneobf162ps   , VexRm_Lx           , V(F20F38,B0,_,x,0,_,_,_  ), 0                         , 85 , 0  , 234, 150), // #839
  INST(Vcvtneoph2ps     , VexRm_Lx           , V(000F38,B0,_,x,0,_,_,_  ), 0                         , 11 , 0  , 234, 150), // #840
  INST(Vcvtneps2bf16    , VexRm_Lx_Narrow    , V(F30F38,72,_,_,_,0,4,FV ), 0                         , 131, 0  , 235, 157), // #841
  INST(Vcvtpd2dq        , VexRm_Lx_Narrow    , V(F20F00,E6,_,x,I,1,4,FV ), 0                         , 132, 0  , 236, 143), // #842
  INST(Vcvtpd2ph        , VexRm_Lx           , E(66MAP5,5A,_,_,_,1,4,FV ), 0                         , 133, 0  , 237, 144), // #843
  INST(Vcvtpd2ps        , VexRm_Lx_Narrow    , V(660F00,5A,_,x,I,1,4,FV ), 0                         , 102, 0  , 236, 143), // #844
  INST(Vcvtpd2qq        , VexRm_Lx           , E(660F00,7B,_,x,_,1,4,FV ), 0                         , 134, 0  , 238, 151), // #845
  INST(Vcvtpd2udq       , VexRm_Lx_Narrow    , E(000F00,79,_,x,_,1,4,FV ), 0                         , 135, 0  , 239, 148), // #846
  INST(Vcvtpd2uqq       , VexRm_Lx           , E(660F00,79,_,x,_,1,4,FV ), 0                         , 134, 0  , 238, 151), // #847
  INST(Vcvtph2dq        , VexRm_Lx           , E(66MAP5,5B,_,_,_,0,3,HV ), 0                         , 136, 0  , 240, 144), // #848
  INST(Vcvtph2pd        , VexRm_Lx           , E(00MAP5,5A,_,_,_,0,2,QV ), 0                         , 137, 0  , 241, 144), // #849
  INST(Vcvtph2ps        , VexRm_Lx           , V(660F38,13,_,x,0,0,3,HVM), 0                         , 138, 0  , 242, 158), // #850
  INST(Vcvtph2psx       , VexRm_Lx           , E(66MAP6,13,_,_,_,0,3,HV ), 0                         , 139, 0  , 243, 144), // #851
  INST(Vcvtph2qq        , VexRm_Lx           , E(66MAP5,7B,_,_,_,0,2,QV ), 0                         , 140, 0  , 244, 144), // #852
  INST(Vcvtph2udq       , VexRm_Lx           , E(00MAP5,79,_,_,_,0,3,HV ), 0                         , 141, 0  , 240, 144), // #853
  INST(Vcvtph2uqq       , VexRm_Lx           , E(66MAP5,79,_,_,_,0,2,QV ), 0                         , 140, 0  , 244, 144), // #854
  INST(Vcvtph2uw        , VexRm_Lx           , E(00MAP5,7D,_,_,_,0,4,FV ), 0                         , 103, 0  , 245, 144), // #855
  INST(Vcvtph2w         , VexRm_Lx           , E(66MAP5,7D,_,_,_,0,4,FV ), 0                         , 142, 0  , 245, 144), // #856
  INST(Vcvtps2dq        , VexRm_Lx           , V(660F00,5B,_,x,I,0,4,FV ), 0                         , 143, 0  , 233, 143), // #857
  INST(Vcvtps2pd        , VexRm_Lx           , V(000F00,5A,_,x,I,0,3,HV ), 0                         , 144, 0  , 246, 143), // #858
  INST(Vcvtps2ph        , VexMri_Lx          , V(660F3A,1D,_,x,0,0,3,HVM), 0                         , 145, 0  , 247, 158), // #859
  INST(Vcvtps2phx       , VexRm_Lx_Narrow    , E(66MAP5,1D,_,_,_,0,4,FV ), 0                         , 142, 0  , 232, 144), // #860
  INST(Vcvtps2qq        , VexRm_Lx           , E(660F00,7B,_,x,_,0,3,HV ), 0                         , 146, 0  , 248, 151), // #861
  INST(Vcvtps2udq       , VexRm_Lx           , E(000F00,79,_,x,_,0,4,FV ), 0                         , 147, 0  , 249, 148), // #862
  INST(Vcvtps2uqq       , VexRm_Lx           , E(660F00,79,_,x,_,0,3,HV ), 0                         , 146, 0  , 248, 151), // #863
  INST(Vcvtqq2pd        , VexRm_Lx           , E(F30F00,E6,_,x,_,1,4,FV ), 0                         , 148, 0  , 238, 151), // #864
  INST(Vcvtqq2ph        , VexRm_Lx           , E(00MAP5,5B,_,_,_,1,4,FV ), 0                         , 149, 0  , 237, 144), // #865
  INST(Vcvtqq2ps        , VexRm_Lx_Narrow    , E(000F00,5B,_,x,_,1,4,FV ), 0                         , 135, 0  , 239, 151), // #866
  INST(Vcvtsd2sh        , VexRvm             , E(F2MAP5,5A,_,_,_,1,3,T1S), 0                         , 150, 0  , 250, 144), // #867
  INST(Vcvtsd2si        , VexRm_Wx           , V(F20F00,2D,_,I,x,x,3,T1F), 0                         , 151, 0  , 251, 143), // #868
  INST(Vcvtsd2ss        , VexRvm             , V(F20F00,5A,_,I,I,1,3,T1S), 0                         , 105, 0  , 196, 143), // #869
  INST(Vcvtsd2usi       , VexRm_Wx           , E(F20F00,79,_,I,_,x,3,T1F), 0                         , 152, 0  , 252, 148), // #870
  INST(Vcvtsh2sd        , VexRvm             , E(F3MAP5,5A,_,_,_,0,1,T1S), 0                         , 106, 0  , 253, 144), // #871
  INST(Vcvtsh2si        , VexRm_Wx           , E(F3MAP5,2D,_,_,_,x,1,T1S), 0                         , 106, 0  , 254, 144), // #872
  INST(Vcvtsh2ss        , VexRvm             , E(00MAP6,13,_,_,_,0,1,T1S), 0                         , 153, 0  , 253, 144), // #873
  INST(Vcvtsh2usi       , VexRm_Wx           , E(F3MAP5,79,_,_,_,x,1,T1S), 0                         , 106, 0  , 254, 144), // #874
  INST(Vcvtsi2sd        , VexRvm_Wx          , V(F20F00,2A,_,I,x,x,2,T1W), 0                         , 154, 0  , 255, 143), // #875
  INST(Vcvtsi2sh        , VexRvm_Wx          , E(F3MAP5,2A,_,_,_,x,2,T1W), 0                         , 155, 0  , 256, 144), // #876
  INST(Vcvtsi2ss        , VexRvm_Wx          , V(F30F00,2A,_,I,x,x,2,T1W), 0                         , 156, 0  , 255, 143), // #877
  INST(Vcvtss2sd        , VexRvm             , V(F30F00,5A,_,I,I,0,2,T1S), 0                         , 107, 0  , 257, 143), // #878
  INST(Vcvtss2sh        , VexRvm             , E(00MAP5,1D,_,_,_,0,2,T1S), 0                         , 157, 0  , 258, 144), // #879
  INST(Vcvtss2si        , VexRm_Wx           , V(F30F00,2D,_,I,x,x,2,T1F), 0                         , 107, 0  , 259, 143), // #880
  INST(Vcvtss2usi       , VexRm_Wx           , E(F30F00,79,_,I,_,x,2,T1F), 0                         , 158, 0  , 260, 148), // #881
  INST(Vcvttpd2dq       , VexRm_Lx_Narrow    , V(660F00,E6,_,x,I,1,4,FV ), 0                         , 102, 0  , 261, 143), // #882
  INST(Vcvttpd2qq       , VexRm_Lx           , E(660F00,7A,_,x,_,1,4,FV ), 0                         , 134, 0  , 262, 148), // #883
  INST(Vcvttpd2udq      , VexRm_Lx_Narrow    , E(000F00,78,_,x,_,1,4,FV ), 0                         , 135, 0  , 263, 148), // #884
  INST(Vcvttpd2uqq      , VexRm_Lx           , E(660F00,78,_,x,_,1,4,FV ), 0                         , 134, 0  , 262, 151), // #885
  INST(Vcvttph2dq       , VexRm_Lx           , E(F3MAP5,5B,_,_,_,0,3,HV ), 0                         , 159, 0  , 243, 144), // #886
  INST(Vcvttph2qq       , VexRm_Lx           , E(66MAP5,7A,_,_,_,0,2,QV ), 0                         , 140, 0  , 241, 144), // #887
  INST(Vcvttph2udq      , VexRm_Lx           , E(00MAP5,78,_,_,_,0,3,HV ), 0                         , 141, 0  , 243, 144), // #888
  INST(Vcvttph2uqq      , VexRm_Lx           , E(66MAP5,78,_,_,_,0,2,QV ), 0                         , 140, 0  , 241, 144), // #889
  INST(Vcvttph2uw       , VexRm_Lx           , E(00MAP5,7C,_,_,_,0,4,FV ), 0                         , 103, 0  , 264, 144), // #890
  INST(Vcvttph2w        , VexRm_Lx           , E(66MAP5,7C,_,_,_,0,4,FV ), 0                         , 142, 0  , 264, 144), // #891
  INST(Vcvttps2dq       , VexRm_Lx           , V(F30F00,5B,_,x,I,0,4,FV ), 0                         , 160, 0  , 265, 143), // #892
  INST(Vcvttps2qq       , VexRm_Lx           , E(660F00,7A,_,x,_,0,3,HV ), 0                         , 146, 0  , 266, 151), // #893
  INST(Vcvttps2udq      , VexRm_Lx           , E(000F00,78,_,x,_,0,4,FV ), 0                         , 147, 0  , 267, 148), // #894
  INST(Vcvttps2uqq      , VexRm_Lx           , E(660F00,78,_,x,_,0,3,HV ), 0                         , 146, 0  , 266, 151), // #895
  INST(Vcvttsd2si       , VexRm_Wx           , V(F20F00,2C,_,I,x,x,3,T1F), 0                         , 151, 0  , 268, 143), // #896
  INST(Vcvttsd2usi      , VexRm_Wx           , E(F20F00,78,_,I,_,x,3,T1F), 0                         , 152, 0  , 269, 148), // #897
  INST(Vcvttsh2si       , VexRm_Wx           , E(F3MAP5,2C,_,_,_,x,1,T1S), 0                         , 106, 0  , 270, 144), // #898
  INST(Vcvttsh2usi      , VexRm_Wx           , E(F3MAP5,78,_,_,_,x,1,T1S), 0                         , 106, 0  , 270, 144), // #899
  INST(Vcvttss2si       , VexRm_Wx           , V(F30F00,2C,_,I,x,x,2,T1F), 0                         , 107, 0  , 271, 143), // #900
  INST(Vcvttss2usi      , VexRm_Wx           , E(F30F00,78,_,I,_,x,2,T1F), 0                         , 158, 0  , 272, 148), // #901
  INST(Vcvtudq2pd       , VexRm_Lx           , E(F30F00,7A,_,x,_,0,3,HV ), 0                         , 161, 0  , 248, 148), // #902
  INST(Vcvtudq2ph       , VexRm_Lx_Narrow    , E(F2MAP5,7A,_,_,_,0,4,FV ), 0                         , 162, 0  , 232, 144), // #903
  INST(Vcvtudq2ps       , VexRm_Lx           , E(F20F00,7A,_,x,_,0,4,FV ), 0                         , 163, 0  , 249, 148), // #904
  INST(Vcvtuqq2pd       , VexRm_Lx           , E(F30F00,7A,_,x,_,1,4,FV ), 0                         , 148, 0  , 238, 151), // #905
  INST(Vcvtuqq2ph       , VexRm_Lx           , E(F2MAP5,7A,_,_,_,1,4,FV ), 0                         , 164, 0  , 237, 144), // #906
  INST(Vcvtuqq2ps       , VexRm_Lx_Narrow    , E(F20F00,7A,_,x,_,1,4,FV ), 0                         , 165, 0  , 239, 151), // #907
  INST(Vcvtusi2sd       , VexRvm_Wx          , E(F20F00,7B,_,I,_,x,2,T1W), 0                         , 166, 0  , 273, 148), // #908
  INST(Vcvtusi2sh       , VexRvm_Wx          , E(F3MAP5,7B,_,_,_,x,2,T1W), 0                         , 155, 0  , 256, 144), // #909
  INST(Vcvtusi2ss       , VexRvm_Wx          , E(F30F00,7B,_,I,_,x,2,T1W), 0                         , 167, 0  , 273, 148), // #910
  INST(Vcvtuw2ph        , VexRm_Lx           , E(F2MAP5,7D,_,_,_,0,4,FV ), 0                         , 162, 0  , 245, 144), // #911
  INST(Vcvtw2ph         , VexRm_Lx           , E(F3MAP5,7D,_,_,_,0,4,FV ), 0                         , 168, 0  , 245, 144), // #912
  INST(Vdbpsadbw        , VexRvmi_Lx         , E(660F3A,42,_,x,_,0,4,FVM), 0                         , 110, 0  , 274, 159), // #913
  INST(Vdivpd           , VexRvm_Lx          , V(660F00,5E,_,x,I,1,4,FV ), 0                         , 102, 0  , 193, 143), // #914
  INST(Vdivph           , VexRvm_Lx          , E(00MAP5,5E,_,_,_,0,4,FV ), 0                         , 103, 0  , 194, 144), // #915
  INST(Vdivps           , VexRvm_Lx          , V(000F00,5E,_,x,I,0,4,FV ), 0                         , 104, 0  , 195, 143), // #916
  INST(Vdivsd           , VexRvm             , V(F20F00,5E,_,I,I,1,3,T1S), 0                         , 105, 0  , 196, 143), // #917
  INST(Vdivsh           , VexRvm             , E(F3MAP5,5E,_,_,_,0,1,T1S), 0                         , 106, 0  , 197, 144), // #918
  INST(Vdivss           , VexRvm             , V(F30F00,5E,_,I,I,0,2,T1S), 0                         , 107, 0  , 198, 143), // #919
  INST(Vdpbf16ps        , VexRvm_Lx          , E(F30F38,52,_,_,_,0,4,FV ), 0                         , 169, 0  , 211, 156), // #920
  INST(Vdppd            , VexRvmi_Lx         , V(660F3A,41,_,x,I,_,_,_  ), 0                         , 75 , 0  , 275, 145), // #921
  INST(Vdpps            , VexRvmi_Lx         , V(660F3A,40,_,x,I,_,_,_  ), 0                         , 75 , 0  , 212, 145), // #922
  INST(Verr             , X86M_NoSize        , O(000F00,00,4,_,_,_,_,_  ), 0                         , 98 , 0  , 105, 11 ), // #923
  INST(Verw             , X86M_NoSize        , O(000F00,00,5,_,_,_,_,_  ), 0                         , 79 , 0  , 105, 11 ), // #924
  INST(Vexpandpd        , VexRm_Lx           , E(660F38,88,_,x,_,1,3,T1S), 0                         , 127, 0  , 276, 148), // #925
  INST(Vexpandps        , VexRm_Lx           , E(660F38,88,_,x,_,0,2,T1S), 0                         , 128, 0  , 276, 148), // #926
  INST(Vextractf128     , VexMri             , V(660F3A,19,_,1,0,_,_,_  ), 0                         , 170, 0  , 277, 145), // #927
  INST(Vextractf32x4    , VexMri_Lx          , E(660F3A,19,_,x,_,0,4,T4 ), 0                         , 171, 0  , 278, 148), // #928
  INST(Vextractf32x8    , VexMri             , E(660F3A,1B,_,2,_,0,5,T8 ), 0                         , 172, 0  , 279, 151), // #929
  INST(Vextractf64x2    , VexMri_Lx          , E(660F3A,19,_,x,_,1,4,T2 ), 0                         , 173, 0  , 278, 151), // #930
  INST(Vextractf64x4    , VexMri             , E(660F3A,1B,_,2,_,1,5,T4 ), 0                         , 174, 0  , 279, 148), // #931
  INST(Vextracti128     , VexMri             , V(660F3A,39,_,1,0,_,_,_  ), 0                         , 170, 0  , 277, 152), // #932
  INST(Vextracti32x4    , VexMri_Lx          , E(660F3A,39,_,x,_,0,4,T4 ), 0                         , 171, 0  , 278, 148), // #933
  INST(Vextracti32x8    , VexMri             , E(660F3A,3B,_,2,_,0,5,T8 ), 0                         , 172, 0  , 279, 151), // #934
  INST(Vextracti64x2    , VexMri_Lx          , E(660F3A,39,_,x,_,1,4,T2 ), 0                         , 173, 0  , 278, 151), // #935
  INST(Vextracti64x4    , VexMri             , E(660F3A,3B,_,2,_,1,5,T4 ), 0                         , 174, 0  , 279, 148), // #936
  INST(Vextractps       , VexMri             , V(660F3A,17,_,0,I,I,2,T1S), 0                         , 175, 0  , 280, 143), // #937
  INST(Vfcmaddcph       , VexRvm_Lx          , E(F2MAP6,56,_,_,_,0,4,FV ), 0                         , 176, 0  , 281, 144), // #938
  INST(Vfcmaddcsh       , VexRvm             , E(F2MAP6,57,_,_,_,0,2,T1S), 0                         , 177, 0  , 258, 144), // #939
  INST(Vfcmulcph        , VexRvm_Lx          , E(F2MAP6,D6,_,_,_,0,4,FV ), 0                         , 176, 0  , 281, 144), // #940
  INST(Vfcmulcsh        , VexRvm             , E(F2MAP6,D7,_,_,_,0,2,T1S), 0                         , 177, 0  , 258, 144), // #941
  INST(Vfixupimmpd      , VexRvmi_Lx         , E(660F3A,54,_,x,_,1,4,FV ), 0                         , 111, 0  , 282, 148), // #942
  INST(Vfixupimmps      , VexRvmi_Lx         , E(660F3A,54,_,x,_,0,4,FV ), 0                         , 110, 0  , 283, 148), // #943
  INST(Vfixupimmsd      , VexRvmi            , E(660F3A,55,_,I,_,1,3,T1S), 0                         , 178, 0  , 284, 148), // #944
  INST(Vfixupimmss      , VexRvmi            , E(660F3A,55,_,I,_,0,2,T1S), 0                         , 179, 0  , 285, 148), // #945
  INST(Vfmadd132pd      , VexRvm_Lx          , V(660F38,98,_,x,1,1,4,FV ), 0                         , 180, 0  , 193, 160), // #946
  INST(Vfmadd132ph      , VexRvm_Lx          , E(66MAP6,98,_,_,_,0,4,FV ), 0                         , 181, 0  , 194, 144), // #947
  INST(Vfmadd132ps      , VexRvm_Lx          , V(660F38,98,_,x,0,0,4,FV ), 0                         , 109, 0  , 195, 160), // #948
  INST(Vfmadd132sd      , VexRvm             , V(660F38,99,_,I,1,1,3,T1S), 0                         , 182, 0  , 196, 160), // #949
  INST(Vfmadd132sh      , VexRvm             , E(66MAP6,99,_,_,_,0,1,T1S), 0                         , 183, 0  , 197, 144), // #950
  INST(Vfmadd132ss      , VexRvm             , V(660F38,99,_,I,0,0,2,T1S), 0                         , 121, 0  , 198, 160), // #951
  INST(Vfmadd213pd      , VexRvm_Lx          , V(660F38,A8,_,x,1,1,4,FV ), 0                         , 180, 0  , 193, 160), // #952
  INST(Vfmadd213ph      , VexRvm_Lx          , E(66MAP6,A8,_,_,_,0,4,FV ), 0                         , 181, 0  , 194, 144), // #953
  INST(Vfmadd213ps      , VexRvm_Lx          , V(660F38,A8,_,x,0,0,4,FV ), 0                         , 109, 0  , 195, 160), // #954
  INST(Vfmadd213sd      , VexRvm             , V(660F38,A9,_,I,1,1,3,T1S), 0                         , 182, 0  , 196, 160), // #955
  INST(Vfmadd213sh      , VexRvm             , E(66MAP6,A9,_,_,_,0,1,T1S), 0                         , 183, 0  , 197, 144), // #956
  INST(Vfmadd213ss      , VexRvm             , V(660F38,A9,_,I,0,0,2,T1S), 0                         , 121, 0  , 198, 160), // #957
  INST(Vfmadd231pd      , VexRvm_Lx          , V(660F38,B8,_,x,1,1,4,FV ), 0                         , 180, 0  , 193, 160), // #958
  INST(Vfmadd231ph      , VexRvm_Lx          , E(66MAP6,B8,_,_,_,0,4,FV ), 0                         , 181, 0  , 194, 144), // #959
  INST(Vfmadd231ps      , VexRvm_Lx          , V(660F38,B8,_,x,0,0,4,FV ), 0                         , 109, 0  , 195, 160), // #960
  INST(Vfmadd231sd      , VexRvm             , V(660F38,B9,_,I,1,1,3,T1S), 0                         , 182, 0  , 196, 160), // #961
  INST(Vfmadd231sh      , VexRvm             , E(66MAP6,B9,_,_,_,0,1,T1S), 0                         , 183, 0  , 197, 144), // #962
  INST(Vfmadd231ss      , VexRvm             , V(660F38,B9,_,I,0,0,2,T1S), 0                         , 121, 0  , 198, 160), // #963
  INST(Vfmaddcph        , VexRvm_Lx          , E(F3MAP6,56,_,_,_,0,4,FV ), 0                         , 184, 0  , 281, 144), // #964
  INST(Vfmaddcsh        , VexRvm             , E(F3MAP6,57,_,_,_,0,2,T1S), 0                         , 185, 0  , 258, 144), // #965
  INST(Vfmaddpd         , Fma4_Lx            , V(660F3A,69,_,x,x,_,_,_  ), 0                         , 75 , 0  , 286, 161), // #966
  INST(Vfmaddps         , Fma4_Lx            , V(660F3A,68,_,x,x,_,_,_  ), 0                         , 75 , 0  , 286, 161), // #967
  INST(Vfmaddsd         , Fma4               , V(660F3A,6B,_,0,x,_,_,_  ), 0                         , 75 , 0  , 287, 161), // #968
  INST(Vfmaddss         , Fma4               , V(660F3A,6A,_,0,x,_,_,_  ), 0                         , 75 , 0  , 288, 161), // #969
  INST(Vfmaddsub132pd   , VexRvm_Lx          , V(660F38,96,_,x,1,1,4,FV ), 0                         , 180, 0  , 193, 160), // #970
  INST(Vfmaddsub132ph   , VexRvm_Lx          , E(66MAP6,96,_,_,_,0,4,FV ), 0                         , 181, 0  , 194, 144), // #971
  INST(Vfmaddsub132ps   , VexRvm_Lx          , V(660F38,96,_,x,0,0,4,FV ), 0                         , 109, 0  , 195, 160), // #972
  INST(Vfmaddsub213pd   , VexRvm_Lx          , V(660F38,A6,_,x,1,1,4,FV ), 0                         , 180, 0  , 193, 160), // #973
  INST(Vfmaddsub213ph   , VexRvm_Lx          , E(66MAP6,A6,_,_,_,0,4,FV ), 0                         , 181, 0  , 194, 144), // #974
  INST(Vfmaddsub213ps   , VexRvm_Lx          , V(660F38,A6,_,x,0,0,4,FV ), 0                         , 109, 0  , 195, 160), // #975
  INST(Vfmaddsub231pd   , VexRvm_Lx          , V(660F38,B6,_,x,1,1,4,FV ), 0                         , 180, 0  , 193, 160), // #976
  INST(Vfmaddsub231ph   , VexRvm_Lx          , E(66MAP6,B6,_,_,_,0,4,FV ), 0                         , 181, 0  , 194, 144), // #977
  INST(Vfmaddsub231ps   , VexRvm_Lx          , V(660F38,B6,_,x,0,0,4,FV ), 0                         , 109, 0  , 195, 160), // #978
  INST(Vfmaddsubpd      , Fma4_Lx            , V(660F3A,5D,_,x,x,_,_,_  ), 0                         , 75 , 0  , 286, 161), // #979
  INST(Vfmaddsubps      , Fma4_Lx            , V(660F3A,5C,_,x,x,_,_,_  ), 0                         , 75 , 0  , 286, 161), // #980
  INST(Vfmsub132pd      , VexRvm_Lx          , V(660F38,9A,_,x,1,1,4,FV ), 0                         , 180, 0  , 193, 160), // #981
  INST(Vfmsub132ph      , VexRvm_Lx          , E(66MAP6,9A,_,_,_,0,4,FV ), 0                         , 181, 0  , 194, 144), // #982
  INST(Vfmsub132ps      , VexRvm_Lx          , V(660F38,9A,_,x,0,0,4,FV ), 0                         , 109, 0  , 195, 160), // #983
  INST(Vfmsub132sd      , VexRvm             , V(660F38,9B,_,I,1,1,3,T1S), 0                         , 182, 0  , 196, 160), // #984
  INST(Vfmsub132sh      , VexRvm             , E(66MAP6,9B,_,_,_,0,1,T1S), 0                         , 183, 0  , 197, 144), // #985
  INST(Vfmsub132ss      , VexRvm             , V(660F38,9B,_,I,0,0,2,T1S), 0                         , 121, 0  , 198, 160), // #986
  INST(Vfmsub213pd      , VexRvm_Lx          , V(660F38,AA,_,x,1,1,4,FV ), 0                         , 180, 0  , 193, 160), // #987
  INST(Vfmsub213ph      , VexRvm_Lx          , E(66MAP6,AA,_,_,_,0,4,FV ), 0                         , 181, 0  , 194, 144), // #988
  INST(Vfmsub213ps      , VexRvm_Lx          , V(660F38,AA,_,x,0,0,4,FV ), 0                         , 109, 0  , 195, 160), // #989
  INST(Vfmsub213sd      , VexRvm             , V(660F38,AB,_,I,1,1,3,T1S), 0                         , 182, 0  , 196, 160), // #990
  INST(Vfmsub213sh      , VexRvm             , E(66MAP6,AB,_,_,_,0,1,T1S), 0                         , 183, 0  , 197, 144), // #991
  INST(Vfmsub213ss      , VexRvm             , V(660F38,AB,_,I,0,0,2,T1S), 0                         , 121, 0  , 198, 160), // #992
  INST(Vfmsub231pd      , VexRvm_Lx          , V(660F38,BA,_,x,1,1,4,FV ), 0                         , 180, 0  , 193, 160), // #993
  INST(Vfmsub231ph      , VexRvm_Lx          , E(66MAP6,BA,_,_,_,0,4,FV ), 0                         , 181, 0  , 194, 144), // #994
  INST(Vfmsub231ps      , VexRvm_Lx          , V(660F38,BA,_,x,0,0,4,FV ), 0                         , 109, 0  , 195, 160), // #995
  INST(Vfmsub231sd      , VexRvm             , V(660F38,BB,_,I,1,1,3,T1S), 0                         , 182, 0  , 196, 160), // #996
  INST(Vfmsub231sh      , VexRvm             , E(66MAP6,BB,_,_,_,0,1,T1S), 0                         , 183, 0  , 197, 144), // #997
  INST(Vfmsub231ss      , VexRvm             , V(660F38,BB,_,I,0,0,2,T1S), 0                         , 121, 0  , 198, 160), // #998
  INST(Vfmsubadd132pd   , VexRvm_Lx          , V(660F38,97,_,x,1,1,4,FV ), 0                         , 180, 0  , 193, 160), // #999
  INST(Vfmsubadd132ph   , VexRvm_Lx          , E(66MAP6,97,_,_,_,0,4,FV ), 0                         , 181, 0  , 194, 144), // #1000
  INST(Vfmsubadd132ps   , VexRvm_Lx          , V(660F38,97,_,x,0,0,4,FV ), 0                         , 109, 0  , 195, 160), // #1001
  INST(Vfmsubadd213pd   , VexRvm_Lx          , V(660F38,A7,_,x,1,1,4,FV ), 0                         , 180, 0  , 193, 160), // #1002
  INST(Vfmsubadd213ph   , VexRvm_Lx          , E(66MAP6,A7,_,_,_,0,4,FV ), 0                         , 181, 0  , 194, 144), // #1003
  INST(Vfmsubadd213ps   , VexRvm_Lx          , V(660F38,A7,_,x,0,0,4,FV ), 0                         , 109, 0  , 195, 160), // #1004
  INST(Vfmsubadd231pd   , VexRvm_Lx          , V(660F38,B7,_,x,1,1,4,FV ), 0                         , 180, 0  , 193, 160), // #1005
  INST(Vfmsubadd231ph   , VexRvm_Lx          , E(66MAP6,B7,_,_,_,0,4,FV ), 0                         , 181, 0  , 194, 144), // #1006
  INST(Vfmsubadd231ps   , VexRvm_Lx          , V(660F38,B7,_,x,0,0,4,FV ), 0                         , 109, 0  , 195, 160), // #1007
  INST(Vfmsubaddpd      , Fma4_Lx            , V(660F3A,5F,_,x,x,_,_,_  ), 0                         , 75 , 0  , 286, 161), // #1008
  INST(Vfmsubaddps      , Fma4_Lx            , V(660F3A,5E,_,x,x,_,_,_  ), 0                         , 75 , 0  , 286, 161), // #1009
  INST(Vfmsubpd         , Fma4_Lx            , V(660F3A,6D,_,x,x,_,_,_  ), 0                         , 75 , 0  , 286, 161), // #1010
  INST(Vfmsubps         , Fma4_Lx            , V(660F3A,6C,_,x,x,_,_,_  ), 0                         , 75 , 0  , 286, 161), // #1011
  INST(Vfmsubsd         , Fma4               , V(660F3A,6F,_,0,x,_,_,_  ), 0                         , 75 , 0  , 287, 161), // #1012
  INST(Vfmsubss         , Fma4               , V(660F3A,6E,_,0,x,_,_,_  ), 0                         , 75 , 0  , 288, 161), // #1013
  INST(Vfmulcph         , VexRvm_Lx          , E(F3MAP6,D6,_,_,_,0,4,FV ), 0                         , 184, 0  , 281, 144), // #1014
  INST(Vfmulcsh         , VexRvm             , E(F3MAP6,D7,_,_,_,0,2,T1S), 0                         , 185, 0  , 258, 144), // #1015
  INST(Vfnmadd132pd     , VexRvm_Lx          , V(660F38,9C,_,x,1,1,4,FV ), 0                         , 180, 0  , 193, 160), // #1016
  INST(Vfnmadd132ph     , VexRvm_Lx          , E(66MAP6,9C,_,_,_,0,4,FV ), 0                         , 181, 0  , 194, 144), // #1017
  INST(Vfnmadd132ps     , VexRvm_Lx          , V(660F38,9C,_,x,0,0,4,FV ), 0                         , 109, 0  , 195, 160), // #1018
  INST(Vfnmadd132sd     , VexRvm             , V(660F38,9D,_,I,1,1,3,T1S), 0                         , 182, 0  , 196, 160), // #1019
  INST(Vfnmadd132sh     , VexRvm             , E(66MAP6,9D,_,_,_,0,1,T1S), 0                         , 183, 0  , 197, 144), // #1020
  INST(Vfnmadd132ss     , VexRvm             , V(660F38,9D,_,I,0,0,2,T1S), 0                         , 121, 0  , 198, 160), // #1021
  INST(Vfnmadd213pd     , VexRvm_Lx          , V(660F38,AC,_,x,1,1,4,FV ), 0                         , 180, 0  , 193, 160), // #1022
  INST(Vfnmadd213ph     , VexRvm_Lx          , E(66MAP6,AC,_,_,_,0,4,FV ), 0                         , 181, 0  , 194, 144), // #1023
  INST(Vfnmadd213ps     , VexRvm_Lx          , V(660F38,AC,_,x,0,0,4,FV ), 0                         , 109, 0  , 195, 160), // #1024
  INST(Vfnmadd213sd     , VexRvm             , V(660F38,AD,_,I,1,1,3,T1S), 0                         , 182, 0  , 196, 160), // #1025
  INST(Vfnmadd213sh     , VexRvm             , E(66MAP6,AD,_,_,_,0,1,T1S), 0                         , 183, 0  , 197, 144), // #1026
  INST(Vfnmadd213ss     , VexRvm             , V(660F38,AD,_,I,0,0,2,T1S), 0                         , 121, 0  , 198, 160), // #1027
  INST(Vfnmadd231pd     , VexRvm_Lx          , V(660F38,BC,_,x,1,1,4,FV ), 0                         , 180, 0  , 193, 160), // #1028
  INST(Vfnmadd231ph     , VexRvm_Lx          , E(66MAP6,BC,_,_,_,0,4,FV ), 0                         , 181, 0  , 194, 144), // #1029
  INST(Vfnmadd231ps     , VexRvm_Lx          , V(660F38,BC,_,x,0,0,4,FV ), 0                         , 109, 0  , 195, 160), // #1030
  INST(Vfnmadd231sd     , VexRvm             , V(660F38,BD,_,I,1,1,3,T1S), 0                         , 182, 0  , 196, 160), // #1031
  INST(Vfnmadd231sh     , VexRvm             , E(66MAP6,BD,_,_,_,0,1,T1S), 0                         , 183, 0  , 197, 144), // #1032
  INST(Vfnmadd231ss     , VexRvm             , V(660F38,BD,_,I,0,0,2,T1S), 0                         , 121, 0  , 198, 160), // #1033
  INST(Vfnmaddpd        , Fma4_Lx            , V(660F3A,79,_,x,x,_,_,_  ), 0                         , 75 , 0  , 286, 161), // #1034
  INST(Vfnmaddps        , Fma4_Lx            , V(660F3A,78,_,x,x,_,_,_  ), 0                         , 75 , 0  , 286, 161), // #1035
  INST(Vfnmaddsd        , Fma4               , V(660F3A,7B,_,0,x,_,_,_  ), 0                         , 75 , 0  , 287, 161), // #1036
  INST(Vfnmaddss        , Fma4               , V(660F3A,7A,_,0,x,_,_,_  ), 0                         , 75 , 0  , 288, 161), // #1037
  INST(Vfnmsub132pd     , VexRvm_Lx          , V(660F38,9E,_,x,1,1,4,FV ), 0                         , 180, 0  , 193, 160), // #1038
  INST(Vfnmsub132ph     , VexRvm_Lx          , E(66MAP6,9E,_,_,_,0,4,FV ), 0                         , 181, 0  , 194, 144), // #1039
  INST(Vfnmsub132ps     , VexRvm_Lx          , V(660F38,9E,_,x,0,0,4,FV ), 0                         , 109, 0  , 195, 160), // #1040
  INST(Vfnmsub132sd     , VexRvm             , V(660F38,9F,_,I,1,1,3,T1S), 0                         , 182, 0  , 196, 160), // #1041
  INST(Vfnmsub132sh     , VexRvm             , E(66MAP6,9F,_,_,_,0,1,T1S), 0                         , 183, 0  , 197, 144), // #1042
  INST(Vfnmsub132ss     , VexRvm             , V(660F38,9F,_,I,0,0,2,T1S), 0                         , 121, 0  , 198, 160), // #1043
  INST(Vfnmsub213pd     , VexRvm_Lx          , V(660F38,AE,_,x,1,1,4,FV ), 0                         , 180, 0  , 193, 160), // #1044
  INST(Vfnmsub213ph     , VexRvm_Lx          , E(66MAP6,AE,_,_,_,0,4,FV ), 0                         , 181, 0  , 194, 144), // #1045
  INST(Vfnmsub213ps     , VexRvm_Lx          , V(660F38,AE,_,x,0,0,4,FV ), 0                         , 109, 0  , 195, 160), // #1046
  INST(Vfnmsub213sd     , VexRvm             , V(660F38,AF,_,I,1,1,3,T1S), 0                         , 182, 0  , 196, 160), // #1047
  INST(Vfnmsub213sh     , VexRvm             , E(66MAP6,AF,_,_,_,0,1,T1S), 0                         , 183, 0  , 197, 144), // #1048
  INST(Vfnmsub213ss     , VexRvm             , V(660F38,AF,_,I,0,0,2,T1S), 0                         , 121, 0  , 198, 160), // #1049
  INST(Vfnmsub231pd     , VexRvm_Lx          , V(660F38,BE,_,x,1,1,4,FV ), 0                         , 180, 0  , 193, 160), // #1050
  INST(Vfnmsub231ph     , VexRvm_Lx          , E(66MAP6,BE,_,_,_,0,4,FV ), 0                         , 181, 0  , 194, 144), // #1051
  INST(Vfnmsub231ps     , VexRvm_Lx          , V(660F38,BE,_,x,0,0,4,FV ), 0                         , 109, 0  , 195, 160), // #1052
  INST(Vfnmsub231sd     , VexRvm             , V(660F38,BF,_,I,1,1,3,T1S), 0                         , 182, 0  , 196, 160), // #1053
  INST(Vfnmsub231sh     , VexRvm             , E(66MAP6,BF,_,_,_,0,1,T1S), 0                         , 183, 0  , 197, 144), // #1054
  INST(Vfnmsub231ss     , VexRvm             , V(660F38,BF,_,I,0,0,2,T1S), 0                         , 121, 0  , 198, 160), // #1055
  INST(Vfnmsubpd        , Fma4_Lx            , V(660F3A,7D,_,x,x,_,_,_  ), 0                         , 75 , 0  , 286, 161), // #1056
  INST(Vfnmsubps        , Fma4_Lx            , V(660F3A,7C,_,x,x,_,_,_  ), 0                         , 75 , 0  , 286, 161), // #1057
  INST(Vfnmsubsd        , Fma4               , V(660F3A,7F,_,0,x,_,_,_  ), 0                         , 75 , 0  , 287, 161), // #1058
  INST(Vfnmsubss        , Fma4               , V(660F3A,7E,_,0,x,_,_,_  ), 0                         , 75 , 0  , 288, 161), // #1059
  INST(Vfpclasspd       , VexRmi_Lx          , E(660F3A,66,_,x,_,1,4,FV ), 0                         , 111, 0  , 289, 151), // #1060
  INST(Vfpclassph       , VexRmi_Lx          , E(000F3A,66,_,_,_,0,4,FV ), 0                         , 122, 0  , 290, 144), // #1061
  INST(Vfpclassps       , VexRmi_Lx          , E(660F3A,66,_,x,_,0,4,FV ), 0                         , 110, 0  , 291, 151), // #1062
  INST(Vfpclasssd       , VexRmi             , E(660F3A,67,_,I,_,1,3,T1S), 0                         , 178, 0  , 292, 151), // #1063
  INST(Vfpclasssh       , VexRmi             , E(000F3A,67,_,_,_,0,1,T1S), 0                         , 186, 0  , 293, 144), // #1064
  INST(Vfpclassss       , VexRmi             , E(660F3A,67,_,I,_,0,2,T1S), 0                         , 179, 0  , 294, 151), // #1065
  INST(Vfrczpd          , VexRm_Lx           , V(XOP_M9,81,_,x,0,_,_,_  ), 0                         , 81 , 0  , 295, 162), // #1066
  INST(Vfrczps          , VexRm_Lx           , V(XOP_M9,80,_,x,0,_,_,_  ), 0                         , 81 , 0  , 295, 162), // #1067
  INST(Vfrczsd          , VexRm              , V(XOP_M9,83,_,0,0,_,_,_  ), 0                         , 81 , 0  , 296, 162), // #1068
  INST(Vfrczss          , VexRm              , V(XOP_M9,82,_,0,0,_,_,_  ), 0                         , 81 , 0  , 297, 162), // #1069
  INST(Vgatherdpd       , VexRmvRm_VM        , V(660F38,92,_,x,1,_,_,_  ), E(660F38,92,_,x,_,1,3,T1S), 187, 79 , 298, 163), // #1070
  INST(Vgatherdps       , VexRmvRm_VM        , V(660F38,92,_,x,0,_,_,_  ), E(660F38,92,_,x,_,0,2,T1S), 30 , 80 , 299, 163), // #1071
  INST(Vgatherqpd       , VexRmvRm_VM        , V(660F38,93,_,x,1,_,_,_  ), E(660F38,93,_,x,_,1,3,T1S), 187, 81 , 300, 163), // #1072
  INST(Vgatherqps       , VexRmvRm_VM        , V(660F38,93,_,x,0,_,_,_  ), E(660F38,93,_,x,_,0,2,T1S), 30 , 82 , 301, 163), // #1073
  INST(Vgetexppd        , VexRm_Lx           , E(660F38,42,_,x,_,1,4,FV ), 0                         , 112, 0  , 262, 148), // #1074
  INST(Vgetexpph        , VexRm_Lx           , E(66MAP6,42,_,_,_,0,4,FV ), 0                         , 181, 0  , 264, 144), // #1075
  INST(Vgetexpps        , VexRm_Lx           , E(660F38,42,_,x,_,0,4,FV ), 0                         , 113, 0  , 267, 148), // #1076
  INST(Vgetexpsd        , VexRvm             , E(660F38,43,_,I,_,1,3,T1S), 0                         , 127, 0  , 302, 148), // #1077
  INST(Vgetexpsh        , VexRvm             , E(66MAP6,43,_,_,_,0,1,T1S), 0                         , 183, 0  , 253, 144), // #1078
  INST(Vgetexpss        , VexRvm             , E(660F38,43,_,I,_,0,2,T1S), 0                         , 128, 0  , 303, 148), // #1079
  INST(Vgetmantpd       , VexRmi_Lx          , E(660F3A,26,_,x,_,1,4,FV ), 0                         , 111, 0  , 304, 148), // #1080
  INST(Vgetmantph       , VexRmi_Lx          , E(000F3A,26,_,_,_,0,4,FV ), 0                         , 122, 0  , 305, 144), // #1081
  INST(Vgetmantps       , VexRmi_Lx          , E(660F3A,26,_,x,_,0,4,FV ), 0                         , 110, 0  , 306, 148), // #1082
  INST(Vgetmantsd       , VexRvmi            , E(660F3A,27,_,I,_,1,3,T1S), 0                         , 178, 0  , 284, 148), // #1083
  INST(Vgetmantsh       , VexRvmi            , E(000F3A,27,_,_,_,0,1,T1S), 0                         , 186, 0  , 307, 144), // #1084
  INST(Vgetmantss       , VexRvmi            , E(660F3A,27,_,I,_,0,2,T1S), 0                         , 179, 0  , 285, 148), // #1085
  INST(Vgf2p8affineinvqb, VexRvmi_Lx         , V(660F3A,CF,_,x,1,1,4,FV ), 0                         , 188, 0  , 308, 164), // #1086
  INST(Vgf2p8affineqb   , VexRvmi_Lx         , V(660F3A,CE,_,x,1,1,4,FV ), 0                         , 188, 0  , 308, 164), // #1087
  INST(Vgf2p8mulb       , VexRvm_Lx          , V(660F38,CF,_,x,0,0,4,FV ), 0                         , 109, 0  , 309, 164), // #1088
  INST(Vhaddpd          , VexRvm_Lx          , V(660F00,7C,_,x,I,_,_,_  ), 0                         , 71 , 0  , 199, 145), // #1089
  INST(Vhaddps          , VexRvm_Lx          , V(F20F00,7C,_,x,I,_,_,_  ), 0                         , 108, 0  , 199, 145), // #1090
  INST(Vhsubpd          , VexRvm_Lx          , V(660F00,7D,_,x,I,_,_,_  ), 0                         , 71 , 0  , 199, 145), // #1091
  INST(Vhsubps          , VexRvm_Lx          , V(F20F00,7D,_,x,I,_,_,_  ), 0                         , 108, 0  , 199, 145), // #1092
  INST(Vinsertf128      , VexRvmi            , V(660F3A,18,_,1,0,_,_,_  ), 0                         , 170, 0  , 310, 145), // #1093
  INST(Vinsertf32x4     , VexRvmi_Lx         , E(660F3A,18,_,x,_,0,4,T4 ), 0                         , 171, 0  , 311, 148), // #1094
  INST(Vinsertf32x8     , VexRvmi            , E(660F3A,1A,_,2,_,0,5,T8 ), 0                         , 172, 0  , 312, 151), // #1095
  INST(Vinsertf64x2     , VexRvmi_Lx         , E(660F3A,18,_,x,_,1,4,T2 ), 0                         , 173, 0  , 311, 151), // #1096
  INST(Vinsertf64x4     , VexRvmi            , E(660F3A,1A,_,2,_,1,5,T4 ), 0                         , 174, 0  , 312, 148), // #1097
  INST(Vinserti128      , VexRvmi            , V(660F3A,38,_,1,0,_,_,_  ), 0                         , 170, 0  , 310, 152), // #1098
  INST(Vinserti32x4     , VexRvmi_Lx         , E(660F3A,38,_,x,_,0,4,T4 ), 0                         , 171, 0  , 311, 148), // #1099
  INST(Vinserti32x8     , VexRvmi            , E(660F3A,3A,_,2,_,0,5,T8 ), 0                         , 172, 0  , 312, 151), // #1100
  INST(Vinserti64x2     , VexRvmi_Lx         , E(660F3A,38,_,x,_,1,4,T2 ), 0                         , 173, 0  , 311, 151), // #1101
  INST(Vinserti64x4     , VexRvmi            , E(660F3A,3A,_,2,_,1,5,T4 ), 0                         , 174, 0  , 312, 148), // #1102
  INST(Vinsertps        , VexRvmi            , V(660F3A,21,_,0,I,0,2,T1S), 0                         , 175, 0  , 313, 143), // #1103
  INST(Vlddqu           , VexRm_Lx           , V(F20F00,F0,_,x,I,_,_,_  ), 0                         , 108, 0  , 234, 145), // #1104
  INST(Vldmxcsr         , VexM               , V(000F00,AE,2,0,I,_,_,_  ), 0                         , 189, 0  , 314, 145), // #1105
  INST(Vmaskmovdqu      , VexRm_ZDI          , V(660F00,F7,_,0,I,_,_,_  ), 0                         , 71 , 0  , 315, 145), // #1106
  INST(Vmaskmovpd       , VexRvmMvr_Lx       , V(660F38,2D,_,x,0,_,_,_  ), V(660F38,2F,_,x,0,_,_,_  ), 30 , 83 , 316, 145), // #1107
  INST(Vmaskmovps       , VexRvmMvr_Lx       , V(660F38,2C,_,x,0,_,_,_  ), V(660F38,2E,_,x,0,_,_,_  ), 30 , 84 , 316, 145), // #1108
  INST(Vmaxpd           , VexRvm_Lx          , V(660F00,5F,_,x,I,1,4,FV ), 0                         , 102, 0  , 317, 143), // #1109
  INST(Vmaxph           , VexRvm_Lx          , E(00MAP5,5F,_,_,_,0,4,FV ), 0                         , 103, 0  , 318, 144), // #1110
  INST(Vmaxps           , VexRvm_Lx          , V(000F00,5F,_,x,I,0,4,FV ), 0                         , 104, 0  , 319, 143), // #1111
  INST(Vmaxsd           , VexRvm             , V(F20F00,5F,_,I,I,1,3,T1S), 0                         , 105, 0  , 320, 143), // #1112
  INST(Vmaxsh           , VexRvm             , E(F3MAP5,5F,_,_,_,0,1,T1S), 0                         , 106, 0  , 253, 144), // #1113
  INST(Vmaxss           , VexRvm             , V(F30F00,5F,_,I,I,0,2,T1S), 0                         , 107, 0  , 257, 143), // #1114
  INST(Vmcall           , X86Op              , O(000F01,C1,_,_,_,_,_,_  ), 0                         , 23 , 0  , 26 , 66 ), // #1115
  INST(Vmclear          , X86M_Only          , O(660F00,C7,6,_,_,_,_,_  ), 0                         , 28 , 0  , 28 , 66 ), // #1116
  INST(Vmfunc           , X86Op              , O(000F01,D4,_,_,_,_,_,_  ), 0                         , 23 , 0  , 26 , 66 ), // #1117
  INST(Vmgexit          , X86Op              , O(F20F01,D9,_,_,_,_,_,_  ), 0                         , 93 , 0  , 26 , 165), // #1118
  INST(Vminpd           , VexRvm_Lx          , V(660F00,5D,_,x,I,1,4,FV ), 0                         , 102, 0  , 317, 143), // #1119
  INST(Vminph           , VexRvm_Lx          , E(00MAP5,5D,_,_,_,0,4,FV ), 0                         , 103, 0  , 318, 144), // #1120
  INST(Vminps           , VexRvm_Lx          , V(000F00,5D,_,x,I,0,4,FV ), 0                         , 104, 0  , 319, 143), // #1121
  INST(Vminsd           , VexRvm             , V(F20F00,5D,_,I,I,1,3,T1S), 0                         , 105, 0  , 320, 143), // #1122
  INST(Vminsh           , VexRvm             , E(F3MAP5,5D,_,_,_,0,1,T1S), 0                         , 106, 0  , 253, 144), // #1123
  INST(Vminss           , VexRvm             , V(F30F00,5D,_,I,I,0,2,T1S), 0                         , 107, 0  , 257, 143), // #1124
  INST(Vmlaunch         , X86Op              , O(000F01,C2,_,_,_,_,_,_  ), 0                         , 23 , 0  , 26 , 66 ), // #1125
  INST(Vmload           , X86Op_xAX          , O(000F01,DA,_,_,_,_,_,_  ), 0                         , 23 , 0  , 321, 22 ), // #1126
  INST(Vmmcall          , X86Op              , O(000F01,D9,_,_,_,_,_,_  ), 0                         , 23 , 0  , 26 , 22 ), // #1127
  INST(Vmovapd          , VexRmMr_Lx         , V(660F00,28,_,x,I,1,4,FVM), V(660F00,29,_,x,I,1,4,FVM), 102, 85 , 322, 166), // #1128
  INST(Vmovaps          , VexRmMr_Lx         , V(000F00,28,_,x,I,0,4,FVM), V(000F00,29,_,x,I,0,4,FVM), 104, 86 , 322, 166), // #1129
  INST(Vmovd            , VexMovdMovq        , V(660F00,6E,_,0,0,0,2,T1S), V(660F00,7E,_,0,0,0,2,T1S), 190, 87 , 323, 143), // #1130
  INST(Vmovddup         , VexRm_Lx           , V(F20F00,12,_,x,I,1,3,DUP), 0                         , 191, 0  , 324, 143), // #1131
  INST(Vmovdqa          , VexRmMr_Lx         , V(660F00,6F,_,x,I,_,_,_  ), V(660F00,7F,_,x,I,_,_,_  ), 71 , 88 , 325, 167), // #1132
  INST(Vmovdqa32        , VexRmMr_Lx         , E(660F00,6F,_,x,_,0,4,FVM), E(660F00,7F,_,x,_,0,4,FVM), 192, 89 , 326, 168), // #1133
  INST(Vmovdqa64        , VexRmMr_Lx         , E(660F00,6F,_,x,_,1,4,FVM), E(660F00,7F,_,x,_,1,4,FVM), 134, 90 , 326, 168), // #1134
  INST(Vmovdqu          , VexRmMr_Lx         , V(F30F00,6F,_,x,I,_,_,_  ), V(F30F00,7F,_,x,I,_,_,_  ), 193, 91 , 325, 167), // #1135
  INST(Vmovdqu16        , VexRmMr_Lx         , E(F20F00,6F,_,x,_,1,4,FVM), E(F20F00,7F,_,x,_,1,4,FVM), 165, 92 , 326, 169), // #1136
  INST(Vmovdqu32        , VexRmMr_Lx         , E(F30F00,6F,_,x,_,0,4,FVM), E(F30F00,7F,_,x,_,0,4,FVM), 194, 93 , 326, 168), // #1137
  INST(Vmovdqu64        , VexRmMr_Lx         , E(F30F00,6F,_,x,_,1,4,FVM), E(F30F00,7F,_,x,_,1,4,FVM), 148, 94 , 326, 168), // #1138
  INST(Vmovdqu8         , VexRmMr_Lx         , E(F20F00,6F,_,x,_,0,4,FVM), E(F20F00,7F,_,x,_,0,4,FVM), 163, 95 , 326, 169), // #1139
  INST(Vmovhlps         , VexRvm             , V(000F00,12,_,0,I,0,_,_  ), 0                         , 74 , 0  , 327, 143), // #1140
  INST(Vmovhpd          , VexRvmMr           , V(660F00,16,_,0,I,1,3,T1S), V(660F00,17,_,0,I,1,3,T1S), 124, 96 , 328, 143), // #1141
  INST(Vmovhps          , VexRvmMr           , V(000F00,16,_,0,I,0,3,T2 ), V(000F00,17,_,0,I,0,3,T2 ), 195, 97 , 328, 143), // #1142
  INST(Vmovlhps         , VexRvm             , V(000F00,16,_,0,I,0,_,_  ), 0                         , 74 , 0  , 327, 143), // #1143
  INST(Vmovlpd          , VexRvmMr           , V(660F00,12,_,0,I,1,3,T1S), V(660F00,13,_,0,I,1,3,T1S), 124, 98 , 328, 143), // #1144
  INST(Vmovlps          , VexRvmMr           , V(000F00,12,_,0,I,0,3,T2 ), V(000F00,13,_,0,I,0,3,T2 ), 195, 99 , 328, 143), // #1145
  INST(Vmovmskpd        , VexRm_Lx           , V(660F00,50,_,x,I,_,_,_  ), 0                         , 71 , 0  , 329, 145), // #1146
  INST(Vmovmskps        , VexRm_Lx           , V(000F00,50,_,x,I,_,_,_  ), 0                         , 74 , 0  , 329, 145), // #1147
  INST(Vmovntdq         , VexMr_Lx           , V(660F00,E7,_,x,I,0,4,FVM), 0                         , 143, 0  , 330, 143), // #1148
  INST(Vmovntdqa        , VexRm_Lx           , V(660F38,2A,_,x,I,0,4,FVM), 0                         , 109, 0  , 331, 153), // #1149
  INST(Vmovntpd         , VexMr_Lx           , V(660F00,2B,_,x,I,1,4,FVM), 0                         , 102, 0  , 330, 143), // #1150
  INST(Vmovntps         , VexMr_Lx           , V(000F00,2B,_,x,I,0,4,FVM), 0                         , 104, 0  , 330, 143), // #1151
  INST(Vmovq            , VexMovdMovq        , V(660F00,6E,_,0,I,1,3,T1S), V(660F00,7E,_,0,I,1,3,T1S), 124, 100, 332, 166), // #1152
  INST(Vmovsd           , VexMovssMovsd      , V(F20F00,10,_,I,I,1,3,T1S), V(F20F00,11,_,I,I,1,3,T1S), 105, 101, 333, 166), // #1153
  INST(Vmovsh           , VexMovssMovsd      , E(F3MAP5,10,_,I,_,0,1,T1S), E(F3MAP5,11,_,I,_,0,1,T1S), 106, 102, 334, 144), // #1154
  INST(Vmovshdup        , VexRm_Lx           , V(F30F00,16,_,x,I,0,4,FVM), 0                         , 160, 0  , 335, 143), // #1155
  INST(Vmovsldup        , VexRm_Lx           , V(F30F00,12,_,x,I,0,4,FVM), 0                         , 160, 0  , 335, 143), // #1156
  INST(Vmovss           , VexMovssMovsd      , V(F30F00,10,_,I,I,0,2,T1S), V(F30F00,11,_,I,I,0,2,T1S), 107, 103, 336, 166), // #1157
  INST(Vmovupd          , VexRmMr_Lx         , V(660F00,10,_,x,I,1,4,FVM), V(660F00,11,_,x,I,1,4,FVM), 102, 104, 322, 166), // #1158
  INST(Vmovups          , VexRmMr_Lx         , V(000F00,10,_,x,I,0,4,FVM), V(000F00,11,_,x,I,0,4,FVM), 104, 105, 322, 166), // #1159
  INST(Vmovw            , VexMovdMovq        , E(66MAP5,6E,_,0,_,I,1,T1S), E(66MAP5,7E,_,0,_,I,1,T1S), 196, 106, 337, 144), // #1160
  INST(Vmpsadbw         , VexRvmi_Lx         , V(660F3A,42,_,x,I,_,_,_  ), 0                         , 75 , 0  , 212, 170), // #1161
  INST(Vmptrld          , X86M_Only          , O(000F00,C7,6,_,_,_,_,_  ), 0                         , 82 , 0  , 28 , 66 ), // #1162
  INST(Vmptrst          , X86M_Only          , O(000F00,C7,7,_,_,_,_,_  ), 0                         , 24 , 0  , 28 , 66 ), // #1163
  INST(Vmread           , X86Mr_NoSize       , O(000F00,78,_,_,_,_,_,_  ), 0                         , 5  , 0  , 338, 66 ), // #1164
  INST(Vmresume         , X86Op              , O(000F01,C3,_,_,_,_,_,_  ), 0                         , 23 , 0  , 26 , 66 ), // #1165
  INST(Vmrun            , X86Op_xAX          , O(000F01,D8,_,_,_,_,_,_  ), 0                         , 23 , 0  , 321, 22 ), // #1166
  INST(Vmsave           , X86Op_xAX          , O(000F01,DB,_,_,_,_,_,_  ), 0                         , 23 , 0  , 321, 22 ), // #1167
  INST(Vmulpd           , VexRvm_Lx          , V(660F00,59,_,x,I,1,4,FV ), 0                         , 102, 0  , 193, 143), // #1168
  INST(Vmulph           , VexRvm_Lx          , E(00MAP5,59,_,_,_,0,4,FV ), 0                         , 103, 0  , 194, 144), // #1169
  INST(Vmulps           , VexRvm_Lx          , V(000F00,59,_,x,I,0,4,FV ), 0                         , 104, 0  , 195, 143), // #1170
  INST(Vmulsd           , VexRvm             , V(F20F00,59,_,I,I,1,3,T1S), 0                         , 105, 0  , 196, 143), // #1171
  INST(Vmulsh           , VexRvm             , E(F3MAP5,59,_,_,_,0,1,T1S), 0                         , 106, 0  , 197, 144), // #1172
  INST(Vmulss           , VexRvm             , V(F30F00,59,_,I,I,0,2,T1S), 0                         , 107, 0  , 198, 143), // #1173
  INST(Vmwrite          , X86Rm_NoSize       , O(000F00,79,_,_,_,_,_,_  ), 0                         , 5  , 0  , 339, 66 ), // #1174
  INST(Vmxoff           , X86Op              , O(000F01,C4,_,_,_,_,_,_  ), 0                         , 23 , 0  , 26 , 66 ), // #1175
  INST(Vmxon            , X86M_Only          , O(F30F00,C7,6,_,_,_,_,_  ), 0                         , 26 , 0  , 28 , 66 ), // #1176
  INST(Vorpd            , VexRvm_Lx          , V(660F00,56,_,x,I,1,4,FV ), 0                         , 102, 0  , 207, 149), // #1177
  INST(Vorps            , VexRvm_Lx          , V(000F00,56,_,x,I,0,4,FV ), 0                         , 104, 0  , 208, 149), // #1178
  INST(Vp2intersectd    , VexRvm_Lx_2xK      , E(F20F38,68,_,_,_,0,4,FV ), 0                         , 130, 0  , 340, 171), // #1179
  INST(Vp2intersectq    , VexRvm_Lx_2xK      , E(F20F38,68,_,_,_,1,4,FV ), 0                         , 197, 0  , 341, 171), // #1180
  INST(Vpabsb           , VexRm_Lx           , V(660F38,1C,_,x,I,_,4,FVM), 0                         , 109, 0  , 335, 172), // #1181
  INST(Vpabsd           , VexRm_Lx           , V(660F38,1E,_,x,I,0,4,FV ), 0                         , 109, 0  , 342, 153), // #1182
  INST(Vpabsq           , VexRm_Lx           , E(660F38,1F,_,x,_,1,4,FV ), 0                         , 112, 0  , 343, 148), // #1183
  INST(Vpabsw           , VexRm_Lx           , V(660F38,1D,_,x,I,_,4,FVM), 0                         , 109, 0  , 335, 172), // #1184
  INST(Vpackssdw        , VexRvm_Lx          , V(660F00,6B,_,x,I,0,4,FV ), 0                         , 143, 0  , 206, 172), // #1185
  INST(Vpacksswb        , VexRvm_Lx          , V(660F00,63,_,x,I,I,4,FVM), 0                         , 143, 0  , 309, 172), // #1186
  INST(Vpackusdw        , VexRvm_Lx          , V(660F38,2B,_,x,I,0,4,FV ), 0                         , 109, 0  , 206, 172), // #1187
  INST(Vpackuswb        , VexRvm_Lx          , V(660F00,67,_,x,I,I,4,FVM), 0                         , 143, 0  , 309, 172), // #1188
  INST(Vpaddb           , VexRvm_Lx          , V(660F00,FC,_,x,I,I,4,FVM), 0                         , 143, 0  , 309, 172), // #1189
  INST(Vpaddd           , VexRvm_Lx          , V(660F00,FE,_,x,I,0,4,FV ), 0                         , 143, 0  , 206, 153), // #1190
  INST(Vpaddq           , VexRvm_Lx          , V(660F00,D4,_,x,I,1,4,FV ), 0                         , 102, 0  , 205, 153), // #1191
  INST(Vpaddsb          , VexRvm_Lx          , V(660F00,EC,_,x,I,I,4,FVM), 0                         , 143, 0  , 309, 172), // #1192
  INST(Vpaddsw          , VexRvm_Lx          , V(660F00,ED,_,x,I,I,4,FVM), 0                         , 143, 0  , 309, 172), // #1193
  INST(Vpaddusb         , VexRvm_Lx          , V(660F00,DC,_,x,I,I,4,FVM), 0                         , 143, 0  , 309, 172), // #1194
  INST(Vpaddusw         , VexRvm_Lx          , V(660F00,DD,_,x,I,I,4,FVM), 0                         , 143, 0  , 309, 172), // #1195
  INST(Vpaddw           , VexRvm_Lx          , V(660F00,FD,_,x,I,I,4,FVM), 0                         , 143, 0  , 309, 172), // #1196
  INST(Vpalignr         , VexRvmi_Lx         , V(660F3A,0F,_,x,I,I,4,FVM), 0                         , 198, 0  , 308, 172), // #1197
  INST(Vpand            , VexRvm_Lx          , V(660F00,DB,_,x,I,_,_,_  ), 0                         , 71 , 0  , 344, 170), // #1198
  INST(Vpandd           , VexRvm_Lx          , E(660F00,DB,_,x,_,0,4,FV ), 0                         , 192, 0  , 345, 148), // #1199
  INST(Vpandn           , VexRvm_Lx          , V(660F00,DF,_,x,I,_,_,_  ), 0                         , 71 , 0  , 346, 170), // #1200
  INST(Vpandnd          , VexRvm_Lx          , E(660F00,DF,_,x,_,0,4,FV ), 0                         , 192, 0  , 347, 148), // #1201
  INST(Vpandnq          , VexRvm_Lx          , E(660F00,DF,_,x,_,1,4,FV ), 0                         , 134, 0  , 348, 148), // #1202
  INST(Vpandq           , VexRvm_Lx          , E(660F00,DB,_,x,_,1,4,FV ), 0                         , 134, 0  , 349, 148), // #1203
  INST(Vpavgb           , VexRvm_Lx          , V(660F00,E0,_,x,I,I,4,FVM), 0                         , 143, 0  , 309, 172), // #1204
  INST(Vpavgw           , VexRvm_Lx          , V(660F00,E3,_,x,I,I,4,FVM), 0                         , 143, 0  , 309, 172), // #1205
  INST(Vpblendd         , VexRvmi_Lx         , V(660F3A,02,_,x,0,_,_,_  ), 0                         , 75 , 0  , 212, 152), // #1206
  INST(Vpblendmb        , VexRvm_Lx          , E(660F38,66,_,x,_,0,4,FVM), 0                         , 113, 0  , 350, 159), // #1207
  INST(Vpblendmd        , VexRvm_Lx          , E(660F38,64,_,x,_,0,4,FV ), 0                         , 113, 0  , 211, 148), // #1208
  INST(Vpblendmq        , VexRvm_Lx          , E(660F38,64,_,x,_,1,4,FV ), 0                         , 112, 0  , 210, 148), // #1209
  INST(Vpblendmw        , VexRvm_Lx          , E(660F38,66,_,x,_,1,4,FVM), 0                         , 112, 0  , 350, 159), // #1210
  INST(Vpblendvb        , VexRvmr_Lx         , V(660F3A,4C,_,x,0,_,_,_  ), 0                         , 75 , 0  , 213, 170), // #1211
  INST(Vpblendw         , VexRvmi_Lx         , V(660F3A,0E,_,x,I,_,_,_  ), 0                         , 75 , 0  , 212, 170), // #1212
  INST(Vpbroadcastb     , VexRm_Lx_Bcst      , V(660F38,78,_,x,0,0,0,T1S), E(660F38,7A,_,x,0,0,0,T1S), 30 , 107, 351, 173), // #1213
  INST(Vpbroadcastd     , VexRm_Lx_Bcst      , V(660F38,58,_,x,0,0,2,T1S), E(660F38,7C,_,x,0,0,0,T1S), 121, 108, 352, 163), // #1214
  INST(Vpbroadcastmb2q  , VexRm_Lx           , E(F30F38,2A,_,x,_,1,_,_  ), 0                         , 199, 0  , 353, 174), // #1215
  INST(Vpbroadcastmw2d  , VexRm_Lx           , E(F30F38,3A,_,x,_,0,_,_  ), 0                         , 200, 0  , 353, 174), // #1216
  INST(Vpbroadcastq     , VexRm_Lx_Bcst      , V(660F38,59,_,x,0,1,3,T1S), E(660F38,7C,_,x,0,1,0,T1S), 120, 109, 354, 163), // #1217
  INST(Vpbroadcastw     , VexRm_Lx_Bcst      , V(660F38,79,_,x,0,0,1,T1S), E(660F38,7B,_,x,0,0,0,T1S), 201, 110, 355, 173), // #1218
  INST(Vpclmulqdq       , VexRvmi_Lx         , V(660F3A,44,_,x,I,_,4,FVM), 0                         , 198, 0  , 356, 175), // #1219
  INST(Vpcmov           , VexRvrmRvmr_Lx     , V(XOP_M8,A2,_,x,x,_,_,_  ), 0                         , 202, 0  , 286, 162), // #1220
  INST(Vpcmpb           , VexRvmi_Lx         , E(660F3A,3F,_,x,_,0,4,FVM), 0                         , 110, 0  , 357, 159), // #1221
  INST(Vpcmpd           , VexRvmi_Lx         , E(660F3A,1F,_,x,_,0,4,FV ), 0                         , 110, 0  , 358, 148), // #1222
  INST(Vpcmpeqb         , VexRvm_Lx_KEvex    , V(660F00,74,_,x,I,I,4,FV ), 0                         , 143, 0  , 359, 172), // #1223
  INST(Vpcmpeqd         , VexRvm_Lx_KEvex    , V(660F00,76,_,x,I,0,4,FVM), 0                         , 143, 0  , 360, 153), // #1224
  INST(Vpcmpeqq         , VexRvm_Lx_KEvex    , V(660F38,29,_,x,I,1,4,FVM), 0                         , 203, 0  , 361, 153), // #1225
  INST(Vpcmpeqw         , VexRvm_Lx_KEvex    , V(660F00,75,_,x,I,I,4,FV ), 0                         , 143, 0  , 359, 172), // #1226
  INST(Vpcmpestri       , VexRmi             , V(660F3A,61,_,0,I,_,_,_  ), 0                         , 75 , 0  , 362, 176), // #1227
  INST(Vpcmpestrm       , VexRmi             , V(660F3A,60,_,0,I,_,_,_  ), 0                         , 75 , 0  , 363, 176), // #1228
  INST(Vpcmpgtb         , VexRvm_Lx_KEvex    , V(660F00,64,_,x,I,I,4,FV ), 0                         , 143, 0  , 359, 172), // #1229
  INST(Vpcmpgtd         , VexRvm_Lx_KEvex    , V(660F00,66,_,x,I,0,4,FVM), 0                         , 143, 0  , 360, 153), // #1230
  INST(Vpcmpgtq         , VexRvm_Lx_KEvex    , V(660F38,37,_,x,I,1,4,FVM), 0                         , 203, 0  , 361, 153), // #1231
  INST(Vpcmpgtw         , VexRvm_Lx_KEvex    , V(660F00,65,_,x,I,I,4,FV ), 0                         , 143, 0  , 359, 172), // #1232
  INST(Vpcmpistri       , VexRmi             , V(660F3A,63,_,0,I,_,_,_  ), 0                         , 75 , 0  , 364, 176), // #1233
  INST(Vpcmpistrm       , VexRmi             , V(660F3A,62,_,0,I,_,_,_  ), 0                         , 75 , 0  , 365, 176), // #1234
  INST(Vpcmpq           , VexRvmi_Lx         , E(660F3A,1F,_,x,_,1,4,FV ), 0                         , 111, 0  , 366, 148), // #1235
  INST(Vpcmpub          , VexRvmi_Lx         , E(660F3A,3E,_,x,_,0,4,FVM), 0                         , 110, 0  , 357, 159), // #1236
  INST(Vpcmpud          , VexRvmi_Lx         , E(660F3A,1E,_,x,_,0,4,FV ), 0                         , 110, 0  , 358, 148), // #1237
  INST(Vpcmpuq          , VexRvmi_Lx         , E(660F3A,1E,_,x,_,1,4,FV ), 0                         , 111, 0  , 366, 148), // #1238
  INST(Vpcmpuw          , VexRvmi_Lx         , E(660F3A,3E,_,x,_,1,4,FVM), 0                         , 111, 0  , 357, 159), // #1239
  INST(Vpcmpw           , VexRvmi_Lx         , E(660F3A,3F,_,x,_,1,4,FVM), 0                         , 111, 0  , 357, 159), // #1240
  INST(Vpcomb           , VexRvmi            , V(XOP_M8,CC,_,0,0,_,_,_  ), 0                         , 202, 0  , 275, 162), // #1241
  INST(Vpcomd           , VexRvmi            , V(XOP_M8,CE,_,0,0,_,_,_  ), 0                         , 202, 0  , 275, 162), // #1242
  INST(Vpcompressb      , VexMr_Lx           , E(660F38,63,_,x,_,0,0,T1S), 0                         , 204, 0  , 230, 177), // #1243
  INST(Vpcompressd      , VexMr_Lx           , E(660F38,8B,_,x,_,0,2,T1S), 0                         , 128, 0  , 230, 148), // #1244
  INST(Vpcompressq      , VexMr_Lx           , E(660F38,8B,_,x,_,1,3,T1S), 0                         , 127, 0  , 230, 148), // #1245
  INST(Vpcompressw      , VexMr_Lx           , E(660F38,63,_,x,_,1,1,T1S), 0                         , 205, 0  , 230, 177), // #1246
  INST(Vpcomq           , VexRvmi            , V(XOP_M8,CF,_,0,0,_,_,_  ), 0                         , 202, 0  , 275, 162), // #1247
  INST(Vpcomub          , VexRvmi            , V(XOP_M8,EC,_,0,0,_,_,_  ), 0                         , 202, 0  , 275, 162), // #1248
  INST(Vpcomud          , VexRvmi            , V(XOP_M8,EE,_,0,0,_,_,_  ), 0                         , 202, 0  , 275, 162), // #1249
  INST(Vpcomuq          , VexRvmi            , V(XOP_M8,EF,_,0,0,_,_,_  ), 0                         , 202, 0  , 275, 162), // #1250
  INST(Vpcomuw          , VexRvmi            , V(XOP_M8,ED,_,0,0,_,_,_  ), 0                         , 202, 0  , 275, 162), // #1251
  INST(Vpcomw           , VexRvmi            , V(XOP_M8,CD,_,0,0,_,_,_  ), 0                         , 202, 0  , 275, 162), // #1252
  INST(Vpconflictd      , VexRm_Lx           , E(660F38,C4,_,x,_,0,4,FV ), 0                         , 113, 0  , 367, 174), // #1253
  INST(Vpconflictq      , VexRm_Lx           , E(660F38,C4,_,x,_,1,4,FV ), 0                         , 112, 0  , 367, 174), // #1254
  INST(Vpdpbssd         , VexRvm_Lx          , V(F20F38,50,_,x,0,_,_,_  ), 0                         , 85 , 0  , 199, 178), // #1255
  INST(Vpdpbssds        , VexRvm_Lx          , V(F20F38,51,_,x,0,_,_,_  ), 0                         , 85 , 0  , 199, 178), // #1256
  INST(Vpdpbsud         , VexRvm_Lx          , V(F30F38,50,_,x,0,_,_,_  ), 0                         , 89 , 0  , 199, 178), // #1257
  INST(Vpdpbsuds        , VexRvm_Lx          , V(F30F38,51,_,x,0,_,_,_  ), 0                         , 89 , 0  , 199, 178), // #1258
  INST(Vpdpbusd         , VexRvm_Lx          , V(660F38,50,_,x,_,0,4,FV ), 0                         , 109, 0  , 368, 179), // #1259
  INST(Vpdpbusds        , VexRvm_Lx          , V(660F38,51,_,x,_,0,4,FV ), 0                         , 109, 0  , 368, 179), // #1260
  INST(Vpdpbuud         , VexRvm_Lx          , V(000F38,50,_,x,0,_,_,_  ), 0                         , 11 , 0  , 199, 178), // #1261
  INST(Vpdpbuuds        , VexRvm_Lx          , V(000F38,51,_,x,0,_,_,_  ), 0                         , 11 , 0  , 199, 178), // #1262
  INST(Vpdpwssd         , VexRvm_Lx          , V(660F38,52,_,x,_,0,4,FV ), 0                         , 109, 0  , 368, 179), // #1263
  INST(Vpdpwssds        , VexRvm_Lx          , V(660F38,53,_,x,_,0,4,FV ), 0                         , 109, 0  , 368, 179), // #1264
  INST(Vpdpwsud         , VexRvm_Lx          , V(F30F38,D2,_,x,0,_,_,_  ), 0                         , 89 , 0  , 199, 180), // #1265
  INST(Vpdpwsuds        , VexRvm_Lx          , V(F30F38,D3,_,x,0,_,_,_  ), 0                         , 89 , 0  , 199, 180), // #1266
  INST(Vpdpwusd         , VexRvm_Lx          , V(660F38,D2,_,x,0,_,_,_  ), 0                         , 30 , 0  , 199, 180), // #1267
  INST(Vpdpwusds        , VexRvm_Lx          , V(660F38,D3,_,x,0,_,_,_  ), 0                         , 30 , 0  , 199, 180), // #1268
  INST(Vpdpwuud         , VexRvm_Lx          , V(000F38,D2,_,x,0,_,_,_  ), 0                         , 11 , 0  , 199, 180), // #1269
  INST(Vpdpwuuds        , VexRvm_Lx          , V(000F38,D3,_,x,0,_,_,_  ), 0                         , 11 , 0  , 199, 180), // #1270
  INST(Vperm2f128       , VexRvmi            , V(660F3A,06,_,1,0,_,_,_  ), 0                         , 170, 0  , 369, 145), // #1271
  INST(Vperm2i128       , VexRvmi            , V(660F3A,46,_,1,0,_,_,_  ), 0                         , 170, 0  , 369, 152), // #1272
  INST(Vpermb           , VexRvm_Lx          , E(660F38,8D,_,x,_,0,4,FVM), 0                         , 113, 0  , 350, 181), // #1273
  INST(Vpermd           , VexRvm_Lx          , V(660F38,36,_,x,0,0,4,FV ), 0                         , 109, 0  , 370, 163), // #1274
  INST(Vpermi2b         , VexRvm_Lx          , E(660F38,75,_,x,_,0,4,FVM), 0                         , 113, 0  , 350, 181), // #1275
  INST(Vpermi2d         , VexRvm_Lx          , E(660F38,76,_,x,_,0,4,FV ), 0                         , 113, 0  , 211, 148), // #1276
  INST(Vpermi2pd        , VexRvm_Lx          , E(660F38,77,_,x,_,1,4,FV ), 0                         , 112, 0  , 210, 148), // #1277
  INST(Vpermi2ps        , VexRvm_Lx          , E(660F38,77,_,x,_,0,4,FV ), 0                         , 113, 0  , 211, 148), // #1278
  INST(Vpermi2q         , VexRvm_Lx          , E(660F38,76,_,x,_,1,4,FV ), 0                         , 112, 0  , 210, 148), // #1279
  INST(Vpermi2w         , VexRvm_Lx          , E(660F38,75,_,x,_,1,4,FVM), 0                         , 112, 0  , 350, 159), // #1280
  INST(Vpermil2pd       , VexRvrmiRvmri_Lx   , V(660F3A,49,_,x,x,_,_,_  ), 0                         , 75 , 0  , 371, 162), // #1281
  INST(Vpermil2ps       , VexRvrmiRvmri_Lx   , V(660F3A,48,_,x,x,_,_,_  ), 0                         , 75 , 0  , 371, 162), // #1282
  INST(Vpermilpd        , VexRvmRmi_Lx       , V(660F38,0D,_,x,0,1,4,FV ), V(660F3A,05,_,x,0,1,4,FV ), 203, 111, 372, 143), // #1283
  INST(Vpermilps        , VexRvmRmi_Lx       , V(660F38,0C,_,x,0,0,4,FV ), V(660F3A,04,_,x,0,0,4,FV ), 109, 112, 373, 143), // #1284
  INST(Vpermpd          , VexRvmRmi_Lx       , E(660F38,16,_,x,1,1,4,FV ), V(660F3A,01,_,x,1,1,4,FV ), 206, 113, 374, 163), // #1285
  INST(Vpermps          , VexRvm_Lx          , V(660F38,16,_,x,0,0,4,FV ), 0                         , 109, 0  , 370, 163), // #1286
  INST(Vpermq           , VexRvmRmi_Lx       , E(660F38,36,_,x,_,1,4,FV ), V(660F3A,00,_,x,1,1,4,FV ), 112, 114, 374, 163), // #1287
  INST(Vpermt2b         , VexRvm_Lx          , E(660F38,7D,_,x,_,0,4,FVM), 0                         , 113, 0  , 350, 181), // #1288
  INST(Vpermt2d         , VexRvm_Lx          , E(660F38,7E,_,x,_,0,4,FV ), 0                         , 113, 0  , 211, 148), // #1289
  INST(Vpermt2pd        , VexRvm_Lx          , E(660F38,7F,_,x,_,1,4,FV ), 0                         , 112, 0  , 210, 148), // #1290
  INST(Vpermt2ps        , VexRvm_Lx          , E(660F38,7F,_,x,_,0,4,FV ), 0                         , 113, 0  , 211, 148), // #1291
  INST(Vpermt2q         , VexRvm_Lx          , E(660F38,7E,_,x,_,1,4,FV ), 0                         , 112, 0  , 210, 148), // #1292
  INST(Vpermt2w         , VexRvm_Lx          , E(660F38,7D,_,x,_,1,4,FVM), 0                         , 112, 0  , 350, 159), // #1293
  INST(Vpermw           , VexRvm_Lx          , E(660F38,8D,_,x,_,1,4,FVM), 0                         , 112, 0  , 350, 159), // #1294
  INST(Vpexpandb        , VexRm_Lx           , E(660F38,62,_,x,_,0,0,T1S), 0                         , 204, 0  , 276, 177), // #1295
  INST(Vpexpandd        , VexRm_Lx           , E(660F38,89,_,x,_,0,2,T1S), 0                         , 128, 0  , 276, 148), // #1296
  INST(Vpexpandq        , VexRm_Lx           , E(660F38,89,_,x,_,1,3,T1S), 0                         , 127, 0  , 276, 148), // #1297
  INST(Vpexpandw        , VexRm_Lx           , E(660F38,62,_,x,_,1,1,T1S), 0                         , 205, 0  , 276, 177), // #1298
  INST(Vpextrb          , VexMri             , V(660F3A,14,_,0,0,I,0,T1S), 0                         , 75 , 0  , 375, 182), // #1299
  INST(Vpextrd          , VexMri             , V(660F3A,16,_,0,0,0,2,T1S), 0                         , 175, 0  , 280, 149), // #1300
  INST(Vpextrq          , VexMri             , V(660F3A,16,_,0,1,1,3,T1S), 0                         , 207, 0  , 376, 149), // #1301
  INST(Vpextrw          , VexMri_Vpextrw     , V(660F3A,15,_,0,0,I,1,T1S), 0                         , 208, 0  , 377, 182), // #1302
  INST(Vpgatherdd       , VexRmvRm_VM        , V(660F38,90,_,x,0,_,_,_  ), E(660F38,90,_,x,_,0,2,T1S), 30 , 115, 299, 163), // #1303
  INST(Vpgatherdq       , VexRmvRm_VM        , V(660F38,90,_,x,1,_,_,_  ), E(660F38,90,_,x,_,1,3,T1S), 187, 116, 298, 163), // #1304
  INST(Vpgatherqd       , VexRmvRm_VM        , V(660F38,91,_,x,0,_,_,_  ), E(660F38,91,_,x,_,0,2,T1S), 30 , 117, 301, 163), // #1305
  INST(Vpgatherqq       , VexRmvRm_VM        , V(660F38,91,_,x,1,_,_,_  ), E(660F38,91,_,x,_,1,3,T1S), 187, 118, 300, 163), // #1306
  INST(Vphaddbd         , VexRm              , V(XOP_M9,C2,_,0,0,_,_,_  ), 0                         , 81 , 0  , 201, 162), // #1307
  INST(Vphaddbq         , VexRm              , V(XOP_M9,C3,_,0,0,_,_,_  ), 0                         , 81 , 0  , 201, 162), // #1308
  INST(Vphaddbw         , VexRm              , V(XOP_M9,C1,_,0,0,_,_,_  ), 0                         , 81 , 0  , 201, 162), // #1309
  INST(Vphaddd          , VexRvm_Lx          , V(660F38,02,_,x,I,_,_,_  ), 0                         , 30 , 0  , 199, 170), // #1310
  INST(Vphadddq         , VexRm              , V(XOP_M9,CB,_,0,0,_,_,_  ), 0                         , 81 , 0  , 201, 162), // #1311
  INST(Vphaddsw         , VexRvm_Lx          , V(660F38,03,_,x,I,_,_,_  ), 0                         , 30 , 0  , 199, 170), // #1312
  INST(Vphaddubd        , VexRm              , V(XOP_M9,D2,_,0,0,_,_,_  ), 0                         , 81 , 0  , 201, 162), // #1313
  INST(Vphaddubq        , VexRm              , V(XOP_M9,D3,_,0,0,_,_,_  ), 0                         , 81 , 0  , 201, 162), // #1314
  INST(Vphaddubw        , VexRm              , V(XOP_M9,D1,_,0,0,_,_,_  ), 0                         , 81 , 0  , 201, 162), // #1315
  INST(Vphaddudq        , VexRm              , V(XOP_M9,DB,_,0,0,_,_,_  ), 0                         , 81 , 0  , 201, 162), // #1316
  INST(Vphadduwd        , VexRm              , V(XOP_M9,D6,_,0,0,_,_,_  ), 0                         , 81 , 0  , 201, 162), // #1317
  INST(Vphadduwq        , VexRm              , V(XOP_M9,D7,_,0,0,_,_,_  ), 0                         , 81 , 0  , 201, 162), // #1318
  INST(Vphaddw          , VexRvm_Lx          , V(660F38,01,_,x,I,_,_,_  ), 0                         , 30 , 0  , 199, 170), // #1319
  INST(Vphaddwd         , VexRm              , V(XOP_M9,C6,_,0,0,_,_,_  ), 0                         , 81 , 0  , 201, 162), // #1320
  INST(Vphaddwq         , VexRm              , V(XOP_M9,C7,_,0,0,_,_,_  ), 0                         , 81 , 0  , 201, 162), // #1321
  INST(Vphminposuw      , VexRm              , V(660F38,41,_,0,I,_,_,_  ), 0                         , 30 , 0  , 201, 145), // #1322
  INST(Vphsubbw         , VexRm              , V(XOP_M9,E1,_,0,0,_,_,_  ), 0                         , 81 , 0  , 201, 162), // #1323
  INST(Vphsubd          , VexRvm_Lx          , V(660F38,06,_,x,I,_,_,_  ), 0                         , 30 , 0  , 199, 170), // #1324
  INST(Vphsubdq         , VexRm              , V(XOP_M9,E3,_,0,0,_,_,_  ), 0                         , 81 , 0  , 201, 162), // #1325
  INST(Vphsubsw         , VexRvm_Lx          , V(660F38,07,_,x,I,_,_,_  ), 0                         , 30 , 0  , 199, 170), // #1326
  INST(Vphsubw          , VexRvm_Lx          , V(660F38,05,_,x,I,_,_,_  ), 0                         , 30 , 0  , 199, 170), // #1327
  INST(Vphsubwd         , VexRm              , V(XOP_M9,E2,_,0,0,_,_,_  ), 0                         , 81 , 0  , 201, 162), // #1328
  INST(Vpinsrb          , VexRvmi            , V(660F3A,20,_,0,0,I,0,T1S), 0                         , 75 , 0  , 378, 182), // #1329
  INST(Vpinsrd          , VexRvmi            , V(660F3A,22,_,0,0,0,2,T1S), 0                         , 175, 0  , 379, 149), // #1330
  INST(Vpinsrq          , VexRvmi            , V(660F3A,22,_,0,1,1,3,T1S), 0                         , 207, 0  , 380, 149), // #1331
  INST(Vpinsrw          , VexRvmi            , V(660F00,C4,_,0,0,I,1,T1S), 0                         , 209, 0  , 381, 182), // #1332
  INST(Vplzcntd         , VexRm_Lx           , E(660F38,44,_,x,_,0,4,FV ), 0                         , 113, 0  , 367, 174), // #1333
  INST(Vplzcntq         , VexRm_Lx           , E(660F38,44,_,x,_,1,4,FV ), 0                         , 112, 0  , 343, 174), // #1334
  INST(Vpmacsdd         , VexRvmr            , V(XOP_M8,9E,_,0,0,_,_,_  ), 0                         , 202, 0  , 382, 162), // #1335
  INST(Vpmacsdqh        , VexRvmr            , V(XOP_M8,9F,_,0,0,_,_,_  ), 0                         , 202, 0  , 382, 162), // #1336
  INST(Vpmacsdql        , VexRvmr            , V(XOP_M8,97,_,0,0,_,_,_  ), 0                         , 202, 0  , 382, 162), // #1337
  INST(Vpmacssdd        , VexRvmr            , V(XOP_M8,8E,_,0,0,_,_,_  ), 0                         , 202, 0  , 382, 162), // #1338
  INST(Vpmacssdqh       , VexRvmr            , V(XOP_M8,8F,_,0,0,_,_,_  ), 0                         , 202, 0  , 382, 162), // #1339
  INST(Vpmacssdql       , VexRvmr            , V(XOP_M8,87,_,0,0,_,_,_  ), 0                         , 202, 0  , 382, 162), // #1340
  INST(Vpmacsswd        , VexRvmr            , V(XOP_M8,86,_,0,0,_,_,_  ), 0                         , 202, 0  , 382, 162), // #1341
  INST(Vpmacssww        , VexRvmr            , V(XOP_M8,85,_,0,0,_,_,_  ), 0                         , 202, 0  , 382, 162), // #1342
  INST(Vpmacswd         , VexRvmr            , V(XOP_M8,96,_,0,0,_,_,_  ), 0                         , 202, 0  , 382, 162), // #1343
  INST(Vpmacsww         , VexRvmr            , V(XOP_M8,95,_,0,0,_,_,_  ), 0                         , 202, 0  , 382, 162), // #1344
  INST(Vpmadcsswd       , VexRvmr            , V(XOP_M8,A6,_,0,0,_,_,_  ), 0                         , 202, 0  , 382, 162), // #1345
  INST(Vpmadcswd        , VexRvmr            , V(XOP_M8,B6,_,0,0,_,_,_  ), 0                         , 202, 0  , 382, 162), // #1346
  INST(Vpmadd52huq      , VexRvm_Lx          , V(660F38,B5,_,x,1,1,4,FV ), 0                         , 180, 0  , 383, 183), // #1347
  INST(Vpmadd52luq      , VexRvm_Lx          , V(660F38,B4,_,x,1,1,4,FV ), 0                         , 180, 0  , 383, 183), // #1348
  INST(Vpmaddubsw       , VexRvm_Lx          , V(660F38,04,_,x,I,I,4,FVM), 0                         , 109, 0  , 309, 172), // #1349
  INST(Vpmaddwd         , VexRvm_Lx          , V(660F00,F5,_,x,I,I,4,FVM), 0                         , 143, 0  , 309, 172), // #1350
  INST(Vpmaskmovd       , VexRvmMvr_Lx       , V(660F38,8C,_,x,0,_,_,_  ), V(660F38,8E,_,x,0,_,_,_  ), 30 , 119, 316, 152), // #1351
  INST(Vpmaskmovq       , VexRvmMvr_Lx       , V(660F38,8C,_,x,1,_,_,_  ), V(660F38,8E,_,x,1,_,_,_  ), 187, 120, 316, 152), // #1352
  INST(Vpmaxsb          , VexRvm_Lx          , V(660F38,3C,_,x,I,I,4,FVM), 0                         , 109, 0  , 384, 172), // #1353
  INST(Vpmaxsd          , VexRvm_Lx          , V(660F38,3D,_,x,I,0,4,FV ), 0                         , 109, 0  , 208, 153), // #1354
  INST(Vpmaxsq          , VexRvm_Lx          , E(660F38,3D,_,x,_,1,4,FV ), 0                         , 112, 0  , 210, 148), // #1355
  INST(Vpmaxsw          , VexRvm_Lx          , V(660F00,EE,_,x,I,I,4,FVM), 0                         , 143, 0  , 384, 172), // #1356
  INST(Vpmaxub          , VexRvm_Lx          , V(660F00,DE,_,x,I,I,4,FVM), 0                         , 143, 0  , 384, 172), // #1357
  INST(Vpmaxud          , VexRvm_Lx          , V(660F38,3F,_,x,I,0,4,FV ), 0                         , 109, 0  , 208, 153), // #1358
  INST(Vpmaxuq          , VexRvm_Lx          , E(660F38,3F,_,x,_,1,4,FV ), 0                         , 112, 0  , 210, 148), // #1359
  INST(Vpmaxuw          , VexRvm_Lx          , V(660F38,3E,_,x,I,I,4,FVM), 0                         , 109, 0  , 384, 172), // #1360
  INST(Vpminsb          , VexRvm_Lx          , V(660F38,38,_,x,I,I,4,FVM), 0                         , 109, 0  , 384, 172), // #1361
  INST(Vpminsd          , VexRvm_Lx          , V(660F38,39,_,x,I,0,4,FV ), 0                         , 109, 0  , 208, 153), // #1362
  INST(Vpminsq          , VexRvm_Lx          , E(660F38,39,_,x,_,1,4,FV ), 0                         , 112, 0  , 210, 148), // #1363
  INST(Vpminsw          , VexRvm_Lx          , V(660F00,EA,_,x,I,I,4,FVM), 0                         , 143, 0  , 384, 172), // #1364
  INST(Vpminub          , VexRvm_Lx          , V(660F00,DA,_,x,I,_,4,FVM), 0                         , 143, 0  , 384, 172), // #1365
  INST(Vpminud          , VexRvm_Lx          , V(660F38,3B,_,x,I,0,4,FV ), 0                         , 109, 0  , 208, 153), // #1366
  INST(Vpminuq          , VexRvm_Lx          , E(660F38,3B,_,x,_,1,4,FV ), 0                         , 112, 0  , 210, 148), // #1367
  INST(Vpminuw          , VexRvm_Lx          , V(660F38,3A,_,x,I,_,4,FVM), 0                         , 109, 0  , 384, 172), // #1368
  INST(Vpmovb2m         , VexRm_Lx           , E(F30F38,29,_,x,_,0,_,_  ), 0                         , 200, 0  , 385, 159), // #1369
  INST(Vpmovd2m         , VexRm_Lx           , E(F30F38,39,_,x,_,0,_,_  ), 0                         , 200, 0  , 385, 151), // #1370
  INST(Vpmovdb          , VexMr_Lx           , E(F30F38,31,_,x,_,0,2,QVM), 0                         , 210, 0  , 386, 148), // #1371
  INST(Vpmovdw          , VexMr_Lx           , E(F30F38,33,_,x,_,0,3,HVM), 0                         , 211, 0  , 387, 148), // #1372
  INST(Vpmovm2b         , VexRm_Lx           , E(F30F38,28,_,x,_,0,_,_  ), 0                         , 200, 0  , 353, 159), // #1373
  INST(Vpmovm2d         , VexRm_Lx           , E(F30F38,38,_,x,_,0,_,_  ), 0                         , 200, 0  , 353, 151), // #1374
  INST(Vpmovm2q         , VexRm_Lx           , E(F30F38,38,_,x,_,1,_,_  ), 0                         , 199, 0  , 353, 151), // #1375
  INST(Vpmovm2w         , VexRm_Lx           , E(F30F38,28,_,x,_,1,_,_  ), 0                         , 199, 0  , 353, 159), // #1376
  INST(Vpmovmskb        , VexRm_Lx           , V(660F00,D7,_,x,I,_,_,_  ), 0                         , 71 , 0  , 329, 170), // #1377
  INST(Vpmovq2m         , VexRm_Lx           , E(F30F38,39,_,x,_,1,_,_  ), 0                         , 199, 0  , 385, 151), // #1378
  INST(Vpmovqb          , VexMr_Lx           , E(F30F38,32,_,x,_,0,1,OVM), 0                         , 212, 0  , 388, 148), // #1379
  INST(Vpmovqd          , VexMr_Lx           , E(F30F38,35,_,x,_,0,3,HVM), 0                         , 211, 0  , 387, 148), // #1380
  INST(Vpmovqw          , VexMr_Lx           , E(F30F38,34,_,x,_,0,2,QVM), 0                         , 210, 0  , 386, 148), // #1381
  INST(Vpmovsdb         , VexMr_Lx           , E(F30F38,21,_,x,_,0,2,QVM), 0                         , 210, 0  , 386, 148), // #1382
  INST(Vpmovsdw         , VexMr_Lx           , E(F30F38,23,_,x,_,0,3,HVM), 0                         , 211, 0  , 387, 148), // #1383
  INST(Vpmovsqb         , VexMr_Lx           , E(F30F38,22,_,x,_,0,1,OVM), 0                         , 212, 0  , 388, 148), // #1384
  INST(Vpmovsqd         , VexMr_Lx           , E(F30F38,25,_,x,_,0,3,HVM), 0                         , 211, 0  , 387, 148), // #1385
  INST(Vpmovsqw         , VexMr_Lx           , E(F30F38,24,_,x,_,0,2,QVM), 0                         , 210, 0  , 386, 148), // #1386
  INST(Vpmovswb         , VexMr_Lx           , E(F30F38,20,_,x,_,0,3,HVM), 0                         , 211, 0  , 387, 159), // #1387
  INST(Vpmovsxbd        , VexRm_Lx           , V(660F38,21,_,x,I,I,2,QVM), 0                         , 213, 0  , 389, 153), // #1388
  INST(Vpmovsxbq        , VexRm_Lx           , V(660F38,22,_,x,I,I,1,OVM), 0                         , 214, 0  , 390, 153), // #1389
  INST(Vpmovsxbw        , VexRm_Lx           , V(660F38,20,_,x,I,I,3,HVM), 0                         , 138, 0  , 391, 172), // #1390
  INST(Vpmovsxdq        , VexRm_Lx           , V(660F38,25,_,x,I,0,3,HVM), 0                         , 138, 0  , 391, 153), // #1391
  INST(Vpmovsxwd        , VexRm_Lx           , V(660F38,23,_,x,I,I,3,HVM), 0                         , 138, 0  , 391, 153), // #1392
  INST(Vpmovsxwq        , VexRm_Lx           , V(660F38,24,_,x,I,I,2,QVM), 0                         , 213, 0  , 389, 153), // #1393
  INST(Vpmovusdb        , VexMr_Lx           , E(F30F38,11,_,x,_,0,2,QVM), 0                         , 210, 0  , 386, 148), // #1394
  INST(Vpmovusdw        , VexMr_Lx           , E(F30F38,13,_,x,_,0,3,HVM), 0                         , 211, 0  , 387, 148), // #1395
  INST(Vpmovusqb        , VexMr_Lx           , E(F30F38,12,_,x,_,0,1,OVM), 0                         , 212, 0  , 388, 148), // #1396
  INST(Vpmovusqd        , VexMr_Lx           , E(F30F38,15,_,x,_,0,3,HVM), 0                         , 211, 0  , 387, 148), // #1397
  INST(Vpmovusqw        , VexMr_Lx           , E(F30F38,14,_,x,_,0,2,QVM), 0                         , 210, 0  , 386, 148), // #1398
  INST(Vpmovuswb        , VexMr_Lx           , E(F30F38,10,_,x,_,0,3,HVM), 0                         , 211, 0  , 387, 159), // #1399
  INST(Vpmovw2m         , VexRm_Lx           , E(F30F38,29,_,x,_,1,_,_  ), 0                         , 199, 0  , 385, 159), // #1400
  INST(Vpmovwb          , VexMr_Lx           , E(F30F38,30,_,x,_,0,3,HVM), 0                         , 211, 0  , 387, 159), // #1401
  INST(Vpmovzxbd        , VexRm_Lx           , V(660F38,31,_,x,I,I,2,QVM), 0                         , 213, 0  , 389, 153), // #1402
  INST(Vpmovzxbq        , VexRm_Lx           , V(660F38,32,_,x,I,I,1,OVM), 0                         , 214, 0  , 390, 153), // #1403
  INST(Vpmovzxbw        , VexRm_Lx           , V(660F38,30,_,x,I,I,3,HVM), 0                         , 138, 0  , 391, 172), // #1404
  INST(Vpmovzxdq        , VexRm_Lx           , V(660F38,35,_,x,I,0,3,HVM), 0                         , 138, 0  , 391, 153), // #1405
  INST(Vpmovzxwd        , VexRm_Lx           , V(660F38,33,_,x,I,I,3,HVM), 0                         , 138, 0  , 391, 153), // #1406
  INST(Vpmovzxwq        , VexRm_Lx           , V(660F38,34,_,x,I,I,2,QVM), 0                         , 213, 0  , 389, 153), // #1407
  INST(Vpmuldq          , VexRvm_Lx          , V(660F38,28,_,x,I,1,4,FV ), 0                         , 203, 0  , 205, 153), // #1408
  INST(Vpmulhrsw        , VexRvm_Lx          , V(660F38,0B,_,x,I,I,4,FVM), 0                         , 109, 0  , 309, 172), // #1409
  INST(Vpmulhuw         , VexRvm_Lx          , V(660F00,E4,_,x,I,I,4,FVM), 0                         , 143, 0  , 309, 172), // #1410
  INST(Vpmulhw          , VexRvm_Lx          , V(660F00,E5,_,x,I,I,4,FVM), 0                         , 143, 0  , 309, 172), // #1411
  INST(Vpmulld          , VexRvm_Lx          , V(660F38,40,_,x,I,0,4,FV ), 0                         , 109, 0  , 206, 153), // #1412
  INST(Vpmullq          , VexRvm_Lx          , E(660F38,40,_,x,_,1,4,FV ), 0                         , 112, 0  , 210, 151), // #1413
  INST(Vpmullw          , VexRvm_Lx          , V(660F00,D5,_,x,I,I,4,FVM), 0                         , 143, 0  , 309, 172), // #1414
  INST(Vpmultishiftqb   , VexRvm_Lx          , E(660F38,83,_,x,_,1,4,FV ), 0                         , 112, 0  , 210, 181), // #1415
  INST(Vpmuludq         , VexRvm_Lx          , V(660F00,F4,_,x,I,1,4,FV ), 0                         , 102, 0  , 205, 153), // #1416
  INST(Vpopcntb         , VexRm_Lx           , E(660F38,54,_,x,_,0,4,FV ), 0                         , 113, 0  , 276, 184), // #1417
  INST(Vpopcntd         , VexRm_Lx           , E(660F38,55,_,x,_,0,4,FVM), 0                         , 113, 0  , 367, 185), // #1418
  INST(Vpopcntq         , VexRm_Lx           , E(660F38,55,_,x,_,1,4,FVM), 0                         , 112, 0  , 343, 185), // #1419
  INST(Vpopcntw         , VexRm_Lx           , E(660F38,54,_,x,_,1,4,FV ), 0                         , 112, 0  , 276, 184), // #1420
  INST(Vpor             , VexRvm_Lx          , V(660F00,EB,_,x,I,_,_,_  ), 0                         , 71 , 0  , 344, 170), // #1421
  INST(Vpord            , VexRvm_Lx          , E(660F00,EB,_,x,_,0,4,FV ), 0                         , 192, 0  , 345, 148), // #1422
  INST(Vporq            , VexRvm_Lx          , E(660F00,EB,_,x,_,1,4,FV ), 0                         , 134, 0  , 349, 148), // #1423
  INST(Vpperm           , VexRvrmRvmr        , V(XOP_M8,A3,_,0,x,_,_,_  ), 0                         , 202, 0  , 392, 162), // #1424
  INST(Vprold           , VexVmi_Lx          , E(660F00,72,1,x,_,0,4,FV ), 0                         , 215, 0  , 393, 148), // #1425
  INST(Vprolq           , VexVmi_Lx          , E(660F00,72,1,x,_,1,4,FV ), 0                         , 216, 0  , 394, 148), // #1426
  INST(Vprolvd          , VexRvm_Lx          , E(660F38,15,_,x,_,0,4,FV ), 0                         , 113, 0  , 211, 148), // #1427
  INST(Vprolvq          , VexRvm_Lx          , E(660F38,15,_,x,_,1,4,FV ), 0                         , 112, 0  , 210, 148), // #1428
  INST(Vprord           , VexVmi_Lx          , E(660F00,72,0,x,_,0,4,FV ), 0                         , 192, 0  , 393, 148), // #1429
  INST(Vprorq           , VexVmi_Lx          , E(660F00,72,0,x,_,1,4,FV ), 0                         , 134, 0  , 394, 148), // #1430
  INST(Vprorvd          , VexRvm_Lx          , E(660F38,14,_,x,_,0,4,FV ), 0                         , 113, 0  , 211, 148), // #1431
  INST(Vprorvq          , VexRvm_Lx          , E(660F38,14,_,x,_,1,4,FV ), 0                         , 112, 0  , 210, 148), // #1432
  INST(Vprotb           , VexRvmRmvRmi       , V(XOP_M9,90,_,0,x,_,_,_  ), V(XOP_M8,C0,_,0,x,_,_,_  ), 81 , 121, 395, 162), // #1433
  INST(Vprotd           , VexRvmRmvRmi       , V(XOP_M9,92,_,0,x,_,_,_  ), V(XOP_M8,C2,_,0,x,_,_,_  ), 81 , 122, 395, 162), // #1434
  INST(Vprotq           , VexRvmRmvRmi       , V(XOP_M9,93,_,0,x,_,_,_  ), V(XOP_M8,C3,_,0,x,_,_,_  ), 81 , 123, 395, 162), // #1435
  INST(Vprotw           , VexRvmRmvRmi       , V(XOP_M9,91,_,0,x,_,_,_  ), V(XOP_M8,C1,_,0,x,_,_,_  ), 81 , 124, 395, 162), // #1436
  INST(Vpsadbw          , VexRvm_Lx          , V(660F00,F6,_,x,I,I,4,FVM), 0                         , 143, 0  , 200, 172), // #1437
  INST(Vpscatterdd      , VexMr_VM           , E(660F38,A0,_,x,_,0,2,T1S), 0                         , 128, 0  , 396, 148), // #1438
  INST(Vpscatterdq      , VexMr_VM           , E(660F38,A0,_,x,_,1,3,T1S), 0                         , 127, 0  , 397, 148), // #1439
  INST(Vpscatterqd      , VexMr_VM           , E(660F38,A1,_,x,_,0,2,T1S), 0                         , 128, 0  , 398, 148), // #1440
  INST(Vpscatterqq      , VexMr_VM           , E(660F38,A1,_,x,_,1,3,T1S), 0                         , 127, 0  , 399, 148), // #1441
  INST(Vpshab           , VexRvmRmv          , V(XOP_M9,98,_,0,x,_,_,_  ), 0                         , 81 , 0  , 400, 162), // #1442
  INST(Vpshad           , VexRvmRmv          , V(XOP_M9,9A,_,0,x,_,_,_  ), 0                         , 81 , 0  , 400, 162), // #1443
  INST(Vpshaq           , VexRvmRmv          , V(XOP_M9,9B,_,0,x,_,_,_  ), 0                         , 81 , 0  , 400, 162), // #1444
  INST(Vpshaw           , VexRvmRmv          , V(XOP_M9,99,_,0,x,_,_,_  ), 0                         , 81 , 0  , 400, 162), // #1445
  INST(Vpshlb           , VexRvmRmv          , V(XOP_M9,94,_,0,x,_,_,_  ), 0                         , 81 , 0  , 400, 162), // #1446
  INST(Vpshld           , VexRvmRmv          , V(XOP_M9,96,_,0,x,_,_,_  ), 0                         , 81 , 0  , 400, 162), // #1447
  INST(Vpshldd          , VexRvmi_Lx         , E(660F3A,71,_,x,_,0,4,FV ), 0                         , 110, 0  , 203, 177), // #1448
  INST(Vpshldq          , VexRvmi_Lx         , E(660F3A,71,_,x,_,1,4,FV ), 0                         , 111, 0  , 204, 177), // #1449
  INST(Vpshldvd         , VexRvm_Lx          , E(660F38,71,_,x,_,0,4,FV ), 0                         , 113, 0  , 211, 177), // #1450
  INST(Vpshldvq         , VexRvm_Lx          , E(660F38,71,_,x,_,1,4,FV ), 0                         , 112, 0  , 210, 177), // #1451
  INST(Vpshldvw         , VexRvm_Lx          , E(660F38,70,_,x,_,1,4,FVM), 0                         , 112, 0  , 350, 177), // #1452
  INST(Vpshldw          , VexRvmi_Lx         , E(660F3A,70,_,x,_,1,4,FVM), 0                         , 111, 0  , 274, 177), // #1453
  INST(Vpshlq           , VexRvmRmv          , V(XOP_M9,97,_,0,x,_,_,_  ), 0                         , 81 , 0  , 400, 162), // #1454
  INST(Vpshlw           , VexRvmRmv          , V(XOP_M9,95,_,0,x,_,_,_  ), 0                         , 81 , 0  , 400, 162), // #1455
  INST(Vpshrdd          , VexRvmi_Lx         , E(660F3A,73,_,x,_,0,4,FV ), 0                         , 110, 0  , 203, 177), // #1456
  INST(Vpshrdq          , VexRvmi_Lx         , E(660F3A,73,_,x,_,1,4,FV ), 0                         , 111, 0  , 204, 177), // #1457
  INST(Vpshrdvd         , VexRvm_Lx          , E(660F38,73,_,x,_,0,4,FV ), 0                         , 113, 0  , 211, 177), // #1458
  INST(Vpshrdvq         , VexRvm_Lx          , E(660F38,73,_,x,_,1,4,FV ), 0                         , 112, 0  , 210, 177), // #1459
  INST(Vpshrdvw         , VexRvm_Lx          , E(660F38,72,_,x,_,1,4,FVM), 0                         , 112, 0  , 350, 177), // #1460
  INST(Vpshrdw          , VexRvmi_Lx         , E(660F3A,72,_,x,_,1,4,FVM), 0                         , 111, 0  , 274, 177), // #1461
  INST(Vpshufb          , VexRvm_Lx          , V(660F38,00,_,x,I,I,4,FVM), 0                         , 109, 0  , 309, 172), // #1462
  INST(Vpshufbitqmb     , VexRvm_Lx          , E(660F38,8F,_,x,0,0,4,FVM), 0                         , 113, 0  , 401, 184), // #1463
  INST(Vpshufd          , VexRmi_Lx          , V(660F00,70,_,x,I,0,4,FV ), 0                         , 143, 0  , 402, 153), // #1464
  INST(Vpshufhw         , VexRmi_Lx          , V(F30F00,70,_,x,I,I,4,FVM), 0                         , 160, 0  , 403, 172), // #1465
  INST(Vpshuflw         , VexRmi_Lx          , V(F20F00,70,_,x,I,I,4,FVM), 0                         , 217, 0  , 403, 172), // #1466
  INST(Vpsignb          , VexRvm_Lx          , V(660F38,08,_,x,I,_,_,_  ), 0                         , 30 , 0  , 199, 170), // #1467
  INST(Vpsignd          , VexRvm_Lx          , V(660F38,0A,_,x,I,_,_,_  ), 0                         , 30 , 0  , 199, 170), // #1468
  INST(Vpsignw          , VexRvm_Lx          , V(660F38,09,_,x,I,_,_,_  ), 0                         , 30 , 0  , 199, 170), // #1469
  INST(Vpslld           , VexRvmVmi_Lx_MEvex , V(660F00,F2,_,x,I,0,4,128), V(660F00,72,6,x,I,0,4,FV ), 218, 125, 404, 153), // #1470
  INST(Vpslldq          , VexVmi_Lx_MEvex    , V(660F00,73,7,x,I,I,4,FVM), 0                         , 219, 0  , 405, 172), // #1471
  INST(Vpsllq           , VexRvmVmi_Lx_MEvex , V(660F00,F3,_,x,I,1,4,128), V(660F00,73,6,x,I,1,4,FV ), 220, 126, 406, 153), // #1472
  INST(Vpsllvd          , VexRvm_Lx          , V(660F38,47,_,x,0,0,4,FV ), 0                         , 109, 0  , 206, 163), // #1473
  INST(Vpsllvq          , VexRvm_Lx          , V(660F38,47,_,x,1,1,4,FV ), 0                         , 180, 0  , 205, 163), // #1474
  INST(Vpsllvw          , VexRvm_Lx          , E(660F38,12,_,x,_,1,4,FVM), 0                         , 112, 0  , 350, 159), // #1475
  INST(Vpsllw           , VexRvmVmi_Lx_MEvex , V(660F00,F1,_,x,I,I,4,128), V(660F00,71,6,x,I,I,4,FVM), 218, 127, 407, 172), // #1476
  INST(Vpsrad           , VexRvmVmi_Lx_MEvex , V(660F00,E2,_,x,I,0,4,128), V(660F00,72,4,x,I,0,4,FV ), 218, 128, 404, 153), // #1477
  INST(Vpsraq           , VexRvmVmi_Lx_MEvex , E(660F00,E2,_,x,_,1,4,128), E(660F00,72,4,x,_,1,4,FV ), 221, 129, 408, 148), // #1478
  INST(Vpsravd          , VexRvm_Lx          , V(660F38,46,_,x,0,0,4,FV ), 0                         , 109, 0  , 206, 163), // #1479
  INST(Vpsravq          , VexRvm_Lx          , E(660F38,46,_,x,_,1,4,FV ), 0                         , 112, 0  , 210, 148), // #1480
  INST(Vpsravw          , VexRvm_Lx          , E(660F38,11,_,x,_,1,4,FVM), 0                         , 112, 0  , 350, 159), // #1481
  INST(Vpsraw           , VexRvmVmi_Lx_MEvex , V(660F00,E1,_,x,I,I,4,128), V(660F00,71,4,x,I,I,4,FVM), 218, 130, 407, 172), // #1482
  INST(Vpsrld           , VexRvmVmi_Lx_MEvex , V(660F00,D2,_,x,I,0,4,128), V(660F00,72,2,x,I,0,4,FV ), 218, 131, 404, 153), // #1483
  INST(Vpsrldq          , VexVmi_Lx_MEvex    , V(660F00,73,3,x,I,I,4,FVM), 0                         , 222, 0  , 405, 172), // #1484
  INST(Vpsrlq           , VexRvmVmi_Lx_MEvex , V(660F00,D3,_,x,I,1,4,128), V(660F00,73,2,x,I,1,4,FV ), 220, 132, 406, 153), // #1485
  INST(Vpsrlvd          , VexRvm_Lx          , V(660F38,45,_,x,0,0,4,FV ), 0                         , 109, 0  , 206, 163), // #1486
  INST(Vpsrlvq          , VexRvm_Lx          , V(660F38,45,_,x,1,1,4,FV ), 0                         , 180, 0  , 205, 163), // #1487
  INST(Vpsrlvw          , VexRvm_Lx          , E(660F38,10,_,x,_,1,4,FVM), 0                         , 112, 0  , 350, 159), // #1488
  INST(Vpsrlw           , VexRvmVmi_Lx_MEvex , V(660F00,D1,_,x,I,I,4,128), V(660F00,71,2,x,I,I,4,FVM), 218, 133, 407, 172), // #1489
  INST(Vpsubb           , VexRvm_Lx          , V(660F00,F8,_,x,I,I,4,FVM), 0                         , 143, 0  , 409, 172), // #1490
  INST(Vpsubd           , VexRvm_Lx          , V(660F00,FA,_,x,I,0,4,FV ), 0                         , 143, 0  , 410, 153), // #1491
  INST(Vpsubq           , VexRvm_Lx          , V(660F00,FB,_,x,I,1,4,FV ), 0                         , 102, 0  , 411, 153), // #1492
  INST(Vpsubsb          , VexRvm_Lx          , V(660F00,E8,_,x,I,I,4,FVM), 0                         , 143, 0  , 409, 172), // #1493
  INST(Vpsubsw          , VexRvm_Lx          , V(660F00,E9,_,x,I,I,4,FVM), 0                         , 143, 0  , 409, 172), // #1494
  INST(Vpsubusb         , VexRvm_Lx          , V(660F00,D8,_,x,I,I,4,FVM), 0                         , 143, 0  , 409, 172), // #1495
  INST(Vpsubusw         , VexRvm_Lx          , V(660F00,D9,_,x,I,I,4,FVM), 0                         , 143, 0  , 409, 172), // #1496
  INST(Vpsubw           , VexRvm_Lx          , V(660F00,F9,_,x,I,I,4,FVM), 0                         , 143, 0  , 409, 172), // #1497
  INST(Vpternlogd       , VexRvmi_Lx         , E(660F3A,25,_,x,_,0,4,FV ), 0                         , 110, 0  , 203, 148), // #1498
  INST(Vpternlogq       , VexRvmi_Lx         , E(660F3A,25,_,x,_,1,4,FV ), 0                         , 111, 0  , 204, 148), // #1499
  INST(Vptest           , VexRm_Lx           , V(660F38,17,_,x,I,_,_,_  ), 0                         , 30 , 0  , 295, 176), // #1500
  INST(Vptestmb         , VexRvm_Lx          , E(660F38,26,_,x,_,0,4,FVM), 0                         , 113, 0  , 401, 159), // #1501
  INST(Vptestmd         , VexRvm_Lx          , E(660F38,27,_,x,_,0,4,FV ), 0                         , 113, 0  , 412, 148), // #1502
  INST(Vptestmq         , VexRvm_Lx          , E(660F38,27,_,x,_,1,4,FV ), 0                         , 112, 0  , 413, 148), // #1503
  INST(Vptestmw         , VexRvm_Lx          , E(660F38,26,_,x,_,1,4,FVM), 0                         , 112, 0  , 401, 159), // #1504
  INST(Vptestnmb        , VexRvm_Lx          , E(F30F38,26,_,x,_,0,4,FVM), 0                         , 169, 0  , 401, 159), // #1505
  INST(Vptestnmd        , VexRvm_Lx          , E(F30F38,27,_,x,_,0,4,FV ), 0                         , 169, 0  , 412, 148), // #1506
  INST(Vptestnmq        , VexRvm_Lx          , E(F30F38,27,_,x,_,1,4,FV ), 0                         , 223, 0  , 413, 148), // #1507
  INST(Vptestnmw        , VexRvm_Lx          , E(F30F38,26,_,x,_,1,4,FVM), 0                         , 223, 0  , 401, 159), // #1508
  INST(Vpunpckhbw       , VexRvm_Lx          , V(660F00,68,_,x,I,I,4,FVM), 0                         , 143, 0  , 309, 172), // #1509
  INST(Vpunpckhdq       , VexRvm_Lx          , V(660F00,6A,_,x,I,0,4,FV ), 0                         , 143, 0  , 206, 153), // #1510
  INST(Vpunpckhqdq      , VexRvm_Lx          , V(660F00,6D,_,x,I,1,4,FV ), 0                         , 102, 0  , 205, 153), // #1511
  INST(Vpunpckhwd       , VexRvm_Lx          , V(660F00,69,_,x,I,I,4,FVM), 0                         , 143, 0  , 309, 172), // #1512
  INST(Vpunpcklbw       , VexRvm_Lx          , V(660F00,60,_,x,I,I,4,FVM), 0                         , 143, 0  , 309, 172), // #1513
  INST(Vpunpckldq       , VexRvm_Lx          , V(660F00,62,_,x,I,0,4,FV ), 0                         , 143, 0  , 206, 153), // #1514
  INST(Vpunpcklqdq      , VexRvm_Lx          , V(660F00,6C,_,x,I,1,4,FV ), 0                         , 102, 0  , 205, 153), // #1515
  INST(Vpunpcklwd       , VexRvm_Lx          , V(660F00,61,_,x,I,I,4,FVM), 0                         , 143, 0  , 309, 172), // #1516
  INST(Vpxor            , VexRvm_Lx          , V(660F00,EF,_,x,I,_,_,_  ), 0                         , 71 , 0  , 346, 170), // #1517
  INST(Vpxord           , VexRvm_Lx          , E(660F00,EF,_,x,_,0,4,FV ), 0                         , 192, 0  , 347, 148), // #1518
  INST(Vpxorq           , VexRvm_Lx          , E(660F00,EF,_,x,_,1,4,FV ), 0                         , 134, 0  , 348, 148), // #1519
  INST(Vrangepd         , VexRvmi_Lx         , E(660F3A,50,_,x,_,1,4,FV ), 0                         , 111, 0  , 282, 151), // #1520
  INST(Vrangeps         , VexRvmi_Lx         , E(660F3A,50,_,x,_,0,4,FV ), 0                         , 110, 0  , 283, 151), // #1521
  INST(Vrangesd         , VexRvmi            , E(660F3A,51,_,I,_,1,3,T1S), 0                         , 178, 0  , 284, 151), // #1522
  INST(Vrangess         , VexRvmi            , E(660F3A,51,_,I,_,0,2,T1S), 0                         , 179, 0  , 285, 151), // #1523
  INST(Vrcp14pd         , VexRm_Lx           , E(660F38,4C,_,x,_,1,4,FV ), 0                         , 112, 0  , 343, 148), // #1524
  INST(Vrcp14ps         , VexRm_Lx           , E(660F38,4C,_,x,_,0,4,FV ), 0                         , 113, 0  , 367, 148), // #1525
  INST(Vrcp14sd         , VexRvm             , E(660F38,4D,_,I,_,1,3,T1S), 0                         , 127, 0  , 414, 148), // #1526
  INST(Vrcp14ss         , VexRvm             , E(660F38,4D,_,I,_,0,2,T1S), 0                         , 128, 0  , 415, 148), // #1527
  INST(Vrcpph           , VexRm_Lx           , E(66MAP6,4C,_,_,_,0,4,FV ), 0                         , 181, 0  , 416, 144), // #1528
  INST(Vrcpps           , VexRm_Lx           , V(000F00,53,_,x,I,_,_,_  ), 0                         , 74 , 0  , 295, 145), // #1529
  INST(Vrcpsh           , VexRvm             , E(66MAP6,4D,_,_,_,0,1,T1S), 0                         , 183, 0  , 417, 144), // #1530
  INST(Vrcpss           , VexRvm             , V(F30F00,53,_,I,I,_,_,_  ), 0                         , 193, 0  , 418, 145), // #1531
  INST(Vreducepd        , VexRmi_Lx          , E(660F3A,56,_,x,_,1,4,FV ), 0                         , 111, 0  , 394, 151), // #1532
  INST(Vreduceph        , VexRmi_Lx          , E(000F3A,56,_,_,_,0,4,FV ), 0                         , 122, 0  , 305, 144), // #1533
  INST(Vreduceps        , VexRmi_Lx          , E(660F3A,56,_,x,_,0,4,FV ), 0                         , 110, 0  , 393, 151), // #1534
  INST(Vreducesd        , VexRvmi            , E(660F3A,57,_,I,_,1,3,T1S), 0                         , 178, 0  , 419, 151), // #1535
  INST(Vreducesh        , VexRvmi            , E(000F3A,57,_,_,_,0,1,T1S), 0                         , 186, 0  , 307, 144), // #1536
  INST(Vreducess        , VexRvmi            , E(660F3A,57,_,I,_,0,2,T1S), 0                         , 179, 0  , 420, 151), // #1537
  INST(Vrndscalepd      , VexRmi_Lx          , E(660F3A,09,_,x,_,1,4,FV ), 0                         , 111, 0  , 304, 148), // #1538
  INST(Vrndscaleph      , VexRmi_Lx          , E(000F3A,08,_,_,_,0,4,FV ), 0                         , 122, 0  , 305, 144), // #1539
  INST(Vrndscaleps      , VexRmi_Lx          , E(660F3A,08,_,x,_,0,4,FV ), 0                         , 110, 0  , 306, 148), // #1540
  INST(Vrndscalesd      , VexRvmi            , E(660F3A,0B,_,I,_,1,3,T1S), 0                         , 178, 0  , 284, 148), // #1541
  INST(Vrndscalesh      , VexRvmi            , E(000F3A,0A,_,_,_,0,1,T1S), 0                         , 186, 0  , 307, 144), // #1542
  INST(Vrndscaless      , VexRvmi            , E(660F3A,0A,_,I,_,0,2,T1S), 0                         , 179, 0  , 285, 148), // #1543
  INST(Vroundpd         , VexRmi_Lx          , V(660F3A,09,_,x,I,_,_,_  ), 0                         , 75 , 0  , 421, 145), // #1544
  INST(Vroundps         , VexRmi_Lx          , V(660F3A,08,_,x,I,_,_,_  ), 0                         , 75 , 0  , 421, 145), // #1545
  INST(Vroundsd         , VexRvmi            , V(660F3A,0B,_,I,I,_,_,_  ), 0                         , 75 , 0  , 422, 145), // #1546
  INST(Vroundss         , VexRvmi            , V(660F3A,0A,_,I,I,_,_,_  ), 0                         , 75 , 0  , 423, 145), // #1547
  INST(Vrsqrt14pd       , VexRm_Lx           , E(660F38,4E,_,x,_,1,4,FV ), 0                         , 112, 0  , 343, 148), // #1548
  INST(Vrsqrt14ps       , VexRm_Lx           , E(660F38,4E,_,x,_,0,4,FV ), 0                         , 113, 0  , 367, 148), // #1549
  INST(Vrsqrt14sd       , VexRvm             , E(660F38,4F,_,I,_,1,3,T1S), 0                         , 127, 0  , 414, 148), // #1550
  INST(Vrsqrt14ss       , VexRvm             , E(660F38,4F,_,I,_,0,2,T1S), 0                         , 128, 0  , 415, 148), // #1551
  INST(Vrsqrtph         , VexRm_Lx           , E(66MAP6,4E,_,_,_,0,4,FV ), 0                         , 181, 0  , 416, 144), // #1552
  INST(Vrsqrtps         , VexRm_Lx           , V(000F00,52,_,x,I,_,_,_  ), 0                         , 74 , 0  , 295, 145), // #1553
  INST(Vrsqrtsh         , VexRvm             , E(66MAP6,4F,_,_,_,0,1,T1S), 0                         , 183, 0  , 417, 144), // #1554
  INST(Vrsqrtss         , VexRvm             , V(F30F00,52,_,I,I,_,_,_  ), 0                         , 193, 0  , 418, 145), // #1555
  INST(Vscalefpd        , VexRvm_Lx          , E(660F38,2C,_,x,_,1,4,FV ), 0                         , 112, 0  , 424, 148), // #1556
  INST(Vscalefph        , VexRvm_Lx          , E(66MAP6,2C,_,_,_,0,4,FV ), 0                         , 181, 0  , 194, 144), // #1557
  INST(Vscalefps        , VexRvm_Lx          , E(660F38,2C,_,x,_,0,4,FV ), 0                         , 113, 0  , 281, 148), // #1558
  INST(Vscalefsd        , VexRvm             , E(660F38,2D,_,I,_,1,3,T1S), 0                         , 127, 0  , 250, 148), // #1559
  INST(Vscalefsh        , VexRvm             , E(66MAP6,2D,_,_,_,0,1,T1S), 0                         , 183, 0  , 197, 144), // #1560
  INST(Vscalefss        , VexRvm             , E(660F38,2D,_,I,_,0,2,T1S), 0                         , 128, 0  , 258, 148), // #1561
  INST(Vscatterdpd      , VexMr_VM           , E(660F38,A2,_,x,_,1,3,T1S), 0                         , 127, 0  , 397, 148), // #1562
  INST(Vscatterdps      , VexMr_VM           , E(660F38,A2,_,x,_,0,2,T1S), 0                         , 128, 0  , 396, 148), // #1563
  INST(Vscatterqpd      , VexMr_VM           , E(660F38,A3,_,x,_,1,3,T1S), 0                         , 127, 0  , 399, 148), // #1564
  INST(Vscatterqps      , VexMr_VM           , E(660F38,A3,_,x,_,0,2,T1S), 0                         , 128, 0  , 398, 148), // #1565
  INST(Vsha512msg1      , VexRm              , V(F20F38,CC,_,1,0,_,_,_  ), 0                         , 224, 0  , 425, 186), // #1566
  INST(Vsha512msg2      , VexRm              , V(F20F38,CD,_,1,0,_,_,_  ), 0                         , 224, 0  , 426, 186), // #1567
  INST(Vsha512rnds2     , VexRvm             , V(F20F38,CB,_,1,0,_,_,_  ), 0                         , 224, 0  , 427, 186), // #1568
  INST(Vshuff32x4       , VexRvmi_Lx         , E(660F3A,23,_,x,_,0,4,FV ), 0                         , 110, 0  , 428, 148), // #1569
  INST(Vshuff64x2       , VexRvmi_Lx         , E(660F3A,23,_,x,_,1,4,FV ), 0                         , 111, 0  , 429, 148), // #1570
  INST(Vshufi32x4       , VexRvmi_Lx         , E(660F3A,43,_,x,_,0,4,FV ), 0                         , 110, 0  , 428, 148), // #1571
  INST(Vshufi64x2       , VexRvmi_Lx         , E(660F3A,43,_,x,_,1,4,FV ), 0                         , 111, 0  , 429, 148), // #1572
  INST(Vshufpd          , VexRvmi_Lx         , V(660F00,C6,_,x,I,1,4,FV ), 0                         , 102, 0  , 430, 143), // #1573
  INST(Vshufps          , VexRvmi_Lx         , V(000F00,C6,_,x,I,0,4,FV ), 0                         , 104, 0  , 431, 143), // #1574
  INST(Vsm3msg1         , VexRvm             , V(000F38,DA,_,0,0,_,_,_  ), 0                         , 11 , 0  , 432, 187), // #1575
  INST(Vsm3msg2         , VexRvm             , V(660F38,DA,_,0,0,_,_,_  ), 0                         , 30 , 0  , 432, 187), // #1576
  INST(Vsm3rnds2        , VexRvmi            , V(660F3A,DE,_,0,0,_,_,_  ), 0                         , 75 , 0  , 275, 187), // #1577
  INST(Vsm4key4         , VexRvm_Lx          , V(F30F38,DA,_,x,0,_,_,_  ), 0                         , 89 , 0  , 199, 188), // #1578
  INST(Vsm4rnds4        , VexRvm_Lx          , V(F20F38,DA,_,x,0,_,_,_  ), 0                         , 85 , 0  , 199, 188), // #1579
  INST(Vsqrtpd          , VexRm_Lx           , V(660F00,51,_,x,I,1,4,FV ), 0                         , 102, 0  , 433, 143), // #1580
  INST(Vsqrtph          , VexRm_Lx           , E(00MAP5,51,_,_,_,0,4,FV ), 0                         , 103, 0  , 245, 144), // #1581
  INST(Vsqrtps          , VexRm_Lx           , V(000F00,51,_,x,I,0,4,FV ), 0                         , 104, 0  , 233, 143), // #1582
  INST(Vsqrtsd          , VexRvm             , V(F20F00,51,_,I,I,1,3,T1S), 0                         , 105, 0  , 196, 143), // #1583
  INST(Vsqrtsh          , VexRvm             , E(F3MAP5,51,_,_,_,0,1,T1S), 0                         , 106, 0  , 197, 144), // #1584
  INST(Vsqrtss          , VexRvm             , V(F30F00,51,_,I,I,0,2,T1S), 0                         , 107, 0  , 198, 143), // #1585
  INST(Vstmxcsr         , VexM               , V(000F00,AE,3,0,I,_,_,_  ), 0                         , 225, 0  , 314, 145), // #1586
  INST(Vsubpd           , VexRvm_Lx          , V(660F00,5C,_,x,I,1,4,FV ), 0                         , 102, 0  , 193, 143), // #1587
  INST(Vsubph           , VexRvm_Lx          , E(00MAP5,5C,_,_,_,0,4,FV ), 0                         , 103, 0  , 194, 144), // #1588
  INST(Vsubps           , VexRvm_Lx          , V(000F00,5C,_,x,I,0,4,FV ), 0                         , 104, 0  , 195, 143), // #1589
  INST(Vsubsd           , VexRvm             , V(F20F00,5C,_,I,I,1,3,T1S), 0                         , 105, 0  , 196, 143), // #1590
  INST(Vsubsh           , VexRvm             , E(F3MAP5,5C,_,_,_,0,1,T1S), 0                         , 106, 0  , 197, 144), // #1591
  INST(Vsubss           , VexRvm             , V(F30F00,5C,_,I,I,0,2,T1S), 0                         , 107, 0  , 198, 143), // #1592
  INST(Vtestpd          , VexRm_Lx           , V(660F38,0F,_,x,0,_,_,_  ), 0                         , 30 , 0  , 295, 176), // #1593
  INST(Vtestps          , VexRm_Lx           , V(660F38,0E,_,x,0,_,_,_  ), 0                         , 30 , 0  , 295, 176), // #1594
  INST(Vucomisd         , VexRm              , V(660F00,2E,_,I,I,1,3,T1S), 0                         , 124, 0  , 227, 154), // #1595
  INST(Vucomish         , VexRm              , E(00MAP5,2E,_,_,_,0,1,T1S), 0                         , 125, 0  , 228, 155), // #1596
  INST(Vucomiss         , VexRm              , V(000F00,2E,_,I,I,0,2,T1S), 0                         , 126, 0  , 229, 154), // #1597
  INST(Vunpckhpd        , VexRvm_Lx          , V(660F00,15,_,x,I,1,4,FV ), 0                         , 102, 0  , 205, 143), // #1598
  INST(Vunpckhps        , VexRvm_Lx          , V(000F00,15,_,x,I,0,4,FV ), 0                         , 104, 0  , 206, 143), // #1599
  INST(Vunpcklpd        , VexRvm_Lx          , V(660F00,14,_,x,I,1,4,FV ), 0                         , 102, 0  , 205, 143), // #1600
  INST(Vunpcklps        , VexRvm_Lx          , V(000F00,14,_,x,I,0,4,FV ), 0                         , 104, 0  , 206, 143), // #1601
  INST(Vxorpd           , VexRvm_Lx          , V(660F00,57,_,x,I,1,4,FV ), 0                         , 102, 0  , 411, 149), // #1602
  INST(Vxorps           , VexRvm_Lx          , V(000F00,57,_,x,I,0,4,FV ), 0                         , 104, 0  , 410, 149), // #1603
  INST(Vzeroall         , VexOp              , V(000F00,77,_,1,I,_,_,_  ), 0                         , 70 , 0  , 434, 145), // #1604
  INST(Vzeroupper       , VexOp              , V(000F00,77,_,0,I,_,_,_  ), 0                         , 74 , 0  , 434, 145), // #1605
  INST(Wbinvd           , X86Op              , O(000F00,09,_,_,_,_,_,_  ), 0                         , 5  , 0  , 26 , 44 ), // #1606
  INST(Wbnoinvd         , X86Op              , O(F30F00,09,_,_,_,_,_,_  ), 0                         , 7  , 0  , 26 , 189), // #1607
  INST(Wrfsbase         , X86M               , O(F30F00,AE,2,_,x,_,_,_  ), 0                         , 226, 0  , 171, 121), // #1608
  INST(Wrgsbase         , X86M               , O(F30F00,AE,3,_,x,_,_,_  ), 0                         , 227, 0  , 171, 121), // #1609
  INST(Wrmsr            , X86Op              , O(000F00,30,_,_,_,_,_,_  ), 0                         , 5  , 0  , 174, 190), // #1610
  INST(Wrssd            , X86Mr              , O(000F38,F6,_,_,_,_,_,_  ), 0                         , 1  , 0  , 435, 64 ), // #1611
  INST(Wrssq            , X86Mr              , O(000F38,F6,_,_,1,_,_,_  ), 0                         , 228, 0  , 436, 64 ), // #1612
  INST(Wrussd           , X86Mr              , O(660F38,F5,_,_,_,_,_,_  ), 0                         , 2  , 0  , 435, 64 ), // #1613
  INST(Wrussq           , X86Mr              , O(660F38,F5,_,_,1,_,_,_  ), 0                         , 229, 0  , 436, 64 ), // #1614
  INST(Xabort           , X86Op_Mod11RM_I8   , O(000000,C6,7,_,_,_,_,_  ), 0                         , 29 , 0  , 77 , 191), // #1615
  INST(Xadd             , X86Xadd            , O(000F00,C0,_,_,x,_,_,_  ), 0                         , 5  , 0  , 437, 39 ), // #1616
  INST(Xbegin           , X86JmpRel          , O(000000,C7,7,_,_,_,_,_  ), 0                         , 29 , 0  , 438, 191), // #1617
  INST(Xchg             , X86Xchg            , O(000000,86,_,_,x,_,_,_  ), 0                         , 0  , 0  , 439, 0  ), // #1618
  INST(Xend             , X86Op              , O(000F01,D5,_,_,_,_,_,_  ), 0                         , 23 , 0  , 26 , 191), // #1619
  INST(Xgetbv           , X86Op              , O(000F01,D0,_,_,_,_,_,_  ), 0                         , 23 , 0  , 174, 192), // #1620
  INST(Xlatb            , X86Op              , O(000000,D7,_,_,_,_,_,_  ), 0                         , 0  , 0  , 26 , 0  ), // #1621
  INST(Xor              , X86Arith           , O(000000,30,6,_,x,_,_,_  ), 0                         , 34 , 0  , 178, 1  ), // #1622
  INST(Xorpd            , ExtRm              , O(660F00,57,_,_,_,_,_,_  ), 0                         , 4  , 0  , 148, 5  ), // #1623
  INST(Xorps            , ExtRm              , O(000F00,57,_,_,_,_,_,_  ), 0                         , 5  , 0  , 148, 6  ), // #1624
  INST(Xresldtrk        , X86Op              , O(F20F01,E9,_,_,_,_,_,_  ), 0                         , 93 , 0  , 26 , 193), // #1625
  INST(Xrstor           , X86M_Only_EDX_EAX  , O(000F00,AE,5,_,_,_,_,_  ), 0                         , 79 , 0  , 440, 192), // #1626
  INST(Xrstor64         , X86M_Only_EDX_EAX  , O(000F00,AE,5,_,1,_,_,_  ), 0                         , 230, 0  , 441, 192), // #1627
  INST(Xrstors          , X86M_Only_EDX_EAX  , O(000F00,C7,3,_,_,_,_,_  ), 0                         , 80 , 0  , 440, 194), // #1628
  INST(Xrstors64        , X86M_Only_EDX_EAX  , O(000F00,C7,3,_,1,_,_,_  ), 0                         , 231, 0  , 441, 194), // #1629
  INST(Xsave            , X86M_Only_EDX_EAX  , O(000F00,AE,4,_,_,_,_,_  ), 0                         , 98 , 0  , 440, 192), // #1630
  INST(Xsave64          , X86M_Only_EDX_EAX  , O(000F00,AE,4,_,1,_,_,_  ), 0                         , 232, 0  , 441, 192), // #1631
  INST(Xsavec           , X86M_Only_EDX_EAX  , O(000F00,C7,4,_,_,_,_,_  ), 0                         , 98 , 0  , 440, 195), // #1632
  INST(Xsavec64         , X86M_Only_EDX_EAX  , O(000F00,C7,4,_,1,_,_,_  ), 0                         , 232, 0  , 441, 195), // #1633
  INST(Xsaveopt         , X86M_Only_EDX_EAX  , O(000F00,AE,6,_,_,_,_,_  ), 0                         , 82 , 0  , 440, 196), // #1634
  INST(Xsaveopt64       , X86M_Only_EDX_EAX  , O(000F00,AE,6,_,1,_,_,_  ), 0                         , 233, 0  , 441, 196), // #1635
  INST(Xsaves           , X86M_Only_EDX_EAX  , O(000F00,C7,5,_,_,_,_,_  ), 0                         , 79 , 0  , 440, 194), // #1636
  INST(Xsaves64         , X86M_Only_EDX_EAX  , O(000F00,C7,5,_,1,_,_,_  ), 0                         , 230, 0  , 441, 194), // #1637
  INST(Xsetbv           , X86Op              , O(000F01,D1,_,_,_,_,_,_  ), 0                         , 23 , 0  , 174, 192), // #1638
  INST(Xsusldtrk        , X86Op              , O(F20F01,E8,_,_,_,_,_,_  ), 0                         , 93 , 0  , 26 , 193), // #1639
  INST(Xtest            , X86Op              , O(000F01,D6,_,_,_,_,_,_  ), 0                         , 23 , 0  , 26 , 197)  // #1640
  // ${InstInfo:End}
};
#undef NAME_DATA_INDEX
#undef INST

// x86::InstDB - Opcode Tables
// ===========================

// ${MainOpcodeTable:Begin}
// ------------------- Automatically generated, do not edit -------------------
const uint32_t InstDB::main_opcode_table[] = {
  O(000000,00,0,0,0,0,0,0   ), // #0 [ref=56x]
  O(000F38,00,0,0,0,0,0,0   ), // #1 [ref=25x]
  O(660F38,00,0,0,0,0,0,0   ), // #2 [ref=44x]
  O(000000,00,2,0,0,0,0,0   ), // #3 [ref=4x]
  O(660F00,00,0,0,0,0,0,0   ), // #4 [ref=37x]
  O(000F00,00,0,0,0,0,0,0   ), // #5 [ref=187x]
  O(F20F00,00,0,0,0,0,0,0   ), // #6 [ref=22x]
  O(F30F00,00,0,0,0,0,0,0   ), // #7 [ref=27x]
  O(F30F38,00,0,0,0,0,0,0   ), // #8 [ref=3x]
  O(660F3A,00,0,0,0,0,0,0   ), // #9 [ref=22x]
  O(000000,00,4,0,0,0,0,0   ), // #10 [ref=4x]
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
  O(000000,00,6,0,0,0,0,0   ), // #34 [ref=4x]
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
  V(660F00,00,0,0,0,1,4,ByLL), // #102 [ref=25x]
  E(00MAP5,00,0,0,0,0,4,ByLL), // #103 [ref=10x]
  V(000F00,00,0,0,0,0,4,ByLL), // #104 [ref=19x]
  V(F20F00,00,0,0,0,1,3,None), // #105 [ref=10x]
  E(F3MAP5,00,0,0,0,0,1,None), // #106 [ref=13x]
  V(F30F00,00,0,0,0,0,2,None), // #107 [ref=12x]
  V(F20F00,00,0,0,0,0,0,None), // #108 [ref=4x]
  V(660F38,00,0,0,0,0,4,ByLL), // #109 [ref=50x]
  E(660F3A,00,0,0,0,0,4,ByLL), // #110 [ref=17x]
  E(660F3A,00,0,0,0,1,4,ByLL), // #111 [ref=18x]
  E(660F38,00,0,0,0,1,4,ByLL), // #112 [ref=38x]
  E(660F38,00,0,0,0,0,4,ByLL), // #113 [ref=25x]
  V(660F38,00,0,1,0,0,0,None), // #114 [ref=2x]
  E(660F38,00,0,0,0,0,3,None), // #115 [ref=2x]
  E(660F38,00,0,0,0,0,4,None), // #116 [ref=2x]
  E(660F38,00,0,2,0,0,5,None), // #117 [ref=2x]
  E(660F38,00,0,0,0,1,4,None), // #118 [ref=2x]
  E(660F38,00,0,2,0,1,5,None), // #119 [ref=2x]
  V(660F38,00,0,0,0,1,3,None), // #120 [ref=2x]
  V(660F38,00,0,0,0,0,2,None), // #121 [ref=14x]
  E(000F3A,00,0,0,0,0,4,ByLL), // #122 [ref=5x]
  E(F30F3A,00,0,0,0,0,1,None), // #123 [ref=1x]
  V(660F00,00,0,0,0,1,3,None), // #124 [ref=5x]
  E(00MAP5,00,0,0,0,0,1,None), // #125 [ref=2x]
  V(000F00,00,0,0,0,0,2,None), // #126 [ref=2x]
  E(660F38,00,0,0,0,1,3,None), // #127 [ref=12x]
  E(660F38,00,0,0,0,0,2,None), // #128 [ref=12x]
  V(F30F00,00,0,0,0,0,3,ByLL), // #129 [ref=1x]
  E(F20F38,00,0,0,0,0,4,ByLL), // #130 [ref=2x]
  V(F30F38,00,0,0,0,0,4,ByLL), // #131 [ref=1x]
  V(F20F00,00,0,0,0,1,4,ByLL), // #132 [ref=1x]
  E(66MAP5,00,0,0,0,1,4,ByLL), // #133 [ref=1x]
  E(660F00,00,0,0,0,1,4,ByLL), // #134 [ref=10x]
  E(000F00,00,0,0,0,1,4,ByLL), // #135 [ref=3x]
  E(66MAP5,00,0,0,0,0,3,ByLL), // #136 [ref=1x]
  E(00MAP5,00,0,0,0,0,2,ByLL), // #137 [ref=1x]
  V(660F38,00,0,0,0,0,3,ByLL), // #138 [ref=7x]
  E(66MAP6,00,0,0,0,0,3,ByLL), // #139 [ref=1x]
  E(66MAP5,00,0,0,0,0,2,ByLL), // #140 [ref=4x]
  E(00MAP5,00,0,0,0,0,3,ByLL), // #141 [ref=2x]
  E(66MAP5,00,0,0,0,0,4,ByLL), // #142 [ref=3x]
  V(660F00,00,0,0,0,0,4,ByLL), // #143 [ref=43x]
  V(000F00,00,0,0,0,0,3,ByLL), // #144 [ref=1x]
  V(660F3A,00,0,0,0,0,3,ByLL), // #145 [ref=1x]
  E(660F00,00,0,0,0,0,3,ByLL), // #146 [ref=4x]
  E(000F00,00,0,0,0,0,4,ByLL), // #147 [ref=2x]
  E(F30F00,00,0,0,0,1,4,ByLL), // #148 [ref=3x]
  E(00MAP5,00,0,0,0,1,4,ByLL), // #149 [ref=1x]
  E(F2MAP5,00,0,0,0,1,3,None), // #150 [ref=1x]
  V(F20F00,00,0,0,0,0,3,None), // #151 [ref=2x]
  E(F20F00,00,0,0,0,0,3,None), // #152 [ref=2x]
  E(00MAP6,00,0,0,0,0,1,None), // #153 [ref=1x]
  V(F20F00,00,0,0,0,0,2,T1W ), // #154 [ref=1x]
  E(F3MAP5,00,0,0,0,0,2,T1W ), // #155 [ref=2x]
  V(F30F00,00,0,0,0,0,2,T1W ), // #156 [ref=1x]
  E(00MAP5,00,0,0,0,0,2,None), // #157 [ref=1x]
  E(F30F00,00,0,0,0,0,2,None), // #158 [ref=2x]
  E(F3MAP5,00,0,0,0,0,3,ByLL), // #159 [ref=1x]
  V(F30F00,00,0,0,0,0,4,ByLL), // #160 [ref=4x]
  E(F30F00,00,0,0,0,0,3,ByLL), // #161 [ref=1x]
  E(F2MAP5,00,0,0,0,0,4,ByLL), // #162 [ref=2x]
  E(F20F00,00,0,0,0,0,4,ByLL), // #163 [ref=2x]
  E(F2MAP5,00,0,0,0,1,4,ByLL), // #164 [ref=1x]
  E(F20F00,00,0,0,0,1,4,ByLL), // #165 [ref=2x]
  E(F20F00,00,0,0,0,0,2,T1W ), // #166 [ref=1x]
  E(F30F00,00,0,0,0,0,2,T1W ), // #167 [ref=1x]
  E(F3MAP5,00,0,0,0,0,4,ByLL), // #168 [ref=1x]
  E(F30F38,00,0,0,0,0,4,ByLL), // #169 [ref=3x]
  V(660F3A,00,0,1,0,0,0,None), // #170 [ref=6x]
  E(660F3A,00,0,0,0,0,4,None), // #171 [ref=4x]
  E(660F3A,00,0,2,0,0,5,None), // #172 [ref=4x]
  E(660F3A,00,0,0,0,1,4,None), // #173 [ref=4x]
  E(660F3A,00,0,2,0,1,5,None), // #174 [ref=4x]
  V(660F3A,00,0,0,0,0,2,None), // #175 [ref=4x]
  E(F2MAP6,00,0,0,0,0,4,ByLL), // #176 [ref=2x]
  E(F2MAP6,00,0,0,0,0,2,None), // #177 [ref=2x]
  E(660F3A,00,0,0,0,1,3,None), // #178 [ref=6x]
  E(660F3A,00,0,0,0,0,2,None), // #179 [ref=6x]
  V(660F38,00,0,0,1,1,4,ByLL), // #180 [ref=22x]
  E(66MAP6,00,0,0,0,0,4,ByLL), // #181 [ref=22x]
  V(660F38,00,0,0,1,1,3,None), // #182 [ref=12x]
  E(66MAP6,00,0,0,0,0,1,None), // #183 [ref=16x]
  E(F3MAP6,00,0,0,0,0,4,ByLL), // #184 [ref=2x]
  E(F3MAP6,00,0,0,0,0,2,None), // #185 [ref=2x]
  E(000F3A,00,0,0,0,0,1,None), // #186 [ref=4x]
  V(660F38,00,0,0,1,0,0,None), // #187 [ref=5x]
  V(660F3A,00,0,0,1,1,4,ByLL), // #188 [ref=2x]
  V(000F00,00,2,0,0,0,0,None), // #189 [ref=1x]
  V(660F00,00,0,0,0,0,2,None), // #190 [ref=1x]
  V(F20F00,00,0,0,0,1,3,DUP ), // #191 [ref=1x]
  E(660F00,00,0,0,0,0,4,ByLL), // #192 [ref=6x]
  V(F30F00,00,0,0,0,0,0,None), // #193 [ref=3x]
  E(F30F00,00,0,0,0,0,4,ByLL), // #194 [ref=1x]
  V(000F00,00,0,0,0,0,3,None), // #195 [ref=2x]
  E(66MAP5,00,0,0,0,0,1,None), // #196 [ref=1x]
  E(F20F38,00,0,0,0,1,4,ByLL), // #197 [ref=1x]
  V(660F3A,00,0,0,0,0,4,ByLL), // #198 [ref=2x]
  E(F30F38,00,0,0,0,1,0,None), // #199 [ref=5x]
  E(F30F38,00,0,0,0,0,0,None), // #200 [ref=5x]
  V(660F38,00,0,0,0,0,1,None), // #201 [ref=1x]
  V(XOP_M8,00,0,0,0,0,0,None), // #202 [ref=22x]
  V(660F38,00,0,0,0,1,4,ByLL), // #203 [ref=4x]
  E(660F38,00,0,0,0,0,0,None), // #204 [ref=2x]
  E(660F38,00,0,0,0,1,1,None), // #205 [ref=2x]
  E(660F38,00,0,0,1,1,4,ByLL), // #206 [ref=1x]
  V(660F3A,00,0,0,1,1,3,None), // #207 [ref=2x]
  V(660F3A,00,0,0,0,0,1,None), // #208 [ref=1x]
  V(660F00,00,0,0,0,0,1,None), // #209 [ref=1x]
  E(F30F38,00,0,0,0,0,2,ByLL), // #210 [ref=6x]
  E(F30F38,00,0,0,0,0,3,ByLL), // #211 [ref=9x]
  E(F30F38,00,0,0,0,0,1,ByLL), // #212 [ref=3x]
  V(660F38,00,0,0,0,0,2,ByLL), // #213 [ref=4x]
  V(660F38,00,0,0,0,0,1,ByLL), // #214 [ref=2x]
  E(660F00,00,1,0,0,0,4,ByLL), // #215 [ref=1x]
  E(660F00,00,1,0,0,1,4,ByLL), // #216 [ref=1x]
  V(F20F00,00,0,0,0,0,4,ByLL), // #217 [ref=1x]
  V(660F00,00,0,0,0,0,4,None), // #218 [ref=6x]
  V(660F00,00,7,0,0,0,4,ByLL), // #219 [ref=1x]
  V(660F00,00,0,0,0,1,4,None), // #220 [ref=2x]
  E(660F00,00,0,0,0,1,4,None), // #221 [ref=1x]
  V(660F00,00,3,0,0,0,4,ByLL), // #222 [ref=1x]
  E(F30F38,00,0,0,0,1,4,ByLL), // #223 [ref=2x]
  V(F20F38,00,0,1,0,0,0,None), // #224 [ref=3x]
  V(000F00,00,3,0,0,0,0,None), // #225 [ref=1x]
  O(F30F00,00,2,0,0,0,0,0   ), // #226 [ref=1x]
  O(F30F00,00,3,0,0,0,0,0   ), // #227 [ref=1x]
  O(000F38,00,0,0,1,0,0,0   ), // #228 [ref=1x]
  O(660F38,00,0,0,1,0,0,0   ), // #229 [ref=1x]
  O(000F00,00,5,0,1,0,0,0   ), // #230 [ref=2x]
  O(000F00,00,3,0,1,0,0,0   ), // #231 [ref=1x]
  O(000F00,00,4,0,1,0,0,0   ), // #232 [ref=2x]
  O(000F00,00,6,0,1,0,0,0   )  // #233 [ref=1x]
};
// ----------------------------------------------------------------------------
// ${MainOpcodeTable:End}

// ${AltOpcodeTable:Begin}
// ------------------- Automatically generated, do not edit -------------------
const uint32_t InstDB::alt_opcode_table[] = {
  O(000000,00,0,0,0,0,0,0   ), // #0 [ref=1506x]
  O(000F00,BA,4,0,0,0,0,0   ), // #1 [ref=1x]
  O(000F00,BA,7,0,0,0,0,0   ), // #2 [ref=1x]
  O(000F00,BA,6,0,0,0,0,0   ), // #3 [ref=1x]
  O(000F00,BA,5,0,0,0,0,0   ), // #4 [ref=1x]
  O(000000,48,0,0,0,0,0,0   ), // #5 [ref=1x]
  O(660F00,78,0,0,0,0,0,0   ), // #6 [ref=1x]
  O_FPU(00,00DF,5)           , // #7 [ref=1x]
  O_FPU(00,00DF,7)           , // #8 [ref=1x]
  O_FPU(00,00DD,1)           , // #9 [ref=1x]
  O_FPU(00,00DB,5)           , // #10 [ref=1x]
  O_FPU(00,DFE0,0)           , // #11 [ref=1x]
  O(000000,DB,7,0,0,0,0,0   ), // #12 [ref=1x]
  O_FPU(9B,DFE0,0)           , // #13 [ref=1x]
  O(000000,E4,0,0,0,0,0,0   ), // #14 [ref=1x]
  O(000000,40,0,0,0,0,0,0   ), // #15 [ref=1x]
  O(F20F00,78,0,0,0,0,0,0   ), // #16 [ref=1x]
  O(000000,72,0,0,0,0,0,0   ), // #17 [ref=1x]
  O(000000,76,0,0,0,0,0,0   ), // #18 [ref=1x]
  O(000000,E3,0,0,0,0,0,0   ), // #19 [ref=1x]
  O(000000,7C,0,0,0,0,0,0   ), // #20 [ref=1x]
  O(000000,7E,0,0,0,0,0,0   ), // #21 [ref=1x]
  O(000000,EB,0,0,0,0,0,0   ), // #22 [ref=1x]
  O(000000,73,0,0,0,0,0,0   ), // #23 [ref=1x]
  O(000000,77,0,0,0,0,0,0   ), // #24 [ref=1x]
  O(000000,7D,0,0,0,0,0,0   ), // #25 [ref=1x]
  O(000000,7F,0,0,0,0,0,0   ), // #26 [ref=1x]
  O(000000,71,0,0,0,0,0,0   ), // #27 [ref=1x]
  O(000000,7B,0,0,0,0,0,0   ), // #28 [ref=1x]
  O(000000,79,0,0,0,0,0,0   ), // #29 [ref=1x]
  O(000000,75,0,0,0,0,0,0   ), // #30 [ref=1x]
  O(000000,70,0,0,0,0,0,0   ), // #31 [ref=1x]
  O(000000,7A,0,0,0,0,0,0   ), // #32 [ref=1x]
  O(000000,78,0,0,0,0,0,0   ), // #33 [ref=1x]
  O(000000,74,0,0,0,0,0,0   ), // #34 [ref=1x]
  V(660F00,92,0,0,0,0,0,None), // #35 [ref=1x]
  V(F20F00,92,0,0,0,0,0,None), // #36 [ref=1x]
  V(F20F00,92,0,0,1,0,0,None), // #37 [ref=1x]
  V(000F00,92,0,0,0,0,0,None), // #38 [ref=1x]
  O(000000,9A,0,0,0,0,0,0   ), // #39 [ref=1x]
  O(000000,EA,0,0,0,0,0,0   ), // #40 [ref=1x]
  O(000000,E2,0,0,0,0,0,0   ), // #41 [ref=1x]
  O(000000,E1,0,0,0,0,0,0   ), // #42 [ref=1x]
  O(000000,E0,0,0,0,0,0,0   ), // #43 [ref=1x]
  O(660F00,29,0,0,0,0,0,0   ), // #44 [ref=1x]
  O(000F00,29,0,0,0,0,0,0   ), // #45 [ref=1x]
  O(000F38,F1,0,0,0,0,0,0   ), // #46 [ref=1x]
  O(000F00,7E,0,0,0,0,0,0   ), // #47 [ref=2x]
  O(660F00,7F,0,0,0,0,0,0   ), // #48 [ref=1x]
  O(F30F00,7F,0,0,0,0,0,0   ), // #49 [ref=1x]
  O(660F00,17,0,0,0,0,0,0   ), // #50 [ref=1x]
  O(000F00,17,0,0,0,0,0,0   ), // #51 [ref=1x]
  O(660F00,13,0,0,0,0,0,0   ), // #52 [ref=1x]
  O(000F00,13,0,0,0,0,0,0   ), // #53 [ref=1x]
  O(660F00,E7,0,0,0,0,0,0   ), // #54 [ref=1x]
  O(660F00,2B,0,0,0,0,0,0   ), // #55 [ref=1x]
  O(000F00,2B,0,0,0,0,0,0   ), // #56 [ref=1x]
  O(000F00,E7,0,0,0,0,0,0   ), // #57 [ref=1x]
  O(F20F00,2B,0,0,0,0,0,0   ), // #58 [ref=1x]
  O(F30F00,2B,0,0,0,0,0,0   ), // #59 [ref=1x]
  O(F20F00,11,0,0,0,0,0,0   ), // #60 [ref=1x]
  O(F30F00,11,0,0,0,0,0,0   ), // #61 [ref=1x]
  O(660F00,11,0,0,0,0,0,0   ), // #62 [ref=1x]
  O(000F00,11,0,0,0,0,0,0   ), // #63 [ref=1x]
  O(000000,E6,0,0,0,0,0,0   ), // #64 [ref=1x]
  O(000F3A,15,0,0,0,0,0,0   ), // #65 [ref=1x]
  O(000000,58,0,0,0,0,0,0   ), // #66 [ref=1x]
  O(000F00,72,6,0,0,0,0,0   ), // #67 [ref=1x]
  O(660F00,73,7,0,0,0,0,0   ), // #68 [ref=1x]
  O(000F00,73,6,0,0,0,0,0   ), // #69 [ref=1x]
  O(000F00,71,6,0,0,0,0,0   ), // #70 [ref=1x]
  O(000F00,72,4,0,0,0,0,0   ), // #71 [ref=1x]
  O(000F00,71,4,0,0,0,0,0   ), // #72 [ref=1x]
  O(000F00,72,2,0,0,0,0,0   ), // #73 [ref=1x]
  O(660F00,73,3,0,0,0,0,0   ), // #74 [ref=1x]
  O(000F00,73,2,0,0,0,0,0   ), // #75 [ref=1x]
  O(000F00,71,2,0,0,0,0,0   ), // #76 [ref=1x]
  O(000000,50,0,0,0,0,0,0   ), // #77 [ref=2x]
  O(000000,F6,0,0,0,0,0,0   ), // #78 [ref=1x]
  E(660F38,92,0,0,0,1,3,None), // #79 [ref=1x]
  E(660F38,92,0,0,0,0,2,None), // #80 [ref=1x]
  E(660F38,93,0,0,0,1,3,None), // #81 [ref=1x]
  E(660F38,93,0,0,0,0,2,None), // #82 [ref=1x]
  V(660F38,2F,0,0,0,0,0,None), // #83 [ref=1x]
  V(660F38,2E,0,0,0,0,0,None), // #84 [ref=1x]
  V(660F00,29,0,0,0,1,4,ByLL), // #85 [ref=1x]
  V(000F00,29,0,0,0,0,4,ByLL), // #86 [ref=1x]
  V(660F00,7E,0,0,0,0,2,None), // #87 [ref=1x]
  V(660F00,7F,0,0,0,0,0,None), // #88 [ref=1x]
  E(660F00,7F,0,0,0,0,4,ByLL), // #89 [ref=1x]
  E(660F00,7F,0,0,0,1,4,ByLL), // #90 [ref=1x]
  V(F30F00,7F,0,0,0,0,0,None), // #91 [ref=1x]
  E(F20F00,7F,0,0,0,1,4,ByLL), // #92 [ref=1x]
  E(F30F00,7F,0,0,0,0,4,ByLL), // #93 [ref=1x]
  E(F30F00,7F,0,0,0,1,4,ByLL), // #94 [ref=1x]
  E(F20F00,7F,0,0,0,0,4,ByLL), // #95 [ref=1x]
  V(660F00,17,0,0,0,1,3,None), // #96 [ref=1x]
  V(000F00,17,0,0,0,0,3,None), // #97 [ref=1x]
  V(660F00,13,0,0,0,1,3,None), // #98 [ref=1x]
  V(000F00,13,0,0,0,0,3,None), // #99 [ref=1x]
  V(660F00,7E,0,0,0,1,3,None), // #100 [ref=1x]
  V(F20F00,11,0,0,0,1,3,None), // #101 [ref=1x]
  E(F3MAP5,11,0,0,0,0,1,None), // #102 [ref=1x]
  V(F30F00,11,0,0,0,0,2,None), // #103 [ref=1x]
  V(660F00,11,0,0,0,1,4,ByLL), // #104 [ref=1x]
  V(000F00,11,0,0,0,0,4,ByLL), // #105 [ref=1x]
  E(66MAP5,7E,0,0,0,0,1,None), // #106 [ref=1x]
  E(660F38,7A,0,0,0,0,0,None), // #107 [ref=1x]
  E(660F38,7C,0,0,0,0,0,None), // #108 [ref=1x]
  E(660F38,7C,0,0,0,1,0,None), // #109 [ref=1x]
  E(660F38,7B,0,0,0,0,0,None), // #110 [ref=1x]
  V(660F3A,05,0,0,0,1,4,ByLL), // #111 [ref=1x]
  V(660F3A,04,0,0,0,0,4,ByLL), // #112 [ref=1x]
  V(660F3A,01,0,0,1,1,4,ByLL), // #113 [ref=1x]
  V(660F3A,00,0,0,1,1,4,ByLL), // #114 [ref=1x]
  E(660F38,90,0,0,0,0,2,None), // #115 [ref=1x]
  E(660F38,90,0,0,0,1,3,None), // #116 [ref=1x]
  E(660F38,91,0,0,0,0,2,None), // #117 [ref=1x]
  E(660F38,91,0,0,0,1,3,None), // #118 [ref=1x]
  V(660F38,8E,0,0,0,0,0,None), // #119 [ref=1x]
  V(660F38,8E,0,0,1,0,0,None), // #120 [ref=1x]
  V(XOP_M8,C0,0,0,0,0,0,None), // #121 [ref=1x]
  V(XOP_M8,C2,0,0,0,0,0,None), // #122 [ref=1x]
  V(XOP_M8,C3,0,0,0,0,0,None), // #123 [ref=1x]
  V(XOP_M8,C1,0,0,0,0,0,None), // #124 [ref=1x]
  V(660F00,72,6,0,0,0,4,ByLL), // #125 [ref=1x]
  V(660F00,73,6,0,0,1,4,ByLL), // #126 [ref=1x]
  V(660F00,71,6,0,0,0,4,ByLL), // #127 [ref=1x]
  V(660F00,72,4,0,0,0,4,ByLL), // #128 [ref=1x]
  E(660F00,72,4,0,0,1,4,ByLL), // #129 [ref=1x]
  V(660F00,71,4,0,0,0,4,ByLL), // #130 [ref=1x]
  V(660F00,72,2,0,0,0,4,ByLL), // #131 [ref=1x]
  V(660F00,73,2,0,0,1,4,ByLL), // #132 [ref=1x]
  V(660F00,71,2,0,0,0,4,ByLL)  // #133 [ref=1x]
};
// ----------------------------------------------------------------------------
// ${AltOpcodeTable:End}

#undef E
#undef V
#undef O
#undef O_FPU
#undef O_ENCODE

// x86::InstDB - CommonInfoTable
// =============================

// ${InstCommonTable:Begin}
// ------------------- Automatically generated, do not edit -------------------
#define F(VAL) uint32_t(InstDB::InstFlags::k##VAL)
#define X(VAL) uint32_t(InstDB::Avx512Flags::k##VAL)
#define CONTROL_FLOW(VAL) uint8_t(InstControlFlow::k##VAL)
#define SAME_REG_HINT(VAL) uint8_t(InstSameRegHint::k##VAL)
const InstDB::CommonInfo InstDB::_inst_common_info_table[] = {
  { 0                                                 , 0                             , 0  , 0 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #0 [ref=1x]
  { 0                                                 , 0                             , 481, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #1 [ref=4x]
  { 0                                                 , 0                             , 482, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #2 [ref=2x]
  { 0                                                 , 0                             , 143, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #3 [ref=6x]
  { F(Lock)                                           , 0                             , 20 , 13, CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #4 [ref=2x]
  { 0                                                 , 0                             , 77 , 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #5 [ref=2x]
  { F(Vec)                                            , 0                             , 99 , 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #6 [ref=54x]
  { F(Vec)                                            , 0                             , 172, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #7 [ref=19x]
  { F(Vec)                                            , 0                             , 309, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #8 [ref=16x]
  { F(Vec)                                            , 0                             , 318, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #9 [ref=20x]
  { F(Lock)                                           , 0                             , 33 , 13, CONTROL_FLOW(Regular), SAME_REG_HINT(RO)}, // #10 [ref=1x]
  { F(Vex)                                            , 0                             , 351, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #11 [ref=3x]
  { F(Vec)                                            , 0                             , 99 , 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(RO)}, // #12 [ref=12x]
  { 0                                                 , 0                             , 483, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #13 [ref=1x]
  { F(Vex)                                            , 0                             , 353, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #14 [ref=5x]
  { F(Vex)                                            , 0                             , 77 , 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #15 [ref=12x]
  { F(Vec)                                            , 0                             , 484, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #16 [ref=4x]
  { 0                                                 , 0                             , 355, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #17 [ref=1x]
  { 0                                                 , 0                             , 76 , 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #18 [ref=21x]
  { 0                                                 , 0                             , 357, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #19 [ref=3x]
  { 0                                                 , 0                             , 163, 5 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #20 [ref=1x]
  { F(Lock)                                           , 0                             , 163, 5 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #21 [ref=3x]
  { F(RepIgnored)                                     , 0                             , 264, 3 , CONTROL_FLOW(Call), SAME_REG_HINT(None)}, // #22 [ref=1x]
  { 0                                                 , 0                             , 485, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #23 [ref=1x]
  { 0                                                 , 0                             , 486, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #24 [ref=2x]
  { 0                                                 , 0                             , 460, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #25 [ref=1x]
  { 0                                                 , 0                             , 145, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #26 [ref=88x]
  { 0                                                 , 0                             , 487, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #27 [ref=24x]
  { 0                                                 , 0                             , 488, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #28 [ref=6x]
  { 0                                                 , 0                             , 489, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #29 [ref=14x]
  { 0                                                 , 0                             , 490, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #30 [ref=1x]
  { 0                                                 , 0                             , 46 , 13, CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #31 [ref=1x]
  { F(Vex)                                            , 0                             , 359, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #32 [ref=16x]
  { F(Rep)                                            , 0                             , 208, 4 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #33 [ref=1x]
  { F(Vec)                                            , 0                             , 491, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #34 [ref=2x]
  { F(Vec)                                            , 0                             , 492, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #35 [ref=3x]
  { F(Lock)                                           , 0                             , 212, 4 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #36 [ref=1x]
  { F(Lock)                                           , 0                             , 493, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #37 [ref=1x]
  { F(Lock)                                           , 0                             , 494, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #38 [ref=1x]
  { 0                                                 , 0                             , 495, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #39 [ref=1x]
  { 0                                                 , 0                             , 496, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #40 [ref=1x]
  { 0                                                 , 0                             , 361, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #41 [ref=1x]
  { F(Mmx)|F(Vec)                                     , 0                             , 497, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #42 [ref=2x]
  { F(Mmx)|F(Vec)                                     , 0                             , 498, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #43 [ref=2x]
  { F(Mmx)|F(Vec)                                     , 0                             , 499, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #44 [ref=2x]
  { F(Vec)                                            , 0                             , 363, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #45 [ref=2x]
  { F(Vec)                                            , 0                             , 365, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #46 [ref=2x]
  { F(Vec)                                            , 0                             , 367, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #47 [ref=2x]
  { 0                                                 , 0                             , 500, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #48 [ref=1x]
  { 0                                                 , 0                             , 501, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #49 [ref=2x]
  { F(Lock)                                           , 0                             , 267, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #50 [ref=2x]
  { 0                                                 , 0                             , 72 , 4 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #51 [ref=3x]
  { F(Mmx)                                            , 0                             , 145, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #52 [ref=1x]
  { 0                                                 , 0                             , 369, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #53 [ref=2x]
  { 0                                                 , 0                             , 502, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #54 [ref=1x]
  { F(Vec)                                            , 0                             , 503, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #55 [ref=2x]
  { F(Vec)                                            , 0                             , 371, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #56 [ref=1x]
  { F(FpuM32)|F(FpuM64)                               , 0                             , 270, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #57 [ref=6x]
  { 0                                                 , 0                             , 373, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #58 [ref=9x]
  { F(FpuM80)                                         , 0                             , 504, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #59 [ref=2x]
  { 0                                                 , 0                             , 374, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #60 [ref=13x]
  { F(FpuM32)|F(FpuM64)                               , 0                             , 375, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #61 [ref=2x]
  { F(FpuM16)|F(FpuM32)                               , 0                             , 505, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #62 [ref=9x]
  { F(FpuM16)|F(FpuM32)|F(FpuM64)                     , 0                             , 506, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #63 [ref=3x]
  { F(FpuM32)|F(FpuM64)|F(FpuM80)                     , 0                             , 507, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #64 [ref=2x]
  { F(FpuM16)                                         , 0                             , 508, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #65 [ref=3x]
  { F(FpuM16)                                         , 0                             , 509, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #66 [ref=2x]
  { F(FpuM32)|F(FpuM64)                               , 0                             , 376, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #67 [ref=1x]
  { 0                                                 , 0                             , 510, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #68 [ref=4x]
  { 0                                                 , 0                             , 511, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #69 [ref=1x]
  { 0                                                 , 0                             , 512, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #70 [ref=1x]
  { 0                                                 , 0                             , 72 , 10, CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #71 [ref=1x]
  { 0                                                 , 0                             , 513, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #72 [ref=1x]
  { 0                                                 , 0                             , 399, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #73 [ref=2x]
  { 0                                                 , 0                             , 358, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #74 [ref=3x]
  { F(Rep)                                            , 0                             , 514, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #75 [ref=1x]
  { F(Vec)                                            , 0                             , 377, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #76 [ref=1x]
  { 0                                                 , 0                             , 515, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #77 [ref=2x]
  { 0                                                 , 0                             , 516, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #78 [ref=8x]
  { 0                                                 , 0                             , 379, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #79 [ref=3x]
  { 0                                                 , 0                             , 381, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #80 [ref=1x]
  { 0                                                 , 0                             , 383, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #81 [ref=1x]
  { 0                                                 , 0                             , 145, 1 , CONTROL_FLOW(Return), SAME_REG_HINT(None)}, // #82 [ref=2x]
  { 0                                                 , 0                             , 489, 1 , CONTROL_FLOW(Return), SAME_REG_HINT(None)}, // #83 [ref=1x]
  { F(RepIgnored)                                     , 0                             , 385, 2 , CONTROL_FLOW(Branch), SAME_REG_HINT(None)}, // #84 [ref=16x]
  { F(RepIgnored)                                     , 0                             , 387, 2 , CONTROL_FLOW(Branch), SAME_REG_HINT(None)}, // #85 [ref=1x]
  { F(RepIgnored)                                     , 0                             , 273, 3 , CONTROL_FLOW(Jump), SAME_REG_HINT(None)}, // #86 [ref=1x]
  { F(Vex)                                            , 0                             , 517, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #87 [ref=19x]
  { F(Vex)                                            , 0                             , 389, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #88 [ref=1x]
  { F(Vex)                                            , 0                             , 391, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #89 [ref=1x]
  { F(Vex)                                            , 0                             , 216, 4 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #90 [ref=1x]
  { F(Vex)                                            , 0                             , 393, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #91 [ref=1x]
  { F(Vex)                                            , 0                             , 518, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #92 [ref=12x]
  { F(Vex)                                            , 0                             , 519, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #93 [ref=8x]
  { F(Vex)                                            , 0                             , 517, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(WO)}, // #94 [ref=8x]
  { 0                                                 , 0                             , 520, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #95 [ref=2x]
  { 0                                                 , 0                             , 282, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #96 [ref=1x]
  { 0                                                 , 0                             , 276, 3 , CONTROL_FLOW(Call), SAME_REG_HINT(None)}, // #97 [ref=1x]
  { F(Vec)                                            , 0                             , 198, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #98 [ref=2x]
  { 0                                                 , 0                             , 521, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #99 [ref=2x]
  { 0                                                 , 0                             , 395, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #100 [ref=2x]
  { F(Vex)                                            , 0                             , 522, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #101 [ref=2x]
  { 0                                                 , 0                             , 397, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #102 [ref=1x]
  { 0                                                 , 0                             , 279, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #103 [ref=3x]
  { 0                                                 , 0                             , 276, 3 , CONTROL_FLOW(Jump), SAME_REG_HINT(None)}, // #104 [ref=1x]
  { 0                                                 , 0                             , 523, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #105 [ref=5x]
  { F(Vex)                                            , 0                             , 399, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #106 [ref=2x]
  { F(Rep)                                            , 0                             , 220, 4 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #107 [ref=1x]
  { 0                                                 , 0                             , 387, 2 , CONTROL_FLOW(Branch), SAME_REG_HINT(None)}, // #108 [ref=3x]
  { 0                                                 , 0                             , 282, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #109 [ref=1x]
  { F(Vex)                                            , 0                             , 401, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #110 [ref=2x]
  { F(Vec)                                            , 0                             , 524, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #111 [ref=1x]
  { F(Mmx)                                            , 0                             , 525, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #112 [ref=1x]
  { 0                                                 , 0                             , 526, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #113 [ref=2x]
  { 0                                                 , 0                             , 0  , 20, CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #114 [ref=1x]
  { 0                                                 , 0                             , 82 , 9 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #115 [ref=1x]
  { F(Vec)                                            , 0                             , 403, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #116 [ref=6x]
  { 0                                                 , 0                             , 139, 6 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #117 [ref=1x]
  { F(Mmx)|F(Vec)                                     , 0                             , 405, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #118 [ref=1x]
  { 0                                                 , 0                             , 407, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #119 [ref=1x]
  { F(Mmx)|F(Vec)                                     , 0                             , 527, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #120 [ref=1x]
  { F(Vec)                                            , 0                             , 372, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #121 [ref=2x]
  { F(Vec)                                            , 0                             , 107, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #122 [ref=4x]
  { F(Vec)                                            , 0                             , 528, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #123 [ref=2x]
  { F(Vec)                                            , 0                             , 101, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #124 [ref=3x]
  { F(Mmx)                                            , 0                             , 529, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #125 [ref=1x]
  { F(Vec)                                            , 0                             , 107, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #126 [ref=1x]
  { F(Vec)                                            , 0                             , 115, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #127 [ref=1x]
  { F(Mmx)|F(Vec)                                     , 0                             , 168, 5 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #128 [ref=1x]
  { F(Mmx)|F(Vec)                                     , 0                             , 530, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #129 [ref=1x]
  { F(Rep)                                            , 0                             , 224, 4 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #130 [ref=1x]
  { F(Vec)                                            , 0                             , 409, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #131 [ref=1x]
  { F(Vec)                                            , 0                             , 411, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #132 [ref=1x]
  { 0                                                 , 0                             , 285, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #133 [ref=2x]
  { 0                                                 , 0                             , 413, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #134 [ref=1x]
  { F(Vex)                                            , 0                             , 415, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #135 [ref=1x]
  { 0                                                 , 0                             , 531, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #136 [ref=1x]
  { 0                                                 , 0                             , 532, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #137 [ref=1x]
  { F(Lock)                                           , 0                             , 268, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #138 [ref=2x]
  { 0                                                 , 0                             , 145, 6 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #139 [ref=1x]
  { F(Lock)                                           , 0                             , 59 , 13, CONTROL_FLOW(Regular), SAME_REG_HINT(RO)}, // #140 [ref=1x]
  { 0                                                 , 0                             , 533, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #141 [ref=1x]
  { F(Rep)                                            , 0                             , 534, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #142 [ref=1x]
  { F(Mmx)|F(Vec)                                     , 0                             , 417, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #143 [ref=37x]
  { F(Mmx)|F(Vec)                                     , 0                             , 419, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #144 [ref=1x]
  { F(Mmx)|F(Vec)                                     , 0                             , 417, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(RO)}, // #145 [ref=6x]
  { F(Mmx)|F(Vec)                                     , 0                             , 417, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(WO)}, // #146 [ref=16x]
  { F(Mmx)                                            , 0                             , 168, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #147 [ref=26x]
  { F(Vec)                                            , 0                             , 99 , 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(WO)}, // #148 [ref=4x]
  { F(Vec)                                            , 0                             , 535, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #149 [ref=1x]
  { F(Vec)                                            , 0                             , 536, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #150 [ref=1x]
  { F(Vec)                                            , 0                             , 537, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #151 [ref=1x]
  { F(Vec)                                            , 0                             , 538, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #152 [ref=1x]
  { F(Vec)                                            , 0                             , 539, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #153 [ref=1x]
  { F(Vec)                                            , 0                             , 540, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #154 [ref=1x]
  { F(Mmx)|F(Vec)                                     , 0                             , 421, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #155 [ref=1x]
  { F(Vec)                                            , 0                             , 541, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #156 [ref=1x]
  { F(Vec)                                            , 0                             , 542, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #157 [ref=1x]
  { F(Vec)                                            , 0                             , 543, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #158 [ref=1x]
  { F(Mmx)|F(Vec)                                     , 0                             , 544, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #159 [ref=1x]
  { F(Mmx)|F(Vec)                                     , 0                             , 545, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #160 [ref=1x]
  { F(Vec)                                            , 0                             , 339, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #161 [ref=2x]
  { 0                                                 , 0                             , 173, 5 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #162 [ref=1x]
  { F(Mmx)                                            , 0                             , 419, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #163 [ref=1x]
  { F(Mmx)|F(Vec)                                     , 0                             , 423, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #164 [ref=8x]
  { F(Vec)                                            , 0                             , 546, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #165 [ref=2x]
  { 0                                                 , 0                             , 425, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #166 [ref=1x]
  { F(Mmx)|F(Vec)                                     , 0                             , 427, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #167 [ref=3x]
  { 0                                                 , 0                             , 178, 5 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #168 [ref=1x]
  { 0                                                 , 0                             , 547, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #169 [ref=1x]
  { 0                                                 , 0                             , 429, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #170 [ref=7x]
  { 0                                                 , 0                             , 548, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #171 [ref=4x]
  { F(Vex)                                            , 0                             , 431, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #172 [ref=1x]
  { 0                                                 , 0                             , 433, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #173 [ref=1x]
  { 0                                                 , 0                             , 431, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #174 [ref=7x]
  { F(RepIgnored)                                     , 0                             , 435, 2 , CONTROL_FLOW(Return), SAME_REG_HINT(None)}, // #175 [ref=1x]
  { 0                                                 , 0                             , 435, 2 , CONTROL_FLOW(Return), SAME_REG_HINT(None)}, // #176 [ref=1x]
  { F(Vex)                                            , 0                             , 437, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #177 [ref=1x]
  { F(Lock)                                           , 0                             , 20 , 13, CONTROL_FLOW(Regular), SAME_REG_HINT(WO)}, // #178 [ref=3x]
  { F(Rep)                                            , 0                             , 228, 4 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #179 [ref=1x]
  { 0                                                 , 0                             , 549, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #180 [ref=16x]
  { 0                                                 , 0                             , 288, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #181 [ref=2x]
  { 0                                                 , 0                             , 439, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #182 [ref=3x]
  { F(Rep)                                            , 0                             , 232, 4 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #183 [ref=1x]
  { F(Vex)                                            , 0                             , 550, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #184 [ref=8x]
  { 0                                                 , 0                             , 91 , 8 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #185 [ref=1x]
  { F(Tsib)|F(Vex)                                    , 0                             , 551, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #186 [ref=2x]
  { F(Vex)                                            , 0                             , 489, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #187 [ref=1x]
  { F(Tsib)|F(Vex)                                    , 0                             , 552, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #188 [ref=1x]
  { F(Vex)                                            , 0                             , 553, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #189 [ref=1x]
  { 0                                                 , 0                             , 554, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #190 [ref=2x]
  { 0                                                 , 0                             , 77 , 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #191 [ref=2x]
  { 0                                                 , 0                             , 441, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #192 [ref=1x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(B64)|X(ER)|X(K)|X(SAE)|X(Z) , 291, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #193 [ref=22x]
  { F(Evex)|F(Vec)                                    , X(B16)|X(ER)|X(K)|X(SAE)|X(Z) , 291, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #194 [ref=23x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(B32)|X(ER)|X(K)|X(SAE)|X(Z) , 291, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #195 [ref=22x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(ER)|X(K)|X(SAE)|X(Z)        , 555, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #196 [ref=18x]
  { F(Evex)|F(Vec)                                    , X(ER)|X(K)|X(SAE)|X(Z)        , 556, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #197 [ref=18x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(ER)|X(K)|X(SAE)|X(Z)        , 557, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #198 [ref=17x]
  { F(Vec)|F(Vex)                                     , 0                             , 291, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #199 [ref=29x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , 0                             , 291, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #200 [ref=5x]
  { F(Vec)|F(Vex)                                     , 0                             , 99 , 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #201 [ref=17x]
  { F(Vec)|F(Vex)                                     , 0                             , 318, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #202 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(B32)|X(K)|X(Z)              , 294, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #203 [ref=4x]
  { F(Evex)|F(Vec)                                    , X(B64)|X(K)|X(Z)              , 294, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #204 [ref=4x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(B64)|X(K)|X(Z)              , 291, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #205 [ref=10x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(B32)|X(K)|X(Z)              , 291, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #206 [ref=12x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(B64)|X(K)|X(Z)              , 291, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(RO)}, // #207 [ref=2x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(B32)|X(K)|X(Z)              , 291, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(RO)}, // #208 [ref=6x]
  { F(Vec)|F(Vex)                                     , 0                             , 558, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #209 [ref=2x]
  { F(Evex)|F(Vec)                                    , X(B64)|X(K)|X(Z)              , 291, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #210 [ref=17x]
  { F(Evex)|F(Vec)                                    , X(B32)|X(K)|X(Z)              , 291, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #211 [ref=12x]
  { F(Vec)|F(Vex)                                     , 0                             , 294, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #212 [ref=6x]
  { F(Vec)|F(Vex)                                     , 0                             , 443, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #213 [ref=3x]
  { F(EvexTransformable)|F(Vec)|F(Vex)                , 0                             , 559, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #214 [ref=2x]
  { F(Evex)|F(Vec)                                    , X(K)|X(Z)                     , 560, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #215 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(K)|X(Z)                     , 561, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #216 [ref=4x]
  { F(Evex)|F(Vec)                                    , X(K)|X(Z)                     , 562, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #217 [ref=4x]
  { F(Evex)|F(Vec)                                    , X(K)|X(Z)                     , 469, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #218 [ref=1x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(K)|X(Z)                     , 560, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #219 [ref=1x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(K)|X(Z)                     , 563, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #220 [ref=1x]
  { F(Evex)|F(EvexKReg)|F(Vec)|F(Vex)                 , X(B64)|X(ImplicitZ)|X(K)|X(SAE), 297, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #221 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(B16)|X(ImplicitZ)|X(K)|X(SAE), 300, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #222 [ref=1x]
  { F(Evex)|F(EvexKReg)|F(Vec)|F(Vex)                 , X(B32)|X(ImplicitZ)|X(K)|X(SAE), 297, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #223 [ref=1x]
  { F(Evex)|F(EvexKReg)|F(Vec)|F(Vex)                 , X(ImplicitZ)|X(K)|X(SAE)      , 564, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #224 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(ImplicitZ)|X(K)|X(SAE)      , 565, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #225 [ref=1x]
  { F(Evex)|F(EvexKReg)|F(Vec)|F(Vex)                 , X(ImplicitZ)|X(K)|X(SAE)      , 566, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #226 [ref=1x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(SAE)                        , 172, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #227 [ref=2x]
  { F(Evex)|F(Vec)                                    , X(SAE)                        , 339, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #228 [ref=2x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(SAE)                        , 309, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #229 [ref=2x]
  { F(Evex)|F(Vec)                                    , X(K)|X(Z)                     , 303, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #230 [ref=6x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(B32)|X(K)|X(Z)              , 306, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #231 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(B32)|X(ER)|X(K)|X(SAE)|X(Z) , 445, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #232 [ref=3x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(B32)|X(ER)|X(K)|X(SAE)|X(Z) , 151, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #233 [ref=3x]
  { F(Vec)|F(Vex)                                     , 0                             , 198, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #234 [ref=5x]
  { F(Evex)|F(EvexCompat)|F(PreferEvex)|F(Vec)|F(Vex) , X(B32)|X(K)|X(Z)              , 445, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #235 [ref=1x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(B64)|X(ER)|X(K)|X(SAE)|X(Z) , 445, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #236 [ref=2x]
  { F(Evex)|F(Vec)                                    , X(B64)|X(ER)|X(K)|X(SAE)|X(Z) , 567, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #237 [ref=3x]
  { F(Evex)|F(Vec)                                    , X(B64)|X(ER)|X(K)|X(SAE)|X(Z) , 151, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #238 [ref=4x]
  { F(Evex)|F(Vec)                                    , X(B64)|X(ER)|X(K)|X(SAE)|X(Z) , 445, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #239 [ref=3x]
  { F(Evex)|F(Vec)                                    , X(B16)|X(ER)|X(K)|X(SAE)|X(Z) , 306, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #240 [ref=2x]
  { F(Evex)|F(Vec)                                    , X(B16)|X(K)|X(SAE)|X(Z)       , 309, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #241 [ref=3x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(K)|X(SAE)|X(Z)              , 306, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #242 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(B16)|X(K)|X(SAE)|X(Z)       , 306, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #243 [ref=3x]
  { F(Evex)|F(Vec)                                    , X(B16)|X(ER)|X(K)|X(SAE)|X(Z) , 309, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #244 [ref=2x]
  { F(Evex)|F(Vec)                                    , X(B16)|X(ER)|X(K)|X(SAE)|X(Z) , 151, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #245 [ref=5x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(B32)|X(ER)|X(K)|X(SAE)|X(Z) , 306, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #246 [ref=1x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(K)|X(SAE)|X(Z)              , 312, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #247 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(B32)|X(ER)|X(K)|X(SAE)|X(Z) , 306, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #248 [ref=3x]
  { F(Evex)|F(Vec)                                    , X(B32)|X(ER)|X(K)|X(SAE)|X(Z) , 151, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #249 [ref=2x]
  { F(Evex)|F(Vec)                                    , X(ER)|X(K)|X(SAE)|X(Z)        , 555, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #250 [ref=2x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(ER)|X(SAE)                  , 363, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #251 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(ER)|X(SAE)                  , 363, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #252 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(K)|X(SAE)|X(Z)              , 556, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #253 [ref=5x]
  { F(Evex)|F(Vec)                                    , X(ER)|X(SAE)                  , 447, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #254 [ref=2x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(ER)|X(SAE)                  , 449, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #255 [ref=2x]
  { F(Evex)|F(Vec)                                    , X(ER)|X(SAE)                  , 451, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #256 [ref=2x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(K)|X(SAE)|X(Z)              , 557, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #257 [ref=3x]
  { F(Evex)|F(Vec)                                    , X(ER)|X(K)|X(SAE)|X(Z)        , 557, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #258 [ref=6x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(ER)|X(SAE)                  , 367, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #259 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(ER)|X(SAE)                  , 367, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #260 [ref=1x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(B64)|X(K)|X(SAE)|X(Z)       , 445, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #261 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(B64)|X(K)|X(SAE)|X(Z)       , 151, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #262 [ref=3x]
  { F(Evex)|F(Vec)                                    , X(B64)|X(K)|X(SAE)|X(Z)       , 445, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #263 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(B16)|X(K)|X(SAE)|X(Z)       , 151, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #264 [ref=3x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(B32)|X(K)|X(SAE)|X(Z)       , 151, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #265 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(B32)|X(K)|X(SAE)|X(Z)       , 306, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #266 [ref=2x]
  { F(Evex)|F(Vec)                                    , X(B32)|X(K)|X(SAE)|X(Z)       , 151, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #267 [ref=2x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(SAE)                        , 363, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #268 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(SAE)                        , 363, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #269 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(SAE)                        , 447, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #270 [ref=2x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(SAE)                        , 367, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #271 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(SAE)                        , 367, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #272 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(ER)|X(SAE)                  , 449, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #273 [ref=2x]
  { F(Evex)|F(Vec)                                    , X(K)|X(Z)                     , 294, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #274 [ref=3x]
  { F(Vec)|F(Vex)                                     , 0                             , 294, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #275 [ref=10x]
  { F(Evex)|F(Vec)                                    , X(K)|X(Z)                     , 151, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #276 [ref=8x]
  { F(EvexTransformable)|F(Vec)|F(Vex)                , 0                             , 313, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #277 [ref=2x]
  { F(Evex)|F(Vec)                                    , X(K)|X(Z)                     , 568, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #278 [ref=4x]
  { F(Evex)|F(Vec)                                    , X(K)|X(Z)                     , 314, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #279 [ref=4x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , 0                             , 503, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #280 [ref=2x]
  { F(Evex)|F(Vec)                                    , X(B32)|X(ER)|X(K)|X(SAE)|X(Z) , 291, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #281 [ref=5x]
  { F(Evex)|F(Vec)                                    , X(B64)|X(K)|X(SAE)|X(Z)       , 294, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #282 [ref=2x]
  { F(Evex)|F(Vec)                                    , X(B32)|X(K)|X(SAE)|X(Z)       , 294, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #283 [ref=2x]
  { F(Evex)|F(Vec)                                    , X(K)|X(SAE)|X(Z)              , 569, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #284 [ref=4x]
  { F(Evex)|F(Vec)                                    , X(K)|X(SAE)|X(Z)              , 570, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #285 [ref=4x]
  { F(Vec)|F(Vex)                                     , 0                             , 236, 4 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #286 [ref=13x]
  { F(Vec)|F(Vex)                                     , 0                             , 453, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #287 [ref=4x]
  { F(Vec)|F(Vex)                                     , 0                             , 455, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #288 [ref=4x]
  { F(Evex)|F(Vec)                                    , X(B64)|X(ImplicitZ)|X(K)      , 571, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #289 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(B16)|X(ImplicitZ)|X(K)      , 571, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #290 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(B32)|X(ImplicitZ)|X(K)      , 571, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #291 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(ImplicitZ)|X(K)             , 572, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #292 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(ImplicitZ)|X(K)             , 573, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #293 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(ImplicitZ)|X(K)             , 574, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #294 [ref=1x]
  { F(Vec)|F(Vex)                                     , 0                             , 99 , 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #295 [ref=7x]
  { F(Vec)|F(Vex)                                     , 0                             , 172, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #296 [ref=1x]
  { F(Vec)|F(Vex)                                     , 0                             , 309, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #297 [ref=1x]
  { F(Evex)|F(EvexTwoOp)|F(Vec)|F(Vex)|F(Vsib)        , X(K)                          , 240, 4 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #298 [ref=2x]
  { F(Evex)|F(EvexTwoOp)|F(Vec)|F(Vex)|F(Vsib)        , X(K)                          , 183, 5 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #299 [ref=2x]
  { F(Evex)|F(EvexTwoOp)|F(Vec)|F(Vex)|F(Vsib)        , X(K)                          , 188, 5 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #300 [ref=2x]
  { F(Evex)|F(EvexTwoOp)|F(Vec)|F(Vex)|F(Vsib)        , X(K)                          , 315, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #301 [ref=2x]
  { F(Evex)|F(Vec)                                    , X(K)|X(SAE)|X(Z)              , 555, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #302 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(K)|X(SAE)|X(Z)              , 557, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #303 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(B64)|X(K)|X(SAE)|X(Z)       , 318, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #304 [ref=2x]
  { F(Evex)|F(Vec)                                    , X(B16)|X(K)|X(SAE)|X(Z)       , 318, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #305 [ref=3x]
  { F(Evex)|F(Vec)                                    , X(B32)|X(K)|X(SAE)|X(Z)       , 318, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #306 [ref=2x]
  { F(Evex)|F(Vec)                                    , X(K)|X(SAE)|X(Z)              , 575, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #307 [ref=3x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(K)|X(Z)                     , 294, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #308 [ref=3x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(K)|X(Z)                     , 291, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #309 [ref=22x]
  { F(EvexTransformable)|F(Vec)|F(Vex)                , 0                             , 457, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #310 [ref=2x]
  { F(Evex)|F(Vec)                                    , X(K)|X(Z)                     , 457, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #311 [ref=4x]
  { F(Evex)|F(Vec)                                    , X(K)|X(Z)                     , 576, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #312 [ref=4x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , 0                             , 570, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #313 [ref=1x]
  { F(Vex)                                            , 0                             , 521, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #314 [ref=2x]
  { F(Vec)|F(Vex)                                     , 0                             , 524, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #315 [ref=1x]
  { F(Vec)|F(Vex)                                     , 0                             , 244, 4 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #316 [ref=4x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(B64)|X(K)|X(SAE)|X(Z)       , 291, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #317 [ref=2x]
  { F(Evex)|F(Vec)                                    , X(B16)|X(K)|X(SAE)|X(Z)       , 291, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #318 [ref=2x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(B32)|X(K)|X(SAE)|X(Z)       , 291, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #319 [ref=2x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(K)|X(SAE)|X(Z)              , 555, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #320 [ref=2x]
  { 0                                                 , 0                             , 459, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #321 [ref=3x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(K)|X(Z)                     , 99 , 8 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #322 [ref=4x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , 0                             , 461, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #323 [ref=1x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(K)|X(Z)                     , 321, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #324 [ref=1x]
  { F(EvexTransformable)|F(Vec)|F(Vex)                , 0                             , 99 , 4 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #325 [ref=2x]
  { F(Evex)|F(Vec)                                    , X(K)|X(Z)                     , 151, 6 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #326 [ref=6x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , 0                             , 109, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #327 [ref=2x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , 0                             , 248, 4 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #328 [ref=4x]
  { F(Vec)|F(Vex)                                     , 0                             , 577, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #329 [ref=3x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , 0                             , 193, 5 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #330 [ref=3x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , 0                             , 198, 5 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #331 [ref=1x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , 0                             , 203, 5 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #332 [ref=1x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(K)|X(Z)                     , 107, 8 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #333 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(K)|X(Z)                     , 252, 4 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #334 [ref=1x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(K)|X(Z)                     , 151, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #335 [ref=4x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(K)|X(Z)                     , 115, 8 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #336 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(K)|X(Z)                     , 463, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #337 [ref=1x]
  { 0                                                 , 0                             , 465, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #338 [ref=1x]
  { 0                                                 , 0                             , 467, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #339 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(B32)                        , 324, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #340 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(B64)                        , 324, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #341 [ref=1x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(B32)|X(K)|X(Z)              , 151, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #342 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(B64)|X(K)|X(Z)              , 151, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #343 [ref=5x]
  { F(EvexTransformable)|F(Vec)|F(Vex)                , 0                             , 291, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(RO)}, // #344 [ref=2x]
  { F(Evex)|F(Vec)                                    , X(B32)|X(K)|X(Z)              , 291, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(RO)}, // #345 [ref=2x]
  { F(EvexTransformable)|F(Vec)|F(Vex)                , 0                             , 291, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(WO)}, // #346 [ref=2x]
  { F(Evex)|F(Vec)                                    , X(B32)|X(K)|X(Z)              , 291, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(WO)}, // #347 [ref=2x]
  { F(Evex)|F(Vec)                                    , X(B64)|X(K)|X(Z)              , 291, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(WO)}, // #348 [ref=2x]
  { F(Evex)|F(Vec)                                    , X(B64)|X(K)|X(Z)              , 291, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(RO)}, // #349 [ref=2x]
  { F(Evex)|F(Vec)                                    , X(K)|X(Z)                     , 291, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #350 [ref=13x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(K)|X(Z)                     , 578, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #351 [ref=1x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(K)|X(Z)                     , 579, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #352 [ref=1x]
  { F(Evex)|F(Vec)                                    , 0                             , 580, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #353 [ref=6x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(K)|X(Z)                     , 469, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #354 [ref=1x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(K)|X(Z)                     , 581, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #355 [ref=1x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , 0                             , 294, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #356 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(ImplicitZ)|X(K)             , 300, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(WO)}, // #357 [ref=4x]
  { F(Evex)|F(Vec)                                    , X(B32)|X(ImplicitZ)|X(K)      , 300, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(WO)}, // #358 [ref=2x]
  { F(Evex)|F(EvexKReg)|F(Vec)|F(Vex)                 , X(ImplicitZ)|X(K)             , 327, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(WO)}, // #359 [ref=4x]
  { F(Evex)|F(EvexKReg)|F(Vec)|F(Vex)                 , X(B32)|X(ImplicitZ)|X(K)      , 327, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(WO)}, // #360 [ref=2x]
  { F(Evex)|F(EvexKReg)|F(Vec)|F(Vex)                 , X(B64)|X(ImplicitZ)|X(K)      , 327, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(WO)}, // #361 [ref=2x]
  { F(Vec)|F(Vex)                                     , 0                             , 535, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #362 [ref=1x]
  { F(Vec)|F(Vex)                                     , 0                             , 536, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #363 [ref=1x]
  { F(Vec)|F(Vex)                                     , 0                             , 537, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #364 [ref=1x]
  { F(Vec)|F(Vex)                                     , 0                             , 538, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #365 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(B64)|X(ImplicitZ)|X(K)      , 300, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(WO)}, // #366 [ref=2x]
  { F(Evex)|F(Vec)                                    , X(B32)|X(K)|X(Z)              , 151, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #367 [ref=6x]
  { F(Evex)|F(EvexCompat)|F(PreferEvex)|F(Vec)|F(Vex) , X(B32)|X(K)|X(Z)              , 291, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #368 [ref=4x]
  { F(Vec)|F(Vex)                                     , 0                             , 295, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #369 [ref=2x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(B32)|X(K)|X(Z)              , 292, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #370 [ref=2x]
  { F(Vec)|F(Vex)                                     , 0                             , 256, 4 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #371 [ref=2x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(B64)|X(K)|X(Z)              , 123, 8 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #372 [ref=1x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(B32)|X(K)|X(Z)              , 123, 8 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #373 [ref=1x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(B64)|X(K)|X(Z)              , 260, 4 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #374 [ref=2x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , 0                             , 539, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #375 [ref=1x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , 0                             , 540, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #376 [ref=1x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , 0                             , 582, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #377 [ref=1x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(K)|X(Z)                     , 583, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #378 [ref=1x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(K)|X(Z)                     , 584, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #379 [ref=1x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(K)|X(Z)                     , 585, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #380 [ref=1x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(K)|X(Z)                     , 586, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #381 [ref=1x]
  { F(Vec)|F(Vex)                                     , 0                             , 443, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #382 [ref=12x]
  { F(Evex)|F(EvexCompat)|F(PreferEvex)|F(Vec)|F(Vex) , X(B64)|X(K)|X(Z)              , 291, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #383 [ref=2x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(K)|X(Z)                     , 291, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(RO)}, // #384 [ref=8x]
  { F(Evex)|F(Vec)                                    , 0                             , 587, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #385 [ref=4x]
  { F(Evex)|F(Vec)                                    , X(K)|X(Z)                     , 330, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #386 [ref=6x]
  { F(Evex)|F(Vec)                                    , X(K)|X(Z)                     , 333, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #387 [ref=9x]
  { F(Evex)|F(Vec)                                    , X(K)|X(Z)                     , 336, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #388 [ref=3x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(K)|X(Z)                     , 309, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #389 [ref=4x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(K)|X(Z)                     , 339, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #390 [ref=2x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(K)|X(Z)                     , 306, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #391 [ref=6x]
  { F(Vec)|F(Vex)                                     , 0                             , 471, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #392 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(B32)|X(K)|X(Z)              , 318, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #393 [ref=3x]
  { F(Evex)|F(Vec)                                    , X(B64)|X(K)|X(Z)              , 318, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #394 [ref=3x]
  { F(Vec)|F(Vex)                                     , 0                             , 473, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #395 [ref=4x]
  { F(Evex)|F(Vec)|F(Vsib)                            , X(K)                          , 342, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #396 [ref=2x]
  { F(Evex)|F(Vec)|F(Vsib)                            , X(K)                          , 475, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #397 [ref=2x]
  { F(Evex)|F(Vec)|F(Vsib)                            , X(K)                          , 477, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #398 [ref=2x]
  { F(Evex)|F(Vec)|F(Vsib)                            , X(K)                          , 345, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #399 [ref=2x]
  { F(Vec)|F(Vex)                                     , 0                             , 479, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #400 [ref=8x]
  { F(Evex)|F(Vec)                                    , X(ImplicitZ)|X(K)             , 348, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #401 [ref=5x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(B32)|X(K)|X(Z)              , 318, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #402 [ref=1x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(K)|X(Z)                     , 318, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #403 [ref=2x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(B32)|X(K)|X(Z)              , 157, 6 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #404 [ref=3x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , 0                             , 318, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #405 [ref=2x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(B64)|X(K)|X(Z)              , 157, 6 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #406 [ref=2x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(K)|X(Z)                     , 157, 6 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #407 [ref=3x]
  { F(Evex)|F(Vec)                                    , X(B64)|X(K)|X(Z)              , 157, 6 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #408 [ref=1x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(K)|X(Z)                     , 291, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(WO)}, // #409 [ref=6x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(B32)|X(K)|X(Z)              , 291, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(WO)}, // #410 [ref=2x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(B64)|X(K)|X(Z)              , 291, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(WO)}, // #411 [ref=2x]
  { F(Evex)|F(Vec)                                    , X(B32)|X(ImplicitZ)|X(K)      , 348, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #412 [ref=2x]
  { F(Evex)|F(Vec)                                    , X(B64)|X(ImplicitZ)|X(K)      , 348, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #413 [ref=2x]
  { F(Evex)|F(Vec)                                    , X(K)|X(Z)                     , 555, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #414 [ref=2x]
  { F(Evex)|F(Vec)                                    , X(K)|X(Z)                     , 557, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #415 [ref=2x]
  { F(Evex)|F(Vec)                                    , X(B16)|X(K)|X(Z)              , 151, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #416 [ref=2x]
  { F(Evex)|F(Vec)                                    , X(K)|X(Z)                     , 556, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #417 [ref=2x]
  { F(Vec)|F(Vex)                                     , 0                             , 557, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #418 [ref=2x]
  { F(Evex)|F(Vec)                                    , X(K)|X(Z)                     , 569, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #419 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(K)|X(Z)                     , 570, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #420 [ref=1x]
  { F(EvexTransformable)|F(Vec)|F(Vex)                , 0                             , 318, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #421 [ref=2x]
  { F(EvexTransformable)|F(Vec)|F(Vex)                , 0                             , 569, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #422 [ref=1x]
  { F(EvexTransformable)|F(Vec)|F(Vex)                , 0                             , 570, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #423 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(B64)|X(ER)|X(K)|X(SAE)|X(Z) , 291, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #424 [ref=1x]
  { F(Vec)|F(Vex)                                     , 0                             , 588, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #425 [ref=1x]
  { F(Vec)|F(Vex)                                     , 0                             , 589, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #426 [ref=1x]
  { F(Vec)|F(Vex)                                     , 0                             , 590, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #427 [ref=1x]
  { F(Evex)|F(Vec)                                    , X(B32)|X(K)|X(Z)              , 295, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #428 [ref=2x]
  { F(Evex)|F(Vec)                                    , X(B64)|X(K)|X(Z)              , 295, 2 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #429 [ref=2x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(B64)|X(K)|X(Z)              , 294, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #430 [ref=1x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(B32)|X(K)|X(Z)              , 294, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #431 [ref=1x]
  { F(Vec)|F(Vex)                                     , 0                             , 291, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #432 [ref=2x]
  { F(Evex)|F(EvexCompat)|F(Vec)|F(Vex)               , X(B64)|X(ER)|X(K)|X(SAE)|X(Z) , 151, 3 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #433 [ref=1x]
  { F(Vec)|F(Vex)                                     , 0                             , 145, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #434 [ref=2x]
  { 0                                                 , 0                             , 143, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #435 [ref=2x]
  { 0                                                 , 0                             , 42 , 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #436 [ref=2x]
  { F(Lock)                                           , 0                             , 20 , 4 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #437 [ref=1x]
  { 0                                                 , 0                             , 265, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #438 [ref=1x]
  { F(Lock)                                           , 0                             , 131, 8 , CONTROL_FLOW(Regular), SAME_REG_HINT(RO)}, // #439 [ref=1x]
  { 0                                                 , 0                             , 591, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}, // #440 [ref=6x]
  { 0                                                 , 0                             , 592, 1 , CONTROL_FLOW(Regular), SAME_REG_HINT(None)}  // #441 [ref=6x]
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
const InstDB::AdditionalInfo InstDB::additional_info_table[] = {
  { 0, 0, { 0 } }, // #0 [ref=68x]
  { 0, 1, { 0 } }, // #1 [ref=31x]
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
  { 0, 11, { EXT(CMOV) } }, // #29 [ref=2x]
  { 0, 12, { EXT(CMOV) } }, // #30 [ref=2x]
  { 0, 13, { EXT(CMOV) } }, // #31 [ref=2x]
  { 0, 14, { EXT(CMOV) } }, // #32 [ref=2x]
  { 0, 15, { EXT(CMOV) } }, // #33 [ref=2x]
  { 0, 16, { EXT(CMOV) } }, // #34 [ref=2x]
  { 0, 17, { EXT(CMOV) } }, // #35 [ref=2x]
  { 0, 18, { EXT(CMOV) } }, // #36 [ref=2x]
  { 0, 1, { EXT(CMPCCXADD) } }, // #37 [ref=16x]
  { 0, 19, { 0 } }, // #38 [ref=2x]
  { 0, 1, { EXT(I486) } }, // #39 [ref=2x]
  { 0, 5, { EXT(CMPXCHG16B) } }, // #40 [ref=1x]
  { 0, 5, { EXT(CMPXCHG8B) } }, // #41 [ref=1x]
  { 0, 1, { EXT(SSE2) } }, // #42 [ref=2x]
  { 0, 1, { EXT(SSE) } }, // #43 [ref=2x]
  { 0, 0, { EXT(I486) } }, // #44 [ref=5x]
  { 0, 0, { EXT(SSE4_2) } }, // #45 [ref=2x]
  { 0, 20, { 0 } }, // #46 [ref=2x]
  { 0, 0, { EXT(MMX) } }, // #47 [ref=1x]
  { 0, 0, { EXT(CET_IBT) } }, // #48 [ref=2x]
  { 0, 1, { EXT(ENQCMD) } }, // #49 [ref=2x]
  { 0, 0, { EXT(SSE4A) } }, // #50 [ref=4x]
  { 0, 21, { EXT(FPU) } }, // #51 [ref=80x]
  { 0, 22, { EXT(CMOV), EXT(FPU) } }, // #52 [ref=2x]
  { 0, 23, { EXT(CMOV), EXT(FPU) } }, // #53 [ref=2x]
  { 0, 24, { EXT(CMOV), EXT(FPU) } }, // #54 [ref=2x]
  { 0, 25, { EXT(CMOV), EXT(FPU) } }, // #55 [ref=2x]
  { 0, 26, { EXT(FPU) } }, // #56 [ref=4x]
  { 0, 0, { EXT(3DNOW) } }, // #57 [ref=21x]
  { 0, 21, { EXT(SSE3), EXT(FPU) } }, // #58 [ref=1x]
  { 0, 21, { EXT(FXSR) } }, // #59 [ref=2x]
  { 0, 27, { EXT(FXSR) } }, // #60 [ref=2x]
  { 0, 0, { EXT(SMX) } }, // #61 [ref=1x]
  { 0, 0, { EXT(GFNI) } }, // #62 [ref=3x]
  { 0, 0, { EXT(HRESET) } }, // #63 [ref=1x]
  { 0, 0, { EXT(CET_SS) } }, // #64 [ref=9x]
  { 0, 15, { 0 } }, // #65 [ref=5x]
  { 0, 0, { EXT(VMX) } }, // #66 [ref=13x]
  { 0, 0, { EXT(INVLPGB) } }, // #67 [ref=2x]
  { 0, 11, { 0 } }, // #68 [ref=4x]
  { 0, 12, { 0 } }, // #69 [ref=4x]
  { 0, 13, { 0 } }, // #70 [ref=4x]
  { 0, 14, { 0 } }, // #71 [ref=4x]
  { 0, 16, { 0 } }, // #72 [ref=4x]
  { 0, 17, { 0 } }, // #73 [ref=4x]
  { 0, 18, { 0 } }, // #74 [ref=6x]
  { 0, 0, { EXT(AVX512_DQ) } }, // #75 [ref=10x]
  { 0, 0, { EXT(AVX512_BW) } }, // #76 [ref=20x]
  { 0, 0, { EXT(AVX512_F) } }, // #77 [ref=9x]
  { 1, 0, { EXT(AVX512_DQ) } }, // #78 [ref=1x]
  { 1, 0, { EXT(AVX512_BW) } }, // #79 [ref=2x]
  { 1, 0, { EXT(AVX512_F) } }, // #80 [ref=1x]
  { 0, 1, { EXT(AVX512_DQ) } }, // #81 [ref=3x]
  { 0, 1, { EXT(AVX512_BW) } }, // #82 [ref=4x]
  { 0, 1, { EXT(AVX512_F) } }, // #83 [ref=1x]
  { 0, 28, { EXT(LAHFSAHF) } }, // #84 [ref=1x]
  { 0, 0, { EXT(AMX_TILE) } }, // #85 [ref=7x]
  { 0, 0, { EXT(LWP) } }, // #86 [ref=4x]
  { 0, 29, { 0 } }, // #87 [ref=3x]
  { 0, 1, { EXT(LZCNT) } }, // #88 [ref=1x]
  { 0, 0, { EXT(MMX2) } }, // #89 [ref=3x]
  { 0, 1, { EXT(MCOMMIT) } }, // #90 [ref=1x]
  { 0, 0, { EXT(MONITOR) } }, // #91 [ref=2x]
  { 0, 0, { EXT(MONITORX) } }, // #92 [ref=2x]
  { 1, 0, { 0 } }, // #93 [ref=1x]
  { 1, 0, { EXT(SSE2) } }, // #94 [ref=5x]
  { 1, 0, { EXT(SSE) } }, // #95 [ref=3x]
  { 0, 0, { EXT(MOVBE) } }, // #96 [ref=1x]
  { 0, 0, { EXT(MMX), EXT(SSE2) } }, // #97 [ref=45x]
  { 0, 0, { EXT(MOVDIR64B) } }, // #98 [ref=1x]
  { 0, 0, { EXT(MOVDIRI) } }, // #99 [ref=1x]
  { 1, 0, { EXT(MMX), EXT(SSE2) } }, // #100 [ref=1x]
  { 0, 0, { EXT(BMI2) } }, // #101 [ref=7x]
  { 0, 0, { EXT(SSSE3) } }, // #102 [ref=16x]
  { 0, 0, { EXT(MMX2), EXT(SSE2) } }, // #103 [ref=10x]
  { 0, 0, { EXT(PCLMULQDQ) } }, // #104 [ref=1x]
  { 0, 1, { EXT(SSE4_2) } }, // #105 [ref=4x]
  { 0, 0, { EXT(PCONFIG) } }, // #106 [ref=1x]
  { 0, 0, { EXT(MMX2), EXT(SSE2), EXT(SSE4_1) } }, // #107 [ref=1x]
  { 0, 0, { EXT(3DNOW2) } }, // #108 [ref=5x]
  { 0, 0, { EXT(GEODE) } }, // #109 [ref=2x]
  { 0, 1, { EXT(POPCNT) } }, // #110 [ref=1x]
  { 0, 30, { 0 } }, // #111 [ref=3x]
  { 0, 0, { EXT(PREFETCHI) } }, // #112 [ref=2x]
  { 0, 1, { EXT(PREFETCHW) } }, // #113 [ref=1x]
  { 0, 1, { EXT(PREFETCHWT1) } }, // #114 [ref=1x]
  { 0, 20, { EXT(SEV_SNP) } }, // #115 [ref=3x]
  { 0, 1, { EXT(SSE4_1) } }, // #116 [ref=1x]
  { 0, 0, { EXT(PTWRITE) } }, // #117 [ref=1x]
  { 0, 31, { 0 } }, // #118 [ref=3x]
  { 0, 1, { EXT(SEV_SNP) } }, // #119 [ref=1x]
  { 0, 32, { 0 } }, // #120 [ref=2x]
  { 0, 0, { EXT(FSGSBASE) } }, // #121 [ref=4x]
  { 0, 0, { EXT(MSR), EXT(MSR_IMM) } }, // #122 [ref=1x]
  { 0, 0, { EXT(RDPID) } }, // #123 [ref=1x]
  { 0, 0, { EXT(OSPKE) } }, // #124 [ref=1x]
  { 0, 0, { EXT(RDPRU) } }, // #125 [ref=1x]
  { 0, 1, { EXT(RDRAND) } }, // #126 [ref=1x]
  { 0, 1, { EXT(RDSEED) } }, // #127 [ref=1x]
  { 0, 0, { EXT(RDTSC) } }, // #128 [ref=1x]
  { 0, 0, { EXT(RDTSCP) } }, // #129 [ref=1x]
  { 0, 33, { 0 } }, // #130 [ref=2x]
  { 0, 34, { EXT(LAHFSAHF) } }, // #131 [ref=1x]
  { 0, 0, { EXT(SEAM) } }, // #132 [ref=4x]
  { 0, 0, { EXT(SERIALIZE) } }, // #133 [ref=1x]
  { 0, 0, { EXT(SHA) } }, // #134 [ref=7x]
  { 0, 0, { EXT(SKINIT) } }, // #135 [ref=2x]
  { 0, 0, { EXT(AMX_COMPLEX) } }, // #136 [ref=2x]
  { 0, 0, { EXT(AMX_BF16) } }, // #137 [ref=1x]
  { 0, 0, { EXT(AMX_INT8) } }, // #138 [ref=4x]
  { 0, 0, { EXT(AMX_FP16) } }, // #139 [ref=1x]
  { 0, 1, { EXT(UINTR) } }, // #140 [ref=1x]
  { 0, 1, { EXT(WAITPKG) } }, // #141 [ref=2x]
  { 0, 0, { EXT(WAITPKG) } }, // #142 [ref=1x]
  { 0, 0, { EXT(AVX), EXT(AVX512_F), EXT(AVX512_VL) } }, // #143 [ref=71x]
  { 0, 0, { EXT(AVX512_FP16), EXT(AVX512_VL) } }, // #144 [ref=104x]
  { 0, 0, { EXT(AVX) } }, // #145 [ref=35x]
  { 0, 0, { EXT(AESNI), EXT(VAES), EXT(AVX), EXT(AVX512_F), EXT(AVX512_VL) } }, // #146 [ref=4x]
  { 0, 0, { EXT(AESNI), EXT(AVX) } }, // #147 [ref=2x]
  { 0, 0, { EXT(AVX512_F), EXT(AVX512_VL) } }, // #148 [ref=135x]
  { 0, 0, { EXT(AVX), EXT(AVX512_DQ), EXT(AVX512_VL) } }, // #149 [ref=12x]
  { 0, 0, { EXT(AVX_NE_CONVERT) } }, // #150 [ref=6x]
  { 0, 0, { EXT(AVX512_DQ), EXT(AVX512_VL) } }, // #151 [ref=42x]
  { 0, 0, { EXT(AVX2) } }, // #152 [ref=7x]
  { 0, 0, { EXT(AVX), EXT(AVX2), EXT(AVX512_F), EXT(AVX512_VL) } }, // #153 [ref=39x]
  { 0, 1, { EXT(AVX), EXT(AVX512_F), EXT(AVX512_VL) } }, // #154 [ref=4x]
  { 0, 1, { EXT(AVX512_FP16), EXT(AVX512_VL) } }, // #155 [ref=2x]
  { 0, 0, { EXT(AVX512_BF16), EXT(AVX512_VL) } }, // #156 [ref=2x]
  { 0, 0, { EXT(AVX_NE_CONVERT), EXT(AVX512_BF16), EXT(AVX512_VL) } }, // #157 [ref=1x]
  { 0, 0, { EXT(F16C), EXT(AVX512_F), EXT(AVX512_VL) } }, // #158 [ref=2x]
  { 0, 0, { EXT(AVX512_BW), EXT(AVX512_VL) } }, // #159 [ref=24x]
  { 0, 0, { EXT(FMA), EXT(AVX512_F), EXT(AVX512_VL) } }, // #160 [ref=60x]
  { 0, 0, { EXT(FMA4) } }, // #161 [ref=20x]
  { 0, 0, { EXT(XOP) } }, // #162 [ref=55x]
  { 0, 0, { EXT(AVX2), EXT(AVX512_F), EXT(AVX512_VL) } }, // #163 [ref=19x]
  { 0, 0, { EXT(GFNI), EXT(AVX), EXT(AVX512_F), EXT(AVX512_VL) } }, // #164 [ref=3x]
  { 0, 0, { EXT(SEV_ES) } }, // #165 [ref=1x]
  { 1, 0, { EXT(AVX), EXT(AVX512_F), EXT(AVX512_VL) } }, // #166 [ref=7x]
  { 1, 0, { EXT(AVX) } }, // #167 [ref=2x]
  { 1, 0, { EXT(AVX512_F), EXT(AVX512_VL) } }, // #168 [ref=4x]
  { 1, 0, { EXT(AVX512_BW), EXT(AVX512_VL) } }, // #169 [ref=2x]
  { 0, 0, { EXT(AVX), EXT(AVX2) } }, // #170 [ref=17x]
  { 0, 0, { EXT(AVX512_VL), EXT(AVX512_VP2INTERSECT) } }, // #171 [ref=2x]
  { 0, 0, { EXT(AVX), EXT(AVX2), EXT(AVX512_BW), EXT(AVX512_VL) } }, // #172 [ref=54x]
  { 0, 0, { EXT(AVX2), EXT(AVX512_BW), EXT(AVX512_VL) } }, // #173 [ref=2x]
  { 0, 0, { EXT(AVX512_CD), EXT(AVX512_VL) } }, // #174 [ref=6x]
  { 0, 0, { EXT(PCLMULQDQ), EXT(VPCLMULQDQ), EXT(AVX), EXT(AVX512_F), EXT(AVX512_VL) } }, // #175 [ref=1x]
  { 0, 1, { EXT(AVX) } }, // #176 [ref=7x]
  { 0, 0, { EXT(AVX512_VBMI2), EXT(AVX512_VL) } }, // #177 [ref=16x]
  { 0, 0, { EXT(AVX_VNNI_INT8) } }, // #178 [ref=6x]
  { 0, 0, { EXT(AVX_VNNI), EXT(AVX512_VL), EXT(AVX512_VNNI) } }, // #179 [ref=4x]
  { 0, 0, { EXT(AVX_VNNI_INT16) } }, // #180 [ref=6x]
  { 0, 0, { EXT(AVX512_VBMI), EXT(AVX512_VL) } }, // #181 [ref=4x]
  { 0, 0, { EXT(AVX), EXT(AVX512_BW), EXT(AVX512_VL) } }, // #182 [ref=4x]
  { 0, 0, { EXT(AVX_IFMA), EXT(AVX512_IFMA), EXT(AVX512_VL) } }, // #183 [ref=2x]
  { 0, 0, { EXT(AVX512_BITALG), EXT(AVX512_VL) } }, // #184 [ref=3x]
  { 0, 0, { EXT(AVX512_VL), EXT(AVX512_VPOPCNTDQ) } }, // #185 [ref=2x]
  { 0, 0, { EXT(SHA512), EXT(AVX) } }, // #186 [ref=3x]
  { 0, 0, { EXT(SM3), EXT(AVX) } }, // #187 [ref=3x]
  { 0, 0, { EXT(SM4), EXT(AVX) } }, // #188 [ref=2x]
  { 0, 0, { EXT(WBNOINVD) } }, // #189 [ref=1x]
  { 0, 0, { EXT(MSR) } }, // #190 [ref=1x]
  { 0, 0, { EXT(RTM) } }, // #191 [ref=3x]
  { 0, 0, { EXT(XSAVE) } }, // #192 [ref=6x]
  { 0, 0, { EXT(TSXLDTRK) } }, // #193 [ref=2x]
  { 0, 0, { EXT(XSAVES) } }, // #194 [ref=4x]
  { 0, 0, { EXT(XSAVEC) } }, // #195 [ref=2x]
  { 0, 0, { EXT(XSAVEOPT) } }, // #196 [ref=2x]
  { 0, 1, { EXT(RTM) } }  // #197 [ref=1x]
};
#undef EXT

#define FLAG(VAL) uint32_t(CpuRWFlags::kX86_##VAL)
const InstDB::RWFlagsInfoTable InstDB::rw_flags_info_table[] = {
  { 0, 0 }, // #0 [ref=1345x]
  { 0, FLAG(AF) | FLAG(CF) | FLAG(OF) | FLAG(PF) | FLAG(SF) | FLAG(ZF) }, // #1 [ref=103x]
  { FLAG(CF), FLAG(AF) | FLAG(CF) | FLAG(OF) | FLAG(PF) | FLAG(SF) | FLAG(ZF) }, // #2 [ref=2x]
  { FLAG(CF), FLAG(CF) }, // #3 [ref=2x]
  { FLAG(OF), FLAG(OF) }, // #4 [ref=1x]
  { 0, FLAG(ZF) }, // #5 [ref=7x]
  { 0, FLAG(AF) | FLAG(CF) | FLAG(OF) | FLAG(PF) | FLAG(SF) }, // #6 [ref=4x]
  { 0, FLAG(AC) }, // #7 [ref=2x]
  { 0, FLAG(CF) }, // #8 [ref=2x]
  { 0, FLAG(DF) }, // #9 [ref=2x]
  { 0, FLAG(IF) }, // #10 [ref=2x]
  { FLAG(CF), 0 }, // #11 [ref=6x]
  { FLAG(CF) | FLAG(ZF), 0 }, // #12 [ref=6x]
  { FLAG(OF) | FLAG(SF), 0 }, // #13 [ref=6x]
  { FLAG(OF) | FLAG(SF) | FLAG(ZF), 0 }, // #14 [ref=6x]
  { FLAG(OF), 0 }, // #15 [ref=7x]
  { FLAG(PF), 0 }, // #16 [ref=6x]
  { FLAG(SF), 0 }, // #17 [ref=6x]
  { FLAG(ZF), 0 }, // #18 [ref=8x]
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
const InstRWFlags InstDB::inst_flags_table[] = {
  InstRWFlags(FLAG(None)), // #0 [ref=1612x]
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
const InstNameIndex InstDB::_inst_name_index = {{
  { Inst::kIdAaa          , Inst::kIdAxor          + 1 },
  { Inst::kIdBextr        , Inst::kIdBzhi          + 1 },
  { Inst::kIdCall         , Inst::kIdCwde          + 1 },
  { Inst::kIdDaa          , Inst::kIdDpps          + 1 },
  { Inst::kIdEmms         , Inst::kIdExtrq         + 1 },
  { Inst::kIdF2xm1        , Inst::kIdFyl2xp1       + 1 },
  { Inst::kIdGetsec       , Inst::kIdGf2p8mulb     + 1 },
  { Inst::kIdHaddpd       , Inst::kIdHsubps        + 1 },
  { Inst::kIdIdiv         , Inst::kIdIretq         + 1 },
  { Inst::kIdJb           , Inst::kIdJz            + 1 },
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
  { Inst::kIdVaddpd       , Inst::kIdVzeroupper    + 1 },
  { Inst::kIdWbinvd       , Inst::kIdWrussq        + 1 },
  { Inst::kIdXabort       , Inst::kIdXtest         + 1 },
  { Inst::kIdNone         , Inst::kIdNone          + 1 },
  { Inst::kIdNone         , Inst::kIdNone          + 1 }
}, uint16_t(17)};

const char InstDB::_inst_name_string_table[] =
  "\x63\x6D\x6F\x76\x62\x0C\x63\x6D\x6F\x76\x2E\x62\x7C\x6E\x61\x65\x7C\x63\x63\x6D\x6F\x76\x62\x65\x0A\x63\x6D\x6F\x76"
  "\x2E\x62\x65\x7C\x6E\x61\x63\x6D\x6F\x76\x6C\x0A\x63\x6D\x6F\x76\x2E\x6C\x7C\x6E\x67\x65\x63\x6D\x6F\x76\x6C\x65\x0A"
  "\x63\x6D\x6F\x76\x2E\x6C\x65\x7C\x6E\x67\x63\x6D\x6F\x76\x6E\x62\x0D\x63\x6D\x6F\x76\x2E\x6E\x62\x7C\x61\x65\x7C\x6E"
  "\x63\x63\x6D\x6F\x76\x6E\x62\x65\x0A\x63\x6D\x6F\x76\x2E\x6E\x62\x65\x7C\x61\x63\x6D\x6F\x76\x6E\x6C\x0A\x63\x6D\x6F"
  "\x76\x2E\x6E\x6C\x7C\x67\x65\x63\x6D\x6F\x76\x6E\x6C\x65\x0A\x63\x6D\x6F\x76\x2E\x6E\x6C\x65\x7C\x67\x63\x6D\x6F\x76"
  "\x6E\x70\x0A\x63\x6D\x6F\x76\x2E\x6E\x70\x7C\x70\x6F\x63\x6D\x6F\x76\x6E\x7A\x0A\x63\x6D\x6F\x76\x2E\x6E\x7A\x7C\x6E"
  "\x65\x63\x6D\x6F\x76\x70\x09\x63\x6D\x6F\x76\x2E\x70\x7C\x70\x65\x63\x6D\x6F\x76\x7A\x08\x63\x6D\x6F\x76\x2E\x7A\x7C"
  "\x65\x6A\x62\x0A\x6A\x62\x7C\x6A\x6E\x61\x65\x7C\x6A\x63\x6A\x62\x65\x07\x6A\x62\x65\x7C\x6A\x6E\x61\x6A\x6C\x07\x6A"
  "\x6C\x7C\x6A\x6E\x67\x65\x6A\x6C\x65\x07\x6A\x6C\x65\x7C\x6A\x6E\x67\x6A\x6E\x62\x0B\x6A\x6E\x62\x7C\x6A\x61\x65\x7C"
  "\x6A\x6E\x63\x6A\x6E\x62\x65\x07\x6A\x6E\x62\x65\x7C\x6A\x61\x6A\x6E\x6C\x07\x6A\x6E\x6C\x7C\x6A\x67\x65\x6A\x6E\x6C"
  "\x65\x07\x6A\x6E\x6C\x65\x7C\x6A\x67\x6A\x6E\x70\x07\x6A\x6E\x70\x7C\x6A\x70\x6F\x6A\x6E\x7A\x07\x6A\x6E\x7A\x7C\x6A"
  "\x6E\x65\x6A\x70\x06\x6A\x70\x7C\x6A\x70\x65\x6A\x7A\x05\x6A\x7A\x7C\x6A\x65\x73\x65\x74\x62\x0B\x73\x65\x74\x2E\x62"
  "\x7C\x6E\x61\x65\x7C\x63\x73\x65\x74\x62\x65\x09\x73\x65\x74\x2E\x62\x65\x7C\x6E\x61\x73\x65\x74\x6C\x09\x73\x65\x74"
  "\x2E\x6C\x7C\x6E\x67\x65\x73\x65\x74\x6C\x65\x09\x73\x65\x74\x2E\x6C\x65\x7C\x6E\x67\x73\x65\x74\x6E\x62\x0C\x73\x65"
  "\x74\x2E\x6E\x62\x7C\x61\x65\x7C\x6E\x63\x73\x65\x74\x6E\x62\x65\x09\x73\x65\x74\x2E\x6E\x62\x65\x7C\x61\x73\x65\x74"
  "\x6E\x6C\x09\x73\x65\x74\x2E\x6E\x6C\x7C\x67\x65\x73\x65\x74\x6E\x6C\x65\x09\x73\x65\x74\x2E\x6E\x6C\x65\x7C\x67\x73"
  "\x65\x74\x6E\x70\x09\x73\x65\x74\x2E\x6E\x70\x7C\x70\x6F\x73\x65\x74\x6E\x7A\x09\x73\x65\x74\x2E\x6E\x7A\x7C\x6E\x65"
  "\x73\x65\x74\x70\x08\x73\x65\x74\x2E\x70\x7C\x70\x65\x73\x65\x74\x7A\x07\x73\x65\x74\x2E\x7A\x7C\x65\x76\x67\x66\x32"
  "\x70\x38\x61\x66\x66\x69\x6E\x65\x69\x6E\x76\x71\x62\x76\x61\x65\x73\x6B\x65\x79\x67\x65\x6E\x61\x73\x73\x69\x73\x76"
  "\x62\x72\x6F\x61\x64\x63\x61\x73\x74\x66\x33\x32\x78\x34\x36\x34\x78\x32\x36\x34\x78\x34\x69\x33\x32\x78\x32\x69\x33"
  "\x32\x78\x34\x69\x33\x32\x78\x38\x69\x36\x34\x78\x32\x69\x36\x34\x78\x34\x76\x70\x62\x72\x6F\x61\x64\x63\x61\x73\x74"
  "\x6D\x62\x32\x77\x32\x64\x76\x62\x63\x73\x74\x6E\x65\x62\x66\x31\x36\x32\x70\x31\x32\x38\x69\x31\x32\x38\x76\x63\x76"
  "\x74\x6E\x65\x32\x70\x73\x32\x76\x63\x76\x74\x6E\x65\x65\x62\x66\x31\x36\x76\x63\x76\x74\x6E\x65\x6F\x62\x66\x31\x36"
  "\x76\x66\x6D\x61\x64\x64\x73\x75\x62\x31\x33\x32\x70\x68\x32\x31\x33\x70\x64\x32\x31\x33\x70\x68\x32\x31\x33\x70\x73"
  "\x32\x33\x31\x70\x64\x32\x33\x31\x70\x68\x32\x33\x31\x70\x73\x76\x66\x6D\x73\x75\x62\x61\x64\x64\x31\x33\x32\x76\x70"
  "\x6D\x75\x6C\x74\x69\x73\x68\x69\x66\x74\x76\x63\x76\x74\x6E\x65\x70\x73\x32\x76\x65\x78\x74\x72\x61\x63\x76\x65\x78"
  "\x74\x72\x61\x63\x74\x66\x76\x70\x32\x69\x6E\x74\x65\x72\x73\x65\x63\x74\x74\x63\x6D\x6D\x69\x6D\x66\x70\x31\x36\x74"
  "\x63\x6D\x6D\x72\x6C\x66\x70\x31\x36\x73\x68\x32\x70\x73\x73\x64\x70\x68\x32\x70\x73\x76\x66\x6E\x6D\x61\x64\x64\x31"
  "\x33\x32\x32\x31\x33\x73\x64\x32\x31\x33\x73\x68\x32\x31\x33\x73\x73\x32\x33\x31\x73\x64\x32\x33\x31\x73\x68\x32\x33"
  "\x31\x73\x73\x76\x66\x6E\x6D\x73\x75\x62\x31\x33\x32\x76\x69\x6E\x73\x65\x72\x76\x69\x6E\x73\x65\x72\x74\x66\x76\x70"
  "\x73\x68\x75\x66\x62\x69\x74\x71\x76\x73\x68\x61\x35\x31\x32\x72\x6E\x64\x70\x72\x65\x66\x65\x74\x63\x68\x69\x74\x30"
  "\x6E\x74\x61\x77\x74\x31\x73\x61\x76\x65\x70\x72\x65\x76\x73\x73\x73\x68\x61\x32\x35\x36\x72\x6E\x64\x74\x69\x6C\x65"
  "\x6C\x6F\x61\x64\x64\x74\x69\x6C\x65\x72\x65\x6C\x65\x76\x61\x65\x73\x64\x65\x63\x6C\x76\x61\x65\x73\x65\x6E\x63\x6C"
  "\x76\x63\x6F\x6D\x70\x72\x65\x73\x73\x76\x63\x76\x74\x74\x70\x64\x32\x75\x64\x71\x71\x76\x63\x76\x74\x74\x70\x68\x32"
  "\x75\x71\x71\x76\x63\x76\x74\x74\x70\x73\x76\x63\x76\x74\x74\x73\x64\x32\x75\x76\x63\x76\x74\x74\x73\x68\x32\x75\x76"
  "\x63\x76\x74\x74\x73\x73\x32\x75\x76\x66\x69\x78\x75\x70\x69\x6D\x6D\x76\x66\x6D\x61\x64\x64\x31\x33\x32\x76\x66\x6D"
  "\x73\x75\x62\x31\x33\x32\x76\x6D\x61\x73\x6B\x6D\x6F\x76\x64\x71\x76\x70\x63\x6F\x6D\x70\x72\x65\x73\x73\x76\x70\x63"
  "\x6F\x6E\x66\x6C\x69\x63\x74\x76\x70\x68\x6D\x69\x6E\x70\x6F\x73\x75\x76\x70\x6D\x61\x64\x64\x35\x32\x68\x6C\x75\x71"
  "\x76\x70\x73\x63\x61\x74\x74\x65\x72\x71\x64\x76\x70\x75\x6E\x70\x63\x6B\x68\x71\x6C\x71\x64\x71\x76\x72\x6E\x64\x73"
  "\x63\x61\x6C\x65\x76\x73\x63\x61\x74\x74\x65\x72\x64\x71\x70\x64\x71\x70\x73\x6D\x73\x67\x31\x6D\x73\x67\x32\x63\x6C"
  "\x66\x6C\x75\x73\x68\x6F\x70\x63\x6D\x70\x6E\x62\x65\x78\x63\x6D\x70\x6E\x6C\x65\x78\x63\x6D\x70\x78\x63\x68\x67\x31"
  "\x36\x74\x32\x74\x69\x6C\x65\x73\x74\x6F\x72\x65\x76\x63\x76\x74\x70\x64\x76\x63\x76\x74\x70\x68\x32\x70\x73\x76\x63"
  "\x76\x74\x70\x73\x32\x70\x68\x76\x63\x76\x74\x73\x64\x32\x75\x76\x63\x76\x74\x73\x68\x32\x75\x76\x63\x76\x74\x73\x73"
  "\x32\x75\x32\x64\x71\x32\x71\x71\x76\x63\x76\x74\x75\x64\x71\x32\x76\x63\x76\x74\x75\x71\x71\x32\x76\x63\x76\x74\x75"
  "\x73\x69\x32\x76\x66\x63\x6D\x61\x64\x64\x63\x76\x66\x70\x63\x6C\x61\x73\x73\x76\x67\x61\x74\x68\x65\x72\x64\x76\x67"
  "\x65\x74\x6D\x61\x6E\x6D\x75\x6C\x62\x76\x70\x63\x6C\x6D\x75\x76\x70\x63\x6D\x70\x65\x73\x74\x72\x76\x70\x63\x6D\x70"
  "\x69\x73\x74\x72\x76\x70\x65\x72\x6D\x32\x66\x76\x70\x65\x72\x6D\x69\x6C\x32\x76\x70\x67\x61\x74\x68\x65\x72\x76\x70"
  "\x6D\x61\x63\x73\x73\x64\x71\x76\x70\x6D\x61\x64\x63\x73\x73\x77\x75\x62\x73\x77\x76\x70\x6D\x61\x73\x6B\x6D\x6F\x76"
  "\x70\x74\x65\x72\x6E\x6C\x6F\x67\x62\x77\x77\x64\x6C\x62\x77\x6C\x64\x71\x6C\x77\x64\x76\x72\x73\x71\x72\x74\x31\x34"
  "\x76\x73\x68\x75\x66\x76\x73\x68\x75\x66\x66\x76\x7A\x65\x72\x6F\x75\x70\x78\x73\x61\x76\x65\x6F\x70\x74\x63\x6D\x70"
  "\x62\x65\x78\x63\x6D\x70\x6C\x65\x78\x63\x6D\x70\x6E\x62\x78\x63\x6D\x70\x6E\x6C\x78\x63\x6D\x70\x6E\x6F\x78\x63\x6D"
  "\x70\x6E\x70\x78\x63\x6D\x70\x6E\x73\x78\x63\x6D\x70\x6E\x7A\x78\x38\x62\x32\x70\x69\x66\x78\x72\x73\x74\x6F\x72\x6C"
  "\x64\x74\x69\x6C\x65\x63\x66\x6D\x6F\x76\x64\x69\x72\x36\x34\x70\x76\x61\x6C\x69\x64\x61\x72\x6D\x70\x61\x64\x6A\x75"
  "\x72\x6D\x70\x75\x70\x64\x61\x73\x65\x72\x69\x61\x6C\x69\x73\x68\x61\x31\x6E\x65\x78\x73\x68\x61\x31\x72\x6E\x64\x73"
  "\x73\x74\x74\x69\x6C\x65\x63\x66\x74\x64\x70\x62\x66\x31\x36\x74\x64\x70\x66\x70\x31\x36\x76\x61\x64\x64\x73\x75\x62"
  "\x76\x62\x6C\x65\x6E\x64\x6D\x76\x70\x64\x76\x63\x76\x74\x64\x71\x32\x75\x77\x76\x63\x76\x74\x71\x71\x32\x76\x63\x76"
  "\x74\x73\x69\x32\x76\x63\x76\x74\x75\x77\x76\x64\x62\x70\x73\x61\x64\x76\x64\x70\x62\x66\x31\x36\x76\x65\x78\x70\x61"
  "\x6E\x64\x76\x66\x63\x6D\x75\x6C\x63\x63\x70\x68\x63\x73\x68\x76\x67\x65\x74\x65\x78\x70\x76\x6D\x6F\x76\x64\x71\x61"
  "\x75\x31\x36\x75\x33\x32\x75\x36\x34\x76\x6D\x6F\x76\x6D\x73\x6B\x76\x6D\x6F\x76\x6E\x74\x76\x6D\x6F\x76\x73\x68\x64"
  "\x76\x6D\x6F\x76\x73\x6C\x64\x76\x70\x61\x63\x6B\x73\x73\x64\x77\x62\x76\x70\x61\x63\x6B\x75\x73\x77\x62\x76\x70\x62"
  "\x6C\x65\x6E\x64\x6D\x64\x76\x70\x64\x70\x62\x73\x73\x75\x64\x73\x76\x70\x64\x70\x62\x75\x73\x76\x70\x64\x70\x77\x73"
  "\x73\x76\x70\x64\x70\x77\x75\x73\x32\x70\x64\x76\x70\x65\x72\x6D\x74\x76\x70\x65\x78\x70\x61\x6E\x76\x70\x68\x61\x64"
  "\x64\x75\x62\x77\x71\x64\x71\x68\x76\x70\x6D\x6F\x76\x6D\x73\x6B\x76\x70\x6D\x6F\x76\x73\x78\x62\x76\x70\x6D\x6F\x76"
  "\x75\x73\x71\x77\x76\x70\x6D\x6F\x76\x7A\x78\x62\x76\x70\x6D\x75\x6C\x68\x72\x76\x70\x74\x65\x73\x74\x6E\x6D\x71\x76"
  "\x72\x65\x64\x75\x63\x65\x76\x73\x63\x61\x6C\x65\x66\x76\x73\x6D\x33\x72\x6E\x64\x76\x73\x6D\x34\x72\x6E\x64\x73\x76"
  "\x75\x6E\x70\x63\x6B\x68\x6C\x70\x64\x6C\x70\x73\x78\x72\x65\x73\x6C\x64\x74\x72\x73\x36\x34\x78\x73\x75\x73\x6C\x64"
  "\x74\x72\x63\x6C\x64\x65\x6D\x6F\x63\x6C\x72\x73\x73\x62\x73\x63\x6D\x70\x62\x78\x63\x6D\x70\x6C\x78\x63\x6D\x70\x6F"
  "\x78\x63\x6D\x70\x70\x78\x63\x6D\x70\x73\x78\x63\x6D\x70\x7A\x78\x63\x76\x74\x70\x69\x66\x78\x73\x61\x76\x65\x6B\x6F"
  "\x72\x74\x65\x73\x74\x77\x6B\x73\x68\x69\x66\x74\x72\x62\x6B\x75\x6E\x70\x63\x6B\x6D\x6F\x6E\x69\x74\x6F\x72\x70\x66"
  "\x72\x63\x70\x69\x70\x66\x72\x73\x71\x69\x72\x74\x76\x72\x64\x66\x73\x62\x72\x64\x67\x73\x62\x73\x73\x70\x73\x65\x61"
  "\x6D\x63\x61\x6C\x73\x65\x6E\x64\x75\x69\x73\x65\x74\x73\x73\x62\x73\x73\x79\x73\x65\x73\x79\x73\x65\x78\x75\x6D\x76"
  "\x63\x76\x74\x77\x76\x66\x6D\x75\x6C\x76\x6C\x64\x6D\x78\x63\x73\x76\x6D\x6C\x61\x75\x6E\x64\x75\x70\x75\x38\x76\x6D"
  "\x6F\x76\x68\x76\x6D\x6F\x76\x6C\x68\x76\x6D\x70\x73\x61\x64\x76\x6D\x72\x65\x73\x75\x6D\x76\x70\x61\x64\x64\x75\x76"
  "\x70\x61\x6C\x69\x67\x6E\x67\x74\x62\x67\x74\x64\x67\x74\x71\x67\x74\x77\x32\x62\x62\x64\x62\x71\x76\x70\x68\x73\x75"
  "\x62\x76\x70\x6C\x7A\x63\x6E\x62\x32\x6D\x64\x32\x6D\x71\x32\x6D\x77\x32\x6D\x76\x70\x6F\x70\x63\x6E\x76\x70\x73\x68"
  "\x6C\x64\x76\x71\x76\x70\x73\x68\x72\x64\x76\x77\x68\x77\x76\x70\x73\x75\x62\x75\x76\x72\x61\x6E\x67\x65\x76\x72\x63"
  "\x70\x31\x34\x76\x72\x6F\x75\x6E\x64\x76\x73\x6D\x34\x6B\x65\x79\x76\x73\x74\x6D\x78\x63\x73\x76\x75\x63\x6F\x6D\x69"
  "\x61\x6C\x6C\x77\x62\x6E\x6F\x69\x6E\x77\x72\x66\x73\x62\x77\x72\x67\x73\x62\x63\x36\x34\x62\x6C\x63\x66\x69\x62\x6C"
  "\x73\x66\x69\x65\x6E\x64\x62\x72\x65\x6E\x71\x63\x6D\x66\x63\x6D\x6F\x76\x6E\x75\x66\x64\x65\x63\x73\x66\x69\x6E\x63"
  "\x73\x66\x6E\x73\x74\x65\x66\x72\x6E\x64\x66\x73\x69\x6E\x63\x66\x75\x63\x6F\x6D\x66\x79\x6C\x32\x78\x69\x6E\x63\x73"
  "\x73\x70\x71\x69\x6E\x76\x6C\x69\x6E\x76\x6C\x70\x69\x6E\x76\x70\x63\x69\x6E\x76\x76\x70\x6D\x63\x6F\x6D\x6D\x6D\x6F"
  "\x76\x71\x70\x61\x76\x67\x75\x70\x66\x63\x6D\x70\x65\x70\x66\x70\x6E\x61\x70\x74\x77\x72\x69\x73\x65\x61\x6D\x6F\x73"
  "\x65\x61\x6D\x72\x73\x79\x73\x63\x73\x79\x73\x72\x65\x74\x64\x70\x62\x75\x74\x6C\x62\x73\x79\x76\x61\x65\x73\x69\x76"
  "\x61\x6C\x69\x67\x76\x61\x6E\x64\x6E\x76\x63\x6F\x6D\x69\x76\x66\x72\x63\x7A\x76\x68\x61\x64\x64\x76\x68\x73\x75\x62"
  "\x76\x6D\x63\x6C\x65\x76\x6D\x67\x65\x78\x76\x6D\x6D\x63\x76\x6D\x6F\x76\x61\x76\x6D\x6F\x76\x75\x76\x6D\x70\x74\x76"
  "\x6D\x77\x72\x69\x76\x70\x61\x6E\x64\x76\x70\x65\x78\x74\x72\x77\x76\x70\x69\x6E\x73\x76\x70\x6D\x61\x78\x76\x70\x6D"
  "\x69\x6E\x76\x70\x72\x6F\x6C\x76\x70\x72\x6F\x72\x76\x70\x73\x61\x64\x76\x70\x73\x69\x67\x76\x70\x73\x6C\x76\x70\x73"
  "\x6C\x6C\x76\x70\x73\x72\x61\x76\x70\x73\x72\x6C\x76\x73\x71\x72\x76\x74\x65\x73";


const uint32_t InstDB::_inst_name_index_table[] = {
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
  0x22AC629E, // Large 'addsub|pd'.
  0x2282629E, // Large 'addsub|ps'.
  0x800C3C81, // Small 'adox'.
  0x86524CA1, // Small 'aesdec'.
  0x322D73AE, // Large 'aesdecl|ast'.
  0x86E2CCA1, // Small 'aesenc'.
  0x322D73B6, // Large 'aesencl|ast'.
  0x86D4CCA1, // Small 'aesimc'.
  0x1154E218, // Large 'aeskeygenassis|t'.
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
  0x28BA58CF, // Large 'blcfi|ll'.
  0x80048D82, // Small 'blci'.
  0x80348D82, // Small 'blcic'.
  0x97368D82, // Small 'blcmsk'.
  0x80098D82, // Small 'blcs'.
  0x22AC563C, // Large 'blend|pd'.
  0x2282563C, // Large 'blend|ps'.
  0x3642563C, // Large 'blend|vpd'.
  0x3364563C, // Large 'blend|vps'.
  0x28BA58D4, // Large 'blsfi|ll'.
  0x8004CD82, // Small 'blsi'.
  0x8034CD82, // Small 'blsic'.
  0x9736CD82, // Small 'blsmsk'.
  0x80094D82, // Small 'blsr'.
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
  0x22FD677C, // Large 'cldemo|te'.
  0x00007486, // Large 'clflush'.
  0x11549486, // Large 'clflushop|t'.
  0x80049D83, // Small 'clgi'.
  0x80002583, // Small 'cli'.
  0x121D7782, // Large 'clrssbs|y'.
  0x8009D183, // Small 'clts'.
  0x8004D583, // Small 'clui'.
  0x80015D83, // Small 'clwb'.
  0x9F22E983, // Small 'clzero'.
  0x80000DA3, // Small 'cmc'.
  0x0FFF5000, // Large 'cmovb' + 'cmov.b|nae|c'
  0x0FFF6012, // Large 'cmovbe' + 'cmov.be|na'
  0x0FFF5023, // Large 'cmovl' + 'cmov.l|nge'
  0x0FFF6033, // Large 'cmovle' + 'cmov.le|ng'
  0x0FFF6044, // Large 'cmovnb' + 'cmov.nb|ae|nc'
  0x0FFF7058, // Large 'cmovnbe' + 'cmov.nbe|a'
  0x0FFF606A, // Large 'cmovnl' + 'cmov.nl|ge'
  0x0FFF707B, // Large 'cmovnle' + 'cmov.nle|g'
  0x9EEB3DA3, // Small 'cmovno'.
  0x0FFF608D, // Large 'cmovnp' + 'cmov.np|po'
  0xA6EB3DA3, // Small 'cmovns'.
  0x0FFF609E, // Large 'cmovnz' + 'cmov.nz|ne'
  0x80FB3DA3, // Small 'cmovo'.
  0x0FFF50AF, // Large 'cmovp' + 'cmov.p|pe'
  0x813B3DA3, // Small 'cmovs'.
  0x0FFF50BE, // Large 'cmovz' + 'cmov.z|e'
  0x800041A3, // Small 'cmp'.
  0x329E65A7, // Large 'cmpbex|add'.
  0x329E5789, // Large 'cmpbx|add'.
  0x329E65AD, // Large 'cmplex|add'.
  0x329E578E, // Large 'cmplx|add'.
  0x329E748F, // Large 'cmpnbex|add'.
  0x329E65B3, // Large 'cmpnbx|add'.
  0x329E7496, // Large 'cmpnlex|add'.
  0x329E65B9, // Large 'cmpnlx|add'.
  0x329E65BF, // Large 'cmpnox|add'.
  0x329E65C5, // Large 'cmpnpx|add'.
  0x329E65CB, // Large 'cmpnsx|add'.
  0x329E65D1, // Large 'cmpnzx|add'.
  0x329E5793, // Large 'cmpox|add'.
  0x804841A3, // Small 'cmppd'.
  0x813841A3, // Small 'cmpps'.
  0x329E5798, // Large 'cmppx|add'.
  0x8009C1A3, // Small 'cmps'.
  0x8049C1A3, // Small 'cmpsd'.
  0x8139C1A3, // Small 'cmpss'.
  0x329E579D, // Large 'cmpsx|add'.
  0x0000749D, // Large 'cmpxchg'.
  0x1004949D, // Large 'cmpxchg16|b'.
  0x25D7749D, // Large 'cmpxchg|8b'.
  0x329E57A2, // Large 'cmpzx|add'.
  0x8934B5E3, // Small 'comisd'.
  0xA734B5E3, // Small 'comiss'.
  0x8044D603, // Small 'cpuid'.
  0x80003E23, // Small 'cqo'.
  0x81DF0E43, // Small 'crc32'.
  0x22AC6646, // Large 'cvtdq2|pd'.
  0x22826646, // Large 'cvtdq2|ps'.
  0x34E154B2, // Large 'cvtpd|2dq'.
  0x35D954B2, // Large 'cvtpd|2pi'.
  0x328154B2, // Large 'cvtpd|2ps'.
  0x36F157A7, // Large 'cvtpi|2pd'.
  0x328157A7, // Large 'cvtpi|2ps'.
  0x23CF64C1, // Large 'cvtps2|dq'.
  0x122B74C1, // Large 'cvtps2p|d'.
  0x120F74C1, // Large 'cvtps2p|i'.
  0x222364CA, // Large 'cvtsd2|si'.
  0x222264CA, // Large 'cvtsd2|ss'.
  0x231D6656, // Large 'cvtsi2|sd'.
  0x22226656, // Large 'cvtsi2|ss'.
  0x231D64DA, // Large 'cvtss2|sd'.
  0x222364DA, // Large 'cvtss2|si'.
  0x23CF73C7, // Large 'cvttpd2|dq'.
  0x240473C7, // Large 'cvttpd2|pi'.
  0x34E163DE, // Large 'cvttps|2dq'.
  0x35D963DE, // Large 'cvttps|2pi'.
  0x222373E5, // Large 'cvttsd2|si'.
  0x222373F7, // Large 'cvttss2|si'.
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
  0x223158D9, // Large 'endbr|32'.
  0x223558D9, // Large 'endbr|64'.
  0x88D1C5C5, // Small 'enqcmd'.
  0x22A058DE, // Large 'enqcm|ds'.
  0x8122D1C5, // Small 'enter'.
  0x228272F0, // Large 'extract|ps'.
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
  0x40143500, // Large 'fcm|ovbe'.
  0x8B67B466, // Small 'fcmove'.
  0x40463500, // Large 'fcm|ovnb'.
  0x505A3500, // Large 'fcm|ovnbe'.
  0x20AD58E3, // Large 'fcmov|ne'.
  0x28E858E3, // Large 'fcmov|nu'.
  0xAB67B466, // Small 'fcmovu'.
  0x8006BC66, // Small 'fcom'.
  0x8096BC66, // Small 'fcomi'.
  0xA096BC66, // Small 'fcomip'.
  0x8106BC66, // Small 'fcomp'.
  0xA106BC66, // Small 'fcompp'.
  0x8009BC66, // Small 'fcos'.
  0x21EF58EA, // Large 'fdecs|tp'.
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
  0x21EF58EF, // Large 'fincs|tp'.
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
  0x221358F4, // Large 'fnste|nv'.
  0xAF3A4DC6, // Small 'fnstsw'.
  0x9C1A0606, // Small 'fpatan'.
  0x80D2CA06, // Small 'fprem'.
  0xB8D2CA06, // Small 'fprem1'.
  0x80E0D206, // Small 'fptan'.
  0x32FB48F9, // Large 'frnd|int'.
  0xA4FA4E46, // Small 'frstor'.
  0x805B0666, // Small 'fsave'.
  0x8AC08E66, // Small 'fscale'.
  0x80072666, // Small 'fsin'.
  0x21DD58FD, // Large 'fsinc|os'.
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
  0x27D45902, // Large 'fucom|ip'.
  0xA0D78EA6, // Small 'fucomp'.
  0x279A5902, // Large 'fucom|pp'.
  0x814486E6, // Small 'fwait'.
  0x80068706, // Small 'fxam'.
  0x80040F06, // Small 'fxch'.
  0x000075DC, // Large 'fxrstor'.
  0x223575DC, // Large 'fxrstor|64'.
  0x8B60CF06, // Small 'fxsave'.
  0x223567AC, // Large 'fxsave|64'.
  0x52F225DC, // Large 'fx|tract'.
  0x818EB326, // Small 'fyl2x'.
  0x22735907, // Large 'fyl2x|p1'.
  0x8659D0A7, // Small 'getsec'.
  0x1004F207, // Large 'gf2p8affineinvq|b'.
  0x2215B207, // Large 'gf2p8affine|qb'.
  0x451E5207, // Large 'gf2p8|mulb'.
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
  0x22AC590C, // Large 'incss|pd'.
  0x2911590C, // Large 'incss|pq'.
  0x80004DC9, // Small 'ins'.
  0x2282635D, // Large 'insert|ps'.
  0x1215635D, // Large 'insert|q'.
  0x800051C9, // Small 'int'.
  0x800F51C9, // Small 'int3'.
  0x8007D1C9, // Small 'into'.
  0x800259C9, // Small 'invd'.
  0xA902D9C9, // Small 'invept'.
  0x8F0659C9, // Small 'invlpg'.
  0x354A4913, // Large 'invl|pga'.
  0x25775917, // Large 'invlp|gb'.
  0x25F7591C, // Large 'invpc|id'.
  0x25F75921, // Large 'invvp|id'.
  0x800A1649, // Small 'iret'.
  0x804A1649, // Small 'iretd'.
  0x811A1649, // Small 'iretq'.
  0x0FFF20CC, // Large 'jb' + 'jb|jnae|jc'
  0x0FFF30D9, // Large 'jbe' + 'jbe|jna'
  0x81AC0CAA, // Small 'jecxz'.
  0x0FFF20E4, // Large 'jl' + 'jl|jnge'
  0x0FFF30EE, // Large 'jle' + 'jle|jng'
  0x800041AA, // Small 'jmp'.
  0x0FFF30F9, // Large 'jnb' + 'jnb|jae|jnc'
  0x0FFF4108, // Large 'jnbe' + 'jnbe|ja'
  0x0FFF3114, // Large 'jnl' + 'jnl|jge'
  0x0FFF411F, // Large 'jnle' + 'jnle|jg'
  0x80003DCA, // Small 'jno'.
  0x0FFF312B, // Large 'jnp' + 'jnp|jpo'
  0x80004DCA, // Small 'jns'.
  0x0FFF3136, // Large 'jnz' + 'jnz|jne'
  0x800001EA, // Small 'jo'.
  0x0FFF2141, // Large 'jp' + 'jp|jpe'
  0x8000026A, // Small 'js'.
  0x0FFF214A, // Large 'jz' + 'jz|je'
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
  0x215467B2, // Large 'kortes|tb'.
  0x262667B2, // Large 'kortes|td'.
  0x236C67B2, // Large 'kortes|tq'.
  0x27B867B2, // Large 'kortes|tw'.
  0x800BC9EB, // Small 'korw'.
  0x252067BA, // Large 'kshift|lb'.
  0x257F67BA, // Large 'kshift|ld'.
  0x246267BA, // Large 'kshift|lq'.
  0x258267BA, // Large 'kshift|lw'.
  0x27C067BA, // Large 'kshift|rb'.
  0x122B77BA, // Large 'kshiftr|d'.
  0x121577BA, // Large 'kshiftr|q'.
  0x126477BA, // Large 'kshiftr|w'.
  0x8549968B, // Small 'ktestb'.
  0x8949968B, // Small 'ktestd'.
  0xA349968B, // Small 'ktestq'.
  0xAF49968B, // Small 'ktestw'.
  0x257867C2, // Large 'kunpck|bw'.
  0x23CF67C2, // Large 'kunpck|dq'.
  0x257A67C2, // Large 'kunpck|wd'.
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
  0x12286815, // Large 'ldmxcs|r'.
  0x80004C8C, // Small 'lds'.
  0x103185E3, // Large 'ldtilecf|g'.
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
  0x12A2941B, // Large 'maskmovdq|u'.
  0x1215741B, // Large 'maskmov|q'.
  0x8048602D, // Small 'maxpd'.
  0x8138602D, // Small 'maxps'.
  0x8049E02D, // Small 'maxsd'.
  0x8139E02D, // Small 'maxss'.
  0x236B5926, // Large 'mcomm|it'.
  0x8A3714CD, // Small 'mfence'.
  0x8048392D, // Small 'minpd'.
  0x8138392D, // Small 'minps'.
  0x8049B92D, // Small 'minsd'.
  0x8139B92D, // Small 'minss'.
  0x000077C8, // Large 'monitor'.
  0x123377C8, // Large 'monitor|x'.
  0x800059ED, // Small 'mov'.
  0xA620D9ED, // Small 'movabs'.
  0x8900D9ED, // Small 'movapd'.
  0xA700D9ED, // Small 'movaps'.
  0x805159ED, // Small 'movbe'.
  0x800259ED, // Small 'movd'.
  0x3821441F, // Large 'movd|dup'.
  0x100485EB, // Large 'movdir64|b'.
  0x120F65EB, // Large 'movdir|i'.
  0x24E4541F, // Large 'movdq|2q'.
  0x831259ED, // Small 'movdqa'.
  0xAB1259ED, // Small 'movdqu'.
  0x37664827, // Large 'movh|lps'.
  0x890459ED, // Small 'movhpd'.
  0xA70459ED, // Small 'movhps'.
  0x2282582C, // Large 'movlh|ps'.
  0x890659ED, // Small 'movlpd'.
  0xA70659ED, // Small 'movlps'.
  0x22AC669C, // Large 'movmsk|pd'.
  0x2282669C, // Large 'movmsk|ps'.
  0x23CF56A3, // Large 'movnt|dq'.
  0x368F56A3, // Large 'movnt|dqa'.
  0x934759ED, // Small 'movnti'.
  0x22AC56A3, // Large 'movnt|pd'.
  0x228256A3, // Large 'movnt|ps'.
  0xA34759ED, // Small 'movntq'.
  0x231D56A3, // Large 'movnt|sd'.
  0x222256A3, // Large 'movnt|ss'.
  0x8008D9ED, // Small 'movq'.
  0x34E1492B, // Large 'movq|2dq'.
  0x8009D9ED, // Small 'movs'.
  0x8049D9ED, // Small 'movsd'.
  0x240366A9, // Large 'movshd|up'.
  0x240366B0, // Large 'movsld|up'.
  0x8139D9ED, // Small 'movss'.
  0x8189D9ED, // Small 'movsx'.
  0x8989D9ED, // Small 'movsxd'.
  0x890AD9ED, // Small 'movupd'.
  0xA70AD9ED, // Small 'movups'.
  0x818D59ED, // Small 'movzx'.
  0x25785832, // Large 'mpsad|bw'.
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
  0x000086B7, // Large 'packssdw'.
  0x26BE66B7, // Large 'packss|wb'.
  0x26BD66C1, // Large 'packus|dw'.
  0x000086C1, // Large 'packuswb'.
  0x80221030, // Small 'paddb'.
  0x80421030, // Small 'paddd'.
  0x81121030, // Small 'paddq'.
  0x85321030, // Small 'paddsb'.
  0xAF321030, // Small 'paddsw'.
  0x2786583F, // Large 'paddu|sb'.
  0x2561583F, // Large 'paddu|sw'.
  0x81721030, // Small 'paddw'.
  0x12286845, // Large 'palign|r'.
  0x80023830, // Small 'pand'.
  0x80E23830, // Small 'pandn'.
  0x8059D430, // Small 'pause'.
  0x8023D830, // Small 'pavgb'.
  0x2786592F, // Large 'pavgu|sb'.
  0x8173D830, // Small 'pavgw'.
  0x200366CA, // Large 'pblend|vb'.
  0x126466CA, // Large 'pblend|w'.
  0x44625523, // Large 'pclmu|lqdq'.
  0x22155529, // Large 'pcmpe|qb'.
  0x24575529, // Large 'pcmpe|qd'.
  0x23D05529, // Large 'pcmpe|qq'.
  0x27255529, // Large 'pcmpe|qw'.
  0x120F8529, // Large 'pcmpestr|i'.
  0x10018529, // Large 'pcmpestr|m'.
  0x384B448E, // Large 'pcmp|gtb'.
  0x384E448E, // Large 'pcmp|gtd'.
  0x3851448E, // Large 'pcmp|gtq'.
  0x3854448E, // Large 'pcmp|gtw'.
  0x120F8532, // Large 'pcmpistr|i'.
  0x10018532, // Large 'pcmpistr|m'.
  0x2848542F, // Large 'pconf|ig'.
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
  0x12156934, // Large 'pfcmpe|q'.
  0x20315934, // Large 'pfcmp|ge'.
  0x284B5934, // Large 'pfcmp|gt'.
  0x8180B4D0, // Small 'pfmax'.
  0x80E4B4D0, // Small 'pfmin'.
  0x80CAB4D0, // Small 'pfmul'.
  0x8630B8D0, // Small 'pfnacc'.
  0x2011593A, // Large 'pfpna|cc'.
  0x8101C8D0, // Small 'pfrcp'.
  0x238767CF, // Large 'pfrcpi|t1'.
  0x24A667CF, // Large 'pfrcpi|t2'.
  0xAD01C8D0, // Small 'pfrcpv'.
  0x238767D5, // Large 'pfrsqi|t1'.
  0x236157D5, // Large 'pfrsq|rt'.
  0x37DB57D5, // Large 'pfrsq|rtv'.
  0x802ACCD0, // Small 'pfsub'.
  0xA42ACCD0, // Small 'pfsubr'.
  0x88420510, // Small 'phaddd'.
  0x25615702, // Large 'phadd|sw'.
  0xAE420510, // Small 'phaddw'.
  0x12649439, // Large 'phminposu|w'.
  0x882ACD10, // Small 'phsubd'.
  0x2561585E, // Large 'phsub|sw'.
  0xAE2ACD10, // Small 'phsubw'.
  0x80437530, // Small 'pi2fd'.
  0x81737530, // Small 'pi2fw'.
  0x8529B930, // Small 'pinsrb'.
  0x8929B930, // Small 'pinsrd'.
  0xA329B930, // Small 'pinsrq'.
  0xAF29B930, // Small 'pinsrw'.
  0x45635443, // Large 'pmadd|ubsw'.
  0x257A5443, // Large 'pmadd|wd'.
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
  0x1004770F, // Large 'pmovmsk|b'.
  0x122B7717, // Large 'pmovsxb|d'.
  0x12157717, // Large 'pmovsxb|q'.
  0x12647717, // Large 'pmovsxb|w'.
  0x23CF6717, // Large 'pmovsx|dq'.
  0x257A6717, // Large 'pmovsx|wd'.
  0x27096717, // Large 'pmovsx|wq'.
  0x122B7728, // Large 'pmovzxb|d'.
  0x12157728, // Large 'pmovzxb|q'.
  0x12647728, // Large 'pmovzxb|w'.
  0x23CF6728, // Large 'pmovzx|dq'.
  0x257A6728, // Large 'pmovzx|wd'.
  0x27096728, // Large 'pmovzx|wq'.
  0xA24655B0, // Small 'pmuldq'.
  0x25616730, // Large 'pmulhr|sw'.
  0x12646730, // Large 'pmulhr|w'.
  0x264C5730, // Large 'pmulh|uw'.
  0xAE8655B0, // Small 'pmulhw'.
  0x88C655B0, // Small 'pmulld'.
  0xAEC655B0, // Small 'pmullw'.
  0x33CE42D4, // Large 'pmul|udq'.
  0x800041F0, // Small 'pop'.
  0x8000C1F0, // Small 'popa'.
  0x8040C1F0, // Small 'popad'.
  0xA8E1C1F0, // Small 'popcnt'.
  0x800341F0, // Small 'popf'.
  0x804341F0, // Small 'popfd'.
  0x811341F0, // Small 'popfq'.
  0x800049F0, // Small 'por'.
  0x00008378, // Large 'prefetch'.
  0x0000B378, // Large 'prefetchit0'.
  0x1270A378, // Large 'prefetchit|1'.
  0x33838378, // Large 'prefetch|nta'.
  0x23818378, // Large 'prefetch|t0'.
  0x23878378, // Large 'prefetch|t1'.
  0x24A68378, // Large 'prefetch|t2'.
  0x12648378, // Large 'prefetch|w'.
  0x33868378, // Large 'prefetch|wt1'.
  0xAE220670, // Small 'psadbw'.
  0x846AA270, // Small 'pshufb'.
  0x886AA270, // Small 'pshufd'.
  0x288B5365, // Large 'pshuf|hw'.
  0x25825365, // Large 'pshuf|lw'.
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
  0x2786588E, // Large 'psubu|sb'.
  0x2561588E, // Large 'psubu|sw'.
  0x81715670, // Small 'psubw'.
  0x8900DE70, // Small 'pswapd'.
  0x81499690, // Small 'ptest'.
  0x22FD593F, // Large 'ptwri|te'.
  0x2578745A, // Large 'punpckh|bw'.
  0x23CF745A, // Large 'punpckh|dq'.
  0x23CF845A, // Large 'punpckhq|dq'.
  0x257A745A, // Large 'punpckh|wd'.
  0x357C645A, // Large 'punpck|lbw'.
  0x357F645A, // Large 'punpck|ldq'.
  0x4462645A, // Large 'punpck|lqdq'.
  0x3582645A, // Large 'punpck|lwd'.
  0x80044EB0, // Small 'push'.
  0x80144EB0, // Small 'pusha'.
  0x88144EB0, // Small 'pushad'.
  0x80644EB0, // Small 'pushf'.
  0x88644EB0, // Small 'pushfd'.
  0xA2644EB0, // Small 'pushfq'.
  0x81744EB0, // Small 'pushw'.
  0x22FD75F3, // Large 'pvalida|te'.
  0x80093F10, // Small 'pxor'.
  0x80003072, // Small 'rcl'.
  0x81384072, // Small 'rcpps'.
  0x8139C072, // Small 'rcpss'.
  0x80004872, // Small 'rcr'.
  0x317057DE, // Large 'rdfsb|ase'.
  0x317057E3, // Large 'rdgsb|ase'.
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
  0x222E75FA, // Large 'rmpadju|st'.
  0x22FD7601, // Large 'rmpupda|te'.
  0x800031F2, // Small 'rol'.
  0x800049F2, // Small 'ror'.
  0x800C49F2, // Small 'rorx'.
  0x22AC58A0, // Large 'round|pd'.
  0x228258A0, // Large 'round|ps'.
  0x231D58A0, // Large 'round|sd'.
  0x222258A0, // Large 'round|ss'.
  0x80003672, // Small 'rsm'.
  0x22825586, // Large 'rsqrt|ps'.
  0x22225586, // Large 'rsqrt|ss'.
  0x37E855DE, // Large 'rstor|ssp'.
  0x80032033, // Small 'sahf'.
  0x80004833, // Small 'sar'.
  0x800C4833, // Small 'sarx'.
  0x1092A389, // Large 'saveprevss|p'.
  0x80000853, // Small 'sbb'.
  0x80098473, // Small 'scas'.
  0x102777EB, // Large 'seamcal|l'.
  0x22825944, // Large 'seamo|ps'.
  0x21535949, // Large 'seamr|et'.
  0x240467F2, // Large 'sendui|pi'.
  0x25997608, // Large 'seriali|ze'.
  0x0FFF4152, // Large 'setb' + 'set.b|nae|c'
  0x0FFF5162, // Large 'setbe' + 'set.be|na'
  0x0FFF4171, // Large 'setl' + 'set.l|nge'
  0x0FFF517F, // Large 'setle' + 'set.le|ng'
  0x0FFF518E, // Large 'setnb' + 'set.nb|ae|nc'
  0x0FFF61A0, // Large 'setnbe' + 'set.nbe|a'
  0x0FFF51B0, // Large 'setnl' + 'set.nl|ge'
  0x0FFF61BF, // Large 'setnle' + 'set.nle|g'
  0x80F750B3, // Small 'setno'.
  0x0FFF51CF, // Large 'setnp' + 'set.np|po'
  0x813750B3, // Small 'setns'.
  0x0FFF51DE, // Large 'setnz' + 'set.nz|ne'
  0x8007D0B3, // Small 'seto'.
  0x0FFF41ED, // Large 'setp' + 'set.p|pe'
  0x8009D0B3, // Small 'sets'.
  0x121D77F8, // Large 'setssbs|y'.
  0x0FFF41FA, // Large 'setz' + 'set.z|e'
  0x8A3714D3, // Small 'sfence'.
  0x800A10F3, // Small 'sgdt'.
  0x447E460F, // Large 'sha1|msg1'.
  0x4482460F, // Large 'sha1|msg2'.
  0x22FD760F, // Large 'sha1nex|te'.
  0x12348616, // Large 'sha1rnds|4'.
  0x447E6393, // Large 'sha256|msg1'.
  0x44826393, // Large 'sha256|msg2'.
  0x22839393, // Large 'sha256rnd|s2'.
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
  0x122868AD, // Large 'stmxcs|r'.
  0x8009BE93, // Small 'stos'.
  0x80004A93, // Small 'str'.
  0x1031861E, // Large 'sttilecf|g'.
  0x8004D693, // Small 'stui'.
  0x80000AB3, // Small 'sub'.
  0x80480AB3, // Small 'subpd'.
  0x81380AB3, // Small 'subps'.
  0x80498AB3, // Small 'subsd'.
  0x81398AB3, // Small 'subss'.
  0xA67806F3, // Small 'swapgs'.
  0x38B9494E, // Large 'sysc|all'.
  0x42FC47FF, // Large 'syse|nter'.
  0x236B5803, // Large 'sysex|it'.
  0x336B5803, // Large 'sysex|itq'.
  0xA8594F33, // Small 'sysret'.
  0x236C5952, // Large 'sysre|tq'.
  0x86B9B794, // Small 't1mskc'.
  0x2282A304, // Large 'tcmmimfp16|ps'.
  0x2282A30E, // Large 'tcmmrlfp16|ps'.
  0x98C08C94, // Small 'tdcall'.
  0x22827626, // Large 'tdpbf16|ps'.
  0x331C4626, // Large 'tdpb|ssd'.
  0x36D84626, // Large 'tdpb|sud'.
  0x231D5957, // Large 'tdpbu|sd'.
  0x23CE5957, // Large 'tdpbu|ud'.
  0x2282762D, // Large 'tdpfp16|ps'.
  0x800A4CB4, // Small 'test'.
  0x935A4CB4, // Small 'testui'.
  0x0000939C, // Large 'tileloadd'.
  0x2387939C, // Large 'tileloadd|t1'.
  0x317083A5, // Large 'tilerele|ase'.
  0x122B94A8, // Large 'tilestore|d'.
  0x4599439C, // Large 'tile|zero'.
  0x2056595C, // Large 'tlbsy|nc'.
  0x8B3A8614, // Small 'tpause'.
  0x81470F54, // Small 'tzcnt'.
  0x80B9B754, // Small 'tzmsk'.
  0x231D58B4, // Large 'ucomi|sd'.
  0x222258B4, // Large 'ucomi|ss'.
  0x80006C95, // Small 'ud0'.
  0x80007095, // Small 'ud1'.
  0x80007495, // Small 'ud2'.
  0x8142C935, // Small 'uiret'.
  0x67C92808, // Large 'um|onitor'.
  0xA890DDB5, // Small 'umwait'.
  0x22AC645B, // Large 'unpckh|pd'.
  0x2282645B, // Large 'unpckh|ps'.
  0x3763545B, // Large 'unpck|lpd'.
  0x3766545B, // Large 'unpck|lps'.
  0x89021036, // Small 'vaddpd'.
  0x91021036, // Small 'vaddph'.
  0xA7021036, // Small 'vaddps'.
  0x89321036, // Small 'vaddsd'.
  0x91321036, // Small 'vaddsh'.
  0xA7321036, // Small 'vaddss'.
  0x22AC7634, // Large 'vaddsub|pd'.
  0x22827634, // Large 'vaddsub|ps'.
  0x000073AD, // Large 'vaesdec'.
  0x322D83AD, // Large 'vaesdecl|ast'.
  0x000073B5, // Large 'vaesenc'.
  0x322D83B5, // Large 'vaesencl|ast'.
  0x27EE5961, // Large 'vaesi|mc'.
  0x1154F217, // Large 'vaeskeygenassis|t'.
  0x23765966, // Large 'valig|nd'.
  0x28DF5966, // Large 'valig|nq'.
  0x22AC596B, // Large 'vandn|pd'.
  0x2282596B, // Large 'vandn|ps'.
  0x89023836, // Small 'vandpd'.
  0xA7023836, // Small 'vandps'.
  0x1152D267, // Large 'vbcstnebf162p|s'.
  0x53187267, // Large 'vbcstne|sh2ps'.
  0x22AC763B, // Large 'vblendm|pd'.
  0x2282763B, // Large 'vblendm|ps'.
  0x22AC663B, // Large 'vblend|pd'.
  0x2282663B, // Large 'vblend|ps'.
  0x3642663B, // Large 'vblend|vpd'.
  0x3364663B, // Large 'vblend|vps'.
  0x3274B226, // Large 'vbroadcastf|128'.
  0x1209E226, // Large 'vbroadcastf32x|2'.
  0x1234E226, // Large 'vbroadcastf32x|4'.
  0x120BE226, // Large 'vbroadcastf32x|8'.
  0x4235B226, // Large 'vbroadcastf|64x2'.
  0x4239B226, // Large 'vbroadcastf|64x4'.
  0x4277A226, // Large 'vbroadcast|i128'.
  0x523DA226, // Large 'vbroadcast|i32x2'.
  0x5242A226, // Large 'vbroadcast|i32x4'.
  0x5247A226, // Large 'vbroadcast|i32x8'.
  0x524CA226, // Large 'vbroadcast|i64x2'.
  0x5251A226, // Large 'vbroadcast|i64x4'.
  0x231DA226, // Large 'vbroadcast|sd'.
  0x2222A226, // Large 'vbroadcast|ss'.
  0x89083476, // Small 'vcmppd'.
  0x91083476, // Small 'vcmpph'.
  0xA7083476, // Small 'vcmpps'.
  0x89383476, // Small 'vcmpsd'.
  0x91383476, // Small 'vcmpsh'.
  0xA7383476, // Small 'vcmpss'.
  0x231D5970, // Large 'vcomi|sd'.
  0x22DA5970, // Large 'vcomi|sh'.
  0x22225970, // Large 'vcomi|ss'.
  0x22AC93BD, // Large 'vcompress|pd'.
  0x228293BD, // Large 'vcompress|ps'.
  0x22AC7645, // Large 'vcvtdq2|pd'.
  0x22A77645, // Large 'vcvtdq2|ph'.
  0x22827645, // Large 'vcvtdq2|ps'.
  0x426EA27B, // Large 'vcvtne2ps2|bf16'.
  0x3281B285, // Large 'vcvtneebf16|2ps'.
  0x531F7285, // Large 'vcvtnee|ph2ps'.
  0x3281B290, // Large 'vcvtneobf16|2ps'.
  0x531F7290, // Large 'vcvtneo|ph2ps'.
  0x426E92DF, // Large 'vcvtneps2|bf16'.
  0x34E164B1, // Large 'vcvtpd|2dq'.
  0x32A664B1, // Large 'vcvtpd|2ph'.
  0x328164B1, // Large 'vcvtpd|2ps'.
  0x34E464B1, // Large 'vcvtpd|2qq'.
  0x63CB427B, // Large 'vcvt|pd2udq'.
  0x43D964B1, // Large 'vcvtpd|2uqq'.
  0x23CF74B7, // Large 'vcvtph2|dq'.
  0x122B84B7, // Large 'vcvtph2p|d'.
  0x000094B7, // Large 'vcvtph2ps'.
  0x123394B7, // Large 'vcvtph2ps|x'.
  0x23D074B7, // Large 'vcvtph2|qq'.
  0x33CE74B7, // Large 'vcvtph2|udq'.
  0x33DA74B7, // Large 'vcvtph2|uqq'.
  0x264C74B7, // Large 'vcvtph2|uw'.
  0x126474B7, // Large 'vcvtph2|w'.
  0x23CF74C0, // Large 'vcvtps2|dq'.
  0x122B84C0, // Large 'vcvtps2p|d'.
  0x000094C0, // Large 'vcvtps2ph'.
  0x123394C0, // Large 'vcvtps2ph|x'.
  0x23D074C0, // Large 'vcvtps2|qq'.
  0x33CE74C0, // Large 'vcvtps2|udq'.
  0x33DA74C0, // Large 'vcvtps2|uqq'.
  0x22AC764E, // Large 'vcvtqq2|pd'.
  0x22A7764E, // Large 'vcvtqq2|ph'.
  0x2282764E, // Large 'vcvtqq2|ps'.
  0x22DA74C9, // Large 'vcvtsd2|sh'.
  0x222374C9, // Large 'vcvtsd2|si'.
  0x222274C9, // Large 'vcvtsd2|ss'.
  0x222384C9, // Large 'vcvtsd2u|si'.
  0x231D74D1, // Large 'vcvtsh2|sd'.
  0x222374D1, // Large 'vcvtsh2|si'.
  0x222274D1, // Large 'vcvtsh2|ss'.
  0x222384D1, // Large 'vcvtsh2u|si'.
  0x231D7655, // Large 'vcvtsi2|sd'.
  0x22DA7655, // Large 'vcvtsi2|sh'.
  0x22227655, // Large 'vcvtsi2|ss'.
  0x231D74D9, // Large 'vcvtss2|sd'.
  0x22DA74D9, // Large 'vcvtss2|sh'.
  0x222374D9, // Large 'vcvtss2|si'.
  0x222384D9, // Large 'vcvtss2u|si'.
  0x23CF83C6, // Large 'vcvttpd2|dq'.
  0x23D083C6, // Large 'vcvttpd2|qq'.
  0x1215A3C6, // Large 'vcvttpd2ud|q'.
  0x23D093C6, // Large 'vcvttpd2u|qq'.
  0x23CF83D2, // Large 'vcvttph2|dq'.
  0x23D083D2, // Large 'vcvttph2|qq'.
  0x43CD73D2, // Large 'vcvttph|2udq'.
  0x43D973D2, // Large 'vcvttph|2uqq'.
  0x126493D2, // Large 'vcvttph2u|w'.
  0x126483D2, // Large 'vcvttph2|w'.
  0x34E173DD, // Large 'vcvttps|2dq'.
  0x34E473DD, // Large 'vcvttps|2qq'.
  0x43CD73DD, // Large 'vcvttps|2udq'.
  0x43D973DD, // Large 'vcvttps|2uqq'.
  0x222383E4, // Large 'vcvttsd2|si'.
  0x222393E4, // Large 'vcvttsd2u|si'.
  0x222383ED, // Large 'vcvttsh2|si'.
  0x222393ED, // Large 'vcvttsh2u|si'.
  0x222383F6, // Large 'vcvttss2|si'.
  0x222393F6, // Large 'vcvttss2u|si'.
  0x22AC84E7, // Large 'vcvtudq2|pd'.
  0x22A784E7, // Large 'vcvtudq2|ph'.
  0x228284E7, // Large 'vcvtudq2|ps'.
  0x22AC84EF, // Large 'vcvtuqq2|pd'.
  0x22A784EF, // Large 'vcvtuqq2|ph'.
  0x228284EF, // Large 'vcvtuqq2|ps'.
  0x231D84F7, // Large 'vcvtusi2|sd'.
  0x22DA84F7, // Large 'vcvtusi2|sh'.
  0x222284F7, // Large 'vcvtusi2|ss'.
  0x32A6665C, // Large 'vcvtuw|2ph'.
  0x32A6580A, // Large 'vcvtw|2ph'.
  0x25787662, // Large 'vdbpsad|bw'.
  0x890B2496, // Small 'vdivpd'.
  0x910B2496, // Small 'vdivph'.
  0xA70B2496, // Small 'vdivps'.
  0x893B2496, // Small 'vdivsd'.
  0x913B2496, // Small 'vdivsh'.
  0xA73B2496, // Small 'vdivss'.
  0x22827669, // Large 'vdpbf16|ps'.
  0x80484096, // Small 'vdppd'.
  0x81384096, // Small 'vdpps'.
  0x800948B6, // Small 'verr'.
  0x800BC8B6, // Small 'verw'.
  0x22AC7670, // Large 'vexpand|pd'.
  0x22827670, // Large 'vexpand|ps'.
  0x327492EF, // Large 'vextractf|128'.
  0x622F72E8, // Large 'vextrac|tf32x4'.
  0x424892EF, // Large 'vextractf|32x8'.
  0x423592EF, // Large 'vextractf|64x2'.
  0x423992EF, // Large 'vextractf|64x4'.
  0x427782EF, // Large 'vextract|i128'.
  0x524282EF, // Large 'vextract|i32x4'.
  0x524782EF, // Large 'vextract|i32x8'.
  0x524C82EF, // Large 'vextract|i64x2'.
  0x525182EF, // Large 'vextract|i64x4'.
  0x228282EF, // Large 'vextract|ps'.
  0x22A784FF, // Large 'vfcmaddc|ph'.
  0x22DA84FF, // Large 'vfcmaddc|sh'.
  0x22A77677, // Large 'vfcmulc|ph'.
  0x22DA7677, // Large 'vfcmulc|sh'.
  0x22AC93FF, // Large 'vfixupimm|pd'.
  0x228293FF, // Large 'vfixupimm|ps'.
  0x231D93FF, // Large 'vfixupimm|sd'.
  0x222293FF, // Large 'vfixupimm|ss'.
  0x22AC9408, // Large 'vfmadd132|pd'.
  0x22A79408, // Large 'vfmadd132|ph'.
  0x22829408, // Large 'vfmadd132|ps'.
  0x231D9408, // Large 'vfmadd132|sd'.
  0x22DA9408, // Large 'vfmadd132|sh'.
  0x22229408, // Large 'vfmadd132|ss'.
  0x52A9629B, // Large 'vfmadd|213pd'.
  0x52AE629B, // Large 'vfmadd|213ph'.
  0x52B3629B, // Large 'vfmadd|213ps'.
  0x532E629B, // Large 'vfmadd|213sd'.
  0x5333629B, // Large 'vfmadd|213sh'.
  0x5338629B, // Large 'vfmadd|213ss'.
  0x52B8629B, // Large 'vfmadd|231pd'.
  0x52BD629B, // Large 'vfmadd|231ph'.
  0x52C2629B, // Large 'vfmadd|231ps'.
  0x533D629B, // Large 'vfmadd|231sd'.
  0x5342629B, // Large 'vfmadd|231sh'.
  0x5347629B, // Large 'vfmadd|231ss'.
  0x367E629B, // Large 'vfmadd|cph'.
  0x3681629B, // Large 'vfmadd|csh'.
  0x22AC629B, // Large 'vfmadd|pd'.
  0x2282629B, // Large 'vfmadd|ps'.
  0x122B729B, // Large 'vfmadds|d'.
  0x1152729B, // Large 'vfmadds|s'.
  0x122BD29B, // Large 'vfmaddsub132p|d'.
  0x12A8D29B, // Large 'vfmaddsub132p|h'.
  0x1152D29B, // Large 'vfmaddsub132p|s'.
  0x52A9929B, // Large 'vfmaddsub|213pd'.
  0x52AE929B, // Large 'vfmaddsub|213ph'.
  0x52B3929B, // Large 'vfmaddsub|213ps'.
  0x52B8929B, // Large 'vfmaddsub|231pd'.
  0x52BD929B, // Large 'vfmaddsub|231ph'.
  0x52C2929B, // Large 'vfmaddsub|231ps'.
  0x22AC929B, // Large 'vfmaddsub|pd'.
  0x2282929B, // Large 'vfmaddsub|ps'.
  0x22AC9411, // Large 'vfmsub132|pd'.
  0x22A79411, // Large 'vfmsub132|ph'.
  0x22829411, // Large 'vfmsub132|ps'.
  0x231D9411, // Large 'vfmsub132|sd'.
  0x22DA9411, // Large 'vfmsub132|sh'.
  0x22229411, // Large 'vfmsub132|ss'.
  0x52A962C7, // Large 'vfmsub|213pd'.
  0x52AE62C7, // Large 'vfmsub|213ph'.
  0x52B362C7, // Large 'vfmsub|213ps'.
  0x532E62C7, // Large 'vfmsub|213sd'.
  0x533362C7, // Large 'vfmsub|213sh'.
  0x533862C7, // Large 'vfmsub|213ss'.
  0x52B862C7, // Large 'vfmsub|231pd'.
  0x52BD62C7, // Large 'vfmsub|231ph'.
  0x52C262C7, // Large 'vfmsub|231ps'.
  0x533D62C7, // Large 'vfmsub|231sd'.
  0x534262C7, // Large 'vfmsub|231sh'.
  0x534762C7, // Large 'vfmsub|231ss'.
  0x22ACC2C7, // Large 'vfmsubadd132|pd'.
  0x22A7C2C7, // Large 'vfmsubadd132|ph'.
  0x2282C2C7, // Large 'vfmsubadd132|ps'.
  0x52A992C7, // Large 'vfmsubadd|213pd'.
  0x52AE92C7, // Large 'vfmsubadd|213ph'.
  0x52B392C7, // Large 'vfmsubadd|213ps'.
  0x52B892C7, // Large 'vfmsubadd|231pd'.
  0x52BD92C7, // Large 'vfmsubadd|231ph'.
  0x52C292C7, // Large 'vfmsubadd|231ps'.
  0x22AC92C7, // Large 'vfmsubadd|pd'.
  0x228292C7, // Large 'vfmsubadd|ps'.
  0x22AC62C7, // Large 'vfmsub|pd'.
  0x228262C7, // Large 'vfmsub|ps'.
  0x231D62C7, // Large 'vfmsub|sd'.
  0x222262C7, // Large 'vfmsub|ss'.
  0x367E580F, // Large 'vfmul|cph'.
  0x3681580F, // Large 'vfmul|csh'.
  0x22ACA324, // Large 'vfnmadd132|pd'.
  0x22A7A324, // Large 'vfnmadd132|ph'.
  0x2282A324, // Large 'vfnmadd132|ps'.
  0x231DA324, // Large 'vfnmadd132|sd'.
  0x22DAA324, // Large 'vfnmadd132|sh'.
  0x2222A324, // Large 'vfnmadd132|ss'.
  0x52A97324, // Large 'vfnmadd|213pd'.
  0x52AE7324, // Large 'vfnmadd|213ph'.
  0x52B37324, // Large 'vfnmadd|213ps'.
  0x532E7324, // Large 'vfnmadd|213sd'.
  0x53337324, // Large 'vfnmadd|213sh'.
  0x53387324, // Large 'vfnmadd|213ss'.
  0x52B87324, // Large 'vfnmadd|231pd'.
  0x52BD7324, // Large 'vfnmadd|231ph'.
  0x52C27324, // Large 'vfnmadd|231ps'.
  0x533D7324, // Large 'vfnmadd|231sd'.
  0x53427324, // Large 'vfnmadd|231sh'.
  0x53477324, // Large 'vfnmadd|231ss'.
  0x22AC7324, // Large 'vfnmadd|pd'.
  0x22827324, // Large 'vfnmadd|ps'.
  0x231D7324, // Large 'vfnmadd|sd'.
  0x22227324, // Large 'vfnmadd|ss'.
  0x22ACA34C, // Large 'vfnmsub132|pd'.
  0x22A7A34C, // Large 'vfnmsub132|ph'.
  0x2282A34C, // Large 'vfnmsub132|ps'.
  0x231DA34C, // Large 'vfnmsub132|sd'.
  0x22DAA34C, // Large 'vfnmsub132|sh'.
  0x2222A34C, // Large 'vfnmsub132|ss'.
  0x52A9734C, // Large 'vfnmsub|213pd'.
  0x52AE734C, // Large 'vfnmsub|213ph'.
  0x52B3734C, // Large 'vfnmsub|213ps'.
  0x532E734C, // Large 'vfnmsub|213sd'.
  0x5333734C, // Large 'vfnmsub|213sh'.
  0x5338734C, // Large 'vfnmsub|213ss'.
  0x52B8734C, // Large 'vfnmsub|231pd'.
  0x52BD734C, // Large 'vfnmsub|231ph'.
  0x52C2734C, // Large 'vfnmsub|231ps'.
  0x533D734C, // Large 'vfnmsub|231sd'.
  0x5342734C, // Large 'vfnmsub|231sh'.
  0x5347734C, // Large 'vfnmsub|231ss'.
  0x22AC734C, // Large 'vfnmsub|pd'.
  0x2282734C, // Large 'vfnmsub|ps'.
  0x231D734C, // Large 'vfnmsub|sd'.
  0x2222734C, // Large 'vfnmsub|ss'.
  0x22AC8507, // Large 'vfpclass|pd'.
  0x22A78507, // Large 'vfpclass|ph'.
  0x22828507, // Large 'vfpclass|ps'.
  0x231D8507, // Large 'vfpclass|sd'.
  0x22DA8507, // Large 'vfpclass|sh'.
  0x22228507, // Large 'vfpclass|ss'.
  0x22AC5975, // Large 'vfrcz|pd'.
  0x22825975, // Large 'vfrcz|ps'.
  0x231D5975, // Large 'vfrcz|sd'.
  0x22225975, // Large 'vfrcz|ss'.
  0x22AC850F, // Large 'vgatherd|pd'.
  0x2282850F, // Large 'vgatherd|ps'.
  0x3478750F, // Large 'vgather|qpd'.
  0x347B750F, // Large 'vgather|qps'.
  0x22AC7684, // Large 'vgetexp|pd'.
  0x22A77684, // Large 'vgetexp|ph'.
  0x22827684, // Large 'vgetexp|ps'.
  0x231D7684, // Large 'vgetexp|sd'.
  0x22DA7684, // Large 'vgetexp|sh'.
  0x22227684, // Large 'vgetexp|ss'.
  0x33CA7517, // Large 'vgetman|tpd'.
  0x33D67517, // Large 'vgetman|tph'.
  0x33E17517, // Large 'vgetman|tps'.
  0x33E87517, // Large 'vgetman|tsd'.
  0x33F17517, // Large 'vgetman|tsh'.
  0x33FA7517, // Large 'vgetman|tss'.
  0x2215F206, // Large 'vgf2p8affineinv|qb'.
  0x2215C206, // Large 'vgf2p8affine|qb'.
  0x451E6206, // Large 'vgf2p8|mulb'.
  0x22AC597A, // Large 'vhadd|pd'.
  0x2282597A, // Large 'vhadd|ps'.
  0x22AC597F, // Large 'vhsub|pd'.
  0x2282597F, // Large 'vhsub|ps'.
  0x3274835C, // Large 'vinsertf|128'.
  0x622F6356, // Large 'vinser|tf32x4'.
  0x4248835C, // Large 'vinsertf|32x8'.
  0x4235835C, // Large 'vinsertf|64x2'.
  0x4239835C, // Large 'vinsertf|64x4'.
  0x4277735C, // Large 'vinsert|i128'.
  0x5242735C, // Large 'vinsert|i32x4'.
  0x5247735C, // Large 'vinsert|i32x8'.
  0x524C735C, // Large 'vinsert|i64x2'.
  0x5251735C, // Large 'vinsert|i64x4'.
  0x2282735C, // Large 'vinsert|ps'.
  0xAB121196, // Small 'vlddqu'.
  0x12287814, // Large 'vldmxcs|r'.
  0x12A2A41A, // Large 'vmaskmovdq|u'.
  0x22AC841A, // Large 'vmaskmov|pd'.
  0x2282841A, // Large 'vmaskmov|ps'.
  0x890C05B6, // Small 'vmaxpd'.
  0x910C05B6, // Small 'vmaxph'.
  0xA70C05B6, // Small 'vmaxps'.
  0x893C05B6, // Small 'vmaxsd'.
  0x913C05B6, // Small 'vmaxsh'.
  0xA73C05B6, // Small 'vmaxss'.
  0x98C08DB6, // Small 'vmcall'.
  0x25F95984, // Large 'vmcle|ar'.
  0x86EA99B6, // Small 'vmfunc'.
  0x236B5989, // Large 'vmgex|it'.
  0x890725B6, // Small 'vminpd'.
  0x910725B6, // Small 'vminph'.
  0xA70725B6, // Small 'vminps'.
  0x893725B6, // Small 'vminsd'.
  0x913725B6, // Small 'vminsh'.
  0xA73725B6, // Small 'vminss'.
  0x237E681B, // Large 'vmlaun|ch'.
  0x8817B1B6, // Small 'vmload'.
  0x38B9498E, // Large 'vmmc|all'.
  0x22AC5992, // Large 'vmova|pd'.
  0x22825992, // Large 'vmova|ps'.
  0x804B3DB6, // Small 'vmovd'.
  0x3821568B, // Large 'vmovd|dup'.
  0x0000768B, // Large 'vmovdqa'.
  0x2231768B, // Large 'vmovdqa|32'.
  0x2235768B, // Large 'vmovdqa|64'.
  0x12A2668B, // Large 'vmovdq|u'.
  0x3692668B, // Large 'vmovdq|u16'.
  0x3695668B, // Large 'vmovdq|u32'.
  0x3698668B, // Large 'vmovdq|u64'.
  0x2824668B, // Large 'vmovdq|u8'.
  0x37665826, // Large 'vmovh|lps'.
  0x22AC5826, // Large 'vmovh|pd'.
  0x22825826, // Large 'vmovh|ps'.
  0x2282682B, // Large 'vmovlh|ps'.
  0x22AC582B, // Large 'vmovl|pd'.
  0x2282582B, // Large 'vmovl|ps'.
  0x22AC769B, // Large 'vmovmsk|pd'.
  0x2282769B, // Large 'vmovmsk|ps'.
  0x23CF66A2, // Large 'vmovnt|dq'.
  0x368F66A2, // Large 'vmovnt|dqa'.
  0x22AC66A2, // Large 'vmovnt|pd'.
  0x228266A2, // Large 'vmovnt|ps'.
  0x811B3DB6, // Small 'vmovq'.
  0x893B3DB6, // Small 'vmovsd'.
  0x913B3DB6, // Small 'vmovsh'.
  0x240376A8, // Large 'vmovshd|up'.
  0x240376AF, // Large 'vmovsld|up'.
  0xA73B3DB6, // Small 'vmovss'.
  0x3604468B, // Large 'vmov|upd'.
  0x22825997, // Large 'vmovu|ps'.
  0x817B3DB6, // Small 'vmovw'.
  0x25786831, // Large 'vmpsad|bw'.
  0x35E2499C, // Large 'vmpt|rld'.
  0x35DE499C, // Large 'vmpt|rst'.
  0x8812C9B6, // Small 'vmread'.
  0x100F7837, // Large 'vmresum|e'.
  0x80EAC9B6, // Small 'vmrun'.
  0x8B60CDB6, // Small 'vmsave'.
  0x890655B6, // Small 'vmulpd'.
  0x910655B6, // Small 'vmulph'.
  0xA70655B6, // Small 'vmulps'.
  0x893655B6, // Small 'vmulsd'.
  0x913655B6, // Small 'vmulsh'.
  0xA73655B6, // Small 'vmulss'.
  0x22FD59A0, // Large 'vmwri|te'.
  0x8C67E1B6, // Small 'vmxoff'.
  0x80E7E1B6, // Small 'vmxon'.
  0x804849F6, // Small 'vorpd'.
  0x813849F6, // Small 'vorps'.
  0x122BC2F8, // Large 'vp2intersect|d'.
  0x1215C2F8, // Large 'vp2intersect|q'.
  0x85310616, // Small 'vpabsb'.
  0x89310616, // Small 'vpabsd'.
  0xA3310616, // Small 'vpabsq'.
  0xAF310616, // Small 'vpabsw'.
  0x126486B6, // Large 'vpackssd|w'.
  0x26BE76B6, // Large 'vpackss|wb'.
  0x36BC66C0, // Large 'vpacku|sdw'.
  0x36C666C0, // Large 'vpacku|swb'.
  0x84420616, // Small 'vpaddb'.
  0x88420616, // Small 'vpaddd'.
  0xA2420616, // Small 'vpaddq'.
  0x2786583E, // Large 'vpadd|sb'.
  0x2561583E, // Large 'vpadd|sw'.
  0x2786683E, // Large 'vpaddu|sb'.
  0x2561683E, // Large 'vpaddu|sw'.
  0xAE420616, // Small 'vpaddw'.
  0x12287844, // Large 'vpalign|r'.
  0x80470616, // Small 'vpand'.
  0x88470616, // Small 'vpandd'.
  0x9C470616, // Small 'vpandn'.
  0x237659A5, // Large 'vpand|nd'.
  0x28DF59A5, // Large 'vpand|nq'.
  0xA2470616, // Small 'vpandq'.
  0x847B0616, // Small 'vpavgb'.
  0xAE7B0616, // Small 'vpavgw'.
  0x122B76C9, // Large 'vpblend|d'.
  0x226176C9, // Large 'vpblend|mb'.
  0x26D076C9, // Large 'vpblend|md'.
  0x121586C9, // Large 'vpblendm|q'.
  0x126486C9, // Large 'vpblendm|w'.
  0x200376C9, // Large 'vpblend|vb'.
  0x126476C9, // Large 'vpblend|w'.
  0x1004B256, // Large 'vpbroadcast|b'.
  0x122BB256, // Large 'vpbroadcast|d'.
  0x1215E256, // Large 'vpbroadcastmb2|q'.
  0x3264C256, // Large 'vpbroadcastm|w2d'.
  0x1215B256, // Large 'vpbroadcast|q'.
  0x1264B256, // Large 'vpbroadcast|w'.
  0x44626522, // Large 'vpclmu|lqdq'.
  0xACF68E16, // Small 'vpcmov'.
  0x85068E16, // Small 'vpcmpb'.
  0x89068E16, // Small 'vpcmpd'.
  0x22156528, // Large 'vpcmpe|qb'.
  0x24576528, // Large 'vpcmpe|qd'.
  0x23D06528, // Large 'vpcmpe|qq'.
  0x27256528, // Large 'vpcmpe|qw'.
  0x120F9528, // Large 'vpcmpestr|i'.
  0x10019528, // Large 'vpcmpestr|m'.
  0x384B5528, // Large 'vpcmp|gtb'.
  0x384E5528, // Large 'vpcmp|gtd'.
  0x38515528, // Large 'vpcmp|gtq'.
  0x38545528, // Large 'vpcmp|gtw'.
  0x120F9531, // Large 'vpcmpistr|i'.
  0x10019531, // Large 'vpcmpistr|m'.
  0xA3068E16, // Small 'vpcmpq'.
  0x22A25528, // Large 'vpcmp|ub'.
  0x23CE5528, // Large 'vpcmp|ud'.
  0x23DA5528, // Large 'vpcmp|uq'.
  0x264C5528, // Large 'vpcmp|uw'.
  0xAF068E16, // Small 'vpcmpw'.
  0x84D78E16, // Small 'vpcomb'.
  0x88D78E16, // Small 'vpcomd'.
  0x1004A424, // Large 'vpcompress|b'.
  0x122BA424, // Large 'vpcompress|d'.
  0x1215A424, // Large 'vpcompress|q'.
  0x1264A424, // Large 'vpcompress|w'.
  0xA2D78E16, // Small 'vpcomq'.
  0x22A25424, // Large 'vpcom|ub'.
  0x23CE5424, // Large 'vpcom|ud'.
  0x23DA5424, // Large 'vpcom|uq'.
  0x264C5424, // Large 'vpcom|uw'.
  0xAED78E16, // Small 'vpcomw'.
  0x122BA42E, // Large 'vpconflict|d'.
  0x1215A42E, // Large 'vpconflict|q'.
  0x122B76D2, // Large 'vpdpbss|d'.
  0x22A076D2, // Large 'vpdpbss|ds'.
  0x23CE66D2, // Large 'vpdpbs|ud'.
  0x36D966D2, // Large 'vpdpbs|uds'.
  0x122B76DC, // Large 'vpdpbus|d'.
  0x22A076DC, // Large 'vpdpbus|ds'.
  0x23CE66DC, // Large 'vpdpbu|ud'.
  0x36D966DC, // Large 'vpdpbu|uds'.
  0x122B76E3, // Large 'vpdpwss|d'.
  0x22A076E3, // Large 'vpdpwss|ds'.
  0x23CE66E3, // Large 'vpdpws|ud'.
  0x36D966E3, // Large 'vpdpws|uds'.
  0x122B76EA, // Large 'vpdpwus|d'.
  0x22A076EA, // Large 'vpdpwus|ds'.
  0x23CE66EA, // Large 'vpdpwu|ud'.
  0x36D966EA, // Large 'vpdpwu|uds'.
  0x3274753A, // Large 'vperm2f|128'.
  0x4277653A, // Large 'vperm2|i128'.
  0x84D91616, // Small 'vpermb'.
  0x88D91616, // Small 'vpermd'.
  0x28576541, // Large 'vpermi|2b'.
  0x22656541, // Large 'vpermi|2d'.
  0x36F16541, // Large 'vpermi|2pd'.
  0x32816541, // Large 'vpermi|2ps'.
  0x24E46541, // Large 'vpermi|2q'.
  0x22636541, // Large 'vpermi|2w'.
  0x22AC8541, // Large 'vpermil2|pd'.
  0x22828541, // Large 'vpermil2|ps'.
  0x22AC7541, // Large 'vpermil|pd'.
  0x22827541, // Large 'vpermil|ps'.
  0x22AC553A, // Large 'vperm|pd'.
  0x2282553A, // Large 'vperm|ps'.
  0xA2D91616, // Small 'vpermq'.
  0x285766F4, // Large 'vpermt|2b'.
  0x226566F4, // Large 'vpermt|2d'.
  0x36F166F4, // Large 'vpermt|2pd'.
  0x328166F4, // Large 'vpermt|2ps'.
  0x24E466F4, // Large 'vpermt|2q'.
  0x226366F4, // Large 'vpermt|2w'.
  0xAED91616, // Small 'vpermw'.
  0x266376FA, // Large 'vpexpan|db'.
  0x229F76FA, // Large 'vpexpan|dd'.
  0x23CF76FA, // Large 'vpexpan|dq'.
  0x26BD76FA, // Large 'vpexpan|dw'.
  0x37BF46FA, // Large 'vpex|trb'.
  0x247659AA, // Large 'vpext|rd'.
  0x245659AA, // Large 'vpext|rq'.
  0x29AF59AA, // Large 'vpext|rw'.
  0x229F8549, // Large 'vpgather|dd'.
  0x23CF8549, // Large 'vpgather|dq'.
  0x24578549, // Large 'vpgather|qd'.
  0x23D08549, // Large 'vpgather|qq'.
  0x28596701, // Large 'vphadd|bd'.
  0x285B6701, // Large 'vphadd|bq'.
  0x25786701, // Large 'vphadd|bw'.
  0x122B6701, // Large 'vphadd|d'.
  0x23CF6701, // Large 'vphadd|dq'.
  0x25616701, // Large 'vphadd|sw'.
  0x122B8701, // Large 'vphaddub|d'.
  0x12158701, // Large 'vphaddub|q'.
  0x12648701, // Large 'vphaddub|w'.
  0x23CF7701, // Large 'vphaddu|dq'.
  0x257A7701, // Large 'vphaddu|wd'.
  0x27097701, // Large 'vphaddu|wq'.
  0x12646701, // Large 'vphadd|w'.
  0x257A6701, // Large 'vphadd|wd'.
  0x27096701, // Large 'vphadd|wq'.
  0x1264A438, // Large 'vphminposu|w'.
  0x2578685D, // Large 'vphsub|bw'.
  0x122B685D, // Large 'vphsub|d'.
  0x23CF685D, // Large 'vphsub|dq'.
  0x2561685D, // Large 'vphsub|sw'.
  0x1264685D, // Large 'vphsub|w'.
  0x257A685D, // Large 'vphsub|wd'.
  0x27C059B1, // Large 'vpins|rb'.
  0x247659B1, // Large 'vpins|rd'.
  0x245659B1, // Large 'vpins|rq'.
  0x29AF59B1, // Large 'vpins|rw'.
  0x26266863, // Large 'vplzcn|td'.
  0x236C6863, // Large 'vplzcn|tq'.
  0x229F6551, // Large 'vpmacs|dd'.
  0x370B6551, // Large 'vpmacs|dqh'.
  0x35806551, // Large 'vpmacs|dql'.
  0x122B8551, // Large 'vpmacssd|d'.
  0x12A89551, // Large 'vpmacssdq|h'.
  0x10279551, // Large 'vpmacssdq|l'.
  0x257A7551, // Large 'vpmacss|wd'.
  0x25797551, // Large 'vpmacss|ww'.
  0x257A6551, // Large 'vpmacs|wd'.
  0x25796551, // Large 'vpmacs|ww'.
  0x122B955A, // Large 'vpmadcssw|d'.
  0x257A755A, // Large 'vpmadcs|wd'.
  0x23DA9442, // Large 'vpmadd52h|uq'.
  0x344B8442, // Large 'vpmadd52|luq'.
  0x45636442, // Large 'vpmadd|ubsw'.
  0x257A6442, // Large 'vpmadd|wd'.
  0x641D4442, // Large 'vpma|skmovd'.
  0x22148567, // Large 'vpmaskmo|vq'.
  0x278659B6, // Large 'vpmax|sb'.
  0x231D59B6, // Large 'vpmax|sd'.
  0x258759B6, // Large 'vpmax|sq'.
  0x256159B6, // Large 'vpmax|sw'.
  0x22A259B6, // Large 'vpmax|ub'.
  0x23CE59B6, // Large 'vpmax|ud'.
  0x23DA59B6, // Large 'vpmax|uq'.
  0x264C59B6, // Large 'vpmax|uw'.
  0x278659BB, // Large 'vpmin|sb'.
  0x231D59BB, // Large 'vpmin|sd'.
  0x258759BB, // Large 'vpmin|sq'.
  0x256159BB, // Large 'vpmin|sw'.
  0x22A259BB, // Large 'vpmin|ub'.
  0x23CE59BB, // Large 'vpmin|ud'.
  0x23DA59BB, // Large 'vpmin|uq'.
  0x264C59BB, // Large 'vpmin|uw'.
  0x3869570E, // Large 'vpmov|b2m'.
  0x386C570E, // Large 'vpmov|d2m'.
  0x2663570E, // Large 'vpmov|db'.
  0x26BD570E, // Large 'vpmov|dw'.
  0x2857670E, // Large 'vpmovm|2b'.
  0x2265670E, // Large 'vpmovm|2d'.
  0x24E4670E, // Large 'vpmovm|2q'.
  0x2263670E, // Large 'vpmovm|2w'.
  0x1004870E, // Large 'vpmovmsk|b'.
  0x386F570E, // Large 'vpmov|q2m'.
  0x2215570E, // Large 'vpmov|qb'.
  0x2457570E, // Large 'vpmov|qd'.
  0x2725570E, // Large 'vpmov|qw'.
  0x26636716, // Large 'vpmovs|db'.
  0x26BD6716, // Large 'vpmovs|dw'.
  0x22156716, // Large 'vpmovs|qb'.
  0x24576716, // Large 'vpmovs|qd'.
  0x27256716, // Large 'vpmovs|qw'.
  0x26BE6716, // Large 'vpmovs|wb'.
  0x122B8716, // Large 'vpmovsxb|d'.
  0x12158716, // Large 'vpmovsxb|q'.
  0x12648716, // Large 'vpmovsxb|w'.
  0x23CF7716, // Large 'vpmovsx|dq'.
  0x257A7716, // Large 'vpmovsx|wd'.
  0x27097716, // Large 'vpmovsx|wq'.
  0x2663771E, // Large 'vpmovus|db'.
  0x26BD771E, // Large 'vpmovus|dw'.
  0x2215771E, // Large 'vpmovus|qb'.
  0x2457771E, // Large 'vpmovus|qd'.
  0x2725771E, // Large 'vpmovus|qw'.
  0x26BE771E, // Large 'vpmovus|wb'.
  0x3872570E, // Large 'vpmov|w2m'.
  0x26BE570E, // Large 'vpmov|wb'.
  0x122B8727, // Large 'vpmovzxb|d'.
  0x12158727, // Large 'vpmovzxb|q'.
  0x12648727, // Large 'vpmovzxb|w'.
  0x23CF7727, // Large 'vpmovzx|dq'.
  0x257A7727, // Large 'vpmovzx|wd'.
  0x27097727, // Large 'vpmovzx|wq'.
  0x23CF52D3, // Large 'vpmul|dq'.
  0x2561772F, // Large 'vpmulhr|sw'.
  0x264C672F, // Large 'vpmulh|uw'.
  0x1264672F, // Large 'vpmulh|w'.
  0x257F52D3, // Large 'vpmul|ld'.
  0x246252D3, // Large 'vpmul|lq'.
  0x258252D3, // Large 'vpmul|lw'.
  0x2215C2D3, // Large 'vpmultishift|qb'.
  0x33CE52D3, // Large 'vpmul|udq'.
  0x21546875, // Large 'vpopcn|tb'.
  0x26266875, // Large 'vpopcn|td'.
  0x236C6875, // Large 'vpopcn|tq'.
  0x27B86875, // Large 'vpopcn|tw'.
  0x80093E16, // Small 'vpor'.
  0x80493E16, // Small 'vpord'.
  0x81193E16, // Small 'vporq'.
  0x9B22C216, // Small 'vpperm'.
  0x88C7CA16, // Small 'vprold'.
  0xA2C7CA16, // Small 'vprolq'.
  0x242159C0, // Large 'vprol|vd'.
  0x221459C0, // Large 'vprol|vq'.
  0x8927CA16, // Small 'vprord'.
  0xA327CA16, // Small 'vprorq'.
  0x242159C5, // Large 'vpror|vd'.
  0x221459C5, // Large 'vpror|vq'.
  0x8547CA16, // Small 'vprotb'.
  0x8947CA16, // Small 'vprotd'.
  0xA347CA16, // Small 'vprotq'.
  0xAF47CA16, // Small 'vprotw'.
  0x257859CA, // Large 'vpsad|bw'.
  0x229F944E, // Large 'vpscatter|dd'.
  0x23CF944E, // Large 'vpscatter|dq'.
  0x2457944E, // Large 'vpscatter|qd'.
  0x1215A44E, // Large 'vpscatterq|q'.
  0x84144E16, // Small 'vpshab'.
  0x88144E16, // Small 'vpshad'.
  0xA2144E16, // Small 'vpshaq'.
  0xAE144E16, // Small 'vpshaw'.
  0x84C44E16, // Small 'vpshlb'.
  0x88C44E16, // Small 'vpshld'.
  0x122B687B, // Large 'vpshld|d'.
  0x1215687B, // Large 'vpshld|q'.
  0x3668587B, // Large 'vpshl|dvd'.
  0x3880587B, // Large 'vpshl|dvq'.
  0x1264787B, // Large 'vpshldv|w'.
  0x1264687B, // Large 'vpshld|w'.
  0xA2C44E16, // Small 'vpshlq'.
  0xAEC44E16, // Small 'vpshlw'.
  0x122B6883, // Large 'vpshrd|d'.
  0x12156883, // Large 'vpshrd|q'.
  0x36685883, // Large 'vpshr|dvd'.
  0x38805883, // Large 'vpshr|dvq'.
  0x38885883, // Large 'vpshr|dvw'.
  0x12646883, // Large 'vpshrd|w'.
  0x00007364, // Large 'vpshufb'.
  0x2261A364, // Large 'vpshufbitq|mb'.
  0x122B6364, // Large 'vpshuf|d'.
  0x288B6364, // Large 'vpshuf|hw'.
  0x25826364, // Large 'vpshuf|lw'.
  0x204859CF, // Large 'vpsig|nb'.
  0x237659CF, // Large 'vpsig|nd'.
  0x28C159CF, // Large 'vpsig|nw'.
  0x88C64E16, // Small 'vpslld'.
  0x357F49D4, // Large 'vpsl|ldq'.
  0xA2C64E16, // Small 'vpsllq'.
  0x242159D8, // Large 'vpsll|vd'.
  0x221459D8, // Large 'vpsll|vq'.
  0x288959D8, // Large 'vpsll|vw'.
  0xAEC64E16, // Small 'vpsllw'.
  0x88194E16, // Small 'vpsrad'.
  0xA2194E16, // Small 'vpsraq'.
  0x242159DD, // Large 'vpsra|vd'.
  0x221459DD, // Large 'vpsra|vq'.
  0x288959DD, // Large 'vpsra|vw'.
  0xAE194E16, // Small 'vpsraw'.
  0x88C94E16, // Small 'vpsrld'.
  0x357F49DD, // Large 'vpsr|ldq'.
  0xA2C94E16, // Small 'vpsrlq'.
  0x242159E2, // Large 'vpsrl|vd'.
  0x221459E2, // Large 'vpsrl|vq'.
  0x288959E2, // Large 'vpsrl|vw'.
  0xAEC94E16, // Small 'vpsrlw'.
  0x842ACE16, // Small 'vpsubb'.
  0x882ACE16, // Small 'vpsubd'.
  0xA22ACE16, // Small 'vpsubq'.
  0x2786588D, // Large 'vpsub|sb'.
  0x2561588D, // Large 'vpsub|sw'.
  0x2786688D, // Large 'vpsubu|sb'.
  0x2561688D, // Large 'vpsubu|sw'.
  0xAE2ACE16, // Small 'vpsubw'.
  0x122B956F, // Large 'vpternlog|d'.
  0x1215956F, // Large 'vpternlog|q'.
  0xA932D216, // Small 'vptest'.
  0x22616736, // Large 'vptest|mb'.
  0x26D06736, // Large 'vptest|md'.
  0x273D6736, // Large 'vptest|mq'.
  0x28716736, // Large 'vptest|mw'.
  0x22617736, // Large 'vptestn|mb'.
  0x26D07736, // Large 'vptestn|md'.
  0x273D7736, // Large 'vptestn|mq'.
  0x12648736, // Large 'vptestnm|w'.
  0x25788459, // Large 'vpunpckh|bw'.
  0x23CF8459, // Large 'vpunpckh|dq'.
  0x23CF9459, // Large 'vpunpckhq|dq'.
  0x257A8459, // Large 'vpunpckh|wd'.
  0x357C7459, // Large 'vpunpck|lbw'.
  0x357F7459, // Large 'vpunpck|ldq'.
  0x44627459, // Large 'vpunpck|lqdq'.
  0x35827459, // Large 'vpunpck|lwd'.
  0x8127E216, // Small 'vpxor'.
  0x8927E216, // Small 'vpxord'.
  0xA327E216, // Small 'vpxorq'.
  0x22AC6893, // Large 'vrange|pd'.
  0x22826893, // Large 'vrange|ps'.
  0x231D6893, // Large 'vrange|sd'.
  0x22226893, // Large 'vrange|ss'.
  0x22AC6899, // Large 'vrcp14|pd'.
  0x22826899, // Large 'vrcp14|ps'.
  0x231D6899, // Large 'vrcp14|sd'.
  0x22226899, // Large 'vrcp14|ss'.
  0x91080E56, // Small 'vrcpph'.
  0xA7080E56, // Small 'vrcpps'.
  0x91380E56, // Small 'vrcpsh'.
  0xA7380E56, // Small 'vrcpss'.
  0x22AC773F, // Large 'vreduce|pd'.
  0x22A7773F, // Large 'vreduce|ph'.
  0x2282773F, // Large 'vreduce|ps'.
  0x231D773F, // Large 'vreduce|sd'.
  0x22DA773F, // Large 'vreduce|sh'.
  0x2222773F, // Large 'vreduce|ss'.
  0x22AC9466, // Large 'vrndscale|pd'.
  0x22A79466, // Large 'vrndscale|ph'.
  0x22829466, // Large 'vrndscale|ps'.
  0x231D9466, // Large 'vrndscale|sd'.
  0x22DA9466, // Large 'vrndscale|sh'.
  0x22229466, // Large 'vrndscale|ss'.
  0x22AC689F, // Large 'vround|pd'.
  0x2282689F, // Large 'vround|ps'.
  0x231D689F, // Large 'vround|sd'.
  0x2222689F, // Large 'vround|ss'.
  0x22AC8585, // Large 'vrsqrt14|pd'.
  0x22828585, // Large 'vrsqrt14|ps'.
  0x231D8585, // Large 'vrsqrt14|sd'.
  0x22228585, // Large 'vrsqrt14|ss'.
  0x22A76585, // Large 'vrsqrt|ph'.
  0x22826585, // Large 'vrsqrt|ps'.
  0x22DA6585, // Large 'vrsqrt|sh'.
  0x22226585, // Large 'vrsqrt|ss'.
  0x22AC7746, // Large 'vscalef|pd'.
  0x22A77746, // Large 'vscalef|ph'.
  0x22827746, // Large 'vscalef|ps'.
  0x231D7746, // Large 'vscalef|sd'.
  0x22DA7746, // Large 'vscalef|sh'.
  0x22227746, // Large 'vscalef|ss'.
  0x22AC946F, // Large 'vscatterd|pd'.
  0x2282946F, // Large 'vscatterd|ps'.
  0x3478846F, // Large 'vscatter|qpd'.
  0x347B846F, // Large 'vscatter|qps'.
  0x447E736E, // Large 'vsha512|msg1'.
  0x4482736E, // Large 'vsha512|msg2'.
  0x2283A36E, // Large 'vsha512rnd|s2'.
  0x5230558D, // Large 'vshuf|f32x4'.
  0x42356592, // Large 'vshuff|64x2'.
  0x5242558D, // Large 'vshuf|i32x4'.
  0x524C558D, // Large 'vshuf|i64x2'.
  0x22AC558D, // Large 'vshuf|pd'.
  0x2282558D, // Large 'vshuf|ps'.
  0x447E474D, // Large 'vsm3|msg1'.
  0x4482474D, // Large 'vsm3|msg2'.
  0x2283774D, // Large 'vsm3rnd|s2'.
  0x123478A5, // Large 'vsm4key|4'.
  0x12348754, // Large 'vsm4rnds|4'.
  0x33CA49E7, // Large 'vsqr|tpd'.
  0x33D649E7, // Large 'vsqr|tph'.
  0x33E149E7, // Large 'vsqr|tps'.
  0x33E849E7, // Large 'vsqr|tsd'.
  0x33F149E7, // Large 'vsqr|tsh'.
  0x33FA49E7, // Large 'vsqr|tss'.
  0x122878AC, // Large 'vstmxcs|r'.
  0x89015676, // Small 'vsubpd'.
  0x91015676, // Small 'vsubph'.
  0xA7015676, // Small 'vsubps'.
  0x89315676, // Small 'vsubsd'.
  0x91315676, // Small 'vsubsh'.
  0xA7315676, // Small 'vsubss'.
  0x33CA49EB, // Large 'vtes|tpd'.
  0x33E149EB, // Large 'vtes|tps'.
  0x231D68B3, // Large 'vucomi|sd'.
  0x22DA68B3, // Large 'vucomi|sh'.
  0x222268B3, // Large 'vucomi|ss'.
  0x22AC775C, // Large 'vunpckh|pd'.
  0x2282775C, // Large 'vunpckh|ps'.
  0x3763675C, // Large 'vunpck|lpd'.
  0x3766675C, // Large 'vunpck|lps'.
  0x89093F16, // Small 'vxorpd'.
  0xA7093F16, // Small 'vxorps'.
  0x38B95598, // Large 'vzero|all'.
  0x353B7598, // Large 'vzeroup|per'.
  0x89672457, // Small 'wbinvd'.
  0x242168BC, // Large 'wbnoin|vd'.
  0x317058C2, // Large 'wrfsb|ase'.
  0x317058C7, // Large 'wrgsb|ase'.
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
  0x121B8769, // Large 'xresldtr|k'.
  0xA4FA4E58, // Small 'xrstor'.
  0x223565DD, // Large 'xrstor|64'.
  0x115265DD, // Large 'xrstor|s'.
  0x377165DD, // Large 'xrstor|s64'.
  0x805B0678, // Small 'xsave'.
  0x2235559F, // Large 'xsave|64'.
  0x865B0678, // Small 'xsavec'.
  0x38CC559F, // Large 'xsave|c64'.
  0x0000859F, // Large 'xsaveopt'.
  0x2235859F, // Large 'xsaveopt|64'.
  0xA65B0678, // Small 'xsaves'.
  0x3771559F, // Large 'xsave|s64'.
  0xAC2A1678, // Small 'xsetbv'.
  0x121B8774, // Large 'xsusldtr|k'.
  0x81499698  // Small 'xtest'.
};

const char InstDB::alias_name_string_table[] =
  "\x63\x6D\x6F\x76\x6E\x61\x65\x67\x65";


const uint32_t InstDB::alias_name_index_table[] = {
  0x801B3DA3, // Small 'cmova'.
  0x8A1B3DA3, // Small 'cmovae'.
  0x803B3DA3, // Small 'cmovc'.
  0x805B3DA3, // Small 'cmove'.
  0x807B3DA3, // Small 'cmovg'.
  0x8A7B3DA3, // Small 'cmovge'.
  0x82EB3DA3, // Small 'cmovna'.
  0x00007000, // Large 'cmovnae'.
  0x86EB3DA3, // Small 'cmovnc'.
  0x8AEB3DA3, // Small 'cmovne'.
  0x8EEB3DA3, // Small 'cmovng'.
  0x20075000, // Large 'cmovn|ge'.
  0x8B0B3DA3, // Small 'cmovpe'.
  0x9F0B3DA3, // Small 'cmovpo'.
  0x8000002A, // Small 'ja'.
  0x8000142A, // Small 'jae'.
  0x8000006A, // Small 'jc'.
  0x800000AA, // Small 'je'.
  0x800000EA, // Small 'jg'.
  0x800014EA, // Small 'jge'.
  0x800005CA, // Small 'jna'.
  0x800285CA, // Small 'jnae'.
  0x80000DCA, // Small 'jnc'.
  0x800015CA, // Small 'jne'.
  0x80001DCA, // Small 'jng'.
  0x80029DCA, // Small 'jnge'.
  0x8000160A, // Small 'jpe'.
  0x80003E0A, // Small 'jpo'.
  0x80003033, // Small 'sal'.
  0x8000D0B3, // Small 'seta'.
  0x8050D0B3, // Small 'setae'.
  0x8001D0B3, // Small 'setc'.
  0x8002D0B3, // Small 'sete'.
  0x8003D0B3, // Small 'setg'.
  0x8053D0B3, // Small 'setge'.
  0x801750B3, // Small 'setna'.
  0x8A1750B3, // Small 'setnae'.
  0x803750B3, // Small 'setnc'.
  0x805750B3, // Small 'setne'.
  0x807750B3, // Small 'setng'.
  0x8A7750B3, // Small 'setnge'.
  0x805850B3, // Small 'setpe'.
  0x80F850B3, // Small 'setpo'.
  0x800A2437  // Small 'wait'.
};

const uint32_t InstDB::alias_index_to_inst_id_table[] = {
  Inst::kIdCmovnbe, // #0
  Inst::kIdCmovnb, // #1
  Inst::kIdCmovb, // #2
  Inst::kIdCmovz, // #3
  Inst::kIdCmovnle, // #4
  Inst::kIdCmovnl, // #5
  Inst::kIdCmovbe, // #6
  Inst::kIdCmovb, // #7
  Inst::kIdCmovnb, // #8
  Inst::kIdCmovnz, // #9
  Inst::kIdCmovle, // #10
  Inst::kIdCmovl, // #11
  Inst::kIdCmovp, // #12
  Inst::kIdCmovnp, // #13
  Inst::kIdJnbe, // #14
  Inst::kIdJnb, // #15
  Inst::kIdJb, // #16
  Inst::kIdJz, // #17
  Inst::kIdJnle, // #18
  Inst::kIdJnl, // #19
  Inst::kIdJbe, // #20
  Inst::kIdJb, // #21
  Inst::kIdJnb, // #22
  Inst::kIdJnz, // #23
  Inst::kIdJle, // #24
  Inst::kIdJl, // #25
  Inst::kIdJp, // #26
  Inst::kIdJnp, // #27
  Inst::kIdShl, // #28
  Inst::kIdSetnbe, // #29
  Inst::kIdSetnb, // #30
  Inst::kIdSetb, // #31
  Inst::kIdSetz, // #32
  Inst::kIdSetnle, // #33
  Inst::kIdSetnl, // #34
  Inst::kIdSetbe, // #35
  Inst::kIdSetb, // #36
  Inst::kIdSetnb, // #37
  Inst::kIdSetnz, // #38
  Inst::kIdSetle, // #39
  Inst::kIdSetl, // #40
  Inst::kIdSetp, // #41
  Inst::kIdSetnp, // #42
  Inst::kIdFwait  // #43
};
// ----------------------------------------------------------------------------
// ${NameData:End}
#endif // !ASMJIT_NO_TEXT

// x86::InstDB - InstSignature & OpSignature
// =========================================

#ifndef ASMJIT_NO_INTROSPECTION
// ${InstSignatureTable:Begin}
// ------------------- Automatically generated, do not edit -------------------
#define ROW(count, x86, x64, implicit, o0, o1, o2, o3, o4, o5)       \
  { count, uint8_t(x86 ? uint8_t(InstDB::Mode::kX86) : uint8_t(0)) | \
                  (x64 ? uint8_t(InstDB::Mode::kX64) : uint8_t(0)) , \
    implicit,                                                        \
    0,                                                               \
    { o0, o1, o2, o3, o4, o5 }                                       \
  }
const InstDB::InstSignature InstDB::_inst_signature_table[] = {
  ROW(2, 1, 1, 0, 1  , 2  , 0  , 0  , 0  , 0  ), // #0   {r8lo|r8hi|m8|m, r8lo|r8hi}
  ROW(2, 1, 1, 0, 3  , 4  , 0  , 0  , 0  , 0  ), //      {r16|m16|m|sreg, r16}
  ROW(2, 1, 1, 0, 5  , 6  , 0  , 0  , 0  , 0  ), //      {r32|m32|m|sreg, r32}
  ROW(2, 0, 1, 0, 7  , 8  , 0  , 0  , 0  , 0  ), //      {r64|m64|m|sreg|creg|dreg, r64}
  ROW(2, 1, 1, 0, 9  , 10 , 0  , 0  , 0  , 0  ), //      {r8lo|r8hi|m8, i8|u8}
  ROW(2, 1, 1, 0, 11 , 12 , 0  , 0  , 0  , 0  ), //      {r16|m16, i16|u16}
  ROW(2, 1, 1, 0, 13 , 14 , 0  , 0  , 0  , 0  ), //      {r32|m32, i32|u32}
  ROW(2, 0, 1, 0, 15 , 16 , 0  , 0  , 0  , 0  ), //      {r64|m64, i32}
  ROW(2, 1, 1, 0, 2  , 17 , 0  , 0  , 0  , 0  ), //      {r8lo|r8hi, m8|m}
  ROW(2, 1, 1, 0, 4  , 18 , 0  , 0  , 0  , 0  ), //      {r16, m16|m|sreg}
  ROW(2, 1, 1, 0, 6  , 19 , 0  , 0  , 0  , 0  ), //      {r32, m32|m|sreg}
  ROW(2, 0, 1, 0, 8  , 20 , 0  , 0  , 0  , 0  ), //      {r64, m64|m|i64|u64|sreg|creg|dreg}
  ROW(2, 1, 1, 0, 21 , 22 , 0  , 0  , 0  , 0  ), //      {m16|m, sreg}
  ROW(2, 1, 1, 0, 21 , 22 , 0  , 0  , 0  , 0  ), //      {m16|m, sreg}
  ROW(2, 0, 1, 0, 21 , 22 , 0  , 0  , 0  , 0  ), //      {m16|m, sreg}
  ROW(2, 1, 1, 0, 22 , 21 , 0  , 0  , 0  , 0  ), //      {sreg, m16|m}
  ROW(2, 1, 1, 0, 22 , 21 , 0  , 0  , 0  , 0  ), //      {sreg, m16|m}
  ROW(2, 0, 1, 0, 22 , 21 , 0  , 0  , 0  , 0  ), //      {sreg, m16|m}
  ROW(2, 1, 0, 0, 6  , 23 , 0  , 0  , 0  , 0  ), //      {r32, creg|dreg}
  ROW(2, 1, 0, 0, 23 , 6  , 0  , 0  , 0  , 0  ), //      {creg|dreg, r32}
  ROW(2, 1, 1, 0, 1  , 2  , 0  , 0  , 0  , 0  ), // #20  {r8lo|r8hi|m8|m, r8lo|r8hi}
  ROW(2, 1, 1, 0, 24 , 4  , 0  , 0  , 0  , 0  ), //      {r16|m16|m, r16}
  ROW(2, 1, 1, 0, 25 , 6  , 0  , 0  , 0  , 0  ), //      {r32|m32|m, r32}
  ROW(2, 0, 1, 0, 26 , 8  , 0  , 0  , 0  , 0  ), //      {r64|m64|m, r64}
  ROW(2, 1, 1, 0, 9  , 10 , 0  , 0  , 0  , 0  ), //      {r8lo|r8hi|m8, i8|u8}
  ROW(2, 1, 1, 0, 27 , 28 , 0  , 0  , 0  , 0  ), //      {r16|m16|r32|m32, i8}
  ROW(2, 0, 1, 0, 15 , 29 , 0  , 0  , 0  , 0  ), //      {r64|m64, i8|i32}
  ROW(2, 1, 1, 0, 11 , 12 , 0  , 0  , 0  , 0  ), //      {r16|m16, i16|u16}
  ROW(2, 1, 1, 0, 13 , 14 , 0  , 0  , 0  , 0  ), //      {r32|m32, i32|u32}
  ROW(2, 1, 1, 0, 2  , 17 , 0  , 0  , 0  , 0  ), //      {r8lo|r8hi, m8|m}
  ROW(2, 1, 1, 0, 4  , 21 , 0  , 0  , 0  , 0  ), //      {r16, m16|m}
  ROW(2, 1, 1, 0, 6  , 30 , 0  , 0  , 0  , 0  ), //      {r32, m32|m}
  ROW(2, 0, 1, 0, 8  , 31 , 0  , 0  , 0  , 0  ), //      {r64, m64|m}
  ROW(2, 1, 1, 0, 9  , 10 , 0  , 0  , 0  , 0  ), // #33  {r8lo|r8hi|m8, i8|u8}
  ROW(2, 1, 1, 0, 11 , 12 , 0  , 0  , 0  , 0  ), //      {r16|m16, i16|u16}
  ROW(2, 1, 1, 0, 13 , 14 , 0  , 0  , 0  , 0  ), //      {r32|m32, i32|u32}
  ROW(2, 0, 1, 0, 8  , 32 , 0  , 0  , 0  , 0  ), //      {r64, u32|i32|i8|r64|m64|m}
  ROW(2, 0, 1, 0, 33 , 29 , 0  , 0  , 0  , 0  ), //      {m64, i32|i8}
  ROW(2, 1, 1, 0, 27 , 28 , 0  , 0  , 0  , 0  ), //      {r16|m16|r32|m32, i8}
  ROW(2, 1, 1, 0, 1  , 2  , 0  , 0  , 0  , 0  ), //      {r8lo|r8hi|m8|m, r8lo|r8hi}
  ROW(2, 1, 1, 0, 24 , 4  , 0  , 0  , 0  , 0  ), //      {r16|m16|m, r16}
  ROW(2, 1, 1, 0, 25 , 6  , 0  , 0  , 0  , 0  ), //      {r32|m32|m, r32}
  ROW(2, 0, 1, 0, 31 , 8  , 0  , 0  , 0  , 0  ), // #42  {m64|m, r64}
  ROW(2, 1, 1, 0, 2  , 17 , 0  , 0  , 0  , 0  ), //      {r8lo|r8hi, m8|m}
  ROW(2, 1, 1, 0, 4  , 21 , 0  , 0  , 0  , 0  ), //      {r16, m16|m}
  ROW(2, 1, 1, 0, 6  , 30 , 0  , 0  , 0  , 0  ), //      {r32, m32|m}
  ROW(2, 1, 1, 0, 1  , 2  , 0  , 0  , 0  , 0  ), // #46  {r8lo|r8hi|m8|m, r8lo|r8hi}
  ROW(2, 1, 1, 0, 24 , 4  , 0  , 0  , 0  , 0  ), //      {r16|m16|m, r16}
  ROW(2, 1, 1, 0, 25 , 6  , 0  , 0  , 0  , 0  ), //      {r32|m32|m, r32}
  ROW(2, 0, 1, 0, 26 , 8  , 0  , 0  , 0  , 0  ), //      {r64|m64|m, r64}
  ROW(2, 1, 1, 0, 2  , 34 , 0  , 0  , 0  , 0  ), //      {r8lo|r8hi, m8|m|i8|u8}
  ROW(2, 1, 1, 0, 4  , 35 , 0  , 0  , 0  , 0  ), //      {r16, m16|m|i8|i16|u16}
  ROW(2, 1, 1, 0, 6  , 36 , 0  , 0  , 0  , 0  ), //      {r32, m32|m|i8|i32|u32}
  ROW(2, 0, 1, 0, 8  , 37 , 0  , 0  , 0  , 0  ), //      {r64, m64|m|i8|i32}
  ROW(2, 1, 1, 0, 38 , 10 , 0  , 0  , 0  , 0  ), //      {m8, i8|u8}
  ROW(2, 1, 1, 0, 39 , 28 , 0  , 0  , 0  , 0  ), //      {m16|m32, i8}
  ROW(2, 0, 1, 0, 33 , 29 , 0  , 0  , 0  , 0  ), //      {m64, i8|i32}
  ROW(2, 1, 1, 0, 40 , 12 , 0  , 0  , 0  , 0  ), //      {m16, i16|u16}
  ROW(2, 1, 1, 0, 41 , 14 , 0  , 0  , 0  , 0  ), //      {m32, i32|u32}
  ROW(2, 1, 1, 0, 1  , 2  , 0  , 0  , 0  , 0  ), // #59  {r8lo|r8hi|m8|m, r8lo|r8hi}
  ROW(2, 1, 1, 0, 24 , 4  , 0  , 0  , 0  , 0  ), //      {r16|m16|m, r16}
  ROW(2, 1, 1, 0, 25 , 6  , 0  , 0  , 0  , 0  ), //      {r32|m32|m, r32}
  ROW(2, 0, 1, 0, 26 , 8  , 0  , 0  , 0  , 0  ), //      {r64|m64|m, r64}
  ROW(2, 1, 1, 0, 9  , 10 , 0  , 0  , 0  , 0  ), //      {r8lo|r8hi|m8, i8|u8}
  ROW(2, 1, 1, 0, 11 , 12 , 0  , 0  , 0  , 0  ), //      {r16|m16, i16|u16}
  ROW(2, 1, 1, 0, 13 , 14 , 0  , 0  , 0  , 0  ), //      {r32|m32, i32|u32}
  ROW(2, 0, 1, 0, 15 , 29 , 0  , 0  , 0  , 0  ), //      {r64|m64, i32|i8}
  ROW(2, 1, 1, 0, 27 , 28 , 0  , 0  , 0  , 0  ), //      {r16|m16|r32|m32, i8}
  ROW(2, 1, 1, 0, 2  , 17 , 0  , 0  , 0  , 0  ), //      {r8lo|r8hi, m8|m}
  ROW(2, 1, 1, 0, 4  , 21 , 0  , 0  , 0  , 0  ), //      {r16, m16|m}
  ROW(2, 1, 1, 0, 6  , 30 , 0  , 0  , 0  , 0  ), //      {r32, m32|m}
  ROW(2, 0, 1, 0, 8  , 31 , 0  , 0  , 0  , 0  ), //      {r64, m64|m}
  ROW(2, 1, 1, 1, 42 , 1  , 0  , 0  , 0  , 0  ), // #72  {<ax>, r8lo|r8hi|m8|m}
  ROW(3, 1, 1, 2, 43 , 42 , 24 , 0  , 0  , 0  ), //      {<dx>, <ax>, r16|m16|m}
  ROW(3, 1, 1, 2, 44 , 45 , 25 , 0  , 0  , 0  ), //      {<edx>, <eax>, r32|m32|m}
  ROW(3, 0, 1, 2, 46 , 47 , 26 , 0  , 0  , 0  ), //      {<rdx>, <rax>, r64|m64|m}
  ROW(2, 1, 1, 0, 4  , 24 , 0  , 0  , 0  , 0  ), // #76  {r16, r16|m16|m}
  ROW(2, 1, 1, 0, 6  , 25 , 0  , 0  , 0  , 0  ), // #77  {r32, r32|m32|m}
  ROW(2, 0, 1, 0, 8  , 26 , 0  , 0  , 0  , 0  ), //      {r64, r64|m64|m}
  ROW(3, 1, 1, 0, 4  , 24 , 48 , 0  , 0  , 0  ), //      {r16, r16|m16|m, i8|i16|u16}
  ROW(3, 1, 1, 0, 6  , 25 , 49 , 0  , 0  , 0  ), //      {r32, r32|m32|m, i8|i32|u32}
  ROW(3, 0, 1, 0, 8  , 26 , 29 , 0  , 0  , 0  ), //      {r64, r64|m64|m, i8|i32}
  ROW(2, 0, 1, 0, 8  , 50 , 0  , 0  , 0  , 0  ), // #82  {r64, i64|u64}
  ROW(2, 1, 1, 0, 51 , 17 , 0  , 0  , 0  , 0  ), //      {al, m8|m}
  ROW(2, 1, 1, 0, 52 , 21 , 0  , 0  , 0  , 0  ), //      {ax, m16|m}
  ROW(2, 1, 1, 0, 53 , 30 , 0  , 0  , 0  , 0  ), //      {eax, m32|m}
  ROW(2, 0, 1, 0, 54 , 31 , 0  , 0  , 0  , 0  ), //      {rax, m64|m}
  ROW(2, 1, 1, 0, 17 , 51 , 0  , 0  , 0  , 0  ), //      {m8|m, al}
  ROW(2, 1, 1, 0, 21 , 52 , 0  , 0  , 0  , 0  ), //      {m16|m, ax}
  ROW(2, 1, 1, 0, 30 , 53 , 0  , 0  , 0  , 0  ), //      {m32|m, eax}
  ROW(2, 0, 1, 0, 31 , 54 , 0  , 0  , 0  , 0  ), //      {m64|m, rax}
  ROW(2, 1, 1, 0, 1  , 2  , 0  , 0  , 0  , 0  ), // #91  {r8lo|r8hi|m8|m, r8lo|r8hi}
  ROW(2, 1, 1, 0, 24 , 4  , 0  , 0  , 0  , 0  ), //      {r16|m16|m, r16}
  ROW(2, 1, 1, 0, 25 , 6  , 0  , 0  , 0  , 0  ), //      {r32|m32|m, r32}
  ROW(2, 0, 1, 0, 26 , 8  , 0  , 0  , 0  , 0  ), //      {r64|m64|m, r64}
  ROW(2, 1, 1, 0, 9  , 10 , 0  , 0  , 0  , 0  ), //      {r8lo|r8hi|m8, i8|u8}
  ROW(2, 1, 1, 0, 11 , 12 , 0  , 0  , 0  , 0  ), //      {r16|m16, i16|u16}
  ROW(2, 1, 1, 0, 13 , 14 , 0  , 0  , 0  , 0  ), //      {r32|m32, i32|u32}
  ROW(2, 0, 1, 0, 15 , 16 , 0  , 0  , 0  , 0  ), //      {r64|m64, i32}
  ROW(2, 1, 1, 0, 55 , 56 , 0  , 0  , 0  , 0  ), // #99  {xmm, xmm|m128|m}
  ROW(2, 1, 1, 0, 57 , 58 , 0  , 0  , 0  , 0  ), //      {ymm, ymm|m256|m}
  ROW(2, 1, 1, 0, 59 , 55 , 0  , 0  , 0  , 0  ), // #101 {m128|m, xmm}
  ROW(2, 1, 1, 0, 60 , 57 , 0  , 0  , 0  , 0  ), //      {m256|m, ymm}
  ROW(2, 1, 1, 0, 61 , 62 , 0  , 0  , 0  , 0  ), //      {zmm, zmm|m512|m}
  ROW(2, 1, 1, 0, 59 , 55 , 0  , 0  , 0  , 0  ), //      {m128|m, xmm}
  ROW(2, 1, 1, 0, 60 , 57 , 0  , 0  , 0  , 0  ), //      {m256|m, ymm}
  ROW(2, 1, 1, 0, 63 , 61 , 0  , 0  , 0  , 0  ), //      {m512|m, zmm}
  ROW(2, 1, 1, 0, 31 , 55 , 0  , 0  , 0  , 0  ), // #107 {m64|m, xmm}
  ROW(2, 1, 1, 0, 55 , 31 , 0  , 0  , 0  , 0  ), //      {xmm, m64|m}
  ROW(3, 1, 1, 0, 55 , 55 , 55 , 0  , 0  , 0  ), // #109 {xmm, xmm, xmm}
  ROW(3, 1, 1, 0, 55 , 55 , 55 , 0  , 0  , 0  ), //      {xmm, xmm, xmm}
  ROW(2, 1, 1, 0, 31 , 55 , 0  , 0  , 0  , 0  ), //      {m64|m, xmm}
  ROW(2, 1, 1, 0, 55 , 31 , 0  , 0  , 0  , 0  ), //      {xmm, m64|m}
  ROW(3, 1, 1, 0, 55 , 55 , 55 , 0  , 0  , 0  ), //      {xmm, xmm, xmm}
  ROW(3, 1, 1, 0, 55 , 55 , 55 , 0  , 0  , 0  ), //      {xmm, xmm, xmm}
  ROW(2, 1, 1, 0, 30 , 55 , 0  , 0  , 0  , 0  ), // #115 {m32|m, xmm}
  ROW(2, 1, 1, 0, 55 , 30 , 0  , 0  , 0  , 0  ), //      {xmm, m32|m}
  ROW(3, 1, 1, 0, 55 , 55 , 55 , 0  , 0  , 0  ), //      {xmm, xmm, xmm}
  ROW(3, 1, 1, 0, 55 , 55 , 55 , 0  , 0  , 0  ), //      {xmm, xmm, xmm}
  ROW(2, 1, 1, 0, 30 , 55 , 0  , 0  , 0  , 0  ), //      {m32|m, xmm}
  ROW(2, 1, 1, 0, 55 , 30 , 0  , 0  , 0  , 0  ), //      {xmm, m32|m}
  ROW(3, 1, 1, 0, 55 , 55 , 55 , 0  , 0  , 0  ), //      {xmm, xmm, xmm}
  ROW(3, 1, 1, 0, 55 , 55 , 55 , 0  , 0  , 0  ), //      {xmm, xmm, xmm}
  ROW(3, 1, 1, 0, 55 , 55 , 64 , 0  , 0  , 0  ), // #123 {xmm, xmm, xmm|m128|m|i8|u8}
  ROW(3, 1, 1, 0, 55 , 59 , 10 , 0  , 0  , 0  ), //      {xmm, m128|m, i8|u8}
  ROW(3, 1, 1, 0, 57 , 57 , 65 , 0  , 0  , 0  ), //      {ymm, ymm, ymm|m256|m|i8|u8}
  ROW(3, 1, 1, 0, 57 , 60 , 10 , 0  , 0  , 0  ), //      {ymm, m256|m, i8|u8}
  ROW(3, 1, 1, 0, 61 , 61 , 66 , 0  , 0  , 0  ), //      {zmm, zmm, zmm|m512|m|i8|u8}
  ROW(3, 1, 1, 0, 55 , 59 , 10 , 0  , 0  , 0  ), //      {xmm, m128|m, i8|u8}
  ROW(3, 1, 1, 0, 57 , 60 , 10 , 0  , 0  , 0  ), //      {ymm, m256|m, i8|u8}
  ROW(3, 1, 1, 0, 61 , 63 , 10 , 0  , 0  , 0  ), //      {zmm, m512|m, i8|u8}
  ROW(2, 1, 1, 0, 1  , 2  , 0  , 0  , 0  , 0  ), // #131 {r8lo|r8hi|m8|m, r8lo|r8hi}
  ROW(2, 1, 1, 0, 24 , 4  , 0  , 0  , 0  , 0  ), //      {r16|m16|m, r16}
  ROW(2, 1, 1, 0, 25 , 6  , 0  , 0  , 0  , 0  ), //      {r32|m32|m, r32}
  ROW(2, 0, 1, 0, 26 , 8  , 0  , 0  , 0  , 0  ), //      {r64|m64|m, r64}
  ROW(2, 1, 1, 0, 2  , 17 , 0  , 0  , 0  , 0  ), //      {r8lo|r8hi, m8|m}
  ROW(2, 1, 1, 0, 4  , 21 , 0  , 0  , 0  , 0  ), //      {r16, m16|m}
  ROW(2, 1, 1, 0, 6  , 30 , 0  , 0  , 0  , 0  ), //      {r32, m32|m}
  ROW(2, 0, 1, 0, 8  , 31 , 0  , 0  , 0  , 0  ), //      {r64, m64|m}
  ROW(2, 1, 1, 0, 4  , 21 , 0  , 0  , 0  , 0  ), // #139 {r16, m16|m}
  ROW(2, 1, 1, 0, 6  , 30 , 0  , 0  , 0  , 0  ), //      {r32, m32|m}
  ROW(2, 0, 1, 0, 8  , 31 , 0  , 0  , 0  , 0  ), //      {r64, m64|m}
  ROW(2, 1, 1, 0, 21 , 4  , 0  , 0  , 0  , 0  ), //      {m16|m, r16}
  ROW(2, 1, 1, 0, 30 , 6  , 0  , 0  , 0  , 0  ), // #143 {m32|m, r32}
  ROW(2, 0, 1, 0, 31 , 8  , 0  , 0  , 0  , 0  ), //      {m64|m, r64}
  ROW(0, 1, 1, 0, 0  , 0  , 0  , 0  , 0  , 0  ), // #145 {}
  ROW(1, 1, 1, 0, 27 , 0  , 0  , 0  , 0  , 0  ), //      {r16|m16|r32|m32}
  ROW(1, 0, 1, 0, 15 , 0  , 0  , 0  , 0  , 0  ), //      {r64|m64}
  ROW(2, 1, 1, 0, 24 , 4  , 0  , 0  , 0  , 0  ), //      {r16|m16|m, r16}
  ROW(2, 1, 1, 0, 25 , 6  , 0  , 0  , 0  , 0  ), //      {r32|m32|m, r32}
  ROW(2, 0, 1, 0, 26 , 8  , 0  , 0  , 0  , 0  ), //      {r64|m64|m, r64}
  ROW(2, 1, 1, 0, 55 , 56 , 0  , 0  , 0  , 0  ), // #151 {xmm, xmm|m128|m}
  ROW(2, 1, 1, 0, 57 , 58 , 0  , 0  , 0  , 0  ), //      {ymm, ymm|m256|m}
  ROW(2, 1, 1, 0, 61 , 62 , 0  , 0  , 0  , 0  ), //      {zmm, zmm|m512|m}
  ROW(2, 1, 1, 0, 59 , 55 , 0  , 0  , 0  , 0  ), //      {m128|m, xmm}
  ROW(2, 1, 1, 0, 60 , 57 , 0  , 0  , 0  , 0  ), //      {m256|m, ymm}
  ROW(2, 1, 1, 0, 63 , 61 , 0  , 0  , 0  , 0  ), //      {m512|m, zmm}
  ROW(3, 1, 1, 0, 55 , 55 , 64 , 0  , 0  , 0  ), // #157 {xmm, xmm, xmm|m128|m|i8|u8}
  ROW(3, 1, 1, 0, 57 , 57 , 64 , 0  , 0  , 0  ), //      {ymm, ymm, xmm|m128|m|i8|u8}
  ROW(3, 1, 1, 0, 61 , 61 , 64 , 0  , 0  , 0  ), //      {zmm, zmm, xmm|m128|m|i8|u8}
  ROW(3, 1, 1, 0, 55 , 59 , 10 , 0  , 0  , 0  ), //      {xmm, m128|m, i8|u8}
  ROW(3, 1, 1, 0, 57 , 60 , 10 , 0  , 0  , 0  ), //      {ymm, m256|m, i8|u8}
  ROW(3, 1, 1, 0, 61 , 63 , 10 , 0  , 0  , 0  ), //      {zmm, m512|m, i8|u8}
  ROW(2, 1, 1, 0, 24 , 4  , 0  , 0  , 0  , 0  ), // #163 {r16|m16|m, r16}
  ROW(2, 1, 1, 0, 25 , 6  , 0  , 0  , 0  , 0  ), //      {r32|m32|m, r32}
  ROW(2, 0, 1, 0, 26 , 8  , 0  , 0  , 0  , 0  ), //      {r64|m64|m, r64}
  ROW(2, 1, 1, 0, 27 , 10 , 0  , 0  , 0  , 0  ), //      {r16|m16|r32|m32, i8|u8}
  ROW(2, 0, 1, 0, 15 , 10 , 0  , 0  , 0  , 0  ), //      {r64|m64, i8|u8}
  ROW(2, 1, 1, 0, 67 , 68 , 0  , 0  , 0  , 0  ), // #168 {mm, mm|m64|m}
  ROW(2, 0, 1, 0, 69 , 26 , 0  , 0  , 0  , 0  ), //      {mm|xmm, r64|m64|m}
  ROW(2, 1, 1, 0, 31 , 69 , 0  , 0  , 0  , 0  ), //      {m64|m, mm|xmm}
  ROW(2, 0, 1, 0, 26 , 69 , 0  , 0  , 0  , 0  ), //      {r64|m64|m, mm|xmm}
  ROW(2, 1, 1, 0, 55 , 70 , 0  , 0  , 0  , 0  ), // #172 {xmm, xmm|m64|m}
  ROW(1, 1, 1, 0, 11 , 0  , 0  , 0  , 0  , 0  ), // #173 {r16|m16}
  ROW(1, 1, 0, 0, 13 , 0  , 0  , 0  , 0  , 0  ), //      {r32|m32}
  ROW(1, 0, 1, 0, 15 , 0  , 0  , 0  , 0  , 0  ), //      {r64|m64}
  ROW(1, 1, 0, 0, 71 , 0  , 0  , 0  , 0  , 0  ), //      {ds|es|ss}
  ROW(1, 1, 1, 0, 72 , 0  , 0  , 0  , 0  , 0  ), //      {fs|gs}
  ROW(1, 1, 1, 0, 73 , 0  , 0  , 0  , 0  , 0  ), // #178 {r16|m16|i8|u8|i16|u16}
  ROW(1, 1, 0, 0, 74 , 0  , 0  , 0  , 0  , 0  ), //      {r32|m32|i32|u32}
  ROW(1, 0, 1, 0, 75 , 0  , 0  , 0  , 0  , 0  ), //      {r64|m64|i32}
  ROW(1, 1, 0, 0, 76 , 0  , 0  , 0  , 0  , 0  ), //      {cs|ss|ds|es}
  ROW(1, 1, 1, 0, 72 , 0  , 0  , 0  , 0  , 0  ), //      {fs|gs}
  ROW(3, 1, 1, 0, 55 , 77 , 55 , 0  , 0  , 0  ), // #183 {xmm, vm32x, xmm}
  ROW(3, 1, 1, 0, 57 , 78 , 57 , 0  , 0  , 0  ), //      {ymm, vm32y, ymm}
  ROW(2, 1, 1, 0, 55 , 77 , 0  , 0  , 0  , 0  ), //      {xmm, vm32x}
  ROW(2, 1, 1, 0, 57 , 78 , 0  , 0  , 0  , 0  ), //      {ymm, vm32y}
  ROW(2, 1, 1, 0, 61 , 79 , 0  , 0  , 0  , 0  ), //      {zmm, vm32z}
  ROW(3, 1, 1, 0, 55 , 80 , 55 , 0  , 0  , 0  ), // #188 {xmm, vm64x, xmm}
  ROW(3, 1, 1, 0, 57 , 81 , 57 , 0  , 0  , 0  ), //      {ymm, vm64y, ymm}
  ROW(2, 1, 1, 0, 55 , 80 , 0  , 0  , 0  , 0  ), //      {xmm, vm64x}
  ROW(2, 1, 1, 0, 57 , 81 , 0  , 0  , 0  , 0  ), //      {ymm, vm64y}
  ROW(2, 1, 1, 0, 61 , 82 , 0  , 0  , 0  , 0  ), //      {zmm, vm64z}
  ROW(2, 1, 1, 0, 59 , 55 , 0  , 0  , 0  , 0  ), // #193 {m128|m, xmm}
  ROW(2, 1, 1, 0, 60 , 57 , 0  , 0  , 0  , 0  ), //      {m256|m, ymm}
  ROW(2, 1, 1, 0, 59 , 55 , 0  , 0  , 0  , 0  ), //      {m128|m, xmm}
  ROW(2, 1, 1, 0, 60 , 57 , 0  , 0  , 0  , 0  ), //      {m256|m, ymm}
  ROW(2, 1, 1, 0, 63 , 61 , 0  , 0  , 0  , 0  ), //      {m512|m, zmm}
  ROW(2, 1, 1, 0, 55 , 59 , 0  , 0  , 0  , 0  ), // #198 {xmm, m128|m}
  ROW(2, 1, 1, 0, 57 , 60 , 0  , 0  , 0  , 0  ), //      {ymm, m256|m}
  ROW(2, 1, 1, 0, 55 , 59 , 0  , 0  , 0  , 0  ), //      {xmm, m128|m}
  ROW(2, 1, 1, 0, 57 , 60 , 0  , 0  , 0  , 0  ), //      {ymm, m256|m}
  ROW(2, 1, 1, 0, 61 , 63 , 0  , 0  , 0  , 0  ), //      {zmm, m512|m}
  ROW(2, 0, 1, 0, 26 , 55 , 0  , 0  , 0  , 0  ), // #203 {r64|m64|m, xmm}
  ROW(2, 1, 1, 0, 55 , 70 , 0  , 0  , 0  , 0  ), //      {xmm, xmm|m64|m}
  ROW(2, 0, 1, 0, 55 , 26 , 0  , 0  , 0  , 0  ), //      {xmm, r64|m64|m}
  ROW(2, 1, 1, 0, 31 , 55 , 0  , 0  , 0  , 0  ), //      {m64|m, xmm}
  ROW(2, 1, 1, 0, 31 , 55 , 0  , 0  , 0  , 0  ), //      {m64|m, xmm}
  ROW(2, 1, 1, 0, 83 , 84 , 0  , 0  , 0  , 0  ), // #208 {ds:[memBase|zsi|m8], es:[memBase|zdi|m8]}
  ROW(2, 1, 1, 0, 85 , 86 , 0  , 0  , 0  , 0  ), //      {ds:[memBase|zsi|m16], es:[memBase|zdi|m16]}
  ROW(2, 1, 1, 0, 87 , 88 , 0  , 0  , 0  , 0  ), //      {ds:[memBase|zsi|m32], es:[memBase|zdi|m32]}
  ROW(2, 0, 1, 0, 89 , 90 , 0  , 0  , 0  , 0  ), //      {ds:[memBase|zsi|m64], es:[memBase|zdi|m64]}
  ROW(3, 1, 1, 1, 1  , 2  , 91 , 0  , 0  , 0  ), // #212 {r8lo|r8hi|m8|m, r8lo|r8hi, <al>}
  ROW(3, 1, 1, 1, 24 , 4  , 42 , 0  , 0  , 0  ), //      {r16|m16|m, r16, <ax>}
  ROW(3, 1, 1, 1, 25 , 6  , 45 , 0  , 0  , 0  ), //      {r32|m32|m, r32, <eax>}
  ROW(3, 0, 1, 1, 26 , 8  , 47 , 0  , 0  , 0  ), //      {r64|m64|m, r64, <rax>}
  ROW(2, 1, 1, 0, 92 , 93 , 0  , 0  , 0  , 0  ), // #216 {k, k|m64|m}
  ROW(2, 0, 1, 0, 92 , 8  , 0  , 0  , 0  , 0  ), //      {k, r64}
  ROW(2, 1, 1, 0, 31 , 92 , 0  , 0  , 0  , 0  ), //      {m64|m, k}
  ROW(2, 0, 1, 0, 8  , 92 , 0  , 0  , 0  , 0  ), //      {r64, k}
  ROW(2, 1, 1, 0, 51 , 94 , 0  , 0  , 0  , 0  ), // #220 {al, ds:[memBase|zsi|m8|m]}
  ROW(2, 1, 1, 0, 52 , 95 , 0  , 0  , 0  , 0  ), //      {ax, ds:[memBase|zsi|m16|m]}
  ROW(2, 1, 1, 0, 53 , 96 , 0  , 0  , 0  , 0  ), //      {eax, ds:[memBase|zsi|m32|m]}
  ROW(2, 0, 1, 0, 54 , 97 , 0  , 0  , 0  , 0  ), //      {rax, ds:[memBase|zsi|m64|m]}
  ROW(2, 1, 1, 0, 84 , 83 , 0  , 0  , 0  , 0  ), // #224 {es:[memBase|zdi|m8], ds:[memBase|zsi|m8]}
  ROW(2, 1, 1, 0, 86 , 85 , 0  , 0  , 0  , 0  ), //      {es:[memBase|zdi|m16], ds:[memBase|zsi|m16]}
  ROW(2, 1, 1, 0, 88 , 87 , 0  , 0  , 0  , 0  ), //      {es:[memBase|zdi|m32], ds:[memBase|zsi|m32]}
  ROW(2, 0, 1, 0, 90 , 89 , 0  , 0  , 0  , 0  ), //      {es:[memBase|zdi|m64], ds:[memBase|zsi|m64]}
  ROW(2, 1, 1, 0, 51 , 98 , 0  , 0  , 0  , 0  ), // #228 {al, es:[memBase|zdi|m8|m]}
  ROW(2, 1, 1, 0, 52 , 99 , 0  , 0  , 0  , 0  ), //      {ax, es:[memBase|zdi|m16|m]}
  ROW(2, 1, 1, 0, 53 , 100, 0  , 0  , 0  , 0  ), //      {eax, es:[memBase|zdi|m32|m]}
  ROW(2, 0, 1, 0, 54 , 101, 0  , 0  , 0  , 0  ), //      {rax, es:[memBase|zdi|m64|m]}
  ROW(2, 1, 1, 0, 98 , 51 , 0  , 0  , 0  , 0  ), // #232 {es:[memBase|zdi|m8|m], al}
  ROW(2, 1, 1, 0, 99 , 52 , 0  , 0  , 0  , 0  ), //      {es:[memBase|zdi|m16|m], ax}
  ROW(2, 1, 1, 0, 100, 53 , 0  , 0  , 0  , 0  ), //      {es:[memBase|zdi|m32|m], eax}
  ROW(2, 0, 1, 0, 101, 54 , 0  , 0  , 0  , 0  ), //      {es:[memBase|zdi|m64|m], rax}
  ROW(4, 1, 1, 0, 55 , 55 , 55 , 56 , 0  , 0  ), // #236 {xmm, xmm, xmm, xmm|m128|m}
  ROW(4, 1, 1, 0, 57 , 57 , 57 , 58 , 0  , 0  ), //      {ymm, ymm, ymm, ymm|m256|m}
  ROW(4, 1, 1, 0, 55 , 55 , 59 , 55 , 0  , 0  ), //      {xmm, xmm, m128|m, xmm}
  ROW(4, 1, 1, 0, 57 , 57 , 60 , 57 , 0  , 0  ), //      {ymm, ymm, m256|m, ymm}
  ROW(3, 1, 1, 0, 55 , 77 , 55 , 0  , 0  , 0  ), // #240 {xmm, vm32x, xmm}
  ROW(3, 1, 1, 0, 57 , 77 , 57 , 0  , 0  , 0  ), //      {ymm, vm32x, ymm}
  ROW(2, 1, 1, 0, 102, 77 , 0  , 0  , 0  , 0  ), //      {xmm|ymm, vm32x}
  ROW(2, 1, 1, 0, 61 , 78 , 0  , 0  , 0  , 0  ), //      {zmm, vm32y}
  ROW(3, 1, 1, 0, 59 , 55 , 55 , 0  , 0  , 0  ), // #244 {m128|m, xmm, xmm}
  ROW(3, 1, 1, 0, 60 , 57 , 57 , 0  , 0  , 0  ), //      {m256|m, ymm, ymm}
  ROW(3, 1, 1, 0, 55 , 55 , 59 , 0  , 0  , 0  ), //      {xmm, xmm, m128|m}
  ROW(3, 1, 1, 0, 57 , 57 , 60 , 0  , 0  , 0  ), //      {ymm, ymm, m256|m}
  ROW(2, 1, 1, 0, 31 , 55 , 0  , 0  , 0  , 0  ), // #248 {m64|m, xmm}
  ROW(3, 1, 1, 0, 55 , 55 , 31 , 0  , 0  , 0  ), //      {xmm, xmm, m64|m}
  ROW(2, 1, 1, 0, 31 , 55 , 0  , 0  , 0  , 0  ), //      {m64|m, xmm}
  ROW(3, 1, 1, 0, 55 , 55 , 31 , 0  , 0  , 0  ), //      {xmm, xmm, m64|m}
  ROW(2, 1, 1, 0, 21 , 55 , 0  , 0  , 0  , 0  ), // #252 {m16|m, xmm}
  ROW(2, 1, 1, 0, 55 , 21 , 0  , 0  , 0  , 0  ), //      {xmm, m16|m}
  ROW(3, 1, 1, 0, 55 , 55 , 55 , 0  , 0  , 0  ), //      {xmm, xmm, xmm}
  ROW(3, 1, 1, 0, 55 , 55 , 55 , 0  , 0  , 0  ), //      {xmm, xmm, xmm}
  ROW(5, 1, 1, 0, 55 , 55 , 56 , 55 , 103, 0  ), // #256 {xmm, xmm, xmm|m128|m, xmm, i4|u4}
  ROW(5, 1, 1, 0, 57 , 57 , 58 , 57 , 103, 0  ), //      {ymm, ymm, ymm|m256|m, ymm, i4|u4}
  ROW(5, 1, 1, 0, 55 , 55 , 55 , 59 , 103, 0  ), //      {xmm, xmm, xmm, m128|m, i4|u4}
  ROW(5, 1, 1, 0, 57 , 57 , 57 , 60 , 103, 0  ), //      {ymm, ymm, ymm, m256|m, i4|u4}
  ROW(3, 1, 1, 0, 57 , 58 , 10 , 0  , 0  , 0  ), // #260 {ymm, ymm|m256|m, i8|u8}
  ROW(3, 1, 1, 0, 57 , 57 , 58 , 0  , 0  , 0  ), //      {ymm, ymm, ymm|m256|m}
  ROW(3, 1, 1, 0, 61 , 61 , 66 , 0  , 0  , 0  ), //      {zmm, zmm, zmm|m512|m|i8|u8}
  ROW(3, 1, 1, 0, 61 , 63 , 10 , 0  , 0  , 0  ), //      {zmm, m512|m, i8|u8}
  ROW(1, 1, 0, 0, 104, 0  , 0  , 0  , 0  , 0  ), // #264 {rel16|r16|m16|m|r32|m32}
  ROW(1, 1, 1, 0, 105, 0  , 0  , 0  , 0  , 0  ), // #265 {rel32}
  ROW(1, 0, 1, 0, 26 , 0  , 0  , 0  , 0  , 0  ), //      {r64|m64|m}
  ROW(1, 1, 0, 0, 106, 0  , 0  , 0  , 0  , 0  ), // #267 {r16|r32}
  ROW(1, 1, 1, 0, 107, 0  , 0  , 0  , 0  , 0  ), // #268 {r8lo|r8hi|m8|r16|m16|r32|m32}
  ROW(1, 0, 1, 0, 15 , 0  , 0  , 0  , 0  , 0  ), //      {r64|m64}
  ROW(1, 1, 1, 0, 108, 0  , 0  , 0  , 0  , 0  ), // #270 {m32|m64}
  ROW(2, 1, 1, 0, 109, 110, 0  , 0  , 0  , 0  ), //      {st0, st}
  ROW(2, 1, 1, 0, 110, 109, 0  , 0  , 0  , 0  ), //      {st, st0}
  ROW(1, 1, 1, 0, 111, 0  , 0  , 0  , 0  , 0  ), // #273 {rel8|rel32}
  ROW(1, 1, 0, 0, 112, 0  , 0  , 0  , 0  , 0  ), //      {rel16|r32|m32}
  ROW(1, 0, 1, 0, 15 , 0  , 0  , 0  , 0  , 0  ), //      {r64|m64}
  ROW(2, 1, 0, 0, 12 , 113, 0  , 0  , 0  , 0  ), // #276 {i16|u16, i16|u16|i32|u32}
  ROW(1, 1, 1, 0, 114, 0  , 0  , 0  , 0  , 0  ), //      {m32|m|m48}
  ROW(1, 0, 1, 0, 115, 0  , 0  , 0  , 0  , 0  ), //      {m80|m}
  ROW(2, 1, 1, 0, 4  , 30 , 0  , 0  , 0  , 0  ), // #279 {r16, m32|m}
  ROW(2, 1, 1, 0, 6  , 116, 0  , 0  , 0  , 0  ), //      {r32, m48|m}
  ROW(2, 0, 1, 0, 8  , 115, 0  , 0  , 0  , 0  ), //      {r64, m80|m}
  ROW(2, 1, 1, 0, 4  , 24 , 0  , 0  , 0  , 0  ), // #282 {r16, r16|m16|m}
  ROW(2, 1, 1, 0, 6  , 117, 0  , 0  , 0  , 0  ), //      {r32, r32|m16|m}
  ROW(2, 0, 1, 0, 8  , 117, 0  , 0  , 0  , 0  ), //      {r64, r32|m16|m}
  ROW(2, 1, 1, 0, 4  , 9  , 0  , 0  , 0  , 0  ), // #285 {r16, r8lo|r8hi|m8}
  ROW(2, 1, 1, 0, 6  , 118, 0  , 0  , 0  , 0  ), //      {r32, r8lo|r8hi|m8|r16|m16}
  ROW(2, 0, 1, 0, 8  , 118, 0  , 0  , 0  , 0  ), //      {r64, r8lo|r8hi|m8|r16|m16}
  ROW(3, 1, 1, 0, 24 , 4  , 119, 0  , 0  , 0  ), // #288 {r16|m16|m, r16, cl|i8|u8}
  ROW(3, 1, 1, 0, 25 , 6  , 119, 0  , 0  , 0  ), //      {r32|m32|m, r32, cl|i8|u8}
  ROW(3, 0, 1, 0, 26 , 8  , 119, 0  , 0  , 0  ), //      {r64|m64|m, r64, cl|i8|u8}
  ROW(3, 1, 1, 0, 55 , 55 , 56 , 0  , 0  , 0  ), // #291 {xmm, xmm, xmm|m128|m}
  ROW(3, 1, 1, 0, 57 , 57 , 58 , 0  , 0  , 0  ), // #292 {ymm, ymm, ymm|m256|m}
  ROW(3, 1, 1, 0, 61 , 61 , 62 , 0  , 0  , 0  ), //      {zmm, zmm, zmm|m512|m}
  ROW(4, 1, 1, 0, 55 , 55 , 56 , 10 , 0  , 0  ), // #294 {xmm, xmm, xmm|m128|m, i8|u8}
  ROW(4, 1, 1, 0, 57 , 57 , 58 , 10 , 0  , 0  ), // #295 {ymm, ymm, ymm|m256|m, i8|u8}
  ROW(4, 1, 1, 0, 61 , 61 , 62 , 10 , 0  , 0  ), //      {zmm, zmm, zmm|m512|m, i8|u8}
  ROW(4, 1, 1, 0, 120, 55 , 56 , 10 , 0  , 0  ), // #297 {xmm|k, xmm, xmm|m128|m, i8|u8}
  ROW(4, 1, 1, 0, 121, 57 , 58 , 10 , 0  , 0  ), //      {ymm|k, ymm, ymm|m256|m, i8|u8}
  ROW(4, 1, 1, 0, 92 , 61 , 62 , 10 , 0  , 0  ), //      {k, zmm, zmm|m512|m, i8|u8}
  ROW(4, 1, 1, 0, 92 , 55 , 56 , 10 , 0  , 0  ), // #300 {k, xmm, xmm|m128|m, i8|u8}
  ROW(4, 1, 1, 0, 92 , 57 , 58 , 10 , 0  , 0  ), //      {k, ymm, ymm|m256|m, i8|u8}
  ROW(4, 1, 1, 0, 92 , 61 , 62 , 10 , 0  , 0  ), //      {k, zmm, zmm|m512|m, i8|u8}
  ROW(2, 1, 1, 0, 56 , 55 , 0  , 0  , 0  , 0  ), // #303 {xmm|m128|m, xmm}
  ROW(2, 1, 1, 0, 58 , 57 , 0  , 0  , 0  , 0  ), //      {ymm|m256|m, ymm}
  ROW(2, 1, 1, 0, 62 , 61 , 0  , 0  , 0  , 0  ), //      {zmm|m512|m, zmm}
  ROW(2, 1, 1, 0, 55 , 70 , 0  , 0  , 0  , 0  ), // #306 {xmm, xmm|m64|m}
  ROW(2, 1, 1, 0, 57 , 56 , 0  , 0  , 0  , 0  ), //      {ymm, xmm|m128|m}
  ROW(2, 1, 1, 0, 61 , 58 , 0  , 0  , 0  , 0  ), //      {zmm, ymm|m256|m}
  ROW(2, 1, 1, 0, 55 , 122, 0  , 0  , 0  , 0  ), // #309 {xmm, xmm|m32|m}
  ROW(2, 1, 1, 0, 57 , 70 , 0  , 0  , 0  , 0  ), //      {ymm, xmm|m64|m}
  ROW(2, 1, 1, 0, 61 , 56 , 0  , 0  , 0  , 0  ), //      {zmm, xmm|m128|m}
  ROW(3, 1, 1, 0, 70 , 55 , 10 , 0  , 0  , 0  ), // #312 {xmm|m64|m, xmm, i8|u8}
  ROW(3, 1, 1, 0, 56 , 57 , 10 , 0  , 0  , 0  ), // #313 {xmm|m128|m, ymm, i8|u8}
  ROW(3, 1, 1, 0, 58 , 61 , 10 , 0  , 0  , 0  ), // #314 {ymm|m256|m, zmm, i8|u8}
  ROW(3, 1, 1, 0, 55 , 123, 55 , 0  , 0  , 0  ), // #315 {xmm, vm64x|vm64y, xmm}
  ROW(2, 1, 1, 0, 55 , 123, 0  , 0  , 0  , 0  ), //      {xmm, vm64x|vm64y}
  ROW(2, 1, 1, 0, 57 , 82 , 0  , 0  , 0  , 0  ), //      {ymm, vm64z}
  ROW(3, 1, 1, 0, 55 , 56 , 10 , 0  , 0  , 0  ), // #318 {xmm, xmm|m128|m, i8|u8}
  ROW(3, 1, 1, 0, 57 , 58 , 10 , 0  , 0  , 0  ), //      {ymm, ymm|m256|m, i8|u8}
  ROW(3, 1, 1, 0, 61 , 62 , 10 , 0  , 0  , 0  ), //      {zmm, zmm|m512|m, i8|u8}
  ROW(2, 1, 1, 0, 55 , 70 , 0  , 0  , 0  , 0  ), // #321 {xmm, xmm|m64|m}
  ROW(2, 1, 1, 0, 57 , 58 , 0  , 0  , 0  , 0  ), //      {ymm, ymm|m256|m}
  ROW(2, 1, 1, 0, 61 , 62 , 0  , 0  , 0  , 0  ), //      {zmm, zmm|m512|m}
  ROW(4, 1, 1, 0, 92 , 92 , 55 , 56 , 0  , 0  ), // #324 {k, k, xmm, xmm|m128|m}
  ROW(4, 1, 1, 0, 92 , 92 , 57 , 58 , 0  , 0  ), //      {k, k, ymm, ymm|m256|m}
  ROW(4, 1, 1, 0, 92 , 92 , 61 , 62 , 0  , 0  ), //      {k, k, zmm, zmm|m512|m}
  ROW(3, 1, 1, 0, 120, 55 , 56 , 0  , 0  , 0  ), // #327 {xmm|k, xmm, xmm|m128|m}
  ROW(3, 1, 1, 0, 121, 57 , 58 , 0  , 0  , 0  ), //      {ymm|k, ymm, ymm|m256|m}
  ROW(3, 1, 1, 0, 92 , 61 , 62 , 0  , 0  , 0  ), //      {k, zmm, zmm|m512|m}
  ROW(2, 1, 1, 0, 122, 55 , 0  , 0  , 0  , 0  ), // #330 {xmm|m32|m, xmm}
  ROW(2, 1, 1, 0, 70 , 57 , 0  , 0  , 0  , 0  ), //      {xmm|m64|m, ymm}
  ROW(2, 1, 1, 0, 56 , 61 , 0  , 0  , 0  , 0  ), //      {xmm|m128|m, zmm}
  ROW(2, 1, 1, 0, 70 , 55 , 0  , 0  , 0  , 0  ), // #333 {xmm|m64|m, xmm}
  ROW(2, 1, 1, 0, 56 , 57 , 0  , 0  , 0  , 0  ), //      {xmm|m128|m, ymm}
  ROW(2, 1, 1, 0, 58 , 61 , 0  , 0  , 0  , 0  ), //      {ymm|m256|m, zmm}
  ROW(2, 1, 1, 0, 124, 55 , 0  , 0  , 0  , 0  ), // #336 {xmm|m16|m, xmm}
  ROW(2, 1, 1, 0, 122, 57 , 0  , 0  , 0  , 0  ), //      {xmm|m32|m, ymm}
  ROW(2, 1, 1, 0, 70 , 61 , 0  , 0  , 0  , 0  ), //      {xmm|m64|m, zmm}
  ROW(2, 1, 1, 0, 55 , 124, 0  , 0  , 0  , 0  ), // #339 {xmm, xmm|m16|m}
  ROW(2, 1, 1, 0, 57 , 122, 0  , 0  , 0  , 0  ), //      {ymm, xmm|m32|m}
  ROW(2, 1, 1, 0, 61 , 70 , 0  , 0  , 0  , 0  ), //      {zmm, xmm|m64|m}
  ROW(2, 1, 1, 0, 77 , 55 , 0  , 0  , 0  , 0  ), // #342 {vm32x, xmm}
  ROW(2, 1, 1, 0, 78 , 57 , 0  , 0  , 0  , 0  ), //      {vm32y, ymm}
  ROW(2, 1, 1, 0, 79 , 61 , 0  , 0  , 0  , 0  ), //      {vm32z, zmm}
  ROW(2, 1, 1, 0, 80 , 55 , 0  , 0  , 0  , 0  ), // #345 {vm64x, xmm}
  ROW(2, 1, 1, 0, 81 , 57 , 0  , 0  , 0  , 0  ), //      {vm64y, ymm}
  ROW(2, 1, 1, 0, 82 , 61 , 0  , 0  , 0  , 0  ), //      {vm64z, zmm}
  ROW(3, 1, 1, 0, 92 , 55 , 56 , 0  , 0  , 0  ), // #348 {k, xmm, xmm|m128|m}
  ROW(3, 1, 1, 0, 92 , 57 , 58 , 0  , 0  , 0  ), //      {k, ymm, ymm|m256|m}
  ROW(3, 1, 1, 0, 92 , 61 , 62 , 0  , 0  , 0  ), //      {k, zmm, zmm|m512|m}
  ROW(3, 1, 1, 0, 6  , 6  , 25 , 0  , 0  , 0  ), // #351 {r32, r32, r32|m32|m}
  ROW(3, 0, 1, 0, 8  , 8  , 26 , 0  , 0  , 0  ), //      {r64, r64, r64|m64|m}
  ROW(3, 1, 1, 0, 6  , 25 , 6  , 0  , 0  , 0  ), // #353 {r32, r32|m32|m, r32}
  ROW(3, 0, 1, 0, 8  , 26 , 8  , 0  , 0  , 0  ), //      {r64, r64|m64|m, r64}
  ROW(2, 1, 0, 0, 4  , 30 , 0  , 0  , 0  , 0  ), // #355 {r16, m32|m}
  ROW(2, 1, 0, 0, 6  , 31 , 0  , 0  , 0  , 0  ), //      {r32, m64|m}
  ROW(1, 1, 1, 0, 106, 0  , 0  , 0  , 0  , 0  ), // #357 {r16|r32}
  ROW(1, 0, 1, 0, 8  , 0  , 0  , 0  , 0  , 0  ), // #358 {r64}
  ROW(3, 1, 1, 0, 30 , 6  , 6  , 0  , 0  , 0  ), // #359 {m32|m, r32, r32}
  ROW(3, 0, 1, 0, 31 , 8  , 8  , 0  , 0  , 0  ), //      {m64|m, r64, r64}
  ROW(2, 1, 1, 0, 6  , 107, 0  , 0  , 0  , 0  ), // #361 {r32, r8lo|r8hi|m8|r16|m16|r32|m32}
  ROW(2, 0, 1, 0, 8  , 125, 0  , 0  , 0  , 0  ), //      {r64, r8lo|r8hi|m8|r64|m64}
  ROW(2, 1, 1, 0, 6  , 70 , 0  , 0  , 0  , 0  ), // #363 {r32, xmm|m64|m}
  ROW(2, 0, 1, 0, 8  , 70 , 0  , 0  , 0  , 0  ), //      {r64, xmm|m64|m}
  ROW(2, 1, 1, 0, 55 , 25 , 0  , 0  , 0  , 0  ), // #365 {xmm, r32|m32|m}
  ROW(2, 0, 1, 0, 55 , 26 , 0  , 0  , 0  , 0  ), //      {xmm, r64|m64|m}
  ROW(2, 1, 1, 0, 6  , 122, 0  , 0  , 0  , 0  ), // #367 {r32, xmm|m32|m}
  ROW(2, 0, 1, 0, 8  , 122, 0  , 0  , 0  , 0  ), //      {r64, xmm|m32|m}
  ROW(2, 1, 0, 0, 126, 63 , 0  , 0  , 0  , 0  ), // #369 {es:[m|m512|memBase], m512|m}
  ROW(2, 0, 1, 0, 126, 63 , 0  , 0  , 0  , 0  ), //      {es:[m|m512|memBase], m512|m}
  ROW(3, 1, 1, 0, 55 , 10 , 10 , 0  , 0  , 0  ), // #371 {xmm, i8|u8, i8|u8}
  ROW(2, 1, 1, 0, 55 , 55 , 0  , 0  , 0  , 0  ), // #372 {xmm, xmm}
  ROW(0, 1, 1, 0, 0  , 0  , 0  , 0  , 0  , 0  ), // #373 {}
  ROW(1, 1, 1, 0, 110, 0  , 0  , 0  , 0  , 0  ), // #374 {st}
  ROW(0, 1, 1, 0, 0  , 0  , 0  , 0  , 0  , 0  ), // #375 {}
  ROW(1, 1, 1, 0, 127, 0  , 0  , 0  , 0  , 0  ), // #376 {m32|m64|st}
  ROW(2, 1, 1, 0, 55 , 55 , 0  , 0  , 0  , 0  ), // #377 {xmm, xmm}
  ROW(4, 1, 1, 0, 55 , 55 , 10 , 10 , 0  , 0  ), //      {xmm, xmm, i8|u8, i8|u8}
  ROW(2, 1, 0, 0, 6  , 59 , 0  , 0  , 0  , 0  ), // #379 {r32, m128|m}
  ROW(2, 0, 1, 0, 8  , 59 , 0  , 0  , 0  , 0  ), //      {r64, m128|m}
  ROW(2, 1, 0, 2, 45 , 128, 0  , 0  , 0  , 0  ), // #381 {<eax>, <ecx>}
  ROW(2, 0, 1, 2, 129, 128, 0  , 0  , 0  , 0  ), //      {<eax|rax>, <ecx>}
  ROW(3, 1, 0, 3, 45 , 44 , 128, 0  , 0  , 0  ), // #383 {<eax>, <edx>, <ecx>}
  ROW(3, 0, 1, 3, 129, 44 , 128, 0  , 0  , 0  ), //      {<eax|rax>, <edx>, <ecx>}
  ROW(1, 1, 1, 0, 111, 0  , 0  , 0  , 0  , 0  ), // #385 {rel8|rel32}
  ROW(1, 1, 0, 0, 105, 0  , 0  , 0  , 0  , 0  ), //      {rel16}
  ROW(2, 1, 0, 1, 130, 131, 0  , 0  , 0  , 0  ), // #387 {<cx|ecx>, rel8}
  ROW(2, 0, 1, 1, 132, 131, 0  , 0  , 0  , 0  ), //      {<ecx|rcx>, rel8}
  ROW(2, 1, 1, 0, 92 , 133, 0  , 0  , 0  , 0  ), // #389 {k, k|m8|m|r32}
  ROW(2, 1, 1, 0, 134, 92 , 0  , 0  , 0  , 0  ), //      {m8|m|r32, k}
  ROW(2, 1, 1, 0, 92 , 135, 0  , 0  , 0  , 0  ), // #391 {k, k|m32|m|r32}
  ROW(2, 1, 1, 0, 25 , 92 , 0  , 0  , 0  , 0  ), //      {m32|m|r32, k}
  ROW(2, 1, 1, 0, 92 , 136, 0  , 0  , 0  , 0  ), // #393 {k, k|m16|m|r32}
  ROW(2, 1, 1, 0, 117, 92 , 0  , 0  , 0  , 0  ), //      {m16|m|r32, k}
  ROW(2, 1, 0, 0, 4  , 30 , 0  , 0  , 0  , 0  ), // #395 {r16, m32|m}
  ROW(2, 1, 0, 0, 6  , 116, 0  , 0  , 0  , 0  ), //      {r32, m48|m}
  ROW(2, 1, 1, 0, 106, 137, 0  , 0  , 0  , 0  ), // #397 {r16|r32, m|m8|m16|m32|m48|m64|m80|m128|m256|m512|m1024}
  ROW(2, 0, 1, 0, 8  , 137, 0  , 0  , 0  , 0  ), //      {r64, m|m8|m16|m32|m48|m64|m80|m128|m256|m512|m1024}
  ROW(1, 1, 1, 0, 6  , 0  , 0  , 0  , 0  , 0  ), // #399 {r32}
  ROW(1, 0, 1, 0, 8  , 0  , 0  , 0  , 0  , 0  ), //      {r64}
  ROW(3, 1, 1, 0, 6  , 25 , 14 , 0  , 0  , 0  ), // #401 {r32, r32|m32|m, i32|u32}
  ROW(3, 0, 1, 0, 8  , 25 , 14 , 0  , 0  , 0  ), //      {r64, r32|m32|m, i32|u32}
  ROW(2, 1, 1, 0, 55 , 56 , 0  , 0  , 0  , 0  ), // #403 {xmm, xmm|m128|m}
  ROW(2, 1, 1, 0, 59 , 55 , 0  , 0  , 0  , 0  ), //      {m128|m, xmm}
  ROW(2, 1, 1, 0, 69 , 25 , 0  , 0  , 0  , 0  ), // #405 {mm|xmm, r32|m32|m}
  ROW(2, 1, 1, 0, 25 , 69 , 0  , 0  , 0  , 0  ), //      {r32|m32|m, mm|xmm}
  ROW(2, 1, 1, 0, 126, 63 , 0  , 0  , 0  , 0  ), // #407 {es:[m|m512|memBase], m512|m}
  ROW(2, 1, 1, 0, 126, 63 , 0  , 0  , 0  , 0  ), //      {es:[m|m512|memBase], m512|m}
  ROW(2, 1, 1, 0, 55 , 70 , 0  , 0  , 0  , 0  ), // #409 {xmm, xmm|m64|m}
  ROW(2, 1, 1, 0, 31 , 55 , 0  , 0  , 0  , 0  ), //      {m64|m, xmm}
  ROW(2, 1, 1, 0, 55 , 122, 0  , 0  , 0  , 0  ), // #411 {xmm, xmm|m32|m}
  ROW(2, 1, 1, 0, 30 , 55 , 0  , 0  , 0  , 0  ), //      {m32|m, xmm}
  ROW(2, 0, 1, 0, 4  , 24 , 0  , 0  , 0  , 0  ), // #413 {r16, r16|m16|m}
  ROW(2, 0, 1, 0, 138, 25 , 0  , 0  , 0  , 0  ), //      {r32|r64, r32|m32|m}
  ROW(4, 1, 1, 1, 6  , 6  , 25 , 44 , 0  , 0  ), // #415 {r32, r32, r32|m32|m, <edx>}
  ROW(4, 0, 1, 1, 8  , 8  , 26 , 46 , 0  , 0  ), //      {r64, r64, r64|m64|m, <rdx>}
  ROW(2, 1, 1, 0, 67 , 68 , 0  , 0  , 0  , 0  ), // #417 {mm, mm|m64|m}
  ROW(2, 1, 1, 0, 55 , 56 , 0  , 0  , 0  , 0  ), //      {xmm, xmm|m128|m}
  ROW(3, 1, 1, 0, 67 , 68 , 10 , 0  , 0  , 0  ), // #419 {mm, mm|m64|m, i8|u8}
  ROW(3, 1, 1, 0, 55 , 56 , 10 , 0  , 0  , 0  ), //      {xmm, xmm|m128|m, i8|u8}
  ROW(3, 1, 1, 0, 6  , 69 , 10 , 0  , 0  , 0  ), // #421 {r32, mm|xmm, i8|u8}
  ROW(3, 1, 1, 0, 21 , 55 , 10 , 0  , 0  , 0  ), //      {m16|m, xmm, i8|u8}
  ROW(2, 1, 1, 0, 67 , 139, 0  , 0  , 0  , 0  ), // #423 {mm, mm|m64|m|i8|u8}
  ROW(2, 1, 1, 0, 55 , 64 , 0  , 0  , 0  , 0  ), //      {xmm, xmm|m128|m|i8|u8}
  ROW(1, 1, 1, 0, 25 , 0  , 0  , 0  , 0  , 0  ), // #425 {r32|m32|m}
  ROW(1, 0, 1, 0, 26 , 0  , 0  , 0  , 0  , 0  ), //      {r64|m64|m}
  ROW(2, 1, 1, 0, 67 , 140, 0  , 0  , 0  , 0  ), // #427 {mm, mm|m32|m}
  ROW(2, 1, 1, 0, 55 , 56 , 0  , 0  , 0  , 0  ), //      {xmm, xmm|m128|m}
  ROW(2, 1, 1, 0, 107, 119, 0  , 0  , 0  , 0  ), // #429 {r8lo|r8hi|m8|r16|m16|r32|m32, cl|i8|u8}
  ROW(2, 0, 1, 0, 15 , 119, 0  , 0  , 0  , 0  ), //      {r64|m64, cl|i8|u8}
  ROW(3, 1, 1, 3, 44 , 45 , 128, 0  , 0  , 0  ), // #431 {<edx>, <eax>, <ecx>}
  ROW(2, 0, 1, 0, 8  , 14 , 0  , 0  , 0  , 0  ), //      {r64, i32|u32}
  ROW(1, 1, 0, 0, 6  , 0  , 0  , 0  , 0  , 0  ), // #433 {r32}
  ROW(1, 0, 1, 0, 8  , 0  , 0  , 0  , 0  , 0  ), //      {r64}
  ROW(0, 1, 1, 0, 0  , 0  , 0  , 0  , 0  , 0  ), // #435 {}
  ROW(1, 1, 1, 0, 141, 0  , 0  , 0  , 0  , 0  ), //      {u16}
  ROW(3, 1, 1, 0, 6  , 25 , 10 , 0  , 0  , 0  ), // #437 {r32, r32|m32|m, i8|u8}
  ROW(3, 0, 1, 0, 8  , 26 , 10 , 0  , 0  , 0  ), //      {r64, r64|m64|m, i8|u8}
  ROW(1, 1, 1, 0, 142, 0  , 0  , 0  , 0  , 0  ), // #439 {r16|m16|m|r32}
  ROW(1, 0, 1, 0, 143, 0  , 0  , 0  , 0  , 0  ), //      {r64|m16|m}
  ROW(1, 1, 0, 0, 144, 0  , 0  , 0  , 0  , 0  ), // #441 {ds:[m|memBase]}
  ROW(1, 0, 1, 0, 144, 0  , 0  , 0  , 0  , 0  ), //      {ds:[m|memBase]}
  ROW(4, 1, 1, 0, 55 , 55 , 56 , 55 , 0  , 0  ), // #443 {xmm, xmm, xmm|m128|m, xmm}
  ROW(4, 1, 1, 0, 57 , 57 , 58 , 57 , 0  , 0  ), //      {ymm, ymm, ymm|m256|m, ymm}
  ROW(2, 1, 1, 0, 55 , 145, 0  , 0  , 0  , 0  ), // #445 {xmm, xmm|m128|ymm|m256}
  ROW(2, 1, 1, 0, 57 , 62 , 0  , 0  , 0  , 0  ), //      {ymm, zmm|m512|m}
  ROW(2, 1, 1, 0, 6  , 124, 0  , 0  , 0  , 0  ), // #447 {r32, xmm|m16|m}
  ROW(2, 0, 1, 0, 8  , 124, 0  , 0  , 0  , 0  ), //      {r64, xmm|m16|m}
  ROW(3, 1, 1, 0, 55 , 55 , 25 , 0  , 0  , 0  ), // #449 {xmm, xmm, r32|m32|m}
  ROW(3, 0, 1, 0, 55 , 55 , 26 , 0  , 0  , 0  ), //      {xmm, xmm, r64|m64|m}
  ROW(3, 1, 1, 0, 55 , 55 , 13 , 0  , 0  , 0  ), // #451 {xmm, xmm, r32|m32}
  ROW(3, 0, 1, 0, 55 , 55 , 15 , 0  , 0  , 0  ), //      {xmm, xmm, r64|m64}
  ROW(4, 1, 1, 0, 55 , 55 , 55 , 70 , 0  , 0  ), // #453 {xmm, xmm, xmm, xmm|m64|m}
  ROW(4, 1, 1, 0, 55 , 55 , 31 , 55 , 0  , 0  ), //      {xmm, xmm, m64|m, xmm}
  ROW(4, 1, 1, 0, 55 , 55 , 55 , 122, 0  , 0  ), // #455 {xmm, xmm, xmm, xmm|m32|m}
  ROW(4, 1, 1, 0, 55 , 55 , 30 , 55 , 0  , 0  ), //      {xmm, xmm, m32|m, xmm}
  ROW(4, 1, 1, 0, 57 , 57 , 56 , 10 , 0  , 0  ), // #457 {ymm, ymm, xmm|m128|m, i8|u8}
  ROW(4, 1, 1, 0, 61 , 61 , 56 , 10 , 0  , 0  ), //      {zmm, zmm, xmm|m128|m, i8|u8}
  ROW(1, 1, 0, 1, 45 , 0  , 0  , 0  , 0  , 0  ), // #459 {<eax>}
  ROW(1, 0, 1, 1, 47 , 0  , 0  , 0  , 0  , 0  ), // #460 {<rax>}
  ROW(2, 1, 1, 0, 25 , 55 , 0  , 0  , 0  , 0  ), // #461 {r32|m32|m, xmm}
  ROW(2, 1, 1, 0, 55 , 25 , 0  , 0  , 0  , 0  ), //      {xmm, r32|m32|m}
  ROW(2, 1, 1, 0, 117, 55 , 0  , 0  , 0  , 0  ), // #463 {r32|m16|m, xmm}
  ROW(2, 1, 1, 0, 55 , 117, 0  , 0  , 0  , 0  ), //      {xmm, r32|m16|m}
  ROW(2, 1, 0, 0, 25 , 6  , 0  , 0  , 0  , 0  ), // #465 {r32|m32|m, r32}
  ROW(2, 0, 1, 0, 26 , 8  , 0  , 0  , 0  , 0  ), //      {r64|m64|m, r64}
  ROW(2, 1, 0, 0, 6  , 25 , 0  , 0  , 0  , 0  ), // #467 {r32, r32|m32|m}
  ROW(2, 0, 1, 0, 8  , 26 , 0  , 0  , 0  , 0  ), //      {r64, r64|m64|m}
  ROW(2, 1, 1, 0, 146, 70 , 0  , 0  , 0  , 0  ), // #469 {xmm|ymm|zmm, xmm|m64|m}
  ROW(2, 0, 1, 0, 146, 8  , 0  , 0  , 0  , 0  ), //      {xmm|ymm|zmm, r64}
  ROW(4, 1, 1, 0, 55 , 55 , 55 , 56 , 0  , 0  ), // #471 {xmm, xmm, xmm, xmm|m128|m}
  ROW(4, 1, 1, 0, 55 , 55 , 59 , 55 , 0  , 0  ), //      {xmm, xmm, m128|m, xmm}
  ROW(3, 1, 1, 0, 55 , 55 , 64 , 0  , 0  , 0  ), // #473 {xmm, xmm, xmm|m128|m|i8|u8}
  ROW(3, 1, 1, 0, 55 , 59 , 147, 0  , 0  , 0  ), //      {xmm, m128|m, i8|u8|xmm}
  ROW(2, 1, 1, 0, 77 , 102, 0  , 0  , 0  , 0  ), // #475 {vm32x, xmm|ymm}
  ROW(2, 1, 1, 0, 78 , 61 , 0  , 0  , 0  , 0  ), //      {vm32y, zmm}
  ROW(2, 1, 1, 0, 123, 55 , 0  , 0  , 0  , 0  ), // #477 {vm64x|vm64y, xmm}
  ROW(2, 1, 1, 0, 82 , 57 , 0  , 0  , 0  , 0  ), //      {vm64z, ymm}
  ROW(3, 1, 1, 0, 55 , 55 , 56 , 0  , 0  , 0  ), // #479 {xmm, xmm, xmm|m128|m}
  ROW(3, 1, 1, 0, 55 , 59 , 55 , 0  , 0  , 0  ), //      {xmm, m128|m, xmm}
  ROW(1, 1, 0, 1, 42 , 0  , 0  , 0  , 0  , 0  ), // #481 {<ax>}
  ROW(2, 1, 0, 1, 42 , 10 , 0  , 0  , 0  , 0  ), // #482 {<ax>, i8|u8}
  ROW(2, 1, 0, 0, 24 , 4  , 0  , 0  , 0  , 0  ), // #483 {r16|m16|m, r16}
  ROW(3, 1, 1, 1, 55 , 56 , 148, 0  , 0  , 0  ), // #484 {xmm, xmm|m128|m, <xmm0>}
  ROW(1, 1, 1, 1, 42 , 0  , 0  , 0  , 0  , 0  ), // #485 {<ax>}
  ROW(2, 1, 1, 2, 44 , 45 , 0  , 0  , 0  , 0  ), // #486 {<edx>, <eax>}
  ROW(1, 1, 1, 0, 149, 0  , 0  , 0  , 0  , 0  ), // #487 {m}
  ROW(1, 1, 1, 0, 31 , 0  , 0  , 0  , 0  , 0  ), // #488 {m64|m}
  ROW(0, 0, 1, 0, 0  , 0  , 0  , 0  , 0  , 0  ), // #489 {}
  ROW(1, 1, 1, 1, 150, 0  , 0  , 0  , 0  , 0  ), // #490 {<ds:[m|m512|memBase|zax]>}
  ROW(3, 1, 1, 0, 55 , 70 , 10 , 0  , 0  , 0  ), // #491 {xmm, xmm|m64|m, i8|u8}
  ROW(3, 1, 1, 0, 55 , 122, 10 , 0  , 0  , 0  ), // #492 {xmm, xmm|m32|m, i8|u8}
  ROW(5, 0, 1, 4, 59 , 46 , 47 , 151, 152, 0  ), // #493 {m128|m, <rdx>, <rax>, <rcx>, <rbx>}
  ROW(5, 1, 1, 4, 31 , 44 , 45 , 128, 153, 0  ), // #494 {m64|m, <edx>, <eax>, <ecx>, <ebx>}
  ROW(4, 1, 1, 4, 45 , 153, 128, 44 , 0  , 0  ), // #495 {<eax>, <ebx>, <ecx>, <edx>}
  ROW(2, 0, 1, 2, 46 , 47 , 0  , 0  , 0  , 0  ), // #496 {<rdx>, <rax>}
  ROW(2, 1, 1, 0, 67 , 56 , 0  , 0  , 0  , 0  ), // #497 {mm, xmm|m128|m}
  ROW(2, 1, 1, 0, 55 , 68 , 0  , 0  , 0  , 0  ), // #498 {xmm, mm|m64|m}
  ROW(2, 1, 1, 0, 67 , 70 , 0  , 0  , 0  , 0  ), // #499 {mm, xmm|m64|m}
  ROW(2, 1, 1, 2, 43 , 42 , 0  , 0  , 0  , 0  ), // #500 {<dx>, <ax>}
  ROW(1, 1, 1, 1, 45 , 0  , 0  , 0  , 0  , 0  ), // #501 {<eax>}
  ROW(2, 1, 1, 0, 12 , 10 , 0  , 0  , 0  , 0  ), // #502 {i16|u16, i8|u8}
  ROW(3, 1, 1, 0, 25 , 55 , 10 , 0  , 0  , 0  ), // #503 {r32|m32|m, xmm, i8|u8}
  ROW(1, 1, 1, 0, 115, 0  , 0  , 0  , 0  , 0  ), // #504 {m80|m}
  ROW(1, 1, 1, 0, 39 , 0  , 0  , 0  , 0  , 0  ), // #505 {m16|m32}
  ROW(1, 1, 1, 0, 154, 0  , 0  , 0  , 0  , 0  ), // #506 {m16|m32|m64}
  ROW(1, 1, 1, 0, 155, 0  , 0  , 0  , 0  , 0  ), // #507 {m32|m64|m80|st}
  ROW(1, 1, 1, 0, 21 , 0  , 0  , 0  , 0  , 0  ), // #508 {m16|m}
  ROW(1, 1, 1, 0, 156, 0  , 0  , 0  , 0  , 0  ), // #509 {ax|m16|m}
  ROW(1, 0, 1, 0, 149, 0  , 0  , 0  , 0  , 0  ), // #510 {m}
  ROW(2, 1, 1, 2, 45 , 153, 0  , 0  , 0  , 0  ), // #511 {<eax>, <ebx>}
  ROW(2, 1, 1, 1, 10 , 45 , 0  , 0  , 0  , 0  ), // #512 {i8|u8, <eax>}
  ROW(2, 1, 1, 0, 157, 158, 0  , 0  , 0  , 0  ), // #513 {al|ax|eax, i8|u8|dx}
  ROW(2, 1, 1, 0, 159, 160, 0  , 0  , 0  , 0  ), // #514 {es:[memBase|zdi|m8|m16|m32], dx}
  ROW(1, 1, 1, 0, 10 , 0  , 0  , 0  , 0  , 0  ), // #515 {i8|u8}
  ROW(0, 1, 0, 0, 0  , 0  , 0  , 0  , 0  , 0  ), // #516 {}
  ROW(3, 1, 1, 0, 92 , 92 , 92 , 0  , 0  , 0  ), // #517 {k, k, k}
  ROW(2, 1, 1, 0, 92 , 92 , 0  , 0  , 0  , 0  ), // #518 {k, k}
  ROW(3, 1, 1, 0, 92 , 92 , 10 , 0  , 0  , 0  ), // #519 {k, k, i8|u8}
  ROW(1, 1, 1, 1, 161, 0  , 0  , 0  , 0  , 0  ), // #520 {<ah>}
  ROW(1, 1, 1, 0, 30 , 0  , 0  , 0  , 0  , 0  ), // #521 {m32|m}
  ROW(1, 0, 1, 0, 63 , 0  , 0  , 0  , 0  , 0  ), // #522 {m512|m}
  ROW(1, 1, 1, 0, 24 , 0  , 0  , 0  , 0  , 0  ), // #523 {r16|m16|m}
  ROW(3, 1, 1, 1, 55 , 55 , 162, 0  , 0  , 0  ), // #524 {xmm, xmm, <ds:[m|m128|memBase|zdi]>}
  ROW(3, 1, 1, 1, 67 , 67 , 163, 0  , 0  , 0  ), // #525 {mm, mm, <ds:[m|m64|memBase|zdi]>}
  ROW(3, 1, 1, 3, 164, 128, 44 , 0  , 0  , 0  ), // #526 {<ds:[m|memBase|zax]>, <ecx>, <edx>}
  ROW(2, 1, 1, 0, 67 , 55 , 0  , 0  , 0  , 0  ), // #527 {mm, xmm}
  ROW(2, 1, 1, 0, 6  , 55 , 0  , 0  , 0  , 0  ), // #528 {r32, xmm}
  ROW(2, 1, 1, 0, 31 , 67 , 0  , 0  , 0  , 0  ), // #529 {m64|m, mm}
  ROW(2, 1, 1, 0, 55 , 67 , 0  , 0  , 0  , 0  ), // #530 {xmm, mm}
  ROW(2, 1, 1, 2, 45 , 128, 0  , 0  , 0  , 0  ), // #531 {<eax>, <ecx>}
  ROW(3, 1, 1, 3, 45 , 128, 153, 0  , 0  , 0  ), // #532 {<eax>, <ecx>, <ebx>}
  ROW(2, 1, 1, 0, 158, 157, 0  , 0  , 0  , 0  ), // #533 {i8|u8|dx, al|ax|eax}
  ROW(2, 1, 1, 0, 160, 165, 0  , 0  , 0  , 0  ), // #534 {dx, ds:[memBase|zsi|m8|m16|m32]}
  ROW(6, 1, 1, 3, 55 , 56 , 10 , 128, 45 , 44 ), // #535 {xmm, xmm|m128|m, i8|u8, <ecx>, <eax>, <edx>}
  ROW(6, 1, 1, 3, 55 , 56 , 10 , 148, 45 , 44 ), // #536 {xmm, xmm|m128|m, i8|u8, <xmm0>, <eax>, <edx>}
  ROW(4, 1, 1, 1, 55 , 56 , 10 , 128, 0  , 0  ), // #537 {xmm, xmm|m128|m, i8|u8, <ecx>}
  ROW(4, 1, 1, 1, 55 , 56 , 10 , 148, 0  , 0  ), // #538 {xmm, xmm|m128|m, i8|u8, <xmm0>}
  ROW(3, 1, 1, 0, 134, 55 , 10 , 0  , 0  , 0  ), // #539 {r32|m8|m, xmm, i8|u8}
  ROW(3, 0, 1, 0, 26 , 55 , 10 , 0  , 0  , 0  ), // #540 {r64|m64|m, xmm, i8|u8}
  ROW(3, 1, 1, 0, 55 , 134, 10 , 0  , 0  , 0  ), // #541 {xmm, r32|m8|m, i8|u8}
  ROW(3, 1, 1, 0, 55 , 25 , 10 , 0  , 0  , 0  ), // #542 {xmm, r32|m32|m, i8|u8}
  ROW(3, 0, 1, 0, 55 , 26 , 10 , 0  , 0  , 0  ), // #543 {xmm, r64|m64|m, i8|u8}
  ROW(3, 1, 1, 0, 69 , 117, 10 , 0  , 0  , 0  ), // #544 {mm|xmm, r32|m16|m, i8|u8}
  ROW(2, 1, 1, 0, 6  , 69 , 0  , 0  , 0  , 0  ), // #545 {r32, mm|xmm}
  ROW(2, 1, 1, 0, 55 , 10 , 0  , 0  , 0  , 0  ), // #546 {xmm, i8|u8}
  ROW(1, 1, 1, 0, 12 , 0  , 0  , 0  , 0  , 0  ), // #547 {i16|u16}
  ROW(1, 0, 1, 0, 138, 0  , 0  , 0  , 0  , 0  ), // #548 {r32|r64}
  ROW(1, 1, 1, 0, 1  , 0  , 0  , 0  , 0  , 0  ), // #549 {r8lo|r8hi|m8|m}
  ROW(3, 0, 1, 0, 166, 166, 166, 0  , 0  , 0  ), // #550 {tmm, tmm, tmm}
  ROW(2, 0, 1, 0, 166, 167, 0  , 0  , 0  , 0  ), // #551 {tmm, tmem}
  ROW(2, 0, 1, 0, 167, 166, 0  , 0  , 0  , 0  ), // #552 {tmem, tmm}
  ROW(1, 0, 1, 0, 166, 0  , 0  , 0  , 0  , 0  ), // #553 {tmm}
  ROW(3, 1, 1, 2, 6  , 44 , 45 , 0  , 0  , 0  ), // #554 {r32, <edx>, <eax>}
  ROW(3, 1, 1, 0, 55 , 55 , 70 , 0  , 0  , 0  ), // #555 {xmm, xmm, xmm|m64|m}
  ROW(3, 1, 1, 0, 55 , 55 , 124, 0  , 0  , 0  ), // #556 {xmm, xmm, xmm|m16|m}
  ROW(3, 1, 1, 0, 55 , 55 , 122, 0  , 0  , 0  ), // #557 {xmm, xmm, xmm|m32|m}
  ROW(2, 1, 1, 0, 102, 21 , 0  , 0  , 0  , 0  ), // #558 {xmm|ymm, m16|m}
  ROW(2, 1, 1, 0, 57 , 59 , 0  , 0  , 0  , 0  ), // #559 {ymm, m128|m}
  ROW(2, 1, 1, 0, 168, 70 , 0  , 0  , 0  , 0  ), // #560 {ymm|zmm, xmm|m64|m}
  ROW(2, 1, 1, 0, 168, 59 , 0  , 0  , 0  , 0  ), // #561 {ymm|zmm, m128|m}
  ROW(2, 1, 1, 0, 61 , 60 , 0  , 0  , 0  , 0  ), // #562 {zmm, m256|m}
  ROW(2, 1, 1, 0, 146, 122, 0  , 0  , 0  , 0  ), // #563 {xmm|ymm|zmm, m32|m|xmm}
  ROW(4, 1, 1, 0, 120, 55 , 70 , 10 , 0  , 0  ), // #564 {xmm|k, xmm, xmm|m64|m, i8|u8}
  ROW(4, 1, 1, 0, 92 , 55 , 124, 10 , 0  , 0  ), // #565 {k, xmm, xmm|m16|m, i8|u8}
  ROW(4, 1, 1, 0, 120, 55 , 122, 10 , 0  , 0  ), // #566 {xmm|k, xmm, xmm|m32|m, i8|u8}
  ROW(2, 1, 1, 0, 55 , 169, 0  , 0  , 0  , 0  ), // #567 {xmm, xmm|m128|ymm|m256|zmm|m512}
  ROW(3, 1, 1, 0, 56 , 168, 10 , 0  , 0  , 0  ), // #568 {xmm|m128|m, ymm|zmm, i8|u8}
  ROW(4, 1, 1, 0, 55 , 55 , 70 , 10 , 0  , 0  ), // #569 {xmm, xmm, xmm|m64|m, i8|u8}
  ROW(4, 1, 1, 0, 55 , 55 , 122, 10 , 0  , 0  ), // #570 {xmm, xmm, xmm|m32|m, i8|u8}
  ROW(3, 1, 1, 0, 92 , 169, 10 , 0  , 0  , 0  ), // #571 {k, xmm|m128|ymm|m256|zmm|m512, i8|u8}
  ROW(3, 1, 1, 0, 92 , 70 , 10 , 0  , 0  , 0  ), // #572 {k, xmm|m64|m, i8|u8}
  ROW(3, 1, 1, 0, 92 , 124, 10 , 0  , 0  , 0  ), // #573 {k, xmm|m16|m, i8|u8}
  ROW(3, 1, 1, 0, 92 , 122, 10 , 0  , 0  , 0  ), // #574 {k, xmm|m32|m, i8|u8}
  ROW(4, 1, 1, 0, 55 , 55 , 124, 10 , 0  , 0  ), // #575 {xmm, xmm, xmm|m16|m, i8|u8}
  ROW(4, 1, 1, 0, 61 , 61 , 58 , 10 , 0  , 0  ), // #576 {zmm, zmm, ymm|m256|m, i8|u8}
  ROW(2, 1, 1, 0, 6  , 102, 0  , 0  , 0  , 0  ), // #577 {r32, xmm|ymm}
  ROW(2, 1, 1, 0, 146, 170, 0  , 0  , 0  , 0  ), // #578 {xmm|ymm|zmm, xmm|m8|m|r32}
  ROW(2, 1, 1, 0, 146, 171, 0  , 0  , 0  , 0  ), // #579 {xmm|ymm|zmm, xmm|m32|m|r32}
  ROW(2, 1, 1, 0, 146, 92 , 0  , 0  , 0  , 0  ), // #580 {xmm|ymm|zmm, k}
  ROW(2, 1, 1, 0, 146, 172, 0  , 0  , 0  , 0  ), // #581 {xmm|ymm|zmm, xmm|m16|m|r32}
  ROW(3, 1, 1, 0, 117, 55 , 10 , 0  , 0  , 0  ), // #582 {r32|m16|m, xmm, i8|u8}
  ROW(4, 1, 1, 0, 55 , 55 , 134, 10 , 0  , 0  ), // #583 {xmm, xmm, r32|m8|m, i8|u8}
  ROW(4, 1, 1, 0, 55 , 55 , 25 , 10 , 0  , 0  ), // #584 {xmm, xmm, r32|m32|m, i8|u8}
  ROW(4, 0, 1, 0, 55 , 55 , 26 , 10 , 0  , 0  ), // #585 {xmm, xmm, r64|m64|m, i8|u8}
  ROW(4, 1, 1, 0, 55 , 55 , 117, 10 , 0  , 0  ), // #586 {xmm, xmm, r32|m16|m, i8|u8}
  ROW(2, 1, 1, 0, 92 , 146, 0  , 0  , 0  , 0  ), // #587 {k, xmm|ymm|zmm}
  ROW(2, 1, 1, 0, 57 , 55 , 0  , 0  , 0  , 0  ), // #588 {ymm, xmm}
  ROW(2, 1, 1, 0, 57 , 57 , 0  , 0  , 0  , 0  ), // #589 {ymm, ymm}
  ROW(3, 1, 1, 0, 57 , 57 , 55 , 0  , 0  , 0  ), // #590 {ymm, ymm, xmm}
  ROW(3, 1, 1, 2, 149, 44 , 45 , 0  , 0  , 0  ), // #591 {m, <edx>, <eax>}
  ROW(3, 0, 1, 2, 149, 44 , 45 , 0  , 0  , 0  )  // #592 {m, <edx>, <eax>}
};
#undef ROW

#define ROW(op_flags, reg_id) { op_flags, uint8_t(reg_id) }
#define F(VAL) uint64_t(InstDB::OpFlags::k##VAL)
const InstDB::OpSignature InstDB::_op_signature_table[] = {
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
  ROW(F(MemUnspecified) | F(Mem8), 0x00),
  ROW(F(RegSReg) | F(MemUnspecified) | F(Mem16), 0x00),
  ROW(F(RegSReg) | F(MemUnspecified) | F(Mem32), 0x00),
  ROW(F(RegSReg) | F(RegCReg) | F(RegDReg) | F(MemUnspecified) | F(Mem64) | F(ImmI64) | F(ImmU64), 0x00),
  ROW(F(MemUnspecified) | F(Mem16), 0x00),
  ROW(F(RegSReg), 0x00),
  ROW(F(RegCReg) | F(RegDReg), 0x00),
  ROW(F(RegGpw) | F(MemUnspecified) | F(Mem16), 0x00),
  ROW(F(RegGpd) | F(MemUnspecified) | F(Mem32), 0x00),
  ROW(F(RegGpq) | F(MemUnspecified) | F(Mem64), 0x00),
  ROW(F(RegGpw) | F(RegGpd) | F(Mem16) | F(Mem32), 0x00),
  ROW(F(ImmI8), 0x00),
  ROW(F(ImmI8) | F(ImmI32), 0x00),
  ROW(F(MemUnspecified) | F(Mem32), 0x00),
  ROW(F(MemUnspecified) | F(Mem64), 0x00),
  ROW(F(RegGpq) | F(MemUnspecified) | F(Mem64) | F(ImmI8) | F(ImmI32) | F(ImmU32), 0x00),
  ROW(F(Mem64), 0x00),
  ROW(F(MemUnspecified) | F(Mem8) | F(ImmI8) | F(ImmU8), 0x00),
  ROW(F(MemUnspecified) | F(Mem16) | F(ImmI8) | F(ImmI16) | F(ImmU16), 0x00),
  ROW(F(MemUnspecified) | F(Mem32) | F(ImmI8) | F(ImmI32) | F(ImmU32), 0x00),
  ROW(F(MemUnspecified) | F(Mem64) | F(ImmI8) | F(ImmI32), 0x00),
  ROW(F(Mem8), 0x00),
  ROW(F(Mem16) | F(Mem32), 0x00),
  ROW(F(Mem16), 0x00),
  ROW(F(Mem32), 0x00),
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
  ROW(F(RegYmm), 0x00),
  ROW(F(RegYmm) | F(MemUnspecified) | F(Mem256), 0x00),
  ROW(F(MemUnspecified) | F(Mem128), 0x00),
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
  ROW(F(RegGpw) | F(Mem16) | F(ImmI8) | F(ImmU8) | F(ImmI16) | F(ImmU16), 0x00),
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
  ROW(F(RegGpbLo) | F(RegGpbHi) | F(RegGpw) | F(RegGpd) | F(Mem8) | F(Mem16) | F(Mem32), 0x00),
  ROW(F(Mem32) | F(Mem64), 0x00),
  ROW(F(RegSt), 0x01),
  ROW(F(RegSt), 0x00),
  ROW(F(ImmI32) | F(ImmI64) | F(Rel8) | F(Rel32), 0x00),
  ROW(F(RegGpd) | F(Mem32) | F(ImmI32) | F(ImmI64) | F(Rel32), 0x00),
  ROW(F(ImmI16) | F(ImmU16) | F(ImmI32) | F(ImmU32), 0x00),
  ROW(F(MemUnspecified) | F(Mem32) | F(Mem48), 0x00),
  ROW(F(MemUnspecified) | F(Mem80), 0x00),
  ROW(F(MemUnspecified) | F(Mem48), 0x00),
  ROW(F(RegGpd) | F(MemUnspecified) | F(Mem16), 0x00),
  ROW(F(RegGpbLo) | F(RegGpbHi) | F(RegGpw) | F(Mem8) | F(Mem16), 0x00),
  ROW(F(RegGpbLo) | F(ImmI8) | F(ImmU8), 0x02),
  ROW(F(RegXmm) | F(RegKReg), 0x00),
  ROW(F(RegYmm) | F(RegKReg), 0x00),
  ROW(F(RegXmm) | F(MemUnspecified) | F(Mem32), 0x00),
  ROW(F(Vm64x) | F(Vm64y), 0x00),
  ROW(F(RegXmm) | F(MemUnspecified) | F(Mem16), 0x00),
  ROW(F(RegGpbLo) | F(RegGpbHi) | F(RegGpq) | F(Mem8) | F(Mem64), 0x00),
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
  ROW(F(MemUnspecified), 0x00),
  ROW(F(MemUnspecified) | F(Mem512) | F(FlagMemBase) | F(FlagMemDs) | F(FlagImplicit), 0x01),
  ROW(F(RegGpq) | F(FlagImplicit), 0x02),
  ROW(F(RegGpq) | F(FlagImplicit), 0x08),
  ROW(F(RegGpd) | F(FlagImplicit), 0x08),
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
#endif // !ASMJIT_NO_INTROSPECTION

// x86::InstInternal - QueryRWInfo
// ===============================

// ${InstRWInfoTable:Begin}
// ------------------- Automatically generated, do not edit -------------------
const uint8_t InstDB::rw_info_index_a_table[Inst::_kIdCount] = {
  0, 0, 1, 2, 1, 2, 0, 3, 4, 3, 5, 5, 6, 7, 5, 5, 4, 5, 5, 5, 5, 8, 0, 3, 0, 5,
  5, 5, 5, 2, 9, 2, 0, 10, 10, 10, 10, 10, 0, 0, 0, 0, 10, 10, 10, 10, 10, 11, 10,
  10, 0, 12, 13, 13, 13, 0, 0, 0, 14, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 15, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 0, 0, 0, 0, 0, 0, 0, 17, 18, 0, 19, 20, 21, 8,
  22, 22, 22, 21, 23, 8, 21, 24, 25, 26, 27, 28, 29, 30, 22, 22, 8, 24, 25, 30,
  31, 0, 0, 0, 0, 32, 5, 5, 6, 7, 0, 0, 0, 0, 0, 33, 33, 0, 0, 34, 0, 0, 35, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 35, 0, 35, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 35, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 35, 0, 35,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 36, 0, 0, 5, 5, 5, 0, 37,
  5, 5, 32, 38, 39, 0, 0, 0, 40, 0, 34, 0, 0, 0, 0, 41, 0, 42, 0, 41, 41, 0, 0,
  0, 0, 0, 43, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 44, 45, 46, 47, 48, 49, 50, 51, 0, 0, 0, 52, 53, 54, 55, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 52, 53, 54, 55, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 56,
  0, 57, 0, 2, 0, 58, 0, 2, 0, 2, 0, 2, 0, 0, 0, 0, 0, 59, 60, 60, 60, 56, 2,
  0, 0, 0, 10, 0, 0, 5, 5, 6, 7, 0, 0, 5, 5, 6, 7, 0, 0, 61, 62, 63, 63, 64, 45,
  21, 33, 64, 50, 63, 63, 65, 66, 66, 67, 68, 68, 69, 69, 57, 57, 64, 57, 57, 68,
  68, 70, 46, 50, 71, 72, 8, 8, 73, 74, 10, 63, 63, 74, 0, 32, 5, 5, 6, 7, 0,
  75, 0, 0, 76, 0, 3, 5, 5, 77, 78, 10, 10, 10, 4, 4, 5, 4, 4, 4, 4, 4, 4, 4, 4,
  4, 0, 4, 4, 0, 4, 79, 4, 0, 0, 0, 4, 4, 5, 4, 0, 0, 4, 4, 5, 4, 0, 0, 0, 0,
  0, 0, 0, 0, 80, 24, 24, 79, 79, 79, 79, 79, 79, 79, 79, 79, 79, 24, 79, 79, 79,
  24, 24, 79, 79, 79, 4, 4, 4, 81, 4, 4, 4, 24, 24, 0, 0, 0, 0, 4, 4, 5, 5, 4,
  4, 5, 5, 5, 5, 4, 4, 5, 5, 82, 83, 84, 21, 21, 21, 83, 83, 84, 21, 21, 21, 83,
  5, 4, 79, 4, 4, 5, 4, 4, 0, 0, 0, 10, 0, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  4, 4, 0, 0, 0, 0, 4, 4, 4, 4, 85, 4, 4, 0, 4, 4, 4, 85, 4, 4, 4, 4, 4, 4, 4,
  4, 4, 4, 24, 86, 0, 4, 4, 5, 4, 87, 87, 5, 87, 0, 0, 0, 0, 0, 0, 0, 0, 4, 88,
  8, 89, 88, 0, 0, 90, 0, 0, 0, 0, 0, 0, 0, 0, 91, 0, 0, 0, 0, 0, 88, 88, 0, 0,
  0, 0, 0, 0, 8, 89, 0, 0, 88, 0, 0, 3, 92, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 8, 8, 8, 0, 8, 8, 0, 88, 0, 0, 88, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 8, 8, 23, 89, 0, 0, 0, 0, 0, 0, 93, 0, 0, 0, 3, 5, 5, 6, 7,
  0, 0, 0, 0, 0, 0, 0, 10, 0, 0, 0, 0, 0, 0, 0, 0, 0, 12, 0, 94, 94, 0, 95, 0,
  0, 0, 10, 10, 17, 18, 96, 96, 0, 0, 0, 0, 5, 5, 5, 5, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 8, 0, 0, 0, 0, 0, 0, 0, 97, 97, 0, 0, 0, 0, 0, 0, 98, 25, 99, 100,
  99, 100, 98, 25, 99, 100, 99, 100, 101, 102, 0, 0, 0, 0, 0, 0, 17, 103, 18,
  104, 104, 105, 106, 10, 0, 64, 64, 64, 64, 106, 106, 107, 106, 10, 106, 10, 105,
  108, 105, 105, 108, 105, 108, 10, 10, 10, 105, 0, 106, 105, 10, 105, 10, 109,
  106, 0, 25, 0, 25, 0, 110, 0, 110, 0, 0, 0, 0, 0, 30, 30, 106, 10, 106, 10,
  105, 108, 105, 108, 10, 10, 10, 105, 10, 105, 25, 25, 110, 110, 30, 30, 105,
  106, 10, 10, 107, 106, 0, 0, 0, 10, 10, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10,
  10, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10, 10, 24, 111, 2, 2, 2, 112, 10, 10, 10, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  64, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 113,
  113, 45, 114, 113, 113, 113, 113, 113, 113, 113, 113, 0, 115, 115, 0, 68, 68,
  116, 117, 64, 64, 64, 64, 118, 68, 119, 10, 10, 70, 113, 113, 47, 0, 0, 0, 104,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 120, 0, 0, 0, 0, 0, 0, 10, 10, 10, 10, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 121,
  30, 122, 122, 25, 110, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 104, 104, 104, 104, 0, 0, 0, 0, 0, 0, 10, 10, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 10, 10, 10, 10, 0, 0, 0, 0, 2, 2, 112, 2, 8, 8, 8,
  0, 8, 0, 8, 8, 8, 8, 8, 8, 0, 8, 8, 81, 8, 0, 8, 0, 0, 8, 0, 0, 0, 0, 10, 10,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 123, 123, 124, 125, 122, 122, 122, 122, 82, 123, 126,
  125, 124, 124, 125, 126, 125, 124, 125, 108, 127, 105, 105, 105, 108, 124, 125,
  126, 125, 124, 125, 123, 125, 108, 127, 105, 105, 105, 108, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 10, 10, 10, 10, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  64, 64, 128, 64, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 120, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10, 10, 0, 0, 10, 10, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10, 10, 0, 0, 10, 10, 0, 0, 0, 0, 0, 0, 0, 0, 64,
  64, 64, 128, 129, 130, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10, 10, 10, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 120, 120, 17, 103, 18, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 70, 68, 70, 68, 0, 131, 0, 132, 0, 0, 0, 3, 5, 5, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0
};

const uint8_t InstDB::rw_info_index_b_table[Inst::_kIdCount] = {
  0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 3, 0,
  0, 0, 0, 0, 0, 0, 4, 0, 0, 0, 0, 0, 5, 5, 6, 6, 0, 0, 0, 0, 0, 0, 0, 0, 7, 0,
  0, 0, 0, 4, 8, 1, 0, 9, 0, 0, 0, 10, 10, 10, 0, 0, 11, 0, 0, 10, 12, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 13, 13, 13, 13, 13, 13, 13, 13, 13,
  13, 13, 13, 13, 5, 5, 13, 0, 14, 15, 13, 16, 17, 18, 13, 0, 0, 19, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 20, 1, 1, 21,
  22, 0, 0, 0, 0, 5, 5, 0, 0, 0, 0, 0, 0, 23, 24, 0, 0, 25, 26, 27, 28, 0, 0,
  26, 26, 26, 26, 26, 26, 26, 26, 29, 30, 30, 29, 0, 0, 0, 25, 26, 25, 26, 0, 26,
  25, 25, 25, 25, 25, 25, 25, 0, 0, 31, 31, 31, 25, 25, 29, 0, 32, 10, 0, 0,
  0, 0, 0, 0, 25, 26, 0, 0, 0, 33, 34, 33, 35, 0, 0, 0, 0, 0, 10, 33, 0, 0, 0, 0,
  36, 34, 33, 36, 35, 25, 26, 25, 26, 0, 30, 30, 30, 30, 0, 0, 0, 26, 10, 10,
  33, 33, 0, 0, 0, 0, 5, 5, 0, 0, 0, 0, 0, 0, 0, 22, 37, 0, 21, 38, 38, 0, 39, 40,
  0, 0, 0, 0, 0, 10, 0, 41, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 8, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 42, 43, 44, 45, 42, 43, 42, 43, 44, 45, 44, 45, 0, 0, 0, 0,
  0, 0, 0, 0, 42, 43, 44, 0, 0, 0, 0, 45, 46, 47, 48, 49, 46, 47, 48, 49, 0, 0,
  0, 0, 50, 51, 52, 42, 43, 44, 45, 42, 43, 44, 45, 53, 0, 25, 0, 54, 0, 55, 0,
  0, 0, 0, 0, 10, 0, 10, 25, 56, 57, 56, 0, 0, 0, 0, 0, 0, 56, 58, 58, 0, 59,
  60, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 61, 61, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 62, 0, 0, 62, 0, 0, 0,
  0, 0, 5, 63, 0, 0, 0, 0, 64, 0, 65, 21, 66, 21, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 67, 0, 0, 0, 0, 0, 0, 6, 5, 5, 0, 0, 0, 0, 68,
  69, 0, 0, 0, 0, 70, 71, 0, 3, 3, 72, 23, 73, 74, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 75, 39, 76, 77,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 78, 0, 0, 0, 0, 0, 0, 0, 10, 10, 10, 10, 10, 10, 10,
  10, 10, 0, 0, 2, 2, 2, 79, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 66, 0, 0, 0, 0, 0, 0, 0, 0, 66, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 80, 80, 81, 80, 81, 81, 81, 80, 80, 82, 83, 0, 84, 0, 0, 0, 0, 0,
  0, 85, 2, 2, 86, 87, 0, 0, 0, 11, 88, 0, 4, 0, 0, 0, 0, 0, 0, 89, 0, 90, 90,
  90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 0, 90, 0, 33, 0, 0, 0, 2,
  0, 0, 91, 0, 92, 4, 0, 92, 4, 5, 5, 33, 20, 93, 80, 93, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 94, 0, 93, 95, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 96, 96, 0, 96, 96,
  96, 96, 96, 96, 0, 0, 0, 0, 0, 0, 97, 0, 98, 0, 0, 0, 0, 0, 0, 0, 0, 10, 98,
  0, 0, 0, 0, 3, 3, 3, 99, 100, 101, 3, 3, 3, 3, 3, 3, 0, 2, 3, 3, 3, 3, 3, 3,
  0, 0, 3, 3, 3, 3, 102, 102, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 103,
  3, 104, 105, 106, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 107, 0, 0, 0, 0, 0, 0, 0, 99, 0, 108, 0, 100,
  0, 109, 0, 110, 111, 112, 113, 114, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 110, 111, 112, 0, 0, 3, 3, 3, 3, 99,
  100, 101, 3, 115, 3, 56, 56, 0, 0, 116, 117, 118, 117, 118, 116, 117, 118,
  117, 118, 23, 119, 120, 119, 120, 121, 121, 122, 123, 121, 121, 121, 124, 125,
  126, 121, 121, 121, 124, 125, 126, 121, 121, 121, 124, 125, 126, 119, 120, 127,
  127, 128, 129, 121, 121, 121, 121, 121, 121, 121, 121, 121, 127, 127, 121,
  121, 121, 124, 130, 126, 121, 121, 121, 124, 130, 126, 121, 121, 121, 124, 130,
  126, 121, 121, 121, 121, 121, 121, 121, 121, 121, 127, 127, 127, 127, 128, 129,
  119, 131, 121, 121, 121, 124, 125, 126, 121, 121, 121, 124, 125, 126, 121,
  121, 121, 124, 125, 126, 127, 127, 128, 129, 121, 121, 121, 124, 130, 126, 121,
  121, 121, 124, 130, 126, 121, 121, 121, 132, 130, 133, 127, 127, 128, 129,
  134, 134, 134, 79, 135, 136, 0, 0, 0, 0, 137, 138, 138, 139, 0, 0, 0, 140, 141,
  142, 85, 85, 85, 140, 141, 142, 3, 3, 3, 3, 3, 3, 3, 143, 144, 145, 144, 145,
  143, 144, 145, 144, 145, 101, 0, 54, 59, 146, 146, 3, 3, 3, 99, 100, 101, 0,
  11, 0, 0, 3, 3, 3, 99, 100, 101, 0, 147, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 148, 149, 149, 150, 151, 151, 0, 0, 0, 0, 0, 0, 0, 152, 153, 0, 0, 154, 0,
  0, 0, 3, 11, 155, 0, 0, 156, 147, 3, 3, 3, 99, 100, 101, 0, 0, 11, 3, 3, 157,
  157, 0, 0, 0, 0, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
  3, 3, 3, 3, 3, 102, 3, 0, 0, 0, 0, 0, 0, 3, 127, 103, 103, 3, 3, 3, 3, 68, 69,
  3, 3, 3, 3, 70, 71, 103, 103, 103, 103, 103, 103, 115, 115, 0, 0, 0, 0, 115,
  115, 115, 115, 115, 115, 0, 0, 121, 121, 121, 121, 121, 121, 121, 121, 121,
  121, 121, 121, 121, 121, 121, 121, 158, 158, 3, 3, 121, 121, 3, 3, 121, 121, 127,
  127, 159, 159, 159, 3, 159, 121, 121, 121, 121, 121, 121, 3, 0, 0, 0, 0, 72,
  23, 73, 160, 138, 137, 139, 138, 0, 0, 0, 3, 0, 3, 0, 0, 0, 0, 0, 0, 3, 0,
  0, 0, 0, 3, 0, 3, 3, 0, 161, 101, 99, 100, 0, 0, 162, 162, 162, 162, 162, 162,
  162, 162, 162, 162, 162, 162, 121, 121, 3, 3, 146, 146, 3, 3, 3, 3, 3, 3, 3,
  3, 3, 3, 3, 3, 3, 3, 3, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 3, 3, 3,
  3, 3, 3, 3, 3, 0, 0, 0, 0, 3, 3, 3, 163, 85, 85, 3, 3, 85, 85, 3, 3, 164, 164,
  164, 164, 3, 0, 0, 0, 0, 164, 164, 164, 164, 164, 164, 3, 3, 121, 121, 121, 3,
  164, 164, 3, 3, 121, 121, 121, 3, 3, 103, 85, 85, 85, 3, 3, 3, 165, 166, 165,
  3, 3, 3, 167, 165, 168, 3, 3, 3, 167, 165, 166, 165, 3, 3, 3, 167, 3, 3, 3,
  3, 3, 3, 3, 3, 169, 169, 0, 103, 103, 103, 103, 103, 103, 103, 103, 3, 3, 3, 3,
  3, 3, 3, 3, 3, 3, 3, 3, 3, 140, 142, 0, 0, 140, 142, 0, 0, 141, 142, 85, 85,
  85, 140, 141, 142, 85, 85, 85, 140, 141, 142, 85, 85, 140, 142, 0, 0, 140, 142,
  0, 0, 141, 142, 3, 3, 3, 99, 100, 101, 0, 0, 0, 0, 0, 0, 170, 3, 3, 3, 3, 3,
  3, 171, 171, 171, 3, 3, 0, 0, 0, 140, 141, 142, 94, 3, 3, 3, 99, 100, 101, 0,
  0, 0, 0, 0, 3, 3, 3, 3, 3, 3, 0, 0, 0, 0, 57, 57, 172, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 81, 0, 0, 0, 0, 0, 173, 173, 173, 173, 174, 174, 174, 174, 174, 174, 174,
  174, 172, 0, 0
};

const InstDB::RWInfo InstDB::rw_info_a_table[] = {
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 0 , 0 , 0 , 0 , 0 , 0  } }, // #0 [ref=999x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 1 , 0 , 0 , 0 , 0 , 0  } }, // #1 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 2 , 3 , 0 , 0 , 0 , 0  } }, // #2 [ref=15x]
  { InstDB::RWInfo::kCategoryGeneric   , 1 , { 2 , 3 , 0 , 0 , 0 , 0  } }, // #3 [ref=7x]
  { InstDB::RWInfo::kCategoryGeneric   , 2 , { 2 , 3 , 0 , 0 , 0 , 0  } }, // #4 [ref=82x]
  { InstDB::RWInfo::kCategoryGeneric   , 3 , { 4 , 5 , 0 , 0 , 0 , 0  } }, // #5 [ref=50x]
  { InstDB::RWInfo::kCategoryGeneric   , 4 , { 6 , 7 , 0 , 0 , 0 , 0  } }, // #6 [ref=6x]
  { InstDB::RWInfo::kCategoryGeneric   , 5 , { 8 , 9 , 0 , 0 , 0 , 0  } }, // #7 [ref=6x]
  { InstDB::RWInfo::kCategoryGeneric   , 3 , { 10, 5 , 0 , 0 , 0 , 0  } }, // #8 [ref=31x]
  { InstDB::RWInfo::kCategoryGeneric   , 7 , { 12, 13, 0 , 0 , 0 , 0  } }, // #9 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 2 , { 11, 3 , 0 , 0 , 0 , 0  } }, // #10 [ref=75x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 3 , 3 , 0 , 0 , 0 , 0  } }, // #11 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 8 , { 3 , 3 , 0 , 0 , 0 , 0  } }, // #12 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 8 , { 2 , 3 , 0 , 0 , 0 , 0  } }, // #13 [ref=3x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 15, 16, 0 , 0 , 0 , 0  } }, // #14 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 1 , { 3 , 3 , 0 , 0 , 0 , 0  } }, // #15 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 13, { 19, 20, 0 , 0 , 0 , 0  } }, // #16 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 4 , { 7 , 7 , 0 , 0 , 0 , 0  } }, // #17 [ref=4x]
  { InstDB::RWInfo::kCategoryGeneric   , 5 , { 9 , 9 , 0 , 0 , 0 , 0  } }, // #18 [ref=4x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 32, 33, 0 , 0 , 0 , 0  } }, // #19 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 15, { 2 , 3 , 0 , 0 , 0 , 0  } }, // #20 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 4 , { 10, 7 , 0 , 0 , 0 , 0  } }, // #21 [ref=10x]
  { InstDB::RWInfo::kCategoryGeneric   , 3 , { 34, 5 , 0 , 0 , 0 , 0  } }, // #22 [ref=5x]
  { InstDB::RWInfo::kCategoryGeneric   , 4 , { 35, 7 , 0 , 0 , 0 , 0  } }, // #23 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 4 , { 34, 7 , 0 , 0 , 0 , 0  } }, // #24 [ref=11x]
  { InstDB::RWInfo::kCategoryGeneric   , 4 , { 11, 7 , 0 , 0 , 0 , 0  } }, // #25 [ref=9x]
  { InstDB::RWInfo::kCategoryGeneric   , 4 , { 36, 7 , 0 , 0 , 0 , 0  } }, // #26 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 15, { 35, 3 , 0 , 0 , 0 , 0  } }, // #27 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 15, { 36, 3 , 0 , 0 , 0 , 0  } }, // #28 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 5 , { 35, 9 , 0 , 0 , 0 , 0  } }, // #29 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 5 , { 11, 9 , 0 , 0 , 0 , 0  } }, // #30 [ref=7x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 37, 38, 0 , 0 , 0 , 0  } }, // #31 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 16, { 1 , 39, 0 , 0 , 0 , 0  } }, // #32 [ref=3x]
  { InstDB::RWInfo::kCategoryGeneric   , 12, { 42, 43, 0 , 0 , 0 , 0  } }, // #33 [ref=3x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 4 , 5 , 0 , 0 , 0 , 0  } }, // #34 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 45, 46, 0 , 0 , 0 , 0  } }, // #35 [ref=6x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 52, 29, 0 , 0 , 0 , 0  } }, // #36 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 0 , 52, 0 , 0 , 0 , 0  } }, // #37 [ref=1x]
  { InstDB::RWInfo::kCategoryImul      , 2 , { 0 , 0 , 0 , 0 , 0 , 0  } }, // #38 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 53, 54, 0 , 0 , 0 , 0  } }, // #39 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 13, { 55, 54, 0 , 0 , 0 , 0  } }, // #40 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 14, { 3 , 5 , 0 , 0 , 0 , 0  } }, // #41 [ref=3x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 21, 28, 0 , 0 , 0 , 0  } }, // #42 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 57, 0 , 0 , 0 , 0 , 0  } }, // #43 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 22, { 58, 39, 0 , 0 , 0 , 0  } }, // #44 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 23, { 44, 9 , 0 , 0 , 0 , 0  } }, // #45 [ref=3x]
  { InstDB::RWInfo::kCategoryGeneric   , 24, { 34, 7 , 0 , 0 , 0 , 0  } }, // #46 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 25, { 50, 13, 0 , 0 , 0 , 0  } }, // #47 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 58, 39, 0 , 0 , 0 , 0  } }, // #48 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 44, 9 , 0 , 0 , 0 , 0  } }, // #49 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 34, 7 , 0 , 0 , 0 , 0  } }, // #50 [ref=3x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 50, 13, 0 , 0 , 0 , 0  } }, // #51 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 39, 39, 0 , 0 , 0 , 0  } }, // #52 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 9 , 9 , 0 , 0 , 0 , 0  } }, // #53 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 7 , 7 , 0 , 0 , 0 , 0  } }, // #54 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 13, 13, 0 , 0 , 0 , 0  } }, // #55 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 26, { 11, 3 , 0 , 0 , 0 , 0  } }, // #56 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 14, { 10, 5 , 0 , 0 , 0 , 0  } }, // #57 [ref=5x]
  { InstDB::RWInfo::kCategoryGeneric   , 10, { 11, 3 , 0 , 0 , 0 , 0  } }, // #58 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 53, 19, 0 , 0 , 0 , 0  } }, // #59 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 60, 0 , 0 , 0 , 0 , 0  } }, // #60 [ref=3x]
  { InstDB::RWInfo::kCategoryMov       , 28, { 0 , 0 , 0 , 0 , 0 , 0  } }, // #61 [ref=1x]
  { InstDB::RWInfo::kCategoryMovabs    , 0 , { 0 , 0 , 0 , 0 , 0 , 0  } }, // #62 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 29, { 10, 5 , 0 , 0 , 0 , 0  } }, // #63 [ref=6x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 11, 3 , 0 , 0 , 0 , 0  } }, // #64 [ref=18x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 35, 64, 0 , 0 , 0 , 0  } }, // #65 [ref=1x]
  { InstDB::RWInfo::kCategoryMovh64    , 11, { 0 , 0 , 0 , 0 , 0 , 0  } }, // #66 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 65, 7 , 0 , 0 , 0 , 0  } }, // #67 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 11, { 34, 7 , 0 , 0 , 0 , 0  } }, // #68 [ref=9x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 58, 5 , 0 , 0 , 0 , 0  } }, // #69 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 27, { 44, 9 , 0 , 0 , 0 , 0  } }, // #70 [ref=4x]
  { InstDB::RWInfo::kCategoryGeneric   , 13, { 66, 19, 0 , 0 , 0 , 0  } }, // #71 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 30, { 34, 7 , 0 , 0 , 0 , 0  } }, // #72 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 32, { 44, 9 , 0 , 0 , 0 , 0  } }, // #73 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 15, { 11, 3 , 0 , 0 , 0 , 0  } }, // #74 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 16, 28, 0 , 0 , 0 , 0  } }, // #75 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 9 , { 3 , 3 , 0 , 0 , 0 , 0  } }, // #76 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 54, 21, 0 , 0 , 0 , 0  } }, // #77 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 13, { 54, 69, 0 , 0 , 0 , 0  } }, // #78 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 4 , { 25, 7 , 0 , 0 , 0 , 0  } }, // #79 [ref=18x]
  { InstDB::RWInfo::kCategoryGeneric   , 35, { 0 , 0 , 0 , 0 , 0 , 0  } }, // #80 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 3 , { 72, 5 , 0 , 0 , 0 , 0  } }, // #81 [ref=2x]
  { InstDB::RWInfo::kCategoryVmov1_8   , 0 , { 0 , 0 , 0 , 0 , 0 , 0  } }, // #82 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 5 , { 10, 9 , 0 , 0 , 0 , 0  } }, // #83 [ref=4x]
  { InstDB::RWInfo::kCategoryGeneric   , 26, { 10, 13, 0 , 0 , 0 , 0  } }, // #84 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 4 , 0 , 0 , 0 , 0 , 0  } }, // #85 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 3 , { 5 , 5 , 0 , 0 , 0 , 0  } }, // #86 [ref=1x]
  { InstDB::RWInfo::kCategoryPunpcklxx , 37, { 0 , 0 , 0 , 0 , 0 , 0  } }, // #87 [ref=3x]
  { InstDB::RWInfo::kCategoryGeneric   , 8 , { 2 , 73, 0 , 0 , 0 , 0  } }, // #88 [ref=7x]
  { InstDB::RWInfo::kCategoryGeneric   , 5 , { 36, 9 , 0 , 0 , 0 , 0  } }, // #89 [ref=3x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 34, 0 , 0 , 0 , 0 , 0  } }, // #90 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 15, 52, 0 , 0 , 0 , 0  } }, // #91 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 21, 20, 0 , 0 , 0 , 0  } }, // #92 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 66, 21, 0 , 0 , 0 , 0  } }, // #93 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 10, { 42, 3 , 0 , 0 , 0 , 0  } }, // #94 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 10, { 11, 43, 0 , 0 , 0 , 0  } }, // #95 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 5 , { 77, 9 , 0 , 0 , 0 , 0  } }, // #96 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 20, { 11, 13, 0 , 0 , 0 , 0  } }, // #97 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 14, { 78, 5 , 0 , 0 , 0 , 0  } }, // #98 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 14, { 11, 5 , 0 , 0 , 0 , 0  } }, // #99 [ref=4x]
  { InstDB::RWInfo::kCategoryGeneric   , 42, { 42, 79, 0 , 0 , 0 , 0  } }, // #100 [ref=4x]
  { InstDB::RWInfo::kCategoryGeneric   , 43, { 11, 7 , 0 , 0 , 0 , 0  } }, // #101 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 44, { 11, 9 , 0 , 0 , 0 , 0  } }, // #102 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 26, { 13, 13, 0 , 0 , 0 , 0  } }, // #103 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 9 , { 11, 3 , 0 , 0 , 0 , 0  } }, // #104 [ref=7x]
  { InstDB::RWInfo::kCategoryVmov2_1   , 45, { 0 , 0 , 0 , 0 , 0 , 0  } }, // #105 [ref=19x]
  { InstDB::RWInfo::kCategoryVmov1_2   , 15, { 0 , 0 , 0 , 0 , 0 , 0  } }, // #106 [ref=11x]
  { InstDB::RWInfo::kCategoryVmov1_4   , 15, { 0 , 0 , 0 , 0 , 0 , 0  } }, // #107 [ref=2x]
  { InstDB::RWInfo::kCategoryVmov4_1   , 46, { 0 , 0 , 0 , 0 , 0 , 0  } }, // #108 [ref=9x]
  { InstDB::RWInfo::kCategoryGeneric   , 15, { 10, 3 , 0 , 0 , 0 , 0  } }, // #109 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 26, { 11, 13, 0 , 0 , 0 , 0  } }, // #110 [ref=5x]
  { InstDB::RWInfo::kCategoryGeneric   , 5 , { 44, 9 , 0 , 0 , 0 , 0  } }, // #111 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 13, { 2 , 3 , 0 , 0 , 0 , 0  } }, // #112 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 56, { 11, 3 , 0 , 0 , 0 , 0  } }, // #113 [ref=12x]
  { InstDB::RWInfo::kCategoryVmovddup  , 37, { 0 , 0 , 0 , 0 , 0 , 0  } }, // #114 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 11, { 34, 64, 0 , 0 , 0 , 0  } }, // #115 [ref=2x]
  { InstDB::RWInfo::kCategoryVmovmskpd , 0 , { 0 , 0 , 0 , 0 , 0 , 0  } }, // #116 [ref=1x]
  { InstDB::RWInfo::kCategoryVmovmskps , 0 , { 0 , 0 , 0 , 0 , 0 , 0  } }, // #117 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 57, { 34, 7 , 0 , 0 , 0 , 0  } }, // #118 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 20, { 50, 13, 0 , 0 , 0 , 0  } }, // #119 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 2 , { 3 , 3 , 0 , 0 , 0 , 0  } }, // #120 [ref=4x]
  { InstDB::RWInfo::kCategoryGeneric   , 16, { 11, 39, 0 , 0 , 0 , 0  } }, // #121 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 11, 7 , 0 , 0 , 0 , 0  } }, // #122 [ref=6x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 34, 3 , 0 , 0 , 0 , 0  } }, // #123 [ref=4x]
  { InstDB::RWInfo::kCategoryVmov1_4   , 60, { 0 , 0 , 0 , 0 , 0 , 0  } }, // #124 [ref=6x]
  { InstDB::RWInfo::kCategoryVmov1_2   , 47, { 0 , 0 , 0 , 0 , 0 , 0  } }, // #125 [ref=9x]
  { InstDB::RWInfo::kCategoryVmov1_8   , 61, { 0 , 0 , 0 , 0 , 0 , 0  } }, // #126 [ref=3x]
  { InstDB::RWInfo::kCategoryVmov8_1   , 62, { 0 , 0 , 0 , 0 , 0 , 0  } }, // #127 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 13, { 11, 3 , 0 , 0 , 0 , 0  } }, // #128 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 88, 5 , 0 , 0 , 0 , 0  } }, // #129 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 88, 79, 0 , 0 , 0 , 0  } }, // #130 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 9 , { 2 , 2 , 0 , 0 , 0 , 0  } }, // #131 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 56, { 2 , 2 , 0 , 0 , 0 , 0  } }  // #132 [ref=1x]
};

const InstDB::RWInfo InstDB::rw_info_b_table[] = {
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 0 , 0 , 0 , 0 , 0 , 0  } }, // #0 [ref=751x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 1 , 0 , 0 , 0 , 0 , 0  } }, // #1 [ref=5x]
  { InstDB::RWInfo::kCategoryGeneric   , 3 , { 10, 5 , 0 , 0 , 0 , 0  } }, // #2 [ref=8x]
  { InstDB::RWInfo::kCategoryGeneric   , 6 , { 11, 3 , 3 , 0 , 0 , 0  } }, // #3 [ref=193x]
  { InstDB::RWInfo::kCategoryGeneric   , 2 , { 11, 3 , 3 , 0 , 0 , 0  } }, // #4 [ref=5x]
  { InstDB::RWInfo::kCategoryGeneric   , 3 , { 4 , 5 , 0 , 0 , 0 , 0  } }, // #5 [ref=13x]
  { InstDB::RWInfo::kCategoryGeneric   , 3 , { 4 , 5 , 14, 0 , 0 , 0  } }, // #6 [ref=3x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 2 , 0 , 0 , 0 , 0 , 0  } }, // #7 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 9 , { 3 , 0 , 0 , 0 , 0 , 0  } }, // #8 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 17, 0 , 0 , 0 , 0 , 0  } }, // #9 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 10, { 3 , 0 , 0 , 0 , 0 , 0  } }, // #10 [ref=21x]
  { InstDB::RWInfo::kCategoryGeneric   , 11, { 7 , 0 , 0 , 0 , 0 , 0  } }, // #11 [ref=5x]
  { InstDB::RWInfo::kCategoryGeneric   , 12, { 18, 0 , 0 , 0 , 0 , 0  } }, // #12 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 2 , 2 , 3 , 0 , 0 , 0  } }, // #13 [ref=16x]
  { InstDB::RWInfo::kCategoryGeneric   , 4 , { 6 , 7 , 0 , 0 , 0 , 0  } }, // #14 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 5 , { 8 , 9 , 0 , 0 , 0 , 0  } }, // #15 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 9 , { 2 , 3 , 21, 0 , 0 , 0  } }, // #16 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 14, { 4 , 22, 17, 23, 24, 0  } }, // #17 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 11, { 25, 26, 27, 28, 29, 0  } }, // #18 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 27, 30, 31, 15, 0 , 0  } }, // #19 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 27, 0 , 0 , 0 , 0 , 0  } }, // #20 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 8 , { 2 , 0 , 0 , 0 , 0 , 0  } }, // #21 [ref=4x]
  { InstDB::RWInfo::kCategoryGeneric   , 6 , { 40, 41, 3 , 0 , 0 , 0  } }, // #22 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 17, { 44, 5 , 0 , 0 , 0 , 0  } }, // #23 [ref=4x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 4 , 0 , 0 , 0 , 0 , 0  } }, // #24 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 13, { 3 , 0 , 0 , 0 , 0 , 0  } }, // #25 [ref=17x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 47, 0 , 0 , 0 , 0 , 0  } }, // #26 [ref=16x]
  { InstDB::RWInfo::kCategoryGeneric   , 18, { 48, 0 , 0 , 0 , 0 , 0  } }, // #27 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 18, { 49, 0 , 0 , 0 , 0 , 0  } }, // #28 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 19, { 3 , 0 , 0 , 0 , 0 , 0  } }, // #29 [ref=3x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 48, 0 , 0 , 0 , 0 , 0  } }, // #30 [ref=6x]
  { InstDB::RWInfo::kCategoryGeneric   , 13, { 11, 0 , 0 , 0 , 0 , 0  } }, // #31 [ref=3x]
  { InstDB::RWInfo::kCategoryGeneric   , 20, { 13, 0 , 0 , 0 , 0 , 0  } }, // #32 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 10, { 11, 0 , 0 , 0 , 0 , 0  } }, // #33 [ref=8x]
  { InstDB::RWInfo::kCategoryGeneric   , 20, { 50, 0 , 0 , 0 , 0 , 0  } }, // #34 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 7 , { 51, 0 , 0 , 0 , 0 , 0  } }, // #35 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 19, { 11, 0 , 0 , 0 , 0 , 0  } }, // #36 [ref=2x]
  { InstDB::RWInfo::kCategoryImul      , 21, { 0 , 0 , 0 , 0 , 0 , 0  } }, // #37 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 39, 0 , 0 , 0 , 0 , 0  } }, // #38 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 5 , { 4 , 9 , 0 , 0 , 0 , 0  } }, // #39 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 4 , 5 , 0 , 0 , 0 , 0  } }, // #40 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 21, 56, 57, 0 , 0 , 0  } }, // #41 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 58, 39, 39, 0 , 0 , 0  } }, // #42 [ref=6x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 44, 9 , 9 , 0 , 0 , 0  } }, // #43 [ref=6x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 34, 7 , 7 , 0 , 0 , 0  } }, // #44 [ref=6x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 50, 13, 13, 0 , 0 , 0  } }, // #45 [ref=6x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 58, 39, 0 , 0 , 0 , 0  } }, // #46 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 44, 9 , 0 , 0 , 0 , 0  } }, // #47 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 34, 7 , 0 , 0 , 0 , 0  } }, // #48 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 50, 13, 0 , 0 , 0 , 0  } }, // #49 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 50, 39, 39, 0 , 0 , 0  } }, // #50 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 34, 9 , 9 , 0 , 0 , 0  } }, // #51 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 44, 13, 13, 0 , 0 , 0  } }, // #52 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 59, 0 , 0 , 0 , 0 , 0  } }, // #53 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 27, { 9 , 0 , 0 , 0 , 0 , 0  } }, // #54 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 12, { 43, 0 , 0 , 0 , 0 , 0  } }, // #55 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 7 , { 13, 0 , 0 , 0 , 0 , 0  } }, // #56 [ref=5x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 3 , 0 , 0 , 0 , 0 , 0  } }, // #57 [ref=3x]
  { InstDB::RWInfo::kCategoryGeneric   , 5 , { 3 , 9 , 0 , 0 , 0 , 0  } }, // #58 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 14, { 5 , 5 , 61, 0 , 0 , 0  } }, // #59 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 11, { 7 , 7 , 62, 0 , 0 , 0  } }, // #60 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 10, { 63, 28, 56, 0 , 0 , 0  } }, // #61 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 31, { 0 , 0 , 0 , 0 , 0 , 0  } }, // #62 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 6 , { 67, 41, 3 , 0 , 0 , 0  } }, // #63 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 6 , { 11, 11, 3 , 68, 0 , 0  } }, // #64 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 16, 28, 29, 0 , 0 , 0  } }, // #65 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 8 , { 3 , 0 , 0 , 0 , 0 , 0  } }, // #66 [ref=3x]
  { InstDB::RWInfo::kCategoryGeneric   , 2 , { 2 , 3 , 0 , 0 , 0 , 0  } }, // #67 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 3 , { 5 , 5 , 0 , 70, 16, 56 } }, // #68 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 3 , { 5 , 5 , 0 , 71, 16, 56 } }, // #69 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 3 , { 5 , 5 , 0 , 70, 0 , 0  } }, // #70 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 3 , { 5 , 5 , 0 , 71, 0 , 0  } }, // #71 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 33, { 58, 5 , 0 , 0 , 0 , 0  } }, // #72 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 34, { 34, 5 , 0 , 0 , 0 , 0  } }, // #73 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 36, { 50, 3 , 0 , 0 , 0 , 0  } }, // #74 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 16, { 4 , 39, 0 , 0 , 0 , 0  } }, // #75 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 4 , { 4 , 7 , 0 , 0 , 0 , 0  } }, // #76 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 26, { 2 , 13, 0 , 0 , 0 , 0  } }, // #77 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 8 , { 11, 0 , 0 , 0 , 0 , 0  } }, // #78 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 4 , { 34, 7 , 0 , 0 , 0 , 0  } }, // #79 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 11, 0 , 0 , 0 , 0 , 0  } }, // #80 [ref=6x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 15, 52, 28, 0 , 0 , 0  } }, // #81 [ref=5x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 44, 0 , 0 , 0 , 0 , 0  } }, // #82 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 34, 0 , 0 , 0 , 0 , 0  } }, // #83 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 15, 52, 70, 0 , 0 , 0  } }, // #84 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 2 , { 11, 3 , 0 , 0 , 0 , 0  } }, // #85 [ref=19x]
  { InstDB::RWInfo::kCategoryGeneric   , 4 , { 35, 7 , 0 , 0 , 0 , 0  } }, // #86 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 5 , { 36, 9 , 0 , 0 , 0 , 0  } }, // #87 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 74, 0 , 0 , 0 , 0 , 0  } }, // #88 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 7 , 0 , 0 , 0 , 0 , 0  } }, // #89 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 33, { 75, 0 , 0 , 0 , 0 , 0  } }, // #90 [ref=16x]
  { InstDB::RWInfo::kCategoryGeneric   , 3 , { 10, 5 , 14, 0 , 0 , 0  } }, // #91 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 9 , { 2 , 3 , 73, 0 , 0 , 0  } }, // #92 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 38, { 11, 0 , 0 , 0 , 0 , 0  } }, // #93 [ref=3x]
  { InstDB::RWInfo::kCategoryGeneric   , 27, { 44, 0 , 0 , 0 , 0 , 0  } }, // #94 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 12, { 42, 0 , 0 , 0 , 0 , 0  } }, // #95 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 76, 43, 43, 0 , 0 , 0  } }, // #96 [ref=8x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 42, 0 , 0 , 0 , 0 , 0  } }, // #97 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 9 , 56, 16, 0 , 0 , 0  } }, // #98 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 39, { 10, 5 , 7 , 0 , 0 , 0  } }, // #99 [ref=9x]
  { InstDB::RWInfo::kCategoryGeneric   , 40, { 10, 5 , 13, 0 , 0 , 0  } }, // #100 [ref=9x]
  { InstDB::RWInfo::kCategoryGeneric   , 41, { 10, 5 , 9 , 0 , 0 , 0  } }, // #101 [ref=9x]
  { InstDB::RWInfo::kCategoryGeneric   , 6 , { 11, 3 , 3 , 3 , 0 , 0  } }, // #102 [ref=3x]
  { InstDB::RWInfo::kCategoryGeneric   , 6 , { 34, 3 , 3 , 0 , 0 , 0  } }, // #103 [ref=18x]
  { InstDB::RWInfo::kCategoryGeneric   , 39, { 11, 5 , 7 , 0 , 0 , 0  } }, // #104 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 40, { 34, 13, 13, 0 , 0 , 0  } }, // #105 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 41, { 11, 5 , 9 , 0 , 0 , 0  } }, // #106 [ref=1x]
  { InstDB::RWInfo::kCategoryVmov1_2   , 47, { 0 , 0 , 0 , 0 , 0 , 0  } }, // #107 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 39, { 10, 80, 7 , 0 , 0 , 0  } }, // #108 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 40, { 10, 5 , 5 , 0 , 0 , 0  } }, // #109 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 48, { 10, 64, 3 , 0 , 0 , 0  } }, // #110 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 48, { 10, 3 , 3 , 0 , 0 , 0  } }, // #111 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 48, { 10, 80, 3 , 0 , 0 , 0  } }, // #112 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 41, { 10, 64, 9 , 0 , 0 , 0  } }, // #113 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 41, { 10, 5 , 5 , 0 , 0 , 0  } }, // #114 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 49, { 10, 5 , 5 , 0 , 0 , 0  } }, // #115 [ref=9x]
  { InstDB::RWInfo::kCategoryGeneric   , 50, { 10, 79, 0 , 0 , 0 , 0  } }, // #116 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 50, { 10, 3 , 0 , 0 , 0 , 0  } }, // #117 [ref=4x]
  { InstDB::RWInfo::kCategoryGeneric   , 51, { 78, 43, 0 , 0 , 0 , 0  } }, // #118 [ref=4x]
  { InstDB::RWInfo::kCategoryGeneric   , 6 , { 81, 3 , 3 , 0 , 0 , 0  } }, // #119 [ref=4x]
  { InstDB::RWInfo::kCategoryGeneric   , 41, { 82, 5 , 5 , 0 , 0 , 0  } }, // #120 [ref=3x]
  { InstDB::RWInfo::kCategoryGeneric   , 6 , { 2 , 3 , 3 , 0 , 0 , 0  } }, // #121 [ref=90x]
  { InstDB::RWInfo::kCategoryGeneric   , 39, { 4 , 64, 7 , 0 , 0 , 0  } }, // #122 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 41, { 4 , 80, 9 , 0 , 0 , 0  } }, // #123 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 39, { 6 , 7 , 7 , 0 , 0 , 0  } }, // #124 [ref=11x]
  { InstDB::RWInfo::kCategoryGeneric   , 40, { 83, 5 , 5 , 0 , 0 , 0  } }, // #125 [ref=6x]
  { InstDB::RWInfo::kCategoryGeneric   , 41, { 8 , 9 , 9 , 0 , 0 , 0  } }, // #126 [ref=11x]
  { InstDB::RWInfo::kCategoryGeneric   , 52, { 11, 3 , 3 , 3 , 0 , 0  } }, // #127 [ref=15x]
  { InstDB::RWInfo::kCategoryGeneric   , 53, { 34, 7 , 7 , 7 , 0 , 0  } }, // #128 [ref=4x]
  { InstDB::RWInfo::kCategoryGeneric   , 54, { 44, 9 , 9 , 9 , 0 , 0  } }, // #129 [ref=4x]
  { InstDB::RWInfo::kCategoryGeneric   , 40, { 83, 5 , 13, 0 , 0 , 0  } }, // #130 [ref=6x]
  { InstDB::RWInfo::kCategoryGeneric   , 41, { 84, 5 , 5 , 0 , 0 , 0  } }, // #131 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 39, { 25, 7 , 7 , 0 , 0 , 0  } }, // #132 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 41, { 77, 9 , 9 , 0 , 0 , 0  } }, // #133 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 15, { 34, 3 , 0 , 0 , 0 , 0  } }, // #134 [ref=3x]
  { InstDB::RWInfo::kCategoryGeneric   , 26, { 34, 13, 0 , 0 , 0 , 0  } }, // #135 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 5 , { 34, 9 , 0 , 0 , 0 , 0  } }, // #136 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 10, { 2 , 3 , 2 , 0 , 0 , 0  } }, // #137 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 2 , 3 , 2 , 0 , 0 , 0  } }, // #138 [ref=4x]
  { InstDB::RWInfo::kCategoryGeneric   , 13, { 4 , 3 , 4 , 0 , 0 , 0  } }, // #139 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 39, { 10, 64, 7 , 0 , 0 , 0  } }, // #140 [ref=9x]
  { InstDB::RWInfo::kCategoryGeneric   , 40, { 10, 85, 13, 0 , 0 , 0  } }, // #141 [ref=7x]
  { InstDB::RWInfo::kCategoryGeneric   , 41, { 10, 80, 9 , 0 , 0 , 0  } }, // #142 [ref=11x]
  { InstDB::RWInfo::kCategoryGeneric   , 49, { 78, 79, 5 , 0 , 0 , 0  } }, // #143 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 49, { 11, 3 , 5 , 0 , 0 , 0  } }, // #144 [ref=4x]
  { InstDB::RWInfo::kCategoryGeneric   , 55, { 42, 43, 79, 0 , 0 , 0  } }, // #145 [ref=4x]
  { InstDB::RWInfo::kCategoryVmaskmov  , 0 , { 0 , 0 , 0 , 0 , 0 , 0  } }, // #146 [ref=4x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 21, 0 , 0 , 0 , 0 , 0  } }, // #147 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 10, 64, 64, 0 , 0 , 0  } }, // #148 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 11, { 10, 7 , 7 , 0 , 0 , 0  } }, // #149 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 10, 7 , 7 , 0 , 0 , 0  } }, // #150 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 11, { 10, 64, 7 , 0 , 0 , 0  } }, // #151 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 10, 64, 7 , 0 , 0 , 0  } }, // #152 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 10, 85, 13, 0 , 0 , 0  } }, // #153 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 10, 80, 9 , 0 , 0 , 0  } }, // #154 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 11, { 34, 0 , 0 , 0 , 0 , 0  } }, // #155 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 86, 0 , 0 , 0 , 0 , 0  } }, // #156 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 58, { 34, 87, 3 , 3 , 0 , 0  } }, // #157 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 55, { 78, 79, 79, 0 , 0 , 0  } }, // #158 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 21, { 11, 3 , 3 , 0 , 0 , 0  } }, // #159 [ref=4x]
  { InstDB::RWInfo::kCategoryGeneric   , 7 , { 50, 5 , 0 , 0 , 0 , 0  } }, // #160 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 59, { 10, 5 , 39, 0 , 0 , 0  } }, // #161 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 49, { 10, 5 , 5 , 5 , 0 , 0  } }, // #162 [ref=12x]
  { InstDB::RWInfo::kCategoryGeneric   , 63, { 10, 5 , 5 , 5 , 0 , 0  } }, // #163 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 64, { 10, 5 , 5 , 0 , 0 , 0  } }, // #164 [ref=12x]
  { InstDB::RWInfo::kCategoryGeneric   , 65, { 11, 3 , 5 , 0 , 0 , 0  } }, // #165 [ref=5x]
  { InstDB::RWInfo::kCategoryGeneric   , 66, { 11, 3 , 0 , 0 , 0 , 0  } }, // #166 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 67, { 11, 3 , 5 , 0 , 0 , 0  } }, // #167 [ref=3x]
  { InstDB::RWInfo::kCategoryGeneric   , 21, { 11, 3 , 5 , 0 , 0 , 0  } }, // #168 [ref=1x]
  { InstDB::RWInfo::kCategoryGenericEx , 6 , { 2 , 3 , 3 , 0 , 0 , 0  } }, // #169 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 88, 79, 5 , 0 , 0 , 0  } }, // #170 [ref=1x]
  { InstDB::RWInfo::kCategoryGeneric   , 49, { 4 , 5 , 5 , 0 , 0 , 0  } }, // #171 [ref=3x]
  { InstDB::RWInfo::kCategoryGeneric   , 0 , { 56, 16, 28, 0 , 0 , 0  } }, // #172 [ref=2x]
  { InstDB::RWInfo::kCategoryGeneric   , 10, { 3 , 56, 16, 0 , 0 , 0  } }, // #173 [ref=4x]
  { InstDB::RWInfo::kCategoryGeneric   , 10, { 11, 56, 16, 0 , 0 , 0  } }  // #174 [ref=8x]
};

const InstDB::RWInfoOp InstDB::rw_info_op_table[] = {
  { 0x0000000000000000u, 0x0000000000000000u, 0xFF, 0, { 0 }, OpRWFlags::kNone }, // #0 [ref=16278x]
  { 0x0000000000000003u, 0x0000000000000003u, 0x00, 0, { 0 }, OpRWFlags::kRW | OpRWFlags::kRegPhysId }, // #1 [ref=10x]
  { 0x0000000000000000u, 0x0000000000000000u, 0xFF, 0, { 0 }, OpRWFlags::kRW | OpRWFlags::kZExt }, // #2 [ref=267x]
  { 0x0000000000000000u, 0x0000000000000000u, 0xFF, 0, { 0 }, OpRWFlags::kRead }, // #3 [ref=1086x]
  { 0x000000000000FFFFu, 0x000000000000FFFFu, 0xFF, 0, { 0 }, OpRWFlags::kRW | OpRWFlags::kZExt }, // #4 [ref=86x]
  { 0x000000000000FFFFu, 0x0000000000000000u, 0xFF, 0, { 0 }, OpRWFlags::kRead }, // #5 [ref=333x]
  { 0x00000000000000FFu, 0x00000000000000FFu, 0xFF, 0, { 0 }, OpRWFlags::kRW }, // #6 [ref=18x]
  { 0x00000000000000FFu, 0x0000000000000000u, 0xFF, 0, { 0 }, OpRWFlags::kRead }, // #7 [ref=186x]
  { 0x000000000000000Fu, 0x000000000000000Fu, 0xFF, 0, { 0 }, OpRWFlags::kRW }, // #8 [ref=18x]
  { 0x000000000000000Fu, 0x0000000000000000u, 0xFF, 0, { 0 }, OpRWFlags::kRead }, // #9 [ref=133x]
  { 0x0000000000000000u, 0x000000000000FFFFu, 0xFF, 0, { 0 }, OpRWFlags::kWrite | OpRWFlags::kZExt }, // #10 [ref=182x]
  { 0x0000000000000000u, 0x0000000000000000u, 0xFF, 0, { 0 }, OpRWFlags::kWrite | OpRWFlags::kZExt }, // #11 [ref=445x]
  { 0x0000000000000003u, 0x0000000000000003u, 0xFF, 0, { 0 }, OpRWFlags::kRW }, // #12 [ref=1x]
  { 0x0000000000000003u, 0x0000000000000000u, 0xFF, 0, { 0 }, OpRWFlags::kRead }, // #13 [ref=71x]
  { 0x000000000000FFFFu, 0x0000000000000000u, 0x00, 0, { 0 }, OpRWFlags::kRead | OpRWFlags::kRegPhysId }, // #14 [ref=4x]
  { 0x0000000000000000u, 0x000000000000000Fu, 0x02, 0, { 0 }, OpRWFlags::kWrite | OpRWFlags::kZExt | OpRWFlags::kRegPhysId }, // #15 [ref=9x]
  { 0x000000000000000Fu, 0x0000000000000000u, 0x00, 0, { 0 }, OpRWFlags::kRead | OpRWFlags::kRegPhysId }, // #16 [ref=23x]
  { 0x00000000000000FFu, 0x00000000000000FFu, 0x00, 0, { 0 }, OpRWFlags::kRW | OpRWFlags::kZExt | OpRWFlags::kRegPhysId }, // #17 [ref=2x]
  { 0xFFFFFFFFFFFFFFFFu, 0x0000000000000000u, 0x00, 0, { 0 }, OpRWFlags::kRead | OpRWFlags::kMemPhysId }, // #18 [ref=1x]
  { 0x0000000000000000u, 0x0000000000000000u, 0x06, 0, { 0 }, OpRWFlags::kRead | OpRWFlags::kMemBaseRW | OpRWFlags::kMemBasePostModify | OpRWFlags::kMemPhysId }, // #19 [ref=3x]
  { 0x0000000000000000u, 0x0000000000000000u, 0x07, 0, { 0 }, OpRWFlags::kRead | OpRWFlags::kMemBaseRW | OpRWFlags::kMemBasePostModify | OpRWFlags::kMemPhysId }, // #20 [ref=2x]
  { 0x0000000000000000u, 0x0000000000000000u, 0x00, 0, { 0 }, OpRWFlags::kRead | OpRWFlags::kRegPhysId }, // #21 [ref=8x]
  { 0x00000000000000FFu, 0x00000000000000FFu, 0x02, 0, { 0 }, OpRWFlags::kRW | OpRWFlags::kZExt | OpRWFlags::kRegPhysId }, // #22 [ref=1x]
  { 0x00000000000000FFu, 0x0000000000000000u, 0x01, 0, { 0 }, OpRWFlags::kRead | OpRWFlags::kRegPhysId }, // #23 [ref=1x]
  { 0x00000000000000FFu, 0x0000000000000000u, 0x03, 0, { 0 }, OpRWFlags::kRead | OpRWFlags::kRegPhysId }, // #24 [ref=1x]
  { 0x00000000000000FFu, 0x00000000000000FFu, 0xFF, 0, { 0 }, OpRWFlags::kRW | OpRWFlags::kZExt }, // #25 [ref=20x]
  { 0x000000000000000Fu, 0x000000000000000Fu, 0x02, 0, { 0 }, OpRWFlags::kRW | OpRWFlags::kZExt | OpRWFlags::kRegPhysId }, // #26 [ref=1x]
  { 0x000000000000000Fu, 0x000000000000000Fu, 0x00, 0, { 0 }, OpRWFlags::kRW | OpRWFlags::kZExt | OpRWFlags::kRegPhysId }, // #27 [ref=4x]
  { 0x000000000000000Fu, 0x0000000000000000u, 0x01, 0, { 0 }, OpRWFlags::kRead | OpRWFlags::kRegPhysId }, // #28 [ref=13x]
  { 0x000000000000000Fu, 0x0000000000000000u, 0x03, 0, { 0 }, OpRWFlags::kRead | OpRWFlags::kRegPhysId }, // #29 [ref=3x]
  { 0x0000000000000000u, 0x000000000000000Fu, 0x03, 0, { 0 }, OpRWFlags::kWrite | OpRWFlags::kZExt | OpRWFlags::kRegPhysId }, // #30 [ref=1x]
  { 0x000000000000000Fu, 0x000000000000000Fu, 0x01, 0, { 0 }, OpRWFlags::kRW | OpRWFlags::kZExt | OpRWFlags::kRegPhysId }, // #31 [ref=1x]
  { 0x0000000000000000u, 0x00000000000000FFu, 0x02, 0, { 0 }, OpRWFlags::kWrite | OpRWFlags::kZExt | OpRWFlags::kRegPhysId }, // #32 [ref=1x]
  { 0x00000000000000FFu, 0x0000000000000000u, 0x00, 0, { 0 }, OpRWFlags::kRead | OpRWFlags::kRegPhysId }, // #33 [ref=1x]
  { 0x0000000000000000u, 0x00000000000000FFu, 0xFF, 0, { 0 }, OpRWFlags::kWrite | OpRWFlags::kZExt }, // #34 [ref=84x]
  { 0x0000000000000000u, 0x00000000000000FFu, 0xFF, 0, { 0 }, OpRWFlags::kWrite }, // #35 [ref=6x]
  { 0x0000000000000000u, 0x000000000000000Fu, 0xFF, 0, { 0 }, OpRWFlags::kWrite }, // #36 [ref=6x]
  { 0x0000000000000000u, 0x0000000000000003u, 0x02, 0, { 0 }, OpRWFlags::kWrite | OpRWFlags::kRegPhysId }, // #37 [ref=1x]
  { 0x0000000000000003u, 0x0000000000000000u, 0x00, 0, { 0 }, OpRWFlags::kRead | OpRWFlags::kRegPhysId }, // #38 [ref=1x]
  { 0x0000000000000001u, 0x0000000000000000u, 0xFF, 0, { 0 }, OpRWFlags::kRead }, // #39 [ref=30x]
  { 0x0000000000000000u, 0x0000000000000000u, 0x02, 0, { 0 }, OpRWFlags::kRW | OpRWFlags::kRegPhysId | OpRWFlags::kZExt }, // #40 [ref=2x]
  { 0x0000000000000000u, 0x0000000000000000u, 0x00, 0, { 0 }, OpRWFlags::kRW | OpRWFlags::kRegPhysId | OpRWFlags::kZExt }, // #41 [ref=3x]
  { 0x0000000000000000u, 0xFFFFFFFFFFFFFFFFu, 0xFF, 0, { 0 }, OpRWFlags::kWrite | OpRWFlags::kZExt }, // #42 [ref=15x]
  { 0xFFFFFFFFFFFFFFFFu, 0x0000000000000000u, 0xFF, 0, { 0 }, OpRWFlags::kRead }, // #43 [ref=29x]
  { 0x0000000000000000u, 0x000000000000000Fu, 0xFF, 0, { 0 }, OpRWFlags::kWrite | OpRWFlags::kZExt }, // #44 [ref=30x]
  { 0x00000000000003FFu, 0x00000000000003FFu, 0x00, 0, { 0 }, OpRWFlags::kRW | OpRWFlags::kZExt | OpRWFlags::kRegPhysId }, // #45 [ref=6x]
  { 0x00000000000003FFu, 0x0000000000000000u, 0x00, 0, { 0 }, OpRWFlags::kRead | OpRWFlags::kRegPhysId }, // #46 [ref=6x]
  { 0x00000000000003FFu, 0x00000000000003FFu, 0xFF, 0, { 0 }, OpRWFlags::kRW | OpRWFlags::kZExt }, // #47 [ref=16x]
  { 0x00000000000003FFu, 0x0000000000000000u, 0xFF, 0, { 0 }, OpRWFlags::kRead }, // #48 [ref=7x]
  { 0x0000000000000000u, 0x00000000000003FFu, 0xFF, 0, { 0 }, OpRWFlags::kWrite | OpRWFlags::kZExt }, // #49 [ref=1x]
  { 0x0000000000000000u, 0x0000000000000003u, 0xFF, 0, { 0 }, OpRWFlags::kWrite | OpRWFlags::kZExt }, // #50 [ref=17x]
  { 0x0000000000000000u, 0x0000000000000003u, 0x00, 0, { 0 }, OpRWFlags::kWrite | OpRWFlags::kRegPhysId | OpRWFlags::kZExt }, // #51 [ref=2x]
  { 0x0000000000000000u, 0x000000000000000Fu, 0x00, 0, { 0 }, OpRWFlags::kWrite | OpRWFlags::kZExt | OpRWFlags::kRegPhysId }, // #52 [ref=9x]
  { 0x0000000000000000u, 0x0000000000000000u, 0x00, 0, { 0 }, OpRWFlags::kWrite | OpRWFlags::kRegPhysId | OpRWFlags::kZExt }, // #53 [ref=2x]
  { 0x0000000000000003u, 0x0000000000000000u, 0x02, 0, { 0 }, OpRWFlags::kRead | OpRWFlags::kRegPhysId }, // #54 [ref=4x]
  { 0x0000000000000000u, 0x0000000000000000u, 0x07, 0, { 0 }, OpRWFlags::kWrite | OpRWFlags::kZExt | OpRWFlags::kMemPhysId }, // #55 [ref=1x]
  { 0x000000000000000Fu, 0x0000000000000000u, 0x02, 0, { 0 }, OpRWFlags::kRead | OpRWFlags::kRegPhysId }, // #56 [ref=23x]
  { 0x0000000000000000u, 0x0000000000000000u, 0x01, 0, { 0 }, OpRWFlags::kRead | OpRWFlags::kRegPhysId }, // #57 [ref=2x]
  { 0x0000000000000000u, 0x0000000000000001u, 0xFF, 0, { 0 }, OpRWFlags::kWrite | OpRWFlags::kZExt }, // #58 [ref=14x]
  { 0x0000000000000000u, 0x0000000000000001u, 0x00, 0, { 0 }, OpRWFlags::kWrite | OpRWFlags::kRegPhysId }, // #59 [ref=1x]
  { 0x0000000000000000u, 0x0000000000000000u, 0x01, 0, { 0 }, OpRWFlags::kRW | OpRWFlags::kRegPhysId | OpRWFlags::kZExt }, // #60 [ref=3x]
  { 0x000000000000FFFFu, 0x000000000000FFFFu, 0x07, 0, { 0 }, OpRWFlags::kRW | OpRWFlags::kZExt | OpRWFlags::kMemPhysId }, // #61 [ref=2x]
  { 0x00000000000000FFu, 0x00000000000000FFu, 0x07, 0, { 0 }, OpRWFlags::kRW | OpRWFlags::kZExt | OpRWFlags::kMemPhysId }, // #62 [ref=1x]
  { 0x0000000000000000u, 0x0000000000000000u, 0x00, 0, { 0 }, OpRWFlags::kRead | OpRWFlags::kMemPhysId }, // #63 [ref=2x]
  { 0x000000000000FF00u, 0x0000000000000000u, 0xFF, 0, { 0 }, OpRWFlags::kRead }, // #64 [ref=21x]
  { 0x0000000000000000u, 0x000000000000FF00u, 0xFF, 0, { 0 }, OpRWFlags::kWrite }, // #65 [ref=1x]
  { 0x0000000000000000u, 0x0000000000000000u, 0x07, 0, { 0 }, OpRWFlags::kWrite | OpRWFlags::kZExt | OpRWFlags::kMemBaseRW | OpRWFlags::kMemBasePostModify | OpRWFlags::kMemPhysId }, // #66 [ref=2x]
  { 0x0000000000000000u, 0x0000000000000000u, 0x02, 0, { 0 }, OpRWFlags::kWrite | OpRWFlags::kRegPhysId | OpRWFlags::kZExt }, // #67 [ref=1x]
  { 0x0000000000000000u, 0x0000000000000000u, 0x02, 0, { 0 }, OpRWFlags::kRead | OpRWFlags::kRegPhysId }, // #68 [ref=1x]
  { 0x0000000000000000u, 0x0000000000000000u, 0x06, 0, { 0 }, OpRWFlags::kRead | OpRWFlags::kMemPhysId }, // #69 [ref=1x]
  { 0x0000000000000000u, 0x000000000000000Fu, 0x01, 0, { 0 }, OpRWFlags::kWrite | OpRWFlags::kZExt | OpRWFlags::kRegPhysId }, // #70 [ref=5x]
  { 0x0000000000000000u, 0x000000000000FFFFu, 0x00, 0, { 0 }, OpRWFlags::kWrite | OpRWFlags::kZExt | OpRWFlags::kRegPhysId }, // #71 [ref=4x]
  { 0x0000000000000000u, 0x0000000000000007u, 0xFF, 0, { 0 }, OpRWFlags::kWrite | OpRWFlags::kZExt }, // #72 [ref=2x]
  { 0x0000000000000001u, 0x0000000000000000u, 0x01, 0, { 0 }, OpRWFlags::kRead | OpRWFlags::kRegPhysId }, // #73 [ref=9x]
  { 0x0000000000000001u, 0x0000000000000000u, 0x00, 0, { 0 }, OpRWFlags::kRead | OpRWFlags::kRegPhysId }, // #74 [ref=1x]
  { 0x0000000000000000u, 0x0000000000000001u, 0xFF, 0, { 0 }, OpRWFlags::kWrite }, // #75 [ref=16x]
  { 0xFFFFFFFFFFFFFFFFu, 0xFFFFFFFFFFFFFFFFu, 0xFF, 0, { 0 }, OpRWFlags::kRW | OpRWFlags::kZExt }, // #76 [ref=8x]
  { 0x000000000000000Fu, 0x000000000000000Fu, 0xFF, 0, { 0 }, OpRWFlags::kRW | OpRWFlags::kZExt }, // #77 [ref=3x]
  { 0x0000000000000000u, 0x00000000FFFFFFFFu, 0xFF, 0, { 0 }, OpRWFlags::kWrite | OpRWFlags::kZExt }, // #78 [ref=10x]
  { 0x00000000FFFFFFFFu, 0x0000000000000000u, 0xFF, 0, { 0 }, OpRWFlags::kRead }, // #79 [ref=18x]
  { 0x000000000000FFF0u, 0x0000000000000000u, 0xFF, 0, { 0 }, OpRWFlags::kRead }, // #80 [ref=16x]
  { 0x0000000000000000u, 0x0000000000000000u, 0xFF, 0, { 0 }, OpRWFlags::kRW | OpRWFlags::kUnique | OpRWFlags::kZExt }, // #81 [ref=4x]
  { 0x000000000000FFFFu, 0x000000000000FFFFu, 0xFF, 0, { 0 }, OpRWFlags::kRW | OpRWFlags::kUnique }, // #82 [ref=3x]
  { 0x000000000000FFFFu, 0x000000000000FFFFu, 0xFF, 0, { 0 }, OpRWFlags::kRW }, // #83 [ref=12x]
  { 0x000000000000FFFFu, 0x000000000000FFFFu, 0xFF, 0, { 0 }, OpRWFlags::kRW | OpRWFlags::kUnique | OpRWFlags::kZExt }, // #84 [ref=1x]
  { 0x000000000000FFFCu, 0x0000000000000000u, 0xFF, 0, { 0 }, OpRWFlags::kRead }, // #85 [ref=8x]
  { 0x0000000000000000u, 0x0000000000000000u, 0x00, 0, { 0 }, OpRWFlags::kRW | OpRWFlags::kZExt | OpRWFlags::kRegPhysId }, // #86 [ref=1x]
  { 0x0000000000000000u, 0x0000000000000000u, 0xFF, 0, { 0 }, OpRWFlags::kWrite | OpRWFlags::kZExt | OpRWFlags::kConsecutive }, // #87 [ref=2x]
  { 0x00000000FFFFFFFFu, 0x00000000FFFFFFFFu, 0xFF, 0, { 0 }, OpRWFlags::kRW | OpRWFlags::kZExt }  // #88 [ref=3x]
};

const InstDB::RWInfoRm InstDB::rw_info_rm_table[] = {
  { InstDB::RWInfoRm::kCategoryNone      , 0x00, 0 , 0, 0 }, // #0 [ref=1989x]
  { InstDB::RWInfoRm::kCategoryConsistent, 0x03, 0 , InstDB::RWInfoRm::kFlagAmbiguous, 0 }, // #1 [ref=8x]
  { InstDB::RWInfoRm::kCategoryConsistent, 0x02, 0 , 0, 0 }, // #2 [ref=187x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x02, 16, 0, 0 }, // #3 [ref=122x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x02, 8 , 0, 0 }, // #4 [ref=66x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x02, 4 , 0, 0 }, // #5 [ref=35x]
  { InstDB::RWInfoRm::kCategoryConsistent, 0x04, 0 , 0, 0 }, // #6 [ref=314x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x01, 2 , 0, 0 }, // #7 [ref=9x]
  { InstDB::RWInfoRm::kCategoryConsistent, 0x01, 0 , InstDB::RWInfoRm::kFlagAmbiguous, 0 }, // #8 [ref=20x]
  { InstDB::RWInfoRm::kCategoryConsistent, 0x01, 0 , 0, 0 }, // #9 [ref=14x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x00, 0 , 0, 0 }, // #10 [ref=49x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x00, 8 , 0, 0 }, // #11 [ref=25x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x00, 64, 0, 0 }, // #12 [ref=6x]
  { InstDB::RWInfoRm::kCategoryNone      , 0x00, 0 , InstDB::RWInfoRm::kFlagAmbiguous, 0 }, // #13 [ref=30x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x00, 16, 0, 0 }, // #14 [ref=17x]
  { InstDB::RWInfoRm::kCategoryConsistent, 0x02, 0 , InstDB::RWInfoRm::kFlagAmbiguous, 0 }, // #15 [ref=22x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x02, 1 , 0, 0 }, // #16 [ref=5x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x01, 4 , 0, 0 }, // #17 [ref=4x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x00, 10, 0, 0 }, // #18 [ref=2x]
  { InstDB::RWInfoRm::kCategoryNone      , 0x01, 0 , InstDB::RWInfoRm::kFlagAmbiguous, 0 }, // #19 [ref=5x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x00, 2 , 0, 0 }, // #20 [ref=6x]
  { InstDB::RWInfoRm::kCategoryConsistent, 0x06, 0 , 0, 0 }, // #21 [ref=6x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x03, 1 , 0, 0 }, // #22 [ref=1x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x03, 4 , 0, 0 }, // #23 [ref=3x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x03, 8 , 0, 0 }, // #24 [ref=2x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x03, 2 , 0, 0 }, // #25 [ref=2x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x02, 2 , 0, 0 }, // #26 [ref=13x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x00, 4 , 0, 0 }, // #27 [ref=8x]
  { InstDB::RWInfoRm::kCategoryNone      , 0x03, 0 , InstDB::RWInfoRm::kFlagAmbiguous, 0 }, // #28 [ref=1x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x03, 16, 0, 0 }, // #29 [ref=6x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x03, 8 , InstDB::RWInfoRm::kFlagMovssMovsd, 0 }, // #30 [ref=1x]
  { InstDB::RWInfoRm::kCategoryNone      , 0x00, 0 , InstDB::RWInfoRm::kFlagMovssMovsd, 0 }, // #31 [ref=2x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x03, 4 , InstDB::RWInfoRm::kFlagMovssMovsd, 0 }, // #32 [ref=1x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x01, 1 , 0, 0 }, // #33 [ref=18x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x01, 8 , 0, 0 }, // #34 [ref=2x]
  { InstDB::RWInfoRm::kCategoryNone      , 0x00, 0 , InstDB::RWInfoRm::kFlagPextrw, 0 }, // #35 [ref=1x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x01, 2 , InstDB::RWInfoRm::kFlagPextrw, uint32_t(CpuFeatures::X86::kSSE4_1) }, // #36 [ref=1x]
  { InstDB::RWInfoRm::kCategoryNone      , 0x02, 0 , 0, 0 }, // #37 [ref=4x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x01, 2 , InstDB::RWInfoRm::kFlagAmbiguous, 0 }, // #38 [ref=3x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x04, 8 , 0, 0 }, // #39 [ref=33x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x04, 2 , 0, 0 }, // #40 [ref=30x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x04, 4 , 0, 0 }, // #41 [ref=40x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x00, 32, 0, 0 }, // #42 [ref=4x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x02, 8 , InstDB::RWInfoRm::kFlagAmbiguous, 0 }, // #43 [ref=1x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x02, 4 , InstDB::RWInfoRm::kFlagAmbiguous, 0 }, // #44 [ref=1x]
  { InstDB::RWInfoRm::kCategoryHalf      , 0x02, 0 , 0, 0 }, // #45 [ref=19x]
  { InstDB::RWInfoRm::kCategoryQuarter   , 0x02, 0 , 0, 0 }, // #46 [ref=9x]
  { InstDB::RWInfoRm::kCategoryHalf      , 0x01, 0 , 0, 0 }, // #47 [ref=10x]
  { InstDB::RWInfoRm::kCategoryConsistent, 0x04, 0 , InstDB::RWInfoRm::kFlagAmbiguous, 0 }, // #48 [ref=6x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x04, 16, 0, 0 }, // #49 [ref=30x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x01, 16, 0, 0 }, // #50 [ref=6x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x01, 32, 0, 0 }, // #51 [ref=4x]
  { InstDB::RWInfoRm::kCategoryConsistent, 0x0C, 0 , 0, 0 }, // #52 [ref=15x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x0C, 8 , 0, 0 }, // #53 [ref=4x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x0C, 4 , 0, 0 }, // #54 [ref=4x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x04, 32, 0, 0 }, // #55 [ref=6x]
  { InstDB::RWInfoRm::kCategoryConsistent, 0x03, 0 , 0, 0 }, // #56 [ref=13x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x03, 8 , InstDB::RWInfoRm::kFlagAmbiguous, 0 }, // #57 [ref=1x]
  { InstDB::RWInfoRm::kCategoryConsistent, 0x08, 0 , 0, 0 }, // #58 [ref=2x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x04, 1 , 0, 0 }, // #59 [ref=1x]
  { InstDB::RWInfoRm::kCategoryQuarter   , 0x01, 0 , 0, 0 }, // #60 [ref=6x]
  { InstDB::RWInfoRm::kCategoryEighth    , 0x01, 0 , 0, 0 }, // #61 [ref=3x]
  { InstDB::RWInfoRm::kCategoryEighth    , 0x02, 0 , 0, 0 }, // #62 [ref=2x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x0C, 16, 0, 0 }, // #63 [ref=1x]
  { InstDB::RWInfoRm::kCategoryFixed     , 0x06, 16, 0, 0 }, // #64 [ref=12x]
  { InstDB::RWInfoRm::kCategoryConsistent, 0x06, 0 , InstDB::RWInfoRm::kFlagFeatureIfRMI, uint32_t(CpuFeatures::X86::kAVX512_F) }, // #65 [ref=5x]
  { InstDB::RWInfoRm::kCategoryConsistent, 0x02, 0 , InstDB::RWInfoRm::kFlagFeatureIfRMI, uint32_t(CpuFeatures::X86::kAVX512_BW) }, // #66 [ref=2x]
  { InstDB::RWInfoRm::kCategoryConsistent, 0x06, 0 , InstDB::RWInfoRm::kFlagFeatureIfRMI, uint32_t(CpuFeatures::X86::kAVX512_BW) }  // #67 [ref=3x]
};
// ----------------------------------------------------------------------------
// ${InstRWInfoTable:End}

ASMJIT_END_SUB_NAMESPACE

#endif // !ASMJIT_NO_X86
