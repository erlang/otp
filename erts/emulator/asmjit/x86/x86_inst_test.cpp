// This file is part of AsmJit project <https://asmjit.com>
//
// See <asmjit/core.h> or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#include <asmjit/core/api_build_test_p.h>
#if defined(ASMJIT_TEST) && !defined(ASMJIT_NO_X86)

#include <asmjit/core/cpu_info.h>
#include <asmjit/core/globals.h>
#include <asmjit/core/misc_p.h>
#include <asmjit/x86/x86_inst_api_p.h>
#include <asmjit/x86/x86_inst_db_p.h>
#include <asmjit/x86/x86_opcode_p.h>
#include <asmjit/x86/x86_operand.h>

ASMJIT_BEGIN_SUB_NAMESPACE(x86)

TEST_CASE(x86_inst_db) {
  TEST_LOG("Checking validity of Inst enums");

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

#if !defined(ASMJIT_NO_TEXT)
TEST_CASE(x86_inst_api_text) {
  // All known instructions should be matched.
  TEST_LOG("Matching all X86 instructions");
  for (uint32_t a = 1; a < Inst::_kIdCount; a++) {
    StringTmp<128> a_name;
    EXPECT_EQ(InstInternal::inst_id_to_string(a, InstStringifyOptions::kNone, a_name), Error::kOk)
      .message("Failed to get the name of instruction #%u", a);

    uint32_t b = InstInternal::string_to_inst_id(a_name.data(), a_name.size());
    StringTmp<128> b_name;
    InstInternal::inst_id_to_string(b, InstStringifyOptions::kNone, b_name);
    EXPECT_EQ(a, b)
      .message("Instructions do not match \"%s\" (#%u) != \"%s\" (#%u)", a_name.data(), a, b_name.data(), b);
  }
}
#endif // !ASMJIT_NO_TEXT

#if !defined(ASMJIT_NO_INTROSPECTION)
template<typename... Args>
static Error query_features_inline(CpuFeatures* out, Arch arch, BaseInst inst, Args&&... args) {
  Operand_ op_array[] = { axl::forward<Args>(args)... };
  return InstInternal::query_features(arch, inst, op_array, sizeof...(args), out);
}

TEST_CASE(x86_inst_api_cpu_features) {
  TEST_LOG("Verifying whether SSE2+ features are reported correctly for legacy instructions");
  {
    CpuFeatures f;

    query_features_inline(&f, Arch::kX64, BaseInst(Inst::kIdPaddd), xmm1, xmm2);
    EXPECT_TRUE(f.x86().has_sse2());

    query_features_inline(&f, Arch::kX64, BaseInst(Inst::kIdAddsubpd), xmm1, xmm2);
    EXPECT_TRUE(f.x86().has_sse3());

    query_features_inline(&f, Arch::kX64, BaseInst(Inst::kIdPshufb), xmm1, xmm2);
    EXPECT_TRUE(f.x86().has_ssse3());

    query_features_inline(&f, Arch::kX64, BaseInst(Inst::kIdBlendpd), xmm1, xmm2, Imm(1));
    EXPECT_TRUE(f.x86().has_sse4_1());

    query_features_inline(&f, Arch::kX64, BaseInst(Inst::kIdCrc32), eax, al);
    EXPECT_TRUE(f.x86().has_sse4_2());
  }

  TEST_LOG("Verifying whether AVX+ features are reported correctly for AVX instructions");
  {
    CpuFeatures f;

    query_features_inline(&f, Arch::kX64, BaseInst(Inst::kIdVpaddd), xmm1, xmm2, xmm3);
    EXPECT_TRUE(f.x86().has_avx());

    query_features_inline(&f, Arch::kX64, BaseInst(Inst::kIdVpaddd), ymm1, ymm2, ymm3);
    EXPECT_TRUE(f.x86().has_avx2());

    query_features_inline(&f, Arch::kX64, BaseInst(Inst::kIdVaddsubpd), xmm1, xmm2, xmm3);
    EXPECT_TRUE(f.x86().has_avx());

    query_features_inline(&f, Arch::kX64, BaseInst(Inst::kIdVaddsubpd), ymm1, ymm2, ymm3);
    EXPECT_TRUE(f.x86().has_avx());

    query_features_inline(&f, Arch::kX64, BaseInst(Inst::kIdVpshufb), xmm1, xmm2, xmm3);
    EXPECT_TRUE(f.x86().has_avx());

    query_features_inline(&f, Arch::kX64, BaseInst(Inst::kIdVpshufb), ymm1, ymm2, ymm3);
    EXPECT_TRUE(f.x86().has_avx2());

    query_features_inline(&f, Arch::kX64, BaseInst(Inst::kIdVblendpd), xmm1, xmm2, xmm3, Imm(1));
    EXPECT_TRUE(f.x86().has_avx());

    query_features_inline(&f, Arch::kX64, BaseInst(Inst::kIdVblendpd), ymm1, ymm2, ymm3, Imm(1));
    EXPECT_TRUE(f.x86().has_avx());

    query_features_inline(&f, Arch::kX64, BaseInst(Inst::kIdVpunpcklbw), xmm1, xmm2, xmm3);
    EXPECT_TRUE(f.x86().has_avx());

    query_features_inline(&f, Arch::kX64, BaseInst(Inst::kIdVpunpcklbw), ymm1, ymm2, ymm3);
    EXPECT_TRUE(f.x86().has_avx2());
  }

  TEST_LOG("Verifying whether AVX2 / AVX512 features are reported correctly for vpgatherxx instructions");
  {
    CpuFeatures f;

    query_features_inline(&f, Arch::kX64, BaseInst(Inst::kIdVpgatherdd), xmm1, ptr(rax, xmm2), xmm3);
    EXPECT_TRUE(f.x86().has_avx2());
    EXPECT_FALSE(f.x86().has_avx512_f());

    // NOTE: This instruction is unencodable, but sometimes this signature is used to check the support (without the {k}).
    query_features_inline(&f, Arch::kX64, BaseInst(Inst::kIdVpgatherdd), xmm1, ptr(rax, xmm2));
    EXPECT_FALSE(f.x86().has_avx2());
    EXPECT_TRUE(f.x86().has_avx512_f());

    query_features_inline(&f, Arch::kX64, BaseInst(Inst::kIdVpgatherdd, InstOptions::kNone, k1), xmm1, ptr(rax, xmm2));
    EXPECT_FALSE(f.x86().has_avx2());
    EXPECT_TRUE(f.x86().has_avx512_f());
  }
}
#endif // !ASMJIT_NO_INTROSPECTION

#if !defined(ASMJIT_NO_INTROSPECTION)
template<typename... Args>
static Error query_rw_info_inline(InstRWInfo* out, Arch arch, BaseInst inst, Args&&... args) {
  Operand_ op_array[] = { axl::forward<Args>(args)... };
  return InstInternal::query_rw_info(arch, inst, op_array, sizeof...(args), out);
}

TEST_CASE(x86_inst_api_rm_features) {
  TEST_LOG("Verifying whether RM/feature is reported correctly for PEXTRW instruction");
  {
    InstRWInfo rwi;

    query_rw_info_inline(&rwi, Arch::kX64, BaseInst(Inst::kIdPextrw), eax, mm1, imm(1));
    EXPECT_EQ(rwi.rm_feature(), 0u);

    query_rw_info_inline(&rwi, Arch::kX64, BaseInst(Inst::kIdPextrw), eax, xmm1, imm(1));
    EXPECT_EQ(rwi.rm_feature(), CpuFeatures::X86::kSSE4_1);
  }

  TEST_LOG("Verifying whether RM/feature is reported correctly for AVX512 shift instructions");
  {
    InstRWInfo rwi;

    query_rw_info_inline(&rwi, Arch::kX64, BaseInst(Inst::kIdVpslld), xmm1, xmm2, imm(8));
    EXPECT_EQ(rwi.rm_feature(), CpuFeatures::X86::kAVX512_F);

    query_rw_info_inline(&rwi, Arch::kX64, BaseInst(Inst::kIdVpsllq), ymm1, ymm2, imm(8));
    EXPECT_EQ(rwi.rm_feature(), CpuFeatures::X86::kAVX512_F);

    query_rw_info_inline(&rwi, Arch::kX64, BaseInst(Inst::kIdVpsrad), xmm1, xmm2, imm(8));
    EXPECT_EQ(rwi.rm_feature(), CpuFeatures::X86::kAVX512_F);

    query_rw_info_inline(&rwi, Arch::kX64, BaseInst(Inst::kIdVpsrld), ymm1, ymm2, imm(8));
    EXPECT_EQ(rwi.rm_feature(), CpuFeatures::X86::kAVX512_F);

    query_rw_info_inline(&rwi, Arch::kX64, BaseInst(Inst::kIdVpsrlq), xmm1, xmm2, imm(8));
    EXPECT_EQ(rwi.rm_feature(), CpuFeatures::X86::kAVX512_F);

    query_rw_info_inline(&rwi, Arch::kX64, BaseInst(Inst::kIdVpslldq), xmm1, xmm2, imm(8));
    EXPECT_EQ(rwi.rm_feature(), CpuFeatures::X86::kAVX512_BW);

    query_rw_info_inline(&rwi, Arch::kX64, BaseInst(Inst::kIdVpsllw), ymm1, ymm2, imm(8));
    EXPECT_EQ(rwi.rm_feature(), CpuFeatures::X86::kAVX512_BW);

    query_rw_info_inline(&rwi, Arch::kX64, BaseInst(Inst::kIdVpsraw), xmm1, xmm2, imm(8));
    EXPECT_EQ(rwi.rm_feature(), CpuFeatures::X86::kAVX512_BW);

    query_rw_info_inline(&rwi, Arch::kX64, BaseInst(Inst::kIdVpsrldq), ymm1, ymm2, imm(8));
    EXPECT_EQ(rwi.rm_feature(), CpuFeatures::X86::kAVX512_BW);

    query_rw_info_inline(&rwi, Arch::kX64, BaseInst(Inst::kIdVpsrlw), xmm1, xmm2, imm(8));
    EXPECT_EQ(rwi.rm_feature(), CpuFeatures::X86::kAVX512_BW);

    query_rw_info_inline(&rwi, Arch::kX64, BaseInst(Inst::kIdVpslld), xmm1, xmm2, xmm3);
    EXPECT_EQ(rwi.rm_feature(), 0u);

    query_rw_info_inline(&rwi, Arch::kX64, BaseInst(Inst::kIdVpsllw), xmm1, xmm2, xmm3);
    EXPECT_EQ(rwi.rm_feature(), 0u);
  }
}
#endif // !ASMJIT_NO_INTROSPECTION

ASMJIT_END_SUB_NAMESPACE

#endif // ASMJIT_TEST && !ASMJIT_NO_X86
