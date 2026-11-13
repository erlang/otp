// This file is part of AsmJit project <https://asmjit.com>
//
// See <asmjit/core.h> or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#include <asmjit/core/api_build_test_p.h>
#if defined(ASMJIT_TEST) && !defined(ASMJIT_NO_X86) && !defined(ASMJIT_NO_LOGGING)

#include <asmjit/axl/commons.h>
#include <asmjit/x86/x86_formatter_p.h>
#include <asmjit/x86/x86_operand.h>

ASMJIT_BEGIN_NAMESPACE

TEST_CASE(x86_formatter_cpu_features) {
  using namespace x86;
  auto expect_feature = [](const char* str, uint32_t feature_id) {
    String sb;
    FormatterInternal::format_feature(sb, feature_id);
    EXPECT_EQ(sb, Span<const char>(str, strlen(str)));
  };

  expect_feature("AVX2", CpuFeatures::X86::kAVX2);
  expect_feature("AVX512_BITALG", CpuFeatures::X86::kAVX512_BITALG);
  expect_feature("BMI2", CpuFeatures::X86::kBMI2);
  expect_feature("SSSE3", CpuFeatures::X86::kSSSE3);
  expect_feature("XSAVES", CpuFeatures::X86::kXSAVES);
}

TEST_CASE(x86_formatter_operands) {
  using namespace x86;
  Arch arch = Arch::kX64;

  auto expect_operand = [&](const char* str, const Operand& op) {
    String sb;
    FormatterInternal::format_operand(sb, FormatFlags::kNone, nullptr, arch, op);
    EXPECT_EQ(sb, Span<const char>(str, strlen(str)));
  };

  auto expect_operand_f = [&](const char* str, FormatFlags flags, const Operand& op) {
    String sb;
    FormatterInternal::format_operand(sb, flags, nullptr, arch, op);
    EXPECT_EQ(sb, Span<const char>(str, strlen(str)));
  };

  expect_operand("al", al);
  expect_operand("ah", ah);
  expect_operand("bl", bl);
  expect_operand("bh", bh);
  expect_operand("cl", cl);
  expect_operand("ch", ch);
  expect_operand("dl", dl);
  expect_operand("dh", dh);
  expect_operand("sil", sil);
  expect_operand("dil", dil);
  expect_operand("r15b", r15b);
  expect_operand("r13b", gp8(13));
  expect_operand("bh", gp8_hi(3));

  expect_operand("ax", ax);
  expect_operand("bx", bx);
  expect_operand("cx", cx);
  expect_operand("dx", dx);
  expect_operand("si", si);
  expect_operand("di", di);
  expect_operand("r11w", gp16(11));

  expect_operand("eax", eax);
  expect_operand("ebx", ebx);
  expect_operand("ecx", ecx);
  expect_operand("edx", edx);
  expect_operand("esi", esi);
  expect_operand("edi", edi);
  expect_operand("r10d", gp32(10));

  expect_operand("rax", rax);
  expect_operand("rbx", rbx);
  expect_operand("rcx", rcx);
  expect_operand("rdx", rdx);
  expect_operand("rsi", rsi);
  expect_operand("rdi", rdi);
  expect_operand("r11", r11);
  expect_operand("r14", gp64(14));

  expect_operand("xmm5", xmm5);
  expect_operand("xmm5", xmm(5));
  expect_operand("ymm20", ymm20);
  expect_operand("ymm21", ymm(21));
  expect_operand("zmm30", zmm30);
  expect_operand("zmm31", zmm(31));

  expect_operand("st1", st1);
  expect_operand("st7", st(7));

  expect_operand("mm1", mm1);
  expect_operand("mm7", mm(7));

  expect_operand("k1", k1);
  expect_operand("k7", k(7));

  expect_operand("cr0", cr0);
  expect_operand("cr1", cr(1));

  expect_operand("dr0", dr0);
  expect_operand("dr1", dr(1));

  expect_operand("tmm0", tmm0);
  expect_operand("tmm1", tmm(1));

  expect_operand("rip", rip);

  expect_operand("[rax]", ptr(rax));
  expect_operand("[rax+129]", ptr(rax, 129));

  expect_operand("byte ptr [rax]", byte_ptr(rax));
  expect_operand("byte ptr [rax+1]", byte_ptr(rax, 1));
  expect_operand("word ptr [rax]", word_ptr(rax));
  expect_operand("word ptr [rax+1]", word_ptr(rax, 1));
  expect_operand("dword ptr [rax]", dword_ptr(rax));
  expect_operand("dword ptr [rax+1]", dword_ptr(rax, 1));
  expect_operand("qword ptr [rax]", qword_ptr(rax));
  expect_operand("qword ptr [rax+1]", qword_ptr(rax, 1));

  expect_operand("byte ptr [rax]", ptr_8(rax));
  expect_operand("byte ptr [rax+1]", ptr_8(rax, 1));
  expect_operand("word ptr [rax]", ptr_16(rax));
  expect_operand("word ptr [rax+1]", ptr_16(rax, 1));
  expect_operand("dword ptr [rax]", ptr_32(rax));
  expect_operand("dword ptr [rax+1]", ptr_32(rax, 1));
  expect_operand("qword ptr [rax]", ptr_64(rax));
  expect_operand("qword ptr [rax+1]", ptr_64(rax, 1));

  expect_operand("[eax+ecx]", ptr(eax, ecx));
  expect_operand("[rax+rcx]", ptr(rax, rcx));
  expect_operand("[rax+rcx+129]", ptr(rax, rcx, 0, 129));

  expect_operand("[rax+rcx*2]", ptr(rax, rcx, 1));
  expect_operand("[rax+rcx*2+129]", ptr(rax, rcx, 1, 129));

  expect_operand("qword ptr [rax+rcx]", qword_ptr(rax, rcx));
  expect_operand("qword ptr [rax+rcx+129]", qword_ptr(rax, rcx, 0, 129));

  expect_operand("qword ptr [rax+rcx*2]", qword_ptr(rax, rcx, 1));
  expect_operand("qword ptr [rax+rcx*2+129]", qword_ptr(rax, rcx, 1, 129));
  expect_operand_f("qword ptr [rax+rcx*2+0x80]", FormatFlags::kHexOffsets, qword_ptr(rax, rcx, 1, 128));

  expect_operand("[rax+zmm0]", ptr(rax, zmm0));
  expect_operand("[rax+rcx] {1to4}", ptr(rax, rcx)._1to4());
  expect_operand("[rax+rcx] {1to16}", ptr(rax, rcx)._1to16());
  expect_operand("[rax+rcx] {1to64}", ptr(rax, rcx)._1to64());

  expect_operand("0", imm(0));
  expect_operand("1", imm(1));
  expect_operand("-1", imm(-1));
  expect_operand("65535", imm(0xFFFF));
  expect_operand("100000000", imm(100000000));

  expect_operand_f("0", FormatFlags::kHexImms, imm(0));
  expect_operand_f("9", FormatFlags::kHexImms, imm(9));
  expect_operand_f("0xA", FormatFlags::kHexImms, imm(10));
  expect_operand_f("0xFFFF", FormatFlags::kHexImms, imm(0xFFFF));
  expect_operand_f("0xFFFFFFFF", FormatFlags::kHexImms, imm(0xFFFFFFFF));
}

template<typename... Args>
static void expect_formatted_inst(const char* str, FormatFlags flags, Arch arch, BaseInst base_inst, Args&&... args) noexcept {
  String sb;

  Operand_ operands[Globals::kMaxOpCount] = {axl::forward<Args>(args)...};
  EXPECT_OK(x86::FormatterInternal::format_instruction(sb, flags, nullptr, arch, base_inst, {operands, sizeof...(Args)}));
  EXPECT_EQ(sb, Span<const char>(str, strlen(str)));
};

TEST_CASE(x86_formatter_instructions) {
  using namespace x86;

  constexpr FormatFlags nf = FormatFlags::kNone;
  constexpr FormatFlags xi = FormatFlags::kExplainImms;

  Arch arch = Arch::kX64;
  expect_formatted_inst("ud2", nf, arch, BaseInst(Inst::kIdUd2));
  expect_formatted_inst("mov eax, edi", nf, arch, BaseInst(Inst::kIdMov), eax, edi);
  expect_formatted_inst("add eax, ebx", nf, arch, BaseInst(Inst::kIdAdd), eax, ebx);
  expect_formatted_inst("lock add [rax], ebx", nf, arch, BaseInst(Inst::kIdAdd, InstOptions::kX86_Lock), ptr(rax), ebx);
  expect_formatted_inst("vpaddb zmm0, zmm2, zmm31", nf, arch, BaseInst(Inst::kIdVpaddb), zmm0, zmm2, zmm31);
  expect_formatted_inst("vpaddb zmm0 {k1}, zmm2, zmm31", nf, arch, BaseInst(Inst::kIdVpaddb, InstOptions::kNone, k1), zmm0, zmm2, zmm31);
  expect_formatted_inst("vpaddb zmm0 {k1}{z}, zmm2, zmm31", nf, arch, BaseInst(Inst::kIdVpaddb, InstOptions::kX86_ZMask, k1), zmm0, zmm2, zmm31);

  expect_formatted_inst("cmppd xmm0, xmm1, 4 /*NEQ_UQ*/", xi, arch, BaseInst(Inst::kIdCmppd), xmm0, xmm1, imm(CmpImm::kNEQ));
  expect_formatted_inst("pshufd xmm0, xmm1, 255 /*3|3|3|3*/", xi, arch, BaseInst(Inst::kIdPshufd), xmm0, xmm1, imm(0xFF));
  expect_formatted_inst("roundss xmm0, xmm1, 8 /*ROUND|SUPPRESS*/", xi, arch, BaseInst(Inst::kIdRoundss), xmm0, xmm1, imm(RoundImm::kNearest | RoundImm::kSuppress));
  expect_formatted_inst("vblendps xmm5, xmm1, xmm6, 3 /*0|0|1|1*/", xi, arch, BaseInst(Inst::kIdVblendps), xmm5, xmm1, xmm6, imm(0x3));
  expect_formatted_inst("vfixupimmpd zmm3, zmm31, zmm30, 8 /*SNAN_IE*/, {sae}", xi, arch, BaseInst(Inst::kIdVfixupimmpd, InstOptions::kX86_SAE), zmm3, zmm31, zmm30, imm(VFixupImm::kIEOnOne));
  expect_formatted_inst("vfnmsub231sd xmm5, xmm4, xmm2, {rn-sae}", xi, arch, BaseInst(Inst::kIdVfnmsub231sd, InstOptions::kX86_ER | InstOptions::kX86_RN_SAE), xmm5, xmm4, xmm2);
  expect_formatted_inst("vpcomb xmm0, xmm1, xmm2, 3 /*GE*/", xi, arch, BaseInst(Inst::kIdVpcomb), xmm0, xmm1, xmm2, imm(VPComImm::kGE));
  expect_formatted_inst("vpcomuw xmm0, xmm1, xmm2, 2 /*GT*/", xi, arch, BaseInst(Inst::kIdVpcomuw), xmm0, xmm1, xmm2, imm(VPComImm::kGT));
  expect_formatted_inst("vpcomd xmm0, xmm1, xmm2, 1 /*LE*/", xi, arch, BaseInst(Inst::kIdVpcomd), xmm0, xmm1, xmm2, imm(VPComImm::kLE));
  expect_formatted_inst("vpshufd xmm0, xmm1, 255 /*3|3|3|3*/", xi, arch, BaseInst(Inst::kIdVpshufd), xmm0, xmm1, imm(0xFF));
  expect_formatted_inst("vpternlogd zmm0, zmm1, zmm31, 62 /*0|0|1|1|1|1|1|0*/", xi, arch, BaseInst(Inst::kIdVpternlogd), zmm0, zmm1, zmm31, imm(0x3E));
  expect_formatted_inst("vpternlogq zmm5, zmm4, zmm11, 24 /*0|0|0|1|1|0|0|0*/", xi, arch, BaseInst(Inst::kIdVpternlogq), zmm5, zmm4, zmm11, imm(0x18));
}

ASMJIT_END_NAMESPACE

#endif // ASMJIT_TEST && !ASMJIT_NO_X86 && !ASMJIT_NO_LOGGING
