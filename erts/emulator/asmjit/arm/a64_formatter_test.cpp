// This file is part of AsmJit project <https://asmjit.com>
//
// See <asmjit/core.h> or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#include <asmjit/core/api_build_test_p.h>
#if defined(ASMJIT_TEST) && !defined(ASMJIT_NO_AARCH64) && !defined(ASMJIT_NO_LOGGING)

#include <asmjit/axl/commons.h>
#include <asmjit/arm/a64_formatter_p.h>
#include <asmjit/arm/a64_operand.h>

ASMJIT_BEGIN_NAMESPACE

TEST_CASE(a64_formatter_cpu_features) {
  auto expect_feature = [](const char* str, uint32_t feature_id) {
    String sb;
    arm::FormatterInternal::format_feature(sb, feature_id);
    EXPECT_EQ(sb, Span<const char>(str, strlen(str)));
  };

  expect_feature("AES", CpuFeatures::ARM::kAES);
  expect_feature("CLRBHB", CpuFeatures::ARM::kCLRBHB);
  expect_feature("LS64WB", CpuFeatures::ARM::kLS64WB);
  expect_feature("SME_B16F32", CpuFeatures::ARM::kSME_B16F32);
  expect_feature("XS", CpuFeatures::ARM::kXS);
}

TEST_CASE(a64_formatter_operands) {
  using namespace a64;
  Arch arch = Arch::kAArch64;

  auto expect_operand = [&](const char* str, const Operand& op) {
    String sb;
    arm::FormatterInternal::format_operand(sb, FormatFlags::kNone, nullptr, arch, op);
    EXPECT_EQ(sb, Span<const char>(str, strlen(str)));
  };

  auto expect_operand_f = [&](const char* str, FormatFlags flags, const Operand& op) {
    String sb;
    arm::FormatterInternal::format_operand(sb, flags, nullptr, arch, op);
    EXPECT_EQ(sb, Span<const char>(str, strlen(str)));
  };

  expect_operand("w0", w0);
  expect_operand("x0", x0);
  expect_operand("w23", w(23));
  expect_operand("x24", x(24));

  expect_operand("b1", b1);
  expect_operand("b11", b(11));
  expect_operand("h1", h1);
  expect_operand("h12", h(12));
  expect_operand("s1", s1);
  expect_operand("s13", s(13));
  expect_operand("d1", d1);
  expect_operand("d14", d(14));
  expect_operand("q1", q1);
  expect_operand("q15", q(15));
  expect_operand("q1", v1);
  expect_operand("q31", v(31));

  expect_operand("v1.8b", v1.b8());
  expect_operand("v2.16b", v2.b16());
  expect_operand("v3.4h", v3.h4());
  expect_operand("v4.8h", v4.h8());
  expect_operand("v5.2s", v5.s2());
  expect_operand("v6.4s", v6.s4());
  expect_operand("v7.2d", v7.d2());

  expect_operand("v1.b[15]", v1.b(15));
  expect_operand("v2.h[7]", v2.h(7));
  expect_operand("v3.s[3]", v3.s(3));
  expect_operand("v4.d[1]", v4.d(1));

  expect_operand("[x0]", ptr(x0));
  expect_operand("[x0, 128]", ptr(x0, 128));
  expect_operand("[x0, 128]!", ptr_pre(x0, 128));
  expect_operand("[x0], 128", ptr_post(x0, 128));

  expect_operand("[x0, x1]", ptr(x0, x1));
  expect_operand("[x0, x1]!", ptr_pre(x0, x1));
  expect_operand("[x0], x1", ptr_post(x0, x1));

  expect_operand("#0", imm(0));
  expect_operand("#1", imm(1));
  expect_operand("#-1", imm(-1));
  expect_operand("#65535", imm(0xFFFF));
  expect_operand("#100000000", imm(100000000));

  expect_operand_f("#0", FormatFlags::kHexImms, imm(0));
  expect_operand_f("#9", FormatFlags::kHexImms, imm(9));
  expect_operand_f("#0xA", FormatFlags::kHexImms, imm(10));
  expect_operand_f("#0xFFFF", FormatFlags::kHexImms, imm(0xFFFF));
  expect_operand_f("#0xFFFFFFFF", FormatFlags::kHexImms, imm(0xFFFFFFFF));
}

template<typename... Args>
static void expect_formatted_inst(const char* str, FormatFlags flags, Arch arch, BaseInst base_inst, Args&&... args) noexcept {
  String sb;
  Operand_ operands[Globals::kMaxOpCount] = {axl::forward<Args>(args)...};

  a64::FormatterInternal::format_instruction(sb, flags, nullptr, arch, base_inst, {operands, sizeof...(Args)});
  EXPECT_EQ(sb, Span<const char>(str, strlen(str)));
};

TEST_CASE(x86_formatter_instructions) {
  using namespace a64;
  FormatFlags nf = FormatFlags::kNone;

  Arch arch = Arch::kAArch64;
  expect_formatted_inst("drps", nf, arch, BaseInst(Inst::kIdDrps));
  expect_formatted_inst("mov w1, #13", nf, arch, BaseInst(Inst::kIdMov), w1, imm(13));
  expect_formatted_inst("add w1, w2, w3", nf, arch, BaseInst(Inst::kIdAdd), w1, w2, w3);
  expect_formatted_inst("add x1, x2, x3", nf, arch, BaseInst(Inst::kIdAdd), x1, x2, x3);

  // TODO: Operand modifiers don't format as expected ATM - missing lsl, seen as regular immediate.
  // expect_formatted_inst("add w1, w2, w3, lsl #4", nf, arch, BaseInst(Inst::kIdAdd), w1, w2, w3, imm(lsl(4)));
  // expect_formatted_inst("add x1, x2, x3, lsl #4", nf, arch, BaseInst(Inst::kIdAdd), x1, x2, x3, imm(lsl(4)));
}

ASMJIT_END_NAMESPACE

#endif // ASMJIT_TEST && !ASMJIT_NO_X86 && !ASMJIT_NO_LOGGING
