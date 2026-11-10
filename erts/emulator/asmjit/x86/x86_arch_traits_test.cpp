// This file is part of AsmJit project <https://asmjit.com>
//
// See <asmjit/core.h> or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#include <asmjit/core/api_build_test_p.h>
#if !defined(ASMJIT_NO_X86)

#include <asmjit/core/arch_traits.h>
#include <asmjit/x86/x86_arch_traits_p.h>
#include <asmjit/x86/x86_operand.h>

ASMJIT_BEGIN_SUB_NAMESPACE(Tests)

TEST_CASE(x86_arch_traits) {
  const ArchTraits& x86 = ArchTraits::by_arch(Arch::kX86);
  const ArchTraits& x64 = ArchTraits::by_arch(Arch::kX64);

  EXPECT_EQ(memcmp(&x86, &x86::x86_arch_traits, sizeof(ArchTraits)), 0);
  EXPECT_EQ(memcmp(&x64, &x86::x64_arch_traits, sizeof(ArchTraits)), 0);

  for (const ArchTraits& traits : {x86, x64}) {
    EXPECT_EQ(traits.fp_reg_id(), x86::Gp::kIdBp);
    EXPECT_EQ(traits.sp_reg_id(), x86::Gp::kIdSp);
    EXPECT_EQ(traits.hw_stack_alignment(), 1u);

    EXPECT_TRUE(traits.has_inst_push_pop(RegGroup::kGp));
    EXPECT_TRUE(traits.has_inst_reg_swap(RegGroup::kGp));

    EXPECT_FALSE(traits.has_inst_push_pop(RegGroup::kVec));
    EXPECT_FALSE(traits.has_inst_reg_swap(RegGroup::kVec));

    EXPECT_FALSE(traits.has_link_reg());
  }
}

ASMJIT_END_SUB_NAMESPACE

#endif // !ASMJIT_NO_X86
