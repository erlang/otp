// This file is part of AsmJit project <https://asmjit.com>
//
// See <asmjit/core.h> or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#include <asmjit/core/api_build_test_p.h>
#if !defined(ASMJIT_NO_AARCH64)

#include <asmjit/core/arch_traits.h>
#include <asmjit/arm/a64_arch_traits_p.h>
#include <asmjit/arm/a64_operand.h>

ASMJIT_BEGIN_SUB_NAMESPACE(Tests)

TEST_CASE(a64_arch_traits) {
  const ArchTraits& traits = ArchTraits::by_arch(Arch::kAArch64);
  EXPECT_EQ(memcmp(&traits, &a64::a64_arch_traits, sizeof(ArchTraits)), 0);

  EXPECT_EQ(traits.fp_reg_id(), a64::Gp::kIdFp);
  EXPECT_EQ(traits.sp_reg_id(), a64::Gp::kIdSp);
    EXPECT_EQ(traits.hw_stack_alignment(), 16u);

  EXPECT_TRUE(traits.has_inst_push_pop(RegGroup::kGp));
  EXPECT_FALSE(traits.has_inst_reg_swap(RegGroup::kGp));

  EXPECT_TRUE(traits.has_inst_push_pop(RegGroup::kVec));
  EXPECT_FALSE(traits.has_inst_reg_swap(RegGroup::kVec));

  EXPECT_TRUE(traits.has_link_reg());
}

ASMJIT_END_SUB_NAMESPACE

#endif // !ASMJIT_NO_AARCH64
