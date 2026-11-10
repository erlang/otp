// This file is part of AsmJit project <https://asmjit.com>
//
// See <asmjit/core.h> or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#include <asmjit/core/api_build_test_p.h>
#if defined(ASMJIT_TEST)

#include <asmjit/axl/arena.h>
#include <asmjit/axl/arena_bit_set_p.h>

ASMJIT_BEGIN_NAMESPACE

TEST_CASE(axl_arena_bit_set) {
  axl::Arena arena(8192);

  uint32_t i, count;
  uint32_t kMaxCount = 100;

  axl::ArenaBitSet vec;
  EXPECT_TRUE(vec.is_empty());
  EXPECT_EQ(vec.size(), 0u);

  TEST_LOG("ArenaBitSet::resize()");
  for (count = 1; count < kMaxCount; count++) {
    vec.clear();
    EXPECT_EQ(vec.resize(arena, count, false), axl::kSuccess);
    EXPECT_EQ(vec.size(), count);

    for (i = 0; i < count; i++) {
      EXPECT_FALSE(vec.bit_at(i));
    }

    vec.clear();
    EXPECT_EQ(vec.resize(arena, count, true), axl::kSuccess);
    EXPECT_EQ(vec.size(), count);

    for (i = 0; i < count; i++) {
      EXPECT_TRUE(vec.bit_at(i));
    }
  }

  TEST_LOG("ArenaBitSet::fill_bits() / clear_bits()");
  for (count = 1; count < kMaxCount; count += 2) {
    vec.clear();
    EXPECT_EQ(vec.resize(arena, count), axl::kSuccess);
    EXPECT_EQ(vec.size(), count);

    for (i = 0; i < (count + 1) / 2; i++) {
      bool value = bool(i & 1);
      if (value) {
        vec.fill_bits(i, count - i * 2);
      }
      else {
        vec.clear_bits(i, count - i * 2);
      }
    }

    for (i = 0; i < count; i++) {
      EXPECT_EQ(vec.bit_at(i), bool(i & 1));
    }
  }
}

ASMJIT_END_NAMESPACE

#endif // ASMJIT_TEST
