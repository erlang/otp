// This file is part of AsmJit project <https://asmjit.com>
//
// See <asmjit/core.h> or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#include <asmjit/core/api_build_test_p.h>
#if defined(ASMJIT_TEST)

#include <asmjit/axl/arena.h>
#include <asmjit/axl/commons.h>

ASMJIT_BEGIN_NAMESPACE

TEST_CASE(axl_arena_oneshot) {
  struct SomeData {
    size_t _x;
    size_t _y;

    inline SomeData(size_t x, size_t y) noexcept
      : _x(x), _y(y) {}
  };

  constexpr size_t kN = 100000u;

  {
    axl::Arena arena(4096u);

    for (size_t r = 0; r < 3u; r++) {
      for (size_t i = 0; i < kN; i++) {
        uint8_t* p = arena.alloc_oneshot<uint8_t>(32);
        EXPECT_NOT_NULL(p);
      }

      axl::ArenaUsage usage = arena.usage();
      EXPECT_GE(usage.block_count(), 2u);
      EXPECT_GE(usage.used_size(), kN * 32u);
      EXPECT_GE(usage.reserved_size(), kN * 32u);
      EXPECT_GE(usage.reserved_size(), usage.used_size());

      if (r == 0) {
        arena.clear();
      }
      else {
        arena.release();
      }
    }
  }

  {
    axl::Arena arena(4096u);

    for (size_t r = 0; r < 3u; r++) {
      for (size_t i = 0; i < kN; i++) {
        SomeData* p = arena.new_oneshot<SomeData>(r, i);
        EXPECT_NOT_NULL(p);
      }

      if (r == 0) {
        arena.clear();
      }
      else {
        arena.release();
      }
    }
  }
}

TEST_CASE(axl_arena_reusable_slots_check) {
  constexpr size_t kMinReusableSlotSize = axl::Arena::kMinReusableSlotSize;
  constexpr size_t kMaxReusableSlotSize = axl::Arena::kMaxReusableSlotSize;

  size_t expected_slot = 0;
  size_t expected_until = kMinReusableSlotSize;

  for (size_t size = 1; size <= kMaxReusableSlotSize; size++) {
    size_t acquired_slot;

    EXPECT_TRUE(axl::Arena::_get_reusable_slot_index(size, Out(acquired_slot)));
    EXPECT_EQ(acquired_slot, expected_slot);
    EXPECT_LT(acquired_slot, axl::Arena::kReusableSlotCount);

    if (size == expected_until) {
      expected_slot++;
      expected_until *= 2;
    }
  }
}

ASMJIT_END_NAMESPACE

#endif // ASMJIT_TEST
