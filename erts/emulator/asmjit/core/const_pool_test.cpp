// This file is part of AsmJit project <https://asmjit.com>
//
// See <asmjit/core.h> or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#include <asmjit/core/api_build_test_p.h>
#if defined(ASMJIT_TEST)

#include <asmjit/core/const_pool.h>
#include <asmjit/core/globals.h>

ASMJIT_BEGIN_NAMESPACE

TEST_CASE(core_const_pool) {
  axl::Arena arena(32u * 1024u);
  ConstPool pool(arena);

  uint32_t i;
  uint32_t kCount = ZeroTest::test_runner()->cmd_line().has_arg("--quick") ? 1000 : 1000000;

  TEST_LOG("Adding %u constants to the pool", kCount);
  {
    size_t prev_offset;
    size_t cur_offset;
    uint64_t c = 0x0101010101010101u;

    EXPECT_OK(pool.add(&c, 8, Out(prev_offset)));
    EXPECT_EQ(prev_offset, 0u);

    for (i = 1; i < kCount; i++) {
      c++;
      EXPECT_OK(pool.add(&c, 8, Out(cur_offset)));
      EXPECT_EQ(prev_offset + 8, cur_offset);
      EXPECT_EQ(pool.size(), (i + 1) * 8);
      prev_offset = cur_offset;
    }

    EXPECT_EQ(pool.alignment(), 8u);
  }

  TEST_LOG("Retrieving %u constants from the pool", kCount);
  {
    uint64_t c = 0x0101010101010101u;

    for (i = 0; i < kCount; i++) {
      size_t offset;
      EXPECT_OK(pool.add(&c, 8, Out(offset)));
      EXPECT_EQ(offset, i * 8);
      c++;
    }
  }

  TEST_LOG("Checking if the constants were split into 4-byte patterns");
  {
    uint32_t c = 0x01010101u;
    size_t offset;

    EXPECT_OK(pool.add(&c, 4, Out(offset)));
    EXPECT_EQ(offset, 0u);

    // NOTE: We have to adjust the offset to successfully test this on big endian architectures.
    size_t base_offset = size_t(axl::kIsBigEndian ? 4 : 0);

    for (i = 1; i < kCount; i++) {
      c++;
      EXPECT_OK(pool.add(&c, 4, Out(offset)));
      EXPECT_EQ(offset, base_offset + i * 8);
    }
  }

  TEST_LOG("Adding 2 byte constant to misalign the current offset");
  {
    uint16_t c = 0xFFFF;
    size_t offset;

    EXPECT_OK(pool.add(&c, 2, Out(offset)));
    EXPECT_EQ(offset, kCount * 8);
    EXPECT_EQ(pool.alignment(), 8u);
  }

  TEST_LOG("Adding 8 byte constant to check if pool gets aligned again");
  {
    uint64_t c = 0xFFFFFFFFFFFFFFFFu;
    size_t offset;

    EXPECT_OK(pool.add(&c, 8, Out(offset)));
    EXPECT_EQ(offset, kCount * 8 + 8u);
  }

  TEST_LOG("Adding 2 byte constant to verify the gap is filled");
  {
    uint16_t c = 0xFFFE;
    size_t offset;

    EXPECT_OK(pool.add(&c, 2, Out(offset)));
    EXPECT_EQ(offset, kCount * 8 + 2);
    EXPECT_EQ(pool.alignment(), 8u);
  }

  TEST_LOG("Checking reset functionality");
  {
    pool.reset();
    arena.clear();

    EXPECT_EQ(pool.size(), 0u);
    EXPECT_EQ(pool.alignment(), 0u);
  }

  TEST_LOG("Checking pool alignment when combined constants are added");
  {
    uint8_t bytes[32] = { 0 };
    size_t offset;

    pool.add(bytes, 1, Out(offset));
    EXPECT_EQ(pool.size(), 1u);
    EXPECT_EQ(pool.alignment(), 1u);
    EXPECT_EQ(offset, 0u);

    pool.add(bytes, 2, Out(offset));
    EXPECT_EQ(pool.size(), 4u);
    EXPECT_EQ(pool.alignment(), 2u);
    EXPECT_EQ(offset, 2u);

    pool.add(bytes, 4, Out(offset));
    EXPECT_EQ(pool.size(), 8u);
    EXPECT_EQ(pool.alignment(), 4u);
    EXPECT_EQ(offset, 4u);

    pool.add(bytes, 4, Out(offset));
    EXPECT_EQ(pool.size(), 8u);
    EXPECT_EQ(pool.alignment(), 4u);
    EXPECT_EQ(offset, 4u);

    pool.add(bytes, 32, Out(offset));
    EXPECT_EQ(pool.size(), 64u);
    EXPECT_EQ(pool.alignment(), 32u);
    EXPECT_EQ(offset, 32u);
  }
}

ASMJIT_END_NAMESPACE

#endif // ASMJIT_TEST
