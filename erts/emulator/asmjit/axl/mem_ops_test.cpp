// This file is part of AsmJit project <https://asmjit.com>
//
// See <asmjit/core.h> or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#include <asmjit/core/api_build_test_p.h>
#if defined(ASMJIT_TEST)

#include <asmjit/axl/mem_ops.h>

#include <initializer_list>

ASMJIT_BEGIN_NAMESPACE

TEST_CASE(axl_mem_ops_load_store_aligned) {
  uint64_t buf[2] {};

  axl::storea_i16(axl::offset_ptr<uint8_t>(buf, 0), int16_t(0x0102u));
  axl::storea_u16(axl::offset_ptr<uint8_t>(buf, 2), uint16_t(0x0304u));
  EXPECT_EQ(axl::loada_i16(axl::offset_ptr<uint8_t>(buf, 0)), 0x0102);
  EXPECT_EQ(axl::loada_u16(axl::offset_ptr<uint8_t>(buf, 2)), 0x0304u);

  axl::storea_i16_be(axl::offset_ptr<uint8_t>(buf, 0), int16_t(0x0102u));
  axl::storea_u16_be(axl::offset_ptr<uint8_t>(buf, 2), uint16_t(0x0304u));
  EXPECT_EQ(axl::loada_i32_be(axl::offset_ptr<uint8_t>(buf, 0)), 0x01020304);
  EXPECT_EQ(axl::loada_u32_be(axl::offset_ptr<uint8_t>(buf, 0)), 0x01020304u);

  axl::storea_i16_le(axl::offset_ptr<uint8_t>(buf, 0), int16_t(0x0102u));
  axl::storea_u16_le(axl::offset_ptr<uint8_t>(buf, 2), uint16_t(0x0304u));
  EXPECT_EQ(axl::loada_i32_le(axl::offset_ptr<uint8_t>(buf, 0)), 0x03040102);
  EXPECT_EQ(axl::loada_u32_le(axl::offset_ptr<uint8_t>(buf, 0)), 0x03040102u);

  axl::storea_i32(axl::offset_ptr<uint8_t>(buf, 0), int32_t(0x01020304u));
  axl::storea_u32(axl::offset_ptr<uint8_t>(buf, 4), uint32_t(0x05060708u));
  EXPECT_EQ(axl::loada_i32(axl::offset_ptr<uint8_t>(buf, 0)), 0x01020304);
  EXPECT_EQ(axl::loada_u32(axl::offset_ptr<uint8_t>(buf, 4)), 0x05060708u);

  axl::storea_i32_be(axl::offset_ptr<uint8_t>(buf, 0), int32_t(0x01020304u));
  axl::storea_u32_be(axl::offset_ptr<uint8_t>(buf, 4), uint32_t(0x05060708u));
  EXPECT_EQ(axl::loada_i64_be(axl::offset_ptr<uint8_t>(buf, 0)), 0x0102030405060708);
  EXPECT_EQ(axl::loada_u64_be(axl::offset_ptr<uint8_t>(buf, 0)), 0x0102030405060708u);

  axl::storea_i64(axl::offset_ptr<uint8_t>(buf, 0), int64_t(0x0102030405060708u));
  axl::storea_u64(axl::offset_ptr<uint8_t>(buf, 8), uint64_t(0x090A0B0C0D0E0FEFu));
  EXPECT_EQ(axl::loada_i64(axl::offset_ptr<uint8_t>(buf, 0)), 0x0102030405060708);
  EXPECT_EQ(axl::loada_u64(axl::offset_ptr<uint8_t>(buf, 8)), 0x090A0B0C0D0E0FEFu);

  axl::storea_i64_be(axl::offset_ptr<uint8_t>(buf, 0), int64_t(0x0102030405060708u));
  axl::storea_i64_le(axl::offset_ptr<uint8_t>(buf, 8), int64_t(0x0102030405060708u));
  EXPECT_EQ(axl::loada_u64_le(axl::offset_ptr<uint8_t>(buf, 0)), 0x0807060504030201u);
  EXPECT_EQ(axl::loada_u64_be(axl::offset_ptr<uint8_t>(buf, 8)), 0x0807060504030201u);

  axl::storea_be(axl::offset_ptr<uint8_t>(buf, 0), int16_t(0x1122));
  axl::storea_le(axl::offset_ptr<uint8_t>(buf, 8), int16_t(0x1122));
  EXPECT_EQ(axl::loada_i16_le(axl::offset_ptr<uint8_t>(buf, 0)), int16_t(0x2211));
  EXPECT_EQ(axl::loada_i16_be(axl::offset_ptr<uint8_t>(buf, 8)), int16_t(0x2211));

  axl::storea(buf, uint16_t(0x0102));
  EXPECT_EQ(axl::loada<uint16_t>(buf), uint16_t(0x0102));

  axl::storea_be(axl::offset_ptr<uint8_t>(buf, 0), uint16_t(0x0102));
  axl::storea_le(axl::offset_ptr<uint8_t>(buf, 8), uint16_t(0x0102));
  EXPECT_EQ(axl::loada_u16_le(axl::offset_ptr<uint8_t>(buf, 0)), uint16_t(0x0201));
  EXPECT_EQ(axl::loada_u16_be(axl::offset_ptr<uint8_t>(buf, 8)), uint16_t(0x0201));

  axl::storea(buf, int32_t(0x11223344));
  EXPECT_EQ(axl::loada<int32_t>(buf), int32_t(0x11223344));

  axl::storea_be(axl::offset_ptr<uint8_t>(buf, 0), int32_t(0x11223344));
  axl::storea_i32_le(axl::offset_ptr<uint8_t>(buf, 8), int32_t(0x11223344));
  EXPECT_EQ(axl::loada_le<int32_t>(axl::offset_ptr<uint8_t>(buf, 0)), int32_t(0x44332211));
  EXPECT_EQ(axl::loada_be<int32_t>(axl::offset_ptr<uint8_t>(buf, 8)), int32_t(0x44332211));

  axl::storea(buf, uint32_t(0x01020304));
  EXPECT_EQ(axl::loada<uint32_t>(buf), uint32_t(0x01020304));

  axl::storea_be(axl::offset_ptr<uint8_t>(buf, 0), uint32_t(0x01020304));
  axl::storea_u32_le(axl::offset_ptr<uint8_t>(buf, 8), uint32_t(0x01020304));
  EXPECT_EQ(axl::loada_le<uint32_t>(axl::offset_ptr<uint8_t>(buf, 0)), uint32_t(0x04030201));
  EXPECT_EQ(axl::loada_be<uint32_t>(axl::offset_ptr<uint8_t>(buf, 8)), uint32_t(0x04030201));

  axl::storea(buf, int64_t(0x1122334455667788));
  EXPECT_EQ(axl::loada<int64_t>(buf), int64_t(0x1122334455667788));

  axl::storea_be(axl::offset_ptr<uint8_t>(buf, 0), int64_t(0x1122334455667788));
  axl::storea_le(axl::offset_ptr<uint8_t>(buf, 8), int64_t(0x1122334455667788));
  EXPECT_EQ(axl::loada_i64_le(axl::offset_ptr<uint8_t>(buf, 0)), int64_t(0x8877665544332211));
  EXPECT_EQ(axl::loada_be<int64_t>(axl::offset_ptr<uint8_t>(buf, 8)), int64_t(0x8877665544332211));

  axl::storea(buf, uint64_t(0x0102030405060708));
  EXPECT_EQ(axl::loada<uint64_t>(buf), uint64_t(0x0102030405060708));

  axl::storea_u64_be(axl::offset_ptr<uint8_t>(buf, 0), uint64_t(0x0102030405060708));
  axl::storea_u64_le(axl::offset_ptr<uint8_t>(buf, 8), uint64_t(0x0102030405060708));
  EXPECT_EQ(axl::loada_le<uint64_t>(axl::offset_ptr<uint8_t>(buf, 0)), uint64_t(0x0807060504030201));
  EXPECT_EQ(axl::loada_be<uint64_t>(axl::offset_ptr<uint8_t>(buf, 8)), uint64_t(0x0807060504030201));

  float f = 1.2f;
  axl::storea(buf, f);
  EXPECT_EQ(axl::loada<float>(buf), f);

  axl::storea_le(axl::offset_ptr<uint8_t>(buf, 0), f);
  axl::storea_be(axl::offset_ptr<uint8_t>(buf, 8), f);
  EXPECT_EQ(axl::loada_le<float>(axl::offset_ptr<uint8_t>(buf, 0)), f);
  EXPECT_EQ(axl::loada_be<float>(axl::offset_ptr<uint8_t>(buf, 8)), f);
  EXPECT_NE(axl::loada<uint32_t>(axl::offset_ptr<uint8_t>(buf, 0)), axl::loada<uint32_t>(axl::offset_ptr<uint8_t>(buf, 8)));

  double d = 1.234;

  axl::storea_le(axl::offset_ptr<uint8_t>(buf, 0), d);
  axl::storea_be(axl::offset_ptr<uint8_t>(buf, 8), d);
  EXPECT_EQ(axl::loada_le<double>(axl::offset_ptr<uint8_t>(buf, 0)), d);
  EXPECT_EQ(axl::loada_be<double>(axl::offset_ptr<uint8_t>(buf, 8)), d);
  EXPECT_NE(axl::loada<uint64_t>(axl::offset_ptr<uint8_t>(buf, 0)), axl::loada<uint64_t>(axl::offset_ptr<uint8_t>(buf, 8)));
}

TEST_CASE(axl_mem_ops_load_store_unaligned) {
  uint8_t arr[32] = {0};

  axl::storeu_i16_be(arr + 1, 0x0102);
  axl::storeu_u16_be(arr + 3, 0x0304u);
  EXPECT_EQ(axl::loadu_u32_be(arr + 1), 0x01020304u);
  EXPECT_EQ(axl::loadu_u32_le(arr + 1), 0x04030201u);
  EXPECT_EQ(axl::loadu_u32_be(arr + 2), 0x02030400u);
  EXPECT_EQ(axl::loadu_u32_le(arr + 2), 0x00040302u);

  axl::storeu_u32_le(arr + 5, 0x05060708u);
  EXPECT_EQ(axl::loadu_u64_be(arr + 1), 0x0102030408070605u);
  EXPECT_EQ(axl::loadu_u64_le(arr + 1), 0x0506070804030201u);

  axl::storeu_u64_le(arr + 7, 0x1122334455667788u);
  EXPECT_EQ(axl::loadu_u32_be(arr + 8), 0x77665544u);

  EXPECT_EQ(axl::load_i8(arr + 1), int8_t(0x01));
  EXPECT_EQ(axl::load_u8(arr + 2), uint8_t(0x02));

  axl::store_i8(arr + 0, int8_t(0x71));
  axl::store_u8(arr + 1, uint8_t(0xF2));
  EXPECT_EQ(axl::loadu_u16_be(arr), 0x71F2u);

  uint8_t pattern[8] = {1, 2, 3, 4, 5, 6, 7, 8};

  EXPECT_EQ(axl::loadu_u64_be(pattern), uint64_t(0x0102030405060708));
  EXPECT_EQ(axl::loadu_u64_le(pattern), uint64_t(0x0807060504030201));

  EXPECT_EQ(axl::loadu_i64_be(pattern), int64_t(0x0102030405060708));
  EXPECT_EQ(axl::loadu_i64_le(pattern), int64_t(0x0807060504030201));

  EXPECT_EQ(axl::loadu_u32_be(pattern), uint64_t(0x01020304));
  EXPECT_EQ(axl::loadu_u32_le(pattern), uint64_t(0x04030201));

  EXPECT_EQ(axl::loadu_i32_be(pattern), int64_t(0x01020304));
  EXPECT_EQ(axl::loadu_i32_le(pattern), int64_t(0x04030201));

  EXPECT_EQ(axl::loadu_u24_be(pattern), uint64_t(0x010203));
  EXPECT_EQ(axl::loadu_u24_le(pattern), uint64_t(0x030201));

  EXPECT_EQ(axl::loadu_u16_be(pattern), uint64_t(0x0102));
  EXPECT_EQ(axl::loadu_u16_le(pattern), uint64_t(0x0201));

  EXPECT_EQ(axl::loadu_i16_be(pattern), int64_t(0x0102));
  EXPECT_EQ(axl::loadu_i16_le(pattern), int64_t(0x0201));

  axl::storeu_u24_le(pattern, 0xF0F1F2u);
  EXPECT_EQ(pattern[0], 0xF2u);
  EXPECT_EQ(pattern[1], 0xF1u);
  EXPECT_EQ(pattern[2], 0xF0u);
  EXPECT_EQ(pattern[3], 0x04u); // storeu_u24_le should not overwrite the 4th byte.

  axl::storeu_u24_be(pattern, 0xF0F1F2u);
  EXPECT_EQ(pattern[0], 0xF0u);
  EXPECT_EQ(pattern[1], 0xF1u);
  EXPECT_EQ(pattern[2], 0xF2u);
  EXPECT_EQ(pattern[3], 0x04u); // storeu_u24_be should not overwrite the 4th byte.

  uint8_t buf[20] {};

  constexpr size_t kA = 1;
  constexpr size_t kB = 11;

  axl::storeu_i16(buf, int16_t(0x1122));
  EXPECT_EQ(axl::loadu_i16(buf), int16_t(0x1122));

  axl::storeu_be(buf + kA, int16_t(0x1122));
  axl::storeu_i16_le(buf + kB, int16_t(0x1122));
  EXPECT_EQ(axl::loadu_le<int16_t>(buf + kA), int16_t(0x2211));
  EXPECT_EQ(axl::loadu_be<int16_t>(buf + kB), int16_t(0x2211));

  axl::storeu_u16(buf, uint16_t(0x0102));
  EXPECT_EQ(axl::loadu<uint16_t>(buf), uint16_t(0x0102));

  axl::storeu_be(buf + kA, uint16_t(0x0102));
  axl::storeu_u16_le(buf + kB, uint16_t(0x0102));
  EXPECT_EQ(axl::loadu_le<uint16_t>(buf + kA), uint16_t(0x0201));
  EXPECT_EQ(axl::loadu_be<uint16_t>(buf + kB), uint16_t(0x0201));

  axl::storeu_i32(buf, int32_t(0x11223344));
  EXPECT_EQ(axl::loadu<int32_t>(buf), int32_t(0x11223344));

  axl::storeu_i32_be(buf + kA, int32_t(0x11223344));
  axl::storeu_i32_le(buf + kB, int32_t(0x11223344));
  EXPECT_EQ(axl::loadu_le<int32_t>(buf + kA), int32_t(0x44332211));
  EXPECT_EQ(axl::loadu_be<int32_t>(buf + kB), int32_t(0x44332211));

  axl::storeu_u32(buf, uint32_t(0x01020304));
  EXPECT_EQ(axl::loadu<uint32_t>(buf), uint32_t(0x01020304));

  axl::storeu_u32_be(buf + kA, uint32_t(0x01020304));
  axl::storeu_le(buf + kB, uint32_t(0x01020304));
  EXPECT_EQ(axl::loadu_le<uint32_t>(buf + kA), uint32_t(0x04030201));
  EXPECT_EQ(axl::loadu_be<uint32_t>(buf + kB), uint32_t(0x04030201));

  axl::storeu_i64(buf, int64_t(0x1122334455667788));
  EXPECT_EQ(axl::loadu<int64_t>(buf), int64_t(0x1122334455667788));

  axl::storeu_i64_be(buf + kA, int64_t(0x1122334455667788));
  axl::storeu_i64_le(buf + kB, int64_t(0x1122334455667788));
  EXPECT_EQ(axl::loadu_le<int64_t>(buf + kA), int64_t(0x8877665544332211));
  EXPECT_EQ(axl::loadu_be<int64_t>(buf + kB), int64_t(0x8877665544332211));

  axl::storeu_u64(buf, uint64_t(0x0102030405060708));
  EXPECT_EQ(axl::loadu<uint64_t>(buf), uint64_t(0x0102030405060708));

  axl::storeu_u64_be(buf + kA, uint64_t(0x0102030405060708));
  axl::storeu_le(buf + kB, uint64_t(0x0102030405060708));
  EXPECT_EQ(axl::loadu_le<uint64_t>(buf + kA), uint64_t(0x0807060504030201));
  EXPECT_EQ(axl::loadu_be<uint64_t>(buf + kB), uint64_t(0x0807060504030201));

  float f = 1.2f;
  axl::storeu(buf, f);
  EXPECT_EQ(axl::loadu<float>(buf), f);

  axl::storeu_le(buf + kA, f);
  axl::storeu_be(buf + kB, f);
  EXPECT_EQ(axl::loadu_le<float>(buf + kA), f);
  EXPECT_EQ(axl::loadu_be<float>(buf + kB), f);
  EXPECT_NE(axl::loadu<uint32_t>(buf + kA), axl::loadu<uint32_t>(buf + kB));

  double d = 1.234;

  axl::storeu_le(buf + kA, d);
  axl::storeu_be(buf + kB, d);
  EXPECT_EQ(axl::loadu_le<double>(buf + kA), d);
  EXPECT_EQ(axl::loadu_be<double>(buf + kB), d);
  EXPECT_NE(axl::loadu<uint64_t>(buf + kA), axl::loadu<uint64_t>(buf + kB));
}

TEST_CASE(axl_mem_ops_fill) {
  for (bool small : {false, true}) {
    uint64_t arr[4] {};

    if (!small)
      axl::fill_items(reinterpret_cast<uint8_t*>(arr), 31, uint8_t(0xFFu));
    else
      axl::fill_items_small(reinterpret_cast<uint8_t*>(arr), 31, uint8_t(0xFFu));

    EXPECT_EQ(axl::load_u8(reinterpret_cast<uint8_t*>(arr) + 0), 0xFFu);
    EXPECT_EQ(axl::load_u8(reinterpret_cast<uint8_t*>(arr) + 30), 0xFFu);
    EXPECT_EQ(axl::load_u8(reinterpret_cast<uint8_t*>(arr) + 31), 0x00u);

    if (!small)
      axl::fill_items(reinterpret_cast<uint16_t*>(arr), 4, uint16_t(0x0102u));
    else
      axl::fill_items_small(reinterpret_cast<uint16_t*>(arr), 4, uint16_t(0x0102u));

    EXPECT_EQ(axl::loadu_u16(reinterpret_cast<uint8_t*>(arr) + 0), uint16_t(0x0102u));
    EXPECT_EQ(axl::loadu_u16(reinterpret_cast<uint8_t*>(arr) + 2), uint16_t(0x0102u));
    EXPECT_EQ(axl::loadu_u16(reinterpret_cast<uint8_t*>(arr) + 4), uint16_t(0x0102u));
    EXPECT_EQ(axl::loadu_u16(reinterpret_cast<uint8_t*>(arr) + 6), uint16_t(0x0102u));
    EXPECT_EQ(axl::loadu_u16(reinterpret_cast<uint8_t*>(arr) + 8), uint16_t(0xFFFFu));

    if (!small)
      axl::fill_items(reinterpret_cast<uint32_t*>(arr), 2, uint32_t(0x01020304u));
    else
      axl::fill_items_small(reinterpret_cast<uint32_t*>(arr), 2, uint32_t(0x01020304u));

    EXPECT_EQ(axl::loadu_u32(reinterpret_cast<uint8_t*>(arr) + 0), uint32_t(0x01020304u));
    EXPECT_EQ(axl::loadu_u32(reinterpret_cast<uint8_t*>(arr) + 4), uint32_t(0x01020304u));
    EXPECT_EQ(axl::loadu_u32(reinterpret_cast<uint8_t*>(arr) + 8), uint32_t(0xFFFFFFFFu));

    if (!small)
      axl::fill_items(reinterpret_cast<uint64_t*>(arr), 2, uint64_t(0x0102030405060708u));
    else
      axl::fill_items_small(reinterpret_cast<uint64_t*>(arr), 2, uint64_t(0x0102030405060708u));

    EXPECT_EQ(axl::loadu_u64(reinterpret_cast<uint8_t*>(arr) + 0), uint64_t(0x0102030405060708u));
    EXPECT_EQ(axl::loadu_u64(reinterpret_cast<uint8_t*>(arr) + 8), uint64_t(0x0102030405060708u));
    EXPECT_EQ(axl::loadu_u64(reinterpret_cast<uint8_t*>(arr) + 16), uint64_t(0xFFFFFFFFFFFFFFFFu));
  }
}

TEST_CASE(axl_mem_ops_copy) {
  uint8_t buffer[16] = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15};

  axl::copy_forward(buffer + 4, buffer + 0, 8u);
  uint8_t expected_a[16] = {0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 3, 12, 13, 14, 15};
  EXPECT_EQ(memcmp(buffer, expected_a, 16), 0);

  axl::copy_reverse(buffer + 7, buffer + 8, 8u);
  uint8_t expected_b[16] = {0, 1, 2, 3, 0, 1, 2, 15, 15, 15, 15, 15, 15, 15, 15, 15};
  EXPECT_EQ(memcmp(buffer, expected_b, 16), 0);

  uint8_t copy[16];
  axl::copy_bytes_small(copy, buffer, 16u);
  EXPECT_EQ(memcmp(buffer, copy, 16), 0);
}

ASMJIT_END_NAMESPACE

#endif // ASMJIT_TEST
