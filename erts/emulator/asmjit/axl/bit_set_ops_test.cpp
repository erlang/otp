// This file is part of AsmJit project <https://asmjit.com>
//
// See <asmjit/core.h> or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#include <asmjit/core/api_build_test_p.h>
#if defined(ASMJIT_TEST)

#include <asmjit/axl/bit_set_ops.h>

ASMJIT_BEGIN_NAMESPACE

TEST_CASE(axl_bit_set_ops) {
  TEST_LOG("axl::bit_vector_op");
  {
    uint32_t vec[3] = { 0 };
    axl::bit_vector_fill(vec, 1, 64);
    EXPECT_EQ(vec[0], 0xFFFFFFFEu);
    EXPECT_EQ(vec[1], 0xFFFFFFFFu);
    EXPECT_EQ(vec[2], 0x00000001u);

    axl::bit_vector_clear(vec, 1, 1);
    EXPECT_EQ(vec[0], 0xFFFFFFFCu);
    EXPECT_EQ(vec[1], 0xFFFFFFFFu);
    EXPECT_EQ(vec[2], 0x00000001u);

    axl::bit_vector_fill(vec, 0, 32);
    EXPECT_EQ(vec[0], 0xFFFFFFFFu);
    EXPECT_EQ(vec[1], 0xFFFFFFFFu);
    EXPECT_EQ(vec[2], 0x00000001u);

    axl::bit_vector_clear(vec, 0, 32);
    EXPECT_EQ(vec[0], 0x00000000u);
    EXPECT_EQ(vec[1], 0xFFFFFFFFu);
    EXPECT_EQ(vec[2], 0x00000001u);

    axl::bit_vector_fill(vec, 1, 30);
    EXPECT_EQ(vec[0], 0x7FFFFFFEu);
    EXPECT_EQ(vec[1], 0xFFFFFFFFu);
    EXPECT_EQ(vec[2], 0x00000001u);

    axl::bit_vector_clear(vec, 1, 95);
    EXPECT_EQ(vec[0], 0x00000000u);
    EXPECT_EQ(vec[1], 0x00000000u);
    EXPECT_EQ(vec[2], 0x00000000u);

    axl::bit_vector_fill(vec, 32, 64);
    EXPECT_EQ(vec[0], 0x00000000u);
    EXPECT_EQ(vec[1], 0xFFFFFFFFu);
    EXPECT_EQ(vec[2], 0xFFFFFFFFu);

    axl::bit_vector_set_bit(vec, 1, true);
    EXPECT_EQ(vec[0], 0x00000002u);
    EXPECT_EQ(vec[1], 0xFFFFFFFFu);
    EXPECT_EQ(vec[2], 0xFFFFFFFFu);

    axl::bit_vector_set_bit(vec, 95, false);
    EXPECT_EQ(vec[0], 0x00000002u);
    EXPECT_EQ(vec[1], 0xFFFFFFFFu);
    EXPECT_EQ(vec[2], 0x7FFFFFFFu);

    axl::bit_vector_clear(vec, 33, 32);
    EXPECT_EQ(vec[0], 0x00000002u);
    EXPECT_EQ(vec[1], 0x00000001u);
    EXPECT_EQ(vec[2], 0x7FFFFFFEu);
  }

  TEST_LOG("axl::bit_vector_index_of");
  {
    uint32_t vec1[1] = { 0x80000000 };
    EXPECT_EQ(axl::bit_vector_index_of(vec1, 0, true), 31u);
    EXPECT_EQ(axl::bit_vector_index_of(vec1, 1, true), 31u);
    EXPECT_EQ(axl::bit_vector_index_of(vec1, 31, true), 31u);

    uint32_t vec2[2] = { 0x00000000, 0x80000000 };
    EXPECT_EQ(axl::bit_vector_index_of(vec2, 0, true), 63u);
    EXPECT_EQ(axl::bit_vector_index_of(vec2, 1, true), 63u);
    EXPECT_EQ(axl::bit_vector_index_of(vec2, 31, true), 63u);
    EXPECT_EQ(axl::bit_vector_index_of(vec2, 32, true), 63u);
    EXPECT_EQ(axl::bit_vector_index_of(vec2, 33, true), 63u);
    EXPECT_EQ(axl::bit_vector_index_of(vec2, 63, true), 63u);

    uint32_t vec3[3] = { 0x00000001, 0x00000000, 0x80000000 };
    EXPECT_EQ(axl::bit_vector_index_of(vec3, 0, true), 0u);
    EXPECT_EQ(axl::bit_vector_index_of(vec3, 1, true), 95u);
    EXPECT_EQ(axl::bit_vector_index_of(vec3, 2, true), 95u);
    EXPECT_EQ(axl::bit_vector_index_of(vec3, 31, true), 95u);
    EXPECT_EQ(axl::bit_vector_index_of(vec3, 32, true), 95u);
    EXPECT_EQ(axl::bit_vector_index_of(vec3, 63, true), 95u);
    EXPECT_EQ(axl::bit_vector_index_of(vec3, 64, true), 95u);
    EXPECT_EQ(axl::bit_vector_index_of(vec3, 95, true), 95u);

    uint32_t vec4[3] = { ~vec3[0], ~vec3[1], ~vec3[2] };
    EXPECT_EQ(axl::bit_vector_index_of(vec4, 0, false), 0u);
    EXPECT_EQ(axl::bit_vector_index_of(vec4, 1, false), 95u);
    EXPECT_EQ(axl::bit_vector_index_of(vec4, 2, false), 95u);
    EXPECT_EQ(axl::bit_vector_index_of(vec4, 31, false), 95u);
    EXPECT_EQ(axl::bit_vector_index_of(vec4, 32, false), 95u);
    EXPECT_EQ(axl::bit_vector_index_of(vec4, 63, false), 95u);
    EXPECT_EQ(axl::bit_vector_index_of(vec4, 64, false), 95u);
    EXPECT_EQ(axl::bit_vector_index_of(vec4, 95, false), 95u);
  }

  TEST_LOG("axl::BitWordIterator<uint32_t>");
  {
    axl::BitWordIterator<uint32_t> it(0x80000F01u);
    EXPECT_TRUE(it.has_next());
    EXPECT_EQ(it.next(), 0u);
    EXPECT_TRUE(it.has_next());
    EXPECT_EQ(it.next(), 8u);
    EXPECT_TRUE(it.has_next());
    EXPECT_EQ(it.next(), 9u);
    EXPECT_TRUE(it.has_next());
    EXPECT_EQ(it.next(), 10u);
    EXPECT_TRUE(it.has_next());
    EXPECT_EQ(it.next(), 11u);
    EXPECT_TRUE(it.has_next());
    EXPECT_EQ(it.next(), 31u);
    EXPECT_FALSE(it.has_next());

    // No bits set.
    it.init(0x00000000u);
    EXPECT_FALSE(it.has_next());

    // Only first bit set.
    it.init(0x00000001u);
    EXPECT_TRUE(it.has_next());
    EXPECT_EQ(it.next(), 0u);
    EXPECT_FALSE(it.has_next());

    // Only last bit set (special case).
    it.init(0x80000000u);
    EXPECT_TRUE(it.has_next());
    EXPECT_EQ(it.next(), 31u);
    EXPECT_FALSE(it.has_next());
  }

  TEST_LOG("axl::BitWordIterator<uint64_t>");
  {
    axl::BitWordIterator<uint64_t> it(uint64_t(1) << 63);
    EXPECT_TRUE(it.has_next());
    EXPECT_EQ(it.next(), 63u);
    EXPECT_FALSE(it.has_next());
  }

  TEST_LOG("axl::BitVectorIterator<uint32_t>");
  {
    // Border cases.
    static const uint32_t bits_none[] = { 0xFFFFFFFFu };
    axl::BitVectorIterator<uint32_t> it(Span<const uint32_t>(bits_none, 0));

    EXPECT_FALSE(it.has_next());
    it.init(Span<const uint32_t>(bits_none, 0), 1);
    EXPECT_FALSE(it.has_next());
    it.init(Span<const uint32_t>(bits_none, 0), 128);
    EXPECT_FALSE(it.has_next());

    static const uint32_t bits1[] = { 0x80000008u, 0x80000001u, 0x00000000u, 0x80000000u, 0x00000000u, 0x00000000u, 0x00003000u };
    it.init(Span<const uint32_t>::from_array(bits1));

    EXPECT_TRUE(it.has_next());
    EXPECT_EQ(it.next(), 3u);
    EXPECT_TRUE(it.has_next());
    EXPECT_EQ(it.next(), 31u);
    EXPECT_TRUE(it.has_next());
    EXPECT_EQ(it.next(), 32u);
    EXPECT_TRUE(it.has_next());
    EXPECT_EQ(it.next(), 63u);
    EXPECT_TRUE(it.has_next());
    EXPECT_EQ(it.next(), 127u);
    EXPECT_TRUE(it.has_next());
    EXPECT_EQ(it.next(), 204u);
    EXPECT_TRUE(it.has_next());
    EXPECT_EQ(it.next(), 205u);
    EXPECT_FALSE(it.has_next());

    it.init(Span<const uint32_t>::from_array(bits1), 4);
    EXPECT_TRUE(it.has_next());
    EXPECT_EQ(it.next(), 31u);

    it.init(Span<const uint32_t>::from_array(bits1), 64);
    EXPECT_TRUE(it.has_next());
    EXPECT_EQ(it.next(), 127u);

    it.init(Span<const uint32_t>::from_array(bits1), 127);
    EXPECT_TRUE(it.has_next());
    EXPECT_EQ(it.next(), 127u);

    static const uint32_t bits2[] = { 0x80000000u, 0x80000000u, 0x00000000u, 0x80000000u };
    it.init(Span<const uint32_t>::from_array(bits2));

    EXPECT_TRUE(it.has_next());
    EXPECT_EQ(it.next(), 31u);
    EXPECT_TRUE(it.has_next());
    EXPECT_EQ(it.next(), 63u);
    EXPECT_TRUE(it.has_next());
    EXPECT_EQ(it.next(), 127u);
    EXPECT_FALSE(it.has_next());

    static const uint32_t bits3[] = { 0x00000000u, 0x00000000u, 0x00000000u, 0x00000000u };
    it.init(Span<const uint32_t>::from_array(bits3));
    EXPECT_FALSE(it.has_next());

    static const uint32_t bits4[] = { 0x00000000u, 0x00000000u, 0x00000000u, 0x80000000u };
    it.init(Span<const uint32_t>::from_array(bits4));
    EXPECT_TRUE(it.has_next());
    EXPECT_EQ(it.next(), 127u);
    EXPECT_FALSE(it.has_next());
  }

  TEST_LOG("axl::BitVectorIterator<uint64_t>");
  {
    static const uint64_t bits1[] = { 0x80000000u, 0x80000000u, 0x00000000u, 0x80000000u };
    axl::BitVectorIterator<uint64_t> it(Span<const uint64_t>::from_array(bits1));

    EXPECT_TRUE(it.has_next());
    EXPECT_EQ(it.next(), 31u);
    EXPECT_TRUE(it.has_next());
    EXPECT_EQ(it.next(), 95u);
    EXPECT_TRUE(it.has_next());
    EXPECT_EQ(it.next(), 223u);
    EXPECT_FALSE(it.has_next());

    static const uint64_t bits2[] = { 0x8000000000000000u, 0, 0, 0 };
    it.init(Span<const uint64_t>::from_array(bits2));

    EXPECT_TRUE(it.has_next());
    EXPECT_EQ(it.next(), 63u);
    EXPECT_FALSE(it.has_next());
  }
}

struct TestRange {
  size_t a, b;
};

template<typename T, uint32_t B>
static void verify_range_iterator(axl::BitVectorRangeIterator<T, B>& it, const TestRange* expected_ranges, size_t expected_count) {
  size_t expected_index = 0;
  TestRange found;

  while (it.next_range(Out(found.a), Out(found.b))) {
    EXPECT_LT(expected_index, expected_count);

    size_t expected_a = expected_ranges[expected_index].a;
    size_t expected_b = expected_ranges[expected_index].b;

    EXPECT_TRUE(found.a == expected_a && found.b == expected_b)
      .message("invalid range - found {%zu, %zu}, expected {%zu, %zu}", found.a, found.b, expected_a, expected_b);
    expected_index++;
  }

  EXPECT_EQ(expected_index, expected_count)
    .message("BitVectorRangeIterator failed to find all ranges (found %zu, expected %zu)", expected_index, expected_count);
}

template<typename T, uint32_t B>
static void verify_range_iterator(const T* data, size_t size, const TestRange* expected_ranges, size_t expected_count) {
  axl::BitVectorRangeIterator<T, B> it(data, size);
  verify_range_iterator<T, B>(it, expected_ranges, expected_count);
}

template<typename T, uint32_t B>
static void verify_range_iterator(const T* data, size_t size, size_t bit_start, size_t bit_end, const TestRange* expected_ranges, size_t expected_count) {
  axl::BitVectorRangeIterator<T, B> it(data, size, bit_start, bit_end);
  verify_range_iterator<T, B>(it, expected_ranges, expected_count);
}

TEST_CASE(axl_bit_set_range_iterator) {
  {
    uint32_t bits[] = {0};
    TestRange expected_0[] = {{0, 32}};
    verify_range_iterator<uint32_t, 0>(bits, ASMJIT_ARRAY_SIZE(bits), expected_0, ASMJIT_ARRAY_SIZE(expected_0));
    verify_range_iterator<uint32_t, 1>(bits, ASMJIT_ARRAY_SIZE(bits), nullptr, 0);
  }

  {
    uint32_t bits[] = {0};
    TestRange expected_0[] = {{5, 27}};
    verify_range_iterator<uint32_t, 0>(bits, ASMJIT_ARRAY_SIZE(bits), 5, 27, expected_0, ASMJIT_ARRAY_SIZE(expected_0));
    verify_range_iterator<uint32_t, 1>(bits, ASMJIT_ARRAY_SIZE(bits), 5, 27, nullptr, 0);
  }

  {
    uint32_t bits[] = {0xFFFFFFFFu};
    TestRange expected_1[] = {{0, 32}};
    verify_range_iterator<uint32_t, 0>(bits, ASMJIT_ARRAY_SIZE(bits), nullptr, 0);
    verify_range_iterator<uint32_t, 1>(bits, ASMJIT_ARRAY_SIZE(bits), expected_1, ASMJIT_ARRAY_SIZE(expected_1));
  }

  {
    uint32_t bits[] = {0xFFFFFFFFu};
    TestRange expected_1[] = {{5, 27}};
    verify_range_iterator<uint32_t, 0>(bits, ASMJIT_ARRAY_SIZE(bits), 5, 27, nullptr, 0);
    verify_range_iterator<uint32_t, 1>(bits, ASMJIT_ARRAY_SIZE(bits), 5, 27, expected_1, ASMJIT_ARRAY_SIZE(expected_1));
  }

  {
    uint32_t bits[] = {0, 0, 0, 0};
    TestRange expected_0[] = {{0, 128}};
    verify_range_iterator<uint32_t, 0>(bits, ASMJIT_ARRAY_SIZE(bits), expected_0, ASMJIT_ARRAY_SIZE(expected_0));
    verify_range_iterator<uint32_t, 1>(bits, ASMJIT_ARRAY_SIZE(bits), nullptr, 0);
  }

  {
    uint32_t bits[] = {0, 0, 0, 0};
    TestRange expected_0[] = {{5, 91}};
    verify_range_iterator<uint32_t, 0>(bits, ASMJIT_ARRAY_SIZE(bits), 5, 91, expected_0, ASMJIT_ARRAY_SIZE(expected_0));
    verify_range_iterator<uint32_t, 1>(bits, ASMJIT_ARRAY_SIZE(bits), 5, 91, nullptr, 0);
  }

  {
    uint32_t bits[] = {0xFFFFFFFFu, 0xFFFFFFFFu, 0xFFFFFFFFu, 0xFFFFFFFFu};
    TestRange expected_1[] = {{0, 128}};
    verify_range_iterator<uint32_t, 0>(bits, ASMJIT_ARRAY_SIZE(bits), nullptr, 0);
    verify_range_iterator<uint32_t, 1>(bits, ASMJIT_ARRAY_SIZE(bits), expected_1, ASMJIT_ARRAY_SIZE(expected_1));
  }

  {
    uint32_t bits[] = {0xFFFFFFFFu, 0xFFFFFFFFu, 0xFFFFFFFFu, 0xFFFFFFFFu};
    TestRange expected_1[] = {{5, 91}};
    verify_range_iterator<uint32_t, 0>(bits, ASMJIT_ARRAY_SIZE(bits), 5, 91, nullptr, 0);
    verify_range_iterator<uint32_t, 1>(bits, ASMJIT_ARRAY_SIZE(bits), 5, 91, expected_1, ASMJIT_ARRAY_SIZE(expected_1));
  }

  {
    uint32_t bits[] = {0x30FF00FFu};
    TestRange expected_0[] = {{8, 16}, {24, 28}, {30, 32}};
    TestRange expected_1[] = {{0, 8}, {16, 24}, {28, 30}};
    verify_range_iterator<uint32_t, 0>(bits, ASMJIT_ARRAY_SIZE(bits), expected_0, ASMJIT_ARRAY_SIZE(expected_0));
    verify_range_iterator<uint32_t, 1>(bits, ASMJIT_ARRAY_SIZE(bits), expected_1, ASMJIT_ARRAY_SIZE(expected_1));
  }

  {
    uint32_t bits[] = {0, 0x30FF00FF, 0};
    TestRange expected_0[] = {{0, 32}, {40, 48}, {56, 60}, {62, 96}};
    TestRange expected_1[] = {{32, 40}, {48, 56}, {60, 62}};
    verify_range_iterator<uint32_t, 0>(bits, ASMJIT_ARRAY_SIZE(bits), expected_0, ASMJIT_ARRAY_SIZE(expected_0));
    verify_range_iterator<uint32_t, 1>(bits, ASMJIT_ARRAY_SIZE(bits), expected_1, ASMJIT_ARRAY_SIZE(expected_1));
  }
}

ASMJIT_END_NAMESPACE

#endif // ASMJIT_TEST
