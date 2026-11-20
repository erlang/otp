// This file is part of AsmJit project <https://asmjit.com>
//
// See <asmjit/core.h> or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#include <asmjit/core/api_build_test_p.h>
#if defined(ASMJIT_TEST)

#include <asmjit/axl/commons.h>

ASMJIT_BEGIN_NAMESPACE

enum class Enum : uint8_t {
  A = 0x00,
  B = 0x01,
  F = 0x80,
  Z = 0xFF
};

TEST_DEFINE_TYPES(test_int_types,
  int8_t, uint8_t, int16_t, uint16_t, int32_t, uint32_t, int64_t, uint64_t);

TEST_DEFINE_TYPES(test_int_enum_types,
  int8_t, uint8_t, int16_t, uint16_t, int32_t, uint32_t, int64_t, uint64_t, Enum);

namespace dispatch {

struct generic {
  template<typename T>
  [[nodiscard]]
  static T add_overflow(const T& x, const T& y, axl::OverflowFlag& of) noexcept { return axl::add_overflow_generic<T>(x, y, of); }

  template<typename T>
  [[nodiscard]]
  static T sub_overflow(const T& x, const T& y, axl::OverflowFlag& of) noexcept { return axl::sub_overflow_generic<T>(x, y, of); }

  template<typename T>
  [[nodiscard]]
  static T mul_overflow(const T& x, const T& y, axl::OverflowFlag& of) noexcept { return axl::mul_overflow_generic<T>(x, y, of); }
};

struct regular {
  template<typename T>
  [[nodiscard]]
  static T add_overflow(const T& x, const T& y, axl::OverflowFlag& of) noexcept { return axl::add_overflow<T>(x, y, of); }

  template<typename T>
  [[nodiscard]]
  static T sub_overflow(const T& x, const T& y, axl::OverflowFlag& of) noexcept { return axl::sub_overflow<T>(x, y, of); }

  template<typename T>
  [[nodiscard]]
  static T mul_overflow(const T& x, const T& y, axl::OverflowFlag& of) noexcept { return axl::mul_overflow<T>(x, y, of); }
};

TEST_DEFINE_TYPES(tests, generic, regular);

} // {dispatch}

TEST_CASE_T(axl_commons_bit_size_of, test_int_enum_types) {
  EXPECT_EQ(axl::bit_size_of<T>, sizeof(T) * 8u);
}

TEST_CASE(axl_commons_bit_ones) {
  EXPECT_EQ(axl::bit_ones<int8_t>, int8_t(0xFFu));
  EXPECT_EQ(axl::bit_ones<uint8_t>, uint8_t(0xFFu));
  EXPECT_EQ(axl::bit_ones<int16_t>, int16_t(0xFFFFu));
  EXPECT_EQ(axl::bit_ones<uint16_t>, uint16_t(0xFFFFu));
  EXPECT_EQ(axl::bit_ones<int32_t>, int32_t(0xFFFFFFFFu));
  EXPECT_EQ(axl::bit_ones<uint32_t>, uint32_t(0xFFFFFFFFu));
  EXPECT_EQ(axl::bit_ones<int64_t>, int64_t(0xFFFFFFFFFFFFFFFFu));
  EXPECT_EQ(axl::bit_ones<uint64_t>, uint64_t(0xFFFFFFFFFFFFFFFFu));
}

TEST_CASE(axl_commons_neg) {
  EXPECT_EQ(axl::neg(int8_t(0)), int8_t(0));
  EXPECT_EQ(axl::neg(uint8_t(0)), uint8_t(0));
  EXPECT_EQ(axl::neg(int16_t(0)), int16_t(0));
  EXPECT_EQ(axl::neg(uint16_t(0)), uint16_t(0));
  EXPECT_EQ(axl::neg(int32_t(0)), int32_t(0));
  EXPECT_EQ(axl::neg(uint32_t(0)), uint32_t(0));
  EXPECT_EQ(axl::neg(int64_t(0)), int64_t(0));
  EXPECT_EQ(axl::neg(uint64_t(0)), uint64_t(0));

  EXPECT_EQ(axl::neg(int8_t(1)), int8_t(-1));
  EXPECT_EQ(axl::neg(uint8_t(1)), uint8_t(0xFF));
  EXPECT_EQ(axl::neg(int16_t(1)), int16_t(-1));
  EXPECT_EQ(axl::neg(uint16_t(1)), uint16_t(0xFFFF));
  EXPECT_EQ(axl::neg(int32_t(1)), int32_t(-1));
  EXPECT_EQ(axl::neg(uint32_t(1)), uint32_t(0xFFFFFFFF));
  EXPECT_EQ(axl::neg(int64_t(1)), int64_t(-1));
  EXPECT_EQ(axl::neg(uint64_t(1)), uint64_t(0xFFFFFFFFFFFFFFFF));

  EXPECT_EQ(axl::neg(int8_t(-1)), int8_t(0x1));
  EXPECT_EQ(axl::neg(uint8_t(0xFF)), uint8_t(1));
  EXPECT_EQ(axl::neg(int16_t(-1)), uint16_t(1));
  EXPECT_EQ(axl::neg(uint16_t(0xFFFF)), uint16_t(1));
  EXPECT_EQ(axl::neg(int32_t(0xFFFF0000)), int32_t(0x00010000));
  EXPECT_EQ(axl::neg(uint32_t(0xFFFF0000)), uint32_t(0x00010000));
  EXPECT_EQ(axl::neg(int64_t(0xFFFF000000000000)), int64_t(0x0001000000000000));
  EXPECT_EQ(axl::neg(uint64_t(0xFFFF000000000000)), uint64_t(0x0001000000000000));
}

TEST_CASE_T(axl_commons_bool_as, test_int_enum_types) {
  EXPECT_EQ(axl::bool_as<T>(false), T(0));
  EXPECT_EQ(axl::bool_as<T>(true), T(1));
}

TEST_CASE_T(axl_commons_bool_as_mask, test_int_enum_types) {
  EXPECT_EQ(axl::bool_as_mask<T>(false), T(0x00));
  EXPECT_EQ(axl::bool_as_mask<T>(true), axl::bit_ones<T>);
}

TEST_CASE_T(axl_commons_is_between, test_int_enum_types) {
  EXPECT_TRUE(axl::is_between<T>(T(10), T(10), T(20)));
  EXPECT_TRUE(axl::is_between<T>(T(11), T(10), T(20)));
  EXPECT_TRUE(axl::is_between<T>(T(20), T(10), T(20)));

  EXPECT_FALSE(axl::is_between<T>(T(9), T(10), T(20)));
  EXPECT_FALSE(axl::is_between<T>(T(21), T(10), T(20)));
  EXPECT_FALSE(axl::is_between<T>(T(101), T(10), T(20)));
}

TEST_CASE(axl_commons_test) {
  EXPECT_FALSE(axl::test(0xFF00, 0x00FF));
  EXPECT_TRUE(axl::test(0xFF00, 0xFFFF));
  EXPECT_FALSE(axl::test(Enum::B, Enum::F));
  EXPECT_TRUE(axl::test(Enum::Z, Enum::B));
}

TEST_CASE_T(axl_commons_bit_test, test_int_types) {
  size_t kBitSize = axl::bit_size_of<T>;
  for (uint32_t i = 0u; i < kBitSize; i++) {
    EXPECT_TRUE(axl::bit_test(T(1) << i, i));
  }
}

TEST_CASE(axl_commons_shl) {
  EXPECT_EQ(axl::shl(int32_t(0x00001111), 16), int32_t(0x11110000u));
  EXPECT_EQ(axl::shl(uint32_t(0x11111111), 16), uint32_t(0x11110000u));
  EXPECT_EQ(axl::shl(uint64_t(0x11111111), 16), uint64_t(0x0000111111110000u));

  EXPECT_EQ(axl::shl_wrap(int32_t(0x00001111), 16), int32_t(0x11110000u));
  EXPECT_EQ(axl::shl_wrap(int32_t(0x00001111), 48), int32_t(0x11110000u));
  EXPECT_EQ(axl::shl_wrap(uint32_t(0x11111111), 16), uint32_t(0x11110000u));
  EXPECT_EQ(axl::shl_wrap(uint32_t(0x11111111), 32), uint32_t(0x11111111u));
  EXPECT_EQ(axl::shl_wrap(uint32_t(0x11111111), 48), uint32_t(0x11110000u));
  EXPECT_EQ(axl::shl_wrap(uint64_t(0x11111111), 16), uint64_t(0x0000111111110000u));
  EXPECT_EQ(axl::shl_wrap(uint64_t(0x11111111), 64), uint64_t(0x11111111u));
  EXPECT_EQ(axl::shl_wrap(uint64_t(0x11111111), 80), uint64_t(0x0000111111110000u));
}

TEST_CASE(axl_commons_shr) {
  EXPECT_EQ(axl::shr(int32_t(0x11110000u), 16), int32_t(0x00001111u));
  EXPECT_EQ(axl::shr(uint32_t(0x11110000u), 16), uint32_t(0x00001111u));
  EXPECT_EQ(axl::shr(uint64_t(0x11110000u), 32), uint32_t(0x00000000u));

  EXPECT_EQ(axl::shr_wrap(int32_t(0x11110000u), 16), int32_t(0x00001111u));
  EXPECT_EQ(axl::shr_wrap(int32_t(0x11110000u), 48), int32_t(0x00001111u));
  EXPECT_EQ(axl::shr_wrap(uint32_t(0x11110000u), 16), uint32_t(0x00001111u));
  EXPECT_EQ(axl::shr_wrap(uint32_t(0x11110000u), 32), uint32_t(0x11110000u));
  EXPECT_EQ(axl::shr_wrap(uint32_t(0x11110000u), 48), uint32_t(0x00001111u));
  EXPECT_EQ(axl::shr_wrap(uint64_t(0x11110000u), 32), uint64_t(0x00000000u));
  EXPECT_EQ(axl::shr_wrap(uint64_t(0x11110000u), 64), uint64_t(0x11110000u));
  EXPECT_EQ(axl::shr_wrap(uint64_t(0x11110000u), 80), uint64_t(0x00001111u));
}

TEST_CASE(axl_commons_sar) {
  EXPECT_EQ(axl::sar(int32_t(0xFFFF0000u), 0), int32_t(0xFFFF0000u));
  EXPECT_EQ(axl::sar(int32_t(0xFFFF0000u), 16), int32_t(0xFFFFFFFFu));
  EXPECT_EQ(axl::sar(uint32_t(0xFFFF0000u), 16), uint32_t(0xFFFFFFFFu));
  EXPECT_EQ(axl::sar(uint64_t(0x8000000000000000u), 0), uint64_t(0x8000000000000000u));
  EXPECT_EQ(axl::sar(uint64_t(0x8000000000000000u), 1), uint64_t(0xC000000000000000u));
  EXPECT_EQ(axl::sar(uint64_t(0x8000000000000000u), 63), uint64_t(0xFFFFFFFFFFFFFFFFu));

  EXPECT_EQ(axl::sar_wrap(int32_t(0xFFFF0000u), 0), int32_t(0xFFFF0000u));
  EXPECT_EQ(axl::sar_wrap(int32_t(0xFFFF1010u), 16), int32_t(0xFFFFFFFFu));
  EXPECT_EQ(axl::sar_wrap(int32_t(0xFFFF0000u), 32), int32_t(0xFFFF0000u));
  EXPECT_EQ(axl::sar_wrap(int32_t(0xFFFF1010u), 48), int32_t(0xFFFFFFFFu));
  EXPECT_EQ(axl::sar_wrap(uint32_t(0xFFFF0000u), 0), uint32_t(0xFFFF0000u));
  EXPECT_EQ(axl::sar_wrap(uint32_t(0xFFFF1010u), 16), uint32_t(0xFFFFFFFFu));
  EXPECT_EQ(axl::sar_wrap(uint32_t(0xFFFF0000u), 48), uint32_t(0xFFFFFFFFu));
  EXPECT_EQ(axl::sar_wrap(uint64_t(0x8000000000000000u), 0), uint64_t(0x8000000000000000u));
  EXPECT_EQ(axl::sar_wrap(uint64_t(0x8000000000000000u), 63), uint64_t(0xFFFFFFFFFFFFFFFFu));
  EXPECT_EQ(axl::sar_wrap(uint64_t(0x8000000000000000u), 127), uint64_t(0xFFFFFFFFFFFFFFFFu));
  EXPECT_EQ(axl::sar_wrap(uint64_t(0x8000000000000000u), 128), uint64_t(0x8000000000000000u));
}

TEST_CASE(axl_commons_rol) {
  EXPECT_EQ(axl::rol(uint32_t(0xFF000000u), 8), uint32_t(0x000000FFu));
  EXPECT_EQ(axl::rol(uint32_t(0xFFFF0000u), 16), uint32_t(0x0000FFFFu));
  EXPECT_EQ(axl::rol(uint64_t(0xFF00000000000000), 16), uint64_t(0x000000000000FF00u));
  EXPECT_EQ(axl::rol<int32_t>(0x00100000 , 16), 0x00000010 );
  EXPECT_EQ(axl::rol<uint32_t>(0x00100000u, 16), 0x00000010u);

  EXPECT_EQ(axl::rol_wrap(uint32_t(0xFF000000u), 8), uint32_t(0x000000FFu));
  EXPECT_EQ(axl::rol_wrap(uint32_t(0xFF000000u), 40), uint32_t(0x000000FFu));
  EXPECT_EQ(axl::rol_wrap(uint32_t(0xFFFF0000u), 16), uint32_t(0x0000FFFFu));
  EXPECT_EQ(axl::rol_wrap(uint32_t(0xFFFF0000u), 48), uint32_t(0x0000FFFFu));
  EXPECT_EQ(axl::rol_wrap(uint64_t(0xFF00000000000000), 16), uint64_t(0x000000000000FF00u));
  EXPECT_EQ(axl::rol_wrap(uint64_t(0xFF00000000000000), 80), uint64_t(0x000000000000FF00u));
  EXPECT_EQ(axl::rol_wrap<int32_t>(0x00100000 , 16), 0x00000010 );
  EXPECT_EQ(axl::rol_wrap<int32_t>(0x00100000 , 48), 0x00000010 );
  EXPECT_EQ(axl::rol_wrap<uint32_t>(0x00100000u, 16), 0x00000010u);
  EXPECT_EQ(axl::rol_wrap<uint32_t>(0x00100000u, 48), 0x00000010u);
}

TEST_CASE(axl_commons_ror) {
  EXPECT_EQ(axl::ror(uint32_t(0xFF000000u), 8), uint32_t(0x00FF0000u));
  EXPECT_EQ(axl::ror(uint32_t(0xFFFF0000u), 16), uint32_t(0x0000FFFFu));
  EXPECT_EQ(axl::ror(uint64_t(0xFF00000000000000), 16), uint64_t(0x0000FF0000000000u));
  EXPECT_EQ(axl::ror<int32_t>(0x00001000 , 16), 0x10000000 );
  EXPECT_EQ(axl::ror<uint32_t>(0x00001000u, 16), 0x10000000u);

  EXPECT_EQ(axl::ror_wrap(uint32_t(0xFF000000u), 8), uint32_t(0x00FF0000u));
  EXPECT_EQ(axl::ror_wrap(uint32_t(0xFF000000u), 40), uint32_t(0x00FF0000u));
  EXPECT_EQ(axl::ror_wrap(uint32_t(0xFFFF0000u), 16), uint32_t(0x0000FFFFu));
  EXPECT_EQ(axl::ror_wrap(uint32_t(0xFFFF0000u), 48), uint32_t(0x0000FFFFu));
  EXPECT_EQ(axl::ror_wrap(uint64_t(0xFF00000000000000), 16), uint64_t(0x0000FF0000000000u));
  EXPECT_EQ(axl::ror_wrap(uint64_t(0xFF00000000000000), 80), uint64_t(0x0000FF0000000000u));
  EXPECT_EQ(axl::ror_wrap<int32_t>(0x00001000 , 16), 0x10000000 );
  EXPECT_EQ(axl::ror_wrap<int32_t>(0x00001000 , 48), 0x10000000 );
  EXPECT_EQ(axl::ror_wrap<uint32_t>(0x00001000u, 16), 0x10000000u);
  EXPECT_EQ(axl::ror_wrap<uint32_t>(0x00001000u, 48), 0x10000000u);
}

TEST_CASE_T(axl_commons_clz, test_int_types) {
  size_t kBitSize = axl::bit_size_of<T>;
  for (uint32_t i = 0u; i < kBitSize; i++) {
    EXPECT_EQ(axl::clz(axl::shl<T>(T(0x01u), i)), unsigned(axl::max(int(kBitSize - 1u) - int(i), 0)));
    EXPECT_EQ(axl::clz(axl::shl<T>(T(0x0Fu), i)), unsigned(axl::max(int(kBitSize - 4u) - int(i), 0)));
    EXPECT_EQ(axl::clz(axl::shl<T>(T(0x7Fu), i)), unsigned(axl::max(int(kBitSize - 7u) - int(i), 0)));
    EXPECT_EQ(axl::clz(axl::shl<T>(T(0xFFu), i)), unsigned(axl::max(int(kBitSize - 8u) - int(i), 0)));
  }
}

TEST_CASE_T(axl_commons_ctz, test_int_types) {
  size_t kBitSize = axl::bit_size_of<T>;
  for (uint32_t i = 0u; i < kBitSize; i++) {
    EXPECT_EQ(axl::ctz(T(T(1u) << i)), i);
    EXPECT_EQ(axl::ctz(T(T(1u) << i)), i);
    EXPECT_EQ(axl::ctz(T(T(1u) << i)), i);
    EXPECT_EQ(axl::ctz(T(T(1u) << i)), i);
    EXPECT_EQ(axl::ctz(T(T(0x3u) << i)), i);
    EXPECT_EQ(axl::ctz(T(T(0xFFu) << i)), i);
    EXPECT_EQ(axl::ctz(T(T(0xFFu) << i)), i);

    if constexpr (sizeof(T) >= 4) {
      EXPECT_EQ(axl::ctz(T(0x80000000u)), 31u);
      EXPECT_EQ(axl::ctz(T(0x88888888u)), 3u);
      EXPECT_EQ(axl::ctz(T(0x11111111u)), 0u);
      EXPECT_EQ(axl::ctz(T(0x12345678u)), 3u);
    }
  }
}

TEST_CASE_T(axl_commons_popcnt, test_int_types) {
  size_t kBitSize = axl::bit_size_of<T>;
  for (uint32_t i = 0u; i < kBitSize; i++) {
    EXPECT_EQ(axl::popcnt(axl::shl(T(1), i)), 1u);
  }

  if constexpr (sizeof(T) >= 4) {
    EXPECT_EQ(axl::popcnt(0x000000F0), 4u);
    EXPECT_EQ(axl::popcnt(0x10101010), 4u);
    EXPECT_EQ(axl::popcnt(0xFF000000), 8u);
    EXPECT_EQ(axl::popcnt(0xFFFFFFF7), 31u);
    EXPECT_EQ(axl::popcnt(0x7FFFFFFF), 31u);
  }
}

template<typename T>
T leading_bits_run_reference(uint32_t n) {
  T mask{};
  for (uint32_t i = 0; i < n; i++) {
    mask = T(mask | (T(1) << (axl::bit_size_of<T> - 1 - i)));
  }
  return mask;
}

template<typename T>
T trailing_bits_run_reference(uint32_t n) {
  T mask{};
  for (uint32_t i = 0; i < n; i++) {
    mask = T(mask | (T(1) << i));
  }
  return mask;
}

TEST_CASE_T(axl_commons_leading_bits_run, test_int_types) {
  size_t kBitSize = axl::bit_size_of<T>;
  for (uint32_t i = 0u; i <= kBitSize; i++) {
    T expected_bits = leading_bits_run_reference<T>(i);
    EXPECT_EQ(axl::leading_bits_run_const<T>(i), expected_bits);
    EXPECT_EQ(axl::leading_bits_run<T>(i), expected_bits);
  }
}

TEST_CASE_T(axl_commons_trailing_bits_run, test_int_types) {
  size_t kBitSize = axl::bit_size_of<T>;
  for (uint32_t i = 0u; i <= kBitSize; i++) {
    T expected_bits = trailing_bits_run_reference<T>(i);
    EXPECT_EQ(axl::trailing_bits_run_const<T>(i), expected_bits);
    EXPECT_EQ(axl::trailing_bits_run<T>(i), expected_bits);
  }
}

TEST_CASE_T(axl_commons_leading_bits_run_nz, test_int_types) {
  size_t kBitSize = axl::bit_size_of<T>;
  for (uint32_t i = 1u; i <= kBitSize; i++) {
    T expected_bits = leading_bits_run_reference<T>(i);
    EXPECT_EQ(axl::leading_bits_run_nz<T>(i), expected_bits);
  }
}

TEST_CASE_T(axl_commons_trailing_bits_run_nz, test_int_types) {
  size_t kBitSize = axl::bit_size_of<T>;
  for (uint32_t i = 1u; i <= kBitSize; i++) {
    T expected_bits = trailing_bits_run_reference<T>(i);
    EXPECT_EQ(axl::trailing_bits_run_nz<T>(i), expected_bits);
  }
}

TEST_CASE_T(axl_commons_bit_mask, test_int_types) {
  size_t kBitSize = axl::bit_size_of<T>;
  for (uint32_t i = 0u; i < kBitSize; i++) {
    EXPECT_EQ(axl::bit_mask<T>(i), axl::shl(T(1), i));
  }

  EXPECT_EQ(axl::bit_mask<T>(0, 1, 7), T(0x83u));
  EXPECT_EQ(axl::bit_mask<T>(0, 1, 2, 3, 4, 5, 6, 7), T(0xFFu));
}

TEST_CASE(axl_commons_leading_bit_mask) {
  for (uint32_t i = 0u; i < 32u; i++) {
    EXPECT_EQ(axl::leading_bit_mask<uint32_t>(i), uint32_t(1) << (31u - i));
  }

  for (uint32_t i = 0u; i < 64u; i++) {
    EXPECT_EQ(axl::leading_bit_mask<uint64_t>(i), uint64_t(1) << (63u - i));
  }

  EXPECT_EQ(axl::leading_bit_mask<uint32_t>(0, 1, 7), 0xC1000000u);
}

TEST_CASE(axl_commons_extract_trailing_bit) {
  for (uint32_t i = 0; i < 32; i++) EXPECT_EQ(axl::extract_trailing_bit(uint32_t(1) << i), uint32_t(1) << i);
  for (uint32_t i = 0; i < 31; i++) EXPECT_EQ(axl::extract_trailing_bit(uint32_t(3) << i), uint32_t(1) << i);
  for (uint32_t i = 0; i < 64; i++) EXPECT_EQ(axl::extract_trailing_bit(uint64_t(1) << i), uint64_t(1) << i);
  for (uint32_t i = 0; i < 63; i++) EXPECT_EQ(axl::extract_trailing_bit(uint64_t(3) << i), uint64_t(1) << i);

  EXPECT_EQ(axl::extract_trailing_bit(uint32_t(0x00000000u)), 0x00000000u);
  EXPECT_EQ(axl::extract_trailing_bit(uint32_t(0x00000001u)), 0x00000001u);
  EXPECT_EQ(axl::extract_trailing_bit(uint32_t(0x0000000Fu)), 0x00000001u);
  EXPECT_EQ(axl::extract_trailing_bit(uint32_t(0xFFFFFFFFu)), 0x00000001u);
}

TEST_CASE(axl_commons_fill_trailing_bits) {
  EXPECT_EQ(axl::fill_trailing_bits(uint8_t(0u)), uint8_t(0u));
  EXPECT_EQ(axl::fill_trailing_bits(uint8_t(1u)), uint8_t(1u));
  EXPECT_EQ(axl::fill_trailing_bits(uint8_t(2u)), uint8_t(3u));
  EXPECT_EQ(axl::fill_trailing_bits(uint8_t(3u)), uint8_t(3u));
  EXPECT_EQ(axl::fill_trailing_bits(uint8_t(4u)), uint8_t(7u));
  EXPECT_EQ(axl::fill_trailing_bits(uint8_t(0x80u)), uint8_t(0xFFu));
  EXPECT_EQ(axl::fill_trailing_bits(uint8_t(0xFEu)), uint8_t(0xFFu));
  EXPECT_EQ(axl::fill_trailing_bits(uint8_t(0xFFu)), uint8_t(0xFFu));
  EXPECT_EQ(axl::fill_trailing_bits(uint16_t(0u)), uint16_t(0u));
  EXPECT_EQ(axl::fill_trailing_bits(uint16_t(1u)), uint16_t(1u));
  EXPECT_EQ(axl::fill_trailing_bits(uint16_t(2u)), uint16_t(3u));
  EXPECT_EQ(axl::fill_trailing_bits(uint16_t(3u)), uint16_t(3u));
  EXPECT_EQ(axl::fill_trailing_bits(uint16_t(4u)), uint16_t(7u));
  EXPECT_EQ(axl::fill_trailing_bits(uint16_t(0x8000u)), uint16_t(0xFFFFu));
  EXPECT_EQ(axl::fill_trailing_bits(uint16_t(0xFFFEu)), uint16_t(0xFFFFu));
  EXPECT_EQ(axl::fill_trailing_bits(uint16_t(0xFFFFu)), uint16_t(0xFFFFu));
  EXPECT_EQ(axl::fill_trailing_bits(uint32_t(0u)), uint32_t(0u));
  EXPECT_EQ(axl::fill_trailing_bits(uint32_t(1u)), uint32_t(1u));
  EXPECT_EQ(axl::fill_trailing_bits(uint32_t(2u)), uint32_t(3u));
  EXPECT_EQ(axl::fill_trailing_bits(uint32_t(3u)), uint32_t(3u));
  EXPECT_EQ(axl::fill_trailing_bits(uint32_t(4u)), uint32_t(7u));
  EXPECT_EQ(axl::fill_trailing_bits(uint32_t(0x80000000u)), uint32_t(0xFFFFFFFFu));
  EXPECT_EQ(axl::fill_trailing_bits(uint32_t(0xFFFFFFFEu)), uint32_t(0xFFFFFFFFu));
  EXPECT_EQ(axl::fill_trailing_bits(uint32_t(0xFFFFFFFFu)), uint32_t(0xFFFFFFFFu));
  EXPECT_EQ(axl::fill_trailing_bits(uint64_t(0u)), uint64_t(0u));
  EXPECT_EQ(axl::fill_trailing_bits(uint64_t(1u)), uint64_t(1u));
  EXPECT_EQ(axl::fill_trailing_bits(uint64_t(2u)), uint64_t(3u));
  EXPECT_EQ(axl::fill_trailing_bits(uint64_t(3u)), uint64_t(3u));
  EXPECT_EQ(axl::fill_trailing_bits(uint64_t(4u)), uint64_t(7u));
  EXPECT_EQ(axl::fill_trailing_bits(uint64_t(0x8000000000000000u)), uint64_t(0xFFFFFFFFFFFFFFFFu));
  EXPECT_EQ(axl::fill_trailing_bits(uint64_t(0xFFFFFFFFFFFFFFFEu)), uint64_t(0xFFFFFFFFFFFFFFFFu));
  EXPECT_EQ(axl::fill_trailing_bits(uint64_t(0xFFFFFFFFFFFFFFFFu)), uint64_t(0xFFFFFFFFFFFFFFFFu));
}

TEST_CASE(axl_commons_is_leading_mask) {
  EXPECT_TRUE(axl::is_leading_mask(uint32_t(0x80000000u)));
  EXPECT_TRUE(axl::is_leading_mask(uint32_t(0xF0000000u)));
  EXPECT_TRUE(axl::is_leading_mask(uint32_t(0xFF000000u)));
  EXPECT_TRUE(axl::is_leading_mask(uint32_t(0xFFFF0000u)));
  EXPECT_TRUE(axl::is_leading_mask(uint32_t(0xFFFFFFFFu)));

  EXPECT_FALSE(axl::is_leading_mask(uint32_t(0x00000001u)));
  EXPECT_FALSE(axl::is_leading_mask(uint32_t(0x7FFFFFFFu)));
  EXPECT_FALSE(axl::is_leading_mask(uint32_t(0x00000000u)));
  EXPECT_FALSE(axl::is_leading_mask(uint32_t(0xF00000FFu)));
  EXPECT_FALSE(axl::is_leading_mask(uint32_t(0xFFFF0EFFu)));
  EXPECT_FALSE(axl::is_leading_mask(uint32_t(0x80000EFFu)));
  EXPECT_FALSE(axl::is_leading_mask(uint32_t(0x80000001u)));
}

TEST_CASE(axl_commons_is_trailing_mask) {
  EXPECT_TRUE(axl::is_trailing_mask(uint32_t(0xFFFFFFFFu)));
  EXPECT_TRUE(axl::is_trailing_mask(uint32_t(0x00000003u)));
  EXPECT_TRUE(axl::is_trailing_mask(uint32_t(0x0000000Fu)));
  EXPECT_TRUE(axl::is_trailing_mask(uint32_t(0x000FFFFFu)));
  EXPECT_TRUE(axl::is_trailing_mask(uint32_t(0x0FFFFFFFu)));
  EXPECT_TRUE(axl::is_trailing_mask(uint32_t(0x7FFFFFFFu)));

  EXPECT_FALSE(axl::is_trailing_mask(uint32_t(0x00000000u)));
  EXPECT_FALSE(axl::is_trailing_mask(uint32_t(0x80000000u)));
  EXPECT_FALSE(axl::is_trailing_mask(uint32_t(0xF0000000u)));
  EXPECT_FALSE(axl::is_trailing_mask(uint32_t(0xFF000000u)));
  EXPECT_FALSE(axl::is_trailing_mask(uint32_t(0xFFFF0EFFu)));
  EXPECT_FALSE(axl::is_trailing_mask(uint32_t(0x80000EFFu)));
  EXPECT_FALSE(axl::is_trailing_mask(uint32_t(0x80000001u)));
}

TEST_CASE(axl_commons_is_consecutive_mask) {
  for (uint32_t i = 0; i < 32; i++) EXPECT_TRUE(axl::is_consecutive_mask(uint32_t(1) << i));
  for (uint32_t i = 0; i < 64; i++) EXPECT_TRUE(axl::is_consecutive_mask(uint64_t(1) << i));

  EXPECT_TRUE(axl::is_consecutive_mask(uint32_t(0x00000003u)));
  EXPECT_TRUE(axl::is_consecutive_mask(uint32_t(0x0000000Fu)));
  EXPECT_TRUE(axl::is_consecutive_mask(uint32_t(0x000FFFFFu)));
  EXPECT_TRUE(axl::is_consecutive_mask(uint32_t(0x0FFFFFFFu)));
  EXPECT_TRUE(axl::is_consecutive_mask(uint32_t(0x7FFFFFFFu)));
  EXPECT_TRUE(axl::is_consecutive_mask(uint32_t(0xFFFFFFFFu)));
  EXPECT_TRUE(axl::is_consecutive_mask(uint32_t(0x0FFFFFF0u)));
  EXPECT_TRUE(axl::is_consecutive_mask(uint32_t(0x00FFFF00u)));
  EXPECT_TRUE(axl::is_consecutive_mask(uint32_t(0x000FF000u)));
  EXPECT_TRUE(axl::is_consecutive_mask(uint32_t(0x0003C000u)));
  EXPECT_TRUE(axl::is_consecutive_mask(uint32_t(0x00018000u)));
  EXPECT_TRUE(axl::is_consecutive_mask(uint32_t(0xF0000000u)));

  EXPECT_FALSE(axl::is_consecutive_mask(uint32_t(0x00000000u)));
  EXPECT_FALSE(axl::is_consecutive_mask(uint32_t(0xFFFF0EFFu)));
  EXPECT_FALSE(axl::is_consecutive_mask(uint32_t(0x80000EFFu)));
  EXPECT_FALSE(axl::is_consecutive_mask(uint32_t(0x80000001u)));
  EXPECT_FALSE(axl::is_consecutive_mask(uint32_t(0x10101010u)));
  EXPECT_FALSE(axl::is_consecutive_mask(uint32_t(0x01010101u)));
}

TEST_CASE(axl_commons_is_power_of_2) {
  EXPECT_FALSE(axl::is_power_of_2(uint8_t(0)));
  EXPECT_FALSE(axl::is_power_of_2(uint16_t(0)));
  EXPECT_FALSE(axl::is_power_of_2(uint32_t(0)));
  EXPECT_FALSE(axl::is_power_of_2(uint64_t(0)));

  EXPECT_FALSE(axl::is_power_of_2(uint8_t(0xFFu)));
  EXPECT_FALSE(axl::is_power_of_2(uint16_t(0xFFFFu)));
  EXPECT_FALSE(axl::is_power_of_2(uint32_t(0xFFFFFFFFu)));
  EXPECT_FALSE(axl::is_power_of_2(uint64_t(0xFFFFFFFFFFFFFFFFu)));

  for (uint32_t i = 0; i < 32; i++) {
    EXPECT_TRUE(axl::is_power_of_2(uint32_t(1) << i));
    EXPECT_FALSE(axl::is_power_of_2((uint32_t(1) << i) ^ 0x001101));
  }

  for (uint32_t i = 0; i < 64; i++) {
    EXPECT_TRUE(axl::is_power_of_2(uint64_t(1) << i));
    EXPECT_FALSE(axl::is_power_of_2((uint64_t(1) << i) ^ 0x001101));
  }
}

TEST_CASE(axl_commons_is_power_of_2_up_to) {
  EXPECT_FALSE(axl::is_power_of_2_up_to(uint8_t(0), 8));
  EXPECT_FALSE(axl::is_power_of_2_up_to(uint16_t(0), 8));
  EXPECT_FALSE(axl::is_power_of_2_up_to(uint32_t(0), 8));
  EXPECT_FALSE(axl::is_power_of_2_up_to(uint64_t(0), 8));

  EXPECT_FALSE(axl::is_power_of_2_up_to(uint8_t(0xFFu), 8));
  EXPECT_FALSE(axl::is_power_of_2_up_to(uint16_t(0xFFFFu), 8));
  EXPECT_FALSE(axl::is_power_of_2_up_to(uint32_t(0xFFFFFFFFu), 8));
  EXPECT_FALSE(axl::is_power_of_2_up_to(uint64_t(0xFFFFFFFFFFFFFFFFu), 8));

  EXPECT_TRUE(axl::is_power_of_2_up_to(uint32_t(1), 8));
  EXPECT_TRUE(axl::is_power_of_2_up_to(uint32_t(2), 8));
  EXPECT_FALSE(axl::is_power_of_2_up_to(uint32_t(3), 8));
  EXPECT_TRUE(axl::is_power_of_2_up_to(uint32_t(4), 8));
  EXPECT_TRUE(axl::is_power_of_2_up_to(uint32_t(8), 8));
  EXPECT_FALSE(axl::is_power_of_2_up_to(uint32_t(9), 8));
  EXPECT_FALSE(axl::is_power_of_2_up_to(uint32_t(16), 8));
  EXPECT_FALSE(axl::is_power_of_2_up_to(uint32_t(0xFFFFFFFFu), 8));

  EXPECT_TRUE(axl::is_power_of_2_up_to(uint32_t(16), 16));
  EXPECT_FALSE(axl::is_power_of_2_up_to(uint32_t(32), 16));
}

TEST_CASE(axl_commons_is_zero_or_power_of_2) {
  EXPECT_TRUE(axl::is_zero_or_power_of_2(uint8_t(0)));
  EXPECT_TRUE(axl::is_zero_or_power_of_2(uint16_t(0)));
  EXPECT_TRUE(axl::is_zero_or_power_of_2(uint32_t(0)));
  EXPECT_TRUE(axl::is_zero_or_power_of_2(uint64_t(0)));

  EXPECT_FALSE(axl::is_zero_or_power_of_2(uint8_t(0xFFu)));
  EXPECT_FALSE(axl::is_zero_or_power_of_2(uint16_t(0xFFFFu)));
  EXPECT_FALSE(axl::is_zero_or_power_of_2(uint32_t(0xFFFFFFFFu)));
  EXPECT_FALSE(axl::is_zero_or_power_of_2(uint64_t(0xFFFFFFFFFFFFFFFFu)));

  for (uint32_t i = 0; i < 32; i++) {
    EXPECT_TRUE(axl::is_zero_or_power_of_2(uint32_t(1) << i));
    EXPECT_FALSE(axl::is_zero_or_power_of_2((uint32_t(1) << i) ^ 0x001101));
  }

  for (uint32_t i = 0; i < 64; i++) {
    EXPECT_TRUE(axl::is_zero_or_power_of_2(uint64_t(1) << i));
    EXPECT_FALSE(axl::is_zero_or_power_of_2((uint64_t(1) << i) ^ 0x001101));
  }
}

TEST_CASE(axl_commons_is_zero_or_power_of_2_up_to) {
  EXPECT_TRUE(axl::is_zero_or_power_of_2_up_to(uint8_t(0), 8));
  EXPECT_TRUE(axl::is_zero_or_power_of_2_up_to(uint16_t(0), 8));
  EXPECT_TRUE(axl::is_zero_or_power_of_2_up_to(uint32_t(0), 8));
  EXPECT_TRUE(axl::is_zero_or_power_of_2_up_to(uint64_t(0), 8));

  EXPECT_FALSE(axl::is_zero_or_power_of_2_up_to(uint8_t(0xFFu), 8));
  EXPECT_FALSE(axl::is_zero_or_power_of_2_up_to(uint16_t(0xFFFFu), 8));
  EXPECT_FALSE(axl::is_zero_or_power_of_2_up_to(uint32_t(0xFFFFFFFFu), 8));
  EXPECT_FALSE(axl::is_zero_or_power_of_2_up_to(uint64_t(0xFFFFFFFFFFFFFFFFu), 8));

  EXPECT_TRUE(axl::is_zero_or_power_of_2_up_to(uint32_t(1), 8));
  EXPECT_TRUE(axl::is_zero_or_power_of_2_up_to(uint32_t(2), 8));
  EXPECT_FALSE(axl::is_zero_or_power_of_2_up_to(uint32_t(3), 8));
  EXPECT_TRUE(axl::is_zero_or_power_of_2_up_to(uint32_t(4), 8));
  EXPECT_TRUE(axl::is_zero_or_power_of_2_up_to(uint32_t(8), 8));
  EXPECT_FALSE(axl::is_zero_or_power_of_2_up_to(uint32_t(9), 8));
  EXPECT_FALSE(axl::is_zero_or_power_of_2_up_to(uint32_t(16), 8));
  EXPECT_FALSE(axl::is_zero_or_power_of_2_up_to(uint32_t(0xFFFFFFFFu), 8));

  EXPECT_TRUE(axl::is_zero_or_power_of_2_up_to(uint32_t(16), 16));
  EXPECT_FALSE(axl::is_zero_or_power_of_2_up_to(uint32_t(32), 16));
}

TEST_CASE(axl_commons_has_at_least_2_bits_set) {
  EXPECT_FALSE(axl::has_at_least_2_bits_set(uint32_t(0)));

  for (uint32_t i = 0; i < 32; i++) {
    EXPECT_FALSE(axl::has_at_least_2_bits_set(uint32_t(1) << i));
  }

  for (uint32_t i = 1; i < 32; i++) {
    EXPECT_TRUE(axl::has_at_least_2_bits_set(uint32_t(1) | (uint32_t(1) << i)));
  }

  for (uint32_t i = 0; i < 64; i++) {
    EXPECT_FALSE(axl::has_at_least_2_bits_set(uint64_t(1) << i));
  }

  for (uint32_t i = 1; i < 64; i++) {
    EXPECT_TRUE(axl::has_at_least_2_bits_set(uint64_t(1) | (uint64_t(1) << i)));
  }

  EXPECT_TRUE(axl::has_at_least_2_bits_set(uint32_t(0x80808080u)));
  EXPECT_TRUE(axl::has_at_least_2_bits_set(uint32_t(0xFFFFFFFFu)));
  EXPECT_TRUE(axl::has_at_least_2_bits_set(uint64_t(0x0010000000000100)));
  EXPECT_TRUE(axl::has_at_least_2_bits_set(uint64_t(0xFFFFFFFFFFFFFFFF)));
}

TEST_CASE_T(axl_commons_pair_bits, test_int_types) {
  size_t kBitSize = axl::bit_size_of<T>;
  for (uint32_t i = 0u; i < kBitSize; i++) {
    EXPECT_EQ(axl::pair_bits(axl::shl(T(1), i)), axl::shl(T(0x3), i & 0xFE));
  }
}

TEST_CASE(axl_commons_is_aligned) {
  EXPECT_FALSE(axl::is_aligned<size_t>(0xFFFF, 4u));
  EXPECT_TRUE(axl::is_aligned<size_t>(0xFFF4, 4u));
  EXPECT_TRUE(axl::is_aligned<size_t>(0xFFF8, 8u));
  EXPECT_TRUE(axl::is_aligned<size_t>(0xFFF0, 16u));
}

TEST_CASE(axl_commons_align_up) {
  EXPECT_EQ(axl::align_up<size_t>(0xFFFF, 4), 0x10000u);
  EXPECT_EQ(axl::align_up<size_t>(0xFFF4, 4), 0x0FFF4u);
  EXPECT_EQ(axl::align_up<size_t>(0xFFF8, 8), 0x0FFF8u);
  EXPECT_EQ(axl::align_up<size_t>(0xFFF0, 16), 0x0FFF0u);
  EXPECT_EQ(axl::align_up<size_t>(0xFFF0, 32), 0x10000u);
}

TEST_CASE(axl_commons_align_diff) {
  EXPECT_EQ(axl::align_up_diff<size_t>(0xFFFF, 4), 1u);
  EXPECT_EQ(axl::align_up_diff<size_t>(0xFFF4, 4), 0u);
  EXPECT_EQ(axl::align_up_diff<size_t>(0xFFF8, 8), 0u);
  EXPECT_EQ(axl::align_up_diff<size_t>(0xFFF0, 16), 0u);
  EXPECT_EQ(axl::align_up_diff<size_t>(0xFFF0, 32), 16u);
}

TEST_CASE(axl_commons_align_up_power_of_2) {
  EXPECT_EQ(axl::align_up_power_of_2<size_t>(0x0000), 0x00000u);
  EXPECT_EQ(axl::align_up_power_of_2<size_t>(0xFFFF), 0x10000u);
  EXPECT_EQ(axl::align_up_power_of_2<size_t>(0xF123), 0x10000u);
  EXPECT_EQ(axl::align_up_power_of_2<size_t>(0x0F00), 0x01000u);
  EXPECT_EQ(axl::align_up_power_of_2<size_t>(0x0100), 0x00100u);
  EXPECT_EQ(axl::align_up_power_of_2<size_t>(0x1001), 0x02000u);
}

TEST_CASE(axl_commons_byteswap) {
  EXPECT_EQ(axl::byteswap16(0x0102), 0x0201u);
  EXPECT_EQ(axl::byteswap32(0x01020304), 0x04030201u);
  EXPECT_EQ(axl::byteswap64(uint64_t(0x0102030405060708)), uint64_t(0x0807060504030201));

  EXPECT_EQ(axl::byteswap(uint8_t(0xFE)), uint8_t(0xFE));
  EXPECT_EQ(axl::byteswap(uint16_t(0x0102)), uint16_t(0x0201u));
  EXPECT_EQ(axl::byteswap(uint32_t(0x01020304)), uint32_t(0x04030201u));
  EXPECT_EQ(axl::byteswap(uint64_t(0x0102030405060708)), uint64_t(0x0807060504030201));
}

TEST_CASE(axl_commons_bit_reverse) {
  EXPECT_EQ(axl::bit_reverse(uint8_t(0xFEu)), uint8_t(0x7Fu));
  EXPECT_EQ(axl::bit_reverse(uint8_t(0x01u)), uint8_t(0x80u));
  EXPECT_EQ(axl::bit_reverse(uint16_t(0x0001u)), uint16_t(0x8000u));
  EXPECT_EQ(axl::bit_reverse(uint16_t(0xFE00u)), uint16_t(0x007Fu));
  EXPECT_EQ(axl::bit_reverse(uint32_t(0x00000001u)), uint32_t(0x80000000u));
  EXPECT_EQ(axl::bit_reverse(uint32_t(0xFE000000u)), uint32_t(0x0000007Fu));
  EXPECT_EQ(axl::bit_reverse(uint64_t(0x0000000000000001u)), uint64_t(0x8000000000000000u));
  EXPECT_EQ(axl::bit_reverse(uint64_t(0xFE00000000000000u)), uint64_t(0x000000000000007Fu));
}

TEST_CASE(axl_commons_is_int_n) {
  EXPECT_TRUE(axl::is_int_n<8>(-128));
  EXPECT_TRUE(axl::is_int_n<8>(127));
  EXPECT_FALSE(axl::is_int_n<8>(-129));
  EXPECT_FALSE(axl::is_int_n<8>(-1000));
  EXPECT_FALSE(axl::is_int_n<8>(128));
  EXPECT_FALSE(axl::is_int_n<8>(1000));

  EXPECT_TRUE(axl::is_int_n<9>(-256));
  EXPECT_TRUE(axl::is_int_n<9>(255));
  EXPECT_FALSE(axl::is_int_n<9>(-257));
  EXPECT_FALSE(axl::is_int_n<9>(-1000));
  EXPECT_FALSE(axl::is_int_n<9>(256));
  EXPECT_FALSE(axl::is_int_n<9>(1000));

  EXPECT_TRUE(axl::is_int_n<10>(-512));
  EXPECT_TRUE(axl::is_int_n<10>(511));
  EXPECT_FALSE(axl::is_int_n<10>(-513));
  EXPECT_FALSE(axl::is_int_n<10>(-1000));
  EXPECT_FALSE(axl::is_int_n<10>(512));
  EXPECT_FALSE(axl::is_int_n<10>(1000));

  EXPECT_TRUE(axl::is_int_n<16>(-32768));
  EXPECT_TRUE(axl::is_int_n<16>(32767));
  EXPECT_FALSE(axl::is_int_n<16>(-32769));
  EXPECT_FALSE(axl::is_int_n<16>(-100000));
  EXPECT_FALSE(axl::is_int_n<16>(32768));
  EXPECT_FALSE(axl::is_int_n<16>(100000));

  EXPECT_TRUE(axl::is_int_n<33>(int64_t(4294967295)));
  EXPECT_TRUE(axl::is_int_n<33>(int64_t(-4294967296)));
  EXPECT_FALSE(axl::is_int_n<33>(uint64_t(4294967296)));
  EXPECT_FALSE(axl::is_int_n<33>(uint64_t(0x1FFFFFFFFu)));
  EXPECT_FALSE(axl::is_int_n<33>(uint64_t(0x1FFFFFFFFu) + 1));

  EXPECT_TRUE(axl::is_int_n<32>( 2147483647    ));
  EXPECT_TRUE(axl::is_int_n<32>(-2147483647 - 1));
  EXPECT_FALSE(axl::is_int_n<32>(uint64_t(2147483648u)));
  EXPECT_FALSE(axl::is_int_n<32>(uint64_t(0xFFFFFFFFu)));
  EXPECT_FALSE(axl::is_int_n<32>(uint64_t(0xFFFFFFFFu) + 1));

  EXPECT_TRUE(axl::is_uint_n<8>(0));
  EXPECT_TRUE(axl::is_uint_n<8>(255));
  EXPECT_FALSE(axl::is_uint_n<8>(256));
  EXPECT_FALSE(axl::is_uint_n<8>(1000));
  EXPECT_FALSE(axl::is_uint_n<8>(-1));
  EXPECT_FALSE(axl::is_uint_n<8>(-1000));

  EXPECT_TRUE(axl::is_uint_n<9>(0)  );
  EXPECT_TRUE(axl::is_uint_n<9>(511));
  EXPECT_FALSE(axl::is_uint_n<9>(512));
  EXPECT_FALSE(axl::is_uint_n<9>(1000));
  EXPECT_FALSE(axl::is_uint_n<9>(-1));
  EXPECT_FALSE(axl::is_uint_n<9>(-1000));

  EXPECT_TRUE(axl::is_uint_n<10>(0)  );
  EXPECT_TRUE(axl::is_uint_n<10>(1023));
  EXPECT_FALSE(axl::is_uint_n<10>(1024));
  EXPECT_FALSE(axl::is_uint_n<10>(10000));
  EXPECT_FALSE(axl::is_uint_n<10>(-1));
  EXPECT_FALSE(axl::is_uint_n<10>(-10000));

  EXPECT_TRUE(axl::is_uint_n<12>(0));
  EXPECT_TRUE(axl::is_uint_n<12>(4095));
  EXPECT_FALSE(axl::is_uint_n<12>(4096));
  EXPECT_FALSE(axl::is_uint_n<12>(10000));
  EXPECT_FALSE(axl::is_uint_n<12>(-1));
  EXPECT_FALSE(axl::is_uint_n<12>(-1000));

  EXPECT_TRUE(axl::is_uint_n<16>(0));
  EXPECT_TRUE(axl::is_uint_n<16>(65535));
  EXPECT_FALSE(axl::is_uint_n<16>(65536));
  EXPECT_FALSE(axl::is_uint_n<16>(100000));
  EXPECT_FALSE(axl::is_uint_n<16>(-1));
  EXPECT_FALSE(axl::is_uint_n<16>(-1000));

  EXPECT_TRUE(axl::is_uint_n<32>(uint64_t(0xFFFFFFFF)));
  EXPECT_FALSE(axl::is_uint_n<32>(uint64_t(0xFFFFFFFF) + 1));
  EXPECT_FALSE(axl::is_uint_n<32>(-1));

  EXPECT_TRUE(axl::is_uint_n<33>(uint64_t(0x1FFFFFFFF)));
  EXPECT_FALSE(axl::is_uint_n<33>(uint64_t(0x1FFFFFFFF) + 1));
  EXPECT_FALSE(axl::is_uint_n<33>(-1));
}

TEST_CASE(axl_commons_csa) {
  uint32_t val_hi, val_lo;
  axl::csa<uint32_t>(
    val_hi, val_lo, 0b01011111u,
                    0b10101101u,
                    0b11000011u);

  EXPECT_EQ(val_lo, 0b00110001u);
  EXPECT_EQ(val_hi, 0b11001111u);
}

TEST_CASE_T(axl_commons_add_overflow, dispatch::tests) {
  using Impl = T;

  using axl::did_overflow;
  using axl::OverflowFlag;

  OverflowFlag of{};


  EXPECT_TRUE(Impl::template add_overflow<int32_t>(0, 0, of) == 0 && !did_overflow(of));
  EXPECT_TRUE(Impl::template add_overflow<int32_t>(0, 1, of) == 1 && !did_overflow(of));
  EXPECT_TRUE(Impl::template add_overflow<int32_t>(1, 0, of) == 1 && !did_overflow(of));

  EXPECT_TRUE(Impl::template add_overflow<int32_t>(2147483647, 0, of) == 2147483647 && !did_overflow(of));
  EXPECT_TRUE(Impl::template add_overflow<int32_t>(0, 2147483647, of) == 2147483647 && !did_overflow(of));
  EXPECT_TRUE(Impl::template add_overflow<int32_t>(2147483647, -1, of) == 2147483646 && !did_overflow(of));
  EXPECT_TRUE(Impl::template add_overflow<int32_t>(-1, 2147483647, of) == 2147483646 && !did_overflow(of));

  EXPECT_TRUE(Impl::template add_overflow<int32_t>(-2147483647, 0, of) == -2147483647 && !did_overflow(of));
  EXPECT_TRUE(Impl::template add_overflow<int32_t>(0, -2147483647, of) == -2147483647 && !did_overflow(of));
  EXPECT_TRUE(Impl::template add_overflow<int32_t>(-2147483647, -1, of) == -2147483647 - 1 && !did_overflow(of));
  EXPECT_TRUE(Impl::template add_overflow<int32_t>(-1, -2147483647, of) == -2147483647 - 1 && !did_overflow(of));

  (void)Impl::template add_overflow<int32_t>(2147483647, 1, of); EXPECT_TRUE(did_overflow(of)); of = OverflowFlag(0);
  (void)Impl::template add_overflow<int32_t>(1, 2147483647, of); EXPECT_TRUE(did_overflow(of)); of = OverflowFlag(0);
  (void)Impl::template add_overflow<int32_t>(-2147483647, -2, of); EXPECT_TRUE(did_overflow(of)); of = OverflowFlag(0);
  (void)Impl::template add_overflow<int32_t>(-2, -2147483647, of); EXPECT_TRUE(did_overflow(of)); of = OverflowFlag(0);

  EXPECT_TRUE(Impl::template add_overflow<uint32_t>(0u, 0u, of) == 0 && !did_overflow(of));
  EXPECT_TRUE(Impl::template add_overflow<uint32_t>(0u, 1u, of) == 1 && !did_overflow(of));
  EXPECT_TRUE(Impl::template add_overflow<uint32_t>(1u, 0u, of) == 1 && !did_overflow(of));

  EXPECT_TRUE(Impl::template add_overflow<uint32_t>(2147483647u, 1u, of) == 2147483648u && !did_overflow(of));
  EXPECT_TRUE(Impl::template add_overflow<uint32_t>(1u, 2147483647u, of) == 2147483648u && !did_overflow(of));
  EXPECT_TRUE(Impl::template add_overflow<uint32_t>(0xFFFFFFFFu, 0u, of) == 0xFFFFFFFFu && !did_overflow(of));
  EXPECT_TRUE(Impl::template add_overflow<uint32_t>(0u, 0xFFFFFFFFu, of) == 0xFFFFFFFFu && !did_overflow(of));

  (void)Impl::template add_overflow<uint32_t>(0xFFFFFFFFu, 1u, of); EXPECT_TRUE(did_overflow(of)); of = OverflowFlag(0);
  (void)Impl::template add_overflow<uint32_t>(1u, 0xFFFFFFFFu, of); EXPECT_TRUE(did_overflow(of)); of = OverflowFlag(0);

  (void)Impl::template add_overflow<uint32_t>(0x80000000u, 0xFFFFFFFFu, of); EXPECT_TRUE(did_overflow(of)); of = OverflowFlag(0);
  (void)Impl::template add_overflow<uint32_t>(0xFFFFFFFFu, 0x80000000u, of); EXPECT_TRUE(did_overflow(of)); of = OverflowFlag(0);
  (void)Impl::template add_overflow<uint32_t>(0xFFFFFFFFu, 0xFFFFFFFFu, of); EXPECT_TRUE(did_overflow(of)); of = OverflowFlag(0);
}

TEST_CASE_T(axl_commons_sub_overflow, dispatch::tests) {
  using Impl = T;

  using axl::did_overflow;
  using axl::OverflowFlag;

  OverflowFlag of{};

  EXPECT_TRUE(Impl::template sub_overflow<int32_t>(0, 0, of) ==  0 && !did_overflow(of));
  EXPECT_TRUE(Impl::template sub_overflow<int32_t>(0, 1, of) == -1 && !did_overflow(of));
  EXPECT_TRUE(Impl::template sub_overflow<int32_t>(1, 0, of) ==  1 && !did_overflow(of));
  EXPECT_TRUE(Impl::template sub_overflow<int32_t>(0, -1, of) ==  1 && !did_overflow(of));
  EXPECT_TRUE(Impl::template sub_overflow<int32_t>(-1, 0, of) == -1 && !did_overflow(of));

  EXPECT_TRUE(Impl::template sub_overflow<int32_t>(2147483647, 1, of) == 2147483646 && !did_overflow(of));
  EXPECT_TRUE(Impl::template sub_overflow<int32_t>(2147483647, 2147483647, of) == 0 && !did_overflow(of));
  EXPECT_TRUE(Impl::template sub_overflow<int32_t>(-2147483647, 1, of) == -2147483647 - 1 && !did_overflow(of));
  EXPECT_TRUE(Impl::template sub_overflow<int32_t>(-2147483647, -1, of) == -2147483646 && !did_overflow(of));
  EXPECT_TRUE(Impl::template sub_overflow<int32_t>(-2147483647 - 0, -2147483647 - 0, of) == 0 && !did_overflow(of));
  EXPECT_TRUE(Impl::template sub_overflow<int32_t>(-2147483647 - 1, -2147483647 - 1, of) == 0 && !did_overflow(of));

  (void)Impl::template sub_overflow<int32_t>(-2, 2147483647, of); EXPECT_TRUE(did_overflow(of)); of = OverflowFlag(0);
  (void)Impl::template sub_overflow<int32_t>(-2147483647, 2, of); EXPECT_TRUE(did_overflow(of)); of = OverflowFlag(0);

  (void)Impl::template sub_overflow<int32_t>(-2147483647    , 2147483647, of); EXPECT_TRUE(did_overflow(of)); of = OverflowFlag(0);
  (void)Impl::template sub_overflow<int32_t>(-2147483647 - 1, 2147483647, of); EXPECT_TRUE(did_overflow(of)); of = OverflowFlag(0);

  (void)Impl::template sub_overflow<int32_t>(2147483647, -2147483647 - 0, of); EXPECT_TRUE(did_overflow(of)); of = OverflowFlag(0);
  (void)Impl::template sub_overflow<int32_t>(2147483647, -2147483647 - 1, of); EXPECT_TRUE(did_overflow(of)); of = OverflowFlag(0);

  EXPECT_TRUE(Impl::template sub_overflow<uint32_t>(0, 0, of) == 0 && !did_overflow(of));
  EXPECT_TRUE(Impl::template sub_overflow<uint32_t>(1, 0, of) == 1 && !did_overflow(of));

  EXPECT_TRUE(Impl::template sub_overflow<uint32_t>(0xFFFFFFFFu, 0u, of) == 0xFFFFFFFFu && !did_overflow(of));
  EXPECT_TRUE(Impl::template sub_overflow<uint32_t>(0xFFFFFFFFu, 0xFFFFFFFFu, of) == 0u && !did_overflow(of));

  (void)Impl::template sub_overflow<uint32_t>(0u, 1u, of); EXPECT_TRUE(did_overflow(of)); of = OverflowFlag(0);
  (void)Impl::template sub_overflow<uint32_t>(1u, 2u, of); EXPECT_TRUE(did_overflow(of)); of = OverflowFlag(0);

  (void)Impl::template sub_overflow<uint32_t>(0u, 0xFFFFFFFFu, of); EXPECT_TRUE(did_overflow(of)); of = OverflowFlag(0);
  (void)Impl::template sub_overflow<uint32_t>(1u, 0xFFFFFFFFu, of); EXPECT_TRUE(did_overflow(of)); of = OverflowFlag(0);

  (void)Impl::template sub_overflow<uint32_t>(0u, 0x7FFFFFFFu, of); EXPECT_TRUE(did_overflow(of)); of = OverflowFlag(0);
  (void)Impl::template sub_overflow<uint32_t>(1u, 0x7FFFFFFFu, of); EXPECT_TRUE(did_overflow(of)); of = OverflowFlag(0);

  (void)Impl::template sub_overflow<uint32_t>(0x7FFFFFFEu, 0x7FFFFFFFu, of); EXPECT_TRUE(did_overflow(of)); of = OverflowFlag(0);
  (void)Impl::template sub_overflow<uint32_t>(0xFFFFFFFEu, 0xFFFFFFFFu, of); EXPECT_TRUE(did_overflow(of)); of = OverflowFlag(0);
}

TEST_CASE_T(axl_commons_mul_overflow, dispatch::tests) {
  using Impl = T;

  using axl::did_overflow;
  using axl::OverflowFlag;

  OverflowFlag of{};

  EXPECT_TRUE(Impl::template mul_overflow<int32_t>(0, 0, of) == 0 && !did_overflow(of));
  EXPECT_TRUE(Impl::template mul_overflow<int32_t>(0, 1, of) == 0 && !did_overflow(of));
  EXPECT_TRUE(Impl::template mul_overflow<int32_t>(1, 0, of) == 0 && !did_overflow(of));

  EXPECT_TRUE(Impl::template mul_overflow<int32_t>( 1,  1, of) ==  1 && !did_overflow(of));
  EXPECT_TRUE(Impl::template mul_overflow<int32_t>( 1, -1, of) == -1 && !did_overflow(of));
  EXPECT_TRUE(Impl::template mul_overflow<int32_t>(-1,  1, of) == -1 && !did_overflow(of));
  EXPECT_TRUE(Impl::template mul_overflow<int32_t>(-1, -1, of) ==  1 && !did_overflow(of));

  EXPECT_TRUE(Impl::template mul_overflow<int32_t>( 32768,  65535, of) ==  2147450880 && !did_overflow(of));
  EXPECT_TRUE(Impl::template mul_overflow<int32_t>( 32768, -65535, of) == -2147450880 && !did_overflow(of));
  EXPECT_TRUE(Impl::template mul_overflow<int32_t>(-32768,  65535, of) == -2147450880 && !did_overflow(of));
  EXPECT_TRUE(Impl::template mul_overflow<int32_t>(-32768, -65535, of) ==  2147450880 && !did_overflow(of));

  EXPECT_TRUE(Impl::template mul_overflow<int32_t>(2147483647, 1, of) == 2147483647 && !did_overflow(of));
  EXPECT_TRUE(Impl::template mul_overflow<int32_t>(1, 2147483647, of) == 2147483647 && !did_overflow(of));

  EXPECT_TRUE(Impl::template mul_overflow<int32_t>(-2147483647 - 1, 1, of) == -2147483647 - 1 && !did_overflow(of));
  EXPECT_TRUE(Impl::template mul_overflow<int32_t>(1, -2147483647 - 1, of) == -2147483647 - 1 && !did_overflow(of));

  (void)Impl::template mul_overflow<int32_t>( 65535,  65535, of); EXPECT_TRUE(did_overflow(of)); of = OverflowFlag(0);
  (void)Impl::template mul_overflow<int32_t>( 65535, -65535, of); EXPECT_TRUE(did_overflow(of)); of = OverflowFlag(0);
  (void)Impl::template mul_overflow<int32_t>(-65535,  65535, of); EXPECT_TRUE(did_overflow(of)); of = OverflowFlag(0);
  (void)Impl::template mul_overflow<int32_t>(-65535, -65535, of); EXPECT_TRUE(did_overflow(of)); of = OverflowFlag(0);

  (void)Impl::template mul_overflow<int32_t>( 2147483647    ,  2147483647    , of); EXPECT_TRUE(did_overflow(of)); of = OverflowFlag(0);
  (void)Impl::template mul_overflow<int32_t>( 2147483647    , -2147483647 - 1, of); EXPECT_TRUE(did_overflow(of)); of = OverflowFlag(0);
  (void)Impl::template mul_overflow<int32_t>(-2147483647 - 1,  2147483647    , of); EXPECT_TRUE(did_overflow(of)); of = OverflowFlag(0);
  (void)Impl::template mul_overflow<int32_t>(-2147483647 - 1, -2147483647 - 1, of); EXPECT_TRUE(did_overflow(of)); of = OverflowFlag(0);

  EXPECT_TRUE(Impl::template mul_overflow<uint32_t>(0, 0, of) == 0 && !did_overflow(of));
  EXPECT_TRUE(Impl::template mul_overflow<uint32_t>(0, 1, of) == 0 && !did_overflow(of));
  EXPECT_TRUE(Impl::template mul_overflow<uint32_t>(1, 0, of) == 0 && !did_overflow(of));
  EXPECT_TRUE(Impl::template mul_overflow<uint32_t>(1, 1, of) == 1 && !did_overflow(of));

  EXPECT_TRUE(Impl::template mul_overflow<uint32_t>(0x10000000u, 15, of) == 0xF0000000u && !did_overflow(of));
  EXPECT_TRUE(Impl::template mul_overflow<uint32_t>(15, 0x10000000u, of) == 0xF0000000u && !did_overflow(of));

  EXPECT_TRUE(Impl::template mul_overflow<uint32_t>(0xFFFFFFFFu, 1, of) == 0xFFFFFFFFu && !did_overflow(of));
  EXPECT_TRUE(Impl::template mul_overflow<uint32_t>(1, 0xFFFFFFFFu, of) == 0xFFFFFFFFu && !did_overflow(of));

  (void)Impl::template mul_overflow<uint32_t>(0xFFFFFFFFu, 2, of); EXPECT_TRUE(did_overflow(of)); of = OverflowFlag(0);
  (void)Impl::template mul_overflow<uint32_t>(2, 0xFFFFFFFFu, of); EXPECT_TRUE(did_overflow(of)); of = OverflowFlag(0);

  (void)Impl::template mul_overflow<uint32_t>(0x80000000u, 2, of); EXPECT_TRUE(did_overflow(of)); of = OverflowFlag(0);
  (void)Impl::template mul_overflow<uint32_t>(2, 0x80000000u, of); EXPECT_TRUE(did_overflow(of)); of = OverflowFlag(0);

  EXPECT_TRUE(Impl::template mul_overflow<int64_t>(0, 0, of) == 0 && !did_overflow(of));
  EXPECT_TRUE(Impl::template mul_overflow<int64_t>(0, 1, of) == 0 && !did_overflow(of));
  EXPECT_TRUE(Impl::template mul_overflow<int64_t>(1, 0, of) == 0 && !did_overflow(of));

  EXPECT_TRUE(Impl::template mul_overflow<int64_t>( 1,  1, of) ==  1 && !did_overflow(of));
  EXPECT_TRUE(Impl::template mul_overflow<int64_t>( 1, -1, of) == -1 && !did_overflow(of));
  EXPECT_TRUE(Impl::template mul_overflow<int64_t>(-1,  1, of) == -1 && !did_overflow(of));
  EXPECT_TRUE(Impl::template mul_overflow<int64_t>(-1, -1, of) ==  1 && !did_overflow(of));

  EXPECT_TRUE(Impl::template mul_overflow<int64_t>( 32768,  65535, of) ==  2147450880 && !did_overflow(of));
  EXPECT_TRUE(Impl::template mul_overflow<int64_t>( 32768, -65535, of) == -2147450880 && !did_overflow(of));
  EXPECT_TRUE(Impl::template mul_overflow<int64_t>(-32768,  65535, of) == -2147450880 && !did_overflow(of));
  EXPECT_TRUE(Impl::template mul_overflow<int64_t>(-32768, -65535, of) ==  2147450880 && !did_overflow(of));

  EXPECT_TRUE(Impl::template mul_overflow<int64_t>(2147483647, 1, of) == 2147483647 && !did_overflow(of));
  EXPECT_TRUE(Impl::template mul_overflow<int64_t>(1, 2147483647, of) == 2147483647 && !did_overflow(of));

  EXPECT_TRUE(Impl::template mul_overflow<int64_t>(-2147483647 - 1, 1, of) == -2147483647 - 1 && !did_overflow(of));
  EXPECT_TRUE(Impl::template mul_overflow<int64_t>(1, -2147483647 - 1, of) == -2147483647 - 1 && !did_overflow(of));

  EXPECT_TRUE(Impl::template mul_overflow<int64_t>( 65535,  65535, of) ==  int64_t(4294836225) && !did_overflow(of));
  EXPECT_TRUE(Impl::template mul_overflow<int64_t>( 65535, -65535, of) == -int64_t(4294836225) && !did_overflow(of));
  EXPECT_TRUE(Impl::template mul_overflow<int64_t>(-65535,  65535, of) == -int64_t(4294836225) && !did_overflow(of));
  EXPECT_TRUE(Impl::template mul_overflow<int64_t>(-65535, -65535, of) ==  int64_t(4294836225) && !did_overflow(of));

  EXPECT_TRUE(Impl::template mul_overflow<int64_t>( 2147483647    ,  2147483647    , of) ==  int64_t(4611686014132420609) && !did_overflow(of));
  EXPECT_TRUE(Impl::template mul_overflow<int64_t>( 2147483647    , -2147483647 - 1, of) == -int64_t(4611686016279904256) && !did_overflow(of));
  EXPECT_TRUE(Impl::template mul_overflow<int64_t>(-2147483647 - 1,  2147483647    , of) == -int64_t(4611686016279904256) && !did_overflow(of));
  EXPECT_TRUE(Impl::template mul_overflow<int64_t>(-2147483647 - 1, -2147483647 - 1, of) ==  int64_t(4611686018427387904) && !did_overflow(of));

  EXPECT_TRUE(Impl::template mul_overflow<int64_t>(int64_t(0x7FFFFFFFFFFFFFFF), int64_t(1), of) == int64_t(0x7FFFFFFFFFFFFFFF) && !did_overflow(of));
  EXPECT_TRUE(Impl::template mul_overflow<int64_t>(int64_t(1), int64_t(0x7FFFFFFFFFFFFFFF), of) == int64_t(0x7FFFFFFFFFFFFFFF) && !did_overflow(of));

  (void)Impl::template mul_overflow<int64_t>(int64_t(0x7FFFFFFFFFFFFFFF), int64_t(2), of); EXPECT_TRUE(did_overflow(of)); of = OverflowFlag(0);
  (void)Impl::template mul_overflow<int64_t>(int64_t(2), int64_t(0x7FFFFFFFFFFFFFFF), of); EXPECT_TRUE(did_overflow(of)); of = OverflowFlag(0);

  (void)Impl::template mul_overflow<int64_t>(int64_t( 0x7FFFFFFFFFFFFFFF), int64_t( 0x7FFFFFFFFFFFFFFF), of); EXPECT_TRUE(did_overflow(of)); of = OverflowFlag(0);
  (void)Impl::template mul_overflow<int64_t>(int64_t( 0x7FFFFFFFFFFFFFFF), int64_t(-0x7FFFFFFFFFFFFFFF), of); EXPECT_TRUE(did_overflow(of)); of = OverflowFlag(0);
  (void)Impl::template mul_overflow<int64_t>(int64_t(-0x7FFFFFFFFFFFFFFF), int64_t( 0x7FFFFFFFFFFFFFFF), of); EXPECT_TRUE(did_overflow(of)); of = OverflowFlag(0);
  (void)Impl::template mul_overflow<int64_t>(int64_t(-0x7FFFFFFFFFFFFFFF), int64_t(-0x7FFFFFFFFFFFFFFF), of); EXPECT_TRUE(did_overflow(of)); of = OverflowFlag(0);

  EXPECT_TRUE(Impl::template mul_overflow<uint64_t>(0, 0, of) == 0 && !did_overflow(of));
  EXPECT_TRUE(Impl::template mul_overflow<uint64_t>(0, 1, of) == 0 && !did_overflow(of));
  EXPECT_TRUE(Impl::template mul_overflow<uint64_t>(1, 0, of) == 0 && !did_overflow(of));
  EXPECT_TRUE(Impl::template mul_overflow<uint64_t>(1, 1, of) == 1 && !did_overflow(of));

  EXPECT_TRUE(Impl::template mul_overflow<uint64_t>(0x1000000000000000u, 15, of) == 0xF000000000000000u && !did_overflow(of));
  EXPECT_TRUE(Impl::template mul_overflow<uint64_t>(15, 0x1000000000000000u, of) == 0xF000000000000000u && !did_overflow(of));

  EXPECT_TRUE(Impl::template mul_overflow<uint64_t>(0xFFFFFFFFFFFFFFFFu, 1, of) == 0xFFFFFFFFFFFFFFFFu && !did_overflow(of));
  EXPECT_TRUE(Impl::template mul_overflow<uint64_t>(1, 0xFFFFFFFFFFFFFFFFu, of) == 0xFFFFFFFFFFFFFFFFu && !did_overflow(of));

  (void)Impl::template mul_overflow<uint64_t>(0xFFFFFFFFFFFFFFFFu, 2, of); EXPECT_TRUE(did_overflow(of)); of = OverflowFlag(0);
  (void)Impl::template mul_overflow<uint64_t>(2, 0xFFFFFFFFFFFFFFFFu, of); EXPECT_TRUE(did_overflow(of)); of = OverflowFlag(0);

  (void)Impl::template mul_overflow<uint64_t>(0x8000000000000000u, 2, of); EXPECT_TRUE(did_overflow(of)); of = OverflowFlag(0);
  (void)Impl::template mul_overflow<uint64_t>(2, 0x8000000000000000u, of); EXPECT_TRUE(did_overflow(of)); of = OverflowFlag(0);
}

TEST_CASE(axl_commons_add_saturate) {
  EXPECT_EQ(axl::add_saturate<int32_t>(0, 0), 0);
  EXPECT_EQ(axl::add_saturate<int32_t>(0, 1), 1);
  EXPECT_EQ(axl::add_saturate<int32_t>(0, -1), -1);
  EXPECT_EQ(axl::add_saturate<int32_t>(1, 0), 1);
  EXPECT_EQ(axl::add_saturate<int32_t>(-1, 0), -1);
  EXPECT_EQ(axl::add_saturate<int32_t>(-1, -1), -2);

  EXPECT_EQ(axl::add_saturate<int32_t>(0, -2147483647), -2147483647);
  EXPECT_EQ(axl::add_saturate<int32_t>(-2147483647, 0), -2147483647);
  EXPECT_EQ(axl::add_saturate<int32_t>(-1, -2147483647), -2147483648);
  EXPECT_EQ(axl::add_saturate<int32_t>(-2147483647, -1), -2147483648);
  EXPECT_EQ(axl::add_saturate<int32_t>(-1, -2147483648), -2147483648);
  EXPECT_EQ(axl::add_saturate<int32_t>(-2147483648, -1), -2147483648);
  EXPECT_EQ(axl::add_saturate<int32_t>(-2147483648, -2147483647), -2147483648);
  EXPECT_EQ(axl::add_saturate<int32_t>(-2147483647, -2147483648), -2147483648);
  EXPECT_EQ(axl::add_saturate<int32_t>(-2147483648, -2147483648), -2147483648);

  EXPECT_EQ(axl::add_saturate<int32_t>(0, 2147483647), 2147483647);
  EXPECT_EQ(axl::add_saturate<int32_t>(1, 2147483647), 2147483647);
  EXPECT_EQ(axl::add_saturate<int32_t>(2147483647, 0), 2147483647);
  EXPECT_EQ(axl::add_saturate<int32_t>(2147483647, 1), 2147483647);
  EXPECT_EQ(axl::add_saturate<int32_t>(2147483647, 2147483647), 2147483647);

  EXPECT_EQ(axl::add_saturate<uint32_t>(0u, 0u), 0u);
  EXPECT_EQ(axl::add_saturate<uint32_t>(0u, 1u), 1u);
  EXPECT_EQ(axl::add_saturate<uint32_t>(1u, 0u), 1u);
  EXPECT_EQ(axl::add_saturate<uint32_t>(1u, 1u), 2u);

  EXPECT_EQ(axl::add_saturate<uint32_t>(2147483647u, 1u), 2147483648u);
  EXPECT_EQ(axl::add_saturate<uint32_t>(1u, 2147483647u), 2147483648u);
  EXPECT_EQ(axl::add_saturate<uint32_t>(2147483648u, 1u), 2147483649u);
  EXPECT_EQ(axl::add_saturate<uint32_t>(1u, 2147483648u), 2147483649u);
  EXPECT_EQ(axl::add_saturate<uint32_t>(0xFFFFFFFFu, 0u), 0xFFFFFFFFu);
  EXPECT_EQ(axl::add_saturate<uint32_t>(0xFFFFFFFFu, 1u), 0xFFFFFFFFu);
  EXPECT_EQ(axl::add_saturate<uint32_t>(0xFFFFFFFFu, 2u), 0xFFFFFFFFu);
  EXPECT_EQ(axl::add_saturate<uint32_t>(0u, 0xFFFFFFFFu), 0xFFFFFFFFu);
  EXPECT_EQ(axl::add_saturate<uint32_t>(1u, 0xFFFFFFFFu), 0xFFFFFFFFu);
  EXPECT_EQ(axl::add_saturate<uint32_t>(2u, 0xFFFFFFFFu), 0xFFFFFFFFu);
  EXPECT_EQ(axl::add_saturate<uint32_t>(0xFFFFFFFFu, 0x80000000u), 0xFFFFFFFFu);
  EXPECT_EQ(axl::add_saturate<uint32_t>(0x80000000u, 0xFFFFFFFFu), 0xFFFFFFFFu);
  EXPECT_EQ(axl::add_saturate<uint32_t>(0xFFFFFFFFu, 0xFFFFFFFFu), 0xFFFFFFFFu);
}

TEST_CASE(axl_commons_sub_saturate) {
  EXPECT_EQ(axl::sub_saturate<int32_t>(0, 0), 0);
  EXPECT_EQ(axl::sub_saturate<int32_t>(0, 1), -1);
  EXPECT_EQ(axl::sub_saturate<int32_t>(0, -1), 1);
  EXPECT_EQ(axl::sub_saturate<int32_t>(1, 0), 1);
  EXPECT_EQ(axl::sub_saturate<int32_t>(-1, 0), -1);
  EXPECT_EQ(axl::sub_saturate<int32_t>(-1, -1), 0);

  EXPECT_EQ(axl::sub_saturate<int32_t>(0, -2147483647), 2147483647);
  EXPECT_EQ(axl::sub_saturate<int32_t>(-2147483647, 0), -2147483647);
  EXPECT_EQ(axl::sub_saturate<int32_t>(-1, -2147483647), 2147483646);
  EXPECT_EQ(axl::sub_saturate<int32_t>(-2147483647, -1), -2147483646);
  EXPECT_EQ(axl::sub_saturate<int32_t>(-1, -2147483648), 2147483647);
  EXPECT_EQ(axl::sub_saturate<int32_t>(-2147483648, -1), -2147483647);
  EXPECT_EQ(axl::sub_saturate<int32_t>(-2147483648, -2147483647), -1);
  EXPECT_EQ(axl::sub_saturate<int32_t>(-2147483647, -2147483648), 1);
  EXPECT_EQ(axl::sub_saturate<int32_t>(-2147483648, -2147483648), 0);

  EXPECT_EQ(axl::sub_saturate<int32_t>(-1000, 2147483647), -2147483648);
  EXPECT_EQ(axl::sub_saturate<int32_t>(-1, 2147483647), -2147483648);
  EXPECT_EQ(axl::sub_saturate<int32_t>(0, 2147483647), -2147483647);
  EXPECT_EQ(axl::sub_saturate<int32_t>(1, 2147483647), -2147483646);
  EXPECT_EQ(axl::sub_saturate<int32_t>(1000, -2147483647), 2147483647);
  EXPECT_EQ(axl::sub_saturate<int32_t>(2147483647, 0), 2147483647);
  EXPECT_EQ(axl::sub_saturate<int32_t>(2147483647, 1), 2147483646);
  EXPECT_EQ(axl::sub_saturate<int32_t>(2147483647, 2147483647), 0);
  EXPECT_EQ(axl::sub_saturate<int32_t>(-2147483647, 2147483647), -2147483648);
  EXPECT_EQ(axl::sub_saturate<int32_t>(-2147483648, 2147483647), -2147483648);

  EXPECT_EQ(axl::sub_saturate<uint32_t>(0u, 0u), 0u);
  EXPECT_EQ(axl::sub_saturate<uint32_t>(0u, 1u), 0u);
  EXPECT_EQ(axl::sub_saturate<uint32_t>(1u, 0u), 1u);
  EXPECT_EQ(axl::sub_saturate<uint32_t>(1u, 1u), 0u);
  EXPECT_EQ(axl::sub_saturate<uint32_t>(1u, 0xFFFFFFFFu), 0u);

  EXPECT_EQ(axl::sub_saturate<uint32_t>(2147483647u, 1u), 2147483646u);
  EXPECT_EQ(axl::sub_saturate<uint32_t>(1u, 2147483647u), 0u);
  EXPECT_EQ(axl::sub_saturate<uint32_t>(2147483648u, 1u), 2147483647u);
  EXPECT_EQ(axl::sub_saturate<uint32_t>(1u, 2147483648u), 0u);
  EXPECT_EQ(axl::sub_saturate<uint32_t>(0xFFFFFFFFu, 0u), 0xFFFFFFFFu);
  EXPECT_EQ(axl::sub_saturate<uint32_t>(0xFFFFFFFFu, 1u), 0xFFFFFFFEu);
  EXPECT_EQ(axl::sub_saturate<uint32_t>(0xFFFFFFFFu, 2u), 0xFFFFFFFDu);
  EXPECT_EQ(axl::sub_saturate<uint32_t>(0u, 0xFFFFFFFFu), 0u);
  EXPECT_EQ(axl::sub_saturate<uint32_t>(1u, 0xFFFFFFFFu), 0u);
  EXPECT_EQ(axl::sub_saturate<uint32_t>(2u, 0xFFFFFFFFu), 0u);
  EXPECT_EQ(axl::sub_saturate<uint32_t>(0xFFFFFFFFu, 0x80000000u), 0x7FFFFFFFu);
  EXPECT_EQ(axl::sub_saturate<uint32_t>(0x80000000u, 0xFFFFFFFFu), 0u);
  EXPECT_EQ(axl::sub_saturate<uint32_t>(0xFFFFFFFEu, 0xFFFFFFFFu), 0u);
  EXPECT_EQ(axl::sub_saturate<uint32_t>(0xFFFFFFFFu, 0xFFFFFFFFu), 0u);
}

TEST_CASE(axl_commons_mul_saturate) {
  EXPECT_EQ(axl::mul_saturate<int32_t>(1, 1), 1);
  EXPECT_EQ(axl::mul_saturate<int32_t>(1, 1000), 1000);
  EXPECT_EQ(axl::mul_saturate<int32_t>(1000, 1000), 1000000);
  EXPECT_EQ(axl::mul_saturate<int32_t>(21474836, 100), 2147483600);
  EXPECT_EQ(axl::mul_saturate<int32_t>(2147483647, 0), 0);
  EXPECT_EQ(axl::mul_saturate<int32_t>(2147483647, 1), 2147483647);
  EXPECT_EQ(axl::mul_saturate<int32_t>(2147483647, 2), 2147483647);
  EXPECT_EQ(axl::mul_saturate<int32_t>(2147483647, 2000013), 2147483647);
  EXPECT_EQ(axl::mul_saturate<int32_t>(2147483647, 2147483647), 2147483647);

  EXPECT_EQ(axl::mul_saturate<int32_t>(1, -1), -1);
  EXPECT_EQ(axl::mul_saturate<int32_t>(1, -1000), -1000);
  EXPECT_EQ(axl::mul_saturate<int32_t>(-1000, 1000), -1000000);
  EXPECT_EQ(axl::mul_saturate<int32_t>(21474836, -100), -2147483600);
  EXPECT_EQ(axl::mul_saturate<int32_t>(-2147483647, 0), 0);
  EXPECT_EQ(axl::mul_saturate<int32_t>(-2147483648, 0), 0);
  EXPECT_EQ(axl::mul_saturate<int32_t>(-2147483647, 1), -2147483647);
  EXPECT_EQ(axl::mul_saturate<int32_t>(-2147483648, 1), -2147483648);
  EXPECT_EQ(axl::mul_saturate<int32_t>(-2147483647, 2), -2147483648);
  EXPECT_EQ(axl::mul_saturate<int32_t>(-2147483648, 2), -2147483648);
  EXPECT_EQ(axl::mul_saturate<int32_t>(-2147483647, 2000013), -2147483648);
  EXPECT_EQ(axl::mul_saturate<int32_t>(-2147483648, 2000013), -2147483648);
  EXPECT_EQ(axl::mul_saturate<int32_t>(-2147483647, 2147483647), -2147483648);
  EXPECT_EQ(axl::mul_saturate<int32_t>(-2147483648, 2147483647), -2147483648);
  EXPECT_EQ(axl::mul_saturate<int32_t>(-2147483647, -2147483647), 2147483647);
  EXPECT_EQ(axl::mul_saturate<int32_t>(-2147483648, -2147483648), 2147483647);
}

TEST_CASE(axl_commons_clamp) {
  EXPECT_EQ(axl::clamp<int8_t>(-1000000), -128);
  EXPECT_EQ(axl::clamp<int8_t>(-1), -1);
  EXPECT_EQ(axl::clamp<int8_t>(42), 42);
  EXPECT_EQ(axl::clamp<int8_t>(255), 0x7F);
  EXPECT_EQ(axl::clamp<int8_t>(256), 0x7F);
  EXPECT_EQ(axl::clamp<int8_t>(0x7FFFFFFF), 0x7F);
  EXPECT_EQ(axl::clamp<int8_t>(0x7FFFFFFFu), 0x7F);
  EXPECT_EQ(axl::clamp<int8_t>(0xFFFFFFFFu), 0x7F);

  EXPECT_EQ(axl::clamp<uint8_t>(-10000000), 0u);
  EXPECT_EQ(axl::clamp<uint8_t>(-1), 0u);
  EXPECT_EQ(axl::clamp<uint8_t>(42), 42u);
  EXPECT_EQ(axl::clamp<uint8_t>(255), 0xFFu);
  EXPECT_EQ(axl::clamp<uint8_t>(256), 0xFFu);
  EXPECT_EQ(axl::clamp<uint8_t>(0x7FFFFFFF), 0xFFu);
  EXPECT_EQ(axl::clamp<uint8_t>(0x7FFFFFFFu), 0xFFu);
  EXPECT_EQ(axl::clamp<uint8_t>(0xFFFFFFFFu), 0xFFu);

  EXPECT_EQ(axl::clamp<int16_t>(-10000000), -32768);
  EXPECT_EQ(axl::clamp<int16_t>(-1), -1);
  EXPECT_EQ(axl::clamp<int16_t>(42), 42);
  EXPECT_EQ(axl::clamp<int16_t>(0xFFFF), 0x7FFF);
  EXPECT_EQ(axl::clamp<int16_t>(0x10000), 0x7FFF);
  EXPECT_EQ(axl::clamp<int16_t>(0x10000u), 0x7FFF);
  EXPECT_EQ(axl::clamp<int16_t>(0x7FFFFFFF), 0x7FFF);
  EXPECT_EQ(axl::clamp<int16_t>(0x7FFFFFFFu), 0x7FFF);
  EXPECT_EQ(axl::clamp<int16_t>(0xFFFFFFFFu), 0x7FFF);

  EXPECT_EQ(axl::clamp<uint16_t>(-10000), 0u);
  EXPECT_EQ(axl::clamp<uint16_t>(-1), 0u);
  EXPECT_EQ(axl::clamp<uint16_t>(42), 42u);
  EXPECT_EQ(axl::clamp<uint16_t>(0xFFFF), 0xFFFFu);
  EXPECT_EQ(axl::clamp<uint16_t>(0x10000), 0xFFFFu);
  EXPECT_EQ(axl::clamp<uint16_t>(0x10000u), 0xFFFFu);
  EXPECT_EQ(axl::clamp<uint16_t>(0x7FFFFFFF), 0xFFFFu);
  EXPECT_EQ(axl::clamp<uint16_t>(0x7FFFFFFFu), 0xFFFFu);
  EXPECT_EQ(axl::clamp<uint16_t>(0xFFFFFFFFu), 0xFFFFu);

  EXPECT_EQ(axl::clamp<int32_t>(-10000), -10000);
  EXPECT_EQ(axl::clamp<int32_t>(-1), -1);
  EXPECT_EQ(axl::clamp<int32_t>(42), 42);
  EXPECT_EQ(axl::clamp<int32_t>(0xFFFF), 0xFFFF);
  EXPECT_EQ(axl::clamp<int32_t>(0x10000), 0x10000);
  EXPECT_EQ(axl::clamp<int32_t>(0x10000u), 0x10000);
  EXPECT_EQ(axl::clamp<int32_t>(0x7FFFFFFF), 0x7FFFFFFF);
  EXPECT_EQ(axl::clamp<int32_t>(0x7FFFFFFFu), 0x7FFFFFFF);
  EXPECT_EQ(axl::clamp<int32_t>(0xFFFFFFFFu), 0x7FFFFFFF);

  EXPECT_EQ(axl::clamp<uint32_t>(-10000), 0u);
  EXPECT_EQ(axl::clamp<uint32_t>(-1), 0u);
  EXPECT_EQ(axl::clamp<uint32_t>(42), 42u);
  EXPECT_EQ(axl::clamp<uint32_t>(0xFFFF), 0xFFFFu);
  EXPECT_EQ(axl::clamp<uint32_t>(0x10000), 0x10000u);
  EXPECT_EQ(axl::clamp<uint32_t>(0x10000u), 0x10000u);
  EXPECT_EQ(axl::clamp<uint32_t>(0x7FFFFFFF), 0x7FFFFFFFu);
  EXPECT_EQ(axl::clamp<uint32_t>(0x7FFFFFFFu), 0x7FFFFFFFu);
  EXPECT_EQ(axl::clamp<uint32_t>(0xFFFFFFFFu), 0xFFFFFFFFu);
}

TEST_CASE(axl_commons_offset_ptr) {
  uint32_t array[16] {};
  EXPECT_EQ(axl::offset_ptr(array, 4u), array + 1u);
}

TEST_CASE(axl_commons_deoffset_ptr) {
  uint32_t array[16] {};
  EXPECT_EQ(axl::deoffset_ptr(array + 2, 4u), array + 1u);
}

TEST_CASE(axl_commons_ptr_byte_diff) {
  uint32_t array[16] {};

  uint32_t* a = array;
  uint32_t* b = array + 16;

  EXPECT_EQ(axl::ptr_byte_diff(a, b), 16u * sizeof(uint32_t));
}

TEST_CASE(axl_commons_ptrs_are_aligned) {
  EXPECT_TRUE(axl::ptrs_are_aligned<4>((void*)(uintptr_t)0x0, (void*)(uintptr_t)0x4));
  EXPECT_FALSE(axl::ptrs_are_aligned<4>((void*)(uintptr_t)0x1, (void*)(uintptr_t)0x4));
  EXPECT_FALSE(axl::ptrs_are_aligned<4>((void*)(uintptr_t)0x1, (void*)(uintptr_t)0x5));
  EXPECT_TRUE(axl::ptrs_are_aligned<16>((void*)(uintptr_t)0x10, (void*)(uintptr_t)0x20));
  EXPECT_FALSE(axl::ptrs_are_aligned<16>((void*)(uintptr_t)0x1, (void*)(uintptr_t)0x5));
}

TEST_CASE(axl_commons_ptrs_share_alignment) {
  EXPECT_TRUE(axl::ptrs_share_alignment<4>((void*)(uintptr_t)0x1, (void*)(uintptr_t)0x5));
  EXPECT_TRUE(axl::ptrs_share_alignment<16>((void*)(uintptr_t)0x1, (void*)(uintptr_t)0x11));
  EXPECT_FALSE(axl::ptrs_share_alignment<16>((void*)(uintptr_t)0x1, (void*)(uintptr_t)0x12));
}

ASMJIT_END_NAMESPACE

#endif // ASMJIT_TEST
