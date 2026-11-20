// This file is part of AsmJit project <https://asmjit.com>
//
// See <asmjit/core.h> or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#ifndef ASMJIT_AXL_COMMONS_H_INCLUDED
#define ASMJIT_AXL_COMMONS_H_INCLUDED

#include <asmjit/axl/build_integration.h>

#include <bit>
#include <limits>
#include <type_traits>

#if defined(_MSC_VER) && (defined(_M_IX86) || defined(_M_X64))
  #include <intrin.h>
#elif defined(__BMI2__)
  #include <x86intrin.h>
#endif

ASMJIT_BEGIN_NAMESPACE

//! \addtogroup asmjit_axl
//! \{

//! Output parameter.
template<typename T>
class Out {
protected:
  T& _val;

public:
  ASMJIT_INLINE_NODEBUG explicit Out(T& val) noexcept
    : _val(val) {}

  ASMJIT_INLINE_NODEBUG Out& operator=(const T& val) noexcept {
    _val = val;
    return *this;
  }

  ASMJIT_INLINE_NODEBUG T& value() const noexcept { return _val; }
  ASMJIT_INLINE_NODEBUG T& operator*() const noexcept { return _val; }
  ASMJIT_INLINE_NODEBUG T* operator->() const noexcept { return &_val; }
};

//! Contains support classes and functions that may be used by AsmJit source and header files. Anything defined
//! here is considered internal and should not be used outside of AsmJit and related projects like AsmTK.
namespace axl {

// Overflow Flag
// =============

//! Overflow flag.
enum class OverflowFlag : uint32_t {};

// ByteOrder
// =========

inline constexpr bool kIsLittleEndian = std::endian::native == std::endian::little;
inline constexpr bool kIsBigEndian = std::endian::native == std::endian::big;

enum class ByteOrder {
  kLE = 0,
  kBE = 1,
  kNative = kIsLittleEndian ? kLE : kBE,
  kSwapped = kIsBigEndian ? kLE : kBE
};

// Placement New (Type)
// ====================

//! Helper to implement placement new/delete without relying on `<new>` header.
struct PlacementNew { void* ptr; };

// Globals Constants
// =================

//! Host memory allocator alignment.
inline constexpr uint32_t kAllocAlignment =
#if defined(__APPLE__)
  16u
#elif defined(__linux__) || defined(__ANDROID__) || defined(_WIN32)
  static_cast<uint32_t>(sizeof(void*) * 2u)
#else
  static_cast<uint32_t>(sizeof(void*))
#endif
;

//! Host memory allocator overhead - just approximation for some specific uses to not overallocate a page.
inline constexpr uint32_t kAllocOverhead = uint32_t(sizeof(intptr_t) * 4u);

//! Aggressive growing strategy threshold.
inline constexpr uint32_t kGrowThreshold = 1024u * 1024u * 16u;

//! Maximum depth of RB-Tree is:
//!
//!   `2 * log2(n + 1)`
//!
//! Size of RB node is at least two pointers (without data), so a theoretical architecture limit would be:
//!
//!   `2 * log2(addressable_memory_size / sizeof(Node) + 1)`
//!
//! Which yields 30 on 32-bit arch and 61 on 64-bit arch. The final value was adjusted by +1 for safety reasons.
inline constexpr uint32_t kMaxTreeHeight = (sizeof(void*) < 8 ? 30 : 61) + 1;

// Standard Utilities
// ==================

// These allow to use these concepts without the need to include <utility> header.

template<typename T>
[[nodiscard]]
ASMJIT_INLINE_CONSTEXPR std::remove_reference_t<T>&& move(T&& v) noexcept { return static_cast<std::remove_reference_t<T>&&>(v); }

template<typename T>
[[nodiscard]]
ASMJIT_INLINE_CONSTEXPR T&& forward(std::remove_reference_t<T>& v) noexcept { return static_cast<T&&>(v); }

template<typename T>
[[nodiscard]]
ASMJIT_INLINE_CONSTEXPR T&& forward(std::remove_reference_t<T>&& v) noexcept {
  static_assert(!std::is_lvalue_reference_v<T>, "Cannot forward an rvalue as an lvalue");
  return static_cast<T&&>(v);
}

template<typename T>
ASMJIT_INLINE_NODEBUG void swap(T& t1, T& t2) noexcept {
  T temp(move(t1));
  t1 = move(t2);
  t2 = move(temp);
}

// Standard Types
// ==============

//! std_int<Size, Unsigned> - Makes a signed or unsigned int-type by size that uses internally the same type as
//! defined by <stdint.h> - thus only `[u]int[8|16|32|64]` types are used here. This is beneficial for creating
//! abstractions that specialize handling of 32-bit and 64-bit types as only two specializations would be required
//! to cover them all.
//!
//! Additionally, std_int can be used to turn enums into their underlying representations, etc...
template<size_t Size, bool IsUnsigned>
struct std_int; // Fail if not specialized.

//! \cond
template<> struct std_int<1, false> { using type = int8_t;   };
template<> struct std_int<1, true > { using type = uint8_t;  };
template<> struct std_int<2, false> { using type = int16_t;  };
template<> struct std_int<2, true > { using type = uint16_t; };
template<> struct std_int<4, false> { using type = int32_t;  };
template<> struct std_int<4, true > { using type = uint32_t; };
template<> struct std_int<8, false> { using type = int64_t;  };
template<> struct std_int<8, true > { using type = uint64_t; };
//! \endcond

//! std_int_t is a shortcut to `std_int<Size, IsUnsigned>::type`.
template<size_t Size, bool IsUnsigned>
using std_int_t = typename std_int<Size, IsUnsigned>::type;

//! std_sint_t is a shortcut to `std_int<Size, false>::type`.
template<size_t Size>
using std_sint_t = typename std_int<Size, false>::type;

//! std_uint_t is a shortcut to `std_int<Size, true>::type`.
template<size_t Size>
using std_uint_t = typename std_int<Size, true>::type;

//! Storage used to store bits in a single word as a standard type as defined by <stdint.h>.
using BitWord = std_uint_t<sizeof(uintptr_t)>;

// Type Conversion
// ===============

template<typename T> struct const_type { using type = const T; };
template<typename T> struct const_type<T*> { using type = const T*; };
template<typename T> struct const_type<T* const*> { using type = const T* const*; };
template<typename T> struct const_type<T**> { using type = const T**; };

template<typename T> using const_type_t = const_type<T>::type;

// Min & Max
// =========

// NOTE: These are constexpr `min()` and `max()` implementations that are not exactly the same as `std::min()`
// and `std::max()`. The return value is not a reference to `a` or `b` but a new value instead.

template<typename T>
[[nodiscard]]
ASMJIT_INLINE_CONSTEXPR T min(const T& a, const T& b) noexcept { return b < a ? b : a; }

template<typename T, typename... Args>
[[nodiscard]]
ASMJIT_INLINE_CONSTEXPR T min(const T& a, const T& b, Args&&... args) noexcept { return min(min(a, b), forward<Args>(args)...); }

template<typename T>
[[nodiscard]]
ASMJIT_INLINE_CONSTEXPR T max(const T& a, const T& b) noexcept { return a < b ? b : a; }

template<typename T, typename... Args>
[[nodiscard]]
ASMJIT_INLINE_CONSTEXPR T max(const T& a, const T& b, Args&&... args) noexcept { return max(max(a, b), forward<Args>(args)...); }

// Anonymous
// =========

namespace {

// Maybe Unused
// ============

//! Silences warnings about unused arguments or variables - more variables can be passed to `maybe_unused()` at once.
template<typename... Args>
ASMJIT_INLINE_NODEBUG void maybe_unused(Args&&...) noexcept {}

// Numeric Limits & Special numbers
// ================================

//! Returns the minimum value of type `T`.
//!
//! \note `T` should be either integer or floating point.
template<typename T>
[[nodiscard]]
ASMJIT_INLINE_CONSTEXPR T min_value() noexcept { return std::numeric_limits<T>::lowest(); }

//! Returns the maximum value of type `T`.
//!
//! \note `T` should be either integer or floating point.
template<typename T>
[[nodiscard]]
ASMJIT_INLINE_CONSTEXPR T max_value() noexcept { return std::numeric_limits<T>::max(); }

//! Returns infinity of `T` type.
//!
//! \note `T` must be floating point.
template<typename T>
[[nodiscard]]
ASMJIT_INLINE_CONSTEXPR T infinity() noexcept { return std::numeric_limits<T>::infinity(); }

//! Returns quiet NaN of `T` type.
//!
//! \note `T` must be floating point.
template<typename T>
[[nodiscard]]
ASMJIT_INLINE_CONSTEXPR T quiet_nan() noexcept { return std::numeric_limits<T>::quiet_NaN(); }

// Type Casting
// ============

//! Converts a value of type `T` into `[int|uint]_[8|16|32|64]_t` integer of the same size as defined in `<stdint.h>`.
//!
//! The signedness of the result depends on the type `T`.
template<typename T>
ASMJIT_INLINE_CONSTEXPR std_int_t<sizeof(T), std::is_unsigned_v<T>> as_std_int(const T& value) noexcept {
  return std_int_t<sizeof(T), std::is_unsigned_v<T>>(value);
}

//! Converts a value of type `T` into `int_[8|16|32|64]_t` integer of the same size  as defined in `<stdint.h>`.
//!
//! The return type is always signed unlike `as_std_int()`.
template<typename T>
ASMJIT_INLINE_CONSTEXPR std_sint_t<sizeof(T)> as_std_sint(const T& value) noexcept {
  return std_sint_t<sizeof(T)>(value);
}

//! Converts a value of type `T` into `uint_[8|16|32|64]_t` integer of the same size  as defined in `<stdint.h>`.
//!
//! The return type is always unsigned unlike `as_std_int()`.
template<typename T>
ASMJIT_INLINE_CONSTEXPR std_uint_t<sizeof(T)> as_std_uint(const T& value) noexcept {
  return std_uint_t<sizeof(T)>(value);
}

//! Converts a value of type `T` into `[int|uint]_[32|64]_t` integer as defined in `<stdint.h>`.
//!
//! This function is designed to convert the value of `T` into at least 32-bit value of the same signedness.
template<typename T>
ASMJIT_INLINE_CONSTEXPR std_int_t<sizeof(T) >= 4u ? sizeof(T) : 4u, std::is_unsigned_v<T>> as_basic_int(const T& value) noexcept {
  return std_int_t<sizeof(T) >= 4u ? sizeof(T) : 4u, std::is_unsigned_v<T>>(value);
}

//! Converts a value of type `T` into `int_[32|64]_t` integer as defined in `<stdint.h>`.
//!
//! The return type is always signed unlike `as_basic_int()`.
template<typename T>
ASMJIT_INLINE_CONSTEXPR std_sint_t<sizeof(T) >= 4u ? sizeof(T) : 4u> as_basic_sint(const T& value) noexcept {
  return std_sint_t<sizeof(T) >= 4u ? sizeof(T) : 4u>(value);
}

//! Converts a value of type `T` into `uint_[32|64]_t` integer as defined in `<stdint.h>`.
//!
//! The return type is always unsigned unlike `as_basic_int()`.
template<typename T>
ASMJIT_INLINE_CONSTEXPR std_uint_t<sizeof(T) >= 4u ? sizeof(T) : 4u> as_basic_uint(const T& value) noexcept {
  return std_uint_t<sizeof(T) >= 4u ? sizeof(T) : 4u>(value);
}

// Negate
// ======

//! Returns `0 - x` in a safe way (no undefined behavior), works for unsigned numbers as well.
template<typename T>
[[nodiscard]]
ASMJIT_INLINE_CONSTEXPR T neg(const T& value) noexcept { return T(as_std_uint(T(0)) - as_std_uint(value)); }

// Repeat Byte
// ===========

template<typename T>
[[nodiscard]]
ASMJIT_INLINE_CONSTEXPR T repeat_byte(const T& x) noexcept {
  using U = std_uint_t<sizeof(T)>;
  constexpr U mul_const = U(0x0101010101010101u & neg(U(1)));
  return T(U(x) * mul_const);
}

// Boolean Utilities
// =================

//! The purpose of `bool_as<T>()` is to cast a boolean value to any integer or enum value. The reason for having
//! it is to ensure that the input parameter is actually a boolean, so the compiler may output a warning in case
//! an implicit narrowing conversion to `bool` happens.
template<typename T>
ASMJIT_INLINE_CONSTEXPR T bool_as(bool b) noexcept { return T(b); }

//! Safely ANDs two or more boolean values by using the `&` operator and not `&&`.
//!
//! \remarks This function should be uses in cases in which two or more conditions are joined without the intention
//! to give the compiler hint to do a short circuit (aka branching after evaluating the first expression).
template<typename... Args>
ASMJIT_INLINE_CONSTEXPR bool bool_and(Args&&... args) noexcept { return bool((... & bool_as<unsigned>(args))); }

//! Safely ORs two or more boolean values by using the `&` operator and not `&&`.
//!
//! \remarks This function should be uses in cases in which two or more conditions are joined without the intention
//! to give the compiler hint to do a short circuit (aka branching after evaluating the first expression).
template<typename... Args>
ASMJIT_INLINE_CONSTEXPR bool bool_or(Args&&... args) noexcept { return bool((... | bool_as<unsigned>(args))); }

//! Converts a boolean value `b` to a mask, which either contains all zeros or all bits set depending on the bool value.
template<typename Dst, typename Src>
[[nodiscard]]
ASMJIT_INLINE_CONSTEXPR Dst bool_as_mask(const Src& b) noexcept { return Dst(neg(as_std_uint(Dst(b)))); }

// Is Between
// ==========

//! Checks whether `x` is greater than or equal to `a` and lesser than or equal to `b`.
template<typename T>
[[nodiscard]]
ASMJIT_INLINE_CONSTEXPR bool is_between(const T& value, const T& a, const T& b) noexcept {
  return bool_and(value >= a, value <= b);
}

// Test & BitTest
// ==============

//! Tests whether `a & b` is non-zero.
[[nodiscard]]
ASMJIT_INLINE_CONSTEXPR bool test(auto a, auto b) noexcept { return (as_std_uint(a) & as_std_uint(b)) != 0u; }

//! Tests whether all bits set in `b` are also set in `a`.
[[nodiscard]]
ASMJIT_INLINE_CONSTEXPR bool test_all(auto a, auto b) noexcept { return (as_std_uint(a) & as_std_uint(b)) == as_std_uint(b); }

//! Tests whether the given `value` has `n`th bit set.
[[nodiscard]]
ASMJIT_INLINE_CONSTEXPR bool bit_test(auto value, auto n) noexcept {
  return (as_std_uint(value) & (as_std_uint(decltype(value)(1)) << as_std_uint(n))) != 0u;
}

// Bit Utilities
// =============

//! Returns a size of `T` in bits - for example `bit_size_of<uint32_t> == 32`.
template<typename T>
inline constexpr uint32_t bit_size_of = uint32_t(sizeof(T) * 8u);

//! Returns `T` with all bits set to 1. For example `bit_ones<uint32_t> == 0xFFFFFFFFu`
template<typename T>
inline constexpr T bit_ones = T(~std_uint_t<sizeof(T)>(0));

//! Returns `x << y` (shift left logical) by explicitly casting `x` to an unsigned type and back to `T`.
//!
//! \remarks This function is provided to prevent undefined behavior depending on the signedness of `T` type. However, there
//! is still a possibility of undefined behavior in case `n >= bit_size_of<T>` - this is undefined behavior according to the
//! C++ standard. See \ref shl_wrap() function that is designed to be wrapping and which prevents undefined behavior.
template<typename T>
[[nodiscard]]
ASMJIT_INLINE_CONSTEXPR T shl(const T& value, auto n) noexcept { return T(as_std_uint(value) << n); }

//! Returns `x << (y & (bit_size_of<T> - 1))` (shift left logical with wrapping semantics).
//!
//! \remarks This function is designed to avoid undefined behavior. On most targets it compiles to the same code as \ref shl(),
//! but without causing undefined behavior depending on the value of `n`.
template<typename T>
[[nodiscard]]
ASMJIT_INLINE_CONSTEXPR T shl_wrap(const T& value, auto n) noexcept { return shl(value, as_std_uint(n) & (bit_size_of<T> - 1u)); }

//! Returns `value >> n` (shift right logical) by explicitly casting `value` to an unsigned type and back to `T`.
//!
//! \remarks This function is provided to prevent undefined behavior depending on the signedness of `T` type. However, there
//! is still a possibility of undefined behavior in case `n >= bit_size_of<T>` - this is undefined behavior according to the
//! C++ standard. See \ref shr_wrap() function that is designed to be wrapping and which prevents undefined behavior.
template<typename T>
[[nodiscard]]
ASMJIT_INLINE_CONSTEXPR T shr(const T& value, auto n) noexcept { return T(as_std_uint(value) >> n); }

//! Returns `x >> (y & (bit_size_of<T> - 1))` (shift right logical with wrapping semantics).
//!
//! \remarks This function is designed to avoid undefined behavior. On most targets it compiles to the same code as \ref shr(),
//! but without causing undefined behavior depending on the value of `n`.
template<typename T>
[[nodiscard]]
ASMJIT_INLINE_CONSTEXPR T shr_wrap(const T& value, auto n) noexcept { return shr(value, as_std_uint(n) & (bit_size_of<T> - 1u)); }

//! Returns `value >> n` (shift right arithmetic) by explicitly casting `value` to a signed type and back to `T`.
template<typename T>
[[nodiscard]]
ASMJIT_INLINE_CONSTEXPR T sar(const T& value, auto n) noexcept { return T(as_std_sint(value) >> n); }

//! Returns `x >> (y & (bit_size_of<T> - 1))` (shift right arithmetic with wrapping semantics).
//!
//! \remarks This function is designed to avoid undefined behavior. On most targets it compiles to the same code as \ref sar(),
//! but without causing undefined behavior depending on the value of `n`.
template<typename T>
[[nodiscard]]
ASMJIT_INLINE_CONSTEXPR T sar_wrap(const T& value, auto n) noexcept { return sar(value, as_std_uint(n) & (bit_size_of<T> - 1u)); }

//! Returns `value` rotated left by `n`.
template<typename T>
[[nodiscard]]
ASMJIT_INLINE_CONSTEXPR T rol(const T& value, auto n) noexcept { return T(std::rotl(as_std_uint(value), int(n))); }

//! Returns `value` rotated left by `n & (bit_size_of<T> - 1)`.
template<typename T>
[[nodiscard]]
ASMJIT_INLINE_CONSTEXPR T rol_wrap(const T& value, auto n) noexcept { return rol(value, as_std_uint(n) & (bit_size_of<T> - 1u)); }

//! Returns `value` rotated right by `n`.
template<typename T>
[[nodiscard]]
ASMJIT_INLINE_CONSTEXPR T ror(const T& value, auto n) noexcept { return T(std::rotr(as_std_uint(value), int(n))); }

//! Returns `value` rotated right by `n & (bit_size_of<T> - 1)`.
template<typename T>
[[nodiscard]]
ASMJIT_INLINE_CONSTEXPR T ror_wrap(const T& value, auto n) noexcept { return ror(value, as_std_uint(n) & (bit_size_of<T> - 1u)); }

//! \cond INTERNAL
#if defined(__GNUC__)
[[nodiscard]] ASMJIT_INLINE_NODEBUG uint32_t clz_32(uint32_t value) noexcept { return static_cast<uint32_t>(__builtin_clz(value)); }
[[nodiscard]] ASMJIT_INLINE_NODEBUG uint32_t clz_64(uint64_t value) noexcept { return static_cast<uint32_t>(__builtin_clzll(value)); }

[[nodiscard]] ASMJIT_INLINE_NODEBUG uint32_t ctz_32(uint32_t value) noexcept { return static_cast<uint32_t>(__builtin_ctz(value)); }
[[nodiscard]] ASMJIT_INLINE_NODEBUG uint32_t ctz_64(uint64_t value) noexcept { return static_cast<uint32_t>(__builtin_ctzll(value)); }
#endif // __GNUC__
//! \endcond

//! Consteval version of \ref clz() - can be only used at compile-time.
template<typename T>
[[nodiscard]] consteval uint32_t clz_const(const T& value) noexcept {
  auto x = as_std_uint(value);
  uint32_t n = 0u;
  while (n < bit_size_of<T> && !extract_msb(x)) { x <<= 1; n++; }
  return n;
}

//! Consteval version of \ref ctz() - can be only used at compile-time.
template<typename T>
[[nodiscard]] consteval uint32_t ctz_const(const T& value) noexcept {
  auto x = as_std_uint(value);
  uint32_t n = 0u;
  while (n < bit_size_of<T> && !(x & 1)) { x >>= 1; n++; }
  return n;
}

//! Counts the number of leading zeros (from left/msb bit) in `value`.
template<typename T>
[[nodiscard]] ASMJIT_INLINE_NODEBUG uint32_t clz(const T& value) noexcept {
#if defined(__GNUC__)
  if constexpr (sizeof(T) <= 4) {
    constexpr uint32_t size_diff = static_cast<uint32_t>(32u - bit_size_of<T>);
    return clz_32(static_cast<uint32_t>(value) << size_diff);
  }
  else if constexpr (sizeof(T) == sizeof(uint64_t)) {
    return clz_64(static_cast<uint64_t>(value));
  }
  else {
    return static_cast<uint32_t>(std::countl_zero(as_std_uint(value)));
  }
#else
  return static_cast<uint32_t>(std::countl_zero(as_std_uint(value)));
#endif
}

//! Counts the number of trailing zeros (from right/lsb bit) in `value`.
template<typename T>
[[nodiscard]]
ASMJIT_INLINE_CONSTEXPR uint32_t ctz(const T& value) noexcept {
#if defined(__GNUC__)
  if constexpr (sizeof(T) <= 4) {
    return ctz_32(static_cast<uint32_t>(value));
  }
  else if constexpr (sizeof(T) == sizeof(uint64_t)) {
    return ctz_64(static_cast<uint64_t>(value));
  }
  else {
    return static_cast<uint32_t>(std::countr_zero(as_std_uint(value)));
  }
#else
  return static_cast<uint32_t>(std::countr_zero(as_std_uint(value)));
#endif
}

//! Calculates count of bits set to 1 in the given `value`.
[[nodiscard]]
ASMJIT_INLINE_CONSTEXPR uint32_t popcnt(auto value) noexcept { return uint32_t(std::popcount(as_std_uint(value))); }

//! Expands most significant bit from `x` to all other bits and returns a value of the same type that can have either
//! all bits set or all bits zero, depending on the MSB bit of `x`.
template<typename T>
[[nodiscard]]
ASMJIT_INLINE_CONSTEXPR T expand_msb(const T& x) noexcept { return sar(x, bit_size_of<T> - 1u); }

//! Extracts most significant bit from `x` returning a value of the same type that can be either 0 or 1.
template<typename T>
[[nodiscard]]
ASMJIT_INLINE_CONSTEXPR T extract_msb(const T& x) noexcept { return shr(x, bit_size_of<T> - 1u); }

//! Creates a bit-mask that has `n` leading bits set (constexpr version).
template<typename T>
[[nodiscard]]
ASMJIT_INLINE_CONSTEXPR T leading_bits_run_const(auto n) noexcept {
  using U = std_uint_t<sizeof(T)>;

  if constexpr (sizeof(U) < sizeof(uintptr_t)) {
    return T(uintptr_t(bit_ones<U>) << (bit_size_of<T> - U(n)));
  }
  else {
    U neg_n = neg(U(n));
    return T(shl_wrap(expand_msb(neg_n), neg_n));
  }
}

//! Creates a bit-mask that has `n` leading bits set.
//!
//! \remarks This function can be used when `n >= 0 && n <= bit_size_of<T>`, which means that ait can create an
//! empty mask when `n == 0` and a mask where all bits are set when `n == bit_size_of<T>`.
template<typename T>
[[nodiscard]]
ASMJIT_INLINE_CONSTEXPR T leading_bits_run(auto n) noexcept { return leading_bits_run_const<T>(n); }

//! Creates a bit-mask that has `n` trailing bits set (constexpr version).
template<typename T>
[[nodiscard]]
ASMJIT_INLINE_CONSTEXPR T trailing_bits_run_const(auto n) noexcept {
  using U = std_uint_t<sizeof(T)>;

  if constexpr (sizeof(U) < sizeof(uintptr_t)) {
    return T(U((uintptr_t(1) << n) - uintptr_t(1)));
  }
  else {
    U neg_n = neg(U(n));
    return T(shr_wrap(expand_msb(neg_n), neg_n));
  }
}

//! Creates a bit-mask that has `n` trailing bits set.
//!
//! \remarks This function can be used when `n >= 0 && n <= bit_size_of<T>`, which means that ait can create an
//! empty mask when `n == 0` and a mask where all bits are set when `n == bit_size_of<T>`.
template<typename T>
[[nodiscard]]
ASMJIT_INLINE_NODEBUG T trailing_bits_run(auto n) noexcept {
#if defined(__BMI2__)
  if constexpr (sizeof(T) == 4u) {
    return T(_bzhi_u32(bit_ones<uint32_t>, uint32_t(n)));
  }
#if ASMJIT_TARGET_ARCH_BITS >= 64
  else if constexpr (sizeof(T) == 8u) {
    return T(_bzhi_u64(bit_ones<uint64_t>, uint32_t(n)));
  }
#endif
#endif
  return T(trailing_bits_run_const<T>(n));
}

//! Like `leading_bits_run()`, but assuming that `n` is always greater than `0` and lesser or equal to `bit_size_of<T>`.
//!
//! \remarks This is an optimized version of \ref leading_bits_run() that takes the advantage of the knowledge that `n`
//! cannot be zero. This means that there is one less border-case to handle, which should make it faster and the resulting
//! assembly shorter.
template<typename T>
[[nodiscard]]
ASMJIT_INLINE_CONSTEXPR T leading_bits_run_nz(auto n) noexcept { return shl_wrap(bit_ones<T>, neg(as_std_uint(n))); }

//! Like `trailing_bits_run()`, but assuming that `n` is always greater than `0` and lesser or equal to `bit_size_of<T>`.
//!
//! \remarks This is an optimized version of \ref trailing_bits_run() that takes the advantage of the knowledge that `n`
//! cannot be zero. This means that there is one less border-case to handle, which should make it faster and the resulting
//! assembly shorter.
template<typename T>
[[nodiscard]]
ASMJIT_INLINE_CONSTEXPR T trailing_bits_run_nz(auto n) noexcept { return shr_wrap(bit_ones<T>, neg(as_std_uint(n))); }

//! Returns `x & -x` - extracts the lowest set bit (like BLSI instruction).
template<typename T>
[[nodiscard]]
ASMJIT_INLINE_CONSTEXPR T extract_trailing_bit(const T& value) noexcept { return T(as_std_uint(value) & neg(as_std_uint(value))); }

//! Returns a bit-mask that has `n` bit set (multiple arguments).
template<typename T, typename... Args>
[[nodiscard]]
ASMJIT_INLINE_CONSTEXPR T leading_bit_mask(Args&&... args) noexcept { return T( ((shl_wrap(T(1), ~as_std_uint(args))) | ...) ); }

//! Returns a bit-mask that has `n` bit set (multiple arguments).
template<typename T, typename... Args>
[[nodiscard]]
ASMJIT_INLINE_CONSTEXPR T bit_mask(Args&&... args) noexcept { return T( ((T(1) << as_std_uint(args)) | ...) ); }

//! Returns a bit-mask that has `n` bit set (multiple arguments). Each bit-index would wrap-around.
template<typename T, typename... Args>
[[nodiscard]]
ASMJIT_INLINE_CONSTEXPR T bit_mask_wrap(Args&&... args) noexcept { return T( ((shl_wrap(T(1), args)) | ...) ); }

// Fills all trailing bits right of the given `value` from the first most significant bit set.
template<typename T>
[[nodiscard]]
ASMJIT_INLINE_NODEBUG T fill_trailing_bits(const T& value) noexcept {
  auto v = as_basic_uint(value);

  uint32_t leading_count = clz(v | 1u);
  return T(((bit_ones<decltype(v)> >> 1u) >> leading_count) | v);
}

// Is Leading|Trailing|Consecutive Mask
// ====================================

// Tests whether the given value `x` is a consecutive mask of bits that start at the most significant bit.
template<typename T>
[[nodiscard]]
ASMJIT_INLINE_CONSTEXPR bool is_leading_mask(const T& x) noexcept { return x && !(as_std_uint(x) + extract_trailing_bit(as_std_uint(x))); }

// Tests whether the given value `x` is a consecutive mask of bits that start at the least significant bit.
template<typename T>
[[nodiscard]]
ASMJIT_INLINE_CONSTEXPR bool is_trailing_mask(const T& x) noexcept { return x && !((as_std_uint(x) + 1u) & as_std_uint(x)); }

// Tests whether the given value `x` contains at least one bit or whether it contains more bits but all consecutive.
//
// This function is similar to \ref is_trailing_mask(), but the mask doesn't have to start at the least significant bit.
template<typename T>
[[nodiscard]]
ASMJIT_INLINE_CONSTEXPR bool is_consecutive_mask(const T& x) noexcept { return x && is_trailing_mask((as_std_uint(x) - 1u) | as_std_uint(x)); }

// Is Power of 2
// =============

//! Tests whether `x` is a power of two (only one bit is set).
template<typename T>
[[nodiscard]]
ASMJIT_INLINE_CONSTEXPR bool is_power_of_2(T x) noexcept { return std::has_single_bit(as_std_uint(x)); }

//! Tests whether `x` is a power of two up to `n`.
template<typename T, typename N>
[[nodiscard]]
ASMJIT_INLINE_CONSTEXPR bool is_power_of_2_up_to(T x, N n) noexcept {
  using U = std::make_unsigned_t<T>;
  U x_minus_1 = U(U(x) - U(1));
  return bool_and(x_minus_1 < U(n), !(U(x) & x_minus_1));
}

//! Tests whether `x` is either zero or a power of two (only one bit is set).
template<typename T>
[[nodiscard]]
ASMJIT_INLINE_CONSTEXPR bool is_zero_or_power_of_2(T x) noexcept {
  using U = std::make_unsigned_t<T>;
  return !(U(x) & (U(x) - U(1)));
}

//! Tests whether `x` is either zero or a power of two up to `n`.
template<typename T, typename N>
[[nodiscard]]
ASMJIT_INLINE_CONSTEXPR bool is_zero_or_power_of_2_up_to(T x, N n) noexcept {
  using U = std::make_unsigned_t<T>;
  return bool_and(U(x) <= U(n), !(U(x) & (U(x) - U(1))));
}

// Has At Least 2 Bits Set
// =======================

//! Tests whether the given `value` has at least 2 bits set.
//!
//! The operation it performs could be rewritten as `popcnt(value) >= 2`, but it's more efficient especially if there
//! is no native population count support on the target architecture (or it's behind a compiler flag that was not used
//! during compilation).
[[nodiscard]]
ASMJIT_INLINE_CONSTEXPR bool has_at_least_2_bits_set(auto value) noexcept {
  auto v = as_basic_uint(value);
  return (v & (v - 1u)) != 0u;
}

// Pair Bits
// =========

template<typename T>
static ASMJIT_INLINE T pair_bits(const T& bits) noexcept {
  using U = std_uint_t<sizeof(T)>;
  return T(U(bits) | ((U(bits) & repeat_byte<U>(0x55u)) << 1) | ((U(bits) & repeat_byte<U>(0xAAu)) >> 1));
}

// Alignment
// =========

template<typename X, typename Y>
[[nodiscard]]
ASMJIT_INLINE_CONSTEXPR bool is_aligned(X base, Y alignment) noexcept {
  using U = std_uint_t<sizeof(X)>;
  return ((U)base % (U)alignment) == 0;
}

template<typename X, typename Y>
[[nodiscard]]
ASMJIT_INLINE_CONSTEXPR X align_up(X x, Y alignment) noexcept {
  using U = std_uint_t<sizeof(X)>;
  return (X)( ((U)x + ((U)(alignment) - 1u)) & ~((U)(alignment) - 1u) );
}

template<typename T>
[[nodiscard]]
ASMJIT_INLINE_CONSTEXPR T align_up_power_of_2(T x) noexcept {
  using U = std_uint_t<sizeof(T)>;
  return (T)(fill_trailing_bits(U(x) - 1u) + 1u);
}

//! Returns either zero or a positive difference between `base` and `base` when aligned up to `alignment`.
template<typename X, typename Y>
[[nodiscard]]
ASMJIT_INLINE_CONSTEXPR std_uint_t<sizeof(X)> align_up_diff(X base, Y alignment) noexcept {
  using U = std_uint_t<sizeof(X)>;
  return align_up(U(base), alignment) - U(base);
}

template<typename X, typename Y>
[[nodiscard]]
ASMJIT_INLINE_CONSTEXPR X align_down(X x, Y alignment) noexcept {
  using U = std_uint_t<sizeof(X)>;
  return (X)((U)x & ~((U)(alignment) - 1u));
}

// ByteSwap
// ========

[[nodiscard]]
ASMJIT_INLINE_NODEBUG uint16_t byteswap16(uint16_t x) noexcept {
  return uint16_t(((x >> 8) & 0xFFu) | ((x & 0xFFu) << 8));
}

[[nodiscard]]
ASMJIT_INLINE_NODEBUG uint32_t byteswap32(uint32_t x) noexcept {
  return (x << 24) | (x >> 24) | ((x << 8) & 0x00FF0000u) | ((x >> 8) & 0x0000FF00);
}

[[nodiscard]]
ASMJIT_INLINE_NODEBUG uint64_t byteswap64(uint64_t x) noexcept {
#if defined(__GNUC__)
  return uint64_t(__builtin_bswap64(uint64_t(x)));
#else
  return (uint64_t(byteswap32(uint32_t(uint64_t(x) >> 32        )))      ) |
         (uint64_t(byteswap32(uint32_t(uint64_t(x) & 0xFFFFFFFFu))) << 32) ;
#endif
}

template<typename T>
[[nodiscard]]
ASMJIT_INLINE_NODEBUG T byteswap(const T& x) noexcept {
  static_assert(std::is_integral_v<T>, "byteswap() expects the given type to be integral");
  if constexpr (sizeof(T) == 8u) {
    return T(byteswap64(uint64_t(x)));
  }
  else if constexpr (sizeof(T) == 4u) {
    return T(byteswap32(uint32_t(x)));
  }
  else if constexpr (sizeof(T) == 2u) {
    return T(byteswap16(uint16_t(x)));
  }
  else {
    static_assert(sizeof(T) == 1u, "byteswap() can be used with a type of size 1, 2, 4, or 8");
    return x;
  }
}

template<ByteOrder BO, typename T>
[[nodiscard]]
ASMJIT_INLINE_NODEBUG T byteswap(const T& x) noexcept {
  if constexpr (BO == ByteOrder::kNative) {
    return x;
  }
  else {
    return byteswap<T>(x);
  }
}

template<typename T>
[[nodiscard]]
ASMJIT_INLINE_NODEBUG T byteswap_le(const T& x) noexcept { return byteswap<ByteOrder::kLE>(x); }

template<typename T>
[[nodiscard]]
ASMJIT_INLINE_NODEBUG T byteswap_be(const T& x) noexcept { return byteswap<ByteOrder::kBE>(x); }

// Bit Reverse
// ===========

template<typename T>
[[nodiscard]]
static ASMJIT_INLINE T bit_reverse(const T& x) noexcept {
  auto v = as_basic_uint(x);

  auto m1 = as_basic_uint(T(0x5555555555555555u & bit_ones<T>));
  auto m2 = as_basic_uint(T(0x3333333333333333u & bit_ones<T>));
  auto m4 = as_basic_uint(T(0x0F0F0F0F0F0F0F0Fu & bit_ones<T>));

  v = ((v >> 1) & m1) | ((v & m1) << 1);
  v = ((v >> 2) & m2) | ((v & m2) << 2);
  v = ((v >> 4) & m4) | ((v & m4) << 4);

  return byteswap(T(v));
}

// Is Int
// ======

//! Checks whether the given integer `x` can be casted to a signed N-bit integer.
template<size_t N, typename T>
[[nodiscard]]
ASMJIT_INLINE_CONSTEXPR bool is_int_n(const T& x) noexcept {
  constexpr size_t bit_size = bit_size_of<T>;

  if constexpr (bit_size < N || (bit_size == N && std::is_signed_v<T>)) {
    // Always castable to a wider integer or to a signed integer of the same width.
    return true;
  }
  else if constexpr (std::is_signed_v<T>) {
    // A cast of a signed integer to a smaller signed integer.
    using U = std_uint_t<sizeof(T)>;
    T maximum_value = T(trailing_bits_run_const<U>(N - 1));
    T minimum_value = T(-maximum_value - 1);

    return bool_and(x >= minimum_value, x <= maximum_value);
  }
  else {
    // A cast of an unsigned integer to a smaller signed integer.
    T maximum_value = trailing_bits_run_const<T>(N - 1);
    return x <= maximum_value;
  }
}

//! Checks whether the given integer `x` can be casted to an unsigned N-bit integer.
template<size_t N, typename T>
[[nodiscard]]
ASMJIT_INLINE_CONSTEXPR bool is_uint_n(const T& x) noexcept {
  constexpr size_t bit_size = bit_size_of<T>;

  if constexpr (bit_size <= N && std::is_unsigned_v<T>) {
    // Always castable to a wider integer if T is unsigned.
    return true;
  }
  else if constexpr (bit_size <= N && std::is_signed_v<T>) {
    return x >= T(0);
  }
  else {
    // A cast of an integer to a smaller unsigned integer.
    return as_std_uint(x) <= trailing_bits_run_const<std_uint_t<sizeof(T)>>(N);
  }
}
// CSA
// ===

//! Carry-save adder (vertical sum of bits from `a`, `b`, and `c` to `lo` and `hi`).
template<typename T>
ASMJIT_INLINE_NODEBUG void csa(T& hi, T& lo, T a, T b, T c) noexcept {
  T u = T(a ^ b);
  hi = T((a & b) | (u & c));
  lo = T(u ^ c);
}

// Bool As Flag
// ============

template<auto Flag, typename T>
ASMJIT_INLINE_CONSTEXPR decltype(Flag) bool_as_flag(const T& v) noexcept {
  using FlagType = decltype(Flag);
  return FlagType(as_std_uint(v) << ctz_const(Flag));
}

// Enumerate
// =========

//! A helper class that can be used to iterate over enum values.
template<typename T>
struct Enumerate {
  using UnderlyingType = std::underlying_type_t<T>;

  UnderlyingType _begin;
  UnderlyingType _end;

  struct Iterator {
    UnderlyingType value;

    ASMJIT_INLINE_CONSTEXPR void operator++() { value = UnderlyingType(value + UnderlyingType(1)); }

    [[nodiscard]] ASMJIT_INLINE_CONSTEXPR T operator*() const { return T(value); }
    [[nodiscard]] ASMJIT_INLINE_CONSTEXPR bool operator==(const Iterator& other) const noexcept { return value == other.value; }
    [[nodiscard]] ASMJIT_INLINE_CONSTEXPR bool operator!=(const Iterator& other) const noexcept { return value != other.value; }
  };

  [[nodiscard]]
  ASMJIT_INLINE_CONSTEXPR Iterator begin() const noexcept { return Iterator{_begin}; }

  [[nodiscard]]
  ASMJIT_INLINE_CONSTEXPR Iterator end() const noexcept { return Iterator{_end}; }
};

template<typename T>
static ASMJIT_INLINE_CONSTEXPR Enumerate<T> enumerate(T from, T to) {
  using U = std::underlying_type_t<T>;
  return Enumerate<T>{U(from), U(U(to) + U(1u))};
}

template<typename T>
static ASMJIT_INLINE_CONSTEXPR Enumerate<T> enumerate(T to = T::kMaxValue) {
  return enumerate<T>(T(0), to);
}

// Arithmetic Operations
// =====================

template<typename T>
ASMJIT_INLINE T mul_high(const T& a, const T& b) noexcept
  requires(std::is_integral_v<T> && sizeof(T) <= 8u)
{
  if constexpr (sizeof(T) < 8) {
    using W = std_int_t<max(sizeof(T) * 2u, size_t(4)), std::is_unsigned_v<T>>;
    using U = std::make_unsigned_t<W>;

    U result = U(W(a) * W(b));
    return T(result >> (sizeof(T) * 8));
  }
  else {
#if defined(_MSC_VER) && defined(_M_X64)
    if constexpr (std::is_signed_v<T>) {
      int64_t hi;
      int64_t lo = _mul128(a, b, &hi);
      return T(hi);
    }
    else {
      uint64_t hi;
      uint64_t lo = _umul128(a, b, &hi);
      return T(hi);
    }
#elif defined(__GNUC__) && ASMJIT_TARGET_ARCH_BITS >= 64
    if constexpr (std::is_signed_v<T>) {
      __int128 product = (__int128)a * b;
      return static_cast<int64_t>(product >> 64);
    }
    else {
      unsigned __int128 product = (unsigned __int128)a * b;
      return static_cast<uint64_t>(product >> 64);
    }
#else
    uint64_t ua = uint64_t(a);
    uint64_t ub = uint64_t(b);

    uint64_t al = ua & 0xFFFFFFFFu;
    uint64_t ah = ua >> 32;
    uint64_t bl = ub & 0xFFFFFFFFu;
    uint64_t bh = ub >> 32;

    uint64_t ll = al * bl;
    uint64_t hl = ah * bl;
    uint64_t lh = al * bh;
    uint64_t hh = ah * bh;

    uint64_t carry = ((ll >> 32) + uint32_t(hl) + uint32_t(lh)) >> 32;
    uint64_t uh = hh + (hl >> 32) + (lh >> 32) + carry;

    if constexpr (std::is_signed_v<T>) {
      if (a < 0) uh -= ub;
      if (b < 0) uh -= ua;
      return T(uh);
    }
    else {
      return T(uh);
    }
#endif
  }
}

// Overflowing Arithmetic
// ======================

//! \cond
[[nodiscard]]
ASMJIT_INLINE_CONSTEXPR OverflowFlag operator|(OverflowFlag a, OverflowFlag b) noexcept {  return OverflowFlag(uint32_t(a) | uint32_t(b)); }
ASMJIT_INLINE_CONSTEXPR OverflowFlag& operator|=(OverflowFlag& a, OverflowFlag b) noexcept { a = a | b; return a; }

template<typename T>
[[nodiscard]]
ASMJIT_INLINE T add_overflow_generic(const T& x, const T& y, OverflowFlag& of) noexcept {
  using U = std::make_unsigned_t<T>;

  U result = U(U(x) + U(y));
  of |= OverflowFlag(std::is_unsigned_v<T> ? result < U(x) : T((U(x) ^ ~U(y)) & (U(x) ^ result)) < 0);
  return T(result);
}

template<typename T>
[[nodiscard]]
ASMJIT_INLINE T sub_overflow_generic(const T& x, const T& y, OverflowFlag& of) noexcept {
  using U = std::make_unsigned_t<T>;

  U result = U(U(x) - U(y));
  of |= OverflowFlag(std::is_unsigned_v<T> ? result > U(x) : T((U(x) ^ U(y)) & (U(x) ^ result)) < 0);
  return T(result);
}

template<typename T>
[[nodiscard]]
ASMJIT_INLINE T mul_overflow_generic(const T& x, const T& y, OverflowFlag& of) noexcept {
  if constexpr (sizeof(T) < 8u) {
    using I = std_int_t<sizeof(T) * 2u, std::is_unsigned_v<T>>;
    using U = std::make_unsigned_t<I>;

    U mask = U(bit_ones<T>);
    if constexpr (std::is_signed_v<T>) {
      U prod = U(I(x)) * U(I(y));
      of |= OverflowFlag(I(prod) < I(min_value<T>()) || I(prod) > I(max_value<T>()));
      return T(I(prod & mask));
    }
    else {
      U prod = U(x) * U(y);
      of |= OverflowFlag((prod & ~mask) != 0);
      return T(prod & mask);
    }
  }
  else if constexpr (sizeof(T) == 8u) {
    uint64_t result = uint64_t(x) * uint64_t(y);
    if constexpr (std::is_signed_v<T>) {
      of |= OverflowFlag(x != 0 && (int64_t(result) / x != y));
    }
    else {
      of |= OverflowFlag(y != 0 && bit_ones<uint64_t> / y < x);
    }
    return T(result);
  }
}
//! \endcond

template<typename T>
[[nodiscard]]
ASMJIT_INLINE T add_overflow(const T& x, const T& y, OverflowFlag& of) noexcept {
#if defined(__GNUC__)
  T result;
  of |= OverflowFlag(__builtin_add_overflow(x, y, &result));
  return result;
#else
  return add_overflow_generic(x, y, of);
#endif
}

template<typename T>
[[nodiscard]]
ASMJIT_INLINE T sub_overflow(const T& x, const T& y, OverflowFlag& of) noexcept {
#if defined(__GNUC__)
  T result;
  of |= OverflowFlag(__builtin_sub_overflow(x, y, &result));
  return result;
#else
  return sub_overflow_generic(x, y, of);
#endif
}

template<typename T>
[[nodiscard]]
ASMJIT_INLINE T mul_overflow(const T& x, const T& y, OverflowFlag& of) noexcept {
#if defined(__GNUC__)
  T result;
  of |= OverflowFlag(__builtin_mul_overflow(x, y, &result));
  return result;
#elif defined(_MSC_VER) && defined(_M_X64)
  if constexpr (sizeof(T) == 8u) {
    if constexpr (std::is_signed_v<T>) {
      int64_t hi;
      int64_t lo = _mul128(x, y, &hi);
      of |= OverflowFlag((lo >> 63) != hi);
      return lo;
    }
    else {
      uint64_t hi;
      uint64_t lo = _umul128(x, y, &hi);
      of |= OverflowFlag(hi != 0u);
      return lo;
    }
  }
  else {
    return mul_overflow_generic(x, y, of);
  }
#else
  return mul_overflow_generic(x, y, of);
#endif
}

template<typename T>
[[nodiscard]]
ASMJIT_INLINE T madd_overflow(const T& x, const T& y, const T& addend, OverflowFlag& of) noexcept {
  return add_overflow(mul_overflow(x, y, of), addend, of);
}

[[nodiscard]]
ASMJIT_INLINE_CONSTEXPR bool did_overflow(OverflowFlag of) noexcept { return of != OverflowFlag(0); }

// Saturating Arithmetic
// =====================

template<typename T>
[[nodiscard]]
ASMJIT_INLINE T add_saturate(const T& x, const T& y) noexcept {
  using U = std_uint_t<sizeof(T)>;

  OverflowFlag of{};
  U result = U(add_overflow(x, y, of));

  if constexpr (std::is_signed_v<T>) {
    U msk = bool_as_mask<U>(of) >> 1;
    U sat = ~U(msk + extract_msb(result));
    return T(did_overflow(of) ? sat : result);
  }
  else {
    return T(result | bool_as_mask<U>(of));
  }
}

template<typename T>
[[nodiscard]]
ASMJIT_INLINE T sub_saturate(const T& x, const T& y) noexcept {
  using U = std_uint_t<sizeof(T)>;

  OverflowFlag of{};
  U result = U(sub_overflow(x, y, of));

  if constexpr (std::is_signed_v<T>) {
    U msk = bool_as_mask<U>(of) >> 1;
    U sat = ~U(msk + extract_msb(result));
    return T(did_overflow(of) ? sat : result);
  }
  else {
    return T(result & ~bool_as_mask<U>(of));
  }
}

template<typename T>
[[nodiscard]]
ASMJIT_INLINE T mul_saturate(const T& x, const T& y) noexcept {
  using U = std_uint_t<sizeof(T)>;

  OverflowFlag of{};
  U result = U(mul_overflow(x, y, of));

  if constexpr (std::is_signed_v<T>) {
    U msk = bool_as_mask<U>(of) >> 1;
    U sat = U(msk + extract_msb(U(x) ^ U(y)));
    return T(did_overflow(of) ? sat : result);
  }
  else {
    return T(result | bool_as_mask<U>(of));
  }
}

template<typename Dst, typename Src>
[[nodiscard]]
ASMJIT_INLINE_CONSTEXPR Dst clamp(const Src& x) noexcept {
  using U = std::make_unsigned_t<Src>;

  constexpr Dst dst_min = min_value<Dst>();
  constexpr Dst dst_max = max_value<Dst>();

  if constexpr (std::is_unsigned_v<Src>) {
    // Saturate to Dst<signed|unsigned> from Src<unsigned>.
    if constexpr (sizeof(Dst) > sizeof(Src)) {
      return Dst(x);
    }
    else {
      return Dst(min(U(x), U(dst_max)));
    }
  }
  else if constexpr (std::is_unsigned_v<Dst>) {
    // Saturate to Dst<unsigned> from Src<signed>.
    U xu = U(x) & ~expand_msb(U(x));
    if constexpr (sizeof(Dst) >= sizeof(Src)) {
      return Dst(xu);
    }
    else {
      return Dst(min(xu, U(dst_max)));
    }
  }
  else {
    // Saturate to Dst<signed> from Src<signed>.
    if constexpr (sizeof(Dst) >= sizeof(Src)) {
      return Dst(x);
    }
    else {
      return x < Src(dst_min) ? dst_min :
             x > Src(dst_max) ? dst_max : Dst(x);
    }
  }
}

// Positive Modulo
// ===============

//! Returns a positive modulo - similar to `x % y`, but for example `-4 % 3` would return `2` instead of `-1`.
template<typename T>
[[nodiscard]]
ASMJIT_INLINE_NODEBUG T pmod(const T& x, const T& y) noexcept {
  T result = x % y;
  return std::is_unsigned_v<T> ? result : result + (y + (result >> (bit_size_of<T> - 1)));
}

// Pointer Utilities
// =================

//! Casts pointer to a different type by keeping its qualifiers.
template<typename Dst, typename Src>
[[nodiscard]]
ASMJIT_INLINE_NODEBUG auto ptr_recast(Src* ptr) noexcept {
  if constexpr (std::is_const_v<Src>) {
    return static_cast<const Dst*>(ptr);
  }
  else {
    return static_cast<Dst*>(ptr);
  }
}

//! Casts pointer to `uintptr_t`.
//!
//! \remarks This function is used within templates to make sure that the input is actually a pointer and
//! not some other type.
template<typename T>
[[nodiscard]]
ASMJIT_INLINE_NODEBUG uintptr_t ptr_as_uintptr(const T* ptr) noexcept { return reinterpret_cast<uintptr_t>(ptr); }

//! Applies a byte-offset `n` to the given `ptr` and casts the result to `Dst*` type.
template<typename Dst, typename Src>
[[nodiscard]]
ASMJIT_INLINE_NODEBUG auto offset_ptr(Src* ptr, auto&& n) noexcept {
  return ptr_recast<Dst>(ptr_recast<void>(ptr_recast<char>(ptr_recast<void>(ptr)) + n));
}

//! Applies a byte-offset `n` to the given `ptr` and returns it.
template<typename T>
[[nodiscard]]
ASMJIT_INLINE_NODEBUG auto offset_ptr(T* ptr, auto&& n) noexcept {
  return offset_ptr<T, T>(ptr, n);
}

//! Applies a byte-offset `-n` to the given `ptr` and casts the result to `Dst*` type.
template<typename Dst, typename Src>
[[nodiscard]]
ASMJIT_INLINE_NODEBUG auto deoffset_ptr(Src* ptr, auto&& n) noexcept {
  return ptr_recast<Dst>(ptr_recast<void>(ptr_recast<char>(ptr_recast<void>(ptr)) - n));
}

//! Applies a byte-offset `-n` to the given `ptr` and returns it.
template<typename T>
[[nodiscard]]
ASMJIT_INLINE_NODEBUG auto deoffset_ptr(T* ptr, auto&& n) noexcept {
  return deoffset_ptr<T, T>(ptr, n);
}

//! Returns a byte difference between `base` and `ptr` pointer - `ptr` must be greater or equal to `base`.
template<typename T1, typename T2>
[[nodiscard]]
ASMJIT_INLINE size_t ptr_byte_diff(const T1* base, const T2* ptr) noexcept {
  // The `ptr_byte_diff` function expects `ptr` to always be greater than `base`, thus the
  // result must be either zero or a positive number as it's represented by an unsigned type.
  ASMJIT_ASSERT(reinterpret_cast<const uint8_t*>(base) <= reinterpret_cast<const uint8_t*>(ptr));
  return (size_t)(reinterpret_cast<const uint8_t*>(ptr) - reinterpret_cast<const uint8_t*>(base));
}

//! Returns true if all passed pointers are aligned to the required `Alignment`.
template<size_t Alignment, typename... Args>
[[nodiscard]]
ASMJIT_INLINE_NODEBUG bool ptrs_are_aligned(Args&&... args) noexcept {
  return ((ptr_as_uintptr(args) | ...)) % uintptr_t(Alignment) == 0;
}

//! Returns true if all passed pointers are either aligned to `Alignment` or misaligned the
//! same way (they are sharing the bits that you would get by applying `ptr % Alignment`.
template<size_t Alignment, typename T1, typename T2>
[[nodiscard]]
ASMJIT_INLINE_NODEBUG bool ptrs_share_alignment(const T1* ptr1, const T2* ptr2) noexcept {
  return ((ptr_as_uintptr(ptr1) ^ ptr_as_uintptr(ptr2))) % uintptr_t(Alignment) == 0;
}

//! Tests whether `[base, base + size)` has `ptr` within it.
//!
//! This can be used as a test for aliased arguments when it comes to containers that own their data (not
//! generic spans).
template<typename T>
[[nodiscard]]
ASMJIT_INLINE_NODEBUG bool is_ptr_within(const T* base, size_t size, const T* ptr) noexcept {
  const T* end = base + size;
  return bool_and(ptr >= base, ptr < end);
}

} // {anonymous}

// Operators
// =========

//! \cond INTERNAL
struct Set    { template<typename T> static ASMJIT_INLINE_NODEBUG T op(T x, T y) noexcept { maybe_unused(x); return y; } };
struct SetNot { template<typename T> static ASMJIT_INLINE_NODEBUG T op(T x, T y) noexcept { maybe_unused(x); return ~y; } };
struct And    { template<typename T> static ASMJIT_INLINE_NODEBUG T op(T x, T y) noexcept { return x & y; } };
struct AndNot { template<typename T> static ASMJIT_INLINE_NODEBUG T op(T x, T y) noexcept { return x & ~y; } };
struct NotAnd { template<typename T> static ASMJIT_INLINE_NODEBUG T op(T x, T y) noexcept { return ~x & y; } };
struct Xor    { template<typename T> static ASMJIT_INLINE_NODEBUG T op(T x, T y) noexcept { return x ^ y; } };
struct Add    { template<typename T> static ASMJIT_INLINE_NODEBUG T op(T x, T y) noexcept { return x + y; } };
struct Sub    { template<typename T> static ASMJIT_INLINE_NODEBUG T op(T x, T y) noexcept { return x - y; } };
struct Min    { template<typename T> static ASMJIT_INLINE_NODEBUG T op(T x, T y) noexcept { return min<T>(x, y); } };
struct Max    { template<typename T> static ASMJIT_INLINE_NODEBUG T op(T x, T y) noexcept { return max<T>(x, y); } };

struct Or {
  template<typename T> static ASMJIT_INLINE_NODEBUG T op(T a, T b) noexcept { return a | b; }
  template<typename T> static ASMJIT_INLINE_NODEBUG T op(T a, T b, T c) noexcept { return a | b | c; }
  template<typename T> static ASMJIT_INLINE_NODEBUG T op(T a, T b, T c, T d) noexcept { return  a | b | c | d; }
};
//! \endcond

// Pack & Unpack
// =============

template<typename T>
[[nodiscard]]
static ASMJIT_INLINE_CONSTEXPR uint32_t unpack_u32_at_0(T x) noexcept {
  constexpr uint32_t kShift = kIsLittleEndian ? 0 : 32;
  return uint32_t((uint64_t(x) >> kShift) & 0xFFFFFFFFu);
}

template<typename T>
[[nodiscard]]
static ASMJIT_INLINE_CONSTEXPR uint32_t unpack_u32_at_1(T x) noexcept {
  constexpr uint32_t kShift = kIsLittleEndian ? 32 : 0;
  return uint32_t((uint64_t(x) >> kShift) & 0xFFFFFFFFu);
}

} // {axl}

//! \}

ASMJIT_END_NAMESPACE

//! \cond INTERNAL

//! \addtogroup asmjit_axl
//! \{

//! Implementation of a placement new so we don't have to depend on `<new>`.
ASMJIT_INLINE_NODEBUG void* operator new(size_t, const asmjit::axl::PlacementNew& p) noexcept {
#if defined(_MSC_VER) && !defined(__clang__)
  __assume(p.ptr != nullptr); // Otherwise MSVC would emit a nullptr check.
#endif
  return p.ptr;
}

//! Implementation of a placement delete (nop) so we don't have to depend on <new>.
ASMJIT_INLINE_NODEBUG void operator delete(void*, const asmjit::axl::PlacementNew&) noexcept {}

//! \}

//! \endcond

#endif // ASMJIT_AXL_COMMONS_H_INCLUDED
