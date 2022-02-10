// This file is part of AsmJit project <https://asmjit.com>
//
// See asmjit.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#ifndef ASMJIT_CORE_SUPPORT_H_INCLUDED
#define ASMJIT_CORE_SUPPORT_H_INCLUDED

#include "../core/globals.h"

#if defined(_MSC_VER)
  #include <intrin.h>
#endif

ASMJIT_BEGIN_NAMESPACE

//! \addtogroup asmjit_utilities
//! \{

//! Contains support classes and functions that may be used by AsmJit source and header files. Anything defined
//! here is considered internal and should not be used outside of AsmJit and related projects like AsmTK.
namespace Support {

// Support - Basic Traits
// ======================

#if ASMJIT_ARCH_X86
typedef uint8_t FastUInt8;
#else
typedef uint32_t FastUInt8;
#endif

//! \cond INTERNAL
namespace Internal {
  template<typename T, size_t Alignment>
  struct AliasedUInt {};

  template<> struct AliasedUInt<uint16_t, 2> { typedef uint16_t ASMJIT_MAY_ALIAS T; };
  template<> struct AliasedUInt<uint32_t, 4> { typedef uint32_t ASMJIT_MAY_ALIAS T; };
  template<> struct AliasedUInt<uint64_t, 8> { typedef uint64_t ASMJIT_MAY_ALIAS T; };

  template<> struct AliasedUInt<uint16_t, 1> { typedef uint16_t ASMJIT_MAY_ALIAS ASMJIT_ALIGN_TYPE(T, 1); };
  template<> struct AliasedUInt<uint32_t, 1> { typedef uint32_t ASMJIT_MAY_ALIAS ASMJIT_ALIGN_TYPE(T, 1); };
  template<> struct AliasedUInt<uint32_t, 2> { typedef uint32_t ASMJIT_MAY_ALIAS ASMJIT_ALIGN_TYPE(T, 2); };
  template<> struct AliasedUInt<uint64_t, 1> { typedef uint64_t ASMJIT_MAY_ALIAS ASMJIT_ALIGN_TYPE(T, 1); };
  template<> struct AliasedUInt<uint64_t, 2> { typedef uint64_t ASMJIT_MAY_ALIAS ASMJIT_ALIGN_TYPE(T, 2); };
  template<> struct AliasedUInt<uint64_t, 4> { typedef uint64_t ASMJIT_MAY_ALIAS ASMJIT_ALIGN_TYPE(T, 4); };

  // StdInt    - Make an int-type by size (signed or unsigned) that is the
  //             same as types defined by <stdint.h>.
  // Int32Or64 - Make an int-type that has at least 32 bits: [u]int[32|64]_t.

  template<size_t Size, unsigned Unsigned>
  struct StdInt {}; // Fail if not specialized.

  template<> struct StdInt<1, 0> { typedef int8_t   Type; };
  template<> struct StdInt<1, 1> { typedef uint8_t  Type; };
  template<> struct StdInt<2, 0> { typedef int16_t  Type; };
  template<> struct StdInt<2, 1> { typedef uint16_t Type; };
  template<> struct StdInt<4, 0> { typedef int32_t  Type; };
  template<> struct StdInt<4, 1> { typedef uint32_t Type; };
  template<> struct StdInt<8, 0> { typedef int64_t  Type; };
  template<> struct StdInt<8, 1> { typedef uint64_t Type; };

  template<typename T, int Unsigned = std::is_unsigned<T>::value>
  struct Int32Or64 : public StdInt<sizeof(T) <= 4 ? size_t(4) : sizeof(T), Unsigned> {};
}
//! \endcond

template<typename T>
static constexpr bool isUnsigned() noexcept { return std::is_unsigned<T>::value; }

//! Casts an integer `x` to either `int32_t` or `int64_t` depending on `T`.
template<typename T>
static constexpr typename Internal::Int32Or64<T, 0>::Type asInt(const T& x) noexcept {
  return (typename Internal::Int32Or64<T, 0>::Type)x;
}

//! Casts an integer `x` to either `uint32_t` or `uint64_t` depending on `T`.
template<typename T>
static constexpr typename Internal::Int32Or64<T, 1>::Type asUInt(const T& x) noexcept {
  return (typename Internal::Int32Or64<T, 1>::Type)x;
}

//! Casts an integer `x` to either `int32_t`, uint32_t`, `int64_t`, or `uint64_t` depending on `T`.
template<typename T>
static constexpr typename Internal::Int32Or64<T>::Type asNormalized(const T& x) noexcept {
  return (typename Internal::Int32Or64<T>::Type)x;
}

//! Casts an integer `x` to the same type as defined by `<stdint.h>`.
template<typename T>
static constexpr typename Internal::StdInt<sizeof(T), isUnsigned<T>()>::Type asStdInt(const T& x) noexcept {
  return (typename Internal::StdInt<sizeof(T), isUnsigned<T>()>::Type)x;
}

//! A helper class that can be used to iterate over enum values.
template<typename T, T from = (T)0, T to = T::kMaxValue>
struct EnumValues {
  typedef typename std::underlying_type<T>::type ValueType;

  struct Iterator {
    ValueType value;

    inline T operator*() const { return (T)value; }
    inline void operator++() { ++value; }

    inline bool operator==(const Iterator& other) const noexcept { return value == other.value; }
    inline bool operator!=(const Iterator& other) const noexcept { return value != other.value; }
  };

  inline Iterator begin() const noexcept { return Iterator{ValueType(from)}; }
  inline Iterator end() const noexcept { return Iterator{ValueType(to) + 1}; }
};

// Support - BitCast
// =================

//! \cond
namespace Internal {
  template<typename DstT, typename SrcT>
  union BitCastUnion {
    inline BitCastUnion(SrcT src) noexcept : src(src) {}
    SrcT src;
    DstT dst;
  };
}
//! \endcond

//! Bit-casts from `Src` type to `Dst` type.
//!
//! Useful to bit-cast between integers and floating points.
template<typename Dst, typename Src>
static inline Dst bitCast(const Src& x) noexcept { return Internal::BitCastUnion<Dst, Src>(x).dst; }

// Support - BitOps
// ================

//! Storage used to store a pack of bits (should by compatible with a machine word).
typedef Internal::StdInt<sizeof(uintptr_t), 1>::Type BitWord;

template<typename T>
static constexpr uint32_t bitSizeOf() noexcept { return uint32_t(sizeof(T) * 8u); }

//! Number of bits stored in a single `BitWord`.
static constexpr uint32_t kBitWordSizeInBits = bitSizeOf<BitWord>();

//! Returns `0 - x` in a safe way (no undefined behavior), works for unsigned numbers as well.
template<typename T>
static constexpr T neg(const T& x) noexcept {
  typedef typename std::make_unsigned<T>::type U;
  return T(U(0) - U(x));
}

template<typename T>
static constexpr T allOnes() noexcept { return neg<T>(T(1)); }

//! Returns `x << y` (shift left logical) by explicitly casting `x` to an unsigned type and back.
template<typename X, typename Y>
static constexpr X shl(const X& x, const Y& y) noexcept {
  typedef typename std::make_unsigned<X>::type U;
  return X(U(x) << y);
}

//! Returns `x >> y` (shift right logical) by explicitly casting `x` to an unsigned type and back.
template<typename X, typename Y>
static constexpr X shr(const X& x, const Y& y) noexcept {
  typedef typename std::make_unsigned<X>::type U;
  return X(U(x) >> y);
}

//! Returns `x >> y` (shift right arithmetic) by explicitly casting `x` to a signed type and back.
template<typename X, typename Y>
static constexpr X sar(const X& x, const Y& y) noexcept {
  typedef typename std::make_signed<X>::type S;
  return X(S(x) >> y);
}

template<typename X, typename Y>
static constexpr X ror(const X& x, const Y& y) noexcept {
  typedef typename std::make_unsigned<X>::type U;
  return X((U(x) >> y) | (U(x) << (bitSizeOf<U>() - y)));
}

//! Returns `x | (x >> y)` - helper used by some bit manipulation helpers.
template<typename X, typename Y>
static constexpr X or_shr(const X& x, const Y& y) noexcept { return X(x | shr(x, y)); }

//! Returns `x & -x` - extracts lowest set isolated bit (like BLSI instruction).
template<typename T>
static constexpr T blsi(T x) noexcept {
  typedef typename std::make_unsigned<T>::type U;
  return T(U(x) & neg(U(x)));
}

//! Tests whether the given value `x` has `n`th bit set.
template<typename T, typename IndexT>
static constexpr bool bitTest(T x, IndexT n) noexcept {
  typedef typename std::make_unsigned<T>::type U;
  return (U(x) & (U(1) << asStdInt(n))) != 0;
}

// Tests whether the given `value` is a consecutive mask of bits that starts at
// the least significant bit.
template<typename T>
static inline constexpr bool isLsbMask(const T& value) {
  typedef typename std::make_unsigned<T>::type U;
  return value && ((U(value) + 1u) & U(value)) == 0;
}

// Tests whether the given value contains at least one bit or whether it's a
// bit-mask of consecutive bits.
//
// This function is similar to \ref isLsbMask(), but the mask doesn't have to
// start at a least significant bit.
template<typename T>
static inline constexpr bool isConsecutiveMask(const T& value) {
  typedef typename std::make_unsigned<T>::type U;
  return value && isLsbMask((U(value) - 1u) | U(value));
}

//! Generates a trailing bit-mask that has `n` least significant (trailing) bits set.
template<typename T, typename CountT>
static constexpr T lsbMask(const CountT& n) noexcept {
  typedef typename std::make_unsigned<T>::type U;
  return (sizeof(U) < sizeof(uintptr_t))
    // Prevent undefined behavior by using a larger type than T.
    ? T(U((uintptr_t(1) << n) - uintptr_t(1)))
    // Prevent undefined behavior by checking `n` before shift.
    : n ? T(shr(allOnes<T>(), bitSizeOf<T>() - size_t(n))) : T(0);
}

//! Generats a leading bit-mask that has `n` most significant (leading) bits set.
template<typename T, typename CountT>
static constexpr T msbMask(const CountT& n) noexcept {
  typedef typename std::make_unsigned<T>::type U;
  return (sizeof(U) < sizeof(uintptr_t))
    // Prevent undefined behavior by using a larger type than T.
    ? T(allOnes<uintptr_t>() >> (bitSizeOf<uintptr_t>() - n))
    // Prevent undefined behavior by performing `n & (nBits - 1)` so it's always within the range.
    : T(sar(U(n != 0) << (bitSizeOf<U>() - 1), n ? uint32_t(n - 1) : uint32_t(0)));
}

//! Returns a bit-mask that has `x` bit set.
template<typename Index>
static constexpr uint32_t bitMask(const Index& x) noexcept { return (1u << asUInt(x)); }

//! Returns a bit-mask that has `x` bit set (multiple arguments).
template<typename Index, typename... Args>
static constexpr uint32_t bitMask(const Index& x, Args... args) noexcept { return bitMask(x) | bitMask(args...); }

//! Converts a boolean value `b` to zero or full mask (all bits set).
template<typename DstT, typename SrcT>
static constexpr DstT bitMaskFromBool(SrcT b) noexcept {
  typedef typename std::make_unsigned<DstT>::type U;
  return DstT(U(0) - U(b));
}

//! Tests whether `a & b` is non-zero.
template<typename A, typename B>
static inline constexpr bool test(A a, B b) noexcept { return (asUInt(a) & asUInt(b)) != 0; }

//! \cond
namespace Internal {
  // Fills all trailing bits right from the first most significant bit set.
  static constexpr uint8_t fillTrailingBitsImpl(uint8_t x) noexcept { return or_shr(or_shr(or_shr(x, 1), 2), 4); }
  // Fills all trailing bits right from the first most significant bit set.
  static constexpr uint16_t fillTrailingBitsImpl(uint16_t x) noexcept { return or_shr(or_shr(or_shr(or_shr(x, 1), 2), 4), 8); }
  // Fills all trailing bits right from the first most significant bit set.
  static constexpr uint32_t fillTrailingBitsImpl(uint32_t x) noexcept { return or_shr(or_shr(or_shr(or_shr(or_shr(x, 1), 2), 4), 8), 16); }
  // Fills all trailing bits right from the first most significant bit set.
  static constexpr uint64_t fillTrailingBitsImpl(uint64_t x) noexcept { return or_shr(or_shr(or_shr(or_shr(or_shr(or_shr(x, 1), 2), 4), 8), 16), 32); }
}
//! \endcond

// Fills all trailing bits right from the first most significant bit set.
template<typename T>
static constexpr T fillTrailingBits(const T& x) noexcept {
  typedef typename std::make_unsigned<T>::type U;
  return T(Internal::fillTrailingBitsImpl(U(x)));
}

// Support - Count Leading/Trailing Zeros
// ======================================

//! \cond
namespace Internal {
namespace {

template<typename T>
struct BitScanData { T x; uint32_t n; };

template<typename T, uint32_t N>
struct BitScanCalc {
  static constexpr BitScanData<T> advanceLeft(const BitScanData<T>& data, uint32_t n) noexcept {
    return BitScanData<T> { data.x << n, data.n + n };
  }

  static constexpr BitScanData<T> advanceRight(const BitScanData<T>& data, uint32_t n) noexcept {
    return BitScanData<T> { data.x >> n, data.n + n };
  }

  static constexpr BitScanData<T> clz(const BitScanData<T>& data) noexcept {
    return BitScanCalc<T, N / 2>::clz(advanceLeft(data, data.x & (allOnes<T>() << (bitSizeOf<T>() - N)) ? uint32_t(0) : N));
  }

  static constexpr BitScanData<T> ctz(const BitScanData<T>& data) noexcept {
    return BitScanCalc<T, N / 2>::ctz(advanceRight(data, data.x & (allOnes<T>() >> (bitSizeOf<T>() - N)) ? uint32_t(0) : N));
  }
};

template<typename T>
struct BitScanCalc<T, 0> {
  static constexpr BitScanData<T> clz(const BitScanData<T>& ctx) noexcept {
    return BitScanData<T> { 0, ctx.n - uint32_t(ctx.x >> (bitSizeOf<T>() - 1)) };
  }

  static constexpr BitScanData<T> ctz(const BitScanData<T>& ctx) noexcept {
    return BitScanData<T> { 0, ctx.n - uint32_t(ctx.x & 0x1) };
  }
};

template<typename T>
constexpr uint32_t clzFallback(const T& x) noexcept {
  return BitScanCalc<T, bitSizeOf<T>() / 2u>::clz(BitScanData<T>{x, 1}).n;
}

template<typename T>
constexpr uint32_t ctzFallback(const T& x) noexcept {
  return BitScanCalc<T, bitSizeOf<T>() / 2u>::ctz(BitScanData<T>{x, 1}).n;
}

template<typename T> inline uint32_t clzImpl(const T& x) noexcept { return clzFallback(asUInt(x)); }
template<typename T> inline uint32_t ctzImpl(const T& x) noexcept { return ctzFallback(asUInt(x)); }

#if !defined(ASMJIT_NO_INTRINSICS)
# if defined(__GNUC__)
template<> inline uint32_t clzImpl(const uint32_t& x) noexcept { return uint32_t(__builtin_clz(x)); }
template<> inline uint32_t clzImpl(const uint64_t& x) noexcept { return uint32_t(__builtin_clzll(x)); }
template<> inline uint32_t ctzImpl(const uint32_t& x) noexcept { return uint32_t(__builtin_ctz(x)); }
template<> inline uint32_t ctzImpl(const uint64_t& x) noexcept { return uint32_t(__builtin_ctzll(x)); }
# elif defined(_MSC_VER)
template<> inline uint32_t clzImpl(const uint32_t& x) noexcept { unsigned long i; _BitScanReverse(&i, x); return uint32_t(i ^ 31); }
template<> inline uint32_t ctzImpl(const uint32_t& x) noexcept { unsigned long i; _BitScanForward(&i, x); return uint32_t(i); }
#  if ASMJIT_ARCH_X86 == 64 || ASMJIT_ARCH_ARM == 64
template<> inline uint32_t clzImpl(const uint64_t& x) noexcept { unsigned long i; _BitScanReverse64(&i, x); return uint32_t(i ^ 63); }
template<> inline uint32_t ctzImpl(const uint64_t& x) noexcept { unsigned long i; _BitScanForward64(&i, x); return uint32_t(i); }
#  endif
# endif
#endif

} // {anonymous}
} // {Internal}
//! \endcond

//! Count leading zeros in `x` (returns a position of a first bit set in `x`).
//!
//! \note The input MUST NOT be zero, otherwise the result is undefined.
template<typename T>
static inline uint32_t clz(T x) noexcept { return Internal::clzImpl(asUInt(x)); }

//! Count trailing zeros in `x` (returns a position of a first bit set in `x`).
//!
//! \note The input MUST NOT be zero, otherwise the result is undefined.
template<typename T>
static inline uint32_t ctz(T x) noexcept { return Internal::ctzImpl(asUInt(x)); }

template<uint64_t kInput>
struct ConstCTZ {
  static constexpr uint32_t value =
    (kInput & (uint64_t(1) <<  0)) ?  0 :
    (kInput & (uint64_t(1) <<  1)) ?  1 :
    (kInput & (uint64_t(1) <<  2)) ?  2 :
    (kInput & (uint64_t(1) <<  3)) ?  3 :
    (kInput & (uint64_t(1) <<  4)) ?  4 :
    (kInput & (uint64_t(1) <<  5)) ?  5 :
    (kInput & (uint64_t(1) <<  6)) ?  6 :
    (kInput & (uint64_t(1) <<  7)) ?  7 :
    (kInput & (uint64_t(1) <<  8)) ?  8 :
    (kInput & (uint64_t(1) <<  9)) ?  9 :
    (kInput & (uint64_t(1) << 10)) ? 10 :
    (kInput & (uint64_t(1) << 11)) ? 11 :
    (kInput & (uint64_t(1) << 12)) ? 12 :
    (kInput & (uint64_t(1) << 13)) ? 13 :
    (kInput & (uint64_t(1) << 14)) ? 14 :
    (kInput & (uint64_t(1) << 15)) ? 15 :
    (kInput & (uint64_t(1) << 16)) ? 16 :
    (kInput & (uint64_t(1) << 17)) ? 17 :
    (kInput & (uint64_t(1) << 18)) ? 18 :
    (kInput & (uint64_t(1) << 19)) ? 19 :
    (kInput & (uint64_t(1) << 20)) ? 20 :
    (kInput & (uint64_t(1) << 21)) ? 21 :
    (kInput & (uint64_t(1) << 22)) ? 22 :
    (kInput & (uint64_t(1) << 23)) ? 23 :
    (kInput & (uint64_t(1) << 24)) ? 24 :
    (kInput & (uint64_t(1) << 25)) ? 25 :
    (kInput & (uint64_t(1) << 26)) ? 26 :
    (kInput & (uint64_t(1) << 27)) ? 27 :
    (kInput & (uint64_t(1) << 28)) ? 28 :
    (kInput & (uint64_t(1) << 29)) ? 29 :
    (kInput & (uint64_t(1) << 30)) ? 30 :
    (kInput & (uint64_t(1) << 31)) ? 31 :
    (kInput & (uint64_t(1) << 32)) ? 32 :
    (kInput & (uint64_t(1) << 33)) ? 33 :
    (kInput & (uint64_t(1) << 34)) ? 34 :
    (kInput & (uint64_t(1) << 35)) ? 35 :
    (kInput & (uint64_t(1) << 36)) ? 36 :
    (kInput & (uint64_t(1) << 37)) ? 37 :
    (kInput & (uint64_t(1) << 38)) ? 38 :
    (kInput & (uint64_t(1) << 39)) ? 39 :
    (kInput & (uint64_t(1) << 40)) ? 40 :
    (kInput & (uint64_t(1) << 41)) ? 41 :
    (kInput & (uint64_t(1) << 42)) ? 42 :
    (kInput & (uint64_t(1) << 43)) ? 43 :
    (kInput & (uint64_t(1) << 44)) ? 44 :
    (kInput & (uint64_t(1) << 45)) ? 45 :
    (kInput & (uint64_t(1) << 46)) ? 46 :
    (kInput & (uint64_t(1) << 47)) ? 47 :
    (kInput & (uint64_t(1) << 48)) ? 48 :
    (kInput & (uint64_t(1) << 49)) ? 49 :
    (kInput & (uint64_t(1) << 50)) ? 50 :
    (kInput & (uint64_t(1) << 51)) ? 51 :
    (kInput & (uint64_t(1) << 52)) ? 52 :
    (kInput & (uint64_t(1) << 53)) ? 53 :
    (kInput & (uint64_t(1) << 54)) ? 54 :
    (kInput & (uint64_t(1) << 55)) ? 55 :
    (kInput & (uint64_t(1) << 56)) ? 56 :
    (kInput & (uint64_t(1) << 57)) ? 57 :
    (kInput & (uint64_t(1) << 58)) ? 58 :
    (kInput & (uint64_t(1) << 59)) ? 59 :
    (kInput & (uint64_t(1) << 60)) ? 60 :
    (kInput & (uint64_t(1) << 61)) ? 61 :
    (kInput & (uint64_t(1) << 62)) ? 62 :
    (kInput & (uint64_t(1) << 63)) ? 63 : 64;
};

// Support - PopCnt
// ================

// Based on the following resource:
//   http://graphics.stanford.edu/~seander/bithacks.html
//
// Alternatively, for a very small number of bits in `x`:
//   uint32_t n = 0;
//   while (x) {
//     x &= x - 1;
//     n++;
//   }
//   return n;

//! \cond
namespace Internal {
  static inline uint32_t constPopcntImpl(uint32_t x) noexcept {
    x = x - ((x >> 1) & 0x55555555u);
    x = (x & 0x33333333u) + ((x >> 2) & 0x33333333u);
    return (((x + (x >> 4)) & 0x0F0F0F0Fu) * 0x01010101u) >> 24;
  }

  static inline uint32_t constPopcntImpl(uint64_t x) noexcept {
    if (ASMJIT_ARCH_BITS >= 64) {
      x = x - ((x >> 1) & 0x5555555555555555u);
      x = (x & 0x3333333333333333u) + ((x >> 2) & 0x3333333333333333u);
      return uint32_t((((x + (x >> 4)) & 0x0F0F0F0F0F0F0F0Fu) * 0x0101010101010101u) >> 56);
    }
    else {
      return constPopcntImpl(uint32_t(x >> 32)) +
             constPopcntImpl(uint32_t(x & 0xFFFFFFFFu));
    }
  }

  static inline uint32_t popcntImpl(uint32_t x) noexcept {
  #if defined(__GNUC__)
    return uint32_t(__builtin_popcount(x));
  #else
    return constPopcntImpl(asUInt(x));
  #endif
  }

  static inline uint32_t popcntImpl(uint64_t x) noexcept {
  #if defined(__GNUC__)
    return uint32_t(__builtin_popcountll(x));
  #else
    return constPopcntImpl(asUInt(x));
  #endif
  }
}
//! \endcond

//! Calculates count of bits in `x`.
template<typename T>
static inline uint32_t popcnt(T x) noexcept { return Internal::popcntImpl(asUInt(x)); }

//! Calculates count of bits in `x` (useful in constant expressions).
template<typename T>
static inline uint32_t constPopcnt(T x) noexcept { return Internal::constPopcntImpl(asUInt(x)); }

// Support - Min/Max
// =================

// NOTE: These are constexpr `min()` and `max()` implementations that are not
// exactly the same as `std::min()` and `std::max()`. The return value is not
// a reference to `a` or `b` but it's a new value instead.

template<typename T>
static constexpr T min(const T& a, const T& b) noexcept { return b < a ? b : a; }

template<typename T, typename... Args>
static constexpr T min(const T& a, const T& b, Args&&... args) noexcept { return min(min(a, b), std::forward<Args>(args)...); }

template<typename T>
static constexpr T max(const T& a, const T& b) noexcept { return a < b ? b : a; }

template<typename T, typename... Args>
static constexpr T max(const T& a, const T& b, Args&&... args) noexcept { return max(max(a, b), std::forward<Args>(args)...); }

// Support - Immediate Helpers
// ===========================

namespace Internal {
  template<typename T, bool IsFloat>
  struct ImmConv {
    static inline int64_t fromT(const T& x) noexcept { return int64_t(x); }
    static inline T toT(int64_t x) noexcept { return T(uint64_t(x) & Support::allOnes<typename std::make_unsigned<T>::type>()); }
  };

  template<typename T>
  struct ImmConv<T, true> {
    static inline int64_t fromT(const T& x) noexcept { return int64_t(bitCast<int64_t>(double(x))); }
    static inline T toT(int64_t x) noexcept { return T(bitCast<double>(x)); }
  };
}

template<typename T>
static inline int64_t immediateFromT(const T& x) noexcept { return Internal::ImmConv<T, std::is_floating_point<T>::value>::fromT(x); }

template<typename T>
static inline T immediateToT(int64_t x) noexcept { return Internal::ImmConv<T, std::is_floating_point<T>::value>::toT(x); }

// Support - Overflow Arithmetic
// =============================

//! \cond
namespace Internal {
  template<typename T>
  inline T addOverflowFallback(T x, T y, FastUInt8* of) noexcept {
    typedef typename std::make_unsigned<T>::type U;

    U result = U(x) + U(y);
    *of = FastUInt8(*of | FastUInt8(isUnsigned<T>() ? result < U(x) : T((U(x) ^ ~U(y)) & (U(x) ^ result)) < 0));
    return T(result);
  }

  template<typename T>
  inline T subOverflowFallback(T x, T y, FastUInt8* of) noexcept {
    typedef typename std::make_unsigned<T>::type U;

    U result = U(x) - U(y);
    *of = FastUInt8(*of | FastUInt8(isUnsigned<T>() ? result > U(x) : T((U(x) ^ U(y)) & (U(x) ^ result)) < 0));
    return T(result);
  }

  template<typename T>
  inline T mulOverflowFallback(T x, T y, FastUInt8* of) noexcept {
    typedef typename Internal::StdInt<sizeof(T) * 2, isUnsigned<T>()>::Type I;
    typedef typename std::make_unsigned<I>::type U;

    U mask = allOnes<U>();
    if (std::is_signed<T>::value) {
      U prod = U(I(x)) * U(I(y));
      *of = FastUInt8(*of | FastUInt8(I(prod) < I(std::numeric_limits<T>::lowest()) || I(prod) > I(std::numeric_limits<T>::max())));
      return T(I(prod & mask));
    }
    else {
      U prod = U(x) * U(y);
      *of = FastUInt8(*of | FastUInt8((prod & ~mask) != 0));
      return T(prod & mask);
    }
  }

  template<>
  inline int64_t mulOverflowFallback(int64_t x, int64_t y, FastUInt8* of) noexcept {
    int64_t result = int64_t(uint64_t(x) * uint64_t(y));
    *of = FastUInt8(*of | FastUInt8(x && (result / x != y)));
    return result;
  }

  template<>
  inline uint64_t mulOverflowFallback(uint64_t x, uint64_t y, FastUInt8* of) noexcept {
    uint64_t result = x * y;
    *of = FastUInt8(*of | FastUInt8(y != 0 && allOnes<uint64_t>() / y < x));
    return result;
  }

  // These can be specialized.
  template<typename T> inline T addOverflowImpl(const T& x, const T& y, FastUInt8* of) noexcept { return addOverflowFallback(x, y, of); }
  template<typename T> inline T subOverflowImpl(const T& x, const T& y, FastUInt8* of) noexcept { return subOverflowFallback(x, y, of); }
  template<typename T> inline T mulOverflowImpl(const T& x, const T& y, FastUInt8* of) noexcept { return mulOverflowFallback(x, y, of); }

  #if defined(__GNUC__) && !defined(ASMJIT_NO_INTRINSICS)
  #if defined(__clang__) || __GNUC__ >= 5
  #define ASMJIT_ARITH_OVERFLOW_SPECIALIZE(FUNC, T, RESULT_T, BUILTIN)     \
    template<>                                                             \
    inline T FUNC(const T& x, const T& y, FastUInt8* of) noexcept {        \
      RESULT_T result;                                                     \
      *of = FastUInt8(*of | (BUILTIN((RESULT_T)x, (RESULT_T)y, &result))); \
      return T(result);                                                    \
    }
  ASMJIT_ARITH_OVERFLOW_SPECIALIZE(addOverflowImpl, int32_t , int               , __builtin_sadd_overflow  )
  ASMJIT_ARITH_OVERFLOW_SPECIALIZE(addOverflowImpl, uint32_t, unsigned int      , __builtin_uadd_overflow  )
  ASMJIT_ARITH_OVERFLOW_SPECIALIZE(addOverflowImpl, int64_t , long long         , __builtin_saddll_overflow)
  ASMJIT_ARITH_OVERFLOW_SPECIALIZE(addOverflowImpl, uint64_t, unsigned long long, __builtin_uaddll_overflow)
  ASMJIT_ARITH_OVERFLOW_SPECIALIZE(subOverflowImpl, int32_t , int               , __builtin_ssub_overflow  )
  ASMJIT_ARITH_OVERFLOW_SPECIALIZE(subOverflowImpl, uint32_t, unsigned int      , __builtin_usub_overflow  )
  ASMJIT_ARITH_OVERFLOW_SPECIALIZE(subOverflowImpl, int64_t , long long         , __builtin_ssubll_overflow)
  ASMJIT_ARITH_OVERFLOW_SPECIALIZE(subOverflowImpl, uint64_t, unsigned long long, __builtin_usubll_overflow)
  ASMJIT_ARITH_OVERFLOW_SPECIALIZE(mulOverflowImpl, int32_t , int               , __builtin_smul_overflow  )
  ASMJIT_ARITH_OVERFLOW_SPECIALIZE(mulOverflowImpl, uint32_t, unsigned int      , __builtin_umul_overflow  )
  ASMJIT_ARITH_OVERFLOW_SPECIALIZE(mulOverflowImpl, int64_t , long long         , __builtin_smulll_overflow)
  ASMJIT_ARITH_OVERFLOW_SPECIALIZE(mulOverflowImpl, uint64_t, unsigned long long, __builtin_umulll_overflow)
  #undef ASMJIT_ARITH_OVERFLOW_SPECIALIZE
  #endif
  #endif

  // There is a bug in MSVC that makes these specializations unusable, maybe in the future...
  #if defined(_MSC_VER) && 0
  #define ASMJIT_ARITH_OVERFLOW_SPECIALIZE(FUNC, T, ALT_T, BUILTIN)        \
    template<>                                                             \
    inline T FUNC(T x, T y, FastUInt8* of) noexcept {                      \
      ALT_T result;                                                        \
      *of = FastUInt8(*of | BUILTIN(0, (ALT_T)x, (ALT_T)y, &result));      \
      return T(result);                                                    \
    }
  ASMJIT_ARITH_OVERFLOW_SPECIALIZE(addOverflowImpl, uint32_t, unsigned int      , _addcarry_u32 )
  ASMJIT_ARITH_OVERFLOW_SPECIALIZE(subOverflowImpl, uint32_t, unsigned int      , _subborrow_u32)
  #if ARCH_BITS >= 64
  ASMJIT_ARITH_OVERFLOW_SPECIALIZE(addOverflowImpl, uint64_t, unsigned __int64  , _addcarry_u64 )
  ASMJIT_ARITH_OVERFLOW_SPECIALIZE(subOverflowImpl, uint64_t, unsigned __int64  , _subborrow_u64)
  #endif
  #undef ASMJIT_ARITH_OVERFLOW_SPECIALIZE
  #endif
} // {Internal}
//! \endcond

template<typename T>
static inline T addOverflow(const T& x, const T& y, FastUInt8* of) noexcept { return T(Internal::addOverflowImpl(asStdInt(x), asStdInt(y), of)); }

template<typename T>
static inline T subOverflow(const T& x, const T& y, FastUInt8* of) noexcept { return T(Internal::subOverflowImpl(asStdInt(x), asStdInt(y), of)); }

template<typename T>
static inline T mulOverflow(const T& x, const T& y, FastUInt8* of) noexcept { return T(Internal::mulOverflowImpl(asStdInt(x), asStdInt(y), of)); }

// Support - Alignment
// ===================

template<typename X, typename Y>
static constexpr bool isAligned(X base, Y alignment) noexcept {
  typedef typename Internal::StdInt<sizeof(X), 1>::Type U;
  return ((U)base % (U)alignment) == 0;
}

//! Tests whether the `x` is a power of two (only one bit is set).
template<typename T>
static constexpr bool isPowerOf2(T x) noexcept {
  typedef typename std::make_unsigned<T>::type U;
  return x && !(U(x) & (U(x) - U(1)));
}

template<typename X, typename Y>
static constexpr X alignUp(X x, Y alignment) noexcept {
  typedef typename Internal::StdInt<sizeof(X), 1>::Type U;
  return (X)( ((U)x + ((U)(alignment) - 1u)) & ~((U)(alignment) - 1u) );
}

template<typename T>
static constexpr T alignUpPowerOf2(T x) noexcept {
  typedef typename Internal::StdInt<sizeof(T), 1>::Type U;
  return (T)(fillTrailingBits(U(x) - 1u) + 1u);
}

//! Returns either zero or a positive difference between `base` and `base` when
//! aligned to `alignment`.
template<typename X, typename Y>
static constexpr typename Internal::StdInt<sizeof(X), 1>::Type alignUpDiff(X base, Y alignment) noexcept {
  typedef typename Internal::StdInt<sizeof(X), 1>::Type U;
  return alignUp(U(base), alignment) - U(base);
}

template<typename X, typename Y>
static constexpr X alignDown(X x, Y alignment) noexcept {
  typedef typename Internal::StdInt<sizeof(X), 1>::Type U;
  return (X)( (U)x & ~((U)(alignment) - 1u) );
}

// Support - NumGranularized
// =========================

//! Calculates the number of elements that would be required if `base` is
//! granularized by `granularity`. This function can be used to calculate
//! the number of BitWords to represent N bits, for example.
template<typename X, typename Y>
static constexpr X numGranularized(X base, Y granularity) noexcept {
  typedef typename Internal::StdInt<sizeof(X), 1>::Type U;
  return X((U(base) + U(granularity) - 1) / U(granularity));
}

// Support - IsBetween
// ===================

//! Checks whether `x` is greater than or equal to `a` and lesser than or equal to `b`.
template<typename T>
static constexpr bool isBetween(const T& x, const T& a, const T& b) noexcept {
  return x >= a && x <= b;
}

// Support - IsInt & IsUInt
// ========================

//! Checks whether the given integer `x` can be casted to a 4-bit signed integer.
template<typename T>
static constexpr bool isInt4(T x) noexcept {
  typedef typename std::make_signed<T>::type S;
  typedef typename std::make_unsigned<T>::type U;

  return std::is_signed<T>::value ? isBetween<S>(S(x), -8, 7) : U(x) <= U(7u);
}

//! Checks whether the given integer `x` can be casted to a 7-bit signed integer.
template<typename T>
static constexpr bool isInt7(T x) noexcept {
  typedef typename std::make_signed<T>::type S;
  typedef typename std::make_unsigned<T>::type U;

  return std::is_signed<T>::value ? isBetween<S>(S(x), -64, 63) : U(x) <= U(63u);
}

//! Checks whether the given integer `x` can be casted to an 8-bit signed integer.
template<typename T>
static constexpr bool isInt8(T x) noexcept {
  typedef typename std::make_signed<T>::type S;
  typedef typename std::make_unsigned<T>::type U;

  return std::is_signed<T>::value ? sizeof(T) <= 1 || isBetween<S>(S(x), -128, 127) : U(x) <= U(127u);
}

//! Checks whether the given integer `x` can be casted to a 9-bit signed integer.
template<typename T>
static constexpr bool isInt9(T x) noexcept {
  typedef typename std::make_signed<T>::type S;
  typedef typename std::make_unsigned<T>::type U;

  return std::is_signed<T>::value ? sizeof(T) <= 1 || isBetween<S>(S(x), -256, 255)
                                  : sizeof(T) <= 1 || U(x) <= U(255u);
}

//! Checks whether the given integer `x` can be casted to a 10-bit signed integer.
template<typename T>
static constexpr bool isInt10(T x) noexcept {
  typedef typename std::make_signed<T>::type S;
  typedef typename std::make_unsigned<T>::type U;

  return std::is_signed<T>::value ? sizeof(T) <= 1 || isBetween<S>(S(x), -512, 511)
                                  : sizeof(T) <= 1 || U(x) <= U(511u);
}

//! Checks whether the given integer `x` can be casted to a 16-bit signed integer.
template<typename T>
static constexpr bool isInt16(T x) noexcept {
  typedef typename std::make_signed<T>::type S;
  typedef typename std::make_unsigned<T>::type U;

  return std::is_signed<T>::value ? sizeof(T) <= 2 || isBetween<S>(S(x), -32768, 32767)
                                  : sizeof(T) <= 1 || U(x) <= U(32767u);
}

//! Checks whether the given integer `x` can be casted to a 32-bit signed integer.
template<typename T>
static constexpr bool isInt32(T x) noexcept {
  typedef typename std::make_signed<T>::type S;
  typedef typename std::make_unsigned<T>::type U;

  return std::is_signed<T>::value ? sizeof(T) <= 4 || isBetween<S>(S(x), -2147483647 - 1, 2147483647)
                                  : sizeof(T) <= 2 || U(x) <= U(2147483647u);
}

//! Checks whether the given integer `x` can be casted to a 4-bit unsigned integer.
template<typename T>
static constexpr bool isUInt4(T x) noexcept {
  typedef typename std::make_unsigned<T>::type U;

  return std::is_signed<T>::value ? x >= T(0) && x <= T(15)
                                  : U(x) <= U(15u);
}

//! Checks whether the given integer `x` can be casted to an 8-bit unsigned integer.
template<typename T>
static constexpr bool isUInt8(T x) noexcept {
  typedef typename std::make_unsigned<T>::type U;

  return std::is_signed<T>::value ? (sizeof(T) <= 1 || T(x) <= T(255)) && x >= T(0)
                                  : (sizeof(T) <= 1 || U(x) <= U(255u));
}

//! Checks whether the given integer `x` can be casted to a 12-bit unsigned integer (ARM specific).
template<typename T>
static constexpr bool isUInt12(T x) noexcept {
  typedef typename std::make_unsigned<T>::type U;

  return std::is_signed<T>::value ? (sizeof(T) <= 1 || T(x) <= T(4095)) && x >= T(0)
                                  : (sizeof(T) <= 1 || U(x) <= U(4095u));
}

//! Checks whether the given integer `x` can be casted to a 16-bit unsigned integer.
template<typename T>
static constexpr bool isUInt16(T x) noexcept {
  typedef typename std::make_unsigned<T>::type U;

  return std::is_signed<T>::value ? (sizeof(T) <= 2 || T(x) <= T(65535)) && x >= T(0)
                                  : (sizeof(T) <= 2 || U(x) <= U(65535u));
}

//! Checks whether the given integer `x` can be casted to a 32-bit unsigned integer.
template<typename T>
static constexpr bool isUInt32(T x) noexcept {
  typedef typename std::make_unsigned<T>::type U;

  return std::is_signed<T>::value ? (sizeof(T) <= 4 || T(x) <= T(4294967295u)) && x >= T(0)
                                  : (sizeof(T) <= 4 || U(x) <= U(4294967295u));
}

//! Checks whether the given integer `x` can be casted to a 32-bit unsigned integer.
template<typename T>
static constexpr bool isIntOrUInt32(T x) noexcept {
  return sizeof(T) <= 4 ? true : (uint32_t(uint64_t(x) >> 32) + 1u) <= 1u;
}

static bool inline isEncodableOffset32(int32_t offset, uint32_t nBits) noexcept {
  uint32_t nRev = 32 - nBits;
  return Support::sar(Support::shl(offset, nRev), nRev) == offset;
}

static bool inline isEncodableOffset64(int64_t offset, uint32_t nBits) noexcept {
  uint32_t nRev = 64 - nBits;
  return Support::sar(Support::shl(offset, nRev), nRev) == offset;
}

// Support - ByteSwap
// ==================

static inline uint16_t byteswap16(uint16_t x) noexcept {
  return uint16_t(((x >> 8) & 0xFFu) | ((x & 0xFFu) << 8));
}

static inline uint32_t byteswap32(uint32_t x) noexcept {
  return (x << 24) | (x >> 24) | ((x << 8) & 0x00FF0000u) | ((x >> 8) & 0x0000FF00);
}

static inline uint64_t byteswap64(uint64_t x) noexcept {
#if (defined(__GNUC__) || defined(__clang__)) && !defined(ASMJIT_NO_INTRINSICS)
  return uint64_t(__builtin_bswap64(uint64_t(x)));
#elif defined(_MSC_VER) && !defined(ASMJIT_NO_INTRINSICS)
  return uint64_t(_byteswap_uint64(uint64_t(x)));
#else
  return (uint64_t(byteswap32(uint32_t(uint64_t(x) >> 32        )))      ) |
         (uint64_t(byteswap32(uint32_t(uint64_t(x) & 0xFFFFFFFFu))) << 32) ;
#endif
}

// Support - BytePack & Unpack
// ===========================

//! Pack four 8-bit integer into a 32-bit integer as it is an array of `{b0,b1,b2,b3}`.
static constexpr uint32_t bytepack32_4x8(uint32_t a, uint32_t b, uint32_t c, uint32_t d) noexcept {
  return ASMJIT_ARCH_LE ? (a | (b << 8) | (c << 16) | (d << 24))
                        : (d | (c << 8) | (b << 16) | (a << 24));
}

template<typename T>
static constexpr uint32_t unpackU32At0(T x) noexcept { return ASMJIT_ARCH_LE ? uint32_t(uint64_t(x) & 0xFFFFFFFFu) : uint32_t(uint64_t(x) >> 32); }
template<typename T>
static constexpr uint32_t unpackU32At1(T x) noexcept { return ASMJIT_ARCH_BE ? uint32_t(uint64_t(x) & 0xFFFFFFFFu) : uint32_t(uint64_t(x) >> 32); }

// Support - Position of byte (in bit-shift)
// =========================================

static inline uint32_t byteShiftOfDWordStruct(uint32_t index) noexcept {
  return ASMJIT_ARCH_LE ? index * 8 : (uint32_t(sizeof(uint32_t)) - 1u - index) * 8;
}

// Support - String Utilities
// ==========================

template<typename T>
static constexpr T asciiToLower(T c) noexcept { return T(c ^ T(T(c >= T('A') && c <= T('Z')) << 5)); }

template<typename T>
static constexpr T asciiToUpper(T c) noexcept { return T(c ^ T(T(c >= T('a') && c <= T('z')) << 5)); }

static ASMJIT_FORCE_INLINE size_t strLen(const char* s, size_t maxSize) noexcept {
  size_t i = 0;
  while (i < maxSize && s[i] != '\0')
    i++;
  return i;
}

static constexpr uint32_t hashRound(uint32_t hash, uint32_t c) noexcept { return hash * 65599 + c; }

// Gets a hash of the given string `data` of size `size`. Size must be valid
// as this function doesn't check for a null terminator and allows it in the
// middle of the string.
static inline uint32_t hashString(const char* data, size_t size) noexcept {
  uint32_t hashCode = 0;
  for (uint32_t i = 0; i < size; i++)
    hashCode = hashRound(hashCode, uint8_t(data[i]));
  return hashCode;
}

static ASMJIT_FORCE_INLINE const char* findPackedString(const char* p, uint32_t id) noexcept {
  uint32_t i = 0;
  while (i < id) {
    while (p[0])
      p++;
    p++;
    i++;
  }
  return p;
}

//! Compares two instruction names.
//!
//! `a` is a null terminated instruction name from arch-specific `nameData[]`
//! table. `b` is a possibly non-null terminated instruction name passed to
//! `InstAPI::stringToInstId()`.
static ASMJIT_FORCE_INLINE int cmpInstName(const char* a, const char* b, size_t size) noexcept {
  for (size_t i = 0; i < size; i++) {
    int c = int(uint8_t(a[i])) - int(uint8_t(b[i]));
    if (c != 0) return c;
  }
  return int(uint8_t(a[size]));
}

// Support - Memory Read Access - 8 Bits
// =====================================

static inline uint8_t readU8(const void* p) noexcept { return static_cast<const uint8_t*>(p)[0]; }
static inline int8_t readI8(const void* p) noexcept { return static_cast<const int8_t*>(p)[0]; }

// Support - Memory Read Access - 16 Bits
// ======================================

template<ByteOrder BO, size_t Alignment>
static inline uint16_t readU16x(const void* p) noexcept {
  typedef typename Internal::AliasedUInt<uint16_t, Alignment>::T U16AlignedToN;
  uint16_t x = static_cast<const U16AlignedToN*>(p)[0];
  return BO == ByteOrder::kNative ? x : byteswap16(x);
}

template<size_t Alignment = 1>
static inline uint16_t readU16u(const void* p) noexcept { return readU16x<ByteOrder::kNative, Alignment>(p); }
template<size_t Alignment = 1>
static inline uint16_t readU16uLE(const void* p) noexcept { return readU16x<ByteOrder::kLE, Alignment>(p); }
template<size_t Alignment = 1>
static inline uint16_t readU16uBE(const void* p) noexcept { return readU16x<ByteOrder::kBE, Alignment>(p); }

static inline uint16_t readU16a(const void* p) noexcept { return readU16x<ByteOrder::kNative, 2>(p); }
static inline uint16_t readU16aLE(const void* p) noexcept { return readU16x<ByteOrder::kLE, 2>(p); }
static inline uint16_t readU16aBE(const void* p) noexcept { return readU16x<ByteOrder::kBE, 2>(p); }

template<ByteOrder BO, size_t Alignment>
static inline int16_t readI16x(const void* p) noexcept { return int16_t(readU16x<BO, Alignment>(p)); }

template<size_t Alignment = 1>
static inline int16_t readI16u(const void* p) noexcept { return int16_t(readU16x<ByteOrder::kNative, Alignment>(p)); }
template<size_t Alignment = 1>
static inline int16_t readI16uLE(const void* p) noexcept { return int16_t(readU16x<ByteOrder::kLE, Alignment>(p)); }
template<size_t Alignment = 1>
static inline int16_t readI16uBE(const void* p) noexcept { return int16_t(readU16x<ByteOrder::kBE, Alignment>(p)); }

static inline int16_t readI16a(const void* p) noexcept { return int16_t(readU16x<ByteOrder::kNative, 2>(p)); }
static inline int16_t readI16aLE(const void* p) noexcept { return int16_t(readU16x<ByteOrder::kLE, 2>(p)); }
static inline int16_t readI16aBE(const void* p) noexcept { return int16_t(readU16x<ByteOrder::kBE, 2>(p)); }

// Support - Memory Read Access - 24 Bits
// ======================================

template<ByteOrder BO = ByteOrder::kNative>
static inline uint32_t readU24u(const void* p) noexcept {
  uint32_t b0 = readU8(static_cast<const uint8_t*>(p) + (BO == ByteOrder::kLE ? 2 : 0));
  uint32_t b1 = readU8(static_cast<const uint8_t*>(p) + (BO == ByteOrder::kLE ? 1 : 1));
  uint32_t b2 = readU8(static_cast<const uint8_t*>(p) + (BO == ByteOrder::kLE ? 0 : 2));
  return (b0 << 16) | (b1 << 8) | b2;
}

static inline uint32_t readU24uLE(const void* p) noexcept { return readU24u<ByteOrder::kLE>(p); }
static inline uint32_t readU24uBE(const void* p) noexcept { return readU24u<ByteOrder::kBE>(p); }

// Support - Memory Read Access - 32 Bits
// ======================================

template<ByteOrder BO, size_t Alignment>
static inline uint32_t readU32x(const void* p) noexcept {
  typedef typename Internal::AliasedUInt<uint32_t, Alignment>::T U32AlignedToN;
  uint32_t x = static_cast<const U32AlignedToN*>(p)[0];
  return BO == ByteOrder::kNative ? x : byteswap32(x);
}

template<size_t Alignment = 1>
static inline uint32_t readU32u(const void* p) noexcept { return readU32x<ByteOrder::kNative, Alignment>(p); }
template<size_t Alignment = 1>
static inline uint32_t readU32uLE(const void* p) noexcept { return readU32x<ByteOrder::kLE, Alignment>(p); }
template<size_t Alignment = 1>
static inline uint32_t readU32uBE(const void* p) noexcept { return readU32x<ByteOrder::kBE, Alignment>(p); }

static inline uint32_t readU32a(const void* p) noexcept { return readU32x<ByteOrder::kNative, 4>(p); }
static inline uint32_t readU32aLE(const void* p) noexcept { return readU32x<ByteOrder::kLE, 4>(p); }
static inline uint32_t readU32aBE(const void* p) noexcept { return readU32x<ByteOrder::kBE, 4>(p); }

template<ByteOrder BO, size_t Alignment>
static inline uint32_t readI32x(const void* p) noexcept { return int32_t(readU32x<BO, Alignment>(p)); }

template<size_t Alignment = 1>
static inline int32_t readI32u(const void* p) noexcept { return int32_t(readU32x<ByteOrder::kNative, Alignment>(p)); }
template<size_t Alignment = 1>
static inline int32_t readI32uLE(const void* p) noexcept { return int32_t(readU32x<ByteOrder::kLE, Alignment>(p)); }
template<size_t Alignment = 1>
static inline int32_t readI32uBE(const void* p) noexcept { return int32_t(readU32x<ByteOrder::kBE, Alignment>(p)); }

static inline int32_t readI32a(const void* p) noexcept { return int32_t(readU32x<ByteOrder::kNative, 4>(p)); }
static inline int32_t readI32aLE(const void* p) noexcept { return int32_t(readU32x<ByteOrder::kLE, 4>(p)); }
static inline int32_t readI32aBE(const void* p) noexcept { return int32_t(readU32x<ByteOrder::kBE, 4>(p)); }

// Support - Memory Read Access - 64 Bits
// ======================================

template<ByteOrder BO, size_t Alignment>
static inline uint64_t readU64x(const void* p) noexcept {
  typedef typename Internal::AliasedUInt<uint64_t, Alignment>::T U64AlignedToN;
  uint64_t x = static_cast<const U64AlignedToN*>(p)[0];
  return BO == ByteOrder::kNative ? x : byteswap64(x);
}

template<size_t Alignment = 1>
static inline uint64_t readU64u(const void* p) noexcept { return readU64x<ByteOrder::kNative, Alignment>(p); }
template<size_t Alignment = 1>
static inline uint64_t readU64uLE(const void* p) noexcept { return readU64x<ByteOrder::kLE, Alignment>(p); }
template<size_t Alignment = 1>
static inline uint64_t readU64uBE(const void* p) noexcept { return readU64x<ByteOrder::kBE, Alignment>(p); }

static inline uint64_t readU64a(const void* p) noexcept { return readU64x<ByteOrder::kNative, 8>(p); }
static inline uint64_t readU64aLE(const void* p) noexcept { return readU64x<ByteOrder::kLE, 8>(p); }
static inline uint64_t readU64aBE(const void* p) noexcept { return readU64x<ByteOrder::kBE, 8>(p); }

template<ByteOrder BO, size_t Alignment>
static inline int64_t readI64x(const void* p) noexcept { return int64_t(readU64x<BO, Alignment>(p)); }

template<size_t Alignment = 1>
static inline int64_t readI64u(const void* p) noexcept { return int64_t(readU64x<ByteOrder::kNative, Alignment>(p)); }
template<size_t Alignment = 1>
static inline int64_t readI64uLE(const void* p) noexcept { return int64_t(readU64x<ByteOrder::kLE, Alignment>(p)); }
template<size_t Alignment = 1>
static inline int64_t readI64uBE(const void* p) noexcept { return int64_t(readU64x<ByteOrder::kBE, Alignment>(p)); }

static inline int64_t readI64a(const void* p) noexcept { return int64_t(readU64x<ByteOrder::kNative, 8>(p)); }
static inline int64_t readI64aLE(const void* p) noexcept { return int64_t(readU64x<ByteOrder::kLE, 8>(p)); }
static inline int64_t readI64aBE(const void* p) noexcept { return int64_t(readU64x<ByteOrder::kBE, 8>(p)); }

// Support - Memory Write Access - 8 Bits
// ======================================

static inline void writeU8(void* p, uint8_t x) noexcept { static_cast<uint8_t*>(p)[0] = x; }
static inline void writeI8(void* p, int8_t x) noexcept { static_cast<int8_t*>(p)[0] = x; }

// Support - Memory Write Access - 16 Bits
// =======================================

template<ByteOrder BO = ByteOrder::kNative, size_t Alignment = 1>
static inline void writeU16x(void* p, uint16_t x) noexcept {
  typedef typename Internal::AliasedUInt<uint16_t, Alignment>::T U16AlignedToN;
  static_cast<U16AlignedToN*>(p)[0] = BO == ByteOrder::kNative ? x : byteswap16(x);
}

template<size_t Alignment = 1>
static inline void writeU16uLE(void* p, uint16_t x) noexcept { writeU16x<ByteOrder::kLE, Alignment>(p, x); }
template<size_t Alignment = 1>
static inline void writeU16uBE(void* p, uint16_t x) noexcept { writeU16x<ByteOrder::kBE, Alignment>(p, x); }

static inline void writeU16a(void* p, uint16_t x) noexcept { writeU16x<ByteOrder::kNative, 2>(p, x); }
static inline void writeU16aLE(void* p, uint16_t x) noexcept { writeU16x<ByteOrder::kLE, 2>(p, x); }
static inline void writeU16aBE(void* p, uint16_t x) noexcept { writeU16x<ByteOrder::kBE, 2>(p, x); }


template<ByteOrder BO = ByteOrder::kNative, size_t Alignment = 1>
static inline void writeI16x(void* p, int16_t x) noexcept { writeU16x<BO, Alignment>(p, uint16_t(x)); }

template<size_t Alignment = 1>
static inline void writeI16uLE(void* p, int16_t x) noexcept { writeU16x<ByteOrder::kLE, Alignment>(p, uint16_t(x)); }
template<size_t Alignment = 1>
static inline void writeI16uBE(void* p, int16_t x) noexcept { writeU16x<ByteOrder::kBE, Alignment>(p, uint16_t(x)); }

static inline void writeI16a(void* p, int16_t x) noexcept { writeU16x<ByteOrder::kNative, 2>(p, uint16_t(x)); }
static inline void writeI16aLE(void* p, int16_t x) noexcept { writeU16x<ByteOrder::kLE, 2>(p, uint16_t(x)); }
static inline void writeI16aBE(void* p, int16_t x) noexcept { writeU16x<ByteOrder::kBE, 2>(p, uint16_t(x)); }

// Support - Memory Write Access - 24 Bits
// =======================================

template<ByteOrder BO = ByteOrder::kNative>
static inline void writeU24u(void* p, uint32_t v) noexcept {
  static_cast<uint8_t*>(p)[0] = uint8_t((v >> (BO == ByteOrder::kLE ?  0 : 16)) & 0xFFu);
  static_cast<uint8_t*>(p)[1] = uint8_t((v >> (BO == ByteOrder::kLE ?  8 :  8)) & 0xFFu);
  static_cast<uint8_t*>(p)[2] = uint8_t((v >> (BO == ByteOrder::kLE ? 16 :  0)) & 0xFFu);
}

static inline void writeU24uLE(void* p, uint32_t v) noexcept { writeU24u<ByteOrder::kLE>(p, v); }
static inline void writeU24uBE(void* p, uint32_t v) noexcept { writeU24u<ByteOrder::kBE>(p, v); }

// Support - Memory Write Access - 32 Bits
// =======================================

template<ByteOrder BO = ByteOrder::kNative, size_t Alignment = 1>
static inline void writeU32x(void* p, uint32_t x) noexcept {
  typedef typename Internal::AliasedUInt<uint32_t, Alignment>::T U32AlignedToN;
  static_cast<U32AlignedToN*>(p)[0] = (BO == ByteOrder::kNative) ? x : Support::byteswap32(x);
}

template<size_t Alignment = 1>
static inline void writeU32u(void* p, uint32_t x) noexcept { writeU32x<ByteOrder::kNative, Alignment>(p, x); }
template<size_t Alignment = 1>
static inline void writeU32uLE(void* p, uint32_t x) noexcept { writeU32x<ByteOrder::kLE, Alignment>(p, x); }
template<size_t Alignment = 1>
static inline void writeU32uBE(void* p, uint32_t x) noexcept { writeU32x<ByteOrder::kBE, Alignment>(p, x); }

static inline void writeU32a(void* p, uint32_t x) noexcept { writeU32x<ByteOrder::kNative, 4>(p, x); }
static inline void writeU32aLE(void* p, uint32_t x) noexcept { writeU32x<ByteOrder::kLE, 4>(p, x); }
static inline void writeU32aBE(void* p, uint32_t x) noexcept { writeU32x<ByteOrder::kBE, 4>(p, x); }

template<ByteOrder BO = ByteOrder::kNative, size_t Alignment = 1>
static inline void writeI32x(void* p, int32_t x) noexcept { writeU32x<BO, Alignment>(p, uint32_t(x)); }

template<size_t Alignment = 1>
static inline void writeI32u(void* p, int32_t x) noexcept { writeU32x<ByteOrder::kNative, Alignment>(p, uint32_t(x)); }
template<size_t Alignment = 1>
static inline void writeI32uLE(void* p, int32_t x) noexcept { writeU32x<ByteOrder::kLE, Alignment>(p, uint32_t(x)); }
template<size_t Alignment = 1>
static inline void writeI32uBE(void* p, int32_t x) noexcept { writeU32x<ByteOrder::kBE, Alignment>(p, uint32_t(x)); }

static inline void writeI32a(void* p, int32_t x) noexcept { writeU32x<ByteOrder::kNative, 4>(p, uint32_t(x)); }
static inline void writeI32aLE(void* p, int32_t x) noexcept { writeU32x<ByteOrder::kLE, 4>(p, uint32_t(x)); }
static inline void writeI32aBE(void* p, int32_t x) noexcept { writeU32x<ByteOrder::kBE, 4>(p, uint32_t(x)); }

// Support - Memory Write Access - 64 Bits
// =======================================

template<ByteOrder BO = ByteOrder::kNative, size_t Alignment = 1>
static inline void writeU64x(void* p, uint64_t x) noexcept {
  typedef typename Internal::AliasedUInt<uint64_t, Alignment>::T U64AlignedToN;
  static_cast<U64AlignedToN*>(p)[0] = BO == ByteOrder::kNative ? x : byteswap64(x);
}

template<size_t Alignment = 1>
static inline void writeU64u(void* p, uint64_t x) noexcept { writeU64x<ByteOrder::kNative, Alignment>(p, x); }
template<size_t Alignment = 1>
static inline void writeU64uLE(void* p, uint64_t x) noexcept { writeU64x<ByteOrder::kLE, Alignment>(p, x); }
template<size_t Alignment = 1>
static inline void writeU64uBE(void* p, uint64_t x) noexcept { writeU64x<ByteOrder::kBE, Alignment>(p, x); }

static inline void writeU64a(void* p, uint64_t x) noexcept { writeU64x<ByteOrder::kNative, 8>(p, x); }
static inline void writeU64aLE(void* p, uint64_t x) noexcept { writeU64x<ByteOrder::kLE, 8>(p, x); }
static inline void writeU64aBE(void* p, uint64_t x) noexcept { writeU64x<ByteOrder::kBE, 8>(p, x); }

template<ByteOrder BO = ByteOrder::kNative, size_t Alignment = 1>
static inline void writeI64x(void* p, int64_t x) noexcept { writeU64x<BO, Alignment>(p, uint64_t(x)); }

template<size_t Alignment = 1>
static inline void writeI64u(void* p, int64_t x) noexcept { writeU64x<ByteOrder::kNative, Alignment>(p, uint64_t(x)); }
template<size_t Alignment = 1>
static inline void writeI64uLE(void* p, int64_t x) noexcept { writeU64x<ByteOrder::kLE, Alignment>(p, uint64_t(x)); }
template<size_t Alignment = 1>
static inline void writeI64uBE(void* p, int64_t x) noexcept { writeU64x<ByteOrder::kBE, Alignment>(p, uint64_t(x)); }

static inline void writeI64a(void* p, int64_t x) noexcept { writeU64x<ByteOrder::kNative, 8>(p, uint64_t(x)); }
static inline void writeI64aLE(void* p, int64_t x) noexcept { writeU64x<ByteOrder::kLE, 8>(p, uint64_t(x)); }
static inline void writeI64aBE(void* p, int64_t x) noexcept { writeU64x<ByteOrder::kBE, 8>(p, uint64_t(x)); }

// Support - Operators
// ===================

//! \cond INTERNAL
struct Set    { template<typename T> static inline T op(T x, T y) noexcept { DebugUtils::unused(x); return  y; } };
struct SetNot { template<typename T> static inline T op(T x, T y) noexcept { DebugUtils::unused(x); return ~y; } };
struct And    { template<typename T> static inline T op(T x, T y) noexcept { return  x &  y; } };
struct AndNot { template<typename T> static inline T op(T x, T y) noexcept { return  x & ~y; } };
struct NotAnd { template<typename T> static inline T op(T x, T y) noexcept { return ~x &  y; } };
struct Or     { template<typename T> static inline T op(T x, T y) noexcept { return  x |  y; } };
struct Xor    { template<typename T> static inline T op(T x, T y) noexcept { return  x ^  y; } };
struct Add    { template<typename T> static inline T op(T x, T y) noexcept { return  x +  y; } };
struct Sub    { template<typename T> static inline T op(T x, T y) noexcept { return  x -  y; } };
struct Min    { template<typename T> static inline T op(T x, T y) noexcept { return min<T>(x, y); } };
struct Max    { template<typename T> static inline T op(T x, T y) noexcept { return max<T>(x, y); } };
//! \endcond

// Support - BitWordIterator
// =========================

//! Iterates over each bit in a number which is set to 1.
//!
//! Example of use:
//!
//! ```
//! uint32_t bitsToIterate = 0x110F;
//! Support::BitWordIterator<uint32_t> it(bitsToIterate);
//!
//! while (it.hasNext()) {
//!   uint32_t bitIndex = it.next();
//!   std::printf("Bit at %u is set\n", unsigned(bitIndex));
//! }
//! ```
template<typename T>
class BitWordIterator {
public:
  ASMJIT_FORCE_INLINE explicit BitWordIterator(T bitWord) noexcept
    : _bitWord(bitWord) {}

  ASMJIT_FORCE_INLINE void init(T bitWord) noexcept { _bitWord = bitWord; }
  ASMJIT_FORCE_INLINE bool hasNext() const noexcept { return _bitWord != 0; }

  ASMJIT_FORCE_INLINE uint32_t next() noexcept {
    ASMJIT_ASSERT(_bitWord != 0);
    uint32_t index = ctz(_bitWord);
    _bitWord ^= T(1u) << index;
    return index;
  }

  T _bitWord;
};

// Support - BitWordFlipIterator
// =============================

template<typename T>
class BitWordFlipIterator {
public:
  ASMJIT_FORCE_INLINE explicit BitWordFlipIterator(T bitWord) noexcept
    : _bitWord(bitWord) {}

  ASMJIT_FORCE_INLINE void init(T bitWord) noexcept { _bitWord = bitWord; }
  ASMJIT_FORCE_INLINE bool hasNext() const noexcept { return _bitWord != 0; }

  ASMJIT_FORCE_INLINE uint32_t nextAndFlip() noexcept {
    ASMJIT_ASSERT(_bitWord != 0);
    uint32_t index = ctz(_bitWord);
    _bitWord ^= T(1u) << index;
    return index;
  }

  T _bitWord;
  T _xorMask;
};

// Support - BitVectorOps
// ======================

//! \cond
namespace Internal {
  template<typename T, class OperatorT, class FullWordOpT>
  static inline void bitVectorOp(T* buf, size_t index, size_t count) noexcept {
    if (count == 0)
      return;

    const size_t kTSizeInBits = bitSizeOf<T>();
    size_t vecIndex = index / kTSizeInBits; // T[]
    size_t bitIndex = index % kTSizeInBits; // T[][]

    buf += vecIndex;

    // The first BitWord requires special handling to preserve bits outside the fill region.
    const T kFillMask = allOnes<T>();
    size_t firstNBits = min<size_t>(kTSizeInBits - bitIndex, count);

    buf[0] = OperatorT::op(buf[0], (kFillMask >> (kTSizeInBits - firstNBits)) << bitIndex);
    buf++;
    count -= firstNBits;

    // All bits between the first and last affected BitWords can be just filled.
    while (count >= kTSizeInBits) {
      buf[0] = FullWordOpT::op(buf[0], kFillMask);
      buf++;
      count -= kTSizeInBits;
    }

    // The last BitWord requires special handling as well
    if (count)
      buf[0] = OperatorT::op(buf[0], kFillMask >> (kTSizeInBits - count));
  }
}
//! \endcond

//! Sets bit in a bit-vector `buf` at `index`.
template<typename T>
static inline bool bitVectorGetBit(T* buf, size_t index) noexcept {
  const size_t kTSizeInBits = bitSizeOf<T>();

  size_t vecIndex = index / kTSizeInBits;
  size_t bitIndex = index % kTSizeInBits;

  return bool((buf[vecIndex] >> bitIndex) & 0x1u);
}

//! Sets bit in a bit-vector `buf` at `index` to `value`.
template<typename T>
static inline void bitVectorSetBit(T* buf, size_t index, bool value) noexcept {
  const size_t kTSizeInBits = bitSizeOf<T>();

  size_t vecIndex = index / kTSizeInBits;
  size_t bitIndex = index % kTSizeInBits;

  T bitMask = T(1u) << bitIndex;
  if (value)
    buf[vecIndex] |= bitMask;
  else
    buf[vecIndex] &= ~bitMask;
}

//! Sets bit in a bit-vector `buf` at `index` to `value`.
template<typename T>
static inline void bitVectorFlipBit(T* buf, size_t index) noexcept {
  const size_t kTSizeInBits = bitSizeOf<T>();

  size_t vecIndex = index / kTSizeInBits;
  size_t bitIndex = index % kTSizeInBits;

  T bitMask = T(1u) << bitIndex;
  buf[vecIndex] ^= bitMask;
}

//! Fills `count` bits in bit-vector `buf` starting at bit-index `index`.
template<typename T>
static inline void bitVectorFill(T* buf, size_t index, size_t count) noexcept { Internal::bitVectorOp<T, Or, Set>(buf, index, count); }

//! Clears `count` bits in bit-vector `buf` starting at bit-index `index`.
template<typename T>
static inline void bitVectorClear(T* buf, size_t index, size_t count) noexcept { Internal::bitVectorOp<T, AndNot, SetNot>(buf, index, count); }

template<typename T>
static inline size_t bitVectorIndexOf(T* buf, size_t start, bool value) noexcept {
  const size_t kTSizeInBits = bitSizeOf<T>();
  size_t vecIndex = start / kTSizeInBits; // T[]
  size_t bitIndex = start % kTSizeInBits; // T[][]

  T* p = buf + vecIndex;

  // We always look for zeros, if value is `true` we have to flip all bits before the search.
  const T kFillMask = allOnes<T>();
  const T kFlipMask = value ? T(0) : kFillMask;

  // The first BitWord requires special handling as there are some bits we want to ignore.
  T bits = (*p ^ kFlipMask) & (kFillMask << bitIndex);
  for (;;) {
    if (bits)
      return (size_t)(p - buf) * kTSizeInBits + ctz(bits);
    bits = *++p ^ kFlipMask;
  }
}

// Support - BitVectorIterator
// ===========================

template<typename T>
class BitVectorIterator {
public:
  const T* _ptr;
  size_t _idx;
  size_t _end;
  T _current;

  ASMJIT_FORCE_INLINE BitVectorIterator(const BitVectorIterator& other) noexcept = default;

  ASMJIT_FORCE_INLINE BitVectorIterator(const T* data, size_t numBitWords, size_t start = 0) noexcept {
    init(data, numBitWords, start);
  }

  ASMJIT_FORCE_INLINE void init(const T* data, size_t numBitWords, size_t start = 0) noexcept {
    const T* ptr = data + (start / bitSizeOf<T>());
    size_t idx = alignDown(start, bitSizeOf<T>());
    size_t end = numBitWords * bitSizeOf<T>();

    T bitWord = T(0);
    if (idx < end) {
      bitWord = *ptr++ & (allOnes<T>() << (start % bitSizeOf<T>()));
      while (!bitWord && (idx += bitSizeOf<T>()) < end)
        bitWord = *ptr++;
    }

    _ptr = ptr;
    _idx = idx;
    _end = end;
    _current = bitWord;
  }

  ASMJIT_FORCE_INLINE bool hasNext() const noexcept {
    return _current != T(0);
  }

  ASMJIT_FORCE_INLINE size_t next() noexcept {
    T bitWord = _current;
    ASMJIT_ASSERT(bitWord != T(0));

    uint32_t bit = ctz(bitWord);
    bitWord ^= T(1u) << bit;

    size_t n = _idx + bit;
    while (!bitWord && (_idx += bitSizeOf<T>()) < _end)
      bitWord = *_ptr++;

    _current = bitWord;
    return n;
  }

  ASMJIT_FORCE_INLINE size_t peekNext() const noexcept {
    ASMJIT_ASSERT(_current != T(0));
    return _idx + ctz(_current);
  }
};

// Support - BitVectorOpIterator
// =============================

template<typename T, class OperatorT>
class BitVectorOpIterator {
public:
  enum : uint32_t {
    kTSizeInBits = bitSizeOf<T>()
  };

  const T* _aPtr;
  const T* _bPtr;
  size_t _idx;
  size_t _end;
  T _current;

  ASMJIT_FORCE_INLINE BitVectorOpIterator(const T* aData, const T* bData, size_t numBitWords, size_t start = 0) noexcept {
    init(aData, bData, numBitWords, start);
  }

  ASMJIT_FORCE_INLINE void init(const T* aData, const T* bData, size_t numBitWords, size_t start = 0) noexcept {
    const T* aPtr = aData + (start / bitSizeOf<T>());
    const T* bPtr = bData + (start / bitSizeOf<T>());
    size_t idx = alignDown(start, bitSizeOf<T>());
    size_t end = numBitWords * bitSizeOf<T>();

    T bitWord = T(0);
    if (idx < end) {
      bitWord = OperatorT::op(*aPtr++, *bPtr++) & (allOnes<T>() << (start % bitSizeOf<T>()));
      while (!bitWord && (idx += kTSizeInBits) < end)
        bitWord = OperatorT::op(*aPtr++, *bPtr++);
    }

    _aPtr = aPtr;
    _bPtr = bPtr;
    _idx = idx;
    _end = end;
    _current = bitWord;
  }

  ASMJIT_FORCE_INLINE bool hasNext() noexcept {
    return _current != T(0);
  }

  ASMJIT_FORCE_INLINE size_t next() noexcept {
    T bitWord = _current;
    ASMJIT_ASSERT(bitWord != T(0));

    uint32_t bit = ctz(bitWord);
    bitWord ^= T(1u) << bit;

    size_t n = _idx + bit;
    while (!bitWord && (_idx += kTSizeInBits) < _end)
      bitWord = OperatorT::op(*_aPtr++, *_bPtr++);

    _current = bitWord;
    return n;
  }
};

// Support - Sorting
// =================

//! Sort order.
enum class SortOrder : uint32_t {
  //!< Ascending order.
  kAscending  = 0,
  //!< Descending order.
  kDescending = 1
};

//! A helper class that provides comparison of any user-defined type that
//! implements `<` and `>` operators (primitive types are supported as well).
template<SortOrder kOrder = SortOrder::kAscending>
struct Compare {
  template<typename A, typename B>
  inline int operator()(const A& a, const B& b) const noexcept {
    return kOrder == SortOrder::kAscending ? int(a > b) - int(a < b) : int(a < b) - int(a > b);
  }
};

//! Insertion sort.
template<typename T, typename CompareT = Compare<SortOrder::kAscending>>
static inline void iSort(T* base, size_t size, const CompareT& cmp = CompareT()) noexcept {
  for (T* pm = base + 1; pm < base + size; pm++)
    for (T* pl = pm; pl > base && cmp(pl[-1], pl[0]) > 0; pl--)
      std::swap(pl[-1], pl[0]);
}

//! \cond
namespace Internal {
  //! Quick-sort implementation.
  template<typename T, class CompareT>
  struct QSortImpl {
    enum : size_t {
      kStackSize = 64 * 2,
      kISortThreshold = 7
    };

    // Based on "PDCLib - Public Domain C Library" and rewritten to C++.
    static void sort(T* base, size_t size, const CompareT& cmp) noexcept {
      T* end = base + size;
      T* stack[kStackSize];
      T** stackptr = stack;

      for (;;) {
        if ((size_t)(end - base) > kISortThreshold) {
          // We work from second to last - first will be pivot element.
          T* pi = base + 1;
          T* pj = end - 1;
          std::swap(base[(size_t)(end - base) / 2], base[0]);

          if (cmp(*pi  , *pj  ) > 0) std::swap(*pi  , *pj  );
          if (cmp(*base, *pj  ) > 0) std::swap(*base, *pj  );
          if (cmp(*pi  , *base) > 0) std::swap(*pi  , *base);

          // Now we have the median for pivot element, entering main loop.
          for (;;) {
            while (pi < pj   && cmp(*++pi, *base) < 0) continue; // Move `i` right until `*i >= pivot`.
            while (pj > base && cmp(*--pj, *base) > 0) continue; // Move `j` left  until `*j <= pivot`.

            if (pi > pj) break;
            std::swap(*pi, *pj);
          }

          // Move pivot into correct place.
          std::swap(*base, *pj);

          // Larger subfile base / end to stack, sort smaller.
          if (pj - base > end - pi) {
            // Left is larger.
            *stackptr++ = base;
            *stackptr++ = pj;
            base = pi;
          }
          else {
            // Right is larger.
            *stackptr++ = pi;
            *stackptr++ = end;
            end = pj;
          }
          ASMJIT_ASSERT(stackptr <= stack + kStackSize);
        }
        else {
          // UB sanitizer doesn't like applying offset to a nullptr base.
          if (base != end)
            iSort(base, (size_t)(end - base), cmp);

          if (stackptr == stack)
            break;

          end = *--stackptr;
          base = *--stackptr;
        }
      }
    }
  };
}
//! \endcond

//! Quick sort implementation.
//!
//! The main reason to provide a custom qsort implementation is that we needed something that will
//! never throw `bad_alloc` exception. This implementation doesn't use dynamic memory allocation.
template<typename T, class CompareT = Compare<SortOrder::kAscending>>
static inline void qSort(T* base, size_t size, const CompareT& cmp = CompareT()) noexcept {
  Internal::QSortImpl<T, CompareT>::sort(base, size, cmp);
}

// Support - Array
// ===============

//! Array type, similar to std::array<T, N>, with the possibility to use enums in operator[].
//!
//! \note The array has C semantics - the elements in the array are not initialized.
template<typename T, size_t N>
struct Array {
  //! \name Members
  //! \{

  //! The underlying array data, use \ref data() to access it.
  T _data[N];

  //! \}

  //! \cond
  // std compatibility.
  typedef T value_type;
  typedef size_t size_type;
  typedef ptrdiff_t difference_type;

  typedef value_type& reference;
  typedef const value_type& const_reference;

  typedef value_type* pointer;
  typedef const value_type* const_pointer;

  typedef pointer iterator;
  typedef const_pointer const_iterator;
  //! \endcond

  //! \name Overloaded Operators
  //! \{

  template<typename Index>
  inline T& operator[](const Index& index) noexcept {
    typedef typename Internal::StdInt<sizeof(Index), 1>::Type U;
    ASMJIT_ASSERT(U(index) < N);
    return _data[U(index)];
  }

  template<typename Index>
  inline const T& operator[](const Index& index) const noexcept {
    typedef typename Internal::StdInt<sizeof(Index), 1>::Type U;
    ASMJIT_ASSERT(U(index) < N);
    return _data[U(index)];
  }

  inline bool operator==(const Array& other) const noexcept {
    for (size_t i = 0; i < N; i++)
      if (_data[i] != other._data[i])
        return false;
    return true;
  }

  inline bool operator!=(const Array& other) const noexcept {
    return !operator==(other);
  }

  //! \}

  //! \name Accessors
  //! \{

  inline bool empty() const noexcept { return false; }
  inline size_t size() const noexcept { return N; }

  inline T* data() noexcept { return _data; }
  inline const T* data() const noexcept { return _data; }

  inline T& front() noexcept { return _data[0]; }
  inline const T& front() const noexcept { return _data[0]; }

  inline T& back() noexcept { return _data[N - 1]; }
  inline const T& back() const noexcept { return _data[N - 1]; }

  inline T* begin() noexcept { return _data; }
  inline T* end() noexcept { return _data + N; }

  inline const T* begin() const noexcept { return _data; }
  inline const T* end() const noexcept { return _data + N; }

  inline const T* cbegin() const noexcept { return _data; }
  inline const T* cend() const noexcept { return _data + N; }

  //! \}

  //! \name Utilities
  //! \{

  inline void swap(Array& other) noexcept {
    for (size_t i = 0; i < N; i++)
      std::swap(_data[i], other._data[i]);
  }

  inline void fill(const T& value) noexcept {
    for (size_t i = 0; i < N; i++)
      _data[i] = value;
  }

  inline void copyFrom(const Array& other) noexcept {
    for (size_t i = 0; i < N; i++)
      _data[i] = other._data[i];
  }

  template<typename Operator>
  inline void combine(const Array& other) noexcept {
    for (size_t i = 0; i < N; i++)
      _data[i] = Operator::op(_data[i], other._data[i]);
  }

  template<typename Operator>
  inline T aggregate(T initialValue = T()) const noexcept {
    T value = initialValue;
    for (size_t i = 0; i < N; i++)
      value = Operator::op(value, _data[i]);
    return value;
  }

  template<typename Fn>
  inline void forEach(Fn&& fn) noexcept {
    for (size_t i = 0; i < N; i++)
      fn(_data[i]);
  }
  //! \}
};

// Support::Temporary
// ==================

//! Used to pass a temporary buffer to:
//!
//!   - Containers that use user-passed buffer as an initial storage (still can grow).
//!   - Zone allocator that would use the temporary buffer as a first block.
struct Temporary {
  //! \name Members
  //! \{

  void* _data;
  size_t _size;

  //! \}

  //! \name Construction & Destruction
  //! \{

  inline constexpr Temporary(const Temporary& other) noexcept = default;
  inline constexpr Temporary(void* data, size_t size) noexcept
    : _data(data),
      _size(size) {}

  //! \}

  //! \name Overloaded Operators
  //! \{

  inline Temporary& operator=(const Temporary& other) noexcept = default;

  //! \}

  //! \name Accessors
  //! \{

  //! Returns the data storage.
  template<typename T = void>
  inline constexpr T* data() const noexcept { return static_cast<T*>(_data); }
  //! Returns the data storage size in bytes.
  inline constexpr size_t size() const noexcept { return _size; }

  //! \}
};

} // {Support}

//! \}

ASMJIT_END_NAMESPACE

#endif // ASMJIT_CORE_SUPPORT_H_INCLUDED
