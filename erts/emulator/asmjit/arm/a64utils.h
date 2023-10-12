// This file is part of AsmJit project <https://asmjit.com>
//
// See asmjit.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#ifndef ASMJIT_ARM_A64UTILS_H_INCLUDED
#define ASMJIT_ARM_A64UTILS_H_INCLUDED

#include "../arm/a64globals.h"

ASMJIT_BEGIN_SUB_NAMESPACE(a64)

//! \addtogroup asmjit_a64
//! \{

//! Public utilities and helpers for targeting AArch64 architecture.
namespace Utils {

//! Decomposed fields of a logical immediate value (AArch64).
struct LogicalImm {
  uint32_t n;
  uint32_t s;
  uint32_t r;
};

//! Encodes the given `imm` value of the given `width` to a logical immediate value represented as N, S, and R fields
//! and writes these fields to `out`.
//!
//! Encoding Table:
//!
//! ```
//! +---+--------+--------+------+
//! | N |  ImmS  |  ImmR  | Size |
//! +---+--------+--------+------+
//! | 1 | ssssss | rrrrrr |  64  |
//! | 0 | 0sssss | .rrrrr |  32  |
//! | 0 | 10ssss | ..rrrr |  16  |
//! | 0 | 110sss | ...rrr |  8   |
//! | 0 | 1110ss | ....rr |  4   |
//! | 0 | 11110s | .....r |  2   |
//! +---+--------+--------+------+
//! ```
ASMJIT_MAYBE_UNUSED
static bool encodeLogicalImm(uint64_t imm, uint32_t width, a64::Utils::LogicalImm* out) noexcept {
  // Determine the element width, which must be 2, 4, 8, 16, 32, or 64 bits.
  do {
    width /= 2;
    uint64_t mask = (uint64_t(1) << width) - 1u;
    if ((imm & mask) != ((imm >> width) & mask)) {
      width *= 2;
      break;
    }
  } while (width > 2);

  // Patterns of all zeros and all ones are not encodable.
  uint64_t lsbMask = Support::lsbMask<uint64_t>(width);
  imm &= lsbMask;

  if (imm == 0 || imm == lsbMask)
    return false;

  // Inspect the pattern and get the most important bit indexes.
  //
  //         oIndex <-+      +-> zIndex
  //                  |      |
  // |..zeros..|oCount|zCount|..ones..|
  // |000000000|111111|000000|11111111|

  uint32_t zIndex = Support::ctz(~imm);
  uint64_t zImm = imm ^ ((uint64_t(1) << zIndex) - 1);
  uint32_t zCount = (zImm ? Support::ctz(zImm) : width) - zIndex;

  uint32_t oIndex = zIndex + zCount;
  uint64_t oImm = ~(zImm ^ Support::lsbMask<uint64_t>(oIndex));
  uint32_t oCount = (oImm ? Support::ctz(oImm) : width) - (oIndex);

  // Verify whether the bit-pattern is encodable.
  uint64_t mustBeZero = oImm ^ ~Support::lsbMask<uint64_t>(oIndex + oCount);
  if (mustBeZero != 0 || (zIndex > 0 && width - (oIndex + oCount) != 0))
    return false;

  out->n = width == 64;
  out->s = (oCount + zIndex - 1) | (Support::neg(width * 2) & 0x3F);
  out->r = width - oIndex;
  return true;
}

//! Returns true if the given `imm` value is encodable as a logical immediate. The `width` argument describes the
//! width of the operation, and must be either 32 or 64. This function can be used to test whether an immediate
//! value can be used with AND, ANDS, BIC, BICS, EON, EOR, ORN, and ORR instruction.
ASMJIT_MAYBE_UNUSED
static inline bool isLogicalImm(uint64_t imm, uint32_t width) noexcept {
  LogicalImm dummy;
  return encodeLogicalImm(imm, width, &dummy);
}

//! Returns true if the given `imm` value is a byte mask. Byte mask has each byte part of the value set to either
//! 0x00 or 0xFF. Some ARM instructions accept immediates that form a byte-mask and this function can be used to
//! verify that the immediate is encodable before using the value.
template<typename T>
static inline bool isByteMaskImm8(const T& imm) noexcept {
  constexpr T kMask = T(0x0101010101010101 & Support::allOnes<T>());
  return imm == (imm & kMask) * T(255);
}

//! \cond
//! A generic implementation that checjs whether a floating point value can be converted to ARM Imm8.
template<typename T, uint32_t kNumBBits, uint32_t kNumCDEFGHBits, uint32_t kNumZeroBits>
static inline bool isFPImm8Generic(T val) noexcept {
  constexpr uint32_t kAllBsMask = Support::lsbMask<uint32_t>(kNumBBits);
  constexpr uint32_t kB0Pattern = Support::bitMask(kNumBBits - 1);
  constexpr uint32_t kB1Pattern = kAllBsMask ^ kB0Pattern;

  T immZ = val & Support::lsbMask<T>(kNumZeroBits);
  uint32_t immB = uint32_t(val >> (kNumZeroBits + kNumCDEFGHBits)) & kAllBsMask;

  // ImmZ must be all zeros and ImmB must either be B0 or B1 pattern.
  return immZ == 0 && (immB == kB0Pattern || immB == kB1Pattern);
}
//! \endcond

//! Returns true if the given half precision floating point `val` can be encoded as ARM IMM8 value, which represents
//! a limited set of floating point immediate values, which can be used with FMOV instruction.
//!
//! The floating point must have bits distributed in the following way:
//!
//! ```
//! [aBbbcdef|gh000000]
//! ```
static inline bool isFP16Imm8(uint32_t val) noexcept { return isFPImm8Generic<uint32_t, 3, 6, 6>(val); }

//! Returns true if the given single precision floating point `val` can be encoded as ARM IMM8 value, which represents
//! a limited set of floating point immediate values, which can be used with FMOV instruction.
//!
//! The floating point must have bits distributed in the following way:
//!
//! ```
//! [aBbbbbbc|defgh000|00000000|00000000]
//! ```
static inline bool isFP32Imm8(uint32_t val) noexcept { return isFPImm8Generic<uint32_t, 6, 6, 19>(val); }
//! \overload
static inline bool isFP32Imm8(float val) noexcept { return isFP32Imm8(Support::bitCast<uint32_t>(val)); }

//! Returns true if the given double precision floating point `val` can be encoded as ARM IMM8 value, which represents
//! a limited set of floating point immediate values, which can be used with FMOV instruction.
//!
//! The floating point must have bits distributed in the following way:
//!
//! ```
//! [aBbbbbbb|bbcdefgh|00000000|00000000|00000000|00000000|00000000|00000000]
//! ```
static inline bool isFP64Imm8(uint64_t val) noexcept { return isFPImm8Generic<uint64_t, 9, 6, 48>(val); }
//! \overload
static inline bool isFP64Imm8(double val) noexcept { return isFP64Imm8(Support::bitCast<uint64_t>(val)); }

//! \cond
template<typename T, uint32_t kNumBBits, uint32_t kNumCDEFGHBits, uint32_t kNumZeroBits>
static inline uint32_t encodeFPToImm8Generic(T val) noexcept {
  uint32_t bits = uint32_t(val >> kNumZeroBits);
  return ((bits >> (kNumBBits + kNumCDEFGHBits - 7)) & 0x80u) | (bits & 0x7F);
}
//! \endcond

//! Encodes a double precision floating point value into IMM8 format.
//!
//! \note This function expects that `isFP64Imm8(val) == true` so it doesn't perform any checks of the value and just
//! rearranges some bits into Imm8 order.
static inline uint32_t encodeFP64ToImm8(uint64_t val) noexcept { return encodeFPToImm8Generic<uint64_t, 9, 6, 48>(val); }
//! \overload
static inline uint32_t encodeFP64ToImm8(double val) noexcept { return encodeFP64ToImm8(Support::bitCast<uint64_t>(val)); }

} // {Utils}

//! \}

ASMJIT_END_SUB_NAMESPACE

#endif // ASMJIT_ARM_A64UTILS_H_INCLUDED

