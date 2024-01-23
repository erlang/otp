// This file is part of AsmJit project <https://asmjit.com>
//
// See asmjit.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#include "../core/api-build_p.h"
#include "../core/codeholder.h"
#include "../core/codewriter_p.h"
#include "../arm/armutils.h"

ASMJIT_BEGIN_NAMESPACE

bool CodeWriterUtils::encodeOffset32(uint32_t* dst, int64_t offset64, const OffsetFormat& format) noexcept {
  uint32_t bitCount = format.immBitCount();
  uint32_t bitShift = format.immBitShift();
  uint32_t discardLsb = format.immDiscardLsb();

  // Invalid offset (should not happen).
  if (!bitCount || bitCount > format.valueSize() * 8u)
    return false;

  uint32_t value;
  uint32_t u = 0;
  bool unsignedLogic = format.type() == OffsetType::kUnsignedOffset;

  // First handle all offsets that use additional field for their sign and the offset is encoded as its
  // absolute value.
  if (format.hasSignBit()) {
    u = uint32_t(offset64 >= 0);
    if (u == 0)
      offset64 = -offset64;
    unsignedLogic = true;
  }

  // First handle all unsigned offset types.
  if (unsignedLogic) {
    if (discardLsb) {
      ASMJIT_ASSERT(discardLsb <= 32);
      if ((offset64 & Support::lsbMask<uint32_t>(discardLsb)) != 0)
        return false;
      offset64 = int64_t(uint64_t(offset64) >> discardLsb);
    }

    value = uint32_t(offset64 & Support::lsbMask<uint32_t>(bitCount));
    if (value != offset64)
      return false;
  }
  else {
    // The rest of OffsetType options are all signed.
    if (discardLsb) {
      ASMJIT_ASSERT(discardLsb <= 32);
      if ((offset64 & Support::lsbMask<uint32_t>(discardLsb)) != 0)
        return false;
      offset64 >>= discardLsb;
    }

    if (!Support::isInt32(offset64))
      return false;

    value = uint32_t(int32_t(offset64));
    if (!Support::isEncodableOffset32(int32_t(value), bitCount))
      return false;
  }

  switch (format.type()) {
    case OffsetType::kSignedOffset:
    case OffsetType::kUnsignedOffset: {
      *dst = (value & Support::lsbMask<uint32_t>(bitCount)) << bitShift;
      return true;
    }

    // Opcode: {.....|imm:1|..N.N|......|imm:3|....|imm:8}
    case OffsetType::kThumb32_ADR: {
      // Sanity checks.
      if (format.valueSize() != 4 || bitCount != 12 || bitShift != 0)
        return false;

      uint32_t imm8 = (value & 0x00FFu);
      uint32_t imm3 = (value & 0x0700u) << (12 - 8);
      uint32_t imm1 = (value & 0x0800u) << (26 - 11);
      uint32_t n = u ^ 1u;

      *dst = imm8 | imm3 | imm1 | (n << 21) | (n << 23);
      return true;
    }

    // Opcode: {....|.|imm[22]|imm[19:10]|..|ja|.|jb|imm[9:0]|.}
    case OffsetType::kThumb32_BLX:
      // The calculation is the same as `B`, but the first LSB bit must be zero, so account for that.
      value <<= 1;
      ASMJIT_FALLTHROUGH;

    // Opcode: {....|.|imm[23]|imm[20:11]|..|ja|.|jb|imm[10:0]}
    case OffsetType::kThumb32_B: {
      // Sanity checks.
      if (format.valueSize() != 4)
        return false;

      uint32_t ia = (value & 0x0007FFu);
      uint32_t ib = (value & 0x1FF800u) << (16 - 11);
      uint32_t ic = (value & 0x800000u) << (26 - 23);
      uint32_t ja = ((~value >> 23) ^ (value >> 22)) & 1u;
      uint32_t jb = ((~value >> 23) ^ (value >> 21)) & 1u;

      *dst = ia | ib | ic | (ja << 14) | (jb << 11);
      return true;
    }

    // Opcode: {....|.|imm[19]|....|imm[16:11]|..|ja|.|jb|imm[10:0]}
    case OffsetType::kThumb32_BCond: {
      // Sanity checks.
      if (format.valueSize() != 4 || bitCount != 20 || bitShift != 0)
        return false;

      uint32_t ia = (value & 0x0007FFu);
      uint32_t ib = (value & 0x01F800u) << (16 - 11);
      uint32_t ic = (value & 0x080000u) << (26 - 19);
      uint32_t ja = ((~value >> 19) ^ (value >> 22)) & 1u;
      uint32_t jb = ((~value >> 19) ^ (value >> 21)) & 1u;

      *dst = ia | ib | ic | (ja << 14) | (jb << 11);
      return true;
    }

    case OffsetType::kAArch32_ADR: {
      uint32_t encodedImm;
      if (!arm::Utils::encodeAArch32Imm(value, &encodedImm))
        return false;

      *dst = (Support::bitMask(22) << u) | (encodedImm << bitShift);
      return true;
    }

    case OffsetType::kAArch32_U23_SignedOffset: {
      *dst = (value << bitShift) | (u << 23);
      return true;
    }

    case OffsetType::kAArch32_U23_0To3At0_4To7At8: {
      // Sanity checks.
      if (format.valueSize() != 4 || bitCount != 8 || bitShift != 0)
        return false;

      uint32_t immLo = (value & 0x0Fu);
      uint32_t immHi = (value & 0xF0u) << (8 - 4);

      *dst = immLo | immHi | (u << 23);
      return true;
    }

    case OffsetType::kAArch32_1To24At0_0At24: {
      // Sanity checks.
      if (format.valueSize() != 4 || bitCount != 25 || bitShift != 0)
        return false;

      uint32_t immLo = (value & 0x0000001u) << 24;
      uint32_t immHi = (value & 0x1FFFFFEu) >> 1;

      *dst = immLo | immHi;
      return true;
    }

    case OffsetType::kAArch64_ADR:
    case OffsetType::kAArch64_ADRP: {
      // Sanity checks.
      if (format.valueSize() != 4 || bitCount != 21 || bitShift != 5)
        return false;

      uint32_t immLo = value & 0x3u;
      uint32_t immHi = (value >> 2) & Support::lsbMask<uint32_t>(19);

      *dst = (immLo << 29) | (immHi << 5);
      return true;
    }

    default:
      return false;
  }
}

bool CodeWriterUtils::encodeOffset64(uint64_t* dst, int64_t offset64, const OffsetFormat& format) noexcept {
  uint32_t bitCount = format.immBitCount();
  uint32_t discardLsb = format.immDiscardLsb();

  if (!bitCount || bitCount > format.valueSize() * 8u)
    return false;

  uint64_t value;

  // First handle all unsigned offset types.
  if (format.type() == OffsetType::kUnsignedOffset) {
    if (discardLsb) {
      ASMJIT_ASSERT(discardLsb <= 32);
      if ((offset64 & Support::lsbMask<uint32_t>(discardLsb)) != 0)
        return false;
      offset64 = int64_t(uint64_t(offset64) >> discardLsb);
    }

    value = uint64_t(offset64) & Support::lsbMask<uint64_t>(bitCount);
    if (value != uint64_t(offset64))
      return false;
  }
  else {
    // The rest of OffsetType options are all signed.
    if (discardLsb) {
      ASMJIT_ASSERT(discardLsb <= 32);
      if ((offset64 & Support::lsbMask<uint32_t>(discardLsb)) != 0)
        return false;
      offset64 >>= discardLsb;
    }

    if (!Support::isEncodableOffset64(offset64, bitCount))
      return false;

    value = uint64_t(offset64);
  }

  switch (format.type()) {
    case OffsetType::kSignedOffset:
    case OffsetType::kUnsignedOffset: {
      *dst = (value & Support::lsbMask<uint64_t>(bitCount)) << format.immBitShift();
      return true;
    }

    default:
      return false;
  }
}

bool CodeWriterUtils::writeOffset(void* dst, int64_t offset64, const OffsetFormat& format) noexcept {
  // Offset the destination by ValueOffset so the `dst` points to the
  // patched word instead of the beginning of the patched region.
  dst = static_cast<char*>(dst) + format.valueOffset();

  switch (format.valueSize()) {
    case 1: {
      uint32_t mask;
      if (!encodeOffset32(&mask, offset64, format))
        return false;

      Support::writeU8(dst, uint8_t(Support::readU8(dst) | mask));
      return true;
    }

    case 2: {
      uint32_t mask;
      if (!encodeOffset32(&mask, offset64, format))
        return false;

      Support::writeU16uLE(dst, uint16_t(Support::readU16uLE(dst) | mask));
      return true;
    }

    case 4: {
      uint32_t mask;
      if (!encodeOffset32(&mask, offset64, format)) {
        return false;
      }

      Support::writeU32uLE(dst, Support::readU32uLE(dst) | mask);
      return true;
    }

    case 8: {
      uint64_t mask;
      if (!encodeOffset64(&mask, offset64, format))
        return false;

      Support::writeU64uLE(dst, Support::readU64uLE(dst) | mask);
      return true;
    }

    default:
      return false;
  }
}

ASMJIT_END_NAMESPACE
