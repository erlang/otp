// This file is part of AsmJit project <https://asmjit.com>
//
// See asmjit.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#include "../core/api-build_p.h"
#if !defined(ASMJIT_NO_AARCH64)

#include "../core/codewriter_p.h"
#include "../core/cpuinfo.h"
#include "../core/emitterutils_p.h"
#include "../core/formatter.h"
#include "../core/logger.h"
#include "../core/misc_p.h"
#include "../core/support.h"
#include "../arm/armformatter_p.h"
#include "../arm/a64assembler.h"
#include "../arm/a64emithelper_p.h"
#include "../arm/a64instdb_p.h"
#include "../arm/a64utils.h"

ASMJIT_BEGIN_SUB_NAMESPACE(a64)

// a64::Assembler - Cond
// =====================

static inline uint32_t condCodeToOpcodeCond(uint32_t cond) noexcept {
  return (uint32_t(cond) - 2u) & 0xFu;
}

// a64::Assembler - Bits
// =====================

template<typename T>
static inline constexpr uint32_t B(const T& index) noexcept { return uint32_t(1u) << uint32_t(index); }

static constexpr uint32_t kSP = Gp::kIdSp;
static constexpr uint32_t kZR = Gp::kIdZr;
static constexpr uint32_t kWX = InstDB::kWX;

// a64::Assembler - ShiftOpToLdStOptMap
// ====================================

// Table that maps ShiftOp to OPT part in LD/ST (register) opcode.
#define VALUE(index) index == uint32_t(ShiftOp::kUXTW) ? 2u : \
                     index == uint32_t(ShiftOp::kLSL)  ? 3u : \
                     index == uint32_t(ShiftOp::kSXTW) ? 6u : \
                     index == uint32_t(ShiftOp::kSXTX) ? 7u : 0xFF
static const uint8_t armShiftOpToLdStOptMap[] = { ASMJIT_LOOKUP_TABLE_16(VALUE, 0) };
#undef VALUE

static inline constexpr uint32_t diff(RegType a, RegType b) noexcept {
  return uint32_t(a) - uint32_t(b);
}

// asmjit::a64::Assembler - SizeOp
// ===============================

//! Struct that contains Size (2 bits), Q flag, and S (scalar) flag. These values
//! are used to encode Q, Size, and Scalar fields in an opcode.
struct SizeOp {
  enum : uint8_t {
    k128BitShift = 0,
    kScalarShift = 1,
    kSizeShift = 2,

    kQ = uint8_t(1u << k128BitShift),
    kS = uint8_t(1u << kScalarShift),

    k00 = uint8_t(0 << kSizeShift),
    k01 = uint8_t(1 << kSizeShift),
    k10 = uint8_t(2 << kSizeShift),
    k11 = uint8_t(3 << kSizeShift),

    k00Q = k00 | kQ,
    k01Q = k01 | kQ,
    k10Q = k10 | kQ,
    k11Q = k11 | kQ,

    k00S = k00 | kS,
    k01S = k01 | kS,
    k10S = k10 | kS,
    k11S = k11 | kS,

    kInvalid = 0xFFu,

    // Masks used by SizeOpMap.
    kSzQ = (0x3u << kSizeShift) | kQ,
    kSzS = (0x3u << kSizeShift) | kS,
    kSzQS = (0x3u << kSizeShift) | kQ | kS
  };

  uint8_t value;

  inline bool isValid() const noexcept { return value != kInvalid; }
  inline void makeInvalid() noexcept { value = kInvalid; }

  inline uint32_t q() const noexcept { return (value >> k128BitShift) & 0x1u; }
  inline uint32_t qs() const noexcept { return ((value >> k128BitShift) | (value >> kScalarShift)) & 0x1u; }
  inline uint32_t scalar() const noexcept { return (value >> kScalarShift) & 0x1u; }
  inline uint32_t size() const noexcept { return (value >> kSizeShift) & 0x3u; }

  inline void decrementSize() noexcept {
    ASMJIT_ASSERT(size() > 0);
    value = uint8_t(value - (1u << kSizeShift));
  }
};

struct SizeOpTable {
  enum TableId : uint8_t {
    kTableBin = 0,
    kTableAny,
    kCount
  };

  // 40 elements for each combination.
  SizeOp array[(uint32_t(RegType::kARM_VecV) - uint32_t(RegType::kARM_VecB) + 1) * 8];
};

#define VALUE_BIN(x) { \
  x == (((uint32_t(RegType::kARM_VecD) - uint32_t(RegType::kARM_VecB)) << 3) | (Vec::kElementTypeNone)) ? SizeOp::k00  : \
  x == (((uint32_t(RegType::kARM_VecV) - uint32_t(RegType::kARM_VecB)) << 3) | (Vec::kElementTypeNone)) ? SizeOp::k00Q : \
  x == (((uint32_t(RegType::kARM_VecD) - uint32_t(RegType::kARM_VecB)) << 3) | (Vec::kElementTypeB   )) ? SizeOp::k00  : \
  x == (((uint32_t(RegType::kARM_VecV) - uint32_t(RegType::kARM_VecB)) << 3) | (Vec::kElementTypeB   )) ? SizeOp::k00Q : SizeOp::kInvalid \
}

#define VALUE_ANY(x) { \
  x == (((uint32_t(RegType::kARM_VecB) - uint32_t(RegType::kARM_VecB)) << 3) | (Vec::kElementTypeNone)) ? SizeOp::k00S : \
  x == (((uint32_t(RegType::kARM_VecH) - uint32_t(RegType::kARM_VecB)) << 3) | (Vec::kElementTypeNone)) ? SizeOp::k01S : \
  x == (((uint32_t(RegType::kARM_VecS) - uint32_t(RegType::kARM_VecB)) << 3) | (Vec::kElementTypeNone)) ? SizeOp::k10S : \
  x == (((uint32_t(RegType::kARM_VecD) - uint32_t(RegType::kARM_VecB)) << 3) | (Vec::kElementTypeNone)) ? SizeOp::k11S : \
  x == (((uint32_t(RegType::kARM_VecD) - uint32_t(RegType::kARM_VecB)) << 3) | (Vec::kElementTypeB   )) ? SizeOp::k00  : \
  x == (((uint32_t(RegType::kARM_VecV) - uint32_t(RegType::kARM_VecB)) << 3) | (Vec::kElementTypeB   )) ? SizeOp::k00Q : \
  x == (((uint32_t(RegType::kARM_VecD) - uint32_t(RegType::kARM_VecB)) << 3) | (Vec::kElementTypeH   )) ? SizeOp::k01  : \
  x == (((uint32_t(RegType::kARM_VecV) - uint32_t(RegType::kARM_VecB)) << 3) | (Vec::kElementTypeH   )) ? SizeOp::k01Q : \
  x == (((uint32_t(RegType::kARM_VecD) - uint32_t(RegType::kARM_VecB)) << 3) | (Vec::kElementTypeS   )) ? SizeOp::k10  : \
  x == (((uint32_t(RegType::kARM_VecV) - uint32_t(RegType::kARM_VecB)) << 3) | (Vec::kElementTypeS   )) ? SizeOp::k10Q : \
  x == (((uint32_t(RegType::kARM_VecD) - uint32_t(RegType::kARM_VecB)) << 3) | (Vec::kElementTypeD   )) ? SizeOp::k11S : \
  x == (((uint32_t(RegType::kARM_VecV) - uint32_t(RegType::kARM_VecB)) << 3) | (Vec::kElementTypeD   )) ? SizeOp::k11Q : SizeOp::kInvalid \
}

static const SizeOpTable sizeOpTable[SizeOpTable::kCount] = {
  {{ ASMJIT_LOOKUP_TABLE_40(VALUE_BIN, 0) }},
  {{ ASMJIT_LOOKUP_TABLE_40(VALUE_ANY, 0) }}
};

#undef VALUE_ANY
#undef VALUE_BIN

struct SizeOpMap {
  uint8_t tableId;
  uint8_t sizeOpMask;
  uint16_t acceptMask;
};

static const constexpr SizeOpMap sizeOpMap[InstDB::kVO_Count] = {
  { // kVO_V_B:
    SizeOpTable::kTableBin, SizeOp::kQ   , uint16_t(B(SizeOp::k00) | B(SizeOp::k00Q))
  },

  { // kVO_V_BH:
    SizeOpTable::kTableAny, SizeOp::kSzQS, uint16_t(B(SizeOp::k00) | B(SizeOp::k00Q) | B(SizeOp::k01) | B(SizeOp::k01Q))
  },

  { // kVO_V_BH_4S:
    SizeOpTable::kTableAny, SizeOp::kSzQS, uint16_t(B(SizeOp::k00) | B(SizeOp::k00Q) | B(SizeOp::k01) | B(SizeOp::k01Q) | B(SizeOp::k10Q))
  },

  { // kVO_V_BHS:
    SizeOpTable::kTableAny, SizeOp::kSzQS, uint16_t(B(SizeOp::k00) | B(SizeOp::k00Q) | B(SizeOp::k01) | B(SizeOp::k01Q) | B(SizeOp::k10) | B(SizeOp::k10Q))
  },

  { // kVO_V_BHS_D2:
    SizeOpTable::kTableAny, SizeOp::kSzQS, uint16_t(B(SizeOp::k00) | B(SizeOp::k00Q) | B(SizeOp::k01) | B(SizeOp::k01Q) | B(SizeOp::k10) | B(SizeOp::k10Q) | B(SizeOp::k11Q))
  },

  { // kVO_V_HS:
    SizeOpTable::kTableAny, SizeOp::kSzQS, uint16_t(B(SizeOp::k01) | B(SizeOp::k01Q) | B(SizeOp::k10) | B(SizeOp::k10Q))
  },

  { // kVO_V_S:
    SizeOpTable::kTableAny, SizeOp::kQ   , uint16_t(B(SizeOp::k10) | B(SizeOp::k10Q))
  },

  { // kVO_V_B8H4:
    SizeOpTable::kTableAny, SizeOp::kSzQS, uint16_t(B(SizeOp::k00) | B(SizeOp::k01))
  },

  { // kVO_V_B8H4S2:
    SizeOpTable::kTableAny, SizeOp::kSzQS, uint16_t(B(SizeOp::k00) | B(SizeOp::k01) | B(SizeOp::k10))
  },

  { // kVO_V_B8D1:
    SizeOpTable::kTableAny, SizeOp::kSzQ , uint16_t(B(SizeOp::k00) | B(SizeOp::k11S))
  },

  { // kVO_V_H4S2:
    SizeOpTable::kTableAny, SizeOp::kSzQS, uint16_t(B(SizeOp::k01) | B(SizeOp::k10))
  },

  { // kVO_V_B16:
    SizeOpTable::kTableBin, SizeOp::kQ   , uint16_t(B(SizeOp::k00Q))
  },

  { // kVO_V_B16H8:
    SizeOpTable::kTableAny, SizeOp::kSzQS, uint16_t(B(SizeOp::k00Q) | B(SizeOp::k01Q))
  },

  { // kVO_V_B16H8S4:
    SizeOpTable::kTableAny, SizeOp::kSzQS, uint16_t(B(SizeOp::k00Q) | B(SizeOp::k01Q) | B(SizeOp::k10Q))
  },

  { // kVO_V_B16D2:
    SizeOpTable::kTableAny, SizeOp::kSzQS, uint16_t(B(SizeOp::k00Q) | B(SizeOp::k11Q))
  },

  { // kVO_V_H8S4:
    SizeOpTable::kTableAny, SizeOp::kSzQS, uint16_t(B(SizeOp::k01Q) | B(SizeOp::k10Q))
  },

  { // kVO_V_S4:
    SizeOpTable::kTableAny, 0            , uint16_t(B(SizeOp::k10Q))
  },

  { // kVO_V_D2:
    SizeOpTable::kTableAny, 0            , uint16_t(B(SizeOp::k11Q))
  },

  { // kVO_SV_BHS:
    SizeOpTable::kTableAny, SizeOp::kSzQS, uint16_t(B(SizeOp::k00) | B(SizeOp::k00Q) | B(SizeOp::k00S) | B(SizeOp::k01) | B(SizeOp::k01Q) | B(SizeOp::k01S) | B(SizeOp::k10) | B(SizeOp::k10Q) | B(SizeOp::k10S))
  },

  { // kVO_SV_B8H4S2:
    SizeOpTable::kTableAny, SizeOp::kSzQS, uint16_t(B(SizeOp::k00) | B(SizeOp::k00S) | B(SizeOp::k01) | B(SizeOp::k01S) | B(SizeOp::k10) | B(SizeOp::k10S))
  },

  { // kVO_SV_HS:
    SizeOpTable::kTableAny, SizeOp::kSzQS, uint16_t(B(SizeOp::k01) | B(SizeOp::k01Q) | B(SizeOp::k01S) | B(SizeOp::k10) | B(SizeOp::k10Q) | B(SizeOp::k10S))
  },

  { // kVO_V_Any:
    SizeOpTable::kTableAny, SizeOp::kSzQS, uint16_t(B(SizeOp::k00) | B(SizeOp::k00Q) | B(SizeOp::k01) | B(SizeOp::k01Q) | B(SizeOp::k10) | B(SizeOp::k10Q) | B(SizeOp::k11S) | B(SizeOp::k11Q))
  },

  { // kVO_SV_Any:
    SizeOpTable::kTableAny, SizeOp::kSzQS, uint16_t(B(SizeOp::k00) | B(SizeOp::k00Q) | B(SizeOp::k00S) |
                                                    B(SizeOp::k01) | B(SizeOp::k01Q) | B(SizeOp::k01S) |
                                                    B(SizeOp::k10) | B(SizeOp::k10Q) | B(SizeOp::k10S) |
                                                    B(SizeOp::k11) | B(SizeOp::k11Q) | B(SizeOp::k11S))
  }
};

static const Operand_& significantSimdOp(const Operand_& o0, const Operand_& o1, uint32_t instFlags) noexcept {
  return !(instFlags & InstDB::kInstFlagLong) ? o0 : o1;
}

static inline SizeOp armElementTypeToSizeOp(uint32_t vecOpType, RegType regType, uint32_t elementType) noexcept {
  // Instruction data or Assembler is wrong if this triggers an assertion failure.
  ASMJIT_ASSERT(vecOpType < InstDB::kVO_Count);
  // ElementType uses 3 bits in the operand signature, it should never overflow.
  ASMJIT_ASSERT(elementType <= 0x7u);

  const SizeOpMap& map = sizeOpMap[vecOpType];
  const SizeOpTable& table = sizeOpTable[map.tableId];

  size_t index = (Support::min<uint32_t>(diff(regType, RegType::kARM_VecB), diff(RegType::kARM_VecV, RegType::kARM_VecB) + 1) << 3) | elementType;
  SizeOp op = table.array[index];
  SizeOp modifiedOp { uint8_t(op.value & map.sizeOpMask) };

  if (!Support::bitTest(map.acceptMask, op.value))
    modifiedOp.makeInvalid();

  return modifiedOp;
}

// a64::Assembler - Immediate Encoding Utilities (Integral)
// ========================================================

using Utils::LogicalImm;

struct HalfWordImm {
  uint32_t hw;
  uint32_t inv;
  uint32_t imm;
};

struct LMHImm {
  uint32_t lm;
  uint32_t h;
  uint32_t maxRmId;
};

static inline uint32_t countZeroHalfWords64(uint64_t imm) noexcept {
  return uint32_t((imm & 0x000000000000FFFFu) == 0) +
         uint32_t((imm & 0x00000000FFFF0000u) == 0) +
         uint32_t((imm & 0x0000FFFF00000000u) == 0) +
         uint32_t((imm & 0xFFFF000000000000u) == 0) ;
}

static uint32_t encodeMovSequence32(uint32_t out[2], uint32_t imm, uint32_t rd, uint32_t x) noexcept {
  ASMJIT_ASSERT(rd <= 31);

  uint32_t kMovZ = 0b01010010100000000000000000000000 | (x << 31);
  uint32_t kMovN = 0b00010010100000000000000000000000;
  uint32_t kMovK = 0b01110010100000000000000000000000;

  if ((imm & 0xFFFF0000u) == 0x00000000u) {
    out[0] = kMovZ | (0 << 21) | ((imm & 0xFFFFu) << 5) | rd;
    return 1;
  }

  if ((imm & 0xFFFF0000u) == 0xFFFF0000u) {
    out[0] = kMovN | (0 << 21) | ((~imm & 0xFFFFu) << 5) | rd;
    return 1;
  }

  if ((imm & 0x0000FFFFu) == 0x00000000u) {
    out[0] = kMovZ | (1 << 21) | ((imm >> 16) << 5) | rd;
    return 1;
  }

  if ((imm & 0x0000FFFFu) == 0x0000FFFFu) {
    out[0] = kMovN | (1 << 21) | ((~imm >> 16) << 5) | rd;
    return 1;
  }

  out[0] = kMovZ | (0 << 21) | ((imm & 0xFFFFu) << 5) | rd;
  out[1] = kMovK | (1 << 21) | ((imm     >> 16) << 5) | rd;
  return 2;
}

static uint32_t encodeMovSequence64(uint32_t out[4], uint64_t imm, uint32_t rd, uint32_t x) noexcept {
  ASMJIT_ASSERT(rd <= 31);

  uint32_t kMovZ = 0b11010010100000000000000000000000;
  uint32_t kMovN = 0b10010010100000000000000000000000;
  uint32_t kMovK = 0b11110010100000000000000000000000;

  if (imm <= 0xFFFFFFFFu)
    return encodeMovSequence32(out, uint32_t(imm), rd, x);

  uint32_t zhw = countZeroHalfWords64( imm);
  uint32_t ohw = countZeroHalfWords64(~imm);

  if (zhw >= ohw) {
    uint32_t op = kMovZ;
    uint32_t count = 0;

    for (uint32_t hwIndex = 0; hwIndex < 4; hwIndex++, imm >>= 16) {
      uint32_t hwImm = uint32_t(imm & 0xFFFFu);
      if (hwImm == 0)
        continue;

      out[count++] = op | (hwIndex << 21) | (hwImm << 5) | rd;
      op = kMovK;
    }

    // This should not happen - zero should be handled by encodeMovSequence32().
    ASMJIT_ASSERT(count > 0);

    return count;
  }
  else {
    uint32_t op = kMovN;
    uint32_t count = 0;
    uint32_t negMask = 0xFFFFu;

    for (uint32_t hwIndex = 0; hwIndex < 4; hwIndex++, imm >>= 16) {
      uint32_t hwImm = uint32_t(imm & 0xFFFFu);
      if (hwImm == 0xFFFFu)
        continue;

      out[count++] = op | (hwIndex << 21) | ((hwImm ^ negMask) << 5) | rd;
      op = kMovK;
      negMask = 0;
    }

    if (count == 0) {
      out[count++] = kMovN | ((0xFFFF ^ negMask) << 5) | rd;
    }

    return count;
  }
}

static inline bool encodeLMH(uint32_t sizeField, uint32_t elementIndex, LMHImm* out) noexcept {
  if (sizeField != 1 && sizeField != 2)
    return false;

  uint32_t hShift = 3u - sizeField;
  uint32_t lmShift = sizeField - 1u;
  uint32_t maxElementIndex = 15u >> sizeField;

  out->h = elementIndex >> hShift;
  out->lm = (elementIndex << lmShift) & 0x3u;
  out->maxRmId = (8u << sizeField) - 1;

  return elementIndex <= maxElementIndex;
}

// [.......A|B.......|.......C|D.......|.......E|F.......|.......G|H.......]
static inline uint32_t encodeImm64ByteMaskToImm8(uint64_t imm) noexcept {
  return uint32_t(((imm >> (7  - 0)) & 0b00000011) | // [.......G|H.......]
                  ((imm >> (23 - 2)) & 0b00001100) | // [.......E|F.......]
                  ((imm >> (39 - 4)) & 0b00110000) | // [.......C|D.......]
                  ((imm >> (55 - 6)) & 0b11000000)); // [.......A|B.......]
}

// a64::Assembler - Opcode
// =======================

//! Helper class to store and manipulate ARM opcode.
struct Opcode {
  uint32_t v;

  enum Bits : uint32_t {
    kN = (1u << 22),
    kQ = (1u << 30),
    kX = (1u << 31)
  };

  // --------------------------------------------------------------------------
  // [Opcode Builder]
  // --------------------------------------------------------------------------

  inline uint32_t get() const noexcept { return v; }
  inline void reset(uint32_t value) noexcept { v = value; }

  inline bool hasQ() const noexcept { return (v & kQ) != 0; }
  inline bool hasX() const noexcept { return (v & kX) != 0; }

  template<typename T>
  inline Opcode& addImm(T value, uint32_t bitIndex) noexcept { return operator|=(uint32_t(value) << bitIndex); }

  template<typename T>
  inline Opcode& xorImm(T value, uint32_t bitIndex) noexcept { return operator^=(uint32_t(value) << bitIndex); }

  template<typename T, typename Condition>
  inline Opcode& addIf(T value, const Condition& condition) noexcept { return operator|=(condition ? uint32_t(value) : uint32_t(0)); }

  inline Opcode& addLogicalImm(const LogicalImm& logicalImm) noexcept {
    addImm(logicalImm.n, 22);
    addImm(logicalImm.r, 16);
    addImm(logicalImm.s, 10);
    return *this;
  }

  inline Opcode& addReg(uint32_t id, uint32_t bitIndex) noexcept { return operator|=((id & 31u) << bitIndex); }
  inline Opcode& addReg(const Operand_& op, uint32_t bitIndex) noexcept { return addReg(op.id(), bitIndex); }

  inline Opcode& operator=(uint32_t x) noexcept { v = x; return *this; }
  inline Opcode& operator&=(uint32_t x) noexcept { v &= x; return *this; }
  inline Opcode& operator|=(uint32_t x) noexcept { v |= x; return *this; }
  inline Opcode& operator^=(uint32_t x) noexcept { v ^= x; return *this; }

  inline uint32_t operator&(uint32_t x) const noexcept { return v & x; }
  inline uint32_t operator|(uint32_t x) const noexcept { return v | x; }
  inline uint32_t operator^(uint32_t x) const noexcept { return v ^ x; }
};

// a64::Assembler - Signature Utilities
// ====================================

// TODO: [ARM] Deprecate matchSignature.
static inline bool matchSignature(const Operand_& o0, const Operand_& o1, uint32_t instFlags) noexcept {
  if (!(instFlags & (InstDB::kInstFlagLong | InstDB::kInstFlagNarrow)))
    return o0.signature() == o1.signature();

  // TODO: [ARM] Something smart to validate this.
  return true;
}

static inline bool matchSignature(const Operand_& o0, const Operand_& o1, const Operand_& o2, uint32_t instFlags) noexcept {
  return matchSignature(o0, o1, instFlags) && o1.signature() == o2.signature();
}

static inline bool matchSignature(const Operand_& o0, const Operand_& o1, const Operand_& o2, const Operand_& o3, uint32_t instFlags) noexcept {
  return matchSignature(o0, o1, instFlags) && o1.signature() == o2.signature() && o2.signature() == o3.signature();;
}

// Memory must be either:
// 1. Absolute address, which will be converted to relative.
// 2. Relative displacement (Label).
// 3. Base register + either offset or index.
static inline bool armCheckMemBaseIndexRel(const Mem& mem) noexcept {
  // Allowed base types (Nothing, Label, and GpX).
  constexpr uint32_t kBaseMask  = B(0) |
                                  B(RegType::kLabelTag) |
                                  B(RegType::kARM_GpX);

  // Allowed index types (Nothing, GpW, and GpX).
  constexpr uint32_t kIndexMask = B(0) |
                                  B(RegType::kARM_GpW) |
                                  B(RegType::kARM_GpX) ;

  RegType baseType = mem.baseType();
  RegType indexType = mem.indexType();

  if (!Support::bitTest(kBaseMask, baseType))
    return false;

  if (baseType > RegType::kLabelTag) {
    // Index allows either GpW or GpX.
    if (!Support::bitTest(kIndexMask, indexType))
      return false;

    if (indexType == RegType::kNone)
      return true;
    else
      return !mem.hasOffset();
  }
  else {
    // No index register allowed if this is a PC relative address (literal).
    return indexType == RegType::kNone;
  }
}

struct EncodeFpOpcodeBits {
  uint32_t sizeMask;
  uint32_t mask[3];
};

static inline bool pickFpOpcode(const Vec& reg, uint32_t sOp, uint32_t sHf, uint32_t vOp, uint32_t vHf, Opcode* opcode, uint32_t* szOut) noexcept {
  static constexpr uint32_t kQBitIndex = 30;

  static const EncodeFpOpcodeBits szBits[InstDB::kHF_Count] = {
    { B(2) | B(1)       , { 0u                           , 0u, B(22) } },
    { B(2) | B(1) | B(0), { 0u                           , 0u, 0u    } },
    { B(2) | B(1) | B(0), { B(23) | B(22)                , 0u, B(22) } },
    { B(2) | B(1) | B(0), { B(22) | B(20) | B(19)        , 0u, B(22) } },
    { B(2) | B(1) | B(0), { B(22) | B(21) | B(15) | B(14), 0u, B(22) } },
    { B(2) | B(1) | B(0), { B(23)                        , 0u, B(22) } }
  };

  if (!reg.hasElementType()) {
    // Scalar operation [HSD].
    uint32_t sz = diff(reg.type(), RegType::kARM_VecH);
    if (sz > 2u || !Support::bitTest(szBits[sHf].sizeMask, sz))
      return false;

    opcode->reset(szBits[sHf].mask[sz] ^ sOp);
    *szOut = sz;
    return sOp != 0;
  }
  else {
    // Vector operation [HSD].
    uint32_t q = diff(reg.type(), RegType::kARM_VecD);
    uint32_t sz = reg.elementType() - Vec::kElementTypeH;

    if (q > 1u || sz > 2u || !Support::bitTest(szBits[vHf].sizeMask, sz))
      return false;

    opcode->reset(szBits[vHf].mask[sz] ^ (vOp | (q << kQBitIndex)));
    *szOut = sz;
    return vOp != 0;
  }
}

static inline bool pickFpOpcode(const Vec& reg, uint32_t sOp, uint32_t sHf, uint32_t vOp, uint32_t vHf, Opcode* opcode) noexcept {
  uint32_t sz;
  return pickFpOpcode(reg, sOp, sHf, vOp, vHf, opcode, &sz);
}

// a64::Assembler - Operand Checks
// ===============================

// Checks whether all operands have the same signature.
static inline bool checkSignature(const Operand_& o0, const Operand_& o1) noexcept {
  return o0.signature() == o1.signature();
}

static inline bool checkSignature(const Operand_& o0, const Operand_& o1, const Operand_& o2) noexcept {
  return o0.signature() == o1.signature() &&
         o1.signature() == o2.signature();
}

static inline bool checkSignature(const Operand_& o0, const Operand_& o1, const Operand_& o2, const Operand_& o3) noexcept {
  return o0.signature() == o1.signature() &&
         o1.signature() == o2.signature() &&
         o2.signature() == o3.signature();
}

// Checks whether the register is GP register of the allowed types.
//
// Allowed is a 2-bit mask, where the first bits allows GpW and the second bit
// allows GpX. These bits are usually stored within the instruction, but could
// be also hardcoded in the assembler for instructions where GP types are not
// selectable.
static inline bool checkGpType(const Operand_& op, uint32_t allowed) noexcept {
  RegType type = op.as<Reg>().type();
  return Support::bitTest(allowed << uint32_t(RegType::kARM_GpW), type);
}

static inline bool checkGpType(const Operand_& op, uint32_t allowed, uint32_t* x) noexcept {
  // NOTE: We set 'x' to one only when GpW is allowed, otherwise the X is part
  // of the opcode and we cannot set it. This is why this works without requiring
  // additional logic.
  RegType type = op.as<Reg>().type();
  *x = diff(type, RegType::kARM_GpW) & allowed;
  return Support::bitTest(allowed << uint32_t(RegType::kARM_GpW), type);
}

static inline bool checkGpType(const Operand_& o0, const Operand_& o1, uint32_t allowed, uint32_t* x) noexcept {
  return checkGpType(o0, allowed, x) && checkSignature(o0, o1);
}

static inline bool checkGpType(const Operand_& o0, const Operand_& o1, const Operand_& o2, uint32_t allowed, uint32_t* x) noexcept {
  return checkGpType(o0, allowed, x) && checkSignature(o0, o1, o2);
}

static inline bool checkGpId(const Operand_& op, uint32_t hiId = kZR) noexcept {
  uint32_t id = op.as<Reg>().id();
  return id < 31u || id == hiId;
}

static inline bool checkGpId(const Operand_& o0, const Operand_& o1, uint32_t hiId = kZR) noexcept {
  uint32_t id0 = o0.as<Reg>().id();
  uint32_t id1 = o1.as<Reg>().id();

  return (id0 < 31u || id0 == hiId) && (id1 < 31u || id1 == hiId);
}

static inline bool checkGpId(const Operand_& o0, const Operand_& o1, const Operand_& o2, uint32_t hiId = kZR) noexcept {
  uint32_t id0 = o0.as<Reg>().id();
  uint32_t id1 = o1.as<Reg>().id();
  uint32_t id2 = o2.as<Reg>().id();

  return (id0 < 31u || id0 == hiId) && (id1 < 31u || id1 == hiId) && (id2 < 31u || id2 == hiId);
}

static inline bool checkVecId(const Operand_& op) noexcept {
  uint32_t id = op.as<Reg>().id();
  return id <= 31u;
}

static inline bool checkVecId(const Operand_& o0, const Operand_& o1) noexcept {
  uint32_t id0 = o0.as<Reg>().id();
  uint32_t id1 = o1.as<Reg>().id();

  return (id0 | id1) <= 31u;
}

/* Unused at the moment.
static inline bool checkVecId(const Operand_& o0, const Operand_& o1, const Operand_& o2) noexcept {
  uint32_t id0 = o0.as<Reg>().id();
  uint32_t id1 = o1.as<Reg>().id();
  uint32_t id2 = o2.as<Reg>().id();

  return (id0 | id1 | id2) <= 31u;
}

static inline bool checkVecId(const Operand_& o0, const Operand_& o1, const Operand_& o2, const Operand_& o3) noexcept {
  uint32_t id0 = o0.as<Reg>().id();
  uint32_t id1 = o1.as<Reg>().id();
  uint32_t id2 = o2.as<Reg>().id();
  uint32_t id3 = o3.as<Reg>().id();

  return (id0 | id1 | id2 | id3) <= 31u;
}
*/

static inline bool checkMemBase(const Mem& mem) noexcept {
  return mem.baseType() == RegType::kARM_GpX && mem.baseId() <= 31;
}

static inline bool checkEven(const Operand_& o0, const Operand_& o1) noexcept {
  return ((o0.id() | o1.id()) & 1) == 0;
}

static inline bool checkConsecutive(const Operand_& o0, const Operand_& o1) noexcept {
  return ((o0.id() + 1u) & 0x1Fu) == o1.id();
}

static inline bool checkConsecutive(const Operand_& o0, const Operand_& o1, const Operand_& o2) noexcept {
  return ((o0.id() + 1u) & 0x1Fu) == o1.id() &&
         ((o0.id() + 2u) & 0x1Fu) == o2.id();
}

static inline bool checkConsecutive(const Operand_& o0, const Operand_& o1, const Operand_& o2, const Operand_& o3) noexcept {
  return ((o0.id() + 1u) & 0x1Fu) == o1.id() &&
         ((o0.id() + 2u) & 0x1Fu) == o2.id() &&
         ((o0.id() + 3u) & 0x1Fu) == o3.id();
}

// a64::Assembler - CheckReg
// =========================

#define V(index) (index == uint32_t(RegType::kARM_GpW)  ? Gp::kIdZr :  \
                  index == uint32_t(RegType::kARM_GpX)  ? Gp::kIdZr :  \
                  index == uint32_t(RegType::kARM_VecB) ? 31u       :  \
                  index == uint32_t(RegType::kARM_VecH) ? 31u       :  \
                  index == uint32_t(RegType::kARM_VecS) ? 31u       :  \
                  index == uint32_t(RegType::kARM_VecD) ? 31u       :  \
                  index == uint32_t(RegType::kARM_VecV) ? 31u       : 0)
static const Support::Array<uint8_t, 32> commonHiRegIdOfType = {{
  ASMJIT_LOOKUP_TABLE_32(V, 0)
}};
#undef V

static inline bool checkValidRegs(const Operand_& o0) noexcept {
  return bool(unsigned(o0.id() < 31) | unsigned(o0.id() == commonHiRegIdOfType[o0.as<Reg>().type()]));
}

static inline bool checkValidRegs(const Operand_& o0, const Operand_& o1) noexcept {
  return bool((unsigned(o0.id() < 31) | unsigned(o0.id() == commonHiRegIdOfType[o0.as<Reg>().type()])) &
              (unsigned(o1.id() < 31) | unsigned(o1.id() == commonHiRegIdOfType[o1.as<Reg>().type()])));
}

static inline bool checkValidRegs(const Operand_& o0, const Operand_& o1, const Operand_& o2) noexcept {
  return bool((unsigned(o0.id() < 31) | unsigned(o0.id() == commonHiRegIdOfType[o0.as<Reg>().type()])) &
              (unsigned(o1.id() < 31) | unsigned(o1.id() == commonHiRegIdOfType[o1.as<Reg>().type()])) &
              (unsigned(o2.id() < 31) | unsigned(o2.id() == commonHiRegIdOfType[o2.as<Reg>().type()])));
}

static inline bool checkValidRegs(const Operand_& o0, const Operand_& o1, const Operand_& o2, const Operand_& o3) noexcept {
  return bool((unsigned(o0.id() < 31) | unsigned(o0.id() == commonHiRegIdOfType[o0.as<Reg>().type()])) &
              (unsigned(o1.id() < 31) | unsigned(o1.id() == commonHiRegIdOfType[o1.as<Reg>().type()])) &
              (unsigned(o2.id() < 31) | unsigned(o2.id() == commonHiRegIdOfType[o2.as<Reg>().type()])) &
              (unsigned(o3.id() < 31) | unsigned(o3.id() == commonHiRegIdOfType[o3.as<Reg>().type()])));
}

// a64::Assembler - Construction & Destruction
// ===========================================

Assembler::Assembler(CodeHolder* code) noexcept : BaseAssembler() {
  _archMask = uint64_t(1) << uint32_t(Arch::kAArch64);
  assignEmitterFuncs(this);

  if (code)
    code->attach(this);
}

Assembler::~Assembler() noexcept {}

// a64::Assembler - Emit
// =====================

#define ENC_OPS1(OP0) \
  (uint32_t(OperandType::k##OP0))

#define ENC_OPS2(OP0, OP1) \
  (uint32_t(OperandType::k##OP0) + \
  (uint32_t(OperandType::k##OP1) << 3))

#define ENC_OPS3(OP0, OP1, OP2) \
  (uint32_t(OperandType::k##OP0) + \
  (uint32_t(OperandType::k##OP1) << 3) + \
  (uint32_t(OperandType::k##OP2) << 6))

#define ENC_OPS4(OP0, OP1, OP2, OP3) \
  (uint32_t(OperandType::k##OP0) + \
  (uint32_t(OperandType::k##OP1) << 3) + \
  (uint32_t(OperandType::k##OP2) << 6) + \
  (uint32_t(OperandType::k##OP3) << 9))

Error Assembler::_emit(InstId instId, const Operand_& o0, const Operand_& o1, const Operand_& o2, const Operand_* opExt) {
  // Logging/Validation/Error.
  constexpr InstOptions kRequiresSpecialHandling = InstOptions::kReserved;

  Error err;
  CodeWriter writer(this);

  // Combine all instruction options and also check whether the instruction
  // is valid. All options that require special handling (including invalid
  // instruction) are handled by the next branch.
  InstOptions options = InstOptions(instId - 1 >= Inst::_kIdCount - 1) | InstOptions((size_t)(_bufferEnd - writer.cursor()) < 4) | instOptions() | forcedInstOptions();

  CondCode instCC = BaseInst::extractARMCondCode(instId);
  instId = instId & uint32_t(InstIdParts::kRealId);

  if (instId >= Inst::_kIdCount)
    instId = 0;

  const InstDB::InstInfo* instInfo = &InstDB::_instInfoTable[instId];
  uint32_t encodingIndex = instInfo->_encodingDataIndex;

  Opcode opcode;
  uint32_t isign4;
  uint32_t instFlags;

  const Operand_& o3 = opExt[EmitterUtils::kOp3];
  const Operand_* rmRel = nullptr;

  uint32_t multipleOpData[4];
  uint32_t multipleOpCount;

  // These are only used when instruction uses a relative displacement.
  OffsetFormat offsetFormat;     // Offset format.
  uint64_t offsetValue;          // Offset value (if known).

  if (ASMJIT_UNLIKELY(Support::test(options, kRequiresSpecialHandling))) {
    if (ASMJIT_UNLIKELY(!_code))
      return reportError(DebugUtils::errored(kErrorNotInitialized));

    // Unknown instruction.
    if (ASMJIT_UNLIKELY(instId == 0))
      goto InvalidInstruction;

    // Condition code can only be used with 'B' instruction.
    if (ASMJIT_UNLIKELY(instCC != CondCode::kAL && instId != Inst::kIdB))
      goto InvalidInstruction;

    // Grow request, happens rarely.
    err = writer.ensureSpace(this, 4);
    if (ASMJIT_UNLIKELY(err))
      goto Failed;

#ifndef ASMJIT_NO_VALIDATION
    // Strict validation.
    if (hasDiagnosticOption(DiagnosticOptions::kValidateAssembler)) {
      Operand_ opArray[Globals::kMaxOpCount];
      EmitterUtils::opArrayFromEmitArgs(opArray, o0, o1, o2, opExt);

      err = _funcs.validate(arch(), BaseInst(instId, options, _extraReg), opArray, Globals::kMaxOpCount, ValidationFlags::kNone);
      if (ASMJIT_UNLIKELY(err))
        goto Failed;
    }
#endif
  }

  // Signature of the first 4 operands.
  isign4 = (uint32_t(o0.opType())     ) +
           (uint32_t(o1.opType()) << 3) +
           (uint32_t(o2.opType()) << 6) +
           (uint32_t(o3.opType()) << 9);
  instFlags = instInfo->flags();

  switch (instInfo->_encoding) {
    // ------------------------------------------------------------------------
    // [Base - Universal]
    // ------------------------------------------------------------------------

    case InstDB::kEncodingBaseOp: {
      const InstDB::EncodingData::BaseOp& opData = InstDB::EncodingData::baseOp[encodingIndex];

      if (isign4 == 0) {
        opcode.reset(opData.opcode);
        goto EmitOp;
      }

      break;
    }

    case InstDB::kEncodingBaseOpImm: {
      const InstDB::EncodingData::BaseOpImm& opData = InstDB::EncodingData::baseOpImm[encodingIndex];

      if (isign4 == ENC_OPS1(Imm)) {
        uint64_t imm = o0.as<Imm>().valueAs<uint64_t>();
        uint32_t immMax = 1u << opData.immBits;

        if (imm >= immMax)
          goto InvalidImmediate;

        opcode.reset(opData.opcode);
        opcode.addImm(imm, opData.immOffset);
        goto EmitOp;
      }

      break;
    }

    case InstDB::kEncodingBaseR: {
      const InstDB::EncodingData::BaseR& opData = InstDB::EncodingData::baseR[encodingIndex];

      if (isign4 == ENC_OPS1(Reg)) {
        if (!checkGpType(o0, opData.rType))
          goto InvalidInstruction;

        if (!checkGpId(o0, opData.rHiId))
          goto InvalidPhysId;

        opcode.reset(opData.opcode);
        opcode.addReg(o0, opData.rShift);
        goto EmitOp;
      }

      break;
    }

    case InstDB::kEncodingBaseRR: {
      const InstDB::EncodingData::BaseRR& opData = InstDB::EncodingData::baseRR[encodingIndex];

      if (isign4 == ENC_OPS2(Reg, Reg)) {
        uint32_t x;
        if (!checkGpType(o0, opData.aType, &x))
          goto InvalidInstruction;

        if (!checkGpType(o1, opData.bType))
          goto InvalidInstruction;

        if (opData.uniform && !checkSignature(o0, o1))
          goto InvalidInstruction;

        if (!checkGpId(o0, opData.aHiId))
          goto InvalidPhysId;

        if (!checkGpId(o1, opData.bHiId))
          goto InvalidPhysId;

        opcode.reset(opData.opcode);
        opcode.addImm(x, 31);
        opcode.addReg(o1, opData.bShift);
        opcode.addReg(o0, opData.aShift);
        goto EmitOp;
      }

      break;
    }

    case InstDB::kEncodingBaseRRR: {
      const InstDB::EncodingData::BaseRRR& opData = InstDB::EncodingData::baseRRR[encodingIndex];

      if (isign4 == ENC_OPS3(Reg, Reg, Reg)) {
        uint32_t x;
        if (!checkGpType(o0, opData.aType, &x))
          goto InvalidInstruction;

        if (!checkGpType(o1, opData.bType))
          goto InvalidInstruction;

        if (!checkGpType(o2, opData.cType))
          goto InvalidInstruction;

        if (opData.uniform && !checkSignature(o0, o1, o2))
          goto InvalidInstruction;

        if (!checkGpId(o0, opData.aHiId))
          goto InvalidPhysId;

        if (!checkGpId(o1, opData.bHiId))
          goto InvalidPhysId;

        if (!checkGpId(o2, opData.cHiId))
          goto InvalidPhysId;

        opcode.reset(opData.opcode());
        opcode.addImm(x, 31);
        opcode.addReg(o2, 16);
        opcode.addReg(o1, 5);
        opcode.addReg(o0, 0);
        goto EmitOp;
      }

      break;
    }

    case InstDB::kEncodingBaseRRRR: {
      const InstDB::EncodingData::BaseRRRR& opData = InstDB::EncodingData::baseRRRR[encodingIndex];

      if (isign4 == ENC_OPS4(Reg, Reg, Reg, Reg)) {
        uint32_t x;
        if (!checkGpType(o0, opData.aType, &x))
          goto InvalidInstruction;

        if (!checkGpType(o1, opData.bType))
          goto InvalidInstruction;

        if (!checkGpType(o2, opData.cType))
          goto InvalidInstruction;

        if (!checkGpType(o3, opData.dType))
          goto InvalidInstruction;

        if (opData.uniform && !checkSignature(o0, o1, o2, o3))
          goto InvalidInstruction;

        if (!checkGpId(o0, opData.aHiId))
          goto InvalidPhysId;

        if (!checkGpId(o1, opData.bHiId))
          goto InvalidPhysId;

        if (!checkGpId(o2, opData.cHiId))
          goto InvalidPhysId;

        if (!checkGpId(o3, opData.dHiId))
          goto InvalidPhysId;

        opcode.reset(opData.opcode());
        opcode.addImm(x, 31);
        opcode.addReg(o2, 16);
        opcode.addReg(o3, 10);
        opcode.addReg(o1, 5);
        opcode.addReg(o0, 0);
        goto EmitOp;
      }

      break;
    }

    case InstDB::kEncodingBaseRRII: {
      const InstDB::EncodingData::BaseRRII& opData = InstDB::EncodingData::baseRRII[encodingIndex];

      if (isign4 == ENC_OPS4(Reg, Reg, Imm, Imm)) {
        if (!checkGpType(o0, opData.aType))
          goto InvalidInstruction;

        if (!checkGpType(o1, opData.bType))
          goto InvalidInstruction;

        if (!checkGpId(o0, opData.aHiId))
          goto InvalidPhysId;

        if (!checkGpId(o1, opData.bHiId))
          goto InvalidPhysId;

        if (o2.as<Imm>().valueAs<uint64_t>() >= Support::bitMask(opData.aImmSize + opData.aImmDiscardLsb) ||
            o3.as<Imm>().valueAs<uint64_t>() >= Support::bitMask(opData.bImmSize + opData.bImmDiscardLsb))
          goto InvalidImmediate;

        uint32_t aImm = o2.as<Imm>().valueAs<uint32_t>() >> opData.aImmDiscardLsb;
        uint32_t bImm = o3.as<Imm>().valueAs<uint32_t>() >> opData.bImmDiscardLsb;

        if ((aImm << opData.aImmDiscardLsb) != o2.as<Imm>().valueAs<uint32_t>() ||
            (bImm << opData.bImmDiscardLsb) != o3.as<Imm>().valueAs<uint32_t>())
          goto InvalidImmediate;

        opcode.reset(opData.opcode());
        opcode.addImm(aImm, opData.aImmOffset);
        opcode.addImm(bImm, opData.bImmOffset);
        opcode.addReg(o1, 5);
        opcode.addReg(o0, 0);
        goto EmitOp;
      }

      break;
    }

    // ------------------------------------------------------------------------
    // [Base - Mov]
    // ------------------------------------------------------------------------

    case InstDB::kEncodingBaseMov: {
      // MOV is a pseudo instruction that uses various instructions depending on its signature.
      uint32_t x = diff(o0.as<Reg>().type(), RegType::kARM_GpW);
      if (x > 1)
        goto InvalidInstruction;

      if (isign4 == ENC_OPS2(Reg, Reg)) {
        if (!o0.as<Reg>().isGp())
          goto InvalidInstruction;

        if (!checkSignature(o0, o1))
          goto InvalidInstruction;

        bool hasSP = o0.as<Gp>().isSP() || o1.as<Gp>().isSP();
        if (hasSP) {
          // Cannot be combined with ZR.
          if (!checkGpId(o0, o1, kSP))
            goto InvalidPhysId;

          // MOV Rd, Rm -> ADD Rd, Rn, #0.
          opcode.reset(0b00010001000000000000000000000000);
          opcode.addImm(x, 31);
          opcode.addReg(o1, 5);
          opcode.addReg(o0, 0);
          goto EmitOp;
        }
        else {
          if (!checkGpId(o0, o1, kZR))
            goto InvalidPhysId;

          // MOV Rd, Rm -> ORR Rd, <ZR>, Rm.
          opcode.reset(0b00101010000000000000001111100000);
          opcode.addImm(x, 31);
          opcode.addReg(o1, 16);
          opcode.addReg(o0, 0);
          goto EmitOp;
        }
      }

      if (isign4 == ENC_OPS2(Reg, Imm)) {
        if (!o0.as<Reg>().isGp())
          goto InvalidInstruction;

        uint64_t immValue = o1.as<Imm>().valueAs<uint64_t>();
        if (!x)
          immValue &= 0xFFFFFFFFu;

        // Prefer a single MOVN/MOVZ instruction over a logical instruction.
        multipleOpCount = encodeMovSequence64(multipleOpData, immValue, o0.id() & 31, x);
        if (multipleOpCount == 1 && !o0.as<Gp>().isSP()) {
          opcode.reset(multipleOpData[0]);
          goto EmitOp;
        }

        // Logical instructions use 13-bit immediate pattern encoded as N:ImmR:ImmS.
        LogicalImm logicalImm;
        if (!o0.as<Gp>().isZR()) {
          if (Utils::encodeLogicalImm(immValue, x ? 64 : 32, &logicalImm)) {
            if (!checkGpId(o0, kSP))
              goto InvalidPhysId;

            opcode.reset(0b00110010000000000000001111100000);
            opcode.addImm(x, 31);
            opcode.addLogicalImm(logicalImm);
            opcode.addReg(o0, 0);
            goto EmitOp;
          }
        }

        if (!checkGpId(o0, kZR))
          goto InvalidPhysId;

        goto EmitOp_Multiple;
      }

      break;
    }

    case InstDB::kEncodingBaseMovKNZ: {
      const InstDB::EncodingData::BaseMovKNZ& opData = InstDB::EncodingData::baseMovKNZ[encodingIndex];

      uint32_t x = diff(o0.as<Reg>().type(), RegType::kARM_GpW);
      if (x > 1)
        goto InvalidInstruction;

      if (!checkGpId(o0, kZR))
        goto InvalidPhysId;

      opcode.reset(opData.opcode);
      opcode.addImm(x, 31);

      if (isign4 == ENC_OPS2(Reg, Imm)) {
        uint64_t imm16 = o1.as<Imm>().valueAs<uint64_t>();
        if (imm16 > 0xFFFFu)
          goto InvalidImmediate;

        opcode.addImm(imm16, 5);
        opcode.addReg(o0, 0);
        goto EmitOp;
      }

      if (isign4 == ENC_OPS3(Reg, Imm, Imm)) {
        uint64_t imm16 = o1.as<Imm>().valueAs<uint64_t>();
        uint32_t shiftType = o2.as<Imm>().predicate();
        uint64_t shiftValue = o2.as<Imm>().valueAs<uint64_t>();

        if (imm16 > 0xFFFFu || shiftValue > 48 || shiftType != uint32_t(ShiftOp::kLSL))
          goto InvalidImmediate;

        // Convert shift value to 'hw' field.
        uint32_t hw = uint32_t(shiftValue) >> 4;
        if ((hw << 4) != uint32_t(shiftValue))
          goto InvalidImmediate;

        opcode.addImm(hw, 21);
        opcode.addImm(imm16, 5);
        opcode.addReg(o0, 0);

        if (!x && hw > 1u)
          goto InvalidImmediate;

        goto EmitOp;
      }

      break;
    }

    // ------------------------------------------------------------------------
    // [Base - Adr]
    // ------------------------------------------------------------------------

    case InstDB::kEncodingBaseAdr: {
      const InstDB::EncodingData::BaseAdr& opData = InstDB::EncodingData::baseAdr[encodingIndex];

      if (isign4 == ENC_OPS2(Reg, Label) || isign4 == ENC_OPS2(Reg, Imm)) {
        if (!o0.as<Reg>().isGpX())
          goto InvalidInstruction;

        if (!checkGpId(o0, kZR))
          goto InvalidPhysId;

        opcode.reset(opData.opcode());
        opcode.addReg(o0, 0);
        offsetFormat.resetToImmValue(opData.offsetType, 4, 5, 21, 0);

        if (instId == Inst::kIdAdrp)
          offsetFormat._immDiscardLsb = 12;

        rmRel = &o1;
        goto EmitOp_Rel;
      }

      break;
    }

    // ------------------------------------------------------------------------
    // [Base - Arithmetic and Logical]
    // ------------------------------------------------------------------------

    case InstDB::kEncodingBaseAddSub: {
      const InstDB::EncodingData::BaseAddSub& opData = InstDB::EncodingData::baseAddSub[encodingIndex];

      uint32_t x;
      if (!checkGpType(o0, o1, kWX, &x))
        goto InvalidInstruction;

      if (isign4 == ENC_OPS3(Reg, Reg, Imm) || isign4 == ENC_OPS4(Reg, Reg, Imm, Imm)) {
        opcode.reset(uint32_t(opData.immediateOp) << 24);

        // ADD | SUB (immediate) - ZR is not allowed.
        // ADDS|SUBS (immediate) - ZR allowed in Rd, SP allowed in Rn.
        uint32_t aHiId = opcode.get() & B(29) ? kZR : kSP;
        uint32_t bHiId = kSP;

        if (!checkGpId(o0, aHiId) || !checkGpId(o1, bHiId))
          goto InvalidPhysId;

        // ADD|SUB (immediate) use 12-bit immediate optionally shifted by 'LSL #12'.
        uint64_t imm = o2.as<Imm>().valueAs<uint64_t>();
        uint32_t shift = 0;

        if (isign4 == ENC_OPS4(Reg, Reg, Imm, Imm)) {
          if (o3.as<Imm>().predicate() != uint32_t(ShiftOp::kLSL))
            goto InvalidImmediate;

          if (o3.as<Imm>().value() != 0 && o3.as<Imm>().value() != 12)
            goto InvalidImmediate;

          shift = uint32_t(o3.as<Imm>().value() != 0);
        }

        // Accept immediate value of '0x00XXX000' by setting 'shift' to 12.
        if (imm > 0xFFFu) {
          if (shift || (imm & ~uint64_t(0xFFFu << 12)) != 0)
            goto InvalidImmediate;
          shift = 1;
          imm >>= 12;
        }

        opcode.addImm(x, 31);
        opcode.addImm(shift, 22);
        opcode.addImm(imm, 10);
        opcode.addReg(o1, 5);
        opcode.addReg(o0, 0);
        goto EmitOp;
      }

      if (isign4 == ENC_OPS3(Reg, Reg, Reg) || isign4 == ENC_OPS4(Reg, Reg, Reg, Imm)) {
        if (!checkSignature(o1, o2))
          goto InvalidInstruction;

        uint32_t opSize = x ? 64 : 32;
        uint64_t shift = 0;
        uint32_t sType = uint32_t(ShiftOp::kLSL);

        if (isign4 == ENC_OPS4(Reg, Reg, Reg, Imm)) {
          sType = o3.as<Imm>().predicate();
          shift = o3.as<Imm>().valueAs<uint64_t>();
        }

        if (!checkGpId(o2, kZR))
          goto InvalidPhysId;

        // Shift operation - LSL, LSR, ASR.
        if (sType <= uint32_t(ShiftOp::kASR)) {
          bool hasSP = o0.as<Gp>().isSP() || o1.as<Gp>().isSP();
          if (!hasSP) {
            if (!checkGpId(o0, o1, kZR))
              goto InvalidPhysId;

            if (shift >= opSize)
              goto InvalidImmediate;

            opcode.reset(uint32_t(opData.shiftedOp) << 21);
            opcode.addImm(x, 31);
            opcode.addImm(sType, 22);
            opcode.addReg(o2, 16);
            opcode.addImm(shift, 10);
            opcode.addReg(o1, 5);
            opcode.addReg(o0, 0);
            goto EmitOp;
          }

          // SP register can only be used with LSL or Extend.
          if (sType != uint32_t(ShiftOp::kLSL))
            goto InvalidImmediate;
          sType = x ? uint32_t(ShiftOp::kUXTX) : uint32_t(ShiftOp::kUXTW);
        }

        // Extend operation - UXTB, UXTH, UXTW, UXTX, SXTB, SXTH, SXTW, SXTX.
        opcode.reset(uint32_t(opData.extendedOp) << 21);
        sType -= uint32_t(ShiftOp::kUXTB);

        if (sType > 7 || shift > 4)
          goto InvalidImmediate;

        if (!(opcode.get() & B(29))) {
          // ADD|SUB (extend) - ZR is not allowed.
          if (!checkGpId(o0, o1, kSP))
            goto InvalidPhysId;
        }
        else {
          // ADDS|SUBS (extend) - ZR allowed in Rd, SP allowed in Rn.
          if (!checkGpId(o0, kZR) || !checkGpId(o1, kSP))
            goto InvalidPhysId;
        }

        opcode.addImm(x, 31);
        opcode.addReg(o2, 16);
        opcode.addImm(sType, 13);
        opcode.addImm(shift, 10);
        opcode.addReg(o1, 5);
        opcode.addReg(o0, 0);
        goto EmitOp;
      }

      break;
    }

    case InstDB::kEncodingBaseLogical: {
      const InstDB::EncodingData::BaseLogical& opData = InstDB::EncodingData::baseLogical[encodingIndex];

      uint32_t x;
      if (!checkGpType(o0, o1, kWX, &x))
        goto InvalidInstruction;

      if (!checkSignature(o0, o1))
        goto InvalidInstruction;

      uint32_t opSize = x ? 64 : 32;

      if (isign4 == ENC_OPS3(Reg, Reg, Imm) && opData.immediateOp != 0) {
        opcode.reset(uint32_t(opData.immediateOp) << 23);

        // AND|ANDS|BIC|BICS|ORR|EOR (immediate) uses a LogicalImm format described by N:R:S values.
        uint64_t immMask = Support::lsbMask<uint64_t>(opSize);
        uint64_t immValue = o2.as<Imm>().valueAs<uint64_t>();

        if (opData.negateImm)
          immValue ^= immMask;

        // Logical instructions use 13-bit immediate pattern encoded as N:ImmS:ImmR.
        LogicalImm logicalImm;
        if (!Utils::encodeLogicalImm(immValue & immMask, opSize, &logicalImm))
          goto InvalidImmediate;

        // AND|BIC|ORR|EOR (immediate) can have SP on destination, but ANDS|BICS (immediate) cannot.
        uint32_t kOpANDS = 0x3 << 29;
        bool isANDS = (opcode.get() & kOpANDS) == kOpANDS;

        if (!checkGpId(o0, isANDS ? kZR : kSP) || !checkGpId(o1, kZR))
          goto InvalidPhysId;

        opcode.addImm(x, 31);
        opcode.addLogicalImm(logicalImm);
        opcode.addReg(o1, 5);
        opcode.addReg(o0, 0);
        goto EmitOp;
      }

      if (!checkSignature(o1, o2))
        goto InvalidInstruction;

      if (isign4 == ENC_OPS3(Reg, Reg, Reg)) {
        if (!checkGpId(o0, o1, o2, kZR))
          goto InvalidPhysId;

        opcode.reset(uint32_t(opData.shiftedOp) << 21);
        opcode.addImm(x, 31);
        opcode.addReg(o2, 16);
        opcode.addReg(o1, 5);
        opcode.addReg(o0, 0);
        goto EmitOp;
      }

      if (isign4 == ENC_OPS4(Reg, Reg, Reg, Imm)) {
        if (!checkGpId(o0, o1, o2, kZR))
          goto InvalidPhysId;

        uint32_t shiftType = o3.as<Imm>().predicate();
        uint64_t opShift = o3.as<Imm>().valueAs<uint64_t>();

        if (shiftType > 0x3 || opShift >= opSize)
          goto InvalidImmediate;

        opcode.reset(uint32_t(opData.shiftedOp) << 21);
        opcode.addImm(x, 31);
        opcode.addImm(shiftType, 22);
        opcode.addReg(o2, 16);
        opcode.addImm(opShift, 10);
        opcode.addReg(o1, 5);
        opcode.addReg(o0, 0);
        goto EmitOp;
      }

      break;
    }

    case InstDB::kEncodingBaseCmpCmn: {
      const InstDB::EncodingData::BaseCmpCmn& opData = InstDB::EncodingData::baseCmpCmn[encodingIndex];

      uint32_t x;
      if (!checkGpType(o0, kWX, &x))
        goto InvalidInstruction;

      if (isign4 == ENC_OPS2(Reg, Imm)) {
        // CMN|CMP (immediate) - ZR is not allowed.
        if (!checkGpId(o0, kSP))
          goto InvalidPhysId;

        // CMN|CMP (immediate) use 12-bit immediate optionally shifted by 'LSL #12'.
        const Imm& imm12 = o1.as<Imm>();
        uint32_t immShift = 0;
        uint64_t immValue = imm12.valueAs<uint64_t>();

        if (immValue > 0xFFFu) {
          if ((immValue & ~uint64_t(0xFFFu << 12)) != 0)
            goto InvalidImmediate;
          immShift = 1;
          immValue >>= 12;
        }

        opcode.reset(uint32_t(opData.immediateOp) << 24);
        opcode.addImm(x, 31);
        opcode.addImm(immShift, 22);
        opcode.addImm(immValue, 10);
        opcode.addReg(o0, 5);
        opcode.addReg(Gp::kIdZr, 0);
        goto EmitOp;
      }

      if (isign4 == ENC_OPS2(Reg, Reg) || isign4 == ENC_OPS3(Reg, Reg, Imm)) {
        if (!checkSignature(o0, o1))
          goto InvalidInstruction;

        uint32_t opSize = x ? 64 : 32;
        uint32_t sType = 0;
        uint64_t shift = 0;

        if (isign4 == ENC_OPS3(Reg, Reg, Imm)) {
          sType = o2.as<Imm>().predicate();
          shift = o2.as<Imm>().valueAs<uint64_t>();
        }

        bool hasSP = o0.as<Gp>().isSP() || o1.as<Gp>().isSP();

        // Shift operation - LSL, LSR, ASR.
        if (sType <= uint32_t(ShiftOp::kASR)) {
          if (!hasSP) {
            if (shift >= opSize)
              goto InvalidImmediate;

            opcode.reset(uint32_t(opData.shiftedOp) << 21);
            opcode.addImm(x, 31);
            opcode.addImm(sType, 22);
            opcode.addReg(o1, 16);
            opcode.addImm(shift, 10);
            opcode.addReg(o0, 5);
            opcode.addReg(Gp::kIdZr, 0);
            goto EmitOp;
          }

          // SP register can only be used with LSL or Extend.
          if (sType != uint32_t(ShiftOp::kLSL))
            goto InvalidImmediate;

          sType = x ? uint32_t(ShiftOp::kUXTX) : uint32_t(ShiftOp::kUXTW);
        }

        // Extend operation - UXTB, UXTH, UXTW, UXTX, SXTB, SXTH, SXTW, SXTX.
        sType -= uint32_t(ShiftOp::kUXTB);
        if (sType > 7 || shift > 4)
          goto InvalidImmediate;

        opcode.reset(uint32_t(opData.extendedOp) << 21);
        opcode.addImm(x, 31);
        opcode.addReg(o1, 16);
        opcode.addImm(sType, 13);
        opcode.addImm(shift, 10);
        opcode.addReg(o0, 5);
        opcode.addReg(Gp::kIdZr, 0);
        goto EmitOp;
      }

      break;
    }

    case InstDB::kEncodingBaseMvnNeg: {
      const InstDB::EncodingData::BaseMvnNeg& opData = InstDB::EncodingData::baseMvnNeg[encodingIndex];

      uint32_t x;
      if (!checkGpType(o0, o1, kWX, &x))
        goto InvalidInstruction;

      opcode.reset(opData.opcode);
      opcode.addImm(x, 31);
      opcode.addReg(o1, 16);
      opcode.addReg(o0, 0);

      if (isign4 == ENC_OPS2(Reg, Reg)) {
        if (!checkGpId(o0, o1, kZR))
          goto InvalidPhysId;

        goto EmitOp;
      }

      if (isign4 == ENC_OPS3(Reg, Reg, Imm)) {
        if (!checkGpId(o0, o1, kZR))
          goto InvalidPhysId;

        uint32_t opSize = x ? 64 : 32;
        uint32_t shiftType = o2.as<Imm>().predicate();
        uint64_t opShift = o2.as<Imm>().valueAs<uint64_t>();

        if (shiftType > uint32_t(ShiftOp::kROR) || opShift >= opSize)
          goto InvalidImmediate;

        opcode.addImm(shiftType, 22);
        opcode.addImm(opShift, 10);
        goto EmitOp;
      }

      break;
    }

    case InstDB::kEncodingBaseTst: {
      const InstDB::EncodingData::BaseTst& opData = InstDB::EncodingData::baseTst[encodingIndex];

      uint32_t x;
      if (!checkGpType(o0, kWX, &x))
        goto InvalidInstruction;

      uint32_t opSize = x ? 64 : 32;

      if (isign4 == ENC_OPS2(Reg, Imm) && opData.immediateOp != 0) {
        if (!checkGpId(o0, kZR))
          goto InvalidPhysId;

        // TST (immediate) uses a LogicalImm format described by N:R:S values.
        uint64_t immMask = Support::lsbMask<uint64_t>(opSize);
        uint64_t immValue = o1.as<Imm>().valueAs<uint64_t>();

        // Logical instructions use 13-bit immediate pattern encoded as N:ImmS:ImmR.
        LogicalImm logicalImm;
        if (!Utils::encodeLogicalImm(immValue & immMask, opSize, &logicalImm))
          goto InvalidImmediate;

        opcode.reset(uint32_t(opData.immediateOp) << 22);
        opcode.addLogicalImm(logicalImm);
        opcode.addImm(x, 31);
        opcode.addReg(o0, 5);
        opcode.addReg(Gp::kIdZr, 0);
        goto EmitOp;
      }

      opcode.reset(uint32_t(opData.shiftedOp) << 21);
      opcode.addImm(x, 31);
      opcode.addReg(o1, 16);
      opcode.addReg(o0, 5);
      opcode.addReg(Gp::kIdZr, 0);

      if (isign4 == ENC_OPS2(Reg, Reg)) {
        if (!checkGpId(o0, o1, kZR))
          goto InvalidPhysId;

        goto EmitOp;
      }

      if (isign4 == ENC_OPS3(Reg, Reg, Imm)) {
        if (!checkGpId(o0, o1, kZR))
          goto InvalidPhysId;

        uint32_t shiftType = o2.as<Imm>().predicate();
        uint64_t opShift = o2.as<Imm>().valueAs<uint64_t>();

        if (shiftType > 0x3 || opShift >= opSize)
          goto InvalidImmediate;

        opcode.addImm(shiftType, 22);
        opcode.addImm(opShift, 10);
        goto EmitOp;
      }

      break;
    }

    // ------------------------------------------------------------------------
    // [Base - Bit Manipulation]
    // ------------------------------------------------------------------------

    case InstDB::kEncodingBaseBfc: {
      const InstDB::EncodingData::BaseBfc& opData = InstDB::EncodingData::baseBfc[encodingIndex];

      if (isign4 == ENC_OPS3(Reg, Imm, Imm)) {
        uint32_t x;
        if (!checkGpType(o0, InstDB::kWX, &x))
          goto InvalidInstruction;

        if (!checkGpId(o0))
          goto InvalidPhysId;

        uint64_t lsb = o1.as<Imm>().valueAs<uint64_t>();
        uint64_t width = o2.as<Imm>().valueAs<uint64_t>();
        uint32_t opSize = x ? 64 : 32;

        if (lsb >= opSize || width == 0 || width > opSize)
          goto InvalidImmediate;

        uint32_t lsb32 = Support::neg(uint32_t(lsb)) & (opSize - 1);
        uint32_t width32 = uint32_t(width) - 1;

        opcode.reset(opData.opcode);
        opcode.addImm(x, 31);
        opcode.addImm(x, 22);
        opcode.addImm(lsb32, 16);
        opcode.addImm(width32, 10);
        opcode.addReg(o0, 0);
        goto EmitOp;
      }

      break;
    }

    case InstDB::kEncodingBaseBfi: {
      const InstDB::EncodingData::BaseBfi& opData = InstDB::EncodingData::baseBfi[encodingIndex];

      if (isign4 == ENC_OPS4(Reg, Reg, Imm, Imm)) {
        uint32_t x;
        if (!checkGpType(o0, InstDB::kWX, &x))
          goto InvalidInstruction;

        if (!checkSignature(o0, o1))
          goto InvalidInstruction;

        if (!checkGpId(o0, o1))
          goto InvalidPhysId;

        uint64_t lsb = o2.as<Imm>().valueAs<uint64_t>();
        uint64_t width = o3.as<Imm>().valueAs<uint64_t>();
        uint32_t opSize = x ? 64 : 32;

        if (lsb >= opSize || width == 0 || width > opSize)
          goto InvalidImmediate;

        uint32_t lImm = Support::neg(uint32_t(lsb)) & (opSize - 1);
        uint32_t wImm = uint32_t(width) - 1;

        opcode.reset(opData.opcode);
        opcode.addImm(x, 31);
        opcode.addImm(x, 22);
        opcode.addImm(lImm, 16);
        opcode.addImm(wImm, 10);
        opcode.addReg(o1, 5);
        opcode.addReg(o0, 0);
        goto EmitOp;
      }

      break;
    }

    case InstDB::kEncodingBaseBfm: {
      const InstDB::EncodingData::BaseBfm& opData = InstDB::EncodingData::baseBfm[encodingIndex];

      if (isign4 == ENC_OPS4(Reg, Reg, Imm, Imm)) {
        uint32_t x;
        if (!checkGpType(o0, InstDB::kWX, &x))
          goto InvalidInstruction;

        if (!checkSignature(o0, o1))
          goto InvalidInstruction;

        if (!checkGpId(o0, o1))
          goto InvalidPhysId;

        uint64_t immR = o2.as<Imm>().valueAs<uint64_t>();
        uint64_t immS = o3.as<Imm>().valueAs<uint64_t>();
        uint32_t opSize = x ? 64 : 32;

        if ((immR | immS) >= opSize)
          goto InvalidImmediate;

        opcode.reset(opData.opcode);
        opcode.addImm(x, 31);
        opcode.addImm(x, 22);
        opcode.addImm(immR, 16);
        opcode.addImm(immS, 10);
        opcode.addReg(o1, 5);
        opcode.addReg(o0, 0);
        goto EmitOp;
      }

      break;
    }

    case InstDB::kEncodingBaseBfx: {
      const InstDB::EncodingData::BaseBfx& opData = InstDB::EncodingData::baseBfx[encodingIndex];

      if (isign4 == ENC_OPS4(Reg, Reg, Imm, Imm)) {
        uint32_t x;
        if (!checkGpType(o0, InstDB::kWX, &x))
          goto InvalidInstruction;

        if (!checkSignature(o0, o1))
          goto InvalidInstruction;

        if (!checkGpId(o0, o1))
          goto InvalidPhysId;

        uint64_t lsb = o2.as<Imm>().valueAs<uint64_t>();
        uint64_t width = o3.as<Imm>().valueAs<uint64_t>();
        uint32_t opSize = x ? 64 : 32;

        if (lsb >= opSize || width == 0 || width > opSize)
          goto InvalidImmediate;

        uint32_t lsb32 = uint32_t(lsb);
        uint32_t width32 = lsb32 + uint32_t(width) - 1u;

        if (width32 >= opSize)
          goto InvalidImmediate;

        opcode.reset(opData.opcode);
        opcode.addImm(x, 31);
        opcode.addImm(x, 22);
        opcode.addImm(lsb32, 16);
        opcode.addImm(width32, 10);
        opcode.addReg(o1, 5);
        opcode.addReg(o0, 0);
        goto EmitOp;
      }

      break;
    }

    case InstDB::kEncodingBaseExtend: {
      const InstDB::EncodingData::BaseExtend& opData = InstDB::EncodingData::baseExtend[encodingIndex];

      if (isign4 == ENC_OPS2(Reg, Reg)) {
        uint32_t x;
        if (!checkGpType(o0, opData.rType, &x))
          goto InvalidInstruction;

        if (!o1.as<Reg>().isGpW())
          goto InvalidInstruction;

        if (!checkGpId(o0, o1))
          goto InvalidPhysId;

        opcode.reset(opData.opcode());
        opcode.addImm(x, 31);
        opcode.addImm(x, 22);
        opcode.addReg(o1, 5);
        opcode.addReg(o0, 0);
        goto EmitOp;
      }

      break;
    }

    case InstDB::kEncodingBaseExtract: {
      const InstDB::EncodingData::BaseExtract& opData = InstDB::EncodingData::baseExtract[encodingIndex];

      if (isign4 == ENC_OPS4(Reg, Reg, Reg, Imm)) {
        uint32_t x;
        if (!checkGpType(o0, kWX, &x))
          goto InvalidInstruction;

        if (!checkSignature(o0, o1, o2))
          goto InvalidInstruction;

        if (!checkGpId(o0, o1, o2))
          goto InvalidPhysId;

        uint64_t lsb = o3.as<Imm>().valueAs<uint64_t>();
        uint32_t opSize = x ? 64 : 32;

        if (lsb >= opSize)
          goto InvalidImmediate;

        opcode.reset(opData.opcode);
        opcode.addImm(x, 31);
        opcode.addImm(x, 22);
        opcode.addReg(o2, 16);
        opcode.addImm(lsb, 10);
        opcode.addReg(o1, 5);
        opcode.addReg(o0, 0);
        goto EmitOp;
      }

      break;
    }

    case InstDB::kEncodingBaseRev: {
      if (isign4 == ENC_OPS2(Reg, Reg)) {
        uint32_t x;
        if (!checkGpType(o0, InstDB::kWX, &x))
          goto InvalidInstruction;

        if (!checkSignature(o0, o1))
          goto InvalidInstruction;

        if (!checkGpId(o0, o1))
          goto InvalidPhysId;

        opcode.reset(0b01011010110000000000100000000000);
        opcode.addImm(x, 31);
        opcode.addImm(x, 10);
        opcode.addReg(o1, 5);
        opcode.addReg(o0, 0);
        goto EmitOp;
      }

      break;
    }

    case InstDB::kEncodingBaseShift: {
      const InstDB::EncodingData::BaseShift& opData = InstDB::EncodingData::baseShift[encodingIndex];

      uint32_t x;
      if (!checkGpType(o0, kWX, &x))
        goto InvalidInstruction;

      if (isign4 == ENC_OPS3(Reg, Reg, Reg)) {
        if (!checkSignature(o0, o1, o2))
          goto InvalidInstruction;

        if (!checkGpId(o0, o1, o2, kZR))
          goto InvalidPhysId;

        opcode.reset(opData.registerOp());
        opcode.addImm(x, 31);
        opcode.addReg(o2, 16);
        opcode.addReg(o1, 5);
        opcode.addReg(o0, 0);
        goto EmitOp;
      }

      if (isign4 == ENC_OPS3(Reg, Reg, Imm) && opData.immediateOp()) {
        if (!checkSignature(o0, o1))
          goto InvalidInstruction;

        if (!checkGpId(o0, o1, kZR))
          goto InvalidPhysId;

        uint64_t immR = o2.as<Imm>().valueAs<uint64_t>();
        uint32_t opSize = x ? 64 : 32;

        if (immR >= opSize)
          goto InvalidImmediate;

        opcode.reset(opData.immediateOp());
        opcode.addImm(x, 31);
        opcode.addImm(x, 22);
        opcode.addReg(o1, 5);
        opcode.addReg(o0, 0);

        if (opcode.get() & B(10)) {
          // ASR and LSR (immediate) has the same logic.
          opcode.addImm(x, 15);
          opcode.addImm(immR, 16);
          goto EmitOp;
        }

        if (opData.ror == 0) {
          // LSL (immediate) is an alias to UBFM
          uint32_t ubfmImmR = Support::neg(uint32_t(immR)) & (opSize - 1);
          uint32_t ubfmImmS = opSize - 1 - uint32_t(immR);

          opcode.addImm(ubfmImmR, 16);
          opcode.addImm(ubfmImmS, 10);
          goto EmitOp;
        }
        else {
          // ROR (immediate) is an alias to EXTR.
          opcode.addImm(immR, 10);
          opcode.addReg(o1, 16);
          goto EmitOp;
        }
      }

      break;
    }

    // ------------------------------------------------------------------------
    // [Base - Conditionals]
    // ------------------------------------------------------------------------

    case InstDB::kEncodingBaseCCmp: {
      const InstDB::EncodingData::BaseCCmp& opData = InstDB::EncodingData::baseCCmp[encodingIndex];

      if (isign4 == ENC_OPS4(Reg, Reg, Imm, Imm) || isign4 == ENC_OPS4(Reg, Imm, Imm, Imm)) {
        uint32_t x;
        if (!checkGpType(o0, InstDB::kWX, &x))
          goto InvalidInstruction;

        if (!checkGpId(o0, kZR))
          goto InvalidPhysId;

        uint64_t nzcv = o2.as<Imm>().valueAs<uint64_t>();
        uint64_t cond = o3.as<Imm>().valueAs<uint64_t>();

        if ((nzcv | cond) > 0xFu)
          goto InvalidImmediate;

        opcode.reset(opData.opcode);
        opcode.addImm(x, 31);
        opcode.addImm(condCodeToOpcodeCond(uint32_t(cond)), 12);
        opcode.addImm(nzcv, 0);

        if (isign4 == ENC_OPS4(Reg, Reg, Imm, Imm)) {
          // CCMN|CCMP (register) form.
          if (!checkSignature(o0, o1))
            goto InvalidInstruction;

          if (!checkGpId(o1, kZR))
            goto InvalidPhysId;

          opcode.addReg(o1, 16);
          opcode.addReg(o0, 5);
          goto EmitOp;
        }
        else {
          // CCMN|CCMP (immediate) form.
          uint64_t imm5 = o1.as<Imm>().valueAs<uint64_t>();
          if (imm5 > 0x1F)
            goto InvalidImmediate;

          opcode.addImm(1, 11);
          opcode.addImm(imm5, 16);
          opcode.addReg(o0, 5);
          goto EmitOp;
        }
      }

      break;
    }

    case InstDB::kEncodingBaseCInc: {
      const InstDB::EncodingData::BaseCInc& opData = InstDB::EncodingData::baseCInc[encodingIndex];

      if (isign4 == ENC_OPS3(Reg, Reg, Imm)) {
        uint32_t x;
        if (!checkGpType(o0, o1, InstDB::kWX, &x))
          goto InvalidInstruction;

        if (!checkGpId(o0, o1, kZR))
          goto InvalidPhysId;

        uint64_t cond = o2.as<Imm>().valueAs<uint64_t>();
        if (cond - 2u > 0xEu)
          goto InvalidImmediate;

        opcode.reset(opData.opcode);
        opcode.addImm(x, 31);
        opcode.addReg(o1, 16);
        opcode.addImm(condCodeToOpcodeCond(uint32_t(cond)) ^ 1u, 12);
        opcode.addReg(o1, 5);
        opcode.addReg(o0, 0);
        goto EmitOp;
      }

      break;
    }

    case InstDB::kEncodingBaseCSel: {
      const InstDB::EncodingData::BaseCSel& opData = InstDB::EncodingData::baseCSel[encodingIndex];

      if (isign4 == ENC_OPS4(Reg, Reg, Reg, Imm)) {
        uint32_t x;
        if (!checkGpType(o0, o1, o2, InstDB::kWX, &x))
          goto InvalidInstruction;

        if (!checkGpId(o0, o1, o2, kZR))
          goto InvalidPhysId;

        uint64_t cond = o3.as<Imm>().valueAs<uint64_t>();
        if (cond > 0xFu)
          goto InvalidImmediate;

        opcode.reset(opData.opcode);
        opcode.addImm(x, 31);
        opcode.addReg(o2, 16);
        opcode.addImm(condCodeToOpcodeCond(uint32_t(cond)), 12);
        opcode.addReg(o1, 5);
        opcode.addReg(o0, 0);
        goto EmitOp;
      }

      break;
    }

    case InstDB::kEncodingBaseCSet: {
      const InstDB::EncodingData::BaseCSet& opData = InstDB::EncodingData::baseCSet[encodingIndex];

      if (isign4 == ENC_OPS2(Reg, Imm)) {
        uint32_t x;
        if (!checkGpType(o0, InstDB::kWX, &x))
          goto InvalidInstruction;

        if (!checkGpId(o0, kZR))
          goto InvalidPhysId;

        uint64_t cond = o1.as<Imm>().valueAs<uint64_t>();
        if (cond - 2u >= 0xEu)
          goto InvalidImmediate;

        opcode.reset(opData.opcode);
        opcode.addImm(x, 31);
        opcode.addImm(condCodeToOpcodeCond(uint32_t(cond)) ^ 1u, 12);
        opcode.addReg(o0, 0);
        goto EmitOp;
      }

      break;
    }

    // ------------------------------------------------------------------------
    // [Base - Special]
    // ------------------------------------------------------------------------

    case InstDB::kEncodingBaseAtDcIcTlbi: {
      const InstDB::EncodingData::BaseAtDcIcTlbi& opData = InstDB::EncodingData::baseAtDcIcTlbi[encodingIndex];

      if (isign4 == ENC_OPS1(Imm) || isign4 == ENC_OPS2(Imm, Reg)) {
        if (opData.mandatoryReg && isign4 != ENC_OPS2(Imm, Reg))
          goto InvalidInstruction;

        if (o0.as<Imm>().valueAs<uint64_t>() > 0x7FFFu)
          goto InvalidImmediate;

        uint32_t imm = o0.as<Imm>().valueAs<uint32_t>();
        if ((imm & opData.immVerifyMask) != opData.immVerifyData)
          goto InvalidImmediate;

        uint32_t rt = 31;
        if (o1.isReg()) {
          if (!o1.as<Reg>().isGpX())
            goto InvalidInstruction;

          if (!checkGpId(o1, kZR))
            goto InvalidPhysId;

          rt = o1.id() & 31;
        }

        opcode.reset(0b11010101000010000000000000000000);
        opcode.addImm(imm, 5);
        opcode.addReg(rt, 0);
        goto EmitOp;
      }
      break;
    }

    case InstDB::kEncodingBaseMrs: {
      if (isign4 == ENC_OPS2(Reg, Imm)) {
        if (!o0.as<Reg>().isGpX())
          goto InvalidInstruction;

        if (!checkGpId(o0, kZR))
          goto InvalidPhysId;

        if (o1.as<Imm>().valueAs<uint64_t>() > 0xFFFFu)
          goto InvalidImmediate;

        uint32_t imm = o1.as<Imm>().valueAs<uint32_t>();
        if (!(imm & B(15)))
          goto InvalidImmediate;

        opcode.reset(0b11010101001100000000000000000000);
        opcode.addImm(imm, 5);
        opcode.addReg(o0, 0);
        goto EmitOp;
      }

      break;
    }

    case InstDB::kEncodingBaseMsr: {
      if (isign4 == ENC_OPS2(Imm, Reg)) {
        if (!o1.as<Reg>().isGpX())
          goto InvalidInstruction;

        if (o0.as<Imm>().valueAs<uint64_t>() > 0xFFFFu)
          goto InvalidImmediate;

        uint32_t imm = o0.as<Imm>().valueAs<uint32_t>();
        if (!(imm & B(15)))
          goto InvalidImmediate;

        if (!checkGpId(o1, kZR))
          goto InvalidPhysId;

        opcode.reset(0b11010101000100000000000000000000);
        opcode.addImm(imm, 5);
        opcode.addReg(o1, 0);
        goto EmitOp;
      }

      if (isign4 == ENC_OPS2(Imm, Imm)) {
        if (o0.as<Imm>().valueAs<uint64_t>() > 0x1Fu)
          goto InvalidImmediate;

        if (o1.as<Imm>().valueAs<uint64_t>() > 0xFu)
          goto InvalidImmediate;

        uint32_t op = o0.as<Imm>().valueAs<uint32_t>();
        uint32_t cRm = o1.as<Imm>().valueAs<uint32_t>();

        uint32_t op1 = uint32_t(op) >> 3;
        uint32_t op2 = uint32_t(op) & 0x7u;

        opcode.reset(0b11010101000000000100000000011111);
        opcode.addImm(op1, 16);
        opcode.addImm(cRm, 8);
        opcode.addImm(op2, 5);
        goto EmitOp;
      }

      break;
    }

    case InstDB::kEncodingBaseSys: {
      if (isign4 == ENC_OPS4(Imm, Imm, Imm, Imm)) {
        if (o0.as<Imm>().valueAs<uint64_t>() > 0x7u ||
            o1.as<Imm>().valueAs<uint64_t>() > 0xFu ||
            o2.as<Imm>().valueAs<uint64_t>() > 0xFu ||
            o3.as<Imm>().valueAs<uint64_t>() > 0x7u)
          goto InvalidImmediate;

        uint32_t op1 = o0.as<Imm>().valueAs<uint32_t>();
        uint32_t cRn = o1.as<Imm>().valueAs<uint32_t>();
        uint32_t cRm = o2.as<Imm>().valueAs<uint32_t>();
        uint32_t op2 = o3.as<Imm>().valueAs<uint32_t>();
        uint32_t rt = 31;

        const Operand_& o4 = opExt[EmitterUtils::kOp4];
        if (o4.isReg()) {
          if (!o4.as<Reg>().isGpX())
            goto InvalidInstruction;

          if (!checkGpId(o4, kZR))
            goto InvalidPhysId;

          rt = o4.id() & 31;
        }
        else if (!o4.isNone()) {
          goto InvalidInstruction;
        }

        opcode.reset(0b11010101000010000000000000000000);
        opcode.addImm(op1, 16);
        opcode.addImm(cRn, 12);
        opcode.addImm(cRm, 8);
        opcode.addImm(op2, 5);
        opcode.addImm(rt, 0);
        goto EmitOp;
      }

      break;
    }

    // ------------------------------------------------------------------------
    // [Base - Branch]
    // ------------------------------------------------------------------------

    case InstDB::kEncodingBaseBranchReg: {
      const InstDB::EncodingData::BaseBranchReg& opData = InstDB::EncodingData::baseBranchReg[encodingIndex];

      if (isign4 == ENC_OPS1(Reg)) {
        if (!o0.as<Reg>().isGpX())
          goto InvalidInstruction;

        if (!checkGpId(o0, kZR))
          goto InvalidPhysId;

        opcode.reset(opData.opcode);
        opcode.addReg(o0, 5);
        goto EmitOp;
      }

      break;
    }

    case InstDB::kEncodingBaseBranchRel: {
      const InstDB::EncodingData::BaseBranchRel& opData = InstDB::EncodingData::baseBranchRel[encodingIndex];

      if (isign4 == ENC_OPS1(Label) || isign4 == ENC_OPS1(Imm)) {
        opcode.reset(opData.opcode);
        rmRel = &o0;

        if (instCC != CondCode::kAL) {
          opcode |= B(30);
          opcode.addImm(condCodeToOpcodeCond(uint32_t(instCC)), 0);
          offsetFormat.resetToImmValue(OffsetType::kSignedOffset, 4, 5, 19, 2);
          goto EmitOp_Rel;
        }

        offsetFormat.resetToImmValue(OffsetType::kSignedOffset, 4, 0, 26, 2);
        goto EmitOp_Rel;
      }

      break;
    }

    case InstDB::kEncodingBaseBranchCmp: {
      const InstDB::EncodingData::BaseBranchCmp& opData = InstDB::EncodingData::baseBranchCmp[encodingIndex];

      if (isign4 == ENC_OPS2(Reg, Label) || isign4 == ENC_OPS2(Reg, Imm)) {
        uint32_t x;
        if (!checkGpType(o0, kWX, &x))
          goto InvalidInstruction;

        if (!checkGpId(o0, kZR))
          goto InvalidPhysId;

        opcode.reset(opData.opcode);
        opcode.addImm(x, 31);
        opcode.addReg(o0, 0);
        offsetFormat.resetToImmValue(OffsetType::kSignedOffset, 4, 5, 19, 2);

        rmRel = &o1;
        goto EmitOp_Rel;
      }

      break;
    }

    case InstDB::kEncodingBaseBranchTst: {
      const InstDB::EncodingData::BaseBranchTst& opData = InstDB::EncodingData::baseBranchTst[encodingIndex];

      if (isign4 == ENC_OPS3(Reg, Imm, Label) || isign4 == ENC_OPS3(Reg, Imm, Imm)) {
        uint32_t x;
        if (!checkGpType(o0, kWX, &x))
          goto InvalidInstruction;

        if (!checkGpId(o0, kZR))
          goto InvalidPhysId;

        uint64_t imm = o1.as<Imm>().valueAs<uint64_t>();

        opcode.reset(opData.opcode);
        if (imm >= 32) {
          if (!x)
            goto InvalidImmediate;
          opcode.addImm(x, 31);
          imm &= 0x1F;
        }

        opcode.addReg(o0, 0);
        opcode.addImm(imm, 19);
        offsetFormat.resetToImmValue(OffsetType::kSignedOffset, 4, 5, 14, 2);

        rmRel = &o2;
        goto EmitOp_Rel;
      }

      break;
    }

    // ------------------------------------------------------------------------
    // [Base - Load / Store]
    // ------------------------------------------------------------------------

    case InstDB::kEncodingBaseLdSt: {
      const InstDB::EncodingData::BaseLdSt& opData = InstDB::EncodingData::baseLdSt[encodingIndex];

      if (isign4 == ENC_OPS2(Reg, Mem)) {
        const Mem& m = o1.as<Mem>();
        rmRel = &m;

        uint32_t x;
        if (!checkGpType(o0, opData.rType, &x))
          goto InvalidInstruction;

        if (!checkGpId(o0, kZR))
          goto InvalidPhysId;

        // Instructions that work with either word or dword have the unsigned
        // offset shift set to 2 (word), so we set it to 3 (dword) if this is
        // X version of the instruction.
        uint32_t xShiftMask = uint32_t(opData.uOffsetShift == 2);
        uint32_t immShift = uint32_t(opData.uOffsetShift) + (x & xShiftMask);

        if (!armCheckMemBaseIndexRel(m))
          goto InvalidAddress;

        int64_t offset = m.offset();
        if (m.hasBaseReg()) {
          // [Base {Offset | Index}]
          if (m.hasIndex()) {
            uint32_t opt = armShiftOpToLdStOptMap[m.predicate()];
            if (opt == 0xFF)
              goto InvalidAddress;

            uint32_t shift = m.shift();
            uint32_t s = shift != 0;

            if (s && shift != immShift)
              goto InvalidAddressScale;

            opcode.reset(uint32_t(opData.registerOp) << 21);
            opcode.xorImm(x, opData.xOffset);
            opcode.addImm(opt, 13);
            opcode.addImm(s, 12);
            opcode |= B(11);
            opcode.addReg(o0, 0);
            goto EmitOp_MemBaseIndex_Rn5_Rm16;
          }

          // Makes it easier to work with the offset especially on 32-bit arch.
          if (!Support::isInt32(offset))
            goto InvalidDisplacement;
          int32_t offset32 = int32_t(offset);

          if (m.isPreOrPost()) {
            if (!Support::isInt9(offset32))
              goto InvalidDisplacement;

            opcode.reset(uint32_t(opData.prePostOp) << 21);
            opcode.xorImm(x, opData.xOffset);
            opcode.addImm(offset32 & 0x1FF, 12);
            opcode.addImm(m.isPreIndex(), 11);
            opcode |= B(10);
            opcode.addReg(o0, 0);
            goto EmitOp_MemBase_Rn5;
          }
          else {
            uint32_t imm12 = uint32_t(offset32) >> immShift;

            // Alternative form of LDUR/STUR and related instructions as described by AArch64 reference manual:
            //
            // If this instruction is not encodable with scaled unsigned offset, try unscaled signed offset.
            if (!Support::isUInt12(imm12) || (imm12 << immShift) != uint32_t(offset32)) {
              instId = opData.uAltInstId;
              instInfo = &InstDB::_instInfoTable[instId];
              encodingIndex = instInfo->_encodingDataIndex;
              goto Case_BaseLdurStur;
            }

            opcode.reset(uint32_t(opData.uOffsetOp) << 22);
            opcode.xorImm(x, opData.xOffset);
            opcode.addImm(imm12, 10);
            opcode.addReg(o0, 0);
            goto EmitOp_MemBase_Rn5;
          }
        }
        else {
          if (!opData.literalOp)
            goto InvalidAddress;

          opcode.reset(uint32_t(opData.literalOp) << 24);
          opcode.xorImm(x, opData.xOffset);
          opcode.addReg(o0, 0);
          offsetFormat.resetToImmValue(OffsetType::kSignedOffset, 4, 5, 19, 2);
          goto EmitOp_Rel;
        }
      }

      break;
    }

    case InstDB::kEncodingBaseLdpStp: {
      const InstDB::EncodingData::BaseLdpStp& opData = InstDB::EncodingData::baseLdpStp[encodingIndex];

      if (isign4 == ENC_OPS3(Reg, Reg, Mem)) {
        const Mem& m = o2.as<Mem>();
        rmRel = &m;

        uint32_t x;
        if (!checkGpType(o0, o1, opData.rType, &x))
          goto InvalidInstruction;

        if (!checkGpId(o0, o1, kZR))
          goto InvalidPhysId;

        if (m.baseType() != RegType::kARM_GpX || m.hasIndex())
          goto InvalidAddress;

        if (m.isOffset64Bit())
          goto InvalidDisplacement;

        uint32_t offsetShift = opData.offsetShift + x;
        int32_t offset32 = m.offsetLo32() >> offsetShift;

        // Make sure we didn't lose bits by applying the mandatory offset shift.
        if (uint32_t(offset32) << offsetShift != uint32_t(m.offsetLo32()))
          goto InvalidDisplacement;

        // Offset is encoded as 7-bit immediate.
        if (!Support::isInt7(offset32))
          goto InvalidDisplacement;

        if (m.isPreOrPost() && offset32 != 0) {
          if (!opData.prePostOp)
            goto InvalidAddress;

          opcode.reset(uint32_t(opData.prePostOp) << 22);
          opcode.addImm(m.isPreIndex(), 24);
        }
        else {
          opcode.reset(uint32_t(opData.offsetOp) << 22);
        }

        opcode.addImm(x, opData.xOffset);
        opcode.addImm(offset32 & 0x7F, 15);
        opcode.addReg(o1, 10);
        opcode.addReg(o0, 0);
        goto EmitOp_MemBase_Rn5;
      }

      break;
    }

    case InstDB::kEncodingBaseStx: {
      const InstDB::EncodingData::BaseStx& opData = InstDB::EncodingData::baseStx[encodingIndex];

      if (isign4 == ENC_OPS3(Reg, Reg, Mem)) {
        const Mem& m = o2.as<Mem>();
        uint32_t x;

        if (!o0.as<Reg>().isGpW() || !checkGpType(o1, opData.rType, &x))
          goto InvalidInstruction;

        if (!checkGpId(o0, o1, kZR))
          goto InvalidPhysId;

        opcode.reset(opData.opcode());
        opcode.addImm(x, opData.xOffset);
        opcode.addReg(o0, 16);
        opcode.addReg(o1, 0);

        rmRel = &m;
        goto EmitOp_MemBaseNoImm_Rn5;
      }

      break;
    }

    case InstDB::kEncodingBaseLdxp: {
      const InstDB::EncodingData::BaseLdxp& opData = InstDB::EncodingData::baseLdxp[encodingIndex];

      if (isign4 == ENC_OPS3(Reg, Reg, Mem)) {
        const Mem& m = o2.as<Mem>();
        uint32_t x;

        if (!checkGpType(o0, opData.rType, &x) || !checkSignature(o0, o1))
          goto InvalidInstruction;

        if (!checkGpId(o0, o1, kZR))
          goto InvalidPhysId;

        opcode.reset(opData.opcode());
        opcode.addImm(x, opData.xOffset);
        opcode.addReg(o1, 10);
        opcode.addReg(o0, 0);

        rmRel = &m;
        goto EmitOp_MemBaseNoImm_Rn5;
      }

      break;
    }

    case InstDB::kEncodingBaseStxp: {
      const InstDB::EncodingData::BaseStxp& opData = InstDB::EncodingData::baseStxp[encodingIndex];

      if (isign4 == ENC_OPS4(Reg, Reg, Reg, Mem)) {
        const Mem& m = o3.as<Mem>();
        uint32_t x;

        if (!o0.as<Reg>().isGpW() || !checkGpType(o1, opData.rType, &x) || !checkSignature(o1, o2))
          goto InvalidInstruction;

        if (!checkGpId(o0, o1, o2, kZR))
          goto InvalidPhysId;

        opcode.reset(opData.opcode());
        opcode.addImm(x, opData.xOffset);
        opcode.addReg(o0, 16);
        opcode.addReg(o2, 10);
        opcode.addReg(o1, 0);

        rmRel = &m;
        goto EmitOp_MemBaseNoImm_Rn5;
      }

      break;
    }

    case InstDB::kEncodingBaseRM_NoImm: {
      const InstDB::EncodingData::BaseRM_NoImm& opData = InstDB::EncodingData::baseRM_NoImm[encodingIndex];

      if (isign4 == ENC_OPS2(Reg, Mem)) {
        const Mem& m = o1.as<Mem>();
        rmRel = &m;

        uint32_t x;
        if (!checkGpType(o0, opData.rType, &x))
          goto InvalidInstruction;

        if (!checkGpId(o0, opData.rHiId))
          goto InvalidPhysId;

        opcode.reset(opData.opcode());
        opcode.addImm(x, opData.xOffset);
        opcode.addReg(o0, 0);
        goto EmitOp_MemBaseNoImm_Rn5;
      }

      break;
    }

    case InstDB::kEncodingBaseRM_SImm9: {
Case_BaseLdurStur:
      const InstDB::EncodingData::BaseRM_SImm9& opData = InstDB::EncodingData::baseRM_SImm9[encodingIndex];

      if (isign4 == ENC_OPS2(Reg, Mem)) {
        const Mem& m = o1.as<Mem>();
        rmRel = &m;

        uint32_t x;
        if (!checkGpType(o0, opData.rType, &x))
          goto InvalidInstruction;

        if (!checkGpId(o0, opData.rHiId))
          goto InvalidPhysId;

        if (m.hasBaseReg() && !m.hasIndex()) {
          if (m.isOffset64Bit())
            goto InvalidDisplacement;

          int32_t offset32 = m.offsetLo32() >> opData.immShift;
          if (Support::shl(offset32, opData.immShift) != m.offsetLo32())
            goto InvalidDisplacement;

          if (!Support::isInt9(offset32))
            goto InvalidDisplacement;

          if (m.isFixedOffset()) {
            opcode.reset(opData.offsetOp());
          }
          else {
            if (!opData.prePostOp())
              goto InvalidInstruction;

            opcode.reset(opData.prePostOp());
            opcode.xorImm(m.isPreIndex(), 11);
          }

          opcode.xorImm(x, opData.xOffset);
          opcode.addImm(offset32 & 0x1FF, 12);
          opcode.addReg(o0, 0);
          goto EmitOp_MemBase_Rn5;
        }

        goto InvalidAddress;
      }

      break;
    }

    case InstDB::kEncodingBaseRM_SImm10: {
      const InstDB::EncodingData::BaseRM_SImm10& opData = InstDB::EncodingData::baseRM_SImm10[encodingIndex];

      if (isign4 == ENC_OPS2(Reg, Mem)) {
        const Mem& m = o1.as<Mem>();
        rmRel = &m;

        uint32_t x;
        if (!checkGpType(o0, opData.rType, &x))
          goto InvalidInstruction;

        if (!checkGpId(o0, opData.rHiId))
          goto InvalidPhysId;

        if (m.hasBaseReg() && !m.hasIndex()) {
          if (m.isOffset64Bit())
            goto InvalidDisplacement;

          int32_t offset32 = m.offsetLo32() >> opData.immShift;
          if (Support::shl(offset32, opData.immShift) != m.offsetLo32())
            goto InvalidDisplacement;

          if (!Support::isInt10(offset32))
            goto InvalidDisplacement;

          if (m.isPostIndex())
            goto InvalidAddress;

          // Offset has 10 bits, sign is stored in the 10th bit.
          offset32 &= 0x3FF;

          opcode.reset(opData.opcode());
          opcode.xorImm(m.isPreIndex(), 11);
          opcode.xorImm(x, opData.xOffset);
          opcode.addImm(offset32 >> 9, 22);
          opcode.addImm(offset32, 12);
          opcode.addReg(o0, 0);
          goto EmitOp_MemBase_Rn5;
        }

        goto InvalidAddress;
      }

      break;
    }

    case InstDB::kEncodingBaseAtomicOp: {
      const InstDB::EncodingData::BaseAtomicOp& opData = InstDB::EncodingData::baseAtomicOp[encodingIndex];

      if (isign4 == ENC_OPS3(Reg, Reg, Mem)) {
        const Mem& m = o2.as<Mem>();
        uint32_t x;

        if (!checkGpType(o0, opData.rType, &x) || !checkSignature(o0, o1))
          goto InvalidInstruction;

        if (!checkGpId(o0, o1, kZR))
          goto InvalidInstruction;

        opcode.reset(opData.opcode());
        opcode.addImm(x, opData.xOffset);
        opcode.addReg(o0, 16);
        opcode.addReg(o1, 0);

        rmRel = &m;
        goto EmitOp_MemBaseNoImm_Rn5;
      }

      break;
    }

    case InstDB::kEncodingBaseAtomicSt: {
      const InstDB::EncodingData::BaseAtomicSt& opData = InstDB::EncodingData::baseAtomicSt[encodingIndex];

      if (isign4 == ENC_OPS2(Reg, Mem)) {
        const Mem& m = o1.as<Mem>();
        uint32_t x;

        if (!checkGpType(o0, opData.rType, &x))
          goto InvalidInstruction;

        if (!checkGpId(o0, kZR))
          goto InvalidPhysId;

        opcode.reset(opData.opcode());
        opcode.addImm(x, opData.xOffset);
        opcode.addReg(o0, 16);
        opcode.addReg(Gp::kIdZr, 0);

        rmRel = &m;
        goto EmitOp_MemBaseNoImm_Rn5;
      }

      break;
    }

    case InstDB::kEncodingBaseAtomicCasp: {
      const InstDB::EncodingData::BaseAtomicCasp& opData = InstDB::EncodingData::baseAtomicCasp[encodingIndex];
      const Operand_& o4 = opExt[EmitterUtils::kOp4];

      if (isign4 == ENC_OPS4(Reg, Reg, Reg, Reg) && o4.isMem()) {
        const Mem& m = o4.as<Mem>();
        uint32_t x;

        if (!checkGpType(o0, opData.rType, &x))
          goto InvalidInstruction;

        if (!checkSignature(o0, o1, o2, o3))
          goto InvalidInstruction;

        if (!checkEven(o0, o2) || !checkGpId(o0, o2, kZR))
          goto InvalidPhysId;

        if (!checkConsecutive(o0, o1) || !checkConsecutive(o2, o3))
          goto InvalidPhysId;

        opcode.reset(opData.opcode());
        opcode.addImm(x, opData.xOffset);
        opcode.addReg(o0, 16);
        opcode.addReg(o2, 0);

        rmRel = &m;
        goto EmitOp_MemBaseNoImm_Rn5;
      }

      break;
    }

    // ------------------------------------------------------------------------
    // [FSimd - Instructions]
    // ------------------------------------------------------------------------

    case InstDB::kEncodingFSimdSV: {
      const InstDB::EncodingData::FSimdSV& opData = InstDB::EncodingData::fSimdSV[encodingIndex];

      if (isign4 == ENC_OPS2(Reg, Reg)) {
        uint32_t q = diff(o1.as<Reg>().type(), RegType::kARM_VecD);
        if (q > 1)
          goto InvalidInstruction;

        if (o0.as<Vec>().hasElementType())
          goto InvalidInstruction;

        // This operation is only defined for:
        //   hD, vS.{4|8}h (16-bit)
        //   sD, vS.4s     (32-bit)
        uint32_t sz = diff(o0.as<Reg>().type(), RegType::kARM_VecH);
        uint32_t elementSz = o1.as<Vec>().elementType() - Vec::kElementTypeH;

        // Size greater than 1 means 64-bit elements, not supported.
        if ((sz | elementSz) > 1 || sz != elementSz)
          goto InvalidInstruction;

        // Size 1 (32-bit float) requires at least 4 elements.
        if (sz && !q)
          goto InvalidInstruction;

        // Bit flipping according to sz.
        static const uint32_t szBits[] = { B(29), 0 };

        opcode.reset(opData.opcode << 10);
        opcode ^= szBits[sz];
        opcode.addImm(q, 30);
        goto EmitOp_Rd0_Rn5;
      }

      break;
    }

    case InstDB::kEncodingFSimdVV: {
      const InstDB::EncodingData::FSimdVV& opData = InstDB::EncodingData::fSimdVV[encodingIndex];

      if (isign4 == ENC_OPS2(Reg, Reg)) {
        if (!matchSignature(o0, o1, instFlags))
          goto InvalidInstruction;

        if (!pickFpOpcode(o0.as<Vec>(), opData.scalarOp(), opData.scalarHf(), opData.vectorOp(), opData.vectorHf(), &opcode))
          goto InvalidInstruction;

        goto EmitOp_Rd0_Rn5;
      }

      break;
    }

    case InstDB::kEncodingFSimdVVV: {
      const InstDB::EncodingData::FSimdVVV& opData = InstDB::EncodingData::fSimdVVV[encodingIndex];

      if (isign4 == ENC_OPS3(Reg, Reg, Reg)) {
        if (!matchSignature(o0, o1, o2, instFlags))
          goto InvalidInstruction;

        if (!pickFpOpcode(o0.as<Vec>(), opData.scalarOp(), opData.scalarHf(), opData.vectorOp(), opData.vectorHf(), &opcode))
          goto InvalidInstruction;

        goto EmitOp_Rd0_Rn5_Rm16;
      }

      break;
    }

    case InstDB::kEncodingFSimdVVVe: {
      const InstDB::EncodingData::FSimdVVVe& opData = InstDB::EncodingData::fSimdVVVe[encodingIndex];

      if (isign4 == ENC_OPS3(Reg, Reg, Reg)) {
        if (!o2.as<Vec>().hasElementIndex()) {
          if (!matchSignature(o0, o1, o2, instFlags))
            goto InvalidInstruction;

          if (!pickFpOpcode(o0.as<Vec>(), opData.scalarOp(), opData.scalarHf(), opData.vectorOp(), opData.vectorHf(), &opcode))
            goto InvalidInstruction;

          goto EmitOp_Rd0_Rn5_Rm16;
        }
        else {
          if (!matchSignature(o0, o1, instFlags))
            goto InvalidInstruction;

          uint32_t q = o1.as<Reg>().isVecQ();
          uint32_t sz;

          if (!pickFpOpcode(o0.as<Vec>(), opData.elementScalarOp(), InstDB::kHF_D, opData.elementVectorOp(), InstDB::kHF_D, &opcode, &sz))
            goto InvalidInstruction;

          if (sz == 0 && o2.as<Reg>().id() > 15)
            goto InvalidPhysId;

          uint32_t elementIndex = o2.as<Vec>().elementIndex();
          if (elementIndex > (7u >> sz))
            goto InvalidElementIndex;

          uint32_t hlm = elementIndex << sz;
          opcode.addImm(q, 30);
          opcode.addImm(hlm & 3u, 20);
          opcode.addImm(hlm >> 2, 11);
          goto EmitOp_Rd0_Rn5_Rm16;
        }
      }

      break;
    }

    case InstDB::kEncodingFSimdVVVV: {
      const InstDB::EncodingData::FSimdVVVV& opData = InstDB::EncodingData::fSimdVVVV[encodingIndex];

      if (isign4 == ENC_OPS4(Reg, Reg, Reg, Reg)) {
        if (!matchSignature(o0, o1, o2, o3, instFlags))
          goto InvalidInstruction;

        if (!pickFpOpcode(o0.as<Vec>(), opData.scalarOp(), opData.scalarHf(), opData.vectorOp(), opData.vectorHf(), &opcode))
          goto InvalidInstruction;

        goto EmitOp_Rd0_Rn5_Rm16_Ra10;
      }

      break;
    }

    case InstDB::kEncodingSimdFcadd: {
      const InstDB::EncodingData::SimdFcadd& opData = InstDB::EncodingData::simdFcadd[encodingIndex];

      if (isign4 == ENC_OPS4(Reg, Reg, Reg, Imm)) {
        if (!checkSignature(o0, o1, o2) || o0.as<Vec>().hasElementIndex())
          goto InvalidInstruction;

        uint32_t q = diff(o0.as<Reg>().type(), RegType::kARM_VecD);
        if (q > 1)
          goto InvalidInstruction;

        uint32_t sz = o0.as<Vec>().elementType() - Vec::kElementTypeB;
        if (sz == 0 || sz > 3)
          goto InvalidInstruction;

        // 0 <- 90deg.
        // 1 <- 270deg.
        uint32_t rot = 0;
        if (o3.as<Imm>().value() == 270)
          rot = 1;
        else if (o3.as<Imm>().value() != 90)
          goto InvalidImmediate;

        opcode.reset(opData.opcode());
        opcode.addImm(q, 30);
        opcode.addImm(sz, 22);
        opcode.addImm(rot, 12);
        goto EmitOp_Rd0_Rn5_Rm16;
      }

      break;
    }

    case InstDB::kEncodingSimdFccmpFccmpe: {
      const InstDB::EncodingData::SimdFccmpFccmpe& opData = InstDB::EncodingData::simdFccmpFccmpe[encodingIndex];

      if (isign4 == ENC_OPS4(Reg, Reg, Imm, Imm)) {
        uint32_t sz = diff(o0.as<Reg>().type(), RegType::kARM_VecH);
        if (sz > 2)
          goto InvalidInstruction;

        if (!checkSignature(o0, o1) || o0.as<Vec>().hasElementType())
          goto InvalidInstruction;

        uint64_t nzcv = o2.as<Imm>().valueAs<uint64_t>();
        uint64_t cond = o3.as<Imm>().valueAs<uint64_t>();

        if ((nzcv | cond) > 0xFu)
          goto InvalidImmediate;

        uint32_t type = (sz - 1) & 0x3u;

        opcode.reset(opData.opcode());
        opcode.addImm(type, 22);
        opcode.addImm(condCodeToOpcodeCond(uint32_t(cond)), 12);
        opcode.addImm(nzcv, 0);

        goto EmitOp_Rn5_Rm16;
      }

      break;
    }

    case InstDB::kEncodingSimdFcm: {
      const InstDB::EncodingData::SimdFcm& opData = InstDB::EncodingData::simdFcm[encodingIndex];

      if (isign4 == ENC_OPS3(Reg, Reg, Reg) && opData.hasRegisterOp()) {
        if (!matchSignature(o0, o1, o2, instFlags))
          goto InvalidInstruction;

        if (!pickFpOpcode(o0.as<Vec>(), opData.registerScalarOp(), opData.registerScalarHf(), opData.registerVectorOp(), opData.registerVectorHf(), &opcode))
          goto InvalidInstruction;

        goto EmitOp_Rd0_Rn5_Rm16;
      }

      if (isign4 == ENC_OPS3(Reg, Reg, Imm) && opData.hasZeroOp()) {
        if (!checkSignature(o0, o1))
          goto InvalidInstruction;

        if (o2.as<Imm>().value() != 0 || o2.as<Imm>().predicate() != 0)
          goto InvalidImmediate;

        if (!pickFpOpcode(o0.as<Vec>(), opData.zeroScalarOp(), InstDB::kHF_B, opData.zeroVectorOp(), InstDB::kHF_B, &opcode))
          goto InvalidInstruction;

        goto EmitOp_Rd0_Rn5;
      }

      break;
    }

    case InstDB::kEncodingSimdFcmla: {
      const InstDB::EncodingData::SimdFcmla& opData = InstDB::EncodingData::simdFcmla[encodingIndex];

      if (isign4 == ENC_OPS4(Reg, Reg, Reg, Imm)) {
        if (!checkSignature(o0, o1))
          goto InvalidInstruction;

        uint32_t q = diff(o0.as<Reg>().type(), RegType::kARM_VecD);
        if (q > 1)
          goto InvalidInstruction;

        uint32_t sz = o0.as<Vec>().elementType() - Vec::kElementTypeB;
        if (sz == 0 || sz > 3)
          goto InvalidInstruction;

        uint32_t rot = 0;
        switch (o3.as<Imm>().value()) {
          case 0  : rot = 0; break;
          case 90 : rot = 1; break;
          case 180: rot = 2; break;
          case 270: rot = 3; break;
          default:
            goto InvalidImmediate;
        }

        if (!o2.as<Vec>().hasElementIndex()) {
          if (!checkSignature(o1, o2))
            goto InvalidInstruction;

          opcode.reset(opData.regularOp());
          opcode.addImm(q, 30);
          opcode.addImm(sz, 22);
          opcode.addImm(rot, 11);
          goto EmitOp_Rd0_Rn5_Rm16;
        }
        else {
          if (o0.as<Vec>().elementType() != o2.as<Vec>().elementType())
            goto InvalidInstruction;

          // Only allowed vectors are: 4H, 8H, and 4S.
          if (!(sz == 1 || (q == 1 && sz == 2)))
            goto InvalidInstruction;

          // Element index ranges:
          //   4H - ElementIndex[0..1] (index 2..3 is UNDEFINED).
          //   8H - ElementIndex[0..3].
          //   4S - ElementIndex[0..1].
          uint32_t elementIndex = o2.as<Vec>().elementIndex();
          uint32_t hlFieldShift = sz == 1 ? 0u : 1u;
          uint32_t maxElementIndex = q == 1 && sz == 1 ? 3u : 1u;

          if (elementIndex > maxElementIndex)
            goto InvalidElementIndex;

          uint32_t hl = elementIndex << hlFieldShift;

          opcode.reset(opData.elementOp());
          opcode.addImm(q, 30);
          opcode.addImm(sz, 22);
          opcode.addImm(hl & 1u, 21); // L field.
          opcode.addImm(hl >> 1, 11); // H field.
          opcode.addImm(rot, 13);
          goto EmitOp_Rd0_Rn5_Rm16;
        }
      }

      break;
    }

    case InstDB::kEncodingSimdFcmpFcmpe: {
      const InstDB::EncodingData::SimdFcmpFcmpe& opData = InstDB::EncodingData::simdFcmpFcmpe[encodingIndex];

      uint32_t sz = diff(o0.as<Reg>().type(), RegType::kARM_VecH);
      uint32_t type = (sz - 1) & 0x3u;

      if (sz > 2)
        goto InvalidInstruction;

      if (o0.as<Vec>().hasElementType())
        goto InvalidInstruction;

      opcode.reset(opData.opcode());
      opcode.addImm(type, 22);

      if (isign4 == ENC_OPS2(Reg, Reg)) {
        if (!checkSignature(o0, o1))
          goto InvalidInstruction;

        goto EmitOp_Rn5_Rm16;
      }

      if (isign4 == ENC_OPS2(Reg, Imm)) {
        if (o1.as<Imm>().value() != 0 || o1.as<Imm>().predicate() != 0)
          goto InvalidInstruction;

        opcode |= B(3);
        goto EmitOp_Rn5;
      }

      break;
    }

    case InstDB::kEncodingSimdFcsel: {
      if (isign4 == ENC_OPS4(Reg, Reg, Reg, Imm)) {
        if (!checkSignature(o0, o1, o2))
          goto InvalidInstruction;

        uint32_t sz = diff(o0.as<Reg>().type(), RegType::kARM_VecH);
        uint32_t type = (sz - 1) & 0x3u;

        if (sz > 2 || o0.as<Vec>().hasElementType())
          goto InvalidInstruction;

        uint64_t cond = o3.as<Imm>().valueAs<uint64_t>();
        if (cond > 0xFu)
          goto InvalidImmediate;

        opcode.reset(0b00011110001000000000110000000000);
        opcode.addImm(type, 22);
        opcode.addImm(condCodeToOpcodeCond(uint32_t(cond)), 12);
        goto EmitOp_Rd0_Rn5_Rm16;
      }

      break;
    }

    case InstDB::kEncodingSimdFcvt: {
      if (isign4 == ENC_OPS2(Reg, Reg)) {
        uint32_t dstSz = diff(o0.as<Reg>().type(), RegType::kARM_VecH);
        uint32_t srcSz = diff(o1.as<Reg>().type(), RegType::kARM_VecH);

        if ((dstSz | srcSz) > 3)
          goto InvalidInstruction;

        if (o0.as<Vec>().hasElementType() || o1.as<Vec>().hasElementType())
          goto InvalidInstruction;

        // Table that provides 'type' and 'opc' according to the dst/src combination.
        static const uint8_t table[] = {
          0xFFu, // H <- H (Invalid).
          0x03u, // H <- S (type=00 opc=11).
          0x13u, // H <- D (type=01 opc=11).
          0xFFu, // H <- Q (Invalid).
          0x30u, // S <- H (type=11 opc=00).
          0xFFu, // S <- S (Invalid).
          0x10u, // S <- D (type=01 opc=00).
          0xFFu, // S <- Q (Invalid).
          0x31u, // D <- H (type=11 opc=01).
          0x01u, // D <- S (type=00 opc=01).
          0xFFu, // D <- D (Invalid).
          0xFFu, // D <- Q (Invalid).
          0xFFu, // Q <- H (Invalid).
          0xFFu, // Q <- S (Invalid).
          0xFFu, // Q <- D (Invalid).
          0xFFu  // Q <- Q (Invalid).
        };

        uint32_t typeOpc = table[(dstSz << 2) | srcSz];
        opcode.reset(0b0001111000100010010000 << 10);
        opcode.addImm(typeOpc >> 4, 22);
        opcode.addImm(typeOpc & 15, 15);
        goto EmitOp_Rd0_Rn5;
      }

      break;
    }

    case InstDB::kEncodingSimdFcvtLN: {
      const InstDB::EncodingData::SimdFcvtLN& opData = InstDB::EncodingData::simdFcvtLN[encodingIndex];

      if (isign4 == ENC_OPS2(Reg, Reg)) {
        // Scalar form - only FCVTXN.
        if (o0.as<Vec>().isVecS() && o1.as<Vec>().isVecD()) {
          if (!opData.hasScalar())
            goto InvalidInstruction;

          if (o0.as<Vec>().hasElementType() || o1.as<Vec>().hasElementType())
            goto InvalidInstruction;

          opcode.reset(opData.scalarOp());
          opcode |= B(22); // sz bit must be 1, the only supported combination of FCVTXN.
          goto EmitOp_Rd0_Rn5;
        }

        opcode.reset(opData.vectorOp());

        const Vec& rL = (instFlags & InstDB::kInstFlagLong) ? o0.as<Vec>() : o1.as<Vec>();
        const Vec& rN = (instFlags & InstDB::kInstFlagLong) ? o1.as<Vec>() : o0.as<Vec>();

        uint32_t q = diff(rN.type(), RegType::kARM_VecD);
        if (uint32_t(opcode.hasQ()) != q)
          goto InvalidInstruction;

        if (rL.isVecS4() && rN.elementType() == Vec::kElementTypeH && !opData.isCvtxn()) {
          goto EmitOp_Rd0_Rn5;
        }

        if (rL.isVecD2() && rN.elementType() == Vec::kElementTypeS) {
          opcode |= B(22);
          goto EmitOp_Rd0_Rn5;
        }
      }

      break;
    }

    case InstDB::kEncodingSimdFcvtSV: {
      const InstDB::EncodingData::SimdFcvtSV& opData = InstDB::EncodingData::simdFcvtSV[encodingIndex];

      // So we can support both IntToFloat and FloatToInt conversions.
      const Operand_& oGp = opData.isFloatToInt() ? o0 : o1;
      const Operand_& oVec = opData.isFloatToInt() ? o1 : o0;

      if (isign4 == ENC_OPS2(Reg, Reg)) {
        if (oGp.as<Reg>().isGp() && oVec.as<Reg>().isVec()) {
          uint32_t x = oGp.as<Reg>().isGpX();
          uint32_t type = diff(oVec.as<Reg>().type(), RegType::kARM_VecH);

          if (type > 2u)
            goto InvalidInstruction;

          type = (type - 1u) & 0x3;
          opcode.reset(opData.generalOp());
          opcode.addImm(type, 22);
          opcode.addImm(x, 31);
          goto EmitOp_Rd0_Rn5;
        }

        if (o0.as<Reg>().isVec() && o1.as<Reg>().isVec()) {
          if (!checkSignature(o0, o1))
            goto InvalidInstruction;

          if (!pickFpOpcode(o0.as<Vec>(), opData.scalarIntOp(), InstDB::kHF_B, opData.vectorIntOp(), InstDB::kHF_B, &opcode))
            goto InvalidInstruction;

          goto EmitOp_Rd0_Rn5;
        }
      }

      if (isign4 == ENC_OPS3(Reg, Reg, Imm) && opData.isFixedPoint()) {
        if (o2.as<Imm>().valueAs<uint64_t>() >= 64)
          goto InvalidInstruction;

        uint32_t scale = o2.as<Imm>().valueAs<uint32_t>();
        if (scale == 0)
          goto InvalidInstruction;

        if (oGp.as<Reg>().isGp() && oVec.as<Reg>().isVec()) {
          uint32_t x = oGp.as<Reg>().isGpX();
          uint32_t type = diff(oVec.as<Reg>().type(), RegType::kARM_VecH);

          uint32_t scaleLimit = 32u << x;
          if (scale > scaleLimit)
            goto InvalidInstruction;

          type = (type - 1u) & 0x3;
          opcode.reset(opData.generalOp() ^ B(21));
          opcode.addImm(type, 22);
          opcode.addImm(x, 31);
          opcode.addImm(64u - scale, 10);
          goto EmitOp_Rd0_Rn5;
        }

        if (o0.as<Reg>().isVec() && o1.as<Reg>().isVec()) {
          if (!checkSignature(o0, o1))
            goto InvalidInstruction;

          uint32_t sz;
          if (!pickFpOpcode(o0.as<Vec>(), opData.scalarFpOp(), InstDB::kHF_0, opData.vectorFpOp(), InstDB::kHF_0, &opcode, &sz))
            goto InvalidInstruction;

          uint32_t scaleLimit = 16u << sz;
          if (scale > scaleLimit)
            goto InvalidInstruction;

          uint32_t imm = Support::neg(scale) & Support::lsbMask<uint32_t>(sz + 4 + 1);
          opcode.addImm(imm, 16);
          goto EmitOp_Rd0_Rn5;
        }
      }

      break;
    }

    case InstDB::kEncodingSimdFmlal: {
      const InstDB::EncodingData::SimdFmlal& opData = InstDB::EncodingData::simdFmlal[encodingIndex];

      if (isign4 == ENC_OPS3(Reg, Reg, Reg)) {
        uint32_t q = diff(o0.as<Reg>().type(), RegType::kARM_VecD);
        uint32_t qIsOptional = opData.optionalQ();

        if (qIsOptional) {
          // This instruction works with either 64-bit or 128-bit registers,
          // encoded by Q bit.
          if (q > 1)
            goto InvalidInstruction;
        }
        else {
          // This instruction requires 128-bit vector registers.
          if (q != 1)
            goto InvalidInstruction;

          // The instruction is ehtier B (bottom) or T (top), which is part of
          // the opcode, which uses Q bit, so we have to clear it explicitly.
          q = 0;
        }

        if (uint32_t(o0.as<Reg>().type()) != uint32_t(o1.as<Reg>().type()) + qIsOptional ||
            o0.as<Vec>().elementType() != opData.tA ||
            o1.as<Vec>().elementType() != opData.tB)
          goto InvalidInstruction;

        if (!o2.as<Vec>().hasElementIndex()) {
          if (!checkSignature(o1, o2))
            goto InvalidInstruction;

          opcode.reset(opData.vectorOp());
          opcode.addImm(q, 30);
          goto EmitOp_Rd0_Rn5_Rm16;
        }
        else {
          if (o2.as<Vec>().elementType() != opData.tElement)
            goto InvalidInstruction;

          if (o2.as<Reg>().id() > 15)
            goto InvalidPhysId;

          uint32_t elementIndex = o2.as<Vec>().elementIndex();
          if (elementIndex > 7u)
            goto InvalidElementIndex;

          opcode.reset(opData.elementOp());
          opcode.addImm(q, 30);
          opcode.addImm(elementIndex & 3u, 20);
          opcode.addImm(elementIndex >> 2, 11);
          goto EmitOp_Rd0_Rn5_Rm16;
        }
      }

      break;
    }

    case InstDB::kEncodingSimdFmov: {
      if (isign4 == ENC_OPS2(Reg, Reg)) {
        // FMOV Gp <-> Vec opcode:
        opcode.reset(0b00011110001001100000000000000000);

        if (o0.as<Reg>().isGp() && o1.as<Reg>().isVec()) {
          // FMOV Wd, Hn      (sf=0 type=11 rmode=00 op=110)
          // FMOV Xd, Hn      (sf=1 type=11 rmode=00 op=110)
          // FMOV Wd, Sn      (sf=0 type=00 rmode=00 op=110)
          // FMOV Xd, Dn      (sf=1 type=11 rmode=00 op=110)
          // FMOV Xd, Vn.d[1] (sf=1 type=10 rmode=01 op=110)
          uint32_t x = o0.as<Reg>().isGpX();
          uint32_t sz = diff(o1.as<Reg>().type(), RegType::kARM_VecH);

          uint32_t type = (sz - 1) & 0x3u;
          uint32_t rModeOp = 0b00110;

          if (o1.as<Vec>().hasElementIndex()) {
            // Special case.
            if (!x || !o1.as<Vec>().isVecD2() || o1.as<Vec>().elementIndex() != 1)
              goto InvalidInstruction;
            type = 0b10;
            rModeOp = 0b01110;
          }
          else {
            // Must be scalar.
            if (sz > 2)
              goto InvalidInstruction;

            if (o1.as<Vec>().hasElementType())
              goto InvalidInstruction;

            if (o1.as<Vec>().isVecS() && x)
              goto InvalidInstruction;

            if (o1.as<Vec>().isVecD() && !x)
              goto InvalidInstruction;
          }

          opcode.addImm(x, 31);
          opcode.addImm(type, 22);
          opcode.addImm(rModeOp, 16);
          goto EmitOp_Rd0_Rn5;
        }

        if (o0.as<Reg>().isVec() && o1.as<Reg>().isGp()) {
          // FMOV Hd, Wn      (sf=0 type=11 rmode=00 op=111)
          // FMOV Hd, Xn      (sf=1 type=11 rmode=00 op=111)
          // FMOV Sd, Wn      (sf=0 type=00 rmode=00 op=111)
          // FMOV Dd, Xn      (sf=1 type=11 rmode=00 op=111)
          // FMOV Vd.d[1], Xn (sf=1 type=10 rmode=01 op=111)
          uint32_t x = o1.as<Reg>().isGpX();
          uint32_t sz = diff(o0.as<Reg>().type(), RegType::kARM_VecH);

          uint32_t type = (sz - 1) & 0x3u;
          uint32_t rModeOp = 0b00111;

          if (o0.as<Vec>().hasElementIndex()) {
            // Special case.
            if (!x || !o0.as<Vec>().isVecD2() || o0.as<Vec>().elementIndex() != 1)
              goto InvalidInstruction;
            type = 0b10;
            rModeOp = 0b01111;
          }
          else {
            // Must be scalar.
            if (sz > 2)
              goto InvalidInstruction;

            if (o0.as<Vec>().hasElementType())
              goto InvalidInstruction;

            if (o0.as<Vec>().isVecS() && x)
              goto InvalidInstruction;

            if (o0.as<Vec>().isVecD() && !x)
              goto InvalidInstruction;
          }

          opcode.addImm(x, 31);
          opcode.addImm(type, 22);
          opcode.addImm(rModeOp, 16);
          goto EmitOp_Rd0_Rn5;
        }

        if (checkSignature(o0, o1)) {
          uint32_t sz = diff(o0.as<Reg>().type(), RegType::kARM_VecH);
          if (sz > 2)
            goto InvalidInstruction;

          if (o0.as<Vec>().hasElementType())
            goto InvalidInstruction;

          uint32_t type = (sz - 1) & 0x3;
          opcode.reset(0b00011110001000000100000000000000);
          opcode.addImm(type, 22);
          goto EmitOp_Rd0_Rn5;
        }
      }

      if (isign4 == ENC_OPS2(Reg, Imm)) {
        if (o0.as<Reg>().isVec()) {
          double fpValue;
          if (o1.as<Imm>().isDouble())
            fpValue = o1.as<Imm>().valueAs<double>();
          else if (o1.as<Imm>().isInt32())
            fpValue = o1.as<Imm>().valueAs<int32_t>();
          else
            goto InvalidImmediate;

          if (!Utils::isFP64Imm8(fpValue))
            goto InvalidImmediate;

          uint32_t imm8 = Utils::encodeFP64ToImm8(fpValue);
          if (!o0.as<Vec>().hasElementType()) {
            // FMOV (scalar, immediate).
            uint32_t sz = diff(o0.as<Reg>().type(), RegType::kARM_VecH);
            uint32_t type = (sz - 1u) & 0x3u;

            if (sz > 2)
              goto InvalidInstruction;

            opcode.reset(0b00011110001000000001000000000000);
            opcode.addImm(type, 22);
            opcode.addImm(imm8, 13);
            goto EmitOp_Rd0;
          }
          else {
            uint32_t q = diff(o0.as<Vec>().type(), RegType::kARM_VecD);
            uint32_t sz = o0.as<Vec>().elementType() - Vec::kElementTypeH;

            if (q > 1 || sz > 2)
              goto InvalidInstruction;

            static const uint32_t szBits[3] = { B(11), B(0), B(29) };
            opcode.reset(0b00001111000000001111010000000000);
            opcode ^= szBits[sz];
            opcode.addImm(q, 30);
            opcode.addImm(imm8 >> 5, 16);
            opcode.addImm(imm8 & 31, 5);
            goto EmitOp_Rd0;
          }
        }
      }

      break;
    }

    case InstDB::kEncodingFSimdPair: {
      const InstDB::EncodingData::FSimdPair& opData = InstDB::EncodingData::fSimdPair[encodingIndex];

      if (isign4 == ENC_OPS2(Reg, Reg)) {
        // This operation is only defined for:
        //   hD, vS.2h (16-bit)
        //   sD, vS.2s (32-bit)
        //   dD, vS.2d (64-bit)
        uint32_t sz = diff(o0.as<Reg>().type(), RegType::kARM_VecH);
        if (sz > 2)
          goto InvalidInstruction;

        static const uint32_t szSignatures[3] = {
          VecS::kSignature | (Vec::kSignatureElementH),
          VecD::kSignature | (Vec::kSignatureElementS),
          VecV::kSignature | (Vec::kSignatureElementD)
        };

        if (o1.signature() != szSignatures[sz])
          goto InvalidInstruction;

        static const uint32_t szBits[] = { B(29), 0, B(22) };
        opcode.reset(opData.scalarOp());
        opcode ^= szBits[sz];
        goto EmitOp_Rd0_Rn5;
      }

      if (isign4 == ENC_OPS3(Reg, Reg, Reg)) {
        if (!checkSignature(o0, o1, o2))
          goto InvalidInstruction;

        uint32_t q = diff(o0.as<Reg>().type(), RegType::kARM_VecD);
        if (q > 1)
          goto InvalidInstruction;

        uint32_t sz = o0.as<Vec>().elementType() - Vec::kElementTypeH;
        if (sz > 2)
          goto InvalidInstruction;

        static const uint32_t szBits[3] = { B(22) | B(21) | B(15) | B(14), 0, B(22) };
        opcode.reset(opData.vectorOp());
        opcode ^= szBits[sz];
        opcode.addImm(q, 30);
        goto EmitOp_Rd0_Rn5_Rm16;
      }

      break;
    }

    // ------------------------------------------------------------------------
    // [ISimd - Instructions]
    // ------------------------------------------------------------------------

    case InstDB::kEncodingISimdSV: {
      const InstDB::EncodingData::ISimdSV& opData = InstDB::EncodingData::iSimdSV[encodingIndex];

      if (isign4 == ENC_OPS2(Reg, Reg)) {
        // The first destination operand is scalar, which matches element-type of source vectors.
        uint32_t L = (instFlags & InstDB::kInstFlagLong) != 0;
        if (diff(o0.as<Vec>().type(), RegType::kARM_VecB) != o1.as<Vec>().elementType() - Vec::kElementTypeB + L)
          goto InvalidInstruction;

        SizeOp sizeOp = armElementTypeToSizeOp(opData.vecOpType, o1.as<Reg>().type(), o1.as<Vec>().elementType());
        if (!sizeOp.isValid())
          goto InvalidInstruction;

        opcode.reset(opData.opcode());
        opcode.addImm(sizeOp.q(), 30);
        opcode.addImm(sizeOp.size(), 22);
        goto EmitOp_Rd0_Rn5;
      }

      break;
    }

    case InstDB::kEncodingISimdVV: {
      const InstDB::EncodingData::ISimdVV& opData = InstDB::EncodingData::iSimdVV[encodingIndex];

      if (isign4 == ENC_OPS2(Reg, Reg)) {
        const Operand_& sop = significantSimdOp(o0, o1, instFlags);
        if (!matchSignature(o0, o1, instFlags))
          goto InvalidInstruction;

        SizeOp sizeOp = armElementTypeToSizeOp(opData.vecOpType, sop.as<Reg>().type(), sop.as<Vec>().elementType());
        if (!sizeOp.isValid())
          goto InvalidInstruction;

        opcode.reset(opData.opcode());
        opcode.addImm(sizeOp.qs(), 30);
        opcode.addImm(sizeOp.scalar(), 28);
        opcode.addImm(sizeOp.size(), 22);
        goto EmitOp_Rd0_Rn5;
      }

      break;
    }

    case InstDB::kEncodingISimdVVx: {
      const InstDB::EncodingData::ISimdVVx& opData = InstDB::EncodingData::iSimdVVx[encodingIndex];

      if (isign4 == ENC_OPS2(Reg, Reg)) {
        if (o0.signature() != opData.op0Signature ||
            o1.signature() != opData.op1Signature)
          goto InvalidInstruction;

        opcode.reset(opData.opcode());
        goto EmitOp_Rd0_Rn5;
      }

      break;
    }

    case InstDB::kEncodingISimdVVV: {
      const InstDB::EncodingData::ISimdVVV& opData = InstDB::EncodingData::iSimdVVV[encodingIndex];

      if (isign4 == ENC_OPS3(Reg, Reg, Reg)) {
        const Operand_& sop = significantSimdOp(o0, o1, instFlags);
        if (!matchSignature(o0, o1, o2, instFlags))
          goto InvalidInstruction;

        SizeOp sizeOp = armElementTypeToSizeOp(opData.vecOpType, sop.as<Reg>().type(), sop.as<Vec>().elementType());
        if (!sizeOp.isValid())
          goto InvalidInstruction;

        opcode.reset(opData.opcode());
        opcode.addImm(sizeOp.qs(), 30);
        opcode.addImm(sizeOp.scalar(), 28);
        opcode.addImm(sizeOp.size(), 22);
        goto EmitOp_Rd0_Rn5_Rm16;
      }

      break;
    }

    case InstDB::kEncodingISimdVVVx: {
      const InstDB::EncodingData::ISimdVVVx& opData = InstDB::EncodingData::iSimdVVVx[encodingIndex];

      if (isign4 == ENC_OPS3(Reg, Reg, Reg)) {
        if (o0.signature() != opData.op0Signature ||
            o1.signature() != opData.op1Signature ||
            o2.signature() != opData.op2Signature)
          goto InvalidInstruction;

        opcode.reset(opData.opcode());
        goto EmitOp_Rd0_Rn5_Rm16;
      }

      break;
    }

    case InstDB::kEncodingISimdWWV: {
      // Special case for wide add/sub [s|b][add|sub][w]{2}.
      const InstDB::EncodingData::ISimdWWV& opData = InstDB::EncodingData::iSimdWWV[encodingIndex];

      if (isign4 == ENC_OPS3(Reg, Reg, Reg)) {
        SizeOp sizeOp = armElementTypeToSizeOp(opData.vecOpType, o2.as<Reg>().type(), o2.as<Vec>().elementType());
        if (!sizeOp.isValid())
          goto InvalidInstruction;

        if (!checkSignature(o0, o1) || !o0.as<Reg>().isVecV() || o0.as<Vec>().elementType() != o2.as<Vec>().elementType() + 1)
          goto InvalidInstruction;

        opcode.reset(opData.opcode());
        opcode.addImm(sizeOp.qs(), 30);
        opcode.addImm(sizeOp.scalar(), 28);
        opcode.addImm(sizeOp.size(), 22);
        goto EmitOp_Rd0_Rn5_Rm16;
      }

      break;
    }

    case InstDB::kEncodingISimdVVVe: {
      const InstDB::EncodingData::ISimdVVVe& opData = InstDB::EncodingData::iSimdVVVe[encodingIndex];

      if (isign4 == ENC_OPS3(Reg, Reg, Reg)) {
        const Operand_& sop = significantSimdOp(o0, o1, instFlags);
        if (!matchSignature(o0, o1, instFlags))
          goto InvalidInstruction;

        if (!o2.as<Vec>().hasElementIndex()) {
          SizeOp sizeOp = armElementTypeToSizeOp(opData.regularVecType, sop.as<Reg>().type(), sop.as<Vec>().elementType());
          if (!sizeOp.isValid())
            goto InvalidInstruction;

          if (!checkSignature(o1, o2))
            goto InvalidInstruction;

          opcode.reset(uint32_t(opData.regularOp) << 10);
          opcode.addImm(sizeOp.qs(), 30);
          opcode.addImm(sizeOp.scalar(), 28);
          opcode.addImm(sizeOp.size(), 22);
          goto EmitOp_Rd0_Rn5_Rm16;
        }
        else {
          SizeOp sizeOp = armElementTypeToSizeOp(opData.elementVecType, sop.as<Reg>().type(), sop.as<Vec>().elementType());
          if (!sizeOp.isValid())
            goto InvalidInstruction;

          uint32_t elementIndex = o2.as<Vec>().elementIndex();
          LMHImm lmh;

          if (!encodeLMH(sizeOp.size(), elementIndex, &lmh))
            goto InvalidElementIndex;

          if (o2.as<Reg>().id() > lmh.maxRmId)
            goto InvalidPhysId;

          opcode.reset(uint32_t(opData.elementOp) << 10);
          opcode.addImm(sizeOp.q(), 30);
          opcode.addImm(sizeOp.size(), 22);
          opcode.addImm(lmh.lm, 20);
          opcode.addImm(lmh.h, 11);
          goto EmitOp_Rd0_Rn5_Rm16;
        }
      }

      break;
    }

    case InstDB::kEncodingISimdVVVI: {
      const InstDB::EncodingData::ISimdVVVI& opData = InstDB::EncodingData::iSimdVVVI[encodingIndex];

      if (isign4 == ENC_OPS4(Reg, Reg, Reg, Imm)) {
        const Operand_& sop = significantSimdOp(o0, o1, instFlags);
        if (!matchSignature(o0, o1, o2, instFlags))
          goto InvalidInstruction;

        SizeOp sizeOp = armElementTypeToSizeOp(opData.vecOpType, sop.as<Reg>().type(), sop.as<Vec>().elementType());
        if (!sizeOp.isValid())
          goto InvalidInstruction;

        uint64_t immValue = o3.as<Imm>().valueAs<uint64_t>();
        uint32_t immSize = opData.immSize;

        if (opData.imm64HasOneBitLess && !sizeOp.q())
          immSize--;

        uint32_t immMax = 1u << immSize;
        if (immValue >= immMax)
          goto InvalidImmediate;

        opcode.reset(opData.opcode());
        opcode.addImm(sizeOp.qs(), 30);
        opcode.addImm(sizeOp.scalar(), 28);
        opcode.addImm(sizeOp.size(), 22);
        opcode.addImm(immValue, opData.immShift);
        goto EmitOp_Rd0_Rn5_Rm16;
      }

      break;
    }

    case InstDB::kEncodingISimdVVVV: {
      const InstDB::EncodingData::ISimdVVVV& opData = InstDB::EncodingData::iSimdVVVV[encodingIndex];

      if (isign4 == ENC_OPS4(Reg, Reg, Reg, Reg)) {
        const Operand_& sop = significantSimdOp(o0, o1, instFlags);
        if (!matchSignature(o0, o1, o2, o3, instFlags))
          goto InvalidInstruction;

        SizeOp sizeOp = armElementTypeToSizeOp(opData.vecOpType, sop.as<Reg>().type(), sop.as<Vec>().elementType());
        if (!sizeOp.isValid())
          goto InvalidInstruction;

        opcode.reset(uint32_t(opData.opcode) << 10);
        opcode.addImm(sizeOp.qs(), 30);
        opcode.addImm(sizeOp.scalar(), 28);
        opcode.addImm(sizeOp.size(), 22);
        goto EmitOp_Rd0_Rn5_Rm16_Ra10;
      }

      break;
    }

    case InstDB::kEncodingISimdVVVVx: {
      const InstDB::EncodingData::ISimdVVVVx& opData = InstDB::EncodingData::iSimdVVVVx[encodingIndex];

      if (isign4 == ENC_OPS4(Reg, Reg, Reg, Reg)) {
        if (o0.signature() != opData.op0Signature ||
            o1.signature() != opData.op1Signature ||
            o2.signature() != opData.op2Signature ||
            o3.signature() != opData.op3Signature)
          goto InvalidInstruction;

        opcode.reset(uint32_t(opData.opcode) << 10);
        goto EmitOp_Rd0_Rn5_Rm16_Ra10;
      }

      break;
    }


    case InstDB::kEncodingISimdPair: {
      const InstDB::EncodingData::ISimdPair& opData = InstDB::EncodingData::iSimdPair[encodingIndex];

      if (isign4 == ENC_OPS2(Reg, Reg) && opData.opcode2) {
        if (o0.as<Vec>().isVecD1() && o1.as<Vec>().isVecD2()) {
          opcode.reset(uint32_t(opData.opcode2) << 10);
          opcode.addImm(0x3, 22); // size.
          goto EmitOp_Rd0_Rn5;
        }
      }

      if (isign4 == ENC_OPS3(Reg, Reg, Reg)) {
        if (!matchSignature(o0, o1, o2, instFlags))
          goto InvalidInstruction;

        SizeOp sizeOp = armElementTypeToSizeOp(opData.opType3, o0.as<Reg>().type(), o0.as<Vec>().elementType());
        if (!sizeOp.isValid())
          goto InvalidInstruction;

        opcode.reset(uint32_t(opData.opcode3) << 10);
        opcode.addImm(sizeOp.qs(), 30);
        opcode.addImm(sizeOp.scalar(), 28);
        opcode.addImm(sizeOp.size(), 22);
        goto EmitOp_Rd0_Rn5_Rm16;
      }

      break;
    }

    case InstDB::kEncodingSimdBicOrr: {
      const InstDB::EncodingData::SimdBicOrr& opData = InstDB::EncodingData::simdBicOrr[encodingIndex];

      if (isign4 == ENC_OPS3(Reg, Reg, Reg)) {
        if (!matchSignature(o0, o1, o2, instFlags))
          goto InvalidInstruction;

        SizeOp sizeOp = armElementTypeToSizeOp(InstDB::kVO_V_B, o0.as<Reg>().type(), o0.as<Vec>().elementType());
        if (!sizeOp.isValid())
          goto InvalidInstruction;

        opcode.reset(uint32_t(opData.registerOp) << 10);
        opcode.addImm(sizeOp.q(), 30);
        goto EmitOp_Rd0_Rn5_Rm16;
      }

      if (isign4 == ENC_OPS2(Reg, Imm) || isign4 == ENC_OPS3(Reg, Imm, Imm)) {
        SizeOp sizeOp = armElementTypeToSizeOp(InstDB::kVO_V_HS, o0.as<Reg>().type(), o0.as<Vec>().elementType());
        if (!sizeOp.isValid())
          goto InvalidInstruction;

        if (o1.as<Imm>().valueAs<uint64_t>() > 0xFFFFFFFFu)
          goto InvalidImmediate;

        uint32_t imm = o1.as<Imm>().valueAs<uint32_t>();
        uint32_t shift = 0;
        uint32_t maxShift = (8u << sizeOp.size()) - 8u;

        if (o2.isImm()) {
          if (o2.as<Imm>().predicate() != uint32_t(ShiftOp::kLSL))
            goto InvalidImmediate;

          if (imm > 0xFFu || o2.as<Imm>().valueAs<uint64_t>() > maxShift)
            goto InvalidImmediate;

          shift = o2.as<Imm>().valueAs<uint32_t>();
          if ((shift & 0x7u) != 0u)
            goto InvalidImmediate;
        }
        else if (imm) {
          shift = Support::ctz(imm) & 0x7u;
          imm >>= shift;

          if (imm > 0xFFu || shift > maxShift)
            goto InvalidImmediate;
        }

        uint32_t cmode = 0x1u | ((shift / 8u) << 1);
        if (sizeOp.size() == 1)
          cmode |= B(3);

        // The immediate value is split into ABC and DEFGH parts.
        uint32_t abc = (imm >> 5) & 0x7u;
        uint32_t defgh = imm & 0x1Fu;

        opcode.reset(uint32_t(opData.immediateOp) << 10);
        opcode.addImm(sizeOp.q(), 30);
        opcode.addImm(abc, 16);
        opcode.addImm(cmode, 12);
        opcode.addImm(defgh, 5);
        goto EmitOp_Rd0;
      }

      break;
    }

    case InstDB::kEncodingSimdCmp: {
      const InstDB::EncodingData::SimdCmp& opData = InstDB::EncodingData::simdCmp[encodingIndex];

      if (isign4 == ENC_OPS3(Reg, Reg, Reg) && opData.regOp) {
        if (!matchSignature(o0, o1, o2, instFlags))
          goto InvalidInstruction;

        SizeOp sizeOp = armElementTypeToSizeOp(opData.vecOpType, o0.as<Reg>().type(), o0.as<Vec>().elementType());
        if (!sizeOp.isValid())
          goto InvalidInstruction;

        opcode.reset(uint32_t(opData.regOp) << 10);
        opcode.addImm(sizeOp.qs(), 30);
        opcode.addImm(sizeOp.scalar(), 28);
        opcode.addImm(sizeOp.size(), 22);
        goto EmitOp_Rd0_Rn5_Rm16;
      }

      if (isign4 == ENC_OPS3(Reg, Reg, Imm) && opData.zeroOp) {
        if (!matchSignature(o0, o1, instFlags))
          goto InvalidInstruction;

        if (o2.as<Imm>().value() != 0)
          goto InvalidImmediate;

        SizeOp sizeOp = armElementTypeToSizeOp(opData.vecOpType, o0.as<Reg>().type(), o0.as<Vec>().elementType());
        if (!sizeOp.isValid())
          goto InvalidInstruction;

        opcode.reset(uint32_t(opData.zeroOp) << 10);
        opcode.addImm(sizeOp.qs(), 30);
        opcode.addImm(sizeOp.scalar(), 28);
        opcode.addImm(sizeOp.size(), 22);
        goto EmitOp_Rd0_Rn5;
      }

      break;
    }

    case InstDB::kEncodingSimdDot: {
      const InstDB::EncodingData::SimdDot& opData = InstDB::EncodingData::simdDot[encodingIndex];

      if (isign4 == ENC_OPS3(Reg, Reg, Reg)) {
        uint32_t q = diff(o0.as<Reg>().type(), RegType::kARM_VecD);
        uint32_t size = 2;

        if (q > 1u)
          goto InvalidInstruction;

        if (!o2.as<Vec>().hasElementIndex()) {
          if (!opData.vectorOp)
            goto InvalidInstruction;

          if (o0.as<Reg>().type() != o1.as<Reg>().type() || o1.as<Reg>().type() != o2.as<Reg>().type())
            goto InvalidInstruction;

          if (o0.as<Vec>().elementType() != opData.tA ||
              o1.as<Vec>().elementType() != opData.tB ||
              o2.as<Vec>().elementType() != opData.tB)
            goto InvalidInstruction;

          opcode.reset(uint32_t(opData.vectorOp) << 10);
          opcode.addImm(q, 30);
          goto EmitOp_Rd0_Rn5_Rm16;
        }
        else {
          if (!opData.elementOp)
            goto InvalidInstruction;

          if (o0.as<Reg>().type() != o1.as<Reg>().type() || !o2.as<Reg>().isVecV())
            goto InvalidInstruction;

          if (o0.as<Vec>().elementType() != opData.tA ||
              o1.as<Vec>().elementType() != opData.tB ||
              o2.as<Vec>().elementType() != opData.tElement)
            goto InvalidInstruction;

          uint32_t elementIndex = o2.as<Vec>().elementIndex();
          LMHImm lmh;

          if (!encodeLMH(size, elementIndex, &lmh))
            goto InvalidElementIndex;

          if (o2.as<Reg>().id() > lmh.maxRmId)
            goto InvalidPhysId;

          opcode.reset(uint32_t(opData.elementOp) << 10);
          opcode.addImm(q, 30);
          opcode.addImm(lmh.lm, 20);
          opcode.addImm(lmh.h, 11);
          goto EmitOp_Rd0_Rn5_Rm16;
        }
      }

      break;
    }

    case InstDB::kEncodingSimdDup: SimdDup: {
      if (isign4 == ENC_OPS2(Reg, Reg)) {
        // Truth table of valid encodings of `Q:1|ElementType:3`
        uint32_t kValidEncodings = B(Vec::kElementTypeB + 0) |
                                   B(Vec::kElementTypeH + 0) |
                                   B(Vec::kElementTypeS + 0) |
                                   B(Vec::kElementTypeB + 8) |
                                   B(Vec::kElementTypeH + 8) |
                                   B(Vec::kElementTypeS + 8) |
                                   B(Vec::kElementTypeD + 8) ;

        uint32_t q = diff(o0.as<Reg>().type(), RegType::kARM_VecD);

        if (o1.as<Reg>().isGp()) {
          // DUP - Vec (scalar|vector) <- GP register.
          //
          // NOTE: This is only scalar for `dup d, x` case, otherwise the value
          // would be duplicated across all vector elements (1, 2, 4, 8, or 16).
          uint32_t elementType = o0.as<Vec>().elementType();
          if (q > 1 || !Support::bitTest(kValidEncodings, (q << 3) | elementType))
            goto InvalidInstruction;

          uint32_t lsbIndex = elementType - 1u;
          uint32_t imm5 = 1u << lsbIndex;

          opcode.reset(0b0000111000000000000011 << 10);
          opcode.addImm(q, 30);
          opcode.addImm(imm5, 16);
          goto EmitOp_Rd0_Rn5;
        }

        if (!o1.as<Reg>().isVec() || !o1.as<Vec>().hasElementIndex())
          goto InvalidInstruction;

        uint32_t dstIndex = o1.as<Vec>().elementIndex();
        if (!o0.as<Vec>().hasElementType()) {
          // DUP - Vec (scalar) <- Vec[N].
          uint32_t lsbIndex = diff(o0.as<Reg>().type(), RegType::kARM_VecB);

          if (lsbIndex != o1.as<Vec>().elementType() - Vec::kElementTypeB || lsbIndex > 3)
            goto InvalidInstruction;

          uint32_t imm5 = ((dstIndex << 1) | 1u) << lsbIndex;
          if (imm5 > 31)
            goto InvalidElementIndex;

          opcode.reset(0b0101111000000000000001 << 10);
          opcode.addImm(imm5, 16);
          goto EmitOp_Rd0_Rn5;
        }
        else {
          // DUP - Vec (all) <- Vec[N].
          uint32_t elementType = o0.as<Vec>().elementType();
          if (q > 1 || !Support::bitTest(kValidEncodings, (q << 3) | elementType))
            goto InvalidInstruction;

          uint32_t lsbIndex = elementType - 1u;
          uint32_t imm5 = ((dstIndex << 1) | 1u) << lsbIndex;

          if (imm5 > 31)
            goto InvalidElementIndex;

          opcode.reset(0b0000111000000000000001 << 10);
          opcode.addImm(q, 30);
          opcode.addImm(imm5, 16);
          goto EmitOp_Rd0_Rn5;
        }
      }

      break;
    }

    case InstDB::kEncodingSimdIns: SimdIns: {
      if (isign4 == ENC_OPS2(Reg, Reg) && o0.as<Reg>().isVecV()) {
        if (!o0.as<Vec>().hasElementIndex())
          goto InvalidInstruction;

        uint32_t elementType = o0.as<Vec>().elementType();
        uint32_t dstIndex = o0.as<Vec>().elementIndex();
        uint32_t lsbIndex = elementType - 1u;

        uint32_t imm5 = ((dstIndex << 1) | 1u) << lsbIndex;
        if (imm5 > 31)
          goto InvalidElementIndex;

        if (o1.as<Reg>().isGp()) {
          // INS - Vec[N] <- GP register.
          opcode.reset(0b0100111000000000000111 << 10);
          opcode.addImm(imm5, 16);
          goto EmitOp_Rd0_Rn5;
        }
        else if (o1.as<Reg>().isVecV() && o1.as<Vec>().hasElementIndex()) {
          // INS - Vec[N] <- Vec[M].
          if (o0.as<Vec>().elementType() != o1.as<Vec>().elementType())
            goto InvalidInstruction;

          uint32_t srcIndex = o1.as<Vec>().elementIndex();
          if (o0.as<Reg>().type() != o1.as<Reg>().type())
            goto InvalidInstruction;

          uint32_t imm4 = srcIndex << lsbIndex;
          if (imm4 > 15)
            goto InvalidElementIndex;

          opcode.reset(0b0110111000000000000001 << 10);
          opcode.addImm(imm5, 16);
          opcode.addImm(imm4, 11);
          goto EmitOp_Rd0_Rn5;
        }
      }

      break;
    }

    case InstDB::kEncodingSimdMov: {
      if (isign4 == ENC_OPS2(Reg, Reg)) {
        if (o0.as<Reg>().isVec() && o1.as<Reg>().isVec()) {
          // INS v.x[index], v.x[index].
          if (o0.as<Vec>().hasElementIndex() && o1.as<Vec>().hasElementIndex())
            goto SimdIns;

          // DUP {b|h|s|d}, v.{b|h|s|d}[index].
          if (o1.as<Vec>().hasElementIndex())
            goto SimdDup;

          if (!checkSignature(o0, o1))
            goto InvalidInstruction;

          // ORR Vd, Vn, Vm
          uint32_t q = diff(o0.as<Reg>().type(), RegType::kARM_VecD);
          if (q > 1)
            goto InvalidInstruction;

          opcode.reset(0b0000111010100000000111 << 10);
          opcode.addImm(q, 30);
          opcode.addReg(o1, 16); // Vn == Vm.
          goto EmitOp_Rd0_Rn5;
        }

        if (o0.as<Reg>().isVec() && o1.as<Reg>().isGp()) {
          // INS v.x[index], Rn.
          if (o0.as<Vec>().hasElementIndex())
            goto SimdIns;

          goto InvalidInstruction;
        }

        if (o0.as<Reg>().isGp() && o1.as<Reg>().isVec()) {
          // UMOV Rd, V.{s|d}[index].
          encodingIndex = 1;
          goto SimdUmov;
        }
      }

      break;
    }

    case InstDB::kEncodingSimdMoviMvni: {
      const InstDB::EncodingData::SimdMoviMvni& opData = InstDB::EncodingData::simdMoviMvni[encodingIndex];

      if (isign4 == ENC_OPS2(Reg, Imm) || isign4 == ENC_OPS3(Reg, Imm, Imm)) {
        SizeOp sizeOp = armElementTypeToSizeOp(InstDB::kVO_V_Any, o0.as<Reg>().type(), o0.as<Vec>().elementType());
        if (!sizeOp.isValid())
          goto InvalidInstruction;

        uint64_t imm64 = o1.as<Imm>().valueAs<uint64_t>();
        uint32_t imm8 = 0;
        uint32_t cmode = 0;
        uint32_t inverted = opData.inverted;
        uint32_t op = 0;
        uint32_t shift = 0;
        uint32_t shiftOp = uint32_t(ShiftOp::kLSL);

        if (sizeOp.size() == 3u) {
          // The second immediate should not be present, however, we accept
          // an immediate value of zero as some user code may still pass it.
          if (o2.isImm() && o0.as<Imm>().value() != 0)
            goto InvalidImmediate;

          if (Utils::isByteMaskImm8(imm64)) {
            imm8 = encodeImm64ByteMaskToImm8(imm64);
          }
          else {
            // Change from D to S and from 64-bit imm to 32-bit imm if this
            // is not a byte-mask pattern.
            if ((imm64 >> 32) == (imm64 & 0xFFFFFFFFu)) {
              imm64 &= 0xFFFFFFFFu;
              sizeOp.decrementSize();
            }
            else {
              goto InvalidImmediate;
            }
          }
        }

        if (sizeOp.size() < 3u) {
          if (imm64 > 0xFFFFFFFFu)
            goto InvalidImmediate;
          imm8 = uint32_t(imm64);

          if (sizeOp.size() == 2) {
            if ((imm8 >> 16) == (imm8 & 0xFFFFu)) {
              imm8 >>= 16;
              sizeOp.decrementSize();
            }
          }

          if (sizeOp.size() == 1) {
            if (imm8 > 0xFFFFu)
              goto InvalidImmediate;

            if ((imm8 >> 8) == (imm8 & 0xFFu)) {
              imm8 >>= 8;
              sizeOp.decrementSize();
            }
          }

          uint32_t maxShift = (8u << sizeOp.size()) - 8u;
          if (o2.isImm()) {
            if (imm8 > 0xFFu || o2.as<Imm>().valueAs<uint64_t>() > maxShift)
              goto InvalidImmediate;

            shift = o2.as<Imm>().valueAs<uint32_t>();
            shiftOp = o2.as<Imm>().predicate();
          }
          else if (imm8) {
            shift = Support::ctz(imm8) & ~0x7u;
            imm8 >>= shift;

            if (imm8 > 0xFFu || shift > maxShift)
              goto InvalidImmediate;
          }

          if ((shift & 0x7u) != 0u)
            goto InvalidImmediate;
        }

        shift /= 8u;

        switch (sizeOp.size()) {
          case 0:
            if (shiftOp != uint32_t(ShiftOp::kLSL))
              goto InvalidImmediate;

            if (inverted) {
              imm8 = ~imm8 & 0xFFu;
              inverted = 0;
            }

            cmode = B(3) | B(2) | B(1);
            break;

          case 1:
            if (shiftOp != uint32_t(ShiftOp::kLSL))
              goto InvalidImmediate;

            cmode = B(3) | (shift << 1);
            op = inverted;
            break;

          case 2:
            if (shiftOp == uint32_t(ShiftOp::kLSL)) {
              cmode = shift << 1;
            }
            else if (shiftOp == uint32_t(ShiftOp::kMSL)) {
              if (shift == 0 || shift > 2)
                goto InvalidImmediate;
              cmode = B(3) | B(2) | (shift - 1u);
            }
            else {
              goto InvalidImmediate;
            }

            op = inverted;
            break;

          case 3:
            if (inverted) {
              imm8 = ~imm8 & 0xFFu;
              inverted = 0;
            }

            op = 1;
            cmode = B(3) | B(2) | B(1);
            break;
        }

        // The immediate value is split into ABC and DEFGH parts.
        uint32_t abc = (imm8 >> 5) & 0x7u;
        uint32_t defgh = imm8 & 0x1Fu;

        opcode.reset(uint32_t(opData.opcode) << 10);
        opcode.addImm(sizeOp.q(), 30);
        opcode.addImm(op, 29);
        opcode.addImm(abc, 16);
        opcode.addImm(cmode, 12);
        opcode.addImm(defgh, 5);
        goto EmitOp_Rd0;
      }

      break;
    }

    case InstDB::kEncodingSimdShift: {
      const InstDB::EncodingData::SimdShift& opData = InstDB::EncodingData::simdShift[encodingIndex];

      const Operand_& sop = significantSimdOp(o0, o1, instFlags);
      SizeOp sizeOp = armElementTypeToSizeOp(opData.vecOpType, sop.as<Reg>().type(), sop.as<Vec>().elementType());

      if (!sizeOp.isValid())
        goto InvalidInstruction;

      if (isign4 == ENC_OPS3(Reg, Reg, Imm) && opData.immediateOp) {
        if (!matchSignature(o0, o1, instFlags))
          goto InvalidInstruction;

        if (o2.as<Imm>().valueAs<uint64_t>() > 63)
          goto InvalidImmediate;

        uint32_t lsbShift = sizeOp.size() + 3u;
        uint32_t lsbMask = (1u << lsbShift) - 1u;
        uint32_t imm = o2.as<Imm>().valueAs<uint32_t>();

        // Some instructions use IMM and some X - IMM, so negate if required.
        if (opData.invertedImm) {
          if (imm == 0 || imm > (1u << lsbShift))
            goto InvalidImmediate;
          imm = Support::neg(imm) & lsbMask;
        }

        if (imm > lsbMask)
          goto InvalidImmediate;
        imm |= (1u << lsbShift);

        opcode.reset(uint32_t(opData.immediateOp) << 10);
        opcode.addImm(sizeOp.qs(), 30);
        opcode.addImm(sizeOp.scalar(), 28);
        opcode.addImm(imm, 16);
        goto EmitOp_Rd0_Rn5;
      }

      if (isign4 == ENC_OPS3(Reg, Reg, Reg) && opData.registerOp) {
        if (!matchSignature(o0, o1, o2, instFlags))
          goto InvalidInstruction;

        opcode.reset(uint32_t(opData.registerOp) << 10);
        opcode.addImm(sizeOp.qs(), 30);
        opcode.addImm(sizeOp.scalar(), 28);
        opcode.addImm(sizeOp.size(), 22);
        goto EmitOp_Rd0_Rn5_Rm16;
      }

      break;
    }

    case InstDB::kEncodingSimdShiftES: {
      const InstDB::EncodingData::SimdShiftES& opData = InstDB::EncodingData::simdShiftES[encodingIndex];

      if (isign4 == ENC_OPS3(Reg, Reg, Imm)) {
        SizeOp sizeOp = armElementTypeToSizeOp(opData.vecOpType, o1.as<Reg>().type(), o1.as<Vec>().elementType());
        if (!sizeOp.isValid())
          goto InvalidInstruction;

        if (!matchSignature(o0, o1, instFlags))
          goto InvalidInstruction;

        // The immediate value must match the element size.
        uint64_t shift = o2.as<Imm>().valueAs<uint64_t>();
        uint32_t shiftOp = o2.as<Imm>().predicate();

        if (shift != (8u << sizeOp.size()) || shiftOp != uint32_t(ShiftOp::kLSL))
          goto InvalidImmediate;

        opcode.reset(uint32_t(opData.opcode) << 10);
        opcode.addImm(sizeOp.q(), 30);
        opcode.addImm(sizeOp.size(), 22);
        goto EmitOp_Rd0_Rn5;
      }

      break;
    }

    case InstDB::kEncodingSimdSm3tt: {
      const InstDB::EncodingData::SimdSm3tt& opData = InstDB::EncodingData::simdSm3tt[encodingIndex];

      if (isign4 == ENC_OPS3(Reg, Reg, Reg)) {
        if (o0.as<Vec>().isVecS4() && o1.as<Vec>().isVecS4() && o2.as<Vec>().isVecS4() && o2.as<Vec>().hasElementIndex()) {
          uint32_t imm2 = o2.as<Vec>().elementIndex();
          if (imm2 > 3)
            goto InvalidElementIndex;

          opcode.reset(uint32_t(opData.opcode) << 10);
          opcode.addImm(imm2, 12);
          goto EmitOp_Rd0_Rn5_Rm16;
        }
      }

      break;
    }


    case InstDB::kEncodingSimdSmovUmov: SimdUmov: {
      const InstDB::EncodingData::SimdSmovUmov& opData = InstDB::EncodingData::simdSmovUmov[encodingIndex];

      if (isign4 == ENC_OPS2(Reg, Reg) && o0.as<Reg>().isGp() && o1.as<Reg>().isVec()) {
        SizeOp sizeOp = armElementTypeToSizeOp(opData.vecOpType, o1.as<Reg>().type(), o1.as<Vec>().elementType());
        if (!sizeOp.isValid())
          goto InvalidInstruction;

        if (!o1.as<Vec>().hasElementIndex())
          goto InvalidInstruction;

        uint32_t x = o0.as<Gp>().isGpX();
        uint32_t gpMustBeX = uint32_t(sizeOp.size() >= 3u - opData.isSigned);

        if (opData.isSigned) {
          if (gpMustBeX && !x)
            goto InvalidInstruction;
        }
        else {
          if (x != gpMustBeX)
            goto InvalidInstruction;
        }

        uint32_t elementIndex = o1.as<Vec>().elementIndex();
        uint32_t maxElementIndex = 15u >> sizeOp.size();

        if (elementIndex > maxElementIndex)
          goto InvalidElementIndex;

        uint32_t imm5 = (1u | (elementIndex << 1)) << sizeOp.size();

        opcode.reset(uint32_t(opData.opcode) << 10);
        opcode.addImm(x, 30);
        opcode.addImm(imm5, 16);
        goto EmitOp_Rd0_Rn5;
      }

      break;
    }

    case InstDB::kEncodingSimdSxtlUxtl: {
      const InstDB::EncodingData::SimdSxtlUxtl& opData = InstDB::EncodingData::simdSxtlUxtl[encodingIndex];

      if (isign4 == ENC_OPS2(Reg, Reg)) {
        SizeOp sizeOp = armElementTypeToSizeOp(opData.vecOpType, o1.as<Reg>().type(), o1.as<Vec>().elementType());
        if (!sizeOp.isValid())
          goto InvalidInstruction;

        if (!matchSignature(o0, o1, instFlags))
          goto InvalidInstruction;

        opcode.reset(uint32_t(opData.opcode) << 10);
        opcode.addImm(sizeOp.q(), 30);
        opcode.addImm(1u, sizeOp.size() + 19);
        goto EmitOp_Rd0_Rn5;
      }

      break;
    }

    case InstDB::kEncodingSimdTblTbx: {
      const InstDB::EncodingData::SimdTblTbx& opData = InstDB::EncodingData::simdTblTbx[encodingIndex];

      if (isign4 == ENC_OPS3(Reg, Reg, Reg) || isign4 == ENC_OPS4(Reg, Reg, Reg, Reg)) {
        // TBL/TBX <Vd>.<Ta>, { <Vn>.16B }, <Vm>.<Ta>
        // TBL/TBX <Vd>.<Ta>, { <Vn>.16B, <Vn+1>.16B }, <Vm>.<Ta>
        // TBL/TBX <Vd>.<Ta>, { <Vn>.16B, <Vn+1>.16B, <Vn+2>.16B }, <Vm>.<Ta>
        // TBL/TBX <Vd>.<Ta>, { <Vn>.16B, <Vn+1>.16B, <Vn+2>.16B, <Vn+3>.16B }, <Vm>.<Ta>
        opcode.reset(uint32_t(opData.opcode) << 10);

        const Operand_& o4 = opExt[EmitterUtils::kOp4];
        const Operand_& o5 = opExt[EmitterUtils::kOp5];

        uint32_t q = diff(o0.as<Reg>().type(), RegType::kARM_VecD);
        if (q > 1 || o0.as<Vec>().hasElementIndex())
          goto InvalidInstruction;

        if (!o1.as<Vec>().isVecB16() || o1.as<Vec>().hasElementIndex())
          goto InvalidInstruction;

        uint32_t len = uint32_t(!o3.isNone()) + uint32_t(!o4.isNone()) + uint32_t(!o5.isNone());
        opcode.addImm(q, 30);
        opcode.addImm(len, 13);

        switch (len) {
          case 0:
            if (!checkSignature(o0, o2))
              goto InvalidInstruction;

            if (o2.id() > 31)
              goto InvalidPhysId;

            opcode.addReg(o2, 16);
            goto EmitOp_Rd0_Rn5;

          case 1:
            if (!checkSignature(o0, o3))
              goto InvalidInstruction;

            if (o3.id() > 31)
              goto InvalidPhysId;

            opcode.addReg(o3, 16);
            goto EmitOp_Rd0_Rn5;

          case 2:
            if (!checkSignature(o0, o4))
              goto InvalidInstruction;

            if (o4.id() > 31)
              goto InvalidPhysId;

            opcode.addReg(o4, 16);
            goto EmitOp_Rd0_Rn5;

          case 3:
            if (!checkSignature(o0, o5))
              goto InvalidInstruction;

            if (o5.id() > 31)
              goto InvalidPhysId;

            opcode.addReg(o5, 16);
            goto EmitOp_Rd0_Rn5;

          default:
            // Should never happen.
            goto InvalidInstruction;
        }
      }

      break;
    }

    // ------------------------------------------------------------------------
    // [Simd - Load / Store]
    // ------------------------------------------------------------------------

    case InstDB::kEncodingSimdLdSt: {
      const InstDB::EncodingData::SimdLdSt& opData = InstDB::EncodingData::simdLdSt[encodingIndex];

      if (isign4 == ENC_OPS2(Reg, Mem)) {
        const Mem& m = o1.as<Mem>();
        rmRel = &m;

        // Width  |       SZ |        XY | XSZ
        // -------+----------+-----------+-----
        // 8-bit  | size==00 | opc == 01 | 000
        // 16-bit | size==01 | opc == 01 | 001
        // 32-bit | size==10 | opc == 01 | 010
        // 64-bit | size==11 | opc == 01 | 011
        // 128-bit| size==00 | opc == 11 | 100
        uint32_t xsz = diff(o0.as<Reg>().type(), RegType::kARM_VecB);
        if (xsz > 4u || o0.as<Vec>().hasElementIndex())
          goto InvalidRegType;

        if (!checkVecId(o0))
          goto InvalidPhysId;

        if (!armCheckMemBaseIndexRel(m))
          goto InvalidAddress;

        int64_t offset = m.offset();
        if (m.hasBaseReg()) {
          // [Base {Offset | Index}]
          if (m.hasIndex()) {
            uint32_t opt = armShiftOpToLdStOptMap[m.predicate()];
            if (opt == 0xFFu)
              goto InvalidAddress;

            uint32_t shift = m.shift();
            uint32_t s = (shift != 0);

            if (s && shift != xsz)
              goto InvalidAddressScale;

            opcode.reset(uint32_t(opData.registerOp) << 21);
            opcode.addImm(xsz & 3u, 30);
            opcode.addImm(xsz >> 2, 23);
            opcode.addImm(opt, 13);
            opcode.addImm(s, 12);
            opcode |= B(11);
            opcode.addReg(o0, 0);
            goto EmitOp_MemBaseIndex_Rn5_Rm16;
          }

          // Makes it easier to work with the offset especially on 32-bit arch.
          if (!Support::isInt32(offset))
            goto InvalidDisplacement;
          int32_t offset32 = int32_t(offset);

          if (m.isPreOrPost()) {
            if (!Support::isInt9(offset32))
              goto InvalidDisplacement;

            opcode.reset(uint32_t(opData.prePostOp) << 21);
            opcode.addImm(xsz & 3u, 30);
            opcode.addImm(xsz >> 2, 23);
            opcode.addImm(offset32 & 0x1FF, 12);
            opcode.addImm(m.isPreIndex(), 11);
            opcode |= B(10);
            opcode.addReg(o0, 0);
            goto EmitOp_MemBase_Rn5;
          }
          else {
            uint32_t imm12 = uint32_t(offset32) >> xsz;

            // If this instruction is not encodable with scaled unsigned offset, try unscaled signed offset.
            if (!Support::isUInt12(imm12) || (imm12 << xsz) != uint32_t(offset32)) {
              instId = opData.uAltInstId;
              instInfo = &InstDB::_instInfoTable[instId];
              encodingIndex = instInfo->_encodingDataIndex;
              goto Case_SimdLdurStur;
            }

            opcode.reset(uint32_t(opData.uOffsetOp) << 22);
            opcode.addImm(xsz & 3u, 30);
            opcode.addImm(xsz >> 2, 23);
            opcode.addImm(imm12, 10);
            opcode.addReg(o0, 0);
            goto EmitOp_MemBase_Rn5;
          }
        }
        else {
          if (!opData.literalOp)
            goto InvalidAddress;

          if (xsz < 2u)
            goto InvalidRegType;

          uint32_t opc = xsz - 2u;
          opcode.reset(uint32_t(opData.literalOp) << 24);
          opcode.addImm(opc, 30);
          opcode.addReg(o0, 0);
          offsetFormat.resetToImmValue(OffsetType::kSignedOffset, 4, 5, 19, 2);
          goto EmitOp_Rel;
        }
      }

      break;
    }

    case InstDB::kEncodingSimdLdpStp: {
      const InstDB::EncodingData::SimdLdpStp& opData = InstDB::EncodingData::simdLdpStp[encodingIndex];

      if (isign4 == ENC_OPS3(Reg, Reg, Mem)) {
        const Mem& m = o2.as<Mem>();
        rmRel = &m;

        uint32_t opc = diff(o0.as<Reg>().type(), RegType::kARM_VecS);
        if (opc > 2u || o0.as<Vec>().hasElementTypeOrIndex())
          goto InvalidInstruction;

        if (!checkSignature(o0, o1))
          goto InvalidInstruction;

        if (!checkVecId(o0, o1))
          goto InvalidPhysId;

        if (m.baseType() != RegType::kARM_GpX || m.hasIndex())
          goto InvalidAddress;

        if (m.isOffset64Bit())
          goto InvalidDisplacement;

        uint32_t offsetShift = 2u + opc;
        int32_t offset32 = m.offsetLo32() >> offsetShift;

        // Make sure we didn't lose bits by applying the mandatory offset shift.
        if (Support::shl(offset32, offsetShift) != m.offsetLo32())
          goto InvalidDisplacement;

        // Offset is encoded as a 7-bit immediate.
        if (!Support::isInt7(offset32))
          goto InvalidDisplacement;

        if (m.isPreOrPost() && offset32 != 0) {
          if (!opData.prePostOp)
            goto InvalidAddress;

          opcode.reset(uint32_t(opData.prePostOp) << 22);
          opcode.addImm(m.isPreIndex(), 24);
        }
        else {
          opcode.reset(uint32_t(opData.offsetOp) << 22);
        }

        opcode.addImm(opc, 30);
        opcode.addImm(offset32 & 0x7F, 15);
        opcode.addReg(o1, 10);
        opcode.addReg(o0, 0);
        goto EmitOp_MemBase_Rn5;
      }

      break;
    }

    case InstDB::kEncodingSimdLdurStur: {
Case_SimdLdurStur:
      const InstDB::EncodingData::SimdLdurStur& opData = InstDB::EncodingData::simdLdurStur[encodingIndex];

      if (isign4 == ENC_OPS2(Reg, Mem)) {
        const Mem& m = o1.as<Mem>();
        rmRel = &m;

        uint32_t sz = diff(o0.as<Reg>().type(), RegType::kARM_VecB);
        if (sz > 4 || o0.as<Vec>().hasElementTypeOrIndex())
          goto InvalidInstruction;

        if (!checkVecId(o0))
          goto InvalidPhysId;

        if (!armCheckMemBaseIndexRel(m))
          goto InvalidAddress;

        if (m.hasBaseReg() && !m.hasIndex() && !m.isPreOrPost()) {
          if (m.isOffset64Bit())
            goto InvalidDisplacement;

          int32_t offset32 = m.offsetLo32();
          if (!Support::isInt9(offset32))
            goto InvalidDisplacement;

          opcode.reset(uint32_t(opData.opcode) << 10);
          opcode.addImm(sz & 3u, 30);
          opcode.addImm(sz >> 2, 23);
          opcode.addImm(offset32 & 0x1FF, 12);
          opcode.addReg(o0, 0);
          goto EmitOp_MemBase_Rn5;
        }

        goto InvalidAddress;
      }

      break;
    }

    case InstDB::kEncodingSimdLdNStN: {
      const InstDB::EncodingData::SimdLdNStN& opData = InstDB::EncodingData::simdLdNStN[encodingIndex];
      const Operand_& o4 = opExt[EmitterUtils::kOp4];

      uint32_t n = 1;

      if (isign4 == ENC_OPS2(Reg, Mem)) {
        if (opData.n != 1)
          goto InvalidInstruction;

        rmRel = &o1;
      }
      else if (isign4 == ENC_OPS3(Reg, Reg, Mem)) {
        if (opData.n != 1 && opData.n != 2)
          goto InvalidInstruction;

        if (!checkSignature(o0, o1) || !checkConsecutive(o0, o1))
          goto InvalidInstruction;

        n = 2;
        rmRel = &o2;
      }
      else if (isign4 == ENC_OPS4(Reg, Reg, Reg, Mem) && o4.isNone()) {
        if (opData.n != 1 && opData.n != 3)
          goto InvalidInstruction;

        if (!checkSignature(o0, o1, o2) || !checkConsecutive(o0, o1, o2))
          goto InvalidInstruction;

        n = 3;
        rmRel = &o3;
      }
      else if (isign4 == ENC_OPS4(Reg, Reg, Reg, Reg) && o4.isMem()) {
        if (opData.n != 1 && opData.n != 4)
          goto InvalidInstruction;

        if (!checkSignature(o0, o1, o2, o3) || !checkConsecutive(o0, o1, o2, o3))
          goto InvalidInstruction;

        n = 4;
        rmRel = &o4;
      }
      else {
        goto InvalidInstruction;
      }

      // We will use `v` and `m` from now as those are relevant for encoding.
      const Vec& v = o0.as<Vec>();
      const Mem& m = rmRel->as<Mem>();

      uint32_t q = 0;
      uint32_t rm = 0;
      uint32_t rn = m.baseId();
      uint32_t sz = v.elementType() - Vec::kElementTypeB;
      uint32_t opcSsize = sz;
      uint32_t offsetPossibility = 0;

      if (sz > 3)
        goto InvalidInstruction;

      if (m.baseType() != RegType::kARM_GpX)
        goto InvalidAddress;

      // Rn cannot be ZR, but can be SP.
      if (rn > 30 && rn != Gp::kIdSp)
        goto InvalidAddress;

      rn &= 31;

      if (opData.replicate) {
        if (n != opData.n)
          goto InvalidInstruction;

        // Replicates to the whole register, element index cannot be used.
        if (v.hasElementIndex())
          goto InvalidInstruction;

        q = diff(v.type(), RegType::kARM_VecD);
        if (q > 1)
          goto InvalidInstruction;

        opcode.reset(uint32_t(opData.singleOp) << 10);
        offsetPossibility = (1u << sz) * n;
      }
      else if (v.hasElementIndex()) {
        if (n != opData.n)
          goto InvalidInstruction;

        // LDx/STx (single structure).
        static const uint8_t opcSsizeBySzS[] = { 0x0u << 3, 0x2u << 3, 0x4u << 3, (0x4u << 3) | 1u };

        opcode.reset(uint32_t(opData.singleOp) << 10);
        opcSsize = opcSsizeBySzS[sz];
        offsetPossibility =  (1u << sz) * opData.n;

        uint32_t elementIndex = v.elementIndex();
        uint32_t maxElementIndex = 15 >> sz;

        if (elementIndex > maxElementIndex)
          goto InvalidElementIndex;

        elementIndex <<= sz;
        q = elementIndex >> 3;
        opcSsize |= elementIndex & 0x7u;
      }
      else {
        // LDx/STx (multiple structures).
        static const uint8_t opcSsizeByN[] = { 0u, 0x7u << 2, 0xAu << 2, 0x6u << 2, 0x2u << 2 };

        q = diff(v.type(), RegType::kARM_VecD);
        if (q > 1)
          goto InvalidInstruction;

        if (opData.n == 1)
          opcSsize |= opcSsizeByN[n];

        opcode.reset(uint32_t(opData.multipleOp) << 10);
        offsetPossibility = (8u << q) * n;
      }

      if (m.hasIndex()) {
        if (m.hasOffset() || !m.isPostIndex())
          goto InvalidAddress;

        rm = m.indexId();
        if (rm > 30)
          goto InvalidAddress;

        // Bit 23 - PostIndex.
        opcode |= B(23);
      }
      else {
        if (m.hasOffset()) {
          if (m.offset() != int32_t(offsetPossibility) || !m.isPostIndex())
            goto InvalidAddress;
          rm = 31;

          // Bit 23 - PostIndex.
          opcode |= B(23);
        }
      }

      opcode.addImm(q, 30);
      opcode.addImm(rm, 16);
      opcode.addImm(opcSsize, 10);
      opcode.addImm(rn, 5);
      goto EmitOp_Rd0;
    }

    default:
      break;
  }

  goto InvalidInstruction;

  // --------------------------------------------------------------------------
  // [EmitGp - Single]
  // --------------------------------------------------------------------------

EmitOp_Rd0:
  if (!checkValidRegs(o0))
    goto InvalidPhysId;

  opcode.addReg(o0, 0);
  goto EmitOp;

EmitOp_Rn5:
  if (!checkValidRegs(o0))
    goto InvalidPhysId;

  opcode.addReg(o0, 5);
  goto EmitOp;

EmitOp_Rn5_Rm16:
  if (!checkValidRegs(o0, o1))
    goto InvalidPhysId;

  opcode.addReg(o0, 5);
  opcode.addReg(o1, 16);
  goto EmitOp;

EmitOp_Rd0_Rn5:
  if (!checkValidRegs(o0, o1))
    goto InvalidPhysId;

  opcode.addReg(o0, 0);
  opcode.addReg(o1, 5);
  goto EmitOp;

EmitOp_Rd0_Rn5_Rm16_Ra10:
  if (!checkValidRegs(o0, o1, o2, o3))
    goto InvalidPhysId;

  opcode.addReg(o0, 0);
  opcode.addReg(o1, 5);
  opcode.addReg(o2, 16);
  opcode.addReg(o3, 10);
  goto EmitOp;

EmitOp_Rd0_Rn5_Rm16:
  if (!checkValidRegs(o0, o1, o3))
    goto InvalidPhysId;

  opcode.addReg(o0, 0);
  opcode.addReg(o1, 5);
  opcode.addReg(o2, 16);
  goto EmitOp;

  // --------------------------------------------------------------------------
  // [EmitGp - Multiple]
  // --------------------------------------------------------------------------

EmitOp_Multiple:
  {
    ASMJIT_ASSERT(multipleOpCount > 0);
    err = writer.ensureSpace(this, multipleOpCount * 4u);
    if (ASMJIT_UNLIKELY(err))
      goto Failed;

    for (uint32_t i = 0; i < multipleOpCount; i++)
      writer.emit32uLE(multipleOpData[i]);

    goto EmitDone;
  }

  // --------------------------------------------------------------------------
  // [EmitGp - Memory]
  // --------------------------------------------------------------------------

EmitOp_MemBase_Rn5:
  if (!checkMemBase(rmRel->as<Mem>()))
    goto InvalidAddress;

  opcode.addReg(rmRel->as<Mem>().baseId(), 5);
  goto EmitOp;

EmitOp_MemBaseNoImm_Rn5:
  if (!checkMemBase(rmRel->as<Mem>()) || rmRel->as<Mem>().hasIndex())
    goto InvalidAddress;

  if (rmRel->as<Mem>().hasOffset())
    goto InvalidDisplacement;

  opcode.addReg(rmRel->as<Mem>().baseId(), 5);
  goto EmitOp;

EmitOp_MemBaseIndex_Rn5_Rm16:
  if (!rmRel->as<Mem>().hasBaseReg())
    goto InvalidAddress;

  if (rmRel->as<Mem>().indexId() > 30 && rmRel->as<Mem>().indexId() != Gp::kIdZr)
    goto InvalidPhysId;

  opcode.addReg(rmRel->as<Mem>().indexId(), 16);
  opcode.addReg(rmRel->as<Mem>().baseId(), 5);
  goto EmitOp;

  // --------------------------------------------------------------------------
  // [EmitOp - PC Relative]
  // --------------------------------------------------------------------------

EmitOp_Rel:
  {
    if (rmRel->isLabel() || rmRel->isMem()) {
      uint32_t labelId;
      int64_t labelOffset = 0;

      if (rmRel->isLabel()) {
        labelId = rmRel->as<Label>().id();
      }
      else {
        labelId = rmRel->as<Mem>().baseId();
        labelOffset = rmRel->as<Mem>().offset();
      }

      LabelEntry* label = _code->labelEntry(labelId);
      if (ASMJIT_UNLIKELY(!label))
        goto InvalidLabel;

      if (offsetFormat.type() == OffsetType::kAArch64_ADRP) {
        // TODO: [ARM] Always create relocation entry.
      }

      if (label->isBoundTo(_section)) {
        // Label bound to the current section.
        offsetValue = label->offset() - uint64_t(offset()) + uint64_t(labelOffset);
        goto EmitOp_DispImm;
      }
      else {
        // Record non-bound label.
        size_t codeOffset = writer.offsetFrom(_bufferData);
        LabelLink* link = _code->newLabelLink(label, _section->id(), codeOffset, intptr_t(labelOffset), offsetFormat);

        if (ASMJIT_UNLIKELY(!link))
          goto OutOfMemory;

        goto EmitOp;
      }
    }
  }

  if (rmRel->isImm()) {
    uint64_t baseAddress = _code->baseAddress();
    uint64_t targetOffset = rmRel->as<Imm>().valueAs<uint64_t>();

    size_t codeOffset = writer.offsetFrom(_bufferData);

    if (baseAddress == Globals::kNoBaseAddress || _section->id() != 0) {
      // Create a new RelocEntry as we cannot calculate the offset right now.
      RelocEntry* re;
      err = _code->newRelocEntry(&re, RelocType::kAbsToRel);
      if (err)
        goto Failed;

      re->_sourceSectionId = _section->id();
      re->_sourceOffset = codeOffset;
      re->_format = offsetFormat;
      re->_payload = rmRel->as<Imm>().valueAs<uint64_t>() + 4u;
      goto EmitOp;
    }
    else {
      uint64_t pc = baseAddress + codeOffset;

      if (offsetFormat.type() == OffsetType::kAArch64_ADRP)
        pc &= ~uint64_t(4096 - 1);

      offsetValue = targetOffset - pc;
      goto EmitOp_DispImm;
    }
  }

  goto InvalidInstruction;

EmitOp_DispImm:
  {
    if ((offsetValue & Support::lsbMask<uint32_t>(offsetFormat.immDiscardLsb())) != 0)
      goto InvalidDisplacement;

    int64_t dispImm64 = int64_t(offsetValue) >> offsetFormat.immDiscardLsb();
    if (!Support::isEncodableOffset64(dispImm64, offsetFormat.immBitCount()))
      goto InvalidDisplacement;

    uint32_t dispImm32 = uint32_t(dispImm64 & Support::lsbMask<uint32_t>(offsetFormat.immBitCount()));
    switch (offsetFormat.type()) {
      case OffsetType::kSignedOffset: {
        opcode.addImm(dispImm32, offsetFormat.immBitShift());
        goto EmitOp;
      }

      case OffsetType::kAArch64_ADR:
      case OffsetType::kAArch64_ADRP: {
        uint32_t immLo = dispImm32 & 0x3u;
        uint32_t immHi = dispImm32 >> 2;
        opcode.addImm(immLo, 29);
        opcode.addImm(immHi, 5);
        goto EmitOp;
      }

      default:
        goto InvalidDisplacement;
    }
  }

  // --------------------------------------------------------------------------
  // [EmitOp - Opcode]
  // --------------------------------------------------------------------------

EmitOp:
  writer.emit32uLE(opcode.get());
  goto EmitDone;

  // --------------------------------------------------------------------------
  // [Done]
  // --------------------------------------------------------------------------

EmitDone:
  if (Support::test(options, InstOptions::kReserved)) {
#ifndef ASMJIT_NO_LOGGING
    if (_logger)
      EmitterUtils::logInstructionEmitted(this, BaseInst::composeARMInstId(instId, instCC), options, o0, o1, o2, opExt, 0, 0, writer.cursor());
#endif
  }

  resetState();

  writer.done(this);
  return kErrorOk;

  // --------------------------------------------------------------------------
  // [Error Handler]
  // --------------------------------------------------------------------------

#define ERROR_HANDLER(ERR) ERR: err = DebugUtils::errored(kError##ERR); goto Failed;
  ERROR_HANDLER(OutOfMemory)
  ERROR_HANDLER(InvalidAddress)
  ERROR_HANDLER(InvalidAddressScale)
  ERROR_HANDLER(InvalidDisplacement)
  ERROR_HANDLER(InvalidElementIndex)
  ERROR_HANDLER(InvalidLabel)
  ERROR_HANDLER(InvalidImmediate)
  ERROR_HANDLER(InvalidInstruction)
  ERROR_HANDLER(InvalidPhysId)
  ERROR_HANDLER(InvalidRegType)
#undef ERROR_HANDLER

Failed:
#ifndef ASMJIT_NO_LOGGING
  return EmitterUtils::logInstructionFailed(this, err, instId, options, o0, o1, o2, opExt);
#else
  resetState();
  return reportError(err);
#endif
}

#undef ENC_OPS1
#undef ENC_OPS2
#undef ENC_OPS3
#undef ENC_OPS4

// a64::Assembler - Align
// ======================

Error Assembler::align(AlignMode alignMode, uint32_t alignment) {
  constexpr uint32_t kNopA64 = 0xD503201Fu; // [11010101|00000011|00100000|00011111].

  if (ASMJIT_UNLIKELY(!_code))
    return reportError(DebugUtils::errored(kErrorNotInitialized));

  if (ASMJIT_UNLIKELY(uint32_t(alignMode) > uint32_t(AlignMode::kMaxValue)))
    return reportError(DebugUtils::errored(kErrorInvalidArgument));

  if (alignment <= 1)
    return kErrorOk;

  if (ASMJIT_UNLIKELY(alignment > Globals::kMaxAlignment || !Support::isPowerOf2(alignment)))
    return reportError(DebugUtils::errored(kErrorInvalidArgument));

  uint32_t i = uint32_t(Support::alignUpDiff<size_t>(offset(), alignment));
  if (i == 0)
    return kErrorOk;

  CodeWriter writer(this);
  ASMJIT_PROPAGATE(writer.ensureSpace(this, i));

  switch (alignMode) {
    case AlignMode::kCode: {
      uint32_t pattern = kNopA64;

      if (ASMJIT_UNLIKELY(offset() & 0x3u))
        return DebugUtils::errored(kErrorInvalidState);

      while (i >= 4) {
        writer.emit32uLE(pattern);
        i -= 4;
      }

      ASMJIT_ASSERT(i == 0);
      break;
    }

    case AlignMode::kData:
    case AlignMode::kZero:
      writer.emitZeros(i);
      break;
  }

  writer.done(this);

#ifndef ASMJIT_NO_LOGGING
  if (_logger) {
    StringTmp<128> sb;
    sb.appendChars(' ', _logger->indentation(FormatIndentationGroup::kCode));
    sb.appendFormat("align %u\n", alignment);
    _logger->log(sb);
  }
#endif

  return kErrorOk;
}

// a64::Assembler - Events
// =======================

Error Assembler::onAttach(CodeHolder* code) noexcept {
  ASMJIT_PROPAGATE(Base::onAttach(code));
  return kErrorOk;
}

Error Assembler::onDetach(CodeHolder* code) noexcept {
  return Base::onDetach(code);
}

ASMJIT_END_SUB_NAMESPACE

#endif // !ASMJIT_NO_AARCH64
