// This file is part of AsmJit project <https://asmjit.com>
//
// See asmjit.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#include "../core/api-build_p.h"
#if !defined(ASMJIT_NO_AARCH32)

#include "../core/codewriter_p.h"
#include "../core/cpuinfo.h"
#include "../core/emitterutils_p.h"
#include "../core/formatter.h"
#include "../core/logger.h"
#include "../core/misc_p.h"
#include "../core/support.h"
#include "../arm/armutils.h"
#include "../arm/a32formatter_p.h"
#include "../arm/a32assembler.h"
#include "../arm/a32emithelper_p.h"
#include "../arm/a32instdb_p.h"

ASMJIT_BEGIN_SUB_NAMESPACE(a32)

struct InstDispatchRecord {
  uint8_t group;
  uint8_t index;
};

// ${a32::DispatchData:Begin}
// ------------------- Automatically generated, do not edit -------------------
static const InstDispatchRecord instDispatchTable[] = {
  {0, 0}, {1, 0}, {1, 1}, {1, 2}, {1, 3}, {2, 0}, {3, 0}, {3, 1}, {3, 2}, {3, 3}, {1, 4}, {1, 5}, {4, 0}, {4, 1}, {5, 0},
  {6, 0}, {7, 0}, {1, 6}, {1, 7}, {8, 0}, {5, 1}, {9, 0}, {10, 0}, {10, 1}, {0, 1}, {0, 2}, {11, 0}, {12, 0}, {13, 0},
  {14, 0}, {14, 1}, {15, 0}, {16, 0}, {16, 1}, {17, 0}, {17, 1}, {17, 2}, {17, 3}, {17, 4}, {17, 5}, {11, 1}, {18, 0},
  {0, 3}, {0, 4}, {0, 5}, {19, 0}, {19, 1}, {1, 8}, {1, 9}, {11, 2}, {11, 3}, {20, 0}, {8, 1}, {19, 2}, {0, 6}, {0, 7},
  {0, 8}, {0, 9}, {0, 10}, {0, 11}, {0, 12}, {0, 13}, {0, 14}, {0, 15}, {0, 16}, {0, 17}, {0, 18}, {0, 19}, {0, 20},
  {21, 0}, {21, 1}, {21, 2}, {21, 3}, {22, 0}, {21, 4}, {21, 5}, {23, 0}, {23, 1}, {23, 2}, {23, 3}, {24, 0}, {24, 1},
  {25, 0}, {26, 0}, {21, 6}, {21, 7}, {22, 1}, {21, 8}, {27, 0}, {28, 0}, {27, 1}, {29, 0}, {27, 2}, {30, 0}, {25, 1},
  {31, 0}, {31, 1}, {31, 2}, {31, 3}, {32, 0}, {33, 0}, {34, 0}, {35, 0}, {36, 0}, {36, 1}, {36, 2}, {37, 0}, {37, 1},
  {38, 0}, {38, 1}, {32, 1}, {33, 1}, {34, 1}, {35, 1}, {39, 0}, {40, 0}, {41, 0}, {41, 1}, {37, 2}, {37, 3}, {11, 4},
  {0, 21}, {0, 22}, {1, 10}, {1, 11}, {42, 0}, {43, 0}, {44, 0}, {45, 0}, {46, 0}, {47, 0}, {12, 1}, {47, 1}, {48, 0},
  {17, 6}, {17, 7}, {17, 8}, {48, 1}, {48, 2}, {17, 9}, {48, 3}, {17, 10}, {17, 11}, {13, 1}, {13, 2}, {13, 3}, {13, 4},
  {49, 0}, {49, 1}, {49, 2}, {49, 3}, {31, 4}, {31, 5}, {13, 5}, {13, 6}, {1, 12}, {1, 13}, {1, 14}, {1, 15}, {17, 12},
  {17, 13}, {17, 14}, {12, 2}, {1, 16}, {1, 17}, {50, 0}, {41, 2}, {17, 15}, {51, 0}, {51, 1}, {11, 5}, {11, 6}, {52, 0},
  {53, 0}, {52, 1}, {52, 2}, {52, 3}, {53, 1}, {52, 4}, {52, 5}, {53, 2}, {52, 6}, {17, 16}, {17, 17}, {17, 18}, {17, 19},
  {17, 20}, {17, 21}, {18, 1}, {36, 3}, {36, 4}, {36, 5}, {36, 6}, {54, 0}, {54, 1}, {54, 2}, {54, 3}, {54, 4}, {54, 5},
  {54, 6}, {54, 7}, {36, 7}, {36, 8}, {36, 9}, {36, 10}, {36, 11}, {36, 12}, {54, 8}, {54, 9}, {36, 13}, {36, 14},
  {36, 15}, {36, 16}, {41, 3}, {41, 4}, {41, 5}, {41, 6}, {41, 7}, {41, 8}, {54, 10}, {54, 11}, {41, 9}, {41, 10},
  {41, 11}, {41, 12}, {41, 13}, {41, 14}, {55, 0}, {55, 1}, {55, 2}, {55, 3}, {56, 0}, {57, 0}, {17, 22}, {12, 3},
  {17, 23}, {17, 24}, {58, 0}, {58, 1}, {59, 0}, {59, 1}, {60, 0}, {59, 2}, {58, 2}, {23, 4}, {23, 5}, {23, 6}, {23, 7},
  {24, 2}, {24, 3}, {61, 0}, {62, 0}, {59, 3}, {59, 4}, {60, 1}, {59, 5}, {27, 3}, {28, 1}, {25, 2}, {1, 18}, {1, 19},
  {63, 0}, {64, 0}, {64, 1}, {64, 2}, {65, 0}, {65, 1}, {65, 2}, {0, 23}, {0, 24}, {14, 2}, {14, 3}, {17, 25}, {17, 26},
  {17, 27}, {50, 1}, {20, 1}, {41, 15}, {17, 28}, {17, 29}, {17, 30}, {17, 31}, {17, 32}, {17, 33}, {54, 12}, {54, 13},
  {54, 14}, {54, 15}, {54, 16}, {17, 34}, {17, 35}, {17, 36}, {17, 37}, {17, 38}, {17, 39}, {41, 16}, {36, 17}, {66, 0},
  {67, 0}, {17, 40}, {17, 41}, {17, 42}, {64, 3}, {64, 4}, {64, 5}, {65, 3}, {65, 4}, {65, 5}, {68, 0}, {69, 0}, {70, 0},
  {69, 1}, {71, 0}, {72, 0}, {72, 1}, {73, 0}, {73, 1}, {74, 0}, {75, 0}, {69, 2}, {76, 0}, {77, 0}, {78, 0}, {79, 0},
  {79, 1}, {79, 2}, {80, 0}, {81, 0}, {82, 0}, {82, 1}, {83, 0}, {84, 0}, {83, 1}, {84, 1}, {85, 0}, {86, 0}, {86, 1},
  {87, 0}, {88, 0}, {89, 0}, {90, 0}, {91, 0}, {91, 1}, {91, 2}, {92, 0}, {90, 1}, {93, 0}, {94, 0}, {95, 0}, {79, 3},
  {96, 0}, {97, 0}, {98, 0}, {99, 0}, {98, 1}, {97, 1}, {99, 1}, {100, 0}, {100, 1}, {68, 1}, {68, 2}, {101, 0}, {102, 0},
  {103, 0}, {104, 0}, {105, 0}, {106, 0}, {107, 0}, {108, 0}, {109, 0}, {110, 0}, {111, 0}, {112, 0}, {113, 0}, {70, 1},
  {114, 0}, {70, 2}, {114, 1}, {115, 0}, {116, 0}, {115, 1}, {116, 1}, {117, 0}, {118, 0}, {119, 0}, {120, 0}, {101, 1},
  {121, 0}, {122, 0}, {123, 0}, {71, 1}, {93, 1}, {93, 2}, {93, 3}, {124, 0}, {125, 0}, {126, 0}, {127, 0}, {126, 1},
  {128, 0}, {128, 1}, {129, 0}, {129, 1}, {130, 0}, {131, 0}, {132, 0}, {132, 1}, {133, 0}, {132, 2}, {134, 0}, {135, 0},
  {130, 1}, {136, 0}, {136, 1}, {133, 1}, {137, 0}, {138, 0}, {139, 0}, {140, 0}, {141, 0}, {138, 1}, {139, 1}, {131, 1},
  {75, 1}, {142, 0}, {72, 2}, {87, 1}, {143, 0}, {84, 2}, {68, 3}, {144, 0}, {144, 1}, {144, 2}, {144, 3}, {145, 0},
  {146, 0}, {146, 1}, {137, 1}, {147, 0}, {148, 0}, {142, 1}, {72, 3}, {149, 0}, {75, 2}, {150, 0}, {151, 0}, {151, 1},
  {151, 2}, {151, 3}, {152, 0}, {153, 0}, {154, 0}, {148, 1}, {155, 0}, {156, 0}, {157, 0}, {149, 1}, {158, 0}, {159, 0},
  {160, 0}, {161, 0}, {162, 0}, {111, 1}, {112, 1}, {113, 1}, {74, 1}, {75, 3}, {69, 3}, {76, 1}, {163, 0}, {164, 0},
  {165, 0}, {165, 1}, {84, 3}, {166, 0}, {167, 0}, {168, 0}, {150, 1}, {156, 1}, {169, 0}, {169, 1}, {11, 7}, {11, 8},
  {11, 9}
};
// ----------------------------------------------------------------------------
// ${a32::DispatchData:End}

// a32::Assembler - Signature Checker
// ==================================

// The assembler checks 8 least significant bits of each operand signature. This allows
// to check multiple signatures at a time, and to know the type of the operand and a possibly

// Low 8-bits of an operand signatures stored in `sgn`.
static constexpr uint32_t kOpRegR = uint32_t(OperandType::kReg) | (uint32_t(RegType::kGp32) << 3);
static constexpr uint32_t kOpRegS = uint32_t(OperandType::kReg) | (uint32_t(RegType::kARM_VecS) << 3);
static constexpr uint32_t kOpRegD = uint32_t(OperandType::kReg) | (uint32_t(RegType::kARM_VecD) << 3);
static constexpr uint32_t kOpRegQ = uint32_t(OperandType::kReg) | (uint32_t(RegType::kARM_VecV) << 3);

static constexpr uint32_t kOpRegListR = uint32_t(OperandType::kRegList) | (uint32_t(RegType::kGp32) << 3);
static constexpr uint32_t kOpRegListS = uint32_t(OperandType::kRegList) | (uint32_t(RegType::kARM_VecS) << 3);
static constexpr uint32_t kOpRegListD = uint32_t(OperandType::kRegList) | (uint32_t(RegType::kARM_VecD) << 3);

static constexpr uint32_t kOpMemB = uint32_t(OperandType::kMem) | (uint32_t(RegType::kGp32) << 3);
static constexpr uint32_t kOpMemAny = uint32_t(OperandType::kMem);
static constexpr uint32_t kOpImmI = uint32_t(OperandType::kImm) | (uint32_t(ImmType::kInt) << 3);
static constexpr uint32_t kOpImmF = uint32_t(OperandType::kImm) | (uint32_t(ImmType::kDouble) << 3);
static constexpr uint32_t kOpLabel = uint32_t(OperandType::kLabel);
static constexpr uint32_t kOpRegC = 0;

// To easier calculate PC + 8
static constexpr uint32_t kPCOffset = 8;

namespace {

// A mask that must be applied to 8-bit signatures.
//
// This is just to allow matching signatures where we don't care of some bits - currently only used to match kOpMemAny.
template<uint32_t kValue>
ASMJIT_FORCE_INLINE constexpr uint32_t opSignatureMask() noexcept {
  return kValue == kOpMemAny ? 0x07u : 0xFFu;
}

template<uint32_t o0 = 0u, uint32_t o1 = 0u, uint32_t o2 = 0u, uint32_t o3 = 0u>
ASMJIT_FORCE_INLINE constexpr uint32_t opSignatureMask0To3() noexcept {
  return (opSignatureMask<o0>() <<  0) |
         (opSignatureMask<o1>() <<  8) |
         (opSignatureMask<o2>() << 16) |
         (opSignatureMask<o3>() << 24) ;
}

template<uint32_t o0 = 0u, uint32_t o1 = 0u, uint32_t o2 = 0u, uint32_t o3 = 0u, uint32_t o4 = 0u, uint32_t o5 = 0u>
ASMJIT_FORCE_INLINE constexpr uint64_t opSignatureMask0To5() noexcept {
  return (uint64_t(opSignatureMask<o0>()) <<  0) |
         (uint64_t(opSignatureMask<o1>()) <<  8) |
         (uint64_t(opSignatureMask<o2>()) << 16) |
         (uint64_t(opSignatureMask<o3>()) << 24) |
         (uint64_t(opSignatureMask<o4>()) << 32) |
         (uint64_t(opSignatureMask<o5>()) << 40) | (uint64_t(0xFFFFu) << 48);
}

template<uint32_t o0 = 0u, uint32_t o1 = 0u, uint32_t o2 = 0u, uint32_t o3 = 0u>
ASMJIT_FORCE_INLINE constexpr uint64_t opSignatureCombine0To3() noexcept {
  return (o0 <<  0) |
         (o1 <<  8) |
         (o2 << 16) |
         (o3 << 24) ;
}

template<uint32_t o0 = 0u, uint32_t o1 = 0u, uint32_t o2 = 0u, uint32_t o3 = 0u, uint32_t o4 = 0u, uint32_t o5 = 0u>
ASMJIT_FORCE_INLINE constexpr uint64_t opSignatureCombine0To5() noexcept {
  return (uint64_t(o0) <<  0) |
         (uint64_t(o1) <<  8) |
         (uint64_t(o2) << 16) |
         (uint64_t(o3) << 24) |
         (uint64_t(o4) << 32) |
         (uint64_t(o5) << 40) ;
}

#if ASMJIT_ARCH_BITS >= 64
struct SignatureChecker {
  uint64_t _0to5;

  ASMJIT_FORCE_INLINE void init(const Operand_& o0, const Operand_& o1, const Operand_& o2, const Operand_& o3, const Operand_& o4, const Operand_& o5) noexcept {
    _0to5 = (uint64_t(o0._signature._bits & 0xFFu)      ) |
            (uint64_t(o1._signature._bits & 0xFFu) <<  8) |
            (uint64_t(o2._signature._bits & 0xFFu) << 16) |
            (uint64_t(o3._signature._bits & 0xFFu) << 24) |
            (uint64_t(o4._signature._bits & 0xFFu) << 32) |
            (uint64_t(o5._signature._bits & 0xFFu) << 40) ;
  }

  ASMJIT_FORCE_INLINE bool empty() const noexcept {
    return _0to5 == 0u;
  }

  template<uint32_t o0>
  ASMJIT_FORCE_INLINE bool test() const noexcept {
    constexpr uint64_t kMask = opSignatureMask0To5<o0>();
    constexpr uint64_t kPred = opSignatureCombine0To5<o0>();

    return (_0to5 & kMask) == kPred;
  }

  template<uint32_t o0, uint32_t o1>
  ASMJIT_FORCE_INLINE bool test() const noexcept {
    constexpr uint64_t kMask = opSignatureMask0To5<o0, o1>();
    constexpr uint64_t kPred = opSignatureCombine0To5<o0, o1>();

    return (_0to5 & kMask) == kPred;
  }

  template<uint32_t o0, uint32_t o1, uint32_t o2>
  ASMJIT_FORCE_INLINE bool test() const noexcept {
    constexpr uint64_t kMask = opSignatureMask0To5<o0, o1, o2>();
    constexpr uint64_t kPred = opSignatureCombine0To5<o0, o1, o2>();

    return (_0to5 & kMask) == kPred;
  }

  template<uint32_t o0, uint32_t o1, uint32_t o2, uint32_t o3>
  ASMJIT_FORCE_INLINE bool test() const noexcept {
    constexpr uint64_t kMask = opSignatureMask0To5<o0, o1, o2, o3>();
    constexpr uint64_t kPred = opSignatureCombine0To5<o0, o1, o2, o3>();

    return (_0to5 & kMask) == kPred;
  }

  template<uint32_t o0, uint32_t o1, uint32_t o2, uint32_t o3, uint32_t o4>
  ASMJIT_FORCE_INLINE bool test() const noexcept {
    constexpr uint64_t kMask = opSignatureMask0To5<o0, o1, o2, o3, o4>();
    constexpr uint64_t kPred = opSignatureCombine0To5<o0, o1, o2, o3, o4>();

    return (_0to5 & kMask) == kPred;
  }

  template<uint32_t o0, uint32_t o1, uint32_t o2, uint32_t o3, uint32_t o4, uint32_t o5>
  ASMJIT_FORCE_INLINE bool test() const noexcept {
    constexpr uint64_t kMask = opSignatureMask0To5<o0, o1, o2, o3, o4, o5>();
    constexpr uint64_t kPred = opSignatureCombine0To5<o0, o1, o2, o3, o4, o5>();

    return (_0to5 & kMask) == kPred;
  }
};
#else
struct SignatureChecker {
  uint32_t _0to3;
  uint32_t _4to5;

  ASMJIT_FORCE_INLINE void init(const Operand_& o0, const Operand_& o1, const Operand_& o2, const Operand_& o3, const Operand_& o4, const Operand_& o5) noexcept {
    _0to3 = ((o0._signature._bits & 0xFFu)      ) |
            ((o1._signature._bits & 0xFFu) <<  8) |
            ((o2._signature._bits & 0xFFu) << 16) |
            ((o3._signature._bits & 0xFFu) << 24) ;
    _4to5 = ((o4._signature._bits & 0xFFu)      ) |
            ((o5._signature._bits & 0xFFu) <<  8) ;
  }

  ASMJIT_FORCE_INLINE bool empty() const noexcept {
    return _0to3 == 0u;
  }

  template<uint32_t o0>
  ASMJIT_FORCE_INLINE bool test() const noexcept {
    constexpr uint32_t kMask = opSignatureMask0To3<o0>();
    constexpr uint32_t kPred = opSignatureCombine0To3<o0>();

    return (_0to3 & kMask) == kPred;
  }

  template<uint32_t o0, uint32_t o1>
  ASMJIT_FORCE_INLINE bool test() const noexcept {
    constexpr uint32_t kMask = opSignatureMask0To3<o0, o1>();
    constexpr uint32_t kPred = opSignatureCombine0To3<o0, o1>();

    return (_0to3 & kMask) == kPred;
  }

  template<uint32_t o0, uint32_t o1, uint32_t o2>
  ASMJIT_FORCE_INLINE bool test() const noexcept {
    constexpr uint32_t kMask = opSignatureMask0To3<o0, o1, o2>();
    constexpr uint32_t kPred = opSignatureCombine0To3<o0, o1, o2>();

    return (_0to3 & kMask) == kPred;
  }

  template<uint32_t o0, uint32_t o1, uint32_t o2, uint32_t o3>
  ASMJIT_FORCE_INLINE bool test() const noexcept {
    constexpr uint32_t kMask = opSignatureMask0To3<o0, o1, o2, o3>();
    constexpr uint32_t kPred = opSignatureCombine0To3<o0, o1, o2, o3>();

    return bool(unsigned((_0to3 & kMask) == kPred) &
                unsigned(_4to5 == 0));
  }

  template<uint32_t o0, uint32_t o1, uint32_t o2, uint32_t o3, uint32_t o4>
  ASMJIT_FORCE_INLINE bool test() const noexcept {
    constexpr uint32_t kMask0To3 = opSignatureMask0To3<o0, o1, o2, o3>();
    constexpr uint32_t kPred0To3 = opSignatureCombine0To3<o0, o1, o2, o3>();
    constexpr uint32_t kMask4To5 = opSignatureMask0To3<o4>();
    constexpr uint32_t kPred4To5 = opSignatureCombine0To3<o4>();

    return bool(unsigned((_0to3 & kMask0To3) == kPred0To3) &
                unsigned((_4to5 & kMask4To5) == kPred4To5));
  }

  template<uint32_t o0, uint32_t o1, uint32_t o2, uint32_t o3, uint32_t o4, uint32_t o5>
  ASMJIT_FORCE_INLINE bool test() const noexcept {
    constexpr uint32_t kMask0To3 = opSignatureMask0To3<o0, o1, o2, o3>();
    constexpr uint32_t kPred0To3 = opSignatureCombine0To3<o0, o1, o2, o3>();
    constexpr uint32_t kMask4To5 = opSignatureMask0To3<o4, o5>();
    constexpr uint32_t kPred4To5 = opSignatureCombine0To3<o4, o5>();

    return bool(unsigned((_0to3 & kMask0To3) == kPred0To3) &
                unsigned((_4to5 & kMask4To5) == kPred4To5));
  }
};
#endif

} // {anonymous}

// a32::Assembler - DT Checks
// ==========================

namespace {

static ASMJIT_FORCE_INLINE uint32_t makeDtBits(DataType dt) noexcept { return (1u << uint32_t(dt)); }

template<typename... Dts>
static ASMJIT_FORCE_INLINE uint32_t makeDtBits(DataType first, Dts&&... dts) noexcept { return makeDtBits(first) | makeDtBits(std::forward<Dts>(dts)...); }

static ASMJIT_FORCE_INLINE bool isDtSingle(uint32_t dtBits, DataType dt) noexcept {
  return dtBits == uint32_t(dt);
}

static ASMJIT_FORCE_INLINE bool isDtMultiple(uint32_t dtBits, uint32_t dtAllowedMask) noexcept {
  return dtBits < 16 && ((1u << dtBits) & dtAllowedMask) != 0;
}

static ASMJIT_FORCE_INLINE bool isDtAndDt2Single(uint32_t dtBits, DataType dt, DataType dt2) noexcept {
  uint32_t dtPredicate = uint32_t(dt) | (uint32_t(dt2) << 4);
  return dtBits == dtPredicate;
}

} // {anonymous}

// a32::Assembler - Operand Checks
// ===============================

namespace {

static ASMJIT_FORCE_INLINE OperandSignature combinedSignature(const Operand_& op) noexcept {
  return op._signature;
}

template<typename... Args>
static ASMJIT_FORCE_INLINE OperandSignature combinedSignature(const Operand_& op, Args&&... args) noexcept {
  return op._signature | combinedSignature(std::forward<Args>(args)...);
}

static ASMJIT_FORCE_INLINE bool isPureVecSignature(OperandSignature signature) noexcept {
  return (signature._bits & (Vec::kSignatureRegElementTypeMask | Vec::kSignatureRegElementFlagMask)) == 0;
};

static ASMJIT_FORCE_INLINE bool isElementVecSignature(OperandSignature signature) noexcept {
  return (signature._bits & (Vec::kSignatureRegElementTypeMask | Vec::kSignatureRegElementFlagMask)) == Vec::kSignatureRegElementFlagMask;
};

template<typename... Args>
static ASMJIT_FORCE_INLINE bool isPureVec(Args&&... args) noexcept {
  return isPureVecSignature(combinedSignature(std::forward<Args>(args)...));
}

template<typename... Args>
static ASMJIT_FORCE_INLINE bool isElementVec(Args&&... args) noexcept {
  return isElementVecSignature(combinedSignature(std::forward<Args>(args)...));
}

static ASMJIT_FORCE_INLINE bool isConsecutive(uint32_t inc, const Reg& o0, const Reg& o1) noexcept {
  return o0.id() + inc == o1.id();
}
static ASMJIT_FORCE_INLINE bool isConsecutive(uint32_t inc, const Reg& o0, const Reg& o1, const Reg& o2) noexcept {
  return bool(unsigned(o0.id() + inc == o1.id()) & unsigned(o1.id() + inc == o2.id()));
}

static ASMJIT_FORCE_INLINE bool isConsecutive(uint32_t inc, const Reg& o0, const Reg& o1, const Reg& o2, const Reg& o3) noexcept {
  return bool(unsigned(o0.id() + inc == o1.id()) & unsigned(o1.id() + inc == o2.id()) & unsigned(o2.id() + inc == o3.id()));
}

static ASMJIT_FORCE_INLINE bool checkUOffset(int32_t offset, uint32_t nBits, uint32_t lsbCut) noexcept {
  uint32_t uoff = uint32_t(offset);
  uint32_t mask = Support::lsbMask<uint32_t>(nBits) << lsbCut;
  return (uoff & ~mask) == 0;
}

static ASMJIT_FORCE_INLINE bool checkSOffset(int32_t offset, uint32_t nBits, uint32_t lsbCut) noexcept {
  uint32_t uoff = offset >= 0 ? uint32_t(offset) : Support::neg(uint32_t(offset));
  uint32_t mask = Support::lsbMask<uint32_t>(nBits) << lsbCut;
  return (uoff & ~mask) == 0;
}

static ASMJIT_FORCE_INLINE bool isMemPCRel(const Mem& mem) noexcept {
  static constexpr uint32_t kAllowedTypeMask = (1u << uint32_t(RegType::kNone    )) |
                                               (1u << uint32_t(RegType::kLabelTag)) |
                                               (1u << uint32_t(RegType::kGp32    )) ;

  uint32_t typeBit = 1u << uint32_t(mem.baseType());
  uint32_t baseId = mem.baseId();

  return (kAllowedTypeMask & typeBit) && (baseId == 15u || mem.baseType() != RegType::kGp32) && !mem.hasIndex() && mem.isFixedOffset();
}

} // {anonymous}

// a32::Assembler - Immediate Encoders - GP
// ========================================

namespace {

static ASMJIT_FORCE_INLINE uint32_t encodeCond(uint32_t cc) noexcept { return (cc - 2u) & 0xFu; }
static ASMJIT_FORCE_INLINE uint32_t szFromDt(uint32_t dtBits) noexcept { return (dtBits - 1) & 0x3; }
static ASMJIT_FORCE_INLINE uint32_t uBitFromDt(uint32_t dtBits) noexcept { return uint32_t(dtBits >= uint32_t(DataType::kU8) && dtBits <= uint32_t(DataType::kU64)); }
static ASMJIT_FORCE_INLINE uint32_t pBitFromDt(uint32_t dtBits) noexcept { return uint32_t(dtBits >= uint32_t(DataType::kP8) && dtBits <= uint32_t(DataType::kP64)); }

struct ImmEncode {
  uint32_t _imm;
  ASMJIT_FORCE_INLINE uint32_t imm() const noexcept { return _imm; }
};

struct ImmAEncode : public ImmEncode {
  ASMJIT_FORCE_INLINE bool init(const Imm& immA) noexcept {
    return Utils::encodeAArch32Imm(immA.valueAs<uint64_t>(), &_imm);
  }
};

struct SsatImmEncode : public ImmEncode {
  uint32_t _n;
  ASMJIT_FORCE_INLINE uint32_t n() const noexcept { return _n; }

  ASMJIT_FORCE_INLINE bool init(const Imm& sat) noexcept {
    uint64_t sat_64 = sat.valueAs<uint64_t>() - 1u;

    if (sat_64 >= 32u)
      return false;

    _imm = uint32_t(sat_64);
    _n = 0u;
    return true;
  }

  ASMJIT_FORCE_INLINE bool init(const Imm& sat, const Imm& n) noexcept {
    uint64_t sat_64 = sat.valueAs<uint64_t>() - 1u;
    uint64_t n_64 = n.valueAs<uint64_t>();

    if ((sat_64 | n_64) >= 32u)
      return false;

    _imm = uint32_t(sat_64);
    _n = uint32_t(n_64);
    return true;
  }
};

struct Ssat16ImmEncode : public ImmEncode {
  ASMJIT_FORCE_INLINE bool init(const Imm& ror) noexcept {
    uint64_t value = ror.valueAs<uint64_t>() - 1u;

    if (value >= 16u)
      return false;

    _imm = uint32_t(value);
    return true;
  }
};

struct Ror8ImmEncode : public ImmEncode {
  ASMJIT_FORCE_INLINE bool init(const Imm& ror) noexcept {
    uint64_t value = ror.valueAs<uint64_t>();
    uint64_t mask = ~uint64_t(Support::lsbMask<uint32_t>(2) << 3);

    _imm = uint32_t((value >> 3) & 0x3u);
    return (value & mask) == 0;
  }
};

struct BfcBfiImmEncode {
  uint32_t _lsb;
  uint32_t _msb;

  ASMJIT_FORCE_INLINE uint32_t lsb() const noexcept { return _lsb; }
  ASMJIT_FORCE_INLINE uint32_t msb() const noexcept { return _msb; }

  ASMJIT_FORCE_INLINE bool init(const Imm& lsb, const Imm& width) noexcept {
    uint64_t lsb_64 = lsb.valueAs<uint64_t>();
    uint64_t width_64 = width.valueAs<uint64_t>();

    if (lsb_64 >= 32 || width_64 == 0 || width_64 > 32u - lsb_64)
      return false;

    _lsb = uint32_t(lsb_64);
    _msb = _lsb + uint32_t(width_64) - 1u;
    return true;
  }
};

struct SbfxUbfxImmEncode {
  uint32_t _lsb;
  uint32_t _widthM1;

  ASMJIT_FORCE_INLINE uint32_t lsb() const noexcept { return _lsb; }
  ASMJIT_FORCE_INLINE uint32_t widthM1() const noexcept { return _widthM1; }

  ASMJIT_FORCE_INLINE bool init(const Imm& lsb, const Imm& width) noexcept {
    uint64_t lsb_64 = lsb.valueAs<uint64_t>();
    uint64_t widthM1_64 = width.valueAs<uint64_t>() - 1u;

    if ((lsb_64 | widthM1_64) >= 32u)
      return false;

    _lsb = uint32_t(lsb_64);
    _widthM1 = uint32_t(widthM1_64);
    return true;
  }
};

} // {anonymous}

// a32::Assembler - Immediate Encoders - VFP/ASIMD
// ===============================================

namespace {

// Encodes immediate value for VAND, VBIC, VORN, VORR instructions.
//
// These only operate on 16-bit and 32-bit vectors - the immediate is an 8-bit value with a  possible shift.
struct VecBicOrrImmEncode : public ImmEncode {
  uint32_t _cmode;
  ASMJIT_FORCE_INLINE uint32_t cmode() const noexcept { return _cmode; }

  ASMJIT_FORCE_INLINE bool initBicOrr(uint32_t sz, uint32_t cmodeBase, uint32_t imm) noexcept {
    ASMJIT_ASSERT(sz >= 1u);

    uint32_t shift = 0;
    uint32_t maxShift = (8u << sz) - 8u;

    if (imm) {
      shift = Support::ctz(imm) & ~0x7u;
      imm >>= shift;

      if (imm > 0xFFu || shift > maxShift)
        return false;
    }

    _imm = imm;
    _cmode = cmodeBase | (sz == 1u ? 0x8u : 0x0u) | ((shift >> 3u) << 1u);

    return true;
  }

  ASMJIT_FORCE_INLINE bool init(uint32_t sz, uint32_t inv, const Imm& immV) noexcept {
    static const uint64_t maskTable[4] = { 0xFFu, 0xFFFFu, 0xFFFFFFFFu, 0xFFFFFFFFFFFFFFFFu };

    uint64_t imm_64 = immV.valueAs<uint64_t>();
    uint64_t mask_64 = maskTable[sz];

    imm_64 ^= mask_64 & Support::bitMaskFromBool<uint64_t>(inv);
    if (imm_64 & ~mask_64)
      return false;

    uint32_t immLo = uint32_t(imm_64 & 0xFFFFFFFFu);
    uint32_t immHi = uint32_t(imm_64 >> 32);

    // Change the operation to 32-bit if the pattern repeats two 32-bit values.
    if (sz == 3) {
      if (immLo != immHi)
        return false;
      sz = 2;
    }
    else {
      if (immHi)
        return false;
    }

    // Change the operation to 16-bit if the pattern repeats two 16-bit values.
    if (sz == 2 && (immLo >> 16) == (immLo & 0xFFFFu)) {
      sz = 1;
      immLo >>= 16;
    }

    return initBicOrr(sz, 0x1u, immLo);
  }
};

// Encodes immediate value for VMOV instruction.
struct VecMovImmEncode : public VecBicOrrImmEncode {
  uint32_t _op;
  ASMJIT_FORCE_INLINE uint32_t op() const noexcept { return _op; }

  bool init(uint32_t sz, uint32_t inv, const Imm& immV) noexcept {
    static const uint64_t maskTable[4] = { 0xFFu, 0xFFFFu, 0xFFFFFFFFu, 0xFFFFFFFFFFFFFFFFu };

    uint64_t imm_64 = immV.valueAs<uint64_t>();
    uint64_t mask_64 = maskTable[sz];

    imm_64 ^= mask_64 & Support::bitMaskFromBool<uint64_t>(inv);
    if (imm_64 & ~mask_64)
      return false;

    // First try the same cmode|op combinations as used by VBIC/VORR.
    uint32_t immLo = uint32_t(imm_64 & 0xFFFFFFFFu);
    uint32_t immHi = uint32_t(imm_64 >> 32);

    // Change the operation to 32-bit if a 64-bit pattern repeats two 32-bit values.
    if (sz == 3) {
      if (immLo == immHi) {
        immHi = 0;
        sz = 2;
      }
    }
    else {
      if (immHi)
        return false;
    }

    // Change the operation to 16-bit if a 32-bit pattern repeats two 16-bit values.
    if (sz == 2 && (immLo >> 16) == (immLo & 0xFFFFu)) {
      sz = 1;
      immLo >>= 16;
    }

    // VBIC/VORR specific encodings.
    _op = 0;
    if ((sz == 1u || sz == 2u) && initBicOrr(sz, 0x0u, immLo))
      return true;

    // VMOV specific encoding {cmode=110x op=0} - either 0x0000xxFF or 0x00xxFFFF.
    if (sz == 2u) {
      if ((immLo & 0xFFFF00FFu) == 0x000000FF) {
        _imm = immLo >> 8u;
        _cmode = 0xCu;
        return true;
      }

      if ((immLo & 0xFF00FFFFu) == 0x0000FFFF) {
        _imm = immLo >> 16u;
        _cmode = 0xDu;
        return true;
      }
    }

    // Change the operation to 8-bit if a 16-bit pattern repeats two 8-bit values.
    if (sz == 1u && (immLo >> 8u) == (immLo & 0xFFu)) {
      sz = 0;
      immLo >>= 8;
    }

    // VMOV specific encoding {cmode=1110 op=0} - 8-bit pattern replicated to all bytes.
    if (sz == 0u) {
      _imm = immLo;
      _cmode = 0xEu;
      return true;
    }

    //                                                           [  _____                         ]
    // VMOV specific encoding {cmode=1111 op=0} - 32-bit pattern [abbbbbbcdefgh0000000000000000000].
    if (sz == 2u) {
      uint32_t b30 = (immLo >> 30u) & 0x1u;

      if (immLo == ((immLo & 0xC1F80000u) | (((b30 ^ 1u) * 0x1Fu) << 25u))) {
        _imm = (((immLo >> 24u) & 0xC0u) ^ 0x40u) | ((immLo >> 25) & 0x3Fu);
        return true;
      }
    }

    // VMOV specific encoding {cmode=1110 op=1} - 64-bit pattern where each byte is either 0x00 or 0xFF.
    {
      uint64_t byteMask = imm_64;

      if (sz == 1u) {
        byteMask |= byteMask << 16;
        byteMask |= byteMask << 32;
      }
      else if (sz == 2u) {
        byteMask |= byteMask << 32;
      }

      if (Utils::isByteMaskImm8(byteMask)) {
        _op = 1u;
        _imm = Utils::encodeImm64ByteMaskToImm8(byteMask);
        _cmode = 0xEu;
        return true;
      }
    }

    return false;
  }
};

struct VecVFPImmEncode : public ImmEncode {
  ASMJIT_FORCE_INLINE bool init(const Imm& immVFP) noexcept {
    double d = immVFP.type() == ImmType::kInt ? double(immVFP.value()) : immVFP.valueAs<double>();

    if (!Utils::isFP64Imm8(d))
      return false;

    _imm = Utils::encodeFP64ToImm8(d);
    return true;
  }
};

struct VecRot1ImmEncode : public ImmEncode {
  ASMJIT_FORCE_INLINE bool init(const Imm& rot1) noexcept {
    uint64_t value = rot1.valueAs<uint64_t>();
    _imm = value == 90u ? 0u : value == 270u ? 1u : 0xFFFFFFFFu;
    return _imm != 0xFFFFFFFFu;
  }
};

struct VecRot2ImmEncode : public ImmEncode {
  ASMJIT_FORCE_INLINE bool init(const Imm& rot2) noexcept {
    uint64_t value = rot2.valueAs<uint64_t>();
    _imm = value == 0u ? 0u :
           value == 90u ? 1u :
           value == 180u ? 2u :
           value == 270u ? 3u : 0xFFFFFFFFu;
    return _imm != 0xFFFFFFFFu;
  }
};

struct VecFBitsVFPEncode : public ImmEncode {
  ASMJIT_FORCE_INLINE bool init(uint32_t szInBits, const Imm& fbits) noexcept {
    uint64_t value = fbits.valueAs<uint64_t>();
    if (value >= szInBits)
      return false;

    _imm = szInBits - uint32_t(value);
    return true;
  }
};

struct VecFBitsASIMDEncode : public ImmEncode {
  ASMJIT_FORCE_INLINE bool init(const Imm& fbits) noexcept {
    uint64_t value = fbits.valueAs<uint64_t>();
    if (value >= 64u)
      return false;

    _imm = 64u - uint32_t(value);
    return true;
  }
};

struct VecShiftPImmEncode : public ImmEncode {
  ASMJIT_FORCE_INLINE bool init(uint32_t szField, const Imm& n) noexcept {
    uint64_t value = n.valueAs<uint64_t>();
    if (value >= (8u << szField))
      return false;

    _imm = (8u << szField) + uint32_t(value);
    return true;
  }
};

struct VecShiftNImmEncode : public ImmEncode {
  ASMJIT_FORCE_INLINE bool init(uint32_t szField, const Imm& n) noexcept {
    uint64_t value = n.valueAs<uint64_t>();
    if (value - 1u >= (8u << szField))
      return false;

    _imm = (16u << szField) - uint32_t(value);
    return true;
  }
};

struct VecShiftNarrowImmEncode : public ImmEncode {
  ASMJIT_FORCE_INLINE bool init(uint32_t szField, const Imm& n) noexcept {
    uint64_t value = n.valueAs<uint64_t>();
    if (value - 1u >= (4u << szField))
      return false;

    _imm = (8u << szField) - uint32_t(value);
    return true;
  }
};

} // {anonymous}

// a32::Assembler - RegList Encoders
// =======================================

namespace {

struct GpListImmEncode {
  uint32_t _mask;

  ASMJIT_FORCE_INLINE bool init(const BaseRegList& op, RegMask allowedMask = 0xFFFFu) noexcept {
    _mask = op.list();
    if (!_mask)
      return false;

    if (_mask & ~allowedMask)
      return false;

    return true;
  }

  ASMJIT_FORCE_INLINE uint32_t immMask() const noexcept { return _mask; }
};

struct VecSListImmEncode {
  uint32_t _index;
  uint32_t _count;

  ASMJIT_FORCE_INLINE bool init(const BaseRegList& op) noexcept {
    RegMask mask = op.list();
    if (!mask)
      return false;

    // The registers must be consecutive, so find index and count.
    _index = Support::ctz(mask);
    mask = mask >> _index;

    _count = Support::ctz(~mask);
    mask = mask >> _count;

    return mask == 0u && _index + _count <= 32u;
  }

  ASMJIT_FORCE_INLINE uint32_t immVd() const noexcept { return _index; }
  ASMJIT_FORCE_INLINE uint32_t immCount() const noexcept { return _count; }
};

struct VecDListImmEncode {
  uint32_t _index;
  uint32_t _count;

  ASMJIT_FORCE_INLINE bool init(const BaseRegList& op) noexcept {
    RegMask mask = op.list();
    if (!mask)
      return false;

    // The registers must be consecutive, so find index and count.
    _index = Support::ctz(mask);
    mask = mask >> _index;

    _count = Support::ctz(~mask);
    mask = mask >> _count;

    return mask == 0u && _index + _count <= 32u;
  }

  ASMJIT_FORCE_INLINE uint32_t immVd() const noexcept { return _index; }
  ASMJIT_FORCE_INLINE uint32_t immCount() const noexcept { return _count * 2u; }
};

} // {anonymous}

// a32::Assembler - Offset Encoders
// ================================

namespace {

struct SOffsetEncode {
  uint32_t _imm;
  uint32_t _u;

  ASMJIT_FORCE_INLINE SOffsetEncode(const Mem& m) noexcept {
    // Memory operand with base register always represents index as 32-bit signed integer. This means
    // it's safe to use `m.offsetLo32()` instead of `m.offset()` to avoid working with 64-bit offset.
    uint32_t offset = uint32_t(m.offsetLo32());
    uint32_t negMsk = uint32_t(int32_t(offset) >> 31);

    _imm = (offset ^ negMsk) - negMsk;
    _u = negMsk + 1u;
  }

  ASMJIT_FORCE_INLINE uint32_t imm() const noexcept { return _imm; }
  ASMJIT_FORCE_INLINE uint32_t u() const noexcept { return _u; }
};

} // {anonymous}

static ASMJIT_FORCE_INLINE InstId recombineInstId(InstId instId, uint32_t cc, uint32_t dt) noexcept {
  return instId | (cc << Support::ConstCTZ<uint32_t(InstIdParts::kARM_Cond)>::value)
                | (dt << Support::ConstCTZ<uint32_t(InstIdParts::kA32_DT)>::value);
}

// a32::Assembler - Construction & Destruction
// ===========================================

Assembler::Assembler(CodeHolder* code) noexcept : BaseAssembler() {
  _archMask = (uint64_t(1) << uint32_t(Arch::kARM  )) |
              (uint64_t(1) << uint32_t(Arch::kThumb)) ;
  if (code)
    code->attach(this);
}

Assembler::~Assembler() noexcept {}

// a32::Assembler - Emit
// =====================

Error Assembler::_emit(InstId instId, const Operand_& o0, const Operand_& o1, const Operand_& o2, const Operand_* opExt) {
  using DT = DataType;

  // Logging/Validation/Error.
  constexpr InstOptions kRequiresSpecialHandling = InstOptions::kReserved;

  // Instruction payload extraction.
  constexpr uint32_t kCondMask = uint32_t(InstIdParts::kARM_Cond);
  constexpr uint32_t kDtMask = uint32_t(InstIdParts::kA32_DT) | uint32_t(InstIdParts::kA32_DT2);

  // Operand #index in `opExt[]`.
  constexpr uint32_t kOp3 = EmitterUtils::kOp3;
  constexpr uint32_t kOp4 = EmitterUtils::kOp4;
  constexpr uint32_t kOp5 = EmitterUtils::kOp5;

  Error err;
  CodeWriter writer(this);

  uint32_t cc = (instId & kCondMask) >> Support::ConstCTZ<kCondMask>::value;
  uint32_t dtBits = (instId & kDtMask) >> Support::ConstCTZ<kDtMask>::value;

  instId &= ~(kCondMask | kDtMask);
  if (instId >= Inst::_kIdCount)
    instId = 0u;

  uint32_t opcode;

  const Operand_& o3 = opExt[kOp3];
  const Operand_& o4 = opExt[kOp4];
  const Operand_& o5 = opExt[kOp5];

  const Mem* mem = nullptr;
  const Operand_* rel = nullptr;

  // uint32_t multipleOpData[4];
  // uint32_t multipleOpCount;

  // These are only used when instruction uses a relative displacement.
  OffsetFormat offsetFormat;
  uint64_t offsetValue;

  // Combine all instruction options and also check whether the instruction
  // is valid. All options that require special handling (including invalid
  // instruction) are handled by the next branch.
  InstOptions options = InstOptions(instId == 0) | InstOptions((size_t)(_bufferEnd - writer.cursor()) < 4) | instOptions() | forcedInstOptions();
  InstDispatchRecord idr;

  // Combined signatures of input operands for quick checks.
  SignatureChecker sgn;
  sgn.init(o0, o1, o2, o3, o4, o5);

  if (Support::test(options, kRequiresSpecialHandling)) {
    if (ASMJIT_UNLIKELY(!_code))
      return reportError(DebugUtils::errored(kErrorNotInitialized));

    // Unknown instruction.
    if (ASMJIT_UNLIKELY(instId == 0))
      goto InvalidInstruction;

    // Grow request, happens rarely.
    err = writer.ensureSpace(this, 4);
    if (ASMJIT_UNLIKELY(err))
      goto Emit_Failed;
  }

  // ${a32::Assembler::Impl:Begin}
  // ------------------- Automatically generated, do not edit -------------------
  idr = instDispatchTable[instId];
  switch (idr.group) {
    case 0: {
      break;
    }

    case 1: {
      static const uint32_t opcodeTable[] = {
        0x00A00010u, 0x00A00000u, 0x00A00000u, 0x02A00000u, // Instruction 'adc'.
        0x00B00010u, 0x00B00000u, 0x00B00000u, 0x02B00000u, // Instruction 'adcs'.
        0x00800010u, 0x00800000u, 0x00800000u, 0x02800000u, // Instruction 'add'.
        0x00900010u, 0x00900000u, 0x00900000u, 0x02900000u, // Instruction 'adds'.
        0x00000010u, 0x00000000u, 0x00000000u, 0x02000000u, // Instruction 'and'.
        0x00100010u, 0x00100000u, 0x00100000u, 0x02100000u, // Instruction 'ands'.
        0x01C00010u, 0x01C00000u, 0x01C00000u, 0x03C00000u, // Instruction 'bic'.
        0x01D00010u, 0x01D00000u, 0x01D00000u, 0x03D00000u, // Instruction 'bics'.
        0x00200010u, 0x00200000u, 0x00200000u, 0x02200000u, // Instruction 'eor'.
        0x00300010u, 0x00300000u, 0x00300000u, 0x02300000u, // Instruction 'eors'.
        0x01800010u, 0x01800000u, 0x01800000u, 0x03800000u, // Instruction 'orr'.
        0x01900010u, 0x01900000u, 0x01900000u, 0x03900000u, // Instruction 'orrs'.
        0x00600010u, 0x00600000u, 0x00600000u, 0x02600000u, // Instruction 'rsb'.
        0x00700010u, 0x00700000u, 0x00700000u, 0x02700000u, // Instruction 'rsbs'.
        0x00E00010u, 0x00E00000u, 0x00E00000u, 0x02E00000u, // Instruction 'rsc'.
        0x00F00010u, 0x00F00000u, 0x00F00000u, 0x02F00000u, // Instruction 'rscs'.
        0x00C00010u, 0x00C00000u, 0x00C00000u, 0x02C00000u, // Instruction 'sbc'.
        0x00D00010u, 0x00D00000u, 0x00D00000u, 0x02D00000u, // Instruction 'sbcs'.
        0x00400010u, 0x00400000u, 0x00400000u, 0x02400000u, // Instruction 'sub'.
        0x00500010u, 0x00500000u, 0x00500000u, 0x02500000u  // Instruction 'subs'.
      };

      const uint32_t* opcodeTablePtr = opcodeTable + uint32_t(idr.index) * 4u;

      if (sgn.test<kOpRegR, kOpRegR, kOpRegR, kOpRegR>()) {
        uint32_t shiftOp = o3.as<Gp>().predicate();
        if (shiftOp <= 3u) {
          opcode = opcodeTablePtr[0];
          opcode |= shiftOp << 5u;
          goto Emit_R0At12Of4_R1At16Of4_R2At0Of4_R3At8Of4_Cond;
        }
      }

      if (sgn.test<kOpRegR, kOpRegR, kOpRegR>()) {
        opcode = opcodeTablePtr[1];
        goto Emit_R0At12Of4_R1At16Of4_R2At0Of4_Cond;
      }

      if (sgn.test<kOpRegR, kOpRegR, kOpRegR, kOpImmI>()) {
        uint32_t shiftOp = o3.as<Imm>().predicate();
        uint64_t shiftImm = o3.as<Imm>().valueAs<uint64_t>();
        if (shiftOp <= 3 && shiftImm <= 31u) {
          opcode = opcodeTablePtr[2];
          opcode |= shiftOp << 5u;
          opcode |= uint32_t(shiftImm) << 7u;
          goto Emit_R0At12Of4_R1At16Of4_R2At0Of4_Cond;
        }
      }

      if (sgn.test<kOpRegR, kOpRegR, kOpImmI>()) {
        ImmAEncode enc0;
        if (enc0.init(o2.as<Imm>())) {
          opcode = opcodeTablePtr[3];
          opcode |= enc0.imm() << 0u;
          goto Emit_R0At12Of4_R1At16Of4_Cond;
        }
      }

      break;
    }

    case 2: {
      // Instruction 'adr'.
      if (sgn.test<kOpRegR, kOpLabel>() || sgn.test<kOpRegR, kOpImmI>()) {
        rel = &o1;
        offsetFormat.resetToImmValue(OffsetType::kAArch32_ADR, 4, 0, 12, 0);
        opcode = 0x020F0000u;
        goto Emit_R0At12Of4_Rel_Cond;
      }

      break;
    }

    case 3: {
      static const uint32_t opcodeTable[] = {
        0xF3B00340u, // Instruction 'aesd'.
        0xF3B00300u, // Instruction 'aese'.
        0xF3B003C0u, // Instruction 'aesimc'.
        0xF3B00380u  // Instruction 'aesmc'.
      };

      const uint32_t* opcodeTablePtr = opcodeTable + uint32_t(idr.index) * 1u;

      if (sgn.test<kOpRegQ, kOpRegQ>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>())) {
          if (isDtMultiple(dtBits, makeDtBits(DT::kS8, DT::kU8))) {
            opcode = opcodeTablePtr[0];
            goto Emit_Q0At12Of4Hi22_Q1At0Of4Hi5_NoCond;
          }
        }
      }

      break;
    }

    case 4: {
      static const uint32_t opcodeTable[] = {
        0x01A00050u, 0x01A00040u, // Instruction 'asr'.
        0x01B00050u, 0x01B00040u  // Instruction 'asrs'.
      };

      const uint32_t* opcodeTablePtr = opcodeTable + uint32_t(idr.index) * 2u;

      if (sgn.test<kOpRegR, kOpRegR, kOpRegR>()) {
        opcode = opcodeTablePtr[0];
        goto Emit_R0At12Of4_R1At0Of4_R2At8Of4_Cond;
      }

      if (sgn.test<kOpRegR, kOpRegR, kOpImmI>()) {
        if (o2.as<Imm>().valueAs<uint64_t>() <= 0x1Fu) {
          opcode = opcodeTablePtr[1];
          opcode |= o2.as<Imm>().valueAs<uint32_t>() << 7u;
          goto Emit_R0At12Of4_R1At0Of4_Cond;
        }
      }

      break;
    }

    case 5: {
      static const uint32_t opcodeTable[] = {
        0x0A000000u, // Instruction 'b'.
        0x0B000000u  // Instruction 'bl'.
      };

      const uint32_t* opcodeTablePtr = opcodeTable + uint32_t(idr.index) * 1u;

      if (sgn.test<kOpLabel>() || sgn.test<kOpImmI>()) {
        rel = &o0;
        offsetFormat.resetToImmValue(OffsetType::kSignedOffset, 4, 0, 24, 2);
        opcode = opcodeTablePtr[0];
        goto Emit_Rel_Cond;
      }

      break;
    }

    case 6: {
      // Instruction 'bfc'.
      if (sgn.test<kOpRegR, kOpImmI, kOpImmI>()) {
        BfcBfiImmEncode enc0;
        if (enc0.init(o1.as<Imm>(), o2.as<Imm>())) {
          opcode = 0x07C0001Fu;
          opcode |= enc0.lsb() << 7u;
          opcode |= enc0.msb() << 16u;
          goto Emit_R0At12Of4_Cond;
        }
      }

      break;
    }

    case 7: {
      // Instruction 'bfi'.
      if (sgn.test<kOpRegR, kOpRegR, kOpImmI, kOpImmI>()) {
        BfcBfiImmEncode enc0;
        if (enc0.init(o2.as<Imm>(), o3.as<Imm>())) {
          opcode = 0x07C00010u;
          opcode |= enc0.lsb() << 7u;
          opcode |= enc0.msb() << 16u;
          goto Emit_R0At12Of4_R1At0Of4_Cond;
        }
      }

      break;
    }

    case 8: {
      static const uint32_t opcodeTable[] = {
        0x01200070u, // Instruction 'bkpt'.
        0x01400070u  // Instruction 'hvc'.
      };

      const uint32_t* opcodeTablePtr = opcodeTable + uint32_t(idr.index) * 1u;

      if (sgn.test<kOpImmI>()) {
        if (o0.as<Imm>().valueAs<uint64_t>() <= 0xFFFFu) {
          opcode = opcodeTablePtr[0];
          opcode |= (o0.as<Imm>().valueAs<uint32_t>() & 0xFu) << 0u;
          opcode |= (o0.as<Imm>().valueAs<uint32_t>() & 0xFFF0u) << 4u;
          goto Emit_Cond;
        }
      }

      break;
    }

    case 9: {
      // Instruction 'blx'.
      if (sgn.test<kOpRegR>()) {
        opcode = 0x012FFF30u;
        goto Emit_R0At0Of4_Cond;
      }

      if (sgn.test<kOpLabel>() || sgn.test<kOpImmI>()) {
        rel = &o0;
        offsetFormat.resetToImmValue(OffsetType::kAArch32_1To24At0_0At24, 4, 0, 25, 1);
        opcode = 0xFA000000u;
        goto Emit_Rel_NoCond;
      }

      break;
    }

    case 10: {
      static const uint32_t opcodeTable[] = {
        0x012FFF10u, // Instruction 'bx'.
        0x012FFF20u  // Instruction 'bxj'.
      };

      const uint32_t* opcodeTablePtr = opcodeTable + uint32_t(idr.index) * 1u;

      if (sgn.test<kOpRegR>()) {
        opcode = opcodeTablePtr[0];
        goto Emit_R0At0Of4_Cond;
      }

      break;
    }

    case 11: {
      static const uint32_t opcodeTable[] = {
        0x0320F016u, // Instruction 'clrbhb'.
        0x0320F014u, // Instruction 'csdb'.
        0x0160006Eu, // Instruction 'eret'.
        0x0320F010u, // Instruction 'esb'.
        0x0320F000u, // Instruction 'nop'.
        0x0320F004u, // Instruction 'sev'.
        0x0320F005u, // Instruction 'sevl'.
        0x0320F002u, // Instruction 'wfe'.
        0x0320F003u, // Instruction 'wfi'.
        0x0320F001u  // Instruction 'yield'.
      };

      const uint32_t* opcodeTablePtr = opcodeTable + uint32_t(idr.index) * 1u;

      if (sgn.empty()) {
        opcode = opcodeTablePtr[0];
        goto Emit_Cond;
      }

      break;
    }

    case 12: {
      static const uint32_t opcodeTable[] = {
        0xF57FF01Fu, // Instruction 'clrex'.
        0xF57FF044u, // Instruction 'pssbb'.
        0xF57FF070u, // Instruction 'sb'.
        0xF57FF040u  // Instruction 'ssbb'.
      };

      const uint32_t* opcodeTablePtr = opcodeTable + uint32_t(idr.index) * 1u;

      if (sgn.empty()) {
        opcode = opcodeTablePtr[0];
        goto Emit_NoCond;
      }

      break;
    }

    case 13: {
      static const uint32_t opcodeTable[] = {
        0x016F0F10u, // Instruction 'clz'.
        0x06FF0F30u, // Instruction 'rbit'.
        0x06BF0F30u, // Instruction 'rev'.
        0x06BF0FB0u, // Instruction 'rev16'.
        0x06FF0FB0u, // Instruction 'revsh'.
        0x01A00060u, // Instruction 'rrx'.
        0x01B00060u  // Instruction 'rrxs'.
      };

      const uint32_t* opcodeTablePtr = opcodeTable + uint32_t(idr.index) * 1u;

      if (sgn.test<kOpRegR, kOpRegR>()) {
        opcode = opcodeTablePtr[0];
        goto Emit_R0At12Of4_R1At0Of4_Cond;
      }

      break;
    }

    case 14: {
      static const uint32_t opcodeTable[] = {
        0x01700010u, 0x01700000u, 0x01700000u, 0x03700000u, // Instruction 'cmn'.
        0x01500010u, 0x01500000u, 0x01500000u, 0x03500000u, // Instruction 'cmp'.
        0x01300010u, 0x01300000u, 0x01300000u, 0x03300000u, // Instruction 'teq'.
        0x01100010u, 0x01100000u, 0x01100000u, 0x03100000u  // Instruction 'tst'.
      };

      const uint32_t* opcodeTablePtr = opcodeTable + uint32_t(idr.index) * 4u;

      if (sgn.test<kOpRegR, kOpRegR, kOpRegR>()) {
        uint32_t shiftOp = o2.as<Gp>().predicate();
        if (shiftOp <= 3u) {
          opcode = opcodeTablePtr[0];
          opcode |= shiftOp << 5u;
          goto Emit_R0At16Of4_R1At0Of4_R2At8Of4_Cond;
        }
      }

      if (sgn.test<kOpRegR, kOpRegR>()) {
        opcode = opcodeTablePtr[1];
        goto Emit_R0At16Of4_R1At0Of4_Cond;
      }

      if (sgn.test<kOpRegR, kOpRegR, kOpImmI>()) {
        uint32_t shiftOp = o2.as<Imm>().predicate();
        uint64_t shiftImm = o2.as<Imm>().valueAs<uint64_t>();
        if (shiftOp <= 3 && shiftImm <= 31u) {
          opcode = opcodeTablePtr[2];
          opcode |= shiftOp << 5u;
          opcode |= uint32_t(shiftImm) << 7u;
          goto Emit_R0At16Of4_R1At0Of4_Cond;
        }
      }

      if (sgn.test<kOpRegR, kOpImmI>()) {
        ImmAEncode enc0;
        if (enc0.init(o1.as<Imm>())) {
          opcode = opcodeTablePtr[3];
          opcode |= enc0.imm() << 0u;
          goto Emit_R0At16Of4_Cond;
        }
      }

      break;
    }

    case 15: {
      // Instruction 'cps'.
      if (sgn.test<kOpImmI>()) {
        if (o0.as<Imm>().valueAs<uint64_t>() <= 0x1Fu) {
          opcode = 0xF1020000u;
          opcode |= o0.as<Imm>().valueAs<uint32_t>() << 0u;
          goto Emit_NoCond;
        }
      }

      break;
    }

    case 16: {
      static const uint32_t opcodeTable[] = {
        0xF10C0000u, 0xF10E0000u, // Instruction 'cpsid'.
        0xF1080000u, 0xF10A0000u  // Instruction 'cpsie'.
      };

      const uint32_t* opcodeTablePtr = opcodeTable + uint32_t(idr.index) * 2u;

      if (sgn.test<kOpImmI>()) {
        if (o0.as<Imm>().valueAs<uint64_t>() <= 0x7u) {
          opcode = opcodeTablePtr[0];
          opcode |= o0.as<Imm>().valueAs<uint32_t>() << 6u;
          goto Emit_NoCond;
        }
      }

      if (sgn.test<kOpImmI, kOpImmI>()) {
        if (o0.as<Imm>().valueAs<uint64_t>() <= 0x7u && o1.as<Imm>().valueAs<uint64_t>() <= 0x1Fu) {
          opcode = opcodeTablePtr[1];
          opcode |= o1.as<Imm>().valueAs<uint32_t>() << 0u;
          opcode |= o0.as<Imm>().valueAs<uint32_t>() << 6u;
          goto Emit_NoCond;
        }
      }

      break;
    }

    case 17: {
      static const uint32_t opcodeTable[] = {
        0x01000040u, // Instruction 'crc32b'.
        0x01000240u, // Instruction 'crc32cb'.
        0x01200240u, // Instruction 'crc32ch'.
        0x01400240u, // Instruction 'crc32cw'.
        0x01200040u, // Instruction 'crc32h'.
        0x01400040u, // Instruction 'crc32w'.
        0x06200F10u, // Instruction 'qadd16'.
        0x06200F90u, // Instruction 'qadd8'.
        0x06200F30u, // Instruction 'qasx'.
        0x06200F50u, // Instruction 'qsax'.
        0x06200F70u, // Instruction 'qsub16'.
        0x06200FF0u, // Instruction 'qsub8'.
        0x06100F10u, // Instruction 'sadd16'.
        0x06100F90u, // Instruction 'sadd8'.
        0x06100F30u, // Instruction 'sasx'.
        0x06800FB0u, // Instruction 'sel'.
        0x06300F10u, // Instruction 'shadd16'.
        0x06300F90u, // Instruction 'shadd8'.
        0x06300F30u, // Instruction 'shasx'.
        0x06300F50u, // Instruction 'shsax'.
        0x06300F70u, // Instruction 'shsub16'.
        0x06300FF0u, // Instruction 'shsub8'.
        0x06100F50u, // Instruction 'ssax'.
        0x06100F70u, // Instruction 'ssub16'.
        0x06100FF0u, // Instruction 'ssub8'.
        0x06500F10u, // Instruction 'uadd16'.
        0x06500F90u, // Instruction 'uadd8'.
        0x06500F30u, // Instruction 'uasx'.
        0x06700F10u, // Instruction 'uhadd16'.
        0x06700F90u, // Instruction 'uhadd8'.
        0x06700F30u, // Instruction 'uhasx'.
        0x06700F50u, // Instruction 'uhsax'.
        0x06700F70u, // Instruction 'uhsub16'.
        0x06700FF0u, // Instruction 'uhsub8'.
        0x06600F10u, // Instruction 'uqadd16'.
        0x06600F90u, // Instruction 'uqadd8'.
        0x06600F30u, // Instruction 'uqasx'.
        0x06600F50u, // Instruction 'uqsax'.
        0x06600F70u, // Instruction 'uqsub16'.
        0x06600FF0u, // Instruction 'uqsub8'.
        0x06500F50u, // Instruction 'usax'.
        0x06500F70u, // Instruction 'usub16'.
        0x06500FF0u  // Instruction 'usub8'.
      };

      const uint32_t* opcodeTablePtr = opcodeTable + uint32_t(idr.index) * 1u;

      if (sgn.test<kOpRegR, kOpRegR, kOpRegR>()) {
        opcode = opcodeTablePtr[0];
        goto Emit_R0At12Of4_R1At16Of4_R2At0Of4_Cond;
      }

      break;
    }

    case 18: {
      static const uint32_t opcodeTable[] = {
        0x0320F0F0u, // Instruction 'dbg'.
        0x01600070u  // Instruction 'smc'.
      };

      const uint32_t* opcodeTablePtr = opcodeTable + uint32_t(idr.index) * 1u;

      if (sgn.test<kOpImmI>()) {
        if (o0.as<Imm>().valueAs<uint64_t>() <= 0xFu) {
          opcode = opcodeTablePtr[0];
          opcode |= o0.as<Imm>().valueAs<uint32_t>() << 0u;
          goto Emit_Cond;
        }
      }

      break;
    }

    case 19: {
      static const uint32_t opcodeTable[] = {
        0xF57FF050u, // Instruction 'dmb'.
        0xF57FF040u, // Instruction 'dsb'.
        0xF57FF060u  // Instruction 'isb'.
      };

      const uint32_t* opcodeTablePtr = opcodeTable + uint32_t(idr.index) * 1u;

      if (sgn.test<kOpImmI>()) {
        if (o0.as<Imm>().valueAs<uint64_t>() <= 0xFu) {
          opcode = opcodeTablePtr[0];
          opcode |= o0.as<Imm>().valueAs<uint32_t>() << 0u;
          goto Emit_NoCond;
        }
      }

      break;
    }

    case 20: {
      static const uint32_t opcodeTable[] = {
        0xE1000070u, // Instruction 'hlt'.
        0xE7F000F0u  // Instruction 'udf'.
      };

      const uint32_t* opcodeTablePtr = opcodeTable + uint32_t(idr.index) * 1u;

      if (sgn.test<kOpImmI>()) {
        if (o0.as<Imm>().valueAs<uint64_t>() <= 0xFFFFu) {
          opcode = opcodeTablePtr[0];
          opcode |= (o0.as<Imm>().valueAs<uint32_t>() & 0xFu) << 0u;
          opcode |= (o0.as<Imm>().valueAs<uint32_t>() & 0xFFF0u) << 4u;
          goto Emit_NoCond;
        }
      }

      break;
    }

    case 21: {
      static const uint32_t opcodeTable[] = {
        0x01900C9Fu, // Instruction 'lda'.
        0x01D00C9Fu, // Instruction 'ldab'.
        0x01900E9Fu, // Instruction 'ldaex'.
        0x01D00E9Fu, // Instruction 'ldaexb'.
        0x01F00E9Fu, // Instruction 'ldaexh'.
        0x01F00C9Fu, // Instruction 'ldah'.
        0x01900F9Fu, // Instruction 'ldrex'.
        0x01D00F9Fu, // Instruction 'ldrexb'.
        0x01F00F9Fu  // Instruction 'ldrexh'.
      };

      const uint32_t* opcodeTablePtr = opcodeTable + uint32_t(idr.index) * 1u;

      if (sgn.test<kOpRegR, kOpMemB>()) {
        mem = &o1.as<Mem>();

        if (mem->baseId() < 15u) {
          if (!mem->hasIndex()) {
            if (!mem->offsetLo32()) {
              if (mem->isFixedOffset()) {
                opcode = opcodeTablePtr[0];
                goto Emit_R0At12Of4_MemBaseAt16_Cond;
              }
            }
          }
        }
      }

      break;
    }

    case 22: {
      static const uint32_t opcodeTable[] = {
        0x01B00E9Fu, // Instruction 'ldaexd'.
        0x01B00F9Fu  // Instruction 'ldrexd'.
      };

      const uint32_t* opcodeTablePtr = opcodeTable + uint32_t(idr.index) * 1u;

      if (sgn.test<kOpRegR, kOpRegR, kOpMemB>()) {
        mem = &o2.as<Mem>();

        if (mem->baseId() < 15u) {
          if (!mem->hasIndex()) {
            if (!mem->offsetLo32()) {
              if (mem->isFixedOffset()) {
                if (isConsecutive(1, o0.as<Reg>(), o1.as<Reg>())) {
                  opcode = opcodeTablePtr[0];
                  goto Emit_R0At12Of4_MemBaseAt16_Cond;
                }
              }
            }
          }
        }
      }

      break;
    }

    case 23: {
      static const uint32_t opcodeTable[] = {
        0x08100000u, 0x08500000u, // Instruction 'ldmda'.
        0x09100000u, 0x09500000u, // Instruction 'ldmdb'.
        0x08900000u, 0x08D00000u, // Instruction 'ldmia'.
        0x09900000u, 0x09D00000u, // Instruction 'ldmib'.
        0x08000000u, 0x08400000u, // Instruction 'stmda'.
        0x09000000u, 0x09400000u, // Instruction 'stmdb'.
        0x08800000u, 0x08C00000u, // Instruction 'stmia'.
        0x09800000u, 0x09C00000u  // Instruction 'stmib'.
      };

      const uint32_t* opcodeTablePtr = opcodeTable + uint32_t(idr.index) * 2u;

      if (sgn.test<kOpMemB, kOpRegListR>()) {
        mem = &o0.as<Mem>();
        const GpList& regList = o1.as<GpList>();

        if (mem->baseId() < 15u) {
          if (!mem->hasIndex()) {
            if (!mem->offsetLo32()) {
              if (!mem->isPostIndex()) {
                GpListImmEncode enc0;
                if (enc0.init(regList)) {
                  opcode = opcodeTablePtr[0];
                  opcode |= enc0.immMask() << 0u;
                  goto Emit_MemBaseAt16W21_Cond;
                }
              }

              if (mem->isFixedOffset()) {
                GpListImmEncode enc1;
                if (enc1.init(regList)) {
                  opcode = opcodeTablePtr[1];
                  opcode |= enc1.immMask() << 0u;
                  goto Emit_MemBaseAt16_Cond;
                }
              }
            }
          }
        }
      }

      break;
    }

    case 24: {
      static const uint32_t opcodeTable[] = {
        0x06100000u, 0x04100000u, 0x05100000u, // Instruction 'ldr'.
        0x06500000u, 0x04500000u, 0x05500000u, // Instruction 'ldrb'.
        0x06000000u, 0x04000000u, 0x05000000u, // Instruction 'str'.
        0x06400000u, 0x04400000u, 0x05400000u  // Instruction 'strb'.
      };

      const uint32_t* opcodeTablePtr = opcodeTable + uint32_t(idr.index) * 3u;

      if (sgn.test<kOpRegR, kOpMemB>()) {
        mem = &o1.as<Mem>();

        if (mem->baseId() < 15u) {
          if (mem->indexType() == RegType::kGp32) {
            if (!mem->offsetLo32()) {
              if (mem->indexId() < 15u) {
                opcode = opcodeTablePtr[0];
                goto Emit_R0At12Of4_MemBaseAt16_MemSIndexAt0_SOPAt5_N5At7_P24W21_Cond;
              }
            }
          }

          if (!mem->hasIndex()) {
            if (checkSOffset(mem->offsetLo32(), 12, 0)) {
              opcode = opcodeTablePtr[1];
              goto Emit_R0At12Of4_MemBaseAt16_SOffAt0Of12_P24W21_Cond;
            }
          }
        }
      }

      if (sgn.test<kOpRegR, kOpMemAny>()) {
        mem = &o1.as<Mem>();

        if (isMemPCRel(*mem)) {
          if (mem->isFixedOffset()) {
            opcode = opcodeTablePtr[2];
            opcode |= 0xFu << 16;
            offsetFormat.resetToImmValue(OffsetType::kAArch32_U23_SignedOffset, 4, 0, 12, 0);
            goto Emit_R0At12Of4_MemPCRel_Cond;
          }
        }
      }

      break;
    }

    case 25: {
      static const uint32_t opcodeTable[] = {
        0x06700000u, 0x04700000u, // Instruction 'ldrbt'.
        0x06300000u, 0x04300000u, // Instruction 'ldrt'.
        0x06200000u, 0x04200000u  // Instruction 'strt'.
      };

      const uint32_t* opcodeTablePtr = opcodeTable + uint32_t(idr.index) * 2u;

      if (sgn.test<kOpRegR, kOpMemB>()) {
        mem = &o1.as<Mem>();

        if (mem->baseId() < 15u) {
          if (mem->indexType() == RegType::kGp32) {
            if (!mem->offsetLo32()) {
              if (mem->indexId() < 15u) {
                if (mem->isPostIndex()) {
                  opcode = opcodeTablePtr[0];
                  goto Emit_R0At12Of4_MemBaseAt16_MemSIndexAt0_SOPAt5_N5At7_Cond;
                }
              }
            }
          }

          if (!mem->hasIndex()) {
            if (checkSOffset(mem->offsetLo32(), 12, 0)) {
              if (mem->isPostIndex()) {
                opcode = opcodeTablePtr[1];
                goto Emit_R0At12Of4_MemBaseAt16_SOffAt0Of12_Cond;
              }
            }
          }
        }
      }

      break;
    }

    case 26: {
      // Instruction 'ldrd'.
      if (sgn.test<kOpRegR, kOpRegR, kOpMemB>()) {
        mem = &o2.as<Mem>();

        if (mem->baseId() < 15u) {
          if (mem->indexType() == RegType::kGp32) {
            if (!mem->offsetLo32()) {
              if (mem->indexId() <= 15u) {
                if (isConsecutive(1, o0.as<Reg>(), o1.as<Reg>())) {
                  opcode = 0x000000D0u;
                  goto Emit_R0At12Of4_MemBaseAt16_MemSIndexAt0_P24W21_Cond;
                }
              }
            }
          }

          if (!mem->hasIndex()) {
            if (checkSOffset(mem->offsetLo32(), 8, 0)) {
              if (!mem->isPostIndex()) {
                if (isConsecutive(1, o0.as<Reg>(), o1.as<Reg>())) {
                  opcode = 0x004000D0u;
                  goto Emit_R0At12Of4_MemBaseAt16_SOffAt0Of4_SOffAt8Of4_P24W21_Cond;
                }
              }
            }
          }
        }
      }

      if (sgn.test<kOpRegR, kOpRegR, kOpMemAny>()) {
        mem = &o2.as<Mem>();

        if (isMemPCRel(*mem)) {
          if (mem->isFixedOffset()) {
            if (isConsecutive(1, o0.as<Reg>(), o1.as<Reg>())) {
              opcode = 0x014000D0u;
              opcode |= 0xFu << 16;
              offsetFormat.resetToImmValue(OffsetType::kAArch32_U23_0To3At0_4To7At8, 4, 0, 8, 0);
              goto Emit_R0At12Of4_MemPCRel_Cond;
            }
          }
        }
      }

      break;
    }

    case 27: {
      static const uint32_t opcodeTable[] = {
        0x001000B0u, 0x005000B0u, 0x015000B0u, // Instruction 'ldrh'.
        0x001000D0u, 0x005000D0u, 0x015000D0u, // Instruction 'ldrsb'.
        0x001000F0u, 0x005000F0u, 0x015000F0u, // Instruction 'ldrsh'.
        0x000000B0u, 0x004000B0u, 0x014000B0u  // Instruction 'strh'.
      };

      const uint32_t* opcodeTablePtr = opcodeTable + uint32_t(idr.index) * 3u;

      if (sgn.test<kOpRegR, kOpMemB>()) {
        mem = &o1.as<Mem>();

        if (mem->baseId() < 15u) {
          if (mem->indexType() == RegType::kGp32) {
            if (!mem->offsetLo32()) {
              if (mem->indexId() < 15u) {
                opcode = opcodeTablePtr[0];
                goto Emit_R0At12Of4_MemBaseAt16_MemSIndexAt0_P24W21_Cond;
              }
            }
          }

          if (!mem->hasIndex()) {
            if (checkSOffset(mem->offsetLo32(), 8, 0)) {
              opcode = opcodeTablePtr[1];
              goto Emit_R0At12Of4_MemBaseAt16_SOffAt0Of4_SOffAt8Of4_P24W21_Cond;
            }
          }
        }
      }

      if (sgn.test<kOpRegR, kOpMemAny>()) {
        mem = &o1.as<Mem>();

        if (isMemPCRel(*mem)) {
          if (mem->isFixedOffset()) {
            opcode = opcodeTablePtr[2];
            opcode |= 0xFu << 16;
            offsetFormat.resetToImmValue(OffsetType::kAArch32_U23_0To3At0_4To7At8, 4, 0, 8, 0);
            goto Emit_R0At12Of4_MemPCRel_Cond;
          }
        }
      }

      break;
    }

    case 28: {
      static const uint32_t opcodeTable[] = {
        0x003000B0u, 0x007000B0u, // Instruction 'ldrht'.
        0x002000B0u, 0x006000B0u  // Instruction 'strht'.
      };

      const uint32_t* opcodeTablePtr = opcodeTable + uint32_t(idr.index) * 2u;

      if (sgn.test<kOpRegR, kOpMemB>()) {
        mem = &o1.as<Mem>();

        if (mem->baseId() < 15u) {
          if (mem->indexType() == RegType::kGp32) {
            if (!mem->offsetLo32()) {
              if (mem->indexId() < 15u) {
                if (mem->isPostIndex()) {
                  opcode = opcodeTablePtr[0];
                  goto Emit_R0At12Of4_MemBaseAt16_MemSIndexAt0_Cond;
                }
              }
            }
          }

          if (!mem->hasIndex()) {
            if (checkSOffset(mem->offsetLo32(), 8, 0)) {
              if (mem->isPostIndex()) {
                opcode = opcodeTablePtr[1];
                goto Emit_R0At12Of4_MemBaseAt16_SOffAt0Of4_SOffAt8Of4_Cond;
              }
            }
          }
        }
      }

      break;
    }

    case 29: {
      // Instruction 'ldrsbt'.
      if (sgn.test<kOpRegR, kOpMemB>()) {
        mem = &o1.as<Mem>();

        if (mem->baseId() < 15u) {
          if (mem->indexType() == RegType::kGp32) {
            if (!mem->offsetLo32()) {
              if (mem->indexId() <= 15u) {
                if (mem->isPostIndex()) {
                  opcode = 0x003000D0u;
                  goto Emit_R0At12Of4_MemBaseAt16_MemSIndexAt0_Cond;
                }
              }
            }
          }

          if (!mem->hasIndex()) {
            if (checkSOffset(mem->offsetLo32(), 8, 0)) {
              if (mem->isPostIndex()) {
                opcode = 0x007000D0u;
                goto Emit_R0At12Of4_MemBaseAt16_SOffAt0Of4_SOffAt8Of4_Cond;
              }
            }
          }
        }
      }

      break;
    }

    case 30: {
      // Instruction 'ldrsht'.
      if (sgn.test<kOpRegR, kOpMemB>()) {
        mem = &o1.as<Mem>();

        if (mem->baseId() < 15u) {
          if (!mem->hasIndex()) {
            if (checkSOffset(mem->offsetLo32(), 8, 0)) {
              if (mem->isPostIndex()) {
                opcode = 0x007000F0u;
                goto Emit_R0At12Of4_MemBaseAt16_SOffAt0Of4_SOffAt8Of4_Cond;
              }
            }
          }

          if (mem->indexType() == RegType::kGp32) {
            if (!mem->offsetLo32()) {
              if (mem->indexId() < 15u) {
                if (mem->isPostIndex()) {
                  opcode = 0x003000F0u;
                  goto Emit_R0At12Of4_MemBaseAt16_MemSIndexAt0_Cond;
                }
              }
            }
          }
        }
      }

      break;
    }

    case 31: {
      static const uint32_t opcodeTable[] = {
        0x01A00000u, 0x01A00010u, // Instruction 'lsl'.
        0x01B00000u, 0x01B00010u, // Instruction 'lsls'.
        0x01A00020u, 0x01A00030u, // Instruction 'lsr'.
        0x01B00020u, 0x01B00030u, // Instruction 'lsrs'.
        0x01A00060u, 0x01A00070u, // Instruction 'ror'.
        0x01B00060u, 0x01B00070u  // Instruction 'rors'.
      };

      const uint32_t* opcodeTablePtr = opcodeTable + uint32_t(idr.index) * 2u;

      if (sgn.test<kOpRegR, kOpRegR, kOpImmI>()) {
        if (o2.as<Imm>().valueAs<uint64_t>() <= 0x1Fu) {
          opcode = opcodeTablePtr[0];
          opcode |= o2.as<Imm>().valueAs<uint32_t>() << 7u;
          goto Emit_R0At12Of4_R1At0Of4_Cond;
        }
      }

      if (sgn.test<kOpRegR, kOpRegR, kOpRegR>()) {
        opcode = opcodeTablePtr[1];
        goto Emit_R0At12Of4_R1At0Of4_R2At8Of4_Cond;
      }

      break;
    }

    case 32: {
      static const uint32_t opcodeTable[] = {
        0x0E000010u, 0x0E000010u, // Instruction 'mcr'.
        0x0E100010u, 0x0E100010u  // Instruction 'mrc'.
      };

      const uint32_t* opcodeTablePtr = opcodeTable + uint32_t(idr.index) * 2u;

      if (sgn.test<kOpImmI, kOpImmI, kOpRegR, kOpRegC, kOpRegC>()) {
        if (o0.as<Imm>().valueAs<uint64_t>() <= 0xFu && o1.as<Imm>().valueAs<uint64_t>() <= 0x7u) {
          opcode = opcodeTablePtr[0];
          opcode |= o0.as<Imm>().valueAs<uint32_t>() << 8u;
          opcode |= o1.as<Imm>().valueAs<uint32_t>() << 21u;
          goto Emit_R2At12Of4_R3At16Of4_R4At0Of4_Cond;
        }
      }

      if (sgn.test<kOpImmI, kOpImmI, kOpRegR, kOpRegC, kOpRegC, kOpImmI>()) {
        if (o0.as<Imm>().valueAs<uint64_t>() <= 0xFu && o1.as<Imm>().valueAs<uint64_t>() <= 0x7u && o5.as<Imm>().valueAs<uint64_t>() <= 0x7u) {
          opcode = opcodeTablePtr[1];
          opcode |= o5.as<Imm>().valueAs<uint32_t>() << 5u;
          opcode |= o0.as<Imm>().valueAs<uint32_t>() << 8u;
          opcode |= o1.as<Imm>().valueAs<uint32_t>() << 21u;
          goto Emit_R2At12Of4_R3At16Of4_R4At0Of4_Cond;
        }
      }

      break;
    }

    case 33: {
      static const uint32_t opcodeTable[] = {
        0xFE000010u, 0xFE000010u, // Instruction 'mcr2'.
        0xFE100010u, 0xFE100010u  // Instruction 'mrc2'.
      };

      const uint32_t* opcodeTablePtr = opcodeTable + uint32_t(idr.index) * 2u;

      if (sgn.test<kOpImmI, kOpImmI, kOpRegR, kOpRegC, kOpRegC>()) {
        if (o0.as<Imm>().valueAs<uint64_t>() <= 0xFu && o1.as<Imm>().valueAs<uint64_t>() <= 0x7u) {
          opcode = opcodeTablePtr[0];
          opcode |= o0.as<Imm>().valueAs<uint32_t>() << 8u;
          opcode |= o1.as<Imm>().valueAs<uint32_t>() << 21u;
          goto Emit_R2At12Of4_R3At16Of4_R4At0Of4_NoCond;
        }
      }

      if (sgn.test<kOpImmI, kOpImmI, kOpRegR, kOpRegC, kOpRegC, kOpImmI>()) {
        if (o0.as<Imm>().valueAs<uint64_t>() <= 0xFu && o1.as<Imm>().valueAs<uint64_t>() <= 0x7u && o5.as<Imm>().valueAs<uint64_t>() <= 0x7u) {
          opcode = opcodeTablePtr[1];
          opcode |= o5.as<Imm>().valueAs<uint32_t>() << 5u;
          opcode |= o0.as<Imm>().valueAs<uint32_t>() << 8u;
          opcode |= o1.as<Imm>().valueAs<uint32_t>() << 21u;
          goto Emit_R2At12Of4_R3At16Of4_R4At0Of4_NoCond;
        }
      }

      break;
    }

    case 34: {
      static const uint32_t opcodeTable[] = {
        0x0C400000u, // Instruction 'mcrr'.
        0x0C500000u  // Instruction 'mrrc'.
      };

      const uint32_t* opcodeTablePtr = opcodeTable + uint32_t(idr.index) * 1u;

      if (sgn.test<kOpImmI, kOpImmI, kOpRegR, kOpRegR, kOpRegC>()) {
        if (o0.as<Imm>().valueAs<uint64_t>() <= 0xFu && o1.as<Imm>().valueAs<uint64_t>() <= 0xFu) {
          opcode = opcodeTablePtr[0];
          opcode |= o1.as<Imm>().valueAs<uint32_t>() << 4u;
          opcode |= o0.as<Imm>().valueAs<uint32_t>() << 8u;
          goto Emit_R2At12Of4_R3At16Of4_R4At0Of4_Cond;
        }
      }

      break;
    }

    case 35: {
      static const uint32_t opcodeTable[] = {
        0xFC400000u, // Instruction 'mcrr2'.
        0xFC500000u  // Instruction 'mrrc2'.
      };

      const uint32_t* opcodeTablePtr = opcodeTable + uint32_t(idr.index) * 1u;

      if (sgn.test<kOpImmI, kOpImmI, kOpRegR, kOpRegR, kOpRegC>()) {
        if (o0.as<Imm>().valueAs<uint64_t>() <= 0xFu && o1.as<Imm>().valueAs<uint64_t>() <= 0xFu) {
          opcode = opcodeTablePtr[0];
          opcode |= o1.as<Imm>().valueAs<uint32_t>() << 4u;
          opcode |= o0.as<Imm>().valueAs<uint32_t>() << 8u;
          goto Emit_R2At12Of4_R3At16Of4_R4At0Of4_NoCond;
        }
      }

      break;
    }

    case 36: {
      static const uint32_t opcodeTable[] = {
        0x00200090u, // Instruction 'mla'.
        0x00300090u, // Instruction 'mlas'.
        0x00600090u, // Instruction 'mls'.
        0x01000080u, // Instruction 'smlabb'.
        0x010000C0u, // Instruction 'smlabt'.
        0x07000010u, // Instruction 'smlad'.
        0x07000030u, // Instruction 'smladx'.
        0x010000A0u, // Instruction 'smlatb'.
        0x010000E0u, // Instruction 'smlatt'.
        0x01200080u, // Instruction 'smlawb'.
        0x012000C0u, // Instruction 'smlawt'.
        0x07000050u, // Instruction 'smlsd'.
        0x07000070u, // Instruction 'smlsdx'.
        0x07500010u, // Instruction 'smmla'.
        0x07500030u, // Instruction 'smmlar'.
        0x075000D0u, // Instruction 'smmls'.
        0x075000F0u, // Instruction 'smmlsr'.
        0x07800010u  // Instruction 'usada8'.
      };

      const uint32_t* opcodeTablePtr = opcodeTable + uint32_t(idr.index) * 1u;

      if (sgn.test<kOpRegR, kOpRegR, kOpRegR, kOpRegR>()) {
        opcode = opcodeTablePtr[0];
        goto Emit_R0At16Of4_R1At0Of4_R2At8Of4_R3At12Of4_Cond;
      }

      break;
    }

    case 37: {
      static const uint32_t opcodeTable[] = {
        0x01A00010u, 0x01A00000u, 0x01A00000u, 0x03A00000u, // Instruction 'mov'.
        0x01B00010u, 0x01B00000u, 0x01B00000u, 0x03B00000u, // Instruction 'movs'.
        0x01E00010u, 0x01E00000u, 0x01E00000u, 0x03E00000u, // Instruction 'mvn'.
        0x01F00010u, 0x01F00000u, 0x01F00000u, 0x03F00000u  // Instruction 'mvns'.
      };

      const uint32_t* opcodeTablePtr = opcodeTable + uint32_t(idr.index) * 4u;

      if (sgn.test<kOpRegR, kOpRegR, kOpRegR>()) {
        uint32_t shiftOp = o2.as<Gp>().predicate();
        if (shiftOp <= 3u) {
          opcode = opcodeTablePtr[0];
          opcode |= shiftOp << 5u;
          goto Emit_R0At12Of4_R1At0Of4_R2At8Of4_Cond;
        }
      }

      if (sgn.test<kOpRegR, kOpRegR>()) {
        opcode = opcodeTablePtr[1];
        goto Emit_R0At12Of4_R1At0Of4_Cond;
      }

      if (sgn.test<kOpRegR, kOpRegR, kOpImmI>()) {
        uint32_t shiftOp = o2.as<Imm>().predicate();
        uint64_t shiftImm = o2.as<Imm>().valueAs<uint64_t>();
        if (shiftOp <= 3 && shiftImm <= 31u) {
          opcode = opcodeTablePtr[2];
          opcode |= shiftOp << 5u;
          opcode |= uint32_t(shiftImm) << 7u;
          goto Emit_R0At12Of4_R1At0Of4_Cond;
        }
      }

      if (sgn.test<kOpRegR, kOpImmI>()) {
        ImmAEncode enc0;
        if (enc0.init(o1.as<Imm>())) {
          opcode = opcodeTablePtr[3];
          opcode |= enc0.imm() << 0u;
          goto Emit_R0At12Of4_Cond;
        }
      }

      break;
    }

    case 38: {
      static const uint32_t opcodeTable[] = {
        0x03400000u, // Instruction 'movt'.
        0x03000000u  // Instruction 'movw'.
      };

      const uint32_t* opcodeTablePtr = opcodeTable + uint32_t(idr.index) * 1u;

      if (sgn.test<kOpRegR, kOpImmI>()) {
        if (o1.as<Imm>().valueAs<uint64_t>() <= 0xFFFFu) {
          opcode = opcodeTablePtr[0];
          opcode |= (o1.as<Imm>().valueAs<uint32_t>() & 0xFFFu) << 0u;
          opcode |= (o1.as<Imm>().valueAs<uint32_t>() & 0xF000u) << 4u;
          goto Emit_R0At12Of4_Cond;
        }
      }

      break;
    }

    case 39: {
      // Instruction 'mrs'.
      if (sgn.test<kOpRegR, kOpImmI>()) {
        if (o1.as<Imm>().valueAs<uint64_t>() <= 0x1u) {
          opcode = 0x010F0000u;
          opcode |= o1.as<Imm>().valueAs<uint32_t>() << 22u;
          goto Emit_R0At12Of4_Cond;
        }
      }

      break;
    }

    case 40: {
      // Instruction 'msr'.
      if (sgn.test<kOpImmI, kOpRegR>()) {
        if (o0.as<Imm>().valueAs<uint64_t>() <= 0x3u) {
          opcode = 0x0120F000u;
          opcode |= o0.as<Imm>().valueAs<uint32_t>() << 18u;
          goto Emit_R1At0Of4_Cond;
        }
      }

      if (sgn.test<kOpImmI, kOpImmI>()) {
        if (o0.as<Imm>().valueAs<uint64_t>() <= 0x3u) {
          ImmAEncode enc0;
          if (enc0.init(o1.as<Imm>())) {
            opcode = 0x0320F000u;
            opcode |= enc0.imm() << 0u;
            opcode |= o0.as<Imm>().valueAs<uint32_t>() << 18u;
            goto Emit_Cond;
          }
        }
      }

      break;
    }

    case 41: {
      static const uint32_t opcodeTable[] = {
        0x00000090u, // Instruction 'mul'.
        0x00100090u, // Instruction 'muls'.
        0x0710F010u, // Instruction 'sdiv'.
        0x0750F010u, // Instruction 'smmul'.
        0x0750F030u, // Instruction 'smmulr'.
        0x0700F010u, // Instruction 'smuad'.
        0x0700F030u, // Instruction 'smuadx'.
        0x01600080u, // Instruction 'smulbb'.
        0x016000C0u, // Instruction 'smulbt'.
        0x016000A0u, // Instruction 'smultb'.
        0x016000E0u, // Instruction 'smultt'.
        0x012000A0u, // Instruction 'smulwb'.
        0x012000E0u, // Instruction 'smulwt'.
        0x0700F050u, // Instruction 'smusd'.
        0x0700F070u, // Instruction 'smusdx'.
        0x0730F010u, // Instruction 'udiv'.
        0x0780F010u  // Instruction 'usad8'.
      };

      const uint32_t* opcodeTablePtr = opcodeTable + uint32_t(idr.index) * 1u;

      if (sgn.test<kOpRegR, kOpRegR, kOpRegR>()) {
        opcode = opcodeTablePtr[0];
        goto Emit_R0At16Of4_R1At0Of4_R2At8Of4_Cond;
      }

      break;
    }

    case 42: {
      // Instruction 'pkhbt'.
      if (sgn.test<kOpRegR, kOpRegR, kOpRegR>()) {
        opcode = 0x06800010u;
        goto Emit_R0At12Of4_R1At16Of4_R2At0Of4_Cond;
      }

      if (sgn.test<kOpRegR, kOpRegR, kOpRegR, kOpImmI>()) {
        uint64_t shiftImm = o3.as<Imm>().valueAs<uint64_t>();
        if (o3.as<Imm>().predicate() == uint32_t(ShiftOp::kLSL) && shiftImm <= 31u) {
          opcode = 0x06800010u;
          opcode |= uint32_t(shiftImm) << 7u;
          goto Emit_R0At12Of4_R1At16Of4_R2At0Of4_Cond;
        }
      }

      break;
    }

    case 43: {
      // Instruction 'pkhtb'.
      if (sgn.test<kOpRegR, kOpRegR, kOpRegR>()) {
        opcode = 0x06800050u;
        goto Emit_R0At12Of4_R1At16Of4_R2At0Of4_Cond;
      }

      if (sgn.test<kOpRegR, kOpRegR, kOpRegR, kOpImmI>()) {
        uint64_t shiftImm = o3.as<Imm>().valueAs<uint64_t>();
        if (o3.as<Imm>().predicate() == uint32_t(ShiftOp::kASR) && shiftImm <= 31u) {
          opcode = 0x06800050u;
          opcode |= uint32_t(shiftImm) << 7u;
          goto Emit_R0At12Of4_R1At16Of4_R2At0Of4_Cond;
        }
      }

      break;
    }

    case 44: {
      // Instruction 'pld'.
      if (sgn.test<kOpMemB>()) {
        mem = &o0.as<Mem>();

        if (mem->baseId() < 15u) {
          if (mem->indexType() == RegType::kGp32) {
            if (!mem->offsetLo32()) {
              if (mem->indexId() < 15u) {
                if (mem->isFixedOffset()) {
                  opcode = 0xF750F000u;
                  goto Emit_MemBaseAt16_MemSIndexAt0_SOPAt5_N5At7_NoCond;
                }
              }
            }
          }
        }

        if (mem->baseId() <= 15u) {
          if (!mem->hasIndex()) {
            if (checkSOffset(mem->offsetLo32(), 12, 0)) {
              if (mem->isFixedOffset()) {
                opcode = 0xF550F000u;
                goto Emit_MemBaseAt16_SOffAt0Of12_NoCond;
              }
            }
          }
        }
      }

      if (sgn.test<kOpMemAny>()) {
        mem = &o0.as<Mem>();

        if (isMemPCRel(*mem)) {
          if (mem->isFixedOffset()) {
            opcode = 0xF550F000u;
            opcode |= 0xFu << 16;
            offsetFormat.resetToImmValue(OffsetType::kAArch32_U23_SignedOffset, 4, 0, 12, 0);
            goto Emit_MemPCRel_NoCond;
          }
        }
      }

      break;
    }

    case 45: {
      // Instruction 'pldw'.
      if (sgn.test<kOpMemB>()) {
        mem = &o0.as<Mem>();

        if (mem->baseId() < 15u) {
          if (mem->indexType() == RegType::kGp32) {
            if (!mem->offsetLo32()) {
              if (mem->indexId() < 15u) {
                if (mem->isFixedOffset()) {
                  opcode = 0xF710F000u;
                  goto Emit_MemBaseAt16_MemSIndexAt0_SOPAt5_N5At7_NoCond;
                }
              }
            }
          }

          if (!mem->hasIndex()) {
            if (checkSOffset(mem->offsetLo32(), 12, 0)) {
              if (mem->isFixedOffset()) {
                opcode = 0xF510F000u;
                goto Emit_MemBaseAt16_SOffAt0Of12_NoCond;
              }
            }
          }
        }
      }

      break;
    }

    case 46: {
      // Instruction 'pli'.
      if (sgn.test<kOpMemB>()) {
        mem = &o0.as<Mem>();

        if (mem->baseId() <= 15u) {
          if (mem->indexType() == RegType::kGp32) {
            if (!mem->offsetLo32()) {
              if (mem->indexId() < 15u) {
                if (mem->isFixedOffset()) {
                  opcode = 0xF650F000u;
                  goto Emit_MemBaseAt16_MemSIndexAt0_SOPAt5_N5At7_NoCond;
                }
              }
            }
          }

          if (!mem->hasIndex()) {
            if (checkSOffset(mem->offsetLo32(), 12, 0)) {
              if (mem->isFixedOffset()) {
                opcode = 0xF450F000u;
                goto Emit_MemBaseAt16_SOffAt0Of12_NoCond;
              }
            }
          }
        }

        if (mem->baseId() == 15u && !mem->offsetLo32()) {
          if (mem->isFixedOffset()) {
            opcode = 0xF650F000u;
            goto Emit_MemBaseAt16_MemSIndexAt0_SOPAt5_N5At7_NoCond;
          }
        }
      }

      if (sgn.test<kOpMemAny>()) {
        mem = &o0.as<Mem>();

        if (isMemPCRel(*mem)) {
          if (mem->isFixedOffset()) {
            opcode = 0xF450F000u;
            opcode |= 0xFu << 16;
            offsetFormat.resetToImmValue(OffsetType::kAArch32_U23_SignedOffset, 4, 0, 12, 0);
            goto Emit_MemPCRel_NoCond;
          }
        }
      }

      break;
    }

    case 47: {
      static const uint32_t opcodeTable[] = {
        0x049D0004u, 0x08BD0000u, // Instruction 'pop'.
        0x052D0004u, 0x092D0000u  // Instruction 'push'.
      };

      const uint32_t* opcodeTablePtr = opcodeTable + uint32_t(idr.index) * 2u;

      if (sgn.test<kOpRegR>()) {
        opcode = opcodeTablePtr[0];
        goto Emit_R0At12Of4_Cond;
      }

      if (sgn.test<kOpRegListR>()) {
        GpListImmEncode enc0;
        const GpList& regList = o0.as<GpList>();
        if (enc0.init(regList)) {
          opcode = opcodeTablePtr[1];
          opcode |= enc0.immMask() << 0u;
          goto Emit_Cond;
        }
      }

      break;
    }

    case 48: {
      static const uint32_t opcodeTable[] = {
        0x01000050u, // Instruction 'qadd'.
        0x01400050u, // Instruction 'qdadd'.
        0x01600050u, // Instruction 'qdsub'.
        0x01200050u  // Instruction 'qsub'.
      };

      const uint32_t* opcodeTablePtr = opcodeTable + uint32_t(idr.index) * 1u;

      if (sgn.test<kOpRegR, kOpRegR, kOpRegR>()) {
        opcode = opcodeTablePtr[0];
        goto Emit_R0At12Of4_R1At0Of4_R2At16Of4_Cond;
      }

      break;
    }

    case 49: {
      static const uint32_t opcodeTable[] = {
        0xF8100A00u, // Instruction 'rfeda'.
        0xF9100A00u, // Instruction 'rfedb'.
        0xF8900A00u, // Instruction 'rfeia'.
        0xF9900A00u  // Instruction 'rfeib'.
      };

      const uint32_t* opcodeTablePtr = opcodeTable + uint32_t(idr.index) * 1u;

      if (sgn.test<kOpMemB>()) {
        mem = &o0.as<Mem>();

        if (mem->baseId() < 15u) {
          if (!mem->hasIndex()) {
            if (!mem->offsetLo32()) {
              if (!mem->isPostIndex()) {
                opcode = opcodeTablePtr[0];
                goto Emit_MemBaseAt16W21_NoCond;
              }
            }
          }
        }
      }

      break;
    }

    case 50: {
      static const uint32_t opcodeTable[] = {
        0x07A00050u, // Instruction 'sbfx'.
        0x07E00050u  // Instruction 'ubfx'.
      };

      const uint32_t* opcodeTablePtr = opcodeTable + uint32_t(idr.index) * 1u;

      if (sgn.test<kOpRegR, kOpRegR, kOpImmI, kOpImmI>()) {
        SbfxUbfxImmEncode enc0;
        if (enc0.init(o2.as<Imm>(), o3.as<Imm>())) {
          opcode = opcodeTablePtr[0];
          opcode |= enc0.lsb() << 7u;
          opcode |= enc0.widthM1() << 16u;
          goto Emit_R0At12Of4_R1At0Of4_Cond;
        }
      }

      break;
    }

    case 51: {
      static const uint32_t opcodeTable[] = {
        0xF1010000u, // Instruction 'setend'.
        0xF1100000u  // Instruction 'setpan'.
      };

      const uint32_t* opcodeTablePtr = opcodeTable + uint32_t(idr.index) * 1u;

      if (sgn.test<kOpImmI>()) {
        if (o0.as<Imm>().valueAs<uint64_t>() <= 0x1u) {
          opcode = opcodeTablePtr[0];
          opcode |= o0.as<Imm>().valueAs<uint32_t>() << 9u;
          goto Emit_NoCond;
        }
      }

      break;
    }

    case 52: {
      static const uint32_t opcodeTable[] = {
        0xF2000C40u, // Instruction 'sha1c'.
        0xF2200C40u, // Instruction 'sha1m'.
        0xF2100C40u, // Instruction 'sha1p'.
        0xF2300C40u, // Instruction 'sha1su0'.
        0xF3000C40u, // Instruction 'sha256h'.
        0xF3100C40u, // Instruction 'sha256h2'.
        0xF3200C40u  // Instruction 'sha256su1'.
      };

      const uint32_t* opcodeTablePtr = opcodeTable + uint32_t(idr.index) * 1u;

      if (sgn.test<kOpRegQ, kOpRegQ, kOpRegQ>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>())) {
          if (isDtMultiple(dtBits, makeDtBits(DT::kS32, DT::kU32, DT::kF32))) {
            opcode = opcodeTablePtr[0];
            goto Emit_Q0At12Of4Hi22_Q1At16Of4Hi7_Q2At0Of4Hi5_NoCond;
          }
        }
      }

      break;
    }

    case 53: {
      static const uint32_t opcodeTable[] = {
        0xF3B902C0u, // Instruction 'sha1h'.
        0xF3BA0380u, // Instruction 'sha1su1'.
        0xF3BA03C0u  // Instruction 'sha256su0'.
      };

      const uint32_t* opcodeTablePtr = opcodeTable + uint32_t(idr.index) * 1u;

      if (sgn.test<kOpRegQ, kOpRegQ>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>())) {
          if (isDtMultiple(dtBits, makeDtBits(DT::kS32, DT::kU32, DT::kF32))) {
            opcode = opcodeTablePtr[0];
            goto Emit_Q0At12Of4Hi22_Q1At0Of4Hi5_NoCond;
          }
        }
      }

      break;
    }

    case 54: {
      static const uint32_t opcodeTable[] = {
        0x00E00090u, // Instruction 'smlal'.
        0x01400080u, // Instruction 'smlalbb'.
        0x014000C0u, // Instruction 'smlalbt'.
        0x07400010u, // Instruction 'smlald'.
        0x07400030u, // Instruction 'smlaldx'.
        0x00F00090u, // Instruction 'smlals'.
        0x014000A0u, // Instruction 'smlaltb'.
        0x014000E0u, // Instruction 'smlaltt'.
        0x07400050u, // Instruction 'smlsld'.
        0x07400070u, // Instruction 'smlsldx'.
        0x00C00090u, // Instruction 'smull'.
        0x00D00090u, // Instruction 'smulls'.
        0x00400090u, // Instruction 'umaal'.
        0x00A00090u, // Instruction 'umlal'.
        0x00B00090u, // Instruction 'umlals'.
        0x00800090u, // Instruction 'umull'.
        0x00900090u  // Instruction 'umulls'.
      };

      const uint32_t* opcodeTablePtr = opcodeTable + uint32_t(idr.index) * 1u;

      if (sgn.test<kOpRegR, kOpRegR, kOpRegR, kOpRegR>()) {
        opcode = opcodeTablePtr[0];
        goto Emit_R0At12Of4_R1At16Of4_R2At0Of4_R3At8Of4_Cond;
      }

      break;
    }

    case 55: {
      static const uint32_t opcodeTable[] = {
        0xF84D0500u, // Instruction 'srsda'.
        0xF94D0500u, // Instruction 'srsdb'.
        0xF8CD0500u, // Instruction 'srsia'.
        0xF9CD0500u  // Instruction 'srsib'.
      };

      const uint32_t* opcodeTablePtr = opcodeTable + uint32_t(idr.index) * 1u;

      if (sgn.test<kOpMemB, kOpImmI>()) {
        mem = &o0.as<Mem>();

        if (mem->baseId() == 13u) {
          if (!mem->hasIndex()) {
            if (!mem->offsetLo32()) {
              if (!mem->isPostIndex()) {
                if (o1.as<Imm>().valueAs<uint64_t>() <= 0x1Fu) {
                  opcode = opcodeTablePtr[0];
                  opcode |= o1.as<Imm>().valueAs<uint32_t>() << 0u;
                  goto EmitW21_NoCond;
                }
              }
            }
          }
        }
      }

      break;
    }

    case 56: {
      // Instruction 'ssat'.
      if (sgn.test<kOpRegR, kOpImmI, kOpRegR>()) {
        SsatImmEncode enc0;
        if (enc0.init(o1.as<Imm>())) {
          opcode = 0x06A00010u;
          opcode |= enc0.imm() << 16u;
          goto Emit_R0At12Of4_R2At0Of4_Cond;
        }
      }

      if (sgn.test<kOpRegR, kOpImmI, kOpRegR, kOpImmI>()) {
        uint32_t shiftOp = o3.as<Imm>().predicate();
        if ((shiftOp == uint32_t(ShiftOp::kLSL) || shiftOp == uint32_t(ShiftOp::kASR))) {
          SsatImmEncode enc1;
          if (enc1.init(o1.as<Imm>(), o3.as<Imm>())) {
            opcode = 0x06A00010u;
            opcode |= shiftOp << 5u;
            opcode |= enc1.n() << 7u;
            opcode |= enc1.imm() << 16u;
            goto Emit_R0At12Of4_R2At0Of4_Cond;
          }
        }
      }

      break;
    }

    case 57: {
      // Instruction 'ssat16'.
      if (sgn.test<kOpRegR, kOpImmI, kOpRegR>()) {
        Ssat16ImmEncode enc0;
        if (enc0.init(o1.as<Imm>())) {
          opcode = 0x06A00F30u;
          opcode |= enc0.imm() << 16u;
          goto Emit_R0At12Of4_R2At0Of4_Cond;
        }
      }

      break;
    }

    case 58: {
      static const uint32_t opcodeTable[] = {
        0x0180FC90u, // Instruction 'stl'.
        0x01C0FC90u, // Instruction 'stlb'.
        0x01E0FC90u  // Instruction 'stlh'.
      };

      const uint32_t* opcodeTablePtr = opcodeTable + uint32_t(idr.index) * 1u;

      if (sgn.test<kOpRegR, kOpMemB>()) {
        mem = &o1.as<Mem>();

        if (mem->baseId() < 15u) {
          if (!mem->hasIndex()) {
            if (!mem->offsetLo32()) {
              if (mem->isFixedOffset()) {
                opcode = opcodeTablePtr[0];
                goto Emit_R0At0Of4_MemBaseAt16_Cond;
              }
            }
          }
        }
      }

      break;
    }

    case 59: {
      static const uint32_t opcodeTable[] = {
        0x01800E90u, // Instruction 'stlex'.
        0x01C00E90u, // Instruction 'stlexb'.
        0x01E00E90u, // Instruction 'stlexh'.
        0x01800F90u, // Instruction 'strex'.
        0x01C00F90u, // Instruction 'strexb'.
        0x01E00F90u  // Instruction 'strexh'.
      };

      const uint32_t* opcodeTablePtr = opcodeTable + uint32_t(idr.index) * 1u;

      if (sgn.test<kOpRegR, kOpRegR, kOpMemB>()) {
        mem = &o2.as<Mem>();

        if (mem->baseId() < 15u) {
          if (!mem->hasIndex()) {
            if (!mem->offsetLo32()) {
              if (mem->isFixedOffset()) {
                opcode = opcodeTablePtr[0];
                goto Emit_R0At12Of4_R1At0Of4_MemBaseAt16_Cond;
              }
            }
          }
        }
      }

      break;
    }

    case 60: {
      static const uint32_t opcodeTable[] = {
        0x01A00E90u, // Instruction 'stlexd'.
        0x01A00F90u  // Instruction 'strexd'.
      };

      const uint32_t* opcodeTablePtr = opcodeTable + uint32_t(idr.index) * 1u;

      if (sgn.test<kOpRegR, kOpRegR, kOpRegR, kOpMemB>()) {
        mem = &o3.as<Mem>();

        if (mem->baseId() < 15u) {
          if (!mem->hasIndex()) {
            if (!mem->offsetLo32()) {
              if (mem->isFixedOffset()) {
                if (isConsecutive(1, o1.as<Reg>(), o2.as<Reg>())) {
                  opcode = opcodeTablePtr[0];
                  goto Emit_R0At12Of4_R1At0Of4_MemBaseAt16_Cond;
                }
              }
            }
          }
        }
      }

      break;
    }

    case 61: {
      // Instruction 'strbt'.
      if (sgn.test<kOpRegR, kOpMemB>()) {
        mem = &o1.as<Mem>();

        if (mem->baseId() < 15u) {
          if (mem->indexType() == RegType::kGp32) {
            if (!mem->offsetLo32()) {
              if (mem->indexId() < 15u) {
                if (mem->isPostIndex()) {
                  opcode = 0x06600000u;
                  goto Emit_R0At12Of4_MemBaseAt16_MemSIndexAt0_SOPAt5_N5At7_Cond;
                }
              }
            }
          }

          if (!mem->hasIndex()) {
            if (checkSOffset(mem->offsetLo32(), 12, 0)) {
              if (mem->isPostIndex()) {
                opcode = 0x04600000u;
                goto Emit_R0At12Of4_MemBaseAt16_SOffAt0Of12_Cond;
              }
            }
          }
        }
      }

      if (sgn.test<kOpRegR, kOpMemAny>()) {
        mem = &o1.as<Mem>();

        if (isMemPCRel(*mem)) {
          if (mem->isFixedOffset()) {
            opcode = 0x04600000u;
            opcode |= 0xFu << 16;
            offsetFormat.resetToImmValue(OffsetType::kAArch32_U23_SignedOffset, 4, 0, 12, 0);
            goto Emit_R0At12Of4_MemPCRel_Cond;
          }
        }
      }

      break;
    }

    case 62: {
      // Instruction 'strd'.
      if (sgn.test<kOpRegR, kOpRegR, kOpMemB>()) {
        mem = &o2.as<Mem>();

        if (mem->baseId() < 15u) {
          if (mem->indexType() == RegType::kGp32) {
            if (!mem->offsetLo32()) {
              if (mem->indexId() < 15u) {
                if (isConsecutive(1, o0.as<Reg>(), o1.as<Reg>())) {
                  opcode = 0x000000F0u;
                  goto Emit_R0At12Of4_MemBaseAt16_MemSIndexAt0_P24W21_Cond;
                }
              }
            }
          }

          if (!mem->hasIndex()) {
            if (checkSOffset(mem->offsetLo32(), 8, 0)) {
              if (isConsecutive(1, o0.as<Reg>(), o1.as<Reg>())) {
                opcode = 0x004000F0u;
                goto Emit_R0At12Of4_MemBaseAt16_SOffAt0Of4_SOffAt8Of4_P24W21_Cond;
              }
            }
          }
        }
      }

      if (sgn.test<kOpRegR, kOpRegR, kOpMemAny>()) {
        mem = &o2.as<Mem>();

        if (isMemPCRel(*mem)) {
          if (mem->isFixedOffset()) {
            if (isConsecutive(1, o0.as<Reg>(), o1.as<Reg>())) {
              opcode = 0x014000F0u;
              opcode |= 0xFu << 16;
              offsetFormat.resetToImmValue(OffsetType::kAArch32_U23_0To3At0_4To7At8, 4, 0, 8, 0);
              goto Emit_R0At12Of4_MemPCRel_Cond;
            }
          }
        }
      }

      break;
    }

    case 63: {
      // Instruction 'svc'.
      if (sgn.test<kOpImmI>()) {
        if (o0.as<Imm>().valueAs<uint64_t>() <= 0xFFFFFFu) {
          opcode = 0x0F000000u;
          opcode |= o0.as<Imm>().valueAs<uint32_t>() << 0u;
          goto Emit_Cond;
        }
      }

      break;
    }

    case 64: {
      static const uint32_t opcodeTable[] = {
        0x06A00070u, 0x06A00070u, // Instruction 'sxtab'.
        0x06800070u, 0x06800070u, // Instruction 'sxtab16'.
        0x06B00070u, 0x06B00070u, // Instruction 'sxtah'.
        0x06E00070u, 0x06E00070u, // Instruction 'uxtab'.
        0x06C00070u, 0x06C00070u, // Instruction 'uxtab16'.
        0x06F00070u, 0x06F00070u  // Instruction 'uxtah'.
      };

      const uint32_t* opcodeTablePtr = opcodeTable + uint32_t(idr.index) * 2u;

      if (sgn.test<kOpRegR, kOpRegR, kOpRegR>()) {
        Ror8ImmEncode enc0;
        if (enc0.init(0u)) {
          opcode = opcodeTablePtr[0];
          opcode |= enc0.imm() << 10u;
          goto Emit_R0At12Of4_R1At16Of4_R2At0Of4_Cond;
        }
      }

      if (sgn.test<kOpRegR, kOpRegR, kOpRegR, kOpImmI>()) {
        if (o3.as<Imm>().predicate() == uint32_t(ShiftOp::kROR)) {
          Ror8ImmEncode enc1;
          if (enc1.init(o3.as<Imm>())) {
            opcode = opcodeTablePtr[1];
            opcode |= enc1.imm() << 10u;
            goto Emit_R0At12Of4_R1At16Of4_R2At0Of4_Cond;
          }
        }
      }

      break;
    }

    case 65: {
      static const uint32_t opcodeTable[] = {
        0x06AF0070u, 0x06AF0070u, // Instruction 'sxtb'.
        0x068F0070u, 0x068F0070u, // Instruction 'sxtb16'.
        0x06BF0070u, 0x06BF0070u, // Instruction 'sxth'.
        0x06EF0070u, 0x06EF0070u, // Instruction 'uxtb'.
        0x06CF0070u, 0x06CF0070u, // Instruction 'uxtb16'.
        0x06FF0070u, 0x06FF0070u  // Instruction 'uxth'.
      };

      const uint32_t* opcodeTablePtr = opcodeTable + uint32_t(idr.index) * 2u;

      if (sgn.test<kOpRegR, kOpRegR>()) {
        Ror8ImmEncode enc0;
        if (enc0.init(0u)) {
          opcode = opcodeTablePtr[0];
          opcode |= enc0.imm() << 10u;
          goto Emit_R0At12Of4_R1At0Of4_Cond;
        }
      }

      if (sgn.test<kOpRegR, kOpRegR, kOpImmI>()) {
        if (o2.as<Imm>().predicate() == uint32_t(ShiftOp::kROR)) {
          Ror8ImmEncode enc1;
          if (enc1.init(o2.as<Imm>())) {
            opcode = opcodeTablePtr[1];
            opcode |= enc1.imm() << 10u;
            goto Emit_R0At12Of4_R1At0Of4_Cond;
          }
        }
      }

      break;
    }

    case 66: {
      // Instruction 'usat'.
      if (sgn.test<kOpRegR, kOpImmI, kOpRegR>()) {
        if (o1.as<Imm>().valueAs<uint64_t>() <= 0x1Fu) {
          opcode = 0x06E00010u;
          opcode |= o1.as<Imm>().valueAs<uint32_t>() << 16u;
          goto Emit_R0At12Of4_R2At0Of4_Cond;
        }
      }

      if (sgn.test<kOpRegR, kOpImmI, kOpRegR, kOpImmI>()) {
        uint32_t shiftOp = o3.as<Imm>().predicate();
        uint64_t shiftImm = o3.as<Imm>().valueAs<uint64_t>();
        if (o1.as<Imm>().valueAs<uint64_t>() <= 0x1Fu && (shiftOp == uint32_t(ShiftOp::kLSL) || shiftOp == uint32_t(ShiftOp::kASR)) && shiftImm <= 31u) {
          opcode = 0x06E00010u;
          opcode |= shiftOp << 5u;
          opcode |= uint32_t(shiftImm) << 7u;
          opcode |= o1.as<Imm>().valueAs<uint32_t>() << 16u;
          goto Emit_R0At12Of4_R2At0Of4_Cond;
        }
      }

      break;
    }

    case 67: {
      // Instruction 'usat16'.
      if (sgn.test<kOpRegR, kOpImmI, kOpRegR>()) {
        if (o1.as<Imm>().valueAs<uint64_t>() <= 0xFu) {
          opcode = 0x06E00F30u;
          opcode |= o1.as<Imm>().valueAs<uint32_t>() << 16u;
          goto Emit_R0At12Of4_R2At0Of4_Cond;
        }
      }

      break;
    }

    case 68: {
      static const uint32_t opcodeTable[] = {
        0xF2000710u, 0xF2000750u, // Instruction 'vaba'.
        0xF2000000u, 0xF2000040u, // Instruction 'vhadd'.
        0xF2000200u, 0xF2000240u, // Instruction 'vhsub'.
        0xF2000100u, 0xF2000140u  // Instruction 'vrhadd'.
      };

      const uint32_t* opcodeTablePtr = opcodeTable + uint32_t(idr.index) * 2u;

      uint32_t sz = szFromDt(dtBits);
      if (sgn.test<kOpRegD, kOpRegD, kOpRegD>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>())) {
          if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kS32, DT::kS8, DT::kU16, DT::kU32, DT::kU8))) {
            opcode = opcodeTablePtr[0];
            opcode |= sz << 20u;
            opcode |= uBitFromDt(dtBits) << 24u;
            goto Emit_R0At12Of4Hi22_R1At16Of4Hi7_R2At0Of4Hi5_NoCond;
          }
        }
      }

      if (sgn.test<kOpRegQ, kOpRegQ, kOpRegQ>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>())) {
          if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kS32, DT::kS8, DT::kU16, DT::kU32, DT::kU8))) {
            opcode = opcodeTablePtr[1];
            opcode |= sz << 20u;
            opcode |= uBitFromDt(dtBits) << 24u;
            goto Emit_Q0At12Of4Hi22_Q1At16Of4Hi7_Q2At0Of4Hi5_NoCond;
          }
        }
      }

      break;
    }

    case 69: {
      static const uint32_t opcodeTable[] = {
        0xF2800500u, // Instruction 'vabal'.
        0xF2800700u, // Instruction 'vabdl'.
        0xF2800000u, // Instruction 'vaddl'.
        0xF2800200u  // Instruction 'vsubl'.
      };

      const uint32_t* opcodeTablePtr = opcodeTable + uint32_t(idr.index) * 1u;

      uint32_t sz = szFromDt(dtBits);
      if (sgn.test<kOpRegQ, kOpRegD, kOpRegD>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>())) {
          if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kS32, DT::kS8, DT::kU16, DT::kU32, DT::kU8))) {
            opcode = opcodeTablePtr[0];
            opcode |= sz << 20u;
            opcode |= uBitFromDt(dtBits) << 24u;
            goto Emit_Q0At12Of4Hi22_R1At16Of4Hi7_R2At0Of4Hi5_NoCond;
          }
        }
      }

      break;
    }

    case 70: {
      static const uint32_t opcodeTable[] = {
        0xF3300D00u, 0xF3200D00u, 0xF2000700u, 0xF3300D40u, 0xF3200D40u, 0xF2000740u, // Instruction 'vabd'.
        0xF2100F00u, 0xF2000F00u, 0xF2000600u, 0xF2100F40u, 0xF2000F40u, 0xF2000640u, // Instruction 'vmax'.
        0xF2300F00u, 0xF2200F00u, 0xF2000610u, 0xF2300F40u, 0xF2200F40u, 0xF2000650u  // Instruction 'vmin'.
      };

      const uint32_t* opcodeTablePtr = opcodeTable + uint32_t(idr.index) * 6u;

      uint32_t sz = szFromDt(dtBits);
      if (sgn.test<kOpRegD, kOpRegD, kOpRegD>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>())) {
          if (isDtSingle(dtBits, DT::kF16)) {
            opcode = opcodeTablePtr[0];
            goto Emit_R0At12Of4Hi22_R1At16Of4Hi7_R2At0Of4Hi5_NoCond;
          }

          if (isDtSingle(dtBits, DT::kF32)) {
            opcode = opcodeTablePtr[1];
            goto Emit_R0At12Of4Hi22_R1At16Of4Hi7_R2At0Of4Hi5_NoCond;
          }

          if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kS32, DT::kS8, DT::kU16, DT::kU32, DT::kU8))) {
            opcode = opcodeTablePtr[2];
            opcode |= sz << 20u;
            opcode |= uBitFromDt(dtBits) << 24u;
            goto Emit_R0At12Of4Hi22_R1At16Of4Hi7_R2At0Of4Hi5_NoCond;
          }
        }
      }

      if (sgn.test<kOpRegQ, kOpRegQ, kOpRegQ>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>())) {
          if (isDtSingle(dtBits, DT::kF16)) {
            opcode = opcodeTablePtr[3];
            goto Emit_Q0At12Of4Hi22_Q1At16Of4Hi7_Q2At0Of4Hi5_NoCond;
          }

          if (isDtSingle(dtBits, DT::kF32)) {
            opcode = opcodeTablePtr[4];
            goto Emit_Q0At12Of4Hi22_Q1At16Of4Hi7_Q2At0Of4Hi5_NoCond;
          }

          if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kS32, DT::kS8, DT::kU16, DT::kU32, DT::kU8))) {
            opcode = opcodeTablePtr[5];
            opcode |= sz << 20u;
            opcode |= uBitFromDt(dtBits) << 24u;
            goto Emit_Q0At12Of4Hi22_Q1At16Of4Hi7_Q2At0Of4Hi5_NoCond;
          }
        }
      }

      break;
    }

    case 71: {
      static const uint32_t opcodeTable[] = {
        0x0EB00AC0u, 0xEEB009C0u, 0x0EB00BC0u, 0xF3B50700u, 0xF3B90700u, 0xF3B10300u, 0xF3B50740u, 0xF3B90740u, 0xF3B10340u, // Instruction 'vabs'.
        0x0EB10A40u, 0xEEB10940u, 0x0EB10B40u, 0xF3B50780u, 0xF3B90780u, 0xF3B10380u, 0xF3B507C0u, 0xF3B907C0u, 0xF3B103C0u  // Instruction 'vneg'.
      };

      const uint32_t* opcodeTablePtr = opcodeTable + uint32_t(idr.index) * 9u;

      uint32_t sz = szFromDt(dtBits);
      if (sgn.test<kOpRegS, kOpRegS>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>())) {
          if (isDtSingle(dtBits, DT::kF32)) {
            opcode = opcodeTablePtr[0];
            goto Emit_R0At12Of4Lo22_R1At0Of4Lo5_Cond;
          }

          if (isDtSingle(dtBits, DT::kF16)) {
            opcode = opcodeTablePtr[1];
            goto Emit_R0At12Of4Lo22_R1At0Of4Lo5_NoCond;
          }
        }
      }

      if (sgn.test<kOpRegD, kOpRegD>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>())) {
          if (isDtSingle(dtBits, DT::kF64)) {
            opcode = opcodeTablePtr[2];
            goto Emit_R0At12Of4Hi22_R1At0Of4Hi5_Cond;
          }

          if (isDtSingle(dtBits, DT::kF16)) {
            opcode = opcodeTablePtr[3];
            goto Emit_R0At12Of4Hi22_R1At0Of4Hi5_NoCond;
          }

          if (isDtSingle(dtBits, DT::kF32)) {
            opcode = opcodeTablePtr[4];
            goto Emit_R0At12Of4Hi22_R1At0Of4Hi5_NoCond;
          }

          if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kS32, DT::kS8))) {
            opcode = opcodeTablePtr[5];
            opcode |= sz << 18u;
            goto Emit_R0At12Of4Hi22_R1At0Of4Hi5_NoCond;
          }
        }
      }

      if (sgn.test<kOpRegQ, kOpRegQ>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>())) {
          if (isDtSingle(dtBits, DT::kF16)) {
            opcode = opcodeTablePtr[6];
            goto Emit_Q0At12Of4Hi22_Q1At0Of4Hi5_NoCond;
          }

          if (isDtSingle(dtBits, DT::kF32)) {
            opcode = opcodeTablePtr[7];
            goto Emit_Q0At12Of4Hi22_Q1At0Of4Hi5_NoCond;
          }

          if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kS32, DT::kS8))) {
            opcode = opcodeTablePtr[8];
            opcode |= sz << 18u;
            goto Emit_Q0At12Of4Hi22_Q1At0Of4Hi5_NoCond;
          }
        }
      }

      break;
    }

    case 72: {
      static const uint32_t opcodeTable[] = {
        0xF3100E10u, 0xF3000E10u, 0xF3100E50u, 0xF3000E50u, // Instruction 'vacge'.
        0xF3300E10u, 0xF3200E10u, 0xF3300E50u, 0xF3200E50u, // Instruction 'vacgt'.
        0xF2100F10u, 0xF2000F10u, 0xF2100F50u, 0xF2000F50u, // Instruction 'vrecps'.
        0xF2300F10u, 0xF2200F10u, 0xF2300F50u, 0xF2200F50u  // Instruction 'vrsqrts'.
      };

      const uint32_t* opcodeTablePtr = opcodeTable + uint32_t(idr.index) * 4u;

      if (sgn.test<kOpRegD, kOpRegD, kOpRegD>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>())) {
          if (isDtSingle(dtBits, DT::kF16)) {
            opcode = opcodeTablePtr[0];
            goto Emit_R0At12Of4Hi22_R1At16Of4Hi7_R2At0Of4Hi5_NoCond;
          }

          if (isDtSingle(dtBits, DT::kF32)) {
            opcode = opcodeTablePtr[1];
            goto Emit_R0At12Of4Hi22_R1At16Of4Hi7_R2At0Of4Hi5_NoCond;
          }
        }
      }

      if (sgn.test<kOpRegQ, kOpRegQ, kOpRegQ>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>())) {
          if (isDtSingle(dtBits, DT::kF16)) {
            opcode = opcodeTablePtr[2];
            goto Emit_Q0At12Of4Hi22_Q1At16Of4Hi7_Q2At0Of4Hi5_NoCond;
          }

          if (isDtSingle(dtBits, DT::kF32)) {
            opcode = opcodeTablePtr[3];
            goto Emit_Q0At12Of4Hi22_Q1At16Of4Hi7_Q2At0Of4Hi5_NoCond;
          }
        }
      }

      break;
    }

    case 73: {
      static const uint32_t opcodeTable[] = {
        0xF3100E10u, 0xF3000E10u, 0xF3100E50u, 0xF3000E50u, // Instruction 'vacle'.
        0xF3300E10u, 0xF3200E10u, 0xF3300E50u, 0xF3200E50u  // Instruction 'vaclt'.
      };

      const uint32_t* opcodeTablePtr = opcodeTable + uint32_t(idr.index) * 4u;

      if (sgn.test<kOpRegD, kOpRegD, kOpRegD>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>())) {
          if (isDtSingle(dtBits, DT::kF16)) {
            opcode = opcodeTablePtr[0];
            goto Emit_R0At12Of4Hi22_R1At0Of4Hi5_R2At16Of4Hi7_NoCond;
          }

          if (isDtSingle(dtBits, DT::kF32)) {
            opcode = opcodeTablePtr[1];
            goto Emit_R0At12Of4Hi22_R1At0Of4Hi5_R2At16Of4Hi7_NoCond;
          }
        }
      }

      if (sgn.test<kOpRegQ, kOpRegQ, kOpRegQ>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>())) {
          if (isDtSingle(dtBits, DT::kF16)) {
            opcode = opcodeTablePtr[2];
            goto Emit_Q0At12Of4Hi22_Q1At0Of4Hi5_Q2At16Of4Hi7_NoCond;
          }

          if (isDtSingle(dtBits, DT::kF32)) {
            opcode = opcodeTablePtr[3];
            goto Emit_Q0At12Of4Hi22_Q1At0Of4Hi5_Q2At16Of4Hi7_NoCond;
          }
        }
      }

      break;
    }

    case 74: {
      static const uint32_t opcodeTable[] = {
        0x0E300A00u, 0xEE300900u, 0x0E300B00u, 0xF2100D00u, 0xF2000D00u, 0xF2000800u, 0xF2100D40u, 0xF2000D40u, 0xF2000840u, // Instruction 'vadd'.
        0x0E300A40u, 0xEE300940u, 0x0E300B40u, 0xF2300D00u, 0xF2200D00u, 0xF3000800u, 0xF2300D40u, 0xF2200D40u, 0xF3000840u  // Instruction 'vsub'.
      };

      const uint32_t* opcodeTablePtr = opcodeTable + uint32_t(idr.index) * 9u;

      uint32_t sz = szFromDt(dtBits);
      if (sgn.test<kOpRegS, kOpRegS, kOpRegS>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>())) {
          if (isDtSingle(dtBits, DT::kF32)) {
            opcode = opcodeTablePtr[0];
            goto Emit_R0At12Of4Lo22_R1At16Of4Lo7_R2At0Of4Lo5_Cond;
          }

          if (isDtSingle(dtBits, DT::kF16)) {
            opcode = opcodeTablePtr[1];
            goto Emit_R0At12Of4Lo22_R1At16Of4Lo7_R2At0Of4Lo5_NoCond;
          }
        }
      }

      if (sgn.test<kOpRegD, kOpRegD, kOpRegD>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>())) {
          if (isDtSingle(dtBits, DT::kF64)) {
            opcode = opcodeTablePtr[2];
            goto Emit_R0At12Of4Hi22_R1At16Of4Hi7_R2At0Of4Hi5_Cond;
          }

          if (isDtSingle(dtBits, DT::kF16)) {
            opcode = opcodeTablePtr[3];
            goto Emit_R0At12Of4Hi22_R1At16Of4Hi7_R2At0Of4Hi5_NoCond;
          }

          if (isDtSingle(dtBits, DT::kF32)) {
            opcode = opcodeTablePtr[4];
            goto Emit_R0At12Of4Hi22_R1At16Of4Hi7_R2At0Of4Hi5_NoCond;
          }

          if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kS32, DT::kS64, DT::kS8, DT::kU16, DT::kU32, DT::kU64, DT::kU8))) {
            opcode = opcodeTablePtr[5];
            opcode |= sz << 20u;
            goto Emit_R0At12Of4Hi22_R1At16Of4Hi7_R2At0Of4Hi5_NoCond;
          }
        }
      }

      if (sgn.test<kOpRegQ, kOpRegQ, kOpRegQ>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>())) {
          if (isDtSingle(dtBits, DT::kF16)) {
            opcode = opcodeTablePtr[6];
            goto Emit_Q0At12Of4Hi22_Q1At16Of4Hi7_Q2At0Of4Hi5_NoCond;
          }

          if (isDtSingle(dtBits, DT::kF32)) {
            opcode = opcodeTablePtr[7];
            goto Emit_Q0At12Of4Hi22_Q1At16Of4Hi7_Q2At0Of4Hi5_NoCond;
          }

          if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kS32, DT::kS64, DT::kS8, DT::kU16, DT::kU32, DT::kU64, DT::kU8))) {
            opcode = opcodeTablePtr[8];
            opcode |= sz << 20u;
            goto Emit_Q0At12Of4Hi22_Q1At16Of4Hi7_Q2At0Of4Hi5_NoCond;
          }
        }
      }

      break;
    }

    case 75: {
      static const uint32_t opcodeTable[] = {
        0xF2800400u, // Instruction 'vaddhn'.
        0xF3800400u, // Instruction 'vraddhn'.
        0xF3800600u, // Instruction 'vrsubhn'.
        0xF2800600u  // Instruction 'vsubhn'.
      };

      const uint32_t* opcodeTablePtr = opcodeTable + uint32_t(idr.index) * 1u;

      uint32_t sz = szFromDt(dtBits);
      if (sgn.test<kOpRegD, kOpRegQ, kOpRegQ>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>())) {
          if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kS32, DT::kS64, DT::kU16, DT::kU32, DT::kU64))) {
            opcode = opcodeTablePtr[0];
            opcode |= (sz - 1) << 20u;
            goto Emit_R0At12Of4Hi22_Q1At16Of4Hi7_Q2At0Of4Hi5_NoCond;
          }
        }
      }

      break;
    }

    case 76: {
      static const uint32_t opcodeTable[] = {
        0xF2800100u, // Instruction 'vaddw'.
        0xF2800300u  // Instruction 'vsubw'.
      };

      const uint32_t* opcodeTablePtr = opcodeTable + uint32_t(idr.index) * 1u;

      uint32_t sz = szFromDt(dtBits);
      if (sgn.test<kOpRegQ, kOpRegQ, kOpRegD>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>())) {
          if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kS32, DT::kS8, DT::kU16, DT::kU32, DT::kU8))) {
            opcode = opcodeTablePtr[0];
            opcode |= sz << 20u;
            opcode |= uBitFromDt(dtBits) << 24u;
            goto Emit_Q0At12Of4Hi22_Q1At16Of4Hi7_R2At0Of4Hi5_NoCond;
          }
        }
      }

      break;
    }

    case 77: {
      // Instruction 'vand'.
      uint32_t sz = szFromDt(dtBits);
      if (sgn.test<kOpRegD, kOpRegD, kOpRegD>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>())) {
          opcode = 0xF2000110u;
          goto Emit_R0At12Of4Hi22_R1At16Of4Hi7_R2At0Of4Hi5_NoCond;
        }
      }

      if (sgn.test<kOpRegQ, kOpRegQ, kOpRegQ>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>())) {
          opcode = 0xF2000150u;
          goto Emit_Q0At12Of4Hi22_Q1At16Of4Hi7_Q2At0Of4Hi5_NoCond;
        }
      }

      if (sgn.test<kOpRegD, kOpImmI>()) {
        if (isPureVec(o0.as<Vec>())) {
          if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kS32, DT::kS64, DT::kU16, DT::kU32, DT::kU64))) {
            VecBicOrrImmEncode enc0;
            if (enc0.init(sz, 1u, o1.as<Imm>())) {
              opcode = 0xF2800030u;
              opcode |= (enc0.imm() & 0xFu) << 0u;
              opcode |= (enc0.imm() & 0x70u) << 12u;
              opcode |= (enc0.imm() & 0x80u) << 17u;
              opcode |= enc0.cmode() << 8u;
              goto Emit_R0At12Of4Hi22_NoCond;
            }
          }
        }
      }

      if (sgn.test<kOpRegQ, kOpImmI>()) {
        if (isPureVec(o0.as<Vec>())) {
          if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kS32, DT::kS64, DT::kU16, DT::kU32, DT::kU64))) {
            VecBicOrrImmEncode enc1;
            if (enc1.init(sz, 1u, o1.as<Imm>())) {
              opcode = 0xF2800070u;
              opcode |= (enc1.imm() & 0xFu) << 0u;
              opcode |= (enc1.imm() & 0x70u) << 12u;
              opcode |= (enc1.imm() & 0x80u) << 17u;
              opcode |= enc1.cmode() << 8u;
              goto Emit_Q0At12Of4Hi22_NoCond;
            }
          }
        }
      }

      break;
    }

    case 78: {
      // Instruction 'vbic'.
      uint32_t sz = szFromDt(dtBits);
      if (sgn.test<kOpRegD, kOpRegD, kOpRegD>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>())) {
          opcode = 0xF2100110u;
          goto Emit_R0At12Of4Hi22_R1At16Of4Hi7_R2At0Of4Hi5_NoCond;
        }
      }

      if (sgn.test<kOpRegQ, kOpRegQ, kOpRegQ>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>())) {
          opcode = 0xF2100150u;
          goto Emit_Q0At12Of4Hi22_Q1At16Of4Hi7_Q2At0Of4Hi5_NoCond;
        }
      }

      if (sgn.test<kOpRegD, kOpImmI>()) {
        if (isPureVec(o0.as<Vec>())) {
          if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kS32, DT::kS64, DT::kU16, DT::kU32, DT::kU64))) {
            VecBicOrrImmEncode enc0;
            if (enc0.init(sz, 0u, o1.as<Imm>())) {
              opcode = 0xF2800030u;
              opcode |= (enc0.imm() & 0xFu) << 0u;
              opcode |= (enc0.imm() & 0x70u) << 12u;
              opcode |= (enc0.imm() & 0x80u) << 17u;
              opcode |= enc0.cmode() << 8u;
              goto Emit_R0At12Of4Hi22_NoCond;
            }
          }
        }
      }

      if (sgn.test<kOpRegQ, kOpImmI>()) {
        if (isPureVec(o0.as<Vec>())) {
          if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kS32, DT::kS64, DT::kU16, DT::kU32, DT::kU64))) {
            VecBicOrrImmEncode enc1;
            if (enc1.init(sz, 0u, o1.as<Imm>())) {
              opcode = 0xF2800070u;
              opcode |= (enc1.imm() & 0xFu) << 0u;
              opcode |= (enc1.imm() & 0x70u) << 12u;
              opcode |= (enc1.imm() & 0x80u) << 17u;
              opcode |= enc1.cmode() << 8u;
              goto Emit_Q0At12Of4Hi22_NoCond;
            }
          }
        }
      }

      break;
    }

    case 79: {
      static const uint32_t opcodeTable[] = {
        0xF3300110u, 0xF3300150u, // Instruction 'vbif'.
        0xF3200110u, 0xF3200150u, // Instruction 'vbit'.
        0xF3100110u, 0xF3100150u, // Instruction 'vbsl'.
        0xF3000110u, 0xF3000150u  // Instruction 'veor'.
      };

      const uint32_t* opcodeTablePtr = opcodeTable + uint32_t(idr.index) * 2u;

      if (sgn.test<kOpRegD, kOpRegD, kOpRegD>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>())) {
          opcode = opcodeTablePtr[0];
          goto Emit_R0At12Of4Hi22_R1At16Of4Hi7_R2At0Of4Hi5_NoCond;
        }
      }

      if (sgn.test<kOpRegQ, kOpRegQ, kOpRegQ>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>())) {
          opcode = opcodeTablePtr[1];
          goto Emit_Q0At12Of4Hi22_Q1At16Of4Hi7_Q2At0Of4Hi5_NoCond;
        }
      }

      break;
    }

    case 80: {
      // Instruction 'vcadd'.
      if (sgn.test<kOpRegD, kOpRegD, kOpRegD, kOpImmI>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>())) {
          if (isDtSingle(dtBits, DT::kF32)) {
            VecRot1ImmEncode enc0;
            if (enc0.init(o3.as<Imm>())) {
              opcode = 0xFC900800u;
              opcode |= enc0.imm() << 24u;
              goto Emit_R0At12Of4Hi22_R1At16Of4Hi7_R2At0Of4Hi5_NoCond;
            }
          }

          if (isDtSingle(dtBits, DT::kF16)) {
            VecRot1ImmEncode enc2;
            if (enc2.init(o3.as<Imm>())) {
              opcode = 0xFC800800u;
              opcode |= enc2.imm() << 24u;
              goto Emit_R0At12Of4Hi22_R1At16Of4Hi7_R2At0Of4Hi5_NoCond;
            }
          }
        }
      }

      if (sgn.test<kOpRegQ, kOpRegQ, kOpRegQ, kOpImmI>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>())) {
          if (isDtSingle(dtBits, DT::kF32)) {
            VecRot1ImmEncode enc1;
            if (enc1.init(o3.as<Imm>())) {
              opcode = 0xFC900840u;
              opcode |= enc1.imm() << 24u;
              goto Emit_Q0At12Of4Hi22_Q1At16Of4Hi7_Q2At0Of4Hi5_NoCond;
            }
          }

          if (isDtSingle(dtBits, DT::kF16)) {
            VecRot1ImmEncode enc3;
            if (enc3.init(o3.as<Imm>())) {
              opcode = 0xFC800840u;
              opcode |= enc3.imm() << 24u;
              goto Emit_Q0At12Of4Hi22_Q1At16Of4Hi7_Q2At0Of4Hi5_NoCond;
            }
          }
        }
      }

      break;
    }

    case 81: {
      // Instruction 'vceq'.
      uint32_t sz = szFromDt(dtBits);
      if (sgn.test<kOpRegD, kOpRegD, kOpRegD>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>())) {
          if (isDtSingle(dtBits, DT::kF16)) {
            opcode = 0xF2100E00u;
            goto Emit_R0At12Of4Hi22_R1At16Of4Hi7_R2At0Of4Hi5_NoCond;
          }

          if (isDtSingle(dtBits, DT::kF32)) {
            opcode = 0xF2000E00u;
            goto Emit_R0At12Of4Hi22_R1At16Of4Hi7_R2At0Of4Hi5_NoCond;
          }

          if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kS32, DT::kS8, DT::kU16, DT::kU32, DT::kU8))) {
            opcode = 0xF3000810u;
            opcode |= sz << 20u;
            goto Emit_R0At12Of4Hi22_R1At16Of4Hi7_R2At0Of4Hi5_NoCond;
          }
        }
      }

      if (sgn.test<kOpRegQ, kOpRegQ, kOpRegQ>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>())) {
          if (isDtSingle(dtBits, DT::kF16)) {
            opcode = 0xF2100E40u;
            goto Emit_Q0At12Of4Hi22_Q1At16Of4Hi7_Q2At0Of4Hi5_NoCond;
          }

          if (isDtSingle(dtBits, DT::kF32)) {
            opcode = 0xF2000E40u;
            goto Emit_Q0At12Of4Hi22_Q1At16Of4Hi7_Q2At0Of4Hi5_NoCond;
          }

          if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kS32, DT::kS8, DT::kU16, DT::kU32, DT::kU8))) {
            opcode = 0xF3000850u;
            opcode |= sz << 20u;
            goto Emit_Q0At12Of4Hi22_Q1At16Of4Hi7_Q2At0Of4Hi5_NoCond;
          }
        }
      }

      if (sgn.test<kOpRegD, kOpRegD, kOpImmI>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>())) {
          if (isDtSingle(dtBits, DT::kF16)) {
            if (o2.as<Imm>().value() == 0u) {
              opcode = 0xF3B50500u;
              goto Emit_R0At12Of4Hi22_R1At0Of4Hi5_NoCond;
            }
          }

          if (isDtSingle(dtBits, DT::kF32)) {
            if (o2.as<Imm>().value() == 0u) {
              opcode = 0xF3B90500u;
              goto Emit_R0At12Of4Hi22_R1At0Of4Hi5_NoCond;
            }
          }

          if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kS32, DT::kS8, DT::kU16, DT::kU32, DT::kU8))) {
            if (o2.as<Imm>().value() == 0u) {
              opcode = 0xF3B10100u;
              opcode |= sz << 18u;
              goto Emit_R0At12Of4Hi22_R1At0Of4Hi5_NoCond;
            }
          }
        }
      }

      if (sgn.test<kOpRegQ, kOpRegQ, kOpImmI>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>())) {
          if (isDtSingle(dtBits, DT::kF16)) {
            if (o2.as<Imm>().value() == 0u) {
              opcode = 0xF3B50540u;
              goto Emit_Q0At12Of4Hi22_Q1At0Of4Hi5_NoCond;
            }
          }

          if (isDtSingle(dtBits, DT::kF32)) {
            if (o2.as<Imm>().value() == 0u) {
              opcode = 0xF3B90540u;
              goto Emit_Q0At12Of4Hi22_Q1At0Of4Hi5_NoCond;
            }
          }

          if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kS32, DT::kS8, DT::kU16, DT::kU32, DT::kU8))) {
            if (o2.as<Imm>().value() == 0u) {
              opcode = 0xF3B10140u;
              opcode |= sz << 18u;
              goto Emit_Q0At12Of4Hi22_Q1At0Of4Hi5_NoCond;
            }
          }
        }
      }

      break;
    }

    case 82: {
      static const uint32_t opcodeTable[] = {
        0xF3100E00u, 0xF3000E00u, 0xF2000310u, 0xF3100E40u, 0xF3000E40u, 0xF2000350u, 0xF3B50480u, 0xF3B90480u, 0xF3B10080u, 0xF3B504C0u, 0xF3B904C0u, 0xF3B100C0u, // Instruction 'vcge'.
        0xF3300E00u, 0xF3200E00u, 0xF2000300u, 0xF3300E40u, 0xF3200E40u, 0xF2000340u, 0xF3B50400u, 0xF3B90400u, 0xF3B10000u, 0xF3B50440u, 0xF3B90440u, 0xF3B10040u  // Instruction 'vcgt'.
      };

      const uint32_t* opcodeTablePtr = opcodeTable + uint32_t(idr.index) * 12u;

      uint32_t sz = szFromDt(dtBits);
      if (sgn.test<kOpRegD, kOpRegD, kOpRegD>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>())) {
          if (isDtSingle(dtBits, DT::kF16)) {
            opcode = opcodeTablePtr[0];
            goto Emit_R0At12Of4Hi22_R1At16Of4Hi7_R2At0Of4Hi5_NoCond;
          }

          if (isDtSingle(dtBits, DT::kF32)) {
            opcode = opcodeTablePtr[1];
            goto Emit_R0At12Of4Hi22_R1At16Of4Hi7_R2At0Of4Hi5_NoCond;
          }

          if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kS32, DT::kS8, DT::kU16, DT::kU32, DT::kU8))) {
            opcode = opcodeTablePtr[2];
            opcode |= sz << 20u;
            opcode |= uBitFromDt(dtBits) << 24u;
            goto Emit_R0At12Of4Hi22_R1At16Of4Hi7_R2At0Of4Hi5_NoCond;
          }
        }
      }

      if (sgn.test<kOpRegQ, kOpRegQ, kOpRegQ>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>())) {
          if (isDtSingle(dtBits, DT::kF16)) {
            opcode = opcodeTablePtr[3];
            goto Emit_Q0At12Of4Hi22_Q1At16Of4Hi7_Q2At0Of4Hi5_NoCond;
          }

          if (isDtSingle(dtBits, DT::kF32)) {
            opcode = opcodeTablePtr[4];
            goto Emit_Q0At12Of4Hi22_Q1At16Of4Hi7_Q2At0Of4Hi5_NoCond;
          }

          if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kS32, DT::kS8, DT::kU16, DT::kU32, DT::kU8))) {
            opcode = opcodeTablePtr[5];
            opcode |= sz << 20u;
            opcode |= uBitFromDt(dtBits) << 24u;
            goto Emit_Q0At12Of4Hi22_Q1At16Of4Hi7_Q2At0Of4Hi5_NoCond;
          }
        }
      }

      if (sgn.test<kOpRegD, kOpRegD, kOpImmI>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>())) {
          if (isDtSingle(dtBits, DT::kF16)) {
            if (o2.as<Imm>().value() == 0u) {
              opcode = opcodeTablePtr[6];
              goto Emit_R0At12Of4Hi22_R1At0Of4Hi5_NoCond;
            }
          }

          if (isDtSingle(dtBits, DT::kF32)) {
            if (o2.as<Imm>().value() == 0u) {
              opcode = opcodeTablePtr[7];
              goto Emit_R0At12Of4Hi22_R1At0Of4Hi5_NoCond;
            }
          }

          if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kS32, DT::kS8))) {
            if (o2.as<Imm>().value() == 0u) {
              opcode = opcodeTablePtr[8];
              opcode |= sz << 18u;
              goto Emit_R0At12Of4Hi22_R1At0Of4Hi5_NoCond;
            }
          }
        }
      }

      if (sgn.test<kOpRegQ, kOpRegQ, kOpImmI>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>())) {
          if (isDtSingle(dtBits, DT::kF16)) {
            if (o2.as<Imm>().value() == 0u) {
              opcode = opcodeTablePtr[9];
              goto Emit_Q0At12Of4Hi22_Q1At0Of4Hi5_NoCond;
            }
          }

          if (isDtSingle(dtBits, DT::kF32)) {
            if (o2.as<Imm>().value() == 0u) {
              opcode = opcodeTablePtr[10];
              goto Emit_Q0At12Of4Hi22_Q1At0Of4Hi5_NoCond;
            }
          }

          if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kS32, DT::kS8))) {
            if (o2.as<Imm>().value() == 0u) {
              opcode = opcodeTablePtr[11];
              opcode |= sz << 18u;
              goto Emit_Q0At12Of4Hi22_Q1At0Of4Hi5_NoCond;
            }
          }
        }
      }

      break;
    }

    case 83: {
      static const uint32_t opcodeTable[] = {
        0xF3100E00u, 0xF3000E00u, 0xF2000310u, 0xF3100E40u, 0xF3000E40u, 0xF2000350u, 0xF3B50580u, 0xF3B90580u, 0xF3B10180u, 0xF3B505C0u, 0xF3B905C0u, 0xF3B101C0u, // Instruction 'vcle'.
        0xF3300E00u, 0xF3200E00u, 0xF2000300u, 0xF3300E40u, 0xF3200E40u, 0xF2000340u, 0xF3B50600u, 0xF3B90600u, 0xF3B10200u, 0xF3B50640u, 0xF3B90640u, 0xF3B10240u  // Instruction 'vclt'.
      };

      const uint32_t* opcodeTablePtr = opcodeTable + uint32_t(idr.index) * 12u;

      uint32_t sz = szFromDt(dtBits);
      if (sgn.test<kOpRegD, kOpRegD, kOpRegD>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>())) {
          if (isDtSingle(dtBits, DT::kF16)) {
            opcode = opcodeTablePtr[0];
            goto Emit_R0At12Of4Hi22_R1At0Of4Hi5_R2At16Of4Hi7_NoCond;
          }

          if (isDtSingle(dtBits, DT::kF32)) {
            opcode = opcodeTablePtr[1];
            goto Emit_R0At12Of4Hi22_R1At0Of4Hi5_R2At16Of4Hi7_NoCond;
          }

          if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kS32, DT::kS8, DT::kU16, DT::kU32, DT::kU8))) {
            opcode = opcodeTablePtr[2];
            opcode |= sz << 20u;
            opcode |= uBitFromDt(dtBits) << 24u;
            goto Emit_R0At12Of4Hi22_R1At0Of4Hi5_R2At16Of4Hi7_NoCond;
          }
        }
      }

      if (sgn.test<kOpRegQ, kOpRegQ, kOpRegQ>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>())) {
          if (isDtSingle(dtBits, DT::kF16)) {
            opcode = opcodeTablePtr[3];
            goto Emit_Q0At12Of4Hi22_Q1At0Of4Hi5_Q2At16Of4Hi7_NoCond;
          }

          if (isDtSingle(dtBits, DT::kF32)) {
            opcode = opcodeTablePtr[4];
            goto Emit_Q0At12Of4Hi22_Q1At0Of4Hi5_Q2At16Of4Hi7_NoCond;
          }

          if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kS32, DT::kS8, DT::kU16, DT::kU32, DT::kU8))) {
            opcode = opcodeTablePtr[5];
            opcode |= sz << 20u;
            opcode |= uBitFromDt(dtBits) << 24u;
            goto Emit_Q0At12Of4Hi22_Q1At0Of4Hi5_Q2At16Of4Hi7_NoCond;
          }
        }
      }

      if (sgn.test<kOpRegD, kOpRegD, kOpImmI>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>())) {
          if (isDtSingle(dtBits, DT::kF16)) {
            if (o2.as<Imm>().value() == 0u) {
              opcode = opcodeTablePtr[6];
              goto Emit_R0At12Of4Hi22_R1At0Of4Hi5_NoCond;
            }
          }

          if (isDtSingle(dtBits, DT::kF32)) {
            if (o2.as<Imm>().value() == 0u) {
              opcode = opcodeTablePtr[7];
              goto Emit_R0At12Of4Hi22_R1At0Of4Hi5_NoCond;
            }
          }

          if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kS32, DT::kS8))) {
            if (o2.as<Imm>().value() == 0u) {
              opcode = opcodeTablePtr[8];
              opcode |= sz << 18u;
              goto Emit_R0At12Of4Hi22_R1At0Of4Hi5_NoCond;
            }
          }
        }
      }

      if (sgn.test<kOpRegQ, kOpRegQ, kOpImmI>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>())) {
          if (isDtSingle(dtBits, DT::kF16)) {
            if (o2.as<Imm>().value() == 0u) {
              opcode = opcodeTablePtr[9];
              goto Emit_Q0At12Of4Hi22_Q1At0Of4Hi5_NoCond;
            }
          }

          if (isDtSingle(dtBits, DT::kF32)) {
            if (o2.as<Imm>().value() == 0u) {
              opcode = opcodeTablePtr[10];
              goto Emit_Q0At12Of4Hi22_Q1At0Of4Hi5_NoCond;
            }
          }

          if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kS32, DT::kS8))) {
            if (o2.as<Imm>().value() == 0u) {
              opcode = opcodeTablePtr[11];
              opcode |= sz << 18u;
              goto Emit_Q0At12Of4Hi22_Q1At0Of4Hi5_NoCond;
            }
          }
        }
      }

      break;
    }

    case 84: {
      static const uint32_t opcodeTable[] = {
        0xF3B00400u, 0xF3B00440u, // Instruction 'vcls'.
        0xF3B00480u, 0xF3B004C0u, // Instruction 'vclz'.
        0xF3B00000u, 0xF3B00040u, // Instruction 'vrev64'.
        0xF3B20080u, 0xF3B200C0u  // Instruction 'vtrn'.
      };

      const uint32_t* opcodeTablePtr = opcodeTable + uint32_t(idr.index) * 2u;

      uint32_t sz = szFromDt(dtBits);
      if (sgn.test<kOpRegD, kOpRegD>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>())) {
          if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kS32, DT::kS8, DT::kU16, DT::kU32, DT::kU8))) {
            opcode = opcodeTablePtr[0];
            opcode |= sz << 18u;
            goto Emit_R0At12Of4Hi22_R1At0Of4Hi5_NoCond;
          }
        }
      }

      if (sgn.test<kOpRegQ, kOpRegQ>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>())) {
          if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kS32, DT::kS8, DT::kU16, DT::kU32, DT::kU8))) {
            opcode = opcodeTablePtr[1];
            opcode |= sz << 18u;
            goto Emit_Q0At12Of4Hi22_Q1At0Of4Hi5_NoCond;
          }
        }
      }

      break;
    }

    case 85: {
      // Instruction 'vcmla'.
      if (sgn.test<kOpRegD, kOpRegD, kOpRegD, kOpImmI>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>())) {
          if (isDtSingle(dtBits, DT::kF32)) {
            VecRot2ImmEncode enc0;
            if (enc0.init(o3.as<Imm>())) {
              opcode = 0xFC300800u;
              opcode |= enc0.imm() << 23u;
              goto Emit_R0At12Of4Hi22_R1At16Of4Hi7_R2At0Of4Hi5_NoCond;
            }
          }

          if (isDtSingle(dtBits, DT::kF16)) {
            VecRot2ImmEncode enc4;
            if (enc4.init(o3.as<Imm>())) {
              opcode = 0xFC200800u;
              opcode |= enc4.imm() << 23u;
              goto Emit_R0At12Of4Hi22_R1At16Of4Hi7_R2At0Of4Hi5_NoCond;
            }
          }
        }

        if (isPureVec(o0.as<Vec>(), o1.as<Vec>()) && isElementVec(o2.as<Vec>())) {
          if (isDtSingle(dtBits, DT::kF32)) {
            uint32_t i = o2.as<Vec>().as<Vec>().elementIndex();
            if (i == 0u) {
              VecRot2ImmEncode enc2;
              if (enc2.init(o3.as<Imm>())) {
                opcode = 0xFE800800u;
                opcode |= enc2.imm() << 20u;
                goto Emit_R0At12Of4Hi22_R1At16Of4Hi7_R2At0Of4Hi5_NoCond;
              }
            }
          }

          if (isDtSingle(dtBits, DT::kF16)) {
            uint32_t i = o2.as<Vec>().as<Vec>().elementIndex();
            if (i <= 0x1) {
              VecRot2ImmEncode enc6;
              if (enc6.init(o3.as<Imm>())) {
                opcode = 0xFE000800u;
                opcode |= i << 5u;
                opcode |= enc6.imm() << 20u;
                goto Emit_R0At12Of4Hi22_R1At16Of4Hi7_R2At0Of4_NoCond;
              }
            }
          }
        }
      }

      if (sgn.test<kOpRegQ, kOpRegQ, kOpRegQ, kOpImmI>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>())) {
          if (isDtSingle(dtBits, DT::kF32)) {
            VecRot2ImmEncode enc1;
            if (enc1.init(o3.as<Imm>())) {
              opcode = 0xFC300840u;
              opcode |= enc1.imm() << 23u;
              goto Emit_Q0At12Of4Hi22_Q1At16Of4Hi7_Q2At0Of4Hi5_NoCond;
            }
          }

          if (isDtSingle(dtBits, DT::kF16)) {
            VecRot2ImmEncode enc5;
            if (enc5.init(o3.as<Imm>())) {
              opcode = 0xFC200840u;
              opcode |= enc5.imm() << 23u;
              goto Emit_Q0At12Of4Hi22_Q1At16Of4Hi7_Q2At0Of4Hi5_NoCond;
            }
          }
        }
      }

      if (sgn.test<kOpRegQ, kOpRegQ, kOpRegD, kOpImmI>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>()) && isElementVec(o2.as<Vec>())) {
          if (isDtSingle(dtBits, DT::kF32)) {
            uint32_t i = o2.as<Vec>().as<Vec>().elementIndex();
            if (i == 0u) {
              VecRot2ImmEncode enc3;
              if (enc3.init(o3.as<Imm>())) {
                opcode = 0xFE800840u;
                opcode |= enc3.imm() << 20u;
                goto Emit_Q0At12Of4Hi22_Q1At16Of4Hi7_R2At0Of4Hi5_NoCond;
              }
            }
          }

          if (isDtSingle(dtBits, DT::kF16)) {
            uint32_t i = o2.as<Vec>().as<Vec>().elementIndex();
            if (i <= 0x1) {
              VecRot2ImmEncode enc7;
              if (enc7.init(o3.as<Imm>())) {
                opcode = 0xFE000840u;
                opcode |= i << 5u;
                opcode |= enc7.imm() << 20u;
                goto Emit_Q0At12Of4Hi22_Q1At16Of4Hi7_R2At0Of4_NoCond;
              }
            }
          }
        }
      }

      break;
    }

    case 86: {
      static const uint32_t opcodeTable[] = {
        0x0EB40A40u, 0xEEB40940u, 0x0EB50A40u, 0xEEB50940u, 0x0EB40B40u, 0x0EB50B40u, // Instruction 'vcmp'.
        0x0EB40AC0u, 0xEEB409C0u, 0x0EB50AC0u, 0xEEB509C0u, 0x0EB40BC0u, 0x0EB50BC0u  // Instruction 'vcmpe'.
      };

      const uint32_t* opcodeTablePtr = opcodeTable + uint32_t(idr.index) * 6u;

      if (sgn.test<kOpRegS, kOpRegS>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>())) {
          if (isDtSingle(dtBits, DT::kF32)) {
            opcode = opcodeTablePtr[0];
            goto Emit_R0At12Of4Lo22_R1At0Of4Lo5_Cond;
          }

          if (isDtSingle(dtBits, DT::kF16)) {
            opcode = opcodeTablePtr[1];
            goto Emit_R0At12Of4Lo22_R1At0Of4Lo5_NoCond;
          }
        }
      }

      if (sgn.test<kOpRegS, kOpImmI>()) {
        if (isPureVec(o0.as<Vec>())) {
          if (isDtSingle(dtBits, DT::kF32)) {
            if (o1.as<Imm>().value() == 0u) {
              opcode = opcodeTablePtr[2];
              goto Emit_R0At12Of4Lo22_Cond;
            }
          }

          if (isDtSingle(dtBits, DT::kF16)) {
            if (o1.as<Imm>().value() == 0u) {
              opcode = opcodeTablePtr[3];
              goto Emit_R0At12Of4Lo22_NoCond;
            }
          }
        }
      }

      if (sgn.test<kOpRegD, kOpRegD>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>())) {
          if (isDtSingle(dtBits, DT::kF64)) {
            opcode = opcodeTablePtr[4];
            goto Emit_R0At12Of4Hi22_R1At0Of4Hi5_Cond;
          }
        }
      }

      if (sgn.test<kOpRegD, kOpImmI>()) {
        if (isPureVec(o0.as<Vec>())) {
          if (isDtSingle(dtBits, DT::kF64)) {
            if (o1.as<Imm>().value() == 0u) {
              opcode = opcodeTablePtr[5];
              goto Emit_R0At12Of4Hi22_Cond;
            }
          }
        }
      }

      break;
    }

    case 87: {
      static const uint32_t opcodeTable[] = {
        0xF3B00500u, 0xF3B00540u, // Instruction 'vcnt'.
        0xF3B00100u, 0xF3B00140u  // Instruction 'vrev16'.
      };

      const uint32_t* opcodeTablePtr = opcodeTable + uint32_t(idr.index) * 2u;

      if (sgn.test<kOpRegD, kOpRegD>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>())) {
          if (isDtMultiple(dtBits, makeDtBits(DT::kS8, DT::kU8))) {
            opcode = opcodeTablePtr[0];
            goto Emit_R0At12Of4Hi22_R1At0Of4Hi5_NoCond;
          }
        }
      }

      if (sgn.test<kOpRegQ, kOpRegQ>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>())) {
          if (isDtMultiple(dtBits, makeDtBits(DT::kS8, DT::kU8))) {
            opcode = opcodeTablePtr[1];
            goto Emit_Q0At12Of4Hi22_Q1At0Of4Hi5_NoCond;
          }
        }
      }

      break;
    }

    case 88: {
      // Instruction 'vcvt'.
      if (sgn.test<kOpRegS, kOpRegD>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>())) {
          if (isDtAndDt2Single(dtBits, DT::kF32, DT::kF64)) {
            opcode = 0x0EB70BC0u;
            goto Emit_R0At12Of4Lo22_R1At0Of4Hi5_Cond;
          }
        }
      }

      if (sgn.test<kOpRegD, kOpRegS>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>())) {
          if (isDtAndDt2Single(dtBits, DT::kF64, DT::kF32)) {
            opcode = 0x0EB70AC0u;
            goto Emit_R0At12Of4Hi22_R1At0Of4Lo5_Cond;
          }

          if (isDtAndDt2Single(dtBits, DT::kF64, DT::kS32)) {
            opcode = 0x0EB80BC0u;
            goto Emit_R0At12Of4Hi22_R1At0Of4Lo5_Cond;
          }

          if (isDtAndDt2Single(dtBits, DT::kF64, DT::kU32)) {
            opcode = 0x0EB80B40u;
            goto Emit_R0At12Of4Hi22_R1At0Of4Lo5_Cond;
          }
        }
      }

      if (sgn.test<kOpRegS, kOpRegS>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>())) {
          if (isDtAndDt2Single(dtBits, DT::kF32, DT::kS32)) {
            opcode = 0x0EB80AC0u;
            goto Emit_R0At12Of4Lo22_R1At0Of4Lo5_Cond;
          }

          if (isDtAndDt2Single(dtBits, DT::kF32, DT::kU32)) {
            opcode = 0x0EB80A40u;
            goto Emit_R0At12Of4Lo22_R1At0Of4Lo5_Cond;
          }
        }
      }

      if (sgn.test<kOpRegS, kOpRegS, kOpImmI>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>())) {
          if (isDtAndDt2Single(dtBits, DT::kF32, DT::kS16)) {
            VecFBitsVFPEncode enc0;
            if (enc0.init(16u, o2.as<Imm>())) {
              opcode = 0x0EBA0A40u;
              opcode |= (enc0.imm() & 0x1Eu) >> 1u;
              opcode |= (enc0.imm() & 0x1u) << 5u;
              goto Emit_R0At12Of4Lo22_R1At12Of4Lo22_Cond;
            }
          }

          if (isDtAndDt2Single(dtBits, DT::kS16, DT::kF32)) {
            VecFBitsVFPEncode enc1;
            if (enc1.init(16u, o2.as<Imm>())) {
              opcode = 0x0EBE0A40u;
              opcode |= (enc1.imm() & 0x1Eu) >> 1u;
              opcode |= (enc1.imm() & 0x1u) << 5u;
              goto Emit_R0At12Of4Lo22_R1At12Of4Lo22_Cond;
            }
          }

          if (isDtAndDt2Single(dtBits, DT::kF32, DT::kU16)) {
            VecFBitsVFPEncode enc2;
            if (enc2.init(16u, o2.as<Imm>())) {
              opcode = 0x0EBB0A40u;
              opcode |= (enc2.imm() & 0x1Eu) >> 1u;
              opcode |= (enc2.imm() & 0x1u) << 5u;
              goto Emit_R0At12Of4Lo22_R1At12Of4Lo22_Cond;
            }
          }

          if (isDtAndDt2Single(dtBits, DT::kU16, DT::kF32)) {
            VecFBitsVFPEncode enc3;
            if (enc3.init(16u, o2.as<Imm>())) {
              opcode = 0x0EBF0A40u;
              opcode |= (enc3.imm() & 0x1Eu) >> 1u;
              opcode |= (enc3.imm() & 0x1u) << 5u;
              goto Emit_R0At12Of4Lo22_R1At12Of4Lo22_Cond;
            }
          }

          if (isDtAndDt2Single(dtBits, DT::kF32, DT::kS32)) {
            VecFBitsVFPEncode enc4;
            if (enc4.init(32u, o2.as<Imm>())) {
              opcode = 0x0EBA0AC0u;
              opcode |= (enc4.imm() & 0x1Eu) >> 1u;
              opcode |= (enc4.imm() & 0x1u) << 5u;
              goto Emit_R0At12Of4Lo22_R1At12Of4Lo22_Cond;
            }
          }

          if (isDtAndDt2Single(dtBits, DT::kS32, DT::kF32)) {
            VecFBitsVFPEncode enc5;
            if (enc5.init(32u, o2.as<Imm>())) {
              opcode = 0x0EBE0AC0u;
              opcode |= (enc5.imm() & 0x1Eu) >> 1u;
              opcode |= (enc5.imm() & 0x1u) << 5u;
              goto Emit_R0At12Of4Lo22_R1At12Of4Lo22_Cond;
            }
          }

          if (isDtAndDt2Single(dtBits, DT::kF32, DT::kU32)) {
            VecFBitsVFPEncode enc6;
            if (enc6.init(32u, o2.as<Imm>())) {
              opcode = 0x0EBB0AC0u;
              opcode |= (enc6.imm() & 0x1Eu) >> 1u;
              opcode |= (enc6.imm() & 0x1u) << 5u;
              goto Emit_R0At12Of4Lo22_R1At12Of4Lo22_Cond;
            }
          }

          if (isDtAndDt2Single(dtBits, DT::kU32, DT::kF32)) {
            VecFBitsVFPEncode enc7;
            if (enc7.init(32u, o2.as<Imm>())) {
              opcode = 0x0EBF0AC0u;
              opcode |= (enc7.imm() & 0x1Eu) >> 1u;
              opcode |= (enc7.imm() & 0x1u) << 5u;
              goto Emit_R0At12Of4Lo22_R1At12Of4Lo22_Cond;
            }
          }
        }
      }

      if (sgn.test<kOpRegD, kOpRegD, kOpImmI>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>())) {
          if (isDtAndDt2Single(dtBits, DT::kF64, DT::kS16)) {
            VecFBitsVFPEncode enc8;
            if (enc8.init(16u, o2.as<Imm>())) {
              opcode = 0x0EBA0B40u;
              opcode |= (enc8.imm() & 0x1Eu) >> 1u;
              opcode |= (enc8.imm() & 0x1u) << 5u;
              goto Emit_R0At12Of4Hi22_R1At12Of4Hi22_Cond;
            }
          }

          if (isDtAndDt2Single(dtBits, DT::kS16, DT::kF64)) {
            VecFBitsVFPEncode enc9;
            if (enc9.init(16u, o2.as<Imm>())) {
              opcode = 0x0EBE0B40u;
              opcode |= (enc9.imm() & 0x1Eu) >> 1u;
              opcode |= (enc9.imm() & 0x1u) << 5u;
              goto Emit_R0At12Of4Hi22_R1At12Of4Hi22_Cond;
            }
          }

          if (isDtAndDt2Single(dtBits, DT::kF64, DT::kU16)) {
            VecFBitsVFPEncode enc10;
            if (enc10.init(16u, o2.as<Imm>())) {
              opcode = 0x0EBB0B40u;
              opcode |= (enc10.imm() & 0x1Eu) >> 1u;
              opcode |= (enc10.imm() & 0x1u) << 5u;
              goto Emit_R0At12Of4Hi22_R1At12Of4Hi22_Cond;
            }
          }

          if (isDtAndDt2Single(dtBits, DT::kU16, DT::kF64)) {
            VecFBitsVFPEncode enc11;
            if (enc11.init(16u, o2.as<Imm>())) {
              opcode = 0x0EBF0B40u;
              opcode |= (enc11.imm() & 0x1Eu) >> 1u;
              opcode |= (enc11.imm() & 0x1u) << 5u;
              goto Emit_R0At12Of4Hi22_R1At12Of4Hi22_Cond;
            }
          }

          if (isDtAndDt2Single(dtBits, DT::kF64, DT::kS32)) {
            VecFBitsVFPEncode enc12;
            if (enc12.init(32u, o2.as<Imm>())) {
              opcode = 0x0EBA0BC0u;
              opcode |= (enc12.imm() & 0x1Eu) >> 1u;
              opcode |= (enc12.imm() & 0x1u) << 5u;
              goto Emit_R0At12Of4Hi22_R1At12Of4Hi22_Cond;
            }
          }

          if (isDtAndDt2Single(dtBits, DT::kS32, DT::kF64)) {
            VecFBitsVFPEncode enc13;
            if (enc13.init(32u, o2.as<Imm>())) {
              opcode = 0x0EBE0BC0u;
              opcode |= (enc13.imm() & 0x1Eu) >> 1u;
              opcode |= (enc13.imm() & 0x1u) << 5u;
              goto Emit_R0At12Of4Hi22_R1At12Of4Hi22_Cond;
            }
          }

          if (isDtAndDt2Single(dtBits, DT::kF64, DT::kU32)) {
            VecFBitsVFPEncode enc14;
            if (enc14.init(32u, o2.as<Imm>())) {
              opcode = 0x0EBB0BC0u;
              opcode |= (enc14.imm() & 0x1Eu) >> 1u;
              opcode |= (enc14.imm() & 0x1u) << 5u;
              goto Emit_R0At12Of4Hi22_R1At12Of4Hi22_Cond;
            }
          }

          if (isDtAndDt2Single(dtBits, DT::kU32, DT::kF64)) {
            VecFBitsVFPEncode enc15;
            if (enc15.init(32u, o2.as<Imm>())) {
              opcode = 0x0EBF0BC0u;
              opcode |= (enc15.imm() & 0x1Eu) >> 1u;
              opcode |= (enc15.imm() & 0x1u) << 5u;
              goto Emit_R0At12Of4Hi22_R1At12Of4Hi22_Cond;
            }
          }

          if (isDtAndDt2Single(dtBits, DT::kF32, DT::kS32)) {
            VecFBitsASIMDEncode enc16;
            if (enc16.init(o2.as<Imm>())) {
              opcode = 0xF2800E10u;
              opcode |= enc16.imm() << 16u;
              goto Emit_R0At12Of4Hi22_R1At0Of4Hi5_NoCond;
            }
          }

          if (isDtAndDt2Single(dtBits, DT::kS32, DT::kF32)) {
            VecFBitsASIMDEncode enc18;
            if (enc18.init(o2.as<Imm>())) {
              opcode = 0xF2800F10u;
              opcode |= enc18.imm() << 16u;
              goto Emit_R0At12Of4Hi22_R1At0Of4Hi5_NoCond;
            }
          }

          if (isDtAndDt2Single(dtBits, DT::kF32, DT::kU32)) {
            VecFBitsASIMDEncode enc20;
            if (enc20.init(o2.as<Imm>())) {
              opcode = 0xF3800E10u;
              opcode |= enc20.imm() << 16u;
              goto Emit_R0At12Of4Hi22_R1At0Of4Hi5_NoCond;
            }
          }

          if (isDtAndDt2Single(dtBits, DT::kU32, DT::kF32)) {
            VecFBitsASIMDEncode enc22;
            if (enc22.init(o2.as<Imm>())) {
              opcode = 0xF3800F10u;
              opcode |= enc22.imm() << 16u;
              goto Emit_R0At12Of4Hi22_R1At0Of4Hi5_NoCond;
            }
          }

          if (isDtAndDt2Single(dtBits, DT::kF16, DT::kS16)) {
            VecFBitsASIMDEncode enc24;
            if (enc24.init(o2.as<Imm>())) {
              opcode = 0xF2800C10u;
              opcode |= enc24.imm() << 16u;
              goto Emit_R0At12Of4Hi22_R1At0Of4Hi5_NoCond;
            }
          }

          if (isDtAndDt2Single(dtBits, DT::kS16, DT::kF16)) {
            VecFBitsASIMDEncode enc26;
            if (enc26.init(o2.as<Imm>())) {
              opcode = 0xF2800D10u;
              opcode |= enc26.imm() << 16u;
              goto Emit_R0At12Of4Hi22_R1At0Of4Hi5_NoCond;
            }
          }

          if (isDtAndDt2Single(dtBits, DT::kF16, DT::kU16)) {
            VecFBitsASIMDEncode enc28;
            if (enc28.init(o2.as<Imm>())) {
              opcode = 0xF3800C10u;
              opcode |= enc28.imm() << 16u;
              goto Emit_R0At12Of4Hi22_R1At0Of4Hi5_NoCond;
            }
          }

          if (isDtAndDt2Single(dtBits, DT::kU16, DT::kF16)) {
            VecFBitsASIMDEncode enc30;
            if (enc30.init(o2.as<Imm>())) {
              opcode = 0xF3800D10u;
              opcode |= enc30.imm() << 16u;
              goto Emit_R0At12Of4Hi22_R1At0Of4Hi5_NoCond;
            }
          }
        }
      }

      if (sgn.test<kOpRegD, kOpRegD>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>())) {
          if (isDtAndDt2Single(dtBits, DT::kS32, DT::kF32)) {
            opcode = 0xF3BB0700u;
            goto Emit_R0At12Of4Hi22_R1At0Of4Hi5_NoCond;
          }

          if (isDtAndDt2Single(dtBits, DT::kU32, DT::kF32)) {
            opcode = 0xF3BB0780u;
            goto Emit_R0At12Of4Hi22_R1At0Of4Hi5_NoCond;
          }

          if (isDtAndDt2Single(dtBits, DT::kF32, DT::kS32)) {
            opcode = 0xF3BB0600u;
            goto Emit_R0At12Of4Hi22_R1At0Of4Hi5_NoCond;
          }

          if (isDtAndDt2Single(dtBits, DT::kF32, DT::kU32)) {
            opcode = 0xF3BB0680u;
            goto Emit_R0At12Of4Hi22_R1At0Of4Hi5_NoCond;
          }
        }
      }

      if (sgn.test<kOpRegQ, kOpRegQ>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>())) {
          if (isDtAndDt2Single(dtBits, DT::kS32, DT::kF32)) {
            opcode = 0xF3BB0740u;
            goto Emit_Q0At12Of4Hi22_Q1At0Of4Hi5_NoCond;
          }

          if (isDtAndDt2Single(dtBits, DT::kU32, DT::kF32)) {
            opcode = 0xF3BB07C0u;
            goto Emit_Q0At12Of4Hi22_Q1At0Of4Hi5_NoCond;
          }

          if (isDtAndDt2Single(dtBits, DT::kF32, DT::kS32)) {
            opcode = 0xF3BB0640u;
            goto Emit_Q0At12Of4Hi22_Q1At0Of4Hi5_NoCond;
          }

          if (isDtAndDt2Single(dtBits, DT::kF32, DT::kU32)) {
            opcode = 0xF3BB06C0u;
            goto Emit_Q0At12Of4Hi22_Q1At0Of4Hi5_NoCond;
          }
        }
      }

      if (sgn.test<kOpRegQ, kOpRegQ, kOpImmI>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>())) {
          if (isDtAndDt2Single(dtBits, DT::kF32, DT::kS32)) {
            VecFBitsASIMDEncode enc17;
            if (enc17.init(o2.as<Imm>())) {
              opcode = 0xF2800E50u;
              opcode |= enc17.imm() << 16u;
              goto Emit_Q0At12Of4Hi22_Q1At0Of4Hi5_NoCond;
            }
          }

          if (isDtAndDt2Single(dtBits, DT::kS32, DT::kF32)) {
            VecFBitsASIMDEncode enc19;
            if (enc19.init(o2.as<Imm>())) {
              opcode = 0xF2800F50u;
              opcode |= enc19.imm() << 16u;
              goto Emit_Q0At12Of4Hi22_Q1At0Of4Hi5_NoCond;
            }
          }

          if (isDtAndDt2Single(dtBits, DT::kF32, DT::kU32)) {
            VecFBitsASIMDEncode enc21;
            if (enc21.init(o2.as<Imm>())) {
              opcode = 0xF3800E50u;
              opcode |= enc21.imm() << 16u;
              goto Emit_Q0At12Of4Hi22_Q1At0Of4Hi5_NoCond;
            }
          }

          if (isDtAndDt2Single(dtBits, DT::kU32, DT::kF32)) {
            VecFBitsASIMDEncode enc23;
            if (enc23.init(o2.as<Imm>())) {
              opcode = 0xF3800F50u;
              opcode |= enc23.imm() << 16u;
              goto Emit_Q0At12Of4Hi22_Q1At0Of4Hi5_NoCond;
            }
          }

          if (isDtAndDt2Single(dtBits, DT::kF16, DT::kS16)) {
            VecFBitsASIMDEncode enc25;
            if (enc25.init(o2.as<Imm>())) {
              opcode = 0xF2800C50u;
              opcode |= enc25.imm() << 16u;
              goto Emit_Q0At12Of4Hi22_Q1At0Of4Hi5_NoCond;
            }
          }

          if (isDtAndDt2Single(dtBits, DT::kS16, DT::kF16)) {
            VecFBitsASIMDEncode enc27;
            if (enc27.init(o2.as<Imm>())) {
              opcode = 0xF2800D50u;
              opcode |= enc27.imm() << 16u;
              goto Emit_Q0At12Of4Hi22_Q1At0Of4Hi5_NoCond;
            }
          }

          if (isDtAndDt2Single(dtBits, DT::kF16, DT::kU16)) {
            VecFBitsASIMDEncode enc29;
            if (enc29.init(o2.as<Imm>())) {
              opcode = 0xF3800C50u;
              opcode |= enc29.imm() << 16u;
              goto Emit_Q0At12Of4Hi22_Q1At0Of4Hi5_NoCond;
            }
          }

          if (isDtAndDt2Single(dtBits, DT::kU16, DT::kF16)) {
            VecFBitsASIMDEncode enc31;
            if (enc31.init(o2.as<Imm>())) {
              opcode = 0xF3800D50u;
              opcode |= enc31.imm() << 16u;
              goto Emit_Q0At12Of4Hi22_Q1At0Of4Hi5_NoCond;
            }
          }
        }
      }

      if (sgn.test<kOpRegD, kOpRegQ>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>())) {
          if (isDtAndDt2Single(dtBits, DT::kF16, DT::kF32)) {
            opcode = 0xF3B60600u;
            goto Emit_R0At12Of4Hi22_Q1At0Of4Hi5_NoCond;
          }
        }
      }

      if (sgn.test<kOpRegQ, kOpRegD>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>())) {
          if (isDtAndDt2Single(dtBits, DT::kF32, DT::kF16)) {
            opcode = 0xF3B60700u;
            goto Emit_Q0At12Of4Hi22_R1At0Of4Hi5_NoCond;
          }
        }
      }

      break;
    }

    case 89: {
      // Instruction 'vcvta'.
      if (sgn.test<kOpRegS, kOpRegS>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>())) {
          if (isDtAndDt2Single(dtBits, DT::kS32, DT::kF32)) {
            opcode = 0xFEBC0AC0u;
            goto Emit_R0At12Of4Lo22_R1At0Of4Lo5_NoCond;
          }

          if (isDtAndDt2Single(dtBits, DT::kU32, DT::kF32)) {
            opcode = 0xFEBC0A40u;
            goto Emit_R0At12Of4Lo22_R1At0Of4Lo5_NoCond;
          }

          if (isDtAndDt2Single(dtBits, DT::kS32, DT::kF16)) {
            opcode = 0xFEBC09C0u;
            goto Emit_R0At12Of4Lo22_R1At0Of4Lo5_NoCond;
          }

          if (isDtAndDt2Single(dtBits, DT::kU32, DT::kF16)) {
            opcode = 0xFEBC0940u;
            goto Emit_R0At12Of4Lo22_R1At0Of4Lo5_NoCond;
          }
        }
      }

      if (sgn.test<kOpRegS, kOpRegD>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>())) {
          if (isDtAndDt2Single(dtBits, DT::kS32, DT::kF64)) {
            opcode = 0xFEBC0BC0u;
            goto Emit_R0At12Of4Lo22_R1At0Of4Hi5_NoCond;
          }

          if (isDtAndDt2Single(dtBits, DT::kU32, DT::kF64)) {
            opcode = 0xFEBC0B40u;
            goto Emit_R0At12Of4Lo22_R1At0Of4Hi5_NoCond;
          }
        }
      }

      break;
    }

    case 90: {
      static const uint32_t opcodeTable[] = {
        0x0EB30940u, 0x0EB30B40u, 0x0EB20A40u, 0x0EB30A40u, 0x0EB20B40u, // Instruction 'vcvtb'.
        0x0EB309C0u, 0x0EB30BC0u, 0x0EB20AC0u, 0x0EB30AC0u, 0x0EB20BC0u  // Instruction 'vcvtt'.
      };

      const uint32_t* opcodeTablePtr = opcodeTable + uint32_t(idr.index) * 5u;

      if (sgn.test<kOpRegS, kOpRegD>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>())) {
          if (isDtAndDt2Single(dtBits, DT::kBF16, DT::kF32)) {
            opcode = opcodeTablePtr[0];
            goto Emit_R0At12Of4Lo22_R1At0Of4Lo5_Cond;
          }

          if (isDtAndDt2Single(dtBits, DT::kF16, DT::kF64)) {
            opcode = opcodeTablePtr[1];
            goto Emit_R0At12Of4Lo22_R1At0Of4Hi5_Cond;
          }
        }
      }

      if (sgn.test<kOpRegS, kOpRegS>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>())) {
          if (isDtAndDt2Single(dtBits, DT::kF32, DT::kF16)) {
            opcode = opcodeTablePtr[2];
            goto Emit_R0At12Of4Lo22_R1At0Of4Lo5_Cond;
          }

          if (isDtAndDt2Single(dtBits, DT::kF16, DT::kF32)) {
            opcode = opcodeTablePtr[3];
            goto Emit_R0At12Of4Lo22_R1At0Of4Lo5_Cond;
          }
        }
      }

      if (sgn.test<kOpRegD, kOpRegS>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>())) {
          if (isDtAndDt2Single(dtBits, DT::kF64, DT::kF16)) {
            opcode = opcodeTablePtr[4];
            goto Emit_R0At12Of4Hi22_R1At0Of4Lo5_Cond;
          }
        }
      }

      break;
    }

    case 91: {
      static const uint32_t opcodeTable[] = {
        0xF3BB0300u, 0xF3BB0380u, 0xF3B70300u, 0xF3B70380u, 0xF3BB0340u, 0xF3BB03C0u, 0xF3B70340u, 0xF3B703C0u, 0xFEBF0AC0u, 0xFEBF0A40u, 0xFEBF09C0u, 0xFEBF0940u, 0xFEBF0BC0u, 0xFEBF0B40u, // Instruction 'vcvtm'.
        0xF3BB0100u, 0xF3BB0180u, 0xF3B70100u, 0xF3B70180u, 0xF3BB0140u, 0xF3BB01C0u, 0xF3B70140u, 0xF3B701C0u, 0xFEBD0AC0u, 0xFEBD0A40u, 0xFEBD09C0u, 0xFEBD0940u, 0xFEBD0BC0u, 0xFEBD0B40u, // Instruction 'vcvtn'.
        0xF3BB0200u, 0xF3BB0280u, 0xF3B70200u, 0xF3B70280u, 0xF3BB0240u, 0xF3BB02C0u, 0xF3B70240u, 0xF3B702C0u, 0xFEBE0AC0u, 0xFEBE0A40u, 0xFEBE09C0u, 0xFEBE0940u, 0xFEBE0BC0u, 0xFEBE0B40u  // Instruction 'vcvtp'.
      };

      const uint32_t* opcodeTablePtr = opcodeTable + uint32_t(idr.index) * 14u;

      if (sgn.test<kOpRegD, kOpRegD>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>())) {
          if (isDtAndDt2Single(dtBits, DT::kS32, DT::kF32)) {
            opcode = opcodeTablePtr[0];
            goto Emit_R0At12Of4Hi22_R1At0Of4Hi5_NoCond;
          }

          if (isDtAndDt2Single(dtBits, DT::kU32, DT::kF32)) {
            opcode = opcodeTablePtr[1];
            goto Emit_R0At12Of4Hi22_R1At0Of4Hi5_NoCond;
          }

          if (isDtAndDt2Single(dtBits, DT::kS16, DT::kF16)) {
            opcode = opcodeTablePtr[2];
            goto Emit_R0At12Of4Hi22_R1At0Of4Hi5_NoCond;
          }

          if (isDtAndDt2Single(dtBits, DT::kU16, DT::kF16)) {
            opcode = opcodeTablePtr[3];
            goto Emit_R0At12Of4Hi22_R1At0Of4Hi5_NoCond;
          }
        }
      }

      if (sgn.test<kOpRegQ, kOpRegQ>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>())) {
          if (isDtAndDt2Single(dtBits, DT::kS32, DT::kF32)) {
            opcode = opcodeTablePtr[4];
            goto Emit_Q0At12Of4Hi22_Q1At0Of4Hi5_NoCond;
          }

          if (isDtAndDt2Single(dtBits, DT::kU32, DT::kF32)) {
            opcode = opcodeTablePtr[5];
            goto Emit_Q0At12Of4Hi22_Q1At0Of4Hi5_NoCond;
          }

          if (isDtAndDt2Single(dtBits, DT::kS16, DT::kF16)) {
            opcode = opcodeTablePtr[6];
            goto Emit_Q0At12Of4Hi22_Q1At0Of4Hi5_NoCond;
          }

          if (isDtAndDt2Single(dtBits, DT::kU16, DT::kF16)) {
            opcode = opcodeTablePtr[7];
            goto Emit_Q0At12Of4Hi22_Q1At0Of4Hi5_NoCond;
          }
        }
      }

      if (sgn.test<kOpRegS, kOpRegS>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>())) {
          if (isDtAndDt2Single(dtBits, DT::kS32, DT::kF32)) {
            opcode = opcodeTablePtr[8];
            goto Emit_R0At12Of4Lo22_R1At0Of4Lo5_NoCond;
          }

          if (isDtAndDt2Single(dtBits, DT::kU32, DT::kF32)) {
            opcode = opcodeTablePtr[9];
            goto Emit_R0At12Of4Lo22_R1At0Of4Lo5_NoCond;
          }

          if (isDtAndDt2Single(dtBits, DT::kS32, DT::kF16)) {
            opcode = opcodeTablePtr[10];
            goto Emit_R0At12Of4Lo22_R1At0Of4Lo5_NoCond;
          }

          if (isDtAndDt2Single(dtBits, DT::kU32, DT::kF16)) {
            opcode = opcodeTablePtr[11];
            goto Emit_R0At12Of4Lo22_R1At0Of4Lo5_NoCond;
          }
        }
      }

      if (sgn.test<kOpRegS, kOpRegD>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>())) {
          if (isDtAndDt2Single(dtBits, DT::kS32, DT::kF64)) {
            opcode = opcodeTablePtr[12];
            goto Emit_R0At12Of4Lo22_R1At0Of4Hi5_NoCond;
          }

          if (isDtAndDt2Single(dtBits, DT::kU32, DT::kF64)) {
            opcode = opcodeTablePtr[13];
            goto Emit_R0At12Of4Lo22_R1At0Of4Hi5_NoCond;
          }
        }
      }

      break;
    }

    case 92: {
      // Instruction 'vcvtr'.
      if (sgn.test<kOpRegS, kOpRegS>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>())) {
          if (isDtAndDt2Single(dtBits, DT::kS32, DT::kF32)) {
            opcode = 0x0EBD0A40u;
            goto Emit_R0At12Of4Lo22_R1At0Of4Lo5_Cond;
          }

          if (isDtAndDt2Single(dtBits, DT::kU32, DT::kF32)) {
            opcode = 0x0EBC0A40u;
            goto Emit_R0At12Of4Lo22_R1At0Of4Lo5_Cond;
          }

          if (isDtAndDt2Single(dtBits, DT::kS32, DT::kF16)) {
            opcode = 0xEEBD0940u;
            goto Emit_R0At12Of4Lo22_R1At0Of4Lo5_NoCond;
          }

          if (isDtAndDt2Single(dtBits, DT::kU32, DT::kF16)) {
            opcode = 0xEEBC0940u;
            goto Emit_R0At12Of4Lo22_R1At0Of4Lo5_NoCond;
          }
        }
      }

      if (sgn.test<kOpRegS, kOpRegD>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>())) {
          if (isDtAndDt2Single(dtBits, DT::kS32, DT::kF64)) {
            opcode = 0x0EBD0B40u;
            goto Emit_R0At12Of4Lo22_R1At0Of4Hi5_Cond;
          }

          if (isDtAndDt2Single(dtBits, DT::kU32, DT::kF64)) {
            opcode = 0x0EBC0B40u;
            goto Emit_R0At12Of4Lo22_R1At0Of4Hi5_Cond;
          }
        }
      }

      break;
    }

    case 93: {
      static const uint32_t opcodeTable[] = {
        0x0E800A00u, 0xEE800900u, 0x0E800B00u, // Instruction 'vdiv'.
        0x0E100A40u, 0xEE100940u, 0x0E100B40u, // Instruction 'vnmla'.
        0x0E100A00u, 0xEE100900u, 0x0E100B00u, // Instruction 'vnmls'.
        0x0E200A40u, 0xEE200940u, 0x0E200B40u  // Instruction 'vnmul'.
      };

      const uint32_t* opcodeTablePtr = opcodeTable + uint32_t(idr.index) * 3u;

      if (sgn.test<kOpRegS, kOpRegS, kOpRegS>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>())) {
          if (isDtSingle(dtBits, DT::kF32)) {
            opcode = opcodeTablePtr[0];
            goto Emit_R0At12Of4Lo22_R1At16Of4Lo7_R2At0Of4Lo5_Cond;
          }

          if (isDtSingle(dtBits, DT::kF16)) {
            opcode = opcodeTablePtr[1];
            goto Emit_R0At12Of4Lo22_R1At16Of4Lo7_R2At0Of4Lo5_NoCond;
          }
        }
      }

      if (sgn.test<kOpRegD, kOpRegD, kOpRegD>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>())) {
          if (isDtSingle(dtBits, DT::kF64)) {
            opcode = opcodeTablePtr[2];
            goto Emit_R0At12Of4Hi22_R1At16Of4Hi7_R2At0Of4Hi5_Cond;
          }
        }
      }

      break;
    }

    case 94: {
      // Instruction 'vdot'.
      if (sgn.test<kOpRegD, kOpRegD, kOpRegD>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>())) {
          if (isDtSingle(dtBits, DT::kBF16)) {
            opcode = 0xFC000D00u;
            goto Emit_R0At12Of4Hi22_R1At16Of4Hi7_R2At0Of4Hi5_NoCond;
          }
        }

        if (isPureVec(o0.as<Vec>(), o1.as<Vec>()) && isElementVec(o2.as<Vec>())) {
          if (isDtSingle(dtBits, DT::kBF16)) {
            uint32_t i = o2.as<Vec>().as<Vec>().elementIndex();
            if (i <= 0x1) {
              opcode = 0xFE000D00u;
              opcode |= i << 5u;
              goto Emit_R0At12Of4Hi22_R1At16Of4Hi7_R2At0Of4_NoCond;
            }
          }
        }
      }

      if (sgn.test<kOpRegQ, kOpRegQ, kOpRegQ>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>())) {
          if (isDtSingle(dtBits, DT::kBF16)) {
            opcode = 0xFC000D40u;
            goto Emit_Q0At12Of4Hi22_Q1At16Of4Hi7_Q2At0Of4Hi5_NoCond;
          }
        }
      }

      if (sgn.test<kOpRegQ, kOpRegQ, kOpRegD>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>()) && isElementVec(o2.as<Vec>())) {
          if (isDtSingle(dtBits, DT::kBF16)) {
            uint32_t i = o2.as<Vec>().as<Vec>().elementIndex();
            if (i <= 0x1) {
              opcode = 0xFE000D40u;
              opcode |= i << 5u;
              goto Emit_Q0At12Of4Hi22_Q1At16Of4Hi7_R2At0Of4_NoCond;
            }
          }
        }
      }

      break;
    }

    case 95: {
      // Instruction 'vdup'.
      if (sgn.test<kOpRegD, kOpRegR>()) {
        if (isPureVec(o0.as<Vec>())) {
          if (isDtMultiple(dtBits, makeDtBits(DT::kS8, DT::kU8))) {
            opcode = 0x0EC00B10u;
            goto Emit_R0At16Of4Hi7_R1At12Of4_Cond;
          }

          if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16))) {
            opcode = 0x0E800B30u;
            goto Emit_R0At16Of4Hi7_R1At12Of4_Cond;
          }

          if (isDtMultiple(dtBits, makeDtBits(DT::kS32, DT::kU32, DT::kF32))) {
            opcode = 0x0E800B10u;
            goto Emit_R0At16Of4Hi7_R1At12Of4_Cond;
          }
        }
      }

      if (sgn.test<kOpRegQ, kOpRegR>()) {
        if (isPureVec(o0.as<Vec>())) {
          if (isDtMultiple(dtBits, makeDtBits(DT::kS8, DT::kU8))) {
            opcode = 0x0EE00B10u;
            goto Emit_Q0At16Of4Hi7_R1At12Of4_Cond;
          }

          if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16))) {
            opcode = 0x0EA00B30u;
            goto Emit_Q0At16Of4Hi7_R1At12Of4_Cond;
          }

          if (isDtMultiple(dtBits, makeDtBits(DT::kS32, DT::kU32, DT::kF32))) {
            opcode = 0x0EA00B10u;
            goto Emit_Q0At16Of4Hi7_R1At12Of4_Cond;
          }
        }
      }

      if (sgn.test<kOpRegD, kOpRegD>()) {
        if (isPureVec(o0.as<Vec>()) && isElementVec(o1.as<Vec>())) {
          if (isDtMultiple(dtBits, makeDtBits(DT::kS8, DT::kU8))) {
            uint32_t i = o1.as<Vec>().as<Vec>().elementIndex();
            if (i <= 0x7) {
              opcode = 0xF3B10C00u;
              opcode |= i << 17u;
              goto Emit_R0At12Of4Hi22_R1At0Of4Hi5_NoCond;
            }
          }

          if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16))) {
            uint32_t i = o1.as<Vec>().as<Vec>().elementIndex();
            if (i <= 0x3) {
              opcode = 0xF3B20C00u;
              opcode |= i << 18u;
              goto Emit_R0At12Of4Hi22_R1At0Of4Hi5_NoCond;
            }
          }

          if (isDtMultiple(dtBits, makeDtBits(DT::kS32, DT::kU32, DT::kF32))) {
            uint32_t i = o1.as<Vec>().as<Vec>().elementIndex();
            if (i <= 0x1) {
              opcode = 0xF3B40C00u;
              opcode |= i << 19u;
              goto Emit_R0At12Of4Hi22_R1At0Of4Hi5_NoCond;
            }
          }
        }
      }

      if (sgn.test<kOpRegQ, kOpRegD>()) {
        if (isPureVec(o0.as<Vec>()) && isElementVec(o1.as<Vec>())) {
          if (isDtMultiple(dtBits, makeDtBits(DT::kS8, DT::kU8))) {
            uint32_t i = o1.as<Vec>().as<Vec>().elementIndex();
            if (i <= 0x7) {
              opcode = 0xF3B10C40u;
              opcode |= i << 17u;
              goto Emit_Q0At12Of4Hi22_R1At0Of4Hi5_NoCond;
            }
          }

          if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16))) {
            uint32_t i = o1.as<Vec>().as<Vec>().elementIndex();
            if (i <= 0x3) {
              opcode = 0xF3B20C40u;
              opcode |= i << 18u;
              goto Emit_Q0At12Of4Hi22_R1At0Of4Hi5_NoCond;
            }
          }

          if (isDtMultiple(dtBits, makeDtBits(DT::kS32, DT::kU32, DT::kF32))) {
            uint32_t i = o1.as<Vec>().as<Vec>().elementIndex();
            if (i <= 0x1) {
              opcode = 0xF3B40C40u;
              opcode |= i << 19u;
              goto Emit_Q0At12Of4Hi22_R1At0Of4Hi5_NoCond;
            }
          }
        }
      }

      break;
    }

    case 96: {
      // Instruction 'vext'.
      if (sgn.test<kOpRegD, kOpRegD, kOpRegD, kOpImmI>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>())) {
          if (isDtMultiple(dtBits, makeDtBits(DT::kS8, DT::kU8))) {
            if (o3.as<Imm>().valueAs<uint64_t>() <= 0xFu) {
              opcode = 0xF2B00000u;
              opcode |= o3.as<Imm>().valueAs<uint32_t>() << 8u;
              goto Emit_R0At12Of4Hi22_R1At16Of4Hi7_R2At0Of4Hi5_NoCond;
            }
          }
        }
      }

      if (sgn.test<kOpRegQ, kOpRegQ, kOpRegQ, kOpImmI>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>())) {
          if (isDtMultiple(dtBits, makeDtBits(DT::kS8, DT::kU8))) {
            if (o3.as<Imm>().valueAs<uint64_t>() <= 0xFu) {
              opcode = 0xF2B00040u;
              opcode |= o3.as<Imm>().valueAs<uint32_t>() << 8u;
              goto Emit_Q0At12Of4Hi22_Q1At16Of4Hi7_Q2At0Of4Hi5_NoCond;
            }
          }
        }
      }

      break;
    }

    case 97: {
      static const uint32_t opcodeTable[] = {
        0xEEA00900u, 0x0EA00A00u, 0x0EA00B00u, 0xF2100C10u, 0xF2000C10u, 0xF2100C50u, 0xF2000C50u, // Instruction 'vfma'.
        0xEEA00940u, 0x0EA00A40u, 0x0EA00B40u, 0xF2300C10u, 0xF2200C10u, 0xF2300C50u, 0xF2200C50u  // Instruction 'vfms'.
      };

      const uint32_t* opcodeTablePtr = opcodeTable + uint32_t(idr.index) * 7u;

      if (sgn.test<kOpRegS, kOpRegS, kOpRegS>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>())) {
          if (isDtSingle(dtBits, DT::kF16)) {
            opcode = opcodeTablePtr[0];
            goto Emit_R0At12Of4Lo22_R1At16Of4Lo7_R2At0Of4Lo5_NoCond;
          }

          if (isDtSingle(dtBits, DT::kF32)) {
            opcode = opcodeTablePtr[1];
            goto Emit_R0At12Of4Lo22_R1At16Of4Lo7_R2At0Of4Lo5_Cond;
          }
        }
      }

      if (sgn.test<kOpRegD, kOpRegD, kOpRegD>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>())) {
          if (isDtSingle(dtBits, DT::kF64)) {
            opcode = opcodeTablePtr[2];
            goto Emit_R0At12Of4Hi22_R1At16Of4Hi7_R2At0Of4Hi5_Cond;
          }

          if (isDtSingle(dtBits, DT::kF16)) {
            opcode = opcodeTablePtr[3];
            goto Emit_R0At12Of4Hi22_R1At16Of4Hi7_R2At0Of4Hi5_NoCond;
          }

          if (isDtSingle(dtBits, DT::kF32)) {
            opcode = opcodeTablePtr[4];
            goto Emit_R0At12Of4Hi22_R1At16Of4Hi7_R2At0Of4Hi5_NoCond;
          }
        }
      }

      if (sgn.test<kOpRegQ, kOpRegQ, kOpRegQ>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>())) {
          if (isDtSingle(dtBits, DT::kF16)) {
            opcode = opcodeTablePtr[5];
            goto Emit_Q0At12Of4Hi22_Q1At16Of4Hi7_Q2At0Of4Hi5_NoCond;
          }

          if (isDtSingle(dtBits, DT::kF32)) {
            opcode = opcodeTablePtr[6];
            goto Emit_Q0At12Of4Hi22_Q1At16Of4Hi7_Q2At0Of4Hi5_NoCond;
          }
        }
      }

      break;
    }

    case 98: {
      static const uint32_t opcodeTable[] = {
        0xFC300810u, 0xFE300810u, // Instruction 'vfmab'.
        0xFC300850u, 0xFE300850u  // Instruction 'vfmat'.
      };

      const uint32_t* opcodeTablePtr = opcodeTable + uint32_t(idr.index) * 2u;

      if (sgn.test<kOpRegQ, kOpRegQ, kOpRegQ>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>())) {
          if (isDtSingle(dtBits, DT::kBF16)) {
            opcode = opcodeTablePtr[0];
            goto Emit_Q0At12Of4Hi22_Q1At16Of4Hi7_Q2At0Of4Hi5_NoCond;
          }
        }
      }

      if (sgn.test<kOpRegQ, kOpRegQ, kOpRegD>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>()) && isElementVec(o2.as<Vec>())) {
          if (isDtSingle(dtBits, DT::kBF16)) {
            uint32_t i = o2.as<Vec>().as<Vec>().elementIndex();
            if (i <= 0x3) {
              opcode = opcodeTablePtr[1];
              opcode |= (i & 0x1u) << 3u;
              opcode |= (i & 0x2u) << 4u;
              goto Emit_Q0At12Of4Hi22_Q1At16Of4Hi7_R2At0Of3_NoCond;
            }
          }
        }
      }

      break;
    }

    case 99: {
      static const uint32_t opcodeTable[] = {
        0xFC200810u, 0xFE000810u, 0xFC200850u, 0xFE000850u, // Instruction 'vfmal'.
        0xFCA00810u, 0xFE100810u, 0xFCA00850u, 0xFE100850u  // Instruction 'vfmsl'.
      };

      const uint32_t* opcodeTablePtr = opcodeTable + uint32_t(idr.index) * 4u;

      if (sgn.test<kOpRegD, kOpRegS, kOpRegS>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>())) {
          if (isDtSingle(dtBits, DT::kF16)) {
            opcode = opcodeTablePtr[0];
            goto Emit_R0At12Of4Hi22_R1At16Of4Lo7_R2At0Of4Lo5_NoCond;
          }
        }

        if (isPureVec(o0.as<Vec>(), o1.as<Vec>()) && isElementVec(o2.as<Vec>())) {
          if (isDtSingle(dtBits, DT::kF16)) {
            uint32_t i = o2.as<Vec>().as<Vec>().elementIndex();
            if (i <= 0x1) {
              opcode = opcodeTablePtr[1];
              opcode |= i << 3u;
              goto Emit_R0At12Of4Hi22_R1At16Of4Lo7_R2At0Of3Lo5_NoCond;
            }
          }
        }
      }

      if (sgn.test<kOpRegQ, kOpRegD, kOpRegD>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>())) {
          if (isDtSingle(dtBits, DT::kF16)) {
            opcode = opcodeTablePtr[2];
            goto Emit_Q0At12Of4Hi22_R1At16Of4Hi7_R2At0Of4Hi5_NoCond;
          }
        }

        if (isPureVec(o0.as<Vec>(), o1.as<Vec>()) && isElementVec(o2.as<Vec>())) {
          if (isDtSingle(dtBits, DT::kF16)) {
            uint32_t i = o2.as<Vec>().as<Vec>().elementIndex();
            if (i <= 0x3) {
              opcode = opcodeTablePtr[3];
              opcode |= (i & 0x1u) << 3u;
              opcode |= (i & 0x2u) << 4u;
              goto Emit_Q0At12Of4Hi22_R1At16Of4Hi7_R2At0Of3_NoCond;
            }
          }
        }
      }

      break;
    }

    case 100: {
      static const uint32_t opcodeTable[] = {
        0xEE900940u, 0x0E900A40u, 0x0E900B40u, // Instruction 'vfnma'.
        0xEE900900u, 0x0E900A00u, 0x0E900B00u  // Instruction 'vfnms'.
      };

      const uint32_t* opcodeTablePtr = opcodeTable + uint32_t(idr.index) * 3u;

      if (sgn.test<kOpRegS, kOpRegS, kOpRegS>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>())) {
          if (isDtSingle(dtBits, DT::kF16)) {
            opcode = opcodeTablePtr[0];
            goto Emit_R0At12Of4Lo22_R1At16Of4Lo7_R2At0Of4Lo5_NoCond;
          }

          if (isDtSingle(dtBits, DT::kF32)) {
            opcode = opcodeTablePtr[1];
            goto Emit_R0At12Of4Lo22_R1At16Of4Lo7_R2At0Of4Lo5_Cond;
          }
        }
      }

      if (sgn.test<kOpRegD, kOpRegD, kOpRegD>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>())) {
          if (isDtSingle(dtBits, DT::kF64)) {
            opcode = opcodeTablePtr[2];
            goto Emit_R0At12Of4Hi22_R1At16Of4Hi7_R2At0Of4Hi5_Cond;
          }
        }
      }

      break;
    }

    case 101: {
      static const uint32_t opcodeTable[] = {
        0xFEB00AC0u, // Instruction 'vins'.
        0xFEB00A40u  // Instruction 'vmovx'.
      };

      const uint32_t* opcodeTablePtr = opcodeTable + uint32_t(idr.index) * 1u;

      if (sgn.test<kOpRegS, kOpRegS>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>())) {
          if (isDtSingle(dtBits, DT::kF16)) {
            opcode = opcodeTablePtr[0];
            goto Emit_R0At12Of4Lo22_R1At0Of4Lo5_NoCond;
          }
        }
      }

      break;
    }

    case 102: {
      // Instruction 'vjcvt'.
      if (sgn.test<kOpRegS, kOpRegD>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>())) {
          if (isDtAndDt2Single(dtBits, DT::kS32, DT::kF64)) {
            opcode = 0xEEB90BC0u;
            goto Emit_R0At12Of4Lo22_R1At0Of4Hi5_NoCond;
          }
        }
      }

      break;
    }

    case 103: {
      // Instruction 'vld1'.
      uint32_t sz = szFromDt(dtBits);
      if (sgn.test<kOpRegD, kOpMemB>()) {
        mem = &o1.as<Mem>();

        if (mem->baseId() < 15u) {
          if (!mem->hasIndex()) {
            if (!mem->offsetLo32()) {
              if (mem->isFixedOffset()) {
                if (isPureVec(o0.as<Vec>())) {
                  if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16, DT::kS32, DT::kU32, DT::kF32, DT::kS64, DT::kU64, DT::kF64, DT::kS8, DT::kU8))) {
                    opcode = 0xF420070Fu;
                    opcode |= sz << 6u;
                    goto Emit_R0At12Of4Hi22_MemBaseAt16_NoCond;
                  }
                }

                if (isElementVec(o0.as<Vec>())) {
                  if (isDtMultiple(dtBits, makeDtBits(DT::kS8, DT::kU8))) {
                    uint32_t i = o0.as<Vec>().as<Vec>().elementIndex();
                    if (i <= 0x7) {
                      opcode = 0xF4A0000Fu;
                      opcode |= i << 5u;
                      goto Emit_R0At12Of4Hi22_MemBaseAt16_NoCond;
                    }
                  }

                  if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16))) {
                    uint32_t i = o0.as<Vec>().as<Vec>().elementIndex();
                    if (i <= 0x3) {
                      opcode = 0xF4A0040Fu;
                      opcode |= i << 6u;
                      goto Emit_R0At12Of4Hi22_MemBaseAt16_NoCond;
                    }
                  }

                  if (isDtMultiple(dtBits, makeDtBits(DT::kS32, DT::kU32, DT::kF32))) {
                    uint32_t i = o0.as<Vec>().as<Vec>().elementIndex();
                    if (i <= 0x1) {
                      opcode = 0xF4A0080Fu;
                      opcode |= i << 7u;
                      goto Emit_R0At12Of4Hi22_MemBaseAt16_NoCond;
                    }
                  }
                }
              }
            }

            if (uint32_t(mem->offsetLo32()) == 8u) {
              if (mem->isPostIndex()) {
                if (isPureVec(o0.as<Vec>())) {
                  if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16, DT::kS32, DT::kU32, DT::kF32, DT::kS64, DT::kU64, DT::kF64, DT::kS8, DT::kU8))) {
                    opcode = 0xF420070Du;
                    opcode |= sz << 6u;
                    goto Emit_R0At12Of4Hi22_MemBaseAt16_NoCond;
                  }
                }
              }
            }

            if (uint32_t(mem->offsetLo32()) == 1u) {
              if (mem->isPostIndex()) {
                if (isElementVec(o0.as<Vec>())) {
                  if (isDtMultiple(dtBits, makeDtBits(DT::kS8, DT::kU8))) {
                    uint32_t i = o0.as<Vec>().as<Vec>().elementIndex();
                    if (i <= 0x7) {
                      opcode = 0xF4A0000Du;
                      opcode |= i << 5u;
                      goto Emit_R0At12Of4Hi22_MemBaseAt16_NoCond;
                    }
                  }
                }
              }
            }

            if (uint32_t(mem->offsetLo32()) == 2u) {
              if (mem->isPostIndex()) {
                if (isElementVec(o0.as<Vec>())) {
                  if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16))) {
                    uint32_t i = o0.as<Vec>().as<Vec>().elementIndex();
                    if (i <= 0x3) {
                      opcode = 0xF4A0040Du;
                      opcode |= i << 6u;
                      goto Emit_R0At12Of4Hi22_MemBaseAt16_NoCond;
                    }
                  }
                }
              }
            }

            if (uint32_t(mem->offsetLo32()) == 4u) {
              if (mem->isPostIndex()) {
                if (isElementVec(o0.as<Vec>())) {
                  if (isDtMultiple(dtBits, makeDtBits(DT::kS32, DT::kU32, DT::kF32))) {
                    uint32_t i = o0.as<Vec>().as<Vec>().elementIndex();
                    if (i <= 0x1) {
                      opcode = 0xF4A0080Du;
                      opcode |= i << 7u;
                      goto Emit_R0At12Of4Hi22_MemBaseAt16_NoCond;
                    }
                  }
                }
              }
            }
          }

          if (mem->indexType() == RegType::kGp32) {
            if (!mem->offsetLo32()) {
              if ((mem->indexId() < 13u) || (mem->indexId() == 14u)) {
                if (mem->isPostIndex()) {
                  if (isPureVec(o0.as<Vec>())) {
                    if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16, DT::kS32, DT::kU32, DT::kF32, DT::kS64, DT::kU64, DT::kF64, DT::kS8, DT::kU8))) {
                      opcode = 0xF4200700u;
                      opcode |= sz << 6u;
                      goto Emit_R0At12Of4Hi22_MemBaseAt16_MemUIndexAt0_NoCond;
                    }
                  }

                  if (isElementVec(o0.as<Vec>())) {
                    if (isDtMultiple(dtBits, makeDtBits(DT::kS8, DT::kU8))) {
                      uint32_t i = o0.as<Vec>().as<Vec>().elementIndex();
                      if (i <= 0x7) {
                        opcode = 0xF4A00000u;
                        opcode |= i << 5u;
                        goto Emit_R0At12Of4Hi22_MemBaseAt16_MemUIndexAt0_NoCond;
                      }
                    }

                    if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16))) {
                      uint32_t i = o0.as<Vec>().as<Vec>().elementIndex();
                      if (i <= 0x3) {
                        opcode = 0xF4A00400u;
                        opcode |= i << 6u;
                        goto Emit_R0At12Of4Hi22_MemBaseAt16_MemUIndexAt0_NoCond;
                      }
                    }

                    if (isDtMultiple(dtBits, makeDtBits(DT::kS32, DT::kU32, DT::kF32))) {
                      uint32_t i = o0.as<Vec>().as<Vec>().elementIndex();
                      if (i <= 0x1) {
                        opcode = 0xF4A00800u;
                        opcode |= i << 7u;
                        goto Emit_R0At12Of4Hi22_MemBaseAt16_MemUIndexAt0_NoCond;
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }

      if (sgn.test<kOpRegD, kOpRegD, kOpMemB>()) {
        mem = &o2.as<Mem>();

        if (mem->baseId() < 15u) {
          if (!mem->hasIndex()) {
            if (!mem->offsetLo32()) {
              if (mem->isFixedOffset()) {
                if (isPureVec(o0.as<Vec>(), o1.as<Vec>())) {
                  if (isConsecutive(1, o0.as<Reg>(), o1.as<Reg>())) {
                    if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16, DT::kS32, DT::kU32, DT::kF32, DT::kS64, DT::kU64, DT::kF64, DT::kS8, DT::kU8))) {
                      opcode = 0xF4200A0Fu;
                      opcode |= sz << 6u;
                      goto Emit_R0At12Of4Hi22_MemBaseAt16_NoCond;
                    }
                  }
                }
              }
            }

            if (uint32_t(mem->offsetLo32()) == 16u) {
              if (mem->isPostIndex()) {
                if (isPureVec(o0.as<Vec>(), o1.as<Vec>())) {
                  if (isConsecutive(1, o0.as<Reg>(), o1.as<Reg>())) {
                    if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16, DT::kS32, DT::kU32, DT::kF32, DT::kS64, DT::kU64, DT::kF64, DT::kS8, DT::kU8))) {
                      opcode = 0xF4200A0Du;
                      opcode |= sz << 6u;
                      goto Emit_R0At12Of4Hi22_MemBaseAt16_NoCond;
                    }
                  }
                }
              }
            }
          }

          if (mem->indexType() == RegType::kGp32) {
            if (!mem->offsetLo32()) {
              if ((mem->indexId() < 13u) || (mem->indexId() == 14u)) {
                if (mem->isPostIndex()) {
                  if (isPureVec(o0.as<Vec>(), o1.as<Vec>())) {
                    if (isConsecutive(1, o0.as<Reg>(), o1.as<Reg>())) {
                      if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16, DT::kS32, DT::kU32, DT::kF32, DT::kS64, DT::kU64, DT::kF64, DT::kS8, DT::kU8))) {
                        opcode = 0xF4200A00u;
                        opcode |= sz << 6u;
                        goto Emit_R0At12Of4Hi22_MemBaseAt16_MemUIndexAt0_NoCond;
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }

      if (sgn.test<kOpRegD, kOpRegD, kOpRegD, kOpMemB>()) {
        mem = &o3.as<Mem>();

        if (mem->baseId() < 15u) {
          if (!mem->hasIndex()) {
            if (!mem->offsetLo32()) {
              if (mem->isFixedOffset()) {
                if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>())) {
                  if (isConsecutive(1, o0.as<Reg>(), o1.as<Reg>(), o2.as<Reg>())) {
                    if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16, DT::kS32, DT::kU32, DT::kF32, DT::kS64, DT::kU64, DT::kF64, DT::kS8, DT::kU8))) {
                      opcode = 0xF420060Fu;
                      opcode |= sz << 6u;
                      goto Emit_R0At12Of4Hi22_MemBaseAt16_NoCond;
                    }
                  }
                }
              }
            }

            if (uint32_t(mem->offsetLo32()) == 24u) {
              if (mem->isPostIndex()) {
                if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>())) {
                  if (isConsecutive(1, o0.as<Reg>(), o1.as<Reg>(), o2.as<Reg>())) {
                    if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16, DT::kS32, DT::kU32, DT::kF32, DT::kS64, DT::kU64, DT::kF64, DT::kS8, DT::kU8))) {
                      opcode = 0xF420060Du;
                      opcode |= sz << 6u;
                      goto Emit_R0At12Of4Hi22_MemBaseAt16_NoCond;
                    }
                  }
                }
              }
            }
          }

          if (mem->indexType() == RegType::kGp32) {
            if (!mem->offsetLo32()) {
              if ((mem->indexId() < 13u) || (mem->indexId() == 14u)) {
                if (mem->isPostIndex()) {
                  if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>())) {
                    if (isConsecutive(1, o0.as<Reg>(), o1.as<Reg>(), o2.as<Reg>())) {
                      if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16, DT::kS32, DT::kU32, DT::kF32, DT::kS64, DT::kU64, DT::kF64, DT::kS8, DT::kU8))) {
                        opcode = 0xF4200600u;
                        opcode |= sz << 6u;
                        goto Emit_R0At12Of4Hi22_MemBaseAt16_MemUIndexAt0_NoCond;
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }

      if (sgn.test<kOpRegD, kOpRegD, kOpRegD, kOpRegD, kOpMemB>()) {
        mem = &o4.as<Mem>();

        if (mem->baseId() < 15u) {
          if (!mem->hasIndex()) {
            if (!mem->offsetLo32()) {
              if (mem->isFixedOffset()) {
                if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>(), o3.as<Vec>())) {
                  if (isConsecutive(1, o0.as<Reg>(), o1.as<Reg>(), o2.as<Reg>(), o3.as<Reg>())) {
                    if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16, DT::kS32, DT::kU32, DT::kF32, DT::kS64, DT::kU64, DT::kF64, DT::kS8, DT::kU8))) {
                      opcode = 0xF420020Fu;
                      opcode |= sz << 6u;
                      goto Emit_R0At12Of4Hi22_MemBaseAt16_NoCond;
                    }
                  }
                }
              }
            }

            if (uint32_t(mem->offsetLo32()) == 32u) {
              if (mem->isPostIndex()) {
                if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>(), o3.as<Vec>())) {
                  if (isConsecutive(1, o0.as<Reg>(), o1.as<Reg>(), o2.as<Reg>(), o3.as<Reg>())) {
                    if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16, DT::kS32, DT::kU32, DT::kF32, DT::kS64, DT::kU64, DT::kF64, DT::kS8, DT::kU8))) {
                      opcode = 0xF420020Du;
                      opcode |= sz << 6u;
                      goto Emit_R0At12Of4Hi22_MemBaseAt16_NoCond;
                    }
                  }
                }
              }
            }
          }

          if (mem->indexType() == RegType::kGp32) {
            if (!mem->offsetLo32()) {
              if ((mem->indexId() < 13u) || (mem->indexId() == 14u)) {
                if (mem->isPostIndex()) {
                  if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>(), o3.as<Vec>())) {
                    if (isConsecutive(1, o0.as<Reg>(), o1.as<Reg>(), o2.as<Reg>(), o3.as<Reg>())) {
                      if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16, DT::kS32, DT::kU32, DT::kF32, DT::kS64, DT::kU64, DT::kF64, DT::kS8, DT::kU8))) {
                        opcode = 0xF4200200u;
                        opcode |= sz << 6u;
                        goto Emit_R0At12Of4Hi22_MemBaseAt16_MemUIndexAt0_NoCond;
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }

      break;
    }

    case 104: {
      // Instruction 'vld1r'.
      uint32_t sz = szFromDt(dtBits);
      if (sgn.test<kOpRegD, kOpMemB>()) {
        mem = &o1.as<Mem>();

        if (mem->baseId() < 15u) {
          if (!mem->hasIndex()) {
            if (!mem->offsetLo32()) {
              if (mem->isFixedOffset()) {
                if (isPureVec(o0.as<Vec>())) {
                  if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16, DT::kS32, DT::kU32, DT::kF32, DT::kS8, DT::kU8))) {
                    opcode = 0xF4A00C0Fu;
                    opcode |= sz << 6u;
                    goto Emit_R0At12Of4Hi22_MemBaseAt16_NoCond;
                  }
                }
              }
            }

            if (uint32_t(mem->offsetLo32()) == (1u << sz)) {
              if (mem->isPostIndex()) {
                if (isPureVec(o0.as<Vec>())) {
                  if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16, DT::kS32, DT::kU32, DT::kF32, DT::kS8, DT::kU8))) {
                    opcode = 0xF4A00C0Du;
                    opcode |= sz << 6u;
                    goto Emit_R0At12Of4Hi22_MemBaseAt16_NoCond;
                  }
                }
              }
            }
          }

          if (mem->indexType() == RegType::kGp32) {
            if (!mem->offsetLo32()) {
              if ((mem->indexId() < 13u) || (mem->indexId() == 14u)) {
                if (mem->isPostIndex()) {
                  if (isPureVec(o0.as<Vec>())) {
                    if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16, DT::kS32, DT::kU32, DT::kF32, DT::kS8, DT::kU8))) {
                      opcode = 0xF4A00C00u;
                      opcode |= sz << 6u;
                      goto Emit_R0At12Of4Hi22_MemBaseAt16_MemUIndexAt0_NoCond;
                    }
                  }
                }
              }
            }
          }
        }
      }

      if (sgn.test<kOpRegD, kOpRegD, kOpMemB>()) {
        mem = &o2.as<Mem>();

        if (mem->baseId() < 15u) {
          if (!mem->hasIndex()) {
            if (!mem->offsetLo32()) {
              if (mem->isFixedOffset()) {
                if (isPureVec(o0.as<Vec>(), o1.as<Vec>())) {
                  if (isConsecutive(1, o0.as<Reg>(), o1.as<Reg>())) {
                    if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16, DT::kS32, DT::kU32, DT::kF32, DT::kS8, DT::kU8))) {
                      opcode = 0xF4A00C2Fu;
                      opcode |= sz << 6u;
                      goto Emit_R0At12Of4Hi22_MemBaseAt16_NoCond;
                    }
                  }
                }
              }
            }

            if (uint32_t(mem->offsetLo32()) == (1u << sz)) {
              if (mem->isPostIndex()) {
                if (isPureVec(o0.as<Vec>(), o1.as<Vec>())) {
                  if (isConsecutive(1, o0.as<Reg>(), o1.as<Reg>())) {
                    if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16, DT::kS32, DT::kU32, DT::kF32, DT::kS8, DT::kU8))) {
                      opcode = 0xF4A00C2Du;
                      opcode |= sz << 6u;
                      goto Emit_R0At12Of4Hi22_MemBaseAt16_NoCond;
                    }
                  }
                }
              }
            }
          }

          if (mem->indexType() == RegType::kGp32) {
            if (!mem->offsetLo32()) {
              if ((mem->indexId() < 13u) || (mem->indexId() == 14u)) {
                if (mem->isPostIndex()) {
                  if (isPureVec(o0.as<Vec>(), o1.as<Vec>())) {
                    if (isConsecutive(1, o0.as<Reg>(), o1.as<Reg>())) {
                      if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16, DT::kS32, DT::kU32, DT::kF32, DT::kS8, DT::kU8))) {
                        opcode = 0xF4A00C20u;
                        opcode |= sz << 6u;
                        goto Emit_R0At12Of4Hi22_MemBaseAt16_MemUIndexAt0_NoCond;
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }

      break;
    }

    case 105: {
      // Instruction 'vld2'.
      uint32_t sz = szFromDt(dtBits);
      if (sgn.test<kOpRegD, kOpRegD, kOpMemB>()) {
        mem = &o2.as<Mem>();

        if (mem->baseId() < 15u) {
          if (!mem->hasIndex()) {
            if (!mem->offsetLo32()) {
              if (mem->isFixedOffset()) {
                if (isPureVec(o0.as<Vec>(), o1.as<Vec>())) {
                  if (isConsecutive(1, o0.as<Reg>(), o1.as<Reg>())) {
                    if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16, DT::kS32, DT::kU32, DT::kF32, DT::kS8, DT::kU8))) {
                      opcode = 0xF420080Fu;
                      opcode |= sz << 6u;
                      goto Emit_R0At12Of4Hi22_MemBaseAt16_NoCond;
                    }
                  }

                  if (isConsecutive(2, o0.as<Reg>(), o1.as<Reg>())) {
                    if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16, DT::kS32, DT::kU32, DT::kF32, DT::kS8, DT::kU8))) {
                      opcode = 0xF420090Fu;
                      opcode |= sz << 6u;
                      goto Emit_R0At12Of4Hi22_MemBaseAt16_NoCond;
                    }
                  }
                }

                if (isElementVec(o0.as<Vec>(), o1.as<Vec>())) {
                  if (isConsecutive(1, o0.as<Reg>(), o1.as<Reg>())) {
                    if (isDtMultiple(dtBits, makeDtBits(DT::kS8, DT::kU8))) {
                      uint32_t i = o0.as<Vec>().as<Vec>().elementIndex();
                      if (i <= 0x7) {
                        opcode = 0xF4A0010Fu;
                        opcode |= i << 5u;
                        goto Emit_R0At12Of4Hi22_MemBaseAt16_NoCond;
                      }
                    }

                    if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16))) {
                      uint32_t i = o0.as<Vec>().as<Vec>().elementIndex();
                      if (i <= 0x3) {
                        opcode = 0xF4A0050Fu;
                        opcode |= i << 6u;
                        goto Emit_R0At12Of4Hi22_MemBaseAt16_NoCond;
                      }
                    }

                    if (isDtMultiple(dtBits, makeDtBits(DT::kS32, DT::kU32, DT::kF32))) {
                      uint32_t i = o0.as<Vec>().as<Vec>().elementIndex();
                      if (i <= 0x1) {
                        opcode = 0xF4A0090Fu;
                        opcode |= i << 7u;
                        goto Emit_R0At12Of4Hi22_MemBaseAt16_NoCond;
                      }
                    }
                  }

                  if (isConsecutive(2, o0.as<Reg>(), o1.as<Reg>())) {
                    if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16))) {
                      uint32_t i = o0.as<Vec>().as<Vec>().elementIndex();
                      if (i <= 0x3) {
                        opcode = 0xF4A0052Fu;
                        opcode |= i << 6u;
                        goto Emit_R0At12Of4Hi22_MemBaseAt16_NoCond;
                      }
                    }

                    if (isDtMultiple(dtBits, makeDtBits(DT::kS32, DT::kU32, DT::kF32))) {
                      uint32_t i = o0.as<Vec>().as<Vec>().elementIndex();
                      if (i <= 0x1) {
                        opcode = 0xF4A0094Fu;
                        opcode |= i << 7u;
                        goto Emit_R0At12Of4Hi22_MemBaseAt16_NoCond;
                      }
                    }
                  }
                }
              }
            }

            if (uint32_t(mem->offsetLo32()) == 16u) {
              if (mem->isPostIndex()) {
                if (isPureVec(o0.as<Vec>(), o1.as<Vec>())) {
                  if (isConsecutive(1, o0.as<Reg>(), o1.as<Reg>())) {
                    if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16, DT::kS32, DT::kU32, DT::kF32, DT::kS8, DT::kU8))) {
                      opcode = 0xF420080Du;
                      opcode |= sz << 6u;
                      goto Emit_R0At12Of4Hi22_MemBaseAt16_NoCond;
                    }
                  }

                  if (isConsecutive(2, o0.as<Reg>(), o1.as<Reg>())) {
                    if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16, DT::kS32, DT::kU32, DT::kF32, DT::kS8, DT::kU8))) {
                      opcode = 0xF420090Du;
                      opcode |= sz << 6u;
                      goto Emit_R0At12Of4Hi22_MemBaseAt16_NoCond;
                    }
                  }
                }
              }
            }

            if (uint32_t(mem->offsetLo32()) == 2u) {
              if (mem->isPostIndex()) {
                if (isElementVec(o0.as<Vec>(), o1.as<Vec>())) {
                  if (isConsecutive(1, o0.as<Reg>(), o1.as<Reg>())) {
                    if (isDtMultiple(dtBits, makeDtBits(DT::kS8, DT::kU8))) {
                      uint32_t i = o0.as<Vec>().as<Vec>().elementIndex();
                      if (i <= 0x7) {
                        opcode = 0xF4A0010Du;
                        opcode |= i << 5u;
                        goto Emit_R0At12Of4Hi22_MemBaseAt16_NoCond;
                      }
                    }
                  }
                }
              }
            }

            if (uint32_t(mem->offsetLo32()) == 4u) {
              if (mem->isPostIndex()) {
                if (isElementVec(o0.as<Vec>(), o1.as<Vec>())) {
                  if (isConsecutive(1, o0.as<Reg>(), o1.as<Reg>())) {
                    if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16))) {
                      uint32_t i = o0.as<Vec>().as<Vec>().elementIndex();
                      if (i <= 0x3) {
                        opcode = 0xF4A0050Du;
                        opcode |= i << 6u;
                        goto Emit_R0At12Of4Hi22_MemBaseAt16_NoCond;
                      }
                    }
                  }

                  if (isConsecutive(2, o0.as<Reg>(), o1.as<Reg>())) {
                    if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16))) {
                      uint32_t i = o0.as<Vec>().as<Vec>().elementIndex();
                      if (i <= 0x3) {
                        opcode = 0xF4A0052Du;
                        opcode |= i << 6u;
                        goto Emit_R0At12Of4Hi22_MemBaseAt16_NoCond;
                      }
                    }
                  }
                }
              }
            }

            if (uint32_t(mem->offsetLo32()) == 8u) {
              if (mem->isPostIndex()) {
                if (isElementVec(o0.as<Vec>(), o1.as<Vec>())) {
                  if (isConsecutive(1, o0.as<Reg>(), o1.as<Reg>())) {
                    if (isDtMultiple(dtBits, makeDtBits(DT::kS32, DT::kU32, DT::kF32))) {
                      uint32_t i = o0.as<Vec>().as<Vec>().elementIndex();
                      if (i <= 0x1) {
                        opcode = 0xF4A0090Du;
                        opcode |= i << 7u;
                        goto Emit_R0At12Of4Hi22_MemBaseAt16_NoCond;
                      }
                    }
                  }

                  if (isConsecutive(2, o0.as<Reg>(), o1.as<Reg>())) {
                    if (isDtMultiple(dtBits, makeDtBits(DT::kS32, DT::kU32, DT::kF32))) {
                      uint32_t i = o0.as<Vec>().as<Vec>().elementIndex();
                      if (i <= 0x1) {
                        opcode = 0xF4A0094Du;
                        opcode |= i << 7u;
                        goto Emit_R0At12Of4Hi22_MemBaseAt16_NoCond;
                      }
                    }
                  }
                }
              }
            }
          }

          if (mem->indexType() == RegType::kGp32) {
            if (!mem->offsetLo32()) {
              if ((mem->indexId() < 13u) || (mem->indexId() == 14u)) {
                if (mem->isPostIndex()) {
                  if (isPureVec(o0.as<Vec>(), o1.as<Vec>())) {
                    if (isConsecutive(1, o0.as<Reg>(), o1.as<Reg>())) {
                      if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16, DT::kS32, DT::kU32, DT::kF32, DT::kS8, DT::kU8))) {
                        opcode = 0xF4200800u;
                        opcode |= sz << 6u;
                        goto Emit_R0At12Of4Hi22_MemBaseAt16_MemUIndexAt0_NoCond;
                      }
                    }

                    if (isConsecutive(2, o0.as<Reg>(), o1.as<Reg>())) {
                      if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16, DT::kS32, DT::kU32, DT::kF32, DT::kS8, DT::kU8))) {
                        opcode = 0xF4200900u;
                        opcode |= sz << 6u;
                        goto Emit_R0At12Of4Hi22_MemBaseAt16_MemUIndexAt0_NoCond;
                      }
                    }
                  }

                  if (isElementVec(o0.as<Vec>(), o1.as<Vec>())) {
                    if (isConsecutive(1, o0.as<Reg>(), o1.as<Reg>())) {
                      if (isDtMultiple(dtBits, makeDtBits(DT::kS8, DT::kU8))) {
                        uint32_t i = o0.as<Vec>().as<Vec>().elementIndex();
                        if (i <= 0x7) {
                          opcode = 0xF4A00100u;
                          opcode |= i << 5u;
                          goto Emit_R0At12Of4Hi22_MemBaseAt16_MemUIndexAt0_NoCond;
                        }
                      }

                      if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16))) {
                        uint32_t i = o0.as<Vec>().as<Vec>().elementIndex();
                        if (i <= 0x3) {
                          opcode = 0xF4A00500u;
                          opcode |= i << 6u;
                          goto Emit_R0At12Of4Hi22_MemBaseAt16_MemUIndexAt0_NoCond;
                        }
                      }

                      if (isDtMultiple(dtBits, makeDtBits(DT::kS32, DT::kU32, DT::kF32))) {
                        uint32_t i = o0.as<Vec>().as<Vec>().elementIndex();
                        if (i <= 0x1) {
                          opcode = 0xF4A00900u;
                          opcode |= i << 7u;
                          goto Emit_R0At12Of4Hi22_MemBaseAt16_MemUIndexAt0_NoCond;
                        }
                      }
                    }

                    if (isConsecutive(2, o0.as<Reg>(), o1.as<Reg>())) {
                      if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16))) {
                        uint32_t i = o0.as<Vec>().as<Vec>().elementIndex();
                        if (i <= 0x3) {
                          opcode = 0xF4A00520u;
                          opcode |= i << 6u;
                          goto Emit_R0At12Of4Hi22_MemBaseAt16_MemUIndexAt0_NoCond;
                        }
                      }

                      if (isDtMultiple(dtBits, makeDtBits(DT::kS32, DT::kU32, DT::kF32))) {
                        uint32_t i = o0.as<Vec>().as<Vec>().elementIndex();
                        if (i <= 0x1) {
                          opcode = 0xF4A00940u;
                          opcode |= i << 7u;
                          goto Emit_R0At12Of4Hi22_MemBaseAt16_MemUIndexAt0_NoCond;
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }

      if (sgn.test<kOpRegD, kOpRegD, kOpRegD, kOpRegD, kOpMemB>()) {
        mem = &o4.as<Mem>();

        if (mem->baseId() < 15u) {
          if (!mem->hasIndex()) {
            if (!mem->offsetLo32()) {
              if (mem->isFixedOffset()) {
                if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>(), o3.as<Vec>())) {
                  if (isConsecutive(1, o0.as<Reg>(), o1.as<Reg>(), o2.as<Reg>(), o3.as<Reg>())) {
                    if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16, DT::kS32, DT::kU32, DT::kF32, DT::kS8, DT::kU8))) {
                      opcode = 0xF420030Fu;
                      opcode |= sz << 6u;
                      goto Emit_R0At12Of4Hi22_MemBaseAt16_NoCond;
                    }
                  }
                }
              }
            }

            if (uint32_t(mem->offsetLo32()) == 32u) {
              if (mem->isPostIndex()) {
                if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>(), o3.as<Vec>())) {
                  if (isConsecutive(1, o0.as<Reg>(), o1.as<Reg>(), o2.as<Reg>(), o3.as<Reg>())) {
                    if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16, DT::kS32, DT::kU32, DT::kF32, DT::kS8, DT::kU8))) {
                      opcode = 0xF420030Du;
                      opcode |= sz << 6u;
                      goto Emit_R0At12Of4Hi22_MemBaseAt16_NoCond;
                    }
                  }
                }
              }
            }
          }

          if (mem->indexType() == RegType::kGp32) {
            if (!mem->offsetLo32()) {
              if ((mem->indexId() < 13u) || (mem->indexId() == 14u)) {
                if (mem->isPostIndex()) {
                  if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>(), o3.as<Vec>())) {
                    if (isConsecutive(1, o0.as<Reg>(), o1.as<Reg>(), o2.as<Reg>(), o3.as<Reg>())) {
                      if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16, DT::kS32, DT::kU32, DT::kF32, DT::kS8, DT::kU8))) {
                        opcode = 0xF4200300u;
                        opcode |= sz << 6u;
                        goto Emit_R0At12Of4Hi22_MemBaseAt16_MemUIndexAt0_NoCond;
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }

      break;
    }

    case 106: {
      // Instruction 'vld2r'.
      uint32_t sz = szFromDt(dtBits);
      if (sgn.test<kOpRegD, kOpRegD, kOpMemB>()) {
        mem = &o2.as<Mem>();

        if (mem->baseId() < 15u) {
          if (!mem->hasIndex()) {
            if (!mem->offsetLo32()) {
              if (mem->isFixedOffset()) {
                if (isPureVec(o0.as<Vec>(), o1.as<Vec>())) {
                  if (isConsecutive(1, o0.as<Reg>(), o1.as<Reg>())) {
                    if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16, DT::kS32, DT::kU32, DT::kF32, DT::kS8, DT::kU8))) {
                      opcode = 0xF4A00D0Fu;
                      opcode |= sz << 6u;
                      goto Emit_R0At12Of4Hi22_MemBaseAt16_NoCond;
                    }
                  }

                  if (isConsecutive(2, o0.as<Reg>(), o1.as<Reg>())) {
                    if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16, DT::kS32, DT::kU32, DT::kF32, DT::kS8, DT::kU8))) {
                      opcode = 0xF4A00D2Fu;
                      opcode |= sz << 6u;
                      goto Emit_R0At12Of4Hi22_MemBaseAt16_NoCond;
                    }
                  }
                }
              }
            }

            if (uint32_t(mem->offsetLo32()) == (2u << sz)) {
              if (mem->isPostIndex()) {
                if (isPureVec(o0.as<Vec>(), o1.as<Vec>())) {
                  if (isConsecutive(1, o0.as<Reg>(), o1.as<Reg>())) {
                    if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16, DT::kS32, DT::kU32, DT::kF32, DT::kS8, DT::kU8))) {
                      opcode = 0xF4A00D0Du;
                      opcode |= sz << 6u;
                      goto Emit_R0At12Of4Hi22_MemBaseAt16_NoCond;
                    }
                  }

                  if (isConsecutive(2, o0.as<Reg>(), o1.as<Reg>())) {
                    if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16, DT::kS32, DT::kU32, DT::kF32, DT::kS8, DT::kU8))) {
                      opcode = 0xF4A00D2Du;
                      opcode |= sz << 6u;
                      goto Emit_R0At12Of4Hi22_MemBaseAt16_NoCond;
                    }
                  }
                }
              }
            }
          }

          if (mem->indexType() == RegType::kGp32) {
            if (!mem->offsetLo32()) {
              if ((mem->indexId() < 13u) || (mem->indexId() == 14u)) {
                if (mem->isPostIndex()) {
                  if (isPureVec(o0.as<Vec>(), o1.as<Vec>())) {
                    if (isConsecutive(1, o0.as<Reg>(), o1.as<Reg>())) {
                      if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16, DT::kS32, DT::kU32, DT::kF32, DT::kS8, DT::kU8))) {
                        opcode = 0xF4A00D00u;
                        opcode |= sz << 6u;
                        goto Emit_R0At12Of4Hi22_MemBaseAt16_MemUIndexAt0_NoCond;
                      }
                    }

                    if (isConsecutive(2, o0.as<Reg>(), o1.as<Reg>())) {
                      if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16, DT::kS32, DT::kU32, DT::kF32, DT::kS8, DT::kU8))) {
                        opcode = 0xF4A00D20u;
                        opcode |= sz << 6u;
                        goto Emit_R0At12Of4Hi22_MemBaseAt16_MemUIndexAt0_NoCond;
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }

      break;
    }

    case 107: {
      // Instruction 'vld3'.
      uint32_t sz = szFromDt(dtBits);
      if (sgn.test<kOpRegD, kOpRegD, kOpRegD, kOpMemB>()) {
        mem = &o3.as<Mem>();

        if (mem->baseId() < 15u) {
          if (!mem->hasIndex()) {
            if (!mem->offsetLo32()) {
              if (mem->isFixedOffset()) {
                if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>())) {
                  if (isConsecutive(1, o0.as<Reg>(), o1.as<Reg>(), o2.as<Reg>())) {
                    if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16, DT::kS32, DT::kU32, DT::kF32, DT::kS8, DT::kU8))) {
                      opcode = 0xF420040Fu;
                      opcode |= sz << 6u;
                      goto Emit_R0At12Of4Hi22_MemBaseAt16_NoCond;
                    }
                  }

                  if (isConsecutive(2, o0.as<Reg>(), o1.as<Reg>(), o2.as<Reg>())) {
                    if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16, DT::kS32, DT::kU32, DT::kF32, DT::kS8, DT::kU8))) {
                      opcode = 0xF420052Fu;
                      opcode |= sz << 6u;
                      goto Emit_R0At12Of4Hi22_MemBaseAt16_NoCond;
                    }
                  }
                }

                if (isElementVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>())) {
                  if (isConsecutive(1, o0.as<Reg>(), o1.as<Reg>(), o2.as<Reg>())) {
                    if (isDtMultiple(dtBits, makeDtBits(DT::kS8, DT::kU8))) {
                      uint32_t i = o0.as<Vec>().as<Vec>().elementIndex();
                      if (i <= 0x7) {
                        opcode = 0xF4A0020Fu;
                        opcode |= i << 5u;
                        goto Emit_R0At12Of4Hi22_MemBaseAt16_NoCond;
                      }
                    }

                    if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16))) {
                      uint32_t i = o0.as<Vec>().as<Vec>().elementIndex();
                      if (i <= 0x3) {
                        opcode = 0xF4A0060Fu;
                        opcode |= i << 6u;
                        goto Emit_R0At12Of4Hi22_MemBaseAt16_NoCond;
                      }
                    }

                    if (isDtMultiple(dtBits, makeDtBits(DT::kS32, DT::kU32, DT::kF32))) {
                      uint32_t i = o0.as<Vec>().as<Vec>().elementIndex();
                      if (i <= 0x1) {
                        opcode = 0xF4A00A0Fu;
                        opcode |= i << 7u;
                        goto Emit_R0At12Of4Hi22_MemBaseAt16_NoCond;
                      }
                    }
                  }

                  if (isConsecutive(2, o0.as<Reg>(), o1.as<Reg>(), o2.as<Reg>())) {
                    if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16))) {
                      uint32_t i = o0.as<Vec>().as<Vec>().elementIndex();
                      if (i <= 0x3) {
                        opcode = 0xF4A0062Fu;
                        opcode |= i << 6u;
                        goto Emit_R0At12Of4Hi22_MemBaseAt16_NoCond;
                      }
                    }

                    if (isDtMultiple(dtBits, makeDtBits(DT::kS32, DT::kU32, DT::kF32))) {
                      uint32_t i = o0.as<Vec>().as<Vec>().elementIndex();
                      if (i <= 0x1) {
                        opcode = 0xF4A00A4Fu;
                        opcode |= i << 7u;
                        goto Emit_R0At12Of4Hi22_MemBaseAt16_NoCond;
                      }
                    }
                  }
                }
              }
            }

            if (uint32_t(mem->offsetLo32()) == 24u) {
              if (mem->isPostIndex()) {
                if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>())) {
                  if (isConsecutive(1, o0.as<Reg>(), o1.as<Reg>(), o2.as<Reg>())) {
                    if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16, DT::kS32, DT::kU32, DT::kF32, DT::kS8, DT::kU8))) {
                      opcode = 0xF420040Du;
                      opcode |= sz << 6u;
                      goto Emit_R0At12Of4Hi22_MemBaseAt16_NoCond;
                    }
                  }

                  if (isConsecutive(2, o0.as<Reg>(), o1.as<Reg>(), o2.as<Reg>())) {
                    if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16, DT::kS32, DT::kU32, DT::kF32, DT::kS8, DT::kU8))) {
                      opcode = 0xF420052Du;
                      opcode |= sz << 6u;
                      goto Emit_R0At12Of4Hi22_MemBaseAt16_NoCond;
                    }
                  }
                }
              }
            }

            if (uint32_t(mem->offsetLo32()) == 3u) {
              if (mem->isPostIndex()) {
                if (isElementVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>())) {
                  if (isConsecutive(1, o0.as<Reg>(), o1.as<Reg>(), o2.as<Reg>())) {
                    if (isDtMultiple(dtBits, makeDtBits(DT::kS8, DT::kU8))) {
                      uint32_t i = o0.as<Vec>().as<Vec>().elementIndex();
                      if (i <= 0x7) {
                        opcode = 0xF4A0020Du;
                        opcode |= i << 5u;
                        goto Emit_R0At12Of4Hi22_MemBaseAt16_NoCond;
                      }
                    }
                  }
                }
              }
            }

            if (uint32_t(mem->offsetLo32()) == 6u) {
              if (mem->isPostIndex()) {
                if (isElementVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>())) {
                  if (isConsecutive(1, o0.as<Reg>(), o1.as<Reg>(), o2.as<Reg>())) {
                    if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16))) {
                      uint32_t i = o0.as<Vec>().as<Vec>().elementIndex();
                      if (i <= 0x3) {
                        opcode = 0xF4A0060Du;
                        opcode |= i << 6u;
                        goto Emit_R0At12Of4Hi22_MemBaseAt16_NoCond;
                      }
                    }
                  }

                  if (isConsecutive(2, o0.as<Reg>(), o1.as<Reg>(), o2.as<Reg>())) {
                    if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16))) {
                      uint32_t i = o0.as<Vec>().as<Vec>().elementIndex();
                      if (i <= 0x3) {
                        opcode = 0xF4A0062Du;
                        opcode |= i << 6u;
                        goto Emit_R0At12Of4Hi22_MemBaseAt16_NoCond;
                      }
                    }
                  }
                }
              }
            }

            if (uint32_t(mem->offsetLo32()) == 12u) {
              if (mem->isPostIndex()) {
                if (isElementVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>())) {
                  if (isConsecutive(1, o0.as<Reg>(), o1.as<Reg>(), o2.as<Reg>())) {
                    if (isDtMultiple(dtBits, makeDtBits(DT::kS32, DT::kU32, DT::kF32))) {
                      uint32_t i = o0.as<Vec>().as<Vec>().elementIndex();
                      if (i <= 0x1) {
                        opcode = 0xF4A00A0Du;
                        opcode |= i << 7u;
                        goto Emit_R0At12Of4Hi22_MemBaseAt16_NoCond;
                      }
                    }
                  }

                  if (isConsecutive(2, o0.as<Reg>(), o1.as<Reg>(), o2.as<Reg>())) {
                    if (isDtMultiple(dtBits, makeDtBits(DT::kS32, DT::kU32, DT::kF32))) {
                      uint32_t i = o0.as<Vec>().as<Vec>().elementIndex();
                      if (i <= 0x1) {
                        opcode = 0xF4A00A4Du;
                        opcode |= i << 7u;
                        goto Emit_R0At12Of4Hi22_MemBaseAt16_NoCond;
                      }
                    }
                  }
                }
              }
            }
          }

          if (mem->indexType() == RegType::kGp32) {
            if (!mem->offsetLo32()) {
              if ((mem->indexId() < 13u) || (mem->indexId() == 14u)) {
                if (mem->isPostIndex()) {
                  if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>())) {
                    if (isConsecutive(1, o0.as<Reg>(), o1.as<Reg>(), o2.as<Reg>())) {
                      if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16, DT::kS32, DT::kU32, DT::kF32, DT::kS8, DT::kU8))) {
                        opcode = 0xF4200400u;
                        opcode |= sz << 6u;
                        goto Emit_R0At12Of4Hi22_MemBaseAt16_MemUIndexAt0_NoCond;
                      }
                    }

                    if (isConsecutive(2, o0.as<Reg>(), o1.as<Reg>(), o2.as<Reg>())) {
                      if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16, DT::kS32, DT::kU32, DT::kF32, DT::kS8, DT::kU8))) {
                        opcode = 0xF4200520u;
                        opcode |= sz << 6u;
                        goto Emit_R0At12Of4Hi22_MemBaseAt16_MemUIndexAt0_NoCond;
                      }
                    }
                  }

                  if (isElementVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>())) {
                    if (isConsecutive(1, o0.as<Reg>(), o1.as<Reg>(), o2.as<Reg>())) {
                      if (isDtMultiple(dtBits, makeDtBits(DT::kS8, DT::kU8))) {
                        uint32_t i = o0.as<Vec>().as<Vec>().elementIndex();
                        if (i <= 0x7) {
                          opcode = 0xF4A00200u;
                          opcode |= i << 5u;
                          goto Emit_R0At12Of4Hi22_MemBaseAt16_MemUIndexAt0_NoCond;
                        }
                      }

                      if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16))) {
                        uint32_t i = o0.as<Vec>().as<Vec>().elementIndex();
                        if (i <= 0x3) {
                          opcode = 0xF4A00600u;
                          opcode |= i << 6u;
                          goto Emit_R0At12Of4Hi22_MemBaseAt16_MemUIndexAt0_NoCond;
                        }
                      }

                      if (isDtMultiple(dtBits, makeDtBits(DT::kS32, DT::kU32, DT::kF32))) {
                        uint32_t i = o0.as<Vec>().as<Vec>().elementIndex();
                        if (i <= 0x1) {
                          opcode = 0xF4A00A00u;
                          opcode |= i << 7u;
                          goto Emit_R0At12Of4Hi22_MemBaseAt16_MemUIndexAt0_NoCond;
                        }
                      }
                    }

                    if (isConsecutive(2, o0.as<Reg>(), o1.as<Reg>(), o2.as<Reg>())) {
                      if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16))) {
                        uint32_t i = o0.as<Vec>().as<Vec>().elementIndex();
                        if (i <= 0x3) {
                          opcode = 0xF4A00620u;
                          opcode |= i << 6u;
                          goto Emit_R0At12Of4Hi22_MemBaseAt16_MemUIndexAt0_NoCond;
                        }
                      }

                      if (isDtMultiple(dtBits, makeDtBits(DT::kS32, DT::kU32, DT::kF32))) {
                        uint32_t i = o0.as<Vec>().as<Vec>().elementIndex();
                        if (i <= 0x1) {
                          opcode = 0xF4A00A40u;
                          opcode |= i << 7u;
                          goto Emit_R0At12Of4Hi22_MemBaseAt16_MemUIndexAt0_NoCond;
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }

      break;
    }

    case 108: {
      // Instruction 'vld3r'.
      uint32_t sz = szFromDt(dtBits);
      if (sgn.test<kOpRegD, kOpRegD, kOpRegD, kOpMemB>()) {
        mem = &o3.as<Mem>();

        if (mem->baseId() < 15u) {
          if (!mem->hasIndex()) {
            if (!mem->offsetLo32()) {
              if (mem->isFixedOffset()) {
                if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>())) {
                  if (isConsecutive(1, o0.as<Reg>(), o1.as<Reg>(), o2.as<Reg>())) {
                    if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16, DT::kS32, DT::kU32, DT::kF32, DT::kS8, DT::kU8))) {
                      opcode = 0xF4A00E0Fu;
                      opcode |= sz << 6u;
                      goto Emit_R0At12Of4Hi22_MemBaseAt16_NoCond;
                    }
                  }

                  if (isConsecutive(2, o0.as<Reg>(), o1.as<Reg>(), o2.as<Reg>())) {
                    if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16, DT::kS32, DT::kU32, DT::kF32, DT::kS8, DT::kU8))) {
                      opcode = 0xF4A00E2Fu;
                      opcode |= sz << 6u;
                      goto Emit_R0At12Of4Hi22_MemBaseAt16_NoCond;
                    }
                  }
                }
              }
            }

            if (uint32_t(mem->offsetLo32()) == (3u << sz)) {
              if (mem->isPostIndex()) {
                if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>())) {
                  if (isConsecutive(1, o0.as<Reg>(), o1.as<Reg>(), o2.as<Reg>())) {
                    if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16, DT::kS32, DT::kU32, DT::kF32, DT::kS8, DT::kU8))) {
                      opcode = 0xF4A00E0Du;
                      opcode |= sz << 6u;
                      goto Emit_R0At12Of4Hi22_MemBaseAt16_NoCond;
                    }
                  }

                  if (isConsecutive(2, o0.as<Reg>(), o1.as<Reg>(), o2.as<Reg>())) {
                    if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16, DT::kS32, DT::kU32, DT::kF32, DT::kS8, DT::kU8))) {
                      opcode = 0xF4A00E2Du;
                      opcode |= sz << 6u;
                      goto Emit_R0At12Of4Hi22_MemBaseAt16_NoCond;
                    }
                  }
                }
              }
            }
          }

          if (mem->indexType() == RegType::kGp32) {
            if (!mem->offsetLo32()) {
              if ((mem->indexId() < 13u) || (mem->indexId() == 14u)) {
                if (mem->isPostIndex()) {
                  if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>())) {
                    if (isConsecutive(1, o0.as<Reg>(), o1.as<Reg>(), o2.as<Reg>())) {
                      if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16, DT::kS32, DT::kU32, DT::kF32, DT::kS8, DT::kU8))) {
                        opcode = 0xF4A00E00u;
                        opcode |= sz << 6u;
                        goto Emit_R0At12Of4Hi22_MemBaseAt16_MemUIndexAt0_NoCond;
                      }
                    }

                    if (isConsecutive(2, o0.as<Reg>(), o1.as<Reg>(), o2.as<Reg>())) {
                      if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16, DT::kS32, DT::kU32, DT::kF32, DT::kS8, DT::kU8))) {
                        opcode = 0xF4A00E20u;
                        opcode |= sz << 6u;
                        goto Emit_R0At12Of4Hi22_MemBaseAt16_MemUIndexAt0_NoCond;
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }

      break;
    }

    case 109: {
      // Instruction 'vld4'.
      uint32_t sz = szFromDt(dtBits);
      if (sgn.test<kOpRegD, kOpRegD, kOpRegD, kOpRegD, kOpMemB>()) {
        mem = &o4.as<Mem>();

        if (mem->baseId() < 15u) {
          if (!mem->hasIndex()) {
            if (!mem->offsetLo32()) {
              if (mem->isFixedOffset()) {
                if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>(), o3.as<Vec>())) {
                  if (isConsecutive(1, o0.as<Reg>(), o1.as<Reg>(), o2.as<Reg>(), o3.as<Reg>())) {
                    if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16, DT::kS32, DT::kU32, DT::kF32, DT::kS8, DT::kU8))) {
                      opcode = 0xF420000Fu;
                      opcode |= sz << 6u;
                      goto Emit_R0At12Of4Hi22_MemBaseAt16_NoCond;
                    }
                  }

                  if (isConsecutive(2, o0.as<Reg>(), o1.as<Reg>(), o2.as<Reg>(), o3.as<Reg>())) {
                    if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16, DT::kS32, DT::kU32, DT::kF32, DT::kS8, DT::kU8))) {
                      opcode = 0xF420010Fu;
                      opcode |= sz << 6u;
                      goto Emit_R0At12Of4Hi22_MemBaseAt16_NoCond;
                    }
                  }
                }

                if (isElementVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>(), o3.as<Vec>())) {
                  if (isConsecutive(1, o0.as<Reg>(), o1.as<Reg>(), o2.as<Reg>(), o3.as<Reg>())) {
                    if (isDtMultiple(dtBits, makeDtBits(DT::kS8, DT::kU8))) {
                      uint32_t i = o0.as<Vec>().as<Vec>().elementIndex();
                      if (i <= 0x7) {
                        opcode = 0xF4A0030Fu;
                        opcode |= i << 5u;
                        goto Emit_R0At12Of4Hi22_MemBaseAt16_NoCond;
                      }
                    }

                    if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16))) {
                      uint32_t i = o0.as<Vec>().as<Vec>().elementIndex();
                      if (i <= 0x3) {
                        opcode = 0xF4A0070Fu;
                        opcode |= i << 6u;
                        goto Emit_R0At12Of4Hi22_MemBaseAt16_NoCond;
                      }
                    }

                    if (isDtMultiple(dtBits, makeDtBits(DT::kS32, DT::kU32, DT::kF32))) {
                      uint32_t i = o0.as<Vec>().as<Vec>().elementIndex();
                      if (i <= 0x1) {
                        opcode = 0xF4A00B0Fu;
                        opcode |= i << 7u;
                        goto Emit_R0At12Of4Hi22_MemBaseAt16_NoCond;
                      }
                    }
                  }

                  if (isConsecutive(2, o0.as<Reg>(), o1.as<Reg>(), o2.as<Reg>(), o3.as<Reg>())) {
                    if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16))) {
                      uint32_t i = o0.as<Vec>().as<Vec>().elementIndex();
                      if (i <= 0x3) {
                        opcode = 0xF4A0072Fu;
                        opcode |= i << 6u;
                        goto Emit_R0At12Of4Hi22_MemBaseAt16_NoCond;
                      }
                    }

                    if (isDtMultiple(dtBits, makeDtBits(DT::kS32, DT::kU32, DT::kF32))) {
                      uint32_t i = o0.as<Vec>().as<Vec>().elementIndex();
                      if (i <= 0x1) {
                        opcode = 0xF4A00B4Fu;
                        opcode |= i << 7u;
                        goto Emit_R0At12Of4Hi22_MemBaseAt16_NoCond;
                      }
                    }
                  }
                }
              }
            }

            if (uint32_t(mem->offsetLo32()) == 32u) {
              if (mem->isPostIndex()) {
                if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>(), o3.as<Vec>())) {
                  if (isConsecutive(1, o0.as<Reg>(), o1.as<Reg>(), o2.as<Reg>(), o3.as<Reg>())) {
                    if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16, DT::kS32, DT::kU32, DT::kF32, DT::kS8, DT::kU8))) {
                      opcode = 0xF420000Du;
                      opcode |= sz << 6u;
                      goto Emit_R0At12Of4Hi22_MemBaseAt16_NoCond;
                    }
                  }

                  if (isConsecutive(2, o0.as<Reg>(), o1.as<Reg>(), o2.as<Reg>(), o3.as<Reg>())) {
                    if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16, DT::kS32, DT::kU32, DT::kF32, DT::kS8, DT::kU8))) {
                      opcode = 0xF420010Du;
                      opcode |= sz << 6u;
                      goto Emit_R0At12Of4Hi22_MemBaseAt16_NoCond;
                    }
                  }
                }
              }
            }

            if (uint32_t(mem->offsetLo32()) == 4u) {
              if (mem->isPostIndex()) {
                if (isElementVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>(), o3.as<Vec>())) {
                  if (isConsecutive(1, o0.as<Reg>(), o1.as<Reg>(), o2.as<Reg>(), o3.as<Reg>())) {
                    if (isDtMultiple(dtBits, makeDtBits(DT::kS8, DT::kU8))) {
                      uint32_t i = o0.as<Vec>().as<Vec>().elementIndex();
                      if (i <= 0x7) {
                        opcode = 0xF4A0030Du;
                        opcode |= i << 5u;
                        goto Emit_R0At12Of4Hi22_MemBaseAt16_NoCond;
                      }
                    }
                  }
                }
              }
            }

            if (uint32_t(mem->offsetLo32()) == 8u) {
              if (mem->isPostIndex()) {
                if (isElementVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>(), o3.as<Vec>())) {
                  if (isConsecutive(1, o0.as<Reg>(), o1.as<Reg>(), o2.as<Reg>(), o3.as<Reg>())) {
                    if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16))) {
                      uint32_t i = o0.as<Vec>().as<Vec>().elementIndex();
                      if (i <= 0x3) {
                        opcode = 0xF4A0070Du;
                        opcode |= i << 6u;
                        goto Emit_R0At12Of4Hi22_MemBaseAt16_NoCond;
                      }
                    }
                  }

                  if (isConsecutive(2, o0.as<Reg>(), o1.as<Reg>(), o2.as<Reg>(), o3.as<Reg>())) {
                    if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16))) {
                      uint32_t i = o0.as<Vec>().as<Vec>().elementIndex();
                      if (i <= 0x3) {
                        opcode = 0xF4A0072Du;
                        opcode |= i << 6u;
                        goto Emit_R0At12Of4Hi22_MemBaseAt16_NoCond;
                      }
                    }
                  }
                }
              }
            }

            if (uint32_t(mem->offsetLo32()) == 16u) {
              if (mem->isPostIndex()) {
                if (isElementVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>(), o3.as<Vec>())) {
                  if (isConsecutive(1, o0.as<Reg>(), o1.as<Reg>(), o2.as<Reg>(), o3.as<Reg>())) {
                    if (isDtMultiple(dtBits, makeDtBits(DT::kS32, DT::kU32, DT::kF32))) {
                      uint32_t i = o0.as<Vec>().as<Vec>().elementIndex();
                      if (i <= 0x1) {
                        opcode = 0xF4A00B0Du;
                        opcode |= i << 7u;
                        goto Emit_R0At12Of4Hi22_MemBaseAt16_NoCond;
                      }
                    }
                  }

                  if (isConsecutive(2, o0.as<Reg>(), o1.as<Reg>(), o2.as<Reg>(), o3.as<Reg>())) {
                    if (isDtMultiple(dtBits, makeDtBits(DT::kS32, DT::kU32, DT::kF32))) {
                      uint32_t i = o0.as<Vec>().as<Vec>().elementIndex();
                      if (i <= 0x1) {
                        opcode = 0xF4A00B4Du;
                        opcode |= i << 7u;
                        goto Emit_R0At12Of4Hi22_MemBaseAt16_NoCond;
                      }
                    }
                  }
                }
              }
            }
          }

          if (mem->indexType() == RegType::kGp32) {
            if (!mem->offsetLo32()) {
              if ((mem->indexId() < 13u) || (mem->indexId() == 14u)) {
                if (mem->isPostIndex()) {
                  if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>(), o3.as<Vec>())) {
                    if (isConsecutive(1, o0.as<Reg>(), o1.as<Reg>(), o2.as<Reg>(), o3.as<Reg>())) {
                      if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16, DT::kS32, DT::kU32, DT::kF32, DT::kS8, DT::kU8))) {
                        opcode = 0xF4200000u;
                        opcode |= sz << 6u;
                        goto Emit_R0At12Of4Hi22_MemBaseAt16_MemUIndexAt0_NoCond;
                      }
                    }

                    if (isConsecutive(2, o0.as<Reg>(), o1.as<Reg>(), o2.as<Reg>(), o3.as<Reg>())) {
                      if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16, DT::kS32, DT::kU32, DT::kF32, DT::kS8, DT::kU8))) {
                        opcode = 0xF4200100u;
                        opcode |= sz << 6u;
                        goto Emit_R0At12Of4Hi22_MemBaseAt16_MemUIndexAt0_NoCond;
                      }
                    }
                  }

                  if (isElementVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>(), o3.as<Vec>())) {
                    if (isConsecutive(1, o0.as<Reg>(), o1.as<Reg>(), o2.as<Reg>(), o3.as<Reg>())) {
                      if (isDtMultiple(dtBits, makeDtBits(DT::kS8, DT::kU8))) {
                        uint32_t i = o0.as<Vec>().as<Vec>().elementIndex();
                        if (i <= 0x7) {
                          opcode = 0xF4A00300u;
                          opcode |= i << 5u;
                          goto Emit_R0At12Of4Hi22_MemBaseAt16_MemUIndexAt0_NoCond;
                        }
                      }

                      if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16))) {
                        uint32_t i = o0.as<Vec>().as<Vec>().elementIndex();
                        if (i <= 0x3) {
                          opcode = 0xF4A00700u;
                          opcode |= i << 6u;
                          goto Emit_R0At12Of4Hi22_MemBaseAt16_MemUIndexAt0_NoCond;
                        }
                      }

                      if (isDtMultiple(dtBits, makeDtBits(DT::kS32, DT::kU32, DT::kF32))) {
                        uint32_t i = o0.as<Vec>().as<Vec>().elementIndex();
                        if (i <= 0x1) {
                          opcode = 0xF4A00B00u;
                          opcode |= i << 7u;
                          goto Emit_R0At12Of4Hi22_MemBaseAt16_MemUIndexAt0_NoCond;
                        }
                      }
                    }

                    if (isConsecutive(2, o0.as<Reg>(), o1.as<Reg>(), o2.as<Reg>(), o3.as<Reg>())) {
                      if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16))) {
                        uint32_t i = o0.as<Vec>().as<Vec>().elementIndex();
                        if (i <= 0x3) {
                          opcode = 0xF4A00720u;
                          opcode |= i << 6u;
                          goto Emit_R0At12Of4Hi22_MemBaseAt16_MemUIndexAt0_NoCond;
                        }
                      }

                      if (isDtMultiple(dtBits, makeDtBits(DT::kS32, DT::kU32, DT::kF32))) {
                        uint32_t i = o0.as<Vec>().as<Vec>().elementIndex();
                        if (i <= 0x1) {
                          opcode = 0xF4A00B40u;
                          opcode |= i << 7u;
                          goto Emit_R0At12Of4Hi22_MemBaseAt16_MemUIndexAt0_NoCond;
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }

      break;
    }

    case 110: {
      // Instruction 'vld4r'.
      uint32_t sz = szFromDt(dtBits);
      if (sgn.test<kOpRegD, kOpRegD, kOpRegD, kOpRegD, kOpMemB>()) {
        mem = &o4.as<Mem>();

        if (mem->baseId() < 15u) {
          if (!mem->hasIndex()) {
            if (!mem->offsetLo32()) {
              if (mem->isFixedOffset()) {
                if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>(), o3.as<Vec>())) {
                  if (isConsecutive(1, o0.as<Reg>(), o1.as<Reg>(), o2.as<Reg>(), o3.as<Reg>())) {
                    if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16, DT::kS32, DT::kU32, DT::kF32, DT::kS8, DT::kU8))) {
                      opcode = 0xF4A00F0Fu;
                      opcode |= sz << 6u;
                      goto Emit_R0At12Of4Hi22_MemBaseAt16_NoCond;
                    }
                  }

                  if (isConsecutive(2, o0.as<Reg>(), o1.as<Reg>(), o2.as<Reg>(), o3.as<Reg>())) {
                    if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16, DT::kS32, DT::kU32, DT::kF32, DT::kS8, DT::kU8))) {
                      opcode = 0xF4A00F2Fu;
                      opcode |= sz << 6u;
                      goto Emit_R0At12Of4Hi22_MemBaseAt16_NoCond;
                    }
                  }
                }
              }
            }

            if (uint32_t(mem->offsetLo32()) == (4u << sz)) {
              if (mem->isPostIndex()) {
                if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>(), o3.as<Vec>())) {
                  if (isConsecutive(1, o0.as<Reg>(), o1.as<Reg>(), o2.as<Reg>(), o3.as<Reg>())) {
                    if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16, DT::kS32, DT::kU32, DT::kF32, DT::kS8, DT::kU8))) {
                      opcode = 0xF4A00F0Du;
                      opcode |= sz << 6u;
                      goto Emit_R0At12Of4Hi22_MemBaseAt16_NoCond;
                    }
                  }

                  if (isConsecutive(2, o0.as<Reg>(), o1.as<Reg>(), o2.as<Reg>(), o3.as<Reg>())) {
                    if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16, DT::kS32, DT::kU32, DT::kF32, DT::kS8, DT::kU8))) {
                      opcode = 0xF4A00F2Du;
                      opcode |= sz << 6u;
                      goto Emit_R0At12Of4Hi22_MemBaseAt16_NoCond;
                    }
                  }
                }
              }
            }
          }

          if (mem->indexType() == RegType::kGp32) {
            if (!mem->offsetLo32()) {
              if ((mem->indexId() < 13u) || (mem->indexId() == 14u)) {
                if (mem->isPostIndex()) {
                  if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>(), o3.as<Vec>())) {
                    if (isConsecutive(1, o0.as<Reg>(), o1.as<Reg>(), o2.as<Reg>(), o3.as<Reg>())) {
                      if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16, DT::kS32, DT::kU32, DT::kF32, DT::kS8, DT::kU8))) {
                        opcode = 0xF4A00F00u;
                        opcode |= sz << 6u;
                        goto Emit_R0At12Of4Hi22_MemBaseAt16_MemUIndexAt0_NoCond;
                      }
                    }

                    if (isConsecutive(2, o0.as<Reg>(), o1.as<Reg>(), o2.as<Reg>(), o3.as<Reg>())) {
                      if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16, DT::kS32, DT::kU32, DT::kF32, DT::kS8, DT::kU8))) {
                        opcode = 0xF4A00F20u;
                        opcode |= sz << 6u;
                        goto Emit_R0At12Of4Hi22_MemBaseAt16_MemUIndexAt0_NoCond;
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }

      break;
    }

    case 111: {
      static const uint32_t opcodeTable[] = {
        0x0D300A00u, 0x0D300A00u, 0x0D300B00u, 0x0D300B00u, // Instruction 'vldmdb'.
        0x0D200A00u, 0x0D200A00u, 0x0D200B00u, 0x0D200B00u  // Instruction 'vstmdb'.
      };

      const uint32_t* opcodeTablePtr = opcodeTable + uint32_t(idr.index) * 4u;

      if (sgn.test<kOpMemB, kOpRegListS>()) {
        mem = &o0.as<Mem>();
        const GpList& regList = o1.as<GpList>();

        if (mem->baseId() <= 15u) {
          if (!mem->hasIndex()) {
            if (!mem->offsetLo32()) {
              if (mem->isPreIndex()) {
                if (isDtMultiple(dtBits, makeDtBits(DT::kS32, DT::kU32, DT::kF32))) {
                  VecSListImmEncode enc0;
                  if (enc0.init(regList)) {
                    opcode = opcodeTablePtr[0];
                    opcode |= enc0.immCount() << 0u;
                    opcode |= (enc0.immVd() & 0x1Eu) << 11u;
                    opcode |= (enc0.immVd() & 0x1u) << 22u;
                    goto Emit_MemBaseAt16_Cond;
                  }
                }
              }
            }
          }
        }

        if (mem->baseId() == 15u && !mem->offsetLo32()) {
          if (mem->isPreIndex()) {
            if (isDtMultiple(dtBits, makeDtBits(DT::kS32, DT::kU32, DT::kF32))) {
              VecSListImmEncode enc1;
              if (enc1.init(regList)) {
                opcode = opcodeTablePtr[1];
                opcode |= enc1.immCount() << 0u;
                opcode |= (enc1.immVd() & 0x1Eu) << 11u;
                opcode |= (enc1.immVd() & 0x1u) << 22u;
                goto Emit_MemBaseAt16_Cond;
              }
            }
          }
        }
      }

      if (sgn.test<kOpMemB, kOpRegListD>()) {
        mem = &o0.as<Mem>();
        const GpList& regList = o1.as<GpList>();

        if (mem->baseId() <= 15u) {
          if (!mem->hasIndex()) {
            if (!mem->offsetLo32()) {
              if (mem->isPreIndex()) {
                if (isDtMultiple(dtBits, makeDtBits(DT::kS64, DT::kU64, DT::kF64))) {
                  VecDListImmEncode enc2;
                  if (enc2.init(regList)) {
                    opcode = opcodeTablePtr[2];
                    opcode |= enc2.immCount() << 0u;
                    opcode |= (enc2.immVd() & 0xFu) << 12u;
                    opcode |= (enc2.immVd() & 0x10u) << 18u;
                    goto Emit_MemBaseAt16_Cond;
                  }
                }
              }
            }
          }
        }

        if (mem->baseId() == 15u && !mem->offsetLo32()) {
          if (mem->isPreIndex()) {
            if (isDtMultiple(dtBits, makeDtBits(DT::kS64, DT::kU64, DT::kF64))) {
              VecDListImmEncode enc3;
              if (enc3.init(regList)) {
                opcode = opcodeTablePtr[3];
                opcode |= enc3.immCount() << 0u;
                opcode |= (enc3.immVd() & 0xFu) << 12u;
                opcode |= (enc3.immVd() & 0x10u) << 18u;
                goto Emit_MemBaseAt16_Cond;
              }
            }
          }
        }
      }

      break;
    }

    case 112: {
      static const uint32_t opcodeTable[] = {
        0x0C900A00u, 0x0C900A00u, 0x0C900B00u, 0x0C900B00u, // Instruction 'vldmia'.
        0x0C800A00u, 0x0C800A00u, 0x0C800B00u, 0x0C800B00u  // Instruction 'vstmia'.
      };

      const uint32_t* opcodeTablePtr = opcodeTable + uint32_t(idr.index) * 4u;

      if (sgn.test<kOpMemB, kOpRegListS>()) {
        mem = &o0.as<Mem>();
        const GpList& regList = o1.as<GpList>();

        if (mem->baseId() < 15u) {
          if (!mem->hasIndex()) {
            if (!mem->offsetLo32()) {
              if (!mem->isPostIndex()) {
                if (isDtMultiple(dtBits, makeDtBits(DT::kS32, DT::kU32, DT::kF32))) {
                  VecSListImmEncode enc0;
                  if (enc0.init(regList)) {
                    opcode = opcodeTablePtr[0];
                    opcode |= enc0.immCount() << 0u;
                    opcode |= (enc0.immVd() & 0x1Eu) << 11u;
                    opcode |= (enc0.immVd() & 0x1u) << 22u;
                    goto Emit_MemBaseAt16W21_Cond;
                  }
                }
              }
            }
          }
        }

        if (mem->baseId() == 15u && !mem->offsetLo32()) {
          if (mem->isFixedOffset()) {
            if (isDtMultiple(dtBits, makeDtBits(DT::kS32, DT::kU32, DT::kF32))) {
              VecSListImmEncode enc2;
              if (enc2.init(regList)) {
                opcode = opcodeTablePtr[1];
                opcode |= enc2.immCount() << 0u;
                opcode |= (enc2.immVd() & 0x1Eu) << 11u;
                opcode |= (enc2.immVd() & 0x1u) << 22u;
                goto Emit_MemBaseAt16_Cond;
              }
            }
          }
        }
      }

      if (sgn.test<kOpMemB, kOpRegListD>()) {
        mem = &o0.as<Mem>();
        const GpList& regList = o1.as<GpList>();

        if (mem->baseId() < 15u) {
          if (!mem->hasIndex()) {
            if (!mem->offsetLo32()) {
              if (!mem->isPostIndex()) {
                if (isDtMultiple(dtBits, makeDtBits(DT::kS64, DT::kU64, DT::kF64))) {
                  VecDListImmEncode enc1;
                  if (enc1.init(regList)) {
                    opcode = opcodeTablePtr[2];
                    opcode |= enc1.immCount() << 0u;
                    opcode |= (enc1.immVd() & 0xFu) << 12u;
                    opcode |= (enc1.immVd() & 0x10u) << 18u;
                    goto Emit_MemBaseAt16W21_Cond;
                  }
                }
              }
            }
          }
        }

        if (mem->baseId() == 15u && !mem->offsetLo32()) {
          if (mem->isFixedOffset()) {
            if (isDtMultiple(dtBits, makeDtBits(DT::kS64, DT::kU64, DT::kF64))) {
              VecDListImmEncode enc3;
              if (enc3.init(regList)) {
                opcode = opcodeTablePtr[3];
                opcode |= enc3.immCount() << 0u;
                opcode |= (enc3.immVd() & 0xFu) << 12u;
                opcode |= (enc3.immVd() & 0x10u) << 18u;
                goto Emit_MemBaseAt16_Cond;
              }
            }
          }
        }
      }

      break;
    }

    case 113: {
      static const uint32_t opcodeTable[] = {
        0x0D100A00u, 0xED100900u, 0x0D100A00u, 0xED100900u, 0x0D100B00u, 0x0D100B00u, // Instruction 'vldr'.
        0x0D000A00u, 0xED000900u, 0x0D000A00u, 0xED000900u, 0x0D000B00u, 0x0D000B00u  // Instruction 'vstr'.
      };

      const uint32_t* opcodeTablePtr = opcodeTable + uint32_t(idr.index) * 6u;

      if (sgn.test<kOpRegS, kOpMemB>()) {
        mem = &o1.as<Mem>();

        if (mem->baseId() <= 15u) {
          if (!mem->hasIndex()) {
            if (checkSOffset(mem->offsetLo32(), 8, 2)) {
              if (mem->isFixedOffset()) {
                if (isPureVec(o0.as<Vec>())) {
                  if (isDtMultiple(dtBits, makeDtBits(DT::kS32, DT::kU32, DT::kF32))) {
                    opcode = opcodeTablePtr[0];
                    opcode |= uBitFromDt(dtBits) << 23u;
                    goto Emit_R0At12Of4Lo22_MemBaseAt16_SOffAt0Of8Mul4_Cond;
                  }
                }
              }
            }

            if (checkSOffset(mem->offsetLo32(), 8, 1)) {
              if (mem->isFixedOffset()) {
                if (isPureVec(o0.as<Vec>())) {
                  if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16))) {
                    opcode = opcodeTablePtr[1];
                    opcode |= uBitFromDt(dtBits) << 23u;
                    goto Emit_R0At12Of4Lo22_MemBaseAt16_SOffAt0Of8Mul2_NoCond;
                  }
                }
              }
            }
          }
        }
      }

      if (sgn.test<kOpRegS, kOpMemAny>()) {
        mem = &o1.as<Mem>();

        if (isMemPCRel(*mem)) {
          if (mem->isFixedOffset()) {
            if (isPureVec(o0.as<Vec>())) {
              if (isDtMultiple(dtBits, makeDtBits(DT::kS32, DT::kU32, DT::kF32))) {
                opcode = opcodeTablePtr[2];
                opcode |= 0xFu << 16;
                offsetFormat.resetToImmValue(OffsetType::kAArch32_U23_SignedOffset, 4, 0, 8, 0);
                goto Emit_R0At12Of4Lo22_MemPCRel_Cond;
              }

              if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16))) {
                opcode = opcodeTablePtr[3];
                opcode |= 0xFu << 16;
                offsetFormat.resetToImmValue(OffsetType::kAArch32_U23_SignedOffset, 4, 0, 8, 0);
                goto Emit_R0At12Of4Lo22_MemPCRel_NoCond;
              }
            }
          }
        }
      }

      if (sgn.test<kOpRegD, kOpMemB>()) {
        mem = &o1.as<Mem>();

        if (mem->baseId() <= 15u) {
          if (!mem->hasIndex()) {
            if (checkSOffset(mem->offsetLo32(), 8, 2)) {
              if (mem->isFixedOffset()) {
                if (isPureVec(o0.as<Vec>())) {
                  if (isDtMultiple(dtBits, makeDtBits(DT::kS64, DT::kU64, DT::kF64))) {
                    opcode = opcodeTablePtr[4];
                    opcode |= uBitFromDt(dtBits) << 23u;
                    goto Emit_R0At12Of4Hi22_MemBaseAt16_SOffAt0Of8Mul4_Cond;
                  }
                }
              }
            }
          }
        }
      }

      if (sgn.test<kOpRegD, kOpMemAny>()) {
        mem = &o1.as<Mem>();

        if (isMemPCRel(*mem)) {
          if (mem->isFixedOffset()) {
            if (isPureVec(o0.as<Vec>())) {
              if (isDtMultiple(dtBits, makeDtBits(DT::kS64, DT::kU64, DT::kF64))) {
                opcode = opcodeTablePtr[5];
                opcode |= 0xFu << 16;
                offsetFormat.resetToImmValue(OffsetType::kAArch32_U23_SignedOffset, 4, 0, 8, 0);
                goto Emit_R0At12Of4Hi22_MemPCRel_Cond;
              }
            }
          }
        }
      }

      break;
    }

    case 114: {
      static const uint32_t opcodeTable[] = {
        0xFE800900u, 0xFE800A00u, 0xFE800B00u, 0xF3100F10u, 0xF3000F10u, 0xF3100F50u, 0xF3000F50u, // Instruction 'vmaxnm'.
        0xFE800940u, 0xFE800A40u, 0xFE800B40u, 0xF3300F10u, 0xF3200F10u, 0xF3300F50u, 0xF3200F50u  // Instruction 'vminnm'.
      };

      const uint32_t* opcodeTablePtr = opcodeTable + uint32_t(idr.index) * 7u;

      if (sgn.test<kOpRegS, kOpRegS, kOpRegS>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>())) {
          if (isDtSingle(dtBits, DT::kF16)) {
            opcode = opcodeTablePtr[0];
            goto Emit_R0At12Of4Lo22_R1At16Of4Lo7_R2At0Of4Lo5_NoCond;
          }

          if (isDtSingle(dtBits, DT::kF32)) {
            opcode = opcodeTablePtr[1];
            goto Emit_R0At12Of4Lo22_R1At16Of4Lo7_R2At0Of4Lo5_NoCond;
          }
        }
      }

      if (sgn.test<kOpRegD, kOpRegD, kOpRegD>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>())) {
          if (isDtSingle(dtBits, DT::kF64)) {
            opcode = opcodeTablePtr[2];
            goto Emit_R0At12Of4Hi22_R1At16Of4Hi7_R2At0Of4Hi5_NoCond;
          }

          if (isDtSingle(dtBits, DT::kF16)) {
            opcode = opcodeTablePtr[3];
            goto Emit_R0At12Of4Hi22_R1At16Of4Hi7_R2At0Of4Hi5_NoCond;
          }

          if (isDtSingle(dtBits, DT::kF32)) {
            opcode = opcodeTablePtr[4];
            goto Emit_R0At12Of4Hi22_R1At16Of4Hi7_R2At0Of4Hi5_NoCond;
          }
        }
      }

      if (sgn.test<kOpRegQ, kOpRegQ, kOpRegQ>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>())) {
          if (isDtSingle(dtBits, DT::kF16)) {
            opcode = opcodeTablePtr[5];
            goto Emit_Q0At12Of4Hi22_Q1At16Of4Hi7_Q2At0Of4Hi5_NoCond;
          }

          if (isDtSingle(dtBits, DT::kF32)) {
            opcode = opcodeTablePtr[6];
            goto Emit_Q0At12Of4Hi22_Q1At16Of4Hi7_Q2At0Of4Hi5_NoCond;
          }
        }
      }

      break;
    }

    case 115: {
      static const uint32_t opcodeTable[] = {
        0x0E000A00u, 0xEE000900u, 0x0E000B00u, 0xF2100D10u, 0xF2000D10u, 0xF2000900u, 0xF2900140u, 0xF2A00140u, 0xF2900040u, 0xF2A00040u, 0xF2100D50u, 0xF2000D50u, 0xF2000940u, 0xF3900140u, 0xF3A00140u, 0xF3900040u, 0xF3A00040u, // Instruction 'vmla'.
        0x0E000A40u, 0xEE000940u, 0x0E000B40u, 0xF2300D10u, 0xF2200D10u, 0xF3000900u, 0xF2900540u, 0xF2A00540u, 0xF2900440u, 0xF2A00440u, 0xF2300D50u, 0xF2200D50u, 0xF3000940u, 0xF3900540u, 0xF3A00540u, 0xF3900440u, 0xF3A00440u  // Instruction 'vmls'.
      };

      const uint32_t* opcodeTablePtr = opcodeTable + uint32_t(idr.index) * 17u;

      uint32_t sz = szFromDt(dtBits);
      if (sgn.test<kOpRegS, kOpRegS, kOpRegS>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>())) {
          if (isDtSingle(dtBits, DT::kF32)) {
            opcode = opcodeTablePtr[0];
            goto Emit_R0At12Of4Lo22_R1At16Of4Lo7_R2At0Of4Lo5_Cond;
          }

          if (isDtSingle(dtBits, DT::kF16)) {
            opcode = opcodeTablePtr[1];
            goto Emit_R0At12Of4Lo22_R1At16Of4Lo7_R2At0Of4Lo5_NoCond;
          }
        }
      }

      if (sgn.test<kOpRegD, kOpRegD, kOpRegD>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>())) {
          if (isDtSingle(dtBits, DT::kF64)) {
            opcode = opcodeTablePtr[2];
            goto Emit_R0At12Of4Hi22_R1At16Of4Hi7_R2At0Of4Hi5_Cond;
          }

          if (isDtSingle(dtBits, DT::kF16)) {
            opcode = opcodeTablePtr[3];
            goto Emit_R0At12Of4Hi22_R1At16Of4Hi7_R2At0Of4Hi5_NoCond;
          }

          if (isDtSingle(dtBits, DT::kF32)) {
            opcode = opcodeTablePtr[4];
            goto Emit_R0At12Of4Hi22_R1At16Of4Hi7_R2At0Of4Hi5_NoCond;
          }

          if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kS32, DT::kS8, DT::kU16, DT::kU32, DT::kU8))) {
            opcode = opcodeTablePtr[5];
            opcode |= sz << 20u;
            goto Emit_R0At12Of4Hi22_R1At16Of4Hi7_R2At0Of4Hi5_NoCond;
          }
        }

        if (isPureVec(o0.as<Vec>(), o1.as<Vec>()) && isElementVec(o2.as<Vec>())) {
          if (isDtSingle(dtBits, DT::kF16)) {
            uint32_t i = o2.as<Vec>().as<Vec>().elementIndex();
            if (i <= 0x3) {
              opcode = opcodeTablePtr[6];
              opcode |= (i & 0x1u) << 3u;
              opcode |= (i & 0x2u) << 4u;
              goto Emit_R0At12Of4Hi22_R1At16Of4Hi7_R2At0Of3_NoCond;
            }
          }

          if (isDtSingle(dtBits, DT::kF32)) {
            uint32_t i = o2.as<Vec>().as<Vec>().elementIndex();
            if (i <= 0x1) {
              opcode = opcodeTablePtr[7];
              opcode |= i << 5u;
              goto Emit_R0At12Of4Hi22_R1At16Of4Hi7_R2At0Of4_NoCond;
            }
          }

          if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16))) {
            uint32_t i = o2.as<Vec>().as<Vec>().elementIndex();
            if (i <= 0x3) {
              opcode = opcodeTablePtr[8];
              opcode |= (i & 0x1u) << 3u;
              opcode |= (i & 0x2u) << 4u;
              goto Emit_R0At12Of4Hi22_R1At16Of4Hi7_R2At0Of3_NoCond;
            }
          }

          if (isDtMultiple(dtBits, makeDtBits(DT::kS32, DT::kU32))) {
            uint32_t i = o2.as<Vec>().as<Vec>().elementIndex();
            if (i <= 0x1) {
              opcode = opcodeTablePtr[9];
              opcode |= i << 5u;
              goto Emit_R0At12Of4Hi22_R1At16Of4Hi7_R2At0Of4_NoCond;
            }
          }
        }
      }

      if (sgn.test<kOpRegQ, kOpRegQ, kOpRegQ>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>())) {
          if (isDtSingle(dtBits, DT::kF16)) {
            opcode = opcodeTablePtr[10];
            goto Emit_Q0At12Of4Hi22_Q1At16Of4Hi7_Q2At0Of4Hi5_NoCond;
          }

          if (isDtSingle(dtBits, DT::kF32)) {
            opcode = opcodeTablePtr[11];
            goto Emit_Q0At12Of4Hi22_Q1At16Of4Hi7_Q2At0Of4Hi5_NoCond;
          }

          if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kS32, DT::kS8, DT::kU16, DT::kU32, DT::kU8))) {
            opcode = opcodeTablePtr[12];
            opcode |= sz << 20u;
            goto Emit_Q0At12Of4Hi22_Q1At16Of4Hi7_Q2At0Of4Hi5_NoCond;
          }
        }
      }

      if (sgn.test<kOpRegQ, kOpRegQ, kOpRegD>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>()) && isElementVec(o2.as<Vec>())) {
          if (isDtSingle(dtBits, DT::kF16)) {
            uint32_t i = o2.as<Vec>().as<Vec>().elementIndex();
            if (i <= 0x3) {
              opcode = opcodeTablePtr[13];
              opcode |= (i & 0x1u) << 3u;
              opcode |= (i & 0x2u) << 4u;
              goto Emit_Q0At12Of4Hi22_Q1At16Of4Hi7_R2At0Of3_NoCond;
            }
          }

          if (isDtSingle(dtBits, DT::kF32)) {
            uint32_t i = o2.as<Vec>().as<Vec>().elementIndex();
            if (i <= 0x1) {
              opcode = opcodeTablePtr[14];
              opcode |= i << 5u;
              goto Emit_Q0At12Of4Hi22_Q1At16Of4Hi7_R2At0Of4_NoCond;
            }
          }

          if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16))) {
            uint32_t i = o2.as<Vec>().as<Vec>().elementIndex();
            if (i <= 0x3) {
              opcode = opcodeTablePtr[15];
              opcode |= (i & 0x1u) << 3u;
              opcode |= (i & 0x2u) << 4u;
              goto Emit_Q0At12Of4Hi22_Q1At16Of4Hi7_R2At0Of3_NoCond;
            }
          }

          if (isDtMultiple(dtBits, makeDtBits(DT::kS32, DT::kU32))) {
            uint32_t i = o2.as<Vec>().as<Vec>().elementIndex();
            if (i <= 0x1) {
              opcode = opcodeTablePtr[16];
              opcode |= i << 5u;
              goto Emit_Q0At12Of4Hi22_Q1At16Of4Hi7_R2At0Of4_NoCond;
            }
          }
        }
      }

      break;
    }

    case 116: {
      static const uint32_t opcodeTable[] = {
        0xF2800800u, 0xF2900240u, 0xF2A00240u, // Instruction 'vmlal'.
        0xF2800A00u, 0xF2900640u, 0xF2A00640u  // Instruction 'vmlsl'.
      };

      const uint32_t* opcodeTablePtr = opcodeTable + uint32_t(idr.index) * 3u;

      uint32_t sz = szFromDt(dtBits);
      if (sgn.test<kOpRegQ, kOpRegD, kOpRegD>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>())) {
          if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kS32, DT::kS8, DT::kU16, DT::kU32, DT::kU8))) {
            opcode = opcodeTablePtr[0];
            opcode |= sz << 20u;
            opcode |= uBitFromDt(dtBits) << 24u;
            goto Emit_Q0At12Of4Hi22_R1At16Of4Hi7_R2At0Of4Hi5_NoCond;
          }
        }

        if (isPureVec(o0.as<Vec>(), o1.as<Vec>()) && isElementVec(o2.as<Vec>())) {
          if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16))) {
            uint32_t i = o2.as<Vec>().as<Vec>().elementIndex();
            if (i <= 0x3) {
              opcode = opcodeTablePtr[1];
              opcode |= (i & 0x1u) << 3u;
              opcode |= (i & 0x2u) << 4u;
              opcode |= uBitFromDt(dtBits) << 24u;
              goto Emit_Q0At12Of4Hi22_R1At16Of4Hi7_R2At0Of3_NoCond;
            }
          }

          if (isDtMultiple(dtBits, makeDtBits(DT::kS32, DT::kU32))) {
            uint32_t i = o2.as<Vec>().as<Vec>().elementIndex();
            if (i <= 0x1) {
              opcode = opcodeTablePtr[2];
              opcode |= i << 5u;
              opcode |= uBitFromDt(dtBits) << 24u;
              goto Emit_Q0At12Of4Hi22_R1At16Of4Hi7_R2At0Of4_NoCond;
            }
          }
        }
      }

      break;
    }

    case 117: {
      // Instruction 'vmmla'.
      if (sgn.test<kOpRegQ, kOpRegQ, kOpRegQ>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>())) {
          if (isDtSingle(dtBits, DT::kBF16)) {
            opcode = 0xFC000C40u;
            goto Emit_Q0At12Of4Hi22_Q1At16Of4Hi7_Q2At0Of4Hi5_NoCond;
          }
        }
      }

      break;
    }

    case 118: {
      // Instruction 'vmov'.
      uint32_t sz = szFromDt(dtBits);
      if (sgn.test<kOpRegS, kOpRegS>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>())) {
          if (isDtSingle(dtBits, DT::kF32)) {
            opcode = 0x0EB00A40u;
            goto Emit_R0At12Of4Lo22_R1At0Of4Lo5_Cond;
          }
        }
      }

      if (sgn.test<kOpRegD, kOpRegD>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>())) {
          if (isDtSingle(dtBits, DT::kF64)) {
            opcode = 0x0EB00B40u;
            goto Emit_R0At12Of4Hi22_R1At0Of4Hi5_Cond;
          }
          opcode = 0xF2200110u;
          goto Emit_R0At12Of4Hi22_R1At0Of4At16Of4Hi5Hi7_NoCond;
        }
      }

      if (sgn.test<kOpRegS, kOpImmI>() || sgn.test<kOpRegS, kOpImmF>()) {
        if (isPureVec(o0.as<Vec>())) {
          if (isDtSingle(dtBits, DT::kF32)) {
            VecVFPImmEncode enc0;
            if (enc0.init(o1.as<Imm>())) {
              opcode = 0x0EB00A00u;
              opcode |= (enc0.imm() & 0xFu) << 0u;
              opcode |= (enc0.imm() & 0xF0u) << 12u;
              goto Emit_R0At12Of4Lo22_Cond;
            }
          }

          if (isDtSingle(dtBits, DT::kF16)) {
            VecVFPImmEncode enc2;
            if (enc2.init(o1.as<Imm>())) {
              opcode = 0xEEB00900u;
              opcode |= (enc2.imm() & 0xFu) << 0u;
              opcode |= (enc2.imm() & 0xF0u) << 12u;
              goto Emit_R0At12Of4Lo22_NoCond;
            }
          }
        }
      }

      if (sgn.test<kOpRegD, kOpImmI>() || sgn.test<kOpRegD, kOpImmF>()) {
        if (isPureVec(o0.as<Vec>())) {
          if (isDtSingle(dtBits, DT::kF64)) {
            VecVFPImmEncode enc1;
            if (enc1.init(o1.as<Imm>())) {
              opcode = 0x0EB00B00u;
              opcode |= (enc1.imm() & 0xFu) << 0u;
              opcode |= (enc1.imm() & 0xF0u) << 12u;
              goto Emit_R0At12Of4Hi22_Cond;
            }
          }
        }
      }

      if (sgn.test<kOpRegR, kOpRegS>()) {
        if (isPureVec(o1.as<Vec>())) {
          if (isDtSingle(dtBits, DT::kF16)) {
            opcode = 0xEE100910u;
            goto Emit_R0At12Of4_R1At16Of4Lo7_NoCond;
          }
          opcode = 0x0E100A10u;
          goto Emit_R0At12Of4_R1At16Of4Lo7_Cond;
        }
      }

      if (sgn.test<kOpRegS, kOpRegR>()) {
        if (isPureVec(o0.as<Vec>())) {
          if (isDtSingle(dtBits, DT::kF16)) {
            opcode = 0xEE000910u;
            goto Emit_R0At16Of4Lo7_R1At12Of4_NoCond;
          }
          opcode = 0x0E000A10u;
          goto Emit_R0At16Of4Lo7_R1At12Of4_Cond;
        }
      }

      if (sgn.test<kOpRegR, kOpRegR, kOpRegS, kOpRegS>()) {
        if (isPureVec(o2.as<Vec>(), o3.as<Vec>())) {
          if (isConsecutive(1, o2.as<Reg>(), o3.as<Reg>())) {
            opcode = 0x0C500A10u;
            goto Emit_R0At12Of4_R1At16Of4_R2At0Of4Lo5_Cond;
          }
        }
      }

      if (sgn.test<kOpRegS, kOpRegS, kOpRegR, kOpRegR>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>())) {
          if (isConsecutive(1, o0.as<Reg>(), o1.as<Reg>())) {
            opcode = 0x0C400A10u;
            goto Emit_R0At0Of4Lo5_R2At12Of4_R3At16Of4_Cond;
          }
        }
      }

      if (sgn.test<kOpRegR, kOpRegR, kOpRegD>()) {
        if (isPureVec(o2.as<Vec>())) {
          opcode = 0x0C500B10u;
          goto Emit_R0At12Of4_R1At16Of4_R2At0Of4Hi5_Cond;
        }
      }

      if (sgn.test<kOpRegD, kOpRegR, kOpRegR>()) {
        if (isPureVec(o0.as<Vec>())) {
          opcode = 0x0C400B10u;
          goto Emit_R0At0Of4Hi5_R1At12Of4_R2At16Of4_Cond;
        }
      }

      if (sgn.test<kOpRegD, kOpRegR>()) {
        if (isElementVec(o0.as<Vec>())) {
          if (isDtMultiple(dtBits, makeDtBits(DT::kS8, DT::kU8))) {
            uint32_t i = o0.as<Vec>().as<Vec>().elementIndex();
            if (i <= 0x7) {
              opcode = 0x0E400B10u;
              opcode |= (i & 0x3u) << 5u;
              opcode |= (i & 0x4u) << 19u;
              goto Emit_R0At16Of4Hi7_R1At12Of4_Cond;
            }
          }

          if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16))) {
            uint32_t i = o0.as<Vec>().as<Vec>().elementIndex();
            if (i <= 0x3) {
              opcode = 0x0E000B30u;
              opcode |= (i & 0x1u) << 6u;
              opcode |= (i & 0x2u) << 20u;
              goto Emit_R0At16Of4Hi7_R1At12Of4_Cond;
            }
          }

          if (isDtMultiple(dtBits, makeDtBits(DT::kS32, DT::kU32))) {
            uint32_t i = o0.as<Vec>().as<Vec>().elementIndex();
            if (i <= 0x1) {
              opcode = 0x0E000B10u;
              opcode |= i << 21u;
              goto Emit_R0At16Of4Hi7_R1At12Of4_Cond;
            }
          }
        }
      }

      if (sgn.test<kOpRegR, kOpRegD>()) {
        if (isElementVec(o1.as<Vec>())) {
          if (isDtMultiple(dtBits, makeDtBits(DT::kS8, DT::kU8))) {
            uint32_t i = o1.as<Vec>().as<Vec>().elementIndex();
            if (i <= 0x7) {
              opcode = 0x0E500B10u;
              opcode |= (i & 0x3u) << 5u;
              opcode |= (i & 0x4u) << 19u;
              opcode |= uBitFromDt(dtBits) << 23u;
              goto Emit_R0At12Of4_R1At16Of4Hi7_Cond;
            }
          }

          if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16))) {
            uint32_t i = o1.as<Vec>().as<Vec>().elementIndex();
            if (i <= 0x3) {
              opcode = 0x0E100B30u;
              opcode |= (i & 0x1u) << 6u;
              opcode |= (i & 0x2u) << 20u;
              opcode |= uBitFromDt(dtBits) << 23u;
              goto Emit_R0At12Of4_R1At16Of4Hi7_Cond;
            }
          }

          if (isDtMultiple(dtBits, makeDtBits(DT::kS32, DT::kU32))) {
            uint32_t i = o1.as<Vec>().as<Vec>().elementIndex();
            if (i <= 0x1) {
              opcode = 0x0E100B10u;
              opcode |= i << 21u;
              goto Emit_R0At12Of4_R1At16Of4Hi7_Cond;
            }
          }
        }
      }

      if (sgn.test<kOpRegQ, kOpRegQ>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>())) {
          opcode = 0xF2200150u;
          goto Emit_Q0At12Of4Hi22_Q1At0Of4At16Of4Hi5Hi7_NoCond;
        }
      }

      if (sgn.test<kOpRegD, kOpImmI>()) {
        if (isPureVec(o0.as<Vec>())) {
          if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kS32, DT::kS64, DT::kS8, DT::kU16, DT::kU32, DT::kU64, DT::kU8))) {
            VecMovImmEncode enc3;
            if (enc3.init(sz, 0u, o1.as<Imm>())) {
              opcode = 0xF2800010u;
              opcode |= (enc3.imm() & 0xFu) << 0u;
              opcode |= (enc3.imm() & 0x70u) << 12u;
              opcode |= (enc3.imm() & 0x80u) << 17u;
              opcode |= enc3.op() << 5u;
              opcode |= enc3.cmode() << 8u;
              goto Emit_R0At12Of4Hi22_NoCond;
            }
          }
        }
      }

      if (sgn.test<kOpRegQ, kOpImmI>()) {
        if (isPureVec(o0.as<Vec>())) {
          if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kS32, DT::kS64, DT::kS8, DT::kU16, DT::kU32, DT::kU64, DT::kU8))) {
            VecMovImmEncode enc4;
            if (enc4.init(sz, 0u, o1.as<Imm>())) {
              opcode = 0xF2800050u;
              opcode |= (enc4.imm() & 0xFu) << 0u;
              opcode |= (enc4.imm() & 0x70u) << 12u;
              opcode |= (enc4.imm() & 0x80u) << 17u;
              opcode |= enc4.op() << 5u;
              opcode |= enc4.cmode() << 8u;
              goto Emit_Q0At12Of4Hi22_NoCond;
            }
          }
        }
      }

      break;
    }

    case 119: {
      // Instruction 'vmovl'.
      if (sgn.test<kOpRegQ, kOpRegD>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>())) {
          if (isDtMultiple(dtBits, makeDtBits(DT::kS8, DT::kU8))) {
            opcode = 0xF2880A10u;
            opcode |= uBitFromDt(dtBits) << 24u;
            goto Emit_Q0At12Of4Hi22_R1At0Of4Hi5_NoCond;
          }

          if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16))) {
            opcode = 0xF2900A10u;
            opcode |= uBitFromDt(dtBits) << 24u;
            goto Emit_Q0At12Of4Hi22_R1At0Of4Hi5_NoCond;
          }

          if (isDtMultiple(dtBits, makeDtBits(DT::kS32, DT::kU32))) {
            opcode = 0xF2A00A10u;
            opcode |= uBitFromDt(dtBits) << 24u;
            goto Emit_Q0At12Of4Hi22_R1At0Of4Hi5_NoCond;
          }
        }
      }

      break;
    }

    case 120: {
      // Instruction 'vmovn'.
      uint32_t sz = szFromDt(dtBits);
      if (sgn.test<kOpRegD, kOpRegQ>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>())) {
          if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kS32, DT::kS64, DT::kU16, DT::kU32, DT::kU64))) {
            opcode = 0xF3B20200u;
            opcode |= (sz - 1) << 18u;
            goto Emit_R0At12Of4Hi22_Q1At0Of4Hi5_NoCond;
          }
        }
      }

      break;
    }

    case 121: {
      // Instruction 'vmul'.
      uint32_t sz = szFromDt(dtBits);
      if (sgn.test<kOpRegS, kOpRegS, kOpRegS>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>())) {
          if (isDtSingle(dtBits, DT::kF32)) {
            opcode = 0x0E200A00u;
            goto Emit_R0At12Of4Lo22_R1At16Of4Lo7_R2At0Of4Lo5_Cond;
          }

          if (isDtSingle(dtBits, DT::kF16)) {
            opcode = 0xEE200900u;
            goto Emit_R0At12Of4Lo22_R1At16Of4Lo7_R2At0Of4Lo5_NoCond;
          }
        }
      }

      if (sgn.test<kOpRegD, kOpRegD, kOpRegD>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>())) {
          if (isDtSingle(dtBits, DT::kF64)) {
            opcode = 0x0E200B00u;
            goto Emit_R0At12Of4Hi22_R1At16Of4Hi7_R2At0Of4Hi5_Cond;
          }

          if (isDtSingle(dtBits, DT::kF16)) {
            opcode = 0xF3100D10u;
            goto Emit_R0At12Of4Hi22_R1At16Of4Hi7_R2At0Of4Hi5_NoCond;
          }

          if (isDtSingle(dtBits, DT::kF32)) {
            opcode = 0xF3000D10u;
            goto Emit_R0At12Of4Hi22_R1At16Of4Hi7_R2At0Of4Hi5_NoCond;
          }

          if (isDtMultiple(dtBits, makeDtBits(DT::kP8, DT::kS16, DT::kS32, DT::kS8, DT::kU16, DT::kU32, DT::kU8))) {
            opcode = 0xF2000910u;
            opcode |= sz << 20u;
            opcode |= pBitFromDt(dtBits) << 24u;
            goto Emit_R0At12Of4Hi22_R1At16Of4Hi7_R2At0Of4Hi5_NoCond;
          }
        }

        if (isPureVec(o0.as<Vec>(), o1.as<Vec>()) && isElementVec(o2.as<Vec>())) {
          if (isDtSingle(dtBits, DT::kF16)) {
            uint32_t i = o2.as<Vec>().as<Vec>().elementIndex();
            if (i <= 0x3) {
              opcode = 0xF2900940u;
              opcode |= (i & 0x1u) << 3u;
              opcode |= (i & 0x2u) << 4u;
              goto Emit_R0At12Of4Hi22_R1At16Of4Hi7_R2At0Of3_NoCond;
            }
          }

          if (isDtSingle(dtBits, DT::kF32)) {
            uint32_t i = o2.as<Vec>().as<Vec>().elementIndex();
            if (i <= 0x1) {
              opcode = 0xF2A00940u;
              opcode |= i << 5u;
              goto Emit_R0At12Of4Hi22_R1At16Of4Hi7_R2At0Of4_NoCond;
            }
          }

          if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16))) {
            uint32_t i = o2.as<Vec>().as<Vec>().elementIndex();
            if (i <= 0x3) {
              opcode = 0xF2900840u;
              opcode |= (i & 0x1u) << 3u;
              opcode |= (i & 0x2u) << 4u;
              goto Emit_R0At12Of4Hi22_R1At16Of4Hi7_R2At0Of3_NoCond;
            }
          }

          if (isDtMultiple(dtBits, makeDtBits(DT::kS32, DT::kU32))) {
            uint32_t i = o2.as<Vec>().as<Vec>().elementIndex();
            if (i <= 0x1) {
              opcode = 0xF2A00840u;
              opcode |= i << 5u;
              goto Emit_R0At12Of4Hi22_R1At16Of4Hi7_R2At0Of4_NoCond;
            }
          }
        }
      }

      if (sgn.test<kOpRegQ, kOpRegQ, kOpRegD>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>()) && isElementVec(o2.as<Vec>())) {
          if (isDtSingle(dtBits, DT::kF16)) {
            uint32_t i = o2.as<Vec>().as<Vec>().elementIndex();
            if (i <= 0x3) {
              opcode = 0xF3900940u;
              opcode |= (i & 0x1u) << 3u;
              opcode |= (i & 0x2u) << 4u;
              goto Emit_Q0At12Of4Hi22_Q1At16Of4Hi7_R2At0Of3_NoCond;
            }
          }

          if (isDtSingle(dtBits, DT::kF32)) {
            uint32_t i = o2.as<Vec>().as<Vec>().elementIndex();
            if (i <= 0x1) {
              opcode = 0xF3A00940u;
              opcode |= i << 5u;
              goto Emit_Q0At12Of4Hi22_Q1At16Of4Hi7_R2At0Of4_NoCond;
            }
          }

          if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16))) {
            uint32_t i = o2.as<Vec>().as<Vec>().elementIndex();
            if (i <= 0x3) {
              opcode = 0xF3900840u;
              opcode |= (i & 0x1u) << 3u;
              opcode |= (i & 0x2u) << 4u;
              goto Emit_Q0At12Of4Hi22_Q1At16Of4Hi7_R2At0Of3_NoCond;
            }
          }

          if (isDtMultiple(dtBits, makeDtBits(DT::kS32, DT::kU32))) {
            uint32_t i = o2.as<Vec>().as<Vec>().elementIndex();
            if (i <= 0x1) {
              opcode = 0xF3A00840u;
              opcode |= i << 5u;
              goto Emit_Q0At12Of4Hi22_Q1At16Of4Hi7_R2At0Of4_NoCond;
            }
          }
        }
      }

      if (sgn.test<kOpRegQ, kOpRegQ, kOpRegQ>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>())) {
          if (isDtSingle(dtBits, DT::kF16)) {
            opcode = 0xF3100D50u;
            goto Emit_Q0At12Of4Hi22_Q1At16Of4Hi7_Q2At0Of4Hi5_NoCond;
          }

          if (isDtSingle(dtBits, DT::kF32)) {
            opcode = 0xF3000D50u;
            goto Emit_Q0At12Of4Hi22_Q1At16Of4Hi7_Q2At0Of4Hi5_NoCond;
          }

          if (isDtMultiple(dtBits, makeDtBits(DT::kP8, DT::kS16, DT::kS32, DT::kS8, DT::kU16, DT::kU32, DT::kU8))) {
            opcode = 0xF2000950u;
            opcode |= sz << 20u;
            opcode |= pBitFromDt(dtBits) << 24u;
            goto Emit_Q0At12Of4Hi22_Q1At16Of4Hi7_Q2At0Of4Hi5_NoCond;
          }
        }
      }

      break;
    }

    case 122: {
      // Instruction 'vmull'.
      uint32_t sz = szFromDt(dtBits);
      if (sgn.test<kOpRegQ, kOpRegD, kOpRegD>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>())) {
          if (isDtMultiple(dtBits, makeDtBits(DT::kP8, DT::kS16, DT::kS32, DT::kS8, DT::kU16, DT::kU32, DT::kU8))) {
            opcode = 0xF2800C00u;
            opcode |= pBitFromDt(dtBits) << 9u;
            opcode |= sz << 20u;
            opcode |= uBitFromDt(dtBits) << 24u;
            goto Emit_Q0At12Of4Hi22_R1At16Of4Hi7_R2At0Of4Hi5_NoCond;
          }

          if (isDtSingle(dtBits, DT::kP64)) {
            opcode = 0xF2A00C00u;
            opcode |= pBitFromDt(dtBits) << 9u;
            goto Emit_Q0At12Of4Hi22_R1At16Of4Hi7_R2At0Of4Hi5_NoCond;
          }
        }

        if (isPureVec(o0.as<Vec>(), o1.as<Vec>()) && isElementVec(o2.as<Vec>())) {
          if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16))) {
            uint32_t i = o2.as<Vec>().as<Vec>().elementIndex();
            if (i <= 0x3) {
              opcode = 0xF2900A40u;
              opcode |= (i & 0x1u) << 3u;
              opcode |= (i & 0x2u) << 4u;
              opcode |= uBitFromDt(dtBits) << 24u;
              goto Emit_Q0At12Of4Hi22_R1At16Of4Hi7_R2At0Of3_NoCond;
            }
          }

          if (isDtMultiple(dtBits, makeDtBits(DT::kS32, DT::kU32))) {
            uint32_t i = o2.as<Vec>().as<Vec>().elementIndex();
            if (i <= 0x1) {
              opcode = 0xF2A00A40u;
              opcode |= i << 5u;
              opcode |= uBitFromDt(dtBits) << 24u;
              goto Emit_Q0At12Of4Hi22_R1At16Of4Hi7_R2At0Of4_NoCond;
            }
          }
        }
      }

      break;
    }

    case 123: {
      // Instruction 'vmvn'.
      uint32_t sz = szFromDt(dtBits);
      if (sgn.test<kOpRegD, kOpImmI>()) {
        if (isPureVec(o0.as<Vec>())) {
          VecMovImmEncode enc0;
          if (enc0.init(sz, 1u, o1.as<Imm>())) {
            opcode = 0xF2800010u;
            opcode |= (enc0.imm() & 0xFu) << 0u;
            opcode |= (enc0.imm() & 0x70u) << 12u;
            opcode |= (enc0.imm() & 0x80u) << 17u;
            opcode |= enc0.op() << 5u;
            opcode |= enc0.cmode() << 8u;
            goto Emit_R0At12Of4Hi22_NoCond;
          }
        }
      }

      if (sgn.test<kOpRegQ, kOpImmI>()) {
        if (isPureVec(o0.as<Vec>())) {
          VecMovImmEncode enc1;
          if (enc1.init(sz, 1u, o1.as<Imm>())) {
            opcode = 0xF2800050u;
            opcode |= (enc1.imm() & 0xFu) << 0u;
            opcode |= (enc1.imm() & 0x70u) << 12u;
            opcode |= (enc1.imm() & 0x80u) << 17u;
            opcode |= enc1.op() << 5u;
            opcode |= enc1.cmode() << 8u;
            goto Emit_Q0At12Of4Hi22_NoCond;
          }
        }
      }

      if (sgn.test<kOpRegD, kOpRegD>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>())) {
          opcode = 0xF3B00580u;
          goto Emit_R0At12Of4Hi22_R1At0Of4Hi5_NoCond;
        }
      }

      if (sgn.test<kOpRegQ, kOpRegQ>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>())) {
          opcode = 0xF3B005C0u;
          goto Emit_Q0At12Of4Hi22_Q1At0Of4Hi5_NoCond;
        }
      }

      break;
    }

    case 124: {
      // Instruction 'vorn'.
      uint32_t sz = szFromDt(dtBits);
      if (sgn.test<kOpRegD, kOpImmI>()) {
        if (isPureVec(o0.as<Vec>())) {
          if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kS32, DT::kS64, DT::kU16, DT::kU32, DT::kU64))) {
            VecBicOrrImmEncode enc0;
            if (enc0.init(sz, 1u, o1.as<Imm>())) {
              opcode = 0xF2800010u;
              opcode |= (enc0.imm() & 0xFu) << 0u;
              opcode |= (enc0.imm() & 0x70u) << 12u;
              opcode |= (enc0.imm() & 0x80u) << 17u;
              opcode |= enc0.cmode() << 8u;
              goto Emit_R0At12Of4Hi22_NoCond;
            }
          }
        }
      }

      if (sgn.test<kOpRegQ, kOpImmI>()) {
        if (isPureVec(o0.as<Vec>())) {
          if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kS32, DT::kS64, DT::kU16, DT::kU32, DT::kU64))) {
            VecBicOrrImmEncode enc1;
            if (enc1.init(sz, 1u, o1.as<Imm>())) {
              opcode = 0xF2800050u;
              opcode |= (enc1.imm() & 0xFu) << 0u;
              opcode |= (enc1.imm() & 0x70u) << 12u;
              opcode |= (enc1.imm() & 0x80u) << 17u;
              opcode |= enc1.cmode() << 8u;
              goto Emit_Q0At12Of4Hi22_NoCond;
            }
          }
        }
      }

      if (sgn.test<kOpRegD, kOpRegD, kOpRegD>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>())) {
          opcode = 0xF2300110u;
          goto Emit_R0At12Of4Hi22_R1At16Of4Hi7_R2At0Of4Hi5_NoCond;
        }
      }

      if (sgn.test<kOpRegQ, kOpRegQ, kOpRegQ>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>())) {
          opcode = 0xF2300150u;
          goto Emit_Q0At12Of4Hi22_Q1At16Of4Hi7_Q2At0Of4Hi5_NoCond;
        }
      }

      break;
    }

    case 125: {
      // Instruction 'vorr'.
      uint32_t sz = szFromDt(dtBits);
      if (sgn.test<kOpRegD, kOpImmI>()) {
        if (isPureVec(o0.as<Vec>())) {
          if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kS32, DT::kS64, DT::kU16, DT::kU32, DT::kU64))) {
            VecBicOrrImmEncode enc0;
            if (enc0.init(sz, 0u, o1.as<Imm>())) {
              opcode = 0xF2800010u;
              opcode |= (enc0.imm() & 0xFu) << 0u;
              opcode |= (enc0.imm() & 0x70u) << 12u;
              opcode |= (enc0.imm() & 0x80u) << 17u;
              opcode |= enc0.cmode() << 8u;
              goto Emit_R0At12Of4Hi22_NoCond;
            }
          }
        }
      }

      if (sgn.test<kOpRegQ, kOpImmI>()) {
        if (isPureVec(o0.as<Vec>())) {
          if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kS32, DT::kS64, DT::kU16, DT::kU32, DT::kU64))) {
            VecBicOrrImmEncode enc1;
            if (enc1.init(sz, 0u, o1.as<Imm>())) {
              opcode = 0xF2800050u;
              opcode |= (enc1.imm() & 0xFu) << 0u;
              opcode |= (enc1.imm() & 0x70u) << 12u;
              opcode |= (enc1.imm() & 0x80u) << 17u;
              opcode |= enc1.cmode() << 8u;
              goto Emit_Q0At12Of4Hi22_NoCond;
            }
          }
        }
      }

      if (sgn.test<kOpRegD, kOpRegD, kOpRegD>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>())) {
          opcode = 0xF2200110u;
          goto Emit_R0At12Of4Hi22_R1At16Of4Hi7_R2At0Of4Hi5_NoCond;
        }
      }

      if (sgn.test<kOpRegQ, kOpRegQ, kOpRegQ>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>())) {
          opcode = 0xF2200150u;
          goto Emit_Q0At12Of4Hi22_Q1At16Of4Hi7_Q2At0Of4Hi5_NoCond;
        }
      }

      break;
    }

    case 126: {
      static const uint32_t opcodeTable[] = {
        0xF3B00600u, 0xF3B00640u, // Instruction 'vpadal'.
        0xF3B00200u, 0xF3B00240u  // Instruction 'vpaddl'.
      };

      const uint32_t* opcodeTablePtr = opcodeTable + uint32_t(idr.index) * 2u;

      uint32_t sz = szFromDt(dtBits);
      if (sgn.test<kOpRegD, kOpRegD>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>())) {
          if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kS32, DT::kS8, DT::kU16, DT::kU32, DT::kU8))) {
            opcode = opcodeTablePtr[0];
            opcode |= uBitFromDt(dtBits) << 7u;
            opcode |= sz << 18u;
            goto Emit_R0At12Of4Hi22_R1At0Of4Hi5_NoCond;
          }
        }
      }

      if (sgn.test<kOpRegQ, kOpRegQ>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>())) {
          if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kS32, DT::kS8, DT::kU16, DT::kU32, DT::kU8))) {
            opcode = opcodeTablePtr[1];
            opcode |= uBitFromDt(dtBits) << 7u;
            opcode |= sz << 18u;
            goto Emit_Q0At12Of4Hi22_Q1At0Of4Hi5_NoCond;
          }
        }
      }

      break;
    }

    case 127: {
      // Instruction 'vpadd'.
      if (sgn.test<kOpRegD, kOpRegD, kOpRegD>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>())) {
          if (isDtSingle(dtBits, DT::kF16)) {
            opcode = 0xF3100D00u;
            goto Emit_R0At12Of4Hi22_R1At16Of4Hi7_R2At0Of4Hi5_NoCond;
          }

          if (isDtSingle(dtBits, DT::kF32)) {
            opcode = 0xF3000D00u;
            goto Emit_R0At12Of4Hi22_R1At16Of4Hi7_R2At0Of4Hi5_NoCond;
          }
        }
      }

      break;
    }

    case 128: {
      static const uint32_t opcodeTable[] = {
        0xF3100F00u, 0xF3000F00u, 0xF2000A00u, // Instruction 'vpmax'.
        0xF3300F00u, 0xF3200F00u, 0xF2000A10u  // Instruction 'vpmin'.
      };

      const uint32_t* opcodeTablePtr = opcodeTable + uint32_t(idr.index) * 3u;

      uint32_t sz = szFromDt(dtBits);
      if (sgn.test<kOpRegD, kOpRegD, kOpRegD>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>())) {
          if (isDtSingle(dtBits, DT::kF16)) {
            opcode = opcodeTablePtr[0];
            goto Emit_R0At12Of4Hi22_R1At16Of4Hi7_R2At0Of4Hi5_NoCond;
          }

          if (isDtSingle(dtBits, DT::kF32)) {
            opcode = opcodeTablePtr[1];
            goto Emit_R0At12Of4Hi22_R1At16Of4Hi7_R2At0Of4Hi5_NoCond;
          }

          if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kS32, DT::kS8, DT::kU16, DT::kU32, DT::kU8))) {
            opcode = opcodeTablePtr[2];
            opcode |= sz << 20u;
            opcode |= uBitFromDt(dtBits) << 24u;
            goto Emit_R0At12Of4Hi22_R1At16Of4Hi7_R2At0Of4Hi5_NoCond;
          }
        }
      }

      break;
    }

    case 129: {
      static const uint32_t opcodeTable[] = {
        0x0CBD0A00u, 0x0CBD0B00u, // Instruction 'vpop'.
        0x0D2D0A00u, 0x0D2D0B00u  // Instruction 'vpush'.
      };

      const uint32_t* opcodeTablePtr = opcodeTable + uint32_t(idr.index) * 2u;

      if (sgn.test<kOpRegListS>()) {
        const GpList& regList = o0.as<GpList>();

        if (isDtMultiple(dtBits, makeDtBits(DT::kS32, DT::kU32, DT::kF32))) {
          VecSListImmEncode enc0;
          if (enc0.init(regList)) {
            opcode = opcodeTablePtr[0];
            opcode |= enc0.immCount() << 0u;
            opcode |= (enc0.immVd() & 0x1Eu) << 11u;
            opcode |= (enc0.immVd() & 0x1u) << 22u;
            goto Emit_Cond;
          }
        }
      }

      if (sgn.test<kOpRegListD>()) {
        const GpList& regList = o0.as<GpList>();

        if (isDtMultiple(dtBits, makeDtBits(DT::kS64, DT::kU64, DT::kF64))) {
          VecDListImmEncode enc1;
          if (enc1.init(regList)) {
            opcode = opcodeTablePtr[1];
            opcode |= enc1.immCount() << 0u;
            opcode |= (enc1.immVd() & 0xFu) << 12u;
            opcode |= (enc1.immVd() & 0x10u) << 18u;
            goto Emit_Cond;
          }
        }
      }

      break;
    }

    case 130: {
      static const uint32_t opcodeTable[] = {
        0xF3B00700u, 0xF3B00740u, // Instruction 'vqabs'.
        0xF3B00780u, 0xF3B007C0u  // Instruction 'vqneg'.
      };

      const uint32_t* opcodeTablePtr = opcodeTable + uint32_t(idr.index) * 2u;

      uint32_t sz = szFromDt(dtBits);
      if (sgn.test<kOpRegD, kOpRegD>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>())) {
          if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kS32, DT::kS8))) {
            opcode = opcodeTablePtr[0];
            opcode |= sz << 18u;
            goto Emit_R0At12Of4Hi22_R1At0Of4Hi5_NoCond;
          }
        }
      }

      if (sgn.test<kOpRegQ, kOpRegQ>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>())) {
          if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kS32, DT::kS8))) {
            opcode = opcodeTablePtr[1];
            opcode |= sz << 18u;
            goto Emit_Q0At12Of4Hi22_Q1At0Of4Hi5_NoCond;
          }
        }
      }

      break;
    }

    case 131: {
      static const uint32_t opcodeTable[] = {
        0xF2000010u, 0xF2000050u, // Instruction 'vqadd'.
        0xF2000210u, 0xF2000250u  // Instruction 'vqsub'.
      };

      const uint32_t* opcodeTablePtr = opcodeTable + uint32_t(idr.index) * 2u;

      uint32_t sz = szFromDt(dtBits);
      if (sgn.test<kOpRegD, kOpRegD, kOpRegD>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>())) {
          if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kS32, DT::kS64, DT::kS8, DT::kU16, DT::kU32, DT::kU64, DT::kU8))) {
            opcode = opcodeTablePtr[0];
            opcode |= sz << 20u;
            opcode |= uBitFromDt(dtBits) << 24u;
            goto Emit_R0At12Of4Hi22_R1At16Of4Hi7_R2At0Of4Hi5_NoCond;
          }
        }
      }

      if (sgn.test<kOpRegQ, kOpRegQ, kOpRegQ>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>())) {
          if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kS32, DT::kS64, DT::kS8, DT::kU16, DT::kU32, DT::kU64, DT::kU8))) {
            opcode = opcodeTablePtr[1];
            opcode |= sz << 20u;
            opcode |= uBitFromDt(dtBits) << 24u;
            goto Emit_Q0At12Of4Hi22_Q1At16Of4Hi7_Q2At0Of4Hi5_NoCond;
          }
        }
      }

      break;
    }

    case 132: {
      static const uint32_t opcodeTable[] = {
        0xF2800900u, 0xF2900340u, 0xF2A00340u, // Instruction 'vqdmlal'.
        0xF2800B00u, 0xF2900740u, 0xF2A00740u, // Instruction 'vqdmlsl'.
        0xF2800D00u, 0xF2900B40u, 0xF2A00B40u  // Instruction 'vqdmull'.
      };

      const uint32_t* opcodeTablePtr = opcodeTable + uint32_t(idr.index) * 3u;

      uint32_t sz = szFromDt(dtBits);
      if (sgn.test<kOpRegQ, kOpRegD, kOpRegD>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>())) {
          if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kS32))) {
            opcode = opcodeTablePtr[0];
            opcode |= sz << 20u;
            goto Emit_Q0At12Of4Hi22_R1At16Of4Hi7_R2At0Of4Hi5_NoCond;
          }
        }

        if (isPureVec(o0.as<Vec>(), o1.as<Vec>()) && isElementVec(o2.as<Vec>())) {
          if (isDtSingle(dtBits, DT::kS16)) {
            uint32_t i = o2.as<Vec>().as<Vec>().elementIndex();
            if (i <= 0x3) {
              opcode = opcodeTablePtr[1];
              opcode |= (i & 0x1u) << 3u;
              opcode |= (i & 0x2u) << 4u;
              goto Emit_Q0At12Of4Hi22_R1At16Of4Hi7_R2At0Of3_NoCond;
            }
          }

          if (isDtSingle(dtBits, DT::kS32)) {
            uint32_t i = o2.as<Vec>().as<Vec>().elementIndex();
            if (i <= 0x1) {
              opcode = opcodeTablePtr[2];
              opcode |= i << 5u;
              goto Emit_Q0At12Of4Hi22_R1At16Of4Hi7_R2At0Of4_NoCond;
            }
          }
        }
      }

      break;
    }

    case 133: {
      static const uint32_t opcodeTable[] = {
        0xF2000B00u, 0xF2900C40u, 0xF2A00C40u, 0xF2000B40u, 0xF3900C40u, 0xF3A00C40u, // Instruction 'vqdmulh'.
        0xF3000B00u, 0xF2900D40u, 0xF2A00D40u, 0xF3000B40u, 0xF3900D40u, 0xF3A00D40u  // Instruction 'vqrdmulh'.
      };

      const uint32_t* opcodeTablePtr = opcodeTable + uint32_t(idr.index) * 6u;

      uint32_t sz = szFromDt(dtBits);
      if (sgn.test<kOpRegD, kOpRegD, kOpRegD>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>())) {
          if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kS32))) {
            opcode = opcodeTablePtr[0];
            opcode |= sz << 20u;
            goto Emit_R0At12Of4Hi22_R1At16Of4Hi7_R2At0Of4Hi5_NoCond;
          }
        }

        if (isPureVec(o0.as<Vec>(), o1.as<Vec>()) && isElementVec(o2.as<Vec>())) {
          if (isDtSingle(dtBits, DT::kS16)) {
            uint32_t i = o2.as<Vec>().as<Vec>().elementIndex();
            if (i <= 0x3) {
              opcode = opcodeTablePtr[1];
              opcode |= (i & 0x1u) << 3u;
              opcode |= (i & 0x2u) << 4u;
              goto Emit_R0At12Of4Hi22_R1At16Of4Hi7_R2At0Of3_NoCond;
            }
          }

          if (isDtSingle(dtBits, DT::kS32)) {
            uint32_t i = o2.as<Vec>().as<Vec>().elementIndex();
            if (i <= 0x1) {
              opcode = opcodeTablePtr[2];
              opcode |= i << 5u;
              goto Emit_R0At12Of4Hi22_R1At16Of4Hi7_R2At0Of4_NoCond;
            }
          }
        }
      }

      if (sgn.test<kOpRegQ, kOpRegQ, kOpRegQ>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>())) {
          if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kS32))) {
            opcode = opcodeTablePtr[3];
            opcode |= sz << 20u;
            goto Emit_Q0At12Of4Hi22_Q1At16Of4Hi7_Q2At0Of4Hi5_NoCond;
          }
        }
      }

      if (sgn.test<kOpRegQ, kOpRegQ, kOpRegD>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>()) && isElementVec(o2.as<Vec>())) {
          if (isDtSingle(dtBits, DT::kS16)) {
            uint32_t i = o2.as<Vec>().as<Vec>().elementIndex();
            if (i <= 0x3) {
              opcode = opcodeTablePtr[4];
              opcode |= (i & 0x1u) << 3u;
              opcode |= (i & 0x2u) << 4u;
              goto Emit_Q0At12Of4Hi22_Q1At16Of4Hi7_R2At0Of3_NoCond;
            }
          }

          if (isDtSingle(dtBits, DT::kS32)) {
            uint32_t i = o2.as<Vec>().as<Vec>().elementIndex();
            if (i <= 0x1) {
              opcode = opcodeTablePtr[5];
              opcode |= i << 5u;
              goto Emit_Q0At12Of4Hi22_Q1At16Of4Hi7_R2At0Of4_NoCond;
            }
          }
        }
      }

      break;
    }

    case 134: {
      // Instruction 'vqmovn'.
      uint32_t sz = szFromDt(dtBits);
      if (sgn.test<kOpRegD, kOpRegQ>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>())) {
          if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kS32, DT::kS64, DT::kU16, DT::kU32, DT::kU64))) {
            opcode = 0xF3B20280u;
            opcode |= uBitFromDt(dtBits) << 6u;
            opcode |= (sz - 1) << 18u;
            goto Emit_R0At12Of4Hi22_Q1At0Of4Hi5_NoCond;
          }
        }
      }

      break;
    }

    case 135: {
      // Instruction 'vqmovun'.
      uint32_t sz = szFromDt(dtBits);
      if (sgn.test<kOpRegD, kOpRegQ>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>())) {
          if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kS32, DT::kS64))) {
            opcode = 0xF3B20240u;
            opcode |= (sz - 1) << 18u;
            goto Emit_R0At12Of4Hi22_Q1At0Of4Hi5_NoCond;
          }
        }
      }

      break;
    }

    case 136: {
      static const uint32_t opcodeTable[] = {
        0xF3000B10u, 0xF2800E40u, 0xF2800E40u, 0xF3000B50u, 0xF3800E40u, 0xF3800E40u, // Instruction 'vqrdmlah'.
        0xF3000C10u, 0xF2800F40u, 0xF2800F40u, 0xF3000C50u, 0xF3800F40u, 0xF3800F40u  // Instruction 'vqrdmlsh'.
      };

      const uint32_t* opcodeTablePtr = opcodeTable + uint32_t(idr.index) * 6u;

      uint32_t sz = szFromDt(dtBits);
      if (sgn.test<kOpRegD, kOpRegD, kOpRegD>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>())) {
          if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kS32))) {
            opcode = opcodeTablePtr[0];
            opcode |= sz << 20u;
            goto Emit_R0At12Of4Hi22_R1At16Of4Hi7_R2At0Of4Hi5_NoCond;
          }
        }

        if (isPureVec(o0.as<Vec>(), o1.as<Vec>()) && isElementVec(o2.as<Vec>())) {
          if (isDtSingle(dtBits, DT::kS16)) {
            uint32_t i = o2.as<Vec>().as<Vec>().elementIndex();
            if (i <= 0x3) {
              opcode = opcodeTablePtr[1];
              opcode |= (i & 0x1u) << 3u;
              opcode |= (i & 0x2u) << 4u;
              opcode |= sz << 20u;
              goto Emit_R0At12Of4Hi22_R1At16Of4Hi7_R2At0Of3_NoCond;
            }
          }

          if (isDtSingle(dtBits, DT::kS32)) {
            uint32_t i = o2.as<Vec>().as<Vec>().elementIndex();
            if (i <= 0x1) {
              opcode = opcodeTablePtr[2];
              opcode |= i << 5u;
              opcode |= sz << 20u;
              goto Emit_R0At12Of4Hi22_R1At16Of4Hi7_R2At0Of4_NoCond;
            }
          }
        }
      }

      if (sgn.test<kOpRegQ, kOpRegQ, kOpRegQ>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>())) {
          if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kS32))) {
            opcode = opcodeTablePtr[3];
            opcode |= sz << 20u;
            goto Emit_Q0At12Of4Hi22_Q1At16Of4Hi7_Q2At0Of4Hi5_NoCond;
          }
        }
      }

      if (sgn.test<kOpRegQ, kOpRegQ, kOpRegD>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>()) && isElementVec(o2.as<Vec>())) {
          if (isDtSingle(dtBits, DT::kS16)) {
            uint32_t i = o2.as<Vec>().as<Vec>().elementIndex();
            if (i <= 0x3) {
              opcode = opcodeTablePtr[4];
              opcode |= (i & 0x1u) << 3u;
              opcode |= (i & 0x2u) << 4u;
              opcode |= sz << 20u;
              goto Emit_Q0At12Of4Hi22_Q1At16Of4Hi7_R2At0Of3_NoCond;
            }
          }

          if (isDtSingle(dtBits, DT::kS32)) {
            uint32_t i = o2.as<Vec>().as<Vec>().elementIndex();
            if (i <= 0x1) {
              opcode = opcodeTablePtr[5];
              opcode |= i << 5u;
              opcode |= sz << 20u;
              goto Emit_Q0At12Of4Hi22_Q1At16Of4Hi7_R2At0Of4_NoCond;
            }
          }
        }
      }

      break;
    }

    case 137: {
      static const uint32_t opcodeTable[] = {
        0xF2000510u, 0xF2000550u, // Instruction 'vqrshl'.
        0xF2000500u, 0xF2000540u  // Instruction 'vrshl'.
      };

      const uint32_t* opcodeTablePtr = opcodeTable + uint32_t(idr.index) * 2u;

      uint32_t sz = szFromDt(dtBits);
      if (sgn.test<kOpRegD, kOpRegD, kOpRegD>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>())) {
          if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kS32, DT::kS64, DT::kS8, DT::kU16, DT::kU32, DT::kU64, DT::kU8))) {
            opcode = opcodeTablePtr[0];
            opcode |= sz << 20u;
            opcode |= uBitFromDt(dtBits) << 24u;
            goto Emit_R0At12Of4Hi22_R1At0Of4Hi5_R2At16Of4Hi7_NoCond;
          }
        }
      }

      if (sgn.test<kOpRegQ, kOpRegQ, kOpRegQ>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>())) {
          if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kS32, DT::kS64, DT::kS8, DT::kU16, DT::kU32, DT::kU64, DT::kU8))) {
            opcode = opcodeTablePtr[1];
            opcode |= sz << 20u;
            opcode |= uBitFromDt(dtBits) << 24u;
            goto Emit_Q0At12Of4Hi22_Q1At0Of4Hi5_Q2At16Of4Hi7_NoCond;
          }
        }
      }

      break;
    }

    case 138: {
      static const uint32_t opcodeTable[] = {
        0xF3B20280u, 0xF2800950u, // Instruction 'vqrshrn'.
        0xF3B20280u, 0xF2800910u  // Instruction 'vqshrn'.
      };

      const uint32_t* opcodeTablePtr = opcodeTable + uint32_t(idr.index) * 2u;

      uint32_t sz = szFromDt(dtBits);
      if (sgn.test<kOpRegD, kOpRegQ, kOpImmI>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>())) {
          if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kS32, DT::kS64, DT::kU16, DT::kU32, DT::kU64))) {
            VecShiftNarrowImmEncode enc0;
            if (o2.as<Imm>().value() == 0u) {
              opcode = opcodeTablePtr[0];
              opcode |= uBitFromDt(dtBits) << 6u;
              opcode |= (sz - 1) << 18u;
              goto Emit_R0At12Of4Hi22_Q1At0Of4Hi5_NoCond;
            }
            if (enc0.init(sz, o2.as<Imm>())) {
              opcode = opcodeTablePtr[1];
              opcode |= enc0.imm() << 16u;
              opcode |= uBitFromDt(dtBits) << 24u;
              goto Emit_R0At12Of4Hi22_Q1At0Of4Hi5_NoCond;
            }
          }
        }
      }

      break;
    }

    case 139: {
      static const uint32_t opcodeTable[] = {
        0xF3B20240u, 0xF3800850u, // Instruction 'vqrshrun'.
        0xF3B20240u, 0xF3800810u  // Instruction 'vqshrun'.
      };

      const uint32_t* opcodeTablePtr = opcodeTable + uint32_t(idr.index) * 2u;

      uint32_t sz = szFromDt(dtBits);
      if (sgn.test<kOpRegD, kOpRegQ, kOpImmI>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>())) {
          if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kS32, DT::kS64))) {
            VecShiftNarrowImmEncode enc0;
            if (o2.as<Imm>().value() == 0u) {
              opcode = opcodeTablePtr[0];
              opcode |= (sz - 1) << 18u;
              goto Emit_R0At12Of4Hi22_Q1At0Of4Hi5_NoCond;
            }
            if (enc0.init(sz, o2.as<Imm>())) {
              opcode = opcodeTablePtr[1];
              opcode |= enc0.imm() << 16u;
              goto Emit_R0At12Of4Hi22_Q1At0Of4Hi5_NoCond;
            }
          }
        }
      }

      break;
    }

    case 140: {
      // Instruction 'vqshl'.
      uint32_t sz = szFromDt(dtBits);
      if (sgn.test<kOpRegD, kOpRegD, kOpRegD>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>())) {
          if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kS32, DT::kS64, DT::kS8, DT::kU16, DT::kU32, DT::kU64, DT::kU8))) {
            opcode = 0xF2000410u;
            opcode |= sz << 20u;
            opcode |= uBitFromDt(dtBits) << 24u;
            goto Emit_R0At12Of4Hi22_R1At0Of4Hi5_R2At16Of4Hi7_NoCond;
          }
        }
      }

      if (sgn.test<kOpRegQ, kOpRegQ, kOpRegQ>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>())) {
          if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kS32, DT::kS64, DT::kS8, DT::kU16, DT::kU32, DT::kU64, DT::kU8))) {
            opcode = 0xF2000450u;
            opcode |= sz << 20u;
            opcode |= uBitFromDt(dtBits) << 24u;
            goto Emit_Q0At12Of4Hi22_Q1At0Of4Hi5_Q2At16Of4Hi7_NoCond;
          }
        }
      }

      if (sgn.test<kOpRegD, kOpRegD, kOpImmI>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>())) {
          if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kS32, DT::kS64, DT::kS8, DT::kU16, DT::kU32, DT::kU64, DT::kU8))) {
            VecShiftPImmEncode enc0;
            if (enc0.init(sz, o2.as<Imm>())) {
              opcode = 0xF2800710u;
              opcode |= (enc0.imm() & 0x40u) << 1u;
              opcode |= (enc0.imm() & 0x3Fu) << 16u;
              opcode |= uBitFromDt(dtBits) << 24u;
              goto Emit_R0At12Of4Hi22_R1At0Of4Hi5_NoCond;
            }
          }
        }
      }

      if (sgn.test<kOpRegQ, kOpRegQ, kOpImmI>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>())) {
          if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kS32, DT::kS64, DT::kS8, DT::kU16, DT::kU32, DT::kU64, DT::kU8))) {
            VecShiftPImmEncode enc1;
            if (enc1.init(sz, o2.as<Imm>())) {
              opcode = 0xF2800750u;
              opcode |= (enc1.imm() & 0x40u) << 1u;
              opcode |= (enc1.imm() & 0x3Fu) << 16u;
              opcode |= uBitFromDt(dtBits) << 24u;
              goto Emit_Q0At12Of4Hi22_Q1At0Of4Hi5_NoCond;
            }
          }
        }
      }

      break;
    }

    case 141: {
      // Instruction 'vqshlu'.
      uint32_t sz = szFromDt(dtBits);
      if (sgn.test<kOpRegD, kOpRegD, kOpImmI>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>())) {
          if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kS32, DT::kS64, DT::kS8))) {
            VecShiftPImmEncode enc0;
            if (enc0.init(sz, o2.as<Imm>())) {
              opcode = 0xF3800610u;
              opcode |= (enc0.imm() & 0x40u) << 1u;
              opcode |= (enc0.imm() & 0x3Fu) << 16u;
              goto Emit_R0At12Of4Hi22_R1At0Of4Hi5_NoCond;
            }
          }
        }
      }

      if (sgn.test<kOpRegQ, kOpRegQ, kOpImmI>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>())) {
          if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kS32, DT::kS64, DT::kS8))) {
            VecShiftPImmEncode enc1;
            if (enc1.init(sz, o2.as<Imm>())) {
              opcode = 0xF3800650u;
              opcode |= (enc1.imm() & 0x40u) << 1u;
              opcode |= (enc1.imm() & 0x3Fu) << 16u;
              goto Emit_Q0At12Of4Hi22_Q1At0Of4Hi5_NoCond;
            }
          }
        }
      }

      break;
    }

    case 142: {
      static const uint32_t opcodeTable[] = {
        0xF3B70500u, 0xF3BB0500u, 0xF3BB0400u, 0xF3B70540u, 0xF3BB0540u, 0xF3BB0440u, // Instruction 'vrecpe'.
        0xF3B70580u, 0xF3BB0580u, 0xF3BB0480u, 0xF3B705C0u, 0xF3BB05C0u, 0xF3BB04C0u  // Instruction 'vrsqrte'.
      };

      const uint32_t* opcodeTablePtr = opcodeTable + uint32_t(idr.index) * 6u;

      if (sgn.test<kOpRegD, kOpRegD>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>())) {
          if (isDtSingle(dtBits, DT::kF16)) {
            opcode = opcodeTablePtr[0];
            goto Emit_R0At12Of4Hi22_R1At0Of4Hi5_NoCond;
          }

          if (isDtSingle(dtBits, DT::kF32)) {
            opcode = opcodeTablePtr[1];
            goto Emit_R0At12Of4Hi22_R1At0Of4Hi5_NoCond;
          }

          if (isDtSingle(dtBits, DT::kU32)) {
            opcode = opcodeTablePtr[2];
            goto Emit_R0At12Of4Hi22_R1At0Of4Hi5_NoCond;
          }
        }
      }

      if (sgn.test<kOpRegQ, kOpRegQ>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>())) {
          if (isDtSingle(dtBits, DT::kF16)) {
            opcode = opcodeTablePtr[3];
            goto Emit_Q0At12Of4Hi22_Q1At0Of4Hi5_NoCond;
          }

          if (isDtSingle(dtBits, DT::kF32)) {
            opcode = opcodeTablePtr[4];
            goto Emit_Q0At12Of4Hi22_Q1At0Of4Hi5_NoCond;
          }

          if (isDtSingle(dtBits, DT::kU32)) {
            opcode = opcodeTablePtr[5];
            goto Emit_Q0At12Of4Hi22_Q1At0Of4Hi5_NoCond;
          }
        }
      }

      break;
    }

    case 143: {
      // Instruction 'vrev32'.
      uint32_t sz = szFromDt(dtBits);
      if (sgn.test<kOpRegD, kOpRegD>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>())) {
          if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kS8, DT::kU16, DT::kU8))) {
            opcode = 0xF3B00080u;
            opcode |= sz << 18u;
            goto Emit_R0At12Of4Hi22_R1At0Of4Hi5_NoCond;
          }
        }
      }

      if (sgn.test<kOpRegQ, kOpRegQ>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>())) {
          if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kS8, DT::kU16, DT::kU8))) {
            opcode = 0xF3B000C0u;
            opcode |= sz << 18u;
            goto Emit_Q0At12Of4Hi22_Q1At0Of4Hi5_NoCond;
          }
        }
      }

      break;
    }

    case 144: {
      static const uint32_t opcodeTable[] = {
        0xFEB80940u, 0xFEB80A40u, 0xFEB80B40u, 0xF3B60500u, 0xF3BA0500u, 0xF3B60540u, 0xF3BA0540u, // Instruction 'vrinta'.
        0xFEBB0940u, 0xFEBB0A40u, 0xFEBB0B40u, 0xF3B60680u, 0xF3BA0680u, 0xF3B606C0u, 0xF3BA06C0u, // Instruction 'vrintm'.
        0xFEB90940u, 0xFEB90A40u, 0xFEB90B40u, 0xF3B60400u, 0xF3BA0400u, 0xF3B60440u, 0xF3BA0440u, // Instruction 'vrintn'.
        0xFEBA0940u, 0xFEBA0A40u, 0xFEBA0B40u, 0xF3B60780u, 0xF3BA0780u, 0xF3B607C0u, 0xF3BA07C0u  // Instruction 'vrintp'.
      };

      const uint32_t* opcodeTablePtr = opcodeTable + uint32_t(idr.index) * 7u;

      if (sgn.test<kOpRegS, kOpRegS>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>())) {
          if (isDtSingle(dtBits, DT::kF16)) {
            opcode = opcodeTablePtr[0];
            goto Emit_R0At12Of4Lo22_R1At0Of4Lo5_NoCond;
          }

          if (isDtSingle(dtBits, DT::kF32)) {
            opcode = opcodeTablePtr[1];
            goto Emit_R0At12Of4Lo22_R1At0Of4Lo5_NoCond;
          }
        }
      }

      if (sgn.test<kOpRegD, kOpRegD>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>())) {
          if (isDtSingle(dtBits, DT::kF64)) {
            opcode = opcodeTablePtr[2];
            goto Emit_R0At12Of4Hi22_R1At0Of4Hi5_NoCond;
          }

          if (isDtSingle(dtBits, DT::kF16)) {
            opcode = opcodeTablePtr[3];
            goto Emit_R0At12Of4Hi22_R1At0Of4Hi5_NoCond;
          }

          if (isDtSingle(dtBits, DT::kF32)) {
            opcode = opcodeTablePtr[4];
            goto Emit_R0At12Of4Hi22_R1At0Of4Hi5_NoCond;
          }
        }
      }

      if (sgn.test<kOpRegQ, kOpRegQ>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>())) {
          if (isDtSingle(dtBits, DT::kF16)) {
            opcode = opcodeTablePtr[5];
            goto Emit_Q0At12Of4Hi22_Q1At0Of4Hi5_NoCond;
          }

          if (isDtSingle(dtBits, DT::kF32)) {
            opcode = opcodeTablePtr[6];
            goto Emit_Q0At12Of4Hi22_Q1At0Of4Hi5_NoCond;
          }
        }
      }

      break;
    }

    case 145: {
      // Instruction 'vrintr'.
      if (sgn.test<kOpRegS, kOpRegS>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>())) {
          if (isDtSingle(dtBits, DT::kF16)) {
            opcode = 0xEEB60940u;
            goto Emit_R0At12Of4Lo22_R1At0Of4Lo5_NoCond;
          }

          if (isDtSingle(dtBits, DT::kF32)) {
            opcode = 0x0EB60A40u;
            goto Emit_R0At12Of4Lo22_R1At0Of4Lo5_Cond;
          }
        }
      }

      if (sgn.test<kOpRegD, kOpRegD>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>())) {
          if (isDtSingle(dtBits, DT::kF64)) {
            opcode = 0x0EB60B40u;
            goto Emit_R0At12Of4Hi22_R1At0Of4Hi5_Cond;
          }
        }
      }

      break;
    }

    case 146: {
      static const uint32_t opcodeTable[] = {
        0xEEB70940u, 0x0EB70A40u, 0x0EB70B40u, 0xF3B60480u, 0xF3BA0480u, 0xF3B604C0u, 0xF3BA04C0u, // Instruction 'vrintx'.
        0xEEB609C0u, 0x0EB60AC0u, 0x0EB60BC0u, 0xF3B60580u, 0xF3BA0580u, 0xF3B605C0u, 0xF3BA05C0u  // Instruction 'vrintz'.
      };

      const uint32_t* opcodeTablePtr = opcodeTable + uint32_t(idr.index) * 7u;

      if (sgn.test<kOpRegS, kOpRegS>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>())) {
          if (isDtSingle(dtBits, DT::kF16)) {
            opcode = opcodeTablePtr[0];
            goto Emit_R0At12Of4Lo22_R1At0Of4Lo5_NoCond;
          }

          if (isDtSingle(dtBits, DT::kF32)) {
            opcode = opcodeTablePtr[1];
            goto Emit_R0At12Of4Lo22_R1At0Of4Lo5_Cond;
          }
        }
      }

      if (sgn.test<kOpRegD, kOpRegD>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>())) {
          if (isDtSingle(dtBits, DT::kF64)) {
            opcode = opcodeTablePtr[2];
            goto Emit_R0At12Of4Hi22_R1At0Of4Hi5_Cond;
          }

          if (isDtSingle(dtBits, DT::kF16)) {
            opcode = opcodeTablePtr[3];
            goto Emit_R0At12Of4Hi22_R1At0Of4Hi5_NoCond;
          }

          if (isDtSingle(dtBits, DT::kF32)) {
            opcode = opcodeTablePtr[4];
            goto Emit_R0At12Of4Hi22_R1At0Of4Hi5_NoCond;
          }
        }
      }

      if (sgn.test<kOpRegQ, kOpRegQ>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>())) {
          if (isDtSingle(dtBits, DT::kF16)) {
            opcode = opcodeTablePtr[5];
            goto Emit_Q0At12Of4Hi22_Q1At0Of4Hi5_NoCond;
          }

          if (isDtSingle(dtBits, DT::kF32)) {
            opcode = opcodeTablePtr[6];
            goto Emit_Q0At12Of4Hi22_Q1At0Of4Hi5_NoCond;
          }
        }
      }

      break;
    }

    case 147: {
      // Instruction 'vrshr'.
      uint32_t sz = szFromDt(dtBits);
      if (sgn.test<kOpRegD, kOpRegD, kOpImmI>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>())) {
          if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kS32, DT::kS8, DT::kU16, DT::kU32, DT::kU8))) {
            VecShiftNImmEncode enc0;
            if (o2.as<Imm>().value() == 0u) {
              opcode = 0xF2200110u;
              goto Emit_R0At12Of4Hi22_R1At0Of4At16Of4Hi5Hi7_NoCond;
            }
            if (enc0.init(sz, o2.as<Imm>())) {
              opcode = 0xF2800210u;
              opcode |= enc0.imm() << 16u;
              opcode |= uBitFromDt(dtBits) << 24u;
              goto Emit_R0At12Of4Hi22_R1At0Of4Hi5_NoCond;
            }
          }
        }
      }

      if (sgn.test<kOpRegQ, kOpRegQ, kOpImmI>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>())) {
          if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kS32, DT::kS8, DT::kU16, DT::kU32, DT::kU8))) {
            VecShiftNImmEncode enc1;
            if (o2.as<Imm>().value() == 0u) {
              opcode = 0xF2200150u;
              goto Emit_Q0At12Of4Hi22_Q1At0Of4At16Of4Hi5Hi7_NoCond;
            }
            if (enc1.init(sz, o2.as<Imm>())) {
              opcode = 0xF2800250u;
              opcode |= enc1.imm() << 16u;
              opcode |= uBitFromDt(dtBits) << 24u;
              goto Emit_Q0At12Of4Hi22_Q1At0Of4Hi5_NoCond;
            }
          }
        }
      }

      break;
    }

    case 148: {
      static const uint32_t opcodeTable[] = {
        0xF3B20210u, 0xF2800850u, // Instruction 'vrshrn'.
        0xFFB20210u, 0xF2800810u  // Instruction 'vshrn'.
      };

      const uint32_t* opcodeTablePtr = opcodeTable + uint32_t(idr.index) * 2u;

      uint32_t sz = szFromDt(dtBits);
      if (sgn.test<kOpRegD, kOpRegQ, kOpImmI>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>())) {
          if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kS32, DT::kS64, DT::kU16, DT::kU32, DT::kU64))) {
            VecShiftNarrowImmEncode enc0;
            if (o2.as<Imm>().value() == 0u) {
              opcode = opcodeTablePtr[0];
              opcode |= sz << 18u;
              goto Emit_R0At12Of4Hi22_Q1At0Of4Hi5_NoCond;
            }
            if (enc0.init(sz, o2.as<Imm>())) {
              opcode = opcodeTablePtr[1];
              opcode |= enc0.imm() << 16u;
              goto Emit_R0At12Of4Hi22_Q1At0Of4Hi5_NoCond;
            }
          }
        }
      }

      break;
    }

    case 149: {
      static const uint32_t opcodeTable[] = {
        0xF2800310u, 0xF2800350u, // Instruction 'vrsra'.
        0xF2800110u, 0xF2800150u  // Instruction 'vsra'.
      };

      const uint32_t* opcodeTablePtr = opcodeTable + uint32_t(idr.index) * 2u;

      uint32_t sz = szFromDt(dtBits);
      if (sgn.test<kOpRegD, kOpRegD, kOpImmI>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>())) {
          if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kS32, DT::kS64, DT::kS8, DT::kU16, DT::kU32, DT::kU64, DT::kU8))) {
            VecShiftNImmEncode enc0;
            if (enc0.init(sz, o2.as<Imm>())) {
              opcode = opcodeTablePtr[0];
              opcode |= (enc0.imm() & 0x40u) << 1u;
              opcode |= (enc0.imm() & 0x3Fu) << 16u;
              opcode |= uBitFromDt(dtBits) << 24u;
              goto Emit_R0At12Of4Hi22_R1At0Of4Hi5_NoCond;
            }
          }
        }
      }

      if (sgn.test<kOpRegQ, kOpRegQ, kOpImmI>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>())) {
          if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kS32, DT::kS64, DT::kS8, DT::kU16, DT::kU32, DT::kU64, DT::kU8))) {
            VecShiftNImmEncode enc1;
            if (enc1.init(sz, o2.as<Imm>())) {
              opcode = opcodeTablePtr[1];
              opcode |= (enc1.imm() & 0x40u) << 1u;
              opcode |= (enc1.imm() & 0x3Fu) << 16u;
              opcode |= uBitFromDt(dtBits) << 24u;
              goto Emit_Q0At12Of4Hi22_Q1At0Of4Hi5_NoCond;
            }
          }
        }
      }

      break;
    }

    case 150: {
      static const uint32_t opcodeTable[] = {
        0xFC200D00u, 0xFE200D00u, 0xFC200D40u, 0xFE200D40u, // Instruction 'vsdot'.
        0xFCA00D00u, 0xFE800D00u, 0xFCA00D40u, 0xFE800D40u  // Instruction 'vusdot'.
      };

      const uint32_t* opcodeTablePtr = opcodeTable + uint32_t(idr.index) * 4u;

      if (sgn.test<kOpRegD, kOpRegD, kOpRegD>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>())) {
          if (isDtSingle(dtBits, DT::kS8)) {
            opcode = opcodeTablePtr[0];
            goto Emit_R0At12Of4Hi22_R1At16Of4Hi7_R2At0Of4Hi5_NoCond;
          }
        }

        if (isPureVec(o0.as<Vec>(), o1.as<Vec>()) && isElementVec(o2.as<Vec>())) {
          if (isDtSingle(dtBits, DT::kS8)) {
            uint32_t i = o2.as<Vec>().as<Vec>().elementIndex();
            if (i <= 0x1) {
              opcode = opcodeTablePtr[1];
              opcode |= i << 5u;
              goto Emit_R0At12Of4Hi22_R1At16Of4Hi7_R2At0Of4_NoCond;
            }
          }
        }
      }

      if (sgn.test<kOpRegQ, kOpRegQ, kOpRegQ>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>())) {
          if (isDtSingle(dtBits, DT::kS8)) {
            opcode = opcodeTablePtr[2];
            goto Emit_Q0At12Of4Hi22_Q1At16Of4Hi7_Q2At0Of4Hi5_NoCond;
          }
        }
      }

      if (sgn.test<kOpRegQ, kOpRegQ, kOpRegD>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>()) && isElementVec(o2.as<Vec>())) {
          if (isDtSingle(dtBits, DT::kS8)) {
            uint32_t i = o2.as<Vec>().as<Vec>().elementIndex();
            if (i <= 0x1) {
              opcode = opcodeTablePtr[3];
              opcode |= i << 5u;
              goto Emit_Q0At12Of4Hi22_Q1At16Of4Hi7_R2At0Of4_NoCond;
            }
          }
        }
      }

      break;
    }

    case 151: {
      static const uint32_t opcodeTable[] = {
        0xFE000900u, 0xFE000A00u, 0xFE000B00u, // Instruction 'vseleq'.
        0xFE200900u, 0xFE200A00u, 0xFE200B00u, // Instruction 'vselge'.
        0xFE300900u, 0xFE300A00u, 0xFE300B00u, // Instruction 'vselgt'.
        0xFE100900u, 0xFE100A00u, 0xFE100B00u  // Instruction 'vselvs'.
      };

      const uint32_t* opcodeTablePtr = opcodeTable + uint32_t(idr.index) * 3u;

      if (sgn.test<kOpRegS, kOpRegS, kOpRegS>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>())) {
          if (isDtSingle(dtBits, DT::kF16)) {
            opcode = opcodeTablePtr[0];
            goto Emit_R0At12Of4Lo22_R1At16Of4Lo7_R2At0Of4Lo5_NoCond;
          }

          if (isDtSingle(dtBits, DT::kF32)) {
            opcode = opcodeTablePtr[1];
            goto Emit_R0At12Of4Lo22_R1At16Of4Lo7_R2At0Of4Lo5_NoCond;
          }
        }
      }

      if (sgn.test<kOpRegD, kOpRegD, kOpRegD>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>())) {
          if (isDtSingle(dtBits, DT::kF64)) {
            opcode = opcodeTablePtr[2];
            goto Emit_R0At12Of4Hi22_R1At16Of4Hi7_R2At0Of4Hi5_NoCond;
          }
        }
      }

      break;
    }

    case 152: {
      // Instruction 'vshl'.
      uint32_t sz = szFromDt(dtBits);
      if (sgn.test<kOpRegD, kOpRegD, kOpRegD>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>())) {
          if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kS32, DT::kS64, DT::kS8, DT::kU16, DT::kU32, DT::kU64, DT::kU8))) {
            opcode = 0xF2000400u;
            opcode |= sz << 20u;
            opcode |= uBitFromDt(dtBits) << 24u;
            goto Emit_R0At12Of4Hi22_R1At0Of4Hi5_R2At16Of4Hi7_NoCond;
          }
        }
      }

      if (sgn.test<kOpRegQ, kOpRegQ, kOpRegQ>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>())) {
          if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kS32, DT::kS64, DT::kS8, DT::kU16, DT::kU32, DT::kU64, DT::kU8))) {
            opcode = 0xF2000440u;
            opcode |= sz << 20u;
            opcode |= uBitFromDt(dtBits) << 24u;
            goto Emit_Q0At12Of4Hi22_Q1At0Of4Hi5_Q2At16Of4Hi7_NoCond;
          }
        }
      }

      if (sgn.test<kOpRegD, kOpRegD, kOpImmI>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>())) {
          if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kS32, DT::kS64, DT::kS8, DT::kU16, DT::kU32, DT::kU64, DT::kU8))) {
            VecShiftPImmEncode enc0;
            if (enc0.init(sz, o2.as<Imm>())) {
              opcode = 0xF2800510u;
              opcode |= (enc0.imm() & 0x40u) << 1u;
              opcode |= (enc0.imm() & 0x3Fu) << 16u;
              goto Emit_R0At12Of4Hi22_R1At0Of4Hi5_NoCond;
            }
          }
        }
      }

      if (sgn.test<kOpRegQ, kOpRegQ, kOpImmI>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>())) {
          if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kS32, DT::kS64, DT::kS8, DT::kU16, DT::kU32, DT::kU64, DT::kU8))) {
            VecShiftPImmEncode enc1;
            if (enc1.init(sz, o2.as<Imm>())) {
              opcode = 0xF2800550u;
              opcode |= (enc1.imm() & 0x40u) << 1u;
              opcode |= (enc1.imm() & 0x3Fu) << 16u;
              goto Emit_Q0At12Of4Hi22_Q1At0Of4Hi5_NoCond;
            }
          }
        }
      }

      break;
    }

    case 153: {
      // Instruction 'vshll'.
      uint32_t sz = szFromDt(dtBits);
      if (sgn.test<kOpRegQ, kOpRegD, kOpImmI>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>())) {
          if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kS32, DT::kS8, DT::kU16, DT::kU32, DT::kU8))) {
            VecShiftPImmEncode enc0;
            if (enc0.init(sz, o2.as<Imm>())) {
              opcode = 0xF2800A10u;
              opcode |= enc0.imm() << 16u;
              opcode |= uBitFromDt(dtBits) << 24u;
              goto Emit_Q0At12Of4Hi22_R1At0Of4Hi5_NoCond;
            }
          }
        }
      }

      break;
    }

    case 154: {
      // Instruction 'vshr'.
      uint32_t sz = szFromDt(dtBits);
      if (sgn.test<kOpRegD, kOpRegD, kOpImmI>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>())) {
          if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kS32, DT::kS64, DT::kS8, DT::kU16, DT::kU32, DT::kU64, DT::kU8))) {
            VecShiftNImmEncode enc0;
            if (o2.as<Imm>().value() == 0u) {
              opcode = 0xF2200110u;
              goto Emit_R0At12Of4Hi22_R1At0Of4At16Of4Hi5Hi7_NoCond;
            }
            if (enc0.init(sz, o2.as<Imm>())) {
              opcode = 0xF2800010u;
              opcode |= (enc0.imm() & 0x40u) << 1u;
              opcode |= (enc0.imm() & 0x3Fu) << 16u;
              opcode |= uBitFromDt(dtBits) << 24u;
              goto Emit_R0At12Of4Hi22_R1At0Of4Hi5_NoCond;
            }
          }
        }
      }

      if (sgn.test<kOpRegQ, kOpRegQ, kOpImmI>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>())) {
          if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kS32, DT::kS64, DT::kS8, DT::kU16, DT::kU32, DT::kU64, DT::kU8))) {
            VecShiftNImmEncode enc1;
            if (o2.as<Imm>().value() == 0u) {
              opcode = 0xF2200150u;
              goto Emit_Q0At12Of4Hi22_Q1At0Of4At16Of4Hi5Hi7_NoCond;
            }
            if (enc1.init(sz, o2.as<Imm>())) {
              opcode = 0xF2800050u;
              opcode |= (enc1.imm() & 0x40u) << 1u;
              opcode |= (enc1.imm() & 0x3Fu) << 16u;
              opcode |= uBitFromDt(dtBits) << 24u;
              goto Emit_Q0At12Of4Hi22_Q1At0Of4Hi5_NoCond;
            }
          }
        }
      }

      break;
    }

    case 155: {
      // Instruction 'vsli'.
      uint32_t sz = szFromDt(dtBits);
      if (sgn.test<kOpRegD, kOpRegD, kOpImmI>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>())) {
          if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kS32, DT::kS64, DT::kS8, DT::kU16, DT::kU32, DT::kU64, DT::kU8))) {
            VecShiftPImmEncode enc0;
            if (enc0.init(sz, o2.as<Imm>())) {
              opcode = 0xF3800510u;
              opcode |= (enc0.imm() & 0x40u) << 1u;
              opcode |= (enc0.imm() & 0x3Fu) << 16u;
              goto Emit_R0At12Of4Hi22_R1At0Of4Hi5_NoCond;
            }
          }
        }
      }

      if (sgn.test<kOpRegQ, kOpRegQ, kOpImmI>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>())) {
          if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kS32, DT::kS64, DT::kS8, DT::kU16, DT::kU32, DT::kU64, DT::kU8))) {
            VecShiftPImmEncode enc1;
            if (enc1.init(sz, o2.as<Imm>())) {
              opcode = 0xF3800550u;
              opcode |= (enc1.imm() & 0x40u) << 1u;
              opcode |= (enc1.imm() & 0x3Fu) << 16u;
              goto Emit_Q0At12Of4Hi22_Q1At0Of4Hi5_NoCond;
            }
          }
        }
      }

      break;
    }

    case 156: {
      static const uint32_t opcodeTable[] = {
        0xFC200C40u, // Instruction 'vsmmla'.
        0xFCA00C40u  // Instruction 'vusmmla'.
      };

      const uint32_t* opcodeTablePtr = opcodeTable + uint32_t(idr.index) * 1u;

      if (sgn.test<kOpRegQ, kOpRegQ, kOpRegQ>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>())) {
          if (isDtSingle(dtBits, DT::kS8)) {
            opcode = opcodeTablePtr[0];
            goto Emit_Q0At12Of4Hi22_Q1At16Of4Hi7_Q2At0Of4Hi5_NoCond;
          }
        }
      }

      break;
    }

    case 157: {
      // Instruction 'vsqrt'.
      if (sgn.test<kOpRegS, kOpRegS>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>())) {
          if (isDtSingle(dtBits, DT::kF32)) {
            opcode = 0x0EB10AC0u;
            goto Emit_R0At12Of4Lo22_R1At0Of4Lo5_Cond;
          }

          if (isDtSingle(dtBits, DT::kF16)) {
            opcode = 0xEEB109C0u;
            goto Emit_R0At12Of4Lo22_R1At0Of4Lo5_NoCond;
          }
        }
      }

      if (sgn.test<kOpRegD, kOpRegD>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>())) {
          if (isDtSingle(dtBits, DT::kF64)) {
            opcode = 0x0EB10BC0u;
            goto Emit_R0At12Of4Hi22_R1At0Of4Hi5_Cond;
          }
        }
      }

      break;
    }

    case 158: {
      // Instruction 'vsri'.
      uint32_t sz = szFromDt(dtBits);
      if (sgn.test<kOpRegD, kOpRegD, kOpImmI>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>())) {
          if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kS32, DT::kS64, DT::kS8, DT::kU16, DT::kU32, DT::kU64, DT::kU8))) {
            VecShiftNImmEncode enc0;
            if (enc0.init(sz, o2.as<Imm>())) {
              opcode = 0xF3800410u;
              opcode |= (enc0.imm() & 0x40u) << 1u;
              opcode |= (enc0.imm() & 0x3Fu) << 16u;
              goto Emit_R0At12Of4Hi22_R1At0Of4Hi5_NoCond;
            }
          }
        }
      }

      if (sgn.test<kOpRegQ, kOpRegQ, kOpImmI>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>())) {
          if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kS32, DT::kS64, DT::kS8, DT::kU16, DT::kU32, DT::kU64, DT::kU8))) {
            VecShiftNImmEncode enc1;
            if (enc1.init(sz, o2.as<Imm>())) {
              opcode = 0xF3800450u;
              opcode |= (enc1.imm() & 0x40u) << 1u;
              opcode |= (enc1.imm() & 0x3Fu) << 16u;
              goto Emit_Q0At12Of4Hi22_Q1At0Of4Hi5_NoCond;
            }
          }
        }
      }

      break;
    }

    case 159: {
      // Instruction 'vst1'.
      uint32_t sz = szFromDt(dtBits);
      if (sgn.test<kOpRegD, kOpMemB>()) {
        mem = &o1.as<Mem>();

        if (mem->baseId() < 15u) {
          if (!mem->hasIndex()) {
            if (!mem->offsetLo32()) {
              if (mem->isFixedOffset()) {
                if (isPureVec(o0.as<Vec>())) {
                  if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16, DT::kS32, DT::kU32, DT::kF32, DT::kS64, DT::kU64, DT::kF64, DT::kS8, DT::kU8))) {
                    opcode = 0xF400070Fu;
                    opcode |= sz << 6u;
                    goto Emit_R0At12Of4Hi22_MemBaseAt16_NoCond;
                  }
                }
              }
            }

            if (uint32_t(mem->offsetLo32()) == 8u) {
              if (mem->isPostIndex()) {
                if (isPureVec(o0.as<Vec>())) {
                  if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16, DT::kS32, DT::kU32, DT::kF32, DT::kS64, DT::kU64, DT::kF64, DT::kS8, DT::kU8))) {
                    opcode = 0xF400070Du;
                    opcode |= sz << 6u;
                    goto Emit_R0At12Of4Hi22_MemBaseAt16_NoCond;
                  }
                }
              }
            }
          }

          if (mem->indexType() == RegType::kGp32) {
            if (!mem->offsetLo32()) {
              if ((mem->indexId() < 13u) || (mem->indexId() == 14u)) {
                if (mem->isPostIndex()) {
                  if (isPureVec(o0.as<Vec>())) {
                    if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16, DT::kS32, DT::kU32, DT::kF32, DT::kS64, DT::kU64, DT::kF64, DT::kS8, DT::kU8))) {
                      opcode = 0xF4000700u;
                      opcode |= sz << 6u;
                      goto Emit_R0At12Of4Hi22_MemBaseAt16_MemUIndexAt0_NoCond;
                    }
                  }
                }
              }
            }
          }
        }
      }

      if (sgn.test<kOpRegD, kOpRegD, kOpMemB>()) {
        mem = &o2.as<Mem>();

        if (mem->baseId() < 15u) {
          if (!mem->hasIndex()) {
            if (!mem->offsetLo32()) {
              if (mem->isFixedOffset()) {
                if (isPureVec(o0.as<Vec>(), o1.as<Vec>())) {
                  if (isConsecutive(1, o0.as<Reg>(), o1.as<Reg>())) {
                    if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16, DT::kS32, DT::kU32, DT::kF32, DT::kS64, DT::kU64, DT::kF64, DT::kS8, DT::kU8))) {
                      opcode = 0xF4000A0Fu;
                      opcode |= sz << 6u;
                      goto Emit_R0At12Of4Hi22_MemBaseAt16_NoCond;
                    }
                  }
                }
              }
            }

            if (uint32_t(mem->offsetLo32()) == 16u) {
              if (mem->isPostIndex()) {
                if (isPureVec(o0.as<Vec>(), o1.as<Vec>())) {
                  if (isConsecutive(1, o0.as<Reg>(), o1.as<Reg>())) {
                    if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16, DT::kS32, DT::kU32, DT::kF32, DT::kS64, DT::kU64, DT::kF64, DT::kS8, DT::kU8))) {
                      opcode = 0xF4000A0Du;
                      opcode |= sz << 6u;
                      goto Emit_R0At12Of4Hi22_MemBaseAt16_NoCond;
                    }
                  }
                }
              }
            }
          }

          if (mem->indexType() == RegType::kGp32) {
            if (!mem->offsetLo32()) {
              if ((mem->indexId() < 13u) || (mem->indexId() == 14u)) {
                if (mem->isPostIndex()) {
                  if (isPureVec(o0.as<Vec>(), o1.as<Vec>())) {
                    if (isConsecutive(1, o0.as<Reg>(), o1.as<Reg>())) {
                      if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16, DT::kS32, DT::kU32, DT::kF32, DT::kS64, DT::kU64, DT::kF64, DT::kS8, DT::kU8))) {
                        opcode = 0xF4000A00u;
                        opcode |= sz << 6u;
                        goto Emit_R0At12Of4Hi22_MemBaseAt16_MemUIndexAt0_NoCond;
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }

      if (sgn.test<kOpRegD, kOpRegD, kOpRegD, kOpMemB>()) {
        mem = &o3.as<Mem>();

        if (mem->baseId() < 15u) {
          if (!mem->hasIndex()) {
            if (!mem->offsetLo32()) {
              if (mem->isFixedOffset()) {
                if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>())) {
                  if (isConsecutive(1, o0.as<Reg>(), o1.as<Reg>(), o2.as<Reg>())) {
                    if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16, DT::kS32, DT::kU32, DT::kF32, DT::kS64, DT::kU64, DT::kF64, DT::kS8, DT::kU8))) {
                      opcode = 0xF400060Fu;
                      opcode |= sz << 6u;
                      goto Emit_R0At12Of4Hi22_MemBaseAt16_NoCond;
                    }
                  }
                }
              }
            }

            if (uint32_t(mem->offsetLo32()) == 24u) {
              if (mem->isPostIndex()) {
                if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>())) {
                  if (isConsecutive(1, o0.as<Reg>(), o1.as<Reg>(), o2.as<Reg>())) {
                    if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16, DT::kS32, DT::kU32, DT::kF32, DT::kS64, DT::kU64, DT::kF64, DT::kS8, DT::kU8))) {
                      opcode = 0xF400060Du;
                      opcode |= sz << 6u;
                      goto Emit_R0At12Of4Hi22_MemBaseAt16_NoCond;
                    }
                  }
                }
              }
            }
          }

          if (mem->indexType() == RegType::kGp32) {
            if (!mem->offsetLo32()) {
              if ((mem->indexId() < 13u) || (mem->indexId() == 14u)) {
                if (mem->isPostIndex()) {
                  if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>())) {
                    if (isConsecutive(1, o0.as<Reg>(), o1.as<Reg>(), o2.as<Reg>())) {
                      if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16, DT::kS32, DT::kU32, DT::kF32, DT::kS64, DT::kU64, DT::kF64, DT::kS8, DT::kU8))) {
                        opcode = 0xF4000600u;
                        opcode |= sz << 6u;
                        goto Emit_R0At12Of4Hi22_MemBaseAt16_MemUIndexAt0_NoCond;
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }

      if (sgn.test<kOpRegD, kOpRegD, kOpRegD, kOpRegD, kOpMemB>()) {
        mem = &o4.as<Mem>();

        if (mem->baseId() < 15u) {
          if (!mem->hasIndex()) {
            if (!mem->offsetLo32()) {
              if (mem->isFixedOffset()) {
                if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>(), o3.as<Vec>())) {
                  if (isConsecutive(1, o0.as<Reg>(), o1.as<Reg>(), o2.as<Reg>(), o3.as<Reg>())) {
                    if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16, DT::kS32, DT::kU32, DT::kF32, DT::kS64, DT::kU64, DT::kF64, DT::kS8, DT::kU8))) {
                      opcode = 0xF400020Fu;
                      opcode |= sz << 6u;
                      goto Emit_R0At12Of4Hi22_MemBaseAt16_NoCond;
                    }
                  }
                }
              }
            }

            if (uint32_t(mem->offsetLo32()) == 32u) {
              if (mem->isPostIndex()) {
                if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>(), o3.as<Vec>())) {
                  if (isConsecutive(1, o0.as<Reg>(), o1.as<Reg>(), o2.as<Reg>(), o3.as<Reg>())) {
                    if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16, DT::kS32, DT::kU32, DT::kF32, DT::kS64, DT::kU64, DT::kF64, DT::kS8, DT::kU8))) {
                      opcode = 0xF400020Du;
                      opcode |= sz << 6u;
                      goto Emit_R0At12Of4Hi22_MemBaseAt16_NoCond;
                    }
                  }
                }
              }
            }
          }

          if (mem->indexType() == RegType::kGp32) {
            if (!mem->offsetLo32()) {
              if ((mem->indexId() < 13u) || (mem->indexId() == 14u)) {
                if (mem->isPostIndex()) {
                  if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>(), o3.as<Vec>())) {
                    if (isConsecutive(1, o0.as<Reg>(), o1.as<Reg>(), o2.as<Reg>(), o3.as<Reg>())) {
                      if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16, DT::kS32, DT::kU32, DT::kF32, DT::kS64, DT::kU64, DT::kF64, DT::kS8, DT::kU8))) {
                        opcode = 0xF4000200u;
                        opcode |= sz << 6u;
                        goto Emit_R0At12Of4Hi22_MemBaseAt16_MemUIndexAt0_NoCond;
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }

      break;
    }

    case 160: {
      // Instruction 'vst2'.
      uint32_t sz = szFromDt(dtBits);
      if (sgn.test<kOpRegD, kOpRegD, kOpMemB>()) {
        mem = &o2.as<Mem>();

        if (mem->baseId() < 15u) {
          if (!mem->hasIndex()) {
            if (!mem->offsetLo32()) {
              if (mem->isFixedOffset()) {
                if (isPureVec(o0.as<Vec>(), o1.as<Vec>())) {
                  if (isConsecutive(1, o0.as<Reg>(), o1.as<Reg>())) {
                    if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16, DT::kS32, DT::kU32, DT::kF32, DT::kS8, DT::kU8))) {
                      opcode = 0xF400080Fu;
                      opcode |= sz << 6u;
                      goto Emit_R0At12Of4Hi22_MemBaseAt16_NoCond;
                    }
                  }

                  if (isConsecutive(2, o0.as<Reg>(), o1.as<Reg>())) {
                    if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16, DT::kS32, DT::kU32, DT::kF32, DT::kS8, DT::kU8))) {
                      opcode = 0xF400090Fu;
                      opcode |= sz << 6u;
                      goto Emit_R0At12Of4Hi22_MemBaseAt16_NoCond;
                    }
                  }
                }
              }
            }

            if (uint32_t(mem->offsetLo32()) == 16u) {
              if (mem->isPostIndex()) {
                if (isPureVec(o0.as<Vec>(), o1.as<Vec>())) {
                  if (isConsecutive(1, o0.as<Reg>(), o1.as<Reg>())) {
                    if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16, DT::kS32, DT::kU32, DT::kF32, DT::kS8, DT::kU8))) {
                      opcode = 0xF400080Du;
                      opcode |= sz << 6u;
                      goto Emit_R0At12Of4Hi22_MemBaseAt16_NoCond;
                    }
                  }

                  if (isConsecutive(2, o0.as<Reg>(), o1.as<Reg>())) {
                    if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16, DT::kS32, DT::kU32, DT::kF32, DT::kS8, DT::kU8))) {
                      opcode = 0xF400090Du;
                      opcode |= sz << 6u;
                      goto Emit_R0At12Of4Hi22_MemBaseAt16_NoCond;
                    }
                  }
                }
              }
            }
          }

          if (mem->indexType() == RegType::kGp32) {
            if (!mem->offsetLo32()) {
              if ((mem->indexId() < 13u) || (mem->indexId() == 14u)) {
                if (mem->isPostIndex()) {
                  if (isPureVec(o0.as<Vec>(), o1.as<Vec>())) {
                    if (isConsecutive(1, o0.as<Reg>(), o1.as<Reg>())) {
                      if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16, DT::kS32, DT::kU32, DT::kF32, DT::kS8, DT::kU8))) {
                        opcode = 0xF4000800u;
                        opcode |= sz << 6u;
                        goto Emit_R0At12Of4Hi22_MemBaseAt16_MemUIndexAt0_NoCond;
                      }
                    }

                    if (isConsecutive(2, o0.as<Reg>(), o1.as<Reg>())) {
                      if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16, DT::kS32, DT::kU32, DT::kF32, DT::kS8, DT::kU8))) {
                        opcode = 0xF4000900u;
                        opcode |= sz << 6u;
                        goto Emit_R0At12Of4Hi22_MemBaseAt16_MemUIndexAt0_NoCond;
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }

      if (sgn.test<kOpRegD, kOpRegD, kOpRegD, kOpRegD, kOpMemB>()) {
        mem = &o4.as<Mem>();

        if (mem->baseId() < 15u) {
          if (!mem->hasIndex()) {
            if (!mem->offsetLo32()) {
              if (mem->isFixedOffset()) {
                if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>(), o3.as<Vec>())) {
                  if (isConsecutive(1, o0.as<Reg>(), o1.as<Reg>(), o2.as<Reg>(), o3.as<Reg>())) {
                    if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16, DT::kS32, DT::kU32, DT::kF32, DT::kS8, DT::kU8))) {
                      opcode = 0xF400030Fu;
                      opcode |= sz << 6u;
                      goto Emit_R0At12Of4Hi22_MemBaseAt16_NoCond;
                    }
                  }
                }
              }
            }

            if (uint32_t(mem->offsetLo32()) == 32u) {
              if (mem->isPostIndex()) {
                if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>(), o3.as<Vec>())) {
                  if (isConsecutive(1, o0.as<Reg>(), o1.as<Reg>(), o2.as<Reg>(), o3.as<Reg>())) {
                    if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16, DT::kS32, DT::kU32, DT::kF32, DT::kS8, DT::kU8))) {
                      opcode = 0xF400030Du;
                      opcode |= sz << 6u;
                      goto Emit_R0At12Of4Hi22_MemBaseAt16_NoCond;
                    }
                  }
                }
              }
            }
          }

          if (mem->indexType() == RegType::kGp32) {
            if (!mem->offsetLo32()) {
              if ((mem->indexId() < 13u) || (mem->indexId() == 14u)) {
                if (mem->isPostIndex()) {
                  if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>(), o3.as<Vec>())) {
                    if (isConsecutive(1, o0.as<Reg>(), o1.as<Reg>(), o2.as<Reg>(), o3.as<Reg>())) {
                      if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16, DT::kS32, DT::kU32, DT::kF32, DT::kS8, DT::kU8))) {
                        opcode = 0xF4000300u;
                        opcode |= sz << 6u;
                        goto Emit_R0At12Of4Hi22_MemBaseAt16_MemUIndexAt0_NoCond;
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }

      break;
    }

    case 161: {
      // Instruction 'vst3'.
      uint32_t sz = szFromDt(dtBits);
      if (sgn.test<kOpRegD, kOpRegD, kOpRegD, kOpMemB>()) {
        mem = &o3.as<Mem>();

        if (mem->baseId() < 15u) {
          if (!mem->hasIndex()) {
            if (!mem->offsetLo32()) {
              if (mem->isFixedOffset()) {
                if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>())) {
                  if (isConsecutive(1, o0.as<Reg>(), o1.as<Reg>(), o2.as<Reg>())) {
                    if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16, DT::kS32, DT::kU32, DT::kF32, DT::kS8, DT::kU8))) {
                      opcode = 0xF400040Fu;
                      opcode |= sz << 6u;
                      goto Emit_R0At12Of4Hi22_MemBaseAt16_NoCond;
                    }
                  }

                  if (isConsecutive(2, o0.as<Reg>(), o1.as<Reg>(), o2.as<Reg>())) {
                    if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16, DT::kS32, DT::kU32, DT::kF32, DT::kS8, DT::kU8))) {
                      opcode = 0xF400052Fu;
                      opcode |= sz << 6u;
                      goto Emit_R0At12Of4Hi22_MemBaseAt16_NoCond;
                    }
                  }
                }
              }
            }

            if (uint32_t(mem->offsetLo32()) == 24u) {
              if (mem->isPostIndex()) {
                if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>())) {
                  if (isConsecutive(1, o0.as<Reg>(), o1.as<Reg>(), o2.as<Reg>())) {
                    if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16, DT::kS32, DT::kU32, DT::kF32, DT::kS8, DT::kU8))) {
                      opcode = 0xF400040Du;
                      opcode |= sz << 6u;
                      goto Emit_R0At12Of4Hi22_MemBaseAt16_NoCond;
                    }
                  }

                  if (isConsecutive(2, o0.as<Reg>(), o1.as<Reg>(), o2.as<Reg>())) {
                    if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16, DT::kS32, DT::kU32, DT::kF32, DT::kS8, DT::kU8))) {
                      opcode = 0xF400052Du;
                      opcode |= sz << 6u;
                      goto Emit_R0At12Of4Hi22_MemBaseAt16_NoCond;
                    }
                  }
                }
              }
            }
          }

          if (mem->indexType() == RegType::kGp32) {
            if (!mem->offsetLo32()) {
              if ((mem->indexId() < 13u) || (mem->indexId() == 14u)) {
                if (mem->isPostIndex()) {
                  if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>())) {
                    if (isConsecutive(1, o0.as<Reg>(), o1.as<Reg>(), o2.as<Reg>())) {
                      if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16, DT::kS32, DT::kU32, DT::kF32, DT::kS8, DT::kU8))) {
                        opcode = 0xF4000400u;
                        opcode |= sz << 6u;
                        goto Emit_R0At12Of4Hi22_MemBaseAt16_MemUIndexAt0_NoCond;
                      }
                    }

                    if (isConsecutive(2, o0.as<Reg>(), o1.as<Reg>(), o2.as<Reg>())) {
                      if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16, DT::kS32, DT::kU32, DT::kF32, DT::kS8, DT::kU8))) {
                        opcode = 0xF4000520u;
                        opcode |= sz << 6u;
                        goto Emit_R0At12Of4Hi22_MemBaseAt16_MemUIndexAt0_NoCond;
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }

      break;
    }

    case 162: {
      // Instruction 'vst4'.
      uint32_t sz = szFromDt(dtBits);
      if (sgn.test<kOpRegD, kOpRegD, kOpRegD, kOpRegD, kOpMemB>()) {
        mem = &o4.as<Mem>();

        if (mem->baseId() < 15u) {
          if (!mem->hasIndex()) {
            if (!mem->offsetLo32()) {
              if (mem->isFixedOffset()) {
                if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>(), o3.as<Vec>())) {
                  if (isConsecutive(1, o0.as<Reg>(), o1.as<Reg>(), o2.as<Reg>(), o3.as<Reg>())) {
                    if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16, DT::kS32, DT::kU32, DT::kF32, DT::kS8, DT::kU8))) {
                      opcode = 0xF400000Fu;
                      opcode |= sz << 6u;
                      goto Emit_R0At12Of4Hi22_MemBaseAt16_NoCond;
                    }
                  }

                  if (isConsecutive(2, o0.as<Reg>(), o1.as<Reg>(), o2.as<Reg>(), o3.as<Reg>())) {
                    if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16, DT::kS32, DT::kU32, DT::kF32, DT::kS8, DT::kU8))) {
                      opcode = 0xF400010Fu;
                      opcode |= sz << 6u;
                      goto Emit_R0At12Of4Hi22_MemBaseAt16_NoCond;
                    }
                  }
                }
              }
            }

            if (uint32_t(mem->offsetLo32()) == 32u) {
              if (mem->isPostIndex()) {
                if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>(), o3.as<Vec>())) {
                  if (isConsecutive(1, o0.as<Reg>(), o1.as<Reg>(), o2.as<Reg>(), o3.as<Reg>())) {
                    if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16, DT::kS32, DT::kU32, DT::kF32, DT::kS8, DT::kU8))) {
                      opcode = 0xF400000Du;
                      opcode |= sz << 6u;
                      goto Emit_R0At12Of4Hi22_MemBaseAt16_NoCond;
                    }
                  }

                  if (isConsecutive(2, o0.as<Reg>(), o1.as<Reg>(), o2.as<Reg>(), o3.as<Reg>())) {
                    if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16, DT::kS32, DT::kU32, DT::kF32, DT::kS8, DT::kU8))) {
                      opcode = 0xF400010Du;
                      opcode |= sz << 6u;
                      goto Emit_R0At12Of4Hi22_MemBaseAt16_NoCond;
                    }
                  }
                }
              }
            }
          }

          if (mem->indexType() == RegType::kGp32) {
            if (!mem->offsetLo32()) {
              if ((mem->indexId() < 13u) || (mem->indexId() == 14u)) {
                if (mem->isPostIndex()) {
                  if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>(), o3.as<Vec>())) {
                    if (isConsecutive(1, o0.as<Reg>(), o1.as<Reg>(), o2.as<Reg>(), o3.as<Reg>())) {
                      if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16, DT::kS32, DT::kU32, DT::kF32, DT::kS8, DT::kU8))) {
                        opcode = 0xF4000000u;
                        opcode |= sz << 6u;
                        goto Emit_R0At12Of4Hi22_MemBaseAt16_MemUIndexAt0_NoCond;
                      }
                    }

                    if (isConsecutive(2, o0.as<Reg>(), o1.as<Reg>(), o2.as<Reg>(), o3.as<Reg>())) {
                      if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kU16, DT::kF16, DT::kBF16, DT::kS32, DT::kU32, DT::kF32, DT::kS8, DT::kU8))) {
                        opcode = 0xF4000100u;
                        opcode |= sz << 6u;
                        goto Emit_R0At12Of4Hi22_MemBaseAt16_MemUIndexAt0_NoCond;
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }

      break;
    }

    case 163: {
      // Instruction 'vsudot'.
      if (sgn.test<kOpRegD, kOpRegD, kOpRegD>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>()) && isElementVec(o2.as<Vec>())) {
          if (isDtSingle(dtBits, DT::kU8)) {
            uint32_t i = o2.as<Vec>().as<Vec>().elementIndex();
            if (i <= 0x1) {
              opcode = 0xFE800D10u;
              opcode |= i << 5u;
              goto Emit_R0At12Of4Hi22_R1At16Of4Hi7_R2At0Of4_NoCond;
            }
          }
        }
      }

      if (sgn.test<kOpRegQ, kOpRegQ, kOpRegD>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>()) && isElementVec(o2.as<Vec>())) {
          if (isDtSingle(dtBits, DT::kU8)) {
            uint32_t i = o2.as<Vec>().as<Vec>().elementIndex();
            if (i <= 0x1) {
              opcode = 0xFE800D50u;
              opcode |= i << 5u;
              goto Emit_Q0At12Of4Hi22_Q1At16Of4Hi7_R2At0Of4_NoCond;
            }
          }
        }
      }

      break;
    }

    case 164: {
      // Instruction 'vswp'.
      if (sgn.test<kOpRegD, kOpRegD>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>())) {
          opcode = 0xF3B20000u;
          goto Emit_R0At12Of4Hi22_R1At0Of4Hi5_NoCond;
        }
      }

      if (sgn.test<kOpRegQ, kOpRegQ>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>())) {
          opcode = 0xF3B20040u;
          goto Emit_Q0At12Of4Hi22_Q1At0Of4Hi5_NoCond;
        }
      }

      break;
    }

    case 165: {
      static const uint32_t opcodeTable[] = {
        0xF3B00800u, 0xF3B00900u, 0xF3B00A00u, 0xF3B00B00u, // Instruction 'vtbl'.
        0xF3B00840u, 0xF3B00940u, 0xF3B00A40u, 0xF3B00B40u  // Instruction 'vtbx'.
      };

      const uint32_t* opcodeTablePtr = opcodeTable + uint32_t(idr.index) * 4u;

      if (sgn.test<kOpRegD, kOpRegD, kOpRegD>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>())) {
          if (isDtMultiple(dtBits, makeDtBits(DT::kS8, DT::kU8))) {
            opcode = opcodeTablePtr[0];
            goto Emit_R0At12Of4Hi22_R1At16Of4Hi7_R2At0Of4Hi5_NoCond;
          }
        }
      }

      if (sgn.test<kOpRegD, kOpRegD, kOpRegD, kOpRegD>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>(), o3.as<Vec>())) {
          if (isConsecutive(1, o1.as<Reg>(), o2.as<Reg>())) {
            if (isDtMultiple(dtBits, makeDtBits(DT::kS8, DT::kU8))) {
              opcode = opcodeTablePtr[1];
              goto Emit_R0At12Of4Hi22_R1At16Of4Hi7_R3At0Of4Hi5_NoCond;
            }
          }
        }
      }

      if (sgn.test<kOpRegD, kOpRegD, kOpRegD, kOpRegD, kOpRegD>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>(), o3.as<Vec>(), o4.as<Vec>())) {
          if (isConsecutive(1, o1.as<Reg>(), o2.as<Reg>(), o3.as<Reg>())) {
            if (isDtMultiple(dtBits, makeDtBits(DT::kS8, DT::kU8))) {
              opcode = opcodeTablePtr[2];
              goto Emit_R0At12Of4Hi22_R1At16Of4Hi7_R4At0Of4Hi5_NoCond;
            }
          }
        }
      }

      if (sgn.test<kOpRegD, kOpRegD, kOpRegD, kOpRegD, kOpRegD, kOpRegD>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>(), o3.as<Vec>(), o4.as<Vec>(), o5.as<Vec>())) {
          if (isConsecutive(1, o1.as<Reg>(), o2.as<Reg>(), o3.as<Reg>(), o4.as<Reg>())) {
            if (isDtMultiple(dtBits, makeDtBits(DT::kS8, DT::kU8))) {
              opcode = opcodeTablePtr[3];
              goto Emit_R0At12Of4Hi22_R1At16Of4Hi7_R5At0Of4Hi5_NoCond;
            }
          }
        }
      }

      break;
    }

    case 166: {
      // Instruction 'vtst'.
      uint32_t sz = szFromDt(dtBits);
      if (sgn.test<kOpRegD, kOpRegD, kOpRegD>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>())) {
          if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kS32, DT::kS8, DT::kU16, DT::kU32, DT::kU8))) {
            opcode = 0xF2000810u;
            opcode |= sz << 20u;
            goto Emit_R0At12Of4Hi22_R1At16Of4Hi7_R2At0Of4Hi5_NoCond;
          }
        }
      }

      if (sgn.test<kOpRegQ, kOpRegQ, kOpRegQ>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>())) {
          if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kS32, DT::kS8, DT::kU16, DT::kU32, DT::kU8))) {
            opcode = 0xF2000850u;
            opcode |= sz << 20u;
            goto Emit_Q0At12Of4Hi22_Q1At16Of4Hi7_Q2At0Of4Hi5_NoCond;
          }
        }
      }

      break;
    }

    case 167: {
      // Instruction 'vudot'.
      if (sgn.test<kOpRegD, kOpRegD, kOpRegD>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>())) {
          if (isDtSingle(dtBits, DT::kU8)) {
            opcode = 0xFC200D10u;
            goto Emit_R0At12Of4Hi22_R1At16Of4Hi7_R2At0Of4Hi5_NoCond;
          }
        }

        if (isPureVec(o0.as<Vec>(), o1.as<Vec>()) && isElementVec(o2.as<Vec>())) {
          if (isDtSingle(dtBits, DT::kU8)) {
            uint32_t i = o2.as<Vec>().as<Vec>().elementIndex();
            if (i <= 0x1) {
              opcode = 0xFE200D10u;
              opcode |= i << 5u;
              goto Emit_R0At12Of4Hi22_R1At16Of4Hi7_R2At0Of4_NoCond;
            }
          }
        }
      }

      if (sgn.test<kOpRegQ, kOpRegQ, kOpRegQ>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>())) {
          if (isDtSingle(dtBits, DT::kU8)) {
            opcode = 0xFC200D50u;
            goto Emit_Q0At12Of4Hi22_Q1At16Of4Hi7_Q2At0Of4Hi5_NoCond;
          }
        }
      }

      if (sgn.test<kOpRegQ, kOpRegQ, kOpRegD>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>()) && isElementVec(o2.as<Vec>())) {
          if (isDtSingle(dtBits, DT::kU8)) {
            uint32_t i = o2.as<Vec>().as<Vec>().elementIndex();
            if (i <= 0x1) {
              opcode = 0xFE200D50u;
              opcode |= i << 5u;
              goto Emit_Q0At12Of4Hi22_Q1At16Of4Hi7_R2At0Of4_NoCond;
            }
          }
        }
      }

      break;
    }

    case 168: {
      // Instruction 'vummla'.
      if (sgn.test<kOpRegQ, kOpRegQ, kOpRegQ>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>(), o2.as<Vec>())) {
          if (isDtSingle(dtBits, DT::kU8)) {
            opcode = 0xFC200C50u;
            goto Emit_Q0At12Of4Hi22_Q1At16Of4Hi7_Q2At0Of4Hi5_NoCond;
          }
        }
      }

      break;
    }

    case 169: {
      static const uint32_t opcodeTable[] = {
        0xF3B20100u, 0xF3B20080u, 0xF3B20140u, // Instruction 'vuzp'.
        0xF3B20180u, 0xF3B20080u, 0xF3B201C0u  // Instruction 'vzip'.
      };

      const uint32_t* opcodeTablePtr = opcodeTable + uint32_t(idr.index) * 3u;

      uint32_t sz = szFromDt(dtBits);
      if (sgn.test<kOpRegD, kOpRegD>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>())) {
          if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kS8, DT::kU16, DT::kU8))) {
            opcode = opcodeTablePtr[0];
            opcode |= sz << 18u;
            goto Emit_R0At12Of4Hi22_R1At0Of4Hi5_NoCond;
          }

          if (isDtMultiple(dtBits, makeDtBits(DT::kS32, DT::kU32))) {
            opcode = opcodeTablePtr[1];
            opcode |= sz << 18u;
            goto Emit_R0At12Of4Hi22_R1At0Of4Hi5_NoCond;
          }
        }
      }

      if (sgn.test<kOpRegQ, kOpRegQ>()) {
        if (isPureVec(o0.as<Vec>(), o1.as<Vec>())) {
          if (isDtMultiple(dtBits, makeDtBits(DT::kS16, DT::kS32, DT::kS8, DT::kU16, DT::kU32, DT::kU8))) {
            opcode = opcodeTablePtr[2];
            opcode |= sz << 18u;
            goto Emit_Q0At12Of4Hi22_Q1At0Of4Hi5_NoCond;
          }
        }
      }

      break;
    }

    default: {
      break;
    }
  }

  goto InvalidInstruction;

  // Emit - Bits
  // -----------

  Emit_R0At12Of4_R1At16Of4_R2At0Of4_R3At8Of4_Cond: {
    uint32_t rId0 = o0.as<Reg>().id();
    uint32_t rId1 = o1.as<Reg>().id();
    uint32_t rId2 = o2.as<Reg>().id();
    uint32_t rId3 = o3.as<Reg>().id();
    opcode |= ((rId0 & 0xF) << 12);
    opcode |= ((rId1 & 0xF) << 16);
    opcode |= (rId2 & 0xF);
    opcode |= ((rId3 & 0xF) << 8);
    opcode |= encodeCond(cc) << 28u;
    goto Emit_Op32;
  }

  Emit_R0At12Of4_R1At16Of4_R2At0Of4_Cond: {
    uint32_t rId0 = o0.as<Reg>().id();
    uint32_t rId1 = o1.as<Reg>().id();
    uint32_t rId2 = o2.as<Reg>().id();
    opcode |= ((rId0 & 0xF) << 12);
    opcode |= ((rId1 & 0xF) << 16);
    opcode |= (rId2 & 0xF);
    opcode |= encodeCond(cc) << 28u;
    goto Emit_Op32;
  }

  Emit_R0At12Of4_R1At16Of4_Cond: {
    uint32_t rId0 = o0.as<Reg>().id();
    uint32_t rId1 = o1.as<Reg>().id();
    opcode |= ((rId0 & 0xF) << 12);
    opcode |= ((rId1 & 0xF) << 16);
    opcode |= encodeCond(cc) << 28u;
    goto Emit_Op32;
  }

  Emit_R0At12Of4_Rel_Cond: {
    uint32_t rId0 = o0.as<Reg>().id();
    opcode |= ((rId0 & 0xF) << 12);
    opcode |= encodeCond(cc) << 28u;
    goto Emit_Op32_Rel;
  }

  Emit_Q0At12Of4Hi22_Q1At0Of4Hi5_NoCond: {
    uint32_t rId0 = o0.as<Reg>().id();
    uint32_t rId1 = o1.as<Reg>().id();
    opcode |= (((rId0 << 1u) & 0xF) << 12) | (((rId0 << 1u) & 0x10) << 18);
    opcode |= ((rId1 << 1u) & 0xF) | (((rId1 << 1u) & 0x10) << 1);
    goto Emit_Op32;
  }

  Emit_R0At12Of4_R1At0Of4_R2At8Of4_Cond: {
    uint32_t rId0 = o0.as<Reg>().id();
    uint32_t rId1 = o1.as<Reg>().id();
    uint32_t rId2 = o2.as<Reg>().id();
    opcode |= ((rId0 & 0xF) << 12);
    opcode |= (rId1 & 0xF);
    opcode |= ((rId2 & 0xF) << 8);
    opcode |= encodeCond(cc) << 28u;
    goto Emit_Op32;
  }

  Emit_R0At12Of4_R1At0Of4_Cond: {
    uint32_t rId0 = o0.as<Reg>().id();
    uint32_t rId1 = o1.as<Reg>().id();
    opcode |= ((rId0 & 0xF) << 12);
    opcode |= (rId1 & 0xF);
    opcode |= encodeCond(cc) << 28u;
    goto Emit_Op32;
  }

  Emit_Rel_Cond: {
    opcode |= encodeCond(cc) << 28u;
    goto Emit_Op32_Rel;
  }

  Emit_R0At12Of4_Cond: {
    uint32_t rId0 = o0.as<Reg>().id();
    opcode |= ((rId0 & 0xF) << 12);
    opcode |= encodeCond(cc) << 28u;
    goto Emit_Op32;
  }

  Emit_Cond: {
    opcode |= encodeCond(cc) << 28u;
    goto Emit_Op32;
  }

  Emit_R0At0Of4_Cond: {
    uint32_t rId0 = o0.as<Reg>().id();
    opcode |= (rId0 & 0xF);
    opcode |= encodeCond(cc) << 28u;
    goto Emit_Op32;
  }

  Emit_Rel_NoCond: {
    goto Emit_Op32_Rel;
  }

  Emit_NoCond: {
    goto Emit_Op32;
  }

  Emit_R0At16Of4_R1At0Of4_R2At8Of4_Cond: {
    uint32_t rId0 = o0.as<Reg>().id();
    uint32_t rId1 = o1.as<Reg>().id();
    uint32_t rId2 = o2.as<Reg>().id();
    opcode |= ((rId0 & 0xF) << 16);
    opcode |= (rId1 & 0xF);
    opcode |= ((rId2 & 0xF) << 8);
    opcode |= encodeCond(cc) << 28u;
    goto Emit_Op32;
  }

  Emit_R0At16Of4_R1At0Of4_Cond: {
    uint32_t rId0 = o0.as<Reg>().id();
    uint32_t rId1 = o1.as<Reg>().id();
    opcode |= ((rId0 & 0xF) << 16);
    opcode |= (rId1 & 0xF);
    opcode |= encodeCond(cc) << 28u;
    goto Emit_Op32;
  }

  Emit_R0At16Of4_Cond: {
    uint32_t rId0 = o0.as<Reg>().id();
    opcode |= ((rId0 & 0xF) << 16);
    opcode |= encodeCond(cc) << 28u;
    goto Emit_Op32;
  }

  Emit_R0At12Of4_MemBaseAt16_Cond: {
    uint32_t rId0 = o0.as<Reg>().id();
    opcode |= ((rId0 & 0xF) << 12);
    opcode |= mem->baseId() << 16;
    opcode |= encodeCond(cc) << 28u;
    goto Emit_Op32;
  }

  Emit_MemBaseAt16W21_Cond: {
    opcode |= mem->baseId() << 16;
    opcode |= uint32_t(mem->isPreIndex()) << 21;
    opcode |= encodeCond(cc) << 28u;
    goto Emit_Op32;
  }

  Emit_MemBaseAt16_Cond: {
    opcode |= mem->baseId() << 16;
    opcode |= encodeCond(cc) << 28u;
    goto Emit_Op32;
  }

  Emit_R0At12Of4_MemBaseAt16_MemSIndexAt0_SOPAt5_N5At7_P24W21_Cond: {
    uint32_t rId0 = o0.as<Reg>().id();
    opcode |= ((rId0 & 0xF) << 12);
    opcode |= mem->baseId() << 16;
    opcode |= mem->indexId() << 0;
    opcode |= 1u << 23;
    uint32_t n = mem->shift();
    uint32_t sop = uint32_t(mem->shiftOp());

    if (n > 31u)
      goto InvalidAddressScale;

    if (sop > 3u)
      goto InvalidAddress;

    opcode |= sop << 5;
    opcode |= n << 7;
    opcode |= uint32_t(mem->isPreIndex()) << 21;
    opcode |= uint32_t(!mem->isPostIndex()) << 24;
    opcode |= encodeCond(cc) << 28u;
    goto Emit_Op32;
  }

  Emit_R0At12Of4_MemBaseAt16_SOffAt0Of12_P24W21_Cond: {
    uint32_t rId0 = o0.as<Reg>().id();
    opcode |= ((rId0 & 0xF) << 12);
    opcode |= mem->baseId() << 16;
    SOffsetEncode off(*mem);
    opcode |= (off.imm() & 0xFFF);
    opcode |= off.u() << 23u;
    opcode |= uint32_t(mem->isPreIndex()) << 21;
    opcode |= uint32_t(!mem->isPostIndex()) << 24;
    opcode |= encodeCond(cc) << 28u;
    goto Emit_Op32;
  }

  Emit_R0At12Of4_MemPCRel_Cond: {
    uint32_t rId0 = o0.as<Reg>().id();
    opcode |= ((rId0 & 0xF) << 12);
    opcode |= encodeCond(cc) << 28u;
    goto Emit_Op32_MemPC;
  }

  Emit_R0At12Of4_MemBaseAt16_MemSIndexAt0_SOPAt5_N5At7_Cond: {
    uint32_t rId0 = o0.as<Reg>().id();
    opcode |= ((rId0 & 0xF) << 12);
    opcode |= mem->baseId() << 16;
    opcode |= mem->indexId() << 0;
    opcode |= 1u << 23;
    uint32_t n = mem->shift();
    uint32_t sop = uint32_t(mem->shiftOp());

    if (n > 31u)
      goto InvalidAddressScale;

    if (sop > 3u)
      goto InvalidAddress;

    opcode |= sop << 5;
    opcode |= n << 7;
    opcode |= encodeCond(cc) << 28u;
    goto Emit_Op32;
  }

  Emit_R0At12Of4_MemBaseAt16_SOffAt0Of12_Cond: {
    uint32_t rId0 = o0.as<Reg>().id();
    opcode |= ((rId0 & 0xF) << 12);
    opcode |= mem->baseId() << 16;
    SOffsetEncode off(*mem);
    opcode |= (off.imm() & 0xFFF);
    opcode |= off.u() << 23u;
    opcode |= encodeCond(cc) << 28u;
    goto Emit_Op32;
  }

  Emit_R0At12Of4_MemBaseAt16_MemSIndexAt0_P24W21_Cond: {
    uint32_t rId0 = o0.as<Reg>().id();
    opcode |= ((rId0 & 0xF) << 12);
    opcode |= mem->baseId() << 16;
    opcode |= mem->indexId() << 0;
    opcode |= 1u << 23;
    opcode |= uint32_t(mem->isPreIndex()) << 21;
    opcode |= uint32_t(!mem->isPostIndex()) << 24;
    opcode |= encodeCond(cc) << 28u;
    goto Emit_Op32;
  }

  Emit_R0At12Of4_MemBaseAt16_SOffAt0Of4_SOffAt8Of4_P24W21_Cond: {
    uint32_t rId0 = o0.as<Reg>().id();
    opcode |= ((rId0 & 0xF) << 12);
    opcode |= mem->baseId() << 16;
    SOffsetEncode off(*mem);
    opcode |= (off.imm() & 0xF) | ((off.imm() & 0xF0) << 4);
    opcode |= off.u() << 23u;
    opcode |= uint32_t(mem->isPreIndex()) << 21;
    opcode |= uint32_t(!mem->isPostIndex()) << 24;
    opcode |= encodeCond(cc) << 28u;
    goto Emit_Op32;
  }

  Emit_R0At12Of4_MemBaseAt16_MemSIndexAt0_Cond: {
    uint32_t rId0 = o0.as<Reg>().id();
    opcode |= ((rId0 & 0xF) << 12);
    opcode |= mem->baseId() << 16;
    opcode |= mem->indexId() << 0;
    opcode |= 1u << 23;
    opcode |= encodeCond(cc) << 28u;
    goto Emit_Op32;
  }

  Emit_R0At12Of4_MemBaseAt16_SOffAt0Of4_SOffAt8Of4_Cond: {
    uint32_t rId0 = o0.as<Reg>().id();
    opcode |= ((rId0 & 0xF) << 12);
    opcode |= mem->baseId() << 16;
    SOffsetEncode off(*mem);
    opcode |= (off.imm() & 0xF) | ((off.imm() & 0xF0) << 4);
    opcode |= off.u() << 23u;
    opcode |= encodeCond(cc) << 28u;
    goto Emit_Op32;
  }

  Emit_R2At12Of4_R3At16Of4_R4At0Of4_Cond: {
    uint32_t rId2 = o2.as<Reg>().id();
    uint32_t rId3 = o3.as<Reg>().id();
    uint32_t rId4 = o4.as<Reg>().id();
    opcode |= ((rId2 & 0xF) << 12);
    opcode |= ((rId3 & 0xF) << 16);
    opcode |= (rId4 & 0xF);
    opcode |= encodeCond(cc) << 28u;
    goto Emit_Op32;
  }

  Emit_R2At12Of4_R3At16Of4_R4At0Of4_NoCond: {
    uint32_t rId2 = o2.as<Reg>().id();
    uint32_t rId3 = o3.as<Reg>().id();
    uint32_t rId4 = o4.as<Reg>().id();
    opcode |= ((rId2 & 0xF) << 12);
    opcode |= ((rId3 & 0xF) << 16);
    opcode |= (rId4 & 0xF);
    goto Emit_Op32;
  }

  Emit_R0At16Of4_R1At0Of4_R2At8Of4_R3At12Of4_Cond: {
    uint32_t rId0 = o0.as<Reg>().id();
    uint32_t rId1 = o1.as<Reg>().id();
    uint32_t rId2 = o2.as<Reg>().id();
    uint32_t rId3 = o3.as<Reg>().id();
    opcode |= ((rId0 & 0xF) << 16);
    opcode |= (rId1 & 0xF);
    opcode |= ((rId2 & 0xF) << 8);
    opcode |= ((rId3 & 0xF) << 12);
    opcode |= encodeCond(cc) << 28u;
    goto Emit_Op32;
  }

  Emit_R1At0Of4_Cond: {
    uint32_t rId1 = o1.as<Reg>().id();
    opcode |= (rId1 & 0xF);
    opcode |= encodeCond(cc) << 28u;
    goto Emit_Op32;
  }

  Emit_MemBaseAt16_MemSIndexAt0_SOPAt5_N5At7_NoCond: {
    opcode |= mem->baseId() << 16;
    opcode |= mem->indexId() << 0;
    opcode |= 1u << 23;
    uint32_t n = mem->shift();
    uint32_t sop = uint32_t(mem->shiftOp());

    if (n > 31u)
      goto InvalidAddressScale;

    if (sop > 3u)
      goto InvalidAddress;

    opcode |= sop << 5;
    opcode |= n << 7;
    goto Emit_Op32;
  }

  Emit_MemBaseAt16_SOffAt0Of12_NoCond: {
    opcode |= mem->baseId() << 16;
    SOffsetEncode off(*mem);
    opcode |= (off.imm() & 0xFFF);
    opcode |= off.u() << 23u;
    goto Emit_Op32;
  }

  Emit_MemPCRel_NoCond: {
    goto Emit_Op32_MemPC;
  }

  Emit_R0At12Of4_R1At0Of4_R2At16Of4_Cond: {
    uint32_t rId0 = o0.as<Reg>().id();
    uint32_t rId1 = o1.as<Reg>().id();
    uint32_t rId2 = o2.as<Reg>().id();
    opcode |= ((rId0 & 0xF) << 12);
    opcode |= (rId1 & 0xF);
    opcode |= ((rId2 & 0xF) << 16);
    opcode |= encodeCond(cc) << 28u;
    goto Emit_Op32;
  }

  Emit_MemBaseAt16W21_NoCond: {
    opcode |= mem->baseId() << 16;
    opcode |= uint32_t(mem->isPreIndex()) << 21;
    goto Emit_Op32;
  }

  Emit_Q0At12Of4Hi22_Q1At16Of4Hi7_Q2At0Of4Hi5_NoCond: {
    uint32_t rId0 = o0.as<Reg>().id();
    uint32_t rId1 = o1.as<Reg>().id();
    uint32_t rId2 = o2.as<Reg>().id();
    opcode |= (((rId0 << 1u) & 0xF) << 12) | (((rId0 << 1u) & 0x10) << 18);
    opcode |= (((rId1 << 1u) & 0x10) << 3) | (((rId1 << 1u) & 0xF) << 16);
    opcode |= ((rId2 << 1u) & 0xF) | (((rId2 << 1u) & 0x10) << 1);
    goto Emit_Op32;
  }

  EmitW21_NoCond: {
    opcode |= uint32_t(mem->isPreIndex()) << 21;
    goto Emit_Op32;
  }

  Emit_R0At12Of4_R2At0Of4_Cond: {
    uint32_t rId0 = o0.as<Reg>().id();
    uint32_t rId2 = o2.as<Reg>().id();
    opcode |= ((rId0 & 0xF) << 12);
    opcode |= (rId2 & 0xF);
    opcode |= encodeCond(cc) << 28u;
    goto Emit_Op32;
  }

  Emit_R0At0Of4_MemBaseAt16_Cond: {
    uint32_t rId0 = o0.as<Reg>().id();
    opcode |= (rId0 & 0xF);
    opcode |= mem->baseId() << 16;
    opcode |= encodeCond(cc) << 28u;
    goto Emit_Op32;
  }

  Emit_R0At12Of4_R1At0Of4_MemBaseAt16_Cond: {
    uint32_t rId0 = o0.as<Reg>().id();
    uint32_t rId1 = o1.as<Reg>().id();
    opcode |= ((rId0 & 0xF) << 12);
    opcode |= (rId1 & 0xF);
    opcode |= mem->baseId() << 16;
    opcode |= encodeCond(cc) << 28u;
    goto Emit_Op32;
  }

  Emit_R0At12Of4Hi22_R1At16Of4Hi7_R2At0Of4Hi5_NoCond: {
    uint32_t rId0 = o0.as<Reg>().id();
    uint32_t rId1 = o1.as<Reg>().id();
    uint32_t rId2 = o2.as<Reg>().id();
    opcode |= ((rId0 & 0xF) << 12) | ((rId0 & 0x10) << 18);
    opcode |= ((rId1 & 0x10) << 3) | ((rId1 & 0xF) << 16);
    opcode |= (rId2 & 0xF) | ((rId2 & 0x10) << 1);
    goto Emit_Op32;
  }

  Emit_Q0At12Of4Hi22_R1At16Of4Hi7_R2At0Of4Hi5_NoCond: {
    uint32_t rId0 = o0.as<Reg>().id();
    uint32_t rId1 = o1.as<Reg>().id();
    uint32_t rId2 = o2.as<Reg>().id();
    opcode |= (((rId0 << 1u) & 0xF) << 12) | (((rId0 << 1u) & 0x10) << 18);
    opcode |= ((rId1 & 0x10) << 3) | ((rId1 & 0xF) << 16);
    opcode |= (rId2 & 0xF) | ((rId2 & 0x10) << 1);
    goto Emit_Op32;
  }

  Emit_R0At12Of4Lo22_R1At0Of4Lo5_Cond: {
    uint32_t rId0 = o0.as<Reg>().id();
    uint32_t rId1 = o1.as<Reg>().id();
    opcode |= ((rId0 & 0x1E) << 11) | ((rId0 & 0x1) << 22);
    opcode |= ((rId1 & 0x1E) >> 1) | ((rId1 & 0x1) << 5);
    opcode |= encodeCond(cc) << 28u;
    goto Emit_Op32;
  }

  Emit_R0At12Of4Hi22_R1At0Of4Hi5_Cond: {
    uint32_t rId0 = o0.as<Reg>().id();
    uint32_t rId1 = o1.as<Reg>().id();
    opcode |= ((rId0 & 0xF) << 12) | ((rId0 & 0x10) << 18);
    opcode |= (rId1 & 0xF) | ((rId1 & 0x10) << 1);
    opcode |= encodeCond(cc) << 28u;
    goto Emit_Op32;
  }

  Emit_R0At12Of4Lo22_R1At0Of4Lo5_NoCond: {
    uint32_t rId0 = o0.as<Reg>().id();
    uint32_t rId1 = o1.as<Reg>().id();
    opcode |= ((rId0 & 0x1E) << 11) | ((rId0 & 0x1) << 22);
    opcode |= ((rId1 & 0x1E) >> 1) | ((rId1 & 0x1) << 5);
    goto Emit_Op32;
  }

  Emit_R0At12Of4Hi22_R1At0Of4Hi5_NoCond: {
    uint32_t rId0 = o0.as<Reg>().id();
    uint32_t rId1 = o1.as<Reg>().id();
    opcode |= ((rId0 & 0xF) << 12) | ((rId0 & 0x10) << 18);
    opcode |= (rId1 & 0xF) | ((rId1 & 0x10) << 1);
    goto Emit_Op32;
  }

  Emit_R0At12Of4Hi22_R1At0Of4Hi5_R2At16Of4Hi7_NoCond: {
    uint32_t rId0 = o0.as<Reg>().id();
    uint32_t rId1 = o1.as<Reg>().id();
    uint32_t rId2 = o2.as<Reg>().id();
    opcode |= ((rId0 & 0xF) << 12) | ((rId0 & 0x10) << 18);
    opcode |= (rId1 & 0xF) | ((rId1 & 0x10) << 1);
    opcode |= ((rId2 & 0x10) << 3) | ((rId2 & 0xF) << 16);
    goto Emit_Op32;
  }

  Emit_Q0At12Of4Hi22_Q1At0Of4Hi5_Q2At16Of4Hi7_NoCond: {
    uint32_t rId0 = o0.as<Reg>().id();
    uint32_t rId1 = o1.as<Reg>().id();
    uint32_t rId2 = o2.as<Reg>().id();
    opcode |= (((rId0 << 1u) & 0xF) << 12) | (((rId0 << 1u) & 0x10) << 18);
    opcode |= ((rId1 << 1u) & 0xF) | (((rId1 << 1u) & 0x10) << 1);
    opcode |= (((rId2 << 1u) & 0x10) << 3) | (((rId2 << 1u) & 0xF) << 16);
    goto Emit_Op32;
  }

  Emit_R0At12Of4Lo22_R1At16Of4Lo7_R2At0Of4Lo5_Cond: {
    uint32_t rId0 = o0.as<Reg>().id();
    uint32_t rId1 = o1.as<Reg>().id();
    uint32_t rId2 = o2.as<Reg>().id();
    opcode |= ((rId0 & 0x1E) << 11) | ((rId0 & 0x1) << 22);
    opcode |= ((rId1 & 0x1) << 7) | ((rId1 & 0x1E) << 15);
    opcode |= ((rId2 & 0x1E) >> 1) | ((rId2 & 0x1) << 5);
    opcode |= encodeCond(cc) << 28u;
    goto Emit_Op32;
  }

  Emit_R0At12Of4Hi22_R1At16Of4Hi7_R2At0Of4Hi5_Cond: {
    uint32_t rId0 = o0.as<Reg>().id();
    uint32_t rId1 = o1.as<Reg>().id();
    uint32_t rId2 = o2.as<Reg>().id();
    opcode |= ((rId0 & 0xF) << 12) | ((rId0 & 0x10) << 18);
    opcode |= ((rId1 & 0x10) << 3) | ((rId1 & 0xF) << 16);
    opcode |= (rId2 & 0xF) | ((rId2 & 0x10) << 1);
    opcode |= encodeCond(cc) << 28u;
    goto Emit_Op32;
  }

  Emit_R0At12Of4Lo22_R1At16Of4Lo7_R2At0Of4Lo5_NoCond: {
    uint32_t rId0 = o0.as<Reg>().id();
    uint32_t rId1 = o1.as<Reg>().id();
    uint32_t rId2 = o2.as<Reg>().id();
    opcode |= ((rId0 & 0x1E) << 11) | ((rId0 & 0x1) << 22);
    opcode |= ((rId1 & 0x1) << 7) | ((rId1 & 0x1E) << 15);
    opcode |= ((rId2 & 0x1E) >> 1) | ((rId2 & 0x1) << 5);
    goto Emit_Op32;
  }

  Emit_R0At12Of4Hi22_Q1At16Of4Hi7_Q2At0Of4Hi5_NoCond: {
    uint32_t rId0 = o0.as<Reg>().id();
    uint32_t rId1 = o1.as<Reg>().id();
    uint32_t rId2 = o2.as<Reg>().id();
    opcode |= ((rId0 & 0xF) << 12) | ((rId0 & 0x10) << 18);
    opcode |= (((rId1 << 1u) & 0x10) << 3) | (((rId1 << 1u) & 0xF) << 16);
    opcode |= ((rId2 << 1u) & 0xF) | (((rId2 << 1u) & 0x10) << 1);
    goto Emit_Op32;
  }

  Emit_Q0At12Of4Hi22_Q1At16Of4Hi7_R2At0Of4Hi5_NoCond: {
    uint32_t rId0 = o0.as<Reg>().id();
    uint32_t rId1 = o1.as<Reg>().id();
    uint32_t rId2 = o2.as<Reg>().id();
    opcode |= (((rId0 << 1u) & 0xF) << 12) | (((rId0 << 1u) & 0x10) << 18);
    opcode |= (((rId1 << 1u) & 0x10) << 3) | (((rId1 << 1u) & 0xF) << 16);
    opcode |= (rId2 & 0xF) | ((rId2 & 0x10) << 1);
    goto Emit_Op32;
  }

  Emit_R0At12Of4Hi22_NoCond: {
    uint32_t rId0 = o0.as<Reg>().id();
    opcode |= ((rId0 & 0xF) << 12) | ((rId0 & 0x10) << 18);
    goto Emit_Op32;
  }

  Emit_Q0At12Of4Hi22_NoCond: {
    uint32_t rId0 = o0.as<Reg>().id();
    opcode |= (((rId0 << 1u) & 0xF) << 12) | (((rId0 << 1u) & 0x10) << 18);
    goto Emit_Op32;
  }

  Emit_R0At12Of4Hi22_R1At16Of4Hi7_R2At0Of4_NoCond: {
    uint32_t rId0 = o0.as<Reg>().id();
    uint32_t rId1 = o1.as<Reg>().id();
    uint32_t rId2 = o2.as<Reg>().id();
    opcode |= ((rId0 & 0xF) << 12) | ((rId0 & 0x10) << 18);
    opcode |= ((rId1 & 0x10) << 3) | ((rId1 & 0xF) << 16);
    opcode |= (rId2 & 0xF);
    goto Emit_Op32;
  }

  Emit_Q0At12Of4Hi22_Q1At16Of4Hi7_R2At0Of4_NoCond: {
    uint32_t rId0 = o0.as<Reg>().id();
    uint32_t rId1 = o1.as<Reg>().id();
    uint32_t rId2 = o2.as<Reg>().id();
    opcode |= (((rId0 << 1u) & 0xF) << 12) | (((rId0 << 1u) & 0x10) << 18);
    opcode |= (((rId1 << 1u) & 0x10) << 3) | (((rId1 << 1u) & 0xF) << 16);
    opcode |= (rId2 & 0xF);
    goto Emit_Op32;
  }

  Emit_R0At12Of4Lo22_Cond: {
    uint32_t rId0 = o0.as<Reg>().id();
    opcode |= ((rId0 & 0x1E) << 11) | ((rId0 & 0x1) << 22);
    opcode |= encodeCond(cc) << 28u;
    goto Emit_Op32;
  }

  Emit_R0At12Of4Hi22_Cond: {
    uint32_t rId0 = o0.as<Reg>().id();
    opcode |= ((rId0 & 0xF) << 12) | ((rId0 & 0x10) << 18);
    opcode |= encodeCond(cc) << 28u;
    goto Emit_Op32;
  }

  Emit_R0At12Of4Lo22_NoCond: {
    uint32_t rId0 = o0.as<Reg>().id();
    opcode |= ((rId0 & 0x1E) << 11) | ((rId0 & 0x1) << 22);
    goto Emit_Op32;
  }

  Emit_R0At12Of4Lo22_R1At0Of4Hi5_Cond: {
    uint32_t rId0 = o0.as<Reg>().id();
    uint32_t rId1 = o1.as<Reg>().id();
    opcode |= ((rId0 & 0x1E) << 11) | ((rId0 & 0x1) << 22);
    opcode |= (rId1 & 0xF) | ((rId1 & 0x10) << 1);
    opcode |= encodeCond(cc) << 28u;
    goto Emit_Op32;
  }

  Emit_R0At12Of4Hi22_R1At0Of4Lo5_Cond: {
    uint32_t rId0 = o0.as<Reg>().id();
    uint32_t rId1 = o1.as<Reg>().id();
    opcode |= ((rId0 & 0xF) << 12) | ((rId0 & 0x10) << 18);
    opcode |= ((rId1 & 0x1E) >> 1) | ((rId1 & 0x1) << 5);
    opcode |= encodeCond(cc) << 28u;
    goto Emit_Op32;
  }

  Emit_R0At12Of4Lo22_R1At12Of4Lo22_Cond: {
    uint32_t rId0 = o0.as<Reg>().id();
    uint32_t rId1 = o1.as<Reg>().id();
    opcode |= ((rId0 & 0x1E) << 11) | ((rId0 & 0x1) << 22);
    opcode |= ((rId1 & 0x1E) << 11) | ((rId1 & 0x1) << 22);
    opcode |= encodeCond(cc) << 28u;
    goto Emit_Op32;
  }

  Emit_R0At12Of4Hi22_R1At12Of4Hi22_Cond: {
    uint32_t rId0 = o0.as<Reg>().id();
    uint32_t rId1 = o1.as<Reg>().id();
    opcode |= ((rId0 & 0xF) << 12) | ((rId0 & 0x10) << 18);
    opcode |= ((rId1 & 0xF) << 12) | ((rId1 & 0x10) << 18);
    opcode |= encodeCond(cc) << 28u;
    goto Emit_Op32;
  }

  Emit_R0At12Of4Hi22_Q1At0Of4Hi5_NoCond: {
    uint32_t rId0 = o0.as<Reg>().id();
    uint32_t rId1 = o1.as<Reg>().id();
    opcode |= ((rId0 & 0xF) << 12) | ((rId0 & 0x10) << 18);
    opcode |= ((rId1 << 1u) & 0xF) | (((rId1 << 1u) & 0x10) << 1);
    goto Emit_Op32;
  }

  Emit_Q0At12Of4Hi22_R1At0Of4Hi5_NoCond: {
    uint32_t rId0 = o0.as<Reg>().id();
    uint32_t rId1 = o1.as<Reg>().id();
    opcode |= (((rId0 << 1u) & 0xF) << 12) | (((rId0 << 1u) & 0x10) << 18);
    opcode |= (rId1 & 0xF) | ((rId1 & 0x10) << 1);
    goto Emit_Op32;
  }

  Emit_R0At12Of4Lo22_R1At0Of4Hi5_NoCond: {
    uint32_t rId0 = o0.as<Reg>().id();
    uint32_t rId1 = o1.as<Reg>().id();
    opcode |= ((rId0 & 0x1E) << 11) | ((rId0 & 0x1) << 22);
    opcode |= (rId1 & 0xF) | ((rId1 & 0x10) << 1);
    goto Emit_Op32;
  }

  Emit_R0At16Of4Hi7_R1At12Of4_Cond: {
    uint32_t rId0 = o0.as<Reg>().id();
    uint32_t rId1 = o1.as<Reg>().id();
    opcode |= ((rId0 & 0x10) << 3) | ((rId0 & 0xF) << 16);
    opcode |= ((rId1 & 0xF) << 12);
    opcode |= encodeCond(cc) << 28u;
    goto Emit_Op32;
  }

  Emit_Q0At16Of4Hi7_R1At12Of4_Cond: {
    uint32_t rId0 = o0.as<Reg>().id();
    uint32_t rId1 = o1.as<Reg>().id();
    opcode |= (((rId0 << 1u) & 0x10) << 3) | (((rId0 << 1u) & 0xF) << 16);
    opcode |= ((rId1 & 0xF) << 12);
    opcode |= encodeCond(cc) << 28u;
    goto Emit_Op32;
  }

  Emit_Q0At12Of4Hi22_Q1At16Of4Hi7_R2At0Of3_NoCond: {
    uint32_t rId0 = o0.as<Reg>().id();
    uint32_t rId1 = o1.as<Reg>().id();
    uint32_t rId2 = o2.as<Reg>().id();
    opcode |= (((rId0 << 1u) & 0xF) << 12) | (((rId0 << 1u) & 0x10) << 18);
    opcode |= (((rId1 << 1u) & 0x10) << 3) | (((rId1 << 1u) & 0xF) << 16);
    opcode |= (rId2 & 0x7);
    goto Emit_Op32;
  }

  Emit_R0At12Of4Hi22_R1At16Of4Lo7_R2At0Of4Lo5_NoCond: {
    uint32_t rId0 = o0.as<Reg>().id();
    uint32_t rId1 = o1.as<Reg>().id();
    uint32_t rId2 = o2.as<Reg>().id();
    opcode |= ((rId0 & 0xF) << 12) | ((rId0 & 0x10) << 18);
    opcode |= ((rId1 & 0x1) << 7) | ((rId1 & 0x1E) << 15);
    opcode |= ((rId2 & 0x1E) >> 1) | ((rId2 & 0x1) << 5);
    goto Emit_Op32;
  }

  Emit_R0At12Of4Hi22_R1At16Of4Lo7_R2At0Of3Lo5_NoCond: {
    uint32_t rId0 = o0.as<Reg>().id();
    uint32_t rId1 = o1.as<Reg>().id();
    uint32_t rId2 = o2.as<Reg>().id();
    opcode |= ((rId0 & 0xF) << 12) | ((rId0 & 0x10) << 18);
    opcode |= ((rId1 & 0x1) << 7) | ((rId1 & 0x1E) << 15);
    opcode |= ((rId2 & 0xE) >> 1) | ((rId2 & 0x1) << 5);
    goto Emit_Op32;
  }

  Emit_Q0At12Of4Hi22_R1At16Of4Hi7_R2At0Of3_NoCond: {
    uint32_t rId0 = o0.as<Reg>().id();
    uint32_t rId1 = o1.as<Reg>().id();
    uint32_t rId2 = o2.as<Reg>().id();
    opcode |= (((rId0 << 1u) & 0xF) << 12) | (((rId0 << 1u) & 0x10) << 18);
    opcode |= ((rId1 & 0x10) << 3) | ((rId1 & 0xF) << 16);
    opcode |= (rId2 & 0x7);
    goto Emit_Op32;
  }

  Emit_R0At12Of4Hi22_MemBaseAt16_NoCond: {
    uint32_t rId0 = o0.as<Reg>().id();
    opcode |= ((rId0 & 0xF) << 12) | ((rId0 & 0x10) << 18);
    opcode |= mem->baseId() << 16;
    goto Emit_Op32;
  }

  Emit_R0At12Of4Hi22_MemBaseAt16_MemUIndexAt0_NoCond: {
    uint32_t rId0 = o0.as<Reg>().id();
    opcode |= ((rId0 & 0xF) << 12) | ((rId0 & 0x10) << 18);
    opcode |= mem->baseId() << 16;
    opcode |= mem->indexId() << 0;
    goto Emit_Op32;
  }

  Emit_R0At12Of4Lo22_MemBaseAt16_SOffAt0Of8Mul4_Cond: {
    uint32_t rId0 = o0.as<Reg>().id();
    opcode |= ((rId0 & 0x1E) << 11) | ((rId0 & 0x1) << 22);
    opcode |= mem->baseId() << 16;
    SOffsetEncode off(*mem);
    opcode |= ((off.imm() & 0x3FC) >> 2);
    opcode |= off.u() << 23u;
    opcode |= encodeCond(cc) << 28u;
    goto Emit_Op32;
  }

  Emit_R0At12Of4Lo22_MemPCRel_Cond: {
    uint32_t rId0 = o0.as<Reg>().id();
    opcode |= ((rId0 & 0x1E) << 11) | ((rId0 & 0x1) << 22);
    opcode |= encodeCond(cc) << 28u;
    goto Emit_Op32_MemPC;
  }

  Emit_R0At12Of4Hi22_MemBaseAt16_SOffAt0Of8Mul4_Cond: {
    uint32_t rId0 = o0.as<Reg>().id();
    opcode |= ((rId0 & 0xF) << 12) | ((rId0 & 0x10) << 18);
    opcode |= mem->baseId() << 16;
    SOffsetEncode off(*mem);
    opcode |= ((off.imm() & 0x3FC) >> 2);
    opcode |= off.u() << 23u;
    opcode |= encodeCond(cc) << 28u;
    goto Emit_Op32;
  }

  Emit_R0At12Of4Hi22_MemPCRel_Cond: {
    uint32_t rId0 = o0.as<Reg>().id();
    opcode |= ((rId0 & 0xF) << 12) | ((rId0 & 0x10) << 18);
    opcode |= encodeCond(cc) << 28u;
    goto Emit_Op32_MemPC;
  }

  Emit_R0At12Of4Lo22_MemBaseAt16_SOffAt0Of8Mul2_NoCond: {
    uint32_t rId0 = o0.as<Reg>().id();
    opcode |= ((rId0 & 0x1E) << 11) | ((rId0 & 0x1) << 22);
    opcode |= mem->baseId() << 16;
    SOffsetEncode off(*mem);
    opcode |= ((off.imm() & 0x1FE) >> 1);
    opcode |= off.u() << 23u;
    goto Emit_Op32;
  }

  Emit_R0At12Of4Lo22_MemPCRel_NoCond: {
    uint32_t rId0 = o0.as<Reg>().id();
    opcode |= ((rId0 & 0x1E) << 11) | ((rId0 & 0x1) << 22);
    goto Emit_Op32_MemPC;
  }

  Emit_R0At12Of4Hi22_R1At16Of4Hi7_R2At0Of3_NoCond: {
    uint32_t rId0 = o0.as<Reg>().id();
    uint32_t rId1 = o1.as<Reg>().id();
    uint32_t rId2 = o2.as<Reg>().id();
    opcode |= ((rId0 & 0xF) << 12) | ((rId0 & 0x10) << 18);
    opcode |= ((rId1 & 0x10) << 3) | ((rId1 & 0xF) << 16);
    opcode |= (rId2 & 0x7);
    goto Emit_Op32;
  }

  Emit_Q0At12Of4Hi22_R1At16Of4Hi7_R2At0Of4_NoCond: {
    uint32_t rId0 = o0.as<Reg>().id();
    uint32_t rId1 = o1.as<Reg>().id();
    uint32_t rId2 = o2.as<Reg>().id();
    opcode |= (((rId0 << 1u) & 0xF) << 12) | (((rId0 << 1u) & 0x10) << 18);
    opcode |= ((rId1 & 0x10) << 3) | ((rId1 & 0xF) << 16);
    opcode |= (rId2 & 0xF);
    goto Emit_Op32;
  }

  Emit_R0At12Of4_R1At16Of4Lo7_NoCond: {
    uint32_t rId0 = o0.as<Reg>().id();
    uint32_t rId1 = o1.as<Reg>().id();
    opcode |= ((rId0 & 0xF) << 12);
    opcode |= ((rId1 & 0x1) << 7) | ((rId1 & 0x1E) << 15);
    goto Emit_Op32;
  }

  Emit_R0At16Of4Lo7_R1At12Of4_NoCond: {
    uint32_t rId0 = o0.as<Reg>().id();
    uint32_t rId1 = o1.as<Reg>().id();
    opcode |= ((rId0 & 0x1) << 7) | ((rId0 & 0x1E) << 15);
    opcode |= ((rId1 & 0xF) << 12);
    goto Emit_Op32;
  }

  Emit_R0At12Of4_R1At16Of4Lo7_Cond: {
    uint32_t rId0 = o0.as<Reg>().id();
    uint32_t rId1 = o1.as<Reg>().id();
    opcode |= ((rId0 & 0xF) << 12);
    opcode |= ((rId1 & 0x1) << 7) | ((rId1 & 0x1E) << 15);
    opcode |= encodeCond(cc) << 28u;
    goto Emit_Op32;
  }

  Emit_R0At16Of4Lo7_R1At12Of4_Cond: {
    uint32_t rId0 = o0.as<Reg>().id();
    uint32_t rId1 = o1.as<Reg>().id();
    opcode |= ((rId0 & 0x1) << 7) | ((rId0 & 0x1E) << 15);
    opcode |= ((rId1 & 0xF) << 12);
    opcode |= encodeCond(cc) << 28u;
    goto Emit_Op32;
  }

  Emit_R0At12Of4_R1At16Of4_R2At0Of4Lo5_Cond: {
    uint32_t rId0 = o0.as<Reg>().id();
    uint32_t rId1 = o1.as<Reg>().id();
    uint32_t rId2 = o2.as<Reg>().id();
    opcode |= ((rId0 & 0xF) << 12);
    opcode |= ((rId1 & 0xF) << 16);
    opcode |= ((rId2 & 0x1E) >> 1) | ((rId2 & 0x1) << 5);
    opcode |= encodeCond(cc) << 28u;
    goto Emit_Op32;
  }

  Emit_R0At0Of4Lo5_R2At12Of4_R3At16Of4_Cond: {
    uint32_t rId0 = o0.as<Reg>().id();
    uint32_t rId2 = o2.as<Reg>().id();
    uint32_t rId3 = o3.as<Reg>().id();
    opcode |= ((rId0 & 0x1E) >> 1) | ((rId0 & 0x1) << 5);
    opcode |= ((rId2 & 0xF) << 12);
    opcode |= ((rId3 & 0xF) << 16);
    opcode |= encodeCond(cc) << 28u;
    goto Emit_Op32;
  }

  Emit_R0At12Of4_R1At16Of4_R2At0Of4Hi5_Cond: {
    uint32_t rId0 = o0.as<Reg>().id();
    uint32_t rId1 = o1.as<Reg>().id();
    uint32_t rId2 = o2.as<Reg>().id();
    opcode |= ((rId0 & 0xF) << 12);
    opcode |= ((rId1 & 0xF) << 16);
    opcode |= (rId2 & 0xF) | ((rId2 & 0x10) << 1);
    opcode |= encodeCond(cc) << 28u;
    goto Emit_Op32;
  }

  Emit_R0At0Of4Hi5_R1At12Of4_R2At16Of4_Cond: {
    uint32_t rId0 = o0.as<Reg>().id();
    uint32_t rId1 = o1.as<Reg>().id();
    uint32_t rId2 = o2.as<Reg>().id();
    opcode |= (rId0 & 0xF) | ((rId0 & 0x10) << 1);
    opcode |= ((rId1 & 0xF) << 12);
    opcode |= ((rId2 & 0xF) << 16);
    opcode |= encodeCond(cc) << 28u;
    goto Emit_Op32;
  }

  Emit_R0At12Of4_R1At16Of4Hi7_Cond: {
    uint32_t rId0 = o0.as<Reg>().id();
    uint32_t rId1 = o1.as<Reg>().id();
    opcode |= ((rId0 & 0xF) << 12);
    opcode |= ((rId1 & 0x10) << 3) | ((rId1 & 0xF) << 16);
    opcode |= encodeCond(cc) << 28u;
    goto Emit_Op32;
  }

  Emit_R0At12Of4Hi22_R1At0Of4At16Of4Hi5Hi7_NoCond: {
    uint32_t rId0 = o0.as<Reg>().id();
    uint32_t rId1 = o1.as<Reg>().id();
    opcode |= ((rId0 & 0xF) << 12) | ((rId0 & 0x10) << 18);
    opcode |= (rId1 & 0xF) | ((rId1 & 0x10) << 1) | ((rId1 & 0x10) << 3) | ((rId1 & 0xF) << 16);
    goto Emit_Op32;
  }

  Emit_Q0At12Of4Hi22_Q1At0Of4At16Of4Hi5Hi7_NoCond: {
    uint32_t rId0 = o0.as<Reg>().id();
    uint32_t rId1 = o1.as<Reg>().id();
    opcode |= (((rId0 << 1u) & 0xF) << 12) | (((rId0 << 1u) & 0x10) << 18);
    opcode |= ((rId1 << 1u) & 0xF) | (((rId1 << 1u) & 0x10) << 1) | (((rId1 << 1u) & 0x10) << 3) | (((rId1 << 1u) & 0xF) << 16);
    goto Emit_Op32;
  }

  Emit_R0At12Of4Hi22_R1At16Of4Hi7_R3At0Of4Hi5_NoCond: {
    uint32_t rId0 = o0.as<Reg>().id();
    uint32_t rId1 = o1.as<Reg>().id();
    uint32_t rId3 = o3.as<Reg>().id();
    opcode |= ((rId0 & 0xF) << 12) | ((rId0 & 0x10) << 18);
    opcode |= ((rId1 & 0x10) << 3) | ((rId1 & 0xF) << 16);
    opcode |= (rId3 & 0xF) | ((rId3 & 0x10) << 1);
    goto Emit_Op32;
  }

  Emit_R0At12Of4Hi22_R1At16Of4Hi7_R4At0Of4Hi5_NoCond: {
    uint32_t rId0 = o0.as<Reg>().id();
    uint32_t rId1 = o1.as<Reg>().id();
    uint32_t rId4 = o4.as<Reg>().id();
    opcode |= ((rId0 & 0xF) << 12) | ((rId0 & 0x10) << 18);
    opcode |= ((rId1 & 0x10) << 3) | ((rId1 & 0xF) << 16);
    opcode |= (rId4 & 0xF) | ((rId4 & 0x10) << 1);
    goto Emit_Op32;
  }

  Emit_R0At12Of4Hi22_R1At16Of4Hi7_R5At0Of4Hi5_NoCond: {
    uint32_t rId0 = o0.as<Reg>().id();
    uint32_t rId1 = o1.as<Reg>().id();
    uint32_t rId5 = o5.as<Reg>().id();
    opcode |= ((rId0 & 0xF) << 12) | ((rId0 & 0x10) << 18);
    opcode |= ((rId1 & 0x10) << 3) | ((rId1 & 0xF) << 16);
    opcode |= (rId5 & 0xF) | ((rId5 & 0x10) << 1);
    goto Emit_Op32;
  }
  // ----------------------------------------------------------------------------
  // ${a32::Assembler::Impl:End}

  // Emit - Opcode - Label or Imm (Branch & Call)
  // --------------------------------------------

  Emit_Op32_Rel: {
    if (rel->isLabel()) {
      uint32_t labelId = rel->as<Label>().id();
      int64_t labelOffset = -int32_t(kPCOffset);

      // ERROR: If the label is not valid.
      LabelEntry* label = _code->labelEntry(labelId);
      if (ASMJIT_UNLIKELY(!label))
        goto InvalidLabel;

      // Avoid creating a relocation record if the label has been already bound in the current section.
      if (label->isBoundTo(_section)) {
        // Label bound to the current section.
        offsetValue = label->offset() - uint64_t(offset()) + uint64_t(labelOffset);
        goto Emit_Op32_KnownDisplacement;
      }

      // LINK: Record a non-bound label or a label bound to a different section.
      size_t codeOffset = writer.offsetFrom(_bufferData);
      LabelLink* link = _code->newLabelLink(label, _section->id(), codeOffset, intptr_t(labelOffset), offsetFormat);

      if (ASMJIT_UNLIKELY(!link))
        goto OutOfMemory;

      goto Emit_Op32;
    }

    if (rel->isImm()) {
      // TODO:
    }

    goto InvalidInstruction;
  }

  // Emit - Opcode - Memory Operand (PC)
  // -----------------------------------

  Emit_Op32_MemPC: {
    if (mem->hasBaseLabel()) {
      // Using offsetLo32() is safe as the offset cannot be 64-bit long if Label is used.
      uint32_t labelId = mem->baseId();
      int64_t labelOffset = int64_t(mem->offsetLo32()) - int64_t(kPCOffset);

      // ERROR: If the label is not valid.
      LabelEntry* label = _code->labelEntry(labelId);
      if (ASMJIT_UNLIKELY(!label))
        goto InvalidLabel;

      // Avoid creating a relocation record if the label has been already bound in the current section.
      if (label->isBoundTo(_section)) {
        // Label bound to the current section.
        offsetValue = label->offset() - uint64_t(offset()) + uint64_t(labelOffset);
        goto Emit_Op32_KnownDisplacement;
      }

      // LINK: Record a non-bound label or a label bound to a different section.
      size_t codeOffset = writer.offsetFrom(_bufferData);
      LabelLink* link = _code->newLabelLink(label, _section->id(), codeOffset, intptr_t(labelOffset), offsetFormat);

      if (ASMJIT_UNLIKELY(!link))
        goto OutOfMemory;

      goto Emit_Op32;
    }

    if (mem->baseType() == RegType::kGp32) {
      if (mem->baseId() == 15u) {
        offsetValue = uint64_t(int64_t(mem->offsetLo32()));
        goto Emit_Op32_KnownDisplacement;
      }
    }

    goto InvalidInstruction;
  }

  // Emit - Opcode - With Known Displacement
  // ---------------------------------------

  Emit_Op32_KnownDisplacement: {
    uint32_t bitMask = Support::lsbMask<uint32_t>(offsetFormat.immBitCount());
    uint64_t offset64 = offsetValue;

    if (!Support::isInt32(int64_t(offset64)))
      goto InvalidDisplacement;

    uint32_t imm = uint32_t(offset64 & 0xFFFFFFFFu);
    if ((imm & Support::lsbMask<uint32_t>(offsetFormat.immDiscardLsb())) != 0)
      goto InvalidDisplacement;

    imm = Support::sar(imm, offsetFormat.immDiscardLsb());
    uint32_t u = uint32_t(int32_t(imm) >= 0);

    if (offsetFormat.hasSignBit()) {
      if (u == 0)
        imm = Support::neg(imm);

      if (imm & ~bitMask)
        goto InvalidDisplacement;
    }
    else {
      if (!Support::isEncodableOffset32(int32_t(imm), offsetFormat.immBitCount()))
        goto InvalidDisplacement;
    }

    switch (offsetFormat.type()) {
      case OffsetType::kSignedOffset: {
        opcode |= (imm & bitMask) << offsetFormat.immBitShift();
        goto Emit_Op32;
      }

      // Opcode: {.....|imm:1|..N.N|......|imm:3|....|imm:8}
      case OffsetType::kThumb32_ADR: {
        uint32_t imm8 = (imm & 0x00FFu);
        uint32_t imm3 = (imm & 0x0700u) << (12 - 8);
        uint32_t imm1 = (imm & 0x0800u) << (26 - 11);
        uint32_t n = u ^ 1u;

        opcode |= imm8 | imm3 | imm1 | (n << 21) | (n << 23);
        goto Emit_Op32;
      }

      // Opcode: {.....|imm[22]|imm[19:10]|..|ja|.|jb|imm[9:0]:0}
      case OffsetType::kThumb32_BLX:
        // The calculation is the same as `B`, but the first LSB bit must be zero, so account for that.
        imm <<= 1;
        ASMJIT_FALLTHROUGH;

      // Opcode: {.....|imm[23]|imm[20:11]|..|ja|.|jb|imm[10:0]}
      case OffsetType::kThumb32_B: {
        uint32_t ia = (imm & 0x0007FFu);
        uint32_t ib = (imm & 0x1FF800u) << (16 - 11);
        uint32_t ic = (imm & 0x800000u) << (26 - 23);
        uint32_t ja = ((~imm >> 23) ^ (imm >> 22)) & 1u;
        uint32_t jb = ((~imm >> 23) ^ (imm >> 21)) & 1u;

        opcode |= ia | ib | ic | (ja << 14) | (jb << 11);
        goto Emit_Op32;
      }

      // Opcode: {.....|imm[19]|....|imm[16:11]|..|ja|.|jb|imm[10:0]}
      case OffsetType::kThumb32_BCond: {
        uint32_t ia = (imm & 0x0007FFu);
        uint32_t ib = (imm & 0x01F800u) << (16 - 11);
        uint32_t ic = (imm & 0x080000u) << (26 - 19);
        uint32_t ja = ((~imm >> 19) ^ (imm >> 22)) & 1u;
        uint32_t jb = ((~imm >> 19) ^ (imm >> 21)) & 1u;

        opcode |= ia | ib | ic | (ja << 14) | (jb << 11);
        goto Emit_Op32;
      }

      case OffsetType::kAArch32_ADR: {
        uint32_t encodedImm;
        if (!arm::Utils::encodeAArch32Imm(imm, &encodedImm))
          goto InvalidDisplacement;

        opcode |= Support::bitMask(22) << u;
        opcode |= encodedImm << offsetFormat.immBitShift();
        goto Emit_Op32;
      }

      case OffsetType::kAArch32_U23_SignedOffset: {
        opcode |= imm << offsetFormat.immBitShift();
        opcode |= u << 23;

        goto Emit_Op32;
      }

      case OffsetType::kAArch32_U23_0To3At0_4To7At8: {
        uint32_t immLo = (imm >> 0) & 0xFu;
        uint32_t immHi = (imm >> 4) & 0xFu;

        opcode |= immLo << 0;
        opcode |= immHi << 8;
        opcode |= u << 23;

        goto Emit_Op32;
      }

      case OffsetType::kAArch32_1To24At0_0At24: {
        uint32_t immLo = (imm >> 0) & 0x1u;
        uint32_t immHi = (imm >> 1) & 0xFFFFFFu;

        opcode |= immLo << 24;
        opcode |= immHi;

        goto Emit_Op32;
      }

      default:
        goto InvalidDisplacement;
    }
  }

  // Emit - Opcode
  // -------------

  Emit_Op32: {
    writer.emit32uLE(opcode);
    goto EmitDone;
  }

  // Emit - Success
  // --------------

  EmitDone: {
    if (Support::test(options, InstOptions::kReserved)) {
#ifndef ASMJIT_NO_LOGGING
      if (_logger) {
        EmitterUtils::logInstructionEmitted(this, recombineInstId(instId, cc, dtBits), options, o0, o1, o2, opExt, 0, 0, writer.cursor());
      }
#endif
    }

    writer.done(this);

    resetState();
    return kErrorOk;
  }

  // Emit - Failure
  // --------------

#define ERROR_HANDLER(ERR) ERR: err = DebugUtils::errored(kError##ERR); goto Emit_Failed;
  ERROR_HANDLER(OutOfMemory)
  ERROR_HANDLER(InvalidAddress)
  ERROR_HANDLER(InvalidAddressScale)
  ERROR_HANDLER(InvalidDisplacement)
  ERROR_HANDLER(InvalidLabel)
  // ERROR_HANDLER(InvalidImmediate)
  ERROR_HANDLER(InvalidInstruction)
  // ERROR_HANDLER(InvalidPhysId)
  // ERROR_HANDLER(InvalidRegType)
#undef ERROR_HANDLER

  Emit_Failed: {
#ifndef ASMJIT_NO_LOGGING
    return EmitterUtils::logInstructionFailed(this, err, recombineInstId(instId, cc, dtBits), options, o0, o1, o2, opExt);
#else
    resetState();
    return reportError(err);
#endif
  }
}

// a32::Assembler - Align
// ======================

Error Assembler::align(AlignMode alignMode, uint32_t alignment) {
  constexpr uint32_t kNopT16 = 0x0000BF00u; // [10111111|00000000].
  constexpr uint32_t kNopT32 = 0xF3AF8000u; // [11110011|10101111|10000000|00000000].
  constexpr uint32_t kNopA32 = 0xE3AF8000u; // [Cond0011|00100000|11110000|00000000].

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
      uint32_t pattern = kNopA32;
      if (isInThumbMode()) {
        if (ASMJIT_UNLIKELY(offset() & 0x1u))
          return DebugUtils::errored(kErrorInvalidState);

        if (i & 0x2u) {
          writer.emit16uLE(kNopT16);
          i -= 2u;
        }
        pattern = kNopT32;
      }

      if (ASMJIT_UNLIKELY(offset() & 0x3u))
        return DebugUtils::errored(kErrorInvalidState);

      while (i >= 4u) {
        writer.emit32uLE(pattern);
        i -= 4u;
      }

      ASMJIT_ASSERT(i == 0u);
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

// a32::Assembler - Events
// =======================

Error Assembler::onAttach(CodeHolder* code) noexcept {
  ASMJIT_PROPAGATE(Base::onAttach(code));

  _instructionAlignment = _environment.isArchThumb() ? uint8_t(2) : uint8_t(4);
  assignEmitterFuncs(this);

  return kErrorOk;
}

Error Assembler::onDetach(CodeHolder* code) noexcept {
  return Base::onDetach(code);
}

ASMJIT_END_SUB_NAMESPACE

#endif // !ASMJIT_NO_AARCH32
