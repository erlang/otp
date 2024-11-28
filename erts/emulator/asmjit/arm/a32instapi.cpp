// This file is part of AsmJit project <https://asmjit.com>
//
// See asmjit.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#include "../core/api-build_p.h"
#if !defined(ASMJIT_NO_AARCH32)

#include "../core/support_p.h"
#include "../arm/a32instapi_p.h"
#include "../arm/a32instdb_p.h"

ASMJIT_BEGIN_SUB_NAMESPACE(a32)

namespace InstInternal {

// a32::InstAPI - Text
// ===================

#ifndef ASMJIT_NO_TEXT
Error instIdToString(InstId instId, String& output) noexcept {
  uint32_t realId = instId & uint32_t(InstIdParts::kRealId);
  if (ASMJIT_UNLIKELY(!Inst::isDefinedId(realId)))
    return DebugUtils::errored(kErrorInvalidInstruction);

  return InstNameUtils::decode(output, InstDB::_instNameIndexTable[realId], InstDB::_instNameStringTable);
}

InstId stringToInstId(const char* s, size_t len) noexcept {
  return InstNameUtils::find(s, len, InstDB::instNameIndex, InstDB::_instNameIndexTable, InstDB::_instNameStringTable);
}
#endif // !ASMJIT_NO_TEXT

// a32::InstAPI - Validation
// =========================

#ifndef ASMJIT_NO_VALIDATION
Error validate(const BaseInst& inst, const Operand_* operands, size_t opCount, ValidationFlags validationFlags) noexcept {
  // TODO: AArch32 tooling.
  DebugUtils::unused(inst, operands, opCount, validationFlags);

  return kErrorOk;
}
#endif // !ASMJIT_NO_VALIDATION

// a32::InstAPI - Introspection
// ============================

#ifndef ASMJIT_NO_INTROSPECTION

struct InstRWInfoRecord {
  //! RWX information for each operand.
  uint8_t rwx[Globals::kMaxOpCount];
  //! Index to InstRWFlagsRecord table.
  uint8_t rwFlagsIndex;
};

struct InstRWFlagsRecord {
  //! Read flags.
  CpuRWFlags r;
  //! Written flags.
  CpuRWFlags w;
};

// ${a32::RWInfo:Begin}
// ------------------- Automatically generated, do not edit -------------------
static const constexpr InstRWInfoRecord instRWInfoData[] = {
  #define R uint8_t(OpRWFlags::kRead)
  #define W uint8_t(OpRWFlags::kWrite)
  #define X uint8_t(OpRWFlags::kRW)

  {{ R, R, R, R, R, R }, 0}, // #0 [ref=105x]
  {{ W, R, R, R, R, R }, 1}, // #1 [ref=3x]
  {{ W, R, R, R, R, R }, 2}, // #2 [ref=1x]
  {{ W, R, R, R, R, R }, 0}, // #3 [ref=243x]
  {{ W, R, R, R, R, R }, 3}, // #4 [ref=5x]
  {{ X, R, R, R, R, R }, 0}, // #5 [ref=32x]
  {{ W, R, R, R, R, R }, 4}, // #6 [ref=11x]
  {{ R, R, R, R, R, R }, 3}, // #7 [ref=2x]
  {{ W, W, R, R, R, R }, 0}, // #8 [ref=6x]
  {{ W, R, R, R, R, R }, 5}, // #9 [ref=2x]
  {{ R, R, W, R, R, R }, 0}, // #10 [ref=2x]
  {{ R, R, W, W, R, R }, 0}, // #11 [ref=2x]
  {{ W, R, R, R, R, R }, 6}, // #12 [ref=20x]
  {{ W, R, R, R, R, R }, 7}, // #13 [ref=12x]
  {{ W, R, R, R, R, R }, 8}, // #14 [ref=1x]
  {{ X, X, R, R, R, R }, 0}, // #15 [ref=13x]
  {{ X, X, R, R, R, R }, 5}, // #16 [ref=2x]
  {{ X, X, R, R, R, R }, 6}, // #17 [ref=2x]
  {{ W, W, R, R, R, R }, 5}, // #18 [ref=2x]
  {{ R, R, R, R, R, R }, 4}, // #19 [ref=2x]
  {{ W, W, W, R, R, R }, 0}, // #20 [ref=2x]
  {{ W, W, W, W, R, R }, 0}  // #21 [ref=2x]

  #undef R
  #undef W
  #undef X
};

static const constexpr InstRWFlagsRecord instRWFlagsData[] = {
  { CpuRWFlags::kNone, CpuRWFlags::kNone }, // #0 [ref=407x]
  { CpuRWFlags::kARM_C, CpuRWFlags::kNone }, // #1 [ref=3x]
  { CpuRWFlags::kARM_C, CpuRWFlags::kARM_C | CpuRWFlags::kARM_N | CpuRWFlags::kARM_V | CpuRWFlags::kARM_Z }, // #2 [ref=1x]
  { CpuRWFlags::kNone, CpuRWFlags::kARM_C | CpuRWFlags::kARM_N | CpuRWFlags::kARM_V | CpuRWFlags::kARM_Z }, // #3 [ref=7x]
  { CpuRWFlags::kNone, CpuRWFlags::kARM_C | CpuRWFlags::kARM_N | CpuRWFlags::kARM_Z }, // #4 [ref=13x]
  { CpuRWFlags::kNone, CpuRWFlags::kARM_N | CpuRWFlags::kARM_Z }, // #5 [ref=6x]
  { CpuRWFlags::kARM_Q, CpuRWFlags::kARM_Q }, // #6 [ref=22x]
  { CpuRWFlags::kNone, CpuRWFlags::kARM_GE }, // #7 [ref=12x]
  { CpuRWFlags::kARM_GE, CpuRWFlags::kNone }  // #8 [ref=1x]
};

static const constexpr uint8_t instRWInfoIndex[] {
  0, 1, 2, 3, 4, 3, 5, 5, 3, 3, 3, 6, 3, 6, 0, 3, 3, 3, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 7, 7, 0, 0, 0, 3, 3, 3, 3, 3, 3,
  0, 0, 0, 0, 0, 0, 0, 3, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 3, 3, 3, 8, 3, 3, 0, 0, 0, 0,
  3, 3, 3, 8, 3, 3, 8, 3, 3, 3, 3, 3, 3, 3, 3, 3, 6, 3, 6, 0, 0, 0, 0, 3, 9, 3, 0xFFu, 0xFFu, 5, 3, 10, 10, 11, 11, 3, 0,
  3, 9, 3, 6, 0, 3, 6, 3, 6, 3, 3, 0, 0, 0, 0xFFu, 0, 0, 12, 3, 3, 3, 12, 12, 3, 12, 3, 3, 3, 3, 3, 3, 0, 0, 0, 0, 3, 6,
  1, 6, 3, 4, 3, 4, 13, 13, 13, 0, 1, 4, 3, 3, 14, 0, 0, 0, 0, 5, 3, 5, 5, 5, 5, 5, 5, 5, 5, 3, 3, 3, 3, 3, 3, 0, 12, 12,
  12, 12, 15, 15, 15, 15, 15, 16, 15, 15, 12, 12, 12, 12, 12, 12, 17, 17, 3, 3, 3, 3, 3, 3, 12, 12, 3, 3, 8, 18, 3, 3, 3,
  3, 3, 3, 0, 0, 0, 0, 12, 12, 13, 0, 13, 13, 0, 0, 3, 3, 3, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 3, 3, 3, 0, 0, 0, 3, 4, 0,
  3, 3, 3, 3, 3, 3, 0, 0, 19, 19, 13, 13, 13, 3, 0, 3, 3, 3, 3, 3, 3, 3, 15, 15, 16, 8, 18, 3, 3, 3, 3, 3, 3, 3, 3, 12,
  12, 13, 13, 13, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 0xFFu, 0xFFu, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
  5, 0, 0, 3, 0xFFu, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 5, 3, 5, 3, 5, 5, 5, 5, 3, 3, 3, 3, 0xFFu, 0xFFu, 0xFFu, 8, 20,
  20, 21, 21, 0, 0, 3, 3, 3, 3, 3, 5, 5, 5, 5, 3, 0xFFu, 3, 3, 3, 3, 3, 3, 3, 5, 5, 3, 0xFFu, 0xFFu, 5, 3, 3, 3, 3, 0, 0,
  3, 3, 5, 5, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 5,
  3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 5, 3, 3, 5, 5, 0, 0, 0, 0, 0, 0, 0, 3, 3, 3, 3, 3, 15, 3, 3, 15, 3, 3, 3, 3, 3, 15, 15, 0,
  0, 0
};
// ----------------------------------------------------------------------------
// ${a32::RWInfo:End}

Error queryRWInfo(const BaseInst& inst, const Operand_* operands, size_t opCount, InstRWInfo* out) noexcept {
  uint32_t instId = inst.id();
  uint32_t realId = instId & uint32_t(InstIdParts::kRealId);

  if (ASMJIT_UNLIKELY(!Inst::isDefinedId(realId)))
    return DebugUtils::errored(kErrorInvalidInstruction);

  out->_instFlags = InstRWFlags::kNone;
  out->_opCount = uint8_t(opCount);
  out->_rmFeature = 0;
  out->_extraReg.reset();

  out->_readFlags = CpuRWFlags::kNone;
  out->_writeFlags = CpuRWFlags::kNone;

  size_t index = instRWInfoIndex[realId];
  if (index < 0xFFu) {
    const InstRWInfoRecord& rwInfo = instRWInfoData[index];
    size_t rwFlagsIndex = rwInfo.rwFlagsIndex;

    out->_readFlags = instRWFlagsData[rwFlagsIndex].r;
    out->_writeFlags = instRWFlagsData[rwFlagsIndex].w;

    for (size_t i = 0; i < opCount; i++) {
      uint32_t access = rwInfo.rwx[i];

      OpRWInfo& rwOp = out->_operands[i];
      const Operand_& srcOp = operands[i];

      if (!srcOp.isRegOrRegListOrMem()) {
        rwOp.reset();
        continue;
      }

      rwOp._opFlags = OpRWFlags(access);
      rwOp._physId = BaseReg::kIdBad;
      rwOp._rmSize = 0;
      rwOp._resetReserved();

      uint64_t rByteMask = rwOp.isRead() ? 0xFFFFFFFFFFFFFFFFu : 0x0000000000000000u;
      uint64_t wByteMask = rwOp.isWrite() ? 0xFFFFFFFFFFFFFFFFu : 0x0000000000000000u;

      rwOp._readByteMask = rByteMask;
      rwOp._writeByteMask = wByteMask;
      rwOp._extendByteMask = 0;
      rwOp._consecutiveLeadCount = 0;

      if (srcOp.isMem()) {
        const Mem& memOp = srcOp.as<Mem>();

        if (memOp.hasBase()) {
          rwOp.addOpFlags(OpRWFlags::kMemBaseRead);
        }

        if (memOp.hasIndex()) {
          rwOp.addOpFlags(memOp.isPreOrPost() ? OpRWFlags::kMemIndexRW : OpRWFlags::kMemIndexRead);
        }
      }
      else if (srcOp.as<Vec>().hasElementIndex()) {
        // Only part of the vector is accessed if element index [] is used.
        uint32_t elementSize = dataTypeSize(inst.armDt());
        uint32_t elementIndex = srcOp.as<Vec>().elementIndex();

        // NOTE: DataType must be present otherwise it's impossible to calculate the access flags.
        if (!elementSize)
          return DebugUtils::errored(kErrorInvalidInstruction);

        uint64_t accessMask = uint64_t(Support::lsbMask<uint32_t>(elementSize)) << (elementIndex * elementSize);
        rwOp._readByteMask &= accessMask;
        rwOp._writeByteMask &= accessMask;
      }
    }
    return kErrorOk;
  }
  else {
    // TODO: [ARM] Not finished introspection.
    return DebugUtils::errored(kErrorInvalidState);
  }
}

Error queryFeatures(const BaseInst& inst, const Operand_* operands, size_t opCount, CpuFeatures* out) noexcept {
  // TODO: AArch32 tooling.
  DebugUtils::unused(inst, operands, opCount, out);

  return kErrorOk;
}
#endif // !ASMJIT_NO_INTROSPECTION

} // {InstInternal}

ASMJIT_END_SUB_NAMESPACE

#endif // !ASMJIT_NO_AARCH32
