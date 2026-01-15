// This file is part of AsmJit project <https://asmjit.com>
//
// See asmjit.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#include "../core/api-build_p.h"
#if !defined(ASMJIT_NO_X86)

#include "../core/cpuinfo.h"
#include "../core/instdb_p.h"
#include "../core/misc_p.h"
#include "../x86/x86instapi_p.h"
#include "../x86/x86instdb_p.h"
#include "../x86/x86opcode_p.h"
#include "../x86/x86operand.h"

ASMJIT_BEGIN_SUB_NAMESPACE(x86)

namespace InstInternal {

// x86::InstInternal - Text
// ========================

#ifndef ASMJIT_NO_TEXT
Error instIdToString(InstId instId, String& output) noexcept {
  if (ASMJIT_UNLIKELY(!Inst::isDefinedId(instId)))
    return DebugUtils::errored(kErrorInvalidInstruction);

  return InstNameUtils::decode(output, InstDB::_instNameIndexTable[instId], InstDB::_instNameStringTable);
}

InstId stringToInstId(const char* s, size_t len) noexcept {
  return InstNameUtils::find(s, len, InstDB::instNameIndex, InstDB::_instNameIndexTable, InstDB::_instNameStringTable);
}
#endif // !ASMJIT_NO_TEXT

// x86::InstInternal - Validate
// ============================

#ifndef ASMJIT_NO_VALIDATION
struct X86ValidationData {
  //! Allowed registers by \ref RegType.
  RegMask allowedRegMask[uint32_t(RegType::kMaxValue) + 1];
  uint32_t allowedMemBaseRegs;
  uint32_t allowedMemIndexRegs;
};

#define VALUE(x) \
  (x == uint32_t(RegType::kX86_GpbLo)) ? InstDB::OpFlags::kRegGpbLo : \
  (x == uint32_t(RegType::kX86_GpbHi)) ? InstDB::OpFlags::kRegGpbHi : \
  (x == uint32_t(RegType::kX86_Gpw  )) ? InstDB::OpFlags::kRegGpw   : \
  (x == uint32_t(RegType::kX86_Gpd  )) ? InstDB::OpFlags::kRegGpd   : \
  (x == uint32_t(RegType::kX86_Gpq  )) ? InstDB::OpFlags::kRegGpq   : \
  (x == uint32_t(RegType::kX86_Xmm  )) ? InstDB::OpFlags::kRegXmm   : \
  (x == uint32_t(RegType::kX86_Ymm  )) ? InstDB::OpFlags::kRegYmm   : \
  (x == uint32_t(RegType::kX86_Zmm  )) ? InstDB::OpFlags::kRegZmm   : \
  (x == uint32_t(RegType::kX86_Mm   )) ? InstDB::OpFlags::kRegMm    : \
  (x == uint32_t(RegType::kX86_KReg )) ? InstDB::OpFlags::kRegKReg  : \
  (x == uint32_t(RegType::kX86_SReg )) ? InstDB::OpFlags::kRegSReg  : \
  (x == uint32_t(RegType::kX86_CReg )) ? InstDB::OpFlags::kRegCReg  : \
  (x == uint32_t(RegType::kX86_DReg )) ? InstDB::OpFlags::kRegDReg  : \
  (x == uint32_t(RegType::kX86_St   )) ? InstDB::OpFlags::kRegSt    : \
  (x == uint32_t(RegType::kX86_Bnd  )) ? InstDB::OpFlags::kRegBnd   : \
  (x == uint32_t(RegType::kX86_Tmm  )) ? InstDB::OpFlags::kRegTmm   : \
  (x == uint32_t(RegType::kX86_Rip  )) ? InstDB::OpFlags::kNone     : InstDB::OpFlags::kNone
static const InstDB::OpFlags _x86OpFlagFromRegType[uint32_t(RegType::kMaxValue) + 1] = { ASMJIT_LOOKUP_TABLE_32(VALUE, 0) };
#undef VALUE

#define REG_MASK_FROM_REG_TYPE_X86(x) \
  (x == uint32_t(RegType::kX86_GpbLo)) ? 0x0000000Fu : \
  (x == uint32_t(RegType::kX86_GpbHi)) ? 0x0000000Fu : \
  (x == uint32_t(RegType::kX86_Gpw  )) ? 0x000000FFu : \
  (x == uint32_t(RegType::kX86_Gpd  )) ? 0x000000FFu : \
  (x == uint32_t(RegType::kX86_Gpq  )) ? 0x000000FFu : \
  (x == uint32_t(RegType::kX86_Xmm  )) ? 0x000000FFu : \
  (x == uint32_t(RegType::kX86_Ymm  )) ? 0x000000FFu : \
  (x == uint32_t(RegType::kX86_Zmm  )) ? 0x000000FFu : \
  (x == uint32_t(RegType::kX86_Mm   )) ? 0x000000FFu : \
  (x == uint32_t(RegType::kX86_KReg )) ? 0x000000FFu : \
  (x == uint32_t(RegType::kX86_SReg )) ? 0x0000007Eu : \
  (x == uint32_t(RegType::kX86_CReg )) ? 0x0000FFFFu : \
  (x == uint32_t(RegType::kX86_DReg )) ? 0x000000FFu : \
  (x == uint32_t(RegType::kX86_St   )) ? 0x000000FFu : \
  (x == uint32_t(RegType::kX86_Bnd  )) ? 0x0000000Fu : \
  (x == uint32_t(RegType::kX86_Tmm  )) ? 0x000000FFu : \
  (x == uint32_t(RegType::kX86_Rip  )) ? 0x00000001u : 0u

#define REG_MASK_FROM_REG_TYPE_X64(x) \
  (x == uint32_t(RegType::kX86_GpbLo)) ? 0x0000FFFFu : \
  (x == uint32_t(RegType::kX86_GpbHi)) ? 0x0000000Fu : \
  (x == uint32_t(RegType::kX86_Gpw  )) ? 0x0000FFFFu : \
  (x == uint32_t(RegType::kX86_Gpd  )) ? 0x0000FFFFu : \
  (x == uint32_t(RegType::kX86_Gpq  )) ? 0x0000FFFFu : \
  (x == uint32_t(RegType::kX86_Xmm  )) ? 0xFFFFFFFFu : \
  (x == uint32_t(RegType::kX86_Ymm  )) ? 0xFFFFFFFFu : \
  (x == uint32_t(RegType::kX86_Zmm  )) ? 0xFFFFFFFFu : \
  (x == uint32_t(RegType::kX86_Mm   )) ? 0x000000FFu : \
  (x == uint32_t(RegType::kX86_KReg )) ? 0x000000FFu : \
  (x == uint32_t(RegType::kX86_SReg )) ? 0x0000007Eu : \
  (x == uint32_t(RegType::kX86_CReg )) ? 0x0000FFFFu : \
  (x == uint32_t(RegType::kX86_DReg )) ? 0x0000FFFFu : \
  (x == uint32_t(RegType::kX86_St   )) ? 0x000000FFu : \
  (x == uint32_t(RegType::kX86_Bnd  )) ? 0x0000000Fu : \
  (x == uint32_t(RegType::kX86_Tmm  )) ? 0x000000FFu : \
  (x == uint32_t(RegType::kX86_Rip  )) ? 0x00000001u : 0u

#define B(RegType) (uint32_t(1) << uint32_t(RegType))

static const X86ValidationData _x86ValidationData = {
  { ASMJIT_LOOKUP_TABLE_32(REG_MASK_FROM_REG_TYPE_X86, 0) },
  B(RegType::kX86_Gpw) | B(RegType::kX86_Gpd) | B(RegType::kX86_Rip) | B(RegType::kLabelTag),
  B(RegType::kX86_Gpw) | B(RegType::kX86_Gpd) | B(RegType::kX86_Xmm) | B(RegType::kX86_Ymm) | B(RegType::kX86_Zmm)
};

static const X86ValidationData _x64ValidationData = {
  { ASMJIT_LOOKUP_TABLE_32(REG_MASK_FROM_REG_TYPE_X64, 0) },
  B(RegType::kX86_Gpd) | B(RegType::kX86_Gpq) | B(RegType::kX86_Rip) | B(RegType::kLabelTag),
  B(RegType::kX86_Gpd) | B(RegType::kX86_Gpq) | B(RegType::kX86_Xmm) | B(RegType::kX86_Ymm) | B(RegType::kX86_Zmm)
};

#undef B

#undef REG_MASK_FROM_REG_TYPE_X64
#undef REG_MASK_FROM_REG_TYPE_X86

static ASMJIT_FORCE_INLINE bool x86IsZmmOrM512(const Operand_& op) noexcept {
  return Reg::isZmm(op) || (op.isMem() && op.x86RmSize() == 64);
}

static ASMJIT_FORCE_INLINE bool x86CheckOSig(const InstDB::OpSignature& op, const InstDB::OpSignature& ref, bool& immOutOfRange) noexcept {
  // Fail if operand types are incompatible.
  InstDB::OpFlags commonFlags = op.flags() & ref.flags();

  if (!Support::test(commonFlags, InstDB::OpFlags::kOpMask)) {
    // Mark temporarily `immOutOfRange` so we can return a more descriptive error later.
    if (op.hasImm() && ref.hasImm()) {
      immOutOfRange = true;
      return true;
    }

    return false;
  }

  // Fail if some memory specific flags do not match.
  if (Support::test(commonFlags, InstDB::OpFlags::kMemMask)) {
    if (ref.hasFlag(InstDB::OpFlags::kFlagMemBase) && !op.hasFlag(InstDB::OpFlags::kFlagMemBase))
      return false;
  }

  // Fail if register indexes do not match.
  if (Support::test(commonFlags, InstDB::OpFlags::kRegMask)) {
    if (ref.regMask() && !Support::test(op.regMask(), ref.regMask()))
      return false;
  }

  return true;
}

static ASMJIT_FAVOR_SIZE Error validate(InstDB::Mode mode, const BaseInst& inst, const Operand_* operands, size_t opCount, ValidationFlags validationFlags) noexcept {
  uint32_t i;

  // Get the instruction data.
  const X86ValidationData* vd = (mode == InstDB::Mode::kX86) ? &_x86ValidationData : &_x64ValidationData;
  InstId instId = inst.id();
  InstOptions options = inst.options();

  if (ASMJIT_UNLIKELY(!Inst::isDefinedId(instId)))
    return DebugUtils::errored(kErrorInvalidInstruction);

  const InstDB::InstInfo& instInfo = InstDB::infoById(instId);
  const InstDB::CommonInfo& commonInfo = instInfo.commonInfo();

  InstDB::InstFlags iFlags = instInfo.flags();

  constexpr InstOptions kRepAny = InstOptions::kX86_Rep | InstOptions::kX86_Repne;
  constexpr InstOptions kXAcqXRel = InstOptions::kX86_XAcquire | InstOptions::kX86_XRelease;
  constexpr InstOptions kAvx512Options = InstOptions::kX86_ZMask | InstOptions::kX86_ER | InstOptions::kX86_SAE;

  // Validate LOCK|XACQUIRE|XRELEASE Prefixes
  // ----------------------------------------

  if (Support::test(options, InstOptions::kX86_Lock | kXAcqXRel)) {
    if (Support::test(options, InstOptions::kX86_Lock)) {
      if (ASMJIT_UNLIKELY(!Support::test(iFlags, InstDB::InstFlags::kLock) && !Support::test(options, kXAcqXRel)))
        return DebugUtils::errored(kErrorInvalidLockPrefix);

      if (ASMJIT_UNLIKELY(opCount < 1 || !operands[0].isMem()))
        return DebugUtils::errored(kErrorInvalidLockPrefix);
    }

    if (Support::test(options, kXAcqXRel)) {
      if (ASMJIT_UNLIKELY(!Support::test(options, InstOptions::kX86_Lock) || (options & kXAcqXRel) == kXAcqXRel))
        return DebugUtils::errored(kErrorInvalidPrefixCombination);

      if (ASMJIT_UNLIKELY(Support::test(options, InstOptions::kX86_XAcquire) && !Support::test(iFlags, InstDB::InstFlags::kXAcquire)))
        return DebugUtils::errored(kErrorInvalidXAcquirePrefix);

      if (ASMJIT_UNLIKELY(Support::test(options, InstOptions::kX86_XRelease) && !Support::test(iFlags, InstDB::InstFlags::kXRelease)))
        return DebugUtils::errored(kErrorInvalidXReleasePrefix);
    }
  }

  // Validate REP and REPNE Prefixes
  // -------------------------------

  if (Support::test(options, kRepAny)) {
    if (ASMJIT_UNLIKELY((options & kRepAny) == kRepAny))
      return DebugUtils::errored(kErrorInvalidPrefixCombination);

    if (ASMJIT_UNLIKELY(!Support::test(iFlags, InstDB::InstFlags::kRep)))
      return DebugUtils::errored(kErrorInvalidRepPrefix);
  }

  // Translate Each Operand to the Corresponding OpSignature
  // -------------------------------------------------------

  InstDB::OpSignature oSigTranslated[Globals::kMaxOpCount];
  InstDB::OpFlags combinedOpFlags = InstDB::OpFlags::kNone;
  uint32_t combinedRegMask = 0;
  const Mem* memOp = nullptr;

  for (i = 0; i < opCount; i++) {
    const Operand_& op = operands[i];
    if (op.opType() == OperandType::kNone)
      break;

    InstDB::OpFlags opFlags = InstDB::OpFlags::kNone;
    RegMask regMask = 0;

    switch (op.opType()) {
      case OperandType::kReg: {
        RegType regType = op.as<BaseReg>().type();
        opFlags = _x86OpFlagFromRegType[size_t(regType)];

        if (ASMJIT_UNLIKELY(opFlags == InstDB::OpFlags::kNone))
          return DebugUtils::errored(kErrorInvalidRegType);

        // If `regId` is equal or greater than Operand::kVirtIdMin it means that the register is virtual and its
        // index will be assigned later by the register allocator. We must pass unless asked to disallow virtual
        // registers.
        uint32_t regId = op.id();
        if (regId < Operand::kVirtIdMin) {
          if (ASMJIT_UNLIKELY(regId >= 32))
            return DebugUtils::errored(kErrorInvalidPhysId);

          if (ASMJIT_UNLIKELY(Support::bitTest(vd->allowedRegMask[size_t(regType)], regId) == 0))
            return DebugUtils::errored(kErrorInvalidPhysId);

          regMask = Support::bitMask(regId);
          combinedRegMask |= regMask;
        }
        else {
          if (uint32_t(validationFlags & ValidationFlags::kEnableVirtRegs) == 0)
            return DebugUtils::errored(kErrorIllegalVirtReg);
          regMask = 0xFFFFFFFFu;
        }
        break;
      }

      // TODO: Validate base and index and combine these with `combinedRegMask`.
      case OperandType::kMem: {
        const Mem& m = op.as<Mem>();
        memOp = &m;

        uint32_t memSize = m.size();
        RegType baseType = m.baseType();
        RegType indexType = m.indexType();

        if (m.segmentId() > 6)
          return DebugUtils::errored(kErrorInvalidSegment);

        // Validate AVX-512 broadcast {1tox}.
        if (m.hasBroadcast()) {
          if (memSize != 0) {
            // If the size is specified it has to match the broadcast size.
            if (ASMJIT_UNLIKELY(commonInfo.hasAvx512B32() && memSize != 4))
              return DebugUtils::errored(kErrorInvalidBroadcast);

            if (ASMJIT_UNLIKELY(commonInfo.hasAvx512B64() && memSize != 8))
              return DebugUtils::errored(kErrorInvalidBroadcast);
          }
          else {
            // If there is no size we implicitly calculate it so we can validate N in {1toN} properly.
            memSize = commonInfo.hasAvx512B64() ? 8 :
                      commonInfo.hasAvx512B32() ? 4 : 2;
          }

          memSize <<= uint32_t(m.getBroadcast());
        }

        if (baseType != RegType::kNone && baseType > RegType::kLabelTag) {
          uint32_t baseId = m.baseId();

          if (m.isRegHome()) {
            // Home address of a virtual register. In such case we don't want to validate the type of the
            // base register as it will always be patched to ESP|RSP.
          }
          else {
            if (ASMJIT_UNLIKELY(!Support::bitTest(vd->allowedMemBaseRegs, baseType)))
              return DebugUtils::errored(kErrorInvalidAddress);
          }

          // Create information that will be validated only if this is an implicit memory operand. Basically
          // only usable for string instructions and other instructions where memory operand is implicit and
          // has 'seg:[reg]' form.
          if (baseId < Operand::kVirtIdMin) {
            if (ASMJIT_UNLIKELY(baseId >= 32))
              return DebugUtils::errored(kErrorInvalidPhysId);

            // Physical base id.
            regMask = Support::bitMask(baseId);
            combinedRegMask |= regMask;
          }
          else {
            // Virtual base id - fill the whole mask for implicit mem validation. The register is not assigned
            // yet, so we cannot predict the phys id.
            if (uint32_t(validationFlags & ValidationFlags::kEnableVirtRegs) == 0)
              return DebugUtils::errored(kErrorIllegalVirtReg);
            regMask = 0xFFFFFFFFu;
          }

          if (indexType == RegType::kNone && !m.offsetLo32())
            opFlags |= InstDB::OpFlags::kFlagMemBase;
        }
        else if (baseType == RegType::kLabelTag) {
          // [Label] - there is no need to validate the base as it's label.
        }
        else {
          // Base is a 64-bit address.
          int64_t offset = m.offset();
          if (!Support::isInt32(offset)) {
            if (mode == InstDB::Mode::kX86) {
              // 32-bit mode: Make sure that the address is either `int32_t` or `uint32_t`.
              if (!Support::isUInt32(offset))
                return DebugUtils::errored(kErrorInvalidAddress64Bit);
            }
            else {
              // 64-bit mode: Zero extension is allowed if the address has 32-bit index register or the address
              // has no index register (it's still encodable).
              if (indexType != RegType::kNone) {
                if (!Support::isUInt32(offset))
                  return DebugUtils::errored(kErrorInvalidAddress64Bit);

                if (indexType != RegType::kX86_Gpd)
                  return DebugUtils::errored(kErrorInvalidAddress64BitZeroExtension);
              }
              else {
                // We don't validate absolute 64-bit addresses without an index register as this also depends
                // on the target's base address. We don't have the information to do it at this moment.
              }
            }
          }
        }

        if (indexType != RegType::kNone) {
          if (ASMJIT_UNLIKELY(!Support::bitTest(vd->allowedMemIndexRegs, indexType)))
            return DebugUtils::errored(kErrorInvalidAddress);

          if (indexType == RegType::kX86_Xmm) {
            opFlags |= InstDB::OpFlags::kVm32x | InstDB::OpFlags::kVm64x;
          }
          else if (indexType == RegType::kX86_Ymm) {
            opFlags |= InstDB::OpFlags::kVm32y | InstDB::OpFlags::kVm64y;
          }
          else if (indexType == RegType::kX86_Zmm) {
            opFlags |= InstDB::OpFlags::kVm32z | InstDB::OpFlags::kVm64z;
          }
          else {
            if (baseType != RegType::kNone)
              opFlags |= InstDB::OpFlags::kFlagMib;
          }

          // [RIP + {XMM|YMM|ZMM}] is not allowed.
          if (baseType == RegType::kX86_Rip && Support::test(opFlags, InstDB::OpFlags::kVmMask))
            return DebugUtils::errored(kErrorInvalidAddress);

          uint32_t indexId = m.indexId();
          if (indexId < Operand::kVirtIdMin) {
            if (ASMJIT_UNLIKELY(indexId >= 32))
              return DebugUtils::errored(kErrorInvalidPhysId);

            combinedRegMask |= Support::bitMask(indexId);
          }
          else {
            if (uint32_t(validationFlags & ValidationFlags::kEnableVirtRegs) == 0)
              return DebugUtils::errored(kErrorIllegalVirtReg);
          }

          // Only used for implicit memory operands having 'seg:[reg]' form, so clear it.
          regMask = 0;
        }

        switch (memSize) {
          case  0: opFlags |= InstDB::OpFlags::kMemUnspecified; break;
          case  1: opFlags |= InstDB::OpFlags::kMem8; break;
          case  2: opFlags |= InstDB::OpFlags::kMem16; break;
          case  4: opFlags |= InstDB::OpFlags::kMem32; break;
          case  6: opFlags |= InstDB::OpFlags::kMem48; break;
          case  8: opFlags |= InstDB::OpFlags::kMem64; break;
          case 10: opFlags |= InstDB::OpFlags::kMem80; break;
          case 16: opFlags |= InstDB::OpFlags::kMem128; break;
          case 32: opFlags |= InstDB::OpFlags::kMem256; break;
          case 64: opFlags |= InstDB::OpFlags::kMem512; break;

          default:
            return DebugUtils::errored(kErrorInvalidOperandSize);
        }

        break;
      }

      case OperandType::kImm: {
        uint64_t immValue = op.as<Imm>().valueAs<uint64_t>();

        if (int64_t(immValue) >= 0) {
          if (immValue <= 0x7u)
            opFlags = InstDB::OpFlags::kImmI64 | InstDB::OpFlags::kImmU64 | InstDB::OpFlags::kImmI32 | InstDB::OpFlags::kImmU32 |
                      InstDB::OpFlags::kImmI16 | InstDB::OpFlags::kImmU16 | InstDB::OpFlags::kImmI8  | InstDB::OpFlags::kImmU8  |
                      InstDB::OpFlags::kImmI4  | InstDB::OpFlags::kImmU4  ;
          else if (immValue <= 0xFu)
            opFlags = InstDB::OpFlags::kImmI64 | InstDB::OpFlags::kImmU64 | InstDB::OpFlags::kImmI32 | InstDB::OpFlags::kImmU32 |
                      InstDB::OpFlags::kImmI16 | InstDB::OpFlags::kImmU16 | InstDB::OpFlags::kImmI8  | InstDB::OpFlags::kImmU8  |
                      InstDB::OpFlags::kImmU4  ;
          else if (immValue <= 0x7Fu)
            opFlags = InstDB::OpFlags::kImmI64 | InstDB::OpFlags::kImmU64 | InstDB::OpFlags::kImmI32 | InstDB::OpFlags::kImmU32 |
                      InstDB::OpFlags::kImmI16 | InstDB::OpFlags::kImmU16 | InstDB::OpFlags::kImmI8  | InstDB::OpFlags::kImmU8  ;
          else if (immValue <= 0xFFu)
            opFlags = InstDB::OpFlags::kImmI64 | InstDB::OpFlags::kImmU64 | InstDB::OpFlags::kImmI32 | InstDB::OpFlags::kImmU32 |
                      InstDB::OpFlags::kImmI16 | InstDB::OpFlags::kImmU16 | InstDB::OpFlags::kImmU8  ;
          else if (immValue <= 0x7FFFu)
            opFlags = InstDB::OpFlags::kImmI64 | InstDB::OpFlags::kImmU64 | InstDB::OpFlags::kImmI32 | InstDB::OpFlags::kImmU32 |
                      InstDB::OpFlags::kImmI16 | InstDB::OpFlags::kImmU16 ;
          else if (immValue <= 0xFFFFu)
            opFlags = InstDB::OpFlags::kImmI64 | InstDB::OpFlags::kImmU64 | InstDB::OpFlags::kImmI32 | InstDB::OpFlags::kImmU32 |
                      InstDB::OpFlags::kImmU16 ;
          else if (immValue <= 0x7FFFFFFFu)
            opFlags = InstDB::OpFlags::kImmI64 | InstDB::OpFlags::kImmU64 | InstDB::OpFlags::kImmI32 | InstDB::OpFlags::kImmU32;
          else if (immValue <= 0xFFFFFFFFu)
            opFlags = InstDB::OpFlags::kImmI64 | InstDB::OpFlags::kImmU64 | InstDB::OpFlags::kImmU32;
          else if (immValue <= 0x7FFFFFFFFFFFFFFFu)
            opFlags = InstDB::OpFlags::kImmI64 | InstDB::OpFlags::kImmU64;
          else
            opFlags = InstDB::OpFlags::kImmU64;
        }
        else {
          immValue = Support::neg(immValue);
          if (immValue <= 0x8u)
            opFlags = InstDB::OpFlags::kImmI64 | InstDB::OpFlags::kImmI32 | InstDB::OpFlags::kImmI16 | InstDB::OpFlags::kImmI8 | InstDB::OpFlags::kImmI4;
          else if (immValue <= 0x80u)
            opFlags = InstDB::OpFlags::kImmI64 | InstDB::OpFlags::kImmI32 | InstDB::OpFlags::kImmI16 | InstDB::OpFlags::kImmI8;
          else if (immValue <= 0x8000u)
            opFlags = InstDB::OpFlags::kImmI64 | InstDB::OpFlags::kImmI32 | InstDB::OpFlags::kImmI16;
          else if (immValue <= 0x80000000u)
            opFlags = InstDB::OpFlags::kImmI64 | InstDB::OpFlags::kImmI32;
          else
            opFlags = InstDB::OpFlags::kImmI64;
        }
        break;
      }

      case OperandType::kLabel: {
        opFlags |= InstDB::OpFlags::kRel8 | InstDB::OpFlags::kRel32;
        break;
      }

      default:
        return DebugUtils::errored(kErrorInvalidState);
    }

    InstDB::OpSignature& oSigDst = oSigTranslated[i];
    oSigDst._flags = uint64_t(opFlags) & 0x00FFFFFFFFFFFFFFu;
    oSigDst._regMask = uint8_t(regMask & 0xFFu);
    combinedOpFlags |= opFlags;
  }

  // Decrease the number of operands of those that are none. This is important as Assembler and Compiler may just pass
  // more operands padded with none (which means that no operand is given at that index). However, validate that there
  // are no gaps (like [reg, none, reg] or [none, reg]).
  if (i < opCount) {
    while (--opCount > i)
      if (ASMJIT_UNLIKELY(!operands[opCount].isNone()))
        return DebugUtils::errored(kErrorInvalidInstruction);
  }

  // Validate X86 and X64 specific cases.
  if (mode == InstDB::Mode::kX86) {
    // Illegal use of 64-bit register in 32-bit mode.
    if (ASMJIT_UNLIKELY(Support::test(combinedOpFlags, InstDB::OpFlags::kRegGpq)))
      return DebugUtils::errored(kErrorInvalidUseOfGpq);
  }
  else {
    // Illegal use of a high 8-bit register with REX prefix.
    bool hasREX = inst.hasOption(InstOptions::kX86_Rex) || (combinedRegMask & 0xFFFFFF00u) != 0;
    if (ASMJIT_UNLIKELY(hasREX && Support::test(combinedOpFlags, InstDB::OpFlags::kRegGpbHi)))
      return DebugUtils::errored(kErrorInvalidUseOfGpbHi);
  }

  // Validate Instruction Signature by Comparing Against All `iSig` Rows
  // -------------------------------------------------------------------

  const InstDB::InstSignature* iSig = InstDB::_instSignatureTable + commonInfo._iSignatureIndex;
  const InstDB::InstSignature* iEnd = iSig + commonInfo._iSignatureCount;

  if (iSig != iEnd) {
    const InstDB::OpSignature* opSignatureTable = InstDB::_opSignatureTable;

    // If set it means that we matched a signature where only immediate value
    // was out of bounds. We can return a more descriptive error if we know this.
    bool globalImmOutOfRange = false;

    do {
      // Check if the architecture is compatible.
      if (!iSig->supportsMode(mode))
        continue;

      // Compare the operands table with reference operands.
      uint32_t j = 0;
      uint32_t iSigCount = iSig->opCount();
      bool localImmOutOfRange = false;

      if (iSigCount == opCount) {
        for (j = 0; j < opCount; j++)
          if (!x86CheckOSig(oSigTranslated[j], iSig->opSignature(j), localImmOutOfRange))
            break;
      }
      else if (iSigCount - iSig->implicitOpCount() == opCount) {
        uint32_t r = 0;
        for (j = 0; j < opCount && r < iSigCount; j++, r++) {
          const InstDB::OpSignature* oChk = oSigTranslated + j;
          const InstDB::OpSignature* oRef;
Next:
          oRef = opSignatureTable + iSig->opSignatureIndex(r);
          // Skip implicit operands.
          if (oRef->isImplicit()) {
            if (++r >= iSigCount)
              break;
            else
              goto Next;
          }

          if (!x86CheckOSig(*oChk, *oRef, localImmOutOfRange))
            break;
        }
      }

      if (j == opCount) {
        if (!localImmOutOfRange) {
          // Match, must clear possible `globalImmOutOfRange`.
          globalImmOutOfRange = false;
          break;
        }
        globalImmOutOfRange = localImmOutOfRange;
      }
    } while (++iSig != iEnd);

    if (iSig == iEnd) {
      if (globalImmOutOfRange)
        return DebugUtils::errored(kErrorInvalidImmediate);
      else
        return DebugUtils::errored(kErrorInvalidInstruction);
    }
  }

  // Validate AVX512 Options
  // -----------------------

  const RegOnly& extraReg = inst.extraReg();

  if (Support::test(options, kAvx512Options)) {
    if (commonInfo.hasFlag(InstDB::InstFlags::kEvex)) {
      // Validate AVX-512 {z}.
      if (Support::test(options, InstOptions::kX86_ZMask)) {
        if (ASMJIT_UNLIKELY(Support::test(options, InstOptions::kX86_ZMask) && !commonInfo.hasAvx512Z()))
          return DebugUtils::errored(kErrorInvalidKZeroUse);
      }

      // Validate AVX-512 {sae} and {er}.
      if (Support::test(options, InstOptions::kX86_SAE | InstOptions::kX86_ER)) {
        // Rounding control is impossible if the instruction is not reg-to-reg.
        if (ASMJIT_UNLIKELY(memOp))
          return DebugUtils::errored(kErrorInvalidEROrSAE);

        // Check if {sae} or {er} is supported by the instruction.
        if (Support::test(options, InstOptions::kX86_ER)) {
          // NOTE: if both {sae} and {er} are set, we don't care, as {sae} is implied.
          if (ASMJIT_UNLIKELY(!commonInfo.hasAvx512ER()))
            return DebugUtils::errored(kErrorInvalidEROrSAE);
        }
        else {
          if (ASMJIT_UNLIKELY(!commonInfo.hasAvx512SAE()))
            return DebugUtils::errored(kErrorInvalidEROrSAE);
        }

        // {sae} and {er} are defined for either scalar ops or vector ops that require LL to be 10 (512-bit vector
        // operations). We don't need any more bits in the instruction database to be able to validate this, as
        // each AVX512 instruction that has broadcast is vector instruction (in this case we require zmm registers),
        // otherwise it's a scalar instruction, which is valid.
        if (commonInfo.hasAvx512B()) {
          // Supports broadcast, thus we require LL to be '10', which means there have to be ZMM registers used. We
          // don't calculate LL here, but we know that it would be '10' if there is at least one ZMM register used.

          // There is no {er}/{sae}-enabled instruction with less than two operands.
          ASMJIT_ASSERT(opCount >= 2);
          if (ASMJIT_UNLIKELY(!x86IsZmmOrM512(operands[0]) && !x86IsZmmOrM512(operands[1])))
            return DebugUtils::errored(kErrorInvalidEROrSAE);
        }
      }
    }
    else {
      // Not an AVX512 instruction - maybe OpExtra is xCX register used by REP/REPNE prefix.
      if (Support::test(options, kAvx512Options) || !Support::test(options, kRepAny))
        return DebugUtils::errored(kErrorInvalidInstruction);
    }
  }

  // Validate {Extra} Register
  // -------------------------

  if (extraReg.isReg()) {
    if (Support::test(options, kRepAny)) {
      // Validate REP|REPNE {cx|ecx|rcx}.
      if (ASMJIT_UNLIKELY(Support::test(iFlags, InstDB::InstFlags::kRepIgnored)))
        return DebugUtils::errored(kErrorInvalidExtraReg);

      if (extraReg.isPhysReg()) {
        if (ASMJIT_UNLIKELY(extraReg.id() != Gp::kIdCx))
          return DebugUtils::errored(kErrorInvalidExtraReg);
      }

      // The type of the {...} register must match the type of the base register
      // of memory operand. So if the memory operand uses 32-bit register the
      // count register must also be 32-bit, etc...
      if (ASMJIT_UNLIKELY(!memOp || extraReg.type() != memOp->baseType()))
        return DebugUtils::errored(kErrorInvalidExtraReg);
    }
    else if (commonInfo.hasFlag(InstDB::InstFlags::kEvex)) {
      // Validate AVX-512 {k}.
      if (ASMJIT_UNLIKELY(extraReg.type() != RegType::kX86_KReg))
        return DebugUtils::errored(kErrorInvalidExtraReg);

      if (ASMJIT_UNLIKELY(extraReg.id() == 0 || !commonInfo.hasAvx512K()))
        return DebugUtils::errored(kErrorInvalidKMaskUse);
    }
    else {
      return DebugUtils::errored(kErrorInvalidExtraReg);
    }
  }

  return kErrorOk;
}

Error validateX86(const BaseInst& inst, const Operand_* operands, size_t opCount, ValidationFlags validationFlags) noexcept {
  return validate(InstDB::Mode::kX86, inst, operands, opCount, validationFlags);
}

Error validateX64(const BaseInst& inst, const Operand_* operands, size_t opCount, ValidationFlags validationFlags) noexcept {
  return validate(InstDB::Mode::kX64, inst, operands, opCount, validationFlags);
}

#endif // !ASMJIT_NO_VALIDATION

// x86::InstInternal - QueryRWInfo
// ===============================

#ifndef ASMJIT_NO_INTROSPECTION
static const Support::Array<uint64_t, uint32_t(RegGroup::kMaxValue) + 1> rwRegGroupByteMask = {{
  0x00000000000000FFu, // GP.
  0xFFFFFFFFFFFFFFFFu, // XMM|YMM|ZMM.
  0x00000000000000FFu, // MM.
  0x00000000000000FFu, // KReg.
  0x0000000000000003u, // SReg.
  0x00000000000000FFu, // CReg.
  0x00000000000000FFu, // DReg.
  0x00000000000003FFu, // St().
  0x000000000000FFFFu, // BND.
  0x00000000000000FFu  // RIP.
}};

static ASMJIT_FORCE_INLINE void rwZeroExtendGp(OpRWInfo& opRwInfo, const Gp& reg, uint32_t nativeGpSize) noexcept {
  ASMJIT_ASSERT(BaseReg::isGp(reg.as<Operand>()));
  if (reg.size() + 4 == nativeGpSize) {
    opRwInfo.addOpFlags(OpRWFlags::kZExt);
    opRwInfo.setExtendByteMask(~opRwInfo.writeByteMask() & 0xFFu);
  }
}

static ASMJIT_FORCE_INLINE void rwZeroExtendAvxVec(OpRWInfo& opRwInfo, const Vec& reg) noexcept {
  DebugUtils::unused(reg);

  uint64_t msk = ~Support::fillTrailingBits(opRwInfo.writeByteMask());
  if (msk) {
    opRwInfo.addOpFlags(OpRWFlags::kZExt);
    opRwInfo.setExtendByteMask(msk);
  }
}

static ASMJIT_FORCE_INLINE void rwZeroExtendNonVec(OpRWInfo& opRwInfo, const Reg& reg) noexcept {
  uint64_t msk = ~Support::fillTrailingBits(opRwInfo.writeByteMask()) & rwRegGroupByteMask[reg.group()];
  if (msk) {
    opRwInfo.addOpFlags(OpRWFlags::kZExt);
    opRwInfo.setExtendByteMask(msk);
  }
}

static ASMJIT_FORCE_INLINE Error rwHandleAVX512(const BaseInst& inst, const InstDB::CommonInfo& commonInfo, InstRWInfo* out) noexcept {
  if (inst.hasExtraReg() && inst.extraReg().type() == RegType::kX86_KReg && out->opCount() > 0) {
    // AVX-512 instruction that uses a destination with {k} register (zeroing vs masking).
    out->_extraReg.addOpFlags(OpRWFlags::kRead);
    out->_extraReg.setReadByteMask(0xFF);
    if (!inst.hasOption(InstOptions::kX86_ZMask) && !commonInfo.hasAvx512Flag(InstDB::Avx512Flags::kImplicitZ)) {
      out->_operands[0].addOpFlags(OpRWFlags::kRead);
      out->_operands[0]._readByteMask |= out->_operands[0]._writeByteMask;
    }
  }

  return kErrorOk;
}

static ASMJIT_FORCE_INLINE bool hasSameRegType(const BaseReg* regs, size_t opCount) noexcept {
  ASMJIT_ASSERT(opCount > 0);
  RegType regType = regs[0].type();
  for (size_t i = 1; i < opCount; i++)
    if (regs[i].type() != regType)
      return false;
  return true;
}

Error queryRWInfo(Arch arch, const BaseInst& inst, const Operand_* operands, size_t opCount, InstRWInfo* out) noexcept {
  // Only called when `arch` matches X86 family.
  ASMJIT_ASSERT(Environment::isFamilyX86(arch));

  // Get the instruction data.
  InstId instId = inst.id();
  if (ASMJIT_UNLIKELY(!Inst::isDefinedId(instId)))
    return DebugUtils::errored(kErrorInvalidInstruction);

  // Read/Write flags.
  const InstDB::InstInfo& instInfo = InstDB::_instInfoTable[instId];
  const InstDB::CommonInfo& commonInfo = InstDB::_commonInfoTable[instInfo._commonInfoIndex];
  const InstDB::AdditionalInfo& additionalInfo = InstDB::_additionalInfoTable[instInfo._additionalInfoIndex];
  const InstDB::RWFlagsInfoTable& rwFlags = InstDB::_rwFlagsInfoTable[additionalInfo._rwFlagsIndex];

  // There are two data tables, one for `opCount == 2` and the second for
  // `opCount != 2`. There are two reasons for that:
  //   - There are instructions that share the same name that have both 2 or 3 operands, which have different
  //     RW information / semantics.
  //   - There must be 2 tables otherwise the lookup index won't fit into 8 bits (there is more than 256 records
  //     of combined rwInfo A and B).
  const InstDB::RWInfo& instRwInfo = opCount == 2 ? InstDB::rwInfoA[InstDB::rwInfoIndexA[instId]]
                                                  : InstDB::rwInfoB[InstDB::rwInfoIndexB[instId]];
  const InstDB::RWInfoRm& instRmInfo = InstDB::rwInfoRm[instRwInfo.rmInfo];

  out->_instFlags = InstDB::_instFlagsTable[additionalInfo._instFlagsIndex];
  out->_opCount = uint8_t(opCount);
  out->_rmFeature = instRmInfo.rmFeature;
  out->_extraReg.reset();
  out->_readFlags = CpuRWFlags(rwFlags.readFlags);
  out->_writeFlags = CpuRWFlags(rwFlags.writeFlags);

  uint32_t opTypeMask = 0u;
  uint32_t nativeGpSize = Environment::registerSizeFromArch(arch);

  constexpr OpRWFlags R = OpRWFlags::kRead;
  constexpr OpRWFlags W = OpRWFlags::kWrite;
  constexpr OpRWFlags X = OpRWFlags::kRW;
  constexpr OpRWFlags RegM = OpRWFlags::kRegMem;
  constexpr OpRWFlags RegPhys = OpRWFlags::kRegPhysId;
  constexpr OpRWFlags MibRead = OpRWFlags::kMemBaseRead | OpRWFlags::kMemIndexRead;

  if (instRwInfo.category <= uint32_t(InstDB::RWInfo::kCategoryGenericEx)) {
    uint32_t i;
    uint32_t rmOpsMask = 0;
    uint32_t rmMaxSize = 0;

    for (i = 0; i < opCount; i++) {
      OpRWInfo& op = out->_operands[i];
      const Operand_& srcOp = operands[i];
      const InstDB::RWInfoOp& rwOpData = InstDB::rwInfoOp[instRwInfo.opInfoIndex[i]];

      opTypeMask |= Support::bitMask(srcOp.opType());

      if (!srcOp.isRegOrMem()) {
        op.reset();
        continue;
      }

      op._opFlags = rwOpData.flags & ~OpRWFlags::kZExt;
      op._physId = rwOpData.physId;
      op._rmSize = 0;
      op._resetReserved();

      uint64_t rByteMask = rwOpData.rByteMask;
      uint64_t wByteMask = rwOpData.wByteMask;

      if (op.isRead()  && !rByteMask) rByteMask = Support::lsbMask<uint64_t>(srcOp.x86RmSize());
      if (op.isWrite() && !wByteMask) wByteMask = Support::lsbMask<uint64_t>(srcOp.x86RmSize());

      op._readByteMask = rByteMask;
      op._writeByteMask = wByteMask;
      op._extendByteMask = 0;
      op._consecutiveLeadCount = rwOpData.consecutiveLeadCount;

      if (srcOp.isReg()) {
        // Zero extension.
        if (op.isWrite()) {
          if (srcOp.as<Reg>().isGp()) {
            // GP registers on X64 are special:
            //   - 8-bit and 16-bit writes aren't zero extended.
            //   - 32-bit writes ARE zero extended.
            rwZeroExtendGp(op, srcOp.as<Gp>(), nativeGpSize);
          }
          else if (Support::test(rwOpData.flags, OpRWFlags::kZExt)) {
            // Otherwise follow ZExt.
            rwZeroExtendNonVec(op, srcOp.as<Gp>());
          }
        }

        // Aggregate values required to calculate valid Reg/M info.
        rmMaxSize  = Support::max(rmMaxSize, srcOp.x86RmSize());
        rmOpsMask |= Support::bitMask<uint32_t>(i);
      }
      else {
        const x86::Mem& memOp = srcOp.as<x86::Mem>();
        // The RW flags of BASE+INDEX are either provided by the data, which means
        // that the instruction is border-case, or they are deduced from the operand.
        if (memOp.hasBaseReg() && !op.hasOpFlag(OpRWFlags::kMemBaseRW))
          op.addOpFlags(OpRWFlags::kMemBaseRead);
        if (memOp.hasIndexReg() && !op.hasOpFlag(OpRWFlags::kMemIndexRW))
          op.addOpFlags(OpRWFlags::kMemIndexRead);
      }
    }

    // Only keep kMovOp if the instruction is actually register to register move of the same kind.
    if (out->hasInstFlag(InstRWFlags::kMovOp)) {
      if (!(opCount >= 2 && opTypeMask == Support::bitMask(OperandType::kReg) && hasSameRegType(reinterpret_cast<const BaseReg*>(operands), opCount)))
        out->_instFlags &= ~InstRWFlags::kMovOp;
    }

    // Special cases require more logic.
    if (instRmInfo.flags & (InstDB::RWInfoRm::kFlagMovssMovsd | InstDB::RWInfoRm::kFlagPextrw | InstDB::RWInfoRm::kFlagFeatureIfRMI)) {
      if (instRmInfo.flags & InstDB::RWInfoRm::kFlagMovssMovsd) {
        if (opCount == 2) {
          if (operands[0].isReg() && operands[1].isReg()) {
            // Doesn't zero extend the destination.
            out->_operands[0]._extendByteMask = 0;
          }
        }
      }
      else if (instRmInfo.flags & InstDB::RWInfoRm::kFlagPextrw) {
        if (opCount == 3 && Reg::isMm(operands[1])) {
          out->_rmFeature = 0;
          rmOpsMask = 0;
        }
      }
      else if (instRmInfo.flags & InstDB::RWInfoRm::kFlagFeatureIfRMI) {
        if (opCount != 3 || !operands[2].isImm()) {
          out->_rmFeature = 0;
        }
      }
    }

    rmOpsMask &= uint32_t(instRmInfo.rmOpsMask);
    if (rmOpsMask && !inst.hasOption(InstOptions::kX86_ER)) {
      Support::BitWordIterator<uint32_t> it(rmOpsMask);
      do {
        i = it.next();

        OpRWInfo& op = out->_operands[i];
        op.addOpFlags(RegM);

        switch (instRmInfo.category) {
          case InstDB::RWInfoRm::kCategoryFixed:
            op.setRmSize(instRmInfo.fixedSize);
            break;
          case InstDB::RWInfoRm::kCategoryConsistent:
            op.setRmSize(operands[i].x86RmSize());
            break;
          case InstDB::RWInfoRm::kCategoryHalf:
            op.setRmSize(rmMaxSize / 2u);
            break;
          case InstDB::RWInfoRm::kCategoryQuarter:
            op.setRmSize(rmMaxSize / 4u);
            break;
          case InstDB::RWInfoRm::kCategoryEighth:
            op.setRmSize(rmMaxSize / 8u);
            break;
        }
      } while (it.hasNext());
    }

    // Special cases per instruction.
    if (instRwInfo.category == InstDB::RWInfo::kCategoryGenericEx) {
      switch (inst.id()) {
        case Inst::kIdVpternlogd:
        case Inst::kIdVpternlogq: {
          if (opCount == 4 && operands[3].isImm()) {
            uint32_t predicate = operands[3].as<Imm>().valueAs<uint8_t>();

            if ((predicate >> 4) == (predicate & 0xF)) {
              out->_operands[0].clearOpFlags(OpRWFlags::kRead);
              out->_operands[0].setReadByteMask(0);
            }
          }
          break;
        }

        default:
          break;
      }
    }

    return rwHandleAVX512(inst, commonInfo, out);
  }

  switch (instRwInfo.category) {
    case InstDB::RWInfo::kCategoryMov: {
      // Special case for 'mov' instruction. Here there are some variants that we have to handle as 'mov' can be
      // used to move between GP, segment, control and debug registers. Moving between GP registers also allow to
      // use memory operand.

      // We will again set the flag if it's actually a move from GP to GP register, otherwise this flag cannot be set.
      out->_instFlags &= ~InstRWFlags::kMovOp;

      if (opCount == 2) {
        if (operands[0].isReg() && operands[1].isReg()) {
          const Reg& o0 = operands[0].as<Reg>();
          const Reg& o1 = operands[1].as<Reg>();

          if (o0.isGp() && o1.isGp()) {
            out->_operands[0].reset(W | RegM, operands[0].x86RmSize());
            out->_operands[1].reset(R | RegM, operands[1].x86RmSize());

            rwZeroExtendGp(out->_operands[0], operands[0].as<Gp>(), nativeGpSize);
            out->_instFlags |= InstRWFlags::kMovOp;
            return kErrorOk;
          }

          if (o0.isGp() && o1.isSReg()) {
            out->_operands[0].reset(W | RegM, nativeGpSize);
            out->_operands[0].setRmSize(2);
            out->_operands[1].reset(R, 2);
            return kErrorOk;
          }

          if (o0.isSReg() && o1.isGp()) {
            out->_operands[0].reset(W, 2);
            out->_operands[1].reset(R | RegM, 2);
            out->_operands[1].setRmSize(2);
            return kErrorOk;
          }

          if (o0.isGp() && (o1.isCReg() || o1.isDReg())) {
            out->_operands[0].reset(W, nativeGpSize);
            out->_operands[1].reset(R, nativeGpSize);
            out->_writeFlags = CpuRWFlags::kX86_OF |
                               CpuRWFlags::kX86_SF |
                               CpuRWFlags::kX86_ZF |
                               CpuRWFlags::kX86_AF |
                               CpuRWFlags::kX86_PF |
                               CpuRWFlags::kX86_CF;
            return kErrorOk;
          }

          if ((o0.isCReg() || o0.isDReg()) && o1.isGp()) {
            out->_operands[0].reset(W, nativeGpSize);
            out->_operands[1].reset(R, nativeGpSize);
            out->_writeFlags = CpuRWFlags::kX86_OF |
                               CpuRWFlags::kX86_SF |
                               CpuRWFlags::kX86_ZF |
                               CpuRWFlags::kX86_AF |
                               CpuRWFlags::kX86_PF |
                               CpuRWFlags::kX86_CF;
            return kErrorOk;
          }
        }

        if (operands[0].isReg() && operands[1].isMem()) {
          const Reg& o0 = operands[0].as<Reg>();
          const Mem& o1 = operands[1].as<Mem>();

          if (o0.isGp()) {
            if (!o1.isOffset64Bit())
              out->_operands[0].reset(W, o0.size());
            else
              out->_operands[0].reset(W | RegPhys, o0.size(), Gp::kIdAx);

            out->_operands[1].reset(R | MibRead, o0.size());
            rwZeroExtendGp(out->_operands[0], operands[0].as<Gp>(), nativeGpSize);
            return kErrorOk;
          }

          if (o0.isSReg()) {
            out->_operands[0].reset(W, 2);
            out->_operands[1].reset(R, 2);
            return kErrorOk;
          }
        }

        if (operands[0].isMem() && operands[1].isReg()) {
          const Mem& o0 = operands[0].as<Mem>();
          const Reg& o1 = operands[1].as<Reg>();

          if (o1.isGp()) {
            out->_operands[0].reset(W | MibRead, o1.size());
            if (!o0.isOffset64Bit())
              out->_operands[1].reset(R, o1.size());
            else
              out->_operands[1].reset(R | RegPhys, o1.size(), Gp::kIdAx);
            return kErrorOk;
          }

          if (o1.isSReg()) {
            out->_operands[0].reset(W | MibRead, 2);
            out->_operands[1].reset(R, 2);
            return kErrorOk;
          }
        }

        if (Reg::isGp(operands[0]) && operands[1].isImm()) {
          const Reg& o0 = operands[0].as<Reg>();
          out->_operands[0].reset(W | RegM, o0.size());
          out->_operands[1].reset();

          rwZeroExtendGp(out->_operands[0], operands[0].as<Gp>(), nativeGpSize);
          return kErrorOk;
        }

        if (operands[0].isMem() && operands[1].isImm()) {
          const Reg& o0 = operands[0].as<Reg>();
          out->_operands[0].reset(W | MibRead, o0.size());
          out->_operands[1].reset();
          return kErrorOk;
        }
      }
      break;
    }

    case InstDB::RWInfo::kCategoryMovabs: {
      if (opCount == 2) {
        if (Reg::isGp(operands[0]) && operands[1].isMem()) {
          const Reg& o0 = operands[0].as<Reg>();
          out->_operands[0].reset(W | RegPhys, o0.size(), Gp::kIdAx);
          out->_operands[1].reset(R | MibRead, o0.size());
          rwZeroExtendGp(out->_operands[0], operands[0].as<Gp>(), nativeGpSize);
          return kErrorOk;
        }

        if (operands[0].isMem() && Reg::isGp(operands[1])) {
          const Reg& o1 = operands[1].as<Reg>();
          out->_operands[0].reset(W | MibRead, o1.size());
          out->_operands[1].reset(R | RegPhys, o1.size(), Gp::kIdAx);
          return kErrorOk;
        }

        if (Reg::isGp(operands[0]) && operands[1].isImm()) {
          const Reg& o0 = operands[0].as<Reg>();
          out->_operands[0].reset(W, o0.size());
          out->_operands[1].reset();

          rwZeroExtendGp(out->_operands[0], operands[0].as<Gp>(), nativeGpSize);
          return kErrorOk;
        }
      }
      break;
    }

    case InstDB::RWInfo::kCategoryImul: {
      // Special case for 'imul' instruction.
      //
      // There are 3 variants in general:
      //
      //   1. Standard multiplication: 'A = A * B'.
      //   2. Multiplication with imm: 'A = B * C'.
      //   3. Extended multiplication: 'A:B = B * C'.

      if (opCount == 2) {
        if (operands[0].isReg() && operands[1].isImm()) {
          out->_operands[0].reset(X, operands[0].as<Reg>().size());
          out->_operands[1].reset();

          rwZeroExtendGp(out->_operands[0], operands[0].as<Gp>(), nativeGpSize);
          return kErrorOk;
        }

        if (Reg::isGpw(operands[0]) && operands[1].x86RmSize() == 1) {
          // imul ax, r8/m8 <- AX = AL * r8/m8
          out->_operands[0].reset(X | RegPhys, 2, Gp::kIdAx);
          out->_operands[0].setReadByteMask(Support::lsbMask<uint64_t>(1));
          out->_operands[1].reset(R | RegM, 1);
        }
        else {
          // imul r?, r?/m?
          out->_operands[0].reset(X, operands[0].as<Gp>().size());
          out->_operands[1].reset(R | RegM, operands[0].as<Gp>().size());
          rwZeroExtendGp(out->_operands[0], operands[0].as<Gp>(), nativeGpSize);
        }

        if (operands[1].isMem())
          out->_operands[1].addOpFlags(MibRead);
        return kErrorOk;
      }

      if (opCount == 3) {
        if (operands[2].isImm()) {
          out->_operands[0].reset(W, operands[0].x86RmSize());
          out->_operands[1].reset(R | RegM, operands[1].x86RmSize());
          out->_operands[2].reset();

          rwZeroExtendGp(out->_operands[0], operands[0].as<Gp>(), nativeGpSize);
          if (operands[1].isMem())
            out->_operands[1].addOpFlags(MibRead);
          return kErrorOk;
        }
        else {
          out->_operands[0].reset(W | RegPhys, operands[0].x86RmSize(), Gp::kIdDx);
          out->_operands[1].reset(X | RegPhys, operands[1].x86RmSize(), Gp::kIdAx);
          out->_operands[2].reset(R | RegM, operands[2].x86RmSize());

          rwZeroExtendGp(out->_operands[0], operands[0].as<Gp>(), nativeGpSize);
          rwZeroExtendGp(out->_operands[1], operands[1].as<Gp>(), nativeGpSize);
          if (operands[2].isMem())
            out->_operands[2].addOpFlags(MibRead);
          return kErrorOk;
        }
      }
      break;
    }

    case InstDB::RWInfo::kCategoryMovh64: {
      // Special case for 'movhpd|movhps' instructions. Note that this is only required for legacy (non-AVX)
      // variants as AVX instructions use either 2 or 3 operands that are in `kCategoryGeneric` category.
      if (opCount == 2) {
        if (BaseReg::isVec(operands[0]) && operands[1].isMem()) {
          out->_operands[0].reset(W, 8);
          out->_operands[0].setWriteByteMask(Support::lsbMask<uint64_t>(8) << 8);
          out->_operands[1].reset(R | MibRead, 8);
          return kErrorOk;
        }

        if (operands[0].isMem() && BaseReg::isVec(operands[1])) {
          out->_operands[0].reset(W | MibRead, 8);
          out->_operands[1].reset(R, 8);
          out->_operands[1].setReadByteMask(Support::lsbMask<uint64_t>(8) << 8);
          return kErrorOk;
        }
      }
      break;
    }

    case InstDB::RWInfo::kCategoryPunpcklxx: {
      // Special case for 'punpcklbw|punpckldq|punpcklwd' instructions.
      if (opCount == 2) {
        if (Reg::isXmm(operands[0])) {
          out->_operands[0].reset(X, 16);
          out->_operands[0].setReadByteMask(0x0F0Fu);
          out->_operands[0].setWriteByteMask(0xFFFFu);
          out->_operands[1].reset(R, 16);
          out->_operands[1].setWriteByteMask(0x0F0Fu);

          if (Reg::isXmm(operands[1])) {
            return kErrorOk;
          }

          if (operands[1].isMem()) {
            out->_operands[1].addOpFlags(MibRead);
            return kErrorOk;
          }
        }

        if (Reg::isMm(operands[0])) {
          out->_operands[0].reset(X, 8);
          out->_operands[0].setReadByteMask(0x0Fu);
          out->_operands[0].setWriteByteMask(0xFFu);
          out->_operands[1].reset(R, 4);
          out->_operands[1].setReadByteMask(0x0Fu);

          if (Reg::isMm(operands[1])) {
            return kErrorOk;
          }

          if (operands[1].isMem()) {
            out->_operands[1].addOpFlags(MibRead);
            return kErrorOk;
          }
        }
      }
      break;
    }

    case InstDB::RWInfo::kCategoryVmaskmov: {
      // Special case for 'vmaskmovpd|vmaskmovps|vpmaskmovd|vpmaskmovq' instructions.
      if (opCount == 3) {
        if (BaseReg::isVec(operands[0]) && BaseReg::isVec(operands[1]) && operands[2].isMem()) {
          out->_operands[0].reset(W, operands[0].x86RmSize());
          out->_operands[1].reset(R, operands[1].x86RmSize());
          out->_operands[2].reset(R | MibRead, operands[1].x86RmSize());

          rwZeroExtendAvxVec(out->_operands[0], operands[0].as<Vec>());
          return kErrorOk;
        }

        if (operands[0].isMem() && BaseReg::isVec(operands[1]) && BaseReg::isVec(operands[2])) {
          out->_operands[0].reset(X | MibRead, operands[1].x86RmSize());
          out->_operands[1].reset(R, operands[1].x86RmSize());
          out->_operands[2].reset(R, operands[2].x86RmSize());
          return kErrorOk;
        }
      }
      break;
    }

    case InstDB::RWInfo::kCategoryVmovddup: {
      // Special case for 'vmovddup' instruction. This instruction has an interesting semantic as 128-bit XMM
      // version only uses 64-bit memory operand (m64), however, 256/512-bit versions use 256/512-bit memory
      // operand, respectively.
      if (opCount == 2) {
        if (BaseReg::isVec(operands[0]) && BaseReg::isVec(operands[1])) {
          uint32_t o0Size = operands[0].x86RmSize();
          uint32_t o1Size = o0Size == 16 ? 8 : o0Size;

          out->_operands[0].reset(W, o0Size);
          out->_operands[1].reset(R | RegM, o1Size);
          out->_operands[1]._readByteMask &= 0x00FF00FF00FF00FFu;

          rwZeroExtendAvxVec(out->_operands[0], operands[0].as<Vec>());
          return rwHandleAVX512(inst, commonInfo, out);
        }

        if (BaseReg::isVec(operands[0]) && operands[1].isMem()) {
          uint32_t o0Size = operands[0].x86RmSize();
          uint32_t o1Size = o0Size == 16 ? 8 : o0Size;

          out->_operands[0].reset(W, o0Size);
          out->_operands[1].reset(R | MibRead, o1Size);

          rwZeroExtendAvxVec(out->_operands[0], operands[0].as<Vec>());
          return rwHandleAVX512(inst, commonInfo, out);
        }
      }
      break;
    }

    case InstDB::RWInfo::kCategoryVmovmskpd:
    case InstDB::RWInfo::kCategoryVmovmskps: {
      // Special case for 'vmovmskpd|vmovmskps' instructions.
      if (opCount == 2) {
        if (BaseReg::isGp(operands[0]) && BaseReg::isVec(operands[1])) {
          out->_operands[0].reset(W, 1);
          out->_operands[0].setExtendByteMask(Support::lsbMask<uint32_t>(nativeGpSize - 1) << 1);
          out->_operands[1].reset(R, operands[1].x86RmSize());
          return kErrorOk;
        }
      }
      break;
    }

    case InstDB::RWInfo::kCategoryVmov1_2:
    case InstDB::RWInfo::kCategoryVmov1_4:
    case InstDB::RWInfo::kCategoryVmov1_8: {
      // Special case for instructions where the destination is 1:N (narrowing).
      //
      // Vmov1_2:
      //   vcvtpd2dq|vcvttpd2dq
      //   vcvtpd2udq|vcvttpd2udq
      //   vcvtpd2ps|vcvtps2ph
      //   vcvtqq2ps|vcvtuqq2ps
      //   vpmovwb|vpmovswb|vpmovuswb
      //   vpmovdw|vpmovsdw|vpmovusdw
      //   vpmovqd|vpmovsqd|vpmovusqd
      //
      // Vmov1_4:
      //   vpmovdb|vpmovsdb|vpmovusdb
      //   vpmovqw|vpmovsqw|vpmovusqw
      //
      // Vmov1_8:
      //   pmovmskb|vpmovmskb
      //   vpmovqb|vpmovsqb|vpmovusqb
      uint32_t shift = instRwInfo.category - InstDB::RWInfo::kCategoryVmov1_2 + 1;

      if (opCount >= 2) {
        if (opCount >= 3) {
          if (opCount > 3)
            return DebugUtils::errored(kErrorInvalidInstruction);
          out->_operands[2].reset();
        }

        if (operands[0].isReg() && operands[1].isReg()) {
          uint32_t size1 = operands[1].x86RmSize();
          uint32_t size0 = size1 >> shift;

          out->_operands[0].reset(W, size0);
          out->_operands[1].reset(R, size1);

          if (instRmInfo.rmOpsMask & 0x1) {
            out->_operands[0].addOpFlags(RegM);
            out->_operands[0].setRmSize(size0);
          }

          if (instRmInfo.rmOpsMask & 0x2) {
            out->_operands[1].addOpFlags(RegM);
            out->_operands[1].setRmSize(size1);
          }

          // Handle 'pmovmskb|vpmovmskb'.
          if (BaseReg::isGp(operands[0]))
            rwZeroExtendGp(out->_operands[0], operands[0].as<Gp>(), nativeGpSize);

          if (BaseReg::isVec(operands[0]))
            rwZeroExtendAvxVec(out->_operands[0], operands[0].as<Vec>());

          return rwHandleAVX512(inst, commonInfo, out);
        }

        if (operands[0].isReg() && operands[1].isMem()) {
          uint32_t size1 = operands[1].x86RmSize() ? operands[1].x86RmSize() : uint32_t(16);
          uint32_t size0 = size1 >> shift;

          out->_operands[0].reset(W, size0);
          out->_operands[1].reset(R | MibRead, size1);

          if (BaseReg::isVec(operands[0]))
            rwZeroExtendAvxVec(out->_operands[0], operands[0].as<Vec>());

          return kErrorOk;
        }

        if (operands[0].isMem() && operands[1].isReg()) {
          uint32_t size1 = operands[1].x86RmSize();
          uint32_t size0 = size1 >> shift;

          out->_operands[0].reset(W | MibRead, size0);
          out->_operands[1].reset(R, size1);

          return rwHandleAVX512(inst, commonInfo, out);
        }
      }
      break;
    }

    case InstDB::RWInfo::kCategoryVmov2_1:
    case InstDB::RWInfo::kCategoryVmov4_1:
    case InstDB::RWInfo::kCategoryVmov8_1: {
      // Special case for instructions where the destination is N:1 (widening).
      //
      // Vmov2_1:
      //   vcvtdq2pd|vcvtudq2pd
      //   vcvtps2pd|vcvtph2ps
      //   vcvtps2qq|vcvtps2uqq
      //   vcvttps2qq|vcvttps2uqq
      //   vpmovsxbw|vpmovzxbw
      //   vpmovsxwd|vpmovzxwd
      //   vpmovsxdq|vpmovzxdq
      //
      // Vmov4_1:
      //   vpmovsxbd|vpmovzxbd
      //   vpmovsxwq|vpmovzxwq
      //
      // Vmov8_1:
      //   vpmovsxbq|vpmovzxbq
      uint32_t shift = instRwInfo.category - InstDB::RWInfo::kCategoryVmov2_1 + 1;

      if (opCount >= 2) {
        if (opCount >= 3) {
          if (opCount > 3)
            return DebugUtils::errored(kErrorInvalidInstruction);
          out->_operands[2].reset();
        }

        uint32_t size0 = operands[0].x86RmSize();
        uint32_t size1 = size0 >> shift;

        out->_operands[0].reset(W, size0);
        out->_operands[1].reset(R, size1);

        if (BaseReg::isVec(operands[0]))
          rwZeroExtendAvxVec(out->_operands[0], operands[0].as<Vec>());

        if (operands[0].isReg() && operands[1].isReg()) {
          if (instRmInfo.rmOpsMask & 0x1) {
            out->_operands[0].addOpFlags(RegM);
            out->_operands[0].setRmSize(size0);
          }

          if (instRmInfo.rmOpsMask & 0x2) {
            out->_operands[1].addOpFlags(RegM);
            out->_operands[1].setRmSize(size1);
          }

          return rwHandleAVX512(inst, commonInfo, out);
        }

        if (operands[0].isReg() && operands[1].isMem()) {
          out->_operands[1].addOpFlags(MibRead);

          return rwHandleAVX512(inst, commonInfo, out);
        }
      }
      break;
    }
  }

  return DebugUtils::errored(kErrorInvalidInstruction);
}
#endif // !ASMJIT_NO_INTROSPECTION

// x86::InstInternal - QueryFeatures
// =================================

#ifndef ASMJIT_NO_INTROSPECTION
struct RegAnalysis {
  uint32_t regTypeMask;
  uint32_t highVecUsed;

  inline bool hasRegType(RegType regType) const noexcept {
    return Support::bitTest(regTypeMask, regType);
  }
};

static RegAnalysis InstInternal_regAnalysis(const Operand_* operands, size_t opCount) noexcept {
  uint32_t mask = 0;
  uint32_t highVecUsed = 0;

  for (uint32_t i = 0; i < opCount; i++) {
    const Operand_& op = operands[i];
    if (op.isReg()) {
      const BaseReg& reg = op.as<BaseReg>();
      mask |= Support::bitMask(reg.type());
      if (reg.isVec())
        highVecUsed |= uint32_t(reg.id() >= 16 && reg.id() < 32);
    }
    else if (op.isMem()) {
      const BaseMem& mem = op.as<BaseMem>();
      if (mem.hasBaseReg()) mask |= Support::bitMask(mem.baseType());
      if (mem.hasIndexReg()) {
        mask |= Support::bitMask(mem.indexType());
        highVecUsed |= uint32_t(mem.indexId() >= 16 && mem.indexId() < 32);
      }
    }
  }

  return RegAnalysis { mask, highVecUsed };
}

static inline uint32_t InstInternal_usesAvx512(InstOptions instOptions, const RegOnly& extraReg, const RegAnalysis& regAnalysis) noexcept {
  uint32_t hasEvex = uint32_t(instOptions & (InstOptions::kX86_Evex | InstOptions::kX86_AVX512Mask));
  uint32_t hasKMask = extraReg.type() == RegType::kX86_KReg;
  uint32_t hasKOrZmm = regAnalysis.regTypeMask & Support::bitMask(RegType::kX86_Zmm, RegType::kX86_KReg);

  return hasEvex | hasKMask | hasKOrZmm;
}

Error queryFeatures(Arch arch, const BaseInst& inst, const Operand_* operands, size_t opCount, CpuFeatures* out) noexcept {
  typedef CpuFeatures::X86 Ext;

  // Only called when `arch` matches X86 family.
  DebugUtils::unused(arch);
  ASMJIT_ASSERT(Environment::isFamilyX86(arch));

  // Get the instruction data.
  InstId instId = inst.id();
  InstOptions options = inst.options();

  if (ASMJIT_UNLIKELY(!Inst::isDefinedId(instId)))
    return DebugUtils::errored(kErrorInvalidInstruction);

  const InstDB::InstInfo& instInfo = InstDB::infoById(instId);
  const InstDB::AdditionalInfo& additionalInfo = InstDB::_additionalInfoTable[instInfo._additionalInfoIndex];

  const uint8_t* fData = additionalInfo.featuresBegin();
  const uint8_t* fEnd = additionalInfo.featuresEnd();

  // Copy all features to `out`.
  out->reset();
  do {
    uint32_t feature = fData[0];
    if (!feature)
      break;
    out->add(feature);
  } while (++fData != fEnd);

  // Since AsmJit aggregates instructions that share the same name we have to
  // deal with some special cases and also with MMX/SSE and AVX/AVX2 overlaps.
  if (fData != additionalInfo.featuresBegin()) {
    RegAnalysis regAnalysis = InstInternal_regAnalysis(operands, opCount);

    // Handle MMX vs SSE overlap.
    if (out->has(Ext::kMMX) || out->has(Ext::kMMX2)) {
      // Only instructions defined by SSE and SSE2 overlap. Instructions introduced by newer instruction sets like
      // SSE3+ don't state MMX as they require SSE3+.
      if (out->has(Ext::kSSE) || out->has(Ext::kSSE2)) {
        if (!regAnalysis.hasRegType(RegType::kX86_Xmm)) {
          // The instruction doesn't use XMM register(s), thus it's MMX/MMX2 only.
          out->remove(Ext::kSSE);
          out->remove(Ext::kSSE2);
          out->remove(Ext::kSSE4_1);
        }
        else {
          out->remove(Ext::kMMX);
          out->remove(Ext::kMMX2);
        }

        // Special case: PEXTRW instruction is MMX/SSE2 instruction. However, MMX/SSE version cannot access memory
        // (only register to register extract) so when SSE4.1 introduced the whole family of PEXTR/PINSR instructions
        // they also introduced PEXTRW with a new opcode 0x15 that can extract directly to memory. This instruction
        // is, of course, not compatible with MMX/SSE2 and would #UD if SSE4.1 is not supported.
        if (instId == Inst::kIdPextrw) {
          if (opCount >= 1 && operands[0].isMem())
            out->remove(Ext::kSSE2);
          else
            out->remove(Ext::kSSE4_1);
        }
      }
    }

    // Handle PCLMULQDQ vs VPCLMULQDQ.
    if (out->has(Ext::kVPCLMULQDQ)) {
      if (regAnalysis.hasRegType(RegType::kX86_Zmm) || Support::test(options, InstOptions::kX86_Evex)) {
        // AVX512_F & VPCLMULQDQ.
        out->remove(Ext::kAVX, Ext::kPCLMULQDQ);
      }
      else if (regAnalysis.hasRegType(RegType::kX86_Ymm)) {
        out->remove(Ext::kAVX512_F, Ext::kAVX512_VL);
      }
      else {
        // AVX & PCLMULQDQ.
        out->remove(Ext::kAVX512_F, Ext::kAVX512_VL, Ext::kVPCLMULQDQ);
      }
    }

    // Handle AVX vs AVX2 overlap.
    if (out->has(Ext::kAVX) && out->has(Ext::kAVX2)) {
      bool isAVX2 = true;
      // Special case: VBROADCASTSS and VBROADCASTSD were introduced in AVX, but only version that uses memory as a
      // source operand. AVX2 then added support for register source operand.
      if (instId == Inst::kIdVbroadcastss || instId == Inst::kIdVbroadcastsd) {
        if (opCount > 1 && operands[1].isMem())
          isAVX2 = false;
      }
      else {
        // AVX instruction set doesn't support integer operations on YMM registers as these were later introcuced by
        // AVX2. In our case we have to check if YMM register(s) are in use and if that is the case this is an AVX2
        // instruction.
        if (!(regAnalysis.regTypeMask & Support::bitMask(RegType::kX86_Ymm, RegType::kX86_Zmm)))
          isAVX2 = false;
      }

      if (isAVX2)
        out->remove(Ext::kAVX);
      else
        out->remove(Ext::kAVX2);
    }

    // Handle AVX vs AVX512 overlap.
    //
    // In general, non-AVX encoding is preferred, however, AVX encoded instructions that were initially provided
    // as AVX-512 instructions must naturally prefer AVX-512 encoding, as that was the first one provided.
    if (out->hasAny(Ext::kAVX,
                    Ext::kAVX_IFMA,
                    Ext::kAVX_NE_CONVERT,
                    Ext::kAVX_VNNI,
                    Ext::kAVX2,
                    Ext::kF16C,
                    Ext::kFMA)
        &&
        out->hasAny(Ext::kAVX512_BF16,
                    Ext::kAVX512_BW,
                    Ext::kAVX512_DQ,
                    Ext::kAVX512_F,
                    Ext::kAVX512_IFMA,
                    Ext::kAVX512_VNNI)) {

      uint32_t useEvex = InstInternal_usesAvx512(options, inst.extraReg(), regAnalysis) | regAnalysis.highVecUsed;
      switch (instId) {
        // Special case: VPBROADCAST[B|D|Q|W] only supports r32/r64 with EVEX prefix.
        case Inst::kIdVpbroadcastb:
        case Inst::kIdVpbroadcastd:
        case Inst::kIdVpbroadcastq:
        case Inst::kIdVpbroadcastw:
          useEvex |= uint32_t(opCount >= 2 && x86::Reg::isGp(operands[1]));
          break;

        case Inst::kIdVcvtpd2dq:
        case Inst::kIdVcvtpd2ps:
        case Inst::kIdVcvttpd2dq:
          useEvex |= uint32_t(opCount >= 2 && Reg::isYmm(operands[0]));
          break;

        case Inst::kIdVgatherdpd:
        case Inst::kIdVgatherdps:
        case Inst::kIdVgatherqpd:
        case Inst::kIdVgatherqps:
        case Inst::kIdVpgatherdd:
        case Inst::kIdVpgatherdq:
        case Inst::kIdVpgatherqd:
        case Inst::kIdVpgatherqq:
          useEvex |= uint32_t(opCount == 2);
          break;

        // Special case: These instructions only allow `reg, reg. imm` combination in AVX|AVX2 mode, then
        // AVX-512 introduced `reg, reg/mem, imm` combination that uses EVEX prefix. This means that if
        // the second operand is memory then this is AVX-512_BW instruction and not AVX/AVX2 instruction.
        case Inst::kIdVpslldq:
        case Inst::kIdVpslld:
        case Inst::kIdVpsllq:
        case Inst::kIdVpsllw:
        case Inst::kIdVpsrad:
        case Inst::kIdVpsraq:
        case Inst::kIdVpsraw:
        case Inst::kIdVpsrld:
        case Inst::kIdVpsrldq:
        case Inst::kIdVpsrlq:
        case Inst::kIdVpsrlw:
          useEvex |= uint32_t(opCount >= 2 && operands[1].isMem());
          break;

        // Special case: VPERMPD - AVX2 vs AVX512-F case.
        case Inst::kIdVpermpd:
          useEvex |= uint32_t(opCount >= 3 && !operands[2].isImm());
          break;

        // Special case: VPERMQ - AVX2 vs AVX512-F case.
        case Inst::kIdVpermq:
          useEvex |= uint32_t(opCount >= 3 && (operands[1].isMem() || !operands[2].isImm()));
          break;
      }

      if (instInfo.commonInfo().preferEvex() && !Support::test(options, InstOptions::kX86_Vex | InstOptions::kX86_Vex3))
        useEvex = 1;

      if (useEvex) {
        out->remove(Ext::kAVX,
                    Ext::kAVX_IFMA,
                    Ext::kAVX_NE_CONVERT,
                    Ext::kAVX_VNNI,
                    Ext::kAVX2,
                    Ext::kF16C,
                    Ext::kFMA);
      }
      else {
        out->remove(Ext::kAVX512_BF16,
                    Ext::kAVX512_BW,
                    Ext::kAVX512_DQ,
                    Ext::kAVX512_F,
                    Ext::kAVX512_IFMA,
                    Ext::kAVX512_VL,
                    Ext::kAVX512_VNNI);
      }
    }

    // Clear AVX512_VL if ZMM register is used.
    if (regAnalysis.hasRegType(RegType::kX86_Zmm))
      out->remove(Ext::kAVX512_VL);
  }

  return kErrorOk;
}
#endif // !ASMJIT_NO_INTROSPECTION

} // {InstInternal}

// x86::InstInternal - Tests
// =========================

#if defined(ASMJIT_TEST)
#ifndef ASMJIT_NO_TEXT
UNIT(x86_inst_api_text) {
  // All known instructions should be matched.
  INFO("Matching all X86 instructions");
  for (uint32_t a = 1; a < Inst::_kIdCount; a++) {
    StringTmp<128> aName;
    EXPECT_EQ(InstInternal::instIdToString(a, aName), kErrorOk)
      .message("Failed to get the name of instruction #%u", a);

    uint32_t b = InstInternal::stringToInstId(aName.data(), aName.size());
    StringTmp<128> bName;
    InstInternal::instIdToString(b, bName);
    EXPECT_EQ(a, b)
      .message("Instructions do not match \"%s\" (#%u) != \"%s\" (#%u)", aName.data(), a, bName.data(), b);
  }
}
#endif // !ASMJIT_NO_TEXT

#ifndef ASMJIT_NO_INTROSPECTION
template<typename... Args>
static Error queryFeaturesInline(CpuFeatures* out, Arch arch, BaseInst inst, Args&&... args) {
  Operand_ opArray[] = { std::forward<Args>(args)... };
  return InstInternal::queryFeatures(arch, inst, opArray, sizeof...(args), out);
}

UNIT(x86_inst_api_cpu_features) {
  INFO("Verifying whether SSE2+ features are reported correctly for legacy instructions");
  {
    CpuFeatures f;

    queryFeaturesInline(&f, Arch::kX64, BaseInst(Inst::kIdPaddd), xmm1, xmm2);
    EXPECT_TRUE(f.x86().hasSSE2());

    queryFeaturesInline(&f, Arch::kX64, BaseInst(Inst::kIdAddsubpd), xmm1, xmm2);
    EXPECT_TRUE(f.x86().hasSSE3());

    queryFeaturesInline(&f, Arch::kX64, BaseInst(Inst::kIdPshufb), xmm1, xmm2);
    EXPECT_TRUE(f.x86().hasSSSE3());

    queryFeaturesInline(&f, Arch::kX64, BaseInst(Inst::kIdBlendpd), xmm1, xmm2, Imm(1));
    EXPECT_TRUE(f.x86().hasSSE4_1());

    queryFeaturesInline(&f, Arch::kX64, BaseInst(Inst::kIdCrc32), eax, al);
    EXPECT_TRUE(f.x86().hasSSE4_2());
  }

  INFO("Verifying whether AVX+ features are reported correctly for AVX instructions");
  {
    CpuFeatures f;

    queryFeaturesInline(&f, Arch::kX64, BaseInst(Inst::kIdVpaddd), xmm1, xmm2, xmm3);
    EXPECT_TRUE(f.x86().hasAVX());

    queryFeaturesInline(&f, Arch::kX64, BaseInst(Inst::kIdVpaddd), ymm1, ymm2, ymm3);
    EXPECT_TRUE(f.x86().hasAVX2());

    queryFeaturesInline(&f, Arch::kX64, BaseInst(Inst::kIdVaddsubpd), xmm1, xmm2, xmm3);
    EXPECT_TRUE(f.x86().hasAVX());

    queryFeaturesInline(&f, Arch::kX64, BaseInst(Inst::kIdVaddsubpd), ymm1, ymm2, ymm3);
    EXPECT_TRUE(f.x86().hasAVX());

    queryFeaturesInline(&f, Arch::kX64, BaseInst(Inst::kIdVpshufb), xmm1, xmm2, xmm3);
    EXPECT_TRUE(f.x86().hasAVX());

    queryFeaturesInline(&f, Arch::kX64, BaseInst(Inst::kIdVpshufb), ymm1, ymm2, ymm3);
    EXPECT_TRUE(f.x86().hasAVX2());

    queryFeaturesInline(&f, Arch::kX64, BaseInst(Inst::kIdVblendpd), xmm1, xmm2, xmm3, Imm(1));
    EXPECT_TRUE(f.x86().hasAVX());

    queryFeaturesInline(&f, Arch::kX64, BaseInst(Inst::kIdVblendpd), ymm1, ymm2, ymm3, Imm(1));
    EXPECT_TRUE(f.x86().hasAVX());

    queryFeaturesInline(&f, Arch::kX64, BaseInst(Inst::kIdVpunpcklbw), xmm1, xmm2, xmm3);
    EXPECT_TRUE(f.x86().hasAVX());

    queryFeaturesInline(&f, Arch::kX64, BaseInst(Inst::kIdVpunpcklbw), ymm1, ymm2, ymm3);
    EXPECT_TRUE(f.x86().hasAVX2());
  }

  INFO("Verifying whether AVX2 / AVX512 features are reported correctly for vpgatherxx instructions");
  {
    CpuFeatures f;

    queryFeaturesInline(&f, Arch::kX64, BaseInst(Inst::kIdVpgatherdd), xmm1, ptr(rax, xmm2), xmm3);
    EXPECT_TRUE(f.x86().hasAVX2());
    EXPECT_FALSE(f.x86().hasAVX512_F());

    // NOTE: This instruction is unencodable, but sometimes this signature is used to check the support (without the {k}).
    queryFeaturesInline(&f, Arch::kX64, BaseInst(Inst::kIdVpgatherdd), xmm1, ptr(rax, xmm2));
    EXPECT_FALSE(f.x86().hasAVX2());
    EXPECT_TRUE(f.x86().hasAVX512_F());

    queryFeaturesInline(&f, Arch::kX64, BaseInst(Inst::kIdVpgatherdd, InstOptions::kNone, k1), xmm1, ptr(rax, xmm2));
    EXPECT_FALSE(f.x86().hasAVX2());
    EXPECT_TRUE(f.x86().hasAVX512_F());
  }
}
#endif // !ASMJIT_NO_INTROSPECTION

#ifndef ASMJIT_NO_INTROSPECTION
template<typename... Args>
static Error queryRWInfoInline(InstRWInfo* out, Arch arch, BaseInst inst, Args&&... args) {
  Operand_ opArray[] = { std::forward<Args>(args)... };
  return InstInternal::queryRWInfo(arch, inst, opArray, sizeof...(args), out);
}

UNIT(x86_inst_api_rm_features) {
  INFO("Verifying whether RM/feature is reported correctly for PEXTRW instruction");
  {
    InstRWInfo rwi;

    queryRWInfoInline(&rwi, Arch::kX64, BaseInst(Inst::kIdPextrw), eax, mm1, imm(1));
    EXPECT_EQ(rwi.rmFeature(), 0u);

    queryRWInfoInline(&rwi, Arch::kX64, BaseInst(Inst::kIdPextrw), eax, xmm1, imm(1));
    EXPECT_EQ(rwi.rmFeature(), CpuFeatures::X86::kSSE4_1);
  }

  INFO("Verifying whether RM/feature is reported correctly for AVX512 shift instructions");
  {
    InstRWInfo rwi;

    queryRWInfoInline(&rwi, Arch::kX64, BaseInst(Inst::kIdVpslld), xmm1, xmm2, imm(8));
    EXPECT_EQ(rwi.rmFeature(), CpuFeatures::X86::kAVX512_F);

    queryRWInfoInline(&rwi, Arch::kX64, BaseInst(Inst::kIdVpsllq), ymm1, ymm2, imm(8));
    EXPECT_EQ(rwi.rmFeature(), CpuFeatures::X86::kAVX512_F);

    queryRWInfoInline(&rwi, Arch::kX64, BaseInst(Inst::kIdVpsrad), xmm1, xmm2, imm(8));
    EXPECT_EQ(rwi.rmFeature(), CpuFeatures::X86::kAVX512_F);

    queryRWInfoInline(&rwi, Arch::kX64, BaseInst(Inst::kIdVpsrld), ymm1, ymm2, imm(8));
    EXPECT_EQ(rwi.rmFeature(), CpuFeatures::X86::kAVX512_F);

    queryRWInfoInline(&rwi, Arch::kX64, BaseInst(Inst::kIdVpsrlq), xmm1, xmm2, imm(8));
    EXPECT_EQ(rwi.rmFeature(), CpuFeatures::X86::kAVX512_F);

    queryRWInfoInline(&rwi, Arch::kX64, BaseInst(Inst::kIdVpslldq), xmm1, xmm2, imm(8));
    EXPECT_EQ(rwi.rmFeature(), CpuFeatures::X86::kAVX512_BW);

    queryRWInfoInline(&rwi, Arch::kX64, BaseInst(Inst::kIdVpsllw), ymm1, ymm2, imm(8));
    EXPECT_EQ(rwi.rmFeature(), CpuFeatures::X86::kAVX512_BW);

    queryRWInfoInline(&rwi, Arch::kX64, BaseInst(Inst::kIdVpsraw), xmm1, xmm2, imm(8));
    EXPECT_EQ(rwi.rmFeature(), CpuFeatures::X86::kAVX512_BW);

    queryRWInfoInline(&rwi, Arch::kX64, BaseInst(Inst::kIdVpsrldq), ymm1, ymm2, imm(8));
    EXPECT_EQ(rwi.rmFeature(), CpuFeatures::X86::kAVX512_BW);

    queryRWInfoInline(&rwi, Arch::kX64, BaseInst(Inst::kIdVpsrlw), xmm1, xmm2, imm(8));
    EXPECT_EQ(rwi.rmFeature(), CpuFeatures::X86::kAVX512_BW);

    queryRWInfoInline(&rwi, Arch::kX64, BaseInst(Inst::kIdVpslld), xmm1, xmm2, xmm3);
    EXPECT_EQ(rwi.rmFeature(), 0u);

    queryRWInfoInline(&rwi, Arch::kX64, BaseInst(Inst::kIdVpsllw), xmm1, xmm2, xmm3);
    EXPECT_EQ(rwi.rmFeature(), 0u);
  }
}
#endif // !ASMJIT_NO_INTROSPECTION

#endif // ASMJIT_TEST

ASMJIT_END_SUB_NAMESPACE

#endif // !ASMJIT_NO_X86
