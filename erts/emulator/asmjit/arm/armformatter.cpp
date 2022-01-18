// This file is part of AsmJit project <https://asmjit.com>
//
// See asmjit.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#include "../core/api-build_p.h"
#ifndef ASMJIT_NO_LOGGING

#include "../core/misc_p.h"
#include "../core/support.h"
#include "../arm/armformatter_p.h"
#include "../arm/armoperand.h"
#include "../arm/a64instdb_p.h"

#ifndef ASMJIT_NO_COMPILER
  #include "../core/compiler.h"
#endif

ASMJIT_BEGIN_SUB_NAMESPACE(arm)

// arm::FormatterInternal - Format Feature
// =======================================

Error FormatterInternal::formatFeature(String& sb, uint32_t featureId) noexcept {
  // @EnumStringBegin{"enum": "CpuFeatures::ARM", "output": "sFeature", "strip": "k"}@
  static const char sFeatureString[] =
    "None\0"
    "THUMB\0"
    "THUMBv2\0"
    "ARMv6\0"
    "ARMv7\0"
    "ARMv8a\0"
    "ARMv8_1a\0"
    "ARMv8_2a\0"
    "ARMv8_3a\0"
    "ARMv8_4a\0"
    "ARMv8_5a\0"
    "ARMv8_6a\0"
    "ARMv8_7a\0"
    "VFPv2\0"
    "VFPv3\0"
    "VFPv4\0"
    "VFP_D32\0"
    "AES\0"
    "ALTNZCV\0"
    "ASIMD\0"
    "BF16\0"
    "BTI\0"
    "CPUID\0"
    "CRC32\0"
    "DGH\0"
    "DIT\0"
    "DOTPROD\0"
    "EDSP\0"
    "FCMA\0"
    "FJCVTZS\0"
    "FLAGM\0"
    "FP16CONV\0"
    "FP16FML\0"
    "FP16FULL\0"
    "FRINT\0"
    "I8MM\0"
    "IDIVA\0"
    "IDIVT\0"
    "LSE\0"
    "MTE\0"
    "RCPC_IMMO\0"
    "RDM\0"
    "PMU\0"
    "PMULL\0"
    "RNG\0"
    "SB\0"
    "SHA1\0"
    "SHA2\0"
    "SHA3\0"
    "SHA512\0"
    "SM3\0"
    "SM4\0"
    "SSBS\0"
    "SVE\0"
    "SVE_BF16\0"
    "SVE_F32MM\0"
    "SVE_F64MM\0"
    "SVE_I8MM\0"
    "SVE_PMULL\0"
    "SVE2\0"
    "SVE2_AES\0"
    "SVE2_BITPERM\0"
    "SVE2_SHA3\0"
    "SVE2_SM4\0"
    "TME\0"
    "<Unknown>\0";

  static const uint16_t sFeatureIndex[] = {
    0, 5, 11, 19, 25, 31, 38, 47, 56, 65, 74, 83, 92, 101, 107, 113, 119, 127,
    131, 139, 145, 150, 154, 160, 166, 170, 174, 182, 187, 192, 200, 206, 215,
    223, 232, 238, 243, 249, 255, 259, 263, 273, 277, 281, 287, 291, 294, 299,
    304, 309, 316, 320, 324, 329, 333, 342, 352, 362, 371, 381, 386, 395, 408,
    418, 427, 431
  };
  // @EnumStringEnd@

  return sb.append(sFeatureString + sFeatureIndex[Support::min<uint32_t>(featureId, uint32_t(CpuFeatures::ARM::kMaxValue) + 1)]);
}

// arm::FormatterInternal - Format Constants
// =========================================

ASMJIT_FAVOR_SIZE Error FormatterInternal::formatCondCode(String& sb, uint32_t condCode) noexcept {
  static const char condCodeData[] =
    "eq\0" "ne\0" "cs\0" "cc\0" "mi\0" "pl\0" "vs\0" "vc\0"
    "hi\0" "ls\0" "ge\0" "lt\0" "gt\0" "le\0" "al\0" "na\0"
    "<Unknown>";
  return sb.append(condCodeData + Support::min<uint32_t>(condCode, 16u) * 3);
}

ASMJIT_FAVOR_SIZE Error FormatterInternal::formatShiftOp(String& sb, ShiftOp shiftOp) noexcept {
  const char* str = "<Unknown>";
  switch (shiftOp) {
    case ShiftOp::kLSL: str = "lsl"; break;
    case ShiftOp::kLSR: str = "lsr"; break;
    case ShiftOp::kASR: str = "asr"; break;
    case ShiftOp::kROR: str = "ror"; break;
    case ShiftOp::kRRX: str = "rrx"; break;
    case ShiftOp::kMSL: str = "msl"; break;
    case ShiftOp::kUXTB: str = "uxtb"; break;
    case ShiftOp::kUXTH: str = "uxth"; break;
    case ShiftOp::kUXTW: str = "uxtw"; break;
    case ShiftOp::kUXTX: str = "uxtx"; break;
    case ShiftOp::kSXTB: str = "sxtb"; break;
    case ShiftOp::kSXTH: str = "sxth"; break;
    case ShiftOp::kSXTW: str = "sxtw"; break;
    case ShiftOp::kSXTX: str = "sxtx"; break;
  }
  return sb.append(str);
}

// arm::FormatterInternal - Format Register
// ========================================

ASMJIT_FAVOR_SIZE Error FormatterInternal::formatRegister(
  String& sb,
  FormatFlags flags,
  const BaseEmitter* emitter,
  Arch arch,
  RegType regType,
  uint32_t rId,
  uint32_t elementType,
  uint32_t elementIndex) noexcept {

  DebugUtils::unused(flags);
  DebugUtils::unused(arch);

  static const char bhsdq[] = "bhsdq";

  bool virtRegFormatted = false;

#ifndef ASMJIT_NO_COMPILER
  if (Operand::isVirtId(rId)) {
    if (emitter && emitter->isCompiler()) {
      const BaseCompiler* cc = static_cast<const BaseCompiler*>(emitter);
      if (cc->isVirtIdValid(rId)) {
        VirtReg* vReg = cc->virtRegById(rId);
        ASMJIT_ASSERT(vReg != nullptr);

        const char* name = vReg->name();
        if (name && name[0] != '\0')
          ASMJIT_PROPAGATE(sb.append(name));
        else
          ASMJIT_PROPAGATE(sb.appendFormat("%%%u", unsigned(Operand::virtIdToIndex(rId))));

        virtRegFormatted = true;
      }
    }
  }
#else
  DebugUtils::unused(emitter, flags);
#endif

  if (!virtRegFormatted) {
    char letter = '\0';
    switch (regType) {
      case RegType::kARM_GpW:
        if (rId == Gp::kIdZr)
          return sb.append("wzr");
        if (rId == Gp::kIdSp)
          return sb.append("wsp");

        letter = 'w';
        break;

      case RegType::kARM_GpX:
        if (rId == Gp::kIdZr)
          return sb.append("xzr");
        if (rId == Gp::kIdSp)
          return sb.append("sp");

        letter = 'x';
        break;

      case RegType::kARM_VecB:
      case RegType::kARM_VecH:
      case RegType::kARM_VecS:
      case RegType::kARM_VecD:
      case RegType::kARM_VecV:
        letter = bhsdq[uint32_t(regType) - uint32_t(RegType::kARM_VecB)];
        if (elementType)
          letter = 'v';
        break;

      default:
        ASMJIT_PROPAGATE(sb.appendFormat("<Reg-%u>?$u", uint32_t(regType), rId));
        break;
    }

    if (letter)
      ASMJIT_PROPAGATE(sb.appendFormat("%c%u", letter, rId));
  }

  if (elementType) {
    char elementLetter = '\0';
    uint32_t elementCount = 0;

    switch (elementType) {
      case Vec::kElementTypeB:
        elementLetter = 'b';
        elementCount = 16;
        break;

      case Vec::kElementTypeH:
        elementLetter = 'h';
        elementCount = 8;
        break;

      case Vec::kElementTypeS:
        elementLetter = 's';
        elementCount = 4;
        break;

      case Vec::kElementTypeD:
        elementLetter = 'd';
        elementCount = 2;
        break;

      default:
        return sb.append(".<Unknown>");
    }

    if (elementLetter) {
      if (elementIndex == 0xFFFFFFFFu) {
        if (regType == RegType::kARM_VecD)
          elementCount /= 2u;
        ASMJIT_PROPAGATE(sb.appendFormat(".%u%c", elementCount, elementLetter));
      }
      else {
        ASMJIT_PROPAGATE(sb.appendFormat(".%c[%u]", elementLetter, elementIndex));
      }
    }
  }

  return kErrorOk;
}

// arm::FormatterInternal - Format Operand
// =======================================

ASMJIT_FAVOR_SIZE Error FormatterInternal::formatOperand(
  String& sb,
  FormatFlags flags,
  const BaseEmitter* emitter,
  Arch arch,
  const Operand_& op) noexcept {

  if (op.isReg()) {
    const BaseReg& reg = op.as<BaseReg>();

    uint32_t elementType = op.as<Vec>().elementType();
    uint32_t elementIndex = op.as<Vec>().elementIndex();

    if (!op.as<Vec>().hasElementIndex())
      elementIndex = 0xFFFFFFFFu;

    return formatRegister(sb, flags, emitter, arch, reg.type(), reg.id(), elementType, elementIndex);
  }

  if (op.isMem()) {
    const Mem& m = op.as<Mem>();
    ASMJIT_PROPAGATE(sb.append('['));

    if (m.hasBase()) {
      if (m.hasBaseLabel()) {
        ASMJIT_PROPAGATE(Formatter::formatLabel(sb, flags, emitter, m.baseId()));
      }
      else {
        FormatFlags modifiedFlags = flags;
        if (m.isRegHome()) {
          ASMJIT_PROPAGATE(sb.append('&'));
          modifiedFlags &= ~FormatFlags::kRegCasts;
        }
        ASMJIT_PROPAGATE(formatRegister(sb, modifiedFlags, emitter, arch, m.baseType(), m.baseId()));
      }
    }
    else {
      // ARM really requires base.
      if (m.hasIndex() || m.hasOffset()) {
        ASMJIT_PROPAGATE(sb.append("<None>"));
      }
    }

    // The post index makes it look like there was another operand, but it's
    // still the part of AsmJit's `arm::Mem` operand so it's consistent with
    // other architectures.
    if (m.isPostIndex())
      ASMJIT_PROPAGATE(sb.append(']'));

    if (m.hasIndex()) {
      ASMJIT_PROPAGATE(sb.append(", "));
      ASMJIT_PROPAGATE(formatRegister(sb, flags, emitter, arch, m.indexType(), m.indexId()));
    }

    if (m.hasOffset()) {
      ASMJIT_PROPAGATE(sb.append(", "));

      int64_t off = int64_t(m.offset());
      uint32_t base = 10;

      if (Support::test(flags, FormatFlags::kHexOffsets) && uint64_t(off) > 9)
        base = 16;

      if (base == 10) {
        ASMJIT_PROPAGATE(sb.appendInt(off, base));
      }
      else {
        ASMJIT_PROPAGATE(sb.append("0x"));
        ASMJIT_PROPAGATE(sb.appendUInt(uint64_t(off), base));
      }
    }

    if (m.hasShift()) {
      ASMJIT_PROPAGATE(sb.append(' '));
      if (!m.isPreOrPost())
        ASMJIT_PROPAGATE(formatShiftOp(sb, (ShiftOp)m.predicate()));
      ASMJIT_PROPAGATE(sb.appendFormat(" %u", m.shift()));
    }

    if (!m.isPostIndex())
      ASMJIT_PROPAGATE(sb.append(']'));

    if (m.isPreIndex())
      ASMJIT_PROPAGATE(sb.append('!'));

    return kErrorOk;
  }

  if (op.isImm()) {
    const Imm& i = op.as<Imm>();
    int64_t val = i.value();

    if (Support::test(flags, FormatFlags::kHexImms) && uint64_t(val) > 9) {
      ASMJIT_PROPAGATE(sb.append("0x"));
      return sb.appendUInt(uint64_t(val), 16);
    }
    else {
      return sb.appendInt(val, 10);
    }
  }

  if (op.isLabel()) {
    return Formatter::formatLabel(sb, flags, emitter, op.id());
  }

  return sb.append("<None>");
}

// arm::FormatterInternal - Format Instruction
// ===========================================

ASMJIT_FAVOR_SIZE Error FormatterInternal::formatInstruction(
  String& sb,
  FormatFlags flags,
  const BaseEmitter* emitter,
  Arch arch,
  const BaseInst& inst, const Operand_* operands, size_t opCount) noexcept {

  InstId instId = inst.id();

  if (Environment::isArchAArch64(arch)) {
    // Format instruction options and instruction mnemonic.
    if (instId < a64::Inst::_kIdCount)
      ASMJIT_PROPAGATE(InstAPI::instIdToString(arch, instId, sb));
    else
      ASMJIT_PROPAGATE(sb.appendFormat("[InstId=#%u]", unsigned(instId)));

    if (inst.hasARMCondFlag()) {
      ASMJIT_PROPAGATE(sb.append('.'));
      ASMJIT_PROPAGATE(formatCondCode(sb, inst.armCondCode()));
    }
  }
  else {
    // TODO: [ARM] 32-bit ARM support.
  }

  for (uint32_t i = 0; i < opCount; i++) {
    const Operand_& op = operands[i];
    if (op.isNone())
      break;

    ASMJIT_PROPAGATE(sb.append(i == 0 ? " " : ", "));
    ASMJIT_PROPAGATE(formatOperand(sb, flags, emitter, arch, op));
  }

  return kErrorOk;
}

ASMJIT_END_SUB_NAMESPACE

#endif // !ASMJIT_NO_LOGGING
