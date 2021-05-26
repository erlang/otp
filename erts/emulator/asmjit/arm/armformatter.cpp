// AsmJit - Machine code generation for C++
//
//  * Official AsmJit Home Page: https://asmjit.com
//  * Official Github Repository: https://github.com/asmjit/asmjit
//
// Copyright (c) 2008-2020 The AsmJit Authors
//
// This software is provided 'as-is', without any express or implied
// warranty. In no event will the authors be held liable for any damages
// arising from the use of this software.
//
// Permission is granted to anyone to use this software for any purpose,
// including commercial applications, and to alter it and redistribute it
// freely, subject to the following restrictions:
//
// 1. The origin of this software must not be misrepresented; you must not
//    claim that you wrote the original software. If you use this software
//    in a product, an acknowledgment in the product documentation would be
//    appreciated but is not required.
// 2. Altered source versions must be plainly marked as such, and must not be
//    misrepresented as being the original software.
// 3. This notice may not be removed or altered from any source distribution.

#include "../core/api-build_p.h"
#ifndef ASMJIT_NO_LOGGING

#include "../core/misc_p.h"
#include "../core/support.h"
#include "../arm/armfeatures.h"
#include "../arm/armformatter_p.h"
#include "../arm/armoperand.h"
#include "../arm/a64instdb_p.h"

#ifndef ASMJIT_NO_COMPILER
  #include "../core/compiler.h"
#endif

ASMJIT_BEGIN_SUB_NAMESPACE(arm)

// ============================================================================
// [asmjit::arm::FormatterInternal - Format Feature]
// ============================================================================

Error FormatterInternal::formatFeature(String& sb, uint32_t featureId) noexcept {
  // @EnumStringBegin{"enum": "arm::Features::Id", "output": "sFeature", "strip": "k"}@
  static const char sFeatureString[] =
    "None\0"
    "THUMB\0"
    "THUMBv2\0"
    "ARMv6\0"
    "ARMv7\0"
    "ARMv8\0"
    "ARMv8_1\0"
    "ARMv8_2\0"
    "ARMv8_3\0"
    "ARMv8_4\0"
    "ARMv8_5\0"
    "ARMv8_6\0"
    "VFPv2\0"
    "VFPv3\0"
    "VFPv4\0"
    "VFP_D32\0"
    "AES\0"
    "ASIMD\0"
    "ATOMICS\0"
    "BF16\0"
    "CPUID\0"
    "CRC32\0"
    "DGH\0"
    "DOTPROD\0"
    "EDSP\0"
    "FCMA\0"
    "FJCVTZS\0"
    "FLAGM\0"
    "FLAGM2\0"
    "FP16CONV\0"
    "FP16FML\0"
    "FP16FULL\0"
    "FRINT\0"
    "I8MM\0"
    "IDIVA\0"
    "IDIVT\0"
    "MTE\0"
    "RDMA\0"
    "PMULL\0"
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
    "<Unknown>\0";

  static const uint16_t sFeatureIndex[] = {
    0, 5, 11, 19, 25, 31, 37, 45, 53, 61, 69, 77, 85, 91, 97, 103, 111, 115,
    121, 129, 134, 140, 146, 150, 158, 163, 168, 176, 182, 189, 198, 206, 215,
    221, 226, 232, 238, 242, 247, 253, 256, 261, 266, 271, 278, 282, 286, 291,
    295, 304, 314, 324, 333, 343, 348, 357, 370, 380, 389
  };
  // @EnumStringEnd@

  return sb.append(sFeatureString + sFeatureIndex[Support::min<uint32_t>(featureId, arm::Features::kCount)]);
}

// ============================================================================
// [asmjit::arm::FormatterInternal - Format Constants]
// ============================================================================

ASMJIT_FAVOR_SIZE Error FormatterInternal::formatCondCode(String& sb, uint32_t condCode) noexcept {
  static const char condCodeData[] =
    "eq\0" "ne\0" "cs\0" "cc\0" "mi\0" "pl\0" "vs\0" "vc\0"
    "hi\0" "ls\0" "ge\0" "lt\0" "gt\0" "le\0" "al\0" "na\0"
    "<Unknown>";
  return sb.append(condCodeData + Support::min<uint32_t>(condCode, 16u) * 3);
}

ASMJIT_FAVOR_SIZE Error FormatterInternal::formatShiftOp(String& sb, uint32_t shiftOp) noexcept {
  const char* str = "<Unknown>";
  switch (shiftOp) {
    case Shift::kOpLSL: str = "lsl"; break;
    case Shift::kOpLSR: str = "lsr"; break;
    case Shift::kOpASR: str = "asr"; break;
    case Shift::kOpROR: str = "ror"; break;
    case Shift::kOpRRX: str = "rrx"; break;
    case Shift::kOpUXTB: str = "uxtb"; break;
    case Shift::kOpUXTH: str = "uxth"; break;
    case Shift::kOpUXTW: str = "uxtw"; break;
    case Shift::kOpUXTX: str = "uxtx"; break;
    case Shift::kOpSXTB: str = "sxtb"; break;
    case Shift::kOpSXTH: str = "sxth"; break;
    case Shift::kOpSXTW: str = "sxtw"; break;
    case Shift::kOpSXTX: str = "sxtx"; break;
  }
  return sb.append(str);
}

// ============================================================================
// [asmjit::arm::FormatterInternal - Format Register]
// ============================================================================

ASMJIT_FAVOR_SIZE Error FormatterInternal::formatRegister(
  String& sb, uint32_t flags,
  const BaseEmitter* emitter,
  uint32_t arch,
  uint32_t rType,
  uint32_t rId,
  uint32_t elementType,
  uint32_t elementIndex) noexcept {

  static const char bhsdq[] = "bhsdq";

  bool virtRegFormatted = false;

#ifndef ASMJIT_NO_COMPILER
  if (Operand::isVirtId(rId)) {
    if (emitter && emitter->emitterType() == BaseEmitter::kTypeCompiler) {
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
    switch (rType) {
      case Reg::kTypeGpW:
        if (rId == Gp::kIdZr)
          return sb.append("wzr");
        if (rId == Gp::kIdSp)
          return sb.append("wsp");

        letter = 'w';
        break;

      case Reg::kTypeGpX:
        if (rId == Gp::kIdZr)
          return sb.append("xzr");
        if (rId == Gp::kIdSp)
          return sb.append("sp");

        letter = 'x';
        break;

      case Reg::kTypeVecB:
      case Reg::kTypeVecH:
      case Reg::kTypeVecS:
      case Reg::kTypeVecD:
      case Reg::kTypeVecV:
        letter = bhsdq[rType - Reg::kTypeVecB];
        if (elementType)
          letter = 'v';
        break;

      default:
        ASMJIT_PROPAGATE(sb.appendFormat("<Reg-%u>?$u", rType, rId));
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
        if (rType == Reg::kTypeVecD)
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

// ============================================================================
// [asmjit::arm::FormatterInternal - Format Operand]
// ============================================================================

ASMJIT_FAVOR_SIZE Error FormatterInternal::formatOperand(
  String& sb,
  uint32_t flags,
  const BaseEmitter* emitter,
  uint32_t arch,
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
        uint32_t modifiedFlags = flags;
        if (m.isRegHome()) {
          ASMJIT_PROPAGATE(sb.append('&'));
          modifiedFlags &= ~FormatOptions::kFlagRegCasts;
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

      if ((flags & FormatOptions::kFlagHexOffsets) != 0 && uint64_t(off) > 9)
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
        ASMJIT_PROPAGATE(formatShiftOp(sb, m.predicate()));
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

    if ((flags & FormatOptions::kFlagHexImms) != 0 && uint64_t(val) > 9) {
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

// ============================================================================
// [asmjit::arm::FormatterInternal - Format Instruction]
// ============================================================================

ASMJIT_FAVOR_SIZE Error FormatterInternal::formatInstruction(
  String& sb,
  uint32_t flags,
  const BaseEmitter* emitter,
  uint32_t arch,
  const BaseInst& inst, const Operand_* operands, size_t opCount) noexcept {

  uint32_t instId = inst.id();

  if (Environment::isArchAArch64(arch)) {
    // Format instruction options and instruction mnemonic.
    if (instId < a64::Inst::_kIdCount)
      ASMJIT_PROPAGATE(InstAPI::instIdToString(arch, instId, sb));
    else
      ASMJIT_PROPAGATE(sb.appendFormat("[InstId=#%u]", unsigned(instId)));

    if (inst.hasOption(a64::Inst::kOptionCondFlagMask)) {
      ASMJIT_PROPAGATE(sb.append('.'));
      ASMJIT_PROPAGATE(formatCondCode(sb, inst.options() >> a64::Inst::kOptionCondCodeShift));
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
