// This file is part of AsmJit project <https://asmjit.com>
//
// See asmjit.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#include "../core/api-build_p.h"
#ifndef ASMJIT_NO_LOGGING

#include "../core/misc_p.h"
#include "../core/support.h"
#include "../arm/armformatter_p.h"
#include "../arm/a64operand.h"
#include "../arm/a64instapi_p.h"
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
    "ARMv6\0"
    "ARMv7\0"
    "ARMv8a\0"
    "THUMB\0"
    "THUMBv2\0"
    "ABLE\0"
    "ADERR\0"
    "AES\0"
    "AFP\0"
    "AIE\0"
    "AMU1\0"
    "AMU1_1\0"
    "ANERR\0"
    "ASIMD\0"
    "BF16\0"
    "BRBE\0"
    "BTI\0"
    "BWE\0"
    "CCIDX\0"
    "CHK\0"
    "CLRBHB\0"
    "CMOW\0"
    "CONSTPACFIELD\0"
    "CPA\0"
    "CPA2\0"
    "CPUID\0"
    "CRC32\0"
    "CSSC\0"
    "CSV2\0"
    "CSV2_3\0"
    "CSV3\0"
    "D128\0"
    "DGH\0"
    "DIT\0"
    "DOTPROD\0"
    "DPB\0"
    "DPB2\0"
    "EBEP\0"
    "EBF16\0"
    "ECBHB\0"
    "ECV\0"
    "EDHSR\0"
    "EDSP\0"
    "FAMINMAX\0"
    "FCMA\0"
    "FGT\0"
    "FGT2\0"
    "FHM\0"
    "FLAGM\0"
    "FLAGM2\0"
    "FMAC\0"
    "FP\0"
    "FP16\0"
    "FP16CONV\0"
    "FP8\0"
    "FP8DOT2\0"
    "FP8DOT4\0"
    "FP8FMA\0"
    "FPMR\0"
    "FRINTTS\0"
    "GCS\0"
    "HACDBS\0"
    "HAFDBS\0"
    "HAFT\0"
    "HDBSS\0"
    "HBC\0"
    "HCX\0"
    "HPDS\0"
    "HPDS2\0"
    "I8MM\0"
    "IDIVA\0"
    "IDIVT\0"
    "ITE\0"
    "JSCVT\0"
    "LOR\0"
    "LRCPC\0"
    "LRCPC2\0"
    "LRCPC3\0"
    "LS64\0"
    "LS64_ACCDATA\0"
    "LS64_V\0"
    "LSE\0"
    "LSE128\0"
    "LSE2\0"
    "LUT\0"
    "LVA\0"
    "LVA3\0"
    "MEC\0"
    "MOPS\0"
    "MPAM\0"
    "MTE\0"
    "MTE2\0"
    "MTE3\0"
    "MTE4\0"
    "MTE_ASYM_FAULT\0"
    "MTE_ASYNC\0"
    "MTE_CANONICAL_TAGS\0"
    "MTE_NO_ADDRESS_TAGS\0"
    "MTE_PERM_S1\0"
    "MTE_STORE_ONLY\0"
    "MTE_TAGGED_FAR\0"
    "MTPMU\0"
    "NMI\0"
    "NV\0"
    "NV2\0"
    "PAN\0"
    "PAN2\0"
    "PAN3\0"
    "PAUTH\0"
    "PFAR\0"
    "PMU\0"
    "PMULL\0"
    "PRFMSLC\0"
    "RAS\0"
    "RAS1_1\0"
    "RAS2\0"
    "RASSA2\0"
    "RDM\0"
    "RME\0"
    "RNG\0"
    "RNG_TRAP\0"
    "RPRES\0"
    "RPRFM\0"
    "S1PIE\0"
    "S1POE\0"
    "S2PIE\0"
    "S2POE\0"
    "SB\0"
    "SCTLR2\0"
    "SEBEP\0"
    "SEL2\0"
    "SHA1\0"
    "SHA256\0"
    "SHA3\0"
    "SHA512\0"
    "SM3\0"
    "SM4\0"
    "SME\0"
    "SME2\0"
    "SME2_1\0"
    "SME_B16B16\0"
    "SME_B16F32\0"
    "SME_BI32I32\0"
    "SME_F16F16\0"
    "SME_F16F32\0"
    "SME_F32F32\0"
    "SME_F64F64\0"
    "SME_F8F16\0"
    "SME_F8F32\0"
    "SME_FA64\0"
    "SME_I16I32\0"
    "SME_I16I64\0"
    "SME_I8I32\0"
    "SME_LUTv2\0"
    "SPE\0"
    "SPE1_1\0"
    "SPE1_2\0"
    "SPE1_3\0"
    "SPE1_4\0"
    "SPE_ALTCLK\0"
    "SPE_CRR\0"
    "SPE_EFT\0"
    "SPE_FDS\0"
    "SPE_FPF\0"
    "SPE_SME\0"
    "SPECRES\0"
    "SPECRES2\0"
    "SPMU\0"
    "SSBS\0"
    "SSBS2\0"
    "SSVE_FP8DOT2\0"
    "SSVE_FP8DOT4\0"
    "SSVE_FP8FMA\0"
    "SVE\0"
    "SVE2\0"
    "SVE2_1\0"
    "SVE_AES\0"
    "SVE_B16B16\0"
    "SVE_BF16\0"
    "SVE_BITPERM\0"
    "SVE_EBF16\0"
    "SVE_F32MM\0"
    "SVE_F64MM\0"
    "SVE_I8MM\0"
    "SVE_PMULL128\0"
    "SVE_SHA3\0"
    "SVE_SM4\0"
    "SYSINSTR128\0"
    "SYSREG128\0"
    "THE\0"
    "TLBIOS\0"
    "TLBIRANGE\0"
    "TLBIW\0"
    "TME\0"
    "TRF\0"
    "UAO\0"
    "VFP_D32\0"
    "VHE\0"
    "VMID16\0"
    "WFXT\0"
    "XNX\0"
    "XS\0"
    "<Unknown>\0";

  static const uint16_t sFeatureIndex[] = {
    0, 5, 11, 17, 24, 30, 38, 43, 49, 53, 57, 61, 66, 73, 79, 85, 90, 95, 99,
    103, 109, 113, 120, 125, 139, 143, 148, 154, 160, 165, 170, 177, 182, 187,
    191, 195, 203, 207, 212, 217, 223, 229, 233, 239, 244, 253, 258, 262, 267,
    271, 277, 284, 289, 292, 297, 306, 310, 318, 326, 333, 338, 346, 350, 357,
    364, 369, 375, 379, 383, 388, 394, 399, 405, 411, 415, 421, 425, 431, 438,
    445, 450, 463, 470, 474, 481, 486, 490, 494, 499, 503, 508, 513, 517, 522,
    527, 532, 547, 557, 576, 596, 608, 623, 638, 644, 648, 651, 655, 659, 664,
    669, 675, 680, 684, 690, 698, 702, 709, 714, 721, 725, 729, 733, 742, 748,
    754, 760, 766, 772, 778, 781, 788, 794, 799, 804, 811, 816, 823, 827, 831,
    835, 840, 847, 858, 869, 881, 892, 903, 914, 925, 935, 945, 954, 965, 976,
    986, 996, 1000, 1007, 1014, 1021, 1028, 1039, 1047, 1055, 1063, 1071, 1079,
    1087, 1096, 1101, 1106, 1112, 1125, 1138, 1150, 1154, 1159, 1166, 1174, 1185,
    1194, 1206, 1216, 1226, 1236, 1245, 1258, 1267, 1275, 1287, 1297, 1301, 1308,
    1318, 1324, 1328, 1332, 1336, 1344, 1348, 1355, 1360, 1364, 1367
  };
  // @EnumStringEnd@

  return sb.append(sFeatureString + sFeatureIndex[Support::min<uint32_t>(featureId, uint32_t(CpuFeatures::ARM::kMaxValue) + 1)]);
}

// arm::FormatterInternal - Format Constants
// =========================================

ASMJIT_FAVOR_SIZE Error FormatterInternal::formatCondCode(String& sb, CondCode cc) noexcept {
  static const char condCodeData[] =
    "al\0" "na\0"
    "eq\0" "ne\0"
    "hs\0" "lo\0" "mi\0" "pl\0" "vs\0" "vc\0"
    "hi\0" "ls\0" "ge\0" "lt\0" "gt\0" "le\0"
    "<Unknown>";
  return sb.append(condCodeData + Support::min<uint32_t>(uint32_t(cc), 16u) * 3);
}

ASMJIT_FAVOR_SIZE Error FormatterInternal::formatShiftOp(String& sb, ShiftOp shiftOp) noexcept {
  const char* str = nullptr;
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
    default: str = "<Unknown>"; break;
  }
  return sb.append(str);
}

// arm::FormatterInternal - Format Register
// ========================================

struct FormatElementData {
  char letter;
  uint8_t elementCount;
  uint8_t onlyIndex;
  uint8_t reserved;
};

static constexpr FormatElementData formatElementDataTable[9] = {
  { '?' , 0 , 0, 0 }, // None
  { 'b' , 16, 0, 0 }, // bX or b[index]
  { 'h' , 8 , 0, 0 }, // hX or h[index]
  { 's' , 4 , 0, 0 }, // sX or s[index]
  { 'd' , 2 , 0, 0 }, // dX or d[index]
  { 'b' , 4 , 1, 0 }, // ?? or b4[index]
  { 'h' , 2 , 1, 0 }, // ?? or h2[index]
  { '?' , 0 , 0, 0 }, // invalid (possibly stored in Operand)
  { '?' , 0 , 0, 0 }  // invalid (never stored in Operand, bug...)
};

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
      case RegType::kARM_VecB:
      case RegType::kARM_VecH:
      case RegType::kARM_VecS:
      case RegType::kARM_VecD:
      case RegType::kARM_VecV:
        letter = bhsdq[uint32_t(regType) - uint32_t(RegType::kARM_VecB)];
        if (elementType)
          letter = 'v';
        break;

      case RegType::kARM_GpW:
        if (Environment::is64Bit(arch)) {
          letter = 'w';

          if (rId == a64::Gp::kIdZr)
            return sb.append("wzr", 3);

          if (rId == a64::Gp::kIdSp)
            return sb.append("wsp", 3);
        }
        else {
          letter = 'r';
        }
        break;

      case RegType::kARM_GpX:
        if (Environment::is64Bit(arch)) {
          if (rId == a64::Gp::kIdZr)
            return sb.append("xzr", 3);
          if (rId == a64::Gp::kIdSp)
            return sb.append("sp", 2);

          letter = 'x';
          break;
        }

        // X registers are undefined in 32-bit mode.
        ASMJIT_FALLTHROUGH;

      default:
        ASMJIT_PROPAGATE(sb.appendFormat("<Reg-%u>?%u", uint32_t(regType), rId));
        break;
    }

    if (letter)
      ASMJIT_PROPAGATE(sb.appendFormat("%c%u", letter, rId));
  }

  constexpr uint32_t kElementTypeCount = uint32_t(a64::VecElementType::kMaxValue) + 1;
  if (elementType) {
    elementType = Support::min(elementType, kElementTypeCount);

    FormatElementData elementData = formatElementDataTable[elementType];
    uint32_t elementCount = elementData.elementCount;

    if (regType == RegType::kARM_VecD) {
      elementCount /= 2u;
    }

    ASMJIT_PROPAGATE(sb.append('.'));
    if (elementCount) {
      ASMJIT_PROPAGATE(sb.appendUInt(elementCount));
    }
    ASMJIT_PROPAGATE(sb.append(elementData.letter));
  }

  if (elementIndex != 0xFFFFFFFFu) {
    ASMJIT_PROPAGATE(sb.appendFormat("[%u]", elementIndex));
  }

  return kErrorOk;
}

ASMJIT_FAVOR_SIZE Error FormatterInternal::formatRegisterList(
  String& sb,
  FormatFlags flags,
  const BaseEmitter* emitter,
  Arch arch,
  RegType regType,
  uint32_t rMask) noexcept {

  bool first = true;

  ASMJIT_PROPAGATE(sb.append('{'));
  while (rMask != 0u) {
    uint32_t start = Support::ctz(rMask);
    uint32_t count = 0u;

    uint32_t mask = 1u << start;
    do {
      rMask &= ~mask;
      mask <<= 1u;
      count++;
    } while (rMask & mask);

    if (!first)
      ASMJIT_PROPAGATE(sb.append(", "));

    ASMJIT_PROPAGATE(formatRegister(sb, flags, emitter, arch, regType, start, 0, 0xFFFFFFFFu));
    if (count >= 2u) {
      ASMJIT_PROPAGATE(sb.append('-'));
      ASMJIT_PROPAGATE(formatRegister(sb, flags, emitter, arch, regType, start + count - 1, 0, 0xFFFFFFFFu));
    }

    first = false;
  }
  ASMJIT_PROPAGATE(sb.append('}'));

  return kErrorOk;
}

// a64::FormatterInternal - Format Operand
// =======================================

ASMJIT_FAVOR_SIZE Error FormatterInternal::formatOperand(
  String& sb,
  FormatFlags flags,
  const BaseEmitter* emitter,
  Arch arch,
  const Operand_& op) noexcept {

  if (op.isReg()) {
    const BaseReg& reg = op.as<BaseReg>();

    uint32_t elementType = op._signature.getField<BaseVec::kSignatureRegElementTypeMask>();
    uint32_t elementIndex = op.as<BaseVec>().elementIndex();

    if (!op.as<BaseVec>().hasElementIndex())
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
        ASMJIT_PROPAGATE(formatShiftOp(sb, m.shiftOp()));
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
    uint32_t predicate = i.predicate();

    if (predicate) {
      ASMJIT_PROPAGATE(formatShiftOp(sb, ShiftOp(predicate)));
      ASMJIT_PROPAGATE(sb.append(' '));
    }

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

  if (op.isRegList()) {
    const BaseRegList& regList = op.as<BaseRegList>();
    return formatRegisterList(sb, flags, emitter, arch, regList.type(), regList.list());
  }

  return sb.append("<None>");
}

ASMJIT_END_SUB_NAMESPACE

#endif // !ASMJIT_NO_LOGGING
