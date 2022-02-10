// This file is part of AsmJit project <https://asmjit.com>
//
// See asmjit.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#include "../core/api-build_p.h"
#ifndef ASMJIT_NO_LOGGING

#include "../core/cpuinfo.h"
#include "../core/misc_p.h"
#include "../core/support.h"
#include "../x86/x86formatter_p.h"
#include "../x86/x86instapi_p.h"
#include "../x86/x86instdb_p.h"
#include "../x86/x86operand.h"

#ifndef ASMJIT_NO_COMPILER
  #include "../core/compiler.h"
#endif

ASMJIT_BEGIN_SUB_NAMESPACE(x86)

// x86::FormatterInternal - Constants
// ==================================

struct RegFormatInfo {
  struct TypeEntry {
    uint8_t index;
  };

  struct NameEntry {
    uint8_t count;
    uint8_t formatIndex;
    uint8_t specialIndex;
    uint8_t specialCount;
  };

  TypeEntry typeEntries[uint32_t(RegType::kMaxValue) + 1];
  char typeStrings[128 - 32];

  NameEntry nameEntries[uint32_t(RegType::kMaxValue) + 1];
  char nameStrings[280];
};

template<uint32_t X>
struct RegFormatInfo_T {
  enum {
    kTypeIndex    = X == uint32_t(RegType::kX86_GpbLo) ? 1   :
                    X == uint32_t(RegType::kX86_GpbHi) ? 8   :
                    X == uint32_t(RegType::kX86_Gpw  ) ? 15  :
                    X == uint32_t(RegType::kX86_Gpd  ) ? 19  :
                    X == uint32_t(RegType::kX86_Gpq  ) ? 23  :
                    X == uint32_t(RegType::kX86_Xmm  ) ? 27  :
                    X == uint32_t(RegType::kX86_Ymm  ) ? 31  :
                    X == uint32_t(RegType::kX86_Zmm  ) ? 35  :
                    X == uint32_t(RegType::kX86_Mm   ) ? 50  :
                    X == uint32_t(RegType::kX86_KReg ) ? 53  :
                    X == uint32_t(RegType::kX86_SReg ) ? 43  :
                    X == uint32_t(RegType::kX86_CReg ) ? 59  :
                    X == uint32_t(RegType::kX86_DReg ) ? 62  :
                    X == uint32_t(RegType::kX86_St   ) ? 47  :
                    X == uint32_t(RegType::kX86_Bnd  ) ? 55  :
                    X == uint32_t(RegType::kX86_Tmm  ) ? 65  :
                    X == uint32_t(RegType::kX86_Rip  ) ? 39  : 0,

    kFormatIndex  = X == uint32_t(RegType::kX86_GpbLo) ? 1   :
                    X == uint32_t(RegType::kX86_GpbHi) ? 6   :
                    X == uint32_t(RegType::kX86_Gpw  ) ? 11  :
                    X == uint32_t(RegType::kX86_Gpd  ) ? 16  :
                    X == uint32_t(RegType::kX86_Gpq  ) ? 21  :
                    X == uint32_t(RegType::kX86_Xmm  ) ? 25  :
                    X == uint32_t(RegType::kX86_Ymm  ) ? 31  :
                    X == uint32_t(RegType::kX86_Zmm  ) ? 37  :
                    X == uint32_t(RegType::kX86_Mm   ) ? 60  :
                    X == uint32_t(RegType::kX86_KReg ) ? 65  :
                    X == uint32_t(RegType::kX86_SReg ) ? 49  :
                    X == uint32_t(RegType::kX86_CReg ) ? 75  :
                    X == uint32_t(RegType::kX86_DReg ) ? 80  :
                    X == uint32_t(RegType::kX86_St   ) ? 55  :
                    X == uint32_t(RegType::kX86_Bnd  ) ? 69  :
                    X == uint32_t(RegType::kX86_Tmm  ) ? 89  :
                    X == uint32_t(RegType::kX86_Rip  ) ? 43  : 0,

    kSpecialIndex = X == uint32_t(RegType::kX86_GpbLo) ? 96  :
                    X == uint32_t(RegType::kX86_GpbHi) ? 128 :
                    X == uint32_t(RegType::kX86_Gpw  ) ? 161 :
                    X == uint32_t(RegType::kX86_Gpd  ) ? 160 :
                    X == uint32_t(RegType::kX86_Gpq  ) ? 192 :
                    X == uint32_t(RegType::kX86_SReg ) ? 224 :
                    X == uint32_t(RegType::kX86_Rip  ) ? 85  : 0,

    kSpecialCount = X == uint32_t(RegType::kX86_GpbLo) ? 8   :
                    X == uint32_t(RegType::kX86_GpbHi) ? 4   :
                    X == uint32_t(RegType::kX86_Gpw  ) ? 8   :
                    X == uint32_t(RegType::kX86_Gpd  ) ? 8   :
                    X == uint32_t(RegType::kX86_Gpq  ) ? 8   :
                    X == uint32_t(RegType::kX86_SReg ) ? 7   :
                    X == uint32_t(RegType::kX86_Rip  ) ? 1   : 0
  };
};

#define ASMJIT_REG_TYPE_ENTRY(TYPE) {   \
  RegFormatInfo_T<TYPE>::kTypeIndex     \
}

#define ASMJIT_REG_NAME_ENTRY(TYPE) {   \
  RegTraits<RegType(TYPE)>::kCount,     \
  RegFormatInfo_T<TYPE>::kFormatIndex,  \
  RegFormatInfo_T<TYPE>::kSpecialIndex, \
  RegFormatInfo_T<TYPE>::kSpecialCount  \
}

static const RegFormatInfo x86RegFormatInfo = {
  // Register type entries and strings.
  { ASMJIT_LOOKUP_TABLE_32(ASMJIT_REG_TYPE_ENTRY, 0) },

  "\0"             // #0
  "gpb\0\0\0\0"    // #1
  "gpb.hi\0"       // #8
  "gpw\0"          // #15
  "gpd\0"          // #19
  "gpq\0"          // #23
  "xmm\0"          // #27
  "ymm\0"          // #31
  "zmm\0"          // #35
  "rip\0"          // #39
  "seg\0"          // #43
  "st\0"           // #47
  "mm\0"           // #50
  "k\0"            // #53
  "bnd\0"          // #55
  "cr\0"           // #59
  "dr\0"           // #62
  "tmm\0"          // #65
  ,

  // Register name entries and strings.
  { ASMJIT_LOOKUP_TABLE_32(ASMJIT_REG_NAME_ENTRY, 0) },

  "\0"
  "r%ub\0"         // #1
  "r%uh\0"         // #6
  "r%uw\0"         // #11
  "r%ud\0"         // #16
  "r%u\0"          // #21
  "xmm%u\0"        // #25
  "ymm%u\0"        // #31
  "zmm%u\0"        // #37
  "rip%u\0"        // #43
  "seg%u\0"        // #49
  "st%u\0"         // #55
  "mm%u\0"         // #60
  "k%u\0"          // #65
  "bnd%u\0"        // #69
  "cr%u\0"         // #75
  "dr%u\0"         // #80

  "rip\0"          // #85
  "tmm%u\0"        // #89
  "\0"             // #95

  "al\0\0" "cl\0\0" "dl\0\0" "bl\0\0" "spl\0"  "bpl\0"  "sil\0"  "dil\0" // #96
  "ah\0\0" "ch\0\0" "dh\0\0" "bh\0\0" "n/a\0"  "n/a\0"  "n/a\0"  "n/a\0" // #128
  "eax\0"  "ecx\0"  "edx\0"  "ebx\0"  "esp\0"  "ebp\0"  "esi\0"  "edi\0" // #160
  "rax\0"  "rcx\0"  "rdx\0"  "rbx\0"  "rsp\0"  "rbp\0"  "rsi\0"  "rdi\0" // #192
  "n/a\0"  "es\0\0" "cs\0\0" "ss\0\0" "ds\0\0" "fs\0\0" "gs\0\0" "n/a\0" // #224
};
#undef ASMJIT_REG_NAME_ENTRY
#undef ASMJIT_REG_TYPE_ENTRY

static const char* x86GetAddressSizeString(uint32_t size) noexcept {
  switch (size) {
    case 1 : return "byte ptr ";
    case 2 : return "word ptr ";
    case 4 : return "dword ptr ";
    case 6 : return "fword ptr ";
    case 8 : return "qword ptr ";
    case 10: return "tbyte ptr ";
    case 16: return "xmmword ptr ";
    case 32: return "ymmword ptr ";
    case 64: return "zmmword ptr ";
    default: return "";
  }
}

// x86::FormatterInternal - Format FeatureId
// =========================================

Error FormatterInternal::formatFeature(String& sb, uint32_t featureId) noexcept {
  // @EnumStringBegin{"enum": "CpuFeatures::X86", "output": "sFeature", "strip": "k"}@
  static const char sFeatureString[] =
    "None\0"
    "MT\0"
    "NX\0"
    "3DNOW\0"
    "3DNOW2\0"
    "ADX\0"
    "AESNI\0"
    "ALTMOVCR8\0"
    "AMX_BF16\0"
    "AMX_INT8\0"
    "AMX_TILE\0"
    "AVX\0"
    "AVX2\0"
    "AVX512_4FMAPS\0"
    "AVX512_4VNNIW\0"
    "AVX512_BF16\0"
    "AVX512_BITALG\0"
    "AVX512_BW\0"
    "AVX512_CDI\0"
    "AVX512_DQ\0"
    "AVX512_ERI\0"
    "AVX512_F\0"
    "AVX512_FP16\0"
    "AVX512_IFMA\0"
    "AVX512_PFI\0"
    "AVX512_VBMI\0"
    "AVX512_VBMI2\0"
    "AVX512_VL\0"
    "AVX512_VNNI\0"
    "AVX512_VP2INTERSECT\0"
    "AVX512_VPOPCNTDQ\0"
    "AVX_VNNI\0"
    "BMI\0"
    "BMI2\0"
    "CET_IBT\0"
    "CET_SS\0"
    "CLDEMOTE\0"
    "CLFLUSH\0"
    "CLFLUSHOPT\0"
    "CLWB\0"
    "CLZERO\0"
    "CMOV\0"
    "CMPXCHG16B\0"
    "CMPXCHG8B\0"
    "ENCLV\0"
    "ENQCMD\0"
    "ERMS\0"
    "F16C\0"
    "FMA\0"
    "FMA4\0"
    "FPU\0"
    "FSGSBASE\0"
    "FXSR\0"
    "FXSROPT\0"
    "GEODE\0"
    "GFNI\0"
    "HLE\0"
    "HRESET\0"
    "I486\0"
    "LAHFSAHF\0"
    "LWP\0"
    "LZCNT\0"
    "MCOMMIT\0"
    "MMX\0"
    "MMX2\0"
    "MONITOR\0"
    "MONITORX\0"
    "MOVBE\0"
    "MOVDIR64B\0"
    "MOVDIRI\0"
    "MPX\0"
    "MSR\0"
    "MSSE\0"
    "OSXSAVE\0"
    "OSPKE\0"
    "PCLMULQDQ\0"
    "PCONFIG\0"
    "POPCNT\0"
    "PREFETCHW\0"
    "PREFETCHWT1\0"
    "PTWRITE\0"
    "RDPID\0"
    "RDPRU\0"
    "RDRAND\0"
    "RDSEED\0"
    "RDTSC\0"
    "RDTSCP\0"
    "RTM\0"
    "SERIALIZE\0"
    "SHA\0"
    "SKINIT\0"
    "SMAP\0"
    "SMEP\0"
    "SMX\0"
    "SNP\0"
    "SSE\0"
    "SSE2\0"
    "SSE3\0"
    "SSE4_1\0"
    "SSE4_2\0"
    "SSE4A\0"
    "SSSE3\0"
    "SVM\0"
    "TBM\0"
    "TSX\0"
    "TSXLDTRK\0"
    "UINTR\0"
    "VAES\0"
    "VMX\0"
    "VPCLMULQDQ\0"
    "WAITPKG\0"
    "WBNOINVD\0"
    "XOP\0"
    "XSAVE\0"
    "XSAVEC\0"
    "XSAVEOPT\0"
    "XSAVES\0"
    "<Unknown>\0";

  static const uint16_t sFeatureIndex[] = {
    0, 5, 8, 11, 17, 24, 28, 34, 44, 53, 62, 71, 75, 80, 94, 108, 120, 134, 144,
    155, 165, 176, 185, 197, 209, 220, 232, 245, 255, 267, 287, 304, 313, 317,
    322, 330, 337, 346, 354, 365, 370, 377, 382, 393, 403, 409, 416, 421, 426,
    430, 435, 439, 448, 453, 461, 467, 472, 476, 483, 488, 497, 501, 507, 515,
    519, 524, 532, 541, 547, 557, 565, 569, 573, 578, 586, 592, 602, 610, 617,
    627, 639, 647, 653, 659, 666, 673, 679, 686, 690, 700, 704, 711, 716, 721,
    725, 729, 733, 738, 743, 750, 757, 763, 769, 773, 777, 781, 790, 796, 801,
    805, 816, 824, 833, 837, 843, 850, 859, 866
  };
  // @EnumStringEnd@

  return sb.append(sFeatureString + sFeatureIndex[Support::min<uint32_t>(featureId, uint32_t(CpuFeatures::X86::kMaxValue) + 1)]);
}

// x86::FormatterInternal - Format Register
// ========================================

ASMJIT_FAVOR_SIZE Error FormatterInternal::formatRegister(String& sb, FormatFlags formatFlags, const BaseEmitter* emitter, Arch arch, RegType type, uint32_t id) noexcept {
  DebugUtils::unused(arch);
  const RegFormatInfo& info = x86RegFormatInfo;

#ifndef ASMJIT_NO_COMPILER
  if (Operand::isVirtId(id)) {
    if (emitter && emitter->emitterType() == EmitterType::kCompiler) {
      const BaseCompiler* cc = static_cast<const BaseCompiler*>(emitter);
      if (cc->isVirtIdValid(id)) {
        VirtReg* vReg = cc->virtRegById(id);
        ASMJIT_ASSERT(vReg != nullptr);

        const char* name = vReg->name();
        if (name && name[0] != '\0')
          ASMJIT_PROPAGATE(sb.append(name));
        else
          ASMJIT_PROPAGATE(sb.appendFormat("%%%u", unsigned(Operand::virtIdToIndex(id))));

        if (vReg->type() != type && uint32_t(type) <= uint32_t(RegType::kMaxValue) && Support::test(formatFlags, FormatFlags::kRegCasts)) {
          const RegFormatInfo::TypeEntry& typeEntry = info.typeEntries[size_t(type)];
          if (typeEntry.index)
            ASMJIT_PROPAGATE(sb.appendFormat("@%s", info.typeStrings + typeEntry.index));
        }

        return kErrorOk;
      }
    }
  }
#else
  DebugUtils::unused(emitter, formatFlags);
#endif

  if (uint32_t(type) <= uint32_t(RegType::kMaxValue)) {
    const RegFormatInfo::NameEntry& nameEntry = info.nameEntries[size_t(type)];

    if (id < nameEntry.specialCount)
      return sb.append(info.nameStrings + nameEntry.specialIndex + id * 4);

    if (id < nameEntry.count)
      return sb.appendFormat(info.nameStrings + nameEntry.formatIndex, unsigned(id));

    const RegFormatInfo::TypeEntry& typeEntry = info.typeEntries[size_t(type)];
    if (typeEntry.index)
      return sb.appendFormat("%s@%u", info.typeStrings + typeEntry.index, id);
  }

  return sb.appendFormat("<Reg-%u>?%u", uint32_t(type), id);
}

// x86::FormatterInternal - Format Operand
// =======================================

ASMJIT_FAVOR_SIZE Error FormatterInternal::formatOperand(
  String& sb,
  FormatFlags formatFlags,
  const BaseEmitter* emitter,
  Arch arch,
  const Operand_& op) noexcept {

  if (op.isReg())
    return formatRegister(sb, formatFlags, emitter, arch, op.as<BaseReg>().type(), op.as<BaseReg>().id());

  if (op.isMem()) {
    const Mem& m = op.as<Mem>();
    ASMJIT_PROPAGATE(sb.append(x86GetAddressSizeString(m.size())));

    // Segment override prefix.
    uint32_t seg = m.segmentId();
    if (seg != SReg::kIdNone && seg < SReg::kIdCount)
      ASMJIT_PROPAGATE(sb.appendFormat("%s:", x86RegFormatInfo.nameStrings + 224 + size_t(seg) * 4));

    ASMJIT_PROPAGATE(sb.append('['));
    switch (m.addrType()) {
      case Mem::AddrType::kDefault:
        break;
      case Mem::AddrType::kAbs:
        ASMJIT_PROPAGATE(sb.append("abs "));
        break;
      case Mem::AddrType::kRel:
        ASMJIT_PROPAGATE(sb.append("rel "));
        break;
    }

    char opSign = '\0';
    if (m.hasBase()) {
      opSign = '+';
      if (m.hasBaseLabel()) {
        ASMJIT_PROPAGATE(Formatter::formatLabel(sb, formatFlags, emitter, m.baseId()));
      }
      else {
        FormatFlags modifiedFlags = formatFlags;
        if (m.isRegHome()) {
          ASMJIT_PROPAGATE(sb.append("&"));
          modifiedFlags &= ~FormatFlags::kRegCasts;
        }
        ASMJIT_PROPAGATE(formatRegister(sb, modifiedFlags, emitter, arch, m.baseType(), m.baseId()));
      }
    }

    if (m.hasIndex()) {
      if (opSign)
        ASMJIT_PROPAGATE(sb.append(opSign));

      opSign = '+';
      ASMJIT_PROPAGATE(formatRegister(sb, formatFlags, emitter, arch, m.indexType(), m.indexId()));
      if (m.hasShift())
        ASMJIT_PROPAGATE(sb.appendFormat("*%u", 1 << m.shift()));
    }

    uint64_t off = uint64_t(m.offset());
    if (off || !m.hasBaseOrIndex()) {
      if (int64_t(off) < 0) {
        opSign = '-';
        off = ~off + 1;
      }

      if (opSign)
        ASMJIT_PROPAGATE(sb.append(opSign));

      uint32_t base = 10;
      if (Support::test(formatFlags, FormatFlags::kHexOffsets) && off > 9) {
        ASMJIT_PROPAGATE(sb.append("0x", 2));
        base = 16;
      }

      ASMJIT_PROPAGATE(sb.appendUInt(off, base));
    }

    return sb.append(']');
  }

  if (op.isImm()) {
    const Imm& i = op.as<Imm>();
    int64_t val = i.value();

    if (Support::test(formatFlags, FormatFlags::kHexImms) && uint64_t(val) > 9) {
      ASMJIT_PROPAGATE(sb.append("0x", 2));
      return sb.appendUInt(uint64_t(val), 16);
    }
    else {
      return sb.appendInt(val, 10);
    }
  }

  if (op.isLabel()) {
    return Formatter::formatLabel(sb, formatFlags, emitter, op.id());
  }

  return sb.append("<None>");
}

// x86::FormatterInternal - Format Immediate (Extension)
// =====================================================

static constexpr char kImmCharStart = '{';
static constexpr char kImmCharEnd   = '}';
static constexpr char kImmCharOr    = '|';

struct ImmBits {
  enum Mode : uint32_t {
    kModeLookup = 0,
    kModeFormat = 1
  };

  uint8_t mask;
  uint8_t shift;
  uint8_t mode;
  char text[48 - 3];
};

ASMJIT_FAVOR_SIZE static Error FormatterInternal_formatImmShuf(String& sb, uint32_t u8, uint32_t bits, uint32_t count) noexcept {
  uint32_t mask = (1 << bits) - 1;

  for (uint32_t i = 0; i < count; i++, u8 >>= bits) {
    uint32_t value = u8 & mask;
    ASMJIT_PROPAGATE(sb.append(i == 0 ? kImmCharStart : kImmCharOr));
    ASMJIT_PROPAGATE(sb.appendUInt(value));
  }

  if (kImmCharEnd)
    ASMJIT_PROPAGATE(sb.append(kImmCharEnd));

  return kErrorOk;
}

ASMJIT_FAVOR_SIZE static Error FormatterInternal_formatImmBits(String& sb, uint32_t u8, const ImmBits* bits, uint32_t count) noexcept {
  uint32_t n = 0;
  char buf[64];

  for (uint32_t i = 0; i < count; i++) {
    const ImmBits& spec = bits[i];

    uint32_t value = (u8 & uint32_t(spec.mask)) >> spec.shift;
    const char* str = nullptr;

    switch (spec.mode) {
      case ImmBits::kModeLookup:
        str = Support::findPackedString(spec.text, value);
        break;

      case ImmBits::kModeFormat:
        snprintf(buf, sizeof(buf), spec.text, unsigned(value));
        str = buf;
        break;

      default:
        return DebugUtils::errored(kErrorInvalidState);
    }

    if (!str[0])
      continue;

    ASMJIT_PROPAGATE(sb.append(++n == 1 ? kImmCharStart : kImmCharOr));
    ASMJIT_PROPAGATE(sb.append(str));
  }

  if (n && kImmCharEnd)
    ASMJIT_PROPAGATE(sb.append(kImmCharEnd));

  return kErrorOk;
}

ASMJIT_FAVOR_SIZE static Error FormatterInternal_formatImmText(String& sb, uint32_t u8, uint32_t bits, uint32_t advance, const char* text, uint32_t count = 1) noexcept {
  uint32_t mask = (1u << bits) - 1;
  uint32_t pos = 0;

  for (uint32_t i = 0; i < count; i++, u8 >>= bits, pos += advance) {
    uint32_t value = (u8 & mask) + pos;
    ASMJIT_PROPAGATE(sb.append(i == 0 ? kImmCharStart : kImmCharOr));
    ASMJIT_PROPAGATE(sb.append(Support::findPackedString(text, value)));
  }

  if (kImmCharEnd)
    ASMJIT_PROPAGATE(sb.append(kImmCharEnd));

  return kErrorOk;
}

ASMJIT_FAVOR_SIZE static Error FormatterInternal_explainConst(
  String& sb,
  FormatFlags formatFlags,
  InstId instId,
  uint32_t vecSize,
  const Imm& imm) noexcept {

  DebugUtils::unused(formatFlags);

  static const char vcmpx[] =
    "EQ_OQ\0" "LT_OS\0"  "LE_OS\0"  "UNORD_Q\0"  "NEQ_UQ\0" "NLT_US\0" "NLE_US\0" "ORD_Q\0"
    "EQ_UQ\0" "NGE_US\0" "NGT_US\0" "FALSE_OQ\0" "NEQ_OQ\0" "GE_OS\0"  "GT_OS\0"  "TRUE_UQ\0"
    "EQ_OS\0" "LT_OQ\0"  "LE_OQ\0"  "UNORD_S\0"  "NEQ_US\0" "NLT_UQ\0" "NLE_UQ\0" "ORD_S\0"
    "EQ_US\0" "NGE_UQ\0" "NGT_UQ\0" "FALSE_OS\0" "NEQ_OS\0" "GE_OQ\0"  "GT_OQ\0"  "TRUE_US\0";

  // Why to make it compatible...
  static const char vpcmpx[] = "EQ\0" "LT\0" "LE\0" "FALSE\0" "NEQ\0" "GE\0"  "GT\0"    "TRUE\0";
  static const char vpcomx[] = "LT\0" "LE\0" "GT\0" "GE\0"    "EQ\0"  "NEQ\0" "FALSE\0" "TRUE\0";

  static const char vshufpd[] = "A0\0A1\0B0\0B1\0A2\0A3\0B2\0B3\0A4\0A5\0B4\0B5\0A6\0A7\0B6\0B7\0";
  static const char vshufps[] = "A0\0A1\0A2\0A3\0A0\0A1\0A2\0A3\0B0\0B1\0B2\0B3\0B0\0B1\0B2\0B3\0";

  static const ImmBits vfpclassxx[] = {
    { 0x07u, 0, ImmBits::kModeLookup, "QNAN\0" "+0\0" "-0\0" "+INF\0" "-INF\0" "DENORMAL\0" "-FINITE\0" "SNAN\0" }
  };

  static const ImmBits vfixupimmxx[] = {
    { 0x01u, 0, ImmBits::kModeLookup, "\0" "+INF_IE\0" },
    { 0x02u, 1, ImmBits::kModeLookup, "\0" "-VE_IE\0"  },
    { 0x04u, 2, ImmBits::kModeLookup, "\0" "-INF_IE\0" },
    { 0x08u, 3, ImmBits::kModeLookup, "\0" "SNAN_IE\0" },
    { 0x10u, 4, ImmBits::kModeLookup, "\0" "ONE_IE\0"  },
    { 0x20u, 5, ImmBits::kModeLookup, "\0" "ONE_ZE\0"  },
    { 0x40u, 6, ImmBits::kModeLookup, "\0" "ZERO_IE\0" },
    { 0x80u, 7, ImmBits::kModeLookup, "\0" "ZERO_ZE\0" }
  };

  static const ImmBits vgetmantxx[] = {
    { 0x03u, 0, ImmBits::kModeLookup, "[1, 2)\0" "[.5, 2)\0" "[.5, 1)\0" "[.75, 1.5)\0" },
    { 0x04u, 2, ImmBits::kModeLookup, "\0" "NO_SIGN\0" },
    { 0x08u, 3, ImmBits::kModeLookup, "\0" "QNAN_IF_SIGN\0" }
  };

  static const ImmBits vmpsadbw[] = {
    { 0x04u, 2, ImmBits::kModeLookup, "BLK1[0]\0" "BLK1[1]\0" },
    { 0x03u, 0, ImmBits::kModeLookup, "BLK2[0]\0" "BLK2[1]\0" "BLK2[2]\0" "BLK2[3]\0" },
    { 0x40u, 6, ImmBits::kModeLookup, "BLK1[4]\0" "BLK1[5]\0" },
    { 0x30u, 4, ImmBits::kModeLookup, "BLK2[4]\0" "BLK2[5]\0" "BLK2[6]\0" "BLK2[7]\0" }
  };

  static const ImmBits vpclmulqdq[] = {
    { 0x01u, 0, ImmBits::kModeLookup, "LQ\0" "HQ\0" },
    { 0x10u, 4, ImmBits::kModeLookup, "LQ\0" "HQ\0" }
  };

  static const ImmBits vperm2x128[] = {
    { 0x0Bu, 0, ImmBits::kModeLookup, "A0\0" "A1\0" "B0\0" "B1\0" "\0" "\0" "\0" "\0" "0\0" "0\0" "0\0" "0\0" },
    { 0xB0u, 4, ImmBits::kModeLookup, "A0\0" "A1\0" "B0\0" "B1\0" "\0" "\0" "\0" "\0" "0\0" "0\0" "0\0" "0\0" }
  };

  static const ImmBits vrangexx[] = {
    { 0x03u, 0, ImmBits::kModeLookup, "MIN\0" "MAX\0" "MIN_ABS\0" "MAX_ABS\0" },
    { 0x0Cu, 2, ImmBits::kModeLookup, "SIGN_A\0" "SIGN_B\0" "SIGN_0\0" "SIGN_1\0" }
  };

  static const ImmBits vreducexx_vrndscalexx[] = {
    { 0x07u, 0, ImmBits::kModeLookup, "\0" "\0" "\0" "\0" "ROUND\0" "FLOOR\0" "CEIL\0" "TRUNC\0" },
    { 0x08u, 3, ImmBits::kModeLookup, "\0" "SAE\0" },
    { 0xF0u, 4, ImmBits::kModeFormat, "LEN=%d" }
  };

  static const ImmBits vroundxx[] = {
    { 0x07u, 0, ImmBits::kModeLookup, "ROUND\0" "FLOOR\0" "CEIL\0" "TRUNC\0" "\0" "\0" "\0" "\0" },
    { 0x08u, 3, ImmBits::kModeLookup, "\0" "INEXACT\0" }
  };

  uint32_t u8 = imm.valueAs<uint8_t>();
  switch (instId) {
    case Inst::kIdVblendpd:
    case Inst::kIdBlendpd:
      return FormatterInternal_formatImmShuf(sb, u8, 1, vecSize / 8);

    case Inst::kIdVblendps:
    case Inst::kIdBlendps:
      return FormatterInternal_formatImmShuf(sb, u8, 1, vecSize / 4);

    case Inst::kIdVcmppd:
    case Inst::kIdVcmpps:
    case Inst::kIdVcmpsd:
    case Inst::kIdVcmpss:
      return FormatterInternal_formatImmText(sb, u8, 5, 0, vcmpx);

    case Inst::kIdCmppd:
    case Inst::kIdCmpps:
    case Inst::kIdCmpsd:
    case Inst::kIdCmpss:
      return FormatterInternal_formatImmText(sb, u8, 3, 0, vcmpx);

    case Inst::kIdVdbpsadbw:
      return FormatterInternal_formatImmShuf(sb, u8, 2, 4);

    case Inst::kIdVdppd:
    case Inst::kIdVdpps:
    case Inst::kIdDppd:
    case Inst::kIdDpps:
      return FormatterInternal_formatImmShuf(sb, u8, 1, 8);

    case Inst::kIdVmpsadbw:
    case Inst::kIdMpsadbw:
      return FormatterInternal_formatImmBits(sb, u8, vmpsadbw, Support::min<uint32_t>(vecSize / 8, 4));

    case Inst::kIdVpblendw:
    case Inst::kIdPblendw:
      return FormatterInternal_formatImmShuf(sb, u8, 1, 8);

    case Inst::kIdVpblendd:
      return FormatterInternal_formatImmShuf(sb, u8, 1, Support::min<uint32_t>(vecSize / 4, 8));

    case Inst::kIdVpclmulqdq:
    case Inst::kIdPclmulqdq:
      return FormatterInternal_formatImmBits(sb, u8, vpclmulqdq, ASMJIT_ARRAY_SIZE(vpclmulqdq));

    case Inst::kIdVroundpd:
    case Inst::kIdVroundps:
    case Inst::kIdVroundsd:
    case Inst::kIdVroundss:
    case Inst::kIdRoundpd:
    case Inst::kIdRoundps:
    case Inst::kIdRoundsd:
    case Inst::kIdRoundss:
      return FormatterInternal_formatImmBits(sb, u8, vroundxx, ASMJIT_ARRAY_SIZE(vroundxx));

    case Inst::kIdVshufpd:
    case Inst::kIdShufpd:
      return FormatterInternal_formatImmText(sb, u8, 1, 2, vshufpd, Support::min<uint32_t>(vecSize / 8, 8));

    case Inst::kIdVshufps:
    case Inst::kIdShufps:
      return FormatterInternal_formatImmText(sb, u8, 2, 4, vshufps, 4);

    case Inst::kIdVcvtps2ph:
      return FormatterInternal_formatImmBits(sb, u8, vroundxx, 1);

    case Inst::kIdVperm2f128:
    case Inst::kIdVperm2i128:
      return FormatterInternal_formatImmBits(sb, u8, vperm2x128, ASMJIT_ARRAY_SIZE(vperm2x128));

    case Inst::kIdVpermilpd:
      return FormatterInternal_formatImmShuf(sb, u8, 1, vecSize / 8);

    case Inst::kIdVpermilps:
      return FormatterInternal_formatImmShuf(sb, u8, 2, 4);

    case Inst::kIdVpshufd:
    case Inst::kIdPshufd:
      return FormatterInternal_formatImmShuf(sb, u8, 2, 4);

    case Inst::kIdVpshufhw:
    case Inst::kIdVpshuflw:
    case Inst::kIdPshufhw:
    case Inst::kIdPshuflw:
    case Inst::kIdPshufw:
      return FormatterInternal_formatImmShuf(sb, u8, 2, 4);

    case Inst::kIdVfixupimmpd:
    case Inst::kIdVfixupimmps:
    case Inst::kIdVfixupimmsd:
    case Inst::kIdVfixupimmss:
      return FormatterInternal_formatImmBits(sb, u8, vfixupimmxx, ASMJIT_ARRAY_SIZE(vfixupimmxx));

    case Inst::kIdVfpclasspd:
    case Inst::kIdVfpclassps:
    case Inst::kIdVfpclasssd:
    case Inst::kIdVfpclassss:
      return FormatterInternal_formatImmBits(sb, u8, vfpclassxx, ASMJIT_ARRAY_SIZE(vfpclassxx));

    case Inst::kIdVgetmantpd:
    case Inst::kIdVgetmantps:
    case Inst::kIdVgetmantsd:
    case Inst::kIdVgetmantss:
      return FormatterInternal_formatImmBits(sb, u8, vgetmantxx, ASMJIT_ARRAY_SIZE(vgetmantxx));

    case Inst::kIdVpcmpb:
    case Inst::kIdVpcmpd:
    case Inst::kIdVpcmpq:
    case Inst::kIdVpcmpw:
    case Inst::kIdVpcmpub:
    case Inst::kIdVpcmpud:
    case Inst::kIdVpcmpuq:
    case Inst::kIdVpcmpuw:
      return FormatterInternal_formatImmText(sb, u8, 3, 0, vpcmpx);

    case Inst::kIdVpcomb:
    case Inst::kIdVpcomd:
    case Inst::kIdVpcomq:
    case Inst::kIdVpcomw:
    case Inst::kIdVpcomub:
    case Inst::kIdVpcomud:
    case Inst::kIdVpcomuq:
    case Inst::kIdVpcomuw:
      return FormatterInternal_formatImmText(sb, u8, 3, 0, vpcomx);

    case Inst::kIdVpermq:
    case Inst::kIdVpermpd:
      return FormatterInternal_formatImmShuf(sb, u8, 2, 4);

    case Inst::kIdVpternlogd:
    case Inst::kIdVpternlogq:
      return FormatterInternal_formatImmShuf(sb, u8, 1, 8);

    case Inst::kIdVrangepd:
    case Inst::kIdVrangeps:
    case Inst::kIdVrangesd:
    case Inst::kIdVrangess:
      return FormatterInternal_formatImmBits(sb, u8, vrangexx, ASMJIT_ARRAY_SIZE(vrangexx));

    case Inst::kIdVreducepd:
    case Inst::kIdVreduceps:
    case Inst::kIdVreducesd:
    case Inst::kIdVreducess:
    case Inst::kIdVrndscalepd:
    case Inst::kIdVrndscaleps:
    case Inst::kIdVrndscalesd:
    case Inst::kIdVrndscaless:
      return FormatterInternal_formatImmBits(sb, u8, vreducexx_vrndscalexx, ASMJIT_ARRAY_SIZE(vreducexx_vrndscalexx));

    case Inst::kIdVshuff32x4:
    case Inst::kIdVshuff64x2:
    case Inst::kIdVshufi32x4:
    case Inst::kIdVshufi64x2: {
      uint32_t count = Support::max<uint32_t>(vecSize / 16, 2u);
      uint32_t bits = count <= 2 ? 1u : 2u;
      return FormatterInternal_formatImmShuf(sb, u8, bits, count);
    }

    default:
      return kErrorOk;
  }
}

// x86::FormatterInternal - Format Instruction
// ===========================================

ASMJIT_FAVOR_SIZE Error FormatterInternal::formatInstruction(
  String& sb,
  FormatFlags formatFlags,
  const BaseEmitter* emitter,
  Arch arch,
  const BaseInst& inst, const Operand_* operands, size_t opCount) noexcept {

  InstId instId = inst.id();
  InstOptions options = inst.options();

  // Format instruction options and instruction mnemonic.
  if (instId < Inst::_kIdCount) {
    // VEX|EVEX options.
    if (Support::test(options, InstOptions::kX86_Vex))
      ASMJIT_PROPAGATE(sb.append("{vex} "));

    if (Support::test(options, InstOptions::kX86_Vex3))
      ASMJIT_PROPAGATE(sb.append("{vex3} "));

    if (Support::test(options, InstOptions::kX86_Evex))
      ASMJIT_PROPAGATE(sb.append("{evex} "));

    // MOD/RM and MOD/MR options
    if (Support::test(options, InstOptions::kX86_ModRM))
      ASMJIT_PROPAGATE(sb.append("{modrm} "));
    else if (Support::test(options, InstOptions::kX86_ModMR))
      ASMJIT_PROPAGATE(sb.append("{modmr} "));

    // SHORT|LONG options.
    if (Support::test(options, InstOptions::kShortForm))
      ASMJIT_PROPAGATE(sb.append("short "));

    if (Support::test(options, InstOptions::kLongForm))
      ASMJIT_PROPAGATE(sb.append("long "));

    // LOCK|XACQUIRE|XRELEASE options.
    if (Support::test(options, InstOptions::kX86_XAcquire))
      ASMJIT_PROPAGATE(sb.append("xacquire "));

    if (Support::test(options, InstOptions::kX86_XRelease))
      ASMJIT_PROPAGATE(sb.append("xrelease "));

    if (Support::test(options, InstOptions::kX86_Lock))
      ASMJIT_PROPAGATE(sb.append("lock "));

    // REP|REPNE options.
    if (Support::test(options, InstOptions::kX86_Rep | InstOptions::kX86_Repne)) {
      sb.append(Support::test(options, InstOptions::kX86_Rep) ? "rep " : "repnz ");
      if (inst.hasExtraReg()) {
        ASMJIT_PROPAGATE(sb.append("{"));
        ASMJIT_PROPAGATE(formatOperand(sb, formatFlags, emitter, arch, inst.extraReg().toReg<BaseReg>()));
        ASMJIT_PROPAGATE(sb.append("} "));
      }
    }

    // REX options.
    if (Support::test(options, InstOptions::kX86_Rex)) {
      const InstOptions kRXBWMask = InstOptions::kX86_OpCodeR |
                                    InstOptions::kX86_OpCodeX |
                                    InstOptions::kX86_OpCodeB |
                                    InstOptions::kX86_OpCodeW ;
      if (Support::test(options, kRXBWMask)) {
        ASMJIT_PROPAGATE(sb.append("rex."));
        if (Support::test(options, InstOptions::kX86_OpCodeR)) sb.append('r');
        if (Support::test(options, InstOptions::kX86_OpCodeX)) sb.append('x');
        if (Support::test(options, InstOptions::kX86_OpCodeB)) sb.append('b');
        if (Support::test(options, InstOptions::kX86_OpCodeW)) sb.append('w');
        sb.append(' ');
      }
      else {
        ASMJIT_PROPAGATE(sb.append("rex "));
      }
    }

    ASMJIT_PROPAGATE(InstInternal::instIdToString(arch, instId, sb));
  }
  else {
    ASMJIT_PROPAGATE(sb.appendFormat("[InstId=#%u]", unsigned(instId)));
  }

  for (uint32_t i = 0; i < opCount; i++) {
    const Operand_& op = operands[i];
    if (op.isNone()) break;

    ASMJIT_PROPAGATE(sb.append(i == 0 ? " " : ", "));
    ASMJIT_PROPAGATE(formatOperand(sb, formatFlags, emitter, arch, op));

    if (op.isImm() && uint32_t(formatFlags & FormatFlags::kExplainImms)) {
      uint32_t vecSize = 16;
      for (uint32_t j = 0; j < opCount; j++)
        if (operands[j].isReg())
          vecSize = Support::max<uint32_t>(vecSize, operands[j].size());
      ASMJIT_PROPAGATE(FormatterInternal_explainConst(sb, formatFlags, instId, vecSize, op.as<Imm>()));
    }

    // Support AVX-512 masking - {k}{z}.
    if (i == 0) {
      if (inst.extraReg().group() == RegGroup::kX86_K) {
        ASMJIT_PROPAGATE(sb.append(" {"));
        ASMJIT_PROPAGATE(formatRegister(sb, formatFlags, emitter, arch, inst.extraReg().type(), inst.extraReg().id()));
        ASMJIT_PROPAGATE(sb.append('}'));

        if (Support::test(options, InstOptions::kX86_ZMask))
          ASMJIT_PROPAGATE(sb.append("{z}"));
      }
      else if (Support::test(options, InstOptions::kX86_ZMask)) {
        ASMJIT_PROPAGATE(sb.append(" {z}"));
      }
    }

    // Support AVX-512 broadcast - {1tox}.
    if (op.isMem() && op.as<Mem>().hasBroadcast()) {
      ASMJIT_PROPAGATE(sb.appendFormat(" {1to%u}", Support::bitMask(uint32_t(op.as<Mem>().getBroadcast()))));
    }
  }

  // Support AVX-512 embedded rounding and suppress-all-exceptions {sae}.
  if (inst.hasOption(InstOptions::kX86_ER | InstOptions::kX86_SAE)) {
    if (inst.hasOption(InstOptions::kX86_ER)) {
      uint32_t bits = uint32_t(inst.options() & InstOptions::kX86_ERMask) >> Support::ConstCTZ<uint32_t(InstOptions::kX86_ERMask)>::value;

      const char roundingModes[] = "rn\0rd\0ru\0rz";
      ASMJIT_PROPAGATE(sb.appendFormat(", {%s-sae}", roundingModes + bits * 3));
    }
    else {
      ASMJIT_PROPAGATE(sb.append(", {sae}"));
    }
  }

  return kErrorOk;
}

ASMJIT_END_SUB_NAMESPACE

#endif // !ASMJIT_NO_LOGGING
