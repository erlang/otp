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

ASMJIT_FAVOR_SIZE Error FormatterInternal::formatCondCode(String& sb, CondCode cc) noexcept {
  static const char condCodeData[] =
    "al\0" "na\0"
    "eq\0" "ne\0"
    "cs\0" "cc\0" "mi\0" "pl\0" "vs\0" "vc\0"
    "hi\0" "ls\0" "ge\0" "lt\0" "gt\0" "le\0"
    "<Unknown>";
  return sb.append(condCodeData + Support::min<uint32_t>(uint32_t(cc), 16u) * 3);
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

ASMJIT_END_SUB_NAMESPACE

#endif // !ASMJIT_NO_LOGGING
