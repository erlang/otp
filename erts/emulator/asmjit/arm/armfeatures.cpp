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
#if !defined(ASMJIT_NO_ARM) && ASMJIT_ARCH_ARM

#include "../core/cpuinfo.h"
#include "../core/support.h"
#include "../arm/armfeatures.h"

// Required by `getauxval()` on Linux.
#if defined(__linux__)
  #include <sys/auxv.h>
#endif

#if defined(__APPLE__)
  #include <mach/machine.h>
  #include <sys/errno.h>
  #include <sys/types.h>
  #include <sys/sysctl.h>
#endif

ASMJIT_BEGIN_SUB_NAMESPACE(arm)

// ============================================================================
// [asmjit::arm::Features - Detect - Base]
// ============================================================================

static inline void populateBaseFeatures(CpuInfo& cpu) noexcept {
  cpu.reset();
#if ASMJIT_ARCH_ARM == 32
  cpu._arch = Environment::kArchARM;
#else
  cpu._arch = Environment::kArchAArch64;

  // AArch64 is based on ARMv8 and later.
  cpu.addFeature(Features::kARMv6);
  cpu.addFeature(Features::kARMv7);
  cpu.addFeature(Features::kARMv8);

  // AArch64 comes with these features by default.
  cpu.addFeature(Features::kVFPv2);
  cpu.addFeature(Features::kVFPv3);
  cpu.addFeature(Features::kVFPv4);
  cpu.addFeature(Features::kASIMD);
  cpu.addFeature(Features::kIDIVA);
#endif
}

static void populateBaseFeaturesByVersion(CpuInfo& cpu) noexcept {
  Features& features = cpu._features.as<Features>();

  // Some information can be found here:
  //   https://developer.arm.com/architectures/learn-the-architecture/understanding-the-armv8-x-extensions/single-page
  //   https://en.wikipedia.org/wiki/AArch64

  if (features.hasARMv8_6()) {
    features.add(Features::kARMv8_5);
  }

  if (features.hasARMv8_5()) {
    features.add(Features::kARMv8_4);
  }

  if (features.hasARMv8_4()) {
    features.add(Features::kARMv8_3,
                 Features::kDOTPROD,
                 Features::kSHA3,
                 Features::kSHA512,
                 Features::kSM3,
                 Features::kSM4);
  }

  if (features.hasARMv8_3()) {
    features.add(Features::kARMv8_2,
                 Features::kFCMA,
                 Features::kFJCVTZS);
  }

  if (features.hasARMv8_2()) {
    features.add(Features::kARMv8_1);
  }

  if (features.hasARMv8_1()) {
    features.add(Features::kARMv8,
                 Features::kATOMICS,
                 Features::kCRC32,
                 Features::kRDMA);
  }

  if (features.hasARMv8()) {
    features.add(Features::kARMv7,
                 Features::kVFPv2,
                 Features::kVFPv3,
                 Features::kVFPv4,
                 Features::kVFP_D32,
                 Features::kASIMD,
                 Features::kIDIVA);
  }
}

// ============================================================================
// [asmjit::arm::Features - Detect - Windows]
// ============================================================================

#if defined(_WIN32)
struct WinPFPMapping {
  uint8_t featureId;
  uint8_t pfpFeatureId;
};

static void detectPFPFeatures(CpuInfo& cpu, const WinPFPMapping* mapping, size_t size) noexcept {
  for (size_t i = 0; i < size; i++)
    if (::IsProcessorFeaturePresent(mapping[i].pfpFeatureId))
      cpu.addFeature(mapping[i].featureId);
}

//! Detect ARM CPU features on Windows.
//!
//! The detection is based on `IsProcessorFeaturePresent()` API call.
ASMJIT_FAVOR_SIZE void detectCpu(CpuInfo& cpu) noexcept {
  populateBaseFeatures(cpu);

  Features& features = cpu._features.as<Features>();

  // Win32 for ARM requires ARMv7 with DSP extensions, VFPv3, and uses THUMBv2 by default.
#if ASMJIT_ARCH_ARM == 32
  features.add(Features::kARMv6);
  features.add(Features::kARMv7);
  features.add(Features::kEDSP);
  features.add(Features::kVFPv2);
  features.add(Features::kVFPv3);
  features.add(Features::kTHUMB);
  features.add(Features::kTHUMBv2);
#endif

  // Windows for ARM requires ASIMD.
  features.add(Features::kASIMD);

  // Detect additional CPU features by calling `IsProcessorFeaturePresent()`.
  static const WinPFPMapping mapping[] = {
#if ASMJIT_ARCH_ARM == 32
    { Features::kVFP_D32  , 18 }, // PF_ARM_VFP_32_REGISTERS_AVAILABLE
    { Features::kIDIVT    , 24 }, // PF_ARM_DIVIDE_INSTRUCTION_AVAILABLE
    { Features::kVFPv4    , 27 }, // PF_ARM_FMAC_INSTRUCTIONS_AVAILABLE
    { Features::kARMv8    , 29 }, // PF_ARM_V8_INSTRUCTIONS_AVAILABLE
#endif
    { Features::kAES      , 30 }, // PF_ARM_V8_CRYPTO_INSTRUCTIONS_AVAILABLE
    { Features::kCRC32    , 31 }, // PF_ARM_V8_CRC32_INSTRUCTIONS_AVAILABLE
    { Features::kATOMICS  , 34 }  // PF_ARM_V81_ATOMIC_INSTRUCTIONS_AVAILABLE

  };
  detectPFPFeatures(cpu, mapping, ASMJIT_ARRAY_SIZE(mapping));

  // Windows provides several instructions under a single flag, so expand:
  if (features.hasAES()) {
    features.add(Features::kSHA1, Features::kSHA2);
  }

  populateBaseFeaturesByVersion();
}
#endif

// ============================================================================
// [asmjit::arm::Features - Detect - Linux]
// ============================================================================

#if defined(__linux__)
struct LinuxHWCapMapping {
  uint8_t featureId;
  uint8_t hwCapBit;
};

static void detectHWCaps(CpuInfo& cpu, unsigned long type, const LinuxHWCapMapping* mapping, size_t size) noexcept {
  unsigned long mask = getauxval(type);
  for (size_t i = 0; i < size; i++)
    if (Support::bitTest(mask, mapping[i].hwCapBit))
      cpu.addFeature(mapping[i].featureId);
}

#if ASMJIT_ARCH_ARM == 32
// `AT_HWCAP` provides ARMv7 (and less) related flags.
static const LinuxHWCapMapping hwCapMapping[] = {
  { Features::kVFPv2       , 6  }, // HWCAP_VFP
  { Features::kEDSP        , 7  }, // HWCAP_EDSP
  { Features::kASIMD       , 12 }, // HWCAP_NEON
  { Features::kVFPv3       , 13 }, // HWCAP_VFPv3
  { Features::kVFPv4       , 16 }, // HWCAP_VFPv4
  { Features::kIDIVA       , 17 }, // HWCAP_IDIVA
  { Features::kIDIVT       , 18 }, // HWCAP_IDIVT
  { Features::kVFP_D32     , 19 }  // HWCAP_VFPD32
};

// `AT_HWCAP2` provides ARMv8+ related flags.
static const LinuxHWCapMapping hwCap2Mapping[] = {
  { Features::kAES         , 0  }, // HWCAP2_AES
  { Features::kPMULL       , 1  }, // HWCAP2_PMULL
  { Features::kSHA1        , 2  }, // HWCAP2_SHA1
  { Features::kSHA2        , 3  }, // HWCAP2_SHA2
  { Features::kCRC32       , 4  }  // HWCAP2_CRC32
};

static inline void detectCpuARM(CpuInfo& cpu) noexcept {
  Features& features = cpu._features.as<Features>();

  detectHWCaps(cpu, AT_HWCAP, hwCapMapping, ASMJIT_ARRAY_SIZE(hwCapMapping));
  detectHWCaps(cpu, AT_HWCAP2, hwCap2Mapping, ASMJIT_ARRAY_SIZE(hwCap2Mapping));

  // VFPv3 implies VFPv2.
  if (features.hasVFPv3())
    features.add(Features::kVFPv2);

  // VFPv2 implies ARMv6.
  if (features.hasVFPv2())
    features.add(Features::kARMv6);

  // VFPv3|ASIMD implies ARMv7.
  if (features.hasVFPv3() || features.hasASIMD())
    features.add(Features::kARMv7);

  if (features.hasAES() || features.hasCRC32() || features.hasPMULL() || features.hasSHA1() || features.hasSHA2())
    features.add(Features::kARMv8);
}
#endif

#if ASMJIT_ARCH_ARM == 64
// `AT_HWCAP` provides ARMv8+ related flags.
static const LinuxHWCapMapping hwCapMapping[] = {
  /*
  { Features::k            , 0  }, // HWCAP_FP
  */
  { Features::kASIMD       , 1  }, // HWCAP_ASIMD
  /*
  { Features::k            , 2  }, // HWCAP_EVTSTRM
  */
  { Features::kAES         , 3  }, // HWCAP_AES
  { Features::kPMULL       , 4  }, // HWCAP_PMULL
  { Features::kSHA1        , 5  }, // HWCAP_SHA1
  { Features::kSHA2        , 6  }, // HWCAP_SHA2
  { Features::kCRC32       , 7  }, // HWCAP_CRC32
  { Features::kATOMICS     , 8  }, // HWCAP_ATOMICS
  { Features::kFP16CONV    , 9  }, // HWCAP_FPHP
  { Features::kFP16FULL    , 10 }, // HWCAP_ASIMDHP
  { Features::kCPUID       , 11 }, // HWCAP_CPUID
  { Features::kRDMA        , 12 }, // HWCAP_ASIMDRDM
  { Features::kFJCVTZS     , 13 }, // HWCAP_JSCVT
  { Features::kFCMA        , 14 }, // HWCAP_FCMA
  /*
  { Features::k            , 15 }, // HWCAP_LRCPC
  { Features::k            , 16 }, // HWCAP_DCPOP
  */
  { Features::kSHA3        , 17 }, // HWCAP_SHA3
  { Features::kSM3         , 18 }, // HWCAP_SM3
  { Features::kSM4         , 19 }, // HWCAP_SM4
  { Features::kDOTPROD     , 20 }, // HWCAP_ASIMDDP
  { Features::kSHA512      , 21 }, // HWCAP_SHA512
  { Features::kSVE         , 22 }, // HWCAP_SVE
  { Features::kFP16FML     , 23 }, // HWCAP_ASIMDFHM
  /*
  { Features::k            , 24 }, // HWCAP_DIT
  { Features::k            , 25 }, // HWCAP_USCAT
  { Features::k            , 26 }, // HWCAP_ILRCPC
  */
  { Features::kFLAGM       , 27 }, // HWCAP_FLAGM
  { Features::kSSBS        , 28 }, // HWCAP_SSBS
  { Features::kSB          , 29 }  // HWCAP_SB
  /*
  { Features::k            , 30 }, // HWCAP_PACA
  { Features::k            , 31 }  // HWCAP_PACG
  */
};

// `AT_HWCAP2` provides ARMv8+ related flags.
static const LinuxHWCapMapping hwCapMapping2[] = {
  /*
  { Features::k            , 0  }, // HWCAP2_DCPODP
  */
  { Features::kSVE2        , 1  }, // HWCAP2_SVE2
  { Features::kSVE2_AES    , 2  }, // HWCAP2_SVEAES
  { Features::kSVE_PMULL   , 3  }, // HWCAP2_SVEPMULL
  { Features::kSVE2_BITPERM, 4  }, // HWCAP2_SVEBITPERM
  { Features::kSVE2_SHA3   , 5  }, // HWCAP2_SVESHA3
  { Features::kSVE2_SM4    , 6  }, // HWCAP2_SVESM4
  { Features::kFLAGM2      , 7  }, // HWCAP2_FLAGM2
  { Features::kFRINT       , 8  }, // HWCAP2_FRINT
  { Features::kSVE_I8MM    , 9  }, // HWCAP2_SVEI8MM
  { Features::kSVE_F32MM   , 10 }, // HWCAP2_SVEF32MM
  { Features::kSVE_F64MM   , 11 }, // HWCAP2_SVEF64MM
  { Features::kSVE_BF16    , 12 }, // HWCAP2_SVEBF16
  { Features::kI8MM        , 13 }, // HWCAP2_I8MM
  { Features::kFLAGM2      , 14 }, // HWCAP2_BF16
  { Features::kDGH         , 15 }  // HWCAP2_DGH
  /*
  { Features::k            , 16 }, // HWCAP2_RNG
  { Features::k            , 17 }, // HWCAP2_BTI
  */
};

static inline void detectCpuAArch64(CpuInfo& cpu) noexcept {
  detectHWCaps(cpu, AT_HWCAP, hwCapMapping, ASMJIT_ARRAY_SIZE(hwCapMapping));
  detectHWCaps(cpu, AT_HWCAP, hwCapMapping2, ASMJIT_ARRAY_SIZE(hwCapMapping2));
}
#endif

//! Detect ARM CPU features on Linux.
//!
//! The detection is based on `getauxval()`.
ASMJIT_FAVOR_SIZE void detectCpu(CpuInfo& cpu) noexcept {
  populateBaseFeatures(cpu);

#if ASMJIT_ARCH_ARM == 32
  detectCpuARM(cpu);
#else
  detectCpuAArch64(cpu);
#endif
}
#endif // __linux__

#if defined(__APPLE__)

ASMJIT_FAVOR_SIZE void detectCpu(CpuInfo& cpu) noexcept {

  // use sysctlbyname() to obtain CPU family; match on it as there is no good way to detect individual features
  uint32_t cpuFamily = 0;
  size_t l = sizeof(cpuFamily);
  int res;
  if ((res = sysctlbyname("hw.cpufamily", &cpuFamily, &l, NULL, 0)) != 0) {
    fprintf(stderr, "Error with sysctlbyname %d\n", errno);
    exit(1);
  }
  populateBaseFeatures(cpu);

  // TODO: so far, only A13/A14/M1 is reasonably complete.
  switch(cpuFamily) {
#if ASMJIT_ARCH_ARM == 32
    case CPUFAMILY_ARM_SWIFT:
    default:
      cpu.addFeature(Features::kARMv6);
      cpu.addFeature(Features::kARMv7);
      
      cpu.addFeature(Features::kVFPv2);
      cpu.addFeature(Features::kVFPv3);
      cpu.addFeature(Features::kVFPv4); //FeatureVFP4

      cpu.addFeature(Features::kASIMD); //FeatureNEONForFP

      // FeatureHasRetAddrStack,
      
      // FeatureUseWideStrideVFP,
      // FeatureMP,
      // FeatureHWDivThumb,
      // FeatureHWDivARM,
      // FeatureAvoidPartialCPSR,
      // FeatureAvoidMOVsShOp,
      // FeatureHasSlowFPVMLx,
      // FeatureHasSlowFPVFMx,
      // FeatureHasVMLxHazards,
      // FeatureProfUnpredicate,
      // FeaturePrefISHSTBarrier,
      // FeatureSlowOddRegister,
      // FeatureSlowLoadDSubreg,
      // FeatureSlowVGETLNi32,
      // FeatureSlowVDUP32,
      // FeatureUseMISched,
      // FeatureNoPostRASched
      break;

/*
  // AArch64 is based on ARMv8 and later.
  cpu.addFeature(Features::kARMv6);
  cpu.addFeature(Features::kARMv7);
  cpu.addFeature(Features::kARMv8);

  // AArch64 comes with these features by default.
  cpu.addFeature(Features::kVFPv2);
  cpu.addFeature(Features::kVFPv3);
  cpu.addFeature(Features::kVFPv4);
  cpu.addFeature(Features::kASIMD);
  cpu.addFeature(Features::kIDIVA);
*/

#elif ASMJIT_ARCH_ARM == 64
    case CPUFAMILY_ARM_FIRESTORM_ICESTORM: // A14 or M1
    case CPUFAMILY_ARM_LIGHTNING_THUNDER: // A13
      cpu.addFeature(Features::kARMv8_4); // HasV8_4aOps
      cpu.addFeature(Features::kFP16FML); // FeatureFP16FML
      cpu.addFeature(Features::kFP16FULL); // FeatureFullFP16
      cpu.addFeature(Features::kSHA3); // FeatureSHA3
      break;
      // Linux: midr=0x410f0000, revidr=0
      // Features : fp asimd evtstrm aes pmull sha1 sha2 crc32
      //            atomics fphp asimdhp cpuid asimdrdm jscvt
      //            fcma lrcpc dcpop sha3 asimddp sha512 asimdfhm
      //            dit uscat ilrcpc flagm ssbs
      //            sb paca pacg dcpodp flagm2 frint

    case CPUFAMILY_ARM_VORTEX_TEMPEST: // A12
      cpu.addFeature(Features::kFP16FULL); // FeatureFullFP16
      cpu.addFeature(Features::kARMv8_3); // HasV8_3aOps
      break;

    case CPUFAMILY_ARM_MONSOON_MISTRAL: // A11
      cpu.addFeature(Features::kFP16FULL); // FeatureFullFP16
      cpu.addFeature(Features::kARMv8_2);  // HasV8_2aOps
      break;

    case CPUFAMILY_ARM_HURRICANE: // A10
      cpu.addFeature(Features::kCRC32); // FeatureCRC
      cpu.addFeature(Features::kRDMA); // FeatureRDM
      // FeaturePAN
      // FeatureLOR
      // FeatureVH
      break;

    case CPUFAMILY_ARM_TWISTER: // A9
    case CPUFAMILY_ARM_TYPHOON: // A8
    case CPUFAMILY_ARM_CYCLONE: // A7
    default:
      // FeatureAlternateSExtLoadCVTF32Pattern
      // FeatureArithmeticBccFusion
      // FeatureArithmeticCbzFusion
      // FeatureCrypto
      cpu.addFeature(Features::kSHA1);
      cpu.addFeature(Features::kSHA2);
      cpu.addFeature(Features::kAES);
      // FeatureDisableLatencySchedHeuristic
      // FeatureFPARMv8 - included in AArch64
      // FeatureFuseAES
      // FeatureFuseCryptoEOR
      // FeatureNEON
      // FeaturePerfMon
      // FeatureZCRegMove
      // FeatureZCZeroing
      // FeatureZCZeroingFPWorkaround -- CYCLONE ONLY
      break;

#endif
  }
  populateBaseFeaturesByVersion(cpu);
}

#endif // __APPLE__

// ============================================================================
// [asmjit::arm::Features - Detect - Unknown]
// ============================================================================

#if !defined(_WIN32) && !defined(__linux__) && !defined(__APPLE__)
  #error "[asmjit] arm::detectCpu() - Unsupported OS."
#endif

ASMJIT_END_SUB_NAMESPACE

#endif // !ASMJIT_NO_ARM && ASMJIT_ARCH_ARM
