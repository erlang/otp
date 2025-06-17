// This file is part of AsmJit project <https://asmjit.com>
//
// See asmjit.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#include "../core/api-build_p.h"
#include "../core/cpuinfo.h"
#include "../core/support.h"

#include <atomic>

// Required by `__cpuidex()` and `_xgetbv()`.
#if ASMJIT_ARCH_X86
  #if defined(_MSC_VER)
    #include <intrin.h>
  #endif
#endif // ASMJIT_ARCH_X86

#if ASMJIT_ARCH_ARM
  // Required by various utilities that are required by features detection.
  #if !defined(_WIN32)
    #include <errno.h>
    #include <sys/utsname.h>
  #endif

  //! Required to detect CPU and features on Apple platforms.
  #if defined(__APPLE__)
    #include <mach/machine.h>
    #include <sys/types.h>
    #include <sys/sysctl.h>
  #endif

  #if (defined(__linux__) || defined(__FreeBSD__))
    // Required by `getauxval()` on Linux and FreeBSD.
    #include <sys/auxv.h>
    #define ASMJIT_ARM_DETECT_VIA_HWCAPS
  #endif

  #if ASMJIT_ARCH_ARM >= 64 && defined(__GNUC__) && defined(__linux__) && 0
    // This feature is disabled at the moment - it works, but it seems linux supports ARM features
    // via HWCAPS pretty well and the most recent features need to access more registers that were
    // not originally accessible, which would break on some systems.
    #define ASMJIT_ARM_DETECT_VIA_CPUID
  #endif

  #if ASMJIT_ARCH_ARM >= 64 && defined(__OpenBSD__)
    #include <sys/sysctl.h>
    #include <machine/cpu.h>
  #endif
#endif // ASMJIT_ARCH_ARM

#if !defined(_WIN32) && (ASMJIT_ARCH_X86 || ASMJIT_ARCH_ARM)
  #include <unistd.h>
#endif

// Unfortunately when compiling in C++11 mode MSVC would warn about unused functions as
// [[maybe_unused]] attribute is not used in that case (it's used only by C++17 mode and later).
#if defined(_MSC_VER)
  #pragma warning(push)
  #pragma warning(disable: 4505) // unreferenced local function has been removed.
#endif // _MSC_VER

ASMJIT_BEGIN_NAMESPACE

// CpuInfo - Detect - Compatibility
// ================================

// CPU features detection is a minefield on non-X86 platforms. The following list describes which
// operating systems and architectures are supported and the status of the implementation:
//
//   * X86, X86_64:
//     - All OSes supported
//     - Detection is based on using a CPUID instruction, which is a user-space instruction, so there
//       is no need to use any OS specific APIs or syscalls to detect all features provided by the CPU.
//
//   * ARM32:
//     - Linux   - HWCAPS based detection.
//     - FreeBSD - HWCAPS based detection (shared with Linux code).
//     - NetBSD  - NOT IMPLEMENTED!
//     - OpenBSD - NOT IMPLEMENTED!
//     - Apple   - sysctlbyname() based detection (this architecture is deprecated on Apple HW).
//     - Windows - IsProcessorFeaturePresent() based detection (only detects a subset of features).
//     - Others  - NOT IMPLEMENTED!
//
//   * ARM64:
//     - Linux   - HWCAPS and CPUID based detection.
//     - FreeBSD - HWCAPS and CPUID based detection (shared with Linux code).
//     - NetBSD  - NOT IMPLEMENTED!
//     - OpenBSD - CPUID based detection (reading CPUID via sysctl's CTL_MACHDEP).
//     - Apple   - sysctlbyname() based detection with FamilyId matrix (record for each family id).
//     - Windows - IsProcessorFeaturePresent() based detection (only detects a subset of features).
//     - Others  - NOT IMPLEMENTED!
//
//   * Others
//     - NOT IMPLEMENTED!

// CpuInfo - Detect - HW-Thread Count
// ==================================

#if defined(_WIN32)
static inline uint32_t detectHWThreadCount() noexcept {
  SYSTEM_INFO info;
  ::GetSystemInfo(&info);
  return info.dwNumberOfProcessors;
}
#elif defined(_SC_NPROCESSORS_ONLN)
static inline uint32_t detectHWThreadCount() noexcept {
  long res = ::sysconf(_SC_NPROCESSORS_ONLN);
  return res <= 0 ? uint32_t(1) : uint32_t(res);
}
#else
static inline uint32_t detectHWThreadCount() noexcept {
  return 1;
}
#endif

// CpuInfo - Detect - X86
// ======================

// X86 and X86_64 detection is based on CPUID.

#if ASMJIT_ARCH_X86

namespace x86 {

typedef CpuFeatures::X86 Ext;

struct cpuid_t { uint32_t eax, ebx, ecx, edx; };
struct xgetbv_t { uint32_t eax, edx; };

// Executes `cpuid` instruction.
static inline void cpuidQuery(cpuid_t* out, uint32_t inEax, uint32_t inEcx = 0) noexcept {
#if defined(_MSC_VER)
  __cpuidex(reinterpret_cast<int*>(out), inEax, inEcx);
#elif defined(__GNUC__) && ASMJIT_ARCH_X86 == 32
  __asm__ __volatile__(
    "mov %%ebx, %%edi\n"
    "cpuid\n"
    "xchg %%edi, %%ebx\n" : "=a"(out->eax), "=D"(out->ebx), "=c"(out->ecx), "=d"(out->edx) : "a"(inEax), "c"(inEcx));
#elif defined(__GNUC__) && ASMJIT_ARCH_X86 == 64
  __asm__ __volatile__(
    "mov %%rbx, %%rdi\n"
    "cpuid\n"
    "xchg %%rdi, %%rbx\n" : "=a"(out->eax), "=D"(out->ebx), "=c"(out->ecx), "=d"(out->edx) : "a"(inEax), "c"(inEcx));
#else
  #error "[asmjit] x86::cpuidQuery() - Unsupported compiler."
#endif
}

// Executes 'xgetbv' instruction.
static inline void xgetbvQuery(xgetbv_t* out, uint32_t inEcx) noexcept {
#if defined(_MSC_VER)
  uint64_t value = _xgetbv(inEcx);
  out->eax = uint32_t(value & 0xFFFFFFFFu);
  out->edx = uint32_t(value >> 32);
#elif defined(__GNUC__)
  uint32_t outEax;
  uint32_t outEdx;

  // Replaced, because the world is not perfect:
  //   __asm__ __volatile__("xgetbv" : "=a"(outEax), "=d"(outEdx) : "c"(inEcx));
  __asm__ __volatile__(".byte 0x0F, 0x01, 0xD0" : "=a"(outEax), "=d"(outEdx) : "c"(inEcx));

  out->eax = outEax;
  out->edx = outEdx;
#else
  out->eax = 0;
  out->edx = 0;
#endif
}

// Map a 12-byte vendor string returned by `cpuid` into a `CpuInfo::Vendor` ID.
static inline void simplifyCpuVendor(CpuInfo& cpu, uint32_t d0, uint32_t d1, uint32_t d2) noexcept {
  struct Vendor {
    char normalized[8];
    union { char text[12]; uint32_t d[3]; };
  };

  static const Vendor table[] = {
    { { 'A', 'M', 'D'                     }, {{ 'A', 'u', 't', 'h', 'e', 'n', 't', 'i', 'c', 'A', 'M', 'D' }} },
    { { 'I', 'N', 'T', 'E', 'L'           }, {{ 'G', 'e', 'n', 'u', 'i', 'n', 'e', 'I', 'n', 't', 'e', 'l' }} },
    { { 'V', 'I', 'A'                     }, {{ 'C', 'e', 'n', 't', 'a', 'u', 'r', 'H', 'a', 'u', 'l', 's' }} },
    { { 'V', 'I', 'A'                     }, {{ 'V', 'I', 'A',  0 , 'V', 'I', 'A',  0 , 'V', 'I', 'A',  0  }} },
    { { 'U', 'N', 'K', 'N', 'O', 'W', 'N' }, {{ 0                                                          }} }
  };

  uint32_t i;
  for (i = 0; i < ASMJIT_ARRAY_SIZE(table) - 1; i++)
    if (table[i].d[0] == d0 && table[i].d[1] == d1 && table[i].d[2] == d2)
      break;
  memcpy(cpu._vendor.str, table[i].normalized, 8);
}

static ASMJIT_FAVOR_SIZE void simplifyCpuBrand(char* s) noexcept {
  char* d = s;

  char c = s[0];
  char prev = 0;

  // Used to always clear the current character to ensure that the result
  // doesn't contain garbage after a new null terminator is placed at the end.
  s[0] = '\0';

  for (;;) {
    if (!c)
      break;

    if (!(c == ' ' && (prev == '@' || s[1] == ' ' || s[1] == '@' || s[1] == '\0'))) {
      *d++ = c;
      prev = c;
    }

    c = *++s;
    s[0] = '\0';
  }

  d[0] = '\0';
}

static ASMJIT_FAVOR_SIZE void detectX86Cpu(CpuInfo& cpu) noexcept {
  using Support::bitTest;

  cpuid_t regs;
  xgetbv_t xcr0 { 0, 0 };
  CpuFeatures::X86& features = cpu.features().x86();

  cpu._wasDetected = true;
  cpu._maxLogicalProcessors = 1;

  // We are gonna execute CPUID, which was introduced by I486, so it's the requirement.
  features.add(Ext::kI486);

  // CPUID EAX=0
  // -----------

  // Get vendor string/id.
  cpuidQuery(&regs, 0x0);

  uint32_t maxId = regs.eax;
  uint32_t maxSubLeafId_0x7 = 0;

  simplifyCpuVendor(cpu, regs.ebx, regs.edx, regs.ecx);

  // CPUID EAX=1
  // -----------

  if (maxId >= 0x1) {
    // Get feature flags in ECX/EDX and family/model in EAX.
    cpuidQuery(&regs, 0x1);

    // Fill family and model fields.
    uint32_t modelId  = (regs.eax >> 4) & 0x0F;
    uint32_t familyId = (regs.eax >> 8) & 0x0F;

    // Use extended family and model fields.
    if (familyId == 0x06u || familyId == 0x0Fu)
      modelId += (((regs.eax >> 16) & 0x0Fu) << 4);

    if (familyId == 0x0Fu)
      familyId += ((regs.eax >> 20) & 0xFFu);

    cpu._modelId = modelId;
    cpu._familyId = familyId;
    cpu._brandId = (regs.ebx) & 0xFF;
    cpu._processorType = (regs.eax >> 12) & 0x03;
    cpu._maxLogicalProcessors = (regs.ebx >> 16) & 0xFF;
    cpu._stepping = (regs.eax) & 0x0F;
    cpu._cacheLineSize  = ((regs.ebx >> 8) & 0xFF) * 8;

    features.addIf(bitTest(regs.ecx,  0), Ext::kSSE3);
    features.addIf(bitTest(regs.ecx,  1), Ext::kPCLMULQDQ);
    features.addIf(bitTest(regs.ecx,  3), Ext::kMONITOR);
    features.addIf(bitTest(regs.ecx,  5), Ext::kVMX);
    features.addIf(bitTest(regs.ecx,  6), Ext::kSMX);
    features.addIf(bitTest(regs.ecx,  9), Ext::kSSSE3);
    features.addIf(bitTest(regs.ecx, 13), Ext::kCMPXCHG16B);
    features.addIf(bitTest(regs.ecx, 19), Ext::kSSE4_1);
    features.addIf(bitTest(regs.ecx, 20), Ext::kSSE4_2);
    features.addIf(bitTest(regs.ecx, 22), Ext::kMOVBE);
    features.addIf(bitTest(regs.ecx, 23), Ext::kPOPCNT);
    features.addIf(bitTest(regs.ecx, 25), Ext::kAESNI);
    features.addIf(bitTest(regs.ecx, 26), Ext::kXSAVE);
    features.addIf(bitTest(regs.ecx, 27), Ext::kOSXSAVE);
    features.addIf(bitTest(regs.ecx, 30), Ext::kRDRAND);
    features.addIf(bitTest(regs.edx,  0), Ext::kFPU);
    features.addIf(bitTest(regs.edx,  4), Ext::kRDTSC);
    features.addIf(bitTest(regs.edx,  5), Ext::kMSR);
    features.addIf(bitTest(regs.edx,  8), Ext::kCMPXCHG8B);
    features.addIf(bitTest(regs.edx, 15), Ext::kCMOV);
    features.addIf(bitTest(regs.edx, 19), Ext::kCLFLUSH);
    features.addIf(bitTest(regs.edx, 23), Ext::kMMX);
    features.addIf(bitTest(regs.edx, 24), Ext::kFXSR);
    features.addIf(bitTest(regs.edx, 25), Ext::kSSE, Ext::kMMX2);
    features.addIf(bitTest(regs.edx, 26), Ext::kSSE2, Ext::kSSE);
    features.addIf(bitTest(regs.edx, 28), Ext::kMT);

    // Get the content of XCR0 if supported by the CPU and enabled by the OS.
    if (features.hasXSAVE() && features.hasOSXSAVE()) {
      xgetbvQuery(&xcr0, 0);
    }

    // Detect AVX+.
    if (bitTest(regs.ecx, 28)) {
      // - XCR0[2:1] == 11b
      //   XMM & YMM states need to be enabled by OS.
      if ((xcr0.eax & 0x00000006u) == 0x00000006u) {
        features.add(Ext::kAVX);
        features.addIf(bitTest(regs.ecx, 12), Ext::kFMA);
        features.addIf(bitTest(regs.ecx, 29), Ext::kF16C);
      }
    }
  }

  constexpr uint32_t kXCR0_AMX_Bits = 0x3u << 17;
  bool amxEnabledByOS = (xcr0.eax & kXCR0_AMX_Bits) == kXCR0_AMX_Bits;

#if defined(__APPLE__)
  // Apple platform provides on-demand AVX512 support. When an AVX512 instruction is used the first time it results
  // in #UD, which would cause the thread being promoted to use AVX512 support by the OS in addition to enabling the
  // necessary bits in XCR0 register.
  bool avx512EnabledByOS = true;
#else
  // - XCR0[2:1] ==  11b - XMM/YMM states need to be enabled by OS.
  // - XCR0[7:5] == 111b - Upper 256-bit of ZMM0-XMM15 and ZMM16-ZMM31 need to be enabled by OS.
  constexpr uint32_t kXCR0_AVX512_Bits = (0x3u << 1) | (0x7u << 5);
  bool avx512EnabledByOS = (xcr0.eax & kXCR0_AVX512_Bits) == kXCR0_AVX512_Bits;
#endif

  // CPUID EAX=7 ECX=0
  // -----------------

  // Detect new features if the processor supports CPUID-07.
  bool maybeMPX = false;

  if (maxId >= 0x7) {
    cpuidQuery(&regs, 0x7);

    maybeMPX = bitTest(regs.ebx, 14);
    maxSubLeafId_0x7 = regs.eax;

    features.addIf(bitTest(regs.ebx,  0), Ext::kFSGSBASE);
    features.addIf(bitTest(regs.ebx,  3), Ext::kBMI);
    features.addIf(bitTest(regs.ebx,  4), Ext::kHLE);
    features.addIf(bitTest(regs.ebx,  7), Ext::kSMEP);
    features.addIf(bitTest(regs.ebx,  8), Ext::kBMI2);
    features.addIf(bitTest(regs.ebx,  9), Ext::kERMS);
    features.addIf(bitTest(regs.ebx, 11), Ext::kRTM);
    features.addIf(bitTest(regs.ebx, 18), Ext::kRDSEED);
    features.addIf(bitTest(regs.ebx, 19), Ext::kADX);
    features.addIf(bitTest(regs.ebx, 20), Ext::kSMAP);
    features.addIf(bitTest(regs.ebx, 23), Ext::kCLFLUSHOPT);
    features.addIf(bitTest(regs.ebx, 24), Ext::kCLWB);
    features.addIf(bitTest(regs.ebx, 29), Ext::kSHA);
    features.addIf(bitTest(regs.ecx,  0), Ext::kPREFETCHWT1);
    features.addIf(bitTest(regs.ecx,  4), Ext::kOSPKE);
    features.addIf(bitTest(regs.ecx,  5), Ext::kWAITPKG);
    features.addIf(bitTest(regs.ecx,  7), Ext::kCET_SS);
    features.addIf(bitTest(regs.ecx,  8), Ext::kGFNI);
    features.addIf(bitTest(regs.ecx,  9), Ext::kVAES);
    features.addIf(bitTest(regs.ecx, 10), Ext::kVPCLMULQDQ);
    features.addIf(bitTest(regs.ecx, 22), Ext::kRDPID);
    features.addIf(bitTest(regs.ecx, 25), Ext::kCLDEMOTE);
    features.addIf(bitTest(regs.ecx, 27), Ext::kMOVDIRI);
    features.addIf(bitTest(regs.ecx, 28), Ext::kMOVDIR64B);
    features.addIf(bitTest(regs.ecx, 29), Ext::kENQCMD);
    features.addIf(bitTest(regs.edx,  4), Ext::kFSRM);
    features.addIf(bitTest(regs.edx,  5), Ext::kUINTR);
    features.addIf(bitTest(regs.edx, 14), Ext::kSERIALIZE);
    features.addIf(bitTest(regs.edx, 16), Ext::kTSXLDTRK);
    features.addIf(bitTest(regs.edx, 18), Ext::kPCONFIG);
    features.addIf(bitTest(regs.edx, 20), Ext::kCET_IBT);

    // Detect 'TSX' - Requires at least one of `HLE` and `RTM` features.
    if (features.hasHLE() || features.hasRTM()) {
      features.add(Ext::kTSX);
    }

    if (bitTest(regs.ebx, 5) && features.hasAVX()) {
      features.add(Ext::kAVX2);
    }

    if (avx512EnabledByOS && bitTest(regs.ebx, 16)) {
      features.add(Ext::kAVX512_F);

      features.addIf(bitTest(regs.ebx, 17), Ext::kAVX512_DQ);
      features.addIf(bitTest(regs.ebx, 21), Ext::kAVX512_IFMA);
      features.addIf(bitTest(regs.ebx, 26), Ext::kAVX512_PF);
      features.addIf(bitTest(regs.ebx, 27), Ext::kAVX512_ER);
      features.addIf(bitTest(regs.ebx, 28), Ext::kAVX512_CD);
      features.addIf(bitTest(regs.ebx, 30), Ext::kAVX512_BW);
      features.addIf(bitTest(regs.ebx, 31), Ext::kAVX512_VL);
      features.addIf(bitTest(regs.ecx,  1), Ext::kAVX512_VBMI);
      features.addIf(bitTest(regs.ecx,  6), Ext::kAVX512_VBMI2);
      features.addIf(bitTest(regs.ecx, 11), Ext::kAVX512_VNNI);
      features.addIf(bitTest(regs.ecx, 12), Ext::kAVX512_BITALG);
      features.addIf(bitTest(regs.ecx, 14), Ext::kAVX512_VPOPCNTDQ);
      features.addIf(bitTest(regs.edx,  2), Ext::kAVX512_4VNNIW);
      features.addIf(bitTest(regs.edx,  3), Ext::kAVX512_4FMAPS);
      features.addIf(bitTest(regs.edx,  8), Ext::kAVX512_VP2INTERSECT);
      features.addIf(bitTest(regs.edx, 23), Ext::kAVX512_FP16);
    }

    if (amxEnabledByOS) {
      features.addIf(bitTest(regs.edx, 22), Ext::kAMX_BF16);
      features.addIf(bitTest(regs.edx, 24), Ext::kAMX_TILE);
      features.addIf(bitTest(regs.edx, 25), Ext::kAMX_INT8);
    }
  }

  // CPUID EAX=7 ECX=1
  // -----------------

  if (maxSubLeafId_0x7 >= 1) {
    cpuidQuery(&regs, 0x7, 1);

    features.addIf(bitTest(regs.eax,  0), Ext::kSHA512);
    features.addIf(bitTest(regs.eax,  1), Ext::kSM3);
    features.addIf(bitTest(regs.eax,  2), Ext::kSM4);
    features.addIf(bitTest(regs.eax,  3), Ext::kRAO_INT);
    features.addIf(bitTest(regs.eax,  7), Ext::kCMPCCXADD);
    features.addIf(bitTest(regs.eax, 10), Ext::kFZRM);
    features.addIf(bitTest(regs.eax, 11), Ext::kFSRS);
    features.addIf(bitTest(regs.eax, 12), Ext::kFSRC);
    features.addIf(bitTest(regs.eax, 19), Ext::kWRMSRNS);
    features.addIf(bitTest(regs.eax, 22), Ext::kHRESET);
    features.addIf(bitTest(regs.eax, 26), Ext::kLAM);
    features.addIf(bitTest(regs.eax, 27), Ext::kMSRLIST);
    features.addIf(bitTest(regs.ebx,  1), Ext::kTSE);
    features.addIf(bitTest(regs.edx, 14), Ext::kPREFETCHI);
    features.addIf(bitTest(regs.edx, 18), Ext::kCET_SSS);
    features.addIf(bitTest(regs.edx, 21), Ext::kAPX_F);

    if (features.hasAVX2()) {
      features.addIf(bitTest(regs.eax,  4), Ext::kAVX_VNNI);
      features.addIf(bitTest(regs.eax, 23), Ext::kAVX_IFMA);
      features.addIf(bitTest(regs.edx,  4), Ext::kAVX_VNNI_INT8);
      features.addIf(bitTest(regs.edx,  5), Ext::kAVX_NE_CONVERT);
      features.addIf(bitTest(regs.edx, 10), Ext::kAVX_VNNI_INT16);
    }

    if (features.hasAVX512_F()) {
      features.addIf(bitTest(regs.eax,  5), Ext::kAVX512_BF16);
    }

    if (amxEnabledByOS) {
      features.addIf(bitTest(regs.eax, 21), Ext::kAMX_FP16);
      features.addIf(bitTest(regs.edx,  8), Ext::kAMX_COMPLEX);
    }
  }

  // CPUID EAX=13 ECX=0
  // ------------------

  if (maxId >= 0xD) {
    cpuidQuery(&regs, 0xD, 0);

    // Both CPUID result and XCR0 has to be enabled to have support for MPX.
    if (((regs.eax & xcr0.eax) & 0x00000018u) == 0x00000018u && maybeMPX)
      features.add(Ext::kMPX);

    cpuidQuery(&regs, 0xD, 1);

    features.addIf(bitTest(regs.eax, 0), Ext::kXSAVEOPT);
    features.addIf(bitTest(regs.eax, 1), Ext::kXSAVEC);
    features.addIf(bitTest(regs.eax, 3), Ext::kXSAVES);
  }

  // CPUID EAX=14 ECX=0
  // ------------------

  if (maxId >= 0xE) {
    cpuidQuery(&regs, 0xE, 0);

    features.addIf(bitTest(regs.ebx, 4), Ext::kPTWRITE);
  }

  // CPUID EAX=0x80000000...maxId
  // ----------------------------

  maxId = 0x80000000u;
  uint32_t i = maxId;

  // The highest EAX that we understand.
  constexpr uint32_t kHighestProcessedEAX = 0x8000001Fu;

  // Several CPUID calls are required to get the whole branc string. It's easier
  // to copy one DWORD at a time instead of copying the string a byte by byte.
  uint32_t* brand = cpu._brand.u32;
  do {
    cpuidQuery(&regs, i);
    switch (i) {
      case 0x80000000u:
        maxId = Support::min<uint32_t>(regs.eax, kHighestProcessedEAX);
        break;

      case 0x80000001u:
        features.addIf(bitTest(regs.ecx,  0), Ext::kLAHFSAHF);
        features.addIf(bitTest(regs.ecx,  2), Ext::kSVM);
        features.addIf(bitTest(regs.ecx,  5), Ext::kLZCNT);
        features.addIf(bitTest(regs.ecx,  6), Ext::kSSE4A);
        features.addIf(bitTest(regs.ecx,  7), Ext::kMSSE);
        features.addIf(bitTest(regs.ecx,  8), Ext::kPREFETCHW);
        features.addIf(bitTest(regs.ecx, 12), Ext::kSKINIT);
        features.addIf(bitTest(regs.ecx, 15), Ext::kLWP);
        features.addIf(bitTest(regs.ecx, 21), Ext::kTBM);
        features.addIf(bitTest(regs.ecx, 29), Ext::kMONITORX);
        features.addIf(bitTest(regs.edx, 20), Ext::kNX);
        features.addIf(bitTest(regs.edx, 21), Ext::kFXSROPT);
        features.addIf(bitTest(regs.edx, 22), Ext::kMMX2);
        features.addIf(bitTest(regs.edx, 27), Ext::kRDTSCP);
        features.addIf(bitTest(regs.edx, 29), Ext::kPREFETCHW);
        features.addIf(bitTest(regs.edx, 30), Ext::k3DNOW2, Ext::kMMX2);
        features.addIf(bitTest(regs.edx, 31), Ext::kPREFETCHW);

        if (features.hasAVX()) {
          features.addIf(bitTest(regs.ecx, 11), Ext::kXOP);
          features.addIf(bitTest(regs.ecx, 16), Ext::kFMA4);
        }

        // This feature seems to be only supported by AMD.
        if (cpu.isVendor("AMD")) {
          features.addIf(bitTest(regs.ecx,  4), Ext::kALTMOVCR8);
        }
        break;

      case 0x80000002u:
      case 0x80000003u:
      case 0x80000004u:
        *brand++ = regs.eax;
        *brand++ = regs.ebx;
        *brand++ = regs.ecx;
        *brand++ = regs.edx;

        // Go directly to the next one we are interested in.
        if (i == 0x80000004u)
          i = 0x80000008u - 1;
        break;

      case 0x80000008u:
        features.addIf(bitTest(regs.ebx,  0), Ext::kCLZERO);
        features.addIf(bitTest(regs.ebx,  0), Ext::kRDPRU);
        features.addIf(bitTest(regs.ebx,  8), Ext::kMCOMMIT);
        features.addIf(bitTest(regs.ebx,  9), Ext::kWBNOINVD);

        // Go directly to the next one we are interested in.
        i = 0x8000001Fu - 1;
        break;

      case 0x8000001Fu:
        features.addIf(bitTest(regs.eax,  0), Ext::kSME);
        features.addIf(bitTest(regs.eax,  1), Ext::kSEV);
        features.addIf(bitTest(regs.eax,  3), Ext::kSEV_ES);
        features.addIf(bitTest(regs.eax,  4), Ext::kSEV_SNP);
        features.addIf(bitTest(regs.eax,  6), Ext::kRMPQUERY);
        break;
    }
  } while (++i <= maxId);

  // Simplify CPU brand string a bit by removing some unnecessary spaces.
  simplifyCpuBrand(cpu._brand.str);
}

} // {x86}

#endif // ASMJIT_ARCH_X86

// CpuInfo - Detect - ARM
// ======================

// Implement the most code outside the platform specific #ifdefs to minimize breaking the detection on
// platforms that don't run on our CI infrastructure. The problem with the detection is that every OS
// requires a specific implementation as ARM features cannot be detected in user-mode without OS enablement.

// The most relevant and accurate information can be found here:
//   https://github.com/llvm-project/llvm/blob/master/lib/Target/AArch64/AArch64.td
//   https://github.com/apple/llvm-project/blob/apple/main/llvm/lib/Target/AArch64/AArch64.td (Apple fork)
//
// Other resources:
//   https://en.wikipedia.org/wiki/AArch64
//   https://en.wikipedia.org/wiki/Apple_silicon#List_of_Apple_processors
//   https://developer.arm.com/downloads/-/exploration-tools/feature-names-for-a-profile
//   https://developer.arm.com/architectures/learn-the-architecture/understanding-the-armv8-x-extensions/single-page

#if ASMJIT_ARCH_ARM

namespace arm {

// ARM commonly refers to CPU features using FEAT_ prefix, we use Ext:: to make it compatible with other parts.
typedef CpuFeatures::ARM Ext;

// CpuInfo - Detect - ARM - OS Kernel Version
// ==========================================

#if defined(__linux__)
struct UNameKernelVersion {
  int parts[3];

  inline bool atLeast(int major, int minor, int patch = 0) const noexcept {
    if (parts[0] >= major) {
      if (parts[0] > major)
        return true;

      if (parts[1] >= minor) {
        if (parts[1] > minor)
          return true;

        return parts[2] >= patch;
      }
    }

    return false;
  }
};

ASMJIT_MAYBE_UNUSED
static UNameKernelVersion getUNameKernelVersion() noexcept {
  UNameKernelVersion ver{};
  ver.parts[0] = -1;

  utsname buffer;
  if (uname(&buffer) != 0)
    return ver;

  size_t count = 0;
  char* p = buffer.release;
  while (*p) {
    uint32_t c = uint8_t(*p);
    if (c >= uint32_t('0') && c <= uint32_t('9')) {
      ver.parts[count] = int(strtol(p, &p, 10));
      if (++count == 3)
        break;
    }
    else if (c == '.' || c == '-') {
      p++;
    }
    else {
      break;
    }
  }

  return ver;
}
#endif // __linux__

// CpuInfo - Detect - ARM - Baseline Features of ARM Architectures
// ===============================================================

ASMJIT_MAYBE_UNUSED
static inline void populateBaseAArch32Features(CpuFeatures::ARM& features) noexcept {
  // No baseline flags at the moment.
  DebugUtils::unused(features);
}

ASMJIT_MAYBE_UNUSED
static inline void populateBaseAArch64Features(CpuFeatures::ARM& features) noexcept {
  // AArch64 is based on ARMv8.0 and later.
  features.add(Ext::kARMv6);
  features.add(Ext::kARMv7);
  features.add(Ext::kARMv8a);

  // AArch64 comes with these features by default.
  features.add(Ext::kASIMD);
  features.add(Ext::kFP);
  features.add(Ext::kIDIVA);
}

static inline void populateBaseARMFeatures(CpuInfo& cpu) noexcept {
#if ASMJIT_ARCH_ARM == 32
  populateBaseAArch32Features(cpu.features().arm());
#else
  populateBaseAArch64Features(cpu.features().arm());
#endif
}

// CpuInfo - Detect - ARM - Mandatory Features of ARM Architectures
// ================================================================

// Populates mandatory ARMv8.[v]A features.
ASMJIT_MAYBE_UNUSED
static ASMJIT_FAVOR_SIZE void populateARMv8AFeatures(CpuFeatures::ARM& features, uint32_t v) noexcept {
  switch (v) {
    default:
      ASMJIT_FALLTHROUGH;
    case 9: // ARMv8.9
      features.add(Ext::kCLRBHB, Ext::kCSSC, Ext::kPRFMSLC, Ext::kSPECRES2, Ext::kRAS2);
      ASMJIT_FALLTHROUGH;
    case 8: // ARMv8.8
      features.add(Ext::kHBC, Ext::kMOPS, Ext::kNMI);
      ASMJIT_FALLTHROUGH;
    case 7: // ARMv8.7
      features.add(Ext::kHCX, Ext::kPAN3, Ext::kWFXT, Ext::kXS);
      ASMJIT_FALLTHROUGH;
    case 6: // ARMv8.6
      features.add(Ext::kAMU1_1, Ext::kBF16, Ext::kECV, Ext::kFGT, Ext::kI8MM);
      ASMJIT_FALLTHROUGH;
    case 5: // ARMv8.5
      features.add(Ext::kBTI, Ext::kCSV2, Ext::kDPB2, Ext::kFLAGM2, Ext::kFRINTTS, Ext::kSB, Ext::kSPECRES, Ext::kSSBS);
      ASMJIT_FALLTHROUGH;
    case 4: // ARMv8.4
      features.add(Ext::kAMU1, Ext::kDIT, Ext::kDOTPROD, Ext::kFLAGM,
                   Ext::kLRCPC2, Ext::kLSE2, Ext::kMPAM, Ext::kNV,
                   Ext::kSEL2, Ext::kTLBIOS, Ext::kTLBIRANGE, Ext::kTRF);
      ASMJIT_FALLTHROUGH;
    case 3: // ARMv8.3
      features.add(Ext::kCCIDX, Ext::kFCMA, Ext::kJSCVT, Ext::kLRCPC, Ext::kPAUTH);
      ASMJIT_FALLTHROUGH;
    case 2: // ARMv8.2
      features.add(Ext::kDPB, Ext::kPAN2, Ext::kRAS, Ext::kUAO);
      ASMJIT_FALLTHROUGH;
    case 1: // ARMv8.1
      features.add(Ext::kCRC32, Ext::kLOR, Ext::kLSE, Ext::kPAN, Ext::kRDM, Ext::kVHE);
      ASMJIT_FALLTHROUGH;
    case 0: // ARMv8.0
      features.add(Ext::kASIMD, Ext::kFP, Ext::kIDIVA, Ext::kVFP_D32);
      break;
  }
}

// Populates mandatory ARMv9.[v] features.
ASMJIT_MAYBE_UNUSED
static ASMJIT_FAVOR_SIZE void populateARMv9AFeatures(CpuFeatures::ARM& features, uint32_t v) noexcept {
  populateARMv8AFeatures(features, v <= 4u ? 5u + v : 9u);

  switch (v) {
    default:
      ASMJIT_FALLTHROUGH;
    case 4: // ARMv9.4 - based on ARMv8.9.
      ASMJIT_FALLTHROUGH;
    case 3: // ARMv9.3 - based on ARMv8.8.
      ASMJIT_FALLTHROUGH;
    case 2: // ARMv9.2 - based on ARMv8.7.
      ASMJIT_FALLTHROUGH;
    case 1: // ARMv9.1 - based on ARMv8.6.
      ASMJIT_FALLTHROUGH;
    case 0: // ARMv9.0 - based on ARMv8.5.
      features.add(Ext::kRME, Ext::kSVE, Ext::kSVE2);
      break;
  }
}

// CpuInfo - Detect - ARM - CPUID Based Features
// =============================================

// This implements detection based on the content of CPUID registers. The following code doesn't actually read any
// of the registers so it's an implementation that can theoretically be tested / used in mocks.

// Merges a feature that contains 0b1111 when it doesn't exist and starts at 0b0000 when it does.
ASMJIT_MAYBE_UNUSED
static ASMJIT_FORCE_INLINE void mergeAArch64CPUIDFeatureNA(CpuFeatures::ARM& features, uint64_t regBits, uint32_t offset,
  Ext::Id f0,
  Ext::Id f1 = Ext::kNone,
  Ext::Id f2 = Ext::kNone,
  Ext::Id f3 = Ext::kNone) noexcept {

  uint32_t val = uint32_t((regBits >> offset) & 0xFu);

  // If val == 0b1111 then the feature is not implemented in this case (some early extensions).
  if (val == 0xFu)
    return;

  if (f0 != Ext::kNone) features.add(f0);
  if (f1 != Ext::kNone) features.addIf(val >= 1, f1);
  if (f2 != Ext::kNone) features.addIf(val >= 2, f2);
  if (f3 != Ext::kNone) features.addIf(val >= 3, f3);
}

// Merges a feature identified by a single bit at `offset`.
ASMJIT_MAYBE_UNUSED
static ASMJIT_FORCE_INLINE void mergeAArch64CPUIDFeature1B(CpuFeatures::ARM& features, uint64_t regBits, uint32_t offset, Ext::Id f1) noexcept {
  features.addIf((regBits & (uint64_t(1) << offset)) != 0, f1);
}

// Merges a feature-list starting from 0b01 when it does (0b00 means feature not supported).
ASMJIT_MAYBE_UNUSED
static ASMJIT_FORCE_INLINE void mergeAArch64CPUIDFeature2B(CpuFeatures::ARM& features, uint64_t regBits, uint32_t offset, Ext::Id f1, Ext::Id f2, Ext::Id f3) noexcept {
  uint32_t val = uint32_t((regBits >> offset) & 0x3u);

  if (f1 != Ext::kNone) features.addIf(val >= 1, f1);
  if (f2 != Ext::kNone) features.addIf(val >= 2, f2);
  if (f3 != Ext::kNone) features.addIf(val == 3, f3);
}

// Merges a feature-list starting from 0b0001 when it does (0b0000 means feature not supported).
ASMJIT_MAYBE_UNUSED
static ASMJIT_FORCE_INLINE void mergeAArch64CPUIDFeature4B(CpuFeatures::ARM& features, uint64_t regBits, uint32_t offset,
  Ext::Id f1,
  Ext::Id f2 = Ext::kNone,
  Ext::Id f3 = Ext::kNone,
  Ext::Id f4 = Ext::kNone) noexcept {

  uint32_t val = uint32_t((regBits >> offset) & 0xFu);

  // if val == 0 it means that this feature is not supported.

  if (f1 != Ext::kNone) features.addIf(val >= 1, f1);
  if (f2 != Ext::kNone) features.addIf(val >= 2, f2);
  if (f3 != Ext::kNone) features.addIf(val >= 3, f3);
  if (f4 != Ext::kNone) features.addIf(val >= 4, f4);
}

// Merges a feature that is identified by an exact bit-combination of 4 bits.
ASMJIT_MAYBE_UNUSED
static ASMJIT_FORCE_INLINE void mergeAArch64CPUIDFeature4S(CpuFeatures::ARM& features, uint64_t regBits, uint32_t offset, uint32_t value, Ext::Id f1) noexcept {
  features.addIf(uint32_t((regBits >> offset) & 0xFu) == value, f1);
}

#define MERGE_FEATURE_NA(identifier, reg, offset, ...) mergeAArch64CPUIDFeatureNA(cpu.features().arm(), reg, offset, __VA_ARGS__)
#define MERGE_FEATURE_1B(identifier, reg, offset, ...) mergeAArch64CPUIDFeature1B(cpu.features().arm(), reg, offset, __VA_ARGS__)
#define MERGE_FEATURE_2B(identifier, reg, offset, ...) mergeAArch64CPUIDFeature2B(cpu.features().arm(), reg, offset, __VA_ARGS__)
#define MERGE_FEATURE_4B(identifier, reg, offset, ...) mergeAArch64CPUIDFeature4B(cpu.features().arm(), reg, offset, __VA_ARGS__)
#define MERGE_FEATURE_4S(identifier, reg, offset, ...) mergeAArch64CPUIDFeature4S(cpu.features().arm(), reg, offset, __VA_ARGS__)

// Detects features based on the content of ID_AA64PFR0_EL1 and ID_AA64PFR1_EL1 registers.
ASMJIT_MAYBE_UNUSED
static inline void detectAArch64FeaturesViaCPUID_AA64PFR0_AA64PFR1(CpuInfo& cpu, uint64_t fpr0, uint64_t fpr1) noexcept {
  // ID_AA64PFR0_EL1
  // ===============

  // FP and AdvSIMD bits should match (i.e. if FP features FP16, ASIMD must feature it too).
  MERGE_FEATURE_NA("FP bits [19:16]"          , fpr0, 16, Ext::kFP, Ext::kFP16);
  MERGE_FEATURE_NA("AdvSIMD bits [23:20]"     , fpr0, 20, Ext::kASIMD, Ext::kFP16);
  /*
  MERGE_FEATURE_4B("GIC bits [27:24]"         , fpr0, 24, ...);
  */
  MERGE_FEATURE_4B("RAS bits [31:28]"         , fpr0, 28, Ext::kRAS, Ext::kRAS1_1, Ext::kRAS2);
  MERGE_FEATURE_4B("SVE bits [35:32]"         , fpr0, 32, Ext::kSVE);
  MERGE_FEATURE_4B("SEL2 bits [39:36]"        , fpr0, 36, Ext::kSEL2);
  MERGE_FEATURE_4B("MPAM bits [43:40]"        , fpr0, 40, Ext::kMPAM);
  MERGE_FEATURE_4B("AMU bits [47:44]"         , fpr0, 44, Ext::kAMU1, Ext::kAMU1_1);
  MERGE_FEATURE_4B("DIT bits [51:48]"         , fpr0, 48, Ext::kDIT);
  MERGE_FEATURE_4B("RME bits [55:52]"         , fpr0, 52, Ext::kRME);
  MERGE_FEATURE_4B("CSV2 bits [59:56]"        , fpr0, 56, Ext::kCSV2, Ext::kCSV2, Ext::kCSV2, Ext::kCSV2_3);
  MERGE_FEATURE_4B("CSV3 bits [63:60]"        , fpr0, 60, Ext::kCSV3);

  // ID_AA64PFR1_EL1
  // ===============

  MERGE_FEATURE_4B("BT bits [3:0]"            , fpr1,  0, Ext::kBTI);
  MERGE_FEATURE_4B("SSBS bits [7:4]"          , fpr1,  4, Ext::kSSBS, Ext::kSSBS2);
  MERGE_FEATURE_4B("MTE bits [11:8]"          , fpr1,  8, Ext::kMTE, Ext::kMTE2, Ext::kMTE3);
  /*
  MERGE_FEATURE_4B("RAS_frac bits [15:12]"    , fpr1, 12, ...);
  MERGE_FEATURE_4B("MPAM_frac bits [19:16]"   , fpr1, 16, ...);
  */
  MERGE_FEATURE_4B("SME bits [27:24]"         , fpr1, 24, Ext::kSME, Ext::kSME2);
  MERGE_FEATURE_4B("RNDR_trap bits [31:28]"   , fpr1, 28, Ext::kRNG_TRAP);
  /*
  MERGE_FEATURE_4B("CSV2_frac bits [35:32]"   , fpr1, 32, ...);
  */
  MERGE_FEATURE_4B("NMI bits [39:36]"         , fpr1, 36, Ext::kNMI);
  /*
  MERGE_FEATURE_4B("MTE_frac bits [43:40]"    , fpr1, 40, ...);
  */
  MERGE_FEATURE_4B("GCS bits [47:44]"         , fpr1, 44, Ext::kGCS);
  MERGE_FEATURE_4B("THE bits [51:48]"         , fpr1, 48, Ext::kTHE);

  // MTEX extensions are only available when MTE3 is available.
  if (cpu.features().arm().hasMTE3())
    MERGE_FEATURE_4B("MTEX bits [55:52]"      , fpr1, 52, Ext::kMTE4);

  /*
  MERGE_FEATURE_4B("DF2 bits [59:56]"         , fpr1, 56, ...);
  */
  MERGE_FEATURE_4B("PFAR bits [63:60]"        , fpr1, 60, Ext::kPFAR);

  // ID_AA64PFR0_EL1 + ID_AA64PFR1_EL1
  // =================================

  uint32_t rasMain = uint32_t((fpr0 >> 28) & 0xFu);
  uint32_t rasFrac = uint32_t((fpr1 >> 12) & 0xFu);

  if (rasMain == 1 && rasFrac == 1) {
    cpu.features().arm().add(Ext::kRAS1_1);
  }

  uint32_t mpamMain = uint32_t((fpr0 >> 40) & 0xFu);
  uint32_t mpamFrac = uint32_t((fpr1 >> 16) & 0xFu);

  if (mpamMain || mpamFrac)
    cpu.features().arm().add(Ext::kMPAM);
}

// Detects features based on the content of ID_AA64ISAR0_EL1 and ID_AA64ISAR1_EL1 registers.
ASMJIT_MAYBE_UNUSED
static inline void detectAArch64FeaturesViaCPUID_AA64ISAR0_AA64ISAR1(CpuInfo& cpu, uint64_t isar0, uint64_t isar1) noexcept {
  // ID_AA64ISAR0_EL1
  // ================

  MERGE_FEATURE_4B("AES bits [7:4]"           , isar0,  4, Ext::kAES, Ext::kPMULL);
  MERGE_FEATURE_4B("SHA1 bits [11:8]"         , isar0,  8, Ext::kSHA1);
  MERGE_FEATURE_4B("SHA2 bits [15:12]"        , isar0, 12, Ext::kSHA256, Ext::kSHA512);
  MERGE_FEATURE_4B("CRC32 bits [19:16]"       , isar0, 16, Ext::kCRC32);
  MERGE_FEATURE_4B("Atomic bits [23:20]"      , isar0, 20, Ext::kNone, Ext::kLSE, Ext::kLSE128);
  MERGE_FEATURE_4B("TME bits [27:24]"         , isar0, 24, Ext::kTME);
  MERGE_FEATURE_4B("RDM bits [31:28]"         , isar0, 28, Ext::kRDM);
  MERGE_FEATURE_4B("SHA3 bits [35:32]"        , isar0, 32, Ext::kSHA3);
  MERGE_FEATURE_4B("SM3 bits [39:36]"         , isar0, 36, Ext::kSM3);
  MERGE_FEATURE_4B("SM4 bits [43:40]"         , isar0, 40, Ext::kSM4);
  MERGE_FEATURE_4B("DP bits [47:44]"          , isar0, 44, Ext::kDOTPROD);
  MERGE_FEATURE_4B("FHM bits [51:48]"         , isar0, 48, Ext::kFHM);
  MERGE_FEATURE_4B("TS bits [55:52]"          , isar0, 52, Ext::kFLAGM, Ext::kFLAGM2);
  /*
  MERGE_FEATURE_4B("TLB bits [59:56]"         , isar0, 56, ...);
  */
  MERGE_FEATURE_4B("RNDR bits [63:60]"        , isar0, 60, Ext::kFLAGM, Ext::kRNG);

  // ID_AA64ISAR1_EL1
  // ================

  MERGE_FEATURE_4B("DPB bits [3:0]"           , isar1,  0, Ext::kDPB, Ext::kDPB2);
  /*
  MERGE_FEATURE_4B("APA bits [7:4]"           , isar1,  4, ...);
  MERGE_FEATURE_4B("API bits [11:8]"          , isar1,  8, ...);
  */
  MERGE_FEATURE_4B("JSCVT bits [15:12]"       , isar1, 12, Ext::kJSCVT);
  MERGE_FEATURE_4B("FCMA bits [19:16]"        , isar1, 16, Ext::kFCMA);
  MERGE_FEATURE_4B("LRCPC bits [23:20]"       , isar1, 20, Ext::kLRCPC, Ext::kLRCPC2, Ext::kLRCPC3);
  /*
  MERGE_FEATURE_4B("GPA bits [27:24]"         , isar1, 24, ...);
  MERGE_FEATURE_4B("GPI bits [31:28]"         , isar1, 28, ...);
  */
  MERGE_FEATURE_4B("FRINTTS bits [35:32]"     , isar1, 32, Ext::kFRINTTS);
  MERGE_FEATURE_4B("SB bits [39:36]"          , isar1, 36, Ext::kSB);
  MERGE_FEATURE_4B("SPECRES bits [43:40]"     , isar1, 40, Ext::kSPECRES, Ext::kSPECRES2);
  MERGE_FEATURE_4B("BF16 bits [47:44]"        , isar1, 44, Ext::kBF16, Ext::kEBF16);
  MERGE_FEATURE_4B("DGH bits [51:48]"         , isar1, 48, Ext::kDGH);
  MERGE_FEATURE_4B("I8MM bits [55:52]"        , isar1, 52, Ext::kI8MM);
  MERGE_FEATURE_4B("XS bits [59:56]"          , isar1, 56, Ext::kXS);
  MERGE_FEATURE_4B("LS64 bits [63:60]"        , isar1, 60, Ext::kLS64, Ext::kLS64_V, Ext::kLS64_ACCDATA);
}

// Detects features based on the content of ID_AA64ISAR2_EL1 register.
ASMJIT_MAYBE_UNUSED
static inline void detectAArch64FeaturesViaCPUID_AA64ISAR2(CpuInfo& cpu, uint64_t isar2) noexcept {
  MERGE_FEATURE_4B("WFxT bits [3:0]"          , isar2,  0, Ext::kNone, Ext::kWFXT);
  MERGE_FEATURE_4B("RPRES bits [7:4]"         , isar2,  4, Ext::kRPRES);
  /*
  MERGE_FEATURE_4B("GPA3 bits [11:8]"         , isar2,  8, ...);
  MERGE_FEATURE_4B("APA3 bits [15:12]"        , isar2, 12, ...);
  */
  MERGE_FEATURE_4B("MOPS bits [19:16]"        , isar2, 16, Ext::kMOPS);
  MERGE_FEATURE_4B("BC bits [23:20]"          , isar2, 20, Ext::kHBC);
  MERGE_FEATURE_4B("PAC_frac bits [27:24]"    , isar2, 24, Ext::kCONSTPACFIELD);
  MERGE_FEATURE_4B("CLRBHB bits [31:28]"      , isar2, 28, Ext::kCLRBHB);
  MERGE_FEATURE_4B("SYSREG128 bits [35:32]"   , isar2, 32, Ext::kSYSREG128);
  MERGE_FEATURE_4B("SYSINSTR128 bits [39:36]" , isar2, 36, Ext::kSYSINSTR128);
  MERGE_FEATURE_4B("PRFMSLC bits [43:40]"     , isar2, 40, Ext::kPRFMSLC);
  MERGE_FEATURE_4B("RPRFM bits [51:48]"       , isar2, 48, Ext::kRPRFM);
  MERGE_FEATURE_4B("CSSC bits [55:52]"        , isar2, 52, Ext::kCSSC);
  MERGE_FEATURE_4B("LUT bits [59:56]"         , isar2, 56, Ext::kLUT);
}

// TODO: This register is not accessed at the moment.
#if 0
// Detects features based on the content of ID_AA64ISAR3_EL1register.
ASMJIT_MAYBE_UNUSED
static inline void detectAArch64FeaturesViaCPUID_AA64ISAR3(CpuInfo& cpu, uint64_t isar3) noexcept {
  // ID_AA64ISAR3_EL1
  // ================

  MERGE_FEATURE_4B("CPA bits [3:0]"           , isar3, 0, Ext::kCPA, Ext::kCPA2);
  MERGE_FEATURE_4B("FAMINMAX bits [7:4]"      , isar3, 4, Ext::kFAMINMAX);
  MERGE_FEATURE_4B("TLBIW bits [11:8]"        , isar3, 8, Ext::kTLBIW);
}
#endif

ASMJIT_MAYBE_UNUSED
static inline void detectAArch64FeaturesViaCPUID_AA64MMFR0(CpuInfo& cpu, uint64_t mmfr0) noexcept {
  // ID_AA64MMFR0_EL1
  // ================

  /*
  MERGE_FEATURE_4B("PARange bits [3:0]"       , mmfr0,  0, ...);
  MERGE_FEATURE_4B("ASIDBits bits [7:4]"      , mmfr0,  4, ...);
  MERGE_FEATURE_4B("BigEnd bits [11:8]"       , mmfr0,  8, ...);
  MERGE_FEATURE_4B("SNSMem bits [15:12]"      , mmfr0, 12, ...);
  MERGE_FEATURE_4B("BigEndEL0 bits [19:16]"   , mmfr0, 16, ...);
  MERGE_FEATURE_4B("TGran16 bits [23:20]"     , mmfr0, 20, ...);
  MERGE_FEATURE_4B("TGran64 bits [27:24]"     , mmfr0, 24, ...);
  MERGE_FEATURE_4B("TGran4 bits [31:28]"      , mmfr0, 28, ...);
  MERGE_FEATURE_4B("TGran16_2 bits [35:32]"   , mmfr0, 32, ...);
  MERGE_FEATURE_4B("TGran64_2 bits [39:36]"   , mmfr0, 36, ...);
  MERGE_FEATURE_4B("TGran4_2 bits [43:40]"    , mmfr0, 40, ...);
  MERGE_FEATURE_4B("ExS bits [47:44]"         , mmfr0, 44, ...);
  */
  MERGE_FEATURE_4B("FGT bits [59:56]"         , mmfr0, 56, Ext::kFGT, Ext::kFGT2);
  MERGE_FEATURE_4B("ECV bits [63:60]"         , mmfr0, 60, Ext::kECV);
}

ASMJIT_MAYBE_UNUSED
static inline void detectAArch64FeaturesViaCPUID_AA64MMFR1(CpuInfo& cpu, uint64_t mmfr1) noexcept {
  // ID_AA64MMFR1_EL1
  // ================

  MERGE_FEATURE_4B("HAFDBS bits [3:0]"        , mmfr1,  0, Ext::kHAFDBS, Ext::kNone, Ext::kHAFT, Ext::kHDBSS);
  MERGE_FEATURE_4B("VMIDBits bits [7:4]"      , mmfr1,  4, Ext::kVMID16);
  MERGE_FEATURE_4B("VH bits [11:8]"           , mmfr1,  8, Ext::kVHE);
  MERGE_FEATURE_4B("HPDS bits [15:12]"        , mmfr1, 12, Ext::kHPDS, Ext::kHPDS2);
  MERGE_FEATURE_4B("LO bits [19:16]"          , mmfr1, 16, Ext::kLOR);
  MERGE_FEATURE_4B("PAN bits [23:20]"         , mmfr1, 20, Ext::kPAN, Ext::kPAN2, Ext::kPAN3);
  /*
  MERGE_FEATURE_4B("SpecSEI bits [27:24]"     , mmfr1, 24, ...);
  */
  MERGE_FEATURE_4B("XNX bits [31:28]"         , mmfr1, 28, Ext::kXNX);
  /*
  MERGE_FEATURE_4B("TWED bits [35:32]"        , mmfr1, 32, ...);
  MERGE_FEATURE_4B("ETS bits [39:36]"         , mmfr1, 36, ...);
  */
  MERGE_FEATURE_4B("HCX bits [43:40]"         , mmfr1, 40, Ext::kHCX);
  MERGE_FEATURE_4B("AFP bits [47:44]"         , mmfr1, 44, Ext::kAFP);
  /*
  MERGE_FEATURE_4B("nTLBPA bits [51:48]"      , mmfr1, 48, ...);
  MERGE_FEATURE_4B("TIDCP1 bits [55:52]"      , mmfr1, 52, ...);
  */
  MERGE_FEATURE_4B("CMOW bits [59:56]"        , mmfr1, 56, Ext::kCMOW);
  MERGE_FEATURE_4B("ECBHB bits [63:60]"       , mmfr1, 60, Ext::kECBHB);
}

ASMJIT_MAYBE_UNUSED
static inline void detectAArch64FeaturesViaCPUID_AA64MMFR2(CpuInfo& cpu, uint64_t mmfr2) noexcept {
  // ID_AA64MMFR2_EL1
  // ================

  /*
  MERGE_FEATURE_4B("CnP bits [3:0]"           , mmfr2,  0, ...);
  */
  MERGE_FEATURE_4B("UAO bits [7:4]"           , mmfr2,  4, Ext::kUAO);
  /*
  MERGE_FEATURE_4B("LSM bits [11:8]"          , mmfr2,  8, ...);
  MERGE_FEATURE_4B("IESB bits [15:12]"        , mmfr2, 12, ...);
  */
  MERGE_FEATURE_4B("VARange bits [19:16]"     , mmfr2, 16, Ext::kLVA, Ext::kLVA3);
  MERGE_FEATURE_4B("CCIDX bits [23:20]"       , mmfr2, 20, Ext::kCCIDX);
  MERGE_FEATURE_4B("NV bits [27:24]"          , mmfr2, 24, Ext::kNV, Ext::kNV2);
  /*
  MERGE_FEATURE_4B("ST bits [31:28]"          , mmfr2, 28, ...);
  */
  MERGE_FEATURE_4B("AT bits [35:32]"          , mmfr2, 32, Ext::kLSE2);
  /*
  MERGE_FEATURE_4B("IDS bits [39:36]"         , mmfr2, 36, ...);
  MERGE_FEATURE_4B("FWB bits [43:40]"         , mmfr2, 40, ...);
  MERGE_FEATURE_4B("TTL bits [51:48]"         , mmfr2, 48, ...);
  MERGE_FEATURE_4B("BBM bits [55:52]"         , mmfr2, 52, ...);
  MERGE_FEATURE_4B("EVT bits [59:56]"         , mmfr2, 56, ...);
  MERGE_FEATURE_4B("E0PD bits [63:60]"        , mmfr2, 60, ...);
  */
}

// Detects features based on the content of ID_AA64ZFR0_EL1 register.
ASMJIT_MAYBE_UNUSED
static inline void detectAArch64FeaturesViaCPUID_AA64ZFR0(CpuInfo& cpu, uint64_t zfr0) noexcept {
  MERGE_FEATURE_4B("SVEver bits [3:0]"        , zfr0,  0, Ext::kSVE2, Ext::kSVE2_1);
  MERGE_FEATURE_4B("AES bits [7:4]"           , zfr0,  4, Ext::kSVE_AES, Ext::kSVE_PMULL128);
  MERGE_FEATURE_4B("BitPerm bits [19:16]"     , zfr0, 16, Ext::kSVE_BITPERM);
  MERGE_FEATURE_4B("BF16 bits [23:20]"        , zfr0, 20, Ext::kSVE_BF16, Ext::kSVE_EBF16);
  MERGE_FEATURE_4B("B16B16 bits [27:24]"      , zfr0, 24, Ext::kSVE_B16B16);
  MERGE_FEATURE_4B("SHA3 bits [35:32]"        , zfr0, 32, Ext::kSVE_SHA3);
  MERGE_FEATURE_4B("SM4 bits [43:40]"         , zfr0, 40, Ext::kSVE_SM4);
  MERGE_FEATURE_4B("I8MM bits [47:44]"        , zfr0, 44, Ext::kSVE_I8MM);
  MERGE_FEATURE_4B("F32MM bits [55:52]"       , zfr0, 52, Ext::kSVE_F32MM);
  MERGE_FEATURE_4B("F64MM bits [59:56]"       , zfr0, 56, Ext::kSVE_F64MM);
}

ASMJIT_MAYBE_UNUSED
static inline void detectAArch64FeaturesViaCPUID_AA64SMFR0(CpuInfo& cpu, uint64_t smfr0) noexcept {
  MERGE_FEATURE_1B("SF8DP2 bit [28]"          , smfr0, 29, Ext::kSSVE_FP8DOT2);
  MERGE_FEATURE_1B("SF8DP4 bit [29]"          , smfr0, 29, Ext::kSSVE_FP8DOT4);
  MERGE_FEATURE_1B("SF8FMA bit [30]"          , smfr0, 30, Ext::kSSVE_FP8FMA);
  MERGE_FEATURE_1B("F32F32 bit [32]"          , smfr0, 32, Ext::kSME_F32F32);
  MERGE_FEATURE_1B("BI32I32 bit [33]"         , smfr0, 33, Ext::kSME_BI32I32);
  MERGE_FEATURE_1B("B16F32 bit [34]"          , smfr0, 34, Ext::kSME_B16F32);
  MERGE_FEATURE_1B("F16F32 bit [35]"          , smfr0, 35, Ext::kSME_F16F32);
  MERGE_FEATURE_4S("I8I32 bits [39:36]"       , smfr0, 36, 0xF, Ext::kSME_I8I32);
  MERGE_FEATURE_1B("F8F32 bit [40]"           , smfr0, 40, Ext::kSME_F8F32);
  MERGE_FEATURE_1B("F8F16 bit [41]"           , smfr0, 41, Ext::kSME_F8F16);
  MERGE_FEATURE_1B("F16F16 bit [42]"          , smfr0, 42, Ext::kSME_F16F16);
  MERGE_FEATURE_1B("B16B16 bit [43]"          , smfr0, 43, Ext::kSME_B16B16);
  MERGE_FEATURE_4S("I16I32 bits [47:44]"      , smfr0, 44, 0x5, Ext::kSME_I16I32);
  MERGE_FEATURE_1B("F64F64 bit [48]"          , smfr0, 48, Ext::kSME_F64F64);
  MERGE_FEATURE_4S("I16I64 bits [55:52]"      , smfr0, 52, 0xF, Ext::kSME_I16I64);
  MERGE_FEATURE_4B("SMEver bits [59:56]"      , smfr0, 56, Ext::kSME2, Ext::kSME2_1);
  MERGE_FEATURE_1B("LUTv2 bit [60]"           , smfr0, 60, Ext::kSME_LUTv2);
  MERGE_FEATURE_1B("FA64 bit [63]"            , smfr0, 63, Ext::kSME_FA64);
}

#undef MERGE_FEATURE_4S
#undef MERGE_FEATURE_4B
#undef MERGE_FEATURE_2B
#undef MERGE_FEATURE_1B
#undef MERGE_FEATURE_NA

// CpuInfo - Detect - ARM - CPU Vendor Features
// ============================================

// CPU features detection based on Apple family ID.
enum class AppleFamilyId : uint32_t {
  // Apple design.
  kSWIFT              = 0x1E2D6381u, // Apple A6/A6X (ARMv7s).
  kCYCLONE            = 0x37A09642u, // Apple A7 (ARMv8.0-A).
  kTYPHOON            = 0x2C91A47Eu, // Apple A8 (ARMv8.0-A).
  kTWISTER            = 0x92FB37C8u, // Apple A9 (ARMv8.0-A).
  kHURRICANE          = 0x67CEEE93u, // Apple A10 (ARMv8.1-A).
  kMONSOON_MISTRAL    = 0xE81E7EF6u, // Apple A11 (ARMv8.2-A).
  kVORTEX_TEMPEST     = 0x07D34B9Fu, // Apple A12 (ARMv8.3-A).
  kLIGHTNING_THUNDER  = 0x462504D2u, // Apple A13 (ARMv8.4-A).
  kFIRESTORM_ICESTORM = 0x1B588BB3u, // Apple A14/M1 (ARMv8.5-A).
  kAVALANCHE_BLIZZARD = 0XDA33D83Du, // Apple A15/M2.
  kEVEREST_SAWTOOTH   = 0X8765EDEAu  // Apple A16.
};

ASMJIT_MAYBE_UNUSED
static ASMJIT_FAVOR_SIZE bool detectARMFeaturesViaAppleFamilyId(CpuInfo& cpu) noexcept {
  typedef AppleFamilyId Id;
  CpuFeatures::ARM& features = cpu.features().arm();

  switch (cpu.familyId()) {
    // Apple A7-A9 (ARMv8.0-A).
    case uint32_t(Id::kCYCLONE):
    case uint32_t(Id::kTYPHOON):
    case uint32_t(Id::kTWISTER):
      populateARMv8AFeatures(features, 0);
      features.add(Ext::kAES, Ext::kPMU, Ext::kPMULL, Ext::kSHA1, Ext::kSHA256);
      return true;

    // Apple A10 (ARMv8.0-A).
    case uint32_t(Id::kHURRICANE):
      populateARMv8AFeatures(features, 0);
      features.add(Ext::kAES, Ext::kCRC32, Ext::kLOR, Ext::kPAN, Ext::kPMU, Ext::kPMULL, Ext::kRDM, Ext::kSHA1,
                   Ext::kSHA256, Ext::kVHE);
      return true;

    // Apple A11 (ARMv8.2-A).
    case uint32_t(Id::kMONSOON_MISTRAL):
      populateARMv8AFeatures(features, 2);
      features.add(Ext::kAES, Ext::kFP16, Ext::kFP16CONV, Ext::kPMU, Ext::kPMULL, Ext::kSHA1, Ext::kSHA256);
      return true;

    // Apple A12 (ARMv8.3-A).
    case uint32_t(Id::kVORTEX_TEMPEST):
      populateARMv8AFeatures(features, 3);
      features.add(Ext::kAES, Ext::kFP16, Ext::kFP16CONV, Ext::kPMU, Ext::kPMULL, Ext::kSHA1, Ext::kSHA256);
      return true;

    // Apple A13 (ARMv8.4-A).
    case uint32_t(Id::kLIGHTNING_THUNDER):
      populateARMv8AFeatures(features, 4);
      features.add(Ext::kAES, Ext::kFHM, Ext::kFP16, Ext::kFP16CONV, Ext::kPMU, Ext::kPMULL, Ext::kSHA1,
                   Ext::kSHA256, Ext::kSHA3, Ext::kSHA512);
      return true;

    // Apple A14/M1 (ARMv8.5-A).
    case uint32_t(Id::kFIRESTORM_ICESTORM):
      populateARMv8AFeatures(features, 4);
      features.add(Ext::kAES, Ext::kCSV2, Ext::kCSV3, Ext::kDPB2, Ext::kECV, Ext::kFHM, Ext::kFLAGM2,
                   Ext::kFP16, Ext::kFP16CONV, Ext::kFRINTTS, Ext::kPMU, Ext::kPMULL, Ext::kSB,
                   Ext::kSHA1, Ext::kSHA256, Ext::kSHA3, Ext::kSHA512, Ext::kSSBS);
      return true;

    // Apple A15/M2.
    case uint32_t(Id::kAVALANCHE_BLIZZARD):
      populateARMv8AFeatures(features, 6);
      features.add(Ext::kAES, Ext::kFHM, Ext::kFP16, Ext::kFP16CONV, Ext::kPMU, Ext::kPMULL, Ext::kSHA1,
                   Ext::kSHA256, Ext::kSHA3, Ext::kSHA512);
      return true;

    // Apple A16.
    case uint32_t(Id::kEVEREST_SAWTOOTH):
      populateARMv8AFeatures(features, 6);
      features.add(Ext::kAES, Ext::kFHM, Ext::kFP16, Ext::kFP16CONV, Ext::kHCX, Ext::kPMU, Ext::kPMULL,
                   Ext::kSHA1, Ext::kSHA256, Ext::kSHA3, Ext::kSHA512);
      return true;

    default:
      return false;
  }
}

// CpuInfo - Detect - ARM - Compile Flags Features
// ===============================================

// Detects ARM version by macros defined at compile time. This means that AsmJit will report features forced at
// compile time that should always be provided by the target CPU. This also means that if we don't provide any
// means to detect CPU features the features reported by AsmJit will at least not report less features than the
// target it was compiled to.

#if ASMJIT_ARCH_ARM == 32
ASMJIT_MAYBE_UNUSED
static ASMJIT_FAVOR_SIZE void detectAArch32FeaturesViaCompilerFlags(CpuInfo& cpu) noexcept {
  DebugUtils::unused(cpu);

  // ARM targets have no baseline at the moment.
#if defined(__ARM_ARCH_7A__)
  cpu.addFeature(CpuFeatures::ARM::kARMv7);
#endif

#if defined(__ARM_ARCH_8A__)
  cpu.addFeature(CpuFeatures::ARM::kARMv8a);
#endif

#if defined(__TARGET_ARCH_THUMB)
  cpu.addFeature(CpuFeatures::ARM::kTHUMB);
#if __TARGET_ARCH_THUMB >= 4
  cpu.addFeature(CpuFeatures::ARM::kTHUMBv2);
#endif
#endif

#if defined(__ARM_FEATURE_FMA)
  cpu.addFeature(Ext::kFP);
#endif

#if defined(__ARM_NEON)
  cpu.addFeature(Ext::kASIMD);
#endif

#if defined(__ARM_FEATURE_IDIV) && defined(__TARGET_ARCH_THUMB)
  cpu.addFeature(Ext::kIDIVT);
#endif
#if defined(__ARM_FEATURE_IDIV) && !defined(__TARGET_ARCH_THUMB)
  cpu.addFeature(Ext::kIDIVA);
#endif
}
#endif // ASMJIT_ARCH_ARM == 32

#if ASMJIT_ARCH_ARM == 64
ASMJIT_MAYBE_UNUSED
static ASMJIT_FAVOR_SIZE void detectAArch64FeaturesViaCompilerFlags(CpuInfo& cpu) noexcept {
  DebugUtils::unused(cpu);

#if defined(__ARM_ARCH_9_5A__)
  populateARMv9AFeatures(cpu.features().arm(), 5);
#elif defined(__ARM_ARCH_9_4A__)
  populateARMv9AFeatures(cpu.features().arm(), 4);
#elif defined(__ARM_ARCH_9_3A__)
  populateARMv9AFeatures(cpu.features().arm(), 3);
#elif defined(__ARM_ARCH_9_2A__)
  populateARMv9AFeatures(cpu.features().arm(), 2);
#elif defined(__ARM_ARCH_9_1A__)
  populateARMv9AFeatures(cpu.features().arm(), 1);
#elif defined(__ARM_ARCH_9A__)
  populateARMv9AFeatures(cpu.features().arm(), 0);
#elif defined(__ARM_ARCH_8_9A__)
  populateARMv8AFeatures(cpu.features().arm(), 9);
#elif defined(__ARM_ARCH_8_8A__)
  populateARMv8AFeatures(cpu.features().arm(), 8);
#elif defined(__ARM_ARCH_8_7A__)
  populateARMv8AFeatures(cpu.features().arm(), 7);
#elif defined(__ARM_ARCH_8_6A__)
  populateARMv8AFeatures(cpu.features().arm(), 6);
#elif defined(__ARM_ARCH_8_5A__)
  populateARMv8AFeatures(cpu.features().arm(), 5);
#elif defined(__ARM_ARCH_8_4A__)
  populateARMv8AFeatures(cpu.features().arm(), 4);
#elif defined(__ARM_ARCH_8_3A__)
  populateARMv8AFeatures(cpu.features().arm(), 3);
#elif defined(__ARM_ARCH_8_2A__)
  populateARMv8AFeatures(cpu.features().arm(), 2);
#elif defined(__ARM_ARCH_8_1A__)
  populateARMv8AFeatures(cpu.features().arm(), 1);
#else
  populateARMv8AFeatures(cpu.features().arm(), 0);
#endif

#if defined(__ARM_FEATURE_AES)
  cpu.addFeature(Ext::kAES);
#endif

#if defined(__ARM_FEATURE_BF16_SCALAR_ARITHMETIC) && defined(__ARM_FEATURE_BF16_VECTOR_ARITHMETIC)
  cpu.addFeature(Ext::kBF16);
#endif

#if defined(__ARM_FEATURE_CRC32)
  cpu.addFeature(Ext::kCRC32);
#endif

#if defined(__ARM_FEATURE_CRYPTO)
  cpu.addFeature(Ext::kAES, Ext::kSHA1, Ext::kSHA256);
#endif

#if defined(__ARM_FEATURE_DOTPROD)
  cpu.addFeature(Ext::kDOTPROD);
#endif

#if defined(__ARM_FEATURE_FP16FML) || defined(__ARM_FEATURE_FP16_FML)
  cpu.addFeature(Ext::kFHM);
#endif

#if defined(__ARM_FEATURE_FP16_SCALAR_ARITHMETIC)
  cpu.addFeature(Ext::kFP16);
#endif

#if defined(__ARM_FEATURE_FRINT)
  cpu.addFeature(Ext::kFRINTTS);
#endif

#if defined(__ARM_FEATURE_JCVT)
  cpu.addFeature(Ext::kJSCVT);
#endif

#if defined(__ARM_FEATURE_MATMUL_INT8)
  cpu.addFeature(Ext::kI8MM);
#endif

#if defined(__ARM_FEATURE_ATOMICS)
  cpu.addFeature(Ext::kLSE);
#endif

#if defined(__ARM_FEATURE_MEMORY_TAGGING)
  cpu.addFeature(Ext::kMTE);
#endif

#if defined(__ARM_FEATURE_QRDMX)
  cpu.addFeature(Ext::kRDM);
#endif

#if defined(__ARM_FEATURE_RNG)
  cpu.addFeature(Ext::kRNG);
#endif

#if defined(__ARM_FEATURE_SHA2)
  cpu.addFeature(Ext::kSHA256);
#endif

#if defined(__ARM_FEATURE_SHA3)
  cpu.addFeature(Ext::kSHA3);
#endif

#if defined(__ARM_FEATURE_SHA512)
  cpu.addFeature(Ext::kSHA512);
#endif

#if defined(__ARM_FEATURE_SM3)
  cpu.addFeature(Ext::kSM3);
#endif

#if defined(__ARM_FEATURE_SM4)
  cpu.addFeature(Ext::kSM4);
#endif

#if defined(__ARM_FEATURE_SVE) || defined(__ARM_FEATURE_SVE_VECTOR_OPERATORS)
  cpu.addFeature(Ext::kSVE);
#endif

#if defined(__ARM_FEATURE_SVE_MATMUL_INT8)
  cpu.addFeature(Ext::kSVE_I8MM);
#endif

#if defined(__ARM_FEATURE_SVE_MATMUL_FP32)
  cpu.addFeature(Ext::kSVE_F32MM);
#endif

#if defined(__ARM_FEATURE_SVE_MATMUL_FP64)
  cpu.addFeature(Ext::kSVE_F64MM);
#endif

#if defined(__ARM_FEATURE_SVE2)
  cpu.addFeature(Ext::kSVE2);
#endif

#if defined(__ARM_FEATURE_SVE2_AES)
  cpu.addFeature(Ext::kSVE_AES);
#endif

#if defined(__ARM_FEATURE_SVE2_BITPERM)
  cpu.addFeature(Ext::kSVE_BITPERM);
#endif

#if defined(__ARM_FEATURE_SVE2_SHA3)
  cpu.addFeature(Ext::kSVE_SHA3);
#endif

#if defined(__ARM_FEATURE_SVE2_SM4)
  cpu.addFeature(Ext::kSVE_SM4);
#endif

#if defined(__ARM_FEATURE_TME)
  cpu.addFeature(Ext::kTME);
#endif
}
#endif // ASMJIT_ARCH_ARM == 64

ASMJIT_MAYBE_UNUSED
static ASMJIT_FAVOR_SIZE void detectARMFeaturesViaCompilerFlags(CpuInfo& cpu) noexcept {
#if ASMJIT_ARCH_ARM == 32
  detectAArch32FeaturesViaCompilerFlags(cpu);
#else
  detectAArch64FeaturesViaCompilerFlags(cpu);
#endif // ASMJIT_ARCH_ARM
}

// CpuInfo - Detect - ARM - Post Processing ARM Features
// =====================================================

// Postprocesses AArch32 features.
ASMJIT_MAYBE_UNUSED
static ASMJIT_FAVOR_SIZE void postProcessAArch32Features(CpuFeatures::ARM& features) noexcept {
  DebugUtils::unused(features);
}

// Postprocesses AArch64 features.
//
// The only reason to use this function is to deduce some flags from others.
ASMJIT_MAYBE_UNUSED
static ASMJIT_FAVOR_SIZE void postProcessAArch64Features(CpuFeatures::ARM& features) noexcept {
  if (features.hasFP16())
    features.add(Ext::kFP16CONV);

  if (features.hasMTE3())
    features.add(Ext::kMTE2);

  if (features.hasMTE2())
    features.add(Ext::kMTE);

  if (features.hasSSBS2())
    features.add(Ext::kSSBS);
}

ASMJIT_MAYBE_UNUSED
static ASMJIT_FAVOR_SIZE void postProcessARMCpuInfo(CpuInfo& cpu) noexcept {
#if ASMJIT_ARCH_ARM == 32
  postProcessAArch32Features(cpu.features().arm());
#else
  postProcessAArch64Features(cpu.features().arm());
#endif // ASMJIT_ARCH_ARM
}

// CpuInfo - Detect - ARM - Detect by Reading CPUID Registers
// ==========================================================

// Support CPUID-based detection on AArch64.
#if defined(ASMJIT_ARM_DETECT_VIA_CPUID)

// Since the register ID is encoded with the instruction we have to create a function for each register ID to read.
#define ASMJIT_AARCH64_DEFINE_CPUID_READ_FN(func, regId)  \
ASMJIT_MAYBE_UNUSED                                       \
static inline uint64_t func() noexcept {                  \
  uint64_t output;                                        \
  __asm__ __volatile__("mrs %0, " #regId : "=r"(output)); \
  return output;                                          \
}

// NOTE: Older tools don't know the IDs. For example Ubuntu on RPI (GCC 9) won't compile ID_AA64ISAR2_EL1 in 2023.
ASMJIT_AARCH64_DEFINE_CPUID_READ_FN(aarch64ReadPFR0, ID_AA64PFR0_EL1)
ASMJIT_AARCH64_DEFINE_CPUID_READ_FN(aarch64ReadPFR1, ID_AA64PFR1_EL1)
ASMJIT_AARCH64_DEFINE_CPUID_READ_FN(aarch64ReadISAR0, ID_AA64ISAR0_EL1)
ASMJIT_AARCH64_DEFINE_CPUID_READ_FN(aarch64ReadISAR1, ID_AA64ISAR1_EL1)
ASMJIT_AARCH64_DEFINE_CPUID_READ_FN(aarch64ReadISAR2, S3_0_C0_C6_2) // ID_AA64ISAR2_EL1
ASMJIT_AARCH64_DEFINE_CPUID_READ_FN(aarch64ReadZFR0, S3_0_C0_C4_4) // ID_AA64ZFR0_EL1

#undef ASMJIT_AARCH64_DEFINE_CPUID_READ_FN

// Detects AArch64 features by reading CPUID bits directly from CPUID registers. This is the most reliable method
// as the OS doesn't have to know all supported extensions this way (if there is something missing in HWCAPS then
// there is no way to detect such feature without reading CPUID bits).
//
// This function uses MSR instructions, which means that it reads registers that cannot be read in user-mode. The
// OS typically implements this feature by handling SIGILL internally and providing a filtered content of these
// registers back to the user - at least this is what Linux documentation states - everything implementation
// dependent is zeroed, only the bits that are used for CPU feature identification would be present.
//
// References:
//   - https://docs.kernel.org/arch/arm64/cpu-feature-registers.html
ASMJIT_MAYBE_UNUSED
static ASMJIT_FAVOR_SIZE void detectAArch64FeaturesViaCPUID(CpuInfo& cpu) noexcept {
  populateBaseARMFeatures(cpu);

  detectAArch64FeaturesViaCPUID_AA64PFR0_AA64PFR1(cpu,
    aarch64ReadPFR0(),
    aarch64ReadPFR1());

  detectAArch64FeaturesViaCPUID_AA64ISAR0_AA64ISAR1(cpu,
    aarch64ReadISAR0(),
    aarch64ReadISAR1());

  // TODO: Fix this on FreeBSD - I don't know what kernel version allows to access the registers below...

#if defined(__linux__)
  UNameKernelVersion kVer = getUNameKernelVersion();

  // Introduced in Linux 4.19 by "arm64: add ID_AA64ISAR2_EL1 sys register"), so we want at least 4.20.
  if (kVer.atLeast(4, 20)) {
    detectAArch64FeaturesViaCPUID_AA64ISAR2(cpu, aarch64ReadISAR2());
  }

  // Introduced in Linux 5.10 by "arm64: Expose SVE2 features for userspace", so we want at least 5.11.
  if (kVer.atLeast(5, 11) && cpu.features().arm().hasAny(Ext::kSVE, Ext::kSME)) {
    // Only read CPU_ID_AA64ZFR0 when either SVE or SME is available.
    detectAArch64FeaturesViaCPUID_AA64ZFR0(cpu, aarch64ReadZFR0());
  }
#endif
}

#endif // ASMJIT_ARM_DETECT_VIA_CPUID

// CpuInfo - Detect - ARM - Detect by Windows API
// ==============================================

#if defined(_WIN32)
struct WinPFPMapping {
  uint8_t featureId;
  uint8_t pfpFeatureId;
};

static ASMJIT_FAVOR_SIZE void detectPFPFeatures(CpuInfo& cpu, const WinPFPMapping* mapping, size_t size) noexcept {
  for (size_t i = 0; i < size; i++) {
    if (::IsProcessorFeaturePresent(mapping[i].pfpFeatureId)) {
      cpu.addFeature(mapping[i].featureId);
    }
  }
}

//! Detect ARM CPU features on Windows.
//!
//! The detection is based on `IsProcessorFeaturePresent()` API call.
static ASMJIT_FAVOR_SIZE void detectARMCpu(CpuInfo& cpu) noexcept {
  cpu._wasDetected = true;
  populateBaseARMFeatures(cpu);

  CpuFeatures::ARM& features = cpu.features().arm();

  // Win32 for ARM requires ARMv7 with DSP extensions, VFPv3 (FP), and uses THUMBv2 by default.
#if ASMJIT_ARCH_ARM == 32
  features.add(Ext::kTHUMB);
  features.add(Ext::kTHUMBv2);
  features.add(Ext::kARMv6);
  features.add(Ext::kARMv7);
  features.add(Ext::kEDSP);
#endif

  // Windows for ARM requires FP and ASIMD.
  features.add(Ext::kFP);
  features.add(Ext::kASIMD);

  // Detect additional CPU features by calling `IsProcessorFeaturePresent()`.
  static const WinPFPMapping mapping[] = {
#if ASMJIT_ARCH_ARM == 32
    { uint8_t(Ext::kVFP_D32)  , 18 }, // PF_ARM_VFP_32_REGISTERS_AVAILABLE
    { uint8_t(Ext::kIDIVT)    , 24 }, // PF_ARM_DIVIDE_INSTRUCTION_AVAILABLE
    { uint8_t(Ext::kFMAC)     , 27 }, // PF_ARM_FMAC_INSTRUCTIONS_AVAILABLE
    { uint8_t(Ext::kARMv8a)   , 29 }, // PF_ARM_V8_INSTRUCTIONS_AVAILABLE
#endif
    { uint8_t(Ext::kAES)      , 30 }, // PF_ARM_V8_CRYPTO_INSTRUCTIONS_AVAILABLE
    { uint8_t(Ext::kCRC32)    , 31 }, // PF_ARM_V8_CRC32_INSTRUCTIONS_AVAILABLE
    { uint8_t(Ext::kLSE)      , 34 }, // PF_ARM_V81_ATOMIC_INSTRUCTIONS_AVAILABLE
    { uint8_t(Ext::kDOTPROD)  , 43 }, // PF_ARM_V82_DP_INSTRUCTIONS_AVAILABLE
    { uint8_t(Ext::kJSCVT)    , 44 }, // PF_ARM_V83_JSCVT_INSTRUCTIONS_AVAILABLE
    { uint8_t(Ext::kLRCPC)    , 45 }  // PF_ARM_V83_LRCPC_INSTRUCTIONS_AVAILABLE
  };
  detectPFPFeatures(cpu, mapping, ASMJIT_ARRAY_SIZE(mapping));

  // Windows can only report ARMv8A at the moment.
  if (features.hasARMv8a()) {
    populateARMv8AFeatures(cpu.features().arm(), 0);
  }

  // Windows provides several instructions under a single flag:
  if (features.hasAES()) {
    features.add(Ext::kPMULL, Ext::kSHA1, Ext::kSHA256);
  }

  postProcessARMCpuInfo(cpu);
}

// CpuInfo - Detect - ARM - Detect by Reading HWCAPS
// =================================================

#elif defined(ASMJIT_ARM_DETECT_VIA_HWCAPS)

#ifndef AT_HWCAP
  #define AT_HWCAP 16
#endif // !AT_HWCAP

#ifndef AT_HWCAP2
  #define AT_HWCAP2 26
#endif // !AT_HWCAP2

#if defined(__linux__)
static void getAuxValues(unsigned long* vals, const unsigned long* tags, size_t count) noexcept {
  for (size_t i = 0; i < count; i++) {
    vals[i] = getauxval(tags[i]);
  }
}
#elif defined(__FreeBSD__)
static void getAuxValues(unsigned long* vals, const unsigned long* tags, size_t count) noexcept {
  for (size_t i = 0; i < count; i++) {
    unsigned long result = 0;
    if (elf_aux_info(int(tags[i]), &result, int(sizeof(unsigned long))) != 0)
      result = 0;
    vals[i] = result;
  }
}
#else
#error "[asmjit] getAuxValues() - Unsupported OS."
#endif

struct HWCapMapping {
  uint8_t featureId;
  uint8_t hwCapBit;
};

static const unsigned long hwCapTags[2] = { AT_HWCAP, AT_HWCAP2 };

static ASMJIT_FAVOR_SIZE void mergeHWCaps(CpuInfo& cpu, unsigned long mask, const HWCapMapping* mapping, size_t size) noexcept {
  for (size_t i = 0; i < size; i++) {
    cpu.features().addIf(Support::bitTest(mask, mapping[i].hwCapBit), mapping[i].featureId);
  }
}

#if ASMJIT_ARCH_ARM == 32

// Reference:
//   - https://github.com/torvalds/linux/blob/master/arch/arm/include/uapi/asm/hwcap.h
static const HWCapMapping hwCapMapping[] = {
  { uint8_t(Ext::kEDSP)         , 7  }, // HWCAP_EDSP
  { uint8_t(Ext::kASIMD)        , 12 }, // HWCAP_NEON
  { uint8_t(Ext::kFP)           , 13 }, // HWCAP_VFPv3
  { uint8_t(Ext::kFMAC)         , 16 }, // HWCAP_VFPv4
  { uint8_t(Ext::kIDIVA)        , 17 }, // HWCAP_IDIVA
  { uint8_t(Ext::kIDIVT)        , 18 }, // HWCAP_IDIVT
  { uint8_t(Ext::kVFP_D32)      , 19 }, // HWCAP_VFPD32
  { uint8_t(Ext::kFP16CONV)     , 22 }, // HWCAP_FPHP
  { uint8_t(Ext::kFP16)         , 23 }, // HWCAP_ASIMDHP
  { uint8_t(Ext::kDOTPROD)      , 24 }, // HWCAP_ASIMDDP
  { uint8_t(Ext::kFHM)          , 25 }, // HWCAP_ASIMDFHM
  { uint8_t(Ext::kBF16)         , 26 }, // HWCAP_ASIMDBF16
  { uint8_t(Ext::kI8MM)         , 27 }  // HWCAP_I8MM
};

static const HWCapMapping hwCap2Mapping[] = {
  { uint8_t(Ext::kAES)          , 0  }, // HWCAP2_AES
  { uint8_t(Ext::kPMULL)        , 1  }, // HWCAP2_PMULL
  { uint8_t(Ext::kSHA1)         , 2  }, // HWCAP2_SHA1
  { uint8_t(Ext::kSHA256)       , 3  }, // HWCAP2_SHA2
  { uint8_t(Ext::kCRC32)        , 4  }, // HWCAP2_CRC32
  { uint8_t(Ext::kSB)           , 5  }, // HWCAP2_SB
  { uint8_t(Ext::kSSBS)         , 6  }  // HWCAP2_SSBS
};

static ASMJIT_FAVOR_SIZE void detectARMCpu(CpuInfo& cpu) noexcept {
  cpu._wasDetected = true;
  populateBaseARMFeatures(cpu);

  unsigned long hwCapMasks[2] {};
  getAuxValues(hwCapMasks, hwCapTags, 2u);

  mergeHWCaps(cpu, hwCapMasks[0], hwCapMapping, ASMJIT_ARRAY_SIZE(hwCapMapping));
  mergeHWCaps(cpu, hwCapMasks[1], hwCap2Mapping, ASMJIT_ARRAY_SIZE(hwCap2Mapping));

  CpuFeatures::ARM& features = cpu.features().arm();

  // ARMv7 provides FP|ASIMD.
  if (features.hasFP() || features.hasASIMD())
    features.add(CpuFeatures::ARM::kARMv7);

  // ARMv8 provives AES, CRC32, PMULL, SHA1, and SHA256.
  if (features.hasAES() || features.hasCRC32() || features.hasPMULL() || features.hasSHA1() || features.hasSHA256())
    features.add(CpuFeatures::ARM::kARMv8a);

  postProcessARMCpuInfo(cpu);
}

#else

// Reference:
//   - https://docs.kernel.org/arch/arm64/elf_hwcaps.html
//   - https://github.com/torvalds/linux/blob/master/arch/arm64/include/uapi/asm/hwcap.h
static const HWCapMapping hwCapMapping[] = {
  { uint8_t(Ext::kFP)           , 0  }, // HWCAP_FP
  { uint8_t(Ext::kASIMD)        , 1  }, // HWCAP_ASIMD
  /*
  { uint8_t(Ext::k)             , 2  }, // HWCAP_EVTSTRM
  */
  { uint8_t(Ext::kAES)          , 3  }, // HWCAP_AES
  { uint8_t(Ext::kPMULL)        , 4  }, // HWCAP_PMULL
  { uint8_t(Ext::kSHA1)         , 5  }, // HWCAP_SHA1
  { uint8_t(Ext::kSHA256)       , 6  }, // HWCAP_SHA2
  { uint8_t(Ext::kCRC32)        , 7  }, // HWCAP_CRC32
  { uint8_t(Ext::kLSE)          , 8  }, // HWCAP_ATOMICS
  { uint8_t(Ext::kFP16CONV)     , 9  }, // HWCAP_FPHP
  { uint8_t(Ext::kFP16)         , 10 }, // HWCAP_ASIMDHP
  { uint8_t(Ext::kCPUID)        , 11 }, // HWCAP_CPUID
  { uint8_t(Ext::kRDM)          , 12 }, // HWCAP_ASIMDRDM
  { uint8_t(Ext::kJSCVT)        , 13 }, // HWCAP_JSCVT
  { uint8_t(Ext::kFCMA)         , 14 }, // HWCAP_FCMA
  { uint8_t(Ext::kLRCPC)        , 15 }, // HWCAP_LRCPC
  { uint8_t(Ext::kDPB)          , 16 }, // HWCAP_DCPOP
  { uint8_t(Ext::kSHA3)         , 17 }, // HWCAP_SHA3
  { uint8_t(Ext::kSM3)          , 18 }, // HWCAP_SM3
  { uint8_t(Ext::kSM4)          , 19 }, // HWCAP_SM4
  { uint8_t(Ext::kDOTPROD)      , 20 }, // HWCAP_ASIMDDP
  { uint8_t(Ext::kSHA512)       , 21 }, // HWCAP_SHA512
  { uint8_t(Ext::kSVE)          , 22 }, // HWCAP_SVE
  { uint8_t(Ext::kFHM)          , 23 }, // HWCAP_ASIMDFHM
  { uint8_t(Ext::kDIT)          , 24 }, // HWCAP_DIT
  { uint8_t(Ext::kLSE2)         , 25 }, // HWCAP_USCAT
  { uint8_t(Ext::kLRCPC2)       , 26 }, // HWCAP_ILRCPC
  { uint8_t(Ext::kFLAGM)        , 27 }, // HWCAP_FLAGM
  { uint8_t(Ext::kSSBS)         , 28 }, // HWCAP_SSBS
  { uint8_t(Ext::kSB)           , 29 }  // HWCAP_SB
  /*
  { uint8_t(Ext::k)             , 30 }, // HWCAP_PACA
  { uint8_t(Ext::k)             , 31 }  // HWCAP_PACG
  */
};

static const HWCapMapping hwCap2Mapping[] = {
  { uint8_t(Ext::kDPB2)         , 0  }, // HWCAP2_DCPODP
  { uint8_t(Ext::kSVE2)         , 1  }, // HWCAP2_SVE2
  { uint8_t(Ext::kSVE_AES)      , 2  }, // HWCAP2_SVEAES
  { uint8_t(Ext::kSVE_PMULL128) , 3  }, // HWCAP2_SVEPMULL
  { uint8_t(Ext::kSVE_BITPERM)  , 4  }, // HWCAP2_SVEBITPERM
  { uint8_t(Ext::kSVE_SHA3)     , 5  }, // HWCAP2_SVESHA3
  { uint8_t(Ext::kSVE_SM4)      , 6  }, // HWCAP2_SVESM4
  { uint8_t(Ext::kFLAGM2)       , 7  }, // HWCAP2_FLAGM2
  { uint8_t(Ext::kFRINTTS)      , 8  }, // HWCAP2_FRINT
  { uint8_t(Ext::kSVE_I8MM)     , 9  }, // HWCAP2_SVEI8MM
  { uint8_t(Ext::kSVE_F32MM)    , 10 }, // HWCAP2_SVEF32MM
  { uint8_t(Ext::kSVE_F64MM)    , 11 }, // HWCAP2_SVEF64MM
  { uint8_t(Ext::kSVE_BF16)     , 12 }, // HWCAP2_SVEBF16
  { uint8_t(Ext::kI8MM)         , 13 }, // HWCAP2_I8MM
  { uint8_t(Ext::kBF16)         , 14 }, // HWCAP2_BF16
  { uint8_t(Ext::kDGH)          , 15 }, // HWCAP2_DGH
  { uint8_t(Ext::kRNG)          , 16 }, // HWCAP2_RNG
  { uint8_t(Ext::kBTI)          , 17 }, // HWCAP2_BTI
  { uint8_t(Ext::kMTE)          , 18 }, // HWCAP2_MTE
  { uint8_t(Ext::kECV)          , 19 }, // HWCAP2_ECV
  { uint8_t(Ext::kAFP)          , 20 }, // HWCAP2_AFP
  { uint8_t(Ext::kRPRES)        , 21 }, // HWCAP2_RPRES
  { uint8_t(Ext::kMTE3)         , 22 }, // HWCAP2_MTE3
  { uint8_t(Ext::kSME)          , 23 }, // HWCAP2_SME
  { uint8_t(Ext::kSME_I16I64)   , 24 }, // HWCAP2_SME_I16I64
  { uint8_t(Ext::kSME_F64F64)   , 25 }, // HWCAP2_SME_F64F64
  { uint8_t(Ext::kSME_I8I32)    , 26 }, // HWCAP2_SME_I8I32
  { uint8_t(Ext::kSME_F16F32)   , 27 }, // HWCAP2_SME_F16F32
  { uint8_t(Ext::kSME_B16F32)   , 28 }, // HWCAP2_SME_B16F32
  { uint8_t(Ext::kSME_F32F32)   , 29 }, // HWCAP2_SME_F32F32
  { uint8_t(Ext::kSME_FA64)     , 30 }, // HWCAP2_SME_FA64
  { uint8_t(Ext::kWFXT)         , 31 }, // HWCAP2_WFXT
  { uint8_t(Ext::kEBF16)        , 32 }, // HWCAP2_EBF16
  { uint8_t(Ext::kSVE_EBF16)    , 33 }, // HWCAP2_SVE_EBF16
  { uint8_t(Ext::kCSSC)         , 34 }, // HWCAP2_CSSC
  { uint8_t(Ext::kRPRFM)        , 35 }, // HWCAP2_RPRFM
  { uint8_t(Ext::kSVE2_1)       , 36 }, // HWCAP2_SVE2P1
  { uint8_t(Ext::kSME2)         , 37 }, // HWCAP2_SME2
  { uint8_t(Ext::kSME2_1)       , 38 }, // HWCAP2_SME2P1
  { uint8_t(Ext::kSME_I16I32)   , 39 }, // HWCAP2_SME_I16I32
  { uint8_t(Ext::kSME_BI32I32)  , 40 }, // HWCAP2_SME_BI32I32
  { uint8_t(Ext::kSME_B16B16)   , 41 }, // HWCAP2_SME_B16B16
  { uint8_t(Ext::kSME_F16F16)   , 42 }, // HWCAP2_SME_F16F16
  { uint8_t(Ext::kMOPS)         , 43 }, // HWCAP2_MOPS
  { uint8_t(Ext::kHBC)          , 44 }, // HWCAP2_HBC
  { uint8_t(Ext::kSVE_B16B16)   , 45 }, // HWCAP2_SVE_B16B16
  { uint8_t(Ext::kLRCPC3)       , 46 }, // HWCAP2_LRCPC3
  { uint8_t(Ext::kLSE128)       , 47 }, // HWCAP2_LSE128
};

static ASMJIT_FAVOR_SIZE void detectARMCpu(CpuInfo& cpu) noexcept {
  cpu._wasDetected = true;
  populateBaseARMFeatures(cpu);

  unsigned long hwCapMasks[2] {};
  getAuxValues(hwCapMasks, hwCapTags, 2u);

  mergeHWCaps(cpu, hwCapMasks[0], hwCapMapping, ASMJIT_ARRAY_SIZE(hwCapMapping));
  mergeHWCaps(cpu, hwCapMasks[1], hwCap2Mapping, ASMJIT_ARRAY_SIZE(hwCap2Mapping));

#if defined(ASMJIT_ARM_DETECT_VIA_CPUID)
  if (cpu.features().arm().hasCPUID()) {
    detectAArch64FeaturesViaCPUID(cpu);
    return;
  }
#endif // ASMJIT_ARM_DETECT_VIA_CPUID

  postProcessARMCpuInfo(cpu);
}

#endif // ASMJIT_ARCH_ARM

// CpuInfo - Detect - ARM - Detect by OpenBSD API That Reads CPUID
// ===============================================================

#elif defined(__OpenBSD__) && ASMJIT_ARCH_ARM >= 64

// Supported CPUID registers on OpenBSD (CTL_MACHDEP definitions):
//   - https://github.com/openbsd/src/blob/master/sys/arch/arm64/include/cpu.h
enum class OpenBSDAArch64CPUID {
  kAA64ISAR0 = 2,
  kAA64ISAR1 = 3,
  kAA64ISAR2 = 4,
  kAA64MMFR0 = 5,
  kAA64MMFR1 = 6,
  kAA64MMFR2 = 7,
  kAA64PFR0 = 8,
  kAA64PFR1 = 9,
  kAA64SMFR0 = 10,
  kAA64ZFR0 = 11
};

static uint64_t openbsdReadAArch64CPUID(OpenBSDAArch64CPUID id) noexcept {
  uint64_t bits = 0;
  size_t size = sizeof(bits);
  int name[2] = { CTL_MACHDEP, int(id) };

  return (sysctl(name, 2, &bits, &size, NULL, 0) < 0) ? uint64_t(0) : bits;
}

static ASMJIT_FAVOR_SIZE void detectARMCpu(CpuInfo& cpu) noexcept {
  typedef OpenBSDAArch64CPUID ID;

  populateBaseARMFeatures(cpu);

  detectAArch64FeaturesViaCPUID_AA64PFR0_AA64PFR1(cpu,
    openbsdReadAArch64CPUID(ID::kAA64PFR0),
    openbsdReadAArch64CPUID(ID::kAA64PFR1));

  detectAArch64FeaturesViaCPUID_AA64ISAR0_AA64ISAR1(cpu,
    openbsdReadAArch64CPUID(ID::kAA64ISAR0),
    openbsdReadAArch64CPUID(ID::kAA64ISAR1));

  detectAArch64FeaturesViaCPUID_AA64ISAR2(cpu, openbsdReadAArch64CPUID(ID::kAA64ISAR2));
  detectAArch64FeaturesViaCPUID_AA64MMFR0(cpu, openbsdReadAArch64CPUID(ID::kAA64MMFR0));
  detectAArch64FeaturesViaCPUID_AA64MMFR1(cpu, openbsdReadAArch64CPUID(ID::kAA64MMFR1));
  detectAArch64FeaturesViaCPUID_AA64MMFR2(cpu, openbsdReadAArch64CPUID(ID::kAA64MMFR2));

  // Only read CPU_ID_AA64ZFR0 when either SVE or SME is available.
  if (cpu.features().arm().hasAny(Ext::kSVE, Ext::kSME)) {
    detectAArch64FeaturesViaCPUID_AA64ZFR0(cpu, openbsdReadAArch64CPUID(ID::kAA64ZFR0));

    if (cpu.features().arm().hasSME())
      detectAArch64FeaturesViaCPUID_AA64SMFR0(cpu, openbsdReadAArch64CPUID(ID::kAA64SMFR0));
  }

  postProcessARMCpuInfo(cpu);
}

// CpuInfo - Detect - ARM - Detect by Apple API (sysctlbyname)
// ===========================================================

#elif defined(__APPLE__)

enum class AppleFeatureType : uint8_t {
  kHWOptional,
  kHWOptionalArmFEAT
};

struct AppleFeatureMapping {
  AppleFeatureType type;
  char name[18];
  uint8_t featureId;
};

template<typename T>
static inline bool appleSysctlByName(const char* sysctlName, T* dst, size_t size = sizeof(T)) noexcept {
  return sysctlbyname(sysctlName, dst, &size, nullptr, 0) == 0;
}

static ASMJIT_FAVOR_SIZE long appleDetectARMFeatureViaSysctl(AppleFeatureType type, const char* featureName) noexcept {
  static const char hwOptionalPrefix[] = "hw.optional.";
  static const char hwOptionalArmFeatPrefix[] = "hw.optional.arm.FEAT_";

  char sysctlName[128];

  const char* prefix = type == AppleFeatureType::kHWOptional ? hwOptionalPrefix : hwOptionalArmFeatPrefix;
  size_t prefixSize = (type == AppleFeatureType::kHWOptional ? sizeof(hwOptionalPrefix) : sizeof(hwOptionalArmFeatPrefix)) - 1u;
  size_t featureNameSize = strlen(featureName);

  if (featureNameSize < 128 - prefixSize) {
    memcpy(sysctlName, prefix, prefixSize);
    memcpy(sysctlName + prefixSize, featureName, featureNameSize + 1u); // Include NULL terminator.

    long val = 0;
    if (appleSysctlByName<long>(sysctlName, &val))
      return val;
  }

  return 0;
}

static ASMJIT_FAVOR_SIZE void appleDetectARMFeaturesViaSysctl(CpuInfo& cpu) noexcept {
  typedef AppleFeatureType FT;

  // Based on:
  //   - https://developer.apple.com/documentation/kernel/1387446-sysctlbyname/determining_instruction_set_characteristics
  static const AppleFeatureMapping mappings[] = {
    // Determine Advanced SIMD and Floating Point Capabilities:
    { FT::kHWOptional       , "AdvSIMD_HPFPCvt", uint8_t(Ext::kFP16CONV)  },
    { FT::kHWOptional       , "neon_hpfp"      , uint8_t(Ext::kFP16CONV)  },
    { FT::kHWOptionalArmFEAT, "BF16"           , uint8_t(Ext::kBF16)      },
    { FT::kHWOptionalArmFEAT, "DotProd"        , uint8_t(Ext::kDOTPROD)   },
    { FT::kHWOptionalArmFEAT, "FCMA"           , uint8_t(Ext::kFCMA)      },
    { FT::kHWOptional       , "armv8_3_compnum", uint8_t(Ext::kFCMA)      },
    { FT::kHWOptionalArmFEAT, "FHM"            , uint8_t(Ext::kFHM)       },
    { FT::kHWOptional       , "armv8_2_fhm"    , uint8_t(Ext::kFHM)       },
    { FT::kHWOptionalArmFEAT, "FP16"           , uint8_t(Ext::kFP16)      },
    { FT::kHWOptional       , "neon_fp16"      , uint8_t(Ext::kFP16)      },
    { FT::kHWOptionalArmFEAT, "FRINTTS"        , uint8_t(Ext::kFRINTTS)   },
    { FT::kHWOptionalArmFEAT, "I8MM"           , uint8_t(Ext::kI8MM)      },
    { FT::kHWOptionalArmFEAT, "JSCVT"          , uint8_t(Ext::kJSCVT)     },
    { FT::kHWOptionalArmFEAT, "RDM"            , uint8_t(Ext::kRDM)       },

    // Determine Integer Capabilities:
    { FT::kHWOptional       , "armv8_crc32"    , uint8_t(Ext::kCRC32)     },
    { FT::kHWOptionalArmFEAT, "FlagM"          , uint8_t(Ext::kFLAGM)     },
    { FT::kHWOptionalArmFEAT, "FlagM2"         , uint8_t(Ext::kFLAGM2)    },

    // Determine Atomic and Memory Ordering Instruction Capabilities:
    { FT::kHWOptionalArmFEAT, "LRCPC"          , uint8_t(Ext::kLRCPC)     },
    { FT::kHWOptionalArmFEAT, "LRCPC2"         , uint8_t(Ext::kLRCPC2)    },
    { FT::kHWOptional       , "armv8_1_atomics", uint8_t(Ext::kLSE)       },
    { FT::kHWOptionalArmFEAT, "LSE"            , uint8_t(Ext::kLSE)       },
    { FT::kHWOptionalArmFEAT, "LSE2"           , uint8_t(Ext::kLSE2)      },

    // Determine Encryption Capabilities:
    { FT::kHWOptionalArmFEAT, "AES"            , uint8_t(Ext::kAES)       },
    { FT::kHWOptionalArmFEAT, "PMULL"          , uint8_t(Ext::kPMULL)     },
    { FT::kHWOptionalArmFEAT, "SHA1"           , uint8_t(Ext::kSHA1)      },
    { FT::kHWOptionalArmFEAT, "SHA256"         , uint8_t(Ext::kSHA256)    },
    { FT::kHWOptionalArmFEAT, "SHA512"         , uint8_t(Ext::kSHA512)    },
    { FT::kHWOptional       , "armv8_2_sha512" , uint8_t(Ext::kSHA512)    },
    { FT::kHWOptionalArmFEAT, "SHA3"           , uint8_t(Ext::kSHA3)      },
    { FT::kHWOptional       , "armv8_2_sha3"   , uint8_t(Ext::kSHA3)      },

    // Determine General Capabilities:
    { FT::kHWOptionalArmFEAT, "BTI"            , uint8_t(Ext::kBTI)       },
    { FT::kHWOptionalArmFEAT, "DPB"            , uint8_t(Ext::kDPB)       },
    { FT::kHWOptionalArmFEAT, "DPB2"           , uint8_t(Ext::kDPB2)      },
    { FT::kHWOptionalArmFEAT, "ECV"            , uint8_t(Ext::kECV)       },
    { FT::kHWOptionalArmFEAT, "SB"             , uint8_t(Ext::kSB)        },
    { FT::kHWOptionalArmFEAT, "SSBS"           , uint8_t(Ext::kSSBS)      }
  };

  for (size_t i = 0; i < ASMJIT_ARRAY_SIZE(mappings); i++) {
    const AppleFeatureMapping& mapping = mappings[i];
    if (!cpu.features().arm().has(mapping.featureId) && appleDetectARMFeatureViaSysctl(mapping.type, mapping.name)) {
      cpu.features().arm().add(mapping.featureId);
    }
  }
}

static ASMJIT_FAVOR_SIZE void detectARMCpu(CpuInfo& cpu) noexcept {
  cpu._wasDetected = true;
  populateBaseARMFeatures(cpu);

  appleSysctlByName<uint32_t>("hw.cpufamily", &cpu._familyId);
  appleSysctlByName<uint32_t>("hw.cachelinesize", &cpu._cacheLineSize);
  appleSysctlByName<uint32_t>("machdep.cpu.logical_per_package", &cpu._maxLogicalProcessors);
  appleSysctlByName<char>("machdep.cpu.brand_string", cpu._brand.str, sizeof(cpu._brand.str));

  memcpy(cpu._vendor.str, "APPLE", 6);

  bool cpuFeaturesPopulated = detectARMFeaturesViaAppleFamilyId(cpu);
  if (!cpuFeaturesPopulated)
    appleDetectARMFeaturesViaSysctl(cpu);
  postProcessARMCpuInfo(cpu);
}

// CpuInfo - Detect - ARM - Detect by Fallback (Using Compiler Flags)
// ==================================================================

#else

#if ASMJIT_ARCH_ARM == 32
  #pragma message("[asmjit] Disabling runtime CPU detection - unsupported OS/CPU combination (Unknown OS with AArch32 CPU)")
#else
  #pragma message("[asmjit] Disabling runtime CPU detection - unsupported OS/CPU combination (Unknown OS with AArch64 CPU)")
#endif

static ASMJIT_FAVOR_SIZE void detectARMCpu(CpuInfo& cpu) noexcept {
  populateBaseARMFeatures(cpu);
  detectARMFeaturesViaCompilerFlags(cpu);
  postProcessARMCpuInfo(cpu);
}
#endif

} // {arm}

#endif

// CpuInfo - Detect - Host
// =======================

const CpuInfo& CpuInfo::host() noexcept {
  static std::atomic<uint32_t> cpuInfoInitialized;
  static CpuInfo cpuInfoGlobal(Globals::NoInit);

  // This should never cause a problem as the resulting information should always
  // be the same. In the worst case it would just be overwritten non-atomically.
  if (!cpuInfoInitialized.load(std::memory_order_relaxed)) {
    CpuInfo cpuInfoLocal;

    cpuInfoLocal._arch = Arch::kHost;
    cpuInfoLocal._subArch = SubArch::kHost;

#if ASMJIT_ARCH_X86
    x86::detectX86Cpu(cpuInfoLocal);
#elif ASMJIT_ARCH_ARM
    arm::detectARMCpu(cpuInfoLocal);
#endif

    cpuInfoLocal._hwThreadCount = detectHWThreadCount();
    cpuInfoGlobal = cpuInfoLocal;
    cpuInfoInitialized.store(1, std::memory_order_seq_cst);
  }

  return cpuInfoGlobal;
}

#if defined(_MSC_VER)
  #pragma warning(pop)
#endif // _MSC_VER

ASMJIT_END_NAMESPACE
