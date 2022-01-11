// This file is part of AsmJit project <https://asmjit.com>
//
// See asmjit.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#include "../core/api-build_p.h"
#include "../core/cpuinfo.h"
#include "../core/support.h"

#if !defined(_WIN32)
  #include <errno.h>
  #include <sys/utsname.h>
  #include <unistd.h>
#endif

// Required by `getauxval()` on Linux.
#if defined(__linux__)
  #include <sys/auxv.h>
#endif

//! Required to detect CPU and features on Apple platforms.
#if defined(__APPLE__)
  #include <mach/machine.h>
  #include <sys/types.h>
  #include <sys/sysctl.h>
#endif

// Required by `__cpuidex()` and `_xgetbv()`.
#if defined(_MSC_VER)
  #include <intrin.h>
#endif

ASMJIT_BEGIN_NAMESPACE

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

#if ASMJIT_ARCH_X86

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

    if (!(c == ' ' && (prev == '@' || s[1] == ' ' || s[1] == '@'))) {
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
  features.add(CpuFeatures::X86::kI486);

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

    cpu._modelId              = modelId;
    cpu._familyId             = familyId;
    cpu._brandId              = ((regs.ebx      ) & 0xFF);
    cpu._processorType        = ((regs.eax >> 12) & 0x03);
    cpu._maxLogicalProcessors = ((regs.ebx >> 16) & 0xFF);
    cpu._stepping             = ((regs.eax      ) & 0x0F);
    cpu._cacheLineSize        = ((regs.ebx >>  8) & 0xFF) * 8;

    features.addIf(bitTest(regs.ecx,  0), CpuFeatures::X86::kSSE3);
    features.addIf(bitTest(regs.ecx,  1), CpuFeatures::X86::kPCLMULQDQ);
    features.addIf(bitTest(regs.ecx,  3), CpuFeatures::X86::kMONITOR);
    features.addIf(bitTest(regs.ecx,  5), CpuFeatures::X86::kVMX);
    features.addIf(bitTest(regs.ecx,  6), CpuFeatures::X86::kSMX);
    features.addIf(bitTest(regs.ecx,  9), CpuFeatures::X86::kSSSE3);
    features.addIf(bitTest(regs.ecx, 13), CpuFeatures::X86::kCMPXCHG16B);
    features.addIf(bitTest(regs.ecx, 19), CpuFeatures::X86::kSSE4_1);
    features.addIf(bitTest(regs.ecx, 20), CpuFeatures::X86::kSSE4_2);
    features.addIf(bitTest(regs.ecx, 22), CpuFeatures::X86::kMOVBE);
    features.addIf(bitTest(regs.ecx, 23), CpuFeatures::X86::kPOPCNT);
    features.addIf(bitTest(regs.ecx, 25), CpuFeatures::X86::kAESNI);
    features.addIf(bitTest(regs.ecx, 26), CpuFeatures::X86::kXSAVE);
    features.addIf(bitTest(regs.ecx, 27), CpuFeatures::X86::kOSXSAVE);
    features.addIf(bitTest(regs.ecx, 30), CpuFeatures::X86::kRDRAND);
    features.addIf(bitTest(regs.edx,  0), CpuFeatures::X86::kFPU);
    features.addIf(bitTest(regs.edx,  4), CpuFeatures::X86::kRDTSC);
    features.addIf(bitTest(regs.edx,  5), CpuFeatures::X86::kMSR);
    features.addIf(bitTest(regs.edx,  8), CpuFeatures::X86::kCMPXCHG8B);
    features.addIf(bitTest(regs.edx, 15), CpuFeatures::X86::kCMOV);
    features.addIf(bitTest(regs.edx, 19), CpuFeatures::X86::kCLFLUSH);
    features.addIf(bitTest(regs.edx, 23), CpuFeatures::X86::kMMX);
    features.addIf(bitTest(regs.edx, 24), CpuFeatures::X86::kFXSR);
    features.addIf(bitTest(regs.edx, 25), CpuFeatures::X86::kSSE);
    features.addIf(bitTest(regs.edx, 25), CpuFeatures::X86::kSSE, CpuFeatures::X86::kSSE2);
    features.addIf(bitTest(regs.edx, 28), CpuFeatures::X86::kMT);

    // Get the content of XCR0 if supported by the CPU and enabled by the OS.
    if (features.hasXSAVE() && features.hasOSXSAVE()) {
      xgetbvQuery(&xcr0, 0);
    }

    // Detect AVX+.
    if (bitTest(regs.ecx, 28)) {
      // - XCR0[2:1] == 11b
      //   XMM & YMM states need to be enabled by OS.
      if ((xcr0.eax & 0x00000006u) == 0x00000006u) {
        features.add(CpuFeatures::X86::kAVX);
        features.addIf(bitTest(regs.ecx, 12), CpuFeatures::X86::kFMA);
        features.addIf(bitTest(regs.ecx, 29), CpuFeatures::X86::kF16C);
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

    features.addIf(bitTest(regs.ebx,  0), CpuFeatures::X86::kFSGSBASE);
    features.addIf(bitTest(regs.ebx,  3), CpuFeatures::X86::kBMI);
    features.addIf(bitTest(regs.ebx,  4), CpuFeatures::X86::kHLE);
    features.addIf(bitTest(regs.ebx,  7), CpuFeatures::X86::kSMEP);
    features.addIf(bitTest(regs.ebx,  8), CpuFeatures::X86::kBMI2);
    features.addIf(bitTest(regs.ebx,  9), CpuFeatures::X86::kERMS);
    features.addIf(bitTest(regs.ebx, 11), CpuFeatures::X86::kRTM);
    features.addIf(bitTest(regs.ebx, 18), CpuFeatures::X86::kRDSEED);
    features.addIf(bitTest(regs.ebx, 19), CpuFeatures::X86::kADX);
    features.addIf(bitTest(regs.ebx, 20), CpuFeatures::X86::kSMAP);
    features.addIf(bitTest(regs.ebx, 23), CpuFeatures::X86::kCLFLUSHOPT);
    features.addIf(bitTest(regs.ebx, 24), CpuFeatures::X86::kCLWB);
    features.addIf(bitTest(regs.ebx, 29), CpuFeatures::X86::kSHA);
    features.addIf(bitTest(regs.ecx,  0), CpuFeatures::X86::kPREFETCHWT1);
    features.addIf(bitTest(regs.ecx,  4), CpuFeatures::X86::kOSPKE);
    features.addIf(bitTest(regs.ecx,  5), CpuFeatures::X86::kWAITPKG);
    features.addIf(bitTest(regs.ecx,  7), CpuFeatures::X86::kCET_SS);
    features.addIf(bitTest(regs.ecx,  8), CpuFeatures::X86::kGFNI);
    features.addIf(bitTest(regs.ecx,  9), CpuFeatures::X86::kVAES);
    features.addIf(bitTest(regs.ecx, 10), CpuFeatures::X86::kVPCLMULQDQ);
    features.addIf(bitTest(regs.ecx, 22), CpuFeatures::X86::kRDPID);
    features.addIf(bitTest(regs.ecx, 25), CpuFeatures::X86::kCLDEMOTE);
    features.addIf(bitTest(regs.ecx, 27), CpuFeatures::X86::kMOVDIRI);
    features.addIf(bitTest(regs.ecx, 28), CpuFeatures::X86::kMOVDIR64B);
    features.addIf(bitTest(regs.ecx, 29), CpuFeatures::X86::kENQCMD);
    features.addIf(bitTest(regs.edx,  5), CpuFeatures::X86::kUINTR);
    features.addIf(bitTest(regs.edx, 14), CpuFeatures::X86::kSERIALIZE);
    features.addIf(bitTest(regs.edx, 16), CpuFeatures::X86::kTSXLDTRK);
    features.addIf(bitTest(regs.edx, 18), CpuFeatures::X86::kPCONFIG);
    features.addIf(bitTest(regs.edx, 20), CpuFeatures::X86::kCET_IBT);

    // Detect 'TSX' - Requires at least one of `HLE` and `RTM` features.
    if (features.hasHLE() || features.hasRTM())
      features.add(CpuFeatures::X86::kTSX);

    // Detect 'AVX2' - Requires AVX as well.
    if (bitTest(regs.ebx, 5) && features.hasAVX())
      features.add(CpuFeatures::X86::kAVX2);

    // Detect 'AVX512'.
    if (avx512EnabledByOS && bitTest(regs.ebx, 16)) {
      features.add(CpuFeatures::X86::kAVX512_F);

      features.addIf(bitTest(regs.ebx, 17), CpuFeatures::X86::kAVX512_DQ);
      features.addIf(bitTest(regs.ebx, 21), CpuFeatures::X86::kAVX512_IFMA);
      features.addIf(bitTest(regs.ebx, 26), CpuFeatures::X86::kAVX512_PFI);
      features.addIf(bitTest(regs.ebx, 27), CpuFeatures::X86::kAVX512_ERI);
      features.addIf(bitTest(regs.ebx, 28), CpuFeatures::X86::kAVX512_CDI);
      features.addIf(bitTest(regs.ebx, 30), CpuFeatures::X86::kAVX512_BW);
      features.addIf(bitTest(regs.ebx, 31), CpuFeatures::X86::kAVX512_VL);
      features.addIf(bitTest(regs.ecx,  1), CpuFeatures::X86::kAVX512_VBMI);
      features.addIf(bitTest(regs.ecx,  6), CpuFeatures::X86::kAVX512_VBMI2);
      features.addIf(bitTest(regs.ecx, 11), CpuFeatures::X86::kAVX512_VNNI);
      features.addIf(bitTest(regs.ecx, 12), CpuFeatures::X86::kAVX512_BITALG);
      features.addIf(bitTest(regs.ecx, 14), CpuFeatures::X86::kAVX512_VPOPCNTDQ);
      features.addIf(bitTest(regs.edx,  2), CpuFeatures::X86::kAVX512_4VNNIW);
      features.addIf(bitTest(regs.edx,  3), CpuFeatures::X86::kAVX512_4FMAPS);
      features.addIf(bitTest(regs.edx,  8), CpuFeatures::X86::kAVX512_VP2INTERSECT);
      features.addIf(bitTest(regs.edx, 23), CpuFeatures::X86::kAVX512_FP16);
    }

    // Detect 'AMX'.
    if (amxEnabledByOS) {
      features.addIf(bitTest(regs.edx, 22), CpuFeatures::X86::kAMX_BF16);
      features.addIf(bitTest(regs.edx, 24), CpuFeatures::X86::kAMX_TILE);
      features.addIf(bitTest(regs.edx, 25), CpuFeatures::X86::kAMX_INT8);
    }
  }

  // CPUID EAX=7 ECX=1
  // -----------------

  if (features.hasAVX512_F() && maxSubLeafId_0x7 >= 1) {
    cpuidQuery(&regs, 0x7, 1);

    features.addIf(bitTest(regs.eax,  3), CpuFeatures::X86::kAVX_VNNI);
    features.addIf(bitTest(regs.eax,  5), CpuFeatures::X86::kAVX512_BF16);
    features.addIf(bitTest(regs.eax, 22), CpuFeatures::X86::kHRESET);
  }

  // CPUID EAX=13 ECX=0
  // ------------------

  if (maxId >= 0xD) {
    cpuidQuery(&regs, 0xD, 0);

    // Both CPUID result and XCR0 has to be enabled to have support for MPX.
    if (((regs.eax & xcr0.eax) & 0x00000018u) == 0x00000018u && maybeMPX)
      features.add(CpuFeatures::X86::kMPX);

    cpuidQuery(&regs, 0xD, 1);

    features.addIf(bitTest(regs.eax, 0), CpuFeatures::X86::kXSAVEOPT);
    features.addIf(bitTest(regs.eax, 1), CpuFeatures::X86::kXSAVEC);
    features.addIf(bitTest(regs.eax, 3), CpuFeatures::X86::kXSAVES);
  }

  // CPUID EAX=14 ECX=0
  // ------------------

  if (maxId >= 0xE) {
    cpuidQuery(&regs, 0xE, 0);

    features.addIf(bitTest(regs.ebx, 4), CpuFeatures::X86::kPTWRITE);
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
        features.addIf(bitTest(regs.ecx,  0), CpuFeatures::X86::kLAHFSAHF);
        features.addIf(bitTest(regs.ecx,  2), CpuFeatures::X86::kSVM);
        features.addIf(bitTest(regs.ecx,  5), CpuFeatures::X86::kLZCNT);
        features.addIf(bitTest(regs.ecx,  6), CpuFeatures::X86::kSSE4A);
        features.addIf(bitTest(regs.ecx,  7), CpuFeatures::X86::kMSSE);
        features.addIf(bitTest(regs.ecx,  8), CpuFeatures::X86::kPREFETCHW);
        features.addIf(bitTest(regs.ecx, 12), CpuFeatures::X86::kSKINIT);
        features.addIf(bitTest(regs.ecx, 15), CpuFeatures::X86::kLWP);
        features.addIf(bitTest(regs.ecx, 21), CpuFeatures::X86::kTBM);
        features.addIf(bitTest(regs.ecx, 29), CpuFeatures::X86::kMONITORX);
        features.addIf(bitTest(regs.edx, 20), CpuFeatures::X86::kNX);
        features.addIf(bitTest(regs.edx, 21), CpuFeatures::X86::kFXSROPT);
        features.addIf(bitTest(regs.edx, 22), CpuFeatures::X86::kMMX2);
        features.addIf(bitTest(regs.edx, 27), CpuFeatures::X86::kRDTSCP);
        features.addIf(bitTest(regs.edx, 29), CpuFeatures::X86::kPREFETCHW);
        features.addIf(bitTest(regs.edx, 30), CpuFeatures::X86::k3DNOW2, CpuFeatures::X86::kMMX2);
        features.addIf(bitTest(regs.edx, 31), CpuFeatures::X86::kPREFETCHW);

        if (features.hasAVX()) {
          features.addIf(bitTest(regs.ecx, 11), CpuFeatures::X86::kXOP);
          features.addIf(bitTest(regs.ecx, 16), CpuFeatures::X86::kFMA4);
        }

        // This feature seems to be only supported by AMD.
        if (cpu.isVendor("AMD")) {
          features.addIf(bitTest(regs.ecx,  4), CpuFeatures::X86::kALTMOVCR8);
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
        features.addIf(bitTest(regs.ebx,  0), CpuFeatures::X86::kCLZERO);
        features.addIf(bitTest(regs.ebx,  0), CpuFeatures::X86::kRDPRU);
        features.addIf(bitTest(regs.ebx,  8), CpuFeatures::X86::kMCOMMIT);
        features.addIf(bitTest(regs.ebx,  9), CpuFeatures::X86::kWBNOINVD);

        // Go directly to the next one we are interested in.
        i = 0x8000001Fu - 1;
        break;

      case 0x8000001Fu:
        features.addIf(bitTest(regs.eax,  4), CpuFeatures::X86::kSNP);
        break;
    }
  } while (++i <= maxId);

  // Simplify CPU brand string a bit by removing some unnecessary spaces.
  simplifyCpuBrand(cpu._brand.str);
}

#endif // ASMJIT_ARCH_X86

// CpuInfo - Detect - ARM
// ======================

// The most relevant and accurate information can be found here:
//   https://github.com/llvm-project/llvm/blob/master/lib/Target/AArch64/AArch64.td
//   https://github.com/apple/llvm-project/blob/apple/main/llvm/lib/Target/AArch64/AArch64.td (Apple fork)
//
// Other resources:
//   https://en.wikipedia.org/wiki/AArch64
//   https://en.wikipedia.org/wiki/Apple_silicon#List_of_Apple_processors
//   https://developer.arm.com/architectures/learn-the-architecture/understanding-the-armv8-x-extensions/single-page

#if ASMJIT_ARCH_ARM

static inline void populateBaseARMFeatures(CpuInfo& cpu) noexcept {
#if ASMJIT_ARCH_ARM == 32
  // No baseline flags at the moment.
  DebugUtils::unused(cpu);
#else
  // AArch64 is based on ARMv8-A and later.
  cpu.addFeature(CpuFeatures::ARM::kARMv6);
  cpu.addFeature(CpuFeatures::ARM::kARMv7);
  cpu.addFeature(CpuFeatures::ARM::kARMv8a);

  // AArch64 comes with these features by default.
  cpu.addFeature(CpuFeatures::ARM::kVFPv2);
  cpu.addFeature(CpuFeatures::ARM::kVFPv3);
  cpu.addFeature(CpuFeatures::ARM::kVFPv4);
  cpu.addFeature(CpuFeatures::ARM::kASIMD);
  cpu.addFeature(CpuFeatures::ARM::kIDIVA);
#endif
}

// Detects ARM version by macros defined at compile time. This means that AsmJit will report features forced at
// compile time that should always be provided by the target CPU. This also means that if we don't provide any
// means to detect CPU features the features reported by AsmJit will at least not report less features than the
// target it was compiled to.
ASMJIT_MAYBE_UNUSED
static ASMJIT_FAVOR_SIZE void detectARMFeaturesViaCompilerFlags(CpuInfo& cpu) noexcept {
  DebugUtils::unused(cpu);

#if ASMJIT_ARCH_ARM == 32

  // ARM targets have no baseline at the moment.
# if defined(__ARM_ARCH_7A__)
  cpu.addFeature(CpuFeatures::ARM::kARMv7);
# endif
# if defined(__ARM_ARCH_8A__)
  cpu.addFeature(CpuFeatures::ARM::kARMv8a);
# endif

# if defined(__TARGET_ARCH_THUMB)
  cpu.addFeature(CpuFeatures::ARM::kTHUMB);
# if __TARGET_ARCH_THUMB >= 4
  cpu.addFeature(CpuFeatures::ARM::kTHUMBv2);
# endif
# endif

# if defined(__ARM_FEATURE_FMA)
  cpu.addFeature(CpuFeatures::ARM::kVFPv3);
  cpu.addFeature(CpuFeatures::ARM::kVFPv4);
# endif

# if defined(__ARM_NEON)
  cpu.addFeature(CpuFeatures::ARM::kASIMD);
# endif

# if defined(__ARM_FEATURE_IDIV) && defined(__TARGET_ARCH_THUMB)
  cpu.addFeature(CpuFeatures::ARM::kIDIVT);
#endif
# if defined(__ARM_FEATURE_IDIV) && !defined(__TARGET_ARCH_THUMB)
  cpu.addFeature(CpuFeatures::ARM::kIDIVA);
# endif

#endif

#if defined(__ARM_ARCH_8_1A__)
  cpu.addFeature(CpuFeatures::ARM::kARMv8_1a);
#endif
#if defined(__ARM_ARCH_8_2A__)
  cpu.addFeature(CpuFeatures::ARM::kARMv8_2a);
#endif
#if defined(__ARM_ARCH_8_3A__)
  cpu.addFeature(CpuFeatures::ARM::kARMv8_3a);
#endif
#if defined(__ARM_ARCH_8_4A__)
  cpu.addFeature(CpuFeatures::ARM::kARMv8_4a);
#endif
#if defined(__ARM_ARCH_8_5A__)
  cpu.addFeature(CpuFeatures::ARM::kARMv8_5a);
#endif
#if defined(__ARM_ARCH_8_6A__)
  cpu.addFeature(CpuFeatures::ARM::kARMv8_6a);
#endif
#if defined(__ARM_ARCH_8_7A__)
  cpu.addFeature(CpuFeatures::ARM::kARMv8_7a);
#endif

#if defined(__ARM_FEATURE_AES)
  cpu.addFeature(CpuFeatures::ARM::kAES);
#endif

#if defined(__ARM_FEATURE_BF16_SCALAR_ARITHMETIC) && defined(__ARM_FEATURE_BF16_VECTOR_ARITHMETIC)
  cpu.addFeature(CpuFeatures::ARM::kBF16);
#endif

#if defined(__ARM_FEATURE_CRC32)
  cpu.addFeature(CpuFeatures::ARM::kCRC32);
#endif

#if defined(__ARM_FEATURE_CRYPTO)
  cpu.addFeature(CpuFeatures::ARM::kAES,
                 CpuFeatures::ARM::kSHA1,
                 CpuFeatures::ARM::kSHA2);
#endif

#if defined(__ARM_FEATURE_DOTPROD)
  cpu.addFeature(CpuFeatures::ARM::kDOTPROD);
#endif

#if defined(__ARM_FEATURE_FP16FML) || defined(__ARM_FEATURE_FP16_FML)
  cpu.addFeature(CpuFeatures::ARM::kFP16FML);
#endif

#if defined(__ARM_FEATURE_FP16_SCALAR_ARITHMETIC)
  cpu.addFeature(CpuFeatures::ARM::kFP16FULL);
#endif

#if defined(__ARM_FEATURE_FRINT)
  cpu.addFeature(CpuFeatures::ARM::kFRINT);
#endif

#if defined(__ARM_FEATURE_JCVT)
  cpu.addFeature(CpuFeatures::ARM::kFJCVTZS);
#endif

#if defined(__ARM_FEATURE_MATMUL_INT8)
  cpu.addFeature(CpuFeatures::ARM::kI8MM);
#endif

#if defined(__ARM_FEATURE_ATOMICS)
  cpu.addFeature(CpuFeatures::ARM::kLSE);
#endif

#if defined(__ARM_FEATURE_MEMORY_TAGGING)
  cpu.addFeature(CpuFeatures::ARM::kMTE);
#endif

#if defined(__ARM_FEATURE_QRDMX)
  cpu.addFeature(CpuFeatures::ARM::kRDM);
#endif

#if defined(__ARM_FEATURE_RNG)
  cpu.addFeature(CpuFeatures::ARM::kRNG);
#endif

#if defined(__ARM_FEATURE_SHA2)
  cpu.addFeature(CpuFeatures::ARM::kSHA2);
#endif

#if defined(__ARM_FEATURE_SHA3)
  cpu.addFeature(CpuFeatures::ARM::kSHA3);
#endif

#if defined(__ARM_FEATURE_SHA512)
  cpu.addFeature(CpuFeatures::ARM::kSHA512);
#endif

#if defined(__ARM_FEATURE_SM3)
  cpu.addFeature(CpuFeatures::ARM::kSM3);
#endif

#if defined(__ARM_FEATURE_SM4)
  cpu.addFeature(CpuFeatures::ARM::kSM4);
#endif

#if defined(__ARM_FEATURE_SVE) || defined(__ARM_FEATURE_SVE_VECTOR_OPERATORS)
  cpu.addFeature(CpuFeatures::ARM::kSVE);
#endif

#if defined(__ARM_FEATURE_SVE_MATMUL_INT8)
  cpu.addFeature(CpuFeatures::ARM::kSVE_I8MM);
#endif

#if defined(__ARM_FEATURE_SVE_MATMUL_FP32)
  cpu.addFeature(CpuFeatures::ARM::kSVE_F32MM);
#endif

#if defined(__ARM_FEATURE_SVE_MATMUL_FP64)
  cpu.addFeature(CpuFeatures::ARM::kSVE_F64MM);
#endif

#if defined(__ARM_FEATURE_SVE2)
  cpu.addFeature(CpuFeatures::ARM::kSVE2);
#endif

#if defined(__ARM_FEATURE_SVE2_AES)
  cpu.addFeature(CpuFeatures::ARM::kSVE2_AES);
#endif

#if defined(__ARM_FEATURE_SVE2_BITPERM)
  cpu.addFeature(CpuFeatures::ARM::kSVE2_BITPERM);
#endif

#if defined(__ARM_FEATURE_SVE2_SHA3)
  cpu.addFeature(CpuFeatures::ARM::kSVE2_SHA3);
#endif

#if defined(__ARM_FEATURE_SVE2_SM4)
  cpu.addFeature(CpuFeatures::ARM::kSVE2_SM4);
#endif

#if defined(__ARM_FEATURE_TME)
  cpu.addFeature(CpuFeatures::ARM::kTME);
#endif
}

ASMJIT_MAYBE_UNUSED
static ASMJIT_FAVOR_SIZE void expandARMFeaturesByVersion(CpuInfo& cpu) noexcept {
  CpuFeatures::ARM& features = cpu.features().arm();

  if (features.hasARMv8_7a()) {
    features.add(CpuFeatures::ARM::kARMv8_6a);
  }

  if (features.hasARMv8_6a()) {
    features.add(CpuFeatures::ARM::kARMv8_5a,
                 CpuFeatures::ARM::kBF16);

    if (features.hasSVE())
      features.add(CpuFeatures::ARM::kSVE_I8MM);
  }

  if (features.hasARMv8_5a()) {
    features.add(CpuFeatures::ARM::kARMv8_4a,
                 CpuFeatures::ARM::kALTNZCV,
                 CpuFeatures::ARM::kBTI,
                 CpuFeatures::ARM::kFRINT,
                 CpuFeatures::ARM::kSB,
                 CpuFeatures::ARM::kSSBS);
  }

  if (features.hasARMv8_4a()) {
    features.add(CpuFeatures::ARM::kARMv8_3a,
                 CpuFeatures::ARM::kDIT,
                 CpuFeatures::ARM::kDOTPROD,
                 CpuFeatures::ARM::kFLAGM,
                 CpuFeatures::ARM::kPMU,
                 CpuFeatures::ARM::kRCPC_IMMO);
  }

  if (features.hasARMv8_3a()) {
    features.add(CpuFeatures::ARM::kARMv8_2a,
                 CpuFeatures::ARM::kFCMA,
                 CpuFeatures::ARM::kFJCVTZS);
  }

  if (features.hasARMv8_2a()) {
    features.add(CpuFeatures::ARM::kARMv8_1a);
  }

  if (features.hasARMv8_1a()) {
    features.add(CpuFeatures::ARM::kARMv8a,
                 CpuFeatures::ARM::kCRC32,
                 CpuFeatures::ARM::kLSE,
                 CpuFeatures::ARM::kRDM);
  }

  if (features.hasARMv8a()) {
    features.add(CpuFeatures::ARM::kARMv7,
                 CpuFeatures::ARM::kVFPv2,
                 CpuFeatures::ARM::kVFPv3,
                 CpuFeatures::ARM::kVFPv4,
                 CpuFeatures::ARM::kVFP_D32,
                 CpuFeatures::ARM::kASIMD,
                 CpuFeatures::ARM::kIDIVA);
  }
}

// CpuInfo - Detect - ARM [Windows]
// ================================

#if defined(_WIN32)
struct WinPFPMapping {
  uint8_t featureId;
  uint8_t pfpFeatureId;
};

static ASMJIT_FAVOR_SIZE void detectPFPFeatures(CpuInfo& cpu, const WinPFPMapping* mapping, size_t size) noexcept {
  for (size_t i = 0; i < size; i++)
    if (::IsProcessorFeaturePresent(mapping[i].pfpFeatureId))
      cpu.addFeature(mapping[i].featureId);
}

//! Detect ARM CPU features on Windows.
//!
//! The detection is based on `IsProcessorFeaturePresent()` API call.
static ASMJIT_FAVOR_SIZE void detectARMCpu(CpuInfo& cpu) noexcept {
  cpu._wasDetected = true;
  populateBaseARMFeatures(cpu);

  CpuFeatures::ARM& features = cpu.features().arm();

  // Win32 for ARM requires ARMv7 with DSP extensions, VFPv3, and uses THUMBv2 by default.
#if ASMJIT_ARCH_ARM == 32
  features.add(CpuFeatures::ARM::kTHUMB);
  features.add(CpuFeatures::ARM::kTHUMBv2);
  features.add(CpuFeatures::ARM::kARMv6);
  features.add(CpuFeatures::ARM::kARMv7);
  features.add(CpuFeatures::ARM::kEDSP);
  features.add(CpuFeatures::ARM::kVFPv2);
  features.add(CpuFeatures::ARM::kVFPv3);
#endif

  // Windows for ARM requires ASIMD.
  features.add(CpuFeatures::ARM::kASIMD);

  // Detect additional CPU features by calling `IsProcessorFeaturePresent()`.
  static const WinPFPMapping mapping[] = {
#if ASMJIT_ARCH_ARM == 32
    { uint8_t(CpuFeatures::ARM::kVFP_D32)  , 18 }, // PF_ARM_VFP_32_REGISTERS_AVAILABLE
    { uint8_t(CpuFeatures::ARM::kIDIVT)    , 24 }, // PF_ARM_DIVIDE_INSTRUCTION_AVAILABLE
    { uint8_t(CpuFeatures::ARM::kVFPv4)    , 27 }, // PF_ARM_FMAC_INSTRUCTIONS_AVAILABLE
    { uint8_t(CpuFeatures::ARM::kARMv8a)   , 29 }, // PF_ARM_V8_INSTRUCTIONS_AVAILABLE
#endif
    { uint8_t(CpuFeatures::ARM::kAES)      , 30 }, // PF_ARM_V8_CRYPTO_INSTRUCTIONS_AVAILABLE
    { uint8_t(CpuFeatures::ARM::kCRC32)    , 31 }, // PF_ARM_V8_CRC32_INSTRUCTIONS_AVAILABLE
    { uint8_t(CpuFeatures::ARM::kLSE)      , 34 }  // PF_ARM_V81_ATOMIC_INSTRUCTIONS_AVAILABLE

  };
  detectPFPFeatures(cpu, mapping, ASMJIT_ARRAY_SIZE(mapping));

  // Windows provides several instructions under a single flag:
  if (features.hasAES()) {
    features.add(CpuFeatures::ARM::kSHA1,
                 CpuFeatures::ARM::kSHA2);
  }

  expandARMFeaturesByVersion(cpu);
}

// CpuInfo - Detect - ARM [Linux]
// ==============================

#elif defined(__linux__)

struct LinuxHWCapMapping {
  uint8_t featureId;
  uint8_t hwCapBit;
};

static ASMJIT_FAVOR_SIZE void detectHWCaps(CpuInfo& cpu, unsigned long type, const LinuxHWCapMapping* mapping, size_t size) noexcept {
  unsigned long mask = getauxval(type);
  for (size_t i = 0; i < size; i++)
    cpu.features().addIf(Support::bitTest(mask, mapping[i].hwCapBit), mapping[i].featureId);
}

#if ASMJIT_ARCH_ARM == 32

// `AT_HWCAP` provides ARMv7 (and less) related flags.
static const LinuxHWCapMapping hwCapMapping[] = {
  { uint8_t(CpuFeatures::ARM::kVFPv2)       , 6  }, // HWCAP_VFP
  { uint8_t(CpuFeatures::ARM::kEDSP)        , 7  }, // HWCAP_EDSP
  { uint8_t(CpuFeatures::ARM::kASIMD)       , 12 }, // HWCAP_NEON
  { uint8_t(CpuFeatures::ARM::kVFPv3)       , 13 }, // HWCAP_VFPv3
  { uint8_t(CpuFeatures::ARM::kVFPv4)       , 16 }, // HWCAP_VFPv4
  { uint8_t(CpuFeatures::ARM::kIDIVA)       , 17 }, // HWCAP_IDIVA
  { uint8_t(CpuFeatures::ARM::kIDIVT)       , 18 }, // HWCAP_IDIVT
  { uint8_t(CpuFeatures::ARM::kVFP_D32)     , 19 }  // HWCAP_VFPD32
};

// `AT_HWCAP2` provides ARMv8+ related flags.
static const LinuxHWCapMapping hwCap2Mapping[] = {
  { uint8_t(CpuFeatures::ARM::kAES)         , 0  }, // HWCAP2_AES
  { uint8_t(CpuFeatures::ARM::kPMULL)       , 1  }, // HWCAP2_PMULL
  { uint8_t(CpuFeatures::ARM::kSHA1)        , 2  }, // HWCAP2_SHA1
  { uint8_t(CpuFeatures::ARM::kSHA2)        , 3  }, // HWCAP2_SHA2
  { uint8_t(CpuFeatures::ARM::kCRC32)       , 4  }  // HWCAP2_CRC32
};

static ASMJIT_FAVOR_SIZE void detectARMCpu(CpuInfo& cpu) noexcept {
  cpu._wasDetected = true;

  populateBaseARMFeatures(cpu);

  CpuFeatures::ARM& features = cpu.features().arm();

  detectHWCaps(cpu, AT_HWCAP, hwCapMapping, ASMJIT_ARRAY_SIZE(hwCapMapping));
  detectHWCaps(cpu, AT_HWCAP2, hwCap2Mapping, ASMJIT_ARRAY_SIZE(hwCap2Mapping));

  // VFPv3 implies VFPv2.
  if (features.hasVFPv3())
    features.add(CpuFeatures::ARM::kVFPv2);

  // VFPv2 implies ARMv6.
  if (features.hasVFPv2())
    features.add(CpuFeatures::ARM::kARMv6);

  // ARMv7 provides VFPv3|ASIMD.
  if (features.hasVFPv3() || features.hasASIMD())
    features.add(CpuFeatures::ARM::kARMv7);

  // ARMv8 provives AES, CRC32, PMULL, SHA1, and SHA2.
  if (features.hasAES() || features.hasCRC32() || features.hasPMULL() || features.hasSHA1() || features.hasSHA2())
    features.add(CpuFeatures::ARM::kARMv8a);
}

#else

// `AT_HWCAP` provides ARMv8+ related flags.
static const LinuxHWCapMapping hwCapMapping[] = {
  /*
  { uint8_t(CpuFeatures::ARM::k)            , 0  }, // HWCAP_FP
  */
  { uint8_t(CpuFeatures::ARM::kASIMD)       , 1  }, // HWCAP_ASIMD
  /*
  { uint8_t(CpuFeatures::ARM::k)            , 2  }, // HWCAP_EVTSTRM
  */
  { uint8_t(CpuFeatures::ARM::kAES)         , 3  }, // HWCAP_AES
  { uint8_t(CpuFeatures::ARM::kPMULL)       , 4  }, // HWCAP_PMULL
  { uint8_t(CpuFeatures::ARM::kSHA1)        , 5  }, // HWCAP_SHA1
  { uint8_t(CpuFeatures::ARM::kSHA2)        , 6  }, // HWCAP_SHA2
  { uint8_t(CpuFeatures::ARM::kCRC32)       , 7  }, // HWCAP_CRC32
  { uint8_t(CpuFeatures::ARM::kLSE)         , 8  }, // HWCAP_ATOMICS
  { uint8_t(CpuFeatures::ARM::kFP16CONV)    , 9  }, // HWCAP_FPHP
  { uint8_t(CpuFeatures::ARM::kFP16FULL)    , 10 }, // HWCAP_ASIMDHP
  { uint8_t(CpuFeatures::ARM::kCPUID)       , 11 }, // HWCAP_CPUID
  { uint8_t(CpuFeatures::ARM::kRDM)         , 12 }, // HWCAP_ASIMDRDM
  { uint8_t(CpuFeatures::ARM::kFJCVTZS)     , 13 }, // HWCAP_JSCVT
  { uint8_t(CpuFeatures::ARM::kFCMA)        , 14 }, // HWCAP_FCMA
  /*
  { uint8_t(CpuFeatures::ARM::k)            , 15 }, // HWCAP_LRCPC
  { uint8_t(CpuFeatures::ARM::k)            , 16 }, // HWCAP_DCPOP
  */
  { uint8_t(CpuFeatures::ARM::kSHA3)        , 17 }, // HWCAP_SHA3
  { uint8_t(CpuFeatures::ARM::kSM3)         , 18 }, // HWCAP_SM3
  { uint8_t(CpuFeatures::ARM::kSM4)         , 19 }, // HWCAP_SM4
  { uint8_t(CpuFeatures::ARM::kDOTPROD)     , 20 }, // HWCAP_ASIMDDP
  { uint8_t(CpuFeatures::ARM::kSHA512)      , 21 }, // HWCAP_SHA512
  { uint8_t(CpuFeatures::ARM::kSVE)         , 22 }, // HWCAP_SVE
  { uint8_t(CpuFeatures::ARM::kFP16FML)     , 23 }, // HWCAP_ASIMDFHM
  { uint8_t(CpuFeatures::ARM::kDIT)         , 24 }, // HWCAP_DIT
  /*
  { uint8_t(CpuFeatures::ARM::k)            , 25 }, // HWCAP_USCAT
  { uint8_t(CpuFeatures::ARM::k)            , 26 }, // HWCAP_ILRCPC
  */
  { uint8_t(CpuFeatures::ARM::kFLAGM)       , 27 }, // HWCAP_FLAGM
  { uint8_t(CpuFeatures::ARM::kSSBS)        , 28 }, // HWCAP_SSBS
  { uint8_t(CpuFeatures::ARM::kSB)          , 29 }  // HWCAP_SB
  /*
  { uint8_t(CpuFeatures::ARM::k)            , 30 }, // HWCAP_PACA
  { uint8_t(CpuFeatures::ARM::k)            , 31 }  // HWCAP_PACG
  */
};

// `AT_HWCAP2` provides ARMv8+ related flags.
static const LinuxHWCapMapping hwCapMapping2[] = {
  /*
  { uint8_t(CpuFeatures::ARM::k)            , 0  }, // HWCAP2_DCPODP
  */
  { uint8_t(CpuFeatures::ARM::kSVE2)        , 1  }, // HWCAP2_SVE2
  { uint8_t(CpuFeatures::ARM::kSVE2_AES)    , 2  }, // HWCAP2_SVEAES
  { uint8_t(CpuFeatures::ARM::kSVE_PMULL)   , 3  }, // HWCAP2_SVEPMULL
  { uint8_t(CpuFeatures::ARM::kSVE2_BITPERM), 4  }, // HWCAP2_SVEBITPERM
  { uint8_t(CpuFeatures::ARM::kSVE2_SHA3)   , 5  }, // HWCAP2_SVESHA3
  { uint8_t(CpuFeatures::ARM::kSVE2_SM4)    , 6  }, // HWCAP2_SVESM4
  { uint8_t(CpuFeatures::ARM::kALTNZCV)     , 7  }, // HWCAP2_FLAGM2
  { uint8_t(CpuFeatures::ARM::kFRINT)       , 8  }, // HWCAP2_FRINT
  { uint8_t(CpuFeatures::ARM::kSVE_I8MM)    , 9  }, // HWCAP2_SVEI8MM
  { uint8_t(CpuFeatures::ARM::kSVE_F32MM)   , 10 }, // HWCAP2_SVEF32MM
  { uint8_t(CpuFeatures::ARM::kSVE_F64MM)   , 11 }, // HWCAP2_SVEF64MM
  { uint8_t(CpuFeatures::ARM::kSVE_BF16)    , 12 }, // HWCAP2_SVEBF16
  { uint8_t(CpuFeatures::ARM::kI8MM)        , 13 }, // HWCAP2_I8MM
  { uint8_t(CpuFeatures::ARM::kBF16)        , 14 }, // HWCAP2_BF16
  { uint8_t(CpuFeatures::ARM::kDGH)         , 15 }, // HWCAP2_DGH
  { uint8_t(CpuFeatures::ARM::kRNG)         , 16 }, // HWCAP2_RNG
  { uint8_t(CpuFeatures::ARM::kBTI)         , 17 }, // HWCAP2_BTI
  { uint8_t(CpuFeatures::ARM::kMTE)         , 18 }  // HWCAP2_MTE
};

static ASMJIT_FAVOR_SIZE void detectARMCpu(CpuInfo& cpu) noexcept {
  cpu._wasDetected = true;
  populateBaseARMFeatures(cpu);

  detectHWCaps(cpu, AT_HWCAP, hwCapMapping, ASMJIT_ARRAY_SIZE(hwCapMapping));
  detectHWCaps(cpu, AT_HWCAP2, hwCapMapping2, ASMJIT_ARRAY_SIZE(hwCapMapping2));
}

#endif

// CpuInfo - Detect - ARM [Apple]
// ==============================

#elif defined(__APPLE__)

namespace AppleHWId {
  enum CpuFamily : uint32_t {
    // Generic ARM.
    kCpuFamily_ARM_9              = 0xE73283AEu,
    kCpuFamily_ARM_11             = 0x8FF620D8u,
    kCpuFamily_ARM_12             = 0xBD1B0AE9u,
    kCpuFamily_ARM_13             = 0x0CC90E64u,
    kCpuFamily_ARM_14             = 0x96077EF1u,
    kCpuFamily_ARM_15             = 0xA8511BCAu,

    // Apple design.
    kCpuFamily_SWIFT              = 0x1E2D6381u,
    kCpuFamily_CYCLONE            = 0x37A09642u,
    kCpuFamily_TYPHOON            = 0x2C91A47Eu,
    kCpuFamily_TWISTER            = 0x92FB37C8u,
    kCpuFamily_HURRICANE          = 0x67CEEE93u,
    kCpuFamily_MONSOON_MISTRAL    = 0xE81E7EF6u,
    kCpuFamily_VORTEX_TEMPEST     = 0x07D34B9Fu,
    kCpuFamily_LIGHTNING_THUNDER  = 0x462504D2u,
    kCpuFamily_FIRESTORM_ICESTORM = 0x1B588BB3u
  };
};

static ASMJIT_FAVOR_SIZE uint32_t queryARMCpuFamilyId() noexcept {
  uint32_t result = 0;
  size_t size = sizeof(result);

  int res = sysctlbyname("hw.cpufamily", &result, &size, nullptr, 0);
  if (res != 0)
    return 0;
  else
    return result;
}

static ASMJIT_FAVOR_SIZE void detectARMCpu(CpuInfo& cpu) noexcept {
  cpu._wasDetected = true;
  populateBaseARMFeatures(cpu);

  uint32_t cpuFamilyId = queryARMCpuFamilyId();
  CpuFeatures::ARM& features = cpu.features().arm();

  switch (cpuFamilyId) {
    case AppleHWId::kCpuFamily_ARM_9:
    case AppleHWId::kCpuFamily_ARM_11:
    case AppleHWId::kCpuFamily_ARM_12:
      break;

    // ARM Cortex A8.
    case AppleHWId::kCpuFamily_ARM_13:
      break;

    // ARM Cortex A9.
    case AppleHWId::kCpuFamily_ARM_14:
      break;

    // ARM Cortex A7 - ARMv7k.
    case AppleHWId::kCpuFamily_ARM_15:
      features.add(CpuFeatures::ARM::kARMv7);
      break;

    // Apple A6/A6X - ARMv7s.
    case AppleHWId::kCpuFamily_SWIFT:
      features.add(CpuFeatures::ARM::kARMv7);
      break;

    // Apple A7 - ARMv8.0-A.
    case AppleHWId::kCpuFamily_CYCLONE:
      features.add(CpuFeatures::ARM::kARMv8a,
                   CpuFeatures::ARM::kAES,
                   CpuFeatures::ARM::kSHA1,
                   CpuFeatures::ARM::kSHA2);
      break;

    // Apple A8 - ARMv8.0-A.
    case AppleHWId::kCpuFamily_TYPHOON:
      features.add(CpuFeatures::ARM::kARMv8a,
                   CpuFeatures::ARM::kAES,
                   CpuFeatures::ARM::kSHA1,
                   CpuFeatures::ARM::kSHA2);
      break;

    // Apple A9 - ARMv8.0-A.
    case AppleHWId::kCpuFamily_TWISTER:
      features.add(CpuFeatures::ARM::kARMv8a,
                   CpuFeatures::ARM::kAES,
                   CpuFeatures::ARM::kSHA1,
                   CpuFeatures::ARM::kSHA2);
      break;

    // Apple A10 - ARMv8.1-A.
    case AppleHWId::kCpuFamily_HURRICANE:
      features.add(CpuFeatures::ARM::kARMv8_1a,
                   CpuFeatures::ARM::kAES,
                   CpuFeatures::ARM::kRDM,
                   CpuFeatures::ARM::kSHA1,
                   CpuFeatures::ARM::kSHA2);

      break;

    // Apple A11 - ARMv8.2-A.
    case AppleHWId::kCpuFamily_MONSOON_MISTRAL:
      features.add(CpuFeatures::ARM::kARMv8_2a,
                   CpuFeatures::ARM::kAES,
                   CpuFeatures::ARM::kFP16FULL,
                   CpuFeatures::ARM::kSHA1,
                   CpuFeatures::ARM::kSHA2);
      break;

    // Apple A12 - ARMv8.3-A.
    case AppleHWId::kCpuFamily_VORTEX_TEMPEST:
      features.add(CpuFeatures::ARM::kARMv8_3a,
                   CpuFeatures::ARM::kAES,
                   CpuFeatures::ARM::kFP16FULL,
                   CpuFeatures::ARM::kSHA1,
                   CpuFeatures::ARM::kSHA2);
      break;

    // Apple A13 - ARMv8.4-A.
    case AppleHWId::kCpuFamily_LIGHTNING_THUNDER:
      features.add(CpuFeatures::ARM::kARMv8_4a,
                   CpuFeatures::ARM::kAES,
                   CpuFeatures::ARM::kFP16FML,
                   CpuFeatures::ARM::kFP16FULL,
                   CpuFeatures::ARM::kSHA1,
                   CpuFeatures::ARM::kSHA2,
                   CpuFeatures::ARM::kSHA3,
                   CpuFeatures::ARM::kSHA512);
      break;

    // Apple A14/M1 - ARMv8.5-A.
    case AppleHWId::kCpuFamily_FIRESTORM_ICESTORM:
      features.add(CpuFeatures::ARM::kARMv8_4a,
                   CpuFeatures::ARM::kAES,
                   CpuFeatures::ARM::kALTNZCV,
                   CpuFeatures::ARM::kFP16FML,
                   CpuFeatures::ARM::kFP16FULL,
                   CpuFeatures::ARM::kFRINT,
                   CpuFeatures::ARM::kSB,
                   CpuFeatures::ARM::kSHA1,
                   CpuFeatures::ARM::kSHA2,
                   CpuFeatures::ARM::kSHA3,
                   CpuFeatures::ARM::kSHA512,
                   CpuFeatures::ARM::kSSBS);
      break;

    default:
      cpu._wasDetected = false;
      break;
  }

  expandARMFeaturesByVersion(cpu);
}

// CpuInfo - Detect - ARM [Unknown]
// ================================

#else

#if ASMJIT_ARCH_ARM == 64
  #pragma message("[asmjit] Disabling runtime CPU detection - unsupported OS/CPU combination (Unknown OS with AArch64 CPU)")
#else
  #pragma message("[asmjit] Disabling runtime CPU detection - unsupported OS/CPU combination (Unknown OS with ARM CPU)")
#endif

static ASMJIT_FAVOR_SIZE void detectARMCpu(CpuInfo& cpu) noexcept {
  populateBaseARMFeatures(cpu);
  detectARMFeaturesViaCompilerFlags(cpu);
  expandARMFeaturesByVersion(cpu);
}
#endif

#endif

// CpuInfo - Detect - Host
// =======================

static uint32_t cpuInfoInitialized;
static CpuInfo cpuInfoGlobal(Globals::NoInit);

const CpuInfo& CpuInfo::host() noexcept {
  // This should never cause a problem as the resulting information should always be the same. In the worst case we
  // would just overwrite it non-atomically.
  if (!cpuInfoInitialized) {
    CpuInfo cpuInfoLocal;

    cpuInfoLocal._arch = Arch::kHost;
    cpuInfoLocal._subArch = SubArch::kHost;

#if ASMJIT_ARCH_X86
    detectX86Cpu(cpuInfoLocal);
#elif ASMJIT_ARCH_ARM
    detectARMCpu(cpuInfoLocal);
#else
    #pragma message("[asmjit] Disabling runtime CPU detection - unsupported OS/CPU combination (Unknown CPU)")
#endif

    cpuInfoLocal._hwThreadCount = detectHWThreadCount();
    cpuInfoGlobal = cpuInfoLocal;
    cpuInfoInitialized = 1;
  }

  return cpuInfoGlobal;
}

ASMJIT_END_NAMESPACE
