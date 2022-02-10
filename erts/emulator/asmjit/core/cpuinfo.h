// This file is part of AsmJit project <https://asmjit.com>
//
// See asmjit.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#ifndef ASMJIT_CORE_CPUINFO_H_INCLUDED
#define ASMJIT_CORE_CPUINFO_H_INCLUDED

#include "../core/archtraits.h"
#include "../core/environment.h"
#include "../core/globals.h"
#include "../core/string.h"
#include "../core/support.h"

ASMJIT_BEGIN_NAMESPACE

//! \addtogroup asmjit_core
//! \{

//! CPU features information.
//!
//! Each feature is represented by a single bit in an embedded bit array.
class CpuFeatures {
public:
  //! A word that is used to represents feature bits.
  typedef Support::BitWord BitWord;
  //! Iterator that can iterate all CPU features set.
  typedef Support::BitVectorIterator<BitWord> Iterator;

  //! \name Constants
  //! \{

  //! \cond INTERNAL
  enum : uint32_t {
    kMaxFeatures = 256,
    kNumBitWords = kMaxFeatures / Support::kBitWordSizeInBits
  };
  //! \endcond

  //! \}

  //! \name Data
  //! \{

  //! CPU features data.
  struct Data {
    //! \name Members
    //! \{

    //! Data bits.
    Support::Array<BitWord, kNumBitWords> _bits;

    //! \}

    //! \name Overloaded Operators
    //! \{

    inline bool operator==(const Data& other) noexcept { return  eq(other); }
    inline bool operator!=(const Data& other) noexcept { return !eq(other); }

    //! \}

    //! \name Accessors
    //! \{

    //! Returns true if there are no features set.
    inline bool empty() const noexcept { return _bits.aggregate<Support::Or>(0) == 0; }

    //! Returns all features as array of bitwords (see \ref Support::BitWord).
    inline BitWord* bits() noexcept { return _bits.data(); }
    //! Returns all features as array of bitwords (const).
    inline const BitWord* bits() const noexcept { return _bits.data(); }

    //! Returns the number of BitWords returned by \ref bits().
    inline size_t bitWordCount() const noexcept { return kNumBitWords; }

    //! Returns \ref Support::BitVectorIterator, that can be used to iterate over all features efficiently.
    inline Iterator iterator() const noexcept { return Iterator(_bits.data(), kNumBitWords); }

    //! Tests whether the feature `featureId` is present.
    template<typename FeatureId>
    ASMJIT_FORCE_INLINE bool has(const FeatureId& featureId) const noexcept {
      ASMJIT_ASSERT(uint32_t(featureId) < kMaxFeatures);

      uint32_t idx = uint32_t(featureId) / Support::kBitWordSizeInBits;
      uint32_t bit = uint32_t(featureId) % Support::kBitWordSizeInBits;

      return bool((_bits[idx] >> bit) & 0x1);
    }

    //! Tests whether all features as defined by `other` are present.
    ASMJIT_FORCE_INLINE bool hasAll(const Data& other) const noexcept {
      for (uint32_t i = 0; i < kNumBitWords; i++)
        if ((_bits[i] & other._bits[i]) != other._bits[i])
          return false;
      return true;
    }

    //! \}

    //! \name Manipulation
    //! \{

    inline void reset() noexcept { _bits.fill(0); }

    //! Adds the given CPU `featureId` to the list of features.
    template<typename FeatureId>
    ASMJIT_FORCE_INLINE void add(const FeatureId& featureId) noexcept {
      ASMJIT_ASSERT(uint32_t(featureId) < kMaxFeatures);

      uint32_t idx = uint32_t(featureId) / Support::kBitWordSizeInBits;
      uint32_t bit = uint32_t(featureId) % Support::kBitWordSizeInBits;

      _bits[idx] |= BitWord(1) << bit;
    }

    template<typename FeatureId, typename... Args>
    ASMJIT_FORCE_INLINE void add(const FeatureId& featureId, Args&&... otherFeatureIds) noexcept {
      add(featureId);
      add(std::forward<Args>(otherFeatureIds)...);
    }

    template<typename FeatureId>
    ASMJIT_FORCE_INLINE void addIf(bool condition, const FeatureId& featureId) noexcept {
      ASMJIT_ASSERT(uint32_t(featureId) < kMaxFeatures);

      uint32_t idx = uint32_t(featureId) / Support::kBitWordSizeInBits;
      uint32_t bit = uint32_t(featureId) % Support::kBitWordSizeInBits;

      _bits[idx] |= BitWord(condition) << bit;
    }

    template<typename FeatureId, typename... Args>
    ASMJIT_FORCE_INLINE void addIf(bool condition, const FeatureId& featureId, Args&&... otherFeatureIds) noexcept {
      addIf(condition, featureId);
      addIf(condition, std::forward<Args>(otherFeatureIds)...);
    }

    //! Removes the given CPU `featureId` from the list of features.
    template<typename FeatureId>
    ASMJIT_FORCE_INLINE void remove(const FeatureId& featureId) noexcept {
      ASMJIT_ASSERT(uint32_t(featureId) < kMaxFeatures);

      uint32_t idx = uint32_t(featureId) / Support::kBitWordSizeInBits;
      uint32_t bit = uint32_t(featureId) % Support::kBitWordSizeInBits;

      _bits[idx] &= ~(BitWord(1) << bit);
    }

    template<typename FeatureId, typename... Args>
    ASMJIT_FORCE_INLINE void remove(const FeatureId& featureId, Args&&... otherFeatureIds) noexcept {
      remove(featureId);
      remove(std::forward<Args>(otherFeatureIds)...);
    }

    //! Tests whether this CPU features data matches `other`.
    ASMJIT_FORCE_INLINE bool eq(const Data& other) const noexcept { return _bits == other._bits; }

    //! \}

  };

  //! X86 specific features data.
  struct X86 : public Data {
    //! X86 CPU feature identifiers.
    enum Id : uint8_t {
      // @EnumValuesBegin{"enum": "CpuFeatures::X86"}@
      kNone,                     //!< No feature (never set, used internally).

      kMT,                       //!< CPU has multi-threading capabilities.
      kNX,                       //!< CPU has Not-Execute-Bit aka DEP (data-execution prevention).
      k3DNOW,                    //!< CPU has 3DNOW            (3DNOW base instructions) [AMD].
      k3DNOW2,                   //!< CPU has 3DNOW2           (enhanced 3DNOW) [AMD].
      kADX,                      //!< CPU has ADX              (multi-precision add-carry instruction extensions).
      kAESNI,                    //!< CPU has AESNI            (AES encode/decode instructions).
      kALTMOVCR8,                //!< CPU has LOCK MOV R<->CR0 (supports `MOV R<->CR8` via `LOCK MOV R<->CR0` in 32-bit mode) [AMD].
      kAMX_BF16,                 //!< CPU has AMX_BF16         (advanced matrix extensions - BF16 instructions).
      kAMX_INT8,                 //!< CPU has AMX_INT8         (advanced matrix extensions - INT8 instructions).
      kAMX_TILE,                 //!< CPU has AMX_TILE         (advanced matrix extensions).
      kAVX,                      //!< CPU has AVX              (advanced vector extensions).
      kAVX2,                     //!< CPU has AVX2             (advanced vector extensions 2).
      kAVX512_4FMAPS,            //!< CPU has AVX512_FMAPS     (FMA packed single).
      kAVX512_4VNNIW,            //!< CPU has AVX512_VNNIW     (vector NN instructions word variable precision).
      kAVX512_BF16,              //!< CPU has AVX512_BF16      (BFLOAT16 support instruction).
      kAVX512_BITALG,            //!< CPU has AVX512_BITALG    (VPOPCNT[B|W], VPSHUFBITQMB).
      kAVX512_BW,                //!< CPU has AVX512_BW        (packed BYTE|WORD).
      kAVX512_CDI,               //!< CPU has AVX512_CDI       (conflict detection).
      kAVX512_DQ,                //!< CPU has AVX512_DQ        (packed DWORD|QWORD).
      kAVX512_ERI,               //!< CPU has AVX512_ERI       (exponential and reciprocal).
      kAVX512_F,                 //!< CPU has AVX512_F         (AVX512 foundation).
      kAVX512_FP16,              //!< CPU has AVX512_FP16      (FP16 extensions).
      kAVX512_IFMA,              //!< CPU has AVX512_IFMA      (integer fused-multiply-add using 52-bit precision).
      kAVX512_PFI,               //!< CPU has AVX512_PFI       (prefetch instructions).
      kAVX512_VBMI,              //!< CPU has AVX512_VBMI      (vector byte manipulation).
      kAVX512_VBMI2,             //!< CPU has AVX512_VBMI2     (vector byte manipulation 2).
      kAVX512_VL,                //!< CPU has AVX512_VL        (vector length extensions).
      kAVX512_VNNI,              //!< CPU has AVX512_VNNI      (vector neural network instructions).
      kAVX512_VP2INTERSECT,      //!< CPU has AVX512_VP2INTERSECT
      kAVX512_VPOPCNTDQ,         //!< CPU has AVX512_VPOPCNTDQ (VPOPCNT[D|Q] instructions).
      kAVX_VNNI,                 //!< CPU has AVX_VNNI         (VEX encoding of vpdpbusd/vpdpbusds/vpdpwssd/vpdpwssds).
      kBMI,                      //!< CPU has BMI              (bit manipulation instructions #1).
      kBMI2,                     //!< CPU has BMI2             (bit manipulation instructions #2).
      kCET_IBT,                  //!< CPU has CET-IBT          (indirect branch tracking).
      kCET_SS,                   //!< CPU has CET-SS.
      kCLDEMOTE,                 //!< CPU has CLDEMOTE         (cache line demote).
      kCLFLUSH,                  //!< CPU has CLFUSH           (Cache Line flush).
      kCLFLUSHOPT,               //!< CPU has CLFUSHOPT        (Cache Line flush - optimized).
      kCLWB,                     //!< CPU has CLWB.
      kCLZERO,                   //!< CPU has CLZERO.
      kCMOV,                     //!< CPU has CMOV             (CMOV and FCMOV instructions).
      kCMPXCHG16B,               //!< CPU has CMPXCHG16B       (compare-exchange 16 bytes) [X86_64].
      kCMPXCHG8B,                //!< CPU has CMPXCHG8B        (compare-exchange 8 bytes).
      kENCLV,                    //!< CPU has ENCLV.
      kENQCMD,                   //!< CPU has ENQCMD           (enqueue stores).
      kERMS,                     //!< CPU has ERMS             (enhanced REP MOVSB/STOSB).
      kF16C,                     //!< CPU has F16C.
      kFMA,                      //!< CPU has FMA              (fused-multiply-add 3 operand form).
      kFMA4,                     //!< CPU has FMA4             (fused-multiply-add 4 operand form).
      kFPU,                      //!< CPU has FPU              (FPU support).
      kFSGSBASE,                 //!< CPU has FSGSBASE.
      kFXSR,                     //!< CPU has FXSR             (FXSAVE/FXRSTOR instructions).
      kFXSROPT,                  //!< CPU has FXSROTP          (FXSAVE/FXRSTOR is optimized).
      kGEODE,                    //!< CPU has GEODE extensions (3DNOW additions).
      kGFNI,                     //!< CPU has GFNI             (Galois field instructions).
      kHLE,                      //!< CPU has HLE.
      kHRESET,                   //!< CPU has HRESET.
      kI486,                     //!< CPU has I486 features    (I486+ support).
      kLAHFSAHF,                 //!< CPU has LAHF/SAHF        (LAHF/SAHF in 64-bit mode) [X86_64].
      kLWP,                      //!< CPU has LWP              (lightweight profiling) [AMD].
      kLZCNT,                    //!< CPU has LZCNT            (LZCNT instruction).
      kMCOMMIT,                  //!< CPU has MCOMMIT          (MCOMMIT instruction).
      kMMX,                      //!< CPU has MMX              (MMX base instructions).
      kMMX2,                     //!< CPU has MMX2             (MMX extensions or MMX2).
      kMONITOR,                  //!< CPU has MONITOR          (MONITOR/MWAIT instructions).
      kMONITORX,                 //!< CPU has MONITORX         (MONITORX/MWAITX instructions).
      kMOVBE,                    //!< CPU has MOVBE            (move with byte-order swap).
      kMOVDIR64B,                //!< CPU has MOVDIR64B        (move 64 bytes as direct store).
      kMOVDIRI,                  //!< CPU has MOVDIRI          (move dword/qword as direct store).
      kMPX,                      //!< CPU has MPX              (memory protection extensions).
      kMSR,                      //!< CPU has MSR              (RDMSR/WRMSR instructions).
      kMSSE,                     //!< CPU has MSSE             (misaligned SSE support).
      kOSXSAVE,                  //!< CPU has OSXSAVE          (XSAVE enabled by OS).
      kOSPKE,                    //!< CPU has OSPKE            (PKE enabled by OS).
      kPCLMULQDQ,                //!< CPU has PCLMULQDQ        (packed carry-less multiplication).
      kPCONFIG,                  //!< CPU has PCONFIG          (PCONFIG instruction).
      kPOPCNT,                   //!< CPU has POPCNT           (POPCNT instruction).
      kPREFETCHW,                //!< CPU has PREFETCHW.
      kPREFETCHWT1,              //!< CPU has PREFETCHWT1.
      kPTWRITE,                  //!< CPU has PTWRITE.
      kRDPID,                    //!< CPU has RDPID.
      kRDPRU,                    //!< CPU has RDPRU.
      kRDRAND,                   //!< CPU has RDRAND.
      kRDSEED,                   //!< CPU has RDSEED.
      kRDTSC,                    //!< CPU has RDTSC.
      kRDTSCP,                   //!< CPU has RDTSCP.
      kRTM,                      //!< CPU has RTM.
      kSERIALIZE,                //!< CPU has SERIALIZE.
      kSHA,                      //!< CPU has SHA              (SHA-1 and SHA-256 instructions).
      kSKINIT,                   //!< CPU has SKINIT           (SKINIT/STGI instructions) [AMD].
      kSMAP,                     //!< CPU has SMAP             (supervisor-mode access prevention).
      kSMEP,                     //!< CPU has SMEP             (supervisor-mode execution prevention).
      kSMX,                      //!< CPU has SMX              (safer mode extensions).
      kSNP,                      //!< CPU has SNP.
      kSSE,                      //!< CPU has SSE.
      kSSE2,                     //!< CPU has SSE2.
      kSSE3,                     //!< CPU has SSE3.
      kSSE4_1,                   //!< CPU has SSE4.1.
      kSSE4_2,                   //!< CPU has SSE4.2.
      kSSE4A,                    //!< CPU has SSE4A [AMD].
      kSSSE3,                    //!< CPU has SSSE3.
      kSVM,                      //!< CPU has SVM              (virtualization) [AMD].
      kTBM,                      //!< CPU has TBM              (trailing bit manipulation) [AMD].
      kTSX,                      //!< CPU has TSX.
      kTSXLDTRK,                 //!< CPU has TSXLDTRK.
      kUINTR,                    //!< CPU has UINTR            (user interrupts).
      kVAES,                     //!< CPU has VAES             (vector AES 256|512 bit support).
      kVMX,                      //!< CPU has VMX              (virtualization) [INTEL].
      kVPCLMULQDQ,               //!< CPU has VPCLMULQDQ       (vector PCLMULQDQ 256|512-bit support).
      kWAITPKG,                  //!< CPU has WAITPKG          (UMONITOR, UMWAIT, TPAUSE).
      kWBNOINVD,                 //!< CPU has WBNOINVD.
      kXOP,                      //!< CPU has XOP              (XOP instructions) [AMD].
      kXSAVE,                    //!< CPU has XSAVE.
      kXSAVEC,                   //!< CPU has XSAVEC.
      kXSAVEOPT,                 //!< CPU has XSAVEOPT.
      kXSAVES,                   //!< CPU has XSAVES.
      // @EnumValuesEnd@

      kMaxValue = kXSAVES
    };

    #define ASMJIT_X86_FEATURE(FEATURE) \
      inline bool has##FEATURE() const noexcept { return has(X86::k##FEATURE); }

    ASMJIT_X86_FEATURE(MT)
    ASMJIT_X86_FEATURE(NX)
    ASMJIT_X86_FEATURE(3DNOW)
    ASMJIT_X86_FEATURE(3DNOW2)
    ASMJIT_X86_FEATURE(ADX)
    ASMJIT_X86_FEATURE(AESNI)
    ASMJIT_X86_FEATURE(ALTMOVCR8)
    ASMJIT_X86_FEATURE(AMX_BF16)
    ASMJIT_X86_FEATURE(AMX_INT8)
    ASMJIT_X86_FEATURE(AMX_TILE)
    ASMJIT_X86_FEATURE(AVX)
    ASMJIT_X86_FEATURE(AVX2)
    ASMJIT_X86_FEATURE(AVX512_4FMAPS)
    ASMJIT_X86_FEATURE(AVX512_4VNNIW)
    ASMJIT_X86_FEATURE(AVX512_BF16)
    ASMJIT_X86_FEATURE(AVX512_BITALG)
    ASMJIT_X86_FEATURE(AVX512_BW)
    ASMJIT_X86_FEATURE(AVX512_CDI)
    ASMJIT_X86_FEATURE(AVX512_DQ)
    ASMJIT_X86_FEATURE(AVX512_ERI)
    ASMJIT_X86_FEATURE(AVX512_F)
    ASMJIT_X86_FEATURE(AVX512_FP16)
    ASMJIT_X86_FEATURE(AVX512_IFMA)
    ASMJIT_X86_FEATURE(AVX512_PFI)
    ASMJIT_X86_FEATURE(AVX512_VBMI)
    ASMJIT_X86_FEATURE(AVX512_VBMI2)
    ASMJIT_X86_FEATURE(AVX512_VL)
    ASMJIT_X86_FEATURE(AVX512_VNNI)
    ASMJIT_X86_FEATURE(AVX512_VP2INTERSECT)
    ASMJIT_X86_FEATURE(AVX512_VPOPCNTDQ)
    ASMJIT_X86_FEATURE(AVX_VNNI)
    ASMJIT_X86_FEATURE(BMI)
    ASMJIT_X86_FEATURE(BMI2)
    ASMJIT_X86_FEATURE(CET_IBT)
    ASMJIT_X86_FEATURE(CET_SS)
    ASMJIT_X86_FEATURE(CLDEMOTE)
    ASMJIT_X86_FEATURE(CLFLUSH)
    ASMJIT_X86_FEATURE(CLFLUSHOPT)
    ASMJIT_X86_FEATURE(CLWB)
    ASMJIT_X86_FEATURE(CLZERO)
    ASMJIT_X86_FEATURE(CMOV)
    ASMJIT_X86_FEATURE(CMPXCHG16B)
    ASMJIT_X86_FEATURE(CMPXCHG8B)
    ASMJIT_X86_FEATURE(ENCLV)
    ASMJIT_X86_FEATURE(ENQCMD)
    ASMJIT_X86_FEATURE(ERMS)
    ASMJIT_X86_FEATURE(F16C)
    ASMJIT_X86_FEATURE(FMA)
    ASMJIT_X86_FEATURE(FMA4)
    ASMJIT_X86_FEATURE(FPU)
    ASMJIT_X86_FEATURE(FSGSBASE)
    ASMJIT_X86_FEATURE(FXSR)
    ASMJIT_X86_FEATURE(FXSROPT)
    ASMJIT_X86_FEATURE(GEODE)
    ASMJIT_X86_FEATURE(GFNI)
    ASMJIT_X86_FEATURE(HLE)
    ASMJIT_X86_FEATURE(HRESET)
    ASMJIT_X86_FEATURE(I486)
    ASMJIT_X86_FEATURE(LAHFSAHF)
    ASMJIT_X86_FEATURE(LWP)
    ASMJIT_X86_FEATURE(LZCNT)
    ASMJIT_X86_FEATURE(MCOMMIT)
    ASMJIT_X86_FEATURE(MMX)
    ASMJIT_X86_FEATURE(MMX2)
    ASMJIT_X86_FEATURE(MONITOR)
    ASMJIT_X86_FEATURE(MONITORX)
    ASMJIT_X86_FEATURE(MOVBE)
    ASMJIT_X86_FEATURE(MOVDIR64B)
    ASMJIT_X86_FEATURE(MOVDIRI)
    ASMJIT_X86_FEATURE(MPX)
    ASMJIT_X86_FEATURE(MSR)
    ASMJIT_X86_FEATURE(MSSE)
    ASMJIT_X86_FEATURE(OSXSAVE)
    ASMJIT_X86_FEATURE(OSPKE)
    ASMJIT_X86_FEATURE(PCLMULQDQ)
    ASMJIT_X86_FEATURE(PCONFIG)
    ASMJIT_X86_FEATURE(POPCNT)
    ASMJIT_X86_FEATURE(PREFETCHW)
    ASMJIT_X86_FEATURE(PREFETCHWT1)
    ASMJIT_X86_FEATURE(PTWRITE)
    ASMJIT_X86_FEATURE(RDPID)
    ASMJIT_X86_FEATURE(RDPRU)
    ASMJIT_X86_FEATURE(RDRAND)
    ASMJIT_X86_FEATURE(RDSEED)
    ASMJIT_X86_FEATURE(RDTSC)
    ASMJIT_X86_FEATURE(RDTSCP)
    ASMJIT_X86_FEATURE(RTM)
    ASMJIT_X86_FEATURE(SERIALIZE)
    ASMJIT_X86_FEATURE(SHA)
    ASMJIT_X86_FEATURE(SKINIT)
    ASMJIT_X86_FEATURE(SMAP)
    ASMJIT_X86_FEATURE(SMEP)
    ASMJIT_X86_FEATURE(SMX)
    ASMJIT_X86_FEATURE(SNP)
    ASMJIT_X86_FEATURE(SSE)
    ASMJIT_X86_FEATURE(SSE2)
    ASMJIT_X86_FEATURE(SSE3)
    ASMJIT_X86_FEATURE(SSE4_1)
    ASMJIT_X86_FEATURE(SSE4_2)
    ASMJIT_X86_FEATURE(SSE4A)
    ASMJIT_X86_FEATURE(SSSE3)
    ASMJIT_X86_FEATURE(SVM)
    ASMJIT_X86_FEATURE(TBM)
    ASMJIT_X86_FEATURE(TSX)
    ASMJIT_X86_FEATURE(TSXLDTRK)
    ASMJIT_X86_FEATURE(UINTR)
    ASMJIT_X86_FEATURE(VAES)
    ASMJIT_X86_FEATURE(VMX)
    ASMJIT_X86_FEATURE(VPCLMULQDQ)
    ASMJIT_X86_FEATURE(WAITPKG)
    ASMJIT_X86_FEATURE(WBNOINVD)
    ASMJIT_X86_FEATURE(XOP)
    ASMJIT_X86_FEATURE(XSAVE)
    ASMJIT_X86_FEATURE(XSAVEC)
    ASMJIT_X86_FEATURE(XSAVEOPT)
    ASMJIT_X86_FEATURE(XSAVES)

    #undef ASMJIT_X86_FEATURE
  };

  //! ARM specific features data.
  struct ARM : public Data {
    //! ARM CPU feature identifiers.
    enum Id : uint8_t {
      // @EnumValuesBegin{"enum": "CpuFeatures::ARM"}@
      kNone = 0,                 //!< No feature (never set, used internally).
      kTHUMB,                    //!< THUMB v1 ISA.
      kTHUMBv2,                  //!< THUMB v2 ISA.

      kARMv6,                    //!< ARMv6 ISA.
      kARMv7,                    //!< ARMv7 ISA.
      kARMv8a,                   //!< ARMv8-A ISA.
      kARMv8_1a,                 //!< ARMv8.1-A ISA.
      kARMv8_2a,                 //!< ARMv8.2-A ISA.
      kARMv8_3a,                 //!< ARMv8.3-A ISA.
      kARMv8_4a,                 //!< ARMv8.4-A ISA.
      kARMv8_5a,                 //!< ARMv8.5-A ISA.
      kARMv8_6a,                 //!< ARMv8.6-A ISA.
      kARMv8_7a,                 //!< ARMv8.7-A ISA.

      kVFPv2,                    //!< CPU has VFPv2 instruction set.
      kVFPv3,                    //!< CPU has VFPv3 instruction set.
      kVFPv4,                    //!< CPU has VFPv4 instruction set.
      kVFP_D32,                  //!< CPU has 32 VFP-D (64-bit) registers.

      kAES,                      //!< CPU has AES (AArch64 only).
      kALTNZCV,                  //!< CPU has ALTNZCV (AArch64 only).
      kASIMD,                    //!< CPU has Advanced SIMD (NEON on ARM/THUMB).
      kBF16,                     //!< CPU has BF16 (AArch64 only).
      kBTI,                      //!< CPU has BTI (branch target identification).
      kCPUID,                    //!< CPU has accessible CPUID register (ID_AA64ZFR0_EL1).
      kCRC32,                    //!< CPU has CRC32 .
      kDGH,                      //!< CPU has DGH (AArch64 only).
      kDIT,                      //!< CPU has data independent timing instructions (DIT).
      kDOTPROD,                  //!< CPU has DOTPROD (SDOT/UDOT).
      kEDSP,                     //!< CPU has EDSP (ARM/THUMB only).
      kFCMA,                     //!< CPU has FCMA (FCADD/FCMLA).
      kFJCVTZS,                  //!< CPU has FJCVTZS (AArch64 only).
      kFLAGM,                    //!< CPU has FLAGM (AArch64 only).
      kFP16CONV,                 //!< CPU has FP16 (half-float) conversion.
      kFP16FML,                  //!< CPU has FMLAL{2}/FMLSL{2}
      kFP16FULL,                 //!< CPU has full support for FP16.
      kFRINT,                    //!< CPU has FRINT[32|64][X|Z] (AArch64 only).
      kI8MM,                     //!< CPU has I8MM (AArch64 only).
      kIDIVA,                    //!< CPU has hardware SDIV and UDIV (ARM mode).
      kIDIVT,                    //!< CPU has hardware SDIV and UDIV (THUMB mode).
      kLSE,                      //!< CPU has large system extensions (LSE) (AArch64 only).
      kMTE,                      //!< CPU has MTE (AArch64 only).
      kRCPC_IMMO,                //!< CPU has RCPC_IMMO (AArch64 only).
      kRDM,                      //!< CPU has RDM (AArch64 only).
      kPMU,                      //!< CPU has PMU (AArch64 only).
      kPMULL,                    //!< CPU has PMULL (AArch64 only).
      kRNG,                      //!< CPU has random number generation (RNG).
      kSB,                       //!< CPU has speculative barrier SB (AArch64 only).
      kSHA1,                     //!< CPU has SHA1.
      kSHA2,                     //!< CPU has SHA2.
      kSHA3,                     //!< CPU has SHA3.
      kSHA512,                   //!< CPU has SHA512.
      kSM3,                      //!< CPU has SM3.
      kSM4,                      //!< CPU has SM4.
      kSSBS,                     //!< CPU has SSBS.
      kSVE,                      //!< CPU has SVE (AArch64 only).
      kSVE_BF16,                 //!< CPU has SVE-BF16 (AArch64 only).
      kSVE_F32MM,                //!< CPU has SVE-F32MM (AArch64 only).
      kSVE_F64MM,                //!< CPU has SVE-F64MM (AArch64 only).
      kSVE_I8MM,                 //!< CPU has SVE-I8MM (AArch64 only).
      kSVE_PMULL,                //!< CPU has SVE-PMULL (AArch64 only).
      kSVE2,                     //!< CPU has SVE2 (AArch64 only).
      kSVE2_AES,                 //!< CPU has SVE2-AES (AArch64 only).
      kSVE2_BITPERM,             //!< CPU has SVE2-BITPERM (AArch64 only).
      kSVE2_SHA3,                //!< CPU has SVE2-SHA3 (AArch64 only).
      kSVE2_SM4,                 //!< CPU has SVE2-SM4 (AArch64 only).
      kTME,                      //!< CPU has transactional memory extensions (TME).
      // @EnumValuesEnd@

      kMaxValue = kTME
    };

    #define ASMJIT_ARM_FEATURE(FEATURE) \
      inline bool has##FEATURE() const noexcept { return has(ARM::k##FEATURE); }

    ASMJIT_ARM_FEATURE(THUMB)
    ASMJIT_ARM_FEATURE(THUMBv2)

    ASMJIT_ARM_FEATURE(ARMv6)
    ASMJIT_ARM_FEATURE(ARMv7)
    ASMJIT_ARM_FEATURE(ARMv8a)
    ASMJIT_ARM_FEATURE(ARMv8_1a)
    ASMJIT_ARM_FEATURE(ARMv8_2a)
    ASMJIT_ARM_FEATURE(ARMv8_3a)
    ASMJIT_ARM_FEATURE(ARMv8_4a)
    ASMJIT_ARM_FEATURE(ARMv8_5a)
    ASMJIT_ARM_FEATURE(ARMv8_6a)
    ASMJIT_ARM_FEATURE(ARMv8_7a)

    ASMJIT_ARM_FEATURE(VFPv2)
    ASMJIT_ARM_FEATURE(VFPv3)
    ASMJIT_ARM_FEATURE(VFPv4)
    ASMJIT_ARM_FEATURE(VFP_D32)

    ASMJIT_ARM_FEATURE(AES)
    ASMJIT_ARM_FEATURE(ALTNZCV)
    ASMJIT_ARM_FEATURE(ASIMD)
    ASMJIT_ARM_FEATURE(BF16)
    ASMJIT_ARM_FEATURE(BTI)
    ASMJIT_ARM_FEATURE(CPUID)
    ASMJIT_ARM_FEATURE(CRC32)
    ASMJIT_ARM_FEATURE(DGH)
    ASMJIT_ARM_FEATURE(DIT)
    ASMJIT_ARM_FEATURE(DOTPROD)
    ASMJIT_ARM_FEATURE(EDSP)
    ASMJIT_ARM_FEATURE(FCMA)
    ASMJIT_ARM_FEATURE(FLAGM)
    ASMJIT_ARM_FEATURE(FP16CONV)
    ASMJIT_ARM_FEATURE(FP16FML)
    ASMJIT_ARM_FEATURE(FP16FULL)
    ASMJIT_ARM_FEATURE(FRINT)
    ASMJIT_ARM_FEATURE(IDIVA)
    ASMJIT_ARM_FEATURE(IDIVT)
    ASMJIT_ARM_FEATURE(LSE)
    ASMJIT_ARM_FEATURE(MTE)
    ASMJIT_ARM_FEATURE(FJCVTZS)
    ASMJIT_ARM_FEATURE(I8MM)
    ASMJIT_ARM_FEATURE(RCPC_IMMO)
    ASMJIT_ARM_FEATURE(RDM)
    ASMJIT_ARM_FEATURE(PMU)
    ASMJIT_ARM_FEATURE(PMULL)
    ASMJIT_ARM_FEATURE(RNG)
    ASMJIT_ARM_FEATURE(SB)
    ASMJIT_ARM_FEATURE(SHA1)
    ASMJIT_ARM_FEATURE(SHA2)
    ASMJIT_ARM_FEATURE(SHA3)
    ASMJIT_ARM_FEATURE(SHA512)
    ASMJIT_ARM_FEATURE(SM3)
    ASMJIT_ARM_FEATURE(SM4)
    ASMJIT_ARM_FEATURE(SSBS)
    ASMJIT_ARM_FEATURE(SVE)
    ASMJIT_ARM_FEATURE(SVE_BF16)
    ASMJIT_ARM_FEATURE(SVE_F32MM)
    ASMJIT_ARM_FEATURE(SVE_F64MM)
    ASMJIT_ARM_FEATURE(SVE_I8MM)
    ASMJIT_ARM_FEATURE(SVE_PMULL)
    ASMJIT_ARM_FEATURE(SVE2)
    ASMJIT_ARM_FEATURE(SVE2_AES)
    ASMJIT_ARM_FEATURE(SVE2_BITPERM)
    ASMJIT_ARM_FEATURE(SVE2_SHA3)
    ASMJIT_ARM_FEATURE(SVE2_SM4)
    ASMJIT_ARM_FEATURE(TME)

    #undef ASMJIT_ARM_FEATURE
  };

  static_assert(uint32_t(X86::kMaxValue) < kMaxFeatures, "The number of X86 CPU features cannot exceed CpuFeatures::kMaxFeatures");
  static_assert(uint32_t(ARM::kMaxValue) < kMaxFeatures, "The number of ARM CPU features cannot exceed CpuFeatures::kMaxFeatures");

  //! \}

  //! \name Members
  //! \{

  Data _data {};

  //! \}

  //! \name Construction & Destruction
  //! \{

  inline CpuFeatures() noexcept {}
  inline CpuFeatures(const CpuFeatures& other) noexcept = default;
  inline explicit CpuFeatures(Globals::NoInit_) noexcept {}

  //! \}

  //! \name Overloaded Operators
  //! \{

  inline CpuFeatures& operator=(const CpuFeatures& other) noexcept = default;

  inline bool operator==(const CpuFeatures& other) noexcept { return  eq(other); }
  inline bool operator!=(const CpuFeatures& other) noexcept { return !eq(other); }

  //! \}

  //! \name Accessors
  //! \{

  //! Returns true if there are no features set.
  inline bool empty() const noexcept { return _data.empty(); }

  //! Casts this base class into a derived type `T`.
  template<typename T = Data>
  inline T& data() noexcept { return static_cast<T&>(_data); }

  //! Casts this base class into a derived type `T` (const).
  template<typename T = Data>
  inline const T& data() const noexcept { return static_cast<const T&>(_data); }

  //! Returns CpuFeatures::Data as \ref CpuFeatures::X86.
  inline X86& x86() noexcept { return data<X86>(); }
  //! Returns CpuFeatures::Data as \ref CpuFeatures::X86 (const).
  inline const X86& x86() const noexcept { return data<X86>(); }

  //! Returns CpuFeatures::Data as \ref CpuFeatures::ARM.
  inline ARM& arm() noexcept { return data<ARM>(); }
  //! Returns CpuFeatures::Data as \ref CpuFeatures::ARM (const).
  inline const ARM& arm() const noexcept { return data<ARM>(); }

  //! Returns all features as array of bitwords (see \ref Support::BitWord).
  inline BitWord* bits() noexcept { return _data.bits(); }
  //! Returns all features as array of bitwords (const).
  inline const BitWord* bits() const noexcept { return _data.bits(); }
  //! Returns the number of BitWords returned by \ref bits().
  inline size_t bitWordCount() const noexcept { return _data.bitWordCount(); }

  //! Returns \ref Support::BitVectorIterator, that can be used to iterate over all features efficiently.
  inline Iterator iterator() const noexcept { return _data.iterator(); }

  //! Tests whether the feature `featureId` is present.
  template<typename FeatureId>
  inline bool has(const FeatureId& featureId) const noexcept { return _data.has(featureId); }

  //! Tests whether all features as defined by `other` are present.
  inline bool hasAll(const CpuFeatures& other) const noexcept { return _data.hasAll(other._data); }

  //! \}

  //! \name Manipulation
  //! \{

  inline void reset() noexcept { _data.reset(); }

  //! Adds the given CPU `featureId` to the list of features.
  template<typename... Args>
  inline void add(Args&&... args) noexcept { return _data.add(std::forward<Args>(args)...); }

  //! Adds the given CPU `featureId` to the list of features if `condition` is true.
  template<typename... Args>
  inline void addIf(bool condition, Args&&... args) noexcept { return _data.addIf(condition, std::forward<Args>(args)...); }

  //! Removes the given CPU `featureId` from the list of features.
  template<typename... Args>
  inline void remove(Args&&... args) noexcept { return _data.remove(std::forward<Args>(args)...); }

  //! Tests whether this CPU features matches `other`.
  inline bool eq(const CpuFeatures& other) const noexcept { return _data.eq(other._data); }

  //! \}
};

//! CPU information.
class CpuInfo {
public:
  //! \name Members
  //! \{

  //! Architecture.
  Arch _arch;
  //! Sub-architecture.
  SubArch _subArch;
  //! True if the CPU was detected, false if the detection failed or it's not available.
  bool _wasDetected;
  //! Reserved for future use.
  uint8_t _reserved;
  //! CPU family ID.
  uint32_t _familyId;
  //! CPU model ID.
  uint32_t _modelId;
  //! CPU brand ID.
  uint32_t _brandId;
  //! CPU stepping.
  uint32_t _stepping;
  //! Processor type.
  uint32_t _processorType;
  //! Maximum number of addressable IDs for logical processors.
  uint32_t _maxLogicalProcessors;
  //! Cache line size (in bytes).
  uint32_t _cacheLineSize;
  //! Number of hardware threads.
  uint32_t _hwThreadCount;

  //! CPU vendor string.
  FixedString<16> _vendor;
  //! CPU brand string.
  FixedString<64> _brand;
  //! CPU features.
  CpuFeatures _features;

  //! \}

  //! \name Construction & Destruction
  //! \{

  inline CpuInfo() noexcept { reset(); }
  inline CpuInfo(const CpuInfo& other) noexcept = default;

  inline explicit CpuInfo(Globals::NoInit_) noexcept
    : _features(Globals::NoInit) {};

  //! Returns the host CPU information.
  ASMJIT_API static const CpuInfo& host() noexcept;

  //! Initializes CpuInfo architecture and sub-architecture members to `arch` and `subArch`, respectively.
  inline void initArch(Arch arch, SubArch subArch = SubArch::kUnknown) noexcept {
    _arch = arch;
    _subArch = subArch;
  }

  inline void reset() noexcept { memset(this, 0, sizeof(*this)); }

  //! \}

  //! \name Overloaded Operators
  //! \{

  inline CpuInfo& operator=(const CpuInfo& other) noexcept = default;

  //! \}

  //! \name Accessors
  //! \{

  //! Returns the CPU architecture this information relates to.
  inline Arch arch() const noexcept { return _arch; }

  //! Returns the CPU sub-architecture this information relates to.
  inline SubArch subArch() const noexcept { return _subArch; }

  //! Returns whether the CPU was detected successfully.
  //!
  //! If the returned value is false it means that AsmJit either failed to detect the CPU or it doesn't have
  //! implementation targeting the host architecture and operating system.
  inline bool wasDetected() const noexcept { return _wasDetected; }

  //! Returns the CPU family ID.
  //!
  //! Family identifier matches the FamilyId read by using CPUID on X86 architecture.
  inline uint32_t familyId() const noexcept { return _familyId; }

  //! Returns the CPU model ID.
  //!
  //! Family identifier matches the ModelId read by using CPUID on X86 architecture.

  inline uint32_t modelId() const noexcept { return _modelId; }
  //! Returns the CPU brand id.
  //!
  //! Family identifier matches the BrandId read by using CPUID on X86 architecture.
  inline uint32_t brandId() const noexcept { return _brandId; }

  //! Returns the CPU stepping.
  //!
  //! Family identifier matches the Stepping information read by using CPUID on X86 architecture.
  inline uint32_t stepping() const noexcept { return _stepping; }

  //! Returns the processor type.
  //!
  //! Family identifier matches the ProcessorType read by using CPUID on X86 architecture.
  inline uint32_t processorType() const noexcept { return _processorType; }

  //! Returns the maximum number of logical processors.
  inline uint32_t maxLogicalProcessors() const noexcept { return _maxLogicalProcessors; }

  //! Returns the size of a cache line flush.
  inline uint32_t cacheLineSize() const noexcept { return _cacheLineSize; }

  //! Returns number of hardware threads available.
  inline uint32_t hwThreadCount() const noexcept { return _hwThreadCount; }

  //! Returns a CPU vendor string.
  inline const char* vendor() const noexcept { return _vendor.str; }
  //! Tests whether the CPU vendor string is equal to `s`.
  inline bool isVendor(const char* s) const noexcept { return _vendor.eq(s); }

  //! Returns a CPU brand string.
  inline const char* brand() const noexcept { return _brand.str; }

  //! Returns CPU features.
  inline CpuFeatures& features() noexcept { return _features; }
  //! Returns CPU features (const).
  inline const CpuFeatures& features() const noexcept { return _features; }

  //! Tests whether the CPU has the given `feature`.
  template<typename FeatureId>
  inline bool hasFeature(const FeatureId& featureId) const noexcept { return _features.has(featureId); }

  //! Adds the given CPU `featureId` to the list of features.
  template<typename... Args>
  inline void addFeature(Args&&... args) noexcept { return _features.add(std::forward<Args>(args)...); }

  //! Removes the given CPU `featureId` from the list of features.
  template<typename... Args>
  inline void removeFeature(Args&&... args) noexcept { return _features.remove(std::forward<Args>(args)...); }

  //! \}
};

//! \}

ASMJIT_END_NAMESPACE

#endif // ASMJIT_CORE_CPUINFO_H_INCLUDED
