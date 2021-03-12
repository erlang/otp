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

#ifndef ASMJIT_ARM_ARMFEATURES_H_INCLUDED
#define ASMJIT_ARM_ARMFEATURES_H_INCLUDED

#include "../core/features.h"

ASMJIT_BEGIN_SUB_NAMESPACE(arm)

//! \addtogroup asmjit_arm
//! \{

// ============================================================================
// [asmjit::arm::Features]
// ============================================================================

//! CPU features (ARM).
class Features : public BaseFeatures {
public:
  //! CPU feature IDs (ARM).
  enum Id : uint32_t {
    // @EnumValuesBegin{"enum": "arm::Features::Id"}@

    kNone = 0,                 //!< No feature (never set, used internally).

    kTHUMB,                    //!< THUMB v1 ISA available.
    kTHUMBv2,                  //!< THUMB v2 ISA available.

    kARMv6,                    //!< ARMv6 ISA available.
    kARMv7,                    //!< ARMv7 ISA available.
    kARMv8,                    //!< ARMv8 ISA available.
    kARMv8_1,                  //!< ARMv8.1 ISA available.
    kARMv8_2,                  //!< ARMv8.2 ISA available.
    kARMv8_3,                  //!< ARMv8.3 ISA available.
    kARMv8_4,                  //!< ARMv8.4 ISA available.
    kARMv8_5,                  //!< ARMv8.5 ISA available.
    kARMv8_6,                  //!< ARMv8.6 ISA available.

    kVFPv2,                    //!< CPU has VFPv2 instruction set.
    kVFPv3,                    //!< CPU has VFPv3 instruction set.
    kVFPv4,                    //!< CPU has VFPv4 instruction set.
    kVFP_D32,                  //!< CPU has 32 VFP-D (64-bit) registers.

    kAES,                      //!< CPU has AES instructions (AArch64 only).
    kASIMD,                    //!< CPU has Advanced SIMD (NEON on ARM/THUMB).
    kATOMICS,                  //!< CPU has 64-bit atomics (AArch64 only).
    kBF16,                     //!< CPU has BF16 instructions (AArch64 only).
    kCPUID,                    //!< CPU has CPUID register (ID_AA64ZFR0_EL1).
    kCRC32,                    //!< CPU has CRC32 instructions.
    kDGH,                      //!< CPU has DGH instruction (AArch64 only).
    kDOTPROD,                  //!< CPU has DOTPROD instructions (SDOT/UDOT).
    kEDSP,                     //!< CPU has EDSP extensions (ARM/THUMB only).
    kFCMA,                     //!< CPU has FCMA instructions (FCADD/FCMLA).
    kFJCVTZS,                  //!< CPU has FJCVTZS instruction (AArch64 only).
    kFLAGM,                    //!< CPU has FLAGM instructions (AArch64 only).
    kFLAGM2,                   //!< CPU has FLAGM2 instructions (AArch64 only).
    kFP16CONV,                 //!< CPU has FP16 (half-float) conversion instructions.
    kFP16FML,                  //!< CPU has FMLAL{2}/FMLSL{2} instructions.
    kFP16FULL,                 //!< CPU has full support for FP16 instructions.
    kFRINT,                    //!< CPU has FRINT[32|64][X|Z] instructions (AArch64 only).
    kI8MM,                     //!< CPU has I8MM instructions (AArch64 only).
    kIDIVA,                    //!< CPU has hardware SDIV and UDIV (ARM mode).
    kIDIVT,                    //!< CPU has hardware SDIV and UDIV (THUMB mode).
    kMTE,                      //!< CPU has MTE instructions (AArch64 only).
    kRDMA,                     //!< CPU has RDMA instructions (AArch64 only).
    kPMULL,                    //!< CPU has PMULL instructions (AArch64 only).
    kSB,                       //!< CPU has SB (speculative barrier) instruction (AArch64 only).
    kSHA1,                     //!< CPU has SHA1 instructions.
    kSHA2,                     //!< CPU has SHA2 instructions.
    kSHA3,                     //!< CPU has SHA3 instructions.
    kSHA512,                   //!< CPU has SHA512 instructions.
    kSM3,                      //!< CPU has SM3 instructions.
    kSM4,                      //!< CPU has SM4 instructions.
    kSSBS,                     //!< CPU has SSBS instructions.
    kSVE,                      //!< CPU has SVE instructions (AArch64 only).
    kSVE_BF16,                 //!< CPU has SVE-BF16 instructions.
    kSVE_F32MM,                //!< CPU has SVE-F32MM instructions.
    kSVE_F64MM,                //!< CPU has SVE-F64MM instructions.
    kSVE_I8MM,                 //!< CPU has SVE-I8MM instructions.
    kSVE_PMULL,                //!< CPU has SVE-PMULL instructions.
    kSVE2,                     //!< CPU has SVE2 instructions (AArch64 only).
    kSVE2_AES,                 //!< CPU has SVE2-AES instructions.
    kSVE2_BITPERM,             //!< CPU has SVE2-BITPERM instructions.
    kSVE2_SHA3,                //!< CPU has SVE2-SHA3 instructions.
    kSVE2_SM4,                 //!< CPU has SVE2-SM4 instructions.

    // @EnumValuesEnd@

    kCount                     //!< Count of ARM CPU features.
  };

  //! \name Construction / Destruction
  //! \{

  inline Features() noexcept
    : BaseFeatures() {}

  inline Features(const Features& other) noexcept
    : BaseFeatures(other) {}

  //! \}

  //! \name Overloaded Operators
  //! \{

  inline Features& operator=(const Features& other) noexcept = default;

  //! \}

  //! \name Accessors
  //! \{

  #define ASMJIT_ARM_FEATURE(FEATURE) \
    inline bool has##FEATURE() const noexcept { return has(k##FEATURE); }

  ASMJIT_ARM_FEATURE(THUMB)
  ASMJIT_ARM_FEATURE(THUMBv2)

  ASMJIT_ARM_FEATURE(ARMv6)
  ASMJIT_ARM_FEATURE(ARMv7)
  ASMJIT_ARM_FEATURE(ARMv8)
  ASMJIT_ARM_FEATURE(ARMv8_1)
  ASMJIT_ARM_FEATURE(ARMv8_2)
  ASMJIT_ARM_FEATURE(ARMv8_3)
  ASMJIT_ARM_FEATURE(ARMv8_4)
  ASMJIT_ARM_FEATURE(ARMv8_5)
  ASMJIT_ARM_FEATURE(ARMv8_6)

  ASMJIT_ARM_FEATURE(VFPv2)
  ASMJIT_ARM_FEATURE(VFPv3)
  ASMJIT_ARM_FEATURE(VFPv4)
  ASMJIT_ARM_FEATURE(VFP_D32)

  ASMJIT_ARM_FEATURE(AES)
  ASMJIT_ARM_FEATURE(ASIMD)
  ASMJIT_ARM_FEATURE(ATOMICS)
  ASMJIT_ARM_FEATURE(BF16)
  ASMJIT_ARM_FEATURE(CPUID)
  ASMJIT_ARM_FEATURE(CRC32)
  ASMJIT_ARM_FEATURE(DOTPROD)
  ASMJIT_ARM_FEATURE(EDSP)
  ASMJIT_ARM_FEATURE(FCMA)
  ASMJIT_ARM_FEATURE(FLAGM)
  ASMJIT_ARM_FEATURE(FLAGM2)
  ASMJIT_ARM_FEATURE(FP16CONV)
  ASMJIT_ARM_FEATURE(FP16FML)
  ASMJIT_ARM_FEATURE(FP16FULL)
  ASMJIT_ARM_FEATURE(FRINT)
  ASMJIT_ARM_FEATURE(IDIVA)
  ASMJIT_ARM_FEATURE(IDIVT)
  ASMJIT_ARM_FEATURE(MTE)
  ASMJIT_ARM_FEATURE(FJCVTZS)
  ASMJIT_ARM_FEATURE(I8MM)
  ASMJIT_ARM_FEATURE(PMULL)
  ASMJIT_ARM_FEATURE(RDMA)
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

  #undef ASMJIT_ARM_FEATURE

  //! \}
};

//! \}

ASMJIT_END_SUB_NAMESPACE

#endif // ASMJIT_ARM_ARMFEATURES_H_INCLUDED
