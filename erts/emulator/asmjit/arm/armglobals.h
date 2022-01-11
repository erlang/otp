// This file is part of AsmJit project <https://asmjit.com>
//
// See asmjit.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#ifndef ASMJIT_ARM_ARMGLOBALS_H_INCLUDED
#define ASMJIT_ARM_ARMGLOBALS_H_INCLUDED

#include "../core/archcommons.h"
#include "../core/inst.h"

//! \namespace asmjit::arm
//! \ingroup asmjit_arm
//!
//! ARM API shared between 32-bit (ARM/AArch32) and 64-bit (AArch64) backends.

ASMJIT_BEGIN_SUB_NAMESPACE(arm)

//! \addtogroup asmjit_arm
//! \{

//! Condition code (ARM|AArch64).
enum class CondCode : uint8_t {
  kEQ             = 0x00u,      //!<        Z==1         (any_sign ==)
  kNE             = 0x01u,      //!<        Z==0         (any_sign !=)
  kCS             = 0x02u,      //!< C==1                (unsigned >=)
  kHS             = 0x02u,      //!< C==1                (unsigned >=)
  kCC             = 0x03u,      //!< C==0                (unsigned < )
  kLO             = 0x03u,      //!< C==0                (unsigned < )
  kMI             = 0x04u,      //!<               N==1  (is negative)
  kPL             = 0x05u,      //!<               N==0  (is positive or zero)
  kVS             = 0x06u,      //!<               V==1  (is overflow)
  kVC             = 0x07u,      //!<               V==0  (no overflow)
  kHI             = 0x08u,      //!< C==1 & Z==0         (unsigned > )
  kLS             = 0x09u,      //!< C==0 | Z==1         (unsigned <=)
  kGE             = 0x0Au,      //!<               N==V  (signed   >=)
  kLT             = 0x0Bu,      //!<               N!=V  (signed   < )
  kGT             = 0x0Cu,      //!<        Z==0 & N==V  (signed   > )
  kLE             = 0x0Du,      //!<        Z==1 | N!=V  (signed   <=)
  kAL             = 0x0Eu,      //!< (no condition code) (always)
  kNA             = 0x0Fu,      //!< (not available)     (special)

  kSign           = kMI,        //!< Sign.
  kNotSign        = kPL,        //!< Not sign.

  kOverflow       = kVS,        //!< Signed overflow.
  kNotOverflow    = kVC,        //!< Not signed overflow.

  kEqual          = kEQ,        //!< Equal     `a == b`.
  kNotEqual       = kNE,        //!< Not Equal `a != b`.

  kZero           = kEQ,        //!< Zero (alias to equal).
  kNotZero        = kNE,        //!< Not Zero (alias to Not Equal).

  kNegative       = kMI,        //!< Negative.
  kPositive       = kPL,        //!< Positive or zero.

  kSignedLT       = kLT,        //!< Signed    `a <  b`.
  kSignedLE       = kLE,        //!< Signed    `a <= b`.
  kSignedGT       = kGT,        //!< Signed    `a >  b`.
  kSignedGE       = kGE,        //!< Signed    `a >= b`.

  kUnsignedLT     = kLO,        //!< Unsigned  `a <  b`.
  kUnsignedLE     = kLS,        //!< Unsigned  `a <= b`.
  kUnsignedGT     = kHI,        //!< Unsigned  `a >  b`.
  kUnsignedGE     = kHS,        //!< Unsigned  `a >= b`.

  kAlways         = kAL,        //!< No condition code (always).

  kMaxValue       = 0x0Fu       //!< Maximum value of `CondCode`.
};

//! Negates a condition code.
static constexpr CondCode negateCond(CondCode cond) noexcept { return CondCode(uint8_t(cond) ^ uint8_t(1)); }

//! \}

ASMJIT_END_SUB_NAMESPACE

#endif // ASMJIT_ARM_ARMGLOBALS_H_INCLUDED
