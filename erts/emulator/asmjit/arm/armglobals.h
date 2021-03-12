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

#ifndef ASMJIT_ARM_ARMGLOBALS_H_INCLUDED
#define ASMJIT_ARM_ARMGLOBALS_H_INCLUDED

#include "../core/archcommons.h"
#include "../core/inst.h"

ASMJIT_BEGIN_SUB_NAMESPACE(arm)

//! \addtogroup asmjit_arm
//! \{

// ============================================================================
// [asmjit::arm::Cond]
// ============================================================================

//! Condition code (ARM|AArch64).
namespace Cond {
  enum Value : uint32_t {
    kEQ               = 0x00u,           //!<        Z==1         (any_sign ==)
    kNE               = 0x01u,           //!<        Z==0         (any_sign !=)
    kCS               = 0x02u,           //!< C==1                (unsigned >=)
    kHS               = 0x02u,           //!< C==1                (unsigned >=)
    kCC               = 0x03u,           //!< C==0                (unsigned < )
    kLO               = 0x03u,           //!< C==0                (unsigned < )
    kMI               = 0x04u,           //!<               N==1  (is negative)
    kPL               = 0x05u,           //!<               N==0  (is positive or zero)
    kVS               = 0x06u,           //!<               V==1  (is overflow)
    kVC               = 0x07u,           //!<               V==0  (no overflow)
    kHI               = 0x08u,           //!< C==1 & Z==0         (unsigned > )
    kLS               = 0x09u,           //!< C==0 | Z==1         (unsigned <=)
    kGE               = 0x0Au,           //!<               N==V  (signed   >=)
    kLT               = 0x0Bu,           //!<               N!=V  (signed   < )
    kGT               = 0x0Cu,           //!<        Z==0 & N==V  (signed   > )
    kLE               = 0x0Du,           //!<        Z==1 | N!=V  (signed   <=)
    kAL               = 0x0Eu,           //!< (no condition code) (always)
    kNA               = 0x0Fu,           //!< (not available)     (special)
    kCount            = 0x10u,

    kSign             = kMI,             //!< Sign.
    kNotSign          = kPL,             //!< Not sign.

    kOverflow         = kVS,             //!< Signed overflow.
    kNotOverflow      = kVC,             //!< Not signed overflow.

    kEqual            = kEQ,             //!< Equal     `a == b`.
    kNotEqual         = kNE,             //!< Not Equal `a != b`.

    kZero             = kEQ,             //!< Zero (alias to equal).
    kNotZero          = kNE,             //!< Not Zero (alias to Not Equal).

    kNegative         = kMI,             //!< Negative.
    kPositive         = kPL,             //!< Positive or zero.

    kSignedLT         = kLT,             //!< Signed    `a <  b`.
    kSignedLE         = kLE,             //!< Signed    `a <= b`.
    kSignedGT         = kGT,             //!< Signed    `a >  b`.
    kSignedGE         = kGE,             //!< Signed    `a >= b`.

    kUnsignedLT       = kLO,             //!< Unsigned  `a <  b`.
    kUnsignedLE       = kLS,             //!< Unsigned  `a <= b`.
    kUnsignedGT       = kHI,             //!< Unsigned  `a >  b`.
    kUnsignedGE       = kHS,             //!< Unsigned  `a >= b`.

    kAlways           = kAL              //!< No condition code (always).
  };

  //! Negates a condition code.
  static constexpr uint32_t negate(uint32_t cond) noexcept { return cond ^ 1u; }
}

//! \}

ASMJIT_END_SUB_NAMESPACE

#endif // ASMJIT_ARM_ARMGLOBALS_H_INCLUDED
