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

#ifndef ASMJIT_ARM_ARMARCHTRAITS_P_H_INCLUDED
#define ASMJIT_ARM_ARMARCHTRAITS_P_H_INCLUDED

#include "../core/archtraits.h"
#include "../core/misc_p.h"
#include "../core/type.h"
#include "../arm/armoperand.h"

ASMJIT_BEGIN_SUB_NAMESPACE(arm)

//! \cond INTERNAL
//! \addtogroup asmjit_arm
//! \{

// ============================================================================
// [asmjit::arm::a64ArchTraits
// ============================================================================

static const constexpr ArchTraits a64ArchTraits = {
  Gp::kIdSp,      // SP.
  Gp::kIdFp,      // FP.
  Gp::kIdLr,      // LR.
  0xFF,           // PC.
  { 0, 0, 0 },    // Reserved.
  16,             // HW stack alignment (AArch64 requires stack aligned to 64 bytes).
  4095,           // Min stack offset - we consider the worst - byte addressing.
  65520,          // Max stack offset - we consider the best - VecQ addressing.

  // ISA features [Gp, Vec, Other0, Other1].
  {
    ArchTraits::kIsaFeaturePushPop,
    ArchTraits::kIsaFeaturePushPop,
    0,
    0
  },

  // RegInfo.
  #define V(type) { arm::RegTraits<type>::kSignature }
  { ASMJIT_LOOKUP_TABLE_32(V, 0) },
  #undef V

  // RegTypeToTypeId.
  #define V(type) arm::RegTraits<type>::kTypeId
  { ASMJIT_LOOKUP_TABLE_32(V, 0) },
  #undef V

  // TypeIdToRegType.
  #define V(index) (index + Type::_kIdBaseStart == Type::kIdI8      ? Reg::kTypeGpW   : \
                    index + Type::_kIdBaseStart == Type::kIdU8      ? Reg::kTypeGpW   : \
                    index + Type::_kIdBaseStart == Type::kIdI16     ? Reg::kTypeGpW   : \
                    index + Type::_kIdBaseStart == Type::kIdU16     ? Reg::kTypeGpW   : \
                    index + Type::_kIdBaseStart == Type::kIdI32     ? Reg::kTypeGpW   : \
                    index + Type::_kIdBaseStart == Type::kIdU32     ? Reg::kTypeGpW   : \
                    index + Type::_kIdBaseStart == Type::kIdI64     ? Reg::kTypeGpX   : \
                    index + Type::_kIdBaseStart == Type::kIdU64     ? Reg::kTypeGpX   : \
                    index + Type::_kIdBaseStart == Type::kIdIntPtr  ? Reg::kTypeGpX   : \
                    index + Type::_kIdBaseStart == Type::kIdUIntPtr ? Reg::kTypeGpX   : \
                    index + Type::_kIdBaseStart == Type::kIdF32     ? Reg::kTypeVecS  : \
                    index + Type::_kIdBaseStart == Type::kIdF64     ? Reg::kTypeVecD  : Reg::kTypeNone)
  { ASMJIT_LOOKUP_TABLE_32(V, 0) }
  #undef V
};

//! \}
//! \endcond

ASMJIT_END_SUB_NAMESPACE

#endif // ASMJIT_ARM_ARMARCHTRAITS_P_H_INCLUDED
