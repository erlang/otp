// This file is part of AsmJit project <https://asmjit.com>
//
// See asmjit.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#ifndef ASMJIT_ARM_A32ARCHTRAITS_P_H_INCLUDED
#define ASMJIT_ARM_A32ARCHTRAITS_P_H_INCLUDED

#include "../core/archtraits.h"
#include "../core/misc_p.h"
#include "../core/type.h"
#include "../arm/a32globals.h"
#include "../arm/a32operand.h"

ASMJIT_BEGIN_SUB_NAMESPACE(a32)

//! \cond INTERNAL
//! \addtogroup asmjit_a32
//! \{

static const constexpr ArchTraits a32ArchTraits = {
  // SP/FP/LR/PC.
  13, 11, 14, 15,

  // Reserved.
  { 0, 0, 0 },

  // HW stack alignment (AArch32 requires stack aligned to 4 bytes at HW level).
  4,

  // Min/max stack offset.
  0, 0, // TODO: ARM32

  // Instruction hints [Gp, Vec, ExtraVirt2, ExtraVirt3].
  {{
    InstHints::kPushPop,
    InstHints::kPushPop,
    InstHints::kNoHints,
    InstHints::kNoHints
  }},

  // RegInfo.
  #define V(index) OperandSignature{RegTraits<RegType(index)>::kSignature}
  {{ ASMJIT_LOOKUP_TABLE_32(V, 0) }},
  #undef V

  // RegTypeToTypeId.
  #define V(index) TypeId(RegTraits<RegType(index)>::kTypeId)
  {{ ASMJIT_LOOKUP_TABLE_32(V, 0) }},
  #undef V

  // TypeIdToRegType.
  #define V(index) (index + uint32_t(TypeId::_kBaseStart) == uint32_t(TypeId::kInt8)    ? RegType::kARM_GpW   : \
                    index + uint32_t(TypeId::_kBaseStart) == uint32_t(TypeId::kUInt8)   ? RegType::kARM_GpW   : \
                    index + uint32_t(TypeId::_kBaseStart) == uint32_t(TypeId::kInt16)   ? RegType::kARM_GpW   : \
                    index + uint32_t(TypeId::_kBaseStart) == uint32_t(TypeId::kUInt16)  ? RegType::kARM_GpW   : \
                    index + uint32_t(TypeId::_kBaseStart) == uint32_t(TypeId::kInt32)   ? RegType::kARM_GpW   : \
                    index + uint32_t(TypeId::_kBaseStart) == uint32_t(TypeId::kUInt32)  ? RegType::kARM_GpW   : \
                    index + uint32_t(TypeId::_kBaseStart) == uint32_t(TypeId::kIntPtr)  ? RegType::kARM_GpW   : \
                    index + uint32_t(TypeId::_kBaseStart) == uint32_t(TypeId::kUIntPtr) ? RegType::kARM_GpW   : \
                    index + uint32_t(TypeId::_kBaseStart) == uint32_t(TypeId::kFloat32) ? RegType::kARM_VecS  : \
                    index + uint32_t(TypeId::_kBaseStart) == uint32_t(TypeId::kFloat64) ? RegType::kARM_VecD  : RegType::kNone)
  {{ ASMJIT_LOOKUP_TABLE_32(V, 0) }},
  #undef V

  // Word names of 8-bit, 16-bit, 32-bit, and 64-bit quantities.
  {
    ArchTypeNameId::kByte,
    ArchTypeNameId::kHWord,
    ArchTypeNameId::kWord,
    ArchTypeNameId::kXWord
  }
};

//! \}
//! \endcond

ASMJIT_END_SUB_NAMESPACE

#endif // ASMJIT_ARM_A32ARCHTRAITS_P_H_INCLUDED
