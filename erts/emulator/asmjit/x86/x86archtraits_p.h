// This file is part of AsmJit project <https://asmjit.com>
//
// See asmjit.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#ifndef ASMJIT_X86_X86ARCHTRAITS_P_H_INCLUDED
#define ASMJIT_X86_X86ARCHTRAITS_P_H_INCLUDED

#include "../core/archtraits.h"
#include "../core/misc_p.h"
#include "../x86/x86operand.h"

ASMJIT_BEGIN_SUB_NAMESPACE(x86)

//! \cond INTERNAL
//! \addtogroup asmjit_x86
//! \{

//! X86 architecture traits (internal).
static const constexpr ArchTraits x86ArchTraits = {
  // SP/FP/LR/PC.
  Gp::kIdSp, Gp::kIdBp, 0xFF, 0xFF,

  // Reserved.
  { 0, 0, 0 },

  // HW stack alignment.
  1,

  // Min/Max stack offset
  0x7FFFFFFFu, 0x7FFFFFFFu,

  // ISA features [Gp, Vec, Other0, Other1].
  {{
    InstHints::kRegSwap | InstHints::kPushPop,
    InstHints::kNoHints,
    InstHints::kNoHints,
    InstHints::kNoHints
  }},

  // Register signatures.
  #define V(index) OperandSignature{x86::RegTraits<RegType(index)>::kSignature}
  {{ ASMJIT_LOOKUP_TABLE_32(V, 0) }},
  #undef V

  // RegTypeToTypeId.
  #define V(index) TypeId(x86::RegTraits<RegType(index)>::kTypeId)
  {{ ASMJIT_LOOKUP_TABLE_32(V, 0) }},
  #undef V

  // TypeIdToRegType.
  #define V(index) (index + uint32_t(TypeId::_kBaseStart) == uint32_t(TypeId::kInt8)    ? RegType::kX86_GpbLo : \
                    index + uint32_t(TypeId::_kBaseStart) == uint32_t(TypeId::kUInt8)   ? RegType::kX86_GpbLo : \
                    index + uint32_t(TypeId::_kBaseStart) == uint32_t(TypeId::kInt16)   ? RegType::kX86_Gpw   : \
                    index + uint32_t(TypeId::_kBaseStart) == uint32_t(TypeId::kUInt16)  ? RegType::kX86_Gpw   : \
                    index + uint32_t(TypeId::_kBaseStart) == uint32_t(TypeId::kInt32)   ? RegType::kX86_Gpd   : \
                    index + uint32_t(TypeId::_kBaseStart) == uint32_t(TypeId::kUInt32)  ? RegType::kX86_Gpd   : \
                    index + uint32_t(TypeId::_kBaseStart) == uint32_t(TypeId::kIntPtr)  ? RegType::kX86_Gpd   : \
                    index + uint32_t(TypeId::_kBaseStart) == uint32_t(TypeId::kUIntPtr) ? RegType::kX86_Gpd   : \
                    index + uint32_t(TypeId::_kBaseStart) == uint32_t(TypeId::kFloat32) ? RegType::kX86_Xmm   : \
                    index + uint32_t(TypeId::_kBaseStart) == uint32_t(TypeId::kFloat64) ? RegType::kX86_Xmm   : \
                    index + uint32_t(TypeId::_kBaseStart) == uint32_t(TypeId::kMask8)   ? RegType::kX86_KReg  : \
                    index + uint32_t(TypeId::_kBaseStart) == uint32_t(TypeId::kMask16)  ? RegType::kX86_KReg  : \
                    index + uint32_t(TypeId::_kBaseStart) == uint32_t(TypeId::kMask32)  ? RegType::kX86_KReg  : \
                    index + uint32_t(TypeId::_kBaseStart) == uint32_t(TypeId::kMask64)  ? RegType::kX86_KReg  : \
                    index + uint32_t(TypeId::_kBaseStart) == uint32_t(TypeId::kMmx32)   ? RegType::kX86_Mm    : \
                    index + uint32_t(TypeId::_kBaseStart) == uint32_t(TypeId::kMmx64)   ? RegType::kX86_Mm    : RegType::kNone)
  {{ ASMJIT_LOOKUP_TABLE_32(V, 0) }},
  #undef V

  // Word names of 8-bit, 16-bit, 32-bit, and 64-bit quantities.
  {
    ArchTypeNameId::kDB,
    ArchTypeNameId::kDW,
    ArchTypeNameId::kDD,
    ArchTypeNameId::kDQ
  }
};

//! X64 architecture traits (internal).
static const constexpr ArchTraits x64ArchTraits = {
  // SP/FP/LR/PC.
  Gp::kIdSp, Gp::kIdBp, 0xFF, 0xFF,

  // Reserved.
  { 0, 0, 0 },

  // HW stack alignment.
  1,

  // Min/Max stack offset
  0x7FFFFFFFu, 0x7FFFFFFFu,

  // ISA features [Gp, Vec, Other0, Other1].
  {{
    InstHints::kRegSwap | InstHints::kPushPop,
    InstHints::kNoHints,
    InstHints::kNoHints,
    InstHints::kNoHints
  }},

  // Register signatures.
  #define V(index) OperandSignature{x86::RegTraits<RegType(index)>::kSignature}
  {{ ASMJIT_LOOKUP_TABLE_32(V, 0) }},
  #undef V

  // RegTypeToTypeId.
  #define V(index) TypeId(x86::RegTraits<RegType(index)>::kTypeId)
  {{ ASMJIT_LOOKUP_TABLE_32(V, 0) }},
  #undef V

  // TypeIdToRegType.
  #define V(index) (index + uint32_t(TypeId::_kBaseStart) == uint32_t(TypeId::kInt8)    ? RegType::kX86_GpbLo : \
                    index + uint32_t(TypeId::_kBaseStart) == uint32_t(TypeId::kUInt8)   ? RegType::kX86_GpbLo : \
                    index + uint32_t(TypeId::_kBaseStart) == uint32_t(TypeId::kInt16)   ? RegType::kX86_Gpw   : \
                    index + uint32_t(TypeId::_kBaseStart) == uint32_t(TypeId::kUInt16)  ? RegType::kX86_Gpw   : \
                    index + uint32_t(TypeId::_kBaseStart) == uint32_t(TypeId::kInt32)   ? RegType::kX86_Gpd   : \
                    index + uint32_t(TypeId::_kBaseStart) == uint32_t(TypeId::kUInt32)  ? RegType::kX86_Gpd   : \
                    index + uint32_t(TypeId::_kBaseStart) == uint32_t(TypeId::kInt64)   ? RegType::kX86_Gpq   : \
                    index + uint32_t(TypeId::_kBaseStart) == uint32_t(TypeId::kUInt64)  ? RegType::kX86_Gpq   : \
                    index + uint32_t(TypeId::_kBaseStart) == uint32_t(TypeId::kIntPtr)  ? RegType::kX86_Gpd   : \
                    index + uint32_t(TypeId::_kBaseStart) == uint32_t(TypeId::kUIntPtr) ? RegType::kX86_Gpd   : \
                    index + uint32_t(TypeId::_kBaseStart) == uint32_t(TypeId::kFloat32) ? RegType::kX86_Xmm   : \
                    index + uint32_t(TypeId::_kBaseStart) == uint32_t(TypeId::kFloat64) ? RegType::kX86_Xmm   : \
                    index + uint32_t(TypeId::_kBaseStart) == uint32_t(TypeId::kMask8)   ? RegType::kX86_KReg  : \
                    index + uint32_t(TypeId::_kBaseStart) == uint32_t(TypeId::kMask16)  ? RegType::kX86_KReg  : \
                    index + uint32_t(TypeId::_kBaseStart) == uint32_t(TypeId::kMask32)  ? RegType::kX86_KReg  : \
                    index + uint32_t(TypeId::_kBaseStart) == uint32_t(TypeId::kMask64)  ? RegType::kX86_KReg  : \
                    index + uint32_t(TypeId::_kBaseStart) == uint32_t(TypeId::kMmx32)   ? RegType::kX86_Mm    : \
                    index + uint32_t(TypeId::_kBaseStart) == uint32_t(TypeId::kMmx64)   ? RegType::kX86_Mm    : RegType::kNone)
  {{ ASMJIT_LOOKUP_TABLE_32(V, 0) }},
  #undef V

  // Word names of 8-bit, 16-bit, 32-bit, and 64-bit quantities.
  {
    ArchTypeNameId::kDB,
    ArchTypeNameId::kDW,
    ArchTypeNameId::kDD,
    ArchTypeNameId::kDQ
  }
};

//! \}
//! \endcond

ASMJIT_END_SUB_NAMESPACE

#endif // ASMJIT_X86_X86ARCHTRAITS_P_H_INCLUDED
