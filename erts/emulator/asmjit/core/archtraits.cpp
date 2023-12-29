// This file is part of AsmJit project <https://asmjit.com>
//
// See asmjit.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#include "../core/api-build_p.h"
#include "../core/archtraits.h"
#include "../core/environment.h"
#include "../core/misc_p.h"

#if !defined(ASMJIT_NO_X86)
  #include "../x86/x86archtraits_p.h"
#endif

#if !defined(ASMJIT_NO_AARCH64)
  #include "../arm/a64archtraits_p.h"
#endif

ASMJIT_BEGIN_NAMESPACE

static const constexpr ArchTraits noArchTraits = {
  // SP/FP/LR/PC.
  0xFF, 0xFF, 0xFF, 0xFF,

  // Reserved,
  { 0, 0, 0 },

  // HW stack alignment.
  0,

  // Min/Max stack offset.
  0, 0,

  // ISA features [Gp, Vec, Other0, Other1].
  {{
    InstHints::kNoHints,
    InstHints::kNoHints,
    InstHints::kNoHints,
    InstHints::kNoHints
  }},

  // RegTypeToSignature.
  #define V(index) OperandSignature{0}
  {{ ASMJIT_LOOKUP_TABLE_32(V, 0) }},
  #undef V

  // RegTypeToTypeId.
  #define V(index) TypeId::kVoid
  {{ ASMJIT_LOOKUP_TABLE_32(V, 0) }},
  #undef V

  // TypeIdToRegType.
  #define V(index) RegType::kNone
  {{ ASMJIT_LOOKUP_TABLE_32(V, 0) }},
  #undef V

  // Word names of 8-bit, 16-bit, 32-bit, and 64-bit quantities.
  {
    ArchTypeNameId::kByte,
    ArchTypeNameId::kHalf,
    ArchTypeNameId::kWord,
    ArchTypeNameId::kQuad
  }
};

ASMJIT_VARAPI const ArchTraits _archTraits[uint32_t(Arch::kMaxValue) + 1] = {
  // No architecture.
  noArchTraits,

  // X86/X86 architectures.
#if !defined(ASMJIT_NO_X86)
  x86::x86ArchTraits,
  x86::x64ArchTraits,
#else
  noArchTraits,
  noArchTraits,
#endif

  // RISCV32/RISCV64 architectures.
  noArchTraits,
  noArchTraits,

  // ARM architecture
  noArchTraits,

  // AArch64 architecture.
#if !defined(ASMJIT_NO_AARCH64)
  a64::a64ArchTraits,
#else
  noArchTraits,
#endif

  // ARM/Thumb architecture.
  noArchTraits,

  // Reserved.
  noArchTraits,

  // MIPS32/MIPS64
  noArchTraits,
  noArchTraits
};

ASMJIT_FAVOR_SIZE Error ArchUtils::typeIdToRegSignature(Arch arch, TypeId typeId, TypeId* typeIdOut, OperandSignature* regSignatureOut) noexcept {
  const ArchTraits& archTraits = ArchTraits::byArch(arch);

  // TODO: Remove this, should never be used like this.
  // Passed RegType instead of TypeId?
  if (uint32_t(typeId) <= uint32_t(RegType::kMaxValue))
    typeId = archTraits.regTypeToTypeId(RegType(uint32_t(typeId)));

  if (ASMJIT_UNLIKELY(!TypeUtils::isValid(typeId)))
    return DebugUtils::errored(kErrorInvalidTypeId);

  // First normalize architecture dependent types.
  if (TypeUtils::isAbstract(typeId)) {
    bool is32Bit = Environment::is32Bit(arch);
    if (typeId == TypeId::kIntPtr)
      typeId = is32Bit ? TypeId::kInt32 : TypeId::kInt64;
    else
      typeId = is32Bit ? TypeId::kUInt32 : TypeId::kUInt64;
  }

  // Type size helps to construct all groups of registers.
  // TypeId is invalid if the size is zero.
  uint32_t size = TypeUtils::sizeOf(typeId);
  if (ASMJIT_UNLIKELY(!size))
    return DebugUtils::errored(kErrorInvalidTypeId);

  if (ASMJIT_UNLIKELY(typeId == TypeId::kFloat80))
    return DebugUtils::errored(kErrorInvalidUseOfF80);

  RegType regType = RegType::kNone;
  if (TypeUtils::isBetween(typeId, TypeId::_kBaseStart, TypeId::_kVec32Start)) {
    regType = archTraits._typeIdToRegType[uint32_t(typeId) - uint32_t(TypeId::_kBaseStart)];
    if (regType == RegType::kNone) {
      if (typeId == TypeId::kInt64 || typeId == TypeId::kUInt64)
        return DebugUtils::errored(kErrorInvalidUseOfGpq);
      else
        return DebugUtils::errored(kErrorInvalidTypeId);
    }
  }
  else {
    if (size <= 8 && archTraits._regSignature[RegType::kVec64].isValid())
      regType = RegType::kVec64;
    else if (size <= 16 && archTraits._regSignature[RegType::kVec128].isValid())
      regType = RegType::kVec128;
    else if (size == 32 && archTraits._regSignature[RegType::kVec256].isValid())
      regType = RegType::kVec256;
    else if (archTraits._regSignature[RegType::kVec512].isValid())
      regType = RegType::kVec512;
    else
      return DebugUtils::errored(kErrorInvalidTypeId);
  }

  *typeIdOut = typeId;
  *regSignatureOut = archTraits.regTypeToSignature(regType);
  return kErrorOk;
}

ASMJIT_END_NAMESPACE
