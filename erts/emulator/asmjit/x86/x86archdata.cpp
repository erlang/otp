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

#include "../core/api-build_p.h"
#ifdef ASMJIT_BUILD_X86

#include "../core/support.h"
#include "../core/type.h"
#include "../x86/x86archdata_p.h"
#include "../x86/x86operand.h"

ASMJIT_BEGIN_SUB_NAMESPACE(x86)

// ============================================================================
// [asmjit::x86::ArchInternal]
// ============================================================================

namespace ArchInternal {

Error typeIdToRegInfo(uint32_t arch, uint32_t typeId, uint32_t* typeIdOut, RegInfo* regInfoOut) noexcept {
  // Passed RegType instead of TypeId?
  if (typeId <= BaseReg::kTypeMax)
    typeId = opData.archRegs.regTypeToTypeId[typeId];

  if (ASMJIT_UNLIKELY(!Type::isValid(typeId)))
    return DebugUtils::errored(kErrorInvalidTypeId);

  // First normalize architecture dependent types.
  if (Type::isAbstract(typeId)) {
    bool is32Bit = arch == Environment::kArchX86;
    if (typeId == Type::kIdIntPtr)
      typeId = is32Bit ? Type::kIdI32 : Type::kIdI64;
    else
      typeId = is32Bit ? Type::kIdU32 : Type::kIdU64;
  }

  // Type size helps to construct all groups of registers.
  // TypeId is invalid if the size is zero.
  uint32_t size = Type::sizeOf(typeId);
  if (ASMJIT_UNLIKELY(!size))
    return DebugUtils::errored(kErrorInvalidTypeId);

  if (ASMJIT_UNLIKELY(typeId == Type::kIdF80))
    return DebugUtils::errored(kErrorInvalidUseOfF80);

  uint32_t regType = 0;

  switch (typeId) {
    case Type::kIdI8:
    case Type::kIdU8:
      regType = Reg::kTypeGpbLo;
      break;

    case Type::kIdI16:
    case Type::kIdU16:
      regType = Reg::kTypeGpw;
      break;

    case Type::kIdI32:
    case Type::kIdU32:
      regType = Reg::kTypeGpd;
      break;

    case Type::kIdI64:
    case Type::kIdU64:
      if (arch == Environment::kArchX86)
        return DebugUtils::errored(kErrorInvalidUseOfGpq);

      regType = Reg::kTypeGpq;
      break;

    // F32 and F64 are always promoted to use vector registers.
    case Type::kIdF32:
      typeId = Type::kIdF32x1;
      regType = Reg::kTypeXmm;
      break;

    case Type::kIdF64:
      typeId = Type::kIdF64x1;
      regType = Reg::kTypeXmm;
      break;

    // Mask registers {k}.
    case Type::kIdMask8:
    case Type::kIdMask16:
    case Type::kIdMask32:
    case Type::kIdMask64:
      regType = Reg::kTypeKReg;
      break;

    // MMX registers.
    case Type::kIdMmx32:
    case Type::kIdMmx64:
      regType = Reg::kTypeMm;
      break;

    // XMM|YMM|ZMM registers.
    default:
      if (size <= 16)
        regType = Reg::kTypeXmm;
      else if (size == 32)
        regType = Reg::kTypeYmm;
      else
        regType = Reg::kTypeZmm;
      break;
  }

  *typeIdOut = typeId;
  regInfoOut->reset(opData.archRegs.regInfo[regType].signature());
  return kErrorOk;
}

} // {ArchInternal}

ASMJIT_END_SUB_NAMESPACE

#endif // ASMJIT_BUILD_X86
