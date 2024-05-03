// This file is part of AsmJit project <https://asmjit.com>
//
// See asmjit.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#include "../core/api-build_p.h"
#include "../core/archtraits.h"
#include "../core/inst.h"

#if !defined(ASMJIT_NO_X86)
  #include "../x86/x86instapi_p.h"
#endif

#if !defined(ASMJIT_NO_AARCH64)
  #include "../arm/a64instapi_p.h"
#endif

ASMJIT_BEGIN_NAMESPACE

// InstAPI - InstId <-> String
// ===========================

#ifndef ASMJIT_NO_TEXT
Error InstAPI::instIdToString(Arch arch, InstId instId, String& output) noexcept {
#if !defined(ASMJIT_NO_X86)
  if (Environment::isFamilyX86(arch))
    return x86::InstInternal::instIdToString(instId, output);
#endif

#if !defined(ASMJIT_NO_AARCH64)
  if (Environment::isFamilyAArch64(arch))
    return a64::InstInternal::instIdToString(instId, output);
#endif

  return DebugUtils::errored(kErrorInvalidArch);
}

InstId InstAPI::stringToInstId(Arch arch, const char* s, size_t len) noexcept {
#if !defined(ASMJIT_NO_X86)
  if (Environment::isFamilyX86(arch))
    return x86::InstInternal::stringToInstId(s, len);
#endif

#if !defined(ASMJIT_NO_AARCH64)
  if (Environment::isFamilyAArch64(arch))
    return a64::InstInternal::stringToInstId(s, len);
#endif

  return 0;
}
#endif // !ASMJIT_NO_TEXT

// InstAPI - Validate
// ==================

#ifndef ASMJIT_NO_VALIDATION
Error InstAPI::validate(Arch arch, const BaseInst& inst, const Operand_* operands, size_t opCount, ValidationFlags validationFlags) noexcept {
#if !defined(ASMJIT_NO_X86)
  if (Environment::isFamilyX86(arch)) {
    if (arch == Arch::kX86)
      return x86::InstInternal::validateX86(inst, operands, opCount, validationFlags);
    else
      return x86::InstInternal::validateX64(inst, operands, opCount, validationFlags);
  }
#endif

#if !defined(ASMJIT_NO_AARCH64)
  if (Environment::isFamilyAArch64(arch))
    return a64::InstInternal::validate(inst, operands, opCount, validationFlags);
#endif

  return DebugUtils::errored(kErrorInvalidArch);
}
#endif // !ASMJIT_NO_VALIDATION

// InstAPI - QueryRWInfo
// =====================

#ifndef ASMJIT_NO_INTROSPECTION
Error InstAPI::queryRWInfo(Arch arch, const BaseInst& inst, const Operand_* operands, size_t opCount, InstRWInfo* out) noexcept {
  if (ASMJIT_UNLIKELY(opCount > Globals::kMaxOpCount))
    return DebugUtils::errored(kErrorInvalidArgument);

#if !defined(ASMJIT_NO_X86)
  if (Environment::isFamilyX86(arch))
    return x86::InstInternal::queryRWInfo(arch, inst, operands, opCount, out);
#endif

#if !defined(ASMJIT_NO_AARCH64)
  if (Environment::isFamilyAArch64(arch))
    return a64::InstInternal::queryRWInfo(inst, operands, opCount, out);
#endif

  return DebugUtils::errored(kErrorInvalidArch);
}
#endif // !ASMJIT_NO_INTROSPECTION

// InstAPI - QueryFeatures
// =======================

#ifndef ASMJIT_NO_INTROSPECTION
Error InstAPI::queryFeatures(Arch arch, const BaseInst& inst, const Operand_* operands, size_t opCount, CpuFeatures* out) noexcept {
#if !defined(ASMJIT_NO_X86)
  if (Environment::isFamilyX86(arch))
    return x86::InstInternal::queryFeatures(arch, inst, operands, opCount, out);
#endif

#if !defined(ASMJIT_NO_AARCH64)
  if (Environment::isFamilyAArch64(arch))
    return a64::InstInternal::queryFeatures(inst, operands, opCount, out);
#endif

  return DebugUtils::errored(kErrorInvalidArch);
}
#endif // !ASMJIT_NO_INTROSPECTION

ASMJIT_END_NAMESPACE
