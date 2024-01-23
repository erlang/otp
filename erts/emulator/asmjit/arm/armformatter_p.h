// This file is part of AsmJit project <https://asmjit.com>
//
// See asmjit.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#ifndef ASMJIT_ARM_ARMFORMATTER_P_H_INCLUDED
#define ASMJIT_ARM_ARMFORMATTER_P_H_INCLUDED

#include "../core/api-config.h"
#ifndef ASMJIT_NO_LOGGING

#include "../core/formatter.h"
#include "../core/string.h"
#include "../arm/armglobals.h"

ASMJIT_BEGIN_SUB_NAMESPACE(arm)

//! \cond INTERNAL
//! \addtogroup asmjit_arm
//! \{

namespace FormatterInternal {

Error ASMJIT_CDECL formatFeature(
  String& sb,
  uint32_t featureId) noexcept;

Error ASMJIT_CDECL formatCondCode(
  String& sb,
  CondCode cc) noexcept;

Error ASMJIT_CDECL formatShiftOp(
  String& sb,
  ShiftOp shiftOp) noexcept;

Error ASMJIT_CDECL formatRegister(
  String& sb,
  FormatFlags flags,
  const BaseEmitter* emitter,
  Arch arch,
  RegType regType,
  uint32_t rId,
  uint32_t elementType = 0,
  uint32_t elementIndex = 0xFFFFFFFF) noexcept;

Error ASMJIT_CDECL formatRegisterList(
  String& sb,
  FormatFlags flags,
  const BaseEmitter* emitter,
  Arch arch,
  RegType regType,
  uint32_t rMask) noexcept;

Error ASMJIT_CDECL formatOperand(
  String& sb,
  FormatFlags flags,
  const BaseEmitter* emitter,
  Arch arch,
  const Operand_& op) noexcept;

} // {FormatterInternal}

//! \}
//! \endcond

ASMJIT_END_SUB_NAMESPACE

#endif // !ASMJIT_NO_LOGGING
#endif // ASMJIT_ARM_ARMFORMATTER_P_H_INCLUDED
