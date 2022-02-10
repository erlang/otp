// This file is part of AsmJit project <https://asmjit.com>
//
// See asmjit.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#ifndef ASMJIT_ARM_A64FORMATTER_P_H_INCLUDED
#define ASMJIT_ARM_A64FORMATTER_P_H_INCLUDED

#include "../core/api-config.h"
#ifndef ASMJIT_NO_LOGGING

#include "../core/formatter.h"
#include "../core/string.h"
#include "../arm/armformatter_p.h"
#include "../arm/a64globals.h"

ASMJIT_BEGIN_SUB_NAMESPACE(a64)

//! \cond INTERNAL
//! \addtogroup asmjit_a64
//! \{

namespace FormatterInternal {

using namespace arm::FormatterInternal;

Error ASMJIT_CDECL formatRegister(
  String& sb,
  FormatFlags flags,
  const BaseEmitter* emitter,
  Arch arch,
  RegType regType,
  uint32_t regId,
  uint32_t elementType = 0,
  uint32_t elementIndex = 0xFFFFFFFFu) noexcept;

Error ASMJIT_CDECL formatOperand(
  String& sb,
  FormatFlags flags,
  const BaseEmitter* emitter,
  Arch arch,
  const Operand_& op) noexcept;

Error ASMJIT_CDECL formatInstruction(
  String& sb,
  FormatFlags flags,
  const BaseEmitter* emitter,
  Arch arch,
  const BaseInst& inst, const Operand_* operands, size_t opCount) noexcept;

} // {FormatterInternal}

//! \}
//! \endcond

ASMJIT_END_SUB_NAMESPACE

#endif // !ASMJIT_NO_LOGGING
#endif // ASMJIT_ARM_A64FORMATTER_P_H_INCLUDED
