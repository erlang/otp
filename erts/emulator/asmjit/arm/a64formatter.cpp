// This file is part of AsmJit project <https://asmjit.com>
//
// See asmjit.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#include "../core/api-build_p.h"
#if !defined(ASMJIT_NO_AARCH64) && !defined(ASMJIT_NO_LOGGING)

#include "../core/misc_p.h"
#include "../core/support.h"
#include "../arm/a64formatter_p.h"
#include "../arm/a64instapi_p.h"
#include "../arm/a64instdb_p.h"
#include "../arm/a64operand.h"

#ifndef ASMJIT_NO_COMPILER
  #include "../core/compiler.h"
#endif

ASMJIT_BEGIN_SUB_NAMESPACE(a64)

// a64::FormatterInternal - Format Instruction
// ===========================================

ASMJIT_FAVOR_SIZE Error FormatterInternal::formatInstruction(
  String& sb,
  FormatFlags flags,
  const BaseEmitter* emitter,
  Arch arch,
  const BaseInst& inst, const Operand_* operands, size_t opCount) noexcept {

  // Format instruction options and instruction mnemonic.
  InstId instId = inst.realId();
  if (instId != Inst::kIdNone && instId < Inst::_kIdCount)
    ASMJIT_PROPAGATE(InstInternal::instIdToString(instId, sb));
  else
    ASMJIT_PROPAGATE(sb.appendFormat("[InstId=#%u]", unsigned(instId)));

  CondCode cc = inst.armCondCode();
  if (cc != CondCode::kAL) {
    ASMJIT_PROPAGATE(sb.append('.'));
    ASMJIT_PROPAGATE(formatCondCode(sb, cc));
  }

  for (uint32_t i = 0; i < opCount; i++) {
    const Operand_& op = operands[i];
    if (op.isNone())
      break;

    ASMJIT_PROPAGATE(sb.append(i == 0 ? " " : ", "));
    ASMJIT_PROPAGATE(formatOperand(sb, flags, emitter, arch, op));
  }

  return kErrorOk;
}

ASMJIT_END_SUB_NAMESPACE

#endif // !ASMJIT_NO_AARCH64 && !ASMJIT_NO_LOGGING
