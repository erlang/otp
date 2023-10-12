// This file is part of AsmJit project <https://asmjit.com>
//
// See asmjit.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#ifndef ASMJIT_CORE_EMITHELPER_P_H_INCLUDED
#define ASMJIT_CORE_EMITHELPER_P_H_INCLUDED

#include "../core/emitter.h"
#include "../core/operand.h"
#include "../core/type.h"

ASMJIT_BEGIN_NAMESPACE

//! \cond INTERNAL
//! \addtogroup asmjit_core
//! \{

//! Helper class that provides utilities for each supported architecture.
class BaseEmitHelper {
public:
  BaseEmitter* _emitter;

  inline explicit BaseEmitHelper(BaseEmitter* emitter = nullptr) noexcept
    : _emitter(emitter) {}

  inline BaseEmitter* emitter() const noexcept { return _emitter; }
  inline void setEmitter(BaseEmitter* emitter) noexcept { _emitter = emitter; }

  //! Emits a pure move operation between two registers or the same type or between a register and its home
  //! slot. This function does not handle register conversion.
  virtual Error emitRegMove(
    const Operand_& dst_,
    const Operand_& src_, TypeId typeId, const char* comment = nullptr) = 0;

  //! Emits swap between two registers.
  virtual Error emitRegSwap(
    const BaseReg& a,
    const BaseReg& b, const char* comment = nullptr) = 0;

  //! Emits move from a function argument (either register or stack) to a register.
  //!
  //! This function can handle the necessary conversion from one argument to another, and from one register type
  //! to another, if it's possible. Any attempt of conversion that requires third register of a different group
  //! (for example conversion from K to MMX on X86/X64) will fail.
  virtual Error emitArgMove(
    const BaseReg& dst_, TypeId dstTypeId,
    const Operand_& src_, TypeId srcTypeId, const char* comment = nullptr) = 0;

  Error emitArgsAssignment(const FuncFrame& frame, const FuncArgsAssignment& args);
};

//! \}
//! \endcond

ASMJIT_END_NAMESPACE

#endif // ASMJIT_CORE_EMITHELPER_P_H_INCLUDED
