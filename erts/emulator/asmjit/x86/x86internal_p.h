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

#ifndef ASMJIT_X86_X86INTERNAL_P_H_INCLUDED
#define ASMJIT_X86_X86INTERNAL_P_H_INCLUDED

#include "../core/api-config.h"

#include "../core/func.h"
#include "../x86/x86emitter.h"
#include "../x86/x86operand.h"

ASMJIT_BEGIN_SUB_NAMESPACE(x86)

//! \cond INTERNAL
//! \addtogroup asmjit_x86
//! \{

// ============================================================================
// [asmjit::X86Internal]
// ============================================================================

//! X86 utilities used at multiple places, not part of public API, not exported.
struct X86Internal {
  //! Initialize `FuncDetail` (X86 specific).
  static Error initFuncDetail(FuncDetail& func, const FuncSignature& signature, uint32_t registerSize) noexcept;

  //! Initialize `FuncFrame` (X86 specific).
  static Error initFuncFrame(FuncFrame& frame, const FuncDetail& signature) noexcept;

  //! Finalize `FuncFrame` (X86 specific).
  static Error finalizeFuncFrame(FuncFrame& frame) noexcept;

  static Error argsToFuncFrame(const FuncArgsAssignment& args, FuncFrame& frame) noexcept;

  //! Emit function prolog.
  static Error emitProlog(Emitter* emitter, const FuncFrame& frame);

  //! Emit function epilog.
  static Error emitEpilog(Emitter* emitter, const FuncFrame& frame);

  //! Emit a pure move operation between two registers or the same type or
  //! between a register and its home slot. This function does not handle
  //! register conversion.
  static Error emitRegMove(Emitter* emitter,
    const Operand_& dst_,
    const Operand_& src_, uint32_t typeId, bool avxEnabled, const char* comment = nullptr);

  //! Emit move from a function argument (either register or stack) to a register.
  //!
  //! This function can handle the necessary conversion from one argument to
  //! another, and from one register type to another, if it's possible. Any
  //! attempt of conversion that requires third register of a different group
  //! (for example conversion from K to MMX) will fail.
  static Error emitArgMove(Emitter* emitter,
    const Reg& dst_, uint32_t dstTypeId,
    const Operand_& src_, uint32_t srcTypeId, bool avxEnabled, const char* comment = nullptr);

  static Error emitArgsAssignment(Emitter* emitter, const FuncFrame& frame, const FuncArgsAssignment& args);
};

//! \}
//! \endcond

ASMJIT_END_SUB_NAMESPACE

#endif // ASMJIT_X86_X86INTERNAL_P_H_INCLUDED
