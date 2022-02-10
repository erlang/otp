// This file is part of AsmJit project <https://asmjit.com>
//
// See asmjit.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#include "../core/api-build_p.h"
#if !defined(ASMJIT_NO_AARCH64) && !defined(ASMJIT_NO_COMPILER)

#include "../arm/a64assembler.h"
#include "../arm/a64compiler.h"
#include "../arm/a64emithelper_p.h"
#include "../arm/a64rapass_p.h"

ASMJIT_BEGIN_SUB_NAMESPACE(a64)

// a64::Compiler - Construction & Destruction
// ==========================================

Compiler::Compiler(CodeHolder* code) noexcept : BaseCompiler() {
  _archMask = uint64_t(1) << uint32_t(Arch::kAArch64);
  assignEmitterFuncs(this);

  if (code)
    code->attach(this);
}
Compiler::~Compiler() noexcept {}

// a64::Compiler - Events
// ======================

Error Compiler::onAttach(CodeHolder* code) noexcept {
  ASMJIT_PROPAGATE(Base::onAttach(code));
  Error err = addPassT<ARMRAPass>();

  if (ASMJIT_UNLIKELY(err)) {
    onDetach(code);
    return err;
  }

  return kErrorOk;
}

Error Compiler::onDetach(CodeHolder* code) noexcept {
  return Base::onDetach(code);
}

// a64::Compiler - Finalize
// ========================

Error Compiler::finalize() {
  ASMJIT_PROPAGATE(runPasses());
  Assembler a(_code);
  a.addEncodingOptions(encodingOptions());
  a.addDiagnosticOptions(diagnosticOptions());
  return serializeTo(&a);
}

ASMJIT_END_SUB_NAMESPACE

#endif // !ASMJIT_NO_AARCH64 && !ASMJIT_NO_COMPILER
