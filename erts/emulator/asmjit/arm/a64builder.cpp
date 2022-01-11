// This file is part of AsmJit project <https://asmjit.com>
//
// See asmjit.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#include "../core/api-build_p.h"
#if !defined(ASMJIT_NO_ARM) && !defined(ASMJIT_NO_BUILDER)

#include "../arm/a64assembler.h"
#include "../arm/a64builder.h"

ASMJIT_BEGIN_SUB_NAMESPACE(a64)

// a64::Builder - Construction & Destruction
// =========================================

Builder::Builder(CodeHolder* code) noexcept : BaseBuilder() {
  if (code)
    code->attach(this);
}
Builder::~Builder() noexcept {}

// a64::Builder - Finalize
// =======================

Error Builder::finalize() {
  ASMJIT_PROPAGATE(runPasses());
  Assembler a(_code);
  a.addEncodingOptions(encodingOptions());
  a.addDiagnosticOptions(diagnosticOptions());
  return serializeTo(&a);
}

// a64::Builder - Events
// =====================

Error Builder::onAttach(CodeHolder* code) noexcept {
  Arch arch = code->arch();
  if (!Environment::isFamilyARM(arch))
    return DebugUtils::errored(kErrorInvalidArch);

  return Base::onAttach(code);
}

ASMJIT_END_SUB_NAMESPACE

#endif // !ASMJIT_NO_ARM && !ASMJIT_NO_BUILDER
