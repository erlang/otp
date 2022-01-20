// This file is part of AsmJit project <https://asmjit.com>
//
// See asmjit.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#include "../core/api-build_p.h"
#if !defined(ASMJIT_NO_X86) && !defined(ASMJIT_NO_BUILDER)

#include "../x86/x86assembler.h"
#include "../x86/x86builder.h"

ASMJIT_BEGIN_SUB_NAMESPACE(x86)

Builder::Builder(CodeHolder* code) noexcept : BaseBuilder() {
  if (code)
    code->attach(this);
}
Builder::~Builder() noexcept {}

Error Builder::finalize() {
  ASMJIT_PROPAGATE(runPasses());
  Assembler a(_code);
  a.addEncodingOptions(encodingOptions());
  a.addDiagnosticOptions(diagnosticOptions());
  return serializeTo(&a);
}

Error Builder::onAttach(CodeHolder* code) noexcept {
  Arch arch = code->arch();
  if (!Environment::isFamilyX86(arch))
    return DebugUtils::errored(kErrorInvalidArch);

  return Base::onAttach(code);
}

ASMJIT_END_SUB_NAMESPACE

#endif // !ASMJIT_NO_X86 && !ASMJIT_NO_BUILDER
