// This file is part of AsmJit project <https://asmjit.com>
//
// See asmjit.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#include "../core/api-build_p.h"
#if !defined(ASMJIT_NO_AARCH32) && !defined(ASMJIT_NO_BUILDER)

#include "../arm/a32assembler.h"
#include "../arm/a32builder.h"
#include "../arm/a32emithelper_p.h"

ASMJIT_BEGIN_SUB_NAMESPACE(a32)

// a32::Builder - Construction & Destruction
// =========================================

Builder::Builder(CodeHolder* code) noexcept : BaseBuilder() {
  _archMask = uint64_t(1) << uint32_t(Arch::kARM     ) |
              uint64_t(1) << uint32_t(Arch::kARM_BE  ) |
              uint64_t(1) << uint32_t(Arch::kThumb   ) |
              uint64_t(1) << uint32_t(Arch::kThumb_BE) ;
  if (code)
    code->attach(this);
}
Builder::~Builder() noexcept {}

// a32::Builder - Events
// =====================

Error Builder::onAttach(CodeHolder* code) noexcept {
  ASMJIT_PROPAGATE(Base::onAttach(code));

  _instructionAlignment = _environment.isArchThumb() ? uint8_t(2) : uint8_t(4);
  assignEmitterFuncs(this);

  return kErrorOk;
}

Error Builder::onDetach(CodeHolder* code) noexcept {
  return Base::onDetach(code);
}

// a32::Builder - Finalize
// =======================

Error Builder::finalize() {
  ASMJIT_PROPAGATE(runPasses());
  Assembler a(_code);
  a.addEncodingOptions(encodingOptions());
  a.addDiagnosticOptions(diagnosticOptions());
  return serializeTo(&a);
}

ASMJIT_END_SUB_NAMESPACE

#endif // !ASMJIT_NO_AARCH32 && !ASMJIT_NO_BUILDER
