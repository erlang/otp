// This file is part of AsmJit project <https://asmjit.com>
//
// See asmjit.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#ifndef ASMJIT_X86_X86INSTAPI_P_H_INCLUDED
#define ASMJIT_X86_X86INSTAPI_P_H_INCLUDED

#include "../core/inst.h"
#include "../core/operand.h"

ASMJIT_BEGIN_SUB_NAMESPACE(x86)

//! \cond INTERNAL
//! \addtogroup asmjit_x86
//! \{

namespace InstInternal {

#ifndef ASMJIT_NO_TEXT
Error ASMJIT_CDECL instIdToString(InstId instId, String& output) noexcept;
InstId ASMJIT_CDECL stringToInstId(const char* s, size_t len) noexcept;
#endif // !ASMJIT_NO_TEXT

#ifndef ASMJIT_NO_VALIDATION
Error ASMJIT_CDECL validateX86(const BaseInst& inst, const Operand_* operands, size_t opCount, ValidationFlags validationFlags) noexcept;
Error ASMJIT_CDECL validateX64(const BaseInst& inst, const Operand_* operands, size_t opCount, ValidationFlags validationFlags) noexcept;
#endif // !ASMJIT_NO_VALIDATION

#ifndef ASMJIT_NO_INTROSPECTION
Error ASMJIT_CDECL queryRWInfo(Arch arch, const BaseInst& inst, const Operand_* operands, size_t opCount, InstRWInfo* out) noexcept;
Error ASMJIT_CDECL queryFeatures(Arch arch, const BaseInst& inst, const Operand_* operands, size_t opCount, CpuFeatures* out) noexcept;
#endif // !ASMJIT_NO_INTROSPECTION

} // {InstInternal}

//! \}
//! \endcond

ASMJIT_END_SUB_NAMESPACE

#endif // ASMJIT_X86_X86INSTAPI_P_H_INCLUDED
