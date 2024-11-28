// This file is part of AsmJit project <https://asmjit.com>
//
// See asmjit.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#ifndef ASMJIT_ARM_A32INSTAPI_H_P_INCLUDED
#define ASMJIT_ARM_A32INSTAPI_H_P_INCLUDED

#include "../core/codeholder.h"
#include "../arm/a32instdb_p.h"

ASMJIT_BEGIN_SUB_NAMESPACE(a32)

//! \cond INTERNAL
//! \addtogroup asmjit_a32
//! \{

namespace InstInternal {

#ifndef ASMJIT_NO_TEXT
Error ASMJIT_CDECL instIdToString(InstId instId, String& output) noexcept;
InstId ASMJIT_CDECL stringToInstId(const char* s, size_t len) noexcept;
#endif // !ASMJIT_NO_TEXT

#ifndef ASMJIT_NO_VALIDATION
Error ASMJIT_CDECL validate(const BaseInst& inst, const Operand_* operands, size_t opCount, ValidationFlags validationFlags) noexcept;
#endif // !ASMJIT_NO_VALIDATION

#ifndef ASMJIT_NO_INTROSPECTION
Error ASMJIT_CDECL queryRWInfo(const BaseInst& inst, const Operand_* operands, size_t opCount, InstRWInfo* out) noexcept;
Error ASMJIT_CDECL queryFeatures(const BaseInst& inst, const Operand_* operands, size_t opCount, CpuFeatures* out) noexcept;
#endif // !ASMJIT_NO_INTROSPECTION

} // {InstInternal}

//! \}
//! \endcond

ASMJIT_END_SUB_NAMESPACE

#endif // ASMJIT_ARM_A32INSTAPI_H_P_INCLUDED

