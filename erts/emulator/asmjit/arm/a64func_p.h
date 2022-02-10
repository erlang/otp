// This file is part of AsmJit project <https://asmjit.com>
//
// See asmjit.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#ifndef ASMJIT_ARM_A64FUNC_P_H_INCLUDED
#define ASMJIT_ARM_A64FUNC_P_H_INCLUDED

#include "../core/func.h"

ASMJIT_BEGIN_SUB_NAMESPACE(a64)

//! \cond INTERNAL
//! \addtogroup asmjit_a64
//! \{

//! AArch64-specific function API (calling conventions and other utilities).
namespace FuncInternal {

//! Initialize `CallConv` structure (AArch64 specific).
Error initCallConv(CallConv& cc, CallConvId ccId, const Environment& environment) noexcept;

//! Initialize `FuncDetail` (AArch64 specific).
Error initFuncDetail(FuncDetail& func, const FuncSignature& signature, uint32_t registerSize) noexcept;

} // {FuncInternal}

//! \}
//! \endcond

ASMJIT_END_SUB_NAMESPACE

#endif // ASMJIT_ARM_A64FUNC_P_H_INCLUDED
