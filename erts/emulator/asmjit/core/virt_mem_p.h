// This file is part of AsmJit project <https://asmjit.com>
//
// See <asmjit/core.h> or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#ifndef ASMJIT_CORE_VIRT_MEM_P_H_INCLUDED
#define ASMJIT_CORE_VIRT_MEM_P_H_INCLUDED

#include <asmjit/core/build_defs.h>
#ifndef ASMJIT_NO_JIT

#include <asmjit/core/globals.h>
#include <asmjit/core/virt_mem.h>

ASMJIT_BEGIN_SUB_NAMESPACE(VirtMem)

//! \cond INTERNAL

//! Returns the address range spanned by the loaded module (executable or shared library) that contains `addr`.
//!
//! \note Implemented on Windows, macOS, Linux and BSDs, other platforms return \ref Error::kFeatureNotEnabled.
Error module_address_range(uintptr_t addr, Out<AddressRange> range) noexcept;

//! \endcond

ASMJIT_END_SUB_NAMESPACE

#endif // !ASMJIT_NO_JIT
#endif // ASMJIT_CORE_VIRT_MEM_P_H_INCLUDED
