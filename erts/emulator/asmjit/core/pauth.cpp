// This file is part of AsmJit project <https://asmjit.com>
//
// See <asmjit/core.h> or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#include <asmjit/core/api_build_p.h>

#include <asmjit/core/pauth.h>
#include <asmjit/core/pauth_utils_p.h>

ASMJIT_BEGIN_NAMESPACE

namespace PAuthUtils {

void* sign_c_func_ptr(void* ptr) noexcept { return sign_c_func_ptr_inline(ptr); }
void* strip_c_func_ptr(void* ptr) noexcept { return strip_c_func_ptr_inline(ptr); }

} // {PAuthUtils}

ASMJIT_END_NAMESPACE
