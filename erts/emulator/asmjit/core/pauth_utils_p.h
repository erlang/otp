// This file is part of AsmJit project <https://asmjit.com>
//
// See <asmjit/core.h> or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#ifndef ASMJIT_CORE_PTR_AUTH_UTILS_P_H_INCLUDED
#define ASMJIT_CORE_PTR_AUTH_UTILS_P_H_INCLUDED

#include <asmjit/core/build_defs.h>

#include <asmjit/core/pauth.h>

#if defined(__APPLE__) && defined(__arm64e__)
  #include <ptrauth.h>
#endif

ASMJIT_BEGIN_SUB_NAMESPACE(PAuthUtils)

static inline void* sign_c_func_ptr_inline(void* ptr) noexcept {
#if defined(__APPLE__) && defined(__arm64e__)
  return ptrauth_sign_unauthenticated(ptr, ptrauth_key_function_pointer, 0);
#else
  return ptr;
#endif
}

static inline void* strip_c_func_ptr_inline(void* ptr) noexcept {
#if defined(__APPLE__) && defined(__arm64e__)
  return ptrauth_strip(ptr, ptrauth_key_function_pointer);
#else
  return ptr;
#endif
}

ASMJIT_END_SUB_NAMESPACE

#endif // ASMJIT_CORE_PTR_AUTH_UTILS_P_H_INCLUDED
