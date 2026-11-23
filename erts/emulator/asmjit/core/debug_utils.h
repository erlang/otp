// This file is part of AsmJit project <https://asmjit.com>
//
// See <asmjit/core.h> or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#ifndef ASMJIT_CORE_DEBUG_UTILS_H_INCLUDED
#define ASMJIT_CORE_DEBUG_UTILS_H_INCLUDED

#include <asmjit/core/build_defs.h>

ASMJIT_BEGIN_NAMESPACE

//! \addtogroup asmjit_error_handling
//! \{

//! Debug utilities.
namespace DebugUtils {

//! Called to output debugging messages.
ASMJIT_API void debug_output(const char* str) noexcept;

//! Called on assertion failure.
//!
//! \param file Source file name where it happened.
//! \param line Line in the source file.
//! \param msg Message to display.
//!
//! If you have problems with assertion failures a breakpoint can be put at \ref assertion_failure() function
//! (asmjit/core/globals.cpp). A call stack will be available when such assertion failure is triggered. AsmJit
//! always returns errors on failures, assertions are a last resort and usually mean unrecoverable state due to out
//! of range array access or totally invalid arguments like nullptr where a valid pointer should be provided, etc...
[[noreturn]]
ASMJIT_API void assertion_failure(const char* file, int line, const char* msg) noexcept;

//! \def ASMJIT_ASSERT(...)
//!
//! AsmJit's own assert macro used in AsmJit code-base.
#if defined(ASMJIT_BUILD_DEBUG)
  #define ASMJIT_ASSERT(...)                                                       \
    do {                                                                           \
      if (ASMJIT_UNLIKELY(!(__VA_ARGS__))) {                                       \
        ::asmjit::DebugUtils::assertion_failure(__FILE__, __LINE__, #__VA_ARGS__); \
      }                                                                            \
    } while (0)
#else
  #define ASMJIT_ASSERT(...) ((void)0)
#endif

//! \def ASMJIT_NOT_REACHED()
//!
//! Run-time assertion used in code that should never be reached.
#if defined(ASMJIT_BUILD_DEBUG)
  #define ASMJIT_NOT_REACHED() ::asmjit::DebugUtils::assertion_failure(__FILE__, __LINE__, "ASMJIT_NOT_REACHED()")
#elif defined(__GNUC__)
  #define ASMJIT_NOT_REACHED() __builtin_unreachable()
#else
  #define ASMJIT_NOT_REACHED() ASMJIT_ASSUME(0)
#endif

} // {DebugUtils}

//! \}

ASMJIT_END_NAMESPACE

#endif // ASMJIT_CORE_DEBUG_UTILS_H_INCLUDED
