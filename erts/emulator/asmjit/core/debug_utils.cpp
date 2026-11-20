// This file is part of AsmJit project <https://asmjit.com>
//
// See <asmjit/core.h> or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#include <asmjit/core/api_build_p.h>

#include <asmjit/core/debug_utils.h>

#if defined(_WIN32)
  #include <windows.h>
#endif

ASMJIT_BEGIN_SUB_NAMESPACE(DebugUtils)

ASMJIT_FAVOR_SIZE void debug_output(const char* str) noexcept {
#if defined(_WIN32)
  ::OutputDebugStringA(str);
#else
  ::fputs(str, stderr);
#endif
}

ASMJIT_FAVOR_SIZE void assertion_failure(const char* file, int line, const char* msg) noexcept {
  char str[1024];

  snprintf(str, 1024,
    "[asmjit] Assertion failed at %s (line %d):\n"
    "[asmjit] %s\n", file, line, msg);

  debug_output(str);
  ::abort();
}

ASMJIT_END_SUB_NAMESPACE
