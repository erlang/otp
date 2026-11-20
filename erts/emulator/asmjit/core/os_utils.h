// This file is part of AsmJit project <https://asmjit.com>
//
// See <asmjit/core.h> or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#ifndef ASMJIT_CORE_OS_UTILS_H_INCLUDED
#define ASMJIT_CORE_OS_UTILS_H_INCLUDED

#include <asmjit/core/build_defs.h>

#if !defined(_WIN32) && !defined(__EMSCRIPTEN__)
  #include <pthread.h>
#endif

ASMJIT_BEGIN_NAMESPACE

//! \addtogroup asmjit_core
//! \{

//! \cond INTERNAL
//! Lock.
//!
//! Lock is internal, it cannot be used outside of AsmJit, however, its internal
//! layout is exposed as it's used by some other classes, which are public.
class Lock {
public:
  ASMJIT_NONCOPYABLE(Lock)

#if defined(_WIN32)
  struct Handle { void* ptr; };
  Handle _handle;
#elif !defined(__EMSCRIPTEN__)
  using Handle = pthread_mutex_t;
  Handle _handle;
#endif

  ASMJIT_INLINE_NODEBUG Lock() noexcept;
  ASMJIT_INLINE_NODEBUG ~Lock() noexcept;

  ASMJIT_INLINE_NODEBUG void lock() noexcept;
  ASMJIT_INLINE_NODEBUG void unlock() noexcept;
};
//! \endcond

//! \}

ASMJIT_END_NAMESPACE

#endif // ASMJIT_CORE_OS_UTILS_H_INCLUDED
