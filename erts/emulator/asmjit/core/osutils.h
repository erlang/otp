// This file is part of AsmJit project <https://asmjit.com>
//
// See asmjit.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#ifndef ASMJIT_CORE_OSUTILS_H_INCLUDED
#define ASMJIT_CORE_OSUTILS_H_INCLUDED

#include "../core/globals.h"

ASMJIT_BEGIN_NAMESPACE

//! \addtogroup asmjit_utilities
//! \{

//! Operating system utilities.
namespace OSUtils {
  //! Gets the current CPU tick count, used for benchmarking (1ms resolution).
  ASMJIT_API uint32_t getTickCount() noexcept;
};


//! \cond INTERNAL
//! Lock.
//!
//! Lock is internal, it cannot be used outside of AsmJit, however, its internal
//! layout is exposed as it's used by some other classes, which are public.
class Lock {
public:
  ASMJIT_NONCOPYABLE(Lock)

#if defined(_WIN32)
#pragma pack(push, 8)
  struct ASMJIT_MAY_ALIAS Handle {
    void* DebugInfo;
    long LockCount;
    long RecursionCount;
    void* OwningThread;
    void* LockSemaphore;
    unsigned long* SpinCount;
  };
  Handle _handle;
#pragma pack(pop)
#elif !defined(__EMSCRIPTEN__)
  typedef pthread_mutex_t Handle;
  Handle _handle;
#endif

  ASMJIT_FORCE_INLINE Lock() noexcept;
  ASMJIT_FORCE_INLINE ~Lock() noexcept;

  ASMJIT_FORCE_INLINE void lock() noexcept;
  ASMJIT_FORCE_INLINE void unlock() noexcept;
};
//! \endcond

//! \}

ASMJIT_END_NAMESPACE

#endif // ASMJIT_CORE_OSUTILS_H_INCLUDED
