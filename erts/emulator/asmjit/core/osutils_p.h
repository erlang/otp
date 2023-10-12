// This file is part of AsmJit project <https://asmjit.com>
//
// See asmjit.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#ifndef ASMJIT_CORE_OSUTILS_P_H_INCLUDED
#define ASMJIT_CORE_OSUTILS_P_H_INCLUDED

#include "../core/osutils.h"

ASMJIT_BEGIN_NAMESPACE

//! \cond INTERNAL
//! \addtogroup asmjit_utilities
//! \{

#if defined(_WIN32)

// Windows implementation.
static_assert(sizeof(Lock::Handle) == sizeof(CRITICAL_SECTION), "asmjit::Lock::Handle layout must match CRITICAL_SECTION");
static_assert(alignof(Lock::Handle) == alignof(CRITICAL_SECTION), "asmjit::Lock::Handle alignment must match CRITICAL_SECTION");

ASMJIT_FORCE_INLINE Lock::Lock() noexcept { InitializeCriticalSection(reinterpret_cast<CRITICAL_SECTION*>(&_handle)); }
ASMJIT_FORCE_INLINE Lock::~Lock() noexcept { DeleteCriticalSection(reinterpret_cast<CRITICAL_SECTION*>(&_handle)); }
ASMJIT_FORCE_INLINE void Lock::lock() noexcept { EnterCriticalSection(reinterpret_cast<CRITICAL_SECTION*>(&_handle)); }
ASMJIT_FORCE_INLINE void Lock::unlock() noexcept { LeaveCriticalSection(reinterpret_cast<CRITICAL_SECTION*>(&_handle)); }

#elif !defined(__EMSCRIPTEN__)

// PThread implementation.
#ifdef PTHREAD_MUTEX_INITIALIZER
ASMJIT_FORCE_INLINE Lock::Lock() noexcept : _handle(PTHREAD_MUTEX_INITIALIZER) {}
#else
ASMJIT_FORCE_INLINE Lock::Lock() noexcept { pthread_mutex_init(&_handle, nullptr); }
#endif
ASMJIT_FORCE_INLINE Lock::~Lock() noexcept { pthread_mutex_destroy(&_handle); }
ASMJIT_FORCE_INLINE void Lock::lock() noexcept { pthread_mutex_lock(&_handle); }
ASMJIT_FORCE_INLINE void Lock::unlock() noexcept { pthread_mutex_unlock(&_handle); }

#else

// Dummy implementation - Emscripten or other unsupported platform.
ASMJIT_FORCE_INLINE Lock::Lock() noexcept {}
ASMJIT_FORCE_INLINE Lock::~Lock() noexcept {}
ASMJIT_FORCE_INLINE void Lock::lock() noexcept {}
ASMJIT_FORCE_INLINE void Lock::unlock() noexcept {}

#endif

//! Scoped lock.
class LockGuard {
public:
  ASMJIT_NONCOPYABLE(LockGuard)

  Lock& _target;

  inline LockGuard(Lock& target) noexcept
    : _target(target) { _target.lock(); }
  inline ~LockGuard() noexcept { _target.unlock(); }
};

//! \}
//! \endcond

ASMJIT_END_NAMESPACE

#endif // ASMJIT_CORE_OSUTILS_P_H_INCLUDED

