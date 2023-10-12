// This file is part of AsmJit project <https://asmjit.com>
//
// See asmjit.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#include "../core/api-build_p.h"
#include "../core/osutils.h"
#include "../core/support.h"

#if defined(_WIN32)
  #include <atomic>
#elif defined(__APPLE__)
  #include <mach/mach_time.h>
#else
  #include <time.h>
  #include <unistd.h>
#endif

ASMJIT_BEGIN_NAMESPACE

uint32_t OSUtils::getTickCount() noexcept {
#if defined(_WIN32)
  enum HiResStatus : uint32_t {
    kHiResUnknown      = 0,
    kHiResAvailable    = 1,
    kHiResNotAvailable = 2
  };

  static std::atomic<uint32_t> _hiResStatus(kHiResUnknown);
  static volatile double _hiResFreq(0);

  uint32_t status = _hiResStatus.load();
  LARGE_INTEGER now, qpf;

  if (status != kHiResNotAvailable && ::QueryPerformanceCounter(&now)) {
    double freq = _hiResFreq;
    if (status == kHiResUnknown) {
      // Detects the availability of high resolution counter.
      if (::QueryPerformanceFrequency(&qpf)) {
        freq = double(qpf.QuadPart) / 1000.0;
        _hiResFreq = freq;
        _hiResStatus.compare_exchange_strong(status, kHiResAvailable);
        status = kHiResAvailable;
      }
      else {
        // High resolution not available.
        _hiResStatus.compare_exchange_strong(status, kHiResNotAvailable);
      }
    }

    if (status == kHiResAvailable)
      return uint32_t(uint64_t(int64_t(double(now.QuadPart) / freq)) & 0xFFFFFFFFu);
  }

  // Bail to `GetTickCount()` if we cannot use high resolution.
  return ::GetTickCount();
#elif defined(__APPLE__)
  // See Apple's QA1398.
  static mach_timebase_info_data_t _machTime;

  uint32_t denom = _machTime.denom;
  if (ASMJIT_UNLIKELY(!denom)) {
    if (mach_timebase_info(&_machTime) != KERN_SUCCESS || !(denom = _machTime.denom))
      return 0;
  }

  // `mach_absolute_time()` returns nanoseconds, we want milliseconds.
  uint64_t t = mach_absolute_time() / 1000000u;
  t = (t * _machTime.numer) / _machTime.denom;
  return uint32_t(t & 0xFFFFFFFFu);
#elif defined(_POSIX_MONOTONIC_CLOCK) && _POSIX_MONOTONIC_CLOCK >= 0
  struct timespec ts;
  if (ASMJIT_UNLIKELY(clock_gettime(CLOCK_MONOTONIC, &ts) != 0))
    return 0;

  uint64_t t = (uint64_t(ts.tv_sec ) * 1000u) + (uint64_t(ts.tv_nsec) / 1000000u);
  return uint32_t(t & 0xFFFFFFFFu);
#else
  #pragma message("[asmjit] OSUtils::getTickCount() doesn't have implementation for the target OS.")
  return 0;
#endif
}

ASMJIT_END_NAMESPACE
