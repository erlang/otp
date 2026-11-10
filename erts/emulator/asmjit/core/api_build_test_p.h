// This file is part of AsmJit project <https://asmjit.com>
//
// See <asmjit/core.h> or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

// This is an internal header file that is always included first by each AsmJit test file.

#ifndef ASMJIT_CORE_API_BUILD_TEST_P_H_INCLUDED
#define ASMJIT_CORE_API_BUILD_TEST_P_H_INCLUDED

#include <asmjit/core/api_build_p.h>

// Build - Tests
// =============

//! \cond NEVER
// Make sure '#ifdef'ed unit tests are not disabled by IDE.
#if !defined(ASMJIT_TEST) && defined(__INTELLISENSE__)
  #define ASMJIT_TEST
#endif
//! \endcond

// Include a unit testing package if this is a `bl_test_runner` build.
#if defined(ASMJIT_TEST)

#include <asmjit/axl/span.h>
#include <asmjit/core/error.h>
#include <asmjit/core/string.h>
#include <asmjit-testing/commons/zero_test.h>

// ZeroTest integration.
template<>
struct ZeroFormatter<asmjit::Error> {
  static constexpr bool kExists = true;

  static inline void stringify(ZeroLib::String& buf, asmjit::Error value) noexcept {
    buf.append(asmjit::stringify_error(value));
  }
};

template<>
struct ZeroFormatter<asmjit::Span<const char>> {
  static constexpr bool kExists = true;

  static inline void stringify(ZeroLib::String& buf, asmjit::Span<const char> value) noexcept {
    buf.append(value.data(), value.size());
  }
};

template<>
struct ZeroFormatter<asmjit::Span<char>> {
  static constexpr bool kExists = true;

  static inline void stringify(ZeroLib::String& buf, asmjit::Span<char> value) noexcept {
    buf.append(value.data(), value.size());
  }
};

template<>
struct ZeroFormatter<asmjit::String> {
  static constexpr bool kExists = true;

  static inline void stringify(ZeroLib::String& buf, const asmjit::String& value) noexcept {
    buf.append(value.data(), value.size());
  }
};

//! \cond INTERNAL
#define EXPECT_OK(...) \
  EXPECT_CUSTOM(::ZeroTest::expect_eq(__FILE__, __LINE__, "EXPECT_OK(" #__VA_ARGS__ ")", (__VA_ARGS__), ::asmjit::Error::kOk))
//! \endcond

#endif // ASMJIT_TEST

#endif // ASMJIT_CORE_API_BUILD_TEST_P_H_INCLUDED
