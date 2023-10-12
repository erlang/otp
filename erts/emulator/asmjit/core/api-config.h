// This file is part of AsmJit project <https://asmjit.com>
//
// See asmjit.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#ifndef ASMJIT_CORE_API_CONFIG_H_INCLUDED
#define ASMJIT_CORE_API_CONFIG_H_INCLUDED

// AsmJit Library & ABI Version
// ============================

//! \addtogroup asmjit_core
//! \{

//! AsmJit library version in `(Major << 16) | (Minor << 8) | (Patch)` format.
#define ASMJIT_LIBRARY_VERSION 0x010A00 /* 1.10.0 */

//! \def ASMJIT_ABI_NAMESPACE
//!
//! AsmJit ABI namespace is an inline namespace within \ref asmjit namespace.
//!
//! It's used to make sure that when user links to an incompatible version of AsmJit, it won't link. It has also some
//! additional properties as well. When `ASMJIT_ABI_NAMESPACE` is defined by the user it would override the AsmJit
//! default, which makes it possible to use use multiple AsmJit libraries within a single project, totally controlled
//! by the users. This is useful especially in cases in which some of such library comes from a third party.
#ifndef ASMJIT_ABI_NAMESPACE
  #define ASMJIT_ABI_NAMESPACE _abi_1_10
#endif

//! \}

// Global Dependencies
// ===================

#include <stdarg.h>
#include <stddef.h>
#include <stdint.h> // We really want std types as globals, not under 'std' namespace.
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <iterator>
#include <limits>
#include <new>
#include <type_traits>
#include <utility>

#if !defined(_WIN32) && !defined(__EMSCRIPTEN__)
  #include <pthread.h>
#endif

// Build Options
// =============

// NOTE: Doxygen cannot document macros that are not defined, that's why we have to define them and then undefine
// them immediately, so it won't use the macros with its own preprocessor.
#ifdef _DOXYGEN
namespace asmjit {

//! \addtogroup asmjit_build
//! \{

//! Asmjit is embedded, implies \ref ASMJIT_STATIC.
#define ASMJIT_EMBED

//! Enables static-library build.
#define ASMJIT_STATIC

//! Defined when AsmJit's build configuration is 'Debug'.
//!
//! \note Can be defined explicitly to bypass autodetection.
#define ASMJIT_BUILD_DEBUG

//! Defined when AsmJit's build configuration is 'Release'.
//!
//! \note Can be defined explicitly to bypass autodetection.
#define ASMJIT_BUILD_RELEASE

//! Disables X86/X64 backends.
#define ASMJIT_NO_X86

//! Disables AArch32 backends (both ARM and Thumb).
#define ASMJIT_NO_AARCH32

//! Disables AArch64 backend.
#define ASMJIT_NO_AARCH64

//! Disables non-host backends entirely (useful for JIT compilers to minimize the library size).
#define ASMJIT_NO_FOREIGN

//! Disables deprecated API at compile time (deprecated API won't be available).
#define ASMJIT_NO_DEPRECATED

//! Disables \ref asmjit_builder functionality completely.
#define ASMJIT_NO_BUILDER

//! Disables \ref asmjit_compiler functionality completely.
#define ASMJIT_NO_COMPILER

//! Disables JIT memory management and \ref asmjit::JitRuntime.
#define ASMJIT_NO_JIT

//! Disables \ref asmjit::Logger and \ref asmjit::Formatter.
#define ASMJIT_NO_LOGGING

//! Disables everything that contains text.
#define ASMJIT_NO_TEXT

//! Disables instruction validation API.
#define ASMJIT_NO_VALIDATION

//! Disables instruction introspection API.
#define ASMJIT_NO_INTROSPECTION

// Avoid doxygen preprocessor using feature-selection definitions.
#undef ASMJIT_BUILD_EMBNED
#undef ASMJIT_BUILD_STATIC
#undef ASMJIT_BUILD_DEBUG
#undef ASMJIT_BUILD_RELEASE
#undef ASMJIT_NO_X86
#undef ASMJIT_NO_FOREIGN
// (keep ASMJIT_NO_DEPRECATED defined, we don't document deprecated APIs).
#undef ASMJIT_NO_BUILDER
#undef ASMJIT_NO_COMPILER
#undef ASMJIT_NO_JIT
#undef ASMJIT_NO_LOGGING
#undef ASMJIT_NO_TEXT
#undef ASMJIT_NO_VALIDATION
#undef ASMJIT_NO_INTROSPECTION

//! \}

} // {asmjit}
#endif // _DOXYGEN

// ASMJIT_NO_BUILDER implies ASMJIT_NO_COMPILER.
#if defined(ASMJIT_NO_BUILDER) && !defined(ASMJIT_NO_COMPILER)
  #define ASMJIT_NO_COMPILER
#endif

// Prevent compile-time errors caused by misconfiguration.
#if defined(ASMJIT_NO_TEXT) && !defined(ASMJIT_NO_LOGGING)
  #pragma message("'ASMJIT_NO_TEXT' can only be defined when 'ASMJIT_NO_LOGGING' is defined.")
  #undef ASMJIT_NO_TEXT
#endif

#if defined(ASMJIT_NO_INTROSPECTION) && !defined(ASMJIT_NO_COMPILER)
  #pragma message("'ASMJIT_NO_INTROSPECTION' can only be defined when 'ASMJIT_NO_COMPILER' is defined")
  #undef ASMJIT_NO_INTROSPECTION
#endif

// Build Mode
// ==========

// Detect ASMJIT_BUILD_DEBUG and ASMJIT_BUILD_RELEASE if not defined.
#if !defined(ASMJIT_BUILD_DEBUG) && !defined(ASMJIT_BUILD_RELEASE)
  #if !defined(NDEBUG)
    #define ASMJIT_BUILD_DEBUG
  #else
    #define ASMJIT_BUILD_RELEASE
  #endif
#endif

// Target Architecture Detection
// =============================

#if defined(_M_X64) || defined(__x86_64__)
  #define ASMJIT_ARCH_X86 64
#elif defined(_M_IX86) || defined(__X86__) || defined(__i386__)
  #define ASMJIT_ARCH_X86 32
#else
  #define ASMJIT_ARCH_X86 0
#endif

#if defined(__arm64__) || defined(__aarch64__)
# define ASMJIT_ARCH_ARM 64
#elif defined(_M_ARM) || defined(_M_ARMT) || defined(__arm__) || defined(__thumb__) || defined(__thumb2__)
  #define ASMJIT_ARCH_ARM 32
#else
  #define ASMJIT_ARCH_ARM 0
#endif

#if defined(_MIPS_ARCH_MIPS64) || defined(__mips64)
  #define ASMJIT_ARCH_MIPS 64
#elif defined(_MIPS_ARCH_MIPS32) || defined(_M_MRX000) || defined(__mips__)
  #define ASMJIT_ARCH_MIPS 32
#else
  #define ASMJIT_ARCH_MIPS 0
#endif

#define ASMJIT_ARCH_BITS (ASMJIT_ARCH_X86 | ASMJIT_ARCH_ARM | ASMJIT_ARCH_MIPS)
#if ASMJIT_ARCH_BITS == 0
  #undef ASMJIT_ARCH_BITS
  #if defined (__LP64__) || defined(_LP64)
    #define ASMJIT_ARCH_BITS 64
  #else
    #define ASMJIT_ARCH_BITS 32
  #endif
#endif

#if (defined(__ARMEB__))  || \
    (defined(__MIPSEB__)) || \
    (defined(__BYTE_ORDER__) && (__BYTE_ORDER__ == __ORDER_BIG_ENDIAN__))
  #define ASMJIT_ARCH_LE 0
  #define ASMJIT_ARCH_BE 1
#else
  #define ASMJIT_ARCH_LE 1
  #define ASMJIT_ARCH_BE 0
#endif

#if defined(ASMJIT_NO_FOREIGN)
  #if !ASMJIT_ARCH_X86 && !defined(ASMJIT_NO_X86)
    #define ASMJIT_NO_X86
  #endif

  #if !ASMJIT_ARCH_ARM && !defined(ASMJIT_NO_AARCH64)
    #define ASMJIT_NO_AARCH64
  #endif
#endif


// C++ Compiler and Features Detection
// ===================================

#define ASMJIT_CXX_GNU 0
#define ASMJIT_CXX_MAKE_VER(MAJOR, MINOR) ((MAJOR) * 1000 + (MINOR))

// Intel Compiler [pretends to be GNU or MSC, so it must be checked first]:
//   - https://software.intel.com/en-us/articles/c0x-features-supported-by-intel-c-compiler
//   - https://software.intel.com/en-us/articles/c14-features-supported-by-intel-c-compiler
//   - https://software.intel.com/en-us/articles/c17-features-supported-by-intel-c-compiler
#if defined(__INTEL_COMPILER)

// MSC Compiler:
//   - https://msdn.microsoft.com/en-us/library/hh567368.aspx
//
// Version List:
//   - 16.00.0 == VS2010
//   - 17.00.0 == VS2012
//   - 18.00.0 == VS2013
//   - 19.00.0 == VS2015
//   - 19.10.0 == VS2017
#elif defined(_MSC_VER) && defined(_MSC_FULL_VER)

// Clang Compiler [Pretends to be GNU, so it must be checked before]:
//   - https://clang.llvm.org/cxx_status.html
#elif defined(__clang_major__) && defined(__clang_minor__) && defined(__clang_patchlevel__)

// GNU Compiler:
//   - https://gcc.gnu.org/projects/cxx-status.html
#elif defined(__GNUC__) && defined(__GNUC_MINOR__) && defined(__GNUC_PATCHLEVEL__)

  #undef ASMJIT_CXX_GNU
  #define ASMJIT_CXX_GNU ASMJIT_CXX_MAKE_VER(__GNUC__, __GNUC_MINOR__)

#endif

// Compiler features detection macros.
#if defined(__clang__) && defined(__has_attribute)
  #define ASMJIT_CXX_HAS_ATTRIBUTE(NAME, CHECK) (__has_attribute(NAME))
#else
  #define ASMJIT_CXX_HAS_ATTRIBUTE(NAME, CHECK) (!(!(CHECK)))
#endif

// API Decorators & C++ Extensions
// ===============================

//! \def ASMJIT_API
//!
//! A decorator that is used to decorate API that AsmJit exports when built as a shared library.

// API (Export / Import).
#if !defined(ASMJIT_STATIC)
  #if defined(_WIN32) && (defined(_MSC_VER) || defined(__MINGW32__))
    #ifdef ASMJIT_EXPORTS
      #define ASMJIT_API __declspec(dllexport)
    #else
      #define ASMJIT_API __declspec(dllimport)
    #endif
  #elif defined(_WIN32) && defined(__GNUC__)
    #ifdef ASMJIT_EXPORTS
      #define ASMJIT_API __attribute__((__dllexport__))
    #else
      #define ASMJIT_API __attribute__((__dllimport__))
    #endif
  #elif defined(__GNUC__)
    #define ASMJIT_API __attribute__((__visibility__("default")))
  #endif
#endif

#if !defined(ASMJIT_API)
  #define ASMJIT_API
#endif

#if !defined(ASMJIT_VARAPI)
  #define ASMJIT_VARAPI extern ASMJIT_API
#endif

//! \def ASMJIT_VIRTAPI
//!
//! This is basically a workaround. When using MSVC and marking class as DLL export everything gets exported, which
//! is unwanted in most projects. MSVC automatically exports typeinfo and vtable if at least one symbol of the class
//! is exported. However, GCC has some strange behavior that even if one or more symbol is exported it doesn't export
//! typeinfo unless the class itself is decorated with "visibility(default)" (i.e. ASMJIT_API).
#if !defined(_WIN32) && defined(__GNUC__)
  #define ASMJIT_VIRTAPI ASMJIT_API
#else
  #define ASMJIT_VIRTAPI
#endif

// Function attributes.
#if !defined(ASMJIT_BUILD_DEBUG) && defined(__GNUC__)
  #define ASMJIT_FORCE_INLINE inline __attribute__((__always_inline__))
#elif !defined(ASMJIT_BUILD_DEBUG) && defined(_MSC_VER)
  #define ASMJIT_FORCE_INLINE __forceinline
#else
  #define ASMJIT_FORCE_INLINE inline
#endif

#if defined(__GNUC__)
  #define ASMJIT_NOINLINE __attribute__((__noinline__))
  #define ASMJIT_NORETURN __attribute__((__noreturn__))
#elif defined(_MSC_VER)
  #define ASMJIT_NOINLINE __declspec(noinline)
  #define ASMJIT_NORETURN __declspec(noreturn)
#else
  #define ASMJIT_NOINLINE
  #define ASMJIT_NORETURN
#endif

// Calling conventions.
#if ASMJIT_ARCH_X86 == 32 && defined(__GNUC__)
  #define ASMJIT_CDECL __attribute__((__cdecl__))
  #define ASMJIT_STDCALL __attribute__((__stdcall__))
  #define ASMJIT_FASTCALL __attribute__((__fastcall__))
  #define ASMJIT_REGPARM(N) __attribute__((__regparm__(N)))
#elif ASMJIT_ARCH_X86 == 32 && defined(_MSC_VER)
  #define ASMJIT_CDECL __cdecl
  #define ASMJIT_STDCALL __stdcall
  #define ASMJIT_FASTCALL __fastcall
  #define ASMJIT_REGPARM(N)
#else
  #define ASMJIT_CDECL
  #define ASMJIT_STDCALL
  #define ASMJIT_FASTCALL
  #define ASMJIT_REGPARM(N)
#endif

#if ASMJIT_ARCH_X86 && defined(_WIN32) && defined(_MSC_VER)
  #define ASMJIT_VECTORCALL __vectorcall
#elif ASMJIT_ARCH_X86 && defined(_WIN32)
  #define ASMJIT_VECTORCALL __attribute__((__vectorcall__))
#else
  #define ASMJIT_VECTORCALL
#endif

// Type alignment (not allowed by C++11 'alignas' keyword).
#if defined(__GNUC__)
  #define ASMJIT_ALIGN_TYPE(TYPE, N) __attribute__((__aligned__(N))) TYPE
#elif defined(_MSC_VER)
  #define ASMJIT_ALIGN_TYPE(TYPE, N) __declspec(align(N)) TYPE
#else
  #define ASMJIT_ALIGN_TYPE(TYPE, N) TYPE
#endif

//! \def ASMJIT_MAY_ALIAS
//!
//! Expands to `__attribute__((__may_alias__))` if supported.
#if defined(__GNUC__)
  #define ASMJIT_MAY_ALIAS __attribute__((__may_alias__))
#else
  #define ASMJIT_MAY_ALIAS
#endif

//! \def ASMJIT_MAYBE_UNUSED
//!
//! Expands to `[[maybe_unused]]` if supported or a compiler attribute instead.
#if __cplusplus >= 201703L
  #define ASMJIT_MAYBE_UNUSED [[maybe_unused]]
#elif defined(__GNUC__)
  #define ASMJIT_MAYBE_UNUSED __attribute__((unused))
#else
  #define ASMJIT_MAYBE_UNUSED
#endif

#if defined(__clang_major__) && __clang_major__ >= 4 && !defined(_DOXYGEN)
  // NOTE: Clang allows to apply this attribute to function arguments, which is what we want. Once GCC decides to
  // support this use, we will enable it for GCC as well. However, until that, it will be clang only, which is
  // what we need for static analysis.
  #define ASMJIT_NONNULL(FUNCTION_ARGUMENT) FUNCTION_ARGUMENT __attribute__((__nonnull__))
#else
  #define ASMJIT_NONNULL(FUNCTION_ARGUMENT) FUNCTION_ARGUMENT
#endif

//! \def ASMJIT_NOEXCEPT_TYPE
//!
//! Defined to `noexcept` in C++17 mode or nothing otherwise. Used by function typedefs.
#if __cplusplus >= 201703L
  #define ASMJIT_NOEXCEPT_TYPE noexcept
#else
  #define ASMJIT_NOEXCEPT_TYPE
#endif

//! \def ASMJIT_ASSUME(...)
//!
//! Macro that tells the C/C++ compiler that the expression `...` evaluates to true.
//!
//! This macro has two purposes:
//!
//!   1. Enable optimizations that would not be possible without the assumption.
//!   2. Hint static analysis tools that a certain condition is true to prevent false positives.
#if defined(__clang__)
  #define ASMJIT_ASSUME(...) __builtin_assume(__VA_ARGS__)
#elif defined(__GNUC__)
  #define ASMJIT_ASSUME(...) do { if (!(__VA_ARGS__)) __builtin_unreachable(); } while (0)
#elif defined(_MSC_VER)
  #define ASMJIT_ASSUME(...) __assume(__VA_ARGS__)
#else
  #define ASMJIT_ASSUME(...) (void)0
#endif

//! \def ASMJIT_LIKELY(...)
//!
//! Condition is likely to be taken (mostly error handling and edge cases).

//! \def ASMJIT_UNLIKELY(...)
//!
//! Condition is unlikely to be taken (mostly error handling and edge cases).
#if defined(__GNUC__)
  #define ASMJIT_LIKELY(...) __builtin_expect(!!(__VA_ARGS__), 1)
  #define ASMJIT_UNLIKELY(...) __builtin_expect(!!(__VA_ARGS__), 0)
#else
  #define ASMJIT_LIKELY(...) (__VA_ARGS__)
  #define ASMJIT_UNLIKELY(...) (__VA_ARGS__)
#endif

//! \def ASMJIT_FALLTHROUGH
//!
//! Portable [[fallthrough]] attribute.
#if defined(__clang__) && __cplusplus >= 201103L
  #define ASMJIT_FALLTHROUGH [[clang::fallthrough]]
#elif defined(__GNUC__) && __GNUC__ >= 7
  #define ASMJIT_FALLTHROUGH __attribute__((__fallthrough__))
#else
  #define ASMJIT_FALLTHROUGH ((void)0) /* fallthrough */
#endif

//! \def ASMJIT_DEPRECATED
//!
//! Marks function, class, struct, enum, or anything else as deprecated.
#if defined(__GNUC__)
  #define ASMJIT_DEPRECATED(MESSAGE) __attribute__((__deprecated__(MESSAGE)))
  #if defined(__clang__)
    #define ASMJIT_DEPRECATED_STRUCT(MESSAGE) __attribute__((__deprecated__(MESSAGE)))
  #else
    #define ASMJIT_DEPRECATED_STRUCT(MESSAGE) /* not usable if a deprecated function uses it */
  #endif
#elif defined(_MSC_VER)
  #define ASMJIT_DEPRECATED(MESSAGE) __declspec(deprecated(MESSAGE))
  #define ASMJIT_DEPRECATED_STRUCT(MESSAGE) /* not usable if a deprecated function uses it */
#else
  #define ASMJIT_DEPRECATED(MESSAGE)
  #define ASMJIT_DEPRECATED_STRUCT(MESSAGE)
#endif

// Utilities.
#define ASMJIT_OFFSET_OF(STRUCT, MEMBER) ((int)(intptr_t)((const char*)&((const STRUCT*)0x100)->MEMBER) - 0x100)
#define ASMJIT_ARRAY_SIZE(X) uint32_t(sizeof(X) / sizeof(X[0]))

#if ASMJIT_CXX_HAS_ATTRIBUTE(no_sanitize, 0)
  #define ASMJIT_ATTRIBUTE_NO_SANITIZE_UNDEF __attribute__((__no_sanitize__("undefined")))
#elif ASMJIT_CXX_GNU >= ASMJIT_CXX_MAKE_VER(4, 9)
  #define ASMJIT_ATTRIBUTE_NO_SANITIZE_UNDEF __attribute__((__no_sanitize_undefined__))
#else
  #define ASMJIT_ATTRIBUTE_NO_SANITIZE_UNDEF
#endif

// Begin-Namespace & End-Namespace Macros
// ======================================

#if defined _DOXYGEN
  #define ASMJIT_BEGIN_NAMESPACE namespace asmjit {
  #define ASMJIT_END_NAMESPACE }
#elif defined(__clang__)
  #define ASMJIT_BEGIN_NAMESPACE                                              \
    namespace asmjit { inline namespace ASMJIT_ABI_NAMESPACE {                \
      _Pragma("clang diagnostic push")                                        \
      _Pragma("clang diagnostic ignored \"-Wconstant-logical-operand\"")      \
      _Pragma("clang diagnostic ignored \"-Wunnamed-type-template-args\"")
  #define ASMJIT_END_NAMESPACE                                                \
      _Pragma("clang diagnostic pop")                                         \
    }}
#elif defined(__GNUC__) && __GNUC__ == 4
  #define ASMJIT_BEGIN_NAMESPACE                                              \
    namespace asmjit { inline namespace ASMJIT_ABI_NAMESPACE {                \
      _Pragma("GCC diagnostic push")                                          \
      _Pragma("GCC diagnostic ignored \"-Wmissing-field-initializers\"")
  #define ASMJIT_END_NAMESPACE                                                \
      _Pragma("GCC diagnostic pop")                                           \
    }}
#elif defined(__GNUC__) && __GNUC__ >= 8
  #define ASMJIT_BEGIN_NAMESPACE                                              \
    namespace asmjit { inline namespace ASMJIT_ABI_NAMESPACE {                \
      _Pragma("GCC diagnostic push")                                          \
      _Pragma("GCC diagnostic ignored \"-Wclass-memaccess\"")
  #define ASMJIT_END_NAMESPACE                                                \
      _Pragma("GCC diagnostic pop")                                           \
    }}
#elif defined(_MSC_VER) && !defined(__INTEL_COMPILER)
  #define ASMJIT_BEGIN_NAMESPACE                                              \
    namespace asmjit { inline namespace ASMJIT_ABI_NAMESPACE {                \
      __pragma(warning(push))                                                 \
      __pragma(warning(disable: 4127))  /* conditional expression is const */ \
      __pragma(warning(disable: 4201))  /* nameless struct/union */
  #define ASMJIT_END_NAMESPACE                                                \
      __pragma(warning(pop))                                                  \
    }}
#endif

#if !defined(ASMJIT_BEGIN_NAMESPACE) && !defined(ASMJIT_END_NAMESPACE)
  #define ASMJIT_BEGIN_NAMESPACE namespace asmjit { inline namespace ASMJIT_ABI_NAMESPACE {
  #define ASMJIT_END_NAMESPACE }}
#endif

#define ASMJIT_BEGIN_SUB_NAMESPACE(NAMESPACE)                                 \
  ASMJIT_BEGIN_NAMESPACE                                                      \
  namespace NAMESPACE {

#define ASMJIT_END_SUB_NAMESPACE                                              \
  }                                                                           \
  ASMJIT_END_NAMESPACE

// C++ Utilities
// =============

#define ASMJIT_NONCOPYABLE(Type)                                              \
    Type(const Type& other) = delete;                                         \
    Type& operator=(const Type& other) = delete;

#define ASMJIT_NONCONSTRUCTIBLE(Type)                                         \
    Type() = delete;                                                          \
    Type(const Type& other) = delete;                                         \
    Type& operator=(const Type& other) = delete;

//! \def ASMJIT_DEFINE_ENUM_FLAGS(T)
//!
//! Defines bit operations for enumeration flags.
#ifdef _DOXYGEN
  #define ASMJIT_DEFINE_ENUM_FLAGS(T)
#else
  #define ASMJIT_DEFINE_ENUM_FLAGS(T)                                         \
    static ASMJIT_FORCE_INLINE constexpr T operator~(T a) noexcept {          \
      return T(~(std::underlying_type<T>::type)(a));                          \
    }                                                                         \
                                                                              \
    static ASMJIT_FORCE_INLINE constexpr T operator|(T a, T b) noexcept {     \
      return T((std::underlying_type<T>::type)(a) |                           \
              (std::underlying_type<T>::type)(b));                            \
    }                                                                         \
    static ASMJIT_FORCE_INLINE constexpr T operator&(T a, T b) noexcept {     \
      return T((std::underlying_type<T>::type)(a) &                           \
              (std::underlying_type<T>::type)(b));                            \
    }                                                                         \
    static ASMJIT_FORCE_INLINE constexpr T operator^(T a, T b) noexcept {     \
      return T((std::underlying_type<T>::type)(a) ^                           \
              (std::underlying_type<T>::type)(b));                            \
    }                                                                         \
                                                                              \
    static ASMJIT_FORCE_INLINE T& operator|=(T& a, T b) noexcept {            \
      a = T((std::underlying_type<T>::type)(a) |                              \
            (std::underlying_type<T>::type)(b));                              \
      return a;                                                               \
    }                                                                         \
    static ASMJIT_FORCE_INLINE T& operator&=(T& a, T b) noexcept {            \
      a = T((std::underlying_type<T>::type)(a) &                              \
            (std::underlying_type<T>::type)(b));                              \
      return a;                                                               \
    }                                                                         \
    static ASMJIT_FORCE_INLINE T& operator^=(T& a, T b) noexcept {            \
      a = T((std::underlying_type<T>::type)(a) ^                              \
            (std::underlying_type<T>::type)(b));                              \
      return a;                                                               \
    }
#endif

//! \def ASMJIT_DEFINE_ENUM_COMPARE(T)
//!
//! Defines comparison operations for enumeration flags.
#if defined(_DOXYGEN) || (defined(_MSC_VER) && _MSC_VER <= 1900)
  #define ASMJIT_DEFINE_ENUM_COMPARE(T)
#else
  #define ASMJIT_DEFINE_ENUM_COMPARE(T)                                                \
    static ASMJIT_FORCE_INLINE bool operator<(T a, T b) noexcept {                     \
      return (std::underlying_type<T>::type)(a) < (std::underlying_type<T>::type)(b);  \
    }                                                                                  \
    static ASMJIT_FORCE_INLINE bool operator<=(T a, T b) noexcept {                    \
      return (std::underlying_type<T>::type)(a) <= (std::underlying_type<T>::type)(b); \
    }                                                                                  \
    static ASMJIT_FORCE_INLINE bool operator>(T a, T b) noexcept {                     \
      return (std::underlying_type<T>::type)(a) > (std::underlying_type<T>::type)(b);  \
    }                                                                                  \
    static ASMJIT_FORCE_INLINE bool operator>=(T a, T b) noexcept {                    \
      return (std::underlying_type<T>::type)(a) >= (std::underlying_type<T>::type)(b); \
    }
#endif

// Cleanup Api-Config Specific Macros
// ==================================

#undef ASMJIT_CXX_GNU
#undef ASMJIT_CXX_MAKE_VER

#endif // ASMJIT_CORE_API_CONFIG_H_INCLUDED
