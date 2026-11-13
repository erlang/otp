// This file is part of AsmJit project <https://asmjit.com>
//
// See <asmjit/core.h> or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#ifndef ASMJIT_AXL_STRING_UTILS_H_INCLUDED
#define ASMJIT_AXL_STRING_UTILS_H_INCLUDED

#include <asmjit/axl/commons.h>
#include <asmjit/axl/span.h>

ASMJIT_BEGIN_SUB_NAMESPACE(axl)

namespace {

//! \addtogroup asmjit_axl
//! \{

// ASCII Utilities
// ===============

template<typename T>
[[nodiscard]]
ASMJIT_INLINE_CONSTEXPR bool is_ascii_between(const T& c, const T& a, const T& b) noexcept { return uint32_t(c) - uint32_t(a) <= uint32_t(b) - uint32_t(a); }

template<typename T>
[[nodiscard]]
ASMJIT_INLINE_CONSTEXPR bool is_ascii_space(const T& c) noexcept { return bool_or(c == T(' '), c == T('\t'), c == T('\r'), c == T('\n')); }

template<typename T>
[[nodiscard]]
ASMJIT_INLINE_CONSTEXPR bool is_ascii_alpha(const T& c) noexcept { return is_ascii_between(T(uint32_t(c) | 0x20u), T('a'), T('z')); }

template<typename T>
[[nodiscard]]
ASMJIT_INLINE_CONSTEXPR bool is_ascii_digit(const T& c) noexcept { return is_ascii_between(c, T('0'), T('9')); }

template<typename T>
[[nodiscard]]
ASMJIT_INLINE_NODEBUG bool is_ascii_alnum(const T& c) noexcept { return bool_or(is_ascii_alpha(c), is_ascii_digit(c)); }

template<typename T, typename... Args>
[[nodiscard]]
ASMJIT_INLINE_NODEBUG bool is_ascii_alnum_or_(const T& c, Args&&... additional_chars) noexcept {
  return is_ascii_alnum(c) || (... || (c == T(additional_chars)));
}

template<typename T>
[[nodiscard]]
ASMJIT_INLINE_CONSTEXPR T ascii_to_lower(const T& c) noexcept { return T(c ^ T(T(c >= T('A') && c <= T('Z')) << 5)); }

template<typename T>
[[nodiscard]]
ASMJIT_INLINE_CONSTEXPR T ascii_to_upper(const T& c) noexcept { return T(c ^ T(T(c >= T('a') && c <= T('z')) << 5)); }

// String Utilities
// ================

[[nodiscard]]
ASMJIT_INLINE_NODEBUG size_t str_nlen(const char* s, size_t max_size) noexcept {
  size_t i = 0;
  while (i < max_size && s[i] != '\0')
    i++;
  return i;
}

//! Compares two string views.
[[nodiscard]]
ASMJIT_INLINE int compare_string_views(const char* a_data, size_t a_size, const char* b_data, size_t b_size) noexcept {
  size_t size = min(a_size, b_size);

  for (size_t i = 0; i < size; i++) {
    int c = int(uint8_t(a_data[i])) - int(uint8_t(b_data[i]));
    if (c != 0)
      return c;
  }

  return int(a_size) - int(b_size);
}

// String Hashing
// ==============

//! Hashes a single character `c` and combines that with `existing_hash`.
template<typename T>
[[nodiscard]]
ASMJIT_INLINE_CONSTEXPR uint32_t hash_char(uint32_t existing_hash, const T& c) noexcept {
  return existing_hash * 65599u + uint32_t(c);
}

// Returns a hash of the given string `data` of size `size`. Size must be valid as this
// function doesn't check for a null terminator and allows it in the middle of the string.
[[nodiscard]]
ASMJIT_INLINE_NODEBUG uint32_t hash_string(const char* str, size_t size) noexcept {
  uint32_t hash_code = 0;
  for (uint32_t i = 0; i < size; i++) {
    hash_code = hash_char(hash_code, uint8_t(str[i]));
  }
  return hash_code;
}

//! Returns a hash of the given string passed as string span `str`.
[[nodiscard]]
ASMJIT_INLINE_NODEBUG uint32_t hash_string(Span<const char> str) noexcept {
  return hash_string(str.data(), str.size());
}

// Packed Strings
// ==============

[[nodiscard]]
ASMJIT_INLINE_NODEBUG const char* find_packed_string(const char* p, uint32_t id) noexcept {
  uint32_t i = 0;
  while (i < id) {
    while (p[0]) {
      p++;
    }
    p++;
    i++;
  }
  return p;
}

//! \}

} // {anonymous}

ASMJIT_END_SUB_NAMESPACE

#endif // ASMJIT_AXL_STRING_UTILS_H_INCLUDED