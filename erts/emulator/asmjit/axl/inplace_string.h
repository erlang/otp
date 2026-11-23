// This file is part of AsmJit project <https://asmjit.com>
//
// See <asmjit/core.h> or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#ifndef ASMJIT_AXL_INPLACE_STRING_H_INCLUDED
#define ASMJIT_AXL_INPLACE_STRING_H_INCLUDED

#include <asmjit/axl/build_integration.h>
#include <asmjit/axl/commons.h>
#include <asmjit/axl/span.h>

ASMJIT_BEGIN_SUB_NAMESPACE(axl)

//! \addtogroup asmjit_axl
//! \{

//! Inplace string - only useful for strings that would never exceed `N` characters; always null-terminated.
template<size_t N>
struct InplaceString {
  //! \name Constants
  //! \{

  static_assert(N > 0 && N < 256, "axl::InplaceString must be 1-255 characters long");

  static inline constexpr uint32_t kBufferCapacity = (N + 1u + 3u) / 4u * 4u;
  static inline constexpr uint32_t kStringCapacity = kBufferCapacity - 1u;
  static inline constexpr size_t kSizeIndex = kStringCapacity - 1;

  //! \}

  //! \name Members
  //! \{

  char _data[kBufferCapacity];

  //! \}

  //! \name Construction & Destruction
  //! \{

  ASMJIT_INLINE_NODEBUG InplaceString() noexcept : _data{} {
    _data[kSizeIndex] = uint8_t(kStringCapacity);
  }

  ASMJIT_INLINE_NODEBUG InplaceString(const InplaceString& other) noexcept = default;

  ASMJIT_INLINE_NODEBUG InplaceString(Span<const char> other) noexcept : _data{} {
    size_t size = other.size();
    ASMJIT_ASSERT(size <= kStringCapacity);

    memcpy(_data, other.data(), size);
    _assign_size(size);
  }

  //! \}

  //! \name Overloaded Operators
  //! \{

  ASMJIT_INLINE_NODEBUG InplaceString& operator=(const InplaceString& other) noexcept = default;

  //! \}

  //! \name Equality & Comparison
  //! \{

  [[nodiscard]] ASMJIT_INLINE_NODEBUG bool equals(const char* str) const noexcept { return strcmp(_data, str) == 0; }
  [[nodiscard]] ASMJIT_INLINE_NODEBUG bool equals(const char* str, size_t len) const noexcept { return size() == len && memcmp(_data, str, len) == 0; }

  [[nodiscard]] ASMJIT_INLINE_NODEBUG bool equals(Span<char> str) const noexcept { return equals(str.data(), str.size()); }
  [[nodiscard]] ASMJIT_INLINE_NODEBUG bool equals(Span<const char> str) const noexcept { return equals(str.data(), str.size()); }

  template<size_t OtherN>
  [[nodiscard]] ASMJIT_INLINE_NODEBUG bool equals(const InplaceString<OtherN>& other) const noexcept {
    if constexpr (N == OtherN) {
      return _data[kSizeIndex] == other._data[kSizeIndex] && memcmp(_data, other._data, size()) == 0;
    }
    else {
      return equals(other.data(), other.size());
    }
  }

  //! \}

  //! \name Accessors
  //! \{

  [[nodiscard]] ASMJIT_INLINE_NODEBUG bool is_empty() const noexcept { return _data[kSizeIndex] == kStringCapacity; }
  [[nodiscard]] ASMJIT_INLINE_NODEBUG size_t size() const noexcept { return size_t(uint8_t(_data[kSizeIndex])) ^ kStringCapacity; }
  [[nodiscard]] ASMJIT_INLINE_CONSTEXPR size_t capacity() const noexcept { return kStringCapacity; }

  [[nodiscard]] ASMJIT_INLINE_NODEBUG const char* data() const noexcept { return _data; }
  [[nodiscard]] ASMJIT_INLINE_NODEBUG char* data_mut() noexcept { return _data; }

  [[nodiscard]] ASMJIT_INLINE_NODEBUG Span<const char> span() const noexcept { return Span<const char>(data(), size()); }
  [[nodiscard]] ASMJIT_INLINE_NODEBUG Span<char> span_mut() const noexcept { return Span<char>(data_mut(), size()); }

  //! \}

  //! \name Manipulation
  //! \{

  ASMJIT_INLINE_NODEBUG void _assign_size(size_t size) noexcept {
    ASMJIT_ASSERT(size <= kStringCapacity);
    _data[size] = '\0';
    _data[kSizeIndex] = char(uint8_t(size ^ kStringCapacity));
  }

  ASMJIT_INLINE_NODEBUG Error clear() noexcept {
    memset(_data, 0, sizeof(_data) - 1);
    _data[kSizeIndex] = char(uint8_t(kStringCapacity));

    return kSuccess;
  }

  ASMJIT_INLINE_NODEBUG Error assign(char c) noexcept {
    _data[0] = c;
    _assign_size(1);

    return kSuccess;
  }

  ASMJIT_INLINE_NODEBUG Error assign(Span<const char> str) noexcept {
    size_t str_size = str.size();
    if (ASMJIT_UNLIKELY(str_size > kStringCapacity)) {
      return make_out_of_memory();
    }

    memcpy(_data, str.data(), str_size);
    _assign_size(str_size);

    return kSuccess;
  }

  ASMJIT_INLINE_NODEBUG Error assign(const char* str) noexcept {
    return assign(Span<const char>(str, strlen(str)));
  }

  ASMJIT_INLINE_NODEBUG Error append(Span<const char> str) noexcept {
    size_t this_size = size();
    size_t str_size = str.size();
    size_t final_size = this_size + str_size;

    if (ASMJIT_UNLIKELY(final_size > kStringCapacity)) {
      return make_out_of_memory();
    }

    memcpy(_data + this_size, str.data(), str_size);
    _assign_size(final_size);

    return kSuccess;
  }

  ASMJIT_INLINE_NODEBUG Error append(const char* str) noexcept {
    return append(Span<const char>(str, strlen(str)));
  }

  //! \}
};

//! \}

ASMJIT_END_SUB_NAMESPACE

#endif // ASMJIT_AXL_INPLACE_STRING_H_INCLUDED
