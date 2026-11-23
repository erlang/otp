// This file is part of AsmJit project <https://asmjit.com>
//
// See <asmjit/core.h> or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#ifndef ASMJIT_CORE_STRING_H_INCLUDED
#define ASMJIT_CORE_STRING_H_INCLUDED

#include <asmjit/axl/commons.h>
#include <asmjit/axl/inplace_string.h>
#include <asmjit/axl/span.h>
#include <asmjit/core/error.h>

ASMJIT_BEGIN_NAMESPACE

//! \addtogroup asmjit_core
//! \{

//! Format flags used by \ref String API.
enum class StringFormatFlags : uint32_t {
  //! No flags.
  kNone = 0x00000000u,
  //! Show sign.
  kShowSign = 0x00000001u,
  //! Show space.
  kShowSpace = 0x00000002u,
  //! Alternate form (use 0x when formatting HEX number).
  kAlternate = 0x00000004u,
  //! The input is signed.
  kSigned = 0x80000000u
};
ASMJIT_DEFINE_ENUM_FLAGS(StringFormatFlags)

//! A simple non-reference counted string that uses small string optimization (SSO).
//!
//! This string has 3 allocation possibilities:
//!
//!   1. Small    - embedded buffer is used for up to `kSSOCapacity` characters. This should handle most small
//!                 strings and thus avoid dynamic memory allocation for most use-cases.
//!
//!   2. Large    - string that doesn't fit into an embedded buffer (or string that was truncated from a larger
//!                 buffer) and is owned by AsmJit. When you destroy the string AsmJit would automatically
//!                 release the large buffer.
//!
//!   3. External - like Large (2), however, the large buffer is not owned by AsmJit and won't be released when
//!                 the string is destroyed or reallocated. This is mostly useful for working with larger temporary
//!                 strings allocated on stack or with immutable strings.
class String {
public:
  ASMJIT_NONCOPYABLE(String)

  //! \cond INTERNAL
  static inline constexpr uint32_t kLayoutSize = 32;
  static inline constexpr uint32_t kSSOCapacity = kLayoutSize - 2;

  //! Large string (owned by String).
  static inline constexpr uint8_t kTypeLarge = 0x1Fu;
  //! External string (arena allocated or not owned by String).
  static inline constexpr uint8_t kTypeExternal = 0x20u;

  [[nodiscard]]
  static ASMJIT_INLINE_NODEBUG bool is_external(uint8_t type) noexcept { return type == kTypeExternal; }

  [[nodiscard]]
  static ASMJIT_INLINE_NODEBUG bool is_large_or_external(uint8_t type) noexcept { return type >= kTypeLarge; }

  union Raw {
    uint8_t u8[kLayoutSize];
    uint64_t u64[kLayoutSize / sizeof(uint64_t)];
    uintptr_t uptr[kLayoutSize / sizeof(uintptr_t)];
  };

  struct Small {
    uint8_t type;
    char data[kSSOCapacity + 1u];
  };

  struct Large {
    uint8_t type;
    uint8_t reserved[sizeof(uintptr_t) - 1];
    size_t size;
    size_t capacity;
    char* data;
  };

  union {
    uint8_t _type;
    Raw _raw;
    Small _small;
    Large _large;
  };
  //! \endcond

  //! \name Construction & Destruction
  //! \{

  //! Creates a default-initialized string if zero length.
  ASMJIT_INLINE_NODEBUG String() noexcept
    : _small {} {}

  //! Creates a string that takes ownership of the content of the `other` string.
  ASMJIT_INLINE_NODEBUG String(String&& other) noexcept {
    _raw = other._raw;
    other._reset_internal();
  }

  //! Destroys the string and all resources associated with it - performs the same operation as \ref reset().
  ASMJIT_INLINE_NODEBUG ~String() noexcept {
    reset();
  }

  //! Resets the string into its default-constructed state.
  //!
  //! \note Reset would release a dynamic buffer if the string is not in SSO-mode. If the string uses external
  //! buffer, it will get detached from the string and internal SSO buffer would be setup for further operations.
  ASMJIT_API Error reset() noexcept;

  //! \}

  //! \name Overloaded Operators
  //! \{

  inline String& operator=(String&& other) noexcept {
    swap(other);
    other.reset();
    return *this;
  }

  [[nodiscard]] ASMJIT_INLINE_NODEBUG bool operator==(const char* c_str) const noexcept { return  equals(c_str); }
  [[nodiscard]] ASMJIT_INLINE_NODEBUG bool operator!=(const char* c_str) const noexcept { return !equals(c_str); }

  [[nodiscard]] ASMJIT_INLINE_NODEBUG bool operator==(const String& other) const noexcept { return  equals(other); }
  [[nodiscard]] ASMJIT_INLINE_NODEBUG bool operator!=(const String& other) const noexcept { return !equals(other); }

  [[nodiscard]] ASMJIT_INLINE_NODEBUG bool operator==(Span<const char> span) const noexcept { return  equals(span); }
  [[nodiscard]] ASMJIT_INLINE_NODEBUG bool operator!=(Span<const char> span) const noexcept { return !equals(span); }

  //! \}

  //! \name Accessors
  //! \{

  [[nodiscard]] ASMJIT_INLINE_NODEBUG bool is_external() const noexcept { return is_external(_type); }
  [[nodiscard]] ASMJIT_INLINE_NODEBUG bool is_large_or_external() const noexcept { return is_large_or_external(_type); }

  //! Tests whether the string is empty.
  [[nodiscard]] ASMJIT_INLINE_NODEBUG bool is_empty() const noexcept { return size() == 0; }
  //! Returns the size of the string.
  [[nodiscard]] ASMJIT_INLINE_NODEBUG size_t size() const noexcept { return is_large_or_external() ? size_t(_large.size) : size_t(_type); }
  //! Returns the capacity of the string.
  [[nodiscard]] ASMJIT_INLINE_NODEBUG size_t capacity() const noexcept { return is_large_or_external() ? _large.capacity : size_t(kSSOCapacity); }

  //! Returns the data of the string.
  [[nodiscard]] ASMJIT_INLINE_NODEBUG char* data() noexcept { return is_large_or_external() ? _large.data : _small.data; }

  //! \overload
  [[nodiscard]] ASMJIT_INLINE_NODEBUG const char* data() const noexcept { return is_large_or_external() ? _large.data : _small.data; }

  [[nodiscard]] ASMJIT_INLINE_NODEBUG char* begin() noexcept { return data(); }
  [[nodiscard]] ASMJIT_INLINE_NODEBUG const char* begin() const noexcept { return data(); }

  [[nodiscard]] ASMJIT_INLINE_NODEBUG char* end() noexcept { return data() + size(); }
  [[nodiscard]] ASMJIT_INLINE_NODEBUG const char* end() const noexcept { return data() + size(); }

  //! Returns a const (read-only) span representing the string data.
  [[nodiscard]] ASMJIT_INLINE_NODEBUG Span<const char> span() const noexcept { return Span<const char>(data(), size()); }
  //! Returns a mutable (read-write) span representing the string data.
  [[nodiscard]] ASMJIT_INLINE_NODEBUG Span<char> span_mut() noexcept { return Span<char>(data(), size()); }

  //! \}

  //! \name String Operations
  //! \{

  //! Swaps the content of this string with `other`.
  ASMJIT_INLINE_NODEBUG void swap(String& other) noexcept { axl::swap(_raw, other._raw); }

  //! Clears the content of the string.
  ASMJIT_API Error clear() noexcept;

  //! Creates space for assignment, changes string length to `size`, but doesn't assign any bytes to the
  //! string except for null terminator at the end of the string data.
  //!
  //! \remarks  This function can be used to reserve space for assign operation in case the caller itself wants
  //! to fill the content. If the caller doesn't assign the whole content reserved by `assign_uninitialized()`,
  //! it can use the internal \ref _set_size() function to truncate the string size afterwards.
  [[nodiscard]] ASMJIT_API char* assign_uninitialized(size_t size) noexcept;

  //! Creates space for append operation, increments string length by `size`, but doesn't append any bytes
  //! to the string except for null terminator at the end of the string data.
  //!
  //! \remarks This function can be used to reserve space for append operation in case the caller itself wants
  //! to fill the content. If the caller doesn't assign the whole content reserved by `append_uninitialized()`,
  //! it can use the internal \ref _set_size() function to truncate the string size afterwards - please read the
  //! documentation of \ref _set_size() to understand the behavior.
  [[nodiscard]] ASMJIT_API char* append_uninitialized(size_t size) noexcept;

  //! Replaces the content of the string by `data` of the given `data_size`.
  //!
  //! Null terminated strings can set `data_size` to `SIZE_MAX`.
  ASMJIT_API Error assign(const char* data, size_t data_size = SIZE_MAX) noexcept;
  //! Replaces the content of the string by the given `span`.
  ASMJIT_INLINE_NODEBUG Error assign(Span<const char> span) noexcept { return assign(span.data(), span.size()); }
  //! Replaces the content of the string by `other` string.
  ASMJIT_INLINE_NODEBUG Error assign(const String& other) noexcept { return assign(other.data(), other.size()); }

  //! Replaces the content of the string by a `c` character, repeated `n` times.
  ASMJIT_API Error assign_chars(char c, size_t n) noexcept;
  //! Replaces the content of the string by a single `c` character.
  ASMJIT_INLINE_NODEBUG Error assign(char c) noexcept { return assign_chars(c, 1u); }

  //! Replaces the content of the string by a formatted integer `i` (internal).
  ASMJIT_API Error _assign_int(uint64_t i, uint32_t base, size_t width, StringFormatFlags flags) noexcept;

  //! Replaces the content of the string by a formatted integer `i`.
  //!
  //! The optional `width` specifies the minimum total output size, including the sign and alternate prefix.
  template<typename Integer>
  ASMJIT_INLINE_NODEBUG Error assign_int(const Integer& i, uint32_t base = 0, size_t width = 0, StringFormatFlags flags = StringFormatFlags::kNone) noexcept {
    if constexpr (std::is_signed_v<Integer>) {
      return _assign_int(static_cast<uint64_t>(static_cast<int64_t>(i)), base, width, flags | StringFormatFlags::kSigned);
    }
    else {
      return _assign_int(static_cast<uint64_t>(i), base, width, flags);
    }
  }

  //! Replaces the current of the string by the given `data` converted to a HEX string with an optional `separator`.
  //!
  //! \warning This function doesn't support aliasing. It's undefined behavior if the provided `data` pointer
  //! aliases the content of the string.
  ASMJIT_API Error assign_hex(const void* data, size_t size, char separator = '\0') noexcept;

  //! Replaces the current of the string by a formatted string `fmt`.
  ASMJIT_API Error assign_format(const char* fmt, ...) noexcept;
  //! Replaces the current of the string by a formatted string `fmt` (va_list version).
  ASMJIT_API Error assign_vformat(const char* fmt, va_list ap) noexcept;

  //! Appends `data` having the given size `data_size` to the string.
  //!
  //! Null terminated strings can set `data_size` to `SIZE_MAX`.
  ASMJIT_API Error append(const char* data, size_t data_size = SIZE_MAX) noexcept;
  //! Appends the given `span` to the string.
  ASMJIT_INLINE_NODEBUG Error append(Span<const char> span) noexcept { return append(span.data(), span.size()); }
  //! Appends `other` string to this string.
  ASMJIT_INLINE_NODEBUG Error append(const String& other) noexcept { return append(other.data(), other.size()); }

  //! Appends `c` character repeated `n` times.
  ASMJIT_API Error append_chars(char c, size_t n) noexcept;
  //! Appends a single `c` character.
  ASMJIT_INLINE_NODEBUG Error append(char c) noexcept { return append_chars(c, 1u); }

  //! Appends formatted integer `i` to the string (internal).
  ASMJIT_API Error _append_int(uint64_t i, uint32_t base, size_t width, StringFormatFlags flags) noexcept;

  //! Appends formatted integer `i` to the string.
  //!
  //! The optional `width` specifies the minimum total output size, including the sign and alternate prefix.
  template<typename Integer>
  ASMJIT_INLINE_NODEBUG Error append_int(const Integer& i, uint32_t base = 0, size_t width = 0, StringFormatFlags flags = StringFormatFlags::kNone) noexcept {
    if constexpr (std::is_signed_v<Integer>) {
      return _append_int(static_cast<uint64_t>(static_cast<int64_t>(i)), base, width, flags | StringFormatFlags::kSigned);
    }
    else {
      return _append_int(static_cast<uint64_t>(i), base, width, flags);
    }
  }

  //! Appends the given `data` converted to a HEX string with an optional `separator`.
  //!
  //! \warning This function doesn't support aliasing. It's undefined behavior if the provided `data` pointer
  //! aliases the content of the string.
  ASMJIT_API Error append_hex(const void* data, size_t size, char separator = '\0') noexcept;

  //! Appends a formatted string `fmt` with `args`.
  ASMJIT_API Error append_format(const char* fmt, ...) noexcept;
  //! Appends a formatted string `fmt` (va_list version).
  ASMJIT_API Error append_vformat(const char* fmt, va_list ap) noexcept;

  ASMJIT_API Error pad_end(size_t n, char c = ' ') noexcept;

  //! Truncates the string length into `new_size`.
  ASMJIT_API Error truncate(size_t new_size) noexcept;

  //! Compares this string with the given span represented by `data` and `size`.
  [[nodiscard]] ASMJIT_API bool equals(const char* data, size_t size = SIZE_MAX) const noexcept;
  //! Compares this string with the given `span`.
  [[nodiscard]] ASMJIT_INLINE_NODEBUG bool equals(Span<const char> span) const noexcept { return equals(span.data(), span.size()); }
  //! Compares this string with `other` string.
  [[nodiscard]] ASMJIT_INLINE_NODEBUG bool equals(const String& other) const noexcept { return equals(other.data(), other.size()); }

  //! \}

  //! \name Internal Functions
  //! \{

  //! Resets string to embedded and makes it empty (zero length, zero first char)
  //!
  //! \note This is always called internally after an external buffer was released as it zeroes all bytes
  //! used by String's embedded storage.
  ASMJIT_INLINE void _reset_internal() noexcept {
    for (size_t i = 0; i < ASMJIT_ARRAY_SIZE(_raw.uptr); i++) {
      _raw.uptr[i] = 0;
    }
  }

  //! Sets the size of the string.
  //!
  //! \note This function is internal to AsmJit - it sets the size of the string without terminating the data by
  //! null terminator - so it can only be used by users that understand the behavior. Typically, this function
  //! is used in cases in which \ref assign_uninitialized() or \ref append_uninitialized() functions were used to
  //! reserve space for data, but the data assigned or appended was smaller than the reservation size.
  ASMJIT_INLINE void _set_size(size_t new_size) noexcept {
    ASMJIT_ASSERT(new_size <= capacity());

    if (is_large_or_external()) {
      _large.size = new_size;
    }
    else {
      _small.type = uint8_t(new_size);
    }
  }

  //! \}
};

//! Temporary string builder, has statically allocated `N` bytes.
template<size_t N>
class StringTmp : public String {
public:
  ASMJIT_NONCOPYABLE(StringTmp)

  //! Embedded data.
  char _embedded_data[axl::align_up(N + 1, sizeof(size_t))];

  //! \name Construction & Destruction
  //! \{

  ASMJIT_INLINE_NODEBUG StringTmp() noexcept {
    _reset_to_temporary();
  }

  inline void _reset_to_temporary() noexcept {
    _large.type = kTypeExternal;
    _large.capacity = ASMJIT_ARRAY_SIZE(_embedded_data) - 1;
    _large.data = _embedded_data;
    _embedded_data[0] = '\0';
  }

  //! \}
};

//! \}

ASMJIT_END_NAMESPACE

#endif // ASMJIT_CORE_STRING_H_INCLUDED
