// This file is part of AsmJit project <https://asmjit.com>
//
// See <asmjit/core.h> or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#ifndef ASMJIT_AXL_INPLACE_ARRAY_H_INCLUDED
#define ASMJIT_AXL_INPLACE_ARRAY_H_INCLUDED

#include <asmjit/axl/commons.h>
#include <asmjit/axl/span.h>

ASMJIT_BEGIN_SUB_NAMESPACE(axl)

//! \addtogroup asmjit_axl
//! \{

//! Enhanced array similar to `std::array<T, N>`, with extended features, like the possibility
//! to use enums in operator[] or to convert the array to `Span` via `span()` and `span_mut()`.
//!
//! \note The array has C semantics - the elements in the array are not initialized unless T has a default constructor.
template<typename T, size_t N>
struct InplaceArray {
  //! \name Members
  //! \{

  //! The underlying array data, use `data()` to access it.
  T _data[N];

  //! \}

  //! \name Equality & Comparison
  //! \{

  ASMJIT_INLINE_CONSTEXPR bool equals(const InplaceArray& other) const noexcept {
    for (size_t i = 0; i < N; i++) {
      if (_data[i] != other._data[i]) {
        return false;
      }
    }
    return true;
  }

  //! \}

  //! \name Overloaded Operators
  //! \{

  template<typename Index>
  ASMJIT_INLINE_CONSTEXPR T& operator[](const Index& index) noexcept {
    ASMJIT_ASSERT(as_std_uint(index) < N);
    return _data[as_std_uint(index)];
  }

  template<typename Index>
  ASMJIT_INLINE_CONSTEXPR const T& operator[](const Index& index) const noexcept {
    ASMJIT_ASSERT(as_std_uint(index) < N);
    return _data[as_std_uint(index)];
  }

  ASMJIT_INLINE_CONSTEXPR bool operator==(const InplaceArray& other) const noexcept { return equals(other); }
  ASMJIT_INLINE_CONSTEXPR bool operator!=(const InplaceArray& other) const noexcept { return !equals(other); }

  //! \}

  //! \name Accessors
  //! \{

  [[nodiscard]]
  ASMJIT_INLINE_CONSTEXPR Span<const T> span() const noexcept { return Span<const T>(_data, N); }

  [[nodiscard]]
  ASMJIT_INLINE_CONSTEXPR Span<T> span_mut() noexcept { return Span<T>(_data, N); }

  [[nodiscard]]
  ASMJIT_INLINE_CONSTEXPR bool is_empty() const noexcept { return false; }

  [[nodiscard]]
  ASMJIT_INLINE_CONSTEXPR size_t size() const noexcept { return N; }

  [[nodiscard]]
  ASMJIT_INLINE_CONSTEXPR T* data() noexcept { return _data; }

  [[nodiscard]]
  ASMJIT_INLINE_CONSTEXPR const T* data() const noexcept { return _data; }

  [[nodiscard]]
  ASMJIT_INLINE_CONSTEXPR T& front() noexcept { return _data[0]; }

  [[nodiscard]]
  ASMJIT_INLINE_CONSTEXPR const T& front() const noexcept { return _data[0]; }

  [[nodiscard]]
  ASMJIT_INLINE_CONSTEXPR T& back() noexcept { return _data[N - 1]; }

  [[nodiscard]]
  ASMJIT_INLINE_CONSTEXPR const T& back() const noexcept { return _data[N - 1]; }

  [[nodiscard]]
  ASMJIT_INLINE_CONSTEXPR T* begin() noexcept { return _data; }

  [[nodiscard]]
  ASMJIT_INLINE_CONSTEXPR T* end() noexcept { return _data + N; }

  [[nodiscard]]
  ASMJIT_INLINE_CONSTEXPR const T* begin() const noexcept { return _data; }

  [[nodiscard]]
  ASMJIT_INLINE_CONSTEXPR const T* end() const noexcept { return _data + N; }

  //! \}

  //! \name Utilities
  //! \{

  ASMJIT_INLINE void swap(InplaceArray& other) noexcept {
    for (size_t i = 0; i < N; i++) {
      axl::swap(_data[i], other._data[i]);
    }
  }

  ASMJIT_INLINE void fill(const T& value) noexcept {
    for (size_t i = 0; i < N; i++) {
      _data[i] = value;
    }
  }

  ASMJIT_INLINE void copy_from(const InplaceArray& other) noexcept {
    for (size_t i = 0; i < N; i++) {
      _data[i] = other._data[i];
    }
  }

  template<typename Operator>
  ASMJIT_INLINE void combine(const InplaceArray& other) noexcept {
    for (size_t i = 0; i < N; i++) {
      _data[i] = Operator::op(_data[i], other._data[i]);
    }
  }

  template<typename Operator>
  ASMJIT_INLINE T reduce(T initial_value = T()) const noexcept {
    T value = initial_value;
    for (size_t i = 0; i < N; i++) {
      value = Operator::op(value, _data[i]);
    }
    return value;
  }

  template<typename Fn>
  ASMJIT_INLINE void for_each(Fn&& fn) noexcept {
    for (size_t i = 0; i < N; i++) {
      fn(_data[i]);
    }
  }
  //! \}
};

//! \}

ASMJIT_END_SUB_NAMESPACE

#endif // ASMJIT_AXL_INPLACE_ARRAY_H_INCLUDED
