// This file is part of AsmJit project <https://asmjit.com>
//
// See <asmjit/core.h> or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#ifndef ASMJIT_AXL_SPAN_H_INCLUDED
#define ASMJIT_AXL_SPAN_H_INCLUDED

#include <asmjit/axl/commons.h>

ASMJIT_BEGIN_NAMESPACE

//! \addtogroup asmjit_axl
//! \{

//! Forward iterator to avoid including `<iterator>` header for iteration over arrays, specialized for AsmJit use.
template<typename T>
class SpanForwardIterator {
public:
  //! \name Members
  //! \{

  T* ptr {};

  //! \}

  //! \name Overloaded Operators
  //! \{

  ASMJIT_INLINE_CONSTEXPR bool operator==(const T* other) const noexcept { return ptr == other; }
  ASMJIT_INLINE_CONSTEXPR bool operator==(const SpanForwardIterator& other) const noexcept { return ptr == other.ptr; }

  ASMJIT_INLINE_CONSTEXPR bool operator!=(const T* other) const noexcept { return ptr != other; }
  ASMJIT_INLINE_CONSTEXPR bool operator!=(const SpanForwardIterator& other) const noexcept { return ptr != other.ptr; }

  ASMJIT_INLINE_CONSTEXPR SpanForwardIterator& operator++() noexcept { ptr++; return *this; }
  ASMJIT_INLINE_CONSTEXPR SpanForwardIterator operator++(int) noexcept { SpanForwardIterator prev(*this); ptr++; return prev; }

  ASMJIT_INLINE_CONSTEXPR T& operator*() const noexcept { return *ptr; }
  ASMJIT_INLINE_CONSTEXPR T* operator->() const noexcept { return ptr; }

  ASMJIT_INLINE_CONSTEXPR operator T*() const noexcept { return ptr; }

  //! \}
};

//! Reverse iterator to avoid including `<iterator>` header for iteration over arrays, specialized for AsmJit use.
template<typename T>
class SpanReverseIterator {
public:
  //! \name Members
  //! \{

  T* ptr {};

  //! \}

  //! \name Overloaded Operators
  //! \{

  ASMJIT_INLINE_CONSTEXPR bool operator==(const T* other) const noexcept { return ptr == other; }
  ASMJIT_INLINE_CONSTEXPR bool operator==(const SpanReverseIterator& other) const noexcept { return ptr == other.ptr; }

  ASMJIT_INLINE_CONSTEXPR bool operator!=(const T* other) const noexcept { return ptr != other; }
  ASMJIT_INLINE_CONSTEXPR bool operator!=(const SpanReverseIterator& other) const noexcept { return ptr != other.ptr; }

  ASMJIT_INLINE_CONSTEXPR SpanReverseIterator& operator++() noexcept { ptr--; return *this; }
  ASMJIT_INLINE_CONSTEXPR SpanReverseIterator operator++(int) noexcept { SpanReverseIterator prev(*this); ptr--; return prev; }

  ASMJIT_INLINE_CONSTEXPR T& operator*() const noexcept { return ptr[-1]; }
  ASMJIT_INLINE_CONSTEXPR T* operator->() const noexcept { return &ptr[-1]; }

  ASMJIT_INLINE_CONSTEXPR operator T*() const noexcept { return &ptr[-1]; }

  //! \}
};

template<typename T>
class SpanForwardIteratorAdaptor {
public:
  //! \name Types
  //! \{

  using iterator = SpanForwardIterator<T>;

  //! \}

  //! \name Members
  //! \{

  T* _begin {};
  T* _end {};

  //! \}

  //! \name C++ Compatibility
  //! \{

  [[nodiscard]]
  ASMJIT_INLINE_CONSTEXPR iterator begin() const noexcept { return iterator{_begin}; };

  [[nodiscard]]
  ASMJIT_INLINE_CONSTEXPR iterator end() const noexcept { return iterator{_end}; };

  //! \}
};

template<typename T>
struct SpanReverseView {
  //! \name Types
  //! \{

  using value_type = std::remove_cv_t<T>;
  using const_type = std::add_const_t<value_type>;

  //! \}

  //! \name Members
  //! \{

  //! Beginning of span data.
  T* _data {};
  //! Span size.
  size_t _size {};

  //! \}

  //! \name Iteration
  //! \{

  struct Iterator {
    //! \name Members
    //! \{

    T* ptr {};

    //! \}

    //! \name Overloaded Operators
    //! \{

    ASMJIT_INLINE_CONSTEXPR bool operator==(const T* other) const noexcept { return ptr == other; }
    ASMJIT_INLINE_CONSTEXPR bool operator==(const Iterator& other) const noexcept { return ptr == other.ptr; }

    ASMJIT_INLINE_CONSTEXPR bool operator!=(const T* other) const noexcept { return ptr != other; }
    ASMJIT_INLINE_CONSTEXPR bool operator!=(const Iterator& other) const noexcept { return ptr != other.ptr; }

    ASMJIT_INLINE_CONSTEXPR Iterator& operator++() noexcept { ptr--; return *this; }
    ASMJIT_INLINE_CONSTEXPR Iterator operator++(int) noexcept { Iterator prev(*this); ptr--; return prev; }

    ASMJIT_INLINE_CONSTEXPR T& operator*() const noexcept { return ptr[-1]; }
    ASMJIT_INLINE_CONSTEXPR T* operator->() const noexcept { return &ptr[-1]; }

    ASMJIT_INLINE_CONSTEXPR operator T*() const noexcept { return &ptr[-1]; }

    //! \}

  };

  [[nodiscard]] ASMJIT_INLINE_CONSTEXPR Iterator begin() const noexcept { return Iterator{_data + _size}; };
  [[nodiscard]] ASMJIT_INLINE_CONSTEXPR Iterator end() const noexcept { return Iterator{_data}; };

  //! \}
};

template<typename T>
class SpanReverseIteratorAdaptor {
public:
  //! \name Types
  //! \{

  using iterator = SpanReverseIterator<T>;

  //! \}

  //! \name Members
  //! \{

  T* _begin {};
  T* _end {};

  //! \}

  //! \name C++ Compatibility
  //! \{

  [[nodiscard]]
  ASMJIT_INLINE_CONSTEXPR iterator begin() const noexcept { return iterator{_end}; };

  [[nodiscard]]
  ASMJIT_INLINE_CONSTEXPR iterator end() const noexcept { return iterator{_begin}; };

  //! \}
};

//! Span is a non-owning array that contains pointer to data and its size.
template<typename T>
struct Span {
  //! \name Types
  //! \{

  using value_type = std::remove_cv_t<T>;
  using const_type = std::add_const_t<value_type>;

  //! \}

  //! \name Members
  //! \{

  //! Beginning of span data.
  T* _data {};
  //! Span size.
  size_t _size {};

  //! \}

  //! \name Construction & Destruction
  //! \{

  //! Creates an empty span.
  //!
  //! \remarks Empty span points to `nullptr` and has a size equal to 0.
  ASMJIT_INLINE_CONSTEXPR Span() noexcept = default;

  //! Creates a Span from the beginning of the `data` passed and `size`.
  ASMJIT_INLINE_CONSTEXPR Span(T* data, size_t size) noexcept
    : _data(data),
      _size(size) {}

  //! Creates a Span from `other` - possibly casting from other data type to a compatible type.
  template<typename X>
  ASMJIT_INLINE_CONSTEXPR Span(Span<X> other) noexcept
    : _data(static_cast<T*>(other._data)),
      _size(other._size) {}

  //! Creates a Span from `std::initializer_list`.
  ASMJIT_INLINE_CONSTEXPR explicit Span(std::initializer_list<value_type> il)
    requires std::is_const_v<T>
    : _data(il.begin()),
      _size(il.size()) {}

  //! Creates a Span from a C/C++ array of a known size.
  template<size_t N>
  static ASMJIT_INLINE_CONSTEXPR Span<T> from_array(T(&array)[N]) noexcept { return Span<T>(array, N); }

  //! \}

  //! \name Overloaded Operators
  //! \{

  //! Assigns `other` to this span.
  //!
  //! \remarks No data is copied, just pointer and size are assigned.
  template<typename X>
  ASMJIT_INLINE_CONSTEXPR T& operator=(Span<X> other) noexcept {
    _data = other._data;
    _size = other._size;
    return *this;
  }

  //! Returns whether this span is equal with `other`.
  template<typename X>
  [[nodiscard]]
  ASMJIT_INLINE bool operator==(Span<X> other) noexcept { return equals(other); }

  //! Returns whether this span is equal with `other`.
  template<typename X>
  [[nodiscard]]
  ASMJIT_INLINE bool operator!=(Span<X> other) noexcept { return !equals(other); }

  //! Returns a reference to an item at the given `index`.
  //!
  //! \remarks Index bounds checked is only performed in debug mode when assertions are enabled.
  [[nodiscard]]
  ASMJIT_INLINE T& operator[](size_t index) noexcept {
    ASMJIT_ASSERT(index < _size);
    return _data[index];
  }

  //! Returns a reference to an item at the given `index` (const).
  //!
  //! \remarks Index bounds checked is only performed in debug mode when assertions are enabled.
  [[nodiscard]]
  ASMJIT_INLINE const_type& operator[](size_t index) const noexcept {
    ASMJIT_ASSERT(index < _size);
    return _data[index];
  }

  //! \}

  //! \name Common Functionality
  //! \{

  //! Returns whether this span is equal with `other` by first comparing span sizes and then
  //! all their elements if both sizes match.
  template<typename X>
  [[nodiscard]]
  ASMJIT_INLINE bool equals(Span<X> other) const noexcept {
    size_t size = _size;

    if (size != other.size()) {
      return false;
    }

    for (size_t i = 0u; i < size; i++) {
      if (_data[i] != other._data[i]) {
        return false;
      }
    }

    return true;
  }

  //! Swaps this span with `other`.
  ASMJIT_INLINE void swap(Span<T>& other) noexcept {
    axl::swap(_data, other._data);
    axl::swap(_size, other._size);
  }

  //! \}

  //! \name Data Accessors
  //! \{

  //! Returns whether the span is empty, which means it holds no data and the `data()` pointer could even be nullptr.
  [[nodiscard]]
  ASMJIT_INLINE_CONSTEXPR bool is_empty() const noexcept { return _size == 0u; }

  //! Returns the number of elements this span holds.
  [[nodiscard]]
  ASMJIT_INLINE_CONSTEXPR size_t size() const noexcept { return _size; }

  //! Returns the beginning of span data (const).
  [[nodiscard]]
  ASMJIT_INLINE_CONSTEXPR const_type* data() const noexcept { return _data; }

  //! Returns the beginning of span data (mutable).
  [[nodiscard]]
  ASMJIT_INLINE_CONSTEXPR value_type* data_mut() noexcept
    requires (!std::is_const_v<T>) { return _data; }

  //! Returns a reference to the first element of the span (const).
  //!
  //! \remarks The span must have at least one element otherwise it's undefined behavior (asserts in debug mode).
  [[nodiscard]]
  ASMJIT_INLINE const_type& first() const noexcept {
    ASMJIT_ASSERT(!is_empty());
    return _data[0];
  }

  //! Returns a reference to the first element of the span (mutable).
  //!
  //! \remarks The span must have at least one element otherwise it's undefined behavior (asserts in debug mode).
  [[nodiscard]]
  ASMJIT_INLINE T& first_mut() noexcept requires (!std::is_const_v<T>) {
    ASMJIT_ASSERT(!is_empty());
    return _data[0];
  }

  //! Returns a reference to the last element of the span (const).
  //!
  //! \remarks The span must have at least one element otherwise it's undefined behavior (asserts in debug mode).
  [[nodiscard]]
  ASMJIT_INLINE const_type& last() const noexcept {
    ASMJIT_ASSERT(!is_empty());
    return _data[_size - 1u];
  }

  //! Returns a reference to the last element of the span (mutable).
  //!
  //! \remarks The span must have at least one element otherwise it's undefined behavior (asserts in debug mode).
  [[nodiscard]]
  ASMJIT_INLINE T& last_mut() noexcept requires (!std::is_const_v<T>) {
    ASMJIT_ASSERT(!is_empty());
    return _data[_size - 1u];
  }

  //! \}

  //! \name Utility Functions
  //! \{

  //! Returns a slice of this span.
  [[nodiscard]]
  ASMJIT_INLINE_CONSTEXPR Span<T> slice(size_t start) const noexcept {
    ASMJIT_ASSERT(start <= _size);
    return Span<T>(_data + start, _size - start);
  }

  //! Returns a slice of this span.
  [[nodiscard]]
  ASMJIT_INLINE_CONSTEXPR Span<T> slice(size_t start, size_t end) const noexcept {
    ASMJIT_ASSERT(start <= end && end <= _size);
    return Span<T>(_data + start, end - start);
  }

  //! Returns the first index of the given value `x` starting from `from_index` or `SIZE_MAX` if it wasn't found'.
  template<typename X>
  [[nodiscard]]
  ASMJIT_INLINE_CONSTEXPR size_t index_of(X&& x, size_t from_index) const noexcept {
    size_t size = _size;
    for (size_t i = from_index; i < size; i++) {
      if (_data[i] == x) {
        return i;
      }
    }
    return SIZE_MAX;
  }

  //! Returns the first index of the given value `x` or `SIZE_MAX` if it wasn't found'.
  template<typename X>
  [[nodiscard]]
  ASMJIT_INLINE_CONSTEXPR size_t index_of(X&& x) const noexcept { return index_of(x, 0u); }

  //! Returns the last index of the given value `x` or `SIZE_MAX` if it wasn't found.
  template<typename X>
  [[nodiscard]]
  ASMJIT_INLINE_CONSTEXPR size_t last_index_of(X&& x, size_t from_index) const noexcept {
    size_t size = _size;
    for (size_t i = from_index - 1; i < size; i--) {
      if (_data[i] == x) {
        return i;
      }
    }
    return SIZE_MAX;
  }

  //! Returns the last index of the given value `x` or `SIZE_MAX` if it wasn't found.
  template<typename X>
  [[nodiscard]]
  ASMJIT_INLINE_CONSTEXPR size_t last_index_of(X&& x) const noexcept {
    size_t i = _size - 1u;
    while (i >= 0u && !(_data[i] == x)) {
      i--;
    }
    return i; // i == SIZE_MAX
  }

  //! Tests whether the vector contains `x`.
  template<typename X>
  [[nodiscard]]
  ASMJIT_INLINE_CONSTEXPR bool contains(X&& x) const noexcept { return index_of(x) != SIZE_MAX; }

  template<typename Fn>
  [[nodiscard]]
  ASMJIT_INLINE T reduce(Fn&& fn, T initial_value = T()) const noexcept {
    T value = initial_value;
    size_t size = _size;
    for (size_t i = 0; i < size; i++) {
      value = fn(value, _data[i]);
    }
    return value;
  }

  //! \}

  //! \name Iteration
  //! \{

  ASMJIT_INLINE_NODEBUG T* begin() const noexcept { return _data; }
  ASMJIT_INLINE_NODEBUG T* end() const noexcept { return _data + _size; }

  ASMJIT_INLINE_NODEBUG SpanReverseView<T> reverse_view() const noexcept { return {_data, _size}; }

  //! \}
};

//! \}

ASMJIT_END_NAMESPACE

#endif // ASMJIT_AXL_SPAN_H_INCLUDED
