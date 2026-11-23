// This file is part of AsmJit project <https://asmjit.com>
//
// See <asmjit/core.h> or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#ifndef ASMJIT_AXL_INPLACE_STACK_H_INCLUDED
#define ASMJIT_AXL_INPLACE_STACK_H_INCLUDED

#include <asmjit/axl/commons.h>

ASMJIT_BEGIN_SUB_NAMESPACE(axl)

//! \addtogroup asmjit_axl
//! \{

//! Inplace stack backed by a fixed storage - cannot hold more items than the storage capacity provides.
template<typename T>
class InplaceStack {
protected:
  T* _begin {};
  T* _end {};
  T* _ptr {};

public:
  //! Creates an inplace stack with a fixed storage provided by `storage` and `capacity` members.
  ASMJIT_INLINE_NODEBUG InplaceStack(T* storage, size_t capacity) noexcept
    : _begin(storage),
      _end(storage + capacity),
      _ptr(storage) {}

  [[nodiscard]] ASMJIT_INLINE_NODEBUG bool is_empty() const noexcept { return _begin == _ptr; }

  [[nodiscard]] ASMJIT_INLINE_NODEBUG bool can_push() const noexcept { return _begin != _end; }
  [[nodiscard]] ASMJIT_INLINE_NODEBUG bool can_pop() const noexcept { return _begin != _ptr; }

  [[nodiscard]] ASMJIT_INLINE_NODEBUG size_t size() const noexcept { return (size_t)(_ptr - _begin); }
  [[nodiscard]] ASMJIT_INLINE_NODEBUG size_t capacity() const noexcept { return (size_t)(_end - _begin); }

  //! Pushes an item to the stack.
  ASMJIT_INLINE void push(T item) noexcept {
    ASMJIT_ASSERT(can_push());
    *_ptr++ = item;
  }

  //! Pops an item from the stack and returns it.
  [[nodiscard]] ASMJIT_INLINE T pop() noexcept {
    ASMJIT_ASSERT(can_pop());
    return *--_ptr;
  }
};

//! \}

ASMJIT_END_SUB_NAMESPACE

#endif // ASMJIT_AXL_INPLACE_STACK_H_INCLUDED
