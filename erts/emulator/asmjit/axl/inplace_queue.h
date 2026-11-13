// This file is part of AsmJit project <https://asmjit.com>
//
// See <asmjit/core.h> or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#ifndef ASMJIT_AXL_INPLACE_QUEUE_H_INCLUDED
#define ASMJIT_AXL_INPLACE_QUEUE_H_INCLUDED

#include <asmjit/axl/commons.h>

ASMJIT_BEGIN_SUB_NAMESPACE(axl)

//! \addtogroup asmjit_axl
//! \{

//! Inplace queue backed by a fixed storage - cannot hold more items than the storage capacity provides.
template<typename T>
struct InplaceQueue {
protected:
  T* _begin {};
  T* _end {};
  T* _read_ptr {};
  T* _store_ptr {};

public:
  ASMJIT_INLINE_NODEBUG InplaceQueue(T* storage, size_t capacity) noexcept
    : _begin(storage),
      _end(storage + capacity),
      _read_ptr(storage),
      _store_ptr(storage) {}

  [[nodiscard]]
  ASMJIT_INLINE_NODEBUG bool is_empty() const noexcept {
    return _read_ptr == _store_ptr;
  }

  //! Gets an item from the queue (the item is removed).
  [[nodiscard]]
  ASMJIT_INLINE T get() noexcept {
    T item = *_read_ptr++;
    if (ASMJIT_UNLIKELY(_read_ptr == _end)) {
      _read_ptr = _begin;
    }
    return item;
  }

  //! Puts an item to the queue's back.
  ASMJIT_INLINE void put_back(T item) noexcept {
    *_store_ptr++ = item;
    if (ASMJIT_UNLIKELY(_store_ptr == _end)) {
      _store_ptr = _begin;
    }
  }

  //! Puts an item to the queue's front.
  ASMJIT_INLINE void put_front(T item) noexcept {
    if (ASMJIT_UNLIKELY(_read_ptr == _begin)) {
      _read_ptr = _end;
    }
    *--_read_ptr = item;
  }
};

//! \}

ASMJIT_END_SUB_NAMESPACE

#endif // ASMJIT_AXL_INPLACE_QUEUE_H_INCLUDED
