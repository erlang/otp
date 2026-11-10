// This file is part of AsmJit project <https://asmjit.com>
//
// See <asmjit/core.h> or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#ifndef ASMJIT_AXL_ARENA_POOL_H_INCLUDED
#define ASMJIT_AXL_ARENA_POOL_H_INCLUDED

#include <asmjit/axl/commons.h>

ASMJIT_BEGIN_SUB_NAMESPACE(axl)

//! \addtogroup asmjit_axl
//! \{

//! Helper class for implementing pooling of arena-allocated objects.
template<typename T, size_t Size = sizeof(T)>
  requires(Size >= sizeof(void*))
class ArenaPool {
public:
  ASMJIT_NONCOPYABLE(ArenaPool)

  struct Link { Link* next; };
  Link* _data {};

  ASMJIT_INLINE_NODEBUG ArenaPool() noexcept = default;

  //! Resets the arena pool.
  //!
  //! Reset must be called before the associated \ref Arena is reset or destroyed to invalidate all pooled chunks.
  ASMJIT_INLINE_NODEBUG void reset() noexcept { _data = nullptr; }

  //! Allocates a memory (or reuses the existing allocation) of `Size` (in bytes).
  [[nodiscard]]
  ASMJIT_INLINE T* alloc(Arena& arena) noexcept {
    Link* p = _data;
    if (ASMJIT_UNLIKELY(p == nullptr)) {
      return arena.alloc_oneshot<T>(Arena::aligned_size(Size));
    }
    _data = p->next;
    return static_cast<T*>(static_cast<void*>(p));
  }

  //! Pools the previously allocated memory.
  ASMJIT_INLINE void release(T* ptr) noexcept {
    ASMJIT_ASSERT(ptr != nullptr);
    Link* p = reinterpret_cast<Link*>(ptr);

    p->next = _data;
    _data = p;
  }

  ASMJIT_INLINE size_t pooled_item_count() const noexcept {
    size_t n = 0;
    Link* p = _data;
    while (p) {
      n++;
      p = p->next;
    }
    return n;
  }
};
//! \}

ASMJIT_END_SUB_NAMESPACE

#endif // ASMJIT_AXL_ARENA_POOL_H_INCLUDED
