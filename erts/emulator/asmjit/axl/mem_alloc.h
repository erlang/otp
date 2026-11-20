// This file is part of Blend2D project <https://blend2d.com>
//
// See blend2d.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#ifndef ASMJIT_AXL_MEM_ALLOC_H_INCLUDED
#define ASMJIT_AXL_MEM_ALLOC_H_INCLUDED

#include <asmjit/axl/build_integration.h>

#include <stdlib.h>

ASMJIT_BEGIN_SUB_NAMESPACE(axl)

//! \addtogroup asmjit_axl
//! \{

// Memory Allocation
// =================

template<typename T = void>
static inline T* mem_alloc(size_t size) noexcept {
  return static_cast<T*>(::malloc(size));
}

template<typename T = void>
static inline T* mem_alloc_zeroed(size_t size) noexcept {
  return static_cast<T*>(::calloc(size, 1u));
}

template<typename T = void>
static inline T* mem_realloc(T* ptr, size_t size) noexcept {
  return static_cast<T*>(::realloc(static_cast<void*>(ptr), size));
}

template<typename T>
static inline void mem_free(T* ptr) noexcept {
  return ::free(static_cast<void*>(ptr));
}

//! \}

ASMJIT_END_SUB_NAMESPACE

#endif // ASMJIT_AXL_MEM_ALLOC_H_INCLUDED
