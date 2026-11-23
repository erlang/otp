// This file is part of AsmJit project <https://asmjit.com>
//
// See <asmjit/core.h> or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#ifndef ASMJIT_AXL_ALGORITHM_H_INCLUDED
#define ASMJIT_AXL_ALGORITHM_H_INCLUDED

#include <asmjit/axl/commons.h>
#include <asmjit/axl/span.h>

ASMJIT_BEGIN_SUB_NAMESPACE(axl)

//! \addtogroup asmjit_axl
//! \{

//! A default implementation of less_than used by some algorithms.
//!
//! The idea is to either specialize this one or to pass your own to functions such as \ref lower_bound() and \ref sort().
struct Less {
  template<typename A, typename B>
  ASMJIT_INLINE_NODEBUG bool operator()(const A& a, const B& b) const noexcept { return a < b; }
};

//! Searches for the first element in the partitioned source range which is not ordered before value.
template<typename T, typename Value, typename LessFn = Less>
[[nodiscard]]
static ASMJIT_INLINE size_t lower_bound(Span<T> span, const Value& value, LessFn&& less_than_fn = LessFn{}) noexcept {
  size_t index = 0;
  size_t size = span.size();
  auto data = span.data();

  while (size) {
    size_t half = size / 2u;
    size_t middle = index + half;

    bool b = less_than_fn(data[middle], value);
    size -= half + uint32_t(b);
    middle++;

    if (b) {
      index = middle;
    }
    else {
      size = half;
    }
  }

  return index;
}

//! Insertion sort.
template<typename T, typename LessFn = Less>
static ASMJIT_INLINE void insertion_sort(Span<T> span, LessFn&& less_than_fn = LessFn{}) noexcept {
  T* base = span.data_mut();
  size_t size = span.size();

  for (T* pm = base + 1; pm < base + size; pm++) {
    for (T* pl = pm; pl > base && less_than_fn(pl[0], pl[-1]); pl--) {
      swap(pl[-1], pl[0]);
    }
  }
}

//! Quick sort.
//!
//! The main reason to provide a custom qsort implementation is that we needed something that will
//! never throw `bad_alloc` exception. This implementation doesn't use dynamic memory allocation.
template<typename T, class LessFn = Less>
static ASMJIT_INLINE_NODEBUG void sort(Span<T> span, LessFn&& less_than_fn = LessFn{}) noexcept {
  constexpr size_t kStackSize = 64u * 2u;
  constexpr size_t kISortThreshold = 7u;

  T* base = span.data_mut();
  size_t size = span.size();

  T* end = base + size;
  T* stack[kStackSize];
  T** stack_ptr = stack;

  for (;;) {
    if ((size_t)(end - base) > kISortThreshold) {
      // We work from second to last - first will be pivot element.
      T* pi = base + 1;
      T* pj = end - 1;
      swap(base[(size_t)(end - base) / 2], base[0]);

      if (less_than_fn(*pj  , *pi  )) { swap(*pi  , *pj  ); }
      if (less_than_fn(*pj  , *base)) { swap(*base, *pj  ); }
      if (less_than_fn(*base, *pi  )) { swap(*pi  , *base); }

      // Now we have the median for pivot element, entering main loop.
      for (;;) {
        while (pi < pj   && less_than_fn(*++pi, *base)) continue; // Move `i` right until `*i >= pivot`.
        while (pj > base && less_than_fn(*base, *--pj)) continue; // Move `j` left  until `*j <= pivot`.

        if (pi > pj) {
          break;
        }
        swap(*pi, *pj);
      }

      // Move pivot into correct place.
      swap(*base, *pj);

      // Larger subfile base / end to stack, sort smaller.
      if (pj - base > end - pi) {
        // Left is larger.
        *stack_ptr++ = base;
        *stack_ptr++ = pj;
        base = pi;
      }
      else {
        // Right is larger.
        *stack_ptr++ = pi;
        *stack_ptr++ = end;
        end = pj;
      }
      ASMJIT_ASSERT(stack_ptr <= stack + kStackSize);
    }
    else {
      // UB sanitizer doesn't like applying offset to a nullptr base.
      if (base != end) {
        insertion_sort(Span<T>(base, (size_t)(end - base)), less_than_fn);
      }

      if (stack_ptr == stack) {
        break;
      }

      end = *--stack_ptr;
      base = *--stack_ptr;
    }
  }
}

//! \}

ASMJIT_END_SUB_NAMESPACE

#endif // ASMJIT_AXL_ALGORITHM_H_INCLUDED
