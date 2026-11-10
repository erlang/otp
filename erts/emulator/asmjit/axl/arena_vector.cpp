// This file is part of AsmJit project <https://asmjit.com>
//
// See <asmjit/core.h> or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#include <asmjit/core/api_build_p.h>

#include <asmjit/axl/arena.h>
#include <asmjit/axl/arena_vector.h>
#include <asmjit/axl/commons.h>

ASMJIT_BEGIN_SUB_NAMESPACE(axl)

template<typename ResultType, bool IsPowerOf2>
static ASMJIT_INLINE_NODEBUG ResultType byte_size_from_item_count(size_t item_count, ArenaVectorBase::ItemSize<IsPowerOf2> item_size) noexcept {
  if constexpr (IsPowerOf2) {
    return ResultType(item_count) << item_size.n;
  }
  else {
    return ResultType(item_count) * item_size.n;
  }
}

template<bool IsPowerOf2>
static ASMJIT_INLINE_NODEBUG size_t item_count_from_byte_size(size_t byte_size, ArenaVectorBase::ItemSize<IsPowerOf2> item_size) noexcept {
  if constexpr (IsPowerOf2) {
    return byte_size >> item_size.n;
  }
  else {
    return byte_size / item_size.n;
  }
}

// Rule based growing strategy - 32 bytes, 128 bytes, 512 bytes, and then grow exponentially until `kGrowThreshold`
// is reached.
static constexpr uint8_t ArenaVector_grow_rule(uint8_t log2_size) noexcept {
  return log2_size < 1u ? uint8_t(0) :
         log2_size < 2u ? uint8_t(2) :
         log2_size < 4u ? uint8_t(4) :
         log2_size < 6u ? uint8_t(6) :
         log2_size < 8u ? uint8_t(8) : uint8_t(log2_size);
}

// The table is never used fully, only indexes up to `ctz(kGrowThreshold) + 1`.
static constexpr uint8_t ArenaVector_grow_table[32] = {
  ArenaVector_grow_rule( 0), ArenaVector_grow_rule( 1), ArenaVector_grow_rule( 2), ArenaVector_grow_rule( 3),
  ArenaVector_grow_rule( 4), ArenaVector_grow_rule( 5), ArenaVector_grow_rule( 6), ArenaVector_grow_rule( 7),
  ArenaVector_grow_rule( 8), ArenaVector_grow_rule( 9), ArenaVector_grow_rule(10), ArenaVector_grow_rule(11),
  ArenaVector_grow_rule(12), ArenaVector_grow_rule(13), ArenaVector_grow_rule(14), ArenaVector_grow_rule(15),
  ArenaVector_grow_rule(16), ArenaVector_grow_rule(17), ArenaVector_grow_rule(18), ArenaVector_grow_rule(19),
  ArenaVector_grow_rule(20), ArenaVector_grow_rule(21), ArenaVector_grow_rule(22), ArenaVector_grow_rule(23),
  ArenaVector_grow_rule(24), ArenaVector_grow_rule(25), ArenaVector_grow_rule(26), ArenaVector_grow_rule(27),
  ArenaVector_grow_rule(28), ArenaVector_grow_rule(29), ArenaVector_grow_rule(30), ArenaVector_grow_rule(31)
};

static ASMJIT_INLINE size_t ArenaVector_expand_byte_size(size_t byte_size) noexcept {
  ASMJIT_ASSERT(byte_size > 0u);

  if (ASMJIT_LIKELY(byte_size <= kGrowThreshold)) {
    uint32_t grow_table_idx = bit_size_of<size_t> - clz((byte_size - 1u) | 1u);
    uint32_t grow_log2_size = ArenaVector_grow_table[grow_table_idx];

    return size_t(1) << grow_log2_size;
  }
  else {
    return align_up(size_t(byte_size) + 1u, kGrowThreshold);
  }
}

template<bool IsPowerOf2>
static ASMJIT_NOINLINE Error ArenaVector_reserve_with_byte_size(ArenaVectorBase& self, Arena& arena, size_t byte_size, ArenaVectorBase::ItemSize<IsPowerOf2> item_size) noexcept {
  size_t allocated_size;
  uint8_t* new_data = static_cast<uint8_t*>(arena.alloc_reusable(byte_size, Out(allocated_size)));

  if (ASMJIT_UNLIKELY(!new_data)) {
    return make_out_of_memory();
  }

  size_t allocated_capacity = item_count_from_byte_size(allocated_size, item_size);

  void* old_data = self._data;
  uint32_t size = self._size;

  if (old_data) {
    memcpy(new_data, old_data, byte_size_from_item_count<size_t>(size, item_size));
    arena.free_reusable(old_data, byte_size_from_item_count<size_t>(self._capacity, item_size));
  }

  self._data = new_data;
  self._capacity = uint32_t(allocated_capacity);

  return axl::kSuccess;
}

static ASMJIT_INLINE bool ArenaVector_is_valid_size(size_t size) noexcept {
  if constexpr (sizeof(size_t) < 8u) {
    // 32-bit machine - `uint32_t` is the same as `size_t` - there is no need to do any checks
    // as it's impossible to end up having a container, which data uses the whole address space.
    return true;
  }
  else {
    // 64-bit machine - since we store size and capacity as `uint32_t`, we have to check whether
    // the `size_t` argument actually fits `uint32_t`.
    return size < size_t(0xFFFFFFFFu);
  }
}

static ASMJIT_INLINE bool ArenaVector_check_byte_size(uint64_t byte_size) noexcept {
  if constexpr (sizeof(size_t) < 8u) {
    // 32-bit machine.
    return byte_size <= 0x80000000u;
  }
  else {
    return true;
  }
}

template<bool IsPowerOf2>
static ASMJIT_INLINE Error ArenaVector_reserve_fit(ArenaVectorBase& self, Arena& arena, size_t item_count, ArenaVectorBase::ItemSize<IsPowerOf2> item_size) noexcept {
  size_t capacity = self._capacity;
  size_t capacity_masked = capacity | bool_as_mask<size_t>(!ArenaVector_is_valid_size(item_count));
  uint64_t byte_size = byte_size_from_item_count<uint64_t>(item_count, item_size);

  if (ASMJIT_UNLIKELY(bool_or(capacity_masked >= item_count, !ArenaVector_check_byte_size(byte_size)))) {
    return capacity >= item_count ? kSuccess : make_out_of_memory();
  }

  return ArenaVector_reserve_with_byte_size(self, arena, size_t(byte_size), item_size);
}

template<bool IsPowerOf2>
static ASMJIT_INLINE Error ArenaVector_reserve_grow(ArenaVectorBase& self, Arena& arena, size_t item_count, ArenaVectorBase::ItemSize<IsPowerOf2> item_size) noexcept {
  size_t capacity = self._capacity;
  size_t capacity_masked = capacity | bool_as_mask<size_t>(!ArenaVector_is_valid_size(item_count));
  uint64_t byte_size = byte_size_from_item_count<uint64_t>(item_count, item_size);

  if (ASMJIT_UNLIKELY(bool_or(capacity_masked >= item_count, !ArenaVector_check_byte_size(byte_size)))) {
    return capacity >= item_count ? kSuccess : make_out_of_memory();
  }

  size_t expanded_byte_size = ArenaVector_expand_byte_size(size_t(byte_size));
  return ArenaVector_reserve_with_byte_size(self, arena, expanded_byte_size, item_size);
}

template<bool IsPowerOf2>
static ASMJIT_INLINE Error ArenaVector_grow(ArenaVectorBase& self, Arena& arena, size_t n, ArenaVectorBase::ItemSize<IsPowerOf2> item_size) noexcept {
  OverflowFlag of {};
  size_t after = add_overflow<size_t>(self._size, n, of);

  if (ASMJIT_UNLIKELY(did_overflow(of))) {
    return make_out_of_memory();
  }

  return ArenaVector_reserve_grow(self, arena, after, item_size);
}

template<bool IsPowerOf2>
static ASMJIT_INLINE Error ArenaVector_resize_fit(ArenaVectorBase& self, Arena& arena, size_t n, ArenaVectorBase::ItemSize<IsPowerOf2> item_size) noexcept {
  size_t size = self._size;
  size_t capacity = self._capacity;

  if (capacity < n) {
    ASMJIT_PROPAGATE(ArenaVector_reserve_fit(self, arena, n, item_size));
  }

  if (size < n) {
    memset(static_cast<uint8_t*>(self._data) + byte_size_from_item_count<size_t>(size, item_size), 0, byte_size_from_item_count<size_t>(n - size, item_size));
  }

  self._size = uint32_t(n);
  return axl::kSuccess;
}

template<bool IsPowerOf2>
static ASMJIT_INLINE Error ArenaVector_resize_grow(ArenaVectorBase& self, Arena& arena, size_t n, ArenaVectorBase::ItemSize<IsPowerOf2> item_size) noexcept {
  size_t size = self._size;
  size_t capacity = self._capacity;

  if (capacity < n) {
    ASMJIT_PROPAGATE(ArenaVector_reserve_grow(self, arena, n, item_size));
  }

  if (size < n) {
    memset(static_cast<uint8_t*>(self._data) + byte_size_from_item_count<size_t>(size, item_size), 0, byte_size_from_item_count<size_t>(n - size, item_size));
  }

  self._size = uint32_t(n);
  return axl::kSuccess;
}

// Public API wrappers:
Error ArenaVectorBase::_reserve_fit(Arena& arena, size_t n, ItemSize<false> item_size) noexcept {
  return ArenaVector_reserve_fit(*this, arena, n, item_size);
}

Error ArenaVectorBase::_reserve_fit(Arena& arena, size_t n, ItemSize<true> item_size) noexcept {
  return ArenaVector_reserve_fit(*this, arena, n, item_size);
}

Error ArenaVectorBase::_reserve_grow(Arena& arena, size_t n, ItemSize<false> item_size) noexcept {
  return ArenaVector_reserve_grow(*this, arena, n, item_size);
}

Error ArenaVectorBase::_reserve_grow(Arena& arena, size_t n, ItemSize<true> item_size) noexcept {
  return ArenaVector_reserve_grow(*this, arena, n, item_size);
}

Error ArenaVectorBase::_reserve_additional(Arena& arena, size_t n, ItemSize<false> item_size) noexcept {
  return ArenaVector_grow(*this, arena, n, item_size);
}

Error ArenaVectorBase::_reserve_additional(Arena& arena, size_t n, ItemSize<true> item_size) noexcept {
  return ArenaVector_grow(*this, arena, n, item_size);
}

Error ArenaVectorBase::_resize_fit(Arena& arena, size_t n, ItemSize<false> item_size) noexcept {
  return ArenaVector_resize_fit(*this, arena, n, item_size);
}

Error ArenaVectorBase::_resize_fit(Arena& arena, size_t n, ItemSize<true> item_size) noexcept {
  return ArenaVector_resize_fit(*this, arena, n, item_size);
}

Error ArenaVectorBase::_resize_grow(Arena& arena, size_t n, ItemSize<false> item_size) noexcept {
  return ArenaVector_resize_grow(*this, arena, n, item_size);
}

Error ArenaVectorBase::_resize_grow(Arena& arena, size_t n, ItemSize<true> item_size) noexcept {
  return ArenaVector_resize_grow(*this, arena, n, item_size);
}

ASMJIT_END_SUB_NAMESPACE
