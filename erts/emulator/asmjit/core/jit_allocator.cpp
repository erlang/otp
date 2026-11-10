// This file is part of AsmJit project <https://asmjit.com>
//
// See <asmjit/core.h> or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#include <asmjit/core/api_build_p.h>
#ifndef ASMJIT_NO_JIT

#include <asmjit/axl/arena.h>
#include <asmjit/axl/arena_list.h>
#include <asmjit/axl/arena_pool.h>
#include <asmjit/axl/arena_tree.h>
#include <asmjit/axl/bit_set_ops.h>
#include <asmjit/axl/commons.h>
#include <asmjit/axl/mem_alloc.h>
#include <asmjit/axl/support_p.h>
#include <asmjit/core/arch_traits.h>
#include <asmjit/core/globals.h>
#include <asmjit/core/jit_allocator.h>
#include <asmjit/core/os_utils_p.h>
#include <asmjit/core/pauth_utils_p.h>
#include <asmjit/core/virt_mem.h>
#include <asmjit/core/virt_mem_p.h>

#if defined(ASMJIT_TEST)
#include <asmjit-testing/commons/random.h>
#endif // ASMJIT_TEST

ASMJIT_BEGIN_NAMESPACE

// JitAllocator - Constants
// ========================

//! Number of pools to use when `JitAllocatorOptions::kUseMultiplePools` is set.
//!
//! Each pool increases granularity twice to make memory management more
//! efficient. Ideal number of pools appears to be 3 to 4 as it distributes
//! small and large functions properly.
static constexpr uint32_t kJitAllocatorMultiPoolCount = 3;

//! Minimum granularity (and the default granularity for pool #0).
static constexpr uint32_t kJitAllocatorBaseGranularity = 64;

//! Maximum block size (32MB).
static constexpr uint32_t kJitAllocatorMaxBlockSize = 1024 * 1024 * 64;

// JitAllocator - Fill Pattern
// ===========================

static inline uint32_t JitAllocator_default_fill_pattern() noexcept {
#if ASMJIT_TARGET_ARCH_X86
  // X86 and X86_64 - 4x 'int3' instruction.
  return 0xCCCCCCCCu;
#else
  // Unknown...
  return 0u;
#endif
}

// JitAllocator - Pool
// ===================

class JitAllocatorBlock;

class JitAllocatorPool {
public:
  ASMJIT_NONCOPYABLE(JitAllocatorPool)

  //! \name Members
  //! \{

  //! Double linked list of blocks.
  axl::ArenaList<JitAllocatorBlock> blocks;
  //! Where to start looking first.
  JitAllocatorBlock* cursor = nullptr;

  //! Count of blocks.
  uint32_t block_count = 0;
  //! Allocation granularity.
  uint16_t granularity = 0;
  //! Log2(granularity).
  uint8_t granularity_log2 = 0;
  //! Count of empty blocks (either 0 or 1 as we won't keep more blocks empty).
  uint8_t empty_block_count = 0;

  //! Number of bits reserved across all blocks.
  size_t total_area_size[2] {};
  //! Number of bits used across all blocks.
  size_t total_area_used[2] {};
  //! Overhead of all blocks (in bytes).
  size_t total_overhead_bytes = 0;

  //! \}

  ASMJIT_INLINE JitAllocatorPool(uint32_t granularity) noexcept
    : blocks(),
      granularity(uint16_t(granularity)),
      granularity_log2(uint8_t(axl::ctz(granularity))) {}

  ASMJIT_INLINE void reset() noexcept {
    blocks.reset();
    cursor = nullptr;
    block_count = 0u;
    total_area_size[0] = 0u;
    total_area_size[1] = 0u;
    total_area_used[0] = 0u;
    total_area_used[1] = 0u;
    total_overhead_bytes = 0u;
  }

  ASMJIT_INLINE_NODEBUG size_t byte_size_from_area_size(uint32_t area_size) const noexcept { return size_t(area_size) * granularity; }
  ASMJIT_INLINE_NODEBUG uint32_t area_size_from_byte_size(size_t size) const noexcept { return uint32_t((size + granularity - 1) >> granularity_log2); }

  ASMJIT_INLINE_NODEBUG size_t bit_word_count_from_area_size(uint32_t area_size) const noexcept {
    static constexpr uint32_t kBitWordSizeInBits = axl::bit_size_of<axl::BitWord>;
    return axl::align_up<size_t>(area_size, kBitWordSizeInBits) / kBitWordSizeInBits;
  }
};

// JitAllocator - Block
// ====================

class JitAllocatorBlock : public axl::ArenaTreeNodeT<JitAllocatorBlock>,
                          public axl::ArenaListNode<JitAllocatorBlock> {
public:
  ASMJIT_NONCOPYABLE(JitAllocatorBlock)

  enum Flags : uint32_t {
    //! Block has initial padding, see \ref JitAllocatorOptions::kDisableInitialPadding.
    kFlagInitialPadding = 0x00000001u,
    //! Block is empty.
    kFlagEmpty = 0x00000002u,
    //! Block is dirty (dirty largest_unused_area, search_start, search_end, ...).
    kFlagDirty = 0x00000004u,
    //! Block is in incremental mode, which means that there is no memory used after search_start.
    kFlagIncremental = 0x00000008u,
    //! Block represents memory that is using large pages.
    kFlagLargePages = 0x00000010u,
    //! Block represents memory that is dual-mapped.
    kFlagDualMapped = 0x00000020u
  };

  static_assert(kFlagInitialPadding == 1, "JitAllocatorBlock::kFlagInitialPadding must be equal to 1");

  static inline uint32_t initial_area_start_by_flags(uint32_t flags) noexcept {
    return flags & kFlagInitialPadding;
  }

  //! Link to the pool that owns this block.
  JitAllocatorPool* _pool {};
  //! Virtual memory mapping - either single mapping (both pointers equal) or
  //! dual mapping, where one pointer is Read+Execute and the second Read+Write.
  VirtMem::DualMapping _mapping {};
  //! Virtual memory size (block size) [bytes].
  size_t _block_size = 0;

  //! Block flags.
  uint32_t _flags = 0;
  //! Size of the whole block area (bit-vector size).
  uint32_t _area_size = 0;
  //! Used area (number of bits in bit-vector used).
  uint32_t _area_used = 0;
  //! The largest unused continuous area in the bit-vector (or `area_size` to initiate rescan).
  uint32_t _largest_unused_area = 0;
  //! Start of a search range (for unused bits).
  uint32_t _search_start = 0;
  //! End of a search range (for unused bits).
  uint32_t _search_end = 0;

  //! Used bit-vector (0 = unused, 1 = used).
  axl::BitWord* _used_bit_vector {};
  //! Stop bit-vector (0 = don't care, 1 = stop).
  axl::BitWord* _stop_bit_vector {};

  ASMJIT_INLINE JitAllocatorBlock(
    JitAllocatorPool* pool,
    VirtMem::DualMapping mapping,
    size_t block_size,
    uint32_t block_flags,
    axl::BitWord* used_bit_vector,
    axl::BitWord* stop_bit_vector,
    uint32_t area_size
  ) noexcept
    : ArenaTreeNodeT(),
      _pool(pool),
      _mapping(mapping),
      _block_size(block_size),
      _flags(block_flags),
      _area_size(area_size),
      _used_bit_vector(used_bit_vector),
      _stop_bit_vector(stop_bit_vector) {
    clear_block();
  }

  [[nodiscard]]
  ASMJIT_INLINE_NODEBUG JitAllocatorPool* pool() const noexcept { return _pool; }

  [[nodiscard]]
  ASMJIT_INLINE_NODEBUG uint8_t* rx_ptr() const noexcept { return static_cast<uint8_t*>(_mapping.rx); }

  [[nodiscard]]
  ASMJIT_INLINE_NODEBUG uint8_t* rw_ptr() const noexcept { return static_cast<uint8_t*>(_mapping.rw); }

  [[nodiscard]]
  ASMJIT_INLINE_NODEBUG bool has_flag(uint32_t f) const noexcept { return (_flags & f) != 0; }

  ASMJIT_INLINE_NODEBUG void add_flags(uint32_t f) noexcept { _flags |= f; }
  ASMJIT_INLINE_NODEBUG void clear_flags(uint32_t f) noexcept { _flags &= ~f; }

  [[nodiscard]]
  ASMJIT_INLINE_NODEBUG bool is_empty() const noexcept { return has_flag(kFlagEmpty); }

  [[nodiscard]]
  ASMJIT_INLINE_NODEBUG bool is_dirty() const noexcept { return has_flag(kFlagDirty); }

  ASMJIT_INLINE_NODEBUG void make_dirty() noexcept { add_flags(kFlagDirty); }

  [[nodiscard]]
  ASMJIT_INLINE_NODEBUG bool is_incremental() const noexcept { return has_flag(kFlagIncremental); }

  [[nodiscard]]
  ASMJIT_INLINE_NODEBUG bool has_large_pages() const noexcept { return has_flag(kFlagLargePages); }

  [[nodiscard]]
  ASMJIT_INLINE_NODEBUG bool has_initial_padding() const noexcept { return has_flag(kFlagInitialPadding); }

  [[nodiscard]]
  ASMJIT_INLINE_NODEBUG uint32_t initial_area_start() const noexcept { return initial_area_start_by_flags(_flags); }

  [[nodiscard]]
  ASMJIT_INLINE_NODEBUG size_t block_size() const noexcept { return _block_size; }

  [[nodiscard]]
  ASMJIT_INLINE_NODEBUG uint32_t area_size() const noexcept { return _area_size; }

  [[nodiscard]]
  ASMJIT_INLINE_NODEBUG uint32_t area_used() const noexcept { return _area_used; }

  [[nodiscard]]
  ASMJIT_INLINE_NODEBUG uint32_t area_available() const noexcept { return _area_size - _area_used; }

  [[nodiscard]]
  ASMJIT_INLINE_NODEBUG uint32_t largest_unused_area() const noexcept { return _largest_unused_area; }

  ASMJIT_INLINE void clear_block() noexcept {
    bool bit = has_initial_padding();
    size_t bit_word_count = _pool->bit_word_count_from_area_size(_area_size);

    memset(_used_bit_vector, 0, bit_word_count * sizeof(axl::BitWord));
    memset(_stop_bit_vector, 0, bit_word_count * sizeof(axl::BitWord));

    axl::bit_vector_set_bit(_used_bit_vector, 0, bit);
    axl::bit_vector_set_bit(_stop_bit_vector, 0, bit);

    uint32_t start = initial_area_start_by_flags(_flags);
    _area_used = start;
    _largest_unused_area = _area_size - start;
    _search_start = start;
    _search_end = _area_size;

    add_flags(JitAllocatorBlock::kFlagEmpty | JitAllocatorBlock::kFlagIncremental);
    clear_flags(JitAllocatorBlock::kFlagDirty);
  }

  ASMJIT_INLINE void mark_allocated_area(uint32_t allocated_area_start, uint32_t allocated_area_end) noexcept {
    uint32_t allocated_area_size = allocated_area_end - allocated_area_start;

    // Mark the newly allocated space as occupied and also the sentinel.
    axl::bit_vector_fill(_used_bit_vector, allocated_area_start, allocated_area_size);
    axl::bit_vector_set_bit(_stop_bit_vector, allocated_area_end - 1, true);

    // Update search region and statistics.
    _pool->total_area_used[size_t(has_large_pages())] += allocated_area_size;
    _area_used += allocated_area_size;

    if (area_available() == 0) {
      _search_start = _area_size;
      _search_end = 0;
      _largest_unused_area = 0;

      clear_flags(kFlagDirty | kFlagEmpty);
    }
    else {
      if (_search_start == allocated_area_start) {
        _search_start = allocated_area_end;
      }

      if (_search_end == allocated_area_end) {
        _search_end = allocated_area_start;
      }

      add_flags(kFlagDirty);
      clear_flags(kFlagEmpty);
    }
  }

  ASMJIT_INLINE void mark_released_area(uint32_t released_area_start, uint32_t released_area_end) noexcept {
    uint32_t released_area_size = released_area_end - released_area_start;

    // Update the search region and statistics.
    _pool->total_area_used[size_t(has_large_pages())] -= released_area_size;
    _area_used -= released_area_size;

    // Unmark occupied bits and also the sentinel.
    axl::bit_vector_clear(_used_bit_vector, released_area_start, released_area_size);
    axl::bit_vector_set_bit(_stop_bit_vector, released_area_end - 1, false);

    if (axl::bool_and(is_incremental(), _search_start == released_area_end)) {
      // Incremental mode: If the area released is at the end of the fully used area, we would like to
      // keep the incremental mode of the block. In order to do that, we are going to use a different
      // approach - decrement `_search_start` and increment `_largest_unused_area`, which are essential
      // for incremental mode blocks. Everything after `_search_start` is unused, so the search region
      // must end at `_area_size` (a full block sets `_search_end` to zero).
      ASMJIT_ASSERT(_search_start >= released_area_size);
      _search_start -= released_area_size;
      _search_end = _area_size;
      _largest_unused_area += released_area_size;

      if (area_used() == initial_area_start()) {
        clear_flags(kFlagDirty);
        add_flags(kFlagEmpty);
      }
    }
    else {
      _search_start = axl::min(_search_start, released_area_start);
      _search_end = axl::max(_search_end, released_area_end);
      clear_flags(kFlagDirty | kFlagIncremental);

      if (area_used() == initial_area_start()) {
        _search_start = initial_area_start();
        _search_end = _area_size;
        _largest_unused_area = _area_size - initial_area_start();
        add_flags(kFlagEmpty);
      }
      else {
        add_flags(kFlagDirty);
      }
    }
  }

  ASMJIT_INLINE void mark_shrunk_area(uint32_t shrunk_area_start, uint32_t shrunk_area_end) noexcept {
    uint32_t shrunk_area_size = shrunk_area_end - shrunk_area_start;

    // Shrunk area cannot start at zero as it would mean that we have shrunk the first
    // block to zero bytes, which is not allowed as such block must be released instead.
    ASMJIT_ASSERT(shrunk_area_start != 0);
    ASMJIT_ASSERT(shrunk_area_size != 0);

    // Update the search region and statistics.
    _pool->total_area_used[size_t(has_large_pages())] -= shrunk_area_size;
    _area_used -= shrunk_area_size;

    if (axl::bool_and(is_incremental(), _search_start == shrunk_area_end)) {
      _search_start -= shrunk_area_size;
      _search_end = _area_size;
      _largest_unused_area += shrunk_area_size;
    }
    else {
      _search_start = axl::min(_search_start, shrunk_area_start);
      _search_end = axl::max(_search_end, shrunk_area_end);

      clear_flags(kFlagIncremental);
      add_flags(kFlagDirty);
    }

    // Unmark the released space and move the sentinel.
    axl::bit_vector_clear(_used_bit_vector, shrunk_area_start, shrunk_area_size);
    axl::bit_vector_set_bit(_stop_bit_vector, shrunk_area_end - 1, false);
    axl::bit_vector_set_bit(_stop_bit_vector, shrunk_area_start - 1, true);
  }
};

static ASMJIT_INLINE_NODEBUG bool operator<(const JitAllocatorBlock& a, const JitAllocatorBlock& b) noexcept { return a.rx_ptr() < b.rx_ptr(); }
static ASMJIT_INLINE_NODEBUG bool operator<(const JitAllocatorBlock& a, const uint8_t* b) noexcept { return a.rx_ptr() + a.block_size() <= b; }
static ASMJIT_INLINE_NODEBUG bool operator<(const uint8_t* a, const JitAllocatorBlock& b) noexcept { return a < b.rx_ptr(); }

// JitAllocator - PrivateImpl
// ==========================

class JitAllocatorPrivateImpl : public JitAllocator::Impl {
public:
  //! \name Members
  //! \{

  //! Lock for thread safety.
  mutable Lock lock;
  //! System page size (also a minimum block size).
  uint32_t page_size;
  //! Number of active allocations.
  size_t allocation_count;
  //! Address ranges for executable memory, in the order they are tried.
  VirtMem::AddressRange address_ranges[2];
  //! Number of address ranges.
  uint32_t address_range_count;
  //! True if an address range was requested, even if none could be computed.
  bool address_range_requested;

  //! Blocks from all pools in RBTree.
  axl::ArenaTree<JitAllocatorBlock> tree;
  //! Allocator pools.
  JitAllocatorPool* pools;
  //! Number of allocator pools.
  size_t pool_count;

  //! \}

  ASMJIT_INLINE JitAllocatorPrivateImpl(JitAllocatorPool* pools, size_t pool_count) noexcept
    : JitAllocator::Impl {},
      page_size(0),
      allocation_count(0),
      address_ranges{},
      address_range_count(0),
      address_range_requested(false),
      pools(pools),
      pool_count(pool_count) {}
  ASMJIT_INLINE ~JitAllocatorPrivateImpl() noexcept {}
};

static const JitAllocator::Impl JitAllocatorImpl_none {};
static const JitAllocator::CreateParams JitAllocatorParams_none {};

// JitAllocator - Utilities
// ========================

// Computes the ranges below and above the module that contains `addr` (or `addr` itself if it's not in a module), from
// where a 32-bit displacement reaches the whole module. Returns the number of ranges, ordered as they should be tried.
static uint32_t JitAllocator_near_address_ranges(uintptr_t addr, VirtMem::AddressRange* ranges) noexcept {
#if ASMJIT_TARGET_ARCH_X86 == 64
  // 2GiB minus 64kB.
  constexpr uintptr_t kReach = 0x7FFF0000u;
  // A lower bound that keeps the range above `vm.mmap_min_addr`.
  constexpr uintptr_t kMinAddress = 0x100000u;

  VirtMem::AddressRange module {};
  if (VirtMem::module_address_range(addr, Out(module)) != Error::kOk) {
    module = VirtMem::AddressRange{addr, addr + 1u};
  }

  if (module.end - module.start > kReach) {
    return 0;
  }

  VirtMem::AddressRange below { module.end > kMinAddress + kReach ? module.end - kReach : kMinAddress, module.start };
  VirtMem::AddressRange above { module.end, module.start + kReach };

#if defined(__APPLE__)
  VirtMem::AddressRange candidates[2] = { above, below };
#else
  VirtMem::AddressRange candidates[2] = { below, above };
#endif

  uint32_t count = 0;
  for (const VirtMem::AddressRange& range : candidates) {
    if (range.start < range.end) {
      ranges[count++] = range;
    }
  }
  return count;
#else
  axl::maybe_unused(addr, ranges);
  return 0;
#endif
}

static inline JitAllocatorPrivateImpl* JitAllocator_new_impl(const JitAllocator::CreateParams* params) noexcept {
  VirtMem::Info vm_info = VirtMem::info();

  if (!params) {
    params = &JitAllocatorParams_none;
  }

  JitAllocatorOptions options = params->options;
  uint32_t block_size = params->block_size;
  uint32_t granularity = params->granularity;
  uint32_t fill_pattern = params->fill_pattern;

  // Setup pool count to [1..3].
  size_t pool_count = 1;
  if (axl::test(options, JitAllocatorOptions::kUseMultiplePools)) {
    pool_count = kJitAllocatorMultiPoolCount;
  }

  // Setup block size [64kB..256MB].
  if (block_size < 64 * 1024 || block_size > 256 * 1024 * 1024 || !axl::is_power_of_2(block_size)) {
    block_size = vm_info.page_granularity;
  }

  // Setup granularity [64..256].
  if (granularity < 64 || granularity > 256 || !axl::is_power_of_2(granularity)) {
    granularity = kJitAllocatorBaseGranularity;
  }

  // Setup fill-pattern.
  if (uint32_t(options & JitAllocatorOptions::kCustomFillPattern) == 0) {
    fill_pattern = JitAllocator_default_fill_pattern();
  }

  size_t size = sizeof(JitAllocatorPrivateImpl) + sizeof(JitAllocatorPool) * pool_count;
  void* p = axl::mem_alloc(size);

  if (ASMJIT_UNLIKELY(!p)) {
    return nullptr;
  }

  VirtMem::HardenedRuntimeInfo hardened_rt_info = VirtMem::hardened_runtime_info();
  if (axl::test(hardened_rt_info.flags, VirtMem::HardenedRuntimeFlags::kEnabled)) {
    // If we are running within a hardened environment (mapping RWX is not allowed) then we have to use dual mapping
    // or other runtime capabilities like Apple specific MAP_JIT. There is no point in not enabling these as otherwise
    // the allocation would fail and JitAllocator would not be able to allocate memory.
    if (!axl::test(hardened_rt_info.flags, VirtMem::HardenedRuntimeFlags::kMapJit)) {
      options |= JitAllocatorOptions::kUseDualMapping;
    }
  }

  JitAllocatorPool* pools = reinterpret_cast<JitAllocatorPool*>((uint8_t*)p + sizeof(JitAllocatorPrivateImpl));
  JitAllocatorPrivateImpl* impl = new(axl::PlacementNew{p}) JitAllocatorPrivateImpl(pools, pool_count);

  impl->options = options;
  impl->block_size = block_size;
  impl->granularity = granularity;
  impl->fill_pattern = fill_pattern;
  impl->page_size = vm_info.page_size;

  if (params->address_range.end) {
    impl->address_ranges[0] = params->address_range;
    impl->address_range_count = 1;
    impl->address_range_requested = true;
  }
  else if (params->near_address) {
    impl->address_range_count = JitAllocator_near_address_ranges(params->near_address, impl->address_ranges);
    impl->address_range_requested = true;
  }

  for (size_t pool_id = 0; pool_id < pool_count; pool_id++) {
    new(axl::PlacementNew{&pools[pool_id]}) JitAllocatorPool(granularity << pool_id);
  }

  return impl;
}

static ASMJIT_INLINE void JitAllocator_destroy_impl(JitAllocatorPrivateImpl* impl) noexcept {
  impl->~JitAllocatorPrivateImpl();
  axl::mem_free(impl);
}

static ASMJIT_INLINE size_t JitAllocator_size_to_pool_id(const JitAllocatorPrivateImpl* impl, size_t size) noexcept {
  size_t pool_id = impl->pool_count - 1;
  size_t granularity = size_t(impl->granularity) << pool_id;

  while (pool_id) {
    if (axl::align_up(size, granularity) == size) {
      break;
    }
    pool_id--;
    granularity >>= 1;
  }

  return pool_id;
}

static ASMJIT_INLINE size_t JitAllocator_bit_vector_size_to_byte_size(uint32_t area_size) noexcept {
  static constexpr uint32_t kBitWordSizeInBits = axl::bit_size_of<axl::BitWord>;
  return ((area_size + kBitWordSizeInBits - 1u) / kBitWordSizeInBits) * sizeof(axl::BitWord);
}

static ASMJIT_INLINE size_t JitAllocator_calculate_ideal_block_size(JitAllocatorPrivateImpl* impl, JitAllocatorPool* pool, size_t allocation_size) noexcept {
  JitAllocatorBlock* last = pool->blocks.last();
  size_t block_size = last ? last->block_size() : size_t(impl->block_size);

  // We have to increase the allocation_size if we know that the block must provide padding.
  if (!axl::test(impl->options, JitAllocatorOptions::kDisableInitialPadding)) {
    size_t granularity = pool->granularity;
    if (SIZE_MAX - allocation_size < granularity) {
      return 0; // Overflown
    }
    allocation_size += granularity;
  }

  if (block_size < kJitAllocatorMaxBlockSize) {
    block_size *= 2u;
  }

  if (allocation_size > block_size) {
    block_size = axl::align_up(allocation_size, impl->block_size);
    if (ASMJIT_UNLIKELY(block_size < allocation_size)) {
      return 0; // Overflown.
    }
  }

  if (axl::test(impl->options, JitAllocatorOptions::kUseDualMapping)) {
    size_t dm_limit = VirtMem::dual_mapping_size_limit();
    if (block_size > dm_limit) {
      block_size = dm_limit;
      if (allocation_size > block_size) {
        return 0;
      }
    }

  }

  return block_size;
}

ASMJIT_NOINLINE
ASMJIT_FAVOR_SPEED static void JitAllocator_fill_pattern(void* mem, uint32_t pattern, size_t byte_size) noexcept {
  // NOTE: This is always used to fill a pattern in allocated / freed memory. The allocation has always
  // a granularity that is greater than the pattern, however, when shrink() is used, we may end up having
  // an unaligned start, so deal with it here and then copy aligned pattern in the loop.
  if ((uintptr_t(mem) & 0x1u) && byte_size >= 1u) {
    static_cast<uint8_t*>(mem)[0] = uint8_t(pattern & 0xFF);
    mem = static_cast<uint8_t*>(mem) + 1;
    byte_size--;
  }

  if ((uintptr_t(mem) & 0x2u) && byte_size >= 2u) {
    static_cast<uint16_t*>(mem)[0] = uint16_t(pattern & 0xFFFF);
    mem = static_cast<uint16_t*>(mem) + 1;
    byte_size -= 2;
  }

  // Something would be seriously broken if we end up with aligned `mem`, but unaligned `byte_size`.
  ASMJIT_ASSERT((byte_size & 0x3u) == 0u);

  uint32_t* mem32 = static_cast<uint32_t*>(mem);
  size_t n = byte_size / 4u;

  for (size_t i = 0; i < n; i++) {
    mem32[i] = pattern;
  }
}

// Allocates virtual memory of a block, within the address range first if the allocator has one.
static Error JitAllocator_alloc_virt_mem(const JitAllocatorPrivateImpl* impl, VirtMem::DualMapping& vm, size_t size, VirtMem::MemoryFlags mem_flags) noexcept {
  bool dual_mapping = axl::test(impl->options, JitAllocatorOptions::kUseDualMapping);
  bool strict = axl::test(impl->options, JitAllocatorOptions::kStrictAddressRange);

  // No range can be computed from `near_address` on architectures other than X86_64.
  Error err = Error::kFeatureNotEnabled;

  for (uint32_t i = 0; i < impl->address_range_count; i++) {
    const VirtMem::AddressRange& range = impl->address_ranges[i];

    err = dual_mapping ? VirtMem::alloc_dual_mapping(Out(vm), size, mem_flags, range)
                       : VirtMem::alloc(&vm.rx, size, mem_flags, range);
    if (err == Error::kOk) {
      return err;
    }
  }

  if (strict && impl->address_range_requested) {
    return make_error(err);
  }

  return dual_mapping ? VirtMem::alloc_dual_mapping(Out(vm), size, mem_flags)
                      : VirtMem::alloc(&vm.rx, size, mem_flags);
}

// Allocate a new `JitAllocatorBlock` for the given `block_size`.
//
// NOTE: The block doesn't have `kFlagEmpty` flag set, because the new block
// is only allocated when it's actually needed, so it would be cleared anyway.
static Error JitAllocator_new_block(JitAllocatorPrivateImpl* impl, JitAllocatorBlock** dst, JitAllocatorPool* pool, size_t block_size) noexcept {
  using axl::BitWord;
  static constexpr uint32_t kBitWordSizeInBits = axl::bit_size_of<axl::BitWord>;

  uint32_t block_flags = 0;
  if (!axl::test(impl->options, JitAllocatorOptions::kDisableInitialPadding)) {
    block_flags |= JitAllocatorBlock::kFlagInitialPadding;
  }

  VirtMem::DualMapping virt_mem {};
  VirtMem::MemoryFlags mem_flags = VirtMem::MemoryFlags::kAccessRWX;

  if (axl::test(impl->options, JitAllocatorOptions::kUseDualMapping)) {
    ASMJIT_PROPAGATE(JitAllocator_alloc_virt_mem(impl, virt_mem, block_size, mem_flags));
    block_flags |= JitAllocatorBlock::kFlagDualMapped;
  }
  else {
    bool allocate_regular_pages = true;
    if (axl::test(impl->options, JitAllocatorOptions::kUseLargePages)) {
      size_t large_page_size = VirtMem::large_page_size();
      bool try_large_page = block_size >= large_page_size || axl::test(impl->options, JitAllocatorOptions::kAlignBlockSizeToLargePage);

      // Only proceed if we can actually allocate large pages.
      if (large_page_size && try_large_page) {
        size_t large_block_size = axl::align_up(block_size, large_page_size);
        Error err = JitAllocator_alloc_virt_mem(impl, virt_mem, large_block_size, mem_flags | VirtMem::MemoryFlags::kMMapLargePages);

        if (err == Error::kOk) {
          // Large page(s) allocated.
          allocate_regular_pages = false;
          block_size = large_block_size;
          block_flags |= JitAllocatorBlock::kFlagLargePages;
        }
        else {
          // Fallback to regular pages if failed to allocate large page(s).
          mem_flags |= VirtMem::MemoryFlags::kMAdviseLargePages;
        }
      }
    }

    // Called either if large pages were not requested or large page(s) allocation failed.
    if (allocate_regular_pages) {
      ASMJIT_PROPAGATE(JitAllocator_alloc_virt_mem(impl, virt_mem, block_size, mem_flags));
    }

    virt_mem.rw = virt_mem.rx;
  }

  uint32_t area_size = uint32_t(block_size >> pool->granularity_log2);
  uint32_t bit_word_count = (area_size + kBitWordSizeInBits - 1u) / kBitWordSizeInBits;
  uint8_t* block_ptr = axl::mem_alloc<uint8_t>(sizeof(JitAllocatorBlock) + size_t(bit_word_count) * 2u * sizeof(BitWord));

  // Out of memory...
  if (ASMJIT_UNLIKELY(block_ptr == nullptr)) {
    if (axl::test(impl->options, JitAllocatorOptions::kUseDualMapping)) {
      (void)VirtMem::release_dual_mapping(virt_mem, block_size);
    }
    else {
      (void)VirtMem::release(virt_mem.rx, block_size);
    }
    return make_error(Error::kOutOfMemory);
  }

  // Fill the allocated virtual memory if secure mode is enabled.
  if (axl::test(impl->options, JitAllocatorOptions::kFillUnusedMemory)) {
    VirtMem::ProtectJitReadWriteScope scope(virt_mem.rw, block_size);
    JitAllocator_fill_pattern(virt_mem.rw, impl->fill_pattern, block_size);
  }

  BitWord* bit_words = reinterpret_cast<BitWord*>(block_ptr + sizeof(JitAllocatorBlock));
  *dst = new(axl::PlacementNew{block_ptr}) JitAllocatorBlock(pool, virt_mem, block_size, block_flags, bit_words, bit_words + bit_word_count, area_size);
  return Error::kOk;
}

static void JitAllocatorImpl_delete_block(JitAllocatorPrivateImpl* impl, JitAllocatorBlock* block) noexcept {
  axl::maybe_unused(impl);

  if (block->has_flag(JitAllocatorBlock::kFlagDualMapped)) {
    (void)VirtMem::release_dual_mapping(block->_mapping, block->block_size());
  }
  else {
    (void)VirtMem::release(block->rx_ptr(), block->block_size());
  }

  axl::mem_free(block);
}

static void JitAllocatorImpl_insert_block(JitAllocatorPrivateImpl* impl, JitAllocatorBlock* block) noexcept {
  JitAllocatorPool* pool = block->pool();

  if (!pool->cursor) {
    pool->cursor = block;
  }

  // Add to RBTree and List.
  impl->tree.insert(block);
  pool->blocks.append(block);

  // Update statistics.
  size_t stat_index = size_t(block->has_large_pages());
  pool->block_count++;
  pool->total_area_size[stat_index] += block->area_size();
  pool->total_area_used[stat_index] += block->area_used();
  pool->total_overhead_bytes += sizeof(JitAllocatorBlock) + JitAllocator_bit_vector_size_to_byte_size(block->area_size()) * 2u;
}

static void JitAllocatorImpl_remove_block(JitAllocatorPrivateImpl* impl, JitAllocatorBlock* block) noexcept {
  JitAllocatorPool* pool = block->pool();

  // Remove from RBTree and List.
  if (pool->cursor == block) {
    pool->cursor = block->has_prev() ? block->prev() : block->next();
  }

  impl->tree.remove(block);
  pool->blocks.unlink(block);

  // Update statistics.
  size_t stat_index = size_t(block->has_large_pages());
  pool->block_count--;
  pool->total_area_size[stat_index] -= block->area_size();
  pool->total_area_used[stat_index] -= block->area_used();
  pool->total_overhead_bytes -= sizeof(JitAllocatorBlock) + JitAllocator_bit_vector_size_to_byte_size(block->area_size()) * 2u;
}

static void JitAllocatorImpl_wipe_out_block(JitAllocatorPrivateImpl* impl, JitAllocatorBlock* block) noexcept {
  if (block->has_flag(JitAllocatorBlock::kFlagEmpty)) {
    return;
  }

  JitAllocatorPool* pool = block->pool();
  if (axl::test(impl->options, JitAllocatorOptions::kFillUnusedMemory)) {
    VirtMem::protect_jit_memory(VirtMem::ProtectJitAccess::kReadWrite);

    uint32_t granularity = pool->granularity;
    uint8_t* rw_ptr = block->rw_ptr();
    axl::BitVectorRangeIterator<axl::BitWord, 0> it(block->_used_bit_vector, pool->bit_word_count_from_area_size(block->area_size()));

    size_t range_start;
    size_t range_end;

    while (it.next_range(Out(range_start), Out(range_end))) {
      uint8_t* span_ptr = rw_ptr + range_start * granularity;
      size_t span_size = (range_end - range_start) * granularity;

      JitAllocator_fill_pattern(span_ptr, impl->fill_pattern, span_size);
      VirtMem::flush_instruction_cache(span_ptr, span_size);
    }
    VirtMem::protect_jit_memory(VirtMem::ProtectJitAccess::kReadExecute);
  }

  block->clear_block();
}

// JitAllocator - Construction & Destruction
// =========================================

JitAllocator::JitAllocator(const CreateParams* params) noexcept {
  _impl = JitAllocator_new_impl(params);
  if (ASMJIT_UNLIKELY(!_impl)) {
    _impl = const_cast<JitAllocator::Impl*>(&JitAllocatorImpl_none);
  }
}

JitAllocator::~JitAllocator() noexcept {
  if (_impl == &JitAllocatorImpl_none) {
    return;
  }

  reset(ResetPolicy::kHard);
  JitAllocator_destroy_impl(static_cast<JitAllocatorPrivateImpl*>(_impl));
}

// JitAllocator - Reset
// ====================

void JitAllocator::reset(ResetPolicy reset_policy) noexcept {
  if (_impl == &JitAllocatorImpl_none) {
    return;
  }

  JitAllocatorPrivateImpl* impl = static_cast<JitAllocatorPrivateImpl*>(_impl);
  impl->tree.reset();
  size_t pool_count = impl->pool_count;

  for (size_t pool_id = 0; pool_id < pool_count; pool_id++) {
    JitAllocatorPool& pool = impl->pools[pool_id];
    JitAllocatorBlock* block = pool.blocks.first();

    pool.reset();

    if (block) {
      JitAllocatorBlock* block_to_keep = nullptr;
      if (reset_policy != ResetPolicy::kHard && uint32_t(impl->options & JitAllocatorOptions::kImmediateRelease) == 0) {
        block_to_keep = block;
        block = block->next();
      }

      while (block) {
        JitAllocatorBlock* next = block->next();
        JitAllocatorImpl_delete_block(impl, block);
        block = next;
      }

      if (block_to_keep) {
        block_to_keep->_list_nodes[0] = nullptr;
        block_to_keep->_list_nodes[1] = nullptr;
        JitAllocatorImpl_wipe_out_block(impl, block_to_keep);
        JitAllocatorImpl_insert_block(impl, block_to_keep);
        pool.empty_block_count = 1;
      }
    }
  }
}

// JitAllocator - Statistics
// =========================

JitAllocator::Statistics JitAllocator::statistics() const noexcept {
  Statistics statistics;
  statistics.reset();

  if (ASMJIT_LIKELY(_impl != &JitAllocatorImpl_none)) {
    JitAllocatorPrivateImpl* impl = static_cast<JitAllocatorPrivateImpl*>(_impl);
    LockGuard guard(impl->lock);

    size_t pool_count = impl->pool_count;
    for (size_t pool_id = 0; pool_id < pool_count; pool_id++) {
      const JitAllocatorPool& pool = impl->pools[pool_id];
      statistics._block_count   += size_t(pool.block_count);
      statistics._reserved_size += size_t(pool.total_area_size[0] + pool.total_area_size[1]) * pool.granularity;
      statistics._used_size     += size_t(pool.total_area_used[0] + pool.total_area_used[1]) * pool.granularity;
      statistics._overhead_size += size_t(pool.total_overhead_bytes);
    }

    statistics._allocation_count = impl->allocation_count;
  }

  return statistics;
}

// JitAllocator - Alloc & Release
// ==============================

Error JitAllocator::alloc(Out<Span> out, size_t size) noexcept {
  constexpr uint32_t no_index = axl::max_value<uint32_t>();
  constexpr size_t max_request_size = axl::max_value<uint32_t>() / 2u;

  JitAllocatorPrivateImpl* impl = static_cast<JitAllocatorPrivateImpl*>(_impl);
  bool not_initialized = _impl == &JitAllocatorImpl_none;

  // Align to the minimum granularity by default.
  size = axl::align_up<size_t>(size, impl->granularity);
  out = Span{};

  if (ASMJIT_UNLIKELY(axl::bool_or(not_initialized, size - 1u >= max_request_size))) {
    return make_error(not_initialized ? Error::kNotInitialized  :
                      size == 0u      ? Error::kInvalidArgument : Error::kTooLarge);
  }

  LockGuard guard(impl->lock);
  JitAllocatorPool* pool = &impl->pools[JitAllocator_size_to_pool_id(impl, size)];

  uint32_t area_index = no_index;
  uint32_t area_size = uint32_t(pool->area_size_from_byte_size(size));

  // Try to find the requested memory area in existing blocks.
  JitAllocatorBlock* block = pool->cursor;

  if (block) {
    JitAllocatorBlock* initial = block;

    do {
      uint32_t largest_unused_area = block->largest_unused_area();

      if (axl::bool_and(block->is_incremental(), largest_unused_area >= area_size)) {
        // Fast path: If the block is in incremental mode, which means that it's guaranteed it's full before
        // `search_start` and completely empty after it, we can just quickly increment `search_start` and be
        // done with the allocation. This is a little bit faster than constructing a BitVectorRangeIterator
        // and searching for zero bit clusters. When a block is in incremental mode its `largest_unused_area`
        // is basically the free space after `search_start`, so that's the only thing to check.
        area_index = block->_search_start;
        block->_largest_unused_area -= area_size;
        break;
      }
      else if (block->area_available() >= area_size) {
        // Regular path: Search for a cluster of bits that would mark an empty area we want to allocate.
        if (axl::bool_or(block->is_dirty(), largest_unused_area >= area_size)) {
          axl::BitVectorRangeIterator<axl::BitWord, 0> it(block->_used_bit_vector, pool->bit_word_count_from_area_size(block->area_size()), block->_search_start, block->_search_end);

          size_t range_start = 0;
          size_t range_end = block->area_size();

          size_t search_start = SIZE_MAX;
          size_t largest_area = 0;

          while (it.next_range(Out(range_start), Out(range_end), area_size)) {
            size_t range_size = range_end - range_start;
            if (range_size >= area_size) {
              area_index = uint32_t(range_start);
              break;
            }

            search_start = axl::min(search_start, range_start);
            largest_area = axl::max(largest_area, range_size);
          }

          if (area_index != no_index) {
            break;
          }

          if (search_start != SIZE_MAX) {
            // Because we have iterated over the entire block, we can now mark the
            // largest unused area that can be used to cache the next traversal.
            size_t search_end = range_end;

            block->_search_start = uint32_t(search_start);
            block->_search_end = uint32_t(search_end);
            block->_largest_unused_area = uint32_t(largest_area);
            block->clear_flags(JitAllocatorBlock::kFlagDirty);
          }
        }
      }

      // The block cursor doesn't have to start with the first block and we want to
      // iterate all before concluding that there is no free space in any block.
      block = block->has_next() ? block->next() : pool->blocks.first();
    } while (block != initial);
  }

  // Allocate a new block if there is no region of a required size.
  if (area_index == no_index) {
    size_t block_size = JitAllocator_calculate_ideal_block_size(impl, pool, size);
    if (ASMJIT_UNLIKELY(!block_size)) {
      return make_error(Error::kOutOfMemory);
    }

    ASMJIT_PROPAGATE(JitAllocator_new_block(impl, &block, pool, block_size));
    area_index = block->initial_area_start();

    JitAllocatorImpl_insert_block(impl, block);
    block->_search_start += area_size;
    block->_largest_unused_area -= area_size;
  }
  else if (block->has_flag(JitAllocatorBlock::kFlagEmpty)) {
    pool->empty_block_count--;
    block->clear_flags(JitAllocatorBlock::kFlagEmpty);
  }

  // Update statistics.
  impl->allocation_count++;
  block->mark_allocated_area(area_index, area_index + area_size);

  // Return a span referencing the allocated memory.
  size_t offset = pool->byte_size_from_area_size(area_index);
  ASMJIT_ASSERT(offset <= block->block_size() - size);

  out->_rx = block->rx_ptr() + offset;
  out->_rw = block->rw_ptr() + offset;
  out->_size = size;
  out->_block = static_cast<void*>(block);

  return Error::kOk;
}

Error JitAllocator::release(void* rx) noexcept {
  bool not_initialized = _impl == &JitAllocatorImpl_none;

  if (ASMJIT_UNLIKELY(axl::bool_or(not_initialized, !rx))) {
    return make_error(not_initialized ? Error::kNotInitialized : Error::kInvalidArgument);
  }

  JitAllocatorPrivateImpl* impl = static_cast<JitAllocatorPrivateImpl*>(_impl);
  LockGuard guard(impl->lock);

  JitAllocatorBlock* block = impl->tree.get(static_cast<uint8_t*>(rx));
  if (ASMJIT_UNLIKELY(!block)) {
    return make_error(Error::kInvalidState);
  }

  // Offset relative to the start of the block.
  JitAllocatorPool* pool = block->pool();
  size_t offset = (size_t)((uint8_t*)rx - block->rx_ptr());

  // The first bit representing the allocated area and its size.
  uint32_t area_index = uint32_t(offset >> pool->granularity_log2);
  uint32_t area_end = uint32_t(axl::bit_vector_index_of(block->_stop_bit_vector, area_index, true)) + 1;
  uint32_t area_size = area_end - area_index;

  impl->allocation_count--;
  block->mark_released_area(area_index, area_end);

  // Fill the released memory if the secure mode is enabled.
  if (axl::test(impl->options, JitAllocatorOptions::kFillUnusedMemory)) {
    uint8_t* span_ptr = block->rw_ptr() + area_index * pool->granularity;
    size_t span_size = area_size * pool->granularity;

    VirtMem::ProtectJitReadWriteScope scope(span_ptr, span_size);
    JitAllocator_fill_pattern(span_ptr, impl->fill_pattern, span_size);
  }

  // Release the whole block if it became empty.
  if (block->is_empty()) {
    if (pool->empty_block_count || axl::test(impl->options, JitAllocatorOptions::kImmediateRelease)) {
      JitAllocatorImpl_remove_block(impl, block);
      JitAllocatorImpl_delete_block(impl, block);
    }
    else {
      pool->empty_block_count++;
    }
  }

  return Error::kOk;
}

static Error JitAllocatorImpl_shrink(JitAllocatorPrivateImpl* impl, JitAllocator::Span& span, size_t new_size, bool already_under_write_scope) noexcept {
  JitAllocatorBlock* block = static_cast<JitAllocatorBlock*>(span._block);
  if (ASMJIT_UNLIKELY(!block)) {
    return make_error(Error::kInvalidArgument);
  }

  LockGuard guard(impl->lock);

  // Offset relative to the start of the block.
  JitAllocatorPool* pool = block->pool();
  size_t offset = (size_t)((uint8_t*)span.rx() - block->rx_ptr());

  // The first bit representing the allocated area and its size.
  uint32_t area_start = uint32_t(offset >> pool->granularity_log2);

  // Don't trust `span.size()` - if it has been already truncated we would be off...
  bool is_used = axl::bit_vector_get_bit(block->_used_bit_vector, area_start);
  if (ASMJIT_UNLIKELY(!is_used)) {
    return make_error(Error::kInvalidArgument);
  }

  uint32_t area_end = uint32_t(axl::bit_vector_index_of(block->_stop_bit_vector, area_start, true)) + 1;
  uint32_t area_prev_size = area_end - area_start;
  uint32_t span_prev_size = area_prev_size * pool->granularity;
  uint32_t area_shrunk_size = pool->area_size_from_byte_size(new_size);

  if (ASMJIT_UNLIKELY(area_shrunk_size > area_prev_size)) {
    return make_error(Error::kInvalidArgument);
  }

  uint32_t area_diff = area_prev_size - area_shrunk_size;
  if (area_diff) {
    block->mark_shrunk_area(area_start + area_shrunk_size, area_end);
    span._size = pool->byte_size_from_area_size(area_shrunk_size);
  }

  // Fill released memory if the secure mode is enabled.
  if (new_size < span_prev_size && axl::test(impl->options, JitAllocatorOptions::kFillUnusedMemory)) {
    uint8_t* span_ptr = block->rw_ptr() + (area_start + area_shrunk_size) * pool->granularity;
    size_t span_size = area_diff * pool->granularity;

    if (!already_under_write_scope) {
      VirtMem::ProtectJitReadWriteScope scope(span_ptr, span_size, VirtMem::CachePolicy::kNeverFlush);
      JitAllocator_fill_pattern(span_ptr, impl->fill_pattern, span_size);
    }
    else {
      JitAllocator_fill_pattern(span_ptr, impl->fill_pattern, span_size);
    }
  }

  return Error::kOk;
}

Error JitAllocator::shrink(Span& span, size_t new_size) noexcept {
  bool not_initialized = _impl == &JitAllocatorImpl_none;

  if (ASMJIT_UNLIKELY(axl::bool_or(not_initialized, !span.rx()))) {
    return make_error(not_initialized ? Error::kNotInitialized : Error::kInvalidArgument);
  }

  if (ASMJIT_UNLIKELY(new_size == 0)) {
    Error err = release(span.rx());
    span = Span{};
    return err;
  }

  return JitAllocatorImpl_shrink(static_cast<JitAllocatorPrivateImpl*>(_impl), span, new_size, false);
}

Error JitAllocator::query(Out<Span> out, void* rx) const noexcept {
  *out = Span{};

  if (ASMJIT_UNLIKELY(_impl == &JitAllocatorImpl_none)) {
    return make_error(Error::kNotInitialized);
  }

  JitAllocatorPrivateImpl* impl = static_cast<JitAllocatorPrivateImpl*>(_impl);

  LockGuard guard(impl->lock);
  JitAllocatorBlock* block = impl->tree.get(static_cast<uint8_t*>(rx));

  if (ASMJIT_UNLIKELY(!block)) {
    return make_error(Error::kInvalidArgument);
  }

  // Offset relative to the start of the block.
  JitAllocatorPool* pool = block->pool();
  size_t offset = (size_t)((uint8_t*)rx - block->rx_ptr());

  // The first bit representing the allocated area and its size.
  uint32_t area_start = uint32_t(offset >> pool->granularity_log2);

  bool is_used = axl::bit_vector_get_bit(block->_used_bit_vector, area_start);
  if (ASMJIT_UNLIKELY(!is_used)) {
    return make_error(Error::kInvalidArgument);
  }

  uint32_t area_end = uint32_t(axl::bit_vector_index_of(block->_stop_bit_vector, area_start, true)) + 1;
  size_t byte_offset = pool->byte_size_from_area_size(area_start);
  size_t byte_size = pool->byte_size_from_area_size(area_end - area_start);

  out->_rx = static_cast<uint8_t*>(block->_mapping.rx) + byte_offset;
  out->_rw = static_cast<uint8_t*>(block->_mapping.rw) + byte_offset;
  out->_size = byte_size;
  out->_block = static_cast<void*>(block);

  return Error::kOk;
}

// JitAllocator - Write
// ====================

static ASMJIT_INLINE VirtMem::CachePolicy JitAllocator_default_policy_for_span(const JitAllocator::Span& span) noexcept {
  if (axl::test(span.flags(), JitAllocator::Span::Flags::kInstructionCacheClean)) {
    return VirtMem::CachePolicy::kNeverFlush;
  }
  else {
    return VirtMem::CachePolicy::kFlushAfterWrite;
  }
}

Error JitAllocator::write(Span& span, size_t offset, const void* src, size_t size, VirtMem::CachePolicy policy) noexcept {
  if (ASMJIT_UNLIKELY(span._block == nullptr || offset > span.size() || span.size() - offset < size)) {
    return make_error(Error::kInvalidArgument);
  }

  if (ASMJIT_UNLIKELY(size == 0)) {
    return Error::kOk;
  }

  if (policy == VirtMem::CachePolicy::kDefault) {
    policy = JitAllocator_default_policy_for_span(span);
  }

  VirtMem::ProtectJitReadWriteScope write_scope(span.rx(), span.size(), policy);
  memcpy(static_cast<uint8_t*>(span.rw()) + offset, src, size);
  return Error::kOk;
}

Error JitAllocator::write(Span& span, WriteFunc write_fn, void* user_data, VirtMem::CachePolicy policy) noexcept {
  if (ASMJIT_UNLIKELY(span._block == nullptr) || span.size() == 0) {
    return make_error(Error::kInvalidArgument);
  }

  size_t size = span.size();
  if (ASMJIT_UNLIKELY(size == 0)) {
    return Error::kOk;
  }

  if (policy == VirtMem::CachePolicy::kDefault) {
    policy = JitAllocator_default_policy_for_span(span);
  }

  VirtMem::ProtectJitReadWriteScope write_scope(span.rx(), span.size(), policy);
  ASMJIT_PROPAGATE(write_fn(span, user_data));

  // Check whether span.truncate() has been called.
  if (span.size() != size) {
    // OK, this is a bit awkward... However, shrink wants the original span and new_size, so we have to swap.
    axl::swap(span._size, size);
    return JitAllocatorImpl_shrink(static_cast<JitAllocatorPrivateImpl*>(_impl), span, size, true);
  }

  return Error::kOk;
}

// JitAllocator - Write Scope
// ==========================

Error JitAllocator::begin_write_scope(WriteScopeData& scope, VirtMem::CachePolicy policy) noexcept {
  scope.policy = policy;
  scope.flags = 0u;
  scope.data[0] = 0u;
  scope.data[1] = 0u;
  return Error::kOk;
}

Error JitAllocator::end_write_scope(WriteScopeData& scope) noexcept {
  axl::maybe_unused(scope);
  return Error::kOk;
}

Error JitAllocator::flush_write_scope(WriteScopeData& scope) noexcept {
  axl::maybe_unused(scope);
  return Error::kOk;
}

Error JitAllocator::scoped_write(WriteScopeData& scope, Span& span, size_t offset, const void* src, size_t size) noexcept {
  return write(span, offset, src, size, scope.policy);
}

Error JitAllocator::scoped_write(WriteScopeData& scope, Span& span, WriteFunc write_fn, void* user_data) noexcept {
  return write(span, write_fn, user_data, scope.policy);
}

ASMJIT_END_NAMESPACE

#endif // !ASMJIT_NO_JIT
