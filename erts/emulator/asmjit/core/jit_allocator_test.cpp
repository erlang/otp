// This file is part of AsmJit project <https://asmjit.com>
//
// See <asmjit/core.h> or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#include <asmjit/core/api_build_test_p.h>
#if defined(ASMJIT_TEST) && !defined(ASMJIT_NO_JIT)

#include <asmjit/axl/arena.h>
#include <asmjit/axl/arena_list.h>
#include <asmjit/axl/arena_pool.h>
#include <asmjit/axl/arena_tree.h>
#include <asmjit/axl/bit_set_ops.h>
#include <asmjit/axl/commons.h>
#include <asmjit/axl/support_p.h>
#include <asmjit/core/arch_traits.h>
#include <asmjit/core/globals.h>
#include <asmjit/core/jit_allocator.h>
#include <asmjit/core/os_utils_p.h>
#include <asmjit/core/virt_mem.h>

#include <asmjit-testing/commons/random.h>

ASMJIT_BEGIN_NAMESPACE

namespace JitAllocatorUtils {
  static void fill_pattern_64(void* p_, uint64_t pattern, size_t size_in_bytes) noexcept {
    uint64_t* p = static_cast<uint64_t*>(p_);
    size_t n = size_in_bytes / 8u;

    for (size_t i = 0; i < n; i++) {
      p[i] = pattern;
    }
  }

  static bool verify_pattern_64(const void* p_, uint64_t pattern, size_t size_in_bytes) noexcept {
    const uint64_t* p = static_cast<const uint64_t*>(p_);
    size_t n = size_in_bytes / 8u;

    for (size_t i = 0; i < n; i++) {
      if (p[i] != pattern) {
        TEST_LOG("Pattern verification failed at 0x%p [%zu * 8]: value(0x%016llX) != expected(0x%016llX)",
          p,
          i,
          (unsigned long long)p[i],
          (unsigned long long)pattern);
        return false;
      }
    }

    return true;
  }
}

// Helper class to verify that JitAllocator doesn't return addresses that overlap.
class JitAllocatorWrapper {
public:
  // Address to a memory region of a given size.
  class Range {
  public:
    inline Range(uint8_t* addr, size_t size) noexcept
      : addr(addr),
        size(size) {}
    uint8_t* addr;
    size_t size;
  };

  // Based on JitAllocator::Block, serves our purpose well...
  class Record : public axl::ArenaTreeNodeT<Record>, public Range {
  public:
    //! Read/write address, in case this is a dual mapping.
    void* _rw;
    //! Describes a pattern used to fill the allocated memory.
    uint64_t pattern;

    inline Record(void* rx, void* rw, size_t size, uint64_t pattern)
      : axl::ArenaTreeNodeT<Record>(),
        Range(static_cast<uint8_t*>(rx), size),
        _rw(rw),
        pattern(pattern) {}

    inline void* rx() const noexcept { return addr; }
    inline void* rw() const noexcept { return _rw; }
  };

  struct RecordLessThan {
    inline bool operator()(const Record& a, const Record& b) const noexcept { return a.addr < b.addr; }
    inline bool operator()(const Record& a, const uint8_t* b) const noexcept { return a.addr + a.size <= b; }
    inline bool operator()(const uint8_t* a, const Record& b) const noexcept { return a < b.addr; }
  };

  axl::Arena _arena;
  axl::ArenaPool<Record> _record_pool;
  axl::ArenaTree<Record> _records;
  JitAllocator _allocator;
  TestUtils::Random _rng;

  explicit JitAllocatorWrapper(const JitAllocator::CreateParams* params) noexcept
    : _arena(1024u * 1024u),
      _allocator(params),
      _rng(0x123456789u) {}

  void _insert(void* rx_ptr, void* rw_ptr, size_t size) noexcept {
    uint8_t* ptr = static_cast<uint8_t*>(rx_ptr);
    uint8_t* end_ptr = ptr + size - 1u;

    Record* record;

    record = _records.get(ptr, RecordLessThan{});
    EXPECT_NULL(record)
      .message("Address [%p:%p] collides with a newly allocated [%p:%p]\n", record->addr, record->addr + record->size, ptr, ptr + size);

    record = _records.get(end_ptr, RecordLessThan{});
    EXPECT_NULL(record)
      .message("Address [%p:%p] collides with a newly allocated [%p:%p]\n", record->addr, record->addr + record->size, ptr, ptr + size);

    uint64_t pattern = _rng.next_uint64();
    record = new(axl::PlacementNew{_record_pool.alloc(_arena)}) Record(rx_ptr, rw_ptr, size, pattern);
    EXPECT_NOT_NULL(record);

    {
      VirtMem::ProtectJitReadWriteScope scope(rw_ptr, size);
      JitAllocatorUtils::fill_pattern_64(rw_ptr, pattern, size);
    }

    VirtMem::flush_instruction_cache(rx_ptr, size);
    EXPECT_TRUE(JitAllocatorUtils::verify_pattern_64(rx_ptr, pattern, size));

    _records.insert(record, RecordLessThan{});
  }

  void _remove(void* p) noexcept {
    Record* record = _records.get(static_cast<uint8_t*>(p), RecordLessThan{});
    EXPECT_NOT_NULL(record);

    EXPECT_TRUE(JitAllocatorUtils::verify_pattern_64(record->rx(), record->pattern, record->size));
    EXPECT_TRUE(JitAllocatorUtils::verify_pattern_64(record->rw(), record->pattern, record->size));

    _records.remove(record, RecordLessThan{});
    _record_pool.release(record);
  }

  void* alloc(size_t size) noexcept {
    JitAllocator::Span span;
    EXPECT_OK(_allocator.alloc(Out(span), size))
      .message("JitAllocator failed to allocate %zu bytes\n", size);

    _insert(span.rx(), span.rw(), size);
    return span.rx();
  }

  void release(void* p) noexcept {
    _remove(p);
    EXPECT_OK(_allocator.release(p))
      .message("JitAllocator failed to release '%p'\n", p);
  }

  void shrink(void* p, size_t new_size) noexcept {
    Record* record = _records.get(static_cast<uint8_t*>(p), RecordLessThan{});
    EXPECT_NOT_NULL(record);

    if (!new_size) {
      return release(p);
    }

    JitAllocator::Span span;
    EXPECT_OK(_allocator.query(Out(span), p));
    EXPECT_OK(_allocator.shrink(span, new_size))
      .message("JitAllocator failed to shrink %p to %zu bytes\n", p, new_size);

    record->size = new_size;
  }
};

static void JitAllocatorTest_shuffle(void** ptr_array, size_t count, TestUtils::Random& prng) noexcept {
  for (size_t i = 0; i < count; ++i) {
    axl::swap(ptr_array[i], ptr_array[size_t(prng.next_uint32() % count)]);
  }
}

static void JitAllocatorTest_usage(JitAllocator& allocator) noexcept {
  JitAllocator::Statistics stats = allocator.statistics();
  TEST_LOG("    Block Count       : %9llu [Blocks]"        , (unsigned long long)(stats.block_count()));
  TEST_LOG("    Reserved (VirtMem): %9llu [Bytes]"         , (unsigned long long)(stats.reserved_size()));
  TEST_LOG("    Used     (VirtMem): %9llu [Bytes] (%.1f%%)", (unsigned long long)(stats.used_size()), stats.used_ratio() * 100.0);
  TEST_LOG("    Overhead (HeapMem): %9llu [Bytes] (%.1f%%)", (unsigned long long)(stats.overhead_size()), stats.overhead_ratio() * 100.0);
}

template<typename T, size_t kPatternSize, bool Bit>
static void BitVectorRangeIterator_test_random(TestUtils::Random& rnd, size_t count) noexcept {
  for (size_t i = 0; i < count; i++) {
    T in[kPatternSize];
    T out[kPatternSize];

    for (size_t j = 0; j < kPatternSize; j++) {
      in[j] = T(uint64_t(rnd.next_uint32() & 0xFFu) * 0x0101010101010101);
      out[j] = Bit == 0 ? axl::bit_ones<T> : T(0);
    }

    {
      axl::BitVectorRangeIterator<T, Bit> it(in, kPatternSize);
      size_t range_start, range_end;
      while (it.next_range(Out(range_start), Out(range_end))) {
        if (Bit) {
          axl::bit_vector_fill(out, range_start, range_end - range_start);
        }
        else {
          axl::bit_vector_clear(out, range_start, range_end - range_start);
        }
      }
    }

    for (size_t j = 0; j < kPatternSize; j++) {
      EXPECT_EQ(in[j], out[j])
        .message("Invalid pattern detected at [%zu] (%llX != %llX)", j, (unsigned long long)in[j], (unsigned long long)out[j]);
    }
  }
}

TEST_CASE(core_jit_allocator_constructors) {
  JitAllocator allocator;
  allocator.reset(ResetPolicy::kSoft);
}

TEST_CASE(core_jit_allocator_alloc_relese) {
  size_t kCount = ZeroTest::test_runner()->cmd_line().has_arg("--quick") ? 20000 : 60000;

  struct TestInfo {
    const char* name;
    JitAllocatorOptions options;
    uint32_t block_size;
    uint32_t granularity;
  };

  using Opt = JitAllocatorOptions;

  VirtMem::HardenedRuntimeInfo hri = VirtMem::hardened_runtime_info();

  TestInfo test_info_table[] = {
    { "Default"                                    , Opt::kNone, 0, 0 },
    { "16MB blocks"                                , Opt::kNone, 16 * 1024 * 1024, 0 },
    { "256B granularity"                           , Opt::kNone, 0, 256 },
    { "kUseMultiplePools"                          , Opt::kUseMultiplePools, 0, 0 },
    { "kFillUnusedMemory"                          , Opt::kFillUnusedMemory, 0, 0 },
    { "kImmediateRelease"                          , Opt::kImmediateRelease, 0, 0 },
    { "kDisableInitialPadding"                     , Opt::kDisableInitialPadding, 0, 0 },
    { "kUseLargePages"                             , Opt::kUseLargePages, 0, 0 },
    { "kUseLargePages | kFillUnusedMemory"         , Opt::kUseLargePages | Opt::kFillUnusedMemory, 0, 0 },
    { "kUseLargePages | kAlignBlockSizeToLargePage", Opt::kUseLargePages | Opt::kAlignBlockSizeToLargePage, 0, 0 },
    { "kUseDualMapping"                            , Opt::kUseDualMapping , 0, 0 },
    { "kUseDualMapping | kFillUnusedMemory"        , Opt::kUseDualMapping | Opt::kFillUnusedMemory, 0, 0 }
  };

  TEST_LOG("BitVectorRangeIterator<uint32_t>");
  {
    TestUtils::Random rnd;
    BitVectorRangeIterator_test_random<uint32_t, 64, 0>(rnd, kCount);
  }

  TEST_LOG("BitVectorRangeIterator<uint64_t>");
  {
    TestUtils::Random rnd;
    BitVectorRangeIterator_test_random<uint64_t, 64, 0>(rnd, kCount);
  }

  for (const TestInfo& test_info : test_info_table) {
    // Don't try to allocate dual-mapping if dual mapping is not possible - it would fail the test.
    if (axl::test(test_info.options, JitAllocatorOptions::kUseDualMapping) &&
        !axl::test(hri.flags, VirtMem::HardenedRuntimeFlags::kDualMapping)) {
      continue;
    }

    TEST_LOG("JitAllocator(%s)", test_info.name);

    JitAllocator::CreateParams params {};
    params.options = test_info.options;
    params.block_size = test_info.block_size;
    params.granularity = test_info.granularity;

    size_t fixed_block_size = 256;

    JitAllocatorWrapper wrapper(&params);
    TestUtils::Random prng(100);

    size_t i;

    TEST_LOG("  Memory alloc/release test - %d allocations", kCount);

    void** ptr_array = (void**)::malloc(sizeof(void*) * size_t(kCount));
    EXPECT_NOT_NULL(ptr_array);

    // Random blocks tests...
    TEST_LOG("  Allocating random blocks...");
    for (i = 0; i < kCount; i++) {
      ptr_array[i] = wrapper.alloc((prng.next_uint32() % 1024) + 8);
    }
    JitAllocatorTest_usage(wrapper._allocator);

    TEST_LOG("  Releasing all allocated blocks from the beginning...");
    for (i = 0; i < kCount; i++) {
      wrapper.release(ptr_array[i]);
    }
    JitAllocatorTest_usage(wrapper._allocator);

    TEST_LOG("  Allocating random blocks again...", kCount);
    for (i = 0; i < kCount; i++) {
      ptr_array[i] = wrapper.alloc((prng.next_uint32() % 1024) + 8);
    }
    JitAllocatorTest_usage(wrapper._allocator);

    TEST_LOG("  Shuffling allocated blocks...");
    JitAllocatorTest_shuffle(ptr_array, unsigned(kCount), prng);

    TEST_LOG("  Releasing 50%% of allocated blocks...");
    for (i = 0; i < kCount / 2; i++) {
      wrapper.release(ptr_array[i]);
    }
    JitAllocatorTest_usage(wrapper._allocator);

    TEST_LOG("  Allocating 50%% more blocks again...");
    for (i = 0; i < kCount / 2; i++) {
      ptr_array[i] = wrapper.alloc((prng.next_uint32() % 1024) + 8);
    }
    JitAllocatorTest_usage(wrapper._allocator);

    TEST_LOG("  Releasing all allocated blocks from the end...");
    for (i = 0; i < kCount; i++) {
      wrapper.release(ptr_array[kCount - i - 1]);
    }
    JitAllocatorTest_usage(wrapper._allocator);

    // Fixed blocks tests...
    TEST_LOG("  Allocating %zuB blocks...", fixed_block_size);
    for (i = 0; i < kCount / 2; i++) {
      ptr_array[i] = wrapper.alloc(fixed_block_size);
    }
    JitAllocatorTest_usage(wrapper._allocator);

    TEST_LOG("  Shrinking each %zuB block to 1 byte", fixed_block_size);
    for (i = 0; i < kCount / 2; i++) {
      wrapper.shrink(ptr_array[i], 1);
    }
    JitAllocatorTest_usage(wrapper._allocator);

    TEST_LOG("  Allocating more 64B blocks...", 64);
    for (i = kCount / 2; i < kCount; i++) {
      ptr_array[i] = wrapper.alloc(64);
    }
    JitAllocatorTest_usage(wrapper._allocator);

    TEST_LOG("  Releasing all blocks from the beginning...");
    for (i = 0; i < kCount; i++) {
      wrapper.release(ptr_array[i]);
    }
    JitAllocatorTest_usage(wrapper._allocator);

    TEST_LOG("  Allocating %zuB blocks...", fixed_block_size);
    for (i = 0; i < kCount; i++) {
      ptr_array[i] = wrapper.alloc(fixed_block_size);
    }
    JitAllocatorTest_usage(wrapper._allocator);

    TEST_LOG("  Shuffling allocated blocks...");
    JitAllocatorTest_shuffle(ptr_array, unsigned(kCount), prng);

    TEST_LOG("  Releasing 50%% of allocated blocks...");
    for (i = 0; i < kCount / 2; i++) {
      wrapper.release(ptr_array[i]);
    }
    JitAllocatorTest_usage(wrapper._allocator);

    TEST_LOG("  Allocating 50%% more %zuB blocks again...", fixed_block_size);
    for (i = 0; i < kCount / 2; i++) {
      ptr_array[i] = wrapper.alloc(fixed_block_size);
    }
    JitAllocatorTest_usage(wrapper._allocator);

    TEST_LOG("  Releasing all allocated blocks from the end...");
    for (i = 0; i < kCount; i++) {
      wrapper.release(ptr_array[kCount - i - 1]);
    }
    JitAllocatorTest_usage(wrapper._allocator);

    ::free(ptr_array);
  }
}

TEST_CASE(core_jit_allocator_query) {
  JitAllocator allocator;
  size_t allocated_size = 100;

  JitAllocator::Span allocated_span;
  EXPECT_OK(allocator.alloc(Out(allocated_span), allocated_size));
  EXPECT_NOT_NULL(allocated_span.rx());
  EXPECT_GE(allocated_span.size(), allocated_size);

  JitAllocator::Span queried_span;
  EXPECT_OK(allocator.query(Out(queried_span), allocated_span.rx()));
  EXPECT_EQ(allocated_span.rx(), queried_span.rx());
  EXPECT_EQ(allocated_span.rw(), queried_span.rw());
  EXPECT_EQ(allocated_span.size(), queried_span.size());
}

TEST_CASE(core_jit_allocator_incremental_release) {
  // A block emptied by releasing its spans in reverse order must be reported as empty,
  // otherwise `kImmediateRelease` never releases it.
  {
    JitAllocator::CreateParams params {};
    params.options = JitAllocatorOptions::kImmediateRelease;

    JitAllocator allocator(&params);
    JitAllocator::Span a;
    JitAllocator::Span b;

    EXPECT_OK(allocator.alloc(Out(a), 1024));
    EXPECT_OK(allocator.alloc(Out(b), 1024));
    EXPECT_OK(allocator.release(b.rx()));
    EXPECT_OK(allocator.release(a.rx()));
    EXPECT_EQ(allocator.statistics().block_count(), 0u);
  }

  // Releasing the last span of a full block must keep the free tail searchable after a release
  // in the middle ends the incremental mode, otherwise the tail is lost until the block is empty.
  {
    JitAllocator::CreateParams params {};
    params.options = JitAllocatorOptions::kDisableInitialPadding;

    JitAllocator allocator(&params);
    JitAllocator::Span head;
    JitAllocator::Span body;
    JitAllocator::Span tail;

    EXPECT_OK(allocator.alloc(Out(head), 1024));
    size_t block_size = allocator.statistics().reserved_size();

    EXPECT_OK(allocator.alloc(Out(body), block_size - 2048));
    EXPECT_OK(allocator.alloc(Out(tail), 1024));
    EXPECT_EQ(allocator.statistics().used_size(), block_size);

    EXPECT_OK(allocator.release(tail.rx()));
    EXPECT_OK(allocator.release(head.rx()));

    JitAllocator::Span x;
    JitAllocator::Span y;

    EXPECT_OK(allocator.alloc(Out(x), 1024));
    EXPECT_OK(allocator.alloc(Out(y), 1024));
    EXPECT_EQ(allocator.statistics().block_count(), 1u);
  }
}

static bool JitAllocatorTest_in_range(const JitAllocator::Span& span, VirtMem::AddressRange range) noexcept {
  uintptr_t rx = uintptr_t(span.rx());
  return rx >= range.start && rx < range.end && range.end - rx >= span.size();
}

[[maybe_unused]]
static bool JitAllocatorTest_within_reach(const JitAllocator::Span& span, uintptr_t addr) noexcept {
  uintptr_t rx = uintptr_t(span.rx());
  return axl::is_int_n<32>(int64_t(addr - rx)) && axl::is_int_n<32>(int64_t(addr - (rx + span.size())));
}

TEST_CASE(core_jit_allocator_address_range) {
  using Opt = JitAllocatorOptions;

  constexpr size_t kRangeSize = 256u * 1024u * 1024u;
  constexpr size_t kSpanSize = 64u * 1024u;
  constexpr size_t kSpanCount = 64u;

  // Each allocator places its first block at a random address within the ranges computed from `near_address`.
  constexpr size_t kNearPlacementCount = 16u;

  // Find free address space by mapping and unmapping it (the test runner is single-threaded).
  void* hole;
  EXPECT_OK(VirtMem::alloc(&hole, kRangeSize, VirtMem::MemoryFlags::kAccessRW));
  EXPECT_OK(VirtMem::release(hole, kRangeSize));

  VirtMem::AddressRange range { uintptr_t(hole), uintptr_t(hole) + kRangeSize };

  VirtMem::HardenedRuntimeInfo hri = VirtMem::hardened_runtime_info();
  size_t page_size = VirtMem::info().page_size;

  static constexpr Opt range_options[] = { Opt::kNone, Opt::kUseDualMapping };
  for (Opt options : range_options) {
    if (axl::test(options, Opt::kUseDualMapping) && !hri.has_flag(VirtMem::HardenedRuntimeFlags::kDualMapping)) {
      continue;
    }

    TEST_LOG("JitAllocator(address range, %s)", options == Opt::kNone ? "kNone" : "kUseDualMapping");

    JitAllocator::CreateParams params {};
    params.options = options | Opt::kStrictAddressRange;
    params.address_range = range;

    JitAllocator allocator(&params);
    JitAllocator::Span spans[kSpanCount];

    for (size_t i = 0; i < kSpanCount; i++) {
      EXPECT_OK(allocator.alloc(Out(spans[i]), kSpanSize));
      EXPECT_TRUE(JitAllocatorTest_in_range(spans[i], range))
        .message("Span %p [%zu] is outside of [%p, %p)", spans[i].rx(), i, (void*)range.start, (void*)range.end);
    }

    EXPECT_GT(allocator.statistics().block_count(), 1u);

    // The first span must not be at the start of its block (UBSAN reads 8 bytes before a function).
    EXPECT_EQ(uintptr_t(spans[0].rx()) & (page_size - 1u), size_t(allocator.granularity()));

    for (size_t i = 0; i < kSpanCount; i++) {
      EXPECT_OK(allocator.release(spans[i].rx()));
    }
  }

  TEST_LOG("JitAllocator(address range that is taken)");
  {
    void* taken;
    EXPECT_OK(VirtMem::alloc(&taken, kSpanSize * 16u, VirtMem::MemoryFlags::kAccessRW));

    JitAllocator::CreateParams params {};
    params.address_range = VirtMem::AddressRange{uintptr_t(taken), uintptr_t(taken) + kSpanSize * 16u};

    // Non-strict allocator places the block anywhere.
    {
      JitAllocator allocator(&params);
      JitAllocator::Span span;
      EXPECT_OK(allocator.alloc(Out(span), 64));
      EXPECT_FALSE(JitAllocatorTest_in_range(span, params.address_range));
      EXPECT_OK(allocator.release(span.rx()));
    }

    // Strict allocator fails.
    params.options = Opt::kStrictAddressRange;
    {
      JitAllocator allocator(&params);
      JitAllocator::Span span;
      EXPECT_EQ(allocator.alloc(Out(span), 64), Error::kOutOfMemory);
      EXPECT_NULL(span.rx());
      EXPECT_EQ(allocator.statistics().block_count(), 0u);
    }

    EXPECT_OK(VirtMem::release(taken, kSpanSize * 16u));
  }

  TEST_LOG("JitAllocator(near_address, kStrictAddressRange)");
  for (size_t i = 0; i < kNearPlacementCount; i++) {
    // This function is in a module, all of which must be within reach of a 32-bit displacement.
    uintptr_t fn = uintptr_t(&JitAllocatorTest_in_range);

    JitAllocator::CreateParams params {};
    params.options = Opt::kStrictAddressRange;
    params.near_address = fn;

    JitAllocator allocator(&params);
    JitAllocator::Span span;
    Error err = allocator.alloc(Out(span), kSpanSize);

#if ASMJIT_TARGET_ARCH_X86 == 64
    EXPECT_OK(err);
    EXPECT_TRUE(JitAllocatorTest_within_reach(span, fn))
      .message("Span %p is out of reach of %p", span.rx(), (void*)fn);
    EXPECT_OK(allocator.release(span.rx()));
#else
    EXPECT_EQ(err, Error::kFeatureNotEnabled);
#endif
  }

#if ASMJIT_TARGET_ARCH_X86 == 64 && !defined(__APPLE__)
  TEST_LOG("JitAllocator(near_address without space below, kStrictAddressRange)");
  for (size_t i = 0; i < kNearPlacementCount; i++) {
    // There is no space below this address (it's the lowest address considered), so the block must be placed above.
    uintptr_t addr = 0x100000u;

    JitAllocator::CreateParams params {};
    params.options = Opt::kStrictAddressRange;
    params.near_address = addr;

    JitAllocator allocator(&params);
    JitAllocator::Span span;
    EXPECT_OK(allocator.alloc(Out(span), kSpanSize));
    EXPECT_GT(uintptr_t(span.rx()), addr);
    EXPECT_TRUE(JitAllocatorTest_within_reach(span, addr))
      .message("Span %p is out of reach of %p", span.rx(), (void*)addr);
    EXPECT_OK(allocator.release(span.rx()));
  }
#endif
}

ASMJIT_END_NAMESPACE

#endif // ASMJIT_TEST && !ASMJIT_NO_JIT
