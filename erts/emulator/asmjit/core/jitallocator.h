// This file is part of AsmJit project <https://asmjit.com>
//
// See asmjit.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#ifndef ASMJIT_CORE_JITALLOCATOR_H_INCLUDED
#define ASMJIT_CORE_JITALLOCATOR_H_INCLUDED

#include "../core/api-config.h"
#ifndef ASMJIT_NO_JIT

#include "../core/globals.h"
#include "../core/virtmem.h"

ASMJIT_BEGIN_NAMESPACE

//! \addtogroup asmjit_virtual_memory
//! \{

//! Options used by \ref JitAllocator.
enum class JitAllocatorOptions : uint32_t {
  //! No options.
  kNone = 0,

  //! Enables the use of an anonymous memory-mapped memory that is mapped into two buffers having a different pointer.
  //! The first buffer has read and execute permissions and the second buffer has read+write permissions.
  //!
  //! See \ref VirtMem::allocDualMapping() for more details about this feature.
  kUseDualMapping = 0x00000001u,

  //! Enables the use of multiple pools with increasing granularity instead of a single pool. This flag would enable
  //! 3 internal pools in total having 64, 128, and 256 bytes granularity.
  //!
  //! This feature is only recommended for users that generate a lot of code and would like to minimize the overhead
  //! of `JitAllocator` itself by having blocks of different allocation granularities. Using this feature only for
  //! few allocations won't pay off as the allocator may need to create more blocks initially before it can take the
  //! advantage of variable block granularity.
  kUseMultiplePools = 0x00000002u,

  //! Always fill reserved memory by a fill-pattern.
  //!
  //! Causes a new block to be cleared by the fill pattern and freshly released memory to be cleared before making
  //! it ready for another use.
  kFillUnusedMemory = 0x00000004u,

  //! When this flag is set the allocator would immediately release unused blocks during `release()` or `reset()`.
  //! When this flag is not set the allocator would keep one empty block in each pool to prevent excessive virtual
  //! memory allocations and deallocations in border cases, which involve constantly allocating and deallocating a
  //! single block caused by repetitive calling `alloc()` and `release()` when the allocator has either no blocks
  //! or have all blocks fully occupied.
  kImmediateRelease = 0x00000008u,

  //! Use a custom fill pattern, must be combined with `kFlagFillUnusedMemory`.
  kCustomFillPattern = 0x10000000u
};
ASMJIT_DEFINE_ENUM_FLAGS(JitAllocatorOptions)

//! A simple implementation of memory manager that uses `asmjit::VirtMem`
//! functions to manage virtual memory for JIT compiled code.
//!
//! Implementation notes:
//!
//! - Granularity of allocated blocks is different than granularity for a typical C malloc. In addition, the allocator
//!   can use several memory pools having a different granularity to minimize the maintenance overhead. Multiple pools
//!   feature requires `kFlagUseMultiplePools` flag to be set.
//!
//! - The allocator doesn't store any information in executable memory, instead, the implementation uses two
//!   bit-vectors to manage allocated memory of each allocator-block. The first bit-vector called 'used' is used to
//!   track used memory (where each bit represents memory size defined by granularity) and the second bit vector called
//!   'stop' is used as a sentinel to mark where the allocated area ends.
//!
//! - Internally, the allocator also uses RB tree to keep track of all blocks across all pools. Each inserted block is
//!   added to the tree so it can be matched fast during `release()` and `shrink()`.
class JitAllocator {
public:
  ASMJIT_NONCOPYABLE(JitAllocator)

  struct Impl {
    //! Allocator options.
    JitAllocatorOptions options;
    //! Base block size (0 if the allocator is not initialized).
    uint32_t blockSize;
    //! Base granularity (0 if the allocator is not initialized).
    uint32_t granularity;
    //! A pattern that is used to fill unused memory if secure mode is enabled.
    uint32_t fillPattern;
  };

  //! Allocator implementation (private).
  Impl* _impl;

  //! \name Construction & Destruction
  //! \{

  //! Parameters that can be passed to `JitAllocator` constructor.
  //!
  //! Use it like this:
  //!
  //! ```
  //! // Zero initialize (zero means the default value) and change what you need.
  //! JitAllocator::CreateParams params {};
  //! params.blockSize = 1024 * 1024;
  //!
  //! // Create the allocator.
  //! JitAllocator allocator(&params);
  //! ```
  struct CreateParams {
    //! Allocator options.
    //!
    //! No options are used by default.
    JitAllocatorOptions options = JitAllocatorOptions::kNone;

    //! Base size of a single block in bytes (default 64kB).
    //!
    //! \remarks Block size must be equal to or greater than page size and must be power of 2. If the input is not
    //! valid then the default block size will be used instead.
    uint32_t blockSize = 0;

    //! Base granularity (and also natural alignment) of allocations in bytes (default 64).
    //!
    //! Since the `JitAllocator` uses bit-arrays to mark used memory the granularity also specifies how many bytes
    //! correspond to a single bit in such bit-array. Higher granularity means more waste of virtual memory (as it
    //! increases the natural alignment), but smaller bit-arrays as less bits would be required per a single block.
    uint32_t granularity = 0;

    //! Patter to use to fill unused memory.
    //!
    //! Only used if \ref JitAllocatorOptions::kCustomFillPattern is set.
    uint32_t fillPattern = 0;

    // Reset the content of `CreateParams`.
    inline void reset() noexcept { memset(this, 0, sizeof(*this)); }
  };

  //! Creates a `JitAllocator` instance.
  ASMJIT_API explicit JitAllocator(const CreateParams* params = nullptr) noexcept;
  //! Destroys the `JitAllocator` instance and release all blocks held.
  ASMJIT_API ~JitAllocator() noexcept;

  inline bool isInitialized() const noexcept { return _impl->blockSize == 0; }

  //! Free all allocated memory - makes all pointers returned by `alloc()` invalid.
  //!
  //! \remarks This function is not thread-safe as it's designed to be used when nobody else is using allocator.
  //! The reason is that there is no point of calling `reset()` when the allocator is still in use.
  ASMJIT_API void reset(ResetPolicy resetPolicy = ResetPolicy::kSoft) noexcept;

  //! \}

  //! \name Accessors
  //! \{

  //! Returns allocator options, see `Flags`.
  inline JitAllocatorOptions options() const noexcept { return _impl->options; }
  //! Tests whether the allocator has the given `option` set.
  inline bool hasOption(JitAllocatorOptions option) const noexcept { return uint32_t(_impl->options & option) != 0; }

  //! Returns a base block size (a minimum size of block that the allocator would allocate).
  inline uint32_t blockSize() const noexcept { return _impl->blockSize; }
  //! Returns granularity of the allocator.
  inline uint32_t granularity() const noexcept { return _impl->granularity; }
  //! Returns pattern that is used to fill unused memory if `kFlagUseFillPattern` is set.
  inline uint32_t fillPattern() const noexcept { return _impl->fillPattern; }

  //! \}

  //! \name Alloc & Release
  //! \{

  //! Allocates a new memory block of the requested `size`.
  //!
  //! When the function is successful it stores two pointers in `rxPtrOut` and `rwPtrOut`. The pointers will be
  //! different only if `kOptionUseDualMapping` was used to setup the allocator (in that case the `rxPtrOut` would
  //! point to a Read+Execute region and `rwPtrOut` would point to a Read+Write region of the same memory-mapped block.
  ASMJIT_API Error alloc(void** rxPtrOut, void** rwPtrOut, size_t size) noexcept;

  //! Releases a memory block returned by `alloc()`.
  //!
  //! \remarks This function is thread-safe.
  ASMJIT_API Error release(void* rxPtr) noexcept;

  //! Frees extra memory allocated with `rxPtr` by shrinking it to the given `newSize`.
  //!
  //! \remarks This function is thread-safe.
  ASMJIT_API Error shrink(void* rxPtr, size_t newSize) noexcept;

  //! Queries information about an allocated memory block that contains the given `rxPtr`.
  //!
  //! The function returns `kErrorOk` when `rxPtr` is matched and fills `rxPtrOut`, `rwPtrOut`, and `sizeOut` output
  //! arguments. The returned `rxPtrOut` and `rwPtrOut` pointers point to the beginning of the block, and `sizeOut`
  //! describes the total amount of bytes this allocation uses - `sizeOut` will always be aligned to the allocation
  //! granularity, so for example if an allocation was 1 byte and the size granularity is 64, the returned `sizeOut`
  //! will be 64 bytes, because that's what the allocator sees.
  ASMJIT_API Error query(void* rxPtr, void** rxPtrOut, void** rwPtrOut, size_t* sizeOut) const noexcept;

  //! \}

  //! \name Statistics
  //! \{

  //! Statistics about `JitAllocator`.
  struct Statistics {
    //! Number of blocks `JitAllocator` maintains.
    size_t _blockCount;
    //! Number of active allocations.
    size_t _allocationCount;
    //! How many bytes are currently used / allocated.
    size_t _usedSize;
    //! How many bytes are currently reserved by the allocator.
    size_t _reservedSize;
    //! Allocation overhead (in bytes) required to maintain all blocks.
    size_t _overheadSize;

    inline void reset() noexcept {
      _blockCount = 0;
      _usedSize = 0;
      _reservedSize = 0;
      _overheadSize = 0;
    }

    //! Returns count of blocks managed by `JitAllocator` at the moment.
    inline size_t blockCount() const noexcept { return _blockCount; }
    //! Returns the number of active allocations.
    inline size_t allocationCount() const noexcept { return _allocationCount; }

    //! Returns how many bytes are currently used.
    inline size_t usedSize() const noexcept { return _usedSize; }
    //! Returns the number of bytes unused by the allocator at the moment.
    inline size_t unusedSize() const noexcept { return _reservedSize - _usedSize; }
    //! Returns the total number of bytes bytes reserved by the allocator (sum of sizes of all blocks).
    inline size_t reservedSize() const noexcept { return _reservedSize; }
    //! Returns the number of bytes the allocator needs to manage the allocated memory.
    inline size_t overheadSize() const noexcept { return _overheadSize; }

    inline double usedSizeAsPercent() const noexcept {
      return (double(usedSize()) / (double(reservedSize()) + 1e-16)) * 100.0;
    }

    inline double unusedSizeAsPercent() const noexcept {
      return (double(unusedSize()) / (double(reservedSize()) + 1e-16)) * 100.0;
    }

    inline double overheadSizeAsPercent() const noexcept {
      return (double(overheadSize()) / (double(reservedSize()) + 1e-16)) * 100.0;
    }
  };

  //! Returns JIT allocator statistics.
  //!
  //! \remarks This function is thread-safe.
  ASMJIT_API Statistics statistics() const noexcept;

  //! \}
};

//! \}

ASMJIT_END_NAMESPACE

#endif
#endif
