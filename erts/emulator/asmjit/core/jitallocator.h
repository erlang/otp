// This file is part of AsmJit project <https://asmjit.com>
//
// See asmjit.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#ifndef ASMJIT_CORE_JITALLOCATOR_H_INCLUDED
#define ASMJIT_CORE_JITALLOCATOR_H_INCLUDED

#include "../core/api-config.h"
#ifndef ASMJIT_NO_JIT

#include "../core/globals.h"
#include "../core/support.h"
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
  //!
  //! \remarks Dual mapping would be automatically turned on by \ref JitAllocator in case of hardened runtime that
  //! enforces `W^X` policy, so specifying this flag is essentially forcing to use dual mapped pages even when RWX
  //! pages can be allocated and dual mapping is not necessary.
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

  //! This flag enables placing functions (or allocating memory) at the very beginning of each memory mapped region.
  //!
  //! Initially, this was the default behavior. However, LLVM developers working on undefined behavior sanitizer
  //! (UBSAN) decided that they want to store metadata before each function and to access such metadata before an
  //! indirect function call. This means that the instrumented code always reads from `[fnPtr - 8]` to decode whether
  //! the function has his metadata present. However, reading 8 bytes below a function means that if a function is
  //! placed at the very beginning of a memory mapped region, it could try to read bytes that are inaccessible. And
  //! since AsmJit can be compiled as a shared library and used by applications instrumented by UBSAN, it's not
  //! possible to conditionally compile the support only when necessary.
  //!
  //! \remarks This flag controls a workaround to make it possible to use LLVM UBSAN with AsmJit's \ref JitAllocator.
  //! There is no undefined behavior even when `kDisableInitialPadding` is used, however, that doesn't really matter
  //! as LLVM's UBSAN introduces one, and according to LLVM developers it's a "trade-off". This flag is safe to use
  //! when the code is not instrumented with LLVM's UBSAN.
  kDisableInitialPadding = 0x00000010u,

  //! Enables the use of large pages, if they are supported and the process can actually allocate them.
  //!
  //! \remarks This flag is a hint - if large pages can be allocated, JitAllocator would try to allocate them.
  //! However, if the allocation fails, it will still try to fallback to use regular pages as \ref JitAllocator
  //! is designed to minimize allocation failures, so a regular page is better than no page at all. Also, if a
  //! block \ref JitAllocator wants to allocate is too small to consume a whole large page, regular page(s) will
  //! be allocated as well.
  kUseLargePages = 0x00000020u,

  //! Forces \ref JitAllocator to always align block size to be at least as big as a large page, if large pages are
  //! enabled. This option does nothing if large pages are disabled.
  //!
  //! \remarks If \ref kUseLargePages option is used, the allocator would prefer large pages only when allocating a
  //! block that has a sufficient size. Usually the allocator first allocates smaller block and when more requests
  //! come it will start increasing the block size of next allocations. This option makes it sure that even the first
  //! allocation would be the same as a minimum large page when large pages are enabled and can be allocated.
  kAlignBlockSizeToLargePage = 0x00000040u,

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

  //! Visible \ref JitAllocator implementation data.
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

  //! \name Members
  //! \{

  //! Allocator implementation (private).
  Impl* _impl;

  //! \}

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
    ASMJIT_INLINE_NODEBUG void reset() noexcept { *this = CreateParams{}; }
  };

  //! Creates a `JitAllocator` instance.
  ASMJIT_API explicit JitAllocator(const CreateParams* params = nullptr) noexcept;
  //! Destroys the `JitAllocator` instance and release all blocks held.
  ASMJIT_API ~JitAllocator() noexcept;

  ASMJIT_INLINE_NODEBUG bool isInitialized() const noexcept { return _impl->blockSize == 0; }

  //! Free all allocated memory - makes all pointers returned by `alloc()` invalid.
  //!
  //! \remarks This function is not thread-safe as it's designed to be used when nobody else is using allocator.
  //! The reason is that there is no point of calling `reset()` when the allocator is still in use.
  ASMJIT_API void reset(ResetPolicy resetPolicy = ResetPolicy::kSoft) noexcept;

  //! \}

  //! \name Accessors
  //! \{

  //! Returns allocator options, see `Flags`.
  ASMJIT_INLINE_NODEBUG JitAllocatorOptions options() const noexcept { return _impl->options; }
  //! Tests whether the allocator has the given `option` set.
  ASMJIT_INLINE_NODEBUG bool hasOption(JitAllocatorOptions option) const noexcept { return uint32_t(_impl->options & option) != 0; }

  //! Returns a base block size (a minimum size of block that the allocator would allocate).
  ASMJIT_INLINE_NODEBUG uint32_t blockSize() const noexcept { return _impl->blockSize; }
  //! Returns granularity of the allocator.
  ASMJIT_INLINE_NODEBUG uint32_t granularity() const noexcept { return _impl->granularity; }
  //! Returns pattern that is used to fill unused memory if `kFlagUseFillPattern` is set.
  ASMJIT_INLINE_NODEBUG uint32_t fillPattern() const noexcept { return _impl->fillPattern; }

  //! \}

  //! \name Alloc & Release
  //! \{

  //! A memory reference returned by \ref JitAllocator::alloc().
  //!
  //! Span contains everything needed to actually write new code to the memory chunk it references.
  class Span {
  public:
    //! \name Constants
    //! \{

    //! Span flags
    enum class Flags : uint32_t {
      //! No flags.
      kNone = 0u,

      //! The process has never executed the region of the span.
      //!
      //! If this flag is set on a \ref Span it would mean that the allocator can avoid flushing
      //! instruction cache after a code has been written to it.
      kInstructionCacheClean = 0x00000001u
    };

    //! \}

    //! \name Members
    //! \{

    //! Address of memory that has Read and Execute permissions.
    void* _rx = nullptr;

    //! Address of memory that has Read and Write permissions.
    void* _rw = nullptr;

    //! Size of the span in bytes (rounded up to the allocation granularity).
    size_t _size = 0;

    //! Pointer that references a memory block maintained by \ref JitAllocator.
    //!
    //! This pointer is considered private and should never be used nor inspected outside of AsmJit.
    void* _block = nullptr;

    //! Span flags.
    Flags _flags = Flags::kNone;

    //! Reserved for future use.
    uint32_t _reserved = 0;

    //! \}

    //! \name Accessors
    //! \{

    //! Returns a pointer having Read & Execute permissions (references executable memory).
    //!
    //! This pointer is never NULL if the allocation succeeded, it points to an executable memory.
    ASMJIT_INLINE_NODEBUG void* rx() const noexcept { return _rx; }

    //! Returns a pointer having Read & Write permissions (references writable memory).
    //!
    //! Depending on the type of the allocation strategy this could either be:
    //!
    //!   - the same address as returned by `rx()` if the allocator uses RWX mapping (pages have all of Read, Write,
    //!     and Execute permissions) or MAP_JIT, which requires either \ref VirtMem::ProtectJitReadWriteScope or to
    //!     call \ref VirtMem::protectJitMemory() manually.
    //!   - a valid pointer, but not the same as `rx` - this would be valid if dual mapping is used.
    //!   - NULL pointer, in case that the allocation strategy doesn't use RWX, MAP_JIT, or dual mapping. In this
    //!     case only \ref JitAllocator can copy new code into the executable memory referenced by \ref Span.
    //!
    //! \note If `rw()` returns a non-null pointer it's important to use either VirtMem::protectJitMemory() or
    //! \ref VirtMem::ProtectJitReadWriteScope to guard the write, because in case of `MAP_JIT` it would temporarily
    //! switch the permissions of the pointer to RW (that's per thread permissions).
    //!
    //! If \ref VirtMem::ProtectJitReadWriteScope is not used it's important to clear the instruction cache via
    //! \ref VirtMem::flushInstructionCache() after the write is done.
    ASMJIT_INLINE_NODEBUG void* rw() const noexcept { return _rw; }

    //! Returns size of this span, aligned to the allocator granularity.
    ASMJIT_INLINE_NODEBUG size_t size() const noexcept { return _size; }

    //! Returns span flags.
    ASMJIT_INLINE_NODEBUG Flags flags() const noexcept { return _flags; }

    //! Shrinks this span to `newSize`.
    //!
    //! \note This is the only function that is able to change the size of a span, and it's only use case is to
    //! shrink the span size during \ref JitAllocator::write(). When the writer detects that the span size shrunk,
    //! it will automatically shrink the memory used by the span, and propagate the new aligned size to the caller.
    ASMJIT_INLINE_NODEBUG void shrink(size_t newSize) noexcept { _size = Support::min(_size, newSize); }

    //! Returns whether \ref rw() returns a non-null pointer.
    ASMJIT_INLINE_NODEBUG bool isDirectlyWritable() const noexcept { return _rw != nullptr; }

    //! \}
  };

  //! Allocates a new memory span of the requested `size`.
  ASMJIT_API Error alloc(Span& out, size_t size) noexcept;

  //! Releases a memory block returned by `alloc()`.
  //!
  //! \remarks This function is thread-safe.
  ASMJIT_API Error release(void* rx) noexcept;

  //! Frees extra memory allocated with `rx` by shrinking it to the given `newSize`.
  //!
  //! \remarks This function is thread-safe.
  ASMJIT_API Error shrink(Span& span, size_t newSize) noexcept;

  //! Queries information about an allocated memory block that contains the given `rx`, and writes it to `out`.
  //!
  //! If the pointer is matched, the function returns `kErrorOk` and fills `out` with the corresponding span.
  ASMJIT_API Error query(Span& out, void* rx) const noexcept;

#if !defined(ASMJIT_NO_DEPRECATED)
  //! Allocates a new memory block of the requested `size`.
  ASMJIT_DEPRECATED("Use alloc(Span& out, size_t size) instead")
  ASMJIT_FORCE_INLINE Error alloc(void** rxPtrOut, void** rwPtrOut, size_t size) noexcept {
    Span span;
    Error err = alloc(span, size);
    *rwPtrOut = span.rw();
    *rxPtrOut = span.rx();
    return err;
  }

  ASMJIT_DEPRECATED("Use shrink(Span& span, size_t newSize) instead")
  ASMJIT_FORCE_INLINE Error shrink(void* rxPtr, size_t newSize) noexcept {
    Span span;
    ASMJIT_PROPAGATE(query(span, rxPtr));
    return (span.size() > newSize) ? shrink(span, newSize) : Error(kErrorOk);
  }

  ASMJIT_DEPRECATED("Use query(Span& out, void* rx) instead")
  ASMJIT_FORCE_INLINE Error query(void* rxPtr, void** rxPtrOut, void** rwPtrOut, size_t* sizeOut) const noexcept {
    Span span;
    Error err = query(span, rxPtr);
    *rxPtrOut = span.rx();
    *rwPtrOut = span.rw();
    *sizeOut = span.size();
    return err;
  }
#endif

  //! \}

  //! \name Write Operations
  //! \{

  typedef Error (ASMJIT_CDECL* WriteFunc)(Span& span, void* userData) ASMJIT_NOEXCEPT_TYPE;

  ASMJIT_API Error write(
    Span& span,
    size_t offset,
    const void* src,
    size_t size,
    VirtMem::CachePolicy policy = VirtMem::CachePolicy::kDefault) noexcept;

  ASMJIT_API Error write(
    Span& span,
    WriteFunc writeFunc,
    void* userData,
    VirtMem::CachePolicy policy = VirtMem::CachePolicy::kDefault) noexcept;

  template<class Lambda>
  ASMJIT_FORCE_INLINE Error write(
    Span& span,
    Lambda&& lambdaFunc,
    VirtMem::CachePolicy policy = VirtMem::CachePolicy::kDefault) noexcept {

    WriteFunc wrapperFunc = [](Span& span, void* userData) noexcept -> Error {
      Lambda& lambdaFunc = *static_cast<Lambda*>(userData);
      return lambdaFunc(span);
    };
    return write(span, wrapperFunc, (void*)(&lambdaFunc), policy);
  }

  //! \}

  //! \name Write Operations with Scope
  //! \{

  //! \cond INTERNAL

  //! Write scope data.
  //!
  //! This is mostly for internal purposes, please use \ref WriteScope instead.
  struct WriteScopeData {
    //! \name Members
    //! \{

    //! Link to the allocator.
    JitAllocator* _allocator;
    //! Cache policy passed to \ref JitAllocator::beginWriteScope().
    VirtMem::CachePolicy _policy;
    //! Internal flags used by the implementation.
    uint32_t _flags;
    //! Internal data used by the implementation.
    size_t _data[64];

    //! \}
  };

  //! Begins a write `scope`.
  //!
  //! This is mostly for internal purposes, please use \ref WriteScope constructor instead.
  ASMJIT_API Error beginWriteScope(WriteScopeData& scope, VirtMem::CachePolicy policy = VirtMem::CachePolicy::kDefault) noexcept;

  //! Ends a write `scope`.
  //!
  //! This is mostly for internal purposes, please use \ref WriteScope destructor instead.
  ASMJIT_API Error endWriteScope(WriteScopeData& scope) noexcept;

  //! Flushes accumulated changes in a write `scope`.
  //!
  //! This is mostly for internal purposes, please use \ref WriteScope destructor or \ref WriteScope::flush() instead.
  ASMJIT_API Error flushWriteScope(WriteScopeData& scope) noexcept;

  //! Alternative to `JitAllocator::write(span, offset, src, size)`, but under a write `scope`.
  //!
  //! This is mostly for internal purposes, please use \ref WriteScope::write() instead.
  ASMJIT_API Error scopedWrite(WriteScopeData& scope, Span& span, size_t offset, const void* src, size_t size) noexcept;

  //! Alternative to `JitAllocator::write(span, writeFunc, userData)`, but under a write `scope`.
  //!
  //! This is mostly for internal purposes, please use \ref WriteScope::write() instead.
  ASMJIT_API Error scopedWrite(WriteScopeData& scope, Span& span, WriteFunc writeFunc, void* userData) noexcept;

  //! Alternative to `JitAllocator::write(span, [lambda])`, but under a write `scope`.
  //!
  //! This is mostly for internal purposes, please use \ref WriteScope::write() instead.
  template<class Lambda>
  inline Error scopedWrite(WriteScopeData& scope, Span& span, Lambda&& lambdaFunc) noexcept {
    WriteFunc wrapperFunc = [](Span& span, void* userData) noexcept -> Error {
      Lambda& lambdaFunc = *static_cast<Lambda*>(userData);
      return lambdaFunc(span);
    };
    return scopedWrite(scope, span, wrapperFunc, (void*)(&lambdaFunc));
  }

  //! \endcond

  //! Write scope can be used to create a single scope that is optimized for writing multiple spans.
  class WriteScope : public WriteScopeData {
  public:
    ASMJIT_NONCOPYABLE(WriteScope)

    //! \name Construction & Destruction
    //! \{

    // Begins a write scope.
    inline explicit WriteScope(JitAllocator* allocator, VirtMem::CachePolicy policy = VirtMem::CachePolicy::kDefault) noexcept {
      allocator->beginWriteScope(*this, policy);
    }

    // Ends a write scope.
    inline ~WriteScope() noexcept {
      if (_allocator)
        _allocator->endWriteScope(*this);
    }

    //! \}

    //! \name Accessors
    //! \{

    ASMJIT_INLINE_NODEBUG JitAllocator* allocator() const noexcept { return _allocator; }
    ASMJIT_INLINE_NODEBUG VirtMem::CachePolicy policy() const noexcept { return _policy; }

    //! \}

    //! \name Operations
    //! \{

    //! Similar to `JitAllocator::write(span, offset, src, size)`, but under a write scope.
    ASMJIT_INLINE_NODEBUG Error write(Span& span, size_t offset, const void* src, size_t size) noexcept {
      return _allocator->scopedWrite(*this, span, offset, src, size);
    }

    //! Similar to `JitAllocator::write(span, writeFunc, userData)`, but under a write scope.
    ASMJIT_INLINE_NODEBUG Error write(Span& span, WriteFunc writeFunc, void* userData) noexcept {
      return _allocator->scopedWrite(*this, span, writeFunc, userData);
    }

    //! Similar to `JitAllocator::write(span, <lambda>)`, but under a write scope.
    template<class Lambda>
    ASMJIT_INLINE_NODEBUG Error write(Span& span, Lambda&& lambdaFunc) noexcept {
      return _allocator->scopedWrite(*this, span, lambdaFunc);
    }

    //! Flushes accumulated changes in this write scope.
    ASMJIT_INLINE_NODEBUG Error flush() noexcept {
      return _allocator->flushWriteScope(*this);
    }

    //! \}
  };

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

    //! Resets the statistics to all zeros.
    ASMJIT_INLINE_NODEBUG void reset() noexcept { *this = Statistics{}; }

    //! Returns count of blocks managed by `JitAllocator` at the moment.
    ASMJIT_INLINE_NODEBUG size_t blockCount() const noexcept { return _blockCount; }
    //! Returns the number of active allocations.
    ASMJIT_INLINE_NODEBUG size_t allocationCount() const noexcept { return _allocationCount; }

    //! Returns how many bytes are currently used.
    ASMJIT_INLINE_NODEBUG size_t usedSize() const noexcept { return _usedSize; }
    //! Returns the number of bytes unused by the allocator at the moment.
    ASMJIT_INLINE_NODEBUG size_t unusedSize() const noexcept { return _reservedSize - _usedSize; }
    //! Returns the total number of bytes reserved by the allocator (sum of sizes of all blocks).
    ASMJIT_INLINE_NODEBUG size_t reservedSize() const noexcept { return _reservedSize; }
    //! Returns the number of bytes the allocator needs to manage the allocated memory.
    ASMJIT_INLINE_NODEBUG size_t overheadSize() const noexcept { return _overheadSize; }

    ASMJIT_INLINE_NODEBUG double usedSizeAsPercent() const noexcept {
      return (double(usedSize()) / (double(reservedSize()) + 1e-16)) * 100.0;
    }

    ASMJIT_INLINE_NODEBUG double unusedSizeAsPercent() const noexcept {
      return (double(unusedSize()) / (double(reservedSize()) + 1e-16)) * 100.0;
    }

    ASMJIT_INLINE_NODEBUG double overheadSizeAsPercent() const noexcept {
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
