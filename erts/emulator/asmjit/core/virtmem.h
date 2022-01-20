// This file is part of AsmJit project <https://asmjit.com>
//
// See asmjit.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#ifndef ASMJIT_CORE_VIRTMEM_H_INCLUDED
#define ASMJIT_CORE_VIRTMEM_H_INCLUDED

#include "../core/api-config.h"
#ifndef ASMJIT_NO_JIT

#include "../core/globals.h"

ASMJIT_BEGIN_NAMESPACE

//! \addtogroup asmjit_virtual_memory
//! \{

//! Virtual memory management.
namespace VirtMem {

//! Flushes instruction cache in the given region.
//!
//! Only useful on non-x86 architectures, however, it's a good practice to call it on any platform to make your
//! code more portable.
ASMJIT_API void flushInstructionCache(void* p, size_t size) noexcept;

//! Virtual memory information.
struct Info {
  //! Virtual memory page size.
  uint32_t pageSize;
  //! Virtual memory page granularity.
  uint32_t pageGranularity;
};

//! Returns virtual memory information, see `VirtMem::Info` for more details.
ASMJIT_API Info info() noexcept;

//! Virtual memory access and mmap-specific flags.
enum class MemoryFlags : uint32_t {
  //! No flags.
  kNone = 0,

  //! Memory is readable.
  kAccessRead = 0x00000001u,

  //! Memory is writable.
  kAccessWrite = 0x00000002u,

  //! Memory is executable.
  kAccessExecute = 0x00000004u,

  //! A combination of \ref MemoryFlags::kAccessRead and \ref MemoryFlags::kAccessWrite.
  kAccessReadWrite = kAccessRead | kAccessWrite,

  //! A combination of \ref MemoryFlags::kAccessRead, \ref MemoryFlags::kAccessWrite.
  kAccessRW = kAccessRead | kAccessWrite,

  //! A combination of \ref MemoryFlags::kAccessRead and \ref MemoryFlags::kAccessExecute.
  kAccessRX = kAccessRead | kAccessExecute,

  //! A combination of \ref MemoryFlags::kAccessRead, \ref MemoryFlags::kAccessWrite, and
  //! \ref MemoryFlags::kAccessExecute.
  kAccessRWX = kAccessRead | kAccessWrite | kAccessExecute,

  //! Use a `MAP_JIT` flag available on Apple platforms (introduced by Mojave), which allows JIT code to be executed
  //! in MAC bundles. This flag is not turned on by default, because when a process uses `fork()` the child process
  //! has no access to the pages mapped with `MAP_JIT`, which could break code that doesn't expect this behavior.
  //!
  //! \note This flag can only be used with \ref VirtMem::alloc().
  kMMapEnableMapJit = 0x00000010u,

  //! Pass `PROT_MAX(PROT_READ)` to mmap() on platforms that support `PROT_MAX`.
  //!
  //! \note This flag can only be used with \ref VirtMem::alloc().
  kMMapMaxAccessRead = 0x00000020u,
  //! Pass `PROT_MAX(PROT_WRITE)` to mmap() on platforms that support `PROT_MAX`.
  //!
  //! \note This flag can only be used with \ref VirtMem::alloc().
  kMMapMaxAccessWrite = 0x00000040u,
  //! Pass `PROT_MAX(PROT_EXEC)` to mmap() on platforms that support `PROT_MAX`.
  //!
  //! \note This flag can only be used with \ref VirtMem::alloc().
  kMMapMaxAccessExecute = 0x00000080u,

  //! A combination of \ref MemoryFlags::kMMapMaxAccessRead and \ref MemoryFlags::kMMapMaxAccessWrite.
  kMMapMaxAccessReadWrite = kMMapMaxAccessRead | kMMapMaxAccessWrite,

  //! A combination of \ref MemoryFlags::kMMapMaxAccessRead and \ref MemoryFlags::kMMapMaxAccessWrite.
  kMMapMaxAccessRW = kMMapMaxAccessRead | kMMapMaxAccessWrite,

  //! A combination of \ref MemoryFlags::kMMapMaxAccessRead and \ref MemoryFlags::kMMapMaxAccessExecute.
  kMMapMaxAccessRX = kMMapMaxAccessRead | kMMapMaxAccessExecute,

  //! A combination of \ref MemoryFlags::kMMapMaxAccessRead, \ref MemoryFlags::kMMapMaxAccessWrite, \ref
  //! MemoryFlags::kMMapMaxAccessExecute.
  kMMapMaxAccessRWX = kMMapMaxAccessRead | kMMapMaxAccessWrite | kMMapMaxAccessExecute,

  //! Not an access flag, only used by `allocDualMapping()` to override the default allocation strategy to always use
  //! a 'tmp' directory instead of "/dev/shm" (on POSIX platforms). Please note that this flag will be ignored if the
  //! operating system allows to allocate an executable memory by a different API than `open()` or `shm_open()`. For
  //! example on Linux `memfd_create()` is preferred and on BSDs `shm_open(SHM_ANON, ...)` is used if SHM_ANON is
  //! defined.
  //!
  //! \note This flag can only be used with \ref VirtMem::alloc().
  kMappingPreferTmp = 0x80000000u
};
ASMJIT_DEFINE_ENUM_FLAGS(MemoryFlags)

//! Allocates virtual memory by either using `mmap()` (POSIX) or `VirtualAlloc()` (Windows).
//!
//! \note `size` should be aligned to page size, use \ref VirtMem::info() to obtain it. Invalid size will not be
//! corrected by the implementation and the allocation would not succeed in such case.
ASMJIT_API Error alloc(void** p, size_t size, MemoryFlags flags) noexcept;

//! Releases virtual memory previously allocated by \ref VirtMem::alloc().
//!
//! \note The size must be the same as used by \ref VirtMem::alloc(). If the size is not the same value the call
//! will fail on any POSIX system, but pass on Windows, because it's implemented differently.
ASMJIT_API Error release(void* p, size_t size) noexcept;

//! A cross-platform wrapper around `mprotect()` (POSIX) and `VirtualProtect()` (Windows).
ASMJIT_API Error protect(void* p, size_t size, MemoryFlags flags) noexcept;

//! Dual memory mapping used to map an anonymous memory into two memory regions where one region is read-only, but
//! executable, and the second region is read+write, but not executable. See \ref VirtMem::allocDualMapping() for
//! more details.
struct DualMapping {
  //! Pointer to data with 'Read+Execute' access (this memory is not writable).
  void* rx;
  //! Pointer to data with 'Read+Write' access (this memory is not executable).
  void* rw;
};

//! Allocates virtual memory and creates two views of it where the first view has no write access. This is an addition
//! to the API that should be used in cases in which the operating system either enforces W^X security policy or the
//! application wants to use this policy by default to improve security and prevent an accidental (or purposed)
//! self-modifying code.
//!
//! The memory returned in the `dm` are two independent mappings of the same shared memory region. You must use
//! \ref VirtMem::releaseDualMapping() to release it when it's no longer needed. Never use `VirtMem::release()` to
//! release the memory returned by `allocDualMapping()` as that would fail on Windows.
//!
//! \remarks Both pointers in `dm` would be set to `nullptr` if the function fails.
ASMJIT_API Error allocDualMapping(DualMapping* dm, size_t size, MemoryFlags flags) noexcept;

//! Releases virtual memory mapping previously allocated by \ref VirtMem::allocDualMapping().
//!
//! \remarks Both pointers in `dm` would be set to `nullptr` if the function succeeds.
ASMJIT_API Error releaseDualMapping(DualMapping* dm, size_t size) noexcept;

//! Hardened runtime flags.
enum class HardenedRuntimeFlags : uint32_t {
  //! No flags.
  kNone = 0,

  //! Hardened runtime is enabled - it's not possible to have "Write & Execute" memory protection. The runtime
  //! enforces W^X (either write or execute).
  //!
  //! \note If the runtime is hardened it means that an operating system specific protection is used. For example on
  //! MacOS platform it's possible to allocate memory with MAP_JIT flag and then use `pthread_jit_write_protect_np()`
  //! to temporarily swap access permissions for the current thread. Dual mapping is also a possibility on X86/X64
  //! architecture.
  kEnabled = 0x00000001u,

  //! Read+Write+Execute can only be allocated with MAP_JIT flag (Apple specific).
  kMapJit = 0x00000002u
};
ASMJIT_DEFINE_ENUM_FLAGS(HardenedRuntimeFlags)

//! Hardened runtime information.
struct HardenedRuntimeInfo {
  //! Hardened runtime flags.
  HardenedRuntimeFlags flags;
};

//! Returns runtime features provided by the OS.
ASMJIT_API HardenedRuntimeInfo hardenedRuntimeInfo() noexcept;

//! Values that can be used with `protectJitMemory()` function.
enum class ProtectJitAccess : uint32_t {
  //! Protect JIT memory with Read+Write permissions.
  kReadWrite = 0,
  //! Protect JIT memory with Read+Execute permissions.
  kReadExecute = 1
};

//! Protects access of memory mapped with MAP_JIT flag for the current thread.
//!
//! \note This feature is only available on Apple hardware (AArch64) at the moment and and uses a non-portable
//! `pthread_jit_write_protect_np()` call when available.
//!
//! This function must be called before and after a memory mapped with MAP_JIT flag is modified. Example:
//!
//! ```
//! void* codePtr = ...;
//! size_t codeSize = ...;
//!
//! VirtMem::protectJitMemory(VirtMem::ProtectJitAccess::kReadWrite);
//! memcpy(codePtr, source, codeSize);
//! VirtMem::protectJitMemory(VirtMem::ProtectJitAccess::kReadExecute);
//! VirtMem::flushInstructionCache(codePtr, codeSize);
//! ```
//!
//! See \ref ProtectJitReadWriteScope, which makes it simpler than the code above.
ASMJIT_API void protectJitMemory(ProtectJitAccess access) noexcept;

//! JIT protection scope that prepares the given memory block to be written to in the current thread.
//!
//! It calls `VirtMem::protectJitMemory(VirtMem::ProtectJitAccess::kReadWrite)` at construction time and
//! `VirtMem::protectJitMemory(VirtMem::ProtectJitAccess::kReadExecute)` combined with `flushInstructionCache()`
//! in destructor. The purpose of this class is to make writing to JIT memory easier.
class ProtectJitReadWriteScope {
public:
  void* _rxPtr;
  size_t _size;

  //! Makes the given memory block RW protected.
  ASMJIT_FORCE_INLINE ProtectJitReadWriteScope(void* rxPtr, size_t size) noexcept
    : _rxPtr(rxPtr),
      _size(size) {
    protectJitMemory(ProtectJitAccess::kReadWrite);
  }

  // Not copyable.
  ProtectJitReadWriteScope(const ProtectJitReadWriteScope& other) = delete;

  //! Makes the memory block RX protected again and flushes instruction cache.
  ASMJIT_FORCE_INLINE  ~ProtectJitReadWriteScope() noexcept {
    protectJitMemory(ProtectJitAccess::kReadExecute);
    flushInstructionCache(_rxPtr, _size);
  }
};

} // VirtMem

//! \}

ASMJIT_END_NAMESPACE

#endif
#endif // ASMJIT_CORE_VIRTMEM_H_INCLUDED
