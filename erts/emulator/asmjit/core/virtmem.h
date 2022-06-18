// AsmJit - Machine code generation for C++
//
//  * Official AsmJit Home Page: https://asmjit.com
//  * Official Github Repository: https://github.com/asmjit/asmjit
//
// Copyright (c) 2008-2020 The AsmJit Authors
//
// This software is provided 'as-is', without any express or implied
// warranty. In no event will the authors be held liable for any damages
// arising from the use of this software.
//
// Permission is granted to anyone to use this software for any purpose,
// including commercial applications, and to alter it and redistribute it
// freely, subject to the following restrictions:
//
// 1. The origin of this software must not be misrepresented; you must not
//    claim that you wrote the original software. If you use this software
//    in a product, an acknowledgment in the product documentation would be
//    appreciated but is not required.
// 2. Altered source versions must be plainly marked as such, and must not be
//    misrepresented as being the original software.
// 3. This notice may not be removed or altered from any source distribution.

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

// ============================================================================
// [asmjit::VirtMem - Virtual Memory Management]
// ============================================================================

//! Virtual memory and memory mapping flags.
enum MemoryFlags : uint32_t {
  //! No access flags.
  kAccessNone = 0x00000000u,
  //! Memory is readable.
  kAccessRead = 0x00000001u,
  //! Memory is writable.
  kAccessWrite = 0x00000002u,
  //! Memory is executable.
  kAccessExecute = 0x00000004u,

  //! A combination of \ref kAccessRead and \ref kAccessWrite.
  kAccessReadWrite = kAccessRead | kAccessWrite,
  //! A combination of \ref kAccessRead, \ref kAccessWrite.
  kAccessRW = kAccessRead | kAccessWrite,
  //! A combination of \ref kAccessRead and \ref kAccessExecute.
  kAccessRX = kAccessRead | kAccessExecute,
  //! A combination of \ref kAccessRead, \ref kAccessWrite, and \ref kAccessExecute.
  kAccessRWX = kAccessRead | kAccessWrite | kAccessExecute,

  //! Use a `MAP_JIT` flag available on Apple platforms (introduced by Mojave),
  //! which allows JIT code to be executed in MAC bundles. This flag is not turned
  //! on by default, because when a process uses `fork()` the child process
  //! has no access to the pages mapped with `MAP_JIT`, which could break code
  //! that doesn't expect this behavior.
  kMMapEnableMapJit = 0x00000010u,

  //! Pass `PROT_MAX(PROT_READ)` to mmap() on platforms that support `PROT_MAX`.
  kMMapMaxAccessRead = 0x00000020u,
  //! Pass `PROT_MAX(PROT_WRITE)` to mmap() on platforms that support `PROT_MAX`.
  kMMapMaxAccessWrite = 0x00000040u,
  //! Pass `PROT_MAX(PROT_EXEC)` to mmap() on platforms that support `PROT_MAX`.
  kMMapMaxAccessExecute = 0x00000080u,

  //! A combination of \ref kMMapMaxAccessRead and \ref kMMapMaxAccessWrite.
  kMMapMaxAccessReadWrite = kMMapMaxAccessRead | kMMapMaxAccessWrite,
  //! A combination of \ref kMMapMaxAccessRead and \ref kMMapMaxAccessWrite.
  kMMapMaxAccessRW = kMMapMaxAccessRead | kMMapMaxAccessWrite,
  //! A combination of \ref kMMapMaxAccessRead and \ref kMMapMaxAccessExecute.
  kMMapMaxAccessRX = kMMapMaxAccessRead | kMMapMaxAccessExecute,
  //! A combination of \ref kMMapMaxAccessRead, \ref kMMapMaxAccessWrite, \ref kMMapMaxAccessExecute.
  kMMapMaxAccessRWX = kMMapMaxAccessRead | kMMapMaxAccessWrite | kMMapMaxAccessExecute,

  //! Not an access flag, only used by `allocDualMapping()` to override the
  //! default allocation strategy to always use a 'tmp' directory instead of
  //! "/dev/shm" (on POSIX platforms). Please note that this flag will be
  //! ignored if the operating system allows to allocate an executable memory
  //! by a different API than `open()` or `shm_open()`. For example on Linux
  //! `memfd_create()` is preferred and on BSDs `shm_open(SHM_ANON, ...)` is
  //! used if SHM_ANON is defined.
  kMappingPreferTmp = 0x80000000u
};

//! Allocates virtual memory by either using `mmap()` (POSIX) or `VirtualAlloc()` (Windows).
//!
//! \note `size` should be aligned to page size, use \ref VirtMem::info()
//! to obtain it. Invalid size will not be corrected by the implementation
//! and the allocation would not succeed in such case.
ASMJIT_API Error alloc(void** p, size_t size, uint32_t memoryFlags) noexcept;

//! Releases virtual memory previously allocated by \ref VirtMem::alloc().
//!
//! \note The size must be the same as used by \ref VirtMem::alloc(). If the
//! size is not the same value the call will fail on any POSIX system, but
//! pass on Windows, because it's implemented differently.
ASMJIT_API Error release(void* p, size_t size) noexcept;

//! A cross-platform wrapper around `mprotect()` (POSIX) and `VirtualProtect` (Windows).
ASMJIT_API Error protect(void* p, size_t size, uint32_t memoryFlags) noexcept;

// ============================================================================
// [asmjit::VirtMem - Dual Mapping]
// ============================================================================

//! Dual memory mapping used to map an anonymous memory into two memory regions
//! where one region is read-only, but executable, and the second region is
//! read+write, but not executable. Please see \ref VirtMem::allocDualMapping()
//! for more details.
struct DualMapping {
  union {
    //! Pointer to data having 'Read+Execute' access.
    void* rx;
    //! \cond
    void* ro;
    //! \endcond
  };
  //! Pointer to data having 'Read-Write' access.
  void* rw;
};

//! Allocates virtual memory and creates two views of it where the first view
//! has no write access. This is an addition to the API that should be used
//! in cases in which the operating system either enforces W^X security policy
//! or the application wants to use this policy by default to improve security
//! and prevent an accidental (or purposed) self-modifying code.
//!
//! The memory returned in the `dm` are two independent mappings of the same
//! shared memory region. You must use \ref VirtMem::releaseDualMapping() to
//! release it when it's no longer needed. Never use `VirtMem::release()` to
//! release the memory returned by `allocDualMapping()` as that would fail on
//! Windows.
//!
//! \remarks Both pointers in `dm` would be set to `nullptr` if the function fails.
ASMJIT_API Error allocDualMapping(DualMapping* dm, size_t size, uint32_t memoryFlags) noexcept;

//! Releases virtual memory mapping previously allocated by \ref VirtMem::allocDualMapping().
//!
//! \remarks Both pointers in `dm` would be set to `nullptr` if the function succeeds.
ASMJIT_API Error releaseDualMapping(DualMapping* dm, size_t size) noexcept;

// ============================================================================
// [asmjit::VirtMem - Instruction Cache]
// ============================================================================

//! Flushes instruction cache in the given region.
//!
//! Only useful on non-x86 architectures, however, it's a good practice to call
//! it on any platform to make your code more portable.
ASMJIT_API void flushInstructionCache(void* p, size_t size) noexcept;

// ============================================================================
// [asmjit::VirtMem - Page Info]
// ============================================================================

//! Virtual memory information.
struct Info {
  //! Virtual memory page size.
  uint32_t pageSize;
  //! Virtual memory page granularity.
  uint32_t pageGranularity;
};

//! Returns virtual memory information, see `VirtMem::Info` for more details.
ASMJIT_API Info info() noexcept;

// ============================================================================
// [asmjit::VirtMem - Hardened Runtime Info]
// ============================================================================

//! Hardened runtime flags.
enum HardenedRuntimeFlags : uint32_t {
  //! Hardened runtime is enabled - it's not possible to have "Write & Execute"
  //! memory protection. The runtime enforces W^X (either write or execute).
  //!
  //! \note If the runtime is hardened it means that an operating system specific
  //! protection is used. For example on MacOS platform it's possible to allocate
  //! memory with MAP_JIT flag and then use `pthread_jit_write_protect_np()` to
  //! temporarily swap access permissions for the current thread. Dual mapping is
  //! also a possibility on X86/X64 architecture.
  kHardenedRuntimeEnabled = 0x00000001u,

  //! Read+Write+Execute can only be allocated with MAP_JIT flag (Apple specific).
  kHardenedRuntimeMapJit = 0x00000002u
};

//! Hardened runtime information.
struct HardenedRuntimeInfo {
  //! Hardened runtime flags, see \ref HardenedRuntimeFlags.
  uint32_t flags;
};

//! Returns runtime features provided by the OS.
ASMJIT_API HardenedRuntimeInfo hardenedRuntimeInfo() noexcept;

// ============================================================================
// [asmjit::VirtMem - JIT Memory Protection]
// ============================================================================

//! Values that can be used with `protectJitMemory()` function.
enum ProtectJitAccess : uint32_t {
  //! Protect JIT memory with Read+Write permissions.
  kProtectJitReadWrite = 0,
  //! Protect JIT memory with Read+Execute permissions.
  kProtectJitReadExecute = 1
};

//! Protects access of memory mapped with MAP_JIT flag for the current thread.
//!
//! \note This feature is only available on Apple hardware (AArch64) at the
//! moment and and uses a non-portable `pthread_jit_write_protect_np()` call
//! when available.
//!
//! This function must be called before and after a memory mapped with MAP_JIT
//! flag is modified. Example:
//!
//! ```
//! void* codePtr = ...;
//! size_t codeSize = ...;
//!
//! VirtMem::protectJitMemory(VirtMem::kProtectJitReadWrite);
//! memcpy(codePtr, source, codeSize);
//! VirtMem::protectJitMemory(VirtMem::kProtectJitReadExecute);
//! VirtMem::flushInstructionCache(codePtr, codeSize);
//! ```
//!
//! See \ref ProtectJitReadWriteScope, which makes it simpler than the code above.
ASMJIT_API void protectJitMemory(ProtectJitAccess access) noexcept;

// ============================================================================
// [asmjit::VirtMem - ProtectJitReadWriteScope]
// ============================================================================

//! JIT protection scope that prepares the given memory block to be written to
//! in the current thread.
//!
//! It calls `VirtMem::protectJitMemory(kProtectJitReadWrite)` upon construction
//! and `VirtMem::protectJitMemory(kProtectJitReadExecute) combined with
//! `flushInstructionCache()` upon destruction. The purpose of this class is to
//! make writing to JIT memory easier.
class ProtectJitReadWriteScope {
public:
  void* _rxPtr;
  size_t _size;

  //! Makes the given memory block RW protected.
  inline ProtectJitReadWriteScope(void* rxPtr, size_t size) noexcept
    : _rxPtr(rxPtr),
      _size(size) {
    protectJitMemory(kProtectJitReadWrite);
  }

  //! Makes the memory block RX protected again and flushes instruction cache.
  inline ~ProtectJitReadWriteScope() noexcept {
    protectJitMemory(kProtectJitReadExecute);
    flushInstructionCache(_rxPtr, _size);
  }
};

} // VirtMem

//! \}

ASMJIT_END_NAMESPACE

#endif
#endif // ASMJIT_CORE_VIRTMEM_H_INCLUDED
