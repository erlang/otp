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

#include "../core/api-build_p.h"
#ifndef ASMJIT_NO_JIT

#include "../core/osutils.h"
#include "../core/string.h"
#include "../core/support.h"
#include "../core/virtmem.h"

#if !defined(_WIN32)
  #include <errno.h>
  #include <fcntl.h>
  #include <sys/mman.h>
  #include <sys/stat.h>
  #include <sys/types.h>
  #include <unistd.h>

  // Linux has a `memfd_create` syscall that we would like to use, if available.
  #if defined(__linux__)
    #include <sys/syscall.h>
  #endif

  // Apple recently introduced MAP_JIT flag, which we want to use.
  #if defined(__APPLE__)
    #include <TargetConditionals.h>
    #if TARGET_OS_OSX
      #include <sys/utsname.h>
    #endif
    // Older SDK doesn't define `MAP_JIT`.
    #ifndef MAP_JIT
      #define MAP_JIT 0x800
    #endif
  #endif

  // BSD/OSX: `MAP_ANONYMOUS` is not defined, `MAP_ANON` is.
  #if !defined(MAP_ANONYMOUS)
    #define MAP_ANONYMOUS MAP_ANON
  #endif
#endif

#include <atomic>

#if defined(__APPLE__) || defined(__BIONIC__)
  #define ASMJIT_VM_SHM_DETECT 0
#else
  #define ASMJIT_VM_SHM_DETECT 1
#endif

// Android NDK doesn't provide `shm_open()` and `shm_unlink()`.
#if defined(__BIONIC__)
  #define ASMJIT_VM_SHM_AVAILABLE 0
#else
  #define ASMJIT_VM_SHM_AVAILABLE 1
#endif

ASMJIT_BEGIN_NAMESPACE

// ============================================================================
// [asmjit::VirtMem - Utilities]
// ============================================================================

static const uint32_t VirtMem_dualMappingFilter[2] = {
  VirtMem::kAccessWrite,
  VirtMem::kAccessExecute
};

// ============================================================================
// [asmjit::VirtMem - Virtual Memory [Windows]]
// ============================================================================

#if defined(_WIN32)
struct ScopedHandle {
  inline ScopedHandle() noexcept
    : value(nullptr) {}

  inline ~ScopedHandle() noexcept {
    if (value != nullptr)
      ::CloseHandle(value);
  }

  HANDLE value;
};

static void VirtMem_getInfo(VirtMem::Info& vmInfo) noexcept {
  SYSTEM_INFO systemInfo;

  ::GetSystemInfo(&systemInfo);
  vmInfo.pageSize = Support::alignUpPowerOf2<uint32_t>(systemInfo.dwPageSize);
  vmInfo.pageGranularity = systemInfo.dwAllocationGranularity;
}

// Windows specific implementation that uses `VirtualAlloc` and `VirtualFree`.
static DWORD VirtMem_accessToWinProtectFlags(uint32_t flags) noexcept {
  DWORD protectFlags;

  // READ|WRITE|EXECUTE.
  if (flags & VirtMem::kAccessExecute)
    protectFlags = (flags & VirtMem::kAccessWrite) ? PAGE_EXECUTE_READWRITE : PAGE_EXECUTE_READ;
  else if (flags & VirtMem::kAccessReadWrite)
    protectFlags = (flags & VirtMem::kAccessWrite) ? PAGE_READWRITE : PAGE_READONLY;
  else
    protectFlags = PAGE_NOACCESS;

  // Any other flags to consider?
  return protectFlags;
}

static DWORD VirtMem_accessToWinDesiredAccess(uint32_t flags) noexcept {
  DWORD access = (flags & VirtMem::kAccessWrite) ? FILE_MAP_WRITE : FILE_MAP_READ;
  if (flags & VirtMem::kAccessExecute)
    access |= FILE_MAP_EXECUTE;
  return access;
}

Error VirtMem::alloc(void** p, size_t size, uint32_t flags) noexcept {
  *p = nullptr;
  if (size == 0)
    return DebugUtils::errored(kErrorInvalidArgument);

  DWORD protectFlags = VirtMem_accessToWinProtectFlags(flags);
  void* result = ::VirtualAlloc(nullptr, size, MEM_COMMIT | MEM_RESERVE, protectFlags);

  if (!result)
    return DebugUtils::errored(kErrorOutOfMemory);

  *p = result;
  return kErrorOk;
}

Error VirtMem::release(void* p, size_t size) noexcept {
  DebugUtils::unused(size);
  if (ASMJIT_UNLIKELY(!::VirtualFree(p, 0, MEM_RELEASE)))
    return DebugUtils::errored(kErrorInvalidArgument);
  return kErrorOk;
}

Error VirtMem::protect(void* p, size_t size, uint32_t flags) noexcept {
  DWORD protectFlags = VirtMem_accessToWinProtectFlags(flags);
  DWORD oldFlags;

  if (::VirtualProtect(p, size, protectFlags, &oldFlags))
    return kErrorOk;

  return DebugUtils::errored(kErrorInvalidArgument);
}

Error VirtMem::allocDualMapping(DualMapping* dm, size_t size, uint32_t flags) noexcept {
  dm->ro = nullptr;
  dm->rw = nullptr;

  if (size == 0)
    return DebugUtils::errored(kErrorInvalidArgument);

  ScopedHandle handle;
  handle.value = ::CreateFileMappingW(
    INVALID_HANDLE_VALUE,
    nullptr,
    PAGE_EXECUTE_READWRITE,
    (DWORD)(uint64_t(size) >> 32),
    (DWORD)(size & 0xFFFFFFFFu),
    nullptr);

  if (ASMJIT_UNLIKELY(!handle.value))
    return DebugUtils::errored(kErrorOutOfMemory);

  void* ptr[2];
  for (uint32_t i = 0; i < 2; i++) {
    DWORD desiredAccess = VirtMem_accessToWinDesiredAccess(flags & ~VirtMem_dualMappingFilter[i]);
    ptr[i] = ::MapViewOfFile(handle.value, desiredAccess, 0, 0, size);

    if (ptr[i] == nullptr) {
      if (i == 0)
        ::UnmapViewOfFile(ptr[0]);
      return DebugUtils::errored(kErrorOutOfMemory);
    }
  }

  dm->ro = ptr[0];
  dm->rw = ptr[1];
  return kErrorOk;
}

Error VirtMem::releaseDualMapping(DualMapping* dm, size_t size) noexcept {
  DebugUtils::unused(size);
  bool failed = false;

  if (!::UnmapViewOfFile(dm->ro))
    failed = true;

  if (dm->ro != dm->rw && !UnmapViewOfFile(dm->rw))
    failed = true;

  if (failed)
    return DebugUtils::errored(kErrorInvalidArgument);

  dm->ro = nullptr;
  dm->rw = nullptr;
  return kErrorOk;
}
#endif

// ============================================================================
// [asmjit::VirtMem - Virtual Memory [Posix]]
// ============================================================================

#if !defined(_WIN32)
class AnonymousMemory {
public:
  enum FileType : uint32_t {
    kFileTypeNone,
    kFileTypeTmp,
    kFileTypeShm
  };

  int fd;
  FileType fileType;
  StringTmp<128> tmpName;

  AnonymousMemory() noexcept
    : fd(-1),
      fileType(kFileTypeNone),
      tmpName() {}

  ~AnonymousMemory() noexcept {
    unlinkFile();
    closeFile();
  }

  int closeFile() noexcept {
    if (fd >= 0) {
      int result = close(fd);
      fd = -1;
      return result;
    }
    else {
      return 0;
    }
  }

  int unlinkFile() noexcept {
    FileType type = fileType;
    fileType = kFileTypeNone;

    if (type == kFileTypeTmp)
      return unlink(tmpName.data());
#if ASMJIT_VM_SHM_AVAILABLE
    else if (type == kFileTypeShm)
      return shm_unlink(tmpName.data());
#endif
    else
      return 0;
  }
};

static void VirtMem_getInfo(VirtMem::Info& vmInfo) noexcept {
  uint32_t pageSize = uint32_t(::getpagesize());

  vmInfo.pageSize = pageSize;
  vmInfo.pageGranularity = Support::max<uint32_t>(pageSize, 65536);
}

// Some operating systems don't allow /dev/shm to be executable. On Linux this
// happens when /dev/shm is mounted with 'noexec', which is enforced by systemd.
// Other operating systems like OSX also restrict executable permissions regarding
// /dev/shm, so we use a runtime detection before trying to allocate the requested
// memory by the user. Sometimes we don't need the detection as we know it would
// always result in 'kShmStrategyTmpDir'.
enum ShmStrategy : uint32_t {
  kShmStrategyUnknown = 0,
  kShmStrategyDevShm = 1,
  kShmStrategyTmpDir = 2
};

// Posix specific implementation that uses `mmap()` and `munmap()`.
static int VirtMem_accessToPosixProtection(uint32_t flags) noexcept {
  int protection = 0;
  if (flags & VirtMem::kAccessRead   ) protection |= PROT_READ;
  if (flags & VirtMem::kAccessWrite  ) protection |= PROT_READ | PROT_WRITE;
  if (flags & VirtMem::kAccessExecute) protection |= PROT_READ | PROT_EXEC;
  return protection;
}

// Translates libc errors specific to VirtualMemory mapping to `asmjit::Error`.
static Error VirtMem_makeErrorFromErrno(int e) noexcept {
  switch (e) {
    case EACCES:
    case EAGAIN:
    case ENODEV:
    case EPERM:
      return kErrorInvalidState;

    case EFBIG:
    case ENOMEM:
    case EOVERFLOW:
      return kErrorOutOfMemory;

    case EMFILE:
    case ENFILE:
      return kErrorTooManyHandles;

    default:
      return kErrorInvalidArgument;
  }
}

#if defined(__APPLE__)
// Detects whether the current process is hardened, which means that pages that
// have WRITE and EXECUTABLE flags cannot be allocated without MAP_JIT flag.
static ASMJIT_INLINE bool VirtMem_isHardened() noexcept {
  static volatile uint32_t globalHardenedFlag;

  enum HardenedFlag : uint32_t {
    kHardenedFlagUnknown  = 0,
    kHardenedFlagDisabled = 1,
    kHardenedFlagEnabled  = 2
  };

  uint32_t flag = globalHardenedFlag;
  if (flag == kHardenedFlagUnknown) {
    VirtMem::Info memInfo;
    VirtMem_getInfo(memInfo);

    void* ptr = mmap(nullptr, memInfo.pageSize, PROT_WRITE | PROT_EXEC, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    if (ptr == MAP_FAILED) {
      flag = kHardenedFlagEnabled;
    }
    else {
      flag = kHardenedFlagDisabled;
      munmap(ptr, memInfo.pageSize);
    }
    globalHardenedFlag = flag;
  }

  return flag == kHardenedFlagEnabled;
}

// MAP_JIT flag required to run unsigned JIT code is only supported by kernel
// version 10.14+ (Mojave) and IOS.
static ASMJIT_INLINE bool VirtMem_hasMapJitSupport() noexcept {
#if TARGET_OS_OSX
  static volatile int globalVersion;

  int ver = globalVersion;
  if (!ver) {
    struct utsname osname;
    uname(&osname);
    ver = atoi(osname.release);
    globalVersion = ver;
  }
  return ver >= 18;
#else
  // Assume it's available.
  return true;
#endif
}

static ASMJIT_INLINE int VirtMem_appleSpecificMMapFlags(uint32_t flags) noexcept {
  // Always use MAP_JIT flag if user asked for it (could be used for testing
  // on non-hardened processes) and detect whether it must be used when the
  // process is actually hardened (in that case it doesn't make sense to rely
  // on user `flags`).
  bool useMapJit = ((flags & VirtMem::kMMapEnableMapJit) != 0) || VirtMem_isHardened();
  if (useMapJit)
    return VirtMem_hasMapJitSupport() ? int(MAP_JIT) : 0;
  else
    return 0;
}
#else
static ASMJIT_INLINE int VirtMem_appleSpecificMMapFlags(uint32_t flags) noexcept {
  DebugUtils::unused(flags);
  return 0;
}
#endif

#if !defined(SHM_ANON)
static const char* VirtMem_getTmpDir() noexcept {
  const char* tmpDir = getenv("TMPDIR");
  return tmpDir ? tmpDir : "/tmp";
}
#endif

static Error VirtMem_openAnonymousMemory(AnonymousMemory* anonMem, bool preferTmpOverDevShm) noexcept {
#if defined(SYS_memfd_create)
  // Linux specific 'memfd_create' - if the syscall returns `ENOSYS` it means
  // it's not available and we will never call it again (would be pointless).

  // Zero initialized, if ever changed to '1' that would mean the syscall is not
  // available and we must use `shm_open()` and `shm_unlink()`.
  static volatile uint32_t memfd_create_not_supported;

  if (!memfd_create_not_supported) {
    anonMem->fd = (int)syscall(SYS_memfd_create, "vmem", 0);
    if (ASMJIT_LIKELY(anonMem->fd >= 0))
      return kErrorOk;

    int e = errno;
    if (e == ENOSYS)
      memfd_create_not_supported = 1;
    else
      return DebugUtils::errored(VirtMem_makeErrorFromErrno(e));
  }
#endif

#if defined(SHM_ANON)
  // Originally FreeBSD extension, apparently works in other BSDs too.
  DebugUtils::unused(preferTmpOverDevShm);
  anonMem->fd = shm_open(SHM_ANON, O_RDWR | O_CREAT | O_EXCL, S_IRUSR | S_IWUSR);

  if (ASMJIT_LIKELY(anonMem->fd >= 0))
    return kErrorOk;
  else
    return DebugUtils::errored(VirtMem_makeErrorFromErrno(errno));
#else
  // POSIX API. We have to generate somehow a unique name. This is nothing
  // cryptographic, just using a bit from the stack address to always have
  // a different base for different threads (as threads have their own stack)
  // and retries for avoiding collisions. We use `shm_open()` with flags that
  // require creation of the file so we never open an existing shared memory.
  static std::atomic<uint32_t> internalCounter;
  const char* kShmFormat = "/shm-id-%016llX";

  uint32_t kRetryCount = 100;
  uint64_t bits = ((uintptr_t)(void*)anonMem) & 0x55555555u;

  for (uint32_t i = 0; i < kRetryCount; i++) {
    bits -= uint64_t(OSUtils::getTickCount()) * 773703683;
    bits = ((bits >> 14) ^ (bits << 6)) + uint64_t(++internalCounter) * 10619863;

    if (!ASMJIT_VM_SHM_DETECT || preferTmpOverDevShm) {
      anonMem->tmpName.assign(VirtMem_getTmpDir());
      anonMem->tmpName.appendFormat(kShmFormat, (unsigned long long)bits);
      anonMem->fd = open(anonMem->tmpName.data(), O_RDWR | O_CREAT | O_EXCL, 0);
      if (ASMJIT_LIKELY(anonMem->fd >= 0)) {
        anonMem->fileType = AnonymousMemory::kFileTypeTmp;
        return kErrorOk;
      }
    }
#if ASMJIT_VM_SHM_AVAILABLE
    else {
      anonMem->tmpName.assignFormat(kShmFormat, (unsigned long long)bits);
      anonMem->fd = shm_open(anonMem->tmpName.data(), O_RDWR | O_CREAT | O_EXCL, S_IRUSR | S_IWUSR);
      if (ASMJIT_LIKELY(anonMem->fd >= 0)) {
        anonMem->fileType = AnonymousMemory::kFileTypeShm;
        return kErrorOk;
      }
    }
#endif

    int e = errno;
    if (e != EEXIST)
      return DebugUtils::errored(VirtMem_makeErrorFromErrno(e));
  }

  return DebugUtils::errored(kErrorFailedToOpenAnonymousMemory);
#endif
}

#if ASMJIT_VM_SHM_DETECT
static Error VirtMem_detectShmStrategy(uint32_t* strategyOut) noexcept {
  AnonymousMemory anonMem;
  VirtMem::Info vmInfo = VirtMem::info();

  ASMJIT_PROPAGATE(VirtMem_openAnonymousMemory(&anonMem, false));
  if (ftruncate(anonMem.fd, off_t(vmInfo.pageSize)) != 0)
    return DebugUtils::errored(VirtMem_makeErrorFromErrno(errno));

  void* ptr = mmap(nullptr, vmInfo.pageSize, PROT_READ | PROT_EXEC, MAP_SHARED, anonMem.fd, 0);
  if (ptr == MAP_FAILED) {
    int e = errno;
    if (e == EINVAL) {
      *strategyOut = kShmStrategyTmpDir;
      return kErrorOk;
    }
    return DebugUtils::errored(VirtMem_makeErrorFromErrno(e));
  }
  else {
    munmap(ptr, vmInfo.pageSize);
    *strategyOut = kShmStrategyDevShm;
    return kErrorOk;
  }
}
#endif

#if ASMJIT_VM_SHM_DETECT
static Error VirtMem_getShmStrategy(uint32_t* strategyOut) noexcept {
  // Initially don't assume anything. It has to be tested whether
  // '/dev/shm' was mounted with 'noexec' flag or not.
  static volatile uint32_t globalShmStrategy = kShmStrategyUnknown;

  uint32_t strategy = globalShmStrategy;
  if (strategy == kShmStrategyUnknown) {
    ASMJIT_PROPAGATE(VirtMem_detectShmStrategy(&strategy));
    globalShmStrategy = strategy;
  }

  *strategyOut = strategy;
  return kErrorOk;
}
#else
static Error VirtMem_getShmStrategy(uint32_t* strategyOut) noexcept {
  *strategyOut = kShmStrategyTmpDir;
  return kErrorOk;
}
#endif

Error VirtMem::alloc(void** p, size_t size, uint32_t flags) noexcept {
  *p = nullptr;

  if (size == 0)
    return DebugUtils::errored(kErrorInvalidArgument);

  int protection = VirtMem_accessToPosixProtection(flags);
  int mmFlags = MAP_PRIVATE | MAP_ANONYMOUS | VirtMem_appleSpecificMMapFlags(flags);
  void* ptr = mmap(nullptr, size, protection, mmFlags, -1, 0);

  if (ptr == MAP_FAILED)
    return DebugUtils::errored(kErrorOutOfMemory);

  *p = ptr;
  return kErrorOk;
}

Error VirtMem::release(void* p, size_t size) noexcept {
  if (ASMJIT_UNLIKELY(munmap(p, size) != 0))
    return DebugUtils::errored(kErrorInvalidArgument);

  return kErrorOk;
}


Error VirtMem::protect(void* p, size_t size, uint32_t flags) noexcept {
  int protection = VirtMem_accessToPosixProtection(flags);
  if (mprotect(p, size, protection) == 0)
    return kErrorOk;

  return DebugUtils::errored(kErrorInvalidArgument);
}

Error VirtMem::allocDualMapping(DualMapping* dm, size_t size, uint32_t flags) noexcept {
  dm->ro = nullptr;
  dm->rw = nullptr;

  if (off_t(size) <= 0)
    return DebugUtils::errored(size == 0 ? kErrorInvalidArgument : kErrorTooLarge);

  bool preferTmpOverDevShm = (flags & kMappingPreferTmp) != 0;
  if (!preferTmpOverDevShm) {
    uint32_t strategy;
    ASMJIT_PROPAGATE(VirtMem_getShmStrategy(&strategy));
    preferTmpOverDevShm = (strategy == kShmStrategyTmpDir);
  }

  AnonymousMemory anonMem;
  ASMJIT_PROPAGATE(VirtMem_openAnonymousMemory(&anonMem, preferTmpOverDevShm));
  if (ftruncate(anonMem.fd, off_t(size)) != 0)
    return DebugUtils::errored(VirtMem_makeErrorFromErrno(errno));

  void* ptr[2];
  for (uint32_t i = 0; i < 2; i++) {
    ptr[i] = mmap(nullptr, size, VirtMem_accessToPosixProtection(flags & ~VirtMem_dualMappingFilter[i]), MAP_SHARED, anonMem.fd, 0);
    if (ptr[i] == MAP_FAILED) {
      // Get the error now before `munmap` has a chance to clobber it.
      int e = errno;
      if (i == 1)
        munmap(ptr[0], size);
      return DebugUtils::errored(VirtMem_makeErrorFromErrno(e));
    }
  }

  dm->ro = ptr[0];
  dm->rw = ptr[1];
  return kErrorOk;
}

Error VirtMem::releaseDualMapping(DualMapping* dm, size_t size) noexcept {
  Error err = release(dm->ro, size);
  if (dm->ro != dm->rw)
    err |= release(dm->rw, size);

  if (err)
    return DebugUtils::errored(kErrorInvalidArgument);

  dm->ro = nullptr;
  dm->rw = nullptr;
  return kErrorOk;
}
#endif

// ============================================================================
// [asmjit::VirtMem - Virtual Memory [Memory Info]]
// ============================================================================

VirtMem::Info VirtMem::info() noexcept {
  static VirtMem::Info vmInfo;
  static std::atomic<uint32_t> vmInfoInitialized;

  if (!vmInfoInitialized.load()) {
    VirtMem::Info localMemInfo;
    VirtMem_getInfo(localMemInfo);

    vmInfo = localMemInfo;
    vmInfoInitialized.store(1u);
  }

  return vmInfo;
}

ASMJIT_END_NAMESPACE

#endif
