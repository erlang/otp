// This file is part of AsmJit project <https://asmjit.com>
//
// See asmjit.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

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
    #include <pthread.h>
    #include <TargetConditionals.h>
    #if TARGET_OS_OSX
      #include <sys/utsname.h>
      #include <libkern/OSCacheControl.h> // sys_icache_invalidate().
    #endif
    // Older SDK doesn't define `MAP_JIT`.
    #ifndef MAP_JIT
      #define MAP_JIT 0x800
    #endif
  #endif

  // BSD/MAC: `MAP_ANONYMOUS` is not defined, `MAP_ANON` is.
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

#if defined(__APPLE__) && ASMJIT_ARCH_ARM >= 64
  #define ASMJIT_HAS_PTHREAD_JIT_WRITE_PROTECT_NP
#endif

ASMJIT_BEGIN_SUB_NAMESPACE(VirtMem)

// Virtual Memory Utilities
// ========================

static const MemoryFlags dualMappingFilter[2] = {
  MemoryFlags::kAccessWrite | MemoryFlags::kMMapMaxAccessWrite,
  MemoryFlags::kAccessExecute | MemoryFlags::kMMapMaxAccessExecute
};

// Virtual Memory [Windows]
// ========================

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

static void getVMInfo(Info& vmInfo) noexcept {
  SYSTEM_INFO systemInfo;

  ::GetSystemInfo(&systemInfo);
  vmInfo.pageSize = Support::alignUpPowerOf2<uint32_t>(systemInfo.dwPageSize);
  vmInfo.pageGranularity = systemInfo.dwAllocationGranularity;
}

// Returns windows-specific protectFlags from \ref MemoryFlags.
static DWORD protectFlagsFromMemoryFlags(MemoryFlags memoryFlags) noexcept {
  DWORD protectFlags;

  // READ|WRITE|EXECUTE.
  if (Support::test(memoryFlags, MemoryFlags::kAccessExecute))
    protectFlags = Support::test(memoryFlags, MemoryFlags::kAccessWrite) ? PAGE_EXECUTE_READWRITE : PAGE_EXECUTE_READ;
  else if (Support::test(memoryFlags, MemoryFlags::kAccessRW))
    protectFlags = Support::test(memoryFlags, MemoryFlags::kAccessWrite) ? PAGE_READWRITE : PAGE_READONLY;
  else
    protectFlags = PAGE_NOACCESS;

  // Any other flags to consider?
  return protectFlags;
}

static DWORD desiredAccessFromMemoryFlags(MemoryFlags memoryFlags) noexcept {
  DWORD access = Support::test(memoryFlags, MemoryFlags::kAccessWrite) ? FILE_MAP_WRITE : FILE_MAP_READ;
  if (Support::test(memoryFlags, MemoryFlags::kAccessExecute))
    access |= FILE_MAP_EXECUTE;
  return access;
}

static HardenedRuntimeFlags getHardenedRuntimeFlags() noexcept {
  return HardenedRuntimeFlags::kNone;
}

Error alloc(void** p, size_t size, MemoryFlags memoryFlags) noexcept {
  *p = nullptr;
  if (size == 0)
    return DebugUtils::errored(kErrorInvalidArgument);

  DWORD protectFlags = protectFlagsFromMemoryFlags(memoryFlags);
  void* result = ::VirtualAlloc(nullptr, size, MEM_COMMIT | MEM_RESERVE, protectFlags);

  if (!result)
    return DebugUtils::errored(kErrorOutOfMemory);

  *p = result;
  return kErrorOk;
}

Error release(void* p, size_t size) noexcept {
  DebugUtils::unused(size);
  if (ASMJIT_UNLIKELY(!::VirtualFree(p, 0, MEM_RELEASE)))
    return DebugUtils::errored(kErrorInvalidArgument);
  return kErrorOk;
}

Error protect(void* p, size_t size, MemoryFlags memoryFlags) noexcept {
  DWORD protectFlags = protectFlagsFromMemoryFlags(memoryFlags);
  DWORD oldFlags;

  if (::VirtualProtect(p, size, protectFlags, &oldFlags))
    return kErrorOk;

  return DebugUtils::errored(kErrorInvalidArgument);
}

Error allocDualMapping(DualMapping* dm, size_t size, MemoryFlags memoryFlags) noexcept {
  dm->rx = nullptr;
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
    MemoryFlags accessFlags = memoryFlags & ~dualMappingFilter[i];
    DWORD desiredAccess = desiredAccessFromMemoryFlags(accessFlags);
    ptr[i] = ::MapViewOfFile(handle.value, desiredAccess, 0, 0, size);

    if (ptr[i] == nullptr) {
      if (i == 0)
        ::UnmapViewOfFile(ptr[0]);
      return DebugUtils::errored(kErrorOutOfMemory);
    }
  }

  dm->rx = ptr[0];
  dm->rw = ptr[1];
  return kErrorOk;
}

Error releaseDualMapping(DualMapping* dm, size_t size) noexcept {
  DebugUtils::unused(size);
  bool failed = false;

  if (!::UnmapViewOfFile(dm->rx))
    failed = true;

  if (dm->rx != dm->rw && !UnmapViewOfFile(dm->rw))
    failed = true;

  if (failed)
    return DebugUtils::errored(kErrorInvalidArgument);

  dm->rx = nullptr;
  dm->rw = nullptr;
  return kErrorOk;
}

#endif

// Virtual Memory [Posix]
// ======================

#if !defined(_WIN32)

static void getVMInfo(Info& vmInfo) noexcept {
  uint32_t pageSize = uint32_t(::getpagesize());

  vmInfo.pageSize = pageSize;
  vmInfo.pageGranularity = Support::max<uint32_t>(pageSize, 65536);
}

#if !defined(SHM_ANON)
static const char* getTmpDir() noexcept {
  const char* tmpDir = getenv("TMPDIR");
  return tmpDir ? tmpDir : "/tmp";
}
#endif

// Translates libc errors specific to VirtualMemory mapping to `asmjit::Error`.
static Error asmjitErrorFromErrno(int e) noexcept {
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

// Some operating systems don't allow /dev/shm to be executable. On Linux this happens when /dev/shm is mounted with
// 'noexec', which is enforced by systemd. Other operating systems like MacOS also restrict executable permissions
// regarding /dev/shm, so we use a runtime detection before attempting to allocate executable memory. Sometimes we
// don't need the detection as we know it would always result in `ShmStrategy::kTmpDir`.
enum class ShmStrategy : uint32_t {
  kUnknown = 0,
  kDevShm = 1,
  kTmpDir = 2
};

class AnonymousMemory {
public:
  enum FileType : uint32_t {
    kFileTypeNone,
    kFileTypeShm,
    kFileTypeTmp
  };

  int _fd;
  FileType _fileType;
  StringTmp<128> _tmpName;

  inline AnonymousMemory() noexcept
    : _fd(-1),
      _fileType(kFileTypeNone),
      _tmpName() {}

  inline ~AnonymousMemory() noexcept {
    unlink();
    close();
  }

  inline int fd() const noexcept { return _fd; }

  Error open(bool preferTmpOverDevShm) noexcept {
#if defined(__linux__) && defined(__NR_memfd_create)
    // Linux specific 'memfd_create' - if the syscall returns `ENOSYS` it means
    // it's not available and we will never call it again (would be pointless).

    // Zero initialized, if ever changed to '1' that would mean the syscall is not
    // available and we must use `shm_open()` and `shm_unlink()`.
    static volatile uint32_t memfd_create_not_supported;

    if (!memfd_create_not_supported) {
      _fd = (int)syscall(__NR_memfd_create, "vmem", 0);
      if (ASMJIT_LIKELY(_fd >= 0))
        return kErrorOk;

      int e = errno;
      if (e == ENOSYS)
        memfd_create_not_supported = 1;
      else
        return DebugUtils::errored(asmjitErrorFromErrno(e));
    }
#endif

#if defined(SHM_ANON)
    // Originally FreeBSD extension, apparently works in other BSDs too.
    DebugUtils::unused(preferTmpOverDevShm);
    _fd = ::shm_open(SHM_ANON, O_RDWR | O_CREAT | O_EXCL, S_IRUSR | S_IWUSR);

    if (ASMJIT_LIKELY(_fd >= 0))
      return kErrorOk;
    else
      return DebugUtils::errored(asmjitErrorFromErrno(errno));
#else
    // POSIX API. We have to generate somehow a unique name. This is nothing cryptographic, just using a bit from
    // the stack address to always have a different base for different threads (as threads have their own stack)
    // and retries for avoiding collisions. We use `shm_open()` with flags that require creation of the file so we
    // never open an existing shared memory.
    static std::atomic<uint32_t> internalCounter;
    const char* kShmFormat = "/shm-id-%016llX";

    uint32_t kRetryCount = 100;
    uint64_t bits = ((uintptr_t)(void*)this) & 0x55555555u;

    for (uint32_t i = 0; i < kRetryCount; i++) {
      bits -= uint64_t(OSUtils::getTickCount()) * 773703683;
      bits = ((bits >> 14) ^ (bits << 6)) + uint64_t(++internalCounter) * 10619863;

      bool useTmp = !ASMJIT_VM_SHM_DETECT || preferTmpOverDevShm;

      if (useTmp) {
        _tmpName.assign(getTmpDir());
        _tmpName.appendFormat(kShmFormat, (unsigned long long)bits);
        _fd = ::open(_tmpName.data(), O_RDWR | O_CREAT | O_EXCL, 0);
        if (ASMJIT_LIKELY(_fd >= 0)) {
          _fileType = kFileTypeTmp;
          return kErrorOk;
        }
      }
#if ASMJIT_VM_SHM_AVAILABLE
      else {
        _tmpName.assignFormat(kShmFormat, (unsigned long long)bits);
        _fd = ::shm_open(_tmpName.data(), O_RDWR | O_CREAT | O_EXCL, S_IRUSR | S_IWUSR);
        if (ASMJIT_LIKELY(_fd >= 0)) {
          _fileType = kFileTypeShm;
          return kErrorOk;
        }
      }
#endif

      int e = errno;
      if (e != EEXIST)
        return DebugUtils::errored(asmjitErrorFromErrno(e));
    }

    return DebugUtils::errored(kErrorFailedToOpenAnonymousMemory);
#endif
  }

  void unlink() noexcept {
    FileType type = _fileType;
    _fileType = kFileTypeNone;

#if ASMJIT_VM_SHM_AVAILABLE
    if (type == kFileTypeShm) {
      ::shm_unlink(_tmpName.data());
      return;
    }
#endif

    if (type == kFileTypeTmp) {
      ::unlink(_tmpName.data());
      return;
    }
  }

  void close() noexcept {
    if (_fd >= 0) {
      ::close(_fd);
      _fd = -1;
    }
  }

  Error allocate(size_t size) noexcept {
    // TODO: Improve this by using `posix_fallocate()` when available.
    if (ftruncate(_fd, off_t(size)) != 0)
      return DebugUtils::errored(asmjitErrorFromErrno(errno));

    return kErrorOk;
  }
};

// Returns `mmap()` protection flags from \ref MemoryFlags.
static int mmProtFromMemoryFlags(MemoryFlags memoryFlags) noexcept {
  int protection = 0;
  if (Support::test(memoryFlags, MemoryFlags::kAccessRead)) protection |= PROT_READ;
  if (Support::test(memoryFlags, MemoryFlags::kAccessWrite)) protection |= PROT_READ | PROT_WRITE;
  if (Support::test(memoryFlags, MemoryFlags::kAccessExecute)) protection |= PROT_READ | PROT_EXEC;
  return protection;
}

#if defined(__APPLE__)
// Detects whether the current process is hardened, which means that pages that have WRITE and EXECUTABLE flags cannot
// be allocated without MAP_JIT flag.
static inline bool hasHardenedRuntimeMacOS() noexcept {
#if TARGET_OS_OSX && ASMJIT_ARCH_ARM >= 64
  // MacOS on AArch64 has always hardened runtime enabled.
  return true;
#else
  static std::atomic<uint32_t> globalHardenedFlag;

  enum HardenedFlag : uint32_t {
    kHardenedFlagUnknown  = 0,
    kHardenedFlagDisabled = 1,
    kHardenedFlagEnabled  = 2
  };

  uint32_t flag = globalHardenedFlag.load();
  if (flag == kHardenedFlagUnknown) {
    size_t pageSize = ::getpagesize();

    void* ptr = mmap(nullptr, pageSize, PROT_WRITE | PROT_EXEC, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    if (ptr == MAP_FAILED) {
      flag = kHardenedFlagEnabled;
    }
    else {
      flag = kHardenedFlagDisabled;
      munmap(ptr, pageSize);
    }
    globalHardenedFlag.store(flag);
  }

  return flag == kHardenedFlagEnabled;
#endif
}

static inline bool hasMapJitSupportMacOS() noexcept {
#if TARGET_OS_OSX && ASMJIT_ARCH_ARM >= 64
  // MacOS for 64-bit AArch architecture always uses hardened runtime. Some documentation can be found here:
  //   - https://developer.apple.com/documentation/apple_silicon/porting_just-in-time_compilers_to_apple_silicon
  return true;
#elif TARGET_OS_OSX
  // MAP_JIT flag required to run unsigned JIT code is only supported by kernel version 10.14+ (Mojave) and IOS.
  static std::atomic<uint32_t> globalVersion;

  int ver = globalVersion.load();
  if (!ver) {
    struct utsname osname {};
    uname(&osname);
    ver = atoi(osname.release);
    globalVersion.store(ver);
  }
  return ver >= 18;
#else
  // Assume it's available.
  return true;
#endif
}
#endif // __APPLE__

// Detects whether the current process is hardened, which means that pages that have WRITE and EXECUTABLE flags
// cannot be normally allocated. On MacOS such allocation requires MAP_JIT flag.
static inline bool hasHardenedRuntime() noexcept {
#if defined(__APPLE__)
  return hasHardenedRuntimeMacOS();
#else
  return false;
#endif
}

// Detects whether MAP_JIT is available.
static inline bool hasMapJitSupport() noexcept {
#if defined(__APPLE__)
  return hasMapJitSupportMacOS();
#else
  return false;
#endif
}

// Returns either MAP_JIT or 0 based on `flags` and the host operating system.
static inline int mmMapJitFromMemoryFlags(MemoryFlags memoryFlags) noexcept {
#if defined(__APPLE__)
  // Always use MAP_JIT flag if user asked for it (could be used for testing on non-hardened processes) and detect
  // whether it must be used when the process is actually hardened (in that case it doesn't make sense to rely on
  // user `memoryFlags`).
  bool useMapJit = Support::test(memoryFlags, MemoryFlags::kMMapEnableMapJit) || hasHardenedRuntime();
  if (useMapJit)
    return hasMapJitSupport() ? int(MAP_JIT) : 0;
  else
    return 0;
#else
  DebugUtils::unused(memoryFlags);
  return 0;
#endif
}

// Returns BSD-specific `PROT_MAX()` flags.
static inline int mmMaxProtFromMemoryFlags(MemoryFlags memoryFlags) noexcept {
#if defined(PROT_MAX)
  static constexpr uint32_t kMaxProtShift = Support::ConstCTZ<uint32_t(MemoryFlags::kMMapMaxAccessRead)>::value;

  if (Support::test(memoryFlags, MemoryFlags::kMMapMaxAccessReadWrite | MemoryFlags::kMMapMaxAccessExecute))
    return PROT_MAX(mmProtFromMemoryFlags((MemoryFlags)(uint32_t(memoryFlags) >> kMaxProtShift)));
  else
    return 0;
#else
  DebugUtils::unused(memoryFlags);
  return 0;
#endif
}

#if ASMJIT_VM_SHM_DETECT
static Error detectShmStrategy(ShmStrategy* strategyOut) noexcept {
  AnonymousMemory anonMem;
  Info vmInfo = info();

  ASMJIT_PROPAGATE(anonMem.open(false));
  ASMJIT_PROPAGATE(anonMem.allocate(vmInfo.pageSize));

  void* ptr = mmap(nullptr, vmInfo.pageSize, PROT_READ | PROT_EXEC, MAP_SHARED, anonMem.fd(), 0);
  if (ptr == MAP_FAILED) {
    int e = errno;
    if (e == EINVAL) {
      *strategyOut = ShmStrategy::kTmpDir;
      return kErrorOk;
    }
    return DebugUtils::errored(asmjitErrorFromErrno(e));
  }
  else {
    munmap(ptr, vmInfo.pageSize);
    *strategyOut = ShmStrategy::kDevShm;
    return kErrorOk;
  }
}
#endif

static Error getShmStrategy(ShmStrategy* strategyOut) noexcept {
#if ASMJIT_VM_SHM_DETECT
  // Initially don't assume anything. It has to be tested whether '/dev/shm' was mounted with 'noexec' flag or not.
  static std::atomic<uint32_t> globalShmStrategy;

  ShmStrategy strategy = static_cast<ShmStrategy>(globalShmStrategy.load());
  if (strategy == ShmStrategy::kUnknown) {
    ASMJIT_PROPAGATE(detectShmStrategy(&strategy));
    globalShmStrategy.store(static_cast<uint32_t>(strategy));
  }

  *strategyOut = strategy;
  return kErrorOk;
#else
  *strategyOut = ShmStrategy::kTmpDir;
  return kErrorOk;
#endif
}

static HardenedRuntimeFlags getHardenedRuntimeFlags() noexcept {
  HardenedRuntimeFlags hrFlags = HardenedRuntimeFlags::kNone;

  if (hasHardenedRuntime())
    hrFlags |= HardenedRuntimeFlags::kEnabled;

  if (hasMapJitSupport())
    hrFlags |= HardenedRuntimeFlags::kMapJit;

  return hrFlags;
}

Error alloc(void** p, size_t size, MemoryFlags memoryFlags) noexcept {
  *p = nullptr;
  if (size == 0)
    return DebugUtils::errored(kErrorInvalidArgument);

  int protection = mmProtFromMemoryFlags(memoryFlags) | mmMaxProtFromMemoryFlags(memoryFlags);
  int mmFlags = MAP_PRIVATE | MAP_ANONYMOUS | mmMapJitFromMemoryFlags(memoryFlags);

  void* ptr = mmap(nullptr, size, protection, mmFlags, -1, 0);
  if (ptr == MAP_FAILED)
    return DebugUtils::errored(kErrorOutOfMemory);

  *p = ptr;
  return kErrorOk;
}

Error release(void* p, size_t size) noexcept {
  if (ASMJIT_UNLIKELY(munmap(p, size) != 0))
    return DebugUtils::errored(kErrorInvalidArgument);

  return kErrorOk;
}


Error protect(void* p, size_t size, MemoryFlags memoryFlags) noexcept {
  int protection = mmProtFromMemoryFlags(memoryFlags);
  if (mprotect(p, size, protection) == 0)
    return kErrorOk;

  return DebugUtils::errored(kErrorInvalidArgument);
}

Error allocDualMapping(DualMapping* dm, size_t size, MemoryFlags memoryFlags) noexcept {
  dm->rx = nullptr;
  dm->rw = nullptr;

  if (off_t(size) <= 0)
    return DebugUtils::errored(size == 0 ? kErrorInvalidArgument : kErrorTooLarge);

  bool preferTmpOverDevShm = Support::test(memoryFlags, MemoryFlags::kMappingPreferTmp);
  if (!preferTmpOverDevShm) {
    ShmStrategy strategy;
    ASMJIT_PROPAGATE(getShmStrategy(&strategy));
    preferTmpOverDevShm = (strategy == ShmStrategy::kTmpDir);
  }

  AnonymousMemory anonMem;
  ASMJIT_PROPAGATE(anonMem.open(preferTmpOverDevShm));
  ASMJIT_PROPAGATE(anonMem.allocate(size));

  void* ptr[2];
  for (uint32_t i = 0; i < 2; i++) {
    MemoryFlags accessFlags = memoryFlags & ~dualMappingFilter[i];
    int protection = mmProtFromMemoryFlags(accessFlags) | mmMaxProtFromMemoryFlags(accessFlags);

    ptr[i] = mmap(nullptr, size, protection, MAP_SHARED, anonMem.fd(), 0);
    if (ptr[i] == MAP_FAILED) {
      // Get the error now before `munmap()` has a chance to clobber it.
      int e = errno;
      if (i == 1)
        munmap(ptr[0], size);
      return DebugUtils::errored(asmjitErrorFromErrno(e));
    }
  }

  dm->rx = ptr[0];
  dm->rw = ptr[1];
  return kErrorOk;
}

Error releaseDualMapping(DualMapping* dm, size_t size) noexcept {
  Error err = release(dm->rx, size);
  if (dm->rx != dm->rw)
    err |= release(dm->rw, size);

  if (err)
    return DebugUtils::errored(kErrorInvalidArgument);

  dm->rx = nullptr;
  dm->rw = nullptr;
  return kErrorOk;
}
#endif

// Virtual Memory - Flush Instruction Cache
// ========================================

void flushInstructionCache(void* p, size_t size) noexcept {
#if ASMJIT_ARCH_X86
  // X86/X86_64 architecture doesn't require to do anything to flush instruction cache.
  DebugUtils::unused(p, size);
#elif defined(__APPLE__)
  sys_icache_invalidate(p, size);
#elif defined(_WIN32)
  // Windows has a built-in support in `kernel32.dll`.
  FlushInstructionCache(GetCurrentProcess(), p, size);
#elif defined(__GNUC__)
  char* start = static_cast<char*>(p);
  char* end = start + size;
  __builtin___clear_cache(start, end);
#else
  #pragma message("asmjit::VirtMem::flushInstructionCache() doesn't have implementation for the target OS and compiler")
  DebugUtils::unused(p, size);
#endif
}

// Virtual Memory - Memory Info
// ============================

Info info() noexcept {
  static std::atomic<uint32_t> vmInfoInitialized;
  static Info vmInfo;

  if (!vmInfoInitialized.load()) {
    Info localMemInfo;
    getVMInfo(localMemInfo);

    vmInfo = localMemInfo;
    vmInfoInitialized.store(1u);
  }

  return vmInfo;
}

// Virtual Memory - Hardened Runtime Info
// ======================================

HardenedRuntimeInfo hardenedRuntimeInfo() noexcept {
  return HardenedRuntimeInfo { getHardenedRuntimeFlags() };
}

// Virtual Memory - Project JIT Memory
// ===================================

void protectJitMemory(ProtectJitAccess access) noexcept {
#if defined(ASMJIT_HAS_PTHREAD_JIT_WRITE_PROTECT_NP)
  pthread_jit_write_protect_np(static_cast<uint32_t>(access));
#else
  DebugUtils::unused(access);
#endif
}

ASMJIT_END_SUB_NAMESPACE

#endif
