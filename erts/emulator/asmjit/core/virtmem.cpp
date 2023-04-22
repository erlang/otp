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

  #define ASMJIT_ANONYMOUS_MEMORY_USE_FD

  #if defined(__APPLE__) || defined(__BIONIC__)
    #define ASMJIT_VM_SHM_DETECT 0
  #else
    #define ASMJIT_VM_SHM_DETECT 1
  #endif

  // Android NDK doesn't provide `shm_open()` and `shm_unlink()`.
  #if !defined(__BIONIC__)
    #define ASMJIT_HAS_SHM_OPEN_AND_UNLINK
  #endif

  #if defined(__APPLE__) && TARGET_OS_OSX && ASMJIT_ARCH_ARM >= 64
    #define ASMJIT_HAS_PTHREAD_JIT_WRITE_PROTECT_NP
  #endif

  #if defined(__NetBSD__) && defined(MAP_REMAPDUP) && defined(PROT_MPROTECT)
    #undef ASMJIT_ANONYMOUS_MEMORY_USE_FD
    #define ASMJIT_ANONYMOUS_MEMORY_USE_REMAPDUP
  #endif
#endif

#include <atomic>

ASMJIT_BEGIN_SUB_NAMESPACE(VirtMem)

// Virtual Memory Utilities
// ========================

ASMJIT_MAYBE_UNUSED
static const constexpr MemoryFlags dualMappingFilter[2] = {
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

// Virtual Memory [Posix] - Utilities
// ==================================

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

static void getVMInfo(Info& vmInfo) noexcept {
  uint32_t pageSize = uint32_t(::getpagesize());

  vmInfo.pageSize = pageSize;
  vmInfo.pageGranularity = Support::max<uint32_t>(pageSize, 65536);
}

#if defined(__APPLE__) && TARGET_OS_OSX
static int getOSXVersion() noexcept {
  // MAP_JIT flag required to run unsigned JIT code is only supported by kernel version 10.14+ (Mojave).
  static std::atomic<int> globalVersion;

  int ver = globalVersion.load();
  if (!ver) {
    struct utsname osname {};
    uname(&osname);
    ver = atoi(osname.release);
    globalVersion.store(ver);
  }

  return ver;
}
#endif // __APPLE__ && TARGET_OS_OSX

// Returns `mmap()` protection flags from \ref MemoryFlags.
static int mmProtFromMemoryFlags(MemoryFlags memoryFlags) noexcept {
  int protection = 0;
  if (Support::test(memoryFlags, MemoryFlags::kAccessRead)) protection |= PROT_READ;
  if (Support::test(memoryFlags, MemoryFlags::kAccessWrite)) protection |= PROT_READ | PROT_WRITE;
  if (Support::test(memoryFlags, MemoryFlags::kAccessExecute)) protection |= PROT_READ | PROT_EXEC;
  return protection;
}

// Virtual Memory [Posix] - Anonymus Memory
// ========================================

#if defined(ASMJIT_ANONYMOUS_MEMORY_USE_FD)

// Some operating systems don't allow /dev/shm to be executable. On Linux this happens when /dev/shm is mounted with
// 'noexec', which is enforced by systemd. Other operating systems like MacOS also restrict executable permissions
// regarding /dev/shm, so we use a runtime detection before attempting to allocate executable memory. Sometimes we
// don't need the detection as we know it would always result in `AnonymousMemoryStrategy::kTmpDir`.
enum class AnonymousMemoryStrategy : uint32_t {
  kUnknown = 0,
  kDevShm = 1,
  kTmpDir = 2
};

#if !defined(SHM_ANON)
static const char* getTmpDir() noexcept {
  const char* tmpDir = getenv("TMPDIR");
  return tmpDir ? tmpDir : "/tmp";
}
#endif

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
    //
    // NOTE: There is also memfd_create() libc function in FreeBSD, but it internally
    // uses `shm_open(SHM_ANON, ...)` so it's not needed to add support for it (it's
    // not a syscall as in Linux).

    // Zero initialized, if ever changed to '1' that would mean the syscall is not
    // available and we must use `shm_open()` and `shm_unlink()` (or regular `open()`).
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
#ifdef ASMJIT_HAS_SHM_OPEN_AND_UNLINK
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

#ifdef ASMJIT_HAS_SHM_OPEN_AND_UNLINK
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

#if ASMJIT_VM_SHM_DETECT
static Error detectAnonymousMemoryStrategy(AnonymousMemoryStrategy* strategyOut) noexcept {
  AnonymousMemory anonMem;
  Info vmInfo = info();

  ASMJIT_PROPAGATE(anonMem.open(false));
  ASMJIT_PROPAGATE(anonMem.allocate(vmInfo.pageSize));

  void* ptr = mmap(nullptr, vmInfo.pageSize, PROT_READ | PROT_EXEC, MAP_SHARED, anonMem.fd(), 0);
  if (ptr == MAP_FAILED) {
    int e = errno;
    if (e == EINVAL) {
      *strategyOut = AnonymousMemoryStrategy::kTmpDir;
      return kErrorOk;
    }
    return DebugUtils::errored(asmjitErrorFromErrno(e));
  }
  else {
    munmap(ptr, vmInfo.pageSize);
    *strategyOut = AnonymousMemoryStrategy::kDevShm;
    return kErrorOk;
  }
}
#endif

static Error getAnonymousMemoryStrategy(AnonymousMemoryStrategy* strategyOut) noexcept {
#if ASMJIT_VM_SHM_DETECT
  // Initially don't assume anything. It has to be tested whether '/dev/shm' was mounted with 'noexec' flag or not.
  static std::atomic<uint32_t> globalStrategy;

  AnonymousMemoryStrategy strategy = static_cast<AnonymousMemoryStrategy>(globalStrategy.load());
  if (strategy == AnonymousMemoryStrategy::kUnknown) {
    ASMJIT_PROPAGATE(detectAnonymousMemoryStrategy(&strategy));
    globalStrategy.store(static_cast<uint32_t>(strategy));
  }

  *strategyOut = strategy;
  return kErrorOk;
#else
  *strategyOut = AnonymousMemoryStrategy::kTmpDir;
  return kErrorOk;
#endif
}

#endif // ASMJIT_ANONYMOUS_MEMORY_USE_FD

// Virtual Memory [Posix] - Hardened Runtime & MAP_JIT
// ===================================================

// Detects whether the current process is hardened, which means that pages that have WRITE and EXECUTABLE flags
// cannot be normally allocated. On OSX + AArch64 such allocation requires MAP_JIT flag, other platforms don't
// support this combination.
static bool hasHardenedRuntime() noexcept {
#if defined(__APPLE__) && TARGET_OS_OSX && ASMJIT_ARCH_ARM >= 64
  // OSX on AArch64 has always hardened runtime enabled.
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
    uint32_t pageSize = uint32_t(::getpagesize());

    void* ptr = mmap(nullptr, pageSize, PROT_READ | PROT_WRITE | PROT_EXEC, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
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

// Detects whether MAP_JIT is available.
static inline bool hasMapJitSupport() noexcept {
#if defined(__APPLE__) && TARGET_OS_OSX && ASMJIT_ARCH_ARM >= 64
  // OSX on AArch64 always uses hardened runtime + MAP_JIT:
  //   - https://developer.apple.com/documentation/apple_silicon/porting_just-in-time_compilers_to_apple_silicon
  return true;
#elif defined(__APPLE__) && TARGET_OS_OSX
  return getOSXVersion() >= 18;
#else
  // MAP_JIT is not available (it's only available on OSX).
  return false;
#endif
}

// Returns either MAP_JIT or 0 based on `flags` and the host operating system.
static inline int mmMapJitFromMemoryFlags(MemoryFlags memoryFlags) noexcept {
#if defined(__APPLE__)
  // Always use MAP_JIT flag if user asked for it (could be used for testing on non-hardened processes) and detect
  // whether it must be used when the process is actually hardened (in that case it doesn't make sense to rely on
  // user `memoryFlags`).
  bool useMapJit = (Support::test(memoryFlags, MemoryFlags::kMMapEnableMapJit) || hasHardenedRuntime())
    && !Support::test(memoryFlags, MemoryFlags::kMapShared);
  if (useMapJit)
    return hasMapJitSupport() ? int(MAP_JIT) : 0;
  else
    return 0;
#else
  DebugUtils::unused(memoryFlags);
  return 0;
#endif
}

ASMJIT_MAYBE_UNUSED
static MemoryFlags maxAccessFlagsToRegularAccessFlags(MemoryFlags memoryFlags) noexcept {
  static constexpr uint32_t kMaxProtShift = Support::ConstCTZ<uint32_t(MemoryFlags::kMMapMaxAccessRead)>::value;
  return MemoryFlags(uint32_t(memoryFlags & MemoryFlags::kMMapMaxAccessRWX) >> kMaxProtShift);
}

ASMJIT_MAYBE_UNUSED
static MemoryFlags regularAccessFlagsToMaxAccessFlags(MemoryFlags memoryFlags) noexcept {
  static constexpr uint32_t kMaxProtShift = Support::ConstCTZ<uint32_t(MemoryFlags::kMMapMaxAccessRead)>::value;
  return MemoryFlags(uint32_t(memoryFlags & MemoryFlags::kAccessRWX) << kMaxProtShift);
}

// Returns maximum protection flags from `memoryFlags`.
//
// Uses:
//   - `PROT_MPROTECT()` on NetBSD.
//   - `PROT_MAX()` when available(BSD).
ASMJIT_MAYBE_UNUSED
static inline int mmMaxProtFromMemoryFlags(MemoryFlags memoryFlags) noexcept {
  MemoryFlags acc = maxAccessFlagsToRegularAccessFlags(memoryFlags);
  if (acc != MemoryFlags::kNone) {
#if defined(__NetBSD__) && defined(PROT_MPROTECT)
    return PROT_MPROTECT(mmProtFromMemoryFlags(acc));
#elif defined(PROT_MAX)
    return PROT_MAX(mmProtFromMemoryFlags(acc));
#else
    return 0;
#endif
  }

  return 0;
}

static HardenedRuntimeFlags getHardenedRuntimeFlags() noexcept {
  HardenedRuntimeFlags flags = HardenedRuntimeFlags::kNone;

  if (hasHardenedRuntime())
    flags |= HardenedRuntimeFlags::kEnabled;

  if (hasMapJitSupport())
    flags |= HardenedRuntimeFlags::kMapJit;

  return flags;
}

static Error mapMemory(void** p, size_t size, MemoryFlags memoryFlags, int fd = -1, off_t offset = 0) noexcept {
  *p = nullptr;
  if (size == 0)
    return DebugUtils::errored(kErrorInvalidArgument);

  int protection = mmProtFromMemoryFlags(memoryFlags) | mmMaxProtFromMemoryFlags(memoryFlags);
  int mmFlags = mmMapJitFromMemoryFlags(memoryFlags);

  mmFlags |= Support::test(memoryFlags, MemoryFlags::kMapShared) ? MAP_SHARED : MAP_PRIVATE;
  if (fd == -1)
    mmFlags |= MAP_ANONYMOUS;

  void* ptr = mmap(nullptr, size, protection, mmFlags, fd, offset);
  if (ptr == MAP_FAILED)
    return DebugUtils::errored(asmjitErrorFromErrno(errno));

  *p = ptr;
  return kErrorOk;
}

static Error unmapMemory(void* p, size_t size) noexcept {
  if (ASMJIT_UNLIKELY(munmap(p, size) != 0))
    return DebugUtils::errored(asmjitErrorFromErrno(errno));

  return kErrorOk;
}

Error alloc(void** p, size_t size, MemoryFlags memoryFlags) noexcept {
  return mapMemory(p, size, memoryFlags);
}

Error release(void* p, size_t size) noexcept {
  return unmapMemory(p, size);
}

Error protect(void* p, size_t size, MemoryFlags memoryFlags) noexcept {
  int protection = mmProtFromMemoryFlags(memoryFlags);
  if (mprotect(p, size, protection) == 0)
    return kErrorOk;

  return DebugUtils::errored(asmjitErrorFromErrno(errno));
}

// Virtual Memory [Posix] - Dual Mapping
// =====================================

static Error unmapDualMapping(DualMapping* dm, size_t size) noexcept {
  Error err1 = unmapMemory(dm->rx, size);
  Error err2 = kErrorOk;

  if (dm->rx != dm->rw)
    err2 = unmapMemory(dm->rw, size);

  // We can report only one error, so report the first...
  if (err1 || err2)
    return DebugUtils::errored(err1 ? err1 : err2);

  dm->rx = nullptr;
  dm->rw = nullptr;
  return kErrorOk;
}

#if defined(ASMJIT_ANONYMOUS_MEMORY_USE_REMAPDUP)
static Error allocDualMappingUsingRemapdup(DualMapping* dmOut, size_t size, MemoryFlags memoryFlags) noexcept {
  MemoryFlags maxAccessFlags = regularAccessFlagsToMaxAccessFlags(memoryFlags);
  MemoryFlags finalFlags = memoryFlags | maxAccessFlags | MemoryFlags::kMapShared;

  MemoryFlags rxFlags = finalFlags & ~(MemoryFlags::kAccessWrite | MemoryFlags::kMMapMaxAccessWrite);
  MemoryFlags rwFlags = finalFlags & ~(MemoryFlags::kAccessExecute);

  // Allocate RW mapping.
  DualMapping dm {};
  ASMJIT_PROPAGATE(mapMemory(&dm.rw, size, rwFlags));

  // Allocate RX mapping.
  dm.rx = mremap(dm.rw, size, nullptr, size, MAP_REMAPDUP);
  if (dm.rx == MAP_FAILED) {
    int e = errno;
    munmap(dm.rw, size);
    return DebugUtils::errored(asmjitErrorFromErrno(e));
  }

  if (mprotect(dm.rx, size, mmProtFromMemoryFlags(rxFlags)) != 0) {
    int e = errno;
    unmapDualMapping(&dm, size);
    return DebugUtils::errored(asmjitErrorFromErrno(e));
  }

  *dmOut = dm;
  return kErrorOk;
}
#endif

Error allocDualMapping(DualMapping* dm, size_t size, MemoryFlags memoryFlags) noexcept {
  dm->rx = nullptr;
  dm->rw = nullptr;

  if (off_t(size) <= 0)
    return DebugUtils::errored(size == 0 ? kErrorInvalidArgument : kErrorTooLarge);

#if defined(ASMJIT_ANONYMOUS_MEMORY_USE_REMAPDUP)
  return allocDualMappingUsingRemapdup(dm, size, memoryFlags);
#elif defined(ASMJIT_ANONYMOUS_MEMORY_USE_FD)
  bool preferTmpOverDevShm = Support::test(memoryFlags, MemoryFlags::kMappingPreferTmp);
  if (!preferTmpOverDevShm) {
    AnonymousMemoryStrategy strategy;
    ASMJIT_PROPAGATE(getAnonymousMemoryStrategy(&strategy));
    preferTmpOverDevShm = (strategy == AnonymousMemoryStrategy::kTmpDir);
  }

  AnonymousMemory anonMem;
  ASMJIT_PROPAGATE(anonMem.open(preferTmpOverDevShm));
  ASMJIT_PROPAGATE(anonMem.allocate(size));

  void* ptr[2];
  for (uint32_t i = 0; i < 2; i++) {
    MemoryFlags restrictedMemoryFlags = memoryFlags & ~dualMappingFilter[i];
    Error err = mapMemory(&ptr[i], size, restrictedMemoryFlags | MemoryFlags::kMapShared, anonMem.fd(), 0);
    if (err != kErrorOk) {
      if (i == 1)
        unmapMemory(ptr[0], size);
      return err;
    }
  }

  dm->rx = ptr[0];
  dm->rw = ptr[1];
  return kErrorOk;
#else
  #error "[asmjit] VirtMem::allocDualMapping() has no implementation"
#endif
}

Error releaseDualMapping(DualMapping* dm, size_t size) noexcept {
  return unmapDualMapping(dm, size);
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
  #pragma message("[asmjit] VirtMem::flushInstructionCache() doesn't have implementation for the target OS and compiler")
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
  pthread_jit_write_protect_np(static_cast<int>(access));
#else
  DebugUtils::unused(access);
#endif
}

ASMJIT_END_SUB_NAMESPACE

// JitAllocator - Tests
// ====================

#if defined(ASMJIT_TEST)
ASMJIT_BEGIN_NAMESPACE

UNIT(virt_mem) {
  VirtMem::Info vmInfo = VirtMem::info();

  INFO("VirtMem::info():");
  INFO("  pageSize: %zu", size_t(vmInfo.pageSize));
  INFO("  pageGranularity: %zu", size_t(vmInfo.pageGranularity));

  VirtMem::HardenedRuntimeInfo hardenedRtInfo = VirtMem::hardenedRuntimeInfo();
  VirtMem::HardenedRuntimeFlags hardenedFlags = hardenedRtInfo.flags;

  INFO("VirtMem::hardenedRuntimeInfo():");
  INFO("  flags:");
  INFO("    kEnabled: %s", Support::test(hardenedFlags, VirtMem::HardenedRuntimeFlags::kEnabled) ? "true" : "false");
  INFO("    kMapJit: %s", Support::test(hardenedFlags, VirtMem::HardenedRuntimeFlags::kMapJit) ? "true" : "false");
}

ASMJIT_END_NAMESPACE
#endif // ASMJIT_TEST

#endif // !ASMJIT_NO_JIT
