// This file is part of AsmJit project <https://asmjit.com>
//
// See asmjit.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#include "../core/api-build_p.h"
#ifndef ASMJIT_NO_JIT

#include "../core/osutils_p.h"
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

  #if !ASMJIT_ARCH_X86
    #include <sys/time.h> // required by gettimeofday()
  #endif

  // Linux has a `memfd_create` syscall that we would like to use, if available.
  #if defined(__linux__)
    #include <sys/syscall.h>
    #include <sys/utsname.h>

    #ifndef MAP_HUGETLB
      #define MAP_HUGETLB 0x40000
    #endif // MAP_HUGETLB

    #ifndef MAP_HUGE_SHIFT
      #define MAP_HUGE_SHIFT 26
    #endif // MAP_HUGE_SHIFT

    #if !defined(MFD_CLOEXEC)
      #define MFD_CLOEXEC 0x0001u
    #endif // MFD_CLOEXEC

    #if !defined(MFD_NOEXEC_SEAL)
      #define MFD_NOEXEC_SEAL 0x0008u
    #endif // MFD_NOEXEC_SEAL

    #if !defined(MFD_EXEC)
      #define MFD_EXEC 0x0010u
    #endif // MFD_EXEC

    #ifndef MFD_HUGETLB
      #define MFD_HUGETLB 0x0004
    #endif // MFD_HUGETLB

    #ifndef MFD_HUGE_SHIFT
      #define MFD_HUGE_SHIFT 26
    #endif // MFD_HUGE_SHIFT
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

  // Android NDK doesn't provide `shm_open()` and `shm_unlink()`.
  #if !defined(__BIONIC__) && !defined(ASMJIT_NO_SHM_OPEN)
    #define ASMJIT_HAS_SHM_OPEN
  #endif

  #if defined(__APPLE__) || defined(__BIONIC__) || !defined(ASMJIT_HAS_SHM_OPEN)
    #define ASMJIT_VM_SHM_DETECT 0
  #else
    #define ASMJIT_VM_SHM_DETECT 1
  #endif

  #if defined(__APPLE__) && TARGET_OS_OSX
    #if ASMJIT_ARCH_X86 != 0
      #define ASMJIT_ANONYMOUS_MEMORY_USE_MACH_VM_REMAP
    #endif
    #if ASMJIT_ARCH_ARM >= 64
      #define ASMJIT_HAS_PTHREAD_JIT_WRITE_PROTECT_NP
    #endif
  #endif

  #if defined(__APPLE__) && ASMJIT_ARCH_X86 == 0
    #define ASMJIT_NO_DUAL_MAPPING
  #endif

  #if defined(__NetBSD__) && defined(MAP_REMAPDUP) && defined(PROT_MPROTECT)
    #define ASMJIT_ANONYMOUS_MEMORY_USE_REMAPDUP
  #endif

  #if !defined(ASMJIT_ANONYMOUS_MEMORY_USE_REMAPDUP) && \
      !defined(ASMJIT_ANONYMOUS_MEMORY_USE_MACH_VM_REMAP) && \
      !defined(ASMJIT_NO_DUAL_MAPPING)
    #define ASMJIT_ANONYMOUS_MEMORY_USE_FD
  #endif
#endif

#include <atomic>

#if defined(ASMJIT_ANONYMOUS_MEMORY_USE_MACH_VM_REMAP)
#include <mach/mach.h>
#include <mach/mach_time.h>

extern "C" {

#ifdef mig_external
mig_external
#else
extern
#endif
kern_return_t mach_vm_remap(
  vm_map_t target_task,
  mach_vm_address_t *target_address,
  mach_vm_size_t size,
  mach_vm_offset_t mask,
  int flags,
  vm_map_t src_task,
  mach_vm_address_t src_address,
  boolean_t copy,
  vm_prot_t *cur_protection,
  vm_prot_t *max_protection,
  vm_inherit_t inheritance
);

} // {extern "C"}
#endif

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

static void detectVMInfo(Info& vmInfo) noexcept {
  SYSTEM_INFO systemInfo;

  ::GetSystemInfo(&systemInfo);
  vmInfo.pageSize = Support::alignUpPowerOf2<uint32_t>(systemInfo.dwPageSize);
  vmInfo.pageGranularity = systemInfo.dwAllocationGranularity;
}

static size_t detectLargePageSize() noexcept {
  return ::GetLargePageMinimum();
}

static bool hasDualMappingSupport() noexcept {
  // TODO: This assumption works on X86 platforms, this may not work on AArch64.
  return true;
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
  HardenedRuntimeFlags flags = HardenedRuntimeFlags::kNone;

  if (hasDualMappingSupport())
    flags |= HardenedRuntimeFlags::kDualMapping;

  return flags;
}

Error alloc(void** p, size_t size, MemoryFlags memoryFlags) noexcept {
  *p = nullptr;
  if (size == 0)
    return DebugUtils::errored(kErrorInvalidArgument);

  DWORD allocationType = MEM_COMMIT | MEM_RESERVE;
  DWORD protectFlags = protectFlagsFromMemoryFlags(memoryFlags);

  if (Support::test(memoryFlags, MemoryFlags::kMMapLargePages)) {
    size_t lpSize = largePageSize();

    // Does it make sense to call VirtualAlloc() if we failed to query large page size?
    if (lpSize == 0)
      return DebugUtils::errored(kErrorFeatureNotEnabled);

    if (!Support::isAligned(size, lpSize))
      return DebugUtils::errored(kErrorInvalidArgument);

    allocationType |= MEM_LARGE_PAGES;
  }

  void* result = ::VirtualAlloc(nullptr, size, allocationType, protectFlags);
  if (!result)
    return DebugUtils::errored(kErrorOutOfMemory);

  *p = result;
  return kErrorOk;
}

Error release(void* p, size_t size) noexcept {
  DebugUtils::unused(size);
  // NOTE: If the `dwFreeType` parameter is MEM_RELEASE, `size` parameter must be zero.
  constexpr DWORD dwFreeType = MEM_RELEASE;
  if (ASMJIT_UNLIKELY(!::VirtualFree(p, 0, dwFreeType)))
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
      if (i == 1u)
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

// Virtual Memory [Unix]
// =====================

#if !defined(_WIN32)

// Virtual Memory [Unix] - Utilities
// =================================

#if defined(__linux__) || (defined(__APPLE__) && TARGET_OS_OSX)
struct KernelVersion {
  long ver[2];

  inline long major() const noexcept { return ver[0]; }
  inline long minor() const noexcept { return ver[1]; }

  inline bool eq(long major, long minor) const noexcept { return ver[0] == major && ver[1] == minor; }
  inline bool ge(long major, long minor) const noexcept { return ver[0] > major || (ver[0] == major && ver[1] >= minor); }
};

ASMJIT_MAYBE_UNUSED
static KernelVersion getKernelVersion() noexcept {
  KernelVersion out {};
  struct utsname buf {};

  uname(&buf);

  size_t i = 0;
  char* p = buf.release;

  while (*p && i < 2u) {
    uint8_t c = uint8_t(*p);
    if (c >= uint8_t('0') && c <= uint8_t('9')) {
      out.ver[i] = strtol(p, &p, 10);
      i++;
      continue;
    }

    p++;
  }

  return out;
}
#endif // getKernelVersion

// Translates libc errors specific to VirtualMemory mapping to `asmjit::Error`.
ASMJIT_MAYBE_UNUSED
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

// Returns `mmap()` protection flags from \ref MemoryFlags.
ASMJIT_MAYBE_UNUSED
static int mmProtFromMemoryFlags(MemoryFlags memoryFlags) noexcept {
  int protection = 0;
  if (Support::test(memoryFlags, MemoryFlags::kAccessRead)) protection |= PROT_READ;
  if (Support::test(memoryFlags, MemoryFlags::kAccessWrite)) protection |= PROT_READ | PROT_WRITE;
  if (Support::test(memoryFlags, MemoryFlags::kAccessExecute)) protection |= PROT_READ | PROT_EXEC;
  return protection;
}

// Returns maximum protection flags from `memoryFlags`.
//
// Uses:
//   - `PROT_MPROTECT()` on NetBSD.
//   - `PROT_MAX()` when available on other BSDs.
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

static void detectVMInfo(Info& vmInfo) noexcept {
  uint32_t pageSize = uint32_t(::getpagesize());

  vmInfo.pageSize = pageSize;
  vmInfo.pageGranularity = Support::max<uint32_t>(pageSize, 65536);
}

static size_t detectLargePageSize() noexcept {
#if defined(__APPLE__) && defined(VM_FLAGS_SUPERPAGE_SIZE_2MB) && ASMJIT_ARCH_X86
  return 2u * 1024u * 1024u;
#elif defined(__FreeBSD__)
  Support::Array<size_t, 2> pageSize;
  // TODO: Does it return unsigned?
  return (getpagesizes(pageSize.data(), 2) < 2) ? 0 : uint32_t(pageSize[1]);
#elif defined(__linux__)
  StringTmp<128> storage;
  if (OSUtils::readFile("/sys/kernel/mm/transparent_hugepage/hpage_pmd_size", storage, 16) != kErrorOk || storage.empty())
    return 0u;

  // The first value should be the size of the page (hpage_pmd_size).
  size_t largePageSize = 0;

  const char* buf = storage.data();
  size_t bufSize = storage.size();

  for (size_t i = 0; i < bufSize; i++) {
    uint32_t digit = uint32_t(uint8_t(buf[i]) - uint8_t('0'));
    if (digit >= 10u)
      break;
    largePageSize = largePageSize * 10 + digit;
  }

  if (Support::isPowerOf2(largePageSize))
    return largePageSize;
  else
    return 0u;
#else
  return 0u;
#endif
}

// Virtual Memory [Posix] - Anonymous Memory
// =========================================

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

#if defined(__linux__) && defined(__NR_memfd_create)
static uint32_t getMfdExecFlag() noexcept {
  static std::atomic<uint32_t> cachedMfdExecSupported;
  uint32_t val = cachedMfdExecSupported.load();

  if (val == 0u) {
    KernelVersion ver = getKernelVersion();
    val = uint32_t(ver.ge(6, 3)) + 1u;
    cachedMfdExecSupported.store(val);
  }

  return val == 2u ? uint32_t(MFD_EXEC) : uint32_t(0u);
}
#endif // __linux__ && __NR_memfd_create


// It's not fully random, just to avoid collisions when opening TMP or SHM file.
ASMJIT_MAYBE_UNUSED
static uint64_t generateRandomBits(uintptr_t stackPtr, uint32_t attempt) noexcept {
  static std::atomic<uint32_t> internalCounter;

#if defined(__GNUC__) && ASMJIT_ARCH_X86
  // Use RDTSC instruction to avoid gettimeofday() as we just need some "random" bits.
  uint64_t mix = __builtin_ia32_rdtsc();
#else
  struct timeval tm {};
  uint64_t mix = 1; // only used when gettimeofday() fails, which is unlikely.
  if (gettimeofday(&tm, nullptr) == 0) {
    mix = uint64_t(tm.tv_usec) ^ uint64_t(tm.tv_sec);
  }
#endif

  uint64_t bits = (uint64_t(stackPtr) & 0x1010505000055590u) - mix * 773703683;
  bits = (bits >> 33) ^ (bits << 7) ^ (attempt * 87178291199);
  return bits + uint64_t(++internalCounter) * 10619863;
}

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
      _fd = (int)syscall(__NR_memfd_create, "vmem", MFD_CLOEXEC | getMfdExecFlag());
      if (ASMJIT_LIKELY(_fd >= 0))
        return kErrorOk;

      int e = errno;
      if (e == ENOSYS)
        memfd_create_not_supported = 1;
      else
        return DebugUtils::errored(asmjitErrorFromErrno(e));
    }
#endif // __linux__ && __NR_memfd_create

#if defined(ASMJIT_HAS_SHM_OPEN) && defined(SHM_ANON)
    // Originally FreeBSD extension, apparently works in other BSDs too.
    DebugUtils::unused(preferTmpOverDevShm);
    _fd = ::shm_open(SHM_ANON, O_RDWR | O_CREAT | O_EXCL, S_IRUSR | S_IWUSR);

    if (ASMJIT_LIKELY(_fd >= 0))
      return kErrorOk;
    else
      return DebugUtils::errored(asmjitErrorFromErrno(errno));
#else
    // POSIX API. We have to generate somehow a unique name, so use `generateRandomBits()` helper. To prevent
    // having file collisions we use `shm_open()` with flags that require creation of the file so we never open
    // an existing shared memory.
    static const char kShmFormat[] = "/shm-id-%016llX";
    uint32_t kRetryCount = 100;

    for (uint32_t i = 0; i < kRetryCount; i++) {
      bool useTmp = !ASMJIT_VM_SHM_DETECT || preferTmpOverDevShm;
      uint64_t bits = generateRandomBits((uintptr_t)this, i);

      if (useTmp) {
        _tmpName.assign(getTmpDir());
        _tmpName.appendFormat(kShmFormat, (unsigned long long)bits);
        _fd = ASMJIT_FILE64_API(::open)(_tmpName.data(), O_RDWR | O_CREAT | O_EXCL, 0);
        if (ASMJIT_LIKELY(_fd >= 0)) {
          _fileType = kFileTypeTmp;
          return kErrorOk;
        }
      }
#if defined(ASMJIT_HAS_SHM_OPEN)
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

#ifdef ASMJIT_HAS_SHM_OPEN
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
    if (ASMJIT_FILE64_API(ftruncate)(_fd, off_t(size)) != 0)
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
  static std::atomic<uint32_t> cachedStrategy;

  AnonymousMemoryStrategy strategy = static_cast<AnonymousMemoryStrategy>(cachedStrategy.load());
  if (strategy == AnonymousMemoryStrategy::kUnknown) {
    ASMJIT_PROPAGATE(detectAnonymousMemoryStrategy(&strategy));
    cachedStrategy.store(static_cast<uint32_t>(strategy));
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
  static std::atomic<uint32_t> cachedHardenedFlag;

  enum HardenedFlag : uint32_t {
    kHardenedFlagUnknown  = 0,
    kHardenedFlagDisabled = 1,
    kHardenedFlagEnabled  = 2
  };

  uint32_t flag = cachedHardenedFlag.load();
  if (flag == kHardenedFlagUnknown) {
    size_t pageSize = size_t(::getpagesize());
    void* ptr = mmap(nullptr, pageSize, PROT_READ | PROT_WRITE | PROT_EXEC, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);

    if (ptr == MAP_FAILED) {
      flag = kHardenedFlagEnabled;
    }
    else {
      flag = kHardenedFlagDisabled;
      munmap(ptr, pageSize);
    }

    cachedHardenedFlag.store(flag);
  }

  return flag == kHardenedFlagEnabled;
#endif
}

// Detects whether MAP_JIT is available.
static inline bool hasMapJitSupport() noexcept {
#if defined(__APPLE__) && TARGET_OS_OSX && ASMJIT_ARCH_X86 == 0
  // Apple platforms always use hardened runtime + MAP_JIT on non-x86 hardware:
  //   - https://developer.apple.com/documentation/apple_silicon/porting_just-in-time_compilers_to_apple_silicon
  return true;
#elif defined(__APPLE__) && TARGET_OS_OSX
  // MAP_JIT flag required to run unsigned JIT code is only supported by kernel version 10.14+ (Mojave).
  static std::atomic<uint32_t> cachedMapJitSupport;
  uint32_t val = cachedMapJitSupport.load();

  if (val == 0u) {
    KernelVersion ver = getKernelVersion();
    val = uint32_t(ver.ge(18, 0)) + 1u;
    cachedMapJitSupport.store(val);
  }

  return val == 2u;
#else
  // MAP_JIT is not available (it's only available on OSX).
  return false;
#endif
}

// Returns either MAP_JIT or 0 based on `memoryFlags` and the host operating system.
static inline int mmMapJitFromMemoryFlags(MemoryFlags memoryFlags) noexcept {
#if defined(__APPLE__)
  // Always use MAP_JIT flag if user asked for it (could be used for testing on non-hardened processes) and detect
  // whether it must be used when the process is actually hardened (in that case it doesn't make sense to rely on
  // user `memoryFlags`).
  //
  // MAP_JIT is not required when dual-mapping memory and is incompatible with MAP_SHARED, so it will not be
  // added when the latter is enabled.
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

static inline bool hasDualMappingSupport() noexcept {
#if defined(ASMJIT_NO_DUAL_MAPPING)
  return false;
#else
  return true;
#endif
}

static HardenedRuntimeFlags getHardenedRuntimeFlags() noexcept {
  HardenedRuntimeFlags flags = HardenedRuntimeFlags::kNone;

  if (hasHardenedRuntime())
    flags |= HardenedRuntimeFlags::kEnabled;

  if (hasMapJitSupport())
    flags |= HardenedRuntimeFlags::kMapJit;

  if (hasDualMappingSupport())
    flags |= HardenedRuntimeFlags::kDualMapping;

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

  bool useLargePages = Support::test(memoryFlags, VirtMem::MemoryFlags::kMMapLargePages);

  if (useLargePages) {
#if defined(__linux__)
    size_t lpSize = largePageSize();
    if (lpSize == 0)
      return DebugUtils::errored(kErrorFeatureNotEnabled);

    if (!Support::isAligned(size, lpSize))
      return DebugUtils::errored(kErrorInvalidArgument);

    unsigned lpSizeLog2 = Support::ctz(lpSize);
    mmFlags |= int(unsigned(MAP_HUGETLB) | (lpSizeLog2 << MAP_HUGE_SHIFT));
#else
    return DebugUtils::errored(kErrorFeatureNotEnabled);
#endif // __linux__
  }

  void* ptr = mmap(nullptr, size, protection, mmFlags, fd, offset);
  if (ptr == MAP_FAILED)
    return DebugUtils::errored(asmjitErrorFromErrno(errno));

#if defined(MADV_HUGEPAGE)
  if (useLargePages) {
    madvise(ptr, size, MADV_HUGEPAGE);
  }
#endif

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

#if !defined(ASMJIT_NO_DUAL_MAPPING)
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
#endif // !ASMJIT_NO_DUAL_MAPPING

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

#if defined(ASMJIT_ANONYMOUS_MEMORY_USE_MACH_VM_REMAP)
static Error asmjitErrorFromKernResult(kern_return_t result) noexcept {
  switch (result) {
    case KERN_PROTECTION_FAILURE:
      return DebugUtils::errored(kErrorProtectionFailure);
    case KERN_NO_SPACE:
      return DebugUtils::errored(kErrorOutOfMemory);
    case KERN_INVALID_ARGUMENT:
      return DebugUtils::errored(kErrorInvalidArgument);
    default:
      return DebugUtils::errored(kErrorInvalidState);
  }
}

static Error allocDualMappingUsingMachVmRemap(DualMapping* dmOut, size_t size, MemoryFlags memoryFlags) noexcept {
  DualMapping dm {};

  MemoryFlags mmapFlags = MemoryFlags::kAccessReadWrite | (memoryFlags & MemoryFlags::kMapShared);
  ASMJIT_PROPAGATE(mapMemory(&dm.rx, size, mmapFlags));

  vm_prot_t curProt;
  vm_prot_t maxProt;

  int rwProtectFlags = VM_PROT_READ | VM_PROT_WRITE;
  int rxProtectFlags = VM_PROT_READ;

  if (Support::test(memoryFlags, MemoryFlags::kAccessExecute))
    rxProtectFlags |= VM_PROT_EXECUTE;

  kern_return_t result {};
  do {
    vm_map_t task = mach_task_self();
    mach_vm_address_t remappedAddr {};

#if defined(VM_FLAGS_RANDOM_ADDR)
    int remapFlags = VM_FLAGS_ANYWHERE | VM_FLAGS_RANDOM_ADDR;
#else
    int remapFlags = VM_FLAGS_ANYWHERE;
#endif

    // Try to remap the existing memory into a different address.
    result = mach_vm_remap(
      task,                       // target_task
      &remappedAddr,              // target_address
      size,                       // size
      0,                          // mask
      remapFlags,                 // flags
      task,                       // src_task
      (mach_vm_address_t)dm.rx,   // src_address
      false,                      // copy
      &curProt,                   // cur_protection
      &maxProt,                   // max_protection
      VM_INHERIT_DEFAULT);        // inheritance

    if (result != KERN_SUCCESS)
      break;

    dm.rw = (void*)remappedAddr;

    // Now, try to change permissions of both map regions into RW and RX. The vm_protect()
    // API is used twice as we also want to set maximum permissions, so nobody would be
    // allowed to change the RX region back to RW or RWX (if RWX is allowed).
    uint32_t i;
    for (i = 0; i < 2; i++) {
      bool setMaximum = (i == 0);

      result = vm_protect(
        task,                       // target_task
        (vm_address_t)dm.rx,        // address
        size,                       // size
        setMaximum,                 // set_maximum
        rxProtectFlags);            // new_protection

      if (result != KERN_SUCCESS)
        break;

      result = vm_protect(task,     // target_task
        (vm_address_t)dm.rw,        // address
        size,                       // size
        setMaximum,                 // set_maximum
        rwProtectFlags);            // new_protection

      if (result != KERN_SUCCESS)
        break;
    }
  } while (0);

  if (result != KERN_SUCCESS) {
    unmapDualMapping(&dm, size);
    return DebugUtils::errored(asmjitErrorFromKernResult(result));
  }

  *dmOut = dm;
  return kErrorOk;
}
#endif // ASMJIT_ANONYMOUS_MEMORY_USE_MACH_VM_REMAP

#if defined(ASMJIT_ANONYMOUS_MEMORY_USE_FD)
static Error allocDualMappingUsingFile(DualMapping* dm, size_t size, MemoryFlags memoryFlags) noexcept {
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
}
#endif // ASMJIT_ANONYMOUS_MEMORY_USE_FD

Error allocDualMapping(DualMapping* dm, size_t size, MemoryFlags memoryFlags) noexcept {
  dm->rx = nullptr;
  dm->rw = nullptr;

#if defined(ASMJIT_NO_DUAL_MAPPING)
  DebugUtils::unused(size, memoryFlags);
  return DebugUtils::errored(kErrorFeatureNotEnabled);
#else
  if (off_t(size) <= 0)
    return DebugUtils::errored(size == 0 ? kErrorInvalidArgument : kErrorTooLarge);

#if defined(ASMJIT_ANONYMOUS_MEMORY_USE_REMAPDUP)
  return allocDualMappingUsingRemapdup(dm, size, memoryFlags);
#elif defined(ASMJIT_ANONYMOUS_MEMORY_USE_MACH_VM_REMAP)
  return allocDualMappingUsingMachVmRemap(dm, size, memoryFlags);
#elif defined(ASMJIT_ANONYMOUS_MEMORY_USE_FD)
  return allocDualMappingUsingFile(dm, size, memoryFlags);
#else
  #error "[asmjit] VirtMem::allocDualMapping() doesn't have implementation for the target OS or architecture"
#endif
#endif // ASMJIT_NO_DUAL_MAPPING
}

Error releaseDualMapping(DualMapping* dm, size_t size) noexcept {
#if defined(ASMJIT_NO_DUAL_MAPPING)
  DebugUtils::unused(dm, size);
  return DebugUtils::errored(kErrorFeatureNotEnabled);
#else
  return unmapDualMapping(dm, size);
#endif // ASMJIT_NO_DUAL_MAPPING
}
#endif

// Virtual Memory - Flush Instruction Cache
// ========================================

void flushInstructionCache(void* p, size_t size) noexcept {
#if ASMJIT_ARCH_X86 || defined(__EMSCRIPTEN__)
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
    detectVMInfo(localMemInfo);

    vmInfo = localMemInfo;
    vmInfoInitialized.store(1u);
  }

  return vmInfo;
}

size_t largePageSize() noexcept {
  static std::atomic<size_t> largePageSize;
  static constexpr size_t kNotAvailable = 1;

  size_t size = largePageSize.load();
  if (ASMJIT_LIKELY(size > kNotAvailable))
    return size;

  if (size == kNotAvailable)
    return 0;

  size = detectLargePageSize();
  largePageSize.store(size != 0 ? size : kNotAvailable);
  return size;
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

// Virtual Memory - Tests
// ======================

#if defined(ASMJIT_TEST)
ASMJIT_BEGIN_NAMESPACE

UNIT(virt_mem) {
  VirtMem::Info vmInfo = VirtMem::info();

  INFO("VirtMem::info():");
  INFO("  pageSize: %zu", size_t(vmInfo.pageSize));
  INFO("  pageGranularity: %zu", size_t(vmInfo.pageGranularity));

  INFO("VirtMem::largePageSize():");
  INFO("  largePageSize: %zu", size_t(VirtMem::largePageSize()));

  VirtMem::HardenedRuntimeInfo hardenedRtInfo = VirtMem::hardenedRuntimeInfo();
  VirtMem::HardenedRuntimeFlags hardenedFlags = hardenedRtInfo.flags;

  INFO("VirtMem::hardenedRuntimeInfo():");
  INFO("  flags:");
  INFO("    kEnabled: %s"    , Support::test(hardenedFlags, VirtMem::HardenedRuntimeFlags::kEnabled    ) ? "true" : "false");
  INFO("    kMapJit: %s"     , Support::test(hardenedFlags, VirtMem::HardenedRuntimeFlags::kMapJit     ) ? "true" : "false");
  INFO("    kDualMapping: %s", Support::test(hardenedFlags, VirtMem::HardenedRuntimeFlags::kDualMapping) ? "true" : "false");
}

ASMJIT_END_NAMESPACE
#endif // ASMJIT_TEST

#endif // !ASMJIT_NO_JIT
