// This file is part of AsmJit project <https://asmjit.com>
//
// See <asmjit/core.h> or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#include <asmjit/core/api_build_p.h>
#if !defined(ASMJIT_NO_JIT)

#include <asmjit/axl/inplace_array.h>
#include <asmjit/core/globals.h>
#include <asmjit/core/os_utils_p.h>
#include <asmjit/core/pauth_utils_p.h>
#include <asmjit/core/string.h>
#include <asmjit/core/virt_mem.h>
#include <asmjit/core/virt_mem_p.h>

#if defined(_WIN32)
  #include <windows.h>
#else
  #include <errno.h>
  #include <fcntl.h>
  #include <sys/mman.h>
  #include <sys/resource.h>
  #include <sys/stat.h>
  #include <sys/types.h>
  #include <unistd.h>

  #if !ASMJIT_TARGET_ARCH_X86
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

    #ifndef MAP_FIXED_NOREPLACE
      #define MAP_FIXED_NOREPLACE 0x100000
    #endif // MAP_FIXED_NOREPLACE

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

  // dl_iterate_phdr() is used to find the address range of a module.
  #if defined(__linux__) || defined(__FreeBSD__) || defined(__NetBSD__) || defined(__OpenBSD__) || defined(__DragonFly__)
    #include <link.h>
    #define ASMJIT_VM_HAS_DL_ITERATE_PHDR
  #endif

  // Apple recently introduced MAP_JIT flag, which we want to use.
  #if defined(__APPLE__)
    #include <dlfcn.h>
    #include <mach-o/loader.h>
    #include <pthread.h>
    #include <TargetConditionals.h>
    #include <libkern/OSCacheControl.h> // sys_icache_invalidate().
    #if TARGET_OS_OSX
      #include <sys/utsname.h>
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
    #if ASMJIT_TARGET_ARCH_X86 != 0
      #define ASMJIT_ANONYMOUS_MEMORY_USE_MACH_VM_REMAP
    #endif
    #if ASMJIT_TARGET_ARCH_ARM >= 64
      #define ASMJIT_HAS_PTHREAD_JIT_WRITE_PROTECT_NP
    #endif
  #endif

  #if defined(__APPLE__) && !defined(ASMJIT_ANONYMOUS_MEMORY_USE_MACH_VM_REMAP)
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

[[maybe_unused]]
static const constexpr MemoryFlags dual_mapping_filter[2] = {
  MemoryFlags::kAccessWrite | MemoryFlags::kMMapMaxAccessWrite,
  MemoryFlags::kAccessExecute | MemoryFlags::kMMapMaxAccessExecute
};

// It's not fully random, just to avoid collisions when opening TMP or SHM file, and to spread mappings within an
// address range.
[[maybe_unused]]
static uint64_t generate_random_bits(uintptr_t stack_ptr, uint32_t attempt) noexcept {
  static std::atomic<uint32_t> internal_counter;

#if defined(__GNUC__) && ASMJIT_TARGET_ARCH_X86
  // Use RDTSC instruction to avoid gettimeofday() as we just need some "random" bits.
  uint64_t mix = __builtin_ia32_rdtsc();
#elif defined(_WIN32)
  LARGE_INTEGER counter {};
  ::QueryPerformanceCounter(&counter);
  uint64_t mix = uint64_t(counter.QuadPart);
#else
  struct timeval tm {};
  uint64_t mix = 1; // only used when gettimeofday() fails, which is unlikely.
  if (gettimeofday(&tm, nullptr) == 0) {
    mix = uint64_t(tm.tv_usec) ^ uint64_t(tm.tv_sec);
  }
#endif

  uint64_t bits = (uint64_t(stack_ptr) & 0x1010505000055590u) - mix * 773703683;
  bits = (bits >> 33) ^ (bits << 7) ^ (attempt * 87178291199);
  return bits + uint64_t(++internal_counter) * 10619863;
}

// Operating systems cannot map memory anywhere within an address range, but all of them can try a given address,
// so mapping within a range tries random `alignment` aligned addresses at which `size` bytes fit within the range.
struct AddressRangeProbe {
  static constexpr uint32_t kMaxAttempts = 64;

  uintptr_t first;
  uintptr_t last;
  size_t alignment;
  size_t slot_count;

  bool init(size_t size, size_t alignment_, AddressRange range) noexcept {
    alignment = alignment_;
    first = axl::align_up(range.start, alignment);

    if (first < range.start || first >= range.end || range.end - first < size) {
      return false;
    }

    last = first + axl::align_down(range.end - first - size, alignment);
    slot_count = (last - first) / alignment + 1u;
    return true;
  }

  void* address(uint32_t attempt) const noexcept {
    return reinterpret_cast<void*>(first + uintptr_t(generate_random_bits(uintptr_t(this), attempt) % slot_count) * alignment);
  }

  bool contains(const void* p) const noexcept { return uintptr_t(p) >= first && uintptr_t(p) <= last; }
};

// Virtual Memory [Windows]
// ========================

#if defined(_WIN32)

// UWP apps cannot map a view of a file mapping at a given address, MapViewOfFileEx() is only available to desktop apps.
#if defined(WINAPI_FAMILY_PARTITION)
  #if !WINAPI_FAMILY_PARTITION(WINAPI_PARTITION_DESKTOP)
    #define ASMJIT_VM_NO_MAP_VIEW_AT_ADDRESS
  #endif
#endif

struct ScopedHandle {
  inline ScopedHandle() noexcept
    : value(nullptr) {}

  inline ~ScopedHandle() noexcept {
    if (value != nullptr) {
      ::CloseHandle(value);
    }
  }

  HANDLE value;
};

static void detect_vm_info(Info& vm_info) noexcept {
  SYSTEM_INFO system_info;

  ::GetSystemInfo(&system_info);
  vm_info.page_size = axl::align_up_power_of_2<uint32_t>(system_info.dwPageSize);
  vm_info.page_granularity = system_info.dwAllocationGranularity;
}

static size_t detect_large_page_size() noexcept {
  return ::GetLargePageMinimum();
}

static size_t size_from_u64(uint64_t value) noexcept {
  if constexpr (sizeof(size_t) < sizeof(uint64_t)) {
    constexpr size_t kMax = axl::bit_ones<size_t>;
    return value <= kMax ? static_cast<size_t>(value) : kMax;
  }
  else {
    return static_cast<size_t>(value);
  }
}

Limits limits() noexcept {
  Limits out {
    .physical_memory_size = SIZE_MAX,
    .address_space_limit = SIZE_MAX,
    .file_size_limit = SIZE_MAX
  };

  MEMORYSTATUSEX status {};
  status.dwLength = sizeof(status);

  if (GlobalMemoryStatusEx(&status)) {
    out.physical_memory_size = size_from_u64(status.ullTotalPhys);
    out.address_space_limit = size_from_u64(status.ullTotalVirtual);
  }

  return out;
}

size_t dual_mapping_size_limit() noexcept {
  Limits vmem_limits = limits();
  return vmem_limits.address_space_limit;
}

static bool has_dual_mapping_support() noexcept {
  // TODO: This assumption works on X86 platforms, this may not work on AArch64.
  return true;
}

// Returns windows-specific protect_flags from \ref MemoryFlags.
static DWORD protect_flags_from_memory_flags(MemoryFlags memory_flags) noexcept {
  DWORD protect_flags;

  // READ|WRITE|EXECUTE.
  if (axl::test(memory_flags, MemoryFlags::kAccessExecute)) {
    protect_flags = axl::test(memory_flags, MemoryFlags::kAccessWrite) ? PAGE_EXECUTE_READWRITE : PAGE_EXECUTE_READ;
  }
  else if (axl::test(memory_flags, MemoryFlags::kAccessRW)) {
    protect_flags = axl::test(memory_flags, MemoryFlags::kAccessWrite) ? PAGE_READWRITE : PAGE_READONLY;
  }
  else {
    protect_flags = PAGE_NOACCESS;
  }

  // Any other flags to consider?
  return protect_flags;
}

static DWORD desired_access_from_memory_flags(MemoryFlags memory_flags) noexcept {
  DWORD access = axl::test(memory_flags, MemoryFlags::kAccessWrite) ? FILE_MAP_WRITE : FILE_MAP_READ;
  if (axl::test(memory_flags, MemoryFlags::kAccessExecute)) {
    access |= FILE_MAP_EXECUTE;
  }
  return access;
}

static HardenedRuntimeFlags get_hardened_runtime_flags() noexcept {
  HardenedRuntimeFlags flags = HardenedRuntimeFlags::kNone;

  if (has_dual_mapping_support()) {
    flags |= HardenedRuntimeFlags::kDualMapping;
  }

  return flags;
}

// VirtualAlloc() and MapViewOfFileEx() fail with ERROR_INVALID_ADDRESS if the address is taken.
static void* virtual_alloc_in_range(size_t size, DWORD allocation_type, DWORD protect_flags, size_t alignment, AddressRange range) noexcept {
  AddressRangeProbe probe;
  if (!probe.init(size, alignment, range)) {
    return nullptr;
  }

  for (uint32_t attempt = 0; attempt < AddressRangeProbe::kMaxAttempts; attempt++) {
    void* ptr = ::VirtualAlloc(probe.address(attempt), size, allocation_type, protect_flags);
    if (ptr || ::GetLastError() != ERROR_INVALID_ADDRESS) {
      return ptr;
    }
  }

  return nullptr;
}

#if !defined(ASMJIT_VM_NO_MAP_VIEW_AT_ADDRESS)
static void* map_view_in_range(HANDLE handle, DWORD desired_access, size_t size, AddressRange range) noexcept {
  AddressRangeProbe probe;
  if (!probe.init(size, info().page_granularity, range)) {
    return nullptr;
  }

  for (uint32_t attempt = 0; attempt < AddressRangeProbe::kMaxAttempts; attempt++) {
    void* ptr = ::MapViewOfFileEx(handle, desired_access, 0, 0, size, probe.address(attempt));
    if (ptr || ::GetLastError() != ERROR_INVALID_ADDRESS) {
      return ptr;
    }
  }

  return nullptr;
}
#endif // !ASMJIT_VM_NO_MAP_VIEW_AT_ADDRESS

// Allocates anywhere when `range.end` is zero, otherwise within `range`.
static Error alloc_impl(void** p, size_t size, MemoryFlags memory_flags, AddressRange range) noexcept {
  *p = nullptr;

  if (size == 0) {
    return make_error(Error::kInvalidArgument);
  }

  DWORD allocation_type = MEM_COMMIT | MEM_RESERVE;
  DWORD protect_flags = protect_flags_from_memory_flags(memory_flags);
  size_t alignment = info().page_granularity;

  if (axl::test(memory_flags, MemoryFlags::kMMapLargePages)) {
    size_t lp_size = large_page_size();

    // Does it make sense to call VirtualAlloc() if we failed to query large page size?
    if (lp_size == 0) {
      return make_error(Error::kFeatureNotEnabled);
    }

    if (!axl::is_aligned(size, lp_size)) {
      return make_error(Error::kInvalidArgument);
    }

    allocation_type |= MEM_LARGE_PAGES;
    alignment = lp_size;
  }

  void* result = range.end ? virtual_alloc_in_range(size, allocation_type, protect_flags, alignment, range)
                           : ::VirtualAlloc(nullptr, size, allocation_type, protect_flags);
  if (!result) {
    return make_error(Error::kOutOfMemory);
  }

  *p = result;
  return Error::kOk;
}

Error alloc(void** p, size_t size, MemoryFlags memory_flags) noexcept {
  return alloc_impl(p, size, memory_flags, AddressRange{});
}

Error alloc(void** p, size_t size, MemoryFlags memory_flags, AddressRange range) noexcept {
  if (range.start >= range.end) {
    *p = nullptr;
    return make_error(Error::kInvalidArgument);
  }
  return alloc_impl(p, size, memory_flags, range);
}

Error release(void* p, size_t size) noexcept {
  axl::maybe_unused(size);

  // NOTE: If the `dw_free_type` parameter is MEM_RELEASE, `size` parameter must be zero.
  constexpr DWORD dw_free_type = MEM_RELEASE;

  if (ASMJIT_UNLIKELY(!::VirtualFree(p, 0, dw_free_type))) {
    return make_error(Error::kInvalidArgument);

  }
  return Error::kOk;
}

Error protect(void* p, size_t size, MemoryFlags memory_flags) noexcept {
  DWORD protect_flags = protect_flags_from_memory_flags(memory_flags);
  DWORD old_flags;

  if (::VirtualProtect(p, size, protect_flags, &old_flags)) {
    return Error::kOk;
  }

  return make_error(Error::kInvalidArgument);
}

// Maps anywhere when `range.end` is zero, otherwise places the RX view within `range`.
static Error alloc_dual_mapping_impl(Out<DualMapping> dm, size_t size, MemoryFlags memory_flags, AddressRange range) noexcept {
  dm->rx = nullptr;
  dm->rw = nullptr;

  if (size == 0) {
    return make_error(Error::kInvalidArgument);
  }

  ScopedHandle handle;
  handle.value = ::CreateFileMappingW(
    INVALID_HANDLE_VALUE,
    nullptr,
    PAGE_EXECUTE_READWRITE,
    (DWORD)(uint64_t(size) >> 32),
    (DWORD)(size & 0xFFFFFFFFu),
    nullptr);

  if (ASMJIT_UNLIKELY(!handle.value)) {
    return make_error(Error::kOutOfMemory);
  }

  void* ptr[2];
  for (uint32_t i = 0; i < 2; i++) {
    MemoryFlags access_flags = memory_flags & ~dual_mapping_filter[i];
    DWORD desired_access = desired_access_from_memory_flags(access_flags);
#if defined(ASMJIT_VM_NO_MAP_VIEW_AT_ADDRESS)
    axl::maybe_unused(range);
    ptr[i] = ::MapViewOfFile(handle.value, desired_access, 0, 0, size);
#else
    ptr[i] = (i == 0 && range.end) ? map_view_in_range(handle.value, desired_access, size, range)
                                   : ::MapViewOfFile(handle.value, desired_access, 0, 0, size);
#endif

    if (ptr[i] == nullptr) {
      if (i == 1u) {
        ::UnmapViewOfFile(ptr[0]);
      }
      return make_error(Error::kOutOfMemory);
    }
  }

  dm->rx = ptr[0];
  dm->rw = ptr[1];
  return Error::kOk;
}

Error alloc_dual_mapping(Out<DualMapping> dm, size_t size, MemoryFlags memory_flags) noexcept {
  return alloc_dual_mapping_impl(dm, size, memory_flags, AddressRange{});
}

Error alloc_dual_mapping(Out<DualMapping> dm, size_t size, MemoryFlags memory_flags, AddressRange range) noexcept {
#if defined(ASMJIT_VM_NO_MAP_VIEW_AT_ADDRESS)
  axl::maybe_unused(size, memory_flags, range);
  dm = DualMapping{};
  return make_error(Error::kFeatureNotEnabled);
#else
  if (range.start >= range.end) {
    dm = DualMapping{};
    return make_error(Error::kInvalidArgument);
  }
  return alloc_dual_mapping_impl(dm, size, memory_flags, range);
#endif
}

Error release_dual_mapping(DualMapping& dm, size_t size) noexcept {
  axl::maybe_unused(size);
  bool failed = false;

  if (!::UnmapViewOfFile(dm.rx)) {
    failed = true;
  }

  if (dm.rx != dm.rw && !UnmapViewOfFile(dm.rw)) {
    failed = true;
  }

  if (failed) {
    return make_error(Error::kInvalidArgument);
  }

  dm.rx = nullptr;
  dm.rw = nullptr;
  return Error::kOk;
}

#endif

// Virtual Memory [Unix]
// =====================

#if !defined(_WIN32)

// Virtual Memory [Unix] - Constants
// =================================

#if defined(RLIMIT_AS)
static constexpr auto kRLimitAS = RLIMIT_AS;
#else
static constexpr auto kRLimitAS = RLIMIT_DATA;
#endif

static constexpr auto kRLimitFileSize = RLIMIT_FSIZE;

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

[[maybe_unused]]
static KernelVersion get_kernel_version() noexcept {
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
#endif

[[maybe_unused]]
static size_t make_page_mask(size_t page_size) noexcept {
  return ~(page_size - 1u);
}

// Translates libc errors specific to VirtualMemory mapping to `asmjit::Error`.
[[maybe_unused]]
static Error asmjit_error_from_errno(int e) noexcept {
  switch (e) {
    case EACCES:
    case EAGAIN:
    case ENODEV:
    case EPERM:
      return Error::kInvalidState;

    case EFBIG:
    case ENOMEM:
    case EOVERFLOW:
      return Error::kOutOfMemory;

    case EMFILE:
    case ENFILE:
      return Error::kTooManyHandles;

    default:
      return Error::kInvalidArgument;
  }
}

[[maybe_unused]]
static MemoryFlags max_access_flags_to_regular_access_flags(MemoryFlags memory_flags) noexcept {
  static constexpr uint32_t kMaxProtShift = axl::ctz_const(MemoryFlags::kMMapMaxAccessRead);
  return MemoryFlags(uint32_t(memory_flags & MemoryFlags::kMMapMaxAccessRWX) >> kMaxProtShift);
}

[[maybe_unused]]
static MemoryFlags regular_access_flags_to_max_access_flags(MemoryFlags memory_flags) noexcept {
  static constexpr uint32_t kMaxProtShift = axl::ctz_const(MemoryFlags::kMMapMaxAccessRead);
  return MemoryFlags(uint32_t(memory_flags & MemoryFlags::kAccessRWX) << kMaxProtShift);
}

// Returns `mmap()` protection flags from \ref MemoryFlags.
[[maybe_unused]]
static int mm_prot_from_memory_flags(MemoryFlags memory_flags) noexcept {
  int protection = 0;
  if (axl::test(memory_flags, MemoryFlags::kAccessRead)) protection |= PROT_READ;
  if (axl::test(memory_flags, MemoryFlags::kAccessWrite)) protection |= PROT_READ | PROT_WRITE;
  if (axl::test(memory_flags, MemoryFlags::kAccessExecute)) protection |= PROT_READ | PROT_EXEC;
  return protection;
}

// Returns maximum protection flags from `memory_flags`.
//
// Uses:
//   - `PROT_MPROTECT()` on NetBSD.
//   - `PROT_MAX()` when available on other BSDs.
[[maybe_unused]]
static inline int mm_max_prot_from_memory_flags(MemoryFlags memory_flags) noexcept {
  MemoryFlags acc = max_access_flags_to_regular_access_flags(memory_flags);
  if (acc != MemoryFlags::kNone) {
#if defined(__NetBSD__) && defined(PROT_MPROTECT)
    return PROT_MPROTECT(mm_prot_from_memory_flags(acc));
#elif defined(PROT_MAX)
    return PROT_MAX(mm_prot_from_memory_flags(acc));
#else
    return 0;
#endif
  }

  return 0;
}

static void detect_vm_info(Info& vm_info) noexcept {
# if defined(_SC_PAGESIZE)
  auto page_size = sysconf(_SC_PAGESIZE);
# elif defined(_SC_PAGE_SIZE)
  auto page_size = sysconf(_SC_PAGE_SIZE);
# else
  auto page_size = getpagesize();
# endif

  vm_info.page_size = page_size > 0 ? uint32_t(page_size) : 4096u;
  vm_info.page_granularity = axl::max<uint32_t>(vm_info.page_size, 65536);
}

static size_t detect_large_page_size() noexcept {
#if defined(__APPLE__) && defined(VM_FLAGS_SUPERPAGE_SIZE_2MB) && ASMJIT_TARGET_ARCH_X86
  return 2u * 1024u * 1024u;
#elif defined(__FreeBSD__)
  axl::InplaceArray<size_t, 2> page_size;
  // TODO: Does it return unsigned?
  return (getpagesizes(page_size.data(), 2) < 2) ? 0 : uint32_t(page_size[1]);
#elif defined(__linux__)
  StringTmp<128> path;

  // The Linux provided directories in `/sys/kernel/mm/hugepages/...` use kB units, so 1 equals KiB.
  [[maybe_unused]] constexpr uint32_t KiB = 1;
  [[maybe_unused]] constexpr uint32_t MiB = 1024;
  [[maybe_unused]] constexpr uint32_t GiB = 1024 * 1024;

  // NOTE: The table must distinguish between 32-bit and 64-bit targets as 32-bit targets cannot have
  // more than 2GiB pages (as 4 GiB cannot really be stored in a 32-bit `size_t`). In practice there
  // are probably no 32-bit targets that would offer 2 GiB pages.
#if ASMJIT_TARGET_ARCH_X86 != 0
  static constexpr uint32_t known_pages_kib[] = { 2 * MiB, 1 * GiB };
#elif ASMJIT_TARGET_ARCH_ARM == 64
  static constexpr uint32_t known_pages_kib[] = { 64 * KiB, 2 * MiB, 32 * MiB, 512 * MiB, 1 * GiB, 16 * GiB };
#elif ASMJIT_TARGET_ARCH_BITS == 32
  static constexpr uint32_t known_pages_kib[] = { 1 * MiB, 2 * MiB, 16 * MiB, 1 * GiB, 2 * GiB };
#else
  static constexpr uint32_t known_pages_kib[] = { 1 * MiB, 2 * MiB, 16 * MiB, 1 * GiB, 2 * GiB, 16 * GiB };
#endif

  StringTmp<128> storage;
  for (uint32_t known_page : known_pages_kib) {
    path.assign_format("/sys/kernel/mm/hugepages/hugepages-%ukB", known_page);
    struct stat sbuf;
    if (stat(path.data(), &sbuf) == 0 && S_ISDIR(sbuf.st_mode)) {
      return static_cast<size_t>(known_page) * 1024u;
    }
  }
  return 0u;
#else
  return 0u;
#endif
}

static size_t read_resource_limit(auto resource, size_t page_size) noexcept {
  struct rlimit rlim;
  if (getrlimit(resource, &rlim) == 0) {
    if (rlim.rlim_cur == RLIM_INFINITY) {
      return SIZE_MAX;
    }

    if constexpr (sizeof(rlim.rlim_cur) > sizeof(size_t)) {
      if (rlim.rlim_cur > SIZE_MAX) {
        return SIZE_MAX;
      }
    }

    return static_cast<size_t>(rlim.rlim_cur) & make_page_mask(page_size);
  }
  else {
    return SIZE_MAX;
  }
}

Limits limits() noexcept {
  size_t page_size = info().page_size;

  Limits out {
    .physical_memory_size = SIZE_MAX,
    .address_space_limit = SIZE_MAX,
    .file_size_limit = SIZE_MAX
  };

  // None can be cached. Number of physical pages depends on installed memory, which can support hotplug, and
  // process limits can be lowered by syscalls, so we always have to get a fresh value instead of caching it.
  long num_pages = sysconf(_SC_PHYS_PAGES);
  if (num_pages > 0) {
    out.physical_memory_size = axl::mul_saturate<size_t>(static_cast<size_t>(num_pages), page_size);
  }

  out.address_space_limit = read_resource_limit(kRLimitAS, page_size);
  out.file_size_limit = read_resource_limit(kRLimitFileSize, page_size);

  return out;
}

size_t dual_mapping_size_limit() noexcept {
#if defined(ASMJIT_ANONYMOUS_MEMORY_USE_REMAPDUP) || \
    defined(ASMJIT_ANONYMOUS_MEMORY_USE_MACH_VM_REMAP) || \
    defined(ASMJIT_ANONYMOUS_MEMORY_USE_FD)
  size_t page_size = info().page_size;
  size_t address_space_limit = read_resource_limit(kRLimitAS, page_size);

#if defined(ASMJIT_ANONYMOUS_MEMORY_USE_FD)
  size_t file_size_limit = read_resource_limit(kRLimitFileSize, page_size);
  return axl::min(file_size_limit, address_space_limit);
#else
  return address_space_limit;
#endif
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
static const char* get_tmp_dir() noexcept {
  const char* tmp_dir = getenv("TMPDIR");
  return tmp_dir ? tmp_dir : "/tmp";
}
#endif

#if defined(__linux__) && defined(__NR_memfd_create)
static uint32_t get_mfd_exec_flag() noexcept {
  static std::atomic<uint32_t> cached_mfd_exec_supported;
  uint32_t val = cached_mfd_exec_supported.load();

  if (val == 0u) {
    KernelVersion ver = get_kernel_version();
    val = uint32_t(ver.ge(6, 3)) + 1u;
    cached_mfd_exec_supported.store(val);
  }

  return val == 2u ? uint32_t(MFD_EXEC) : uint32_t(0u);
}
#endif // __linux__ && __NR_memfd_create


[[maybe_unused]]
static inline int default_open_flags() noexcept { return O_RDWR | O_CREAT | O_EXCL | O_CLOEXEC; }

// shm_open() should automatically use O_CLOEXEC on a comforming implementation.
[[maybe_unused]]
static inline int default_shm_open_flags() noexcept { return O_RDWR | O_CREAT | O_EXCL; }

class AnonymousMemory {
public:
  enum FileType : uint32_t {
    kFileTypeNone,
    kFileTypeShm,
    kFileTypeTmp
  };

  int _fd;
  FileType _file_type;
  StringTmp<128> _tmp_name;

  inline AnonymousMemory() noexcept
    : _fd(-1),
      _file_type(kFileTypeNone),
      _tmp_name() {}

  inline ~AnonymousMemory() noexcept {
    unlink();
    close();
  }

  inline int fd() const noexcept { return _fd; }

  Error open(bool prefer_tmp_over_dev_shm, [[maybe_unused]] size_t lp_size = 0u) noexcept {
#if defined(__linux__) && defined(__NR_memfd_create)
    // Linux specific 'memfd_create' - if the syscall returns `ENOSYS` it means
    // it's not available and we will never call it again (would be pointless).
    //
    // NOTE: There is also memfd_create() libc function in FreeBSD, but it internally
    // uses `shm_open(SHM_ANON, ...)` so it's not needed to add support for it (it's
    // not a syscall as in Linux).

    // Zero initialized, if ever changed to '1' that would mean the syscall is not
    // available and we must use `shm_open()` and `shm_unlink()` (or regular `open()`).
    static std::atomic<uint32_t> memfd_create_not_supported;

    if (!memfd_create_not_supported.load(std::memory_order::relaxed)) {
      uint32_t flags = get_mfd_exec_flag() | MFD_CLOEXEC;
      if (lp_size != 0u) {
        flags |= uint32_t(MFD_HUGETLB) | (uint32_t(axl::ctz(lp_size)) << MFD_HUGE_SHIFT);
      }

      _fd = (int)syscall(__NR_memfd_create, "vmem", flags);
      if (ASMJIT_LIKELY(_fd >= 0)) {
        return Error::kOk;
      }

      int e = errno;
      if (e == ENOSYS) {
        memfd_create_not_supported.store(1, std::memory_order::relaxed);
      }
      else {
        return make_error(asmjit_error_from_errno(e));
      }
    }
#endif // __linux__ && __NR_memfd_create

    if (lp_size != 0) {
      return make_error(Error::kFeatureNotEnabled);
    }

#if defined(ASMJIT_HAS_SHM_OPEN) && defined(SHM_ANON)
    // Originally FreeBSD extension, apparently works in other BSDs too.
    axl::maybe_unused(prefer_tmp_over_dev_shm);
    _fd = ::shm_open(SHM_ANON, default_shm_open_flags(), S_IRUSR | S_IWUSR);

    if (ASMJIT_LIKELY(_fd >= 0)) {
      return Error::kOk;
    }
    else {
      return make_error(asmjit_error_from_errno(errno));
    }
#else
    // POSIX API. We have to generate somehow a unique name, so use `generate_random_bits()` helper. To prevent
    // having file collisions we use `shm_open()` with flags that require creation of the file so we never open
    // an existing shared memory.
    static const char shm_format_string[] = "/shm-id-%016llX";
    uint32_t retry_count = 100;

    for (uint32_t i = 0; i < retry_count; i++) {
      bool use_tmp = !ASMJIT_VM_SHM_DETECT || prefer_tmp_over_dev_shm;
      uint64_t bits = generate_random_bits((uintptr_t)this, i);

      if (use_tmp) {
        ASMJIT_PROPAGATE(_tmp_name.assign(get_tmp_dir()));
        ASMJIT_PROPAGATE(_tmp_name.append_format(shm_format_string, (unsigned long long)bits));
        _fd = ::open(_tmp_name.data(), default_open_flags(), 0);
        if (ASMJIT_LIKELY(_fd >= 0)) {
          _file_type = kFileTypeTmp;
          return Error::kOk;
        }
      }
#if defined(ASMJIT_HAS_SHM_OPEN)
      else {
        ASMJIT_PROPAGATE(_tmp_name.assign_format(shm_format_string, (unsigned long long)bits));
        _fd = ::shm_open(_tmp_name.data(), default_shm_open_flags(), S_IRUSR | S_IWUSR);
        if (ASMJIT_LIKELY(_fd >= 0)) {
          _file_type = kFileTypeShm;
          return Error::kOk;
        }
      }
#endif

      int e = errno;
      if (e != EEXIST) {
        return make_error(asmjit_error_from_errno(e));
      }
    }

    return make_error(Error::kFailedToOpenAnonymousMemory);
#endif
  }

  void unlink() noexcept {
    FileType type = _file_type;
    _file_type = kFileTypeNone;

#ifdef ASMJIT_HAS_SHM_OPEN
    if (type == kFileTypeShm) {
      ::shm_unlink(_tmp_name.data());
      return;
    }
#endif

    if (type == kFileTypeTmp) {
      ::unlink(_tmp_name.data());
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
    if (ftruncate(_fd, off_t(size)) != 0) {
      return make_error(asmjit_error_from_errno(errno));
    }

    return Error::kOk;
  }
};

#if ASMJIT_VM_SHM_DETECT
static Error detect_anonymous_memory_strategy(Out<AnonymousMemoryStrategy> strategy_out, size_t page_size) noexcept {
  AnonymousMemory anon_mem;

  ASMJIT_PROPAGATE(anon_mem.open(false));
  ASMJIT_PROPAGATE(anon_mem.allocate(page_size));

  void* ptr = mmap(nullptr, page_size, PROT_READ | PROT_EXEC, MAP_SHARED, anon_mem.fd(), 0);
  if (ptr == MAP_FAILED) {
    int e = errno;
    if (e == EINVAL || e == EPERM) {
      *strategy_out = AnonymousMemoryStrategy::kTmpDir;
      return Error::kOk;
    }
    return make_error(asmjit_error_from_errno(e));
  }
  else {
    munmap(ptr, page_size);
    *strategy_out = AnonymousMemoryStrategy::kDevShm;
    return Error::kOk;
  }
}
#endif

static Error get_anonymous_memory_strategy(AnonymousMemoryStrategy* strategy_out) noexcept {
#if ASMJIT_VM_SHM_DETECT
  // Initially don't assume anything. It has to be tested whether '/dev/shm' was mounted with 'noexec' flag or not.
  static std::atomic<uint32_t> cached_strategy;

  AnonymousMemoryStrategy strategy = static_cast<AnonymousMemoryStrategy>(cached_strategy.load());
  if (strategy == AnonymousMemoryStrategy::kUnknown) {
    Info vm_info = info();
    size_t file_size_limit = read_resource_limit(kRLimitFileSize, vm_info.page_size);

    if (file_size_limit < vm_info.page_size) {
      // Do not cache anything if we cannot map a single page, just return kTmpDir.
      *strategy_out = AnonymousMemoryStrategy::kTmpDir;
      return Error::kOk;
    }

    ASMJIT_PROPAGATE(detect_anonymous_memory_strategy(Out(strategy), vm_info.page_size));
    cached_strategy.store(static_cast<uint32_t>(strategy));
  }

  *strategy_out = strategy;
  return Error::kOk;
#else
  *strategy_out = AnonymousMemoryStrategy::kTmpDir;
  return Error::kOk;
#endif
}

#endif // ASMJIT_ANONYMOUS_MEMORY_USE_FD

// Virtual Memory [Posix] - Hardened Runtime & MAP_JIT
// ===================================================

// Detects whether the current process is hardened, which means that pages that have WRITE and EXECUTABLE flags
// cannot be normally allocated. On OSX + AArch64 such allocation requires MAP_JIT flag, other platforms don't
// support this combination.
static bool has_hardened_runtime() noexcept {
#if defined(__APPLE__) && TARGET_OS_OSX && ASMJIT_TARGET_ARCH_ARM >= 64
  // OSX on AArch64 has always hardened runtime enabled.
  return true;
#else
  static std::atomic<uint32_t> cached_hardened_flag;

  constexpr uint32_t hf_unknown  = 0;
  constexpr uint32_t hf_disabled = 1;
  constexpr uint32_t hf_enabled  = 2;

  uint32_t flag = cached_hardened_flag.load();
  if (flag == hf_unknown) {
    size_t page_size = size_t(::getpagesize());
    void* ptr = mmap(nullptr, page_size, PROT_READ | PROT_WRITE | PROT_EXEC, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);

    if (ptr == MAP_FAILED) {
      flag = hf_enabled;
    }
    else {
      flag = hf_disabled;
      munmap(ptr, page_size);
    }

    cached_hardened_flag.store(flag);
  }

  return flag == hf_enabled;
#endif
}

// Detects whether MAP_JIT is available.
static inline bool has_mapjit_support() noexcept {
#if defined(__APPLE__) && TARGET_OS_OSX && ASMJIT_TARGET_ARCH_X86 == 0
  // Apple platforms always use hardened runtime + MAP_JIT on non-x86 hardware:
  //   - https://developer.apple.com/documentation/apple_silicon/porting_just-in-time_compilers_to_apple_silicon
  return true;
#elif defined(__APPLE__) && TARGET_OS_OSX
  // MAP_JIT flag required to run unsigned JIT code is only supported by kernel version 10.14+ (Mojave).
  static std::atomic<uint32_t> cached_mapjit_support;
  uint32_t val = cached_mapjit_support.load();

  if (val == 0u) {
    KernelVersion ver = get_kernel_version();
    val = uint32_t(ver.ge(18, 0)) + 1u;
    cached_mapjit_support.store(val);
  }

  return val == 2u;
#else
  // MAP_JIT is not available (it's only available on OSX).
  return false;
#endif
}

// Returns either MAP_JIT or 0 based on `memory_flags` and the host operating system.
static inline int mm_mapjit_from_memory_flags(MemoryFlags memory_flags) noexcept {
#if defined(__APPLE__)
  // Always use MAP_JIT flag if user asked for it (could be used for testing on non-hardened processes) and detect
  // whether it must be used when the process is actually hardened (in that case it doesn't make sense to rely on
  // user `memory_flags`).
  //
  // MAP_JIT is not required when dual-mapping memory and is incompatible with MAP_SHARED, so it will not be
  // added when the latter is enabled.
  bool use_mapjit = (axl::test(memory_flags, MemoryFlags::kMMapEnableMapJit) || has_hardened_runtime()) &&
                    !axl::test(memory_flags, MemoryFlags::kMapShared);
  if (use_mapjit) {
    return has_mapjit_support() ? int(MAP_JIT) : 0;
  }
  else {
    return 0;
  }
#else
  axl::maybe_unused(memory_flags);
  return 0;
#endif
}

static inline bool has_dual_mapping_support() noexcept {
#if defined(ASMJIT_NO_DUAL_MAPPING)
  return false;
#else
  return true;
#endif
}

static HardenedRuntimeFlags get_hardened_runtime_flags() noexcept {
  HardenedRuntimeFlags flags = HardenedRuntimeFlags::kNone;

  if (has_hardened_runtime()) {
    flags |= HardenedRuntimeFlags::kEnabled;
  }

  if (has_mapjit_support()) {
    flags |= HardenedRuntimeFlags::kMapJit;
  }

  if (has_dual_mapping_support()) {
    flags |= HardenedRuntimeFlags::kDualMapping;
  }

#if defined(__APPLE__) && defined(__arm64e__)
  flags |= HardenedRuntimeFlags::kPtrAuth;
#endif

  return flags;
}

// Flags that make mmap() fail if the address is taken. Without such flags the address is only a hint and the mapping
// may be placed elsewhere, so `map_in_range()` checks the result either way.
#if defined(__linux__)
// Linux 4.17+, older kernels treat the address as a hint.
static constexpr int kMMapNoReplace = MAP_FIXED_NOREPLACE;
#elif defined(MAP_EXCL)
// FreeBSD.
static constexpr int kMMapNoReplace = MAP_FIXED | MAP_EXCL;
#elif defined(__MAP_NOREPLACE)
// OpenBSD.
static constexpr int kMMapNoReplace = MAP_FIXED | __MAP_NOREPLACE;
#else
// macOS and NetBSD.
static constexpr int kMMapNoReplace = 0;
#endif

// Tests whether mapping at a given address failed because the address is taken, so another address can be tried.
static inline bool is_address_taken_error(int e) noexcept {
#if defined(MAP_EXCL)
  // FreeBSD documents EINVAL, but returns ENOMEM.
  if (e == EINVAL || e == ENOMEM) {
    return true;
  }
#elif defined(__MAP_NOREPLACE) || defined(__NetBSD__)
  // OpenBSD's mmap() and NetBSD's mremap(MAP_FIXED).
  if (e == ENOMEM) {
    return true;
  }
#endif

  // Linux returns EEXIST, and EPERM for addresses below `vm.mmap_min_addr`.
  return e == EEXIST || e == EPERM;
}

// Calls `map_fn(addr)` with addresses picked by `AddressRangeProbe` until it returns a mapping within the range.
static void* map_in_range(size_t size, size_t alignment, AddressRange range, auto&& map_fn) noexcept {
  AddressRangeProbe probe;
  if (!probe.init(size, alignment, range)) {
    errno = ENOMEM;
    return MAP_FAILED;
  }

  for (uint32_t attempt = 0; attempt < AddressRangeProbe::kMaxAttempts; attempt++) {
    void* ptr = map_fn(probe.address(attempt));

    if (ptr == MAP_FAILED) {
      if (!is_address_taken_error(errno)) {
        return MAP_FAILED;
      }
      continue;
    }

    if (probe.contains(ptr)) {
      return ptr;
    }

    munmap(ptr, size);
  }

  errno = ENOMEM;
  return MAP_FAILED;
}

// Maps anywhere when `range.end` is zero, otherwise within `range`.
static Error map_memory(void** p, size_t size, MemoryFlags memory_flags, int fd = -1, off_t offset = 0, AddressRange range = AddressRange{}) noexcept {
  *p = nullptr;

  if (size == 0) {
    return make_error(Error::kInvalidArgument);
  }

  int protection = mm_prot_from_memory_flags(memory_flags) | mm_max_prot_from_memory_flags(memory_flags);
  int mm_flags = mm_mapjit_from_memory_flags(memory_flags);

  mm_flags |= axl::test(memory_flags, MemoryFlags::kMapShared) ? MAP_SHARED : MAP_PRIVATE;
  if (fd == -1) {
    mm_flags |= MAP_ANONYMOUS;
  }

  bool use_large_pages = axl::test(memory_flags, MemoryFlags::kMMapLargePages);
  size_t alignment = info().page_size;

  if (use_large_pages) {
#if defined(__linux__)
    size_t lp_size = large_page_size();
    if (lp_size == 0) {
      return make_error(Error::kFeatureNotEnabled);
    }

    if (!axl::is_aligned(size, lp_size)) {
      return make_error(Error::kInvalidArgument);
    }

    unsigned lp_size_log2 = axl::ctz(lp_size);
    mm_flags |= int(unsigned(MAP_HUGETLB) | (lp_size_log2 << MAP_HUGE_SHIFT));
    alignment = lp_size;
#else
    return make_error(Error::kFeatureNotEnabled);
#endif // __linux__
  }

  void* ptr;
  if (range.end) {
    ptr = map_in_range(size, alignment, range, [&](void* addr) noexcept {
      return mmap(addr, size, protection, mm_flags | kMMapNoReplace, fd, offset);
    });
  }
  else {
    ptr = mmap(nullptr, size, protection, mm_flags, fd, offset);
  }

  if (ptr == MAP_FAILED) {
    return make_error(asmjit_error_from_errno(errno));
  }

#if defined(MADV_HUGEPAGE)
  if (axl::test(memory_flags, MemoryFlags::kMAdviseLargePages)) {
    madvise(ptr, size, MADV_HUGEPAGE);
  }
#endif

  *p = ptr;
  return Error::kOk;
}

static Error unmap_memory(void* p, size_t size) noexcept {
  if (ASMJIT_UNLIKELY(munmap(p, size) != 0)) {
    return make_error(asmjit_error_from_errno(errno));
  }

  return Error::kOk;
}

Error alloc(void** p, size_t size, MemoryFlags memory_flags) noexcept {
  return map_memory(p, size, memory_flags);
}

Error alloc(void** p, size_t size, MemoryFlags memory_flags, AddressRange range) noexcept {
  if (range.start >= range.end) {
    *p = nullptr;
    return make_error(Error::kInvalidArgument);
  }
  return map_memory(p, size, memory_flags, -1, 0, range);
}

Error release(void* p, size_t size) noexcept {
  return unmap_memory(p, size);
}

Error protect(void* p, size_t size, MemoryFlags memory_flags) noexcept {
  int protection = mm_prot_from_memory_flags(memory_flags);
  if (mprotect(p, size, protection) == 0) {
    return Error::kOk;
  }
  return make_error(asmjit_error_from_errno(errno));
}

// Virtual Memory [Posix] - Dual Mapping
// =====================================

#if !defined(ASMJIT_NO_DUAL_MAPPING)
static Error unmap_dual_mapping(DualMapping& dm, size_t size) noexcept {
  Error err1 = unmap_memory(dm.rx, size);
  Error err2 = Error::kOk;

  if (dm.rx != dm.rw) {
    err2 = unmap_memory(dm.rw, size);
  }

  // We can report only one error, so report the first...
  if (err1 != Error::kOk || err2 != Error::kOk) {
    return make_error(err1 != Error::kOk ? err1 : err2);
  }

  dm.rx = nullptr;
  dm.rw = nullptr;
  return Error::kOk;
}
#endif // !ASMJIT_NO_DUAL_MAPPING

#if defined(ASMJIT_ANONYMOUS_MEMORY_USE_REMAPDUP)
static Error alloc_dual_mapping_via_remapdup(Out<DualMapping> dm_out, size_t size, MemoryFlags memory_flags, AddressRange range) noexcept {
  MemoryFlags max_access_flags = regular_access_flags_to_max_access_flags(memory_flags);
  MemoryFlags final_flags = memory_flags | max_access_flags | MemoryFlags::kMapShared;

  MemoryFlags rx_flags = final_flags & ~(MemoryFlags::kAccessWrite | MemoryFlags::kMMapMaxAccessWrite);
  MemoryFlags rw_flags = final_flags & ~(MemoryFlags::kAccessExecute);

  // Allocate RW mapping.
  DualMapping dm {};
  ASMJIT_PROPAGATE(map_memory(&dm.rw, size, rw_flags));

  // Allocate RX mapping, mremap() ignores the address without MAP_FIXED, which fails if the address is taken.
  if (range.end) {
    dm.rx = map_in_range(size, info().page_size, range, [&](void* addr) noexcept {
      return mremap(dm.rw, size, addr, size, MAP_FIXED | MAP_REMAPDUP);
    });
  }
  else {
    dm.rx = mremap(dm.rw, size, nullptr, size, MAP_REMAPDUP);
  }

  if (dm.rx == MAP_FAILED) {
    int e = errno;
    munmap(dm.rw, size);
    return make_error(asmjit_error_from_errno(e));
  }

  if (mprotect(dm.rx, size, mm_prot_from_memory_flags(rx_flags)) != 0) {
    int e = errno;
    unmap_dual_mapping(dm, size);
    return make_error(asmjit_error_from_errno(e));
  }

  *dm_out = dm;
  return Error::kOk;
}
#endif

#if defined(ASMJIT_ANONYMOUS_MEMORY_USE_MACH_VM_REMAP)
static Error asmjit_error_from_kern_result(kern_return_t result) noexcept {
  switch (result) {
    case KERN_PROTECTION_FAILURE:
      return make_error(Error::kProtectionFailure);
    case KERN_NO_SPACE:
      return make_error(Error::kOutOfMemory);
    case KERN_INVALID_ARGUMENT:
      return make_error(Error::kInvalidArgument);
    default:
      return make_error(Error::kInvalidState);
  }
}

static Error alloc_dual_mapping_using_mach_vm_remap(Out<DualMapping> dm_out, size_t size, MemoryFlags memory_flags, AddressRange range) noexcept {
  DualMapping dm {};
  MemoryFlags mmap_flags = MemoryFlags::kAccessReadWrite | (memory_flags & MemoryFlags::kMapShared);

  // The original mapping becomes the RX view.
  ASMJIT_PROPAGATE(map_memory(&dm.rx, size, mmap_flags, -1, 0, range));

  vm_prot_t cur_prot;
  vm_prot_t max_prot;

  int rw_protect_flags = VM_PROT_READ | VM_PROT_WRITE;
  int rx_protect_flags = VM_PROT_READ;

  if (axl::test(memory_flags, MemoryFlags::kAccessExecute)) {
    rx_protect_flags |= VM_PROT_EXECUTE;
  }

  kern_return_t result {};
  do {
    vm_map_t task = mach_task_self();
    mach_vm_address_t remapped_addr {};

#if defined(VM_FLAGS_RANDOM_ADDR)
    int remap_flags = VM_FLAGS_ANYWHERE | VM_FLAGS_RANDOM_ADDR;
#else
    int remap_flags = VM_FLAGS_ANYWHERE;
#endif

    // Try to remap the existing memory into a different address.
    result = mach_vm_remap(
      task,                       // target_task
      &remapped_addr,             // target_address
      size,                       // size
      0,                          // mask
      remap_flags,                // flags
      task,                       // src_task
      (mach_vm_address_t)dm.rx,   // src_address
      false,                      // copy
      &cur_prot,                  // cur_protection
      &max_prot,                  // max_protection
      VM_INHERIT_DEFAULT);        // inheritance

    if (result != KERN_SUCCESS) {
      break;
    }

    dm.rw = (void*)remapped_addr;

    // Now, try to change permissions of both map regions into RW and RX. The vm_protect()
    // API is used twice as we also want to set maximum permissions, so nobody would be
    // allowed to change the RX region back to RW or RWX (if RWX is allowed).
    uint32_t i;
    for (i = 0; i < 2; i++) {
      bool set_maximum = (i == 0);

      result = vm_protect(
        task,                       // target_task
        (vm_address_t)dm.rx,        // address
        size,                       // size
        set_maximum,                // set_maximum
        rx_protect_flags);          // new_protection

      if (result != KERN_SUCCESS) {
        break;
      }

      result = vm_protect(task,     // target_task
        (vm_address_t)dm.rw,        // address
        size,                       // size
        set_maximum,                // set_maximum
        rw_protect_flags);          // new_protection

      if (result != KERN_SUCCESS) {
        break;
      }
    }
  } while (0);

  if (result != KERN_SUCCESS) {
    unmap_dual_mapping(dm, size);
    return make_error(asmjit_error_from_kern_result(result));
  }

  *dm_out = dm;
  return Error::kOk;
}
#endif // ASMJIT_ANONYMOUS_MEMORY_USE_MACH_VM_REMAP

#if defined(ASMJIT_ANONYMOUS_MEMORY_USE_FD)
static Error alloc_dual_mapping_using_file(Out<DualMapping> dm, size_t size, MemoryFlags memory_flags, AddressRange range) noexcept {
  Info vm_info = info();
  size_t file_size_limit = read_resource_limit(kRLimitFileSize, vm_info.page_size);

  if (size > file_size_limit) {
    return make_error(Error::kTooLarge);
  }

  bool use_large_pages = axl::test(memory_flags, MemoryFlags::kMMapLargePages);
  size_t lp_size = 0u;

  if (use_large_pages) {
    lp_size = large_page_size();
    if (lp_size == 0u) {
      return make_error(Error::kFeatureNotEnabled);
    }

    if (!axl::is_aligned(size, lp_size)) {
      return make_error(Error::kInvalidArgument);
    }
  }

  bool prefer_tmp_over_dev_shm = axl::test(memory_flags, MemoryFlags::kMappingPreferTmp);
  if (!prefer_tmp_over_dev_shm && lp_size == 0u) {
    AnonymousMemoryStrategy strategy;
    ASMJIT_PROPAGATE(get_anonymous_memory_strategy(&strategy));
    prefer_tmp_over_dev_shm = (strategy == AnonymousMemoryStrategy::kTmpDir);
  }

  AnonymousMemory anon_mem;
  ASMJIT_PROPAGATE(anon_mem.open(prefer_tmp_over_dev_shm, lp_size));
  ASMJIT_PROPAGATE(anon_mem.allocate(size));

  void* ptr[2];
  for (uint32_t i = 0; i < 2; i++) {
    // Only the RX view (index 0) is placed within the range.
    MemoryFlags restricted_memory_flags = memory_flags & ~dual_mapping_filter[i];
    Error err = map_memory(&ptr[i], size, restricted_memory_flags | MemoryFlags::kMapShared, anon_mem.fd(), 0,
                           i == 0 ? range : AddressRange{});
    if (err != Error::kOk) {
      if (i == 1) {
        unmap_memory(ptr[0], size);
      }
      return err;
    }
  }

  dm->rx = ptr[0];
  dm->rw = ptr[1];
  return Error::kOk;
}
#endif // ASMJIT_ANONYMOUS_MEMORY_USE_FD

// Maps anywhere when `range.end` is zero, otherwise places the RX view within `range`.
static Error alloc_dual_mapping_impl(Out<DualMapping> dm, size_t size, MemoryFlags memory_flags, AddressRange range) noexcept {
  dm = DualMapping{};

#if defined(ASMJIT_NO_DUAL_MAPPING)
  axl::maybe_unused(size, memory_flags, range);
  return make_error(Error::kFeatureNotEnabled);
#else
  if (off_t(size) <= 0) {
    return make_error(size == 0 ? Error::kInvalidArgument : Error::kTooLarge);
  }

#if defined(ASMJIT_ANONYMOUS_MEMORY_USE_REMAPDUP)
  return alloc_dual_mapping_via_remapdup(dm, size, memory_flags, range);
#elif defined(ASMJIT_ANONYMOUS_MEMORY_USE_MACH_VM_REMAP)
  return alloc_dual_mapping_using_mach_vm_remap(dm, size, memory_flags, range);
#elif defined(ASMJIT_ANONYMOUS_MEMORY_USE_FD)
  return alloc_dual_mapping_using_file(dm, size, memory_flags, range);
#else
  #error "[asmjit] VirtMem::alloc_dual_mapping() doesn't have implementation for the target OS or architecture"
#endif
#endif // ASMJIT_NO_DUAL_MAPPING
}

Error alloc_dual_mapping(Out<DualMapping> dm, size_t size, MemoryFlags memory_flags) noexcept {
  return alloc_dual_mapping_impl(dm, size, memory_flags, AddressRange{});
}

Error alloc_dual_mapping(Out<DualMapping> dm, size_t size, MemoryFlags memory_flags, AddressRange range) noexcept {
  if (range.start >= range.end) {
    dm = DualMapping{};
    return make_error(Error::kInvalidArgument);
  }
  return alloc_dual_mapping_impl(dm, size, memory_flags, range);
}

Error release_dual_mapping(DualMapping& dm, size_t size) noexcept {
#if defined(ASMJIT_NO_DUAL_MAPPING)
  axl::maybe_unused(dm, size);
  return make_error(Error::kFeatureNotEnabled);
#else
  return unmap_dual_mapping(dm, size);
#endif // ASMJIT_NO_DUAL_MAPPING
}
#endif

// Virtual Memory - Flush Instruction Cache
// ========================================

void flush_instruction_cache(void* p, size_t size) noexcept {
#if ASMJIT_TARGET_ARCH_X86 || defined(__EMSCRIPTEN__)
  // X86|X86_64 architecture doesn't require to do anything to flush instruction cache.
  axl::maybe_unused(p, size);
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
  #pragma message("[asmjit] VirtMem::flush_instruction_cache() doesn't have implementation for the target OS and compiler")
  axl::maybe_unused(p, size);
#endif
}

// Virtual Memory - Memory Info
// ============================

Info info() noexcept {
  static std::atomic<uint32_t> vm_info_initialized;
  static std::atomic<uint32_t> page_size;
  static std::atomic<uint32_t> page_granularity;

  if (!vm_info_initialized.load()) {
    Info local_mem_info;
    detect_vm_info(local_mem_info);

    page_size.store(local_mem_info.page_size, std::memory_order::relaxed);
    page_granularity.store(local_mem_info.page_granularity, std::memory_order::relaxed);
    vm_info_initialized.store(1u, std::memory_order::seq_cst);
  }

  return {
    page_size.load(std::memory_order::relaxed),
    page_granularity.load(std::memory_order::relaxed)
  };
}

size_t large_page_size() noexcept {
  static std::atomic<size_t> large_page_size;
  static constexpr size_t not_available = 1;

  size_t size = large_page_size.load();
  if (ASMJIT_LIKELY(size > not_available)) {
    return size;
  }

  if (size == not_available) {
    return 0;
  }

  size = detect_large_page_size();
  large_page_size.store(size != 0 ? size : not_available);
  return size;
}

// Virtual Memory - Module Address Range
// =====================================

#if defined(_WIN32)
Error module_address_range(uintptr_t addr, Out<AddressRange> range) noexcept {
  MEMORY_BASIC_INFORMATION mbi {};
  if (!::VirtualQuery(reinterpret_cast<const void*>(addr), &mbi, sizeof(mbi)) || mbi.Type != MEM_IMAGE) {
    return make_error(Error::kInvalidArgument);
  }

  // An image is mapped as a single allocation that starts with its headers.
  const uint8_t* base = static_cast<const uint8_t*>(mbi.AllocationBase);
  const IMAGE_DOS_HEADER* dos_header = reinterpret_cast<const IMAGE_DOS_HEADER*>(base);
  if (dos_header->e_magic != IMAGE_DOS_SIGNATURE) {
    return make_error(Error::kInvalidState);
  }

  const IMAGE_NT_HEADERS* nt_headers = reinterpret_cast<const IMAGE_NT_HEADERS*>(base + dos_header->e_lfanew);
  if (nt_headers->Signature != IMAGE_NT_SIGNATURE) {
    return make_error(Error::kInvalidState);
  }

  range = AddressRange{uintptr_t(base), uintptr_t(base) + uintptr_t(nt_headers->OptionalHeader.SizeOfImage)};
  return Error::kOk;
}
#elif defined(__APPLE__)
Error module_address_range(uintptr_t addr, Out<AddressRange> range) noexcept {
#if ASMJIT_TARGET_ARCH_BITS == 64
  using MachHeader = mach_header_64;
  using SegmentCommand = segment_command_64;
  constexpr uint32_t kSegmentCmd = LC_SEGMENT_64;
#else
  using MachHeader = mach_header;
  using SegmentCommand = segment_command;
  constexpr uint32_t kSegmentCmd = LC_SEGMENT;
#endif

  Dl_info dl_info {};
  if (!dladdr(reinterpret_cast<const void*>(addr), &dl_info) || !dl_info.dli_fbase) {
    return make_error(Error::kInvalidArgument);
  }

  const MachHeader* header = static_cast<const MachHeader*>(dl_info.dli_fbase);
  const uint8_t* cmd = reinterpret_cast<const uint8_t*>(header + 1);

  uintptr_t lo = UINTPTR_MAX;
  uintptr_t hi = 0;
  uintptr_t slide = 0;

  for (uint32_t i = 0; i < header->ncmds; i++) {
    const load_command* lc = reinterpret_cast<const load_command*>(cmd);
    cmd += lc->cmdsize;

    if (lc->cmd != kSegmentCmd) {
      continue;
    }

    // Skip __PAGEZERO, which only reserves the low address space of executables.
    const SegmentCommand* seg = reinterpret_cast<const SegmentCommand*>(lc);
    if (seg->initprot == 0 && seg->maxprot == 0) {
      continue;
    }

    // __TEXT starts with the header, which gives the slide of the image.
    if (seg->fileoff == 0 && seg->filesize != 0) {
      slide = uintptr_t(header) - uintptr_t(seg->vmaddr);
    }

    lo = axl::min(lo, uintptr_t(seg->vmaddr));
    hi = axl::max(hi, uintptr_t(seg->vmaddr + seg->vmsize));
  }

  if (lo >= hi) {
    return make_error(Error::kInvalidState);
  }

  range = AddressRange{lo + slide, hi + slide};
  return Error::kOk;
}
#elif defined(ASMJIT_VM_HAS_DL_ITERATE_PHDR)
struct ModuleRangeQuery {
  uintptr_t addr;
  AddressRange range;
};

static int module_address_range_callback(struct dl_phdr_info* phdr_info, size_t, void* data) noexcept {
  ModuleRangeQuery* query = static_cast<ModuleRangeQuery*>(data);

  uintptr_t start = UINTPTR_MAX;
  uintptr_t end = 0;
  bool found = false;

  for (size_t i = 0; i < phdr_info->dlpi_phnum; i++) {
    const auto& phdr = phdr_info->dlpi_phdr[i];
    if (phdr.p_type != PT_LOAD) {
      continue;
    }

    uintptr_t segment_start = uintptr_t(phdr_info->dlpi_addr + phdr.p_vaddr);
    uintptr_t segment_end = segment_start + uintptr_t(phdr.p_memsz);

    start = axl::min(start, segment_start);
    end = axl::max(end, segment_end);
    found |= query->addr >= segment_start && query->addr < segment_end;
  }

  if (!found) {
    return 0;
  }

  query->range = AddressRange{start, end};
  return 1;
}

Error module_address_range(uintptr_t addr, Out<AddressRange> range) noexcept {
  ModuleRangeQuery query { addr, {} };
  if (!dl_iterate_phdr(module_address_range_callback, &query)) {
    return make_error(Error::kInvalidArgument);
  }

  range = query.range;
  return Error::kOk;
}
#else
Error module_address_range(uintptr_t addr, Out<AddressRange> range) noexcept {
  axl::maybe_unused(addr, range);
  return make_error(Error::kFeatureNotEnabled);
}
#endif

// Virtual Memory - Hardened Runtime Info
// ======================================

HardenedRuntimeInfo hardened_runtime_info() noexcept {
  return HardenedRuntimeInfo { get_hardened_runtime_flags() };
}

// Virtual Memory - Project JIT Memory
// ===================================

void protect_jit_memory(ProtectJitAccess access) noexcept {
#if defined(ASMJIT_HAS_PTHREAD_JIT_WRITE_PROTECT_NP)
  pthread_jit_write_protect_np(static_cast<int>(access));
#else
  axl::maybe_unused(access);
#endif
}

ASMJIT_END_SUB_NAMESPACE

#endif // !ASMJIT_NO_JIT
