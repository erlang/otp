// This file is part of AsmJit project <https://asmjit.com>
//
// See asmjit.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#ifndef ASMJIT_CORE_ENVIRONMENT_H_INCLUDED
#define ASMJIT_CORE_ENVIRONMENT_H_INCLUDED

#include "../core/archtraits.h"

#if defined(__APPLE__)
  #include <TargetConditionals.h>
#endif

ASMJIT_BEGIN_NAMESPACE

//! \addtogroup asmjit_core
//! \{

//! Vendor.
//!
//! \note AsmJit doesn't use vendor information at the moment. It's provided for future use, if required.
enum class Vendor : uint8_t {
  //! Unknown or uninitialized platform vendor.
  kUnknown = 0,

  //! Maximum value of `PlatformVendor`.
  kMaxValue = kUnknown,

  //! Platform vendor detected at compile-time.
  kHost =
#if defined(_DOXYGEN)
    DETECTED_AT_COMPILE_TIME
#else
    kUnknown
#endif
};

//! Platform - runtime environment or operating system.
enum class Platform : uint8_t {
  //! Unknown or uninitialized platform.
  kUnknown = 0,

  //! Windows OS.
  kWindows,

  //! Other platform that is not Windows, most likely POSIX based.
  kOther,

  //! Linux OS.
  kLinux,
  //! GNU/Hurd OS.
  kHurd,

  //! FreeBSD OS.
  kFreeBSD,
  //! OpenBSD OS.
  kOpenBSD,
  //! NetBSD OS.
  kNetBSD,
  //! DragonFly BSD OS.
  kDragonFlyBSD,

  //! Haiku OS.
  kHaiku,

  //! Apple OSX.
  kOSX,
  //! Apple iOS.
  kIOS,
  //! Apple TVOS.
  kTVOS,
  //! Apple WatchOS.
  kWatchOS,

  //! Emscripten platform.
  kEmscripten,

  //! Maximum value of `Platform`.
  kMaxValue = kEmscripten,

  //! Platform detected at compile-time (platform of the host).
  kHost =
#if defined(_DOXYGEN)
    DETECTED_AT_COMPILE_TIME
#elif defined(__EMSCRIPTEN__)
    kEmscripten
#elif defined(_WIN32)
    kWindows
#elif defined(__linux__)
    kLinux
#elif defined(__gnu_hurd__)
    kHurd
#elif defined(__FreeBSD__)
    kFreeBSD
#elif defined(__OpenBSD__)
    kOpenBSD
#elif defined(__NetBSD__)
    kNetBSD
#elif defined(__DragonFly__)
    kDragonFlyBSD
#elif defined(__HAIKU__)
    kHaiku
#elif defined(__APPLE__) && TARGET_OS_OSX
    kOSX
#elif defined(__APPLE__) && TARGET_OS_TV
    kTVOS
#elif defined(__APPLE__) && TARGET_OS_WATCH
    kWatchOS
#elif defined(__APPLE__) && TARGET_OS_IPHONE
    kIOS
#else
    kOther
#endif
};

//! Platform ABI (application binary interface).
enum class PlatformABI : uint8_t {
  //! Unknown or uninitialied environment.
  kUnknown = 0,
  //! Microsoft ABI.
  kMSVC,
  //! GNU ABI.
  kGNU,
  //! Android Environment / ABI.
  kAndroid,
  //! Cygwin ABI.
  kCygwin,

  //! Maximum value of `PlatformABI`.
  kMaxValue,

  //! Host ABI detected at compile-time.
  kHost =
#if defined(_DOXYGEN)
    DETECTED_AT_COMPILE_TIME
#elif defined(_MSC_VER)
    kMSVC
#elif defined(__CYGWIN__)
    kCygwin
#elif defined(__MINGW32__) || defined(__GLIBC__)
    kGNU
#elif defined(__ANDROID__)
    kAndroid
#else
    kUnknown
#endif
};

//! Object format.
//!
//! \note AsmJit doesn't really use anything except \ref ObjectFormat::kUnknown and \ref ObjectFormat::kJIT at
//! the moment. Object file formats are provided for future extensibility and a possibility to generate object
//! files at some point.
enum class ObjectFormat : uint8_t {
  //! Unknown or uninitialized object format.
  kUnknown = 0,

  //! JIT code generation object, most likely \ref JitRuntime or a custom
  //! \ref Target implementation.
  kJIT,

  //! Executable and linkable format (ELF).
  kELF,
  //! Common object file format.
  kCOFF,
  //! Extended COFF object format.
  kXCOFF,
  //! Mach object file format.
  kMachO,

  //! Maximum value of `ObjectFormat`.
  kMaxValue
};

//! Represents an environment, which is usually related to a \ref Target.
//!
//! Environment has usually an 'arch-subarch-vendor-os-abi' format, which is sometimes called "Triple" (historically
//! it used to be 3 only parts) or "Tuple", which is a convention used by Debian Linux.
//!
//! AsmJit doesn't support all possible combinations or architectures and ABIs, however, it models the environment
//! similarly to other compilers for future extensibility.
class Environment {
public:
  //! \name Members
  //! \{

  //! Architecture.
  Arch _arch;
  //! Sub-architecture type.
  SubArch _subArch;
  //! Vendor type.
  Vendor _vendor;
  //! Platform.
  Platform _platform;
  //! Platform ABI.
  PlatformABI _platformABI;
  //! Object format.
  ObjectFormat _objectFormat;
  //! Reserved for future use, must be zero.
  uint8_t _reserved[2];

  //! \}

  //! \name Construction & Destruction
  //! \{

  inline Environment() noexcept :
    _arch(Arch::kUnknown),
    _subArch(SubArch::kUnknown),
    _vendor(Vendor::kUnknown),
    _platform(Platform::kUnknown),
    _platformABI(PlatformABI::kUnknown),
    _objectFormat(ObjectFormat::kUnknown),
    _reserved { 0, 0 } {}

  inline explicit Environment(
    Arch arch,
    SubArch subArch = SubArch::kUnknown,
    Vendor vendor = Vendor::kUnknown,
    Platform platform = Platform::kUnknown,
    PlatformABI abi = PlatformABI::kUnknown,
    ObjectFormat objectFormat = ObjectFormat::kUnknown) noexcept {

    init(arch, subArch, vendor, platform, abi, objectFormat);
  }

  inline Environment(const Environment& other) noexcept = default;

  //! Returns the host environment constructed from preprocessor macros defined by the compiler.
  //!
  //! The returned environment should precisely match the target host architecture, sub-architecture, platform,
  //! and ABI.
  static inline Environment host() noexcept {
    return Environment(Arch::kHost, SubArch::kHost, Vendor::kHost, Platform::kHost, PlatformABI::kHost, ObjectFormat::kUnknown);
  }

  //! \}

  //! \name Overloaded Operators
  //! \{

  inline Environment& operator=(const Environment& other) noexcept = default;

  inline bool operator==(const Environment& other) const noexcept { return  equals(other); }
  inline bool operator!=(const Environment& other) const noexcept { return !equals(other); }

  //! \}

  //! \name Accessors
  //! \{

  //! Tests whether the environment is not set up.
  //!
  //! Returns true if all members are zero, and thus unknown.
  inline bool empty() const noexcept {
    // Unfortunately compilers won't optimize fields are checked one by one...
    return _packed() == 0;
  }

  //! Tests whether the environment is initialized, which means it must have
  //! a valid architecture.
  inline bool isInitialized() const noexcept {
    return _arch != Arch::kUnknown;
  }

  inline uint64_t _packed() const noexcept {
    uint64_t x;
    memcpy(&x, this, 8);
    return x;
  }

  //! Resets all members of the environment to zero / unknown.
  inline void reset() noexcept {
    _arch = Arch::kUnknown;
    _subArch = SubArch::kUnknown;
    _vendor = Vendor::kUnknown;
    _platform = Platform::kUnknown;
    _platformABI = PlatformABI::kUnknown;
    _objectFormat = ObjectFormat::kUnknown;
    _reserved[0] = 0;
    _reserved[1] = 0;
  }

  inline bool equals(const Environment& other) const noexcept {
    return _packed() == other._packed();
  }

  //! Returns the architecture.
  inline Arch arch() const noexcept { return _arch; }
  //! Returns the sub-architecture.
  inline SubArch subArch() const noexcept { return _subArch; }
  //! Returns vendor.
  inline Vendor vendor() const noexcept { return _vendor; }
  //! Returns target's platform or operating system.
  inline Platform platform() const noexcept { return _platform; }
  //! Returns target's ABI.
  inline PlatformABI platformABI() const noexcept { return _platformABI; }
  //! Returns target's object format.
  inline ObjectFormat objectFormat() const noexcept { return _objectFormat; }

  inline void init(
    Arch arch,
    SubArch subArch = SubArch::kUnknown,
    Vendor vendor = Vendor::kUnknown,
    Platform platform = Platform::kUnknown,
    PlatformABI platformABI = PlatformABI::kUnknown,
    ObjectFormat objectFormat = ObjectFormat::kUnknown) noexcept {

    _arch = arch;
    _subArch = subArch;
    _vendor = vendor;
    _platform = platform;
    _platformABI = platformABI;
    _objectFormat = objectFormat;
    _reserved[0] = 0;
    _reserved[1] = 0;
  }

  inline bool isArchX86() const noexcept { return _arch == Arch::kX86; }
  inline bool isArchX64() const noexcept { return _arch == Arch::kX64; }
  inline bool isArchARM() const noexcept { return isArchARM(_arch); }
  inline bool isArchThumb() const noexcept { return isArchThumb(_arch); }
  inline bool isArchAArch64() const noexcept { return isArchAArch64(_arch); }
  inline bool isArchMIPS32() const noexcept { return isArchMIPS32(_arch); }
  inline bool isArchMIPS64() const noexcept { return isArchMIPS64(_arch); }
  inline bool isArchRISCV32() const noexcept { return _arch == Arch::kRISCV32; }
  inline bool isArchRISCV64() const noexcept { return _arch == Arch::kRISCV64; }

  //! Tests whether the architecture is 32-bit.
  inline bool is32Bit() const noexcept { return is32Bit(_arch); }
  //! Tests whether the architecture is 64-bit.
  inline bool is64Bit() const noexcept { return is64Bit(_arch); }

  //! Tests whether the architecture is little endian.
  inline bool isLittleEndian() const noexcept { return isLittleEndian(_arch); }
  //! Tests whether the architecture is big endian.
  inline bool isBigEndian() const noexcept { return isBigEndian(_arch); }

  //! Tests whether this architecture is of X86 family.
  inline bool isFamilyX86() const noexcept { return isFamilyX86(_arch); }
  //! Tests whether this architecture family is ARM, THUMB, or AArch64.
  inline bool isFamilyARM() const noexcept { return isFamilyARM(_arch); }
  //! Tests whether this architecture family is AArch32 (ARM or THUMB).
  inline bool isFamilyAArch32() const noexcept { return isFamilyAArch32(_arch); }
  //! Tests whether this architecture family is AArch64.
  inline bool isFamilyAArch64() const noexcept { return isFamilyAArch64(_arch); }
  //! Tests whether this architecture family is MISP or MIPS64.
  inline bool isFamilyMIPS() const noexcept { return isFamilyMIPS(_arch); }
  //! Tests whether this architecture family is RISC-V (both 32-bit and 64-bit).
  inline bool isFamilyRISCV() const noexcept { return isFamilyRISCV(_arch); }

  //! Tests whether the environment platform is Windows.
  inline bool isPlatformWindows() const noexcept { return _platform == Platform::kWindows; }

  //! Tests whether the environment platform is Linux.
  inline bool isPlatformLinux() const noexcept { return _platform == Platform::kLinux; }

  //! Tests whether the environment platform is Hurd.
  inline bool isPlatformHurd() const noexcept { return _platform == Platform::kHurd; }

  //! Tests whether the environment platform is Haiku.
  inline bool isPlatformHaiku() const noexcept { return _platform == Platform::kHaiku; }

  //! Tests whether the environment platform is any BSD.
  inline bool isPlatformBSD() const noexcept {
    return _platform == Platform::kFreeBSD ||
           _platform == Platform::kOpenBSD ||
           _platform == Platform::kNetBSD ||
           _platform == Platform::kDragonFlyBSD;
  }

  //! Tests whether the environment platform is any Apple platform (OSX, iOS, TVOS, WatchOS).
  inline bool isPlatformApple() const noexcept {
    return _platform == Platform::kOSX ||
           _platform == Platform::kIOS ||
           _platform == Platform::kTVOS ||
           _platform == Platform::kWatchOS;
  }

  //! Tests whether the ABI is MSVC.
  inline bool isMSVC() const noexcept { return _platformABI == PlatformABI::kMSVC; }
  //! Tests whether the ABI is GNU.
  inline bool isGNU() const noexcept { return _platformABI == PlatformABI::kGNU; }

  //! Returns a calculated stack alignment for this environment.
  ASMJIT_API uint32_t stackAlignment() const noexcept;

  //! Returns a native register size of this architecture.
  uint32_t registerSize() const noexcept { return registerSizeFromArch(_arch); }

  //! Sets the architecture to `arch`.
  inline void setArch(Arch arch) noexcept { _arch = arch; }
  //! Sets the sub-architecture to `subArch`.
  inline void setSubArch(SubArch subArch) noexcept { _subArch = subArch; }
  //! Sets the vendor to `vendor`.
  inline void setVendor(Vendor vendor) noexcept { _vendor = vendor; }
  //! Sets the platform to `platform`.
  inline void setPlatform(Platform platform) noexcept { _platform = platform; }
  //! Sets the ABI to `platformABI`.
  inline void setPlatformABI(PlatformABI platformABI) noexcept { _platformABI = platformABI; }
  //! Sets the object format to `objectFormat`.
  inline void setObjectFormat(ObjectFormat objectFormat) noexcept { _objectFormat = objectFormat; }

  //! \}

  //! \name Static Utilities
  //! \{

  static inline bool isDefinedArch(Arch arch) noexcept {
    return uint32_t(arch) <= uint32_t(Arch::kMaxValue);
  }

  static inline bool isValidArch(Arch arch) noexcept {
    return arch != Arch::kUnknown && uint32_t(arch) <= uint32_t(Arch::kMaxValue);
  }

  //! Tests whether the given architecture `arch` is 32-bit.
  static inline bool is32Bit(Arch arch) noexcept {
    return (uint32_t(arch) & uint32_t(Arch::k32BitMask)) == uint32_t(Arch::k32BitMask);
  }

  //! Tests whether the given architecture `arch` is 64-bit.
  static inline bool is64Bit(Arch arch) noexcept {
    return (uint32_t(arch) & uint32_t(Arch::k32BitMask)) == 0;
  }

  //! Tests whether the given architecture `arch` is little endian.
  static inline bool isLittleEndian(Arch arch) noexcept {
    return uint32_t(arch) < uint32_t(Arch::kBigEndian);
  }

  //! Tests whether the given architecture `arch` is big endian.
  static inline bool isBigEndian(Arch arch) noexcept {
    return uint32_t(arch) >= uint32_t(Arch::kBigEndian);
  }

  //! Tests whether the given architecture is Thumb or Thumb_BE.
  static inline bool isArchThumb(Arch arch) noexcept {
    return arch == Arch::kThumb || arch == Arch::kThumb_BE;
  }

  //! Tests whether the given architecture is ARM or ARM_BE.
  static inline bool isArchARM(Arch arch) noexcept {
    return arch == Arch::kARM || arch == Arch::kARM_BE;
  }

  //! Tests whether the given architecture is AArch64 or AArch64_BE.
  static inline bool isArchAArch64(Arch arch) noexcept {
    return arch == Arch::kAArch64 || arch == Arch::kAArch64_BE;
  }

  //! Tests whether the given architecture is MIPS32_LE or MIPS32_BE.
  static inline bool isArchMIPS32(Arch arch) noexcept {
    return arch == Arch::kMIPS32_LE || arch == Arch::kMIPS32_BE;
  }

  //! Tests whether the given architecture is MIPS64_LE or MIPS64_BE.
  static inline bool isArchMIPS64(Arch arch) noexcept {
    return arch == Arch::kMIPS64_LE || arch == Arch::kMIPS64_BE;
  }

  //! Tests whether the given architecture family is X86 or X64.
  static inline bool isFamilyX86(Arch arch) noexcept {
    return arch == Arch::kX86 || arch == Arch::kX64;
  }

  //! Tests whether the given architecture family is ARM, THUMB, or AArch64.
  static inline bool isFamilyARM(Arch arch) noexcept {
    return isArchARM(arch) || isArchAArch64(arch) || isArchThumb(arch);
  }

  //! Tests whether the given architecture family is AArch32 (ARM or THUMB).
  static inline bool isFamilyAArch32(Arch arch) noexcept {
    return isArchARM(arch) || isArchThumb(arch);
  }

  //! Tests whether the given architecture family is AArch64.
  static inline bool isFamilyAArch64(Arch arch) noexcept {
    return isArchAArch64(arch);
  }

  //! Tests whether the given architecture family is MISP or MIPS64.
  static inline bool isFamilyMIPS(Arch arch) noexcept {
    return isArchMIPS32(arch) || isArchMIPS64(arch);
  }

  //! Tests whether the given architecture family is RISC-V (both 32-bit and 64-bit).
  static inline bool isFamilyRISCV(Arch arch) noexcept {
    return arch == Arch::kRISCV32 || arch == Arch::kRISCV64;
  }

  //! Returns a native general purpose register size from the given architecture.
  static inline uint32_t registerSizeFromArch(Arch arch) noexcept {
    return is32Bit(arch) ? 4u : 8u;
  }

  //! \}
};

static_assert(sizeof(Environment) == 8,
              "Environment must occupy exactly 8 bytes.");

//! \}

ASMJIT_END_NAMESPACE

#endif // ASMJIT_CORE_ENVIRONMENT_H_INCLUDED
