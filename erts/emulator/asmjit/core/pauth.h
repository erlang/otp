// This file is part of AsmJit project <https://asmjit.com>
//
// See <asmjit/core.h> or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#ifndef ASMJIT_CORE_PTR_AUTH_H_INCLUDED
#define ASMJIT_CORE_PTR_AUTH_H_INCLUDED

#include <asmjit/core/build_defs.h>

ASMJIT_BEGIN_NAMESPACE

//! \addtogroup asmjit_core
//! \{

//! Pointer authentication key (hardware key slot).
enum class PAuthKey : uint32_t {
  //! No key, generally means no pointer authentication.
  kNone = 0u,

 //! AArch64-ASIA key.
  kAArch64ASIA,
  //! AArch64-ASIB key.
  kAArch64ASIB,
  //! AArch64-ASDA key.
  kAArch64ASDA,
  //! AArch64-ASDB key.
  kAArch64ASDB,

  //! A process-independent key which can be used to sign code pointers (AArch64).
  kAArch64ProcessIndependentCode = kAArch64ASIA,

  // A process-specific key which can be used to sign code pointers (AArch64).
  kAArch64ProcessDependentCode = kAArch64ASIB,

  // A process-independent key which can be used to sign data pointers (AArch64).
  kAArch64ProcessIndependentData = kAArch64ASDA,

  // A process-specific key which can be used to sign data pointers (AArch64).
  kAArch64ProcessDependentData = kAArch64ASDB,

  //! "C function pointer" key that will get translated to an ABI compliant key.
  kFuncPtr,
  //! "Return address" key that will get translated to an ABI compliant key.
  kRetAddr,
  //! "Frame pointer" key that will get translated to an ABI compliant key.
  kFramePtr,
  //! "C++ virtual table pointer" key that will get translated to an ABI compliant key.
  kVTablePtr,

  //! Maximum value of PAuthKey.
  kMaxValue = kVTablePtr
};

//! Pointer authentication flags (for future use).
enum class PAuthFlags : uint32_t {
  kNone = 0u
};
ASMJIT_DEFINE_ENUM_FLAGS(PAuthFlags)

//! Pointer authentication information.
struct PAuthInfo {
  //! \name Members
  //! \{

  PAuthKey _key = PAuthKey::kNone;
  PAuthFlags _flags = PAuthFlags::kNone;
  uint64_t _modifier = 0u;

  //! \}

  //! \name Accessors
  //! \{

  ASMJIT_INLINE_NODEBUG bool has_authentication() const noexcept { return _key != PAuthKey::kNone; }

  ASMJIT_INLINE_CONSTEXPR PAuthKey key() const noexcept { return _key; }
  ASMJIT_INLINE_CONSTEXPR PAuthFlags flags() const noexcept { return _flags; }
  ASMJIT_INLINE_CONSTEXPR uint64_t modifier() const noexcept { return _modifier; }

  ASMJIT_INLINE_NODEBUG void set_key(PAuthKey key) noexcept { _key = key; }
  ASMJIT_INLINE_NODEBUG void set_flags(PAuthFlags flags) noexcept { _flags = flags; }
  ASMJIT_INLINE_NODEBUG void set_modifier(uint64_t modifier) noexcept { _modifier = modifier; }

  //! \}
};

//! Creates a pointer authentication information that has the given `key` and `modifier`.
static ASMJIT_INLINE_CONSTEXPR PAuthInfo pauth_info(PAuthKey key, uint64_t modifier = 0u) noexcept {
  return {key, PAuthFlags::kNone, modifier};
}

//! Creates a pointer authentication information that has the given `key`, `flags`, and `modifier`.
static ASMJIT_INLINE_CONSTEXPR PAuthInfo pauth_info(PAuthKey key, PAuthFlags flags, uint64_t modifier) noexcept {
  return {key, flags, modifier};
}

static ASMJIT_INLINE_CONSTEXPR PAuthInfo pauth_c_func(uint64_t modifier = 0u) noexcept {
  return pauth_info(PAuthKey::kFuncPtr, PAuthFlags::kNone, modifier);
}

//! Pointer authentication utilities.
namespace PAuthUtils {

static constexpr bool is_pauth_enforced() noexcept {
#if defined(__APPLE__) && defined(__arm64e__)
  return true;
#else
  return false;
#endif
}

//! Authenticates the given pointer `ptr` as a C function pointer.
//!
//! \remarks Returns the original `ptr` if pointer authentication is not enforced by ABI.
ASMJIT_API void* sign_c_func_ptr(void* ptr) noexcept;

//! Strips an authenticated C function pointer `ptr`.
//!
//! \remarks Returns the original `ptr` if pointer authentication is not enforced by ABI.
ASMJIT_API void* strip_c_func_ptr(void* ptr) noexcept;

} // {PAuthUtils}

//! \}

ASMJIT_END_NAMESPACE

#endif // ASMJIT_CORE_PTR_AUTH_H_INCLUDED
