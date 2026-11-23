// This file is part of AsmJit project <https://asmjit.com>
//
// See <asmjit/core.h> or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#ifndef ASMJIT_CORE_GLOBALS_H_INCLUDED
#define ASMJIT_CORE_GLOBALS_H_INCLUDED

#include <asmjit/axl/commons.h>
#include <asmjit/axl/span.h>
#include <asmjit/core/build_defs.h>
#include <asmjit/core/debug_utils.h>

ASMJIT_BEGIN_NAMESPACE

//! \addtogroup asmjit_core
//! \{

//! Contains constants and variables used globally across AsmJit.
namespace Globals {

//! Maximum number of operands per a single instruction.
static constexpr uint32_t kMaxOpCount = 6;

//! Maximum arguments of a function supported by the Compiler / Function API.
static constexpr uint32_t kMaxFuncArgs = 32;

//! The number of values that can be assigned to a single function argument or return value.
static constexpr uint32_t kMaxValuePack = 4;

//! Maximum number of physical registers AsmJit can use per register group.
static constexpr uint32_t kMaxPhysRegs = 32;

//! Maximum alignment.
static constexpr uint32_t kMaxAlignment = 64;

//! Maximum label or symbol size in bytes.
static constexpr uint32_t kMaxLabelNameSize = 2048;

//! Maximum section name size.
static constexpr uint32_t kMaxSectionNameSize = 35;

//! Maximum size of a comment.
static constexpr uint32_t kMaxCommentSize = 1024;

//! Number of virtual register groups.
static constexpr uint32_t kNumVirtGroups = 4;

//! Invalid base address.
static constexpr uint64_t kNoBaseAddress = ~uint64_t(0);

//! Invalid section offset.
static constexpr uint64_t kNoSectionOffset = ~uint64_t(0);

//! Invalid identifier.
static constexpr uint32_t kInvalidId = 0xFFFFFFFFu;

struct NoInit_ {};

//! A decorator used to not initialize.
static const constexpr NoInit_ NoInit {};

} // {Globals}

using axl::BitWord;

//! A policy that can be used with some `reset()` functions.
enum class ResetPolicy : uint32_t {
  //! Soft reset, doesn't deallocate memory (default).
  kSoft = 0,
  //! Hard reset, releases all memory used, if any.
  kHard = 1
};

// Pointer <-> Function Casting
// ============================

//! \cond INTERNAL
//! Cast designed to cast between function and void* pointers.
template<typename Dst, typename Src>
static inline Dst func_ptr_cast_impl(Src p) noexcept { return (Dst)p; }
//! \endcond

//! Casts a `void*` pointer `func` to a function pointer `Func`.
template<typename Func>
static ASMJIT_INLINE_NODEBUG Func ptr_as_func(void* p) noexcept {
  return func_ptr_cast_impl<Func, void*>(p);
}

//! Casts a `void*` pointer `func` to a function pointer `Func`.
template<typename Func>
static ASMJIT_INLINE_NODEBUG Func ptr_as_func(void* p, size_t offset) noexcept {
  return func_ptr_cast_impl<Func, void*>(static_cast<void*>(static_cast<char*>(p) + offset));
}

//! Casts a function pointer `func` to a void pointer `void*`.
template<typename Func>
static ASMJIT_INLINE_NODEBUG void* func_as_ptr(Func func) noexcept {
  return func_ptr_cast_impl<void*, Func>(func);
}
//! \}

ASMJIT_END_NAMESPACE

#endif // ASMJIT_CORE_GLOBALS_H_INCLUDED
