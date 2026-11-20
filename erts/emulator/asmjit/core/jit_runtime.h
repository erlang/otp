// This file is part of AsmJit project <https://asmjit.com>
//
// See <asmjit/core.h> or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#ifndef ASMJIT_CORE_JIT_RUNTIME_H_INCLUDED
#define ASMJIT_CORE_JIT_RUNTIME_H_INCLUDED

#include <asmjit/core/build_defs.h>
#ifndef ASMJIT_NO_JIT

#include <asmjit/core/code_holder.h>
#include <asmjit/core/jit_allocator.h>
#include <asmjit/core/target.h>

ASMJIT_BEGIN_NAMESPACE

class CodeHolder;

//! \addtogroup asmjit_virtual_memory
//! \{

//! JIT execution runtime is a special `Target` that is designed to store and execute a generated code.
//!
//! JIT runtime is the easiest way of using AsmJit as it abstracts allocation and deallocation of virtual memory
//! where executable code can be placed and from which it can be executed as well.
//!
//! \note Set \ref JitAllocator::CreateParams::near_address to the address of a function the generated code calls
//! to make calls to functions of its module direct (`call rel32` instead of `call [rip + addrtab]` on X86_64).
//!
//! \note JIT runtime understands PAUTH and would automatically authenticate pointers when compiled for `arm64e`
//! targets. This means that if you want to read RAW bytes of acquired pointers, you would have to strip the
//! signature from them via \ref PAuthUtils::strip_c_func_ptr().
class ASMJIT_VIRTAPI JitRuntime : public Target {
public:
  ASMJIT_NONCOPYABLE(JitRuntime)

  //! Virtual memory allocator.
  JitAllocator _allocator;

  //! \name Construction & Destruction
  //! \{

  //! Creates a `JitRuntime` instance.
  //!
  //! Pass non-null `params` (\ref JitAllocator::CreateParams) to configure the properties of the runtime. By default
  //! AsmJit tries to configure a basic working runtime for generating JIT code. To enable advanced features such as
  //! huge pages, dual-mapped X^W pages, or other features, `params` must be setup and passed to the constructor. For
  //! more details, see \ref JitAllocator, which is an underlying allocator of \ref JitRuntime.
  ASMJIT_API explicit JitRuntime(const JitAllocator::CreateParams* params = nullptr) noexcept;

  //! Destroys the `JitRuntime` instance.
  ASMJIT_API ~JitRuntime() noexcept override;

  //! \}

  //! \name Accessors
  //! \{

  //! Resets the \ref JitRuntime, freeing everything that was allocated by it.
  //!
  //! Depending on `reset_policy` the currently held memory can be either freed entirely when ResetPolicy::kHard is used,
  //! or the allocator can keep some of it for next allocations when ResetPolicy::kSoft is used, which is the default
  //! behavior.
  ASMJIT_INLINE_NODEBUG void reset(ResetPolicy reset_policy = ResetPolicy::kSoft) noexcept {
    _allocator.reset(reset_policy);
  }

  //! \}

  //! \name Accessors
  //! \{

  //! Returns the associated `JitAllocator`.
  [[nodiscard]]
  ASMJIT_INLINE_NODEBUG JitAllocator& allocator() const noexcept { return const_cast<JitAllocator&>(_allocator); }

  //! \}

  //! \name Utilities
  //! \{

  // NOTE: To allow passing function pointers to `add()` and `release()` the
  // virtual methods are prefixed with `_` and called from templates instead.

  //! Allocates memory needed for a code stored in the `CodeHolder` and relocates the code to the pointer allocated.
  //!
  //! The beginning of the memory allocated for the function is returned in `dst`. If failed `Error` code is returned
  //! and `dst` is explicitly set to `nullptr`  (this means that you don't have to set it to null before calling `add()`).
  template<typename Func>
  ASMJIT_INLINE_NODEBUG Error add(Func* dst, CodeHolder* code) noexcept {
    return _add(func_ptr_cast_impl<void**, Func*>(dst), code);
  }

  //! Releases `p` which was obtained by calling `add()`.
  template<typename Func>
  ASMJIT_INLINE_NODEBUG Error release(Func p) noexcept {
    return _release(func_ptr_cast_impl<void*, Func>(p));
  }

  //! Type-unsafe version of `add()`.
  ASMJIT_API virtual Error _add(void** dst, CodeHolder* code) noexcept;

  //! Type-unsafe version of `release()`.
  ASMJIT_API virtual Error _release(void* p) noexcept;

  //! \}
};

//! \}

ASMJIT_END_NAMESPACE

#endif
#endif // ASMJIT_CORE_JIT_RUNTIME_H_INCLUDED
