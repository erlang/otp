// This file is part of AsmJit project <https://asmjit.com>
//
// See <asmjit/core.h> or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#ifndef ASMJIT_ARM_A64_COMPILER_H_INCLUDED
#define ASMJIT_ARM_A64_COMPILER_H_INCLUDED

#include <asmjit/core/build_defs.h>
#ifndef ASMJIT_NO_COMPILER

#include <asmjit/axl/commons.h>
#include <asmjit/core/compiler.h>
#include <asmjit/core/type.h>
#include <asmjit/arm/a64_emitter.h>

ASMJIT_BEGIN_SUB_NAMESPACE(a64)

//! \addtogroup asmjit_a64
//! \{

//! AArch64 compiler implementation.
//!
//! ### Compiler Basics
//!
//! The first \ref a64::Compiler example shows how to generate a function that simply returns an integer value. It's
//! an analogy to the first Assembler example:
//!
//! ```
//! #include <asmjit/a64.h>
//! #include <stdio.h>
//!
//! using namespace asmjit;
//!
//! int main() {
//!   using Func = int (*)(void);              // Signature of the generated function.
//!
//!   JitRuntime rt;                           // Runtime specialized for JIT code execution.
//!   CodeHolder code;                         // Holds code and relocation information.
//!
//!   code.init(rt.environment(),              // Initialize code to match the JIT environment.
//!             rt.cpu_features());
//!   a64::Compiler cc(&code);                 // Create and attach a64::Compiler to code.
//!
//!   cc.add_func(FuncSignature::build<int>());// Begin a function of `int fn(void)` signature.
//!
//!   a64::Gp virt_reg = cc.new_gp32();        // Create a 32-bit general purpose register.
//!   cc.mov(virt_reg, 1);                     // Move one to our virtual register `virt_reg`.
//!   cc.ret(virt_reg);                        // Return `virt_reg` from the function.
//!
//!   cc.end_func();                           // End of the function body.
//!   cc.finalize();                           // Translate and assemble the whole 'cc' content.
//!   // ----> a64::Compiler is no longer needed from here and can be destroyed <----
//!
//!   Func fn;
//!   Error err = rt.add(&fn, &code);          // Add the generated code to the runtime.
//!   if (err != Error::kOk) {
//!     return 1;                              // Handle a possible error returned by AsmJit.
//!   }
//!   // ----> CodeHolder is no longer needed from here and can be destroyed <----
//!
//!   int result = fn();                       // Execute the generated code.
//!   printf("%d\n", result);                  // Print the resulting "1".
//!
//!   rt.release(fn);                          // Explicitly remove the function from the runtime.
//!   return 0;
//! }
//! ```
//!
//! ### Pointer authentication (PAUTH)
//!
//! PAUTH is an ARMv8.3-A architecture extension that secures memory by adding a cryptographic signature to
//! unused high bits of 64-bit pointers. Successful use of this extension requires CPU capabilities (PAUTH)
//! and also ABI enablement, for example `arm64e` ABI as used by Apple platforms.
//!
//! The example below shows how to decorate a function call to emit AArch64 code that would seamlessly work
//! on platforms that enforce PAUTH, such as `arm64e`. It's safe to use these decorators as when PAUTH is
//! not enforced the PAUTH information would be simply ignored by the Compiler.
//!
//! ```
//! #include <asmjit/a64.h>
//! #include <stdio.h>
//!
//! using namespace asmjit;
//!
//! static int called_fn() noexcept { return 41; }
//!
//! int main() {
//!   using Func = int (*)(void);              // Signature of the generated function.
//!
//!   JitRuntime rt;                           // Runtime specialized for JIT code execution.
//!   CodeHolder code;                         // Holds code and relocation information.
//!
//!   code.init(rt.environment(),              // Initialize code to match the JIT environment.
//!             rt.cpu_features());
//!   a64::Compiler cc(&code);                 // Create and attach a64::Compiler to code.
//!
//!   cc.add_func(FuncSignature::build<int>());// Begin a function of `int fn(void)` signature.
//!   a64::Gp v = cc.new_gp32();
//!
//!   // InvokeNode represents function invocation.
//!   InvokeNode* invoke_node;
//!   cc.invoke(Out(invoke_node),
//!     (uint64_t)called_fn,                   // Called function pointer (signed on arm64e).
//!     FuncSignature::build<int>(),           // Called function signature.
//!     pauth_c_func());                       // PAuth information that will be used when enforced.
//!
//!   invoke_node->set_ret(0, v);              // Assign the first return value into `v`.
//!   cc.add(v, v, 1);                         // Add 1 to `v`.
//!   cc.ret(v);                               // Return the value returned by `called_fn`.
//!
//!   cc.end_func();                           // End of the function body.
//!   cc.finalize();                           // Translate and assemble the whole 'cc' content.
//!   // ----> a64::Compiler is no longer needed from here and can be destroyed <----
//!
//!   Func fn;
//!   Error err = rt.add(&fn, &code);          // Add the generated code to the runtime.
//!   if (err != Error::kOk) {
//!     return 1;                              // Handle a possible error returned by AsmJit.
//!   }
//!   // ----> CodeHolder is no longer needed from here and can be destroyed <----
//!
//!   int result = fn();                       // Execute the generated code.
//!   printf("%d\n", result);                  // Print the resulting "42".
//!
//!   rt.release(fn);                          // Explicitly remove the function from the runtime.
//!   return 0;
//! }
//! ```

class ASMJIT_VIRTAPI Compiler final
  : public BaseCompiler,
    public EmitterExplicitT<Compiler> {
public:
  ASMJIT_NONCOPYABLE(Compiler)
  using Base = BaseCompiler;

  //! \name Construction & Destruction
  //! \{

  ASMJIT_API explicit Compiler(CodeHolder* code = nullptr) noexcept;
  ASMJIT_API ~Compiler() noexcept override;

  //! \}

  //! \name Virtual Registers
  //! \{

  //! Creates a new general-purpose register with `type_id` type and optional name passed via `args`.
  //!
  //! \note Using \ref TypeId is too generic. In general it's recommended to use \ref new_gp32(),
  //! \ref new_gp64(), and \ref new_gpz() or \ref new_gp_ptr().
  template<typename... Args>
  ASMJIT_INLINE_NODEBUG Gp new_gp(TypeId type_id, Args&&... args) { return new_reg<Gp>(type_id, axl::forward<Args>(args)...); }

  //! Creates a new 32-bit general purpose register mapped to low 32 bits of a full register (on 64-bit targets).
  template<typename... Args>
  ASMJIT_INLINE_NODEBUG Gp new_gp32(Args&&... args) { return new_reg<Gp>(TypeId::kUInt32, axl::forward<Args>(args)...); }

  //! Creates a new 64-bit general purpose register.
  template<typename... Args>
  ASMJIT_INLINE_NODEBUG Gp new_gp64(Args&&... args) { return new_reg<Gp>(TypeId::kUInt64, axl::forward<Args>(args)...); }

  //! Creates a new 32-bit general purpose register.
  //!
  //! \note This is a convenience function alias of \ref new_gp32().
  template<typename... Args>
  ASMJIT_INLINE_NODEBUG Gp new_gpw(Args&&... args) { return new_reg<Gp>(TypeId::kUInt32, axl::forward<Args>(args)...); }

  //! Creates a new 64-bit general purpose register.
  //!
  //! \note This is a convenience function alias of \ref new_gp64().
  template<typename... Args>
  ASMJIT_INLINE_NODEBUG Gp new_gpx(Args&&... args) { return new_reg<Gp>(TypeId::kUInt64, axl::forward<Args>(args)...); }

  //! Creates a new 32-bit or 64-bit general purpose register depending on the target register width.
  //!
  //! \note This is a convenience function, on aarch64 target it always creates a 64-bit general-purpose register.
  template<typename... Args>
  ASMJIT_INLINE_NODEBUG Gp new_gpz(Args&&... args) { return new_reg<Gp>(TypeId::kUIntPtr, axl::forward<Args>(args)...); }

  //! Creates a new 32-bit or 64-bit general purpose register depending on the target register width.
  //!
  //! \note This is a convenience function, on aarch64 target it always creates a 64-bit general-purpose register.
  template<typename... Args>
  ASMJIT_INLINE_NODEBUG Gp new_gp_ptr(Args&&... args) { return new_reg<Gp>(TypeId::kUIntPtr, axl::forward<Args>(args)...); }

  //! Creates a new vector register with `type_id` type and optional name passed via `args`.
  //!
  //! \note Using \ref TypeId is too generic. In general it's recommended to use \ref new_vec128(),
  //! \ref new_vec_s(), \ref new_vec_d(), \ref new_vec_q(), ...
  template<typename... Args>
  ASMJIT_INLINE_NODEBUG Vec new_vec(TypeId type_id, Args&&... args) { return new_reg<Vec>(type_id, axl::forward<Args>(args)...); }

  //! Creates a new 128-bit vector register.
  template<typename... Args>
  ASMJIT_INLINE_NODEBUG Vec new_vec128(Args&&... args) { return new_reg<Vec>(TypeId::kInt32x4, axl::forward<Args>(args)...); }

  //! Creates a new 128-bit vector register that will be used for scalar 32-bit floating point operation.
  template<typename... Args>
  ASMJIT_INLINE_NODEBUG Vec new_vec128_f32x1(Args&&... args) { return new_reg<Vec>(TypeId::kFloat32x1, axl::forward<Args>(args)...); }

  //! Creates a new 128-bit vector register that will be used for scalar 64-bit floating point operation.
  template<typename... Args>
  ASMJIT_INLINE_NODEBUG Vec new_vec128_f64x1(Args&&... args) { return new_reg<Vec>(TypeId::kFloat64x1, axl::forward<Args>(args)...); }

  //! Creates a new 128-bit vector register that will be used for packed 32-bit floating point operation.
  template<typename... Args>
  ASMJIT_INLINE_NODEBUG Vec new_vec128_f32x4(Args&&... args) { return new_reg<Vec>(TypeId::kFloat32x4, axl::forward<Args>(args)...); }

  //! Creates a new 128-bit vector register that will be used for packed 64-bit floating point operation.
  template<typename... Args>
  ASMJIT_INLINE_NODEBUG Vec new_vec128_f64x2(Args&&... args) { return new_reg<Vec>(TypeId::kFloat64x2, axl::forward<Args>(args)...); }

  //! Creates a new 32-bit vector register (S).
  //!
  //! \note This may look like an alias of \ref new_vec128_f32x1(), but it's not. This really creates a 32-bit
  //! register, which has a type \ref RegType::kVec32, whereas \ref new_vec128_f32x1() creates a register,
  //! which has a type \ref RegType::kVec64
  template<typename... Args>
  ASMJIT_INLINE_NODEBUG Vec new_vec_s(Args&&... args) { return new_reg<Vec>(TypeId::kFloat32, axl::forward<Args>(args)...); }

  //! Alias of \ref new_vec128_f64x1() that matches aarch64 architecture terminology.
  template<typename... Args>
  ASMJIT_INLINE_NODEBUG Vec new_vec_d(Args&&... args) { return new_reg<Vec>(TypeId::kFloat64, axl::forward<Args>(args)...); }

  //! Alias of \ref new_vec128() that matches aarch64 architecture terminology.
  template<typename... Args>
  ASMJIT_INLINE_NODEBUG Vec new_vec_q(Args&&... args) { return new_reg<Vec>(TypeId::kInt32x4, axl::forward<Args>(args)...); }

  //! \}

  //! \name Stack
  //! \{

  //! Creates a new stack and returns a \ref Mem operand that can be used to address it.
  ASMJIT_INLINE_NODEBUG Mem new_stack(uint32_t size, uint32_t alignment, const char* name = nullptr) {
    Mem m(Globals::NoInit);
    _new_stack(Out<BaseMem>(m), size, alignment, name);
    return m;
  }

  //! \}

  //! \name Constants
  //! \{

  //! Put data to a constant-pool and get a memory reference to it.
  ASMJIT_INLINE_NODEBUG Mem new_const(ConstPoolScope scope, const void* data, size_t size) {
    Mem m(Globals::NoInit);
    _new_const(Out<BaseMem>(m), scope, data, size);
    return m;
  }

  //! Put a BYTE `val` to a constant-pool (8 bits).
  ASMJIT_INLINE_NODEBUG Mem new_byte_const(ConstPoolScope scope, uint8_t val) { return new_const(scope, &val, 1); }
  //! Put a HWORD `val` to a constant-pool (16 bits).
  ASMJIT_INLINE_NODEBUG Mem new_half_const(ConstPoolScope scope, uint16_t val) { return new_const(scope, &val, 2); }
  //! Put a WORD `val` to a constant-pool (32 bits).
  ASMJIT_INLINE_NODEBUG Mem new_word_const(ConstPoolScope scope, uint32_t val) { return new_const(scope, &val, 4); }
  //! Put a DWORD `val` to a constant-pool (64 bits).
  ASMJIT_INLINE_NODEBUG Mem new_dword_const(ConstPoolScope scope, uint64_t val) { return new_const(scope, &val, 8); }

  //! Put a WORD `val` to a constant-pool.
  ASMJIT_INLINE_NODEBUG Mem new_int16_const(ConstPoolScope scope, int16_t val) { return new_const(scope, &val, 2); }
  //! Put a WORD `val` to a constant-pool.
  ASMJIT_INLINE_NODEBUG Mem new_uint16_const(ConstPoolScope scope, uint16_t val) { return new_const(scope, &val, 2); }
  //! Put a DWORD `val` to a constant-pool.
  ASMJIT_INLINE_NODEBUG Mem new_int32_const(ConstPoolScope scope, int32_t val) { return new_const(scope, &val, 4); }
  //! Put a DWORD `val` to a constant-pool.
  ASMJIT_INLINE_NODEBUG Mem new_uint32_const(ConstPoolScope scope, uint32_t val) { return new_const(scope, &val, 4); }
  //! Put a QWORD `val` to a constant-pool.
  ASMJIT_INLINE_NODEBUG Mem new_int64_const(ConstPoolScope scope, int64_t val) { return new_const(scope, &val, 8); }
  //! Put a QWORD `val` to a constant-pool.
  ASMJIT_INLINE_NODEBUG Mem new_uint64_const(ConstPoolScope scope, uint64_t val) { return new_const(scope, &val, 8); }

  //! Put a SP-FP `val` to a constant-pool.
  ASMJIT_INLINE_NODEBUG Mem new_float_const(ConstPoolScope scope, float val) { return new_const(scope, &val, 4); }
  //! Put a DP-FP `val` to a constant-pool.
  ASMJIT_INLINE_NODEBUG Mem new_double_const(ConstPoolScope scope, double val) { return new_const(scope, &val, 8); }

  //! \}

  //! \name Instruction Options
  //! \{

  //! Force the compiler to not follow the conditional or unconditional jump.
  ASMJIT_INLINE_NODEBUG Compiler& unfollow() noexcept { _inst_options |= InstOptions::kUnfollow; return *this; }

  //! \}

  //! \name Compiler specific
  //! \{

  //! Special pseudo-instruction that can be used to load a memory address into `o0` GP register.
  //!
  //! This instruction has the same semantics as the X86 `LEA` (load effective address) instruction.
  //! Label-based addresses use a PC-relative `ADR` instruction and must fit its signed 21-bit displacement.
  ASMJIT_INLINE_NODEBUG Error load_address_of(const Gp& o0, const Mem& o1) { return _emitter()->_emitI(Inst::kIdAdr, o0, o1); }

  //! \}

  //! \name Function Call & Ret Intrinsics
  //! \{

  //! Invoke a function call without `target` type enforcement.
  ASMJIT_INLINE_NODEBUG Error invoke_(Out<InvokeNode*> out, const Operand_& target, const FuncSignature& signature) {
    return add_invoke_node(out, Inst::kIdBl, target, signature);
  }

  //! \overload
  ASMJIT_INLINE_NODEBUG Error invoke_(Out<InvokeNode*> out, const Operand_& target, const FuncSignature& signature, const PAuthInfo& pauth_info) {
    return add_invoke_node(out, Inst::kIdBl, target, signature, pauth_info);
  }

  //! Invoke a function call of the given `target` and `signature` and store the added node to `out`.
  ASMJIT_INLINE_NODEBUG Error invoke(Out<InvokeNode*> out, const Gp& target, const FuncSignature& signature) { return invoke_(out, target, signature); }
  //! \overload
  ASMJIT_INLINE_NODEBUG Error invoke(Out<InvokeNode*> out, const Mem& target, const FuncSignature& signature) { return invoke_(out, target, signature); }
  //! \overload
  ASMJIT_INLINE_NODEBUG Error invoke(Out<InvokeNode*> out, const Label& target, const FuncSignature& signature) { return invoke_(out, target, signature); }
  //! \overload
  ASMJIT_INLINE_NODEBUG Error invoke(Out<InvokeNode*> out, const Imm& target, const FuncSignature& signature) { return invoke_(out, target, signature); }
  //! \overload
  ASMJIT_INLINE_NODEBUG Error invoke(Out<InvokeNode*> out, uint64_t target, const FuncSignature& signature) { return invoke_(out, Imm(int64_t(target)), signature); }

  //! Invoke a function call of the given `target`, `signature`, and `pauth_info` and store the added node to `out`.
  ASMJIT_INLINE_NODEBUG Error invoke(Out<InvokeNode*> out, const Gp& target, const FuncSignature& signature, const PAuthInfo& pauth_info) { return invoke_(out, target, signature, pauth_info); }
  //! \overload
  ASMJIT_INLINE_NODEBUG Error invoke(Out<InvokeNode*> out, const Mem& target, const FuncSignature& signature, const PAuthInfo& pauth_info) { return invoke_(out, target, signature, pauth_info); }
  //! \overload
  ASMJIT_INLINE_NODEBUG Error invoke(Out<InvokeNode*> out, const Label& target, const FuncSignature& signature, const PAuthInfo& pauth_info) { return invoke_(out, target, signature, pauth_info); }
  //! \overload
  ASMJIT_INLINE_NODEBUG Error invoke(Out<InvokeNode*> out, const Imm& target, const FuncSignature& signature, const PAuthInfo& pauth_info) { return invoke_(out, target, signature, pauth_info); }
  //! \overload
  ASMJIT_INLINE_NODEBUG Error invoke(Out<InvokeNode*> out, uint64_t target, const FuncSignature& signature, const PAuthInfo& pauth_info) { return invoke_(out, Imm(int64_t(target)), signature, pauth_info); }

  //! Return from function.
  //!
  //! \note This doesn't end the function - it just emits a return.
  ASMJIT_INLINE_NODEBUG Error ret() { return add_ret(Operand(), Operand()); }

  //! Return from function - one value.
  //!
  //! \note This doesn't end the function - it just emits a return.
  ASMJIT_INLINE_NODEBUG Error ret(const Reg& o0) { return add_ret(o0, Operand()); }

  //! Return from function - two values / register pair.
  //!
  //! \note This doesn't end the function - it just emits a return.
  ASMJIT_INLINE_NODEBUG Error ret(const Reg& o0, const Reg& o1) { return add_ret(o0, o1); }

  //! \}

  //! \name Jump Tables Support
  //! \{

  using EmitterExplicitT<Compiler>::br;

  //! Adds a jump to the given `target` with the provided jump `annotation`.
  ASMJIT_INLINE_NODEBUG Error br(const Reg& target, JumpAnnotation* annotation) { return emit_annotated_jump(Inst::kIdBr, target, annotation); }

  //! \}

  //! \name Events
  //! \{

  ASMJIT_API Error on_attach(CodeHolder& code) noexcept override;
  ASMJIT_API Error on_detach(CodeHolder& code) noexcept override;
  ASMJIT_API Error on_reinit(CodeHolder& code) noexcept override;

  //! \}

  //! \name Finalize
  //! \{

  ASMJIT_API Error finalize() override;

  //! \}
};

//! \}

ASMJIT_END_SUB_NAMESPACE

#endif // !ASMJIT_NO_COMPILER
#endif // ASMJIT_ARM_A64_COMPILER_H_INCLUDED
