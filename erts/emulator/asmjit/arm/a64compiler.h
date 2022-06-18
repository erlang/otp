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

#ifndef ASMJIT_ARM_ARMCOMPILER_H_INCLUDED
#define ASMJIT_ARM_ARMCOMPILER_H_INCLUDED

#include "../core/api-config.h"
#ifndef ASMJIT_NO_COMPILER

#include "../core/compiler.h"
#include "../core/datatypes.h"
#include "../core/type.h"
#include "../arm/a64emitter.h"

ASMJIT_BEGIN_SUB_NAMESPACE(a64)

//! \addtogroup asmjit_a64
//! \{

// ============================================================================
// [asmjit::a64::Compiler]
// ============================================================================

//! AArch64 compiler implementation.
class ASMJIT_VIRTAPI Compiler
  : public BaseCompiler,
    public EmitterExplicitT<Compiler> {
public:
  ASMJIT_NONCOPYABLE(Compiler)
  typedef BaseCompiler Base;

  //! \name Construction & Destruction
  //! \{

  ASMJIT_API explicit Compiler(CodeHolder* code = nullptr) noexcept;
  ASMJIT_API virtual ~Compiler() noexcept;

  //! \}

  //! \name Virtual Registers
  //! \{

  //! \cond INTERNAL
  template<typename RegT, typename Type>
  ASMJIT_INLINE RegT _newRegInternal(const Type& type) {
    RegT reg(Globals::NoInit);
    _newReg(&reg, type, nullptr);
    return reg;
  }

  template<typename RegT, typename Type, typename... Args>
  ASMJIT_INLINE RegT _newRegInternal(const Type& type, const char* s, Args&&... args) {
#ifndef ASMJIT_NO_LOGGING
    RegT reg(Globals::NoInit);
    if (sizeof...(Args) == 0)
      _newReg(&reg, type, s);
    else
      _newRegFmt(&reg, type, s, std::forward<Args>(args)...);
    return reg;
#else
    DebugUtils::unused(std::forward<Args>(args)...);
    RegT reg(Globals::NoInit);
    _newReg(&reg, type, nullptr);
    return reg;
#endif
  }
  //! \endcond

  template<typename RegT, typename... Args>
  inline RegT newSimilarReg(const RegT& ref, Args&&... args) {
    return _newRegInternal<RegT>(ref, std::forward<Args>(args)...);
  }

  template<typename... Args>
  inline Reg newReg(uint32_t typeId, Args&&... args) { return _newRegInternal<Reg>(typeId, std::forward<Args>(args)...); }

  template<typename... Args>
  inline Gp newGp(uint32_t typeId, Args&&... args) { return _newRegInternal<Gp>(typeId, std::forward<Args>(args)...); }

  template<typename... Args>
  inline Vec newVec(uint32_t typeId, Args&&... args) { return _newRegInternal<Vec>(typeId, std::forward<Args>(args)...); }

  template<typename... Args>
  inline Gp newInt32(Args&&... args) { return _newRegInternal<Gp>(Type::kIdI32, std::forward<Args>(args)...); }
  template<typename... Args>
  inline Gp newUInt32(Args&&... args) { return _newRegInternal<Gp>(Type::kIdU32, std::forward<Args>(args)...); }

  template<typename... Args>
  inline Gp newInt64(Args&&... args) { return _newRegInternal<Gp>(Type::kIdI64, std::forward<Args>(args)...); }
  template<typename... Args>
  inline Gp newUInt64(Args&&... args) { return _newRegInternal<Gp>(Type::kIdU64, std::forward<Args>(args)...); }

  template<typename... Args>
  inline Gp newIntPtr(Args&&... args) { return _newRegInternal<Gp>(Type::kIdIntPtr, std::forward<Args>(args)...); }
  template<typename... Args>
  inline Gp newUIntPtr(Args&&... args) { return _newRegInternal<Gp>(Type::kIdUIntPtr, std::forward<Args>(args)...); }

  template<typename... Args>
  inline Gp newGpw(Args&&... args) { return _newRegInternal<Gp>(Type::kIdU32, std::forward<Args>(args)...); }
  template<typename... Args>
  inline Gp newGpx(Args&&... args) { return _newRegInternal<Gp>(Type::kIdU64, std::forward<Args>(args)...); }
  template<typename... Args>
  inline Gp newGpz(Args&&... args) { return _newRegInternal<Gp>(Type::kIdUIntPtr, std::forward<Args>(args)...); }

  template<typename... Args>
  inline Vec newVecS(Args&&... args) { return _newRegInternal<Vec>(Type::kIdF32, std::forward<Args>(args)...); }

  template<typename... Args>
  inline Vec newVecD(Args&&... args) { return _newRegInternal<Vec>(Type::kIdF64, std::forward<Args>(args)...); }

  template<typename... Args>
  inline Vec newVecQ(Args&&... args) { return _newRegInternal<Vec>(Type::kIdU8x16, std::forward<Args>(args)...); }

  //! \}

  //! \name Stack
  //! \{

  //! Creates a new memory chunk allocated on the current function's stack.
  inline Mem newStack(uint32_t size, uint32_t alignment, const char* name = nullptr) {
    Mem m(Globals::NoInit);
    _newStack(&m, size, alignment, name);
    return m;
  }

  //! \}

  //! \name Constants
  //! \{

  //! Put data to a constant-pool and get a memory reference to it.
  inline Mem newConst(uint32_t scope, const void* data, size_t size) {
    Mem m(Globals::NoInit);
    _newConst(&m, scope, data, size);
    return m;
  }

  //! Put a BYTE `val` to a constant-pool (8 bits).
  inline Mem newByteConst(uint32_t scope, uint8_t val) noexcept { return newConst(scope, &val, 1); }
  //! Put a HWORD `val` to a constant-pool (16 bits).
  inline Mem newHWordConst(uint32_t scope, uint16_t val) noexcept { return newConst(scope, &val, 2); }
  //! Put a WORD `val` to a constant-pool (32 bits).
  inline Mem newWordConst(uint32_t scope, uint32_t val) noexcept { return newConst(scope, &val, 4); }
  //! Put a DWORD `val` to a constant-pool (64 bits).
  inline Mem newDWordConst(uint32_t scope, uint64_t val) noexcept { return newConst(scope, &val, 8); }

  //! Put a WORD `val` to a constant-pool.
  inline Mem newInt16Const(uint32_t scope, int16_t val) noexcept { return newConst(scope, &val, 2); }
  //! Put a WORD `val` to a constant-pool.
  inline Mem newUInt16Const(uint32_t scope, uint16_t val) noexcept { return newConst(scope, &val, 2); }
  //! Put a DWORD `val` to a constant-pool.
  inline Mem newInt32Const(uint32_t scope, int32_t val) noexcept { return newConst(scope, &val, 4); }
  //! Put a DWORD `val` to a constant-pool.
  inline Mem newUInt32Const(uint32_t scope, uint32_t val) noexcept { return newConst(scope, &val, 4); }
  //! Put a QWORD `val` to a constant-pool.
  inline Mem newInt64Const(uint32_t scope, int64_t val) noexcept { return newConst(scope, &val, 8); }
  //! Put a QWORD `val` to a constant-pool.
  inline Mem newUInt64Const(uint32_t scope, uint64_t val) noexcept { return newConst(scope, &val, 8); }

  //! Put a SP-FP `val` to a constant-pool.
  inline Mem newFloatConst(uint32_t scope, float val) noexcept { return newConst(scope, &val, 4); }
  //! Put a DP-FP `val` to a constant-pool.
  inline Mem newDoubleConst(uint32_t scope, double val) noexcept { return newConst(scope, &val, 8); }

  //! \}

  //! \name Instruction Options
  //! \{

  //! Force the compiler to not follow the conditional or unconditional jump.
  inline Compiler& unfollow() noexcept { _instOptions |= Inst::kOptionUnfollow; return *this; }

  //! \}

  //! \name Function Call & Ret Intrinsics
  //! \{

  //! Invoke a function call without `target` type enforcement.
  inline Error invoke_(InvokeNode** out, const Operand_& target, const FuncSignature& signature) {
    return _addInvokeNode(out, Inst::kIdBlr, target, signature);
  }

  //! Invoke a function call of the given `target` and `signature` and store
  //! the added node to `out`.
  //!
  //! Creates a new \ref InvokeNode, initializes all the necessary members to
  //! match the given function `signature`, adds the node to the compiler, and
  //! stores its pointer to `out`. The operation is atomic, if anything fails
  //! nullptr is stored in `out` and error code is returned.
  inline Error invoke(InvokeNode** out, const Gp& target, const FuncSignature& signature) { return invoke_(out, target, signature); }
  //! \overload
  inline Error invoke(InvokeNode** out, const Mem& target, const FuncSignature& signature) { return invoke_(out, target, signature); }
  //! \overload
  inline Error invoke(InvokeNode** out, const Label& target, const FuncSignature& signature) { return invoke_(out, target, signature); }
  //! \overload
  inline Error invoke(InvokeNode** out, const Imm& target, const FuncSignature& signature) { return invoke_(out, target, signature); }
  //! \overload
  inline Error invoke(InvokeNode** out, uint64_t target, const FuncSignature& signature) { return invoke_(out, Imm(int64_t(target)), signature); }

  //! Return.
  inline FuncRetNode* ret() { return addRet(Operand(), Operand()); }
  //! \overload
  inline FuncRetNode* ret(const BaseReg& o0) { return addRet(o0, Operand()); }
  //! \overload
  inline FuncRetNode* ret(const BaseReg& o0, const BaseReg& o1) { return addRet(o0, o1); }

  //! \}

  //! \name Jump Tables Support
  //! \{

  using EmitterExplicitT<Compiler>::br;

  //! Adds a jump to the given `target` with the provided jump `annotation`.
  inline Error br(const BaseReg& target, JumpAnnotation* annotation) { return emitAnnotatedJump(Inst::kIdBr, target, annotation); }

  //! \}

  //! \name Finalize
  //! \{

  ASMJIT_API Error finalize() override;

  //! \}

  //! \name Events
  //! \{

  ASMJIT_API Error onAttach(CodeHolder* code) noexcept override;

  //! \}
};

//! \}

ASMJIT_END_SUB_NAMESPACE

#endif // !ASMJIT_NO_COMPILER
#endif // ASMJIT_ARM_ARMCOMPILER_H_INCLUDED
