// This file is part of AsmJit project <https://asmjit.com>
//
// See asmjit.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#ifndef ASMJIT_X86_X86RAPASS_P_H_INCLUDED
#define ASMJIT_X86_X86RAPASS_P_H_INCLUDED

#include "../core/api-config.h"
#ifndef ASMJIT_NO_COMPILER

#include "../core/compiler.h"
#include "../core/rabuilders_p.h"
#include "../core/rapass_p.h"
#include "../x86/x86assembler.h"
#include "../x86/x86compiler.h"
#include "../x86/x86emithelper_p.h"

ASMJIT_BEGIN_SUB_NAMESPACE(x86)

//! \cond INTERNAL
//! \addtogroup asmjit_x86
//! \{

//! X86 register allocation pass.
//!
//! Takes care of generating function prologs and epilogs, and also performs register allocation.
class X86RAPass : public BaseRAPass {
public:
  ASMJIT_NONCOPYABLE(X86RAPass)
  typedef BaseRAPass Base;

  EmitHelper _emitHelper;

  //! \name Construction & Destruction
  //! \{

  X86RAPass() noexcept;
  ~X86RAPass() noexcept override;

  //! \}

  //! \name Accessors
  //! \{

  //! Returns the compiler casted to `x86::Compiler`.
  ASMJIT_INLINE_NODEBUG Compiler* cc() const noexcept { return static_cast<Compiler*>(_cb); }

  //! Returns emit helper.
  ASMJIT_INLINE_NODEBUG EmitHelper* emitHelper() noexcept { return &_emitHelper; }

  ASMJIT_INLINE_NODEBUG bool avxEnabled() const noexcept { return _emitHelper._avxEnabled; }
  ASMJIT_INLINE_NODEBUG bool avx512Enabled() const noexcept { return _emitHelper._avx512Enabled; }

  //! \}

  //! \name Utilities
  //! \{

  ASMJIT_INLINE_NODEBUG InstId choose(InstId sseInstId, InstId avxInstId) noexcept {
    return avxEnabled() ? avxInstId : sseInstId;
  }

  //! \}

  //! \name Interface
  //! \{

  void onInit() noexcept override;
  void onDone() noexcept override;

  Error buildCFG() noexcept override;

  Error _rewrite(BaseNode* first, BaseNode* stop) noexcept override;

  Error emitMove(uint32_t workId, uint32_t dstPhysId, uint32_t srcPhysId) noexcept override;
  Error emitSwap(uint32_t aWorkId, uint32_t aPhysId, uint32_t bWorkId, uint32_t bPhysId) noexcept override;

  Error emitLoad(uint32_t workId, uint32_t dstPhysId) noexcept override;
  Error emitSave(uint32_t workId, uint32_t srcPhysId) noexcept override;

  Error emitJump(const Label& label) noexcept override;
  Error emitPreCall(InvokeNode* invokeNode) noexcept override;

  //! \}
};

//! \}
//! \endcond

ASMJIT_END_SUB_NAMESPACE

#endif // !ASMJIT_NO_COMPILER
#endif // ASMJIT_X86_X86RAPASS_P_H_INCLUDED
