// This file is part of AsmJit project <https://asmjit.com>
//
// See asmjit.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#ifndef ASMJIT_ARM_A64RAPASS_P_H_INCLUDED
#define ASMJIT_ARM_A64RAPASS_P_H_INCLUDED

#include "../core/api-config.h"
#ifndef ASMJIT_NO_COMPILER

#include "../core/compiler.h"
#include "../core/rabuilders_p.h"
#include "../core/rapass_p.h"
#include "../arm/a64assembler.h"
#include "../arm/a64compiler.h"
#include "../arm/a64emithelper_p.h"

ASMJIT_BEGIN_SUB_NAMESPACE(a64)

//! \cond INTERNAL
//! \addtogroup asmjit_a64
//! \{

//! ARM register allocation pass.
//!
//! Takes care of generating function prologs and epilogs, and also performs
//! register allocation.
class ARMRAPass : public BaseRAPass {
public:
  ASMJIT_NONCOPYABLE(ARMRAPass)
  typedef BaseRAPass Base;

  EmitHelper _emitHelper;

  //! \name Construction & Destruction
  //! \{

  ARMRAPass() noexcept;
  virtual ~ARMRAPass() noexcept;

  //! \}

  //! \name Accessors
  //! \{

  //! Returns the compiler casted to `arm::Compiler`.
  inline Compiler* cc() const noexcept { return static_cast<Compiler*>(_cb); }

  //! Returns emit helper.
  inline EmitHelper* emitHelper() noexcept { return &_emitHelper; }

  //! \}

  //! \name Events
  //! \{

  void onInit() noexcept override;
  void onDone() noexcept override;

  //! \}

  //! \name CFG
  //! \{

  Error buildCFG() noexcept override;

  //! \}

  //! \name Rewrite
  //! \{

  Error _rewrite(BaseNode* first, BaseNode* stop) noexcept override;

  //! \}

  //! \name Prolog & Epilog
  //! \{

  Error updateStackFrame() noexcept override;

  //! \}

  //! \name Emit Helpers
  //! \{

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
#endif // ASMJIT_ARM_A64RAPASS_P_H_INCLUDED
