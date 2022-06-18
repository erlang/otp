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

// ============================================================================
// [asmjit::ARMRAPass]
// ============================================================================

//! ARM register allocation pass.
//!
//! Takes care of generating function prologs and epilogs, and also performs
//! register allocation.
class ARMRAPass : public BaseRAPass {
public:
  ASMJIT_NONCOPYABLE(ARMRAPass)
  typedef BaseRAPass Base;

  EmitHelper _emitHelper;

  // --------------------------------------------------------------------------
  // [Construction / Destruction]
  // --------------------------------------------------------------------------

  ARMRAPass() noexcept;
  virtual ~ARMRAPass() noexcept;

  // --------------------------------------------------------------------------
  // [Accessors]
  // --------------------------------------------------------------------------

  //! Returns the compiler casted to `arm::Compiler`.
  inline Compiler* cc() const noexcept { return static_cast<Compiler*>(_cb); }

  //! Returns emit helper.
  inline EmitHelper* emitHelper() noexcept { return &_emitHelper; }

  // --------------------------------------------------------------------------
  // [OnInit / OnDone]
  // --------------------------------------------------------------------------

  void onInit() noexcept override;
  void onDone() noexcept override;

  // --------------------------------------------------------------------------
  // [CFG]
  // --------------------------------------------------------------------------

  Error buildCFG() noexcept override;

  // --------------------------------------------------------------------------
  // [Rewrite]
  // --------------------------------------------------------------------------

  Error _rewrite(BaseNode* first, BaseNode* stop) noexcept override;

  // --------------------------------------------------------------------------
  // [Prolog / Epilog]
  // --------------------------------------------------------------------------

  Error updateStackFrame() noexcept override;

  // --------------------------------------------------------------------------
  // [Emit]
  // --------------------------------------------------------------------------

  Error emitMove(uint32_t workId, uint32_t dstPhysId, uint32_t srcPhysId) noexcept override;
  Error emitSwap(uint32_t aWorkId, uint32_t aPhysId, uint32_t bWorkId, uint32_t bPhysId) noexcept override;

  Error emitLoad(uint32_t workId, uint32_t dstPhysId) noexcept override;
  Error emitSave(uint32_t workId, uint32_t srcPhysId) noexcept override;

  Error emitJump(const Label& label) noexcept override;
  Error emitPreCall(InvokeNode* invokeNode) noexcept override;
};

//! \}
//! \endcond

ASMJIT_END_SUB_NAMESPACE

#endif // !ASMJIT_NO_COMPILER
#endif // ASMJIT_ARM_A64RAPASS_P_H_INCLUDED
