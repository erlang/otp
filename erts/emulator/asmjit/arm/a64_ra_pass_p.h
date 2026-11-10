// This file is part of AsmJit project <https://asmjit.com>
//
// See <asmjit/core.h> or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#ifndef ASMJIT_ARM_A64_RA_PASS_P_H_INCLUDED
#define ASMJIT_ARM_A64_RA_PASS_P_H_INCLUDED

#include <asmjit/core/build_defs.h>
#ifndef ASMJIT_NO_COMPILER

#include <asmjit/core/compiler.h>
#include <asmjit/core/ra_cfg_block_p.h>
#include <asmjit/core/ra_cfg_builder_p.h>
#include <asmjit/core/ra_pass_p.h>
#include <asmjit/arm/a64_assembler.h>
#include <asmjit/arm/a64_compiler.h>
#include <asmjit/arm/a64_emit_helper_p.h>

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
  using Base = BaseRAPass;

  //! \name Members
  //! \{

  EmitHelper _emit_helper;

  //! \}

  //! \name Construction & Destruction
  //! \{

  ARMRAPass(BaseCompiler& cc) noexcept;
  ~ARMRAPass() noexcept override;

  //! \}

  //! \name Accessors
  //! \{

  //! Returns the compiler casted to `arm::Compiler`.
  [[nodiscard]] ASMJIT_INLINE_NODEBUG Compiler& cc() const noexcept { return static_cast<Compiler&>(_cb); }

  //! Returns emit helper.
  [[nodiscard]] ASMJIT_INLINE_NODEBUG EmitHelper* emit_helper() noexcept { return &_emit_helper; }

  //! \}

  //! \name Interface
  //! \{

  void on_init() noexcept override;
  void on_done() noexcept override;

  Error build_cfg_nodes() override;
  Error rewrite() override;
  Error update_stack_frame() noexcept override;

  //! \}

  //! \name Emit Helpers
  //! \{

  Error emit_move(RAWorkReg* work_reg, uint32_t dst_phys_id, uint32_t src_phys_id) override;
  Error emit_swap(RAWorkReg* a_reg, uint32_t a_phys_id, RAWorkReg* b_reg, uint32_t b_phys_id) override;

  Error emit_load(RAWorkReg* work_reg, uint32_t dst_phys_id) override;
  Error emit_save(RAWorkReg* work_reg, uint32_t src_phys_id) override;

  Error emit_jump(const Label& label) override;
  Error emit_pre_call(InvokeNode* invoke_node) override;

  //! \}
};

//! \}
//! \endcond

ASMJIT_END_SUB_NAMESPACE

#endif // !ASMJIT_NO_COMPILER
#endif // ASMJIT_ARM_A64_RA_PASS_P_H_INCLUDED
