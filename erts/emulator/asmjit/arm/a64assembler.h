// This file is part of AsmJit project <https://asmjit.com>
//
// See asmjit.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#ifndef ASMJIT_ARM_A64ASSEMBLER_H_INCLUDED
#define ASMJIT_ARM_A64ASSEMBLER_H_INCLUDED

#include "../core/assembler.h"
#include "../arm/a64emitter.h"
#include "../arm/a64operand.h"

ASMJIT_BEGIN_SUB_NAMESPACE(a64)

//! \addtogroup asmjit_a64
//! \{

//! AArch64 assembler implementation.
class ASMJIT_VIRTAPI Assembler
  : public BaseAssembler,
    public EmitterExplicitT<Assembler> {

public:
  typedef BaseAssembler Base;

  //! \name Construction / Destruction
  //! \{

  ASMJIT_API Assembler(CodeHolder* code = nullptr) noexcept;
  ASMJIT_API virtual ~Assembler() noexcept;

  //! \}

  //! \name Accessors
  //! \{

  //! Gets whether the current ARM mode is THUMB (alternative to 32-bit ARM encoding).
  inline bool isInThumbMode() const noexcept { return _environment.isArchThumb(); }

  //! Gets the current code alignment of the current mode (ARM vs THUMB).
  inline uint32_t codeAlignment() const noexcept { return isInThumbMode() ? 2 : 4; }

  //! \}

  //! \name Emit
  //! \{

  ASMJIT_API Error _emit(InstId instId, const Operand_& o0, const Operand_& o1, const Operand_& o2, const Operand_* opExt) override;

  //! \}

  //! \name Align
  //! \{

  ASMJIT_API Error align(AlignMode alignMode, uint32_t alignment) override;

  //! \}

  //! \name Events
  //! \{

  ASMJIT_API Error onAttach(CodeHolder* code) noexcept override;
  ASMJIT_API Error onDetach(CodeHolder* code) noexcept override;

  //! \}
};

//! \}

ASMJIT_END_SUB_NAMESPACE

#endif // ASMJIT_ARM_A64ASSEMBLER_H_INCLUDED
