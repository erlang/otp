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

#ifndef ASMJIT_ARM_A64ASSEMBLER_H_INCLUDED
#define ASMJIT_ARM_A64ASSEMBLER_H_INCLUDED

#include "../core/assembler.h"
#include "../arm/a64emitter.h"
#include "../arm/a64operand.h"

ASMJIT_BEGIN_SUB_NAMESPACE(a64)

//! \addtogroup asmjit_a64
//! \{

// ============================================================================
// [asmjit::a64::Assembler]
// ============================================================================

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

  ASMJIT_API Error _emit(uint32_t instId, const Operand_& o0, const Operand_& o1, const Operand_& o2, const Operand_* opExt) override;

  //! \}

  //! \name Align
  //! \{

  ASMJIT_API Error align(uint32_t alignMode, uint32_t alignment) override;

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
