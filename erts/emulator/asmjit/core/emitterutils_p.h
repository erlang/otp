// This file is part of AsmJit project <https://asmjit.com>
//
// See asmjit.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#ifndef ASMJIT_CORE_EMITTERUTILS_P_H_INCLUDED
#define ASMJIT_CORE_EMITTERUTILS_P_H_INCLUDED

#include "../core/emitter.h"
#include "../core/operand.h"

ASMJIT_BEGIN_NAMESPACE

class BaseAssembler;
class FormatOptions;

//! \cond INTERNAL
//! \addtogroup asmjit_core
//! \{

//! Utilities used by various emitters, mostly Assembler implementations.
namespace EmitterUtils {

//! Default paddings used by Emitter utils and Formatter.

static constexpr Operand noExt[3];

enum kOpIndex : uint32_t {
  kOp3 = 0,
  kOp4 = 1,
  kOp5 = 2
};

static ASMJIT_FORCE_INLINE uint32_t opCountFromEmitArgs(const Operand_& o0, const Operand_& o1, const Operand_& o2, const Operand_* opExt) noexcept {
  uint32_t opCount = 0;

  if (opExt[kOp3].isNone()) {
    if (!o0.isNone()) opCount = 1;
    if (!o1.isNone()) opCount = 2;
    if (!o2.isNone()) opCount = 3;
  }
  else {
    opCount = 4;
    if (!opExt[kOp4].isNone()) {
      opCount = 5 + uint32_t(!opExt[kOp5].isNone());
    }
  }

  return opCount;
}

static ASMJIT_FORCE_INLINE void opArrayFromEmitArgs(Operand_ dst[Globals::kMaxOpCount], const Operand_& o0, const Operand_& o1, const Operand_& o2, const Operand_* opExt) noexcept {
  dst[0].copyFrom(o0);
  dst[1].copyFrom(o1);
  dst[2].copyFrom(o2);
  dst[3].copyFrom(opExt[kOp3]);
  dst[4].copyFrom(opExt[kOp4]);
  dst[5].copyFrom(opExt[kOp5]);
}

#ifndef ASMJIT_NO_LOGGING
Error finishFormattedLine(String& sb, const FormatOptions& formatOptions, const uint8_t* binData, size_t binSize, size_t offsetSize, size_t immSize, const char* comment) noexcept;

void logLabelBound(BaseAssembler* self, const Label& label) noexcept;

void logInstructionEmitted(
  BaseAssembler* self,
  InstId instId,
  InstOptions options,
  const Operand_& o0, const Operand_& o1, const Operand_& o2, const Operand_* opExt,
  uint32_t relSize, uint32_t immSize, uint8_t* afterCursor);

Error logInstructionFailed(
  BaseAssembler* self,
  Error err,
  InstId instId,
  InstOptions options,
  const Operand_& o0, const Operand_& o1, const Operand_& o2, const Operand_* opExt);
#endif

}

//! \}
//! \endcond

ASMJIT_END_NAMESPACE

#endif // ASMJIT_CORE_EMITTERUTILS_P_H_INCLUDED

