// This file is part of AsmJit project <https://asmjit.com>
//
// See asmjit.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#ifndef ASMJIT_ARM_A64INSTDB_H_INCLUDED
#define ASMJIT_ARM_A64INSTDB_H_INCLUDED

#include "../arm/a64globals.h"

ASMJIT_BEGIN_SUB_NAMESPACE(a64)

//! \addtogroup asmjit_a64
//! \{

//! Instruction database (AArch64).
namespace InstDB {

//! Instruction flags.
enum InstFlags : uint32_t {
  //! The instruction provides conditional execution.
  kInstFlagCond = 0x00000001u,
  //! SIMD instruction that processes elements in pairs.
  kInstFlagPair = 0x00000002u,
  //! SIMD instruction that does widening (Long).
  kInstFlagLong = 0x00000004u,
  //! SIMD instruction that does narrowing (Narrow).
  kInstFlagNarrow = 0x00000008u,
  //! SIMD element access of half-words can only be used with v0..15.
  kInstFlagVH0_15 = 0x00000010u,

  //! Instruction may consecutive registers if the number of operands is greater than 2.
  kInstFlagConsecutive = 0x00000080u
};

//! Instruction information (AArch64).
struct InstInfo {
  //! Instruction encoding type.
  uint32_t _encoding : 8;
  //! Index to data specific to each encoding type.
  uint32_t _encodingDataIndex : 8;
  uint32_t _reserved : 16;

  uint16_t _rwInfoIndex;
  uint16_t _flags;

  //! \name Accessors
  //! \{

  inline uint32_t rwInfoIndex() const noexcept { return _rwInfoIndex; }
  inline uint32_t flags() const noexcept { return _flags; }

  inline bool hasFlag(uint32_t flag) const { return (_flags & flag) != 0; }

  //! \}
};

ASMJIT_VARAPI const InstInfo _instInfoTable[];

static inline const InstInfo& infoById(InstId instId) noexcept {
  instId &= uint32_t(InstIdParts::kRealId);
  ASMJIT_ASSERT(Inst::isDefinedId(instId));
  return _instInfoTable[instId];
}

} // {InstDB}

//! \}

ASMJIT_END_SUB_NAMESPACE

#endif // ASMJIT_ARM_A64INSTDB_H_INCLUDED
