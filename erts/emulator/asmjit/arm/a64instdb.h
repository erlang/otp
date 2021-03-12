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

#ifndef ASMJIT_ARM_A64INSTDB_H_INCLUDED
#define ASMJIT_ARM_A64INSTDB_H_INCLUDED

#include "../arm/a64globals.h"

ASMJIT_BEGIN_SUB_NAMESPACE(a64)

//! \addtogroup asmjit_a64
//! \{

//! Instruction database (ARM/THUMB/AArch64).
namespace InstDB {

// ============================================================================
// [asmjit::a64::InstDB::InstFlags]
// ============================================================================

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
  kInstFlagVH0_15 = 0x00000010u
};

// ============================================================================
// [asmjit::a64::InstDB::InstInfo]
// ============================================================================

//! Instruction information (ARM/THUMB/AArch64).
struct InstInfo {
  //! Instruction encoding type.
  uint32_t _encoding : 8;
  //! Index to data specific to each encoding type.
  uint32_t _encodingDataIndex : 8;
  uint32_t _reserved : 2;
  //! Index to \ref _nameData.
  uint32_t _nameDataIndex : 14;

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

static inline const InstInfo& infoById(uint32_t instId) noexcept {
  ASMJIT_ASSERT(Inst::isDefinedId(instId));
  return _instInfoTable[instId];
}

} // {InstDB}

//! \}

ASMJIT_END_SUB_NAMESPACE

#endif // ASMJIT_ARM_A64INSTDB_H_INCLUDED
