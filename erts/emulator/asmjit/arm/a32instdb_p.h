// This file is part of AsmJit project <https://asmjit.com>
//
// See asmjit.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#ifndef ASMJIT_ARM_A32INSTDB_H_P_INCLUDED
#define ASMJIT_ARM_A32INSTDB_H_P_INCLUDED

#include "../core/codeholder.h"
#include "../core/instdb_p.h"
// #include "../arm/a32instdb.h"
#include "../arm/a32operand.h"

ASMJIT_BEGIN_SUB_NAMESPACE(a32)

//! \cond INTERNAL
//! \addtogroup asmjit_a32
//! \{

namespace InstDB {

// a32::InstDB - Tables
// ====================

#ifndef ASMJIT_NO_TEXT
extern const InstNameIndex instNameIndex;
extern const char _instNameStringTable[];
extern const uint32_t _instNameIndexTable[];
#endif // !ASMJIT_NO_TEXT

} // {InstDB}

//! \}
//! \endcond

ASMJIT_END_SUB_NAMESPACE

#endif // ASMJIT_ARM_A32INSTDB_H_P_INCLUDED

