// This file is part of AsmJit project <https://asmjit.com>
//
// See asmjit.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#ifndef ASMJIT_A32_H_INCLUDED
#define ASMJIT_A32_H_INCLUDED

//! \addtogroup asmjit_a32
//!
//! ### Emitters
//!
//!   - \ref a32::Assembler - AArch32 assembler (must read, provides examples).
//!   - \ref a32::Builder - AArch32 builder.
//!   - \ref a32::Compiler - AArch32 compiler.
//!   - \ref a32::Emitter - AArch32 emitter (abstract).
//!
//! ### Supported Instructions
//!
//!   - Emitters:
//!     - \ref a32::EmitterExplicitT - Provides all instructions that use explicit operands, provides also utility
//!       functions. The member functions provided are part of all AArch32 emitters.
//!
//!   - Instruction representation:
//!     - \ref a32::Inst::Id - instruction identifiers.
//!
//! ### Register Operands
//!
//!   - \ref arm::Reg - Base class of any AArch32/AArch64 register.
//!     - \ref a32::Gp - 32-bit general purpose register (AArch32)
//!     - \ref arm::Vec - Vector (SIMD) register:
//!       - \ref arm::VecS - 32-bit SIMD register.
//!       - \ref arm::VecD - 64-bit SIMD register.
//!       - \ref arm::VecV - 128-bit SIMD register.
//!
//! ### Memory Operands
//!
//!   - \ref arm::Mem - AArch32/AArch64 memory operand that provides support for all ARM addressing features
//!     including base, index, pre/post increment, and ARM-specific shift addressing and index extending.
//!
//! ### Other
//!
//!   - \ref arm::Shift - Shift operation and value.
//!   - \ref arm::DataType - Data type that is part of an instruction in AArch32 mode.
//!   - \ref a32::Utils - Utilities that can help during code generation for AArch32.

#include "./arm.h"
#include "./arm/a32assembler.h"
#include "./arm/a32builder.h"
#include "./arm/a32emitter.h"
#include "./arm/a32globals.h"
#include "./arm/a32operand.h"

#endif // ASMJIT_A32_H_INCLUDED

