// This file is part of AsmJit project <https://asmjit.com>
//
// See asmjit.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#ifndef ASMJIT_A64_H_INCLUDED
#define ASMJIT_A64_H_INCLUDED

//! \addtogroup asmjit_a64
//!
//! ### Emitters
//!
//!   - \ref a64::Assembler - AArch64 assembler (must read, provides examples).
//!   - \ref a64::Builder - AArch64 builder.
//!   - \ref a64::Compiler - AArch64 compiler.
//!   - \ref a64::Emitter - AArch64 emitter (abstract).
//!
//! ### Supported Instructions
//!
//!   - Emitters:
//!     - \ref a64::EmitterExplicitT - Provides all instructions that use explicit
//!       operands, provides also utility functions. The member functions provided
//!       are part of all ARM/AArch64 emitters.
//!
//!   - Instruction representation:
//!     - \ref a64::Inst::Id - instruction identifiers.
//!
//! ### Register Operands
//!
//!   - \ref arm::Reg - Base class for any AArch32/AArch64 register.
//!     - \ref arm::Gp - General purpose register:
//!       - \ref arm::GpW - 32-bit register.
//!       - \ref arm::GpX - 64-bit register.
//!     - \ref arm::Vec - Vector (SIMD) register:
//!       - \ref arm::VecB - 8-bit SIMD register (AArch64 only).
//!       - \ref arm::VecH - 16-bit SIMD register (AArch64 only).
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
//!   - \ref a64::Utils - Utilities that can help during code generation for AArch64.

#include "./arm.h"
#include "./arm/a64assembler.h"
#include "./arm/a64builder.h"
#include "./arm/a64compiler.h"
#include "./arm/a64emitter.h"
#include "./arm/a64globals.h"
#include "./arm/a64instdb.h"
#include "./arm/a64operand.h"
#include "./arm/a64utils.h"

#endif // ASMJIT_A64_H_INCLUDED

