// This file is part of AsmJit project <https://asmjit.com>
//
// See asmjit.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#ifndef ASMJIT_ARM_H_INCLUDED
#define ASMJIT_ARM_H_INCLUDED

//! \addtogroup asmjit_arm
//!
//! ### Namespace
//!
//!   - \ref arm - arm namespace provides base support for both 32-bit and 64-bit ARM.
//!   - \ref a64 - a64 namespace provides support for AArch64 architecture. In addition it includes
//!     \ref arm namespace, so you can only use a single namespace when targeting AArch64 architecture.
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
//!   - \ref arm::Reg - Base class for any X86 register.
//!     - \ref arm::Gp - General purpose register:
//!       - \ref arm::GpW - 32-bit register.
//!       - \ref arm::GpX - 64-bit register.
//!     - \ref arm::Vec - Vector (SIMD) register:
//!       - \ref arm::VecB - 8-bit SIMD register.
//!       - \ref arm::VecH - 16-bit SIMD register.
//!       - \ref arm::VecS - 32-bit SIMD register.
//!       - \ref arm::VecD - 64-bit SIMD register.
//!       - \ref arm::VecV - 128-bit SIMD register.
//!
//! ### Memory Operands
//!
//!   - \ref arm::Mem - ARM/AArch64 memory operand that provides support
//!     for all ARM addressing features including BASE, INDEX, Pre/Post increment,
//!     and ARM-specific shift addressing features.
//!
//! ### Other
//!
//!   - \ref arm::Shift - Shift operation and value.
//!   - \ref a64::Utils - Utilities that can help during code generation for ARM target.
//!
//! ### Predicates
//!
//!   - \ref a64::Predicate - namespace that provides AArch64 predicates.

#include "./core.h"
#include "./arm/armglobals.h"
#include "./arm/armoperand.h"

#endif // ASMJIT_ARM_H_INCLUDED
