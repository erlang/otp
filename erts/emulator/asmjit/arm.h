// This file is part of AsmJit project <https://asmjit.com>
//
// See asmjit.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#ifndef ASMJIT_ARM_H_INCLUDED
#define ASMJIT_ARM_H_INCLUDED

//! \addtogroup asmjit_arm
//!
//! ### Namespaces
//!
//!   - \ref arm - arm namespace provides common functionality for both AArch32 and AArch64 backends.
//!   - \ref a32 - a32 namespace provides support for AArch32 architecture. In addition it includes
//!     \ref arm namespace, so you can only use a single namespace when targeting AArch32 architecture.
//!   - \ref a64 - a64 namespace provides support for AArch64 architecture. In addition it includes
//!     \ref arm namespace, so you can only use a single namespace when targeting AArch64 architecture.
//!
//! ### Emitters
//!
//!   - AArch32
//!     - \ref a32::Assembler - AArch32 assembler (must read, provides examples).
//!     - \ref a32::Builder - AArch32 builder.
//!     - \ref a32::Compiler - AArch32 compiler.
//!     - \ref a32::Emitter - AArch32 emitter (abstract).
//!
//!   - AArch64
//!     - \ref a64::Assembler - AArch64 assembler (must read, provides examples).
//!     - \ref a64::Builder - AArch64 builder.
//!     - \ref a64::Compiler - AArch64 compiler.
//!     - \ref a64::Emitter - AArch64 emitter (abstract).
//!
//! ### Supported Instructions
//!
//!   - AArch32:
//!     - Emitters:
//!       - \ref a32::EmitterExplicitT - Provides all instructions that use explicit operands, provides also
//!         utility functions. The member functions provided are part of all AArch32 emitters.
//!     - Instruction representation:
//!       - \ref a32::Inst::Id - instruction identifiers.
//!
//!   - AArch64:
//!     - Emitters:
//!       - \ref a64::EmitterExplicitT - Provides all instructions that use explicit operands, provides also
//!         utility functions. The member functions provided are part of all AArch64 emitters.
//!     - Instruction representation:
//!       - \ref a64::Inst::Id - instruction identifiers.
//!
//! ### Register Operands
//!
//!   - \ref arm::Reg - Base class of all AArch32/AArch64 registers.
//!     - \ref a32::Gp - 32-bit general purpose register used by AArch32:
//!     - \ref a64::Gp - 32-bit or 64-bit general purpose register used by AArch64:
//!       - \ref a64::GpW - 32-bit register (AArch64).
//!       - \ref a64::GpX - 64-bit register (AArch64).
//!     - \ref arm::BaseVec - Base vector (SIMD) register.
//!       - \ref a32::Vec - Vector (SIMD) register (AArch32):
//!         - \ref a32::VecS - 32-bit SIMD register (AArch32).
//!         - \ref a32::VecD - 64-bit SIMD register (AArch32).
//!         - \ref a32::VecV - 128-bit SIMD register (AArch32).
//!       - \ref a64::Vec - Vector (SIMD) register (AArch64):
//!         - \ref a64::VecB - 8-bit SIMD register (AArch64).
//!         - \ref a64::VecH - 16-bit SIMD register (AArch64).
//!         - \ref a64::VecS - 32-bit SIMD register (AArch64).
//!         - \ref a64::VecD - 64-bit SIMD register (AArch64).
//!         - \ref a64::VecV - 128-bit SIMD register (AArch64).
//!
//! ### Memory Operands
//!
//!   - \ref arm::Mem - AArch32/AArch64 memory operand that provides support for all ARM addressing features
//!     including base, index, pre/post increment, and ARM-specific shift addressing and index extending.
//!
//! ### Other
//!
//!   - \ref arm::Shift - Shift operation and value (both AArch32 and AArch64).
//!   - \ref arm::DataType - Data type that is part of an instruction in AArch32 mode.
//!   - \ref arm::Utils - Utilities that can help during code generation for AArch32 and AArch64.

#include "./core.h"
#include "./arm/armglobals.h"
#include "./arm/armoperand.h"
#include "./arm/armutils.h"

#endif // ASMJIT_ARM_H_INCLUDED
