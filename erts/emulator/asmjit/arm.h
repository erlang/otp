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

#ifndef ASMJIT_ARM_H_INCLUDED
#define ASMJIT_ARM_H_INCLUDED

//! \defgroup asmjit_arm ARM (generic)
//! \brief ARM & AArch64 generic support.

//! \addtogroup asmjit_arm
//!
//! ### Namespace
//!
//!   - \ref arm - arm namespace provides base support for both 32-bit and 64-bit ARM.
//!   - \ref a64 - a64 namespace provides support for AArch64 architecture. In addition
//!     it includes \ref arm namespace, so you can only use a single namespace when
//!     targeting AArch64 architecture.
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
//!     - \ref a64::Inst::Options - instruction options.
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
//!   - \ref arm::Features - ARM/AArch64 CPU features on top of \ref BaseFeatures.
//!   - \ref arm::Shift - Shift operation and value.
//!   - \ref a64::Utils - Utilities that can help during code generation for
//!     ARM target.
//!
//! ### Predicates
//!
//!   - \ref a64::Predicate - namespace that provides AArch64 predicates.

#include "./core.h"
#include "./arm/armfeatures.h"
#include "./arm/armglobals.h"
#include "./arm/armoperand.h"

#endif // ASMJIT_ARM_H_INCLUDED
