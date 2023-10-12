// This file is part of AsmJit project <https://asmjit.com>
//
// See asmjit.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#ifndef ASMJIT_X86_H_INCLUDED
#define ASMJIT_X86_H_INCLUDED

//! \addtogroup asmjit_x86
//!
//! ### Namespace
//!
//!   - \ref x86 - x86 namespace provides support for X86/X64 code generation.
//!
//! ### Emitters
//!
//!   - \ref x86::Assembler - X86/X64 assembler (must read, provides examples).
//!   - \ref x86::Builder - X86/X64 builder.
//!   - \ref x86::Compiler - X86/X64 compiler.
//!   - \ref x86::Emitter - X86/X64 emitter (abstract).
//!
//! ### Supported Instructions
//!
//!   - Emitters:
//!     - \ref x86::EmitterExplicitT - Provides all instructions that use explicit operands, provides also utility
//!       functions. The member functions provided are part of all X86 emitters.
//!     - \ref x86::EmitterImplicitT - Provides all instructions that use implicit operands, these cannot be used
//!       with \ref x86::Compiler.
//!
//!   - Instruction representation:
//!     - \ref x86::Inst::Id - Provides instruction identifiers for both X86/X86_64 architectures.
//!     - \ref InstOptions - Provides generic and X86/X86_64 specific options.
//!
//! ### Register Operands
//!
//!   - \ref x86::Reg - Base class for any X86 register.
//!     - \ref x86::Gp - General purpose register:
//!       - \ref x86::GpbLo - 8-bit low register.
//!       - \ref x86::GpbHi - 8-bit high register.
//!       - \ref x86::Gpw - 16-bit register.
//!       - \ref x86::Gpd - 32-bit register.
//!       - \ref x86::Gpq - 64-bit register (X64 only).
//!     - \ref x86::Vec - Vector (SIMD) register:
//!       - \ref x86::Xmm - 128-bit SIMD register (SSE+).
//!       - \ref x86::Ymm - 256-bit SIMD register (AVX+).
//!       - \ref x86::Zmm - 512-bit SIMD register (AVX512+).
//!     - \ref x86::Mm - 64-bit MMX register.
//!     - \ref x86::St - 80-bit FPU register.
//!     - \ref x86::KReg - opmask registers (AVX512+).
//!     - \ref x86::SReg - segment register.
//!     - \ref x86::CReg - control register.
//!     - \ref x86::DReg - debug register.
//!     - \ref x86::Bnd - bound register (discontinued).
//!     - \ref x86::Rip - relative instruction pointer.
//!
//! ### Memory Operands
//!
//!   - \ref x86::Mem - X86/X64 memory operand that provides support for all X86 and X64 addressing features
//!     including absolute addresses, index scales, and segment override prefixes.
//!
//! ### Status and Control Words
//!
//!   - \ref x86::FpuStatusWord - FPU status word bits / decomposition.
//!   - \ref x86::FpuControlWord - FPU control word bits / decomposition.
//!
//! ### Predicates (immediate values)
//!
//!   - \ref x86::CmpImm - `CMP[PD|PS|SD|SS]` predicate (SSE+).
//!   - \ref x86::PCmpStrImm - `[V]PCMP[I|E]STR[I|M]` predicate (SSE4.1+, AVX+).
//!   - \ref x86::RoundImm - `[V]ROUND[PD|PS|SD|SS]` predicate (SSE+, AVX+).
//!   - \ref x86::VCmpImm - `VCMP[PD|PS|SD|SS]` predicate (AVX+).
//!   - \ref x86::VFixupImm - `VFIXUPIMM[PD|PS|SD|SS]` predicate (AVX512+).
//!   - \ref x86::VFPClassImm - `VFPCLASS[PD|PS|SD|SS]` predicate (AVX512+).
//!   - \ref x86::VGetMantImm - `VGETMANT[PD|PS|SD|SS]` predicate (AVX512+).
//!   - \ref x86::VPCmpImm - `VPCMP[U][B|W|D|Q]` predicate (AVX512+).
//!   - \ref x86::VPComImm - `VPCOM[U][B|W|D|Q]` predicate (XOP).
//!   - \ref x86::VRangeImm - `VRANGE[PD|PS|SD|SS]` predicate (AVX512+).
//!   - \ref x86::VReduceImm - `REDUCE[PD|PS|SD|SS]` predicate (AVX512+).
//!   - \ref x86::TLogImm - `VPTERNLOG[D|Q]` predicate and operations (AVX512+).

#include "core.h"

#include "asmjit-scope-begin.h"
#include "x86/x86assembler.h"
#include "x86/x86builder.h"
#include "x86/x86compiler.h"
#include "x86/x86emitter.h"
#include "x86/x86globals.h"
#include "x86/x86instdb.h"
#include "x86/x86operand.h"
#include "asmjit-scope-end.h"

#endif // ASMJIT_X86_H_INCLUDED
