// This file is part of AsmJit project <https://asmjit.com>
//
// See asmjit.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#ifndef ASMJIT_ARM_A64OPERAND_H_INCLUDED
#define ASMJIT_ARM_A64OPERAND_H_INCLUDED

#include "../arm/armoperand.h"

ASMJIT_BEGIN_SUB_NAMESPACE(a64)

//! \addtogroup asmjit_a64
//! \{

using arm::Reg;
using arm::Mem;
using arm::Gp;
using arm::GpW;
using arm::GpX;

using arm::Vec;
using arm::VecB;
using arm::VecH;
using arm::VecS;
using arm::VecD;
using arm::VecV;

#ifndef _DOXYGEN
namespace regs {
#endif

using namespace ::asmjit::arm::regs;

static constexpr GpW w0 = GpW(0);
static constexpr GpW w1 = GpW(1);
static constexpr GpW w2 = GpW(2);
static constexpr GpW w3 = GpW(3);
static constexpr GpW w4 = GpW(4);
static constexpr GpW w5 = GpW(5);
static constexpr GpW w6 = GpW(6);
static constexpr GpW w7 = GpW(7);
static constexpr GpW w8 = GpW(8);
static constexpr GpW w9 = GpW(9);
static constexpr GpW w10 = GpW(10);
static constexpr GpW w11 = GpW(11);
static constexpr GpW w12 = GpW(12);
static constexpr GpW w13 = GpW(13);
static constexpr GpW w14 = GpW(14);
static constexpr GpW w15 = GpW(15);
static constexpr GpW w16 = GpW(16);
static constexpr GpW w17 = GpW(17);
static constexpr GpW w18 = GpW(18);
static constexpr GpW w19 = GpW(19);
static constexpr GpW w20 = GpW(20);
static constexpr GpW w21 = GpW(21);
static constexpr GpW w22 = GpW(22);
static constexpr GpW w23 = GpW(23);
static constexpr GpW w24 = GpW(24);
static constexpr GpW w25 = GpW(25);
static constexpr GpW w26 = GpW(26);
static constexpr GpW w27 = GpW(27);
static constexpr GpW w28 = GpW(28);
static constexpr GpW w29 = GpW(29);
static constexpr GpW w30 = GpW(30);
static constexpr GpW wzr = GpW(Gp::kIdZr);
static constexpr GpW wsp = GpW(Gp::kIdSp);

static constexpr GpX x0 = GpX(0);
static constexpr GpX x1 = GpX(1);
static constexpr GpX x2 = GpX(2);
static constexpr GpX x3 = GpX(3);
static constexpr GpX x4 = GpX(4);
static constexpr GpX x5 = GpX(5);
static constexpr GpX x6 = GpX(6);
static constexpr GpX x7 = GpX(7);
static constexpr GpX x8 = GpX(8);
static constexpr GpX x9 = GpX(9);
static constexpr GpX x10 = GpX(10);
static constexpr GpX x11 = GpX(11);
static constexpr GpX x12 = GpX(12);
static constexpr GpX x13 = GpX(13);
static constexpr GpX x14 = GpX(14);
static constexpr GpX x15 = GpX(15);
static constexpr GpX x16 = GpX(16);
static constexpr GpX x17 = GpX(17);
static constexpr GpX x18 = GpX(18);
static constexpr GpX x19 = GpX(19);
static constexpr GpX x20 = GpX(20);
static constexpr GpX x21 = GpX(21);
static constexpr GpX x22 = GpX(22);
static constexpr GpX x23 = GpX(23);
static constexpr GpX x24 = GpX(24);
static constexpr GpX x25 = GpX(25);
static constexpr GpX x26 = GpX(26);
static constexpr GpX x27 = GpX(27);
static constexpr GpX x28 = GpX(28);
static constexpr GpX x29 = GpX(29);
static constexpr GpX x30 = GpX(30);
static constexpr GpX xzr = GpX(Gp::kIdZr);
static constexpr GpX sp = GpX(Gp::kIdSp);

static constexpr VecB b0 = VecB(0);
static constexpr VecB b1 = VecB(1);
static constexpr VecB b2 = VecB(2);
static constexpr VecB b3 = VecB(3);
static constexpr VecB b4 = VecB(4);
static constexpr VecB b5 = VecB(5);
static constexpr VecB b6 = VecB(6);
static constexpr VecB b7 = VecB(7);
static constexpr VecB b8 = VecB(8);
static constexpr VecB b9 = VecB(9);
static constexpr VecB b10 = VecB(10);
static constexpr VecB b11 = VecB(11);
static constexpr VecB b12 = VecB(12);
static constexpr VecB b13 = VecB(13);
static constexpr VecB b14 = VecB(14);
static constexpr VecB b15 = VecB(15);
static constexpr VecB b16 = VecB(16);
static constexpr VecB b17 = VecB(17);
static constexpr VecB b18 = VecB(18);
static constexpr VecB b19 = VecB(19);
static constexpr VecB b20 = VecB(20);
static constexpr VecB b21 = VecB(21);
static constexpr VecB b22 = VecB(22);
static constexpr VecB b23 = VecB(23);
static constexpr VecB b24 = VecB(24);
static constexpr VecB b25 = VecB(25);
static constexpr VecB b26 = VecB(26);
static constexpr VecB b27 = VecB(27);
static constexpr VecB b28 = VecB(28);
static constexpr VecB b29 = VecB(29);
static constexpr VecB b30 = VecB(30);
static constexpr VecB b31 = VecB(31);

static constexpr VecH h0 = VecH(0);
static constexpr VecH h1 = VecH(1);
static constexpr VecH h2 = VecH(2);
static constexpr VecH h3 = VecH(3);
static constexpr VecH h4 = VecH(4);
static constexpr VecH h5 = VecH(5);
static constexpr VecH h6 = VecH(6);
static constexpr VecH h7 = VecH(7);
static constexpr VecH h8 = VecH(8);
static constexpr VecH h9 = VecH(9);
static constexpr VecH h10 = VecH(10);
static constexpr VecH h11 = VecH(11);
static constexpr VecH h12 = VecH(12);
static constexpr VecH h13 = VecH(13);
static constexpr VecH h14 = VecH(14);
static constexpr VecH h15 = VecH(15);
static constexpr VecH h16 = VecH(16);
static constexpr VecH h17 = VecH(17);
static constexpr VecH h18 = VecH(18);
static constexpr VecH h19 = VecH(19);
static constexpr VecH h20 = VecH(20);
static constexpr VecH h21 = VecH(21);
static constexpr VecH h22 = VecH(22);
static constexpr VecH h23 = VecH(23);
static constexpr VecH h24 = VecH(24);
static constexpr VecH h25 = VecH(25);
static constexpr VecH h26 = VecH(26);
static constexpr VecH h27 = VecH(27);
static constexpr VecH h28 = VecH(28);
static constexpr VecH h29 = VecH(29);
static constexpr VecH h30 = VecH(30);
static constexpr VecH h31 = VecH(31);

static constexpr VecS s0 = VecS(0);
static constexpr VecS s1 = VecS(1);
static constexpr VecS s2 = VecS(2);
static constexpr VecS s3 = VecS(3);
static constexpr VecS s4 = VecS(4);
static constexpr VecS s5 = VecS(5);
static constexpr VecS s6 = VecS(6);
static constexpr VecS s7 = VecS(7);
static constexpr VecS s8 = VecS(8);
static constexpr VecS s9 = VecS(9);
static constexpr VecS s10 = VecS(10);
static constexpr VecS s11 = VecS(11);
static constexpr VecS s12 = VecS(12);
static constexpr VecS s13 = VecS(13);
static constexpr VecS s14 = VecS(14);
static constexpr VecS s15 = VecS(15);
static constexpr VecS s16 = VecS(16);
static constexpr VecS s17 = VecS(17);
static constexpr VecS s18 = VecS(18);
static constexpr VecS s19 = VecS(19);
static constexpr VecS s20 = VecS(20);
static constexpr VecS s21 = VecS(21);
static constexpr VecS s22 = VecS(22);
static constexpr VecS s23 = VecS(23);
static constexpr VecS s24 = VecS(24);
static constexpr VecS s25 = VecS(25);
static constexpr VecS s26 = VecS(26);
static constexpr VecS s27 = VecS(27);
static constexpr VecS s28 = VecS(28);
static constexpr VecS s29 = VecS(29);
static constexpr VecS s30 = VecS(30);
static constexpr VecS s31 = VecS(31);

static constexpr VecD d0 = VecD(0);
static constexpr VecD d1 = VecD(1);
static constexpr VecD d2 = VecD(2);
static constexpr VecD d3 = VecD(3);
static constexpr VecD d4 = VecD(4);
static constexpr VecD d5 = VecD(5);
static constexpr VecD d6 = VecD(6);
static constexpr VecD d7 = VecD(7);
static constexpr VecD d8 = VecD(8);
static constexpr VecD d9 = VecD(9);
static constexpr VecD d10 = VecD(10);
static constexpr VecD d11 = VecD(11);
static constexpr VecD d12 = VecD(12);
static constexpr VecD d13 = VecD(13);
static constexpr VecD d14 = VecD(14);
static constexpr VecD d15 = VecD(15);
static constexpr VecD d16 = VecD(16);
static constexpr VecD d17 = VecD(17);
static constexpr VecD d18 = VecD(18);
static constexpr VecD d19 = VecD(19);
static constexpr VecD d20 = VecD(20);
static constexpr VecD d21 = VecD(21);
static constexpr VecD d22 = VecD(22);
static constexpr VecD d23 = VecD(23);
static constexpr VecD d24 = VecD(24);
static constexpr VecD d25 = VecD(25);
static constexpr VecD d26 = VecD(26);
static constexpr VecD d27 = VecD(27);
static constexpr VecD d28 = VecD(28);
static constexpr VecD d29 = VecD(29);
static constexpr VecD d30 = VecD(30);
static constexpr VecD d31 = VecD(31);

static constexpr VecV q0 = VecV(0);
static constexpr VecV q1 = VecV(1);
static constexpr VecV q2 = VecV(2);
static constexpr VecV q3 = VecV(3);
static constexpr VecV q4 = VecV(4);
static constexpr VecV q5 = VecV(5);
static constexpr VecV q6 = VecV(6);
static constexpr VecV q7 = VecV(7);
static constexpr VecV q8 = VecV(8);
static constexpr VecV q9 = VecV(9);
static constexpr VecV q10 = VecV(10);
static constexpr VecV q11 = VecV(11);
static constexpr VecV q12 = VecV(12);
static constexpr VecV q13 = VecV(13);
static constexpr VecV q14 = VecV(14);
static constexpr VecV q15 = VecV(15);
static constexpr VecV q16 = VecV(16);
static constexpr VecV q17 = VecV(17);
static constexpr VecV q18 = VecV(18);
static constexpr VecV q19 = VecV(19);
static constexpr VecV q20 = VecV(20);
static constexpr VecV q21 = VecV(21);
static constexpr VecV q22 = VecV(22);
static constexpr VecV q23 = VecV(23);
static constexpr VecV q24 = VecV(24);
static constexpr VecV q25 = VecV(25);
static constexpr VecV q26 = VecV(26);
static constexpr VecV q27 = VecV(27);
static constexpr VecV q28 = VecV(28);
static constexpr VecV q29 = VecV(29);
static constexpr VecV q30 = VecV(30);
static constexpr VecV q31 = VecV(31);

static constexpr VecV v0 = VecV(0);
static constexpr VecV v1 = VecV(1);
static constexpr VecV v2 = VecV(2);
static constexpr VecV v3 = VecV(3);
static constexpr VecV v4 = VecV(4);
static constexpr VecV v5 = VecV(5);
static constexpr VecV v6 = VecV(6);
static constexpr VecV v7 = VecV(7);
static constexpr VecV v8 = VecV(8);
static constexpr VecV v9 = VecV(9);
static constexpr VecV v10 = VecV(10);
static constexpr VecV v11 = VecV(11);
static constexpr VecV v12 = VecV(12);
static constexpr VecV v13 = VecV(13);
static constexpr VecV v14 = VecV(14);
static constexpr VecV v15 = VecV(15);
static constexpr VecV v16 = VecV(16);
static constexpr VecV v17 = VecV(17);
static constexpr VecV v18 = VecV(18);
static constexpr VecV v19 = VecV(19);
static constexpr VecV v20 = VecV(20);
static constexpr VecV v21 = VecV(21);
static constexpr VecV v22 = VecV(22);
static constexpr VecV v23 = VecV(23);
static constexpr VecV v24 = VecV(24);
static constexpr VecV v25 = VecV(25);
static constexpr VecV v26 = VecV(26);
static constexpr VecV v27 = VecV(27);
static constexpr VecV v28 = VecV(28);
static constexpr VecV v29 = VecV(29);
static constexpr VecV v30 = VecV(30);
static constexpr VecV v31 = VecV(31);

#ifndef _DOXYGEN
} // {regs}

// Make `a64::regs` accessible through `a64` namespace as well.
using namespace regs;
#endif

//! \}

ASMJIT_END_SUB_NAMESPACE

#endif // ASMJIT_ARM_A64OPERAND_H_INCLUDED
