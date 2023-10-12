// This file is part of AsmJit project <https://asmjit.com>
//
// See asmjit.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#include "../core/api-build_p.h"
#if !defined(ASMJIT_NO_AARCH64)

#include "../core/misc_p.h"
#include "../arm/a64operand.h"

ASMJIT_BEGIN_SUB_NAMESPACE(a64)

// a64::Operand - Tests
// ====================

#if defined(ASMJIT_TEST)
UNIT(a64_operand) {
  INFO("Checking if a64::reg(...) matches built-in IDs");
  EXPECT(w(5) == w5);
  EXPECT(x(5) == x5);

  INFO("Checking Gp register properties");
  EXPECT(Gp().isReg() == true);
  EXPECT(w0.isReg() == true);
  EXPECT(x0.isReg() == true);
  EXPECT(w0.id() == 0);
  EXPECT(x0.id() == 0);
  EXPECT(wzr.id() == Gp::kIdZr);
  EXPECT(xzr.id() == Gp::kIdZr);
  EXPECT(wsp.id() == Gp::kIdSp);
  EXPECT(sp.id() == Gp::kIdSp);
  EXPECT(w0.size() == 4);
  EXPECT(x0.size() == 8);
  EXPECT(w0.type() == RegType::kARM_GpW);
  EXPECT(x0.type() == RegType::kARM_GpX);
  EXPECT(w0.group() == RegGroup::kGp);
  EXPECT(x0.group() == RegGroup::kGp);

  INFO("Checking Vec register properties");
  EXPECT(v0.type() == RegType::kARM_VecV);
  EXPECT(d0.type() == RegType::kARM_VecD);
  EXPECT(s0.type() == RegType::kARM_VecS);
  EXPECT(h0.type() == RegType::kARM_VecH);
  EXPECT(b0.type() == RegType::kARM_VecB);

  EXPECT(v0.group() == RegGroup::kVec);
  EXPECT(d0.group() == RegGroup::kVec);
  EXPECT(s0.group() == RegGroup::kVec);
  EXPECT(h0.group() == RegGroup::kVec);
  EXPECT(b0.group() == RegGroup::kVec);

  INFO("Checking Vec register element[] access");
  Vec vd_1 = v15.d(1);
  EXPECT(vd_1.type() == RegType::kARM_VecV);
  EXPECT(vd_1.group() == RegGroup::kVec);
  EXPECT(vd_1.id() == 15);
  EXPECT(vd_1.isVecD2());
  EXPECT(vd_1.elementType() == Vec::kElementTypeD);
  EXPECT(vd_1.hasElementIndex());
  EXPECT(vd_1.elementIndex() == 1);

  Vec vs_3 = v15.s(3);
  EXPECT(vs_3.type() == RegType::kARM_VecV);
  EXPECT(vs_3.group() == RegGroup::kVec);
  EXPECT(vs_3.id() == 15);
  EXPECT(vs_3.isVecS4());
  EXPECT(vs_3.elementType() == Vec::kElementTypeS);
  EXPECT(vs_3.hasElementIndex());
  EXPECT(vs_3.elementIndex() == 3);

  Vec vb_4 = v15.b4(3);
  EXPECT(vb_4.type() == RegType::kARM_VecV);
  EXPECT(vb_4.group() == RegGroup::kVec);
  EXPECT(vb_4.id() == 15);
  EXPECT(vb_4.isVecB4x4());
  EXPECT(vb_4.elementType() == Vec::kElementTypeB4);
  EXPECT(vb_4.hasElementIndex());
  EXPECT(vb_4.elementIndex() == 3);
}
#endif

ASMJIT_END_SUB_NAMESPACE

#endif // !ASMJIT_NO_AARCH64
