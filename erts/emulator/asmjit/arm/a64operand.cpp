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

#include "../core/api-build_p.h"
#if !defined(ASMJIT_NO_ARM)

#include "../core/misc_p.h"
#include "../arm/a64operand.h"

ASMJIT_BEGIN_SUB_NAMESPACE(a64)

// ============================================================================
// [asmjit::ArmOperand - Unit]
// ============================================================================

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
  EXPECT(w0.type() == Reg::kTypeGpW);
  EXPECT(x0.type() == Reg::kTypeGpX);
  EXPECT(w0.group() == Reg::kGroupGp);
  EXPECT(x0.group() == Reg::kGroupGp);

  INFO("Checking Vec register properties");
  Vec vd = v15.d(1);
  EXPECT(vd.isVecD2());
  EXPECT(vd.elementType() == Vec::kElementTypeD);
  EXPECT(vd.hasElementIndex());
  EXPECT(vd.elementIndex() == 1);

  Vec vs = v15.s(3);
  EXPECT(vs.id() == 15);
  EXPECT(vs.isVecS4());
  EXPECT(vs.elementType() == Vec::kElementTypeS);
  EXPECT(vs.hasElementIndex());
  EXPECT(vs.elementIndex() == 3);
}
#endif

ASMJIT_END_SUB_NAMESPACE

#endif // !ASMJIT_NO_ARM
