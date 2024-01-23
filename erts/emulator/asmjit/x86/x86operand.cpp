// This file is part of AsmJit project <https://asmjit.com>
//
// See asmjit.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#include "../core/api-build_p.h"
#if !defined(ASMJIT_NO_X86)

#include "../core/misc_p.h"
#include "../x86/x86operand.h"

ASMJIT_BEGIN_SUB_NAMESPACE(x86)

// x86::Operand - Tests
// ====================

#if defined(ASMJIT_TEST)
UNIT(x86_operand) {
  Label L(1000); // Label with some ID.

  INFO("Checking basic properties of built-in X86 registers");
  EXPECT_EQ(gpb(Gp::kIdAx), al);
  EXPECT_EQ(gpb(Gp::kIdBx), bl);
  EXPECT_EQ(gpb(Gp::kIdCx), cl);
  EXPECT_EQ(gpb(Gp::kIdDx), dl);

  EXPECT_EQ(gpb_lo(Gp::kIdAx), al);
  EXPECT_EQ(gpb_lo(Gp::kIdBx), bl);
  EXPECT_EQ(gpb_lo(Gp::kIdCx), cl);
  EXPECT_EQ(gpb_lo(Gp::kIdDx), dl);

  EXPECT_EQ(gpb_hi(Gp::kIdAx), ah);
  EXPECT_EQ(gpb_hi(Gp::kIdBx), bh);
  EXPECT_EQ(gpb_hi(Gp::kIdCx), ch);
  EXPECT_EQ(gpb_hi(Gp::kIdDx), dh);

  EXPECT_EQ(gpw(Gp::kIdAx), ax);
  EXPECT_EQ(gpw(Gp::kIdBx), bx);
  EXPECT_EQ(gpw(Gp::kIdCx), cx);
  EXPECT_EQ(gpw(Gp::kIdDx), dx);

  EXPECT_EQ(gpd(Gp::kIdAx), eax);
  EXPECT_EQ(gpd(Gp::kIdBx), ebx);
  EXPECT_EQ(gpd(Gp::kIdCx), ecx);
  EXPECT_EQ(gpd(Gp::kIdDx), edx);

  EXPECT_EQ(gpq(Gp::kIdAx), rax);
  EXPECT_EQ(gpq(Gp::kIdBx), rbx);
  EXPECT_EQ(gpq(Gp::kIdCx), rcx);
  EXPECT_EQ(gpq(Gp::kIdDx), rdx);

  EXPECT_NE(gpb(Gp::kIdAx), dl);
  EXPECT_NE(gpw(Gp::kIdBx), cx);
  EXPECT_NE(gpd(Gp::kIdCx), ebx);
  EXPECT_NE(gpq(Gp::kIdDx), rax);

  INFO("Checking if x86::reg(...) matches built-in IDs");
  EXPECT_EQ(gpb(5), bpl);
  EXPECT_EQ(gpw(5), bp);
  EXPECT_EQ(gpd(5), ebp);
  EXPECT_EQ(gpq(5), rbp);
  EXPECT_EQ(st(5) , st5);
  EXPECT_EQ(mm(5) , mm5);
  EXPECT_EQ(k(5)  , k5);
  EXPECT_EQ(cr(5) , cr5);
  EXPECT_EQ(dr(5) , dr5);
  EXPECT_EQ(xmm(5), xmm5);
  EXPECT_EQ(ymm(5), ymm5);
  EXPECT_EQ(zmm(5), zmm5);

  INFO("Checking x86::Gp register properties");
  EXPECT_TRUE(Gp().isReg());
  EXPECT_TRUE(eax.isReg());
  EXPECT_EQ(eax.id(), 0u);
  EXPECT_EQ(eax.size(), 4u);
  EXPECT_EQ(eax.type(), RegType::kX86_Gpd);
  EXPECT_EQ(eax.group(), RegGroup::kGp);

  INFO("Checking x86::Xmm register properties");
  EXPECT_TRUE(Xmm().isReg());
  EXPECT_TRUE(xmm4.isReg());
  EXPECT_EQ(xmm4.id(), 4u);
  EXPECT_EQ(xmm4.size(), 16u);
  EXPECT_EQ(xmm4.type(), RegType::kX86_Xmm);
  EXPECT_EQ(xmm4.group(), RegGroup::kVec);
  EXPECT_TRUE(xmm4.isVec());

  INFO("Checking x86::Ymm register properties");
  EXPECT_TRUE(Ymm().isReg());
  EXPECT_TRUE(ymm5.isReg());
  EXPECT_EQ(ymm5.id(), 5u);
  EXPECT_EQ(ymm5.size(), 32u);
  EXPECT_EQ(ymm5.type(), RegType::kX86_Ymm);
  EXPECT_EQ(ymm5.group(), RegGroup::kVec);
  EXPECT_TRUE(ymm5.isVec());

  INFO("Checking x86::Zmm register properties");
  EXPECT_TRUE(Zmm().isReg());
  EXPECT_TRUE(zmm6.isReg());
  EXPECT_EQ(zmm6.id(), 6u);
  EXPECT_EQ(zmm6.size(), 64u);
  EXPECT_EQ(zmm6.type(), RegType::kX86_Zmm);
  EXPECT_EQ(zmm6.group(), RegGroup::kVec);
  EXPECT_TRUE(zmm6.isVec());

  INFO("Checking x86::Vec register properties");
  EXPECT_TRUE(Vec().isReg());
  // Converts a VEC register to a type of the passed register, but keeps the ID.
  EXPECT_EQ(xmm4.cloneAs(ymm10), ymm4);
  EXPECT_EQ(xmm4.cloneAs(zmm11), zmm4);
  EXPECT_EQ(ymm5.cloneAs(xmm12), xmm5);
  EXPECT_EQ(ymm5.cloneAs(zmm13), zmm5);
  EXPECT_EQ(zmm6.cloneAs(xmm14), xmm6);
  EXPECT_EQ(zmm6.cloneAs(ymm15), ymm6);

  EXPECT_EQ(xmm7.xmm(), xmm7);
  EXPECT_EQ(xmm7.ymm(), ymm7);
  EXPECT_EQ(xmm7.zmm(), zmm7);

  EXPECT_EQ(ymm7.xmm(), xmm7);
  EXPECT_EQ(ymm7.ymm(), ymm7);
  EXPECT_EQ(ymm7.zmm(), zmm7);

  EXPECT_EQ(zmm7.xmm(), xmm7);
  EXPECT_EQ(zmm7.ymm(), ymm7);
  EXPECT_EQ(zmm7.zmm(), zmm7);

  INFO("Checking x86::Mm register properties");
  EXPECT_TRUE(Mm().isReg());
  EXPECT_TRUE(mm2.isReg());
  EXPECT_EQ(mm2.id(), 2u);
  EXPECT_EQ(mm2.size(), 8u);
  EXPECT_EQ(mm2.type(), RegType::kX86_Mm);
  EXPECT_EQ(mm2.group(), RegGroup::kX86_MM);

  INFO("Checking x86::KReg register properties");
  EXPECT_TRUE(KReg().isReg());
  EXPECT_TRUE(k3.isReg());
  EXPECT_EQ(k3.id(), 3u);
  EXPECT_EQ(k3.size(), 0u);
  EXPECT_EQ(k3.type(), RegType::kX86_KReg);
  EXPECT_EQ(k3.group(), RegGroup::kX86_K);

  INFO("Checking x86::St register properties");
  EXPECT_TRUE(St().isReg());
  EXPECT_TRUE(st1.isReg());
  EXPECT_EQ(st1.id(), 1u);
  EXPECT_EQ(st1.size(), 10u);
  EXPECT_EQ(st1.type(), RegType::kX86_St);
  EXPECT_EQ(st1.group(), RegGroup::kX86_St);

  INFO("Checking if default constructed regs behave as expected");
  EXPECT_FALSE(Reg().isValid());
  EXPECT_FALSE(Gp().isValid());
  EXPECT_FALSE(Xmm().isValid());
  EXPECT_FALSE(Ymm().isValid());
  EXPECT_FALSE(Zmm().isValid());
  EXPECT_FALSE(Mm().isValid());
  EXPECT_FALSE(KReg().isValid());
  EXPECT_FALSE(SReg().isValid());
  EXPECT_FALSE(CReg().isValid());
  EXPECT_FALSE(DReg().isValid());
  EXPECT_FALSE(St().isValid());
  EXPECT_FALSE(Bnd().isValid());

  INFO("Checking x86::Mem operand");
  Mem m;
  EXPECT_EQ(m, Mem());

  m = ptr(L);
  EXPECT_TRUE(m.hasBase());
  EXPECT_FALSE(m.hasBaseReg());
  EXPECT_TRUE(m.hasBaseLabel());
  EXPECT_FALSE(m.hasOffset());
  EXPECT_FALSE(m.isOffset64Bit());
  EXPECT_EQ(m.offset(), 0);
  EXPECT_EQ(m.offsetLo32(), 0);

  m = ptr(0x0123456789ABCDEFu);
  EXPECT_FALSE(m.hasBase());
  EXPECT_FALSE(m.hasBaseReg());
  EXPECT_FALSE(m.hasIndex());
  EXPECT_FALSE(m.hasIndexReg());
  EXPECT_TRUE(m.hasOffset());
  EXPECT_TRUE(m.isOffset64Bit());
  EXPECT_EQ(m.offset(), int64_t(0x0123456789ABCDEFu));
  EXPECT_EQ(m.offsetLo32(), int32_t(0x89ABCDEFu));
  m.addOffset(1);
  EXPECT_EQ(m.offset(), int64_t(0x0123456789ABCDF0u));

  m = ptr(0x0123456789ABCDEFu, rdi, 3);
  EXPECT_FALSE(m.hasSegment());
  EXPECT_FALSE(m.hasBase());
  EXPECT_FALSE(m.hasBaseReg());
  EXPECT_TRUE(m.hasIndex());
  EXPECT_TRUE(m.hasIndexReg());
  EXPECT_EQ(m.indexType(), rdi.type());
  EXPECT_EQ(m.indexId(), rdi.id());
  EXPECT_EQ(m.shift(), 3u);
  EXPECT_TRUE(m.hasOffset());
  EXPECT_TRUE(m.isOffset64Bit());
  EXPECT_EQ(m.offset(), int64_t(0x0123456789ABCDEFu));
  EXPECT_EQ(m.offsetLo32(), int32_t(0x89ABCDEFu));
  m.resetIndex();
  EXPECT_FALSE(m.hasIndex());
  EXPECT_FALSE(m.hasIndexReg());

  m = ptr(rax);
  EXPECT_TRUE(m.hasBase());
  EXPECT_TRUE(m.hasBaseReg());
  EXPECT_EQ(m.baseType(), rax.type());
  EXPECT_EQ(m.baseId(), rax.id());
  EXPECT_FALSE(m.hasIndex());
  EXPECT_FALSE(m.hasIndexReg());
  EXPECT_EQ(m.indexType(), RegType::kNone);
  EXPECT_EQ(m.indexId(), 0u);
  EXPECT_FALSE(m.hasOffset());
  EXPECT_FALSE(m.isOffset64Bit());
  EXPECT_EQ(m.offset(), 0);
  EXPECT_EQ(m.offsetLo32(), 0);
  m.setIndex(rsi);
  EXPECT_TRUE(m.hasIndex());
  EXPECT_TRUE(m.hasIndexReg());
  EXPECT_EQ(m.indexType(), rsi.type());
  EXPECT_EQ(m.indexId(), rsi.id());
}
#endif

ASMJIT_END_SUB_NAMESPACE

#endif // !ASMJIT_NO_X86
