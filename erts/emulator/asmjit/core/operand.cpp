// This file is part of AsmJit project <https://asmjit.com>
//
// See asmjit.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#include "../core/api-build_p.h"
#include "../core/operand.h"

ASMJIT_BEGIN_NAMESPACE

// Operand - Tests
// ===============

#if defined(ASMJIT_TEST)
enum class StrongEnumForImmTests : uint32_t {
  kValue0,
  kValue0xFFFFFFFF = 0xFFFFFFFFu
};

UNIT(operand) {
  INFO("Checking operand sizes");
  EXPECT_EQ(sizeof(Operand), 16u);
  EXPECT_EQ(sizeof(BaseReg), 16u);
  EXPECT_EQ(sizeof(BaseMem), 16u);
  EXPECT_EQ(sizeof(Imm), 16u);
  EXPECT_EQ(sizeof(Label), 16u);

  INFO("Checking basic functionality of Operand");
  Operand a, b;
  Operand dummy;

  EXPECT_TRUE(a.isNone());
  EXPECT_FALSE(a.isReg());
  EXPECT_FALSE(a.isMem());
  EXPECT_FALSE(a.isImm());
  EXPECT_FALSE(a.isLabel());
  EXPECT_EQ(a, b);
  EXPECT_EQ(a._data[0], 0u);
  EXPECT_EQ(a._data[1], 0u);

  INFO("Checking basic functionality of Label");
  Label label;
  EXPECT_FALSE(label.isValid());
  EXPECT_EQ(label.id(), Globals::kInvalidId);

  INFO("Checking basic functionality of BaseReg");
  EXPECT_TRUE(BaseReg().isReg());
  EXPECT_FALSE(BaseReg().isValid());
  EXPECT_EQ(BaseReg()._data[0], 0u);
  EXPECT_EQ(BaseReg()._data[1], 0u);
  EXPECT_FALSE(dummy.as<BaseReg>().isValid());

  // Create some register (not specific to any architecture).
  OperandSignature rSig = OperandSignature::fromOpType(OperandType::kReg) |
                          OperandSignature::fromRegType(RegType::kVec128) |
                          OperandSignature::fromRegGroup(RegGroup::kVec) |
                          OperandSignature::fromSize(8);
  BaseReg r1(rSig, 5);

  EXPECT_TRUE(r1.isValid());
  EXPECT_TRUE(r1.isReg());
  EXPECT_TRUE(r1.isReg(RegType::kVec128));
  EXPECT_TRUE(r1.isPhysReg());
  EXPECT_FALSE(r1.isVirtReg());
  EXPECT_EQ(r1.signature(), rSig);
  EXPECT_EQ(r1.type(), RegType::kVec128);
  EXPECT_EQ(r1.group(), RegGroup::kVec);
  EXPECT_EQ(r1.size(), 8u);
  EXPECT_EQ(r1.id(), 5u);
  EXPECT_TRUE(r1.isReg(RegType::kVec128, 5)); // RegType and Id.
  EXPECT_EQ(r1._data[0], 0u);
  EXPECT_EQ(r1._data[1], 0u);

  // The same type of register having different id.
  BaseReg r2(r1, 6);
  EXPECT_TRUE(r2.isValid());
  EXPECT_TRUE(r2.isReg());
  EXPECT_TRUE(r2.isReg(RegType::kVec128));
  EXPECT_TRUE(r2.isPhysReg());
  EXPECT_FALSE(r2.isVirtReg());
  EXPECT_EQ(r2.signature(), rSig);
  EXPECT_EQ(r2.type(), r1.type());
  EXPECT_EQ(r2.group(), r1.group());
  EXPECT_EQ(r2.size(), r1.size());
  EXPECT_EQ(r2.id(), 6u);
  EXPECT_TRUE(r2.isReg(RegType::kVec128, 6));

  r1.reset();
  EXPECT_FALSE(r1.isReg());
  EXPECT_FALSE(r1.isValid());

  INFO("Checking basic functionality of BaseMem");
  BaseMem m;
  EXPECT_TRUE(m.isMem());
  EXPECT_EQ(m, BaseMem());
  EXPECT_FALSE(m.hasBase());
  EXPECT_FALSE(m.hasIndex());
  EXPECT_FALSE(m.hasOffset());
  EXPECT_TRUE(m.isOffset64Bit());
  EXPECT_EQ(m.offset(), 0);

  m.setOffset(-1);
  EXPECT_EQ(m.offsetLo32(), -1);
  EXPECT_EQ(m.offset(), -1);

  int64_t x = int64_t(0xFF00FF0000000001u);
  int32_t xHi = int32_t(0xFF00FF00u);

  m.setOffset(x);
  EXPECT_EQ(m.offset(), x);
  EXPECT_EQ(m.offsetLo32(), 1);
  EXPECT_EQ(m.offsetHi32(), xHi);

  INFO("Checking basic functionality of Imm");
  Imm immValue(-42);
  EXPECT_EQ(immValue.type(), ImmType::kInt);
  EXPECT_EQ(Imm(-1).value(), -1);
  EXPECT_EQ(imm(-1).value(), -1);
  EXPECT_EQ(immValue.value(), -42);
  EXPECT_EQ(imm(0xFFFFFFFF).value(), int64_t(0xFFFFFFFF));

  Imm immDouble(0.4);
  EXPECT_EQ(immDouble.type(), ImmType::kDouble);
  EXPECT_EQ(immDouble.valueAs<double>(), 0.4);
  EXPECT_EQ(immDouble, imm(0.4));

  EXPECT_EQ(Imm(StrongEnumForImmTests::kValue0).value(), 0);
  EXPECT_EQ(Imm(StrongEnumForImmTests::kValue0xFFFFFFFF).value(), 0xFFFFFFFFu);
}
#endif

ASMJIT_END_NAMESPACE
