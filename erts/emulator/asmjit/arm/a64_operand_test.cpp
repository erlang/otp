// This file is part of AsmJit project <https://asmjit.com>
//
// See <asmjit/core.h> or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#include <asmjit/core/api_build_test_p.h>
#if defined(ASMJIT_TEST) && !defined(ASMJIT_NO_AARCH64)

#include <asmjit/core/misc_p.h>
#include <asmjit/arm/a64_operand.h>

ASMJIT_BEGIN_SUB_NAMESPACE(a64)

// a64::Operand - Tests
// ====================

TEST_CASE(a64_operand_gp) {
  EXPECT_EQ(w(5), w5);
  EXPECT_EQ(x(5), x5);
  EXPECT_EQ(Gp::make_w(5), gp32(5));
  EXPECT_EQ(Gp::make_x(5), gp64(5));
  EXPECT_EQ(x5.w(), w5);
  EXPECT_EQ(w5.x(), x5);

  EXPECT_TRUE(Gp().is_reg());
  EXPECT_TRUE(w0.is_reg());
  EXPECT_TRUE(x0.is_reg());
  EXPECT_EQ(w0.id(), 0u);
  EXPECT_EQ(x0.id(), 0u);
  EXPECT_EQ(wzr.id(), Gp::kIdZr);
  EXPECT_EQ(xzr.id(), Gp::kIdZr);
  EXPECT_EQ(wsp.id(), Gp::kIdSp);
  EXPECT_EQ(sp.id(), Gp::kIdSp);
  EXPECT_TRUE(wzr.is_zr());
  EXPECT_FALSE(sp.is_zr());
  EXPECT_TRUE(wsp.is_sp());
  EXPECT_FALSE(xzr.is_sp());
  EXPECT_EQ(w0.size(), 4u);
  EXPECT_EQ(x0.size(), 8u);
  EXPECT_EQ(w0.reg_type(), RegType::kGp32);
  EXPECT_EQ(x0.reg_type(), RegType::kGp64);
  EXPECT_EQ(w0.reg_group(), RegGroup::kGp);
  EXPECT_EQ(x0.reg_group(), RegGroup::kGp);
}

TEST_CASE(a64_operand_vec) {
  EXPECT_EQ(Vec::make_q(0).reg_type(), RegType::kVec128);
  EXPECT_EQ(Vec::make_d(0).reg_type(), RegType::kVec64);
  EXPECT_EQ(Vec::make_s(0).reg_type(), RegType::kVec32);
  EXPECT_EQ(Vec::make_h(0).reg_type(), RegType::kVec16);
  EXPECT_EQ(Vec::make_b(0).reg_type(), RegType::kVec8);

  EXPECT_EQ(v0.reg_group(), RegGroup::kVec);
  EXPECT_EQ(d0.reg_group(), RegGroup::kVec);
  EXPECT_EQ(s0.reg_group(), RegGroup::kVec);
  EXPECT_EQ(h0.reg_group(), RegGroup::kVec);
  EXPECT_EQ(b0.reg_group(), RegGroup::kVec);

  Vec vd_1 = v15.d(1);
  EXPECT_EQ(vd_1.reg_type(), RegType::kVec128);
  EXPECT_EQ(vd_1.reg_group(), RegGroup::kVec);
  EXPECT_EQ(vd_1.id(), 15u);
  EXPECT_TRUE(vd_1.is_vec_d2());
  EXPECT_EQ(vd_1.element_type(), VecElementType::kD);
  EXPECT_TRUE(vd_1.has_element_index());
  EXPECT_EQ(vd_1.element_index(), 1u);

  Vec vs_3 = v15.s(3);
  EXPECT_EQ(vs_3.reg_type(), RegType::kVec128);
  EXPECT_EQ(vs_3.reg_group(), RegGroup::kVec);
  EXPECT_EQ(vs_3.id(), 15u);
  EXPECT_TRUE(vs_3.is_vec_s4());
  EXPECT_EQ(vs_3.element_type(), VecElementType::kS);
  EXPECT_TRUE(vs_3.has_element_index());
  EXPECT_EQ(vs_3.element_index(), 3u);

  Vec vb_4 = v15.b4(3);
  EXPECT_EQ(vb_4.reg_type(), RegType::kVec128);
  EXPECT_EQ(vb_4.reg_group(), RegGroup::kVec);
  EXPECT_EQ(vb_4.id(), 15u);
  EXPECT_TRUE(vb_4.is_vec_b4x4());
  EXPECT_EQ(vb_4.element_type(), VecElementType::kB4);
  EXPECT_TRUE(vb_4.has_element_index());
  EXPECT_EQ(vb_4.element_index(), 3u);

  EXPECT_EQ(vb_4.b(), b(15));
  EXPECT_EQ(vb_4.h(), h(15));
  EXPECT_EQ(vb_4.s(), s(15));
  EXPECT_EQ(vb_4.d(), d(15));
  EXPECT_EQ(vb_4.q(), q(15));
  EXPECT_EQ(vb_4.v8(), b15);
  EXPECT_EQ(vb_4.v16(), h15);
  EXPECT_EQ(vb_4.v32(), s15);
  EXPECT_EQ(vb_4.v64(), d15);
  EXPECT_EQ(vb_4.v128(), v(15));

  EXPECT_TRUE(v15.b8().is_vec_b8());
  EXPECT_TRUE(v15.h4().is_vec_h4());
  EXPECT_TRUE(v15.s2().is_vec_s2());
  EXPECT_TRUE(v15.d().is_vec_d1());
  EXPECT_TRUE(v15.b16().is_vec_b16());
  EXPECT_TRUE(v15.h8().is_vec_h8());
  EXPECT_TRUE(v15.s4().is_vec_s4());
  EXPECT_TRUE(v15.d2().is_vec_d2());
  EXPECT_TRUE(v15.h2(3).is_vec_h2x4());
  EXPECT_EQ(v15.h2().reg_type(), RegType::kVec32);
  EXPECT_EQ(v15.h2().element_type(), VecElementType::kH);
  EXPECT_EQ(v15.b(3), v15.b16().at(3));
  EXPECT_EQ(v15.h(3), v15.h8().at(3));
  EXPECT_EQ(vs_3.at(1), v15.s(1));

  vb_4.set_element_type(VecElementType::kH2);
  vb_4.set_element_index(1);
  EXPECT_EQ(vb_4, v15.h2(1));
  EXPECT_TRUE(vb_4.has_element_type_or_index());
  vb_4.reset_element_index();
  EXPECT_FALSE(vb_4.has_element_index());
  EXPECT_EQ(vb_4.element_index(), 0u);
  EXPECT_TRUE(vb_4.has_element_type());
  vb_4.reset_element_type();
  EXPECT_FALSE(vb_4.has_element_type_or_index());
  EXPECT_EQ(vb_4, v15);
}

TEST_CASE(a64_operand_mem) {
  a64::Mem m_base(Globals::NoInit);
  m_base = a64::Mem();
  EXPECT_EQ(m_base.clone(), a64::ptr(uint64_t(0)));

  m_base = a64::ptr(x5);
  EXPECT_TRUE(m_base.has_base());
  EXPECT_EQ(m_base.base_reg(), x5);
  EXPECT_FALSE(m_base.has_index());
  EXPECT_EQ(m_base.offset(), 0);
  EXPECT_FALSE(m_base.has_offset());

  a64::Mem m_off = a64::ptr(x25, 128);
  EXPECT_TRUE(m_off.has_base());
  EXPECT_EQ(m_off.base_reg(), x25);
  EXPECT_FALSE(m_off.has_index());
  EXPECT_EQ(m_off.offset(), 128);
  EXPECT_TRUE(m_off.has_offset());

  a64::Mem m_idx = a64::ptr(x16, x17);
  EXPECT_TRUE(m_idx.has_base());
  EXPECT_EQ(m_idx.base_reg(), x16);
  EXPECT_TRUE(m_idx.has_index());
  EXPECT_EQ(m_idx.index_reg(), x17);
  EXPECT_EQ(m_idx.offset(), 0);
  EXPECT_FALSE(m_idx.has_offset());

  EXPECT_NE(m_base, m_off);
  EXPECT_NE(m_base, m_idx);

  a64::Mem m_cloned = m_off.clone_adjusted(-128);
  m_cloned.set_base_id(5);
  EXPECT_EQ(m_base, m_cloned);

  EXPECT_TRUE(m_off.is_fixed_offset());
  EXPECT_FALSE(m_off.is_pre_or_post());
  EXPECT_TRUE(m_off.pre().is_pre_index());
  EXPECT_TRUE(m_off.post().is_post_index());
  EXPECT_EQ(m_off.pre(), ptr_pre(x25, 128));
  EXPECT_EQ(m_off.post(), ptr_post(x25, 128));
  EXPECT_EQ(m_off.pre(-64), ptr_pre(x25, 64));
  EXPECT_EQ(m_off.post(-64), ptr_post(x25, 64));
  EXPECT_EQ(m_idx.pre(), ptr_pre(x16, x17));
  EXPECT_EQ(m_idx.post(), ptr_post(x16, x17));

  m_cloned.make_pre_index();
  EXPECT_TRUE(m_cloned.is_pre_or_post());
  EXPECT_EQ(m_cloned, m_base.pre());
  m_cloned.make_post_index();
  EXPECT_EQ(m_cloned, m_base.post());
  m_cloned.reset_offset_mode();
  EXPECT_EQ(m_cloned, m_base);

  m_idx.set_index(w17, uxtw(2));
  EXPECT_EQ(m_idx, ptr(x16, w17, uxtw(2)));
  EXPECT_EQ(m_idx.shift_op(), ShiftOp::kUXTW);
  EXPECT_EQ(m_idx.shift(), 2u);
  EXPECT_TRUE(m_idx.has_shift());
  m_idx.reset_shift_op();
  EXPECT_EQ(m_idx, ptr(x16, w17, lsl(2)));
  m_idx.set_index(x17, 3u);
  EXPECT_EQ(m_idx, ptr(x16, x17, lsl(3)));
  m_idx.set_shift_op(ShiftOp::kSXTX);
  EXPECT_EQ(m_idx, ptr(x16, x17, sxtx(3)));
  m_idx.reset_shift();
  EXPECT_FALSE(m_idx.has_shift());
  EXPECT_EQ(m_idx.shift_op(), ShiftOp::kSXTX);
  EXPECT_EQ(Mem(m_idx.signature(), 16, 17, 0), m_idx);

  m_cloned = ptr(Label(1), -16);
  EXPECT_TRUE(m_cloned.has_base_label());
  EXPECT_EQ(m_cloned.base_id(), 1u);
  EXPECT_EQ(m_cloned.offset(), -16);

  m_cloned = ptr(0x0123456789ABCDEFu);
  EXPECT_FALSE(m_cloned.has_base());
  EXPECT_EQ(m_cloned.offset(), int64_t(0x0123456789ABCDEFu));
}

TEST_CASE(a64_operand_shift) {
  struct ShiftCase {
    Shift (*make)(uint32_t) noexcept;
    ShiftOp op;
  };

  ShiftCase shifts[] = {
    {lsl, ShiftOp::kLSL},
    {lsr, ShiftOp::kLSR},
    {asr, ShiftOp::kASR},
    {ror, ShiftOp::kROR},
    {msl, ShiftOp::kMSL},
    {uxtb, ShiftOp::kUXTB},
    {uxth, ShiftOp::kUXTH},
    {uxtw, ShiftOp::kUXTW},
    {uxtx, ShiftOp::kUXTX},
    {sxtb, ShiftOp::kSXTB},
    {sxth, ShiftOp::kSXTH},
    {sxtw, ShiftOp::kSXTW},
    {sxtx, ShiftOp::kSXTX}
  };

  for (const ShiftCase& test : shifts) {
    Shift shift = test.make(3);
    EXPECT_EQ(shift.op(), test.op);
    EXPECT_EQ(shift.value(), 3u);
  }

  EXPECT_EQ(rrx().op(), ShiftOp::kRRX);
  EXPECT_EQ(rrx().value(), 0u);
}

ASMJIT_END_SUB_NAMESPACE

#endif // ASMJIT_TEST && !ASMJIT_NO_AARCH64
