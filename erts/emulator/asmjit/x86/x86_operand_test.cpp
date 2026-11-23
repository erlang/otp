// This file is part of AsmJit project <https://asmjit.com>
//
// See <asmjit/core.h> or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#include <asmjit/core/api_build_test_p.h>
#if defined(ASMJIT_TEST) && !defined(ASMJIT_NO_X86)

#include <asmjit/core/misc_p.h>
#include <asmjit/x86/x86_operand.h>

ASMJIT_BEGIN_SUB_NAMESPACE(x86)

TEST_CASE(x86_operand_gp) {
  EXPECT_TRUE(Gp().is_reg());
  EXPECT_FALSE(Gp().is_valid());

  EXPECT_TRUE(al.is_reg());
  EXPECT_TRUE(al.is_phys_reg());
  EXPECT_TRUE(al.is_valid());
  EXPECT_TRUE(al.is_gp());
  EXPECT_TRUE(al.is_gp(0));
  EXPECT_TRUE(al.is_gp8());
  EXPECT_TRUE(al.is_gp8(0));
  EXPECT_TRUE(al.is_gp8_lo());
  EXPECT_TRUE(al.is_gp8_lo(0));
  EXPECT_FALSE(al.is_virt_reg());
  EXPECT_FALSE(al.is_none());
  EXPECT_FALSE(al.is_gp8_lo(3));
  EXPECT_FALSE(al.is_gp8_hi());
  EXPECT_FALSE(al.is_vec());
  EXPECT_FALSE(al.is_kreg());
  EXPECT_FALSE(al.is_debug_reg());
  EXPECT_FALSE(al.is_control_reg());

  EXPECT_EQ(gpb(Gp::kIdAx), al);
  EXPECT_EQ(gpb(Gp::kIdBx), bl);
  EXPECT_EQ(gpb(Gp::kIdCx), cl);
  EXPECT_EQ(gpb(Gp::kIdDx), dl);

  EXPECT_EQ(gp8(Gp::kIdAx), al);
  EXPECT_EQ(gp8(Gp::kIdBx), bl);
  EXPECT_EQ(gp8(Gp::kIdCx), cl);
  EXPECT_EQ(gp8(Gp::kIdDx), dl);

  EXPECT_EQ(gpb_lo(Gp::kIdAx), al);
  EXPECT_EQ(gpb_lo(Gp::kIdBx), bl);
  EXPECT_EQ(gpb_lo(Gp::kIdCx), cl);
  EXPECT_EQ(gpb_lo(Gp::kIdDx), dl);

  EXPECT_EQ(gp8_lo(Gp::kIdAx), al);
  EXPECT_EQ(gp8_lo(Gp::kIdBx), bl);
  EXPECT_EQ(gp8_lo(Gp::kIdCx), cl);
  EXPECT_EQ(gp8_lo(Gp::kIdDx), dl);

  EXPECT_EQ(gpb_hi(Gp::kIdAx), ah);
  EXPECT_EQ(gpb_hi(Gp::kIdBx), bh);
  EXPECT_EQ(gpb_hi(Gp::kIdCx), ch);
  EXPECT_EQ(gpb_hi(Gp::kIdDx), dh);

  EXPECT_EQ(gp8_hi(Gp::kIdAx), ah);
  EXPECT_EQ(gp8_hi(Gp::kIdBx), bh);
  EXPECT_EQ(gp8_hi(Gp::kIdCx), ch);
  EXPECT_EQ(gp8_hi(Gp::kIdDx), dh);

  EXPECT_EQ(gpw(Gp::kIdAx), ax);
  EXPECT_EQ(gpw(Gp::kIdBx), bx);
  EXPECT_EQ(gpw(Gp::kIdCx), cx);
  EXPECT_EQ(gpw(Gp::kIdDx), dx);

  EXPECT_EQ(gp16(Gp::kIdAx), ax);
  EXPECT_EQ(gp16(Gp::kIdBx), bx);
  EXPECT_EQ(gp16(Gp::kIdCx), cx);
  EXPECT_EQ(gp16(Gp::kIdDx), dx);

  EXPECT_EQ(gpd(Gp::kIdAx), eax);
  EXPECT_EQ(gpd(Gp::kIdBx), ebx);
  EXPECT_EQ(gpd(Gp::kIdCx), ecx);
  EXPECT_EQ(gpd(Gp::kIdDx), edx);

  EXPECT_EQ(gp32(Gp::kIdAx), eax);
  EXPECT_EQ(gp32(Gp::kIdBx), ebx);
  EXPECT_EQ(gp32(Gp::kIdCx), ecx);
  EXPECT_EQ(gp32(Gp::kIdDx), edx);

  EXPECT_EQ(gpq(Gp::kIdAx), rax);
  EXPECT_EQ(gpq(Gp::kIdBx), rbx);
  EXPECT_EQ(gpq(Gp::kIdCx), rcx);
  EXPECT_EQ(gpq(Gp::kIdDx), rdx);

  EXPECT_EQ(gp64(Gp::kIdAx), rax);
  EXPECT_EQ(gp64(Gp::kIdBx), rbx);
  EXPECT_EQ(gp64(Gp::kIdCx), rcx);
  EXPECT_EQ(gp64(Gp::kIdDx), rdx);

  EXPECT_NE(gpb(Gp::kIdAx), dl);
  EXPECT_NE(gpw(Gp::kIdBx), cx);
  EXPECT_NE(gpd(Gp::kIdCx), ebx);
  EXPECT_NE(gpq(Gp::kIdDx), rax);

  EXPECT_EQ(gpb(5), bpl);
  EXPECT_EQ(gpw(5), bp);
  EXPECT_EQ(gpd(5), ebp);
  EXPECT_EQ(gpq(5), rbp);

  EXPECT_TRUE(Gp::make_r32(0).is_reg());
  EXPECT_TRUE(Gp::make_r32(0).is_gp());
  EXPECT_TRUE(Gp::make_r32(0).is_gp32());
  EXPECT_TRUE(eax.is_reg());
  EXPECT_TRUE(eax.is_gp());
  EXPECT_TRUE(eax.is_gp(0));
  EXPECT_TRUE(eax.is_gp32());
  EXPECT_TRUE(eax.is_gp32(0));
  EXPECT_FALSE(eax.is_gp32(1));
  EXPECT_EQ(eax.id(), 0u);
  EXPECT_EQ(eax.size(), 4u);
  EXPECT_EQ(eax.reg_type(), RegType::kGp32);
  EXPECT_EQ(eax.reg_group(), RegGroup::kGp);

  EXPECT_TRUE(Gp::make_r64(0).is_reg());
  EXPECT_TRUE(Gp::make_r64(0).is_gp());
  EXPECT_TRUE(Gp::make_r64(0).is_gp64());
  EXPECT_TRUE(rax.is_reg());
  EXPECT_TRUE(rax.is_gp());
  EXPECT_TRUE(rax.is_gp(0));
  EXPECT_TRUE(rax.is_gp64());
  EXPECT_FALSE(rax.is_gp64(1));
  EXPECT_EQ(rax.id(), 0u);
  EXPECT_EQ(rax.size(), 8u);
  EXPECT_EQ(rax.reg_type(), RegType::kGp64);
  EXPECT_EQ(rax.reg_group(), RegGroup::kGp);

  EXPECT_EQ(rax.r8(), al);
  EXPECT_EQ(rbx.r8_lo(), bl);
  EXPECT_EQ(rcx.r8_hi(), ch);
  EXPECT_EQ(rdx.r16(), dx);
  EXPECT_EQ(rsi.r32(), esi);
  EXPECT_EQ(rdi.r64(), rdi);
}

TEST_CASE(x86_operand_vec) {
  EXPECT_TRUE(Vec().is_reg());
  EXPECT_FALSE(Vec().is_valid());

  EXPECT_TRUE(xmm5.is_reg());
  EXPECT_TRUE(xmm5.is_valid());

  EXPECT_EQ(xmm(5), xmm5);
  EXPECT_EQ(ymm(5), ymm5);
  EXPECT_EQ(zmm(5), zmm5);

  EXPECT_TRUE(Vec::make_xmm(0).is_reg());
  EXPECT_TRUE(Vec::make_xmm(0).is_vec128());
  EXPECT_TRUE(xmm4.is_reg());
  EXPECT_TRUE(xmm4.is_xmm());
  EXPECT_TRUE(xmm4.is_vec());
  EXPECT_TRUE(xmm4.is_vec128());
  EXPECT_FALSE(xmm4.is_kreg());
  EXPECT_FALSE(xmm4.is_gp());
  EXPECT_FALSE(xmm4.is_reg_list());

  EXPECT_EQ(xmm4.id(), 4u);
  EXPECT_EQ(xmm4.size(), 16u);
  EXPECT_EQ(xmm4.reg_type(), RegType::kVec128);
  EXPECT_EQ(xmm4.reg_group(), RegGroup::kVec);

  EXPECT_TRUE(Vec::make_ymm(0).is_reg());
  EXPECT_TRUE(Vec::make_ymm(0).is_vec256());
  EXPECT_TRUE(ymm5.is_reg());
  EXPECT_TRUE(ymm5.is_ymm());
  EXPECT_TRUE(ymm5.is_vec());
  EXPECT_TRUE(ymm5.is_vec256());
  EXPECT_FALSE(ymm5.is_kreg());
  EXPECT_FALSE(ymm5.is_gp());
  EXPECT_FALSE(ymm5.is_reg_list());

  EXPECT_EQ(ymm5.id(), 5u);
  EXPECT_EQ(ymm5.size(), 32u);
  EXPECT_EQ(ymm5.reg_type(), RegType::kVec256);
  EXPECT_EQ(ymm5.reg_group(), RegGroup::kVec);

  EXPECT_TRUE(Vec::make_zmm(0).is_reg());
  EXPECT_TRUE(Vec::make_zmm(0).is_vec512());
  EXPECT_TRUE(zmm6.is_reg());
  EXPECT_TRUE(zmm6.is_zmm());
  EXPECT_TRUE(zmm6.is_vec());
  EXPECT_TRUE(zmm6.is_vec512());
  EXPECT_FALSE(zmm6.is_kreg());
  EXPECT_FALSE(zmm6.is_gp());
  EXPECT_FALSE(zmm6.is_reg_list());

  EXPECT_EQ(zmm6.id(), 6u);
  EXPECT_EQ(zmm6.size(), 64u);
  EXPECT_EQ(zmm6.reg_type(), RegType::kVec512);
  EXPECT_EQ(zmm6.reg_group(), RegGroup::kVec);

  // Converts a VEC register to a type of the passed register, but keeps the ID.
  EXPECT_EQ(xmm4.clone_as(ymm10), ymm4);
  EXPECT_EQ(xmm4.clone_as(zmm11), zmm4);
  EXPECT_EQ(ymm5.clone_as(xmm12), xmm5);
  EXPECT_EQ(ymm5.clone_as(zmm13), zmm5);
  EXPECT_EQ(zmm6.clone_as(xmm14), xmm6);
  EXPECT_EQ(zmm6.clone_as(ymm15), ymm6);

  EXPECT_EQ(xmm7.xmm(), xmm7);
  EXPECT_EQ(xmm7.ymm(), ymm7);
  EXPECT_EQ(xmm7.zmm(), zmm7);

  EXPECT_EQ(xmm7.v128(), xmm7);
  EXPECT_EQ(xmm7.v256(), ymm7);
  EXPECT_EQ(xmm7.v512(), zmm7);

  EXPECT_EQ(ymm7.xmm(), xmm7);
  EXPECT_EQ(ymm7.ymm(), ymm7);
  EXPECT_EQ(ymm7.zmm(), zmm7);

  EXPECT_EQ(ymm7.v128(), xmm7);
  EXPECT_EQ(ymm7.v256(), ymm7);
  EXPECT_EQ(ymm7.v512(), zmm7);

  EXPECT_EQ(zmm7.xmm(), xmm7);
  EXPECT_EQ(zmm7.ymm(), ymm7);
  EXPECT_EQ(zmm7.zmm(), zmm7);

  EXPECT_EQ(zmm7.v128(), xmm7);
  EXPECT_EQ(zmm7.v256(), ymm7);
  EXPECT_EQ(zmm7.v512(), zmm7);

  EXPECT_EQ(xmm4.half(), xmm4);
  EXPECT_EQ(ymm4.half(), xmm4);
  EXPECT_EQ(zmm4.half(), ymm4);

  EXPECT_EQ(Vec::make_xmm(3), Vec::make_v128(3));
  EXPECT_EQ(Vec::make_ymm(4), Vec::make_v256(4));
  EXPECT_EQ(Vec::make_zmm(5), Vec::make_v512(5));
}

TEST_CASE(x86_operand_mm) {
  EXPECT_TRUE(Mm().is_reg());
  EXPECT_FALSE(Mm().is_valid());

  EXPECT_TRUE(mm2.is_reg());
  EXPECT_TRUE(mm2.is_valid());
  EXPECT_TRUE(mm2.is_mm_reg());
  EXPECT_FALSE(mm2.is_kreg());
  EXPECT_FALSE(mm2.is_tmm_reg());
  EXPECT_FALSE(mm2.is_reg_list());

  EXPECT_EQ(mm2.id(), 2u);
  EXPECT_EQ(mm2.size(), 8u);
  EXPECT_EQ(mm2.reg_type(), RegType::kX86_Mm);
  EXPECT_EQ(mm2.reg_group(), RegGroup::kX86_MM);

  EXPECT_EQ(mm(5), mm5);
}

TEST_CASE(x86_operand_k) {
  EXPECT_TRUE(KReg().is_reg());
  EXPECT_FALSE(KReg().is_valid());

  EXPECT_TRUE(k3.is_reg());
  EXPECT_TRUE(k3.is_valid());
  EXPECT_TRUE(k3.is_kreg());
  EXPECT_FALSE(k3.is_gp());
  EXPECT_FALSE(k3.is_gp64());
  EXPECT_FALSE(k3.is_tmm_reg());
  EXPECT_FALSE(k3.is_reg_list());

  EXPECT_EQ(k3.id(), 3u);
  EXPECT_EQ(k3.size(), 0u);
  EXPECT_EQ(k3.reg_type(), RegType::kMask);
  EXPECT_EQ(k3.reg_group(), RegGroup::kMask);

  EXPECT_EQ(k(5), k5);
}

TEST_CASE(x86_operand_st) {
  EXPECT_TRUE(St().is_reg());
  EXPECT_FALSE(St().is_valid());

  EXPECT_TRUE(st1.is_reg());
  EXPECT_TRUE(st1.is_st_reg());
  EXPECT_FALSE(st1.is_gp());
  EXPECT_FALSE(st1.is_mm_reg());
  EXPECT_FALSE(st1.is_label());

  EXPECT_EQ(st1.id(), 1u);
  EXPECT_EQ(st1.size(), 10u);
  EXPECT_EQ(st1.reg_type(), RegType::kX86_St);
  EXPECT_EQ(st1.reg_group(), RegGroup::kX86_St);

  EXPECT_EQ(st(5) , st5);
}

TEST_CASE(x86_operand_tmm) {
  EXPECT_EQ(tmm1, tmm(1));
  EXPECT_TRUE(tmm1.is_tmm_reg());
  EXPECT_TRUE(tmm1.is_tmm_reg(1u));
  EXPECT_TRUE(tmm1.is_tile_reg());
  EXPECT_TRUE(tmm1.is_tile_reg(1u));
  EXPECT_FALSE(tmm1.is_gp());
}

TEST_CASE(x86_operand_other_regs) {
  EXPECT_TRUE(Reg().is_reg());
  EXPECT_FALSE(Reg().is_valid());

  EXPECT_TRUE(SReg().is_reg());
  EXPECT_FALSE(SReg().is_valid());

  EXPECT_TRUE(CReg().is_reg());
  EXPECT_FALSE(CReg().is_valid());

  EXPECT_TRUE(DReg().is_reg());
  EXPECT_FALSE(DReg().is_valid());

  EXPECT_TRUE(cr1.is_control_reg());
  EXPECT_FALSE(cr1.is_debug_reg());
  EXPECT_FALSE(cr1.is_gp());

  EXPECT_FALSE(dr2.is_control_reg());
  EXPECT_TRUE(dr2.is_debug_reg());
  EXPECT_FALSE(dr2.is_gp());

  EXPECT_EQ(cr(5) , cr5);
  EXPECT_EQ(dr(5) , dr5);

  EXPECT_FALSE(rip.is_none());
  EXPECT_FALSE(rip.is_gp());
  EXPECT_TRUE(rip.is_pc());
}

TEST_CASE(x86_operand_mem) {
  Label L(1000); // Label with some ID.

  Mem m(Globals::NoInit);
  m = Mem();
  EXPECT_EQ(m.clone(), Mem());

  m = ptr(L);
  EXPECT_TRUE(m.has_base());
  EXPECT_FALSE(m.has_base_reg());
  EXPECT_TRUE(m.has_base_label());
  EXPECT_FALSE(m.has_offset());
  EXPECT_FALSE(m.is_offset_64bit());
  EXPECT_EQ(m.offset(), 0);
  EXPECT_EQ(m.offset_lo32(), 0);
  EXPECT_FALSE(m.has_broadcast());

  m.set_index(xmm1, 2);
  EXPECT_EQ(m, ptr(L, xmm1, 2));

  m = ptr(0x0123456789ABCDEFu);
  EXPECT_FALSE(m.has_base());
  EXPECT_FALSE(m.has_base_reg());
  EXPECT_FALSE(m.has_index());
  EXPECT_FALSE(m.has_index_reg());
  EXPECT_TRUE(m.has_offset());
  EXPECT_TRUE(m.is_offset_64bit());
  EXPECT_EQ(m.offset(), int64_t(0x0123456789ABCDEFu));
  EXPECT_EQ(m.offset_lo32(), int32_t(0x89ABCDEFu));
  m.add_offset(1);
  EXPECT_EQ(m.offset(), int64_t(0x0123456789ABCDF0u));
  EXPECT_FALSE(m.has_broadcast());

  m = ptr(0x0123456789ABCDEFu, rdi, 3);
  EXPECT_FALSE(m.has_broadcast());
  EXPECT_FALSE(m.has_segment());
  EXPECT_FALSE(m.has_base());
  EXPECT_FALSE(m.has_base_reg());
  EXPECT_TRUE(m.has_index());
  EXPECT_TRUE(m.has_index_reg());
  EXPECT_EQ(m.index_type(), rdi.reg_type());
  EXPECT_EQ(m.index_id(), rdi.id());
  EXPECT_EQ(m.shift(), 3u);
  EXPECT_TRUE(m.has_offset());
  EXPECT_TRUE(m.is_offset_64bit());
  EXPECT_EQ(m.offset(), int64_t(0x0123456789ABCDEFu));
  EXPECT_EQ(m.offset_lo32(), int32_t(0x89ABCDEFu));
  m.reset_index();
  EXPECT_FALSE(m.has_index());
  EXPECT_FALSE(m.has_index_reg());
  EXPECT_FALSE(m.has_broadcast());

  m = ptr(rax);
  EXPECT_TRUE(m.has_base());
  EXPECT_TRUE(m.has_base_reg());
  EXPECT_EQ(m.base_type(), rax.reg_type());
  EXPECT_EQ(m.base_id(), rax.id());
  EXPECT_FALSE(m.has_index());
  EXPECT_FALSE(m.has_index_reg());
  EXPECT_EQ(m.index_type(), RegType::kNone);
  EXPECT_EQ(m.index_id(), 0u);
  EXPECT_FALSE(m.has_offset());
  EXPECT_FALSE(m.is_offset_64bit());
  EXPECT_EQ(m.offset(), 0);
  EXPECT_EQ(m.offset_lo32(), 0);
  m.set_index(rsi);
  EXPECT_TRUE(m.has_index());
  EXPECT_TRUE(m.has_index_reg());
  EXPECT_EQ(m.index_type(), rsi.reg_type());
  EXPECT_EQ(m.index_id(), rsi.id());

  m.set_segment(fs);
  EXPECT_TRUE(m.has_segment());
  EXPECT_EQ(m.segment(), fs);
  m.reset_segment();
  EXPECT_FALSE(m.has_segment());
  EXPECT_EQ(m.segment(), no_seg);

  m = ptr(rax)._1to2();
  EXPECT_TRUE(m.is_mem());
  EXPECT_TRUE(m.has_broadcast());
  EXPECT_EQ(m.get_broadcast(), Mem::Broadcast::k1To2);
  EXPECT_EQ(m._1to1(), ptr(rax));
  m.reset_broadcast();
  EXPECT_FALSE(m.has_broadcast());
  EXPECT_EQ(m.get_broadcast(), Mem::Broadcast::kNone);

  m.set_broadcast(Mem::Broadcast::k1To4);
  EXPECT_EQ(m, ptr(rax)._1to4());
  EXPECT_TRUE(m.has_broadcast());
  EXPECT_EQ(m.get_broadcast(), Mem::Broadcast::k1To4);

  m = ptr(rax)._1to8();
  EXPECT_TRUE(m.has_broadcast());
  EXPECT_EQ(m.get_broadcast(), Mem::Broadcast::k1To8);

  m = ptr(rax)._1to16();
  EXPECT_TRUE(m.has_broadcast());
  EXPECT_EQ(m.get_broadcast(), Mem::Broadcast::k1To16);

  m = ptr(rax)._1to32();
  EXPECT_TRUE(m.has_broadcast());
  EXPECT_EQ(m.get_broadcast(), Mem::Broadcast::k1To32);

  m = ptr(rax)._1to64();
  EXPECT_TRUE(m.has_broadcast());
  EXPECT_EQ(m.get_broadcast(), Mem::Broadcast::k1To64);

  m = ptr(rax);
  EXPECT_FALSE(m.has_size());
  EXPECT_EQ(m.clone_resized(4), dword_ptr(rax));
  EXPECT_EQ(m.clone_adjusted(4), ptr(rax, 4));

  m = byte_ptr(rax);
  EXPECT_TRUE(m.has_size());
  EXPECT_EQ(m.size(), 1u);
  EXPECT_TRUE(m.has_size(1u));

  m.set_size(4);
  EXPECT_TRUE(m.has_size());
  EXPECT_EQ(m.size(), 4u);
  EXPECT_TRUE(m.has_size(4u));

  m = ptr(rsi, rdi);
  EXPECT_EQ(m.base_reg(), rsi);
  EXPECT_EQ(m.index_reg(), rdi);
  EXPECT_FALSE(m.has_offset());
  EXPECT_FALSE(m.has_shift());
  EXPECT_EQ(m.shift(), 0u);

  m.set_shift(2);
  EXPECT_TRUE(m.has_shift());
  EXPECT_EQ(m.shift(), 2u);
  m.reset_shift();
  EXPECT_FALSE(m.has_shift());
  EXPECT_EQ(m.shift(), 0u);

  m = ptr_rel(0x10000u);
  EXPECT_FALSE(m.has_base());
  EXPECT_FALSE(m.has_index());
  EXPECT_TRUE(m.has_offset());
  EXPECT_TRUE(m.is_addr_rel());
  EXPECT_FALSE(m.is_addr_abs());

  m = ptr_abs(0x10000u);
  EXPECT_FALSE(m.has_base());
  EXPECT_FALSE(m.has_index());
  EXPECT_TRUE(m.has_offset());
  EXPECT_FALSE(m.is_addr_rel());
  EXPECT_TRUE(m.is_addr_abs());

  m.set_addr_rel();
  EXPECT_EQ(m, ptr_rel(0x10000u));
  m.set_addr_abs();
  EXPECT_EQ(m, ptr_abs(0x10000u));
  m.reset_addr_type();
  EXPECT_EQ(m, ptr(0x10000u));

#define ASMJIT_TEST_X86_MEM_OP(MAKE_FN, SIZE)                                    \
  EXPECT_EQ(MAKE_FN(L, 16)                  , ptr(L, 16, SIZE));                 \
  EXPECT_EQ(MAKE_FN(L, rsi, 2, 16)          , ptr(L, rsi, 2, 16, SIZE));         \
  EXPECT_EQ(MAKE_FN(rsi, rdi, 2, 16)        , ptr(rsi, rdi, 2, 16, SIZE));       \
  EXPECT_EQ(MAKE_FN(rsi, xmm1, 2, 16)       , ptr(rsi, xmm1, 2, 16, SIZE));      \
  EXPECT_EQ(MAKE_FN(rip, 16)                , ptr(rip, 16, SIZE));               \
  EXPECT_EQ(MAKE_FN(0x10000u)               , ptr(0x10000u, SIZE));              \
  EXPECT_EQ(MAKE_FN(0x10000u, rdi, 2)       , ptr(0x10000u, rdi, 2, SIZE));      \
  EXPECT_EQ(MAKE_FN(0x10000u, xmm1, 2)      , ptr(0x10000u, xmm1, 2, SIZE));     \
  EXPECT_EQ(MAKE_FN##_abs(0x10000u)         , ptr_abs(0x10000u, SIZE));          \
  EXPECT_EQ(MAKE_FN##_abs(0x10000u, rdi, 2) , ptr_abs(0x10000u, rdi, 2, SIZE));  \
  EXPECT_EQ(MAKE_FN##_abs(0x10000u, xmm1, 2), ptr_abs(0x10000u, xmm1, 2, SIZE)); \
  EXPECT_EQ(MAKE_FN##_rel(0x10000u)         , ptr_rel(0x10000u, SIZE));          \
  EXPECT_EQ(MAKE_FN##_rel(0x10000u, rdi, 2) , ptr_rel(0x10000u, rdi, 2, SIZE));  \
  EXPECT_EQ(MAKE_FN##_rel(0x10000u, xmm1, 2), ptr_rel(0x10000u, xmm1, 2, SIZE))

  ASMJIT_TEST_X86_MEM_OP(byte_ptr, 1);
  ASMJIT_TEST_X86_MEM_OP(word_ptr, 2);
  ASMJIT_TEST_X86_MEM_OP(dword_ptr, 4);
  ASMJIT_TEST_X86_MEM_OP(qword_ptr, 8);
  ASMJIT_TEST_X86_MEM_OP(tbyte_ptr, 10);
  ASMJIT_TEST_X86_MEM_OP(tword_ptr, 10);
  ASMJIT_TEST_X86_MEM_OP(oword_ptr, 16);
  ASMJIT_TEST_X86_MEM_OP(dqword_ptr, 16);
  ASMJIT_TEST_X86_MEM_OP(qqword_ptr, 32);
  ASMJIT_TEST_X86_MEM_OP(xmmword_ptr, 16);
  ASMJIT_TEST_X86_MEM_OP(ymmword_ptr, 32);
  ASMJIT_TEST_X86_MEM_OP(zmmword_ptr, 64);

  ASMJIT_TEST_X86_MEM_OP(ptr_8, 1);
  ASMJIT_TEST_X86_MEM_OP(ptr_16, 2);
  ASMJIT_TEST_X86_MEM_OP(ptr_32, 4);
  ASMJIT_TEST_X86_MEM_OP(ptr_48, 6);
  ASMJIT_TEST_X86_MEM_OP(ptr_64, 8);
  ASMJIT_TEST_X86_MEM_OP(ptr_80, 10);
  ASMJIT_TEST_X86_MEM_OP(ptr_128, 16);
  ASMJIT_TEST_X86_MEM_OP(ptr_256, 32);
  ASMJIT_TEST_X86_MEM_OP(ptr_512, 64);

#undef ASMJIT_TEST_X86_MEM_OP
}

ASMJIT_END_SUB_NAMESPACE

#endif // ASMJIT_TEST && !ASMJIT_NO_X86
