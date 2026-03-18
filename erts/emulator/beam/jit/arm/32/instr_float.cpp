/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2020-2023. All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * %CopyrightEnd%
 */

#include "beam_asm.hpp"

extern "C"
{
#include "big.h"
}

/* Checks whether d0 contains a finite value.
 *
 * Clobbers d30 and d31. */
void BeamGlobalAssembler::emit_check_float_error() {
    Label error = a.newLabel();

    /* ARM64 uses FCMP against DBL_MAX. On ARM32 we keep the same semantics
     * (error on non-finite values) by checking the exponent field after fabs:
     * exponent == 0x7ff means NaN or infinity. */
    a.vabs_f64(a32::d30, a32::d0);
    a.vmov(TMP, VAR, a32::d30);
    mov_imm(ARG1, 0x7FF00000u);
    a.and_(VAR, VAR, ARG1);
    a.cmp(VAR, ARG1);
    a.b_eq(error);
    a.bx(a32::lr);

    a.bind(error);
    {
        mov_imm(ARG4, 0);
        mov_imm(TMP, EXC_BADARITH);
        a.str(TMP, arm::Mem(c_p, offsetof(Process, freason)));
        a.b(labels[raise_exception]);
    }
}

void BeamModuleAssembler::emit_float_instr(uint32_t instId,
                                           const ArgFRegister &LHS,
                                           const ArgFRegister &RHS,
                                           const ArgFRegister &Dst) {
    auto lhs = load_source(LHS, a32::d0);
    auto rhs = load_source(RHS, a32::d1);
    auto dst = init_destination(Dst, a32::d2);

    switch (instId) {
    case a32::Inst::kIdVadd:
        a.vadd_f64(a32::d0, lhs.reg, rhs.reg);
        break;
    case a32::Inst::kIdVsub:
        a.vsub_f64(a32::d0, lhs.reg, rhs.reg);
        break;
    case a32::Inst::kIdVmul:
        a.vmul_f64(a32::d0, lhs.reg, rhs.reg);
        break;
    case a32::Inst::kIdVdiv:
        a.vdiv_f64(a32::d0, lhs.reg, rhs.reg);
        break;
    default:
        emit_nyi("emit_float_instr(instId)");
        return;
    }

    fragment_call(ga->get_check_float_error());
    a.vmov_f64(dst.reg, a32::d0);
    flush_var(dst);
}

/* * * * */

void BeamModuleAssembler::emit_fload(const ArgSource &Src,
                                     const ArgFRegister &Dst) {
    auto src = load_source(Src, TMP);
    auto dst = init_destination(Dst, a32::d0);
    a32::Gp float_ptr = emit_ptr_val(TMP, src.reg);

    lea(TMP, emit_boxed_val(float_ptr, sizeof(Eterm)));
    a.vldr_64(dst.reg, arm::Mem(TMP));
    flush_var(dst);
}

void BeamModuleAssembler::emit_fstore(const ArgFRegister &Src,
                                      const ArgRegister &Dst) {
    auto src = load_source(Src, a32::d0);
    auto dst = init_destination(Dst, VAR);

    a.add(dst.reg, HTOP, imm(TAG_PRIMARY_BOXED));

    mov_imm(TMP, HEADER_FLONUM);
    a.str(TMP, arm::Mem(HTOP).post(sizeof(Eterm)));

    a.vstr_64(src.reg, arm::Mem(HTOP));
    ERTS_CT_ASSERT(sizeof(Eterm) == 4);
    a.add(HTOP, HTOP, imm(2 * sizeof(Eterm)));

    flush_var(dst);
}

/* ARG1 = source term */
void BeamGlobalAssembler::emit_fconv_shared() {
    Label error = a.newLabel();

    /* Is the source a bignum? */
    {
        emit_is_boxed(error, ARG1);

        emit_untag_ptr(TMP, ARG1);
        a.ldr(TMP, arm::Mem(TMP));

        /* The mask (0b111011) cannot be encoded directly on ARM32. */
        mov_imm(VAR, _TAG_HEADER_MASK - _BIG_SIGN_BIT);
        a.and_(VAR, TMP, VAR);
        a.cmp(VAR, imm(_TAG_HEADER_POS_BIG));
        a.b_ne(error);
    }

    emit_enter_runtime_frame();
    emit_enter_runtime();

    /* ARG1 already contains the source term. */
    lea(ARG2, TMP_MEM1q);
    runtime_call<2>(big_to_double);

    emit_leave_runtime();
    emit_leave_runtime_frame();

    a.tst(ARG1, ARG1);
    a.b_mi(error);

    lea(TMP, TMP_MEM1q);
    a.vldr_64(a32::d0, arm::Mem(TMP));
    a.bx(a32::lr);

    a.bind(error);
    {
        mov_imm(ARG4, 0);
        mov_imm(TMP, EXC_BADARITH);
        a.str(TMP, arm::Mem(c_p, offsetof(Process, freason)));
        a.b(labels[raise_exception]);
    }
}

void BeamModuleAssembler::emit_fconv(const ArgSource &Src,
                                     const ArgFRegister &Dst) {
    auto dst = init_destination(Dst, a32::d0);
    auto src = load_source(Src, ARG1);

    if (always_small(Src)) {
        comment("skipped test for small operand since it is always small");
        a.asr(TMP, src.reg, imm(_TAG_IMMED1_SIZE));
        a.vmov_s32(a32::s0, TMP);
        a.vcvt_f64_s32(dst.reg, a32::s0);
        flush_var(dst);
        return;
    }

    Label next = a.newLabel(),
          not_small = a.newLabel(),
          fallback = a.newLabel();

    a.and_(TMP, src.reg, imm(_TAG_IMMED1_MASK));
    a.cmp(TMP, imm(_TAG_IMMED1_SMALL));
    a.b_ne(not_small);

    a.asr(TMP, src.reg, imm(_TAG_IMMED1_SIZE));
    a.vmov_s32(a32::s0, TMP);
    a.vcvt_f64_s32(dst.reg, a32::s0);
    a.b(next);

    a.bind(not_small);
    {
        if (never_one_of<BeamTypeId::Float>(Src)) {
            comment("skipped float path since source cannot be a float");
        } else {
            /* If the source is always a number, we can skip the box test when
             * it's not a small. */
            if (always_one_of<BeamTypeId::Number>(Src)) {
                comment("skipped box test since source is always a number");
            } else {
                emit_is_boxed(fallback, Src, src.reg);
            }

            emit_untag_ptr(TMP, src.reg);

            /* Speculatively load the float value, this is safe since all boxed
             * terms are at least two words long. */
            lea(VAR, arm::Mem(TMP, sizeof(Eterm)));
            a.vldr_64(dst.reg, arm::Mem(VAR));

            a.ldr(TMP, arm::Mem(TMP));
            mov_imm(VAR, HEADER_FLONUM);
            a.cmp(TMP, VAR);
            a.b_eq(next);
        }

        a.bind(fallback);
        {
            mov_var(ARG1, src);
            fragment_call(ga->get_fconv_shared());
            a.vmov_f64(dst.reg, a32::d0);
        }
    }

    a.bind(next);
    flush_var(dst);
}

void BeamModuleAssembler::emit_i_fadd(const ArgFRegister &LHS,
                                      const ArgFRegister &RHS,
                                      const ArgFRegister &Dst) {
    emit_float_instr(a32::Inst::kIdVadd, LHS, RHS, Dst);
}

void BeamModuleAssembler::emit_i_fsub(const ArgFRegister &LHS,
                                      const ArgFRegister &RHS,
                                      const ArgFRegister &Dst) {
    emit_float_instr(a32::Inst::kIdVsub, LHS, RHS, Dst);
}

void BeamModuleAssembler::emit_i_fmul(const ArgFRegister &LHS,
                                      const ArgFRegister &RHS,
                                      const ArgFRegister &Dst) {
    emit_float_instr(a32::Inst::kIdVmul, LHS, RHS, Dst);
}

void BeamModuleAssembler::emit_i_fdiv(const ArgFRegister &LHS,
                                      const ArgFRegister &RHS,
                                      const ArgFRegister &Dst) {
    emit_float_instr(a32::Inst::kIdVdiv, LHS, RHS, Dst);
}

void BeamModuleAssembler::emit_i_fnegate(const ArgFRegister &Src,
                                         const ArgFRegister &Dst) {
    auto src = load_source(Src, a32::d0);
    auto dst = init_destination(Dst, a32::d1);

    /* Note that there is no need to check for errors since flipping the sign
     * of a finite float is guaranteed to produce a finite float. */
    a.vneg_f64(a32::d0, src.reg);
    a.vmov_f64(dst.reg, a32::d0);
    flush_var(dst);
}
