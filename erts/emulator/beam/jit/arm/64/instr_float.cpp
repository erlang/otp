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
    Label double_max = a.newLabel(), error = a.newLabel();

    a.fabs(a64::d30, a64::d0);
    a.ldr(a64::d31, arm::Mem(double_max));
    a.fcmp(a64::d30, a64::d31);
    a.b_hi(error);
    a.ret(a64::x30);

    a.align(AlignMode::kCode, 8);
    a.bind(double_max);
    a.embedUInt64(0x7FEFFFFFFFFFFFFFul);

    a.bind(error);
    {
        mov_imm(ARG4, 0);
        mov_imm(TMP1, EXC_BADARITH);
        a.str(TMP1, arm::Mem(c_p, offsetof(Process, freason)));
        a.b(labels[raise_exception]);
    }
}

void BeamModuleAssembler::emit_float_instr(uint32_t instId,
                                           const ArgFRegister &LHS,
                                           const ArgFRegister &RHS,
                                           const ArgFRegister &Dst) {
    auto lhs = load_source(LHS, a64::d0);
    auto rhs = load_source(RHS, a64::d1);
    auto dst = init_destination(Dst, a64::d2);

    a.emit(instId, a64::d0, lhs.reg, rhs.reg);
    fragment_call(ga->get_check_float_error());
    a.mov(dst.reg, a64::d0);
    flush_var(dst);
}

/* * * * */

void BeamModuleAssembler::emit_fload(const ArgSource &Src,
                                     const ArgFRegister &Dst) {
    auto src = load_source(Src, TMP1);
    auto dst = init_destination(Dst, a64::d0);
    a64::Gp float_ptr = emit_ptr_val(TMP1, src.reg);

    a.ldur(dst.reg, emit_boxed_val(float_ptr, sizeof(Eterm)));
    flush_var(dst);
}

void BeamModuleAssembler::emit_fstore(const ArgFRegister &Src,
                                      const ArgRegister &Dst) {
    auto src = load_source(Src, a64::d0);
    auto dst = init_destination(Dst, TMP1);

    a.add(dst.reg, HTOP, imm(TAG_PRIMARY_BOXED));

    mov_imm(TMP2, HEADER_FLONUM);
    a.str(TMP2, arm::Mem(HTOP).post(sizeof(Eterm)));
    a.str(src.reg, arm::Mem(HTOP).post(sizeof(Eterm)));

    flush_var(dst);
}

/* ARG1 = source term */
void BeamGlobalAssembler::emit_fconv_shared() {
    Label error = a.newLabel();

    /* Is the source a bignum? */
    {
        emit_is_boxed(error, ARG1);

        emit_untag_ptr(TMP1, ARG1);
        a.ldr(TMP1, arm::Mem(TMP1));

        /* The mask (0b111011) cannot be encoded as an immediate operand for
         * 'and'. */
        mov_imm(TMP2, _TAG_HEADER_MASK - _BIG_SIGN_BIT);
        a.and_(TMP2, TMP1, TMP2);
        a.cmp(TMP2, imm(_TAG_HEADER_POS_BIG));
        a.b_ne(error);
    }

    emit_enter_runtime_frame();
    emit_enter_runtime();

    /* ARG1 already contains the source term. */
    lea(ARG2, TMP_MEM1q);
    runtime_call<2>(big_to_double);

    emit_leave_runtime();
    emit_leave_runtime_frame();

    a.tbnz(ARG1.w(), imm(31), error);

    a.ldr(a64::d0, TMP_MEM1q);
    a.ret(a64::x30);

    a.bind(error);
    {
        mov_imm(ARG4, 0);
        mov_imm(TMP1, EXC_BADARITH);
        a.str(TMP1, arm::Mem(c_p, offsetof(Process, freason)));
        a.b(labels[raise_exception]);
    }
}

void BeamModuleAssembler::emit_fconv(const ArgSource &Src,
                                     const ArgFRegister &Dst) {
    auto dst = init_destination(Dst, a64::d0);
    auto src = load_source(Src, ARG1);

    if (always_small(Src)) {
        comment("skipped test for small operand since it is always small");
        a.asr(TMP1, src.reg, imm(_TAG_IMMED1_SIZE));
        a.scvtf(dst.reg, TMP1);
        flush_var(dst);
        return;
    }

    Label next = a.newLabel(), not_small = a.newLabel(),
          fallback = a.newLabel();

    a.and_(TMP1, src.reg, imm(_TAG_IMMED1_MASK));
    a.cmp(TMP1, imm(_TAG_IMMED1_MASK));
    a.b_ne(not_small);

    a.asr(TMP1, src.reg, imm(_TAG_IMMED1_SIZE));
    a.scvtf(dst.reg, TMP1);
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

            emit_untag_ptr(TMP1, src.reg);

            /* Speculatively load the float value, this is safe since all boxed
             * terms are at least two words long. */
            a.ldr(dst.reg, arm::Mem(TMP1, sizeof(Eterm)));

            a.ldr(TMP1, arm::Mem(TMP1));
            a.cmp(TMP1, imm(HEADER_FLONUM));
            a.b_eq(next);
        }

        a.bind(fallback);
        {
            mov_var(ARG1, src);
            fragment_call(ga->get_fconv_shared());
            mov_var(dst, a64::d0);
        }
    }

    a.bind(next);
    flush_var(dst);
}

void BeamModuleAssembler::emit_i_fadd(const ArgFRegister &LHS,
                                      const ArgFRegister &RHS,
                                      const ArgFRegister &Dst) {
    emit_float_instr(a64::Inst::kIdFadd_v, LHS, RHS, Dst);
}

void BeamModuleAssembler::emit_i_fsub(const ArgFRegister &LHS,
                                      const ArgFRegister &RHS,
                                      const ArgFRegister &Dst) {
    emit_float_instr(a64::Inst::kIdFsub_v, LHS, RHS, Dst);
}

void BeamModuleAssembler::emit_i_fmul(const ArgFRegister &LHS,
                                      const ArgFRegister &RHS,
                                      const ArgFRegister &Dst) {
    emit_float_instr(a64::Inst::kIdFmul_v, LHS, RHS, Dst);
}

void BeamModuleAssembler::emit_i_fdiv(const ArgFRegister &LHS,
                                      const ArgFRegister &RHS,
                                      const ArgFRegister &Dst) {
    emit_float_instr(a64::Inst::kIdFdiv_v, LHS, RHS, Dst);
}

void BeamModuleAssembler::emit_i_fnegate(const ArgFRegister &Src,
                                         const ArgFRegister &Dst) {
    auto src = load_source(Src, a64::d0);
    auto dst = init_destination(Dst, a64::d1);

    /* Note that there is no need to check for errors since flipping the sign
     * of a finite float is guaranteed to produce a finite float. */
    a.fneg(a64::d0, src.reg);
    a.mov(dst.reg, a64::d0);
    flush_var(dst);
}
