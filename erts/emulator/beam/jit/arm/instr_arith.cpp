/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2020-2020. All Rights Reserved.
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
#include "erl_bif_table.h"
}

/*
 * ARG2 = LHS
 * ARG3 = RHS
 *
 * The module code must have executed emit_enter_runtime()
 * before calling this function.
 *
 * The result is returned in ARG1.
 */
void BeamGlobalAssembler::emit_plus_body_shared() {
    static const ErtsCodeMFA bif_mfa = {am_erlang, am_Plus, 2};

    Label error = a.newLabel();

    /* Save original arguments for the error path. */
    a.stp(ARG2, ARG3, TMP_MEM1q);

    emit_enter_runtime_frame();

    a.mov(ARG1, c_p);
    runtime_call<3>(erts_mixed_plus);

    emit_leave_runtime_frame();

    emit_branch_if_not_value(ARG1, error);

    a.ret(a64::x30);

    a.bind(error);
    {
        /* emit_enter_runtime() was done in the module code. */
        emit_leave_runtime(0);

        /* Place the original arguments in X registers. */
        a.ldp(XREG0, XREG1, TMP_MEM1q);
        mov_imm(ARG4, &bif_mfa);
        a.b(labels[raise_exception]);
    }
}

void BeamModuleAssembler::emit_i_plus(const ArgVal &Fail,
                                      const ArgVal &Live,
                                      const ArgVal &LHS,
                                      const ArgVal &RHS,
                                      const ArgVal &Dst) {
    Label next = a.newLabel();

    auto [lhs, rhs] = load_sources(LHS, ARG2, RHS, ARG3);
    bool rhs_is_small = RHS.isImmed() && is_small(RHS.getValue());

    if (rhs_is_small && Support::isUInt12(RHS.getValue())) {
        comment("add small constant with overflow check");
        Uint cleared_tag = RHS.getValue() & ~_TAG_IMMED1_MASK;
        a.adds(ARG1, lhs.reg, imm(cleared_tag));
    } else {
        comment("addition with overflow check");
        a.and_(TMP1, rhs.reg, imm(~_TAG_IMMED1_MASK));
        a.adds(ARG1, lhs.reg, TMP1);
    }

    if (rhs_is_small) {
        a.and_(TMP1, lhs.reg, imm(_TAG_IMMED1_MASK));
    } else if (LHS.isImmed() && is_small(LHS.getValue())) {
        a.and_(TMP1, rhs.reg, imm(_TAG_IMMED1_MASK));
    } else {
        ERTS_CT_ASSERT(_TAG_IMMED1_SMALL == _TAG_IMMED1_MASK);
        a.and_(TMP1, lhs.reg, rhs.reg);
        a.and_(TMP1, TMP1, imm(_TAG_IMMED1_MASK));
    }

    /* Test for not overflow AND small operands. */
    a.ccmp(TMP1, imm(_TAG_IMMED1_SMALL), 0, arm::Cond::kVC);
    a.cond_eq().b(next);

    mov_var(ARG2, lhs);
    mov_var(ARG3, rhs);

    if (Fail.getValue() != 0) {
        emit_enter_runtime(Live.getValue());
        a.mov(ARG1, c_p);
        runtime_call<3>(erts_mixed_plus);
        emit_leave_runtime(Live.getValue());
        emit_branch_if_not_value(ARG1, resolve_beam_label(Fail, dispUnknown));
    } else {
        emit_enter_runtime(Live.getValue());
        fragment_call(ga->get_plus_body_shared());
        emit_leave_runtime(Live.getValue());
    }

    a.bind(next);
    mov_arg(Dst, ARG1);
}

/*
 * ARG2 = Src
 *
 * The module code must have executed emit_enter_runtime()
 * before calling this function.
 *
 * The result is returned in ARG1.
 */
void BeamGlobalAssembler::emit_unary_minus_body_shared() {
    Label error = a.newLabel();

    /* Save original argument for the error path. */
    a.str(ARG2, TMP_MEM1q);

    emit_enter_runtime_frame();

    a.mov(ARG1, c_p);
    runtime_call<2>(erts_unary_minus);

    emit_leave_runtime_frame();

    emit_branch_if_not_value(ARG1, error);

    a.ret(a64::x30);

    a.bind(error);
    {
        static const ErtsCodeMFA bif_mfa = {am_erlang, am_Minus, 1};

        /* emit_enter_runtime() was done in the module code. */
        emit_leave_runtime(0);

        /* Place the original argument in an X registers. */
        a.ldr(XREG0, TMP_MEM1q);
        mov_imm(ARG4, &bif_mfa);
        a.b(labels[raise_exception]);
    }
}

void BeamModuleAssembler::emit_i_unary_minus(const ArgVal &Fail,
                                             const ArgVal &Live,
                                             const ArgVal &Src,
                                             const ArgVal &Dst) {
    Label next = a.newLabel();
    auto src = load_source(Src, ARG2);

    a.mov(TMP1, imm(_TAG_IMMED1_SMALL));
    a.and_(TMP2, src.reg, imm(~_TAG_IMMED1_MASK));
    a.subs(ARG1, TMP1, TMP2);

    /* Test for not overflow AND small operands. */
    a.ccmp(TMP2, imm(_TAG_IMMED1_SMALL), 0, arm::Cond::kVC);
    a.cond_eq().b(next);

    mov_var(ARG2, src);
    if (Fail.getValue() != 0) {
        emit_enter_runtime(Live.getValue());
        a.mov(ARG1, c_p);
        runtime_call<2>(erts_unary_minus);
        emit_leave_runtime(Live.getValue());
        emit_branch_if_not_value(ARG1, resolve_beam_label(Fail, dispUnknown));
    } else {
        emit_enter_runtime(Live.getValue());
        fragment_call(ga->get_unary_minus_body_shared());
        emit_leave_runtime(Live.getValue());
    }

    a.bind(next);
    mov_arg(Dst, ARG1);
}

/*
 * ARG2 = LHS
 * ARG3 = RHS
 *
 * The module code must have executed emit_enter_runtime()
 * before calling this function.
 *
 * The result is returned in ARG1.
 */
void BeamGlobalAssembler::emit_minus_body_shared() {
    static const ErtsCodeMFA bif_mfa = {am_erlang, am_Minus, 2};

    Label error = a.newLabel();

    /* Save original arguments for the error path. */
    a.stp(ARG2, ARG3, TMP_MEM1q);

    emit_enter_runtime_frame();

    a.mov(ARG1, c_p);
    runtime_call<3>(erts_mixed_minus);

    emit_leave_runtime_frame();

    emit_branch_if_not_value(ARG1, error);

    a.ret(a64::x30);

    a.bind(error);
    {
        /* emit_enter_runtime() was done in the module code. */
        emit_leave_runtime(0);

        /* Place the original arguments in X registers. */
        a.ldp(XREG0, XREG1, TMP_MEM1q);
        mov_imm(ARG4, &bif_mfa);
        a.b(labels[raise_exception]);
    }
}

void BeamModuleAssembler::emit_i_minus(const ArgVal &Fail,
                                       const ArgVal &Live,
                                       const ArgVal &LHS,
                                       const ArgVal &RHS,
                                       const ArgVal &Dst) {
    Label next = a.newLabel();

    auto [lhs, rhs] = load_sources(LHS, ARG2, RHS, ARG3);
    bool rhs_is_small = RHS.isImmed() && is_small(RHS.getValue());

    if (rhs_is_small && Support::isUInt12(RHS.getValue())) {
        comment("subtract small constant with overflow check");
        Uint cleared_tag = RHS.getValue() & ~_TAG_IMMED1_MASK;
        a.subs(ARG1, lhs.reg, imm(cleared_tag));
    } else {
        comment("subtraction with overflow check");
        a.and_(TMP1, rhs.reg, imm(~_TAG_IMMED1_MASK));
        a.subs(ARG1, lhs.reg, TMP1);
    }

    if (rhs_is_small) {
        a.and_(TMP1, lhs.reg, imm(_TAG_IMMED1_MASK));
    } else if (LHS.isImmed() && is_small(LHS.getValue())) {
        a.and_(TMP1, rhs.reg, imm(_TAG_IMMED1_MASK));
    } else {
        ERTS_CT_ASSERT(_TAG_IMMED1_SMALL == _TAG_IMMED1_MASK);
        a.and_(TMP1, lhs.reg, rhs.reg);
        a.and_(TMP1, TMP1, imm(_TAG_IMMED1_MASK));
    }

    /* Test for not overflow AND small operands. */
    a.ccmp(TMP1, imm(_TAG_IMMED1_SMALL), 0, arm::Cond::kVC);
    a.cond_eq().b(next);

    mov_var(ARG2, lhs);
    mov_var(ARG3, rhs);

    if (Fail.getValue() != 0) {
        emit_enter_runtime(Live.getValue());
        a.mov(ARG1, c_p);
        runtime_call<3>(erts_mixed_minus);
        emit_leave_runtime(Live.getValue());
        emit_branch_if_not_value(ARG1, resolve_beam_label(Fail, dispUnknown));
    } else {
        emit_enter_runtime(Live.getValue());
        fragment_call(ga->get_minus_body_shared());
        emit_leave_runtime(Live.getValue());
    }

    a.bind(next);
    mov_arg(Dst, ARG1);
}

/* ARG2 = LHS
 * ARG3 = RHS
 *
 * The result is returned in ARG1 (set to THE_NON_VALUE if
 * the call failed).
 */
void BeamGlobalAssembler::emit_times_guard_shared() {
    Label generic = a.newLabel();

    /* Speculatively untag and multiply. */
    a.and_(TMP1, ARG2, imm(~_TAG_IMMED1_MASK));
    a.asr(TMP2, ARG3, imm(_TAG_IMMED1_SIZE));
    a.mul(TMP3, TMP1, TMP2);
    a.smulh(TMP4, TMP1, TMP2);

    /* Check that both operands are small integers. */
    ERTS_CT_ASSERT(_TAG_IMMED1_SMALL == _TAG_IMMED1_MASK);
    a.and_(TMP1, ARG2, ARG3);
    a.and_(TMP1, TMP1, imm(_TAG_IMMED1_MASK));
    a.cmp(TMP1, imm(_TAG_IMMED1_SMALL));
    a.cond_ne().b(generic);

    /* The high 65 bits of result will all be the same if no overflow
     * occurred. Another way to say that is that the sign bit of the
     * low 64 bits repeated 64 times must be equal to the high 64 bits
     * of the product. */
    a.cmp(TMP4, TMP3, arm::asr(63));
    a.cond_ne().b(generic);

    a.orr(ARG1, TMP3, imm(_TAG_IMMED1_SMALL));
    a.ret(a64::x30);

    a.bind(generic);

    emit_enter_runtime_frame();
    emit_enter_runtime();

    a.mov(ARG1, c_p);
    runtime_call<3>(erts_mixed_times);

    emit_leave_runtime();
    emit_leave_runtime_frame();

    a.ret(a64::x30);
}

/* ARG2 = LHS
 * ARG3 = RHS
 *
 * The result is returned in ARG1.
 */
void BeamGlobalAssembler::emit_times_body_shared() {
    Label generic = a.newLabel(), error = a.newLabel();

    /* Speculatively untag and multiply. */
    a.and_(TMP1, ARG2, imm(~_TAG_IMMED1_MASK));
    a.asr(TMP2, ARG3, imm(_TAG_IMMED1_SIZE));
    a.mul(TMP3, TMP1, TMP2);
    a.smulh(TMP4, TMP1, TMP2);

    /* Check that both operands are integers. */
    ERTS_CT_ASSERT(_TAG_IMMED1_SMALL == _TAG_IMMED1_MASK);
    a.and_(TMP1, ARG2, ARG3);
    a.and_(TMP1, TMP1, imm(_TAG_IMMED1_MASK));
    a.cmp(TMP1, imm(_TAG_IMMED1_SMALL));
    a.cond_ne().b(generic);

    /* The high 65 bits of result will all be the same if no overflow
     * occurred. Another way to say that is that the sign bit of the
     * low 64 bits repeated 64 times must be equal to the high 64 bits
     * of the product. */
    a.cmp(TMP4, TMP3, arm::asr(63));
    a.cond_ne().b(generic);

    a.orr(ARG1, TMP3, imm(_TAG_IMMED1_SMALL));
    a.ret(a64::x30);

    a.bind(generic);

    /* Save original arguments for the error path. */
    a.stp(ARG2, ARG3, TMP_MEM1q);

    emit_enter_runtime_frame();
    emit_enter_runtime();

    a.mov(ARG1, c_p);
    runtime_call<3>(erts_mixed_times);

    emit_leave_runtime();
    emit_leave_runtime_frame();

    emit_branch_if_not_value(ARG1, error);

    a.ret(a64::x30);

    a.bind(error);
    {
        static const ErtsCodeMFA bif_mfa = {am_erlang, am_Times, 2};

        /* Place the original arguments in x-registers. */
        a.ldp(XREG0, XREG1, TMP_MEM1q);
        mov_imm(ARG4, &bif_mfa);
        a.b(labels[raise_exception]);
    }
}

void BeamModuleAssembler::emit_i_times(const ArgVal &Fail,
                                       const ArgVal &Live,
                                       const ArgVal &LHS,
                                       const ArgVal &RHS,
                                       const ArgVal &Dst) {
    auto [lhs, rhs] = load_sources(LHS, ARG2, RHS, ARG3);
    mov_var(ARG2, lhs);
    mov_var(ARG3, rhs);

    if (Fail.getValue() != 0) {
        fragment_call(ga->get_times_guard_shared());
        emit_branch_if_not_value(ARG1, resolve_beam_label(Fail, dispUnknown));
    } else {
        fragment_call(ga->get_times_body_shared());
    }

    mov_arg(Dst, ARG1);
}

/*
 * ARG2 = LHS
 * ARG3 = RHS
 *
 * Quotient is returned in ARG1, remainder in ARG2.
 * Error is indicated by the Z flag.
 */
void BeamGlobalAssembler::emit_int_div_rem_guard_shared() {
    Label exit = a.newLabel(), generic = a.newLabel();

    /* Speculatively go ahead with the division. */
    a.asr(TMP1, ARG2, imm(_TAG_IMMED1_SIZE));
    a.asr(TMP2, ARG3, imm(_TAG_IMMED1_SIZE));
    a.sdiv(TMP3, TMP1, TMP2);
    a.msub(TMP4, TMP3, TMP2, TMP1);

    a.cmp(ARG3, imm(make_small(0)));
    a.cond_eq().b(exit);

    /* Check whether both operands are small integers. */
    ERTS_CT_ASSERT(_TAG_IMMED1_SMALL == _TAG_IMMED1_MASK);
    a.and_(TMP1, ARG2, ARG3);
    a.and_(TMP1, TMP1, imm(_TAG_IMMED1_MASK));
    a.cmp(TMP1, imm(_TAG_IMMED1_SMALL));
    a.cond_ne().b(generic);

    /* MIN_SMALL divided by -1 will overflow, and we'll need to fall
     * back to the generic handler in that case. */
    a.asr(TMP1, TMP3, imm(SMALL_BITS - 1));
    a.cmp(TMP1, imm(1));
    a.cond_ge().b(generic);

    /* The Z flag is now clear (meaning no error). */

    mov_imm(TMP1, _TAG_IMMED1_SMALL);
    arm::Shift tagShift = arm::lsl(_TAG_IMMED1_SIZE);
    a.orr(ARG1, TMP1, TMP3, tagShift);
    a.orr(ARG2, TMP1, TMP4, tagShift);

    a.bind(exit);
    { a.ret(a64::x30); }

    a.bind(generic);
    {
        emit_enter_runtime_frame();
        emit_enter_runtime();

        a.mov(ARG1, c_p);
        lea(ARG4, TMP_MEM4q);
        lea(ARG5, TMP_MEM5q);
        runtime_call<5>(erts_int_div_rem);

        emit_leave_runtime();
        emit_leave_runtime_frame();

        a.tst(ARG1, ARG1);
        a.ldp(ARG1, ARG2, TMP_MEM4q);

        a.ret(a64::x30);
    }
}

/* ARG2 = LHS
 * ARG3 = RHS
 * ARG4 = error MFA
 *
 * Quotient is returned in ARG1, remainder in ARG2.
 */
void BeamGlobalAssembler::emit_int_div_rem_body_shared() {
    Label div_zero = a.newLabel(), generic_div = a.newLabel(),
          generic_error = a.newLabel();

    /* Speculatively go ahead with the division. */
    a.asr(TMP1, ARG2, imm(_TAG_IMMED1_SIZE));
    a.asr(TMP2, ARG3, imm(_TAG_IMMED1_SIZE));
    a.sdiv(TMP3, TMP1, TMP2);
    a.msub(TMP4, TMP3, TMP2, TMP1);

    a.cmp(ARG3, imm(make_small(0)));
    a.cond_eq().b(div_zero);

    /* Check whether both operands are small integers. */
    ERTS_CT_ASSERT(_TAG_IMMED1_SMALL == _TAG_IMMED1_MASK);
    a.and_(TMP1, ARG2, ARG3);
    a.and_(TMP1, TMP1, imm(_TAG_IMMED1_MASK));
    a.cmp(TMP1, imm(_TAG_IMMED1_SMALL));
    a.cond_ne().b(generic_div);

    /* MIN_SMALL divided by -1 will overflow, and we'll need to fall
     * back to the generic handler in that case. */
    a.asr(TMP1, TMP3, imm(SMALL_BITS - 1));
    a.cmp(TMP1, imm(1));
    a.cond_ge().b(generic_div);

    mov_imm(TMP1, _TAG_IMMED1_SMALL);
    arm::Shift tagShift = arm::lsl(_TAG_IMMED1_SIZE);
    a.orr(ARG1, TMP1, TMP3, tagShift);
    a.orr(ARG2, TMP1, TMP4, tagShift);

    a.ret(a64::x30);

    a.bind(generic_div);
    {
        emit_enter_runtime_frame();
        emit_enter_runtime();

        /* Save MFA and original arguments for the error path. */
        a.stp(ARG2, ARG3, TMP_MEM1q);
        a.str(ARG4, TMP_MEM3q);

        a.mov(ARG1, c_p);
        lea(ARG4, TMP_MEM4q);
        lea(ARG5, TMP_MEM5q);
        runtime_call<5>(erts_int_div_rem);

        emit_leave_runtime();
        emit_leave_runtime_frame();

        a.tst(ARG1, ARG1);
        a.ldp(ARG1, ARG2, TMP_MEM4q);
        a.cond_eq().b(generic_error);

        a.ret(a64::x30);
    }

    a.bind(div_zero);
    {
        mov_imm(TMP1, EXC_BADARITH);
        a.str(TMP1, arm::Mem(c_p, offsetof(Process, freason)));
        a.mov(XREG0, ARG2);
        a.mov(XREG1, ARG3);
        a.b(labels[raise_exception]);
    }

    a.bind(generic_error);
    {
        a.ldp(XREG0, XREG1, TMP_MEM1q);
        a.ldr(ARG4, TMP_MEM3q); // MFA
        a.b(labels[raise_exception]);
    }
}

void BeamModuleAssembler::emit_div_rem(const ArgVal &Fail,
                                       const ArgVal &LHS,
                                       const ArgVal &RHS,
                                       const ErtsCodeMFA *error_mfa) {
    auto [lhs, rhs] = load_sources(LHS, ARG2, RHS, ARG3);

    mov_var(ARG2, lhs);
    mov_var(ARG3, rhs);

    if (Fail.getValue() != 0) {
        fragment_call(ga->get_int_div_rem_guard_shared());
        a.cond_eq().b(resolve_beam_label(Fail, disp1MB));
    } else {
        a.mov(ARG4, imm(error_mfa));
        fragment_call(ga->get_int_div_rem_body_shared());
    }
}

void BeamModuleAssembler::emit_i_rem_div(const ArgVal &Fail,
                                         const ArgVal &Live,
                                         const ArgVal &LHS,
                                         const ArgVal &RHS,
                                         const ArgVal &Remainder,
                                         const ArgVal &Quotient) {
    static const ErtsCodeMFA bif_mfa = {am_erlang, am_rem, 2};

    emit_div_rem(Fail, LHS, RHS, &bif_mfa);

    mov_arg(Remainder, ARG2);
    mov_arg(Quotient, ARG1);
}

void BeamModuleAssembler::emit_i_div_rem(const ArgVal &Fail,
                                         const ArgVal &Live,
                                         const ArgVal &LHS,
                                         const ArgVal &RHS,
                                         const ArgVal &Quotient,
                                         const ArgVal &Remainder) {
    static const ErtsCodeMFA bif_mfa = {am_erlang, am_div, 2};

    emit_div_rem(Fail, LHS, RHS, &bif_mfa);

    mov_arg(Quotient, ARG1);
    mov_arg(Remainder, ARG2);
}

void BeamModuleAssembler::emit_i_int_div(const ArgVal &Fail,
                                         const ArgVal &Live,
                                         const ArgVal &LHS,
                                         const ArgVal &RHS,
                                         const ArgVal &Quotient) {
    static const ErtsCodeMFA bif_mfa = {am_erlang, am_div, 2};

    emit_div_rem(Fail, LHS, RHS, &bif_mfa);

    mov_arg(Quotient, ARG1);
}

void BeamModuleAssembler::emit_i_rem(const ArgVal &Fail,
                                     const ArgVal &Live,
                                     const ArgVal &LHS,
                                     const ArgVal &RHS,
                                     const ArgVal &Remainder) {
    static const ErtsCodeMFA bif_mfa = {am_erlang, am_rem, 2};

    emit_div_rem(Fail, LHS, RHS, &bif_mfa);

    mov_arg(Remainder, ARG2);
}

void BeamModuleAssembler::emit_i_m_div(const ArgVal &Fail,
                                       const ArgVal &Live,
                                       const ArgVal &LHS,
                                       const ArgVal &RHS,
                                       const ArgVal &Dst) {
    static const ErtsCodeMFA bif_mfa = {am_erlang, am_Div, 2};

    Label next = a.newLabel();

    auto [lhs, rhs] = load_sources(LHS, ARG2, RHS, ARG3);

    mov_var(ARG2, lhs);
    mov_var(ARG3, rhs);

    emit_enter_runtime(Live.getValue());

    a.mov(ARG1, c_p);
    runtime_call<3>(erts_mixed_div);

    emit_leave_runtime(Live.getValue());

    a.cmp(ARG1, imm(THE_NON_VALUE));

    if (Fail.getValue() != 0) {
        a.cond_eq().b(resolve_beam_label(Fail, disp1MB));
    } else {
        a.cond_ne().b(next);

        mov_arg(XREG0, LHS);
        mov_arg(XREG1, RHS);

        emit_raise_exception(&bif_mfa);
    }

    a.bind(next);
    mov_arg(Dst, ARG1);
}

/*
 * ARG2 = LHS
 * ARG3 = RHS
 *
 * The module code must have executed emit_enter_runtime()
 * before calling this function.
 *
 * The result is returned in ARG1.
 */
template<typename T>
void BeamGlobalAssembler::emit_bitwise_fallback_body(T(*func_ptr),
                                                     const ErtsCodeMFA *mfa) {
    Label error = a.newLabel();

    emit_enter_runtime_frame();

    /* Save original arguments for the error path. */
    a.stp(ARG2, ARG3, TMP_MEM1q);

    a.mov(ARG1, c_p);
    runtime_call<3>(func_ptr);

    emit_leave_runtime_frame();

    emit_branch_if_not_value(ARG1, error);

    a.ret(a64::x30);

    a.bind(error);
    {
        /* emit_enter_runtime() was done in the module code. */
        emit_leave_runtime(0);

        /* Place the original arguments in X registers. */
        a.ldp(XREG0, XREG1, TMP_MEM1q);
        a.mov(ARG4, imm(mfa));
        a.b(labels[raise_exception]);
    }
}

void BeamGlobalAssembler::emit_i_band_body_shared() {
    static const ErtsCodeMFA bif_mfa = {am_erlang, am_band, 2};
    emit_bitwise_fallback_body(erts_band, &bif_mfa);
}

void BeamModuleAssembler::emit_i_band(const ArgVal &Fail,
                                      const ArgVal &Live,
                                      const ArgVal &LHS,
                                      const ArgVal &RHS,
                                      const ArgVal &Dst) {
    Label next = a.newLabel();
    auto [lhs, rhs] = load_sources(LHS, ARG2, RHS, ARG3);
    auto dst = init_destination(Dst, ARG1);

    /* TAG & TAG = TAG, so we don't need to tag it again. */
    a.and_(ARG1, lhs.reg, rhs.reg);

    /* All other term types has at least one zero in the low 4
     * bits. Therefore, the result will be a small iff both operands
     * are small. */
    ERTS_CT_ASSERT(_TAG_IMMED1_SMALL == _TAG_IMMED1_MASK);
    a.and_(TMP1, ARG1, imm(_TAG_IMMED1_MASK));
    a.cmp(TMP1, imm(_TAG_IMMED1_SMALL));
    a.cond_eq().b(next);

    mov_var(ARG2, lhs);
    mov_var(ARG3, rhs);

    if (Fail.getValue() != 0) {
        emit_enter_runtime(Live.getValue());
        a.mov(ARG1, c_p);
        runtime_call<3>(erts_band);
        emit_leave_runtime(Live.getValue());
        emit_branch_if_not_value(ARG1, resolve_beam_label(Fail, dispUnknown));
    } else {
        emit_enter_runtime(Live.getValue());
        fragment_call(ga->get_i_band_body_shared());
        emit_leave_runtime(Live.getValue());
    }

    a.bind(next);
    {
        mov_var(dst, ARG1);
        flush_var(dst);
    }
}

/*
 * ARG2 = LHS
 * ARG3 = RHS
 *
 * The module code must have executed emit_enter_runtime()
 * before calling this function.
 *
 * Result is returned in RET.
 */
void BeamGlobalAssembler::emit_i_bor_body_shared() {
    static const ErtsCodeMFA bif_mfa = {am_erlang, am_bor, 2};
    emit_bitwise_fallback_body(erts_bor, &bif_mfa);
}

void BeamModuleAssembler::emit_i_bor(const ArgVal &Fail,
                                     const ArgVal &Live,
                                     const ArgVal &LHS,
                                     const ArgVal &RHS,
                                     const ArgVal &Dst) {
    Label generic = a.newLabel(), next = a.newLabel();
    auto [lhs, rhs] = load_sources(LHS, ARG2, RHS, ARG3);
    auto dst = init_destination(Dst, ARG1);

    /* TAG | TAG = TAG, so we don't need to tag it again. */
    a.orr(ARG1, lhs.reg, rhs.reg);

    if (RHS.isImmed() && is_small(RHS.getValue())) {
        a.and_(TMP1, lhs.reg, imm(_TAG_IMMED1_MASK));
    } else if (LHS.isImmed() && is_small(LHS.getValue())) {
        a.and_(TMP1, rhs.reg, imm(_TAG_IMMED1_MASK));
    } else {
        ERTS_CT_ASSERT(_TAG_IMMED1_SMALL == _TAG_IMMED1_MASK);
        a.and_(TMP1, lhs.reg, rhs.reg);
        a.and_(TMP1, TMP1, imm(_TAG_IMMED1_MASK));
    }

    a.cmp(TMP1, imm(_TAG_IMMED1_SMALL));
    a.cond_eq().b(next);

    a.bind(generic);
    {
        mov_var(ARG2, lhs);
        mov_var(ARG3, rhs);

        if (Fail.getValue() != 0) {
            emit_enter_runtime(Live.getValue());
            a.mov(ARG1, c_p);
            runtime_call<3>(erts_bor);
            emit_leave_runtime(Live.getValue());
            emit_branch_if_not_value(ARG1,
                                     resolve_beam_label(Fail, dispUnknown));
        } else {
            emit_enter_runtime(Live.getValue());
            fragment_call(ga->get_i_bor_body_shared());
            emit_leave_runtime(Live.getValue());
        }
    }

    a.bind(next);
    {
        mov_var(dst, ARG1);
        flush_var(dst);
    }
}

/*
 * ARG2 = LHS
 * ARG3 = RHS
 *
 * The module code must have executed emit_enter_runtime()
 * before calling this function.
 *
 * The result is returned in ARG1.
 */
void BeamGlobalAssembler::emit_i_bxor_body_shared() {
    static const ErtsCodeMFA bif_mfa = {am_erlang, am_bxor, 2};
    emit_bitwise_fallback_body(erts_bxor, &bif_mfa);
}

void BeamModuleAssembler::emit_i_bxor(const ArgVal &Fail,
                                      const ArgVal &Live,
                                      const ArgVal &LHS,
                                      const ArgVal &RHS,
                                      const ArgVal &Dst) {
    Label next = a.newLabel();
    auto [lhs, rhs] = load_sources(LHS, ARG2, RHS, ARG3);
    auto dst = init_destination(Dst, ARG1);

    /* TAG ^ TAG = 0, so we'll need to tag it again. */
    a.eor(ARG1, lhs.reg, rhs.reg);
    a.orr(ARG1, ARG1, imm(_TAG_IMMED1_SMALL));

    if (RHS.isImmed() && is_small(RHS.getValue())) {
        a.and_(TMP1, lhs.reg, imm(_TAG_IMMED1_MASK));
    } else if (LHS.isImmed() && is_small(LHS.getValue())) {
        a.and_(TMP1, rhs.reg, imm(_TAG_IMMED1_MASK));
    } else {
        ERTS_CT_ASSERT(_TAG_IMMED1_SMALL == _TAG_IMMED1_MASK);
        a.and_(TMP1, lhs.reg, rhs.reg);
        a.and_(TMP1, TMP1, imm(_TAG_IMMED1_MASK));
    }

    a.cmp(TMP1, imm(_TAG_IMMED1_SMALL));
    a.cond_eq().b(next);

    mov_var(ARG2, lhs);
    mov_var(ARG3, rhs);

    if (Fail.getValue() != 0) {
        emit_enter_runtime(Live.getValue());
        runtime_call<3>(erts_bxor);
        emit_leave_runtime(Live.getValue());
        emit_branch_if_not_value(ARG1, resolve_beam_label(Fail, dispUnknown));
    } else {
        emit_enter_runtime(Live.getValue());
        fragment_call(ga->get_i_bxor_body_shared());
        emit_leave_runtime(Live.getValue());
    }

    a.bind(next);
    {
        mov_var(dst, ARG1);
        flush_var(dst);
    }
}

/*
 * ARG1 = Src
 *
 * The module code must have executed emit_enter_runtime()
 * before calling this function.
 *
 * The result is returned in ARG1. Error is indicated by
 * THE_NON_VALUE.
 */
void BeamGlobalAssembler::emit_i_bnot_guard_shared() {
    emit_enter_runtime_frame();

    /* Undo the speculative inversion in module code. */
    a.eor(ARG2, ARG1, imm(~_TAG_IMMED1_MASK));

    a.mov(ARG1, c_p);
    runtime_call<2>(erts_bnot);

    emit_leave_runtime_frame();

    a.ret(a64::x30);
}

/*
 * ARG1 = Src
 *
 * The module code must have executed emit_enter_runtime()
 * before calling this function.
 *
 * The result is returned in ARG1.
 */
void BeamGlobalAssembler::emit_i_bnot_body_shared() {
    Label error = a.newLabel();

    emit_enter_runtime_frame();

    /* Undo the speculative inversion in module code. */
    a.eor(ARG2, ARG1, imm(~_TAG_IMMED1_MASK));

    /* Save original arguments for the error path. */
    a.str(ARG2, TMP_MEM1q);

    a.mov(ARG1, c_p);
    runtime_call<2>(erts_bnot);

    emit_leave_runtime_frame();

    emit_branch_if_not_value(ARG1, error);
    a.ret(a64::x30);

    a.bind(error);
    {
        static const ErtsCodeMFA bif_mfa = {am_erlang, am_bnot, 1};

        /* emit_enter_runtime() was done in the module code. */
        emit_leave_runtime(0);

        /* Place the original arguments in X registers. */
        a.ldr(XREG0, TMP_MEM1q);
        mov_imm(ARG4, &bif_mfa);
        a.b(labels[raise_exception]);
    }
}

void BeamModuleAssembler::emit_i_bnot(const ArgVal &Fail,
                                      const ArgVal &Live,
                                      const ArgVal &Src,
                                      const ArgVal &Dst) {
    Label next = a.newLabel();
    auto src = load_source(Src, TMP2);
    auto dst = init_destination(Dst, ARG1);

    /* Invert everything except the tag so we don't have to tag it again. */
    a.eor(ARG1, src.reg, imm(~_TAG_IMMED1_MASK));

    a.and_(TMP1, src.reg, imm(_TAG_IMMED1_MASK));
    a.cmp(TMP1, imm(_TAG_IMMED1_SMALL));
    a.cond_eq().b(next);

    if (Fail.getValue() != 0) {
        emit_enter_runtime(Live.getValue());
        fragment_call(ga->get_i_bnot_guard_shared());
        emit_leave_runtime(Live.getValue());
        emit_branch_if_not_value(ARG1, resolve_beam_label(Fail, dispUnknown));
    } else {
        emit_enter_runtime(Live.getValue());
        fragment_call(ga->get_i_bnot_body_shared());
        emit_leave_runtime(Live.getValue());
    }

    a.bind(next);
    mov_var(dst, ARG1);
    flush_var(dst);
}

/*
 * ARG2 = LHS
 * ARG3 = RHS
 *
 * The module code must have executed emit_enter_runtime()
 * before calling this function.
 *
 * The result is returned in ARG1.
 */
void BeamGlobalAssembler::emit_i_bsr_body_shared() {
    static const ErtsCodeMFA bif_mfa = {am_erlang, am_bsr, 2};
    emit_bitwise_fallback_body(erts_bsr, &bif_mfa);
}

void BeamModuleAssembler::emit_i_bsr(const ArgVal &Fail,
                                     const ArgVal &Live,
                                     const ArgVal &LHS,
                                     const ArgVal &RHS,
                                     const ArgVal &Dst) {
    Label generic = a.newLabel(), next = a.newLabel();
    auto lhs = load_source(LHS, ARG2);
    auto dst = init_destination(Dst, ARG1);

    if (RHS.isImmed() && is_small(RHS.getValue())) {
        Sint shift = signed_val(RHS.getValue());

        if (shift >= 0 && shift < SMALL_BITS - 1) {
            a.and_(TMP1, lhs.reg, imm(_TAG_IMMED1_MASK));
            a.cmp(TMP1, imm(_TAG_IMMED1_SMALL));
            a.cond_ne().b(generic);

            /* We don't need to clear the mask after shifting because
             * _TAG_IMMED1_SMALL will set all the bits anyway. */
            ERTS_CT_ASSERT(_TAG_IMMED1_MASK == _TAG_IMMED1_SMALL);
            a.asr(TMP1, lhs.reg, imm(shift));
            a.orr(dst.reg, TMP1, imm(_TAG_IMMED1_SMALL));

            a.b(next);
        } else {
            /* Constant shift is negative or too big to fit the `asr`
             * instruction; fall back to the generic path. */
        }
    }

    a.bind(generic);
    {
        mov_var(ARG2, lhs);
        mov_arg(ARG3, RHS);

        if (Fail.getValue() != 0) {
            emit_enter_runtime(Live.getValue());
            a.mov(ARG1, c_p);
            runtime_call<3>(erts_bsr);
            emit_leave_runtime(Live.getValue());
            emit_branch_if_not_value(ARG1,
                                     resolve_beam_label(Fail, dispUnknown));
        } else {
            emit_enter_runtime(Live.getValue());
            fragment_call(ga->get_i_bsr_body_shared());
            emit_leave_runtime(Live.getValue());
        }

        mov_var(dst, ARG1);
    }

    a.bind(next);
    flush_var(dst);
}

/*
 * ARG2 = LHS
 * ARG3 = RHS
 *
 * The module code must have executed emit_enter_runtime()
 * before calling this function.
 *
 * The result is returned in ARG1.
 */
void BeamGlobalAssembler::emit_i_bsl_body_shared() {
    static const ErtsCodeMFA bif_mfa = {am_erlang, am_bsl, 2};
    emit_bitwise_fallback_body(erts_bsl, &bif_mfa);
}

static int count_leading_zeroes(UWord value) {
    const int word_bits = sizeof(value) * CHAR_BIT;

    if (value == 0) {
        return word_bits;
    }

    return Support::clz(value);
}

void BeamModuleAssembler::emit_i_bsl(const ArgVal &Fail,
                                     const ArgVal &Live,
                                     const ArgVal &LHS,
                                     const ArgVal &RHS,
                                     const ArgVal &Dst) {
    Label generic = a.newLabel(), next = a.newLabel();
    auto [lhs, rhs] = load_sources(LHS, ARG2, RHS, ARG3);
    auto dst = init_destination(Dst, ARG1);

    bool inline_shift = true;
    if (LHS.isImmed() && RHS.isImmed()) {
        /* The compiler should've optimized this away, so we'll fall
         * back to the generic path to simplify the inline
         * implementation. */
        inline_shift = false;
    } else if (LHS.isLiteral() || RHS.isLiteral()) {
        /* At least one argument is not a small. */
        inline_shift = false;
    } else if (LHS.isImmed() && !is_small(LHS.getValue())) {
        /* Invalid constant. */
        inline_shift = false;
    } else if (RHS.isImmed() &&
               (!is_small(RHS.getValue()) || signed_val(RHS.getValue()) < 0 ||
                signed_val(RHS.getValue()) >= SMALL_BITS - 1)) {
        /* Constant shift is invalid or always produces a bignum. */
        inline_shift = false;
    }

    if (inline_shift) {
        /* shiftLimit will be calculated as the number of leading sign
         * bits not counting the sign bit. Note that this value is one
         * lower than the number of leading zeros as used by the
         * x86_64 JIT. */
        Operand shiftLimit, shiftCount;

        ASSERT(!(LHS.isImmed() && RHS.isImmed()));
        if (LHS.isRegister()) {
            /* Count the number of leading sign bits so we can test
             * whether the shift will overflow. (The count does not
             * include the sign bit.) To ensure that the tag bits are
             * not counted, we must make sure that the topmost tag bit
             * is equal to the inverted value of the sign bit. */
            ERTS_CT_ASSERT(_TAG_IMMED1_SMALL == _TAG_IMMED1_MASK);
            a.eor(TMP1, lhs.reg, lhs.reg, arm::lsr(64 - _TAG_IMMED1_SIZE));
            a.cls(ARG4, TMP1);
            shiftLimit = ARG4;

            a.and_(TMP1, lhs.reg, imm(_TAG_IMMED1_MASK));
            a.cmp(TMP1, imm(_TAG_IMMED1_SMALL));
            a.cond_ne().b(generic);
        } else {
            UWord value = LHS.getValue();

            if (signed_val(value) < 0) {
                value ^= ~(UWord)_TAG_IMMED1_MASK;
            }

            shiftLimit = imm(count_leading_zeroes(value) - 1);
        }

        if (RHS.isRegister()) {
            /* Negate the tag bits and then rotate them out, forcing the
             * comparison below to fail for non-smalls. */
            ERTS_CT_ASSERT(_TAG_IMMED1_SMALL == _TAG_IMMED1_MASK);
            a.eor(ARG5, rhs.reg, imm(_TAG_IMMED1_SMALL));
            a.ror(ARG5, ARG5, imm(_TAG_IMMED1_SIZE));
            shiftCount = ARG5;

            /* Fall back to generic path when the shift magnitude is negative or
             * greater than the leading zero count.
             *
             * The raw emit form is used since `shiftLimit` may be a register
             * or immediate, and the `cmp` helper doesn't accept untyped
             * `Operand`s. */
            a.emit(a64::Inst::kIdCmp, ARG5, shiftLimit);
            a.cond_hi().b(generic);
        } else {
            ASSERT(!shiftLimit.isImm());

            shiftCount = imm(signed_val(RHS.getValue()));

            a.emit(a64::Inst::kIdCmp, shiftLimit, shiftCount);
            a.cond_lo().b(generic);
        }

        a.and_(TMP1, lhs.reg, imm(~_TAG_IMMED1_MASK));
        a.emit(a64::Inst::kIdLsl, TMP1, TMP1, shiftCount);
        a.orr(dst.reg, TMP1, imm(_TAG_IMMED1_SMALL));

        flush_var(dst);
        a.b(next);
    }

    a.bind(generic);
    {
        mov_var(ARG2, lhs);
        mov_var(ARG3, rhs);

        if (Fail.getValue() != 0) {
            emit_enter_runtime(Live.getValue());
            a.mov(ARG1, c_p);
            runtime_call<3>(erts_bsl);
            emit_leave_runtime(Live.getValue());
            emit_branch_if_not_value(ARG1,
                                     resolve_beam_label(Fail, dispUnknown));
        } else {
            emit_enter_runtime(Live.getValue());
            fragment_call(ga->get_i_bsl_body_shared());
            emit_leave_runtime(Live.getValue());
        }

        mov_var(dst, ARG1);
        flush_var(dst);
    }

    a.bind(next);
}
