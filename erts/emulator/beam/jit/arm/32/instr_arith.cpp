/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2020-2024. All Rights Reserved.
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
#include "big.h"
}

void BeamModuleAssembler::emit_add_sub_types(bool is_small_result,
                                             const ArgSource &LHS,
                                             const a64::Gp lhs_reg,
                                             const ArgSource &RHS,
                                             const a64::Gp rhs_reg,
                                             const Label next) {
    if (is_small_result) {
        comment("skipped overflow test because the result is always small");
        emit_are_both_small(LHS, lhs_reg, RHS, rhs_reg, next);
    } else if (RHS.isLiteral()) {
        /* Skipping test for small */
    } else {
        if (always_small(RHS)) {
            a.and_(TMP1, lhs_reg, imm(_TAG_IMMED1_MASK));
        } else if (always_small(LHS)) {
            a.and_(TMP1, rhs_reg, imm(_TAG_IMMED1_MASK));
        } else {
            ERTS_CT_ASSERT(_TAG_IMMED1_SMALL == _TAG_IMMED1_MASK);
            a.and_(TMP1, lhs_reg, rhs_reg);
            a.and_(TMP1, TMP1, imm(_TAG_IMMED1_MASK));
        }

        comment("test for not overflow and small operands");
        a.ccmp(TMP1,
               imm(_TAG_IMMED1_SMALL),
               imm(NZCV::kNone),
               imm(arm::CondCode::kVC));
        a.b_eq(next);
    }
}

void BeamModuleAssembler::emit_are_both_small(const ArgSource &LHS,
                                              const a64::Gp lhs_reg,
                                              const ArgSource &RHS,
                                              const a64::Gp rhs_reg,
                                              const Label next) {
    if (RHS.isLiteral()) {
        comment("skipped test for small because one operand is never small");
    } else if (always_small(RHS) &&
               always_one_of<BeamTypeId::Integer, BeamTypeId::AlwaysBoxed>(
                       LHS)) {
        comment("simplified test for small operand since other types are "
                "boxed");
        emit_is_boxed(next, lhs_reg);
    } else if (always_small(LHS) &&
               always_one_of<BeamTypeId::Integer, BeamTypeId::AlwaysBoxed>(
                       RHS)) {
        comment("simplified test for small operand since other types are "
                "boxed");
        emit_is_boxed(next, rhs_reg);
    } else if (always_one_of<BeamTypeId::Integer, BeamTypeId::AlwaysBoxed>(
                       LHS) &&
               always_one_of<BeamTypeId::Integer, BeamTypeId::AlwaysBoxed>(
                       RHS)) {
        comment("simplified test for small operands since other types are "
                "boxed");
        ERTS_CT_ASSERT(_TAG_IMMED1_SMALL == _TAG_IMMED1_MASK);
        a.and_(TMP1, lhs_reg, rhs_reg);
        emit_is_boxed(next, TMP1);
    } else {
        if (always_small(RHS)) {
            a.and_(TMP1, lhs_reg, imm(_TAG_IMMED1_MASK));
        } else if (always_small(LHS)) {
            a.and_(TMP1, rhs_reg, imm(_TAG_IMMED1_MASK));
        } else {
            ERTS_CT_ASSERT(_TAG_IMMED1_SMALL == _TAG_IMMED1_MASK);
            a.and_(TMP1, lhs_reg, rhs_reg);
            a.and_(TMP1, TMP1, imm(_TAG_IMMED1_MASK));
        }
        a.cmp(TMP1, imm(_TAG_IMMED1_SMALL));
        a.b_eq(next);
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

void BeamModuleAssembler::emit_i_plus(const ArgLabel &Fail,
                                      const ArgWord &Live,
                                      const ArgSource &LHS,
                                      const ArgSource &RHS,
                                      const ArgRegister &Dst) {
    bool rhs_is_arm_literal =
            RHS.isSmall() && Support::isUInt12(RHS.as<ArgSmall>().get());
    bool is_small_result = is_sum_small_if_args_are_small(LHS, RHS);

    if (always_small(LHS) && always_small(RHS) && is_small_result) {
        auto dst = init_destination(Dst, ARG1);
        if (rhs_is_arm_literal) {
            auto lhs = load_source(LHS);
            Uint cleared_tag = RHS.as<ArgSmall>().get() & ~_TAG_IMMED1_MASK;
            comment("add small constant without overflow check");
            a.add(dst.reg, lhs.reg, imm(cleared_tag));
        } else {
            auto [lhs, rhs] = load_sources(LHS, ARG2, RHS, ARG3);
            comment("addition without overflow check");
            a.and_(TMP1, rhs.reg, imm(~_TAG_IMMED1_MASK));
            a.add(dst.reg, lhs.reg, TMP1);
        }
        flush_var(dst);
        return;
    }

    Label next = a.newLabel();

    auto [lhs, rhs] = load_sources(LHS, ARG2, RHS, ARG3);

    if (RHS.isLiteral()) {
        comment("skipped test for small because one operand is never small");
    } else if (rhs_is_arm_literal) {
        Uint cleared_tag = RHS.as<ArgSmall>().get() & ~_TAG_IMMED1_MASK;
        a.adds(ARG1, lhs.reg, imm(cleared_tag));
    } else {
        a.and_(TMP1, rhs.reg, imm(~_TAG_IMMED1_MASK));
        a.adds(ARG1, lhs.reg, TMP1);
    }

    emit_add_sub_types(is_small_result, LHS, lhs.reg, RHS, rhs.reg, next);

    mov_var(ARG2, lhs);
    mov_var(ARG3, rhs);

    if (Fail.get() != 0) {
        emit_enter_runtime(Live.get());
        a.mov(ARG1, c_p);
        runtime_call<3>(erts_mixed_plus);
        emit_leave_runtime(Live.get());

        emit_branch_if_not_value(ARG1, resolve_beam_label(Fail, dispUnknown));
    } else {
        emit_enter_runtime(Live.get());
        fragment_call(ga->get_plus_body_shared());
        emit_leave_runtime(Live.get());
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

void BeamModuleAssembler::emit_i_unary_minus(const ArgLabel &Fail,
                                             const ArgWord &Live,
                                             const ArgSource &Src,
                                             const ArgRegister &Dst) {
    auto src = load_source(Src, ARG2);
    auto zero = ArgImmed(make_small(0));
    bool is_small_result = is_diff_small_if_args_are_small(zero, Src);

    a.mov(TMP1, imm(_TAG_IMMED1_SMALL));
    a.and_(TMP2, src.reg, imm(~_TAG_IMMED1_MASK));

    if (always_small(Src) && is_small_result) {
        auto dst = init_destination(Dst, ARG1);
        comment("no overflow test because result is always small");
        a.sub(dst.reg, TMP1, TMP2);
        flush_var(dst);
        return;
    }

    Label next = a.newLabel();

    a.subs(ARG1, TMP1, TMP2);

    /* Test for not overflow AND small operands. */
    a.ccmp(TMP2,
           imm(_TAG_IMMED1_SMALL),
           imm(NZCV::kNone),
           imm(arm::CondCode::kVC));
    a.b_eq(next);

    mov_var(ARG2, src);
    if (Fail.get() != 0) {
        emit_enter_runtime(Live.get());
        a.mov(ARG1, c_p);
        runtime_call<2>(erts_unary_minus);
        emit_leave_runtime(Live.get());

        emit_branch_if_not_value(ARG1, resolve_beam_label(Fail, dispUnknown));
    } else {
        emit_enter_runtime(Live.get());
        fragment_call(ga->get_unary_minus_body_shared());
        emit_leave_runtime(Live.get());
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

void BeamModuleAssembler::emit_i_minus(const ArgLabel &Fail,
                                       const ArgWord &Live,
                                       const ArgSource &LHS,
                                       const ArgSource &RHS,
                                       const ArgRegister &Dst) {
    bool rhs_is_arm_literal =
            RHS.isSmall() && Support::isUInt12(RHS.as<ArgSmall>().get());
    bool is_small_result = is_diff_small_if_args_are_small(LHS, RHS);

    if (always_small(LHS) && always_small(RHS) && is_small_result) {
        auto dst = init_destination(Dst, ARG1);
        if (rhs_is_arm_literal) {
            auto lhs = load_source(LHS);
            Uint cleared_tag = RHS.as<ArgSmall>().get() & ~_TAG_IMMED1_MASK;
            comment("subtract small constant without overflow check");
            a.sub(dst.reg, lhs.reg, imm(cleared_tag));
        } else {
            auto [lhs, rhs] = load_sources(LHS, ARG2, RHS, ARG3);
            comment("subtraction without overflow check");
            a.and_(TMP1, rhs.reg, imm(~_TAG_IMMED1_MASK));
            a.sub(dst.reg, lhs.reg, TMP1);
        }
        flush_var(dst);
        return;
    }

    Label next = a.newLabel();
    auto [lhs, rhs] = load_sources(LHS, ARG2, RHS, ARG3);

    if (RHS.isLiteral()) {
        comment("skipped test for small because one operand is never small");
    } else if (rhs_is_arm_literal) {
        Uint cleared_tag = RHS.as<ArgSmall>().get() & ~_TAG_IMMED1_MASK;
        a.subs(ARG1, lhs.reg, imm(cleared_tag));
    } else {
        a.and_(TMP1, rhs.reg, imm(~_TAG_IMMED1_MASK));
        a.subs(ARG1, lhs.reg, TMP1);
    }

    emit_add_sub_types(is_small_result, LHS, lhs.reg, RHS, rhs.reg, next);

    mov_var(ARG2, lhs);
    mov_var(ARG3, rhs);

    if (Fail.get() != 0) {
        emit_enter_runtime(Live.get());
        a.mov(ARG1, c_p);
        runtime_call<3>(erts_mixed_minus);
        emit_leave_runtime(Live.get());
        emit_branch_if_not_value(ARG1, resolve_beam_label(Fail, dispUnknown));
    } else {
        emit_enter_runtime(Live.get());
        fragment_call(ga->get_minus_body_shared());
        emit_leave_runtime(Live.get());
    }

    a.bind(next);
    mov_arg(Dst, ARG1);
}

/*
 * Create a bignum from a the 128-bit product of two smalls shifted
 * left _TAG_IMMED1_SIZE bits.
 *
 * ARG1 = low 64 bits
 * TMP2 = high 64 bits
 *
 * The result is returned in ARG1.
 */
void BeamGlobalAssembler::emit_int128_to_big_shared() {
    Label positive = a.newLabel();

    a.extr(ARG3, TMP2, ARG1, imm(_TAG_IMMED1_SIZE));
    a.asr(ARG4, TMP2, imm(_TAG_IMMED1_SIZE));

    a.mov(ARG1, c_p);

    a.cmp(ARG4, imm(0));
    a.cset(ARG2, arm::CondCode::kMI);

    a.b_pl(positive);
    a.negs(ARG3, ARG3);
    a.ngc(ARG4, ARG4);

    a.bind(positive);
    emit_enter_runtime_frame();
    emit_enter_runtime();

    runtime_call<4>(beam_jit_int128_to_big);

    emit_leave_runtime();
    emit_leave_runtime_frame();

    a.ret(a64::x30);
}

/* ARG2 = Src1
 * ARG3 = Src2
 * ARG4 = Src4
 *
 * The result is returned in ARG1.
 */
void BeamGlobalAssembler::emit_mul_add_body_shared() {
    Label mul_only = a.newLabel(), error = a.newLabel(),
          mul_error = a.newLabel(), do_error = a.newLabel();

    emit_enter_runtime_frame();
    emit_enter_runtime();

    /* Save original arguments. */
    a.stp(ARG2, ARG3, TMP_MEM1q);
    a.mov(ARG1, c_p);
    a.cmp(ARG4, imm(make_small(0)));
    a.b_eq(mul_only);
    a.str(ARG4, TMP_MEM4q);

    lea(ARG5, TMP_MEM3q);
    runtime_call<5>(erts_mul_add);

    emit_leave_runtime();
    emit_leave_runtime_frame();

    emit_branch_if_not_value(ARG1, error);
    a.ret(a64::x30);

    a.bind(mul_only);
    {
        runtime_call<3>(erts_mixed_times);

        emit_leave_runtime();
        emit_leave_runtime_frame();

        emit_branch_if_not_value(ARG1, mul_error);
        a.ret(a64::x30);
    }

    a.bind(error);
    {
        static const ErtsCodeMFA mul_mfa = {am_erlang, am_Times, 2};
        static const ErtsCodeMFA add_mfa = {am_erlang, am_Plus, 2};

        a.ldp(XREG0, XREG1, TMP_MEM3q);
        mov_imm(ARG4, &add_mfa);
        emit_branch_if_value(XREG0, do_error);

        a.bind(mul_error);
        a.ldp(XREG0, XREG1, TMP_MEM1q);
        mov_imm(ARG4, &mul_mfa);

        a.bind(do_error);
        a.b(labels[raise_exception]);
    }
}

/* ARG2 = Src1
 * ARG3 = Src2
 * ARG4 = Src4
 *
 * The result is returned in ARG1 (set to THE_NON_VALUE if
 * the call failed).
 */
void BeamGlobalAssembler::emit_mul_add_guard_shared() {
    Label mul_failed = a.newLabel();

    a.str(ARG4, TMP_MEM1q);

    emit_enter_runtime_frame();
    emit_enter_runtime();

    a.mov(ARG1, c_p);
    runtime_call<3>(erts_mixed_times);
    emit_branch_if_not_value(ARG1, mul_failed);

    a.ldr(ARG3, TMP_MEM1q);
    a.mov(ARG2, ARG1);
    a.mov(ARG1, c_p);
    runtime_call<3>(erts_mixed_plus);

    a.bind(mul_failed);
    emit_leave_runtime();
    emit_leave_runtime_frame();

    a.ret(a64::x30);
}

/* ARG2 = Src1
 * ARG3 = Src2
 *
 * The result is returned in ARG1.
 */
void BeamGlobalAssembler::emit_mul_body_shared() {
    mov_imm(ARG4, make_small(0));
    a.b(labels[mul_add_body_shared]);
}

/* ARG2 = Src1
 * ARG3 = Src2
 *
 * The result is returned in ARG1 (set to THE_NON_VALUE if
 * the call failed).
 */
void BeamGlobalAssembler::emit_mul_guard_shared() {
    mov_imm(ARG4, make_small(0));
    a.b(labels[mul_add_guard_shared]);
}

void BeamModuleAssembler::emit_i_mul_add(const ArgLabel &Fail,
                                         const ArgSource &Src1,
                                         const ArgSource &Src2,
                                         const ArgSource &Src3,
                                         const ArgSource &Src4,
                                         const ArgRegister &Dst) {
    bool is_product_small = is_product_small_if_args_are_small(Src1, Src2);
    bool is_sum_small = is_sum_small_if_args_are_small(Src3, Src4);
    bool sometimes_small = !(Src2.isLiteral() || Src4.isLiteral());
    bool is_increment_zero =
            Src4.isSmall() && Src4.as<ArgSmall>().getSigned() == 0;
    Sint factor = 0;
    int left_shift = -1;

    if (is_increment_zero) {
        comment("(adding zero)");
    }

    if (Src2.isSmall()) {
        factor = Src2.as<ArgSmall>().getSigned();
        if (Support::isPowerOf2(factor)) {
            left_shift = Support::ctz<Eterm>(factor);
        }
    }

    if (always_small(Src1) && Src2.isSmall() && always_small(Src4) &&
        is_product_small && is_sum_small) {
        auto dst = init_destination(Dst, ARG1);
        auto [src1, src4] = load_sources(Src1, ARG2, Src4, ARG3);

        comment("multiplication and addition without overflow check");
        a.and_(TMP1, src1.reg, imm(~_TAG_IMMED1_MASK));
        if (left_shift > 0) {
            comment("optimized multiplication by replacing with left "
                    "shift");
            a.add(dst.reg, src4.reg, TMP1, arm::lsl(left_shift));
        } else {
            mov_imm(TMP2, factor);
            a.madd(dst.reg, TMP1, TMP2, src4.reg);
        }
        flush_var(dst);
    } else {
        Label small = a.newLabel();
        Label store_result = a.newLabel();
        auto [src1, src2] = load_sources(Src1, ARG2, Src2, ARG3);
        auto src4 = load_source(ArgXRegister(0), XREG0);

        if (!is_increment_zero) {
            src4 = load_source(Src4, ARG4);
        }

        if (always_small(Src1) && always_small(Src2) && always_small(Src4)) {
            comment("skipped test for small operands since they are always "
                    "small");
        } else {
            if (always_small(Src4)) {
                emit_are_both_small(Src1, src1.reg, Src2, src2.reg, small);
            } else if (always_small(Src2)) {
                emit_are_both_small(Src1, src1.reg, Src4, src4.reg, small);
            } else if (sometimes_small) {
                ASSERT(!is_increment_zero);
                ERTS_CT_ASSERT(_TAG_IMMED1_SMALL == _TAG_IMMED1_MASK);
                a.and_(TMP1, src1.reg, src2.reg);
                a.and_(TMP1, TMP1, src4.reg);
                if (always_one_of<BeamTypeId::Integer, BeamTypeId::AlwaysBoxed>(
                            Src1) &&
                    always_one_of<BeamTypeId::Integer, BeamTypeId::AlwaysBoxed>(
                            Src2) &&
                    always_one_of<BeamTypeId::Integer, BeamTypeId::AlwaysBoxed>(
                            Src4)) {
                    emit_is_boxed(small, TMP1);
                } else {
                    a.and_(TMP1, TMP1, imm(_TAG_IMMED1_MASK));
                    a.cmp(TMP1, imm(_TAG_IMMED1_SMALL));
                    a.b_eq(small);
                }
            } else {
                comment("skipped test for small because one operand is never "
                        "small");
            }

            mov_var(ARG2, src1);
            mov_var(ARG3, src2);

            if (Fail.get() != 0) {
                if (is_increment_zero) {
                    fragment_call(ga->get_mul_guard_shared());
                } else {
                    mov_var(ARG4, src4);
                    fragment_call(ga->get_mul_add_guard_shared());
                }
                emit_branch_if_not_value(ARG1,
                                         resolve_beam_label(Fail, dispUnknown));
            } else {
                if (is_increment_zero) {
                    fragment_call(ga->get_mul_body_shared());
                } else {
                    mov_var(ARG4, src4);
                    fragment_call(ga->get_mul_add_body_shared());
                }
            }

            if (sometimes_small) {
                a.b(store_result);
            }
        }

        a.bind(small);
        if (sometimes_small) {
            if (is_increment_zero) {
                comment("multiply smalls");
            } else {
                comment("multiply and add smalls");
            }
        }

        if (is_product_small && is_sum_small) {
            a64::Gp increment_reg;

            a.and_(TMP3, src1.reg, imm(~_TAG_IMMED1_MASK));

            if (is_increment_zero) {
                mov_imm(TMP1, make_small(0));
                increment_reg = TMP1;
            } else {
                increment_reg = src4.reg;
            }

            if (left_shift > 0) {
                comment("optimized multiplication by replacing with left "
                        "shift");
                a.add(ARG1, increment_reg, TMP3, arm::lsl(left_shift));
            } else {
                a.asr(TMP4, src2.reg, imm(_TAG_IMMED1_SIZE));
                a.madd(ARG1, TMP3, TMP4, increment_reg);
            }

            comment("skipped test for small result");
        } else if (sometimes_small) {
            auto min_increment = std::get<0>(getClampedRange(Src4));

            a.and_(TMP3, src1.reg, imm(~_TAG_IMMED1_MASK));
            if (left_shift == 0) {
                comment("optimized multiplication by one");
                a.mov(ARG1, TMP3);
                a.asr(TMP2, TMP3, imm(63));
            } else if (left_shift > 0) {
                comment("optimized multiplication by replacing with left "
                        "shift");
                a.lsl(ARG1, TMP3, imm(left_shift));
                a.asr(TMP2, TMP3, imm(64 - left_shift));
            } else {
                ASSERT(left_shift == -1);
                a.asr(TMP4, src2.reg, imm(_TAG_IMMED1_SIZE));
                a.mul(ARG1, TMP3, TMP4);
                a.smulh(TMP2, TMP3, TMP4);
            }

            if (is_increment_zero) {
                a.add(ARG1, ARG1, imm(_TAG_IMMED1_SMALL));
            } else {
                a64::Gp sign_reg;

                if (min_increment > 0) {
                    sign_reg = ZERO;
                } else {
                    sign_reg = TMP3;
                    a.asr(sign_reg, src4.reg, imm(63));
                }

                a.adds(ARG1, ARG1, src4.reg);
                a.adc(TMP2, TMP2, sign_reg);
            }

            comment("test whether the result fits in a small");
            /* The high 65 bits of result will all be the same if no
             * overflow occurred. Another way to say that is that the
             * sign bit of the low 64 bits repeated 64 times must be
             * equal to the high 64 bits of the result. */
            a.asr(TMP3, ARG1, imm(SMALL_BITS + _TAG_IMMED1_SIZE - 1));
            a.cmp(TMP2, TMP3);
            a.b_eq(store_result);

            fragment_call(ga->get_int128_to_big_shared());
        }

        a.bind(store_result);
        mov_arg(Dst, ARG1);
    }
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
    a.b_eq(exit);

    /* Check whether both operands are small integers. */
    ERTS_CT_ASSERT(_TAG_IMMED1_SMALL == _TAG_IMMED1_MASK);
    a.and_(TMP1, ARG2, ARG3);
    a.and_(TMP1, TMP1, imm(_TAG_IMMED1_MASK));
    a.cmp(TMP1, imm(_TAG_IMMED1_SMALL));
    a.b_ne(generic);

    /* MIN_SMALL divided by -1 will overflow, and we'll need to fall
     * back to the generic handler in that case. */
    a.asr(TMP1, TMP3, imm(SMALL_BITS - 1));
    a.cmp(TMP1, imm(1));
    a.b_ge(generic);

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
    a.b_eq(div_zero);

    /* Check whether both operands are small integers. */
    ERTS_CT_ASSERT(_TAG_IMMED1_SMALL == _TAG_IMMED1_MASK);
    a.and_(TMP1, ARG2, ARG3);
    a.and_(TMP1, TMP1, imm(_TAG_IMMED1_MASK));
    a.cmp(TMP1, imm(_TAG_IMMED1_SMALL));
    a.b_ne(generic_div);

    /* MIN_SMALL divided by -1 will overflow, and we'll need to fall
     * back to the generic handler in that case. */
    a.asr(TMP1, TMP3, imm(SMALL_BITS - 1));
    a.cmp(TMP1, imm(1));
    a.b_ge(generic_div);

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
        a.b_eq(generic_error);

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

void BeamModuleAssembler::emit_div_rem_literal(Sint divisor,
                                               const ArgSource &Dividend,
                                               a64::Gp dividend,
                                               a64::Gp quotient,
                                               a64::Gp remainder,
                                               const Label &generic,
                                               bool need_div,
                                               bool need_rem) {
    a64::Gp small_tag = TMP6;
    bool small_dividend = !generic.isValid();

    ASSERT(divisor != (Sint)0);

    if (!small_dividend) {
        a.and_(small_tag, dividend, imm(_TAG_IMMED1_MASK));
        a.cmp(small_tag, imm(_TAG_IMMED1_SMALL));
        a.b_ne(generic);
    }

    if (Support::isPowerOf2(divisor)) {
        a64::Gp original_dividend = dividend;
        int shift = Support::ctz<Eterm>(divisor);

        if (need_div && small_dividend) {
            mov_imm(small_tag, _TAG_IMMED1_SMALL);
        }

        ERTS_CT_ASSERT(_TAG_IMMED1_SMALL == _TAG_IMMED1_MASK);
        if (std::get<0>(getClampedRange(Dividend)) >= 0) {
            /* Positive dividend. */
            if (need_div) {
                comment("optimized div by replacing with right shift");
                if (need_rem && quotient == dividend) {
                    original_dividend = TMP5;
                    a.mov(original_dividend, dividend);
                }
                a.orr(quotient, small_tag, dividend, arm::lsr(shift));
            }
            if (need_rem) {
                auto mask = Support::lsbMask<Uint>(shift + _TAG_IMMED1_SIZE);
                comment("optimized rem by replacing with masking");
                a.and_(remainder, original_dividend, imm(mask));
            }
        } else {
            /* Negative dividend. */
            if (need_div) {
                comment("optimized div by replacing with right shift");
            }
            if (divisor == 2) {
                ERTS_CT_ASSERT(_TAG_IMMED1_SMALL == _TAG_IMMED1_MASK);
                a.add(TMP3, dividend, dividend, arm::lsr(63));
            } else {
                add(TMP1, dividend, (divisor - 1) << _TAG_IMMED1_SIZE);
                a.cmp(dividend, imm(0));
                a.csel(TMP3, TMP1, dividend, imm(arm::CondCode::kLT));
            }
            if (need_div) {
                if (need_rem && quotient == dividend) {
                    original_dividend = TMP5;
                    a.mov(original_dividend, dividend);
                }
                a.orr(quotient, small_tag, TMP3, arm::asr(shift));
            }
            if (need_rem) {
                Uint mask = (Uint)-1 << (shift + _TAG_IMMED1_SIZE);
                comment("optimized rem by replacing with subtraction");
                a.and_(TMP1, TMP3, imm(mask));
                a.sub(remainder, original_dividend, TMP1);
            }
        }
    } else {
        a.asr(TMP1, dividend, imm(_TAG_IMMED1_SIZE));
        mov_imm(TMP2, divisor);
        a.sdiv(quotient, TMP1, TMP2);
        if (need_rem) {
            a.msub(remainder, quotient, TMP2, TMP1);
        }

        if (small_dividend) {
            mov_imm(small_tag, _TAG_IMMED1_SMALL);
        }
        const arm::Shift tagShift = arm::lsl(_TAG_IMMED1_SIZE);
        if (need_div) {
            a.orr(quotient, small_tag, quotient, tagShift);
        }
        if (need_rem) {
            a.orr(remainder, small_tag, remainder, tagShift);
        }
    }
}

void BeamModuleAssembler::emit_div_rem(const ArgLabel &Fail,
                                       const ArgSource &LHS,
                                       const ArgSource &RHS,
                                       const ErtsCodeMFA *error_mfa,
                                       const ArgRegister &Quotient,
                                       const ArgRegister &Remainder,
                                       bool need_div,
                                       bool need_rem) {
    Sint divisor = 0;

    if (RHS.isSmall()) {
        divisor = RHS.as<ArgSmall>().getSigned();
        if (divisor == -1) {
            divisor = 0;
        }
    }

    if (always_small(LHS) && divisor != 0) {
        auto lhs = load_source(LHS, ARG3);
        auto quotient = init_destination(Quotient, ARG1);
        auto remainder = init_destination(Remainder, ARG2);
        Label invalidLabel; /* Intentionally not initialized */

        comment("skipped test for smalls operands and overflow");
        emit_div_rem_literal(divisor,
                             LHS,
                             lhs.reg,
                             quotient.reg,
                             remainder.reg,
                             invalidLabel,
                             need_div,
                             need_rem);
        if (need_div) {
            flush_var(quotient);
        }
        if (need_rem) {
            flush_var(remainder);
        }
    } else {
        Label generic = a.newLabel(), done = a.newLabel();
        auto [lhs, rhs] = load_sources(LHS, ARG2, RHS, ARG3);

        if (divisor != (Sint)0) {
            emit_div_rem_literal(divisor,
                                 LHS,
                                 lhs.reg,
                                 ARG1,
                                 ARG2,
                                 generic,
                                 need_div,
                                 need_rem);
            a.b(done);
        }

        a.bind(generic);
        mov_var(ARG2, lhs);
        mov_var(ARG3, rhs);
        if (Fail.get() != 0) {
            fragment_call(ga->get_int_div_rem_guard_shared());
            a.b_eq(resolve_beam_label(Fail, disp1MB));
        } else {
            a.mov(ARG4, imm(error_mfa));
            fragment_call(ga->get_int_div_rem_body_shared());
        }

        a.bind(done);
        if (need_div) {
            mov_arg(Quotient, ARG1);
        }
        if (need_rem) {
            mov_arg(Remainder, ARG2);
        }
    }
}

void BeamModuleAssembler::emit_i_rem_div(const ArgLabel &Fail,
                                         const ArgWord &Live,
                                         const ArgSource &LHS,
                                         const ArgSource &RHS,
                                         const ArgRegister &Remainder,
                                         const ArgRegister &Quotient) {
    static const ErtsCodeMFA bif_mfa = {am_erlang, am_rem, 2};
    bool need_rem = Quotient != Remainder;

    emit_div_rem(Fail, LHS, RHS, &bif_mfa, Quotient, Remainder, true, need_rem);
}

void BeamModuleAssembler::emit_i_div_rem(const ArgLabel &Fail,
                                         const ArgWord &Live,
                                         const ArgSource &LHS,
                                         const ArgSource &RHS,
                                         const ArgRegister &Quotient,
                                         const ArgRegister &Remainder) {
    static const ErtsCodeMFA bif_mfa = {am_erlang, am_div, 2};
    bool need_div = Quotient != Remainder;

    emit_div_rem(Fail, LHS, RHS, &bif_mfa, Quotient, Remainder, need_div, true);
}

void BeamModuleAssembler::emit_i_int_div(const ArgLabel &Fail,
                                         const ArgWord &Live,
                                         const ArgSource &LHS,
                                         const ArgSource &RHS,
                                         const ArgRegister &Quotient) {
    static const ErtsCodeMFA bif_mfa = {am_erlang, am_div, 2};
    ArgYRegister Dummy(0);

    emit_div_rem(Fail, LHS, RHS, &bif_mfa, Quotient, Dummy, true, false);
}

void BeamModuleAssembler::emit_i_rem(const ArgLabel &Fail,
                                     const ArgWord &Live,
                                     const ArgSource &LHS,
                                     const ArgSource &RHS,
                                     const ArgRegister &Remainder) {
    static const ErtsCodeMFA bif_mfa = {am_erlang, am_rem, 2};
    ArgYRegister Dummy(0);

    emit_div_rem(Fail, LHS, RHS, &bif_mfa, Dummy, Remainder, false, true);
}

void BeamModuleAssembler::emit_i_m_div(const ArgLabel &Fail,
                                       const ArgWord &Live,
                                       const ArgSource &LHS,
                                       const ArgSource &RHS,
                                       const ArgRegister &Dst) {
    static const ErtsCodeMFA bif_mfa = {am_erlang, am_Div, 2};

    Label next = a.newLabel();

    auto [lhs, rhs] = load_sources(LHS, ARG2, RHS, ARG3);

    mov_var(ARG2, lhs);
    mov_var(ARG3, rhs);

    emit_enter_runtime(Live.get());

    a.mov(ARG1, c_p);
    runtime_call<3>(erts_mixed_div);

    emit_leave_runtime(Live.get());

    a.cmp(ARG1, imm(THE_NON_VALUE));

    if (Fail.get() != 0) {
        a.b_eq(resolve_beam_label(Fail, disp1MB));
    } else {
        a.b_ne(next);

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

void BeamModuleAssembler::emit_i_band(const ArgLabel &Fail,
                                      const ArgWord &Live,
                                      const ArgSource &LHS,
                                      const ArgSource &RHS,
                                      const ArgRegister &Dst) {
    if (always_small(LHS) && RHS.isSmall()) {
        a64::Utils::LogicalImm ignore;
        if (a64::Utils::encodeLogicalImm(RHS.as<ArgSmall>().get(),
                                         64,
                                         &ignore)) {
            comment("skipped test for small operands since they are always "
                    "small");
            auto lhs = load_source(LHS);
            auto dst = init_destination(Dst, ARG1);

            /* TAG & TAG = TAG, so we don't need to tag it again. */
            a.and_(dst.reg, lhs.reg, RHS.as<ArgSmall>().get());
            flush_var(dst);
            return;
        }
    }

    auto [lhs, rhs] = load_sources(LHS, ARG2, RHS, ARG3);
    auto dst = init_destination(Dst, ARG1);

    if (always_small(LHS) && always_small(RHS)) {
        comment("skipped test for small operands since they are always small");

        /* TAG & TAG = TAG, so we don't need to tag it again. */
        a.and_(dst.reg, lhs.reg, rhs.reg);
        flush_var(dst);
    } else {
        Label next = a.newLabel();

        if (RHS.isLiteral()) {
            comment("skipped test for small because one operand is never "
                    "small");
        } else {
            /* TAG & TAG = TAG, so we don't need to tag it again. */
            a.and_(ARG1, lhs.reg, rhs.reg);

            ERTS_CT_ASSERT(_TAG_IMMED1_SMALL == _TAG_IMMED1_MASK);
            if (always_one_of<BeamTypeId::Integer, BeamTypeId::AlwaysBoxed>(
                        LHS) &&
                always_one_of<BeamTypeId::Integer, BeamTypeId::AlwaysBoxed>(
                        RHS)) {
                comment("simplified test for small operands since other types "
                        "are boxed");
                emit_is_boxed(next, ARG1);
            } else {
                /* All other term types has at least one zero in the low 4
                 * bits. Therefore, the result will be a small iff both
                 * operands are small. */
                a.and_(TMP1, ARG1, imm(_TAG_IMMED1_MASK));
                a.cmp(TMP1, imm(_TAG_IMMED1_SMALL));
                a.b_eq(next);
            }
        }

        mov_var(ARG2, lhs);
        mov_var(ARG3, rhs);

        if (Fail.get() != 0) {
            emit_enter_runtime(Live.get());
            a.mov(ARG1, c_p);
            runtime_call<3>(erts_band);
            emit_leave_runtime(Live.get());
            emit_branch_if_not_value(ARG1,
                                     resolve_beam_label(Fail, dispUnknown));
        } else {
            emit_enter_runtime(Live.get());
            fragment_call(ga->get_i_band_body_shared());
            emit_leave_runtime(Live.get());
        }

        a.bind(next);
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

void BeamModuleAssembler::emit_i_bor(const ArgLabel &Fail,
                                     const ArgWord &Live,
                                     const ArgSource &LHS,
                                     const ArgSource &RHS,
                                     const ArgRegister &Dst) {
    if (always_small(LHS) && RHS.isSmall()) {
        a64::Utils::LogicalImm ignore;
        Uint64 rhs = RHS.as<ArgSmall>().get() & ~_TAG_IMMED1_SMALL;
        if (a64::Utils::encodeLogicalImm(rhs, 64, &ignore)) {
            comment("skipped test for small operands since they are always "
                    "small");
            auto lhs = load_source(LHS);
            auto dst = init_destination(Dst, ARG1);

            a.orr(dst.reg, lhs.reg, rhs);
            flush_var(dst);
            return;
        }
    }

    auto [lhs, rhs] = load_sources(LHS, ARG2, RHS, ARG3);
    auto dst = init_destination(Dst, ARG1);

    if (always_small(LHS) && always_small(RHS)) {
        comment("skipped test for small operands since they are always small");

        /* TAG | TAG = TAG, so we don't need to tag it again. */
        a.orr(dst.reg, lhs.reg, rhs.reg);
        flush_var(dst);
    } else {
        Label next = a.newLabel();

        /* TAG | TAG = TAG, so we don't need to tag it again. */
        a.orr(ARG1, lhs.reg, rhs.reg);

        emit_are_both_small(LHS, lhs.reg, RHS, rhs.reg, next);

        mov_var(ARG2, lhs);
        mov_var(ARG3, rhs);

        if (Fail.get() != 0) {
            emit_enter_runtime(Live.get());
            a.mov(ARG1, c_p);
            runtime_call<3>(erts_bor);
            emit_leave_runtime(Live.get());
            emit_branch_if_not_value(ARG1,
                                     resolve_beam_label(Fail, dispUnknown));
        } else {
            emit_enter_runtime(Live.get());
            fragment_call(ga->get_i_bor_body_shared());
            emit_leave_runtime(Live.get());
        }

        a.bind(next);
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

void BeamModuleAssembler::emit_i_bxor(const ArgLabel &Fail,
                                      const ArgWord &Live,
                                      const ArgSource &LHS,
                                      const ArgSource &RHS,
                                      const ArgRegister &Dst) {
    auto [lhs, rhs] = load_sources(LHS, ARG2, RHS, ARG3);
    auto dst = init_destination(Dst, ARG1);

    if (always_small(LHS) && always_small(RHS)) {
        comment("skipped test for small operands because they are always "
                "small");

        /* TAG ^ TAG = 0, so we'll need to tag it again. */
        a.eor(dst.reg, lhs.reg, rhs.reg);
        a.orr(dst.reg, dst.reg, imm(_TAG_IMMED1_SMALL));
        flush_var(dst);
        return;
    }

    Label next = a.newLabel();

    /* TAG ^ TAG = 0, so we'll need to tag it again. */
    a.eor(ARG1, lhs.reg, rhs.reg);
    a.orr(ARG1, ARG1, imm(_TAG_IMMED1_SMALL));

    emit_are_both_small(LHS, lhs.reg, RHS, rhs.reg, next);

    mov_var(ARG2, lhs);
    mov_var(ARG3, rhs);

    if (Fail.get() != 0) {
        emit_enter_runtime(Live.get());
        a.mov(ARG1, c_p);
        runtime_call<3>(erts_bxor);
        emit_leave_runtime(Live.get());
        emit_branch_if_not_value(ARG1, resolve_beam_label(Fail, dispUnknown));
    } else {
        emit_enter_runtime(Live.get());
        fragment_call(ga->get_i_bxor_body_shared());
        emit_leave_runtime(Live.get());
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

void BeamModuleAssembler::emit_i_bnot(const ArgLabel &Fail,
                                      const ArgWord &Live,
                                      const ArgSource &Src,
                                      const ArgRegister &Dst) {
    Label next = a.newLabel();
    auto src = load_source(Src, TMP2);
    auto dst = init_destination(Dst, ARG1);

    /* Invert everything except the tag so we don't have to tag it again. */
    a.eor(ARG1, src.reg, imm(~_TAG_IMMED1_MASK));

    if (always_one_of<BeamTypeId::Number>(Src)) {
        comment("simplified test for small operand since it is a number");
        emit_is_boxed(next, Src, ARG1);
    } else {
        a.and_(TMP1, src.reg, imm(_TAG_IMMED1_MASK));
        a.cmp(TMP1, imm(_TAG_IMMED1_SMALL));
        a.b_eq(next);
    }

    if (Fail.get() != 0) {
        emit_enter_runtime(Live.get());
        fragment_call(ga->get_i_bnot_guard_shared());
        emit_leave_runtime(Live.get());
        emit_branch_if_not_value(ARG1, resolve_beam_label(Fail, dispUnknown));
    } else {
        emit_enter_runtime(Live.get());
        fragment_call(ga->get_i_bnot_body_shared());
        emit_leave_runtime(Live.get());
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

void BeamModuleAssembler::emit_i_bsr(const ArgLabel &Fail,
                                     const ArgWord &Live,
                                     const ArgSource &LHS,
                                     const ArgSource &RHS,
                                     const ArgRegister &Dst) {
    Label generic = a.newLabel(), next = a.newLabel();
    auto lhs = load_source(LHS, ARG2);
    auto dst = init_destination(Dst, ARG1);
    bool need_generic = true;

    if (RHS.isSmall()) {
        Sint shift = RHS.as<ArgSmall>().getSigned();

        if (shift >= 0) {
            a64::Gp small_tag = TMP1;
            if (always_small(LHS)) {
                comment("skipped test for small left operand because it is "
                        "always small");
                need_generic = false;
                mov_imm(small_tag, _TAG_IMMED1_SMALL);
            } else if (always_one_of<BeamTypeId::Number>(LHS)) {
                comment("simplified test for small operand since it is a "
                        "number");
                emit_is_not_boxed(generic, lhs.reg);
                mov_imm(small_tag, _TAG_IMMED1_SMALL);
            } else {
                a.and_(small_tag, lhs.reg, imm(_TAG_IMMED1_MASK));
                a.cmp(small_tag, imm(_TAG_IMMED1_SMALL));
                a.b_ne(generic);
            }

            /* We don't need to clear the mask after shifting because
             * _TAG_IMMED1_SMALL will set all the bits anyway. */
            ERTS_CT_ASSERT(_TAG_IMMED1_MASK == _TAG_IMMED1_SMALL);
            shift = std::min<Sint>(shift, 63);
            a.orr(dst.reg, small_tag, lhs.reg, arm::asr(shift));

            if (need_generic) {
                a.b(next);
            }
        } else {
            /* Constant shift is negative; fall back to the generic
             * path. */
        }
    } else {
        auto rhs = load_source(RHS, ARG3);

        /* Ensure that both operands are small and that the shift
         * count is positive. */
        ERTS_CT_ASSERT(_TAG_IMMED1_SMALL == _TAG_IMMED1_MASK);
        a.ands(TMP1, rhs.reg, imm((1ull << 63) | _TAG_IMMED1_MASK));
        a.and_(TMP1, lhs.reg, TMP1);
        a.ccmp(TMP1,
               imm(_TAG_IMMED1_SMALL),
               imm(NZCV::kNone),
               arm::CondCode::kPL);
        a.b_ne(generic);

        /* Calculate shift count. */
        a.asr(TMP1, rhs.reg, imm(_TAG_IMMED1_SIZE));
        mov_imm(TMP2, 63);
        a.cmp(TMP1, TMP2);
        a.csel(TMP1, TMP1, TMP2, imm(arm::CondCode::kLE));

        /* Shift right. */
        ERTS_CT_ASSERT(_TAG_IMMED1_MASK == _TAG_IMMED1_SMALL);
        a.asr(dst.reg, lhs.reg, TMP1);
        a.orr(dst.reg, dst.reg, imm(_TAG_IMMED1_SMALL));
        a.b(next);
    }

    a.bind(generic);
    if (need_generic) {
        mov_var(ARG2, lhs);
        mov_arg(ARG3, RHS);

        if (Fail.get() != 0) {
            emit_enter_runtime(Live.get());
            a.mov(ARG1, c_p);
            runtime_call<3>(erts_bsr);
            emit_leave_runtime(Live.get());
            emit_branch_if_not_value(ARG1,
                                     resolve_beam_label(Fail, dispUnknown));
        } else {
            emit_enter_runtime(Live.get());
            fragment_call(ga->get_i_bsr_body_shared());
            emit_leave_runtime(Live.get());
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

void BeamModuleAssembler::emit_i_bsl(const ArgLabel &Fail,
                                     const ArgWord &Live,
                                     const ArgSource &LHS,
                                     const ArgSource &RHS,
                                     const ArgRegister &Dst) {
    auto dst = init_destination(Dst, ARG1);

    if (is_bsl_small(LHS, RHS)) {
        comment("skipped tests because operands and result are always small");
        if (RHS.isSmall()) {
            auto lhs = load_source(LHS);
            a.and_(TMP1, lhs.reg, imm(~_TAG_IMMED1_MASK));
            a.lsl(TMP1, TMP1, imm(RHS.as<ArgSmall>().getSigned()));
        } else {
            auto [lhs, rhs] = load_sources(LHS, ARG2, RHS, ARG3);
            a.and_(TMP1, lhs.reg, imm(~_TAG_IMMED1_MASK));
            a.lsr(TMP2, rhs.reg, imm(_TAG_IMMED1_SIZE));
            a.lsl(TMP1, TMP1, TMP2);
        }
        a.orr(dst.reg, TMP1, imm(_TAG_IMMED1_SMALL));
        flush_var(dst);
        return;
    }

    auto [lhs, rhs] = load_sources(LHS, ARG2, RHS, ARG3);
    Label generic = a.newLabel(), next = a.newLabel();
    bool inline_shift = true;

    if (LHS.isImmed() && RHS.isImmed()) {
        /* The compiler should've optimized this away, so we'll fall
         * back to the generic path to simplify the inline
         * implementation. */
        inline_shift = false;
    } else if (LHS.isLiteral() || RHS.isLiteral()) {
        /* At least one argument is not a small. */
        inline_shift = false;
    } else if (LHS.isImmed() && !LHS.isSmall()) {
        /* Invalid constant. */
        inline_shift = false;
    } else if (RHS.isImmed() &&
               (!RHS.isSmall() || RHS.as<ArgSmall>().getSigned() < 0 ||
                RHS.as<ArgSmall>().getSigned() >= SMALL_BITS - 1)) {
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

            if (always_small(LHS)) {
                comment("skipped test for small operand since it is always "
                        "small");
            } else if (always_one_of<BeamTypeId::Number>(LHS)) {
                comment("simplified test for small operand since it is a "
                        "number");
                emit_is_not_boxed(generic, TMP1);
            } else {
                a.and_(TMP1, lhs.reg, imm(_TAG_IMMED1_MASK));
                a.cmp(TMP1, imm(_TAG_IMMED1_SMALL));
                a.b_ne(generic);
            }
        } else {
            UWord value = LHS.as<ArgSmall>().get();

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
            a.b_hi(generic);
        } else {
            ASSERT(!shiftLimit.isImm());

            shiftCount = imm(RHS.as<ArgSmall>().getSigned());

            a.emit(a64::Inst::kIdCmp, shiftLimit, shiftCount);
            a.b_lo(generic);
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

        if (Fail.get() != 0) {
            emit_enter_runtime(Live.get());
            a.mov(ARG1, c_p);
            runtime_call<3>(erts_bsl);
            emit_leave_runtime(Live.get());
            emit_branch_if_not_value(ARG1,
                                     resolve_beam_label(Fail, dispUnknown));
        } else {
            emit_enter_runtime(Live.get());
            fragment_call(ga->get_i_bsl_body_shared());
            emit_leave_runtime(Live.get());
        }

        mov_var(dst, ARG1);
        flush_var(dst);
    }

    a.bind(next);
}
