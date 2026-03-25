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
                                             const a32::Gp lhs_reg,
                                             const ArgSource &RHS,
                                             const a32::Gp rhs_reg,
                                             const Label next) {
    if (is_small_result) {
        comment("skipped overflow test because the result is always small");
        emit_are_both_small(LHS, lhs_reg, RHS, rhs_reg, next);
    } else if (RHS.isLiteral()) {
        /* Skipping test for small */
    } else {
        Label overflow = a.newLabel();

        if (always_small(RHS)) {
            a.and_(TMP, lhs_reg, imm(_TAG_IMMED1_MASK));
        } else if (always_small(LHS)) {
            a.and_(TMP, rhs_reg, imm(_TAG_IMMED1_MASK));
        } else {
            ERTS_CT_ASSERT(_TAG_IMMED1_SMALL == _TAG_IMMED1_MASK);
            a.and_(TMP, lhs_reg, rhs_reg);
            a.and_(TMP, TMP, imm(_TAG_IMMED1_MASK));
        }

        comment("test for not overflow and small operands");
        a.b_vs(overflow);
        a.cmp(TMP, imm(_TAG_IMMED1_SMALL));
        a.b_eq(next);
        a.bind(overflow);
    }
}

void BeamModuleAssembler::emit_are_both_small(const ArgSource &LHS,
                                              const a32::Gp lhs_reg,
                                              const ArgSource &RHS,
                                              const a32::Gp rhs_reg,
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
        a.and_(TMP, lhs_reg, rhs_reg);
        emit_is_boxed(next, TMP);
    } else {
        if (always_small(RHS)) {
            a.and_(TMP, lhs_reg, imm(_TAG_IMMED1_MASK));
        } else if (always_small(LHS)) {
            a.and_(TMP, rhs_reg, imm(_TAG_IMMED1_MASK));
        } else {
            ERTS_CT_ASSERT(_TAG_IMMED1_SMALL == _TAG_IMMED1_MASK);
            a.and_(TMP, lhs_reg, rhs_reg);
            a.and_(TMP, TMP, imm(_TAG_IMMED1_MASK));
        }
        a.cmp(TMP, imm(_TAG_IMMED1_SMALL));
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
    a.str(ARG2, TMP_MEM1q);
    a.str(ARG3, TMP_MEM2q);

    emit_enter_runtime_frame();

    a.mov(ARG1, c_p);
    runtime_call<3>(erts_mixed_plus);

    emit_leave_runtime_frame();

    emit_branch_if_not_value(ARG1, error);
    a.bx(a32::lr);

    a.bind(error);
    {
        /* emit_enter_runtime() was done in the module code. */
        emit_leave_runtime();

        /* Place the original arguments in X registers. */
        a.ldr(ARG1, TMP_MEM1q);
        a.str(ARG1, getXRef(0));
        a.ldr(ARG1, TMP_MEM2q);
        a.str(ARG1, getXRef(1));
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
            a.bic(TMP, rhs.reg, imm(_TAG_IMMED1_MASK));
            a.add(dst.reg, lhs.reg, TMP);
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
        a.bic(TMP, rhs.reg, imm(_TAG_IMMED1_MASK));
        a.adds(ARG1, lhs.reg, TMP);
    }

    emit_add_sub_types(is_small_result, LHS, lhs.reg, RHS, rhs.reg, next);

    mov_var(ARG2, lhs);
    mov_var(ARG3, rhs);

    if (Fail.get() != 0) {
        emit_enter_runtime();
        a.mov(ARG1, c_p);
        runtime_call<3>(erts_mixed_plus);
        emit_leave_runtime();

        emit_branch_if_not_value(ARG1, resolve_beam_label(Fail, dispUnknown));
    } else {
        emit_enter_runtime();
        fragment_call(ga->get_plus_body_shared());
        emit_leave_runtime();
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
    static const ErtsCodeMFA bif_mfa = {am_erlang, am_Minus, 1};
    Label error = a.newLabel();

    /* Save original argument for the error path. */
    a.str(ARG2, TMP_MEM1q);

    emit_enter_runtime_frame();

    a.mov(ARG1, c_p);
    runtime_call<2>(erts_unary_minus);

    emit_leave_runtime_frame();

    emit_branch_if_not_value(ARG1, error);
    a.bx(a32::lr);

    a.bind(error);
    {
        /* emit_enter_runtime() was done in the module code. */
        emit_leave_runtime();

        /* Place the original argument in X0. */
        a.ldr(VAR, TMP_MEM1q);
        a.str(VAR, getXRef(0));
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

    if (always_small(Src) && is_small_result) {
        auto dst = init_destination(Dst, ARG1);
        comment("no overflow test because result is always small");
        mov_imm(TMP, _TAG_IMMED1_SMALL);
        a.bic(VAR, src.reg, imm(_TAG_IMMED1_MASK));
        a.sub(dst.reg, TMP, VAR);
        flush_var(dst);
        return;
    }

    Label next = a.newLabel(), overflow = a.newLabel();

    mov_imm(TMP, _TAG_IMMED1_SMALL);
    a.bic(VAR, src.reg, imm(_TAG_IMMED1_MASK));
    a.subs(ARG1, TMP, VAR);

    /* Test for not overflow AND small operand. */
    a.b_vs(overflow);
    a.and_(TMP, src.reg, imm(_TAG_IMMED1_MASK));
    a.cmp(TMP, imm(_TAG_IMMED1_SMALL));
    a.b_eq(next);
    a.bind(overflow);

    mov_var(ARG2, src);

    if (Fail.get() != 0) {
        emit_enter_runtime();
        a.mov(ARG1, c_p);
        runtime_call<2>(erts_unary_minus);
        emit_leave_runtime();

        emit_branch_if_not_value(ARG1, resolve_beam_label(Fail, dispUnknown));
    } else {
        emit_enter_runtime();
        fragment_call(ga->get_unary_minus_body_shared());
        emit_leave_runtime();
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
    lea(TMP, TMP_MEM1q);
    a.stmia(arm::Mem(TMP), a32::GpList({ARG2, ARG3}));

    emit_enter_runtime_frame();

    a.mov(ARG1, c_p);
    runtime_call<3>(erts_mixed_minus);

    emit_leave_runtime_frame();

    emit_branch_if_not_value(ARG1, error);

    a.bx(a32::lr);

    a.bind(error);
    {
        /* emit_enter_runtime() was done in the module code. */
        emit_leave_runtime();

        /* Place the original arguments in X registers. */
        lea(ARG4, TMP_MEM1q);
        a.ldmia(arm::Mem(ARG4), a32::GpList({VAR, TMP}));
        a.stmia(arm::Mem(scheduler_registers), a32::GpList({VAR, TMP}));
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
            a.bic(TMP, rhs.reg, imm(_TAG_IMMED1_MASK));
            a.sub(dst.reg, lhs.reg, TMP);
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
        a.bic(TMP, rhs.reg, imm(_TAG_IMMED1_MASK));
        a.subs(ARG1, lhs.reg, TMP);
    }

    emit_add_sub_types(is_small_result, LHS, lhs.reg, RHS, rhs.reg, next);

    mov_var(ARG2, lhs);
    mov_var(ARG3, rhs);

    if (Fail.get() != 0) {
        emit_enter_runtime();
        a.mov(ARG1, c_p);
        runtime_call<3>(erts_mixed_minus);
        emit_leave_runtime();
        emit_branch_if_not_value(ARG1, resolve_beam_label(Fail, dispUnknown));
    } else {
        emit_enter_runtime();
        fragment_call(ga->get_minus_body_shared());
        emit_leave_runtime();
    }

    a.bind(next);
    mov_arg(Dst, ARG1);

    (void)Live;
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
    // TODO
    emit_nyi("emit_int128_to_big_shared");
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
    a.str(ARG2, TMP_MEM1q);
    a.str(ARG3, TMP_MEM2q);
    a.mov(ARG1, c_p);
    a.cmp(ARG4, imm(make_small(0)));
    a.b_eq(mul_only);
    a.str(ARG4, TMP_MEM4q);

    lea(TMP, TMP_MEM3q);
    a.sub(a32::sp, a32::sp, imm(8));
    a.str(TMP, arm::Mem(a32::sp, 0));
    runtime_call<5>(erts_mul_add);
    a.add(a32::sp, a32::sp, imm(8));

    emit_leave_runtime();
    emit_leave_runtime_frame();

    emit_branch_if_not_value(ARG1, error);
    a.bx(a32::lr);

    a.bind(mul_only);
    {
        runtime_call<3>(erts_mixed_times);

        emit_leave_runtime();
        emit_leave_runtime_frame();

        emit_branch_if_not_value(ARG1, mul_error);
        a.bx(a32::lr);
    }

    a.bind(error);
    {
        static const ErtsCodeMFA mul_mfa = {am_erlang, am_Times, 2};
        static const ErtsCodeMFA add_mfa = {am_erlang, am_Plus, 2};

        a.ldr(ARG1, TMP_MEM3q);
        a.str(ARG1, getXRef(0));
        a.ldr(ARG1, TMP_MEM4q);
        a.str(ARG1, getXRef(1));
        mov_imm(ARG4, &add_mfa);
        emit_branch_if_value(ARG1, do_error);

        a.bind(mul_error);
        a.ldr(ARG1, TMP_MEM1q);
        a.str(ARG1, getXRef(0));
        a.ldr(ARG1, TMP_MEM2q);
        a.str(ARG1, getXRef(1));
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

    emit_enter_runtime_frame();
    emit_enter_runtime();

    a.str(ARG4, TMP_MEM1q);

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

    a.bx(a32::lr);
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
        a.bic(TMP, src1.reg, imm(_TAG_IMMED1_MASK));
        if (left_shift > 0) {
            comment("optimized multiplication by replacing with left shift");
            a.add(dst.reg, src4.reg, TMP, arm::lsl(left_shift));
        } else {
            mov_imm(VAR, factor);
            a.mul(dst.reg, TMP, VAR);
            a.add(dst.reg, dst.reg, src4.reg);
        }
        flush_var(dst);
        return;
    }

    Label mixed = a.newLabel(), next = a.newLabel();
    auto [src1, src2] = load_sources(Src1, ARG2, Src2, ARG3);
    auto src4 = load_source(ArgXRegister(0), ARG4);

    if (!is_increment_zero) {
        src4 = load_source(Src4, ARG4);
    }

    /* Preserve original arguments for the mixed fallback path. */
    mov_var(ARG2, src1);
    mov_var(ARG3, src2);
    if (!is_increment_zero) {
        mov_var(ARG4, src4);
    }

    if (sometimes_small) {
        Label small = a.newLabel();
        if (always_small(Src1) && always_small(Src2) && always_small(Src4)) {
            comment("skipped test for small operands since they are always "
                    "small");
            a.b(small);
        } else {
            if (always_small(Src4)) {
                emit_are_both_small(Src1, src1.reg, Src2, src2.reg, small);
            } else if (always_small(Src2)) {
                emit_are_both_small(Src1, src1.reg, Src4, src4.reg, small);
            } else {
                ASSERT(!is_increment_zero);
                ERTS_CT_ASSERT(_TAG_IMMED1_SMALL == _TAG_IMMED1_MASK);
                a.and_(TMP, src1.reg, src2.reg);
                a.and_(TMP, TMP, src4.reg);
                if (always_one_of<BeamTypeId::Integer, BeamTypeId::AlwaysBoxed>(
                            Src1) &&
                    always_one_of<BeamTypeId::Integer, BeamTypeId::AlwaysBoxed>(
                            Src2) &&
                    always_one_of<BeamTypeId::Integer, BeamTypeId::AlwaysBoxed>(
                            Src4)) {
                    emit_is_boxed(mixed, TMP);
                    a.b(small);
                } else {
                    a.and_(TMP, TMP, imm(_TAG_IMMED1_MASK));
                    a.cmp(TMP, imm(_TAG_IMMED1_SMALL));
                    a.b_eq(small);
                    a.b_ne(mixed);
                }
            }
        }

        a.bind(small);

        /* Small-only fast path: keep logic conservative and fall back when
         * range analysis doesn't guarantee a small result. */
        if (!(is_product_small && is_sum_small)) {
            a.b(mixed);
        }

        if (is_increment_zero) {
            mov_imm(ARG4, make_small(0));
        }

        a.bic(TMP, src1.reg, imm(_TAG_IMMED1_MASK));
        if (left_shift > 0) {
            comment("optimized multiplication by replacing with left shift");
            a.add(ARG1, ARG4, TMP, arm::lsl(left_shift));
        } else {
            if (Src2.isSmall()) {
                mov_imm(VAR, factor);
            } else {
                a.asr(VAR, src2.reg, imm(_TAG_IMMED1_SIZE));
            }
            a.mul(ARG1, TMP, VAR);
            a.add(ARG1, ARG1, ARG4);
        }

        a.b(next);
    }

    /* Mixed multiplication/addition fallback. */
    a.bind(mixed);
    {
        if (Fail.get() != 0) {
            if (is_increment_zero) {
                fragment_call(ga->get_mul_guard_shared());
            } else {
                fragment_call(ga->get_mul_add_guard_shared());
            }
            emit_branch_if_not_value(ARG1, resolve_beam_label(Fail, dispUnknown));
        } else {
            if (is_increment_zero) {
                fragment_call(ga->get_mul_body_shared());
            } else {
                fragment_call(ga->get_mul_add_body_shared());
            }
        }
    }

    a.bind(next);
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
    a.asr(TMP, ARG2, imm(_TAG_IMMED1_SIZE)); /* lhs int */
    a.asr(VAR, ARG3, imm(_TAG_IMMED1_SIZE)); /* rhs int */
    a.sdiv(ARG4, TMP, VAR);                  /* quotient int */
    a.mul(ARG1, ARG4, VAR);
    a.sub(VAR, TMP, ARG1);                   /* remainder int */

    a.cmp(ARG3, imm(make_small(0)));
    a.b_eq(exit);

    /* Check whether both operands are small integers. */
    ERTS_CT_ASSERT(_TAG_IMMED1_SMALL == _TAG_IMMED1_MASK);
    a.and_(TMP, ARG2, ARG3);
    a.and_(TMP, TMP, imm(_TAG_IMMED1_MASK));
    a.cmp(TMP, imm(_TAG_IMMED1_SMALL));
    a.b_ne(generic);

    /* MIN_SMALL divided by -1 will overflow and must use generic path. */
    a.asr(TMP, ARG4, imm(SMALL_BITS - 1));
    a.cmp(TMP, imm(1));
    a.b_ge(generic);

    /* The Z flag is now clear (meaning no error). */
    a.lsl(ARG1, ARG4, imm(_TAG_IMMED1_SIZE));
    a.orr(ARG1, ARG1, imm(_TAG_IMMED1_SMALL));
    a.lsl(ARG2, VAR, imm(_TAG_IMMED1_SIZE));
    a.orr(ARG2, ARG2, imm(_TAG_IMMED1_SMALL));

    a.bind(exit);
    {
        a.bx(a32::lr);
    }

    a.bind(generic);
    {
        emit_enter_runtime_frame();
        emit_enter_runtime();

        a.mov(ARG1, c_p);
        lea(ARG4, TMP_MEM4q); /* quotient out */
        lea(TMP, TMP_MEM5q);  /* remainder out */
        a.sub(a32::sp, a32::sp, imm(8)); /* keep AAPCS alignment */
        a.str(TMP, arm::Mem(a32::sp, 0)); /* arg5 */
        runtime_call<5>(erts_int_div_rem);
        a.add(a32::sp, a32::sp, imm(8));

        emit_leave_runtime();
        emit_leave_runtime_frame();

        a.tst(ARG1, ARG1);
        a.ldr(ARG1, TMP_MEM4q);
        a.ldr(ARG2, TMP_MEM5q);

        a.bx(a32::lr);
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
    a.asr(VAR, ARG2, imm(_TAG_IMMED1_SIZE)); /* lhs int */
    a.asr(TMP, ARG3, imm(_TAG_IMMED1_SIZE)); /* rhs int */
    a.sdiv(ARG1, VAR, TMP);                  /* quotient int */

    a.cmp(ARG3, imm(make_small(0)));
    a.b_eq(div_zero);

    /* Check whether both operands are small integers. */
    ERTS_CT_ASSERT(_TAG_IMMED1_SMALL == _TAG_IMMED1_MASK);
    a.and_(TMP, ARG2, ARG3);
    a.and_(TMP, TMP, imm(_TAG_IMMED1_MASK));
    a.cmp(TMP, imm(_TAG_IMMED1_SMALL));
    a.b_ne(generic_div);

    /* MIN_SMALL divided by -1 will overflow and must use generic path. */
    a.asr(TMP, ARG1, imm(SMALL_BITS - 1));
    a.cmp(TMP, imm(1));
    a.b_ge(generic_div);

    a.asr(TMP, ARG3, imm(_TAG_IMMED1_SIZE)); /* rhs int */
    a.mul(ARG2, ARG1, TMP);
    a.sub(ARG2, VAR, ARG2);                  /* remainder int */
    a.lsl(ARG1, ARG1, imm(_TAG_IMMED1_SIZE));
    a.orr(ARG1, ARG1, imm(_TAG_IMMED1_SMALL));
    a.lsl(ARG2, ARG2, imm(_TAG_IMMED1_SIZE));
    a.orr(ARG2, ARG2, imm(_TAG_IMMED1_SMALL));

    a.bx(a32::lr);

    a.bind(generic_div);
    {
        emit_enter_runtime_frame();
        emit_enter_runtime();

        /* Save MFA and original arguments for the error path. */
        a.str(ARG2, TMP_MEM1q);
        a.str(ARG3, TMP_MEM2q);
        a.str(ARG4, TMP_MEM3q);

        a.mov(ARG1, c_p);
        lea(ARG4, TMP_MEM4q); /* quotient out */
        lea(TMP, TMP_MEM5q);  /* remainder out */
        a.sub(a32::sp, a32::sp, imm(8)); /* keep AAPCS alignment */
        a.str(TMP, arm::Mem(a32::sp, 0)); /* arg5 */
        runtime_call<5>(erts_int_div_rem);
        a.add(a32::sp, a32::sp, imm(8));

        emit_leave_runtime();
        emit_leave_runtime_frame();

        a.tst(ARG1, ARG1);
        a.ldr(ARG1, TMP_MEM4q);
        a.ldr(ARG2, TMP_MEM5q);
        a.b_eq(generic_error);

        a.bx(a32::lr);
    }

    a.bind(div_zero);
    {
        mov_imm(TMP, EXC_BADARITH);
        a.str(TMP, arm::Mem(c_p, offsetof(Process, freason)));
        a.str(ARG2, getXRef(0));
        a.str(ARG3, getXRef(1));
        a.b(labels[raise_exception]);
    }

    a.bind(generic_error);
    {
        a.ldr(ARG1, TMP_MEM1q);
        a.str(ARG1, getXRef(0));
        a.ldr(ARG1, TMP_MEM2q);
        a.str(ARG1, getXRef(1));
        a.ldr(ARG4, TMP_MEM3q); /* MFA */
        a.b(labels[raise_exception]);
    }
}

void BeamModuleAssembler::emit_div_rem_literal(Sint divisor,
                                               const ArgSource &Dividend,
                                               a32::Gp dividend,
                                               a32::Gp quotient,
                                               a32::Gp remainder,
                                               const Label &generic,
                                               bool need_div,
                                               bool need_rem) {
    bool small_dividend = !generic.isValid();

    ASSERT(divisor != (Sint)0);

    if (!small_dividend) {
        a.and_(TMP, dividend, imm(_TAG_IMMED1_MASK));
        a.cmp(TMP, imm(_TAG_IMMED1_SMALL));
        a.b_ne(generic);
    }

    if (Support::isPowerOf2(divisor)) {
        a32::Gp original_dividend = dividend;
        int shift = Support::ctz<Eterm>(divisor);

        ERTS_CT_ASSERT(_TAG_IMMED1_SMALL == _TAG_IMMED1_MASK);
        if (std::get<0>(getClampedRange(Dividend)) >= 0) {
            /* Positive dividend. */
            if (need_div) {
                comment("optimized div by replacing with right shift");
                if (need_rem && quotient == dividend) {
                    original_dividend = ARG4;
                    a.mov(original_dividend, dividend);
                }
                a.mov(quotient, dividend, arm::lsr(shift));
                a.orr(quotient, quotient, imm(_TAG_IMMED1_SMALL));
            }
            if (need_rem) {
                auto mask = Support::lsbMask<Uint>(shift + _TAG_IMMED1_SIZE);
                comment("optimized rem by replacing with masking");
                mov_imm(TMP, mask);
                a.and_(remainder, original_dividend, TMP);
            }
        } else {
            /* Negative dividend. */
            if (need_div) {
                comment("optimized div by replacing with right shift");
            }

            if (divisor == 2) {
                ERTS_CT_ASSERT(_TAG_IMMED1_SMALL == _TAG_IMMED1_MASK);
                a.add(VAR, dividend, dividend, arm::lsr(31));
            } else {
                Label non_negative = a.newLabel();
                add(TMP, dividend, (divisor - 1) << _TAG_IMMED1_SIZE);
                a.mov(VAR, dividend);
                a.cmp(dividend, imm(0));
                a.b_pl(non_negative);
                a.mov(VAR, TMP);
                a.bind(non_negative);
            }

            if (need_div) {
                if (need_rem && quotient == dividend) {
                    original_dividend = ARG4;
                    a.mov(original_dividend, dividend);
                }
                a.mov(quotient, VAR, arm::asr(shift));
                a.orr(quotient, quotient, imm(_TAG_IMMED1_SMALL));
            }
            if (need_rem) {
                Uint mask = (Uint)-1 << (shift + _TAG_IMMED1_SIZE);
                comment("optimized rem by replacing with subtraction");
                mov_imm(TMP, mask);
                a.and_(TMP, VAR, TMP);
                a.sub(remainder, original_dividend, TMP);
            }
        }
    } else {
        a.asr(TMP, dividend, imm(_TAG_IMMED1_SIZE)); /* dividend int */
        mov_imm(VAR, divisor);
        a.sdiv(quotient, TMP, VAR); /* quotient int */
        if (need_rem) {
            a.mul(ARG4, quotient, VAR);
            a.sub(remainder, TMP, ARG4); /* remainder int */
        }

        if (need_div) {
            a.lsl(quotient, quotient, imm(_TAG_IMMED1_SIZE));
            a.orr(quotient, quotient, imm(_TAG_IMMED1_SMALL));
        }
        if (need_rem) {
            a.lsl(remainder, remainder, imm(_TAG_IMMED1_SIZE));
            a.orr(remainder, remainder, imm(_TAG_IMMED1_SMALL));
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
            a.b_eq(resolve_beam_label(Fail, dispUnknown));
        } else {
            mov_imm(ARG4, error_mfa);
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
    // TODO
    emit_nyi("emit_i_m_div");
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

    /* Save original arguments for the error path. */
    a.str(ARG2, TMP_MEM1q);
    a.str(ARG3, TMP_MEM2q);

    emit_enter_runtime_frame();

    a.mov(ARG1, c_p);
    runtime_call<3>(func_ptr);

    emit_leave_runtime_frame();

    emit_branch_if_not_value(ARG1, error);
    a.bx(a32::lr);

    a.bind(error);
    {
        /* emit_enter_runtime() was done in the module code. */
        emit_leave_runtime();

        /* Place the original arguments in X registers. */
        a.ldr(ARG1, TMP_MEM1q);
        a.str(ARG1, getXRef(0));
        a.ldr(ARG1, TMP_MEM2q);
        a.str(ARG1, getXRef(1));
        mov_imm(ARG4, mfa);
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
    auto [lhs, rhs] = load_sources(LHS, ARG2, RHS, ARG3);
    auto dst = init_destination(Dst, ARG1);

    if (always_small(LHS) && always_small(RHS)) {
        comment("skipped test for small operands since they are always small");

        /* TAG & TAG = TAG, so we don't need to tag it again. */
        a.and_(dst.reg, lhs.reg, rhs.reg);
        flush_var(dst);
        return;
    }

    Label next = a.newLabel();

    if (RHS.isLiteral()) {
        comment("skipped test for small because one operand is never small");
    } else {
        /* TAG & TAG = TAG, so we don't need to tag it again. */
        a.and_(ARG1, lhs.reg, rhs.reg);

        ERTS_CT_ASSERT(_TAG_IMMED1_SMALL == _TAG_IMMED1_MASK);
        if (always_one_of<BeamTypeId::Integer, BeamTypeId::AlwaysBoxed>(LHS) &&
            always_one_of<BeamTypeId::Integer, BeamTypeId::AlwaysBoxed>(RHS)) {
            comment("simplified test for small operands since other types are "
                    "boxed");
            emit_is_boxed(next, ARG1);
        } else {
            /* All other term types has at least one zero in the low 4 bits.
             * Therefore, the result will be a small iff both operands are
             * small. */
            a.and_(TMP, ARG1, imm(_TAG_IMMED1_MASK));
            a.cmp(TMP, imm(_TAG_IMMED1_SMALL));
            a.b_eq(next);
        }
    }

    mov_var(ARG2, lhs);
    mov_var(ARG3, rhs);

    if (Fail.get() != 0) {
        emit_enter_runtime();
        a.mov(ARG1, c_p);
        runtime_call<3>(erts_band);
        emit_leave_runtime();
        emit_branch_if_not_value(ARG1, resolve_beam_label(Fail, dispUnknown));
    } else {
        emit_enter_runtime();
        fragment_call(ga->get_i_band_body_shared());
        emit_leave_runtime();
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
    auto [lhs, rhs] = load_sources(LHS, ARG2, RHS, ARG3);
    auto dst = init_destination(Dst, ARG1);

    if (always_small(LHS) && always_small(RHS)) {
        comment("skipped test for small operands since they are always small");

        /* TAG | TAG = TAG, so we don't need to tag it again. */
        a.orr(dst.reg, lhs.reg, rhs.reg);
        flush_var(dst);
        return;
    }

    Label next = a.newLabel();

    /* TAG | TAG = TAG, so we don't need to tag it again. */
    a.orr(ARG1, lhs.reg, rhs.reg);

    emit_are_both_small(LHS, lhs.reg, RHS, rhs.reg, next);

    mov_var(ARG2, lhs);
    mov_var(ARG3, rhs);

    if (Fail.get() != 0) {
        emit_enter_runtime();
        a.mov(ARG1, c_p);
        runtime_call<3>(erts_bor);
        emit_leave_runtime();
        emit_branch_if_not_value(ARG1, resolve_beam_label(Fail, dispUnknown));
    } else {
        emit_enter_runtime();
        fragment_call(ga->get_i_bor_body_shared());
        emit_leave_runtime();
    }

    a.bind(next);
    mov_var(dst, ARG1);
    flush_var(dst);

    (void)Live;
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
        emit_enter_runtime();
        a.mov(ARG1, c_p);
        runtime_call<3>(erts_bxor);
        emit_leave_runtime();
        emit_branch_if_not_value(ARG1, resolve_beam_label(Fail, dispUnknown));
    } else {
        emit_enter_runtime();
        fragment_call(ga->get_i_bxor_body_shared());
        emit_leave_runtime();
    }

    a.bind(next);
    {
        mov_var(dst, ARG1);
        flush_var(dst);
    }

    (void)Live;
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
    mov_imm(TMP, ~_TAG_IMMED1_MASK);
    a.eor(ARG2, ARG1, TMP);

    a.mov(ARG1, c_p);
    runtime_call<2>(erts_bnot);

    emit_leave_runtime_frame();

    a.bx(a32::lr);
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
    static const ErtsCodeMFA bif_mfa = {am_erlang, am_bnot, 1};
    Label error = a.newLabel();

    emit_enter_runtime_frame();

    /* Undo the speculative inversion in module code. */
    mov_imm(TMP, ~_TAG_IMMED1_MASK);
    a.eor(ARG2, ARG1, TMP);

    /* Save original argument for the error path. */
    a.str(ARG2, TMP_MEM1q);

    a.mov(ARG1, c_p);
    runtime_call<2>(erts_bnot);

    emit_leave_runtime_frame();

    emit_branch_if_not_value(ARG1, error);
    a.bx(a32::lr);

    a.bind(error);
    {
        /* emit_enter_runtime() was done in the module code. */
        emit_leave_runtime();

        /* Place the original argument in X register. */
        a.ldr(ARG1, TMP_MEM1q);
        a.str(ARG1, getXRef(0));
        mov_imm(ARG4, &bif_mfa);
        a.b(labels[raise_exception]);
    }
}

void BeamModuleAssembler::emit_i_bnot(const ArgLabel &Fail,
                                      const ArgWord &Live,
                                      const ArgSource &Src,
                                      const ArgRegister &Dst) {
    Label next = a.newLabel();
    auto src = load_source(Src, ARG2);
    auto dst = init_destination(Dst, ARG1);

    /* Invert everything except the tag so we don't have to tag it again. */
    mov_imm(TMP, ~_TAG_IMMED1_MASK);
    a.eor(ARG1, src.reg, TMP);

    if (always_one_of<BeamTypeId::Number>(Src)) {
        comment("simplified test for small operand since it is a number");
        emit_is_boxed(next, Src, ARG1);
    } else {
        a.and_(TMP, src.reg, imm(_TAG_IMMED1_MASK));
        a.cmp(TMP, imm(_TAG_IMMED1_SMALL));
        a.b_eq(next);
    }

    if (Fail.get() != 0) {
        emit_enter_runtime();
        fragment_call(ga->get_i_bnot_guard_shared());
        emit_leave_runtime();
        emit_branch_if_not_value(ARG1, resolve_beam_label(Fail, dispUnknown));
    } else {
        emit_enter_runtime();
        fragment_call(ga->get_i_bnot_body_shared());
        emit_leave_runtime();
    }

    a.bind(next);
    mov_var(dst, ARG1);
    flush_var(dst);

    (void)Live;
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
            a32::Gp small_tag = TMP;
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
            shift = std::min<Sint>(shift, 31);
            if (shift == 0) {
                a.mov(dst.reg, lhs.reg);
            } else {
                a.asr(dst.reg, lhs.reg, imm(shift));
            }
            a.orr(dst.reg, dst.reg, small_tag);

            if (need_generic) {
                a.b(next);
            }
        } else {
            /* Constant shift is negative; fall back to the generic
             * path. */
        }
    } else {
        auto rhs = load_source(RHS, ARG3);
        Label both_small = a.newLabel(), no_clamp = a.newLabel();

        /* Ensure both operands are small. */
        emit_are_both_small(LHS, lhs.reg, RHS, rhs.reg, both_small);
        a.b(generic);

        a.bind(both_small);
        {
            /* Calculate shift count and ensure it's positive. */
            a.asr(TMP, rhs.reg, imm(_TAG_IMMED1_SIZE));
            a.cmp(TMP, imm(0));
            a.b_lt(generic);

            a.cmp(TMP, imm(31));
            a.b_le(no_clamp);
            mov_imm(TMP, 31);
            a.bind(no_clamp);

            /* Shift right. */
            ERTS_CT_ASSERT(_TAG_IMMED1_MASK == _TAG_IMMED1_SMALL);
            a.asr(dst.reg, lhs.reg, TMP);
            a.orr(dst.reg, dst.reg, imm(_TAG_IMMED1_SMALL));
            a.b(next);
        }
    }

    a.bind(generic);
    if (need_generic) {
        mov_var(ARG2, lhs);
        mov_arg(ARG3, RHS);

        if (Fail.get() != 0) {
            emit_enter_runtime();
            a.mov(ARG1, c_p);
            runtime_call<3>(erts_bsr);
            emit_leave_runtime();
            emit_branch_if_not_value(ARG1, resolve_beam_label(Fail, dispUnknown));
        } else {
            emit_enter_runtime();
            fragment_call(ga->get_i_bsr_body_shared());
            emit_leave_runtime();
        }

        mov_var(dst, ARG1);
    }

    a.bind(next);
    flush_var(dst);

    (void)Live;
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
    bool use_small_fast_path = is_bsl_small(LHS, RHS);

    /* ARM32 LSL-immediate only accepts 0..31. If the shift is a literal
     * outside that range, skip the inline fast path and use the runtime path
     * instead to preserve semantics. */
    if (use_small_fast_path && RHS.isSmall()) {
        int shift = RHS.as<ArgSmall>().getSigned();
        if (shift < 0 || shift >= 32) {
            use_small_fast_path = false;
        }
    }

    if (use_small_fast_path) {
        comment("skipped tests because operands and result are always small");
        if (RHS.isSmall()) {
            auto lhs = load_source(LHS);
            a.bic(TMP, lhs.reg, imm(_TAG_IMMED1_MASK));
            a.lsl(TMP, TMP, imm(RHS.as<ArgSmall>().getSigned()));
        } else {
            auto [lhs, rhs] = load_sources(LHS, ARG2, RHS, ARG3);
            a.bic(TMP, lhs.reg, imm(_TAG_IMMED1_MASK));
            a.lsr(VAR, rhs.reg, imm(_TAG_IMMED1_SIZE));
            a.lsl(TMP, TMP, VAR);
        }
        a.orr(dst.reg, TMP, imm(_TAG_IMMED1_SMALL));
        flush_var(dst);
        return;
    }

    auto [lhs, rhs] = load_sources(LHS, ARG2, RHS, ARG3);

    mov_var(ARG2, lhs);
    mov_var(ARG3, rhs);

    if (Fail.get() != 0) {
        emit_enter_runtime();
        a.mov(ARG1, c_p);
        runtime_call<3>(erts_bsl);
        emit_leave_runtime();
        emit_branch_if_not_value(ARG1, resolve_beam_label(Fail, dispUnknown));
    } else {
        emit_enter_runtime();
        fragment_call(ga->get_i_bsl_body_shared());
        emit_leave_runtime();
    }

    mov_var(dst, ARG1);
    flush_var(dst);

    (void)Live;
}
