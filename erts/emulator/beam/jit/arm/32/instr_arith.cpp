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
    // TODO
    emit_nyi("emit_plus_body_shared");
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
    // TODO
    emit_nyi("emit_unary_minus_body_shared");
}

void BeamModuleAssembler::emit_i_unary_minus(const ArgLabel &Fail,
                                             const ArgWord &Live,
                                             const ArgSource &Src,
                                             const ArgRegister &Dst) {
    // TODO
    emit_nyi("emit_i_unary_minus");
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
    // TODO
    emit_nyi("emit_minus_body_shared");
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
    // TODO
    emit_nyi("emit_mul_add_guard_shared");
}

/* ARG2 = Src1
 * ARG3 = Src2
 *
 * The result is returned in ARG1.
 */
void BeamGlobalAssembler::emit_mul_body_shared() {
    // TODO
    emit_nyi("emit_mul_body_shared");
}

/* ARG2 = Src1
 * ARG3 = Src2
 *
 * The result is returned in ARG1 (set to THE_NON_VALUE if
 * the call failed).
 */
void BeamGlobalAssembler::emit_mul_guard_shared() {
    // TODO
    emit_nyi("emit_mul_guard_shared");
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

    Label mixed = a.newLabel(), small = a.newLabel(), next = a.newLabel();
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
    // TODO
    emit_nyi("emit_int_div_rem_guard_shared");
}

/* ARG2 = LHS
 * ARG3 = RHS
 * ARG4 = error MFA
 *
 * Quotient is returned in ARG1, remainder in ARG2.
 */
void BeamGlobalAssembler::emit_int_div_rem_body_shared() {
    // TODO
    emit_nyi("emit_int_div_rem_body_shared");
}

void BeamModuleAssembler::emit_div_rem_literal(Sint divisor,
                                               const ArgSource &Dividend,
                                               a32::Gp dividend,
                                               a32::Gp quotient,
                                               a32::Gp remainder,
                                               const Label &generic,
                                               bool need_div,
                                               bool need_rem) {
    // TODO
    emit_nyi("emit_div_rem_literal");
}

void BeamModuleAssembler::emit_div_rem(const ArgLabel &Fail,
                                       const ArgSource &LHS,
                                       const ArgSource &RHS,
                                       const ErtsCodeMFA *error_mfa,
                                       const ArgRegister &Quotient,
                                       const ArgRegister &Remainder,
                                       bool need_div,
                                       bool need_rem) {
    // TODO
    emit_nyi("emit_div_rem");
}

void BeamModuleAssembler::emit_i_rem_div(const ArgLabel &Fail,
                                         const ArgWord &Live,
                                         const ArgSource &LHS,
                                         const ArgSource &RHS,
                                         const ArgRegister &Remainder,
                                         const ArgRegister &Quotient) {
    // TODO
    emit_nyi("emit_i_rem_div");
}

void BeamModuleAssembler::emit_i_div_rem(const ArgLabel &Fail,
                                         const ArgWord &Live,
                                         const ArgSource &LHS,
                                         const ArgSource &RHS,
                                         const ArgRegister &Quotient,
                                         const ArgRegister &Remainder) {
    // TODO
    emit_nyi("emit_i_div_rem");
}

void BeamModuleAssembler::emit_i_int_div(const ArgLabel &Fail,
                                         const ArgWord &Live,
                                         const ArgSource &LHS,
                                         const ArgSource &RHS,
                                         const ArgRegister &Quotient) {
    // TODO
    emit_nyi("emit_i_int_div");
}

void BeamModuleAssembler::emit_i_rem(const ArgLabel &Fail,
                                     const ArgWord &Live,
                                     const ArgSource &LHS,
                                     const ArgSource &RHS,
                                     const ArgRegister &Remainder) {
    // TODO
    emit_nyi("emit_i_rem");
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
    // TODO
    emit_nyi("emit_i_band_body_shared");
}

void BeamGlobalAssembler::emit_i_band_body_shared() {
    // TODO
    emit_nyi("emit_i_band_body_shared");
}

void BeamModuleAssembler::emit_i_band(const ArgLabel &Fail,
                                      const ArgWord &Live,
                                      const ArgSource &LHS,
                                      const ArgSource &RHS,
                                      const ArgRegister &Dst) {
    // TODO
    emit_nyi("emit_i_band");
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
    // TODO
    emit_nyi("emit_i_bor_body_shared");
}

void BeamModuleAssembler::emit_i_bor(const ArgLabel &Fail,
                                     const ArgWord &Live,
                                     const ArgSource &LHS,
                                     const ArgSource &RHS,
                                     const ArgRegister &Dst) {
    // TODO
    emit_nyi("emit_i_bor");
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
    // TODO
    emit_nyi("emit_i_bxor_body_shared");
}

void BeamModuleAssembler::emit_i_bxor(const ArgLabel &Fail,
                                      const ArgWord &Live,
                                      const ArgSource &LHS,
                                      const ArgSource &RHS,
                                      const ArgRegister &Dst) {
    // TODO
    emit_nyi("emit_i_bxor");
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
    // TODO
    emit_nyi("emit_i_bnot_guard_shared");
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
    // TODO
    emit_nyi("emit_i_bnot_body_shared");
}

void BeamModuleAssembler::emit_i_bnot(const ArgLabel &Fail,
                                      const ArgWord &Live,
                                      const ArgSource &Src,
                                      const ArgRegister &Dst) {
    // TODO
    emit_nyi("emit_i_bnot");
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
    // TODO
    emit_nyi("emit_i_bsr");
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
    // TODO
    emit_nyi("emit_i_bsl_body_shared");
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
        Label error = a.newLabel(), done = a.newLabel();
        static const ErtsCodeMFA bif_mfa = {am_erlang, am_bsl, 2};

        /* Save original arguments for an accurate exception path. */
        a.str(ARG2, getXRef(0));
        a.str(ARG3, getXRef(1));

        emit_enter_runtime();
        a.mov(ARG1, c_p);
        runtime_call<3>(erts_bsl);
        emit_leave_runtime();

        emit_branch_if_not_value(ARG1, error);
        mov_var(dst, ARG1);
        flush_var(dst);
        a.b(done);

        a.bind(error);
        mov_imm(ARG4, &bif_mfa);
        emit_raise_exception();

        a.bind(done);
        return;
    }

    mov_var(dst, ARG1);
    flush_var(dst);

    (void)Live;
}
