/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2020-2025. All Rights Reserved.
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

/*
 * See the comment in instr_common.cpp for some notes on how
 * to minimize code size.
 */

#include "beam_asm.hpp"

extern "C"
{
#include "big.h"
#include "erl_bif_table.h"
}

/*
 * Clobbers ARG1.
 */
void BeamModuleAssembler::emit_is_small(Label fail,
                                        const ArgSource &Arg,
                                        x86::Gp Reg) {
    ASSERT(ARG1 != Reg);

    if (always_small(Arg)) {
        comment("skipped test for small operand since it is always small");
    } else if (always_one_of<BeamTypeId::Number>(Arg)) {
        comment("simplified test for small operand since it is a number");
        a.test(Reg.r8(), imm(TAG_PRIMARY_LIST));
        a.short_().je(fail);
    } else {
        comment("is the operand small?");
        a.mov(ARG1d, Reg.r32());
        a.and_(ARG1d, imm(_TAG_IMMED1_MASK));
        a.cmp(ARG1d, imm(_TAG_IMMED1_SMALL));
        a.short_().jne(fail);
    }
}

/*
 * Clobbers RET, ARG1.
 */
void BeamModuleAssembler::emit_are_both_small(Label fail,
                                              const ArgSource &LHS,
                                              x86::Gp A,
                                              const ArgSource &RHS,
                                              x86::Gp B) {
    ASSERT(ARG1 != A && ARG1 != B);
    if (always_small(LHS) && always_small(RHS)) {
        comment("skipped test for small operands since they are always small");
    } else if (always_one_of<BeamTypeId::Number>(LHS) &&
               always_one_of<BeamTypeId::Number>(RHS)) {
        comment("simplified test for small operands since both are numbers");
        if (always_small(RHS)) {
            a.test(A.r8(), imm(TAG_PRIMARY_LIST));
        } else if (always_small(LHS)) {
            a.test(B.r8(), imm(TAG_PRIMARY_LIST));
        } else if (A != RET && B != RET) {
            a.mov(RETd, A.r32());
            a.and_(RETd, B.r32());
            a.test(RETb, imm(TAG_PRIMARY_LIST));
        } else {
            a.mov(ARG1d, A.r32());
            a.and_(ARG1d, B.r32());
            a.test(ARG1.r8(), imm(TAG_PRIMARY_LIST));
        }
        a.short_().je(fail);
    } else if (always_small(LHS)) {
        if (A == RET || B == RET) {
            emit_is_small(fail, RHS, B);
        } else {
            comment("is the operand small?");
            a.mov(RETd, B.r32());
            a.and_(RETb, imm(_TAG_IMMED1_MASK));
            a.cmp(RETb, imm(_TAG_IMMED1_SMALL));
            a.short_().jne(fail);
        }
    } else if (always_small(RHS)) {
        if (A == RET || B == RET) {
            emit_is_small(fail, LHS, A);
        } else {
            comment("is the operand small?");
            a.mov(RETd, A.r32());
            a.and_(RETb, imm(_TAG_IMMED1_MASK));
            a.cmp(RETb, imm(_TAG_IMMED1_SMALL));
            a.short_().jne(fail);
        }
    } else {
        comment("are both operands small?");
        if (A != RET && B != RET) {
            a.mov(RETd, A.r32());
            a.and_(RETd, B.r32());
            a.and_(RETb, imm(_TAG_IMMED1_MASK));
            a.cmp(RETb, imm(_TAG_IMMED1_SMALL));
        } else {
            a.mov(ARG1d, A.r32());
            a.and_(ARG1d, B.r32());
            a.and_(ARG1d, imm(_TAG_IMMED1_MASK));
            a.cmp(ARG1d, imm(_TAG_IMMED1_SMALL));
        }
        a.short_().jne(fail);
    }
}

void BeamGlobalAssembler::emit_plus_body_shared() {
    static const ErtsCodeMFA bif_mfa = {am_erlang, am_Plus, 2};

    Label error = a.newLabel();

    emit_enter_frame();
    emit_enter_runtime();

    /* Save original arguments for the error path. */
    a.mov(TMP_MEM1q, ARG2);
    a.mov(TMP_MEM2q, ARG3);

    a.mov(ARG1, c_p);
    runtime_call<Eterm (*)(Process *, Eterm, Eterm), erts_mixed_plus>();

    emit_leave_runtime();
    emit_leave_frame();

    emit_test_the_non_value(RET);
    a.short_().je(error);
    a.ret();

    a.bind(error);
    {
        /* Place the original arguments in x-registers. */
        a.mov(ARG1, TMP_MEM1q);
        a.mov(ARG2, TMP_MEM2q);
        a.mov(getXRef(0), ARG1);
        a.mov(getXRef(1), ARG2);

        a.mov(ARG4, imm(&bif_mfa));
        a.jmp(labels[raise_exception]);
    }
}

void BeamGlobalAssembler::emit_plus_guard_shared() {
    emit_enter_frame();
    emit_enter_runtime();

    a.mov(ARG1, c_p);
    /* ARG2 and ARG3 were set by the caller */
    runtime_call<Eterm (*)(Process *, Eterm, Eterm), erts_mixed_plus>();

    emit_leave_runtime();
    emit_leave_frame();

    /* Set ZF if the addition failed. */
    emit_test_the_non_value(RET);
    a.ret();
}

void BeamModuleAssembler::emit_i_plus(const ArgSource &LHS,
                                      const ArgSource &RHS,
                                      const ArgLabel &Fail,
                                      const ArgRegister &Dst) {
    bool is_rhs_literal = false;
    Sint rhs_untagged;

    bool small_result = is_sum_small_if_args_are_small(LHS, RHS);

    if (RHS.isSmall()) {
        /* Must be signed for the template magic in isInt32 to work for
         * negative numbers. */
        rhs_untagged = RHS.as<ArgSmall>().getSigned() << _TAG_IMMED1_SIZE;
        is_rhs_literal = Support::isInt32(rhs_untagged);
    }

    if (always_small(LHS) && always_small(RHS) && small_result) {
        comment("add without overflow check");

        mov_arg(RET, LHS);

        if (is_rhs_literal) {
            preserve_cache(
                    [&]() {
                        a.add(RET, imm(rhs_untagged));
                    },
                    RET);
        } else if (RHS.isSmall()) {
            preserve_cache(
                    [&]() {
                        mov_imm(ARG2,
                                RHS.as<ArgSmall>().get() & ~_TAG_IMMED1_MASK);
                        a.add(RET, ARG2);
                    },
                    RET,
                    ARG2);
        } else {
            mov_arg(ARG2, RHS);
            preserve_cache(
                    [&]() {
                        a.lea(RET,
                              x86::qword_ptr(RET, ARG2, 0, -_TAG_IMMED1_SMALL));
                    },
                    RET);
        }

        mov_arg(Dst, RET);
        return;
    }

    Label next = a.newLabel(), mixed = a.newLabel();

    mov_arg(ARG2, LHS); /* Used by erts_mixed_plus in this slot */
    mov_arg(ARG3, RHS); /* Used by erts_mixed_plus in this slot */

    if (LHS.isLiteral() || RHS.isLiteral()) {
        comment("skipped test for small because one operand is never small");
    } else {
        emit_are_both_small(mixed, LHS, ARG2, RHS, ARG3);

        if (small_result) {
            comment("skipped overflow test because the result is always small");
            a.lea(RET, x86::qword_ptr(ARG2, ARG3, 0, -_TAG_IMMED1_SMALL));
            a.short_().jmp(next);
        } else {
            a.lea(RET, x86::qword_ptr(ARG2, -_TAG_IMMED1_SMALL));
            a.add(RET, ARG3);
            a.short_().jno(next);
        }
    }

    /* Call mixed addition. */
    a.bind(mixed);
    {
        if (Fail.get() != 0) {
            safe_fragment_call(ga->get_plus_guard_shared());
            a.je(resolve_beam_label(Fail));
        } else {
            safe_fragment_call(ga->get_plus_body_shared());
        }
    }

    a.bind(next);
    mov_arg(Dst, RET);
}

void BeamGlobalAssembler::emit_minus_body_shared() {
    static const ErtsCodeMFA bif_mfa = {am_erlang, am_Minus, 2};

    Label error = a.newLabel();

    emit_enter_frame();
    emit_enter_runtime();

    /* Save original arguments for the error path. */
    a.mov(TMP_MEM1q, ARG2);
    a.mov(TMP_MEM2q, ARG3);

    a.mov(ARG1, c_p);
    runtime_call<Eterm (*)(Process *, Eterm, Eterm), erts_mixed_minus>();

    emit_leave_runtime();
    emit_leave_frame();

    emit_test_the_non_value(RET);
    a.short_().je(error);
    a.ret();

    a.bind(error);
    {
        /* Place the original arguments in x-registers. */
        a.mov(ARG1, TMP_MEM1q);
        a.mov(ARG2, TMP_MEM2q);
        a.mov(getXRef(0), ARG1);
        a.mov(getXRef(1), ARG2);

        a.mov(ARG4, imm(&bif_mfa));
        a.jmp(labels[raise_exception]);
    }
}

void BeamGlobalAssembler::emit_minus_guard_shared() {
    emit_enter_frame();
    emit_enter_runtime();

    a.mov(ARG1, c_p);
    /* ARG2 and ARG3 were set by the caller */
    runtime_call<Eterm (*)(Process *, Eterm, Eterm), erts_mixed_minus>();

    emit_leave_runtime();
    emit_leave_frame();

    /* Set ZF if the addition failed. */
    emit_test_the_non_value(RET);
    a.ret();
}

void BeamModuleAssembler::emit_i_minus(const ArgSource &LHS,
                                       const ArgSource &RHS,
                                       const ArgLabel &Fail,
                                       const ArgRegister &Dst) {
    bool is_rhs_literal = false;
    Sint rhs_untagged = 0;
    bool small_result = is_diff_small_if_args_are_small(LHS, RHS);

    if (RHS.isSmall()) {
        /* Must be signed for the template magic in isInt32 to work for
         * negative numbers. */
        rhs_untagged = RHS.as<ArgSmall>().getSigned() << _TAG_IMMED1_SIZE;
        is_rhs_literal = Support::isInt32(rhs_untagged);
    }

    if (always_small(LHS) && always_small(RHS) && small_result) {
        comment("subtract without overflow check");
        mov_arg(RET, LHS);

        if (is_rhs_literal) {
            preserve_cache(
                    [&]() {
                        a.sub(RET, imm(rhs_untagged));
                    },
                    RET);
        } else if (RHS.isSmall()) {
            mov_imm(ARG2, RHS.as<ArgSmall>().get() & ~_TAG_IMMED1_MASK);
            a.sub(RET, ARG2);
        } else {
            mov_arg(ARG2, RHS);
            a.and_(ARG2, imm(~_TAG_IMMED1_MASK));
            a.sub(RET, ARG2);
        }

        mov_arg(Dst, RET);
        return;
    }

    Label next = a.newLabel(), mixed = a.newLabel();

    mov_arg(ARG2, LHS); /* Used by erts_mixed_plus in this slot */
    mov_arg(ARG3, RHS); /* Used by erts_mixed_plus in this slot */

    if (LHS.isLiteral() || RHS.isLiteral()) {
        comment("skipped test for small because one operand is never small");
    } else {
        emit_are_both_small(mixed, LHS, ARG2, RHS, ARG3);

        if (small_result) {
            comment("skipped overflow test because the result is always small");
            a.mov(RET, ARG2);
            if (is_rhs_literal) {
                a.sub(RET, imm(rhs_untagged));
            } else {
                a.and_(ARG3, imm(~_TAG_IMMED1_MASK));
                a.sub(RET, ARG3);
            }
            a.short_().jmp(next);
        } else {
            a.mov(RET, ARG2);
            if (is_rhs_literal) {
                a.sub(RET, imm(rhs_untagged));
            } else {
                a.lea(ARG4, x86::qword_ptr(ARG3, -_TAG_IMMED1_SMALL));
                a.sub(RET, ARG4);
            }
            a.short_().jno(next);
        }
    }

    a.bind(mixed);
    if (Fail.get() != 0) {
        safe_fragment_call(ga->get_minus_guard_shared());
        a.je(resolve_beam_label(Fail));
    } else {
        safe_fragment_call(ga->get_minus_body_shared());
    }

    a.bind(next);
    mov_arg(Dst, RET);
}

void BeamGlobalAssembler::emit_unary_minus_body_shared() {
    static const ErtsCodeMFA bif_mfa = {am_erlang, am_Minus, 1};

    Label error = a.newLabel();

    emit_enter_frame();
    emit_enter_runtime();

    /* Save original arguments for the error path. */
    a.mov(TMP_MEM1q, ARG2);

    a.mov(ARG1, c_p);
    runtime_call<Eterm (*)(Process *, Eterm), erts_unary_minus>();

    emit_leave_runtime();
    emit_leave_frame();

    emit_test_the_non_value(RET);
    a.short_().je(error);
    a.ret();

    a.bind(error);
    {
        /* Place the original argument in x0. */
        a.mov(ARG1, TMP_MEM1q);
        a.mov(getXRef(0), ARG1);

        a.mov(ARG4, imm(&bif_mfa));
        a.jmp(labels[raise_exception]);
    }
}

void BeamGlobalAssembler::emit_unary_minus_guard_shared() {
    emit_enter_frame();
    emit_enter_runtime();

    a.mov(ARG1, c_p);
    /* ARG2 was set by the caller */
    runtime_call<Eterm (*)(Process *, Eterm), erts_unary_minus>();

    emit_leave_runtime();
    emit_leave_frame();

    /* Set ZF if the negation failed. */
    emit_test_the_non_value(RET);
    a.ret();
}

void BeamModuleAssembler::emit_i_unary_minus(const ArgSource &Src,
                                             const ArgLabel &Fail,
                                             const ArgRegister &Dst) {
    ArgVal zero = ArgVal(ArgVal::Type::Immediate, make_small(0));
    bool small_result = is_diff_small_if_args_are_small(zero, Src);

    if (always_small(Src) && small_result) {
        comment("negation without overflow test");
        mov_arg(ARG2, Src);
        a.mov(RETd, imm(_TAG_IMMED1_SMALL));
        a.and_(ARG2, imm(~_TAG_IMMED1_MASK));
        a.sub(RET, ARG2);
        mov_arg(Dst, RET);

        return;
    }

    Label next = a.newLabel(), mixed = a.newLabel();

    mov_arg(ARG2, Src);
    a.mov(RETd, ARG2d);
    a.and_(RETb, imm(_TAG_IMMED1_MASK));
    a.cmp(RETb, imm(_TAG_IMMED1_SMALL));
    a.short_().jne(mixed);

    /* RETb is now equal to _TAG_IMMED1_SMALL. */
    a.movzx(RET, RETb); /* Set RET to make_small(0). */
    a.mov(ARG3, ARG2);
    a.and_(ARG3, imm(~_TAG_IMMED1_MASK));
    a.sub(RET, ARG3);

    if (small_result) {
        comment("skipped overflow test because the result is always small");
        a.short_().jmp(next);
    } else {
        a.short_().jno(next);
    }

    a.bind(mixed);
    if (Fail.get() != 0) {
        safe_fragment_call(ga->get_unary_minus_guard_shared());
        a.je(resolve_beam_label(Fail));
    } else {
        safe_fragment_call(ga->get_unary_minus_body_shared());
    }

    a.bind(next);
    mov_arg(Dst, RET);
}

/* ARG1 = LHS, ARG4 (!) = RHS
 *
 * We avoid using ARG2 and ARG3 because multiplication clobbers RDX, which is
 * ARG2 on Windows and ARG3 on SystemV.
 *
 * Quotient is returned in RAX, remainder in RDX. Error is indicated by ZF. */
void BeamGlobalAssembler::emit_int_div_rem_guard_shared() {
    Label exit = a.newLabel(), generic = a.newLabel();

    emit_enter_frame();

    a.cmp(ARG4, imm(SMALL_ZERO));
    a.je(exit);

    /* Are both smalls? */
    a.mov(ARG2d, ARG1d);
    a.and_(ARG2d, ARG4d);
    a.and_(ARG2d, imm(_TAG_IMMED1_MASK));
    a.cmp(ARG2d, imm(_TAG_IMMED1_SMALL));
    a.jne(generic);

    a.mov(ARG5, ARG4);
    a.sar(ARG5, imm(_TAG_IMMED1_SIZE));
    a.mov(x86::rax, ARG1);
    a.sar(x86::rax, imm(_TAG_IMMED1_SIZE));

    /* Sign-extend and divide. The result is implicitly placed in RAX and the
     * remainder in RDX (ARG3). */
    a.cqo();
    a.idiv(ARG5);

    a.mov(ARG5, x86::rax);

    /* Speculatively tag the result as overflow is very rare. */
    a.sal(x86::rax, imm(_TAG_IMMED1_SIZE));
    a.sal(x86::rdx, imm(_TAG_IMMED1_SIZE));
    a.or_(x86::rax, imm(_TAG_IMMED1_SMALL));
    a.or_(x86::rdx, imm(_TAG_IMMED1_SMALL));

    /* MIN_SMALL divided by -1 will overflow, and we'll need to fall back to the
     * generic handler in that case. */
    a.sar(ARG5, imm(SMALL_BITS - 1));
    a.cmp(ARG5, imm(1));
    a.jl(exit);

    /* Fall through to `generic` */

    a.bind(generic);
    {
        emit_enter_runtime();

        a.mov(ARG2, ARG1);
        a.mov(ARG3, ARG4);
        a.lea(ARG4, TMP_MEM1q);
        a.lea(ARG5, TMP_MEM2q);
        a.mov(ARG1, c_p);
        runtime_call<int (*)(Process *, Eterm, Eterm, Eterm *, Eterm *),
                     erts_int_div_rem>();

        emit_leave_runtime();

        /* erts_int_div returns 0 on failure and 1 on success. */
        a.test(RETd, RETd);

        /* Place the result in RAX:RDX, mirroring the `idiv` instruction. */
        a.mov(x86::rax, TMP_MEM1q);
        a.mov(x86::rdx, TMP_MEM2q);

        /* Fall through */
    }

    /* Return with a potential error in ZF. It will be set if we came here from
     * the guard against SMALL_ZERO or if we're returning THE_NON_VALUE. */
    a.bind(exit);
    {
        emit_leave_frame();
        a.ret();
    }
}

/* ARG1 = LHS, ARG4 (!) = RHS, ARG5 = error MFA
 *
 * We avoid using ARG2 and ARG3 because multiplication clobbers RDX, which is
 * ARG2 on Windows and ARG3 on SystemV.
 *
 * Quotient is returned in RAX, remainder in RDX. */
void BeamGlobalAssembler::emit_int_div_rem_body_shared() {
    Label div_zero = a.newLabel(), generic_div = a.newLabel(),
          generic_error = a.newLabel();

    emit_enter_frame();

    a.cmp(ARG4, imm(SMALL_ZERO));
    a.je(div_zero);

    /* Are both smalls? */
    a.mov(ARG2d, ARG1d);
    a.and_(ARG2d, ARG4d);
    a.and_(ARG2d, imm(_TAG_IMMED1_MASK));
    a.cmp(ARG2d, imm(_TAG_IMMED1_SMALL));
    a.jne(generic_div);

    a.mov(ARG6, ARG4);
    a.sar(ARG6, imm(_TAG_IMMED1_SIZE));
    a.mov(x86::rax, ARG1);
    a.sar(x86::rax, imm(_TAG_IMMED1_SIZE));

    /* Sign-extend and divide. The result is implicitly placed in RAX and the
     * remainder in RDX (ARG3). */
    a.cqo();
    a.idiv(ARG6);

    a.mov(ARG6, x86::rax);

    /* Speculatively tag the result as overflow is very rare. */
    a.sal(x86::rax, imm(_TAG_IMMED1_SIZE));
    a.sal(x86::rdx, imm(_TAG_IMMED1_SIZE));
    a.or_(x86::rax, imm(_TAG_IMMED1_SMALL));
    a.or_(x86::rdx, imm(_TAG_IMMED1_SMALL));

    /* MIN_SMALL divided by -1 will overflow, and we'll need to fall back to the
     * generic handler in that case. */
    a.sar(ARG6, imm(SMALL_BITS - 1));
    a.cmp(ARG6, imm(1));
    a.short_().jge(generic_div);

    emit_leave_frame();
    a.ret();

    a.bind(generic_div);
    {
        emit_enter_runtime();

        /* Save MFA and original arguments for the error path. */
        a.mov(TMP_MEM1q, ARG1);
        a.mov(TMP_MEM2q, ARG4);
        a.mov(TMP_MEM3q, ARG5);

        a.mov(ARG2, ARG1);
        a.mov(ARG3, ARG4);
        a.lea(ARG4, TMP_MEM4q);
        a.lea(ARG5, TMP_MEM5q);
        a.mov(ARG1, c_p);
        runtime_call<int (*)(Process *, Eterm, Eterm, Eterm *, Eterm *),
                     erts_int_div_rem>();

        emit_leave_runtime();
        emit_leave_frame();

        /* erts_int_div returns 0 on failure and 1 on success. */
        a.test(RETd, RETd);

        /* Place the result in RAX:RDX, mirroring the `idiv` instruction. */
        a.mov(x86::rax, TMP_MEM4q);
        a.mov(x86::rdx, TMP_MEM5q);

        a.short_().je(generic_error);
        a.ret();
    }

    a.bind(div_zero);
    {
        emit_leave_frame();

        /* Set up a badarith exception and place the original arguments in
         * x-registers. */
        a.mov(x86::qword_ptr(c_p, offsetof(Process, freason)),
              imm(EXC_BADARITH));

        a.mov(getXRef(0), ARG1);
        a.mov(getXRef(1), ARG4);

        a.mov(ARG4, ARG5);
        a.jmp(labels[raise_exception]);
    }

    a.bind(generic_error);
    {
        /* Place the original arguments in x-registers. */
        a.mov(ARG1, TMP_MEM1q);
        a.mov(ARG2, TMP_MEM2q);
        a.mov(getXRef(0), ARG1);
        a.mov(getXRef(1), ARG2);

        /* Read saved MFA. */
        a.mov(ARG4, TMP_MEM3q);
        a.jmp(labels[raise_exception]);
    }
}

void BeamModuleAssembler::emit_div_rem(const ArgLabel &Fail,
                                       const ArgSource &LHS,
                                       const ArgSource &RHS,
                                       const ErtsCodeMFA *error_mfa,
                                       bool need_div,
                                       bool need_rem) {
    Label generic_div = a.newLabel(), next = a.newLabel();
    bool need_generic = true;
    Sint divisor = 0;

    if (RHS.isSmall()) {
        divisor = RHS.as<ArgSmall>().getSigned();
    }

    mov_arg(x86::rax, LHS);

    if (divisor != 0 && divisor != -1) {
        /* There is no possibility of overflow. */
        if (always_small(LHS)) {
            comment("skipped test for small dividend since it is always small");
            need_generic = false;
        } else if (always_one_of<BeamTypeId::Number>(LHS)) {
            comment("simplified test for small dividend since it is an "
                    "integer");
            a.test(x86::al, imm(TAG_PRIMARY_LIST));
            a.short_().je(generic_div);
        } else {
            comment("testing for a small dividend");
            a.mov(ARG2d, x86::eax);
            a.and_(ARG2d, imm(_TAG_IMMED1_MASK));
            a.cmp(ARG2d, imm(_TAG_IMMED1_SMALL));
            a.short_().jne(generic_div);
        }

        if (Support::isPowerOf2(divisor) &&
            std::get<0>(getClampedRange(LHS)) >= 0) {
            /* Unsigned integer division. */
            int trailing_bits = Support::ctz<Eterm>(divisor);

            if (need_rem) {
                Uint mask = Support::lsbMask<Uint>(trailing_bits +
                                                   _TAG_IMMED1_SIZE);
                mask = (1ULL << (trailing_bits + _TAG_IMMED1_SIZE)) - 1;
                comment("optimized rem by replacing with masking");
                mov_imm(x86::rdx, mask);
                a.and_(x86::rdx, x86::rax);
            }
            if (need_div) {
                comment("optimized div by replacing with right shift");
                ERTS_CT_ASSERT(_TAG_IMMED1_SMALL == _TAG_IMMED1_MASK);
                a.shr(x86::rax, imm(trailing_bits));
                a.or_(x86::rax, imm(_TAG_IMMED1_SMALL));
            }
        } else if (Support::isPowerOf2(divisor)) {
            /* Signed integer division. */
            int shift = Support::ctz<Eterm>(divisor);
            Sint offset = (divisor - 1) << _TAG_IMMED1_SIZE;

            if (need_rem) {
                a.mov(x86::rdx, x86::rax);
                ASSERT(x86::rdx != ARG1);
            }

            if (need_div) {
                comment("optimized div by replacing with right shift");
            }

            if (divisor == 2) {
                ERTS_CT_ASSERT(_TAG_IMMED1_SMALL == _TAG_IMMED1_MASK);
                a.mov(ARG1, x86::rax);
                a.shr(ARG1, imm(63));
                a.add(x86::rax, ARG1);
            } else {
                if (Support::isInt32(offset)) {
                    a.lea(ARG1, x86::qword_ptr(x86::rax, offset));
                } else {
                    a.mov(ARG1, offset);
                    a.add(ARG1, x86::rax);
                }
                a.test(x86::rax, x86::rax);
                a.cmovs(x86::rax, ARG1);
            }

            if (need_rem) {
                Uint mask = (Uint)-1 << (shift + _TAG_IMMED1_SIZE);
                comment("optimized rem by replacing with subtraction");
                mov_imm(ARG1, mask);
                a.and_(ARG1, x86::rax);
                a.sub(x86::rdx, ARG1);
            }

            if (need_div) {
                ERTS_CT_ASSERT(_TAG_IMMED1_SMALL == _TAG_IMMED1_MASK);
                a.sar(x86::rax, imm(shift));
                a.or_(x86::rax, imm(_TAG_IMMED1_SMALL));
            }
        } else {
            comment("divide with inlined code");
            a.mov(ARG6, imm(divisor));
            a.sar(x86::rax, imm(_TAG_IMMED1_SIZE));
            a.cqo();
            a.idiv(ARG6);

            if (need_div) {
                a.sal(x86::rax, imm(_TAG_IMMED1_SIZE));
            }

            if (need_rem) {
                a.sal(x86::rdx, imm(_TAG_IMMED1_SIZE));
            }

            if (need_div) {
                a.or_(x86::rax, imm(_TAG_IMMED1_SMALL));
            }

            if (need_rem) {
                a.or_(x86::rdx, imm(_TAG_IMMED1_SMALL));
            }
        }

        if (need_generic) {
            a.short_().jmp(next);
        }
    }

    a.bind(generic_div);
    if (need_generic) {
        mov_arg(ARG4, RHS); /* Done first as mov_arg may clobber ARG1 */
        a.mov(ARG1, x86::rax);

        if (Fail.get() != 0) {
            safe_fragment_call(ga->get_int_div_rem_guard_shared());
            a.je(resolve_beam_label(Fail));
        } else {
            a.mov(ARG5, imm(error_mfa));
            safe_fragment_call(ga->get_int_div_rem_body_shared());
        }
    }

    a.bind(next);
}

void BeamModuleAssembler::emit_i_rem_div(const ArgSource &LHS,
                                         const ArgSource &RHS,
                                         const ArgLabel &Fail,
                                         const ArgRegister &Remainder,
                                         const ArgRegister &Quotient) {
    static const ErtsCodeMFA bif_mfa = {am_erlang, am_rem, 2};

    emit_div_rem(Fail, LHS, RHS, &bif_mfa);

    mov_arg(Remainder, x86::rdx);
    mov_arg(Quotient, x86::rax);
}

void BeamModuleAssembler::emit_i_div_rem(const ArgSource &LHS,
                                         const ArgSource &RHS,
                                         const ArgLabel &Fail,
                                         const ArgRegister &Quotient,
                                         const ArgRegister &Remainder) {
    static const ErtsCodeMFA bif_mfa = {am_erlang, am_div, 2};

    emit_div_rem(Fail, LHS, RHS, &bif_mfa);

    mov_arg(Quotient, x86::rax);
    mov_arg(Remainder, x86::rdx);
}

void BeamModuleAssembler::emit_i_int_div(const ArgLabel &Fail,
                                         const ArgSource &LHS,
                                         const ArgSource &RHS,
                                         const ArgRegister &Quotient) {
    static const ErtsCodeMFA bif_mfa = {am_erlang, am_div, 2};

    emit_div_rem(Fail, LHS, RHS, &bif_mfa, true, false);

    mov_arg(Quotient, x86::rax);
}

void BeamModuleAssembler::emit_i_rem(const ArgSource &LHS,
                                     const ArgSource &RHS,
                                     const ArgLabel &Fail,
                                     const ArgRegister &Remainder) {
    static const ErtsCodeMFA bif_mfa = {am_erlang, am_rem, 2};

    emit_div_rem(Fail, LHS, RHS, &bif_mfa, false, true);

    mov_arg(Remainder, x86::rdx);
}

void BeamModuleAssembler::emit_i_m_div(const ArgLabel &Fail,
                                       const ArgSource &LHS,
                                       const ArgSource &RHS,
                                       const ArgRegister &Dst) {
    static const ErtsCodeMFA bif_mfa = {am_erlang, am_Div, 2};

    Label next = a.newLabel();

    mov_arg(ARG2, LHS);
    mov_arg(ARG3, RHS);

    emit_enter_runtime();

    /* Must be set last since mov_arg() may clobber ARG1 */
    a.mov(ARG1, c_p);
    runtime_call<Eterm (*)(Process *, Eterm, Eterm), erts_mixed_div>();

    emit_leave_runtime();

    emit_test_the_non_value(RET);

    if (Fail.get() != 0) {
        a.je(resolve_beam_label(Fail));
    } else {
        a.short_().jne(next);

        mov_arg(ARG2, LHS);
        mov_arg(ARG3, RHS);
        mov_arg(ArgXRegister(0), ARG2);
        mov_arg(ArgXRegister(1), ARG3);

        emit_raise_exception(&bif_mfa);
    }

    a.bind(next);
    mov_arg(Dst, RET);
}

/* ARG2 = Src1
 * ARG3 = Src2
 * ARG4 = Increment
 *
 * Result is returned in RET, error is indicated by ZF. */
void BeamGlobalAssembler::emit_mul_add_guard_shared() {
    Label done = a.newLabel();

    emit_enter_frame();
    emit_enter_runtime();

    a.mov(TMP_MEM1q, ARG4);

    a.mov(ARG1, c_p);
    runtime_call<Eterm (*)(Process *, Eterm, Eterm), erts_mixed_times>();
    emit_test_the_non_value(RET);
    a.short_().je(done);

    a.mov(ARG3, TMP_MEM1q);
    a.mov(ARG2, RET);
    a.mov(ARG1, c_p);
    a.cmp(ARG3, imm(make_small(0)));
    a.short_().je(done);
    runtime_call<Eterm (*)(Process *, Eterm, Eterm), erts_mixed_plus>();

    a.bind(done);
    emit_leave_runtime();
    emit_leave_frame();

    emit_test_the_non_value(RET); /* Sets ZF for use in caller */

    a.ret();
}

/* ARG2 = Src1
 * ARG3 = Src2
 * ARG4 = Increment
 *
 * Result is returned in RET. */
void BeamGlobalAssembler::emit_mul_add_body_shared() {
    Label mul_only = a.newLabel(), error = a.newLabel(),
          mul_error = a.newLabel(), do_error = a.newLabel();

    emit_enter_frame();
    emit_enter_runtime();

    /* Save original arguments for the error path. */
    a.mov(TMP_MEM1q, ARG2);
    a.mov(TMP_MEM2q, ARG3);
    a.mov(ARG1, c_p);
    a.cmp(ARG4, imm(make_small(0)));
    a.short_().je(mul_only);
    a.mov(TMP_MEM4q, ARG4);

    a.lea(ARG5, TMP_MEM3q);
    runtime_call<Eterm (*)(Process *, Eterm, Eterm, Eterm, Eterm *),
                 erts_mul_add>();

    emit_leave_runtime();
    emit_leave_frame();

    emit_test_the_non_value(RET);
    a.short_().je(error);

    a.ret();

    a.bind(mul_only);
    {
        runtime_call<Eterm (*)(Process *, Eterm, Eterm), erts_mixed_times>();

        emit_leave_runtime();
        emit_leave_frame();

        emit_test_the_non_value(RET);
        a.short_().je(mul_error);

        a.ret();
    }

    a.bind(error);
    {
        static const ErtsCodeMFA mul_mfa = {am_erlang, am_Times, 2};
        static const ErtsCodeMFA add_mfa = {am_erlang, am_Plus, 2};

        a.mov(ARG1, TMP_MEM3q);
        a.mov(ARG2, TMP_MEM4q);
        mov_imm(ARG4, &add_mfa);
        emit_test_the_non_value(ARG1);
        a.short_().jne(do_error);

        a.bind(mul_error);
        a.mov(ARG1, TMP_MEM1q);
        a.mov(ARG2, TMP_MEM2q);
        mov_imm(ARG4, &mul_mfa);

        a.bind(do_error);
        a.mov(getXRef(0), ARG1);
        a.mov(getXRef(1), ARG2);
        a.jmp(labels[raise_exception]);
    }
}

/* ARG2 = Src1
 * ARG3 = Src2
 *
 * The result is returned in RET.
 */
void BeamGlobalAssembler::emit_mul_body_shared() {
    mov_imm(ARG4, make_small(0));
    a.jmp(labels[mul_add_body_shared]);
}

/* ARG2 = Src1
 * ARG3 = Src2
 *
 * Result is returned in RET, error is indicated by ZF.
 */
void BeamGlobalAssembler::emit_mul_guard_shared() {
    mov_imm(ARG4, make_small(0));
    a.jmp(labels[mul_add_guard_shared]);
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

    if (always_small(Src1) && Src2.isSmall() && Src4.isSmall() &&
        is_product_small && is_sum_small) {
        x86::Mem p;
        Sint increment = Src4.as<ArgSmall>().get();
        increment -= factor * _TAG_IMMED1_SMALL;

        switch (factor) {
        case 2:
            p = ptr(RET, RET, 0, increment);
            break;
        case 3:
            p = ptr(RET, RET, 1, increment);
            break;
        case 4:
            p = ptr(x86::Gp(), RET, 2, increment);
            break;
        case 5:
            p = ptr(RET, RET, 2, increment);
            break;
        case 8:
            p = ptr(x86::Gp(), RET, 3, increment);
            break;
        case 9:
            p = ptr(RET, RET, 3, increment);
            break;
        }

        if (Support::isInt32(increment) && p.hasIndex()) {
            comment("optimizing multiplication and addition using LEA");
            mov_arg(RET, Src1);
            a.lea(RET, p);
            mov_arg(Dst, RET);
            return;
        }
    }

    if (always_small(Src1) && always_small(Src2) && always_small(Src4) &&
        is_product_small && is_sum_small) {
        comment("multiplication and addition without overflow check");
        if (Src2.isSmall()) {
            mov_arg(RET, Src1);
            a.and_(RET, imm(~_TAG_IMMED1_MASK));
            if (Support::isPowerOf2(factor)) {
                comment("optimized multiplication by replacing with left "
                        "shift");
                a.shl(RET, imm(left_shift));
            } else {
                mov_imm(ARG2, factor);
                a.imul(RET, ARG2);
            }
        } else {
            mov_arg(RET, Src1);
            mov_arg(ARG2, Src2);
            a.and_(RET, imm(~_TAG_IMMED1_MASK));
            a.sar(ARG2, imm(_TAG_IMMED1_SIZE));
            a.imul(RET, ARG2);
        }

        if (is_increment_zero) {
            a.or_(RET, imm(_TAG_IMMED1_SMALL));
        } else {
            mov_arg(ARG2, Src4);
            a.add(RET, ARG2);
        }
        mov_arg(Dst, RET);

        return;
    }

    Label next = a.newLabel(), mixed = a.newLabel();

    mov_arg(ARG2, Src1);
    mov_arg(ARG3, Src2);
    if (!is_increment_zero) {
        mov_arg(ARG4, Src4);
    }

    if (!sometimes_small) {
        comment("skipped test for small because one operand is never small");
    } else if (Src2.isSmall()) {
        Sint val = Src2.as<ArgSmall>().getSigned();
        emit_are_both_small(mixed, Src1, ARG2, Src4, ARG4);
        a.mov(RET, ARG2);
        mov_imm(ARG5, val);
    } else {
        if (is_increment_zero) {
            emit_are_both_small(mixed, Src1, ARG2, Src2, ARG3);
        } else if (always_small(Src1)) {
            emit_are_both_small(mixed, Src2, ARG3, Src4, ARG4);
        } else {
            a.mov(RETd, ARG2.r32());
            a.and_(RETd, ARG3.r32());
            a.and_(RETd, ARG4.r32());
            if (always_one_of<BeamTypeId::Integer, BeamTypeId::AlwaysBoxed>(
                        Src1) &&
                always_one_of<BeamTypeId::Integer, BeamTypeId::AlwaysBoxed>(
                        Src2) &&
                always_one_of<BeamTypeId::Integer, BeamTypeId::AlwaysBoxed>(
                        Src4)) {
                emit_is_not_boxed(mixed, RET);
            } else {
                a.and_(RETb, imm(_TAG_IMMED1_MASK));
                a.cmp(RETb, imm(_TAG_IMMED1_SMALL));
                a.short_().jne(mixed);
            }
        }
        a.mov(RET, ARG2);
        a.mov(ARG5, ARG3);
        a.sar(ARG5, imm(_TAG_IMMED1_SIZE));
    }

    if (sometimes_small) {
        a.and_(RET, imm(~_TAG_IMMED1_MASK));
        a.imul(RET, ARG5);
        if (is_product_small) {
            comment("skipped overflow check because product is always small");
        } else {
            a.short_().jo(mixed);
        }

        if (is_increment_zero) {
            a.or_(RET, imm(_TAG_IMMED1_SMALL));
        } else {
            a.add(RET, ARG4);
            if (is_sum_small) {
                comment("skipped overflow check because sum is always small");
            } else {
                a.short_().jo(mixed);
            }
        }

        a.short_().jmp(next);
    }

    /* Call mixed multiplication. */
    a.bind(mixed);
    {
        if (Fail.get() != 0) {
            if (is_increment_zero) {
                safe_fragment_call(ga->get_mul_guard_shared());
            } else {
                safe_fragment_call(ga->get_mul_add_guard_shared());
            }
            a.je(resolve_beam_label(Fail));
        } else {
            if (is_increment_zero) {
                safe_fragment_call(ga->get_mul_body_shared());
            } else {
                safe_fragment_call(ga->get_mul_add_body_shared());
            }
        }
    }

    a.bind(next);
    mov_arg(Dst, RET);
}

/* ARG2 (!) = LHS, RET (!) = RHS
 *
 * Result is returned in RET. Error is indicated by ZF. */
template<typename T, T Func>
void BeamGlobalAssembler::emit_bitwise_fallback_guard() {
    emit_enter_frame();
    emit_enter_runtime();

    a.mov(ARG1, c_p);
    /* ARG2 is already set to LHS */
    a.mov(ARG3, RET);
    runtime_call<T, Func>();

    emit_leave_runtime();
    emit_leave_frame();

    emit_test_the_non_value(RET);
    a.ret();
}

/* ARG2 (!) = LHS, RET (!) = RHS
 *
 * Result is returned in RET. */
template<typename T, T Func>
void BeamGlobalAssembler::emit_bitwise_fallback_body(const ErtsCodeMFA *mfa) {
    Label error = a.newLabel();

    emit_enter_frame();
    emit_enter_runtime();

    /* Save original arguments for the error path. */
    a.mov(TMP_MEM1q, ARG2);
    a.mov(TMP_MEM2q, RET);

    a.mov(ARG1, c_p);
    /* ARG2 is already set to LHS */
    a.mov(ARG3, RET);
    runtime_call<T, Func>();

    emit_leave_runtime();
    emit_leave_frame();

    emit_test_the_non_value(RET);
    a.short_().je(error);
    a.ret();

    a.bind(error);
    {
        /* Place the original arguments in x-registers. */
        a.mov(ARG1, TMP_MEM1q);
        a.mov(ARG2, TMP_MEM2q);
        a.mov(getXRef(0), ARG1);
        a.mov(getXRef(1), ARG2);

        a.mov(ARG4, imm(mfa));
        a.jmp(labels[raise_exception]);
    }
}

void BeamGlobalAssembler::emit_i_band_guard_shared() {
    emit_bitwise_fallback_guard<Eterm (*)(Process *, Eterm, Eterm),
                                erts_band>();
}

void BeamGlobalAssembler::emit_i_band_body_shared() {
    static const ErtsCodeMFA bif_mfa = {am_erlang, am_band, 2};
    emit_bitwise_fallback_body<Eterm (*)(Process *, Eterm, Eterm), erts_band>(
            &bif_mfa);
}

void BeamModuleAssembler::emit_i_band(const ArgSource &LHS,
                                      const ArgSource &RHS,
                                      const ArgLabel &Fail,
                                      const ArgRegister &Dst) {
    if (always_small(LHS) && always_small(RHS)) {
        comment("skipped test for small operands since they are always small");
        mov_arg(RET, LHS);
        if (RHS.isSmall() && Support::isInt32(RHS.as<ArgSmall>().get())) {
            preserve_cache(
                    [&]() {
                        a.and_(RETd, imm(RHS.as<ArgSmall>().get()));
                    },
                    RET);
        } else if (RHS.isSmall() &&
                   Support::isInt32((Sint)RHS.as<ArgSmall>().get())) {
            preserve_cache(
                    [&]() {
                        a.and_(RET, imm(RHS.as<ArgSmall>().get()));
                    },
                    RET);
        } else {
            mov_arg(ARG2, RHS);
            a.and_(RET, ARG2);
        }
        mov_arg(Dst, RET);
        return;
    }

    mov_arg(ARG2, LHS);
    mov_arg(RET, RHS);

    Label generic = a.newLabel(), next = a.newLabel();

    if (RHS.isLiteral()) {
        comment("skipped test for small because one operand is never small");
    } else {
        emit_are_both_small(generic, LHS, ARG2, RHS, RET);

        /* TAG & TAG = TAG, so we don't need to tag it again. */
        a.and_(RET, ARG2);
        a.short_().jmp(next);
    }

    a.bind(generic);
    {
        if (Fail.get() != 0) {
            safe_fragment_call(ga->get_i_band_guard_shared());
            a.je(resolve_beam_label(Fail));
        } else {
            safe_fragment_call(ga->get_i_band_body_shared());
        }
    }

    a.bind(next);
    mov_arg(Dst, RET);
}

/* ARG2 (!) = LHS, RET (!) = RHS
 *
 * Result is returned in RET. Error is indicated by ZF. */
void BeamGlobalAssembler::emit_i_bor_guard_shared() {
    emit_bitwise_fallback_guard<Eterm (*)(Process *, Eterm, Eterm), erts_bor>();
}

/* ARG2 (!) = LHS, RET (!) = RHS
 *
 * Result is returned in RET. */
void BeamGlobalAssembler::emit_i_bor_body_shared() {
    static const ErtsCodeMFA bif_mfa = {am_erlang, am_bor, 2};
    emit_bitwise_fallback_body<Eterm (*)(Process *, Eterm, Eterm), erts_bor>(
            &bif_mfa);
}

void BeamModuleAssembler::emit_i_bor(const ArgLabel &Fail,
                                     const ArgSource &LHS,
                                     const ArgSource &RHS,
                                     const ArgRegister &Dst) {
    if (always_small(LHS) && always_small(RHS)) {
        comment("skipped test for small operands since they are always small");
        mov_arg(RET, LHS);
        if (RHS.isImmed() && Support::isInt32((Sint)RHS.as<ArgSmall>().get())) {
            a.or_(RET, imm(RHS.as<ArgSmall>().get()));
        } else {
            mov_arg(ARG2, RHS);
            a.or_(RET, ARG2);
        }
        mov_arg(Dst, RET);
        return;
    }

    mov_arg(ARG2, LHS);
    mov_arg(RET, RHS);

    Label generic = a.newLabel(), next = a.newLabel();

    emit_are_both_small(generic, LHS, ARG2, RHS, RET);

    /* TAG | TAG = TAG, so we don't need to tag it again. */
    a.or_(RET, ARG2);
    a.short_().jmp(next);

    a.bind(generic);
    {
        if (Fail.get() != 0) {
            safe_fragment_call(ga->get_i_bor_guard_shared());
            a.je(resolve_beam_label(Fail));
        } else {
            safe_fragment_call(ga->get_i_bor_body_shared());
        }
    }

    a.bind(next);
    mov_arg(Dst, RET);
}

/* ARG2 (!) = LHS, RET (!) = RHS
 *
 * Result is returned in RET. Error is indicated by ZF. */
void BeamGlobalAssembler::emit_i_bxor_guard_shared() {
    emit_bitwise_fallback_guard<Eterm (*)(Process *, Eterm, Eterm),
                                erts_bxor>();
}

/* ARG2 (!) = LHS, RET (!) = RHS
 *
 * Result is returned in RET. */
void BeamGlobalAssembler::emit_i_bxor_body_shared() {
    static const ErtsCodeMFA bif_mfa = {am_erlang, am_bxor, 2};
    emit_bitwise_fallback_body<Eterm (*)(Process *, Eterm, Eterm), erts_bxor>(
            &bif_mfa);
}

void BeamModuleAssembler::emit_i_bxor(const ArgLabel &Fail,
                                      const ArgSource &LHS,
                                      const ArgSource &RHS,
                                      const ArgRegister &Dst) {
    if (always_small(LHS) && always_small(RHS)) {
        comment("skipped test for small operands since they are always small");
        mov_arg(RET, LHS);
        if (RHS.isImmed() && Support::isInt32((Sint)RHS.as<ArgSmall>().get())) {
            preserve_cache(
                    [&]() {
                        a.xor_(RET,
                               imm(RHS.as<ArgSmall>().get() &
                                   ~_TAG_IMMED1_SMALL));
                    },
                    RET);
        } else {
            mov_arg(ARG2, RHS);
            preserve_cache(
                    [&]() {
                        /* TAG ^ TAG = 0, so we need to tag it again. */
                        a.xor_(RET, ARG2);
                        a.or_(RET, imm(_TAG_IMMED1_SMALL));
                    },
                    RET);
        }
        mov_arg(Dst, RET);
        return;
    }

    mov_arg(ARG2, LHS);
    mov_arg(RET, RHS);

    Label generic = a.newLabel(), next = a.newLabel();

    emit_are_both_small(generic, LHS, ARG2, RHS, RET);

    /* TAG ^ TAG = 0, so we need to tag it again. */
    a.xor_(RET, ARG2);
    a.or_(RET, imm(_TAG_IMMED1_SMALL));
    a.short_().jmp(next);

    a.bind(generic);
    {
        if (Fail.get() != 0) {
            safe_fragment_call(ga->get_i_bxor_guard_shared());
            a.je(resolve_beam_label(Fail));
        } else {
            safe_fragment_call(ga->get_i_bxor_body_shared());
        }
    }

    a.bind(next);
    mov_arg(Dst, RET);
}

/* RET (!) = Src
 *
 * Result is returned in RET. Error is indicated by ZF. */
void BeamGlobalAssembler::emit_i_bnot_guard_shared() {
    emit_enter_frame();

    /* Undo the speculative inversion in module code */
    a.xor_(RET, imm(~_TAG_IMMED1_MASK));

    emit_enter_runtime();

    a.mov(ARG1, c_p);
    a.mov(ARG2, RET);
    runtime_call<Eterm (*)(Process *, Eterm), erts_bnot>();

    emit_leave_runtime();
    emit_leave_frame();

    emit_test_the_non_value(RET);
    a.ret();
}

/* RET (!) = Src
 *
 * Result is returned in RET. */
void BeamGlobalAssembler::emit_i_bnot_body_shared() {
    static const ErtsCodeMFA bif_mfa = {am_erlang, am_bnot, 1};

    Label error = a.newLabel();

    emit_enter_frame();

    /* Undo the speculative inversion in module code */
    a.xor_(RET, imm(~_TAG_IMMED1_MASK));

    emit_enter_runtime();

    /* Save original arguments for the error path. */
    a.mov(TMP_MEM1q, RET);

    a.mov(ARG1, c_p);
    a.mov(ARG2, RET);
    runtime_call<Eterm (*)(Process *, Eterm), erts_bnot>();

    emit_leave_runtime();
    emit_leave_frame();

    emit_test_the_non_value(RET);
    a.short_().je(error);
    a.ret();

    a.bind(error);
    {
        /* Place the original arguments in x-registers. */
        a.mov(ARG1, TMP_MEM1q);
        a.mov(getXRef(0), ARG1);

        a.mov(ARG4, imm(&bif_mfa));
        a.jmp(labels[raise_exception]);
    }
}

void BeamModuleAssembler::emit_i_bnot(const ArgLabel &Fail,
                                      const ArgSource &Src,
                                      const ArgRegister &Dst) {
    Label next = a.newLabel();

    mov_arg(RET, Src);

    /* Invert everything except the tag so we don't have to tag it again. */
    a.xor_(RET, imm(~_TAG_IMMED1_MASK));

    /* Fall through to the generic path if the result is not a small, where the
     * above operation will be reverted. */
    if (always_one_of<BeamTypeId::Number>(Src)) {
        comment("simplified test for small operand since it is a number");
        a.test(RETb, imm(TAG_PRIMARY_LIST));
        a.short_().jne(next);
    } else {
        a.mov(ARG1d, RETd);
        a.and_(ARG1d, imm(_TAG_IMMED1_MASK));
        a.cmp(ARG1d, imm(_TAG_IMMED1_SMALL));
        a.short_().je(next);
    }

    if (Fail.get() != 0) {
        safe_fragment_call(ga->get_i_bnot_guard_shared());
        a.je(resolve_beam_label(Fail));
    } else {
        safe_fragment_call(ga->get_i_bnot_body_shared());
    }

    a.bind(next);
    mov_arg(Dst, RET);
}

/* ARG2 (!) = LHS, RET (!) = RHS
 *
 * Result is returned in RET. Error is indicated by ZF. */
void BeamGlobalAssembler::emit_i_bsr_guard_shared() {
    emit_bitwise_fallback_guard<Eterm (*)(Process *, Eterm, Eterm), erts_bsr>();
}

/* ARG2 (!) = LHS, RET (!) = RHS
 *
 * Result is returned in RET. */
void BeamGlobalAssembler::emit_i_bsr_body_shared() {
    static const ErtsCodeMFA bif_mfa = {am_erlang, am_bsr, 2};
    emit_bitwise_fallback_body<Eterm (*)(Process *, Eterm, Eterm), erts_bsr>(
            &bif_mfa);
}

void BeamModuleAssembler::emit_i_bsr(const ArgSource &LHS,
                                     const ArgSource &RHS,
                                     const ArgLabel &Fail,
                                     const ArgRegister &Dst) {
    Label generic = a.newLabel(), next = a.newLabel();
    bool need_generic = true;
    bool need_register_load = true;

    mov_arg(ARG2, LHS);

    if (RHS.isSmall()) {
        Sint shift = RHS.as<ArgSmall>().getSigned();

        if (shift >= 0) {
            if (always_small(LHS)) {
                comment("skipped test for small left operand because it is "
                        "always small");
                need_generic = false;
            } else {
                emit_is_small(generic, LHS, ARG2);
            }

            a.mov(RET, ARG2);

            /* We don't need to clear the mask after shifting because
             * _TAG_IMMED1_SMALL will set all the bits anyway. */
            ERTS_CT_ASSERT(_TAG_IMMED1_MASK == _TAG_IMMED1_SMALL);
            shift = std::min<Sint>(shift, 63);
            a.sar(RET, imm(shift));
            a.or_(RET, imm(_TAG_IMMED1_SMALL));

            if (need_generic) {
                a.short_().jmp(next);
            }
        } else {
            /* Constant shift is negative; fall back to the generic
             * path. */
        }
    } else if (hasCpuFeature(CpuFeatures::X86::kBMI2)) {
        mov_arg(RET, RHS);
        need_register_load = false;

        emit_are_both_small(generic, LHS, ARG2, RHS, RET);

        a.mov(ARG1, RET);
        a.sar(ARG1, imm(_TAG_IMMED1_SIZE));
        a.js(generic);

        mov_imm(RET, 63);
        a.cmp(ARG1, RET);
        a.cmova(ARG1, RET);

        a.sarx(RET, ARG2, ARG1);
        a.or_(RET, imm(_TAG_IMMED1_SMALL));
        a.short_().jmp(next);
    }

    a.bind(generic);
    if (need_generic) {
        if (need_register_load) {
            mov_arg(RET, RHS);
        }

        if (Fail.get() != 0) {
            safe_fragment_call(ga->get_i_bsr_guard_shared());
            a.je(resolve_beam_label(Fail));
        } else {
            safe_fragment_call(ga->get_i_bsr_body_shared());
        }
    }

    a.bind(next);
    mov_arg(Dst, RET);
}

/* ARG2 (!) = LHS, RET (!) = RHS
 *
 * Result is returned in RET. Error is indicated by ZF. */
void BeamGlobalAssembler::emit_i_bsl_guard_shared() {
    emit_bitwise_fallback_guard<Eterm (*)(Process *, Eterm, Eterm), erts_bsl>();
}

/* ARG2 (!) = LHS, RET (!) = RHS
 *
 * Result is returned in RET. */
void BeamGlobalAssembler::emit_i_bsl_body_shared() {
    static const ErtsCodeMFA bif_mfa = {am_erlang, am_bsl, 2};
    emit_bitwise_fallback_body<Eterm (*)(Process *, Eterm, Eterm), erts_bsl>(
            &bif_mfa);
}

static int count_leading_zeroes(UWord value) {
    const int word_bits = sizeof(value) * CHAR_BIT;

    if (value == 0) {
        return word_bits;
    }

    return Support::clz(value);
}

void BeamModuleAssembler::emit_i_bsl(const ArgSource &LHS,
                                     const ArgSource &RHS,
                                     const ArgLabel &Fail,
                                     const ArgRegister &Dst) {
    if (is_bsl_small(LHS, RHS)) {
        comment("skipped tests because operands and result are always small");
        mov_arg(RET, LHS);
        ERTS_CT_ASSERT(_TAG_IMMED1_MASK == _TAG_IMMED1_SMALL);
        a.xor_(RET, imm(_TAG_IMMED1_MASK));
        if (RHS.isSmall()) {
            a.shl(RET, imm(RHS.as<ArgSmall>().getSigned()));
        } else {
            mov_arg(x86::rcx, RHS);
            a.shr(x86::rcx, imm(_TAG_IMMED1_SIZE));
            a.shl(RET, x86::cl);
        }
        a.or_(RET, imm(_TAG_IMMED1_SMALL));
        mov_arg(Dst, RET);
        return;
    }

    bool inline_shift = hasCpuFeature(CpuFeatures::X86::kLZCNT);
    Label generic = a.newLabel(), next = a.newLabel();

    mov_arg(ARG2, LHS);
    mov_arg(RET, RHS);

    if (LHS.isImmed() && RHS.isImmed()) {
        /* The compiler should've optimized this away, so we'll fall back to the
         * generic path to simplify the inline implementation. */
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
        Operand shiftLimit, shiftCount;

        ASSERT(!(LHS.isImmed() && RHS.isImmed()));

        if (LHS.isRegister()) {
            a.mov(ARG1, ARG2);
            a.mov(ARG3, ARG2);

            /* If LHS is negative we need to invert the leading-zero count
             * below. We leave the tag alone so we can test it later on. */
            a.xor_(ARG3, imm(~_TAG_IMMED1_MASK));
            a.cmovns(ARG1, ARG3);

            /* Get number of leading zeroes in LHS so we can test whether it
             * will overflow before shifting, as `sal` doesn't set the overflow
             * flag on shifts greater than 1. */
            a.lzcnt(ARG3, ARG1);

            if (always_small(LHS)) {
                comment("skipped test for small operand since it is always "
                        "small");
            } else {
                a.and_(ARG1d, imm(_TAG_IMMED1_MASK));
                a.cmp(ARG1d, imm(_TAG_IMMED1_SMALL));
                a.short_().jne(generic);
            }

            shiftLimit = ARG3;
        } else {
            UWord value = LHS.as<ArgSmall>().get();

            if (signed_val(value) < 0) {
                value ^= ~(UWord)_TAG_IMMED1_MASK;
            }

            shiftLimit = imm(count_leading_zeroes(value));
        }

        if (RHS.isRegister()) {
            /* Move RHS to the counter register, as it's the only one that can
             * be used for variable shifts. */
            a.mov(x86::rcx, RET);

            /* Negate the tag bits and then rotate them out, forcing the
             * comparison below to fail for non-smalls. */
            ERTS_CT_ASSERT(_TAG_IMMED1_SMALL == _TAG_IMMED1_MASK);
            a.xor_(x86::rcx, imm(_TAG_IMMED1_SMALL));
            a.ror(x86::rcx, imm(_TAG_IMMED1_SIZE));

            /* Fall back to generic path when the shift magnitude is negative or
             * greater than the leading zero count.
             *
             * The raw emit form is used since `shiftLimit` may be a register
             * or immediate, and the `cmp` helper doesn't accept untyped
             * `Operand`s. */
            a.emit(x86::Inst::kIdCmp, x86::rcx, shiftLimit);
            a.short_().jae(generic);

            shiftCount = x86::cl;
        } else {
            ASSERT(!shiftLimit.isImm());

            shiftCount = imm(RHS.as<ArgSmall>().getSigned());

            a.emit(x86::Inst::kIdCmp, shiftLimit, shiftCount);
            a.short_().jbe(generic);
        }

        a.and_(ARG2, imm(~_TAG_IMMED1_MASK));
        a.emit(x86::Inst::kIdSal, ARG2, shiftCount);

        a.lea(RET, x86::qword_ptr(ARG2, _TAG_IMMED1_SMALL));
        a.short_().jmp(next);
    }

    a.bind(generic);
    {
        if (Fail.get() != 0) {
            safe_fragment_call(ga->get_i_bsl_guard_shared());
            a.je(resolve_beam_label(Fail));
        } else {
            safe_fragment_call(ga->get_i_bsl_body_shared());
        }
    }

    a.bind(next);
    mov_arg(Dst, RET);
}
