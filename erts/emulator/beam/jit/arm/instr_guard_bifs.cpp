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

/*
 * Guard BIF calls using the generic bif1, bif2, and bif3 instructions
 * are expensive. Not only are there two indirect calls (one to the
 * fragment, one to the BIF itself), but the caller-saved X registers
 * must also be saved and restored, and the BIF operands that are
 * usually in CPU registers must be written out to memory.
 *
 * Therefore, guard BIFs that are used fairly frequently and can
 * be implemented entirely in assembly language without any calls to
 * C function are implemented in this source file.
 */

#include <algorithm>
#include <numeric>
#include "beam_asm.hpp"

extern "C"
{
#include "erl_bif_table.h"
#include "big.h"
#include "beam_catches.h"
#include "beam_common.h"
#include "code_ix.h"
#include "erl_map.h"
}

using namespace asmjit;

/* Raise a badarg exception for the given MFA. */
void BeamGlobalAssembler::emit_raise_badarg(const ErtsCodeMFA *mfa) {
    mov_imm(TMP1, BADARG);
    a.str(TMP1, arm::Mem(c_p, offsetof(Process, freason)));
    mov_imm(ARG4, mfa);
    a.b(labels[raise_exception]);
}

/* ================================================================
 *  '=:='/2
 *  '=/='/2
 *  '>='/2
 *  '<'/2
 * ================================================================
 */

void BeamGlobalAssembler::emit_bif_is_eq_exact_shared() {
    Label succ = a.newLabel(), fail = a.newLabel();

    a.cmp(ARG1, ARG2);
    a.b_eq(succ);

    /* The terms could still be equal if both operands are pointers
     * having the same tag. */
    emit_is_unequal_based_on_tags(ARG1, ARG2);
    a.b_eq(fail);

    emit_enter_runtime_frame();
    emit_enter_runtime();

    runtime_call<2>(eq);

    emit_leave_runtime();
    emit_leave_runtime_frame();

    a.cbz(ARG1.w(), fail);

    a.bind(succ);
    {
        mov_imm(ARG1, am_true);
        a.ret(a64::x30);
    }

    a.bind(fail);
    {
        mov_imm(ARG1, am_false);
        a.ret(a64::x30);
    }
}

void BeamGlobalAssembler::emit_bif_is_ne_exact_shared() {
    Label succ = a.newLabel(), fail = a.newLabel();

    a.cmp(ARG1, ARG2);
    a.b_eq(fail);

    emit_is_unequal_based_on_tags(ARG1, ARG2);
    a.b_eq(succ);

    emit_enter_runtime_frame();
    emit_enter_runtime();

    runtime_call<2>(eq);

    emit_leave_runtime();
    emit_leave_runtime_frame();

    a.cbnz(ARG1.w(), fail);

    a.bind(succ);
    {
        mov_imm(ARG1, am_true);
        a.ret(a64::x30);
    }

    a.bind(fail);
    {
        mov_imm(ARG1, am_false);
        a.ret(a64::x30);
    }
}

void BeamModuleAssembler::emit_cond_to_bool(arm::CondCode cc,
                                            const ArgRegister &Dst) {
    auto dst = init_destination(Dst, TMP2);

    mov_imm(TMP3, am_true);
    mov_imm(TMP4, am_false);
    a.csel(dst.reg, TMP3, TMP4, cc);
    flush_var(dst);
}

void BeamModuleAssembler::emit_cmp_immed_to_bool(arm::CondCode cc,
                                                 const ArgSource &LHS,
                                                 const ArgSource &RHS,
                                                 const ArgRegister &Dst) {
    if (RHS.isImmed()) {
        auto lhs = load_source(LHS);
        cmp_arg(lhs.reg, RHS);
    } else {
        auto [lhs, rhs] = load_sources(LHS, TMP1, RHS, TMP2);
        a.cmp(lhs.reg, rhs.reg);
    }
    emit_cond_to_bool(cc, Dst);
}

void BeamModuleAssembler::emit_bif_is_eq_exact(const ArgRegister &LHS,
                                               const ArgSource &RHS,
                                               const ArgRegister &Dst) {
    if (always_immediate(LHS) || always_immediate(RHS)) {
        if (!LHS.isImmed() && !RHS.isImmed()) {
            comment("simplified check since one argument is an immediate");
        }
        emit_cmp_immed_to_bool(arm::CondCode::kEQ, LHS, RHS, Dst);
    } else {
        auto [lhs, rhs] = load_sources(LHS, ARG1, RHS, ARG2);
        auto dst = init_destination(Dst, ARG1);

        mov_var(ARG1, lhs);
        mov_var(ARG2, rhs);
        fragment_call(ga->get_bif_is_eq_exact_shared());
        mov_var(dst, ARG1);
        flush_var(dst);
    }
}

void BeamModuleAssembler::emit_bif_is_ne_exact(const ArgRegister &LHS,
                                               const ArgSource &RHS,
                                               const ArgRegister &Dst) {
    if (always_immediate(LHS) || always_immediate(RHS)) {
        if (!LHS.isImmed() && !RHS.isImmed()) {
            comment("simplified check since one argument is an immediate");
        }
        emit_cmp_immed_to_bool(arm::CondCode::kNE, LHS, RHS, Dst);
    } else {
        auto [lhs, rhs] = load_sources(LHS, ARG1, RHS, ARG2);
        auto dst = init_destination(Dst, ARG1);

        mov_var(ARG1, lhs);
        mov_var(ARG2, rhs);
        fragment_call(ga->get_bif_is_ne_exact_shared());
        mov_var(dst, ARG1);
        flush_var(dst);
    }
}

void BeamModuleAssembler::emit_bif_is_ge_lt(arm::CondCode cc,
                                            const ArgSource &LHS,
                                            const ArgSource &RHS,
                                            const ArgRegister &Dst) {
    auto [lhs, rhs] = load_sources(LHS, ARG1, RHS, ARG2);

    Label generic = a.newLabel(), next = a.newLabel();

    if (always_one_of<BeamTypeId::Integer, BeamTypeId::AlwaysBoxed>(LHS) &&
        always_one_of<BeamTypeId::Integer, BeamTypeId::AlwaysBoxed>(RHS)) {
        /* The only possible kind of immediate is a small and all
         * other values are boxed, so we can test for smalls by
         * testing boxed. */
        comment("simplified small test since all other types are boxed");
        if (always_small(LHS)) {
            emit_is_not_boxed(generic, rhs.reg);
        } else if (always_small(RHS)) {
            emit_is_not_boxed(generic, lhs.reg);
        } else {
            a.and_(TMP1, lhs.reg, rhs.reg);
            emit_is_not_boxed(generic, TMP1);
        }
    } else {
        /* Relative comparisons are overwhelmingly likely to be used
         * on smalls, so we'll specialize those and keep the rest in a
         * shared fragment. */
        if (always_small(RHS)) {
            a.and_(TMP1, lhs.reg, imm(_TAG_IMMED1_MASK));
        } else if (always_small(LHS)) {
            a.and_(TMP1, rhs.reg, imm(_TAG_IMMED1_MASK));
        } else {
            ERTS_CT_ASSERT(_TAG_IMMED1_SMALL == _TAG_IMMED1_MASK);
            a.and_(TMP1, lhs.reg, rhs.reg);
            a.and_(TMP1, TMP1, imm(_TAG_IMMED1_MASK));
        }

        a.cmp(TMP1, imm(_TAG_IMMED1_SMALL));
        a.b_ne(generic);
    }

    a.cmp(lhs.reg, rhs.reg);
    a.b(next);

    a.bind(generic);
    {
        a.cmp(lhs.reg, rhs.reg);
        a.b_eq(next);

        mov_var(ARG1, lhs);
        mov_var(ARG2, rhs);
        fragment_call(ga->get_arith_compare_shared());
    }

    a.bind(next);
    emit_cond_to_bool(cc, Dst);
}

void BeamModuleAssembler::emit_bif_is_ge(const ArgSource &LHS,
                                         const ArgSource &RHS,
                                         const ArgRegister &Dst) {
    if (always_small(LHS) && RHS.isSmall() && RHS.isImmed()) {
        auto lhs = load_source(LHS, ARG1);

        comment("simplified compare because one operand is an immediate small");
        cmp(lhs.reg, RHS.as<ArgImmed>().get());
        emit_cond_to_bool(arm::CondCode::kGE, Dst);

        return;
    } else if (LHS.isSmall() && LHS.isImmed() && always_small(RHS)) {
        auto rhs = load_source(RHS, ARG1);

        comment("simplified compare because one operand is an immediate small");
        cmp(rhs.reg, LHS.as<ArgImmed>().get());
        emit_cond_to_bool(arm::CondCode::kLE, Dst);

        return;
    }

    emit_bif_is_ge_lt(arm::CondCode::kGE, LHS, RHS, Dst);
}

void BeamModuleAssembler::emit_bif_is_lt(const ArgSource &LHS,
                                         const ArgSource &RHS,
                                         const ArgRegister &Dst) {
    emit_bif_is_ge_lt(arm::CondCode::kLT, LHS, RHS, Dst);
}

/* ================================================================
 *  and/2
 * ================================================================
 */

void BeamGlobalAssembler::emit_handle_and_error() {
    static ErtsCodeMFA mfa = {am_erlang, am_and, 2};
    emit_raise_badarg(&mfa);
}

void BeamModuleAssembler::emit_bif_and(const ArgLabel &Fail,
                                       const ArgSource &Src1,
                                       const ArgSource &Src2,
                                       const ArgRegister &Dst) {
    static const Uint diff_bit = am_true - am_false;
    Label next = a.newLabel();

    auto [src1, src2] = load_sources(Src1, TMP1, Src2, TMP2);
    auto dst = init_destination(Dst, TMP3);

    ERTS_CT_ASSERT(am_false == make_atom(0));
    ERTS_CT_ASSERT(am_true == make_atom(1));

    if (exact_type<BeamTypeId::Atom>(Src1) &&
        exact_type<BeamTypeId::Atom>(Src2)) {
        comment("simplified type check because operands are atoms");
        a.orr(TMP3, src1.reg, src2.reg);
        a.tst(TMP3, imm(-1ull << (_TAG_IMMED2_SIZE + 1)));
    } else {
        a.and_(TMP3, src1.reg, imm(_TAG_IMMED2_MASK | ~diff_bit));
        a.and_(TMP4, src2.reg, imm(_TAG_IMMED2_MASK | ~diff_bit));
        a.cmp(TMP3, imm(_TAG_IMMED2_ATOM));
        a.ccmp(TMP3, TMP4, imm(NZCV::kNone), imm(arm::CondCode::kEQ));
    }

    if (Fail.get()) {
        a.b_ne(resolve_beam_label(Fail, disp1MB));
    } else {
        a.b_eq(next);
        mov_var(XREG0, src1);
        mov_var(XREG1, src2);
        fragment_call(ga->get_handle_or_error());
    }

    a.bind(next);
    {
        a.and_(dst.reg, src1.reg, src2.reg);
        flush_var(dst);
    }
}

/* ================================================================
 *  bit_size/1
 * ================================================================
 */
void BeamGlobalAssembler::emit_bif_bit_size_helper(Label error) {
    emit_is_boxed(error, ARG1);
    emit_untag_ptr(TMP3, ARG1);

    ERTS_CT_ASSERT_FIELD_PAIR(ErlHeapBits, thing_word, size);
    a.ldp(TMP1, TMP2, arm::Mem(TMP3));

    Label not_sub_bits = a.newLabel();
    a.cmp(TMP1, imm(HEADER_SUB_BITS));
    a.b_ne(not_sub_bits);
    {
        ERTS_CT_ASSERT_FIELD_PAIR(ErlSubBits, start, end);
        a.ldp(TMP2, TMP3, arm::Mem(TMP3, offsetof(ErlSubBits, start)));
        a.sub(TMP2, TMP3, TMP2);
    }
    a.bind(not_sub_bits);

    const auto mask = _BITSTRING_TAG_MASK & ~_TAG_PRIMARY_MASK;
    ERTS_CT_ASSERT(TAG_PRIMARY_HEADER == 0);
    ERTS_CT_ASSERT(_TAG_HEADER_HEAP_BITS == (_TAG_HEADER_HEAP_BITS & mask));
    a.and_(TMP1, TMP1, imm(mask));
    a.cmp(TMP1, imm(_TAG_HEADER_HEAP_BITS));
    a.b_ne(error);
}

void BeamGlobalAssembler::emit_bif_bit_size_body() {
    Label error = a.newLabel();

    emit_bif_bit_size_helper(error);

    a.lsl(TMP2, TMP2, imm(_TAG_IMMED1_SIZE));
    a.orr(ARG1, TMP2, imm(_TAG_IMMED1_SMALL));

    a.ret(a64::x30);

    a.bind(error);
    {
        static ErtsCodeMFA mfa = {am_erlang, am_bit_size, 1};
        a.mov(XREG0, ARG1);
        emit_raise_badarg(&mfa);
    }
}

void BeamModuleAssembler::emit_bif_bit_size(const ArgLabel &Fail,
                                            const ArgSource &Src,
                                            const ArgRegister &Dst) {
    auto src = load_source(Src, ARG1);
    auto dst = init_destination(Dst, ARG1);

    if (Fail.get() == 0) {
        mov_var(ARG1, src);
        fragment_call(ga->get_bif_bit_size_body());
        mov_var(dst, ARG1);
    } else {
        emit_is_boxed(resolve_beam_label(Fail, dispUnknown), Src, src.reg);
        emit_untag_ptr(ARG1, src.reg);

        ERTS_CT_ASSERT_FIELD_PAIR(ErlHeapBits, thing_word, size);
        a.ldp(TMP1, TMP2, arm::Mem(ARG1));

        Label not_sub_bits = a.newLabel();
        a.cmp(TMP1, imm(HEADER_SUB_BITS));
        a.b_ne(not_sub_bits);
        {
            ERTS_CT_ASSERT_FIELD_PAIR(ErlSubBits, start, end);
            a.ldp(TMP2, TMP3, arm::Mem(ARG1, offsetof(ErlSubBits, start)));
            a.sub(TMP2, TMP3, TMP2);
        }
        a.bind(not_sub_bits);

        if (masked_types<BeamTypeId::MaybeBoxed>(Src) ==
            BeamTypeId::Bitstring) {
            comment("skipped header test since we know it's a bitstring when "
                    "boxed");
        } else {
            const auto mask = _BITSTRING_TAG_MASK & ~_TAG_PRIMARY_MASK;
            ERTS_CT_ASSERT(TAG_PRIMARY_HEADER == 0);
            ERTS_CT_ASSERT(_TAG_HEADER_HEAP_BITS ==
                           (_TAG_HEADER_HEAP_BITS & mask));
            a.and_(TMP1, TMP1, imm(mask));
            a.cmp(TMP1, imm(_TAG_HEADER_HEAP_BITS));
            a.b_ne(resolve_beam_label(Fail, dispUnknown));
        }

        a.lsl(TMP2, TMP2, imm(_TAG_IMMED1_SIZE));
        a.orr(dst.reg, TMP2, imm(_TAG_IMMED1_SMALL));
    }

    flush_var(dst);
}

/* ================================================================
 *  byte_size/1
 * ================================================================
 */

void BeamGlobalAssembler::emit_bif_byte_size_body() {
    Label error = a.newLabel();

    emit_bif_bit_size_helper(error);

    /* Round up to the next byte. */
    a.add(TMP2, TMP2, imm(7));
    a.lsl(TMP2, TMP2, imm(_TAG_IMMED1_SIZE - 3));
    a.orr(ARG1, TMP2, imm(_TAG_IMMED1_SMALL));

    a.ret(a64::x30);

    a.bind(error);
    {
        static ErtsCodeMFA mfa = {am_erlang, am_byte_size, 1};
        a.mov(XREG0, ARG1);
        emit_raise_badarg(&mfa);
    }
}

void BeamModuleAssembler::emit_bif_byte_size(const ArgLabel &Fail,
                                             const ArgSource &Src,
                                             const ArgRegister &Dst) {
    auto src = load_source(Src, ARG1);
    auto dst = init_destination(Dst, ARG1);

    if (Fail.get() == 0) {
        mov_var(ARG1, src);
        fragment_call(ga->get_bif_byte_size_body());
        mov_var(dst, ARG1);
    } else {
        emit_is_boxed(resolve_beam_label(Fail, dispUnknown), Src, src.reg);
        emit_untag_ptr(ARG1, src.reg);

        ERTS_CT_ASSERT_FIELD_PAIR(ErlHeapBits, thing_word, size);
        a.ldp(TMP1, TMP2, arm::Mem(ARG1));

        Label not_sub_bits = a.newLabel();
        a.cmp(TMP1, imm(HEADER_SUB_BITS));
        a.b_ne(not_sub_bits);
        {
            ERTS_CT_ASSERT_FIELD_PAIR(ErlSubBits, start, end);
            a.ldp(TMP2, TMP3, arm::Mem(ARG1, offsetof(ErlSubBits, start)));
            a.sub(TMP2, TMP3, TMP2);
        }
        a.bind(not_sub_bits);

        if (masked_types<BeamTypeId::MaybeBoxed>(Src) ==
            BeamTypeId::Bitstring) {
            comment("skipped header test since we know it's a bitstring when "
                    "boxed");
        } else {
            const auto mask = _BITSTRING_TAG_MASK & ~_TAG_PRIMARY_MASK;
            ERTS_CT_ASSERT(TAG_PRIMARY_HEADER == 0);
            ERTS_CT_ASSERT(_TAG_HEADER_HEAP_BITS ==
                           (_TAG_HEADER_HEAP_BITS & mask));
            a.and_(TMP1, TMP1, imm(mask));
            a.cmp(TMP1, imm(_TAG_HEADER_HEAP_BITS));
            a.b_ne(resolve_beam_label(Fail, dispUnknown));
        }

        /* Round up to the next byte. */
        a.add(TMP2, TMP2, imm(7));
        a.lsl(TMP2, TMP2, imm(_TAG_IMMED1_SIZE - 3));
        a.orr(dst.reg, TMP2, imm(_TAG_IMMED1_SMALL));
    }

    flush_var(dst);
}

/* ================================================================
 *  element/2
 * ================================================================
 */

/* ARG1 = Position (1-based)
 * ARG2 = Tuple
 *
 * Will return the result in ARG1, or jump to the label `fail` if
 * the operation fails.
 */
void BeamGlobalAssembler::emit_bif_element_helper(Label fail) {
    /* Ensure that ARG2 contains a tuple. */
    emit_is_boxed(fail, ARG2);
    a64::Gp boxed_ptr = emit_ptr_val(TMP1, ARG2);
    lea(TMP1, emit_boxed_val(boxed_ptr));
    a.ldr(TMP2, arm::Mem(TMP1));
    ERTS_CT_ASSERT(make_arityval_zero() == 0);
    a.tst(TMP2, imm(_TAG_HEADER_MASK));
    a.b_ne(fail);

    a.and_(TMP3, ARG1, imm(_TAG_IMMED1_MASK));
    a.cmp(TMP3, imm(_TAG_IMMED1_SMALL));
    a.ccmp(ARG1, make_small(0), imm(NZCV::kZF), imm(arm::CondCode::kEQ));
    a.b_eq(fail);

    /* Ensure that the position points within the tuple. */
    a.asr(TMP3, ARG1, imm(_TAG_IMMED1_SIZE));
    a.cmp(TMP3, TMP2, arm::lsr(_HEADER_ARITY_OFFS));
    a.b_hi(fail);

    a.ldr(ARG1, arm::Mem(TMP1, TMP3, arm::lsl(3)));
    a.ret(a64::x30);
}

void BeamGlobalAssembler::emit_bif_element_body_shared() {
    Label error = a.newLabel();

    emit_bif_element_helper(error);

    a.bind(error);
    {
        static ErtsCodeMFA mfa = {am_erlang, am_element, 2};
        a.mov(XREG0, ARG1);
        a.mov(XREG1, ARG2);
        emit_raise_badarg(&mfa);
    }
}

void BeamGlobalAssembler::emit_bif_element_guard_shared() {
    Label error = a.newLabel();

    emit_bif_element_helper(error);

    a.bind(error);
    {
        mov_imm(ARG1, THE_NON_VALUE);
        a.ret(a64::x30);
    }
}

void BeamGlobalAssembler::emit_handle_element_error_shared() {
    static ErtsCodeMFA mfa = {am_erlang, am_element, 2};
    a.mov(XREG0, ARG1);
    a.mov(XREG1, ARG2);
    emit_raise_badarg(&mfa);
}

void BeamModuleAssembler::emit_bif_element(const ArgLabel &Fail,
                                           const ArgSource &Pos,
                                           const ArgSource &Tuple,
                                           const ArgRegister &Dst) {
    /*
     * Try to optimize the use of a tuple as a lookup table.
     */
    if (exact_type<BeamTypeId::Integer>(Pos) && Tuple.isLiteral()) {
        Eterm tuple_literal =
                beamfile_get_literal(beam, Tuple.as<ArgLiteral>().get());

        if (is_tuple(tuple_literal)) {
            Label next = a.newLabel(), fail = a.newLabel();
            Sint size = Sint(arityval(*tuple_val(tuple_literal)));
            auto [min, max] = getClampedRange(Pos);
            bool can_fail = min < 1 || size < max;
            auto [pos, tuple] = load_sources(Pos, ARG3, Tuple, ARG4);
            auto dst = init_destination(Dst, ARG1);

            if (always_small(Pos)) {
                comment("skipped test for small position since it is always "
                        "small");
            } else {
                comment("simplified test for small position since it is an "
                        "integer");
                emit_is_not_boxed(fail, pos.reg);
            }

            comment("skipped tuple test since source is always a literal "
                    "tuple");
            a64::Gp boxed_ptr = emit_ptr_val(TMP1, tuple.reg);
            lea(TMP1, emit_boxed_val(boxed_ptr));

            a.asr(TMP3, pos.reg, imm(_TAG_IMMED1_SIZE));

            if (min >= 1) {
                comment("skipped check for position >= 1");
            } else {
                a.cmp(TMP3, imm(1));
                a.b_mi(fail);
            }

            if (size >= max) {
                comment("skipped check for position beyond tuple");
            } else {
                mov_imm(TMP2, size);
                a.cmp(TMP2, TMP3);
                a.b_lo(fail);
            }

            a.ldr(dst.reg, arm::Mem(TMP1, TMP3, arm::lsl(3)));

            if (can_fail) {
                a.b(next);
            }

            a.bind(fail);
            if (can_fail) {
                if (Fail.get() != 0) {
                    a.b(resolve_beam_label(Fail, disp128MB));
                } else {
                    a.mov(ARG1, pos.reg);
                    a.mov(ARG2, tuple.reg);
                    fragment_call(ga->get_handle_element_error_shared());
                }
            }

            a.bind(next);
            flush_var(dst);

            return;
        }
    }

    bool const_position;

    const_position = Pos.isSmall() && Pos.as<ArgSmall>().getSigned() > 0 &&
                     Pos.as<ArgSmall>().getSigned() <= (Sint)MAX_ARITYVAL;

    if (const_position) {
        if (exact_type<BeamTypeId::Tuple>(Tuple)) {
            comment("simplified element/2 because arguments are known types");
        } else {
            comment("simplified element/2 because position is constant");
        }
        auto tuple = load_source(Tuple, ARG2);
        auto dst = init_destination(Dst, ARG1);
        Uint position = Pos.as<ArgSmall>().getUnsigned();
        a64::Gp boxed_ptr;
        Label fail = a.newLabel();

        if (exact_type<BeamTypeId::Tuple>(Tuple)) {
            boxed_ptr = emit_ptr_val(TMP1, tuple.reg);
            a.ldur(TMP2, emit_boxed_val(boxed_ptr));
            ERTS_CT_ASSERT(make_arityval_zero() == 0);
            cmp(TMP2, position << _HEADER_ARITY_OFFS);
        } else {
            if (Fail.get() != 0) {
                emit_is_boxed(resolve_beam_label(Fail, dispUnknown),
                              Tuple,
                              tuple.reg);
            } else {
                emit_is_boxed(fail, Tuple, tuple.reg);
            }
            boxed_ptr = emit_ptr_val(TMP1, tuple.reg);
            a.ldur(TMP2, emit_boxed_val(boxed_ptr));
            mov_imm(TMP3, position << _HEADER_ARITY_OFFS);
            ERTS_CT_ASSERT(make_arityval_zero() == 0);
            a.tst(TMP2, imm(_TAG_HEADER_MASK));
            a.ccmp(TMP2, TMP3, imm(NZCV::kNone), imm(arm::CondCode::kEQ));
        }

        if (Fail.get() != 0) {
            a.b_lo(resolve_beam_label(Fail, disp1MB));
            a.bind(fail);
        } else {
            Label good = a.newLabel();

            a.b_hs(good);

            a.bind(fail);
            mov_arg(ARG1, Pos);
            mov_var(ARG2, tuple);
            fragment_call(ga->get_handle_element_error_shared());

            a.bind(good);
        }

        /* Fetch the element. */
        safe_ldur(dst.reg, emit_boxed_val(boxed_ptr, position << 3));
        flush_var(dst);
    } else if (exact_type<BeamTypeId::Tuple>(Tuple) && Fail.get() == 0) {
        auto [pos, tuple] = load_sources(Pos, ARG1, Tuple, ARG2);
        auto dst = init_destination(Dst, ARG1);
        a64::Gp boxed_ptr = emit_ptr_val(TMP1, tuple.reg);
        Label fail = a.newLabel();
        Label good = a.newLabel();

        lea(TMP1, emit_boxed_val(boxed_ptr));
        a.ldr(TMP2, arm::Mem(TMP1));

        if (always_one_of<BeamTypeId::Integer, BeamTypeId::AlwaysBoxed>(Pos)) {
            ERTS_CT_ASSERT(_TAG_PRIMARY_MASK - TAG_PRIMARY_LIST ==
                           TAG_PRIMARY_BOXED);
            a.tst(pos.reg, imm(TAG_PRIMARY_LIST));
            a.ccmp(pos.reg,
                   make_small(0),
                   imm(NZCV::kZF),
                   imm(arm::CondCode::kNE));
        } else {
            a.and_(TMP3, pos.reg, imm(_TAG_IMMED1_MASK));
            a.cmp(TMP3, imm(_TAG_IMMED1_SMALL));
            a.ccmp(pos.reg,
                   make_small(0),
                   imm(NZCV::kZF),
                   imm(arm::CondCode::kEQ));
        }
        a.b_eq(fail);

        /* Ensure that the position points within the tuple. */
        a.asr(TMP3, pos.reg, imm(_TAG_IMMED1_SIZE));
        a.cmp(TMP3, TMP2, arm::lsr(_HEADER_ARITY_OFFS));
        a.b_ls(good);

        a.bind(fail);
        mov_arg(ARG1, Pos);
        mov_var(ARG2, tuple);
        fragment_call(ga->get_handle_element_error_shared());

        a.bind(good);
        a.ldr(dst.reg, arm::Mem(TMP1, TMP3, arm::lsl(3)));
        flush_var(dst);
    } else {
        /* Too much code to inline. Call a helper fragment. */
        mov_arg(ARG1, Pos);
        mov_arg(ARG2, Tuple);

        if (Fail.get() != 0) {
            fragment_call(ga->get_bif_element_guard_shared());
            emit_branch_if_not_value(ARG1,
                                     resolve_beam_label(Fail, dispUnknown));
        } else {
            fragment_call(ga->get_bif_element_body_shared());
        }

        auto dst = init_destination(Dst, ARG1);
        mov_var(dst, ARG1);
        flush_var(dst);
    }
}

/* ================================================================
 *  hd/1
 * ================================================================
 */

void BeamGlobalAssembler::emit_handle_hd_error() {
    static ErtsCodeMFA mfa = {am_erlang, am_hd, 1};
    emit_raise_badarg(&mfa);
}

void BeamModuleAssembler::emit_bif_hd(const ArgSource &Src,
                                      const ArgRegister &Hd) {
    Label good_cons = a.newLabel();
    auto src = load_source(Src, TMP1);
    auto hd = init_destination(Hd, TMP2);
    const int bitNumber = 1;

    ERTS_CT_ASSERT(_TAG_PRIMARY_MASK - TAG_PRIMARY_LIST == (1 << bitNumber));

    a.tbz(src.reg, imm(bitNumber), good_cons);
    mov_var(XREG0, src);
    fragment_call(ga->get_handle_hd_error());

    a.bind(good_cons);
    {
        a64::Gp cons_ptr = emit_ptr_val(TMP1, src.reg);
        a.ldur(hd.reg, getCARRef(cons_ptr));
        flush_var(hd);
    }
}

/* ================================================================
 *  is_map_key/2
 * ================================================================
 */

void BeamModuleAssembler::emit_bif_is_map_key(const ArgWord &Bif,
                                              const ArgLabel &Fail,
                                              const ArgSource &Key,
                                              const ArgSource &Src,
                                              const ArgRegister &Dst) {
    if (!exact_type<BeamTypeId::Map>(Src)) {
        emit_i_bif2(Key, Src, Fail, Bif, Dst);
        return;
    }

    comment("inlined BIF is_map_key/2");

    mov_arg(ARG1, Src);
    mov_arg(ARG2, Key);

    if (maybe_one_of<BeamTypeId::MaybeImmediate>(Key)) {
        fragment_call(ga->get_i_get_map_element_shared());
        emit_cond_to_bool(arm::CondCode::kEQ, Dst);
    } else {
        emit_enter_runtime();
        runtime_call<2>(get_map_element);
        emit_leave_runtime();

        cmp(ARG1, THE_NON_VALUE);
        emit_cond_to_bool(arm::CondCode::kNE, Dst);
    }
}

/* ================================================================
 *  map_get/2
 * ================================================================
 */

void BeamGlobalAssembler::emit_handle_map_get_badmap() {
    static ErtsCodeMFA mfa = {am_erlang, am_map_get, 2};
    mov_imm(TMP1, BADMAP);
    ERTS_CT_ASSERT_FIELD_PAIR(Process, freason, fvalue);
    a.stp(TMP1, ARG1, arm::Mem(c_p, offsetof(Process, freason)));
    a.mov(XREG0, ARG2);
    a.mov(XREG1, ARG1);
    mov_imm(ARG4, &mfa);
    a.b(labels[raise_exception]);
}

void BeamGlobalAssembler::emit_handle_map_get_badkey() {
    static ErtsCodeMFA mfa = {am_erlang, am_map_get, 2};
    mov_imm(TMP1, BADKEY);
    ERTS_CT_ASSERT_FIELD_PAIR(Process, freason, fvalue);
    a.stp(TMP1, ARG2, arm::Mem(c_p, offsetof(Process, freason)));
    a.mov(XREG0, ARG2);
    a.mov(XREG1, ARG1);
    mov_imm(ARG4, &mfa);
    a.b(labels[raise_exception]);
}

void BeamModuleAssembler::emit_bif_map_get(const ArgLabel &Fail,
                                           const ArgSource &Key,
                                           const ArgSource &Src,
                                           const ArgRegister &Dst) {
    Label good_key = a.newLabel();

    mov_arg(ARG1, Src);
    mov_arg(ARG2, Key);

    if (exact_type<BeamTypeId::Map>(Src)) {
        comment("skipped test for map for known map argument");
    } else {
        Label bad_map = a.newLabel();
        Label good_map = a.newLabel();

        if (Fail.get() == 0) {
            emit_is_boxed(bad_map, Src, ARG1);
        } else {
            emit_is_boxed(resolve_beam_label(Fail, dispUnknown), Src, ARG1);
        }

        /* As an optimization for the `error | #{}` case, skip checking the
         * header word when we know that the only possible boxed type
         * is a map. */
        if (masked_types<BeamTypeId::MaybeBoxed>(Src) == BeamTypeId::Map) {
            comment("skipped header test since we know it's a map when boxed");
            if (Fail.get() == 0) {
                a.b(good_map);
            }
        } else {
            a64::Gp boxed_ptr = emit_ptr_val(TMP1, ARG1);
            a.ldur(TMP1, emit_boxed_val(boxed_ptr));
            a.and_(TMP1, TMP1, imm(_TAG_HEADER_MASK));
            a.cmp(TMP1, imm(_TAG_HEADER_MAP));
            if (Fail.get() == 0) {
                a.b_eq(good_map);
            } else {
                a.b_ne(resolve_beam_label(Fail, disp1MB));
            }
        }

        a.bind(bad_map);
        if (Fail.get() == 0) {
            fragment_call(ga->get_handle_map_get_badmap());
        }

        a.bind(good_map);
    }

    if (maybe_one_of<BeamTypeId::MaybeImmediate>(Key)) {
        fragment_call(ga->get_i_get_map_element_shared());
        if (Fail.get() == 0) {
            a.b_eq(good_key);
        } else {
            a.b_ne(resolve_beam_label(Fail, disp1MB));
        }
    } else {
        emit_enter_runtime();
        runtime_call<2>(get_map_element);
        emit_leave_runtime();

        if (Fail.get() == 0) {
            emit_branch_if_value(ARG1, good_key);
        } else {
            emit_branch_if_not_value(ARG1,
                                     resolve_beam_label(Fail, dispUnknown));
        }
    }

    if (Fail.get() == 0) {
        mov_arg(ARG1, Src);
        mov_arg(ARG2, Key);
        fragment_call(ga->get_handle_map_get_badkey());
    }

    a.bind(good_key);
    mov_arg(Dst, ARG1);
}

/* ================================================================
 *  map_size/1
 * ================================================================
 */

void BeamGlobalAssembler::emit_handle_map_size_error() {
    static ErtsCodeMFA mfa = {am_erlang, am_map_size, 1};
    mov_imm(TMP1, BADMAP);
    ERTS_CT_ASSERT_FIELD_PAIR(Process, freason, fvalue);
    a.stp(TMP1, XREG0, arm::Mem(c_p, offsetof(Process, freason)));
    mov_imm(ARG4, &mfa);
    a.b(labels[raise_exception]);
}

void BeamModuleAssembler::emit_bif_map_size(const ArgLabel &Fail,
                                            const ArgSource &Src,
                                            const ArgRegister &Dst) {
    Label error = a.newLabel(), good_map = a.newLabel();
    auto src = load_source(Src, TMP1);
    auto dst = init_destination(Dst, TMP2);

    if (Fail.get() == 0) {
        emit_is_boxed(error, Src, src.reg);
    } else {
        emit_is_boxed(resolve_beam_label(Fail, dispUnknown), Src, src.reg);
    }

    a64::Gp boxed_ptr = emit_ptr_val(TMP3, src.reg);

    if (exact_type<BeamTypeId::Map>(Src)) {
        comment("skipped type check because the argument is always a map");
        a.bind(error); /* Never referenced. */
    } else {
        a.ldur(TMP4, emit_boxed_val(boxed_ptr));
        a.and_(TMP4, TMP4, imm(_TAG_HEADER_MASK));
        a.cmp(TMP4, imm(_TAG_HEADER_MAP));

        if (Fail.get() == 0) {
            a.b_eq(good_map);
            a.bind(error);
            {
                mov_var(XREG0, src);
                fragment_call(ga->get_handle_map_size_error());
            }
        } else {
            a.b_ne(resolve_beam_label(Fail, disp1MB));
            a.bind(error); /* Never referenced. */
        }
    }

    a.bind(good_map);
    {
        ERTS_CT_ASSERT(offsetof(flatmap_t, size) == sizeof(Eterm));
        a.ldur(TMP1, emit_boxed_val(boxed_ptr, sizeof(Eterm)));
        mov_imm(dst.reg, _TAG_IMMED1_SMALL);
        a.bfi(dst.reg, TMP1, imm(_TAG_IMMED1_SIZE), imm(SMALL_BITS));
        flush_var(dst);
    }
}

/* ================================================================
 *  min/2
 *  max/2
 * ================================================================
 */

void BeamModuleAssembler::emit_bif_min_max(arm::CondCode cc,
                                           const ArgSource &LHS,
                                           const ArgSource &RHS,
                                           const ArgRegister &Dst) {
    auto [lhs, rhs] = load_sources(LHS, ARG1, RHS, ARG2);
    auto dst = init_destination(Dst, ARG1);
    bool both_small = always_small(LHS) && always_small(RHS);
    bool need_generic = !both_small;
    Label generic = a.newLabel(), next = a.newLabel();

    if (both_small) {
        comment("skipped test for small operands since they are always small");
    } else if (always_small(RHS) &&
               always_one_of<BeamTypeId::Integer, BeamTypeId::AlwaysBoxed>(
                       LHS)) {
        comment("simplified test for small operand");
        emit_is_not_boxed(generic, lhs.reg);
    } else if (always_small(LHS) &&
               always_one_of<BeamTypeId::Integer, BeamTypeId::AlwaysBoxed>(
                       RHS)) {
        comment("simplified test for small operand");
        emit_is_not_boxed(generic, rhs.reg);
    } else if (always_one_of<BeamTypeId::Integer, BeamTypeId::AlwaysBoxed>(
                       LHS) &&
               always_one_of<BeamTypeId::Integer, BeamTypeId::AlwaysBoxed>(
                       RHS)) {
        comment("simplified test for small operands");
        ERTS_CT_ASSERT(_TAG_IMMED1_SMALL == _TAG_IMMED1_MASK);
        a.and_(TMP1, lhs.reg, rhs.reg);
        emit_is_not_boxed(generic, TMP1);
    } else {
        if (RHS.isSmall()) {
            a.and_(TMP1, lhs.reg, imm(_TAG_IMMED1_MASK));
        } else if (LHS.isSmall()) {
            a.and_(TMP1, rhs.reg, imm(_TAG_IMMED1_MASK));
        } else {
            /* Avoid the expensive generic comparison for equal terms. */
            a.cmp(lhs.reg, rhs.reg);
            a.b_eq(next);

            ERTS_CT_ASSERT(_TAG_IMMED1_SMALL == _TAG_IMMED1_MASK);
            a.and_(TMP1, lhs.reg, rhs.reg);
            a.and_(TMP1, TMP1, imm(_TAG_IMMED1_MASK));
        }

        a.cmp(TMP1, imm(_TAG_IMMED1_SMALL));
        a.b_ne(generic);
    }

    /* Both arguments are smalls. */
    a.cmp(lhs.reg, rhs.reg);
    if (need_generic) {
        a.b(next);
    }

    a.bind(generic);
    if (need_generic) {
        mov_var(ARG1, lhs);
        mov_var(ARG2, rhs);
        fragment_call(ga->get_arith_compare_shared());
        load_sources(LHS, ARG1, RHS, ARG2);
    }

    a.bind(next);
    a.csel(dst.reg, rhs.reg, lhs.reg, cc);
    flush_var(dst);
}

void BeamModuleAssembler::emit_bif_max(const ArgSource &LHS,
                                       const ArgSource &RHS,
                                       const ArgRegister &Dst) {
    emit_bif_min_max(arm::CondCode::kLT, LHS, RHS, Dst);
}

void BeamModuleAssembler::emit_bif_min(const ArgSource &LHS,
                                       const ArgSource &RHS,
                                       const ArgRegister &Dst) {
    emit_bif_min_max(arm::CondCode::kGT, LHS, RHS, Dst);
}

/* ================================================================
 *  node/1
 * ================================================================
 */

void BeamGlobalAssembler::emit_handle_node_error() {
    static ErtsCodeMFA mfa = {am_erlang, am_node, 1};
    emit_raise_badarg(&mfa);
}

void BeamModuleAssembler::emit_bif_node(const ArgLabel &Fail,
                                        const ArgRegister &Src,
                                        const ArgRegister &Dst) {
    bool always_identifier = always_one_of<BeamTypeId::Identifier>(Src);
    Label test_internal = a.newLabel();
    Label internal = a.newLabel();
    Label next = a.newLabel();
    auto src = load_source(Src, ARG2);
    Label fail;

    if (Fail.get() != 0) {
        fail = resolve_beam_label(Fail, dispUnknown);
    } else if (!always_identifier) {
        fail = a.newLabel();
    }

    emit_is_boxed(test_internal, Src, src.reg);

    a64::Gp boxed_ptr = emit_ptr_val(TMP1, src.reg);

    if (!always_one_of<BeamTypeId::Pid, BeamTypeId::Port>(Src)) {
        a.ldur(TMP2, emit_boxed_val(boxed_ptr));
        a.and_(TMP2, TMP2, imm(_TAG_HEADER_MASK));
    }

    if (maybe_one_of<BeamTypeId::Reference>(Src)) {
        a.cmp(TMP2, imm(_TAG_HEADER_REF));
        a.b_eq(internal);
    }

    if (!always_identifier) {
        Label external = a.newLabel();
        ERTS_CT_ASSERT((_TAG_HEADER_EXTERNAL_PORT - _TAG_HEADER_EXTERNAL_PID) >>
                               _TAG_PRIMARY_SIZE ==
                       1);
        ERTS_CT_ASSERT((_TAG_HEADER_EXTERNAL_REF - _TAG_HEADER_EXTERNAL_PORT) >>
                               _TAG_PRIMARY_SIZE ==
                       1);
        a.sub(TMP3, TMP2, imm(_TAG_HEADER_EXTERNAL_PID));
        a.cmp(TMP3, imm(_TAG_HEADER_EXTERNAL_REF - _TAG_HEADER_EXTERNAL_PID));

        if (Fail.get() != 0) {
            a.b_hi(fail);
        } else {
            a.b_ls(external);

            a.bind(fail);
            {
                mov_var(XREG0, src);
                fragment_call(ga->get_handle_node_error());
            }
        }

        a.bind(external);
    }

    a.ldur(TMP1, emit_boxed_val(boxed_ptr, offsetof(ExternalThing, node)));
    a.b(next);

    a.bind(test_internal);
    if (!always_identifier) {
        a.and_(TMP1, src.reg, imm(_TAG_IMMED1_MASK));
        a.cmp(TMP1, imm(_TAG_IMMED1_PID));
        a.ccmp(TMP1,
               imm(_TAG_IMMED1_PORT),
               imm(NZCV::kEqual),
               imm(arm::CondCode::kNE));
        a.b_ne(fail);
    }

    a.bind(internal);
    a.ldr(TMP1, embed_constant(&erts_this_node, disp32K));
    a.ldr(TMP1, arm::Mem(TMP1));

    a.bind(next);
    mov_arg(Dst, arm::Mem(TMP1, offsetof(ErlNode, sysname)));
}

/* ================================================================
 *  not/1
 * ================================================================
 */

void BeamGlobalAssembler::emit_handle_not_error() {
    static ErtsCodeMFA mfa = {am_erlang, am_not, 1};
    emit_raise_badarg(&mfa);
}

void BeamModuleAssembler::emit_bif_not(const ArgLabel &Fail,
                                       const ArgRegister &Src,
                                       const ArgRegister &Dst) {
    Label next = a.newLabel();
    auto src = load_source(Src, TMP1);
    auto dst = init_destination(Dst, TMP2);
    ERTS_CT_ASSERT(am_false == make_atom(0));
    ERTS_CT_ASSERT(am_true == make_atom(1));
    Uint diff_bit = am_true - am_false;

    a.and_(TMP3, src.reg, imm(_TAG_IMMED2_MASK | ~diff_bit));
    a.cmp(TMP3, imm(_TAG_IMMED2_ATOM));

    if (Fail.get() == 0) {
        a.b_eq(next);
        mov_var(XREG0, src);
        fragment_call(ga->get_handle_not_error());
    } else {
        a.b_ne(resolve_beam_label(Fail, disp1MB));
    }

    a.bind(next);
    {
        a.eor(dst.reg, src.reg, imm(diff_bit));
        flush_var(dst);
    }
}

/* ================================================================
 *  or/2
 * ================================================================
 */

void BeamGlobalAssembler::emit_handle_or_error() {
    static ErtsCodeMFA mfa = {am_erlang, am_or, 2};
    emit_raise_badarg(&mfa);
}

void BeamModuleAssembler::emit_bif_or(const ArgLabel &Fail,
                                      const ArgSource &Src1,
                                      const ArgSource &Src2,
                                      const ArgRegister &Dst) {
    static const Uint diff_bit = am_true - am_false;
    Label next = a.newLabel();

    auto [src1, src2] = load_sources(Src1, TMP1, Src2, TMP2);
    auto dst = init_destination(Dst, TMP3);

    ERTS_CT_ASSERT(am_false == make_atom(0));
    ERTS_CT_ASSERT(am_true == make_atom(1));

    if (exact_type<BeamTypeId::Atom>(Src1) &&
        exact_type<BeamTypeId::Atom>(Src2)) {
        comment("simplified type check because operands are atoms");
        a.orr(TMP3, src1.reg, src2.reg);
        a.tst(TMP3, imm(-1ull << (_TAG_IMMED2_SIZE + 1)));
    } else {
        a.and_(TMP3, src1.reg, imm(_TAG_IMMED2_MASK | ~diff_bit));
        a.and_(TMP4, src2.reg, imm(_TAG_IMMED2_MASK | ~diff_bit));
        a.cmp(TMP3, imm(_TAG_IMMED2_ATOM));
        a.ccmp(TMP3, TMP4, imm(NZCV::kNone), imm(arm::CondCode::kEQ));
    }

    if (Fail.get()) {
        a.b_ne(resolve_beam_label(Fail, disp1MB));
    } else {
        a.b_eq(next);
        mov_var(XREG0, src1);
        mov_var(XREG1, src2);
        fragment_call(ga->get_handle_or_error());
    }

    a.bind(next);
    {
        a.orr(dst.reg, src1.reg, src2.reg);
        flush_var(dst);
    }
}

/* ================================================================
 *  tl/1
 * ================================================================
 */

void BeamGlobalAssembler::emit_handle_tl_error() {
    static ErtsCodeMFA mfa = {am_erlang, am_tl, 1};
    emit_raise_badarg(&mfa);
}

void BeamModuleAssembler::emit_bif_tl(const ArgSource &Src,
                                      const ArgRegister &Tl) {
    Label good_cons = a.newLabel();
    auto src = load_source(Src, TMP1);
    auto tl = init_destination(Tl, TMP2);
    const int bitNumber = 1;

    ERTS_CT_ASSERT(_TAG_PRIMARY_MASK - TAG_PRIMARY_LIST == (1 << bitNumber));

    a.tbz(src.reg, imm(bitNumber), good_cons);
    mov_var(XREG0, src);
    fragment_call(ga->get_handle_tl_error());

    a.bind(good_cons);
    {
        a64::Gp cons_ptr = emit_ptr_val(TMP1, src.reg);
        a.ldur(tl.reg, getCDRRef(cons_ptr));
        flush_var(tl);
    }
}

/* ================================================================
 *  tuple_size/1
 * ================================================================
 */

void BeamGlobalAssembler::emit_bif_tuple_size_helper(Label fail) {
    a64::Gp boxed_ptr = emit_ptr_val(TMP1, ARG1);

    emit_is_boxed(fail, boxed_ptr);

    ERTS_CT_ASSERT(_TAG_HEADER_ARITYVAL == 0);
    a.ldur(TMP1, emit_boxed_val(boxed_ptr));
    a.tst(TMP1, imm(_TAG_HEADER_MASK));
    a.b_ne(fail);

    ERTS_CT_ASSERT(_HEADER_ARITY_OFFS - _TAG_IMMED1_SIZE > 0);
    ERTS_CT_ASSERT(_TAG_IMMED1_SMALL == _TAG_IMMED1_MASK);
    a.lsr(TMP1, TMP1, _HEADER_ARITY_OFFS - _TAG_IMMED1_SIZE);
    a.orr(ARG1, TMP1, imm(_TAG_IMMED1_SMALL));

    a.ret(a64::x30);
}

void BeamGlobalAssembler::emit_bif_tuple_size_body() {
    Label error = a.newLabel();

    emit_bif_tuple_size_helper(error);

    a.bind(error);
    {
        static ErtsCodeMFA mfa = {am_erlang, am_tuple_size, 1};
        a.mov(XREG0, ARG1);
        emit_raise_badarg(&mfa);
    }
}

void BeamGlobalAssembler::emit_bif_tuple_size_guard() {
    Label error = a.newLabel();

    emit_bif_tuple_size_helper(error);

    a.bind(error);
    {
        mov_imm(ARG1, THE_NON_VALUE);
        a.ret(a64::x30);
    }
}

void BeamModuleAssembler::emit_bif_tuple_size(const ArgLabel &Fail,
                                              const ArgRegister &Src,
                                              const ArgRegister &Dst) {
    auto src = load_source(Src, ARG1);
    auto dst = init_destination(Dst, ARG1);

    if (exact_type<BeamTypeId::Tuple>(Src)) {
        comment("simplifed tuple_size/1 because the argument is always a "
                "tuple");
        a64::Gp boxed_ptr = emit_ptr_val(TMP1, src.reg);
        a.ldur(TMP1, emit_boxed_val(boxed_ptr));
        ERTS_CT_ASSERT(_HEADER_ARITY_OFFS - _TAG_IMMED1_SIZE > 0);
        ERTS_CT_ASSERT(_TAG_IMMED1_SMALL == _TAG_IMMED1_MASK);
        a.lsr(TMP1, TMP1, _HEADER_ARITY_OFFS - _TAG_IMMED1_SIZE);
        a.orr(dst.reg, TMP1, imm(_TAG_IMMED1_SMALL));
    } else {
        mov_var(ARG1, src);

        if (Fail.get() == 0) {
            fragment_call(ga->get_bif_tuple_size_body());
        } else {
            fragment_call(ga->get_bif_tuple_size_guard());
            emit_branch_if_not_value(ARG1,
                                     resolve_beam_label(Fail, dispUnknown));
        }

        mov_var(dst, ARG1);
    }
    flush_var(dst);
}
