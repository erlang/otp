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
    mov_imm(TMP, BADARG);
    a.str(TMP, arm::Mem(c_p, offsetof(Process, freason)));
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

    /* Terms may still be equal if both are pointers with same tag. */
    emit_is_unequal_based_on_tags(ARG1, ARG2);
    a.b_eq(fail);

    emit_enter_runtime_frame();
    emit_enter_runtime();

    runtime_call<2>(eq);

    emit_leave_runtime();
    emit_leave_runtime_frame();

    a.cmp(ARG1, imm(0));
    a.b_eq(fail);

    a.bind(succ);
    {
        mov_imm(ARG1, am_true);
        a.bx(a32::lr);
    }

    a.bind(fail);
    {
        mov_imm(ARG1, am_false);
        a.bx(a32::lr);
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

    a.cmp(ARG1, imm(0));
    a.b_ne(fail);

    a.bind(succ);
    {
        mov_imm(ARG1, am_true);
        a.bx(a32::lr);
    }

    a.bind(fail);
    {
        mov_imm(ARG1, am_false);
        a.bx(a32::lr);
    }
}

void BeamModuleAssembler::emit_cond_to_bool(arm::CondCode cc,
                                            const ArgRegister &Dst) {
    Label set_true = a.newLabel(), done = a.newLabel();
    auto dst = init_destination(Dst, TMP);

    switch (cc) {
    case arm::CondCode::kEQ:
        a.b_eq(set_true);
        break;
    case arm::CondCode::kNE:
        a.b_ne(set_true);
        break;
    case arm::CondCode::kLT:
        a.b_lt(set_true);
        break;
    case arm::CondCode::kLE:
        a.b_le(set_true);
        break;
    case arm::CondCode::kGT:
        a.b_gt(set_true);
        break;
    case arm::CondCode::kGE:
        a.b_ge(set_true);
        break;
    default:
        ASSERT(!"Unsupported condition code in emit_cond_to_bool");
        break;
    }

    mov_imm(dst.reg, am_false);
    a.b(done);

    a.bind(set_true);
    mov_imm(dst.reg, am_true);

    a.bind(done);
    flush_var(dst);
}

void BeamModuleAssembler::emit_cmp_immed_to_bool(arm::CondCode cc,
                                                 const ArgSource &LHS,
                                                 const ArgSource &RHS,
                                                 const ArgRegister &Dst) {
    if (RHS.isImmed()) {
        auto lhs = load_source(LHS, ARG1);
        cmp_arg(lhs.reg, RHS);
    } else {
        auto [lhs, rhs] = load_sources(LHS, ARG1, RHS, ARG2);
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
            a.and_(TMP, lhs.reg, rhs.reg);
            emit_is_not_boxed(generic, TMP);
        }
    } else {
        /* Relative comparisons are overwhelmingly likely to be used
         * on smalls, so we'll specialize those and keep the rest in a
         * shared fragment. */
        if (always_small(RHS)) {
            a.and_(TMP, lhs.reg, imm(_TAG_IMMED1_MASK));
        } else if (always_small(LHS)) {
            a.and_(TMP, rhs.reg, imm(_TAG_IMMED1_MASK));
        } else {
            ERTS_CT_ASSERT(_TAG_IMMED1_SMALL == _TAG_IMMED1_MASK);
            a.and_(TMP, lhs.reg, rhs.reg);
            a.and_(TMP, TMP, imm(_TAG_IMMED1_MASK));
        }

        a.cmp(TMP, imm(_TAG_IMMED1_SMALL));
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
        cmp_arg(lhs.reg, RHS);
        emit_cond_to_bool(arm::CondCode::kGE, Dst);

        return;
    } else if (LHS.isSmall() && LHS.isImmed() && always_small(RHS)) {
        auto rhs = load_source(RHS, ARG1);

        comment("simplified compare because one operand is an immediate small");
        cmp_arg(rhs.reg, LHS);
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
    Label valid = a.newLabel();
    Label invalid = a.newLabel();

    auto [src1, src2] = load_sources(Src1, ARG1, Src2, ARG2);
    auto dst = init_destination(Dst, TMP);

    ERTS_CT_ASSERT(am_false == make_atom(0));
    ERTS_CT_ASSERT(am_true == make_atom(1));

    if (exact_type<BeamTypeId::Atom>(Src1) &&
        exact_type<BeamTypeId::Atom>(Src2)) {
        comment("simplified type check because operands are atoms");
        a.orr(TMP, src1.reg, src2.reg);
        mov_imm(VAR, (-1u << (_TAG_IMMED2_SIZE + 1)));
        a.tst(TMP, VAR);
        a.b_eq(valid);
    } else {
        const Uint mask = (_TAG_IMMED2_MASK | ~diff_bit);
        mov_imm(VAR, mask);
        a.and_(TMP, src1.reg, VAR);
        a.and_(VAR, src2.reg, VAR);
        a.cmp(TMP, imm(_TAG_IMMED2_ATOM));
        a.b_ne(invalid);
        a.cmp(TMP, VAR);
        a.b_eq(valid);
    }

    a.bind(invalid);
    if (Fail.get()) {
        a.b(resolve_beam_label(Fail, dispUnknown));
    } else {
        mov_var(ARG1, src1);
        mov_var(ARG2, src2);
        fragment_call(ga->get_handle_and_error());
    }

    a.bind(valid);
    a.and_(dst.reg, src1.reg, src2.reg);
    flush_var(dst);
}

/* ================================================================
 *  bit_size/1
 * ================================================================
 */
void BeamGlobalAssembler::emit_bif_bit_size_helper(Label error) {
    emit_is_boxed(error, ARG1);
    emit_untag_ptr(TMP, ARG1);

    ERTS_CT_ASSERT(offsetof(ErlHeapBits, thing_word) == 0);
    ERTS_CT_ASSERT(offsetof(ErlHeapBits, size) == sizeof(Eterm));
    a.ldr(ARG3, arm::Mem(TMP, offsetof(ErlHeapBits, thing_word)));
    a.ldr(ARG2, arm::Mem(TMP, offsetof(ErlHeapBits, size)));

    Label not_sub_bits = a.newLabel();
    mov_imm(VAR, HEADER_SUB_BITS);
    a.cmp(ARG3, VAR);
    a.b_ne(not_sub_bits);
    {
        ERTS_CT_ASSERT_FIELD_PAIR(ErlSubBits, start, end);
        a.ldr(ARG2, arm::Mem(TMP, offsetof(ErlSubBits, start)));
        a.ldr(VAR, arm::Mem(TMP, offsetof(ErlSubBits, end)));
        a.sub(ARG2, VAR, ARG2);
    }
    a.bind(not_sub_bits);

    const auto mask = _BITSTRING_TAG_MASK & ~_TAG_PRIMARY_MASK;
    ERTS_CT_ASSERT(TAG_PRIMARY_HEADER == 0);
    ERTS_CT_ASSERT(_TAG_HEADER_HEAP_BITS == (_TAG_HEADER_HEAP_BITS & mask));
    mov_imm(VAR, mask);
    a.and_(ARG3, ARG3, VAR);
    mov_imm(VAR, _TAG_HEADER_HEAP_BITS);
    a.cmp(ARG3, VAR);
    a.b_ne(error);
}

void BeamGlobalAssembler::emit_bif_bit_size_body() {
    Label error = a.newLabel();

    emit_bif_bit_size_helper(error);

    a.lsl(ARG2, ARG2, imm(_TAG_IMMED1_SIZE));
    a.orr(ARG1, ARG2, imm(_TAG_IMMED1_SMALL));
    a.bx(a32::lr);

    a.bind(error);
    {
        static ErtsCodeMFA mfa = {am_erlang, am_bit_size, 1};
        a.str(ARG1, getXRef(0));
        emit_raise_badarg(&mfa);
    }
}

void BeamModuleAssembler::emit_bif_bit_size(const ArgLabel &Fail,
                                            const ArgSource &Src,
                                            const ArgRegister &Dst) {
    auto src = load_source(Src, ARG1);
    auto dst = init_destination(Dst, ARG1);

    if ((Fail.get() != 0) || exact_type<BeamTypeId::Bitstring>(Src)) {
        if (Fail.get() != 0) {
            emit_is_boxed(resolve_beam_label(Fail, disp32MB), Src, src.reg);
        }

        emit_untag_ptr(TMP, src.reg);

        ERTS_CT_ASSERT(offsetof(ErlHeapBits, thing_word) == 0);
        ERTS_CT_ASSERT(offsetof(ErlHeapBits, size) == sizeof(Eterm));
        a.ldr(ARG3, arm::Mem(TMP, offsetof(ErlHeapBits, thing_word)));
        a.ldr(ARG2, arm::Mem(TMP, offsetof(ErlHeapBits, size)));

        Label not_sub_bits = a.newLabel();
        mov_imm(VAR, HEADER_SUB_BITS);
        a.cmp(ARG3, VAR);
        a.b_ne(not_sub_bits);
        {
            a.ldr(ARG2, arm::Mem(TMP, offsetof(ErlSubBits, start)));
            a.ldr(VAR, arm::Mem(TMP, offsetof(ErlSubBits, end)));
            a.sub(ARG2, VAR, ARG2);
        }
        a.bind(not_sub_bits);

        if (masked_types<BeamTypeId::MaybeBoxed>(Src) ==
            BeamTypeId::Bitstring) {
            comment("skipped header test since we know it's a bitstring when "
                    "boxed");
        } else {
            const auto mask = _BITSTRING_TAG_MASK & ~_TAG_PRIMARY_MASK;
            mov_imm(VAR, mask);
            a.and_(ARG3, ARG3, VAR);
            mov_imm(VAR, _TAG_HEADER_HEAP_BITS);
            a.cmp(ARG3, VAR);
            a.b_ne(resolve_beam_label(Fail, disp32MB));
        }

        a.lsl(dst.reg, ARG2, imm(_TAG_IMMED1_SIZE));
        a.orr(dst.reg, dst.reg, imm(_TAG_IMMED1_SMALL));
    } else {
        mov_var(ARG1, src);
        fragment_call(ga->get_bif_bit_size_body());
        mov_var(dst, ARG1);
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
    add(ARG2, ARG2, 7);
    a.lsl(ARG2, ARG2, imm(_TAG_IMMED1_SIZE - 3));
    a.orr(ARG1, ARG2, imm(_TAG_IMMED1_SMALL));
    a.bx(a32::lr);

    a.bind(error);
    {
        static ErtsCodeMFA mfa = {am_erlang, am_byte_size, 1};
        a.str(ARG1, getXRef(0));
        emit_raise_badarg(&mfa);
    }
}

void BeamModuleAssembler::emit_bif_byte_size(const ArgLabel &Fail,
                                             const ArgSource &Src,
                                             const ArgRegister &Dst) {
    auto src = load_source(Src, ARG1);
    auto dst = init_destination(Dst, ARG1);

    if ((Fail.get() != 0) || exact_type<BeamTypeId::Bitstring>(Src)) {
        if (Fail.get() != 0) {
            emit_is_boxed(resolve_beam_label(Fail, disp32MB), Src, src.reg);
        }

        emit_untag_ptr(TMP, src.reg);

        ERTS_CT_ASSERT(offsetof(ErlHeapBits, thing_word) == 0);
        ERTS_CT_ASSERT(offsetof(ErlHeapBits, size) == sizeof(Eterm));
        a.ldr(ARG3, arm::Mem(TMP, offsetof(ErlHeapBits, thing_word)));
        a.ldr(ARG2, arm::Mem(TMP, offsetof(ErlHeapBits, size)));

        Label not_sub_bits = a.newLabel();
        mov_imm(VAR, HEADER_SUB_BITS);
        a.cmp(ARG3, VAR);
        a.b_ne(not_sub_bits);
        {
            a.ldr(ARG2, arm::Mem(TMP, offsetof(ErlSubBits, start)));
            a.ldr(VAR, arm::Mem(TMP, offsetof(ErlSubBits, end)));
            a.sub(ARG2, VAR, ARG2);
        }
        a.bind(not_sub_bits);

        if (masked_types<BeamTypeId::MaybeBoxed>(Src) ==
            BeamTypeId::Bitstring) {
            comment("skipped header test since we know it's a bitstring when "
                    "boxed");
        } else {
            const auto mask = _BITSTRING_TAG_MASK & ~_TAG_PRIMARY_MASK;
            mov_imm(VAR, mask);
            a.and_(ARG3, ARG3, VAR);
            mov_imm(VAR, _TAG_HEADER_HEAP_BITS);
            a.cmp(ARG3, VAR);
            a.b_ne(resolve_beam_label(Fail, disp32MB));
        }

        /* Round up to the next byte. */
        add(ARG2, ARG2, 7);
        a.lsl(dst.reg, ARG2, imm(_TAG_IMMED1_SIZE - 3));
        a.orr(dst.reg, dst.reg, imm(_TAG_IMMED1_SMALL));
    } else {
        mov_var(ARG1, src);
        fragment_call(ga->get_bif_byte_size_body());
        mov_var(dst, ARG1);
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
    a32::Gp boxed_ptr = emit_ptr_val(TMP, ARG2);
    lea(TMP, emit_boxed_val(boxed_ptr));
    a.ldr(ARG3, arm::Mem(TMP));
    ERTS_CT_ASSERT(make_arityval_zero() == 0);
    a.tst(ARG3, imm(_TAG_HEADER_MASK));
    a.b_ne(fail);

    a.and_(ARG4, ARG1, imm(_TAG_IMMED1_MASK));
    a.cmp(ARG4, imm(_TAG_IMMED1_SMALL));
    a.b_ne(fail);
    a.cmp(ARG1, imm(make_small(0)));
    a.b_eq(fail);

    /* Ensure that the position points within the tuple. */
    a.lsr(ARG4, ARG3, _HEADER_ARITY_OFFS);
    a.asr(ARG3, ARG1, imm(_TAG_IMMED1_SIZE));
    a.cmp(ARG3, ARG4);
    a.b_hi(fail);

    a.ldr(ARG1, arm::Mem(TMP, ARG3, arm::lsl(2)));
    a.bx(a32::lr);
}

void BeamGlobalAssembler::emit_bif_element_body_shared() {
    Label error = a.newLabel();

    emit_bif_element_helper(error);

    a.bind(error);
    {
        static ErtsCodeMFA mfa = {am_erlang, am_element, 2};
        a.str(ARG1, getXRef(0));
        a.str(ARG2, getXRef(1));
        emit_raise_badarg(&mfa);
    }
}

void BeamGlobalAssembler::emit_bif_element_guard_shared() {
    Label error = a.newLabel();

    emit_bif_element_helper(error);

    a.bind(error);
    {
        mov_imm(ARG1, THE_NON_VALUE);
        a.bx(a32::lr);
    }
}

void BeamGlobalAssembler::emit_handle_element_error_shared() {
    static ErtsCodeMFA mfa = {am_erlang, am_element, 2};
    a.str(ARG1, getXRef(0));
    a.str(ARG2, getXRef(1));
    emit_raise_badarg(&mfa);
}

void BeamModuleAssembler::emit_bif_element(const ArgLabel &Fail,
                                           const ArgSource &Pos,
                                           const ArgSource &Tuple,
                                           const ArgRegister &Dst) {
    // TODO: check arm64 implementation for fast paths to optimize this emitter
    mov_arg(ARG1, Pos);
    mov_arg(ARG2, Tuple);

    if (Fail.get() != 0) {
        fragment_call(ga->get_bif_element_guard_shared());
        emit_branch_if_not_value(ARG1, resolve_beam_label(Fail, dispUnknown));
    } else {
        fragment_call(ga->get_bif_element_body_shared());
    }

    auto dst = init_destination(Dst, ARG1);
    mov_var(dst, ARG1);
    flush_var(dst);
    reg_cache.invalidate();
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
    auto src = load_source(Src, TMP);
    auto hd = init_destination(Hd, ARG1);

    /* A list has primary tag TAG_PRIMARY_LIST. */
    emit_is_not_cons(good_cons, src.reg);
    a.str(src.reg, getXRef(0));
    fragment_call(ga->get_handle_hd_error());

    a.bind(good_cons);
    {
        a32::Gp cons_ptr = emit_ptr_val(TMP, src.reg);
        a.ldr(hd.reg, getCARRef(cons_ptr));
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

        mov_imm(TMP, THE_NON_VALUE);
        a.cmp(ARG1, TMP);
        emit_cond_to_bool(arm::CondCode::kNE, Dst);
    }
}

/* ================================================================
 *  map_get/2
 * ================================================================
 */

void BeamGlobalAssembler::emit_handle_map_get_badmap() {
    static ErtsCodeMFA mfa = {am_erlang, am_map_get, 2};
    mov_imm(TMP, BADMAP);
    ERTS_CT_ASSERT_FIELD_PAIR(Process, freason, fvalue);
    a.str(TMP, arm::Mem(c_p, offsetof(Process, freason)));
    a.str(ARG1, arm::Mem(c_p, offsetof(Process, fvalue)));
    a.str(ARG2, getXRef(0));
    a.str(ARG1, getXRef(1));
    mov_imm(ARG4, &mfa);
    a.b(labels[raise_exception]);
}

void BeamGlobalAssembler::emit_handle_map_get_badkey() {
    static ErtsCodeMFA mfa = {am_erlang, am_map_get, 2};
    mov_imm(TMP, BADKEY);
    ERTS_CT_ASSERT_FIELD_PAIR(Process, freason, fvalue);
    a.str(TMP, arm::Mem(c_p, offsetof(Process, freason)));
    a.str(ARG2, arm::Mem(c_p, offsetof(Process, fvalue)));
    a.str(ARG2, getXRef(0));
    a.str(ARG1, getXRef(1));
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
            a32::Gp boxed_ptr = emit_ptr_val(VAR, ARG1);
            a.ldr(TMP, emit_boxed_val(boxed_ptr));
            a.and_(TMP, TMP, imm(_TAG_HEADER_MASK));
            a.cmp(TMP, imm(_TAG_HEADER_MAP));
            if (Fail.get() == 0) {
                a.b_eq(good_map);
            } else {
                a.b_ne(resolve_beam_label(Fail, dispUnknown));
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
            a.b_ne(resolve_beam_label(Fail, dispUnknown));
        }
    } else {
        emit_enter_runtime();
        runtime_call<2>(get_map_element);
        emit_leave_runtime();

        if (Fail.get() == 0) {
            emit_branch_if_value(ARG1, good_key);
        } else {
            emit_branch_if_not_value(ARG1, resolve_beam_label(Fail, dispUnknown));
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
    // TODO
    emit_nyi("emit_handle_map_size_error");
}

void BeamModuleAssembler::emit_bif_map_size(const ArgLabel &Fail,
                                            const ArgSource &Src,
                                            const ArgRegister &Dst) {
    // TODO
    emit_nyi("emit_bif_map_size");
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

    Label generic = a.newLabel(), next = a.newLabel();
    Label select_rhs = a.newLabel(), done = a.newLabel();
    bool both_small = always_small(LHS) && always_small(RHS);
    bool need_generic = !both_small;
    auto dst = init_destination(Dst, ARG1);

    if (both_small) {
        comment("skipped test for small operands");
    } else if (always_one_of<BeamTypeId::Integer, BeamTypeId::AlwaysBoxed>(
                       LHS) &&
               always_small(RHS)) {
        emit_is_not_boxed(generic, lhs.reg);
    } else if (always_small(LHS) &&
               always_one_of<BeamTypeId::Integer, BeamTypeId::AlwaysBoxed>(
                       RHS)) {
        emit_is_not_boxed(generic, rhs.reg);
    } else if (always_one_of<BeamTypeId::Integer, BeamTypeId::AlwaysBoxed>(
                       LHS) &&
               always_one_of<BeamTypeId::Integer, BeamTypeId::AlwaysBoxed>(
                       RHS)) {
        comment("simplified test for small operands");
        ERTS_CT_ASSERT(_TAG_IMMED1_SMALL == _TAG_IMMED1_MASK);
        a.and_(TMP, lhs.reg, rhs.reg);
        emit_is_not_boxed(generic, TMP);
    } else {
        if (RHS.isSmall()) {
            a.and_(TMP, lhs.reg, imm(_TAG_IMMED1_MASK));
        } else if (LHS.isSmall()) {
            a.and_(TMP, rhs.reg, imm(_TAG_IMMED1_MASK));
        } else {
            /* Avoid the expensive generic comparison for equal terms. */
            a.cmp(lhs.reg, rhs.reg);
            a.b_eq(next);

            ERTS_CT_ASSERT(_TAG_IMMED1_SMALL == _TAG_IMMED1_MASK);
            a.and_(TMP, lhs.reg, rhs.reg);
            a.and_(TMP, TMP, imm(_TAG_IMMED1_MASK));
        }

        a.cmp(TMP, imm(_TAG_IMMED1_SMALL));
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
    switch (cc) {
    case arm::CondCode::kLT:
        a.b_lt(select_rhs);
        break;
    case arm::CondCode::kGT:
        a.b_gt(select_rhs);
        break;
    default:
        ASSERT(!"Unsupported condition code in emit_bif_min_max");
        break;
    }

    mov_var(dst, lhs);
    a.b(done);

    a.bind(select_rhs);
    mov_var(dst, rhs);

    a.bind(done);
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

    a32::Gp boxed_ptr = emit_ptr_val(TMP, src.reg);

    if (!always_one_of<BeamTypeId::Pid, BeamTypeId::Port>(Src)) {
        a.ldr(VAR, emit_boxed_val(boxed_ptr));
        a.and_(VAR, VAR, imm(_TAG_HEADER_MASK));
    }

    if (maybe_one_of<BeamTypeId::Reference>(Src)) {
        a.cmp(VAR, imm(_TAG_HEADER_REF));
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
        a.sub(TMP, VAR, imm(_TAG_HEADER_EXTERNAL_PID));
        a.cmp(TMP, imm(_TAG_HEADER_EXTERNAL_REF - _TAG_HEADER_EXTERNAL_PID));

        if (Fail.get() != 0) {
            a.b_hi(fail);
        } else {
            a.b_ls(external);

            a.bind(fail);
            {
                mov_var(ARG1, src);
                mov_arg(ArgXRegister(0), ARG1);
                fragment_call(ga->get_handle_node_error());
            }
        }

        a.bind(external);
    }

    a.ldr(TMP, emit_boxed_val(boxed_ptr, offsetof(ExternalThing, node)));
    a.b(next);

    a.bind(test_internal);
    if (!always_identifier) {
        /* Internal identifiers are either pid or port immediates. */
        Label ok = a.newLabel();
        a.and_(TMP, src.reg, imm(_TAG_IMMED1_MASK));
        a.cmp(TMP, imm(_TAG_IMMED1_PID));
        a.b_eq(ok);
        a.cmp(TMP, imm(_TAG_IMMED1_PORT));
        a.b_ne(fail);
        a.bind(ok);
    }

    a.bind(internal);
    mov_imm(TMP, &erts_this_node);
    a.ldr(TMP, arm::Mem(TMP));

    a.bind(next);
    mov_arg(Dst, arm::Mem(TMP, offsetof(ErlNode, sysname)));
}

/* ================================================================
 *  not/1
 * ================================================================
 */

void BeamGlobalAssembler::emit_handle_not_error() {
    // TODO
    emit_nyi("emit_handle_not_error");
}

void BeamModuleAssembler::emit_bif_not(const ArgLabel &Fail,
                                       const ArgRegister &Src,
                                       const ArgRegister &Dst) {
    // TODO
    emit_nyi("emit_bif_not");
}

/* ================================================================
 *  or/2
 * ================================================================
 */

void BeamGlobalAssembler::emit_handle_or_error() {
    // TODO
    emit_nyi("emit_handle_or_error");
}

void BeamModuleAssembler::emit_bif_or(const ArgLabel &Fail,
                                      const ArgSource &Src1,
                                      const ArgSource &Src2,
                                      const ArgRegister &Dst) {
    // TODO
    emit_nyi("emit_bif_or");
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
    auto src = load_source(Src, TMP);
    auto tl = init_destination(Tl, ARG1);

    /* A list has primary tag TAG_PRIMARY_LIST. */
    emit_is_not_cons(good_cons, src.reg);
    a.str(src.reg, getXRef(0));
    fragment_call(ga->get_handle_tl_error());

    a.bind(good_cons);
    {
        a32::Gp cons_ptr = emit_ptr_val(TMP, src.reg);
        a.ldr(tl.reg, getCDRRef(cons_ptr));
        flush_var(tl);
    }
}

/* ================================================================
 *  tuple_size/1
 * ================================================================
 */

void BeamGlobalAssembler::emit_bif_tuple_size_helper(Label fail) {
    a32::Gp boxed_ptr = emit_ptr_val(TMP, ARG1);

    emit_is_boxed(fail, boxed_ptr);

    ERTS_CT_ASSERT(_TAG_HEADER_ARITYVAL == 0);
    a.ldr(TMP, emit_boxed_val(boxed_ptr));
    a.tst(TMP, imm(_TAG_HEADER_MASK));
    a.b_ne(fail);

    ERTS_CT_ASSERT(_HEADER_ARITY_OFFS - _TAG_IMMED1_SIZE > 0);
    ERTS_CT_ASSERT(_TAG_IMMED1_SMALL == _TAG_IMMED1_MASK);
    a.lsr(TMP, TMP, _HEADER_ARITY_OFFS - _TAG_IMMED1_SIZE);
    a.orr(ARG1, TMP, imm(_TAG_IMMED1_SMALL));

    a.bx(a32::lr);
}

void BeamGlobalAssembler::emit_bif_tuple_size_body() {
    Label error = a.newLabel();

    emit_bif_tuple_size_helper(error);

    a.bind(error);
    {
        static ErtsCodeMFA mfa = {am_erlang, am_tuple_size, 1};
        a.str(ARG1, getXRef(0));
        emit_raise_badarg(&mfa);
    }
}

void BeamGlobalAssembler::emit_bif_tuple_size_guard() {
    Label error = a.newLabel();

    emit_bif_tuple_size_helper(error);

    a.bind(error);
    {
        mov_imm(ARG1, THE_NON_VALUE);
        a.bx(a32::lr);
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
        a32::Gp boxed_ptr = emit_ptr_val(TMP, src.reg);
        a.ldr(TMP, emit_boxed_val(boxed_ptr));
        ERTS_CT_ASSERT(_HEADER_ARITY_OFFS - _TAG_IMMED1_SIZE > 0);
        ERTS_CT_ASSERT(_TAG_IMMED1_SMALL == _TAG_IMMED1_MASK);
        a.lsr(TMP, TMP, _HEADER_ARITY_OFFS - _TAG_IMMED1_SIZE);
        a.orr(dst.reg, TMP, imm(_TAG_IMMED1_SMALL));
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
