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
 * ================================================================
 */

void BeamGlobalAssembler::emit_bif_is_eq_exact_shared() {
    Label succ = a.newLabel(), fail = a.newLabel();

    a.cmp(ARG1, ARG2);
    a.cond_eq().b(succ);

    /* The terms could still be equal if both operands are pointers
     * having the same tag. */
    emit_is_unequal_based_on_tags(ARG1, ARG2);
    a.cond_eq().b(fail);

    emit_enter_runtime_frame();
    emit_enter_runtime();

    runtime_call<2>(eq);

    emit_leave_runtime();
    emit_leave_runtime_frame();

    a.cbz(ARG1, fail);

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
    a.cond_eq().b(fail);

    emit_is_unequal_based_on_tags(ARG1, ARG2);
    a.cond_eq().b(succ);

    emit_enter_runtime_frame();
    emit_enter_runtime();

    runtime_call<2>(eq);

    emit_leave_runtime();
    emit_leave_runtime_frame();

    a.cbnz(ARG1, fail);

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

void BeamModuleAssembler::emit_bif_is_eq_ne_exact_immed(const ArgVal &Src,
                                                        const ArgVal &Immed,
                                                        const ArgVal &Dst,
                                                        Eterm fail_value,
                                                        Eterm succ_value) {
    auto src = load_source(Src, TMP1);
    auto dst = init_destination(Dst, TMP2);

    cmp_arg(src.reg, Immed);
    mov_imm(TMP3, succ_value);
    mov_imm(TMP4, fail_value);
    a.csel(dst.reg, TMP3, TMP4, arm::Cond::kEQ);
    flush_var(dst);
}

void BeamModuleAssembler::emit_bif_is_eq_exact(const ArgVal &LHS,
                                               const ArgVal &RHS,
                                               const ArgVal &Dst) {
    if (RHS.isImmed()) {
        emit_bif_is_eq_ne_exact_immed(LHS, RHS, Dst, am_false, am_true);
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

void BeamModuleAssembler::emit_bif_is_ne_exact(const ArgVal &LHS,
                                               const ArgVal &RHS,
                                               const ArgVal &Dst) {
    if (RHS.isImmed()) {
        emit_bif_is_eq_ne_exact_immed(LHS, RHS, Dst, am_true, am_false);
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

/* ================================================================
 *  and/2
 * ================================================================
 */

void BeamGlobalAssembler::emit_handle_and_error() {
    static ErtsCodeMFA mfa = {am_erlang, am_and, 2};
    emit_raise_badarg(&mfa);
}

void BeamModuleAssembler::emit_bif_and(const ArgVal &Fail,
                                       const ArgVal &Src1,
                                       const ArgVal &Src2,
                                       const ArgVal &Dst) {
    static const Uint diff_bit = am_true - am_false;
    Label next = a.newLabel();

    auto [src1, src2] = load_sources(Src1, TMP1, Src2, TMP2);
    auto dst = init_destination(Dst, TMP3);

    ERTS_CT_ASSERT(am_false == make_atom(0));
    ERTS_CT_ASSERT(am_true == make_atom(1));

    a.and_(TMP3, src1.reg, imm(_TAG_IMMED2_MASK | ~diff_bit));
    a.and_(TMP4, src2.reg, imm(_TAG_IMMED2_MASK | ~diff_bit));
    a.cmp(TMP3, imm(_TAG_IMMED2_ATOM));
    a.ccmp(TMP3, TMP4, 0, arm::Cond::kEQ);

    if (Fail.getValue()) {
        a.cond_ne().b(resolve_beam_label(Fail, disp1MB));
    } else {
        a.cond_eq().b(next);
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

void BeamGlobalAssembler::emit_bif_bit_size_helper(Label fail) {
    Label not_sub_bin = a.newLabel();
    arm::Gp boxed_ptr = emit_ptr_val(ARG1, ARG1);

    emit_is_boxed(fail, boxed_ptr);

    a.ldur(TMP1, emit_boxed_val(boxed_ptr));
    a.and_(TMP1, TMP1, imm(_TAG_HEADER_MASK));
    a.cmp(TMP1, imm(_TAG_HEADER_SUB_BIN));
    a.cond_ne().b(not_sub_bin);

    a.ldur(TMP1, emit_boxed_val(boxed_ptr, sizeof(Eterm)));
    a.ldurb(TMP2.w(), emit_boxed_val(boxed_ptr, offsetof(ErlSubBin, bitsize)));

    mov_imm(ARG1, _TAG_IMMED1_SMALL);
    a.add(TMP1, TMP2, TMP1, arm::lsl(3));
    a.bfi(ARG1, TMP1, imm(_TAG_IMMED1_SIZE), imm(SMALL_BITS));
    a.ret(a64::x30);

    a.bind(not_sub_bin);
    ERTS_CT_ASSERT(_TAG_HEADER_REFC_BIN + 4 == _TAG_HEADER_HEAP_BIN);
    a.and_(TMP1, TMP1, imm(~4));
    a.cmp(TMP1, imm(_TAG_HEADER_REFC_BIN));
    a.cond_ne().b(fail);

    a.ldur(TMP1, emit_boxed_val(boxed_ptr, sizeof(Eterm)));
    mov_imm(ARG1, _TAG_IMMED1_SMALL);
    a.bfi(ARG1, TMP1, imm(_TAG_IMMED1_SIZE + 3), imm(SMALL_BITS - 3));

    a.ret(a64::x30);
}

void BeamGlobalAssembler::emit_bif_bit_size_body() {
    Label error = a.newLabel();

    emit_bif_bit_size_helper(error);

    a.bind(error);
    {
        static ErtsCodeMFA mfa = {am_erlang, am_bit_size, 1};
        a.mov(XREG0, ARG1);
        emit_raise_badarg(&mfa);
    }
}

void BeamGlobalAssembler::emit_bif_bit_size_guard() {
    Label error = a.newLabel();

    emit_bif_bit_size_helper(error);

    a.bind(error);
    {
        mov_imm(ARG1, THE_NON_VALUE);
        a.ret(a64::x30);
    }
}

void BeamModuleAssembler::emit_bif_bit_size(const ArgVal &Fail,
                                            const ArgVal &Src,
                                            const ArgVal &Dst) {
    auto src = load_source(Src, ARG1);
    auto dst = init_destination(Dst, ARG1);

    mov_var(ARG1, src);

    if (Fail.getValue() == 0) {
        fragment_call(ga->get_bif_bit_size_body());
    } else {
        fragment_call(ga->get_bif_bit_size_guard());
        emit_branch_if_not_value(ARG1, resolve_beam_label(Fail, dispUnknown));
    }

    mov_var(dst, ARG1);
    flush_var(dst);
}

/* ================================================================
 *  byte_size/1
 * ================================================================
 */

void BeamGlobalAssembler::emit_bif_byte_size_helper(Label fail) {
    Label not_sub_bin = a.newLabel();
    arm::Gp boxed_ptr = emit_ptr_val(ARG1, ARG1);

    emit_is_boxed(fail, boxed_ptr);

    a.ldur(TMP1, emit_boxed_val(boxed_ptr));
    a.and_(TMP1, TMP1, imm(_TAG_HEADER_MASK));
    a.cmp(TMP1, imm(_TAG_HEADER_SUB_BIN));
    a.cond_ne().b(not_sub_bin);

    a.ldurb(TMP2.w(), emit_boxed_val(boxed_ptr, offsetof(ErlSubBin, bitsize)));
    a.ldur(TMP1, emit_boxed_val(boxed_ptr, sizeof(Eterm)));
    a.cmp(TMP2, imm(0));
    a.cinc(TMP1, TMP1, arm::Cond::kNE);

    mov_imm(ARG1, _TAG_IMMED1_SMALL);
    a.bfi(ARG1, TMP1, imm(_TAG_IMMED1_SIZE), imm(SMALL_BITS));
    a.ret(a64::x30);

    a.bind(not_sub_bin);
    ERTS_CT_ASSERT(_TAG_HEADER_REFC_BIN + 4 == _TAG_HEADER_HEAP_BIN);
    a.and_(TMP1, TMP1, imm(~4));
    a.cmp(TMP1, imm(_TAG_HEADER_REFC_BIN));
    a.cond_ne().b(fail);

    a.ldur(TMP1, emit_boxed_val(boxed_ptr, sizeof(Eterm)));
    mov_imm(ARG1, _TAG_IMMED1_SMALL);
    a.bfi(ARG1, TMP1, imm(_TAG_IMMED1_SIZE), imm(SMALL_BITS));

    a.ret(a64::x30);
}

void BeamGlobalAssembler::emit_bif_byte_size_body() {
    Label error = a.newLabel();

    emit_bif_byte_size_helper(error);

    a.bind(error);
    {
        static ErtsCodeMFA mfa = {am_erlang, am_byte_size, 1};
        a.mov(XREG0, ARG1);
        emit_raise_badarg(&mfa);
    }
}

void BeamGlobalAssembler::emit_bif_byte_size_guard() {
    Label error = a.newLabel();

    emit_bif_byte_size_helper(error);

    a.bind(error);
    {
        mov_imm(ARG1, THE_NON_VALUE);
        a.ret(a64::x30);
    }
}

void BeamModuleAssembler::emit_bif_byte_size(const ArgVal &Fail,
                                             const ArgVal &Src,
                                             const ArgVal &Dst) {
    auto src = load_source(Src, ARG1);
    auto dst = init_destination(Dst, ARG1);

    mov_var(ARG1, src);

    if (Fail.getValue() == 0) {
        fragment_call(ga->get_bif_byte_size_body());
    } else {
        fragment_call(ga->get_bif_byte_size_guard());
        emit_branch_if_not_value(ARG1, resolve_beam_label(Fail, dispUnknown));
    }

    mov_var(dst, ARG1);
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
    a.and_(TMP1, ARG1, imm(_TAG_IMMED1_MASK));
    a.cmp(TMP1, imm(_TAG_IMMED1_SMALL));
    a.cond_ne().b(fail);

    /* Ensure that ARG2 contains a tuple. */
    emit_is_boxed(fail, ARG2);
    arm::Gp boxed_ptr = emit_ptr_val(TMP1, ARG2);
    lea(TMP1, emit_boxed_val(boxed_ptr));
    a.ldr(TMP2, arm::Mem(TMP1));
    ERTS_CT_ASSERT(make_arityval(0) == 0);
    a.tst(TMP2, imm(_TAG_HEADER_MASK));
    a.cond_ne().b(fail);

    /* Ensure that the position points within the tuple. */
    a.lsr(TMP2, TMP2, imm(_HEADER_ARITY_OFFS));
    a.asr(TMP3, ARG1, imm(_TAG_IMMED1_SIZE));
    a.cmp(TMP3, imm(1));
    a.cond_mi().b(fail);
    a.cmp(TMP2, TMP3);
    a.cond_lo().b(fail);

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

void BeamModuleAssembler::emit_bif_element(const ArgVal &Fail,
                                           const ArgVal &Pos,
                                           const ArgVal &Tuple,
                                           const ArgVal &Dst) {
    mov_arg(ARG1, Pos);
    mov_arg(ARG2, Tuple);

    if (Fail.getValue() != 0) {
        fragment_call(ga->get_bif_element_guard_shared());
        emit_branch_if_not_value(ARG1, resolve_beam_label(Fail, dispUnknown));
    } else {
        fragment_call(ga->get_bif_element_body_shared());
    }

    auto dst = init_destination(Dst, ARG1);
    mov_var(dst, ARG1);
    flush_var(dst);
}

/* ================================================================
 *  hd/1
 * ================================================================
 */

void BeamGlobalAssembler::emit_handle_hd_error() {
    static ErtsCodeMFA mfa = {am_erlang, am_hd, 1};
    emit_raise_badarg(&mfa);
}

void BeamModuleAssembler::emit_bif_hd(const ArgVal &Src, const ArgVal &Hd) {
    Label good_cons = a.newLabel();
    auto src = load_source(Src, TMP1);
    auto hd = init_destination(Hd, TMP2);
    const int bitNumber = 1;

    ERTS_CT_ASSERT(_TAG_PRIMARY_MASK - TAG_PRIMARY_LIST == (1 << bitNumber));

    a.tbz(src.reg, bitNumber, good_cons);
    mov_var(XREG0, src);
    fragment_call(ga->get_handle_hd_error());

    a.bind(good_cons);
    {
        arm::Gp cons_ptr = emit_ptr_val(TMP1, src.reg);
        a.ldur(hd.reg, getCARRef(cons_ptr));
        flush_var(hd);
    }
}

/* ================================================================
 *  map_size/1
 * ================================================================
 */

void BeamGlobalAssembler::emit_handle_map_size_error() {
    static ErtsCodeMFA mfa = {am_erlang, am_map_size, 1};
    mov_imm(TMP1, BADMAP);
    a.str(TMP1, arm::Mem(c_p, offsetof(Process, freason)));
    a.str(XREG0, arm::Mem(c_p, offsetof(Process, fvalue)));
    mov_imm(ARG4, &mfa);
    a.b(labels[raise_exception]);
}

void BeamModuleAssembler::emit_bif_map_size(const ArgVal &Fail,
                                            const ArgVal &Src,
                                            const ArgVal &Dst) {
    Label error = a.newLabel(), good_map = a.newLabel();
    auto src = load_source(Src, TMP1);
    auto dst = init_destination(Dst, TMP2);

    if (Fail.getValue() == 0) {
        emit_is_boxed(error, src.reg);
    } else {
        emit_is_boxed(resolve_beam_label(Fail, dispUnknown), src.reg);
    }

    arm::Gp boxed_ptr = emit_ptr_val(TMP3, src.reg);
    a.ldur(TMP4, emit_boxed_val(boxed_ptr));
    a.and_(TMP4, TMP4, imm(_TAG_HEADER_MASK));
    a.cmp(TMP4, imm(_TAG_HEADER_MAP));

    if (Fail.getValue() == 0) {
        a.cond_eq().b(good_map);
        a.bind(error);
        {
            mov_var(XREG0, src);
            fragment_call(ga->get_handle_map_size_error());
        }
    } else {
        a.cond_ne().b(resolve_beam_label(Fail, disp1MB));
        a.bind(error); /* Never referenced. */
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
 *  not/1
 * ================================================================
 */

void BeamGlobalAssembler::emit_handle_not_error() {
    static ErtsCodeMFA mfa = {am_erlang, am_not, 1};
    emit_raise_badarg(&mfa);
}

void BeamModuleAssembler::emit_bif_not(const ArgVal &Fail,
                                       const ArgVal &Src,
                                       const ArgVal &Dst) {
    Label next = a.newLabel();
    auto src = load_source(Src, TMP1);
    auto dst = init_destination(Dst, TMP2);
    ERTS_CT_ASSERT(am_false == make_atom(0));
    ERTS_CT_ASSERT(am_true == make_atom(1));
    Uint diff_bit = am_true - am_false;

    a.and_(TMP3, src.reg, imm(_TAG_IMMED2_MASK | ~diff_bit));
    a.cmp(TMP3, imm(_TAG_IMMED2_ATOM));

    if (Fail.getValue() == 0) {
        a.cond_eq().b(next);
        mov_var(XREG0, src);
        fragment_call(ga->get_handle_not_error());
    } else {
        a.cond_ne().b(resolve_beam_label(Fail, disp1MB));
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

void BeamModuleAssembler::emit_bif_or(const ArgVal &Fail,
                                      const ArgVal &Src1,
                                      const ArgVal &Src2,
                                      const ArgVal &Dst) {
    static const Uint diff_bit = am_true - am_false;
    Label next = a.newLabel();

    auto [src1, src2] = load_sources(Src1, TMP1, Src2, TMP2);
    auto dst = init_destination(Dst, TMP3);

    ERTS_CT_ASSERT(am_false == make_atom(0));
    ERTS_CT_ASSERT(am_true == make_atom(1));

    a.and_(TMP3, src1.reg, imm(_TAG_IMMED2_MASK | ~diff_bit));
    a.and_(TMP4, src2.reg, imm(_TAG_IMMED2_MASK | ~diff_bit));
    a.cmp(TMP3, imm(_TAG_IMMED2_ATOM));
    a.ccmp(TMP3, TMP4, 0, arm::Cond::kEQ);

    if (Fail.getValue()) {
        a.cond_ne().b(resolve_beam_label(Fail, disp1MB));
    } else {
        a.cond_eq().b(next);
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

void BeamModuleAssembler::emit_bif_tl(const ArgVal &Src, const ArgVal &Tl) {
    Label good_cons = a.newLabel();
    auto src = load_source(Src, TMP1);
    auto tl = init_destination(Tl, TMP2);
    const int bitNumber = 1;

    ERTS_CT_ASSERT(_TAG_PRIMARY_MASK - TAG_PRIMARY_LIST == (1 << bitNumber));

    a.tbz(src.reg, bitNumber, good_cons);
    mov_var(XREG0, src);
    fragment_call(ga->get_handle_tl_error());

    a.bind(good_cons);
    {
        arm::Gp cons_ptr = emit_ptr_val(TMP1, src.reg);
        a.ldur(tl.reg, getCDRRef(cons_ptr));
        flush_var(tl);
    }
}

/* ================================================================
 *  tuple_size/1
 * ================================================================
 */

void BeamGlobalAssembler::emit_bif_tuple_size_helper(Label fail) {
    arm::Gp boxed_ptr = emit_ptr_val(TMP1, ARG1);

    emit_is_boxed(fail, boxed_ptr);

    ERTS_CT_ASSERT(_TAG_HEADER_ARITYVAL == 0);
    a.ldur(TMP1, emit_boxed_val(boxed_ptr));
    a.tst(TMP1, imm(_TAG_HEADER_MASK));
    a.cond_ne().b(fail);

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

void BeamModuleAssembler::emit_bif_tuple_size(const ArgVal &Fail,
                                              const ArgVal &Src,
                                              const ArgVal &Dst) {
    auto src = load_source(Src, ARG1);
    auto dst = init_destination(Dst, ARG1);

    mov_var(ARG1, src);

    if (Fail.getValue() == 0) {
        fragment_call(ga->get_bif_tuple_size_body());
    } else {
        fragment_call(ga->get_bif_tuple_size_guard());
        emit_branch_if_not_value(ARG1, resolve_beam_label(Fail, dispUnknown));
    }

    mov_var(dst, ARG1);
    flush_var(dst);
}
