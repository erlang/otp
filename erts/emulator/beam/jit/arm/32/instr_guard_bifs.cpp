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
    // TODO
    emit_nyi("emit_raise_badarg");
}

/* ================================================================
 *  '=:='/2
 *  '=/='/2
 *  '>='/2
 *  '<'/2
 * ================================================================
 */

void BeamGlobalAssembler::emit_bif_is_eq_exact_shared() {
    // TODO
    emit_nyi("emit_bif_is_eq_exact_shared");
}

void BeamGlobalAssembler::emit_bif_is_ne_exact_shared() {
    // TODO
    emit_nyi("emit_bif_is_ne_exact_shared");
}

void BeamModuleAssembler::emit_cond_to_bool(arm::CondCode cc,
                                            const ArgRegister &Dst) {
    // TODO
    emit_nyi("emit_cond_to_bool");
}

void BeamModuleAssembler::emit_cmp_immed_to_bool(arm::CondCode cc,
                                                 const ArgSource &LHS,
                                                 const ArgSource &RHS,
                                                 const ArgRegister &Dst) {
    // TODO
    emit_nyi("emit_cmp_immed_to_bool");
}

void BeamModuleAssembler::emit_bif_is_eq_exact(const ArgRegister &LHS,
                                               const ArgSource &RHS,
                                               const ArgRegister &Dst) {
    // TODO
    emit_nyi("emit_bif_is_eq_exact");
}

void BeamModuleAssembler::emit_bif_is_ne_exact(const ArgRegister &LHS,
                                               const ArgSource &RHS,
                                               const ArgRegister &Dst) {
    // TODO
    emit_nyi("emit_bif_is_ne_exact");
}

void BeamModuleAssembler::emit_bif_is_ge_lt(arm::CondCode cc,
                                            const ArgSource &LHS,
                                            const ArgSource &RHS,
                                            const ArgRegister &Dst) {
    // TODO
    emit_nyi("emit_bif_is_ge_lt");
}

void BeamModuleAssembler::emit_bif_is_ge(const ArgSource &LHS,
                                         const ArgSource &RHS,
                                         const ArgRegister &Dst) {
    // TODO
    emit_nyi("emit_bif_is_ge");
}

void BeamModuleAssembler::emit_bif_is_lt(const ArgSource &LHS,
                                         const ArgSource &RHS,
                                         const ArgRegister &Dst) {
    // TODO
    emit_nyi("emit_bif_is_lt");
}

/* ================================================================
 *  and/2
 * ================================================================
 */

void BeamGlobalAssembler::emit_handle_and_error() {
    // TODO
    emit_nyi("emit_handle_and_error");
}

void BeamModuleAssembler::emit_bif_and(const ArgLabel &Fail,
                                       const ArgSource &Src1,
                                       const ArgSource &Src2,
                                       const ArgRegister &Dst) {
    // TODO
    emit_nyi("emit_bif_and");
}

/* ================================================================
 *  bit_size/1
 * ================================================================
 */
void BeamGlobalAssembler::emit_bif_bit_size_helper(Label error) {
    // TODO
    emit_nyi("emit_bif_bit_size_helper");
}

void BeamGlobalAssembler::emit_bif_bit_size_body() {
    // TODO
    emit_nyi("emit_bif_bit_size_body");
}

void BeamModuleAssembler::emit_bif_bit_size(const ArgLabel &Fail,
                                            const ArgSource &Src,
                                            const ArgRegister &Dst) {
    // TODO
    emit_nyi("emit_bif_bit_size");
}

/* ================================================================
 *  byte_size/1
 * ================================================================
 */

void BeamGlobalAssembler::emit_bif_byte_size_body() {
    // TODO
    emit_nyi("emit_bif_byte_size_body");
}

void BeamModuleAssembler::emit_bif_byte_size(const ArgLabel &Fail,
                                             const ArgSource &Src,
                                             const ArgRegister &Dst) {
    // TODO
    emit_nyi("emit_bif_byte_size");
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
    // TODO
    emit_nyi("emit_handle_hd_error");
}

void BeamModuleAssembler::emit_bif_hd(const ArgSource &Src,
                                      const ArgRegister &Hd) {
    // TODO
    emit_nyi("emit_bif_hd");
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
    // TODO
    emit_nyi("emit_handle_map_get_badmap");
}

/* ================================================================
 *  map_get/2
 * ================================================================
 */

void BeamGlobalAssembler::emit_handle_map_get_badmap() {
    // TODO
    emit_nyi("emit_handle_map_get_badmap");
}

void BeamGlobalAssembler::emit_handle_map_get_badkey() {
    // TODO
    emit_nyi("emit_handle_map_get_badkey");
}

void BeamModuleAssembler::emit_bif_map_get(const ArgLabel &Fail,
                                           const ArgSource &Key,
                                           const ArgSource &Src,
                                           const ArgRegister &Dst) {
    // TODO
    emit_nyi("emit_bif_map_get");
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
    // TODO
    emit_nyi("emit_bif_min_max");
}

void BeamModuleAssembler::emit_bif_max(const ArgSource &LHS,
                                       const ArgSource &RHS,
                                       const ArgRegister &Dst) {
    // TODO
    emit_nyi("emit_bif_max");
}

void BeamModuleAssembler::emit_bif_min(const ArgSource &LHS,
                                       const ArgSource &RHS,
                                       const ArgRegister &Dst) {
    // TODO
    emit_nyi("emit_bif_min");
}

/* ================================================================
 *  node/1
 * ================================================================
 */

void BeamGlobalAssembler::emit_handle_node_error() {
    // TODO
    emit_nyi("emit_handle_node_error");
}

void BeamModuleAssembler::emit_bif_node(const ArgLabel &Fail,
                                        const ArgRegister &Src,
                                        const ArgRegister &Dst) {
    // TODO
    emit_nyi("emit_bif_node");
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
    // TODO
    emit_nyi("emit_handle_tl_error");
}

void BeamModuleAssembler::emit_bif_tl(const ArgSource &Src,
                                      const ArgRegister &Tl) {
    // TODO
    emit_nyi("emit_bif_tl");
}

/* ================================================================
 *  tuple_size/1
 * ================================================================
 */

void BeamGlobalAssembler::emit_bif_tuple_size_helper(Label fail) {
    // TODO
    emit_nyi("emit_bif_tuple_size_helper");
}

void BeamGlobalAssembler::emit_bif_tuple_size_body() {
    // TODO
    emit_nyi("emit_bif_tuple_size_body");
}

void BeamGlobalAssembler::emit_bif_tuple_size_guard() {
    // TODO
    emit_nyi("emit_bif_tuple_size_guard");
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
