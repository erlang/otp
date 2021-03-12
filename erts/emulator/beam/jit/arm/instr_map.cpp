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
#include <algorithm>
#include "beam_asm.hpp"

using namespace asmjit;

extern "C"
{
#include "erl_map.h"
#include "beam_common.h"
}

void BeamModuleAssembler::emit_ensure_map(const ArgVal &map) {
    Label next = a.newLabel(), badmap = a.newLabel();

    auto map_reg = load_source(map, TMP1);

    emit_is_boxed(badmap, map_reg.reg);

    arm::Gp boxed_ptr = emit_ptr_val(TMP2, map_reg.reg);
    a.ldur(TMP2, emit_boxed_val(boxed_ptr));
    a.and_(TMP2, TMP2, imm(_TAG_HEADER_MASK));
    a.cmp(TMP2, imm(_TAG_HEADER_MAP));
    a.cond_eq().b(next);

    a.bind(badmap);
    {
        a.stur(map_reg.reg, arm::Mem(c_p, offsetof(Process, fvalue)));
        emit_error(BADMAP);
    }

    a.bind(next);
}

void BeamGlobalAssembler::emit_new_map_shared() {
    emit_enter_runtime_frame();
    emit_enter_runtime<Update::eStack | Update::eHeap | Update::eXRegs |
                       Update::eReductions>();

    a.mov(ARG1, c_p);
    load_x_reg_array(ARG2);
    runtime_call<5>(erts_gc_new_map);

    emit_leave_runtime<Update::eStack | Update::eHeap | Update::eXRegs |
                       Update::eReductions>();
    emit_leave_runtime_frame();

    a.ret(a64::x30);
}

void BeamModuleAssembler::emit_new_map(const ArgVal &Dst,
                                       const ArgVal &Live,
                                       const ArgVal &Size,
                                       const Span<ArgVal> &args) {
    embed_vararg_rodata(args, ARG5);

    mov_imm(ARG3, Live.getValue());
    mov_imm(ARG4, args.size());
    fragment_call(ga->get_new_map_shared());

    mov_arg(Dst, ARG1);
}

void BeamGlobalAssembler::emit_i_new_small_map_lit_shared() {
    emit_enter_runtime_frame();
    emit_enter_runtime<Update::eStack | Update::eHeap | Update::eXRegs |
                       Update::eReductions>();

    a.mov(ARG1, c_p);
    load_x_reg_array(ARG2);
    runtime_call<5>(erts_gc_new_small_map_lit);

    emit_leave_runtime<Update::eStack | Update::eHeap | Update::eXRegs |
                       Update::eReductions>();
    emit_leave_runtime_frame();

    a.ret(a64::x30);
}

void BeamModuleAssembler::emit_i_new_small_map_lit(const ArgVal &Dst,
                                                   const ArgVal &Live,
                                                   const ArgVal &Keys,
                                                   const ArgVal &Size,
                                                   const Span<ArgVal> &args) {
    ASSERT(Size.getValue() == args.size());

    embed_vararg_rodata(args, ARG5);

    ASSERT(Keys.isLiteral());
    mov_arg(ARG3, Keys);
    mov_imm(ARG4, Live.getValue());

    fragment_call(ga->get_i_new_small_map_lit_shared());

    mov_arg(Dst, ARG1);
}

void BeamModuleAssembler::emit_i_get_map_element(const ArgVal &Fail,
                                                 const ArgVal &Src,
                                                 const ArgVal &Key,
                                                 const ArgVal &Dst) {
    mov_arg(ARG1, Src);
    mov_arg(ARG2, Key);

    emit_enter_runtime();

    runtime_call<2>(get_map_element);

    emit_leave_runtime();

    emit_branch_if_not_value(ARG1, resolve_beam_label(Fail, dispUnknown));

    /*
     * Don't store the result if the destination is the scratch X register.
     * (This instruction was originally a has_map_fields instruction.)
     */
    if (!(Dst.getType() == ArgVal::XReg && Dst.getValue() == SCRATCH_X_REG)) {
        mov_arg(Dst, ARG1);
    }
}

void BeamModuleAssembler::emit_i_get_map_elements(const ArgVal &Fail,
                                                  const ArgVal &Src,
                                                  const ArgVal &Size,
                                                  const Span<ArgVal> &args) {
    ASSERT(Size.getValue() == args.size());

    embed_vararg_rodata(args, ARG5);

    mov_arg(ARG1, Src);
    a.mov(ARG3, E);

    emit_enter_runtime<Update::eXRegs>();

    mov_imm(ARG4, args.size() / 3);
    load_x_reg_array(ARG2);
    runtime_call<5>(beam_jit_get_map_elements);

    emit_leave_runtime<Update::eXRegs>();

    a.cbz(ARG1, resolve_beam_label(Fail, disp1MB));
}

void BeamModuleAssembler::emit_i_get_map_element_hash(const ArgVal &Fail,
                                                      const ArgVal &Src,
                                                      const ArgVal &Key,
                                                      const ArgVal &Hx,
                                                      const ArgVal &Dst) {
    mov_arg(ARG1, Src);
    mov_arg(ARG2, Key);
    mov_arg(ARG3, Hx);

    emit_enter_runtime();

    runtime_call<3>(get_map_element_hash);

    emit_leave_runtime();

    emit_branch_if_not_value(ARG1, resolve_beam_label(Fail, dispUnknown));

    /*
     * Don't store the result if the destination is the scratch X register.
     * (This instruction was originally a has_map_fields instruction.)
     */
    if (!(Dst.getType() == ArgVal::XReg && Dst.getValue() == SCRATCH_X_REG)) {
        mov_arg(Dst, ARG1);
    }
}

/* ARG3 = live registers, ARG4 = update vector size, ARG5 = update vector. */
void BeamGlobalAssembler::emit_update_map_assoc_shared() {
    emit_enter_runtime_frame();
    emit_enter_runtime<Update::eStack | Update::eHeap | Update::eXRegs |
                       Update::eReductions>();

    a.mov(ARG1, c_p);
    load_x_reg_array(ARG2);
    runtime_call<5>(erts_gc_update_map_assoc);

    emit_leave_runtime<Update::eStack | Update::eHeap | Update::eXRegs |
                       Update::eReductions>();
    emit_leave_runtime_frame();

    a.ret(a64::x30);
}

void BeamModuleAssembler::emit_update_map_assoc(const ArgVal &Src,
                                                const ArgVal &Dst,
                                                const ArgVal &Live,
                                                const ArgVal &Size,
                                                const Span<ArgVal> &args) {
    auto src_reg = load_source(Src, TMP1);

    ASSERT(Size.getValue() == args.size());

    embed_vararg_rodata(args, ARG5);

    mov_arg(ArgVal(ArgVal::XReg, Live.getValue()), src_reg.reg);
    mov_imm(ARG3, Live.getValue());
    mov_imm(ARG4, args.size());

    fragment_call(ga->get_update_map_assoc_shared());

    mov_arg(Dst, ARG1);
}

/* ARG3 = live registers, ARG4 = update vector size, ARG5 = update vector.
 *
 * Result is returned in RET, error is indicated by ZF. */
void BeamGlobalAssembler::emit_update_map_exact_guard_shared() {
    emit_enter_runtime_frame();
    emit_enter_runtime<Update::eStack | Update::eHeap | Update::eXRegs |
                       Update::eReductions>();

    a.mov(ARG1, c_p);
    load_x_reg_array(ARG2);
    runtime_call<5>(erts_gc_update_map_exact);

    emit_leave_runtime<Update::eStack | Update::eHeap | Update::eXRegs |
                       Update::eReductions>();
    emit_leave_runtime_frame();

    a.ret(a64::x30);
}

/* ARG3 = live registers, ARG4 = update vector size, ARG5 = update vector.
 *
 * Does not return on error. */
void BeamGlobalAssembler::emit_update_map_exact_body_shared() {
    Label error = a.newLabel();

    emit_enter_runtime_frame();
    emit_enter_runtime<Update::eStack | Update::eHeap | Update::eXRegs |
                       Update::eReductions>();

    a.mov(ARG1, c_p);
    load_x_reg_array(ARG2);
    runtime_call<5>(erts_gc_update_map_exact);

    emit_leave_runtime<Update::eStack | Update::eHeap | Update::eXRegs |
                       Update::eReductions>();
    emit_leave_runtime_frame();

    emit_branch_if_not_value(ARG1, error);
    a.ret(a64::x30);

    a.bind(error);
    {
        mov_imm(ARG4, 0);
        a.b(labels[raise_exception]);
    }
}

void BeamModuleAssembler::emit_update_map_exact(const ArgVal &Src,
                                                const ArgVal &Fail,
                                                const ArgVal &Dst,
                                                const ArgVal &Live,
                                                const ArgVal &Size,
                                                const Span<ArgVal> &args) {
    auto src_reg = load_source(Src, TMP1);

    ASSERT(Size.getValue() == args.size());

    embed_vararg_rodata(args, ARG5);

    /* We _KNOW_ Src is a map */

    mov_arg(ArgVal(ArgVal::XReg, Live.getValue()), src_reg.reg);
    mov_imm(ARG3, Live.getValue());
    mov_imm(ARG4, args.size());

    if (Fail.getValue() != 0) {
        fragment_call(ga->get_update_map_exact_guard_shared());
        emit_branch_if_not_value(ARG1, resolve_beam_label(Fail, dispUnknown));
    } else {
        fragment_call(ga->get_update_map_exact_body_shared());
    }

    mov_arg(Dst, ARG1);
}
