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

    mov_arg(ARG1, map);
    emit_is_boxed(badmap, ARG1, dShort);
    /* We use ARG1 in the badmap branch, so use ARG2 below */
    x86::Gp boxed_ptr = emit_ptr_val(ARG2, ARG1);
    a.mov(ARG2, emit_boxed_val(boxed_ptr));
    a.and_(ARG2d, imm(_TAG_HEADER_MASK));
    a.cmp(ARG2d, imm(_TAG_HEADER_MAP));
    a.short_().je(next);

    a.bind(badmap);
    {
        a.mov(x86::qword_ptr(c_p, offsetof(Process, fvalue)), ARG1);
        emit_error(BADMAP);
    }

    a.bind(next);
}

void BeamGlobalAssembler::emit_new_map_shared() {
    emit_enter_frame();
    emit_enter_runtime<Update::eReductions | Update::eStack | Update::eHeap>();

    a.mov(ARG1, c_p);
    load_x_reg_array(ARG2);
    runtime_call<5>(erts_gc_new_map);

    emit_leave_runtime<Update::eReductions | Update::eStack | Update::eHeap>();
    emit_leave_frame();

    a.ret();
}

void BeamModuleAssembler::emit_new_map(const ArgVal &Dst,
                                       const ArgVal &Live,
                                       const ArgVal &Size,
                                       const Span<ArgVal> &args) {
    Label data = embed_vararg_rodata(args, CP_SIZE);

    ASSERT(Size.getValue() == args.size());

    mov_imm(ARG3, Live.getValue());
    mov_imm(ARG4, args.size());
    a.lea(ARG5, x86::qword_ptr(data));
    fragment_call(ga->get_new_map_shared());

    mov_arg(Dst, RET);
}

void BeamGlobalAssembler::emit_i_new_small_map_lit_shared() {
    emit_enter_frame();
    emit_enter_runtime<Update::eReductions | Update::eStack | Update::eHeap>();

    a.mov(ARG1, c_p);
    load_x_reg_array(ARG2);
    runtime_call<5>(erts_gc_new_small_map_lit);

    emit_leave_runtime<Update::eReductions | Update::eStack | Update::eHeap>();
    emit_leave_frame();

    a.ret();
}

void BeamModuleAssembler::emit_i_new_small_map_lit(const ArgVal &Dst,
                                                   const ArgVal &Live,
                                                   const ArgVal &Keys,
                                                   const ArgVal &Size,
                                                   const Span<ArgVal> &args) {
    Label data = embed_vararg_rodata(args, CP_SIZE);

    ASSERT(Size.getValue() == args.size());

    ASSERT(Keys.isLiteral());
    mov_arg(ARG3, Keys);
    mov_imm(ARG4, Live.getValue());
    a.lea(ARG5, x86::qword_ptr(data));

    fragment_call(ga->get_i_new_small_map_lit_shared());

    mov_arg(Dst, RET);
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

    emit_test_the_non_value(RET);
    a.je(labels[Fail.getValue()]);

    /*
     * Don't store the result if the destination is the scratch X register.
     * (This instruction was originally a has_map_fields instruction.)
     */
    if (!(Dst.getType() == ArgVal::XReg && Dst.getValue() == SCRATCH_X_REG)) {
        mov_arg(Dst, RET);
    }
}

void BeamModuleAssembler::emit_i_get_map_elements(const ArgVal &Fail,
                                                  const ArgVal &Src,
                                                  const ArgVal &Size,
                                                  const Span<ArgVal> &args) {
    Label data = embed_vararg_rodata(args, 0);

    ASSERT(Size.getValue() == args.size());

    mov_arg(ARG1, Src);
    a.mov(ARG3, E);

    emit_enter_runtime();

    mov_imm(ARG4, args.size() / 3);
    a.lea(ARG5, x86::qword_ptr(data));
    load_x_reg_array(ARG2);
    runtime_call<5>(beam_jit_get_map_elements);

    emit_leave_runtime();

    a.test(RET, RET);
    a.je(labels[Fail.getValue()]);
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

    emit_test_the_non_value(RET);
    a.je(labels[Fail.getValue()]);

    /*
     * Don't store the result if the destination is the scratch X register.
     * (This instruction was originally a has_map_fields instruction.)
     */
    if (!(Dst.getType() == ArgVal::XReg && Dst.getValue() == SCRATCH_X_REG)) {
        mov_arg(Dst, RET);
    }
}

/* ARG3 = live registers, ARG4 = update vector size, ARG5 = update vector. */
void BeamGlobalAssembler::emit_update_map_assoc_shared() {
    emit_enter_frame();
    emit_enter_runtime<Update::eReductions | Update::eStack | Update::eHeap>();

    a.mov(ARG1, c_p);
    load_x_reg_array(ARG2);
    runtime_call<5>(erts_gc_update_map_assoc);

    emit_leave_runtime<Update::eReductions | Update::eStack | Update::eHeap>();
    emit_leave_frame();

    a.ret();
}

void BeamModuleAssembler::emit_update_map_assoc(const ArgVal &Src,
                                                const ArgVal &Dst,
                                                const ArgVal &Live,
                                                const ArgVal &Size,
                                                const Span<ArgVal> &args) {
    Label data = embed_vararg_rodata(args, CP_SIZE);

    ASSERT(Size.getValue() == args.size());

    mov_arg(getXRef(Live.getValue()), Src);

    mov_imm(ARG3, Live.getValue());
    mov_imm(ARG4, args.size());
    a.lea(ARG5, x86::qword_ptr(data));
    fragment_call(ga->get_update_map_assoc_shared());

    mov_arg(Dst, RET);
}

/* ARG3 = live registers, ARG4 = update vector size, ARG5 = update vector.
 *
 * Result is returned in RET, error is indicated by ZF. */
void BeamGlobalAssembler::emit_update_map_exact_guard_shared() {
    emit_enter_frame();
    emit_enter_runtime<Update::eReductions | Update::eStack | Update::eHeap>();

    a.mov(ARG1, c_p);
    load_x_reg_array(ARG2);
    runtime_call<5>(erts_gc_update_map_exact);

    emit_leave_runtime<Update::eReductions | Update::eStack | Update::eHeap>();
    emit_leave_frame();

    emit_test_the_non_value(RET);
    a.ret();
}

/* ARG3 = live registers, ARG4 = update vector size, ARG5 = update vector.
 *
 * Does not return on error. */
void BeamGlobalAssembler::emit_update_map_exact_body_shared() {
    Label error = a.newLabel();

    emit_enter_frame();
    emit_enter_runtime<Update::eReductions | Update::eStack | Update::eHeap>();

    a.mov(ARG1, c_p);
    load_x_reg_array(ARG2);
    runtime_call<5>(erts_gc_update_map_exact);

    emit_leave_runtime<Update::eReductions | Update::eStack | Update::eHeap>();
    emit_leave_frame();

    emit_test_the_non_value(RET);
    a.short_().je(error);

    a.ret();

    a.bind(error);
    {
        mov_imm(ARG4, 0);
        a.jmp(labels[raise_exception]);
    }
}

void BeamModuleAssembler::emit_update_map_exact(const ArgVal &Src,
                                                const ArgVal &Fail,
                                                const ArgVal &Dst,
                                                const ArgVal &Live,
                                                const ArgVal &Size,
                                                const Span<ArgVal> &args) {
    Label data = embed_vararg_rodata(args, CP_SIZE);

    ASSERT(Size.getValue() == args.size());

    /* We _KNOW_ Src is a map */
    mov_arg(getXRef(Live.getValue()), Src);

    mov_imm(ARG3, Live.getValue());
    mov_imm(ARG4, args.size());
    a.lea(ARG5, x86::qword_ptr(data));

    if (Fail.getValue() != 0) {
        fragment_call(ga->get_update_map_exact_guard_shared());
        a.je(labels[Fail.getValue()]);
    } else {
        fragment_call(ga->get_update_map_exact_body_shared());
    }

    mov_arg(Dst, RET);
}
