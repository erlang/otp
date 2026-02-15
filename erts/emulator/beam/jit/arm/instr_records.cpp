/*
 * %CopyrightBegin%
 *
 * SPDX-License-Identifier: Apache-2.0
 *
 * Copyright Ericsson AB 2025-2026. All Rights Reserved.
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
#include "erl_record.h"
}

void BeamModuleAssembler::emit_is_any_native_record(const ArgLabel &Fail,
                                                    const ArgRegister &Src) {
    auto src = load_source(Src, ARG1);

    emit_is_boxed(resolve_beam_label(Fail, dispUnknown), Src, src.reg);

    a64::Gp boxed_ptr = emit_ptr_val(TMP3, src.reg);
    a.ldur(TMP3, emit_boxed_val(boxed_ptr));
    a.and_(TMP3, TMP3, imm(_TAG_HEADER_MASK));
    a.cmp(TMP3, imm(_TAG_HEADER_RECORD));
    a.b_ne(resolve_beam_label(Fail, disp1MB));
}

void BeamModuleAssembler::emit_is_native_record(const ArgLabel &Fail,
                                                const ArgRegister &Src,
                                                const ArgAtom &Module,
                                                const ArgAtom &Name) {
    mov_arg(ARG1, Src);
    mov_arg(ARG2, Module);
    mov_arg(ARG3, Name);

    emit_enter_runtime();
    runtime_call<bool (*)(Eterm, Eterm, Eterm), erl_is_native_record>();
    emit_leave_runtime();

    a.tbz(ARG1.w(), imm(0), resolve_beam_label(Fail, disp32K));
}

void BeamModuleAssembler::emit_is_record_accessible(const ArgLabel &Fail,
                                                    const ArgRegister &Src,
                                                    const ArgAtom &Scope) {
    mov_arg(ARG1, Src);

    emit_enter_runtime();
    if (Scope.get() == am_external) {
        comment("external operation");
        runtime_call<bool (*)(Eterm), erl_is_record_accessible>();
    } else {
        comment("auto_local operation");
        mov_arg(ARG2, ArgAtom(mod));
        runtime_call<bool (*)(Eterm, Eterm),
                     erl_is_wildcard_record_accessible>();
    }
    emit_leave_runtime();

    a.tbz(ARG1.w(), imm(0), resolve_beam_label(Fail, disp32K));
}

void BeamModuleAssembler::emit_i_get_record_elements(
        const ArgLabel &Fail,
        const ArgRegister &Src,
        const ArgWord &Size,
        const Span<const ArgVal> &args) {
    a.mov(ARG1, c_p);
    load_x_reg_array(ARG2);
    mov_arg(ARG3, Src);
    mov_imm(ARG4, args.size());
    embed_vararg_rodata(args, ARG5);

    emit_enter_runtime<Update::eHeapAlloc | Update::eXRegs>();

    runtime_call<bool (*)(Process *, Eterm *, Eterm, Uint, const Eterm *),
                 erl_get_record_elements>();

    emit_leave_runtime<Update::eHeapAlloc | Update::eXRegs |
                       Update::eReductions>();

    a.tbz(ARG1.w(), imm(0), resolve_beam_label(Fail, disp32K));
}

void BeamModuleAssembler::emit_i_create_local_native_record(
        const ArgLiteral &Def,
        const ArgRegister &Dst,
        const ArgWord &Live,
        const ArgWord &size,
        const Span<const ArgVal> &args) {
    Label next = a.new_label();

    a.mov(ARG1, c_p);
    load_x_reg_array(ARG2);
    mov_arg(ARG3, Def);
    mov_arg(ARG4, Live);
    mov_imm(ARG5, args.size());
    embed_vararg_rodata(args, ARG6);

    emit_enter_runtime<Update::eHeapAlloc | Update::eXRegs |
                       Update::eReductions>(Live.get());

    runtime_call<
            Eterm (*)(Process *, Eterm *, Eterm, Uint, Uint, const Eterm *),
            erl_create_local_native_record>();

    emit_leave_runtime<Update::eHeapAlloc | Update::eXRegs |
                       Update::eReductions>(Live.get());

    emit_branch_if_value(ARG1, next);
    emit_raise_exception();

    a.bind(next);
    mov_arg(Dst, ARG1);
}

void BeamModuleAssembler::emit_i_create_native_record(
        const ArgConstant &Id,
        const ArgRegister &Dst,
        const ArgWord &Live,
        const ArgWord &size,
        const Span<const ArgVal> &args) {
    Label next = a.new_label();

    a.mov(ARG1, c_p);
    load_x_reg_array(ARG2);
    mov_arg(ARG3, Id);
    mov_arg(ARG4, Live);
    mov_imm(ARG5, args.size());
    embed_vararg_rodata(args, ARG6);

    emit_enter_runtime<Update::eHeapAlloc | Update::eXRegs |
                       Update::eReductions>(Live.get());

    runtime_call<
            Eterm (*)(Process *, Eterm *, Eterm, Uint, Uint, const Eterm *),
            erl_create_native_record>();

    emit_leave_runtime<Update::eHeapAlloc | Update::eXRegs |
                       Update::eReductions>(Live.get());

    emit_branch_if_value(ARG1, next);
    emit_raise_exception();

    a.bind(next);
    mov_arg(Dst, ARG1);
}

void BeamModuleAssembler::emit_i_update_native_record(
        const ArgSource &Src,
        const ArgRegister &Dst,
        const ArgWord &Live,
        const ArgWord &size,
        const Span<const ArgVal> &args) {
    Label next = a.new_label();

    a.mov(ARG1, c_p);
    load_x_reg_array(ARG2);
    mov_arg(ARG3, Src);
    mov_arg(ARG4, Live);
    mov_imm(ARG5, args.size());
    embed_vararg_rodata(args, ARG6);

    emit_enter_runtime<Update::eHeapAlloc | Update::eXRegs |
                       Update::eReductions>();

    runtime_call<
            Eterm (*)(Process *, Eterm *, Eterm, Uint, Uint, const Eterm *args),
            erl_update_native_record>();

    emit_leave_runtime<Update::eHeapAlloc | Update::eXRegs |
                       Update::eReductions>();

    emit_branch_if_value(ARG1, next);
    emit_raise_exception();

    a.bind(next);
    mov_arg(Dst, ARG1);
}

void BeamModuleAssembler::emit_get_record_field(const ArgLabel &Fail,
                                                const ArgRegister &Src,
                                                const ArgConstant &Id,
                                                const ArgAtom &Name,
                                                const ArgRegister &Dst) {
    a.mov(ARG1, c_p);
    mov_arg(ARG2, Src);
    mov_arg(ARG3, Id);
    mov_arg(ARG4, Name);

    emit_enter_runtime<Update::eHeapAlloc>();
    if (Id.isImmed()) {
        comment("local record");
        runtime_call<Eterm (*)(Process *, Eterm, Eterm, Eterm),
                     erl_get_local_record_field>();
    } else {
        comment("external record");
        runtime_call<Eterm (*)(Process *, Eterm, Eterm, Eterm),
                     erl_get_record_field>();
    }
    emit_leave_runtime<Update::eHeapAlloc>();

    if (Fail.get() != 0) {
        emit_branch_if_not_value(ARG1, resolve_beam_label(Fail, dispUnknown));
    } else {
        Label next = a.new_label();

        emit_branch_if_value(ARG1, next);
        emit_raise_exception();

        a.bind(next);
    }

    mov_arg(Dst, ARG1);
}
