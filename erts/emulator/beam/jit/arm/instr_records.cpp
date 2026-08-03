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
    auto src = load_source(Src, ARG3);

    emit_is_boxed(resolve_beam_label(Fail, dispUnknown), Src, src.reg);

    preserve_cache(
            [&]() {
                a64::Gp boxed_ptr = emit_ptr_val(TMP3, src.reg);
                a.ldur(TMP3, emit_boxed_val(boxed_ptr));
                a.and_(TMP3, TMP3, imm(_TAG_HEADER_MASK));
                a.cmp(TMP3, imm(_TAG_HEADER_RECORD));
                a.b_ne(resolve_beam_label(Fail, disp1MB));
            },
            TMP3);
}

void BeamModuleAssembler::emit_is_native_record(const ArgLabel &Fail,
                                                const ArgRegister &Src,
                                                const ArgAtom &Module,
                                                const ArgAtom &Name) {
    auto src = load_source(Src, ARG3);

    preserve_cache(
            [&]() {
                a64::Gp boxed_ptr = emit_ptr_val(TMP1, src.reg);
                a.ldur(TMP1,
                       emit_boxed_val(boxed_ptr,
                                      offsetof(ErtsRecordInstance,
                                               record_definition)));
                boxed_ptr = emit_ptr_val(TMP1, TMP1);
                lea(TMP1,
                    emit_boxed_val(boxed_ptr,
                                   offsetof(ErtsRecordDefinition, module)));
                ERTS_CT_ASSERT_FIELD_PAIR(ErtsRecordDefinition, module, name);
                a.ldp(TMP2, TMP3, a64::Mem(TMP1));

                mov_imm(TMP1, Module.get());
                a.cmp(TMP2, TMP1);
                mov_imm(TMP1, Name.get());
                a.ccmp(TMP3, TMP1, imm(NZCV::kNone), imm(arm::CondCode::kEQ));
                a.b_ne(resolve_beam_label(Fail, disp1MB));
            },
            TMP1,
            TMP2,
            TMP3);
}

void BeamModuleAssembler::emit_is_record_accessible(const ArgLabel &Fail,
                                                    const ArgRegister &Src,
                                                    const ArgAtom &Scope) {
    auto src = load_source(Src, ARG3);

    preserve_cache(
            [&]() {
                a64::Gp boxed_ptr = emit_ptr_val(TMP1, src.reg);
                a.ldur(TMP1,
                       emit_boxed_val(boxed_ptr,
                                      offsetof(ErtsRecordInstance,
                                               record_definition)));
                boxed_ptr = emit_ptr_val(TMP1, TMP1);
                a.ldr(TMP2,
                      emit_boxed_val(
                              boxed_ptr,
                              offsetof(ErtsRecordDefinition, is_exported)));
                if (Scope.get() == am_external) {
                    const Uint bit_num = _TAG_IMMED2_SIZE;
                    ERTS_CT_ASSERT(am_false == make_atom(0));
                    ERTS_CT_ASSERT(am_true == make_atom(1));
                    ERTS_CT_ASSERT((1 << bit_num) == (am_true - am_false));

                    comment("external operation");
                    a.tbz(TMP2,
                          imm(bit_num),
                          resolve_beam_label(Fail, disp32K));
                } else {
                    comment("auto_local operation");
                    cmp(TMP2, am_true);
                    a.ldr(TMP2,
                          emit_boxed_val(
                                  TMP1,
                                  offsetof(ErtsRecordDefinition, module)));
                    mov_imm(TMP1, mod);
                    a.ccmp(TMP2,
                           TMP1,
                           imm(NZCV::kEqual),
                           imm(arm::CondCode::kNE));
                    a.b_ne(resolve_beam_label(Fail, disp1MB));
                }
            },
            TMP1,
            TMP2);
}

void BeamModuleAssembler::emit_i_get_record_elements(
        const ArgLabel &Fail,
        const ArgRegister &Src,
        const ArgWord &Size,
        const Span<const ArgVal> &args) {
    mov_arg(ARG3, Src);
    a.mov(ARG1, c_p);
    load_x_reg_array(ARG2);
    mov_imm(ARG4, args.size());
    embed_vararg_rodata(args, ARG5);

    emit_enter_runtime<Update::eStack | Update::eXRegs>();

    runtime_call<bool (*)(Process *, Eterm *, Eterm, Uint, const Eterm *),
                 erl_get_record_elements>();

    emit_leave_runtime<Update::eXRegs>();

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

    mov_arg(ARG3, Src);
    a.mov(ARG1, c_p);
    load_x_reg_array(ARG2);
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
