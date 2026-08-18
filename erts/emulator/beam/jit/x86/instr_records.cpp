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
    mov_arg(ARG3, Src);

    emit_is_boxed(resolve_beam_label(Fail), Src, ARG3);

    preserve_cache(
            [&]() {
                x86::Gp boxed_ptr = emit_ptr_val(RET, ARG3);
                a.mov(RETd, emit_boxed_val(boxed_ptr, 0, sizeof(Uint32)));
                a.and_(RETb, imm(_TAG_HEADER_MASK));
                a.cmp(RETb, imm(_TAG_HEADER_RECORD));
                a.jne(resolve_beam_label(Fail));
            },
            RET);
}

void BeamModuleAssembler::emit_is_native_record(const ArgLabel &Fail,
                                                const ArgRegister &Src,
                                                const ArgAtom &Module,
                                                const ArgAtom &Name) {
    x86::Gp boxed_ptr;

    mov_arg(ARG3, Src);
    boxed_ptr = emit_ptr_val(RET, ARG3);

    mov_preserve_cache(
            RET,
            emit_boxed_val(boxed_ptr,
                           offsetof(ErtsRecordInstance, record_definition),
                           sizeof(Uint64)));
    boxed_ptr = emit_ptr_val(RET, RET);
    cmp_arg(emit_boxed_val(boxed_ptr, offsetof(ErtsRecordDefinition, module)),
            Module);
    preserve_cache([&]() {
        a.jne(resolve_beam_label(Fail));
    });
    cmp_arg(emit_boxed_val(boxed_ptr, offsetof(ErtsRecordDefinition, name)),
            Name);
    preserve_cache([&]() {
        a.jne(resolve_beam_label(Fail));
    });
}

void BeamModuleAssembler::emit_is_record_accessible(const ArgLabel &Fail,
                                                    const ArgRegister &Src,
                                                    const ArgAtom &Scope) {
    x86::Gp boxed_ptr;
    mov_arg(ARG3, Src);

    boxed_ptr = emit_ptr_val(RET, ARG3);

    mov_preserve_cache(
            RET,
            emit_boxed_val(boxed_ptr,
                           offsetof(ErtsRecordInstance, record_definition)));
    boxed_ptr = emit_ptr_val(RET, RET);
    cmp_arg(emit_boxed_val(boxed_ptr,
                           offsetof(ErtsRecordDefinition, is_exported)),
            ArgAtom(am_true));
    if (Scope.get() == am_external) {
        comment("external operation");
        preserve_cache([&]() {
            a.jne(resolve_beam_label(Fail));
        });
    } else {
        Label next = a.new_label();

        comment("auto_local operation");
        preserve_cache([&]() {
            a.short_().je(next);
        });

        cmp_arg(emit_boxed_val(boxed_ptr,
                               offsetof(ErtsRecordDefinition, module)),
                ArgAtom(mod));
        preserve_cache([&]() {
            a.jne(resolve_beam_label(Fail));
        });

        a.bind(next);
    }
}

void BeamModuleAssembler::emit_i_get_local_record_elements(
        const ArgLiteral &Def,
        const ArgRegister &Src,
        const ArgWord &Size,
        const Span<const ArgVal> &args) {
    Eterm def;
    ErtsRecordDefinition *defp;
    int field_count;
    Eterm cons = beamfile_get_literal(beam, Def.get());
    Uint argp;
    const Uint header_offset =
            offsetof(ErtsRecordInstance, values) - TAG_PRIMARY_BOXED;

    def = CAR(list_val(cons));
    defp = (ErtsRecordDefinition *)tuple_val(def);

    field_count = RECORD_DEF_FIELD_COUNT(defp);

    comment("name: %T", defp->name);
    mov_arg(ARG1, Src);
    emit_ptr_val(ARG1, ARG1);
    argp = 0;
    for (int i = 0; i < field_count; i++) {
        if (argp + 1 < args.size() &&
            args[argp].as<ArgAtom>().get() == defp->keys[i]) {
            if (argp + 3 < args.size() &&
                args[argp + 2].as<ArgAtom>().get() == defp->keys[i + 1]) {
                emit_get_pair(ARG1,
                              header_offset + i * sizeof(Eterm),
                              args[argp + 1],
                              args[argp + 3]);
                argp += 4;
            } else {
                a.mov(ARG2,
                      x86::qword_ptr(ARG1, header_offset + i * sizeof(Eterm)));
                mov_arg(args[argp + 1], ARG2);
                argp += 2;
            }
        }
    }
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
    embed_vararg_rodata(args, ARG5, 0);

    emit_enter_runtime<Update::eStack>();
    runtime_call<bool (*)(Process *, Eterm *, Eterm, Uint, const Eterm *),
                 erl_get_record_elements>();
    emit_leave_runtime<Update::eStack>();

    a.test(RETb, RETb);
    a.je(resolve_beam_label(Fail));
}

void BeamModuleAssembler::emit_i_create_local_native_record(
        const ArgLiteral &Def,
        const ArgRegister &Dst,
        const ArgWord &Live,
        const ArgWord &size,
        const Span<const ArgVal> &args) {
    Eterm def;
    ErtsRecordDefinition *defp;
    int field_count;
    Uint num_words_needed;
    Eterm *loader_def_values;
    Eterm cons = beamfile_get_literal(beam, Def.get());
    Uint argp;
    bool otp_29 = beam->code.max_opcode <= genop_get_record_field_5;

    def = CAR(list_val(cons));
    defp = (ErtsRecordDefinition *)tuple_val(def);
    loader_def_values = tuple_val(CDR(list_val(cons))) + 1;

    field_count = RECORD_DEF_FIELD_COUNT(defp);
    num_words_needed = RECORD_INST_SIZE(field_count);

    comment("name: %T", defp->name);

    if (otp_29) {
        /* If compiled by OTP 29, we must do a GC test here.
         * If compiled by OTP 30 or later, this instruction is
         * preceded by a `test_heap` instruction that has already
         * ensured sufficient heap space. */
        emit_gc_test(ArgWord(0), ArgWord(num_words_needed), Live);
    }

    extract_from_literal(RET, Def, [](Eterm value) -> Eterm {
        return CAR(list_val(value));
    });

    a.mov(x86::qword_ptr(HTOP), MAKE_RECORD_HEADER(field_count));
    a.mov(x86::qword_ptr(HTOP, sizeof(Eterm)), RET);

    argp = 0;
    for (int i = 0; i < field_count; i++) {
        x86::Mem dst_ptr = x86::qword_ptr(HTOP, (i + 2) * sizeof(Eterm));
        if (argp < args.size() &&
            args[argp].as<ArgAtom>().get() == defp->keys[i]) {
            if (args[argp + 1].isImmed() &&
                Support::is_int_n<32>(
                        (Sint)(args[argp + 1].as<ArgImmed>().get()))) {
                Eterm value = args[argp + 1].as<ArgImmed>().get();
                a.mov(dst_ptr, imm(value));
            } else {
                mov_arg(RET, args[argp + 1]);
                a.mov(dst_ptr, RET);
            }
            argp += 2;
        } else {
            Eterm value = loader_def_values[i];
            if (is_immed(value) && Support::is_int_n<32>((Sint)(value))) {
                a.mov(dst_ptr, imm(value));
            } else {
                extract_from_literal(RET, Def, [i](Eterm value) -> Eterm {
                    auto defaults = CDR(list_val(value));
                    return tuple_val(defaults)[i + 1];
                });

                a.mov(dst_ptr, RET);
            }
        }
    }

    comment("Create boxed ptr");
    x86::Gp tmp_reg = alloc_temp_reg();
    preserve_cache(
            [&]() {
                a.lea(tmp_reg, x86::qword_ptr(HTOP, TAG_PRIMARY_BOXED));
                a.add(HTOP, imm(num_words_needed * sizeof(Eterm)));
            },
            HTOP,
            tmp_reg);

    mov_arg(Dst, tmp_reg);
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
    embed_vararg_rodata(args, ARG6, 0);

    emit_enter_runtime<Update::eHeapAlloc | Update::eReductions>();

    runtime_call<
            Eterm (*)(Process *, Eterm *, Eterm, Uint, Uint, const Eterm *),
            erl_create_native_record>();

    emit_leave_runtime<Update::eHeapAlloc | Update::eReductions>();

    emit_test_the_non_value(RET);
    a.short_().jne(next);

    emit_raise_exception();

    a.bind(next);
    mov_arg(Dst, RET);
}

void BeamModuleAssembler::emit_i_update_local_native_record(
        const ArgAtom &Hint,
        const ArgLiteral &Def,
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
    embed_vararg_rodata(args, ARG6, 0);

    emit_enter_runtime<Update::eHeapAlloc | Update::eReductions>();

    runtime_call<
            Eterm (*)(Process *, Eterm *, Eterm, Uint, Uint, const Eterm *args),
            erl_update_native_record>();

    emit_leave_runtime<Update::eHeapAlloc | Update::eReductions>();

    emit_test_the_non_value(RET);
    a.short_().jne(next);

    emit_raise_exception();

    a.bind(next);
    mov_arg(Dst, RET);
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
    embed_vararg_rodata(args, ARG6, 0);

    emit_enter_runtime<Update::eHeapAlloc | Update::eReductions>();

    runtime_call<
            Eterm (*)(Process *, Eterm *, Eterm, Uint, Uint, const Eterm *args),
            erl_update_native_record>();

    emit_leave_runtime<Update::eHeapAlloc | Update::eReductions>();

    emit_test_the_non_value(RET);
    a.short_().jne(next);

    emit_raise_exception();

    a.bind(next);
    mov_arg(Dst, RET);
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
        emit_test_the_non_value(RET);
        a.je(resolve_beam_label(Fail));
    } else {
        Label next = a.new_label();

        emit_test_the_non_value(RET);
        a.short_().jne(next);

        emit_raise_exception();

        a.bind(next);
    }

    mov_arg(Dst, RET);
}
