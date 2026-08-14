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

void BeamModuleAssembler::emit_i_get_local_record_elements(
        const ArgLiteral &Def,
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

    if (Fail.get() != 0) {
        a.tbz(ARG1.w(), imm(0), resolve_beam_label(Fail, disp32K));
    }
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
    int argp;
    Variable<a64::Gp> regs[2] = {a64::xzr, a64::xzr};
    bool any_literal_defaults = false;
    bool otp_29 = beam->code.max_opcode <= genop_get_record_field_5;

    def = CAR(list_val(cons));
    defp = (ErtsRecordDefinition *)tuple_val(def);
    loader_def_values = tuple_val(CDR(list_val(cons))) + 1;

    field_count = RECORD_DEF_FIELD_COUNT(defp);
    num_words_needed = RECORD_INST_SIZE(field_count);

    comment("name: %T", defp->name);

    /* Find out whether any non-immediate default value will
     * be used for this construction. */
    argp = 0;
    for (int i = 0; i < field_count; i++) {
        if (argp < args.size() &&
            args[argp].as<ArgAtom>().get() == defp->keys[i]) {
            argp += 2;
        } else {
            Eterm value = loader_def_values[i];
            any_literal_defaults |= !is_immed(value);
        }
    }

    if (otp_29) {
        /* If compiled by OTP 29, we must do a GC test here.
         * If compiled by OTP 30 or later, this instruction is
         * preceded by a `test_heap` instruction that has already
         * ensured sufficient heap space. */
        emit_gc_test(ArgWord(0), ArgWord(num_words_needed), Live);
    }

    a64::Gp def_values = a64::xzr;
    mov_arg(TMP3, Def);
    if (any_literal_defaults) {
        /* At least one default value is a literal. Literals are not
         * yet at their final location. We must emit code that set up
         * a pointer (ARG3) to the beginning of the default values. */
        emit_untag_ptr(TMP3, TMP3);
        a.ldp(TMP2, ARG3, a64::Mem(TMP3));
        def_values = emit_ptr_val(ARG3, ARG3);
        a.add(def_values, def_values, sizeof(Eterm) - TAG_PRIMARY_BOXED);
    } else {
        /* We will not need a pointer to the default values. */
        a64::Gp cons_ptr = emit_ptr_val(TMP3, TMP3);
        a.ldur(TMP2, getCARRef(cons_ptr));
    }

    /* Store header word and pointer to the definition. */
    mov_imm(TMP1, MAKE_RECORD_HEADER(field_count));
    a.stp(TMP1, TMP2, a64::Mem(HTOP).post(sizeof(Eterm[2])));

    /* We'll keep a cache of loaded immediate values. */
    static a64::Gp value_regs[] =
            {ARG4, ARG5, ARG6, ARG7, ARG8, TMP1, TMP2, TMP3, TMP4, TMP5, TMP6};
    ImmedRegCache values(*this,
                         sizeof(value_regs) / sizeof(a64::Gp),
                         value_regs);

    argp = 0;
    for (int i = 0; i < field_count; i += 2) {
        if ((i % 128) == 0) {
            check_pending_stubs();
        }

        regs[0] = a64::xzr;
        regs[1] = a64::xzr;

        /* We will always get the values for two consecutive fields. */
        if (argp + 2 < args.size() &&
            args[argp].as<ArgAtom>().get() == defp->keys[i] &&
            args[argp + 2].as<ArgAtom>().get() == defp->keys[i + 1] &&
            args[argp + 1].isRegister() && args[argp + 3].isRegister()) {
            /* Load the values for two fields at once from two BEAM
             * registers. */
            auto [r0, r1] =
                    load_sources(args[argp + 1], ARG1, args[argp + 3], ARG2);
            regs[0] = r0;
            regs[1] = r1;
            argp += 4;
        } else {
            int limit = i + 1 < field_count ? 2 : 1;

            /* In this inner loop, grab the values one at a time. */
            for (int j = 0; j < limit; j++) {
                static a64::Gp def_regs[] = {ARG1, ARG2};
                auto default_reg = def_regs[j];
                if (argp < args.size() &&
                    args[argp].as<ArgAtom>().get() == defp->keys[i + j]) {
                    if (args[argp + 1].isImmed()) {
                        Eterm value = args[argp + 1].as<ArgImmed>().get();
                        regs[j] = values.load_value(value);
                    } else {
                        regs[j] = load_source(args[argp + 1], default_reg);
                    }
                    argp += 2;
                } else {
                    Eterm value = loader_def_values[i + j];
                    if (is_immed(value)) {
                        regs[j] = values.load_value(value);
                    } else {
                        ASSERT(any_literal_defaults);
                        regs[j] = default_reg;
                        a.ldr(regs[j].reg,
                              a64::Mem(def_values, (i + j) * sizeof(Eterm)));
                    }
                }
            }
        }

        /* Now store the values we colleced. */
        if (regs[1].reg != a64::xzr) {
            /* Store two values. */
            a.stp(regs[0].reg,
                  regs[1].reg,
                  a64::Mem(HTOP).post(sizeof(Eterm[2])));
        } else {
            /* At the end. There is only one value. */
            a.str(regs[0].reg, a64::Mem(HTOP).post(sizeof(Eterm)));
        }
    }

    auto ptr = init_destination(Dst, TMP1);
    sub(ptr.reg, HTOP, num_words_needed * sizeof(Eterm) - TAG_PRIMARY_BOXED);
    flush_var(ptr);
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
