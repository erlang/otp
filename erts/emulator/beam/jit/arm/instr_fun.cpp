/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2021-2021. All Rights Reserved.
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

/* Calls to functions that are being purged (but haven't finished) land here.
 *
 * Keep in mind that this runs in the limbo between caller and callee. It must
 * not clobber LR (x30).
 *
 * ARG3 = arity
 * ARG4 = fun thing
 * ARG5 = address of the call_fun instruction that got us here. Note that we
 *        can't use LR (x30) for this because tail calls point elsewhere. */
void BeamGlobalAssembler::emit_unloaded_fun() {
    Label error = a.newLabel();

    a.str(ARG5, TMP_MEM1q);

    emit_enter_runtime_frame();
    emit_enter_runtime<Update::eHeap | Update::eStack | Update::eXRegs |
                       Update::eReductions>();

    a.mov(ARG1, c_p);
    load_x_reg_array(ARG2);
    /* ARG3 and ARG4 have already been set. */
    runtime_call<4>(beam_jit_handle_unloaded_fun);

    emit_leave_runtime<Update::eHeap | Update::eStack | Update::eXRegs |
                       Update::eReductions | Update::eCodeIndex>();
    emit_leave_runtime_frame();

    a.cbz(ARG1, error);

    a.ldr(TMP1, emit_setup_dispatchable_call(ARG1));
    a.br(TMP1);

    a.bind(error);
    {
        a.ldr(ARG2, TMP_MEM1q);
        mov_imm(ARG4, nullptr);
        a.b(labels[raise_exception_shared]);
    }
}

/* Handles errors for `call_fun`. Assumes that we're running on the Erlang
 * stack with a valid stack frame.
 *
 * ARG3 = arity
 * ARG4 = fun thing
 * ARG5 = address of the call_fun instruction that got us here. Note that we
 *        can't use LR (x30) for this because tail calls point elsewhere. */
void BeamGlobalAssembler::emit_handle_call_fun_error() {
    Label bad_arity = a.newLabel(), bad_fun = a.newLabel();

    emit_is_boxed(bad_fun, ARG4);

    arm::Gp fun_thing = emit_ptr_val(TMP1, ARG4);
    a.ldur(TMP1, emit_boxed_val(fun_thing));
    a.cmp(TMP1, imm(HEADER_FUN));
    a.cond_eq().b(bad_arity);

    a.bind(bad_fun);
    {
        mov_imm(TMP1, EXC_BADFUN);
        a.str(TMP1, arm::Mem(c_p, offsetof(Process, freason)));
        a.str(ARG4, arm::Mem(c_p, offsetof(Process, fvalue)));

        a.mov(ARG2, ARG5);
        mov_imm(ARG4, nullptr);
        a.b(labels[raise_exception_shared]);
    }

    a.bind(bad_arity);
    {
        a.stp(ARG4, ARG5, TMP_MEM1q);

        emit_enter_runtime<Update::eHeap | Update::eStack | Update::eXRegs>();

        a.mov(ARG1, c_p);
        load_x_reg_array(ARG2);
        /* ARG3 is already set */
        runtime_call<3>(beam_jit_build_argument_list);

        emit_leave_runtime<Update::eHeap | Update::eStack | Update::eXRegs>();

        a.ldr(XREG0, TMP_MEM1q);
        a.mov(XREG1, ARG1);

        /* Create the {Fun, Args} tuple. */
        {
            const int32_t bytes_needed = (3 + S_RESERVED) * sizeof(Eterm);
            Label after_gc = a.newLabel();

            add(ARG3, HTOP, bytes_needed);
            a.cmp(ARG3, E);
            a.cond_ls().b(after_gc);
            {
                mov_imm(ARG4, 2);
                a.bl(labels[garbage_collect]);
            }
            a.bind(after_gc);

            a.add(ARG1, HTOP, imm(TAG_PRIMARY_BOXED));

            mov_imm(TMP1, make_arityval(2));
            a.str(TMP1, arm::Mem(HTOP).post(sizeof(Eterm)));
            a.stp(XREG0, XREG1, arm::Mem(HTOP).post(sizeof(Eterm[2])));
        }

        a.mov(TMP1, imm(EXC_BADARITY));
        a.str(TMP1, arm::Mem(c_p, offsetof(Process, freason)));
        a.str(ARG1, arm::Mem(c_p, offsetof(Process, fvalue)));

        a.ldr(ARG2, TMP_MEM2q);
        mov_imm(ARG4, nullptr);
        a.b(labels[raise_exception_shared]);
    }
}

/* `call_fun` instructions land here to set up their environment before jumping
 * to the actual implementation.
 *
 * Keep in mind that this runs in the limbo between caller and callee. It must
 * not clobber LR (x30).
 *
 * ARG3 = arity
 * ARG4 = fun thing
 * ARG5 = current PC */
void BeamModuleAssembler::emit_i_lambda_trampoline(const ArgVal &Index,
                                                   const ArgVal &Lbl,
                                                   const ArgVal &Arity,
                                                   const ArgVal &NumFree) {
    const ssize_t env_offset = offsetof(ErlFunThing, env) - TAG_PRIMARY_BOXED;
    const ssize_t fun_arity = Arity.getValue() - NumFree.getValue();
    const ssize_t total_arity = Arity.getValue();

    auto &lambda = lambdas[Index.getValue()];

    if (NumFree.getValue() == 0) {
        /* No free variables, let the lambda jump directly to our target. */
        lambda.trampoline = rawLabels[Lbl.getValue()];
        return;
    }

    lambda.trampoline = a.newLabel();
    a.bind(lambda.trampoline);

    if (NumFree.getValue() == 1) {
        auto first = init_destination(ArgVal(ArgVal::XReg, fun_arity), TMP1);

        /* Don't bother untagging when there's only a single element, it's
         * guaranteed to be within range of LDUR. */
        emit_ptr_val(ARG4, ARG4);
        a.ldur(first.reg, arm::Mem(ARG4, env_offset));
        flush_var(first);
    } else if (NumFree.getValue() >= 2) {
        ssize_t i;

        emit_ptr_val(ARG4, ARG4);
        a.add(ARG4, ARG4, imm(env_offset));

        for (i = fun_arity; i < total_arity - 1; i += 2) {
            auto first = init_destination(ArgVal(ArgVal::XReg, i), TMP1);
            auto second = init_destination(ArgVal(ArgVal::XReg, i + 1), TMP2);

            a.ldp(first.reg, second.reg, arm::Mem(ARG4).post(sizeof(Eterm[2])));
            flush_vars(first, second);
        }

        if (i < total_arity) {
            auto last = init_destination(ArgVal(ArgVal::XReg, i), TMP1);
            a.ldr(last.reg, arm::Mem(ARG4));
            flush_var(last);
        }
    }

    a.b(resolve_beam_label(Lbl, disp128MB));
}

void BeamModuleAssembler::emit_i_make_fun3(const ArgVal &Fun,
                                           const ArgVal &Dst,
                                           const ArgVal &Arity,
                                           const ArgVal &NumFree,
                                           const Span<ArgVal> &env) {
    const ssize_t num_free = NumFree.getValue();
    ssize_t i;

    ASSERT(num_free == env.size());

    a.mov(ARG1, c_p);
    mov_arg(ARG2, Fun);
    mov_arg(ARG3, Arity);
    mov_arg(ARG4, NumFree);

    emit_enter_runtime<Update::eHeap>();

    runtime_call<4>(erts_new_local_fun_thing);

    emit_leave_runtime<Update::eHeap>();

    if (num_free) {
        comment("Move fun environment");
    }

    for (i = 0; i < num_free - 1; i += 2) {
        ssize_t offset = offsetof(ErlFunThing, env) + i * sizeof(Eterm);

        if ((i % 128) == 0) {
            check_pending_stubs();
        }

        auto [first, second] = load_sources(env[i], TMP1, env[i + 1], TMP2);
        safe_stp(first.reg, second.reg, arm::Mem(ARG1, offset));
    }

    if (i < num_free) {
        ssize_t offset = offsetof(ErlFunThing, env) + i * sizeof(Eterm);
        mov_arg(arm::Mem(ARG1, offset), env[i]);
    }

    comment("Create boxed ptr");
    auto dst = init_destination(Dst, TMP1);
    a.orr(dst.reg, ARG1, imm(TAG_PRIMARY_BOXED));
    flush_var(dst);
}

void BeamGlobalAssembler::emit_apply_fun_shared() {
    Label finished = a.newLabel();

    /* Put the arity and fun into the right registers for `call_fun`, and stash
     * the argument list in ARG5 for the error path. We'll bump the arity as
     * we go through the argument list. */
    mov_imm(ARG3, 0);
    a.mov(ARG4, XREG0);
    a.mov(ARG5, XREG1);

    {
        Label unpack_next = a.newLabel(), malformed_list = a.newLabel(),
              raise_error = a.newLabel();

        /* apply/2 is rarely used on a hot code path, so we'll simplify things
         * by switching to the runtime environment where we can operate
         * entirely on the X register array.
         *
         * Note that we don't have any live registers at this point. */
        emit_enter_runtime<Update::eXRegs>(0);

        a.mov(TMP1, ARG5);
        lea(TMP2, getXRef(0));

        a.bind(unpack_next);
        {
            a.cmp(TMP1, imm(NIL));
            a.cond_eq().b(finished);

            ERTS_CT_ASSERT(_TAG_PRIMARY_MASK - TAG_PRIMARY_LIST == (1 << 1));
            a.tbnz(TMP1, imm(1), malformed_list);

            emit_ptr_val(TMP1, TMP1);
            a.sub(TMP1, TMP1, imm(TAG_PRIMARY_LIST));
            a.ldp(TMP3, TMP1, arm::Mem(TMP1));
            a.str(TMP3, arm::Mem(TMP2).post(sizeof(Eterm)));

            /* We bail at MAX_REG-1 rather than MAX_REG as the highest register
             * is reserved for the loader. */
            a.add(ARG3, ARG3, imm(1));
            a.cmp(ARG3, imm(MAX_REG - 1));
            a.cond_lo().b(unpack_next);
        }

        a.mov(TMP1, imm(SYSTEM_LIMIT));
        a.b(raise_error);

        a.bind(malformed_list);
        a.mov(TMP1, imm(BADARG));

        a.bind(raise_error);
        {
            static const ErtsCodeMFA apply_mfa = {am_erlang, am_apply, 2};

            emit_leave_runtime<Update::eXRegs>(0);

            a.mov(XREG0, ARG4);
            a.mov(XREG1, ARG5);

            a.str(TMP1, arm::Mem(c_p, offsetof(Process, freason)));
            mov_imm(ARG4, &apply_mfa);
            a.b(labels[raise_exception]);
        }
    }

    a.bind(finished);
    {
        emit_leave_runtime<Update::eXRegs>();
        a.ret(a64::x30);
    }
}

void BeamModuleAssembler::emit_i_apply_fun() {
    fragment_call(ga->get_apply_fun_shared());
    erlang_call(emit_call_fun());
}

void BeamModuleAssembler::emit_i_apply_fun_last(const ArgVal &Deallocate) {
    emit_deallocate(Deallocate);
    emit_i_apply_fun_only();
}

void BeamModuleAssembler::emit_i_apply_fun_only() {
    fragment_call(ga->get_apply_fun_shared());
    emit_leave_erlang_frame();
    a.br(emit_call_fun());
}

/* Assumes that:
 *   ARG3 = arity
 *   ARG4 = fun thing */
arm::Gp BeamModuleAssembler::emit_call_fun(bool skip_box_test,
                                           bool skip_fun_test,
                                           bool skip_arity_test) {
    const bool never_fails = skip_box_test && skip_fun_test && skip_arity_test;
    Label next = a.newLabel();

    /* Speculatively untag the ErlFunThing. */
    emit_untag_ptr(TMP2, ARG4);

    if (!never_fails) {
        /* Load the error fragment into TMP3 so we can CSEL ourselves there on
         * error. */
        a.adr(TMP3, resolve_fragment(ga->get_handle_call_fun_error(), disp1MB));
    }

    /* The `handle_call_fun_error` and `unloaded_fun` fragments expect current
     * PC in ARG5. */
    a.adr(ARG5, next);

    if (!skip_box_test) {
        /* As emit_is_boxed(), but explicitly sets ZF so we can rely on that
         * for error checking in `next`. */
        a.tst(ARG4, imm(_TAG_PRIMARY_MASK - TAG_PRIMARY_BOXED));
        a.cond_ne().b(next);
    } else {
        comment("skipped box test since source is always boxed");
    }

    if (!skip_fun_test) {
        /* Load header word and `ErlFunThing->entry`. We can safely do this
         * before testing the header because boxed terms are guaranteed to be
         * at least two words long. */
        ERTS_CT_ASSERT_FIELD_PAIR(ErlFunThing, thing_word, entry);
        a.ldp(TMP1, ARG1, arm::Mem(TMP2));

        a.cmp(TMP1, imm(HEADER_FUN));
        a.cond_ne().b(next);
    } else {
        comment("skipped fun test since source is always a fun when boxed");
        a.ldr(ARG1, arm::Mem(TMP2, offsetof(ErlFunThing, entry)));
    }

    if (!skip_arity_test) {
        a.ldr(TMP2, arm::Mem(TMP2, offsetof(ErlFunThing, arity)));
        a.cmp(TMP2, ARG3);
    } else {
        comment("skipped arity test since source always has right arity");
    }

    a.ldr(TMP1, emit_setup_dispatchable_call(ARG1));

    /* Assumes that ZF is set on success and clear on error, overwriting our
     * destination with the error fragment's address. */
    a.bind(next);

    if (!never_fails) {
        a.csel(TMP1, TMP1, TMP3, imm(arm::Cond::kEQ));
    }

    return TMP1;
}

void BeamModuleAssembler::emit_i_call_fun2(const ArgVal &Safe,
                                           const ArgVal &Arity,
                                           const ArgVal &Func) {
    arm::Gp target;

    mov_imm(ARG3, Arity.getValue());
    mov_arg(ARG4, Func);

    target = emit_call_fun(always_one_of(Func, BEAM_TYPE_MASK_ALWAYS_BOXED),
                           masked_types(Func, BEAM_TYPE_MASK_BOXED) ==
                                   BEAM_TYPE_FUN,
                           Safe.getValue() == am_true);

    erlang_call(target);
}

void BeamModuleAssembler::emit_i_call_fun2_last(const ArgVal &Safe,
                                                const ArgVal &Arity,
                                                const ArgVal &Func,
                                                const ArgVal &Deallocate) {
    arm::Gp target;

    mov_imm(ARG3, Arity.getValue());
    mov_arg(ARG4, Func);

    target = emit_call_fun(always_one_of(Func, BEAM_TYPE_MASK_ALWAYS_BOXED),
                           masked_types(Func, BEAM_TYPE_MASK_BOXED) ==
                                   BEAM_TYPE_FUN,
                           Safe.getValue() == am_true);

    emit_deallocate(Deallocate);
    emit_leave_erlang_frame();
    a.br(target);
}

void BeamModuleAssembler::emit_i_call_fun(const ArgVal &Arity) {
    const ArgVal Func(ArgVal::XReg, Arity.getValue());
    const ArgVal Safe(ArgVal::Immediate, am_false);

    emit_i_call_fun2(Safe, Arity, Func);
}

void BeamModuleAssembler::emit_i_call_fun_last(const ArgVal &Arity,
                                               const ArgVal &Deallocate) {
    const ArgVal Func(ArgVal::XReg, Arity.getValue());
    const ArgVal Safe(ArgVal::Immediate, am_false);

    emit_i_call_fun2_last(Safe, Arity, Func, Deallocate);
}

/* Psuedo-instruction for signalling lambda load errors. Never actually runs. */
void BeamModuleAssembler::emit_i_lambda_error(const ArgVal &Dummy) {
    emit_nyi("emit_i_lambda_error");
}
