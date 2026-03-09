/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2021-2023. All Rights Reserved.
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
 * ARG3 = lower 16 bits of expected header, containing FUN_SUBTAG and arity
 * ARG4 = fun thing
 * ARG5 = address of the call_fun instruction that got us here. Note that we
 *        can't use LR (x30) for this because tail calls point elsewhere. */
void BeamGlobalAssembler::emit_unloaded_fun() {
    // TODO
    emit_nyi("emit_unloaded_fun");
}

/* Handles errors for `call_fun`. Assumes that we're running on the Erlang
 * stack with a valid stack frame.
 *
 * ARG3 = lower 16 bits of expected header, containing FUN_SUBTAG and arity
 * ARG4 = fun thing
 * ARG5 = address of the call_fun instruction that got us here. Note that we
 *        can't use LR (x30) for this because tail calls point elsewhere. */
void BeamGlobalAssembler::emit_handle_call_fun_error() {
    // TODO
    emit_nyi("emit_handle_call_fun_error");
}

/* Handles save_calls for local funs, which is a side-effect of our calling
 * convention. Fun entry is in ARG1.
 *
 * When the active code index is ERTS_SAVE_CALLS_CODE_IX, all local fun calls
 * will land here. */
void BeamGlobalAssembler::emit_dispatch_save_calls_fun() {
    // TODO
    emit_nyi("emit_dispatch_save_calls_fun");
}

/* `call_fun` instructions land here to set up their environment before jumping
 * to the actual implementation.
 *
 * Keep in mind that this runs in the limbo between caller and callee. It must
 * not clobber LR (x30).
 *
 * ARG4 = fun thing */
void BeamModuleAssembler::emit_i_lambda_trampoline(const ArgLambda &Lambda,
                                                   const ArgLabel &Lbl,
                                                   const ArgWord &Arity,
                                                   const ArgWord &NumFree) {
    const ssize_t env_offset = offsetof(ErlFunThing, env) - TAG_PRIMARY_BOXED;
    const ssize_t fun_arity = Arity.get() - NumFree.get();
    const ssize_t total_arity = Arity.get();

    const auto &lambda = lambdas[Lambda.get()];
    a.bind(lambda.trampoline);

    if (NumFree.get() == 1) {
        auto first = init_destination(ArgXRegister(fun_arity), TMP);

        /* Don't bother untagging when there's only a single element, it's
         * guaranteed to be within range of LDR. */
        emit_ptr_val(ARG4, ARG4);
        a.ldr(first.reg, arm::Mem(ARG4, env_offset));
        flush_var(first);
    } else if (NumFree.get() >= 2) {
        ssize_t i;

        emit_ptr_val(ARG4, ARG4);
        a.add(ARG4, ARG4, imm(env_offset));

        for (i = fun_arity; i < total_arity - 1; i += 2) {
            auto first = init_destination(ArgXRegister(i), VAR);
            auto second = init_destination(ArgXRegister(i + 1), TMP);

            a.ldr(first.reg, arm::Mem(ARG4).post(sizeof(Eterm)));
            a.ldr(second.reg, arm::Mem(ARG4).post(sizeof(Eterm)));
            flush_var(first);
            flush_var(second);
        }

        if (i < total_arity) {
            auto last = init_destination(ArgXRegister(i), TMP);
            a.ldr(last.reg, arm::Mem(ARG4));
            flush_var(last);
        }
    }

    a.b(resolve_beam_label(Lbl, disp32MB));
    mark_unreachable();
}

void BeamModuleAssembler::emit_i_make_fun3(const ArgLambda &Lambda,
                                           const ArgRegister &Dst,
                                           const ArgWord &Arity,
                                           const ArgWord &NumFree,
                                           const Span<ArgVal> &env) {
    ASSERT((NumFree.get() + 1) == env.size() &&
           (NumFree.get() + Arity.get()) < MAX_ARG);

    mov_arg(ARG2, Lambda);

    comment("Create fun thing");
    mov_imm(ARG1, MAKE_FUN_HEADER(Arity.get(), NumFree.get(), 0));
    a.str(ARG1, arm::Mem(HTOP, offsetof(ErlFunThing, thing_word)));
    a.str(ARG2, arm::Mem(HTOP, offsetof(ErlFunThing, entry.fun)));

    comment("Move fun environment");
    add(ARG2, HTOP, offsetof(ErlFunThing, env));
    for (Uint i = 0; i < env.size(); i++) {
        if ((i % 128) == 0) {
            check_pending_stubs();
        }

        mov_arg(ARG1, env[i]);
        a.str(ARG1, arm::Mem(ARG2).post(sizeof(Eterm)));
    }

    comment("Create boxed ptr");
    auto dst = init_destination(Dst, VAR);
    a.orr(dst.reg, HTOP, imm(TAG_PRIMARY_BOXED));
    add(HTOP, HTOP, (ERL_FUN_SIZE + env.size()) * sizeof(Eterm));
    flush_var(dst);
}

void BeamGlobalAssembler::emit_apply_fun_shared() {
    Label finished = a.newLabel();

    /* Put the arity and fun into the right registers for `call_fun`, and stash
     * the argument list in ARG2 for the error path. We'll bump the arity as
     * we go through the argument list. */
    mov_imm(ARG3, 0);
    a.ldr(ARG4, getXRef(0));
    a.ldr(ARG2, getXRef(1));
    {
        Label unpack_next = a.newLabel(), malformed_list = a.newLabel(),
              raise_error = a.newLabel();

        auto x_register = arm::Mem(scheduler_registers);

        ASSERT(x_register.shift() == 0);
        x_register.setIndex(ARG3);
        x_register.setShift(2);

        a.mov(ARG1, ARG2);
        a.bind(unpack_next);
        {
            a.cmp(ARG1, imm(NIL));
            a.b_eq(finished);

            ERTS_CT_ASSERT(_TAG_PRIMARY_MASK - TAG_PRIMARY_LIST == (1 << 1));
            a.tst(ARG1, imm(1));
            a.b_ne(malformed_list);

            emit_ptr_val(ARG1, ARG1);
            a.ldr(TMP, getCARRef(ARG1));
            a.ldr(ARG1, getCDRRef(ARG1));
            a.str(TMP, x_register);

            /* We bail at MAX_REG-1 rather than MAX_REG as the highest register
             * is reserved for the loader. */
            mov_imm(TMP, MAX_REG - 1);
            a.add(ARG3, ARG3, imm(1));
            a.cmp(ARG3, TMP);
            a.b_lo(unpack_next);
        }

        mov_imm(ARG1, SYSTEM_LIMIT);
        a.b(raise_error);

        a.bind(malformed_list);
        mov_imm(ARG1, BADARG);

        a.bind(raise_error);
        {
            static const ErtsCodeMFA apply_mfa = {am_erlang, am_apply, 2};

            a.str(ARG4, getXRef(0));
            a.str(ARG2, getXRef(1));

            a.str(ARG1, arm::Mem(c_p, offsetof(Process, freason)));
            mov_imm(ARG4, &apply_mfa);
            a.b(labels[raise_exception]);
        }
    }

    a.bind(finished);
    {
        /* Make the lower 16 bits of ARG3 equal those of the header word of all
         * funs with the same arity. */
        a.lsl(ARG3, ARG3, imm(FUN_HEADER_ARITY_OFFS));
        a.add(ARG3, ARG3, imm(FUN_SUBTAG));

       a.bx(a32::lr);
    }
}

void BeamModuleAssembler::emit_i_apply_fun() {
    // TODO
    emit_nyi("emit_i_apply_fun");
}

void BeamModuleAssembler::emit_i_apply_fun_last(const ArgWord &Deallocate) {
    // TODO
    emit_nyi("emit_i_apply_fun_last");
}

void BeamModuleAssembler::emit_i_apply_fun_only() {
    // TODO
    emit_nyi("emit_i_apply_fun_only");
}

/* Assumes that:
 *   ARG3 = lower 16 bits of expected header, containing FUN_SUBTAG and arity
 *   ARG4 = fun thing */
a32::Gp BeamModuleAssembler::emit_call_fun(bool skip_box_test,
                                           bool skip_header_test) {
    const bool can_fail = !(skip_box_test && skip_header_test);
    Label next = a.newLabel();

    /* Speculatively untag the ErlFunThing. */
    emit_untag_ptr(TMP, ARG4);

    if (can_fail) {
        /* Load the error fragment so we can land there on any failure. */
        a.adr(ARG1,
              resolve_fragment(ga->get_handle_call_fun_error(), disp32MB));
    }

    /* Error fragments expect current PC in ARG5. */
    a.adr(VAR, next);
    // Maybe allocate it on the stack ?
    //a.sub(a32::sp, a32::sp, imm(8));
    //a.str(VAR, arm::Mem(a32::sp));

    if (skip_box_test) {
        comment("skipped box test since source is always boxed");
    } else {
        /* As emit_is_boxed(), but explicitly sets flags so we can rely on them
         * for error checking at `next`. */
        a.tst(ARG4, imm(_TAG_PRIMARY_MASK - TAG_PRIMARY_BOXED));
        a.b_ne(next);
    }

    if (skip_header_test) {
        comment("skipped fun/arity test since source is always a fun of the "
                "right arity when boxed");
        a.ldr(ARG1, arm::Mem(TMP, offsetof(ErlFunThing, entry)));
    } else {
        /* Load header and entry, then compare low 16 bits of header with ARG3
         * (FUN_SUBTAG + arity). */
        a.ldr(ARG2, arm::Mem(TMP, offsetof(ErlFunThing, thing_word)));
        a.ldr(ARG1, arm::Mem(TMP, offsetof(ErlFunThing, entry)));
        a.lsl(ARG2, ARG2, imm(16));
        a.lsr(ARG2, ARG2, imm(16));
        a.cmp(ARG3, ARG2);
        a.b_ne(next);
    }

    a.ldr(ARG1, emit_setup_dispatchable_call(ARG1));

    a.bind(next);
    return ARG1;
}

void BeamModuleAssembler::emit_i_call_fun2(const ArgVal &Tag,
                                           const ArgWord &Arity,
                                           const ArgRegister &Func) {
    mov_arg(ARG4, Func);

    if (Tag.isAtom()) {
        /* Make the lower 16 bits of ARG3 equal those of the header word of all
         * funs with the same arity. */
        mov_imm(ARG3, MAKE_FUN_HEADER(Arity.get(), 0, 0) & 0xFFFF);

        ASSERT(Tag.as<ArgImmed>().get() != am_safe || beam->types.fallback ||
               exact_type<BeamTypeId::Fun>(Func));
        auto target =
                emit_call_fun(always_one_of<BeamTypeId::AlwaysBoxed>(Func),
                              Tag.as<ArgAtom>().get() == am_safe);

        erlang_call(target);
    } else {
        const auto &trampoline = lambdas[Tag.as<ArgLambda>().get()].trampoline;
        erlang_call(resolve_label(trampoline, disp32MB));
    }
}

void BeamModuleAssembler::emit_i_call_fun2_last(const ArgVal &Tag,
                                                const ArgWord &Arity,
                                                const ArgRegister &Func,
                                                const ArgWord &Deallocate) {
    mov_arg(ARG4, Func);

    if (Tag.isAtom()) {
        /* Make the lower 16 bits of ARG3 equal those of the header word of all
         * funs with the same arity. */
        mov_imm(ARG3, MAKE_FUN_HEADER(Arity.get(), 0, 0) & 0xFFFF);

        ASSERT(Tag.as<ArgImmed>().get() != am_safe || beam->types.fallback ||
               exact_type<BeamTypeId::Fun>(Func));
        auto target =
                emit_call_fun(always_one_of<BeamTypeId::AlwaysBoxed>(Func),
                              Tag.as<ArgAtom>().get() == am_safe);

        emit_deallocate(Deallocate);
        emit_leave_erlang_frame();

        a.bx(target);
        mark_unreachable();
    } else {
        emit_deallocate(Deallocate);
        emit_leave_erlang_frame();

        const auto &trampoline = lambdas[Tag.as<ArgLambda>().get()].trampoline;
        a.b(resolve_label(trampoline, disp32MB));
        mark_unreachable();
    }
}

void BeamModuleAssembler::emit_i_call_fun(const ArgWord &Arity) {
    // TODO
    emit_nyi("emit_i_call_fun");
}

void BeamModuleAssembler::emit_i_call_fun_last(const ArgWord &Arity,
                                               const ArgWord &Deallocate) {
    // TODO
    emit_nyi("emit_i_call_fun_last");
}

/* Psuedo-instruction for signalling lambda load errors. Never actually runs. */
void BeamModuleAssembler::emit_i_lambda_error(const ArgWord &Dummy) {
    // TODO
    emit_nyi("emit_i_lambda_error");
}
