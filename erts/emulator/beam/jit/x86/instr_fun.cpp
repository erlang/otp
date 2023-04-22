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
 * ARG3 = arity
 * ARG4 = fun thing
 * ARG5 = current PC */
void BeamGlobalAssembler::emit_unloaded_fun() {
    Label error = a.newLabel();

    emit_enter_frame();

    a.mov(TMP_MEM1q, ARG5);

    emit_enter_runtime<Update::eHeapAlloc | Update::eReductions>();

    a.mov(ARG1, c_p);
    load_x_reg_array(ARG2);
    /* ARG3 and ARG4 have already been set. */
    runtime_call<4>(beam_jit_handle_unloaded_fun);

    emit_leave_runtime<Update::eHeapAlloc | Update::eReductions |
                       Update::eCodeIndex>();

    a.test(RET, RET);
    a.jz(error);

    emit_leave_frame();
    a.jmp(emit_setup_dispatchable_call(RET));

    a.bind(error);
    {
        /* The `raise_exception` fragment expects that the PC is on the
         * stack. */
        a.push(TMP_MEM1q);
        mov_imm(ARG4, nullptr);
        a.jmp(labels[raise_exception]);
    }
}

/* Handles errors for `call_fun`.
 *
 * ARG3 = arity
 * ARG4 = fun thing
 * ARG5 = current PC */
void BeamGlobalAssembler::emit_handle_call_fun_error() {
    Label bad_arity = a.newLabel(), bad_fun = a.newLabel();

    emit_enter_frame();

    emit_is_boxed(bad_fun, ARG4);

    x86::Gp fun_thing = emit_ptr_val(RET, ARG4);
    a.cmp(emit_boxed_val(fun_thing), imm(HEADER_FUN));
    a.short_().je(bad_arity);

    a.bind(bad_fun);
    {
        a.mov(x86::qword_ptr(c_p, offsetof(Process, freason)), imm(EXC_BADFUN));
        a.mov(x86::qword_ptr(c_p, offsetof(Process, fvalue)), ARG4);

        /* The `raise_exception` fragment expects that the PC is on the
         * stack. */
        a.push(ARG5);
        mov_imm(ARG4, nullptr);
        a.jmp(labels[raise_exception]);
    }

    a.bind(bad_arity);
    {
        /* Stash our fun and current PC. Note that we don't move the fun to
         * {x,0} straight away as that would clobber the first argument. */
        a.mov(TMP_MEM1q, ARG4);
        a.mov(TMP_MEM2q, ARG5);

        emit_enter_runtime<Update::eHeapAlloc>();

        a.mov(ARG1, c_p);
        load_x_reg_array(ARG2);
        /* ARG3 is already set. */
        runtime_call<3>(beam_jit_build_argument_list);

        emit_leave_runtime<Update::eHeapAlloc>();

        a.mov(ARG1, TMP_MEM1q);
        a.mov(getXRef(0), ARG1);
        a.mov(getXRef(1), RET);

        /* Create the {Fun, Args} tuple. */
        {
            const int32_t bytes_needed = (3 + S_RESERVED) * sizeof(Eterm);
            Label after_gc = a.newLabel();

            a.lea(ARG3, x86::qword_ptr(HTOP, bytes_needed));
            a.cmp(ARG3, E);
            a.short_().jbe(after_gc);
            {
                mov_imm(ARG4, 2);
                aligned_call(labels[garbage_collect]);
            }
            a.bind(after_gc);

            a.mov(ARG1, getXRef(0));
            a.mov(ARG2, getXRef(1));

            a.mov(x86::qword_ptr(HTOP), imm(make_arityval(2)));
            a.mov(x86::qword_ptr(HTOP, sizeof(Eterm[1])), ARG1);
            a.mov(x86::qword_ptr(HTOP, sizeof(Eterm[2])), ARG2);

            a.lea(ARG1, x86::qword_ptr(HTOP, TAG_PRIMARY_BOXED));
            a.add(HTOP, imm(sizeof(Eterm[3])));
        }

        a.mov(x86::qword_ptr(c_p, offsetof(Process, freason)),
              imm(EXC_BADARITY));
        a.mov(x86::qword_ptr(c_p, offsetof(Process, fvalue)), ARG1);

        /* The `raise_exception` fragment expects that the PC is on the
         * stack. */
        a.push(TMP_MEM2q);
        mov_imm(ARG4, nullptr);
        a.jmp(labels[raise_exception]);
    }
}

/* Handles save_calls for local funs, which is a side-effect of our calling
 * convention. Fun entry is in RET.
 *
 * When the active code index is ERTS_SAVE_CALLS_CODE_IX, all local fun calls
 * will land here. */
void BeamGlobalAssembler::emit_dispatch_save_calls_fun() {
    /* Keep going with the actual code index. */
    a.mov(ARG1, imm(&the_active_code_index));
    a.mov(ARG1d, x86::dword_ptr(ARG1));

    a.jmp(emit_setup_dispatchable_call(RET, ARG1));
}

/* `call_fun` instructions land here to set up their environment before jumping
 * to the actual implementation.
 *
 * Keep in mind that this runs in the limbo between caller and callee, so we
 * must not enter a frame here.
 *
 * ARG4 = fun thing */
void BeamModuleAssembler::emit_i_lambda_trampoline(const ArgLambda &Lambda,
                                                   const ArgLabel &Lbl,
                                                   const ArgWord &Arity,
                                                   const ArgWord &NumFree) {
    const ssize_t effective_arity = Arity.get() - NumFree.get();
    const ssize_t num_free = NumFree.get();

    const auto &lambda = lambdas[Lambda.get()];
    a.bind(lambda.trampoline);

    emit_ptr_val(ARG4, ARG4);

    ASSERT(num_free > 0);
    emit_copy_words(emit_boxed_val(ARG4, offsetof(ErlFunThing, env)),
                    getXRef(effective_arity),
                    num_free,
                    RET);

    a.jmp(resolve_beam_label(Lbl));
}

void BeamModuleAssembler::emit_i_make_fun3(const ArgLambda &Lambda,
                                           const ArgRegister &Dst,
                                           const ArgWord &Arity,
                                           const ArgWord &NumFree,
                                           const Span<ArgVal> &env) {
    size_t num_free = env.size();

    ASSERT(NumFree.get() == num_free);

    mov_arg(ARG2, Lambda);
    mov_arg(ARG3, Arity);
    mov_arg(ARG4, NumFree);

    emit_enter_runtime<Update::eHeapOnlyAlloc>();

    a.mov(ARG1, c_p);
    runtime_call<4>(erts_new_local_fun_thing);

    emit_leave_runtime<Update::eHeapOnlyAlloc>();

    comment("Move fun environment");
    for (unsigned i = 0; i < num_free; i++) {
        mov_arg(x86::qword_ptr(RET,
                               offsetof(ErlFunThing, env) + i * sizeof(Eterm)),
                env[i]);
    }

    comment("Create boxed ptr");
    a.or_(RETb, TAG_PRIMARY_BOXED);
    mov_arg(Dst, RET);
}

/* Unwraps `apply_fun` so we can share the rest of the implementation with
 * `call_fun`. */
void BeamGlobalAssembler::emit_apply_fun_shared() {
    Label finished = a.newLabel();

    emit_enter_frame();

    /* Put the arity and fun into the right registers for `call_fun`, and stash
     * the argument list in ARG5 for the error path. We'll bump the arity as
     * we go through the argument list. */
    mov_imm(ARG3, 0);
    a.mov(ARG4, getXRef(0));
    a.mov(ARG5, getXRef(1));

    {
        Label unpack_next = a.newLabel(), malformed_list = a.newLabel(),
              raise_error = a.newLabel();

        auto x_register = getXRef(0);

        ASSERT(x_register.shift() == 0);
        x_register.setIndex(ARG3);
        x_register.setShift(3);

        a.mov(ARG1, ARG5);
        a.bind(unpack_next);
        {
            a.cmp(ARG1d, imm(NIL));
            a.short_().je(finished);

            a.test(ARG1.r8(), imm(_TAG_PRIMARY_MASK - TAG_PRIMARY_LIST));
            a.short_().jne(malformed_list);

            emit_ptr_val(ARG1, ARG1);
            a.mov(RET, getCARRef(ARG1));
            a.mov(ARG1, getCDRRef(ARG1));
            a.mov(x_register, RET);

            a.inc(ARG3);

            /* We bail at MAX_REG-1 rather than MAX_REG as the highest register
             * is reserved for the loader. */
            a.cmp(ARG3, imm(MAX_REG - 1));
            a.jb(unpack_next);
        }

        a.mov(RET, imm(SYSTEM_LIMIT));
        a.jmp(raise_error);

        a.bind(malformed_list);
        a.mov(RET, imm(BADARG));

        a.bind(raise_error);
        {
            static const ErtsCodeMFA apply_mfa = {am_erlang, am_apply, 2};

            a.mov(getXRef(0), ARG4);
            a.mov(getXRef(1), ARG5);

            a.mov(x86::qword_ptr(c_p, offsetof(Process, freason)), RET);
            mov_imm(ARG4, &apply_mfa);

            emit_leave_frame();
            a.jmp(labels[raise_exception]);
        }
    }

    a.bind(finished);

    emit_leave_frame();
    a.ret();
}

void BeamModuleAssembler::emit_i_apply_fun() {
    safe_fragment_call(ga->get_apply_fun_shared());

    x86::Gp target = emit_call_fun();
    ASSERT(target != ARG6);
    erlang_call(target, ARG6);
}

void BeamModuleAssembler::emit_i_apply_fun_last(const ArgWord &Deallocate) {
    emit_deallocate(Deallocate);
    emit_i_apply_fun_only();
}

void BeamModuleAssembler::emit_i_apply_fun_only() {
    safe_fragment_call(ga->get_apply_fun_shared());

    x86::Gp target = emit_call_fun();
    emit_leave_frame();
    a.jmp(target);
}

/* Assumes that:
 *   ARG3 = arity
 *   ARG4 = fun thing */
x86::Gp BeamModuleAssembler::emit_call_fun(bool skip_box_test,
                                           bool skip_fun_test,
                                           bool skip_arity_test) {
    const bool never_fails = skip_box_test && skip_fun_test && skip_arity_test;
    Label next = a.newLabel();

    /* Speculatively strip the literal tag when needed. */
    x86::Gp fun_thing = emit_ptr_val(RET, ARG4);

    if (!never_fails) {
        /* Load the error fragment into ARG2 so we can CMOV ourselves there on
         * error. */
        a.mov(ARG2, ga->get_handle_call_fun_error());
    }

    /* The `handle_call_fun_error` and `unloaded_fun` fragments expect current
     * PC in ARG5. */
    a.lea(ARG5, x86::qword_ptr(next));

    if (!skip_box_test) {
        /* As emit_is_boxed(), but explicitly sets ZF so we can rely on that
         * for error checking in `next`. */
        a.test(ARG4d, imm(_TAG_PRIMARY_MASK - TAG_PRIMARY_BOXED));
        a.short_().jne(next);
    } else {
        comment("skipped box test since source is always boxed");
    }

    if (skip_fun_test) {
        comment("skipped fun test since source is always a fun when boxed");
    } else {
        a.cmp(emit_boxed_val(fun_thing), imm(HEADER_FUN));
        a.short_().jne(next);
    }

    if (skip_arity_test) {
        comment("skipped arity test since source always has right arity");
    } else {
        a.cmp(emit_boxed_val(fun_thing,
                             offsetof(ErlFunThing, arity),
                             sizeof(byte)),
              ARG3.r8());
    }

    a.mov(RET, emit_boxed_val(fun_thing, offsetof(ErlFunThing, entry)));
    a.mov(ARG1, emit_setup_dispatchable_call(RET));

    a.bind(next);

    if (!never_fails) {
        /* Assumes that ZF is set on success and clear on error, overwriting
         * our destination with the error fragment's address. */
        a.cmovne(ARG1, ARG2);
    }

    return ARG1;
}

void BeamModuleAssembler::emit_i_call_fun2(const ArgVal &Tag,
                                           const ArgWord &Arity,
                                           const ArgRegister &Func) {
    mov_arg(ARG4, Func);

    if (Tag.isImmed()) {
        mov_imm(ARG3, Arity.get());

        auto target = emit_call_fun(
                always_one_of<BeamTypeId::AlwaysBoxed>(Func),
                masked_types<BeamTypeId::MaybeBoxed>(Func) == BeamTypeId::Fun,
                Tag.as<ArgImmed>().get() == am_safe);

        erlang_call(target, ARG6);
    } else {
        const auto &trampoline = lambdas[Tag.as<ArgLambda>().get()].trampoline;
        erlang_call(trampoline, RET);
    }
}

void BeamModuleAssembler::emit_i_call_fun2_last(const ArgVal &Tag,
                                                const ArgWord &Arity,
                                                const ArgRegister &Func,
                                                const ArgWord &Deallocate) {
    mov_arg(ARG4, Func);

    if (Tag.isImmed()) {
        mov_imm(ARG3, Arity.get());

        auto target = emit_call_fun(
                always_one_of<BeamTypeId::AlwaysBoxed>(Func),
                masked_types<BeamTypeId::MaybeBoxed>(Func) == BeamTypeId::Fun,
                Tag.as<ArgImmed>().get() == am_safe);

        emit_deallocate(Deallocate);
        emit_leave_frame();

        a.jmp(target);
    } else {
        const auto &trampoline = lambdas[Tag.as<ArgLambda>().get()].trampoline;

        emit_deallocate(Deallocate);
        emit_leave_frame();

        a.jmp(trampoline);
    }
}

void BeamModuleAssembler::emit_i_call_fun(const ArgWord &Arity) {
    const ArgXRegister Func(Arity.get());
    const ArgAtom Tag(am_unsafe);

    emit_i_call_fun2(Tag, Arity, Func);
}

void BeamModuleAssembler::emit_i_call_fun_last(const ArgWord &Arity,
                                               const ArgWord &Deallocate) {
    const ArgXRegister Func(Arity.get());
    const ArgAtom Tag(am_unsafe);

    emit_i_call_fun2_last(Tag, Arity, Func, Deallocate);
}

/* Psuedo-instruction for signalling lambda load errors. Never actually runs. */
void BeamModuleAssembler::emit_i_lambda_error(const ArgWord &Dummy) {
    comment("lambda error");
    a.ud2();
}
