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
    ASSERT(false);
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
    ASSERT(false);
}

/* Handles save_calls for local funs, which is a side-effect of our calling
 * convention. Fun entry is in ARG1.
 *
 * When the active code index is ERTS_SAVE_CALLS_CODE_IX, all local fun calls
 * will land here. */
void BeamGlobalAssembler::emit_dispatch_save_calls_fun() {
    // TODO
    ASSERT(false);
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
    // TODO
    emit_nyi("emit_i_lambda_trampoline");
}

void BeamModuleAssembler::emit_i_make_fun3(const ArgLambda &Lambda,
                                           const ArgRegister &Dst,
                                           const ArgWord &Arity,
                                           const ArgWord &NumFree,
                                           const Span<ArgVal> &env) {
    // TODO
    emit_nyi("emit_i_make_fun3");
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
    // TODO
    emit_nyi("emit_call_fun");
    a32::Gp reg;
    return reg;
}

void BeamModuleAssembler::emit_i_call_fun2(const ArgVal &Tag,
                                           const ArgWord &Arity,
                                           const ArgRegister &Func) {
    // TODO
    emit_nyi("emit_i_call_fun2");
}

void BeamModuleAssembler::emit_i_call_fun2_last(const ArgVal &Tag,
                                                const ArgWord &Arity,
                                                const ArgRegister &Func,
                                                const ArgWord &Deallocate) {
    // TODO
    emit_nyi("emit_i_call_fun2_last");
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
