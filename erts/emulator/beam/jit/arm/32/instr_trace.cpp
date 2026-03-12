/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2020-2023. All Rights Reserved.
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
#include "beam_common.h"
#include "erl_bif_table.h"
#include "beam_bp.h"
};

/* This function is jumped to from the export entry of a function.
 *
 * ARG1 = export entry */
void BeamGlobalAssembler::emit_generic_bp_global() {
    /* Enter an Erlang frame to make the stack consistent with local
     * breakpoints. */
    emit_enter_erlang_frame();

    lea(ARG2, arm::Mem(ARG1, offsetof(Export, info)));

    emit_enter_runtime<Update::eHeapAlloc | Update::eReductions>();

    a.mov(ARG1, c_p);
    /* ARG2 is already set above. */
    load_x_reg_array(ARG3);
    runtime_call<3>(erts_generic_breakpoint);

    emit_leave_runtime<Update::eHeapAlloc | Update::eReductions>();

    /* This is technically a tail call so we must leave the current frame
     * before jumping. Note that we might not leave the frame we entered
     * earlier in this function, but one added by erts_generic_breakpoint. */
    emit_leave_erlang_frame();
    a.bx(ARG1);
}

/* This function is called from the module header, which is in turn called from
 * the prologue of the traced function. As such, the real return address is at
 * SP+8 rather than LR (x30).
 *
 * See beam_asm.h for more details */
void BeamGlobalAssembler::emit_generic_bp_local() {
    /* For ARM32 runtime frames we push {fp, lr}; caller LR is at SP+4. */
    a.ldr(ARG2, arm::Mem(a32::sp, 4));

    /* Stash return address for later use in `debug_bp`. */
    a.str(ARG2, TMP_MEM1q);

    /* Our actual return address is valid (and word-aligned), but it points
     * just after the trampoline word so we'll need to skip that to find our
     * ErtsCodeInfo. */
    a.sub(ARG2, ARG2, imm(BEAM_ASM_FUNC_PROLOGUE_SIZE + sizeof(ErtsCodeInfo)));

    emit_enter_runtime_frame();
    emit_enter_runtime<Update::eHeapAlloc | Update::eReductions>();

    a.mov(ARG1, c_p);
    /* ARG2 is already set above. */
    load_x_reg_array(ARG3);
    runtime_call<3>(erts_generic_breakpoint);

    emit_leave_runtime<Update::eHeapAlloc | Update::eReductions>();

    mov_imm(TMP, BeamOpCodeAddr(op_i_debug_breakpoint));
    a.cmp(ARG1, TMP);
    a.b_eq(labels[debug_bp]);

    emit_leave_runtime_frame();
    a.bx(a32::lr);
}

/* This function is called from the module header which is called from the
 * prologue of the function to trace. See beam_asm.h for more details
 *
 * The only place that we can come to here is from generic_bp_local */
void BeamGlobalAssembler::emit_debug_bp() {
    Label error = a.newLabel();

    /* Read and adjust the return address we saved in generic_bp_local. */
    a.ldr(ARG2, TMP_MEM1q);
    a.sub(ARG2, ARG2, imm(BEAM_ASM_FUNC_PROLOGUE_SIZE + sizeof(ErtsCodeMFA)));

    emit_enter_runtime<Update::eHeapAlloc | Update::eReductions>();

    a.mov(ARG1, c_p);
    load_x_reg_array(ARG3);
    mov_imm(ARG4, am_breakpoint);
    runtime_call<4>(call_error_handler);

    emit_leave_runtime<Update::eHeapAlloc | Update::eReductions>();

    /* We skip two runtime frames (ours and the one entered in the module
     * header) so that we can call the error handler's code instead of
     * `call_nif_early`, if necessary. */
    emit_leave_runtime_frame();
    emit_leave_runtime_frame();

    a.tst(ARG1, ARG1);
    a.b_eq(error);

    emit_leave_erlang_frame();
    branch(emit_setup_dispatchable_call(ARG1));

    a.bind(error);
    a.ldr(ARG2, TMP_MEM1q);
    mov_imm(ARG4, 0);
    a.b(labels[raise_exception_shared]);
}

static void return_trace(Process *c_p,
                         ErtsCodeMFA *mfa,
                         Eterm val,
                         ErtsTracer tracer,
                         Eterm session_id) {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_return_trace() {
    // TODO
    emit_nyi("emit_return_trace");
}

void BeamModuleAssembler::emit_i_call_trace_return() {
    // TODO
    emit_nyi("emit_i_call_trace_return");
}

void BeamModuleAssembler::emit_i_return_to_trace() {
    // TODO
    emit_nyi("emit_i_return_to_trace");
}

void BeamModuleAssembler::emit_i_hibernate() {
    Label error = a.newLabel();

    emit_enter_runtime<Update::eReductions | Update::eHeapAlloc>();

    a.mov(ARG1, c_p);
    load_x_reg_array(ARG2);
    runtime_call<2>(erts_hibernate);

    emit_leave_runtime<Update::eReductions | Update::eHeapAlloc>();

    a.tst(ARG1, ARG1);
    a.b_eq(error);

    a.ldr(TMP, arm::Mem(c_p, offsetof(Process, flags)));
    mov_imm(VAR, ~F_HIBERNATE_SCHED);
    a.and_(TMP, TMP, VAR);
    a.str(TMP, arm::Mem(c_p, offsetof(Process, flags)));
    a.b(resolve_fragment(ga->get_do_schedule(), disp32MB));

    a.bind(error);
    emit_raise_exception(&BIF_TRAP_EXPORT(BIF_hibernate_3)->info.mfa);
}
