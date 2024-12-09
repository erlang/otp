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

    emit_enter_runtime<Update::eHeapAlloc | Update::eXRegs |
                       Update::eReductions>();

    a.mov(ARG1, c_p);
    /* ARG2 is already set above */
    load_x_reg_array(ARG3);
    runtime_call<3>(erts_generic_breakpoint);

    emit_leave_runtime<Update::eHeapAlloc | Update::eXRegs |
                       Update::eReductions>();

    /* This is technically a tail call so we must leave the current frame
     * before jumping. Note that we might not leave the frame we entered
     * earlier in this function, but one added by erts_generic_breakpoint. */
    emit_leave_erlang_frame();
    a.br(ARG1);
}

/* This function is called from the module header, which is in turn called from
 * the prologue of the traced function. As such, the real return address is at
 * SP+8 rather than LR (x30).
 *
 * See beam_asm.h for more details */
void BeamGlobalAssembler::emit_generic_bp_local() {
    a.ldr(ARG2, arm::Mem(a64::sp, 8));

    /* Stash return address for later use in `debug_bp` */
    a.str(ARG2, TMP_MEM1q);

    /* Our actual return address is valid (and word-aligned), but it points
     * just after the trampoline word so we'll need to skip that to find our
     * ErtsCodeInfo. */
    a.sub(ARG2, ARG2, imm(BEAM_ASM_FUNC_PROLOGUE_SIZE + sizeof(ErtsCodeInfo)));

    emit_enter_runtime_frame();
    emit_enter_runtime<Update::eHeapAlloc | Update::eXRegs |
                       Update::eReductions>();

    a.mov(ARG1, c_p);
    /* ARG2 is already set above */
    load_x_reg_array(ARG3);
    runtime_call<3>(erts_generic_breakpoint);

    emit_leave_runtime<Update::eHeapAlloc | Update::eXRegs |
                       Update::eReductions>();

    a.cmp(ARG1, imm(BeamOpCodeAddr(op_i_debug_breakpoint)));
    a.b_eq(labels[debug_bp]);

    emit_leave_runtime_frame();
    a.ret(a64::x30);
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

    emit_enter_runtime<Update::eHeapAlloc | Update::eXRegs |
                       Update::eReductions>();

    a.mov(ARG1, c_p);
    load_x_reg_array(ARG3);
    a.mov(ARG4, imm(am_breakpoint));
    runtime_call<4>(call_error_handler);

    emit_leave_runtime<Update::eHeapAlloc | Update::eXRegs |
                       Update::eReductions>();

    /* We skip two runtime frames (ours and the one entered in the module
     * header) so that we can call the error handler's code instead of
     * `call_nif_early`, if necessary. */
    emit_leave_runtime_frame();
    emit_leave_runtime_frame();

    a.cbz(ARG1, error);

    emit_leave_erlang_frame();
    branch(emit_setup_dispatchable_call(ARG1));

    a.bind(error);
    {
        a.ldr(ARG2, TMP_MEM1q);
        mov_imm(ARG4, 0);

        a.b(labels[raise_exception_shared]);
    }
}

static void return_trace(Process *c_p,
                         ErtsCodeMFA *mfa,
                         Eterm val,
                         ErtsTracer tracer,
                         Eterm session_id) {
    ERTS_UNREQ_PROC_MAIN_LOCK(c_p);
    erts_trace_return(c_p, mfa, val, tracer, session_id);
    ERTS_REQ_PROC_MAIN_LOCK(c_p);
}

void BeamModuleAssembler::emit_return_trace() {
    a.ldr(ARG2, getYRef(0));
    a.mov(ARG3, XREG0);
    a.ldr(ARG4, getYRef(1)); /* tracer */
    a.ldr(ARG5, getYRef(2)); /* session_id */

    ERTS_CT_ASSERT(ERTS_HIGHEST_CALLEE_SAVE_XREG >= 1);
    emit_enter_runtime<Update::eHeapAlloc>(1);

    a.mov(ARG1, c_p);
    runtime_call<5>(return_trace);

    emit_leave_runtime<Update::eHeapAlloc>(1);

    emit_deallocate(ArgVal(ArgVal::Word, BEAM_RETURN_TRACE_FRAME_SZ));
    emit_return();
}

void BeamModuleAssembler::emit_i_call_trace_return() {
    /* Pass prev_info if present (is a CP), otherwise null. */
    a.ldr(ARG2, getYRef(0));
    mov_imm(ARG4, 0);

    a.tst(ARG2, imm(_CPMASK));
    a.sub(ARG2, ARG2, imm(sizeof(ErtsCodeInfo)));
    a.csel(ARG2, ARG2, ARG4, arm::CondCode::kEQ);
    a.ldr(ARG3, getYRef(1));
    a.ldr(ARG4, getYRef(2));

    ERTS_CT_ASSERT(ERTS_HIGHEST_CALLEE_SAVE_XREG >= 1);
    emit_enter_runtime<Update::eHeapAlloc>(1);

    a.mov(ARG1, c_p);
    runtime_call<4>(erts_call_trace_return);

    emit_leave_runtime<Update::eHeapAlloc>(1);

    emit_deallocate(ArgVal(ArgVal::Word, BEAM_RETURN_CALL_ACC_TRACE_FRAME_SZ));
    emit_return();
}

void BeamModuleAssembler::emit_i_return_to_trace() {
    a.ldr(ARG2, getYRef(0)); /* session_id */
    a.add(ARG3, E, imm(BEAM_RETURN_TO_TRACE_FRAME_SZ * sizeof(Eterm)));

    ERTS_CT_ASSERT(ERTS_HIGHEST_CALLEE_SAVE_XREG >= 1);

    emit_enter_runtime<Update::eHeapAlloc>(1);

    a.mov(ARG1, c_p);
    runtime_call<3>(beam_jit_return_to_trace);

    emit_leave_runtime<Update::eHeapAlloc>(1);

    emit_deallocate(ArgVal(ArgVal::Word, BEAM_RETURN_TO_TRACE_FRAME_SZ));
    emit_return();
}

void BeamModuleAssembler::emit_i_hibernate() {
    Label error = a.newLabel();

    emit_enter_runtime<Update::eHeapAlloc | Update::eXRegs |
                       Update::eReductions>(3);

    a.mov(ARG1, c_p);
    load_x_reg_array(ARG2);
    runtime_call<2>(erts_hibernate);

    emit_leave_runtime<Update::eHeapAlloc | Update::eXRegs |
                       Update::eReductions>(3);

    a.cbz(ARG1, error);

    a.ldr(TMP1.w(), arm::Mem(c_p, offsetof(Process, flags)));
    a.and_(TMP1, TMP1, imm(~F_HIBERNATE_SCHED));
    a.str(TMP1.w(), arm::Mem(c_p, offsetof(Process, flags)));
    a.b(resolve_fragment(ga->get_do_schedule(), disp128MB));

    a.bind(error);
    emit_raise_exception(&BIF_TRAP_EXPORT(BIF_hibernate_3)->info.mfa);
    mark_unreachable();
}
