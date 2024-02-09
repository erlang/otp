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
 * RET = export entry */
void BeamGlobalAssembler::emit_generic_bp_global() {
    emit_enter_frame();
    emit_enter_runtime<Update::eReductions | Update::eHeapAlloc>();

    a.mov(ARG1, c_p);
    a.lea(ARG2, x86::qword_ptr(RET, offsetof(Export, info)));
    load_x_reg_array(ARG3);
    runtime_call<3>(erts_generic_breakpoint);

    emit_leave_runtime<Update::eReductions | Update::eHeapAlloc>();

    /* This is technically a tail call so we must leave the current frame
     * before jumping. Note that we might not leave the frame we entered
     * earlier this function, but one added by `erts_generic_breakpoint`. */
    emit_leave_frame();
    a.jmp(RET);
}

/* This function is called from the module header, which is in turn called from
 * the prologue of the traced function. As such, the real return address is at
 * RSP+8.
 *
 * See beam_asm.h about more details */
void BeamGlobalAssembler::emit_generic_bp_local() {
    emit_assert_erlang_stack();

#ifdef NATIVE_ERLANG_STACK
    /* Since we've entered here on the Erlang stack, we need to stash our
     * return addresses in case `erts_generic_breakpoint` pushes any trace
     * frames.
     *
     * Note that both of these are return addresses even when frame pointers
     * are enabled due to the way the breakpoint trampoline works. They must
     * not be restored until we're ready to return to module code, lest we
     * leave the stack in an inconsistent state. */
    a.pop(TMP_MEM2q);
    a.pop(ARG2);
#else
    a.mov(ARG2, x86::qword_ptr(x86::rsp, 8));
#endif

    a.mov(TMP_MEM1q, ARG2);

    /* Our actual return address is valid (and word-aligned), but it points just
     * after the trampoline word so we'll need to skip that to find our
     * ErtsCodeInfo. */
    a.sub(ARG2, imm(sizeof(UWord) + sizeof(ErtsCodeInfo)));

#ifdef DEBUG
    {
        Label next = a.newLabel();

        /* Crash if our return address isn't word-aligned. */
        a.test(ARG2, imm(sizeof(UWord) - 1));
        a.je(next);

        a.hlt();

        a.bind(next);
    }
#endif

    emit_enter_frame();
    emit_enter_runtime<Update::eReductions | Update::eHeapAlloc>();

    a.mov(ARG1, c_p);
    /* ARG2 is already set above */
    load_x_reg_array(ARG3);
    runtime_call<3>(erts_generic_breakpoint);

    emit_leave_runtime<Update::eReductions | Update::eHeapAlloc>();

    /* This doesn't necessarily leave the frame entered above: see the
     * corresponding comment in `generic_bp_global` */
    emit_leave_frame();

    a.cmp(RET, imm(BeamOpCodeAddr(op_i_debug_breakpoint)));
    a.je(labels[debug_bp]);

#ifdef NATIVE_ERLANG_STACK
    /* Note that we don't restore our return addresses in the `debug_bp` case
     * above, since it tail calls the error handler and thus never returns to
     * module code or `call_nif_early`. */
    a.push(TMP_MEM1q);
    a.push(TMP_MEM2q);
#endif

    a.ret();
}

/* This function is called from the module header which is called from the
 * prologue of the function to trace. See beam_asm.h about more details
 *
 * The only place that we can come to here is from generic_bp_local */
void BeamGlobalAssembler::emit_debug_bp() {
    Label error = a.newLabel();

#ifndef NATIVE_ERLANG_STACK
    /* We're never going to return to module code, so we have to discard the
     * return addresses added by the breakpoint trampoline. */
    a.add(x86::rsp, imm(sizeof(ErtsCodePtr[2])));
#endif

    emit_assert_erlang_stack();

    emit_enter_frame();
    emit_enter_runtime<Update::eReductions | Update::eHeapAlloc>();

    /* Read and adjust the return address we saved in generic_bp_local. */
    a.mov(ARG2, TMP_MEM1q);
    a.sub(ARG2, imm(sizeof(UWord)));

    a.mov(ARG1, c_p);
    a.lea(ARG2, x86::qword_ptr(ARG2, -(int)sizeof(ErtsCodeMFA)));
    load_x_reg_array(ARG3);
    a.mov(ARG4, imm(am_breakpoint));
    runtime_call<4>(call_error_handler);

    emit_leave_runtime<Update::eReductions | Update::eHeapAlloc>();
    emit_leave_frame();

    a.test(RET, RET);
    a.je(error);

    a.jmp(emit_setup_dispatchable_call(RET));

    a.bind(error);
    {
        a.mov(ARG2, TMP_MEM1q);
        a.jmp(labels[raise_exception]);
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
    a.mov(ARG2, getYRef(0));
    a.mov(ARG3, getXRef(0));
    a.mov(ARG4, getYRef(1)); /* tracer */
    a.mov(ARG5, getYRef(2)); /* session_id */

    emit_enter_runtime<Update::eHeapAlloc>();

    a.mov(ARG1, c_p);
    runtime_call<5>(return_trace);

    emit_leave_runtime<Update::eHeapAlloc>();

    emit_deallocate(ArgWord(BEAM_RETURN_TRACE_FRAME_SZ));
    emit_return();
}

void BeamModuleAssembler::emit_i_call_trace_return() {
    /* Pass prev_info if present (is a CP), otherwise null. */
    a.mov(ARG2, getYRef(0));
    mov_imm(ARG4, 0);

    a.test(ARG2, imm(_CPMASK));
    a.lea(ARG2, x86::qword_ptr(ARG2, -(Sint)sizeof(ErtsCodeInfo)));
    a.cmovnz(ARG2, ARG4);
    a.mov(ARG3, getYRef(1));
    a.mov(ARG4, getYRef(2));

    emit_enter_runtime<Update::eHeapAlloc>();

    a.mov(ARG1, c_p);
    runtime_call<4>(erts_call_trace_return);

    emit_leave_runtime<Update::eHeapAlloc>();

    emit_deallocate(ArgWord(BEAM_RETURN_CALL_ACC_TRACE_FRAME_SZ));
    emit_return();
}

void BeamModuleAssembler::emit_i_return_to_trace() {
    UWord frame_size = BEAM_RETURN_TO_TRACE_FRAME_SZ;

#if !defined(NATIVE_ERLANG_STACK)
    frame_size += CP_SIZE;
#endif

    a.mov(ARG2, getYRef(0)); /* session_id */
    a.lea(ARG3, x86::qword_ptr(E, frame_size * sizeof(Eterm)));

    emit_enter_runtime<Update::eReductions | Update::eHeapAlloc>();

    a.mov(ARG1, c_p);
    runtime_call<3>(beam_jit_return_to_trace);

    emit_leave_runtime<Update::eReductions | Update::eHeapAlloc>();

    emit_deallocate(ArgWord(BEAM_RETURN_TO_TRACE_FRAME_SZ));
    emit_return();
}

void BeamModuleAssembler::emit_i_hibernate() {
    Label error = a.newLabel();

    emit_enter_runtime<Update::eReductions | Update::eHeapAlloc>();

    a.mov(ARG1, c_p);
    load_x_reg_array(ARG2);
    runtime_call<2>(erts_hibernate);

    emit_leave_runtime<Update::eReductions | Update::eHeapAlloc>();

    a.test(RET, RET);
    a.je(error);

    a.and_(x86::dword_ptr(c_p, offsetof(Process, flags)),
           imm(~F_HIBERNATE_SCHED));
    a.jmp(resolve_fragment(ga->get_do_schedule()));

    a.bind(error);
    emit_raise_exception(&BIF_TRAP_EXPORT(BIF_hibernate_3)->info.mfa);
}
