/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2020-2020. All Rights Reserved.
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

/* This function is called from the export entry of a function */
void BeamGlobalAssembler::emit_generic_bp_global() {
    /* ARG2 contains a pointer to exactly after the ErtsCodeInfo */
    emit_heavy_swapout();
    a.mov(ARG1, c_p);
    a.lea(ARG2, x86::qword_ptr(ARG2, -(Sint)sizeof(ErtsCodeInfo)));
    load_x_reg_array(ARG3);
    abs_call<3>(erts_generic_breakpoint);
    emit_heavy_swapin();
    a.jmp(RET);
}

/* This function is called from the module header which is called
 * from the prologue of the function to trace.
 * See beam_asm.h about more details */
void BeamGlobalAssembler::emit_generic_bp_local() {
    /* Reserve 1 word and align stack. We are aligned coming into this
     * function which is very odd, but had to be done for the module
     * trampoline section to work. */
    a.sub(x86::rsp, imm(2 * sizeof(UWord)));

    emit_stackcheck();

    /* Read address from stack and get ErtsCodeInfo* from it into ARG2 */
    a.mov(ARG2, x86::qword_ptr(x86::rsp, 3 * sizeof(UWord)));
    a.dec(ARG2);
    a.and_(ARG2, imm(~0x7));
    a.mov(x86::qword_ptr(x86::rsp), ARG2);
    a.lea(ARG2, x86::qword_ptr(ARG2, -(Sint)sizeof(ErtsCodeInfo)));

    emit_heavy_swapout();
    a.mov(ARG1, c_p);
    /* ARG2 is already set above */
    load_x_reg_array(ARG3);
    abs_call<3>(erts_generic_breakpoint);
    emit_heavy_swapin();

    /* If the return value is op_i_debug_breakpoint, we do a debug breakpoint */
    a.cmp(RET, imm(BeamOpCodeAddr(op_i_debug_breakpoint)));
    a.je(labels[debug_bp]);
    a.lea(x86::rsp, x86::qword_ptr(x86::rsp, 2 * sizeof(UWord)));
    a.ret();
}

/* This function is called from the module header which is called
 * from the prologue of the function to trace.
 * See beam_asm.h about more details
 *
 * The only place that we can come to here is from generic_bp_local */
void BeamGlobalAssembler::emit_debug_bp() {
    Label next = a.newLabel();

    emit_stackcheck();

    /* Read I from stack, saved in generic_bp_local */
    a.mov(ARG2, x86::qword_ptr(x86::rsp));

    /* Put on stack for later use in error case */
    a.mov(x86::qword_ptr(x86::rsp), ARG2);

    emit_heavy_swapout();

    a.mov(ARG1, c_p);
    /* ARG2 is already set above */
    load_x_reg_array(ARG3);
    a.mov(ARG4, imm(am_breakpoint));
    abs_call<4>(call_error_handler);

    emit_heavy_swapin();

    a.test(RET, RET);
    a.jne(next);
    a.mov(ARG2, x86::qword_ptr(x86::rsp));
    a.lea(RET, x86::qword_ptr(labels[handle_error_shared]));

    a.bind(next);
    {
        /* We patch the return address to jump to the correct place. */
        a.mov(x86::qword_ptr(x86::rsp, 3 * sizeof(UWord)), RET);
        /* Here we skip one call frame to jump to the place we just set. This
         * makes it so that if we are to do a call_nif_early, we skip that and
         * call the error handlers code instead. This in order to be compliant
         * with the way that the interpreter works. */
        a.lea(x86::rsp, x86::qword_ptr(x86::rsp, 3 * sizeof(UWord)));
        a.ret();
    }
}

static void return_trace(Process *c_p,
                         ErtsCodeMFA *mfa,
                         Eterm val,
                         ErtsTracer *tracer) {
    ERTS_UNREQ_PROC_MAIN_LOCK(c_p);
    erts_trace_return(c_p, mfa, val, tracer);
    ERTS_REQ_PROC_MAIN_LOCK(c_p);
}

void BeamModuleAssembler::emit_return_trace() {
    emit_swapout();
    a.mov(ARG1, c_p);
    a.mov(ARG2, getYRef(0));
    a.mov(ARG3, getXRef(0));
    a.lea(ARG4, getYRef(1));
    abs_call<4>(return_trace);
    emit_swapin();
    emit_deallocate(ArgVal(ArgVal::i, 3 * sizeof(Eterm)));
    emit_return();
}

void BeamModuleAssembler::emit_i_return_time_trace() {
    Label is_cp = a.newLabel(), execute = a.newLabel();

    a.mov(ARG2, getYRef(0));
    a.mov(ARG3, ARG2);

    a.and_(ARG3, imm(_CPMASK));
    a.jz(is_cp);

    a.sub(ARG2, ARG2);
    a.jmp(execute);

    a.bind(is_cp);
    {
        a.lea(ARG2, x86::qword_ptr(ARG2, -(Sint)sizeof(ErtsCodeInfo)));
        /* Fall through*/
    }

    a.bind(execute);
    {
        emit_swapout();
        a.mov(ARG1, c_p);
        abs_call<2>(erts_trace_time_return);
        emit_swapin();
        emit_deallocate(ArgVal(ArgVal::i, 2 * sizeof(Eterm)));
        emit_return();
    }
}

static void i_return_to_trace(Process *c_p) {
    if (IS_TRACED_FL(c_p, F_TRACE_RETURN_TO)) {
        Uint *cpp = (Uint *)c_p->stop;
        while (is_not_CP(*cpp)) {
            cpp++;
        }
        for (;;) {
            BeamInstr *w = cp_val(*cpp);
            if (BeamIsReturnTrace(w)) {
                do
                    ++cpp;
                while (is_not_CP(*cpp));
                cpp += 2;
            } else if (BeamIsReturnToTrace(w)) {
                do
                    ++cpp;
                while (is_not_CP(*cpp));
            } else {
                break;
            }
        }
        ERTS_UNREQ_PROC_MAIN_LOCK(c_p);
        erts_trace_return_to(c_p, cp_val(*cpp));
        ERTS_REQ_PROC_MAIN_LOCK(c_p);
    }
}

void BeamModuleAssembler::emit_i_return_to_trace() {
    emit_swapout();
    a.mov(ARG1, c_p);
    abs_call<1>(i_return_to_trace);
    emit_swapin();
    emit_deallocate(ArgVal(ArgVal::i, 1 * 8));
    emit_return();
}

void BeamModuleAssembler::emit_i_hibernate() {
    Label error = a.newLabel(), entry = a.newLabel();

    a.align(kAlignCode, 8);
    a.bind(entry);

    emit_heavy_swapout();
    a.mov(ARG1, c_p);
    load_x_reg_array(ARG2);
    abs_call<2>(erts_hibernate);
    a.test(RET, RET);
    a.je(error);

    emit_heavy_swapin();
    a.mov(ARG1, x86::qword_ptr(c_p, offsetof(Process, flags)));
    a.and_(ARG1, imm(~F_HIBERNATE_SCHED));
    a.mov(x86::qword_ptr(c_p, offsetof(Process, flags)), ARG1);
    abs_jmp(ga->get_do_schedule());

    a.bind(error);
    {
        emit_heavy_swapin();
        emit_handle_error(entry, &BIF_TRAP_EXPORT(BIF_hibernate_3)->info.mfa);
    }
}
