/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2020-2024. All Rights Reserved.
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
#include "bif.h"
#include "beam_common.h"
#include "code_ix.h"
#include "export.h"
}

#undef x

#if defined(DEBUG) || defined(ERTS_ENABLE_LOCK_CHECK)
static Process *erts_debug_schedule(ErtsSchedulerData *esdp,
                                    Process *c_p,
                                    int calls) {
    PROCESS_MAIN_CHK_LOCKS(c_p);
    ERTS_UNREQ_PROC_MAIN_LOCK(c_p);
    ERTS_VERIFY_UNUSED_TEMP_ALLOC(c_p);
    c_p = erts_schedule(esdp, c_p, calls);
    ERTS_VERIFY_UNUSED_TEMP_ALLOC(c_p);
    ERTS_REQ_PROC_MAIN_LOCK(c_p);
    PROCESS_MAIN_CHK_LOCKS(c_p);
    return c_p;
}
#endif

/* void process_main(ErtsSchedulerData *esdp); */
void BeamGlobalAssembler::emit_process_main() {
    Label context_switch_local = a.newLabel(),
          context_switch_simplified_local = a.newLabel(),
          do_schedule_local = a.newLabel(), schedule_next = a.newLabel();

    const arm::Mem start_time_i =
            getSchedulerRegRef(offsetof(ErtsSchedulerRegisters, start_time_i));
    const arm::Mem start_time =
            getSchedulerRegRef(offsetof(ErtsSchedulerRegisters, start_time));

    /* Be kind to debuggers and `perf` by setting up a proper stack frame. */
    a.stp(a64::x29, a64::x30, arm::Mem(a64::sp, -16).pre());

    /* Allocate the register structure on the stack to allow computing the
     * runtime stack address from it, greatly reducing the cost of stack
     * swapping. */
    a.mov(TMP1, a64::sp);
    sub(TMP1, TMP1, sizeof(ErtsSchedulerRegisters) + ERTS_CACHE_LINE_SIZE);
    a.and_(TMP1, TMP1, imm(~ERTS_CACHE_LINE_MASK));
    a.mov(a64::sp, TMP1);
    a.mov(a64::x29, a64::sp);

    a.str(TMP1, arm::Mem(ARG1, offsetof(ErtsSchedulerData, registers)));

    a.mov(scheduler_registers, a64::sp);

    /* Save the initial SP of the thread so that we can verify that it
     * doesn't grow. */
#ifdef JIT_HARD_DEBUG
    a.mov(TMP1, a64::sp);
    a.str(TMP1, getInitialSPRef());
#endif

    a.str(a64::xzr, start_time_i);
    a.str(a64::xzr, start_time);

    mov_imm(c_p, 0);
    mov_imm(FCALLS, 0);
    mov_imm(ARG3, 0); /* Set reds_used for erts_schedule call */

    a.b(schedule_next);

    a.bind(do_schedule_local);
    {
        /* Figure out reds_used. def_arg_reg[5] = REDS_IN */
        a.ldr(TMP1, arm::Mem(c_p, offsetof(Process, def_arg_reg[5])));
        a.sub(ARG3.w(), TMP1.w(), FCALLS);
        a.b(schedule_next);
    }

    /*
     * The *next* instruction pointer is provided in ARG3, and must be preceded
     * by an ErtsCodeMFA.
     */
    a.bind(context_switch_local);
    comment("Context switch, unknown arity/MFA");
    {
        Sint arity_offset = offsetof(ErtsCodeMFA, arity) - sizeof(ErtsCodeMFA);

        a.ldur(TMP1.w(), arm::Mem(ARG3, arity_offset));
        a.strb(TMP1.w(), arm::Mem(c_p, offsetof(Process, arity)));

        a.sub(TMP1, ARG3, imm(sizeof(ErtsCodeMFA)));
        a.str(TMP1, arm::Mem(c_p, offsetof(Process, current)));

        /* !! Fall through !! */
    }

    a.bind(context_switch_simplified_local);
    comment("Context switch, known arity and MFA");
    {
        Label not_exiting = a.newLabel();

#ifdef DEBUG
        Label check_i = a.newLabel();
        /* Check that ARG3 is set to a valid CP. */
        a.tst(ARG3, imm(_CPMASK));
        a.b_eq(check_i);
        a.udf(1);
        a.bind(check_i);
#endif

        a.str(ARG3, arm::Mem(c_p, offsetof(Process, i)));
        a.ldr(TMP1.w(), arm::Mem(c_p, offsetof(Process, state.value)));

        a.tst(TMP1, imm(ERTS_PSFLG_EXITING));
        a.b_eq(not_exiting);
        {
            comment("Process exiting");

            a.adr(TMP1, labels[process_exit]);
            a.str(TMP1, arm::Mem(c_p, offsetof(Process, i)));
            a.strb(ZERO.w(), arm::Mem(c_p, offsetof(Process, arity)));
            a.str(ZERO, arm::Mem(c_p, offsetof(Process, current)));
            a.b(do_schedule_local);
        }

        a.bind(not_exiting);

        /* Figure out reds_used. def_arg_reg[5] = REDS_IN */
        a.ldr(TMP1.w(), arm::Mem(c_p, offsetof(Process, def_arg_reg[5])));
        a.sub(FCALLS, TMP1.w(), FCALLS);

        comment("Copy out X registers");
        a.mov(ARG1, c_p);
        load_x_reg_array(ARG2);
        runtime_call<2>(copy_out_registers);

        /* Restore reds_used from FCALLS */
        a.mov(ARG3.w(), FCALLS);

        /* !! Fall through !! */
    }

    a.bind(schedule_next);
    comment("schedule_next");

    {
        Label schedule = a.newLabel(), skip_long_schedule = a.newLabel();

        /* ARG3 contains reds_used at this point */

        a.ldr(TMP1, start_time);
        a.cbz(TMP1, schedule);
        {
            a.mov(ARG1, c_p);
            a.ldr(ARG2, start_time);

            /* Spill reds_used in start_time slot */
            a.str(ARG3, start_time);

            a.ldr(ARG3, start_time_i);
            runtime_call<3>(check_monitor_long_schedule);

            /* Restore reds_used */
            a.ldr(ARG3, start_time);
        }

        a.bind(schedule);
        mov_imm(ARG1, 0);
        a.mov(ARG2, c_p);
#if defined(DEBUG) || defined(ERTS_ENABLE_LOCK_CHECK)
        runtime_call<3>(erts_debug_schedule);
#else
        runtime_call<3>(erts_schedule);
#endif
        a.mov(c_p, ARG1);

#ifdef ERTS_MSACC_EXTENDED_STATES
        lea(ARG1, erts_msacc_cache);
        runtime_call<1>(erts_msacc_update_cache);
#endif

        a.str(ZERO, start_time);
        mov_imm(ARG1, &erts_system_monitor_long_schedule);
        a.ldr(TMP1, arm::Mem(ARG1));
        a.cbz(TMP1, skip_long_schedule);

        {
            /* Enable long schedule test */
            runtime_call<0>(erts_timestamp_millis);
            a.str(ARG1, start_time);
            a.ldr(TMP1, arm::Mem(c_p, offsetof(Process, i)));
            a.str(TMP1, start_time_i);
        }

        a.bind(skip_long_schedule);
        comment("skip_long_schedule");

        /* Copy arguments */
        a.mov(ARG1, c_p);
        load_x_reg_array(ARG2);
        runtime_call<2>(copy_in_registers);

        /* Setup reduction counting */
        a.ldr(FCALLS, arm::Mem(c_p, offsetof(Process, fcalls)));
        a.str(FCALLS.x(), arm::Mem(c_p, offsetof(Process, def_arg_reg[5])));

#ifdef DEBUG
        a.str(FCALLS.x(), a64::Mem(c_p, offsetof(Process, debug_reds_in)));
#endif

        comment("check whether save calls is on");
        a.mov(ARG1, c_p);
        mov_imm(ARG2, ERTS_PSD_SAVED_CALLS_BUF);
        runtime_call<2>(erts_psd_get);

        /* Read the active code index, overriding it with
         * ERTS_SAVE_CALLS_CODE_IX when save_calls is enabled (ARG1 != 0). */
        mov_imm(TMP1, &the_active_code_index);
        a.ldr(TMP1.w(), arm::Mem(TMP1));
        a.mov(TMP2, imm(ERTS_SAVE_CALLS_CODE_IX));
        a.cmp(ARG1, ZERO);
        a.csel(active_code_ix, TMP1, TMP2, arm::CondCode::kEQ);

        /* Start executing the Erlang process. Note that reductions have
         * already been set up above. */
        emit_leave_runtime<Update::eStack | Update::eHeap | Update::eXRegs>();

        /* Check if we are just returning from a dirty nif/bif call and if so we
         * need to do a bit of cleaning up before continuing.
         *
         * This relies on `op_call_nif_WWW` / `op_call_bif_W` being encoded as
         * UDF(opcode) followed by UDF(0), which we will never emit. */
        a.ldr(ARG1, arm::Mem(c_p, offsetof(Process, i)));
        a.ldr(TMP1, arm::Mem(ARG1));

        ERTS_CT_ASSERT((op_call_nif_WWW & 0xFFFF0000) == 0);
        a.cmp(TMP1, imm(op_call_nif_WWW));
        a.b_eq(labels[dispatch_nif]);

        ERTS_CT_ASSERT((op_call_bif_W & 0xFFFF0000) == 0);
        a.cmp(TMP1, imm(op_call_bif_W));
        a.b_eq(labels[dispatch_bif]);

        a.br(ARG1);
    }

    /* Processes may jump to the exported entry points below, executing on the
     * Erlang stack when entering. These are separate from the `_local` labels
     * above as we don't want to worry about which stack we're on when the
     * cases overlap. */

    /* `ga->get_context_switch()`
     *
     * The *next* instruction pointer is provided in ARG3, and must be preceded
     * by an ErtsCodeMFA.
     *
     * The X registers are expected to be in CPU registers.
     */
    a.bind(labels[context_switch]);
    {
        emit_enter_runtime<Update::eStack | Update::eHeap | Update::eXRegs>();

        a.b(context_switch_local);
    }

    /* `ga->get_context_switch_simplified()`
     *
     * The next instruction pointer is provided in ARG3, which does not need to
     * point past an ErtsCodeMFA as the process structure has already been
     * updated.
     *
     * The X registers are expected to be in CPU registers.
     */
    a.bind(labels[context_switch_simplified]);
    {
        emit_enter_runtime<Update::eStack | Update::eHeap | Update::eXRegs>();

        a.b(context_switch_simplified_local);
    }

    /* `ga->get_do_schedule()`
     *
     * `c_p->i` must be set prior to jumping here.
     *
     * The X registers are expected to be in CPU registers.
     */
    a.bind(labels[do_schedule]);
    {
        emit_enter_runtime<Update::eStack | Update::eHeap | Update::eXRegs>();

        a.b(do_schedule_local);
    }
}
