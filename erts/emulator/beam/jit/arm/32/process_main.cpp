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

    /* Be kind to debuggers and `perf` by setting up a proper stack frame. */
    emit_enter_runtime_frame();

    /* The offset of start_time_i in ErtsSchedulerRegisters cannot stay
     * in the 12 bit immediate accepted by the STR instruction.
     *
     * We use ARG4 to point to start_time_i so then we can use a relative offset
     * to point to start_time.
     */
    const Uint start_t_i_offset = offsetof(ErtsSchedulerRegisters, start_time_i);
    const Uint start_t_offset = offsetof(ErtsSchedulerRegisters, start_time);
    // start_time precedes start_time_i in the struct
    const Uint relative_start_t_offset = start_t_offset - start_t_i_offset;
    a.mov(ARG4, scheduler_registers);
    a.add(ARG4, ARG4, imm(start_t_i_offset));
    const arm::Mem start_time_i = arm::Mem(ARG4);
    const arm::Mem start_time = arm::Mem(ARG4, relative_start_t_offset);

    // Scheduling loop initialization
    mov_imm(TMP, 0);
    a.str(TMP, start_time_i);
    a.str(TMP, start_time);

    mov_imm(c_p, 0);
    mov_imm(FCALLS, 0);
    mov_imm(ARG3, 0); /* Set reds_used for erts_schedule call */

    // Start scheduling loop
    a.b(schedule_next);

    // We will jump here when a process is exiting to register
    // how many reductions were used
    a.bind(do_schedule_local);
    {
        /* Figure out reds_used. def_arg_reg[5] = REDS_IN */
        a.ldr(TMP, arm::Mem(c_p, offsetof(Process, def_arg_reg[5])));
        a.sub(ARG3, TMP, FCALLS);
        a.b(schedule_next);
    }

    a.bind(context_switch_local);
    comment("Context switch, unknown arity/MFA");
    //TODO
    emit_nyi("context_switch_local");
    a.bind(context_switch_simplified_local);
    comment("Context switch, known arity and MFA");
    //TODO
    emit_nyi("context_switch_simplified_local");

    a.bind(schedule_next);
    comment("schedule_next");

    {
        Label schedule = a.newLabel(), skip_long_schedule = a.newLabel();

        /* ARG3 contains reds_used at this point */

        //Jump to schedule if start_time is 0
        a.ldr(TMP, start_time);
        a.tst(TMP, TMP);
        a.b_eq(schedule);
        // Call check_monitor_long_schedule, a performance monitoring function
        // that detects when Erlang processes run for too long without yielding.
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
        /* TODO */
        emit_nyi("erts_msacc_cache check");
#endif

        mov_imm(TMP, 0);
        a.str(TMP, start_time);
        mov_imm(ARG1, &erts_system_monitor_long_schedule);
        a.ldr(TMP, arm::Mem(ARG1));
        a.tst(TMP, TMP);
        a.b_eq(skip_long_schedule);

        {
            /* Enable long schedule test */
            runtime_call<0>(erts_timestamp_millis);
            a.str(ARG1, start_time);
            a.ldr(TMP, arm::Mem(c_p, offsetof(Process, i)));
            a.str(TMP, start_time_i);
        }

        a.bind(skip_long_schedule);
        comment("skip_long_schedule");

        a.mov(ARG1, c_p);
        load_x_reg_array(ARG2);
        runtime_call<2>(copy_in_registers);
    }

    /* Processes may jump to the exported entry points below, executing on the
     * Erlang stack when entering. These are separate from the `_local` labels
     * above as we don't want to worry about which stack we're on when the
     * cases overlap. */

    /* `ga->get_context_switch()`
     *
     * The *next* instruction pointer is provided in ARG3, and must be preceded
     * by an ErtsCodeMFA.
     */
    a.bind(labels[context_switch]);
    {
        emit_enter_runtime<Update::eStack | Update::eHeap>();

        a.b(context_switch_local);
    }

    /* `ga->get_context_switch_simplified()`
     *
     * The next instruction pointer is provided in ARG3, which does not need to
     * point past an ErtsCodeMFA as the process structure has already been
     * updated.
     */
    a.bind(labels[context_switch_simplified]);
    {
        emit_enter_runtime<Update::eStack | Update::eHeap>();

        a.b(context_switch_simplified_local);
    }

    /* `ga->get_do_schedule()`
     *
     * `c_p->i` must be set prior to jumping here.
     */
    a.bind(labels[do_schedule]);
    {
        emit_enter_runtime<Update::eStack | Update::eHeap>();

        a.b(do_schedule_local);
    }
}
