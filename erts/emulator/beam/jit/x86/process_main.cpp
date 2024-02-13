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

const uint8_t *BeamAssembler::nops[3] = {nop1, nop2, nop3};
const uint8_t BeamAssembler::nop1[1] = {0x90};
const uint8_t BeamAssembler::nop2[2] = {0x66, 0x90};
const uint8_t BeamAssembler::nop3[3] = {0x0F, 0x1F, 0x00};

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

    const x86::Mem start_time_i =
            getSchedulerRegRef(offsetof(ErtsSchedulerRegisters, start_time_i));
    const x86::Mem start_time =
            getSchedulerRegRef(offsetof(ErtsSchedulerRegisters, start_time));

    /* Allocate the register structure on the stack to allow computing the
     * runtime stack address from it, greatly reducing the cost of stack
     * swapping. */
    a.sub(x86::rsp, imm(sizeof(ErtsSchedulerRegisters) + ERTS_CACHE_LINE_SIZE));
    a.and_(x86::rsp, imm(~ERTS_CACHE_LINE_MASK));

    a.mov(x86::qword_ptr(ARG1, offsetof(ErtsSchedulerData, registers)),
          x86::rsp);

    /* Center `registers` at the base of x_reg_array so we can use negative
     * 8-bit displacement to address the commonly used aux_regs, located at the
     * start of the ErtsSchedulerRegisters struct. */
    a.lea(registers,
          x86::qword_ptr(x86::rsp,
                         offsetof(ErtsSchedulerRegisters, x_reg_array.d)));

#if defined(DEBUG) && defined(NATIVE_ERLANG_STACK)
    /* Save stack bounds so they can be tested without clobbering anything. */
    runtime_call<0>(erts_get_stacklimit);

    a.mov(getSchedulerRegRef(
                  offsetof(ErtsSchedulerRegisters, runtime_stack_end)),
          RET);
    a.mov(getSchedulerRegRef(
                  offsetof(ErtsSchedulerRegisters, runtime_stack_start)),
          x86::rsp);
#elif !defined(NATIVE_ERLANG_STACK)
    /* Save the initial SP of the thread so that we can verify that it
     * doesn't grow. */
#    ifdef JIT_HARD_DEBUG
    a.mov(getInitialSPRef(), x86::rsp);
#    endif

    /* Manually do an `emit_enter_runtime` to match the `emit_leave_runtime`
     * below. We avoid `emit_enter_runtime` because it may do additional
     * assertions that may currently fail.
     *
     * IMPORTANT: We must ensure that this sequence leaves the stack
     * aligned on a 16-byte boundary. */
    a.mov(getRuntimeStackRef(), x86::rsp);
    a.sub(x86::rsp, imm(15));
    a.and_(x86::rsp, imm(-16));
#endif

    a.mov(start_time_i, imm(0));
    a.mov(start_time, imm(0));

    mov_imm(c_p, 0);
    mov_imm(FCALLS, 0);
    mov_imm(ARG3, 0); /* Set reds_used for erts_schedule call */

    a.jmp(schedule_next);

    a.bind(do_schedule_local);
    {
        /* Figure out reds_used. def_arg_reg[5] = REDS_IN */
        a.mov(ARG3, x86::qword_ptr(c_p, offsetof(Process, def_arg_reg[5])));
        a.sub(ARG3d, FCALLS);

        a.jmp(schedule_next);
    }

    a.bind(context_switch_local);
    comment("Context switch, unknown arity/MFA");
    {
        Sint arity_offset = offsetof(ErtsCodeMFA, arity) - sizeof(ErtsCodeMFA);

        a.movzx(ARG1d, x86::byte_ptr(ARG3, arity_offset));
        a.mov(x86::byte_ptr(c_p, offsetof(Process, arity)), ARG1.r8());

        a.lea(ARG1, x86::qword_ptr(ARG3, -(Sint)sizeof(ErtsCodeMFA)));
        a.mov(x86::qword_ptr(c_p, offsetof(Process, current)), ARG1);

        /* !! Fall through !! */
    }

    a.bind(context_switch_simplified_local);
    comment("Context switch, known arity and MFA");
    {
        Label not_exiting = a.newLabel();

#ifdef ERLANG_FRAME_POINTERS
        /* Kill the current frame pointer to avoid confusing `perf` and similar
         * tools. */
        a.sub(frame_pointer, frame_pointer);
#endif

#ifdef DEBUG
        Label check_i = a.newLabel();
        /* Check that ARG3 is set to a valid CP. */
        a.test(ARG3, imm(_CPMASK));
        a.je(check_i);
        comment("# ARG3 is not a valid CP");
        a.ud2();
        a.bind(check_i);
#endif

        a.mov(x86::qword_ptr(c_p, offsetof(Process, i)), ARG3);

#if defined(JIT_HARD_DEBUG) && defined(ERLANG_FRAME_POINTERS)
        a.mov(ARG1, c_p);
        a.mov(ARG2, x86::qword_ptr(c_p, offsetof(Process, frame_pointer)));
        a.mov(ARG3, x86::qword_ptr(c_p, offsetof(Process, stop)));

        runtime_call<3>(erts_validate_stack);
#endif

#ifdef WIN32
        a.mov(ARG1d, x86::dword_ptr(c_p, offsetof(Process, state.value)));
#else
        a.mov(ARG1d, x86::dword_ptr(c_p, offsetof(Process, state.counter)));
#endif

        a.test(ARG1d, imm(ERTS_PSFLG_EXITING));
        a.short_().je(not_exiting);
        {
            comment("Process exiting");

            a.lea(ARG1, x86::qword_ptr(labels[process_exit]));
            a.mov(x86::qword_ptr(c_p, offsetof(Process, i)), ARG1);
            a.mov(x86::byte_ptr(c_p, offsetof(Process, arity)), imm(0));
            a.mov(x86::qword_ptr(c_p, offsetof(Process, current)), imm(0));
            a.jmp(do_schedule_local);
        }
        a.bind(not_exiting);

        /* Figure out reds_used. def_arg_reg[5] = REDS_IN */
        a.mov(ARG3, x86::qword_ptr(c_p, offsetof(Process, def_arg_reg[5])));
        a.sub(ARG3d, FCALLS);

        /* Spill reds_used to FCALLS as we no longer need that value */
        a.mov(FCALLS, ARG3d);

        a.mov(ARG1, c_p);
        load_x_reg_array(ARG2);
        runtime_call<2>(copy_out_registers);

        /* Restore reds_used from FCALLS */
        a.mov(ARG3d, FCALLS);

        /* !! Fall through !! */
    }

    a.bind(schedule_next);
    comment("schedule_next");
    {
        Label schedule = a.newLabel(), skip_long_schedule = a.newLabel();

        /* ARG3 contains reds_used at this point */

        a.cmp(start_time, imm(0));
        a.short_().je(schedule);
        {
            a.mov(ARG1, c_p);
            a.mov(ARG2, start_time);

            /* Spill reds_used in start_time slot */
            a.mov(start_time, ARG3);

            a.mov(ARG3, start_time_i);
            runtime_call<3>(check_monitor_long_schedule);

            /* Restore reds_used */
            a.mov(ARG3, start_time);
        }
        a.bind(schedule);

#ifdef ERLANG_FRAME_POINTERS
        if (erts_frame_layout == ERTS_FRAME_LAYOUT_FP_RA) {
            /* Kill the current frame pointer so that misc jobs that execute
             * during `erts_schedule` aren't attributed to the function we
             * were scheduled out of. */
            a.sub(frame_pointer, frame_pointer);
        }
#endif

        mov_imm(ARG1, 0);
        a.mov(ARG2, c_p);
#if defined(DEBUG) || defined(ERTS_ENABLE_LOCK_CHECK)
        runtime_call<3>(erts_debug_schedule);
#else
        runtime_call<3>(erts_schedule);
#endif
        a.mov(c_p, RET);

#ifdef ERTS_MSACC_EXTENDED_STATES
        a.lea(ARG1,
              x86::qword_ptr(registers,
                             offsetof(ErtsSchedulerRegisters,
                                      aux_regs.d.erts_msacc_cache)));
        runtime_call<1>(erts_msacc_update_cache);
#endif

        a.mov(ARG1, imm((UWord)&erts_system_monitor_long_schedule));
        a.cmp(x86::qword_ptr(ARG1), imm(0));
        a.mov(start_time, imm(0));
        a.short_().je(skip_long_schedule);
        {
            /* Enable long schedule test */
            runtime_call<0>(erts_timestamp_millis);
            a.mov(start_time, RET);
            a.mov(RET, x86::qword_ptr(c_p, offsetof(Process, i)));
            a.mov(start_time_i, RET);
        }
        a.bind(skip_long_schedule);

        /* Copy arguments */
        a.mov(ARG1, c_p);
        load_x_reg_array(ARG2);
        runtime_call<2>(copy_in_registers);

        /* Setup reduction counting */
        a.mov(FCALLS, x86::dword_ptr(c_p, offsetof(Process, fcalls)));
        a.mov(x86::qword_ptr(c_p, offsetof(Process, def_arg_reg[5])),
              FCALLS.r64());

#ifdef DEBUG
        a.mov(x86::qword_ptr(c_p, offsetof(Process, debug_reds_in)),
              FCALLS.r64());
#endif

        /* Check whether save calls is on */
        a.mov(ARG1, c_p);
        a.mov(ARG2, imm(ERTS_PSD_SAVED_CALLS_BUF));
        runtime_call<2>(erts_psd_get);

        /* Read the active code index, overriding it with
         * ERTS_SAVE_CALLS_CODE_IX when save_calls is enabled (RET != 0). */
        a.test(RET, RET);
        a.mov(ARG1, imm(&the_active_code_index));
        a.mov(ARG2, imm(ERTS_SAVE_CALLS_CODE_IX));
        a.mov(active_code_ix.r32(), x86::dword_ptr(ARG1));
        a.cmovnz(active_code_ix, ARG2);

        /* Start executing the Erlang process. Note that reductions have
         * already been set up above. */
        emit_leave_runtime<Update::eStack | Update::eHeap>();

        /* Check if we are just returning from a dirty nif/bif call and if so we
         * need to do a bit of cleaning up before continuing. */
        a.mov(RET, x86::qword_ptr(c_p, offsetof(Process, i)));
        a.cmp(x86::qword_ptr(RET), imm(op_call_nif_WWW));
        a.je(labels[dispatch_nif]);
        a.cmp(x86::qword_ptr(RET), imm(op_call_bif_W));
        a.je(labels[dispatch_bif]);
        a.jmp(RET);
    }

    /* Processes may jump to the exported entry points below, executing on the
     * Erlang stack when entering. These are separate from the `_local` labels
     * above as we don't want to worry about which stack we're on when the
     * cases overlap. */

    /* `ga->get_context_switch()`
     *
     * The *next* instruction pointer is provided in ARG3, and must be preceded
     * by an ErtsCodeMFA. */
    a.bind(labels[context_switch]);
    {
        emit_enter_runtime<Update::eStack | Update::eHeap>();

        a.jmp(context_switch_local);
    }

    /* `ga->get_context_switch_simplified()`
     *
     * The next instruction pointer is provided in ARG3, which does not need to
     * point past an ErtsCodeMFA as the process structure has already been
     * updated. */
    a.bind(labels[context_switch_simplified]);
    {
        emit_enter_runtime<Update::eStack | Update::eHeap>();

        a.jmp(context_switch_simplified_local);
    }

    /* `ga->get_do_schedule()`
     *
     * `c_p->i` must be set prior to jumping here. */
    a.bind(labels[do_schedule]);
    {
        emit_enter_runtime<Update::eStack | Update::eHeap>();

        a.jmp(do_schedule_local);
    }
}
