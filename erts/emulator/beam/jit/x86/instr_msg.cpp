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
#include "bif.h"
#include "code_ix.h"
#include "erl_proc_sig_queue.h"
#ifdef USE_VM_PROBES
#    include "dtrace-wrapper.h"
#endif
}

#ifdef ERTS_SUPPORT_OLD_RECV_MARK_INSTRS

static void recv_mark(Process *p) {
    /* inlined here... */
    erts_msgq_recv_marker_insert_bind(p, erts_old_recv_marker_id);
}

static void recv_mark_set(Process *p) {
    /* inlined here... */
    erts_msgq_recv_marker_set_save(p, erts_old_recv_marker_id);
}

void BeamModuleAssembler::emit_i_recv_mark() {
    /*
     * OLD INSTRUCTION: This instruction is to be removed
     *                  in OTP 26.
     *
     * Save the current end of message queue
     */
    emit_enter_runtime();

    a.mov(ARG1, c_p);
    runtime_call<1>(recv_mark);

    emit_leave_runtime();
}

void BeamModuleAssembler::emit_i_recv_set() {
    /*
     * OLD INSTRUCTION: This instruction is to be removed
     *                  in OTP 26.
     *
     * If previously saved recv mark, set save pointer to it
     */
    emit_enter_runtime();

    a.mov(ARG1, c_p);
    runtime_call<1>(recv_mark_set);

    emit_leave_runtime();
}

#endif /* ERTS_SUPPORT_OLD_RECV_MARK_INSTRS */

void BeamModuleAssembler::emit_recv_marker_reserve(const ArgVal &Dst) {
    emit_enter_runtime();

    a.mov(ARG1, c_p);
    runtime_call<1>(erts_msgq_recv_marker_insert);

    emit_leave_runtime();

    mov_arg(Dst, RET);
}

void BeamModuleAssembler::emit_recv_marker_bind(const ArgVal &Marker,
                                                const ArgVal &Reference) {
    mov_arg(ARG2, Marker);
    mov_arg(ARG3, Reference);

    emit_enter_runtime();

    a.mov(ARG1, c_p);
    runtime_call<3>(erts_msgq_recv_marker_bind);

    emit_leave_runtime();
}

void BeamModuleAssembler::emit_recv_marker_clear(const ArgVal &Reference) {
    mov_arg(ARG2, Reference);

    emit_enter_runtime();

    a.mov(ARG1, c_p);
    runtime_call<2>(erts_msgq_recv_marker_clear);

    emit_leave_runtime();
}

void BeamModuleAssembler::emit_recv_marker_use(const ArgVal &Reference) {
    mov_arg(ARG2, Reference);

    emit_enter_runtime();

    a.mov(ARG1, c_p);
    runtime_call<2>(erts_msgq_recv_marker_set_save);

    emit_leave_runtime();
}

#ifdef ERTS_ENABLE_LOCK_CHECK
int erts_lc_proc_sig_receive_helper(Process *c_p,
                                    int fcalls,
                                    int neg_o_reds,
                                    ErtsMessage **msgpp,
                                    int *get_outp) {
    int res;
    /*
     * erts_proc_sig_receive_helper() may temporarliy release
     * its own main lock...
     */
    ERTS_UNREQ_PROC_MAIN_LOCK(c_p);
    res = erts_proc_sig_receive_helper(c_p,
                                       fcalls,
                                       neg_o_reds,
                                       msgpp,
                                       get_outp);
    ERTS_REQ_PROC_MAIN_LOCK(c_p);
    return res;
}
#endif

void BeamGlobalAssembler::emit_i_loop_rec_shared() {
    Label restart = a.newLabel(), peek_message = a.newLabel(),
          schedule_out = a.newLabel(), check_is_distributed = a.newLabel(),
          done = a.newLabel();

    x86::Mem await_addr = TMP_MEM1q, message_ptr = TMP_MEM2q,
             get_out = TMP_MEM3d;

    emit_enter_frame();

    a.or_(x86::dword_ptr(c_p, offsetof(Process, flags)), imm(F_DELAY_GC));
    a.mov(x86::qword_ptr(c_p, offsetof(Process, i)), ARG1);
    a.mov(await_addr, ARG2);

    a.bind(restart);
    {
        a.test(FCALLS, FCALLS);
        a.jle(schedule_out);

        /* !! FALL THROUGH !! */
    }

    comment("Peek next message");
    a.bind(peek_message);
    {
        a.mov(ARG1, x86::qword_ptr(c_p, offsetof(Process, sig_qs.save)));
        a.mov(ARG1, x86::qword_ptr(ARG1));
        a.test(ARG1, ARG1);
        a.jne(check_is_distributed);

        comment("Inner queue empty, fetch more from outer/middle queues");

        emit_enter_runtime<Update::eReductions | Update::eStack |
                           Update::eHeap>();

        a.mov(message_ptr, imm(0));
        a.mov(ARG1, c_p);
        a.mov(ARG2, FCALLS);
        mov_imm(ARG3, 0);
        a.lea(ARG4, message_ptr);
        a.lea(ARG5, get_out);
#ifdef ERTS_ENABLE_LOCK_CHECK
        runtime_call<5>(erts_lc_proc_sig_receive_helper);
#else
        runtime_call<5>(erts_proc_sig_receive_helper);
#endif

        /* erts_proc_sig_receive_helper merely inspects FCALLS, so we don't
         * need to update it here.
         *
         * Also note that another process may have loaded new code and sent us
         * a message to notify us about it, so we must update the active code
         * index. */
        emit_leave_runtime<Update::eStack | Update::eHeap |
                           Update::eCodeIndex>();

        a.sub(FCALLS, RET);

        /* Need to spill message_ptr to ARG1 as check_is_distributed uses it */
        a.mov(ARG1, message_ptr);
        a.test(ARG1, ARG1);
        /* NOTE: Short won't reach if JIT_HARD_DEBUG is defined. */
        a.jne(check_is_distributed);

        /* Did we receive a signal or run out of reds? */
        a.cmp(get_out, imm(0));
        a.short_().jne(schedule_out);

        /* The queue is empty and we're not yielding or exiting, so we'll jump
         * to our wait/timeout instruction.
         *
         * Note that the message queue lock is still held in this case. */
        a.and_(x86::dword_ptr(c_p, offsetof(Process, flags)), imm(~F_DELAY_GC));

        emit_unwind_frame();
        a.jmp(await_addr);
    }

    a.bind(schedule_out);
    {
        /* We either ran out of reductions or received an exit signal; schedule
         * ourselves out. The yield address (`c_p->i`) was set on ingress. */
        a.and_(x86::dword_ptr(c_p, offsetof(Process, flags)), imm(~F_DELAY_GC));
        a.mov(x86::qword_ptr(c_p, offsetof(Process, arity)), imm(0));
        a.mov(x86::qword_ptr(c_p, offsetof(Process, current)), imm(0));

        emit_unwind_frame();
        a.jmp(labels[do_schedule]);
    }

    comment("Check if message is distributed");
    a.bind(check_is_distributed);
    {
        a.cmp(x86::qword_ptr(ARG1, offsetof(ErtsSignal, common.tag)),
              imm(THE_NON_VALUE));
        /* NOTE: Short won't reach if JIT_HARD_DEBUG is defined. */
        a.jne(done);

        a.sub(FCALLS, imm(10));

        emit_enter_runtime();

        a.mov(ARG2, ARG1);
        a.mov(ARG1, c_p);
        runtime_call<2>(beam_jit_decode_dist);

        emit_leave_runtime();

        a.test(RET, RET);
        a.je(restart);

        a.mov(ARG1, RET);
        /* !! FALL THROUGH !! */
    }

    a.bind(done);
    {
        a.mov(ARG1, x86::qword_ptr(ARG1, offsetof(ErtsMessage, m[0])));
        a.mov(getXRef(0), ARG1);

        emit_leave_frame();
        a.ret();
    }
}

void BeamModuleAssembler::emit_i_loop_rec(const ArgVal &Wait) {
    Label entry = a.newLabel();

    align_erlang_cp();
    a.bind(entry);

    a.lea(ARG1, x86::qword_ptr(entry));
    a.lea(ARG2, x86::qword_ptr(labels[Wait.getValue()]));
    fragment_call(ga->get_i_loop_rec_shared());
}

void BeamModuleAssembler::emit_remove_message() {
    /* HTOP and E are passed explicitly and only read from, so we don't need to
     * swap them out. */
    a.mov(ARG3, HTOP);
    a.mov(ARG4, E);

    emit_enter_runtime();

    a.mov(ARG1, c_p);
    a.mov(ARG2, FCALLS);
    a.mov(ARG5, active_code_ix);
    runtime_call<5>(beam_jit_remove_message);
    a.mov(FCALLS, RET);

    emit_leave_runtime();
}

void BeamModuleAssembler::emit_loop_rec_end(const ArgVal &Dest) {
    emit_enter_runtime();

    a.mov(ARG1, c_p);
    runtime_call<1>(erts_msgq_set_save_next);

    emit_leave_runtime();

    a.dec(FCALLS);
    a.jmp(labels[Dest.getValue()]);
}

void BeamModuleAssembler::emit_wait_unlocked(const ArgVal &Dest) {
    emit_enter_runtime();

    a.mov(ARG1, c_p);
    a.lea(ARG2, x86::qword_ptr(labels[Dest.getValue()]));
    runtime_call<2>(beam_jit_wait_unlocked);

    emit_leave_runtime();

    abs_jmp(ga->get_do_schedule());
}

void BeamModuleAssembler::emit_wait_locked(const ArgVal &Dest) {
    emit_enter_runtime();

    a.mov(ARG1, c_p);
    a.lea(ARG2, x86::qword_ptr(labels[Dest.getValue()]));
    runtime_call<2>(beam_jit_wait_locked);

    emit_leave_runtime();

    abs_jmp(ga->get_do_schedule());
}

void BeamModuleAssembler::emit_wait_timeout_unlocked(const ArgVal &Src,
                                                     const ArgVal &Dest) {
    emit_enter_runtime();

    a.mov(ARG1, c_p);
    runtime_call<1>(beam_jit_take_receive_lock);

    emit_leave_runtime();

    emit_wait_timeout_locked(Src, Dest);
}

void BeamModuleAssembler::emit_wait_timeout_locked(const ArgVal &Src,
                                                   const ArgVal &Dest) {
    Label wait = a.newLabel(), next = a.newLabel();

    mov_arg(ARG2, Src);

    emit_enter_runtime();

    a.mov(ARG1, c_p);
    a.lea(ARG3, x86::qword_ptr(next));
    runtime_call<3>(beam_jit_wait_timeout);

    emit_leave_runtime();

    ERTS_CT_ASSERT(RET_next < RET_wait && RET_wait < RET_badarg);
    a.cmp(RET, RET_wait);
    a.short_().je(wait);
#ifdef JIT_HARD_DEBUG
    a.jl(next);
#else
    a.short_().jl(next);
#endif

    emit_raise_exception(currLabel, (ErtsCodeMFA *)nullptr);

    a.bind(wait);
    emit_wait_locked(Dest);

    align_erlang_cp();
    a.bind(next);
}

void BeamModuleAssembler::emit_timeout_locked() {
    emit_enter_runtime();

    a.mov(ARG1, c_p);
    runtime_call<1>(beam_jit_timeout_locked);

    emit_leave_runtime();
}

void BeamModuleAssembler::emit_timeout() {
    emit_enter_runtime();

    a.mov(ARG1, c_p);
    runtime_call<1>(beam_jit_timeout);

    emit_leave_runtime();
}
