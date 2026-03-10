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
#include "bif.h"
#include "code_ix.h"
#include "erl_proc_sig_queue.h"
#ifdef USE_VM_PROBES
#    include "dtrace-wrapper.h"
#endif
}

void BeamModuleAssembler::emit_recv_marker_reserve(const ArgRegister &Dst) {
    // TODO
    emit_nyi("emit_recv_marker_reserve");
}

void BeamModuleAssembler::emit_recv_marker_bind(const ArgRegister &Marker,
                                                const ArgRegister &Reference) {
    // TODO
    emit_nyi("emit_recv_marker_bind");
}

void BeamModuleAssembler::emit_recv_marker_clear(const ArgRegister &Reference) {
    // TODO
    emit_nyi("emit_recv_marker_clear");
}

void BeamModuleAssembler::emit_recv_marker_use(const ArgRegister &Reference) {
    // TODO
    emit_nyi("emit_recv_marker_use");
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

    arm::Mem await_addr = TMP_MEM1q, message_ptr = TMP_MEM2q,
             get_out = TMP_MEM3q, saved_lr = TMP_MEM4q;
    arm::Mem flags = arm::Mem(c_p, offsetof(Process, flags));

    a.str(a32::lr, saved_lr);

    a.ldr(TMP, flags);
    a.orr(TMP, TMP, imm(F_DELAY_GC));
    a.str(TMP, flags);
    a.str(ARG1, arm::Mem(c_p, offsetof(Process, i)));
    a.str(ARG2, await_addr);

    a.bind(restart);
    {
        a.tst(FCALLS, FCALLS);
        a.b_le(schedule_out);

        /* !! FALL THROUGH !! */
    }

    comment("Peek next message");
    a.bind(peek_message);
    {
        a.ldr(TMP, arm::Mem(c_p, offsetof(Process, sig_qs.save)));
        a.ldr(ARG1, arm::Mem(TMP));
        emit_branch_if_ne(ARG1, 0, check_is_distributed);
        comment("Inner queue empty, fetch more from outer/middle queues");

        emit_enter_runtime<Update::eReductions | Update::eHeapAlloc |
                           Update::eHeap>();

        mov_imm(TMP, 0);
        a.str(TMP, message_ptr);
        a.mov(ARG1, c_p);
        a.mov(ARG2, FCALLS);
        mov_imm(ARG3, 0);
        lea(ARG4, message_ptr);
        lea(TMP, get_out);
        a.sub(a32::sp, a32::sp, imm(8));
        a.str(TMP, arm::Mem(a32::sp, 0));
#ifdef ERTS_ENABLE_LOCK_CHECK
        runtime_call<5>(erts_lc_proc_sig_receive_helper);
#else
        runtime_call<5>(erts_proc_sig_receive_helper);
#endif
        a.add(a32::sp, a32::sp, imm(8));

        /* erts_proc_sig_receive_helper merely inspects FCALLS, so we don't
         * need to update it here.
         *
         * Also note that another process may have loaded new code and sent us
         * a message to notify us about it, so we must update the active code
         * index. */
        emit_leave_runtime<Update::eHeapAlloc | Update::eCodeIndex>();

        a.sub(FCALLS, FCALLS, ARG1);

        /* Need to spill message_ptr to ARG1 as check_is_distributed uses it. */
        a.ldr(ARG1, message_ptr);
        emit_branch_if_ne(ARG1, 0, check_is_distributed);

        /* Did we receive a signal or run out of reds? */
        a.ldr(TMP, get_out);
        emit_branch_if_ne(TMP, 0, schedule_out);

        /* The queue is empty and we're not yielding or exiting, so we'll jump
         * to our wait/timeout instruction.
         *
         * Note that the message queue lock is still held in this case. */
        a.ldr(TMP, flags);
        a.bic(TMP, TMP, imm(F_DELAY_GC));
        a.str(TMP, flags);

        a.ldr(TMP, await_addr);
        a.bx(TMP);
    }

    a.bind(schedule_out);
    {
        /* We either ran out of reductions or received an exit signal; schedule
         * ourselves out. The yield address (`c_p->i`) was set on ingress. */
        a.ldr(TMP, flags);
        a.bic(TMP, TMP, imm(F_DELAY_GC));
        a.str(TMP, flags);
        mov_imm(TMP, 0);
        a.strb(TMP, arm::Mem(c_p, offsetof(Process, arity)));
        a.str(TMP, arm::Mem(c_p, offsetof(Process, current)));

        a.b(labels[do_schedule]);
    }

    /*
     * ARG1 now contains the pointer to a message.
     */
    comment("Check if message is distributed");
    a.bind(check_is_distributed);
    {
        a.ldr(TMP, arm::Mem(ARG1, offsetof(ErtsSignal, common.tag)));
        emit_branch_if_value(TMP, done);

        sub(FCALLS, FCALLS, 10);

        emit_enter_runtime<>();

        a.mov(ARG2, ARG1);
        a.mov(ARG1, c_p);
        runtime_call<2>(beam_jit_decode_dist);

        emit_leave_runtime<>();

        emit_branch_if_eq(ARG1, 0, restart);

        /* !! FALL THROUGH !! */
    }

    a.bind(done);
    {
        a.ldr(TMP, arm::Mem(ARG1, offsetof(ErtsMessage, m[0])));
        a.str(TMP, getXRef(0)); // return message pointer in X0
        a.ldr(a32::lr, saved_lr);
        a.bx(a32::lr);
    }
}

void BeamModuleAssembler::emit_i_loop_rec(const ArgLabel &Wait) {
    Label entry = a.newLabel();

    a.bind(entry);
    a.adr(ARG1, entry);
    a.ldr(ARG2, embed_constant(Wait, disp4KB));
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
    a.mov(TMP, active_code_ix);

    a.sub(a32::sp, a32::sp, imm(8));
    a.str(TMP, arm::Mem(a32::sp, 0));
    runtime_call<5>(beam_jit_remove_message);
    a.add(a32::sp, a32::sp, imm(8));

    a.mov(FCALLS, ARG1);

    emit_leave_runtime();
}

void BeamModuleAssembler::emit_loop_rec_end(const ArgLabel &Dest) {
    // TODO
    emit_nyi("emit_loop_rec_end");
}

void BeamModuleAssembler::emit_wait_unlocked(const ArgLabel &Dest) {
    // TODO
    emit_nyi("emit_wait_unlocked");
}

void BeamModuleAssembler::emit_wait_locked(const ArgLabel &Dest) {
    emit_enter_runtime();

    a.mov(ARG1, c_p);
    a.ldr(ARG2, embed_constant(Dest, disp4KB));
    runtime_call<2>(beam_jit_wait_locked);

    emit_leave_runtime();

    a.b(resolve_fragment(ga->get_do_schedule(), disp32MB));

    /* Must check stubs here because this branch is followed by
     * a label when part of `wait_timeout_locked`. */
    mark_unreachable_check_pending_stubs();
}

void BeamModuleAssembler::emit_wait_timeout_unlocked(const ArgSource &Src,
                                                     const ArgLabel &Dest) {
    // TODO
    emit_nyi("emit_wait_timeout_unlocked");
}

void BeamModuleAssembler::emit_wait_timeout_locked(const ArgSource &Src,
                                                   const ArgLabel &Dest) {
    // TODO
    emit_nyi("emit_wait_timeout_locked");
}

void BeamModuleAssembler::emit_timeout_locked() {
    // TODO
    emit_nyi("emit_timeout_locked");
}

void BeamModuleAssembler::emit_timeout() {
    // TODO
    emit_nyi("emit_timeout");
}
