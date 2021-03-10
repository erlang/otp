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

static ErtsMessage *decode_dist(Process *c_p, ErtsMessage *msgp) {
    if (!erts_proc_sig_decode_dist(c_p, ERTS_PROC_LOCK_MAIN, msgp, 0)) {
        /*
         * A corrupt distribution message that we weren't able to decode;
         * remove it...
         */

        /* TODO: Add DTrace probe for this bad message situation? */
        erts_msgq_unlink_msg(c_p, msgp);
        msgp->next = NULL;
        erts_cleanup_messages(msgp);

        return nullptr;
    }

    return msgp;
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
        a.short_().jne(check_is_distributed);

        /* Did we receive a signal or run out of reds? */
        a.cmp(get_out, imm(0));
        a.short_().jne(schedule_out);

        /* The queue is empty and we're not yielding or exiting, so we'll jump
         * to our wait/timeout instruction.
         *
         * Note that the message queue lock is still held in this case. */
        a.and_(x86::dword_ptr(c_p, offsetof(Process, flags)), imm(~F_DELAY_GC));

        emit_discard_cp();
        a.jmp(await_addr);
    }

    a.bind(schedule_out);
    {
        /* We either ran out of reductions or received an exit signal; schedule
         * ourselves out. The yield address (`c_p->i`) was set on ingress. */
        a.and_(x86::dword_ptr(c_p, offsetof(Process, flags)), imm(~F_DELAY_GC));
        a.mov(x86::qword_ptr(c_p, offsetof(Process, arity)), imm(0));
        a.mov(x86::qword_ptr(c_p, offsetof(Process, current)), imm(0));

        emit_discard_cp();
        a.jmp(labels[do_schedule]);
    }

    comment("Check if message is distributed");
    a.bind(check_is_distributed);
    {
        a.cmp(x86::qword_ptr(ARG1, offsetof(ErtsSignal, common.tag)),
              imm(THE_NON_VALUE));
        a.short_().jne(done);

        a.sub(FCALLS, imm(10));

        emit_enter_runtime();

        a.mov(ARG2, ARG1);
        a.mov(ARG1, c_p);
        runtime_call<2>(decode_dist);

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

/* Remove a (matched) message from the message queue. */
static Sint remove_message(Process *c_p,
                           Sint FCALLS,
                           Eterm *HTOP,
                           Eterm *E,
                           Uint32 active_code_ix) {
    ErtsMessage *msgp;

    ERTS_CHK_MBUF_SZ(c_p);

    if (active_code_ix == ERTS_SAVE_CALLS_CODE_IX) {
        save_calls(c_p, &exp_receive);
    }

    msgp = erts_msgq_peek_msg(c_p);

    if (ERL_MESSAGE_TOKEN(msgp) == NIL) {
#ifdef USE_VM_PROBES
        if (DT_UTAG(c_p) != NIL) {
            if (DT_UTAG_FLAGS(c_p) & DT_UTAG_PERMANENT) {
                SEQ_TRACE_TOKEN(c_p) = am_have_dt_utag;
            } else {
                DT_UTAG(c_p) = NIL;
                SEQ_TRACE_TOKEN(c_p) = NIL;
            }
        } else {
#endif
            SEQ_TRACE_TOKEN(c_p) = NIL;
#ifdef USE_VM_PROBES
        }
        DT_UTAG_FLAGS(c_p) &= ~DT_UTAG_SPREADING;
#endif
    } else if (ERL_MESSAGE_TOKEN(msgp) != am_undefined) {
        Eterm msg;
        SEQ_TRACE_TOKEN(c_p) = ERL_MESSAGE_TOKEN(msgp);
#ifdef USE_VM_PROBES
        if (ERL_MESSAGE_TOKEN(msgp) == am_have_dt_utag) {
            if (DT_UTAG(c_p) == NIL) {
                DT_UTAG(c_p) = ERL_MESSAGE_DT_UTAG(msgp);
            }
            DT_UTAG_FLAGS(c_p) |= DT_UTAG_SPREADING;
        } else {
#endif
            ASSERT(is_tuple(SEQ_TRACE_TOKEN(c_p)));
            ASSERT(SEQ_TRACE_TOKEN_ARITY(c_p) == 5);
            ASSERT(is_small(SEQ_TRACE_TOKEN_SERIAL(c_p)));
            ASSERT(is_small(SEQ_TRACE_TOKEN_LASTCNT(c_p)));
            ASSERT(is_small(SEQ_TRACE_TOKEN_FLAGS(c_p)));
            ASSERT(is_pid(SEQ_TRACE_TOKEN_SENDER(c_p)) ||
                   is_atom(SEQ_TRACE_TOKEN_SENDER(c_p)));
            c_p->seq_trace_lastcnt = unsigned_val(SEQ_TRACE_TOKEN_SERIAL(c_p));
            if (c_p->seq_trace_clock <
                unsigned_val(SEQ_TRACE_TOKEN_SERIAL(c_p))) {
                c_p->seq_trace_clock =
                        unsigned_val(SEQ_TRACE_TOKEN_SERIAL(c_p));
            }
            msg = ERL_MESSAGE_TERM(msgp);
            seq_trace_output(SEQ_TRACE_TOKEN(c_p),
                             msg,
                             SEQ_TRACE_RECEIVE,
                             c_p->common.id,
                             c_p);
#ifdef USE_VM_PROBES
        }
#endif
    }
#ifdef USE_VM_PROBES
    if (DTRACE_ENABLED(message_receive)) {
        Eterm token2 = NIL;
        DTRACE_CHARBUF(receiver_name, DTRACE_TERM_BUF_SIZE);
        Sint tok_label = 0;
        Sint tok_lastcnt = 0;
        Sint tok_serial = 0;
        Sint len = erts_proc_sig_privqs_len(c_p);

        dtrace_proc_str(c_p, receiver_name);
        token2 = SEQ_TRACE_TOKEN(c_p);
        if (have_seqtrace(token2)) {
            tok_label = SEQ_TRACE_T_DTRACE_LABEL(token2);
            tok_lastcnt = signed_val(SEQ_TRACE_T_LASTCNT(token2));
            tok_serial = signed_val(SEQ_TRACE_T_SERIAL(token2));
        }
        DTRACE6(message_receive,
                receiver_name,
                size_object(ERL_MESSAGE_TERM(msgp)),
                len, /* This is NOT message queue len, but its something... */
                tok_label,
                tok_lastcnt,
                tok_serial);
    }
#endif
    erts_msgq_unlink_msg(c_p, msgp);
    erts_msgq_set_save_first(c_p);
    CANCEL_TIMER(c_p);

    erts_save_message_in_proc(c_p, msgp);
    c_p->flags &= ~F_DELAY_GC;

    if (ERTS_IS_GC_DESIRED_INTERNAL(c_p, HTOP, E)) {
        /*
         * We want to GC soon but we leave a few
         * reductions giving the message some time
         * to turn into garbage.
         */
        ERTS_VBUMP_LEAVE_REDS_INTERNAL(c_p, 5, FCALLS);
    }

    ERTS_CHK_MBUF_SZ(c_p);

    ERTS_VERIFY_UNUSED_TEMP_ALLOC(c_p);
    return FCALLS;
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
    runtime_call<5>(remove_message);
    a.mov(FCALLS, RET);

    emit_leave_runtime();
}

static void save_message(Process *c_p) {
    erts_msgq_set_save_next(c_p);
}

void BeamModuleAssembler::emit_loop_rec_end(const ArgVal &Dest) {
    emit_enter_runtime();

    a.mov(ARG1, c_p);
    runtime_call<1>(save_message);

    emit_leave_runtime();

    a.dec(FCALLS);
    a.jmp(labels[Dest.getValue()]);
}

static void take_receive_lock(Process *c_p) {
    erts_proc_lock(c_p, ERTS_PROC_LOCKS_MSG_RECEIVE);
}

static void wait_locked(Process *c_p, ErtsCodePtr cp) {
    c_p->arity = 0;
    if (!ERTS_PTMR_IS_TIMED_OUT(c_p)) {
        erts_atomic32_read_band_relb(&c_p->state, ~ERTS_PSFLG_ACTIVE);
    }
    ASSERT(!ERTS_PROC_IS_EXITING(c_p));
    erts_proc_unlock(c_p, ERTS_PROC_LOCKS_MSG_RECEIVE);
    c_p->current = NULL;
    c_p->i = cp;
}

static void wait_unlocked(Process *c_p, ErtsCodePtr cp) {
    take_receive_lock(c_p);
    wait_locked(c_p, cp);
}

void BeamModuleAssembler::emit_wait_unlocked(const ArgVal &Dest) {
    emit_enter_runtime();

    a.mov(ARG1, c_p);
    a.lea(ARG2, x86::qword_ptr(labels[Dest.getValue()]));
    runtime_call<2>(wait_unlocked);

    emit_leave_runtime();

    abs_jmp(ga->get_do_schedule());
}

void BeamModuleAssembler::emit_wait_locked(const ArgVal &Dest) {
    emit_enter_runtime();

    a.mov(ARG1, c_p);
    a.lea(ARG2, x86::qword_ptr(labels[Dest.getValue()]));
    runtime_call<2>(wait_locked);

    emit_leave_runtime();

    abs_jmp(ga->get_do_schedule());
}

enum tmo_ret { RET_next = 0, RET_wait = 1, RET_badarg = 2 };

static enum tmo_ret wait_timeout(Process *c_p,
                                 Eterm timeout_value,
                                 ErtsCodePtr next) {
    /*
     * If we have already set the timer, we must NOT set it again.  Therefore,
     * we must test the F_INSLPQUEUE flag as well as the F_TIMO flag.
     */
    if ((c_p->flags & (F_INSLPQUEUE | F_TIMO)) == 0) {
        if (timeout_value == make_small(0)) {
            erts_proc_unlock(c_p, ERTS_PROC_LOCKS_MSG_RECEIVE);
            return RET_next;
        } else if (timeout_value == am_infinity) {
            c_p->flags |= F_TIMO;
        } else {
            int tres = erts_set_proc_timer_term(c_p, timeout_value);
            if (tres == 0) {
                /*
                 * The timer routiner will set c_p->i to the value in
                 * c_p->def_arg_reg[0].  Note that it is safe to use this
                 * location because there are no living x registers in
                 * a receive statement.
                 */
                c_p->def_arg_reg[0] = (Eterm)next;
            } else { /* Wrong time */
                erts_proc_unlock(c_p, ERTS_PROC_LOCKS_MSG_RECEIVE);
                c_p->freason = EXC_TIMEOUT_VALUE;
                return RET_badarg;
            }
        }
    }
    return RET_wait;
}

void BeamModuleAssembler::emit_wait_timeout_unlocked(const ArgVal &Src,
                                                     const ArgVal &Dest) {
    emit_enter_runtime();

    a.mov(ARG1, c_p);
    runtime_call<1>(take_receive_lock);

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
    runtime_call<3>(wait_timeout);

    emit_leave_runtime();

    ERTS_CT_ASSERT(RET_next < RET_wait && RET_wait < RET_badarg);
    a.cmp(RET, RET_wait);
    a.short_().je(wait);
    a.short_().jl(next);

    emit_handle_error(currLabel, (ErtsCodeMFA *)nullptr);

    a.bind(wait);
    emit_wait_locked(Dest);

    align_erlang_cp();
    a.bind(next);
}

static void timeout(Process *c_p) {
    if (IS_TRACED_FL(c_p, F_TRACE_RECEIVE)) {
        trace_receive(c_p, am_clock_service, am_timeout, NULL);
    }
    if (ERTS_PROC_GET_SAVED_CALLS_BUF(c_p)) {
        save_calls(c_p, &exp_timeout);
    }
    c_p->flags &= ~F_TIMO;
    erts_msgq_set_save_first(c_p);
}

static void timeout_locked(Process *c_p) {
    erts_proc_unlock(c_p, ERTS_PROC_LOCKS_MSG_RECEIVE);
    timeout(c_p);
}

void BeamModuleAssembler::emit_timeout_locked() {
    emit_enter_runtime();

    a.mov(ARG1, c_p);
    runtime_call<1>(timeout_locked);

    emit_leave_runtime();
}

void BeamModuleAssembler::emit_timeout() {
    emit_enter_runtime();

    a.mov(ARG1, c_p);
    runtime_call<1>(timeout);

    emit_leave_runtime();
}
