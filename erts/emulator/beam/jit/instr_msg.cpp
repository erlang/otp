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
}

static ErtsMessage *decode_dist(Process *c_p,
                                ErtsMessage *msgp,
                                Eterm *HTOP,
                                Eterm *E) {
    if (!erts_proc_sig_decode_dist(c_p, ERTS_PROC_LOCK_MAIN, msgp, 0)) {
        /*
         * A corrupt distribution message that we weren't able to decode;
         * remove it...
         */
        /* No swapin should be needed */
        ASSERT(HTOP == c_p->htop && E == c_p->stop);
        /* TODO: Add DTrace probe for this bad message situation? */
        UNLINK_MESSAGE(c_p, msgp);
        msgp->next = NULL;
        erts_cleanup_messages(msgp);
        return nullptr;
    }
    return msgp;
}

static void recv_mark_save(Process *p) {
    ERTS_RECV_MARK_SAVE(p);
}

static void recv_mark_set(Process *p) {
    ERTS_RECV_MARK_SET(p);
}

void BeamModuleAssembler::emit_i_recv_mark() {
    a.mov(ARG1, c_p);
    abs_call<1>(recv_mark_save);
}

void BeamModuleAssembler::emit_i_recv_set() {
    a.mov(ARG1, c_p);
    abs_call<1>(recv_mark_set);
}

void BeamGlobalAssembler::emit_i_loop_rec_shared() {
    Label restart = a.newLabel(), peek_message = a.newLabel(),
          schedule_out = a.newLabel(), check_is_distributed = a.newLabel(),
          done = a.newLabel();

    x86::Mem await_addr = TMP_MEM1q, message_ptr = TMP_MEM2q,
             get_out = dTMP3_MEM;

    emit_function_preamble();

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
        emit_heavy_swapout();

        a.mov(message_ptr, imm(0));
        a.mov(ARG1, c_p);
        a.mov(ARG2, FCALLS);
        a.sub(ARG3, ARG3);
        a.lea(ARG4, message_ptr);
        a.lea(ARG5, get_out);
        abs_call<5>(erts_proc_sig_receive_helper);

        a.sub(FCALLS, RET);
        emit_swapin();

        /* Another process may have loaded new code and sent us a message to
         * notify us about it, so we must update the active code index. */
        emit_update_code_index();

        /* Need to spill message_ptr to ARG1 as check_is_distributed uses it */
        a.mov(ARG1, message_ptr);
        a.test(ARG1, ARG1);
        a.jne(check_is_distributed);

        a.cmp(get_out, 0);

        /* We either ran out of reductions or received an exit signal; schedule
         * ourselves out. */
        a.jne(schedule_out);

        /* The queue is empty and we're not yielding or exiting, so we'll jump
         * to our wait/timeout instruction.
         *
         * Note that the message queue lock is still held in this case. */
        a.and_(x86::dword_ptr(c_p, offsetof(Process, flags)), imm(~F_DELAY_GC));

        emit_function_postamble();
        a.add(x86::rsp, 8);
        a.jmp(await_addr);
    }

    a.bind(schedule_out);
    {
        emit_swapout();

        a.and_(x86::dword_ptr(c_p, offsetof(Process, flags)), imm(~F_DELAY_GC));
        a.mov(x86::qword_ptr(c_p, offsetof(Process, arity)), imm(0));
        a.mov(x86::qword_ptr(c_p, offsetof(Process, current)), imm(0));

        emit_function_postamble();
        a.add(x86::rsp, imm(8));
        a.jmp(labels[do_schedule]);
    }

    comment("Check if message is distributed");
    a.bind(check_is_distributed);
    {
        a.cmp(x86::qword_ptr(ARG1, offsetof(ErtsSignal, common.tag)),
              imm(THE_NON_VALUE));
        a.jne(done);

        a.sub(FCALLS, imm(10));
        emit_swapout();

        a.mov(ARG2, ARG1);
        a.mov(ARG1, c_p);
#ifdef DEBUG
        a.mov(ARG3, HTOP);
        a.mov(ARG4, E);
#endif
        abs_call<4>(decode_dist);
        a.test(RET, RET);
        a.je(restart);

        a.mov(ARG1, RET);
        emit_swapin();

        /* !! FALL THROUGH !! */
    }

    a.bind(done);
    {
        a.mov(ARG1, x86::qword_ptr(ARG1, offsetof(ErtsMessage, m[0])));
        a.mov(getXRef(0), ARG1);

        emit_function_postamble();
        a.ret();
    }
}

void BeamModuleAssembler::emit_i_loop_rec(const ArgVal &Wait) {
    Label entry = a.newLabel();

    a.align(kAlignCode, 8);
    a.bind(entry);

    a.lea(ARG1, x86::qword_ptr(entry));
    a.lea(ARG2, x86::qword_ptr(labels[Wait.getValue()]));
    aligned_call(ga->get_i_loop_rec_shared());
}

/* Remove a (matched) message from the message queue. */
static Sint remove_message(Process *c_p,
                           Sint FCALLS,
                           Eterm *HTOP,
                           Eterm *E,
                           UWord active_code_ix) {
    ErtsMessage *msgp;

    ERTS_CHK_MBUF_SZ(c_p);

    if (active_code_ix == ERTS_SAVE_CALLS_CODE_IX) {
        save_calls(c_p, &exp_receive);
    }

    msgp = PEEK_MESSAGE(c_p);

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
    UNLINK_MESSAGE(c_p, msgp);
    JOIN_MESSAGE(c_p);
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
    a.mov(ARG1, c_p);
    a.mov(ARG2, FCALLS);
    a.mov(ARG3, HTOP);
    a.mov(ARG4, E);
    a.mov(ARG5, active_code_ix);
    abs_call<5>(remove_message);
    a.mov(FCALLS, RET);
}

void BeamModuleAssembler::emit_loop_rec_end(const ArgVal &Dest) {
    ERTS_CT_ASSERT(0 == offsetof(ErtsMessage, next));
    a.mov(ARG1, x86::qword_ptr(c_p, offsetof(Process, sig_qs.save)));
    a.mov(ARG1, x86::qword_ptr(ARG1));
    a.mov(x86::qword_ptr(c_p, offsetof(Process, sig_qs.save)), ARG1);
    a.dec(FCALLS);
    a.jmp(labels[Dest.getValue()]);
}

static void take_receive_lock(Process *c_p) {
    erts_proc_lock(c_p, ERTS_PROC_LOCKS_MSG_RECEIVE);
}

void BeamModuleAssembler::emit_wait_unlocked(const ArgVal &Dest) {
    a.mov(ARG1, c_p);
    abs_call<1>(take_receive_lock);

    emit_wait_locked(Dest);
}

static void wait_locked(Process *c_p, BeamInstr *cp) {
    c_p->arity = 0;
    if (!ERTS_PTMR_IS_TIMED_OUT(c_p)) {
        erts_atomic32_read_band_relb(&c_p->state, ~ERTS_PSFLG_ACTIVE);
    }
    ASSERT(!ERTS_PROC_IS_EXITING(c_p));
    erts_proc_unlock(c_p, ERTS_PROC_LOCKS_MSG_RECEIVE);
    c_p->current = NULL;
    c_p->i = cp;
}

void BeamModuleAssembler::emit_wait_locked(const ArgVal &Dest) {
    a.mov(ARG1, c_p);
    a.lea(ARG2, x86::qword_ptr(labels[Dest.getValue()]));
    abs_call<2>(wait_locked);
    emit_swapout();
    abs_jmp(ga->get_do_schedule());
}

enum tmo_ret { RET_next = 0, RET_wait = 1, RET_badarg = 2 };

static enum tmo_ret wait_timeout(Process *c_p,
                                 Eterm timeout_value,
                                 BeamInstr *next) {
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
                BeamInstr **pi = (BeamInstr **)c_p->def_arg_reg;
                *pi = next;
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
    a.mov(ARG1, c_p);
    abs_call<1>(take_receive_lock);

    emit_wait_timeout_locked(Src, Dest);
}

void BeamModuleAssembler::emit_wait_timeout_locked(const ArgVal &Src,
                                                   const ArgVal &Dest) {
    Label wait = a.newLabel(), next = a.newLabel();

    a.mov(ARG1, c_p);
    mov_arg(ARG2, Src);
    a.lea(ARG3, x86::qword_ptr(next));
    abs_call<3>(wait_timeout);

    ERTS_CT_ASSERT(RET_next < RET_wait && RET_wait < RET_badarg);
    a.cmp(RET, RET_wait);
    a.je(wait);
    a.jl(next);

    emit_handle_error(currLabel, (ErtsCodeMFA *)nullptr);

    a.bind(wait);
    emit_wait_locked(Dest);

    a.align(kAlignCode, 8);
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
    JOIN_MESSAGE(c_p);
}

static void timeout_locked(Process *c_p) {
    erts_proc_unlock(c_p, ERTS_PROC_LOCKS_MSG_RECEIVE);
    timeout(c_p);
}

void BeamModuleAssembler::emit_timeout_locked() {
    a.mov(ARG1, c_p);
    abs_call<1>(timeout_locked);
}

void BeamModuleAssembler::emit_timeout() {
    a.mov(ARG1, c_p);
    abs_call<1>(timeout);
}
