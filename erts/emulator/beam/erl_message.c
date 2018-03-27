/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1997-2018. All Rights Reserved.
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
/*
 * Message passing primitives.
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "erl_vm.h"
#include "global.h"
#include "erl_message.h"
#include "erl_process.h"
#include "erl_binary.h"
#include "dtrace-wrapper.h"
#include "beam_bp.h"
#include "erl_proc_sig_queue.h"

ERTS_SCHED_PREF_QUICK_ALLOC_IMPL(message_ref,
				 ErtsMessageRef,
				 ERL_MESSAGE_BUF_SZ,
				 ERTS_ALC_T_MSG_REF)

#if defined(DEBUG) && 0
#define HARD_DEBUG
#else
#undef HARD_DEBUG
#endif

void
init_message(void)
{
    init_message_ref_alloc();
}

void *erts_alloc_message_ref(void)
{
    return (void *) message_ref_alloc();
}

void erts_free_message_ref(void *mp)
{
    message_ref_free((ErtsMessageRef *) mp);
}

/* Allocate message buffer (size in words) */
ErlHeapFragment*
new_message_buffer(Uint size)
{
    ErlHeapFragment* bp;
    bp = (ErlHeapFragment*) ERTS_HEAP_ALLOC(ERTS_ALC_T_HEAP_FRAG,
					    ERTS_HEAP_FRAG_SIZE(size));
    ERTS_INIT_HEAP_FRAG(bp, size, size);
    VERBOSE(DEBUG_SHCOPY, ("[pid=%T] new message buffer %p\n", erts_get_current_pid(), bp->mem));
    return bp;
}

ErlHeapFragment*
erts_resize_message_buffer(ErlHeapFragment *bp, Uint size,
			   Eterm *brefs, Uint brefs_size)
{
#ifdef DEBUG
    int i;
#endif
#ifdef HARD_DEBUG
    ErlHeapFragment *dbg_bp;
    Eterm *dbg_brefs;
    Uint dbg_size;
    Uint dbg_tot_size;
    Eterm *dbg_hp;
#endif
    ErlHeapFragment* nbp;

#ifdef DEBUG
    {
	Uint off_sz = size < bp->used_size ? size : bp->used_size;
	for (i = 0; i < brefs_size; i++) {
	    Eterm *ptr;
	    if (is_immed(brefs[i]))
		continue;
	    ptr = ptr_val(brefs[i]);
	    ASSERT(&bp->mem[0] <= ptr && ptr < &bp->mem[0] + off_sz);

	}
    }
#endif

    if (size >= (bp->used_size - bp->used_size / 16)) {
        bp->used_size = size;
	return bp;
    }

#ifdef HARD_DEBUG
    dbg_brefs = erts_alloc(ERTS_ALC_T_UNDEF, sizeof(Eterm *)*brefs_size);
    dbg_bp = new_message_buffer(bp->used_size);
    dbg_hp = dbg_bp->mem;
    dbg_tot_size = 0;
    for (i = 0; i < brefs_size; i++) {
	dbg_size = size_object(brefs[i]);
	dbg_tot_size += dbg_size;
	dbg_brefs[i] = copy_struct(brefs[i], dbg_size, &dbg_hp,
				   &dbg_bp->off_heap);
    }
    ASSERT(dbg_tot_size == (size < bp->used_size ? size : bp->used_size));
#endif

    nbp = (ErlHeapFragment*) ERTS_HEAP_REALLOC(ERTS_ALC_T_HEAP_FRAG,
					       (void *) bp,
					       ERTS_HEAP_FRAG_SIZE(bp->alloc_size),
					       ERTS_HEAP_FRAG_SIZE(size));
    if (bp != nbp) {
	Uint off_sz = size < nbp->used_size ? size : nbp->used_size;
	Eterm *sp = &bp->mem[0];
	Eterm *ep = sp + off_sz;
	Sint offs = &nbp->mem[0] - sp;
	erts_offset_off_heap(&nbp->off_heap, offs, sp, ep);
	erts_offset_heap(&nbp->mem[0], off_sz, offs, sp, ep);
	if (brefs && brefs_size)
	    erts_offset_heap_ptr(brefs, brefs_size, offs, sp, ep);
#ifdef DEBUG
	for (i = 0; i < brefs_size; i++) {
	    Eterm *ptr;
	    if (is_immed(brefs[i]))
		continue;
	    ptr = ptr_val(brefs[i]);
	    ASSERT(&nbp->mem[0] <= ptr && ptr < &nbp->mem[0] + off_sz);
	}
#endif
    }
    nbp->alloc_size = size;
    nbp->used_size = size;

#ifdef HARD_DEBUG
    for (i = 0; i < brefs_size; i++)
	ASSERT(eq(dbg_brefs[i], brefs[i]));
    free_message_buffer(dbg_bp);
    erts_free(ERTS_ALC_T_UNDEF, dbg_brefs);
#endif

    return nbp;
}


void
erts_cleanup_offheap(ErlOffHeap *offheap)
{
    union erl_off_heap_ptr u;

    for (u.hdr = offheap->first; u.hdr; u.hdr = u.hdr->next) {
	switch (thing_subtag(u.hdr->thing_word)) {
	case REFC_BINARY_SUBTAG:
            erts_bin_release(u.pb->val);
	    break;
	case FUN_SUBTAG:
	    if (erts_refc_dectest(&u.fun->fe->refc, 0) == 0) {
		erts_erase_fun_entry(u.fun->fe);		    
	    }
	    break;
	case REF_SUBTAG:
	    ASSERT(is_magic_ref_thing(u.hdr));
            erts_bin_release((Binary *)u.mref->mb);
	    break;
	default:
	    ASSERT(is_external_header(u.hdr->thing_word));
	    erts_deref_node_entry(u.ext->node);
	    break;
	}
    }
}

void
free_message_buffer(ErlHeapFragment* bp)
{
    ASSERT(bp != NULL);
    do {
	ErlHeapFragment* next_bp = bp->next;

	erts_cleanup_offheap(&bp->off_heap);
	ERTS_HEAP_FREE(ERTS_ALC_T_HEAP_FRAG, (void *) bp,
		       ERTS_HEAP_FRAG_SIZE(bp->alloc_size));	
	bp = next_bp;
    }while (bp != NULL);
}

void
erts_cleanup_messages(ErtsMessage *msgp)
{
    ErtsMessage *mp = msgp;
    while (mp) {
	ErtsMessage *fmp;
	ErlHeapFragment *bp;
	if (ERTS_SIG_IS_EXTERNAL_MSG(mp)) {
	    if (is_not_immed(ERL_MESSAGE_TOKEN(mp))) {
		bp = (ErlHeapFragment *) mp->data.dist_ext->ext_endp;
		erts_cleanup_offheap(&bp->off_heap);
	    }
	    if (mp->data.dist_ext)
		erts_free_dist_ext_copy(mp->data.dist_ext);
	}
        else {
	    if (ERTS_SIG_IS_INTERNAL_MSG(mp)
                && mp->data.attached != ERTS_MSG_COMBINED_HFRAG) {
		bp = mp->data.heap_frag;
            }
	    else {
                mp->data.attached = ERTS_MSG_COMBINED_HFRAG;
		bp = mp->hfrag.next;
		erts_cleanup_offheap(&mp->hfrag.off_heap);
	    }
	    if (bp)
		free_message_buffer(bp);
	}
	fmp = mp;
	mp = mp->next;
	erts_free_message(fmp);
    }
}

ErtsMessage *
erts_realloc_shrink_message(ErtsMessage *mp, Uint sz, Eterm *brefs, Uint brefs_size)
{
    ErtsMessage *nmp = erts_realloc(ERTS_ALC_T_MSG, mp,
				    sizeof(ErtsMessage) + (sz - 1)*sizeof(Eterm));
    if (nmp != mp) {
	Eterm *sp = &mp->hfrag.mem[0];
	Eterm *ep = sp + sz;
	Sint offs = &nmp->hfrag.mem[0] - sp;
	erts_offset_off_heap(&nmp->hfrag.off_heap, offs, sp, ep);
	erts_offset_heap(&nmp->hfrag.mem[0], sz, offs, sp, ep);
	if (brefs && brefs_size)
	    erts_offset_heap_ptr(brefs, brefs_size, offs, sp, ep);
    }

    nmp->hfrag.used_size = sz;
    nmp->hfrag.alloc_size = sz;

    return nmp;
}

void
erts_queue_dist_message(Process *rcvr,
			ErtsProcLocks rcvr_locks,
			ErtsDistExternal *dist_ext,
			Eterm token,
                        Eterm from)
{
    ErtsMessage* mp;
#ifdef USE_VM_PROBES
    Sint tok_label = 0;
    Sint tok_lastcnt = 0;
    Sint tok_serial = 0;
#endif
    erts_aint_t state;

    ERTS_LC_ASSERT(rcvr_locks == erts_proc_lc_my_proc_locks(rcvr));

    mp = erts_alloc_message(0, NULL);
    mp->data.dist_ext = dist_ext;

    ERL_MESSAGE_FROM(mp) = dist_ext->dep->sysname;
    ERL_MESSAGE_TERM(mp) = THE_NON_VALUE;
#ifdef USE_VM_PROBES
    ERL_MESSAGE_DT_UTAG(mp) = NIL;
    if (token == am_have_dt_utag)
	ERL_MESSAGE_TOKEN(mp) = NIL;
    else
#endif
	ERL_MESSAGE_TOKEN(mp) = token;

    if (!(rcvr_locks & ERTS_PROC_LOCK_MSGQ)) {
	if (erts_proc_trylock(rcvr, ERTS_PROC_LOCK_MSGQ) == EBUSY) {
	    ErtsProcLocks need_locks = ERTS_PROC_LOCK_MSGQ;
            ErtsProcLocks unlocks =
                rcvr_locks & ERTS_PROC_LOCKS_HIGHER_THAN(ERTS_PROC_LOCK_MSGQ);
	    if (unlocks) {
		erts_proc_unlock(rcvr, unlocks);
		need_locks |= unlocks;
	    }
	    erts_proc_lock(rcvr, need_locks);
	}
    }


    state = erts_atomic32_read_acqb(&rcvr->state);
    if (state & ERTS_PSFLG_EXITING) {
	if (!(rcvr_locks & ERTS_PROC_LOCK_MSGQ))
	    erts_proc_unlock(rcvr, ERTS_PROC_LOCK_MSGQ);
	/* Drop message if receiver is exiting or has a pending exit ... */
	erts_cleanup_messages(mp);
    }
    else {

#ifdef USE_VM_PROBES
        if (DTRACE_ENABLED(message_queued)) {
            DTRACE_CHARBUF(receiver_name, DTRACE_TERM_BUF_SIZE);

            dtrace_proc_str(rcvr, receiver_name);
            if (have_seqtrace(token)) {
                tok_label = SEQ_TRACE_T_DTRACE_LABEL(token);
                tok_lastcnt = signed_val(SEQ_TRACE_T_LASTCNT(token));
                tok_serial = signed_val(SEQ_TRACE_T_SERIAL(token));
            }
            /*
             * TODO: We don't know the real size of the external message here.
             *       -1 will appear to a D script as 4294967295.
             */
            DTRACE6(message_queued, receiver_name, -1, rcvr->sig_qs.len + 1,
                    tok_label, tok_lastcnt, tok_serial);
        }
#endif

	LINK_MESSAGE(rcvr, mp, &mp->next, 1);

	if (!(rcvr_locks & ERTS_PROC_LOCK_MSGQ))
	    erts_proc_unlock(rcvr, ERTS_PROC_LOCK_MSGQ);

	erts_proc_notify_new_message(rcvr, rcvr_locks);
    }
}

/* Add messages last in message queue */
static Sint
queue_messages(Process* receiver,
               erts_aint32_t *receiver_state,
               ErtsProcLocks receiver_locks,
               ErtsMessage* first,
               ErtsMessage** last,
               Uint len)
{
    Sint res;
    int locked_msgq = 0;
    erts_aint32_t state;

    ASSERT(is_value(ERL_MESSAGE_TERM(first)));
    ASSERT(is_value(ERL_MESSAGE_FROM(first)));
    ASSERT(ERL_MESSAGE_TOKEN(first) == am_undefined ||
           ERL_MESSAGE_TOKEN(first) == NIL ||
           is_tuple(ERL_MESSAGE_TOKEN(first)));

#ifdef ERTS_ENABLE_LOCK_CHECK
    ERTS_LC_ASSERT(erts_proc_lc_my_proc_locks(receiver) < ERTS_PROC_LOCK_MSGQ ||
                       receiver_locks == erts_proc_lc_my_proc_locks(receiver));
#endif

    if (!(receiver_locks & ERTS_PROC_LOCK_MSGQ)) {
	if (erts_proc_trylock(receiver, ERTS_PROC_LOCK_MSGQ) == EBUSY) {
            ErtsProcLocks need_locks;

	    if (receiver_state)
		state = *receiver_state;
	    else
		state = erts_atomic32_read_nob(&receiver->state);
	    if (state & ERTS_PSFLG_EXITING)
		goto exiting;

            need_locks = receiver_locks & ERTS_PROC_LOCKS_HIGHER_THAN(ERTS_PROC_LOCK_MSGQ);
	    if (need_locks) {
		erts_proc_unlock(receiver, need_locks);
	    }
            need_locks |= ERTS_PROC_LOCK_MSGQ;
	    erts_proc_lock(receiver, need_locks);
	}
	locked_msgq = 1;
    }


    state = erts_atomic32_read_nob(&receiver->state);

    if (state & ERTS_PSFLG_EXITING) {
    exiting:
	/* Drop message if receiver is exiting or has a pending exit... */
	if (locked_msgq)
	    erts_proc_unlock(receiver, ERTS_PROC_LOCK_MSGQ);
	erts_cleanup_messages(first);
	return 0;
    }

    res = receiver->sig_qs.len;
    if (receiver_locks & ERTS_PROC_LOCK_MAIN) {
	/*
	 * We move 'in queue' to 'private queue' and place
	 * message at the end of 'private queue' in order
	 * to ensure that the 'in queue' doesn't contain
	 * references into the heap. By ensuring this,
	 * we don't need to include the 'in queue' in
	 * the root set when garbage collecting.
	 */
	res += receiver->sig_inq.len;
        erts_proc_sig_fetch(receiver);
        LINK_MESSAGE_PRIVQ(receiver, first, last, len);
    }
    else
    {
	LINK_MESSAGE(receiver, first, last, len);
    }

    if (locked_msgq) {
	erts_proc_unlock(receiver, ERTS_PROC_LOCK_MSGQ);
    }

    erts_proc_notify_new_message(receiver, receiver_locks);
    return res;
}

static Sint
queue_message(Process* receiver,
              erts_aint32_t *receiver_state,
              ErtsProcLocks receiver_locks,
              ErtsMessage* mp, Eterm msg, Eterm from)
{
    ERL_MESSAGE_TERM(mp) = msg;
    ERL_MESSAGE_FROM(mp) = from;
    return queue_messages(receiver, receiver_state, receiver_locks,
                          mp, &mp->next, 1);
}

Sint
erts_queue_message(Process* receiver, ErtsProcLocks receiver_locks,
                   ErtsMessage* mp, Eterm msg, Eterm from)
{
    return queue_message(receiver, NULL, receiver_locks, mp, msg, from);
}


Sint
erts_queue_messages(Process* receiver, ErtsProcLocks receiver_locks,
                    ErtsMessage* first, ErtsMessage** last, Uint len)
{
    return queue_messages(receiver, NULL, receiver_locks,
                          first, last, len);
}

void
erts_link_mbuf_to_proc(Process *proc, ErlHeapFragment *first_bp)
{
    if (first_bp) {
	ErlHeapFragment *bp = first_bp;

	while (1) {
	    /* Move any off_heap's into the process */
	    if (bp->off_heap.first != NULL) {
		struct erl_off_heap_header** next_p = &bp->off_heap.first;
		while (*next_p != NULL) {
		    next_p = &((*next_p)->next);
		}
		*next_p = MSO(proc).first;
		MSO(proc).first = bp->off_heap.first;
		bp->off_heap.first = NULL;
		OH_OVERHEAD(&(MSO(proc)), bp->off_heap.overhead);
	    }
	    MBUF_SIZE(proc) += bp->used_size;
	    if (!bp->next)
		break;
	    bp = bp->next;
	}

	/* Link the message buffer */
	bp->next = MBUF(proc);
	MBUF(proc) = first_bp;
    }
}

Uint
erts_msg_attached_data_size_aux(ErtsMessage *msg)
{
    Sint sz;
    ASSERT(is_non_value(ERL_MESSAGE_TERM(msg)));
    ASSERT(msg->data.dist_ext);
    ASSERT(msg->data.dist_ext->heap_size < 0);

    sz = erts_decode_dist_ext_size(msg->data.dist_ext);
    if (sz < 0) {
	/* Bad external
	 * We leave the message intact in this case as it's not worth the trouble
	 * to make all callers remove it from queue. It will be detected again
	 * and removed from message queue later anyway.
	 */
	return 0;
    }

    msg->data.dist_ext->heap_size = sz;
    if (is_not_nil(msg->m[1])) {
	ErlHeapFragment *heap_frag;
	heap_frag = erts_dist_ext_trailer(msg->data.dist_ext);
	sz += heap_frag->used_size;
    }
    return sz;
}

ErtsMessage *
erts_try_alloc_message_on_heap(Process *pp,
			       erts_aint32_t *psp,
			       ErtsProcLocks *plp,
			       Uint sz,
			       Eterm **hpp,
			       ErlOffHeap **ohpp,
			       int *on_heap_p)
{
    int locked_main = 0;
    ErtsMessage *mp;

    ASSERT(!(*psp & ERTS_PSFLG_OFF_HEAP_MSGQ));

    if ((*psp) & ERTS_PSFLGS_VOLATILE_HEAP)
	goto in_message_fragment;
    else if (
	*plp & ERTS_PROC_LOCK_MAIN
	) {
    try_on_heap:
	if (((*psp) & ERTS_PSFLGS_VOLATILE_HEAP)
	    || (pp->flags & F_DISABLE_GC)
	    || HEAP_LIMIT(pp) - HEAP_TOP(pp) <= sz) {
	    /*
	     * The heap is either potentially in an inconsistent
	     * state, or not large enough.
	     */
	    if (locked_main) {
		*plp &= ~ERTS_PROC_LOCK_MAIN;
		erts_proc_unlock(pp, ERTS_PROC_LOCK_MAIN);
	    }
	    goto in_message_fragment;
	}

	*hpp = HEAP_TOP(pp);
	HEAP_TOP(pp) = *hpp + sz;
	*ohpp = &MSO(pp);
	mp = erts_alloc_message(0, NULL);
	mp->data.attached = NULL;
	*on_heap_p = !0;
    }
    else if (pp && erts_proc_trylock(pp, ERTS_PROC_LOCK_MAIN) == 0) {
	locked_main = 1;
	*psp = erts_atomic32_read_nob(&pp->state);
	*plp |= ERTS_PROC_LOCK_MAIN;
	goto try_on_heap;
    }
    else {
    in_message_fragment:
	if ((*psp) & ERTS_PSFLG_OFF_HEAP_MSGQ) {
	    mp = erts_alloc_message(sz, hpp);
	    *ohpp = sz == 0 ? NULL : &mp->hfrag.off_heap;
	}
	else {
	    mp = erts_alloc_message(0, NULL);
	    if (!sz) {
		*hpp = NULL;
		*ohpp = NULL;
	    }
	    else {
		ErlHeapFragment *bp;
		bp = new_message_buffer(sz);
		*hpp = &bp->mem[0];
		mp->data.heap_frag = bp;
		*ohpp = &bp->off_heap;
	    }
	}
	*on_heap_p = 0;
    }

    return mp;
}

/*
 * Send a local message when sender & receiver processes are known.
 */

Sint
erts_send_message(Process* sender,
		  Process* receiver,
		  ErtsProcLocks *receiver_locks,
		  Eterm message,
		  unsigned flags)
{
    Uint msize;
    ErtsMessage* mp;
    ErlOffHeap *ohp;
    Eterm token = NIL;
    Sint res = 0;
#ifdef USE_VM_PROBES
    DTRACE_CHARBUF(sender_name, 64);
    DTRACE_CHARBUF(receiver_name, 64);
    Sint tok_label = 0;
    Sint tok_lastcnt = 0;
    Sint tok_serial = 0;
    Eterm utag = NIL;
#endif
    erts_aint32_t receiver_state;
#ifdef SHCOPY_SEND
    erts_shcopy_t info;
#else
    erts_literal_area_t litarea;
    INITIALIZE_LITERAL_PURGE_AREA(litarea);
#endif

#ifdef USE_VM_PROBES
    *sender_name = *receiver_name = '\0';
    if (DTRACE_ENABLED(message_send)) {
        erts_snprintf(sender_name, sizeof(DTRACE_CHARBUF_NAME(sender_name)),
		      "%T", sender->common.id);
        erts_snprintf(receiver_name, sizeof(DTRACE_CHARBUF_NAME(receiver_name)),
		      "%T", receiver->common.id);
    }
#endif

    receiver_state = erts_atomic32_read_nob(&receiver->state);

    if (SEQ_TRACE_TOKEN(sender) != NIL && !(flags & ERTS_SND_FLG_NO_SEQ_TRACE)) {
        Eterm* hp;
	Eterm stoken = SEQ_TRACE_TOKEN(sender);
	Uint seq_trace_size = 0;
#ifdef USE_VM_PROBES
	Uint dt_utag_size = 0;
#endif

        /* SHCOPY corrupts the heap between
         * copy_shared_calculate, and
         * copy_shared_perform. (it inserts move_markers like the gc).
         * Make sure we don't use the heap between those instances.
         */
        if (have_seqtrace(stoken)) {
	    seq_trace_update_send(sender);
	    seq_trace_output(stoken, message, SEQ_TRACE_SEND,
			     receiver->common.id, sender);

	    seq_trace_size = size_object(stoken);
	}
#ifdef USE_VM_PROBES
        if (DT_UTAG_FLAGS(sender) & DT_UTAG_SPREADING) {
            dt_utag_size = size_object(DT_UTAG(sender));
        } else if (stoken == am_have_dt_utag ) {
            stoken = NIL;
        }
#endif

#ifdef SHCOPY_SEND
        INITIALIZE_SHCOPY(info);
        msize = copy_shared_calculate(message, &info);
#else
        msize = size_object_litopt(message, &litarea);
#endif
        mp = erts_alloc_message_heap_state(receiver,
                                           &receiver_state,
                                           receiver_locks,
                                           (msize
#ifdef USE_VM_PROBES
                                            + dt_utag_size
#endif
                                            + seq_trace_size),
                                           &hp,
                                           &ohp);

#ifdef SHCOPY_SEND
	if (is_not_immed(message))
            message = copy_shared_perform(message, msize, &info, &hp, ohp);
        DESTROY_SHCOPY(info);
#else
	if (is_not_immed(message))
            message = copy_struct_litopt(message, msize, &hp, ohp, &litarea);
#endif
	if (is_immed(stoken))
	    token = stoken;
	else
	    token = copy_struct(stoken, seq_trace_size, &hp, ohp);

#ifdef USE_VM_PROBES
	if (DT_UTAG_FLAGS(sender) & DT_UTAG_SPREADING) {
	    if (is_immed(DT_UTAG(sender)))
		utag = DT_UTAG(sender);
	    else
		utag = copy_struct(DT_UTAG(sender), dt_utag_size, &hp, ohp);
	}
        if (DTRACE_ENABLED(message_send)) {
            if (have_seqtrace(stoken)) {
                tok_label = SEQ_TRACE_T_DTRACE_LABEL(stoken);
		tok_lastcnt = signed_val(SEQ_TRACE_T_LASTCNT(stoken));
		tok_serial = signed_val(SEQ_TRACE_T_SERIAL(stoken));
	    }
	    DTRACE6(message_send, sender_name, receiver_name,
		    msize, tok_label, tok_lastcnt, tok_serial);
        }
#endif
    } else {
        Eterm *hp;

	if (receiver == sender && !(receiver_state & ERTS_PSFLG_OFF_HEAP_MSGQ)) {
	    mp = erts_alloc_message(0, NULL);
	    msize = 0;
	}
	else {
#ifdef SHCOPY_SEND
            INITIALIZE_SHCOPY(info);
            msize = copy_shared_calculate(message, &info);
#else
            msize = size_object_litopt(message, &litarea);
#endif
	    mp = erts_alloc_message_heap_state(receiver,
					       &receiver_state,
					       receiver_locks,
					       msize,
					       &hp,
					       &ohp);
#ifdef SHCOPY_SEND
            if (is_not_immed(message))
                message = copy_shared_perform(message, msize, &info, &hp, ohp);
            DESTROY_SHCOPY(info);
#else
            if (is_not_immed(message))
                message = copy_struct_litopt(message, msize, &hp, ohp, &litarea);
#endif
	}
#ifdef USE_VM_PROBES
        DTRACE6(message_send, sender_name, receiver_name,
                msize, tok_label, tok_lastcnt, tok_serial);
#endif
    }

    ERL_MESSAGE_TOKEN(mp) = token;
#ifdef USE_VM_PROBES
    ERL_MESSAGE_DT_UTAG(mp) = utag;
#endif
    res = queue_message(receiver,
			&receiver_state,
			*receiver_locks,
			mp, message,
                        sender->common.id);

    return res;
}


/*
 * This function delivers an EXIT message to a process
 * which is trapping EXITs.
 */

void
erts_deliver_exit_message(Eterm from, Process *to, ErtsProcLocks *to_locksp,
			  Eterm reason, Eterm token)
{
    Eterm mess;
    Eterm save;
    Eterm from_copy;
    Uint sz_reason;
    Uint sz_token;
    Uint sz_from;
    Eterm* hp;
    Eterm temptoken;
    ErtsMessage* mp;
    ErlOffHeap *ohp;
#ifdef SHCOPY_SEND
    erts_shcopy_t info;
#endif

    if (have_seqtrace(token)) {
	ASSERT(is_tuple(token));
	sz_token = size_object(token);
	sz_from = size_object(from);
#ifdef SHCOPY_SEND
        INITIALIZE_SHCOPY(info);
        sz_reason = copy_shared_calculate(reason, &info);
#else
	sz_reason = size_object(reason);
#endif
	mp = erts_alloc_message_heap(to, to_locksp,
				     sz_reason + sz_from + sz_token + 4,
				     &hp, &ohp);
#ifdef SHCOPY_SEND
        mess = copy_shared_perform(reason, sz_reason, &info, &hp, ohp);
        DESTROY_SHCOPY(info);
#else
	mess = copy_struct(reason, sz_reason, &hp, ohp);
#endif
	from_copy = copy_struct(from, sz_from, &hp, ohp);
	save = TUPLE3(hp, am_EXIT, from_copy, mess);
	hp += 4;
	/* the trace token must in this case be updated by the caller */
	seq_trace_output(token, save, SEQ_TRACE_SEND, to->common.id, NULL);
	temptoken = copy_struct(token, sz_token, &hp, ohp);
        ERL_MESSAGE_TOKEN(mp) = temptoken;
	erts_queue_message(to, *to_locksp, mp, save, am_system);
    } else {
	sz_from = IS_CONST(from) ? 0 : size_object(from);
#ifdef SHCOPY_SEND
        INITIALIZE_SHCOPY(info);
        sz_reason = copy_shared_calculate(reason, &info);
#else
	sz_reason = size_object(reason);
#endif
	mp = erts_alloc_message_heap(to, to_locksp,
				     sz_reason+sz_from+4, &hp, &ohp);

#ifdef SHCOPY_SEND
        mess = copy_shared_perform(reason, sz_reason, &info, &hp, ohp);
        DESTROY_SHCOPY(info);
#else
	mess = copy_struct(reason, sz_reason, &hp, ohp);
#endif
	from_copy = (IS_CONST(from)
		     ? from
		     : copy_struct(from, sz_from, &hp, ohp));
	save = TUPLE3(hp, am_EXIT, from_copy, mess);
	erts_queue_message(to, *to_locksp, mp, save, am_system);
    }
}

void erts_save_message_in_proc(Process *p, ErtsMessage *msgp)
{
    ErlHeapFragment *hfp;

    if (msgp->data.attached == ERTS_MSG_COMBINED_HFRAG)
	hfp = &msgp->hfrag;
    else if (msgp->data.attached) {
	hfp = msgp->data.heap_frag;
    }
    else {
	erts_free_message(msgp);
	return; /* Nothing to save */
    }

    while (1) {
	struct erl_off_heap_header *ohhp = hfp->off_heap.first;
	if (ohhp) {
	    for ( ; ohhp->next; ohhp = ohhp->next)
		;
	    ohhp->next = p->off_heap.first;
	    p->off_heap.first = hfp->off_heap.first;
	    hfp->off_heap.first = NULL;
	}
	p->off_heap.overhead += hfp->off_heap.overhead;
	hfp->off_heap.overhead = 0;
	p->mbuf_sz += hfp->used_size;

	if (!hfp->next)
	    break;
	hfp = hfp->next;
    }

    msgp->next = p->msg_frag;
    p->msg_frag = msgp;
}

Sint
erts_move_messages_off_heap(Process *c_p)
{
    int reds = 1;
    int i;
    ErtsMessage *msgq[] = {c_p->sig_qs.first, c_p->sig_qs.cont};
    /*
     * Move all messages off heap. This *only* occurs when the
     * process had off heap message disabled and just enabled
     * it...
     */

    reds += c_p->sig_qs.len / 10;

    ASSERT(erts_atomic32_read_nob(&c_p->state)
	   & ERTS_PSFLG_OFF_HEAP_MSGQ);
    ASSERT(c_p->flags & F_OFF_HEAP_MSGQ_CHNG);

    for (i = 0; i < sizeof(msgq)/sizeof(msgq[0]); i++) {
        ErtsMessage *mp;
        for (mp = msgq[i]; mp; mp = mp->next) {
            Uint msg_sz, token_sz;
#ifdef USE_VM_PROBES
            Uint utag_sz;
#endif
            Eterm *hp;
            ErlHeapFragment *hfrag;

            if (!ERTS_SIG_IS_INTERNAL_MSG(mp))
                continue;

            if (mp->data.attached)
                continue;

            if (is_immed(ERL_MESSAGE_TERM(mp))
#ifdef USE_VM_PROBES
                && is_immed(ERL_MESSAGE_DT_UTAG(mp))
#endif
                && is_not_immed(ERL_MESSAGE_TOKEN(mp)))
                continue;

            /*
             * The message refers into the heap. Copy the message
             * from the heap into a heap fragment and attach
             * it to the message...
             */
            msg_sz = size_object(ERL_MESSAGE_TERM(mp));
#ifdef USE_VM_PROBES
            utag_sz = size_object(ERL_MESSAGE_DT_UTAG(mp));
#endif
            token_sz = size_object(ERL_MESSAGE_TOKEN(mp));

            hfrag = new_message_buffer(msg_sz
#ifdef USE_VM_PROBES
                                       + utag_sz
#endif
                                       + token_sz);
            hp = hfrag->mem;
            if (is_not_immed(ERL_MESSAGE_TERM(mp)))
                ERL_MESSAGE_TERM(mp) = copy_struct(ERL_MESSAGE_TERM(mp),
                                                   msg_sz, &hp,
                                                   &hfrag->off_heap);
            if (is_not_immed(ERL_MESSAGE_TOKEN(mp)))
                ERL_MESSAGE_TOKEN(mp) = copy_struct(ERL_MESSAGE_TOKEN(mp),
                                                    token_sz, &hp,
                                                    &hfrag->off_heap);
#ifdef USE_VM_PROBES
            if (is_not_immed(ERL_MESSAGE_DT_UTAG(mp)))
                ERL_MESSAGE_DT_UTAG(mp) = copy_struct(ERL_MESSAGE_DT_UTAG(mp),
                                                      utag_sz, &hp,
                                                      &hfrag->off_heap);
#endif
            mp->data.heap_frag = hfrag;
            reds += 1;
        }
    }

    return reds;
}

Sint
erts_complete_off_heap_message_queue_change(Process *c_p)
{
    int reds = 1;

    ERTS_LC_ASSERT(ERTS_PROC_LOCK_MAIN == erts_proc_lc_my_proc_locks(c_p));
    ASSERT(c_p->flags & F_OFF_HEAP_MSGQ_CHNG);
    ASSERT(erts_atomic32_read_nob(&c_p->state) & ERTS_PSFLG_OFF_HEAP_MSGQ);

    /*
     * This job was first initiated when the process changed to off heap
     * message queue management. Since then ERTS_PSFLG_OFF_HEAP_MSGQ
     * has been set. However, the management state might have been changed
     * again (multiple times) since then. Check users last requested state
     * (the flags F_OFF_HEAP_MSGQ, and F_ON_HEAP_MSGQ), and make the state
     * consistent with that.
     */

    if (!(c_p->flags & F_OFF_HEAP_MSGQ))
	erts_atomic32_read_band_nob(&c_p->state,
					~ERTS_PSFLG_OFF_HEAP_MSGQ);
    else {
	reds += 2;
	erts_proc_lock(c_p, ERTS_PROC_LOCK_MSGQ);
        erts_proc_sig_fetch(c_p);
	erts_proc_unlock(c_p, ERTS_PROC_LOCK_MSGQ);
	reds += erts_move_messages_off_heap(c_p);
    }
    c_p->flags &= ~F_OFF_HEAP_MSGQ_CHNG;
    return reds;
}

typedef struct {
    Eterm pid;
    ErtsThrPrgrLaterOp lop;
} ErtsChangeOffHeapMessageQueue;

static void
change_off_heap_msgq(void *vcohmq)
{
    ErtsChangeOffHeapMessageQueue *cohmq;
    /*
     * Now we've waited thread progress which ensures that all
     * messages to the process are enqueued off heap. Schedule
     * completion of this change as a system task on the process
     * itself. This in order to avoid lock contention on its
     * main lock. We will be called in
     * erts_complete_off_heap_message_queue_change() (above) when
     * the system task has been selected for execution.
     */
    cohmq = (ErtsChangeOffHeapMessageQueue *) vcohmq;
    erts_schedule_complete_off_heap_message_queue_change(cohmq->pid);
    erts_free(ERTS_ALC_T_MSGQ_CHNG, vcohmq);
}

Eterm
erts_change_message_queue_management(Process *c_p, Eterm new_state)
{
    Eterm res;

#ifdef DEBUG
    if (c_p->flags & F_OFF_HEAP_MSGQ) {
	ASSERT(erts_atomic32_read_nob(&c_p->state)
	       & ERTS_PSFLG_OFF_HEAP_MSGQ);
    }
    else {
	if (c_p->flags & F_OFF_HEAP_MSGQ_CHNG) {
	    ASSERT(erts_atomic32_read_nob(&c_p->state)
		   & ERTS_PSFLG_OFF_HEAP_MSGQ);
	}
	else {
	    ASSERT(!(erts_atomic32_read_nob(&c_p->state)
		     & ERTS_PSFLG_OFF_HEAP_MSGQ));
	}
    }
#endif

    switch (c_p->flags & (F_OFF_HEAP_MSGQ|F_ON_HEAP_MSGQ)) {

    case F_OFF_HEAP_MSGQ:
	res = am_off_heap;

	switch (new_state) {
	case am_off_heap:
	    break;
	case am_on_heap:
	    c_p->flags |= F_ON_HEAP_MSGQ;
	    c_p->flags &= ~F_OFF_HEAP_MSGQ;
	    /*
	     * We are not allowed to clear ERTS_PSFLG_OFF_HEAP_MSGQ
	     * if a off heap change is ongoing. It will be adjusted
	     * when the change completes...
	     */
	    if (!(c_p->flags & F_OFF_HEAP_MSGQ_CHNG)) {
		/* Safe to clear ERTS_PSFLG_OFF_HEAP_MSGQ... */
		erts_atomic32_read_band_nob(&c_p->state,
						~ERTS_PSFLG_OFF_HEAP_MSGQ);
	    }
	    break;
	default:
	    res = THE_NON_VALUE; /* badarg */
	    break;
	}
	break;

    case F_ON_HEAP_MSGQ:
	res = am_on_heap;

	switch (new_state) {
	case am_on_heap:
	    break;
	case am_off_heap:
	    c_p->flags &= ~F_ON_HEAP_MSGQ;
	    goto change_to_off_heap;
	default:
	    res = THE_NON_VALUE; /* badarg */
	    break;
	}
	break;

    default:
	res = am_error;
	ERTS_INTERNAL_ERROR("Inconsistent message queue management state");
	break;
    }

    return res;

change_to_off_heap:

    c_p->flags |= F_OFF_HEAP_MSGQ;

    /*
     * We do not have to schedule a change if
     * we have an ongoing off heap change...
     */
    if (!(c_p->flags & F_OFF_HEAP_MSGQ_CHNG)) {
	ErtsChangeOffHeapMessageQueue *cohmq;
	/*
	 * Need to set ERTS_PSFLG_OFF_HEAP_MSGQ and wait
	 * thread progress before completing the change in
	 * order to ensure that all senders observe that
	 * messages should be passed off heap. When the
	 * change has completed, GC does not need to inspect
	 * the message queue at all.
	 */
	erts_atomic32_read_bor_nob(&c_p->state,
				       ERTS_PSFLG_OFF_HEAP_MSGQ);
	c_p->flags |= F_OFF_HEAP_MSGQ_CHNG;
	cohmq = erts_alloc(ERTS_ALC_T_MSGQ_CHNG,
			   sizeof(ErtsChangeOffHeapMessageQueue));
	cohmq->pid = c_p->common.id;
	erts_schedule_thr_prgr_later_op(change_off_heap_msgq,
					(void *) cohmq,
					&cohmq->lop);
    }

    return res;
}

int
erts_decode_dist_message(Process *proc, ErtsProcLocks proc_locks,
			 ErtsMessage *msgp, int force_off_heap)
{
    ErtsHeapFactory factory;
    Eterm msg;
    ErlHeapFragment *bp;
    Sint need;
    int decode_in_heap_frag;

    decode_in_heap_frag = (force_off_heap
			   || !(proc_locks & ERTS_PROC_LOCK_MAIN)
			   || (proc->flags & F_OFF_HEAP_MSGQ));

    if (msgp->data.dist_ext->heap_size >= 0)
	need = msgp->data.dist_ext->heap_size;
    else {
	need = erts_decode_dist_ext_size(msgp->data.dist_ext);
	if (need < 0) {
	    /* bad msg; remove it... */
	    if (is_not_immed(ERL_MESSAGE_TOKEN(msgp))) {
		bp = erts_dist_ext_trailer(msgp->data.dist_ext);
		erts_cleanup_offheap(&bp->off_heap);
	    }
	    erts_free_dist_ext_copy(msgp->data.dist_ext);
	    msgp->data.dist_ext = NULL;
	    return 0;
	}

	msgp->data.dist_ext->heap_size = need;
    }

    if (is_not_immed(ERL_MESSAGE_TOKEN(msgp))) {
	bp = erts_dist_ext_trailer(msgp->data.dist_ext);
	need += bp->used_size;
    }

    if (decode_in_heap_frag)
	erts_factory_heap_frag_init(&factory, new_message_buffer(need));
    else
	erts_factory_proc_prealloc_init(&factory, proc, need);

    ASSERT(msgp->data.dist_ext->heap_size >= 0);
    if (is_not_immed(ERL_MESSAGE_TOKEN(msgp))) {
	ErlHeapFragment *heap_frag;
	heap_frag = erts_dist_ext_trailer(msgp->data.dist_ext);
	ERL_MESSAGE_TOKEN(msgp) = copy_struct(ERL_MESSAGE_TOKEN(msgp),
					      heap_frag->used_size,
					      &factory.hp,
					      factory.off_heap);
	erts_cleanup_offheap(&heap_frag->off_heap);
    }

    msg = erts_decode_dist_ext(&factory, msgp->data.dist_ext);
    ERL_MESSAGE_TERM(msgp) = msg;
    erts_free_dist_ext_copy(msgp->data.dist_ext);
    msgp->data.attached = NULL;

    if (is_non_value(msg)) {
	erts_factory_undo(&factory);
	return 0;
    }

    erts_factory_trim_and_close(&factory, msgp->m,
				ERL_MESSAGE_REF_ARRAY_SZ);

    ASSERT(!msgp->data.heap_frag);

    if (decode_in_heap_frag)
	msgp->data.heap_frag = factory.heap_frags;

    return 1;
}

void erts_factory_proc_init(ErtsHeapFactory* factory,
			    Process* p)
{
    erts_factory_proc_prealloc_init(factory, p, HEAP_LIMIT(p) - HEAP_TOP(p));
}

void erts_factory_proc_prealloc_init(ErtsHeapFactory* factory,
				     Process* p,
				     Sint size)
{
    ErlHeapFragment *bp = p->mbuf;
    factory->mode     = FACTORY_HALLOC;
    factory->p        = p;
    factory->hp_start = HAlloc(p, size);
    factory->hp       = factory->hp_start;
    factory->hp_end   = factory->hp_start + size;
    factory->off_heap = &p->off_heap;
    factory->message  = NULL;
    factory->off_heap_saved.first    = p->off_heap.first;
    factory->off_heap_saved.overhead = p->off_heap.overhead;
    factory->heap_frags_saved = bp;
    factory->heap_frags_saved_used = bp ? bp->used_size : 0;
    factory->heap_frags = NULL; /* not used */
    factory->alloc_type = 0; /* not used */
}

void erts_factory_heap_frag_init(ErtsHeapFactory* factory,
				 ErlHeapFragment* bp)
{
    factory->mode     = FACTORY_HEAP_FRAGS;
    factory->p        = NULL;
    factory->hp_start = bp->mem;
    factory->hp       = bp->mem;
    factory->hp_end   = bp->mem + bp->alloc_size;
    factory->off_heap = &bp->off_heap;
    factory->message  = NULL;
    factory->heap_frags = bp;
    factory->heap_frags_saved = NULL;
    factory->heap_frags_saved_used = 0;
    factory->alloc_type = ERTS_ALC_T_HEAP_FRAG;
    ASSERT(!bp->next);
    factory->off_heap_saved.first    = factory->off_heap->first;
    factory->off_heap_saved.overhead = factory->off_heap->overhead;

    ASSERT(factory->hp >= factory->hp_start && factory->hp <= factory->hp_end);
}


ErtsMessage *
erts_factory_message_create(ErtsHeapFactory* factory,
			    Process *proc,
			    ErtsProcLocks *proc_locksp,
			    Uint sz)
{
    Eterm *hp;
    ErlOffHeap *ohp;
    ErtsMessage *msgp;
    int on_heap;
    erts_aint32_t state;

    state = proc ? erts_atomic32_read_nob(&proc->state) : 0;

    if (state & ERTS_PSFLG_OFF_HEAP_MSGQ) {
	msgp = erts_alloc_message(sz, &hp);
	ohp = sz == 0 ? NULL : &msgp->hfrag.off_heap;
	on_heap = 0;
    }
    else {
	msgp = erts_try_alloc_message_on_heap(proc, &state,
					      proc_locksp,
					      sz, &hp, &ohp,
					      &on_heap);
    }

    if (on_heap) {
	ERTS_ASSERT(*proc_locksp & ERTS_PROC_LOCK_MAIN);
	ASSERT(ohp == &proc->off_heap);
	factory->mode = FACTORY_HALLOC;
	factory->p = proc;
	factory->heap_frags_saved = proc->mbuf;
	factory->heap_frags_saved_used = proc->mbuf ? proc->mbuf->used_size : 0;
    }
    else {
	factory->mode = FACTORY_MESSAGE;
	factory->p = NULL;
	factory->heap_frags_saved = NULL;
	factory->heap_frags_saved_used = 0;

	if (msgp->data.attached == ERTS_MSG_COMBINED_HFRAG) {
	    ASSERT(!msgp->hfrag.next);
	    factory->heap_frags = NULL;
	}
	else {
	    ASSERT(!msgp->data.heap_frag
		   || !msgp->data.heap_frag->next);
	    factory->heap_frags = msgp->data.heap_frag;
	}
    }
    factory->hp_start = hp;
    factory->hp       = hp;
    factory->hp_end   = hp + sz;
    factory->message  = msgp;
    factory->off_heap = ohp;
    factory->alloc_type = ERTS_ALC_T_HEAP_FRAG;
    if (ohp) {
	factory->off_heap_saved.first    = ohp->first;
	factory->off_heap_saved.overhead = ohp->overhead;
    }
    else {
	factory->off_heap_saved.first    = NULL;
	factory->off_heap_saved.overhead = 0;
    }

    ASSERT(factory->hp >= factory->hp_start && factory->hp <= factory->hp_end);

    return msgp;
}

void erts_factory_selfcontained_message_init(ErtsHeapFactory* factory,
					     ErtsMessage *msgp,
					     Eterm *hp)
{
    ErlHeapFragment* bp;
    if (msgp->data.attached == ERTS_MSG_COMBINED_HFRAG) {
	bp = &msgp->hfrag;
	factory->heap_frags = NULL;
    }
    else {
	bp = msgp->data.heap_frag;
	factory->heap_frags = bp;
    }
    factory->mode     = FACTORY_MESSAGE;
    factory->p        = NULL;
    factory->hp_start = bp->mem;
    factory->hp       = hp;
    factory->hp_end   = bp->mem + bp->alloc_size;
    factory->message  = msgp;
    factory->off_heap = &bp->off_heap;
    factory->heap_frags_saved = NULL;
    factory->heap_frags_saved_used = 0;
    factory->alloc_type = ERTS_ALC_T_HEAP_FRAG;
    ASSERT(!bp->next);
    factory->off_heap_saved.first    = factory->off_heap->first;
    factory->off_heap_saved.overhead = factory->off_heap->overhead;

    ASSERT(factory->hp >= factory->hp_start && factory->hp <= factory->hp_end);
}

/* One static sized heap that must suffice.
   No extra heap fragments will be allocated.
*/
void erts_factory_static_init(ErtsHeapFactory* factory,
			     Eterm* hp,
			     Uint size,
			     ErlOffHeap* off_heap)
{
    factory->mode     = FACTORY_STATIC;
    factory->hp_start = hp;
    factory->hp       = hp;
    factory->hp_end   = hp + size;
    factory->off_heap = off_heap;
    factory->off_heap_saved.first    = factory->off_heap->first;
    factory->off_heap_saved.overhead = factory->off_heap->overhead;
}

/* A temporary heap with default buffer allocated/freed by client.
 * factory_close is same as factory_undo
 */
void erts_factory_tmp_init(ErtsHeapFactory* factory, Eterm* hp, Uint size,
			   Uint32 atype)
{
    factory->mode     = FACTORY_TMP;
    factory->hp_start = hp;
    factory->hp       = hp;
    factory->hp_end   = hp + size;
    factory->heap_frags = NULL;
    factory->off_heap_saved.first    = NULL;
    factory->off_heap_saved.overhead = 0;
    factory->off_heap = &factory->off_heap_saved;
    factory->alloc_type = atype;
}

/* When we know the term is an immediate and need no heap.
*/
void erts_factory_dummy_init(ErtsHeapFactory* factory)
{
    factory->mode = FACTORY_CLOSED;
}

static void reserve_heap(ErtsHeapFactory*, Uint need, Uint xtra);

Eterm* erts_produce_heap(ErtsHeapFactory* factory, Uint need, Uint xtra)
{
    Eterm* res;

    ASSERT((unsigned int)factory->mode > (unsigned int)FACTORY_CLOSED);
    if (factory->hp + need > factory->hp_end) {
	reserve_heap(factory, need, xtra);
    }
    res = factory->hp;
    factory->hp += need;
    return res;
}

Eterm* erts_reserve_heap(ErtsHeapFactory* factory, Uint need)
{
    ASSERT((unsigned int)factory->mode > (unsigned int)FACTORY_CLOSED);
    if (factory->hp + need > factory->hp_end) {
	reserve_heap(factory, need, 200);
    }
    return factory->hp;
}

static void reserve_heap(ErtsHeapFactory* factory, Uint need, Uint xtra)
{
    ErlHeapFragment* bp;

    switch (factory->mode) {
    case FACTORY_HALLOC:
	HRelease(factory->p, factory->hp_end, factory->hp);
	factory->hp     = HAllocX(factory->p, need, xtra);
	factory->hp_end = factory->hp + need;
	return;

    case FACTORY_MESSAGE:
	if (!factory->heap_frags) {
	    ASSERT(factory->message->data.attached == ERTS_MSG_COMBINED_HFRAG);
	    bp = &factory->message->hfrag;
	}
	else {
	    /* Fall through */
	case FACTORY_HEAP_FRAGS:
	case FACTORY_TMP:
	    bp = factory->heap_frags;
	}

        if (bp) {
	    ASSERT(factory->hp > bp->mem);
	    ASSERT(factory->hp <= factory->hp_end);
	    ASSERT(factory->hp_end == bp->mem + bp->alloc_size);

	    bp->used_size = factory->hp - bp->mem;
        }
	bp = (ErlHeapFragment*) ERTS_HEAP_ALLOC(factory->alloc_type,
						ERTS_HEAP_FRAG_SIZE(need+xtra));
	bp->next = factory->heap_frags;
	factory->heap_frags = bp;
	bp->alloc_size = need + xtra;
	bp->used_size = need;
	bp->off_heap.first = NULL;
	bp->off_heap.overhead = 0;

	factory->hp     = bp->mem;
	factory->hp_end = bp->mem + bp->alloc_size;
	return;

    case FACTORY_STATIC:
    case FACTORY_CLOSED:
    default:
	ASSERT(!"Invalid factory mode");
    }
}

void erts_factory_close(ErtsHeapFactory* factory)
{
    ErlHeapFragment* bp;

    switch (factory->mode) {
    case FACTORY_HALLOC:
	HRelease(factory->p, factory->hp_end, factory->hp);
	break;

    case FACTORY_MESSAGE:
	if (!factory->heap_frags) {
	    if (factory->message->data.attached == ERTS_MSG_COMBINED_HFRAG)
		bp = &factory->message->hfrag;
	    else
		bp = NULL;
	}
	else {
	    if (factory->message->data.attached	== ERTS_MSG_COMBINED_HFRAG)
		factory->message->hfrag.next = factory->heap_frags;
	    else
		factory->message->data.heap_frag = factory->heap_frags;

	    /* Fall through */
	case FACTORY_HEAP_FRAGS:
	    bp = factory->heap_frags;
	}

	if (bp) {
	    ASSERT(factory->hp >= bp->mem);
	    ASSERT(factory->hp <= factory->hp_end);
	    ASSERT(factory->hp_end == bp->mem + bp->alloc_size);

	    bp->used_size = factory->hp - bp->mem;
        }
	break;
    case FACTORY_TMP:
	erts_factory_undo(factory);
	break;
    case FACTORY_STATIC: break;
    case FACTORY_CLOSED: break;
    default:
	ASSERT(!"Invalid factory mode");
    }
    factory->mode = FACTORY_CLOSED;
}

void erts_factory_trim_and_close(ErtsHeapFactory* factory,
				 Eterm *brefs, Uint brefs_size)
{
    ErlHeapFragment *bp;

    switch (factory->mode) {
    case FACTORY_MESSAGE: {
	ErtsMessage *mp = factory->message;
	if (mp->data.attached == ERTS_MSG_COMBINED_HFRAG) {
	    if (!factory->heap_frags) {
		Uint sz = factory->hp - factory->hp_start;
		mp = erts_shrink_message(mp, sz, brefs, brefs_size);
		factory->message = mp;
		factory->mode = FACTORY_CLOSED;
		return;
	    }
	    /*else we don't trim multi fragmented messages for now (off_heap...) */
	    break;
	}
	/* Fall through... */
    }
    case FACTORY_HEAP_FRAGS:
	bp = factory->heap_frags;
	if (!bp)
	    break;
        if (bp->next == NULL) {
            Uint used_sz = factory->hp - bp->mem;
            ASSERT(used_sz <= bp->alloc_size);
	    if (used_sz > 0)
		bp = erts_resize_message_buffer(bp, used_sz,
						brefs, brefs_size);
	    else {
		free_message_buffer(bp);
		bp = NULL;
	    }
	    factory->heap_frags = bp;
	    if (factory->mode == FACTORY_MESSAGE)
		factory->message->data.heap_frag = bp;
            factory->mode = FACTORY_CLOSED;
            return;
        }
        /*else we don't trim multi fragmented messages for now (off_heap...) */
    default:
	break;
    }
    erts_factory_close(factory);
}

void erts_factory_undo(ErtsHeapFactory* factory)
{
    ErlHeapFragment* bp;
    struct erl_off_heap_header *hdr, **hdr_nextp;

    switch (factory->mode) {
    case FACTORY_HALLOC:
    case FACTORY_STATIC:
	/* Cleanup off-heap
	 */
	hdr_nextp = NULL;
        for (hdr = factory->off_heap->first;
	     hdr != factory->off_heap_saved.first;
	     hdr = hdr->next) {

	    hdr_nextp = &hdr->next;
        }

        if (hdr_nextp != NULL) {
	    *hdr_nextp = NULL;
	    erts_cleanup_offheap(factory->off_heap);
	    factory->off_heap->first    = factory->off_heap_saved.first;
	    factory->off_heap->overhead = factory->off_heap_saved.overhead;
        }

        if (factory->mode == FACTORY_HALLOC) {
            /* Free heap frags
             */
            bp = factory->p->mbuf;
            if (bp != factory->heap_frags_saved) {
                do {
                    ErlHeapFragment *next_bp = bp->next;
                    ASSERT(bp->off_heap.first == NULL);
                    ERTS_HEAP_FREE(ERTS_ALC_T_HEAP_FRAG, (void *) bp,
                                   ERTS_HEAP_FRAG_SIZE(bp->alloc_size));
                    bp = next_bp;
                } while (bp != factory->heap_frags_saved);

                factory->p->mbuf = bp;
            }

            /* Rollback heap top
	     */

	    if (HEAP_START(factory->p) <= factory->hp_start
		&& factory->hp_start <= HEAP_LIMIT(factory->p)) {
		HEAP_TOP(factory->p) = factory->hp_start;
	    }

	    /* Fix last heap frag */
            if (factory->heap_frags_saved) {
                ASSERT(factory->heap_frags_saved == factory->p->mbuf);
                if (factory->hp_start != factory->heap_frags_saved->mem)
                    factory->heap_frags_saved->used_size = factory->heap_frags_saved_used;
		else {
                    factory->p->mbuf = factory->p->mbuf->next;
                    ERTS_HEAP_FREE(ERTS_ALC_T_HEAP_FRAG, factory->heap_frags_saved,
                                   ERTS_HEAP_FRAG_SIZE(factory->heap_frags_saved->alloc_size));
                }
            }
        }
        break;

    case FACTORY_MESSAGE:
	if (factory->message->data.attached == ERTS_MSG_COMBINED_HFRAG)
	    factory->message->hfrag.next = factory->heap_frags;
	else
	    factory->message->data.heap_frag = factory->heap_frags;
	erts_cleanup_messages(factory->message);
	break;
    case FACTORY_TMP:
    case FACTORY_HEAP_FRAGS:
	erts_cleanup_offheap(factory->off_heap);
	factory->off_heap->first = NULL;

        bp = factory->heap_frags;
        while (bp != NULL) {
            ErlHeapFragment* next_bp = bp->next;

            ASSERT(bp->off_heap.first == NULL);
            ERTS_HEAP_FREE(factory->alloc_type, (void *) bp,
                           ERTS_HEAP_FRAG_SIZE(bp->alloc_size));
            bp = next_bp;
        }
	break;

    case FACTORY_CLOSED: break;
    default:
	ASSERT(!"Invalid factory mode");
    }
    factory->mode = FACTORY_CLOSED;
#ifdef DEBUG
    factory->p = NULL;
    factory->hp = NULL;
    factory->heap_frags = NULL;
#endif
}

Uint
erts_mbuf_size(Process *p)
{
    Uint sz = 0;
    ErlHeapFragment* bp;
    ErtsMessage* mp;

    for (bp = p->mbuf; bp; bp = bp->next)
	sz += bp->used_size;

    for (mp = p->msg_frag; mp; mp = mp->next)
	for (bp = erts_message_to_heap_frag(mp); bp; bp = bp->next)
	    sz += bp->used_size;

    return sz;
}
