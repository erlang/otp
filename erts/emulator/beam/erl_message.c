/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1997-2012. All Rights Reserved.
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

ERTS_SCHED_PREF_QUICK_ALLOC_IMPL(message,
				 ErlMessage,
				 ERL_MESSAGE_BUF_SZ,
				 ERTS_ALC_T_MSG_REF)

#if defined(DEBUG) && 0
#define HARD_DEBUG
#else
#undef HARD_DEBUG
#endif




#ifdef DEBUG
static ERTS_INLINE int in_heapfrag(const Eterm* ptr, const ErlHeapFragment *bp)
{
    return ((unsigned)(ptr - bp->mem) < bp->used_size);
}
#endif


void
init_message(void)
{
    init_message_alloc();
}

void
free_message(ErlMessage* mp)
{
    message_free(mp);
}

/* Allocate message buffer (size in words) */
ErlHeapFragment*
new_message_buffer(Uint size)
{
    ErlHeapFragment* bp;
    bp = (ErlHeapFragment*) ERTS_HEAP_ALLOC(ERTS_ALC_T_HEAP_FRAG,
					    ERTS_HEAP_FRAG_SIZE(size));
    ERTS_INIT_HEAP_FRAG(bp, size);
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
	    if (erts_refc_dectest(&u.pb->val->refc, 0) == 0) {
		erts_bin_free(u.pb->val);
	    }
	    break;
	case FUN_SUBTAG:
	    if (erts_refc_dectest(&u.fun->fe->refc, 0) == 0) {
		erts_erase_fun_entry(u.fun->fe);		    
	    }
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
		       ERTS_HEAP_FRAG_SIZE(bp->size));	
	bp = next_bp;
    }while (bp != NULL);
}

static ERTS_INLINE void
link_mbuf_to_proc(Process *proc, ErlHeapFragment *bp)
{
    if (bp) {
	/* Link the message buffer */
	bp->next = MBUF(proc);
	MBUF(proc) = bp;
	MBUF_SIZE(proc) += bp->used_size;
	FLAGS(proc) |= F_FORCE_GC;

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
    }
}

Eterm
erts_msg_distext2heap(Process *pp,
		      ErtsProcLocks *plcksp,
		      ErlHeapFragment **bpp,
		      Eterm *tokenp,
		      ErtsDistExternal *dist_extp)
{
    Eterm msg;
    Uint tok_sz = 0;
    Eterm *hp = NULL;
    ErtsHeapFactory factory;
    Sint sz;

    *bpp = NULL;
    sz = erts_decode_dist_ext_size(dist_extp);
    if (sz < 0)
	goto decode_error;
    if (is_not_nil(*tokenp)) {
	ErlHeapFragment *heap_frag = erts_dist_ext_trailer(dist_extp);
	tok_sz = heap_frag->used_size;
	sz += tok_sz;
    }
    if (pp) {
	ErlOffHeap *ohp;
	hp = erts_alloc_message_heap(sz, bpp, &ohp, pp, plcksp);
    }
    else {
	*bpp = new_message_buffer(sz);
	hp = (*bpp)->mem;
    }
    erts_factory_message_init(&factory, pp, hp, *bpp);
    msg = erts_decode_dist_ext(&factory, dist_extp);
    if (is_non_value(msg))
	goto decode_error;
    if (is_not_nil(*tokenp)) {
	ErlHeapFragment *heap_frag = erts_dist_ext_trailer(dist_extp);
	hp = erts_produce_heap(&factory, tok_sz, 0);
	*tokenp = copy_struct(*tokenp, tok_sz, &hp, factory.off_heap);
	erts_cleanup_offheap(&heap_frag->off_heap);
    }
    erts_free_dist_ext_copy(dist_extp);
    erts_factory_close(&factory);
    return msg;

 decode_error:
    if (is_not_nil(*tokenp)) {
	ErlHeapFragment *heap_frag = erts_dist_ext_trailer(dist_extp);
	erts_cleanup_offheap(&heap_frag->off_heap);
    }
    erts_free_dist_ext_copy(dist_extp);
    *bpp = NULL;
    return THE_NON_VALUE;
 }

void
erts_queue_dist_message(Process *rcvr,
			ErtsProcLocks *rcvr_locks,
			ErtsDistExternal *dist_ext,
			Eterm token)
{
    ErlMessage* mp;
#ifdef USE_VM_PROBES
    Sint tok_label = 0;
    Sint tok_lastcnt = 0;
    Sint tok_serial = 0;
#endif
#ifdef ERTS_SMP
    erts_aint_t state;
#endif

    ERTS_SMP_LC_ASSERT(*rcvr_locks == erts_proc_lc_my_proc_locks(rcvr));

    mp = message_alloc();

#ifdef ERTS_SMP
    if (!(*rcvr_locks & ERTS_PROC_LOCK_MSGQ)) {
	if (erts_smp_proc_trylock(rcvr, ERTS_PROC_LOCK_MSGQ) == EBUSY) {
	    ErtsProcLocks need_locks = ERTS_PROC_LOCK_MSGQ;
	    if (*rcvr_locks & ERTS_PROC_LOCK_STATUS) {
		erts_smp_proc_unlock(rcvr, ERTS_PROC_LOCK_STATUS);
		need_locks |= ERTS_PROC_LOCK_STATUS;
	    }
	    erts_smp_proc_lock(rcvr, need_locks);
	}
    }

    state = erts_smp_atomic32_read_acqb(&rcvr->state);
    if (state & (ERTS_PSFLG_PENDING_EXIT|ERTS_PSFLG_EXITING)) {
	if (!(*rcvr_locks & ERTS_PROC_LOCK_MSGQ))
	    erts_smp_proc_unlock(rcvr, ERTS_PROC_LOCK_MSGQ);
	/* Drop message if receiver is exiting or has a pending exit ... */
	if (is_not_nil(token)) {
	    ErlHeapFragment *heap_frag;
	    heap_frag = erts_dist_ext_trailer(mp->data.dist_ext);
	    erts_cleanup_offheap(&heap_frag->off_heap);
	}
	erts_free_dist_ext_copy(dist_ext);
	message_free(mp);
    }
    else
#endif
    if (IS_TRACED_FL(rcvr, F_TRACE_RECEIVE)) {
	/* Ahh... need to decode it in order to trace it... */
	ErlHeapFragment *mbuf;
	Eterm msg;
	if (!(*rcvr_locks & ERTS_PROC_LOCK_MSGQ))
	    erts_smp_proc_unlock(rcvr, ERTS_PROC_LOCK_MSGQ);
	message_free(mp);
	msg = erts_msg_distext2heap(rcvr, rcvr_locks, &mbuf, &token, dist_ext);
	if (is_value(msg))
#ifdef USE_VM_PROBES
            if (DTRACE_ENABLED(message_queued)) {
                DTRACE_CHARBUF(receiver_name, DTRACE_TERM_BUF_SIZE);

                dtrace_proc_str(rcvr, receiver_name);
                if (token != NIL && token != am_have_dt_utag) {
                    tok_label = signed_val(SEQ_TRACE_T_LABEL(token));
                    tok_lastcnt = signed_val(SEQ_TRACE_T_LASTCNT(token));
                    tok_serial = signed_val(SEQ_TRACE_T_SERIAL(token));
                }
                DTRACE6(message_queued,
                        receiver_name, size_object(msg), rcvr->msg.len,
                        tok_label, tok_lastcnt, tok_serial);
            }
#endif
	    erts_queue_message(rcvr, rcvr_locks, mbuf, msg, token);
    }
    else {
	/* Enqueue message on external format */

	ERL_MESSAGE_TERM(mp) = THE_NON_VALUE;
#ifdef USE_VM_PROBES
	ERL_MESSAGE_DT_UTAG(mp) = NIL;
	if (token == am_have_dt_utag) {
	    ERL_MESSAGE_TOKEN(mp) = NIL;
	} else {
#endif
	    ERL_MESSAGE_TOKEN(mp) = token;
#ifdef USE_VM_PROBES
	}
#endif
	mp->next = NULL;

#ifdef USE_VM_PROBES
        if (DTRACE_ENABLED(message_queued)) {
            DTRACE_CHARBUF(receiver_name, DTRACE_TERM_BUF_SIZE);

            dtrace_proc_str(rcvr, receiver_name);
            if (token != NIL && token != am_have_dt_utag) {
                tok_label = signed_val(SEQ_TRACE_T_LABEL(token));
                tok_lastcnt = signed_val(SEQ_TRACE_T_LASTCNT(token));
                tok_serial = signed_val(SEQ_TRACE_T_SERIAL(token));
            }
            /*
             * TODO: We don't know the real size of the external message here.
             *       -1 will appear to a D script as 4294967295.
             */
            DTRACE6(message_queued, receiver_name, -1, rcvr->msg.len + 1,
                    tok_label, tok_lastcnt, tok_serial);
        }
#endif
	mp->data.dist_ext = dist_ext;
	LINK_MESSAGE(rcvr, mp);

	if (!(*rcvr_locks & ERTS_PROC_LOCK_MSGQ))
	    erts_smp_proc_unlock(rcvr, ERTS_PROC_LOCK_MSGQ);

	erts_proc_notify_new_message(rcvr,
#ifdef ERTS_SMP
				     *rcvr_locks
#else
				     0
#endif
	    );
    }
}

/* Add a message last in message queue */
static Sint
queue_message(Process *c_p,
	      Process* receiver,
	      ErtsProcLocks *receiver_locks,
	      erts_aint32_t *receiver_state,
	      ErlHeapFragment* bp,
	      Eterm message,
	      Eterm seq_trace_token
#ifdef USE_VM_PROBES
		   , Eterm dt_utag
#endif
    )
{
    Sint res;
    ErlMessage* mp;
    int locked_msgq = 0;
    erts_aint_t state;

#ifndef ERTS_SMP
    ASSERT(bp != NULL || receiver->mbuf == NULL);
#endif

    ERTS_SMP_LC_ASSERT(*receiver_locks == erts_proc_lc_my_proc_locks(receiver));

    mp = message_alloc();

    if (receiver_state)
	state = *receiver_state;
    else
	state = erts_smp_atomic32_read_acqb(&receiver->state);

#ifdef ERTS_SMP

    if (state & (ERTS_PSFLG_EXITING|ERTS_PSFLG_PENDING_EXIT))
	goto exiting;

    if (!(*receiver_locks & ERTS_PROC_LOCK_MSGQ)) {
	if (erts_smp_proc_trylock(receiver, ERTS_PROC_LOCK_MSGQ) == EBUSY) {
	    ErtsProcLocks need_locks = ERTS_PROC_LOCK_MSGQ;
	    if (*receiver_locks & ERTS_PROC_LOCK_STATUS) {
		erts_smp_proc_unlock(receiver, ERTS_PROC_LOCK_STATUS);
		need_locks |= ERTS_PROC_LOCK_STATUS;
	    }
	    erts_smp_proc_lock(receiver, need_locks);
	}
	locked_msgq = 1;
	state = erts_smp_atomic32_read_nob(&receiver->state);
	if (receiver_state)
	    *receiver_state = state;
    }

#endif

    if (state & (ERTS_PSFLG_PENDING_EXIT|ERTS_PSFLG_EXITING)) {
#ifdef ERTS_SMP
    exiting:
#endif
	/* Drop message if receiver is exiting or has a pending exit... */
	if (locked_msgq)
	    erts_smp_proc_unlock(receiver, ERTS_PROC_LOCK_MSGQ);
	if (bp)
	    free_message_buffer(bp);
	message_free(mp);
	return 0;
    }

    ERL_MESSAGE_TERM(mp) = message;
    ERL_MESSAGE_TOKEN(mp) = seq_trace_token;
#ifdef USE_VM_PROBES
    ERL_MESSAGE_DT_UTAG(mp) = dt_utag;
#endif
    mp->next = NULL;
    mp->data.heap_frag = bp;

#ifndef ERTS_SMP
    res = receiver->msg.len;
#else
    res = receiver->msg_inq.len;
    if (*receiver_locks & ERTS_PROC_LOCK_MAIN) {
	/*
	 * We move 'in queue' to 'private queue' and place
	 * message at the end of 'private queue' in order
	 * to ensure that the 'in queue' doesn't contain
	 * references into the heap. By ensuring this,
	 * we don't need to include the 'in queue' in
	 * the root set when garbage collecting.
	 */
	res += receiver->msg.len;
	ERTS_SMP_MSGQ_MV_INQ2PRIVQ(receiver);
	LINK_MESSAGE_PRIVQ(receiver, mp);
    }
    else
#endif
    {
	LINK_MESSAGE(receiver, mp);
    }

#ifdef USE_VM_PROBES
    if (DTRACE_ENABLED(message_queued)) {
        DTRACE_CHARBUF(receiver_name, DTRACE_TERM_BUF_SIZE);
        Sint tok_label = 0;
        Sint tok_lastcnt = 0;
        Sint tok_serial = 0;

        dtrace_proc_str(receiver, receiver_name);
        if (seq_trace_token != NIL && is_tuple(seq_trace_token)) {
            tok_label = signed_val(SEQ_TRACE_T_LABEL(seq_trace_token));
            tok_lastcnt = signed_val(SEQ_TRACE_T_LASTCNT(seq_trace_token));
            tok_serial = signed_val(SEQ_TRACE_T_SERIAL(seq_trace_token));
        }
        DTRACE6(message_queued,
                receiver_name, size_object(message), receiver->msg.len,
                tok_label, tok_lastcnt, tok_serial);
    }
#endif

    if (IS_TRACED_FL(receiver, F_TRACE_RECEIVE))
	trace_receive(receiver, message);

    if (locked_msgq)
	erts_smp_proc_unlock(receiver, ERTS_PROC_LOCK_MSGQ);

    erts_proc_notify_new_message(receiver,
#ifdef ERTS_SMP
				 *receiver_locks
#else
				 0
#endif
	);

#ifndef ERTS_SMP
    ERTS_HOLE_CHECK(receiver);
#endif
    return res;
}

void
#ifdef USE_VM_PROBES
erts_queue_message_probe(Process* receiver, ErtsProcLocks *receiver_locks,
                         ErlHeapFragment* bp,
                         Eterm message, Eterm seq_trace_token, Eterm dt_utag)
#else
erts_queue_message(Process* receiver, ErtsProcLocks *receiver_locks,
                   ErlHeapFragment* bp,
                   Eterm message, Eterm seq_trace_token)
#endif
{
    queue_message(NULL,
		  receiver,
		  receiver_locks,
		  NULL,
		  bp,
		  message,
		  seq_trace_token
#ifdef USE_VM_PROBES
		  , dt_utag
#endif
	);
}

void
erts_link_mbuf_to_proc(struct process *proc, ErlHeapFragment *bp)
{
    Eterm* htop = HEAP_TOP(proc);

    link_mbuf_to_proc(proc, bp);
    if (htop < HEAP_LIMIT(proc)) {
	*htop = make_pos_bignum_header(HEAP_LIMIT(proc)-htop-1);
	HEAP_TOP(proc) = HEAP_LIMIT(proc);
    }
}

/*
 * Moves content of message buffer attached to a message into a heap.
 * The message buffer is deallocated.
 */
void
erts_move_msg_mbuf_to_heap(Eterm** hpp, ErlOffHeap* off_heap, ErlMessage *msg)
{
    struct erl_off_heap_header* oh;
    Eterm term, token, *fhp, *hp;
    Sint offs;
    Uint sz;
    ErlHeapFragment *bp;
#ifdef USE_VM_PROBES
    Eterm utag;
#endif

#ifdef HARD_DEBUG
    struct erl_off_heap_header* dbg_oh_start = off_heap->first;
    Eterm dbg_term, dbg_token;
    ErlHeapFragment *dbg_bp;
    Uint *dbg_hp, *dbg_thp_start;
    Uint dbg_term_sz, dbg_token_sz;
#ifdef USE_VM_PROBES
    Eterm dbg_utag;
    Uint dbg_utag_sz;
#endif
#endif

    bp = msg->data.heap_frag;
    term = ERL_MESSAGE_TERM(msg);
    token = ERL_MESSAGE_TOKEN(msg);
#ifdef USE_VM_PROBES
    utag = ERL_MESSAGE_DT_UTAG(msg);
#endif
    if (!bp) {
#ifdef USE_VM_PROBES
	ASSERT(is_immed(term) && is_immed(token) && is_immed(utag));
#else
	ASSERT(is_immed(term) && is_immed(token));
#endif
	return;
    }

#ifdef HARD_DEBUG
    dbg_term_sz = size_object(term);
    dbg_token_sz = size_object(token);
    dbg_bp = new_message_buffer(dbg_term_sz + dbg_token_sz);
#ifdef USE_VM_PROBES
    dbg_utag_sz = size_object(utag);
    dbg_bp = new_message_buffer(dbg_term_sz + dbg_token_sz + dbg_utag_sz );
#endif
    /*ASSERT(dbg_term_sz + dbg_token_sz == erts_msg_used_frag_sz(msg));
      Copied size may be smaller due to removed SubBins's or garbage.
      Copied size may be larger due to duplicated shared terms.
    */
    dbg_hp = dbg_bp->mem;
    dbg_term = copy_struct(term, dbg_term_sz, &dbg_hp, &dbg_bp->off_heap);
    dbg_token = copy_struct(token, dbg_token_sz, &dbg_hp, &dbg_bp->off_heap);
#ifdef USE_VM_PROBES
    dbg_utag = copy_struct(utag, dbg_utag_sz, &dbg_hp, &dbg_bp->off_heap);
#endif
   dbg_thp_start = *hpp;
#endif

    if (bp->next != NULL) {
	move_multi_frags(hpp, off_heap, bp, msg->m, 
#ifdef USE_VM_PROBES
			 3
#else
			 2
#endif
			 );
	goto copy_done;
    }

    OH_OVERHEAD(off_heap, bp->off_heap.overhead);
    sz = bp->used_size;

    ASSERT(is_immed(term) || in_heapfrag(ptr_val(term),bp));
    ASSERT(is_immed(token) || in_heapfrag(ptr_val(token),bp));

    fhp = bp->mem;
    hp = *hpp;
    offs = hp - fhp;

    oh = NULL;
    while (sz--) {
	Uint cpy_sz;
	Eterm val = *fhp++;

	switch (primary_tag(val)) {
	case TAG_PRIMARY_IMMED1:
	    *hp++ = val;
	    break;
	case TAG_PRIMARY_LIST:
	case TAG_PRIMARY_BOXED:
	    ASSERT(in_heapfrag(ptr_val(val), bp));
	    *hp++ = offset_ptr(val, offs);
	    break;
	case TAG_PRIMARY_HEADER:
	    *hp++ = val;
	    switch (val & _HEADER_SUBTAG_MASK) {
	    case ARITYVAL_SUBTAG:
		break;
	    case REFC_BINARY_SUBTAG:
	    case FUN_SUBTAG:
	    case EXTERNAL_PID_SUBTAG:
	    case EXTERNAL_PORT_SUBTAG:
	    case EXTERNAL_REF_SUBTAG:
		oh = (struct erl_off_heap_header*) (hp-1);
		cpy_sz = thing_arityval(val);
		goto cpy_words;
	    default:
		cpy_sz = header_arity(val);

	    cpy_words:
		ASSERT(sz >= cpy_sz);
		sz -= cpy_sz;
		while (cpy_sz >= 8) {
		    cpy_sz -= 8;
		    *hp++ = *fhp++;
		    *hp++ = *fhp++;
		    *hp++ = *fhp++;
		    *hp++ = *fhp++;
		    *hp++ = *fhp++;
		    *hp++ = *fhp++;
		    *hp++ = *fhp++;
		    *hp++ = *fhp++;
		}
		switch (cpy_sz) {
		case 7: *hp++ = *fhp++;
		case 6: *hp++ = *fhp++;
		case 5: *hp++ = *fhp++;
		case 4: *hp++ = *fhp++;
		case 3: *hp++ = *fhp++;
		case 2: *hp++ = *fhp++;
		case 1: *hp++ = *fhp++;
		default: break;
		}
		if (oh) {
		    /* Add to offheap list */
		    oh->next = off_heap->first;
		    off_heap->first = oh;
		    ASSERT(*hpp <= (Eterm*)oh);
		    ASSERT(hp > (Eterm*)oh);
		    oh = NULL;
		}
		break;
	    }
	    break;
	}
    }

    ASSERT(bp->used_size == hp - *hpp);
    *hpp = hp;

    if (is_not_immed(token)) {
	ASSERT(in_heapfrag(ptr_val(token), bp));
	ERL_MESSAGE_TOKEN(msg) = offset_ptr(token, offs);
#ifdef HARD_DEBUG
	ASSERT(dbg_thp_start <= ptr_val(ERL_MESSAGE_TOKEN(msg)));
	ASSERT(hp > ptr_val(ERL_MESSAGE_TOKEN(msg)));
#endif
    }

    if (is_not_immed(term)) {
	ASSERT(in_heapfrag(ptr_val(term),bp));
	ERL_MESSAGE_TERM(msg) = offset_ptr(term, offs);
#ifdef HARD_DEBUG
	ASSERT(dbg_thp_start <= ptr_val(ERL_MESSAGE_TERM(msg)));
	ASSERT(hp > ptr_val(ERL_MESSAGE_TERM(msg)));
#endif
    }
#ifdef USE_VM_PROBES
    if (is_not_immed(utag)) {
	ASSERT(in_heapfrag(ptr_val(utag), bp));
	ERL_MESSAGE_DT_UTAG(msg) = offset_ptr(utag, offs);
#ifdef HARD_DEBUG
	ASSERT(dbg_thp_start <= ptr_val(ERL_MESSAGE_DT_UTAG(msg)));
	ASSERT(hp > ptr_val(ERL_MESSAGE_DT_UTAG(msg)));
#endif
    }
#endif

copy_done:

#ifdef HARD_DEBUG
    {
	int i, j;
	ErlHeapFragment* frag;
	{
	    struct erl_off_heap_header* dbg_oh = off_heap->first;
	    i = j = 0;
	    while (dbg_oh != dbg_oh_start) {
		dbg_oh = dbg_oh->next;
		i++;
	    }
	    for (frag=bp; frag; frag=frag->next) {
		dbg_oh = frag->off_heap.first;
		while (dbg_oh) {
		    dbg_oh = dbg_oh->next;
		    j++;
		}
	    }
	    ASSERT(i == j);
	}
    }
#endif
	    

    bp->off_heap.first = NULL;
    free_message_buffer(bp);
    msg->data.heap_frag = NULL;

#ifdef HARD_DEBUG
    ASSERT(eq(ERL_MESSAGE_TERM(msg), dbg_term));
    ASSERT(eq(ERL_MESSAGE_TOKEN(msg), dbg_token));
#ifdef USE_VM_PROBES
    ASSERT(eq(ERL_MESSAGE_DT_UTAG(msg), dbg_utag));
#endif
    free_message_buffer(dbg_bp);
#endif

}


Uint
erts_msg_attached_data_size_aux(ErlMessage *msg)
{
    Sint sz;
    ASSERT(is_non_value(ERL_MESSAGE_TERM(msg)));
    ASSERT(msg->data.dist_ext);
    ASSERT(msg->data.dist_ext->heap_size < 0);

    sz = erts_decode_dist_ext_size(msg->data.dist_ext);
    if (sz < 0) {
	/* Bad external; remove it */
	if (is_not_nil(ERL_MESSAGE_TOKEN(msg))) {
	    ErlHeapFragment *heap_frag;
	    heap_frag = erts_dist_ext_trailer(msg->data.dist_ext);
	    erts_cleanup_offheap(&heap_frag->off_heap);
	}
	erts_free_dist_ext_copy(msg->data.dist_ext);
	msg->data.dist_ext = NULL;
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

void
erts_move_msg_attached_data_to_heap(ErtsHeapFactory* factory,
				    ErlMessage *msg)
{
    if (is_value(ERL_MESSAGE_TERM(msg)))
	erts_move_msg_mbuf_to_heap(&factory->hp, factory->off_heap, msg);
    else if (msg->data.dist_ext) {
	ASSERT(msg->data.dist_ext->heap_size >= 0);
	if (is_not_nil(ERL_MESSAGE_TOKEN(msg))) {
	    ErlHeapFragment *heap_frag;
	    heap_frag = erts_dist_ext_trailer(msg->data.dist_ext);
	    ERL_MESSAGE_TOKEN(msg) = copy_struct(ERL_MESSAGE_TOKEN(msg),
						 heap_frag->used_size,
						 &factory->hp,
						 factory->off_heap);
	    erts_cleanup_offheap(&heap_frag->off_heap);
	}
	ERL_MESSAGE_TERM(msg) = erts_decode_dist_ext(factory,
						     msg->data.dist_ext);
	erts_free_dist_ext_copy(msg->data.dist_ext);
	msg->data.dist_ext = NULL;
    }
    /* else: bad external detected when calculating size */
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
    ErlHeapFragment* bp = NULL;
    Eterm token = NIL;
    Sint res = 0;
#ifdef USE_VM_PROBES
    DTRACE_CHARBUF(sender_name, 64);
    DTRACE_CHARBUF(receiver_name, 64);
    Sint tok_label = 0;
    Sint tok_lastcnt = 0;
    Sint tok_serial = 0;
#endif
    BM_STOP_TIMER(system);
    BM_MESSAGE(message,sender,receiver);
    BM_START_TIMER(send);

 #ifdef USE_VM_PROBES
    *sender_name = *receiver_name = '\0';
   if (DTRACE_ENABLED(message_send)) {
        erts_snprintf(sender_name, sizeof(DTRACE_CHARBUF_NAME(sender_name)),
		      "%T", sender->common.id);
        erts_snprintf(receiver_name, sizeof(DTRACE_CHARBUF_NAME(receiver_name)),
		      "%T", receiver->common.id);
    }
#endif
    if (SEQ_TRACE_TOKEN(sender) != NIL && !(flags & ERTS_SND_FLG_NO_SEQ_TRACE)) {
        Eterm* hp;
	Eterm stoken = SEQ_TRACE_TOKEN(sender);
	Uint seq_trace_size = 0;
#ifdef USE_VM_PROBES
	Uint dt_utag_size = 0;
	Eterm utag = NIL;
#endif

	BM_SWAP_TIMER(send,size);
	msize = size_object(message);
	BM_SWAP_TIMER(size,send);

#ifdef USE_VM_PROBES
	if (stoken != am_have_dt_utag) {
#endif

	    seq_trace_update_send(sender);
	    seq_trace_output(stoken, message, SEQ_TRACE_SEND, 
			     receiver->common.id, sender);
	    seq_trace_size = 6; /* TUPLE5 */
#ifdef USE_VM_PROBES
	}
	if (DT_UTAG_FLAGS(sender) & DT_UTAG_SPREADING) {
	    dt_utag_size = size_object(DT_UTAG(sender));
	} else if (stoken == am_have_dt_utag ) {
	    stoken = NIL;
	}
#endif

	bp = new_message_buffer(msize + seq_trace_size 
#ifdef USE_VM_PROBES
				+ dt_utag_size
#endif
				);
	hp = bp->mem;

        BM_SWAP_TIMER(send,copy);
	token = copy_struct(stoken,
			    seq_trace_size,
			    &hp,
			    &bp->off_heap);

	message = copy_struct(message, msize, &hp, &bp->off_heap);
#ifdef USE_VM_PROBES
	if (DT_UTAG_FLAGS(sender) & DT_UTAG_SPREADING) {
	    utag = copy_struct(DT_UTAG(sender), dt_utag_size, &hp, &bp->off_heap);
#ifdef DTRACE_TAG_HARDDEBUG
	    erts_fprintf(stderr,
			 "Dtrace -> (%T) Spreading tag (%T) with "
			 "message %T!\r\n",sender->common.id, utag, message);
#endif
	}
#endif
        BM_MESSAGE_COPIED(msize);
        BM_SWAP_TIMER(copy,send);

#ifdef USE_VM_PROBES
        if (DTRACE_ENABLED(message_send)) {
	    if (stoken != NIL && stoken != am_have_dt_utag) {
		tok_label = signed_val(SEQ_TRACE_T_LABEL(stoken));
		tok_lastcnt = signed_val(SEQ_TRACE_T_LASTCNT(stoken));
		tok_serial = signed_val(SEQ_TRACE_T_SERIAL(stoken));
	    }
	    DTRACE6(message_send, sender_name, receiver_name,
		    msize, tok_label, tok_lastcnt, tok_serial);
        }
#endif
        res = queue_message(NULL,
			    receiver,
			    receiver_locks,
			    NULL,
			    bp,
			    message,
			    token
#ifdef USE_VM_PROBES
			    , utag
#endif
	    );
        BM_SWAP_TIMER(send,system);
    } else if (sender == receiver) {
	/* Drop message if receiver has a pending exit ... */
#ifdef ERTS_SMP
	ErtsProcLocks need_locks = (~(*receiver_locks)
				    & (ERTS_PROC_LOCK_MSGQ
				       | ERTS_PROC_LOCK_STATUS));
	if (need_locks) {
	    *receiver_locks |= need_locks;
	    if (erts_smp_proc_trylock(receiver, need_locks) == EBUSY) {
		if (need_locks == ERTS_PROC_LOCK_MSGQ) {
		    erts_smp_proc_unlock(receiver, ERTS_PROC_LOCK_STATUS);
		    need_locks = ERTS_PROC_LOCK_MSGQ|ERTS_PROC_LOCK_STATUS;
		}
		erts_smp_proc_lock(receiver, need_locks);
	    }
	}
	if (!ERTS_PROC_PENDING_EXIT(receiver))
#endif
	{
	    ErlMessage* mp = message_alloc();

            DTRACE6(message_send, sender_name, receiver_name,
                    size_object(message), tok_label, tok_lastcnt, tok_serial);
	    mp->data.attached = NULL;
	    ERL_MESSAGE_TERM(mp) = message;
	    ERL_MESSAGE_TOKEN(mp) = NIL;
#ifdef USE_VM_PROBES
	    ERL_MESSAGE_DT_UTAG(mp) = NIL;
#endif
	    mp->next = NULL;
	    /*
	     * We move 'in queue' to 'private queue' and place
	     * message at the end of 'private queue' in order
	     * to ensure that the 'in queue' doesn't contain
	     * references into the heap. By ensuring this,
	     * we don't need to include the 'in queue' in
	     * the root set when garbage collecting.
	     */
	    
	    ERTS_SMP_MSGQ_MV_INQ2PRIVQ(receiver);
	    LINK_MESSAGE_PRIVQ(receiver, mp);

	    res = receiver->msg.len;

	    if (IS_TRACED_FL(receiver, F_TRACE_RECEIVE)) {
		trace_receive(receiver, message);
	    }
	}
        BM_SWAP_TIMER(send,system);
    } else {
	ErlOffHeap *ohp;
        Eterm *hp;
	erts_aint32_t state;

	BM_SWAP_TIMER(send,size);
	msize = size_object(message);
	BM_SWAP_TIMER(size,send);
	hp = erts_alloc_message_heap_state(msize,
					   &bp,
					   &ohp,
					   receiver,
					   receiver_locks,
					   &state);
	BM_SWAP_TIMER(send,copy);
	message = copy_struct(message, msize, &hp, ohp);
	BM_MESSAGE_COPIED(msz);
	BM_SWAP_TIMER(copy,send);
        DTRACE6(message_send, sender_name, receiver_name,
                msize, tok_label, tok_lastcnt, tok_serial);
	res = queue_message(sender,
			    receiver,
			    receiver_locks,
			    &state,
			    bp,
			    message,
			    token
#ifdef USE_VM_PROBES
			    , NIL
#endif
	    );
        BM_SWAP_TIMER(send,system);
    }
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
    ErlHeapFragment* bp = NULL;

    if (token != NIL
#ifdef USE_VM_PROBES
	&& token != am_have_dt_utag
#endif
	) {

	ASSERT(is_tuple(token));
	sz_reason = size_object(reason);
	sz_token = size_object(token);
	sz_from = size_object(from);
	bp = new_message_buffer(sz_reason + sz_from + sz_token + 4);
	hp = bp->mem;
	mess = copy_struct(reason, sz_reason, &hp, &bp->off_heap);
	from_copy = copy_struct(from, sz_from, &hp, &bp->off_heap);
	save = TUPLE3(hp, am_EXIT, from_copy, mess);
	hp += 4;
	/* the trace token must in this case be updated by the caller */
	seq_trace_output(token, save, SEQ_TRACE_SEND, to->common.id, NULL);
	temptoken = copy_struct(token, sz_token, &hp, &bp->off_heap);
	erts_queue_message(to, to_locksp, bp, save, temptoken);
    } else {
	ErlOffHeap *ohp;
	sz_reason = size_object(reason);
	sz_from = IS_CONST(from) ? 0 : size_object(from);

	hp = erts_alloc_message_heap(sz_reason+sz_from+4,
				     &bp,
				     &ohp,
				     to,
				     to_locksp);

	mess = copy_struct(reason, sz_reason, &hp, ohp);
	from_copy = (IS_CONST(from)
		     ? from
		     : copy_struct(from, sz_from, &hp, ohp));
	save = TUPLE3(hp, am_EXIT, from_copy, mess);
	erts_queue_message(to, to_locksp, bp, save, NIL);
    }
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
    factory->mode     = FACTORY_HALLOC;
    factory->p        = p;
    factory->hp_start = HAlloc(p, size);
    factory->hp       = factory->hp_start;
    factory->hp_end   = factory->hp_start + size;
    factory->off_heap = &p->off_heap;
    factory->off_heap_saved.first    = p->off_heap.first;
    factory->off_heap_saved.overhead = p->off_heap.overhead;
    factory->heap_frags_saved = p->mbuf;
    factory->heap_frags = NULL; /* not used */
    factory->alloc_type = 0; /* not used */
}

void erts_factory_message_init(ErtsHeapFactory* factory,
			       Process* rp,
			       Eterm* hp,
			       ErlHeapFragment* bp)
{
    if (bp) {
	factory->mode     = FACTORY_HEAP_FRAGS;
	factory->p        = NULL;
	factory->hp_start = bp->mem;
	factory->hp       = hp ? hp : bp->mem;
	factory->hp_end   = bp->mem + bp->alloc_size;
	factory->off_heap = &bp->off_heap;
        factory->heap_frags = bp;
        factory->heap_frags_saved = bp;
        factory->alloc_type = ERTS_ALC_T_HEAP_FRAG;
        ASSERT(!bp->next);
    }
    else {
	factory->mode     = FACTORY_HALLOC;
	factory->p        = rp;
	factory->hp_start = hp;
	factory->hp       = hp;
	factory->hp_end   = HEAP_TOP(rp);
	factory->off_heap = &rp->off_heap;
        factory->heap_frags_saved = rp->mbuf;
        factory->heap_frags = NULL; /* not used */
        factory->alloc_type = 0; /* not used */
    }
    factory->off_heap_saved.first    = factory->off_heap->first;
    factory->off_heap_saved.overhead = factory->off_heap->overhead;

    ASSERT(factory->hp >= factory->hp_start && factory->hp <= factory->hp_end);
}

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

    case FACTORY_HEAP_FRAGS:
	bp = factory->heap_frags;

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

    case FACTORY_HEAP_FRAGS:
	bp = factory->heap_frags;

	if (bp) {
	    ASSERT(factory->hp >= bp->mem);
	    ASSERT(factory->hp <= factory->hp_end);
	    ASSERT(factory->hp_end == bp->mem + bp->alloc_size);

	    bp->used_size = factory->hp - bp->mem;
        }
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
    if (factory->mode == FACTORY_HEAP_FRAGS) {
        ErlHeapFragment* bp = factory->heap_frags;
        if (bp->next == NULL) {
            Uint used_sz = factory->hp - bp->mem;
            ASSERT(used_sz <= bp->alloc_size);
            factory->heap_frags = erts_resize_message_buffer(bp, used_sz,
                                                             brefs, brefs_size);
            factory->mode = FACTORY_CLOSED;
            return;
        }
        /*else we don't trim multi fragmented messages for now */
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
            if (factory->heap_frags_saved == NULL) { /* No heap frags when we started */
                ASSERT(factory->hp_start >= HEAP_START(factory->p));
                ASSERT(factory->hp_start <= HEAP_LIMIT(factory->p));

                HEAP_TOP(factory->p) = factory->hp_start;
            }
            else {
                ASSERT(factory->heap_frags_saved == factory->p->mbuf);
                if (factory->hp_start == factory->heap_frags_saved->mem) {
                    factory->p->mbuf = factory->p->mbuf->next;
                    ERTS_HEAP_FREE(ERTS_ALC_T_HEAP_FRAG, factory->heap_frags_saved,
                                   ERTS_HEAP_FRAG_SIZE(factory->heap_frags_saved->alloc_size));
                }
                else if (factory->hp_start != factory->hp_end) {
                    unsigned remains = factory->hp_start - factory->heap_frags_saved->mem;
                    ASSERT(remains > 0 && remains < factory->heap_frags_saved->used_size);
                    factory->heap_frags_saved->used_size = remains;
                }
            }
        }
        break;

    case FACTORY_HEAP_FRAGS:
        bp = factory->heap_frags;
        do {
            ErlHeapFragment* next_bp = bp->next;

            erts_cleanup_offheap(&bp->off_heap);
            ERTS_HEAP_FREE(factory->alloc_type, (void *) bp,
                           ERTS_HEAP_FRAG_SIZE(bp->size));
            bp = next_bp;
        }while (bp != NULL);
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

