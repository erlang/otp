/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1997-2010. All Rights Reserved.
 *
 * The contents of this file are subject to the Erlang Public License,
 * Version 1.1, (the "License"); you may not use this file except in
 * compliance with the License. You should have received a copy of the
 * Erlang Public License along with this software. If not, it can be
 * retrieved online at http://www.erlang.org/.
 *
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
 * the License for the specific language governing rights and limitations
 * under the License.
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
#include "erl_nmgc.h"

ERTS_SCHED_PREF_QUICK_ALLOC_IMPL(message,
				 ErlMessage,
				 ERL_MESSAGE_BUF_SZ,
				 ERTS_ALC_T_MSG_REF)

#if defined(DEBUG) && 0
#define HARD_DEBUG
#else
#undef HARD_DEBUG
#endif




static ERTS_INLINE int in_heapfrag(const Eterm* ptr, const ErlHeapFragment *bp)
{
    return ((unsigned)(ptr - bp->mem) < bp->used_size);
}


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

    /* ToDo: Make use of 'used_size' to avoid realloc
	when shrinking just a few words */

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

    if (size == bp->used_size)
	return bp;

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
    if (offheap->mso) {
	erts_cleanup_mso(offheap->mso);
    }
#ifndef HYBRID /* FIND ME! */
    if (offheap->funs) {
	erts_cleanup_funs(offheap->funs);
    }
#endif
    if (offheap->externals) {
	erts_cleanup_externals(offheap->externals);
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

	/* Move any binaries into the process */
	if (bp->off_heap.mso != NULL) {
	    ProcBin** next_p = &bp->off_heap.mso;
	    while (*next_p != NULL) {
		next_p = &((*next_p)->next);
	    }
	    *next_p = MSO(proc).mso;
	    MSO(proc).mso = bp->off_heap.mso;
	    bp->off_heap.mso = NULL;
	    MSO(proc).overhead += bp->off_heap.overhead;
	}

	/* Move any funs into the process */
#ifndef HYBRID
	if (bp->off_heap.funs != NULL) {
	    ErlFunThing** next_p = &bp->off_heap.funs;
	    while (*next_p != NULL) {
		next_p = &((*next_p)->next);
	    }
	    *next_p = MSO(proc).funs;
	    MSO(proc).funs = bp->off_heap.funs;
	    bp->off_heap.funs = NULL;
	}
#endif

	/* Move any external things into the process */
	if (bp->off_heap.externals != NULL) {
	    ExternalThing** next_p = &bp->off_heap.externals;
	    while (*next_p != NULL) {
		next_p = &((*next_p)->next);
	    }
	    *next_p = MSO(proc).externals;
	    MSO(proc).externals = bp->off_heap.externals;
	    bp->off_heap.externals = NULL;
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
    Eterm *hp_end = NULL;
    ErlOffHeap *ohp;
    Sint sz;

    *bpp = NULL;
    sz = erts_decode_dist_ext_size(dist_extp, 0);
    if (sz < 0)
	goto decode_error;
    if (is_not_nil(*tokenp)) {
	ErlHeapFragment *heap_frag = erts_dist_ext_trailer(dist_extp);
	tok_sz = heap_frag->used_size;
	sz += tok_sz;
    }
    if (pp)
	hp = erts_alloc_message_heap(sz, bpp, &ohp, pp, plcksp);
    else {
	*bpp = new_message_buffer(sz);
	hp = (*bpp)->mem;
	ohp = &(*bpp)->off_heap;
    }
    hp_end = hp + sz;
    msg = erts_decode_dist_ext(&hp, ohp, dist_extp);
    if (is_non_value(msg))
	goto decode_error;
    if (is_not_nil(*tokenp)) {
	ErlHeapFragment *heap_frag = erts_dist_ext_trailer(dist_extp);
	*tokenp = copy_struct(*tokenp, tok_sz, &hp, ohp);
	erts_cleanup_offheap(&heap_frag->off_heap);
    }
    erts_free_dist_ext_copy(dist_extp);
    if (hp_end != hp) {
	if (!(*bpp)) {
	    HRelease(pp, hp_end, hp);
	}
	else {
	    Uint final_size = hp - &(*bpp)->mem[0];
	    Eterm brefs[2] = {msg, *tokenp};
	    ASSERT(sz - (hp_end - hp) == final_size);
	    *bpp = erts_resize_message_buffer(*bpp, final_size, &brefs[0], 2);
	    msg = brefs[0];
	    *tokenp = brefs[1];
	}
    }
    return msg;

 decode_error:
    if (is_not_nil(*tokenp)) {
	ErlHeapFragment *heap_frag = erts_dist_ext_trailer(dist_extp);
	erts_cleanup_offheap(&heap_frag->off_heap);
    }
    erts_free_dist_ext_copy(dist_extp);
    if (*bpp) {
	free_message_buffer(*bpp);
	*bpp = NULL;
    }    
    else if (hp) {
	HRelease(pp, hp_end, hp);
    }
    return THE_NON_VALUE;
 }

static ERTS_INLINE void
notify_new_message(Process *receiver)
{
    ERTS_SMP_LC_ASSERT(ERTS_PROC_LOCK_STATUS
		       & erts_proc_lc_my_proc_locks(receiver));

    ACTIVATE(receiver);

    switch (receiver->status) {
    case P_GARBING:
	switch (receiver->gcstatus) {
	case P_SUSPENDED:
	    goto suspended;
	case P_WAITING:
	    goto waiting;
	default:
	    break;
	}
	break;
    case P_SUSPENDED:
    suspended:
	receiver->rstatus = P_RUNABLE;
	break;
    case P_WAITING:
    waiting:
	erts_add_to_runq(receiver);
	break;
    default:
	break;
    }
}

void
erts_queue_dist_message(Process *rcvr,
			ErtsProcLocks *rcvr_locks,
			ErtsDistExternal *dist_ext,
			Eterm token)
{
    ErlMessage* mp;
#ifdef ERTS_SMP
    ErtsProcLocks need_locks;
#endif

    ERTS_SMP_LC_ASSERT(*rcvr_locks == erts_proc_lc_my_proc_locks(rcvr));

    mp = message_alloc();

#ifdef ERTS_SMP
    need_locks = ~(*rcvr_locks) & (ERTS_PROC_LOCK_MSGQ|ERTS_PROC_LOCK_STATUS);
    if (need_locks) {
	*rcvr_locks |= need_locks;
	if (erts_smp_proc_trylock(rcvr, need_locks) == EBUSY) {
	    if (need_locks == ERTS_PROC_LOCK_MSGQ) {
		erts_smp_proc_unlock(rcvr, ERTS_PROC_LOCK_STATUS);
		need_locks = (ERTS_PROC_LOCK_MSGQ
			      | ERTS_PROC_LOCK_STATUS);
	    }
	    erts_smp_proc_lock(rcvr, need_locks);
	}
    }

    if (rcvr->is_exiting || ERTS_PROC_PENDING_EXIT(rcvr)) {
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
	message_free(mp);
	msg = erts_msg_distext2heap(rcvr, rcvr_locks, &mbuf, &token, dist_ext);
	if (is_value(msg))
	    erts_queue_message(rcvr, rcvr_locks, mbuf, msg, token);
    }
    else {
	/* Enqueue message on external format */

	ERL_MESSAGE_TERM(mp) = THE_NON_VALUE;
	ERL_MESSAGE_TOKEN(mp) = token;
	mp->next = NULL;

	mp->data.dist_ext = dist_ext;
	LINK_MESSAGE(rcvr, mp);

	notify_new_message(rcvr);
    }
}

/* Add a message last in message queue */
void
erts_queue_message(Process* receiver,
		   ErtsProcLocks *receiver_locks,
		   ErlHeapFragment* bp,
		   Eterm message,
		   Eterm seq_trace_token)
{
    ErlMessage* mp;
#ifdef ERTS_SMP
    ErtsProcLocks need_locks;
#else
    ASSERT(bp != NULL || receiver->mbuf == NULL);
#endif

    ERTS_SMP_LC_ASSERT(*receiver_locks == erts_proc_lc_my_proc_locks(receiver));

    mp = message_alloc();

#ifdef ERTS_SMP
    need_locks = ~(*receiver_locks) & (ERTS_PROC_LOCK_MSGQ
				       | ERTS_PROC_LOCK_STATUS);
    if (need_locks) {
	*receiver_locks |= need_locks;
	if (erts_smp_proc_trylock(receiver, need_locks) == EBUSY) {
	    if (need_locks == ERTS_PROC_LOCK_MSGQ) {
		erts_smp_proc_unlock(receiver, ERTS_PROC_LOCK_STATUS);
		need_locks = (ERTS_PROC_LOCK_MSGQ
			      | ERTS_PROC_LOCK_STATUS);
	    }
	    erts_smp_proc_lock(receiver, need_locks);
	}
    }

    if (receiver->is_exiting || ERTS_PROC_PENDING_EXIT(receiver)) {
	/* Drop message if receiver is exiting or has a pending
	 * exit ...
	 */
	if (bp)
	    free_message_buffer(bp);
	message_free(mp);
	return;
    }
#endif

    ERL_MESSAGE_TERM(mp) = message;
    ERL_MESSAGE_TOKEN(mp) = seq_trace_token;
    mp->next = NULL;
    mp->data.heap_frag = bp;

#ifdef ERTS_SMP
    if (*receiver_locks & ERTS_PROC_LOCK_MAIN) {
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
    }
    else {
	LINK_MESSAGE(receiver, mp);
    }
#else
    LINK_MESSAGE(receiver, mp);
#endif

    notify_new_message(receiver);

    if (IS_TRACED_FL(receiver, F_TRACE_RECEIVE)) {
	trace_receive(receiver, message);
    }
    
#ifndef ERTS_SMP
    ERTS_HOLE_CHECK(receiver);
#endif
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
    /* Unions for typecasts avoids warnings about type-punned pointers and aliasing */
    union {
	Uint** upp;
	ProcBin **pbpp;
	ErlFunThing **efpp;
	ExternalThing **etpp;
    } oh_list_pp, oh_el_next_pp;
    union {
	Uint *up;
	ProcBin *pbp;
	ErlFunThing *efp;
	ExternalThing *etp;
    } oh_el_p;
    Eterm term, token, *fhp, *hp;
    Sint offs;
    Uint sz;
    ErlHeapFragment *bp;

#ifdef HARD_DEBUG
    ProcBin *dbg_mso_start = off_heap->mso;
    ErlFunThing *dbg_fun_start = off_heap->funs;
    ExternalThing *dbg_external_start = off_heap->externals;
    Eterm dbg_term, dbg_token;
    ErlHeapFragment *dbg_bp;
    Uint *dbg_hp, *dbg_thp_start;
    Uint dbg_term_sz, dbg_token_sz;
#endif

    bp = msg->data.heap_frag;
    term = ERL_MESSAGE_TERM(msg);
    token = ERL_MESSAGE_TOKEN(msg);
    if (!bp) {
	ASSERT(is_immed(term) && is_immed(token));
	return;
    }

#ifdef HARD_DEBUG
    dbg_term_sz = size_object(term);
    dbg_token_sz = size_object(token);
    /*ASSERT(dbg_term_sz + dbg_token_sz == erts_msg_used_frag_sz(msg));
      Copied size may be smaller due to removed SubBins's or garbage.
      Copied size may be larger due to duplicated shared terms.
    */
    dbg_bp = new_message_buffer(dbg_term_sz + dbg_token_sz);
    dbg_hp = dbg_bp->mem;
    dbg_term = copy_struct(term, dbg_term_sz, &dbg_hp, &dbg_bp->off_heap);
    dbg_token = copy_struct(token, dbg_token_sz, &dbg_hp, &dbg_bp->off_heap);
    dbg_thp_start = *hpp;
#endif

    if (bp->next != NULL) {
	move_multi_frags(hpp, off_heap, bp, msg->m, 2);
	goto copy_done;
    }

    off_heap->overhead += bp->off_heap.overhead;
    sz = bp->used_size;

    ASSERT(is_immed(term) || in_heapfrag(ptr_val(term),bp));
    ASSERT(is_immed(token) || in_heapfrag(ptr_val(token),bp));

    fhp = bp->mem;
    hp = *hpp;
    offs = hp - fhp;

    oh_list_pp.upp = NULL;
    oh_el_next_pp.upp = NULL; /* Shut up compiler warning */
    oh_el_p.up = NULL; /* Shut up compiler warning */
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
		oh_list_pp.pbpp = &off_heap->mso;
		oh_el_p.up = (hp-1);
		oh_el_next_pp.pbpp = &(oh_el_p.pbp)->next;
		cpy_sz = thing_arityval(val);
		goto cpy_words;
	    case FUN_SUBTAG:
#ifndef HYBRID
		oh_list_pp.efpp = &off_heap->funs;
		oh_el_p.up = (hp-1);
		oh_el_next_pp.efpp = &(oh_el_p.efp)->next;
#endif
		cpy_sz = thing_arityval(val);
		goto cpy_words;
	    case EXTERNAL_PID_SUBTAG:
	    case EXTERNAL_PORT_SUBTAG:
	    case EXTERNAL_REF_SUBTAG:
		oh_list_pp.etpp = &off_heap->externals;
		oh_el_p.up = (hp-1);
		oh_el_next_pp.etpp =  &(oh_el_p.etp)->next;
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
		if (oh_list_pp.upp) {
#ifdef HARD_DEBUG
		    Uint *dbg_old_oh_list_p = *oh_list_pp.upp;
#endif
		    /* Add to offheap list */
		    *oh_el_next_pp.upp = *oh_list_pp.upp;
		    *oh_list_pp.upp = oh_el_p.up;
		    ASSERT(*hpp <= oh_el_p.up);
		    ASSERT(hp > oh_el_p.up);
#ifdef HARD_DEBUG
		    switch (val & _HEADER_SUBTAG_MASK) {
		    case REFC_BINARY_SUBTAG:
			ASSERT(off_heap->mso == *oh_list_pp.pbpp);
			ASSERT(off_heap->mso->next
			       == (ProcBin *) dbg_old_oh_list_p);
			break;
#ifndef HYBRID
		    case FUN_SUBTAG:
			ASSERT(off_heap->funs == *oh_list_pp.efpp);
			ASSERT(off_heap->funs->next
			       == (ErlFunThing *) dbg_old_oh_list_p);
			break;
#endif
		    case EXTERNAL_PID_SUBTAG:
		    case EXTERNAL_PORT_SUBTAG:
		    case EXTERNAL_REF_SUBTAG:
			ASSERT(off_heap->externals
			       == *oh_list_pp.etpp);
			ASSERT(off_heap->externals->next
			       == (ExternalThing *) dbg_old_oh_list_p);
			break;
		    default:
			ASSERT(0);
		    }
#endif
		    oh_list_pp.upp = NULL;


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

copy_done:

#ifdef HARD_DEBUG
    {
	int i, j;
	ErlHeapFragment* frag;
	{
	    ProcBin *mso = off_heap->mso;
	    i = j = 0;
	    while (mso != dbg_mso_start) {
		mso = mso->next;
		i++;
	    }
	    for (frag=bp; frag; frag=frag->next) {
		mso = frag->off_heap.mso;
		while (mso) {
		    mso = mso->next;
		    j++;
		}
	    }
	    ASSERT(i == j);
	}
	{
	    ErlFunThing *fun = off_heap->funs;
	    i = j = 0;
	    while (fun != dbg_fun_start) {
		fun = fun->next;
		i++;
	    }
	    for (frag=bp; frag; frag=frag->next) {
		fun = frag->off_heap.funs;
		while (fun) {
		    fun = fun->next;
		    j++;
		}
	    }
	    ASSERT(i == j);
	}
	{
	    ExternalThing *external = off_heap->externals;
	    i = j = 0;
	    while (external != dbg_external_start) {
		external = external->next;
		i++;
	    }
	    for (frag=bp; frag; frag=frag->next) {
		external = frag->off_heap.externals;
		while (external) {
		    external = external->next;
		    j++;
		}
	    }
	    ASSERT(i == j);
	}
    }
#endif
	    

    bp->off_heap.mso = NULL;
#ifndef HYBRID
    bp->off_heap.funs = NULL;
#endif
    bp->off_heap.externals = NULL;
    free_message_buffer(bp);
    msg->data.heap_frag = NULL;

#ifdef HARD_DEBUG
    ASSERT(eq(ERL_MESSAGE_TERM(msg), dbg_term));
    ASSERT(eq(ERL_MESSAGE_TOKEN(msg), dbg_token));
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

    sz = erts_decode_dist_ext_size(msg->data.dist_ext, 0);
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
erts_move_msg_attached_data_to_heap(Eterm **hpp, ErlOffHeap *ohp, ErlMessage *msg)
{
    if (is_value(ERL_MESSAGE_TERM(msg)))
	erts_move_msg_mbuf_to_heap(hpp, ohp, msg);
    else if (msg->data.dist_ext) {
	ASSERT(msg->data.dist_ext->heap_size >= 0);
	if (is_not_nil(ERL_MESSAGE_TOKEN(msg))) {
	    ErlHeapFragment *heap_frag;
	    heap_frag = erts_dist_ext_trailer(msg->data.dist_ext);
	    ERL_MESSAGE_TOKEN(msg) = copy_struct(ERL_MESSAGE_TOKEN(msg),
						 heap_frag->used_size,
						 hpp,
						 ohp);
	    erts_cleanup_offheap(&heap_frag->off_heap);
	}
	ERL_MESSAGE_TERM(msg) = erts_decode_dist_ext(hpp,
						     ohp,
						     msg->data.dist_ext);
	erts_free_dist_ext_copy(msg->data.dist_ext);
	msg->data.dist_ext = NULL;
    }
    /* else: bad external detected when calculating size */
}

/*
 * Send a local message when sender & receiver processes are known.
 */

void
erts_send_message(Process* sender,
		  Process* receiver,
		  ErtsProcLocks *receiver_locks,
		  Eterm message,
		  unsigned flags)
{
    Uint msize;
    ErlHeapFragment* bp = NULL;
    Eterm token = NIL;

    BM_STOP_TIMER(system);
    BM_MESSAGE(message,sender,receiver);
    BM_START_TIMER(send);

    if (SEQ_TRACE_TOKEN(sender) != NIL && !(flags & ERTS_SND_FLG_NO_SEQ_TRACE)) {
        Eterm* hp;

        BM_SWAP_TIMER(send,size);
	msize = size_object(message);
        BM_SWAP_TIMER(size,send);

	seq_trace_update_send(sender);
	seq_trace_output(SEQ_TRACE_TOKEN(sender), message, SEQ_TRACE_SEND, 
			 receiver->id, sender);
	bp = new_message_buffer(msize + 6 /* TUPLE5 */);
	hp = bp->mem;

        BM_SWAP_TIMER(send,copy);
	token = copy_struct(SEQ_TRACE_TOKEN(sender),
			    6 /* TUPLE5 */,
			    &hp,
			    &bp->off_heap);

	message = copy_struct(message, msize, &hp, &bp->off_heap);
        BM_MESSAGE_COPIED(msize);
        BM_SWAP_TIMER(copy,send);

        erts_queue_message(receiver,
			   receiver_locks,
			   bp,
			   message,
			   token);
        BM_SWAP_TIMER(send,system);
#ifdef HYBRID
    } else {
        ErlMessage* mp = message_alloc();
        BM_SWAP_TIMER(send,copy);
#ifdef INCREMENTAL
        /* TODO: During GC activate processes if the message relies in
         * the fromspace and the sender is active. During major
         * collections add the message to the gray stack if it relies
         * in the old generation and the sender is active and the
         * receiver is inactive.

        if (!IS_CONST(message) && (ma_gc_flags & GC_CYCLE) &&
            (ptr_val(message) >= inc_fromspc &&
            ptr_val(message) < inc_fromend) && INC_IS_ACTIVE(sender))
            INC_ACTIVATE(receiver);
        else if (!IS_CONST(message) && (ma_gc_flags & GC_CYCLE) &&
            (ptr_val(message) >= global_old_heap &&
            ptr_val(message) < global_old_hend) &&
            INC_IS_ACTIVE(sender) && !INC_IS_ACTIVE(receiver))
            Mark message in blackmap and add it to the gray stack
        */

         if (!IS_CONST(message))
            INC_ACTIVATE(receiver);
#endif
        LAZY_COPY(sender,message);
        BM_SWAP_TIMER(copy,send);
        ERL_MESSAGE_TERM(mp) = message;
        ERL_MESSAGE_TOKEN(mp) = NIL;
        mp->next = NULL;
	LINK_MESSAGE(receiver, mp);
        ACTIVATE(receiver);

        if (receiver->status == P_WAITING) {
            erts_add_to_runq(receiver);
        } else if (receiver->status == P_SUSPENDED) {
            receiver->rstatus = P_RUNABLE;
        }
        if (IS_TRACED_FL(receiver, F_TRACE_RECEIVE)) {
            trace_receive(receiver, message);
        }

        BM_SWAP_TIMER(send,system);
        return;
#else
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

	    mp->data.attached = NULL;
	    ERL_MESSAGE_TERM(mp) = message;
	    ERL_MESSAGE_TOKEN(mp) = NIL;
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

	    if (IS_TRACED_FL(receiver, F_TRACE_RECEIVE)) {
		trace_receive(receiver, message);
	    }
	}
        BM_SWAP_TIMER(send,system);
	return;
    } else {
#ifdef ERTS_SMP
	ErlOffHeap *ohp;
        Eterm *hp;
	BM_SWAP_TIMER(send,size);
	msize = size_object(message);
	BM_SWAP_TIMER(size,send);
	hp = erts_alloc_message_heap(msize,&bp,&ohp,receiver,receiver_locks);
	BM_SWAP_TIMER(send,copy);
	message = copy_struct(message, msize, &hp, ohp);
	BM_MESSAGE_COPIED(msz);
	BM_SWAP_TIMER(copy,send);
	erts_queue_message(receiver, receiver_locks, bp, message, token);
        BM_SWAP_TIMER(send,system);
#else
	ErlMessage* mp = message_alloc();
        Eterm *hp;
        BM_SWAP_TIMER(send,size);
	msize = size_object(message);
        BM_SWAP_TIMER(size,send);
	
	if (receiver->stop - receiver->htop <= msize) {
            BM_SWAP_TIMER(send,system);
	    erts_garbage_collect(receiver, msize, receiver->arg_reg, receiver->arity);
            BM_SWAP_TIMER(system,send);
	}
	hp = receiver->htop;
	receiver->htop = hp + msize;
        BM_SWAP_TIMER(send,copy);
	message = copy_struct(message, msize, &hp, &receiver->off_heap);
	BM_MESSAGE_COPIED(msize);
        BM_SWAP_TIMER(copy,send);
	ERL_MESSAGE_TERM(mp) = message;
	ERL_MESSAGE_TOKEN(mp) = NIL;
	mp->next = NULL;
	mp->data.attached = NULL;
	LINK_MESSAGE(receiver, mp);

	if (receiver->status == P_WAITING) {
	    erts_add_to_runq(receiver);
	} else if (receiver->status == P_SUSPENDED) {
	    receiver->rstatus = P_RUNABLE;
	}
	if (IS_TRACED_FL(receiver, F_TRACE_RECEIVE)) {
	    trace_receive(receiver, message);
	}
        BM_SWAP_TIMER(send,system);
#endif /* #ifndef ERTS_SMP */
	return;
#endif /* HYBRID */
    }
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

    if (token != NIL) {

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
	seq_trace_output(token, save, SEQ_TRACE_SEND, to->id, NULL);
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

