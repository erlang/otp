/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2005-2012. All Rights Reserved.
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

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "erl_bif_timer.h"
#include "global.h"
#include "bif.h"
#include "error.h"
#include "big.h"
#include "erl_thr_progress.h"

/****************************************************************************
** BIF Timer support
****************************************************************************/

#define BTM_FLG_SL_TIMER	(((Uint32) 1) << 0)
#define BTM_FLG_CANCELED	(((Uint32) 1) << 1)
#define BTM_FLG_HEAD		(((Uint32) 1) << 2)
#define BTM_FLG_BYNAME		(((Uint32) 1) << 3)
#define BTM_FLG_WRAP		(((Uint32) 1) << 4)

struct ErtsBifTimer_ {
    struct {
	union {
	    ErtsBifTimer **head;
	    ErtsBifTimer *prev;
	} u;
	ErtsBifTimer *next;
    } tab;
    union {
	Eterm name;
	struct {
	    ErtsBifTimer *prev;
	    ErtsBifTimer *next;
	    Process *ess;
	} proc;
    } receiver;
    ErlTimer tm;
    ErlHeapFragment* bp;
    Uint32 flags;
    Eterm message;
    Uint32 ref_numbers[ERTS_REF_NUMBERS];
};

#ifdef SMALL_MEMORY
#define TIMER_HASH_VEC_SZ	3331
#define BTM_PREALC_SZ		10
#else
#define TIMER_HASH_VEC_SZ	10007
#define BTM_PREALC_SZ		100
#endif
static ErtsBifTimer **bif_timer_tab;  
static Uint no_bif_timers;


static erts_smp_rwmtx_t bif_timer_lock;

#define erts_smp_safe_btm_rwlock(P, L) \
	safe_btm_lock((P), (L), 1)
#define erts_smp_safe_btm_rlock(P, L) \
	safe_btm_lock((P), (L), 0)
#define erts_smp_btm_rwlock() \
	erts_smp_rwmtx_rwlock(&bif_timer_lock)
#define erts_smp_btm_tryrwlock() \
	erts_smp_rwmtx_tryrwlock(&bif_timer_lock)
#define erts_smp_btm_rwunlock() \
	erts_smp_rwmtx_rwunlock(&bif_timer_lock)
#define erts_smp_btm_rlock() \
	erts_smp_rwmtx_rlock(&bif_timer_lock)
#define erts_smp_btm_tryrlock() \
	erts_smp_rwmtx_tryrlock(&bif_timer_lock)
#define erts_smp_btm_runlock() \
	erts_smp_rwmtx_runlock(&bif_timer_lock)
#define erts_smp_btm_lock_init() \
	erts_smp_rwmtx_init(&bif_timer_lock, "bif_timers")


static ERTS_INLINE int
safe_btm_lock(Process *c_p, ErtsProcLocks c_p_locks, int rw_lock)
{
    ASSERT(c_p && c_p_locks);
#ifdef ERTS_SMP
    if ((rw_lock ? erts_smp_btm_tryrwlock() : erts_smp_btm_tryrlock()) != EBUSY)
	return 0;
    erts_smp_proc_unlock(c_p, c_p_locks);
    if (rw_lock)
	erts_smp_btm_rwlock();
    else
	erts_smp_btm_rlock();
    erts_smp_proc_lock(c_p, c_p_locks);
    if (ERTS_PROC_IS_EXITING(c_p)) {
	if (rw_lock)
	    erts_smp_btm_rwunlock();
	else
	    erts_smp_btm_runlock();
	return 1;
    }
#endif
    return 0;
}

ERTS_SCHED_PREF_PALLOC_IMPL(btm_pre, ErtsBifTimer, BTM_PREALC_SZ)

static ERTS_INLINE int
get_index(Uint32 *ref_numbers, Uint32 len)
{
    Uint32 hash;
    /* len can potentially be larger than ERTS_REF_NUMBERS
       if it has visited another node... */
    if (len > ERTS_REF_NUMBERS)
	len = ERTS_REF_NUMBERS;

#if ERTS_REF_NUMBERS != 3
#error "ERTS_REF_NUMBERS changed. Update me..."
#endif
    switch (len) {
    case 3: if (!ref_numbers[2]) len = 2;
    case 2: if (!ref_numbers[1]) len = 1;
    default:  break;
    }

    ASSERT(1 <= len && len <= ERTS_REF_NUMBERS);

    hash = block_hash((byte *) ref_numbers, len * sizeof(Uint32), 0x08d12e65);
    return (int) (hash % ((Uint32) TIMER_HASH_VEC_SZ));
}

static Eterm
create_ref(Uint *hp, Uint32 *ref_numbers, Uint32 len)
{
    Uint32 *datap;
    int i;


    if (len > ERTS_MAX_REF_NUMBERS) {
	/* Such large refs should no be able to appear in the emulator */
	erl_exit(1, "%s:%d: Internal error\n", __FILE__, __LINE__);
    }

#if defined(ARCH_64) && !HALFWORD_HEAP
    hp[0] = make_ref_thing_header(len/2 + 1);
    datap = (Uint32 *) &hp[1];
    *(datap++) = len;
#else
    hp[0] = make_ref_thing_header(len);
    datap = (Uint32 *) &hp[1];
#endif

    for (i = 0; i < len; i++)
	datap[i] = ref_numbers[i];

    return make_internal_ref(hp);
}

static int
eq_non_standard_ref_numbers(Uint32 *rn1, Uint32 len1, Uint32 *rn2, Uint32 len2)
{
#if defined(ARCH_64) && !HALFWORD_HEAP
#define MAX_REF_HEAP_SZ (1+(ERTS_MAX_REF_NUMBERS/2+1))
#else
#define MAX_REF_HEAP_SZ (1+ERTS_MAX_REF_NUMBERS)
#endif
    DeclareTmpHeapNoproc(r1_hp,(MAX_REF_HEAP_SZ * 2));
    Eterm *r2_hp = r1_hp +MAX_REF_HEAP_SZ;

    return eq(create_ref(r1_hp, rn1, len1), create_ref(r2_hp, rn2, len2));
#undef MAX_REF_HEAP_SZ
}

static ERTS_INLINE int
eq_ref_numbers(Uint32 *rn1, Uint32 len1, Uint32 *rn2, Uint32 len2)
{
    int res;
    if (len1 != ERTS_REF_NUMBERS || len2 != ERTS_REF_NUMBERS) {
	/* Can potentially happen, but will never... */
	return eq_non_standard_ref_numbers(rn1, len1, rn2, len2);
    }

#if ERTS_REF_NUMBERS != 3
#error "ERTS_REF_NUMBERS changed. Update me..."
#endif
    res = rn1[0] == rn2[0] && rn1[1] == rn2[1] && rn1[2] == rn2[2];

    ASSERT(res
	   ? eq_non_standard_ref_numbers(rn1, len1, rn2, len2)
	   : !eq_non_standard_ref_numbers(rn1, len1, rn2, len2));

    return res;
}

static ERTS_INLINE ErtsBifTimer *
tab_find(Eterm ref)
{
    Uint32 *ref_numbers = internal_ref_numbers(ref);
    Uint32 ref_numbers_len = internal_ref_no_of_numbers(ref);
    int ix = get_index(ref_numbers, ref_numbers_len);
    ErtsBifTimer* btm;

    for (btm = bif_timer_tab[ix]; btm; btm = btm->tab.next)
	if (eq_ref_numbers(ref_numbers, ref_numbers_len,
			   btm->ref_numbers, ERTS_REF_NUMBERS))
	    return btm;
    return NULL;
}

static ERTS_INLINE void
tab_remove(ErtsBifTimer* btm)
{
    if (btm->flags & BTM_FLG_HEAD) {
	*btm->tab.u.head = btm->tab.next;
	if (btm->tab.next) {
	    btm->tab.next->flags |= BTM_FLG_HEAD;
	    btm->tab.next->tab.u.head = btm->tab.u.head;
	}
    }
    else {
	btm->tab.u.prev->tab.next = btm->tab.next;
	if (btm->tab.next)
	    btm->tab.next->tab.u.prev = btm->tab.u.prev;
    }
    btm->flags |= BTM_FLG_CANCELED;
    ASSERT(no_bif_timers > 0);
    no_bif_timers--;
}

static ERTS_INLINE void
tab_insert(ErtsBifTimer* btm)
{
    int ix = get_index(btm->ref_numbers, ERTS_REF_NUMBERS);
    ErtsBifTimer* btm_list = bif_timer_tab[ix];

    if (btm_list) {
	btm_list->flags &= ~BTM_FLG_HEAD;
	btm_list->tab.u.prev = btm;
    }

    btm->flags |= BTM_FLG_HEAD;
    btm->tab.u.head = &bif_timer_tab[ix];
    btm->tab.next = btm_list;
    bif_timer_tab[ix] = btm;
    no_bif_timers++;
}

static ERTS_INLINE void
link_proc(Process *p, ErtsBifTimer* btm)
{
    btm->receiver.proc.ess = p;
    btm->receiver.proc.prev = NULL;
    btm->receiver.proc.next = p->u.bif_timers;
    if (p->u.bif_timers)	
	p->u.bif_timers->receiver.proc.prev = btm;
    p->u.bif_timers = btm;
}

static ERTS_INLINE void
unlink_proc(ErtsBifTimer* btm)
{
    if (btm->receiver.proc.prev)
	btm->receiver.proc.prev->receiver.proc.next = btm->receiver.proc.next;
    else
	btm->receiver.proc.ess->u.bif_timers = btm->receiver.proc.next;
    if (btm->receiver.proc.next)
	btm->receiver.proc.next->receiver.proc.prev = btm->receiver.proc.prev;
}

static void
bif_timer_cleanup(ErtsBifTimer* btm)
{
    ASSERT(btm);

    if (btm->bp)
	free_message_buffer(btm->bp);

    if (!btm_pre_free(btm)) {
	if (btm->flags & BTM_FLG_SL_TIMER)
	    erts_free(ERTS_ALC_T_SL_BIF_TIMER, (void *) btm);
	else
	    erts_free(ERTS_ALC_T_LL_BIF_TIMER, (void *) btm);
    }
}

static void
bif_timer_timeout(ErtsBifTimer* btm)
{
    ASSERT(btm);


    erts_smp_btm_rwlock();

    if (btm->flags & BTM_FLG_CANCELED) {
    /*
     * A concurrent cancel is ongoing. Do not send the timeout message,
     * but cleanup here since the cancel call-back won't be called.
     */
#ifndef ERTS_SMP
	ASSERT(0);
#endif
    }
    else {
	ErtsProcLocks rp_locks = 0;
	Process* rp;

	tab_remove(btm);

	ASSERT(!erts_get_current_process());

	if (btm->flags & BTM_FLG_BYNAME)
	    rp = erts_whereis_process(NULL, 0, btm->receiver.name, 0, 0);
	else {
	    rp = btm->receiver.proc.ess;
	    unlink_proc(btm);
	}

	if (rp) {
	    Eterm message;
	    ErlHeapFragment *bp;

	    bp = btm->bp;
	    btm->bp = NULL; /* Prevent cleanup of message buffer... */

	    if (!(btm->flags & BTM_FLG_WRAP))
		message = btm->message;
	    else {
#if ERTS_REF_NUMBERS != 3
#error "ERTS_REF_NUMBERS changed. Update me..."
#endif
		Eterm ref;
		Uint *hp;
		Uint wrap_size = REF_THING_SIZE + 4;
		message = btm->message;

		if (!bp) {
		    ErlOffHeap *ohp;
		    ASSERT(is_immed(message));
		    hp = erts_alloc_message_heap(wrap_size,
						 &bp,
						 &ohp,
						 rp,
						 &rp_locks);
		} else {
		    Eterm old_size = bp->used_size;
		    bp = erts_resize_message_buffer(bp, old_size + wrap_size,
						    &message, 1);
		    hp = &bp->mem[0] + old_size;
		}

		write_ref_thing(hp,
				btm->ref_numbers[0],
				btm->ref_numbers[1],
				btm->ref_numbers[2]);
		ref = make_internal_ref(hp);
		hp += REF_THING_SIZE;
		message = TUPLE3(hp, am_timeout, ref, message);
	    }

	    erts_queue_message(rp, &rp_locks, bp, message, NIL
#ifdef USE_VM_PROBES
			       , NIL
#endif
			       );
	    erts_smp_proc_unlock(rp, rp_locks);
	}
    }

    erts_smp_btm_rwunlock();

    bif_timer_cleanup(btm);
}

static Eterm
setup_bif_timer(Uint32 xflags,
		Process *c_p,
		Eterm time,
		Eterm receiver,
		Eterm message)
{
    Process *rp;
    ErtsBifTimer* btm;
    Uint timeout;
    Eterm ref;
    Uint32 *ref_numbers;
    
    if (!term_to_Uint(time, &timeout))
	return THE_NON_VALUE;
#if defined(ARCH_64) && !HALFWORD_HEAP
    if ((timeout >> 32) != 0)
	return THE_NON_VALUE;
#endif
    if (is_not_internal_pid(receiver) && is_not_atom(receiver))
	return THE_NON_VALUE;

    ref = erts_make_ref(c_p);

    if (is_atom(receiver))
	rp = NULL;
    else {
	rp = erts_pid2proc(c_p, ERTS_PROC_LOCK_MAIN,
			   receiver, ERTS_PROC_LOCK_MSGQ);
	if (!rp)
	    return ref;
    }

    if (timeout < ERTS_ALC_MIN_LONG_LIVED_TIME) {
	if (timeout < 1000) {
	    btm = btm_pre_alloc();
	    if (!btm)
		goto sl_timer_alloc;
	    btm->flags = 0;
	}
	else {
	sl_timer_alloc:
	    btm = (ErtsBifTimer *) erts_alloc(ERTS_ALC_T_SL_BIF_TIMER,
					      sizeof(ErtsBifTimer));
	    btm->flags = BTM_FLG_SL_TIMER;
	}
    }
    else {
	btm = (ErtsBifTimer *) erts_alloc(ERTS_ALC_T_LL_BIF_TIMER,
					 sizeof(ErtsBifTimer));
	btm->flags = 0;
    }

    if (rp) {
	link_proc(rp, btm);
	erts_smp_proc_unlock(rp, ERTS_PROC_LOCK_MSGQ);
    }
    else {
	ASSERT(is_atom(receiver));
	btm->receiver.name = receiver;
	btm->flags |= BTM_FLG_BYNAME;
    }

    btm->flags |= xflags;

    ref_numbers = internal_ref_numbers(ref);
    ASSERT(internal_ref_no_of_numbers(ref) == 3);
#if ERTS_REF_NUMBERS != 3
#error "ERTS_REF_NUMBERS changed. Update me..."
#endif
    btm->ref_numbers[0] = ref_numbers[0];
    btm->ref_numbers[1] = ref_numbers[1];
    btm->ref_numbers[2] = ref_numbers[2];

    ASSERT(eq_ref_numbers(btm->ref_numbers, ERTS_REF_NUMBERS,
			  ref_numbers, ERTS_REF_NUMBERS));

    if (is_immed(message)) {
	btm->bp = NULL;
	btm->message = message;
    }
    else {
	ErlHeapFragment* bp;
	Eterm* hp;
	Uint size;

	size = size_object(message);
	btm->bp = bp = new_message_buffer(size);
	hp = bp->mem;
	btm->message = copy_struct(message, size, &hp, &bp->off_heap);
    }

    tab_insert(btm);
    ASSERT(btm == tab_find(ref));
    btm->tm.active = 0; /* MUST be initalized */
    erts_set_timer(&btm->tm,
		  (ErlTimeoutProc) bif_timer_timeout,
		  (ErlCancelProc) bif_timer_cleanup,
		  (void *) btm,
		  timeout);
    return ref;
}

/* send_after(Time, Pid, Message) -> Ref */
BIF_RETTYPE send_after_3(BIF_ALIST_3)
{
    Eterm res;

    if (erts_smp_safe_btm_rwlock(BIF_P, ERTS_PROC_LOCK_MAIN))
	ERTS_BIF_EXITED(BIF_P);

    res = setup_bif_timer(0, BIF_P, BIF_ARG_1, BIF_ARG_2, BIF_ARG_3);

    erts_smp_btm_rwunlock();

    if (is_non_value(res)) {
	BIF_ERROR(BIF_P, BADARG);
    }
    else {
	ASSERT(is_internal_ref(res));
	BIF_RET(res);
    }
}

/* start_timer(Time, Pid, Message) -> Ref */
BIF_RETTYPE start_timer_3(BIF_ALIST_3)
{
    Eterm res;

    if (erts_smp_safe_btm_rwlock(BIF_P, ERTS_PROC_LOCK_MAIN))
	ERTS_BIF_EXITED(BIF_P);

    res = setup_bif_timer(BTM_FLG_WRAP, BIF_P, BIF_ARG_1, BIF_ARG_2, BIF_ARG_3);

    erts_smp_btm_rwunlock();

    if (is_non_value(res)) {
	BIF_ERROR(BIF_P, BADARG);
    }
    else {
	ASSERT(is_internal_ref(res));
	BIF_RET(res);
    }
}

/* cancel_timer(Ref) -> false | RemainingTime */
BIF_RETTYPE cancel_timer_1(BIF_ALIST_1)
{
    Eterm res;
    ErtsBifTimer *btm;

    if (is_not_internal_ref(BIF_ARG_1)) {
	if (is_ref(BIF_ARG_1)) {
	    BIF_RET(am_false);
	}
	BIF_ERROR(BIF_P, BADARG);
    }

    if (erts_smp_safe_btm_rwlock(BIF_P, ERTS_PROC_LOCK_MAIN))
	ERTS_BIF_EXITED(BIF_P);

    btm = tab_find(BIF_ARG_1);
    if (!btm || btm->flags & BTM_FLG_CANCELED) {
	erts_smp_btm_rwunlock();
	res = am_false;
    }
    else {
	Uint left = erts_time_left(&btm->tm);
	if (!(btm->flags & BTM_FLG_BYNAME)) {
	    erts_smp_proc_lock(btm->receiver.proc.ess, ERTS_PROC_LOCK_MSGQ);
	    unlink_proc(btm);
	    erts_smp_proc_unlock(btm->receiver.proc.ess, ERTS_PROC_LOCK_MSGQ);
	}
	tab_remove(btm);
	ASSERT(!tab_find(BIF_ARG_1));
	erts_cancel_timer(&btm->tm);
	erts_smp_btm_rwunlock();
	res = erts_make_integer(left, BIF_P);
    }

    BIF_RET(res);
}

/* read_timer(Ref) -> false | RemainingTime */
BIF_RETTYPE read_timer_1(BIF_ALIST_1)
{
    Eterm res;
    ErtsBifTimer *btm;

    if (is_not_internal_ref(BIF_ARG_1)) {
	if (is_ref(BIF_ARG_1)) {
	    BIF_RET(am_false);
	}
	BIF_ERROR(BIF_P, BADARG);
    }

    if (erts_smp_safe_btm_rlock(BIF_P, ERTS_PROC_LOCK_MAIN))
	ERTS_BIF_EXITED(BIF_P);

    btm = tab_find(BIF_ARG_1);
    if (!btm || btm->flags & BTM_FLG_CANCELED) {
	res = am_false;
    }
    else {
	Uint left = erts_time_left(&btm->tm);
	res = erts_make_integer(left, BIF_P);
    }

    erts_smp_btm_runlock();

    BIF_RET(res);
}

void
erts_print_bif_timer_info(int to, void *to_arg)
{
    int i;
    int lock = !ERTS_IS_CRASH_DUMPING;

    if (lock)
	erts_smp_btm_rlock();

    for (i = 0; i < TIMER_HASH_VEC_SZ; i++) {
	ErtsBifTimer *btm;
	for (btm = bif_timer_tab[i]; btm; btm = btm->tab.next) {
	    Eterm receiver = (btm->flags & BTM_FLG_BYNAME
			      ? btm->receiver.name
			      : btm->receiver.proc.ess->common.id);
	    erts_print(to, to_arg, "=timer:%T\n", receiver);
	    erts_print(to, to_arg, "Message: %T\n", btm->message);
	    erts_print(to, to_arg, "Time left: %u ms\n",
		       erts_time_left(&btm->tm));
	}
    }

    if (lock)
	erts_smp_btm_runlock();
}


void
erts_cancel_bif_timers(Process *p, ErtsProcLocks plocks)
{
    ErtsBifTimer *btm;

    if (erts_smp_btm_tryrwlock() == EBUSY) {
	erts_smp_proc_unlock(p, plocks);
	erts_smp_btm_rwlock();
	erts_smp_proc_lock(p, plocks);
    }

    btm = p->u.bif_timers;
    while (btm) {
	ErtsBifTimer *tmp_btm;
	ASSERT(!(btm->flags & BTM_FLG_CANCELED));
	tab_remove(btm);
	tmp_btm = btm;
	btm = btm->receiver.proc.next;
	erts_cancel_timer(&tmp_btm->tm);
    }

    p->u.bif_timers = NULL;

    erts_smp_btm_rwunlock();
}

void erts_bif_timer_init(void)
{
    int i;
    no_bif_timers = 0;
    init_btm_pre_alloc();
    erts_smp_btm_lock_init();
    bif_timer_tab = erts_alloc(ERTS_ALC_T_BIF_TIMER_TABLE,
			       sizeof(ErtsBifTimer *)*TIMER_HASH_VEC_SZ);
    for (i = 0; i < TIMER_HASH_VEC_SZ; ++i)
	bif_timer_tab[i] = NULL;
}

Uint
erts_bif_timer_memory_size(void)
{
    Uint res;
    int lock = !ERTS_IS_CRASH_DUMPING;

    if (lock)
	erts_smp_btm_rlock();

    res = (sizeof(ErtsBifTimer *)*TIMER_HASH_VEC_SZ
	   + no_bif_timers*sizeof(ErtsBifTimer));

    if (lock)
	erts_smp_btm_runlock();

    return res;
}


void
erts_bif_timer_foreach(void (*func)(Eterm, Eterm, ErlHeapFragment *, void *),
		       void *arg)
{
    int i;

    ERTS_SMP_LC_ASSERT(erts_smp_thr_progress_is_blocking());

    for (i = 0; i < TIMER_HASH_VEC_SZ; i++) {
	ErtsBifTimer *btm;
	for (btm = bif_timer_tab[i]; btm; btm = btm->tab.next) {
	    (*func)((btm->flags & BTM_FLG_BYNAME
		     ? btm->receiver.name
		     : btm->receiver.proc.ess->common.id),
		    btm->message,
		    btm->bp,
		    arg);
	}
    }
}
