/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2007-2016. All Rights Reserved.
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
 * Description:	Impementation of Erlang process locks.
 *
 * Author: 	Rickard Green
 */

#ifndef ERTS_PROC_LOCK_TYPE__
#define ERTS_PROC_LOCK_TYPE__

#ifdef ERTS_ENABLE_LOCK_CHECK
#define ERTS_PROC_LOCK_DEBUG
#endif

#ifdef ERTS_ENABLE_LOCK_COUNT
#include "erl_lock_count.h"
#endif

#include "erl_smp.h"

#if defined(VALGRIND) || defined(ETHR_DISABLE_NATIVE_IMPLS)
#  define ERTS_PROC_LOCK_OWN_IMPL 0
#else
#  define ERTS_PROC_LOCK_OWN_IMPL 1
#endif

#define ERTS_PROC_LOCK_ATOMIC_IMPL 0
#define ERTS_PROC_LOCK_SPINLOCK_IMPL 0
#define ERTS_PROC_LOCK_MUTEX_IMPL 0

#if !ERTS_PROC_LOCK_OWN_IMPL
#define ERTS_PROC_LOCK_RAW_MUTEX_IMPL 1
#else
#define ERTS_PROC_LOCK_RAW_MUTEX_IMPL 0

#if defined(ETHR_HAVE_32BIT_NATIVE_ATOMIC_OPS)
#  undef ERTS_PROC_LOCK_ATOMIC_IMPL
#  define ERTS_PROC_LOCK_ATOMIC_IMPL 1
#elif defined(ETHR_HAVE_NATIVE_SPINLOCKS)
#  undef ERTS_PROC_LOCK_SPINLOCK_IMPL
#  define ERTS_PROC_LOCK_SPINLOCK_IMPL 1
#else
#  undef ERTS_PROC_LOCK_MUTEX_IMPL
#  define ERTS_PROC_LOCK_MUTEX_IMPL 1
#endif

#endif

#define ERTS_PROC_LOCK_MAX_BIT 5

typedef erts_aint32_t ErtsProcLocks;

typedef struct erts_proc_lock_t_ {
#if ERTS_PROC_LOCK_OWN_IMPL
#if ERTS_PROC_LOCK_ATOMIC_IMPL
    erts_smp_atomic32_t flags;
#else
    ErtsProcLocks flags;
#endif
    erts_tse_t *queue[ERTS_PROC_LOCK_MAX_BIT+1];
#ifdef ERTS_ENABLE_LOCK_COUNT
    erts_lcnt_lock_t lcnt_main;
    erts_lcnt_lock_t lcnt_link;
    erts_lcnt_lock_t lcnt_msgq;
    erts_lcnt_lock_t lcnt_btm;
    erts_lcnt_lock_t lcnt_status;
    erts_lcnt_lock_t lcnt_trace;
#endif
#elif ERTS_PROC_LOCK_RAW_MUTEX_IMPL
    erts_mtx_t main;
    erts_mtx_t link;
    erts_mtx_t msgq;
    erts_mtx_t btm;
    erts_mtx_t status;
    erts_mtx_t trace;
#else
#  error "no implementation"
#endif
#ifdef ERTS_PROC_LOCK_DEBUG
    erts_smp_atomic32_t locked[ERTS_PROC_LOCK_MAX_BIT+1];
#endif
} erts_proc_lock_t;

/* Process lock flags */

/*
 * Main lock:
 *   The main lock is held by the scheduler running a process. It
 *   is used to protect all fields in the process structure except
 *   for those fields protected by other process locks (follows).
 */
#define ERTS_PROC_LOCK_MAIN		(((ErtsProcLocks) 1) << 0)

/*
 * Link lock:
 *   Protects the following fields in the process structure:
 *   * nlinks
 *   * monitors
 *   * suspend_monitors
 */
#define ERTS_PROC_LOCK_LINK		(((ErtsProcLocks) 1) << 1)

/*
 * Message queue lock:
 *   Protects the following fields in the process structure:
 *   * msg_inq
 */
#define ERTS_PROC_LOCK_MSGQ		(((ErtsProcLocks) 1) << 2)

/*
 * Bif timer lock:
 *   Protects the following fields in the process structure:
 *   * bif_timers
 */
#define ERTS_PROC_LOCK_BTM		(((ErtsProcLocks) 1) << 3)

/*
 * Status lock:
 *   Protects the following fields in the process structure:
 *   * pending_suspenders
 *   * suspendee
 *   * sys_tasks
 *   * ...
 */
#define ERTS_PROC_LOCK_STATUS		(((ErtsProcLocks) 1) << 4)

/*
 * Trace message lock:
 *   Protects the order in which messages are sent
 *   from trace nifs. This lock is taken inside enif_send.
 *
 */
#define ERTS_PROC_LOCK_TRACE            (((ErtsProcLocks) 1) << ERTS_PROC_LOCK_MAX_BIT)

/*
 * Special fields:
 *
 *   The following fields are read only and can be read if at
 *   least one process lock (whichever one doesn't matter)
 *   is held, or if the process structure is guaranteed not to
 *   disappear by other means (e.g. pix lock is held):
 *     * id
 *
 *   The following fields are only allowed to be written if
 *   all process locks are held, and are allowed to be read if
 *   at least one process lock (whichever one doesn't matter)
 *   is held:
 *     * common.tracer
 *     * common.trace_flags
 *
 *   The following fields are only allowed to be accessed if
 *   both the schedule queue lock and at least one process lock
 *   (whichever one doesn't matter) are held:
 *     * prio
 *     * next
 *     * scheduler_flags
 */

/*
 * Other rules regarding process locking:
 *
 * Exiting processes:
 *   When changing state to exiting (ERTS_PSFLG_EXITING) on a process,
 *   you are required to take all process locks (ERTS_PROC_LOCKS_ALL).
 *   Thus, by holding at least one process lock (whichever one doesn't
 *   matter) you are guaranteed that the process won't exit until the
 *   lock you are holding has been released.
 *
 * Lock order:
 *   Process locks with low numeric values has to be locked before
 *   process locks with high numeric values. E.g., main locks has
 *   to be locked before message queue locks.
 *
 *   When process locks with the same numeric value are to be locked
 *   on multiple processes, locks on processes with low process ids
 *   have to be locked before locks on processes with high process
 *   ids. E.g., if the main and the message queue locks are to be
 *   locked on processes p1 and p2 and p1->common.id < p2->common.id,
 *   then locks should be locked in the following order:
 *     1. main lock on p1
 *     2. main lock on p2
 *     3. message queue lock on p1
 *     4. message queue lock on p2
 */

/* Other lock flags */
#define ERTS_PROC_LOCK_WAITER_SHIFT (ERTS_PROC_LOCK_MAX_BIT + 1)


/* ERTS_PROC_LOCKS_* are combinations of process locks */

#define ERTS_PROC_LOCKS_MSG_RECEIVE	ERTS_PROC_LOCK_MSGQ
#define ERTS_PROC_LOCKS_MSG_SEND	ERTS_PROC_LOCK_MSGQ
#define ERTS_PROC_LOCKS_XSIG_SEND	ERTS_PROC_LOCK_STATUS

#define ERTS_PROC_LOCKS_ALL \
  ((((ErtsProcLocks) 1) << (ERTS_PROC_LOCK_MAX_BIT + 1)) - 1)

#define ERTS_PROC_LOCKS_ALL_MINOR	(ERTS_PROC_LOCKS_ALL \
                                         & ~ERTS_PROC_LOCK_MAIN)


#define ERTS_PIX_LOCKS_BITS		10
#define ERTS_NO_OF_PIX_LOCKS		(1 << ERTS_PIX_LOCKS_BITS)


#endif /* #ifndef ERTS_PROC_LOCK_TYPE__ */

#ifndef ERTS_PROCESS_LOCK_ONLY_PROC_LOCK_TYPE__
#ifndef ERTS_PROC_LOCK_LOCK_CHECK__
#define  ERTS_PROC_LOCK_LOCK_CHECK__

/* Lock counter implemetation */

#ifdef ERTS_ENABLE_LOCK_POSITION
#define erts_smp_proc_lock__(P,I,L) erts_smp_proc_lock_x__(P,I,L,__FILE__,__LINE__)
#define erts_smp_proc_lock(P,L) erts_smp_proc_lock_x(P,L,__FILE__,__LINE__)
#endif

#if defined(ERTS_SMP) && defined (ERTS_ENABLE_LOCK_COUNT)

void erts_lcnt_proc_lock_init(Process *p);
void erts_lcnt_proc_lock_destroy(Process *p);
void erts_lcnt_proc_lock(erts_proc_lock_t *lock, ErtsProcLocks locks);
void erts_lcnt_proc_lock_post_x(erts_proc_lock_t *lock, ErtsProcLocks locks, char *file, unsigned int line);
void erts_lcnt_proc_lock_unaquire(erts_proc_lock_t *lock, ErtsProcLocks locks);
void erts_lcnt_proc_unlock(erts_proc_lock_t *lock, ErtsProcLocks locks);
void erts_lcnt_proc_trylock(erts_proc_lock_t *lock, ErtsProcLocks locks, int res);

void erts_lcnt_enable_proc_lock_count(int enable);

#endif /* ERTS_ENABLE_LOCK_COUNT*/



/* --- Process lock checking ----------------------------------------------- */

#if defined(ERTS_SMP) && defined(ERTS_ENABLE_LOCK_CHECK)
#define ERTS_SMP_CHK_NO_PROC_LOCKS \
  erts_proc_lc_chk_no_proc_locks(__FILE__, __LINE__)
#define ERTS_SMP_CHK_HAVE_ONLY_MAIN_PROC_LOCK(P) \
  erts_proc_lc_chk_only_proc_main((P))
void erts_proc_lc_lock(Process *p, ErtsProcLocks locks,
		       char *file, unsigned int line);
void erts_proc_lc_trylock(Process *p, ErtsProcLocks locks, int locked,
			  char *file, unsigned int line);
void erts_proc_lc_unlock(Process *p, ErtsProcLocks locks);
void erts_proc_lc_might_unlock(Process *p, ErtsProcLocks locks);
void erts_proc_lc_chk_have_proc_locks(Process *p, ErtsProcLocks locks);
void erts_proc_lc_chk_proc_locks(Process *p, ErtsProcLocks locks);
void erts_proc_lc_chk_only_proc_main(Process *p);
void erts_proc_lc_chk_only_proc(Process *p, ErtsProcLocks locks);
void erts_proc_lc_chk_no_proc_locks(char *file, int line);
ErtsProcLocks erts_proc_lc_my_proc_locks(Process *p);
int erts_proc_lc_trylock_force_busy(Process *p, ErtsProcLocks locks);
void erts_proc_lc_require_lock(Process *p, ErtsProcLocks locks,
			       char* file, unsigned int line);
void erts_proc_lc_unrequire_lock(Process *p, ErtsProcLocks locks);
#else
#define ERTS_SMP_CHK_NO_PROC_LOCKS
#define ERTS_SMP_CHK_HAVE_ONLY_MAIN_PROC_LOCK(P)
#endif

#endif /* #ifndef ERTS_PROC_LOCK_LOCK_CHECK__ */
#endif /* #ifndef ERTS_PROCESS_LOCK_ONLY_PROC_LOCK_TYPE__ */

#if !defined(ERTS_PROCESS_LOCK_ONLY_PROC_LOCK_TYPE__) \
    && !defined(ERTS_PROCESS_LOCK_ONLY_LOCK_CHECK_PROTO__)
#ifndef ERTS_PROCESS_LOCK_H__
#define ERTS_PROCESS_LOCK_H__

#ifdef ERTS_SMP

typedef struct {
    union {
	erts_mtx_t mtx;
	char buf[ERTS_ALC_CACHE_LINE_ALIGN_SIZE(sizeof(erts_mtx_t))];
    } u;
} erts_pix_lock_t;

#define ERTS_PID2PIXLOCK(PID) \
    (&erts_pix_locks[(internal_pid_data((PID)) & ((1 << ERTS_PIX_LOCKS_BITS) - 1))])

#if ERTS_PROC_LOCK_OWN_IMPL

#if ERTS_PROC_LOCK_ATOMIC_IMPL

#define ERTS_PROC_LOCK_FLGS_BAND_(L, MSK) \
  ((ErtsProcLocks) erts_smp_atomic32_read_band_nob(&(L)->flags, \
						   (erts_aint32_t) (MSK)))
#define ERTS_PROC_LOCK_FLGS_BOR_ACQB_(L, MSK) \
  ((ErtsProcLocks) erts_smp_atomic32_read_bor_acqb(&(L)->flags, \
						   (erts_aint32_t) (MSK)))
#define ERTS_PROC_LOCK_FLGS_CMPXCHG_ACQB_(L, NEW, EXPECTED) \
  ((ErtsProcLocks) erts_smp_atomic32_cmpxchg_acqb(&(L)->flags, \
						  (erts_aint32_t) (NEW), \
						  (erts_aint32_t) (EXPECTED)))
#define ERTS_PROC_LOCK_FLGS_CMPXCHG_RELB_(L, NEW, EXPECTED) \
  ((ErtsProcLocks) erts_smp_atomic32_cmpxchg_relb(&(L)->flags, \
						  (erts_aint32_t) (NEW), \
						  (erts_aint32_t) (EXPECTED)))
#define ERTS_PROC_LOCK_FLGS_READ_(L) \
  ((ErtsProcLocks) erts_smp_atomic32_read_nob(&(L)->flags))

#else /* no opt atomic ops */

ERTS_GLB_INLINE ErtsProcLocks erts_proc_lock_flags_band(erts_proc_lock_t *,
							ErtsProcLocks);
ERTS_GLB_INLINE ErtsProcLocks erts_proc_lock_flags_bor(erts_proc_lock_t *,
						       ErtsProcLocks);
ERTS_GLB_INLINE ErtsProcLocks erts_proc_lock_flags_cmpxchg(erts_proc_lock_t *,
							   ErtsProcLocks,
							   ErtsProcLocks);

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

ERTS_GLB_INLINE ErtsProcLocks
erts_proc_lock_flags_band(erts_proc_lock_t *lck, ErtsProcLocks mask)
{
    ErtsProcLocks res = lck->flags;
    lck->flags &= mask;
    return res;
}

ERTS_GLB_INLINE ErtsProcLocks
erts_proc_lock_flags_bor(erts_proc_lock_t *lck, ErtsProcLocks mask)
{
    ErtsProcLocks res = lck->flags;
    lck->flags |= mask;
    return res;
}

ERTS_GLB_INLINE ErtsProcLocks
erts_proc_lock_flags_cmpxchg(erts_proc_lock_t *lck, ErtsProcLocks new,
                             ErtsProcLocks expected)
{
    ErtsProcLocks res = lck->flags;
    if (res == expected)
        lck->flags = new;
    return res;
}

#endif

#define ERTS_PROC_LOCK_FLGS_BAND_(L, MSK) erts_proc_lock_flags_band((L), (MSK))
#define ERTS_PROC_LOCK_FLGS_BOR_ACQB_(L, MSK) erts_proc_lock_flags_bor((L), (MSK))
#define ERTS_PROC_LOCK_FLGS_CMPXCHG_ACQB_(L, NEW, EXPECTED) \
  erts_proc_lock_flags_cmpxchg((L), (NEW), (EXPECTED))
#define ERTS_PROC_LOCK_FLGS_CMPXCHG_RELB_(L, NEW, EXPECTED) \
  erts_proc_lock_flags_cmpxchg((L), (NEW), (EXPECTED))
#define ERTS_PROC_LOCK_FLGS_READ_(L) ((L)->flags)

#endif /* end no opt atomic ops */
#endif /* ERTS_PROC_LOCK_OWN_IMPL */

extern erts_pix_lock_t erts_pix_locks[ERTS_NO_OF_PIX_LOCKS];

void erts_init_proc_lock(int cpus);
void erts_proc_lock_prepare_proc_lock_waiter(void);
#if ERTS_PROC_LOCK_OWN_IMPL
void erts_proc_lock_failed(Process *,
			   erts_pix_lock_t *,
			   ErtsProcLocks,
			   ErtsProcLocks);
void erts_proc_unlock_failed(Process *,
			     erts_pix_lock_t *,
			     ErtsProcLocks);
#endif

ERTS_GLB_INLINE void erts_pix_lock(erts_pix_lock_t *);
ERTS_GLB_INLINE void erts_pix_unlock(erts_pix_lock_t *);
ERTS_GLB_INLINE int erts_lc_pix_lock_is_locked(erts_pix_lock_t *);

ERTS_GLB_INLINE ErtsProcLocks erts_smp_proc_raw_trylock__(Process *p,
							  ErtsProcLocks locks);
#ifdef ERTS_ENABLE_LOCK_POSITION
ERTS_GLB_INLINE void erts_smp_proc_lock_x__(Process *,
					    erts_pix_lock_t *,
					    ErtsProcLocks,
					    char *file, unsigned int line);
#else
ERTS_GLB_INLINE void erts_smp_proc_lock__(Process *,
					  erts_pix_lock_t *,
					  ErtsProcLocks);
#endif
ERTS_GLB_INLINE void erts_smp_proc_unlock__(Process *,
					    erts_pix_lock_t *,
					    ErtsProcLocks);
ERTS_GLB_INLINE int erts_smp_proc_trylock__(Process *,
					    erts_pix_lock_t *,
					    ErtsProcLocks);

#ifdef ERTS_PROC_LOCK_DEBUG
ERTS_GLB_INLINE void erts_proc_lock_op_debug(Process *, ErtsProcLocks, int);
#endif

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

ERTS_GLB_INLINE void erts_pix_lock(erts_pix_lock_t *pixlck)
{
    ERTS_LC_ASSERT(pixlck);
    erts_mtx_lock(&pixlck->u.mtx);
}

ERTS_GLB_INLINE void erts_pix_unlock(erts_pix_lock_t *pixlck)
{
    ERTS_LC_ASSERT(pixlck);
    erts_mtx_unlock(&pixlck->u.mtx);
}

ERTS_GLB_INLINE int erts_lc_pix_lock_is_locked(erts_pix_lock_t *pixlck)
{
    return erts_lc_mtx_is_locked(&pixlck->u.mtx);
}

/*
 * Helper function for erts_smp_proc_lock__ and erts_smp_proc_trylock__.
 *
 * Attempts to grab all of 'locks' simultaneously.
 *
 * On success, returns zero.
 *
 * On failure, returns the p->locks at the moment it tried to grab them,
 * at least some of which will intersect with 'locks', so it is nonzero.
 *
 * This assumes p's pix lock is held on entry if !ERTS_PROC_LOCK_ATOMIC_IMPL.
 * Does not release the pix lock.
 */
ERTS_GLB_INLINE ErtsProcLocks
erts_smp_proc_raw_trylock__(Process *p, ErtsProcLocks locks)
{
#if ERTS_PROC_LOCK_OWN_IMPL
    ErtsProcLocks expct_lflgs = 0;

    while (1) {
        ErtsProcLocks lflgs = ERTS_PROC_LOCK_FLGS_CMPXCHG_ACQB_(&p->lock,
								expct_lflgs | locks,
								expct_lflgs);
        if (ERTS_LIKELY(lflgs == expct_lflgs)) {
            /* We successfully grabbed all locks. */
            return 0;
        }

        if (lflgs & locks) {
            /* Some locks we need are locked, give up. */
            return lflgs;
        }

        /* cmpxchg failed, try again (should be rare). */
        expct_lflgs = lflgs;
    }

#elif ERTS_PROC_LOCK_RAW_MUTEX_IMPL

    if (locks & ERTS_PROC_LOCK_MAIN)
	if (erts_mtx_trylock(&p->lock.main) == EBUSY)
	    goto busy_main;
    if (locks & ERTS_PROC_LOCK_LINK)
	if (erts_mtx_trylock(&p->lock.link) == EBUSY)
	    goto busy_link;
    if (locks & ERTS_PROC_LOCK_MSGQ)
	if (erts_mtx_trylock(&p->lock.msgq) == EBUSY)
	    goto busy_msgq;
    if (locks & ERTS_PROC_LOCK_BTM)
	if (erts_mtx_trylock(&p->lock.btm) == EBUSY)
	    goto busy_btm;
    if (locks & ERTS_PROC_LOCK_STATUS)
	if (erts_mtx_trylock(&p->lock.status) == EBUSY)
	    goto busy_status;
    if (locks & ERTS_PROC_LOCK_TRACE)
	if (erts_mtx_trylock(&p->lock.trace) == EBUSY)
	    goto busy_trace;

    return 0;

busy_trace:
    if (locks & ERTS_PROC_LOCK_TRACE)
	erts_mtx_unlock(&p->lock.trace);
busy_status:
    if (locks & ERTS_PROC_LOCK_BTM)
	erts_mtx_unlock(&p->lock.btm);
busy_btm:
    if (locks & ERTS_PROC_LOCK_MSGQ)
	erts_mtx_unlock(&p->lock.msgq);
busy_msgq:
    if (locks & ERTS_PROC_LOCK_LINK)
	erts_mtx_unlock(&p->lock.link);
busy_link:
    if (locks & ERTS_PROC_LOCK_MAIN)
	erts_mtx_unlock(&p->lock.main);
busy_main:

    return EBUSY;
#endif
}

ERTS_GLB_INLINE void
#ifdef ERTS_ENABLE_LOCK_POSITION
erts_smp_proc_lock_x__(Process *p,
		     erts_pix_lock_t *pix_lck,
		     ErtsProcLocks locks,
		     char *file, unsigned int line)
#else
erts_smp_proc_lock__(Process *p,
		     erts_pix_lock_t *pix_lck,
		     ErtsProcLocks locks)
#endif
{
#if ERTS_PROC_LOCK_OWN_IMPL

    ErtsProcLocks old_lflgs;
#if !ERTS_PROC_LOCK_ATOMIC_IMPL
    erts_pix_lock(pix_lck);
#endif

#ifdef ERTS_ENABLE_LOCK_COUNT
    erts_lcnt_proc_lock(&(p->lock), locks);
#endif

    ERTS_LC_ASSERT((locks & ~ERTS_PROC_LOCKS_ALL) == 0);

#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_proc_lc_lock(p, locks, file, line);
#endif

    old_lflgs = erts_smp_proc_raw_trylock__(p, locks);

    if (old_lflgs != 0) {
	/*
         * There is lock contention, so let erts_proc_lock_failed() deal
         * with it. Note that erts_proc_lock_failed() returns with
         * pix_lck unlocked.
         */
	erts_proc_lock_failed(p, pix_lck, locks, old_lflgs);
    }

#if !ERTS_PROC_LOCK_ATOMIC_IMPL
    else {
	ERTS_LC_ASSERT(locks == (ERTS_PROC_LOCK_FLGS_READ_(&p->lock) & locks));
	erts_pix_unlock(pix_lck);
    }
#endif

#ifdef ERTS_ENABLE_LOCK_COUNT
    erts_lcnt_proc_lock_post_x(&(p->lock), locks, file, line);
#endif

#ifdef ERTS_PROC_LOCK_DEBUG
    erts_proc_lock_op_debug(p, locks, 1);
#endif

#if ERTS_PROC_LOCK_ATOMIC_IMPL
    ETHR_COMPILER_BARRIER;
#endif

#elif ERTS_PROC_LOCK_RAW_MUTEX_IMPL
    if (locks & ERTS_PROC_LOCK_MAIN)
	erts_mtx_lock(&p->lock.main);
    if (locks & ERTS_PROC_LOCK_LINK)
	erts_mtx_lock(&p->lock.link);
    if (locks & ERTS_PROC_LOCK_MSGQ)
	erts_mtx_lock(&p->lock.msgq);
    if (locks & ERTS_PROC_LOCK_BTM)
	erts_mtx_lock(&p->lock.btm);
    if (locks & ERTS_PROC_LOCK_STATUS)
	erts_mtx_lock(&p->lock.status);
    if (locks & ERTS_PROC_LOCK_TRACE)
	erts_mtx_lock(&p->lock.trace);

#ifdef ERTS_PROC_LOCK_DEBUG
    erts_proc_lock_op_debug(p, locks, 1);
#endif

#endif
}

ERTS_GLB_INLINE void
erts_smp_proc_unlock__(Process *p,
		       erts_pix_lock_t *pix_lck,
		       ErtsProcLocks locks)
{
#if ERTS_PROC_LOCK_OWN_IMPL
    ErtsProcLocks old_lflgs;

#if ERTS_PROC_LOCK_ATOMIC_IMPL
    ETHR_COMPILER_BARRIER;
#endif

#ifdef ERTS_ENABLE_LOCK_COUNT
    erts_lcnt_proc_unlock(&(p->lock), locks);
#endif

#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_proc_lc_unlock(p, locks);
#endif
#ifdef ERTS_PROC_LOCK_DEBUG
    erts_proc_lock_op_debug(p, locks, 0);
#endif

#if !ERTS_PROC_LOCK_ATOMIC_IMPL
    erts_pix_lock(pix_lck);
#endif

    old_lflgs = ERTS_PROC_LOCK_FLGS_READ_(&p->lock);

    ERTS_LC_ASSERT((locks & ~ERTS_PROC_LOCKS_ALL) == 0);
    ERTS_LC_ASSERT(locks == (old_lflgs & locks));

    while (1) {
        /*
         * We'll atomically unlock every lock that has no waiter.
         * If any locks with waiters remain we'll let
         * erts_proc_unlock_failed() deal with them.
         */
        ErtsProcLocks wait_locks =
            (old_lflgs >> ERTS_PROC_LOCK_WAITER_SHIFT) & locks;

        /* What p->lock will look like with all non-waited locks released. */
        ErtsProcLocks want_lflgs = old_lflgs & (wait_locks | ~locks);

        if (want_lflgs != old_lflgs) {
            ErtsProcLocks new_lflgs =
                ERTS_PROC_LOCK_FLGS_CMPXCHG_RELB_(&p->lock, want_lflgs, old_lflgs);

            if (new_lflgs != old_lflgs) {
                /* cmpxchg failed, try again. */
                old_lflgs = new_lflgs;
                continue;
            }
        }

        /* We have successfully unlocked every lock with no waiter. */

        if (want_lflgs & locks) {
            /* Locks with waiters remain. */
            /* erts_proc_unlock_failed() returns with pix_lck unlocked. */
            erts_proc_unlock_failed(p, pix_lck, want_lflgs & locks);
        }
        else {
#if !ERTS_PROC_LOCK_ATOMIC_IMPL
            erts_pix_unlock(pix_lck);
#endif
        }

        break;
    }

#elif ERTS_PROC_LOCK_RAW_MUTEX_IMPL

#ifdef ERTS_PROC_LOCK_DEBUG
    erts_proc_lock_op_debug(p, locks, 0);
#endif

    if (locks & ERTS_PROC_LOCK_TRACE)
	erts_mtx_unlock(&p->lock.trace);
    if (locks & ERTS_PROC_LOCK_STATUS)
	erts_mtx_unlock(&p->lock.status);
    if (locks & ERTS_PROC_LOCK_BTM)
	erts_mtx_unlock(&p->lock.btm);
    if (locks & ERTS_PROC_LOCK_MSGQ)
	erts_mtx_unlock(&p->lock.msgq);
    if (locks & ERTS_PROC_LOCK_LINK)
	erts_mtx_unlock(&p->lock.link);
    if (locks & ERTS_PROC_LOCK_MAIN)
	erts_mtx_unlock(&p->lock.main);
#endif

}

ERTS_GLB_INLINE int
erts_smp_proc_trylock__(Process *p,
			erts_pix_lock_t *pix_lck,
			ErtsProcLocks locks)
{
#if ERTS_PROC_LOCK_OWN_IMPL
    int res;

#ifdef ERTS_ENABLE_LOCK_CHECK
    ERTS_LC_ASSERT((locks & ~ERTS_PROC_LOCKS_ALL) == 0);
    if (erts_proc_lc_trylock_force_busy(p, locks)) {
	res = EBUSY; /* Make sure caller can handle the situation without
			causing a lock order violation to occur */
    }
    else
#endif
    {

#if !ERTS_PROC_LOCK_ATOMIC_IMPL
	erts_pix_lock(pix_lck);
#endif

	if (erts_smp_proc_raw_trylock__(p, locks) != 0) {
	    /* Didn't get all locks... */
	    res = EBUSY;

#if !ERTS_PROC_LOCK_ATOMIC_IMPL
	    erts_pix_unlock(pix_lck);
#endif
	}
	else {
	    res = 0;

	    ERTS_LC_ASSERT(locks
			   == (ERTS_PROC_LOCK_FLGS_READ_(&p->lock) & locks));

#if !ERTS_PROC_LOCK_ATOMIC_IMPL
	    erts_pix_unlock(pix_lck);
#endif

#ifdef ERTS_PROC_LOCK_DEBUG
	    erts_proc_lock_op_debug(p, locks, 1);
#endif
	}
    }
#ifdef ERTS_ENABLE_LOCK_COUNT
    erts_lcnt_proc_trylock(&(p->lock), locks, res);
#endif

#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_proc_lc_trylock(p, locks, res == 0, __FILE__, __LINE__);
#endif

#if ERTS_PROC_LOCK_ATOMIC_IMPL
    ETHR_COMPILER_BARRIER;
#endif
    return res;

#elif ERTS_PROC_LOCK_RAW_MUTEX_IMPL
    if (erts_smp_proc_raw_trylock__(p, locks) != 0)
	return EBUSY;
    else {
#ifdef ERTS_PROC_LOCK_DEBUG
	erts_proc_lock_op_debug(p, locks, 1);
#endif
	return 0;
    }
#endif
}

#ifdef ERTS_PROC_LOCK_DEBUG
ERTS_GLB_INLINE void 
erts_proc_lock_op_debug(Process *p, ErtsProcLocks locks, int locked)
{
    int i;
    for (i = 0; i <= ERTS_PROC_LOCK_MAX_BIT; i++) {
	ErtsProcLocks lock = ((ErtsProcLocks) 1) << i;
	if (locks & lock) {
	    erts_aint32_t lock_count;
	    if (locked) {
		lock_count = erts_smp_atomic32_inc_read_nob(&p->lock.locked[i]);
		ERTS_LC_ASSERT(lock_count == 1);
	    }
	    else {
		lock_count = erts_smp_atomic32_dec_read_nob(&p->lock.locked[i]);
		ERTS_LC_ASSERT(lock_count == 0);
	    }
	}
    }
}
#endif

#endif /* #if ERTS_GLB_INLINE_INCL_FUNC_DEF */

#endif /* ERTS_SMP */

#ifdef ERTS_ENABLE_LOCK_POSITION
ERTS_GLB_INLINE void erts_smp_proc_lock_x(Process *, ErtsProcLocks, char *file, unsigned int line);
#else
ERTS_GLB_INLINE void erts_smp_proc_lock(Process *, ErtsProcLocks);
#endif
ERTS_GLB_INLINE void erts_smp_proc_unlock(Process *, ErtsProcLocks);
ERTS_GLB_INLINE int erts_smp_proc_trylock(Process *, ErtsProcLocks);

ERTS_GLB_INLINE void erts_proc_inc_refc(Process *);
ERTS_GLB_INLINE void erts_proc_dec_refc(Process *);
ERTS_GLB_INLINE void erts_proc_add_refc(Process *, Sint);
ERTS_GLB_INLINE Sint erts_proc_read_refc(Process *);

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

ERTS_GLB_INLINE void
#ifdef ERTS_ENABLE_LOCK_POSITION
erts_smp_proc_lock_x(Process *p, ErtsProcLocks locks, char *file, unsigned int line)
#else
erts_smp_proc_lock(Process *p, ErtsProcLocks locks)
#endif 
{
#if defined(ERTS_SMP) && defined(ERTS_ENABLE_LOCK_POSITION)
    erts_smp_proc_lock_x__(p,
#if ERTS_PROC_LOCK_ATOMIC_IMPL
			 NULL,
#else
			 ERTS_PID2PIXLOCK(p->common.id),
#endif /*ERTS_PROC_LOCK_ATOMIC_IMPL*/
			 locks, file, line);
#elif defined(ERTS_SMP)
    erts_smp_proc_lock__(p,
#if ERTS_PROC_LOCK_ATOMIC_IMPL
			 NULL,
#else
			 ERTS_PID2PIXLOCK(p->common.id),
#endif /*ERTS_PROC_LOCK_ATOMIC_IMPL*/
			 locks);
#endif /*ERTS_SMP*/
}

ERTS_GLB_INLINE void
erts_smp_proc_unlock(Process *p, ErtsProcLocks locks)
{
#ifdef ERTS_SMP
    erts_smp_proc_unlock__(p,
#if ERTS_PROC_LOCK_ATOMIC_IMPL
			   NULL,
#else
			   ERTS_PID2PIXLOCK(p->common.id),
#endif
			   locks);
#endif
}

ERTS_GLB_INLINE int
erts_smp_proc_trylock(Process *p, ErtsProcLocks locks)
{
#ifndef ERTS_SMP
    return 0;
#else
    return erts_smp_proc_trylock__(p,
#if ERTS_PROC_LOCK_ATOMIC_IMPL
				   NULL,
#else
				   ERTS_PID2PIXLOCK(p->common.id),
#endif
				   locks);
#endif
}

ERTS_GLB_INLINE void erts_proc_inc_refc(Process *p)
{
    ASSERT(!(erts_smp_atomic32_read_nob(&p->state) & ERTS_PSFLG_PROXY));
#ifdef ERTS_SMP
    erts_ptab_atmc_inc_refc(&p->common);
#else
    erts_ptab_inc_refc(&p->common);
#endif
}

ERTS_GLB_INLINE void erts_proc_dec_refc(Process *p)
{
    Sint referred;
    ASSERT(!(erts_smp_atomic32_read_nob(&p->state) & ERTS_PSFLG_PROXY));
#ifdef ERTS_SMP
    referred = erts_ptab_atmc_dec_test_refc(&p->common);
#else
    referred = erts_ptab_dec_test_refc(&p->common);
#endif
    if (!referred) {
	ASSERT(ERTS_PROC_IS_EXITING(p));
	erts_free_proc(p);
    }
}

ERTS_GLB_INLINE void erts_proc_add_refc(Process *p, Sint add_refc)
{
    Sint referred;
    ASSERT(!(erts_smp_atomic32_read_nob(&p->state) & ERTS_PSFLG_PROXY));
#ifdef ERTS_SMP
    referred = erts_ptab_atmc_add_test_refc(&p->common, add_refc);
#else
    referred = erts_ptab_add_test_refc(&p->common, add_refc);
#endif
    if (!referred) {
	ASSERT(ERTS_PROC_IS_EXITING(p));
	erts_free_proc(p);
    }
}

ERTS_GLB_INLINE Sint erts_proc_read_refc(Process *p)
{
    ASSERT(!(erts_smp_atomic32_read_nob(&p->state) & ERTS_PSFLG_PROXY));
#ifdef ERTS_SMP
    return erts_ptab_atmc_read_refc(&p->common);
#else
    return erts_ptab_read_refc(&p->common);
#endif
}

#endif /* #if ERTS_GLB_INLINE_INCL_FUNC_DEF */

#ifdef ERTS_SMP
void erts_proc_lock_init(Process *);
void erts_proc_lock_fin(Process *);
void erts_proc_safelock(Process *a_proc,
			ErtsProcLocks a_have_locks,
			ErtsProcLocks a_need_locks,
			Process *b_proc,
			ErtsProcLocks b_have_locks,
			ErtsProcLocks b_need_locks);
#endif

/*
 * --- Process table lookup ------------------------------------------------
 *
 * erts_pid2proc() and friends looks up the process structure of a pid
 * and at the same time acquires process locks in the smp case. Locks
 * on currently executing process and looked up process are taken according
 * to the lock order, i.e., locks on currently executing process may have
 * been released and reacquired.
 *
 * erts_pid2proc_opt() currently accepts the following flags:
 *   ERTS_P2P_FLG_ALLOW_OTHER_X    Lookup process even if it currently
 *                                 is exiting.
 */

#define ERTS_P2P_FLG_ALLOW_OTHER_X	(1 <<  0)
#define ERTS_P2P_FLG_TRY_LOCK		(1 <<  1)
#define ERTS_P2P_FLG_INC_REFC		(1 <<  2)

#define ERTS_PROC_LOCK_BUSY ((Process *) &erts_invalid_process)

#define erts_pid2proc(PROC, HL, PID, NL) \
  erts_pid2proc_opt((PROC), (HL), (PID), (NL), 0)


ERTS_GLB_INLINE Process *erts_pix2proc(int ix);
ERTS_GLB_INLINE Process *erts_proc_lookup_raw(Eterm pid);
ERTS_GLB_INLINE Process *erts_proc_lookup(Eterm pid);

#ifndef ERTS_SMP
ERTS_GLB_INLINE
#endif
Process *erts_pid2proc_opt(Process *, ErtsProcLocks, Eterm, ErtsProcLocks, int);

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

ERTS_GLB_INLINE Process *erts_pix2proc(int ix)
{
    Process *proc;
    ASSERT(0 <= ix && ix < erts_ptab_max(&erts_proc));
    proc = (Process *) erts_ptab_pix2intptr_nob(&erts_proc, ix);
    return proc == ERTS_PROC_LOCK_BUSY ? NULL : proc;
}

ERTS_GLB_INLINE Process *erts_proc_lookup_raw(Eterm pid)
{
    Process *proc;

    ERTS_SMP_LC_ASSERT(erts_thr_progress_lc_is_delaying());

    if (is_not_internal_pid(pid))
	return NULL;

    proc = (Process *) erts_ptab_pix2intptr_ddrb(&erts_proc,
						 internal_pid_index(pid));
    if (proc && proc->common.id != pid)
	return NULL;
    return proc;
}

ERTS_GLB_INLINE Process *erts_proc_lookup(Eterm pid)
{
    Process *proc = erts_proc_lookup_raw(pid);
    if (proc && ERTS_PROC_IS_EXITING(proc))
	return NULL;
    return proc;
}

#ifndef ERTS_SMP
ERTS_GLB_INLINE Process *
erts_pid2proc_opt(Process *c_p_unused,
		  ErtsProcLocks c_p_have_locks_unused,
		  Eterm pid,
		  ErtsProcLocks pid_need_locks_unused,
		  int flags)
{
    Process *proc = erts_proc_lookup_raw(pid);
    if (!proc)
	return NULL;
    if (!(flags & ERTS_P2P_FLG_ALLOW_OTHER_X)
	&& ERTS_PROC_IS_EXITING(proc))
	return NULL;
    if (flags & ERTS_P2P_FLG_INC_REFC)
	erts_proc_inc_refc(proc);
    return proc;
}
#endif /* !ERTS_SMP */

#endif /* #if ERTS_GLB_INLINE_INCL_FUNC_DEF */

#endif /* #ifndef ERTS_PROCESS_LOCK_H__ */
#endif /* #if !defined(ERTS_PROCESS_LOCK_ONLY_PROC_LOCK_TYPE__) 
	  && !defined(ERTS_PROCESS_LOCK_ONLY_LOCK_CHECK_PROTO__) */
