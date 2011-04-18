/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2007-2011. All Rights Reserved.
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

#define ERTS_PROC_LOCK_ATOMIC_IMPL 0
#define ERTS_PROC_LOCK_SPINLOCK_IMPL 0
#define ERTS_PROC_LOCK_MUTEX_IMPL 0

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

#define ERTS_PROC_LOCK_MAX_BIT 3

typedef erts_aint32_t ErtsProcLocks;

typedef struct erts_proc_lock_queues_t_ erts_proc_lock_queues_t;

typedef struct erts_proc_lock_t_ {
#if ERTS_PROC_LOCK_ATOMIC_IMPL
    erts_smp_atomic32_t flags;
#else
    ErtsProcLocks flags;
#endif
    erts_proc_lock_queues_t *queues;
    Sint32 refc;
#ifdef ERTS_PROC_LOCK_DEBUG
    erts_smp_atomic32_t locked[ERTS_PROC_LOCK_MAX_BIT+1];
#endif
#ifdef ERTS_ENABLE_LOCK_COUNT
    erts_lcnt_lock_t lcnt_main;
    erts_lcnt_lock_t lcnt_link;
    erts_lcnt_lock_t lcnt_msgq;
    erts_lcnt_lock_t lcnt_status;
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
 *   * bif_timers
 */
#define ERTS_PROC_LOCK_MSGQ		(((ErtsProcLocks) 1) << 2)

/*
 * Status lock:
 *   Protects the following fields in the process structure:
 *   * status
 *   * rstatus
 *   * status_flags
 *   * pending_suspenders
 *   * suspendee
 */
#define ERTS_PROC_LOCK_STATUS		(((ErtsProcLocks) 1) << ERTS_PROC_LOCK_MAX_BIT)

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
 *     * tracer_proc
 *     * tracer_flags
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
 *   When changing status to P_EXITING on a process, you are required
 *   to take all process locks (ERTS_PROC_LOCKS_ALL). Thus, by holding
 *   at least one process lock (whichever one doesn't matter) you
 *   are guaranteed that the process won't exit until the lock you are
 *   holding has been released. Appart from all process locks also
 *   the pix lock corresponding to the process has to be held.
 *     At the same time as status is changed to P_EXITING, also the
 *   field 'is_exiting' in the process structure is set to a value != 0.
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
 *   locked on processes p1 and p2 and p1->id < p2->id, then locks
 *   should be locked in the following order:
 *     1. main lock on p1
 *     2. main lock on p2
 *     3. message queue lock on p1
 *     4. message queue lock on p2
 */

/* Other lock flags */
#define ERTS_PROC_LOCK_WAITER_SHIFT (ERTS_PROC_LOCK_MAX_BIT + 1)


/* ERTS_PROC_LOCKS_* are combinations of process locks */

#define ERTS_PROC_LOCKS_MSG_RECEIVE	(ERTS_PROC_LOCK_MSGQ		\
					 | ERTS_PROC_LOCK_STATUS)
#define ERTS_PROC_LOCKS_MSG_SEND	(ERTS_PROC_LOCK_MSGQ		\
					 | ERTS_PROC_LOCK_STATUS)
#define ERTS_PROC_LOCKS_XSIG_SEND	ERTS_PROC_LOCK_STATUS

#define ERTS_PROC_LOCKS_ALL \
  ((((ErtsProcLocks) 1) << (ERTS_PROC_LOCK_MAX_BIT + 1)) - 1)

#define ERTS_PROC_LOCKS_ALL_MINOR	(ERTS_PROC_LOCKS_ALL \
					 & ~ERTS_PROC_LOCK_MAIN)


#define ERTS_PIX_LOCKS_BITS		8
#define ERTS_NO_OF_PIX_LOCKS		(1 << ERTS_PIX_LOCKS_BITS)


#endif /* #ifndef ERTS_PROC_LOCK_TYPE__ */

#ifndef ERTS_PROCESS_LOCK_ONLY_PROC_LOCK_TYPE__
#ifndef ERTS_PROC_LOCK_LOCK_CHECK__
#define  ERTS_PROC_LOCK_LOCK_CHECK__

/* Lock counter implemetation */

#ifdef ERTS_ENABLE_LOCK_COUNT
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

#endif /* ERTS_ENABLE_LOCK_COUNT*/



/* --- Process lock checking ----------------------------------------------- */

#if defined(ERTS_SMP) && defined(ERTS_ENABLE_LOCK_CHECK)
#define ERTS_SMP_CHK_NO_PROC_LOCKS \
  erts_proc_lc_chk_no_proc_locks(__FILE__, __LINE__)
#define ERTS_SMP_CHK_HAVE_ONLY_MAIN_PROC_LOCK(P) \
  erts_proc_lc_chk_only_proc_main((P))
void erts_proc_lc_lock(Process *p, ErtsProcLocks locks);
void erts_proc_lc_trylock(Process *p, ErtsProcLocks locks, int locked);
void erts_proc_lc_unlock(Process *p, ErtsProcLocks locks);
void erts_proc_lc_might_unlock(Process *p, ErtsProcLocks locks);
void erts_proc_lc_chk_have_proc_locks(Process *p, ErtsProcLocks locks);
void erts_proc_lc_chk_proc_locks(Process *p, ErtsProcLocks locks);
void erts_proc_lc_chk_only_proc_main(Process *p);
void erts_proc_lc_chk_no_proc_locks(char *file, int line);
ErtsProcLocks erts_proc_lc_my_proc_locks(Process *p);
int erts_proc_lc_trylock_force_busy(Process *p, ErtsProcLocks locks);
void erts_proc_lc_require_lock(Process *p, ErtsProcLocks locks);
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
	erts_smp_spinlock_t spnlck;
	char buf[64]; /* Try to get locks in different cache lines */
    } u;
} erts_pix_lock_t;

#define ERTS_PIX2PIXLOCKIX(PIX) \
  ((PIX) & ((1 << ERTS_PIX_LOCKS_BITS) - 1))
#define ERTS_PIX2PIXLOCK(PIX) \
  (&erts_pix_locks[ERTS_PIX2PIXLOCKIX((PIX))])
#define ERTS_PID2PIXLOCK(PID) \
  ERTS_PIX2PIXLOCK(internal_pid_data((PID)))

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

extern erts_pix_lock_t erts_pix_locks[ERTS_NO_OF_PIX_LOCKS];

void erts_init_proc_lock(int cpus);
void erts_proc_lock_prepare_proc_lock_waiter(void);
void erts_proc_lock_failed(Process *,
			   erts_pix_lock_t *,
			   ErtsProcLocks,
			   ErtsProcLocks);
void erts_proc_unlock_failed(Process *,
			     erts_pix_lock_t *,
			     ErtsProcLocks);

ERTS_GLB_INLINE void erts_pix_lock(erts_pix_lock_t *);
ERTS_GLB_INLINE void erts_pix_unlock(erts_pix_lock_t *);
ERTS_GLB_INLINE int erts_lc_pix_lock_is_locked(erts_pix_lock_t *);

ERTS_GLB_INLINE ErtsProcLocks erts_smp_proc_raw_trylock__(Process *p,
							  ErtsProcLocks locks);
#ifdef ERTS_ENABLE_LOCK_COUNT
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
    erts_smp_spin_lock(&pixlck->u.spnlck);
}

ERTS_GLB_INLINE void erts_pix_unlock(erts_pix_lock_t *pixlck)
{
    ERTS_LC_ASSERT(pixlck);
    erts_smp_spin_unlock(&pixlck->u.spnlck);
}

ERTS_GLB_INLINE int erts_lc_pix_lock_is_locked(erts_pix_lock_t *pixlck)
{
    return erts_smp_lc_spinlock_is_locked(&pixlck->u.spnlck);
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
}


ERTS_GLB_INLINE void
#ifdef ERTS_ENABLE_LOCK_COUNT
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
    ErtsProcLocks old_lflgs;
#if !ERTS_PROC_LOCK_ATOMIC_IMPL
    erts_pix_lock(pix_lck);
#endif
#ifdef ERTS_ENABLE_LOCK_COUNT
    erts_lcnt_proc_lock(&(p->lock), locks);
#endif

    ERTS_LC_ASSERT((locks & ~ERTS_PROC_LOCKS_ALL) == 0);

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
#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_proc_lc_lock(p, locks);
#endif
#ifdef ERTS_PROC_LOCK_DEBUG
    erts_proc_lock_op_debug(p, locks, 1);
#endif

#if ERTS_PROC_LOCK_ATOMIC_IMPL
    ETHR_COMPILER_BARRIER;
#endif
}

ERTS_GLB_INLINE void
erts_smp_proc_unlock__(Process *p,
		       erts_pix_lock_t *pix_lck,
		       ErtsProcLocks locks)
{
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
}

ERTS_GLB_INLINE int
erts_smp_proc_trylock__(Process *p,
			erts_pix_lock_t *pix_lck,
			ErtsProcLocks locks)
{
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
    erts_proc_lc_trylock(p, locks, res == 0);
#endif

#if ERTS_PROC_LOCK_ATOMIC_IMPL
    ETHR_COMPILER_BARRIER;
#endif

    return res;
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

#ifdef ERTS_ENABLE_LOCK_COUNT
ERTS_GLB_INLINE void erts_smp_proc_lock_x(Process *, ErtsProcLocks, char *file, unsigned int line);
#else
ERTS_GLB_INLINE void erts_smp_proc_lock(Process *, ErtsProcLocks);
#endif
ERTS_GLB_INLINE void erts_smp_proc_unlock(Process *, ErtsProcLocks);
ERTS_GLB_INLINE int erts_smp_proc_trylock(Process *, ErtsProcLocks);

ERTS_GLB_INLINE void erts_smp_proc_inc_refc(Process *);
ERTS_GLB_INLINE void erts_smp_proc_dec_refc(Process *);


#if ERTS_GLB_INLINE_INCL_FUNC_DEF

ERTS_GLB_INLINE void
#ifdef ERTS_ENABLE_LOCK_COUNT
erts_smp_proc_lock_x(Process *p, ErtsProcLocks locks, char *file, unsigned int line)
#else
erts_smp_proc_lock(Process *p, ErtsProcLocks locks)
#endif 
{
#if defined(ERTS_SMP) && defined(ERTS_ENABLE_LOCK_COUNT)
    erts_smp_proc_lock_x__(p,
#if ERTS_PROC_LOCK_ATOMIC_IMPL
			 NULL,
#else
			 ERTS_PID2PIXLOCK(p->id),
#endif /*ERTS_PROC_LOCK_ATOMIC_IMPL*/
			 locks, file, line);
#elif defined(ERTS_SMP)
    erts_smp_proc_lock__(p,
#if ERTS_PROC_LOCK_ATOMIC_IMPL
			 NULL,
#else
			 ERTS_PID2PIXLOCK(p->id),
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
			   ERTS_PID2PIXLOCK(p->id),
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
				   ERTS_PID2PIXLOCK(p->id),
#endif
				   locks);
#endif
}


ERTS_GLB_INLINE void erts_smp_proc_inc_refc(Process *p)
{
#ifdef ERTS_SMP
    erts_pix_lock_t *pixlck = ERTS_PID2PIXLOCK(p->id);
    erts_pix_lock(pixlck);
    ERTS_LC_ASSERT(p->lock.refc > 0);
    p->lock.refc++;
    erts_pix_unlock(pixlck);
#endif
}

ERTS_GLB_INLINE void erts_smp_proc_dec_refc(Process *p)
{
#ifdef ERTS_SMP
    Process *fp;
    erts_pix_lock_t *pixlck = ERTS_PID2PIXLOCK(p->id);
    erts_pix_lock(pixlck);
    ERTS_LC_ASSERT(p->lock.refc > 0);
    fp = --p->lock.refc == 0 ? p : NULL; 
    erts_pix_unlock(pixlck);
    if (fp)
	erts_free_proc(fp);
#endif
}

#endif /* #if ERTS_GLB_INLINE_INCL_FUNC_DEF */

#ifdef ERTS_SMP
void erts_proc_lock_init(Process *);
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
#define ERTS_P2P_FLG_SMP_INC_REFC	(1 <<  2)

#define ERTS_PROC_LOCK_BUSY ((Process *) &erts_proc_lock_busy)
extern const Process erts_proc_lock_busy;

#define erts_pid2proc(PROC, HL, PID, NL) \
  erts_pid2proc_opt((PROC), (HL), (PID), (NL), 0)

ERTS_GLB_INLINE Process *
erts_pid2proc_opt(Process *, ErtsProcLocks, Eterm, ErtsProcLocks, int);

#ifdef ERTS_SMP
void
erts_pid2proc_safelock(Process *c_p,
		       ErtsProcLocks c_p_have_locks,
		       Process **proc,
		       ErtsProcLocks need_locks,
		       erts_pix_lock_t *pix_lock,
		       int flags);
ERTS_GLB_INLINE Process *erts_pid2proc_unlocked_opt(Eterm pid, int flags);
#define erts_pid2proc_unlocked(PID) erts_pid2proc_unlocked_opt((PID), 0)
#else
#define erts_pid2proc_unlocked_opt(PID, FLGS) \
  erts_pid2proc_opt(NULL, 0, (PID), 0, FLGS)
#define erts_pid2proc_unlocked(PID) erts_pid2proc_opt(NULL, 0, (PID), 0, 0)
#endif

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

ERTS_GLB_INLINE Process *
#ifdef ERTS_SMP
erts_pid2proc_unlocked_opt(Eterm pid, int flags)
#else
erts_pid2proc_opt(Process *c_p_unused,
		  ErtsProcLocks c_p_have_locks_unused,
		  Eterm pid,
		  ErtsProcLocks pid_need_locks_unused,
		  int flags)
#endif
{
    Uint pix;
    Process *proc;

    if (is_not_internal_pid(pid))
	return NULL;
    pix = internal_pid_index(pid);
    if(pix >= erts_max_processes)
	return NULL;
    proc = process_tab[pix];
    if (proc) {
	if (proc->id != pid
	    || (!(flags & ERTS_P2P_FLG_ALLOW_OTHER_X)
		&& proc->status == P_EXITING))
	    proc = NULL;
    }
    return proc;
}

#ifdef ERTS_SMP

ERTS_GLB_INLINE Process *
erts_pid2proc_opt(Process *c_p,
		  ErtsProcLocks c_p_have_locks,
		  Eterm pid,
		  ErtsProcLocks pid_need_locks,
		  int flags)
{
    erts_pix_lock_t *pix_lock;
    ErtsProcLocks need_locks;
    Uint pix;
    Process *proc;
#ifdef ERTS_ENABLE_LOCK_COUNT
    ErtsProcLocks lcnt_locks;
#endif

#ifdef ERTS_ENABLE_LOCK_CHECK
    if (c_p) {
	ErtsProcLocks might_unlock = c_p_have_locks & pid_need_locks;
	if (might_unlock)
	    erts_proc_lc_might_unlock(c_p, might_unlock);
    }
#endif
    if (is_not_internal_pid(pid)) {
	proc = NULL;
	goto done;
    }
    pix = internal_pid_index(pid);
    if(pix >= erts_max_processes) {
	proc = NULL;
	goto done;
    }

    ERTS_LC_ASSERT((pid_need_locks & ERTS_PROC_LOCKS_ALL) == pid_need_locks);
    need_locks = pid_need_locks;

    pix_lock = ERTS_PIX2PIXLOCK(pix);

    if (c_p && c_p->id == pid) {
	ASSERT(c_p->id != ERTS_INVALID_PID);
	ASSERT(c_p == process_tab[pix]);
	if (!(flags & ERTS_P2P_FLG_ALLOW_OTHER_X) && c_p->is_exiting) {
	    proc = NULL;
	    goto done;
	}
	need_locks &= ~c_p_have_locks;
	if (!need_locks) {
	    proc = c_p;
	    erts_pix_lock(pix_lock);
	    if (flags & ERTS_P2P_FLG_SMP_INC_REFC)
		proc->lock.refc++;
	    erts_pix_unlock(pix_lock);
	    goto done;
	}
    }

    erts_pix_lock(pix_lock);

    proc = process_tab[pix];
    if (proc) {
	if (proc->id != pid || (!(flags & ERTS_P2P_FLG_ALLOW_OTHER_X)
				&& ERTS_PROC_IS_EXITING(proc))) {
	    proc = NULL;
	}
	else if (!need_locks) {
	    if (flags & ERTS_P2P_FLG_SMP_INC_REFC)
		proc->lock.refc++;
	}
	else {
	    int busy;

#ifdef ERTS_ENABLE_LOCK_COUNT
	    lcnt_locks = need_locks;
	    if (!(flags & ERTS_P2P_FLG_TRY_LOCK)) {
	    	erts_lcnt_proc_lock(&proc->lock, need_locks);
	    }
#endif

#ifdef ERTS_ENABLE_LOCK_CHECK
	    /* Make sure erts_pid2proc_safelock() is enough to handle
	       a potential lock order violation situation... */
	    busy = erts_proc_lc_trylock_force_busy(proc, need_locks);
	    if (!busy)
#endif
	    {
		/* Try a quick trylock to grab all the locks we need. */
		busy = (int) erts_smp_proc_raw_trylock__(proc, need_locks);
#ifdef ERTS_ENABLE_LOCK_CHECK
		erts_proc_lc_trylock(proc, need_locks, !busy);
#endif
#ifdef ERTS_PROC_LOCK_DEBUG
		if (!busy)
		    erts_proc_lock_op_debug(proc, need_locks, 1);
#endif
	    }

#ifdef ERTS_ENABLE_LOCK_COUNT
	    if (flags & ERTS_P2P_FLG_TRY_LOCK) {
	    	if (busy) {
		    erts_lcnt_proc_trylock(&proc->lock, need_locks, EBUSY);
		} else {
		    erts_lcnt_proc_trylock(&proc->lock, need_locks, 0);
		}
	    }
#endif
	    if (!busy) {
		if (flags & ERTS_P2P_FLG_SMP_INC_REFC)
		    proc->lock.refc++;
#ifdef ERTS_ENABLE_LOCK_COUNT
	    	/* all is great */
	    	if (!(flags & ERTS_P2P_FLG_TRY_LOCK)) {
		    erts_lcnt_proc_lock_post_x(&proc->lock, lcnt_locks, __FILE__, __LINE__);
	    	}
#endif
	    }
	    else {
		if (flags & ERTS_P2P_FLG_TRY_LOCK)
		    proc = ERTS_PROC_LOCK_BUSY;
		else {
		    if (flags & ERTS_P2P_FLG_SMP_INC_REFC)
			proc->lock.refc++;
#ifdef ERTS_ENABLE_LOCK_COUNT
		    erts_lcnt_proc_lock_unaquire(&proc->lock, lcnt_locks);
#endif
		    erts_pid2proc_safelock(c_p,
					   c_p_have_locks,
					   &proc,
					   pid_need_locks,
					   pix_lock,
					   flags);
		}
	    }
        }
    }

    erts_pix_unlock(pix_lock);
#ifdef ERTS_PROC_LOCK_DEBUG
    ERTS_LC_ASSERT(!proc
		   || proc == ERTS_PROC_LOCK_BUSY
		   || (pid_need_locks ==
		       (ERTS_PROC_LOCK_FLGS_READ_(&proc->lock)
			& pid_need_locks)));
#endif


 done:		

#if ERTS_PROC_LOCK_ATOMIC_IMPL
    ETHR_COMPILER_BARRIER;
#endif

    return proc;
}
#endif /* ERTS_SMP */

#endif /* #if ERTS_GLB_INLINE_INCL_FUNC_DEF */

#endif /* #ifndef ERTS_PROCESS_LOCK_H__ */
#endif /* #if !defined(ERTS_PROCESS_LOCK_ONLY_PROC_LOCK_TYPE__) 
	  && !defined(ERTS_PROCESS_LOCK_ONLY_LOCK_CHECK_PROTO__) */
