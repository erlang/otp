/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2001-2009. All Rights Reserved.
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

/* Description: Error checking thread interface to the ethread library.
 *              All functions terminates the emulator on failure.
 * Author: Rickard Green
 */

#ifndef ERL_THREAD_H__
#define ERL_THREAD_H__

#include "sys.h"
#ifdef USE_THREADS

#define ETHR_TRY_INLINE_FUNCS
#include "ethread.h"
#include "erl_lock_check.h"
#include "erl_lock_count.h"
#include "erl_term.h"

#ifdef ERTS_ENABLE_LOCK_COUNT
#define erts_mtx_lock(L) erts_mtx_lock_x(L, __FILE__, __LINE__)
#define erts_spin_lock(L) erts_spin_lock_x(L, __FILE__, __LINE__)
#define erts_rwmtx_rlock(L) erts_rwmtx_rlock_x(L, __FILE__, __LINE__)
#define erts_rwmtx_rwlock(L) erts_rwmtx_rwlock_x(L, __FILE__, __LINE__)
#define erts_read_lock(L) erts_read_lock_x(L, __FILE__, __LINE__)
#define erts_write_lock(L) erts_write_lock_x(L, __FILE__, __LINE__)
#endif

#define ERTS_THR_OPTS_DEFAULT_INITER ETHR_THR_OPTS_DEFAULT_INITER
typedef ethr_thr_opts erts_thr_opts_t;
typedef ethr_init_data erts_thr_init_data_t;
typedef ethr_tid erts_tid_t;

/* mutex */
typedef struct {
    ethr_mutex mtx;
#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_lc_lock_t lc;
#endif
#ifdef ERTS_ENABLE_LOCK_COUNT
    erts_lcnt_lock_t lcnt;
#endif

} erts_mtx_t;
typedef ethr_cond erts_cnd_t;

/* rwmutex */
typedef struct {
    ethr_rwmutex rwmtx;
#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_lc_lock_t lc;
#endif
#ifdef ERTS_ENABLE_LOCK_COUNT
    erts_lcnt_lock_t lcnt;
#endif
} erts_rwmtx_t;
typedef ethr_tsd_key erts_tsd_key_t;
typedef ethr_gate erts_gate_t;
typedef ethr_atomic_t erts_atomic_t;

/* spinlock */
typedef struct {
    ethr_spinlock_t slck;
#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_lc_lock_t lc;
#endif
#ifdef ERTS_ENABLE_LOCK_COUNT
    erts_lcnt_lock_t lcnt;
#endif
} erts_spinlock_t;

/* rwlock */
typedef struct {
    ethr_rwlock_t rwlck;
#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_lc_lock_t lc;
#endif
#ifdef ERTS_ENABLE_LOCK_COUNT
    erts_lcnt_lock_t lcnt;
#endif
} erts_rwlock_t;

typedef ethr_timeval erts_thr_timeval_t;
__decl_noreturn void  __noreturn erts_thr_fatal_error(int, char *); 
                                 /* implemented in erl_init.c */

#ifdef ERTS_ENABLE_LOCK_CHECK
#define ERTS_REC_MTX_INITER \
  {ETHR_REC_MUTEX_INITER, \
   ERTS_LC_LOCK_INIT(-1,THE_NON_VALUE,ERTS_LC_FLG_LT_MUTEX)}
#define ERTS_MTX_INITER \
  {ETHR_MUTEX_INITER, \
   ERTS_LC_LOCK_INIT(-1, THE_NON_VALUE, ERTS_LC_FLG_LT_MUTEX)}
#else
#define ERTS_REC_MTX_INITER		{ETHR_REC_MUTEX_INITER}
#define ERTS_MTX_INITER			{ETHR_MUTEX_INITER}
#endif
#define ERTS_CND_INITER			ETHR_COND_INITER
#define ERTS_THR_INIT_DATA_DEF_INITER	ETHR_INIT_DATA_DEFAULT_INITER

#ifdef ETHR_HAVE_ETHR_REC_MUTEX_INIT
#  define ERTS_HAVE_REC_MTX_INIT	ETHR_HAVE_ETHR_REC_MUTEX_INIT
#endif


#else /* #ifdef USE_THREADS */

#define ERTS_THR_OPTS_DEFAULT_INITER 0
typedef int erts_thr_opts_t;
typedef int erts_thr_init_data_t;
typedef int erts_tid_t;
typedef int erts_mtx_t;
typedef int erts_cnd_t;
typedef int erts_rwmtx_t;
typedef int erts_tsd_key_t;
typedef int erts_gate_t;
typedef long erts_atomic_t;
#if __GNUC__ > 2
typedef struct { } erts_spinlock_t;
typedef struct { } erts_rwlock_t;
#else
typedef struct { int gcc_is_buggy; } erts_spinlock_t;
typedef struct { int gcc_is_buggy; } erts_rwlock_t;
#endif
typedef struct {
    long tv_sec;
    long tv_nsec;
} erts_thr_timeval_t;

#define ERTS_REC_MTX_INITER		0
#define ERTS_MTX_INITER			0
#define ERTS_CND_INITER			0
#define ERTS_THR_INIT_DATA_DEF_INITER	0

#define ERTS_HAVE_REC_MTX_INIT		1

#endif /* #ifdef USE_THREADS */

ERTS_GLB_INLINE void erts_thr_init(erts_thr_init_data_t *id);
ERTS_GLB_INLINE void erts_thr_create(erts_tid_t *tid, void * (*func)(void *),
				     void *arg, erts_thr_opts_t *opts);
ERTS_GLB_INLINE void erts_thr_join(erts_tid_t tid, void **thr_res);
ERTS_GLB_INLINE void erts_thr_detach(erts_tid_t tid);
ERTS_GLB_INLINE void erts_thr_exit(void *res);
ERTS_GLB_INLINE void erts_thr_install_exit_handler(void (*exit_handler)(void));
ERTS_GLB_INLINE erts_tid_t erts_thr_self(void);
ERTS_GLB_INLINE int erts_equal_tids(erts_tid_t x, erts_tid_t y);
#ifdef ERTS_HAVE_REC_MTX_INIT
ERTS_GLB_INLINE void erts_rec_mtx_init(erts_mtx_t *mtx);
#endif
ERTS_GLB_INLINE void erts_mtx_init_x(erts_mtx_t *mtx, char *name, Eterm extra);
ERTS_GLB_INLINE void erts_mtx_init_x_opt(erts_mtx_t *mtx, char *name, Eterm extra, Uint16 opt);
ERTS_GLB_INLINE void erts_mtx_init_locked_x(erts_mtx_t *mtx,
					    char *name,
					    Eterm extra);
ERTS_GLB_INLINE void erts_mtx_init(erts_mtx_t *mtx, char *name);
ERTS_GLB_INLINE void erts_mtx_init_locked(erts_mtx_t *mtx, char *name);
ERTS_GLB_INLINE void erts_mtx_destroy(erts_mtx_t *mtx);
ERTS_GLB_INLINE void erts_mtx_set_forksafe(erts_mtx_t *mtx);
ERTS_GLB_INLINE void erts_mtx_unset_forksafe(erts_mtx_t *mtx);
ERTS_GLB_INLINE int erts_mtx_trylock(erts_mtx_t *mtx);
#ifdef ERTS_ENABLE_LOCK_COUNT
ERTS_GLB_INLINE void erts_mtx_lock_x(erts_mtx_t *mtx, char *file, unsigned int line);
#else
ERTS_GLB_INLINE void erts_mtx_lock(erts_mtx_t *mtx);
#endif
ERTS_GLB_INLINE void erts_mtx_unlock(erts_mtx_t *mtx);
ERTS_GLB_INLINE int erts_lc_mtx_is_locked(erts_mtx_t *mtx);
ERTS_GLB_INLINE void erts_cnd_init(erts_cnd_t *cnd);
ERTS_GLB_INLINE void erts_cnd_destroy(erts_cnd_t *cnd);
ERTS_GLB_INLINE void erts_cnd_wait(erts_cnd_t *cnd, erts_mtx_t *mtx);
ERTS_GLB_INLINE void erts_cnd_signal(erts_cnd_t *cnd);
ERTS_GLB_INLINE void erts_cnd_broadcast(erts_cnd_t *cnd);
ERTS_GLB_INLINE void erts_rwmtx_init_x(erts_rwmtx_t *rwmtx,
				       char *name,
				       Eterm extra);
ERTS_GLB_INLINE void erts_rwmtx_init(erts_rwmtx_t *rwmtx,
					 char *name);
ERTS_GLB_INLINE void erts_rwmtx_destroy(erts_rwmtx_t *rwmtx);
ERTS_GLB_INLINE int erts_rwmtx_tryrlock(erts_rwmtx_t *rwmtx);
#ifdef ERTS_ENABLE_LOCK_COUNT
ERTS_GLB_INLINE void erts_rwmtx_rlock_x(erts_rwmtx_t *rwmtx, char *file, unsigned int line);
ERTS_GLB_INLINE void erts_rwmtx_rwlock_x(erts_rwmtx_t *rwmtx, char *file, unsigned int line);
#else
ERTS_GLB_INLINE void erts_rwmtx_rlock(erts_rwmtx_t *rwmtx);
ERTS_GLB_INLINE void erts_rwmtx_rwlock(erts_rwmtx_t *rwmtx);
#endif
ERTS_GLB_INLINE void erts_rwmtx_runlock(erts_rwmtx_t *rwmtx);
ERTS_GLB_INLINE int erts_rwmtx_tryrwlock(erts_rwmtx_t *rwmtx);
ERTS_GLB_INLINE void erts_rwmtx_rwunlock(erts_rwmtx_t *rwmtx);
ERTS_GLB_INLINE int erts_lc_rwmtx_is_rlocked(erts_rwmtx_t *mtx);
ERTS_GLB_INLINE int erts_lc_rwmtx_is_rwlocked(erts_rwmtx_t *mtx);
ERTS_GLB_INLINE void erts_atomic_init(erts_atomic_t *var, long i);
ERTS_GLB_INLINE void erts_atomic_set(erts_atomic_t *var, long i);
ERTS_GLB_INLINE long erts_atomic_read(erts_atomic_t *var);
ERTS_GLB_INLINE long erts_atomic_inctest(erts_atomic_t *incp);
ERTS_GLB_INLINE long erts_atomic_dectest(erts_atomic_t *decp);
ERTS_GLB_INLINE void erts_atomic_inc(erts_atomic_t *incp);
ERTS_GLB_INLINE void erts_atomic_dec(erts_atomic_t *decp);
ERTS_GLB_INLINE long erts_atomic_addtest(erts_atomic_t *addp,
					 long i);
ERTS_GLB_INLINE void erts_atomic_add(erts_atomic_t *addp, long i);
ERTS_GLB_INLINE long erts_atomic_xchg(erts_atomic_t *xchgp,
				      long new);
ERTS_GLB_INLINE long erts_atomic_cmpxchg(erts_atomic_t *xchgp,
					 long new,
					 long expected);
ERTS_GLB_INLINE long erts_atomic_bor(erts_atomic_t *var, long mask);
ERTS_GLB_INLINE long erts_atomic_band(erts_atomic_t *var, long mask);
ERTS_GLB_INLINE void erts_spinlock_init_x(erts_spinlock_t *lock,
					  char *name,
					  Eterm extra);
ERTS_GLB_INLINE void erts_spinlock_init(erts_spinlock_t *lock,
					char *name);
ERTS_GLB_INLINE void erts_spinlock_destroy(erts_spinlock_t *lock);
ERTS_GLB_INLINE void erts_spin_unlock(erts_spinlock_t *lock);
#ifdef ERTS_ENABLE_LOCK_COUNT
ERTS_GLB_INLINE void erts_spin_lock_x(erts_spinlock_t *lock, char *file, unsigned int line);
#else
ERTS_GLB_INLINE void erts_spin_lock(erts_spinlock_t *lock);
#endif
ERTS_GLB_INLINE int erts_lc_spinlock_is_locked(erts_spinlock_t *lock);
ERTS_GLB_INLINE void erts_rwlock_init_x(erts_rwlock_t *lock,
					char *name,
					Eterm extra);
ERTS_GLB_INLINE void erts_rwlock_init(erts_rwlock_t *lock,
				      char *name);
ERTS_GLB_INLINE void erts_rwlock_destroy(erts_rwlock_t *lock);
ERTS_GLB_INLINE void erts_read_unlock(erts_rwlock_t *lock);
#ifdef ERTS_ENABLE_LOCK_COUNT
ERTS_GLB_INLINE void erts_read_lock_x(erts_rwlock_t *lock, char *file, unsigned int line);
ERTS_GLB_INLINE void erts_write_lock_x(erts_rwlock_t *lock, char *file, unsigned int line);
#else
ERTS_GLB_INLINE void erts_read_lock(erts_rwlock_t *lock);
ERTS_GLB_INLINE void erts_write_lock(erts_rwlock_t *lock);
#endif
ERTS_GLB_INLINE void erts_write_unlock(erts_rwlock_t *lock);
ERTS_GLB_INLINE int erts_lc_rwlock_is_rlocked(erts_rwlock_t *lock);
ERTS_GLB_INLINE int erts_lc_rwlock_is_rwlocked(erts_rwlock_t *lock);
ERTS_GLB_INLINE void erts_thr_time_now(erts_thr_timeval_t *time);
ERTS_GLB_INLINE void erts_tsd_key_create(erts_tsd_key_t *keyp);
ERTS_GLB_INLINE void erts_tsd_key_delete(erts_tsd_key_t key);
ERTS_GLB_INLINE void erts_tsd_set(erts_tsd_key_t key, void *value);
ERTS_GLB_INLINE void * erts_tsd_get(erts_tsd_key_t key);
ERTS_GLB_INLINE void erts_gate_init(erts_gate_t *gp);
ERTS_GLB_INLINE void erts_gate_destroy(erts_gate_t *gp);
ERTS_GLB_INLINE void erts_gate_close(erts_gate_t *gp);
ERTS_GLB_INLINE void erts_gate_let_through(erts_gate_t *gp, unsigned no);
ERTS_GLB_INLINE void erts_gate_wait(erts_gate_t *gp);
ERTS_GLB_INLINE void erts_gate_swait(erts_gate_t *gp, int spincount);

#ifdef ETHR_HAVE_ETHR_SIG_FUNCS
#define ERTS_THR_HAVE_SIG_FUNCS 1
ERTS_GLB_INLINE void erts_thr_sigmask(int how, const sigset_t *set,
				      sigset_t *oset);
ERTS_GLB_INLINE void erts_thr_sigwait(const sigset_t *set, int *sig);
#endif /* #ifdef HAVE_ETHR_SIG_FUNCS */

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

ERTS_GLB_INLINE void
erts_thr_init(erts_thr_init_data_t *id)
{
#ifdef USE_THREADS
    int res = ethr_init(id);
    if (res)
	erts_thr_fatal_error(res, "initialize thread library");
#endif
}

ERTS_GLB_INLINE void
erts_thr_create(erts_tid_t *tid, void * (*func)(void *), void *arg,
		erts_thr_opts_t *opts)
{
#ifdef USE_THREADS
#ifdef ERTS_ENABLE_LOCK_COUNT
    int res = erts_lcnt_thr_create(tid, func, arg, opts);
#else
    int res = ethr_thr_create(tid, func, arg, opts);
#endif
    if (res)
	erts_thr_fatal_error(res, "create thread");
#endif
}

ERTS_GLB_INLINE void
erts_thr_join(erts_tid_t tid, void **thr_res)
{
#ifdef USE_THREADS
    int res = ethr_thr_join(tid, thr_res);
    if (res)
	erts_thr_fatal_error(res, "join thread");
#endif
}


ERTS_GLB_INLINE void
erts_thr_detach(erts_tid_t tid)
{
#ifdef USE_THREADS
    int res = ethr_thr_detach(tid);
    if (res)
	erts_thr_fatal_error(res, "detach thread");
#endif
}


ERTS_GLB_INLINE void
erts_thr_exit(void *res)
{
#ifdef USE_THREADS
    ethr_thr_exit(res);
    erts_thr_fatal_error(0, "terminate thread");
#endif
}

ERTS_GLB_INLINE void
erts_thr_install_exit_handler(void (*exit_handler)(void))
{
#ifdef USE_THREADS
    int res = ethr_install_exit_handler(exit_handler);
    if (res != 0)
	erts_thr_fatal_error(res, "install thread exit handler");
#endif
}

ERTS_GLB_INLINE erts_tid_t
erts_thr_self(void)
{
#ifdef USE_THREADS
    return ethr_self();
#else
    return 0;
#endif
}


ERTS_GLB_INLINE int
erts_equal_tids(erts_tid_t x, erts_tid_t y)
{
#ifdef USE_THREADS
    return ethr_equal_tids(x, y);
#else
    return 1;
#endif
}


#ifdef ERTS_HAVE_REC_MTX_INIT
ERTS_GLB_INLINE void
erts_rec_mtx_init(erts_mtx_t *mtx)
{
#ifdef USE_THREADS
    int res = ethr_rec_mutex_init(&mtx->mtx);
    if (res)
	erts_thr_fatal_error(res, "initialize recursive mutex");
#endif
}
#endif


ERTS_GLB_INLINE void
erts_mtx_init_x(erts_mtx_t *mtx, char *name, Eterm extra)
{
#ifdef USE_THREADS
    int res = ethr_mutex_init(&mtx->mtx);
    if (res)
	erts_thr_fatal_error(res, "initialize mutex");
#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_lc_init_lock_x(&mtx->lc, name, ERTS_LC_FLG_LT_MUTEX, extra);
#endif
#ifdef ERTS_ENABLE_LOCK_COUNT
    erts_lcnt_init_lock_x(&mtx->lcnt, name, ERTS_LCNT_LT_MUTEX, extra);
#endif
#endif
}

ERTS_GLB_INLINE void
erts_mtx_init_x_opt(erts_mtx_t *mtx, char *name, Eterm extra, Uint16 opt)
{
#ifdef USE_THREADS
    int res = ethr_mutex_init(&mtx->mtx);
    if (res)
	erts_thr_fatal_error(res, "initialize mutex");
#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_lc_init_lock_x(&mtx->lc, name, ERTS_LC_FLG_LT_MUTEX, extra);
#endif
#ifdef ERTS_ENABLE_LOCK_COUNT
    erts_lcnt_init_lock_x(&mtx->lcnt, name, ERTS_LCNT_LT_MUTEX | opt, extra);
#endif
#endif
}


ERTS_GLB_INLINE void
erts_mtx_init_locked_x(erts_mtx_t *mtx, char *name, Eterm extra)
{
#ifdef USE_THREADS
    int res = ethr_mutex_init(&mtx->mtx);
    if (res)
	erts_thr_fatal_error(res, "initialize mutex");
#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_lc_init_lock_x(&mtx->lc, name, ERTS_LC_FLG_LT_MUTEX, extra);
#endif
#ifdef ERTS_ENABLE_LOCK_COUNT
    erts_lcnt_init_lock_x(&mtx->lcnt, name, ERTS_LCNT_LT_MUTEX, extra);
#endif
    res = ethr_mutex_lock(&mtx->mtx);
    if (res)
	erts_thr_fatal_error(res, "lock mutex");
#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_lc_trylock(1, &mtx->lc);
#endif
#ifdef ERTS_ENABLE_LOCK_COUNT
    erts_lcnt_trylock(&mtx->lcnt, 1);
#endif
#endif
}

ERTS_GLB_INLINE void
erts_mtx_init(erts_mtx_t *mtx, char *name)
{
#ifdef USE_THREADS
    int res = ethr_mutex_init(&mtx->mtx);
    if (res)
	erts_thr_fatal_error(res, "initialize mutex");
#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_lc_init_lock(&mtx->lc, name, ERTS_LC_FLG_LT_MUTEX);
#endif
#ifdef ERTS_ENABLE_LOCK_COUNT
    erts_lcnt_init_lock(&mtx->lcnt, name, ERTS_LCNT_LT_MUTEX);
#endif
#endif
}

ERTS_GLB_INLINE void
erts_mtx_init_locked(erts_mtx_t *mtx, char *name)
{
#ifdef USE_THREADS
    int res = ethr_mutex_init(&mtx->mtx);
    if (res)
	erts_thr_fatal_error(res, "initialize mutex");
#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_lc_init_lock(&mtx->lc, name, ERTS_LC_FLG_LT_MUTEX);
#endif
#ifdef ERTS_ENABLE_LOCK_COUNT
    erts_lcnt_init_lock(&mtx->lcnt, name, ERTS_LCNT_LT_MUTEX);
#endif
    res = ethr_mutex_lock(&mtx->mtx);
    if (res)
	erts_thr_fatal_error(res, "lock mutex");
#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_lc_trylock(1, &mtx->lc);
#endif
#ifdef ERTS_ENABLE_LOCK_COUNT
    erts_lcnt_trylock(&mtx->lcnt, 1);
#endif
#endif
}

ERTS_GLB_INLINE void
erts_mtx_destroy(erts_mtx_t *mtx)
{
#ifdef USE_THREADS
    int res;
#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_lc_destroy_lock(&mtx->lc);
#endif
#ifdef ERTS_ENABLE_LOCK_COUNT
    erts_lcnt_destroy_lock(&mtx->lcnt);
#endif
    res = ethr_mutex_destroy(&mtx->mtx);
    if (res)
	erts_thr_fatal_error(res, "destroy mutex");
#endif
}

ERTS_GLB_INLINE void
erts_mtx_set_forksafe(erts_mtx_t *mtx)
{
#ifdef USE_THREADS
    int res = ethr_mutex_set_forksafe(&mtx->mtx);
    if (res != 0 && res != ENOTSUP)
	erts_thr_fatal_error(res, "set mutex forksafe");
#endif
}

ERTS_GLB_INLINE void
erts_mtx_unset_forksafe(erts_mtx_t *mtx)
{
#ifdef USE_THREADS
    int res = ethr_mutex_unset_forksafe(&mtx->mtx);
    if (res != 0 && res != ENOTSUP)
	erts_thr_fatal_error(res, "unset mutex forksafe");
#endif
}

ERTS_GLB_INLINE int
erts_mtx_trylock(erts_mtx_t *mtx)
{
#ifdef USE_THREADS
    int res;

#ifdef ERTS_ENABLE_LOCK_CHECK
    if (erts_lc_trylock_force_busy(&mtx->lc))
	return EBUSY; /* Make sure caller can handle the situation without
			 causing a lock order violation */
#endif

    res = ethr_mutex_trylock(&mtx->mtx);

#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_lc_trylock(res == 0, &mtx->lc);
#endif
#ifdef ERTS_ENABLE_LOCK_COUNT
    erts_lcnt_trylock(&mtx->lcnt, res);
#endif

    if (res != 0 && res != EBUSY)
	erts_thr_fatal_error(res, "try lock mutex");
    
    return res;
#else
    return 0;
#endif

}

ERTS_GLB_INLINE void
#ifdef ERTS_ENABLE_LOCK_COUNT
erts_mtx_lock_x(erts_mtx_t *mtx, char *file, unsigned int line)
#else
erts_mtx_lock(erts_mtx_t *mtx)
#endif
{
#ifdef USE_THREADS
    int res;
#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_lc_lock(&mtx->lc);
#endif
#ifdef ERTS_ENABLE_LOCK_COUNT
    erts_lcnt_lock(&mtx->lcnt);
#endif
    res = ethr_mutex_lock(&mtx->mtx);
#ifdef ERTS_ENABLE_LOCK_COUNT
    erts_lcnt_lock_post_x(&mtx->lcnt, file, line);
#endif
    if (res)
	erts_thr_fatal_error(res, "lock mutex");
#endif
}

ERTS_GLB_INLINE void
erts_mtx_unlock(erts_mtx_t *mtx)
{
#ifdef USE_THREADS
    int res;
#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_lc_unlock(&mtx->lc);
#endif
#ifdef ERTS_ENABLE_LOCK_COUNT
    erts_lcnt_unlock(&mtx->lcnt);
#endif
    res = ethr_mutex_unlock(&mtx->mtx);
    if (res)
	erts_thr_fatal_error(res, "unlock mutex");
#endif
}

ERTS_GLB_INLINE int
erts_lc_mtx_is_locked(erts_mtx_t *mtx)
{
#if defined(USE_THREADS) && defined(ERTS_ENABLE_LOCK_CHECK)
    int res;
    erts_lc_lock_t lc = mtx->lc;
    lc.flags = 0;
    erts_lc_have_locks(&res, &lc, 1);
    return res;
#else
    return 0;
#endif
}

ERTS_GLB_INLINE void
erts_cnd_init(erts_cnd_t *cnd)
{
#ifdef USE_THREADS
    int res = ethr_cond_init(cnd);
    if (res)
	erts_thr_fatal_error(res, "initialize condition variable");
#endif
}

ERTS_GLB_INLINE void
erts_cnd_destroy(erts_cnd_t *cnd)
{
#ifdef USE_THREADS
    int res = ethr_cond_destroy(cnd);
    if (res)
	erts_thr_fatal_error(res, "destroy condition variable");
#endif
}

ERTS_GLB_INLINE void
erts_cnd_wait(erts_cnd_t *cnd, erts_mtx_t *mtx)
{
#ifdef USE_THREADS
    int res;
#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_lc_unlock(&mtx->lc);
#endif
#ifdef ERTS_ENABLE_LOCK_COUNT
    erts_lcnt_unlock(&mtx->lcnt);
#endif
    res = ethr_cond_wait(cnd, &mtx->mtx);
#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_lc_lock(&mtx->lc);
#endif
#ifdef ERTS_ENABLE_LOCK_COUNT
    erts_lcnt_lock(&mtx->lcnt);
#endif
#ifdef ERTS_ENABLE_LOCK_COUNT
    erts_lcnt_lock_post(&mtx->lcnt);
#endif
    if (res != 0 && res != EINTR)
	erts_thr_fatal_error(res, "wait on condition variable");
#endif
}

ERTS_GLB_INLINE void
erts_cnd_signal(erts_cnd_t *cnd)
{
#ifdef USE_THREADS
    int res = ethr_cond_signal(cnd);
    if (res)
	erts_thr_fatal_error(res, "signal on condition variable");
#endif
}


ERTS_GLB_INLINE void
erts_cnd_broadcast(erts_cnd_t *cnd)
{
#ifdef USE_THREADS
    int res = ethr_cond_broadcast(cnd);
    if (res)
	erts_thr_fatal_error(res, "broadcast on condition variable");
#endif
}

/* rwmutex */

ERTS_GLB_INLINE void
erts_rwmtx_init_x(erts_rwmtx_t *rwmtx, char *name, Eterm extra)
{
#ifdef USE_THREADS
    int res = ethr_rwmutex_init(&rwmtx->rwmtx);
    if (res != 0)
	erts_thr_fatal_error(res, "initialize rwmutex");
#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_lc_init_lock_x(&rwmtx->lc, name, ERTS_LC_FLG_LT_RWMUTEX, extra);
#endif
#ifdef ERTS_ENABLE_LOCK_COUNT
    erts_lcnt_init_lock_x(&rwmtx->lcnt, name, ERTS_LCNT_LT_RWMUTEX, extra);
#endif
#endif
}

ERTS_GLB_INLINE void
erts_rwmtx_init(erts_rwmtx_t *rwmtx, char *name)
{
#ifdef USE_THREADS
    int res = ethr_rwmutex_init(&rwmtx->rwmtx);
    if (res != 0)
	erts_thr_fatal_error(res, "initialize rwmutex");
#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_lc_init_lock(&rwmtx->lc, name, ERTS_LC_FLG_LT_RWMUTEX);
#endif
#ifdef ERTS_ENABLE_LOCK_COUNT
    erts_lcnt_init_lock(&rwmtx->lcnt, name, ERTS_LCNT_LT_RWMUTEX);
#endif
#endif
}

ERTS_GLB_INLINE void
erts_rwmtx_destroy(erts_rwmtx_t *rwmtx)
{
#ifdef USE_THREADS
    int res;
#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_lc_destroy_lock(&rwmtx->lc);
#endif
#ifdef ERTS_ENABLE_LOCK_COUNT
    erts_lcnt_destroy_lock(&rwmtx->lcnt);
#endif
    res = ethr_rwmutex_destroy(&rwmtx->rwmtx);
    if (res != 0)
	erts_thr_fatal_error(res, "destroy rwmutex");
#endif
}

ERTS_GLB_INLINE int
erts_rwmtx_tryrlock(erts_rwmtx_t *rwmtx)
{
#ifdef USE_THREADS
    int res;

#ifdef ERTS_ENABLE_LOCK_CHECK
    if (erts_lc_trylock_force_busy_flg(&rwmtx->lc, ERTS_LC_FLG_LO_READ))
	return EBUSY; /* Make sure caller can handle the situation without
			 causing a lock order violation */
#endif

    res = ethr_rwmutex_tryrlock(&rwmtx->rwmtx);

#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_lc_trylock_flg(res == 0, &rwmtx->lc, ERTS_LC_FLG_LO_READ);
#endif
#ifdef ERTS_ENABLE_LOCK_COUNT
    erts_lcnt_trylock_opt(&rwmtx->lcnt, res, ERTS_LCNT_LO_READ);
#endif

    if (res != 0 && res != EBUSY)
	erts_thr_fatal_error(res, "try read lock rwmutex");
    
    return res;
#else
    return 0;
#endif
}

ERTS_GLB_INLINE void
#ifdef ERTS_ENABLE_LOCK_COUNT
erts_rwmtx_rlock_x(erts_rwmtx_t *rwmtx, char *file, unsigned int line)
#else
erts_rwmtx_rlock(erts_rwmtx_t *rwmtx)
#endif
{
#ifdef USE_THREADS
    int res;
#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_lc_lock_flg(&rwmtx->lc, ERTS_LC_FLG_LO_READ);
#endif
#ifdef ERTS_ENABLE_LOCK_COUNT
    erts_lcnt_lock_opt(&rwmtx->lcnt, ERTS_LCNT_LO_READ);
#endif
    res = ethr_rwmutex_rlock(&rwmtx->rwmtx);
#ifdef ERTS_ENABLE_LOCK_COUNT
    erts_lcnt_lock_post_x(&rwmtx->lcnt, file, line);
#endif
    if (res != 0)
	erts_thr_fatal_error(res, "read lock rwmutex");
#endif
}

ERTS_GLB_INLINE void
erts_rwmtx_runlock(erts_rwmtx_t *rwmtx)
{
#ifdef USE_THREADS
    int res;
#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_lc_unlock_flg(&rwmtx->lc, ERTS_LC_FLG_LO_READ);
#endif
#ifdef ERTS_ENABLE_LOCK_COUNT
    erts_lcnt_unlock_opt(&rwmtx->lcnt, ERTS_LCNT_LO_READ);
#endif
    res = ethr_rwmutex_runlock(&rwmtx->rwmtx);
    if (res != 0)
	erts_thr_fatal_error(res, "read unlock rwmutex");
#endif
}


ERTS_GLB_INLINE int
erts_rwmtx_tryrwlock(erts_rwmtx_t *rwmtx)
{
#ifdef USE_THREADS
    int res;

#ifdef ERTS_ENABLE_LOCK_CHECK
    if (erts_lc_trylock_force_busy_flg(&rwmtx->lc, ERTS_LC_FLG_LO_READ_WRITE))
	return EBUSY; /* Make sure caller can handle the situation without
			 causing a lock order violation */
#endif

    res = ethr_rwmutex_tryrwlock(&rwmtx->rwmtx);

#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_lc_trylock_flg(res == 0, &rwmtx->lc, ERTS_LC_FLG_LO_READ_WRITE);
#endif
#ifdef ERTS_ENABLE_LOCK_COUNT
    erts_lcnt_trylock_opt(&rwmtx->lcnt, res, ERTS_LCNT_LO_READ_WRITE);
#endif

    if (res != 0 && res != EBUSY)
	erts_thr_fatal_error(res, "try write lock rwmutex");
    
    return res;
#else
    return 0;
#endif
}

ERTS_GLB_INLINE void
#ifdef ERTS_ENABLE_LOCK_COUNT
erts_rwmtx_rwlock_x(erts_rwmtx_t *rwmtx, char *file, unsigned int line)
#else
erts_rwmtx_rwlock(erts_rwmtx_t *rwmtx)
#endif
{
#ifdef USE_THREADS
    int res;
#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_lc_lock_flg(&rwmtx->lc, ERTS_LC_FLG_LO_READ_WRITE);
#endif
#ifdef ERTS_ENABLE_LOCK_COUNT
    erts_lcnt_lock_opt(&rwmtx->lcnt, ERTS_LCNT_LO_READ_WRITE);
#endif
    res = ethr_rwmutex_rwlock(&rwmtx->rwmtx);
#ifdef ERTS_ENABLE_LOCK_COUNT
    erts_lcnt_lock_post_x(&rwmtx->lcnt, file, line);
#endif
    if (res != 0)
	erts_thr_fatal_error(res, "write lock rwmutex");
#endif
}

ERTS_GLB_INLINE void
erts_rwmtx_rwunlock(erts_rwmtx_t *rwmtx)
{
#ifdef USE_THREADS
    int res;
#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_lc_unlock_flg(&rwmtx->lc, ERTS_LC_FLG_LO_READ_WRITE);
#endif
#ifdef ERTS_ENABLE_LOCK_COUNT
    erts_lcnt_unlock_opt(&rwmtx->lcnt, ERTS_LCNT_LO_READ_WRITE);
#endif
    res = ethr_rwmutex_rwunlock(&rwmtx->rwmtx);
    if (res != 0)
	erts_thr_fatal_error(res, "write unlock rwmutex");
#endif
}

#if 0 /* The following rwmtx function names are
	 reserved for potential future use. */ 

/* Try upgrade from r-locked state to rw-locked state */
ERTS_GLB_INLINE int
erts_rwmtx_trywlock(erts_rwmtx_t *rwmtx)
{
    return 0;
}

/* Upgrade from r-locked state to rw-locked state */
ERTS_GLB_INLINE void
erts_rwmtx_wlock(erts_rwmtx_t *rwmtx)
{

}

/* Downgrade from rw-locked state to r-locked state */
ERTS_GLB_INLINE void
erts_rwmtx_wunlock(erts_rwmtx_t *rwmtx)
{

}

#endif

ERTS_GLB_INLINE int
erts_lc_rwmtx_is_rlocked(erts_rwmtx_t *mtx)
{
#if defined(USE_THREADS) && defined(ERTS_ENABLE_LOCK_CHECK)
    int res;
    erts_lc_lock_t lc = mtx->lc;
    lc.flags = ERTS_LC_FLG_LO_READ;
    erts_lc_have_locks(&res, &lc, 1);
    return res;
#else
    return 0;
#endif
}

ERTS_GLB_INLINE int
erts_lc_rwmtx_is_rwlocked(erts_rwmtx_t *mtx)
{
#if defined(USE_THREADS) && defined(ERTS_ENABLE_LOCK_CHECK)
    int res;
    erts_lc_lock_t lc = mtx->lc;
    lc.flags = ERTS_LC_FLG_LO_READ|ERTS_LC_FLG_LO_WRITE;
    erts_lc_have_locks(&res, &lc, 1);
    return res;
#else
    return 0;
#endif
}

ERTS_GLB_INLINE void
erts_atomic_init(erts_atomic_t *var, long i)
{
#ifdef USE_THREADS
    int res = ethr_atomic_init(var, i);
    if (res)
	erts_thr_fatal_error(res, "perform atomic init");
#else
    *var = i;
#endif
}

ERTS_GLB_INLINE void
erts_atomic_set(erts_atomic_t *var, long i)
{
#ifdef USE_THREADS
    int res = ethr_atomic_set(var, i);
    if (res)
	erts_thr_fatal_error(res, "perform atomic set");
#else
    *var = i;
#endif
}

ERTS_GLB_INLINE long
erts_atomic_read(erts_atomic_t *var)
{
#ifdef USE_THREADS
    long i;
    int res = ethr_atomic_read(var, &i);
    if (res)
	erts_thr_fatal_error(res, "perform atomic read");
    return i;
#else
    return *var;
#endif
}

ERTS_GLB_INLINE long
erts_atomic_inctest(erts_atomic_t *incp)
{
#ifdef USE_THREADS
    long test;
    int res = ethr_atomic_inctest(incp, &test);
    if (res)
	erts_thr_fatal_error(res, "perform atomic increment and test");
    return test;
#else
    return ++(*incp);
#endif
}

ERTS_GLB_INLINE long
erts_atomic_dectest(erts_atomic_t *decp)
{
#ifdef USE_THREADS
    long test;
    int res = ethr_atomic_dectest(decp, &test);
    if (res)
	erts_thr_fatal_error(res, "perform atomic decrement and test");
    return test;
#else
    return --(*decp);
#endif
}

ERTS_GLB_INLINE void
erts_atomic_inc(erts_atomic_t *incp)
{
#ifdef USE_THREADS
    int res = ethr_atomic_inc(incp);
    if (res)
	erts_thr_fatal_error(res, "perform atomic increment");
#else
    ++(*incp);
#endif
}

ERTS_GLB_INLINE void
erts_atomic_dec(erts_atomic_t *decp)
{
#ifdef USE_THREADS
    int res = ethr_atomic_dec(decp);
    if (res)
	erts_thr_fatal_error(res, "perform atomic decrement");
#else
    --(*decp);
#endif
}

ERTS_GLB_INLINE long
erts_atomic_addtest(erts_atomic_t *addp, long i)
{
#ifdef USE_THREADS
    long test;
    int res = ethr_atomic_addtest(addp, i, &test);
    if (res)
	erts_thr_fatal_error(res, "perform atomic addition and test");
    return test;
#else
    return *addp += i;
#endif
}

ERTS_GLB_INLINE void
erts_atomic_add(erts_atomic_t *addp, long i)
{
#ifdef USE_THREADS
    int res = ethr_atomic_add(addp, i);
    if (res)
	erts_thr_fatal_error(res, "perform atomic addition");
#else
    *addp += i;
#endif
}

ERTS_GLB_INLINE long
erts_atomic_xchg(erts_atomic_t *xchgp, long new)
{
    long old;
#ifdef USE_THREADS
    int res = ethr_atomic_xchg(xchgp, new, &old);
    if (res)
	erts_thr_fatal_error(res, "perform atomic exchange");
#else
    old = *xchgp;
    *xchgp = new;
#endif
    return old;
}

ERTS_GLB_INLINE long
erts_atomic_cmpxchg(erts_atomic_t *xchgp, long new, long expected)
{
#ifdef USE_THREADS
    long old;
    int res = ethr_atomic_cmpxchg(xchgp, new, expected, &old);
    if (ERTS_UNLIKELY(res != 0))
	erts_thr_fatal_error(res, "perform atomic exchange");
    return old;
#else
    long old = *xchgp;
    if (old == expected)
        *xchgp = new;
    return old;
#endif
}

ERTS_GLB_INLINE long
erts_atomic_bor(erts_atomic_t *var, long mask)
{
    long old;
#ifdef USE_THREADS
    int res = ethr_atomic_or_old(var, mask, &old);
    if (res != 0)
	erts_thr_fatal_error(res, "perform atomic bitwise or");
#else
    old = *var;
    *var |= mask;
#endif
    return old;
}

ERTS_GLB_INLINE long
erts_atomic_band(erts_atomic_t *var, long mask)
{
    long old;
#ifdef USE_THREADS
    int res = ethr_atomic_and_old(var, mask, &old);
    if (res != 0)
	erts_thr_fatal_error(res, "perform atomic bitwise and");
#else
    old = *var;
    *var &= mask;
#endif
    return old;
}

/* spinlock */

ERTS_GLB_INLINE void
erts_spinlock_init_x(erts_spinlock_t *lock, char *name, Eterm extra)
{
#ifdef USE_THREADS
    int res = ethr_spinlock_init(&lock->slck);
    if (res)
	erts_thr_fatal_error(res, "init spinlock");
#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_lc_init_lock_x(&lock->lc, name, ERTS_LC_FLG_LT_SPINLOCK, extra);
#endif
#ifdef ERTS_ENABLE_LOCK_COUNT
    erts_lcnt_init_lock_x(&lock->lcnt, name, ERTS_LCNT_LT_SPINLOCK, extra);
#endif
#else
    (void)lock;
#endif
}

ERTS_GLB_INLINE void
erts_spinlock_init(erts_spinlock_t *lock, char *name)
{
#ifdef USE_THREADS
    int res = ethr_spinlock_init(&lock->slck);
    if (res)
	erts_thr_fatal_error(res, "init spinlock");
#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_lc_init_lock(&lock->lc, name, ERTS_LC_FLG_LT_SPINLOCK);
#endif
#ifdef ERTS_ENABLE_LOCK_COUNT
    erts_lcnt_init_lock(&lock->lcnt, name, ERTS_LCNT_LT_SPINLOCK);
#endif
#else
    (void)lock;
#endif
}

ERTS_GLB_INLINE void
erts_spinlock_destroy(erts_spinlock_t *lock)
{
#ifdef USE_THREADS
    int res;
#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_lc_destroy_lock(&lock->lc);
#endif
#ifdef ERTS_ENABLE_LOCK_COUNT
    erts_lcnt_destroy_lock(&lock->lcnt);
#endif
    res = ethr_spinlock_destroy(&lock->slck);
    if (res)
	erts_thr_fatal_error(res, "destroy spinlock");
#else
    (void)lock;
#endif
}

ERTS_GLB_INLINE void
erts_spin_unlock(erts_spinlock_t *lock)
{
#ifdef USE_THREADS
    int res;
#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_lc_unlock(&lock->lc);
#endif
#ifdef ERTS_ENABLE_LOCK_COUNT
    erts_lcnt_unlock(&lock->lcnt);
#endif
    res = ethr_spin_unlock(&lock->slck);
    if (res)
	erts_thr_fatal_error(res, "release spin lock");
#else
    (void)lock;
#endif
}

ERTS_GLB_INLINE void
#ifdef ERTS_ENABLE_LOCK_COUNT
erts_spin_lock_x(erts_spinlock_t *lock, char *file, unsigned int line)
#else
erts_spin_lock(erts_spinlock_t *lock)
#endif
{
#ifdef USE_THREADS
    int res;
#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_lc_lock(&lock->lc);
#endif
#ifdef ERTS_ENABLE_LOCK_COUNT
    erts_lcnt_lock(&lock->lcnt);
#endif
    res = ethr_spin_lock(&lock->slck);
#ifdef ERTS_ENABLE_LOCK_COUNT
    erts_lcnt_lock_post_x(&lock->lcnt, file, line);
#endif
    if (res)
	erts_thr_fatal_error(res, "take spin lock");
#else
    (void)lock;
#endif
}

ERTS_GLB_INLINE int
erts_lc_spinlock_is_locked(erts_spinlock_t *lock)
{
#if defined(USE_THREADS) && defined(ERTS_ENABLE_LOCK_CHECK)
    int res;
    erts_lc_lock_t lc = lock->lc;
    lc.flags = 0;
    erts_lc_have_locks(&res, &lc, 1);
    return res;
#else
    return 0;
#endif
}

/* rwspinlock */

ERTS_GLB_INLINE void
erts_rwlock_init_x(erts_rwlock_t *lock, char *name, Eterm extra)
{
#ifdef USE_THREADS
    int res = ethr_rwlock_init(&lock->rwlck);
    if (res)
	erts_thr_fatal_error(res, "init rwlock");
#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_lc_init_lock_x(&lock->lc, name, ERTS_LC_FLG_LT_RWSPINLOCK, extra);
#endif
#ifdef ERTS_ENABLE_LOCK_COUNT
    erts_lcnt_init_lock_x(&lock->lcnt, name, ERTS_LCNT_LT_RWSPINLOCK, extra);
#endif
#else
    (void)lock;
#endif
}

ERTS_GLB_INLINE void
erts_rwlock_init(erts_rwlock_t *lock, char *name)
{
#ifdef USE_THREADS
    int res = ethr_rwlock_init(&lock->rwlck);
    if (res)
	erts_thr_fatal_error(res, "init rwlock");
#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_lc_init_lock(&lock->lc, name, ERTS_LC_FLG_LT_RWSPINLOCK);
#endif
#ifdef ERTS_ENABLE_LOCK_COUNT
    erts_lcnt_init_lock(&lock->lcnt, name, ERTS_LCNT_LT_RWSPINLOCK);
#endif
#else
    (void)lock;
#endif
}

ERTS_GLB_INLINE void
erts_rwlock_destroy(erts_rwlock_t *lock)
{
#ifdef USE_THREADS
    int res;
#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_lc_destroy_lock(&lock->lc);
#endif
#ifdef ERTS_ENABLE_LOCK_COUNT
    erts_lcnt_destroy_lock(&lock->lcnt);
#endif
    res = ethr_rwlock_destroy(&lock->rwlck);
    if (res)
	erts_thr_fatal_error(res, "destroy rwlock");
#else
    (void)lock;
#endif
}

ERTS_GLB_INLINE void
erts_read_unlock(erts_rwlock_t *lock)
{
#ifdef USE_THREADS
    int res;
#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_lc_unlock_flg(&lock->lc, ERTS_LC_FLG_LO_READ);
#endif
#ifdef ERTS_ENABLE_LOCK_COUNT
    erts_lcnt_unlock_opt(&lock->lcnt, ERTS_LCNT_LO_READ);
#endif
    res = ethr_read_unlock(&lock->rwlck);
    if (res)
	erts_thr_fatal_error(res, "release read lock");
#else
    (void)lock;
#endif
}

ERTS_GLB_INLINE void
#ifdef ERTS_ENABLE_LOCK_COUNT
erts_read_lock_x(erts_rwlock_t *lock, char *file, unsigned int line)
#else
erts_read_lock(erts_rwlock_t *lock)
#endif
{
#ifdef USE_THREADS
    int res;
#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_lc_lock_flg(&lock->lc, ERTS_LC_FLG_LO_READ);
#endif
#ifdef ERTS_ENABLE_LOCK_COUNT
    erts_lcnt_lock_opt(&lock->lcnt, ERTS_LCNT_LO_READ);
#endif
    res = ethr_read_lock(&lock->rwlck);
#ifdef ERTS_ENABLE_LOCK_COUNT
    erts_lcnt_lock_post_x(&lock->lcnt, file, line);
#endif
    if (res)
	erts_thr_fatal_error(res, "take read lock");
#else
    (void)lock;
#endif
}

ERTS_GLB_INLINE void
erts_write_unlock(erts_rwlock_t *lock)
{
#ifdef USE_THREADS
    int res;
#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_lc_unlock_flg(&lock->lc, ERTS_LC_FLG_LO_READ_WRITE);
#endif
#ifdef ERTS_ENABLE_LOCK_COUNT
    erts_lcnt_unlock_opt(&lock->lcnt, ERTS_LCNT_LO_READ_WRITE);
#endif
    res = ethr_write_unlock(&lock->rwlck);
    if (res)
	erts_thr_fatal_error(res, "release write lock");
#else
    (void)lock;
#endif
}

ERTS_GLB_INLINE void
#ifdef ERTS_ENABLE_LOCK_COUNT
erts_write_lock_x(erts_rwlock_t *lock, char *file, unsigned int line)
#else
erts_write_lock(erts_rwlock_t *lock)
#endif
{
#ifdef USE_THREADS
    int res;
#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_lc_lock_flg(&lock->lc, ERTS_LC_FLG_LO_READ_WRITE);
#endif
#ifdef ERTS_ENABLE_LOCK_COUNT
    erts_lcnt_lock_opt(&lock->lcnt, ERTS_LCNT_LO_READ_WRITE);
#endif
    res = ethr_write_lock(&lock->rwlck);
#ifdef ERTS_ENABLE_LOCK_COUNT
    erts_lcnt_lock_post_x(&lock->lcnt, file, line);
#endif
    if (res)
	erts_thr_fatal_error(res, "take write lock");
#else
    (void)lock;
#endif
}

ERTS_GLB_INLINE int
erts_lc_rwlock_is_rlocked(erts_rwlock_t *lock)
{
#if defined(USE_THREADS) && defined(ERTS_ENABLE_LOCK_CHECK)
    int res;
    erts_lc_lock_t lc = lock->lc;
    lc.flags = ERTS_LC_FLG_LO_READ;
    erts_lc_have_locks(&res, &lc, 1);
    return res;
#else
    return 0;
#endif
}

ERTS_GLB_INLINE int
erts_lc_rwlock_is_rwlocked(erts_rwlock_t *lock)
{
#if defined(USE_THREADS) && defined(ERTS_ENABLE_LOCK_CHECK)
    int res;
    erts_lc_lock_t lc = lock->lc;
    lc.flags = ERTS_LC_FLG_LO_READ|ERTS_LC_FLG_LO_WRITE;
    erts_lc_have_locks(&res, &lc, 1);
    return res;
#else
    return 0;
#endif
}

ERTS_GLB_INLINE void
erts_thr_time_now(erts_thr_timeval_t *time)
{
#ifdef USE_THREADS
    int res = ethr_time_now(time);
    if (res)
	erts_thr_fatal_error(res, "get current time");
#endif
}

ERTS_GLB_INLINE void
erts_tsd_key_create(erts_tsd_key_t *keyp)
{
#ifdef USE_THREADS
    int res = ethr_tsd_key_create(keyp);
    if (res)
	erts_thr_fatal_error(res, "create thread specific data key");
#endif
}

ERTS_GLB_INLINE void
erts_tsd_key_delete(erts_tsd_key_t key)
{
#ifdef USE_THREADS
    int res = ethr_tsd_key_delete(key);
    if (res)
	erts_thr_fatal_error(res, "delete thread specific data key");
#endif
}

ERTS_GLB_INLINE void
erts_tsd_set(erts_tsd_key_t key, void *value)
{
#ifdef USE_THREADS
    int res = ethr_tsd_set(key, value);
    if (res)
	erts_thr_fatal_error(res, "set thread specific data");
#endif
}

ERTS_GLB_INLINE void *
erts_tsd_get(erts_tsd_key_t key)
{
#ifdef USE_THREADS
    return ethr_tsd_get(key);
#else
    return NULL;
#endif
}

ERTS_GLB_INLINE void
erts_gate_init(erts_gate_t *gp)
{
#ifdef USE_THREADS
    int res = ethr_gate_init((ethr_gate *) gp);
    if (res != 0)
	erts_thr_fatal_error(res, "initialize gate");
#endif
}

ERTS_GLB_INLINE void
erts_gate_destroy(erts_gate_t *gp)
{
#ifdef USE_THREADS
    int res = ethr_gate_destroy((ethr_gate *) gp);
    if (res != 0)
	erts_thr_fatal_error(res, "destroy gate");
#endif
}

ERTS_GLB_INLINE void
erts_gate_close(erts_gate_t *gp)
{
#ifdef USE_THREADS
    int res = ethr_gate_close((ethr_gate *) gp);
    if (res != 0)
	erts_thr_fatal_error(res, "close gate");
#endif
}

ERTS_GLB_INLINE void
erts_gate_let_through(erts_gate_t *gp, unsigned no)
{
#ifdef USE_THREADS
    int res = ethr_gate_let_through((ethr_gate *) gp, no);
    if (res != 0)
	erts_thr_fatal_error(res, "let through gate");
#endif
}

ERTS_GLB_INLINE void
erts_gate_wait(erts_gate_t *gp)
{
#ifdef USE_THREADS
    int res = ethr_gate_wait((ethr_gate *) gp);
    if (res != 0)
	erts_thr_fatal_error(res, "wait on gate");
#endif
}

ERTS_GLB_INLINE void
erts_gate_swait(erts_gate_t *gp, int spincount)
{
#ifdef USE_THREADS
    int res = ethr_gate_swait((ethr_gate *) gp, spincount);
    if (res != 0)
	erts_thr_fatal_error(res, "swait on gate");
#endif
}

#ifdef ETHR_HAVE_ETHR_SIG_FUNCS

ERTS_GLB_INLINE void
erts_thr_sigmask(int how, const sigset_t *set, sigset_t *oset)
{
#ifdef USE_THREADS
    int res = ethr_sigmask(how, set, oset);
    if (res)
	erts_thr_fatal_error(res, "get or set signal mask");
#endif
}

ERTS_GLB_INLINE void
erts_thr_sigwait(const sigset_t *set, int *sig)
{
#ifdef USE_THREADS
    int res;
    do {
	res = ethr_sigwait(set, sig);
    } while (res == EINTR);
    if (res)
	erts_thr_fatal_error(res, "to wait for signal");
#endif
}

#endif /* #ifdef HAVE_ETHR_SIG_FUNCS */

#endif /* #if ERTS_GLB_INLINE_INCL_FUNC_DEF */

#endif /* #ifndef ERL_THREAD_H__ */
