/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2005-2009. All Rights Reserved.
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
 * SMP interface to ethread library.
 * This is essentially "sed s/erts_/erts_smp_/g < erl_threads.h > erl_smp.h",
 * plus changes to NOP operations when ERTS_SMP is disabled.
 * Author: Mikael Pettersson
 */
#ifndef ERL_SMP_H
#define ERL_SMP_H
#include "erl_threads.h"

#ifdef ERTS_ENABLE_LOCK_COUNT
#define erts_smp_mtx_lock(L) erts_smp_mtx_lock_x(L, __FILE__, __LINE__)
#define erts_smp_spin_lock(L) erts_smp_spin_lock_x(L, __FILE__, __LINE__)
#define erts_smp_rwmtx_rlock(L) erts_smp_rwmtx_rlock_x(L, __FILE__, __LINE__)
#define erts_smp_rwmtx_rwlock(L) erts_smp_rwmtx_rwlock_x(L, __FILE__, __LINE__)
#define erts_smp_read_lock(L) erts_smp_read_lock_x(L, __FILE__, __LINE__)
#define erts_smp_write_lock(L) erts_smp_write_lock_x(L, __FILE__, __LINE__)
#endif


#ifdef ERTS_SMP
#define ERTS_SMP_THR_OPTS_DEFAULT_INITER ERTS_THR_OPTS_DEFAULT_INITER
typedef erts_thr_opts_t erts_smp_thr_opts_t;
typedef erts_thr_init_data_t erts_smp_thr_init_data_t;
typedef erts_tid_t erts_smp_tid_t;
typedef erts_mtx_t erts_smp_mtx_t;
typedef erts_cnd_t erts_smp_cnd_t;
typedef erts_rwmtx_t erts_smp_rwmtx_t;
typedef erts_tsd_key_t erts_smp_tsd_key_t;
typedef erts_gate_t erts_smp_gate_t;
typedef ethr_atomic_t erts_smp_atomic_t;
typedef erts_spinlock_t erts_smp_spinlock_t;
typedef erts_rwlock_t erts_smp_rwlock_t;
typedef erts_thr_timeval_t erts_smp_thr_timeval_t;
void erts_thr_fatal_error(int, char *); /* implemented in erl_init.c */

#else /* #ifdef ERTS_SMP */

#define ERTS_SMP_THR_OPTS_DEFAULT_INITER 0
typedef int erts_smp_thr_opts_t;
typedef int erts_smp_thr_init_data_t;
typedef int erts_smp_tid_t;
typedef int erts_smp_mtx_t;
typedef int erts_smp_cnd_t;
typedef int erts_smp_rwmtx_t;
typedef int erts_smp_tsd_key_t;
typedef int erts_smp_gate_t;
typedef long erts_smp_atomic_t;
#if __GNUC__ > 2
typedef struct { } erts_smp_spinlock_t;
typedef struct { } erts_smp_rwlock_t;
#else
typedef struct { int gcc_is_buggy; } erts_smp_spinlock_t;
typedef struct { int gcc_is_buggy; } erts_smp_rwlock_t;
#endif

typedef struct {
    long tv_sec;
    long tv_nsec;
} erts_smp_thr_timeval_t;

#endif /* #ifdef ERTS_SMP */

ERTS_GLB_INLINE void erts_smp_thr_init(erts_smp_thr_init_data_t *id);
ERTS_GLB_INLINE void erts_smp_thr_create(erts_smp_tid_t *tid,
					 void * (*func)(void *),
					 void *arg,
					 erts_smp_thr_opts_t *opts);
ERTS_GLB_INLINE void erts_smp_thr_join(erts_smp_tid_t tid, void **thr_res);
ERTS_GLB_INLINE void erts_smp_thr_detach(erts_smp_tid_t tid);
ERTS_GLB_INLINE void erts_smp_thr_exit(void *res);
ERTS_GLB_INLINE void erts_smp_install_exit_handler(void (*exit_handler)(void));
ERTS_GLB_INLINE erts_smp_tid_t erts_smp_thr_self(void);
ERTS_GLB_INLINE int erts_smp_equal_tids(erts_smp_tid_t x, erts_smp_tid_t y);
#ifdef ERTS_HAVE_REC_MTX_INIT
#define ERTS_SMP_HAVE_REC_MTX_INIT 1
ERTS_GLB_INLINE void erts_smp_rec_mtx_init(erts_smp_mtx_t *mtx);
#endif
ERTS_GLB_INLINE void erts_smp_mtx_init_x(erts_smp_mtx_t *mtx,
					 char *name,
					 Eterm extra);
ERTS_GLB_INLINE void erts_smp_mtx_init_locked_x(erts_smp_mtx_t *mtx,
						char *name,
						Eterm extra);
ERTS_GLB_INLINE void erts_smp_mtx_init(erts_smp_mtx_t *mtx, char *name);
ERTS_GLB_INLINE void erts_smp_mtx_init_locked(erts_smp_mtx_t *mtx, char *name);
ERTS_GLB_INLINE void erts_smp_mtx_destroy(erts_smp_mtx_t *mtx);
ERTS_GLB_INLINE void erts_smp_mtx_set_forksafe(erts_smp_mtx_t *mtx);
ERTS_GLB_INLINE void erts_smp_mtx_unset_forksafe(erts_smp_mtx_t *mtx);
ERTS_GLB_INLINE int erts_smp_mtx_trylock(erts_smp_mtx_t *mtx);
#ifdef ERTS_ENABLE_LOCK_COUNT
ERTS_GLB_INLINE void erts_smp_mtx_lock_x(erts_smp_mtx_t *mtx, char *file, int line);
#else
ERTS_GLB_INLINE void erts_smp_mtx_lock(erts_smp_mtx_t *mtx);
#endif
ERTS_GLB_INLINE void erts_smp_mtx_unlock(erts_smp_mtx_t *mtx);
ERTS_GLB_INLINE int erts_smp_lc_mtx_is_locked(erts_smp_mtx_t *mtx);
ERTS_GLB_INLINE void erts_smp_cnd_init(erts_smp_cnd_t *cnd);
ERTS_GLB_INLINE void erts_smp_cnd_destroy(erts_smp_cnd_t *cnd);
ERTS_GLB_INLINE void erts_smp_cnd_wait(erts_smp_cnd_t *cnd,
					   erts_smp_mtx_t *mtx);
ERTS_GLB_INLINE void erts_smp_cnd_signal(erts_smp_cnd_t *cnd);
ERTS_GLB_INLINE void erts_smp_cnd_broadcast(erts_smp_cnd_t *cnd);
ERTS_GLB_INLINE void erts_smp_rwmtx_init_x(erts_smp_rwmtx_t *rwmtx,
					   char *name,
					   Eterm extra);
ERTS_GLB_INLINE void erts_smp_rwmtx_init(erts_smp_rwmtx_t *rwmtx,
					 char *name);
ERTS_GLB_INLINE void erts_smp_rwmtx_destroy(erts_smp_rwmtx_t *rwmtx);
ERTS_GLB_INLINE int erts_smp_rwmtx_tryrlock(erts_smp_rwmtx_t *rwmtx);
#ifdef ERTS_ENABLE_LOCK_COUNT
ERTS_GLB_INLINE void erts_smp_rwmtx_rlock_x(erts_smp_rwmtx_t *rwmtx, char *file, unsigned int line);
ERTS_GLB_INLINE void erts_smp_rwmtx_rwlock_x(erts_smp_rwmtx_t *rwmtx, char *file, unsigned int line);
#else
ERTS_GLB_INLINE void erts_smp_rwmtx_rlock(erts_smp_rwmtx_t *rwmtx);
ERTS_GLB_INLINE void erts_smp_rwmtx_rwlock(erts_smp_rwmtx_t *rwmtx);
#endif
ERTS_GLB_INLINE void erts_smp_rwmtx_runlock(erts_smp_rwmtx_t *rwmtx);
ERTS_GLB_INLINE int erts_smp_rwmtx_tryrwlock(erts_smp_rwmtx_t *rwmtx);
ERTS_GLB_INLINE void erts_smp_rwmtx_rwunlock(erts_smp_rwmtx_t *rwmtx);
ERTS_GLB_INLINE int erts_smp_lc_rwmtx_is_rlocked(erts_smp_rwmtx_t *mtx);
ERTS_GLB_INLINE int erts_smp_lc_rwmtx_is_rwlocked(erts_smp_rwmtx_t *mtx);
ERTS_GLB_INLINE void erts_smp_atomic_init(erts_smp_atomic_t *var, long i);
ERTS_GLB_INLINE void erts_smp_atomic_set(erts_smp_atomic_t *var, long i);
ERTS_GLB_INLINE long erts_smp_atomic_read(erts_smp_atomic_t *var);
ERTS_GLB_INLINE long erts_smp_atomic_inctest(erts_smp_atomic_t *incp);
ERTS_GLB_INLINE long erts_smp_atomic_dectest(erts_smp_atomic_t *decp);
ERTS_GLB_INLINE void erts_smp_atomic_inc(erts_smp_atomic_t *incp);
ERTS_GLB_INLINE void erts_smp_atomic_dec(erts_smp_atomic_t *decp);
ERTS_GLB_INLINE long erts_smp_atomic_addtest(erts_smp_atomic_t *addp,
					     long i);
ERTS_GLB_INLINE void erts_smp_atomic_add(erts_smp_atomic_t *addp, long i);
ERTS_GLB_INLINE long erts_smp_atomic_xchg(erts_smp_atomic_t *xchgp,
					  long new);
ERTS_GLB_INLINE long erts_smp_atomic_cmpxchg(erts_smp_atomic_t *xchgp,
					     long new,
					     long expected);
ERTS_GLB_INLINE long erts_smp_atomic_bor(erts_smp_atomic_t *var, long mask);
ERTS_GLB_INLINE long erts_smp_atomic_band(erts_smp_atomic_t *var, long mask);
ERTS_GLB_INLINE void erts_smp_spinlock_init_x(erts_smp_spinlock_t *lock,
					      char *name,
					      Eterm extra);
ERTS_GLB_INLINE void erts_smp_spinlock_init(erts_smp_spinlock_t *lock,
					    char *name);
ERTS_GLB_INLINE void erts_smp_spinlock_destroy(erts_smp_spinlock_t *lock);
ERTS_GLB_INLINE void erts_smp_spin_unlock(erts_smp_spinlock_t *lock);
#ifdef ERTS_ENABLE_LOCK_COUNT
ERTS_GLB_INLINE void erts_smp_spin_lock_x(erts_smp_spinlock_t *lock, char *file, unsigned int line);
#else
ERTS_GLB_INLINE void erts_smp_spin_lock(erts_smp_spinlock_t *lock);
#endif
ERTS_GLB_INLINE int erts_smp_lc_spinlock_is_locked(erts_smp_spinlock_t *lock);
ERTS_GLB_INLINE void erts_smp_rwlock_init_x(erts_smp_rwlock_t *lock,
					    char *name,
					    Eterm extra);
ERTS_GLB_INLINE void erts_smp_rwlock_init(erts_smp_rwlock_t *lock,
					  char *name);
ERTS_GLB_INLINE void erts_smp_rwlock_destroy(erts_smp_rwlock_t *lock);
ERTS_GLB_INLINE void erts_smp_read_unlock(erts_smp_rwlock_t *lock);
#ifdef ERTS_ENABLE_LOCK_COUNT
ERTS_GLB_INLINE void erts_smp_read_lock_x(erts_smp_rwlock_t *lock, char *file, unsigned int line);
ERTS_GLB_INLINE void erts_smp_write_lock_x(erts_smp_rwlock_t *lock, char *file, unsigned int line);
#else
ERTS_GLB_INLINE void erts_smp_read_lock(erts_smp_rwlock_t *lock);
ERTS_GLB_INLINE void erts_smp_write_lock(erts_smp_rwlock_t *lock);
#endif
ERTS_GLB_INLINE void erts_smp_write_unlock(erts_smp_rwlock_t *lock);
ERTS_GLB_INLINE int erts_smp_lc_rwlock_is_rlocked(erts_smp_rwlock_t *lock);
ERTS_GLB_INLINE int erts_smp_lc_rwlock_is_rwlocked(erts_smp_rwlock_t *lock);
ERTS_GLB_INLINE void erts_smp_thr_time_now(erts_smp_thr_timeval_t *time);
ERTS_GLB_INLINE void erts_smp_tsd_key_create(erts_smp_tsd_key_t *keyp);
ERTS_GLB_INLINE void erts_smp_tsd_key_delete(erts_smp_tsd_key_t key);
ERTS_GLB_INLINE void erts_smp_tsd_set(erts_smp_tsd_key_t key, void *value);
ERTS_GLB_INLINE void * erts_smp_tsd_get(erts_smp_tsd_key_t key);
ERTS_GLB_INLINE void erts_smp_gate_init(erts_smp_gate_t *gp);
ERTS_GLB_INLINE void erts_smp_gate_destroy(erts_smp_gate_t *gp);
ERTS_GLB_INLINE void erts_smp_gate_close(erts_smp_gate_t *gp);
ERTS_GLB_INLINE void erts_smp_gate_let_through(erts_smp_gate_t *gp, unsigned no);
ERTS_GLB_INLINE void erts_smp_gate_wait(erts_smp_gate_t *gp);
ERTS_GLB_INLINE void erts_smp_gate_swait(erts_smp_gate_t *gp, int spincount);

#ifdef ERTS_THR_HAVE_SIG_FUNCS
#define ERTS_SMP_THR_HAVE_SIG_FUNCS 1
ERTS_GLB_INLINE void erts_smp_thr_sigmask(int how,
					  const sigset_t *set,
					  sigset_t *oset);
ERTS_GLB_INLINE void erts_smp_thr_sigwait(const sigset_t *set, int *sig);
#endif /* #ifdef ERTS_THR_HAVE_SIG_FUNCS */


#if ERTS_GLB_INLINE_INCL_FUNC_DEF

ERTS_GLB_INLINE void
erts_smp_thr_init(erts_smp_thr_init_data_t *id)
{
#ifdef ERTS_SMP
    erts_thr_init(id);
#endif
}

ERTS_GLB_INLINE void
erts_smp_thr_create(erts_smp_tid_t *tid, void * (*func)(void *), void *arg,
		    erts_smp_thr_opts_t *opts)
{
#ifdef ERTS_SMP
    erts_thr_create(tid, func, arg, opts);
#endif
}

ERTS_GLB_INLINE void
erts_smp_thr_join(erts_smp_tid_t tid, void **thr_res)
{
#ifdef ERTS_SMP
    erts_thr_join(tid, thr_res);
#endif
}


ERTS_GLB_INLINE void
erts_smp_thr_detach(erts_smp_tid_t tid)
{
#ifdef ERTS_SMP
    erts_thr_detach(tid);
#endif
}


ERTS_GLB_INLINE void
erts_smp_thr_exit(void *res)
{
#ifdef ERTS_SMP
    erts_thr_exit(res);
#endif
}

ERTS_GLB_INLINE void
erts_smp_install_exit_handler(void (*exit_handler)(void))
{
#ifdef ERTS_SMP
    erts_thr_install_exit_handler(exit_handler);
#endif
}

ERTS_GLB_INLINE erts_smp_tid_t
erts_smp_thr_self(void)
{
#ifdef ERTS_SMP
    return erts_thr_self();
#else
    return 0;
#endif
}


ERTS_GLB_INLINE int
erts_smp_equal_tids(erts_smp_tid_t x, erts_smp_tid_t y)
{
#ifdef ERTS_SMP
    return erts_equal_tids(x, y);
#else
    return 1;
#endif
}


#ifdef ERTS_HAVE_REC_MTX_INIT
ERTS_GLB_INLINE void
erts_smp_rec_mtx_init(erts_smp_mtx_t *mtx)
{
#ifdef ERTS_SMP
    erts_rec_mtx_init(mtx);
#endif
}
#endif

ERTS_GLB_INLINE void
erts_smp_mtx_init_x(erts_smp_mtx_t *mtx, char *name, Eterm extra)
{
#ifdef ERTS_SMP
    erts_mtx_init_x(mtx, name, extra);
#endif
}

ERTS_GLB_INLINE void
erts_smp_mtx_init_locked_x(erts_smp_mtx_t *mtx, char *name, Eterm extra)
{
#ifdef ERTS_SMP
    erts_mtx_init_locked_x(mtx, name, extra);
#endif
}

ERTS_GLB_INLINE void
erts_smp_mtx_init(erts_smp_mtx_t *mtx, char *name)
{
#ifdef ERTS_SMP
    erts_mtx_init(mtx, name);
#endif
}

ERTS_GLB_INLINE void
erts_smp_mtx_init_locked(erts_smp_mtx_t *mtx, char *name)
{
#ifdef ERTS_SMP
    erts_mtx_init_locked(mtx, name);
#endif
}

ERTS_GLB_INLINE void
erts_smp_mtx_destroy(erts_smp_mtx_t *mtx)
{
#ifdef ERTS_SMP
    erts_mtx_destroy(mtx);
#endif
}

ERTS_GLB_INLINE void
erts_smp_mtx_set_forksafe(erts_smp_mtx_t *mtx)
{
#ifdef ERTS_SMP
    erts_mtx_set_forksafe(mtx);
#endif
}

ERTS_GLB_INLINE void
erts_smp_mtx_unset_forksafe(erts_smp_mtx_t *mtx)
{
#ifdef ERTS_SMP
    erts_mtx_unset_forksafe(mtx);
#endif
}

ERTS_GLB_INLINE int
erts_smp_mtx_trylock(erts_smp_mtx_t *mtx)
{
#ifdef ERTS_SMP
    return erts_mtx_trylock(mtx);
#else
    return 0;
#endif

}


ERTS_GLB_INLINE void
#ifdef ERTS_ENABLE_LOCK_COUNT
erts_smp_mtx_lock_x(erts_smp_mtx_t *mtx, char *file, int line)
#else
erts_smp_mtx_lock(erts_smp_mtx_t *mtx)
#endif
{
#if defined(ERTS_SMP) && defined(ERTS_ENABLE_LOCK_COUNT)
    erts_mtx_lock_x(mtx, file, line);
#elif defined(ERTS_SMP)
    erts_mtx_lock(mtx);
#endif
}

ERTS_GLB_INLINE void
erts_smp_mtx_unlock(erts_smp_mtx_t *mtx)
{
#ifdef ERTS_SMP
    erts_mtx_unlock(mtx);
#endif
}

ERTS_GLB_INLINE int
erts_smp_lc_mtx_is_locked(erts_smp_mtx_t *mtx)
{
#if defined(ERTS_SMP) && defined(ERTS_ENABLE_LOCK_CHECK)
    return erts_lc_mtx_is_locked(mtx);
#else
    return 0;
#endif
}

ERTS_GLB_INLINE void
erts_smp_cnd_init(erts_smp_cnd_t *cnd)
{
#ifdef ERTS_SMP
    erts_cnd_init(cnd);
#endif
}

ERTS_GLB_INLINE void
erts_smp_cnd_destroy(erts_smp_cnd_t *cnd)
{
#ifdef ERTS_SMP
    erts_cnd_destroy(cnd);
#endif
}

ERTS_GLB_INLINE void
erts_smp_cnd_wait(erts_smp_cnd_t *cnd, erts_smp_mtx_t *mtx)
{
#ifdef ERTS_SMP
    erts_cnd_wait(cnd, mtx);
#endif
}

ERTS_GLB_INLINE void
erts_smp_cnd_signal(erts_smp_cnd_t *cnd)
{
#ifdef ERTS_SMP
    erts_cnd_signal(cnd);
#endif
}


ERTS_GLB_INLINE void
erts_smp_cnd_broadcast(erts_smp_cnd_t *cnd)
{
#ifdef ERTS_SMP
    erts_cnd_broadcast(cnd);
#endif
}

ERTS_GLB_INLINE void
erts_smp_rwmtx_init_x(erts_smp_rwmtx_t *rwmtx, char *name, Eterm extra)
{
#ifdef ERTS_SMP
    erts_rwmtx_init_x(rwmtx, name, extra);
#endif
}

ERTS_GLB_INLINE void
erts_smp_rwmtx_init(erts_smp_rwmtx_t *rwmtx, char *name)
{
#ifdef ERTS_SMP
    erts_rwmtx_init(rwmtx, name);
#endif
}

ERTS_GLB_INLINE void
erts_smp_rwmtx_destroy(erts_smp_rwmtx_t *rwmtx)
{
#ifdef ERTS_SMP
    erts_rwmtx_destroy(rwmtx);
#endif
}

ERTS_GLB_INLINE int
erts_smp_rwmtx_tryrlock(erts_smp_rwmtx_t *rwmtx)
{
#ifdef ERTS_SMP
    return erts_rwmtx_tryrlock(rwmtx);
#else
    return 0;
#endif
}

ERTS_GLB_INLINE void
#ifdef ERTS_ENABLE_LOCK_COUNT
erts_smp_rwmtx_rlock_x(erts_smp_rwmtx_t *rwmtx, char *file, unsigned int line)
#else
erts_smp_rwmtx_rlock(erts_smp_rwmtx_t *rwmtx)
#endif
{
#if defined(ERTS_SMP) && defined(ERTS_ENABLE_LOCK_COUNT)
    erts_rwmtx_rlock_x(rwmtx, file, line);
#elif defined(ERTS_SMP)
    erts_rwmtx_rlock(rwmtx);
#endif
}

ERTS_GLB_INLINE void
erts_smp_rwmtx_runlock(erts_smp_rwmtx_t *rwmtx)
{
#ifdef ERTS_SMP
    erts_rwmtx_runlock(rwmtx);
#endif
}


ERTS_GLB_INLINE int
erts_smp_rwmtx_tryrwlock(erts_smp_rwmtx_t *rwmtx)
{
#ifdef ERTS_SMP
    return erts_rwmtx_tryrwlock(rwmtx);
#else
    return 0;
#endif
}

ERTS_GLB_INLINE void
#ifdef ERTS_ENABLE_LOCK_COUNT
erts_smp_rwmtx_rwlock_x(erts_smp_rwmtx_t *rwmtx, char *file, unsigned int line)
#else
erts_smp_rwmtx_rwlock(erts_smp_rwmtx_t *rwmtx)
#endif
{
#if defined(ERTS_SMP) && defined(ERTS_ENABLE_LOCK_COUNT)
    erts_rwmtx_rwlock_x(rwmtx, file, line);
#elif defined(ERTS_SMP)
    erts_rwmtx_rwlock(rwmtx);
#endif
}

ERTS_GLB_INLINE void
erts_smp_rwmtx_rwunlock(erts_smp_rwmtx_t *rwmtx)
{
#ifdef ERTS_SMP
    erts_rwmtx_rwunlock(rwmtx);
#endif
}

#if 0 /* The following rwmtx function names are
	 reserved for potential future use. */ 

/* Try upgrade from r-locked state to rw-locked state */
ERTS_GLB_INLINE int
erts_smp_rwmtx_trywlock(erts_smp_rwmtx_t *rwmtx)
{
    return 0;
}

/* Upgrade from r-locked state to rw-locked state */
ERTS_GLB_INLINE void
erts_smp_rwmtx_wlock(erts_smp_rwmtx_t *rwmtx)
{

}

/* Downgrade from rw-locked state to r-locked state */
ERTS_GLB_INLINE void
erts_smp_rwmtx_wunlock(erts_smp_rwmtx_t *rwmtx)
{

}

#endif

ERTS_GLB_INLINE int
erts_smp_lc_rwmtx_is_rlocked(erts_smp_rwmtx_t *mtx)
{
#if defined(ERTS_SMP) && defined(ERTS_ENABLE_LOCK_CHECK)
    return erts_lc_rwmtx_is_rlocked(mtx);
#else
    return 0;
#endif
}

ERTS_GLB_INLINE int
erts_smp_lc_rwmtx_is_rwlocked(erts_smp_rwmtx_t *mtx)
{
#if defined(ERTS_SMP) && defined(ERTS_ENABLE_LOCK_CHECK)
    return erts_lc_rwmtx_is_rwlocked(mtx);
#else
    return 0;
#endif
}

ERTS_GLB_INLINE void
erts_smp_atomic_init(erts_smp_atomic_t *var, long i)
{
#ifdef ERTS_SMP
    erts_atomic_init(var, i);
#else
    *var = i;
#endif
}

ERTS_GLB_INLINE void
erts_smp_atomic_set(erts_smp_atomic_t *var, long i)
{
#ifdef ERTS_SMP
    erts_atomic_set(var, i);
#else
    *var = i;
#endif
}

ERTS_GLB_INLINE long
erts_smp_atomic_read(erts_smp_atomic_t *var)
{
#ifdef ERTS_SMP
    return erts_atomic_read(var);
#else
    return *var;
#endif
}

ERTS_GLB_INLINE long
erts_smp_atomic_inctest(erts_smp_atomic_t *incp)
{
#ifdef ERTS_SMP
    return erts_atomic_inctest(incp);
#else
    return ++(*incp);
#endif
}

ERTS_GLB_INLINE long
erts_smp_atomic_dectest(erts_smp_atomic_t *decp)
{
#ifdef ERTS_SMP
    return erts_atomic_dectest(decp);
#else
    return --(*decp);
#endif
}

ERTS_GLB_INLINE void
erts_smp_atomic_inc(erts_smp_atomic_t *incp)
{
#ifdef ERTS_SMP
    erts_atomic_inc(incp);
#else
    ++(*incp);
#endif
}

ERTS_GLB_INLINE void
erts_smp_atomic_dec(erts_smp_atomic_t *decp)
{
#ifdef ERTS_SMP
    erts_atomic_dec(decp);
#else
    --(*decp);
#endif
}

ERTS_GLB_INLINE long
erts_smp_atomic_addtest(erts_smp_atomic_t *addp, long i)
{
#ifdef ERTS_SMP
    return erts_atomic_addtest(addp, i);
#else
    return *addp += i;
#endif
}

ERTS_GLB_INLINE void
erts_smp_atomic_add(erts_smp_atomic_t *addp, long i)
{
#ifdef ERTS_SMP
    erts_atomic_add(addp, i);
#else
    *addp += i;
#endif
}

ERTS_GLB_INLINE long
erts_smp_atomic_xchg(erts_smp_atomic_t *xchgp, long new)
{
#ifdef ERTS_SMP
    return erts_atomic_xchg(xchgp, new);
#else
    long old;
    old = *xchgp;
    *xchgp = new;
    return old;
#endif
}

ERTS_GLB_INLINE long
erts_smp_atomic_cmpxchg(erts_smp_atomic_t *xchgp, long new, long expected)
{
#ifdef ERTS_SMP
    return erts_atomic_cmpxchg(xchgp, new, expected);
#else
    long old = *xchgp;
    if (old == expected)
        *xchgp = new;
    return old;
#endif
}

ERTS_GLB_INLINE long
erts_smp_atomic_bor(erts_smp_atomic_t *var, long mask)
{
#ifdef ERTS_SMP
    return erts_atomic_bor(var, mask);
#else
    long old;
    old = *var;
    *var |= mask;
    return old;
#endif
}

ERTS_GLB_INLINE long
erts_smp_atomic_band(erts_smp_atomic_t *var, long mask)
{
#ifdef ERTS_SMP
    return erts_atomic_band(var, mask);
#else
    long old;
    old = *var;
    *var &= mask;
    return old;
#endif
}

ERTS_GLB_INLINE void
erts_smp_spinlock_init_x(erts_smp_spinlock_t *lock, char *name, Eterm extra)
{
#ifdef ERTS_SMP
    erts_spinlock_init_x(lock, name, extra);
#else
    (void)lock;
#endif
}

ERTS_GLB_INLINE void
erts_smp_spinlock_init(erts_smp_spinlock_t *lock, char *name)
{
#ifdef ERTS_SMP
    erts_spinlock_init(lock, name);
#else
    (void)lock;
#endif
}

ERTS_GLB_INLINE void
erts_smp_spinlock_destroy(erts_smp_spinlock_t *lock)
{
#ifdef ERTS_SMP
    erts_spinlock_destroy(lock);
#else
    (void)lock;
#endif
}

ERTS_GLB_INLINE void
erts_smp_spin_unlock(erts_smp_spinlock_t *lock)
{
#ifdef ERTS_SMP
    erts_spin_unlock(lock);
#else
    (void)lock;
#endif
}

ERTS_GLB_INLINE void
#ifdef ERTS_ENABLE_LOCK_COUNT
erts_smp_spin_lock_x(erts_smp_spinlock_t *lock, char *file, unsigned int line)
#else
erts_smp_spin_lock(erts_smp_spinlock_t *lock)
#endif
{
#if defined(ERTS_SMP) && defined(ERTS_ENABLE_LOCK_COUNT)
    erts_spin_lock_x(lock, file, line);
#elif defined(ERTS_SMP)
    erts_spin_lock(lock);
#else
    (void)lock;
#endif
}

ERTS_GLB_INLINE int
erts_smp_lc_spinlock_is_locked(erts_smp_spinlock_t *lock)
{
#if defined(ERTS_SMP) && defined(ERTS_ENABLE_LOCK_CHECK)
    return erts_lc_spinlock_is_locked(lock);
#else
    return 0;
#endif
}

ERTS_GLB_INLINE void
erts_smp_rwlock_init_x(erts_smp_rwlock_t *lock, char *name, Eterm extra)
{
#ifdef ERTS_SMP
    erts_rwlock_init_x(lock, name, extra);
#else
    (void)lock;
#endif
}

ERTS_GLB_INLINE void
erts_smp_rwlock_init(erts_smp_rwlock_t *lock, char *name)
{
#ifdef ERTS_SMP
    erts_rwlock_init(lock, name);
#else
    (void)lock;
#endif
}

ERTS_GLB_INLINE void
erts_smp_rwlock_destroy(erts_smp_rwlock_t *lock)
{
#ifdef ERTS_SMP
    erts_rwlock_destroy(lock);
#else
    (void)lock;
#endif
}

ERTS_GLB_INLINE void
erts_smp_read_unlock(erts_smp_rwlock_t *lock)
{
#ifdef ERTS_SMP
    erts_read_unlock(lock);
#else
    (void)lock;
#endif
}

ERTS_GLB_INLINE void
#ifdef ERTS_ENABLE_LOCK_COUNT
erts_smp_read_lock_x(erts_smp_rwlock_t *lock, char *file, unsigned int line)
#else
erts_smp_read_lock(erts_smp_rwlock_t *lock)
#endif 
{
#if defined(ERTS_ENABLE_LOCK_COUNT) && defined(ERTS_SMP)
    erts_read_lock_x(lock, file, line);
#elif defined(ERTS_SMP)
    erts_read_lock(lock);
#else
    (void)lock;
#endif
}

ERTS_GLB_INLINE void
erts_smp_write_unlock(erts_smp_rwlock_t *lock)
{
#ifdef ERTS_SMP
    erts_write_unlock(lock);
#else
    (void)lock;
#endif
}

ERTS_GLB_INLINE void
#ifdef ERTS_ENABLE_LOCK_COUNT
erts_smp_write_lock_x(erts_smp_rwlock_t *lock, char *file, unsigned int line)
#else
erts_smp_write_lock(erts_smp_rwlock_t *lock)
#endif 
{
#if defined(ERTS_SMP) && defined(ERTS_ENABLE_LOCK_COUNT)
    erts_write_lock_x(lock, file, line);
#elif defined(ERTS_SMP)
    erts_write_lock(lock);
#else
    (void)lock;
#endif
}

ERTS_GLB_INLINE int
erts_smp_lc_rwlock_is_rlocked(erts_smp_rwlock_t *lock)
{
#if defined(ERTS_SMP) && defined(ERTS_ENABLE_LOCK_CHECK)
    return erts_lc_rwlock_is_rlocked(lock);
#else
    return 0;
#endif
}

ERTS_GLB_INLINE int
erts_smp_lc_rwlock_is_rwlocked(erts_smp_rwlock_t *lock)
{
#if defined(ERTS_SMP) && defined(ERTS_ENABLE_LOCK_CHECK)
    return erts_lc_rwlock_is_rwlocked(lock);
#else
    return 0;
#endif
}

ERTS_GLB_INLINE void
erts_smp_thr_time_now(erts_smp_thr_timeval_t *time)
{
#ifdef ERTS_SMP
    erts_thr_time_now(time);
#endif
}

ERTS_GLB_INLINE void
erts_smp_tsd_key_create(erts_smp_tsd_key_t *keyp)
{
#ifdef ERTS_SMP
    erts_tsd_key_create(keyp);
#endif
}

ERTS_GLB_INLINE void
erts_smp_tsd_key_delete(erts_smp_tsd_key_t key)
{
#ifdef ERTS_SMP
    erts_tsd_key_delete(key);
#endif
}

ERTS_GLB_INLINE void
erts_smp_tsd_set(erts_smp_tsd_key_t key, void *value)
{
#ifdef ERTS_SMP
    erts_tsd_set(key, value);
#endif
}

ERTS_GLB_INLINE void *
erts_smp_tsd_get(erts_smp_tsd_key_t key)
{
#ifdef ERTS_SMP
    return erts_tsd_get(key);
#else
    return NULL;
#endif
}

ERTS_GLB_INLINE void
erts_smp_gate_init(erts_smp_gate_t *gp)
{
#ifdef ERTS_SMP
    erts_gate_init((erts_gate_t *) gp);
#endif
}

ERTS_GLB_INLINE void
erts_smp_gate_destroy(erts_smp_gate_t *gp)
{
#ifdef ERTS_SMP
    erts_gate_destroy((erts_gate_t *) gp);
#endif
}

ERTS_GLB_INLINE void
erts_smp_gate_close(erts_smp_gate_t *gp)
{
#ifdef ERTS_SMP
    erts_gate_close((erts_gate_t *) gp);
#endif
}

ERTS_GLB_INLINE void
erts_smp_gate_let_through(erts_smp_gate_t *gp, unsigned no)
{
#ifdef ERTS_SMP
    erts_gate_let_through((erts_gate_t *) gp, no);
#endif
}

ERTS_GLB_INLINE void
erts_smp_gate_wait(erts_smp_gate_t *gp)
{
#ifdef ERTS_SMP
    erts_gate_wait((erts_gate_t *) gp);
#endif
}

ERTS_GLB_INLINE void
erts_smp_gate_swait(erts_smp_gate_t *gp, int spincount)
{
#ifdef ERTS_SMP
    erts_gate_swait((erts_gate_t *) gp, spincount);
#endif
}

#ifdef ERTS_THR_HAVE_SIG_FUNCS
#define ERTS_SMP_THR_HAVE_SIG_FUNCS 1

ERTS_GLB_INLINE void
erts_smp_thr_sigmask(int how, const sigset_t *set, sigset_t *oset)
{
#ifdef ERTS_SMP
    erts_thr_sigmask(how, set, oset);
#endif
}

ERTS_GLB_INLINE void
erts_smp_thr_sigwait(const sigset_t *set, int *sig)
{
#ifdef ERTS_SMP
    erts_thr_sigwait(set, sig);
#endif
}

#endif /* #ifdef ERTS_THR_HAVE_SIG_FUNCS */

#endif /* #if ERTS_GLB_INLINE_INCL_FUNC_DEF */

#endif /* ERL_SMP_H */
