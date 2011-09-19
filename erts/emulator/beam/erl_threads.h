/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2001-2011. All Rights Reserved.
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

#define ERTS_SPIN_BODY ETHR_SPIN_BODY

#include "sys.h"

typedef struct { SWord sint[2]; } erts_no_dw_atomic_t;
typedef SWord erts_no_atomic_t;
typedef Sint32 erts_no_atomic32_t;

#ifdef USE_THREADS

#define ETHR_TRY_INLINE_FUNCS
#include "ethread.h"
#include "erl_lock_check.h"
#include "erl_lock_count.h"
#include "erl_term.h"

#if defined(__GLIBC__) && (__GLIBC__ << 16) + __GLIBC_MINOR__ < (2 << 16) + 4
/*
 * pthread_mutex_destroy() may return EBUSY when it shouldn't :( We have
 * only seen this bug in glibc versions before 2.4. Note that condition
 * variables, rwmutexes, spinlocks, and rwspinlocks also may be effected by
 * this bug since these implementations may use mutexes internally.
 */
#  define ERTS_THR_HAVE_BUSY_DESTROY_BUG
#endif

#define ERTS_THR_MEMORY_BARRIER ETHR_MEMORY_BARRIER

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
typedef ethr_late_init_data erts_thr_late_init_data_t;
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

#define ERTS_RWMTX_OPT_DEFAULT_INITER ETHR_RWMUTEX_OPT_DEFAULT_INITER
#define ERTS_RWMTX_TYPE_NORMAL ETHR_RWMUTEX_TYPE_NORMAL
#define ERTS_RWMTX_TYPE_FREQUENT_READ ETHR_RWMUTEX_TYPE_FREQUENT_READ
#define ERTS_RWMTX_TYPE_EXTREMELY_FREQUENT_READ \
  ETHR_RWMUTEX_TYPE_EXTREMELY_FREQUENT_READ
#define ERTS_RWMTX_LONG_LIVED ETHR_RWMUTEX_LONG_LIVED
#define ERTS_RWMTX_SHORT_LIVED ETHR_RWMUTEX_SHORT_LIVED
#define ERTS_RWMTX_UNKNOWN_LIVED ETHR_RWMUTEX_UNKNOWN_LIVED
typedef ethr_rwmutex_opt erts_rwmtx_opt_t;

typedef ethr_tsd_key erts_tsd_key_t;
typedef ethr_ts_event erts_tse_t;
#define erts_dw_aint_t ethr_dw_sint_t
#define erts_dw_atomic_t ethr_dw_atomic_t
#define erts_aint_t ethr_sint_t
#define erts_atomic_t ethr_atomic_t
#define erts_aint32_t ethr_sint32_t 
#define erts_atomic32_t ethr_atomic32_t

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

__decl_noreturn void  __noreturn erts_thr_fatal_error(int, char *); 
                                 /* implemented in erl_init.c */

#define ERTS_THR_INIT_DATA_DEF_INITER	ETHR_INIT_DATA_DEFAULT_INITER
#define ERTS_THR_LATE_INIT_DATA_DEF_INITER \
                                        ETHR_LATE_INIT_DATA_DEFAULT_INITER

#ifdef ETHR_HAVE_ETHR_REC_MUTEX_INIT
#  define ERTS_HAVE_REC_MTX_INIT	ETHR_HAVE_ETHR_REC_MUTEX_INIT
#endif

#else /* #ifdef USE_THREADS */

#define ERTS_THR_MEMORY_BARRIER

#define ERTS_THR_OPTS_DEFAULT_INITER 0
typedef int erts_thr_opts_t;
typedef int erts_thr_init_data_t;
typedef int erts_thr_late_init_data_t;
typedef int erts_tid_t;
typedef int erts_mtx_t;
typedef int erts_cnd_t;
#define ERTS_RWMTX_OPT_DEFAULT_INITER {0}
#define ERTS_RWMTX_TYPE_NORMAL 0
#define ERTS_RWMTX_TYPE_FREQUENT_READ 0
#define ERTS_RWMTX_TYPE_EXTREMELY_FREQUENT_READ 0
#define ERTS_RWMTX_LONG_LIVED 0
#define ERTS_RWMTX_SHORT_LIVED 0
#define ERTS_RWMTX_UNKNOWN_LIVED 0
typedef struct {
    char type;
    char lived;
    int main_spincount;
    int aux_spincount;
} erts_rwmtx_opt_t;
typedef int erts_rwmtx_t;
typedef int erts_tsd_key_t;
typedef int erts_tse_t;
#define erts_dw_aint_t erts_no_dw_atomic_t
#define erts_dw_atomic_t erts_no_dw_atomic_t
#define erts_aint_t SWord
#define erts_atomic_t erts_no_atomic_t
#define erts_aint32_t Sint32
#define erts_atomic32_t erts_no_atomic32_t
#if __GNUC__ > 2
typedef struct { } erts_spinlock_t;
typedef struct { } erts_rwlock_t;
#else
typedef struct { int gcc_is_buggy; } erts_spinlock_t;
typedef struct { int gcc_is_buggy; } erts_rwlock_t;
#endif

#define ERTS_MTX_INITER			0
#define ERTS_CND_INITER			0
#define ERTS_THR_INIT_DATA_DEF_INITER	0

#define ERTS_HAVE_REC_MTX_INIT		1

#endif /* #ifdef USE_THREADS */

#define ERTS_AINT_T_MAX (~(((erts_aint_t) 1) << (sizeof(erts_aint_t)*8-1)))
#define ERTS_AINT_T_MIN ((((erts_aint_t) 1) << (sizeof(erts_aint_t)*8-1)))
#define ERTS_AINT32_T_MAX (~(((erts_aint32_t) 1) << (sizeof(erts_aint32_t)*8-1)))
#define ERTS_AINT32_T_MIN ((((erts_aint32_t) 1) << (sizeof(erts_aint32_t)*8-1)))

ERTS_GLB_INLINE void erts_thr_init(erts_thr_init_data_t *id);
ERTS_GLB_INLINE void erts_thr_late_init(erts_thr_late_init_data_t *id);
ERTS_GLB_INLINE void erts_thr_create(erts_tid_t *tid, void * (*func)(void *),
				     void *arg, erts_thr_opts_t *opts);
ERTS_GLB_INLINE void erts_thr_join(erts_tid_t tid, void **thr_res);
ERTS_GLB_INLINE void erts_thr_detach(erts_tid_t tid);
ERTS_GLB_INLINE void erts_thr_exit(void *res);
ERTS_GLB_INLINE void erts_thr_install_exit_handler(void (*exit_handler)(void));
ERTS_GLB_INLINE erts_tid_t erts_thr_self(void);
ERTS_GLB_INLINE int erts_equal_tids(erts_tid_t x, erts_tid_t y);
ERTS_GLB_INLINE void erts_mtx_init_x(erts_mtx_t *mtx, char *name, Eterm extra);
ERTS_GLB_INLINE void erts_mtx_init_x_opt(erts_mtx_t *mtx, char *name, Eterm extra, Uint16 opt);
ERTS_GLB_INLINE void erts_mtx_init_locked_x(erts_mtx_t *mtx,
					    char *name,
					    Eterm extra);
ERTS_GLB_INLINE void erts_mtx_init(erts_mtx_t *mtx, char *name);
ERTS_GLB_INLINE void erts_mtx_init_locked(erts_mtx_t *mtx, char *name);
ERTS_GLB_INLINE void erts_mtx_destroy(erts_mtx_t *mtx);
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
ERTS_GLB_INLINE void erts_rwmtx_set_reader_group(int no);
ERTS_GLB_INLINE void erts_rwmtx_init_opt_x(erts_rwmtx_t *rwmtx,
					   erts_rwmtx_opt_t *opt,
					   char *name,
					   Eterm extra);
ERTS_GLB_INLINE void erts_rwmtx_init_x(erts_rwmtx_t *rwmtx,
				       char *name,
				       Eterm extra);
ERTS_GLB_INLINE void erts_rwmtx_init_opt(erts_rwmtx_t *rwmtx,
					 erts_rwmtx_opt_t *opt,
					 char *name);
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

ERTS_GLB_INLINE void erts_no_dw_atomic_set(erts_no_dw_atomic_t *var, erts_no_dw_atomic_t *val);
ERTS_GLB_INLINE void erts_no_dw_atomic_read(erts_no_dw_atomic_t *var, erts_no_dw_atomic_t *val);
ERTS_GLB_INLINE int erts_no_dw_atomic_cmpxchg(erts_no_dw_atomic_t *var,
					      erts_no_dw_atomic_t *val,
					      erts_no_dw_atomic_t *old_val);
ERTS_GLB_INLINE void erts_no_atomic_set(erts_no_atomic_t *var, erts_aint_t i);
ERTS_GLB_INLINE erts_aint_t erts_no_atomic_read(erts_no_atomic_t *var);
ERTS_GLB_INLINE erts_aint_t erts_no_atomic_inc_read(erts_no_atomic_t *incp);
ERTS_GLB_INLINE erts_aint_t erts_no_atomic_dec_read(erts_no_atomic_t *decp);
ERTS_GLB_INLINE void erts_no_atomic_inc(erts_no_atomic_t *incp);
ERTS_GLB_INLINE void erts_no_atomic_dec(erts_no_atomic_t *decp);
ERTS_GLB_INLINE erts_aint_t erts_no_atomic_add_read(erts_no_atomic_t *addp,
						    erts_aint_t i);
ERTS_GLB_INLINE void erts_no_atomic_add(erts_no_atomic_t *addp, erts_aint_t i);
ERTS_GLB_INLINE erts_aint_t erts_no_atomic_read_bor(erts_no_atomic_t *var,
						    erts_aint_t mask);
ERTS_GLB_INLINE erts_aint_t erts_no_atomic_read_band(erts_no_atomic_t *var,
						     erts_aint_t mask);
ERTS_GLB_INLINE erts_aint_t erts_no_atomic_xchg(erts_no_atomic_t *xchgp,
						erts_aint_t new);
ERTS_GLB_INLINE erts_aint_t erts_no_atomic_cmpxchg(erts_no_atomic_t *xchgp,
						   erts_aint_t new,
						   erts_aint_t expected);
ERTS_GLB_INLINE void erts_no_atomic32_set(erts_no_atomic32_t *var,
					  erts_aint32_t i);
ERTS_GLB_INLINE erts_aint32_t erts_no_atomic32_read(erts_no_atomic32_t *var);
ERTS_GLB_INLINE erts_aint32_t erts_no_atomic32_inc_read(erts_no_atomic32_t *incp);
ERTS_GLB_INLINE erts_aint32_t erts_no_atomic32_dec_read(erts_no_atomic32_t *decp);
ERTS_GLB_INLINE void erts_no_atomic32_inc(erts_no_atomic32_t *incp);
ERTS_GLB_INLINE void erts_no_atomic32_dec(erts_no_atomic32_t *decp);
ERTS_GLB_INLINE erts_aint32_t erts_no_atomic32_add_read(erts_no_atomic32_t *addp,
							erts_aint32_t i);
ERTS_GLB_INLINE void erts_no_atomic32_add(erts_no_atomic32_t *addp,
					  erts_aint32_t i);
ERTS_GLB_INLINE erts_aint32_t erts_no_atomic32_read_bor(erts_no_atomic32_t *var,
							erts_aint32_t mask);
ERTS_GLB_INLINE erts_aint32_t erts_no_atomic32_read_band(erts_no_atomic32_t *var,
							 erts_aint32_t mask);
ERTS_GLB_INLINE erts_aint32_t erts_no_atomic32_xchg(erts_no_atomic32_t *xchgp,
						    erts_aint32_t new);
ERTS_GLB_INLINE erts_aint32_t erts_no_atomic32_cmpxchg(erts_no_atomic32_t *xchgp,
						       erts_aint32_t new,
						       erts_aint32_t expected);

ERTS_GLB_INLINE void erts_spinlock_init_x_opt(erts_spinlock_t *lock,
					      char *name,
					      Eterm extra,
					      Uint16 opt);
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
ERTS_GLB_INLINE void erts_tsd_key_create(erts_tsd_key_t *keyp);
ERTS_GLB_INLINE void erts_tsd_key_delete(erts_tsd_key_t key);
ERTS_GLB_INLINE void erts_tsd_set(erts_tsd_key_t key, void *value);
ERTS_GLB_INLINE void * erts_tsd_get(erts_tsd_key_t key);
ERTS_GLB_INLINE erts_tse_t *erts_tse_fetch(void);
ERTS_GLB_INLINE void erts_tse_return(erts_tse_t *ep);
ERTS_GLB_INLINE void erts_tse_set(erts_tse_t *ep);
ERTS_GLB_INLINE void erts_tse_reset(erts_tse_t *ep);
ERTS_GLB_INLINE int erts_tse_wait(erts_tse_t *ep);
ERTS_GLB_INLINE int erts_tse_swait(erts_tse_t *ep, int spincount);
ERTS_GLB_INLINE int erts_tse_is_tmp(erts_tse_t *ep);
ERTS_GLB_INLINE void erts_thr_set_main_status(int, int);
ERTS_GLB_INLINE int erts_thr_get_main_status(void);
ERTS_GLB_INLINE void erts_thr_yield(void);

#ifdef ETHR_HAVE_ETHR_SIG_FUNCS
#define ERTS_THR_HAVE_SIG_FUNCS 1
ERTS_GLB_INLINE void erts_thr_sigmask(int how, const sigset_t *set,
				      sigset_t *oset);
ERTS_GLB_INLINE void erts_thr_sigwait(const sigset_t *set, int *sig);
#endif /* #ifdef HAVE_ETHR_SIG_FUNCS */

/*
 * Functions implementing atomic operations with with no (nob),
 * full (mb), acquire (acqb), release (relb), read (rb), and
 * write (wb) memory barriers.
 *
 * If thread support has been disabled, they are mapped to
 * functions that performs the same operation, but aren't atomic
 * and don't imply memory barriers.
 */

#ifdef USE_THREADS

/* Double word size atomics */

#define erts_dw_atomic_init_nob ethr_dw_atomic_init
#define erts_dw_atomic_set_nob ethr_dw_atomic_set
#define erts_dw_atomic_read_nob ethr_dw_atomic_read
#define erts_dw_atomic_cmpxchg_nob ethr_dw_atomic_cmpxchg

#define erts_dw_atomic_init_mb ethr_dw_atomic_init_mb
#define erts_dw_atomic_set_mb ethr_dw_atomic_set_mb
#define erts_dw_atomic_read_mb ethr_dw_atomic_read_mb
#define erts_dw_atomic_cmpxchg_mb ethr_dw_atomic_cmpxchg_mb

#define erts_dw_atomic_init_acqb ethr_dw_atomic_init_acqb
#define erts_dw_atomic_set_acqb ethr_dw_atomic_set_acqb
#define erts_dw_atomic_read_acqb ethr_dw_atomic_read_acqb
#define erts_dw_atomic_cmpxchg_acqb ethr_dw_atomic_cmpxchg_acqb

#define erts_dw_atomic_init_relb ethr_dw_atomic_init_relb
#define erts_dw_atomic_set_relb ethr_dw_atomic_set_relb
#define erts_dw_atomic_read_relb ethr_dw_atomic_read_relb
#define erts_dw_atomic_cmpxchg_relb ethr_dw_atomic_cmpxchg_relb

#define erts_dw_atomic_init_rb ethr_dw_atomic_init_rb
#define erts_dw_atomic_set_rb ethr_dw_atomic_set_rb
#define erts_dw_atomic_read_rb ethr_dw_atomic_read_rb
#define erts_dw_atomic_cmpxchg_rb ethr_dw_atomic_cmpxchg_rb

#define erts_dw_atomic_init_wb ethr_dw_atomic_init_wb
#define erts_dw_atomic_set_wb ethr_dw_atomic_set_wb
#define erts_dw_atomic_read_wb ethr_dw_atomic_read_wb
#define erts_dw_atomic_cmpxchg_wb ethr_dw_atomic_cmpxchg_wb

/* Word size atomics */

#define erts_atomic_init_nob ethr_atomic_init
#define erts_atomic_set_nob ethr_atomic_set
#define erts_atomic_read_nob ethr_atomic_read
#define erts_atomic_inc_read_nob ethr_atomic_inc_read
#define erts_atomic_dec_read_nob ethr_atomic_dec_read
#define erts_atomic_inc_nob ethr_atomic_inc
#define erts_atomic_dec_nob ethr_atomic_dec
#define erts_atomic_add_read_nob ethr_atomic_add_read
#define erts_atomic_add_nob ethr_atomic_add
#define erts_atomic_read_bor_nob ethr_atomic_read_bor
#define erts_atomic_read_band_nob ethr_atomic_read_band
#define erts_atomic_xchg_nob ethr_atomic_xchg
#define erts_atomic_cmpxchg_nob ethr_atomic_cmpxchg

#define erts_atomic_init_mb ethr_atomic_init_mb
#define erts_atomic_set_mb ethr_atomic_set_mb
#define erts_atomic_read_mb ethr_atomic_read_mb
#define erts_atomic_inc_read_mb ethr_atomic_inc_read_mb
#define erts_atomic_dec_read_mb ethr_atomic_dec_read_mb
#define erts_atomic_inc_mb ethr_atomic_inc_mb
#define erts_atomic_dec_mb ethr_atomic_dec_mb
#define erts_atomic_add_read_mb ethr_atomic_add_read_mb
#define erts_atomic_add_mb ethr_atomic_add_mb
#define erts_atomic_read_bor_mb ethr_atomic_read_bor_mb
#define erts_atomic_read_band_mb ethr_atomic_read_band_mb
#define erts_atomic_xchg_mb ethr_atomic_xchg_mb
#define erts_atomic_cmpxchg_mb ethr_atomic_cmpxchg_mb

#define erts_atomic_init_acqb ethr_atomic_init_acqb
#define erts_atomic_set_acqb ethr_atomic_set_acqb
#define erts_atomic_read_acqb ethr_atomic_read_acqb
#define erts_atomic_inc_read_acqb ethr_atomic_inc_read_acqb
#define erts_atomic_dec_read_acqb ethr_atomic_dec_read_acqb
#define erts_atomic_inc_acqb ethr_atomic_inc_acqb
#define erts_atomic_dec_acqb ethr_atomic_dec_acqb
#define erts_atomic_add_read_acqb ethr_atomic_add_read_acqb
#define erts_atomic_add_acqb ethr_atomic_add_acqb
#define erts_atomic_read_bor_acqb ethr_atomic_read_bor_acqb
#define erts_atomic_read_band_acqb ethr_atomic_read_band_acqb
#define erts_atomic_xchg_acqb ethr_atomic_xchg_acqb
#define erts_atomic_cmpxchg_acqb ethr_atomic_cmpxchg_acqb

#define erts_atomic_init_relb ethr_atomic_init_relb
#define erts_atomic_set_relb ethr_atomic_set_relb
#define erts_atomic_read_relb ethr_atomic_read_relb
#define erts_atomic_inc_read_relb ethr_atomic_inc_read_relb
#define erts_atomic_dec_read_relb ethr_atomic_dec_read_relb
#define erts_atomic_inc_relb ethr_atomic_inc_relb
#define erts_atomic_dec_relb ethr_atomic_dec_relb
#define erts_atomic_add_read_relb ethr_atomic_add_read_relb
#define erts_atomic_add_relb ethr_atomic_add_relb
#define erts_atomic_read_bor_relb ethr_atomic_read_bor_relb
#define erts_atomic_read_band_relb ethr_atomic_read_band_relb
#define erts_atomic_xchg_relb ethr_atomic_xchg_relb
#define erts_atomic_cmpxchg_relb ethr_atomic_cmpxchg_relb

#define erts_atomic_init_rb ethr_atomic_init_rb
#define erts_atomic_set_rb ethr_atomic_set_rb
#define erts_atomic_read_rb ethr_atomic_read_rb
#define erts_atomic_inc_read_rb ethr_atomic_inc_read_rb
#define erts_atomic_dec_read_rb ethr_atomic_dec_read_rb
#define erts_atomic_inc_rb ethr_atomic_inc_rb
#define erts_atomic_dec_rb ethr_atomic_dec_rb
#define erts_atomic_add_read_rb ethr_atomic_add_read_rb
#define erts_atomic_add_rb ethr_atomic_add_rb
#define erts_atomic_read_bor_rb ethr_atomic_read_bor_rb
#define erts_atomic_read_band_rb ethr_atomic_read_band_rb
#define erts_atomic_xchg_rb ethr_atomic_xchg_rb
#define erts_atomic_cmpxchg_rb ethr_atomic_cmpxchg_rb

#define erts_atomic_init_wb ethr_atomic_init_wb
#define erts_atomic_set_wb ethr_atomic_set_wb
#define erts_atomic_read_wb ethr_atomic_read_wb
#define erts_atomic_inc_read_wb ethr_atomic_inc_read_wb
#define erts_atomic_dec_read_wb ethr_atomic_dec_read_wb
#define erts_atomic_inc_wb ethr_atomic_inc_wb
#define erts_atomic_dec_wb ethr_atomic_dec_wb
#define erts_atomic_add_read_wb ethr_atomic_add_read_wb
#define erts_atomic_add_wb ethr_atomic_add_wb
#define erts_atomic_read_bor_wb ethr_atomic_read_bor_wb
#define erts_atomic_read_band_wb ethr_atomic_read_band_wb
#define erts_atomic_xchg_wb ethr_atomic_xchg_wb
#define erts_atomic_cmpxchg_wb ethr_atomic_cmpxchg_wb

/* 32-bit atomics */

#define erts_atomic32_init_nob ethr_atomic32_init
#define erts_atomic32_set_nob ethr_atomic32_set
#define erts_atomic32_read_nob ethr_atomic32_read
#define erts_atomic32_inc_read_nob ethr_atomic32_inc_read
#define erts_atomic32_dec_read_nob ethr_atomic32_dec_read
#define erts_atomic32_inc_nob ethr_atomic32_inc
#define erts_atomic32_dec_nob ethr_atomic32_dec
#define erts_atomic32_add_read_nob ethr_atomic32_add_read
#define erts_atomic32_add_nob ethr_atomic32_add
#define erts_atomic32_read_bor_nob ethr_atomic32_read_bor
#define erts_atomic32_read_band_nob ethr_atomic32_read_band
#define erts_atomic32_xchg_nob ethr_atomic32_xchg
#define erts_atomic32_cmpxchg_nob ethr_atomic32_cmpxchg

#define erts_atomic32_init_mb ethr_atomic32_init_mb
#define erts_atomic32_set_mb ethr_atomic32_set_mb
#define erts_atomic32_read_mb ethr_atomic32_read_mb
#define erts_atomic32_inc_read_mb ethr_atomic32_inc_read_mb
#define erts_atomic32_dec_read_mb ethr_atomic32_dec_read_mb
#define erts_atomic32_inc_mb ethr_atomic32_inc_mb
#define erts_atomic32_dec_mb ethr_atomic32_dec_mb
#define erts_atomic32_add_read_mb ethr_atomic32_add_read_mb
#define erts_atomic32_add_mb ethr_atomic32_add_mb
#define erts_atomic32_read_bor_mb ethr_atomic32_read_bor_mb
#define erts_atomic32_read_band_mb ethr_atomic32_read_band_mb
#define erts_atomic32_xchg_mb ethr_atomic32_xchg_mb
#define erts_atomic32_cmpxchg_mb ethr_atomic32_cmpxchg_mb

#define erts_atomic32_init_acqb ethr_atomic32_init_acqb
#define erts_atomic32_set_acqb ethr_atomic32_set_acqb
#define erts_atomic32_read_acqb ethr_atomic32_read_acqb
#define erts_atomic32_inc_read_acqb ethr_atomic32_inc_read_acqb
#define erts_atomic32_dec_read_acqb ethr_atomic32_dec_read_acqb
#define erts_atomic32_inc_acqb ethr_atomic32_inc_acqb
#define erts_atomic32_dec_acqb ethr_atomic32_dec_acqb
#define erts_atomic32_add_read_acqb ethr_atomic32_add_read_acqb
#define erts_atomic32_add_acqb ethr_atomic32_add_acqb
#define erts_atomic32_read_bor_acqb ethr_atomic32_read_bor_acqb
#define erts_atomic32_read_band_acqb ethr_atomic32_read_band_acqb
#define erts_atomic32_xchg_acqb ethr_atomic32_xchg_acqb
#define erts_atomic32_cmpxchg_acqb ethr_atomic32_cmpxchg_acqb

#define erts_atomic32_init_relb ethr_atomic32_init_relb
#define erts_atomic32_set_relb ethr_atomic32_set_relb
#define erts_atomic32_read_relb ethr_atomic32_read_relb
#define erts_atomic32_inc_read_relb ethr_atomic32_inc_read_relb
#define erts_atomic32_dec_read_relb ethr_atomic32_dec_read_relb
#define erts_atomic32_inc_relb ethr_atomic32_inc_relb
#define erts_atomic32_dec_relb ethr_atomic32_dec_relb
#define erts_atomic32_add_read_relb ethr_atomic32_add_read_relb
#define erts_atomic32_add_relb ethr_atomic32_add_relb
#define erts_atomic32_read_bor_relb ethr_atomic32_read_bor_relb
#define erts_atomic32_read_band_relb ethr_atomic32_read_band_relb
#define erts_atomic32_xchg_relb ethr_atomic32_xchg_relb
#define erts_atomic32_cmpxchg_relb ethr_atomic32_cmpxchg_relb

#define erts_atomic32_init_rb ethr_atomic32_init_rb
#define erts_atomic32_set_rb ethr_atomic32_set_rb
#define erts_atomic32_read_rb ethr_atomic32_read_rb
#define erts_atomic32_inc_read_rb ethr_atomic32_inc_read_rb
#define erts_atomic32_dec_read_rb ethr_atomic32_dec_read_rb
#define erts_atomic32_inc_rb ethr_atomic32_inc_rb
#define erts_atomic32_dec_rb ethr_atomic32_dec_rb
#define erts_atomic32_add_read_rb ethr_atomic32_add_read_rb
#define erts_atomic32_add_rb ethr_atomic32_add_rb
#define erts_atomic32_read_bor_rb ethr_atomic32_read_bor_rb
#define erts_atomic32_read_band_rb ethr_atomic32_read_band_rb
#define erts_atomic32_xchg_rb ethr_atomic32_xchg_rb
#define erts_atomic32_cmpxchg_rb ethr_atomic32_cmpxchg_rb

#define erts_atomic32_init_wb ethr_atomic32_init_wb
#define erts_atomic32_set_wb ethr_atomic32_set_wb
#define erts_atomic32_read_wb ethr_atomic32_read_wb
#define erts_atomic32_inc_read_wb ethr_atomic32_inc_read_wb
#define erts_atomic32_dec_read_wb ethr_atomic32_dec_read_wb
#define erts_atomic32_inc_wb ethr_atomic32_inc_wb
#define erts_atomic32_dec_wb ethr_atomic32_dec_wb
#define erts_atomic32_add_read_wb ethr_atomic32_add_read_wb
#define erts_atomic32_add_wb ethr_atomic32_add_wb
#define erts_atomic32_read_bor_wb ethr_atomic32_read_bor_wb
#define erts_atomic32_read_band_wb ethr_atomic32_read_band_wb
#define erts_atomic32_xchg_wb ethr_atomic32_xchg_wb
#define erts_atomic32_cmpxchg_wb ethr_atomic32_cmpxchg_wb

#else /* !USE_THREADS */

/* Double word size atomics */

#define erts_dw_atomic_init_nob erts_no_dw_atomic_set
#define erts_dw_atomic_set_nob erts_no_dw_atomic_set
#define erts_dw_atomic_read_nob erts_no_dw_atomic_read
#define erts_dw_atomic_cmpxchg_nob erts_no_dw_atomic_cmpxchg

#define erts_dw_atomic_init_mb erts_no_dw_atomic_init
#define erts_dw_atomic_set_mb erts_no_dw_atomic_set
#define erts_dw_atomic_read_mb erts_no_dw_atomic_read
#define erts_dw_atomic_cmpxchg_mb erts_no_dw_atomic_cmpxchg

#define erts_dw_atomic_init_acqb erts_no_dw_atomic_init
#define erts_dw_atomic_set_acqb erts_no_dw_atomic_set
#define erts_dw_atomic_read_acqb erts_no_dw_atomic_read
#define erts_dw_atomic_cmpxchg_acqb erts_no_dw_atomic_cmpxchg

#define erts_dw_atomic_init_relb erts_no_dw_atomic_init
#define erts_dw_atomic_set_relb erts_no_dw_atomic_set
#define erts_dw_atomic_read_relb erts_no_dw_atomic_read
#define erts_dw_atomic_cmpxchg_relb erts_no_dw_atomic_cmpxchg

#define erts_dw_atomic_init_rb erts_no_dw_atomic_init
#define erts_dw_atomic_set_rb erts_no_dw_atomic_set
#define erts_dw_atomic_read_rb erts_no_dw_atomic_read
#define erts_dw_atomic_cmpxchg_rb erts_no_dw_atomic_cmpxchg

#define erts_dw_atomic_init_wb erts_no_dw_atomic_init
#define erts_dw_atomic_set_wb erts_no_dw_atomic_set
#define erts_dw_atomic_read_wb erts_no_dw_atomic_read
#define erts_dw_atomic_cmpxchg_wb erts_no_dw_atomic_cmpxchg

/* Word size atomics */

#define erts_atomic_init_nob erts_no_atomic_set
#define erts_atomic_set_nob erts_no_atomic_set
#define erts_atomic_read_nob erts_no_atomic_read
#define erts_atomic_inc_read_nob erts_no_atomic_inc_read
#define erts_atomic_dec_read_nob erts_no_atomic_dec_read
#define erts_atomic_inc_nob erts_no_atomic_inc
#define erts_atomic_dec_nob erts_no_atomic_dec
#define erts_atomic_add_read_nob erts_no_atomic_add_read
#define erts_atomic_add_nob erts_no_atomic_add
#define erts_atomic_read_bor_nob erts_no_atomic_read_bor
#define erts_atomic_read_band_nob erts_no_atomic_read_band
#define erts_atomic_xchg_nob erts_no_atomic_xchg
#define erts_atomic_cmpxchg_nob erts_no_atomic_cmpxchg

#define erts_atomic_init_mb erts_no_atomic_set
#define erts_atomic_set_mb erts_no_atomic_set
#define erts_atomic_read_mb erts_no_atomic_read
#define erts_atomic_inc_read_mb erts_no_atomic_inc_read
#define erts_atomic_dec_read_mb erts_no_atomic_dec_read
#define erts_atomic_inc_mb erts_no_atomic_inc
#define erts_atomic_dec_mb erts_no_atomic_dec
#define erts_atomic_add_read_mb erts_no_atomic_add_read
#define erts_atomic_add_mb erts_no_atomic_add
#define erts_atomic_read_bor_mb erts_no_atomic_read_bor
#define erts_atomic_read_band_mb erts_no_atomic_read_band
#define erts_atomic_xchg_mb erts_no_atomic_xchg
#define erts_atomic_cmpxchg_mb erts_no_atomic_cmpxchg

#define erts_atomic_init_acqb erts_no_atomic_set
#define erts_atomic_set_acqb erts_no_atomic_set
#define erts_atomic_read_acqb erts_no_atomic_read
#define erts_atomic_inc_read_acqb erts_no_atomic_inc_read
#define erts_atomic_dec_read_acqb erts_no_atomic_dec_read
#define erts_atomic_inc_acqb erts_no_atomic_inc
#define erts_atomic_dec_acqb erts_no_atomic_dec
#define erts_atomic_add_read_acqb erts_no_atomic_add_read
#define erts_atomic_add_acqb erts_no_atomic_add
#define erts_atomic_read_bor_acqb erts_no_atomic_read_bor
#define erts_atomic_read_band_acqb erts_no_atomic_read_band
#define erts_atomic_xchg_acqb erts_no_atomic_xchg
#define erts_atomic_cmpxchg_acqb erts_no_atomic_cmpxchg

#define erts_atomic_init_relb erts_no_atomic_set
#define erts_atomic_set_relb erts_no_atomic_set
#define erts_atomic_read_relb erts_no_atomic_read
#define erts_atomic_inc_read_relb erts_no_atomic_inc_read
#define erts_atomic_dec_read_relb erts_no_atomic_dec_read
#define erts_atomic_inc_relb erts_no_atomic_inc
#define erts_atomic_dec_relb erts_no_atomic_dec
#define erts_atomic_add_read_relb erts_no_atomic_add_read
#define erts_atomic_add_relb erts_no_atomic_add
#define erts_atomic_read_bor_relb erts_no_atomic_read_bor
#define erts_atomic_read_band_relb erts_no_atomic_read_band
#define erts_atomic_xchg_relb erts_no_atomic_xchg
#define erts_atomic_cmpxchg_relb erts_no_atomic_cmpxchg

#define erts_atomic_init_rb erts_no_atomic_set
#define erts_atomic_set_rb erts_no_atomic_set
#define erts_atomic_read_rb erts_no_atomic_read
#define erts_atomic_inc_read_rb erts_no_atomic_inc_read
#define erts_atomic_dec_read_rb erts_no_atomic_dec_read
#define erts_atomic_inc_rb erts_no_atomic_inc
#define erts_atomic_dec_rb erts_no_atomic_dec
#define erts_atomic_add_read_rb erts_no_atomic_add_read
#define erts_atomic_add_rb erts_no_atomic_add
#define erts_atomic_read_bor_rb erts_no_atomic_read_bor
#define erts_atomic_read_band_rb erts_no_atomic_read_band
#define erts_atomic_xchg_rb erts_no_atomic_xchg
#define erts_atomic_cmpxchg_rb erts_no_atomic_cmpxchg

#define erts_atomic_init_wb erts_no_atomic_set
#define erts_atomic_set_wb erts_no_atomic_set
#define erts_atomic_read_wb erts_no_atomic_read
#define erts_atomic_inc_read_wb erts_no_atomic_inc_read
#define erts_atomic_dec_read_wb erts_no_atomic_dec_read
#define erts_atomic_inc_wb erts_no_atomic_inc
#define erts_atomic_dec_wb erts_no_atomic_dec
#define erts_atomic_add_read_wb erts_no_atomic_add_read
#define erts_atomic_add_wb erts_no_atomic_add
#define erts_atomic_read_bor_wb erts_no_atomic_read_bor
#define erts_atomic_read_band_wb erts_no_atomic_read_band
#define erts_atomic_xchg_wb erts_no_atomic_xchg
#define erts_atomic_cmpxchg_wb erts_no_atomic_cmpxchg

/* 32-bit atomics */

#define erts_atomic32_init_nob erts_no_atomic32_set
#define erts_atomic32_set_nob erts_no_atomic32_set
#define erts_atomic32_read_nob erts_no_atomic32_read
#define erts_atomic32_inc_read_nob erts_no_atomic32_inc_read
#define erts_atomic32_dec_read_nob erts_no_atomic32_dec_read
#define erts_atomic32_inc_nob erts_no_atomic32_inc
#define erts_atomic32_dec_nob erts_no_atomic32_dec
#define erts_atomic32_add_read_nob erts_no_atomic32_add_read
#define erts_atomic32_add_nob erts_no_atomic32_add
#define erts_atomic32_read_bor_nob erts_no_atomic32_read_bor
#define erts_atomic32_read_band_nob erts_no_atomic32_read_band
#define erts_atomic32_xchg_nob erts_no_atomic32_xchg
#define erts_atomic32_cmpxchg_nob erts_no_atomic32_cmpxchg

#define erts_atomic32_init_mb erts_no_atomic32_set
#define erts_atomic32_set_mb erts_no_atomic32_set
#define erts_atomic32_read_mb erts_no_atomic32_read
#define erts_atomic32_inc_read_mb erts_no_atomic32_inc_read
#define erts_atomic32_dec_read_mb erts_no_atomic32_dec_read
#define erts_atomic32_inc_mb erts_no_atomic32_inc
#define erts_atomic32_dec_mb erts_no_atomic32_dec
#define erts_atomic32_add_read_mb erts_no_atomic32_add_read
#define erts_atomic32_add_mb erts_no_atomic32_add
#define erts_atomic32_read_bor_mb erts_no_atomic32_read_bor
#define erts_atomic32_read_band_mb erts_no_atomic32_read_band
#define erts_atomic32_xchg_mb erts_no_atomic32_xchg
#define erts_atomic32_cmpxchg_mb erts_no_atomic32_cmpxchg

#define erts_atomic32_init_acqb erts_no_atomic32_set
#define erts_atomic32_set_acqb erts_no_atomic32_set
#define erts_atomic32_read_acqb erts_no_atomic32_read
#define erts_atomic32_inc_read_acqb erts_no_atomic32_inc_read
#define erts_atomic32_dec_read_acqb erts_no_atomic32_dec_read
#define erts_atomic32_inc_acqb erts_no_atomic32_inc
#define erts_atomic32_dec_acqb erts_no_atomic32_dec
#define erts_atomic32_add_read_acqb erts_no_atomic32_add_read
#define erts_atomic32_add_acqb erts_no_atomic32_add
#define erts_atomic32_read_bor_acqb erts_no_atomic32_read_bor
#define erts_atomic32_read_band_acqb erts_no_atomic32_read_band
#define erts_atomic32_xchg_acqb erts_no_atomic32_xchg
#define erts_atomic32_cmpxchg_acqb erts_no_atomic32_cmpxchg

#define erts_atomic32_init_relb erts_no_atomic32_set
#define erts_atomic32_set_relb erts_no_atomic32_set
#define erts_atomic32_read_relb erts_no_atomic32_read
#define erts_atomic32_inc_read_relb erts_no_atomic32_inc_read
#define erts_atomic32_dec_read_relb erts_no_atomic32_dec_read
#define erts_atomic32_inc_relb erts_no_atomic32_inc
#define erts_atomic32_dec_relb erts_no_atomic32_dec
#define erts_atomic32_add_read_relb erts_no_atomic32_add_read
#define erts_atomic32_add_relb erts_no_atomic32_add
#define erts_atomic32_read_bor_relb erts_no_atomic32_read_bor
#define erts_atomic32_read_band_relb erts_no_atomic32_read_band
#define erts_atomic32_xchg_relb erts_no_atomic32_xchg
#define erts_atomic32_cmpxchg_relb erts_no_atomic32_cmpxchg

#define erts_atomic32_init_rb erts_no_atomic32_set
#define erts_atomic32_set_rb erts_no_atomic32_set
#define erts_atomic32_read_rb erts_no_atomic32_read
#define erts_atomic32_inc_read_rb erts_no_atomic32_inc_read
#define erts_atomic32_dec_read_rb erts_no_atomic32_dec_read
#define erts_atomic32_inc_rb erts_no_atomic32_inc
#define erts_atomic32_dec_rb erts_no_atomic32_dec
#define erts_atomic32_add_read_rb erts_no_atomic32_add_read
#define erts_atomic32_add_rb erts_no_atomic32_add
#define erts_atomic32_read_bor_rb erts_no_atomic32_read_bor
#define erts_atomic32_read_band_rb erts_no_atomic32_read_band
#define erts_atomic32_xchg_rb erts_no_atomic32_xchg
#define erts_atomic32_cmpxchg_rb erts_no_atomic32_cmpxchg

#define erts_atomic32_init_wb erts_no_atomic32_set
#define erts_atomic32_set_wb erts_no_atomic32_set
#define erts_atomic32_read_wb erts_no_atomic32_read
#define erts_atomic32_inc_read_wb erts_no_atomic32_inc_read
#define erts_atomic32_dec_read_wb erts_no_atomic32_dec_read
#define erts_atomic32_inc_wb erts_no_atomic32_inc
#define erts_atomic32_dec_wb erts_no_atomic32_dec
#define erts_atomic32_add_read_wb erts_no_atomic32_add_read
#define erts_atomic32_add_wb erts_no_atomic32_add
#define erts_atomic32_read_bor_wb erts_no_atomic32_read_bor
#define erts_atomic32_read_band_wb erts_no_atomic32_read_band
#define erts_atomic32_xchg_wb erts_no_atomic32_xchg
#define erts_atomic32_cmpxchg_wb erts_no_atomic32_cmpxchg

#endif /* !USE_THREADS */

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
erts_thr_late_init(erts_thr_late_init_data_t *id)
{
#ifdef USE_THREADS
    int res = ethr_late_init(id);
    if (res)
	erts_thr_fatal_error(res, "complete initialization of thread library");
#endif
}

ERTS_GLB_INLINE void
erts_thr_create(erts_tid_t *tid, void * (*func)(void *), void *arg,
		erts_thr_opts_t *opts)
{
#ifdef USE_THREADS
    int res = ethr_thr_create(tid, func, arg, opts);
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
    ethr_mutex_lock(&mtx->mtx);
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
    ethr_mutex_lock(&mtx->mtx);
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
    if (res != 0) {
#ifdef ERTS_THR_HAVE_BUSY_DESTROY_BUG
	if (res == EBUSY) {
	    char *warn = "Ignoring busy mutex destroy. "
		"Most likely a bug in pthread implementation.";
	    erts_send_warning_to_logger_str_nogl(warn);
	}
	else
#endif
	    erts_thr_fatal_error(res, "destroy mutex");
    }
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
#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_lc_lock(&mtx->lc);
#endif
#ifdef ERTS_ENABLE_LOCK_COUNT
    erts_lcnt_lock(&mtx->lcnt);
#endif
    ethr_mutex_lock(&mtx->mtx);
#ifdef ERTS_ENABLE_LOCK_COUNT
    erts_lcnt_lock_post_x(&mtx->lcnt, file, line);
#endif
#endif
}

ERTS_GLB_INLINE void
erts_mtx_unlock(erts_mtx_t *mtx)
{
#ifdef USE_THREADS
#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_lc_unlock(&mtx->lc);
#endif
#ifdef ERTS_ENABLE_LOCK_COUNT
    erts_lcnt_unlock(&mtx->lcnt);
#endif
    ethr_mutex_unlock(&mtx->mtx);
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
    if (res != 0) {
#ifdef ERTS_THR_HAVE_BUSY_DESTROY_BUG
	if (res == EBUSY) {
	    char *warn = "Ignoring busy cond destroy. "
		"Most likely a bug in pthread implementation.";
	    erts_send_warning_to_logger_str_nogl(warn);
	}
	else
#endif
	    erts_thr_fatal_error(res, "destroy condition variable");
    }
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
    ethr_cond_signal(cnd);
#endif
}


ERTS_GLB_INLINE void
erts_cnd_broadcast(erts_cnd_t *cnd)
{
#ifdef USE_THREADS
    ethr_cond_broadcast(cnd);
#endif
}

/* rwmutex */

ERTS_GLB_INLINE void
erts_rwmtx_set_reader_group(int no)
{
#ifdef USE_THREADS
    int res;
#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_lc_check_no_locked_of_type(ERTS_LC_FLG_LT_RWMUTEX);
#endif
    res = ethr_rwmutex_set_reader_group(no);
    if (res != 0)
	erts_thr_fatal_error(res, "set reader group");
#endif
}

ERTS_GLB_INLINE void
erts_rwmtx_init_opt_x(erts_rwmtx_t *rwmtx,
		      erts_rwmtx_opt_t *opt,
		      char *name,
		      Eterm extra)
{
#ifdef USE_THREADS
    int res = ethr_rwmutex_init_opt(&rwmtx->rwmtx, opt);
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
erts_rwmtx_init_x(erts_rwmtx_t *rwmtx,
		  char *name,
		  Eterm extra)
{
    erts_rwmtx_init_opt_x(rwmtx, NULL, name, extra);
}

ERTS_GLB_INLINE void
erts_rwmtx_init_opt(erts_rwmtx_t *rwmtx,
		    erts_rwmtx_opt_t *opt,
		    char *name)
{
#ifdef USE_THREADS
    int res = ethr_rwmutex_init_opt(&rwmtx->rwmtx, opt);
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
erts_rwmtx_init(erts_rwmtx_t *rwmtx, char *name)
{
    erts_rwmtx_init_opt(rwmtx, NULL, name);
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
    if (res != 0) {
#ifdef ERTS_THR_HAVE_BUSY_DESTROY_BUG
	if (res == EBUSY) {
	    char *warn = "Ignoring busy rwmutex destroy. "
		"Most likely a bug in pthread implementation.";
	    erts_send_warning_to_logger_str_nogl(warn);
	}
	else
#endif
	    erts_thr_fatal_error(res, "destroy rwmutex");
    }
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
#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_lc_lock_flg(&rwmtx->lc, ERTS_LC_FLG_LO_READ);
#endif
#ifdef ERTS_ENABLE_LOCK_COUNT
    erts_lcnt_lock_opt(&rwmtx->lcnt, ERTS_LCNT_LO_READ);
#endif
    ethr_rwmutex_rlock(&rwmtx->rwmtx);
#ifdef ERTS_ENABLE_LOCK_COUNT
    erts_lcnt_lock_post_x(&rwmtx->lcnt, file, line);
#endif
#endif
}

ERTS_GLB_INLINE void
erts_rwmtx_runlock(erts_rwmtx_t *rwmtx)
{
#ifdef USE_THREADS
#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_lc_unlock_flg(&rwmtx->lc, ERTS_LC_FLG_LO_READ);
#endif
#ifdef ERTS_ENABLE_LOCK_COUNT
    erts_lcnt_unlock_opt(&rwmtx->lcnt, ERTS_LCNT_LO_READ);
#endif
    ethr_rwmutex_runlock(&rwmtx->rwmtx);
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
#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_lc_lock_flg(&rwmtx->lc, ERTS_LC_FLG_LO_READ_WRITE);
#endif
#ifdef ERTS_ENABLE_LOCK_COUNT
    erts_lcnt_lock_opt(&rwmtx->lcnt, ERTS_LCNT_LO_READ_WRITE);
#endif
    ethr_rwmutex_rwlock(&rwmtx->rwmtx);
#ifdef ERTS_ENABLE_LOCK_COUNT
    erts_lcnt_lock_post_x(&rwmtx->lcnt, file, line);
#endif
#endif
}

ERTS_GLB_INLINE void
erts_rwmtx_rwunlock(erts_rwmtx_t *rwmtx)
{
#ifdef USE_THREADS
#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_lc_unlock_flg(&rwmtx->lc, ERTS_LC_FLG_LO_READ_WRITE);
#endif
#ifdef ERTS_ENABLE_LOCK_COUNT
    erts_lcnt_unlock_opt(&rwmtx->lcnt, ERTS_LCNT_LO_READ_WRITE);
#endif
    ethr_rwmutex_rwunlock(&rwmtx->rwmtx);
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

/* No atomic ops */

ERTS_GLB_INLINE void
erts_no_dw_atomic_set(erts_no_dw_atomic_t *var, erts_no_dw_atomic_t *val)
{
    var->sint[0] = val->sint[0];
    var->sint[1] = val->sint[1];
}

ERTS_GLB_INLINE void
erts_no_dw_atomic_read(erts_no_dw_atomic_t *var, erts_no_dw_atomic_t *val)
{
    val->sint[0] = var->sint[0];
    val->sint[1] = var->sint[1];
}

ERTS_GLB_INLINE int erts_no_dw_atomic_cmpxchg(erts_no_dw_atomic_t *var,
					      erts_no_dw_atomic_t *new_val,
					      erts_no_dw_atomic_t *old_val)
{
    if (var->sint[0] != old_val->sint[0] || var->sint[1] != old_val->sint[1]) {
	erts_no_dw_atomic_read(var, old_val);
	return 0;
    }
    else {
	erts_no_dw_atomic_set(var, new_val);
	return !0;
    }
}

ERTS_GLB_INLINE void
erts_no_atomic_set(erts_no_atomic_t *var, erts_aint_t i)
{
    *var = i;
}

ERTS_GLB_INLINE erts_aint_t
erts_no_atomic_read(erts_no_atomic_t *var)
{
    return *var;
}

ERTS_GLB_INLINE erts_aint_t
erts_no_atomic_inc_read(erts_no_atomic_t *incp)
{
    return ++(*incp);
}

ERTS_GLB_INLINE erts_aint_t
erts_no_atomic_dec_read(erts_no_atomic_t *decp)
{
    return --(*decp);
}

ERTS_GLB_INLINE void
erts_no_atomic_inc(erts_no_atomic_t *incp)
{
    ++(*incp);
}

ERTS_GLB_INLINE void
erts_no_atomic_dec(erts_no_atomic_t *decp)
{
    --(*decp);
}

ERTS_GLB_INLINE erts_aint_t
erts_no_atomic_add_read(erts_no_atomic_t *addp, erts_aint_t i)
{
    return *addp += i;
}

ERTS_GLB_INLINE void
erts_no_atomic_add(erts_no_atomic_t *addp, erts_aint_t i)
{
    *addp += i;
}

ERTS_GLB_INLINE erts_aint_t
erts_no_atomic_read_bor(erts_no_atomic_t *var, erts_aint_t mask)
{
    erts_aint_t old;
    old = *var;
    *var |= mask;
    return old;
}

ERTS_GLB_INLINE erts_aint_t
erts_no_atomic_read_band(erts_no_atomic_t *var, erts_aint_t mask)
{
    erts_aint_t old;
    old = *var;
    *var &= mask;
    return old;
}

ERTS_GLB_INLINE erts_aint_t
erts_no_atomic_xchg(erts_no_atomic_t *xchgp, erts_aint_t new)
{
    erts_aint_t old = *xchgp;
    *xchgp = new;
    return old;
}

ERTS_GLB_INLINE erts_aint_t
erts_no_atomic_cmpxchg(erts_no_atomic_t *xchgp,
		       erts_aint_t new,
		       erts_aint_t expected)
{
    erts_aint_t old = *xchgp;
    if (old == expected)
        *xchgp = new;
    return old;
}

/* atomic32 */

ERTS_GLB_INLINE void
erts_no_atomic32_set(erts_no_atomic32_t *var, erts_aint32_t i)
{
    *var = i;
}

ERTS_GLB_INLINE erts_aint32_t
erts_no_atomic32_read(erts_no_atomic32_t *var)
{
    return *var;
}

ERTS_GLB_INLINE erts_aint32_t
erts_no_atomic32_inc_read(erts_no_atomic32_t *incp)
{
    return ++(*incp);
}

ERTS_GLB_INLINE erts_aint32_t
erts_no_atomic32_dec_read(erts_no_atomic32_t *decp)
{
    return --(*decp);
}

ERTS_GLB_INLINE void
erts_no_atomic32_inc(erts_no_atomic32_t *incp)
{
    ++(*incp);
}

ERTS_GLB_INLINE void
erts_no_atomic32_dec(erts_no_atomic32_t *decp)
{
    --(*decp);
}

ERTS_GLB_INLINE erts_aint32_t
erts_no_atomic32_add_read(erts_no_atomic32_t *addp, erts_aint32_t i)
{
    return *addp += i;
}

ERTS_GLB_INLINE void
erts_no_atomic32_add(erts_no_atomic32_t *addp, erts_aint32_t i)
{
    *addp += i;
}

ERTS_GLB_INLINE erts_aint32_t
erts_no_atomic32_read_bor(erts_no_atomic32_t *var, erts_aint32_t mask)
{
    erts_aint32_t old;
    old = *var;
    *var |= mask;
    return old;
}

ERTS_GLB_INLINE erts_aint32_t
erts_no_atomic32_read_band(erts_no_atomic32_t *var, erts_aint32_t mask)
{
    erts_aint32_t old;
    old = *var;
    *var &= mask;
    return old;
}

ERTS_GLB_INLINE erts_aint32_t
erts_no_atomic32_xchg(erts_no_atomic32_t *xchgp, erts_aint32_t new)
{
    erts_aint32_t old = *xchgp;
    *xchgp = new;
    return old;
}

ERTS_GLB_INLINE erts_aint32_t
erts_no_atomic32_cmpxchg(erts_no_atomic32_t *xchgp,
			 erts_aint32_t new,
			 erts_aint32_t expected)
{
    erts_aint32_t old = *xchgp;
    if (old == expected)
        *xchgp = new;
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
erts_spinlock_init_x_opt(erts_spinlock_t *lock, char *name, Eterm extra,
			 Uint16 opt)
{
#ifdef USE_THREADS
    int res = ethr_spinlock_init(&lock->slck);
    if (res)
	erts_thr_fatal_error(res, "init spinlock");
#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_lc_init_lock_x(&lock->lc, name, ERTS_LC_FLG_LT_SPINLOCK, extra);
#endif
#ifdef ERTS_ENABLE_LOCK_COUNT
    erts_lcnt_init_lock_x(&lock->lcnt, name, ERTS_LCNT_LT_SPINLOCK|opt, extra);
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
    if (res != 0) {
#ifdef ERTS_THR_HAVE_BUSY_DESTROY_BUG
	if (res == EBUSY) {
	    char *warn = "Ignoring busy spinlock destroy. "
		"Most likely a bug in pthread implementation.";
	    erts_send_warning_to_logger_str_nogl(warn);
	}
	else
#endif
	    erts_thr_fatal_error(res, "destroy rwlock");
    }
#else
    (void)lock;
#endif
}

ERTS_GLB_INLINE void
erts_spin_unlock(erts_spinlock_t *lock)
{
#ifdef USE_THREADS
#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_lc_unlock(&lock->lc);
#endif
#ifdef ERTS_ENABLE_LOCK_COUNT
    erts_lcnt_unlock(&lock->lcnt);
#endif
    ethr_spin_unlock(&lock->slck);
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
#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_lc_lock(&lock->lc);
#endif
#ifdef ERTS_ENABLE_LOCK_COUNT
    erts_lcnt_lock(&lock->lcnt);
#endif
    ethr_spin_lock(&lock->slck);
#ifdef ERTS_ENABLE_LOCK_COUNT
    erts_lcnt_lock_post_x(&lock->lcnt, file, line);
#endif
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
    if (res != 0) {
#ifdef ERTS_THR_HAVE_BUSY_DESTROY_BUG
	if (res == EBUSY) {
	    char *warn = "Ignoring busy rwlock destroy. "
		"Most likely a bug in pthread implementation.";
	    erts_send_warning_to_logger_str_nogl(warn);
	}
	else
#endif
	    erts_thr_fatal_error(res, "destroy rwlock");
    }
#else
    (void)lock;
#endif
}

ERTS_GLB_INLINE void
erts_read_unlock(erts_rwlock_t *lock)
{
#ifdef USE_THREADS
#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_lc_unlock_flg(&lock->lc, ERTS_LC_FLG_LO_READ);
#endif
#ifdef ERTS_ENABLE_LOCK_COUNT
    erts_lcnt_unlock_opt(&lock->lcnt, ERTS_LCNT_LO_READ);
#endif
    ethr_read_unlock(&lock->rwlck);
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
#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_lc_lock_flg(&lock->lc, ERTS_LC_FLG_LO_READ);
#endif
#ifdef ERTS_ENABLE_LOCK_COUNT
    erts_lcnt_lock_opt(&lock->lcnt, ERTS_LCNT_LO_READ);
#endif
    ethr_read_lock(&lock->rwlck);
#ifdef ERTS_ENABLE_LOCK_COUNT
    erts_lcnt_lock_post_x(&lock->lcnt, file, line);
#endif
#else
    (void)lock;
#endif
}

ERTS_GLB_INLINE void
erts_write_unlock(erts_rwlock_t *lock)
{
#ifdef USE_THREADS
#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_lc_unlock_flg(&lock->lc, ERTS_LC_FLG_LO_READ_WRITE);
#endif
#ifdef ERTS_ENABLE_LOCK_COUNT
    erts_lcnt_unlock_opt(&lock->lcnt, ERTS_LCNT_LO_READ_WRITE);
#endif
    ethr_write_unlock(&lock->rwlck);
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
#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_lc_lock_flg(&lock->lc, ERTS_LC_FLG_LO_READ_WRITE);
#endif
#ifdef ERTS_ENABLE_LOCK_COUNT
    erts_lcnt_lock_opt(&lock->lcnt, ERTS_LCNT_LO_READ_WRITE);
#endif
    ethr_write_lock(&lock->rwlck);
#ifdef ERTS_ENABLE_LOCK_COUNT
    erts_lcnt_lock_post_x(&lock->lcnt, file, line);
#endif
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

ERTS_GLB_INLINE erts_tse_t *erts_tse_fetch(void)
{
#ifdef USE_THREADS
    return (erts_tse_t *) ethr_get_ts_event();
#else
    return (erts_tse_t *) NULL;
#endif
}

ERTS_GLB_INLINE void erts_tse_return(erts_tse_t *ep)
{
#ifdef USE_THREADS
    ethr_leave_ts_event(ep);
#endif
}

ERTS_GLB_INLINE void erts_tse_set(erts_tse_t *ep)
{
#ifdef USE_THREADS
    ethr_event_set(&((ethr_ts_event *) ep)->event);
#endif
}

ERTS_GLB_INLINE void erts_tse_reset(erts_tse_t *ep)
{
#ifdef USE_THREADS
    ethr_event_reset(&((ethr_ts_event *) ep)->event);
#endif
}

ERTS_GLB_INLINE int erts_tse_wait(erts_tse_t *ep)
{
#ifdef USE_THREADS
    return ethr_event_wait(&((ethr_ts_event *) ep)->event);
#else
    return ENOTSUP;
#endif
}

ERTS_GLB_INLINE int erts_tse_swait(erts_tse_t *ep, int spincount)
{
#ifdef USE_THREADS
    return ethr_event_swait(&((ethr_ts_event *) ep)->event, spincount);
#else
    return ENOTSUP;
#endif
}

ERTS_GLB_INLINE int erts_tse_is_tmp(erts_tse_t *ep)
{
#ifdef USE_THREADS
    return (ep->iflgs & ETHR_TS_EV_TMP) == ETHR_TS_EV_TMP;
#else
    return 0;
#endif
}

ERTS_GLB_INLINE void erts_thr_set_main_status(int on, int no)
{
#ifdef USE_THREADS
    int res = ethr_set_main_thr_status(on, no);
    if (res != 0)
	erts_thr_fatal_error(res, "set thread main status");
#endif
}

ERTS_GLB_INLINE int erts_thr_get_main_status(void)
{
#ifdef USE_THREADS
    int main_status;
    int res = ethr_get_main_thr_status(&main_status);
    if (res != 0)
	erts_thr_fatal_error(res, "get thread main status");
    return main_status;
#else
    return 1;
#endif
}

ERTS_GLB_INLINE void erts_thr_yield(void)
{
#ifdef USE_THREADS
    int res = ETHR_YIELD();
    if (res != 0)
	erts_thr_fatal_error(res, "yield");
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

#ifdef ERTS_UNDEF_DEPRECATED_ATOMICS

/* Deprecated functions to replace */

#undef erts_atomic_init
#undef erts_atomic_set
#undef erts_atomic_read
#undef erts_atomic_inctest
#undef erts_atomic_dectest
#undef erts_atomic_inc
#undef erts_atomic_dec
#undef erts_atomic_addtest
#undef erts_atomic_add
#undef erts_atomic_xchg
#undef erts_atomic_cmpxchg
#undef erts_atomic_bor
#undef erts_atomic_band

#undef erts_atomic32_init
#undef erts_atomic32_set
#undef erts_atomic32_read
#undef erts_atomic32_inctest
#undef erts_atomic32_dectest
#undef erts_atomic32_inc
#undef erts_atomic32_dec
#undef erts_atomic32_addtest
#undef erts_atomic32_add
#undef erts_atomic32_xchg
#undef erts_atomic32_cmpxchg
#undef erts_atomic32_bor
#undef erts_atomic32_band

#endif
