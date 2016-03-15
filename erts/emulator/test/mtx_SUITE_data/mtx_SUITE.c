/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2010-2016. All Rights Reserved.
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
 * Stress tests of rwmutex implementation.
 *
 * Author: Rickard Green
 */

#include "erl_nif.h"

#ifdef __WIN32__
#  ifndef WIN32_LEAN_AND_MEAN
#    define WIN32_LEAN_AND_MEAN
#  endif
#  include <windows.h>
#else
#  include "ethread.h"
#  include "erl_misc_utils.h"
#  include <unistd.h>
#endif

#include <errno.h>
#include <stdio.h>

static int
fail(const char *file, int line, const char *function, const char *assertion);

#undef ASSERT
#define ASSERT(X) ((void) ((X) ? 1 : fail(__FILE__, __LINE__, __func__, #X)))

#ifdef __WIN32__
/*
 * We cannot access the ethread symbols directly; test
 * what we got in the nif api instead...
 */
#define HAVE_FREQREAD_SUPPORT 0
#define RWMUTEX_T ErlNifRWLock
#define RWMUTEX_CREATE(FR) enif_rwlock_create("dummy")
#define RWMUTEX_DESTROY enif_rwlock_destroy
#define RWMUTEX_WLOCK enif_rwlock_rwlock
#define RWMUTEX_TRYWLOCK enif_rwlock_tryrwlock
#define RWMUTEX_WUNLOCK enif_rwlock_rwunlock
#define RWMUTEX_TRYRLOCK enif_rwlock_tryrlock
#define RWMUTEX_RLOCK enif_rwlock_rlock
#define RWMUTEX_RUNLOCK enif_rwlock_runlock
#define THR_ID ErlNifTid
#define THR_CREATE(A, B, C, D) enif_thread_create("dummy", (A), (B), (C), (D))
#define THR_JOIN enif_thread_join
#define ATOMIC_T volatile LONG
#define ATOMIC_INIT(VarP, Val) (*(VarP) = (Val))
#define ATOMIC_SET(VarP, Val) (*(VarP) = (Val))
#define ATOMIC_READ(VarP) (*(VarP))
#define ATOMIC_INC InterlockedIncrement
#define ATOMIC_DEC InterlockedDecrement

#else

#ifdef ETHR_USE_OWN_RWMTX_IMPL__
#  define HAVE_FREQREAD_SUPPORT 1
#else
#  define HAVE_FREQREAD_SUPPORT 0
#endif

#define RWMUTEX_T ethr_rwmutex
static ethr_rwmutex *
RWMUTEX_CREATE(int freqread)
{
    ethr_rwmutex *rwmtx = enif_alloc(sizeof(ethr_rwmutex));
    ethr_rwmutex_opt rwmtx_opt = ETHR_RWMUTEX_OPT_DEFAULT_INITER;
    if (freqread)
	rwmtx_opt.type = ETHR_RWMUTEX_TYPE_FREQUENT_READ;
    ASSERT(rwmtx);
    ASSERT(ethr_rwmutex_init_opt(rwmtx, &rwmtx_opt) == 0);
    return rwmtx;
}
static void
RWMUTEX_DESTROY(ethr_rwmutex *rwmtx)
{
    ASSERT(ethr_rwmutex_destroy(rwmtx) == 0);
    enif_free(rwmtx);
}
#define RWMUTEX_TRYWLOCK ethr_rwmutex_tryrwlock
#define RWMUTEX_WLOCK ethr_rwmutex_rwlock
#define RWMUTEX_WUNLOCK ethr_rwmutex_rwunlock
#define RWMUTEX_TRYRLOCK ethr_rwmutex_tryrlock
#define RWMUTEX_RLOCK ethr_rwmutex_rlock
#define RWMUTEX_RUNLOCK ethr_rwmutex_runlock
#define THR_ID ethr_tid
#define THR_CREATE ethr_thr_create
#define THR_JOIN ethr_thr_join
#define ATOMIC_T ethr_atomic_t
#define ATOMIC_INIT ethr_atomic_init
#define ATOMIC_SET ethr_atomic_set
#define ATOMIC_READ ethr_atomic_read
#define ATOMIC_INC ethr_atomic_inc
#define ATOMIC_DEC ethr_atomic_dec

#endif


#if !defined(__func__)
#  if !defined(__STDC_VERSION__) || __STDC_VERSION__ < 199901L
#    if !defined(__GNUC__) ||  __GNUC__ < 2
#      define __func__ "[unknown_function]"
#    else
#      define __func__ __FUNCTION__
#    endif
#  endif
#endif

static void milli_sleep(int ms);
static int get_bool(ErlNifEnv* env, ERL_NIF_TERM term);

/*
 * Long rwlock testcase
 */

#define LONG_RW_NO_W_THREADS 6
#define LONG_RW_NO_THREADS 20
#define LONG_RW_NO_WLOCK_COUNT 100

typedef struct {
    RWMUTEX_T *rwlock;
    ATOMIC_T *is_wlocked;
    ATOMIC_T *is_rlocked;
    int *stop;
    int *count;
    int sleep;
} long_rw_t;

static void *
long_rw_w(void *varg)
{
    long_rw_t *arg = varg;
    int stop = 0;
    do {
	RWMUTEX_WLOCK(arg->rwlock);
	ASSERT(!ATOMIC_READ(arg->is_wlocked));
	ATOMIC_SET(arg->is_wlocked, 1);
	ASSERT(!ATOMIC_READ(arg->is_rlocked));
	milli_sleep(arg->sleep);
	if (++(*arg->count) > LONG_RW_NO_WLOCK_COUNT)
	    stop = *arg->stop = 1;
	ATOMIC_SET(arg->is_wlocked, 0);
	ASSERT(!ATOMIC_READ(arg->is_rlocked));
	RWMUTEX_WUNLOCK(arg->rwlock);
    } while (!stop);
    return NULL;
}

static void *
long_rw_r(void *varg)
{
    long_rw_t *arg = varg;
    int stop;
    do {
	RWMUTEX_RLOCK(arg->rwlock);
	ASSERT(!ATOMIC_READ(arg->is_wlocked));
	ATOMIC_INC(arg->is_rlocked);
	milli_sleep(arg->sleep);
	stop = *arg->stop;
	ATOMIC_DEC(arg->is_rlocked);
	ASSERT(!ATOMIC_READ(arg->is_wlocked));
	RWMUTEX_RUNLOCK(arg->rwlock);
    } while (!stop);
    return NULL;
}


static ERL_NIF_TERM long_rw_test(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    int res, freqread, i, count, stop;
    ATOMIC_T is_wlocked, is_rlocked;
    THR_ID tid[LONG_RW_NO_THREADS];
    long_rw_t arg;
    long_rw_t targ[LONG_RW_NO_THREADS];

    ATOMIC_INIT(&is_wlocked, 0);
    ATOMIC_INIT(&is_rlocked, 0);

    freqread = 0;

    arg.is_wlocked = &is_wlocked;
    arg.is_rlocked = &is_rlocked;
    arg.count = &count;
    arg.stop = &stop;

 restart:

    stop = 0;
    count = 0;

    arg.rwlock = RWMUTEX_CREATE(freqread);

    ASSERT(arg.rwlock);

    for (i = 0; i < LONG_RW_NO_W_THREADS; i++) {
	targ[i] = arg;
	targ[i].sleep = 100 + i*10;
	ASSERT(THR_CREATE(&tid[i], long_rw_w, &targ[i], NULL) == 0);
    }
    for (; i < LONG_RW_NO_THREADS; i++) {
	targ[i] = arg;
	targ[i].sleep = 100;
	ASSERT(THR_CREATE(&tid[i], long_rw_r, &targ[i], NULL) == 0);
    }
    for (i = 0; i < LONG_RW_NO_THREADS; i++)
	ASSERT(THR_JOIN(tid[i], NULL) == 0);

    ASSERT(!ATOMIC_READ(arg.is_wlocked));
    ASSERT(!ATOMIC_READ(arg.is_rlocked));

    RWMUTEX_DESTROY(arg.rwlock);

    if (HAVE_FREQREAD_SUPPORT && !freqread) {
	freqread = 1;
	goto restart;
    }

    if (freqread)
	return enif_make_atom(env, "ok");
    else
	return enif_make_tuple2(env,
				enif_make_atom(env,
					       "comment"),
				enif_make_string(env,
						 "No frequent read test made.",
						 ERL_NIF_LATIN1));
}

/*
 * Hammer rwlock testcase
 */

#define HAMMER_RW_NO_W_THREADS 6
#define HAMMER_RW_NO_THREADS 20
#define HAMMER_RW_NO_WLOCK_COUNT 1000000

typedef struct {
    RWMUTEX_T *rwlock;
    ATOMIC_T is_locked;
    int lock_check;
    int stop;
    int count;
} hammer_rw_t;

static void *
hammer_rw_w(void *varg)
{
    hammer_rw_t *arg = varg;
    int stop = 0;
    do {
	RWMUTEX_WLOCK(arg->rwlock);
	if (arg->lock_check) {
	    ASSERT(!ATOMIC_READ(&arg->is_locked));
	    ATOMIC_SET(&arg->is_locked, -1);
	}
	if (++arg->count > HAMMER_RW_NO_WLOCK_COUNT)
	    stop = arg->stop = 1;
	if (arg->lock_check) {
	    ASSERT(ATOMIC_READ(&arg->is_locked) == -1);
	    ATOMIC_SET(&arg->is_locked, 0);
	}
	RWMUTEX_WUNLOCK(arg->rwlock);
    } while (!stop);
    return NULL;
}

static void *
hammer_rw_r(void *varg)
{
    hammer_rw_t *arg = varg;
    int stop;
    do {
	RWMUTEX_RLOCK(arg->rwlock);
	if (arg->lock_check) {
	    ASSERT(ATOMIC_READ(&arg->is_locked) >= 0);
	    ATOMIC_INC(&arg->is_locked);
	}
	stop = arg->stop;
	if (arg->lock_check) {
	    ASSERT(ATOMIC_READ(&arg->is_locked) > 0);
	    ATOMIC_DEC(&arg->is_locked);
	}
	RWMUTEX_RUNLOCK(arg->rwlock);
    } while (!stop); 
    return NULL;
}


static ERL_NIF_TERM hammer_rw_test(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    hammer_rw_t arg;
    char buf[10];
    int res, freqread, i;
    THR_ID tid[HAMMER_RW_NO_THREADS];

    if (argc != 1)
	goto badarg;

    arg.lock_check = get_bool(env, argv[0]);
    if (arg.lock_check < 0)
	goto badarg;

    ATOMIC_INIT(&arg.is_locked, 0);

    freqread = 0;

 restart:
    arg.stop = 0;
    arg.count = 0;

    arg.rwlock = RWMUTEX_CREATE(freqread);

    ASSERT(arg.rwlock);

    for (i = 0; i < HAMMER_RW_NO_W_THREADS; i++)
	ASSERT(THR_CREATE(&tid[i], hammer_rw_w, &arg, NULL) == 0);
    for (; i < HAMMER_RW_NO_THREADS; i++)
	ASSERT(THR_CREATE(&tid[i], hammer_rw_r, &arg, NULL) == 0);
    for (i = 0; i < HAMMER_RW_NO_THREADS; i++)
	ASSERT(THR_JOIN(tid[i], NULL) == 0);

    ASSERT(!ATOMIC_READ(&arg.is_locked));

    RWMUTEX_DESTROY(arg.rwlock);

    if (HAVE_FREQREAD_SUPPORT && !freqread) {
	freqread = 1;
	goto restart;
    }

    if (freqread)
	return enif_make_atom(env, "ok");
    else
	return enif_make_tuple2(env,
				enif_make_atom(env,
					       "comment"),
				enif_make_string(env,
						 "No frequent read test made.",
						 ERL_NIF_LATIN1));
 badarg:
    return enif_make_badarg(env);
}

/*
 * Hammer try rwlock testcase
 */

#define HAMMER_TRYRW_NO_W_THREADS 10
#define HAMMER_TRYRW_NO_THREADS 20
#define HAMMER_TRYRW_NO_WLOCK_COUNT 10000000
#define HAMMER_TRYRW_NO_RLOCK_COUNT 10000000
#define HAMMER_TRYRW_NO_WLOCK_WAIT_COUNT ((10*HAMMER_TRYRW_NO_WLOCK_COUNT)/8)
#define HAMMER_TRYRW_NO_RLOCK_WAIT_COUNT ((10*HAMMER_TRYRW_NO_RLOCK_COUNT)/8)

typedef struct {
    RWMUTEX_T *rwlock;
    ATOMIC_T is_locked;
    int lock_check;
    int w_count;
    ATOMIC_T r_count;
} hammer_tryrw_t;

static void *
hammer_tryrw_w(void *varg)
{
    hammer_tryrw_t *arg = varg;
    int stop = 0;
    int wait = 0;
    do {
	while (EBUSY == RWMUTEX_TRYWLOCK(arg->rwlock));
	if (arg->lock_check) {
	    ASSERT(!ATOMIC_READ(&arg->is_locked));
	    ATOMIC_SET(&arg->is_locked, -1);
	}
	if (++arg->w_count > HAMMER_TRYRW_NO_WLOCK_COUNT)
	    stop = 1;
	else if (arg->w_count > HAMMER_TRYRW_NO_RLOCK_WAIT_COUNT)
	    wait = 1;
	if (arg->lock_check) {
	    ASSERT(ATOMIC_READ(&arg->is_locked) == -1);
	    ATOMIC_SET(&arg->is_locked, 0);
	}
	RWMUTEX_WUNLOCK(arg->rwlock);
	if (wait)
	    milli_sleep(1);
    } while (!stop);
    return NULL;
}

static void *
hammer_tryrw_r(void *varg)
{
    hammer_tryrw_t *arg = varg;
    long r_count;
    int stop = 0;
    int wait = 0;
    do {
	while (EBUSY == RWMUTEX_TRYRLOCK(arg->rwlock));
	if (arg->lock_check) {
	    ASSERT(ATOMIC_READ(&arg->is_locked) >= 0);
	    ATOMIC_INC(&arg->is_locked);
	}
	ATOMIC_INC(&arg->r_count);
	r_count = ATOMIC_READ(&arg->r_count);
	if (r_count > HAMMER_TRYRW_NO_RLOCK_COUNT)
	    stop = 1;
	else if (r_count > HAMMER_TRYRW_NO_RLOCK_WAIT_COUNT)
	    wait = 1;
	if (arg->lock_check) {
	    ASSERT(ATOMIC_READ(&arg->is_locked) > 0);
	    ATOMIC_DEC(&arg->is_locked);
	}
	RWMUTEX_RUNLOCK(arg->rwlock);
	if (wait)
	    milli_sleep(1);
    } while (!stop);
    return NULL;
}


static ERL_NIF_TERM hammer_tryrw_test(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    hammer_tryrw_t arg;
    char buf[10];
    int res, freqread, i;
    THR_ID tid[HAMMER_TRYRW_NO_THREADS];

    if (argc != 1)
	goto badarg;

    arg.lock_check = get_bool(env, argv[0]);
    if (arg.lock_check < 0)
	goto badarg;

    ATOMIC_INIT(&arg.is_locked, 0);
    freqread = 0;

 restart:

    arg.w_count = 0;
    ATOMIC_INIT(&arg.r_count, 0);

    arg.rwlock = RWMUTEX_CREATE(freqread);

    ASSERT(arg.rwlock);

    for (i = 0; i < HAMMER_TRYRW_NO_W_THREADS; i++)
	ASSERT(THR_CREATE(&tid[i], hammer_tryrw_w, &arg, NULL) == 0);
    for (; i < HAMMER_TRYRW_NO_THREADS; i++)
	ASSERT(THR_CREATE(&tid[i], hammer_tryrw_r, &arg, NULL) == 0);
    for (i = 0; i < HAMMER_TRYRW_NO_THREADS; i++)
	ASSERT(THR_JOIN(tid[i], NULL) == 0);

    ASSERT(!ATOMIC_READ(&arg.is_locked));

    RWMUTEX_DESTROY(arg.rwlock);

    if (HAVE_FREQREAD_SUPPORT && !freqread) {
	freqread = 1;
	goto restart;
    }

    if (freqread)
	return enif_make_atom(env, "ok");
    else
	return enif_make_tuple2(env,
				enif_make_atom(env,
					       "comment"),
				enif_make_string(env,
						 "No frequent read test made.",
						 ERL_NIF_LATIN1));
 badarg:
    return enif_make_badarg(env);
}

typedef struct {
    int lock_check;
    ATOMIC_T is_locked;
    RWMUTEX_T *rwlock;
} rwlock_resource_t;

static void
rwlock_destructor(ErlNifEnv* env, void* obj)
{
    rwlock_resource_t *rwlr = obj;
    if (rwlr->lock_check)
	ASSERT(!ATOMIC_READ(&rwlr->is_locked));
    RWMUTEX_DESTROY(rwlr->rwlock);
}

/*
 * create_rwlock(FreqRead, LockCheck)
 */

static ERL_NIF_TERM
create_rwlock(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    int lock_check, freqread;
    ERL_NIF_TERM rwlock_term;
    rwlock_resource_t *rwlr;
    char buf[100];

    if (argc != 2)
	goto badarg;

    freqread = get_bool(env, argv[0]);
    if (freqread < 0)
	goto badarg;

    if (!HAVE_FREQREAD_SUPPORT && freqread)
	return enif_make_atom(env, "enotsup");

    lock_check = get_bool(env, argv[1]);
    if (lock_check < 0)
	goto badarg;

    rwlr = enif_alloc_resource(enif_priv_data(env), sizeof(rwlock_resource_t));
    rwlr->lock_check = lock_check;
    ATOMIC_INIT(&rwlr->is_locked, 0);
    rwlr->rwlock = RWMUTEX_CREATE(freqread);
    rwlock_term = enif_make_resource(env, rwlr);
    enif_release_resource(rwlr);
    return rwlock_term;

 badarg:
    return enif_make_badarg(env);
}

/*
 * rwlock_op(RWLock, Blocking, WriteOp, WaitTime) 
 */

static ERL_NIF_TERM
rwlock_op(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    /*
     * Use a union for pointer type conversion to avoid compiler warnings
     * about strict-aliasing violations with gcc-4.1. gcc >= 4.2 does not
     * emit the warning.
     * TODO: Reconsider use of union once gcc-4.1 is obsolete?
     */
    union { void* vp; rwlock_resource_t *p; } rwlr;
    int blocking, write, wait_locked, wait_unlocked;

    if (argc != 5)
	goto badarg;

    if (!enif_get_resource(env, argv[0], enif_priv_data(env), &rwlr.vp))
	goto badarg;

    blocking = get_bool(env, argv[1]);
    if (blocking < 0)
	goto badarg;

    write = get_bool(env, argv[2]);
    if (write < 0)
	goto badarg;

    if (!enif_get_int(env, argv[3], &wait_locked))
	goto badarg;
    if (wait_locked < 0)
	goto badarg;

    if (!enif_get_int(env, argv[4], &wait_unlocked))
	goto badarg;
    if (wait_unlocked < 0)
	goto badarg;

    if (write) {
	if (blocking)
	    RWMUTEX_WLOCK(rwlr.p->rwlock);
	else
	    while (EBUSY == RWMUTEX_TRYWLOCK(rwlr.p->rwlock));
	if (rwlr.p->lock_check) {
	    ASSERT(!ATOMIC_READ(&rwlr.p->is_locked));
	    ATOMIC_SET(&rwlr.p->is_locked, -1);
	}
    }
    else {
	if (blocking)
	    RWMUTEX_RLOCK(rwlr.p->rwlock);
	else
	    while (EBUSY == RWMUTEX_TRYRLOCK(rwlr.p->rwlock));
	if (rwlr.p->lock_check) {
	    ASSERT(ATOMIC_READ(&rwlr.p->is_locked) >= 0);
	    ATOMIC_INC(&rwlr.p->is_locked);
	}
    }

    if (wait_locked)
	milli_sleep(wait_locked);

    if (write) {
	if (rwlr.p->lock_check) {
	    ASSERT(ATOMIC_READ(&rwlr.p->is_locked) == -1);
	    ATOMIC_SET(&rwlr.p->is_locked, 0);
	}
	RWMUTEX_WUNLOCK(rwlr.p->rwlock);
    }
    else {
	if (rwlr.p->lock_check) {
	    ASSERT(ATOMIC_READ(&rwlr.p->is_locked) > 0);
	    ATOMIC_DEC(&rwlr.p->is_locked);
	}
	RWMUTEX_RUNLOCK(rwlr.p->rwlock);
    }

    if (wait_unlocked)
	milli_sleep(wait_unlocked);

    return enif_make_atom(env, "ok");
 badarg:
    return enif_make_badarg(env);
}

static int load_nif_lib(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    *priv_data = enif_open_resource_type(env,
					 NULL,
					 "rwlock_resource",
					 rwlock_destructor,
					 ERL_NIF_RT_CREATE,
					 NULL);
    if (*priv_data)
	return 0;
    else
	return -1;
}

/*
 * 0 -> false
 * >0 -> true
 * <0 -> error
 */

static int
get_bool(ErlNifEnv* env, ERL_NIF_TERM term)
{
    int res;
    char buf[10];

    res = enif_get_atom(env, term, buf, sizeof(buf), ERL_NIF_LATIN1);
    if (res == 0)
	return -1;
    if (strcmp("false", buf) == 0)
	return 0;
    else if (strcmp("true", buf) == 0)
	return 1;
    else
	return -1;
}

static int
fail(const char *file, int line, const char *function, const char *assertion)
{
    fprintf(stderr, "%s:%d: Assertion failed in %s(): %s\n",
	    file, line, function, assertion);
    abort();
}

static void
milli_sleep(int ms)
{
#ifdef __WIN32__
    Sleep(ms);
#else
    while (erts_milli_sleep(ms) != 0);
#endif
}

static ErlNifFunc nif_funcs[] = {
    {"long_rw_test", 0, long_rw_test},
    {"hammer_rw_test", 1, hammer_rw_test},
    {"hammer_tryrw_test", 1, hammer_tryrw_test},
    {"create_rwlock", 2, create_rwlock},
    {"rwlock_op", 5, rwlock_op}
};

ERL_NIF_INIT(mtx_SUITE, nif_funcs, load_nif_lib, NULL, NULL, NULL)
