/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2007-2018. All Rights Reserved.
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

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "global.h"
#include <string.h>

#if defined(__APPLE__) && defined(__MACH__) && !defined(__DARWIN__)
#define __DARWIN__ 1
#endif

#define ERL_DRV_THR_OPTS_SIZE(LAST_FIELD) \
  (((size_t) &((ErlDrvThreadOpts *) 0)->LAST_FIELD) \
   + sizeof(((ErlDrvThreadOpts *) 0)->LAST_FIELD))

static void
fatal_error(int err, char *func)
{
    char *estr = strerror(err);
    if (!estr) {
	if (err == ENOTSUP)
	    estr = "Not supported";
	else
	    estr = "Unknown error";
    }
    erts_exit(ERTS_ABORT_EXIT, "Fatal error in %s: %s [%d]\n", func, estr, err);
}

#define ERL_DRV_TSD_KEYS_INC 10
#define ERL_DRV_TSD_EXTRA 10
#define ERL_DRV_INVALID_TSD_KEY INT_MAX


struct ErlDrvMutex_ {
    ethr_mutex mtx;
#ifdef ERTS_ENABLE_LOCK_COUNT
    erts_lcnt_ref_t lcnt;
#endif
    char *name;
};

struct ErlDrvCond_ {
    ethr_cond cnd;
    char *name;
};

struct ErlDrvRWLock_ {
    ethr_rwmutex rwmtx;
#ifdef ERTS_ENABLE_LOCK_COUNT
    erts_lcnt_ref_t lcnt;
#endif
    char *name;
};

struct ErlDrvTid_ {
    ethr_tid tid;
    void* (*func)(void*);
    void* arg;
    int drv_thr;
    Uint tsd_len;
    void **tsd;
    char *name;
};

static ethr_tsd_key tid_key;


static ErlDrvTSDKey next_tsd_key;
static ErlDrvTSDKey max_used_tsd_key;
static ErlDrvTSDKey used_tsd_keys_len;
static char **used_tsd_keys;
static erts_mtx_t tsd_mtx;
static char *no_name;


static void
thread_exit_handler(void)
{
    struct ErlDrvTid_ *dtid = ethr_tsd_get(tid_key);
    if (dtid) {
	if (dtid->tsd)
	    erts_free(ERTS_ALC_T_DRV_TSD, dtid->tsd);
	if (!dtid->drv_thr)
	    erts_free(ERTS_ALC_T_DRV_TID, dtid);
    }
}

static void *
erl_drv_thread_wrapper(void *vdtid)
{
    int res;
    struct ErlDrvTid_ *dtid = (struct ErlDrvTid_ *) vdtid;
    res = ethr_tsd_set(tid_key, vdtid);
    if (res != 0)
	fatal_error(res, "erl_drv_thread_wrapper()");
    return (*dtid->func)(dtid->arg);
}


void erl_drv_thr_init(void)
{
    int i;
    int res = ethr_tsd_key_create(&tid_key,"erts_tid_key");
    if (res == 0)
	res = ethr_install_exit_handler(thread_exit_handler);
    if (res != 0)
	fatal_error(res, "erl_drv_thr_init()");

    no_name = "unknown";
    next_tsd_key = 0;
    max_used_tsd_key = -1;
    used_tsd_keys_len = ERL_DRV_TSD_KEYS_INC;
    used_tsd_keys = erts_alloc(ERTS_ALC_T_DRV_TSD,
			       sizeof(char *)*ERL_DRV_TSD_KEYS_INC);
    for (i = 0; i < ERL_DRV_TSD_KEYS_INC; i++)
	used_tsd_keys[i] = NULL;
    erts_mtx_init(&tsd_mtx, "drv_tsd", NIL,
        ERTS_LOCK_FLAGS_PROPERTY_STATIC | ERTS_LOCK_FLAGS_CATEGORY_IO);
}

/*
 * These functions implement the driver thread interface in erl_driver.h.
 * NOTE: Only use this interface from drivers. From within the emulator use
 * either the erl_threads.h or the ethread.h interface.
 */

ErlDrvMutex *
erl_drv_mutex_create(char *name)
{
    ErlDrvMutex *dmtx = erts_alloc_fnf(ERTS_ALC_T_DRV_MTX,
				       (sizeof(ErlDrvMutex)
					+ (name ? sys_strlen(name) + 1 : 0)));
    if (dmtx) {
	ethr_mutex_opt opt = ETHR_MUTEX_OPT_DEFAULT_INITER;
	opt.posix_compliant = 1;
	if (ethr_mutex_init_opt(&dmtx->mtx, &opt) != 0) {
	    erts_free(ERTS_ALC_T_DRV_MTX, (void *) dmtx);
	    return NULL;
	}
	if (name) {
	    dmtx->name = ((char *) dmtx) + sizeof(ErlDrvMutex);
	    sys_strcpy(dmtx->name, name);
        } else {
	    dmtx->name = no_name;
	}
#ifdef ERTS_ENABLE_LOCK_COUNT
    erts_lcnt_init_ref_x(&dmtx->lcnt, dmtx->name, NIL,
        ERTS_LOCK_TYPE_MUTEX | ERTS_LOCK_FLAGS_CATEGORY_IO);
#endif
    }
    return dmtx;
}

void
erl_drv_mutex_destroy(ErlDrvMutex *dmtx)
{
    int res;
#ifdef ERTS_ENABLE_LOCK_COUNT
    erts_lcnt_uninstall(&dmtx->lcnt);
#endif
    res = dmtx ? ethr_mutex_destroy(&dmtx->mtx) : EINVAL;
    if (res != 0)
	fatal_error(res, "erl_drv_mutex_destroy()");
    erts_free(ERTS_ALC_T_DRV_MTX, (void *) dmtx);
}


char *
erl_drv_mutex_name(ErlDrvMutex *dmtx)
{
    return dmtx ? dmtx->name : NULL;
}

int
erl_drv_mutex_trylock(ErlDrvMutex *dmtx)
{
    int res;
    if (!dmtx)
	fatal_error(EINVAL, "erl_drv_mutex_trylock()");
    res = ethr_mutex_trylock(&dmtx->mtx);
#ifdef ERTS_ENABLE_LOCK_COUNT
    erts_lcnt_trylock(&dmtx->lcnt, res);
#endif
    return res;
}

void
erl_drv_mutex_lock(ErlDrvMutex *dmtx)
{
    if (!dmtx)
	fatal_error(EINVAL, "erl_drv_mutex_lock()");
#ifdef ERTS_ENABLE_LOCK_COUNT
    erts_lcnt_lock(&dmtx->lcnt);
#endif
    ethr_mutex_lock(&dmtx->mtx);
#ifdef ERTS_ENABLE_LOCK_COUNT
    erts_lcnt_lock_post(&dmtx->lcnt);
#endif
}

void
erl_drv_mutex_unlock(ErlDrvMutex *dmtx)
{
    if (!dmtx)
	fatal_error(EINVAL, "erl_drv_mutex_unlock()");
#ifdef ERTS_ENABLE_LOCK_COUNT
    erts_lcnt_unlock(&dmtx->lcnt);
#endif
    ethr_mutex_unlock(&dmtx->mtx);
}

ErlDrvCond *
erl_drv_cond_create(char *name)
{
    ErlDrvCond *dcnd = erts_alloc_fnf(ERTS_ALC_T_DRV_CND,
				      (sizeof(ErlDrvCond)
				       + (name ? sys_strlen(name) + 1 : 0)));
    if (dcnd) {
	ethr_cond_opt opt = ETHR_COND_OPT_DEFAULT_INITER;
	opt.posix_compliant = 1;
	if (ethr_cond_init_opt(&dcnd->cnd, &opt) != 0) {
	    erts_free(ERTS_ALC_T_DRV_CND, (void *) dcnd);
	    dcnd = NULL;
	}
	else if (!name)
	    dcnd->name = no_name;
	else {
	    dcnd->name = ((char *) dcnd) + sizeof(ErlDrvCond);
	    sys_strcpy(dcnd->name, name);
	}
    }
    return dcnd;
}

void
erl_drv_cond_destroy(ErlDrvCond *dcnd)
{
    int res = dcnd ? ethr_cond_destroy(&dcnd->cnd) : EINVAL;
    if (res != 0)
	fatal_error(res, "erl_drv_cond_destroy()");
    erts_free(ERTS_ALC_T_DRV_CND, (void *) dcnd);
}

char *
erl_drv_cond_name(ErlDrvCond *dcnd)
{
    return dcnd ? dcnd->name : NULL;
}

void
erl_drv_cond_signal(ErlDrvCond *dcnd)
{
    if (!dcnd)
	fatal_error(EINVAL, "erl_drv_cond_signal()");
    ethr_cond_signal(&dcnd->cnd);
}

void
erl_drv_cond_broadcast(ErlDrvCond *dcnd)
{
    if (!dcnd)
	fatal_error(EINVAL, "erl_drv_cond_broadcast()");
    ethr_cond_broadcast(&dcnd->cnd);
}


void
erl_drv_cond_wait(ErlDrvCond *dcnd, ErlDrvMutex *dmtx)
{
    if (!dcnd || !dmtx) {
	fatal_error(EINVAL, "erl_drv_cond_wait()");
    }
#ifdef ERTS_ENABLE_LOCK_COUNT
    erts_lcnt_unlock(&dmtx->lcnt);
#endif
    while (1) {
	int res = ethr_cond_wait(&dcnd->cnd, &dmtx->mtx);
	if (res == 0) {
#ifdef ERTS_ENABLE_LOCK_COUNT
            erts_lcnt_lock(&dmtx->lcnt);
            erts_lcnt_lock_post(&dmtx->lcnt);
#endif
	    break;
        }
    }
}

ErlDrvRWLock *
erl_drv_rwlock_create(char *name)
{
    ErlDrvRWLock *drwlck = erts_alloc_fnf(ERTS_ALC_T_DRV_RWLCK,
					  (sizeof(ErlDrvRWLock)
					   + (name ? sys_strlen(name) + 1 : 0)));
    if (drwlck) {
	if (ethr_rwmutex_init(&drwlck->rwmtx) != 0) {
	    erts_free(ERTS_ALC_T_DRV_RWLCK, (void *) drwlck);
	    return NULL;
	}
	if (name) {
	    drwlck->name = ((char *) drwlck) + sizeof(ErlDrvRWLock);
	    sys_strcpy(drwlck->name, name);
        } else {
	    drwlck->name = no_name;
	}
#ifdef ERTS_ENABLE_LOCK_COUNT
    erts_lcnt_init_ref_x(&drwlck->lcnt, drwlck->name, NIL,
        ERTS_LOCK_TYPE_RWMUTEX | ERTS_LOCK_FLAGS_CATEGORY_IO);
#endif
    }
    return drwlck;
}

void
erl_drv_rwlock_destroy(ErlDrvRWLock *drwlck)
{
    int res;
#ifdef ERTS_ENABLE_LOCK_COUNT
    erts_lcnt_uninstall(&drwlck->lcnt);
#endif
    res = drwlck ? ethr_rwmutex_destroy(&drwlck->rwmtx) : EINVAL;
    if (res != 0)
	fatal_error(res, "erl_drv_rwlock_destroy()");
    erts_free(ERTS_ALC_T_DRV_RWLCK, (void *) drwlck);
}

char *
erl_drv_rwlock_name(ErlDrvRWLock *drwlck)
{
    return drwlck ? drwlck->name : NULL;
}

int
erl_drv_rwlock_tryrlock(ErlDrvRWLock *drwlck)
{
    int res;
    if (!drwlck)
	fatal_error(EINVAL, "erl_drv_rwlock_tryrlock()");
    res = ethr_rwmutex_tryrlock(&drwlck->rwmtx);
#ifdef ERTS_ENABLE_LOCK_COUNT
    erts_lcnt_trylock_opt(&drwlck->lcnt, res, ERTS_LOCK_OPTIONS_READ);
#endif
    return res;
}

void
erl_drv_rwlock_rlock(ErlDrvRWLock *drwlck)
{
    if (!drwlck)
	fatal_error(EINVAL, "erl_drv_rwlock_rlock()");
#ifdef ERTS_ENABLE_LOCK_COUNT
    erts_lcnt_lock_opt(&drwlck->lcnt, ERTS_LOCK_OPTIONS_READ);
#endif
    ethr_rwmutex_rlock(&drwlck->rwmtx);
#ifdef ERTS_ENABLE_LOCK_COUNT
    erts_lcnt_lock_post(&drwlck->lcnt);
#endif
}

void
erl_drv_rwlock_runlock(ErlDrvRWLock *drwlck)
{
    if (!drwlck)
	fatal_error(EINVAL, "erl_drv_rwlock_runlock()");
#ifdef ERTS_ENABLE_LOCK_COUNT
    erts_lcnt_unlock_opt(&drwlck->lcnt, ERTS_LOCK_OPTIONS_READ);
#endif
    ethr_rwmutex_runlock(&drwlck->rwmtx);
}

int
erl_drv_rwlock_tryrwlock(ErlDrvRWLock *drwlck)
{
    int res;
    if (!drwlck)
	fatal_error(EINVAL, "erl_drv_rwlock_tryrwlock()");
    res = ethr_rwmutex_tryrwlock(&drwlck->rwmtx);
#ifdef ERTS_ENABLE_LOCK_COUNT
    erts_lcnt_trylock_opt(&drwlck->lcnt, res, ERTS_LOCK_OPTIONS_RDWR);
#endif
    return res;
}

void
erl_drv_rwlock_rwlock(ErlDrvRWLock *drwlck)
{
    if (!drwlck)
	fatal_error(EINVAL, "erl_drv_rwlock_rwlock()");
#ifdef ERTS_ENABLE_LOCK_COUNT
    erts_lcnt_lock_opt(&drwlck->lcnt, ERTS_LOCK_OPTIONS_RDWR);
#endif
    ethr_rwmutex_rwlock(&drwlck->rwmtx);
#ifdef ERTS_ENABLE_LOCK_COUNT
    erts_lcnt_lock_post(&drwlck->lcnt);
#endif
}

void
erl_drv_rwlock_rwunlock(ErlDrvRWLock *drwlck)
{
    if (!drwlck)
	fatal_error(EINVAL, "erl_drv_rwlock_rwunlock()");
#ifdef ERTS_ENABLE_LOCK_COUNT
    erts_lcnt_unlock_opt(&drwlck->lcnt, ERTS_LOCK_OPTIONS_RDWR);
#endif
    ethr_rwmutex_rwunlock(&drwlck->rwmtx);
}

int
erl_drv_tsd_key_create(char *name, ErlDrvTSDKey *key)
{
    char *name_copy;
    Uint old_used_tsd_keys_len;
    ErlDrvTSDKey res;

    if (!key)
	fatal_error(EINVAL, "erl_drv_tsd_key_create()");

    if (!name)
	name_copy = no_name;
    else {
	name_copy = erts_alloc_fnf(ERTS_ALC_T_DRV_TSD,
				   sizeof(char)*(sys_strlen(name) + 1));
	if (!name_copy) {
	    *key = -1;
	    return ENOMEM;
	}
	sys_strcpy(name_copy, name);
    }

    erts_mtx_lock(&tsd_mtx);

    *key = next_tsd_key;

    if (next_tsd_key < 0)
	res = ENOMEM;
    else {
	res = 0;

	ASSERT(!used_tsd_keys[next_tsd_key]);
	used_tsd_keys[next_tsd_key] = name_copy;

	if (max_used_tsd_key < next_tsd_key)
	    max_used_tsd_key = next_tsd_key;

	if (max_used_tsd_key + 1 >= used_tsd_keys_len) {
	    int i;
	    old_used_tsd_keys_len = used_tsd_keys_len;
	    if (used_tsd_keys_len + ERL_DRV_TSD_KEYS_INC >= INT_MAX) 
		next_tsd_key = -1;
	    else {
		char **new_used_tsd_keys;
		used_tsd_keys_len += ERL_DRV_TSD_KEYS_INC;
		new_used_tsd_keys = erts_realloc_fnf(ERTS_ALC_T_DRV_TSD,
						     used_tsd_keys,
						     (sizeof(char *)
						      * used_tsd_keys_len));
		if (!new_used_tsd_keys)
		    next_tsd_key = -1;
		else {
		    used_tsd_keys = new_used_tsd_keys;
		    for (i = old_used_tsd_keys_len; i < used_tsd_keys_len; i++)
			used_tsd_keys[i] = NULL;
		}
	    }
	}

	if (next_tsd_key >= 0) {
	    do {
		next_tsd_key++;
	    } while (used_tsd_keys[next_tsd_key]);
	}
	ASSERT(next_tsd_key < used_tsd_keys_len);
    }

    erts_mtx_unlock(&tsd_mtx);

    return res;
}

void
erl_drv_tsd_key_destroy(ErlDrvTSDKey key)
{
    erts_mtx_lock(&tsd_mtx);

    if (key < 0 || max_used_tsd_key < key || !used_tsd_keys[key])
	fatal_error(EINVAL, "erl_drv_tsd_key_destroy()");

    if (used_tsd_keys[key] != no_name)
	erts_free(ERTS_ALC_T_DRV_TSD, used_tsd_keys[key]);

    used_tsd_keys[key] = NULL;
    if (next_tsd_key < 0 || key < next_tsd_key)
	next_tsd_key = key;

    erts_mtx_unlock(&tsd_mtx);
}


#define ERL_DRV_TSD__ (dtid->tsd)
#define ERL_DRV_TSD_LEN__ (dtid->tsd_len)

void
erl_drv_tsd_set(ErlDrvTSDKey key, void *data)
{
    struct ErlDrvTid_ *dtid = (struct ErlDrvTid_ *) erl_drv_thread_self();

    if (key < 0 || max_used_tsd_key < key || !used_tsd_keys[key])
	fatal_error(EINVAL, "erl_drv_tsd_set()");

    if (!ERL_DRV_TSD__) {
	ErlDrvTSDKey i;
	ERL_DRV_TSD_LEN__ = key + ERL_DRV_TSD_EXTRA;
	ERL_DRV_TSD__ = erts_alloc(ERTS_ALC_T_DRV_TSD,
				   sizeof(void *)*ERL_DRV_TSD_LEN__);
	for (i = 0; i < ERL_DRV_TSD_LEN__; i++)
	    ERL_DRV_TSD__[i] = NULL;
    }
    else if (ERL_DRV_TSD_LEN__ <= key) {
	ErlDrvTSDKey i = ERL_DRV_TSD_LEN__;
	ERL_DRV_TSD_LEN__ = key + ERL_DRV_TSD_EXTRA;
	ERL_DRV_TSD__ = erts_realloc(ERTS_ALC_T_DRV_TSD,
				     ERL_DRV_TSD__,
				     sizeof(void *)*ERL_DRV_TSD_LEN__);
	for (; i < ERL_DRV_TSD_LEN__; i++)
	    ERL_DRV_TSD__[i] = NULL;
    }
    ERL_DRV_TSD__[key] = data;
}

void *
erl_drv_tsd_get(ErlDrvTSDKey key)
{
    struct ErlDrvTid_ *dtid = ethr_tsd_get(tid_key);
    if (key < 0 || max_used_tsd_key < key || !used_tsd_keys[key])
	fatal_error(EINVAL, "erl_drv_tsd_get()");
    if (!dtid)
	return NULL;
    if (ERL_DRV_TSD_LEN__ <= key)
	return NULL;
    return ERL_DRV_TSD__[key];
}

#undef ERL_DRV_TSD_LEN__
#undef ERL_DRV_TSD__

ErlDrvThreadOpts *
erl_drv_thread_opts_create(char *name)
{
    ErlDrvThreadOpts *opts = erts_alloc_fnf(ERTS_ALC_T_DRV_THR_OPTS,
					    sizeof(ErlDrvThreadOpts));
    if (!opts)
	return NULL;
    opts->suggested_stack_size = -1;
    return opts;
}

void
erl_drv_thread_opts_destroy(ErlDrvThreadOpts *opts)
{
    if (!opts)
	fatal_error(EINVAL, "erl_drv_thread_opts_destroy()");
    erts_free(ERTS_ALC_T_DRV_THR_OPTS, opts);
}

int
erl_drv_thread_create(char *name,
		      ErlDrvTid *tid,
		      void* (*func)(void*),
		      void* arg,
		      ErlDrvThreadOpts *opts)
{
    int res;
    struct ErlDrvTid_ *dtid;
    ethr_thr_opts ethr_opts = ETHR_THR_OPTS_DEFAULT_INITER;
    ethr_thr_opts *use_opts;

    if (!opts && !name)
	use_opts = NULL;
    else {
	if(opts)
	    ethr_opts.suggested_stack_size = opts->suggested_stack_size;

        ethr_opts.name = name;
	use_opts = &ethr_opts;
    }

    dtid = erts_alloc_fnf(ERTS_ALC_T_DRV_TID,
			  (sizeof(struct ErlDrvTid_)
			   + (name ? sys_strlen(name) + 1 : 0)));
    if (!dtid)
	return ENOMEM;

    dtid->drv_thr = 1;
    dtid->func = func;
    dtid->arg = arg;
    dtid->tsd = NULL;
    dtid->tsd_len = 0;
    if (!name)
	dtid->name = no_name;
    else {
	dtid->name = ((char *) dtid) + sizeof(struct ErlDrvTid_);
	sys_strcpy(dtid->name, name);
    }
    res = ethr_thr_create(&dtid->tid, erl_drv_thread_wrapper, dtid, use_opts);

    if (res != 0) {
	erts_free(ERTS_ALC_T_DRV_TID, dtid);
	return res;
    }

    *tid = (ErlDrvTid) dtid;
    return 0;
}

char *
erl_drv_thread_name(ErlDrvTid tid)
{
    struct ErlDrvTid_ *dtid = (struct ErlDrvTid_ *) tid;
    return dtid ? dtid->name : NULL;
}


ErlDrvTid
erl_drv_thread_self(void)
{
    struct ErlDrvTid_ *dtid = ethr_tsd_get(tid_key);
    if (!dtid) {
	int res;
	/* This is a thread not spawned by this interface. thread_exit_handler()
	   will clean it up when it terminates. */
	dtid = erts_alloc(ERTS_ALC_T_DRV_TID, sizeof(struct ErlDrvTid_));
	dtid->drv_thr = 0; /* Not a driver thread */
	dtid->tid = ethr_self();
	dtid->func = NULL;
	dtid->arg = NULL;
	dtid->tsd = NULL;
	dtid->tsd_len = 0;
	dtid->name = no_name;
	res = ethr_tsd_set(tid_key, (void *) dtid);
	if (res != 0)
	    fatal_error(res, "erl_drv_thread_self()");
    }
    return (ErlDrvTid) dtid;
}

int
erl_drv_equal_tids(ErlDrvTid tid1, ErlDrvTid tid2)
{   
    int res;
    struct ErlDrvTid_ *dtid1 = (struct ErlDrvTid_ *) tid1;
    struct ErlDrvTid_ *dtid2 = (struct ErlDrvTid_ *) tid2;
    if (!dtid1 || !dtid2)
	fatal_error(EINVAL, "erl_drv_equal_tids()");

    res = dtid1 == dtid2;

    ASSERT(res
	   ? ethr_equal_tids(dtid1->tid, dtid2->tid)
	   : !ethr_equal_tids(dtid1->tid, dtid2->tid));

    return res;
}

void
erl_drv_thread_exit(void *res)
{
    struct ErlDrvTid_ *dtid = ethr_tsd_get(tid_key);
    if (dtid && dtid->drv_thr) {
	ethr_thr_exit(res);
	fatal_error(0, "erl_drv_thread_exit()");
    }
    fatal_error(EACCES, "erl_drv_thread_exit()");
}

int
erl_drv_thread_join(ErlDrvTid tid, void **respp)
{
    int res;
    struct ErlDrvTid_ *dtid = (struct ErlDrvTid_ *) tid;

    ASSERT(dtid);

    if (!dtid->drv_thr)
	return EINVAL;

    res = ethr_thr_join(dtid->tid, respp);
    if (res == 0)
	erts_free(ERTS_ALC_T_DRV_TID, dtid);
    return res;
}

#if defined(__DARWIN__)
extern int erts_darwin_main_thread_pipe[2];
extern int erts_darwin_main_thread_result_pipe[2];

int erl_drv_stolen_main_thread_join(ErlDrvTid tid, void **respp);
int erl_drv_steal_main_thread(char *name,
			      ErlDrvTid *dtid,
			      void* (*func)(void*),
			      void* arg,
			      ErlDrvThreadOpts *opts);


int
erl_drv_stolen_main_thread_join(ErlDrvTid tid, void **respp)
{
    void *dummy;
    void **x;
    if (respp == NULL)
	x = &dummy;
    else
	x = respp;
    read(erts_darwin_main_thread_result_pipe[0],x,sizeof(void *));
    return 0;
}

int
erl_drv_steal_main_thread(char *name,
			  ErlDrvTid *tid,
			  void* (*func)(void*),
			  void* arg,
			  ErlDrvThreadOpts *opts)
{
    char buff[sizeof(void* (*)(void*)) + sizeof(void *)];
    int buff_sz = sizeof(void* (*)(void*)) + sizeof(void *);
    /*struct ErlDrvTid_ *dtid;

    dtid = erts_alloc_fnf(ERTS_ALC_T_DRV_TID,
			  (sizeof(struct ErlDrvTid_)
			   + (name ? sys_strlen(name) + 1 : 0)));
    if (!dtid)
	return ENOMEM;
    sys_memset(dtid,0,sizeof(ErlDrvTid_));
    dtid->tid = (void * ) -1;
    dtid->drv_thr = 1;
    dtid->func = func;
    dtid->arg = arg;
    dtid->tsd = NULL;
    dtid->tsd_len = 0;
    dtid->name = no_name;
    *tid = (ErlDrvTid) dtid;
    */
    *tid = NULL;
    /* Ignore options and name... */
    
    sys_memcpy(buff,&func,sizeof(void* (*)(void*)));
    sys_memcpy(buff + sizeof(void* (*)(void*)),&arg,sizeof(void *));
    write(erts_darwin_main_thread_pipe[1],buff,buff_sz);
    return 0;
}

#endif
