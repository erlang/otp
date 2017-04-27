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
 * Description: Windows native threads implementation of the ethread library
 * Author: Rickard Green
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#define ETHR_CHILD_WAIT_SPIN_COUNT 4000

#undef WIN32_LEAN_AND_MEAN
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <process.h>
#include <winerror.h>
#include <stdio.h>
#include <limits.h>
#include <intrin.h>

#define ETHR_INLINE_FUNC_NAME_(X) X ## __
#define ETHREAD_IMPL__

#include "ethread.h"
#include "ethr_internal.h"

#ifndef ETHR_HAVE_ETHREAD_DEFINES
#error Missing configure defines
#endif

/* Argument passed to thr_wrapper() */
typedef struct {
    ethr_tid *tid;
    ethr_atomic32_t result;
    ethr_ts_event *tse;
    void *(*thr_func)(void *);
    void *arg;
    void *prep_func_res;
    size_t stacksize;
} ethr_thr_wrap_data__;

#define ETHR_INVALID_TID_ID -1

struct ethr_join_data_ {
    HANDLE handle;
    void *res;
};

static ethr_atomic_t thread_id_counter;
static DWORD own_tid_key;
static ethr_tid main_thr_tid;
static int child_wait_spin_count;

DWORD ethr_ts_event_key__;

#define ETHR_GET_OWN_TID__	((ethr_tid *) TlsGetValue(own_tid_key))

/*
 * --------------------------------------------------------------------------
 * Static functions
 * --------------------------------------------------------------------------
 */

static void thr_exit_cleanup(ethr_tid *tid, void *res)
{

    ETHR_ASSERT(tid == ETHR_GET_OWN_TID__);

    if (tid->jdata)
	tid->jdata->res = res;

    ethr_run_exit_handlers__();
    ethr_ts_event_destructor__((void *) ethr_get_tse__());
}

static unsigned __stdcall thr_wrapper(LPVOID vtwd)
{
    char c;
    ethr_tid my_tid;
    ethr_sint32_t result;
    void *res;
    ethr_thr_wrap_data__ *twd = (ethr_thr_wrap_data__ *) vtwd;
    void *(*thr_func)(void *) = twd->thr_func;
    void *arg = twd->arg;
    ethr_ts_event *tsep = NULL;

    ethr_set_stacklimit__(&c, twd->stacksize);

    result = (ethr_sint32_t) ethr_make_ts_event__(&tsep);

    if (result == 0) {
	tsep->iflgs |= ETHR_TS_EV_ETHREAD;
	my_tid = *twd->tid;
	if (!TlsSetValue(own_tid_key, (LPVOID) &my_tid)) {
	    result = (ethr_sint32_t) ethr_win_get_errno__();
	    ethr_free_ts_event__(tsep);
	}
	else {
	    if (ethr_thr_child_func__)
		ethr_thr_child_func__(twd->prep_func_res);
	}
    }

    tsep = twd->tse; /* We aren't allowed to follow twd after
			result has been set! */

    ethr_atomic32_set(&twd->result, result);

    ethr_event_set(&tsep->event);

    res = result == 0 ? (*thr_func)(arg) : NULL;

    thr_exit_cleanup(&my_tid, res);
    return 0;
}

/* internal exports */

int
ethr_win_get_errno__(void)
{
    return erts_get_last_win_errno();
}

int ethr_set_tse__(ethr_ts_event *tsep)
{
    return (TlsSetValue(ethr_ts_event_key__, (LPVOID) tsep)
	    ? 0
	    : ethr_win_get_errno__());
}

ethr_ts_event *ethr_get_tse__(void)
{
    return (ethr_ts_event *) TlsGetValue(ethr_ts_event_key__);
}

ETHR_IMPL_NORETURN__
ethr_abort__(void)
{
#if 1
    DebugBreak();
#else
    abort();
#endif
}

#if defined(ETHR_X86_RUNTIME_CONF__)

#pragma intrinsic(__cpuid)

void
ethr_x86_cpuid__(int *eax, int *ebx, int *ecx, int *edx)
{
    int CPUInfo[4];

    __cpuid(CPUInfo, *eax);

    *eax = CPUInfo[0];
    *ebx = CPUInfo[1];
    *ecx = CPUInfo[2];
    *edx = CPUInfo[3];
}

#endif /* ETHR_X86_RUNTIME_CONF__ */

/*
 * ----------------------------------------------------------------------------
 * Exported functions
 * ----------------------------------------------------------------------------
 */

int
ethr_init(ethr_init_data *id)
{
#ifdef _WIN32_WINNT
    DWORD major = (_WIN32_WINNT >> 8) & 0xff;
    DWORD minor = _WIN32_WINNT & 0xff;
    OSVERSIONINFO os_version;
#endif
    int err = 0;
    unsigned long i;

    if (!ethr_not_inited__)
	return EINVAL;

    ethr_not_inited__ = 0;

#ifdef _WIN32_WINNT
    os_version.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);
    GetVersionEx(&os_version);
    if (os_version.dwPlatformId != VER_PLATFORM_WIN32_NT
	|| os_version.dwMajorVersion < major
	|| (os_version.dwMajorVersion == major
	    && os_version.dwMinorVersion < minor))
	return ENOTSUP;
#endif
    err = ethr_init_common__(id);
    if (err)
	goto error;

    own_tid_key = TlsAlloc();
    if (own_tid_key == TLS_OUT_OF_INDEXES)
	goto error;

    ethr_atomic_init(&thread_id_counter, 0);

    main_thr_tid.id = 0;
    main_thr_tid.jdata = NULL;

    if (!TlsSetValue(own_tid_key, (LPVOID) &main_thr_tid))
	goto error;

    ETHR_ASSERT(&main_thr_tid == ETHR_GET_OWN_TID__);

    ethr_ts_event_key__ = TlsAlloc();
    if (ethr_ts_event_key__ == TLS_OUT_OF_INDEXES)
	goto error;

    child_wait_spin_count = ETHR_CHILD_WAIT_SPIN_COUNT;
    if (erts_get_cpu_configured(ethr_cpu_info__) == 1)
	child_wait_spin_count = 0;

    return 0;

 error:
    ethr_not_inited__ = 1;
    if (err == 0)
	err = ethr_win_get_errno__();
    ETHR_ASSERT(err != 0);
    return err;
}

int
ethr_late_init(ethr_late_init_data *id)
{
    int res = ethr_late_init_common__(id);
    if (res != 0)
	return res;
    ethr_not_completely_inited__ = 0;
    return res;
}


/*
 * Thread functions.
 */

int
ethr_thr_create(ethr_tid *tid, void * (*func)(void *), void *arg,
		ethr_thr_opts *opts)
{
    HANDLE handle = INVALID_HANDLE_VALUE;
    int err = 0;
    ethr_thr_wrap_data__ twd;
    DWORD code;
    unsigned ID;
    unsigned stack_size = 0; /* 0 = system default */
    int use_stack_size = (opts && opts->suggested_stack_size >= 0
			  ? opts->suggested_stack_size
			  : -1 /* Use system default */);

#ifdef ETHR_MODIFIED_DEFAULT_STACK_SIZE
    if (use_stack_size < 0)
	use_stack_size = ETHR_MODIFIED_DEFAULT_STACK_SIZE;
#endif

#if ETHR_XCHK
    if (ethr_not_completely_inited__) {
	ETHR_ASSERT(0);
	return EACCES;
    }
    if (!tid || !func) {
	ETHR_ASSERT(0);
	return EINVAL;
    }
#endif

    do {
	tid->id = ethr_atomic_inc_read(&thread_id_counter);
    } while (tid->id == ETHR_INVALID_TID_ID);

    if (opts && opts->detached)
	tid->jdata = NULL;
    else {
	tid->jdata = ethr_mem__.std.alloc(sizeof(struct ethr_join_data_));
	if (!tid->jdata)
	    return ENOMEM;
	tid->jdata->handle = INVALID_HANDLE_VALUE;
	tid->jdata->res = NULL;
    }

    twd.stacksize = 0;

    if (use_stack_size >= 0) {
	size_t suggested_stack_size = (size_t) use_stack_size;
#ifdef ETHR_DEBUG
	suggested_stack_size /= 2; /* Make sure we got margin */
#endif
        stack_size = (unsigned) ETHR_PAGE_ALIGN(ETHR_KW2B(suggested_stack_size));

	if (stack_size < (unsigned) ethr_min_stack_size__)
	    stack_size = (unsigned) ethr_min_stack_size__;
	else if (stack_size > (unsigned) ethr_max_stack_size__)
	    stack_size = (unsigned) ethr_max_stack_size__;

        twd.stacksize = stack_size;
    }

    ethr_atomic32_init(&twd.result, -1);

    twd.tid = tid;
    twd.thr_func = func;
    twd.arg = arg;
    twd.tse = ethr_get_ts_event();

    /* Call prepare func if it exist */
    if (ethr_thr_prepare_func__)
	twd.prep_func_res = ethr_thr_prepare_func__();
    else
	twd.prep_func_res = NULL;

    /* spawn the thr_wrapper function */
    handle = (HANDLE) _beginthreadex(NULL, stack_size, thr_wrapper,
				     (LPVOID) &twd, 0, &ID);
    if (handle == (HANDLE) 0) {
	handle = INVALID_HANDLE_VALUE;
	goto error;
    }
    else {
	int spin_count = child_wait_spin_count;

	ETHR_ASSERT(handle != INVALID_HANDLE_VALUE);

	if (!tid->jdata)
	    CloseHandle(handle);
	else
	    tid->jdata->handle = handle;

	/* Wait for child to initialize... */
	while (1) {
	    ethr_sint32_t result;
	    int err;
	    ethr_event_reset(&twd.tse->event);

	    result = ethr_atomic32_read(&twd.result);
	    if (result == 0)
		break;

	    if (result > 0) {
		err = (int) result;
		goto error;
	    }

	    err = ethr_event_swait(&twd.tse->event, spin_count);
	    if (err && err != EINTR)
		goto error;
	    spin_count = 0;
	}
    }

    if (ethr_thr_parent_func__)
	ethr_thr_parent_func__(twd.prep_func_res);

    if (twd.tse)
	ethr_leave_ts_event(twd.tse);

    return 0;

 error:

    if (err == 0)
	err = ethr_win_get_errno__();
    ETHR_ASSERT(err != 0);

    if (ethr_thr_parent_func__)
	ethr_thr_parent_func__(twd.prep_func_res);

    if (handle != INVALID_HANDLE_VALUE) {
	WaitForSingleObject(handle, INFINITE);
	CloseHandle(handle);
    }

    if (tid->jdata) {
	ethr_mem__.std.free(tid->jdata);
	tid->jdata = NULL;
    }

    tid->id = ETHR_INVALID_TID_ID;

    if (twd.tse)
	ethr_leave_ts_event(twd.tse);

    return err;
}

int ethr_thr_join(ethr_tid tid, void **res)
{
    DWORD code;

#if ETHR_XCHK 
    if (ethr_not_inited__) {
	ETHR_ASSERT(0);
	return EACCES;
    }
#endif

    if (tid.id == ETHR_INVALID_TID_ID || !tid.jdata)
	return EINVAL;

    /* Wait for thread to terminate */
    code = WaitForSingleObject(tid.jdata->handle, INFINITE);
    if (code != WAIT_OBJECT_0)
	return ethr_win_get_errno__();

    CloseHandle(tid.jdata->handle);
    tid.jdata->handle = INVALID_HANDLE_VALUE;

    if (res)
	*res = tid.jdata->res;

    /*
     * User better not try to join or detach again; or
     * bad things will happen... (users responsibility)
     */

    ethr_mem__.std.free(tid.jdata);

    return 0;
}


int
ethr_thr_detach(ethr_tid tid)
{
#if ETHR_XCHK 
    if (ethr_not_inited__) {
	ETHR_ASSERT(0);
	return EACCES;
    }
#endif

    if (tid.id == ETHR_INVALID_TID_ID || !tid.jdata)
	return EINVAL;

    CloseHandle(tid.jdata->handle);
    tid.jdata->handle = INVALID_HANDLE_VALUE;

    /*
     * User better not try to join or detach again; or
     * bad things will happen... (users responsibility)
     */

    ethr_mem__.std.free(tid.jdata);

    return 0;
}


void
ethr_thr_exit(void *res)
{
    ethr_tid *tid;
#if ETHR_XCHK
    if (ethr_not_inited__) {
	ETHR_ASSERT(0);
	return;
    }
#endif
    tid = ETHR_GET_OWN_TID__;
    if (!tid) {
	ETHR_ASSERT(0);
	_endthreadex((unsigned) 0);
    }
    thr_exit_cleanup(tid, res);
    _endthreadex((unsigned) 0);
}

ethr_tid
ethr_self(void)
{
    ethr_tid *tid;
#if ETHR_XCHK
    if (ethr_not_inited__) {
	ethr_tid dummy_tid = {ETHR_INVALID_TID_ID, NULL};
	ETHR_ASSERT(0);
	return dummy_tid;
    }
#endif
    /* It is okay for non-ethreads (i.e. native win32 threads) to call
       ethr_self(). They will however be returned an invalid tid. */
    tid = ETHR_GET_OWN_TID__;
    if (!tid) {
	ethr_tid dummy_tid = {ETHR_INVALID_TID_ID, NULL};
	return dummy_tid;
    }
    return *tid;
}

/* getname and setname are not available on windows */
int
ethr_getname(ethr_tid tid, char *buf, size_t len)
{
    return ENOSYS;
}

void
ethr_setname(char *name)
{
    return;
}

int
ethr_equal_tids(ethr_tid tid1, ethr_tid tid2)
{
    /* An invalid tid does not equal any tid, not even an invalid tid */
    return tid1.id == tid2.id && tid1.id != ETHR_INVALID_TID_ID;
}

/*
 * Thread specific data
 */

int
ethr_tsd_key_create(ethr_tsd_key *keyp, char *keyname)
{
    DWORD key;
#if ETHR_XCHK
    if (ethr_not_inited__) {
	ETHR_ASSERT(0);
	return EACCES;
    }
    if (!keyp) {
	ETHR_ASSERT(0);
	return EINVAL;
    }
#endif
    key = TlsAlloc();
    if (key == TLS_OUT_OF_INDEXES)
	return ethr_win_get_errno__();
    *keyp = (ethr_tsd_key) key;
    return 0;
}

int
ethr_tsd_key_delete(ethr_tsd_key key)
{
#if ETHR_XCHK
    if (ethr_not_inited__) {
	ETHR_ASSERT(0);
	return EACCES;
    }
#endif
    if (!TlsFree((DWORD) key))
	return ethr_win_get_errno__();
    return 0;
}

int
ethr_tsd_set(ethr_tsd_key key, void *value)
{
#if ETHR_XCHK
    if (ethr_not_inited__) {
	ETHR_ASSERT(0);
	return EACCES;
    }
#endif
    if (!TlsSetValue((DWORD) key, (LPVOID) value))
	return ethr_win_get_errno__();
    return 0;
}

void *
ethr_tsd_get(ethr_tsd_key key)
{
#if ETHR_XCHK
    if (ethr_not_inited__) {
	ETHR_ASSERT(0);
	return NULL;
    }
#endif
    return (void *) TlsGetValue((DWORD) key);
}


/*
 * Thread specific events
 */

ethr_ts_event *
ethr_get_ts_event(void)
{
    return ethr_get_ts_event__();
}

void
ethr_leave_ts_event(ethr_ts_event *tsep)
{
    ethr_leave_ts_event__(tsep);
}

ethr_ts_event *
ethr_create_ts_event__(void)
{
    ethr_ts_event *tsep;
    ethr_make_ts_event__(&tsep);
    return tsep;
}
