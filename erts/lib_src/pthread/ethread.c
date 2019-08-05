/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2010-2017. All Rights Reserved.
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
 * Description: Pthread implementation of the ethread library
 * Author: Rickard Green
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#define ETHR_CHILD_WAIT_SPIN_COUNT 4000

#include <stdio.h>
#ifdef ETHR_TIME_WITH_SYS_TIME
#  include <time.h>
#  include <sys/time.h>
#else
#  ifdef ETHR_HAVE_SYS_TIME_H
#    include <sys/time.h>
#  else
#    include <time.h>
#  endif
#endif
#include <sys/types.h>
#include <unistd.h>
#include <signal.h>
#include <string.h>

#include <limits.h>

#define ETHR_INLINE_FUNC_NAME_(X) X ## __
#define ETHREAD_IMPL__

#include "ethread.h"
#undef ETHR_INCLUDE_MONOTONIC_CLOCK__
#define ETHR_INCLUDE_MONOTONIC_CLOCK__
#include "ethr_internal.h"

#ifndef ETHR_HAVE_ETHREAD_DEFINES
#error Missing configure defines
#endif

pthread_key_t ethr_ts_event_key__;
static int child_wait_spin_count;

/*
 * --------------------------------------------------------------------------
 * Static functions
 * --------------------------------------------------------------------------
 */

static void thr_exit_cleanup(void)
{
    ethr_run_exit_handlers__();
}


/* Argument passed to thr_wrapper() */
typedef struct {
    ethr_atomic32_t result;
    ethr_ts_event *tse;
    void *(*thr_func)(void *);
    void *arg;
    void *prep_func_res;
    size_t stacksize;
    char *name;
    char name_buff[16];
} ethr_thr_wrap_data__;

static void *thr_wrapper(void *vtwd)
{
    ethr_sint32_t result;
    char c;
    void *res;
    ethr_thr_wrap_data__ *twd = (ethr_thr_wrap_data__ *) vtwd;
    void *(*thr_func)(void *) = twd->thr_func;
    void *arg = twd->arg;
    ethr_ts_event *tsep = NULL;

    ethr_set_stacklimit__(&c, twd->stacksize);

    result = (ethr_sint32_t) ethr_make_ts_event__(&tsep);

    if (result == 0) {
	tsep->iflgs |= ETHR_TS_EV_ETHREAD;
	if (ethr_thr_child_func__)
	    ethr_thr_child_func__(twd->prep_func_res);
    }

    tsep = twd->tse; /* We aren't allowed to follow twd after
			result has been set! */
    if (twd->name)
        ethr_setname(twd->name);

    ethr_atomic32_set(&twd->result, result);

    ethr_event_set(&tsep->event);

    res = result == 0 ? (*thr_func)(arg) : NULL;

    thr_exit_cleanup();
    return res;
}

/* internal exports */

int ethr_set_tse__(ethr_ts_event *tsep)
{
    return pthread_setspecific(ethr_ts_event_key__, (void *) tsep);
}

ethr_ts_event *ethr_get_tse__(void)
{
    return pthread_getspecific(ethr_ts_event_key__);
}

#if defined(ETHR_PPC_RUNTIME_CONF__)

#include <sys/wait.h>

static void
handle_lwsync_sigill(int signum)
{
    _exit(1);
}

static int
ppc_init__(void)
{
    int pid;

    /* If anything what so ever failes we assume no lwsync for safety */
    ethr_runtime__.conf.have_lwsync = 0;

    /*
     * We perform the lwsync test (which might cause an illegal
     * instruction signal) in a separate process in order to be
     * completely certain that we do not mess up our own state.
     */
    pid = fork();
    if (pid == 0) {
	struct sigaction act, oact;

	sigemptyset(&act.sa_mask);
	act.sa_flags = SA_RESETHAND;
	act.sa_handler = handle_lwsync_sigill;
	if (sigaction(SIGILL, &act, &oact) != 0)
	    _exit(2);

	__asm__ __volatile__ ("lwsync\n\t" : : : "memory");

	_exit(0);
    }

    if (pid != -1) {
	while (1) {
	    int status, res;
	    res = waitpid(pid, &status, 0);
	    if (res == pid) {
		if (WIFEXITED(status) && WEXITSTATUS(status) == 0)
		    ethr_runtime__.conf.have_lwsync = 1;
		break;
	    }
	}
    }
    return 0;
}

#endif

#if defined(ETHR_X86_RUNTIME_CONF__)

void
ethr_x86_cpuid__(int *eax, int *ebx, int *ecx, int *edx)
{
#if ETHR_SIZEOF_PTR == 4
    int have_cpuid;
    /*
     * If it is possible to toggle eflags bit 21,
     * we have the cpuid instruction.
     */
    __asm__ ("pushf\n\t"
             "popl %%eax\n\t"
             "movl %%eax, %%ecx\n\t"
             "xorl $0x200000, %%eax\n\t"
             "pushl %%eax\n\t"
             "popf\n\t"
             "pushf\n\t"
             "popl %%eax\n\t"
             "movl $0x0, %0\n\t"
             "xorl %%ecx, %%eax\n\t"
             "jz 1f\n\t"
	     "movl $0x1, %0\n\t"
             "1:\n\t"
             : "=r"(have_cpuid)
             :
             : "%eax", "%ecx", "cc");
    if (!have_cpuid) {
	*eax = *ebx = *ecx = *edx = 0;
	return;
    }
#endif
#if ETHR_SIZEOF_PTR == 4 && defined(__PIC__) && __PIC__
    /*
     * When position independet code is used in 32-bit mode, the B register
     * is used for storage of global offset table address, and we may not
     * use it as input or output in an asm. We need to save and restore the
     * B register explicitly (for some reason gcc doesn't provide this
     * service to us).
     */
    __asm__ ("pushl %%ebx\n\t"
	     "cpuid\n\t"
	     "movl %%ebx, %1\n\t"
	     "popl %%ebx\n\t"
	     : "=a"(*eax), "=r"(*ebx), "=c"(*ecx), "=d"(*edx)
	     : "0"(*eax)
	     : "cc");
#else
    __asm__ ("cpuid\n\t"
	     : "=a"(*eax), "=b"(*ebx), "=c"(*ecx), "=d"(*edx)
	     : "0"(*eax)
	     : "cc");
#endif
}

#endif /* ETHR_X86_RUNTIME_CONF__ */

#ifdef ETHR_HAVE_ETHR_GET_MONOTONIC_TIME
static void init_get_monotonic_time(void);
#endif

/*
 * --------------------------------------------------------------------------
 * Exported functions
 * --------------------------------------------------------------------------
 */

int
ethr_init(ethr_init_data *id)
{
    int res;

    if (!ethr_not_inited__)
	return EINVAL;

    ethr_not_inited__ = 0;

#if defined(ETHR_PPC_RUNTIME_CONF__)
    res = ppc_init__();
    if (res != 0)
	goto error;
#endif

#ifdef ETHR_HAVE_ETHR_GET_MONOTONIC_TIME
    init_get_monotonic_time();
#endif

    res = ethr_init_common__(id);
    if (res != 0)
	goto error;

    child_wait_spin_count = ETHR_CHILD_WAIT_SPIN_COUNT;
    if (erts_get_cpu_configured(ethr_cpu_info__) == 1)
	child_wait_spin_count = 0;

    res = pthread_key_create(&ethr_ts_event_key__, ethr_ts_event_destructor__);
    if (res != 0)
	goto error;

    return 0;
 error:
    ethr_not_inited__ = 1;
    return res;

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

int
ethr_thr_create(ethr_tid *tid, void * (*func)(void *), void *arg,
		ethr_thr_opts *opts)
{
    ethr_thr_wrap_data__ twd;
    pthread_attr_t attr;
    int res, dres;
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

    ethr_atomic32_init(&twd.result, (ethr_sint32_t) -1);
    twd.tse = ethr_get_ts_event();
    twd.thr_func = func;
    twd.arg = arg;
    twd.stacksize = 0;

    if (opts && opts->name) {
        snprintf(twd.name_buff, 16, "%s", opts->name);
	twd.name = twd.name_buff;
    } else
        twd.name = NULL;

    res = pthread_attr_init(&attr);
    if (res != 0)
	return res;

    /* Error cleanup needed after this point */

    /* Schedule child thread in system scope (if possible) ... */
    res = pthread_attr_setscope(&attr, PTHREAD_SCOPE_SYSTEM);
    if (res != 0 && res != ENOTSUP)
	goto error;

    if (use_stack_size >= 0) {
	size_t suggested_stack_size = (size_t) use_stack_size;
	size_t stack_size;
#ifdef ETHR_DEBUG
	suggested_stack_size /= 2; /* Make sure we got margin */
#endif
        stack_size = ETHR_KW2B(suggested_stack_size);
        stack_size = ETHR_PAGE_ALIGN(stack_size);
        stack_size += ethr_pagesize__; /* For possible system usage */
#ifdef ETHR_STACK_GUARD_SIZE
	/* The guard is at least on some platforms included in the stack size
	   passed when creating threads */
	stack_size += ETHR_STACK_GUARD_SIZE;
#endif
	if (stack_size < ethr_min_stack_size__)
	    stack_size = ethr_min_stack_size__;
	else if (stack_size > ethr_max_stack_size__)
	    stack_size = ethr_max_stack_size__;
	(void) pthread_attr_setstacksize(&attr, stack_size);
        twd.stacksize = stack_size;
        twd.stacksize -= ethr_pagesize__; /* For possible system usage */
#ifdef ETHR_STACK_GUARD_SIZE
        twd.stacksize -= ETHR_STACK_GUARD_SIZE;
#endif
    }

#ifdef ETHR_STACK_GUARD_SIZE
    (void) pthread_attr_setguardsize(&attr, ETHR_STACK_GUARD_SIZE);
#endif

    /* Detached or joinable... */
    res = pthread_attr_setdetachstate(&attr,
				      (opts && opts->detached
				       ? PTHREAD_CREATE_DETACHED
				       : PTHREAD_CREATE_JOINABLE));
    if (res != 0)
	goto error;

    /* Call prepare func if it exist */
    if (ethr_thr_prepare_func__)
	twd.prep_func_res = ethr_thr_prepare_func__();
    else
	twd.prep_func_res = NULL;

    res = pthread_create((pthread_t *) tid, &attr, thr_wrapper, (void*) &twd);

    if (res == 0) {
	int spin_count = child_wait_spin_count;

	/* Wait for child to initialize... */
	while (1) {
	    ethr_sint32_t result;
	    ethr_event_reset(&twd.tse->event);

	    result = ethr_atomic32_read(&twd.result);
	    if (result == 0)
		break;

	    if (result > 0) {
		res = (int) result;
		goto error;
	    }

	    res = ethr_event_swait(&twd.tse->event, spin_count);
	    if (res != 0 && res != EINTR)
		goto error;
	    spin_count = 0;
	}
    }

    /* Cleanup... */

 error:
    dres = pthread_attr_destroy(&attr);
    if (res == 0)
	res = dres;
    if (ethr_thr_parent_func__)
	ethr_thr_parent_func__(twd.prep_func_res);
    return res;
}

int
ethr_thr_join(ethr_tid tid, void **res)
{
#if ETHR_XCHK
    if (ethr_not_inited__) {
	ETHR_ASSERT(0);
	return EACCES;
    }
#endif
    return pthread_join((pthread_t) tid, res);
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
    return pthread_detach((pthread_t) tid);
}

void
ethr_thr_exit(void *res)
{
#if ETHR_XCHK
    if (ethr_not_inited__) {
	ETHR_ASSERT(0);
	return;
    }
#endif
    thr_exit_cleanup();
    pthread_exit(res);
}

ethr_tid
ethr_self(void)
{
    return (ethr_tid) pthread_self();
}

int
ethr_getname(ethr_tid tid, char *buf, size_t len)
{
#if defined(ETHR_HAVE_PTHREAD_GETNAME_NP_3)
    return pthread_getname_np((pthread_t) tid, buf, len);
#elif defined(ETHR_HAVE_PTHREAD_GETNAME_NP_2)
    return pthread_getname_np((pthread_t) tid, buf);
#else
    return ENOSYS;
#endif
}

void
ethr_setname(char *name)
{
#if defined(ETHR_HAVE_PTHREAD_SETNAME_NP_2) 
    pthread_setname_np(ethr_self(), name);
#elif defined(ETHR_HAVE_PTHREAD_SET_NAME_NP_2)
    pthread_set_name_np(ethr_self(), name);
#elif defined(ETHR_HAVE_PTHREAD_SETNAME_NP_1)
    pthread_setname_np(name);
#endif
}

int
ethr_equal_tids(ethr_tid tid1, ethr_tid tid2)
{
    return pthread_equal((pthread_t) tid1, (pthread_t) tid2);
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

/*
 * Thread specific data
 */

int
ethr_tsd_key_create(ethr_tsd_key *keyp, char *keyname)
{
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
    return pthread_key_create((pthread_key_t *) keyp, NULL);
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
    return pthread_key_delete((pthread_key_t) key);
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
    return pthread_setspecific((pthread_key_t) key, value);
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
    return pthread_getspecific((pthread_key_t) key);
}

/*
 * Signal functions
 */

#if ETHR_HAVE_ETHR_SIG_FUNCS

int ethr_sigmask(int how, const sigset_t *set, sigset_t *oset)
{
#if ETHR_XCHK
    if (ethr_not_inited__) {
	ETHR_ASSERT(0);
	return EACCES;
    }
    if (!set && !oset) {
	ETHR_ASSERT(0);
	return EINVAL;
    }
#endif
#if defined(__ANDROID__)
   return sigprocmask(how, set, oset);
#else 
   return pthread_sigmask(how, set, oset);
#endif    
}

int ethr_sigwait(const sigset_t *set, int *sig)
{
#if ETHR_XCHK
    if (ethr_not_inited__) {
	ETHR_ASSERT(0);
	return EACCES;
    }
    if (!set || !sig) {
	ETHR_ASSERT(0);
	return EINVAL;
    }
#endif
    if (sigwait(set, sig) < 0)
	return errno;
    return 0;
}

int ethr_kill(const ethr_tid tid, const int sig)
{
#if ETHR_XCHK
    if (ethr_not_inited__) {
	ETHR_ASSERT(0);
	return EACCES;
    }
#endif
    return pthread_kill((const pthread_t)tid, sig);
}

#endif /* #if ETHR_HAVE_ETHR_SIG_FUNCS */

#ifdef ETHR_HAVE_ETHR_GET_MONOTONIC_TIME

static int broken_get_monotonic_time;

#if defined(ETHR_HAVE_CLOCK_GETTIME_MONOTONIC)
#  ifndef ETHR_MONOTONIC_CLOCK_ID
#    error ETHR_MONOTONIC_CLOCK_ID should have been defined
#  endif

ethr_sint64_t
ethr_get_monotonic_time(void)
{
    ethr_sint64_t time;
    struct timespec ts;

    if (broken_get_monotonic_time)
	return (ethr_sint64_t) 0;

    if (0 != clock_gettime(ETHR_MONOTONIC_CLOCK_ID, &ts))
	ETHR_FATAL_ERROR__(errno);

    time = (ethr_sint64_t) ts.tv_sec;
    time *= (ethr_sint64_t) 1000*1000*1000;
    time += (ethr_sint64_t) ts.tv_nsec;
    return time;
}

#elif defined(ETHR_HAVE_MACH_CLOCK_GET_TIME)
#  ifndef ETHR_MONOTONIC_CLOCK_ID
#    error ETHR_MONOTONIC_CLOCK_ID should have been defined
#  endif

ethr_sint64_t
ethr_get_monotonic_time(void)
{
    ethr_sint64_t time;
    kern_return_t res;
    clock_serv_t clk_srv;
    mach_timespec_t time_spec;

    if (broken_get_monotonic_time)
	return (ethr_sint64_t) 0;
    
    errno = EFAULT;
    host_get_clock_service(mach_host_self(),
			   ETHR_MONOTONIC_CLOCK_ID,
			   &clk_srv);
    res = clock_get_time(clk_srv, &time_spec);
    if (res != KERN_SUCCESS)
	ETHR_FATAL_ERROR__(errno);
    mach_port_deallocate(mach_task_self(), clk_srv);

    time = (ethr_sint64_t) time_spec.tv_sec;
    time *= (ethr_sint64_t) 1000*1000*1000;
    time += (ethr_sint64_t) time_spec.tv_nsec;
    return time;
}

#elif defined(ETHR_HAVE_GETHRTIME)

ethr_sint64_t
ethr_get_monotonic_time(void)
{
    if (broken_get_monotonic_time)
	return (ethr_sint64_t) 0;
    return (ethr_sint64_t) gethrtime();
}

#else
#error missing monotonic clock
#endif

int
ethr_get_monotonic_time_is_broken(void)
{
    return broken_get_monotonic_time;
}

#include <string.h>
#include <ctype.h>
#include <sys/utsname.h>

static void
init_get_monotonic_time(void)
{
    struct utsname uts;
    int vsn[3];
    int i;
    char *c;

    broken_get_monotonic_time = 0;
    
    (void) uname(&uts);
    
    for (c = uts.sysname; *c; c++) {
	if (isupper((int) *c))
	    *c = tolower((int) *c);
    }

    c = uts.release;
    for (i = 0; i < sizeof(vsn)/sizeof(int); i++) {
	if (!isdigit((int) *c))
	    vsn[i] = 0;
	else {
	    char *c2 = c;
	    do {
		c2++;
	    } while (isdigit((int) *c2));
	    *c2 = '\0';
	    vsn[i] = atoi(c);
	    c = c2;
	    c++;
	}
    }

    if (strcmp("linux", uts.sysname) == 0) {
	if (vsn[0] < 2
	    || (vsn[0] == 2 && vsn[1] < 6)
	    || (vsn[0] == 2 && vsn[1] == 6 && vsn[2] < 33)) {
	    broken_get_monotonic_time = 1;
	}
    }
    else if (strcmp("sunos", uts.sysname) == 0) {
	if ((vsn[0] < 5
	     || (vsn[0] == 5 && vsn[1] < 8))
#if defined(HAVE_SYSCONF) && defined(_SC_NPROCESSORS_CONF)
	    && sysconf(_SC_NPROCESSORS_CONF) > 1
#endif
	    ) {
	    broken_get_monotonic_time = 1;
	}
    }

}
			

#endif /* ETHR_HAVE_ETHR_GET_MONOTONIC_TIME */

ETHR_IMPL_NORETURN__
ethr_abort__(void)
{
    abort();
}
