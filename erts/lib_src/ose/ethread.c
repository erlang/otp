/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2010-2011. All Rights Reserved.
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
 * Description: OSE implementation of the ethread library
 * Author: Lukas Larsson
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

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

#include <limits.h>

#define ETHR_INLINE_FUNC_NAME_(X) X ## __
#define ETHREAD_IMPL__

#include "ethread.h"
#include "ethr_internal.h"

#include "erl_printf.h"
#include "efs.h"

#include "ose_spi.h"

#include "string.h"

#ifndef ETHR_HAVE_ETHREAD_DEFINES
#error Missing configure defines
#endif

#define ETHR_INVALID_TID_ID -1

static ethr_tid main_thr_tid;
static const char* own_tid_key = "ethread_own_tid";
char* ethr_ts_event_key__ = "ethread_tse";

#define ETHREADWRAPDATASIG 1

/* Init data sent to thr_wrapper() */
typedef struct {
    SIGSELECT sig_no;
    ethr_ts_event *tse;
    ethr_tid *tid;
    ethr_sint32_t result;
    void *(*thr_func)(void *);
    void *arg;
    void *prep_func_res;
    const char *name;
} ethr_thr_wrap_data__;

union SIGNAL {
  SIGSELECT sig_no;
  ethr_thr_wrap_data__ data;
};

#define ETHR_GET_OWN_TID__	((ethr_tid *) get_envp(current_process(),\
						       own_tid_key))

/*
 * --------------------------------------------------------------------------
 * Static functions
 * --------------------------------------------------------------------------
 */

static PROCESS blockId(void) {
   static PROCESS bid = (PROCESS)0;

   /* For now we only use the same block. */
   /*   if (bid == 0) {
      bid = create_block("Erlang-VM", 0, 0, 0, 0);
   }
   return bid; */
   return 0;
}

static void thr_exit_cleanup(ethr_tid *tid, void *res)
{

     ETHR_ASSERT(tid == ETHR_GET_OWN_TID__);

     tid->res = res;

     ethr_run_exit_handlers__();
     ethr_ts_event_destructor__((void *) ethr_get_tse__());
}

//static OS_PROCESS(thr_wrapper);
static OS_PROCESS(thr_wrapper)
{
    ethr_tid my_tid;
    ethr_sint32_t result;
    void *res;
    void *(*thr_func)(void *);
    void *arg;
    ethr_ts_event *tsep = NULL;

#ifdef DEBUG
    {
       PROCESS pid = current_process();

       const char *execMode;

       PROCESS     bid      = get_bid(pid);

       /* In the call below, 16 is a secret number provided by frbr that makes
        * the function return current domain. */
       OSADDRESS   domain   = get_pid_info(current_process(), 16);

#ifdef HAVE_OSE_SPI_H
       execMode = get_pid_info(pid, OSE_PI_SUPERVISOR)
          ? "Supervisor"
          : "User";
#else
       execMode = "unknown";
#endif

       fprintf(stderr,"[0x%x] New process. Bid:0x%x, domain:%d, exec mode:%s\n",
               current_process(), bid, domain, execMode);
    }
#endif

    {
       SIGSELECT sigsel[] = {1,ETHREADWRAPDATASIG};
       union SIGNAL *init_msg = receive(sigsel);

       thr_func = init_msg->data.thr_func;
       arg      = init_msg->data.arg;

       result = (ethr_sint32_t) ethr_make_ts_event__(&tsep);

       if (result == 0) {
          tsep->iflgs |= ETHR_TS_EV_ETHREAD;
          my_tid = *init_msg->data.tid;
          set_envp(current_process(), own_tid_key, (OSADDRESS)&my_tid);
          if (ethr_thr_child_func__)
             ethr_thr_child_func__(init_msg->data.prep_func_res);
       }

       init_msg->data.result = result;

       send(&init_msg,sender(&init_msg));
    }

    /* pthread mutex api says we have to do this */
    /*erts_fprintf(stderr, "(%s / %d) curr_pid 0x%x / signal_fsem to 0x%x (fsem val %d)\n",
      __FUNCTION__, __LINE__, current_process(), current_process(), get_fsem(current_process()));*/
    signal_fsem(current_process());
    ETHR_ASSERT(get_fsem(current_process()) == 0);

    res = result == 0 ? (*thr_func)(arg) : NULL;

    thr_exit_cleanup(&my_tid, res);
}

/* internal exports */

int ethr_set_tse__(ethr_ts_event *tsep)
{
    return set_envp(current_process(),ethr_ts_event_key__, (OSADDRESS) tsep);
}

ethr_ts_event *ethr_get_tse__(void)
{
  return (ethr_ts_event *) get_envp(current_process(),ethr_ts_event_key__);
}

#if defined(ETHR_PPC_RUNTIME_CONF__)

static int
ppc_init__(void)
{
    int pid;


    ethr_runtime__.conf.have_lwsync = 0;

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
             "jz no_cpuid\n\t"
	     "movl $0x1, %0\n\t"
             "no_cpuid:\n\t"
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


#if defined(ETHR_PPC_RUNTIME_CONF__)
    res = ppc_init__();
    if (res != 0)
	goto error;
#endif

    res = ethr_init_common__(id);
    if (res != 0)
	goto error;

    main_thr_tid.id = current_process();
    main_thr_tid.tsd_key_index = 0;

    set_envp(current_process(),own_tid_key,(OSADDRESS)&main_thr_tid);
        /*erts_fprintf(stderr, "(%s / %d) curr_pid 0x%x / signal_fsem to 0x%x (fsem_val %d)\n",
                __FUNCTION__, __LINE__, current_process(), current_process(), get_fsem(current_process()));*/
    signal_fsem(current_process());

    ETHR_ASSERT(&main_thr_tid == ETHR_GET_OWN_TID__);

    ethr_not_inited__ = 0;

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
    int res;
    int use_stack_size = (opts && opts->suggested_stack_size >= 0
			  ? opts->suggested_stack_size
			  : 0x200 /* Use system default */);
    union SIGNAL *init_msg;
    SIGSELECT sigsel[] = {1,ETHREADWRAPDATASIG};
    void *prep_func_res;

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

    if (use_stack_size >= 0) {
	size_t suggested_stack_size = (size_t) use_stack_size;
	size_t stack_size;
#ifdef ETHR_DEBUG
	suggested_stack_size /= 2; /* Make sure we got margin */
#endif
#ifdef ETHR_STACK_GUARD_SIZE
	/* The guard is at least on some platforms included in the stack size
	   passed when creating threads */
	suggested_stack_size += ETHR_B2KW(ETHR_STACK_GUARD_SIZE);
#endif

	if (suggested_stack_size < ethr_min_stack_size__)
	    stack_size = ETHR_KW2B(ethr_min_stack_size__);
	else if (suggested_stack_size > ethr_max_stack_size__)
	    stack_size = ETHR_KW2B(ethr_max_stack_size__);
	else
	    stack_size = ETHR_PAGE_ALIGN(ETHR_KW2B(suggested_stack_size));
	use_stack_size = stack_size;
    }

    init_msg = alloc(sizeof(ethr_thr_wrap_data__), ETHREADWRAPDATASIG);

    /* Call prepare func if it exist */
    if (ethr_thr_prepare_func__)
	init_msg->data.prep_func_res = ethr_thr_prepare_func__();
    else
	init_msg->data.prep_func_res = NULL;

    /*erts_fprintf(stderr, "creating process %s / stack %d\n", opts->name, use_stack_size);*/

#if 0
    ramlog_printf("[0x%x] process '%s', coreNo = %u\n",
                  current_process(), opts->name, opts->coreNo);
#endif
    tid->id = create_process(/*OS_PRI_PROC*/OS_BG_PROC, opts->name, thr_wrapper,
			     use_stack_size, /*opts->prio+5*/31, 0,
                             blockId(), NULL, 0, 0);

    /* For now we do not attempt to bind schedulers to different cores.
      if (ose_bind_process(tid->id, opts->coreNo)) {
       printf("[0x%x] Binding pid 0x%x (%s) to core no %u.\n",
              current_process(), tid->id, opts->name, opts->coreNo);
	      }*/

    /*FIXME!!! Normally this shouldn't be used in shared mode. Still there is
     * a problem with stdin fd in fd_ processes which should be further
     * investigated */
    efs_clone(tid->id);

    tid->tsd_key_index = 0;
    tid->res = NULL;

    init_msg->data.tse      = ethr_get_ts_event();
    init_msg->data.thr_func = func;
    init_msg->data.arg      = arg;
    init_msg->data.tid      = tid;
    init_msg->data.name     = opts->name;

    send(&init_msg, tid->id);

    start(tid->id);
    init_msg = receive(sigsel);

    res = init_msg->data.result;
    prep_func_res = init_msg->data.prep_func_res;

    free_buf(&init_msg);
    /* Cleanup... */

    if (ethr_thr_parent_func__)
	ethr_thr_parent_func__(prep_func_res);

    return res;
}

int
ethr_thr_join(ethr_tid tid, void **res)
{
    SIGSELECT sigsel[] = {1,OS_ATTACH_SIG};
#if ETHR_XCHK
    if (ethr_not_inited__) {
	ETHR_ASSERT(0);
	return EACCES;
    }
#endif

    if (tid.id == ETHR_INVALID_TID_ID)
      return EINVAL;

    attach(NULL,tid.id);
    receive(sigsel);

    if (res)
      *res = tid.res;

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
	kill_proc(current_process());
    }
    thr_exit_cleanup(tid, res);
    /* Harakiri possible? */
    kill_proc(current_process());
}

ethr_tid
ethr_self(void)
{
    ethr_tid *tid;
#if ETHR_XCHK
    if (ethr_not_inited__) {
      ethr_tid dummy_tid = {ETHR_INVALID_TID_ID, 0, NULL};
	ETHR_ASSERT(0);
	return dummy_tid;
    }
#endif
    tid = ETHR_GET_OWN_TID__;
    if (!tid) {
	ethr_tid dummy_tid = {ETHR_INVALID_TID_ID, 0, NULL};
	return dummy_tid;
    }
    return *tid;
}

int
ethr_equal_tids(ethr_tid tid1, ethr_tid tid2)
{
    return tid1.id == tid2.id && tid1.id != ETHR_INVALID_TID_ID;
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

    ose_create_ppdata(keyname,keyp);

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
    /* Not possible to delete ppdata */

    return 0;
}

int
ethr_tsd_set(ethr_tsd_key key, void *value)
{
    void **ppdp;
#if ETHR_XCHK
    if (ethr_not_inited__) {
	ETHR_ASSERT(0);
	return EACCES;
    }
#endif
    ppdp = (void **)ose_get_ppdata(key);
    *ppdp = value;
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
    return *(void**)ose_get_ppdata(key);
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
  return pthread_sigmask(how, set, oset);
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

#endif /* #if ETHR_HAVE_ETHR_SIG_FUNCS */

ETHR_IMPL_NORETURN__
ethr_abort__(void)
{
    abort();
}
