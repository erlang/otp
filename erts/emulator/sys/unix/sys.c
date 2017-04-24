/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1996-2016. All Rights Reserved.
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

#ifdef ISC32
#define _POSIX_SOURCE
#define _XOPEN_SOURCE
#endif

#include <sys/times.h>		/* ! */
#include <time.h>
#include <signal.h>
#include <sys/wait.h>
#include <sys/uio.h>
#include <termios.h>
#include <ctype.h>
#include <sys/utsname.h>
#include <sys/select.h>

#ifdef ISC32
#include <sys/bsdtypes.h>
#endif

#include <termios.h>
#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif
#ifdef HAVE_SYS_IOCTL_H
#include <sys/ioctl.h>
#endif

#define ERTS_WANT_BREAK_HANDLING
#define WANT_NONBLOCKING    /* must define this to pull in defs from sys.h */
#include "sys.h"
#include "erl_thr_progress.h"

#if defined(__APPLE__) && defined(__MACH__) && !defined(__DARWIN__)
#define __DARWIN__ 1
#endif

#ifdef USE_THREADS
#include "erl_threads.h"
#endif

#include "erl_mseg.h"

extern char **environ;
erts_smp_rwmtx_t environ_rwmtx;

#define MAX_VSIZE 16		/* Max number of entries allowed in an I/O
				 * vector sock_sendv().
				 */
/*
 * Don't need global.h, but bif_table.h (included by bif.h),
 * won't compile otherwise
 */
#include "global.h"
#include "bif.h"

#include "erl_check_io.h"
#include "erl_cpu_topology.h"

extern int  driver_interrupt(int, int);
extern void do_break(void);

extern void erl_sys_args(int*, char**);

/* The following two defs should probably be moved somewhere else */

extern void erts_sys_init_float(void);


#ifdef DEBUG
static int debug_log = 0;
#endif

#ifdef ERTS_SMP
static erts_smp_atomic32_t have_prepared_crash_dump;
#define ERTS_PREPARED_CRASH_DUMP \
  ((int) erts_smp_atomic32_xchg_nob(&have_prepared_crash_dump, 1))
#else
static volatile int have_prepared_crash_dump;
#define ERTS_PREPARED_CRASH_DUMP \
  (have_prepared_crash_dump++)
#endif

erts_smp_atomic_t sys_misc_mem_sz;

#if defined(ERTS_SMP)
static void smp_sig_notify(int signum);
static int sig_notify_fds[2] = {-1, -1};

#ifdef ERTS_SYS_SUSPEND_SIGNAL
static int sig_suspend_fds[2] = {-1, -1};
#endif

#endif

jmp_buf erts_sys_sigsegv_jmp;

static int crashdump_companion_cube_fd = -1;

/********************* General functions ****************************/

/* This is used by both the drivers and general I/O, must be set early */
static int max_files = -1;

/* 
 * a few variables used by the break handler 
 */
#ifdef ERTS_SMP
erts_smp_atomic32_t erts_break_requested;
#define ERTS_SET_BREAK_REQUESTED \
  erts_smp_atomic32_set_nob(&erts_break_requested, (erts_aint32_t) 1)
#define ERTS_UNSET_BREAK_REQUESTED \
  erts_smp_atomic32_set_nob(&erts_break_requested, (erts_aint32_t) 0)
#else
volatile int erts_break_requested = 0;
#define ERTS_SET_BREAK_REQUESTED (erts_break_requested = 1)
#define ERTS_UNSET_BREAK_REQUESTED (erts_break_requested = 0)
#endif

#ifndef ERTS_SMP
static Eterm signalstate_sigterm[] = {
    am_sigint,   /* 0 */
    am_sighup,   /* 1 */
    am_sigquit,  /* 2 */
    am_sigabrt,  /* 3 */
    am_sigalrm,  /* 4 */
    am_sigterm,  /* 5 */
    am_sigusr1,  /* 6 */
    am_sigusr2,  /* 7 */
    am_sigchld,  /* 8 */
    am_sigstop,  /* 9 */
    am_sigtstp   /* 10 */
};

volatile Uint erts_signal_state = 0;
#define ERTS_SET_SIGNAL_STATE(S) (erts_signal_state |= signum_to_signalstate(S))
#define ERTS_CLEAR_SIGNAL_STATE (erts_signal_state = 0)
static ERTS_INLINE Uint signum_to_signalstate(int signum);
#endif

/* set early so the break handler has access to initial mode */
static struct termios initial_tty_mode;
static int replace_intr = 0;
/* assume yes initially, ttsl_init will clear it */
int using_oldshell = 1;

#ifdef ERTS_ENABLE_KERNEL_POLL

int erts_use_kernel_poll = 0;

struct {
    int (*select)(ErlDrvPort, ErlDrvEvent, int, int);
    int (*enif_select)(ErlNifEnv*, ErlNifEvent, enum ErlNifSelectFlags, void*, const ErlNifPid*, Eterm);
    int (*event)(ErlDrvPort, ErlDrvEvent, ErlDrvEventData);
    void (*check_io_as_interrupt)(void);
    void (*check_io_interrupt)(int);
    void (*check_io_interrupt_tmd)(int, ErtsMonotonicTime);
    void (*check_io)(int);
    Uint (*size)(void);
    Eterm (*info)(void *);
    int (*check_io_debug)(ErtsCheckIoDebugInfo *);
} io_func = {0};


int
driver_select(ErlDrvPort port, ErlDrvEvent event, int mode, int on)
{
    return (*io_func.select)(port, event, mode, on);
}

int
driver_event(ErlDrvPort port, ErlDrvEvent event, ErlDrvEventData event_data)
{
    return (*io_func.event)(port, event, event_data);
}

int enif_select(ErlNifEnv* env, ErlNifEvent event,
                enum ErlNifSelectFlags flags, void* obj, const ErlNifPid* pid, Eterm ref)
{
    return (*io_func.enif_select)(env, event, flags, obj, pid, ref);
}


Eterm erts_check_io_info(void *p)
{
    return (*io_func.info)(p);
}

int
erts_check_io_debug(ErtsCheckIoDebugInfo *ip)
{
    return (*io_func.check_io_debug)(ip);
}


static void
init_check_io(void)
{
    if (erts_use_kernel_poll) {
	io_func.select			= driver_select_kp;
        io_func.enif_select		= enif_select_kp;
	io_func.event			= driver_event_kp;
#ifdef ERTS_POLL_NEED_ASYNC_INTERRUPT_SUPPORT
	io_func.check_io_as_interrupt	= erts_check_io_async_sig_interrupt_kp;
#endif
	io_func.check_io_interrupt	= erts_check_io_interrupt_kp;
	io_func.check_io_interrupt_tmd	= erts_check_io_interrupt_timed_kp;
	io_func.check_io		= erts_check_io_kp;
	io_func.size			= erts_check_io_size_kp;
	io_func.info			= erts_check_io_info_kp;
	io_func.check_io_debug		= erts_check_io_debug_kp;
	erts_init_check_io_kp();
	max_files = erts_check_io_max_files_kp();
    }
    else {
	io_func.select			= driver_select_nkp;
        io_func.enif_select		= enif_select_nkp;
	io_func.event			= driver_event_nkp;
#ifdef ERTS_POLL_NEED_ASYNC_INTERRUPT_SUPPORT
	io_func.check_io_as_interrupt	= erts_check_io_async_sig_interrupt_nkp;
#endif
	io_func.check_io_interrupt	= erts_check_io_interrupt_nkp;
	io_func.check_io_interrupt_tmd	= erts_check_io_interrupt_timed_nkp;
	io_func.check_io		= erts_check_io_nkp;
	io_func.size			= erts_check_io_size_nkp;
	io_func.info			= erts_check_io_info_nkp;
	io_func.check_io_debug		= erts_check_io_debug_nkp;
	erts_init_check_io_nkp();
	max_files = erts_check_io_max_files_nkp();
    }
}

#ifdef ERTS_POLL_NEED_ASYNC_INTERRUPT_SUPPORT
#define ERTS_CHK_IO_AS_INTR()	(*io_func.check_io_as_interrupt)()
#else
#define ERTS_CHK_IO_AS_INTR()	(*io_func.check_io_interrupt)(1)
#endif
#define ERTS_CHK_IO_INTR	(*io_func.check_io_interrupt)
#define ERTS_CHK_IO_INTR_TMD	(*io_func.check_io_interrupt_tmd)
#define ERTS_CHK_IO		(*io_func.check_io)
#define ERTS_CHK_IO_SZ		(*io_func.size)

#else /* !ERTS_ENABLE_KERNEL_POLL */

static void
init_check_io(void)
{
    erts_init_check_io();
    max_files = erts_check_io_max_files();
}

#ifdef ERTS_POLL_NEED_ASYNC_INTERRUPT_SUPPORT
#define ERTS_CHK_IO_AS_INTR()	erts_check_io_async_sig_interrupt()
#else
#define ERTS_CHK_IO_AS_INTR()	erts_check_io_interrupt(1)
#endif
#define ERTS_CHK_IO_INTR	erts_check_io_interrupt
#define ERTS_CHK_IO_INTR_TMD	erts_check_io_interrupt_timed
#define ERTS_CHK_IO		erts_check_io
#define ERTS_CHK_IO_SZ		erts_check_io_size

#endif

void
erts_sys_schedule_interrupt(int set)
{
    ERTS_CHK_IO_INTR(set);
}

#ifdef ERTS_SMP
void
erts_sys_schedule_interrupt_timed(int set, ErtsMonotonicTime timeout_time)
{
    ERTS_CHK_IO_INTR_TMD(set, timeout_time);
}
#endif

UWord
erts_sys_get_page_size(void)
{
#if defined(_SC_PAGESIZE)
    return (UWord) sysconf(_SC_PAGESIZE);
#elif defined(HAVE_GETPAGESIZE)
    return (UWord) getpagesize();
#else
    return (UWord) 4*1024; /* Guess 4 KB */
#endif
}

Uint
erts_sys_misc_mem_sz(void)
{
    Uint res = ERTS_CHK_IO_SZ();
    res += erts_smp_atomic_read_mb(&sys_misc_mem_sz);
    return res;
}

/*
 * reset the terminal to the original settings on exit
 */
void sys_tty_reset(int exit_code)
{
  if (using_oldshell && !replace_intr) {
    SET_BLOCKING(0);
  }
  else if (isatty(0)) {
    tcsetattr(0,TCSANOW,&initial_tty_mode);
  }
}

#ifdef __tile__
/* Direct malloc to spread memory around the caches of multiple tiles. */
#include <malloc.h>
#if defined(MALLOC_USE_HASH)
MALLOC_USE_HASH(1);
#endif
#endif

#ifdef USE_THREADS

#ifdef ERTS_THR_HAVE_SIG_FUNCS

/*
 * Child thread inherits parents signal mask at creation. In order to
 * guarantee that the main thread will receive all SIGINT, and
 * SIGUSR1 signals sent to the process, we block these signals in the
 * parent thread when creating a new thread.
 */

static sigset_t thr_create_sigmask;

#endif /* #ifdef ERTS_THR_HAVE_SIG_FUNCS */

typedef struct {
#ifdef ERTS_THR_HAVE_SIG_FUNCS
    sigset_t saved_sigmask;
#endif
    int sched_bind_data;
} erts_thr_create_data_t;

/*
 * thr_create_prepare() is called in parent thread before thread creation.
 * Returned value is passed as argument to thr_create_cleanup().
 */
static void *
thr_create_prepare(void)
{
    erts_thr_create_data_t *tcdp;

    tcdp = erts_alloc(ERTS_ALC_T_TMP, sizeof(erts_thr_create_data_t));

#ifdef ERTS_THR_HAVE_SIG_FUNCS
    erts_thr_sigmask(SIG_BLOCK, &thr_create_sigmask, &tcdp->saved_sigmask);
#endif
    tcdp->sched_bind_data = erts_sched_bind_atthrcreate_prepare();

    return (void *) tcdp;
}


/* thr_create_cleanup() is called in parent thread after thread creation. */
static void
thr_create_cleanup(void *vtcdp)
{
    erts_thr_create_data_t *tcdp = (erts_thr_create_data_t *) vtcdp;

    erts_sched_bind_atthrcreate_parent(tcdp->sched_bind_data);

#ifdef ERTS_THR_HAVE_SIG_FUNCS
    /* Restore signalmask... */
    erts_thr_sigmask(SIG_SETMASK, &tcdp->saved_sigmask, NULL);
#endif

    erts_free(ERTS_ALC_T_TMP, tcdp);
}

static void
thr_create_prepare_child(void *vtcdp)
{
    erts_thr_create_data_t *tcdp = (erts_thr_create_data_t *) vtcdp;

#ifdef ERTS_ENABLE_LOCK_COUNT
    erts_lcnt_thread_setup();
#endif

#ifndef NO_FPE_SIGNALS
    /*
     * We do not want fp exeptions in other threads than the
     * scheduler threads. We enable fpe explicitly in the scheduler
     * threads after this.
     */
    erts_thread_disable_fpe();
#endif

    erts_sched_bind_atthrcreate_child(tcdp->sched_bind_data);
}

#endif /* #ifdef USE_THREADS */

void
erts_sys_pre_init(void)
{
#ifdef USE_THREADS
    erts_thr_init_data_t eid = ERTS_THR_INIT_DATA_DEF_INITER;
#endif

    erts_printf_add_cr_to_stdout = 1;
    erts_printf_add_cr_to_stderr = 1;

#ifdef USE_THREADS

    eid.thread_create_child_func = thr_create_prepare_child;
    /* Before creation in parent */
    eid.thread_create_prepare_func = thr_create_prepare;
    /* After creation in parent */
    eid.thread_create_parent_func = thr_create_cleanup,

    erts_thr_init(&eid);

#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_lc_init();
#endif

#ifdef ERTS_ENABLE_LOCK_COUNT
    erts_lcnt_init();
#endif

#endif /* USE_THREADS */

    erts_init_sys_time_sup();

#ifdef USE_THREADS

#ifdef ERTS_SMP
    erts_smp_atomic32_init_nob(&erts_break_requested, 0);
    erts_smp_atomic32_init_nob(&have_prepared_crash_dump, 0);
#else
    erts_break_requested = 0;
    have_prepared_crash_dump = 0;
#endif

#endif /* USE_THREADS */

    erts_smp_atomic_init_nob(&sys_misc_mem_sz, 0);

    {
      /*
       * Unfortunately we depend on fd 0,1,2 in the old shell code.
       * So if for some reason we do not have those open when we start
       * we have to open them here. Not doing this can cause the emulator
       * to deadlock when reaping the fd_driver ports :(
       */
      int fd;
      /* Make sure fd 0 is open */
      if ((fd = open("/dev/null", O_RDONLY)) != 0)
	close(fd);
      /* Make sure fds 1 and 2 are open */
      while (fd < 3) {
	fd = open("/dev/null", O_WRONLY);
      }
      close(fd);
    }

    /* We need a file descriptor to close in the crashdump creation.
     * We close this one to be sure we can get a fd for our real file ...
     * so, we create one here ... a stone to carry all the way home.
     */

    crashdump_companion_cube_fd = open("/dev/null", O_RDONLY);

    /* don't lose it, there will be cake */
}

void
erl_sys_init(void)
{

#ifdef USE_SETLINEBUF
    setlinebuf(stdout);
#else
    setvbuf(stdout, (char *)NULL, _IOLBF, BUFSIZ);
#endif

    erts_sys_init_float();

    /* we save this so the break handler can set and reset it properly */
    /* also so that we can reset on exit (break handler or not) */
    if (isatty(0)) {
	tcgetattr(0,&initial_tty_mode);
    }
    tzset(); /* Required at least for NetBSD with localtime_r() */
}

/* signal handling */

SIGFUNC sys_signal(int sig, SIGFUNC func)
{
    struct sigaction act, oact;

    sigemptyset(&act.sa_mask);
    act.sa_flags = 0;
    act.sa_handler = func;
    sigaction(sig, &act, &oact);
    return(oact.sa_handler);
}

#ifdef USE_THREADS
#undef  sigprocmask
#define sigprocmask erts_thr_sigmask
#endif

void sys_sigblock(int sig)
{
    sigset_t mask;

    sigemptyset(&mask);
    sigaddset(&mask, sig);
    sigprocmask(SIG_BLOCK, &mask, (sigset_t *)NULL);
}

void sys_sigrelease(int sig)
{
    sigset_t mask;

    sigemptyset(&mask);
    sigaddset(&mask, sig);
    sigprocmask(SIG_UNBLOCK, &mask, (sigset_t *)NULL);
}

#ifdef ERTS_HAVE_TRY_CATCH
void erts_sys_sigsegv_handler(int signo) {
    if (signo == SIGSEGV) {
        longjmp(erts_sys_sigsegv_jmp, 1);
    }
}
#endif

/*
 * Function returns 1 if we can read from all values in between
 * start and stop.
 */
int
erts_sys_is_area_readable(char *start, char *stop) {
    int fds[2];
    if (!pipe(fds)) {
        /* We let write try to figure out if the pointers are readable */
        int res = write(fds[1], start, (char*)stop - (char*)start);
        if (res == -1) {
            close(fds[0]);
            close(fds[1]);
            return 0;
        }
        close(fds[0]);
        close(fds[1]);
        return 1;
    }
    return 0;

}

static ERTS_INLINE int
prepare_crash_dump(int secs)
{
#define NUFBUF (3)
    int i;
    char env[21]; /* enough to hold any 64-bit integer */
    size_t envsz;
    DeclareTmpHeapNoproc(heap,NUFBUF);
    Port *heart_port;
    Eterm *hp = heap;
    Eterm list = NIL;
    int has_heart = 0;

    UseTmpHeapNoproc(NUFBUF);

    if (ERTS_PREPARED_CRASH_DUMP)
	return 0; /* We have already been called */

    heart_port = erts_get_heart_port();

    /* Positive secs means an alarm must be set
     * 0 or negative means no alarm
     *
     * Set alarm before we try to write to a port
     * we don't want to hang on a port write with
     * no alarm.
     *
     */

    if (secs >= 0) {
	alarm((unsigned int)secs);
    }

    /* close all viable sockets via emergency close callbacks.
     * Specifically we want to close epmd sockets.
     */

    erts_emergency_close_ports();

    if (heart_port) {
	has_heart = 1;
	list = CONS(hp, make_small(8), list); hp += 2;
	/* send to heart port, CMD = 8, i.e. prepare crash dump =o */
	erts_port_output(NULL, ERTS_PORT_SIG_FLG_FORCE_IMM_CALL, heart_port,
			 heart_port->common.id, list, NULL);
    }

    /* Make sure we have a fd for our crashdump file. */
    close(crashdump_companion_cube_fd);

    envsz = sizeof(env);
    i = erts_sys_getenv__("ERL_CRASH_DUMP_NICE", env, &envsz);
    if (i >= 0) {
	int nice_val;
	nice_val = i != 0 ? 0 : atoi(env);
	if (nice_val > 39) {
	    nice_val = 39;
	}
	erts_silence_warn_unused_result(nice(nice_val));
    }

    UnUseTmpHeapNoproc(NUFBUF);
#undef NUFBUF
    return has_heart;
}

int erts_sys_prepare_crash_dump(int secs)
{
    return prepare_crash_dump(secs);
}

static void signal_notify_requested(Eterm type) {
    Process* p = NULL;
    Eterm msg, *hp;
    ErtsProcLocks locks = 0;
    ErlOffHeap *ohp;

    Eterm id = erts_whereis_name_to_id(NULL, am_erl_signal_server);

    if ((p = (erts_pid2proc_opt(NULL, 0, id, 0, ERTS_P2P_FLG_INC_REFC))) != NULL) {
        ErtsMessage *msgp = erts_alloc_message_heap(p, &locks, 3, &hp, &ohp);

        /* erl_signal_server ! {notify, sighup} */
        msg = TUPLE2(hp, am_notify, type);
        erts_queue_message(p, locks, msgp, msg, am_system);

        if (locks)
            erts_smp_proc_unlock(p, locks);
        erts_proc_dec_refc(p);
    }
}


static ERTS_INLINE void
break_requested(void)
{
  /*
   * just set a flag - checked for and handled by
   * scheduler threads erts_check_io() (not signal handler).
   */
#ifdef DEBUG			
  fprintf(stderr,"break!\n");
#endif
  if (ERTS_BREAK_REQUESTED)
      erts_exit(ERTS_INTR_EXIT, "");

  ERTS_SET_BREAK_REQUESTED;
  ERTS_CHK_IO_AS_INTR(); /* Make sure we don't sleep in poll */
}

static RETSIGTYPE request_break(int signum)
{
#ifdef ERTS_SMP
    smp_sig_notify(signum);
#else
    break_requested();
#endif
}

#ifdef ETHR_UNUSABLE_SIGUSRX
#warning "Unusable SIGUSR1 & SIGUSR2. Disabling use of these signals"

#else

#ifdef ERTS_SYS_SUSPEND_SIGNAL
void
sys_thr_suspend(erts_tid_t tid) {
    erts_thr_kill(tid, ERTS_SYS_SUSPEND_SIGNAL);
}

void
sys_thr_resume(erts_tid_t tid) {
    int i = 0, res;
    do {
        res = write(sig_suspend_fds[1],&i,sizeof(i));
    } while (res < 0 && errno == EAGAIN);
}
#endif

#ifdef ERTS_SYS_SUSPEND_SIGNAL
#if (defined(SIG_SIGSET) || defined(SIG_SIGNAL))
static RETSIGTYPE suspend_signal(void)
#else
static RETSIGTYPE suspend_signal(int signum)
#endif
{
    int res, buf[1], tmp_errno = errno;
    do {
        res = read(sig_suspend_fds[0], buf, sizeof(int));
    } while (res < 0 && errno == EINTR);

    /* restore previous errno in case read changed it */
    errno = tmp_errno;
}
#endif /* #ifdef ERTS_SYS_SUSPEND_SIGNAL */

#endif /* #ifndef ETHR_UNUSABLE_SIGUSRX */

/*
 Signal      Action   Comment
 ─────────────────────────────────────────────────────────────
  SIGHUP     Term     Hangup detected on controlling terminal or death of controlling process
 !SIGINT     Term     Interrupt from keyboard
  SIGQUIT    Core     Quit from keyboard
 !SIGILL     Core     Illegal Instruction
  SIGABRT    Core     Abort signal from abort(3)
 !SIGFPE     Core     Floating point exception
 !SIGKILL    Term     Kill signal
 !SIGSEGV    Core     Invalid memory reference
 !SIGPIPE    Term     Broken pipe: write to pipe with no readers
  SIGALRM    Term     Timer signal from alarm(2)
  SIGTERM    Term     Termination signal
  SIGUSR1    Term     User-defined signal 1
  SIGUSR2    Term     User-defined signal 2
 !SIGCHLD    Ign      Child stopped or terminated
 !SIGCONT    Cont     Continue if stopped
  SIGSTOP    Stop     Stop process
  SIGTSTP    Stop     Stop typed at terminal
 !SIGTTIN    Stop     Terminal input for background process
 !SIGTTOU    Stop     Terminal output for background process
*/


static ERTS_INLINE int
signalterm_to_signum(Eterm signal)
{
    switch (signal) {
    case am_sighup:  return SIGHUP;
    /* case am_sigint:  return SIGINT; */
    case am_sigquit: return SIGQUIT;
    /* case am_sigill:  return SIGILL; */
    case am_sigabrt: return SIGABRT;
    /* case am_sigsegv: return SIGSEGV; */
    case am_sigalrm: return SIGALRM;
    case am_sigterm: return SIGTERM;
    case am_sigusr1: return SIGUSR1;
    case am_sigusr2: return SIGUSR2;
    case am_sigchld: return SIGCHLD;
    case am_sigstop: return SIGSTOP;
    case am_sigtstp: return SIGTSTP;
    default:         return 0;
    }
}

static ERTS_INLINE Eterm
signum_to_signalterm(int signum)
{
    switch (signum) {
    case SIGHUP:  return am_sighup;
    /* case SIGINT:  return am_sigint; */    /* ^c */
    case SIGQUIT: return am_sigquit;   /* ^\ */
    /* case SIGILL:  return am_sigill; */
    case SIGABRT: return am_sigabrt;
    /* case SIGSEGV: return am_sigsegv; */
    case SIGALRM: return am_sigalrm;
    case SIGTERM: return am_sigterm;
    case SIGUSR1: return am_sigusr1;
    case SIGUSR2: return am_sigusr2;
    case SIGCHLD: return am_sigchld;
    case SIGSTOP: return am_sigstop;
    case SIGTSTP: return am_sigtstp;   /* ^z */
    default:      return am_error;
    }
}

#ifndef ERTS_SMP
static ERTS_INLINE Uint
signum_to_signalstate(int signum)
{
    switch (signum) {
    case SIGINT:  return (1 <<  0);
    case SIGHUP:  return (1 <<  1);
    case SIGQUIT: return (1 <<  2);
    case SIGABRT: return (1 <<  3);
    case SIGALRM: return (1 <<  4);
    case SIGTERM: return (1 <<  5);
    case SIGUSR1: return (1 <<  6);
    case SIGUSR2: return (1 <<  7);
    case SIGCHLD: return (1 <<  8);
    case SIGSTOP: return (1 <<  9);
    case SIGTSTP: return (1 << 10);
    default:      return 0;
    }
}
#endif

static RETSIGTYPE generic_signal_handler(int signum)
{
#ifdef ERTS_SMP
    smp_sig_notify(signum);
#else
    ERTS_SET_SIGNAL_STATE(signum);
    ERTS_CHK_IO_AS_INTR(); /* Make sure we don't sleep in poll */
#endif
}

int erts_set_signal(Eterm signal, Eterm type) {
    int signum;
    if ((signum = signalterm_to_signum(signal)) > 0) {
        if (type == am_ignore) {
            sys_signal(signum, SIG_IGN);
        } else if (type == am_default) {
            sys_signal(signum, SIG_DFL);
        } else {
            sys_signal(signum, generic_signal_handler);
        }
        return 1;
    }
    return 0;
}

/* Disable break */
void erts_set_ignore_break(void) {
    /*
     * Ignore signals that can be sent to the VM by
     * typing certain key combinations at the
     * controlling terminal...
     */
    sys_signal(SIGINT,  SIG_IGN);       /* Ctrl-C */
    sys_signal(SIGQUIT, SIG_IGN);       /* Ctrl-\ */
    sys_signal(SIGTSTP, SIG_IGN);       /* Ctrl-Z */
}

/* Don't use ctrl-c for break handler but let it be 
   used by the shell instead (see user_drv.erl) */
void erts_replace_intr(void) {
  struct termios mode;

  if (isatty(0)) {
    tcgetattr(0, &mode);

    /* here's an example of how to replace ctrl-c with ctrl-u */
    /* mode.c_cc[VKILL] = 0;
       mode.c_cc[VINTR] = CKILL; */

    mode.c_cc[VINTR] = 0;	/* disable ctrl-c */
    tcsetattr(0, TCSANOW, &mode);
    replace_intr = 1;
  }
}

void init_break_handler(void)
{
   sys_signal(SIGINT,  request_break);
   sys_signal(SIGHUP,  generic_signal_handler);
#ifndef ETHR_UNUSABLE_SIGUSRX
   sys_signal(SIGUSR1, generic_signal_handler);
#endif /* #ifndef ETHR_UNUSABLE_SIGUSRX */
   sys_signal(SIGQUIT, generic_signal_handler);
}

void sys_init_suspend_handler(void)
{
#ifdef ERTS_SYS_SUSPEND_SIGNAL
   sys_signal(ERTS_SYS_SUSPEND_SIGNAL, suspend_signal);
#endif
}

void
erts_sys_unix_later_init(void)
{
    sys_signal(SIGTERM, generic_signal_handler);
}

int sys_max_files(void)
{
   return(max_files);
}

/************************** OS info *******************************/

/* Used by erlang:info/1. */
/* (This code was formerly in drv.XXX/XXX_os_drv.c) */

char os_type[] = "unix";

static int
get_number(char **str_ptr)
{
    char* s = *str_ptr;		/* Pointer to beginning of string. */
    char* dot;			/* Pointer to dot in string or NULL. */

    if (!isdigit((int) *s))
	return 0;
    if ((dot = strchr(s, '.')) == NULL) {
	*str_ptr = s+strlen(s);
	return atoi(s);
    } else {
	*dot = '\0';
	*str_ptr = dot+1;
	return atoi(s);
    }
}

void os_flavor(char* namebuf, unsigned size) {
    struct utsname uts;		/* Information about the system. */
    char* s;

    (void) uname(&uts);
    for (s = uts.sysname; *s; s++) {
	if (isupper((int) *s)) {
	    *s = tolower((int) *s);
	}
    }
    strcpy(namebuf, uts.sysname);
}

void os_version(int *pMajor, int *pMinor, int *pBuild) {
    struct utsname uts;		/* Information about the system. */
    char* release;		/* Pointer to the release string:
				 * X.Y or X.Y.Z.  */

    (void) uname(&uts);
    release = uts.release;
    *pMajor = get_number(&release); /* Pointer to major version. */
    *pMinor = get_number(&release); /* Pointer to minor version. */
    *pBuild = get_number(&release); /* Pointer to build number. */
}

void init_getenv_state(GETENV_STATE *state)
{
   erts_smp_rwmtx_rlock(&environ_rwmtx);
   *state = NULL;
}

char *getenv_string(GETENV_STATE *state0)
{
   char **state = (char **) *state0;
   char *cp;

   ERTS_SMP_LC_ASSERT(erts_smp_lc_rwmtx_is_rlocked(&environ_rwmtx));

   if (state == NULL)
      state = environ;

   cp = *state++;
   *state0 = (GETENV_STATE) state;

   return cp;
}

void fini_getenv_state(GETENV_STATE *state)
{
   *state = NULL;
   erts_smp_rwmtx_runlock(&environ_rwmtx);
}

void erts_do_break_handling(void)
{
    struct termios temp_mode;
    int saved = 0;

    /*
     * Most functions that do_break() calls are intentionally not thread safe;
     * therefore, make sure that all threads but this one are blocked before
     * proceeding!
     */
    erts_smp_thr_progress_block();

    /* during break we revert to initial settings */
    /* this is done differently for oldshell */
    if (using_oldshell && !replace_intr) {
      SET_BLOCKING(1);
    }
    else if (isatty(0)) {
      tcgetattr(0,&temp_mode);
      tcsetattr(0,TCSANOW,&initial_tty_mode);
      saved = 1;
    }

    /* call the break handling function, reset the flag */
    do_break();

    ERTS_UNSET_BREAK_REQUESTED;

    fflush(stdout);

    /* after break we go back to saved settings */
    if (using_oldshell && !replace_intr) {
      SET_NONBLOCKING(1);
    }
    else if (saved) {
      tcsetattr(0,TCSANOW,&temp_mode);
    }

    erts_smp_thr_progress_unblock();
}

#ifdef ERTS_SIGNAL_STATE
void erts_handle_signal_state(void) {
    Uint signal_state = ERTS_SIGNAL_STATE;
    Uint i = 0;

    ERTS_CLEAR_SIGNAL_STATE;

    while (signal_state) {
        if (signal_state & 0x1) {
            signal_notify_requested(signalstate_sigterm[i]);
        }
        i++;
        signal_state = signal_state >> 1;
    }
}
#endif

/* Fills in the systems representation of the jam/beam process identifier.
** The Pid is put in STRING representation in the supplied buffer,
** no interpretatione of this should be done by the rest of the
** emulator. The buffer should be at least 21 bytes long.
*/
void sys_get_pid(char *buffer, size_t buffer_size){
    pid_t p = getpid();
    /* Assume the pid is scalar and can rest in an unsigned long... */
    erts_snprintf(buffer, buffer_size, "%lu",(unsigned long) p);
}

int
erts_sys_putenv_raw(char *key, char *value) {
    return erts_sys_putenv(key, value);
}
int
erts_sys_putenv(char *key, char *value)
{
    int res;
    char *env;
    Uint need = strlen(key) + strlen(value) + 2;

#ifdef HAVE_COPYING_PUTENV
    env = erts_alloc(ERTS_ALC_T_TMP, need);
#else
    env = erts_alloc(ERTS_ALC_T_PUTENV_STR, need);
    erts_smp_atomic_add_nob(&sys_misc_mem_sz, need);
#endif
    strcpy(env,key);
    strcat(env,"=");
    strcat(env,value);
    erts_smp_rwmtx_rwlock(&environ_rwmtx);
    res = putenv(env);
    erts_smp_rwmtx_rwunlock(&environ_rwmtx);
#ifdef HAVE_COPYING_PUTENV
    erts_free(ERTS_ALC_T_TMP, env);
#endif
    return res;
}

int
erts_sys_getenv__(char *key, char *value, size_t *size)
{
    int res;
    char *orig_value = getenv(key);
    if (!orig_value)
	res = -1;
    else {
	size_t len = sys_strlen(orig_value);
	if (len >= *size) {
	    *size = len + 1;
	    res = 1;
	}
	else {
	    *size = len;
	    sys_memcpy((void *) value, (void *) orig_value, len+1);
	    res = 0;
	}
    }
    return res;
}

int
erts_sys_getenv_raw(char *key, char *value, size_t *size) {
    return erts_sys_getenv(key, value, size);
}

/*
 * erts_sys_getenv
 * returns:
 *  -1, if environment key is not set with a value
 *   0, if environment key is set and value fits into buffer size
 *   1, if environment key is set but does not fit into buffer size
 *      size is set with the needed buffer size value
 */

int
erts_sys_getenv(char *key, char *value, size_t *size)
{
    int res;
    erts_smp_rwmtx_rlock(&environ_rwmtx);
    res = erts_sys_getenv__(key, value, size);
    erts_smp_rwmtx_runlock(&environ_rwmtx);
    return res;
}

int
erts_sys_unsetenv(char *key)
{
    int res;
    erts_smp_rwmtx_rwlock(&environ_rwmtx);
    res = unsetenv(key);
    erts_smp_rwmtx_rwunlock(&environ_rwmtx);
    return res;
}

void sys_init_io(void) { }
void erts_sys_alloc_init(void) { }

extern const char pre_loaded_code[];
extern Preload pre_loaded[];

#if ERTS_HAVE_ERTS_SYS_ALIGNED_ALLOC
void *erts_sys_aligned_alloc(UWord alignment, UWord size)
{
#ifdef HAVE_POSIX_MEMALIGN
    void *ptr = NULL;
    int error;
    ASSERT(alignment && (alignment & (alignment-1)) == 0); /* power of 2 */
    error = posix_memalign(&ptr, (size_t) alignment, (size_t) size);
#if HAVE_ERTS_MSEG
    if (error || !ptr) {
	erts_mseg_clear_cache();
	error = posix_memalign(&ptr, (size_t) alignment, (size_t) size);
    }
#endif
    if (error) {
	errno = error;
	return NULL;
    }
    if (!ptr)
	errno = ENOMEM;
    ASSERT(!ptr || (((UWord) ptr) & (alignment - 1)) == 0);
    return ptr;
#else
#  error "Missing erts_sys_aligned_alloc() implementation"
#endif
}

void erts_sys_aligned_free(UWord alignment, void *ptr)
{
    ASSERT(alignment && (alignment & (alignment-1)) == 0); /* power of 2 */
    free(ptr);
}

void *erts_sys_aligned_realloc(UWord alignment, void *ptr, UWord size, UWord old_size)
{
    void *new_ptr = erts_sys_aligned_alloc(alignment, size);
    if (new_ptr) {
	UWord copy_size = old_size < size ? old_size : size;
	sys_memcpy(new_ptr, ptr, (size_t) copy_size);
	erts_sys_aligned_free(alignment, ptr);
    }
    return new_ptr;
}

#endif

void *erts_sys_alloc(ErtsAlcType_t t, void *x, Uint sz)
{
    void *res = malloc((size_t) sz);
#if HAVE_ERTS_MSEG
    if (!res) {
	erts_mseg_clear_cache();
	return malloc((size_t) sz);
    }
#endif
    return res;
}

void *erts_sys_realloc(ErtsAlcType_t t, void *x, void *p, Uint sz)
{
    void *res = realloc(p, (size_t) sz);
#if HAVE_ERTS_MSEG
    if (!res) {
	erts_mseg_clear_cache();
	return realloc(p, (size_t) sz);
    }
#endif
    return res;
}

void erts_sys_free(ErtsAlcType_t t, void *x, void *p)
{
    free(p);
}

/* Return a pointer to a vector of names of preloaded modules */

Preload*
sys_preloaded(void)
{
    return pre_loaded;
}

/* Return a pointer to preloaded code for module "module" */
unsigned char*
sys_preload_begin(Preload* p)
{
    return p->code;
}

/* Clean up if allocated */
void sys_preload_end(Preload* p)
{
    /* Nothing */
}

/* Read a key from console, used by break.c
   Here we assume that all schedulers are stopped so that erl_poll
   does not interfere with the select below.
*/
int sys_get_key(int fd) {
    int c, ret;
    unsigned char rbuf[64];
    fd_set fds;

    fflush(stdout);		/* Flush query ??? */

    FD_ZERO(&fds);
    FD_SET(fd,&fds);

    ret = select(fd+1, &fds, NULL, NULL, NULL);

    if (ret == 1) {
        do {
            c = read(fd,rbuf,64);
        } while (c < 0 && errno == EAGAIN);
        if (c <= 0)
            return c;
    }
    return rbuf[0];
}


extern int erts_initialized;
void
erl_assert_error(const char* expr, const char* func, const char* file, int line)
{
    fflush(stdout);
    fprintf(stderr, "%s:%d:%s() Assertion failed: %s\n",
            file, line, func, expr);
    fflush(stderr);
#if !defined(ERTS_SMP) && 0
    /* Writing a crashdump from a failed assertion when smp support
     * is enabled almost a guaranteed deadlocking, don't even bother.
     *
     * It could maybe be useful (but I'm not convinced) to write the
     * crashdump if smp support is disabled...
     */
    if (erts_initialized)
	erl_crash_dump(file, line, "Assertion failed: %s\n", expr);
#endif
    abort();
}

#ifdef DEBUG

void
erl_debug(char* fmt, ...)
{
    char sbuf[1024];		/* Temporary buffer. */
    va_list va;

    if (debug_log) {
	va_start(va, fmt);
	vsprintf(sbuf, fmt, va);
	va_end(va);
	fprintf(stderr, "%s", sbuf);
    }
}

#endif /* DEBUG */

/*
 * Called from schedule() when it runs out of runnable processes,
 * or when Erlang code has performed INPUT_REDUCTIONS reduction
 * steps. runnable == 0 iff there are no runnable Erlang processes.
 */
void
erl_sys_schedule(int runnable)
{
    ERTS_CHK_IO(!runnable);
    ERTS_SMP_LC_ASSERT(!erts_thr_progress_is_blocking());
}


#ifdef ERTS_SMP

static erts_smp_tid_t sig_dispatcher_tid;

static void
smp_sig_notify(int signum)
{
    int res;
    do {
	/* write() is async-signal safe (according to posix) */
	res = write(sig_notify_fds[1], &signum, sizeof(int));
    } while (res < 0 && errno == EINTR);
    if (res != sizeof(int)) {
	char msg[] =
	    "smp_sig_notify(): Failed to notify signal-dispatcher thread "
	    "about received signal";
	erts_silence_warn_unused_result(write(2, msg, sizeof(msg)));
	abort();
    }
}

static void *
signal_dispatcher_thread_func(void *unused)
{
#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_lc_set_thread_name("signal_dispatcher");
#endif
    while (1) {
        union {int signum; char buf[4];} sb;
        Eterm signal;
	int res, i = 0;
	/* Block on read() waiting for a signal notification to arrive... */

        do {
            res = read(sig_notify_fds[0], (void *) &sb.buf[i], sizeof(int) - i);
            i += res > 0 ? res : 0;
        } while ((i < sizeof(int) && res >= 0) || (res < 0 && errno == EINTR));

	if (res < 0) {
	    erts_exit(ERTS_ABORT_EXIT,
		     "signal-dispatcher thread got unexpected error: %s (%d)\n",
		     erl_errno_id(errno),
		     errno);
	}
        /*
         * NOTE 1: The signal dispatcher thread should not do work
         *         that takes a substantial amount of time (except
         *         perhaps in test and debug builds). It needs to
         *         be responsive, i.e, it should only dispatch work
         *         to other threads.
         *
         * NOTE 2: The signal dispatcher thread is not a blockable
         *         thread (i.e., not a thread managed by the
         *         erl_thr_progress module). This is intentional.
         *         We want to be able to interrupt writing of a crash
         *         dump by hitting C-c twice. Since it isn't a
         *         blockable thread it is important that it doesn't
         *         change the state of any data that a blocking thread
         *         expects to have exclusive access to (unless the
         *         signal dispatcher itself explicitly is blocking all
         *         blockable threads).
         */
        switch (sb.signum) {
            case 0: continue;
            case SIGINT:
                break_requested();
                break;
            default:
                if ((signal = signum_to_signalterm(sb.signum)) == am_error) {
                    erts_exit(ERTS_ABORT_EXIT,
                            "signal-dispatcher thread received unknown "
                            "signal notification: '%d'\n",
                            sb.signum);
                }
                signal_notify_requested(signal);
        }
        ERTS_SMP_LC_ASSERT(!erts_thr_progress_is_blocking());
    }
    return NULL;
}

static void
init_smp_sig_notify(void)
{
    erts_smp_thr_opts_t thr_opts = ERTS_SMP_THR_OPTS_DEFAULT_INITER;
    thr_opts.detached = 1;
    thr_opts.name = "sys_sig_dispatcher";

    if (pipe(sig_notify_fds) < 0) {
	erts_exit(ERTS_ABORT_EXIT,
		 "Failed to create signal-dispatcher pipe: %s (%d)\n",
		 erl_errno_id(errno),
		 errno);
    }

    /* Start signal handler thread */
    erts_smp_thr_create(&sig_dispatcher_tid,
			signal_dispatcher_thread_func,
			NULL,
			&thr_opts);
}

static void
init_smp_sig_suspend(void) {
#ifdef ERTS_SYS_SUSPEND_SIGNAL
  if (pipe(sig_suspend_fds) < 0) {
    erts_exit(ERTS_ABORT_EXIT,
	     "Failed to create sig_suspend pipe: %s (%d)\n",
	     erl_errno_id(errno),
	     errno);
  }
#endif
}

#ifdef __DARWIN__

int erts_darwin_main_thread_pipe[2];
int erts_darwin_main_thread_result_pipe[2];

static void initialize_darwin_main_thread_pipes(void)
{
    if (pipe(erts_darwin_main_thread_pipe) < 0 ||
	pipe(erts_darwin_main_thread_result_pipe) < 0) {
	erts_exit(ERTS_ERROR_EXIT,"Fatal error initializing Darwin main thread stealing");
    }
}

#endif
void
erts_sys_main_thread(void)
{
    erts_thread_disable_fpe();
#ifdef __DARWIN__
    initialize_darwin_main_thread_pipes();
#endif
    /* Become signal receiver thread... */
#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_lc_set_thread_name("signal_receiver");
#endif

    smp_sig_notify(0); /* Notify initialized */

    /* Wait for a signal to arrive... */

#ifdef __DARWIN__
    while (1) {
	/*
	 * The wx driver needs to be able to steal the main thread for Cocoa to
	 * work properly.
	 */
	fd_set readfds;
	int res;

	FD_ZERO(&readfds);
	FD_SET(erts_darwin_main_thread_pipe[0], &readfds);
	res = select(erts_darwin_main_thread_pipe[0] + 1, &readfds, NULL, NULL, NULL);
	if (res > 0 && FD_ISSET(erts_darwin_main_thread_pipe[0],&readfds)) {
	    void* (*func)(void*);
	    void* arg;
	    void *resp;
            res = read(erts_darwin_main_thread_pipe[0],&func,sizeof(void* (*)(void*)));
            if (res != sizeof(void* (*)(void*)))
                break;
            res = read(erts_darwin_main_thread_pipe[0],&arg,sizeof(void*));
            if (res != sizeof(void*))
                break;
	    resp = (*func)(arg);
	    write(erts_darwin_main_thread_result_pipe[1],&resp,sizeof(void *));
	}

        if (res == -1 && errno != EINTR)
            break;
    }
    /* Something broke with the main thread pipe, so we ignore it for now.
       Most probably erts has closed this pipe and is about to exit. */
#endif /* #ifdef __DARWIN__ */

    while (1) {
#ifdef DEBUG
	int res =
#else
	(void)
#endif
	    select(0, NULL, NULL, NULL, NULL);
	ASSERT(res < 0);
	ASSERT(errno == EINTR);
    }
}

#endif /* ERTS_SMP */

#ifdef ERTS_ENABLE_KERNEL_POLL /* get_value() is currently only used when
				  kernel-poll is enabled */

/* Get arg marks argument as handled by
   putting NULL in argv */
static char *
get_value(char* rest, char** argv, int* ip)
{
    char *param = argv[*ip]+1;
    argv[*ip] = NULL;
    if (*rest == '\0') {
	char *next = argv[*ip + 1];
	if (next[0] == '-'
	    && next[1] == '-'
	    &&  next[2] == '\0') {
	    erts_fprintf(stderr, "bad \"%s\" value: \n", param);
	    erts_usage();
	}
	(*ip)++;
	argv[*ip] = NULL;
	return next;
    }
    return rest;
}

#endif /* ERTS_ENABLE_KERNEL_POLL */

void
erl_sys_args(int* argc, char** argv)
{
    int i, j;

    erts_smp_rwmtx_init(&environ_rwmtx, "environ");

    i = 1;

    ASSERT(argc && argv);

    while (i < *argc) {
	if(argv[i][0] == '-') {
	    switch (argv[i][1]) {
#ifdef ERTS_ENABLE_KERNEL_POLL
	    case 'K': {
		char *arg = get_value(argv[i] + 2, argv, &i);
		if (strcmp("true", arg) == 0) {
		    erts_use_kernel_poll = 1;
		}
		else if (strcmp("false", arg) == 0) {
		    erts_use_kernel_poll = 0;
		}
		else {
		    erts_fprintf(stderr, "bad \"K\" value: %s\n", arg);
		    erts_usage();
		}
		break;
	    }
#endif
	    case '-':
		goto done_parsing;
	    default:
		break;
	    }
	}
	i++;
    }

 done_parsing:

#ifdef ERTS_ENABLE_KERNEL_POLL
    if (erts_use_kernel_poll) {
	char no_kp[10];
	size_t no_kp_sz = sizeof(no_kp);
	int res = erts_sys_getenv_raw("ERL_NO_KERNEL_POLL", no_kp, &no_kp_sz);
	if (res > 0
	    || (res == 0
		&& sys_strcmp("false", no_kp) != 0
		&& sys_strcmp("FALSE", no_kp) != 0)) {
	    erts_use_kernel_poll = 0;
	}
    }
#endif

    init_check_io();

#ifdef ERTS_SMP
    init_smp_sig_notify();
    init_smp_sig_suspend();
#endif

    /* Handled arguments have been marked with NULL. Slide arguments
       not handled towards the beginning of argv. */
    for (i = 0, j = 0; i < *argc; i++) {
	if (argv[i])
	    argv[j++] = argv[i];
    }
    *argc = j;
}
