/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1996-2010. All Rights Reserved.
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

#define NEED_CHILD_SETUP_DEFINES
#define ERTS_WANT_BREAK_HANDLING
#define ERTS_WANT_GOT_SIGUSR1
#define WANT_NONBLOCKING    /* must define this to pull in defs from sys.h */
#include "sys.h"

#ifdef USE_THREADS
#include "erl_threads.h"
#endif

#include "erl_mseg.h"

extern char **environ;
static erts_smp_rwmtx_t environ_rwmtx;

#define MAX_VSIZE 16		/* Max number of entries allowed in an I/O
				 * vector sock_sendv().
				 */

/*
 * Don't need global.h, but bif_table.h (included by bif.h),
 * won't compile otherwise
 */
#include "global.h" 
#include "bif.h"

#include "erl_sys_driver.h"
#include "erl_check_io.h"

#ifndef DISABLE_VFORK
#define DISABLE_VFORK 0
#endif

#ifdef USE_THREADS
#  ifdef ENABLE_CHILD_WAITER_THREAD
#    define CHLDWTHR ENABLE_CHILD_WAITER_THREAD
#  else
#    define CHLDWTHR 0
#  endif
#else
#  define CHLDWTHR 0
#endif
/*
 * [OTP-3906]
 * Solaris signal management gets confused when threads are used and a
 * lot of child processes dies. The confusion results in that SIGCHLD
 * signals aren't delivered to the emulator which in turn results in
 * a lot of defunct processes in the system.
 *
 * The problem seems to appear when a signal is frequently
 * blocked/unblocked at the same time as the signal is frequently
 * propagated. The child waiter thread is a workaround for this problem.
 * The SIGCHLD signal is always blocked (in all threads), and the child
 * waiter thread fetches the signal by a call to sigwait(). See
 * child_waiter().
 */

typedef struct ErtsSysReportExit_ ErtsSysReportExit;
struct ErtsSysReportExit_ {
    ErtsSysReportExit *next;
    Eterm port;
    int pid;
    int ifd;
    int ofd;
#if CHLDWTHR && !defined(ERTS_SMP)
    int status;
#endif
};

static ErtsSysReportExit *report_exit_list;
#if CHLDWTHR && !defined(ERTS_SMP)
static ErtsSysReportExit *report_exit_transit_list;
#endif

extern int  check_async_ready(void);
extern int  driver_interrupt(int, int);
extern void do_break(void);

extern void erl_sys_args(int*, char**);

/* The following two defs should probably be moved somewhere else */

extern void erts_sys_init_float(void);

extern void erl_crash_dump(char* file, int line, char* fmt, ...);

#define DIR_SEPARATOR_CHAR    '/'

#if defined(DEBUG)
#define ERL_BUILD_TYPE_MARKER ".debug"
#elif defined(PURIFY)
#define ERL_BUILD_TYPE_MARKER ".purify"
#elif defined(QUANTIFY)
#define ERL_BUILD_TYPE_MARKER ".quantify"
#elif defined(PURECOV)
#define ERL_BUILD_TYPE_MARKER ".purecov"
#elif defined(VALGRIND)
#define ERL_BUILD_TYPE_MARKER ".valgrind"
#else /* opt */
#define ERL_BUILD_TYPE_MARKER
#endif

#define CHILD_SETUP_PROG_NAME	"child_setup" ERL_BUILD_TYPE_MARKER
#if !DISABLE_VFORK
static char *child_setup_prog;
#endif

#ifdef DEBUG
static int debug_log = 0;
#endif

#ifdef ERTS_SMP
erts_smp_atomic_t erts_got_sigusr1;
#define ERTS_SET_GOT_SIGUSR1 \
  erts_smp_atomic_set(&erts_got_sigusr1, 1)
#define ERTS_UNSET_GOT_SIGUSR1 \
  erts_smp_atomic_set(&erts_got_sigusr1, 0)
static erts_smp_atomic_t have_prepared_crash_dump;
#define ERTS_PREPARED_CRASH_DUMP \
  ((int) erts_smp_atomic_xchg(&have_prepared_crash_dump, 1))
#else
volatile int erts_got_sigusr1;
#define ERTS_SET_GOT_SIGUSR1 (erts_got_sigusr1 = 1)
#define ERTS_UNSET_GOT_SIGUSR1 (erts_got_sigusr1 = 0)
static volatile int have_prepared_crash_dump;
#define ERTS_PREPARED_CRASH_DUMP \
  (have_prepared_crash_dump++)
#endif

static erts_smp_atomic_t sys_misc_mem_sz;

#if defined(ERTS_SMP)
static void smp_sig_notify(char c);
static int sig_notify_fds[2] = {-1, -1};
#elif defined(USE_THREADS)
static int async_fd[2];
#endif

#if CHLDWTHR || defined(ERTS_SMP)
erts_mtx_t chld_stat_mtx;
#endif
#if CHLDWTHR
static erts_tid_t child_waiter_tid;
/* chld_stat_mtx is used to protect against concurrent accesses
   of the driver_data fields pid, alive, and status. */
erts_cnd_t chld_stat_cnd;
static long children_alive;
#define CHLD_STAT_LOCK		erts_mtx_lock(&chld_stat_mtx)
#define CHLD_STAT_UNLOCK	erts_mtx_unlock(&chld_stat_mtx)
#define CHLD_STAT_WAIT		erts_cnd_wait(&chld_stat_cnd, &chld_stat_mtx)
#define CHLD_STAT_SIGNAL	erts_cnd_signal(&chld_stat_cnd)
#elif defined(ERTS_SMP) /* ------------------------------------------------- */
#define CHLD_STAT_LOCK		erts_mtx_lock(&chld_stat_mtx)
#define CHLD_STAT_UNLOCK	erts_mtx_unlock(&chld_stat_mtx)

#else /* ------------------------------------------------------------------- */
#define CHLD_STAT_LOCK
#define CHLD_STAT_UNLOCK
static volatile int children_died;
#endif


static struct fd_data {
    char  pbuf[4];   /* hold partial packet bytes */
    int   psz;       /* size of pbuf */
    char  *buf;
    char  *cpos;
    int   sz;
    int   remain;  /* for input on fd */
} *fd_data;			/* indexed by fd */

/* static FUNCTION(int, write_fill, (int, char*, int)); unused? */
static void note_child_death(int, int);

#if CHLDWTHR
static void* child_waiter(void *);
#endif

/********************* General functions ****************************/

/* This is used by both the drivers and general I/O, must be set early */
static int max_files = -1;

/* 
 * a few variables used by the break handler 
 */
#ifdef ERTS_SMP
erts_smp_atomic_t erts_break_requested;
#define ERTS_SET_BREAK_REQUESTED \
  erts_smp_atomic_set(&erts_break_requested, (long) 1)
#define ERTS_UNSET_BREAK_REQUESTED \
  erts_smp_atomic_set(&erts_break_requested, (long) 0)
#else
volatile int erts_break_requested = 0;
#define ERTS_SET_BREAK_REQUESTED (erts_break_requested = 1)
#define ERTS_UNSET_BREAK_REQUESTED (erts_break_requested = 0)
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
    int (*event)(ErlDrvPort, ErlDrvEvent, ErlDrvEventData);
    void (*check_io_interrupt)(int);
    void (*check_io_interrupt_tmd)(int, long);
    void (*check_io)(int);
    Uint (*size)(void);
    Eterm (*info)(void *);
    int (*check_io_debug)(void);
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

Eterm erts_check_io_info(void *p)
{
    return (*io_func.info)(p);
}

int
erts_check_io_debug(void)
{
    return (*io_func.check_io_debug)();
}


static void
init_check_io(void)
{
    if (erts_use_kernel_poll) {
	io_func.select			= driver_select_kp;
	io_func.event			= driver_event_kp;
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
	io_func.event			= driver_event_nkp;
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

#define ERTS_CHK_IO_INTR	erts_check_io_interrupt
#define ERTS_CHK_IO_INTR_TMD	erts_check_io_interrupt_timed
#define ERTS_CHK_IO		erts_check_io
#define ERTS_CHK_IO_SZ		erts_check_io_size

#endif

#ifdef ERTS_SMP
void
erts_sys_schedule_interrupt(int set)
{
    ERTS_CHK_IO_INTR(set);
}

void
erts_sys_schedule_interrupt_timed(int set, long msec)
{
    ERTS_CHK_IO_INTR_TMD(set, msec);
}
#endif

Uint
erts_sys_misc_mem_sz(void)
{
    Uint res = ERTS_CHK_IO_SZ();
    res += erts_smp_atomic_read(&sys_misc_mem_sz);
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
MALLOC_USE_HASH(1);
#endif

#ifdef USE_THREADS

#ifdef ERTS_THR_HAVE_SIG_FUNCS
/*
 * Child thread inherits parents signal mask at creation. In order to
 * guarantee that the main thread will receive all SIGINT, SIGCHLD, and
 * SIGUSR1 signals sent to the process, we block these signals in the
 * parent thread when creating a new thread.
 */

static sigset_t thr_create_sigmask;

#endif /* #ifdef ERTS_THR_HAVE_SIG_FUNCS */

typedef struct {
#ifdef ERTS_THR_HAVE_SIG_FUNCS
    sigset_t saved_sigmask;
#endif
    int unbind_child;
} erts_thr_create_data_t;

/*
 * thr_create_prepare() is called in parent thread before thread creation.
 * Returned value is passed as argument to thr_create_cleanup().
 */
static void *
thr_create_prepare(void)
{
    erts_thr_create_data_t *tcdp;
    ErtsSchedulerData *esdp;

    tcdp = erts_alloc(ERTS_ALC_T_TMP, sizeof(erts_thr_create_data_t));

#ifdef ERTS_THR_HAVE_SIG_FUNCS
    erts_thr_sigmask(SIG_BLOCK, &thr_create_sigmask, &tcdp->saved_sigmask);
#endif
    esdp = erts_get_scheduler_data();
    tcdp->unbind_child = esdp && erts_is_scheduler_bound(esdp);

    return (void *) tcdp;
}


/* thr_create_cleanup() is called in parent thread after thread creation. */
static void
thr_create_cleanup(void *vtcdp)
{
    erts_thr_create_data_t *tcdp = (erts_thr_create_data_t *) vtcdp;

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

    if (tcdp->unbind_child) {
	erts_smp_rwmtx_rlock(&erts_cpu_bind_rwmtx);
	erts_unbind_from_cpu(erts_cpuinfo);
	erts_smp_rwmtx_runlock(&erts_cpu_bind_rwmtx);
    }
    
}

#endif /* #ifdef USE_THREADS */

void
erts_sys_pre_init(void)
{
    erts_printf_add_cr_to_stdout = 1;
    erts_printf_add_cr_to_stderr = 1;
#ifdef USE_THREADS
    {
    erts_thr_init_data_t eid = ERTS_THR_INIT_DATA_DEF_INITER;

    eid.thread_create_child_func = thr_create_prepare_child;
    /* Before creation in parent */
    eid.thread_create_prepare_func = thr_create_prepare;
    /* After creation in parent */
    eid.thread_create_parent_func = thr_create_cleanup,

#ifdef ERTS_THR_HAVE_SIG_FUNCS
    sigemptyset(&thr_create_sigmask);
    sigaddset(&thr_create_sigmask, SIGINT);   /* block interrupt */
    sigaddset(&thr_create_sigmask, SIGCHLD);  /* block child signals */
    sigaddset(&thr_create_sigmask, SIGUSR1);  /* block user defined signal */
#endif

    erts_thr_init(&eid);

    report_exit_list = NULL;

#ifdef ERTS_ENABLE_LOCK_COUNT
    erts_lcnt_init();
#endif

#if CHLDWTHR || defined(ERTS_SMP)
    erts_mtx_init(&chld_stat_mtx, "child_status");
#endif
#if CHLDWTHR
#ifndef ERTS_SMP
    report_exit_transit_list = NULL;
#endif
    erts_cnd_init(&chld_stat_cnd);
    children_alive = 0;
#endif
    }
#ifdef ERTS_SMP
    erts_smp_atomic_init(&erts_break_requested, 0);
    erts_smp_atomic_init(&erts_got_sigusr1, 0);
    erts_smp_atomic_init(&have_prepared_crash_dump, 0);
#else
    erts_break_requested = 0;
    erts_got_sigusr1 = 0;
    have_prepared_crash_dump = 0;
#endif
#if !CHLDWTHR && !defined(ERTS_SMP)
    children_died = 0;
#endif
#endif /* USE_THREADS */
    erts_smp_atomic_init(&sys_misc_mem_sz, 0);
}

void
erl_sys_init(void)
{
    erts_smp_rwmtx_init(&environ_rwmtx, "environ");
#if !DISABLE_VFORK
 {
    int res;
    char bindir[MAXPATHLEN];
    size_t bindirsz = sizeof(bindir);
    Uint csp_path_sz;

    res = erts_sys_getenv("BINDIR", bindir, &bindirsz);
    if (res != 0) {
	if (res < 0)
	    erl_exit(-1,
		     "Environment variable BINDIR is not set\n");
	if (res > 0)
	    erl_exit(-1,
		     "Value of environment variable BINDIR is too large\n");
    }
    if (bindir[0] != DIR_SEPARATOR_CHAR)
	erl_exit(-1,
		 "Environment variable BINDIR does not contain an"
		 " absolute path\n");
    csp_path_sz = (strlen(bindir)
		   + 1 /* DIR_SEPARATOR_CHAR */
		   + sizeof(CHILD_SETUP_PROG_NAME)
		   + 1);
    child_setup_prog = erts_alloc(ERTS_ALC_T_CS_PROG_PATH, csp_path_sz);
    erts_smp_atomic_add(&sys_misc_mem_sz, csp_path_sz);
    sprintf(child_setup_prog,
            "%s%c%s",
            bindir,
            DIR_SEPARATOR_CHAR,
            CHILD_SETUP_PROG_NAME);
 }
#endif

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

#ifdef SIG_SIGSET		/* Old SysV */
RETSIGTYPE (*sys_sigset(sig, func))()
int sig;
RETSIGTYPE (*func)();
{
    return(sigset(sig, func));
}
void sys_sigblock(int sig)
{
    sighold(sig);
}
void sys_sigrelease(int sig)
{
    sigrelse(sig);
}
#else /* !SIG_SIGSET */
#ifdef SIG_SIGNAL		/* Old BSD */
RETSIGTYPE (*sys_sigset(sig, func))(int, int)
int sig;
RETSIGTYPE (*func)();
{
    return(signal(sig, func));
}
sys_sigblock(int sig)
{
    sigblock(sig);
}
sys_sigrelease(int sig)
{
    sigsetmask(sigblock(0) & ~sigmask(sig));
}
#else /* !SIG_SIGNAL */	/* The True Way - POSIX!:-) */
RETSIGTYPE (*sys_sigset(int sig, RETSIGTYPE (*func)(int)))(int)
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
#endif /* !SIG_SIGNAL */
#endif /* !SIG_SIGSET */

#if (0) /* not used? -- gordon */
static void (*break_func)();
static RETSIGTYPE break_handler(int sig)
{
#ifdef QNX
    /* Turn off SIGCHLD during break processing */
    sys_sigblock(SIGCHLD);
#endif
    (*break_func)();
#ifdef QNX
    sys_sigrelease(SIGCHLD);
#endif
}
#endif /* 0 */

static ERTS_INLINE void
prepare_crash_dump(void)
{
    int i, max;
    char env[21]; /* enough to hold any 64-bit integer */
    size_t envsz;

    if (ERTS_PREPARED_CRASH_DUMP)
	return; /* We have already been called */

    /* Make sure we unregister at epmd (unknown fd) and get at least
       one free filedescriptor (for erl_crash.dump) */
    max = max_files;
    if (max < 1024)
	max = 1024;
    for (i = 3; i < max; i++) {
#if defined(ERTS_SMP)
	/* We don't want to close the signal notification pipe... */
	if (i == sig_notify_fds[0] || i == sig_notify_fds[1])
	    continue;
#elif defined(USE_THREADS)
	/* We don't want to close the async notification pipe... */
	if (i == async_fd[0] || i == async_fd[1])
	    continue;
#endif
	close(i);
    }

    envsz = sizeof(env);
    i = erts_sys_getenv("ERL_CRASH_DUMP_NICE", env, &envsz);
    if (i >= 0) {
	int nice_val;
	nice_val = i != 0 ? 0 : atoi(env);
	if (nice_val > 39) {
	    nice_val = 39;
	}
	erts_silence_warn_unused_result(nice(nice_val));
    }
    
    envsz = sizeof(env);
    i = erts_sys_getenv("ERL_CRASH_DUMP_SECONDS", env, &envsz);
    if (i >= 0) {
	unsigned sec;
	sec = (unsigned) i != 0 ? 0 : atoi(env);
	alarm(sec);
    }

}

void
erts_sys_prepare_crash_dump(void)
{
    prepare_crash_dump();
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
      erl_exit(ERTS_INTR_EXIT, "");

  ERTS_SET_BREAK_REQUESTED;
  ERTS_CHK_IO_INTR(1); /* Make sure we don't sleep in poll */
}

/* set up signal handlers for break and quit */
#if (defined(SIG_SIGSET) || defined(SIG_SIGNAL))
static RETSIGTYPE request_break(void)
#else
static RETSIGTYPE request_break(int signum)
#endif
{
#ifdef ERTS_SMP
    smp_sig_notify('I');
#else
    break_requested();
#endif
}

static ERTS_INLINE void
sigusr1_exit(void)
{
   /* We do this at interrupt level, since the main reason for
      wanting to generate a crash dump in this way is that the emulator
      is hung somewhere, so it won't be able to poll any flag we set here.
      */
    ERTS_SET_GOT_SIGUSR1;
    prepare_crash_dump();
    erl_exit(1, "Received SIGUSR1\n");
}

#ifdef ETHR_UNUSABLE_SIGUSRX
#warning "Unusable SIGUSR1 & SIGUSR2. Disabling use of these signals"
#endif

#ifndef ETHR_UNUSABLE_SIGUSRX

#if (defined(SIG_SIGSET) || defined(SIG_SIGNAL))
static RETSIGTYPE user_signal1(void)
#else
static RETSIGTYPE user_signal1(int signum)
#endif
{
#ifdef ERTS_SMP
   smp_sig_notify('1');
#else
   sigusr1_exit();
#endif
}

#ifdef QUANTIFY
#if (defined(SIG_SIGSET) || defined(SIG_SIGNAL))
static RETSIGTYPE user_signal2(void)
#else
static RETSIGTYPE user_signal2(int signum)
#endif
{
#ifdef ERTS_SMP
   smp_sig_notify('2');
#else
   quantify_save_data();
#endif
}
#endif

#endif /* #ifndef ETHR_UNUSABLE_SIGUSRX */

static void
quit_requested(void)
{
    erl_exit(ERTS_INTR_EXIT, "");
}

#if (defined(SIG_SIGSET) || defined(SIG_SIGNAL))
static RETSIGTYPE do_quit(void)
#else
static RETSIGTYPE do_quit(int signum)
#endif
{
#ifdef ERTS_SMP
    smp_sig_notify('Q');
#else
    quit_requested();
#endif
}

/* Disable break */
void erts_set_ignore_break(void) {
    sys_sigset(SIGINT,  SIG_IGN);
    sys_sigset(SIGQUIT, SIG_IGN);
    sys_sigset(SIGTSTP, SIG_IGN);
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
   sys_sigset(SIGINT, request_break);
#ifndef ETHR_UNUSABLE_SIGUSRX
   sys_sigset(SIGUSR1, user_signal1);
#ifdef QUANTIFY
   sys_sigset(SIGUSR2, user_signal2);
#endif
#endif /* #ifndef ETHR_UNUSABLE_SIGUSRX */
   sys_sigset(SIGQUIT, do_quit);
}

int sys_max_files(void)
{
   return(max_files);
}

static void block_signals(void)
{
#if !CHLDWTHR
   sys_sigblock(SIGCHLD);
#endif
#ifndef ERTS_SMP
   sys_sigblock(SIGINT);
#ifndef ETHR_UNUSABLE_SIGUSRX
   sys_sigblock(SIGUSR1);
#endif
#endif
}

static void unblock_signals(void)
{
    /* Update erl_child_setup.c if changed */
#if !CHLDWTHR
   sys_sigrelease(SIGCHLD);
#endif
#ifndef ERTS_SMP
   sys_sigrelease(SIGINT);
#ifndef ETHR_UNUSABLE_SIGUSRX
   sys_sigrelease(SIGUSR1);
#endif /* #ifndef ETHR_UNUSABLE_SIGUSRX */
#endif
}
/************************** Time stuff **************************/
#ifdef HAVE_GETHRTIME
#ifdef GETHRTIME_WITH_CLOCK_GETTIME

SysHrTime sys_gethrtime(void)
{
    struct timespec ts;
    long long result;
    if (clock_gettime(CLOCK_MONOTONIC,&ts) != 0) {
	erl_exit(1,"Fatal, could not get clock_monotonic value!, "
		 "errno = %d\n", errno);
    }
    result = ((long long) ts.tv_sec) * 1000000000LL + 
	((long long) ts.tv_nsec);
    return (SysHrTime) result;
}
#endif
#endif

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

void
os_flavor(char* namebuf, 	/* Where to return the name. */
	  unsigned size) 	/* Size of name buffer. */
{
    static int called = 0;
    static struct utsname uts;	/* Information about the system. */

    if (!called) {
	char* s;

	(void) uname(&uts);
	called = 1;
	for (s = uts.sysname; *s; s++) {
	    if (isupper((int) *s)) {
		*s = tolower((int) *s);
	    }
	}
    }
    strcpy(namebuf, uts.sysname);
}

void
os_version(pMajor, pMinor, pBuild)
int* pMajor;			/* Pointer to major version. */
int* pMinor;			/* Pointer to minor version. */
int* pBuild;			/* Pointer to build number. */
{
    struct utsname uts;		/* Information about the system. */
    char* release;		/* Pointer to the release string:
				 * X.Y or X.Y.Z.
				 */

    (void) uname(&uts);
    release = uts.release;
    *pMajor = get_number(&release);
    *pMinor = get_number(&release);
    *pBuild = get_number(&release);
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


/************************** Port I/O *******************************/



/* I. Common stuff */

/*
 * Decreasing the size of it below 16384 is not allowed.
 */

/* II. The spawn/fd/vanilla drivers */

#define ERTS_SYS_READ_BUF_SZ (64*1024)

/* This data is shared by these drivers - initialized by spawn_init() */
static struct driver_data {
    int port_num, ofd, packet_bytes;
    ErtsSysReportExit *report_exit;
    int pid;
    int alive;
    int status;
} *driver_data;			/* indexed by fd */

/* Driver interfaces */
static ErlDrvData spawn_start(ErlDrvPort, char*, SysDriverOpts*);
static ErlDrvData fd_start(ErlDrvPort, char*, SysDriverOpts*);
static int fd_control(ErlDrvData, unsigned int, char *, int, char **, int);
static ErlDrvData vanilla_start(ErlDrvPort, char*, SysDriverOpts*);
static int spawn_init(void);
static void fd_stop(ErlDrvData);
static void stop(ErlDrvData);
static void ready_input(ErlDrvData, ErlDrvEvent);
static void ready_output(ErlDrvData, ErlDrvEvent);
static void output(ErlDrvData, char*, int);
static void outputv(ErlDrvData, ErlIOVec*);
static void stop_select(ErlDrvEvent, void*);

struct erl_drv_entry spawn_driver_entry = {
    spawn_init,
    spawn_start,
    stop,
    output,
    ready_input,
    ready_output,
    "spawn",
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    ERL_DRV_EXTENDED_MARKER,
    ERL_DRV_EXTENDED_MAJOR_VERSION,
    ERL_DRV_EXTENDED_MINOR_VERSION,
    ERL_DRV_FLAG_USE_PORT_LOCKING,
    NULL, NULL,
    stop_select
};
struct erl_drv_entry fd_driver_entry = {
    NULL,
    fd_start,
    fd_stop,
    output,
    ready_input,
    ready_output, 
    "fd",
    NULL,
    NULL,
    fd_control,
    NULL,
    outputv,
    NULL, /* ready_async */
    NULL, /* flush */
    NULL, /* call */
    NULL, /* event */
    ERL_DRV_EXTENDED_MARKER,
    ERL_DRV_EXTENDED_MAJOR_VERSION,
    ERL_DRV_EXTENDED_MINOR_VERSION,
    0, /* ERL_DRV_FLAGs */
    NULL, /* handle2 */
    NULL, /* process_exit */
    stop_select
};
struct erl_drv_entry vanilla_driver_entry = {
    NULL,
    vanilla_start,
    stop,
    output,
    ready_input,
    ready_output,
    "vanilla",
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL, /* flush */
    NULL, /* call */
    NULL, /* event */
    ERL_DRV_EXTENDED_MARKER,
    ERL_DRV_EXTENDED_MAJOR_VERSION,
    ERL_DRV_EXTENDED_MINOR_VERSION,
    0, /* ERL_DRV_FLAGs */
    NULL, /* handle2 */
    NULL, /* process_exit */
    stop_select
};

#if defined(USE_THREADS) && !defined(ERTS_SMP)
static int  async_drv_init(void);
static ErlDrvData async_drv_start(ErlDrvPort, char*, SysDriverOpts*);
static void async_drv_stop(ErlDrvData);
static void async_drv_input(ErlDrvData, ErlDrvEvent);

/* INTERNAL use only */

struct erl_drv_entry async_driver_entry = {
    async_drv_init,
    async_drv_start,
    async_drv_stop,
    NULL,
    async_drv_input,
    NULL,
    "async",
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL
};
#endif

/* Handle SIGCHLD signals. */
#if (defined(SIG_SIGSET) || defined(SIG_SIGNAL))
static RETSIGTYPE onchld(void)
#else
static RETSIGTYPE onchld(int signum)
#endif
{
#if CHLDWTHR
    ASSERT(0); /* We should *never* catch a SIGCHLD signal */
#elif defined(ERTS_SMP)
    smp_sig_notify('C');
#else
    children_died = 1;
    ERTS_CHK_IO_INTR(1); /* Make sure we don't sleep in poll */
#endif
}

static int set_driver_data(int port_num,
			   int ifd,
			   int ofd,
			   int packet_bytes,
			   int read_write,
			   int exit_status,
			   int pid)
{
    ErtsSysReportExit *report_exit;

    if (!exit_status)
	report_exit = NULL;
    else {
	report_exit = erts_alloc(ERTS_ALC_T_PRT_REP_EXIT,
				 sizeof(ErtsSysReportExit));
	report_exit->next = report_exit_list;
	report_exit->port = erts_port[port_num].id;
	report_exit->pid = pid;
	report_exit->ifd = read_write & DO_READ ? ifd : -1;
	report_exit->ofd = read_write & DO_WRITE ? ofd : -1;
#if CHLDWTHR && !defined(ERTS_SMP)
	report_exit->status = 0;
#endif
	report_exit_list = report_exit;
    }

    if (read_write & DO_READ) {
	driver_data[ifd].packet_bytes = packet_bytes;
	driver_data[ifd].port_num = port_num;
	driver_data[ifd].report_exit = report_exit;
	driver_data[ifd].pid = pid;
	driver_data[ifd].alive = 1;
	driver_data[ifd].status = 0;
	if (read_write & DO_WRITE) {
	    driver_data[ifd].ofd = ofd;
	    if (ifd != ofd)
		driver_data[ofd] = driver_data[ifd];  /* structure copy */
	} else {		/* DO_READ only */
	    driver_data[ifd].ofd = -1;
	}
	(void) driver_select(port_num, ifd, (ERL_DRV_READ|ERL_DRV_USE), 1);
	return(ifd);
    } else {			/* DO_WRITE only */
	driver_data[ofd].packet_bytes = packet_bytes;
	driver_data[ofd].port_num = port_num;
	driver_data[ofd].report_exit = report_exit;
	driver_data[ofd].ofd = ofd;
	driver_data[ofd].pid = pid;
	driver_data[ofd].alive = 1;
	driver_data[ofd].status = 0;
	return(ofd);
    }
}

static int spawn_init()
{
   int i;
#if CHLDWTHR
   erts_thr_opts_t thr_opts = ERTS_THR_OPTS_DEFAULT_INITER;
   thr_opts.detached = 0;
   thr_opts.suggested_stack_size = 0; /* Smallest possible */
#endif

   sys_sigset(SIGPIPE, SIG_IGN); /* Ignore - we'll handle the write failure */
   driver_data = (struct driver_data *)
       erts_alloc(ERTS_ALC_T_DRV_TAB, max_files * sizeof(struct driver_data));
   erts_smp_atomic_add(&sys_misc_mem_sz,
		       max_files * sizeof(struct driver_data));

   for (i = 0; i < max_files; i++)
      driver_data[i].pid = -1;

#if CHLDWTHR
   sys_sigblock(SIGCHLD);
#endif

   sys_sigset(SIGCHLD, onchld); /* Reap children */

#if CHLDWTHR
   erts_thr_create(&child_waiter_tid, child_waiter, NULL, &thr_opts);
#endif

   return 1;
}

static void close_pipes(int ifd[2], int ofd[2], int read_write)
{
    if (read_write & DO_READ) {
	(void) close(ifd[0]);
	(void) close(ifd[1]);
    }
    if (read_write & DO_WRITE) {
	(void) close(ofd[0]);
	(void) close(ofd[1]);
    }
}

static void init_fd_data(int fd, int prt)
{
    fd_data[fd].buf = NULL;
    fd_data[fd].cpos = NULL;
    fd_data[fd].remain = 0;
    fd_data[fd].sz = 0;
    fd_data[fd].psz = 0;
}

static char **build_unix_environment(char *block)
{
    int i;
    int j;
    int len;
    char *cp;
    char **cpp;
    char** old_env;
    
    ERTS_SMP_LC_ASSERT(erts_smp_lc_rwmtx_is_rlocked(&environ_rwmtx));

    cp = block;
    len = 0;
    while (*cp != '\0') {
	cp += strlen(cp) + 1;
	len++;
    }
    old_env = environ;
    while (*old_env++ != NULL) {
	len++;
    }
    
    cpp = (char **) erts_alloc_fnf(ERTS_ALC_T_ENVIRONMENT,
				   sizeof(char *) * (len+1));
    if (cpp == NULL) {
	return NULL;
    }

    cp = block;
    len = 0;
    while (*cp != '\0') {
	cpp[len] = cp;
	cp += strlen(cp) + 1;
	len++;
    }
    
    i = len;
    for (old_env = environ; *old_env; old_env++) {
	char* old = *old_env;

	for (j = 0; j < len; j++) {
	    char *s, *t;

	    s = cpp[j];
	    t = old;
	    while (*s == *t && *s != '=') {
		s++, t++;
	    }
	    if (*s == '=' && *t == '=') {
		break;
	    }
	}

	if (j == len) {		/* New version not found */
	    cpp[len++] = old;
	}
    }

    for (j = 0; j < i; ) {
        size_t last = strlen(cpp[j])-1;
	if (cpp[j][last] == '=' && strchr(cpp[j], '=') == cpp[j]+last) {
	    cpp[j] = cpp[--len];
	    if (len < i) {
		i--;
	    } else {
		j++;
	    }
	}
	else {
	    j++;
	}
    }

    cpp[len] = NULL;
    return cpp;
}

/*
  [arndt] In most Unix systems, including Solaris 2.5, 'fork' allocates memory
  in swap space for the child of a 'fork', whereas 'vfork' does not do this.
  The natural call to use here is therefore 'vfork'. Due to a bug in
  'vfork' in Solaris 2.5 (apparently fixed in 2.6), using 'vfork'
  can be dangerous in what seems to be these circumstances:
      If the child code under a vfork sets the signal action to SIG_DFL
      (or SIG_IGN)
      for any signal which was previously set to a signal handler, the
      state of the parent is clobbered, so that the later arrival of
      such a signal yields a sigsegv in the parent. If the signal was
      not set to a signal handler, but ignored, all seems to work.
  If you change the forking code below, beware of this.
 */

static ErlDrvData spawn_start(ErlDrvPort port_num, char* name, SysDriverOpts* opts)
{
#define CMD_LINE_PREFIX_STR "exec "
#define CMD_LINE_PREFIX_STR_SZ (sizeof(CMD_LINE_PREFIX_STR) - 1)

    int ifd[2], ofd[2], len, pid, i;
    char **volatile new_environ; /* volatile since a vfork() then cannot
				    cause 'new_environ' to be clobbered
				    in the parent process. */
    int saved_errno;
    long res;
    char *cmd_line;
#ifndef QNX
    int unbind;
#endif
#if !DISABLE_VFORK
    int no_vfork;
    size_t no_vfork_sz = sizeof(no_vfork);

    no_vfork = (erts_sys_getenv("ERL_NO_VFORK",
				(char *) &no_vfork,
				&no_vfork_sz) >= 0);
#endif

    switch (opts->read_write) {
    case DO_READ:
	if (pipe(ifd) < 0)
	    return ERL_DRV_ERROR_ERRNO;
	if (ifd[0] >= max_files) {
	    close_pipes(ifd, ofd, opts->read_write);
	    errno = EMFILE;
	    return ERL_DRV_ERROR_ERRNO;
	}
	ofd[1] = -1;		/* keep purify happy */
	break;
    case DO_WRITE:
	if (pipe(ofd) < 0) return ERL_DRV_ERROR_ERRNO;
	if (ofd[1] >= max_files) {
	    close_pipes(ifd, ofd, opts->read_write);
	    errno = EMFILE;
	    return ERL_DRV_ERROR_ERRNO;
	}
	ifd[0] = -1;		/* keep purify happy */
	break;
    case DO_READ|DO_WRITE:
	if (pipe(ifd) < 0) return ERL_DRV_ERROR_ERRNO;
	errno = EMFILE;		/* default for next two conditions */
	if (ifd[0] >= max_files || pipe(ofd) < 0) {
	    close_pipes(ifd, ofd, DO_READ);
	    return ERL_DRV_ERROR_ERRNO;
	}
	if (ofd[1] >= max_files) {
	    close_pipes(ifd, ofd, opts->read_write);
	    errno = EMFILE;
	    return ERL_DRV_ERROR_ERRNO;
	}
	break;
    default:
	ASSERT(0);
	return ERL_DRV_ERROR_GENERAL;
    }

    if (opts->spawn_type == ERTS_SPAWN_EXECUTABLE) {
	/* started with spawn_executable, not with spawn */
	len = strlen(name);
	cmd_line = (char *) erts_alloc_fnf(ERTS_ALC_T_TMP, len + 1);
	if (!cmd_line) {
	    close_pipes(ifd, ofd, opts->read_write);
	    errno = ENOMEM;
	    return ERL_DRV_ERROR_ERRNO;
	}
	memcpy((void *) cmd_line,(void *) name, len);
	cmd_line[len] = '\0';
	if (access(cmd_line,X_OK) != 0) {
	    int save_errno = errno;
	    erts_free(ERTS_ALC_T_TMP, cmd_line);
	    errno = save_errno;
	    return ERL_DRV_ERROR_ERRNO;
	}
    } else {
	/* make the string suitable for giving to "sh" */
	len = strlen(name);
	cmd_line = (char *) erts_alloc_fnf(ERTS_ALC_T_TMP,
					   CMD_LINE_PREFIX_STR_SZ + len + 1);
	if (!cmd_line) {
	    close_pipes(ifd, ofd, opts->read_write);
	    errno = ENOMEM;
	    return ERL_DRV_ERROR_ERRNO;
	}
	memcpy((void *) cmd_line,
	       (void *) CMD_LINE_PREFIX_STR,
	       CMD_LINE_PREFIX_STR_SZ);
	memcpy((void *) (cmd_line + CMD_LINE_PREFIX_STR_SZ), (void *) name, len);
	cmd_line[CMD_LINE_PREFIX_STR_SZ + len] = '\0';
    }

    erts_smp_rwmtx_rlock(&environ_rwmtx);

    if (opts->envir == NULL) {
	new_environ = environ;
    } else if ((new_environ = build_unix_environment(opts->envir)) == NULL) {
	erts_smp_rwmtx_runlock(&environ_rwmtx);
	erts_free(ERTS_ALC_T_TMP, (void *) cmd_line);
	errno = ENOMEM;
	return ERL_DRV_ERROR_ERRNO;
    } 

#ifndef QNX
    /* Block child from SIGINT and SIGUSR1. Must be before fork()
       to be safe. */
    block_signals();

    CHLD_STAT_LOCK;

    unbind = erts_is_scheduler_bound(NULL);
    if (unbind)
	erts_smp_rwmtx_rlock(&erts_cpu_bind_rwmtx);

#if !DISABLE_VFORK
    /* See fork/vfork discussion before this function. */
    if (no_vfork) {
#endif

	DEBUGF(("Using fork\n"));
	pid = fork();

	if (pid == 0) {
	    /* The child! Setup child... */

	    if (unbind && erts_unbind_from_cpu(erts_cpuinfo) != 0)
		goto child_error;

	    /* OBSERVE!
	     * Keep child setup after vfork() (implemented below and in
	     * erl_child_setup.c) up to date if changes are made here.
	     */

	    if (opts->use_stdio) {
		if (opts->read_write & DO_READ) {
		    /* stdout for process */
		    if (dup2(ifd[1], 1) < 0)
			goto child_error;
		    if(opts->redir_stderr)
			/* stderr for process */
			if (dup2(ifd[1], 2) < 0)
			    goto child_error;
		}
		if (opts->read_write & DO_WRITE)
		    /* stdin for process */
		    if (dup2(ofd[0], 0) < 0)
			goto child_error;
	    }
	    else {	/* XXX will fail if ofd[0] == 4 (unlikely..) */
		if (opts->read_write & DO_READ)
		    if (dup2(ifd[1], 4) < 0)
			goto child_error;
		if (opts->read_write & DO_WRITE)
		    if (dup2(ofd[0], 3) < 0)
			goto child_error;
	    }

	    for (i = opts->use_stdio ? 3 : 5; i < max_files; i++)
		(void) close(i);
	    
	    if (opts->wd && chdir(opts->wd) < 0)
		goto child_error;

#if defined(USE_SETPGRP_NOARGS)		/* SysV */
	    (void) setpgrp();
#elif defined(USE_SETPGRP)		/* BSD */
	    (void) setpgrp(0, getpid());
#else					/* POSIX */
	    (void) setsid();
#endif
	    
	    unblock_signals();

	    if (opts->spawn_type == ERTS_SPAWN_EXECUTABLE) {
		if (opts->argv == NULL) {
		    execle(cmd_line,cmd_line,(char *) NULL, new_environ);
		} else {
		    if (opts->argv[0] == erts_default_arg0) {
			opts->argv[0] = cmd_line;
		    }
		    execve(cmd_line, opts->argv, new_environ);
		    if (opts->argv[0] == cmd_line) {
			opts->argv[0] = erts_default_arg0;
		    }
		}
	    } else {
		execle("/bin/sh", "sh", "-c", cmd_line, (char *) NULL, new_environ);
	    }
	child_error:
	    _exit(1);
	}
#if !DISABLE_VFORK
    }
    else { /* Use vfork() */
	char **cs_argv= erts_alloc(ERTS_ALC_T_TMP,(CS_ARGV_NO_OF_ARGS + 1)*
				   sizeof(char *));
	char fd_close_range[44];                  /* 44 bytes are enough to  */
	char dup2_op[CS_ARGV_NO_OF_DUP2_OPS][44]; /* hold any "%d:%d" string */
                                                  /* on a 64-bit machine.    */

	/* Setup argv[] for the child setup program (implemented in
	   erl_child_setup.c) */
	i = 0;
	if (opts->use_stdio) {
	    if (opts->read_write & DO_READ){
		/* stdout for process */
		sprintf(&dup2_op[i++][0], "%d:%d", ifd[1], 1);
		if(opts->redir_stderr)
		    /* stderr for process */
		    sprintf(&dup2_op[i++][0], "%d:%d", ifd[1], 2);
	    }
	    if (opts->read_write & DO_WRITE)
		/* stdin for process */
		sprintf(&dup2_op[i++][0], "%d:%d", ofd[0], 0);
	} else {	/* XXX will fail if ofd[0] == 4 (unlikely..) */
	    if (opts->read_write & DO_READ)
		sprintf(&dup2_op[i++][0], "%d:%d", ifd[1], 4);
	    if (opts->read_write & DO_WRITE)
		sprintf(&dup2_op[i++][0], "%d:%d", ofd[0], 3);
	}
	for (; i < CS_ARGV_NO_OF_DUP2_OPS; i++)
	    strcpy(&dup2_op[i][0], "-");
	sprintf(fd_close_range, "%d:%d", opts->use_stdio ? 3 : 5, max_files-1);

	cs_argv[CS_ARGV_PROGNAME_IX] = child_setup_prog;
	cs_argv[CS_ARGV_WD_IX] = opts->wd ? opts->wd : ".";
	cs_argv[CS_ARGV_UNBIND_IX]
	    = (unbind ? erts_get_unbind_from_cpu_str(erts_cpuinfo) : "false");
	cs_argv[CS_ARGV_FD_CR_IX] = fd_close_range;
	for (i = 0; i < CS_ARGV_NO_OF_DUP2_OPS; i++)
	    cs_argv[CS_ARGV_DUP2_OP_IX(i)] = &dup2_op[i][0];

	if (opts->spawn_type == ERTS_SPAWN_EXECUTABLE) {
	    int num = 0;
	    int j = 0;
	    if (opts->argv != NULL) {
		for(; opts->argv[num] != NULL; ++num)
		    ;
	    }
	    cs_argv = erts_realloc(ERTS_ALC_T_TMP,cs_argv, (CS_ARGV_NO_OF_ARGS + 1 + num + 1) * sizeof(char *));
	    cs_argv[CS_ARGV_CMD_IX] = "-";
	    cs_argv[CS_ARGV_NO_OF_ARGS] = cmd_line;
	    if (opts->argv != NULL) {
		for (;opts->argv[j] != NULL; ++j) {
		    if (opts->argv[j] == erts_default_arg0) {
			cs_argv[CS_ARGV_NO_OF_ARGS + 1 + j] = cmd_line;
		    } else {
			cs_argv[CS_ARGV_NO_OF_ARGS + 1 + j] = opts->argv[j];
		    }
		}
	    }
	    cs_argv[CS_ARGV_NO_OF_ARGS + 1 + j] = NULL;
	} else {
	    cs_argv[CS_ARGV_CMD_IX] = cmd_line; /* Command */
	    cs_argv[CS_ARGV_NO_OF_ARGS] = NULL;
	}
	DEBUGF(("Using vfork\n"));
	pid = vfork();

	if (pid == 0) {
	    /* The child! */

	    /* Observe!
	     * OTP-4389: The child setup program (implemented in
	     * erl_child_setup.c) will perform the necessary setup of the
	     * child before it execs to the user program. This because
	     * vfork() only allow an *immediate* execve() or _exit() in the
	     * child.
	     */
	    execve(child_setup_prog, cs_argv, new_environ);
	    _exit(1);
	}
	erts_free(ERTS_ALC_T_TMP,cs_argv);
    }
#endif

    if (unbind)
	erts_smp_rwmtx_runlock(&erts_cpu_bind_rwmtx);

    if (pid == -1) {
        saved_errno = errno;
	CHLD_STAT_UNLOCK;
	erts_smp_rwmtx_runlock(&environ_rwmtx);
	erts_free(ERTS_ALC_T_TMP, (void *) cmd_line);
        unblock_signals();
        close_pipes(ifd, ofd, opts->read_write);
	errno = saved_errno;
	return ERL_DRV_ERROR_ERRNO;
    }
#else /* QNX */
    if (opts->use_stdio) {
	if (opts->read_write & DO_READ)
	    qnx_spawn_options.iov[1] = ifd[1];  /* stdout for process */
	if (opts->read_write & DO_WRITE)
	    qnx_spawn_options.iov[0] = ofd[0];  /* stdin for process */
	} 
    else {
	if (opts->read_write & DO_READ)
	    qnx_spawn_options.iov[4] = ifd[1];
	if (opts->read_write & DO_WRITE)
	    qnx_spawn_options.iov[3] = ofd[0];
    }
    /* Close fds on exec */
    for (i = 3; i < max_files; i++)
	fcntl(i, F_SETFD, 1);

    qnx_spawn_options.flags = _SPAWN_SETSID;
    if ((pid = spawnl(P_NOWAIT, "/bin/sh", "/bin/sh", "-c", cmd_line, 
                      (char *) 0)) < 0) {
	erts_free(ERTS_ALC_T_TMP, (void *) cmd_line);
        reset_qnx_spawn();
	erts_smp_rwmtx_runlock(&environ_rwmtx);
	close_pipes(ifd, ofd, opts->read_write);
	return ERL_DRV_ERROR_GENERAL;
    }
    reset_qnx_spawn();
#endif /* QNX */

    erts_free(ERTS_ALC_T_TMP, (void *) cmd_line);

    if (new_environ != environ)
	erts_free(ERTS_ALC_T_ENVIRONMENT, (void *) new_environ);

    if (opts->read_write & DO_READ) 
	(void) close(ifd[1]);
    if (opts->read_write & DO_WRITE)
	(void) close(ofd[0]);
	
    if (opts->read_write & DO_READ) {
	SET_NONBLOCKING(ifd[0]);
	init_fd_data(ifd[0], port_num);
    }
    if (opts->read_write & DO_WRITE) {
	SET_NONBLOCKING(ofd[1]);
        init_fd_data(ofd[1], port_num);
    }

    res = set_driver_data(port_num, ifd[0], ofd[1], opts->packet_bytes,
			  opts->read_write, opts->exit_status, pid);
    /* Don't unblock SIGCHLD until now, since the call above must
       first complete putting away the info about our new subprocess. */
    unblock_signals();

#if CHLDWTHR
    ASSERT(children_alive >= 0);

    if (!(children_alive++))
	CHLD_STAT_SIGNAL; /* Wake up child waiter thread if no children
			     was alive before we fork()ed ... */
#endif
    /* Don't unlock chld_stat_mtx until now of the same reason as above */
    CHLD_STAT_UNLOCK;

    erts_smp_rwmtx_runlock(&environ_rwmtx);

    return (ErlDrvData)res;
#undef CMD_LINE_PREFIX_STR
#undef CMD_LINE_PREFIX_STR_SZ
}

#ifdef QNX
static reset_qnx_spawn()
{
    int i;

    /* Reset qnx_spawn_options */
    qnx_spawn_options.flags = 0; 
    qnx_spawn_options.iov[0] = 0xff;
    qnx_spawn_options.iov[1] = 0xff;
    qnx_spawn_options.iov[2] = 0xff;
    qnx_spawn_options.iov[3] = 0xff;
}
#endif

#define FD_DEF_HEIGHT 24
#define FD_DEF_WIDTH 80
/* Control op */
#define FD_CTRL_OP_GET_WINSIZE 100

static int fd_get_window_size(int fd, Uint32 *width, Uint32 *height)
{
#ifdef TIOCGWINSZ 
    struct winsize ws;
    if (ioctl(fd,TIOCGWINSZ,&ws) == 0) {
	*width = (Uint32) ws.ws_col;
	*height = (Uint32) ws.ws_row;
	return 0;
    }
#endif
    return -1;
}

static int fd_control(ErlDrvData drv_data,
		      unsigned int command,
		      char *buf, int len,
		      char **rbuf, int rlen)
{
    int fd = (int)(long)drv_data;
    char resbuff[2*sizeof(Uint32)];
    switch (command) {
    case FD_CTRL_OP_GET_WINSIZE:
	{
	    Uint32 w,h;
	    if (fd_get_window_size(fd,&w,&h)) 
		return 0;
	    memcpy(resbuff,&w,sizeof(Uint32));
	    memcpy(resbuff+sizeof(Uint32),&h,sizeof(Uint32));
	}
	break;
    default:
	return 0;
    }
    if (rlen < 2*sizeof(Uint32)) {
	*rbuf = driver_alloc(2*sizeof(Uint32));
    }
    memcpy(*rbuf,resbuff,2*sizeof(Uint32));
    return 2*sizeof(Uint32);
}

static ErlDrvData fd_start(ErlDrvPort port_num, char* name,
			   SysDriverOpts* opts)
{
    ErlDrvData res;

    if (((opts->read_write & DO_READ) && opts->ifd >= max_files) ||
	((opts->read_write & DO_WRITE) && opts->ofd >= max_files))
	return ERL_DRV_ERROR_GENERAL;

    /*
     * Historical:
     *
     * "Note about nonblocking I/O.
     *
     * At least on Solaris, setting the write end of a TTY to nonblocking,
     * will set the input end to nonblocking as well (and vice-versa).
     * If erl is run in a pipeline like this:  cat | erl
     * the input end of the TTY will be the standard input of cat.
     * And cat is not prepared to handle nonblocking I/O."
     *
     * Actually, the reason for this is not that the tty itself gets set
     * in non-blocking mode, but that the "input end" (cat's stdin) and
     * the "output end" (erlang's stdout) are typically the "same" file
     * descriptor, dup()'ed from a single fd by one of this process'
     * ancestors.
     *
     * The workaround for this problem used to be a rather bad kludge,
     * interposing an extra process ("internal cat") between erlang's
     * stdout and the original stdout, allowing erlang to set its stdout
     * in non-blocking mode without affecting the stdin of the preceding
     * process in the pipeline - and being a kludge, it caused all kinds
     * of weird problems.
     *
     * So, this is the current logic:
     *
     * The only reason to set non-blocking mode on the output fd at all is
     * if it's something that can cause a write() to block, of course,
     * i.e. primarily if it points to a tty, socket, pipe, or fifo. 
     *
     * If we don't set non-blocking mode when we "should" have, and output
     * becomes blocked, the entire runtime system will be suspended - this
     * is normally bad of course, and can happen fairly "easily" - e.g. user
     * hits ^S on tty - but doesn't necessarily happen.
     * 
     * If we do set non-blocking mode when we "shouldn't" have, the runtime
     * system will end up seeing EOF on the input fd (due to the preceding
     * process dying), which typically will cause the entire runtime system
     * to terminate immediately (due to whatever erlang process is seeing
     * the EOF taking it as a signal to halt the system). This is *very* bad.
     * 
     * I.e. we should take a conservative approach, and only set non-
     * blocking mode when we a) need to, and b) are reasonably certain
     * that it won't be a problem. And as in the example above, the problem
     * occurs when input fd and output fd point to different "things".
     *
     * However, determining that they are not just the same "type" of
     * "thing", but actually the same instance of that type of thing, is
     * unreasonably complex in many/most cases.
     *
     * Also, with pipes, sockets, and fifos it's far from obvious that the
     * user *wants* non-blocking output: If you're running erlang inside
     * some complex pipeline, you're probably not running a real-time system
     * that must never stop, but rather *want* it to suspend if the output
     * channel is "full".
     *
     * So, the bottom line: We will only set the output fd non-blocking if
     * it points to a tty, and either a) the input fd also points to a tty,
     * or b) we can make sure that setting the output fd non-blocking
     * doesn't interfere with someone else's input, via a somewhat milder
     * kludge than the above.
     *
     * Also keep in mind that while this code is almost exclusively run as
     * a result of an erlang open_port({fd,0,1}, ...), that isn't the only
     * case - it can be called with any old pre-existing file descriptors,
     * the relations between which (if they're even two) we can only guess
     * at - still, we try our best...
     */

    if (opts->read_write & DO_READ) {
	init_fd_data(opts->ifd, port_num);
    }
    if (opts->read_write & DO_WRITE) {
	init_fd_data(opts->ofd, port_num);

	/* If we don't have a read end, all bets are off - no non-blocking. */
	if (opts->read_write & DO_READ) {

	    if (isatty(opts->ofd)) { /* output fd is a tty:-) */

		if (isatty(opts->ifd)) { /* input fd is also a tty */

		    /* To really do this "right", we should also check that
		       input and output fd point to the *same* tty - but
		       this seems like overkill; ttyname() isn't for free,
		       and this is a very common case - and it's hard to
		       imagine a scenario where setting non-blocking mode
		       here would cause problems - go ahead and do it. */

		    SET_NONBLOCKING(opts->ofd);

		} else {	/* output fd is a tty, input fd isn't */

		    /* This is a "problem case", but also common (see the
		       example above) - i.e. it makes sense to try a bit
		       harder before giving up on non-blocking mode: Try to
		       re-open the tty that the output fd points to, and if
		       successful replace the original one with the "new" fd
		       obtained this way, and set *that* one in non-blocking
		       mode. (Yes, this is a kludge.)

		       However, re-opening the tty may fail in a couple of
		       (unusual) cases:

		       1) The name of the tty (or an equivalent one, i.e.
			  same major/minor number) can't be found, because
			  it actually lives somewhere other than /dev (or
			  wherever ttyname() looks for it), and isn't
			  equivalent to any of those that do live in the
			  "standard" place - this should be *very* unusual.

		       2) Permissions on the tty don't allow us to open it -
			  it's perfectly possible to have an fd open to an
			  object whose permissions wouldn't allow us to open
			  it. This is not as unusual as it sounds, one case
			  is if the user has su'ed to someone else (not
			  root) - we have a read/write fd open to the tty
			  (because it has been inherited all the way down
			  here), but we have neither read nor write
			  permission for the tty.

		       In these cases, we finally give up, and don't set the
		       output fd in non-blocking mode. */

		    char *tty;
		    int nfd;

		    if ((tty = ttyname(opts->ofd)) != NULL &&
			(nfd = open(tty, O_WRONLY)) != -1) {
			dup2(nfd, opts->ofd);
			close(nfd);
			SET_NONBLOCKING(opts->ofd);
		    }
		}
	    }
	}
    }
    CHLD_STAT_LOCK;
    res = (ErlDrvData)(long)set_driver_data(port_num, opts->ifd, opts->ofd,
				      opts->packet_bytes,
				      opts->read_write, 0, -1);
    CHLD_STAT_UNLOCK;
    return res;
}

static void clear_fd_data(int fd) 
{
    if (fd_data[fd].sz > 0) {
	erts_free(ERTS_ALC_T_FD_ENTRY_BUF, (void *) fd_data[fd].buf);
	ASSERT(erts_smp_atomic_read(&sys_misc_mem_sz) >= fd_data[fd].sz);
	erts_smp_atomic_add(&sys_misc_mem_sz, -1*fd_data[fd].sz);
    }
    fd_data[fd].buf = NULL;
    fd_data[fd].sz = 0;
    fd_data[fd].remain = 0;
    fd_data[fd].cpos = NULL;
    fd_data[fd].psz = 0;
}

static void nbio_stop_fd(int prt, int fd)
{
    driver_select(prt,fd,DO_READ|DO_WRITE,0);
    clear_fd_data(fd);
    SET_BLOCKING(fd);
}

static void fd_stop(ErlDrvData fd)  /* Does not close the fds */
{
    int ofd;
    
    nbio_stop_fd(driver_data[(int)(long)fd].port_num, (int)(long)fd);
    ofd = driver_data[(int)(long)fd].ofd;
    if (ofd != (int)(long)fd && ofd != -1) 
	nbio_stop_fd(driver_data[(int)(long)fd].port_num, (int)(long)ofd);
}

static ErlDrvData vanilla_start(ErlDrvPort port_num, char* name,
				SysDriverOpts* opts)
{
    int flags, fd;
    ErlDrvData res;

    flags = (opts->read_write == DO_READ ? O_RDONLY :
	     opts->read_write == DO_WRITE ? O_WRONLY|O_CREAT|O_TRUNC :
	     O_RDWR|O_CREAT);
    if ((fd = open(name, flags, 0666)) < 0)
	return ERL_DRV_ERROR_GENERAL;
    if (fd >= max_files) {
	close(fd);
	return ERL_DRV_ERROR_GENERAL;
    }
    SET_NONBLOCKING(fd);
    init_fd_data(fd, port_num);

    CHLD_STAT_LOCK;
    res = (ErlDrvData)(long)set_driver_data(port_num, fd, fd,
				      opts->packet_bytes,
				      opts->read_write, 0, -1);
    CHLD_STAT_UNLOCK;
    return res;
}

/* Note that driver_data[fd].ifd == fd if the port was opened for reading, */
/* otherwise (i.e. write only) driver_data[fd].ofd = fd.  */

static void stop(ErlDrvData fd)
{
    int prt, ofd;

    prt = driver_data[(int)(long)fd].port_num;
    nbio_stop_fd(prt, (int)(long)fd);

    ofd = driver_data[(int)(long)fd].ofd;
    if (ofd != (int)(long)fd && (int)(long)ofd != -1)
	nbio_stop_fd(prt, ofd);
    else
	ofd = -1;

    CHLD_STAT_LOCK;

    /* Mark as unused. Maybe resetting the 'port_num' slot is better? */
    driver_data[(int)(long)fd].pid = -1;

    CHLD_STAT_UNLOCK;

    /* SMP note: Close has to be last thing done (open file descriptors work
       as locks on driver_data[] entries) */
    driver_select(prt, (int)(long)fd, ERL_DRV_USE, 0);  /* close(fd); */
    if (ofd >= 0) {
	driver_select(prt, (int)(long)ofd, ERL_DRV_USE, 0);  /* close(ofd); */
    }
}

static void outputv(ErlDrvData e, ErlIOVec* ev)
{
    int fd = (int)(long)e;
    int ix = driver_data[fd].port_num;
    int pb = driver_data[fd].packet_bytes;
    int ofd = driver_data[fd].ofd;
    int n;
    int sz;
    char lb[4];
    char* lbp;
    int len = ev->size;

    /* (len > ((unsigned long)-1 >> (4-pb)*8)) */
    if (((pb == 2) && (len > 0xffff)) || (pb == 1 && len > 0xff)) {
	driver_failure_posix(ix, EINVAL);
	return; /* -1; */
    }
    put_int32(len, lb);
    lbp = lb + (4-pb);

    ev->iov[0].iov_base = lbp;
    ev->iov[0].iov_len = pb;
    ev->size += pb;
    if ((sz = driver_sizeq(ix)) > 0) {
	driver_enqv(ix, ev, 0);
	if (sz + ev->size >= (1 << 13))
	    set_busy_port(ix, 1);
    }
    else {
	int vsize = ev->vsize > MAX_VSIZE ? MAX_VSIZE : ev->vsize;

	n = writev(ofd, (const void *) (ev->iov), vsize);
	if (n == ev->size)
	    return; /* 0;*/
	if (n < 0) {
	    if ((errno != EINTR) && (errno != ERRNO_BLOCK)) {
		driver_failure_posix(ix, errno);
		return; /* -1;*/
	    }
	    n = 0;
	}
	driver_enqv(ix, ev, n);  /* n is the skip value */
	driver_select(ix, ofd, ERL_DRV_WRITE|ERL_DRV_USE, 1);
    }
    /* return 0;*/
}


static void output(ErlDrvData e, char* buf, int len)
{
    int fd = (int)(long)e;
    int ix = driver_data[fd].port_num;
    int pb = driver_data[fd].packet_bytes;
    int ofd = driver_data[fd].ofd;
    int n;
    int sz;
    char lb[4];
    char* lbp;
    struct iovec iv[2];

    /* (len > ((unsigned long)-1 >> (4-pb)*8)) */
    if (((pb == 2) && (len > 0xffff)) || (pb == 1 && len > 0xff)) {
	driver_failure_posix(ix, EINVAL);
	return; /* -1; */
    }
    put_int32(len, lb);
    lbp = lb + (4-pb);

    if ((sz = driver_sizeq(ix)) > 0) {
	driver_enq(ix, lbp, pb);
	driver_enq(ix, buf, len);
	if (sz + len + pb >= (1 << 13))
	    set_busy_port(ix, 1);
    }
    else {
	iv[0].iov_base = lbp;
	iv[0].iov_len = pb;  /* should work for pb=0 */
	iv[1].iov_base = buf;
	iv[1].iov_len = len;
	n = writev(ofd, iv, 2);
	if (n == pb+len)
	    return; /* 0; */
	if (n < 0) {
	    if ((errno != EINTR) && (errno != ERRNO_BLOCK)) {
		driver_failure_posix(ix, errno);
		return; /* -1; */
	    }
	    n = 0;
	}
	if (n < pb) {
	    driver_enq(ix, lbp+n, pb-n);
	    driver_enq(ix, buf, len);
	}
	else {
	    n -= pb;
	    driver_enq(ix, buf+n, len-n);
	}
	driver_select(ix, ofd, ERL_DRV_WRITE|ERL_DRV_USE, 1);
    }
    return; /* 0; */
}

static int port_inp_failure(int port_num, int ready_fd, int res)
				/* Result: 0 (eof) or -1 (error) */
{
    int err = errno;

    ASSERT(res <= 0);
    (void) driver_select(port_num, ready_fd, ERL_DRV_READ|ERL_DRV_WRITE, 0); 
    clear_fd_data(ready_fd);
    if (res == 0) {
	if (driver_data[ready_fd].report_exit) {
	    CHLD_STAT_LOCK;

	    if (driver_data[ready_fd].alive) {
		/*
		 * We have eof and want to report exit status, but the process
		 * hasn't exited yet. When it does report_exit_status() will
		 * driver_select() this fd which will make sure that we get
		 * back here with driver_data[ready_fd].alive == 0 and
		 * driver_data[ready_fd].status set.
		 */
		CHLD_STAT_UNLOCK;
		return 0;
	    }
	    else {
		int status = driver_data[ready_fd].status;
		CHLD_STAT_UNLOCK;

		/* We need not be prepared for stopped/continued processes. */
		if (WIFSIGNALED(status))
		    status = 128 + WTERMSIG(status);
		else
		    status = WEXITSTATUS(status);

		driver_report_exit(driver_data[ready_fd].port_num, status);
	    }
       }
       driver_failure_eof(port_num);
    } else {
	driver_failure_posix(port_num, err);
    }
    return 0;
}

/* fd is the drv_data that is returned from the */
/* initial start routine                        */
/* ready_fd is the descriptor that is ready to read */

static void ready_input(ErlDrvData e, ErlDrvEvent ready_fd)
{
    int fd = (int)(long)e;
    int port_num;
    int packet_bytes;
    int res;
    Uint h;

    port_num = driver_data[fd].port_num;
    packet_bytes = driver_data[fd].packet_bytes;

    if (packet_bytes == 0) {
	byte *read_buf = (byte *) erts_alloc(ERTS_ALC_T_SYS_READ_BUF,
					     ERTS_SYS_READ_BUF_SZ);
	res = read(ready_fd, read_buf, ERTS_SYS_READ_BUF_SZ);
	if (res < 0) {
	    if ((errno != EINTR) && (errno != ERRNO_BLOCK))
		port_inp_failure(port_num, ready_fd, res);
	}
	else if (res == 0)
	    port_inp_failure(port_num, ready_fd, res);
	else 
	    driver_output(port_num, (char*) read_buf, res);
	erts_free(ERTS_ALC_T_SYS_READ_BUF, (void *) read_buf);
    }
    else if (fd_data[ready_fd].remain > 0) { /* We try to read the remainder */
	/* space is allocated in buf */
	res = read(ready_fd, fd_data[ready_fd].cpos, 
		   fd_data[ready_fd].remain);
	if (res < 0) {
	    if ((errno != EINTR) && (errno != ERRNO_BLOCK))
		port_inp_failure(port_num, ready_fd, res);
	}
	else if (res == 0) {
	    port_inp_failure(port_num, ready_fd, res);
	}
	else if (res == fd_data[ready_fd].remain) { /* we're done  */
	    driver_output(port_num, fd_data[ready_fd].buf, 
			  fd_data[ready_fd].sz);
	    clear_fd_data(ready_fd);
	}
	else { /*  if (res < fd_data[ready_fd].remain) */
	    fd_data[ready_fd].cpos += res;
	    fd_data[ready_fd].remain -= res;
	}
    }
    else if (fd_data[ready_fd].remain == 0) { /* clean fd */
	byte *read_buf = (byte *) erts_alloc(ERTS_ALC_T_SYS_READ_BUF,
					     ERTS_SYS_READ_BUF_SZ);
	/* We make one read attempt and see what happens */
	res = read(ready_fd, read_buf, ERTS_SYS_READ_BUF_SZ);
	if (res < 0) {  
	    if ((errno != EINTR) && (errno != ERRNO_BLOCK))
		port_inp_failure(port_num, ready_fd, res);
	}
	else if (res == 0) {     	/* eof */
	    port_inp_failure(port_num, ready_fd, res);
	} 
	else if (res < packet_bytes - fd_data[ready_fd].psz) { 
	    memcpy(fd_data[ready_fd].pbuf+fd_data[ready_fd].psz,
		   read_buf, res);
	    fd_data[ready_fd].psz += res;
	}
	else  { /* if (res >= packet_bytes) */
	    unsigned char* cpos = read_buf;
	    int bytes_left = res;

	    while (1) {
		int psz = fd_data[ready_fd].psz;
		char* pbp = fd_data[ready_fd].pbuf + psz;

		while(bytes_left && (psz < packet_bytes)) {
		    *pbp++ = *cpos++;
		    bytes_left--;
		    psz++;
		}
		
		if (psz < packet_bytes) {
		    fd_data[ready_fd].psz = psz;
		    break;
		}
		fd_data[ready_fd].psz = 0;

		switch (packet_bytes) {
		case 1: h = get_int8(fd_data[ready_fd].pbuf);  break;
		case 2: h = get_int16(fd_data[ready_fd].pbuf); break;
		case 4: h = get_int32(fd_data[ready_fd].pbuf); break;
		default: ASSERT(0); return; /* -1; */
		}

		if (h <= (bytes_left)) {
		    driver_output(port_num, (char*) cpos, h);
		    cpos += h;
		    bytes_left -= h;
		    continue;
		}
		else {		/* The last message we got was split */
		        char *buf = erts_alloc_fnf(ERTS_ALC_T_FD_ENTRY_BUF, h);
		    if (!buf) {
			errno = ENOMEM;
			port_inp_failure(port_num, ready_fd, -1);
		    }
		    else {
			erts_smp_atomic_add(&sys_misc_mem_sz, h);
			sys_memcpy(buf, cpos, bytes_left);
			fd_data[ready_fd].buf = buf;
			fd_data[ready_fd].sz = h;
			fd_data[ready_fd].remain = h - bytes_left;
			fd_data[ready_fd].cpos = buf + bytes_left;
		    }
		    break;
		}
	    }
	}
	erts_free(ERTS_ALC_T_SYS_READ_BUF, (void *) read_buf);
    }
}


/* fd is the drv_data that is returned from the */
/* initial start routine                        */
/* ready_fd is the descriptor that is ready to read */

static void ready_output(ErlDrvData e, ErlDrvEvent ready_fd)
{
    int fd = (int)(long)e;
    int ix = driver_data[fd].port_num;
    int n;
    struct iovec* iv;
    int vsize;
    

    if ((iv = (struct iovec*) driver_peekq(ix, &vsize)) == NULL) {
	driver_select(ix, ready_fd, ERL_DRV_WRITE, 0);
	return; /* 0; */
    }
    vsize = vsize > MAX_VSIZE ? MAX_VSIZE : vsize;
    if ((n = writev(ready_fd, iv, vsize)) > 0) {
	if (driver_deq(ix, n) == 0)
	    set_busy_port(ix, 0);
    }
    else if (n < 0) {
	if (errno == ERRNO_BLOCK || errno == EINTR)
	    return; /* 0; */
	else {
	    int res = errno;
	    driver_select(ix, ready_fd, ERL_DRV_WRITE, 0);
	    driver_failure_posix(ix, res);
	    return; /* -1; */
	}
    }
    return; /* 0; */
}

static void stop_select(ErlDrvEvent fd, void* _)
{
    close((int)fd);
}

/*
** Async opertation support
*/
#if defined(USE_THREADS) && !defined(ERTS_SMP)
static void
sys_async_ready_failed(int fd, int r, int err)
{
    char buf[120];
    sprintf(buf, "sys_async_ready(): Fatal error: fd=%d, r=%d, errno=%d\n",
	     fd, r, err);
    erts_silence_warn_unused_result(write(2, buf, strlen(buf)));
    abort();
}

/* called from threads !! */
void sys_async_ready(int fd)
{
    int r;
    while (1) {
	r = write(fd, "0", 1);  /* signal main thread fd MUST be async_fd[1] */
	if (r == 1) {
	    DEBUGF(("sys_async_ready(): r = 1\r\n"));
	    break;
	}
	if (r < 0 && errno == EINTR) {
	    DEBUGF(("sys_async_ready(): r = %d\r\n", r));
	    continue;
	}
	sys_async_ready_failed(fd, r, errno);
    }
}

static int async_drv_init(void)
{
    async_fd[0] = -1;
    async_fd[1] = -1;
    return 0;
}

static ErlDrvData async_drv_start(ErlDrvPort port_num,
				  char* name, SysDriverOpts* opts)
{
    if (async_fd[0] != -1)
	return ERL_DRV_ERROR_GENERAL;
    if (pipe(async_fd) < 0)
	return ERL_DRV_ERROR_GENERAL;

    DEBUGF(("async_drv_start: %d\r\n", port_num));

    SET_NONBLOCKING(async_fd[0]);
    driver_select(port_num, async_fd[0], ERL_DRV_READ, 1);

    if (init_async(async_fd[1]) < 0)
	return ERL_DRV_ERROR_GENERAL;
    return (ErlDrvData)port_num;
}

static void async_drv_stop(ErlDrvData e)
{
    int port_num = (int)(long)e;

    DEBUGF(("async_drv_stop: %d\r\n", port_num));

    exit_async();

    driver_select(port_num, async_fd[0], ERL_DRV_READ, 0);

    close(async_fd[0]);
    close(async_fd[1]);
    async_fd[0] = async_fd[1] = -1;
}


static void async_drv_input(ErlDrvData e, ErlDrvEvent fd)
{
    char *buf[32];
    DEBUGF(("async_drv_input\r\n"));
    while (read((int) fd, (void *) buf, 32) > 0); /* fd MUST be async_fd[0] */
    check_async_ready();  /* invoke all async_ready */
}
#endif

void erts_do_break_handling(void)
{
    struct termios temp_mode;
    int saved = 0;
    
    /*
     * Most functions that do_break() calls are intentionally not thread safe;
     * therefore, make sure that all threads but this one are blocked before
     * proceeding!
     */
    erts_smp_block_system(0);
    /*
     * NOTE: since we allow gc we are not allowed to lock
     *       (any) process main locks while blocking system...
     */

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

    erts_smp_release_system();
}

/* Fills in the systems representation of the jam/beam process identifier.
** The Pid is put in STRING representation in the supplied buffer,
** no interpretatione of this should be done by the rest of the
** emulator. The buffer should be at least 21 bytes long.
*/
void sys_get_pid(char *buffer){
    pid_t p = getpid();
    /* Assume the pid is scalar and can rest in an unsigned long... */
    sprintf(buffer,"%lu",(unsigned long) p);
}

int
erts_sys_putenv(char *buffer, int sep_ix)
{
    int res;
    char *env;
#ifdef HAVE_COPYING_PUTENV
    env = buffer;
#else
    Uint sz = strlen(buffer)+1;
    env = erts_alloc(ERTS_ALC_T_PUTENV_STR, sz);
    erts_smp_atomic_add(&sys_misc_mem_sz, sz);
    strcpy(env,buffer);
#endif
    erts_smp_rwmtx_rwlock(&environ_rwmtx);
    res = putenv(env);
    erts_smp_rwmtx_rwunlock(&environ_rwmtx);
    return res;
}

int
erts_sys_getenv(char *key, char *value, size_t *size)
{
    char *orig_value;
    int res;
    erts_smp_rwmtx_rlock(&environ_rwmtx);
    orig_value = getenv(key);
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
    erts_smp_rwmtx_runlock(&environ_rwmtx);
    return res;
}

void
sys_init_io(void)
{
    fd_data = (struct fd_data *)
	erts_alloc(ERTS_ALC_T_FD_TAB, max_files * sizeof(struct fd_data));
    erts_smp_atomic_add(&sys_misc_mem_sz,
			max_files * sizeof(struct fd_data));

#ifdef USE_THREADS
#ifdef ERTS_SMP
    if (init_async(-1) < 0)
	erl_exit(1, "Failed to initialize async-threads\n");
#else
    {
	/* This is speical stuff, starting a driver from the 
	 * system routines, but is a nice way of handling stuff
	 * the erlang way
	 */
	SysDriverOpts dopts;
	int ret;

	sys_memset((void*)&dopts, 0, sizeof(SysDriverOpts));
	add_driver_entry(&async_driver_entry);
	ret = erts_open_driver(NULL, NIL, "async", &dopts, NULL);
	DEBUGF(("open_driver = %d\n", ret));
	if (ret < 0)
	    erl_exit(1, "Failed to open async driver\n");
	erts_port[ret].status |= ERTS_PORT_SFLG_IMMORTAL;
    }
#endif
#endif

}

#if (0) /* unused? */
static int write_fill(fd, buf, len)
int fd, len;
char *buf;
{
    int i, done = 0;
    
    do {
	if ((i = write(fd, buf+done, len-done)) < 0) {
	    if (errno != EINTR)
		return (i);
	    i = 0;
	}
	done += i;
    } while (done < len);
    return (len);
}
#endif

extern const char pre_loaded_code[];
extern Preload pre_loaded[];

void erts_sys_alloc_init(void)
{
}

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

/* Read a key from console (?) */

int sys_get_key(fd)
int fd;
{
    int c;
    unsigned char rbuf[64];

    fflush(stdout);		/* Flush query ??? */

    if ((c = read(fd,rbuf,64)) <= 0) {
      return c; 
    }

    return rbuf[0]; 
}


#ifdef DEBUG

extern int erts_initialized;
void
erl_assert_error(char* expr, char* file, int line)
{   
    fflush(stdout);
    fprintf(stderr, "Assertion failed: %s in %s, line %d\n",
	    expr, file, line);
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

static ERTS_INLINE void
report_exit_status(ErtsSysReportExit *rep, int status)
{
    Port *pp;
#ifdef ERTS_SMP
    CHLD_STAT_UNLOCK;
#endif
    pp = erts_id2port_sflgs(rep->port,
			    NULL,
			    0,
			    ERTS_PORT_SFLGS_INVALID_DRIVER_LOOKUP);
#ifdef ERTS_SMP
    CHLD_STAT_LOCK;
#endif
    if (pp) {
	if (rep->ifd >= 0) {
	    driver_data[rep->ifd].alive = 0;
	    driver_data[rep->ifd].status = status;
	    (void) driver_select((ErlDrvPort) internal_port_index(pp->id),
				 rep->ifd,
				 (ERL_DRV_READ|ERL_DRV_USE),
				 1);
	}
	if (rep->ofd >= 0) {
	    driver_data[rep->ofd].alive = 0;
	    driver_data[rep->ofd].status = status;
	    (void) driver_select((ErlDrvPort) internal_port_index(pp->id),
				 rep->ofd,
				 (ERL_DRV_WRITE|ERL_DRV_USE),
				 1);
	}
	erts_port_release(pp);
    }
    erts_free(ERTS_ALC_T_PRT_REP_EXIT, rep);
}

#if !CHLDWTHR  /* ---------------------------------------------------------- */

#define ERTS_REPORT_EXIT_STATUS report_exit_status

static int check_children(void)
{
    int res = 0;
    int pid;
    int status;

#ifndef ERTS_SMP
    if (children_died)
#endif
    {
	sys_sigblock(SIGCHLD);
	CHLD_STAT_LOCK;
	while ((pid = waitpid(-1, &status, WNOHANG)) > 0)
	    note_child_death(pid, status);
#ifndef ERTS_SMP
	children_died = 0;
#endif
	CHLD_STAT_UNLOCK;
	sys_sigrelease(SIGCHLD);
	res = 1;
    }
    return res;
}

#ifdef ERTS_SMP

void
erts_check_children(void)
{
    (void) check_children();
}

#endif

#elif CHLDWTHR && defined(ERTS_SMP) /* ------------------------------------- */

#define ERTS_REPORT_EXIT_STATUS report_exit_status

#define check_children() (0)


#else /* CHLDWTHR && !defined(ERTS_SMP) ------------------------------------ */

#define ERTS_REPORT_EXIT_STATUS initiate_report_exit_status

static ERTS_INLINE void
initiate_report_exit_status(ErtsSysReportExit *rep, int status)
{
    rep->next = report_exit_transit_list;
    rep->status = status;
    report_exit_transit_list = rep;
    /*
     * We need the scheduler thread to call check_children().
     * If the scheduler thread is sleeping in a poll with a
     * timeout, we need to wake the scheduler thread. We use the
     * functionality of the async driver to do this, instead of
     * implementing yet another driver doing the same thing. A
     * little bit ugly, but it works...
     */
    sys_async_ready(async_fd[1]);
}

static int check_children(void)
{
    int res;
    ErtsSysReportExit *rep;
    CHLD_STAT_LOCK;
    rep = report_exit_transit_list;
    res = rep != NULL;
    while (rep) {
	ErtsSysReportExit *curr_rep = rep;
	rep = rep->next;
	report_exit_status(curr_rep, curr_rep->status);
    }
    report_exit_transit_list = NULL;
    CHLD_STAT_UNLOCK;
    return res;
}

#endif /* ------------------------------------------------------------------ */

static void note_child_death(int pid, int status)
{
    ErtsSysReportExit **repp = &report_exit_list;
    ErtsSysReportExit *rep = report_exit_list;

    while (rep) {
	if (pid == rep->pid) {
	    *repp = rep->next;
	    ERTS_REPORT_EXIT_STATUS(rep, status);
	    break;
	}
	repp = &rep->next;
	rep = rep->next;
    }
}

#if CHLDWTHR

static void *
child_waiter(void *unused)
{
  int pid;
  int status;

#ifdef ERTS_ENABLE_LOCK_CHECK
  erts_lc_set_thread_name("child waiter");
#endif

  while(1) {
#ifdef DEBUG
      int waitpid_errno;
#endif
      pid = waitpid(-1, &status, 0);
#ifdef DEBUG
      waitpid_errno = errno;
#endif
      CHLD_STAT_LOCK;
      if (pid < 0) {
	  ASSERT(waitpid_errno == ECHILD);
      }
      else {
	  children_alive--;
	  ASSERT(children_alive >= 0);
	  note_child_death(pid, status);
      }
      while (!children_alive)
	  CHLD_STAT_WAIT; /* Wait for children to wait on... :) */
      CHLD_STAT_UNLOCK;
  }

  return NULL;
}

#endif

/*
 * Called from schedule() when it runs out of runnable processes,
 * or when Erlang code has performed INPUT_REDUCTIONS reduction
 * steps. runnable == 0 iff there are no runnable Erlang processes.
 */
void
erl_sys_schedule(int runnable)
{
#ifdef ERTS_SMP
    ERTS_CHK_IO(!runnable);
    ERTS_SMP_LC_ASSERT(!ERTS_LC_IS_BLOCKING);
#else
    ERTS_CHK_IO_INTR(0);
    if (runnable) {
	ERTS_CHK_IO(0);		/* Poll for I/O */
	check_async_ready();	/* Check async completions */
    } else {
	int wait_for_io = !check_async_ready();
	if (wait_for_io)
	    wait_for_io = !check_children();
	ERTS_CHK_IO(wait_for_io);
    }
    (void) check_children();
#endif
}


#ifdef ERTS_SMP

static erts_smp_tid_t sig_dispatcher_tid;

static void
smp_sig_notify(char c)
{
    int res;
    do {
	/* write() is async-signal safe (according to posix) */
	res = write(sig_notify_fds[1], &c, 1);
    } while (res < 0 && errno == EINTR);
    if (res != 1) {
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
    int initialized = 0;
#if !CHLDWTHR
    int notify_check_children = 0;
#endif
#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_lc_set_thread_name("signal_dispatcher");
#endif
    while (1) {
	char buf[32];
	int res, i;
	/* Block on read() waiting for a signal notification to arrive... */
	res = read(sig_notify_fds[0], (void *) &buf[0], 32);
	if (res < 0) {
	    if (errno == EINTR)
		continue;
	    erl_exit(ERTS_ABORT_EXIT,
		     "signal-dispatcher thread got unexpected error: %s (%d)\n",
		     erl_errno_id(errno),
		     errno);
	}
	for (i = 0; i < res; i++) {
	    /*
	     * NOTE 1: The signal dispatcher thread should not do work
	     *         that takes a substantial amount of time (except
	     *         perhaps in test and debug builds). It needs to
	     *         be responsive, i.e, it should only dispatch work
	     *         to other threads.
	     *
	     * NOTE 2: The signal dispatcher thread is not a blockable
	     *         thread (i.e., it hasn't called 
	     *         erts_register_blockable_thread()). This is
	     *         intentional. We want to be able to interrupt
	     *         writing of a crash dump by hitting C-c twice.
	     *         Since it isn't a blockable thread it is important
	     *         that it doesn't change the state of any data that
	     *         a blocking thread expects to have exclusive access
	     *         to (unless the signal dispatcher itself explicitly
	     *         is blocking all blockable threads).
	     */
	    switch (buf[i]) {
	    case 0: /* Emulator initialized */
		initialized = 1;
#if !CHLDWTHR
		if (!notify_check_children)
#endif
		    break;
#if !CHLDWTHR
	    case 'C': /* SIGCHLD */
		if (initialized)
		    erts_smp_notify_check_children_needed();
		else
		    notify_check_children = 1;
		break;
#endif
	    case 'I': /* SIGINT */
		break_requested();
		break;
	    case 'Q': /* SIGQUIT */
		quit_requested();
		break;
	    case '1': /* SIGUSR1 */
		sigusr1_exit();
		break;
#ifdef QUANTIFY
	    case '2': /* SIGUSR2 */
		quantify_save_data(); /* Might take a substantial amount of
					 time, but this is a test/debug
					 build */
		break;
#endif
	    default:
		erl_exit(ERTS_ABORT_EXIT,
			 "signal-dispatcher thread received unknown "
			 "signal notification: '%c'\n",
			 buf[i]);
	    }
	}
	ERTS_SMP_LC_ASSERT(!ERTS_LC_IS_BLOCKING);
    }
    return NULL;
}

static void
init_smp_sig_notify(void)
{
    erts_smp_thr_opts_t thr_opts = ERTS_SMP_THR_OPTS_DEFAULT_INITER;
    thr_opts.detached = 1;

    if (pipe(sig_notify_fds) < 0) {
	erl_exit(ERTS_ABORT_EXIT,
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

void
erts_sys_main_thread(void)
{
    erts_thread_disable_fpe();
    /* Become signal receiver thread... */
#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_lc_set_thread_name("signal_receiver");
#endif

    smp_sig_notify(0); /* Notify initialized */
    while (1) {
	/* Wait for a signal to arrive... */
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
	int res = erts_sys_getenv("ERL_NO_KERNEL_POLL", no_kp, &no_kp_sz);
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
#endif

    /* Handled arguments have been marked with NULL. Slide arguments
       not handled towards the beginning of argv. */
    for (i = 0, j = 0; i < *argc; i++) {
	if (argv[i])
	    argv[j++] = argv[i];
    }
    *argc = j;
}

#ifdef ERTS_TIMER_THREAD

/*
 * Interruptible-wait facility: low-level synchronisation state
 * and methods that are implementation dependent.
 *
 * Constraint: Every implementation must define 'struct erts_iwait'
 * with a field 'erts_smp_atomic_t state;'.
 */

/* values for struct erts_iwait's state field */
#define IWAIT_WAITING	0
#define IWAIT_AWAKE	1
#define IWAIT_INTERRUPT	2

#if 0	/* XXX: needs feature test in erts/configure.in */

/*
 * This is an implementation of the interruptible wait facility on
 * top of Linux-specific futexes.
 */
#include <asm/unistd.h>
#define FUTEX_WAIT		0
#define FUTEX_WAKE		1
static int sys_futex(void *futex, int op, int val, const struct timespec *timeout)
{
    return syscall(__NR_futex, futex, op, val, timeout);
}

struct erts_iwait {
    erts_smp_atomic_t state; /* &state.counter is our futex */
};

static void iwait_lowlevel_init(struct erts_iwait *iwait) { /* empty */ }

static void iwait_lowlevel_wait(struct erts_iwait *iwait, struct timeval *delay)
{
    struct timespec timeout;
    int res;

    timeout.tv_sec = delay->tv_sec;
    timeout.tv_nsec = delay->tv_usec * 1000;
    res = sys_futex((void*)&iwait->state.counter, FUTEX_WAIT, IWAIT_WAITING, &timeout);
    if (res < 0 && errno != ETIMEDOUT && errno != EWOULDBLOCK && errno != EINTR)
	perror("FUTEX_WAIT");
}

static void iwait_lowlevel_interrupt(struct erts_iwait *iwait)
{
    int res = sys_futex((void*)&iwait->state.counter, FUTEX_WAKE, 1, NULL);
    if (res < 0)
	perror("FUTEX_WAKE");
}

#else	/* using poll() or select() */

/*
 * This is an implementation of the interruptible wait facility on
 * top of pipe(), poll() or select(), read(), and write().
 */
struct erts_iwait {
    erts_smp_atomic_t state;
    int read_fd;	/* wait polls and reads this fd */
    int write_fd;	/* interrupt writes this fd */
};

static void iwait_lowlevel_init(struct erts_iwait *iwait)
{
    int fds[2];

    if (pipe(fds) < 0) {
	perror("pipe()");
	exit(1);
    }
    iwait->read_fd = fds[0];
    iwait->write_fd = fds[1];
}

#if defined(ERTS_USE_POLL)

#include <sys/poll.h>
#define PERROR_POLL "poll()"

static int iwait_lowlevel_poll(int read_fd, struct timeval *delay)
{
    struct pollfd pollfd;
    int timeout;

    pollfd.fd = read_fd;
    pollfd.events = POLLIN;
    pollfd.revents = 0;
    timeout = delay->tv_sec * 1000 + delay->tv_usec / 1000;
    return poll(&pollfd, 1, timeout);
}

#else	/* !ERTS_USE_POLL */

#include <sys/select.h>
#define PERROR_POLL "select()"

static int iwait_lowlevel_poll(int read_fd, struct timeval *delay)
{
    fd_set readfds;

    FD_ZERO(&readfds);
    FD_SET(read_fd, &readfds);
    return select(read_fd + 1, &readfds, NULL, NULL, delay);
}

#endif	/* !ERTS_USE_POLL */

static void iwait_lowlevel_wait(struct erts_iwait *iwait, struct timeval *delay)
{
    int res;
    char buf[64];

    res = iwait_lowlevel_poll(iwait->read_fd, delay);
    if (res > 0)
	(void)read(iwait->read_fd, buf, sizeof buf);
    else if (res < 0 && errno != EINTR)
	perror(PERROR_POLL);
}

static void iwait_lowlevel_interrupt(struct erts_iwait *iwait)
{
    int res = write(iwait->write_fd, "!", 1);
    if (res < 0)
	perror("write()");
}

#endif	/* using poll() or select() */

#if 0	/* not using poll() or select() */
/*
 * This is an implementation of the interruptible wait facility on
 * top of pthread_cond_timedwait(). This has two problems:
 * 1. pthread_cond_timedwait() requires an absolute time point,
 *    so the relative delay must be converted to absolute time.
 *    Worse, this breaks if the machine's time is adjusted while
 *    we're preparing to wait.
 * 2. Each cond operation requires additional mutex lock/unlock operations.
 *
 * Problem 2 is probably not too bad on Linux (they'll just become
 * relatively cheap futex operations), but problem 1 is the real killer.
 * Only use this implementation if no better alternatives are available!
 */
struct erts_iwait {
    erts_smp_atomic_t state;
    pthread_cond_t cond;
    pthread_mutex_t mutex;
};

static void iwait_lowlevel_init(struct erts_iwait *iwait)
{
    iwait->cond = (pthread_cond_t) PTHREAD_COND_INITIALIZER;
    iwait->mutex = (pthread_mutex_t) PTHREAD_MUTEX_INITIALIZER;
}

static void iwait_lowlevel_wait(struct erts_iwait *iwait, struct timeval *delay)
{
    struct timeval tmp;
    struct timespec timeout;

    /* Due to pthread_cond_timedwait()'s use of absolute
       time, this must be the real gettimeofday(), _not_
       the "smoothed" one beam/erl_time_sup.c implements. */
    gettimeofday(&tmp, NULL);

    tmp.tv_sec += delay->tv_sec;
    tmp.tv_usec += delay->tv_usec;
    if (tmp.tv_usec >= 1000*1000) {
	tmp.tv_usec -= 1000*1000;
	tmp.tv_sec += 1;
    }
    timeout.tv_sec = tmp.tv_sec;
    timeout.tv_nsec = tmp.tv_usec * 1000;
    pthread_mutex_lock(&iwait->mutex);
    pthread_cond_timedwait(&iwait->cond, &iwait->mutex, &timeout);
    pthread_mutex_unlock(&iwait->mutex);
}

static void iwait_lowlevel_interrupt(struct erts_iwait *iwait)
{
    pthread_mutex_lock(&iwait->mutex);
    pthread_cond_signal(&iwait->cond);
    pthread_mutex_unlock(&iwait->mutex);
}

#endif /* not using POLL */

/*
 * Interruptible-wait facility. This is just a wrapper around the
 * low-level synchronisation code, where we maintain our logical
 * state in order to suppress some state transitions.
 */

struct erts_iwait *erts_iwait_init(void)
{
    struct erts_iwait *iwait = malloc(sizeof *iwait);
    if (!iwait) {
	perror("malloc");
	exit(1);
    }
    iwait_lowlevel_init(iwait);
    erts_smp_atomic_init(&iwait->state, IWAIT_AWAKE);
    return iwait;
}

void erts_iwait_wait(struct erts_iwait *iwait, struct timeval *delay)
{
    if (erts_smp_atomic_xchg(&iwait->state, IWAIT_WAITING) != IWAIT_INTERRUPT)
	iwait_lowlevel_wait(iwait, delay);
    erts_smp_atomic_set(&iwait->state, IWAIT_AWAKE);
}

void erts_iwait_interrupt(struct erts_iwait *iwait)
{
    if (erts_smp_atomic_xchg(&iwait->state, IWAIT_INTERRUPT) == IWAIT_WAITING)
	iwait_lowlevel_interrupt(iwait);
}

#endif /* ERTS_TIMER_THREAD */
