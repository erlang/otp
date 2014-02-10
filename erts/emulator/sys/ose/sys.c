/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1996-2013. All Rights Reserved.
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
#include "sys/time.h"
#include "time.h"
#include "sys/uio.h"
#include "termios.h"
#include "ctype.h"
#include "termios.h"

#ifdef HAVE_FCNTL_H
#include "fcntl.h"
#endif

#ifdef HAVE_SYS_IOCTL_H
#include "sys/ioctl.h"
#endif

#define ERTS_WANT_BREAK_HANDLING
#define WANT_NONBLOCKING
#include "sys.h"
#include "erl_thr_progress.h"

#ifdef USE_THREADS
#include "erl_threads.h"
#endif

#include "erl_mseg.h"

#include "unistd.h"
#include "efs.h"
#include "erl_printf.h"

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
#include "erl_cpu_topology.h"

/* The priority for reader/writer processes */
#define FD_PROC_PRI 20

typedef struct ErtsSysReportExit_ ErtsSysReportExit;
struct ErtsSysReportExit_ {
    ErtsSysReportExit *next;
    Eterm port;
    int pid;
    int ifd;
    int ofd;
    ErlDrvEvent in_sig_descr;
    ErlDrvEvent out_sig_descr;
};

/* This data is shared by these drivers - initialized by spawn_init() */
static struct driver_data {
    ErlDrvPort port_num;
    int ofd, packet_bytes;
    ErtsSysReportExit *report_exit;
    int pid;
    int alive;
    int status;
    ErlDrvEvent in_sig_descr;
    ErlDrvEvent out_sig_descr;
    PROCESS in_proc;
    PROCESS out_proc;
    ErlDrvPDL pdl;
} *driver_data;			/* indexed by fd */

static ErtsSysReportExit *report_exit_list;

extern int  driver_interrupt(int, int);
extern void do_break(void);

extern void erl_sys_args(int*, char**);

/* The following two defs should probably be moved somewhere else */

extern void erts_sys_init_float(void);

extern void erl_crash_dump(char* file, int line, char* fmt, ...);

#define DIR_SEPARATOR_CHAR    '/'

#if defined(DEBUG)
#define ERL_BUILD_TYPE_MARKER ".debug"
#else /* opt */
#define ERL_BUILD_TYPE_MARKER
#endif

#define CHILD_SETUP_PROG_NAME	"child_setup" ERL_BUILD_TYPE_MARKER

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

static erts_smp_atomic_t sys_misc_mem_sz;

#if defined(ERTS_SMP)
erts_mtx_t chld_stat_mtx;
#endif

#if defined(ERTS_SMP) /* ------------------------------------------------- */
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
/* set early so the break handler has access to initial mode */
static struct termios initial_tty_mode;
static int replace_intr = 0;
/* assume yes initially, ttsl_init will clear it */
int using_oldshell = 1;

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


void
erts_sys_schedule_interrupt(int set)
{
    ERTS_CHK_IO_INTR(set);
}

#ifdef ERTS_SMP
void
erts_sys_schedule_interrupt_timed(int set, erts_short_time_t msec)
{
    ERTS_CHK_IO_INTR_TMD(set, msec);
}
#endif

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

#ifdef USE_THREADS

typedef struct {
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

    tcdp->sched_bind_data = erts_sched_bind_atthrcreate_prepare();

    return (void *) tcdp;
}


/* thr_create_cleanup() is called in parent thread after thread creation. */
static void
thr_create_cleanup(void *vtcdp)
{
    erts_thr_create_data_t *tcdp = (erts_thr_create_data_t *) vtcdp;

    erts_sched_bind_atthrcreate_parent(tcdp->sched_bind_data);

    erts_free(ERTS_ALC_T_TMP, tcdp);
}

static void
thr_create_prepare_child(void *vtcdp)
{
    erts_thr_create_data_t *tcdp = (erts_thr_create_data_t *) vtcdp;

#ifdef ERTS_ENABLE_LOCK_COUNT
    erts_lcnt_thread_setup();
#endif

    erts_sched_bind_atthrcreate_child(tcdp->sched_bind_data);
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

    erts_thr_init(&eid);

    report_exit_list = NULL;

#ifdef ERTS_ENABLE_LOCK_COUNT
    erts_lcnt_init();
#endif

#if defined(ERTS_SMP)
    erts_mtx_init(&chld_stat_mtx, "child_status");
#endif
    }
#ifdef ERTS_SMP
    erts_smp_atomic32_init_nob(&erts_break_requested, 0);
    erts_smp_atomic32_init_nob(&have_prepared_crash_dump, 0);
#else
    erts_break_requested = 0;
    have_prepared_crash_dump = 0;
#endif
#if !defined(ERTS_SMP)
    children_died = 0;
#endif
#endif /* USE_THREADS */
    erts_smp_atomic_init_nob(&sys_misc_mem_sz, 0);
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

static ERTS_INLINE int
prepare_crash_dump(int secs)
{
#define NUFBUF (3)
    int i, max;
    char env[21]; /* enough to hold any 64-bit integer */
    size_t envsz;
    /*DeclareTmpHeapNoproc(heap,NUFBUF);*/
    /*Eterm *hp = heap;*/
    /*Eterm list = NIL;*/
    int has_heart = 0;

    UseTmpHeapNoproc(NUFBUF);

    if (ERTS_PREPARED_CRASH_DUMP)
	return 0; /* We have already been called */


    /* Positive secs means an alarm must be set
     * 0 or negative means no alarm
     *
     * Set alarm before we try to write to a port
     * we don't want to hang on a port write with
     * no alarm.
     *
     */

#if 0 /*ose TBD!!!*/
    if (secs >= 0) {
	alarm((unsigned int)secs);
    }
#endif

    /* Make sure we unregister at epmd (unknown fd) and get at least
       one free filedescriptor (for erl_crash.dump) */

    max = max_files;
    if (max < 1024)
	max = 1024;
    for (i = 3; i < max; i++) {
	close(i);
    }

    envsz = sizeof(env);
    i = erts_sys_getenv__("ERL_CRASH_DUMP_NICE", env, &envsz);
    if (i >= 0) {
	int nice_val;
	nice_val = i != 0 ? 0 : atoi(env);
	if (nice_val > 39) {
	    nice_val = 39;
	}
	set_pri(nice_val);
    }

    UnUseTmpHeapNoproc(NUFBUF);
#undef NUFBUF
    return has_heart;
}

int erts_sys_prepare_crash_dump(int secs)
{
    return prepare_crash_dump(secs);
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
  ERTS_CHK_IO_AS_INTR(); /* Make sure we don't sleep in poll */
}

/* Disable break */
void erts_set_ignore_break(void) {

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

}

int sys_max_files(void)
{
   return(max_files);
}


/************************** OS info *******************************/

/* Used by erlang:info/1. */
/* (This code was formerly in drv.XXX/XXX_os_drv.c) */

char os_type[] = "ose";

void
os_flavor(char* namebuf,	/* Where to return the name. */
	  unsigned size)	/* Size of name buffer. */
{
#if 0
    struct utsname uts;		/* Information about the system. */
    char* s;

    (void) uname(&uts);
    for (s = uts.sysname; *s; s++) {
	if (isupper((int) *s)) {
	    *s = tolower((int) *s);
	}
    }
    strcpy(namebuf, uts.sysname);
#else
    strncpy(namebuf, "release", size);
#endif
}

void
os_version(pMajor, pMinor, pBuild)
int* pMajor;			/* Pointer to major version. */
int* pMinor;			/* Pointer to minor version. */
int* pBuild;			/* Pointer to build number. */
{
    *pMajor = 5;
    *pMinor = 7;
    *pBuild = 0;
}

void init_getenv_state(GETENV_STATE *state)
{
   erts_smp_rwmtx_rlock(&environ_rwmtx);
   *state = NULL;
}

char **environ; /*ose - needs replacement*/

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

typedef struct SysDriverAsyncSignal_ {
    SIGSELECT sig_no;
    int type;
    byte *buff;
    ssize_t res;
    int errno_copy;
} SysDriverAsyncSignal;

typedef struct SysDriverConfSignal_ {
    SIGSELECT sig_no;
    int fd;
    PROCESS parent;
} SysDriverConfSignal;

union SIGNAL {
    SIGSELECT sig_no;
    SysDriverAsyncSignal sys_async;
    SysDriverConfSignal conf_async;
};

/* II. The spawn/fd drivers */

/*
 * Decreasing the size of it below 16384 is not allowed.
 */
#define ERTS_SYS_READ_BUF_SZ (64*1024)

/* Driver interfaces */
static ErlDrvData spawn_start(ErlDrvPort, char*, SysDriverOpts*);
static ErlDrvData fd_start(ErlDrvPort, char*, SysDriverOpts*);
static ErlDrvSSizeT fd_control(ErlDrvData, unsigned int, char *, ErlDrvSizeT,
			       char **, ErlDrvSizeT);
static int spawn_init(void);
static void fd_stop(ErlDrvData);
static void erl_stop(ErlDrvData);
static void ready_input(ErlDrvData, ErlDrvEvent);
static void ready_output(ErlDrvData, ErlDrvEvent);
static void output(ErlDrvData, char*, ErlDrvSizeT);
static void outputv(ErlDrvData, ErlIOVec*);
static void stop_select(ErlDrvEvent, void*);
static ErlDrvOseEventId resolve_signal(union SIGNAL* sig) {
    return sig->sig_no == ERTS_SIGNAL_FD_DRV_ASYNC ? sig->sys_async.type : -1;
}

OS_PROCESS(fd_writer_process);
OS_PROCESS(fd_reader_process);

struct erl_drv_entry spawn_driver_entry = {
    spawn_init,
    spawn_start,
    erl_stop,
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

static int set_driver_data(ErlDrvPort port_num,
			   int ifd,
			   int ofd,
			   int packet_bytes,
			   int read_write,
			   int exit_status,
			   int pid)
{
    Port *prt;
    ErtsSysReportExit *report_exit;
    union SIGNAL *sig;

    /*erts_fprintf(stderr, " %s / pid %x / ofd %d / ifd %d\n",
      __FUNCTION__, current_process(), ofd, ifd);*/


    if (!exit_status)
	report_exit = NULL;
    else {
	report_exit = erts_alloc(ERTS_ALC_T_PRT_REP_EXIT,
				 sizeof(ErtsSysReportExit));
	report_exit->next = report_exit_list;
	report_exit->port = erts_drvport2id(port_num);
	report_exit->pid = pid;
	report_exit->ifd = read_write & DO_READ ? ifd : -1;
	report_exit->ofd = read_write & DO_WRITE ? ofd : -1;

	if (read_write & DO_READ)
	  report_exit->in_sig_descr =
	    erl_drv_ose_event_alloc(ERTS_SIGNAL_FD_DRV_ASYNC, ifd,
				    resolve_signal);
	if (read_write & DO_WRITE)
	  report_exit->out_sig_descr =
	    erl_drv_ose_event_alloc(ERTS_SIGNAL_FD_DRV_ASYNC, ofd,
				    resolve_signal);

	report_exit_list = report_exit;
    }

    prt = erts_drvport2port(port_num);
    if (prt != ERTS_INVALID_ERL_DRV_PORT)
	prt->os_pid = pid;

    if (read_write & DO_READ) {
	driver_data[ifd].packet_bytes = packet_bytes;
	driver_data[ifd].port_num = port_num;
	driver_data[ifd].report_exit = report_exit;
	driver_data[ifd].pid = pid;
	driver_data[ifd].alive = 1;
	driver_data[ifd].status = 0;
	driver_data[ifd].in_sig_descr =
	  erl_drv_ose_event_alloc(ERTS_SIGNAL_FD_DRV_ASYNC,ifd,
				  resolve_signal);

	driver_data[ifd].in_proc = create_process(OS_PRI_PROC,"beam_fd_reader",
                                                  fd_reader_process, 0x800,
                                                  FD_PROC_PRI, 0, 0,
						  NULL, 0, 0);
	efs_clone(driver_data[ifd].in_proc);
	sig = alloc(sizeof(SysDriverConfSignal), ERTS_SIGNAL_FD_DRV_CONFIG);
	sig->conf_async.fd = ifd;
	sig->conf_async.parent = current_process();
	send(&sig, driver_data[ifd].in_proc);
	start(driver_data[ifd].in_proc);

	if (read_write & DO_WRITE) {
	    driver_data[ifd].ofd = ofd;
	    driver_data[ifd].out_sig_descr =
	      erl_drv_ose_event_alloc(ERTS_SIGNAL_FD_DRV_ASYNC,ofd,
				      resolve_signal);
	    driver_data[ifd].pdl = driver_pdl_create(port_num);
	    driver_data[ifd].out_proc =
	      create_process(OS_PRI_PROC,"beam_fd_writer",
			     fd_writer_process, 0x800,
			     FD_PROC_PRI, 0, 0, NULL, 0, 0);
	    sig = alloc(sizeof(SysDriverConfSignal), ERTS_SIGNAL_FD_DRV_CONFIG);
	    sig->conf_async.fd = ofd;
	    sig->conf_async.parent = current_process();
	    send(&sig, driver_data[ifd].out_proc);
	    //	efs_clone(driver_data[ifd].out_proc);
	    start(driver_data[ifd].out_proc);
	    if (ifd != ofd)
		driver_data[ofd] = driver_data[ifd]; /* structure copy */
	} else {		/* DO_READ only */
	    driver_data[ifd].ofd = -1;
	}
	(void) driver_select(port_num, driver_data[ifd].in_sig_descr,
			     (ERL_DRV_READ | ERL_DRV_USE), 1);
	return(ifd);
    } else {			/* DO_WRITE only */
	driver_data[ofd].packet_bytes = packet_bytes;
	driver_data[ofd].port_num = port_num;
	driver_data[ofd].report_exit = report_exit;
	driver_data[ofd].ofd = ofd;
	driver_data[ofd].pid = pid;
	driver_data[ofd].alive = 1;
	driver_data[ofd].status = 0;
	driver_data[ofd].in_sig_descr =
	  erl_drv_ose_event_alloc(ERTS_SIGNAL_FD_DRV_ASYNC,ofd,
				    resolve_signal);
	driver_data[ofd].out_sig_descr = driver_data[ofd].in_sig_descr;
	driver_data[ofd].out_proc =
	  create_process(OS_PRI_PROC, "beam_fd_writer",
			 fd_writer_process, 0x800,
			 FD_PROC_PRI, 0, 0, NULL, 0, 0);
	sig = alloc(sizeof(SysDriverConfSignal), ERTS_SIGNAL_FD_DRV_CONFIG);
	sig->conf_async.fd = ofd;
	sig->conf_async.parent = current_process();
	send(&sig, driver_data[ofd].out_proc);
	start(driver_data[ofd].out_proc);
	//efs_clone(driver_data[ifd].out_proc);
	driver_data[ofd].pdl = driver_pdl_create(port_num);
	return(ofd);
    }
}

static int spawn_init()
{
    int i;

    driver_data = (struct driver_data *)
      erts_alloc(ERTS_ALC_T_DRV_TAB, max_files * sizeof(struct driver_data));
    erts_smp_atomic_add_nob(&sys_misc_mem_sz,
			    max_files * sizeof(struct driver_data));

    for (i = 0; i < max_files; i++)
        driver_data[i].pid = -1;

   return 1;
}

static void init_fd_data(int fd, ErlDrvPort port_num)
{

    fd_data[fd].buf = NULL;
    fd_data[fd].cpos = NULL;
    fd_data[fd].remain = 0;
    fd_data[fd].sz = 0;
    fd_data[fd].psz = 0;
}

static ErlDrvData spawn_start(ErlDrvPort port_num,
			      char* name,
			      SysDriverOpts* opts)
{

    long res = 0;

    /* Have to implement for OSE */
    return (ErlDrvData)res;
}

OS_PROCESS(fd_reader_process) {
    union SIGNAL *sig;
    PROCESS parent;
    int fd;
    byte *read_buf;

    SIGSELECT sigsel[] = {1,ERTS_SIGNAL_FD_DRV_CONFIG};

#ifdef ERTS_ENABLE_LOCK_COUNT
    erts_lcnt_init();
#endif

    sig = receive(sigsel);

    fd = sig->conf_async.fd;

    parent = sig->conf_async.parent;
    free_buf(&sig);

#ifdef ERTS_ENABLE_LOCK_CHECK
    {
	char buf[31];
	erts_snprintf(&buf[0], 31, "fd_reader %beu", fd);
	erts_lc_set_thread_name(&buf[0]);
    }
#endif

    if (fd == 0) {
      FILE *ffd = stdin;
      (void)stdin;
    }

    sigsel[1] = ERTS_SIGNAL_FD_DRV_ASYNC;

    read_buf = (byte *) erts_alloc(ERTS_ALC_T_SYS_READ_BUF,
				   ERTS_SYS_READ_BUF_SZ);
    while (1) {
	int errno_copy = errno;
	ssize_t res;
	res = read(fd, read_buf, ERTS_SYS_READ_BUF_SZ);
	sig = alloc(sizeof(SysDriverAsyncSignal), ERTS_SIGNAL_FD_DRV_ASYNC);
	sig->sys_async.buff = read_buf;
	sig->sys_async.res = res;
	if (res <= 0 && errno == EBADF) {
	    fprintf(stderr,"Could not read from input fd (fd %d/ errno %d/ res %d)\n",
                fd, errno, res);
	    break;
	}
	if (errno != errno_copy)
	    sig->sys_async.errno_copy = errno;
	else
	    sig->sys_async.errno_copy = -1;
	sig->sys_async.type = fd;
	send(&sig,parent);
	/* Wait for acc from async_read */
	sig = receive(sigsel);
	free_buf(&sig);
    }
    erts_free(ERTS_ALC_T_SYS_READ_BUF, read_buf);
}

OS_PROCESS(fd_writer_process) {
    union SIGNAL *sig;
    PROCESS parent;
    int fd;
    SIGSELECT sigsel[] = { 1, ERTS_SIGNAL_FD_DRV_CONFIG,
			   ERTS_SIGNAL_FD_DRV_ASYNC };

    /* Only wait for config event with the fd which we are printing to */
    sig = receive(sigsel);

    fd = sig->conf_async.fd;
    parent = sig->conf_async.parent;
    free_buf(&sig);

#ifdef ERTS_ENABLE_LOCK_COUNT
    {
	char buf[31];
	erts_snprintf(&buf[0], 31, "fd_writer %beu", fd);
	erts_lc_set_thread_name(&buf[0]);
    }
#endif

    sigsel[0] = 2;
    /* Why do I need these?!? */
    if (fd == 1) {
      FILE* ffd = stdout;
      (void)stdout;
    } else if (fd == 2) {
      FILE* ffd = stderr;
      (void)stderr;
    }

    while (1) {
	int errno_copy = errno;
	int res;
	SysIOVec *iov0;
	SysIOVec *iov;
	int iovlen;
	int iovcnt;
	int n = 0, i = 0, offset = 0;
	size_t p;
	/* fprintf(stderr,"0x%x: fd_writer, receive\n", current_process()); */
	sig = receive(sigsel);
	/* size = sig->sys_async.res;*/
	if (sig->sig_no == ERTS_SIGNAL_FD_DRV_CONFIG)
	    return;
	driver_pdl_lock(driver_data[fd].pdl);

	iov0 = driver_peekq(driver_data[fd].port_num, &iovlen);

	/* Calculate iovcnt */
	for (p = 0, iovcnt = 0; iovcnt < iovlen;
		p += iov0[iovcnt++].iov_len)
	    ;
	iov = driver_alloc(sizeof(SysIOVec) * iovcnt);
	memcpy(iov, iov0, iovcnt * sizeof(SysIOVec));
	/* Let go of lock until we deque from original vector */
	driver_pdl_unlock(driver_data[fd].pdl);

	if (iovlen > 0) {
	    while(i < iovcnt) {
	      /* We only write 256 chars at the time in order to
		 not overflow the stdout process */
	      if ((iov[i].iov_len-offset) > 256) {
		res = write(fd, iov[i].iov_base+offset, 256);
		if (res < 0)
		    break;
		offset += res;
	      } else {
		res = write(fd, iov[i].iov_base+offset, iov[i].iov_len-offset);
		if (res < 0)
		    break;
		offset = 0;
		i++;
	      }
	      n += res;
	    }
	    if (res > 0)
		res = n;
	} else if (iovlen == 0) {
	    res = 0;
	} else { /* Port has terminated */
	    res = -1;
	}
	driver_free(iov);

	sig->sys_async.buff = NULL;
	sig->sys_async.res = res;
	if (errno != errno_copy)
	    sig->sys_async.errno_copy = errno;
	else
	    sig->sys_async.errno_copy = -1;
	sig->sys_async.type = fd;
	send(&sig, parent);
    }
}

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

static ErlDrvSSizeT fd_control(ErlDrvData drv_data,
			       unsigned int command,
			       char *buf, ErlDrvSizeT len,
			       char **rbuf, ErlDrvSizeT rlen)
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

    CHLD_STAT_LOCK;
    if (opts->read_write & DO_READ) {
	init_fd_data(opts->ifd, port_num);
    }
    if (opts->read_write & DO_WRITE) {
	init_fd_data(opts->ofd, port_num);
    }
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
	ASSERT(erts_smp_atomic_read_nob(&sys_misc_mem_sz) >= fd_data[fd].sz);
	erts_smp_atomic_add_nob(&sys_misc_mem_sz, -1*fd_data[fd].sz);
    }
    fd_data[fd].buf = NULL;
    fd_data[fd].sz = 0;
    fd_data[fd].remain = 0;
    fd_data[fd].cpos = NULL;
    fd_data[fd].psz = 0;
}

static void nbio_stop_fd(ErlDrvPort prt, ErlDrvEvent ev)
{
    int fd;
    driver_select(prt,ev,DO_READ|DO_WRITE,0);
    erl_drv_ose_event_fetch(ev, NULL, &fd);
    clear_fd_data(fd);
    SET_BLOCKING(fd);
}

static void fd_stop(ErlDrvData fd)  /* Does not close the fds */
{
    int ofd;

    nbio_stop_fd(driver_data[(int)(long)fd].port_num,
		 driver_data[(int)(long)fd].in_sig_descr);
    ofd = driver_data[(int)(long)fd].ofd;
    if (ofd != (int)(long)fd && ofd != -1)
      nbio_stop_fd(driver_data[(int)(long)fd].port_num,
		   driver_data[(int)(long)fd].out_sig_descr);
}

/* Note that driver_data[fd].ifd == fd if the port was opened for reading, */
/* otherwise (i.e. write only) driver_data[fd].ofd = fd.  */

static void erl_stop(ErlDrvData fd)
{
    ErlDrvPort prt;
    int ofd;

    prt = driver_data[(int)(long)fd].port_num;
    nbio_stop_fd(prt, driver_data[(int)(long)fd].in_sig_descr);

    ofd = driver_data[(int)(long)fd].ofd;
    if (ofd != (int)(long)fd && (int)(long)ofd != -1)
	nbio_stop_fd(prt, driver_data[(int)(long)fd].out_sig_descr);
    else
	ofd = -1;

    CHLD_STAT_LOCK;

    /* Mark as unused. */
    driver_data[(int)(long)fd].pid = -1;

    CHLD_STAT_UNLOCK;

    /* SMP note: Close has to be last thing done (open file descriptors work
       as locks on driver_data[] entries) */
    driver_select(prt, driver_data[(int)(long)fd].in_sig_descr,
		  ERL_DRV_USE, 0);  /* close(fd); */
    if (ofd >= 0) {
	driver_select(prt, driver_data[(int)(long)fd].out_sig_descr,
		      ERL_DRV_USE, 0);  /* close(ofd); */
    }
}

static void outputv(ErlDrvData e, ErlIOVec* ev)
{
    int fd = (int)(long)e;
    ErlDrvPort ix = driver_data[fd].port_num;
    int pb = driver_data[fd].packet_bytes;
    ErlDrvSizeT sz;
    char lb[4];
    char* lbp;
    ErlDrvSizeT len = ev->size;

    /* (len > ((unsigned long)-1 >> (4-pb)*8)) */
    /*    if (pb >= 0 && (len & (((ErlDrvSizeT)1 << (pb*8))) - 1) != len) {*/
    if (((pb == 2) && (len > 0xffff)) || (pb == 1 && len > 0xff)) {
	driver_failure_posix(ix, EINVAL);
	return; /* -1; */
    }
    /* Handles 0 <= pb <= 4 only */
    put_int32((Uint32) len, lb);
    lbp = lb + (4-pb);

    ev->iov[0].iov_base = lbp;
    ev->iov[0].iov_len = pb;
    ev->size += pb;
    driver_pdl_lock(driver_data[fd].pdl);
    if ((sz = driver_sizeq(ix)) > 0) {
	/* fprintf(stderr,"0x%x: outputv, enq\n", current_process()); */
	driver_enqv(ix, ev, 0);
	if (sz + ev->size >= (1 << 13))
	    set_busy_port(ix, 1);
	driver_pdl_unlock(driver_data[fd].pdl);
    }
    else {
	union SIGNAL *sig;
	/* fprintf(stderr,"0x%x: outputv, enq+sel\n", current_process()); */
	driver_enqv(ix, ev, 0);  /* n is the skip value */
	driver_pdl_unlock(driver_data[fd].pdl);
	driver_select(ix, driver_data[fd].out_sig_descr,
		      ERL_DRV_WRITE|ERL_DRV_USE, 1);
	sig = alloc(sizeof(SysDriverAsyncSignal),ERTS_SIGNAL_FD_DRV_ASYNC);
	sig->sys_async.type = fd;
	sig->sys_async.res = pb+len;
	send(&sig,driver_data[fd].out_proc);
    }
    /* return 0;*/
}


static void output(ErlDrvData e, char* buf, ErlDrvSizeT len)
{
    int fd = (int)(long)e;
    ErlDrvPort ix = driver_data[fd].port_num;
    int pb = driver_data[fd].packet_bytes;
    int ofd = driver_data[fd].ofd;
    ErlDrvSizeT sz;
    char lb[4];
    char* lbp;
#if 0
    struct iovec iv[2];
#endif

    /* (len > ((unsigned long)-1 >> (4-pb)*8)) */
    if (((pb == 2) && (len > 0xffff)) || (pb == 1 && len > 0xff)) {
	driver_failure_posix(ix, EINVAL);
	return; /* -1; */
    }
    put_int32(len, lb);
    lbp = lb + (4-pb);

    driver_pdl_lock(driver_data[fd].pdl);
    if ((sz = driver_sizeq(ix)) > 0) {
	/* fprintf(stderr,"0x%x: output, enq\n", current_process()); */
	driver_enq(ix, lbp, pb);
	driver_enq(ix, buf, len);
	driver_pdl_unlock(driver_data[fd].pdl);
	if (sz + len + pb >= (1 << 13))
	    set_busy_port(ix, 1);
    }
    else {
	union SIGNAL *sig;
	/* fprintf(stderr,"0x%x: output, enq+select\n", current_process()); */
#if 0
	iv[0].iov_base = lbp;
	iv[0].iov_len = pb;  /* should work for pb=0 */
	iv[1].iov_base = buf;
	iv[1].iov_len = len;
#endif
	driver_enq(ix, lbp, pb);
	driver_enq(ix, buf, len);
	driver_pdl_unlock(driver_data[fd].pdl);
	driver_select(ix, driver_data[ofd].out_sig_descr,
		      ERL_DRV_WRITE|ERL_DRV_USE, 1);
	sig = alloc(sizeof(SysDriverAsyncSignal),ERTS_SIGNAL_FD_DRV_ASYNC);
	sig->sys_async.type = fd;
	sig->sys_async.res = pb+len;
	send(&sig,driver_data[fd].out_proc);
    }
    return; /* 0; */
}

static int port_inp_failure(ErlDrvPort port_num, ErlDrvEvent ready_fd, int res)
				/* Result: 0 (eof) or -1 (error) */
{
    int err = errno;
    int fd;

    ASSERT(res <= 0);
    (void) driver_select(port_num, ready_fd, ERL_DRV_READ|ERL_DRV_WRITE, 0);
    erl_drv_ose_event_fetch(ready_fd,NULL,&fd);
    clear_fd_data(fd);
    if (res == 0) {
	if (driver_data[fd].report_exit) {
	    CHLD_STAT_LOCK;

	    if (driver_data[fd].alive) {
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
		int status = driver_data[fd].status;
		CHLD_STAT_UNLOCK;

#if 0 /*ose we should find something for these statuses*/
		/* We need not be prepared for stopped/continued processes. */
		if (WIFSIGNALED(status))
		    status = 128 + WTERMSIG(status);
		else
		    status = WEXITSTATUS(status);
#endif
		driver_report_exit(driver_data[fd].port_num, status);
	    }
       }
       driver_failure_eof(port_num);
    } else {
	driver_failure_posix(port_num, err);
    }
    return 0;
}

static int async_read(ErlDrvEvent fd, byte *buff, int size) {
   union SIGNAL *sigptr = erl_drv_ose_get_signal(fd);
   int res = sigptr->sys_async.res;
   if (res > 0)
       memcpy(buff,sigptr->sys_async.buff,sigptr->sys_async.res);
   errno = sigptr->sys_async.errno_copy;
   send(&sigptr,sender(&sigptr));
   ASSERT(erl_drv_ose_get_signal(fd) == NULL);
   return res;
}

/* fd is the drv_data that is returned from the */
/* initial start routine                        */
/* ready_fd is the descriptor that is ready to read */

static void ready_input(ErlDrvData e, ErlDrvEvent ready_fd)
{
    int fd = (int)(long)e;
    ErlDrvPort port_num;
    int packet_bytes;
    int res;
    Uint h;

    port_num = driver_data[fd].port_num;
    packet_bytes = driver_data[fd].packet_bytes;

    if (packet_bytes == 0) {
	byte *read_buf = (byte *) erts_alloc(ERTS_ALC_T_SYS_READ_BUF,
					     ERTS_SYS_READ_BUF_SZ);
	res = async_read(ready_fd, read_buf, ERTS_SYS_READ_BUF_SZ);
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
    else if (fd_data[fd].remain > 0) { /* We try to read the remainder */
	/* space is allocated in buf */
	res = async_read(ready_fd, (byte*)fd_data[fd].cpos,
		   fd_data[fd].remain);
	if (res < 0) {
	    if ((errno != EINTR) && (errno != ERRNO_BLOCK))
		port_inp_failure(port_num, ready_fd, res);
	}
	else if (res == 0) {
	    port_inp_failure(port_num, ready_fd, res);
	}
	else if (res == fd_data[fd].remain) { /* we're done  */
	    driver_output(port_num, fd_data[fd].buf,
			  fd_data[fd].sz);
	    clear_fd_data(fd);
	}
	else { /*  if (res < fd_data[ready_fd].remain) */
	    fd_data[fd].cpos += res;
	    fd_data[fd].remain -= res;
	}
    }
    else if (fd_data[fd].remain == 0) { /* clean fd */
	byte *read_buf = (byte *) erts_alloc(ERTS_ALC_T_SYS_READ_BUF,
					     ERTS_SYS_READ_BUF_SZ);
	/* We make one read attempt and see what happens */
	res = async_read(ready_fd, read_buf, ERTS_SYS_READ_BUF_SZ);
	if (res < 0) {
	    if ((errno != EINTR) && (errno != ERRNO_BLOCK))
		port_inp_failure(port_num, ready_fd, res);
	}
	else if (res == 0) {	/* eof */
	    port_inp_failure(port_num, ready_fd, res);
	}
	else if (res < packet_bytes - fd_data[fd].psz) {
	    memcpy(fd_data[fd].pbuf+fd_data[fd].psz,
		   read_buf, res);
	    fd_data[fd].psz += res;
	}
	else  { /* if (res >= packet_bytes) */
	    unsigned char* cpos = read_buf;
	    int bytes_left = res;

	    while (1) {
		int psz = fd_data[fd].psz;
		char* pbp = fd_data[fd].pbuf + psz;

		while(bytes_left && (psz < packet_bytes)) {
		    *pbp++ = *cpos++;
		    bytes_left--;
		    psz++;
		}

		if (psz < packet_bytes) {
		    fd_data[fd].psz = psz;
		    break;
		}
		fd_data[fd].psz = 0;

		switch (packet_bytes) {
		case 1: h = get_int8(fd_data[fd].pbuf);  break;
		case 2: h = get_int16(fd_data[fd].pbuf); break;
		case 4: h = get_int32(fd_data[fd].pbuf); break;
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
			erts_smp_atomic_add_nob(&sys_misc_mem_sz, h);
			sys_memcpy(buf, cpos, bytes_left);
			fd_data[fd].buf = buf;
			fd_data[fd].sz = h;
			fd_data[fd].remain = h - bytes_left;
			fd_data[fd].cpos = buf + bytes_left;
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
    ErlDrvPort ix = driver_data[fd].port_num;
    union SIGNAL *sigptr = erl_drv_ose_get_signal(ready_fd);
    ssize_t n;
    struct iovec* iv;
    int vsize;

    while (sigptr != NULL) {

      driver_pdl_lock(driver_data[fd].pdl);
      if ((iv = (struct iovec*) driver_peekq(ix, &vsize)) == NULL) {
	/* fprintf(stderr,"0x%x: ready_output, unselect\n", current_process()); */
	driver_pdl_unlock(driver_data[fd].pdl);
	driver_select(ix, ready_fd, ERL_DRV_WRITE, 0);
	set_busy_port(ix, 0);
	free_buf(&sigptr);
	if ((sigptr = erl_drv_ose_get_signal(ready_fd)) == NULL)
	  return; /* 0; */
	continue;
      }
      driver_pdl_unlock(driver_data[fd].pdl);
      n = sigptr->sys_async.res;
      if (n < 0) {
	if (errno == ERRNO_BLOCK || errno == EINTR) {
	  /* fprintf(stderr,"0x%x: ready_output, send to %x\n", current_process(),driver_data[fd].out_proc);*/
	  send(&sigptr,driver_data[fd].out_proc);
	  if ((sigptr = erl_drv_ose_get_signal(ready_fd)) == NULL)
	    return; /* 0; */
	  continue;
	} else {
	  int res = sigptr->sys_async.errno_copy;
	  /* fprintf(stderr,"0x%x: ready_output, error\n", current_process()); */
	  free_buf(&sigptr);
	  driver_select(ix, ready_fd, ERL_DRV_WRITE, 0);
	  driver_failure_posix(ix, res);
	  if ((sigptr = erl_drv_ose_get_signal(ready_fd)) == NULL)
	    return; /* -1; */
	  continue;
	}
      } else {
	  int remain;
	    driver_pdl_lock(driver_data[fd].pdl);
	    if ((remain = driver_deq(driver_data[fd].port_num, n)) == -1)
		abort();
	    /* fprintf(stderr, "0x%x: ready_output, %d to %x, remain %d\n", current_process(),
			    n, driver_data[fd].out_proc, remain); */
	    driver_pdl_unlock(driver_data[fd].pdl);
	    if (remain != 0)
		send(&sigptr, driver_data[fd].out_proc);
	    else
		continue;
      }
      sigptr = erl_drv_ose_get_signal(ready_fd);
    }
    return; /* 0; */
}

static void stop_select(ErlDrvEvent fd, void* _)
{
    close((int)fd);
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

static pid_t
getpid(void)
{
   return get_bid(current_process());
}

int getpagesize(void)
{
   return 1024;
}


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

    erts_smp_rwmtx_rwlock(&environ_rwmtx);
    res = set_env(get_bid(current_process()), key,
		  value);
    erts_smp_rwmtx_rwunlock(&environ_rwmtx);
    return res;
}


int
erts_sys_unsetenv(char *key)
{
    int res;

    erts_smp_rwmtx_rwlock(&environ_rwmtx);
    res = set_env(get_bid(current_process()),key,NULL);
    erts_smp_rwmtx_rwunlock(&environ_rwmtx);

    return res;
}

int
erts_sys_getenv__(char *key, char *value, size_t *size)
{
    int res;
    char *orig_value = get_env(get_bid(current_process()), key);
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
	free_buf((union SIGNAL **)&orig_value);
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
 *   0, if environment key is set and value fits into buffer res
 *   1, if environment key is set but does not fit into buffer res
 *      res is set with the needed buffer res value
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

void
sys_init_io(void)
{
    fd_data = (struct fd_data *)
	erts_alloc(ERTS_ALC_T_FD_TAB, max_files * sizeof(struct fd_data));
    erts_smp_atomic_add_nob(&sys_misc_mem_sz,
			    max_files * sizeof(struct fd_data));
}

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
erl_assert_error(const char* expr, const char* func,
		 const char* file, int line)
{
    fflush(stdout);
    fprintf(stderr, "%s:%d:%s() Assertion failed: %s\n",
	    file, func, line, expr);
    fflush(stderr);
    ramlog_printf("%s:%d:%s() Assertion failed: %s\n",
		  file, func, line, expr);

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
    pp = erts_thr_id2port_sflgs(rep->port,
				ERTS_PORT_SFLGS_INVALID_DRIVER_LOOKUP);
    CHLD_STAT_LOCK;
#else
    pp = erts_id2port_sflgs(rep->port,
			    NULL,
			    0,
			    ERTS_PORT_SFLGS_INVALID_DRIVER_LOOKUP);
#endif
    if (pp) {
	if (rep->ifd >= 0) {
	    driver_data[rep->ifd].alive = 0;
	    driver_data[rep->ifd].status = status;
	    (void) driver_select(ERTS_Port2ErlDrvPort(pp),
				 rep->in_sig_descr,
				 (ERL_DRV_READ|ERL_DRV_USE),
				 1);
	}
	if (rep->ofd >= 0) {
	    driver_data[rep->ofd].alive = 0;
	    driver_data[rep->ofd].status = status;
	    (void) driver_select(ERTS_Port2ErlDrvPort(pp),
				 rep->out_sig_descr,
				 (ERL_DRV_WRITE|ERL_DRV_USE),
				 1);
	}
#ifdef ERTS_SMP
	erts_thr_port_release(pp);
#else
	erts_port_release(pp);
#endif
    }
    erts_free(ERTS_ALC_T_PRT_REP_EXIT, rep);
}

#define ERTS_REPORT_EXIT_STATUS report_exit_status

/*
 * Called from schedule() when it runs out of runnable processes,
 * or when Erlang code has performed INPUT_REDUCTIONS reduction
 * steps. runnable == 0 iff there are no runnable Erlang processes.
 */
void
erl_sys_schedule(int runnable)
{
    ASSERT(get_fsem(current_process()) == 0);
#ifdef ERTS_SMP
    ASSERT(erts_get_scheduler_data()->no == 1);
    ERTS_CHK_IO(!runnable);
#else
    ERTS_CHK_IO( 1 );
#endif
    ASSERT(get_fsem(current_process()) == 0);
    ERTS_SMP_LC_ASSERT(!erts_thr_progress_is_blocking());
}


#ifdef ERTS_SMP

void
erts_sys_main_thread(void)
{
    erts_thread_disable_fpe();

    /* Become signal receiver thread... */
#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_lc_set_thread_name("signal_receiver");
#endif

    while (1) {
       static const SIGSELECT sigsel[] = {0};
       union SIGNAL *msg = receive(sigsel);

       fprintf(stderr,"Main thread got message %d from 0x%x!!\r\n",
               msg->sig_no, sender(&msg));
       free_buf(&msg);
    }
}

#endif /* ERTS_SMP */

void
erl_sys_args(int* argc, char** argv)
{
    int i, j;

    erts_smp_rwmtx_init(&environ_rwmtx, "environ");

    init_check_io();

    /* Handled arguments have been marked with NULL. Slide arguments
       not handled towards the beginning of argv. */
    for (i = 0, j = 0; i < *argc; i++) {
	if (argv[i])
	    argv[j++] = argv[i];
    }
    *argc = j;

}
