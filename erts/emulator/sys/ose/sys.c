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
#include "aio.h"
#include "pm.h"
#include "fcntl.h"

/* Set the define to 1 to get some logging */
#if 0
#include "ramlog.h"
#define LOG(output) ramlog_printf output
#else
#define LOG(output)
#endif

extern char **environ;
static erts_smp_rwmtx_t environ_rwmtx;
static PROCESS sig_proxy_pid = 0;

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
#define FD_PROC_PRI get_pri(current_process())

typedef struct ErtsSysReportExit_ ErtsSysReportExit;
struct ErtsSysReportExit_ {
    ErtsSysReportExit  *next;
    Eterm              port;
    int                pid;
    int                ifd;
    int                ofd;
    ErlDrvEvent        attach_event;
    ErlDrvEvent        input_event;
    ErlDrvEvent        output_event;
};

/* This data is shared by these drivers - initialized by spawn_init() */
static struct driver_data {
   ErlDrvPort          port_num;
   int                 ofd;
   int                 ifd;
   int                 packet_bytes;
   ErtsSysReportExit   *report_exit;
   int                 pid;
   int                 alive;
   int                 status;
   ErlDrvEvent         input_event;
   ErlDrvEvent         output_event;
   struct aiocb        aiocb;
   FmHandle            handle;
   char                *install_handle;
} *driver_data;			/* indexed by fd */

struct async {
  SIGSELECT            signo;
  ErlDrvTermData       port;
  ErlDrvTermData       proc;
  PROCESS              spid;
  PROCESS              target;
  Uint32               ref;
};

static ErtsSysReportExit *report_exit_list;
static ERTS_INLINE void report_exit_status(ErtsSysReportExit *rep, int status);

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

#define SET_AIO(REQ,FD,SIZE,BUFF)                                       \
   memset(&(REQ),0,sizeof(REQ));                                        \
   (REQ).aio_fildes = FD;                                               \
   (REQ).aio_offset = FM_POSITION_CURRENT;                              \
   (REQ).aio_nbytes = SIZE;                                             \
   (REQ).aio_buf = BUFF;                                                \
   (REQ).aio_sigevent.sigev_notify = SIGEV_NONE

/* the first sizeof(struct aiocb *) bytes of the write buffer
 * will contain the pointer to the aiocb struct, this needs
 * to be freed between asynchronous writes.
 * A write of 0 bytes is ignored. */
#define WRITE_AIO(FD,SIZE,BUFF) do {                                    \
   if (SIZE > 0) {                                                      \
      struct aiocb *write_req = driver_alloc(sizeof(struct aiocb));     \
      char *write_buff = driver_alloc((sizeof(char)*SIZE)+1+            \
            (sizeof(struct aiocb *)));                                  \
      *(struct aiocb **)write_buff = (struct aiocb *)write_req;         \
      write_buff += sizeof(struct aiocb *);                             \
      memcpy(write_buff,BUFF,SIZE+1);                                   \
      SET_AIO(*write_req,FD,SIZE,write_buff);                           \
      if (aio_write(write_req))						\
	ramlog_printf("%s:%d: write failed with %d\n",			\
		      __FILE__,__LINE__,errno);				\
   }                                                                    \
} while(0)

/* free the write_buffer and write_req
 * created in the WRITE_AIO() request macro */
#define FREE_AIO(ptr) do {                                              \
   struct aiocb *aiocb_ptr;                                             \
   char *buffer_ptr;                                                    \
   aiocb_ptr = *(struct aiocb **)((ptr)-sizeof(struct aiocb *));        \
   buffer_ptr = (((char*)ptr)-sizeof(struct aiocb *));			\
   driver_free(aiocb_ptr);                                              \
   driver_free(buffer_ptr);                                             \
} while(0)

#define DISPATCH_AIO(sig) do {                                          \
   if (aio_dispatch(sig))						\
     ramlog_printf("%s:%d: dispatch failed with %d\n",			\
		   __FILE__,__LINE__,errno);				\
   } while(0)

#define AIO_PIPE_SIZE 1024

/* debug print macros */
#define DEBUG_RES 0

#ifdef DEBUG_RES
#define DEBUG_CHECK_RES(actual, expected) \
   do { \
      if (actual != expected ) { \
         ramlog_printf("Result check failed" \
                       " got: 0x%08x expected:0x%08x\nat: %s:%d\n", \
               actual, expected, __FILE__, __LINE__); \
         abort();  /* This might perhaps be too harsh? */ \
      } \
   } while(0)
#else
#define DEBUG_CHECK_RES
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
static PROCESS get_signal_proxy_pid(void);

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

/* The two functions below are stolen from win_con.c
   They have to use malloc/free/realloc directly becasue
   we want to do able to do erts_printf very early on.
 */
#define VPRINTF_BUF_INC_SIZE 128
static erts_dsprintf_buf_t *
grow_vprintf_buf(erts_dsprintf_buf_t *dsbufp, size_t need)
{
    char *buf;
    size_t size;

    ASSERT(dsbufp);

    if (!dsbufp->str) {
	size = (((need + VPRINTF_BUF_INC_SIZE - 1)
		 / VPRINTF_BUF_INC_SIZE)
		* VPRINTF_BUF_INC_SIZE);
	buf = (char *) malloc(size * sizeof(char));
    }
    else {
	size_t free_size = dsbufp->size - dsbufp->str_len;

	if (need <= free_size)
	    return dsbufp;

	size = need - free_size + VPRINTF_BUF_INC_SIZE;
	size = (((size + VPRINTF_BUF_INC_SIZE - 1)
		 / VPRINTF_BUF_INC_SIZE)
		* VPRINTF_BUF_INC_SIZE);
	size += dsbufp->size;
        buf = (char *) realloc((void *) dsbufp->str,
			       size * sizeof(char));
    }
    if (!buf)
	return NULL;
    if (buf != dsbufp->str)
	dsbufp->str = buf;
    dsbufp->size = size;
    return dsbufp;
}

static int erts_sys_ramlog_printf(char *format, va_list arg_list)
{
    int res,i;
    erts_dsprintf_buf_t dsbuf = ERTS_DSPRINTF_BUF_INITER(grow_vprintf_buf);
    res = erts_vdsprintf(&dsbuf, format, arg_list);
    if (res >= 0) {
      for (i = 0; i < dsbuf.str_len; i+= 50)
	/* We print 50 characters at a time because otherwise
	   the ramlog looks broken */
        ramlog_printf("%.*s",dsbuf.str_len-50 < 0?dsbuf.str_len:50,dsbuf.str+i);
    }
    if (dsbuf.str)
      free((void *) dsbuf.str);
    return res;
}

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

    erts_printf_stdout_func = erts_sys_ramlog_printf;

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

union SIGNAL {
    SIGSELECT sig_no;
    struct FmReadPtr fm_read_reply;
    struct FmWritePtr fm_write_reply;
    struct async async;
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
static void stop_select(ErlDrvEvent, void*);

static PROCESS
get_signal_proxy_pid(void) {
   union SIGNAL *sig;
   SIGSELECT any_sig[] = {1,ERTS_SIGNAL_OSE_DRV_ATTACH};

   if (!sig_proxy_pid) {
      sig = alloc(sizeof(union SIGNAL), ERTS_SIGNAL_OSE_DRV_ATTACH);
      hunt("ose_signal_driver_proxy", 0, NULL, &sig);
      sig = receive(any_sig);
      sig_proxy_pid = sender(&sig);
      free_buf(&sig);
   }
   ASSERT(sig_proxy_pid);
   return sig_proxy_pid;
}

static ErlDrvOseEventId
resolve_signal(union SIGNAL* sig) {
   switch(sig->sig_no) {

      case FM_READ_PTR_REPLY:
            return (ErlDrvOseEventId)sig->fm_read_reply.handle;

       case FM_WRITE_PTR_REPLY:
            return (ErlDrvOseEventId)sig->fm_write_reply.handle;

       case ERTS_SIGNAL_OSE_DRV_ATTACH:
            return (ErlDrvOseEventId)sig->async.target;

       default:
            break;
    }
    return (ErlDrvOseEventId)-1;
}

struct erl_drv_entry spawn_driver_entry = {
    spawn_init,
    spawn_start,
    NULL, /* erl_stop, */
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
    NULL,
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

static void
set_spawn_fd(int local_fd, int remote_fd, PROCESS remote_pid) {
   PROCESS vm_pid;
   FmHandle handle;
   char env_val[55];
   char env_name[10];
   EfsStatus efs_res;

   /* get pid of pipevm and handle of chosen fd */
   efs_res = efs_examine_fd(local_fd, FLIB_FD_VMPID, &vm_pid, 0);
   DEBUG_CHECK_RES(efs_res, EFS_SUCCESS);

   /* setup the file descriptor to buffer per line */
   efs_res = efs_config_fd(local_fd, FLIB_FD_BUFMODE, FM_BUFF_LINE,
                    FLIB_FD_BUFSIZE, 80, 0);
   DEBUG_CHECK_RES(efs_res, EFS_SUCCESS);

   /* duplicate handle  and set spawn pid owner */
   efs_res = efs_dup_to(local_fd, remote_pid, &handle);
   DEBUG_CHECK_RES(efs_res, EFS_SUCCESS);

   sprintf(env_name, "FD%d", remote_fd);

   /* Syntax of the environment variable:
    * "FD#" "<pid of pipevm>,<handle>,<buffer mode>,<buff size>,<omode>" */
   sprintf(env_val, "0x%lx,0x%lx,%lu,%lu,0x%x",
                    vm_pid, handle,
                    FM_BUFF_LINE, 80,
                    O_APPEND);

   set_env(remote_pid, env_name, env_val);
}

static ErlDrvData
set_driver_data(ErlDrvPort port_num,
			   int ifd,
			   int ofd,
			   int packet_bytes,
			   int read_write,
			   int exit_status,
			   PROCESS pid)
{
    Port *prt;
    ErtsSysReportExit *report_exit;

    prt = erts_drvport2port(port_num);
    if (prt != ERTS_INVALID_ERL_DRV_PORT) {
       prt->os_pid = pid;
    }

    /* READ */
    if (read_write & DO_READ) {
       EfsStatus res = efs_examine_fd(ifd, FLIB_FD_HANDLE,
				      &driver_data[ifd].handle, 0);
       if (res != EFS_SUCCESS)
	 ramlog_printf("%s:%d: efs_examine_fd(%d) failed with %d\n",
		       __FILE__,__LINE__,ifd,errno);
       driver_data[ifd].ifd = ifd;
       driver_data[ifd].packet_bytes = packet_bytes;
       driver_data[ifd].port_num = port_num;
       driver_data[ifd].pid = pid;

       /* async read struct */
       memset(&driver_data[ifd].aiocb, 0, sizeof(struct aiocb));
       driver_data[ifd].aiocb.aio_buf = driver_alloc(AIO_PIPE_SIZE);
       driver_data[ifd].aiocb.aio_fildes = ifd;
       driver_data[ifd].aiocb.aio_nbytes = (packet_bytes?packet_bytes:AIO_PIPE_SIZE);
       driver_data[ifd].alive = 1;
       driver_data[ifd].status = 0;
       driver_data[ifd].input_event =
          erl_drv_ose_event_alloc(FM_READ_PTR_REPLY,
                driver_data[ifd].handle, resolve_signal,
                &driver_data[ifd].ifd);

       /* READ & WRITE */
       if (read_write & DO_WRITE) {
          driver_data[ifd].ofd = ofd;
          efs_examine_fd(ofd, FLIB_FD_HANDLE, &driver_data[ofd].handle, 0);

          driver_data[ifd].output_event =
             erl_drv_ose_event_alloc(FM_WRITE_PTR_REPLY,
                   driver_data[ofd].handle, resolve_signal,
                   &driver_data[ofd].ofd);
          driver_data[ofd].pid = pid;
          if (ifd != ofd) {
             driver_data[ofd] = driver_data[ifd];
             driver_data[ofd].aiocb.aio_buf = NULL;
           }
       }
       else { /* READ ONLY */
          driver_data[ifd].ofd = -1;
       }

       /* enable input event */
       (void) driver_select(port_num, driver_data[ifd].input_event,
			     (ERL_DRV_READ | ERL_DRV_USE), 1);

       if (aio_read(&driver_data[ifd].aiocb))
	 ramlog_printf("%s:%d: aio_read(%d) failed with %d\n",
		       __FILE__,__LINE__,ifd,errno);
    }
    else { /* WRITE ONLY */
       efs_examine_fd(ofd, FLIB_FD_HANDLE, &driver_data[ofd].handle, 0);
       driver_data[ofd].packet_bytes = packet_bytes;
       driver_data[ofd].port_num = port_num;
       driver_data[ofd].ofd = ofd;
       driver_data[ofd].pid = pid;
       driver_data[ofd].alive = 1;
       driver_data[ofd].status = 0;
       driver_data[ofd].output_event =
          erl_drv_ose_event_alloc(FM_WRITE_PTR_REPLY, driver_data[ofd].handle,
				    resolve_signal, &driver_data[ofd].ofd);
       driver_data[ofd].input_event = driver_data[ofd].output_event;
    }

    /* this is used for spawned load modules, and is needed
     * to properly uninstall them */
    if (exit_status) {
       struct PmProgramInfo *info;
       int install_handle_size;
       union SIGNAL *sig;
       PmStatus pm_status;
       report_exit = erts_alloc(ERTS_ALC_T_PRT_REP_EXIT,
				 sizeof(ErtsSysReportExit));
       report_exit->next = report_exit_list;
       report_exit->port = erts_drvport2id(port_num);
       report_exit->pid = pid;
       report_exit->ifd = (read_write & DO_READ) ? ifd : -1;
       report_exit->ofd = (read_write & DO_WRITE) ? ofd : -1;
       report_exit_list = report_exit;
       report_exit->attach_event =
          erl_drv_ose_event_alloc(ERTS_SIGNAL_OSE_DRV_ATTACH, pid,
				resolve_signal, &driver_data[ifd].ifd);

       /* setup ifd and ofd report exit */
       driver_data[ifd].report_exit = report_exit;
       driver_data[ofd].report_exit = report_exit;

       pm_status = ose_pm_program_info(pid, &info);
       DEBUG_CHECK_RES(pm_status, PM_SUCCESS);

       install_handle_size = strlen(info->install_handle)+1;
       driver_data[ifd].install_handle = driver_alloc(install_handle_size);
       strcpy(driver_data[ifd].install_handle,
             info->install_handle);

       free_buf((union SIGNAL **)&info);

       sig = alloc(sizeof(struct async), ERTS_SIGNAL_OSE_DRV_ATTACH);
       sig->async.target = pid;
       send(&sig, get_signal_proxy_pid());

       /* this event will trigger when we receive an attach signal
        * from the recently dead load module */
       (void)driver_select(port_num,report_exit->attach_event, DO_READ, 1);
    }
    else {
       report_exit = NULL;
    }

    /* the return value is the pointer to the driver_data struct we created
     * in this function, it will be used in the drivers input
     * and output functions */
    return (ErlDrvData)((!(read_write & DO_READ) && read_write & DO_WRITE)
          ? &driver_data[ofd]
          : &driver_data[ifd]);
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

static void
init_fd_data(int fd, ErlDrvPort port_num)
{
    fd_data[fd].buf = NULL;
    fd_data[fd].cpos = NULL;
    fd_data[fd].remain = 0;
    fd_data[fd].sz = 0;
    fd_data[fd].psz = 0;
}

/* FIXME write a decent text on pipes on ose */
static ErlDrvData
spawn_start(ErlDrvPort port_num, char* name, SysDriverOpts* opts)
{
    int ifd[2];
    int ofd[2];
    static uint32_t ticker = 1;
    PmStatus pm_status;
    OSDOMAIN domain = PM_NEW_DOMAIN;
    PROCESS progpid, mainbid, mainpid;
    char *handle = NULL;
    struct PmProgramInfo *info;
    char *args = NULL;
    char *tmp_handle;
    ErlDrvData res = (ErlDrvData)-1;
    int handle_size;
    char *ptr;

   
    args = driver_alloc(strlen(name)+1);
    strcpy(args, name);
    /* We need to handle name in three parts 
     * - install handle (must be unique)
     * - install binary (needed for ose_pm_install_load_module())
     * - full path (as argument to the spawned applications env.var
     */

    /* full path including arguments */
    args = driver_alloc(strlen(name)+1);
    strcpy(args, name);

    /* handle path */
    tmp_handle = strrchr(name, '/');
    if (tmp_handle == NULL) {
       tmp_handle = name;
    }
    else {
       tmp_handle++;
    }

    /* handle args */
    ptr = strchr(tmp_handle, ' ');
    if (ptr != NULL) {
       *ptr = '\0';
       handle_size = ptr - tmp_handle;
    }
    else {
       handle_size = strlen(name)+1;
    }

    /* make room for ticker */
    handle_size += (ticker<10)?3:((ticker<100)?4:5);
    handle = driver_alloc(handle_size);
    
    do {
       snprintf(handle, handle_size, "%s_%d", tmp_handle, ticker);
       pm_status = ose_pm_install_load_module(0, "ELF", name, handle,
                                              0, 0, NULL);
       ticker++;
    } while (pm_status == PM_EINSTALL_HANDLE_ALREADY_INSTALLED);

    if (pm_status != PM_SUCCESS) {
       errno = ENOSYS; /* FIXME add comment */
       return ERL_DRV_ERROR_ERRNO; 
    }

    /* Create Program */
    pm_status = ose_pm_create_program(&domain, handle, 0, 0,
                                      NULL, &progpid, &mainbid);
    DEBUG_CHECK_RES(pm_status, PM_SUCCESS);

    /* Get the mainpid from the newly created program */
    pm_status = ose_pm_program_info(progpid, &info);
    DEBUG_CHECK_RES(pm_status, PM_SUCCESS);

    mainpid = info->main_process;
    free_buf ((union SIGNAL **)&info);

    /* pipevm needs to be started
     * pipe will return 0 if success, -1 if not,
     * errno will be set */
    if (pipe(ifd) != 0 || pipe(ofd) != 0) {
       DEBUG_CHECK_RES(0, -1);
       ASSERT(0);
    }

    /* setup driver data */
    res = set_driver_data(port_num, ofd[0], ifd[1], opts->packet_bytes,
              opts->read_write, 1 /* opts->exit_status */, progpid);

    /* init the fd_data array for read/write */
    init_fd_data(ofd[0], port_num);
    init_fd_data(ifd[1], port_num);

    /* setup additional configurations
     * for the spawned applications environment */
    if (args != NULL) {
       set_env(progpid, "ARGV", args);
    }
    set_env(mainbid, "EFS_RESOLVE_TMO", 0);
    set_spawn_fd(ifd[0], 0, mainpid);
    set_spawn_fd(ofd[1], 1, mainpid);
    set_spawn_fd(ofd[1], 2, mainpid);

    /* start the spawned program */
    pm_status = ose_pm_start_program(mainbid);
    DEBUG_CHECK_RES(pm_status, PM_SUCCESS);

    /* close unused fd's */
    close(ifd[0]);
    close(ofd[1]);

    if (handle) {
       driver_free(handle);
    }

    return (ErlDrvData)res;
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
   struct driver_data *data = (struct driver_data *)drv_data;
    char resbuff[2*sizeof(Uint32)];
    switch (command) {
    case FD_CTRL_OP_GET_WINSIZE:
	{
	    Uint32 w,h;
	    if (fd_get_window_size(data->ifd,&w,&h))
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
    res = set_driver_data(port_num, opts->ifd, opts->ofd,
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
    int *fd;
    driver_select(prt,ev,DO_READ|DO_WRITE,0);
    erl_drv_ose_event_fetch(ev, NULL, NULL, (void **)&fd);
    clear_fd_data(*fd);
    SET_BLOCKING(*fd);
}

static void fd_stop(ErlDrvData drv_data)  /* Does not close the fds */
{
   struct driver_data *data = (struct driver_data *)drv_data;

   if (data->ofd != -1) {
      if (data->ifd != data->ofd) { /* read and write */
         nbio_stop_fd(data->port_num, data->input_event);
         nbio_stop_fd(data->port_num, data->output_event);
      }
      else { /* write only */
         nbio_stop_fd(data->port_num, data->output_event);
      }
   }
   else { /* read only */
      nbio_stop_fd(data->port_num, data->input_event);
   }
}


static void erl_stop(ErlDrvData drv_data)
{
   struct driver_data *data = (struct driver_data *)drv_data;

   CHLD_STAT_LOCK;
   data->pid = -1;
   CHLD_STAT_UNLOCK;

   if (data->ofd != -1) {
      if (data->ifd != data->ofd) { /* read and write */
         nbio_stop_fd(data->port_num, data->input_event);
         nbio_stop_fd(data->port_num, data->output_event);
      }
      else { /* write only */
         nbio_stop_fd(data->port_num, data->output_event);
      }
   }
   else { /* read only */
      nbio_stop_fd(data->port_num, data->input_event);
   }
   close(data->ifd);
   close(data->ofd);
}

/* The parameter e is a pointer to the driver_data structure
 * related to the fd to be used as output */
static void output(ErlDrvData drv_data, char* buf, ErlDrvSizeT len)
{
    ErlDrvSizeT sz;
    char lb[4];
    char* lbp;
    struct driver_data *data = (struct driver_data *)drv_data;

    if (((data->packet_bytes == 2) &&
             (len > 0xffff)) || (data->packet_bytes == 1 && len > 0xff)) {
	driver_failure_posix(data->port_num, EINVAL);
	return; /* -1; */
    }
    put_int32(len, lb);
    lbp = lb + (4-(data->packet_bytes));

    if ((sz = driver_sizeq(data->port_num)) > 0) {
       if (data->packet_bytes != 0) {
          driver_enq(data->port_num, lbp, data->packet_bytes);
       }
       driver_enq(data->port_num, buf, len);

       if (sz + len + data->packet_bytes >= (1 << 13))
	    set_busy_port(data->port_num, 1);
    }
    else {
       char *pbbuf;
       if (data->packet_bytes != 0) {
          pbbuf = malloc(len + data->packet_bytes);
          int i;
          for (i = 0; i < data->packet_bytes; i++) {
             *pbbuf++ = *lbp++;
          }
          strncpy(pbbuf, buf, len);
          pbbuf -= data->packet_bytes;
       }
       driver_select(data->port_num, data->output_event,
		      ERL_DRV_WRITE|ERL_DRV_USE, 1);
       WRITE_AIO(data->ofd, 
             (data->packet_bytes ? len+data->packet_bytes : len), 
             (data->packet_bytes ? pbbuf : buf));
       if (data->packet_bytes != 0) free(pbbuf);
    }
    return; /* 0; */
}

/* This function is being run when we in recieve
 * either a read of 0 bytes, or the attach signal from a dying
 * spawned load module */
static int port_inp_failure(ErlDrvPort port_num, ErlDrvEvent ready_fd, int res)
				/* Result: 0 (eof) or -1 (error) */
{
    int *fd;
    SIGSELECT sig_no;
    ASSERT(res <= 0);

    erl_drv_ose_event_fetch(ready_fd,&sig_no, NULL, (void **)&fd);
    /* As we need to handle two signals, we do this in two steps */
    if (driver_data[*fd].alive) {
       report_exit_status(driver_data[*fd].report_exit, 0); /* status? */
    }
    else {
       driver_select(port_num,ready_fd,DO_READ|DO_WRITE,0);
       clear_fd_data(*fd);
       driver_report_exit(driver_data[*fd].port_num, driver_data[*fd].status);
       /* As we do not really know if the spawn has crashed or exited nicely
        * we do not check the result status of the following call.. FIXME
        * can we handle this in a better way? */
       ose_pm_uninstall_load_module(driver_data[*fd].install_handle);
       driver_free(driver_data[*fd].install_handle);
       driver_free((void *)driver_data[*fd].aiocb.aio_buf);

       close(*fd);
    }

    return 0;
}

/* The parameter e is a pointer to the driver_data structure
 * related to the fd to be used as output.
 * ready_fd is the event that triggered this call to ready_input */
static void ready_input(ErlDrvData drv_data, ErlDrvEvent ready_fd)
{
    int res;
    Uint h;
    char *buf;
    union SIGNAL *sig;
    struct driver_data *data = (struct driver_data *)drv_data;

    sig = erl_drv_ose_get_signal(ready_fd);
    ASSERT(sig);


   while (sig) {
      /* If we've recieved an attach signal, we need to handle
       * it in port_inp_failure */
      if (sig->sig_no == ERTS_SIGNAL_OSE_DRV_ATTACH) {
         port_inp_failure(data->port_num, ready_fd, 0);
       }
       else {
          res = sig->fm_read_reply.actual;
          if (res == 0) {
             port_inp_failure(data->port_num, ready_fd, res);
             break;
          }

          if (data->packet_bytes == 0) {
             if (res < 0) {
                if ((errno != EINTR) && (errno != ERRNO_BLOCK)) {
                   port_inp_failure(data->port_num, ready_fd, res);
                }
             }
             else if (res == 0) {
                /* read of 0 bytes, eof, otherside of pipe is assumed dead */
                port_inp_failure(data->port_num, ready_fd, res);
                break;
             }
             else {
                buf = driver_alloc(res);
                memcpy(buf, (void *)data->aiocb.aio_buf, res);
                driver_select(data->port_num, data->output_event,
                      ERL_DRV_WRITE|ERL_DRV_USE, 1);
                driver_output(data->port_num, (char*) buf, res);
                driver_free(buf);
             }
                /* clear the previous read */
                memset(data->aiocb.aio_buf, 0, res);

                /* issue a new read */
                DISPATCH_AIO(sig);
                aio_read(&data->aiocb);
          }
          else if (data->packet_bytes && fd_data[data->ifd].remain > 0) {
             /* we've read a partial package, or a header */

             if (res == fd_data[data->ifd].remain) { /* we are done! */
                char *buf = data->aiocb.aio_buf;
                int i;

                /* do we have anything buffered? */
                if (fd_data[data->ifd].buf != NULL) {
                   memcpy(fd_data[data->ifd].buf + fd_data[data->ifd].sz,
                          buf, res);
                   buf = fd_data[data->ifd].buf;
                }

                fd_data[data->ifd].sz += res;
                driver_output(data->port_num, buf, (fd_data[data->ifd].sz>0?fd_data[data->ifd].sz:res));
                clear_fd_data(data->ifd);

                /* clear the previous read */
                memset(data->aiocb.aio_buf, 0, res);
             
                /* issue a new read */
                DISPATCH_AIO(sig);
                data->aiocb.aio_nbytes = data->packet_bytes;

                if (data->aiocb.aio_buf == NULL) {
                   port_inp_failure(data->port_num, ready_fd, -1);
                }
                aio_read(&data->aiocb);
             }
             else if(res < fd_data[data->ifd].remain) { /* received part of a package */
                if (fd_data[data->ifd].sz == 0) {

                   fd_data[data->ifd].sz += res;
                   memcpy(fd_data[data->ifd].buf, data->aiocb.aio_buf, res);
                   fd_data[data->ifd].remain -= res;
                }
                else {
                   memcpy(fd_data[data->ifd].buf + fd_data[data->ifd].sz,
                          data->aiocb.aio_buf, res);
                   fd_data[data->ifd].sz += res;
                   fd_data[data->ifd].remain -= res;
                }
                /* clear the previous read */
                memset(data->aiocb.aio_buf, 0, res);

                /* issue a new read */
                DISPATCH_AIO(sig);
                data->aiocb.aio_nbytes = fd_data[data->ifd].remain;

                if (data->aiocb.aio_buf == NULL) {
                    port_inp_failure(data->port_num, ready_fd, -1);
                }
                aio_read(&data->aiocb);
             }
          }
          else if (data->packet_bytes && fd_data[data->ifd].remain == 0) { /* we've recieved a header */

             /* analyze the header FIXME  */
             switch (data->packet_bytes) {
                case 1: h = get_int8(data->aiocb.aio_buf);  break;
                case 2: h = get_int16(data->aiocb.aio_buf); break;
                case 4: h = get_int32(data->aiocb.aio_buf); break;
             }

             fd_data[data->ifd].buf = erts_alloc_fnf(ERTS_ALC_T_FD_ENTRY_BUF, h + data->packet_bytes);
             fd_data[data->ifd].remain = ((h + data->packet_bytes) - res);

             /* clear the previous read */
             memset(data->aiocb.aio_buf, 0, data->packet_bytes);

             /* issue a new read */
             DISPATCH_AIO(sig);
             data->aiocb.aio_nbytes = h;

             if (data->aiocb.aio_buf == NULL) {
                port_inp_failure(data->port_num, ready_fd, -1);
             }
             aio_read(&data->aiocb);
          }
       }
       sig = erl_drv_ose_get_signal(ready_fd);
    }
}


/* The parameter e is a pointer to the driver_data structure
 * related to the fd to be used as output.
 * ready_fd is the event that triggered this call to ready_input */
static void ready_output(ErlDrvData drv_data, ErlDrvEvent ready_fd)
{
   SysIOVec *iov;
   int vlen;
   int res;
   union SIGNAL *sig;
   struct driver_data *data = (struct driver_data *)drv_data;

   sig = erl_drv_ose_get_signal(ready_fd);
   ASSERT(sig);

   while (sig != NULL) {
      if (sig->fm_write_reply.actual <= 0) {
         int status;

         status = efs_status_to_errno(sig->fm_write_reply.status);
         driver_select(data->port_num, ready_fd, ERL_DRV_WRITE, 0);
         DISPATCH_AIO(sig);
         FREE_AIO(sig->fm_write_reply.buffer);

         driver_failure_posix(data->port_num, status);
      }
      else { /* written bytes > 0 */
          iov = driver_peekq(data->port_num, &vlen);
          if (vlen > 0) {
             DISPATCH_AIO(sig);
             FREE_AIO(sig->fm_write_reply.buffer);
             res = driver_deq(data->port_num, iov[0].iov_len);
             if (res > 0) { 
                iov = driver_peekq(data->port_num, &vlen);
                WRITE_AIO(data->ofd, iov[0].iov_len, iov[0].iov_base);
             }
         }
         else if (vlen == 0) {
            DISPATCH_AIO(sig);
            FREE_AIO(sig->fm_write_reply.buffer);
         }

      }
      sig = erl_drv_ose_get_signal(ready_fd);
   }
}

static void stop_select(ErlDrvEvent ready_fd, void* _)
{
   int *fd;
   erl_drv_ose_event_fetch(ready_fd, NULL, NULL, (void **)&fd);
   erl_drv_ose_event_free(ready_fd);
   close(*fd);
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
	    file, line, func, expr);
    fflush(stderr);
    ramlog_printf("%s:%d:%s() Assertion failed: %s\n",
		  file, line, func, expr);

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
   if (rep->ifd >= 0) {
      driver_data[rep->ifd].alive = 0;
      driver_data[rep->ifd].status = status;
   }
   if (rep->ofd >= 0) {
      driver_data[rep->ofd].alive = 0;
      driver_data[rep->ofd].status = status;
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
