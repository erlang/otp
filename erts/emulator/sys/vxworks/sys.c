/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1997-2010. All Rights Reserved.
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
 * system-dependent functions
 * 
 */
#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif
#include <vxWorks.h>
#include <version.h>
#include <string.h>
#include <types.h>
#include <sigLib.h>
#include <ioLib.h>
#include <iosLib.h>
#include <envLib.h>
#include <fioLib.h>
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <symLib.h>
#include <sysLib.h>
#include <sysSymTbl.h>
#include <loadLib.h>
#include <taskLib.h>
#include <taskVarLib.h>
#include <taskHookLib.h>
#include <tickLib.h>
#include <time.h>
#include <rngLib.h>
#include <semLib.h>
#include <selectLib.h>
#include <sockLib.h>
#include <a_out.h>
#include <wdLib.h>
#include <timers.h>
#include <ctype.h>
#include <sys/stat.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <stdarg.h>


#ifndef WANT_NONBLOCKING
#define WANT_NONBLOCKING
#endif

#include "sys.h"
#include "erl_alloc.h"

/* don't need global.h, but bif_table.h (included by bif.h) won't compile otherwise */
#include "global.h" 
#include "bif.h"

#include "erl_sys_driver.h"

#include "elib_stat.h"

#include "reclaim_private.h" /* Some more or less private reclaim facilities */

#ifndef RETSIGTYPE
#define RETSIGTYPE void
#endif

EXTERN_FUNCTION(void, erl_start, (int, char**));
EXTERN_FUNCTION(void, erl_exit, (int n, char*, _DOTS_));
EXTERN_FUNCTION(void, erl_error, (char*, va_list));
EXTERN_FUNCTION(int, driver_interrupt, (int, int));
EXTERN_FUNCTION(void, increment_time, (int));
EXTERN_FUNCTION(int, next_time, (_VOID_));
EXTERN_FUNCTION(void, set_reclaim_free_function, (FreeFunction));
EXTERN_FUNCTION(int, erl_mem_info_get, (MEM_PART_STATS *));
EXTERN_FUNCTION(void, erl_crash_dump, (char* file, int line, char* fmt, ...));

#define ISREG(st) (((st).st_mode&S_IFMT) == S_IFREG)

/* these are defined in usrLib.c */
extern int spTaskPriority, spTaskOptions;

/* forward declarations */
static FUNCTION(FUNCPTR, lookup, (char*));
static FUNCTION(int, read_fill, (int, char*, int));
#if (CPU == SPARC)
static FUNCTION(RETSIGTYPE, fpe_sig_handler, (int)); /*where is this fun? */
#elif (CPU == PPC603)
static FUNCTION(void, fix_registers, (void));
#endif
static FUNCTION(void, close_pipes, (int*, int*, int));
static FUNCTION(void, delete_hook, (void));
static FUNCTION(void, initialize_allocation, (void));

FUNCTION(STATUS, uxPipeDrv, (void));
FUNCTION(STATUS, pipe, (int*));
FUNCTION(void, uxPipeShow, (int));

void erl_main(int argc, char **argv);
void argcall(char *args);

/* Malloc-realted functions called from the VxWorks shell */
EXTERN_FUNCTION(int, erl_set_memory_block, 
		(int, int, int, int, int, int, int, int, int, int));
EXTERN_FUNCTION(int, erl_memory_show, 
		(int, int, int, int, int, int, int, int, int, int));

#define DEFAULT_PORT_STACK_SIZE  100000 
static int port_stack_size;

static int erlang_id = 0;	/* Inited at loading, set/reset at each run */

/* interval time reported to emulator */
static int sys_itime;

/* XXX - This is defined in .../config/all/configAll.h (NUM_FILES),
   and not easily accessible at compile or run time - however,
   in VxWorks 5.1 it is stored in the (undocumented?) maxFiles variable;
   probably shouldn't depend on it, but we try to pick it up... */
static int max_files = 50;	/* default configAll.h */

int erts_vxworks_max_files;

/* 
 * used by the break handler (set by signal handler on ctl-c)
 */
volatile int erts_break_requested;

/********************* General functions ****************************/

/*
 * Reset the terminal to the original settings on exit
 * (nothing to do for WxWorks).
 */
void sys_tty_reset(int exit_code)
{
}

Uint
erts_sys_misc_mem_sz(void)
{
    Uint res = erts_check_io_size();
    /* res += FIXME */
    return res;
}

/*
 * XXX This declaration should not be here.
 */
void erl_sys_schedule_loop(void);

#ifdef SOFTDEBUG
static void do_trace(int line, char *file, char *format, ...)
{
    va_list va;
    int tid = taskIdSelf();
    char buff[512];

    va_start(va, format);
    sprintf(buff,"Trace: Task: 0x%08x, %s:%d - ",
	    tid, file, line);
    vsprintf(buff + strlen(buff), format, va);
    va_end(va);
    strcat(buff,"\r\n");
    write(2,buff,strlen(buff));
}

#define TRACE() do_trace(__LINE__, __FILE__,"")
#define TRACEF(Args...) do_trace(__LINE__,__FILE__, ## Args)
#endif

void
erts_sys_pre_init(void)
{
    if (erlang_id != 0) {
	/* NOTE: This particular case must *not* call erl_exit() */
	erts_fprintf(stderr, "Sorry, erlang is already running (as task %d)\n",
		     erlang_id);
	exit(1);
    }

    /* This must be done as early as possible... */
    if(!reclaim_init()) 
	fprintf(stderr, "Warning : reclaim facility should be initiated before "
		"erlang is started!\n");
    erts_vxworks_max_files = max_files = reclaim_max_files();

    /* Floating point exceptions */
#if (CPU == SPARC)
    sys_sigset(SIGFPE, fpe_sig_handler);
#elif (CPU == PPC603)
    fix_registers();
#endif

    /* register the private delete hook in reclaim */
    save_delete_hook((FUNCPTR)delete_hook, (caddr_t)0);
    erlang_id = taskIdSelf();
#ifdef DEBUG
    printf("emulator task id = 0x%x\n", erlang_id);
#endif
}

void erts_sys_alloc_init(void)
{
    initialize_allocation();
}

void
erl_sys_init(void)
{
    setvbuf(stdout, (char *)NULL, _IOLBF, BUFSIZ);
    /* XXX Bug in VxWorks stdio loses fputch()'ed output after the
       setvbuf() but before a *printf(), and possibly worse (malloc
       errors, crash?) - so let's give it a *printf().... */
    fprintf(stdout, "%s","");
}

void
erl_sys_args(int* argc, char** argv)
{
    erts_init_check_io();
    max_files = erts_check_io_max_files();
    ASSERT(max_files <= erts_vxworks_max_files);
}

/*
 * Called from schedule() when it runs out of runnable processes,
 * or when Erlang code has performed INPUT_REDUCTIONS reduction
 * steps. runnable == 0 iff there are no runnable Erlang processes.
 */
void
erl_sys_schedule(int runnable)
{	
    erts_check_io_interrupt(0);
    erts_check_io(!runnable);
}

void erts_do_break_handling(void)
{
    SET_BLOCKING(0);
    /* call the break handling function, reset the flag */
    do_break();
    erts_break_requested = 0;
    SET_NONBLOCKING(0);
}

/* signal handling */
RETSIGTYPE (*sys_sigset(sig, func))()
     int sig;
     RETSIGTYPE (*func)();
{
  struct sigaction act, oact;

  sigemptyset(&act.sa_mask);
  act.sa_flags = 0;
  act.sa_handler = func;
  sigaction(sig, &act, &oact);
  return(oact.sa_handler);
}

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

void
erts_sys_prepare_crash_dump(void)
{
    
}

/* register signal handlers XXX - they don't work, need to find out why... */
/* set up signal handlers for break and quit */
static void request_break(void)
{
  /* just set a flag - checked for and handled 
   * in main thread (not signal handler).
   * see check_io() 
   */
#ifdef DEBUG
  fprintf(stderr,"break!\n");
#endif
  erts_break_requested = 1;
  erts_check_io_interrupt(1); /* Make sure we don't sleep in erts_poll_wait */
}

static void do_quit(void)
{
    halt_0(0);
}

void erts_set_ignore_break(void) {
}

void init_break_handler(void)
{
  sys_sigset(SIGINT, request_break);
  sys_sigset(SIGQUIT, do_quit);
}

void erts_replace_intr(void) {
}

int sys_max_files(void) 
{
  return(max_files);
}

/******************* Routines for time measurement *********************/

int sys_init_time(void) 
{
    erts_clock_rate =  sysClkRateGet();
    /*
    ** One could imagine that it would be better returning 
    ** a resolution more near the clock rate, like in:
    ** return 1000 / erts_clock_rate; 
    ** but tests show that such isn't the case (rounding errors?)
    ** Well, we go for the Unix variant of returning 1 
    ** as a constant virtual clock rate.
    */
    return SYS_CLOCK_RESOLUTION;
}

int erts_clock_rate;
static volatile int ticks_inuse;
static volatile unsigned long ticks_collected; /* will wrap */
static WDOG_ID watchdog_id;
static ULONG user_time;
static int this_task_id, sys_itime;
static SysHrTime hrtime_wrap;
static unsigned long last_tick_count;

static void tolerant_time_clockint(int count)
{
  if (watchdog_id != NULL) {
    if (taskIsReady(this_task_id))
      user_time += 1;
    ++count;
    if (!ticks_inuse) {
      ticks_collected += count;
      count = 0;
    }
    wdStart(watchdog_id, 1, (FUNCPTR)tolerant_time_clockint, count);
  }
}

int sys_init_hrtime(void)
{
    this_task_id = taskIdSelf(); /* OK, this only works for one single task
				    in the system... */
    user_time = 0;

    ticks_inuse = 0;
    ticks_collected = 0;
    hrtime_wrap = 0;
    last_tick_count = 0;

    sys_itime = 1000 / erts_clock_rate;
    watchdog_id = wdCreate();
    wdStart(watchdog_id, 1, (FUNCPTR) tolerant_time_clockint, 0);
    return 0;
}

SysHrTime sys_gethrtime(void) 
{
    SysHrTime ticks;

    ++ticks_inuse;
    ticks = (SysHrTime) (ticks_collected & 0x7FFFFFFF);
    ticks_inuse = 0;
    if (ticks < (SysHrTime) last_tick_count) {
	hrtime_wrap += 1UL << 31;
    }
    last_tick_count = ticks;
    return (ticks + hrtime_wrap) * ((SysHrTime) (1000000000UL / 
						 erts_clock_rate));
}
    
void sys_gettimeofday(SysTimeval *tvp)
{
    struct timespec now;
    
    clock_gettime(CLOCK_REALTIME, &now);
    tvp->tv_sec = now.tv_sec;
    tvp->tv_usec = now.tv_nsec / 1000;
}

clock_t sys_times(SysTimes *t)
{
    t->tms_stime = t->tms_cutime = t->tms_cstime = 0;
    ++ticks_inuse;
    t->tms_utime = user_time;
    ticks_inuse = 0;
    return tickGet(); /* The best we can do... */
}

/* This is called when *this task* is deleted */
static void delete_hook(void)
{
  if (watchdog_id != NULL) {
    wdDelete(watchdog_id);
    watchdog_id = NULL;
  }
  erlang_id = 0;
  this_task_id = 0;
}

/************************** OS info *******************************/

/* Used by erlang:info/1. */
/* (This code was formerly in drv.XXX/XXX_os_drv.c) */

#define MAX_VER_STR 9           /* Number of characters to
                                   consider in version string */                              

static FUNCTION(int, get_number, (char** str_ptr));

char os_type[] = "vxworks";

static int
get_number(char **str_ptr)
{
    char* s = *str_ptr;		/* Pointer to beginning of string. */
    char* dot;			/* Pointer to dot in string or NULL. */

    if (!isdigit(*s))
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

/* namebuf;     Where to return the name. */
/* size;	Size of name buffer. */
void
os_flavor(char *namebuf, unsigned size)
{
    strcpy(namebuf, "-");
}

/* int* pMajor;			Pointer to major version. */
/* int* pMinor;			Pointer to minor version. */
/* int* pBuild;			Pointer to build number. */
void
os_version(int *pMajor, int *pMinor, int *pBuild)
{
    char os_ver[MAX_VER_STR+2];
    char* release;		/* Pointer to the release string:
				 * X.Y or X.Y.Z.
				 */
    strncpy(os_ver, vxWorksVersion, MAX_VER_STR);
    release = os_ver;
    *pMajor = get_number(&release);
    *pMinor = get_number(&release);
    *pBuild = get_number(&release);
}

void init_getenv_state(GETENV_STATE *state)
{
   *state = NULL;
}

char *getenv_string(GETENV_STATE *state0)
{
   return NULL;
}

void fini_getenv_state(GETENV_STATE *state)
{
   *state = NULL;
}

/************************** Port I/O *******************************/


/* I. Common stuff */

#define TMP_BUF_MAX (tmp_buf_size - 1024)
static byte *tmp_buf;
static Uint tmp_buf_size;

/* II. The spawn/fd/vanilla drivers */

/* This data is shared by these drivers - initialized by spawn_init() */
static struct driver_data {
    int port_num, ofd, packet_bytes, report_exit;
    int exitcode, exit_reported; /* For returning of exit codes. */
} *driver_data;			/* indexed by fd */

/* 
 * Locking only for exitcodes and exit_reported, one global sem for all
 * spawn ports as this is rare.
 */
static SEM_ID driver_data_sem = NULL;
/*
 * Also locking when looking up entries in the load table
 */
static SEM_ID entry_data_sem = NULL;

/* We maintain a linked fifo queue of these structs in order */
/* to manage unfinnished reads/and writes on differenet fd's */

typedef struct pend {
    char *cpos;
    int fd;
    int remain;
    struct pend *next;
    char buf[1];   /* this is a trick to be able to malloc one chunk */
} Pend;

static struct fd_data {
    int inport, outport;
    char *buf, *cpos;
    int sz, remain;  /* for input on fd */
    Pend* pending;   /* pending outputs */

} *fd_data;			/* indexed by fd */
    

/* Driver interfaces */
static ErlDrvData spawn_start(ErlDrvPort port_num, char *name, SysDriverOpts* opts);
static ErlDrvData fd_start(ErlDrvPort port_num, char *name, SysDriverOpts* opts);
static ErlDrvData vanilla_start(ErlDrvPort port_num, char *name, SysDriverOpts* opts);
static int spawn_init(void);
static void fd_stop(ErlDrvData);
static void stop(ErlDrvData);
static void ready_input(ErlDrvData fd, ErlDrvEvent ready_fd);
static void ready_output(ErlDrvData fd, ErlDrvEvent ready_fd);
static void output(ErlDrvData fd, char *buf, int len);
static void stop_select(ErlDrvEvent, void*);

struct erl_drv_entry spawn_driver_entry = {
    spawn_init,
    spawn_start,
    stop,
    output,
    ready_input,
    ready_output,
    "spawn",
    NULL, /* finish */
    NULL, /* handle */
    NULL, /* control */
    NULL, /* timeout */
    NULL, /* outputv */
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
struct erl_drv_entry fd_driver_entry = {
    NULL,
    fd_start,
    fd_stop,
    output,
    ready_input,
    ready_output,
    "fd",
    NULL, /* finish */
    NULL, /* handle */
    NULL, /* control */
    NULL, /* timeout */
    NULL, /* outputv */
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
    NULL, /* finish */
    NULL, /* handle */
    NULL, /* control */
    NULL, /* timeout */
    NULL, /* outputv */
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

/*
** Set up enough of the driver_data structure to be able to report exit status.
** Some things may be initiated again, but that is no real problem.
*/
static int pre_set_driver_data(int ifd, int ofd, 
			       int read_write, int report_exit) {
  if (read_write & DO_READ) {
    driver_data[ifd].report_exit = report_exit;
    driver_data[ifd].exitcode = 0;
    driver_data[ifd].exit_reported = 0;
    if (read_write & DO_WRITE) {
      driver_data[ifd].ofd = ofd;
      if (ifd != ofd) {
	driver_data[ofd] = driver_data[ifd];
	driver_data[ofd].report_exit = 0;
      }
    } else {			/* DO_READ only */
      driver_data[ifd].ofd = -1;
    }
    return(ifd);
  } else {			/* DO_WRITE only */
    driver_data[ofd].report_exit = 0;
    driver_data[ofd].exitcode = 0;
    driver_data[ofd].exit_reported = 0;
    driver_data[ofd].ofd = ofd;
    return(ofd);
  }
}
    
/*
** Set up the driver_data structure, it may have been initiated
** partly by the function above, but we dont care. 
*/
static int set_driver_data(int port_num, int ifd, int ofd, 
			   int packet_bytes, int read_write,
			   int report_exit)
{
  if (read_write & DO_READ) {
    driver_data[ifd].packet_bytes = packet_bytes;
    driver_data[ifd].port_num = port_num;
    driver_data[ifd].report_exit = report_exit;
    if (read_write & DO_WRITE) {
      driver_data[ifd].ofd = ofd;
      if (ifd != ofd) {
	driver_data[ofd] = driver_data[ifd];
	driver_data[ofd].report_exit = 0;
      }
    } else {			/* DO_READ only */
      driver_data[ifd].ofd = -1;
    }
    (void) driver_select(port_num, ifd, ERL_DRV_READ|ERL_DRV_USE, 1);
    return(ifd);
  } else {			/* DO_WRITE only */
    driver_data[ofd].packet_bytes = packet_bytes;
    driver_data[ofd].port_num = port_num;
    driver_data[ofd].report_exit = 0;
    driver_data[ofd].ofd = ofd;
    return(ofd);
  }
}

static int need_new_sems = 1;

static int spawn_init(void)
{
  char *stackenv;
  int size;
  driver_data = (struct driver_data *)
      erts_alloc(ERTS_ALC_T_DRV_TAB, max_files * sizeof(struct driver_data));
  if (need_new_sems) {
      driver_data_sem = semMCreate
	  (SEM_Q_PRIORITY | SEM_DELETE_SAFE | SEM_INVERSION_SAFE);
      entry_data_sem = semMCreate
	  (SEM_Q_PRIORITY | SEM_DELETE_SAFE | SEM_INVERSION_SAFE);
  }
  if (driver_data_sem == NULL || entry_data_sem == NULL) {
      erl_exit(1,"Could not allocate driver locking semaphore.");
  }
  need_new_sems = 0;

  (void)uxPipeDrv();		/* Install pipe driver */

  if ((stackenv = getenv("ERLPORTSTACKSIZE")) != NULL &&
      (size = atoi(stackenv)) > 0)
    port_stack_size = size;
  else
    port_stack_size = DEFAULT_PORT_STACK_SIZE;
  return 0;
}

/* Argv has to be built vith the save_xxx routines, not with whathever 
   sys_xxx2 has in mind... */
#define argv_alloc save_malloc
#define argv_realloc save_realloc
#define argv_free save_free
/* Build argv, return argc or -1 on failure */
static int build_argv(char *name, char ***argvp)
{
  int argvsize = 10, argc = 0;
  char *args, *arglast = NULL, *argp;
  char **argv;

#ifdef DEBUG
  fdprintf(2, "Building argv, %s =>\n", name);
#endif
  if ((argv = (char **)argv_alloc(argvsize * sizeof(char *))) == NULL)
    return(-1);
  if ((args = argv_alloc(strlen(name) + 1)) == NULL)
    return(-1);
  strcpy(args, name);
  argp = strtok_r(args, " \t", &arglast);
  while (argp != NULL) {
    if (argc + 1 >= argvsize) {
      argvsize += 10;
      argv = (char **)argv_realloc((char *)argv, argvsize*sizeof(char *));
      if (argv == NULL) {
	argv_free(args);
	return(-1);
      }
    }
#ifdef DEBUG
  fdprintf(2, "%s\n", argp);
#endif
    argv[argc++] = argp;
    argp = strtok_r((char *)NULL, " \t", &arglast);
  }
  argv[argc] = NULL;
  *argvp = argv;
  return(argc);
}
#undef argv_alloc
#undef argv_realloc
#undef argv_free


/* Lookup and return global text symbol or NULL on failure
   Symbol name is null-terminated and without the leading '_' */
static FUNCPTR
lookup(char *sym)
{
  char buf[256];
  char *symname = buf;
  int len;
  FUNCPTR entry;
  SYM_TYPE type;

  len = strlen(sym);
  if (len > 254 && (symname = malloc(len+2)) == NULL)
    return(NULL);
#if defined _ARCH_PPC || defined SIMSPARCSOLARIS
  /* GCC for PPC and SIMSPARC doesn't add a leading _ to symbols */
  strcpy(symname, sym);
#else
  sprintf(symname, "_%s", sym);
#endif
  if (symFindByNameAndType(sysSymTbl, symname, (char **)&entry,
			   &type, N_EXT | N_TEXT, N_EXT | N_TEXT) != OK)
    entry = NULL;
  if (symname != buf)
    free(symname);
  return(entry);
}

/* This function is spawned to build argc, argv, lookup the symbol to call,
   connect and set up file descriptors, and make the actual call.
   N.B. 'name' was allocated by the Erlang task (through plain_malloc) and
   is freed by this port program task.
   Note: 'name' may be a path containing '/'. */

static void call_proc(char *name, int ifd, int ofd, int read_write, 
		      int redir_stderr, int driver_index, 
		      int p6, int p7, int p8, int p9)
{
  int argc;
  char **argv, *bname;
  FUNCPTR entry;
  int ret = -1;

  /* Must consume 'name' */
  argc = build_argv(name, &argv);
  plain_free(name);
  /* Find basename of path */
  if ((bname = strrchr(argv[0], '/')) != NULL) {
    bname++;
  } else {
    bname = argv[0];
  }
#ifdef DEBUG
  fdprintf(2, "Port program name: %s\n", bname);
#endif
  semTake(entry_data_sem, WAIT_FOREVER);

  if (argc > 0) {
    if ((entry = lookup(bname)) == NULL) {
      int fd;
      char *fn;
      /* NOTE: We don't check the return value of loadModule,
	 since that was incompatibly changed from 5.0.2b to 5.1,
	 but rather do a repeated lookup().   */
      if ((fd = open(argv[0], O_RDONLY)) > 0) {
	(void) loadModule(fd, GLOBAL_SYMBOLS);
	close(fd);
	entry = lookup(bname);
      }
      if (entry == NULL) {
	/* filename == func failed, try func.o */
	if ((fn = malloc(strlen(argv[0]) + 3)) != NULL) { /* ".o\0" */
	  strcpy(fn, argv[0]);
	  strcat(fn, ".o");
	  if ((fd = open(fn, O_RDONLY)) > 0) {
	    (void) loadModule(fd, GLOBAL_SYMBOLS);
	    close(fd);
	    entry = lookup(bname);
	  }
	  free(fn);
	}
      }
    }
  } else {
    entry = NULL;
  }
  semGive(entry_data_sem);
    
  if (read_write & DO_READ) {	/* emulator read */
    save_fd(ofd);
    ioTaskStdSet(0, 1, ofd);	/* stdout for process */
    if(redir_stderr)
	ioTaskStdSet(0, 2, ofd);/* stderr for process */
  }
  if (read_write & DO_WRITE) { /* emulator write */
    save_fd(ifd);
    ioTaskStdSet(0, 0, ifd);	/* stdin for process */
  }
  if (entry != NULL) {
    ret = (*entry)(argc, argv, (char **)NULL); /* NULL for envp */
  } else {
    fdprintf(2, "Could not exec \"%s\"\n", argv[0]);
    ret = -1;
  }
  if (driver_data[driver_index].report_exit) {
      semTake(driver_data_sem, WAIT_FOREVER);
      driver_data[driver_index].exitcode = ret;
      driver_data[driver_index].exit_reported = 1;
      semGive(driver_data_sem);
  }
  /* We *don't* want to close the pipes here, but let the delete
     hook take care of it - it might want to flush stdout and there'd
     better be an open descriptor to flush to...   */
  exit(ret);
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

static void init_fd_data(int fd, int port_unused_argument)
{
  SET_NONBLOCKING(fd);
  fd_data[fd].pending = NULL;
  fd_data[fd].buf = fd_data[fd].cpos = NULL;
  fd_data[fd].remain = fd_data[fd].sz = 0;
}

static ErlDrvData spawn_start(ErlDrvPort port_num, char *name,SysDriverOpts* opts)
{
    int ifd[2], ofd[2], len, nl, id;
    char taskname[11], *progname, *bname;
    char *space_in_command;
    int packet_bytes = opts->packet_bytes;
    int read_write = opts->read_write;
    int use_stdio = opts->use_stdio;
    int redir_stderr = opts->redir_stderr;
    int driver_index;

    if (!use_stdio){
	return (ErlDrvData) -3;
  }

    /* Create pipes and set the Erlang task as owner of its 
     * read and write ends (through save_fd()).
     */
    switch (read_write) {
    case DO_READ:
	if (pipe(ifd) < 0){
	    return (ErlDrvData) -2;
	}
	if (ifd[0] >= max_files) {
	    close_pipes(ifd, ofd, read_write);
	    errno = ENFILE;
	    return (ErlDrvData) -2;
	}
	save_fd(ifd[0]);
	break;
    case DO_WRITE:
	if (pipe(ofd) < 0) {
	    return (ErlDrvData) -2;
	}
	if (ofd[1] >= max_files) {
	    close_pipes(ifd, ofd, read_write);
	    errno = ENFILE;
	    return (ErlDrvData) -2;
	}
	save_fd(ofd[1]);
	break;
    case DO_READ|DO_WRITE:
	if (pipe(ifd) < 0){
	    return (ErlDrvData) -2;
	}
	if (ifd[0] >= max_files || pipe(ofd) < 0) {
	    close_pipes(ifd, ofd, DO_READ);
	    errno = ENFILE;
	    return (ErlDrvData) -2;
	}
	if (ofd[1] >= max_files) {
	    close_pipes(ifd, ofd, read_write);
	    errno = ENFILE;
	    return (ErlDrvData) -2;
	}
	save_fd(ifd[0]);
	save_fd(ofd[1]);
	break;
    default:
	return (ErlDrvData) -1;
    }

    /* Allocate space for program name to be freed by the
     * spawned task. We use plain_malloc so that the allocated
     * space is not owned by the Erlang task.
     */

    if ((progname = plain_malloc(strlen(name) + 1)) == NULL) {
	close_pipes(ifd, ofd, read_write);
	errno = ENOMEM;
	return (ErlDrvData) -2;
    }
    strcpy(progname, name);

    /* Check if name contains a space 
     *  (e.g "port_test -o/home/gandalf/tornado/wind/target/erlang")
     */
    if ((space_in_command = strrchr(progname, ' ')) != NULL) {
	*space_in_command = '\0';
    }

    /* resulting in "port_test" */
    if ((bname = strrchr(progname, '/')) != NULL)
	bname++;
    else
	bname = progname;

    /* resulting in "port_test" */
    len = strlen(bname);
    nl = len > 10 ? 10 : len;
    strncpy(taskname, bname, nl);
    taskname[nl] = '\0';
    if (space_in_command != NULL) 
	*space_in_command = ' ';
    driver_index = pre_set_driver_data(ifd[0], ofd[1], 
				       read_write, opts->exit_status);

  /* resetting to "port_test -o/home/gandalf/tornado/wind/target/erlang" */
    if ((id = taskSpawn(taskname, spTaskPriority, spTaskOptions,
			port_stack_size, (FUNCPTR)call_proc, (int)progname,
			ofd[0], ifd[1], read_write, redir_stderr, driver_index,
			0,0,0,0))
	== ERROR) {
	close_pipes(ifd, ofd, read_write);
	plain_free(progname);	/* only when spawn fails */
	errno = ENOMEM;
	return (ErlDrvData) -2;
    }    
#ifdef DEBUG
    fdprintf(2, "Spawned %s as %s[0x%x]\n", name, taskname, id);
#endif
    if (read_write & DO_READ) 
	init_fd_data(ifd[0], port_num);
    if (read_write & DO_WRITE) 
	init_fd_data(ofd[1], port_num);
    return (ErlDrvData) (set_driver_data(port_num, ifd[0], ofd[1],
					 packet_bytes,read_write,
					 opts->exit_status));
}

static ErlDrvData fd_start(ErlDrvPort port_num, char *name, SysDriverOpts* opts)
{
    if (((opts->read_write & DO_READ) && opts->ifd >= max_files) ||
	((opts->read_write & DO_WRITE) && opts->ofd >= max_files)) {
	return (ErlDrvData) -1;
    }

    if (opts->read_write & DO_READ) 
	init_fd_data(opts->ifd, port_num);
    if (opts->read_write & DO_WRITE) 
	init_fd_data(opts->ofd, port_num);
    return (ErlDrvData) (set_driver_data(port_num, opts->ifd, opts->ofd,
					 opts->packet_bytes, opts->read_write, 0));
}

static void clear_fd_data(int fd) 
{
    
  if (fd_data[fd].sz > 0) 
    erts_free(ERTS_ALC_T_FD_ENTRY_BUF, (void *) fd_data[fd].buf);
  fd_data[fd].buf = NULL;
  fd_data[fd].sz = 0;
  fd_data[fd].remain = 0;
  fd_data[fd].cpos = NULL;
}

static void nbio_stop_fd(int port_num, int fd)
{
  Pend *p, *p1;
    
  driver_select(port_num, fd, ERL_DRV_READ|ERL_DRV_WRITE, 0);
  clear_fd_data(fd);
  p = fd_data[fd].pending;
  SET_BLOCKING(fd);
  while (p) {
    p1 = p->next;
    free(p);
    p = p1;
  }
  fd_data[fd].pending = NULL;
}

static void fd_stop(ErlDrvData drv_data)
{
    int ofd;
    int fd = (int) drv_data;

    nbio_stop_fd(driver_data[fd].port_num, (int)fd);
    ofd = driver_data[fd].ofd;
    if (ofd != fd && ofd != -1) 
	nbio_stop_fd(driver_data[fd].port_num, (int)ofd); /* XXX fd = ofd? */
}

static ErlDrvData
vanilla_start(ErlDrvPort port_num, char *name, SysDriverOpts* opts)
{
    int flags, fd;
    struct stat statbuf;

    DEBUGF(("vanilla_start, name: %s [r=%1i w=%1i]\n", name, 
	    opts->read_write & DO_READ, 
	    opts->read_write & DO_WRITE));
  
    flags = (opts->read_write == DO_READ ? O_RDONLY :
	     opts->read_write == DO_WRITE ? O_WRONLY|O_CREAT|O_TRUNC :
	     O_RDWR|O_CREAT);
    if ((fd = open(name, flags, 0666)) < 0){
	errno = ENFILE;
	return (ErlDrvData) -2;
    }
    if (fd >= max_files) {
	close(fd);
	errno = ENFILE;
	return (ErlDrvData) -2;
    }
    if (fstat(fd, &statbuf) < 0) {
	close(fd);
	errno = ENFILE;
	return (ErlDrvData) -2;
    }

    /* Return error for reading regular files (doesn't work) */
    if (ISREG(statbuf) && ((opts->read_write) & DO_READ)) {
	close(fd);
	return (ErlDrvData) -3;
    }
    init_fd_data(fd, port_num);
    return (ErlDrvData) (set_driver_data(port_num, fd, fd,
					 opts->packet_bytes, opts->read_write, 0));
}

/* Note that driver_data[fd].ifd == fd if the port was opened for reading, */
/* otherwise (i.e. write only) driver_data[fd].ofd = fd.  */

static void stop(ErlDrvData drv_data)
{
    int port_num, ofd;
    int fd = (int) drv_data;

    port_num = driver_data[fd].port_num;
    nbio_stop_fd(port_num, fd);
    driver_select(port_num, fd, ERL_DRV_USE, 0); /* close(fd) */

    ofd = driver_data[fd].ofd;
    if (ofd != fd && ofd != -1) {
	nbio_stop_fd(port_num, ofd);
	driver_select(port_num, ofd, ERL_DRV_USE, 0); /* close(fd) */
    }
}

static int sched_write(int port_num,int fd, char *buf, int len, int pb)
{
  Pend *p, *p2, *p3;
  int p_bytes = len;

  p = (Pend*) erts_alloc_fnf(ERTS_ALC_T_PEND_DATA, pb + len + sizeof(Pend));
  if (!p) {
    driver_failure(port_num, -1);
    return(-1);
  }

  switch(pb) {
  case 4: put_int32(len, p->buf); break;
  case 2: put_int16(len, p->buf); break;
  case 1: put_int8(len, p->buf); break;
  case 0: break;		/* Handles this case too */
  }
  sys_memcpy(p->buf + pb, buf, len);
  driver_select(port_num, fd, ERL_DRV_WRITE|ERL_DRV_USE, 1);
  p->cpos = p->buf;
  p->fd = fd;
  p->next = NULL;
  p->remain = len + pb;
  p2 = fd_data[fd].pending;
  if (p2 == NULL) 
    fd_data[fd].pending = p;
  else {
    p3 = p2->next;
    while(p3) {
      p_bytes += p2->remain;
      p2 = p2->next;
      p3 = p3->next;
    }
    p2->next = p;
  }
  if (p_bytes > (1 << 13))	/* More than 8 k pending */
    set_busy_port(port_num, 1);
  return(0);
}

/* Fd is the value returned as drv_data by the start func */
static void output(ErlDrvData drv_data, char *buf, int len)
{
    int buf_done, port_num, wval, pb, ofd;
    byte lb[4];
    struct iovec iv[2];
    int fd = (int) drv_data;

    pb = driver_data[fd].packet_bytes;
    port_num = driver_data[fd].port_num;

    if ((ofd = driver_data[fd].ofd) == -1) {
	return;
    }

    if (fd_data[ofd].pending) {
	sched_write(port_num, ofd, buf, len, pb);
	return;
    }
    
    if ((pb == 2 && len > 65535) || (pb == 1 && len > 255)) {
	driver_failure_posix(port_num, EINVAL);
	return;
    }
    if (pb == 0) {
	wval = write(ofd, buf, len);
    } else {
	lb[0] = (len >> 24) & 255;	/* MSB */
	lb[1] = (len >> 16) & 255;
	lb[2] = (len >> 8) & 255;
	lb[3] = len & 255;		/* LSB */
	iv[0].iov_base = (char*) lb + (4 - pb);
	iv[0].iov_len = pb;
	iv[1].iov_base = buf;
	iv[1].iov_len = len;
	wval = writev(ofd, iv, 2);
    }
    if (wval == pb + len ) {
	return;
    }
    if (wval < 0) {
	if ((errno == EINTR) || (errno == ERRNO_BLOCK)) {
	    if (pb) {
		sched_write(port_num, ofd, buf ,len, pb);
	    } else if (pb == 0) {
		sched_write(port_num, ofd, buf ,len, 0);
	    }
	    return;
	}
	driver_failure_posix(driver_data[fd].port_num, EINVAL);
	return;
    }
    if (wval < pb) {
	sched_write(port_num, ofd, (lb +4 -pb)  + wval, pb-wval, 0);
	sched_write(port_num, ofd, buf ,len, 0);
	return;
    }

    /* we now know that  wval < (pb + len) */
    buf_done = wval - pb;
    sched_write(port_num, ofd, buf + buf_done, len - buf_done,0);
}

static void stop_select(ErlDrvEvent fd, void* _)
{
    close((int)fd);
}

static int ensure_header(int fd,char *buf,int packet_size, int sofar)
{
  int res = 0;
  int remaining = packet_size - sofar;

  SET_BLOCKING(fd);
  if (read_fill(fd, buf+sofar, remaining) != remaining)
    return -1;
  switch (packet_size) {
  case 1: res = get_int8(buf); break;
  case 2: res = get_int16(buf); break;
  case 4: res = get_int32(buf); break;
  }
  SET_NONBLOCKING(fd);
  return(res);
}

static int port_inp_failure(int port_num, int ready_fd, int res)
{
    (void) driver_select(port_num, ready_fd, ERL_DRV_READ|ERL_DRV_WRITE, 0); 
    clear_fd_data(ready_fd);
    if (res == 0) {
	if (driver_data[ready_fd].report_exit) {
	    int tmpexit = 0;
	    int reported;
	    /* Lock the driver_data structure */
	    semTake(driver_data_sem, WAIT_FOREVER);
	    if ((reported = driver_data[ready_fd].exit_reported))
		tmpexit = driver_data[ready_fd].exitcode;
	    semGive(driver_data_sem);
	    if (reported) {
		erts_fprintf(stderr,"Exitcode %d reported\r\n", tmpexit);
		driver_report_exit(port_num, tmpexit);
	    }
	}
	driver_failure_eof(port_num);
    } else {
	driver_failure(port_num, res);
    }
    return 0;
}

/* fd is the drv_data that is returned from the */
/* initial start routine                        */
/* ready_fd is the descriptor that is ready to read */

static void ready_input(ErlDrvData drv_data, ErlDrvEvent drv_event)
{
  int port_num, packet_bytes, res;
  Uint h = 0;
  char *buf;
  int fd = (int) drv_data;
  int ready_fd = (int) drv_event;

  port_num = driver_data[fd].port_num;
  packet_bytes = driver_data[fd].packet_bytes;

  if (packet_bytes == 0) {
    if ((res = read(ready_fd, tmp_buf, tmp_buf_size)) > 0) {
      driver_output(port_num, (char*)tmp_buf, res);
      return;
    }
    port_inp_failure(port_num, ready_fd, res);
    return;
  }

  if (fd_data[ready_fd].remain > 0) { /* We try to read the remainder */
    /* space is allocated in buf */
    res = read(ready_fd, fd_data[ready_fd].cpos, 
	       fd_data[ready_fd].remain);
    if (res < 0) {
      if ((errno == EINTR) || (errno == ERRNO_BLOCK)) {
	  ;
      } else {
	  port_inp_failure(port_num, ready_fd, res);
      }
    } else if (res == 0) { 
	port_inp_failure(port_num, ready_fd, res);
    } else if (res == fd_data[ready_fd].remain) { /* we're done  */
	driver_output(port_num, fd_data[ready_fd].buf, 
		      fd_data[ready_fd].sz);
	clear_fd_data(ready_fd);
    } else {			/*  if (res < fd_data[ready_fd].remain) */
	fd_data[ready_fd].cpos += res;
	fd_data[ready_fd].remain -= res;
    }
    return;
  }


  if (fd_data[ready_fd].remain == 0) { /* clean fd */
    /* We make one read attempt and see what happens */
    res = read(ready_fd, tmp_buf, tmp_buf_size);
    if (res < 0) {  
      if ((errno == EINTR) || (errno == ERRNO_BLOCK))
	return;
      port_inp_failure(port_num, ready_fd, res);
      return;
    }
    else if (res == 0) {		/* eof */
      port_inp_failure(port_num, ready_fd, res);
      return;
    }
    else if (res < packet_bytes) { /* Ugly case... get at least */
      if ((h = ensure_header(ready_fd, tmp_buf, packet_bytes, res))==-1) {
	port_inp_failure(port_num, ready_fd, -1);
	return;
      }
      buf = erts_alloc_fnf(ERTS_ALC_T_FD_ENTRY_BUF, h);
      if (!buf) {
	port_inp_failure(port_num, ready_fd, -1);
	return;
      }
      fd_data[ready_fd].buf = buf;
      fd_data[ready_fd].sz = h;
      fd_data[ready_fd].remain = h;
      fd_data[ready_fd].cpos = buf;
      return;
    }
    else  {			/* if (res >= packet_bytes) */
      unsigned char* cpos = tmp_buf;
      int bytes_left = res;
      while (1) {		/* driver_output as many as possible */
	  if (bytes_left == 0) {
	      clear_fd_data(ready_fd);
	      return;
	  }
	  if (bytes_left < packet_bytes) { /* Yet an ugly case */
	      if((h=ensure_header(ready_fd, cpos, 
				  packet_bytes, bytes_left))==-1) {
		  port_inp_failure(port_num, ready_fd, -1);
		  return;
	      }
	      buf = erts_alloc_fnf(ERTS_ALC_T_FD_ENTRY_BUF, h);
	      if (!buf) 
		  port_inp_failure(port_num, ready_fd, -1);
	      fd_data[ready_fd].buf = buf;
	      fd_data[ready_fd].sz = h;
	      fd_data[ready_fd].remain = h;
	      fd_data[ready_fd].cpos = buf;
	      return;
	  }
	  switch (packet_bytes) {
	  case 1: h = get_int8(cpos); cpos += 1; break;
	  case 2: h = get_int16(cpos); cpos += 2; break;
	  case 4: h = get_int32(cpos); cpos += 4; break;
	  }
	  bytes_left -= packet_bytes;
	  /* we've got the header, now check if we've got the data */
	  if (h <= (bytes_left)) {
	      driver_output(port_num, (char*) cpos, h);
	      cpos += h;
	      bytes_left -= h;
	      continue;
	  }
	  else {			/* The last message we got was split */
	      buf = erts_alloc_fnf(ERTS_ALC_T_FD_ENTRY_BUF, h);
	      if (!buf) {
		  port_inp_failure(port_num, ready_fd, -1);
	      }
	      sys_memcpy(buf, cpos, bytes_left);
	      fd_data[ready_fd].buf = buf;
	      fd_data[ready_fd].sz = h;
	      fd_data[ready_fd].remain = h - bytes_left;
	      fd_data[ready_fd].cpos = buf + bytes_left;
	      return;
	  }
      }
      return;
    }
  }
  fprintf(stderr, "remain %d \n", fd_data[ready_fd].remain);
  port_inp_failure(port_num, ready_fd, -1);
}


/* fd is the drv_data that is returned from the */
/* initial start routine                        */
/* ready_fd is the descriptor that is ready to read */

static void ready_output(ErlDrvData drv_data, ErlDrvEvent drv_event)
{
  Pend *p;
  int wval;

  int fd = (int) drv_data;
  int ready_fd = (int) drv_event;

  while(1) {
    if ((p = fd_data[ready_fd].pending) == NULL) {
      driver_select(driver_data[fd].port_num, ready_fd, 
		    ERL_DRV_WRITE, 0);
      return;
    }
    wval = write(p->fd, p->cpos, p->remain);
    if (wval == p->remain) {
      fd_data[ready_fd].pending = p->next;
      erts_free(ERTS_ALC_T_PEND_DATA, p);
      if (fd_data[ready_fd].pending == NULL) {
	driver_select(driver_data[fd].port_num, ready_fd, 
		      ERL_DRV_WRITE, 0);
	set_busy_port(driver_data[fd].port_num, 0);
	return;
      }
      else
	continue;
    }
    else if (wval < 0) {
      if (errno == ERRNO_BLOCK || errno == EINTR)
	return;
      else {
	driver_select(driver_data[fd].port_num, ready_fd, 
		      ERL_DRV_WRITE, 0);
	driver_failure(driver_data[fd].port_num, -1);
	return;
      }
    }
    else if (wval < p->remain) {
      p->cpos += wval;
      p->remain -= wval;
      return;
    }
  }
}

/* Fills in the systems representation of the jam/beam process identifier.
** The Pid is put in STRING representation in the supplied buffer,
** no interpretatione of this should be done by the rest of the
** emulator. The buffer should be at least 21 bytes long.
*/
void sys_get_pid(char *buffer){
    int p = taskIdSelf(); /* Hmm, may be negative??? requires some GB of
			     memory to make the TCB address convert to a
			     negative value. */
    sprintf(buffer,"%d", p);
}

int
erts_sys_putenv(char *buffer, int sep_ix)
{
    return putenv(buffer);
}

int
erts_sys_getenv(char *key, char *value, size_t *size)
{
    char *orig_value;
    int res;
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
    return res;
}

void
sys_init_io(void)
{
  tmp_buf = (byte *) erts_alloc(ERTS_ALC_T_SYS_TMP_BUF, SYS_TMP_BUF_SIZE);
  tmp_buf_size = SYS_TMP_BUF_SIZE;
  fd_data = (struct fd_data *)
      erts_alloc(ERTS_ALC_T_FD_TAB, max_files * sizeof(struct fd_data));
}


/* Fill buffer, return buffer length, 0 for EOF, < 0 for error. */

static int read_fill(int fd, char *buf, int len)
{
  int i, got = 0;
  do {
    if ((i = read(fd, buf+got, len-got)) <= 0) {
      return i;
    }
    got += i;
  } while (got < len);
  return (len);
}	


/************************** Misc... *******************************/

extern const char pre_loaded_code[];
extern char* const pre_loaded[];


/* Float conversion */

int sys_chars_to_double(char *buf, double *fp)
{
  char *s = buf;

  /* The following check is incorporated from the Vee machine */
    
#define ISDIGIT(d) ((d) >= '0' && (d) <= '9')

  /* Robert says that something like this is what he really wanted:
   *
   * 7 == sscanf(Tbuf, "%[+-]%[0-9].%[0-9]%[eE]%[+-]%[0-9]%s", ....);
   * if (*s2 == 0 || *s3 == 0 || *s4 == 0 || *s6 == 0 || *s7)
   *   break;
   */

  /* Scan string to check syntax. */
  if (*s == '+' || *s == '-')
    s++;
	    
  if (!ISDIGIT(*s))		/* Leading digits. */
    return -1;
  while (ISDIGIT(*s)) s++;
  if (*s++ != '.')		/* Decimal part. */
    return -1;
  if (!ISDIGIT(*s))
    return -1;
  while (ISDIGIT(*s)) s++;
  if (*s == 'e' || *s == 'E') {
    /* There is an exponent. */
    s++;
    if (*s == '+' || *s == '-')
      s++;
    if (!ISDIGIT(*s))
      return -1;
    while (ISDIGIT(*s)) s++;
  }
  if (*s)			/* That should be it */
    return -1;
    
  if (sscanf(buf, "%lf", fp) != 1)
    return -1;
  return 0;
}

/* 
 ** Convert a double to ascii format 0.dddde[+|-]ddd
 ** return number of characters converted
 */

int sys_double_to_chars(double fp, char *buf)
{
  (void) sprintf(buf, "%.20e", fp);
  return strlen(buf);
}


/* Floating point exceptions */

#if (CPU == SPARC)
jmp_buf fpe_jmp;

RETSIGTYPE fpe_sig_handler(int sig)
{
  longjmp(fpe_jmp, 1);
}

#elif (CPU == PPC603)
static void fix_registers(void){
    FP_CONTEXT fpcontext;
    fppSave(&fpcontext);
    fpcontext.fpcsr &= ~(_PPC_FPSCR_INIT);
    fppRestore(&fpcontext);
}
#endif


/* Return a pointer to a vector of names of preloaded modules */

Preload* sys_preloaded(void)
{
    return (Preload *) pre_loaded;
}

/* Return a pointer to preloaded code for module "module" */
unsigned char* sys_preload_begin(Preload *pp)
{
    return pp->code;
}

/* Clean up if allocated */
void sys_preload_end(Preload *pp)
{
    /* Nothing */
}

/* Read a key from console (?) */

int sys_get_key(int fd)
{
  int c;
  unsigned char rbuf[64];

  fflush(stdout);		/* Flush query ??? */

  if ((c = read(fd,rbuf,64)) <= 0)
    return c;
  return rbuf[0];
}


/* A real printf that does the equivalent of fprintf(stdout, ...) */

/* ARGSUSED */
static STATUS
stdio_write(char *buf, int nchars, int fp)
{
  if (fwrite(buf, sizeof(char), nchars, (FILE *)fp) == 0)
    return(ERROR);
  return(OK);
}

int real_printf(const char *fmt, ...)
{
  va_list ap;
  int err;
  
  va_start(ap, fmt);
  err = fioFormatV(fmt, ap, stdio_write, (int)stdout);
  va_end(ap);
  return(err);
}
	
	
/* 
 * Little function to do argc, argv calls from (e.g.) VxWorks shell
 *  The arguments should be in the form of a single ""-enclosed string
 *  NOTE: This isn't really part of the emulator, just included here
 *  so we can use the handy functions and memory reclamation.
 */
void argcall(char *args)
{
  int argc;
  char **argv;
  FUNCPTR entry;

  if (args != NULL) {
    if ((argc = build_argv(args, &argv)) > 0) {
      if ((entry = lookup(argv[0])) != NULL)
	(*entry)(argc, argv, (char **)NULL); /* NULL for envp */
      else
	fprintf(stderr, "Couldn't find %s\n", argv[0]);
    } else
      fprintf(stderr, "Failed to build argv!\n");
  } else
    fprintf(stderr, "No argument list!\n");
}


/* That concludes the Erlang stuff - now we just need to implement an OS...
   - Just kidding, but resource reclamation isn't the strength of VxWorks */
#undef calloc
#undef free
#undef cfree
#undef malloc
#undef realloc
#undef open
#undef creat
#undef socket
#undef accept
#undef close
#undef fopen
#undef fdopen
#undef freopen
#undef fclose

/********************* Using elib_malloc ****************************/
/* This gives us yet another level of malloc wrappers. The purpouse */
/* is to be able to select between different varieties of memory    */
/* allocation without recompiling.                                  */
/* Maybe the performance is somewhat degraded by this, but          */
/* on the other hand, performance may be much better if the most    */
/* suiting malloc is used (not to mention the much lower            */
/* fragmentation).                                                  */
/* /Patrik N                                                        */
/********************************************************************/

/* 
 * I don't want to include the whole elib header, especially
 * as it uses char * for generic pointers. Let's fool ANSI C instead.
 */
extern void *elib_malloc(size_t);
extern void *elib_realloc(void *, size_t);
extern void elib_free(void *);
extern void elib_init(void *, int);
extern void elib_force_init(void *, int);
extern size_t elib_sizeof(void *);

/* Flags */
#define USING_ELIB_MALLOC         1   /* We are using the elib_malloc */
#define WARN_MALLOC_MIX           2   /* Warn if plain malloc or save_malloc
					 is mixed with sys_free2 or 
					 sys_realloc2 */
#define REALLOC_MOVES             4   /* Always move on realloc 
					 (less fragmentation) */
#define USER_POOL                 8   /* The user supplied the memory
					 pool, it was not save_alloced. */
#define RECLAIM_USER_POOL        16   /* Use the reclaim mechanism in the
					 user pool. */
#define NEW_USER_POOL            32   /* The user pool is newly suppllied,
					 any old pool should be discarded */


#define ELIB_LOCK \
if(alloc_flags & USING_ELIB_MALLOC) \
    semTake(elib_malloc_sem, WAIT_FOREVER)

#define ELIB_UNLOCK \
if(alloc_flags & USING_ELIB_MALLOC) \
    semGive(elib_malloc_sem)

#define USER_RECLAIM() ((alloc_flags & USING_ELIB_MALLOC) && \
			(alloc_flags & USER_POOL) && \
			(alloc_flags & RECLAIM_USER_POOL))

/*
 * Global state
 * The use of function pointers for the malloc/realloc/free functions
 * is actually only useful in the malloc case, we must know what kind of
 * realloc/free we are going to use, so we could call elib_xxx directly.
 * However, as the overhead is small and this construction makes it 
 * fairly easy to add another malloc algorithm, the function pointers
 * are used in realloc/free to.
 */
static MallocFunction actual_alloc = &save_malloc;
static ReallocFunction actual_realloc = &save_realloc;
static FreeFunction actual_free = &save_free;
static int alloc_flags = 0;
static int alloc_pool_size = 0;
static void *alloc_pool_ptr = NULL;
static SEM_ID elib_malloc_sem = NULL; 

/*
 * Descide if we should use the save_free instead of elib_free or, 
 * in the case of the free used in a delete hook, if we should
 * use plain free instead of elib_free.
 */
static int use_save_free(void *ptr){
    register int diff = ((char *) ptr) - ((char *) alloc_pool_ptr);
    /* 
     * Hmmm... should it be save_free even if diff is exactly 0? 
     * The answer is Yes if the whole area is save_alloced and No if not,
     * so reclaim_free_hook is NOT run in the case of one save_alloced area. 
     */
    return (!(alloc_flags & USING_ELIB_MALLOC) ||
	    (diff < 0 || diff >= alloc_pool_size));
}

/*
 * A free function used by the task deletion hook for the save_xxx functions.
 * Set with the set_reclaim_free_function function.
 */
static void reclaim_free_hook(void *ptr){
    if(use_save_free(ptr)){
	free(ptr);
    } else {
	ELIB_LOCK;
	(*actual_free)(ptr);
	ELIB_UNLOCK;
    }
}
    

/*
 * Initialize, sets the values of pointers based on
 * either nothing (the default) or what's set previously by the
 * erl_set_memory_block function.
 */
static void initialize_allocation(void){
    set_reclaim_free_function(NULL);
    if(alloc_pool_size == 0){
	actual_alloc = (void *(*)(size_t))&save_malloc;
	actual_realloc = (void *(*)(void *, size_t))&save_realloc;
	actual_free = &save_free;
	alloc_flags &= ~(USING_ELIB_MALLOC | USER_POOL | RECLAIM_USER_POOL);
    } else {
	if(elib_malloc_sem == NULL)
	    elib_malloc_sem = semMCreate
		(SEM_Q_PRIORITY | SEM_DELETE_SAFE | SEM_INVERSION_SAFE);
	if(elib_malloc_sem == NULL)
	    erl_exit(1,"Could not create mutex semaphore for elib_malloc");
	if(!(alloc_flags & USER_POOL)){
	    if((alloc_pool_ptr = save_malloc(alloc_pool_size)) == NULL)
		erl_exit(1,"Erlang set to allocate a %d byte block initially;"
			 " not enough memory available.", alloc_pool_size);
	    elib_force_init(alloc_pool_ptr, alloc_pool_size);
	} else if(alloc_flags & NEW_USER_POOL){
	    elib_force_init(alloc_pool_ptr, alloc_pool_size);
	}
	actual_alloc=&elib_malloc;
	actual_realloc=&elib_realloc;
	actual_free=&elib_free;
	alloc_flags |= USING_ELIB_MALLOC;
	/* We MUST see to that the right free function is used
	   otherwise we'll get a very nasty crash! */
	if(USER_RECLAIM())
	    set_reclaim_free_function(&reclaim_free_hook);
    }
    alloc_flags &= ~(NEW_USER_POOL); /* It's never new after initialization*/
}

/* This does not exist on other platforms, we just use it in sys.c 
   and the BSD resolver */
void *sys_calloc2(Uint nelem, Uint elsize){
    void *ptr = erts_alloc_fnf(ERTS_ALC_T_UNDEF, nelem*elsize);
    if(ptr != NULL)
	memset(ptr,0,nelem*elsize);
    return ptr;
}

/*
 * The malloc wrapper
 */
void *
erts_sys_alloc(ErtsAlcType_t type, void *extra, Uint size)
{
    register void *ret;
    ELIB_LOCK;
    if(USER_RECLAIM())
	ret = save_malloc2((size_t)size,actual_alloc);
    else
	ret = (*actual_alloc)((size_t)size);
    ELIB_UNLOCK;
    return ret;
}

/*
 * The realloc wrapper, may respond to the "realloc-always-moves" flag
 * if the area is initially allocated with elib_malloc.
 */
void *
erts_sys_realloc(ErtsAlcType_t type, void *extra, void *ptr, Uint size)
{
    register void *ret;
    if(use_save_free(ptr)){
	if((alloc_flags & WARN_MALLOC_MIX) && 
	   (alloc_flags & USING_ELIB_MALLOC))
	    erts_fprintf(stderr,"Warning, save_malloced data realloced "
		       "by sys_realloc2\n");
	return save_realloc(ptr, (size_t) size);
    } else {	
	ELIB_LOCK;
	if((alloc_flags & REALLOC_MOVES) && 
	   (alloc_flags & USING_ELIB_MALLOC)){
	    size_t osz = elib_sizeof(ptr);
	    if(USER_RECLAIM())
		ret = save_malloc2((size_t) size, actual_alloc);
	    else
		ret = (*actual_alloc)((size_t) size);
	    if(ret != NULL){
		memcpy(ret,ptr,(((size_t)size) < osz) ? ((size_t)size) : osz);
		if(USER_RECLAIM())
		    save_free2(ptr,actual_free);
		else
		    (*actual_free)(ptr);
	    }
	} else {
	    if(USER_RECLAIM())
		ret = save_realloc2(ptr,(size_t)size,actual_realloc);
	    else
		ret = (*actual_realloc)(ptr,(size_t)size);
	}
	ELIB_UNLOCK;
	return ret;
    }
}

/*
 * Wrapped free().
 */
void
erts_sys_free(ErtsAlcType_t type, void *extra, void *ptr)
{
    if(use_save_free(ptr)){
	/* 
	 * This might happen when linked in drivers use save_malloc etc 
	 * directly. 
	 */
	if((alloc_flags & WARN_MALLOC_MIX) && 
	   (alloc_flags & USING_ELIB_MALLOC))
	    erts_fprintf(stderr,"Warning, save_malloced data freed by "
		       "sys_free2\n");
	save_free(ptr);
    } else {
	ELIB_LOCK;
	if(USER_RECLAIM())
	    save_free2(ptr,actual_free);
	else
	    (*actual_free)(ptr);
	ELIB_UNLOCK;
    }
}

/* 
 * External interface to be called before erlang is started 
 * Parameters:
 * isize: The size of the memory block where erlang should malloc().
 * iptr: (optional) A pointer to a user supplied memory block of 
 *       size isize.
 * warn_save: Instructs sys_free2 and sys_realloc2 to warn if
 *            memory allocation/reallocation/freeing is mixed between
 *            pure malloc/save_malloc/sys_alloc2 routines (only
 *            warns if elib is actually used in the sys_alloc2 routines).
 * realloc_moves: Always allocate a fresh memory block on reallocation 
 *                (less fragmentation).
 * reclaim_in_supplied: Use memory reclaim mechanisms inside the user
 *                      supplied area, this makes one area reusable between
 *                      starts of erlang and might be nice for drivers etc.
 */

int erl_set_memory_block(int isize, int iptr, int warn_save, 
			 int realloc_moves, int reclaim_in_supplied, int p5,
			 int p6, int p7, int p8, int p9){
    if(erlang_id != 0){
	erts_fprintf(stderr,"Error, cannot set erlang memory block while an "
		     "erlang task is running!\n");
	return 1;
    }
    if(isize < 8 * 1024 *1024)
	erts_fprintf(stderr,
		     "Warning, the memory pool of %dMb may be to small to "
		     "run erlang in!\n", isize / (1024 * 1024));
    alloc_pool_size = (size_t) isize;
    alloc_pool_ptr = (void *) iptr;
    alloc_flags = 0;
    /* USING_ELIB_MALLOC gets set by the initialization routine */
    if((void *)iptr != NULL)
	alloc_flags |= (USER_POOL | NEW_USER_POOL);
    if(realloc_moves)
	alloc_flags |= REALLOC_MOVES;
    if(warn_save)
	alloc_flags |= WARN_MALLOC_MIX;
    if((void *)iptr != NULL && reclaim_in_supplied)
	alloc_flags |= RECLAIM_USER_POOL;
    return 0;
}

/* External statistics interface */
int erl_memory_show(int p0, int p1, int p2, int p3, int p4, int p5,
		    int p6, int p7, int p8, int p9){
    struct elib_stat statistics;
    if(!(alloc_flags & USING_ELIB_MALLOC) && erlang_id != 0){
	erts_printf("Using plain save_alloc, use memShow instead.\n");
	return 1;
    }
    if(erlang_id == 0 && !((alloc_flags & USER_POOL) && 
			   !(alloc_flags & NEW_USER_POOL))){
	erts_printf("Sorry, no allocation statistics until erlang "
		   "is started.\n");
	return 1;
    }
    erts_printf("Allocation settings:\n");
    erts_printf("Using elib_malloc with memory pool size of %lu bytes.\n",
	       (unsigned long) alloc_pool_size);
    erts_printf("Realloc-always-moves is %s\n", 
	       (alloc_flags & REALLOC_MOVES) ? "on" : "off");
    erts_printf("Warnings about mixed malloc/free's are %s\n", 
	       (alloc_flags & WARN_MALLOC_MIX) ? "on" : "off");
    if(alloc_flags & USER_POOL){
	erts_printf("The memory block used by elib is user supplied "
		   "at 0x%08x.\n", (unsigned int) alloc_pool_ptr);
	if(alloc_flags & RECLAIM_USER_POOL)
	    erts_printf("Allocated memory within the user supplied pool\n"
		       "  will be automatically reclaimed at task exit.\n");
    } else {
	erts_printf("The memory block used by elib is save_malloc'ed "
		   "at 0x%08x.\n", (unsigned int) alloc_pool_ptr);
    }
#ifdef NO_FIX_ALLOC
    erts_printf("Fix_alloc is disabled in this build\n");
#endif
    erts_printf("Statistics from elib_malloc:\n");
    ELIB_LOCK;

    elib_stat(&statistics);
    ELIB_UNLOCK;
    erts_printf("Type          Size (bytes) Number of blocks\n");
    erts_printf("============= ============ ================\n");
    erts_printf("Total:        %12lu %16lu\n",
	       (unsigned long) statistics.mem_total*4,
	       (unsigned long) statistics.mem_blocks);
    erts_printf("Allocated:    %12lu %16lu\n",
	       (unsigned long) statistics.mem_alloc*4,
	       (unsigned long) statistics.mem_blocks-statistics.free_blocks);
    erts_printf("Free:         %12lu %16lu\n",
	       (unsigned long) statistics.mem_free*4,
	       (unsigned long) statistics.free_blocks);
    erts_printf("Largest free: %12lu                -\n\n",
	       (unsigned long) statistics.max_free*4);
    return 0;
}


/*
** More programmer friendly (as opposed to user friendly ;-) interface
** to the memory statistics. Resembles the VxWorks memPartInfoGet but
** does not take a partition id as parameter...
*/    
int erl_mem_info_get(MEM_PART_STATS *stats){
    struct elib_stat statistics;
    if(!(alloc_flags & USING_ELIB_MALLOC))
	return -1;
    ELIB_LOCK;
    elib_stat(&statistics);
    ELIB_UNLOCK;
    stats->numBytesFree = statistics.mem_free*4;
    stats->numBlocksFree = statistics.free_blocks;
    stats->maxBlockSizeFree = statistics.max_free*4;
    stats->numBytesAlloc = statistics.mem_alloc*4;
    stats->numBlocksAlloc = statistics.mem_blocks-statistics.free_blocks;
    return 0;
}

/********************* Pipe driver **********************************/
/*
 * Purpose:  Pipe driver with Unix (unnamed) pipe semantics.
 * Author:   Peter Hogfeldt (peter@erix.ericsson.se) from an outline
 *           by Per Hedeland (per@erix.ericsson.se).
 *
 * Note: This driver must *not* use the reclaim facilities, hence it 
 * is placed here. (after the #undef's of open,malloc etc)
 *
 * This driver supports select() and non-blocking I/O via 
 * ioctl(fd, FIONBIO, val).
 * 
 * 1997-03-21 Peter Hogfeldt
 *            Added non-blocking I/O.
 *
 */

/* 
 *  SEMAPHORES
 *
 *  Each end of a pipe has two semaphores: semExcl for serialising access to 
 *  the pipe end, and semBlock for blocking I/O.
 *
 *  reader->semBlock         is available (full) if and only if the pipe is
 *                           not empty, or the write end is closed. Otherwise
 *                           it is unavailable (empty). It is initially
 *                           unavailable.
 *
 *  writer->semBlock         is available (full) if and only if the pipe is
 *                           not full, or if the reader end is closed. 
 *                           Otherwise it is unavailable. It is initially
 *                           available.
 */

#define UXPIPE_SIZE 4096

/* Forward declaration */
typedef struct uxPipeDev UXPIPE_DEV;

/*
 * Pipe descriptor (one for each open pipe).
 */
typedef struct {
  int drvNum;
  UXPIPE_DEV *reader, *writer;
  RING_ID ringId;
}     UXPIPE;

/*
 * Device descriptor (one for each of the read and write
		      * ends of an open pipe).
		      */
struct uxPipeDev {
  UXPIPE *pipe;
  int blocking;
  SEL_WAKEUP_LIST wakeupList;
  SEM_ID semExcl;
  SEM_ID semBlock;
};

int uxPipeDrvNum = 0; /* driver number of pipe driver */

#define PIPE_NAME 	"/uxpipe" /* only used internally */
#define PIPE_READ 	"/r"	/* ditto */
#define PIPE_WRITE 	"/w"	/* ditto */

LOCAL char pipeRead[64], pipeWrite[64];
LOCAL DEV_HDR devHdr;
LOCAL UXPIPE *newPipe; /* communicate btwn open()s in pipe() */
LOCAL SEM_ID pipeSem; /* mutual exclusion in pipe() */

/* forward declarations */
LOCAL int uxPipeOpen(DEV_HDR *pDv, char *name, int mode);
LOCAL int uxPipeClose(UXPIPE_DEV *pDev);
LOCAL int uxPipeRead(UXPIPE_DEV *pDev, char *buffer, int maxbytes);
LOCAL int uxPipeWrite(UXPIPE_DEV *pDev, char *buffer, int nbytes);
LOCAL STATUS uxPipeIoctl(FAST UXPIPE_DEV *pDev, FAST int function, int arg);


/***************************************************************************
 *
 * uxPipeDrv - install Unix pipe driver
 *
 * This routine initializes the Unix pipe driver.  It must be called
 * before any other routine in this driver.
 *
 * RETURNS:
 *    OK, or ERROR if I/O system is unable to install driver.
 */

STATUS
uxPipeDrv(void)
{
  if (uxPipeDrvNum > 0)
    return (OK);		/* driver already installed */
  if ((uxPipeDrvNum = iosDrvInstall((FUNCPTR) NULL, (FUNCPTR) NULL,
				    uxPipeOpen, uxPipeClose, uxPipeRead,
				    uxPipeWrite, uxPipeIoctl)) == ERROR)
    return (ERROR);
  if (iosDevAdd(&devHdr, PIPE_NAME, uxPipeDrvNum) == ERROR)
    return (ERROR);
  strcpy(pipeRead, PIPE_NAME);
  strcat(pipeRead, PIPE_READ);
  strcpy(pipeWrite, PIPE_NAME);
  strcat(pipeWrite, PIPE_WRITE);
  if ((pipeSem = semMCreate(SEM_Q_PRIORITY | SEM_DELETE_SAFE)) == NULL)
    return (ERROR);
  return (OK);
}

/***************************************************************************
 *
 * uxPipeOpen - open a pipe
 *
 * RETURNS: Pointer to device descriptor, or ERROR if memory cannot be
 * allocated (errno = ENOMEM),  or invalid argument (errno = EINVAL).
 */

/*
 * DEV_HDR *pDv;  pointer to device header (dummy) 
 * char *name;  name of pipe to open ("/r" or "/w") 
 * int   mode;  access mode (O_RDONLY or O_WRONLY)
 */		      
LOCAL int
uxPipeOpen(DEV_HDR *pDv, char *name, int mode)
{
  UXPIPE_DEV *reader, *writer;

  if (mode == O_RDONLY && strcmp(name, PIPE_READ) == 0) {
    /* reader open */
    if ((newPipe = (UXPIPE *) malloc(sizeof(UXPIPE))) != NULL) {
      if ((newPipe->ringId = rngCreate(UXPIPE_SIZE)) != NULL) {
	if ((reader = (UXPIPE_DEV *) malloc(sizeof(UXPIPE_DEV))) != NULL) {
	  if ((reader->semExcl = semBCreate(SEM_Q_FIFO, SEM_FULL)) != NULL) {
	    if ((reader->semBlock = semBCreate(SEM_Q_FIFO, SEM_EMPTY)) != NULL) {
	      reader->pipe = newPipe;
	      reader->blocking = 1;
	      selWakeupListInit(&reader->wakeupList);
	      newPipe->reader = reader;
	      newPipe->writer = NULL;
	      newPipe->drvNum = uxPipeDrvNum;
	      return ((int) reader);
	    }
	    semDelete(reader->semExcl);
	  }
	  free(reader);
	}
	rngDelete(newPipe->ringId);
      }
      free(newPipe);
      newPipe = NULL;
      errno = ENOMEM;
    }
  } else if (mode == O_WRONLY && strcmp(name, PIPE_WRITE) == 0) {
    /* writer open */
    if (newPipe != NULL &&
	(writer = (UXPIPE_DEV *) malloc(sizeof(UXPIPE_DEV))) != NULL) {
      if ((writer->semExcl = semBCreate(SEM_Q_FIFO, SEM_FULL)) != NULL) {
	if ((writer->semBlock = semBCreate(SEM_Q_FIFO, SEM_FULL)) != NULL) {
	  writer->blocking = 1;
	  writer->pipe = newPipe;
	  selWakeupListInit(&writer->wakeupList);
	  newPipe->writer = writer;
	  newPipe = NULL;
	  return ((int) writer);
	}
	semDelete(writer->semExcl);
      }
      free(writer);
    }
    if (newPipe != NULL)
      free(newPipe);
    newPipe = NULL;
    errno = ENOMEM;
  } else {
    errno = EINVAL;
  }
  return (ERROR);
}

/***************************************************************************
 *
 * uxPipeClose - close read or write end of a pipe.
 *
 * RETURNS:
 *    OK, or ERROR if device descriptor does not refer to an open read or
 write end of a pipe (errno = EBADF).
 */

LOCAL int
uxPipeClose(UXPIPE_DEV *pDev)
{
  UXPIPE *pajp = pDev->pipe;

  taskLock();
  if (pDev == pajp->reader) {
    /* Close this end */
    semDelete(pDev->semExcl);
    semDelete(pDev->semBlock);
    free(pDev);
    pajp->reader = NULL;
    /* Inform the other end */
    if (pajp->writer != NULL) {
      selWakeupAll(&pajp->writer->wakeupList, SELWRITE);
      semGive(pajp->writer->semBlock);
    }
  } else if (pDev == pajp->writer) {
    /* Close this end */
    semDelete(pDev->semExcl);
    semDelete(pDev->semBlock);
    free(pDev);
    pajp->writer = NULL;
    /* Inform the other end */
    if (pajp->reader != NULL) {
      selWakeupAll(&pajp->reader->wakeupList, SELREAD);
      semGive(pajp->reader->semBlock);
    }
  } else {
    errno = EBADF;
    taskUnlock();
    return (ERROR);
  }
  if (pajp->reader == NULL && pajp->writer == NULL) {
    rngDelete(pajp->ringId);
    pajp->drvNum = 0;
    free(pajp);
  }
  taskUnlock();
  return (OK);
}
/***************************************************************************
 *
 * uxPipeRead - read from a pipe.
 *
 * Reads at most maxbytes bytes from the pipe. Blocks if blocking mode is
 * set and the pipe is empty.
 *
 * RETURNS:
 *    number of bytes read, 0 on EOF, or ERROR if device descriptor does
 *    not refer to an open read end of a pipe (errno = EBADF), or if 
 *    non-blocking mode is set and the pipe is empty (errno = EWOULDBLOCK).
 */

LOCAL int
uxPipeRead(UXPIPE_DEV *pDev, char *buffer, int maxbytes)
{
  UXPIPE *pajp = pDev->pipe;
  int   nbytes = 0;

  if (pDev != pajp->reader) {
    errno = EBADF;
    return (ERROR);
  }
  if (maxbytes == 0)
    return (0);
  semTake(pDev->semExcl, WAIT_FOREVER);
  /* 
   * Note that semBlock may be full, although there is nothing to read.
   * This happens e.g. after the following sequence of operations: a 
   * reader task blocks, a writer task writes two times (the first 
   * write unblocks the reader task, the second write makes semBlock
   * full).
   */
  while (nbytes == 0) {
    if (pDev->blocking) 
      semTake(pDev->semBlock, WAIT_FOREVER);
    /* 
     * Reading and updating of the write end must not be interleaved
     * with a write from another task - hence we lock this task.
     */
    taskLock();
    nbytes = rngBufGet(pajp->ringId, buffer, maxbytes);
    if (nbytes > 0) {
      /* Give own semaphore if bytes remain or if write end is closed */
      if ((!rngIsEmpty(pajp->ringId) || pajp->writer == NULL) &&
	  pDev->blocking)
	semGive(pDev->semBlock);
      /* Inform write end */
      if (pajp->writer != NULL) {
	if (pajp->writer->blocking) 
	  semGive(pajp->writer->semBlock);
	selWakeupAll(&pajp->writer->wakeupList, SELWRITE);
      }
    } else if (pajp->writer == NULL) {
      nbytes = 0;		/* EOF */
      /* Give semaphore when write end is closed */
      if (pDev->blocking) 
	semGive(pDev->semBlock);
      taskUnlock();
      semGive(pDev->semExcl);
      return (nbytes);
    } else if (!pDev->blocking) {
      taskUnlock();
      semGive(pDev->semExcl);
      errno = EWOULDBLOCK;
      return (ERROR);
    }      
    taskUnlock();
  }
  semGive(pDev->semExcl);
  return (nbytes);
}

/***************************************************************************
 *
 * uxPipeWrite - write to a pipe.
 *
 * Writes nbytes bytes to the pipe. Blocks if blocking mode is set, and if 
 * the pipe is full.
 *
 * RETURNS:
 * number of bytes written, or ERROR if the device descriptor does not
 * refer to an open write end of a pipe (errno = EBADF); or if the read end
 * of the pipe is closed (errno = EPIPE); or if non-blocking mode is set
 * and the pipe is full (errno = EWOULDBLOCK).
 *
 */

LOCAL int
uxPipeWrite(UXPIPE_DEV *pDev, char *buffer, int nbytes)
{

  UXPIPE *pajp = pDev->pipe;
  int   sofar = 0, written;

  if (pDev != pajp->writer) {
    errno = EBADF;
    return (ERROR);
  }
  if (pajp->reader == NULL) {
    errno = EPIPE;
    return (ERROR);
  }
  if (nbytes == 0)
    return (0);
  semTake(pDev->semExcl, WAIT_FOREVER);
  while (sofar < nbytes) {
    if (pDev->blocking)
      semTake(pDev->semBlock, WAIT_FOREVER);
    if (pajp->reader == NULL) {
      errno = EPIPE;
      semGive(pDev->semBlock);
      semGive(pDev->semExcl);
      return (ERROR);
    }
    /* Writing and updating of the read end must not be interleaved
     * with a read from another task - hence we lock this task.
     */
    taskLock();
    written = rngBufPut(pajp->ringId, buffer + sofar, nbytes - sofar);
    sofar += written;
    /* Inform the read end if we really wrote something */
    if (written > 0 && pajp->reader != NULL) {
      selWakeupAll(&pajp->reader->wakeupList, SELREAD);
      if (pajp->reader->blocking)
	semGive(pajp->reader->semBlock);
    }
    taskUnlock();
    if (!pDev->blocking) {
      if (sofar == 0) {
      errno = EWOULDBLOCK;
      sofar = ERROR;
      }
      break;
    }
  }
  /* Give own semaphore if space remains */
  if (!rngIsFull(pajp->ringId) && pDev->blocking)
    semGive(pDev->semBlock);
  semGive(pDev->semExcl);
  return (sofar);
}

/***************************************************************************
 *
 * uxPipeIoctl - do device specific I/O control
 *
 * RETURNS:
 *    OK or ERROR.
 */

LOCAL STATUS
uxPipeIoctl(FAST UXPIPE_DEV *pDev, FAST int function, int arg)
     
{
  UXPIPE *pajp = pDev->pipe;
  int   status = OK;

  switch (function) {
  case FIONBIO:
    pDev->blocking = (*(int *)arg) ? 0 : 1;
    break;
  case FIOSELECT:
    taskLock();
    selNodeAdd(&pDev->wakeupList, (SEL_WAKEUP_NODE *) arg);
    if (selWakeupType((SEL_WAKEUP_NODE *) arg) == SELREAD &&
	pDev == pajp->reader &&
	(!rngIsEmpty(pajp->ringId) || pajp->writer == NULL))
      selWakeup((SEL_WAKEUP_NODE *) arg);
    if (selWakeupType((SEL_WAKEUP_NODE *) arg) == SELWRITE &&
	pDev == pajp->writer &&
	(!rngIsFull(pajp->ringId) || pajp->reader == NULL))
      selWakeup((SEL_WAKEUP_NODE *) arg);
    taskUnlock();
    break;
  case FIOUNSELECT:
    selNodeDelete(&pDev->wakeupList, (SEL_WAKEUP_NODE *) arg);
    break;
  default:
    status = ERROR;
    break;
  }
  return (status);
}

/***************************************************************************
 *
 * pipe - create an intertask channel
 *
 * Creates a pipe. fd[0] (fd[1]) is the read (write) file descriptor.
 *
 * RETURNS:
 *    OK or ERROR, if the pipe could not be created.
 */

STATUS
pipe(int fd[2])
{
  semTake(pipeSem, WAIT_FOREVER);
  if ((fd[0] = open(pipeRead, O_RDONLY, 0)) != ERROR) {
    if ((fd[1] = open(pipeWrite, O_WRONLY, 0)) != ERROR) {
      semGive(pipeSem);
      return (OK);
    }
    (void) close(fd[0]);
  }
  errno &= 0xFFFF;
  if((errno & 0xFFFF) == EINTR) /* Why on earth EINTR??? */
      errno = ENFILE;           /* It means we are out of file descriptors...*/
  semGive(pipeSem);
  return (ERROR);
}

/***************************************************************************
 *
 * uxPipeShow - display pipe information
 *
 * RETURNS:
 *    N/A.
 */

void
uxPipeShow(int fd)
{
  UXPIPE_DEV *pDev;
  UXPIPE *pajp;
  int drvValue;

  if ((drvValue = iosFdValue(fd)) == ERROR) {
    erts_fprintf(stderr, "Error: file descriptor invalid\n");
    return;
  }
  pDev = (UXPIPE_DEV *)drvValue;
  pajp = pDev->pipe;
  if (pajp->drvNum != uxPipeDrvNum) {
    erts_fprintf(stderr, "Error: Not a ux pipe device\n");
    return;
  }
  erts_fprintf(stderr, "Device              : 0x%x\n", (int) pDev);
  erts_fprintf(stderr, "Buffer size         : %d\n", UXPIPE_SIZE);
  erts_fprintf(stderr, "Bytes in buffer     : %d\n\n", rngNBytes(pajp->ringId));
  erts_fprintf(stderr, "READ END\n\n");
  if (pajp->reader != NULL) {
    erts_fprintf(stderr, "Mode                : ");
    erts_fprintf(stderr, "%s\n", 
	       (pajp->reader->blocking) ? "blocking" : "non-blocking");
  }
  erts_fprintf(stderr, "Status              : ");
  if (pajp->reader != NULL) {
    erts_fprintf(stderr, "OPEN\n");
    erts_fprintf(stderr, "Wake-up list        : %d\n\n", 
	       selWakeupListLen(&pajp->reader->wakeupList));
    erts_fprintf(stderr, "Exclusion Semaphore\n");
    semShow(pajp->reader->semExcl, 1);
    erts_fprintf(stderr, "Blocking Semaphore\n");
    semShow(pajp->reader->semBlock, 1);
  } else 
    erts_fprintf(stderr, "CLOSED\n\n");
  erts_fprintf(stderr, "WRITE END\n\n");
  if (pajp->writer != NULL) {
    erts_fprintf(stderr, "Mode                : ");
    erts_fprintf(stderr, "%s\n", 
	       (pajp->writer->blocking) ? "blocking" : "non-blocking");
  }
  erts_fprintf(stderr, "Status              : ");
  if (pajp->writer != NULL) {
    erts_fprintf(stderr, "OPEN\n");
    erts_fprintf(stderr, "Wake-up list        : %d\n\n", 
	       selWakeupListLen(&pajp->writer->wakeupList));
    erts_fprintf(stderr, "Exclusion Semaphore\n");
    semShow(pajp->writer->semExcl, 1);
    erts_fprintf(stderr, "Blocking Semaphore\n");
    semShow(pajp->writer->semBlock, 1);
  } else 
    erts_fprintf(stderr, "CLOSED\n\n");
}

#ifdef DEBUG
void
erl_assert_error(char* expr, char* file, int line)
{   
    fflush(stdout);
    fprintf(stderr, "Assertion failed: %s in %s, line %d\n",
	    expr, file, line);
    fflush(stderr);
    erl_crash_dump(file, line, "Assertion failed: %s\n", expr);
    abort();
}
void
erl_debug(char* fmt, ...)
{
    char sbuf[1024];		/* Temporary buffer. */
    va_list va;
    
    va_start(va, fmt);
    vsprintf(sbuf, fmt, va);
    va_end(va);
    fprintf(stderr, "%s\n", sbuf);
}
#endif
