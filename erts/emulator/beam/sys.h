/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1996-2011. All Rights Reserved.
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

#ifndef __SYS_H__
#define __SYS_H__


#if defined(VALGRIND) && !defined(NO_FPE_SIGNALS)
#  define NO_FPE_SIGNALS
#endif

/* xxxP __VXWORKS__ */
#ifdef VXWORKS
#include <vxWorks.h>
#endif

#ifdef DISABLE_CHILD_WAITER_THREAD
#undef ENABLE_CHILD_WAITER_THREAD
#endif

#if defined(ERTS_SMP) && !defined(DISABLE_CHILD_WAITER_THREAD)
#undef ENABLE_CHILD_WAITER_THREAD
#define ENABLE_CHILD_WAITER_THREAD 1
#endif

#if defined (__WIN32__)
#  include "erl_win_sys.h"
#elif defined (VXWORKS) 
#  include "erl_vxworks_sys.h"
#else 
#  include "erl_unix_sys.h"
#ifndef UNIX
#  define UNIX 1
#endif
#endif

#include "erl_misc_utils.h"

/*
 * To allow building of Universal Binaries for Mac OS X,
 * we must not depend on the endian detected by the configure script.
 */
#if defined(__APPLE__)
#  if defined(__BIG_ENDIAN__) && !defined(WORDS_BIGENDIAN)
#    define WORDS_BIGENDIAN 1
#  elif !defined(__BIG_ENDIAN__) && defined(WORDS_BIGENDIAN)
#    undef WORDS_BIGENDIAN
#  endif
#endif

/*
 * Make sure we have a type for FD's (used by erl_check_io)
 */

#ifndef ERTS_SYS_FD_TYPE
typedef int ErtsSysFdType;
#else
typedef ERTS_SYS_FD_TYPE ErtsSysFdType;
#endif

#ifdef ERTS_INLINE
#  ifndef ERTS_CAN_INLINE
#    define ERTS_CAN_INLINE 1
#  endif
#else
#  if defined(__GNUC__)
#    define ERTS_CAN_INLINE 1
#    define ERTS_INLINE __inline__
#  elif defined(__WIN32__)
#    define ERTS_CAN_INLINE 1
#    define ERTS_INLINE __inline
#  else
#    define ERTS_CAN_INLINE 0
#    define ERTS_INLINE
#  endif
#endif

#ifdef __GNUC__
#  if __GNUC__ < 3 && (__GNUC__ != 2 || __GNUC_MINOR__ < 96)
#    define ERTS_LIKELY(BOOL)   (BOOL)
#    define ERTS_UNLIKELY(BOOL) (BOOL)
#  else
#    define ERTS_LIKELY(BOOL)   __builtin_expect((BOOL), !0)
#    define ERTS_UNLIKELY(BOOL) __builtin_expect((BOOL), 0)
#  endif
#else
#  define ERTS_LIKELY(BOOL)   (BOOL)
#  define ERTS_UNLIKELY(BOOL) (BOOL)
#endif

#if defined(DEBUG) || defined(ERTS_ENABLE_LOCK_CHECK)
#  undef ERTS_CAN_INLINE
#  define ERTS_CAN_INLINE 0
#  undef ERTS_INLINE
#  define ERTS_INLINE
#endif

#if ERTS_CAN_INLINE
#define ERTS_GLB_INLINE static ERTS_INLINE
#else
#define ERTS_GLB_INLINE
#endif

#if ERTS_CAN_INLINE || defined(ERTS_DO_INCL_GLB_INLINE_FUNC_DEF) 
#  define ERTS_GLB_INLINE_INCL_FUNC_DEF 1
#else
#  define ERTS_GLB_INLINE_INCL_FUNC_DEF 0
#endif

#ifndef ERTS_EXIT_AFTER_DUMP
#  define ERTS_EXIT_AFTER_DUMP exit
#endif

#ifdef DEBUG
#  define ASSERT(e) \
  if (e) { \
     ; \
  } else { \
     erl_assert_error(#e, __FILE__, __LINE__); \
  }
#  define ASSERT_EXPR(e) \
    ((void) ((e) ? 1 : (erl_assert_error(#e, __FILE__, __LINE__), 0)))
void erl_assert_error(char* expr, char* file, int line);
#else
#  define ASSERT(e)
#  define ASSERT_EXPR(e) ((void) 1)
#endif

/*
 * Microsoft C/C++: We certainly want to use stdarg.h and prototypes.
 * But MSC doesn't define __STDC__, unless we compile with the -Za
 * flag (strict ANSI C, no Microsoft extension).  Compiling with -Za
 * doesn't work: some Microsoft headers fail to compile...
 *
 * Solution: Test if __STDC__ or _MSC_VER is defined.
 *
 * Note: Simply defining __STDC__ doesn't work, as some Microsoft
 * headers will fail to compile!
 */

#include <stdarg.h>

/* This isn't sys-dependent, but putting it here benefits sys.c and drivers
   - allow use of 'const' regardless of compiler */

#if !defined(__STDC__) && !defined(_MSC_VER)
#  define const
#endif

#ifdef VXWORKS
/* Replace VxWorks' printf with a real one that does fprintf(stdout, ...) */
int real_printf(const char *fmt, ...);
#  define printf real_printf
#endif

/* In VC++, noreturn is a declspec that has to be before the types,
 * but in GNUC it is an att ribute to be placed between return type 
 * and function name, hence __decl_noreturn <types> __noreturn <function name>
 */
#if __GNUC__
#  define __decl_noreturn 
#  define __noreturn __attribute__((noreturn))
#  undef __deprecated
#  if __GNUC__ >= 3
#    define __deprecated __attribute__((deprecated))
#  else
#    define __deprecated
#  endif
#else
#  if defined(__WIN32__) && defined(_MSC_VER)
#    define __noreturn 
#    define __decl_noreturn __declspec(noreturn)
#  else
#    define __noreturn
#    define __decl_noreturn 
#  endif
#  define __deprecated
#endif

/*
** Data types:
**
** Eterm: A tagged erlang term (possibly 64 bits)
** BeamInstr: A beam code instruction unit, possibly larger than Eterm, not smaller.
** UInt:  An unsigned integer exactly as large as an Eterm.
** SInt:  A signed integer exactly as large as an eterm and therefor large
**        enough to hold the return value of the signed_val() macro.
** UWord: An unsigned integer at least as large as a void * and also as large
**          or larger than an Eterm
** SWord: A signed integer at least as large as a void * and also as large
**          or larger than an Eterm
** Uint32: An unsigned integer of 32 bits exactly
** Sint32: A signed integer of 32 bits exactly
** Uint16: An unsigned integer of 16 bits exactly
** Sint16: A signed integer of 16 bits exactly.
*/

#if !((SIZEOF_VOID_P >= 4) && (SIZEOF_VOID_P == SIZEOF_SIZE_T) \
      && ((SIZEOF_VOID_P == SIZEOF_INT) || (SIZEOF_VOID_P == SIZEOF_LONG)))
#error Cannot handle this combination of int/long/void*/size_t sizes
#endif

#if SIZEOF_VOID_P == 8
#undef  ARCH_32
#define ARCH_64
#elif SIZEOF_VOID_P == 4
#define ARCH_32
#undef  ARCH_64
#else
#error Neither 32 nor 64 bit architecture
#endif
#if defined(ARCH_64) && defined(HALFWORD_HEAP_EMULATOR)
#    define HALFWORD_HEAP 1
#    define HALFWORD_ASSERT 0
#    define ASSERT_HALFWORD(COND) ASSERT(COND)
#else
#    define HALFWORD_HEAP 0
#    define HALFWORD_ASSERT 0
#    define ASSERT_HALFWORD(COND)
#endif

#if SIZEOF_VOID_P != SIZEOF_SIZE_T
#error sizeof(void*) != sizeof(size_t)
#endif

#if HALFWORD_HEAP

#if SIZEOF_INT == 4
typedef unsigned int Eterm;
typedef unsigned int Uint;
typedef int          Sint;
#define ERTS_SIZEOF_ETERM SIZEOF_INT
#else
#error Found no appropriate type to use for 'Eterm', 'Uint' and 'Sint'
#endif

#if SIZEOF_VOID_P == SIZEOF_LONG
typedef unsigned long UWord;
typedef long          SWord;
#elif SIZEOF_VOID_P == SIZEOF_INT
typedef unsigned int UWord;
typedef int          SWord;
#else
#error Found no appropriate type to use for 'Eterm', 'Uint' and 'Sint'
#endif

#else /* !HALFWORD_HEAP */

#if SIZEOF_VOID_P == SIZEOF_LONG
typedef unsigned long Eterm;
typedef unsigned long Uint;
typedef long          Sint;
#define ERTS_SIZEOF_ETERM SIZEOF_LONG
#elif SIZEOF_VOID_P == SIZEOF_INT
typedef unsigned int Eterm;
typedef unsigned int Uint;
typedef int          Sint;
#define ERTS_SIZEOF_ETERM SIZEOF_INT
#else
#error Found no appropriate type to use for 'Eterm', 'Uint' and 'Sint'
#endif

typedef Uint UWord;
typedef Sint SWord;

#endif /* HALFWORD_HEAP */

typedef UWord BeamInstr;

#ifndef HAVE_INT64
#if SIZEOF_LONG == 8
#define HAVE_INT64 1
typedef unsigned long Uint64;
typedef long          Sint64;
#elif SIZEOF_LONG_LONG == 8
#define HAVE_INT64 1
typedef unsigned long long Uint64;
typedef long long          Sint64;
#else
#define HAVE_INT64 0
#endif
#endif

#if SIZEOF_LONG == 4
typedef unsigned long Uint32;
typedef long          Sint32;
#elif SIZEOF_INT == 4
typedef unsigned int Uint32;
typedef int          Sint32;
#else
#error Found no appropriate type to use for 'Uint32' and 'Sint32'
#endif

#if SIZEOF_INT == 2
typedef unsigned int Uint16;
typedef int          Sint16;
#elif SIZEOF_SHORT == 2
typedef unsigned short Uint16;
typedef short          Sint16;
#else
#error Found no appropriate type to use for 'Uint16' and 'Sint16'
#endif

#if CHAR_BIT == 8
typedef unsigned char byte;
#else
#error Found no appropriate type to use for 'byte'
#endif

#if defined(ARCH_64) && !HAVE_INT64
#error 64-bit architecture, but no appropriate type to use for Uint64 and Sint64 found 
#endif

#  define ERTS_EXTRA_DATA_ALIGN_SZ(X) \
    (((size_t) 8) - (((size_t) (X)) & ((size_t) 7)))

#include "erl_lock_check.h"

/* needed by erl_smp.h */
int erts_send_warning_to_logger_str_nogl(char *);

#include "erl_smp.h"

#ifdef ERTS_WANT_BREAK_HANDLING
#  ifdef ERTS_SMP
extern erts_smp_atomic32_t erts_break_requested;
#    define ERTS_BREAK_REQUESTED \
  ((int) erts_smp_atomic32_read_nob(&erts_break_requested))
#  else
extern volatile int erts_break_requested;
#    define ERTS_BREAK_REQUESTED erts_break_requested
#  endif
void erts_do_break_handling(void);
#endif

#ifdef ERTS_WANT_GOT_SIGUSR1
#  ifndef UNIX
#    define ERTS_GOT_SIGUSR1 0
#  else
#    ifdef ERTS_SMP
extern erts_smp_atomic32_t erts_got_sigusr1;
#      define ERTS_GOT_SIGUSR1 ((int) erts_smp_atomic32_read_mb(&erts_got_sigusr1))
#    else
extern volatile int erts_got_sigusr1;
#      define ERTS_GOT_SIGUSR1 erts_got_sigusr1
#    endif
#  endif
#endif

#ifdef ERTS_SMP
extern erts_smp_atomic32_t erts_writing_erl_crash_dump;
extern erts_tsd_key_t erts_is_crash_dumping_key;
#define ERTS_SOMEONE_IS_CRASH_DUMPING \
  ((int) erts_smp_atomic32_read_mb(&erts_writing_erl_crash_dump))
#define ERTS_IS_CRASH_DUMPING \
  ((int) (SWord) erts_tsd_get(erts_is_crash_dumping_key))
#else
extern volatile int erts_writing_erl_crash_dump;
#define ERTS_SOMEONE_IS_CRASH_DUMPING erts_writing_erl_crash_dump
#define ERTS_IS_CRASH_DUMPING erts_writing_erl_crash_dump
#endif

/* Deal with memcpy() vs bcopy() etc. We want to use the mem*() functions,
   but be able to fall back on bcopy() etc on systems that don't have
   mem*(), but this doesn't work to well with memset()/bzero() - thus the
   memzero() macro.
*/

/* xxxP */
#if defined(USE_BCOPY)
#  define memcpy(a, b, c) bcopy((b), (a), (c))
#  define memcmp(a, b, c) bcmp((a), (b), (c))
#  define memzero(buf, len) bzero((buf), (len))
#else
#  define memzero(buf, len) memset((buf), '\0', (len))
#endif

/* Stuff that is useful for port programs, drivers, etc */

#ifdef ISC32			/* Too much for the Makefile... */
#  define signal	sigset
#  define NO_ASINH
#  define NO_ACOSH
#  define NO_ATANH
#  define NO_FTRUNCATE
#  define SIG_SIGHOLD
#  define _POSIX_SOURCE 
#  define _XOPEN_SOURCE
#endif

#ifdef QNX			/* Too much for the Makefile... */
#  define SYS_SELECT_H
#  define NO_ERF
#  define NO_ERFC
/* This definition doesn't take NaN into account, but matherr() gets those */
#  define finite(x) (fabs(x) != HUGE_VAL)
#  define USE_MATHERR
#  define HAVE_FINITE
#endif


#ifdef WANT_NONBLOCKING	    /* must define this to pull in fcntl.h/ioctl.h */

/* This is really a mess... We used to use fcntl O_NDELAY, but that seems
   to only work on SunOS 4 - in particular, on SysV-based systems
   (including Solaris 2), it does set non-blocking mode, but causes
   read() to return 0!!  fcntl O_NONBLOCK is specified by POSIX, and
   seems to work on most systems, with the notable exception of AIX,
   where the old ioctl FIONBIO is the *only* one that will set a *socket*
   in non-blocking mode - and ioctl FIONBIO on AIX *doesn't* work for
   pipes or ttys (O_NONBLOCK does)!!! For now, we'll use FIONBIO for AIX. */

#  ifdef __WIN32__

static unsigned long zero_value = 0, one_value = 1;
#    define SET_BLOCKING(fd)	{ if (ioctlsocket((fd), FIONBIO, &zero_value) != 0) fprintf(stderr, "Error setting socket to non-blocking: %d\n", WSAGetLastError()); }
#    define SET_NONBLOCKING(fd)	ioctlsocket((fd), FIONBIO, &one_value)

#  else
#    ifdef VXWORKS
#      include <fcntl.h> /* xxxP added for O_WRONLY etc ... macro:s ... */
#      include <ioLib.h>
static const int zero_value = 0, one_value = 1;
#      define SET_BLOCKING(fd)	ioctl((fd), FIONBIO, (int)&zero_value)
#      define SET_NONBLOCKING(fd)	ioctl((fd), FIONBIO, (int)&one_value)
#      define ERRNO_BLOCK EWOULDBLOCK

#    else
#      ifdef NB_FIONBIO		/* Old BSD */
#        include <sys/ioctl.h>
  static const int zero_value = 0, one_value = 1;
#        define SET_BLOCKING(fd)	ioctl((fd), FIONBIO, &zero_value)
#        define SET_NONBLOCKING(fd)	ioctl((fd), FIONBIO, &one_value)
#        define ERRNO_BLOCK EWOULDBLOCK
#      else /* !NB_FIONBIO */
#        include <fcntl.h>
#        ifdef NB_O_NDELAY		/* Nothing needs this? */
#          define NB_FLAG O_NDELAY
#          ifndef ERRNO_BLOCK		/* allow override (e.g. EAGAIN) via Makefile */
#            define ERRNO_BLOCK EWOULDBLOCK
#          endif
#        else  /* !NB_O_NDELAY */	/* The True Way - POSIX!:-) */
#          define NB_FLAG O_NONBLOCK
#          define ERRNO_BLOCK EAGAIN
#        endif /* !NB_O_NDELAY */
#        define SET_BLOCKING(fd)	fcntl((fd), F_SETFL, \
	  			      fcntl((fd), F_GETFL, 0) & ~NB_FLAG)
#        define SET_NONBLOCKING(fd)	fcntl((fd), F_SETFL, \
				      fcntl((fd), F_GETFL, 0) | NB_FLAG)
#      endif /* !NB_FIONBIO */
#    endif /* _WXWORKS_ */
#  endif /* !__WIN32__ */
#endif /* WANT_NONBLOCKING */

__decl_noreturn void __noreturn erl_exit(int n, char*, ...);

/* Some special erl_exit() codes: */
#define ERTS_INTR_EXIT	INT_MIN		/* called from signal handler */
#define ERTS_ABORT_EXIT	(INT_MIN + 1)	/* no crash dump; only abort() */
#define ERTS_DUMP_EXIT	(127)		/* crash dump; then exit() */


#ifndef ERTS_SMP
int check_async_ready(void);
#ifdef USE_THREADS
void sys_async_ready(int hndl);
int erts_register_async_ready_callback(void (*funcp)(void));
#endif
#endif

Eterm erts_check_io_info(void *p);

/* Size of misc memory allocated from system dependent code */
Uint erts_sys_misc_mem_sz(void);

/* print stuff is declared here instead of in global.h, so sys stuff won't
   have to include global.h */
#include "erl_printf.h"

/* Io constants to erts_print and erts_putc */
#define ERTS_PRINT_STDERR	(2)
#define ERTS_PRINT_STDOUT	(1)
#define ERTS_PRINT_INVALID	(0) /* Don't want to use 0 since CBUF was 0 */
#define ERTS_PRINT_FILE		(-1)
#define ERTS_PRINT_SBUF		(-2)
#define ERTS_PRINT_SNBUF	(-3)
#define ERTS_PRINT_DSBUF	(-4)

#define ERTS_PRINT_MIN		ERTS_PRINT_DSBUF

typedef struct {
    char *buf;
    size_t size;
} erts_print_sn_buf;

int erts_print(int to, void *arg, char *format, ...);	/* in utils.c */
int erts_putc(int to, void *arg, char);			/* in utils.c */

/* logger stuff is declared here instead of in global.h, so sys files
   won't have to include global.h */

erts_dsprintf_buf_t *erts_create_logger_dsbuf(void);
int erts_send_info_to_logger(Eterm, erts_dsprintf_buf_t *);
int erts_send_warning_to_logger(Eterm, erts_dsprintf_buf_t *);
int erts_send_error_to_logger(Eterm, erts_dsprintf_buf_t *);
int erts_send_info_to_logger_str(Eterm, char *); 
int erts_send_warning_to_logger_str(Eterm, char *);
int erts_send_error_to_logger_str(Eterm, char *);
int erts_send_info_to_logger_nogl(erts_dsprintf_buf_t *);
int erts_send_warning_to_logger_nogl(erts_dsprintf_buf_t *);
int erts_send_error_to_logger_nogl(erts_dsprintf_buf_t *);
int erts_send_info_to_logger_str_nogl(char *);
/* needed by erl_smp.h (declared above)
   int erts_send_warning_to_logger_str_nogl(char *); */
int erts_send_error_to_logger_str_nogl(char *);

typedef struct preload {
    char *name;			/* Name of module */
    int  size;			/* Size of code */
    unsigned char* code;	/* Code pointer */
} Preload;


/*
 * This structure contains options to all built in drivers.
 * None of the drivers use all of the fields.
 */

typedef struct _SysDriverOpts {
    Uint ifd;			/* Input file descriptor (fd driver). */
    Uint ofd;			/* Outputfile descriptor (fd driver). */
    int packet_bytes;		/* Number of bytes in packet header. */
    int read_write;		/* Read and write bits. */
    int use_stdio;		/* Use standard I/O: TRUE or FALSE. */
    int redir_stderr;           /* Redirect stderr to stdout: TRUE/FALSE. */
    int hide_window;		/* Hide this windows (Windows). */
    int exit_status;		/* Report exit status of subprocess. */
    int overlapped_io;          /* Only has effect on windows NT et al */
    char *envir;		/* Environment of the port process, */
				/* in Windows format. */
    char **argv;                /* Argument vector in Unix'ish format. */
    char *wd;			/* Working directory. */
    unsigned spawn_type;        /* Bitfield of ERTS_SPAWN_DRIVER | 
				   ERTS_SPAWN_EXTERNAL | both*/ 
} SysDriverOpts;

extern char *erts_default_arg0;

extern char os_type[];

extern int sys_init_time(void);
extern void erts_deliver_time(void);
extern void erts_time_remaining(SysTimeval *);
extern int erts_init_time_sup(void);
extern void erts_sys_init_float(void);
extern void erts_thread_init_float(void);
extern void erts_thread_disable_fpe(void);

ERTS_GLB_INLINE int erts_block_fpe(void);
ERTS_GLB_INLINE void erts_unblock_fpe(int);

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

ERTS_GLB_INLINE int erts_block_fpe(void)
{
    return erts_sys_block_fpe();
}

ERTS_GLB_INLINE void erts_unblock_fpe(int unmasked)
{
    erts_sys_unblock_fpe(unmasked);
}

#endif /* #if ERTS_GLB_INLINE_INCL_FUNC_DEF */


/* Dynamic library/driver loading */
typedef struct {
    char* str;
}ErtsSysDdllError;
#define ERTS_SYS_DDLL_ERROR_INIT {NULL}
extern void erts_sys_ddll_free_error(ErtsSysDdllError*);
extern void erl_sys_ddll_init(void); /* to initialize mutexes etc */
extern int erts_sys_ddll_open2(char *path, void **handle, ErtsSysDdllError*);
#define erts_sys_ddll_open(P,H) erts_sys_ddll_open2(P,H,NULL)
extern int erts_sys_ddll_open_noext(char *path, void **handle, ErtsSysDdllError*);
extern int erts_sys_ddll_load_driver_init(void *handle, void **function);
extern int erts_sys_ddll_load_nif_init(void *handle, void **function,ErtsSysDdllError*);
extern int erts_sys_ddll_close2(void *handle, ErtsSysDdllError*);
#define erts_sys_ddll_close(H) erts_sys_ddll_close2(H,NULL)
extern void *erts_sys_ddll_call_init(void *function);
extern void *erts_sys_ddll_call_nif_init(void *function);
extern int erts_sys_ddll_sym2(void *handle, char *name, void **function, ErtsSysDdllError*);
#define erts_sys_ddll_sym(H,N,F) erts_sys_ddll_sym2(H,N,F,NULL)
extern char *erts_sys_ddll_error(int code);



/*
 * System interfaces for startup.
 */


#ifdef ERTS_SMP
void erts_sys_schedule_interrupt(int set);
void erts_sys_schedule_interrupt_timed(int set, long msec);
void erts_sys_main_thread(void);
#else
#define erts_sys_schedule_interrupt(Set)
#endif

extern void erts_sys_prepare_crash_dump(void);
extern void erts_sys_pre_init(void);
extern void erl_sys_init(void);
extern void erl_sys_args(int *argc, char **argv);
extern void erl_sys_schedule(int);
void sys_tty_reset(int);

int sys_max_files(void);
void sys_init_io(void);
Preload* sys_preloaded(void);
unsigned char* sys_preload_begin(Preload*);
void sys_preload_end(Preload*);
int sys_get_key(int);
void elapsed_time_both(unsigned long *ms_user, unsigned long *ms_sys, 
		       unsigned long *ms_user_diff, unsigned long *ms_sys_diff);
void wall_clock_elapsed_time_both(unsigned long *ms_total, 
				  unsigned long *ms_diff);
void get_time(int *hour, int *minute, int *second);
void get_date(int *year, int *month, int *day);
void get_localtime(int *year, int *month, int *day, 
		   int *hour, int *minute, int *second);
void get_universaltime(int *year, int *month, int *day, 
		       int *hour, int *minute, int *second);
int univ_to_local(Sint *year, Sint *month, Sint *day, 
		  Sint *hour, Sint *minute, Sint *second);
int local_to_univ(Sint *year, Sint *month, Sint *day, 
		  Sint *hour, Sint *minute, Sint *second, int isdst);
void get_now(Uint*, Uint*, Uint*);
void get_sys_now(Uint*, Uint*, Uint*);
void set_break_quit(void (*)(void), void (*)(void));

void os_flavor(char*, unsigned);
void os_version(int*, int*, int*);
void init_getenv_state(GETENV_STATE *);
char * getenv_string(GETENV_STATE *);
void fini_getenv_state(GETENV_STATE *);

/* xxxP */
void init_sys_float(void);
int sys_chars_to_double(char*, double*);
int sys_double_to_chars(double, char*);
void sys_get_pid(char *);

/* erts_sys_putenv() returns, 0 on success and a value != 0 on failure. */
int erts_sys_putenv(char *key_value, int sep_ix);
/* erts_sys_getenv() returns 0 on success (length of value string in
   *size), a value > 0 if value buffer is too small (*size is set to needed
   size), and a value < 0 on failure. */
int erts_sys_getenv(char *key, char *value, size_t *size);

/* Easier to use, but not as efficient, environment functions */
char *erts_read_env(char *key);
void erts_free_read_env(void *value);
int erts_write_env(char *key, char *value);

/* utils.c */

/* Options to sys_alloc_opt */
#define SYS_ALLOC_OPT_TRIM_THRESHOLD 0
#define SYS_ALLOC_OPT_TOP_PAD        1
#define SYS_ALLOC_OPT_MMAP_THRESHOLD 2
#define SYS_ALLOC_OPT_MMAP_MAX       3

/* Default values to sys_alloc_opt options */
#define ERTS_DEFAULT_TRIM_THRESHOLD  (128 * 1024)
#define ERTS_DEFAULT_TOP_PAD         0
#define ERTS_DEFAULT_MMAP_THRESHOLD  (128 * 1024)
#define ERTS_DEFAULT_MMAP_MAX        64

int sys_alloc_opt(int, int);

typedef struct {
  Sint trim_threshold;
  Sint top_pad;
  Sint mmap_threshold;
  Sint mmap_max;
} SysAllocStat;

void sys_alloc_stat(SysAllocStat *);

/* Block the whole system... */

#define ERTS_BS_FLG_ALLOW_GC				(((Uint32) 1) << 0)
#define ERTS_BS_FLG_ALLOW_IO				(((Uint32) 1) << 1)

/* Activities... */
typedef enum {
    ERTS_ACTIVITY_UNDEFINED,	/* Undefined activity */
    ERTS_ACTIVITY_WAIT,		/* Waiting */
    ERTS_ACTIVITY_GC,		/* Garbage collecting */
    ERTS_ACTIVITY_IO		/* I/O including message passing to erl procs */
} erts_activity_t;

#ifdef ERTS_SMP

typedef enum {
    ERTS_ACT_ERR_LEAVE_WAIT_UNLOCKED,
    ERTS_ACT_ERR_LEAVE_UNKNOWN_ACTIVITY,
    ERTS_ACT_ERR_ENTER_UNKNOWN_ACTIVITY
} erts_activity_error_t;

typedef struct {
    erts_smp_atomic32_t do_block;
    struct {
	erts_smp_atomic32_t wait;
	erts_smp_atomic32_t gc;
	erts_smp_atomic32_t io;
    } in_activity;
} erts_system_block_state_t;

extern erts_system_block_state_t erts_system_block_state;

int erts_is_system_blocked(erts_activity_t allowed_activities);
void erts_block_me(void (*prepare)(void *), void (*resume)(void *), void *arg);
void erts_register_blockable_thread(void);
void erts_unregister_blockable_thread(void);
void erts_note_activity_begin(erts_activity_t activity);
void
erts_check_block(erts_activity_t old_activity,
		 erts_activity_t new_activity,
		 int locked,
		 void (*prepare)(void *),
		 void (*resume)(void *),
		 void *arg);
void erts_block_system(Uint32 allowed_activities);
int erts_emergency_block_system(long timeout, Uint32 allowed_activities);
void erts_release_system(void);
void erts_system_block_init(void);
void erts_set_activity_error(erts_activity_error_t, char *, int);
#ifdef ERTS_ENABLE_LOCK_CHECK
void erts_lc_activity_change_begin(void);
void erts_lc_activity_change_end(void);
int erts_lc_is_blocking(void);
#define ERTS_LC_IS_BLOCKING \
  (erts_smp_pending_system_block() && erts_lc_is_blocking())
#endif
#endif

#define erts_smp_activity_begin(NACT, PRP, RSM, ARG)		\
  erts_smp_set_activity(ERTS_ACTIVITY_UNDEFINED,		\
			(NACT),					\
			0,					\
			(PRP),					\
			(RSM),					\
			(ARG),					\
			__FILE__,				\
			__LINE__)
#define erts_smp_activity_change(OACT, NACT, PRP, RSM, ARG)	\
  erts_smp_set_activity((OACT),					\
			(NACT),					\
			0,					\
			(PRP),					\
			(RSM),					\
			(ARG),					\
			__FILE__,				\
			__LINE__)
#define erts_smp_activity_end(OACT, PRP, RSM, ARG)		\
  erts_smp_set_activity((OACT),					\
			ERTS_ACTIVITY_UNDEFINED,		\
			0,					\
			(PRP),					\
			(RSM),					\
			(ARG),					\
			__FILE__,				\
			__LINE__)

#define erts_smp_locked_activity_begin(NACT)			\
  erts_smp_set_activity(ERTS_ACTIVITY_UNDEFINED,		\
			(NACT),					\
			1,					\
			NULL,					\
			NULL,					\
			NULL,					\
			__FILE__,				\
			__LINE__)
#define erts_smp_locked_activity_change(OACT, NACT)		\
  erts_smp_set_activity((OACT),					\
			(NACT),					\
			1,					\
			NULL,					\
			NULL,					\
			NULL,					\
			__FILE__,				\
			__LINE__)
#define erts_smp_locked_activity_end(OACT)			\
  erts_smp_set_activity((OACT),					\
			ERTS_ACTIVITY_UNDEFINED,		\
			1,					\
			NULL,					\
			NULL,					\
			NULL,					\
			__FILE__,				\
			__LINE__)


ERTS_GLB_INLINE int erts_smp_is_system_blocked(erts_activity_t allowed_activities);
ERTS_GLB_INLINE void erts_smp_block_system(Uint32 allowed_activities);
ERTS_GLB_INLINE int erts_smp_emergency_block_system(long timeout,
						    Uint32 allowed_activities);
ERTS_GLB_INLINE void erts_smp_release_system(void);
ERTS_GLB_INLINE int erts_smp_pending_system_block(void);
ERTS_GLB_INLINE void erts_smp_chk_system_block(void (*prepare)(void *),
					       void (*resume)(void *),
					       void *arg);
ERTS_GLB_INLINE void
erts_smp_set_activity(erts_activity_t old_activity,
		      erts_activity_t new_activity,
		      int locked,
		      void (*prepare)(void *),
		      void (*resume)(void *),
		      void *arg,
		      char *file,
		      int line);

#if ERTS_GLB_INLINE_INCL_FUNC_DEF


ERTS_GLB_INLINE int
erts_smp_is_system_blocked(erts_activity_t allowed_activities)
{
#ifdef ERTS_SMP
    return erts_is_system_blocked(allowed_activities);
#else
    return 1;
#endif
}

ERTS_GLB_INLINE void
erts_smp_block_system(Uint32 allowed_activities)
{
#ifdef ERTS_SMP
    erts_block_system(allowed_activities);
#endif
}

ERTS_GLB_INLINE int
erts_smp_emergency_block_system(long timeout, Uint32 allowed_activities)
{
#ifdef ERTS_SMP
    return erts_emergency_block_system(timeout, allowed_activities);
#else
    return 0;
#endif
}

ERTS_GLB_INLINE void
erts_smp_release_system(void)
{
#ifdef ERTS_SMP
    erts_release_system();
#endif
}

ERTS_GLB_INLINE int
erts_smp_pending_system_block(void)
{
#ifdef ERTS_SMP
    return (int) erts_smp_atomic32_read_nob(&erts_system_block_state.do_block);
#else
    return 0;
#endif
}


ERTS_GLB_INLINE void
erts_smp_chk_system_block(void (*prepare)(void *),
			  void (*resume)(void *),
			  void *arg)
{
#ifdef ERTS_SMP
    if (erts_smp_pending_system_block())
	erts_block_me(prepare, resume, arg);
#endif
}

ERTS_GLB_INLINE void
erts_smp_set_activity(erts_activity_t old_activity,
		      erts_activity_t new_activity,
		      int locked,
		      void (*prepare)(void *),
		      void (*resume)(void *),
		      void *arg,
		      char *file,
		      int line)
{
#ifdef ERTS_SMP
#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_lc_activity_change_begin();
#endif
    switch (old_activity) {
    case ERTS_ACTIVITY_UNDEFINED:
	break;
    case ERTS_ACTIVITY_WAIT:
	erts_smp_atomic32_dec_acqb(&erts_system_block_state.in_activity.wait);
	if (locked) {
	    /* You are not allowed to leave activity waiting
	     * without supplying the possibility to block
	     * unlocked.
	     */
	    erts_set_activity_error(ERTS_ACT_ERR_LEAVE_WAIT_UNLOCKED,
				    file, line);
	}
	break;
    case ERTS_ACTIVITY_GC:
	erts_smp_atomic32_dec_acqb(&erts_system_block_state.in_activity.gc);
	break;
    case ERTS_ACTIVITY_IO:
	erts_smp_atomic32_dec_acqb(&erts_system_block_state.in_activity.io);
	break;
    default:
	erts_set_activity_error(ERTS_ACT_ERR_LEAVE_UNKNOWN_ACTIVITY,
				file, line);
	break;
    }

    /* We are not allowed to block when going to activity waiting... */
    if (new_activity != ERTS_ACTIVITY_WAIT && erts_smp_pending_system_block())
	erts_check_block(old_activity,new_activity,locked,prepare,resume,arg);

    switch (new_activity) {
    case ERTS_ACTIVITY_UNDEFINED:
	break;
    case ERTS_ACTIVITY_WAIT:
	erts_smp_atomic32_inc_mb(&erts_system_block_state.in_activity.wait);
	break;
    case ERTS_ACTIVITY_GC:
	erts_smp_atomic32_inc_mb(&erts_system_block_state.in_activity.gc);
	break;
    case ERTS_ACTIVITY_IO:
	erts_smp_atomic32_inc_mb(&erts_system_block_state.in_activity.io);
	break;
    default:
	erts_set_activity_error(ERTS_ACT_ERR_ENTER_UNKNOWN_ACTIVITY,
				file, line);
	break;
    }

    switch (new_activity) {
    case ERTS_ACTIVITY_WAIT:
    case ERTS_ACTIVITY_GC:
    case ERTS_ACTIVITY_IO:
	if (erts_smp_pending_system_block())
	    erts_note_activity_begin(new_activity);
	break;
    default:
	break;
    }

#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_lc_activity_change_end();
#endif

#endif
}

#endif /* #if ERTS_GLB_INLINE_INCL_FUNC_DEF */

#if defined(DEBUG) || defined(ERTS_ENABLE_LOCK_CHECK)
#undef ERTS_REFC_DEBUG
#define ERTS_REFC_DEBUG
#endif

typedef erts_smp_atomic_t erts_refc_t;

ERTS_GLB_INLINE void erts_refc_init(erts_refc_t *refcp, erts_aint_t val);
ERTS_GLB_INLINE void erts_refc_inc(erts_refc_t *refcp, erts_aint_t min_val);
ERTS_GLB_INLINE erts_aint_t erts_refc_inctest(erts_refc_t *refcp,
					      erts_aint_t min_val);
ERTS_GLB_INLINE void erts_refc_dec(erts_refc_t *refcp, erts_aint_t min_val);
ERTS_GLB_INLINE erts_aint_t erts_refc_dectest(erts_refc_t *refcp,
					      erts_aint_t min_val);
ERTS_GLB_INLINE void erts_refc_add(erts_refc_t *refcp, erts_aint_t diff,
				   erts_aint_t min_val);
ERTS_GLB_INLINE erts_aint_t erts_refc_read(erts_refc_t *refcp,
					   erts_aint_t min_val);

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

ERTS_GLB_INLINE void
erts_refc_init(erts_refc_t *refcp, erts_aint_t val)
{
    erts_smp_atomic_init_nob((erts_smp_atomic_t *) refcp, val);
}

ERTS_GLB_INLINE void
erts_refc_inc(erts_refc_t *refcp, erts_aint_t min_val)
{
#ifdef ERTS_REFC_DEBUG
    erts_aint_t val = erts_smp_atomic_inc_read_nob((erts_smp_atomic_t *) refcp);
    if (val < min_val)
	erl_exit(ERTS_ABORT_EXIT,
		 "erts_refc_inc(): Bad refc found (refc=%ld < %ld)!\n",
		 val, min_val);
#else
    erts_smp_atomic_inc_nob((erts_smp_atomic_t *) refcp);
#endif
}

ERTS_GLB_INLINE erts_aint_t
erts_refc_inctest(erts_refc_t *refcp, erts_aint_t min_val)
{
    erts_aint_t val = erts_smp_atomic_inc_read_nob((erts_smp_atomic_t *) refcp);
#ifdef ERTS_REFC_DEBUG
    if (val < min_val)
	erl_exit(ERTS_ABORT_EXIT,
		 "erts_refc_inctest(): Bad refc found (refc=%ld < %ld)!\n",
		 val, min_val);
#endif
    return val;
}

ERTS_GLB_INLINE void
erts_refc_dec(erts_refc_t *refcp, erts_aint_t min_val)
{
#ifdef ERTS_REFC_DEBUG
    erts_aint_t val = erts_smp_atomic_dec_read_nob((erts_smp_atomic_t *) refcp);
    if (val < min_val)
	erl_exit(ERTS_ABORT_EXIT,
		 "erts_refc_dec(): Bad refc found (refc=%ld < %ld)!\n",
		 val, min_val);
#else
    erts_smp_atomic_dec_nob((erts_smp_atomic_t *) refcp);
#endif
}

ERTS_GLB_INLINE erts_aint_t
erts_refc_dectest(erts_refc_t *refcp, erts_aint_t min_val)
{
    erts_aint_t val = erts_smp_atomic_dec_read_nob((erts_smp_atomic_t *) refcp);
#ifdef ERTS_REFC_DEBUG
    if (val < min_val)
	erl_exit(ERTS_ABORT_EXIT,
		 "erts_refc_dectest(): Bad refc found (refc=%ld < %ld)!\n",
		 val, min_val);
#endif
    return val;
}

ERTS_GLB_INLINE void
erts_refc_add(erts_refc_t *refcp, erts_aint_t diff, erts_aint_t min_val)
{
#ifdef ERTS_REFC_DEBUG
    erts_aint_t val = erts_smp_atomic_add_read_nob((erts_smp_atomic_t *) refcp, diff);
    if (val < min_val)
	erl_exit(ERTS_ABORT_EXIT,
		 "erts_refc_add(%ld): Bad refc found (refc=%ld < %ld)!\n",
		 diff, val, min_val);
#else
    erts_smp_atomic_add_nob((erts_smp_atomic_t *) refcp, diff);
#endif
}

ERTS_GLB_INLINE erts_aint_t
erts_refc_read(erts_refc_t *refcp, erts_aint_t min_val)
{
    erts_aint_t val = erts_smp_atomic_read_nob((erts_smp_atomic_t *) refcp);
#ifdef ERTS_REFC_DEBUG
    if (val < min_val)
	erl_exit(ERTS_ABORT_EXIT,
		 "erts_refc_read(): Bad refc found (refc=%ld < %ld)!\n",
		 val, min_val);
#endif
    return val;
}

#endif /* #if ERTS_GLB_INLINE_INCL_FUNC_DEF */

#ifdef ERTS_ENABLE_KERNEL_POLL
extern int erts_use_kernel_poll;
#endif

#if defined(VXWORKS)
/* NOTE! sys_calloc2 does not exist on other 
   platforms than VxWorks and OSE */
void* sys_calloc2(Uint, Uint);
#endif /* VXWORKS || OSE */


#define sys_memcpy(s1,s2,n)  memcpy(s1,s2,n)
#define sys_memmove(s1,s2,n) memmove(s1,s2,n)
#define sys_memcmp(s1,s2,n)  memcmp(s1,s2,n)
#define sys_memset(s,c,n)    memset(s,c,n)
#define sys_memzero(s, n)    memset(s,'\0',n)
#define sys_strcmp(s1,s2)    strcmp(s1,s2)
#define sys_strncmp(s1,s2,n) strncmp(s1,s2,n)
#define sys_strcpy(s1,s2)    strcpy(s1,s2)
#define sys_strncpy(s1,s2,n) strncpy(s1,s2,n)
#define sys_strlen(s)        strlen(s)

/* define function symbols (needed in sys_drv_api) */
#define sys_fp_alloc     sys_alloc
#define sys_fp_realloc   sys_realloc
#define sys_fp_free      sys_free
#define sys_fp_memcpy    memcpy
#define sys_fp_memmove   memmove
#define sys_fp_memcmp    memcmp
#define sys_fp_memset    memset
/* #define sys_fp_memzero    elib_memzero */
#define sys_fp_strcmp    strcmp
#define sys_fp_strncmp   strncmp
#define sys_fp_strcpy    strcpy
#define sys_fp_strncpy   strncpy
#define sys_fp_strlen    strlen


/* Return codes from the nb_read and nb_write functions */
#define FD_READY 1
#define FD_CONTINUE 2
#define FD_ERROR 3



/* Standard set of integer macros  .. */

#define get_int64(s) (((Uint64)(((unsigned char*) (s))[0]) << 56) | \
                      (((Uint64)((unsigned char*) (s))[1]) << 48) | \
                      (((Uint64)((unsigned char*) (s))[2]) << 40) | \
                      (((Uint64)((unsigned char*) (s))[3]) << 32) | \
                      (((Uint64)((unsigned char*) (s))[4]) << 24) | \
                      (((Uint64)((unsigned char*) (s))[5]) << 16) | \
                      (((Uint64)((unsigned char*) (s))[6]) << 8)  | \
                      (((Uint64)((unsigned char*) (s))[7])))

#define put_int64(i, s) do {((char*)(s))[0] = (char)((Sint64)(i) >> 56) & 0xff;\
                            ((char*)(s))[1] = (char)((Sint64)(i) >> 48) & 0xff;\
                            ((char*)(s))[2] = (char)((Sint64)(i) >> 40) & 0xff;\
                            ((char*)(s))[3] = (char)((Sint64)(i) >> 32) & 0xff;\
                            ((char*)(s))[4] = (char)((Sint64)(i) >> 24) & 0xff;\
                            ((char*)(s))[5] = (char)((Sint64)(i) >> 16) & 0xff;\
                            ((char*)(s))[6] = (char)((Sint64)(i) >> 8)  & 0xff;\
                            ((char*)(s))[7] = (char)((Sint64)(i))       & 0xff;\
                           } while (0) 

#define get_int32(s) ((((unsigned char*) (s))[0] << 24) | \
                      (((unsigned char*) (s))[1] << 16) | \
                      (((unsigned char*) (s))[2] << 8)  | \
                      (((unsigned char*) (s))[3]))

#define put_int32(i, s) do {((char*)(s))[0] = (char)((i) >> 24) & 0xff;   \
                            ((char*)(s))[1] = (char)((i) >> 16) & 0xff;   \
                            ((char*)(s))[2] = (char)((i) >> 8)  & 0xff;   \
                            ((char*)(s))[3] = (char)(i)         & 0xff;} \
                        while (0)

#define get_int24(s) ((((unsigned char*) (s))[0] << 16) | \
                      (((unsigned char*) (s))[1] << 8)  | \
                      (((unsigned char*) (s))[2]))

#define put_int24(i, s) do {((char*)(s))[0] = (char)((i) >> 16) & 0xff;  \
                            ((char*)(s))[1] = (char)((i) >> 8)  & 0xff;  \
                            ((char*)(s))[2] = (char)(i)         & 0xff;} \
                        while (0)

#define get_int16(s) ((((unsigned char*)  (s))[0] << 8) | \
                      (((unsigned char*)  (s))[1]))


#define put_int16(i, s) do {((char*)(s))[0] = (char)((i) >> 8) & 0xff;  \
                            ((char*)(s))[1] = (char)(i)        & 0xff;} \
                        while (0)

#define get_int8(s) ((((unsigned char*)  (s))[0] ))


#define put_int8(i, s) do {((unsigned char*)(s))[0] = (i) & 0xff;} while (0)


/*
 * Use DEBUGF as you would use printf, but use double parentheses:
 *
 *   DEBUGF(("Error: %s\n", error));
 *
 * The output will appear in a special console.
 */

#ifdef DEBUG
void erl_debug(char* format, ...);
void erl_bin_write(unsigned char *, int, int);

#  define DEBUGF(x) erl_debug x
#else
#  define DEBUGF(x)
#endif


#ifdef VXWORKS
/* This includes redefines of malloc etc 
   this should be done after sys_alloc, etc, above */
#  include "reclaim.h"
/*********************Malloc and friends************************
 * There is a problem with the naming of malloc and friends, 
 * malloc is used throughout sys.c and the resolver to mean save_alloc,
 * but it should actually mean either sys_alloc or sys_alloc2,
 * so the definitions from reclaim_master.h are not any
 * good, i redefine the malloc family here, although it's quite 
 * ugly, actually it would be preferrable to use the
 * names sys_alloc and so on throughout the offending code, but
 * that will be saved as an later exercise...
 * I also add an own calloc, to make the BSD resolver source happy.
 ***************************************************************/
/* Undefine malloc and friends */
#  ifdef malloc
#    undef malloc
#  endif
#  ifdef calloc
#    undef calloc
#  endif
#  ifdef realloc
#    undef realloc
#  endif
#  ifdef free
#    undef free
#  endif
/* Redefine malloc and friends */
#  define malloc sys_alloc
#  define calloc  sys_calloc
#  define realloc  sys_realloc
#  define free sys_free

#endif


#ifdef __WIN32__

void call_break_handler(void);
char* last_error(void);
char* win32_errorstr(int);


#endif

/************************************************************************
 * Find out the native filename encoding of the process (look at locale of 
 * Unix processes and just do UTF16 on windows 
 ************************************************************************/
#define ERL_FILENAME_UNKNOWN 0
#define ERL_FILENAME_LATIN1 1
#define ERL_FILENAME_UTF8 2
#define ERL_FILENAME_UTF8_MAC 3
#define ERL_FILENAME_WIN_WCHAR 4

int erts_get_native_filename_encoding(void);
/* The set function is only to be used by erl_init! */
void erts_set_user_requested_filename_encoding(int encoding); 
int erts_get_user_requested_filename_encoding(void);

void erts_init_sys_common_misc(void);

#endif

