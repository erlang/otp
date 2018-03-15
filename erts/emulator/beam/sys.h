/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1996-2017. All Rights Reserved.
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

#ifndef __SYS_H__
#define __SYS_H__

#if !defined(__GNUC__) || defined(__e2k__)
#  define ERTS_AT_LEAST_GCC_VSN__(MAJ, MIN, PL) 0
#elif !defined(__GNUC_MINOR__)
#  define ERTS_AT_LEAST_GCC_VSN__(MAJ, MIN, PL) \
  ((__GNUC__ << 24) >= (((MAJ) << 24) | ((MIN) << 12) | (PL)))
#elif !defined(__GNUC_PATCHLEVEL__)
#  define ERTS_AT_LEAST_GCC_VSN__(MAJ, MIN, PL) \
  (((__GNUC__ << 24) | (__GNUC_MINOR__ << 12)) >= (((MAJ) << 24) | ((MIN) << 12) | (PL)))
#else
#  define ERTS_AT_LEAST_GCC_VSN__(MAJ, MIN, PL) \
  (((__GNUC__ << 24) | (__GNUC_MINOR__ << 12) | __GNUC_PATCHLEVEL__) >= (((MAJ) << 24) | ((MIN) << 12) | (PL)))
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

#ifndef ERTS_FORCE_INLINE
#  if ERTS_AT_LEAST_GCC_VSN__(3,1,1)
#    define ERTS_FORCE_INLINE __inline__ __attribute__((__always_inline__))
#  elif defined(__WIN32__)
#    define ERTS_FORCE_INLINE __forceinline
#  endif
#  ifndef ERTS_FORCE_INLINE
#    define ERTS_FORCE_INLINE ERTS_INLINE
#  endif
#endif

#if defined(DEBUG) || defined(ERTS_ENABLE_LOCK_CHECK)
#  undef ERTS_CAN_INLINE
#  define ERTS_CAN_INLINE 0
#  undef ERTS_INLINE
#  define ERTS_INLINE
#endif

#if ERTS_CAN_INLINE
#define ERTS_GLB_FORCE_INLINE static ERTS_FORCE_INLINE
#define ERTS_GLB_INLINE static ERTS_INLINE
#else
#define ERTS_GLB_FORCE_INLINE
#define ERTS_GLB_INLINE
#endif

#if ERTS_CAN_INLINE || defined(ERTS_DO_INCL_GLB_INLINE_FUNC_DEF) 
#  define ERTS_GLB_INLINE_INCL_FUNC_DEF 1
#else
#  define ERTS_GLB_INLINE_INCL_FUNC_DEF 0
#endif

#if defined(VALGRIND) && !defined(NO_FPE_SIGNALS)
#  define NO_FPE_SIGNALS
#endif

#define ERTS_I64_LITERAL(X) X##LL

#define ErtsInArea(ptr,start,nbytes) \
    ((UWord)((char*)(ptr) - (char*)(start)) < (nbytes))

#define ErtsContainerStruct(ptr, type, member) \
    ((type *)((char *)(1 ? (ptr) : &((type *)0)->member) - offsetof(type, member)))

/* Use this variant when the member is an array */
#define ErtsContainerStruct_(ptr, type, memberv) \
    ((type *)((char *)(1 ? (ptr) : ((type *)0)->memberv) - offsetof(type, memberv)))

#define ErtsSizeofMember(type, member) sizeof(((type *)0)->member)

#if defined (__WIN32__)
#  include "erl_win_sys.h"
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
#define ERTS_SYS_FD_INVALID ((ErtsSysFdType) -1)
typedef int ErtsSysFdType;
#else
#ifndef ERTS_SYS_FD_INVALID
# error missing ERTS_SYS_FD_INVALID
#endif
typedef ERTS_SYS_FD_TYPE ErtsSysFdType;
#endif

#if ERTS_AT_LEAST_GCC_VSN__(2, 96, 0)
#  define ERTS_LIKELY(BOOL)   __builtin_expect((BOOL), !0)
#  define ERTS_UNLIKELY(BOOL) __builtin_expect((BOOL), 0)
#else
#  define ERTS_LIKELY(BOOL)   (BOOL)
#  define ERTS_UNLIKELY(BOOL) (BOOL)
#endif

#if ERTS_AT_LEAST_GCC_VSN__(2, 96, 0)
#if (defined(__APPLE__) && defined(__MACH__)) || defined(__DARWIN__)
#  define ERTS_WRITE_UNLIKELY(X) X __attribute__ ((section ("__DATA,ERTS_LOW_WRITE") ))
#else
#  define ERTS_WRITE_UNLIKELY(X) X __attribute__ ((section ("ERTS_LOW_WRITE") ))
#endif
#else
#  define ERTS_WRITE_UNLIKELY(X) X
#endif

/* clang may have too low __GNUC__ versions but can handle it */
#ifdef __GNUC__
#  if __GNUC__ > 4 || (__GNUC__ == 4 && __GNUC_MINOR__ > 5) || defined(__clang__)
#    define ERTS_DECLARE_DUMMY(X) X __attribute__ ((unused))
#  else
#    define ERTS_DECLARE_DUMMY(X) X
#  endif
#else
#  define ERTS_DECLARE_DUMMY(X) X
#endif

#if !defined(__func__)
#  if !defined(__STDC_VERSION__) || __STDC_VERSION__ < 199901L
#    if !defined(__GNUC__) ||  __GNUC__ < 2
#      define __func__ "[unknown_function]"
#    else
#      define __func__ __FUNCTION__
#    endif
#  endif
#endif

#define ERTS_MK_VSN_INT(Major, Minor, Build) \
    ((((Major) & 0x3ff) << 20) | (((Minor) & 0x3ff) << 10) | ((Build) & 0x3ff))

#ifndef ERTS_EXIT_AFTER_DUMP
#  define ERTS_EXIT_AFTER_DUMP exit
#endif

/* In VC++, noreturn is a declspec that has to be before the types,
 * but in GNUC it is an att ribute to be placed between return type
 * and function name, hence __decl_noreturn <types> __noreturn <function name>
 *
 * at some platforms (e.g. Android) __noreturn is defined at sys/cdef.h
 */
#if __GNUC__
#  define __decl_noreturn
#  ifndef __noreturn
#     define __noreturn __attribute__((noreturn))
#  endif
#else
#  if defined(__WIN32__) && defined(_MSC_VER)
#    define __noreturn
#    define __decl_noreturn __declspec(noreturn)
#  else
#    define __noreturn
#    define __decl_noreturn
#  endif
#endif

#define ERTS_ASSERT(e) \
    ((void) ((e) ? 1 : (erl_assert_error(#e, __func__, __FILE__, __LINE__), 0)))

__decl_noreturn void __noreturn erl_assert_error(const char* expr, const char *func,
						 const char* file, int line);

#ifdef DEBUG
#  define ASSERT(e) ERTS_ASSERT(e)
#else
#  define ASSERT(e) ((void) 1)
#endif

/* ERTS_UNDEF can be used to silence false warnings about
 * "variable may be used uninitialized" while keeping the variable
 * marked as undefined by valgrind.
 */
#ifdef VALGRIND
#  define ERTS_UNDEF(V,I)
#else
#  define ERTS_UNDEF(V,I) V = I
#endif

/*
 * Compile time assert
 * (the actual compiler error msg can be a bit confusing)
 */
#if ERTS_AT_LEAST_GCC_VSN__(3,1,1)
# define ERTS_CT_ASSERT(e) \
    do { \
	enum { compile_time_assert__ = __builtin_choose_expr((e),0,(void)0) }; \
    } while(0)
#else
# define ERTS_CT_ASSERT(e) \
    do { \
        enum { compile_time_assert__ = 1/(e) }; \
    } while (0)
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

#undef __deprecated
#if ERTS_AT_LEAST_GCC_VSN__(3, 0, 0)
#  define __deprecated __attribute__((deprecated))
#else
#  define __deprecated
#endif
#if ERTS_AT_LEAST_GCC_VSN__(3, 0, 4)
#  define erts_align_attribute(SZ) __attribute__ ((aligned (SZ)))
#else
#  define erts_align_attribute(SZ)
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
      && ((SIZEOF_VOID_P == SIZEOF_INT) || (SIZEOF_VOID_P == SIZEOF_LONG) || \
          (SIZEOF_VOID_P == SIZEOF_LONG_LONG)))
#error Cannot handle this combination of int/long/void*/size_t sizes
#endif

#if SIZEOF_VOID_P == 8
#undef  ARCH_32
#define ARCH_64
#define ERTS_SIZEOF_TERM 8
#elif SIZEOF_VOID_P == 4
#define ARCH_32
#undef  ARCH_64
#define ERTS_SIZEOF_TERM 4
#else
#error Neither 32 nor 64 bit architecture
#endif

#if SIZEOF_VOID_P != SIZEOF_SIZE_T
#error sizeof(void*) != sizeof(size_t)
#endif

#if SIZEOF_VOID_P == SIZEOF_LONG
typedef unsigned long Eterm erts_align_attribute(sizeof(long));
typedef unsigned long Uint  erts_align_attribute(sizeof(long));
typedef long          Sint  erts_align_attribute(sizeof(long));
#define SWORD_CONSTANT(Const) Const##L
#define UWORD_CONSTANT(Const) Const##UL
#define ERTS_UWORD_MAX ULONG_MAX
#define ERTS_SWORD_MAX LONG_MAX
#define ERTS_SIZEOF_ETERM SIZEOF_LONG
#define ErtsStrToSint strtol
#elif SIZEOF_VOID_P == SIZEOF_INT
typedef unsigned int Eterm erts_align_attribute(sizeof(int));
typedef unsigned int Uint  erts_align_attribute(sizeof(int));
typedef int          Sint  erts_align_attribute(sizeof(int));
#define SWORD_CONSTANT(Const) Const
#define UWORD_CONSTANT(Const) Const##U
#define ERTS_UWORD_MAX UINT_MAX
#define ERTS_SWORD_MAX INT_MAX
#define ERTS_SIZEOF_ETERM SIZEOF_INT
#define ErtsStrToSint strtol
#elif SIZEOF_VOID_P == SIZEOF_LONG_LONG
typedef unsigned long long Eterm erts_align_attribute(sizeof(long long));
typedef unsigned long long Uint  erts_align_attribute(sizeof(long long));
typedef long long          Sint  erts_align_attribute(sizeof(long long));
#define SWORD_CONSTANT(Const) Const##LL
#define UWORD_CONSTANT(Const) Const##ULL
#define ERTS_UWORD_MAX ULLONG_MAX
#define ERTS_SWORD_MAX LLONG_MAX
#define ERTS_SIZEOF_ETERM SIZEOF_LONG_LONG
#if defined(__WIN32__)
#define ErtsStrToSint _strtoi64
#else
#define ErtsStrToSint strtoll
#endif
#else
#error Found no appropriate type to use for 'Eterm', 'Uint' and 'Sint'
#endif

typedef Uint UWord;
typedef Sint SWord;
#define ERTS_UINT_MAX ERTS_UWORD_MAX

typedef UWord BeamInstr;

#ifndef HAVE_INT64
#  if SIZEOF_LONG == 8
#    define HAVE_INT64 1
typedef unsigned long Uint64;
typedef long          Sint64;
#    define ErtsStrToSint64 strtol
#  elif SIZEOF_LONG_LONG == 8
#    define HAVE_INT64 1
typedef unsigned long long Uint64;
typedef long long          Sint64;
#    define ErtsStrToSint64 strtoll
#  else
#    error "No 64-bit integer type found"
#  endif
#endif

#ifndef ERTS_UINT64_MAX
#  define ERTS_UINT64_MAX (~((Uint64) 0))
#endif
#ifndef ERTS_SINT64_MAX
#  define ERTS_SINT64_MAX ((Sint64) ((((Uint64) 1) << 63)-1))
#endif
#ifndef ERTS_SINT64_MIN
#  define ERTS_SINT64_MIN ((Sint64) ((((Uint64) 1) << 63)))
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

#ifndef ERTS_UINT32_MAX
#  define ERTS_UINT32_MAX (~((Uint32) 0))
#endif
#ifndef ERTS_SINT32_MAX
#  define ERTS_SINT32_MAX ((Sint32) ((((Uint32) 1) << 31)-1))
#endif
#ifndef ERTS_SINT32_MIN
#  define ERTS_SINT32_MIN ((Sint32) ((((Uint32) 1) << 31)))
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

#ifndef ERTS_UINT16_MAX
#  define ERTS_UINT16_MAX (~((Uint16) 0))
#endif
#ifndef ERTS_SINT16_MAX
#  define ERTS_SINT16_MAX ((Sint16) ((((Uint16) 1) << 15)-1))
#endif
#ifndef ERTS_SINT16_MIN
#  define ERTS_SINT16_MIN ((Sint16) ((((Uint16) 1) << 15)))
#endif

#if CHAR_BIT == 8
typedef unsigned char byte;
#else
#error Found no appropriate type to use for 'byte'
#endif

#if defined(ARCH_64) && !HAVE_INT64
#error 64-bit architecture, but no appropriate type to use for Uint64 and Sint64 found 
#endif

#ifdef WORDS_BIGENDIAN
#  define ERTS_HUINT_HVAL_HIGH 0
#  define ERTS_HUINT_HVAL_LOW 1
#else
#  define ERTS_HUINT_HVAL_HIGH 1
#  define ERTS_HUINT_HVAL_LOW 0
#endif
#if ERTS_SIZEOF_TERM == 8
typedef union {
    Uint val;
    Uint32 hval[2];
} HUint;
#elif ERTS_SIZEOF_TERM == 4
typedef union {
    Uint val;
    Uint16 hval[2];
} HUint;
#else
#error "Unsupported size of term"
#endif

#  define ERTS_EXTRA_DATA_ALIGN_SZ(X) \
    (((size_t) 8) - (((size_t) (X)) & ((size_t) 7)))

#include "erl_lock_check.h"

/* needed by erl_threads.h */
int erts_send_warning_to_logger_str_nogl(char *);

#include "erl_threads.h"

#ifdef ERTS_WANT_BREAK_HANDLING
extern erts_atomic32_t erts_break_requested;
#    define ERTS_BREAK_REQUESTED \
  ((int) erts_atomic32_read_nob(&erts_break_requested))
void erts_do_break_handling(void);
#endif


extern erts_atomic32_t erts_writing_erl_crash_dump;
extern erts_tsd_key_t erts_is_crash_dumping_key;
#define ERTS_SOMEONE_IS_CRASH_DUMPING \
  ((int) erts_atomic32_read_mb(&erts_writing_erl_crash_dump))
#define ERTS_IS_CRASH_DUMPING \
  ((int) (SWord) erts_tsd_get(erts_is_crash_dumping_key))

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
#  define isfinite(x) (fabs(x) != HUGE_VAL)
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
#    ifdef NB_FIONBIO		/* Old BSD */
#      include <sys/ioctl.h>
  static const int zero_value = 0, one_value = 1;
#      define SET_BLOCKING(fd)         ioctl((fd), FIONBIO, &zero_value)
#      define SET_NONBLOCKING(fd)      ioctl((fd), FIONBIO, &one_value)
#      define ERRNO_BLOCK EWOULDBLOCK
#    else /* !NB_FIONBIO */
#      include <fcntl.h>
#      ifdef NB_O_NDELAY               /* Nothing needs this? */
#        define NB_FLAG O_NDELAY
#        ifndef ERRNO_BLOCK            /* allow override (e.g. EAGAIN) via Makefile */
#          define ERRNO_BLOCK EWOULDBLOCK
#        endif
#      else  /* !NB_O_NDELAY */	/* The True Way - POSIX!:-) */
#        define NB_FLAG O_NONBLOCK
#        define ERRNO_BLOCK EAGAIN
#      endif /* !NB_O_NDELAY */
#      define SET_BLOCKING(fd)         fcntl((fd), F_SETFL, \
                                           fcntl((fd), F_GETFL, 0) & ~NB_FLAG)
#      define SET_NONBLOCKING(fd)      fcntl((fd), F_SETFL, \
                                           fcntl((fd), F_GETFL, 0) | NB_FLAG)
#    endif /* !NB_FIONBIO */
#  endif /* !__WIN32__ */
#endif /* WANT_NONBLOCKING */

__decl_noreturn void __noreturn erts_exit(int n, char*, ...);

/* Some special erts_exit() codes: */
#define ERTS_INTR_EXIT	-1		/* called from signal handler */
#define ERTS_ABORT_EXIT	-2	        /* no crash dump; only abort() */
#define ERTS_DUMP_EXIT	-3              /* crash dump; then exit() */
#define ERTS_ERROR_EXIT	-4              /* crash dump; then abort() */

#define ERTS_INTERNAL_ERROR(What) \
    erts_exit(ERTS_ABORT_EXIT, "%s:%d:%s(): Internal error: %s\n", \
	     __FILE__, __LINE__, __func__, What)

UWord erts_sys_get_page_size(void);

/* Size of misc memory allocated from system dependent code */
Uint erts_sys_misc_mem_sz(void);

/* print stuff is declared here instead of in global.h, so sys stuff won't
   have to include global.h */
#include "erl_printf.h"

/* Io constants to erts_print and erts_putc */
#define ERTS_PRINT_STDERR	((fmtfn_t)0)
#define ERTS_PRINT_STDOUT	((fmtfn_t)1)
#define ERTS_PRINT_FILE		((fmtfn_t)2)
#define ERTS_PRINT_SBUF		((fmtfn_t)3)
#define ERTS_PRINT_SNBUF	((fmtfn_t)4)
#define ERTS_PRINT_DSBUF	((fmtfn_t)5)
#define ERTS_PRINT_FD           ((fmtfn_t)6)

typedef struct {
    char *buf;
    size_t size;
} erts_print_sn_buf;

int erts_print(fmtfn_t to, void *arg, char *format, ...);	/* in utils.c */
int erts_putc(fmtfn_t to, void *arg, char);			/* in utils.c */

/* logger stuff is declared here instead of in global.h, so sys files
   won't have to include global.h */

erts_dsprintf_buf_t *erts_create_logger_dsbuf(void);
int erts_send_info_to_logger(Eterm, erts_dsprintf_buf_t *);
int erts_send_warning_to_logger(Eterm, erts_dsprintf_buf_t *);
int erts_send_error_to_logger(Eterm, erts_dsprintf_buf_t *);
int erts_send_error_term_to_logger(Eterm, erts_dsprintf_buf_t *, Eterm);
int erts_send_info_to_logger_str(Eterm, char *); 
int erts_send_warning_to_logger_str(Eterm, char *);
int erts_send_error_to_logger_str(Eterm, char *);
int erts_send_info_to_logger_nogl(erts_dsprintf_buf_t *);
int erts_send_warning_to_logger_nogl(erts_dsprintf_buf_t *);
int erts_send_error_to_logger_nogl(erts_dsprintf_buf_t *);
int erts_send_info_to_logger_str_nogl(char *);
/* needed by erl_threads.h (declared above)
   int erts_send_warning_to_logger_str_nogl(char *); */
int erts_send_error_to_logger_str_nogl(char *);

typedef struct preload {
    char *name;			/* Name of module */
    int  size;			/* Size of code */
    unsigned char* code;	/* Code pointer */
} Preload;

/*
 * ErtsTracer is either NIL, 'true' or [Mod | State]
 *
 * If set to NIL, it means no tracer.
 * If set to 'true' it means the current process' tracer.
 * If set to [Mod | State], there is a tracer.
 *  See erts_tracer_update for more details
 */
typedef Eterm ErtsTracer;

#include "erl_osenv.h"

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
    erts_osenv_t envir;		/* Environment of the port process */
    char **argv;                /* Argument vector in Unix'ish format. */
    char *wd;			/* Working directory. */
    unsigned spawn_type;        /* Bitfield of ERTS_SPAWN_DRIVER | 
				   ERTS_SPAWN_EXTERNAL | both*/ 
    int parallelism;            /* Optimize for parallelism */
} SysDriverOpts;

extern char *erts_default_arg0;

extern char os_type[];

typedef struct {
    int have_os_monotonic_time;
    int have_corrected_os_monotonic_time;
    ErtsMonotonicTime os_monotonic_time_unit;
    ErtsMonotonicTime sys_clock_resolution;
    struct {
	Uint64 resolution;
	char *func;
	char *clock_id;
	int locked_use;
	int extended;
    } os_monotonic_time_info;
    struct {
	Uint64 resolution;
	char *func;
	char *clock_id;
	int locked_use;
    } os_system_time_info;
} ErtsSysInitTimeResult;

#define ERTS_SYS_INIT_TIME_RESULT_INITER \
    {0, 0, (ErtsMonotonicTime) -1, (ErtsMonotonicTime) 1}

extern void erts_init_sys_time_sup(void);
extern void sys_init_time(ErtsSysInitTimeResult *);
extern void erts_late_sys_init_time(void);
extern void erts_deliver_time(void);
extern void erts_time_remaining(SysTimeval *);
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
extern int erts_sys_ddll_open(const char *path, void **handle, ErtsSysDdllError*);
extern int erts_sys_ddll_open_noext(char *path, void **handle, ErtsSysDdllError*);
extern int erts_sys_ddll_load_driver_init(void *handle, void **function);
extern int erts_sys_ddll_load_nif_init(void *handle, void **function,ErtsSysDdllError*);
extern int erts_sys_ddll_close2(void *handle, ErtsSysDdllError*);
#define erts_sys_ddll_close(H) erts_sys_ddll_close2(H,NULL)
extern void *erts_sys_ddll_call_init(void *function);
extern void *erts_sys_ddll_call_nif_init(void *function);
extern int erts_sys_ddll_sym2(void *handle, const char *name, void **function, ErtsSysDdllError*);
#define erts_sys_ddll_sym(H,N,F) erts_sys_ddll_sym2(H,N,F,NULL)
extern char *erts_sys_ddll_error(int code);



/*
 * System interfaces for startup.
 */
void erts_sys_main_thread(void);

extern int erts_sys_prepare_crash_dump(int secs);
extern void erts_sys_pre_init(void);
extern void erl_sys_init(void);
extern void erl_sys_late_init(void);
extern void erl_sys_args(int *argc, char **argv);
extern void erl_sys_schedule(int);
void sys_tty_reset(int);

int sys_max_files(void);
void sys_init_io(void);
Preload* sys_preloaded(void);
unsigned char* sys_preload_begin(Preload*);
void sys_preload_end(Preload*);
int sys_get_key(int);
void get_time(int *hour, int *minute, int *second);
void get_date(int *year, int *month, int *day);
void get_localtime(int *year, int *month, int *day, 
		   int *hour, int *minute, int *second);
void get_universaltime(int *year, int *month, int *day, 
		       int *hour, int *minute, int *second);
int seconds_to_univ(Sint64 seconds, 
		    Sint *year, Sint *month, Sint *day, 
		    Sint *hour, Sint *minute, Sint *second);
int univ_to_seconds(Sint year, Sint month, Sint day, 
		    Sint hour, Sint minute, Sint second,
		    Sint64* seconds);
int univ_to_local(
    Sint *year, Sint *month, Sint *day, 
		  Sint *hour, Sint *minute, Sint *second);
int local_to_univ(Sint *year, Sint *month, Sint *day, 
		  Sint *hour, Sint *minute, Sint *second, int isdst);
void get_now(Uint*, Uint*, Uint*);
struct ErtsSchedulerData_;
ErtsMonotonicTime erts_get_monotonic_time(struct ErtsSchedulerData_ *);
ErtsMonotonicTime erts_get_time_offset(void);
void
erts_make_timestamp_value(Uint* megasec, Uint* sec, Uint* microsec,
			  ErtsMonotonicTime mtime, ErtsMonotonicTime offset);
void get_sys_now(Uint*, Uint*, Uint*);
void set_break_quit(void (*)(void), void (*)(void));

void os_flavor(char*, unsigned);
void os_version(int*, int*, int*);

#define HAVE_ERTS_CHECK_IO_DEBUG
typedef struct {
    int no_used_fds;
    int no_driver_select_structs;
    int no_enif_select_structs;
} ErtsCheckIoDebugInfo;
int erts_check_io_debug(ErtsCheckIoDebugInfo *ip);

int erts_sys_is_area_readable(char *start, char *stop);

/* xxxP */
#define SYS_DEFAULT_FLOAT_DECIMALS 20
void init_sys_float(void);
int sys_chars_to_double(char*, double*);
int sys_double_to_chars(double, char*, size_t);
int sys_double_to_chars_ext(double, char*, size_t, size_t);
int sys_double_to_chars_fast(double, char*, int, int, int);
void sys_get_pid(char *, size_t);

/* erl_drv_get/putenv have been implicitly 8-bit for so long that we can't
 * change them without breaking things on Windows. Their return values are
 * identical to erts_osenv_get/putenv */
int erts_sys_explicit_8bit_getenv(char *key, char *value, size_t *size);
int erts_sys_explicit_8bit_putenv(char *key, char *value);

/* This is identical to erts_sys_explicit_8bit_getenv but falls down to the
 * host OS implementation instead of erts_osenv. */
int erts_sys_explicit_host_getenv(char *key, char *value, size_t *size);

const erts_osenv_t *erts_sys_rlock_global_osenv(void);
void erts_sys_runlock_global_osenv(void);

erts_osenv_t *erts_sys_rwlock_global_osenv(void);
void erts_sys_rwunlock_global_osenv(void);

/* Easier to use, but not as efficient, environment functions */
char *erts_read_env(char *key);
void erts_free_read_env(void *value);

#if defined(ERTS_THR_HAVE_SIG_FUNCS) &&                         \
    (!defined(ETHR_UNUSABLE_SIGUSRX) || defined(SIGRTMIN))
extern void sys_thr_resume(erts_tid_t tid);
extern void sys_thr_suspend(erts_tid_t tid);
#ifdef SIGRTMIN
#define ERTS_SYS_SUSPEND_SIGNAL (SIGRTMIN+1)
#else
#define ERTS_SYS_SUSPEND_SIGNAL (SIGUSR2)
#endif /* SIGRTMIN */
#endif /* HAVE_SIG_FUNCS */

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
  int trim_threshold;
  int top_pad;
  int mmap_threshold;
  int mmap_max;
} SysAllocStat;

void sys_alloc_stat(SysAllocStat *);

#if defined(DEBUG) || defined(ERTS_ENABLE_LOCK_CHECK)
#undef ERTS_REFC_DEBUG
#define ERTS_REFC_DEBUG
#endif

typedef erts_atomic_t erts_refc_t;

ERTS_GLB_INLINE void erts_refc_init(erts_refc_t *refcp, erts_aint_t val);
ERTS_GLB_INLINE void erts_refc_inc(erts_refc_t *refcp, erts_aint_t min_val);
ERTS_GLB_INLINE erts_aint_t erts_refc_inc_unless(erts_refc_t *refcp,
                                                 erts_aint_t unless_val,
                                                 erts_aint_t min_val);
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
    erts_atomic_init_nob((erts_atomic_t *) refcp, val);
}

ERTS_GLB_INLINE void
erts_refc_inc(erts_refc_t *refcp, erts_aint_t min_val)
{
#ifdef ERTS_REFC_DEBUG
    erts_aint_t val = erts_atomic_inc_read_nob((erts_atomic_t *) refcp);
    if (val < min_val)
	erts_exit(ERTS_ABORT_EXIT,
		 "erts_refc_inc(): Bad refc found (refc=%ld < %ld)!\n",
		 val, min_val);
#else
    erts_atomic_inc_nob((erts_atomic_t *) refcp);
#endif
}

ERTS_GLB_INLINE erts_aint_t
erts_refc_inc_unless(erts_refc_t *refcp,
                     erts_aint_t unless_val,
                     erts_aint_t min_val)
{
    erts_aint_t val = erts_atomic_read_nob((erts_atomic_t *) refcp);
    while (1) {
        erts_aint_t exp, new;
#ifdef ERTS_REFC_DEBUG
        if (val < 0)
            erts_exit(ERTS_ABORT_EXIT,
                      "erts_refc_inc_unless(): Bad refc found (refc=%ld < %ld)!\n",
                      val, min_val);
#endif
        if (val == unless_val)
            return val;
        new = val + 1;
        exp = val;
        val = erts_atomic_cmpxchg_nob((erts_atomic_t *) refcp, new, exp);
        if (val == exp)
            return new;
    }
}

ERTS_GLB_INLINE erts_aint_t
erts_refc_inctest(erts_refc_t *refcp, erts_aint_t min_val)
{
    erts_aint_t val = erts_atomic_inc_read_nob((erts_atomic_t *) refcp);
#ifdef ERTS_REFC_DEBUG
    if (val < min_val)
	erts_exit(ERTS_ABORT_EXIT,
		 "erts_refc_inctest(): Bad refc found (refc=%ld < %ld)!\n",
		 val, min_val);
#endif
    return val;
}

ERTS_GLB_INLINE void
erts_refc_dec(erts_refc_t *refcp, erts_aint_t min_val)
{
#ifdef ERTS_REFC_DEBUG
    erts_aint_t val = erts_atomic_dec_read_nob((erts_atomic_t *) refcp);
    if (val < min_val)
	erts_exit(ERTS_ABORT_EXIT,
		 "erts_refc_dec(): Bad refc found (refc=%ld < %ld)!\n",
		 val, min_val);
#else
    erts_atomic_dec_nob((erts_atomic_t *) refcp);
#endif
}

ERTS_GLB_INLINE erts_aint_t
erts_refc_dectest(erts_refc_t *refcp, erts_aint_t min_val)
{
    erts_aint_t val = erts_atomic_dec_read_nob((erts_atomic_t *) refcp);
#ifdef ERTS_REFC_DEBUG
    if (val < min_val)
	erts_exit(ERTS_ABORT_EXIT,
		 "erts_refc_dectest(): Bad refc found (refc=%ld < %ld)!\n",
		 val, min_val);
#endif
    return val;
}

ERTS_GLB_INLINE void
erts_refc_add(erts_refc_t *refcp, erts_aint_t diff, erts_aint_t min_val)
{
#ifdef ERTS_REFC_DEBUG
    erts_aint_t val = erts_atomic_add_read_nob((erts_atomic_t *) refcp, diff);
    if (val < min_val)
	erts_exit(ERTS_ABORT_EXIT,
		 "erts_refc_add(%ld): Bad refc found (refc=%ld < %ld)!\n",
		 diff, val, min_val);
#else
    erts_atomic_add_nob((erts_atomic_t *) refcp, diff);
#endif
}

ERTS_GLB_INLINE erts_aint_t
erts_refc_read(erts_refc_t *refcp, erts_aint_t min_val)
{
    erts_aint_t val = erts_atomic_read_nob((erts_atomic_t *) refcp);
#ifdef ERTS_REFC_DEBUG
    if (val < min_val)
	erts_exit(ERTS_ABORT_EXIT,
		 "erts_refc_read(): Bad refc found (refc=%ld < %ld)!\n",
		 val, min_val);
#endif
    return val;
}

#endif  /* #if ERTS_GLB_INLINE_INCL_FUNC_DEF */


/* Thin wrappers around memcpy and friends, which should always be used in
 * place of plain memcpy, memset, etc.
 *
 * Passing NULL to any of these functions is undefined behavior even though it
 * may seemingly work when the length (if any) is zero; a compiler can take
 * this as a hint that the passed operand may *never* be NULL and then optimize
 * based on that information.
 */
ERTS_GLB_INLINE void *sys_memcpy(void *dest, const void *src, size_t n);
ERTS_GLB_INLINE void *sys_memmove(void *dest, const void *src, size_t n);
ERTS_GLB_INLINE int sys_memcmp(const void *s1, const void *s2, size_t n);
ERTS_GLB_INLINE void *sys_memset(void *s, int c, size_t n);
ERTS_GLB_INLINE void *sys_memzero(void *s, size_t n);
ERTS_GLB_INLINE int sys_strcmp(const char *s1, const char *s2);
ERTS_GLB_INLINE int sys_strncmp(const char *s1, const char *s2, size_t n);
ERTS_GLB_INLINE char *sys_strcpy(char *dest, const char *src);
ERTS_GLB_INLINE char *sys_strncpy(char *dest, const char *src, size_t n);
ERTS_GLB_INLINE size_t sys_strlen(const char *s);

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

ERTS_GLB_INLINE void *sys_memcpy(void *dest, const void *src, size_t n)
{
    ASSERT(dest != NULL && src != NULL);
    return memcpy(dest,src,n);
}
ERTS_GLB_INLINE void *sys_memmove(void *dest, const void *src, size_t n)
{
    ASSERT(dest != NULL && src != NULL);
    return memmove(dest,src,n);
}
ERTS_GLB_INLINE int sys_memcmp(const void *s1, const void *s2, size_t n)
{
    ASSERT(s1 != NULL && s2 != NULL);
    return memcmp(s1,s2,n);
}
ERTS_GLB_INLINE void *sys_memset(void *s, int c, size_t n)
{
    ASSERT(s != NULL);
    return memset(s,c,n);
}
ERTS_GLB_INLINE void *sys_memzero(void *s, size_t n)
{
    ASSERT(s != NULL);
    return memset(s,'\0',n);
}
ERTS_GLB_INLINE int sys_strcmp(const char *s1, const char *s2)
{
    ASSERT(s1 != NULL && s2 != NULL);
    return strcmp(s1,s2);
}
ERTS_GLB_INLINE int sys_strncmp(const char *s1, const char *s2, size_t n)
{
    ASSERT(s1 != NULL && s2 != NULL);
    return strncmp(s1,s2,n);
}
ERTS_GLB_INLINE char *sys_strcpy(char *dest, const char *src)
{
    ASSERT(dest != NULL && src != NULL);
    return strcpy(dest,src);

}
ERTS_GLB_INLINE char *sys_strncpy(char *dest, const char *src, size_t n)
{
    ASSERT(dest != NULL && src != NULL);
    return strncpy(dest,src,n);
}
ERTS_GLB_INLINE size_t sys_strlen(const char *s)
{
    ASSERT(s != NULL);
    return strlen(s);
}
#endif /* #if ERTS_GLB_INLINE_INCL_FUNC_DEF */

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

#ifndef MAX
#define MAX(A, B) ((A) > (B) ? (A) : (B))
#endif

#ifndef MIN
#define MIN(A, B) ((A) < (B) ? (A) : (B))
#endif

#ifdef __WIN32__
#ifdef ARCH_64
#define ERTS_ALLOC_ALIGN_BYTES 16
#define ERTS_SMALL_ABS(Small) _abs64(Small) 
#else
#define ERTS_ALLOC_ALIGN_BYTES 8
#define ERTS_SMALL_ABS(Small) labs(Small) 
#endif
#else
#define ERTS_ALLOC_ALIGN_BYTES 8
#define ERTS_SMALL_ABS(Small) labs(Small) 
#endif

#ifndef ERTS_HAVE_ERTS_SYS_ALIGNED_ALLOC
#  define ERTS_HAVE_ERTS_SYS_ALIGNED_ALLOC 0
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
#define ERL_FILENAME_UNKNOWN   (0)
#define ERL_FILENAME_LATIN1    (1)
#define ERL_FILENAME_UTF8      (2)
#define ERL_FILENAME_UTF8_MAC  (3)
#define ERL_FILENAME_WIN_WCHAR (4)

/************************************************************************
 * If a filename in for example list_dir is not in the right encoding, it
 * will be skipped in the resulting list, but depending on a startup setting
 * we will inform the user in different ways. These macros define the
 * different reactions to wrongly coded filenames. In the error case an
 * exception will be thrown by prim_file.
 ************************************************************************/
#define ERL_FILENAME_WARNING_WARNING (0)
#define ERL_FILENAME_WARNING_IGNORE (1)
#define ERL_FILENAME_WARNING_ERROR (2)

/***********************************************************************
 * The user can request a range of character that he/she consider
 * printable. Currently this can be either latin1 or unicode, but
 * in the future a set of ranges, or languages, could be specified.
 ***********************************************************************/
#define ERL_PRINTABLE_CHARACTERS_LATIN1 (0)
#define ERL_PRINTABLE_CHARACTERS_UNICODE (1)

int erts_get_native_filename_encoding(void);
/* The set function is only to be used by erl_init! */
void erts_set_user_requested_filename_encoding(int encoding, int warning);
int erts_get_user_requested_filename_encoding(void);
int erts_get_filename_warning_type(void);
/* This function is called from erl_init. The setting is read by BIF's 
   in io/io_lib. Setting is not atomic. */
void erts_set_printable_characters(int range);
/* Get the setting (ERL_PRINTABLE_CHARACTERS_{LATIN1|UNICODE} */
int erts_get_printable_characters(void);

void erts_init_sys_common_misc(void);

ERTS_GLB_INLINE Sint erts_raw_env_7bit_ascii_char_need(int encoding);
ERTS_GLB_INLINE byte *erts_raw_env_7bit_ascii_char_put(byte c, byte *p,
                                                       int encoding);
ERTS_GLB_INLINE int  erts_raw_env_char_is_7bit_ascii_char(byte c, byte *p,
                                                          int encoding);
ERTS_GLB_INLINE byte *erts_raw_env_next_char(byte *p, int encoding);

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

ERTS_GLB_INLINE Sint
erts_raw_env_7bit_ascii_char_need(int encoding)
{
    return (encoding == ERL_FILENAME_WIN_WCHAR) ? 2 : 1;
}

ERTS_GLB_INLINE byte *
erts_raw_env_7bit_ascii_char_put(byte c,
                                 byte *p,
                                 int encoding)
{
    *(p++) = c;
    if (encoding == ERL_FILENAME_WIN_WCHAR)
        *(p++) = 0;
    return p;
}

ERTS_GLB_INLINE int
erts_raw_env_char_is_7bit_ascii_char(byte c,
                                     byte *p,
                                     int encoding)
{
    if (encoding == ERL_FILENAME_WIN_WCHAR)
        return (p[0] == c) & (p[1] == 0);
    else
        return p[0] == c;
}

ERTS_GLB_INLINE byte *
erts_raw_env_next_char(byte *p, int encoding)
{
    if (encoding == ERL_FILENAME_WIN_WCHAR)
        return p + 2;
    else
        return p + 1;
}

#endif /* #if ERTS_GLB_INLINE_INCL_FUNC_DEF */

#endif
