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
/*
 * Function: Some nice error routines taken from:
 *    "Advanced Programming in the UNIX Environment",
 *    by W.Richard Stevens
 *   
 *   void erl_err_sys(const char *fmt, ... ) fatal, sys-error 
 *   void erl_err_ret(const char *fmt, ... ) non-fatal, sys-error
 *   void erl_err_quit(const char *fmt, ...) fatal, non-sys-error
 *   void erl_err_msg(const char *fmt, ... ) non-fatal, non-sys-error
 */

#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>

#ifdef VRTX			/* What's VRIX? [sverkerw] */
#define __READY_EXTENSIONS__
#endif
#include <errno.h>

#if defined(VXWORKS)
#include <taskLib.h>
#include <taskVarLib.h>
#endif

#include "eidef.h"
#include "erl_interface.h"
#include "erl_error.h"

/* Forward */
static void err_doit(int, const char*, va_list); 
/*    __attribute__ ((format (printf, 2, 0)))*/

/*
 * Some thoughts on flushing stdout/stderr:
 *
 * The defaults are reasonable (linebuffered stdout, unbuffered
 * stderr). If they are in effect (the user neither knows nor cares),
 * there's no need to flush.
 *
 * If the user changes these defaults (and knows what he's doing, so
 * he knows and cares) we shouldn't surprise him by
 * second-guessing. So there's a need to not flush.
 *
 * If the user doesn't know what he's doing, he's hosed anyway.
 */

/* Fatal error related to a system call.
 * Print a message and terminate.
 */
void erl_err_sys(const char *fmt, ... )
{
  va_list ap;

  va_start(ap, fmt);
  err_doit(1, fmt, ap);
  va_end(ap);
  exit(1);
} /* erl_err_sys */

/* Nonfatal error related to a system call.
 * Print a message and return
 */
void erl_err_ret(const char *fmt, ... )
{
  va_list ap;

  va_start(ap, fmt);
  err_doit(1, fmt, ap);
  va_end(ap);
  return;
} /* erl_err_ret */

/* Nonfatal error unrelated to a system call.
 * Print a message and return
 */
void erl_err_msg(const char *fmt, ... )
{
  va_list ap;

  va_start(ap, fmt);
  err_doit(0, fmt, ap);
  va_end(ap);
  return;
} /* erl_err_msg */

/* Fatal error unrelated to a system call.
 * Print a message and terminate
 */
void erl_err_quit(const char *fmt, ... )
{
  va_list ap;

  va_start(ap, fmt);
  err_doit(0, fmt, ap);
  va_end(ap);
  exit(1);
} /* erl_err_quit */



/* 
 * For example on SunOS we don't have the ANSI C strerror.
 * 
 * maybe move to a convenince lib     [sverkerw]
 */
#ifndef HAVE_STRERROR

/* FIXME: move to configure */
/* CONFIG: probe for sys_nerr/_sys_nerr */
extern int sys_nerr;

/* CONFIG: probe for sys_errlist/_sys_errlist and maybe for const-ness */
#ifdef FREEBSD
extern const char * const sys_errlist[];
#else
extern char * sys_errlist[];
#endif

/* Should be in string.h */
/* Is supposed to return 'char *' (no const-ness in ANSI's prototype),
   but if you rewrite the returned string in place you deserve to
   lose. */
static const char *strerror(int errnum)
{
    if (errnum >= 0 && errnum < sys_nerr) {
	return sys_errlist[errnum];
    } else {
	/* Enough buffer for 64 bits of error. It should last a while. */
        /* FIXME problem for threaded ? */
	static char b[] = "(error -9223372036854775808)";
	sprintf(b, "(error %d)", errnum);
	buf[sizeof(b)-1] = '\0';
	return b;
    }
}
#endif /* !HAVE_STRERROR */


/* Print a message and return to caller.
 * Caller specifies "errnoflag".
 */
static void err_doit(int errnoflag, const char *fmt, va_list ap)
{
#ifndef NO_ERR_MSG
  int errno_save;

  errno_save = errno;

  vfprintf(stderr, fmt, ap);
  if (errnoflag)
  {
      fputs(": ", stderr);
      fputs(strerror(errno_save), stderr);
  }
  fputs("\n", stderr);
#endif

  return;
} /* err_doit */

