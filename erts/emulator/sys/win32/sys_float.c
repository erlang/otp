/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1997-2009. All Rights Reserved.
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
/* Float conversions */

#include "sys.h"
#include "signal.h"

/* global variable for floating point checks, (see sys.h) */
/* Note! This is part of the interface Machine <---> sys.c */
volatile int erl_fp_exception = 0;

static void fpe_exception(int sig);

void
erts_sys_init_float(void)
{
}
void erts_thread_init_float(void)
{
}
void erts_thread_disable_fpe(void)
{
}

/*
 ** These two functions should maybe use localeconv() to pick up
 ** the current radix character, but since it is uncertain how
 ** expensive such a system call is, and since no-one has heard
 ** of other radix characters than '.' and ',' an ad-hoc 
 ** low execution time solution is used instead.
 */

int 
sys_chars_to_double(char *buf, double *fp)
{
    char *s = buf, *t, *dp;
    
    /* Robert says that something like this is what he really wanted:
     * (The [.,] radix test is NOT what Robert wanted - it was added later)
     *
     * 7 == sscanf(Tbuf, "%[+-]%[0-9][.,]%[0-9]%[eE]%[+-]%[0-9]%s", ....);
     * if (*s2 == 0 || *s3 == 0 || *s4 == 0 || *s6 == 0 || *s7)
     *   break;
     */
    
    /* Scan string to check syntax. */
    if (*s == '+' || *s == '-') s++;
    if (!isdigit(*s))		/* Leading digits. */
      return -1;
    while (isdigit(*s)) s++;
    if (*s != '.' && *s != ',')/* Decimal part. */
      return -1;
    dp = s++;			/* Remember decimal point pos just in case */
    if (!isdigit(*s))
      return -1;
    while (isdigit(*s)) s++;
    if (*s == 'e' || *s == 'E') {
	/* There is an exponent. */
	s++;
	if (*s == '+' || *s == '-') s++;
	if (!isdigit(*s))
	  return -1;
	while (isdigit(*s)) s++;
    }
    if (*s)			/* That should be it */
      return -1;
    
    errno = 0;
    *fp = strtod(buf, &t);
    if (t != s) {		/* Whole string not scanned */
	/* Try again with other radix char */
	*dp = (*dp == '.') ? ',' : '.';
	errno = 0;
	*fp = strtod(buf, &t);
	if (t != s) {		/* Whole string not scanned */
	    return -1;
	}
    }
    if (*fp < -1.0e-307 || 1.0e-307 < *fp) {
	if (errno == ERANGE) {
	    return -1;
	}
    } else {
	if (errno == ERANGE) {
	    /* Special case: Windows (at least some) regard very small 
	     * i.e non-normalized numbers as a range error for strtod().
	     * But not for atof. 
	     */
	    *fp = atof(buf);
	}
    }
    
    return 0;
}

/* 
** Convert a double to ascii format 0.dddde[+|-]ddd
** return number of characters converted
*/

int
sys_double_to_chars(double fp, char *buf)
{
    char *s = buf;
    
    (void) sprintf(buf, "%.20e", fp);
    /* Search upto decimal point */
    if (*s == '+' || *s == '-') s++;
    while (isdigit(*s)) s++;
    if (*s == ',') *s++ = '.'; /* Replace ',' with '.' */
    /* Scan to end of string */
    while (*s) s++;
    return s-buf; /* i.e strlen(buf) */
}

int
matherr(struct _exception *exc)
{
    erl_fp_exception = 1;
    DEBUGF(("FP exception (matherr) (0x%x) (%d)\n", exc->type, erl_fp_exception));
    return 1;
}

static void
fpe_exception(int sig)
{
    erl_fp_exception = 1;
    DEBUGF(("FP exception\n"));
}
