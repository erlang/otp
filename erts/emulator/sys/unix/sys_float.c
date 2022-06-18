/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2001-2021. All Rights Reserved.
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

#include "sys.h"
#include "global.h"
#include "erl_process.h"

void
erts_sys_init_float(void)
{
# ifdef SIGFPE
    sys_signal(SIGFPE, SIG_IGN); /* Ignore so we can test for NaN and Inf */
# endif
}

/* The following check is incorporated from the Vee machine */
    
#define ISDIGIT(d) ((d) >= '0' && (d) <= '9')

/* 
 ** Convert a double to ascii format 0.dddde[+|-]ddd
 ** return number of characters converted or -1 if error.
 **
 ** These two functions should maybe use localeconv() to pick up
 ** the current radix character, but since it is uncertain how
 ** expensive such a system call is, and since no-one has heard
 ** of other radix characters than '.' and ',' an ad-hoc 
 ** low execution time solution is used instead.
 */

int
sys_double_to_chars_ext(double fp, char *buffer, size_t buffer_size, size_t decimals)
{
    char *s = buffer;

    if (erts_snprintf(buffer, buffer_size, "%.*e", decimals, fp) >= buffer_size)
        return -1;
    /* Search up to decimal point */
    if (*s == '+' || *s == '-') s++;
    while (ISDIGIT(*s)) s++;
    if (*s == ',') *s++ = '.'; /* Replace ',' with '.' */
    /* Scan to end of string */
    while (*s) s++;
    return s-buffer; /* i.e strlen(buffer) */
}

/* Float conversion */

int
sys_chars_to_double(char* buf, double* fp)
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
    if (!ISDIGIT(*s))		/* Leading digits. */
      return -1;
    while (ISDIGIT(*s)) s++;
    if (*s != '.' && *s != ',')	/* Decimal part. */
      return -1;
    dp = s++;			/* Remember decimal point pos just in case */
    if (!ISDIGIT(*s))
      return -1;
    while (ISDIGIT(*s)) s++;
    if (*s == 'e' || *s == 'E') {
	/* There is an exponent. */
	s++;
	if (*s == '+' || *s == '-') s++;
	if (!ISDIGIT(*s))
	  return -1;
	while (ISDIGIT(*s)) s++;
    }
    if (*s)			/* That should be it */
      return -1;

    errno = 0;

    *fp = strtod(buf, &t);

    if (!erts_isfinite(*fp)) {
        return -1;
    }

    if (t != s) {		/* Whole string not scanned */
	/* Try again with other radix char */
	*dp = (*dp == '.') ? ',' : '.';
	errno = 0;
	__ERTS_FP_CHECK_INIT(fpexnp);
	*fp = strtod(buf, &t);
        if (!erts_isfinite(*fp)) {
            return -1;
        }
    }

    if (errno == ERANGE) {
	if (*fp == HUGE_VAL || *fp == -HUGE_VAL) {
	    /* overflow, should give error */
	    return -1;
	} else if (t == s && *fp == 0.0) {
	    /* This should give 0.0 - OTP-7178 */
	    errno = 0;

	} else if (*fp == 0.0) {
	    return -1;
	}
    }

    return 0;
}

#ifdef USE_MATHERR

int
matherr(struct exception *exc)
{
    return 1;
}

#endif
