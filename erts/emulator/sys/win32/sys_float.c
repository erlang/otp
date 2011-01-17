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
sys_double_to_chars_fast(double f, char *outbuf, int maxlen, int precision, int compact)
{
    enum {
          FRAC_SIZE = 52
        , EXP_SIZE  = 11
        , EXP_MASK  = (1ll << EXP_SIZE) - 1
        , FRAC_MASK = (1ll << FRAC_SIZE) - 1
        , FRAC_MASK2 = (1ll << (FRAC_SIZE + 1)) - 1
        , MAX_FLOAT  = 1ll << (FRAC_SIZE+1)
    };

    long long mantissa, int_part, int_part2, frac_part;
    short exp;
    int sign, i, n, m, max;
    double absf;
    union { long long L; double F; } x;
    char c, *p = outbuf;
    int digit, roundup;

    x.F = f;

    exp      = (x.L >> FRAC_SIZE) & EXP_MASK;
    mantissa = x.L & FRAC_MASK;
    sign     = x.L >= 0 ? 1 : -1;
    if (exp == EXP_MASK) {
        if (mantissa == 0) {
            if (sign == -1)
                *p++ = '-';
            *p++ = 'i';
            *p++ = 'n';
            *p++ = 'f';
        } else {
            *p++ = 'n';
            *p++ = 'a';
            *p++ = 'n';
        }
        *p = '\0';
        return p - outbuf;
    }

    exp     -= EXP_MASK >> 1;
    mantissa |= (1ll << FRAC_SIZE);
    frac_part = 0;
    int_part  = 0;
    absf      = f * sign;

    /* Don't bother with optimizing too large numbers and precision */
    if (absf > MAX_FLOAT || precision > maxlen-17) {
       int len = snprintf(outbuf, maxlen, "%.*f", precision, f);
       return len;
    }

    if (exp >= FRAC_SIZE)
        int_part  = mantissa << (exp - FRAC_SIZE);
    else if (exp >= 0) {
        int_part  = mantissa >> (FRAC_SIZE - exp);
        frac_part = (mantissa << (exp + 1)) & FRAC_MASK2;
    }
    else /* if (exp < 0) */
        frac_part = (mantissa & FRAC_MASK2) >> -(exp + 1);

    if (int_part == 0) {
        if (sign == -1)
            *p++ = '-';
        *p++ = '0';
    } else {
        int ret;
        while (int_part != 0) {
            int_part2 = int_part / 10;
            *p++ = (char)(int_part - ((int_part2 << 3) + (int_part2 << 1)) + '0');
            int_part = int_part2;
        }
        if (sign == -1)
            *p++ = '-';
        /* Reverse string */
        ret = p - outbuf;
        for (i = 0, n = ret/2; i < n; i++) {
            c = outbuf[i];
            outbuf[i] = outbuf[ret - i - 1];
            outbuf[ret - i - 1] = c;
        }
    }
    if (precision != 0)
        *p++ = '.';

    max = maxlen - (p - outbuf);
    if (max > precision)
        max = precision;
    for (m = 0; m < max; m++) {
        /* frac_part *= 10; */
        frac_part = (frac_part << 3) + (frac_part << 1);

        *p++ = (char)((frac_part >> (FRAC_SIZE + 1)) + '0');
        frac_part &= FRAC_MASK2;
    }
    /* Delete ending zeroes */
    if (compact)
        for (--p; *p == '0' && *(p-1) == '0'; --p);

    roundup = 0;
    /* Rounding - look at the next digit */
    frac_part = (frac_part << 3) + (frac_part << 1);
    digit = (frac_part >> (FRAC_SIZE + 1));
    if (digit > 5)
        roundup = 1;
    else if (digit == 5) {
        frac_part &= FRAC_MASK2;
        if (frac_part != 0) roundup = 1;
    }
    if (roundup) {
        char d;
        int pos = p - outbuf - 1;
        do {
            d = outbuf[pos];
            if (d == '-') break;
            if (d == '.') { pos--; continue; }
            d++; outbuf[pos] = d;
            if (d != ':') break;
            outbuf[pos] = '0';
            pos--;
        } while (pos);
    }

    *p = '\0';
    return p - outbuf;
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
