/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2006-2010. All Rights Reserved.
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
 * Darwin needs conversion!
 * http://developer.apple.com/library/mac/#qa/qa2001/qa1235.html
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "global.h"

#if defined(__APPLE__) && defined(__MACH__) && !defined(__DARWIN__)
#define __DARWIN__ 1
#endif

#if !defined(__WIN32__)
#include <locale.h>
#if !defined(HAVE_SETLOCALE) || !defined(HAVE_NL_LANGINFO) || !defined(HAVE_LANGINFO_H)
#define PRIMITIVE_UTF8_CHECK 1
#else
#include <langinfo.h>
#endif
#endif

/* Written once and only once */

static int filename_encoding = ERL_FILENAME_UNKNOWN;
#if defined(__WIN32__) || defined(__DARWIN__)
static int user_filename_encoding = ERL_FILENAME_UTF8; /* Default unicode on windows */
#else
static int user_filename_encoding = ERL_FILENAME_LATIN1;
#endif
void erts_set_user_requested_filename_encoding(int encoding)
{
    user_filename_encoding = encoding;
}

int erts_get_user_requested_filename_encoding(void)
{
    return user_filename_encoding;
}

void erts_init_sys_common_misc(void)
{
#if defined(__WIN32__)
    /* win_efile will totally fail if this is not set. */
    filename_encoding = ERL_FILENAME_WIN_WCHAR;
#else
    if (user_filename_encoding != ERL_FILENAME_UNKNOWN) {
	filename_encoding = user_filename_encoding;
    } else {
	char *l;
	filename_encoding = ERL_FILENAME_LATIN1;
#  ifdef PRIMITIVE_UTF8_CHECK
	setlocale(LC_CTYPE, "");  /* Set international environment, 
				     ignore result */
	if (((l = getenv("LC_ALL"))   && *l) ||
	    ((l = getenv("LC_CTYPE")) && *l) ||
	    ((l = getenv("LANG"))     && *l)) {
	    if (strstr(l, "UTF-8")) {
		filename_encoding = ERL_FILENAME_UTF8;
	    } 
	}
	
#  else
	l = setlocale(LC_CTYPE, "");  /* Set international environment */
	if (l != NULL) {
	    if (strcmp(nl_langinfo(CODESET), "UTF-8") == 0) {
		filename_encoding = ERL_FILENAME_UTF8;
	    }
	}
#  endif
    }
#  if defined(__DARWIN__)
    if (filename_encoding == ERL_FILENAME_UTF8) {
	filename_encoding = ERL_FILENAME_UTF8_MAC;
    }
#  endif
#endif
}

int erts_get_native_filename_encoding(void)
{
    return filename_encoding;
}

/* For internal use by sys_double_to_chars_fast() */
static char* float_first_trailing_zero(char* p)
{
    for (--p; *p == '0' && *(p-1) == '0'; --p);
    if (*(p-1) == '.') ++p;
    return p;
}

int
sys_double_to_chars(double fp, char *buffer, size_t buffer_size)
{
    return sys_double_to_chars_ext(fp, buffer, buffer_size, SYS_DEFAULT_FLOAT_DECIMALS);
}

int
sys_double_to_chars_fast(double f, char *outbuf, int maxlen, int decimals, int compact)
{
    enum {
          FRAC_SIZE  = 52
        , EXP_SIZE   = 11
        , EXP_MASK   = (1ll << EXP_SIZE) - 1
        , FRAC_MASK  = (1ll << FRAC_SIZE) - 1
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

    exp      -= EXP_MASK >> 1;
    mantissa |= (1ll << FRAC_SIZE);
    frac_part = 0;
    int_part  = 0;
    absf      = f * sign;

    /* Don't bother with optimizing too large numbers and decimals */
    if (absf > MAX_FLOAT || decimals > maxlen-17) {
        int len = erts_snprintf(outbuf, maxlen, "%.*f", decimals, f);
        if (len >= maxlen)
            return -1;
        p = outbuf + len;
        /* Delete trailing zeroes */
        if (compact)
            p = float_first_trailing_zero(outbuf + len);
        *p = '\0';
        return p - outbuf;
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
            int j = ret - i - 1;
            c = outbuf[i];
            outbuf[i] = outbuf[j];
            outbuf[j] = c;
        }
    }
    if (decimals != 0)
        *p++ = '.';

    max = maxlen - (p - outbuf) - 1 /* leave room for trailing '\0' */;
    if (max > decimals)
        max = decimals;
    for (m = 0; m < max; m++) {
        /* frac_part *= 10; */
        frac_part = (frac_part << 3) + (frac_part << 1);

        *p++ = (char)((frac_part >> (FRAC_SIZE + 1)) + '0');
        frac_part &= FRAC_MASK2;
    }

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
            if (d == '.') continue;
            if (++d != ':') {
                outbuf[pos] = d;
                break;
            }
            outbuf[pos] = '0';
        } while (--pos);
    }

    /* Delete trailing zeroes */
    if (compact && *(p - 1) == '0')
        p = float_first_trailing_zero(--p);
    *p = '\0';
    return p - outbuf;
}
