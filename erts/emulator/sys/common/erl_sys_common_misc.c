/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2006-2018. All Rights Reserved.
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
static int filename_warning = ERL_FILENAME_WARNING_WARNING;
#if defined(__WIN32__) || defined(__DARWIN__)
/* Default unicode on windows and MacOS X */
static int user_filename_encoding = ERL_FILENAME_UTF8; 
#else
static int user_filename_encoding = ERL_FILENAME_UNKNOWN;
#endif
/* This controls the heuristic in printing characters in shell and w/ 
   io:format("~tp", ...) etc. */
static int printable_character_set = ERL_PRINTABLE_CHARACTERS_LATIN1;

void erts_set_user_requested_filename_encoding(int encoding, int warning)
{
    user_filename_encoding = encoding;
    filename_warning = warning;
}

int erts_get_user_requested_filename_encoding(void)
{
    return user_filename_encoding;
}

int erts_get_filename_warning_type(void)
{
    return filename_warning;
}

void erts_set_printable_characters(int range) {
    /* Not an atomic */
    printable_character_set = range;
}

int erts_get_printable_characters(void) {
    return  printable_character_set;
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
static char* find_first_trailing_zero(char* p)
{
    for (; *(p-1) == '0'; --p);
    if (*(p-1) == '.') ++p;
    return p;
}

int
sys_double_to_chars(double fp, char *buffer, size_t buffer_size)
{
    return sys_double_to_chars_ext(fp, buffer, buffer_size, SYS_DEFAULT_FLOAT_DECIMALS);
}


#if SIZEOF_LONG == 8
# define round_int64 lround
#elif SIZEOF_LONG_LONG == 8
# define round_int64 llround
#else
# error "No 64-bit integer type?"
#endif

/* Convert float to string
 *   decimals must be >= 0
 *   if compact != 0, the trailing 0's will be truncated
 */
int
sys_double_to_chars_fast(double f, char *buffer, int buffer_size, int decimals,
			 int compact)
{
    #define SYS_DOUBLE_RND_CONST 0.5
    #define FRAC_SIZE            52
    #define EXP_SIZE             11
    #define EXP_MASK             (((Uint64)1 << EXP_SIZE) - 1)
    #define MAX_DECIMALS         (sizeof(pow10v) / sizeof(pow10v[0]))
    #define FRAC_MASK            (((Uint64)1 << FRAC_SIZE) - 1)
    #define FRAC_MASK2           (((Uint64)1 << (FRAC_SIZE + 1)) - 1)
    #define MAX_FLOAT            ((Uint64)1 << (FRAC_SIZE+1))

    static const double pow10v[] = {
        1e0,  1e1,  1e2,  1e3,  1e4,  1e5,  1e6,  1e7,  1e8, 1e9,
        1e10, 1e11, 1e12, 1e13, 1e14, 1e15, 1e16, 1e17, 1e18
    };

    double af;
    Uint64 int_part, frac_part;
    int neg;
    int  has_decimals = decimals != 0;
    char *p = buffer;

    if (decimals < 0)
        return -1;

    if (f < 0) {
        neg = 1;
        af = -f;
    }
    else {
        neg = 0;
        af = f;
    }

    /* Don't bother with optimizing too large numbers or too large precision */
    if (af > MAX_FLOAT || decimals >= MAX_DECIMALS) {
        int len = erts_snprintf(buffer, buffer_size, "%.*f", decimals, f);
        char* p = buffer + len;
        if (len >= buffer_size)
            return -1;
        /* Delete trailing zeroes */
        if (compact)
            p = find_first_trailing_zero(p);
        *p = '\0';
        return p - buffer;
    }

    if (decimals) {
        double int_f = floor(af);
        double frac_f = round((af - int_f) * pow10v[decimals]);

        int_part = (Uint64)int_f;
        frac_part = (Uint64)frac_f;

        if (frac_f >= pow10v[decimals]) {
            /* rounding overflow carry into int_part */
            int_part++;
            frac_part = 0;
        }

        do {
            Uint64 n;
            if (!frac_part) {
                do {
                    *p++ = '0';
                } while (--decimals);
                break;
            }
            n = frac_part / 10;
            *p++ = (char)((frac_part - n*10) + '0');
            frac_part = n;
        } while (--decimals);

        *p++ = '.';
    }
    else
        int_part = (Uint64)round_int64(af);

    if (!int_part) {
        *p++ = '0';
    } else {
        do {
            Uint64 n = int_part / 10;
            *p++ = (char)((int_part - n*10) + '0');
            int_part = n;
        } while (int_part);
    }
    if (neg)
        *p++ = '-';

    {/* Reverse string */
        int i = 0;
        int j = p - buffer - 1;
        for ( ; i < j; i++, j--) {
            char tmp = buffer[i];
            buffer[i] = buffer[j];
            buffer[j] = tmp;
        }
    }

    /* Delete trailing zeroes */
    if (compact && has_decimals)
        p = find_first_trailing_zero(p);
    *p = '\0';
    return p - buffer;
}
