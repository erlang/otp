/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2005-2016. All Rights Reserved.
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
 * Description: Test suite for the ethread thread library.
 * Author: Rickard Green
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <limits.h>
#include <float.h>
#ifndef __WIN32__
#include <unistd.h>
#endif

#include "erl_printf.h"
#ifdef THREAD_SAFE
#include "ethread.h"
#endif

#ifdef __WIN32__
#undef HAVE_VSNPRINTF
#define HAVE_VSNPRINTF 1
#define vsnprintf _vsnprintf
#endif

#ifdef __WIN32__
#define signed_long_long	__int64
#define unsigned_long_long	unsigned __int64
#else
#define signed_long_long	signed long long
#define unsigned_long_long	unsigned long long
#endif

/*
 * Auxiliary functions
 */

#define PRINT_VA_LIST(FRMT)						\
do {									\
    if (FRMT && FRMT != '\0') {						\
	va_list args;							\
	va_start(args, FRMT);						\
	vfprintf(stderr, FRMT, args);					\
	va_end(args);							\
    }									\
} while (0)

#define ASSERT(B)							\
do {									\
    if (!(B))								\
	fail("%s:%d: Assertion \"%s\" failed!",__FILE__,__LINE__,#B);	\
} while (0)

static void
print_eol(void)
{
    fprintf(stderr, "\n");
}

static void print_line(char *frmt,...)
{
    PRINT_VA_LIST(frmt);
    print_eol();
}

static void print(char *frmt,...)
{
    PRINT_VA_LIST(frmt);
}

static void fail(char *frmt,...)
{
    char *abrt_env;
    print_eol();
    fprintf(stderr, "EP-TEST-FAILURE");
    PRINT_VA_LIST(frmt);
    print_eol();
    abrt_env = getenv("ERL_ABORT_ON_FAILURE");
    if (abrt_env && strcmp("true", abrt_env) == 0)
	abort();
    else
	exit(1);
}

static void skip(char *frmt,...)
{
    print_eol();
    fprintf(stderr, "EP-TEST-SKIP");
    PRINT_VA_LIST(frmt);
    print_eol();
    exit(0);
}

static void succeed(char *frmt,...)
{
    print_eol();
    fprintf(stderr, "EP-TEST-SUCCESS");
    PRINT_VA_LIST(frmt);
    print_eol();
    exit(0);
}

#if 0 /* Not used */

static void
do_sleep(unsigned secs)
{
#ifdef __WIN32__
	Sleep((DWORD) secs*1000);
#else
	sleep(secs);
#endif
}

#endif

static void
send_my_pid(void)
{
#ifndef __WIN32__
    int pid = (int) getpid();
    fprintf(stderr, "\nEP-TEST-PID%d\n", pid);
#endif
}

#define BUF_SIZE (1024*1024)

FILE *outfile = NULL;
char **expected_result;

#define FENCE_SIZE 512
static void
print_cmp_test(int n, char *frmt, ...)
{
    int res = -1;
    static char clib_buf[BUF_SIZE];
    static unsigned char the_erts_buf[BUF_SIZE];
    char *erts_buf = (char *) &the_erts_buf[FENCE_SIZE]; 
    va_list args;

    if (outfile) {
	char *fp, *tp;
	va_start(args, frmt);
	if (n < 0)
	    res = vsprintf(erts_buf, frmt, args);
	else {
#ifdef HAVE_VSNPRINTF
	    res = vsnprintf(erts_buf, (size_t) n, frmt, args);
#else
	    fail("No vsnprintf()");
#endif
	}
	va_end(args);
	ASSERT(res >= 0);
	fp = erts_buf;
	tp = clib_buf;
	while (*fp) {
	    switch (*fp) {
	    case '\a':	*(tp++) = '\\';	*(tp++) = 'a';	break;
	    case '\b':	*(tp++) = '\\';	*(tp++) = 'b';	break;
	    case '\f':	*(tp++) = '\\';	*(tp++) = 'f';	break;
	    case '\n':	*(tp++) = '\\';	*(tp++) = 'n';	break;
	    case '\r':	*(tp++) = '\\';	*(tp++) = 'r';	break;
	    case '\t':	*(tp++) = '\\';	*(tp++) = 't';	break;
	    case '\v':	*(tp++) = '\\';	*(tp++) = 'v';	break;
	    case '\"':	*(tp++) = '\\';	*(tp++) = '\"';	break;
	    case '\\':	*(tp++) = '\\';	*(tp++) = '\\';	break;
	    default:	*(tp++) = *fp;			break;
	    }
	    fp++;
	}
	*tp = '\0';
	res = fprintf(outfile, "\t\"%s\",\n", clib_buf);
	ASSERT(res >= 0);
    }
    else {
	char *xres;
	va_start(args, frmt);
	if (n < 0)
	    res = erts_vsprintf(erts_buf, frmt, args);
	else {
	    int i;
	    int chk_sz = 2*FENCE_SIZE + n;
	    for (i = 0; i < chk_sz; i++)
		the_erts_buf[i] = 0xeb;
	    res = erts_vsnprintf(erts_buf, (size_t) n, frmt, args);
	    for (i = 0; i < chk_sz; i++)
		if ((((char *) &the_erts_buf[i]) < erts_buf
		     || erts_buf + n <= ((char *) &the_erts_buf[i]))
		    && the_erts_buf[i] != 0xeb) {
		    int j;
		    for (j = 0; j < chk_sz; j++)
			print(j ? ",%x(%d)" : "%x(%d)",
			      (unsigned) the_erts_buf[j], j - FENCE_SIZE);
		    print_eol();
		    fail("Garbage written out of bounds (%d,%d)",
			 i - FENCE_SIZE, n);
		}
	}
	va_end(args);
	ASSERT(res >= 0);

	if (expected_result) {
	    ASSERT(*expected_result);
	    xres = *expected_result;
	    expected_result++;
	}
	else {
	    va_start(args, frmt);
	    if (n < 0)
		res = vsprintf(clib_buf, frmt, args);
	    else {
#ifdef HAVE_VSNPRINTF
		res = vsnprintf(clib_buf, (size_t) n, frmt, args);
#else
		fail("No vsnprintf()");
#endif
	    }
	    va_end(args);
	    ASSERT(res >= 0);
	    xres = clib_buf;
	}

	if (strcmp(xres, erts_buf) != 0) {
	    print_line("expected result : \"%s\"", xres);
	    print_line("erts_buf        : \"%s\"", erts_buf);
	    fail("\"%s\" != \"%s\" (format=\"%s\")", xres, erts_buf, frmt);
	}

	print_line("Checked format \"%s\" with result: \"%s\"", frmt, erts_buf);
    }
}

/*
 * The test-cases
 */

#include "integer_64_test.h"
#include "integer_test.h"

#define INT_SUB_BATCH_TEST(FRMT, TYPE)				\
    print_cmp_test(-1, FRMT, ((TYPE) 4711));			\
    print_cmp_test(-1, FRMT, ~((TYPE) 4711));			\
    print_cmp_test(-1, FRMT, (~((TYPE) 0))/2 + (~((TYPE) 0))/4);\
    print_cmp_test(-1, FRMT, ((TYPE) - 1));			\
    print_cmp_test(-1, FRMT, ((TYPE) 1));			\
    print_cmp_test(-1, FRMT, ((TYPE) ((long) 0xabcdef01)));	\

#define INT_BATCH_TEST(P, X, S)					\
    print_line("%s:%d",__FILE__,__LINE__);			\
    INT_SUB_BATCH_TEST("%" P "h" X, S char);			\
    INT_SUB_BATCH_TEST("%" P "h" X, S short);			\
    INT_SUB_BATCH_TEST("%" P X, S int);				\
    INT_SUB_BATCH_TEST("%" P "l" X, S long);			\
    INT_SUB_BATCH_TEST("%" P "ll" X, S ## _long_long);		\

static void
integer_test(void)
{
    /* This testcase should be rewritten. It assumes the following
       sizes of types... */
    if (sizeof(char) != 1
	|| sizeof(short) != 2
	|| sizeof(int) != 4
	|| sizeof(long) != (sizeof(void *) == 8 ? 8 : 4)
	|| sizeof(signed_long_long) != 8)
	skip("Unexpected size of primitive datatype:"
	     " sizeof(char) == %d (expected 1);"
	     " sizeof(short) == %d (expected 2);"
	     " sizeof(int) == %d (expected 4);"
	     " sizeof(long) == %d (expected %d);"
	     " sizeof(signed_long_long) == %d (expected 8)",
	     sizeof(char),
	     sizeof(short),
	     sizeof(int),
	     sizeof(long), sizeof(void *) == 8 ? 8 : 4,
	     sizeof(signed_long_long));

    expected_result = (sizeof(void *) == 8
		       ? integer_64_expected_result
		       : integer_expected_result);

    INT_BATCH_TEST("", "i", signed);
    INT_BATCH_TEST("", "d", signed);
    INT_BATCH_TEST("", "u", unsigned);
    INT_BATCH_TEST("", "o", unsigned);
    INT_BATCH_TEST("", "x", unsigned);
    INT_BATCH_TEST("", "X", unsigned);
    INT_BATCH_TEST("010.5", "i", signed);
    INT_BATCH_TEST("010.5", "d", signed);
    INT_BATCH_TEST("010.5", "u", unsigned);
    INT_BATCH_TEST("010.5", "o", unsigned);
    INT_BATCH_TEST("010.5", "x", unsigned);
    INT_BATCH_TEST("010.5", "X", unsigned);
    INT_BATCH_TEST("-+29", "i", signed);
    INT_BATCH_TEST("-+29", "d", signed);
    INT_BATCH_TEST("-29", "u", unsigned);
    INT_BATCH_TEST("-29", "o", unsigned);
    INT_BATCH_TEST("-29", "x", unsigned);
    INT_BATCH_TEST("-29", "X", unsigned);
    INT_BATCH_TEST("22.8", "i", signed);
    INT_BATCH_TEST("22.8", "d", signed);
    INT_BATCH_TEST("22.8", "u", unsigned);
    INT_BATCH_TEST("22.8", "o", unsigned);
    INT_BATCH_TEST("22.8", "x", unsigned);
    INT_BATCH_TEST("22.8", "X", unsigned);
    INT_BATCH_TEST("-22.8", "i", signed);
    INT_BATCH_TEST("-22.8", "d", signed);
    INT_BATCH_TEST("-22.8", "u", unsigned);
    INT_BATCH_TEST("-22.8", "o", unsigned);
    INT_BATCH_TEST("-22.8", "x", unsigned);
    INT_BATCH_TEST("-22.8", "X", unsigned);
    INT_BATCH_TEST("-823.193", "i", signed);
    INT_BATCH_TEST("-823.193", "d", signed);
    INT_BATCH_TEST("-823.193", "u", unsigned);
    INT_BATCH_TEST("-823.193", "o", unsigned);
    INT_BATCH_TEST("-823.193", "x", unsigned);
    INT_BATCH_TEST("-823.193", "X", unsigned);

}

static void
float_test(void)
{
    expected_result = NULL;
    print_cmp_test(-1, "%70.10f", DBL_MAX);
    print_cmp_test(-1, "%500.10f", DBL_MAX);
    print_cmp_test(-1, "%-500.10f", DBL_MAX);
    print_cmp_test(-1, "%500.10e", DBL_MAX);
    print_cmp_test(-1, "%-500.10e", DBL_MAX);
    print_cmp_test(-1, "%500.10E", DBL_MAX);
    print_cmp_test(-1, "%-500.10E", DBL_MAX);
    print_cmp_test(-1, "%500.10g", DBL_MAX);
    print_cmp_test(-1, "%-500.10g", DBL_MAX);
    print_cmp_test(-1, "%500.10G", DBL_MAX);
    print_cmp_test(-1, "%-500.10G", DBL_MAX);
}

char some_characters[] =
"abcdefghijklmnopqrstuvwxyzåäö"
"ABCDEFGHIJKLMNOPQRSTUVXYZÅÄÖ"
"1234567890"
"()[]{}+-;,:.@£$!\"#¤%&/\\=?'`´^~§½|<>¨*_"
"\a\b\f\n\r\t\v";

#include "string_test.h"

static void
string_test(void)
{
    expected_result = string_expected_result;
    print_cmp_test(-1, "%s", "hej");
    print_cmp_test(-1, "%-10.5s", "hopp");
    print_cmp_test(-1, "%10.5s", "hopp");
    print_cmp_test(-1, "%-500.500s", "hopp");
    print_cmp_test(-1, "%500.500s", "hopp");
    print_cmp_test(-1, "\t%10.4s", some_characters);
    print_cmp_test(-1, "\t%500.500s", some_characters);
}

#include "character_test.h"

static void
character_test(void)
{
    char *cp;
    expected_result = character_expected_result;
    for (cp = some_characters; *cp; cp++) {
	print_cmp_test(-1, "%c", *cp);
	print_cmp_test(-1, "%-10.5c",  *cp);
	print_cmp_test(-1, "%10.5c",  *cp);
	print_cmp_test(-1, "%-500.500c",  *cp);
	print_cmp_test(-1, "%500.500c",  *cp);
    }
}

#include "snprintf_test.h"

static void
snprintf_test(void)
{
    expected_result = snprintf_expected_result;
    print_cmp_test(6, "hej hopp");
    print_cmp_test(7, "hej hopp");
    print_cmp_test(8, "hej hopp");
    print_cmp_test(9, "hej hopp");
    print_cmp_test(10, "hej hopp");
    print_cmp_test(6, "hej %d", 4711);
    print_cmp_test(7, "hej %d", 4711);
    print_cmp_test(8, "hej %d", 4711);
    print_cmp_test(9, "hej %d", 4711);
    print_cmp_test(10, "hej %d", 4711);
    print_cmp_test(sizeof(some_characters)-2, "%s", some_characters);
    print_cmp_test(sizeof(some_characters)-1, "%s", some_characters);
    print_cmp_test(sizeof(some_characters), "%s", some_characters);
    print_cmp_test(sizeof(some_characters)+1, "%s", some_characters);
    print_cmp_test(sizeof(some_characters)+2, "%s", some_characters);
    print_cmp_test(sizeof(some_characters)/2, "%s%s",
		   some_characters, some_characters);
    print_cmp_test(sizeof(some_characters)*3, "%s%s",
		   some_characters, some_characters);
}

static void
quote_test(void)
{
    expected_result = NULL;
    print_cmp_test(-1, "\n");
    print_cmp_test(-1, "\\n");
    print_cmp_test(-1, "\r");
    print_cmp_test(-1, "\\r");
    print_cmp_test(-1, "\t");
    print_cmp_test(-1, "\\t");
    print_cmp_test(-1, "\v");
    print_cmp_test(-1, "\\v");
    print_cmp_test(-1, "\b");
    print_cmp_test(-1, "\\b");
    print_cmp_test(-1, "\f");
    print_cmp_test(-1, "\\f");
    print_cmp_test(-1, "\x80");
    print_cmp_test(-1, "\\x80");
    print_cmp_test(-1, "\x14");
    print_cmp_test(-1, "\\x14");
    print_cmp_test(-1, "\xff");
    print_cmp_test(-1, "\\xff");
    print_cmp_test(-1, "\043");
    print_cmp_test(-1, "\\043");
    print_cmp_test(-1, "\053");
    print_cmp_test(-1, "\\053");
    print_cmp_test(-1, "\0143");
    print_cmp_test(-1, "\\0143");
    print_cmp_test(-1, "\\lf");
    print_cmp_test(-1, "\\msss");
    print_cmp_test(-1, "\\ss");
    
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * The dispatcher                                                            *
\*                                                                           */

int
main(int argc, char *argv[])
{
    if (argc < 2)
	fail("To few arguments for test case");

    {
	char *testcase;
	int save_xres = 0;
	int i;

	send_my_pid();

	testcase = argv[1];
#ifdef THREAD_SAFE
	{
	    int res = ethr_init(NULL);
	    if (res != 0)
		fail("Failed to initialize the ethread library");
	}
#endif

	for (i = 2; i < argc; i++) {
	    if (strcmp(argv[i], "save_expected_result") == 0) {
		save_xres = 1;
		break;
	    }
	}

	if (save_xres) {
	    char filename[100];
	    sprintf(filename,
		    "%s%s_test.h",
		    testcase,
		    sizeof(void *) == 8 ? "_64" : "");
	    printf("Saving expected result to %s\n", filename);
	    outfile = fopen(filename, "w");
	    ASSERT(outfile);
	    fprintf(outfile,
		    "/*\n"
		    " * %%CopyrightBegin%%\n"
		    " * Copyright Ericsson AB 1996-2009. All Rights Reserved.\n"
		    " * \n"
                    " * Licensed under the Apache License, Version 2.0 (the \"License\");\n"
                    " * you may not use this file except in compliance with the License.\n"
                    " * You may obtain a copy of the License at\n"
                    " * \n"
                    " *      http://www.apache.org/licenses/LICENSE-2.0\n"
                    " * \n"
                    " * Unless required by applicable law or agreed to in writing, software\n"
                    " * distributed under the License is distributed on an \"AS IS\" BASIS,\n"
                    " * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.\n"
                    " * See the License for the specific language governing permissions and\n"
                    " * limitations under the License.\n"
		    " * %%CopyrightEnd%%\n"
		    " */\n"
		    "\n");
	    fprintf(outfile,
		    "/* \n"
		    " * This file has been automatically generated. Do NOT edit it; instead,\n"
		    " * run '%s %s save_expected_result'%s.\n"
		    " */\n"
		    "\n",
		    argv[0],
		    testcase,
		    sizeof(void *) == 8 ? " on a 64-bit machine" : "");
	    fprintf(outfile,
		    "char *%s%s_expected_result[] = {\n",
		    testcase,
		    sizeof(void *) == 8 ? "_64" : "");
	}

	if (strcmp("integer", testcase) == 0)
	    integer_test();
	else if (strcmp("float", testcase) == 0)
	    float_test();
	else if (strcmp("string", testcase) == 0)
	    string_test();
	else if (strcmp("character", testcase) == 0)
	    character_test();
	else if (strcmp("snprintf", testcase) == 0)
	    snprintf_test();
	else if (strcmp("quote", testcase) == 0)
	    quote_test();
	else if (!save_xres)
	    skip("Test case \"%s\" not implemented yet", testcase);

	if (save_xres) {
	    fprintf(outfile, "\tNULL};\n");
	    fclose(outfile);
	}

	succeed(NULL);
    }

    return 0;
}



