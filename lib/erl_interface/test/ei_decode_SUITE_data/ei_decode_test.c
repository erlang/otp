/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2004-2017. All Rights Reserved.
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

#include <string.h>

#ifdef VXWORKS
#include "reclaim.h"
#endif

#include "ei_runner.h"

/*
 * Purpose: Tests the ei_format() function.
 * Author:  Kent
 */

#ifdef VXWORKS
#define MESSAGE_BACK(SIZE) \
    message("err = %d, size2 = %d, expected size = %d", \
             err, size1, SIZE); 
#else
#define MESSAGE_BACK(SIZE) \
    message("err = %d, size2 = %d, expected size = %d, long long val = %lld", \
             err, size1, SIZE, (EI_LONGLONG)p); 
#endif

#define ERLANG_ANY (ERLANG_ASCII|ERLANG_LATIN1|ERLANG_UTF8)

struct my_atom {
  erlang_char_encoding from;
  erlang_char_encoding was_check;
  erlang_char_encoding result_check;
};

/* Allow arrays constants to be part of macro arguments */
#define P99(...) __VA_ARGS__ 

int ei_decode_my_atom_as(const char *buf, int *index, char *to,
			 struct my_atom *atom);
int ei_decode_my_atom(const char *buf, int *index, char *to,
		      struct my_atom *atom);
int ei_decode_my_string(const char *buf, int *index, char *to,
			struct my_atom *atom);

#define EI_DECODE_2(FUNC,SIZE,TYPE,VAL) \
  { \
    TYPE p; \
    char *buf; \
    int size1 = 0; \
    int size2 = 0; \
    int err; \
    message("ei_" #FUNC " " #TYPE " should be " #VAL); \
    buf = read_packet(NULL); \
\
    err = ei_ ## FUNC(buf+1, &size1, NULL); \
    message("err = %d, size1 = %d, expected size = %d", \
             err, size1, SIZE); \
    if (err != 0) { \
      if (err != -1) { \
	fail("returned non zero but not -1 if NULL pointer"); \
      } else { \
	fail("returned non zero if NULL pointer"); \
      } \
      return; \
    } \
\
    err = ei_ ## FUNC(buf+1, &size2, &p); \
    MESSAGE_BACK(SIZE) \
    if (err != 0) { \
      if (err != -1) { \
	fail("returned non zero but not -1"); \
      } else { \
	fail("returned non zero"); \
      } \
      return; \
    } \
    if (p != (TYPE)VAL) { \
      fail("value is not correct"); \
      return; \
    } \
\
    if (size1 != size2) { \
      fail("size with and without pointer differs"); \
      return; \
    } \
\
    if (size1 != SIZE) { \
        fail1("size of encoded data (%d) is incorrect", size1);    \
      return; \
    } \
  } \

#define EI_DECODE_2_FAIL(FUNC,SIZE,TYPE,VAL) \
  { \
    TYPE p, saved_p; \
    char *buf; \
    int size1 = 0; \
    int size2 = 0; \
    int err; \
    message("ei_" #FUNC " " #TYPE " should fail"); \
    memset(&p,'\0',sizeof(p)); \
    saved_p = p; \
    buf = read_packet(NULL); \
\
    err = ei_ ## FUNC(buf+1, &size1, NULL); \
    message("err = %d, size1 = %d, expected size = %d", \
             err, size1, SIZE); \
    if (err != -1) { \
      fail("should return -1 if NULL pointer"); \
      return; \
    } \
\
    err = ei_ ## FUNC(buf+1, &size2, &p); \
    message("err = %d, size2 = %d, expected size = %d", \
             err, size1, SIZE); \
    if (err != -1) { \
      fail("should return -1"); \
      return; \
    } \
    if (p != saved_p) { \
      fail("p argument was modified"); \
      return; \
    } \
\
    if (size1 != 0) { \
      fail("size of encoded data should be 0 if NULL"); \
      return; \
    } \
\
    if (size2 != 0) { \
      fail("size of encoded data should be 0"); \
      return; \
    } \
  } \

#define dump(arr, num) {	    \
    int i;		    \
    message("Dumping " #arr ": ");			\
    for (i = 0; i < num; i++) message("%u, ",(unsigned char)arr[i]);	\
    message("\n");					\
  }
  
#define EI_DECODE_STRING_4(FUNC,SIZE,VAL,ATOM)	\
  { \
    char p[1024]; \
    char *buf; \
    unsigned char val[] = VAL; \
    int size1 = 0; \
    int size2 = 0; \
    int err; \
    struct my_atom atom = ATOM; \
    message("ei_" #FUNC " should be " #VAL "\n"); \
    buf = read_packet(NULL); \
\
    err = ei_ ## FUNC(buf+1, &size1, NULL, &atom); \
    message("err = %d, size = %d, expected size = %d\n",err,size1,SIZE); \
    if (err != 0) { \
      if (err != -1) { \
	fail("returned non zero but not -1 if NULL pointer"); \
      } else { \
	fail("returned non zero if NULL pointer"); \
      } \
      return; \
    } \
\
    err = ei_ ## FUNC(buf+1, &size2, p, &atom);	\
    message("err = %d, size = %d, expected size = %d\n",err,size2,SIZE); \
    if (err != 0) { \
      if (err != -1) { \
	fail("returned non zero but not -1"); \
      } else { \
	fail("returned non zero"); \
      } \
      return; \
    } \
\
    if (strcmp(p,val) != 0) { \
      fail("value is not correct"); \
      return; \
    } \
\
    if (size1 != size2) { \
      fail("size with and without pointer differs"); \
      return; \
    } \
\
    if (size1 != SIZE) { \
      fail("size of encoded data is incorrect"); \
      return; \
    } \
  } \

#define EI_DECODE_STRING(FUNC,SIZE,VAL) \
  EI_DECODE_STRING_4(FUNC,SIZE,VAL, \
		     P99({ERLANG_ANY,ERLANG_ANY,ERLANG_ANY}))

#define EI_DECODE_STRING_FAIL(FUNC,ATOM) \
  { \
    char p[1024]; \
    char *buf; \
    int size1 = 0; \
    int size2 = 0; \
    int err;					  \
    struct my_atom atom = ATOM;\
    message("ei_" #FUNC " should fail\n"); \
    p[0] = 0;				   \
    message("p[0] is %d\n",p[0]);		   \
    buf = read_packet(NULL); \
\
    err = ei_ ## FUNC(buf+1, &size1, NULL, &atom);			\
    if (err != -1) {				\
      fail("should return -1 if NULL pointer"); \
      return; \
    } \
\
    err = ei_ ## FUNC(buf+1, &size2, p, &atom);				\
    if (err != -1) {							\
      fail("should return -1"); \
      return; \
    } \
    if (p[0] != 0) { \
      message("p[0] argument was modified to %u\n",(unsigned char)p[0]);	\
    } \
\
    if (size1 != 0) { \
      fail("size of encoded data should be 0 if NULL"); \
      return; \
    } \
\
    if (size2 != 0) { \
      fail("size of encoded data should be 0"); \
      return; \
    } \
  } \

//#define EI_DECODE_UTF8_STRING(FUNC,SIZE,VAL) 

#define EI_DECODE_BIN(FUNC,SIZE,VAL,LEN) \
  { \
    char p[1024]; \
    char *buf; \
    long len; \
    int size1 = 0; \
    int size2 = 0; \
    int err; \
    message("ei_" #FUNC " should be " #VAL); \
    buf = read_packet(NULL); \
    err = ei_ ## FUNC(buf+1, &size1, NULL, &len); \
    message("err = %d, size = %d, len = %d, expected size = %d, expected len = %d\n",\
            err,size1,len,SIZE,LEN); \
    if (err != 0) { \
      if (err != -1) { \
	fail("returned non zero but not -1 if NULL pointer"); \
      } else { \
	fail("returned non zero"); \
      } \
      return; \
    } \
\
    if (len != LEN) { \
      fail("size is not correct"); \
      return; \
    } \
\
    err = ei_ ## FUNC(buf+1, &size2, p, &len); \
    message("err = %d, size = %d, len = %d, expected size = %d, expected len = %d\n",\
            err,size2,len,SIZE,LEN); \
    if (err != 0) { \
      if (err != -1) { \
	fail("returned non zero but not -1 if NULL pointer"); \
      } else { \
	fail("returned non zero"); \
      } \
      return; \
    } \
\
    if (len != LEN) { \
      fail("size is not correct"); \
      return; \
    } \
\
    if (strncmp(p,VAL,LEN) != 0) { \
      fail("value is not correct"); \
      return; \
    } \
\
    if (size1 != size2) { \
      fail("size with and without pointer differs"); \
      return; \
    } \
\
    if (size1 != SIZE) { \
      fail("size of encoded data is incorrect"); \
      return; \
    } \
  } \

/* ******************************************************************** */

TESTCASE(test_ei_decode_long)
{
    EI_DECODE_2     (decode_long,  2, long, 0);
    EI_DECODE_2     (decode_long,  2, long, 255);
    EI_DECODE_2     (decode_long,  5, long, 256);
    EI_DECODE_2     (decode_long,  5, long, -1);

    /* Old 28 bit limits for INTEGER_EXT */
    EI_DECODE_2     (decode_long,  5, long,  0x07ffffff);
    EI_DECODE_2     (decode_long,  5, long, -0x08000000);
    EI_DECODE_2     (decode_long,  5, long,  0x08000000);  
    EI_DECODE_2     (decode_long,  5, long, -0x08000001);

    /* New 32 bit limits for INTEGER_EXT */
    EI_DECODE_2     (decode_long,  5, long,     0x7fffffff);
    EI_DECODE_2     (decode_long,  5, long, -ll(0x80000000)); /* Strange :-( */
    if (sizeof(long) > 4) {
	EI_DECODE_2(decode_long,  7, long,     0x80000000);
	EI_DECODE_2(decode_long,  7, long, -ll(0x80000001));
    }
    else {
	EI_DECODE_2_FAIL(decode_long,  7, long,     0x80000000);
	EI_DECODE_2_FAIL(decode_long,  7, long, -ll(0x80000001));       
    }

    EI_DECODE_2_FAIL(decode_long,  7, long,  0x80000000);
    EI_DECODE_2_FAIL(decode_long,  7, long,  0xffffffff);

    EI_DECODE_2_FAIL(decode_long,  9, long,  ll(0x7fffffffffff));
    EI_DECODE_2_FAIL(decode_long,  9, long, -ll(0x800000000000));
    EI_DECODE_2_FAIL(decode_long,  9, long,  ll(0xffffffffffff));
    EI_DECODE_2_FAIL(decode_long, 11, long,  ll(0x7fffffffffffffff));
    EI_DECODE_2_FAIL(decode_long, 11, long, -ll(0x8000000000000000));
    EI_DECODE_2_FAIL(decode_long, 11, long,  ll(0xffffffffffffffff));

    EI_DECODE_2_FAIL(decode_long,  1, long,  0); /* Illegal type sent */

    report(1);
}

/* ******************************************************************** */

TESTCASE(test_ei_decode_ulong)
{
    EI_DECODE_2     (decode_ulong,  2, unsigned long, 0);
    EI_DECODE_2     (decode_ulong,  2, unsigned long, 255);
    EI_DECODE_2     (decode_ulong,  5, unsigned long, 256);
    EI_DECODE_2_FAIL(decode_ulong,  5, unsigned long, -1);

    EI_DECODE_2     (decode_ulong,  5, unsigned long,  0x07ffffff);
    EI_DECODE_2_FAIL(decode_ulong,  5, unsigned long, -0x08000000);
    EI_DECODE_2     (decode_ulong,  5, unsigned long,  0x08000000);
    EI_DECODE_2_FAIL(decode_ulong,  5, unsigned long, -0x08000001);

    EI_DECODE_2     (decode_ulong,  5, unsigned long,     0x7fffffff);
    EI_DECODE_2_FAIL(decode_ulong,  5, unsigned long, -ll(0x80000000));
    EI_DECODE_2     (decode_ulong,  7, unsigned long,     0x80000000);
    EI_DECODE_2_FAIL(decode_ulong,  7, unsigned long, -ll(0x80000001));

    if (sizeof(long) > 4) {
      EI_DECODE_2     (decode_ulong,  11, unsigned long,  ll(0x8000000000000000));
      EI_DECODE_2     (decode_ulong,  11, unsigned long,  ll(0xffffffffffffffff));
    } else {
        if (sizeof(void*) > 4) {
            /* Windows */
            EI_DECODE_2_FAIL(decode_ulong,  11, unsigned long,  ll(0x8000000000000000));
            EI_DECODE_2_FAIL(decode_ulong,  11, unsigned long,  ll(0xffffffffffffffff));
        } else {
            EI_DECODE_2     (decode_ulong,  7, unsigned long,  0x80000000);
            EI_DECODE_2     (decode_ulong,  7, unsigned long,  0xffffffff);
        }
    }

    EI_DECODE_2_FAIL(decode_ulong,  9, unsigned long,  ll(0x7fffffffffff));
    EI_DECODE_2_FAIL(decode_ulong,  9, unsigned long, -ll(0x800000000000));
    EI_DECODE_2_FAIL(decode_ulong,  9, unsigned long,  ll(0xffffffffffff));
    EI_DECODE_2_FAIL(decode_ulong, 11, unsigned long,  ll(0x7fffffffffffffff));
    EI_DECODE_2_FAIL(decode_ulong, 11, unsigned long, -ll(0x8000000000000000));
    EI_DECODE_2_FAIL(decode_ulong, 11, unsigned long,  ll(0xffffffffffffffff));

    EI_DECODE_2_FAIL(decode_ulong,  1, unsigned long,  0); /* Illegal type */

    report(1);
}

/* ******************************************************************** */


TESTCASE(test_ei_decode_longlong)
{
#ifndef VXWORKS
    EI_DECODE_2     (decode_longlong,  2, EI_LONGLONG, 0);
    EI_DECODE_2     (decode_longlong,  2, EI_LONGLONG, 255);
    EI_DECODE_2     (decode_longlong,  5, EI_LONGLONG, 256);
    EI_DECODE_2     (decode_longlong,  5, EI_LONGLONG, -1);

    EI_DECODE_2     (decode_longlong,  5, EI_LONGLONG,  0x07ffffff);
    EI_DECODE_2     (decode_longlong,  5, EI_LONGLONG, -0x08000000);
    EI_DECODE_2     (decode_longlong,  5, EI_LONGLONG,  0x08000000);
    EI_DECODE_2     (decode_longlong,  5, EI_LONGLONG, -0x08000001);

    EI_DECODE_2     (decode_longlong,  5, EI_LONGLONG,  0x7fffffff);
    EI_DECODE_2     (decode_longlong,  5, EI_LONGLONG, -ll(0x80000000));
    EI_DECODE_2     (decode_longlong,  7, EI_LONGLONG,  0x80000000);
    EI_DECODE_2     (decode_longlong,  7, EI_LONGLONG, -ll(0x80000001));

    EI_DECODE_2     (decode_longlong,  7, EI_LONGLONG,  0xffffffff);

    EI_DECODE_2     (decode_longlong,  9, EI_LONGLONG,  ll(0x7fffffffffff));
    EI_DECODE_2     (decode_longlong,  9, EI_LONGLONG, -ll(0x800000000000));
    EI_DECODE_2     (decode_longlong,  9, EI_LONGLONG,  ll(0xffffffffffff));
    EI_DECODE_2     (decode_longlong, 11, EI_LONGLONG,  ll(0x7fffffffffffffff));
    EI_DECODE_2     (decode_longlong, 11, EI_LONGLONG, -ll(0x8000000000000000));
    EI_DECODE_2_FAIL(decode_longlong, 11, EI_LONGLONG,  ll(0xffffffffffffffff));

    EI_DECODE_2_FAIL(decode_longlong,  1, EI_LONGLONG,  0); /* Illegal type */
#endif
    report(1);
}

/* ******************************************************************** */

TESTCASE(test_ei_decode_ulonglong)
{
#ifndef VXWORKS
    EI_DECODE_2     (decode_ulonglong, 2, EI_ULONGLONG, 0);
    EI_DECODE_2     (decode_ulonglong, 2, EI_ULONGLONG, 255);
    EI_DECODE_2     (decode_ulonglong, 5, EI_ULONGLONG, 256);
    EI_DECODE_2_FAIL(decode_ulonglong, 5, EI_ULONGLONG, -1);

    EI_DECODE_2     (decode_ulonglong, 5, EI_ULONGLONG,  0x07ffffff);
    EI_DECODE_2_FAIL(decode_ulonglong, 5, EI_ULONGLONG, -0x08000000);
    EI_DECODE_2     (decode_ulonglong, 5, EI_ULONGLONG,  0x08000000);
    EI_DECODE_2_FAIL(decode_ulonglong, 5, EI_ULONGLONG, -0x08000001);

    EI_DECODE_2     (decode_ulonglong, 5, EI_ULONGLONG,  0x7fffffff);
    EI_DECODE_2_FAIL(decode_ulonglong, 5, EI_ULONGLONG, -ll(0x80000000));
    EI_DECODE_2     (decode_ulonglong, 7, EI_ULONGLONG,  0x80000000);
    EI_DECODE_2_FAIL(decode_ulonglong, 7, EI_ULONGLONG, -0x80000001);

    EI_DECODE_2     (decode_ulonglong, 7, EI_ULONGLONG,  0xffffffff);

    EI_DECODE_2     (decode_ulonglong, 9, EI_ULONGLONG,  ll(0x7fffffffffff));
    EI_DECODE_2_FAIL(decode_ulonglong, 9, EI_ULONGLONG, -ll(0x800000000000));
    EI_DECODE_2     (decode_ulonglong, 9, EI_ULONGLONG,  ll(0xffffffffffff));
    EI_DECODE_2     (decode_ulonglong,11, EI_ULONGLONG,  ll(0x7fffffffffffffff));
    EI_DECODE_2_FAIL(decode_ulonglong,11, EI_ULONGLONG, -ll(0x8000000000000000));
    EI_DECODE_2     (decode_ulonglong,11, EI_ULONGLONG,  ll(0xffffffffffffffff));

    EI_DECODE_2_FAIL(decode_ulonglong, 1, EI_ULONGLONG, 0); /* Illegal type */
#endif
    report(1);
}


/* ******************************************************************** */

TESTCASE(test_ei_decode_char)
{
    EI_DECODE_2(decode_char, 2, char, 0);
    EI_DECODE_2(decode_char, 2, char, 0x7f);
    EI_DECODE_2(decode_char, 2, char, 0xff);

    EI_DECODE_2_FAIL(decode_char, 1, char, 0); /* Illegal type */

    report(1);
}

/* ******************************************************************** */

TESTCASE(test_ei_decode_nonoptimal)
{
    EI_DECODE_2(decode_char,  2, char, 42);
    EI_DECODE_2(decode_char,  5, char, 42);
    EI_DECODE_2(decode_char,  4, char, 42);
    EI_DECODE_2(decode_char,  5, char, 42);
    EI_DECODE_2(decode_char,  7, char, 42);
    EI_DECODE_2(decode_char,  7, char, 42);
    EI_DECODE_2(decode_char,  8, char, 42);
    EI_DECODE_2(decode_char,  9, char, 42);
    EI_DECODE_2(decode_char, 12, char, 42);

/*  EI_DECODE_2(decode_char, char, -42); */
/*  EI_DECODE_2(decode_char, char, -42); */
/*  EI_DECODE_2(decode_char, char, -42); */
/*  EI_DECODE_2(decode_char, char, -42); */
/*  EI_DECODE_2(decode_char, char, -42); */
/*  EI_DECODE_2(decode_char, char, -42); */
/*  EI_DECODE_2(decode_char, char, -42); */
/*  EI_DECODE_2(decode_char, char, -42); */
/*  EI_DECODE_2(decode_char, char, -42); */

    /* ---------------------------------------------------------------- */

    EI_DECODE_2(decode_long,  2, long, 42);
    EI_DECODE_2(decode_long,  5, long, 42);
    EI_DECODE_2(decode_long,  4, long, 42);
    EI_DECODE_2(decode_long,  5, long, 42);
    EI_DECODE_2(decode_long,  7, long, 42);
    EI_DECODE_2(decode_long,  7, long, 42);
    EI_DECODE_2(decode_long,  8, long, 42);
    EI_DECODE_2(decode_long,  9, long, 42);
    EI_DECODE_2(decode_long, 12, long, 42);

/*  EI_DECODE_2(decode_long,  2, long, -42); */
    EI_DECODE_2(decode_long,  5, long, -42);
    EI_DECODE_2(decode_long,  4, long, -42);
    EI_DECODE_2(decode_long,  5, long, -42);
    EI_DECODE_2(decode_long,  7, long, -42);
    EI_DECODE_2(decode_long,  7, long, -42);
    EI_DECODE_2(decode_long,  8, long, -42);
    EI_DECODE_2(decode_long,  9, long, -42);
    EI_DECODE_2(decode_long, 12, long, -42);

    /* ---------------------------------------------------------------- */

    EI_DECODE_2(decode_ulong,  2, unsigned long, 42);
    EI_DECODE_2(decode_ulong,  5, unsigned long, 42);
    EI_DECODE_2(decode_ulong,  4, unsigned long, 42);
    EI_DECODE_2(decode_ulong,  5, unsigned long, 42);
    EI_DECODE_2(decode_ulong,  7, unsigned long, 42);
    EI_DECODE_2(decode_ulong,  7, unsigned long, 42);
    EI_DECODE_2(decode_ulong,  8, unsigned long, 42);
    EI_DECODE_2(decode_ulong,  9, unsigned long, 42);
    EI_DECODE_2(decode_ulong, 12, unsigned long, 42);

/*  EI_DECODE_2(decode_ulong, unsigned long, -42); */
/*  EI_DECODE_2(decode_ulong, unsigned long, -42); */
/*  EI_DECODE_2(decode_ulong, unsigned long, -42); */
/*  EI_DECODE_2(decode_ulong, unsigned long, -42); */
/*  EI_DECODE_2(decode_ulong, unsigned long, -42); */
/*  EI_DECODE_2(decode_ulong, unsigned long, -42); */
/*  EI_DECODE_2(decode_ulong, unsigned long, -42); */
/*  EI_DECODE_2(decode_ulong, unsigned long, -42); */
/*  EI_DECODE_2(decode_ulong, unsigned long, -42); */

    /* ---------------------------------------------------------------- */

#ifndef VXWORKS

    EI_DECODE_2(decode_longlong,  2, EI_LONGLONG, 42);
    EI_DECODE_2(decode_longlong,  5, EI_LONGLONG, 42);
    EI_DECODE_2(decode_longlong,  4, EI_LONGLONG, 42);
    EI_DECODE_2(decode_longlong,  5, EI_LONGLONG, 42);
    EI_DECODE_2(decode_longlong,  7, EI_LONGLONG, 42);
    EI_DECODE_2(decode_longlong,  7, EI_LONGLONG, 42);
    EI_DECODE_2(decode_longlong,  8, EI_LONGLONG, 42);
    EI_DECODE_2(decode_longlong,  9, EI_LONGLONG, 42);
    EI_DECODE_2(decode_longlong, 12, EI_LONGLONG, 42);

/*  EI_DECODE_2(decode_longlong,  2, EI_LONGLONG, -42); */
    EI_DECODE_2(decode_longlong,  5, EI_LONGLONG, -42);
    EI_DECODE_2(decode_longlong,  4, EI_LONGLONG, -42);
    EI_DECODE_2(decode_longlong,  5, EI_LONGLONG, -42);
    EI_DECODE_2(decode_longlong,  7, EI_LONGLONG, -42);
    EI_DECODE_2(decode_longlong,  7, EI_LONGLONG, -42);
    EI_DECODE_2(decode_longlong,  8, EI_LONGLONG, -42);
    EI_DECODE_2(decode_longlong,  9, EI_LONGLONG, -42);
    EI_DECODE_2(decode_longlong, 12, EI_LONGLONG, -42);

    /* ---------------------------------------------------------------- */

    EI_DECODE_2(decode_ulonglong,  2, EI_ULONGLONG, 42);
    EI_DECODE_2(decode_ulonglong,  5, EI_ULONGLONG, 42);
    EI_DECODE_2(decode_ulonglong,  4, EI_ULONGLONG, 42);
    EI_DECODE_2(decode_ulonglong,  5, EI_ULONGLONG, 42);
    EI_DECODE_2(decode_ulonglong,  7, EI_ULONGLONG, 42);
    EI_DECODE_2(decode_ulonglong,  7, EI_ULONGLONG, 42);
    EI_DECODE_2(decode_ulonglong,  8, EI_ULONGLONG, 42);
    EI_DECODE_2(decode_ulonglong,  9, EI_ULONGLONG, 42);
    EI_DECODE_2(decode_ulonglong, 12, EI_ULONGLONG, 42);

/*  EI_DECODE_2(decode_ulonglong, EI_ULONGLONG, -42); */
/*  EI_DECODE_2(decode_ulonglong, EI_ULONGLONG, -42); */
/*  EI_DECODE_2(decode_ulonglong, EI_ULONGLONG, -42); */
/*  EI_DECODE_2(decode_ulonglong, EI_ULONGLONG, -42); */
/*  EI_DECODE_2(decode_ulonglong, EI_ULONGLONG, -42); */
/*  EI_DECODE_2(decode_ulonglong, EI_ULONGLONG, -42); */
/*  EI_DECODE_2(decode_ulonglong, EI_ULONGLONG, -42); */
/*  EI_DECODE_2(decode_ulonglong, EI_ULONGLONG, -42); */
/*  EI_DECODE_2(decode_ulonglong, EI_ULONGLONG, -42); */

#endif /* !VXWORKS */

    /* ---------------------------------------------------------------- */

    report(1);
}

/* ******************************************************************** */

TESTCASE(test_ei_decode_misc)
{
/*
    EI_DECODE_0(decode_version);
*/
    EI_DECODE_2(decode_double, 9, double, 0.0);
    EI_DECODE_2(decode_double, 9, double, -1.0);
    EI_DECODE_2(decode_double, 9, double, 1.0);

    EI_DECODE_2(decode_boolean, 8, int, 0);
    EI_DECODE_2(decode_boolean, 7, int, 1);

    EI_DECODE_STRING(decode_my_atom, 6, "foo");
    EI_DECODE_STRING(decode_my_atom, 3, "");
    EI_DECODE_STRING(decode_my_atom, 9, "ÅÄÖåäö");

    EI_DECODE_STRING(decode_my_string, 6, "foo");
    EI_DECODE_STRING(decode_my_string, 1, "");
    EI_DECODE_STRING(decode_my_string, 9, "ÅÄÖåäö");

    EI_DECODE_BIN(decode_binary,  8, "foo", 3);
    EI_DECODE_BIN(decode_binary,  5, "", 0);
    EI_DECODE_BIN(decode_binary, 11, "ÅÄÖåäö", 6);

    /* FIXME check \0 in strings and atoms? */
/*
    EI_ENCODE_1(decode_tuple_header, 0);

    EI_ENCODE_0(decode_empty_list);
*/
    report(1);
}

/* ******************************************************************** */

TESTCASE(test_ei_decode_utf8_atom)
{

  EI_DECODE_STRING_4(decode_my_atom_as, 4, P99({229,0}), /* LATIN1 "å" */
		   P99({ERLANG_ANY,ERLANG_LATIN1,ERLANG_LATIN1}));
  EI_DECODE_STRING_4(decode_my_atom_as, 4, P99({195,164,0}), /* UTF8 "ä" */
		     P99({ERLANG_UTF8,ERLANG_LATIN1,ERLANG_UTF8}));
  EI_DECODE_STRING_4(decode_my_atom_as, 4, P99({246,0}), /* LATIN1 "ö" */
		     P99({ERLANG_LATIN1,ERLANG_LATIN1,ERLANG_LATIN1}));
  EI_DECODE_STRING_FAIL(decode_my_atom_as, 
			P99({ERLANG_ASCII,ERLANG_ANY,ERLANG_ANY}));

  EI_DECODE_STRING_4(decode_my_atom_as, 4, P99({219,158,0}),
		     P99({ERLANG_ANY,ERLANG_UTF8,ERLANG_UTF8}));
  EI_DECODE_STRING_4(decode_my_atom_as, 6, P99({219,158,219,158,0}),
		     P99({ERLANG_UTF8,ERLANG_UTF8,ERLANG_UTF8}));
  EI_DECODE_STRING_FAIL(decode_my_atom_as,
			P99({ERLANG_LATIN1,ERLANG_ANY,ERLANG_ANY}));
  EI_DECODE_STRING_FAIL(decode_my_atom_as,
			P99({ERLANG_ASCII,ERLANG_ANY,ERLANG_ANY}));

  EI_DECODE_STRING_4(decode_my_atom_as, 4, "a",
		   P99({ERLANG_ANY,ERLANG_LATIN1,ERLANG_ASCII}));
  EI_DECODE_STRING_4(decode_my_atom_as, 4, "b",
		     P99({ERLANG_UTF8,ERLANG_LATIN1,ERLANG_ASCII}));
  EI_DECODE_STRING_4(decode_my_atom_as, 4, "c",
		     P99({ERLANG_LATIN1,ERLANG_LATIN1,ERLANG_ASCII}));
  EI_DECODE_STRING_4(decode_my_atom_as, 4, "d",
		     P99({ERLANG_ASCII,ERLANG_LATIN1,ERLANG_ASCII}));

  report(1);
}

/* ******************************************************************** */

int ei_decode_my_atom_as(const char *buf, int *index, char *to,
			 struct my_atom *atom) {
  erlang_char_encoding was,result;
  int res = ei_decode_atom_as(buf,index,to,1024,atom->from,&was,&result);
  if (res != 0)
    return res;
  if (!(was & atom->was_check)) {
    message("Original encoding was %d not %d\n",was,atom->was_check);
    return -1;
  } else if (!(result & atom->result_check)) {
    message("Result encoding was %d not %d\n",result,atom->result_check);
    return -1;
  }
  return res;
}

int ei_decode_my_atom(const char *buf, int *index, char *to, 
		      struct my_atom *atom) {
  return ei_decode_atom(buf, index, to);
}

int ei_decode_my_string(const char *buf, int *index, char *to, 
			struct my_atom *atom) {
  return ei_decode_string(buf, index, to);
}
