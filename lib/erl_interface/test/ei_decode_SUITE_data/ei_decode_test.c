/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2004-2009. All Rights Reserved.
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
      fail("size of encoded data is incorrect"); \
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

#define EI_DECODE_STRING(FUNC,SIZE,VAL) \
  { \
    char p[1024]; \
    char *buf; \
    int size1 = 0; \
    int size2 = 0; \
    int err; \
    message("ei_" #FUNC " should be " #VAL); \
    buf = read_packet(NULL); \
\
    err = ei_ ## FUNC(buf+1, &size1, NULL); \
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
    err = ei_ ## FUNC(buf+1, &size2, p); \
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
    if (strcmp(p,VAL) != 0) { \
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

    EI_DECODE_2     (decode_long,  5, long,  0x07ffffff);
    EI_DECODE_2     (decode_long,  5, long, -0x08000000);
    EI_DECODE_2     (decode_long,  7, long,  0x08000000);
    EI_DECODE_2     (decode_long,  7, long, -0x08000001);

    EI_DECODE_2     (decode_long,  7, long,  0x7fffffff);
    EI_DECODE_2     (decode_long,  7, long, -ll(0x80000000)); /* Strange :-( */

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
    EI_DECODE_2     (decode_ulong,  7, unsigned long,  0x08000000);
    EI_DECODE_2_FAIL(decode_ulong,  7, unsigned long, -0x08000001);

    EI_DECODE_2     (decode_ulong,  7, unsigned long,  0x7fffffff);
    EI_DECODE_2_FAIL(decode_ulong,  7, unsigned long, -ll(0x80000000));

    if (sizeof(long) > 4) {
      EI_DECODE_2     (decode_ulong,  11, unsigned long,  ll(0x8000000000000000));
      EI_DECODE_2     (decode_ulong,  11, unsigned long,  ll(0xffffffffffffffff));
    } else {
      EI_DECODE_2     (decode_ulong,  7, unsigned long,  0x80000000);
      EI_DECODE_2     (decode_ulong,  7, unsigned long,  0xffffffff);
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
    EI_DECODE_2     (decode_longlong,  7, EI_LONGLONG,  0x08000000);
    EI_DECODE_2     (decode_longlong,  7, EI_LONGLONG, -0x08000001);

    EI_DECODE_2     (decode_longlong,  7, EI_LONGLONG,  0x7fffffff);
    EI_DECODE_2     (decode_longlong,  7, EI_LONGLONG, -ll(0x80000000));

    EI_DECODE_2     (decode_longlong,  7, EI_LONGLONG,  0x80000000);
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
    EI_DECODE_2     (decode_ulonglong, 7, EI_ULONGLONG,  0x08000000);
    EI_DECODE_2_FAIL(decode_ulonglong, 7, EI_ULONGLONG, -0x08000001);

    EI_DECODE_2     (decode_ulonglong, 7, EI_ULONGLONG,  0x7fffffff);
    EI_DECODE_2_FAIL(decode_ulonglong, 7, EI_ULONGLONG, -ll(0x80000000));

    EI_DECODE_2     (decode_ulonglong, 7, EI_ULONGLONG,  0x80000000);
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
    EI_DECODE_2(decode_double, 32, double, 0.0);
    EI_DECODE_2(decode_double, 32, double, -1.0);
    EI_DECODE_2(decode_double, 32, double, 1.0);

    EI_DECODE_2(decode_boolean, 8, int, 0);
    EI_DECODE_2(decode_boolean, 7, int, 1);

    EI_DECODE_STRING(decode_atom, 6, "foo");
    EI_DECODE_STRING(decode_atom, 3, "");
    EI_DECODE_STRING(decode_atom, 9, "≈ƒ÷Â‰ˆ");

    EI_DECODE_STRING(decode_string, 6, "foo");
    EI_DECODE_STRING(decode_string, 1, "");
    EI_DECODE_STRING(decode_string, 9, "≈ƒ÷Â‰ˆ");

    EI_DECODE_BIN(decode_binary,  8, "foo", 3);
    EI_DECODE_BIN(decode_binary,  5, "", 0);
    EI_DECODE_BIN(decode_binary, 11, "≈ƒ÷Â‰ˆ", 6);

    /* FIXME check \0 in strings and atoms? */
/*
    EI_ENCODE_1(decode_tuple_header, 0);

    EI_ENCODE_0(decode_empty_list);
*/
    report(1);
}

/* ******************************************************************** */

