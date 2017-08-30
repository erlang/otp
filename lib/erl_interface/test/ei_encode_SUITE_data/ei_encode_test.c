/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2004-2016. All Rights Reserved.
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

#ifdef VXWORKS
#include "reclaim.h"
#endif

#include "ei_runner.h"

/*
 * Purpose: Tests the ei_format() function.
 * Author:  Kent
 */

#define EI_ENCODE_0(FUNC) \
  { \
    char buf[1024]; \
    int size1 = 0; \
    int size2 = 0; \
    int err; \
    message("ei_" #FUNC " encoded as "); \
    err = ei_ ## FUNC(NULL, &size1); \
    if (err != 0) { \
      if (err != -1) { \
	fail("size calculation returned non zero but not -1"); \
	return; \
      } else { \
	fail("size calculation returned non zero"); \
	return; \
      } \
    } \
    err = ei_ ## FUNC(buf, &size2); \
    if (err != 0) { \
      if (err != -1) { \
	fail("returned non zero but not -1"); \
      } else { \
	fail("returned non zero"); \
      } \
      return; \
    } \
    if (size1 != size2) { \
      fail("size differs when arg is NULL or buf"); \
      return; \
    } \
    if (size1 < 1) { \
      fail("size is < 1"); \
      return; \
    } \
    send_buffer(buf, size1); \
  } \
  { \
    ei_x_buff arg; \
    int err; \
    message("ei_x_" #FUNC " encoded as "); \
    ei_x_new(&arg); \
    err = ei_x_ ## FUNC(&arg); \
    if (err != 0) { \
      if (err != -1) { \
	fail("returned non zero but not -1"); \
      } else { \
	fail("returned non zero"); \
      } \
      ei_x_free(&arg); \
      return; \
    } \
    if (arg.index < 1) { \
      fail("size is < 1"); \
      ei_x_free(&arg); \
      return; \
    } \
    send_buffer(arg.buff, arg.index); \
    ei_x_free(&arg); \
  }

#define EI_ENCODE_1(FUNC,ARG) \
  { \
    char buf[1024]; \
    int size1 = 0; \
    int size2 = 0; \
    int err; \
    message("ei_" #FUNC " " #ARG " encoded as "); \
    err = ei_ ## FUNC(NULL, &size1, ARG); \
    if (err != 0) { \
      if (err != -1) { \
	fail("size calculation returned non zero but not -1"); \
	return; \
      } else { \
	fail("size calculation returned non zero"); \
	return; \
      } \
    } \
    err = ei_ ## FUNC(buf, &size2, ARG); \
    if (err != 0) { \
      if (err != -1) { \
	fail("returned non zero but not -1"); \
      } else { \
	fail("returned non zero"); \
      } \
      return; \
    } \
    if (size1 != size2) { \
      fail("size differs when arg is NULL or buf"); \
      return; \
    } \
    if (size1 < 1) { \
      fail("size is < 1"); \
      return; \
    } \
    send_buffer(buf, size1); \
  } \
  { \
    ei_x_buff arg; \
    int err; \
    message("ei_x_" #FUNC " " #ARG " encoded as "); \
    ei_x_new(&arg); \
    err = ei_x_ ## FUNC(&arg, ARG); \
    if (err != 0) { \
      if (err != -1) { \
	fail("returned non zero but not -1"); \
      } else { \
	fail("returned non zero"); \
      } \
      ei_x_free(&arg); \
      return; \
    } \
    if (arg.index < 1) { \
      fail("size is < 1"); \
      ei_x_free(&arg); \
      return; \
    } \
    send_buffer(arg.buff, arg.index); \
    ei_x_free(&arg); \
  }

#define EI_ENCODE_2(FUNC,ARG1,ARG2) \
  { \
    char buf[1024]; \
    int size1 = 0; \
    int size2 = 0; \
    int err; \
    message("ei_" #FUNC " " #ARG1 " " #ARG2 " encoded as "); \
    err = ei_ ## FUNC(NULL, &size1, ARG1, ARG2); \
    if (err != 0) { \
      if (err != -1) { \
	fail("size calculation returned non zero but not -1"); \
	return; \
      } else { \
	fail("size calculation returned non zero"); \
	return; \
      } \
    } \
    err = ei_ ## FUNC(buf, &size2, ARG1, ARG2); \
    if (err != 0) { \
      if (err != -1) { \
	fail("returned non zero but not -1"); \
      } else { \
	fail("returned non zero"); \
      } \
      return; \
    } \
    if (size1 != size2) { \
      fail("size differs when arg is NULL or buf"); \
      return; \
    } \
    if (size1 < 1) { \
      fail("size is < 1"); \
      return; \
    } \
    send_buffer(buf, size1); \
  } \
  { \
    ei_x_buff arg; \
    int err; \
    message("ei_x_" #FUNC " " #ARG1 " " #ARG2 " encoded as "); \
    ei_x_new(&arg); \
    err = ei_x_ ## FUNC(&arg, ARG1, ARG2); \
    if (err != 0) { \
      if (err != -1) { \
	fail("returned non zero but not -1"); \
      } else { \
	fail("returned non zero"); \
      } \
      ei_x_free(&arg); \
      return; \
    } \
    if (arg.index < 1) { \
      fail("size is < 1"); \
      ei_x_free(&arg); \
      return; \
    } \
    send_buffer(arg.buff, arg.index); \
    ei_x_free(&arg); \
  }

#define EI_ENCODE_3(FUNC,ARG1,ARG2,ARG3)	\
  { \
    char buf[1024]; \
    int size1 = 0; \
    int size2 = 0; \
    int err; \
    message("ei_" #FUNC " " #ARG1 " " #ARG2 " " #ARG3 " encoded as "); \
    err = ei_ ## FUNC(NULL, &size1, ARG1, ARG2, ARG3);	     \
    if (err != 0) { \
      if (err != -1) { \
	fail("size calculation returned non zero but not -1"); \
	return; \
      } else { \
	fail("size calculation returned non zero"); \
	return; \
      } \
    } \
    err = ei_ ## FUNC(buf, &size2, ARG1, ARG2, ARG3); \
    if (err != 0) { \
      if (err != -1) { \
	fail("returned non zero but not -1"); \
      } else { \
	fail("returned non zero"); \
      } \
      return; \
    } \
    if (size1 != size2) { \
      fail("size differs when arg is NULL or buf"); \
      return; \
    } \
    if (size1 < 1) { \
      fail("size is < 1"); \
      return; \
    } \
    send_buffer(buf, size1); \
  } \
  { \
    ei_x_buff arg; \
    int err; \
    message("ei_x_" #FUNC " " #ARG1 " " #ARG2 " " #ARG3 " encoded as "); \
    ei_x_new(&arg); \
    err = ei_x_ ## FUNC(&arg, ARG1, ARG2, ARG3); \
    if (err != 0) { \
      if (err != -1) { \
	fail("returned non zero but not -1"); \
      } else { \
	fail("returned non zero"); \
      } \
      ei_x_free(&arg); \
      return; \
    } \
    if (arg.index < 1) { \
      fail("size is < 1"); \
      ei_x_free(&arg); \
      return; \
    } \
    send_buffer(arg.buff, arg.index); \
    ei_x_free(&arg); \
  }

#define EI_ENCODE_3_FAIL(FUNC,ARG1,ARG2,ARG3)	\
  { \
    char buf[1024]; \
    int size1 = 0; \
    int size2 = 0; \
    int err; \
    message("ei_" #FUNC " " #ARG1 " " #ARG2 " " #ARG3 " encoded as "); \
    err = ei_ ## FUNC(NULL, &size1, ARG1, ARG2, ARG3);	     \
    if (err != -1) { \
      fail("size calculation returned non -1");	\
      return;					\
    } \
    err = ei_ ## FUNC(buf, &size2, ARG1, ARG2, ARG3); \
    if (err != -1) {					    \
      fail("returned non -1");				    \
    }							    \
    if (size1 != size2) { \
      fail("size differs when arg is NULL or buf"); \
      return; \
    } \
  } \
  { \
    ei_x_buff arg; \
    int err; \
    message("ei_x_" #FUNC " " #ARG1 " " #ARG2 " " #ARG3 " encoded as "); \
    ei_x_new(&arg); \
    err = ei_x_ ## FUNC(&arg, ARG1, ARG2, ARG3); \
    if (err != -1) {				       \
      fail("returned non -1");			       \
      ei_x_free(&arg);				       \
      return;					       \
    }						       \
    ei_x_free(&arg); \
  }

#define EI_ENCODE_4(FUNC,ARG1,ARG2,ARG3,ARG4)	\
  { \
    char buf[1024]; \
    int size1 = 0; \
    int size2 = 0; \
    int err; \
    message("ei_" #FUNC " " #ARG1 " " #ARG2 " " #ARG3 " " #ARG4 " encoded as "); \
    err = ei_ ## FUNC(NULL, &size1, ARG1, ARG2, ARG3, ARG4);	     \
    if (err != 0) { \
      if (err != -1) { \
	fail("size calculation returned non zero but not -1"); \
	return; \
      } else { \
	fail("size calculation returned non zero"); \
	return; \
      } \
    } \
    err = ei_ ## FUNC(buf, &size2, ARG1, ARG2, ARG3, ARG4); \
    if (err != 0) { \
      if (err != -1) { \
	fail("returned non zero but not -1"); \
      } else { \
	fail("returned non zero"); \
      } \
      return; \
    } \
    if (size1 != size2) { \
      fail("size differs when arg is NULL or buf"); \
      return; \
    } \
    if (size1 < 1) { \
      fail("size is < 1"); \
      return; \
    } \
    send_buffer(buf, size1); \
  } \
  { \
    ei_x_buff arg; \
    int err; \
    message("ei_x_" #FUNC " " #ARG1 " " #ARG2 " " #ARG3 " " #ARG4 " encoded as "); \
    ei_x_new(&arg); \
    err = ei_x_ ## FUNC(&arg, ARG1, ARG2, ARG3, ARG4); \
    if (err != 0) { \
      if (err != -1) { \
	fail("returned non zero but not -1"); \
      } else { \
	fail("returned non zero"); \
      } \
      ei_x_free(&arg); \
      return; \
    } \
    if (arg.index < 1) { \
      fail("size is < 1"); \
      ei_x_free(&arg); \
      return; \
    } \
    send_buffer(arg.buff, arg.index); \
    ei_x_free(&arg); \
  }

#define EI_ENCODE_4_FAIL(FUNC,ARG1,ARG2,ARG3,ARG4)	\
  { \
    char buf[1024]; \
    int size1 = 0; \
    int size2 = 0; \
    int err; \
    message("ei_" #FUNC " " #ARG1 " " #ARG2 " " #ARG3 " " #ARG4 " encoded as "); \
    err = ei_ ## FUNC(NULL, &size1, ARG1, ARG2, ARG3, ARG4);	     \
    if (err != -1) { \
      fail("size calculation returned non -1");	\
      return;					\
    } \
    err = ei_ ## FUNC(buf, &size2, ARG1, ARG2, ARG3, ARG4); \
    if (err != -1) {					    \
      fail("returned non -1");				    \
    }							    \
    if (size1 != size2) { \
      fail("size differs when arg is NULL or buf"); \
      return; \
    } \
  } \
  { \
    ei_x_buff arg; \
    int err; \
    message("ei_x_" #FUNC " " #ARG1 " " #ARG2 " " #ARG3 " " #ARG4 " encoded as "); \
    ei_x_new(&arg); \
    err = ei_x_ ## FUNC(&arg, ARG1, ARG2, ARG3, ARG4); \
    if (err != -1) {				       \
      fail("returned non -1");			       \
      ei_x_free(&arg);				       \
      return;					       \
    }						       \
    ei_x_free(&arg); \
  }

/* ******************************************************************** */

TESTCASE(test_ei_encode_long)
{
    EI_ENCODE_1(encode_long, 0);

    EI_ENCODE_1(encode_long, 255);

    EI_ENCODE_1(encode_long, 256);

    EI_ENCODE_1(encode_long, -1);

    EI_ENCODE_1(encode_long,  0x07ffffff);

    EI_ENCODE_1(encode_long, -ll(0x08000000));

    EI_ENCODE_1(encode_long,  0x07ffffff+1);

    EI_ENCODE_1(encode_long, -ll(0x08000000)-1);

    EI_ENCODE_1(encode_long,  0x7fffffff);

    EI_ENCODE_1(encode_long, -ll(0x80000000));

    report(1);
}

/* ******************************************************************** */

TESTCASE(test_ei_encode_ulong)
{
    EI_ENCODE_1(encode_ulong, 0);

    EI_ENCODE_1(encode_ulong, 255);

    EI_ENCODE_1(encode_ulong, 256);

    EI_ENCODE_1(encode_ulong,  0x07ffffff);

    EI_ENCODE_1(encode_ulong,  0x07ffffff+1);

    EI_ENCODE_1(encode_ulong,  0x7fffffff);

    EI_ENCODE_1(encode_ulong,  0x80000000);

    EI_ENCODE_1(encode_ulong,  0xffffffff);

    report(1);
}

/* ******************************************************************** */


TESTCASE(test_ei_encode_longlong)
{

#ifndef VXWORKS

    EI_ENCODE_1(encode_longlong, 0);

    EI_ENCODE_1(encode_longlong, 255);

    EI_ENCODE_1(encode_longlong, 256);

    EI_ENCODE_1(encode_longlong, -1);

    EI_ENCODE_1(encode_longlong,  0x07ffffff);

    EI_ENCODE_1(encode_longlong, -ll(0x08000000));

    EI_ENCODE_1(encode_longlong,  0x07ffffff+1);

    EI_ENCODE_1(encode_longlong, -ll(0x08000000)-1);

    EI_ENCODE_1(encode_longlong,  0x7fffffff);

    EI_ENCODE_1(encode_longlong, -ll(0x80000000));

    EI_ENCODE_1(encode_longlong,  ll(0x7fffffffffff));

    EI_ENCODE_1(encode_longlong, -ll(0x800000000000));

    EI_ENCODE_1(encode_longlong,  ll(0x7fffffffffffffff));

    EI_ENCODE_1(encode_longlong, -ll(0x8000000000000000));

#endif /* !VXWORKS */

    report(1);
}

/* ******************************************************************** */

TESTCASE(test_ei_encode_ulonglong)
{

#ifndef VXWORKS

    EI_ENCODE_1(encode_ulonglong, 0);

    EI_ENCODE_1(encode_ulonglong, 255);

    EI_ENCODE_1(encode_ulonglong, 256);

    EI_ENCODE_1(encode_ulonglong,  0x07ffffff);

    EI_ENCODE_1(encode_ulonglong,  0x07ffffff+1);

    EI_ENCODE_1(encode_ulonglong,  0x7fffffff);

    EI_ENCODE_1(encode_ulonglong,  0x80000000);

    EI_ENCODE_1(encode_ulonglong,  0xffffffff);

    EI_ENCODE_1(encode_ulonglong,  ll(0xffffffffffff));

    EI_ENCODE_1(encode_ulonglong,  ll(0xffffffffffffffff));

#endif /* !VXWORKS */

    report(1);
}


/* ******************************************************************** */

TESTCASE(test_ei_encode_char)
{
    EI_ENCODE_1(encode_char, 0);

    EI_ENCODE_1(encode_char, 0x7f);

    EI_ENCODE_1(encode_char, 0xff);

    report(1);
}

/* ******************************************************************** */

TESTCASE(test_ei_encode_misc)
{
    EI_ENCODE_0(encode_version);

    EI_ENCODE_1(encode_double, 0.0);

    EI_ENCODE_1(encode_double, -1.0);

    EI_ENCODE_1(encode_double, 1.0);

    EI_ENCODE_1(encode_boolean, 0) /* Only case it should be false */;

    EI_ENCODE_1(encode_boolean, 1);

    EI_ENCODE_1(encode_boolean, 42);

    EI_ENCODE_1(encode_boolean, -1);

    EI_ENCODE_1(encode_atom, "foo");
    EI_ENCODE_2(encode_atom_len, "foo", 3);

    EI_ENCODE_1(encode_atom, "");
    EI_ENCODE_2(encode_atom_len, "", 0);

    EI_ENCODE_1(encode_atom, "ÅÄÖåäö");
    EI_ENCODE_2(encode_atom_len, "ÅÄÖåäö", 6);

    EI_ENCODE_1(encode_string, "foo");
    EI_ENCODE_2(encode_string_len, "foo", 3);

    EI_ENCODE_1(encode_string, "");
    EI_ENCODE_2(encode_string_len, "", 0);

    EI_ENCODE_1(encode_string, "ÅÄÖåäö");
    EI_ENCODE_2(encode_string_len, "ÅÄÖåäö", 6);

    EI_ENCODE_2(encode_binary, "foo", 3);
    EI_ENCODE_2(encode_binary, "", 0);
    EI_ENCODE_2(encode_binary, "ÅÄÖåäö", 6);

    /* FIXME check \0 in strings and atoms */

    EI_ENCODE_1(encode_tuple_header, 0);

    EI_ENCODE_0(encode_empty_list);

    report(1);
}

/* ******************************************************************** */

TESTCASE(test_ei_encode_fails)
{
    char buf[1024];
    int index;

    /* FIXME the ei_x versions are not tested */

    index = 0;
    if (ei_encode_atom(buf, &index, "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx") != 0) {
	fail("could not encode atom with 255 chars");
    }
    message("Encoding atom with 255 chars, encoded %d",index);
    if (index != 255+3) {
	fail("encoded with incorrect size");
    }
    send_buffer(buf, index);

    index = 0;
    if (ei_encode_atom_len(buf, &index, "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx", 255) != 0) {
	fail("could not encode atom with 255 chars");
    }
    message("Encoding atom with 255 chars, encoded %d",index);
    if (index != 255+3) {
	fail("encoded with incorrect size");
    }
    send_buffer(buf, index);

    index = 0;
    if (ei_encode_atom(buf, &index, "yyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy") != 0) {
	fail("could not encode atom with 256 chars, truncated to 255");
    }
    message("Encoding atom with 256 chars, encoded %d",index);
    if (index != 255+3) {
	fail("did not truncate at 255 chars");
    }
    send_buffer(buf, index);

    index = 0;
    if (ei_encode_atom_len(buf, &index, "yyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy", 256) != 0) {
	fail("could not encode atom with 256 chars, truncated to 255");
    }
    message("Encoding atom with 256 chars, encoded %d",index);
    if (index != 255+3) {
	fail("did not truncate at 255 chars");
    }
    send_buffer(buf, index);

    /* ---------------------------------------------------------------------- */

    index = 0;
    if (ei_encode_tuple_header(buf, &index, 1) != 0) {
	fail("could not create tuple header arity 1, take 1");
    }
    if (ei_encode_tuple_header(buf, &index, 1) != 0) {
	fail("could not create tuple header arity 1, take 2");
    }
    if (ei_encode_tuple_header(buf, &index, 1) != 0) {
	fail("could not create tuple header arity 1, take 3");
    }
    if (ei_encode_tuple_header(buf, &index, 0) != 0) {
	fail("could not create tuple header arity 0");
    }
    send_buffer(buf, index);

    report(1);
}

/* ******************************************************************** */

TESTCASE(test_ei_encode_utf8_atom)
{

  EI_ENCODE_3(encode_atom_as, "Å", ERLANG_LATIN1, ERLANG_UTF8);
  EI_ENCODE_3(encode_atom_as, "Å", ERLANG_LATIN1, ERLANG_LATIN1);
  EI_ENCODE_3(encode_atom_as, "\303\205",
	      ERLANG_UTF8, ERLANG_LATIN1);
  EI_ENCODE_3(encode_atom_as, "\303\205",
	      ERLANG_UTF8, ERLANG_UTF8);

  EI_ENCODE_3(encode_atom_as, "A", ERLANG_ASCII, ERLANG_UTF8);
  EI_ENCODE_3(encode_atom_as, "A", ERLANG_ASCII, ERLANG_LATIN1);

  EI_ENCODE_3_FAIL(encode_atom_as, "Å", ERLANG_UTF8, ERLANG_UTF8);
  EI_ENCODE_3_FAIL(encode_atom_as, "Å", ERLANG_UTF8, ERLANG_LATIN1);
  EI_ENCODE_3_FAIL(encode_atom_as, "Å", ERLANG_ASCII, ERLANG_UTF8);
  EI_ENCODE_3_FAIL(encode_atom_as, "Å", ERLANG_ASCII, ERLANG_LATIN1);

  EI_ENCODE_3_FAIL(encode_atom_as, "yyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy", ERLANG_ASCII, ERLANG_LATIN1);
  EI_ENCODE_3_FAIL(encode_atom_as, "yyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy", ERLANG_LATIN1, ERLANG_LATIN1);

  EI_ENCODE_3_FAIL(encode_atom_as, "yyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy", ERLANG_UTF8, ERLANG_LATIN1);

  report(1);
}

TESTCASE(test_ei_encode_utf8_atom_len)
{

  EI_ENCODE_4(encode_atom_len_as, "ÅÄÖ", 1, ERLANG_LATIN1, ERLANG_UTF8);
  EI_ENCODE_4(encode_atom_len_as, "ÅÄÖ", 2, ERLANG_LATIN1, ERLANG_LATIN1);
  EI_ENCODE_4(encode_atom_len_as, "\303\205\303\204\303\226", 2,
	      ERLANG_UTF8, ERLANG_LATIN1);
  EI_ENCODE_4(encode_atom_len_as, "\303\205\303\204\303\226", 4,
	      ERLANG_UTF8, ERLANG_UTF8);

  EI_ENCODE_4(encode_atom_len_as, "ABC", 1, ERLANG_ASCII, ERLANG_UTF8);
  EI_ENCODE_4(encode_atom_len_as, "ABC", 2, ERLANG_ASCII, ERLANG_LATIN1);

  EI_ENCODE_4_FAIL(encode_atom_len_as, "ÅÄÖ", 1, ERLANG_UTF8, ERLANG_UTF8);
  EI_ENCODE_4_FAIL(encode_atom_len_as, "ÅÄÖ", 2, ERLANG_UTF8, ERLANG_LATIN1);
  EI_ENCODE_4_FAIL(encode_atom_len_as, "ÅÄÖ", 3, ERLANG_ASCII, ERLANG_UTF8);
  EI_ENCODE_4_FAIL(encode_atom_len_as, "ÅÄÖ", 4, ERLANG_ASCII, ERLANG_LATIN1);

  EI_ENCODE_4(encode_atom_len_as, "yyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy", 255, ERLANG_ASCII, ERLANG_LATIN1);

  EI_ENCODE_4_FAIL(encode_atom_len_as, "\303\205\303\204\303\226", 1,
		   ERLANG_UTF8, ERLANG_LATIN1);

  report(1);
}
