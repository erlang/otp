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
 * Purpose: Read pids, funs and others without real meaning on the C side 
 *          and pass it back to Erlang to test that it is still the same. 
 * Author:  kent@erix.ericsson.se
 */

#define EI_DECODE_ENCODE(FUNC,TYPE) \
  { \
    char *buf; \
    char buf2[1024]; \
    TYPE p; \
    int size1 = 0; \
    int size2 = 0; \
    int size3 = 0; \
    int err; \
    ei_x_buff arg; \
\
    message("ei_decode_" #FUNC ", arg is type " #TYPE); \
    buf = read_packet(NULL); \
    err = ei_decode_ ## FUNC(buf+1, &size1, &p); \
    if (err != 0) { \
      if (err != -1) { \
	fail("decode returned non zero but not -1"); \
      } else { \
	fail("decode returned non zero"); \
      } \
      return; \
    } \
    if (size1 < 1) { \
      fail("size is < 1"); \
      return; \
    } \
\
    message("ei_encode_" #FUNC " buf is NULL, arg is type " #TYPE); \
    err = ei_encode_ ## FUNC(NULL, &size2, &p); \
    if (err != 0) { \
      if (err != -1) { \
	fail("size calculation returned non zero but not -1"); \
	return; \
      } else { \
	fail("size calculation returned non zero"); \
	return; \
      } \
    } \
    if (size1 != size2) { \
      message("size1 = %d, size2 = %d\n",size1,size2); \
      fail("decode and encode size differs when buf is NULL"); \
      return; \
    } \
    message("ei_encode_" #FUNC ", arg is type " #TYPE); \
    err = ei_encode_ ## FUNC(buf2, &size3, &p); \
    if (err != 0) { \
      if (err != -1) { \
	fail("returned non zero but not -1"); \
      } else { \
	fail("returned non zero"); \
      } \
      return; \
    } \
    if (size1 != size3) { \
      message("size1 = %d, size2 = %d\n",size1,size3); \
      fail("decode and encode size differs"); \
      return; \
    } \
    send_buffer(buf2, size1); \
\
    message("ei_x_encode_" #FUNC ", arg is type " #TYPE); \
    ei_x_new(&arg); \
    err = ei_x_encode_ ## FUNC(&arg, &p); \
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

#define EI_DECODE_ENCODE_BIG(FUNC,TYPE) \
  { \
    char *buf; \
    char buf2[2048]; \
    TYPE *p; \
    int size1 = 0; \
    int size2 = 0; \
    int size3 = 0; \
    int err, index = 0, len, type; \
    ei_x_buff arg; \
\
    message("ei_decode_" #FUNC ", arg is type " #TYPE); \
    buf = read_packet(NULL); \
    ei_get_type(buf+1, &index, &type, &len); \
    p = ei_alloc_big(len); \
    err = ei_decode_ ## FUNC(buf+1, &size1, p); \
    if (err != 0) { \
      if (err != -1) { \
	fail("decode returned non zero but not -1"); \
      } else { \
	fail("decode returned non zero"); \
      } \
      return; \
    } \
    if (size1 < 1) { \
      fail("size is < 1"); \
      return; \
    } \
\
    message("ei_encode_" #FUNC " buf is NULL, arg is type " #TYPE); \
    err = ei_encode_ ## FUNC(NULL, &size2, p); \
    if (err != 0) { \
      if (err != -1) { \
	fail("size calculation returned non zero but not -1"); \
	return; \
      } else { \
	fail("size calculation returned non zero"); \
	return; \
      } \
    } \
    if (size1 != size2) { \
      message("size1 = %d, size2 = %d\n",size1,size2); \
      fail("decode and encode size differs when buf is NULL"); \
      return; \
    } \
    message("ei_encode_" #FUNC ", arg is type " #TYPE); \
    err = ei_encode_ ## FUNC(buf2, &size3, p); \
    if (err != 0) { \
      if (err != -1) { \
	fail("returned non zero but not -1"); \
      } else { \
	fail("returned non zero"); \
      } \
      return; \
    } \
    if (size1 != size3) { \
      message("size1 = %d, size2 = %d\n",size1,size3); \
      fail("decode and encode size differs"); \
      return; \
    } \
    send_buffer(buf2, size1); \
\
    message("ei_x_encode_" #FUNC ", arg is type " #TYPE); \
    ei_x_new(&arg); \
    err = ei_x_encode_ ## FUNC(&arg, p); \
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
    ei_free_big(p); \
  }




/* ******************************************************************** */

TESTCASE(test_ei_decode_encode)
{
    EI_DECODE_ENCODE(fun  , erlang_fun);
    EI_DECODE_ENCODE(pid  , erlang_pid);
    EI_DECODE_ENCODE(port , erlang_port);
    EI_DECODE_ENCODE(ref  , erlang_ref);
    EI_DECODE_ENCODE(trace, erlang_trace);

    EI_DECODE_ENCODE_BIG(big  , erlang_big);
    EI_DECODE_ENCODE_BIG(big  , erlang_big);
    EI_DECODE_ENCODE_BIG(big  , erlang_big);

    EI_DECODE_ENCODE_BIG(big  , erlang_big);
    EI_DECODE_ENCODE_BIG(big  , erlang_big);
    EI_DECODE_ENCODE_BIG(big  , erlang_big);

    /* Test large node containers... */
    EI_DECODE_ENCODE(pid  , erlang_pid);
    EI_DECODE_ENCODE(port , erlang_port);
    EI_DECODE_ENCODE(ref  , erlang_ref);
    EI_DECODE_ENCODE(pid  , erlang_pid);
    EI_DECODE_ENCODE(port , erlang_port);
    EI_DECODE_ENCODE(ref  , erlang_ref);

    report(1);
}

/* ******************************************************************** */
