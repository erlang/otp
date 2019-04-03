/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2002-2016. All Rights Reserved.
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
 *

 */
#ifndef _EI_INTERNAL_H
#define _EI_INTERNAL_H

#ifdef EI_HIDE_REAL_ERRNO
#  define EI_CONN_SAVE_ERRNO__(E) \
    ((E) == ETIMEDOUT ? (erl_errno = ETIMEDOUT) : (erl_errno = EIO))
#else
#  define EI_CONN_SAVE_ERRNO__(E) \
    (erl_errno = (E))
#endif

/* 
 * Some useful stuff not to be exported to users.
 */

#ifdef __WIN32__
#define MAXPATHLEN 256
#endif

/* 
 * Trace functions
 *
 * The variable ei_tracelevel means
 *      0    No tracing
 *      1    Write verbose error messages
 *      2    Write verbose warning messages + 1
 *      3    Write progress reports for connect handlin + 1 + 2
 *      4    Write progress reports for communication + 1 + 2 + 3
 *      5    Write progress reports for data conversion + 1 + 2 + 3 + 4
 *
 */

#define EI_TRACE_ERR0(NAME,FORMAT) \
  {if (ei_tracelevel >= 1) ei_trace_printf(NAME,1,FORMAT);}
#define EI_TRACE_ERR1(NAME,FORMAT,ARG1)  \
  {if (ei_tracelevel >= 1) ei_trace_printf(NAME,1,FORMAT,ARG1);}
#define EI_TRACE_ERR2(NAME,FORMAT,ARG1,ARG2)  \
  {if (ei_tracelevel >= 1) ei_trace_printf(NAME,1,FORMAT,ARG1,ARG2);}
#define EI_TRACE_ERR3(NAME,FORMAT,ARG1,ARG2,ARG3)  \
  {if (ei_tracelevel >= 1) ei_trace_printf(NAME,1,FORMAT,ARG1,ARG2,ARG3);}
#define EI_TRACE_ERR4(NAME,FORMAT,ARG1,ARG2,ARG3,ARG4)  \
  {if (ei_tracelevel >= 1) ei_trace_printf(NAME,1,FORMAT,ARG1,ARG2,ARG3,ARG4);}
#define EI_TRACE_ERR5(NAME,FORMAT,ARG1,ARG2,ARG3,ARG4,ARG5)  \
  {if (ei_tracelevel >= 1) ei_trace_printf(NAME,1,FORMAT,ARG1,ARG2,ARG3,ARG4, \
                                                         ARG5);}
#define EI_TRACE_ERR6(NAME,FORMAT,ARG1,ARG2,ARG3,ARG4,ARG5,ARG6)  \
  {if (ei_tracelevel >= 1) ei_trace_printf(NAME,1,FORMAT,ARG1,ARG2,ARG3,ARG4, \
                                                         ARG5,ARG6);}
#define EI_TRACE_ERR7(NAME,FORMAT,ARG1,ARG2,ARG3,ARG4,ARG5,ARG6,ARG7)  \
  {if (ei_tracelevel >= 1) ei_trace_printf(NAME,1,FORMAT,ARG1,ARG2,ARG3,ARG4, \
                                                         ARG5,ARG6,ARG7);}

#define EI_TRACE_WARN0(NAME,FORMAT) \
  {if (ei_tracelevel >= 2) ei_trace_printf(NAME,1,FORMAT);}
#define EI_TRACE_WARN1(NAME,FORMAT,ARG1)  \
  {if (ei_tracelevel >= 2) ei_trace_printf(NAME,1,FORMAT,ARG1);}
#define EI_TRACE_WARN2(NAME,FORMAT,ARG1,ARG2)  \
  {if (ei_tracelevel >= 2) ei_trace_printf(NAME,1,FORMAT,ARG1,ARG2);}
#define EI_TRACE_WARN3(NAME,FORMAT,ARG1,ARG2,ARG3)  \
  {if (ei_tracelevel >= 2) ei_trace_printf(NAME,1,FORMAT,ARG1,ARG2,ARG3);}
#define EI_TRACE_WARN4(NAME,FORMAT,ARG1,ARG2,ARG3,ARG4)  \
  {if (ei_tracelevel >= 2) ei_trace_printf(NAME,1,FORMAT,ARG1,ARG2,ARG3,ARG4);}
#define EI_TRACE_WARN5(NAME,FORMAT,ARG1,ARG2,ARG3,ARG4,ARG5)  \
  {if (ei_tracelevel >= 2) ei_trace_printf(NAME,1,FORMAT,ARG1,ARG2,ARG3,ARG4, \
                                                         ARG5);}
#define EI_TRACE_WARN6(NAME,FORMAT,ARG1,ARG2,ARG3,ARG4,ARG5,ARG6)  \
  {if (ei_tracelevel >= 2) ei_trace_printf(NAME,1,FORMAT,ARG1,ARG2,ARG3,ARG4, \
                                                         ARG5,ARG6);}
#define EI_TRACE_WARN7(NAME,FORMAT,ARG1,ARG2,ARG3,ARG4,ARG5,ARG6,ARG7)  \
  {if (ei_tracelevel >= 2) ei_trace_printf(NAME,1,FORMAT,ARG1,ARG2,ARG3,ARG4, \
                                                         ARG5,ARG6,ARG7);}

#define EI_TRACE_CONN0(NAME,FORMAT) \
  {if (ei_tracelevel >= 3) ei_trace_printf(NAME,1,FORMAT);}
#define EI_TRACE_CONN1(NAME,FORMAT,ARG1)  \
  {if (ei_tracelevel >= 3) ei_trace_printf(NAME,1,FORMAT,ARG1);}
#define EI_TRACE_CONN2(NAME,FORMAT,ARG1,ARG2)  \
  {if (ei_tracelevel >= 3) ei_trace_printf(NAME,1,FORMAT,ARG1,ARG2);}
#define EI_TRACE_CONN3(NAME,FORMAT,ARG1,ARG2,ARG3)  \
  {if (ei_tracelevel >= 3) ei_trace_printf(NAME,1,FORMAT,ARG1,ARG2,ARG3);}
#define EI_TRACE_CONN4(NAME,FORMAT,ARG1,ARG2,ARG3,ARG4)  \
  {if (ei_tracelevel >= 3) ei_trace_printf(NAME,1,FORMAT,ARG1,ARG2,ARG3,ARG4);}
#define EI_TRACE_CONN5(NAME,FORMAT,ARG1,ARG2,ARG3,ARG4,ARG5)  \
  {if (ei_tracelevel >= 3) ei_trace_printf(NAME,1,FORMAT,ARG1,ARG2,ARG3,ARG4, \
                                                         ARG5);}
#define EI_TRACE_CONN6(NAME,FORMAT,ARG1,ARG2,ARG3,ARG4,ARG5,ARG6)  \
  {if (ei_tracelevel >= 3) ei_trace_printf(NAME,1,FORMAT,ARG1,ARG2,ARG3,ARG4, \
                                                         ARG5,ARG6);}
#define EI_TRACE_CONN7(NAME,FORMAT,ARG1,ARG2,ARG3,ARG4,ARG5,ARG6,ARG7)  \
  {if (ei_tracelevel >= 3) ei_trace_printf(NAME,1,FORMAT,ARG1,ARG2,ARG3,ARG4, \
                                                         ARG5,ARG6,ARG7);}

#define EI_TRACE_COMM0(NAME,FORMAT) \
  {if (ei_tracelevel >= 4) ei_trace_printf(NAME,1,FORMAT);}
#define EI_TRACE_COMM1(NAME,FORMAT,ARG1)  \
  {if (ei_tracelevel >= 4) ei_trace_printf(NAME,1,FORMAT,ARG1);}
#define EI_TRACE_COMM2(NAME,FORMAT,ARG1,ARG2)  \
  {if (ei_tracelevel >= 4) ei_trace_printf(NAME,1,FORMAT,ARG1,ARG2);}
#define EI_TRACE_COMM3(NAME,FORMAT,ARG1,ARG2,ARG3)  \
  {if (ei_tracelevel >= 4) ei_trace_printf(NAME,1,FORMAT,ARG1,ARG2,ARG3);}
#define EI_TRACE_COMM4(NAME,FORMAT,ARG1,ARG2,ARG3,ARG4)  \
  {if (ei_tracelevel >= 4) ei_trace_printf(NAME,1,FORMAT,ARG1,ARG2,ARG3,ARG4);}
#define EI_TRACE_COMM5(NAME,FORMAT,ARG1,ARG2,ARG3,ARG4,ARG5)  \
  {if (ei_tracelevel >= 4) ei_trace_printf(NAME,1,FORMAT,ARG1,ARG2,ARG3,ARG4, \
                                                         ARG5);}
#define EI_TRACE_COMM6(NAME,FORMAT,ARG1,ARG2,ARG3,ARG4,ARG5,ARG6)  \
  {if (ei_tracelevel >= 4) ei_trace_printf(NAME,1,FORMAT,ARG1,ARG2,ARG3,ARG4, \
                                                         ARG5,ARG6);}
#define EI_TRACE_COMM7(NAME,FORMAT,ARG1,ARG2,ARG3,ARG4,ARG5,ARG6,ARG7)  \
  {if (ei_tracelevel >= 4) ei_trace_printf(NAME,1,FORMAT,ARG1,ARG2,ARG3,ARG4, \
                                                         ARG5,ARG6,ARG7);}

#define EI_TRACE0(NAME,FORMAT) \
  {if (ei_tracelevel >= 5) ei_trace_printf(NAME,1,FORMAT);}
#define EI_TRACE1(NAME,FORMAT,ARG1)  \
  {if (ei_tracelevel >= 5) ei_trace_printf(NAME,1,FORMAT,ARG1);}
#define EI_TRACE2(NAME,FORMAT,ARG1,ARG2)  \
  {if (ei_tracelevel >= 5) ei_trace_printf(NAME,1,FORMAT,ARG1,ARG2);}
#define EI_TRACE3(NAME,FORMAT,ARG1,ARG2,ARG3)  \
  {if (ei_tracelevel >= 5) ei_trace_printf(NAME,1,FORMAT,ARG1,ARG2,ARG3);}
#define EI_TRACE4(NAME,FORMAT,ARG1,ARG2,ARG3,ARG4)  \
  {if (ei_tracelevel >= 5) ei_trace_printf(NAME,1,FORMAT,ARG1,ARG2,ARG3,ARG4);}
#define EI_TRACE5(NAME,FORMAT,ARG1,ARG2,ARG3,ARG4,ARG5)  \
  {if (ei_tracelevel >= 5) ei_trace_printf(NAME,1,FORMAT,ARG1,ARG2,ARG3,ARG4, \
                                                         ARG5);}
#define EI_TRACE6(NAME,FORMAT,ARG1,ARG2,ARG3,ARG4,ARG5,ARG6)  \
  {if (ei_tracelevel >= 5) ei_trace_printf(NAME,1,FORMAT,ARG1,ARG2,ARG3,ARG4, \
                                                         ARG5,ARG6);}
#define EI_TRACE7(NAME,FORMAT,ARG1,ARG2,ARG3,ARG4,ARG5,ARG6,ARG7)  \
  {if (ei_tracelevel >= 5) ei_trace_printf(NAME,1,FORMAT,ARG1,ARG2,ARG3,ARG4, \
                                                         ARG5,ARG6,ARG7);}

extern int ei_tracelevel;

int ei_init_connect(void);

void ei_trace_printf(const char *name, int level, const char *format, ...);

int ei_internal_use_21_bitstr_expfun(void);

int ei_get_cbs_ctx__(ei_socket_callbacks **cbs, void **ctx, int fd);

#endif /* _EI_INTERNAL_H */
