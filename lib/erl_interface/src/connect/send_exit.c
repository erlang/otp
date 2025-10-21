/*
 * %CopyrightBegin%
 *
 * SPDX-License-Identifier: Apache-2.0
 *
 * Copyright Ericsson AB 1998-2025. All Rights Reserved.
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
#ifdef __WIN32__
# include <winsock2.h>
# include <windows.h>
# include <winbase.h>
#else /* Unix */
#include <unistd.h>
#endif

#include <string.h>
#include <stdlib.h>
#include "eidef.h"
#include "eiext.h"
#include "eisend.h"
#include "ei_connect_int.h"
#include "ei_trace.h"
#include "ei_internal.h"
#include "putget.h"
#include "ei_portio.h"
#include "show_msg.h"

/* use this to break a link */
int ei_send_exit(int fd, const erlang_pid *from, 
		 const erlang_pid *to, const char *reason)
{
    return ei_send_exit_tmo(fd,from,to,reason,0);
}


int ei_send_exit_tmo(int fd, const erlang_pid *from, const erlang_pid *to,
		     const char *reason, unsigned ms)
{
  char sbuf[EISMALLBUF];
  erlang_trace *token = NULL;
  char *dbuf = NULL;
  char *msgbuf;
  char *s;
  int index = 0;
  int len = strlen(reason) + 1080; /* see below */
  ei_socket_callbacks *cbs;
  void *ctx;
  int err;
  ssize_t wlen;
  unsigned tmo = ms == 0 ? EI_SCLBK_INF_TMO : ms;
  
  err = EI_GET_CBS_CTX__(&cbs, &ctx, fd);
  if (err) {
      EI_CONN_SAVE_ERRNO__(err);
      return ERL_ERROR;
  }

  if (len > EISMALLBUF) {
      if (!(dbuf = malloc(len))) {
          EI_CONN_SAVE_ERRNO__(ENOMEM);
          return ERL_ERROR;
      }
  }
  msgbuf = (dbuf ? dbuf : sbuf);


  /* are we tracing? */
  /* check that he can receive trace tokens first */
  if (ei_distversion(fd) > 0) token = ei_trace(0,NULL);

  EI_CONN_SAVE_ERRNO__(EINVAL);
  
  index = 5;                                     /* max sizes: */
  if (ei_encode_version(msgbuf,&index) < 0)             /*   1 */
      return ERL_ERROR;
  if (token) {
      if (ei_encode_tuple_header(msgbuf,&index,5) < 0)  /*   2 */
          return ERL_ERROR;
      if (ei_encode_long(msgbuf,&index,ERL_EXIT_TT) < 0)/*   2 */
          return ERL_ERROR;
  }
  else {
      if (ei_encode_tuple_header(msgbuf,&index,4) < 0)
          return ERL_ERROR;
      if (ei_encode_long(msgbuf,&index,ERL_EXIT) < 0)
          return ERL_ERROR;
  }
  if (ei_encode_pid(msgbuf,&index,from) < 0)            /* 268 */
      return ERL_ERROR;
  if (ei_encode_pid(msgbuf,&index,to) < 0)              /* 268 */
      return ERL_ERROR;

  if (token) {
      if (ei_encode_trace(msgbuf,&index,token) < 0)     /* 534 */
          return ERL_ERROR;
  }

  /* Reason */
  if (ei_encode_string(msgbuf,&index,reason) < 0)       /* len */
      return ERL_ERROR;

  /* 5 byte header missing */
  s = msgbuf;
  put32be(s, index - 4);                                /*   4 */
  put8(s, ERL_PASS_THROUGH);                            /*   1 */
                                          /*** sum: len + 1080 */
  if (ei_tracelevel >= 4) {
      if (ei_show_sendmsg(stderr,msgbuf,NULL) < 0)
          return ERL_ERROR;
  }

  wlen = (ssize_t) index;
  err = ei_write_fill_ctx_t__(cbs, ctx, msgbuf, &wlen, tmo);
  if (!err && wlen != (ssize_t) index)
      err = EIO;
  if (dbuf)
      free(dbuf);
  if (err) {
      EI_CONN_SAVE_ERRNO__(err);
      return ERL_ERROR;
  }

  erl_errno = 0;
  return 0;
}

