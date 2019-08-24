/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1998-2016. All Rights Reserved.
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
#ifdef __WIN32__
#include <winsock2.h>
#include <windows.h>
#include <winbase.h>

#elif VXWORKS
#include <unistd.h>

#else /* unix */
#include <unistd.h>

#endif

#include <string.h>
#include <stdlib.h>
#include "eidef.h"
#include "eiext.h"
#include "eisend.h"
#include "ei_internal.h"
#include "putget.h"
#include "erl_rport.h"


/* this sends either link or unlink ('which' decides) */
static int link_unlink(int fd, const erlang_pid *from, const erlang_pid *to,
		       int which, unsigned ms)
{
  char msgbuf[EISMALLBUF];
  char *s;
  int index = 0;
  int n;
  unsigned tmo = ms == 0 ? EI_SCLBK_INF_TMO : ms;

  index = 5;                                     /* max sizes: */
  ei_encode_version(msgbuf,&index);                     /*   1 */
  ei_encode_tuple_header(msgbuf,&index,3);
  ei_encode_long(msgbuf,&index,which);
  ei_encode_pid(msgbuf,&index,from);                    /* 268 */
  ei_encode_pid(msgbuf,&index,to);                      /* 268 */

  /* 5 byte header missing */
  s = msgbuf;
  put32be(s, index - 4);                                /*   4 */
  put8(s, ERL_PASS_THROUGH);                            /*   1 */
                                                  /* sum:  542 */


#ifdef DEBUG_DIST
  if (ei_trace_distribution > 1) ei_show_sendmsg(stderr,msgbuf,NULL);
#endif

  n = ei_write_fill_t__(fd,msgbuf,index,tmo); 

  return (n==index ? 0 : -1);
}

/* FIXME not used? */
#if 0
/* use this to send a link */
int ei_send_unlink(int fd, const erlang_pid *from, const erlang_pid *to)
{
  return link_unlink(fd, from, to, ERL_UNLINK,0);
}

/* use this to send an unlink */
int ei_send_link(int fd, const erlang_pid *from, const erlang_pid *to)
{
  return link_unlink(fd, from, to, ERL_LINK,0);
}
/* use this to send a link */
int ei_send_unlink_tmo(int fd, const erlang_pid *from, const erlang_pid *to,
		       unsigned ms)
{
  return link_unlink(fd, from, to, ERL_UNLINK,ms);
}

/* use this to send an unlink */
int ei_send_link_tmo(int fd, const erlang_pid *from, const erlang_pid *to,
		     unsigned ms)
{
  return link_unlink(fd, from, to, ERL_LINK,ms);
}
#endif
