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
 */
#ifdef __WIN32__
# include <winsock2.h>
# include <windows.h>
# include <winbase.h>
#else /* Unix/VxWorks */
# include <unistd.h>
#endif

/* common */
#include <string.h>
#include <stdlib.h>
#include <errno.h>

#include "eidef.h"
#include "eiext.h"
#include "eirecv.h"
#include "ei_portio.h"
#include "ei_internal.h"
#include "putget.h"
#include "ei_trace.h"
#include "show_msg.h"

#include <errno.h>

#define EIRECVBUF 2048 /* largest possible header is approx 1300 bytes */

/* length (4), PASS_THOUGH (1), header, message */
int 
ei_recv_internal (int fd, 
		  char **mbufp, int *bufsz, 
		  erlang_msg *msg, int *msglenp, 
		  int staticbufp, unsigned ms)
{
  char header[EIRECVBUF];
  char *s=header;
  char *mbuf=*mbufp;
  int len = 0;
  int msglen = 0;
  int bytesread = 0;
  int remain;
  int arity;
  int version;
  int index = 0;
  int i = 0;
  int res;
  int show_this_msg = 0;

  /* get length field */
  if ((res = ei_read_fill_t(fd, header, 4, ms)) != 4) 
  {
      erl_errno = (res == -2) ? ETIMEDOUT : EIO;
      return -1;
  }
  len = get32be(s);

  /* got tick - respond and return */
  if (!len) {
    char tock[] = {0,0,0,0};
    ei_write_fill_t(fd, tock, sizeof(tock), ms); /* Failure no problem */
    *msglenp = 0;
    return 0;			/* maybe flag ERL_EAGAIN [sverkerw] */
  }
  
  /* turn off tracing on each receive. it will be turned back on if
   * we receive a trace token.
   */
  ei_trace(-1,NULL);
  
  /* read enough to get at least entire header */
  bytesread = (len > EIRECVBUF ? EIRECVBUF : len); 
  if ((i = ei_read_fill_t(fd,header,bytesread,ms)) != bytesread) {
      erl_errno = (i == -2) ? ETIMEDOUT : EIO;
      return -1;
  }

  /* now decode header */
  /* pass-through, version, control tuple header, control message type */
  s = header;
  index = 1;
  if ((get8(s) != ERL_PASS_THROUGH)
      || ei_decode_version(header,&index,&version)
      || (version != ERL_VERSION_MAGIC) 
      || ei_decode_tuple_header(header,&index,&arity) 
      || ei_decode_long(header,&index,&msg->msgtype))
  {
      erl_errno = EIO;	/* Maybe another code for decoding errors */
      return -1;
  }
  
  switch (msg->msgtype) {
  case ERL_SEND:          /* { SEND, Cookie, ToPid } */
    if (ei_tracelevel >= 4) show_this_msg = 1;
    if (ei_decode_atom_as(header,&index,msg->cookie,sizeof(msg->cookie),ERLANG_UTF8,NULL,NULL) 
	|| ei_decode_pid(header,&index,&msg->to))
    {
	erl_errno = EIO;
	return -1;
    }

    break;

  case ERL_REG_SEND:     /* { REG_SEND, From, Cookie, ToName } */
    if (ei_tracelevel >= 4) show_this_msg = 1;
    if (ei_decode_pid(header,&index,&msg->from) 
	|| ei_decode_atom_as(header,&index,msg->cookie,sizeof(msg->cookie),ERLANG_UTF8,NULL,NULL) 
	|| ei_decode_atom_as(header,&index,msg->toname,sizeof(msg->toname),ERLANG_UTF8,NULL,NULL))
    {
	erl_errno = EIO;
	return -1;
    }

    /* actual message is remaining part of headerbuf, plus any unread bytes */
    break;

  case ERL_LINK:         /* { LINK, From, To } */
  case ERL_UNLINK:       /* { UNLINK, From, To } */
  case ERL_GROUP_LEADER: /* { GROUP_LEADER, From, To } */
    if (ei_tracelevel >= 4) show_this_msg = 1;
    if (ei_decode_pid(header,&index,&msg->from) 
	|| ei_decode_pid(header,&index,&msg->to))
    {
	erl_errno = EIO;
	return -1;
    }

    break;
    
  case ERL_EXIT:         /* { EXIT, From, To, Reason } */
  case ERL_EXIT2:        /* { EXIT2, From, To, Reason } */
    if (ei_tracelevel >= 4) show_this_msg = 1;
    if (ei_decode_pid(header,&index,&msg->from) 
	|| ei_decode_pid(header,&index,&msg->to))
    {
	erl_errno = EIO;
	return -1;
    }

    break;
    
  case ERL_SEND_TT:      /* { SEND_TT, Cookie, ToPid, TraceToken } */
    if (ei_tracelevel >= 4) show_this_msg = 1;
    if (ei_decode_atom_as(header,&index,msg->cookie,sizeof(msg->cookie),ERLANG_UTF8,NULL,NULL) 
	|| ei_decode_pid(header,&index,&msg->to)
	|| ei_decode_trace(header,&index,&msg->token))
    {
	erl_errno = EIO;
	return -1;
    }

    ei_trace(1,&msg->token); /* turn on tracing */
    break;

  case ERL_REG_SEND_TT: /* { REG_SEND_TT, From, Cookie, ToName, TraceToken } */
    if (ei_tracelevel >= 4) show_this_msg = 1;
    if (ei_decode_pid(header,&index,&msg->from) 
	|| ei_decode_atom_as(header,&index,msg->cookie,sizeof(msg->cookie),ERLANG_UTF8,NULL,NULL) 
	|| ei_decode_atom_as(header,&index,msg->toname,sizeof(msg->toname),ERLANG_UTF8,NULL,NULL)
	|| ei_decode_trace(header,&index,&msg->token))
    {
	erl_errno = EIO;
	return -1;
    }

    ei_trace(1,&msg->token); /* turn on tracing */
    break;

  case ERL_EXIT_TT:     /* { EXIT_TT, From, To, TraceToken, Reason } */
  case ERL_EXIT2_TT:    /* { EXIT2_TT, From, To, TraceToken, Reason } */
    if (ei_tracelevel >= 4) show_this_msg = 1;
    if (ei_decode_pid(header,&index,&msg->from) 
	|| ei_decode_pid(header,&index,&msg->to)
	|| ei_decode_trace(header,&index,&msg->token))
    {
	erl_errno = EIO;
	return -1;
    }

    ei_trace(1,&msg->token); /* turn on tracing */
    break;

  default:
    /* unknown type, just put any remaining bytes into buffer */
    break;
  }

  /* actual message is remaining part of headerbuf, plus any unread bytes */
  msglen = len - index;     /* message size (payload) */
  remain = len - bytesread; /* bytes left to read */

  /* if callers buffer is too small, we flush in the rest of the
   * message and discard it, unless we know that we can reallocate
   * the buffer in which case we do that and read the message.
   */
  if (msglen > *bufsz) {
    if (staticbufp) {
      int sz = EIRECVBUF;
      /* flush in rest of packet */
      while (remain > 0) {
	if (remain < sz) sz = remain;
	if ((i=ei_read_fill_t(fd,header,sz,ms)) <= 0) break;
	remain -= i;
      }
      erl_errno = EMSGSIZE;
      return -1;
    }
    else {
	/* Dynamic buffer --- grow it. */
#ifdef DEBUG
      fprintf(stderr, "Growing buffer from %d bytes to %d bytes\n",
	      *bufsz, msglen);
#endif
      if ((mbuf = realloc(*mbufp, msglen)) == NULL)
      {
	  erl_errno = ENOMEM;
	  return -1;
      }

      *mbufp = mbuf;
      *bufsz = msglen;
    }  
  }
  
  /* move remaining bytes to callers buffer */
  memmove(mbuf,header+index,bytesread-index);

  /* let the caller know how big the message is in his buffer */
  *msglenp = msglen;

  /* read the rest of the message into callers buffer */
  if (remain > 0) {
    if ((i = ei_read_fill_t(fd,mbuf+bytesread-index,remain,ms)) != remain) {
      *msglenp = bytesread-index+1; /* actual bytes in users buffer */
      erl_errno = (i == -2) ? ETIMEDOUT : EIO;
      return -1;
    }
  }

  if (show_this_msg)
      ei_show_recmsg(stderr,msg,mbuf);

  /* the caller only sees "untraced" message types */
  /* the trace token is buried in the message struct */
  if (msg->msgtype > 10) msg->msgtype -= 10;
  
  return msg->msgtype;
}

int ei_receive_encoded(int fd, char **mbufp, int *bufsz,
		       erlang_msg *msg, int *msglen)
{
  return ei_recv_internal(fd, mbufp, bufsz, msg, msglen, 0, 0);
}

int ei_receive_encoded_tmo(int fd, char **mbufp, int *bufsz, erlang_msg *msg, int *msglen, unsigned ms)
{
  return ei_recv_internal(fd, mbufp, bufsz, msg, msglen, 0, ms);
}

