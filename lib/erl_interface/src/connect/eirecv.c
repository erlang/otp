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
#include "ei_connect_int.h"

#include <errno.h>

#define EIRECVBUF 2048 /* largest possible header is approx 1300 bytes */

static int
send_unlink_id_ack(ei_socket_callbacks *cbs, void *ctx,
                   char *id_ext, size_t id_size,
                   erlang_pid *from, erlang_pid *to,
                   unsigned tmo);

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
  int err;
  int show_this_msg = 0;
  ei_socket_callbacks *cbs;
  void *ctx;
  ssize_t rlen;
  unsigned tmo = ms == 0 ? EI_SCLBK_INF_TMO : ms;

  err = EI_GET_CBS_CTX__(&cbs, &ctx, fd);
  if (err) {
      EI_CONN_SAVE_ERRNO__(err);
      return ERL_ERROR;
  }

  /* get length field */
  rlen = 4;
  err = ei_read_fill_ctx_t__(cbs, ctx, header, &rlen, tmo);
  if (!err && rlen != 4)
      err = EIO;
  if (err) {
      EI_CONN_SAVE_ERRNO__(err);
      return ERL_ERROR;
  }
  
  len = get32be(s);

  /* got tick - respond and return */
  if (!len) {
    char tock[] = {0,0,0,0};
    ssize_t wlen = sizeof(tock);
    ei_write_fill_ctx_t__(cbs, ctx, tock, &wlen, tmo); /* Failure no problem */
    *msglenp = 0;
    return ERL_TICK;
  }
  
  /* turn off tracing on each receive. it will be turned back on if
   * we receive a trace token.
   */
  ei_trace(-1,NULL);
  
  /* read enough to get at least entire header */
  rlen = bytesread = (len > EIRECVBUF ? EIRECVBUF : len);
  err = ei_read_fill_ctx_t__(cbs, ctx, header, &rlen, tmo);
  if (!err && rlen != bytesread)
      err = EIO;
  if (err) {
      EI_CONN_SAVE_ERRNO__(err);
      return ERL_ERROR;
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
      EI_CONN_SAVE_ERRNO__(EBADMSG);      
      return ERL_ERROR;
  }
  
  switch (msg->msgtype) {
  case ERL_SEND:          /* { SEND, Cookie, ToPid } */
    if (ei_tracelevel >= 4) show_this_msg = 1;
    if (ei_decode_atom_as(header,&index,msg->cookie,sizeof(msg->cookie),ERLANG_UTF8,NULL,NULL) 
	|| ei_decode_pid(header,&index,&msg->to))
    {
        EI_CONN_SAVE_ERRNO__(EBADMSG);      
        return ERL_ERROR;
    }

    break;

  case ERL_REG_SEND:     /* { REG_SEND, From, Cookie, ToName } */
    if (ei_tracelevel >= 4) show_this_msg = 1;
    if (ei_decode_pid(header,&index,&msg->from) 
	|| ei_decode_atom_as(header,&index,msg->cookie,sizeof(msg->cookie),ERLANG_UTF8,NULL,NULL) 
	|| ei_decode_atom_as(header,&index,msg->toname,sizeof(msg->toname),ERLANG_UTF8,NULL,NULL))
    {
        EI_CONN_SAVE_ERRNO__(EBADMSG);      
        return ERL_ERROR;
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
        EI_CONN_SAVE_ERRNO__(EBADMSG);      
        return ERL_ERROR;
    }

    break;

  case ERL_UNLINK_ID: { /* {UNLINK_ID, Id, From, To} */
      int id;
      size_t id_size;

      /* Expose old ERL_UNLINK tag to user... */
      msg->msgtype = ERL_UNLINK;

      if (ei_tracelevel >= 4) show_this_msg = 1;
      /* Save id on external format... */
      id = index;
      if (ei_skip_term(header, &index)) {
          EI_CONN_SAVE_ERRNO__(EBADMSG);      
          return ERL_ERROR;
      }
      id_size = index - id;

      if (ei_decode_pid(header,&index,&msg->from) 
          || ei_decode_pid(header,&index,&msg->to)) {
          EI_CONN_SAVE_ERRNO__(EBADMSG);      
          return ERL_ERROR;
      }
      
      if (send_unlink_id_ack(cbs, ctx, &header[0] + id, id_size,
                             &msg->to, &msg->from, tmo)) {
          return ERL_ERROR;
      }
          
      break;
  }

  case ERL_UNLINK_ID_ACK: /* {UNLINK_ID_ACK, Id, From, To} */
      /*
       * ei currently do not support setting up links and removing
       * links from the ei side, so an 'unlink id ack' signal should
       * never arrive...
       */
      EI_CONN_SAVE_ERRNO__(EBADMSG);      
      return ERL_ERROR;

  case ERL_EXIT:         /* { EXIT, From, To, Reason } */
  case ERL_EXIT2:        /* { EXIT2, From, To, Reason } */
    if (ei_tracelevel >= 4) show_this_msg = 1;
    if (ei_decode_pid(header,&index,&msg->from) 
	|| ei_decode_pid(header,&index,&msg->to))
    {
        EI_CONN_SAVE_ERRNO__(EBADMSG);      
        return ERL_ERROR;
    }

    break;
    
  case ERL_SEND_TT:      /* { SEND_TT, Cookie, ToPid, TraceToken } */
    if (ei_tracelevel >= 4) show_this_msg = 1;
    if (ei_decode_atom_as(header,&index,msg->cookie,sizeof(msg->cookie),ERLANG_UTF8,NULL,NULL) 
	|| ei_decode_pid(header,&index,&msg->to)
	|| ei_decode_trace(header,&index,&msg->token))
    {
        EI_CONN_SAVE_ERRNO__(EBADMSG);      
        return ERL_ERROR;
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
        EI_CONN_SAVE_ERRNO__(EBADMSG);      
        return ERL_ERROR;
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
        EI_CONN_SAVE_ERRNO__(EBADMSG);      
        return ERL_ERROR;
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
      /* flush in rest of packet */
      while (remain > 0) {
          rlen = remain > EIRECVBUF ? EIRECVBUF : remain;
          err = ei_read_fill_ctx_t__(cbs, ctx, header, &rlen, tmo);
          if (err) {
              EI_CONN_SAVE_ERRNO__(err);
              return ERL_ERROR;
          }
          if (rlen == 0)
              break;
          remain -= rlen;
      }
      erl_errno = EMSGSIZE;
      return ERL_ERROR;
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
	  return ERL_ERROR;
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
      rlen = remain;
      err = ei_read_fill_ctx_t__(cbs, ctx, mbuf+bytesread-index, &rlen, tmo);
      if (!err && rlen != remain)
          err = EIO;
      if (err) {
          *msglenp = bytesread-index+1; /* actual bytes in users buffer */
          EI_CONN_SAVE_ERRNO__(err);
          return ERL_ERROR;
      }
  }

  if (show_this_msg)
      ei_show_recmsg(stderr,msg,mbuf);

  /* the caller only sees "untraced" message types */
  /* the trace token is buried in the message struct */
  if (msg->msgtype > 10) msg->msgtype -= 10;
  
  return msg->msgtype;
}

static int
send_unlink_id_ack(ei_socket_callbacks *cbs, void *ctx,
                   char *id_ext, size_t id_size,
                   erlang_pid *from, erlang_pid *to,
                   unsigned tmo)
{
    /* Send: {ERL_UNLINK_ID_ACK, Id, From, To} */
    int err, index;
    ssize_t wlen;
    char ctl[EIRECVBUF]; /* should be large enough for any ctrl msg */
    char *s;

    EI_CONN_SAVE_ERRNO__(EINVAL);

    /* leave space for packet size and pass through marker... */
    index = 5;
    if (ei_encode_version(ctl, &index) < 0)
        return ERL_ERROR;
    if (ei_encode_tuple_header(ctl, &index, 4) < 0)
        return ERL_ERROR;
    if (ei_encode_long(ctl, &index, ERL_UNLINK_ID_ACK) < 0)
        return ERL_ERROR;
    s = &ctl[0] + index;
    index += id_size;
    memcpy((void *) s, (void *) id_ext, id_size);
    if (ei_encode_pid(ctl, &index, from) < 0)
        return ERL_ERROR;
    if (ei_encode_pid(ctl, &index, to) < 0)
        return ERL_ERROR;

    s = &ctl[0];
    /* packet size */
    put32be(s, index - 4);
    /* pass through */
    put8(s, ERL_PASS_THROUGH);

    if (ei_tracelevel >= 4) {
        if (ei_show_sendmsg(stderr, ctl, NULL) < 0)
            return ERL_ERROR;
    }

    wlen = (ssize_t) index;
    err = ei_write_fill_ctx_t__(cbs, ctx, ctl, &wlen, tmo);
    if (!err && wlen != (ssize_t) index)
        err = EIO;
    if (err) {
        EI_CONN_SAVE_ERRNO__(err);
        return ERL_ERROR;
  }

  erl_errno = 0;
  return 0;
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

