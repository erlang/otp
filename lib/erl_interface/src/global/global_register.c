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
#include <string.h>
#include "eidef.h"
#include "eiext.h"
#include "eisend.h"
#include "eirecv.h"
#include "ei.h"
#include "ei_internal.h"

int ei_global_register(int fd, const char *name, erlang_pid *self)
{
  char buf[EISMALLBUF];
  char *bufp=buf;
  char tmpbuf[64];
  int index = 0;
  erlang_msg msg;
  int needlink, needatom, needmonitor;
  int arity;
  int version;
  int msglen;
  int i;

  /* set up rpc arguments */
  /* { PidFrom, { call, Mod, Fun, Args, user }}  */
  index = 0;
  if (ei_encode_version(buf,&index)
      || ei_encode_tuple_header(buf,&index,2)
      || ei_encode_pid(buf,&index,self)               /* PidFrom */
      || ei_encode_tuple_header(buf,&index,5)
      || ei_encode_atom(buf,&index,"call")            /* call */
      || ei_encode_atom(buf,&index,"global")          /* Mod */
      || ei_encode_atom(buf,&index,"register_name_external")/* Fun */
      || ei_encode_list_header(buf,&index,3) /* Args: [ name, self(), cnode ] */
      || ei_encode_atom(buf,&index,name)
      || ei_encode_pid(buf,&index,self) 
      || ei_encode_tuple_header(buf,&index,2)
      || ei_encode_atom(buf,&index,"global") /* special "resolve" treatment */ 
      || ei_encode_atom(buf,&index,"cnode")  /* i.e. we get a SEND when conflict */
      || ei_encode_empty_list(buf,&index)
      || ei_encode_atom(buf,&index,"user")) {           /* user */
      EI_CONN_SAVE_ERRNO__(EINVAL);
      return ERL_ERROR;
  }

  /* make the rpc call */
  if (ei_send_reg_encoded(fd,self,"rex",buf,index)) return ERL_ERROR;

  /* get the reply: expect link and an atom, or just an atom */
  needlink = needatom = needmonitor = 1;
  while (1) {
    /* get message */
    while (1) {
      index = EISMALLBUF;
      if (!(i = ei_recv_internal(fd,&bufp,&index,&msg,&msglen,1,0))) continue;
      else break;
    }

    switch (i) {
    case ERL_LINK:
      /* got link */
      if (!needlink) {
          EI_CONN_SAVE_ERRNO__(EBADMSG);
          return ERL_ERROR;
      }
      needlink = 0;
      break;
      
    case ERL_MONITOR_P-10:
      /* got monitor */
	if (!needmonitor) {
            EI_CONN_SAVE_ERRNO__(EBADMSG);
            return ERL_ERROR;
        }
	needmonitor = 0;
      break;

    case ERL_SEND:
      /* got message - does it contain our atom? */
      if (!needatom) {
          EI_CONN_SAVE_ERRNO__(EBADMSG);
          return ERL_ERROR;
      }
      else {
	/* expecting { rex, yes } */
	index = 0;
	if (ei_decode_version(buf,&index,&version) 
	    || ei_decode_tuple_header(buf,&index,&arity) 
	    || (arity != 2) 
	    || ei_decode_atom(buf,&index,tmpbuf) 
	    || strcmp(tmpbuf,"rex")
	    || ei_decode_atom(buf,&index,tmpbuf) 
	    || strcmp(tmpbuf,"yes")) {
            EI_CONN_SAVE_ERRNO__(EBADMSG);
            return ERL_ERROR; /* bad response from other side */
        }

	/* we're done */
	return 0;
      }
      break;
      
    default:
      EI_CONN_SAVE_ERRNO__(EBADMSG);
      return ERL_ERROR; /* something else */
    }
  }
  return 0;
}

