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
#include <stdlib.h>
#include <string.h>
#include "eidef.h"
#include "eiext.h"
#include "eisend.h"
#include "eirecv.h"
#include "ei_connect_int.h"
#include "erl_interface.h"
#include "erl_connect.h"

/* return the ETERM pid corresponding to name. If caller
 * provides non-NULL node, nodename will be returned there
 */
/* global:whereis_name(name) -> pid */

ETERM *erl_global_whereis(int fd, const char *name, char *node)
{
  char buf[EISMALLBUF];
  char *bufp=buf;
  char tmpbuf[64];
  int index = 0;
  erlang_pid *self = erl_self();
  erlang_pid epid;
  ETERM *opid;
  erlang_msg msg;
  int i;
  int version,arity,msglen;

  self->num = fd;		/* FIXME looks strange to change something?! */

  ei_encode_version(buf,&index);
  ei_encode_tuple_header(buf,&index,2);
  ei_encode_pid(buf,&index,self);               /* PidFrom */
  ei_encode_tuple_header(buf,&index,5);
  ei_encode_atom(buf,&index,"call");            /* call */
  ei_encode_atom(buf,&index,"global");          /* Mod */
  ei_encode_atom(buf,&index,"whereis_name");    /* Fun */
  ei_encode_list_header(buf,&index,1);          /* Args: [ name ] */
  ei_encode_atom(buf,&index,name);
  ei_encode_empty_list(buf,&index);
  ei_encode_atom(buf,&index,"user");            /* user */

  /* make the rpc call */
  if (ei_send_reg_encoded(fd,self,"rex",buf,index)) return NULL;

  while (1) {
    index = EISMALLBUF;
    if (!(i = ei_recv_internal(fd,&bufp,&index,&msg,&msglen,1,0))) continue;
    else break;
  }

  if (i != ERL_SEND) return NULL;
    
  /* expecting { rex, pid } */
  index = 0;
  if (ei_decode_version(buf,&index,&version) 
      || ei_decode_tuple_header(buf,&index,&arity) 
      || (arity != 2) 
      || ei_decode_atom(buf,&index,tmpbuf) 
      || strcmp(tmpbuf,"rex")
      || ei_decode_pid(buf,&index,&epid))
    return NULL; /* bad response from other side */

  /* put the pid into a format for the caller */
  index = 0;
  ei_encode_pid(buf,&index,&epid);
  opid = erl_decode((unsigned char*)buf);

  /* extract the nodename for the caller */
  if (node) {
      char* node_str = ERL_PID_NODE(opid);
      if (node_str) {
	  strcpy(node, node_str);
      }
      else {
	  erl_free_term(opid);
	  return NULL;
      }
  }

  return opid;
}
