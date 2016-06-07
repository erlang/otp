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

#define GLOBALNAMEBUF (16*1024) /* not very small actually */

/* return a list of all registered names. Function allocates and
 * returns a NULL-terminated array of pointers to strings. The caller
 * is responsible for freeing the array. Space for the array and
 * all strings is allocated with a single call to malloc, so the
 * caller can make one call to free().
 */
/* global:registered_names() -> [name1,name2,...] */
char **erl_global_names(int fd, int *count)
{
  char buf[GLOBALNAMEBUF];
  char *bufp=buf;
  char tmpbuf[64];
  int size = 0;
  int index = 0;
  erlang_pid *self = erl_self();
  erlang_msg msg;
  int i;
  int version;
  int arity;
  int msglen;
  char **names;
  char *s;
  
  self->num = fd;
  ei_encode_version(buf,&index);
  ei_encode_tuple_header(buf,&index,2);
  ei_encode_pid(buf,&index,self);               /* PidFrom */
  ei_encode_tuple_header(buf,&index,5);
  ei_encode_atom(buf,&index,"call");            /* call */
  ei_encode_atom(buf,&index,"global");          /* Mod */
  ei_encode_atom(buf,&index,"registered_names");    /* Fun */
  ei_encode_list_header(buf,&index,0);          /* Args: [ ] */
  ei_encode_atom(buf,&index,"user");            /* user */
  
  /* make the rpc call */
  if (ei_send_reg_encoded(fd,self,"rex",buf,index)) return NULL;

  while (1) {
    index = GLOBALNAMEBUF;
    if (!(i = ei_recv_internal(fd,&bufp,&index,&msg,&msglen,1,0))) continue;
    else break;
  }

  if (i != ERL_SEND) return NULL;

  /* expecting { rex, [name1, name2, ...] } */
  size = msglen;
  index = 0;

  if (ei_decode_version(buf,&index,&version) 
      || ei_decode_tuple_header(buf,&index,&arity) 
      || (arity != 2) 
      || ei_decode_atom(buf,&index,tmpbuf) 
      || strcmp(tmpbuf,"rex")
      || ei_decode_list_header(buf,&index,&arity)) return NULL;

  
  /* we use the size of the rest of the received message to estimate
   * the buffer space required for all the strings. we know how many
   * they are (arity) so we need space for that many pointers, plus
   * a little less than the atoms themselves needed in the reply.
   */
  arity++; /* we will need a terminating NULL as well */
  if (!(names = malloc((arity * sizeof(char**)) + (size-index)))) return NULL;

  /* arity pointers first, followed by s */
  s = (char *)(names+arity);

  if (count) *count = 0;
  for (i=0; i<arity; i++) {
    names[i] = s;                 /* insert the pointer */
    if (ei_decode_atom(buf,&index,s)) break; /* copy the data */
    if (count) (*count)++; 
    s += strlen(names[i]) + 1;    /* advance pointer */
  }
  names[i]=NULL;

  return names;
}
