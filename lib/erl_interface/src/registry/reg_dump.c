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
#include <stdlib.h>
#include <string.h>
#include "eidef.h"
#include "eiext.h"
#include "reg.h"
#include "eisend.h"
#include "eirecv.h"
#include "ei_connect_int.h"

static int mn_start_dump(int fd, const erlang_pid *self, 
			 erlang_pid *mnesia, const char *mntab)
{
  char buf[EISMALLBUF];
  char *bufp = buf;
  char tmpbuf[64];
  int index = 0;
  erlang_msg msg;
  int type;
  int arity;
  int version;
  int msglen;
  int i;
  int needlink;
  int needpid;

  /* set up rpc arguments */
  /* { PidFrom, { call, Mod, Fun, Args, user }}  */
  ei_encode_version(buf,&index);
  ei_encode_tuple_header(buf,&index,2);
  ei_encode_pid(buf,&index,self);               /* PidFrom */
  ei_encode_tuple_header(buf,&index,5);
  ei_encode_atom(buf,&index,"call");            /* call */
  ei_encode_atom(buf,&index,EI_MNESIA_MODULE);  /* Mod */
  ei_encode_atom(buf,&index,EI_MNESIA_DUMP);    /* Fun */
  ei_encode_list_header(buf,&index,2);          /* Args: [ table, self() ] */
  ei_encode_atom(buf,&index,mntab);
  ei_encode_pid(buf,&index,self); 
  ei_encode_empty_list(buf,&index);
  ei_encode_atom(buf,&index,"user");            /* user */

  /* make the rpc call */
  if (ei_send_reg_encoded(fd,self,"rex",buf,index)) return -1;

  /* get the reply: expect link and pid (not sure which will come first though) */
  needlink = needpid = 1;
  while (needlink || needpid) {
    /* get message */
    while (1) {
      index = EISMALLBUF;
      if (!(i = ei_recv_internal(fd,&bufp,&index,&msg,&msglen,1,0))) continue;
      else break;
    }

    switch (i) {
    case ERL_LINK:
      /* got link */
      if (!needlink) return -1;
	needlink = 0;
      break;
      
    case ERL_SEND:
      /* got message - does it contain a pid? */
      if (!needpid) return -1;
      else {
	/* expecting { rex, <pid> } */
	index = 0;
	if (ei_decode_version(buf,&index,&version) 
	    || ei_decode_tuple_header(buf,&index,&arity) 
	    || (arity != 2) 
	    || ei_decode_atom(buf,&index,tmpbuf) 
	    || strcmp(tmpbuf,"rex")
	    || ei_get_type_internal(buf,&index,&type,&arity) 
	    || (type != ERL_PID_EXT))
	  return -1; /* bad response from other side */
      
	if (ei_decode_pid(buf,&index,mnesia)) return -1;

	/* got pid */
	needpid = 0;
      }
      break;
      
    default:
      return -1; /* wasn't link or pid */
    }
  }
  return 0;
}

static int mn_send_commit(int fd, erlang_pid *mnesia, erlang_pid *self)
{
  char buf[EISMALLBUF];
  char *bufp=buf;
  char string[256];
  int index = 0;
  int version,arity;
  int msglen;
  erlang_msg msg;
  int i;
  
  /* set up commit message { commit, self() } */
  ei_encode_version(buf,&index);
  ei_encode_tuple_header(buf,&index,2);
  ei_encode_atom(buf,&index,EI_MNESIA_COMMIT);
  ei_encode_pid(buf,&index,self);

  /* send it */
  if (ei_send_encoded(fd,mnesia,buf,index)) return -1;

  /* get reply */
  while (1) {
    index = EISMALLBUF;
    if (!(i=ei_recv_internal(fd,&bufp,&index,&msg,&msglen,1,0))) continue;
    else if (i < 0) return -1;
    else break;
  }
  
  if (i == ERL_SEND) {
    index = 0;
    if (ei_decode_version(buf,&index,&version) 
	|| ei_decode_tuple_header(buf,&index,&arity) 
	|| ei_decode_atom(buf,&index,string))
      return -1;

    if (!strcmp(string,"ok")) return 0;
  }
  /* wrong message type */
  return -1;
}

static int mn_send_delete(int fd, erlang_pid *mnesia, const char *key)
{
  char sbuf[EISMALLBUF];
  char *dbuf = NULL;
  char *msgbuf;
  int index = 0;
  int len = strlen(key) + 32; /* 32 is a slight overestimate */

  if (len > EISMALLBUF)
    if (!(dbuf = malloc(len)))
      return -1;
  msgbuf = (dbuf ? dbuf : sbuf);

  /* set up delete message { delete, Key } */
  ei_encode_version(msgbuf,&index);
  ei_encode_tuple_header(msgbuf,&index,2);
  ei_encode_atom(msgbuf,&index,EI_MNESIA_DELETE);
  ei_encode_string(msgbuf,&index,key);

  /* send it */
  if (ei_send_encoded(fd,mnesia,msgbuf,index)) {
    if (dbuf) free(dbuf);
    return -1;
  }

  if (dbuf) free(dbuf);
  return 0;
}

static int mn_send_write(int fd, erlang_pid *mnesia, const char *key, ei_reg_obj *obj)
{
  char sbuf[EISMALLBUF];
  char *dbuf = NULL;
  char *msgbuf;
  int index = 0;
  int keylen = strlen(key) + 1;
  int len = 32 + keylen + obj->size;

  if (len > EISMALLBUF)
    if (!(dbuf = malloc(len)))
      return -1;
  msgbuf = (dbuf ? dbuf : sbuf);

  ei_encode_version(msgbuf,&index);
  ei_encode_tuple_header(msgbuf,&index,6);
  ei_encode_atom(msgbuf,&index,EI_MNESIA_WRITE);
  ei_encode_string(msgbuf,&index,key);
  ei_encode_long(msgbuf,&index,keylen);
  ei_encode_long(msgbuf,&index,obj->attr);
  ei_encode_long(msgbuf,&index,obj->size);
					      
  switch (ei_reg_typeof(obj)) {
  case EI_INT:
    ei_encode_long(msgbuf,&index,obj->val.i);
    break;
  case EI_FLT:
    ei_encode_double(msgbuf,&index,obj->val.f);
    break;
  case EI_STR:
    if (obj->size > 0) ei_encode_string(msgbuf,&index,obj->val.s);
    else ei_encode_long(msgbuf,&index, (long)NULL);  /* just the NULL pointer */
    break;
  case EI_BIN:
    if (obj->size > 0) ei_encode_binary(msgbuf,&index,obj->val.p,obj->size);
    else ei_encode_long(msgbuf,&index,(long)(obj->val.p));  /* just the pointer */
    break;
  default:
    if (dbuf) free(dbuf);
    return -1;
  }

  /* send it */
  if (ei_send_encoded(fd,mnesia,msgbuf,index)) {
    if (dbuf) free(dbuf);
    return -1;
  }

  if (dbuf) free(dbuf);
  return 0;
}

static int mn_get_unlink(int fd)
{
  erlang_msg msg;
  char buf[EISMALLBUF];
  char *bufp=buf;
  int index;
  int msglen;
    
  /* wait for unlink or exit */
  while (1) {
    index = EISMALLBUF;
    switch (ei_recv_internal(fd,&bufp,&index,&msg,&msglen,1,0)) {
    case 0: continue;
    case ERL_UNLINK: return 0;
    default: return -1;
    }
  }
  return 0;
}

/* dump to backup */
/* fd is open connection to erlang node */
int ei_reg_dump(int fd, ei_reg *reg, const char *mntab, int flags)
{
  ei_hash *tab;
  erlang_pid self;
  erlang_pid mnesia;
  ei_bucket *b;
  ei_reg_obj *obj;
  const char *key;
  ei_cnode *ec;
  int i;
  
  if (!reg || !mntab) return -1; /* return EI_BADARG; */
  tab = reg->tab;
  
  /* make a self pid */
  
  if ((ec = ei_fd_to_cnode(fd)) == NULL) {
      return -1;
  }
  strcpy(self.node,ei_thisnodename(ec));
  self.num = fd;
  self.serial = 0;
  self.creation = ei_thiscreation(ec);

  if (mn_start_dump(fd,&self,&mnesia,mntab)) return -1;

  /* traverse the table, passing objects to mnesia */
  for (i=0; i<tab->size; i++) {
    b=tab->tab[i];
    while (b) {
      obj = (ei_reg_obj*)(b->value); /* cast to eliminate 'const' warning */
      key = b->key;

      if ((flags & EI_FORCE) || (obj->attr & EI_DIRTY)) {
	if (obj->attr & EI_DELET) {
	  if (mn_send_delete(fd,&mnesia,key)) {
	    ei_send_exit(fd,&self,&mnesia,"delete failed");
	    return -1;
	  }
	}
	else {
	  if (mn_send_write(fd,&mnesia,key,obj)) {
	    ei_send_exit(fd,&self,&mnesia,"update failed");
	    return -1;
	  }
	}
      }
      b = b->next;
    }
  }

  /* end the transaction */
  if (mn_send_commit(fd,&mnesia,&self)) {
    ei_send_exit(fd,&self,&mnesia,"commit failed");
    return -1;
  }

  /* wait for unlink */
  if (mn_get_unlink(fd)) return -1;
  
  /* this point only reached if all went ok so far... */
  
  /* now remove all deleted objects, unless the caller asked us not to */
  if (!(flags & EI_NOPURGE)) ei_reg_purge(reg);

  /* success */
  return 0;

}
