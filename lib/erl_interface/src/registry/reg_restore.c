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

static int mn_start_restore(int fd, const erlang_pid *self, erlang_pid *mnesia, const char *mntab, long *count, long *maxkey,long *maxobj)
{
  char buf[EISMALLBUF];
  char *bufp=buf;
  char tmpbuf[64];
  int index = 0;
  erlang_msg msg;
  int arity;
  int version;
  int i;
  int needlink;
  int needmsg;
  int msglen;
  
  /* set up rpc arguments */
  /* { PidFrom, { call, Mod, Fun, Args, user }}  */
  ei_encode_version(buf,&index);
  ei_encode_tuple_header(buf,&index,2);
  ei_encode_pid(buf,&index,self);               /* PidFrom */
  ei_encode_tuple_header(buf,&index,5);
  ei_encode_atom(buf,&index,"call");            /* call */
  ei_encode_atom(buf,&index,EI_MNESIA_MODULE);  /* Mod */
  ei_encode_atom(buf,&index,EI_MNESIA_RESTORE); /* Fun */
  ei_encode_list_header(buf,&index,2);          /* Args: [ table, self() ] */
  ei_encode_atom(buf,&index,mntab);
  ei_encode_pid(buf,&index,self); 
  ei_encode_empty_list(buf,&index);
  ei_encode_atom(buf,&index,"user");            /* user */

  /* make the rpc call */
  if (ei_send_reg_encoded(fd,self,"rex",buf,index)) return -1;

  /* get the reply: expect link and message (not sure which will come first though) */
  needlink = needmsg = 1;
  while (needlink || needmsg) {
    /* get message */
    index = EISMALLBUF;
    while (!(i = ei_recv_internal(fd,&bufp,&index,&msg,&msglen,1,0))) index = EISMALLBUF;

    switch (i) {
    case ERL_LINK:
      /* got link */
      if (!needlink) return -1;
      needlink = 0;
      break;
      
    case ERL_SEND:
      /* got message - is it the right one? */
      if (!needmsg) return -1;
      else {
	/* expecting { rex, { size, Pid, Count, MaxKey, MaxObj }} */
	index = 0;
	if (ei_decode_version(buf,&index,&version) 
	    || ei_decode_tuple_header(buf,&index,&arity)
	    || (arity != 2) 
	    || ei_decode_atom(buf,&index,tmpbuf)
	    || strcmp(tmpbuf,"rex")
	    || ei_decode_tuple_header(buf,&index,&arity)
	    || (arity != 5) 
	    || ei_decode_atom(buf,&index,tmpbuf)
	    || strcmp(tmpbuf,EI_MNESIA_SIZE)
	    || ei_decode_pid(buf,&index,mnesia)
	    || ei_decode_long(buf,&index,count)
	    || ei_decode_long(buf,&index,maxkey)
	    || ei_decode_long(buf,&index,maxobj))
	  return -1; /* bad response from other side */
      
	/* got msg */
	needmsg = 0;
      }
      break;
      
    default:
      return -1; /* wasn't link or pid */
    }
  }
  return 0;
}

static int mn_unlink(int fd)
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

/* decode an object and insert it into the table */
static int mn_decode_insert(ei_reg *reg, const char *msgbuf, int *index, char *key)
{
  long keylen;
  long objlen;
  long objtype;
  void *objbuf = NULL;
  long i;
  double f;

  if (ei_decode_long(msgbuf,index,&keylen) 
      || ei_decode_long(msgbuf,index,&objlen) 
      || ei_decode_long(msgbuf,index,&objtype)) 
    return -1;


  /* decode key */
  if (ei_decode_string(msgbuf,index,key)) {
    if (objbuf) free(objbuf);
    return -1;
  }

  /* finally! decode object and insert in table */
  /* don't forget to fix attributes (dirty bit for example) */

  /* FIXME: added cast but 64 bit trouble I think */
  switch ((int)objtype & EI_REG_TYPEMASK) {
  case EI_INT:
    if (ei_decode_long(msgbuf,index,&i)) return -1;
    ei_reg_setival(reg,key,i);
    break;

  case EI_FLT:
    if (ei_decode_double(msgbuf,index,&f)) return -1;
    ei_reg_setfval(reg,key,f);
    break;

  case EI_STR:
    objbuf = NULL;
    if (objlen > 0) {
      if (!(objbuf = malloc(objlen))) return -1;
      if (ei_decode_string(msgbuf,index,objbuf)) {
	free(objbuf);
	return -1;
      }
      ei_reg_setsval(reg,key,objbuf);
    }
    else {
      /* just a pointer to nothing */
      if (ei_decode_long(msgbuf,index,&i)) return -1;
      ei_reg_setsval(reg,key,NULL);
    }
    break;

  case EI_BIN:
    objbuf = NULL;
    if (objlen > 0) {
      if (!(objbuf = malloc(objlen))) return -1;
      if (ei_decode_binary(msgbuf,index,objbuf,&i)) {
	free(objbuf);
	return -1;
      }
      /* assert(i == objlen) */
      ei_reg_setpval(reg,key,objbuf,objlen);
    }
    else {
      /* just a pointer to nothing */
      if (ei_decode_long(msgbuf,index,&i)) return -1;
      ei_reg_setpval(reg,key,(void *)i,0);
    }
    break;

  default:
    /* unknown type */
    if (objbuf) free(objbuf);
    return -1;
  } /* switch */

  return 0;
}

/* help function passed to hash_foreach, to clear dirty bits */
/* use after successful restore */
static int clean_obj(const char *key, const void *p)
{
  ei_reg_obj *obj = (ei_reg_obj *)p;

  if (obj) obj->attr &= ~EI_DIRTY;

  return 0;
}

int ei_reg_restore(int fd, ei_reg *reg, const char *mntab)
{
  int i,j;
  char tag[32];
  char sbuf[EISMALLBUF];
  char *dbuf = NULL;
  char *msgbuf = NULL;
  char *keybuf = NULL;
  erlang_pid self;
  erlang_pid mnesia = {"",0,0,0};
  erlang_msg msg;
  int index = 0;
  int len = 0;
  int msglen;
  int version = 0;
  int arity = 0;
  long count = 0;
  long maxkey = 0;
  long maxobj = 0;
  ei_cnode *ec;

  if (!reg || !mntab) return -1; /* return EI_BADARG; */

  /* make a self pid */

  if ((ec = ei_fd_to_cnode(fd)) == NULL) {
      return -1;
  }
  strcpy(self.node,ei_thisnodename(ec));
  self.num = fd;
  self.serial = 0;
  self.creation = ei_thiscreation(ec);

  
  if (mn_start_restore(fd,&self,&mnesia,mntab,&count,&maxkey,&maxobj)) {
    /* send exit *only* if we have pid */
    if (mnesia.node[0]) ei_send_exit(fd,&self,&mnesia,"bad response from rpc start");
    return -1;
  }

  if (count <= 0) {
    ei_send_exit(fd,&self,&mnesia,"nothing to do");
    return 0;
  }
  
  /* make sure receive buffer can handle largest expected message */
  len = maxkey + maxobj + 512; 
  if (len > EISMALLBUF)
    if (!(dbuf = malloc(len))) {
      ei_send_exit(fd,&self,&mnesia,"cannot allocate space for incoming data");
      return -1;
    }
  msgbuf = (dbuf ? dbuf : sbuf);

  /* allocate space for largest key */
  if (!(keybuf = malloc(maxkey+1))) goto restore_failure;
  
  /* get this ball rolling */
  index = 0;
  ei_encode_version(msgbuf,&index);
  ei_encode_tuple_header(msgbuf,&index,2);
  ei_encode_atom(msgbuf,&index,"send_records");
  ei_encode_pid(msgbuf,&index,&self);
  if (ei_send_encoded(fd,&mnesia,msgbuf,index)) goto restore_failure;

  /* read as much as possible, until count or EXIT */
  for (i=0; i<count; i++) {
    index = len;
    while ((j = ei_recv_internal(fd,&msgbuf,&index,&msg,&msglen,1,0)) == 0) index = len;
    if (j<0) goto restore_failure;
    
    /* decode the first part of the message */
    index = 0;
    if ((msg.msgtype != ERL_SEND) 
	|| ei_decode_version(msgbuf,&index,&version) 
	|| ei_decode_tuple_header(msgbuf,&index,&arity) 
	|| (arity != 6) 
	|| ei_decode_atom(msgbuf,&index,tag) 
	|| strcmp(tag,EI_MNESIA_RECV)) 
      goto restore_failure;

    /* decode the rest of the message and insert data into table */
    if (mn_decode_insert(reg,msgbuf,&index,keybuf)) goto restore_failure;
  }
  
  if (keybuf) free(keybuf);
  if (dbuf) free(dbuf);

  /* wait for unlink */
  if (mn_unlink(fd)) return -1;

  /* clear all the dirty bits */
  ei_hash_foreach(reg->tab,clean_obj);

  /* success */
  return 0;

restore_failure:
  ei_send_exit(fd,&self,&mnesia,"restore failure");
  if (keybuf) free(keybuf);
  if (dbuf) free(dbuf);
  return -1;
}

