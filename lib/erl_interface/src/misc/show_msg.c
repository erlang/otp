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
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>

#include <sys/types.h>

#include "eidef.h"

#ifndef __WIN32__
#  ifdef TIME_WITH_SYS_TIME
#    include <sys/time.h>
#    include <time.h>
#  else
#    ifdef HAVE_SYS_TIME_H
#       include <sys/time.h>
#    else
#       include <time.h>
#    endif
#  endif
#else
#  include <time.h>
#endif

#include "eiext.h"
#include "putget.h"
#include "ei_printterm.h"
#include "ei_internal.h"
#include "show_msg.h"

#ifndef EISHOWBUF
#define EISHOWBUF 512
#endif

static void show_term(const char *termbuf, int *index, FILE *stream);
static void show_pid(FILE *stream, const erlang_pid *pid);
static void show_trace(FILE *stream, const erlang_trace *t);
static void show_msg(FILE *stream, int direction, const erlang_msg *msg,
		    const char *buf);
static void ei_efprint(FILE *stream, const char *termbuf);
static int ei_decode_skip_bignum(const char *buf, int *index, void *p);
static int printable_list_p(const uint8 *buf, int buflen);


/***************************************************************************
 *
 *  Write trace information to stderr
 *
 ***************************************************************************/

void ei_trace_printf(const char *name, int level, const char *format,...)
{
    time_t now;
    char *timestr;
    char buf[2048];
    int len;
    va_list args;

    va_start(args, format);

    time(&now);
    timestr = (char *)ctime(&now);
    sprintf(buf, "%s: %.*s: ", name, (int) strlen(timestr)-1, timestr);
    len = strlen(buf);
    vsprintf(buf + len, format, args);
    fprintf(stderr,"%s\r\n",buf);
    va_end(args);
}


/***************************************************************************
 *
 *  Debug printing of incoming and outgoing messages
 *
 ***************************************************************************/

/*
 * FIXME maybe this function should be rewritten to use ei_printterm instead
 * (or the other way around)
 */

/* 
 * the new TT stuff has been added, but when these messages are shown
 * they will look just like the non-tt ones for now.
 */
   
/*
 * this should be long enough for longest atoms (256) but short enough for
 * fprintf to handle all at once (a few kb probably).
 */


void ei_show_recmsg(FILE *stream, erlang_msg *msg, char *buf)
{
    show_msg(stream, 0, msg, buf);
}


/* decode the buffer again before showing it */
int ei_show_sendmsg(FILE *stream, const char *header, const char *msgbuf)
{
    erlang_msg msg;
    const char *mbuf = NULL;
    int index = 0;
    int arity = 0;
    int version = 0;

    /* skip five bytes */
    index = 5;
    ei_decode_version(header,&index,&version);
    ei_decode_tuple_header(header,&index,&arity);
    ei_decode_long(header,&index,&msg.msgtype);

    switch (msg.msgtype) {
    case ERL_SEND:
	if (ei_decode_atom_as(header,&index,msg.cookie,sizeof(msg.cookie),ERLANG_UTF8,NULL,NULL) 
	    || ei_decode_pid(header,&index,&msg.to)) return -1;
	mbuf = msgbuf;
	break;

    case ERL_SEND_TT:
	if (ei_decode_atom_as(header,&index,msg.cookie,sizeof(msg.cookie),ERLANG_UTF8,NULL,NULL) 
	    || ei_decode_pid(header,&index,&msg.to)
	    || ei_decode_trace(header,&index,&msg.token)) return -1;
	mbuf = msgbuf;
	break;
    
    case ERL_REG_SEND:
	if (ei_decode_pid(header,&index,&msg.from) 
	    || ei_decode_atom_as(header,&index,msg.cookie,sizeof(msg.cookie),ERLANG_UTF8,NULL,NULL) 
	    || ei_decode_atom_as(header,&index,msg.toname,sizeof(msg.toname),ERLANG_UTF8,NULL,NULL)) return -1;
	mbuf = msgbuf;
	break;
    
    case ERL_REG_SEND_TT:
	if (ei_decode_pid(header,&index,&msg.from) 
	    || ei_decode_atom_as(header,&index,msg.cookie,sizeof(msg.cookie),ERLANG_UTF8,NULL,NULL) 
	    || ei_decode_atom_as(header,&index,msg.toname,sizeof(msg.toname),ERLANG_UTF8,NULL,NULL)
	    || ei_decode_trace(header,&index,&msg.token)) return -1;
	mbuf = msgbuf;
	break;

    case ERL_EXIT:
    case ERL_EXIT2:
	if (ei_decode_pid(header,&index,&msg.from) 
	    || ei_decode_pid(header,&index,&msg.to)) return -1;
	mbuf = header+index;

    case ERL_EXIT_TT:
    case ERL_EXIT2_TT:
	if (ei_decode_pid(header,&index,&msg.from) 
	    || ei_decode_pid(header,&index,&msg.to)
	    || ei_decode_trace(header,&index,&msg.token)) return -1;
	mbuf = header+index;
	break;
    
    case ERL_LINK:
    case ERL_UNLINK:
    case ERL_GROUP_LEADER:
	if (ei_decode_pid(header,&index,&msg.from) 
	    || ei_decode_pid(header,&index,&msg.to)) return -1;
	mbuf = header;
	break;
    
    default:
	break;
    }

    show_msg(stream, 1, &msg, mbuf);

    return 0;
}


/***************************************************************************
 *
 *  Common function for ei_show_recmsg() and ei_show_sendmsg()
 *
 ***************************************************************************/

static void show_msg(FILE *stream, int direction, const erlang_msg *msg,
		     const char *buf)
{
    if (direction) fprintf(stream,"-> ");
    else fprintf(stream,"<- ");
  
    switch (msg->msgtype) {
    case ERL_LINK:
	fprintf(stream,"LINK From: ");
	show_pid(stream,&msg->from);
	fprintf(stream," To: ");
	show_pid(stream,&msg->to);
	break;
      
    case ERL_SEND:
	fprintf(stream,"SEND To: ");
	show_pid(stream,&msg->to);
	fprintf(stream,"\n   ");
	/* show the message */
	ei_efprint(stream,buf);
	break;
      
    case ERL_EXIT:
	fprintf(stream,"EXIT From: ");
	show_pid(stream,&msg->from);
	fprintf(stream," To: ");
	show_pid(stream,&msg->to);
	/* show the reason */
	fprintf(stream,"\n   Reason: ");
	ei_efprint(stream,buf);
	break;
      
    case ERL_UNLINK:
	fprintf(stream,"UNLINK From: ");
	show_pid(stream,&msg->from);
	fprintf(stream," To: ");
	show_pid(stream,&msg->to);
	break;
      
    case ERL_REG_SEND:
	fprintf(stream,"REG_SEND From: ");
	show_pid(stream,&msg->from);
	fprintf(stream," To: %s\n   ",msg->toname);
	/* show the message */
	ei_efprint(stream,buf);
	break;
      
    case ERL_GROUP_LEADER:
	fprintf(stream,"GROUP_LEADER From: ");
	show_pid(stream,&msg->from);
	fprintf(stream," To: ");
	show_pid(stream,&msg->to);
	break;
      
    case ERL_EXIT2:
	fprintf(stream,"EXIT2 From: ");
	show_pid(stream,&msg->from);
	fprintf(stream," To: ");
	show_pid(stream,&msg->to);
	/* show the reason */
	fprintf(stream,"\n   Reason: ");
	ei_efprint(stream,buf);
	break;

	/* the new TT stuff below */

    case ERL_EXIT_TT:
	fprintf(stream,"EXIT_TT From: ");
	show_pid(stream,&msg->from);
	fprintf(stream," To: ");
	show_pid(stream,&msg->to);
	fprintf(stream,"\n   ");
	show_trace(stream,&msg->token);
	/* show the reason */
	fprintf(stream,"\n   Reason: ");
	ei_efprint(stream,buf);
	break;
    
    case ERL_EXIT2_TT:
	fprintf(stream,"EXIT2_TT From: ");
	show_pid(stream,&msg->from);
	fprintf(stream," To: ");
	show_pid(stream,&msg->to);
	fprintf(stream,"\n   ");
	show_trace(stream,&msg->token);
	/* show the reason */
	fprintf(stream,"\n   Reason: ");
	ei_efprint(stream,buf);
	break;
    
    case ERL_SEND_TT:
	fprintf(stream,"SEND_TT To: ");
	show_pid(stream,&msg->to);
	fprintf(stream,"\n   ");
	show_trace(stream,&msg->token);
	fprintf(stream,"\n   ");
	/* show the message */
	ei_efprint(stream,buf);
	break;

    case ERL_REG_SEND_TT:
	fprintf(stream,"REG_SEND_TT From: ");
	show_pid(stream,&msg->from);
	fprintf(stream," To: %s\n   ",msg->toname);
	show_trace(stream,&msg->token);
	fprintf(stream,"\n   ");
	/* show the message */
	ei_efprint(stream,buf);
	break;

    default:
	fprintf(stream,"Unknown message type: %ld",msg->msgtype);
    }
    fprintf(stream,"\n");
}

/***************************************************************************
 *
 *  Print term to stream with fprintf
 *
 ***************************************************************************/


static void ei_efprint(FILE *stream, const char *termbuf)
{
    int index = 0;
    show_term(termbuf,&index,stream);
}

static void show_term(const char *termbuf, int *index, FILE *stream)
{
    int type;
    char smallbuf[EISHOWBUF];
    int version;
    long num;
    double fnum;
    erlang_pid pid;
    erlang_port port;
    erlang_ref ref;
    int i, len;
    char *s;

    ei_get_type_internal(termbuf,index,&type,&len);
  
    switch (type) {
    case ERL_VERSION_MAGIC:
	/* just skip past this */
	ei_decode_version(termbuf,index,&version);
	show_term(termbuf,index,stream);
	break;
      
    case ERL_ATOM_EXT:
	ei_decode_atom(termbuf,index,smallbuf);
	fprintf(stream,"%s",smallbuf);
	break;

    case ERL_STRING_EXT:
	/* strings can be much longer than EISHOWBUF */
	if (len < EISHOWBUF) s = smallbuf;
	else if (!(s = malloc(len+1))) break; /* FIXME just break if can't? */

	ei_decode_string(termbuf,index,s);

	if (printable_list_p((uint8 *)s,len)) {
	    /* just show it as it is */
	    fprintf(stream,"\"%s\"",s);
	} else {
	    /* show it as a list instead */
	    fprintf(stream,"[");
	    for (i=0; i<len; i++) {
		if (i > 0) fprintf(stream,", ");
		fprintf(stream,"%d",s[i]);
	    }
	    fprintf(stream,"]");
	}

	/* did we allocate anything? */
	if (s && (s != smallbuf)) free(s);

	break;

    /* FIXME add case using ei_decode_longlong */
    case ERL_SMALL_BIG_EXT:
    case ERL_SMALL_INTEGER_EXT:
    case ERL_INTEGER_EXT:
	if (ei_decode_long(termbuf,index,&num) == 0) {
	    fprintf(stream,"%ld",num);
	} else {
	    ei_decode_skip_bignum(termbuf,index,NULL);
	    fprintf(stream,"#Bignum");
	}
	break;

    case ERL_FLOAT_EXT:
    case NEW_FLOAT_EXT:
	ei_decode_double(termbuf,index,&fnum);
	fprintf(stream,"%f",fnum);
	break;

    case ERL_PID_EXT:
    case ERL_NEW_PID_EXT:
	ei_decode_pid(termbuf,index,&pid);
	show_pid(stream,&pid);
	break;
    
    case ERL_SMALL_TUPLE_EXT:
    case ERL_LARGE_TUPLE_EXT:
	ei_decode_tuple_header(termbuf,index,&len);
	fprintf(stream,"{");
	for (i=0; i<len; i++) {
	    if (i > 0) fprintf(stream,", ");
	    show_term(termbuf,index,stream);
	}
	fprintf(stream,"}");
	break;
    
    case ERL_LIST_EXT:
	ei_decode_list_header(termbuf,index,&len);
	fprintf(stream,"[");
	for (i=0; i<len; i++) {
	    if (i > 0) fprintf(stream,", ");
	    show_term(termbuf,index,stream);
	}
	/* get the empty list at the end */
	ei_decode_list_header(termbuf,index,&len);
	fprintf(stream,"]");
	break;

    case ERL_NIL_EXT:
	ei_decode_list_header(termbuf,index,&len);
	fprintf(stream,"[]");
	break;
    
    case ERL_REFERENCE_EXT:
    case ERL_NEW_REFERENCE_EXT:
    case ERL_NEWER_REFERENCE_EXT:
	ei_decode_ref(termbuf,index,&ref);
	fprintf(stream,"#Ref<%s",ref.node);
	for (i = 0; i < ref.len; i++) {
	    fprintf(stream,".%u",ref.n[i]);
	}
	fprintf(stream,".%u>",ref.creation);
	break;

    case ERL_PORT_EXT:
    case ERL_NEW_PORT_EXT:
	ei_decode_port(termbuf,index,&port);
	fprintf(stream,"#Port<%s.%u.%u>",port.node,port.id,port.creation);
	break;
      
    case ERL_BINARY_EXT:
	ei_decode_binary(termbuf,index,NULL,&num);
	fprintf(stream,"#Bin<%ld>",num);
	break;
    
    case ERL_LARGE_BIG_EXT:
	/* doesn't actually decode - just skip over it */
	/* FIXME if GMP, what to do here?? */
	ei_decode_skip_bignum(termbuf,index,NULL);
	fprintf(stream,"#Bignum");
	break;
    
    case ERL_FUN_EXT: {
	char atom[MAXATOMLEN];
	long idx;
	long uniq;
	const char* s = termbuf + *index, * s0 = s;
	int n_free;

	++s;
	n_free = get32be(s);
	*index += s - s0;
	ei_decode_pid(termbuf, index, NULL); /* skip pid */
	ei_decode_atom(termbuf, index, atom); /* get module, index, uniq */
	ei_decode_long(termbuf, index, &idx);
	ei_decode_long(termbuf, index, &uniq);
	fprintf(stream,"#Fun<%s.%ld.%ld>", atom, idx, uniq);
	for (i = 0; i < n_free; ++i) {
	    /* FIXME how to report error ?! */
	    if (ei_skip_term(termbuf, index) != 0)
		fprintf(stderr,"<ERROR> show_msg: unknown type of term !");
	}
	break;
    }
    default:
	fprintf(stream,"#Unknown<%d.%d>",type,len);
	/* unfortunately we don't know how to skip over this type in
	 * the buffer if we don't even know what it is, so we return.
	 */
	return;
	break;
    }
}

/***************************************************************************
 *
 *  this help function does the actual decoding of the
 *  terms and is used by both ei_efprint and ei_sprintt.
 *
 *  termbuf contains the undecoded term.
 *  idx is the current position in termbuf.
 *  stream is print destination, e.g. a FILE*
 *
 ***************************************************************************/

static void show_pid(FILE *stream, const erlang_pid *pid)
{
    fprintf(stream,"#Pid<%s.%u.%u.%u>",
	    pid->node,pid->num,pid->serial,pid->creation);
}

static void show_trace(FILE *stream, const erlang_trace *t)
{
    fprintf(stream,
	    "Trace: Label: %ld, Flags: 0x%lx serial: %ld, prev: %ld From: ",
	    t->label,t->flags,t->serial,t->prev);
    show_pid(stream,&t->from);
}

/***************************************************************************
 *
 *  Try do decide if a buffer only contains printable characters
 *
 ***************************************************************************/

/* we only need to initialize some of these (after 32 everything printable) */
/* FIXME they are not!!!! We use isprint() for now but we could create a */
/* custom print function that escape some non printable like \t and \n */
#if 0
static int non_printable[256] = {
    /*                  1                   2                   3   */
    /*  2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 */
    1,1,1,1,1,1,1,1,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1
    /*             \b\t\n\v\f\r                                     */
};
#endif

static int printable_list_p(const uint8 *buf, int buflen)
{
    int i;
  
    for (i=0; i<buflen; i++) if (!isprint(buf[i])) return 0;

    /* is printable */
    return 1;
}

/***************************************************************************
 *
 *  Skip over bignums, we can't print them
 *
 ***************************************************************************/

/* FIXME we can if bignum small enough or if we use Per's functions or
   if we have compiled in gmp support */

/* this function doesn't do anything but skip over the number in the buffer */
/* it doesn't really belong here either... */

static int ei_decode_skip_bignum(const char *buf, int *index, void *p)
{
    const char *s = buf + *index;
    const char *s0 = s;
    long n;
  
    switch (get8(s)) {
    case ERL_LARGE_BIG_EXT:
	n = get32be(s);
	s += n+1;
	break;
    
    default:
	erl_errno = EIO;
	return -1;
    }

    *index += s-s0;
  
    return 0;
}
