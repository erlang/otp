/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1996-2016. All Rights Reserved.
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
/*
 * Purpose: Connect to any node at any host.
 */

/***************************************************************************
 *
 *  'erl_interface' node connection handling is to use 'ei' for all
 *  operations without access to the internal structure of saved data,
 *  e.i. it should use the public interface functions. The connection
 *  handling can be seen as a restricted node interface where only one
 *  node can be used in one operating system process.
 *
 ***************************************************************************/

#include "eidef.h"

#include <stdlib.h>
#include <sys/types.h>
#include <fcntl.h>

#ifdef __WIN32__
#include <winsock2.h>
#include <windows.h>
#include <winbase.h>

#elif VXWORKS
#include <vxWorks.h>
#include <hostLib.h>
#include <selectLib.h>
#include <ifLib.h>
#include <sockLib.h>
#include <taskLib.h>
#include <inetLib.h>

#include <unistd.h>
#include <sys/types.h>
#include <sys/times.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/tcp.h> 
#include <timers.h> 

#include "erl_error.h"

#else /* some other unix */
#include <unistd.h>
#include <sys/types.h>
#include <sys/times.h>

#if TIME_WITH_SYS_TIME
# include <sys/time.h>
# include <time.h>
#else
# if HAVE_SYS_TIME_H
#  include <sys/time.h>
# else
#  include <time.h>
# endif
#endif

#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/tcp.h> 
#include <arpa/inet.h>
#include <netdb.h>
#include <sys/utsname.h>  /* for gen_challenge (NEED FIX?) */
#endif

/* common includes */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

/* FIXME include less */
#include "erl_interface.h"
#include "erl_connect.h"
#include "erl_eterm.h"
#include "erl_malloc.h"
#include "putget.h"
#include "ei.h"
#include "ei_connect_int.h"
#include "ei_locking.h"
#include "ei_epmd.h"
#include "ei_internal.h"

/* rpc_from() uses a buffer this size */
#ifndef MAX_RECEIVE_BUF
#define MAX_RECEIVE_BUF 32*1024
#endif

/* This is the global state of the old erl_* API */

static ei_cnode erl_if_ec;

/***************************************************************************
 *
 *  API: erl_connect_init()
 *  API: erl_connect_xinit()
 *
 *  Returns 1 on success and 0 on failure.
 *  Not documented to set erl_errno.
 *
 ***************************************************************************/

int erl_connect_init(int this_node_number, char *cookie, short creation)
{
    char nn[MAXATOMLEN];

    sprintf(nn, "c%d", this_node_number);

    return ei_connect_init(&erl_if_ec, nn, cookie, creation) == 0;
}

/* FIXME documented to use struct in_addr as addr */

int erl_connect_xinit(char *thishostname,
		      char *thisalivename,
		      char *thisnodename,
		      struct in_addr *thisipaddr,
		      char *cookie,
		      short creation)
{
    return ei_connect_xinit(&erl_if_ec, thishostname, thisalivename,
			    thisnodename, thisipaddr, cookie, creation) >= 0;
}

/***************************************************************************
 *
 *  API: erl_connect()
 *  API: erl_xconnect()
 *
 *  Set up a connection to a given Node, and interchange hand shake
 *  messages with it.
 *
 *  Returns valid file descriptor on success and < 0 on failure.
 *  Set erl_errno to EHOSTUNREACH, ENOMEM, EIO or errno from socket(2)
 *  or connect(2).
 *
 ***************************************************************************/

int erl_connect(char *nodename)
{
  int res = ei_connect(&erl_if_ec, nodename);
  if (res < 0) erl_errno = EIO;
  return res;
}

/* FIXME documented to use struct in_addr as addr */

int erl_xconnect(Erl_IpAddr addr, char *alivename)
{
    return ei_xconnect(&erl_if_ec, addr, alivename);
}


/***************************************************************************
 *
 *  API: erl_close_connection()
 *
 *  Close a connection. FIXME call ei_close_connection() later. 
 *
 *  Returns 0 on success and -1 on failure.
 *
 ***************************************************************************/

int erl_close_connection(int fd)
{
    return closesocket(fd);
}

/*
 * Accept and initiate a connection from another
 * Erlang node. Return a file descriptor at success,
 * otherwise -1;
 */
int erl_accept(int lfd, ErlConnect *conp)
{
    return ei_accept(&erl_if_ec, lfd, conp);
}


/* Receives a message from an Erlang socket.
 * If the message was a TICK it is immediately
 * answered. Returns: ERL_ERROR, ERL_TICK or
 * the number of bytes read.
 */
int erl_receive(int s, unsigned char *bufp, int bufsize) 
{
    return ei_receive(s, bufp, bufsize);
}

/* 
 * Send an Erlang message to a registered process
 * at the Erlang node, connected with a socket.
 */
int erl_reg_send(int fd, char *server_name, ETERM *msg)
{
    ei_x_buff x;
    int r;

    ei_x_new_with_version(&x);
    if (ei_x_encode_term(&x, msg) < 0) {
	erl_errno = EINVAL;
	r = 0;
    } else {
	r = ei_reg_send(&erl_if_ec, fd, server_name, x.buff, x.index);
    }
    ei_x_free(&x);
    return r == 0;
}

/* 
 * Sends an Erlang message to a process at an Erlang node
 */
int erl_send(int fd, ETERM *to ,ETERM *msg)
{
    erlang_pid topid;
    ei_x_buff x;
    int r;

    ei_x_new_with_version(&x);
    ei_x_encode_term(&x, msg);
    /* make the to-pid */
    if (!ERL_IS_PID(to)) {
	ei_x_free(&x);
	erl_errno = EINVAL;
	return -1;
    }

    if (to->uval.pidval.node.latin1) {
	strcpy(topid.node, to->uval.pidval.node.latin1);
    }
    else {
	strcpy(topid.node, to->uval.pidval.node.utf8);
    }    
    topid.num = ERL_PID_NUMBER(to);
    topid.serial = ERL_PID_SERIAL(to);
    topid.creation = ERL_PID_CREATION(to);
    r = ei_send(fd, &topid, x.buff, x.index);
    ei_x_free(&x);
    return r == 0;
}

static int erl_do_receive_msg(int fd, ei_x_buff* x, ErlMessage* emsg)
{
    erlang_msg msg;

    int r;
    msg.from.node[0] = msg.to.node[0] = msg.toname[0] = '\0';
    r = ei_do_receive_msg(fd, 0, &msg, x, 0);

    if (r == ERL_MSG) {
	int index = 0;
	emsg->type = msg.msgtype;

	/*
	  We can't call ei_decode_term for cases where there are no
	  data following the type information. If there are other
	  types added later where there are data this case has to be
	  extended.
	*/

	switch (msg.msgtype) {
	case ERL_SEND:
	case ERL_REG_SEND:
	case ERL_EXIT:
	case ERL_EXIT2:
	  if (ei_decode_term(x->buff, &index, &emsg->msg) < 0)
	    r = ERL_ERROR;
	  break;
	default:
	  emsg->msg = NULL;	/* Not needed but may avoid problems for unsafe caller  */
	  break;
	}
    } else
	emsg->msg = NULL;
    if (msg.from.node[0] != '\0')
	emsg->from = erl_mk_pid(msg.from.node, msg.from.num, msg.from.serial, msg.from.creation);
    else
	emsg->from = NULL;
    if (msg.to.node[0] != '\0')
	emsg->to = erl_mk_pid(msg.to.node, msg.to.num, msg.to.serial, msg.to.creation);
    else
	emsg->to = NULL;
    strcpy(emsg->to_name, msg.toname);
    return r;
}

int erl_receive_msg(int fd, unsigned char *buf, int bufsize, ErlMessage *emsg)
{
    ei_x_buff x;
    int r;

    ei_x_new(&x);
    r = erl_do_receive_msg(fd, &x, emsg);
    /* FIXME what is this about? */
    if (bufsize > x.index)
	bufsize = x.index;
    memcpy(buf, x.buff, bufsize);
    ei_x_free(&x);
    return r;
}

int erl_xreceive_msg(int fd, unsigned char **buf, int *bufsize,
		  ErlMessage *emsg)
{
    ei_x_buff x;
    int r;

    ei_x_new(&x);
    r = erl_do_receive_msg(fd, &x, emsg);
    if (*bufsize < x.index)
	*buf = erl_realloc(*buf, x.index);
    *bufsize = x.index;
    memcpy(*buf, x.buff, *bufsize);
    ei_x_free(&x);
    return r;
}

/* 
 * The RPC consists of two parts, send and receive.
 * Here is the send part ! 
 * { PidFrom, { call, Mod, Fun, Args, user }} 
 */
/*
 * Now returns non-negative number for success, negative for failure.
 */
int erl_rpc_to(int fd, char *mod, char *fun, ETERM *args)
{
    int r;
    ei_x_buff x;

    ei_x_new(&x);
    ei_x_encode_term(&x, args);
    r = ei_rpc_to(&erl_if_ec, fd, mod, fun, x.buff, x.index);
    ei_x_free(&x);
    return r;
} /* rpc_to */

  /*
  * And here is the rpc receiving part. A negative
  * timeout means 'infinity'. Returns either of: ERL_MSG,
  * ERL_TICK, ERL_ERROR or ERL_TIMEOUT.
*/
int erl_rpc_from(int fd, int timeout, ErlMessage *emsg) 
{
    fd_set readmask;
    struct timeval tv;
    struct timeval *t = NULL;
    unsigned char rbuf[MAX_RECEIVE_BUF];    
    
    if (timeout >= 0) {
	tv.tv_sec = timeout / 1000;
	tv.tv_usec = (timeout % 1000) * 1000;
	t = &tv;
    }
    
    FD_ZERO(&readmask);
    FD_SET(fd,&readmask);
    
    switch (select(fd+1, &readmask, NULL, NULL, t)) {
    case -1: 
	erl_errno = EIO;
	return ERL_ERROR;
    case 0:
	erl_errno = ETIMEDOUT;
	return ERL_TIMEOUT;
    default:
	if (FD_ISSET(fd, &readmask)) 
	    return erl_receive_msg(fd, rbuf, MAX_RECEIVE_BUF, emsg);
	else {
	    erl_errno = EIO;
	    return ERL_ERROR;
	}
    }
} /* rpc_from */

/*
 * A true RPC. It return a NULL pointer
 * in case of failure, otherwise a valid
 * (ETERM *) pointer containing the reply
 */
ETERM *erl_rpc(int fd, char *mod, char *fun, ETERM *args)
{
    int i;
    ETERM *ep;
    ErlMessage emsg;
    
    if (erl_rpc_to(fd, mod, fun, args) < 0) {
	return NULL; }
    while ((i=erl_rpc_from(fd, ERL_NO_TIMEOUT, &emsg)) == ERL_TICK);
    
    if (i == ERL_ERROR)  return NULL;
    
    ep = erl_element(2,emsg.msg); /* {RPC_Tag, RPC_Reply} */
    erl_free_term(emsg.msg);
    erl_free_term(emsg.to);
    return ep;
} /* rpc */


/*
 ** Handshake
 */

int erl_publish(int port)
{
    return ei_publish(&erl_if_ec, port);
} 

int erl_unpublish(const char *alive)
{
    return ei_unpublish_tmo(alive,0);
}

erlang_pid *erl_self(void)
{
    return ei_self(&erl_if_ec);
}

const char *erl_thisnodename(void)
{
    return ei_thisnodename(&erl_if_ec);
}

const char *erl_thishostname(void)
{
    return ei_thishostname(&erl_if_ec);
}

const char *erl_thisalivename(void)
{
    return ei_thisalivename(&erl_if_ec);
}

const char *erl_thiscookie(void)
{
    return ei_thiscookie(&erl_if_ec);
}

short erl_thiscreation(void)
{
    return ei_thiscreation(&erl_if_ec);
}
