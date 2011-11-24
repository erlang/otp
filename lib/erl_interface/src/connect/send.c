/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1998-2011. All Rights Reserved.
 * 
 * The contents of this file are subject to the Erlang Public License,
 * Version 1.1, (the "License"); you may not use this file except in
 * compliance with the License. You should have received a copy of the
 * Erlang Public License along with this software. If not, it can be
 * retrieved online at http://www.erlang.org/.
 * 
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
 * the License for the specific language governing rights and limitations
 * under the License.
 * 
 * %CopyrightEnd%
 */

#ifdef __WIN32__

# include <winsock2.h>
# include <windows.h>
# include <winbase.h>

#elif VXWORKS

# include <sys/types.h>
# include <unistd.h>
# include <sysLib.h>
# include <tickLib.h>

#else /* unix */

# include <sys/types.h>
# include <unistd.h>
# include <sys/uio.h>

#endif

#include <string.h>

#include "eidef.h"
#include "eiext.h"
#include "eisend.h"
#include "putget.h"
#include "ei_connect_int.h"
#include "ei_internal.h"
#include "ei_trace.h"
#include "ei_portio.h"
#include "show_msg.h"


int ei_send_encoded_tmo(int fd, const erlang_pid *to, 
			char *msg, int msglen, unsigned ms) 
{
    char *s, header[1200]; /* see size calculation below */
    erlang_trace *token = NULL;
    int index = 5; /* reserve 5 bytes for control message */
    int res;
#ifdef HAVE_WRITEV
    struct iovec v[2];
#endif

    /* are we tracing? */
    /* check that he can receive trace tokens first */
    if (ei_distversion(fd) > 0) token = ei_trace(0,NULL);
    
    /* header = SEND, cookie, to                      max sizes: */
    ei_encode_version(header,&index);		      /*   1 */
    if (token) {
	ei_encode_tuple_header(header,&index,4);      /*   2 */
	ei_encode_long(header,&index,ERL_SEND_TT);    /*   2 */
    } else {
	ei_encode_tuple_header(header,&index,3);
	ei_encode_long(header,&index,ERL_SEND); 
    }
    ei_encode_atom(header,&index,ei_getfdcookie(fd)); /* 258 */
    ei_encode_pid(header,&index,to);		      /* 268 */
    
    if (token) ei_encode_trace(header,&index,token);  /* 534 */
    
    /* control message (precedes header actually) */
    /* length = 1 ('p') + header len + message len */
    s = header;
    put32be(s, index + msglen - 4);		      /*   4 */
    put8(s, ERL_PASS_THROUGH);			      /*   1 */
				/*** sum: 1070 */

    if (ei_tracelevel >= 4)
	ei_show_sendmsg(stderr,header,msg);

#ifdef HAVE_WRITEV
    
    v[0].iov_base = (char *)header;
    v[0].iov_len = index;
    v[1].iov_base = (char *)msg;
    v[1].iov_len = msglen;
    
    if ((res = ei_writev_fill_t(fd,v,2,ms)) != index+msglen) {
	erl_errno = (res == -2) ? ETIMEDOUT : EIO;
	return -1;
    }
  
#else  /* !HAVE_WRITEV */
  
    if ((res = ei_write_fill_t(fd,header,index,ms)) != index) { 
	erl_errno = (res == -2) ? ETIMEDOUT : EIO;
	return -1;
    }
    if ((res = ei_write_fill_t(fd,msg,msglen,ms)) != msglen) { 
	erl_errno = (res == -2) ? ETIMEDOUT : EIO;
	return -1;
    }

#endif  /* !HAVE_WRITEV */

    return 0;
}

int ei_send_encoded(int fd, const erlang_pid *to, char *msg, int msglen)
{
    return ei_send_encoded_tmo(fd, to, msg, msglen, 0);
}
