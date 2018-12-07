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
    int err;
    ei_socket_callbacks *cbs;
    void *ctx;
    ssize_t len, tot_len;
    unsigned tmo = ms == 0 ? EI_SCLBK_INF_TMO : ms;

    err = EI_GET_CBS_CTX__(&cbs, &ctx, fd);
    if (err) {
        EI_CONN_SAVE_ERRNO__(err);
        return ERL_ERROR;
    }

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


#ifdef EI_HAVE_STRUCT_IOVEC__
    if (ei_socket_callbacks_have_writev__(cbs)) {
        struct iovec v[2];

        v[0].iov_base = (char *)header;
        v[0].iov_len = index;
        v[1].iov_base = (char *)msg;
        v[1].iov_len = msglen;

        len = tot_len = (ssize_t) index+msglen;
        err = ei_writev_fill_ctx_t__(cbs, ctx, v, 2, &len, tmo);
        if (!err && len != tot_len)
            err = EIO;
        if (err) {
            EI_CONN_SAVE_ERRNO__(err);
            return -1;
        }

        return 0;
    }
#endif /* EI_HAVE_STRUCT_IOVEC__ */

    /* no writev() */
    len = tot_len = (ssize_t) index;
    err = ei_write_fill_ctx_t__(cbs, ctx, header, &len, tmo);
    if (!err && len != tot_len)
        err = EIO;
    if (err) {
        EI_CONN_SAVE_ERRNO__(err);
        return -1;
    }

    len = tot_len = (ssize_t) msglen;
    err = ei_write_fill_ctx_t__(cbs, ctx, msg, &len, tmo);
    if (!err && len != tot_len)
        err = EIO;
    if (err) {
        EI_CONN_SAVE_ERRNO__(err);
        return -1;
    }

    return 0;
}

int ei_send_encoded(int fd, const erlang_pid *to, char *msg, int msglen)
{
    return ei_send_encoded_tmo(fd, to, msg, msglen, 0);
}
