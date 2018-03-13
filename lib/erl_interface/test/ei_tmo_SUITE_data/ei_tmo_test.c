/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2003-2016. All Rights Reserved.
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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifdef VXWORKS
#include "reclaim.h"
#endif

#ifdef __WIN32__
#include <winsock2.h>
#include <windows.h>
#else
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#endif

#include "ei_runner.h"

#ifndef __WIN32__
#define closesocket(X) close(X)
#endif

#define DEBUG 1

#ifdef DEBUG
#include <stdarg.h>

FILE *debugfile =  NULL;
#define OPEN_DEBUGFILE(Number) debugf_open(Number)
#define CLOSE_DEBUGFILE() debugf_close()
#define DEBUGF(X) debugf X

static void debugf(char *format, ...)
{
    va_list ap;
    va_start(ap,format);
    if (debugfile) {
	vfprintf(debugfile,format,ap);
	fflush(debugfile);
    } else {
	fprintf(stderr,"Attempt to write to debugfile when not open...\n");
    }
    va_end(ap);
}

static void debugf_open(int number)
{
    char filename[1024];
    sprintf(filename,"ei_tmo_test%d.debug",number);
#if !defined(VXWORKS) && !defined(__WIN32__)
    close(2);
#endif
    debugfile = fopen(filename,"a");
    fprintf(debugfile,"===================================================\n");
}

static void debugf_close(void)
{
    if (debugfile) 
	fclose(debugfile);
}

#else
#define OPEN_DEBUGFILE(X) /* noop */
#define CLOSE_DEBUGFILE() /* noop */
#define DEBUGF(X) /* noop */
#endif

TESTCASE(framework_check)
{
    char *ptr = NULL;
    int len;

#ifdef DEBUG
    int version;
    int i;
#endif

    OPEN_DEBUGFILE(1);
    
    DEBUGF(("Börjar... \n"));
    ptr = read_packet(&len);
    if (*ptr != 't') {
	DEBUGF(("Gick fel \n"));
	report(1);
    } else {
	ei_x_buff x;
	ei_x_new(&x);
	ei_x_append_buf(&x, ptr+1,len-1);
	DEBUGF(("Gick bra? %d\n",x.index));
#ifdef DEBUG
	for(i=0;i < x.index; ++i)
	    DEBUGF(("%d ",(int) ((unsigned char *) x.buff)[i]));
	DEBUGF(("\n"));
	len = 0;
	ei_decode_version(x.buff,&len,&version);
	ei_print_term(debugfile,x.buff,&len);
	fflush(debugfile);
#endif
	send_bin_term(&x);
	ei_x_free(&x);
    }
    if (ptr != NULL)
	free(ptr);
    CLOSE_DEBUGFILE();
    report(1);
}

int decode_request(char **nodename_p, char **cookie_p, char **peername_p)
{
    char *nodename = NULL;
    char *cookie = NULL;
    char *peername = NULL;
    char *ptr = NULL;
    ei_x_buff x;
    int len;
    int version;
    int type;
    int size;
    int expected_size = (peername_p == NULL) ? 2 : 3;
    int ret = -1;
    
    ptr = read_packet(&len);
    ei_x_new(&x);
    if (*ptr != 't') {
	goto cleanup;
    } 
    ei_x_append_buf(&x, ptr+1,len-1);
    len = 0;
    ei_decode_version(x.buff,&len,&version);
#ifdef DEBUG
    {
	int tlen = len;
	ei_print_term(debugfile,x.buff,&tlen);
	DEBUGF(("\n"));
    }
#endif
    if (ei_get_type(x.buff,&len,&type,&size) != 0) {
	DEBUGF(("Failure at line %d\n",__LINE__));
	goto cleanup;
    }
    if (type != ERL_SMALL_TUPLE_EXT || size != expected_size) {
	DEBUGF(("Failure at line %d, type=%d, size = %d\n",__LINE__,
		type,size));
	goto cleanup;
    }
    if (ei_decode_tuple_header(x.buff,&len,&size) != 0 || size != expected_size) {
	DEBUGF(("Failure at line %d\n",__LINE__));
	goto cleanup;
    }
    if (ei_get_type(x.buff,&len,&type,&size) != 0) {
	DEBUGF(("Failure at line %d\n",__LINE__));
	goto cleanup;
    }
    if (type != ERL_ATOM_EXT) {
	DEBUGF(("Failure at line %d\n",__LINE__));
	goto cleanup;
    }
    nodename = malloc(size+1);
    ei_decode_atom(x.buff,&len,nodename);
    nodename[size] = '\0'; /* needed????? */
    if (ei_get_type(x.buff,&len,&type,&size) != 0) {
	DEBUGF(("Failure at line %d\n",__LINE__));
	goto cleanup;
    }
    if (type != ERL_ATOM_EXT) {
	DEBUGF(("Failure at line %d\n",__LINE__));
	goto cleanup;
    }
    cookie = malloc(size + 1);
    ei_decode_atom(x.buff,&len,cookie);
    cookie[size] = '\0'; /* needed????? */
    if (expected_size > 2) {
	if (ei_get_type(x.buff,&len,&type,&size) != 0) {
	    DEBUGF(("Failure at line %d\n",__LINE__));
	    goto cleanup;
	}
	if (type != ERL_ATOM_EXT) {
	    DEBUGF(("Failure at line %d\n",__LINE__));
	    goto cleanup;
	}
	peername = malloc(size + 1);
	ei_decode_atom(x.buff,&len,peername);
	peername[size] = '\0'; /* needed????? */
	DEBUGF(("nodename = %s, cookie = %s, peername = %s\n",
		nodename, cookie, peername));
	*peername_p = peername;
	peername = NULL;
    } else {
	DEBUGF(("nodename = %s, cookie = %s\n",
		nodename, cookie));
    }
    *nodename_p = nodename;
    nodename = NULL;
    *cookie_p = cookie;
    cookie = NULL;
    ret = 0;
 cleanup:
    ei_x_free(&x);
    if (ptr != NULL) {
	free(ptr);
    }
    if (nodename != NULL) {
	free(nodename);
    }
    if (cookie != NULL) {
	free(cookie);
    }
    if (peername != NULL) {
	free(peername);
    }
    return ret;
}

int get_message(int com_sock, ei_x_buff *buff, 
		char *atom_buff, erlang_pid *pid, int *iterations)
{
    ei_x_buff buffer;
    int ret_val,index;
    erlang_msg msg;
    int res = -1;
    int totlen;
    int type;
    int size;
    int version;
    long tmp;

    ei_x_new(&buffer);

    for (;;) {
	/* Reset buffer index before reading */
	buffer.index = 0; 
	/* Receive message */
	if ((ret_val = ei_xreceive_msg(com_sock, &msg, &buffer)) ==
	    ERL_TICK) {
	    /* Ticks are automatically answered, just continue */
	    continue;
	} else if (ret_val != ERL_MSG) {
	    DEBUGF(("Peer has closed, ret_val = %d (%d).\n",
		    ret_val,erl_errno));
	    goto cleanup;
	}
	switch (msg.msgtype) {
	case ERL_SEND:
	case ERL_REG_SEND:
	    index = 0;
	    ei_decode_version(buffer.buff,&index,&version);
	    DEBUGF(("Peer sent the following message to me: "));
#ifdef DEBUG
	    {
		int ndx = index;
		/*in debug log on Unix*/
		ei_print_term(debugfile, buffer.buff, &ndx);
	    }
#endif
	    DEBUGF(("\n"));
	    if (ei_get_type(buffer.buff,&index,&type,&size) != 0) {
		DEBUGF(("Failure at line %d\n",__LINE__));
		goto cleanup;
	    }
	    if (type != ERL_SMALL_TUPLE_EXT || size != 3) {
		DEBUGF(("Failure at line %d, type=%d, size = %d\n",__LINE__,
			type,size));
		goto cleanup;
	    }
	    if (ei_decode_tuple_header(buffer.buff,&index,&size) != 0 || 
		size != 3) {
		DEBUGF(("Failure at line %d\n",__LINE__));
		goto cleanup;
	    }
	    if (ei_get_type(buffer.buff,&index,&type,&size) != 0) {
		DEBUGF(("Failure at line %d\n",__LINE__));
		goto cleanup;
	    }
	    if (type == ERL_ATOM_EXT) {
		ei_decode_atom(buffer.buff,&index,atom_buff);
		atom_buff[size] ='\0';
		res = 2;
	    } else if (type == ERL_PID_EXT) {
		ei_decode_pid(buffer.buff,&index,pid);
		res = 1;
	    } else {
		DEBUGF(("Failure at line %d\n",__LINE__));
		goto cleanup;
	    }
	    if (ei_get_type(buffer.buff,&index,&type,&size) != 0) {
		DEBUGF(("Failure at line %d\n",__LINE__));
		goto cleanup;
	    }
	    switch (type) {
	    case ERL_SMALL_INTEGER_EXT:
	    case ERL_INTEGER_EXT:
		ei_decode_long(buffer.buff,&index,&tmp);
		break;
	    default:
		DEBUGF(("Failure at line %d\n",__LINE__));
		goto cleanup;
	    }

	    *iterations = (int)tmp;

	    totlen = buffer.index - index;
	    ei_x_append_buf(buff,buffer.buff+index,totlen);
	    goto cleanup;
	default:
	    DEBUGF(("Unexpected message type from peer. Goodbye.\n"));
	    goto cleanup;
	}
    }

 cleanup:
    ei_x_free(&buffer);
    return res;
}
TESTCASE(recv_tmo)
{
    char *nodename = NULL;
    char *cookie = NULL;
    char *peername = NULL;
    int com_sock = -1;
    ei_cnode nodeinfo;


    OPEN_DEBUGFILE(5);

    if (decode_request(&nodename,&cookie,&peername) != 0) {
	goto cleanup;
    }
    if (ei_connect_init(&nodeinfo, nodename, cookie, 0) < 0) {
	DEBUGF(("Failure at line %d\n",__LINE__));
	goto cleanup;
    }
	
    if ((com_sock = ei_connect_tmo(&nodeinfo, peername, 5000)) < 0) {
	ei_x_buff answer;
	DEBUGF(("Got error while connecting.{%d,%d}\n",com_sock,erl_errno));
	ei_x_new(&answer);
	ei_x_format(&answer,"{~i,~i,~i}",com_sock,erl_errno,ETIMEDOUT);
#ifdef DEBUG
	{
	    int tlen = 0;
	    int v;
	    ei_decode_version(answer.buff,&tlen,&v);
	    ei_print_term(debugfile,answer.buff,&tlen);
	    DEBUGF(("\n"));
	}
#endif
	send_bin_term(&answer);
	DEBUGF(("Binary term sent.\n"));
	ei_x_free(&answer);
    } else {
	ei_x_buff answer;
	int ret_val;
	ei_x_buff buffer;
	erlang_msg msg;
	int index,version;

	DEBUGF(("Success when connecting.{%d,%d}\n",com_sock,erl_errno));
	ei_x_new(&answer);
	ei_x_format(&answer,"~i",com_sock);
	send_bin_term(&answer);
	ei_x_free(&answer);
	ei_x_new(&buffer);
	
	for (;;) {
	    /* Reset buffer index before reading */
	    buffer.index = 0; 
	    /* Receive message */
	    if ((ret_val = ei_xreceive_msg_tmo(com_sock, &msg, &buffer,5000)) 
		== ERL_TICK) {
		/* Ticks are automatically answered, just continue */
		continue;
	    } else if (ret_val != ERL_MSG) {
		ei_x_new(&answer);
		ei_x_format(&answer,"{~i,~i,~i}",ret_val,erl_errno,ETIMEDOUT);
		send_bin_term(&answer);
		ei_x_free(&answer);
		ei_x_free(&buffer);
		DEBUGF(("Got error receiving, sending {%d,%d} and exiting\n",
			ret_val,erl_errno));
		goto cleanup;
	    }
	    switch (msg.msgtype) {
	    case ERL_SEND:
	    case ERL_REG_SEND:
		index = 0;
		ei_decode_version(buffer.buff,&index,&version);
		DEBUGF(("Peer sent the following message to me: "));
#ifdef DEBUG
		{
		    int ndx = index;
		    /*in debug log on Unix*/
		    ei_print_term(debugfile, buffer.buff, &ndx);
		}
#endif
		DEBUGF(("\n"));
		send_bin_term(&buffer);
		ei_x_free(&buffer);
		goto cleanup;
	    default:
		DEBUGF(("Unexpected message type from peer. Goodbye.\n"));
		goto cleanup;
		
	    }
	}	
    }
cleanup:
    if (com_sock >= 0) {
	closesocket(com_sock);
    }

    if (nodename != NULL) {
	free(nodename);
    }
    if (cookie != NULL) {
	free(cookie);
    }
    if (peername != NULL) {
	free(peername);
    }
    CLOSE_DEBUGFILE();
    report(1);
}

TESTCASE(send_tmo)
{
    char *nodename = NULL;
    char *cookie = NULL;
    char *peername = NULL;
    int com_sock = -1;
    ei_cnode nodeinfo;


    OPEN_DEBUGFILE(4);

    if (decode_request(&nodename,&cookie,&peername) != 0) {
	goto cleanup;
    }
    if (ei_connect_init(&nodeinfo, nodename, cookie, 0) < 0) {
	DEBUGF(("Failure at line %d\n",__LINE__));
	goto cleanup;
    }
	
    if ((com_sock = ei_connect_tmo(&nodeinfo, peername, 5000)) < 0) {
	ei_x_buff answer;
	DEBUGF(("Got error while connecting.{%d,%d}\n",com_sock,erl_errno));
	ei_x_new(&answer);
	ei_x_format(&answer,"{~i,~i,~i}",com_sock,erl_errno,ETIMEDOUT);
#ifdef DEBUG
	{
	    int tlen = 0;
	    int v;
	    ei_decode_version(answer.buff,&tlen,&v);
	    ei_print_term(debugfile,answer.buff,&tlen);
	    DEBUGF(("\n"));
	}
#endif
	send_bin_term(&answer);
	DEBUGF(("Binary term sent.\n"));
	ei_x_free(&answer);
    } else {
	ei_x_buff answer;
	char atom[256];
	erlang_pid pid;
	int res, iterations, i;
	ei_x_buff send_buffer;

	DEBUGF(("Success when connecting.{%d,%d}\n",com_sock,erl_errno));
	ei_x_new(&answer);
	ei_x_format(&answer,"~i",com_sock);
	send_bin_term(&answer);
	ei_x_free(&answer);
	ei_x_new_with_version(&send_buffer);
	if ((res = get_message(com_sock, &send_buffer, 
			      atom ,&pid, &iterations)) < 0) {
	    DEBUGF(("Get_message_failure at line %d\n",__LINE__));
	    ei_x_free(&send_buffer);
	    goto cleanup;
	}
	DEBUGF(("Get_message success (%d), bindata:\n",res));
#ifdef DEBUG
	{
	    int ndx = 0;
	    int v;
	    ei_decode_version(send_buffer.buff,&ndx,&v);
	    ei_print_term(debugfile, send_buffer.buff, &ndx);
	}
#endif
	DEBUGF(("\n"));
	switch (res) {
	case 1: /* Send to pid in 'pid' */
	    ei_x_new(&answer);
	    for (i=0;i < iterations; ++i) {
		res = ei_send_tmo(com_sock, &pid, send_buffer.buff, 
				  send_buffer.index, 5000);
		if (res < 0) {
		    DEBUGF(("Sent bindata failed (%d) after %d iterations:\n", res, i));
		    break;
		}
#ifdef DEBUG
		if (i < 10 || (i % 100 == 0))  /* don't flood the log */
		{
		    int ndx = 0;
		    int v;
		    DEBUGF(("%d: Sent bindata (%d): ", i, res));
		    ei_decode_version(send_buffer.buff,&ndx,&v);
		    ei_print_term(debugfile, send_buffer.buff, &ndx);
		    DEBUGF(("\n"));
		}
#endif
	    }
	    if (res < 0) {
		DEBUGF(("ei_send_tmo failure at line %d\n",__LINE__));
		ei_x_format(&answer,"{~i,~i,~i,~i}",res,erl_errno,i,ETIMEDOUT);
	    } else {
		ei_x_format(&answer,"~i",res);
	    }
	    send_bin_term(&answer);
	    ei_x_free(&answer);
	    ei_x_free(&send_buffer);
	    goto cleanup;
	case 2: /* Registered name in 'atom' */
	    ei_x_new(&answer);
	    for (i=0;i < iterations; ++i) {
		res = ei_reg_send_tmo(&nodeinfo, com_sock, atom, 
				      send_buffer.buff, 
				      send_buffer.index,5000);
		if (res < 0) 
		    break;
	    }
	    if (res < 0) {
		DEBUGF(("ei_reg_send_tmo failure at line %d\n",__LINE__));
		ei_x_format(&answer,"{~i,~i,~i,~i}",res,erl_errno,i,ETIMEDOUT);
	    } else {
		ei_x_format(&answer,"~i",res);
	    }
	    send_bin_term(&answer);
	    ei_x_free(&answer);
	    ei_x_free(&send_buffer);
	    goto cleanup;
	default:
	    DEBUGF(("unexpected request number %d at line %d\n",res,__LINE__));
	    ei_x_free(&send_buffer);
	    goto cleanup;
	}
    }
cleanup:
    if (com_sock >= 0) {
	closesocket(com_sock);
    }

    if (nodename != NULL) {
	free(nodename);
    }
    if (cookie != NULL) {
	free(cookie);
    }
    if (peername != NULL) {
	free(peername);
    }
    CLOSE_DEBUGFILE();
    report(1);
}


TESTCASE(connect_tmo)
{
    char *nodename = NULL;
    char *cookie = NULL;
    char *peername = NULL;
    int com_sock = -1;
    ei_cnode nodeinfo;
    


    OPEN_DEBUGFILE(3);

    if (decode_request(&nodename,&cookie,&peername) != 0) {
	goto cleanup;
    }
    if (ei_connect_init(&nodeinfo, nodename, cookie, 0) < 0) {
	DEBUGF(("Failure at line %d\n",__LINE__));
	goto cleanup;
    }
	
    if ((com_sock = ei_connect_tmo(&nodeinfo, peername, 5000)) < 0) {
	ei_x_buff answer;
	DEBUGF(("Got error while connecting.{%d,%d}\n",com_sock,erl_errno));
	ei_x_new(&answer);

	/* On some systems errno gets set to EHOSTUNREACH rather than
	   ETIMEDOUT, which is ok. Let's check for that and report timeout
	   if it happens. 
	   Max OS X seems to respond EHOSTDOWN, which should be ok.
	*/


#if defined(EHOSTUNREACH)
	if (errno == EHOSTUNREACH) 
	    ei_x_format(&answer,"{~i,~i,~i}",com_sock,ETIMEDOUT,ETIMEDOUT);
	else
#endif

#if defined(EHOSTDOWN)
	if (errno == EHOSTDOWN)
	    ei_x_format(&answer,"{~i,~i,~i}",com_sock,ETIMEDOUT,ETIMEDOUT);
	else
#endif

	    ei_x_format(&answer,"{~i,~i,~i}",com_sock,erl_errno,ETIMEDOUT);

#ifdef DEBUG
	{
	    int tlen = 0;
	    int v;
	    ei_decode_version(answer.buff,&tlen,&v);
	    ei_print_term(debugfile,answer.buff,&tlen);
	    DEBUGF(("\n"));
	}
#endif
	send_bin_term(&answer);
	DEBUGF(("Binary term sent.\n"));
	ei_x_free(&answer);
    } else {
	ei_x_buff answer;
	DEBUGF(("Success when connecting.{%d,%d}\n",com_sock,erl_errno));
	ei_x_new(&answer);
	ei_x_format(&answer,"~i",com_sock);
	send_bin_term(&answer);
	ei_x_free(&answer);
    }

cleanup:
    if (com_sock >= 0) {
	closesocket(com_sock);
    }

    if (nodename != NULL) {
	free(nodename);
    }
    if (cookie != NULL) {
	free(cookie);
    }
    if (peername != NULL) {
	free(peername);
    }
    CLOSE_DEBUGFILE();
    report(1);
}

TESTCASE(accept_tmo)
{
    char *nodename = NULL;
    char *cookie = NULL;
    int listen_sock = -1;
    int epmd_sock = -1;
    int com_sock = -1;
    struct sockaddr_in sin;
    int sin_siz = sizeof(sin);
    ErlConnect peer;
    ei_cnode nodeinfo;
    


    OPEN_DEBUGFILE(2);

    putenv("EI_TRACELEVEL=10");

    if (decode_request(&nodename,&cookie,NULL) != 0) {
	goto cleanup;
    }
    if (ei_connect_init(&nodeinfo, nodename, cookie, 0) < 0) {
	DEBUGF(("Failure at line %d\n",__LINE__));
	goto cleanup;
    }
	
    if ((listen_sock = socket(AF_INET, SOCK_STREAM, 0)) < 0) {
	DEBUGF(("Failure at line %d\n",__LINE__));
	goto cleanup;
    }
    memset(&sin, 0, sizeof(sin));
    sin.sin_family = AF_INET;
    sin.sin_addr.s_addr = INADDR_ANY;
    
    if (bind(listen_sock,(struct sockaddr *) &sin, sizeof(sin)) != 0) {
	DEBUGF(("Failure at line %d\n",__LINE__));
	goto cleanup;
    }
    if (getsockname(listen_sock, 
		    (struct sockaddr *) &sin, &sin_siz) != 0) {
	DEBUGF(("Failure at line %d\n",__LINE__));
	goto cleanup;
    }
    if (listen(listen_sock, 5) != 0) {
	DEBUGF(("Failure at line %d\n",__LINE__));
	goto cleanup;
    }

    if ((epmd_sock = ei_publish(&nodeinfo, ntohs(sin.sin_port))) < 0) {
	DEBUGF(("Failure at line %d[%d,%d]\n",__LINE__,sin.sin_port,erl_errno));
	goto cleanup;
    }

    if ((com_sock = ei_accept_tmo(&nodeinfo, 
				  listen_sock, &peer, 5000)) == ERL_ERROR) {
	ei_x_buff answer;
	DEBUGF(("Got error while accepting.{%d,%d}\n",com_sock,erl_errno));
	ei_x_new(&answer);
	ei_x_format(&answer,"{~i,~i,~i}",com_sock,erl_errno,ETIMEDOUT);
#ifdef DEBUG
	{
	    int tlen = 0;
	    int v;
	    ei_decode_version(answer.buff,&tlen,&v);
	    ei_print_term(debugfile,answer.buff,&tlen);
	    DEBUGF(("\n"));
	}
#endif
	send_bin_term(&answer);
	DEBUGF(("Binary term sent.\n"));
	ei_x_free(&answer);
    } else {
	ei_x_buff answer;
	DEBUGF(("Success when connecting.{%d,%d}\n",com_sock,erl_errno));
	ei_x_new(&answer);
	ei_x_format(&answer,"~i",com_sock);
	send_bin_term(&answer);
	ei_x_free(&answer);
    }

cleanup:

    if (listen_sock >= 0) {
	closesocket(listen_sock);
    }
    if (epmd_sock >= 0) {
	closesocket(epmd_sock);
    }
    if (com_sock >= 0) {
	closesocket(com_sock);
    }

    if (nodename != NULL) {
	free(nodename);
    }
    if (cookie != NULL) {
	free(cookie);
    }
    CLOSE_DEBUGFILE();
    report(1);
}

