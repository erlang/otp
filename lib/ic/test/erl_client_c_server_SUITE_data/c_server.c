/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2002-2016. All Rights Reserved.
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
/* C-server for test of IC.
 *
 * The C-node implemented here connects to its peer node, waits for
 * one message, evaluates the message, returns an result message, and 
 * terminates.
 * 
 * TODO:
 *
 * 1. XXX #includes for VxWorks, Windows
 */

#include <stdio.h>
#include <stdlib.h>

#ifndef __WIN32__
#  include <unistd.h>
#endif

#include <string.h>

#ifdef __WIN32__
#  include <time.h>
#  include <sys/timeb.h>
#elif defined VXWORKS
#  include <time.h>
#  include <sys/times.h>
#else
#  include <sys/time.h>
#endif

#include <ctype.h>

#ifdef __WIN32__
#  include <winsock2.h>
#  include <windows.h>
#else
#  include <sys/types.h>
#  include <sys/socket.h>
#  include <netinet/in.h>
#  include <arpa/inet.h>
#  include <netdb.h>
#endif

#include "ic.h"
#include "ei.h"
#include "erl_interface.h"
#include "eicode.h"
#include "m_i__s.h"
#include "m__s.h"

#ifdef __WIN32__
typedef struct {
    long tv_sec;
    long tv_usec;
} MyTimeval;
#else
typedef struct timeval MyTimeval;
#endif
static void my_gettimeofday(MyTimeval *tv);
static void showtime(MyTimeval *start, MyTimeval *stop);
static void usage(void);
static void done(int r);

#define HOSTNAMESZ 	255
#define NODENAMESZ 	512
#define INBUFSZ 	 10
#define OUTBUFSZ 	  0
#define MAXTRIES 	  5

static char *progname;

/* main */
#ifdef VXWORKS
int c_server(int argc, char **argv)
#else
int main(int argc, char **argv)
#endif
{
    struct hostent *hp;
    MyTimeval start, stop;
    int i, fd, ires, tries;
    CORBA_Environment *env;
    char *this_node_name = NULL;
    char *peer_node = NULL;
    char *cookie = NULL;
    char host[HOSTNAMESZ + 1];
    char this_node[NODENAMESZ + 1];
    erlang_msg msg;
    int status, loop; 
    
#ifdef __WIN32__
    WORD wVersionRequested;
    WSADATA wsaData;
    
    wVersionRequested = MAKEWORD(2, 0);
    
    if (WSAStartup(wVersionRequested, &wsaData) != 0) {
	fprintf(stderr, "Could not load winsock2 v2.0 compatible DLL");
	exit(1);
    }
#endif
    
    progname = argv[0];
    host[HOSTNAMESZ] = '\0';
    if (gethostname(host, HOSTNAMESZ + 1) < 0) {
	fprintf(stderr, "Can't find own hostname\n");
	done(1);
    } 
    if ((hp = gethostbyname(host)) == 0) {
	fprintf(stderr, "Can't get ip address for host %s\n", host);
	done(1);
    }
    for (i = 1; i < argc; i++) {
	if (strcmp(argv[i], "-help") == 0) {
	    usage();
	    done(0);
	} else if (strcmp(argv[i], "-this-node-name") == 0) {
	    i++;
	    this_node_name = argv[i];
	} else if (strcmp(argv[i], "-peer-node") == 0) {
	    i++;
	    peer_node = argv[i];
	} else if (strcmp(argv[i], "-cookie") == 0) {
	    i++;
	    cookie = argv[i];
	} else {
	    fprintf(stderr, "Error : invalid argument \"%s\"\n", argv[i]);
	    usage();
	    done(1);
	}
    }

    if (this_node_name == NULL || peer_node == NULL || cookie == NULL) {
	fprintf(stderr, "Error: missing option\n");
	usage();
	done(1);
    }

    /* Behead hostname at first dot */
    for (i=0; host[i] != '\0'; i++) {
	if (host[i] == '.') { host[i] = '\0'; break; }
    }
    sprintf(this_node, "%s@%s", this_node_name, host);

    fprintf(stderr, "c_server: this node: \"%s\"\n", this_node);
    fprintf(stderr, "c_server: peer node: \"%s\"\n", peer_node);

    /* initialize erl_interface */
    erl_init(NULL, 0);

    for (tries = 0; tries < MAXTRIES; tries++) {
	/* connect to peer node */ 
    	ires = erl_connect_xinit(host, this_node_name, this_node, 
				 (struct in_addr *)*hp->h_addr_list, 
				 cookie, 0);
	fprintf(stderr, "c_server: erl_connect_xinit(): %d\n", ires);
    
	fd = erl_connect(peer_node);
	fprintf(stderr, "c_server: erl_connect(): %d\n", fd);
	if (fd >= 0) 
	    break;
	fprintf(stderr, "c_server: cannot connect, retrying\n");
    }
    if (fd < 0) {
	fprintf(stderr, "c_server: cannot connect, exiting\n");
	done(1);
    }
    env = CORBA_Environment_alloc(INBUFSZ, OUTBUFSZ);
    env->_fd = fd; 

    status = 1;
    loop = 1;
    my_gettimeofday(&start);
    while (status >= 0 && loop > 0) {
        status = ei_receive_encoded(env->_fd, &env->_inbuf, &env->_inbufsz, 
				    &msg, &env->_iin); 
	switch(status) {
	case ERL_SEND:
	case ERL_REG_SEND:
	    /* get result */
	    m_i__switch(NULL, env);
	    switch(env->_major) {
	    case CORBA_NO_EXCEPTION:
		break;
	    case CORBA_SYSTEM_EXCEPTION:
		fprintf(stderr, "Request failure, reason : %s\n", 
			(char *) CORBA_exception_value(env));
		CORBA_exception_free(env);
		break;
	    default: /* Should not happen */
		CORBA_exception_free(env);
		break;
	    }
	    /* send back result data */
	    if (env->_iout > 0) 
		ei_send_encoded(env->_fd, &env->_caller, env->_outbuf, 
				env->_iout);
	    loop = 0;
	    break;
	case ERL_TICK:
	    break;
	default: 
	    if (status < 0) {
		fprintf(stderr, "Status negative: %d\n", status);
		loop = 0;
	    }
	    break;
	}
    }	
    my_gettimeofday(&stop);
    showtime(&start, &stop);

    erl_close_connection(fd);

    CORBA_free(env->_inbuf);
    CORBA_free(env->_outbuf);
    CORBA_free(env);
    if (status < 0) 
	done(-status);
    else 
	done(0); 
}

static void usage() 
{
    fprintf(stderr, "Usage: %s [-help] -this-node-name <name> "
	    "-peer-node <nodename> -cookie <cookie>\n", progname);
    fprintf(stderr, "Example:\n  %s  -this-node-name kalle "
	    "-peer-node olle@home -cookie oa678er\n", progname);
}

static void done(int r) 
{
#ifdef __WIN32__
    WSACleanup();
#endif
    exit(r);
}

static void showtime(MyTimeval *start, MyTimeval *stop)
{
    MyTimeval elapsed;

    elapsed.tv_sec = stop->tv_sec - start->tv_sec;
    elapsed.tv_usec = stop->tv_usec - start->tv_usec;
    while (elapsed.tv_usec < 0) {
	elapsed.tv_sec -= 1;
	elapsed.tv_usec += 1000000;
    }
    fprintf(stderr,"%ld.%06ld seconds\n",elapsed.tv_sec, elapsed.tv_usec);
}



static void my_gettimeofday(MyTimeval *tv)
#ifdef __WIN32__
#define EPOCH_JULIAN_DIFF 11644473600i64
{
    SYSTEMTIME t;
    FILETIME ft;
    LONGLONG lft;

    GetSystemTime(&t);
    SystemTimeToFileTime(&t, &ft);
    memcpy(&lft, &ft, sizeof(lft));
    tv->tv_usec = (long) ((lft / 10i64) % 1000000i64);
    tv->tv_sec = (long) ((lft / 10000000i64) - EPOCH_JULIAN_DIFF);
}
#elif defined VXWORKS
{
    int rate = sysClkRateGet(); /* Ticks per second */
    unsigned long ctick = tickGet();
    tv->tv_sec = ctick / rate; /* secs since reboot */
    tv->tv_usec = ((ctick - (tv->tv_sec * rate))*1000000)/rate;
}
#else
{
    gettimeofday(tv, NULL);
}
#endif
