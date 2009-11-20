/*<copyright>
 * <year>1999-2008</year>
 * <holder>Ericsson AB, All Rights Reserved</holder>
 *</copyright>
 *<legalnotice>
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
 * The Initial Developer of the Original Code is Ericsson AB.
 *</legalnotice>
 */
/*
 * Purpose:  Implementation of Secure Socket Layer (SSL).
 *
 */

#ifndef ESOCK_H
#define ESOCK_H

#ifdef __WIN32__
#include "esock_winsock.h"
#endif
#include <stdio.h>

#ifdef __WIN32__
#define INVALID_FD			INVALID_SOCKET

#define sock_read(fd, buf, len)		recv((fd), (buf), (len), 0)
#define sock_write(fd, buf, len)	send((fd), (buf), (len), 0)
#define sock_close(fd)			closesocket(fd)
#define sock_errno()			WSAGetLastError()
#define sock_set_errno(err)		WSASetLastError(err)

#define ERRNO_NONE			0
#define ERRNO_BLOCK			WSAEWOULDBLOCK
#define ERRNO_CONNREFUSED		WSAECONNREFUSED
#define ERRNO_PROGRESS			WSAEINPROGRESS
#define ERRNO_PROTONOSUPPORT		WSAEPROTONOSUPPORT
#define ERRNO_INVAL			WSAEINVAL
#define ERRNO_ADDRNOTAVAIL		WSAEADDRNOTAVAIL
#define ERRNO_NOTSOCK			WSAENOTSOCK
#define ERRNO_OPNOTSUPP			WSAEOPNOTSUPP
#define ERRNO_MFILE			WSAEMFILE
#define SET_BLOCKING(fd)	    do { \
    					unsigned long zeroval = 0; \
    					ioctlsocket((fd), FIONBIO, &zeroval); \
				       } while (0)
#define SET_NONBLOCKING(fd)	    do { \
    					unsigned long oneval = 1; \
    					ioctlsocket((fd), FIONBIO, &oneval); \
				       } while (0)
#else
#define INVALID_FD			(-1)

#define sock_read(fd, buf, len)		read((fd), (buf), (len))
#define sock_write(fd, buf, len)	write((fd), (buf), (len))
#define sock_close(fd)			close(fd)
#define sock_errno()			errno
#define sock_set_errno(err)		do {errno = (err);} while(0)

#define ERRNO_NONE			0
#define ERRNO_BLOCK			EAGAIN
#define ERRNO_CONNREFUSED		ECONNREFUSED
#define ERRNO_PROGRESS			EINPROGRESS
#define ERRNO_PROTONOSUPPORT		EPROTONOSUPPORT
#define ERRNO_INVAL			EINVAL
#define ERRNO_ADDRNOTAVAIL		EADDRNOTAVAIL
#define ERRNO_NOTSOCK			ENOTSOCK
#define ERRNO_OPNOTSUPP			EOPNOTSUPP
#define ERRNO_MFILE			EMFILE
#define SET_BLOCKING(fd)        	fcntl((fd), F_SETFL, \
                                      	fcntl((fd), F_GETFL, 0) & ~O_NONBLOCK)
#define SET_NONBLOCKING(fd)     	fcntl((fd), F_SETFL, \
                                      	fcntl((fd), F_GETFL, 0) | O_NONBLOCK)
#endif

#define GET_INT8(s)    ((s)[0])
#define GET_INT16(s)   (((s)[0] << 8) | (s)[1])
#define GET_INT32(s)   (((s)[0] << 24) | ((s)[1] << 16) | \
			((s)[2] << 8) | (s)[3])

#define PUT_INT8(x, s)  do { (s)[0] = x; } while(0)
#define PUT_INT16(x, s) do { (s)[0] = ((x) >> 8) & 0xff; \
			     (s)[1] = ((x) & 0xff); } while(0)
#define PUT_INT32(x, s) do { (s)[0] = ((x) >> 24) & 0xff; \
			     (s)[1] = ((x) >> 16) & 0xff; \
			     (s)[2] = ((x) >> 8) & 0xff; \
			     (s)[3] = (x) & 0xff; } while(0)

/* type for Connections */
#define ESOCK_STATE_NONE	0
#define ESOCK_ACTIVE_LISTENING 	1
#define ESOCK_PASSIVE_LISTENING 2
#define ESOCK_CONNECTED		3
#define ESOCK_WAIT_CONNECT	4
#define ESOCK_SSL_CONNECT	5
#define ESOCK_SSL_ACCEPT	6
#define ESOCK_TRANSPORT_ACCEPT  7
#define ESOCK_JOINED		8
#define ESOCK_SSL_SHUTDOWN	9
#define ESOCK_DEFUNCT	       10

#ifdef __WIN32__
    typedef SOCKET FD;
#else
    typedef int FD;
#endif

/* For the shutdown(fd, how) call */
#ifdef __WIN32__
#define SHUTDOWN_READ  SD_RECEIVE
#define SHUTDOWN_WRITE SD_SEND
#define SHUTDOWN_ALL   SD_BOTH
#else
#define SHUTDOWN_READ  0
#define SHUTDOWN_WRITE 1
#define SHUTDOWN_ALL   2
#endif

#define ORIG_LISTEN  0
#define ORIG_ACCEPT  1
#define ORIG_CONNECT 2
 
typedef struct {
    int size;			/* Total size of buf */
    unsigned char *buf;
    int len;			/* Current number of bytes in buf */
    int offset;			/* Bytes already written  */
} WriteQueue;

typedef struct _proxy Proxy;

typedef struct Connection {
    FD fd;
    FD listen_fd;		/* Needed for async listen error */
    unsigned char state;
    int acceptors;		/* Count acceptors for listen socket */
    Proxy *proxy;
    void *opaque;		/* Any suitable ssl structure */
    int ssl_want;		/* read/write flags */
    int eof;			/* end of file (read) */
    int bp;			/* broken pipe (write) */
    int clean;			/* Clean SSL shutdown initiated */
    int close;			/* Close if set */
    int origin;			/* listen, accept or connect */
    int encrypted;		/* 1 = SSL encrypted, 0 = normal, unencrypted tcp */
    char *flags;		/* ssl parameters */
    FILE *logfp;		/* connection log file (not used) */
    WriteQueue wq;
    struct Connection* next;
    const char* errstr; 	/* only used to report errors from ssl_accept_init in SSL_ACCEPT */
} Connection;

struct _proxy {
    FD fd;
    int peer_port;
    int eof;			/* end of file (read) */
    int bp;			/* broken pipe (write) */
    Connection *conn;
    WriteQueue wq;
    Proxy *next;
};

/* Commands, replies, and error responses */

#define ESOCK_CONNECT_CMD	1
#define ESOCK_CONNECT_WAIT_REP	2
#define ESOCK_CONNECT_REP	3
#define ESOCK_CONNECT_ERR	4

#define ESOCK_TERMINATE_CMD	5
#define ESOCK_CLOSE_CMD	        6

#define ESOCK_LISTEN_CMD	7
#define ESOCK_LISTEN_REP	8
#define ESOCK_LISTEN_ERR	9

#define ESOCK_TRANSPORT_ACCEPT_CMD 10
#define ESOCK_NOACCEPT_CMD      11
#define ESOCK_TRANSPORT_ACCEPT_REP 12
#define ESOCK_TRANSPORT_ACCEPT_ERR 13

#define ESOCK_FROMNET_CLOSE_REP 14

#define ESOCK_CONNECT_SYNC_ERR	15
#define ESOCK_LISTEN_SYNC_ERR	16

#define ESOCK_PROXY_PORT_REP    23
#define ESOCK_PROXY_JOIN_CMD	24
#define ESOCK_PROXY_JOIN_REP	25
#define ESOCK_PROXY_JOIN_ERR	26

#define ESOCK_SET_SOCKOPT_CMD   27
#define ESOCK_IOCTL_OK          28
#define ESOCK_IOCTL_ERR		29

#define ESOCK_GETPEERNAME_CMD   30
#define ESOCK_GETPEERNAME_REP   31
#define ESOCK_GETPEERNAME_ERR   32

#define ESOCK_GETSOCKNAME_CMD   33
#define ESOCK_GETSOCKNAME_REP   34
#define ESOCK_GETSOCKNAME_ERR   35

#define ESOCK_GETPEERCERT_CMD   36
#define ESOCK_GETPEERCERT_REP   37
#define ESOCK_GETPEERCERT_ERR   38

#define ESOCK_GETVERSION_CMD    39
#define ESOCK_GETVERSION_REP    40

#define ESOCK_SET_SEED_CMD      41

#define ESOCK_GETCONNINFO_CMD   42
#define ESOCK_GETCONNINFO_REP   43
#define ESOCK_GETCONNINFO_ERR   44

#define ESOCK_SSL_ACCEPT_CMD     45
#define ESOCK_SSL_ACCEPT_REP     46
#define ESOCK_SSL_ACCEPT_ERR     47

#define ESOCK_DUMP_STATE_CMD      48
#define ESOCK_SET_DEBUG_CMD       49
#define ESOCK_SET_DEBUGMSG_CMD    50


/* Option codes  for ESOCK_SET_SOCKOPT_CMD */
#define ESOCK_SET_TCP_NODELAY	1

/* SSL want to read or write */
#define ESOCK_SSL_WANT_READ	1
#define ESOCK_SSL_WANT_WRITE	2

/* Protocol version according to ssl_server */
#define ESOCK_SSLv2		1
#define ESOCK_SSLv3		2
#define ESOCK_TLSv1		4


#endif

























