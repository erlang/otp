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
 * Purpose:  Std filedescriptors, break handler
 *
 */

#include <stdio.h>
#include <stdlib.h>
#ifdef __WIN32__
#include "esock_winsock.h"
#include <process.h>
#include <io.h>
#include <fcntl.h>
#else
#include <unistd.h>
#include <signal.h>  
#endif 

#include "esock.h"
#include "debuglog.h"
#include "esock_utils.h"
#include "esock_osio.h"

#ifdef __WIN32__
#define write	_write
#define read	_read
#define LOCALHOSTADDR	"127.0.0.1"
#define LOCBUFSIZE      1024
#endif

#define PACKET_SIZE	4
#define EBUFSIZE	256

FD local_read_fd = 0;

static int inc_rbuf(int size);
static void free_rbuf(void);
static int read_fill(unsigned char *buf, int len);
#ifdef __WIN32__
static int create_local_thread(void);
static DWORD WINAPI local_thread(LPVOID lpvParam);
static BOOL WINAPI signal_handler(DWORD ctrl);
#endif

static unsigned char *rbuf = NULL;
static int rbuf_malloced = 0;
#ifdef __WIN32__
static unsigned long one = 1, zero = 0;
static int local_portno;
static char *local_buf;
#endif

int set_break_handler(void) 
{
#ifndef __WIN32__
    struct sigaction act;

    /* Ignore SIGPIPE signal */
    sigemptyset(&act.sa_mask);
    act.sa_flags = 0;
    act.sa_handler = SIG_IGN;
    sigaction(SIGPIPE, &act, NULL);
    return 0;
#else 
    SetConsoleCtrlHandler(signal_handler, TRUE);
    return 0;
#endif
}


#ifdef __WIN32__

int set_binary_mode(void)
{
    _setmode(0, _O_BINARY);
    _setmode(1, _O_BINARY);
    return 0;
}

int esock_osio_init(void)
{
    return create_local_thread();
}

void esock_osio_finish(void)
{
    sock_close(local_read_fd);
}

#endif

int read_ctrl(unsigned char **ebufp)
{
    int tbh, cc;
    unsigned char *mbuf;

    if (inc_rbuf(EBUFSIZE) < 0) {
	fprintf(stderr, "read_ctrl: cannot alloc rbuf\n");
	return -1;
    }
    cc = read_fill(rbuf, PACKET_SIZE);
    if (cc < 0) {
	free_rbuf();
	return -1;
    }
    if (cc == 0) {
	free_rbuf();
	return -1;		/* XXX 0 ?? */
    }
    tbh = GET_INT32(rbuf);

    if (tbh > rbuf_malloced - 4) {
	if (inc_rbuf(tbh + 4) < 0)
	    return -1;
    }
    
    mbuf = rbuf + PACKET_SIZE;
    cc = read_fill(mbuf, tbh);
    DEBUGF(("-----------------------------------\n"));
    DEBUGF(("read_ctrl: cc = %d\n", cc));
    if(cc > 0) {
	DEBUGMSGF(("message (hex) : [%3.*a]\n", cc, mbuf));
	DEBUGMSGF(("message (char): [%3.*b]\n", cc, mbuf));
    }
    *ebufp = mbuf;
    return cc;
}

int write_ctrl(unsigned char *buf, int len)
{
    unsigned char lb[4];

    PUT_INT32(len, lb);
    DEBUGF(("write_ctrl: len = %d\n", len));
    DEBUGMSGF(("message (hex) : [%3.*a] [%3.*a]\n", PACKET_SIZE, lb, 
	       len, buf));
    DEBUGMSGF(("message (char): [%3.*b] [%3.*b]\n", PACKET_SIZE, lb, 
	       len, buf));

    if (write(1, lb, PACKET_SIZE) != PACKET_SIZE) { /* XXX */
	fprintf(stderr, "write_ctrl: Bad write \n");
	return -1;
    }
    if (write(1, buf, len) != len) { /* XXX */
	fprintf(stderr, "write_ctrl: Bad write \n");
	return -1;
    }
    return len;
}


/* 
 * Local functions
 *
 */

static int inc_rbuf(int size)
{
    unsigned char *nbuf;

    if (rbuf_malloced >= size)
	return 0;
    if (rbuf != NULL)		
	nbuf = esock_realloc(rbuf, size);
    else
	nbuf = esock_malloc(size);
    if(nbuf != NULL) {
	rbuf = nbuf;
	rbuf_malloced = size;
	return 0;
    }
    return -1;
}

static void free_rbuf(void) 
{
    if (rbuf != NULL) {
	esock_free(rbuf);
	rbuf = NULL;
	rbuf_malloced = 0;
    }
}

/* Fill buffer, return buffer length, 0 for EOF, < 0 for error. */

static int read_fill(unsigned char *buf, int len)
{
    int i, got = 0;
  
    do {
	if ((i = sock_read(local_read_fd, buf+got, len-got)) <= 0)
	    return i;
	got += i;
    } while (got < len);
    return len;
}


#ifdef __WIN32__

/* 
 * This routine creates a local thread, which reads from standard input
 * and writes to a socket. 
 */

static int create_local_thread(void)
{
    struct sockaddr_in iserv_addr;
    SOCKET tmpsock;
    int length;
    unsigned threadaddr;

    local_buf = esock_malloc(LOCBUFSIZE);
    if ((tmpsock = socket(AF_INET, SOCK_STREAM, 0)) == INVALID_SOCKET) {
 	fprintf(stderr, "create_local_thread could not create socket.\n");
	return -1;
    }
    memset(&iserv_addr, 0, sizeof(iserv_addr));
    iserv_addr.sin_family = AF_INET;
    iserv_addr.sin_addr.s_addr = inet_addr(LOCALHOSTADDR);
    iserv_addr.sin_port = htons(0); /* Have any port */

    if (bind(tmpsock, (struct sockaddr *) &iserv_addr, 
	     sizeof(iserv_addr)) < 0) {
	fprintf(stderr, "create_local_thread could not bind.\n");
	closesocket(tmpsock);
	return -1;
    }
    listen(tmpsock, 1);
    length = sizeof(iserv_addr);
    if (getsockname(tmpsock, (struct sockaddr *) &iserv_addr, &length) < 0) {
	fprintf(stderr, "create_local_thread could not getsockname.\n");
	closesocket(tmpsock);
	return -1;
    }
    local_portno = ntohs(iserv_addr.sin_port);

    if (_beginthreadex(NULL, 0, local_thread, NULL, 0, &threadaddr) == 0) {
	fprintf(stderr, "create_local_thread could not _beginthreadex().\n");
	closesocket(tmpsock);
	return -1;
    }
    local_read_fd = accept(tmpsock, (struct sockaddr *) NULL, (int *) NULL);
    if (local_read_fd == INVALID_FD) {
	fprintf(stderr, "create_local_thread could not accept.\n");
	closesocket(tmpsock);
	return -1;
    }
    closesocket(tmpsock);
    return 0;
}

static DWORD WINAPI local_thread(LPVOID lpvParam)
{
    SOCKET sock;
    struct hostent *host;
    char hostname[64];
    struct sockaddr_in iserv_addr;
    unsigned long addr;
    int len;
    HANDLE thread;

    sock = socket(AF_INET, SOCK_STREAM, 0);
    memset(&iserv_addr, 0, sizeof(struct sockaddr_in));
    iserv_addr.sin_family = AF_INET;
    iserv_addr.sin_addr.s_addr = inet_addr(LOCALHOSTADDR);
    iserv_addr.sin_port = htons(local_portno);
    if(connect(sock, (struct sockaddr*)&iserv_addr, sizeof iserv_addr) == 
       SOCKET_ERROR) {
	fprintf(stderr, "local_thread thread could not connect\n");
	closesocket(sock);
	return 0;
    }
    setsockopt(sock, IPPROTO_TCP, TCP_NODELAY, &one, sizeof(one));

    /* read from 0 and write to sock */
    while (1) {
	if ((len = read(0, local_buf, LOCBUFSIZE)) <= 0) {
	    closesocket(sock);
	    close(0);
	    return 0;
	}
	if (send(sock, local_buf, len, 0) != len ) {
	    closesocket(sock);
	    close(0);
	    return 0;
	}
    }
    return 0;
}

/* Signal handler */

static BOOL WINAPI signal_handler(DWORD ctrl)
{
    switch (ctrl) {
    case CTRL_C_EVENT:
    case CTRL_BREAK_EVENT:
	break;
    case CTRL_LOGOFF_EVENT:
	if (!getenv("ERLSRV_SERVICE_NAME"))
	    return FALSE;
	break;
    default:
	exit(1);
    }
    return TRUE;
}

#endif
