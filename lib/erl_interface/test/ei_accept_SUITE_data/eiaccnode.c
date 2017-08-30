/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2001-2016. All Rights Reserved.
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

/* to test multiple threads in ei */

#include <stdlib.h>
#include <stdio.h>

#ifdef __WIN32__
#include <winsock2.h>
#include <windows.h>
#include <process.h>
#else
#ifndef VXWORKS
#include <pthread.h>
#endif
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#endif

#include "ei.h"

#ifdef VXWORKS
#include <vxWorks.h>
#include <sockLib.h>
#include <inetLib.h>
#define MAIN cnode
#else
#define MAIN main
#endif

static int my_listen(int port);

/*
   A small einode.
   To be called from the test case ei_accept_SUITE:multi_thread
   usage: eiaccnode <cookie> <n>

   - start threads 0..n-1
   - in each thread
      - listen on "ei0" .. "ei<n-1>"
      - wait for connection 
      - receive a pid
      - send {i, <pid>} back
      - shutdown gracefully
*/

static const char* cookie, * desthost;
static int port; /* actually base port */

#ifndef SD_SEND
#ifdef SHUTWR
#define SD_SEND SHUT_WR
#else
#define SD_SEND 1
#endif
#endif

#ifndef __WIN32__
#define closesocket(fd) close(fd)
#endif

#ifdef __WIN32__
static DWORD WINAPI
#else
static void*
#endif
    einode_thread(void* num)
{
    int n = (int)num;
    ei_cnode ec;
    char myname[100], destname[100];
    int r, fd, listen;
    ErlConnect conn;
    erlang_msg msg;
/*    FILE* f;*/

    sprintf(myname, "eiacc%d", n);
    printf("thread %d (%s) listening\n", n, myname, destname);
    r = ei_connect_init(&ec, myname, cookie, 0);
    if ((listen = my_listen(port+n)) <= 0) {
	printf("listen err\n");
	exit(7);
    }
    if (ei_publish(&ec, port + n) == -1) {
	printf("ei_publish port %d\n", port+n);
	exit(8);
    }
    fd = ei_accept(&ec, listen, &conn);
    printf("ei_accept %d\n", fd);
    if (fd >= 0) {
	ei_x_buff x, xs;
	int index, version;
	erlang_pid pid;

	ei_x_new(&x);
	for (;;) {
	    int got = ei_xreceive_msg(fd, &msg, &x);
	    if (got == ERL_TICK)
		continue;
	    if (got == ERL_ERROR) {
		printf("receive error %d\n", n);
		return 0;
	    }
	    printf("received %d\n", got);
	    break;
	}
	index = 0;
	if (ei_decode_version(x.buff, &index, &version) != 0) {
	    printf("ei_decode_version %d\n", n);
	    return 0;
	}
	if (ei_decode_pid(x.buff, &index, &pid) != 0) {
	    printf("ei_decode_pid %d\n", n);
	    return 0;
	}
/*	fprintf(f, "got pid from %s \n", pid.node);*/
	ei_x_new_with_version(&xs);
	ei_x_encode_tuple_header(&xs, 2);
	ei_x_encode_long(&xs, n);
	ei_x_encode_pid(&xs, &pid);
	r = ei_send(fd, &pid, xs.buff, xs.index);
/*	fprintf(f, "sent %d bytes %d\n", xs.index, r);*/
	shutdown(fd, SD_SEND);
	closesocket(fd);
	ei_x_free(&x);
	ei_x_free(&xs);
    } else {
	printf("coudn't connect fd %d r %d\n", fd, r);
    }
    printf("done thread %d\n", n);
/*    fclose(f);*/
    return 0;
}

MAIN(int argc, char *argv[])
{
    int i, n, no_threads;
#ifndef VXWORKS
#ifdef __WIN32__
    HANDLE threads[100];
#else
    pthread_t threads[100];
#endif
#endif

    if (argc < 3)
	exit(1);

    cookie = argv[1];
    n = atoi(argv[2]);
    if (n > 100)
	exit(2);
    desthost = argv[3];
    port = atoi(argv[4]);
#ifndef VXWORKS	   
    no_threads = argv[5] != NULL && strcmp(argv[5], "nothreads") == 0;
#else
    no_threads = 1;
#endif
    for (i = 0; i < n; ++i) {
	if (!no_threads) {
#ifndef VXWORKS
#ifdef __WIN32__
	    unsigned tid;
	    threads[i] = (HANDLE)_beginthreadex(NULL, 0, einode_thread,
						(void*)i, 0, &tid);
#else
	    pthread_create(&threads[i], NULL, einode_thread, (void*)i);
#endif
#else
	    ;
#endif
	} else
	    einode_thread((void*)i);
    }

    if (!no_threads)
#ifndef VXWORKS
	for (i = 0; i < n; ++i) {
#ifdef __WIN32__
	    if (WaitForSingleObject(threads[i], INFINITE) != WAIT_OBJECT_0)
#else
	    if (pthread_join(threads[i], NULL) != 0)
#endif
		printf("bad wait thread %d\n", i);
	}
#else
    ;
#endif
    printf("ok\n");
    return 0;
}

static int my_listen(int port)
{
    int listen_fd;
    struct sockaddr_in addr;
    const char *on = "1";
    
    if ((listen_fd = socket(AF_INET, SOCK_STREAM, 0)) < 0)
	return -1;
    
    setsockopt(listen_fd, SOL_SOCKET, SO_REUSEADDR, on, sizeof(on));
    
    memset((void*) &addr, 0, (size_t) sizeof(addr));
    addr.sin_family = AF_INET;
    addr.sin_port = htons(port);
    addr.sin_addr.s_addr = htonl(INADDR_ANY);
    
    if (bind(listen_fd, (struct sockaddr*) &addr, sizeof(addr)) < 0)
	return -1;

    listen(listen_fd, 5);
    return listen_fd;
}

