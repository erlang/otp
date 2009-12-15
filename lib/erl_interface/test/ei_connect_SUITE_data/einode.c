/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2001-2009. All Rights Reserved.
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
#include <sys/socket.h>
#endif

#include "ei.h"

#ifdef VXWORKS
#define MAIN cnode
#else
#define MAIN main
#endif

/*
   A small einode.
   To be called from the test case ei_accept_SUITE:multi_thread
   usage: einode <cookie> <n> <destnode>

   - start threads 0..n-1
   - in each thread
      - connect to destnode
      - send a message ("ei0".."ei<n-1>") to mth0..mth<n-1> on destnode
      - shutdown gracefully
*/

static const char* cookie, * desthost;

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
    int r, fd;

    sprintf(myname, "ei%d", n);
    sprintf(destname, "mth%d", n);
    printf("thread %d (%s %s) connecting\n", n, myname, destname);
    r = ei_connect_init(&ec, myname, cookie, 0);
    fd = ei_connect(&ec, (char*)desthost);
    if (r == 0 && fd >= 0) {
	ei_x_buff x;
	ei_x_new_with_version(&x);
	ei_x_encode_string(&x, myname);
	ei_reg_send(&ec, fd, destname, x.buff, x.index);
	ei_x_free(&x);
	//SleepEx(100);
	shutdown(fd, SD_SEND);
	closesocket(fd);
    } else {
	printf("coudn't connect fd %d r %d\n", fd, r); //	DebugBreak();
    }
    printf("done thread %d\n", n);
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
#ifndef VXWORKS
    no_threads = argv[4] != NULL && strcmp(argv[4], "nothreads") == 0;
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
