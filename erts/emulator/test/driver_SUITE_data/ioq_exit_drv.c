/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2007-2010. All Rights Reserved.
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

/*
 * Author: Rickard Green
 *
 * Description: Tests that port I/O queues can be flushed via:
 *		- ready_input(),
 *		- ready_output(),
 *		- timeout(),
 *		- driver_async() -> read_async(), and
 *		- event()
 */

#ifndef UNIX
#if !defined(__WIN32__) && !defined(VXWORKS)
#define UNIX 1
#endif
#endif

#if defined(DEBUG) || 0
#  define PRINTF(X) printf X
#else
#  define PRINTF(X)
#endif

#if defined(UNIX)
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#ifdef HAVE_POLL_H
#  include <poll.h>
#endif
#elif defined(__WIN32__)
#include <windows.h>
#endif

#include <errno.h>

#include "erl_driver.h"

typedef enum {
    IOQ_EXIT_INVALID = 0,
    IOQ_EXIT_READY_INPUT = 1,
    IOQ_EXIT_READY_OUTPUT = 2,
    IOQ_EXIT_TIMEOUT = 3,
    IOQ_EXIT_READY_ASYNC = 4,
    IOQ_EXIT_EVENT = 5,
    IOQ_EXIT_READY_INPUT_ASYNC = 6,
    IOQ_EXIT_READY_OUTPUT_ASYNC = 7,
    IOQ_EXIT_TIMEOUT_ASYNC = 8,
    IOQ_EXIT_EVENT_ASYNC = 9
} IOQExitTest;

typedef struct {
    ErlDrvPort port;
    IOQExitTest test;
    int ifd;
    int ofd;
    int outstanding_async_task;
    long async_task;
    ErlDrvPDL pdl;
#ifdef HAVE_POLL_H
    struct erl_drv_event_data event_data;
#endif
} IOQExitDrvData;

#define EV2FD(EV) ((int) ((long) (EV)))
#define FD2EV(FD) ((ErlDrvEvent) ((long) (FD)))

static ErlDrvData start(ErlDrvPort port, char *command);
static void stop(ErlDrvData drv_data);
static void ready_input(ErlDrvData drv_data, ErlDrvEvent event); 
static void ready_output(ErlDrvData drv_data, ErlDrvEvent event);  
static int control(ErlDrvData, unsigned int, char *, int, char **, int);
static void timeout(ErlDrvData drv_data);
static void ready_async(ErlDrvData drv_data, ErlDrvThreadData thread_data);
static void flush(ErlDrvData drv_data);
static void event(ErlDrvData drv_data, ErlDrvEvent event,
		  ErlDrvEventData event_data);
static void async_invoke(void*);
static void do_driver_async(IOQExitDrvData *);

static ErlDrvEntry ioq_exit_drv_entry = { 
    NULL /* init */,
    start,
    stop,
    NULL /* output */,
    ready_input,
    ready_output,
    "ioq_exit_drv",
    NULL /* finish */,
    NULL /* handle */,
    control,
    timeout,
    NULL /* outputv */,
    ready_async,
    flush,
    NULL /* call */,
    event,
    ERL_DRV_EXTENDED_MARKER,
    ERL_DRV_EXTENDED_MAJOR_VERSION,
    ERL_DRV_EXTENDED_MINOR_VERSION,
    ERL_DRV_FLAG_USE_PORT_LOCKING,
    NULL /* handle2 */,
    NULL /* process_exit */
};

DRIVER_INIT(ioq_exit_drv)
{
    return &ioq_exit_drv_entry;
}

static ErlDrvData
start(ErlDrvPort port, char *command)
{
    IOQExitDrvData *ddp = driver_alloc(sizeof(IOQExitDrvData));
    PRINTF(("%p = start(%ld, %s) called\r\n", ddp, (long) port, command));
    if (!ddp) {
	errno = ENOMEM;
	return ERL_DRV_ERROR_ERRNO;
    }

    ddp->port = port;
    ddp->test = IOQ_EXIT_INVALID;
    ddp->ifd = -1;
    ddp->ofd = -1;
    ddp->outstanding_async_task = 0;
    ddp->async_task = -1;
    ddp->pdl = driver_pdl_create(port);
#ifdef HAVE_POLL_H
    ddp->event_data.events = (short) 0;
    ddp->event_data.revents = (short) 0;
#endif

    return (ErlDrvData) ddp;
}

static int control(ErlDrvData drv_data,
		   unsigned int command,
		   char *buf, int len,
		   char **rbuf, int rlen)
{
    IOQExitDrvData *ddp = (IOQExitDrvData *) drv_data;
    char *res_str = "nyiftos";

    PRINTF(("control(%p, %d, ...) called\r\n", drv_data, command));

    switch (command) {
    case IOQ_EXIT_READY_INPUT:
    case IOQ_EXIT_READY_INPUT_ASYNC:
#ifdef UNIX
	ddp->ifd = open("/dev/zero", O_RDONLY);
	if (ddp->ifd < 0) {
	    driver_failure_posix(ddp->port, errno);
	    return 0;
	}
	break;
#else
	goto done;
#endif
    case IOQ_EXIT_READY_OUTPUT:
    case IOQ_EXIT_READY_OUTPUT_ASYNC:
#ifdef UNIX
	ddp->ofd = open("/dev/null", O_WRONLY);
	if (ddp->ofd < 0) {
	    driver_failure_posix(ddp->port, errno);
	    return 0;
	}
	break;
#else
	goto done;
#endif
    case IOQ_EXIT_EVENT:
    case IOQ_EXIT_EVENT_ASYNC:
#ifdef UNIX
#ifdef HAVE_POLL_H
	ddp->ofd = open("/dev/null", O_WRONLY);
	if (ddp->ofd < 0) {
	    driver_failure_posix(ddp->port, errno);
	    return 0;
	}
	else if (driver_event(ddp->port, FD2EV(ddp->ofd), NULL) != 0) {
	    res_str = "skip: driver_event() not supported";
	    goto done;
	}
#else
	res_str = "skip: No poll.h found which is needed for this test";
	goto done;
#endif
	break;
#else /* UNIX */
	goto done;
#endif
    case IOQ_EXIT_TIMEOUT:
    case IOQ_EXIT_TIMEOUT_ASYNC:
	break;
    case IOQ_EXIT_READY_ASYNC:
	break;
    default:
	res_str = "error: command not supported";
	goto done;
    }
    driver_pdl_lock(ddp->pdl);
    driver_enq(ddp->port, "!", 1);
    driver_pdl_unlock(ddp->pdl);
    ddp->test = (IOQExitTest) command;
    res_str = "ok";

 done: {
	int res_len = strlen(res_str);
	if (res_len > rlen) {
	    char *abuf = driver_alloc(sizeof(char)*res_len);
	    if (!abuf)
		return 0;
	    *rbuf = abuf;
	}

	memcpy((void *) *rbuf, (void *) res_str, res_len);

	return res_len;
    }
}

static void stop(ErlDrvData drv_data)
{
    IOQExitDrvData *ddp = (IOQExitDrvData *) drv_data;

    PRINTF(("stop(%p) called\r\n", drv_data));

    if (ddp) {
	switch (ddp->test) {
#ifdef UNIX
	case IOQ_EXIT_READY_INPUT:
	case IOQ_EXIT_READY_INPUT_ASYNC:
	    if (ddp->ifd >= 0) {
		driver_select(ddp->port, FD2EV(ddp->ifd), DO_READ, 0);
		close(ddp->ifd);
	    }
	    break;
	case IOQ_EXIT_READY_OUTPUT:
	case IOQ_EXIT_READY_OUTPUT_ASYNC:
	    if (ddp->ofd >= 0) {
		driver_select(ddp->port, FD2EV(ddp->ofd), DO_WRITE, 0);
		close(ddp->ofd);
	    }
	    break;
	case IOQ_EXIT_EVENT:
	case IOQ_EXIT_EVENT_ASYNC:
	    if (ddp->ofd >= 0) {
		driver_event(ddp->port, FD2EV(ddp->ofd), NULL);
		close(ddp->ofd);
	    }
	    break;
#endif
	case IOQ_EXIT_TIMEOUT:
	case IOQ_EXIT_TIMEOUT_ASYNC:
	    driver_cancel_timer(ddp->port);
	    break;
	case IOQ_EXIT_READY_ASYNC:
	    if (ddp->outstanding_async_task)
		driver_async_cancel(ddp->async_task);
	    break;
	default:
	    break;
	}
	driver_free(ddp);
    }
}


static void flush(ErlDrvData drv_data)
{
    IOQExitDrvData *ddp = (IOQExitDrvData *) drv_data;

    PRINTF(("flush(%p) called\r\n", drv_data));

    switch (ddp->test) {
#ifdef UNIX
    case IOQ_EXIT_READY_INPUT:
    case IOQ_EXIT_READY_INPUT_ASYNC:
	driver_select(ddp->port, FD2EV(ddp->ifd), DO_READ, 1);
	break;
    case IOQ_EXIT_READY_OUTPUT:
    case IOQ_EXIT_READY_OUTPUT_ASYNC:
	driver_select(ddp->port, FD2EV(ddp->ofd), DO_WRITE, 1);
	break;
    case IOQ_EXIT_EVENT:
    case IOQ_EXIT_EVENT_ASYNC:
#ifdef HAVE_POLL_H
	ddp->event_data.events |= POLLOUT;
	driver_event(ddp->port, FD2EV(ddp->ofd), &ddp->event_data);
#endif
	break;
#endif
    case IOQ_EXIT_TIMEOUT:
    case IOQ_EXIT_TIMEOUT_ASYNC:
	driver_set_timer(ddp->port, 0);
	break;
    case IOQ_EXIT_READY_ASYNC:
	do_driver_async(ddp);
	break;
    default:
	break;
    }
}

static void ready_input(ErlDrvData drv_data, ErlDrvEvent event)
{
    IOQExitDrvData *ddp = (IOQExitDrvData *) drv_data;

    PRINTF(("ready_input(%p, %d) called\r\n", drv_data, EV2FD(event)));

#ifdef UNIX
    if (ddp->ifd == EV2FD(event)) {
	driver_select(ddp->port, FD2EV(ddp->ifd), DO_READ, 0);
	close(ddp->ifd);
	ddp->ifd = -1;
	if (ddp->test == IOQ_EXIT_READY_INPUT_ASYNC)
	    do_driver_async(ddp);
	else {
	    driver_pdl_lock(ddp->pdl);
	    driver_deq(ddp->port, 1);
	    driver_pdl_unlock(ddp->pdl);
	}
    }
#endif
}

static void ready_output(ErlDrvData drv_data, ErlDrvEvent event)
{
    IOQExitDrvData *ddp = (IOQExitDrvData *) drv_data;

    PRINTF(("ready_output(%p, %d) called\r\n", drv_data, EV2FD(event)));

#ifdef UNIX
    if (ddp->ofd == EV2FD(event)) {
	driver_select(ddp->port, FD2EV(ddp->ofd), DO_WRITE, 0);
	close(ddp->ofd);
	ddp->ofd = -1;
	if (ddp->test == IOQ_EXIT_READY_OUTPUT_ASYNC)
	    do_driver_async(ddp);
	else {
	    driver_pdl_lock(ddp->pdl);
	    driver_deq(ddp->port, 1);
	    driver_pdl_unlock(ddp->pdl);
	}
    }
#endif
}

static void timeout(ErlDrvData drv_data)
{
    IOQExitDrvData *ddp = (IOQExitDrvData *) drv_data;

    PRINTF(("timeout(%p) called\r\n", drv_data));

    if (ddp->test == IOQ_EXIT_TIMEOUT_ASYNC)
	do_driver_async(ddp);
    else {
	driver_pdl_lock(ddp->pdl);
	driver_deq(ddp->port, 1);
	driver_pdl_unlock(ddp->pdl);
    }
}

static void ready_async(ErlDrvData drv_data, ErlDrvThreadData thread_data)
{
    IOQExitDrvData *ddp = (IOQExitDrvData *) drv_data;

    PRINTF(("ready_async(%p, %p) called\r\n", drv_data, thread_data));

    if (drv_data == (ErlDrvData) thread_data) {
	driver_pdl_lock(ddp->pdl);
	driver_deq(ddp->port, 1);
	driver_pdl_unlock(ddp->pdl);
	ddp->outstanding_async_task = 0;
    }
}

static void event(ErlDrvData drv_data,
		  ErlDrvEvent event,
		  ErlDrvEventData event_data)
{
    IOQExitDrvData *ddp = (IOQExitDrvData *) drv_data;

    PRINTF(("event(%p, %d, %p) called\r\n", drv_data, EV2FD(event), event_data));

#if defined(UNIX) && defined(HAVE_POLL_H)
    if (ddp->ofd == EV2FD(event)) {
	driver_event(ddp->port, FD2EV(ddp->ofd), NULL);
	close(ddp->ofd);
	ddp->ofd = -1;
	if (ddp->test == IOQ_EXIT_EVENT_ASYNC)
	    do_driver_async(ddp);
	else {
	    driver_pdl_lock(ddp->pdl);
	    driver_deq(ddp->port, 1);
	    driver_pdl_unlock(ddp->pdl);
	}
    }
#endif
}

static void async_invoke(void *arg)
{
    PRINTF(("async_invoke(%p) called\r\n", arg));
}

static void do_driver_async(IOQExitDrvData *ddp)
{
    ErlDrvSysInfo si;
    long task;
    ddp->outstanding_async_task = 1;
    task = driver_async(ddp->port, NULL, async_invoke, ddp, NULL);
    /* If no async threads, ddp has been deallocated now */
    driver_system_info(&si, sizeof(ErlDrvSysInfo));
    if (si.async_threads)
	ddp->async_task = task;
}


