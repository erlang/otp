/* ``The contents of this file are subject to the Erlang Public License,
 * Version 1.1, (the "License"); you may not use this file except in
 * compliance with the License. You should have received a copy of the
 * Erlang Public License along with this software. If not, it can be
 * retrieved via the world wide web at http://www.erlang.org/.
 * 
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
 * the License for the specific language governing rights and limitations
 * under the License.
 * 
 * The Initial Developer of the Original Code is Ericsson AB. Portions
 * created by Ericsson are Copyright 2008, Ericsson Utvecklings AB. All
 * Rights Reserved.''
 * 
 *     $Id$
 */

#ifndef UNIX
#if !defined(__WIN32__)
#define UNIX 1
#endif
#endif

#ifdef UNIX
#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#ifdef HAVE_POLL_H
#  include <poll.h>
#endif
#endif /* UNIX */

#include "erl_driver.h"

typedef struct {
    int ofd;
    int ifd;
    int efd;
#ifdef HAVE_POLL_H
    struct erl_drv_event_data edata;
#endif
} mcd_data_t;

static ErlDrvData start(ErlDrvPort port, char *command);
static void stop(ErlDrvData data);

static ErlDrvEntry missing_callback_drv_entry = { 
    NULL /* init */,
    start,
    stop,
    NULL /* output */,
    NULL /* ready_input */,
    NULL /* ready_output */,
    "missing_callback_drv",
    NULL /* finish */,
    NULL /* handle */,
    NULL /* control */,
    NULL /* timeout */,
    NULL /* outputv */,
    NULL /* ready_async */,
    NULL /* flush */,
    NULL /* call */,
    NULL /* event */,
    ERL_DRV_EXTENDED_MARKER,
    ERL_DRV_EXTENDED_MAJOR_VERSION,
    ERL_DRV_EXTENDED_MINOR_VERSION,
    ERL_DRV_FLAG_USE_PORT_LOCKING,
    NULL, /* handle2 */
    NULL /* process_exit */
};

DRIVER_INIT(missing_callback_drv)
{
    return &missing_callback_drv_entry;
}

static ErlDrvData
start(ErlDrvPort port, char *command)
{
    mcd_data_t *mcd = driver_alloc(sizeof(mcd_data_t));

    if (!mcd)
	goto error;

    mcd->ofd = -1;
    mcd->ifd = -1;
    mcd->efd = -1;

#ifdef UNIX

    mcd->ofd = open("/dev/null", O_WRONLY);
    if (mcd->ofd < 0)
	goto error;
    if (driver_select(port, (ErlDrvEvent) (long) mcd->ofd, DO_WRITE, 1) != 0)
	goto error;

    mcd->ifd = open("/dev/zero", O_RDONLY);
    if (mcd->ifd < 0)
	goto error;
    if (driver_select(port, (ErlDrvEvent) (long) mcd->ifd, DO_READ, 1) != 0)
	goto error;

#ifdef HAVE_POLL_H
    mcd->efd = open("/dev/null", O_WRONLY);
    if (mcd->efd < 0)
	goto error;
    mcd->edata.events = POLLOUT;
    mcd->edata.revents = 0;
    driver_event(port, (ErlDrvEvent) (long) mcd->efd, &mcd->edata);
#endif
#endif

    driver_set_timer(port, 0);

    return (ErlDrvData) mcd;

 error:
    stop((ErlDrvData) mcd);
    return ERL_DRV_ERROR_GENERAL;
}

static void
stop(ErlDrvData data)
{
    mcd_data_t *mcd = (mcd_data_t *) data;
    if (mcd) {
#ifdef UNIX
	if (mcd->ofd >= 0)
	    close(mcd->ofd);
	if (mcd->ifd >= 0)
	    close(mcd->ifd);
#ifdef HAVE_POLL_H
	if (mcd->efd >= 0)
	    close(mcd->efd);
#endif
#endif
	driver_free(mcd);
    }
}
