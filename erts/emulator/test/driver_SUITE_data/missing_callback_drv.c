/* ``Licensed under the Apache License, Version 2.0 (the "License");
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
#endif
	driver_free(mcd);
    }
}
