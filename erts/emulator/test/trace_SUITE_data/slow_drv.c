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
 * The Initial Developer of the Original Code is Ericsson Utvecklings AB.
 * Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
 * AB. All Rights Reserved.''
 * 
 *     $Id$
 */

#ifndef UNIX
#if !defined(__WIN32__) && !defined(VXWORKS)
#define UNIX 1
#endif
#endif

/* We actually only want to run this on 
   Unix machines which have the usleep call */
#if defined(UNIX) && !defined(HAVE_USLEEP)
#undef UNIX
#endif

#ifdef UNIX
#include <errno.h>
#include <stdio.h>
#include <stdlib.h> /* rand */
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

static int slow_drv_init(void);
static void slow_drv_finish(void);
static ErlDrvData slow_drv_start(ErlDrvPort, char *);
static void slow_drv_stop(ErlDrvData);
static void slow_drv_ready_input(ErlDrvData, ErlDrvEvent);
static ErlDrvSSizeT slow_drv_control(ErlDrvData, unsigned int,
				      char *, ErlDrvSizeT, char **, ErlDrvSizeT);

static ErlDrvEntry slow_drv_entry = { 
    slow_drv_init,
    slow_drv_start,
    slow_drv_stop,
    NULL, /* output */
    slow_drv_ready_input,
    NULL, /* ready_output */
    "slow_drv",
    slow_drv_finish,
    NULL, /* handle */
    slow_drv_control,
    NULL, /* timeout */
    NULL, /* outputv */
    NULL, /* ready_async */
    NULL, /* flush */
    NULL, /* call */
    NULL, /* event */

    ERL_DRV_EXTENDED_MARKER,
    ERL_DRV_EXTENDED_MAJOR_VERSION,
    ERL_DRV_EXTENDED_MINOR_VERSION,
    ERL_DRV_FLAG_USE_PORT_LOCKING,
    NULL,/* void *handle2 */
    NULL,/* process_exit */
    NULL /* stop select */
};
typedef struct {
    ErlDrvPort port;
    ErlDrvTermData id;
    int test;
    int s[2];
} SlowDrvData;

/* -------------------------------------------------------------------------
** Entry functions
**/

DRIVER_INIT(slow_drv)
{
    return &slow_drv_entry;
}


static int
slow_drv_init(void)
{
    return 0;
}

static void
slow_drv_finish(void)
{
}


static ErlDrvData
slow_drv_start(ErlDrvPort port, char *command)
{
#ifndef UNIX
    return NULL;
#else
    SlowDrvData *sddp = driver_alloc(sizeof(SlowDrvData));
    if (!sddp) {
	errno = ENOMEM;
	return ERL_DRV_ERROR_ERRNO;
    }
    sddp->port = port;
    sddp->id = driver_mk_port(port);
    sddp->test = 0;
    sddp->s[0] = sddp->s[1] = -1;
    return (ErlDrvData) sddp;
#endif
}

static void
slow_drv_stop(ErlDrvData drv_data) {
#ifdef UNIX
    SlowDrvData *sddp = (SlowDrvData *) drv_data;

    if (sddp->test) {
	driver_select(sddp->port, (ErlDrvEvent) (ErlDrvSInt) sddp->s[0], DO_READ, 0);
	close(sddp->s[0]);
	close(sddp->s[1]);
    }

    driver_free((void *) sddp);

#endif
}

static ErlDrvSSizeT
slow_drv_control(ErlDrvData drv_data,
		 unsigned int command,
		 char *buf, ErlDrvSizeT len,
		 char **rbuf, ErlDrvSizeT rlen)
{
    SlowDrvData *sddp = (SlowDrvData *) drv_data;
    char *res_str = "ok";
    int res_len;
    switch (command) {
    case 0:
	/* just be slow */
	usleep(222000);
	break;
    case 1:
	/* create pipes and select on input */
	if (sddp->test) {
	    res_str = "no";
	    break;
	}
	sddp->test = 1;
	pipe(sddp->s);
	driver_select(sddp->port, (ErlDrvEvent) (ErlDrvSInt) sddp->s[0], DO_READ, 1);
	break;
    case 2:
	if (!sddp->test) {
	    res_str = "no";
	    break;
	}
	write(sddp->s[1],"boo",3);
	break;
    }

    res_len = strlen(res_str);
    if (res_len > rlen) {
	char *abuf = driver_alloc(sizeof(char)*res_len);
	if (!abuf)
	    return 0;
	*rbuf = abuf;
    }

    memcpy((void *) *rbuf, (void *) res_str, res_len);

    return res_len;
}

static void
slow_drv_ready_input(ErlDrvData drv_data, ErlDrvEvent event)
{
#ifdef UNIX
    SlowDrvData *sddp = (SlowDrvData *) drv_data;
    int fd = (int) (ErlDrvSInt) event;
    if (sddp->test) {
	char buff[3];
	usleep(212000);
	read(sddp->s[0],buff,3);
	driver_output(sddp->port, "TheEnd", 6);
    }

#endif
}
