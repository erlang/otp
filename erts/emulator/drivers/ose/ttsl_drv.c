/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1996-2013. All Rights Reserved.
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
 * Stub tty driver because group/user depend on this.
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "erl_driver.h"

static int ttysl_init(void);
static ErlDrvData ttysl_start(ErlDrvPort, char*);

/* Define the driver table entry. */
struct erl_drv_entry ttsl_driver_entry = {
    ttysl_init,
    ttysl_start,
    NULL,
    NULL,
    NULL,
    NULL,
    "tty_sl",
    NULL,
    NULL,
    NULL,
    NULL, /* timeout */
    NULL, /* outputv */
    NULL, /* ready_async */
    NULL, /* flush */
    NULL, /* call */
    NULL, /* event */
    ERL_DRV_EXTENDED_MARKER,
    ERL_DRV_EXTENDED_MAJOR_VERSION,
    ERL_DRV_EXTENDED_MINOR_VERSION,
    0, /* ERL_DRV_FLAGs */
    NULL,
    NULL, /* process_exit */
    NULL
};


static int ttysl_init(void)
{
    return 0;
}

static ErlDrvData ttysl_start(ErlDrvPort port, char* buf)
{
    return ERL_DRV_ERROR_GENERAL;
}
