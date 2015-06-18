/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1996-2013. All Rights Reserved.
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
