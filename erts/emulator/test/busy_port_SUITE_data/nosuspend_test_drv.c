/*
 * %CopyrightBegin%
 *
 * SPDX-License-Identifier: Apache-2.0
 *
 * Copyright Ericsson AB 2026. All Rights Reserved.
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

#include <errno.h>
#include <stdbool.h>
#include "erl_driver.h"

static void stop(ErlDrvData drv_data);
static ErlDrvData start(ErlDrvPort port, char *command);
static void output(ErlDrvData drv_data, char *buf, ErlDrvSizeT len);
static ErlDrvSSizeT control(ErlDrvData drv_data, unsigned int command, char *buf,
                            ErlDrvSizeT len, char **rbuf, ErlDrvSizeT rlen);

static ErlDrvEntry nosuspend_test_drv_entry = { 
    NULL /* init */,
    start,
    stop,
    output,
    NULL /* ready_input */,
    NULL /* ready_output */,
    "nosuspend_test_drv",
    NULL /* finish */,
    NULL /* handle */,
    control,
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
    NULL /* handle2 */,
    NULL /* handle_monitor */,
    NULL /* stop_select */
};

typedef struct {
    ErlDrvPort port;
    bool is_busy;
} PortState;

DRIVER_INIT(nosuspend_test_drv)
{
    return &nosuspend_test_drv_entry;
}

static void stop(ErlDrvData drv_data)
{
    driver_free(drv_data);
}

static ErlDrvData start(ErlDrvPort port, char *command)
{
    PortState *state = driver_alloc(sizeof(PortState));
    if (!state)
        return ERL_DRV_ERROR_GENERAL;
    state->port = port;
    state->is_busy = false;
    return (ErlDrvData) state;
}

static void output(ErlDrvData drv_data, char *buf, ErlDrvSizeT len)
{
    PortState *state = (PortState *) drv_data;
    if (state->is_busy) {
        driver_failure_atom(state->port, "got_data_when_busy");
    }
}

static ErlDrvSSizeT control(ErlDrvData drv_data, unsigned int command, char *buf,
                            ErlDrvSizeT len, char **rbuf, ErlDrvSizeT rlen)
{
    PortState *state = (PortState *) drv_data;
    switch (command) {
    case 'B': /* busy */
        set_busy_port(state->port, !0);
        state->is_busy = true;
        break;
    case 'N': /* not busy */
        set_busy_port(state->port, 0);
        state->is_busy = false;
        break;
    default:
        driver_failure_posix(state->port, EINVAL);
        break;
    }
    return 0;
}
