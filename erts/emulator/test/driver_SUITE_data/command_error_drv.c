/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2023. All Rights Reserved.
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

#if defined(__WIN32__)
#include <windows.h>
#else
#include <errno.h>
#endif /* UNIX */
#include <stdlib.h>
#include <string.h>
#include "erl_driver.h"

static ErlDrvData start(ErlDrvPort port,
			char *command);
#ifndef USE_OUTPUTV
static void output(ErlDrvData drv_data,
		   char *buf, ErlDrvSizeT len);
#else
static void outputv(ErlDrvData drv_data,
		    ErlIOVec *ev);
#endif
static ErlDrvSSizeT control(ErlDrvData drv_data,
			    unsigned int command, char *buf,
			    ErlDrvSizeT len, char **rbuf, ErlDrvSizeT rlen);

static void chk_cmd_error_eacces(ErlDrvPort port);
static void chk_cmd_error_success_or_eacces(ErlDrvPort port, char *err, int len);

static ErlDrvEntry command_error_drv_entry = { 
    NULL /* init */,
    start,
    NULL /* stop */,
#ifndef USE_OUTPUTV
    output,
#else
    NULL,
#endif
    NULL /* ready_input */,
    NULL /* ready_output */,
    COMMAND_ERROR_DRV_NAME,
    NULL /* finish */,
    NULL /* handle */,
    control,
    NULL /* timeout */,
#ifdef USE_OUTPUTV
    outputv,
#else
    NULL,
#endif
    NULL /* ready_async */,
    NULL /* flush */,
    NULL /* call */,
    NULL /* event */,
    ERL_DRV_EXTENDED_MARKER,
    ERL_DRV_EXTENDED_MAJOR_VERSION,
    ERL_DRV_EXTENDED_MINOR_VERSION,
    ERL_DRV_FLAG_USE_PORT_LOCKING,
    NULL /* handle2 */,
    NULL /* handle_monitor */
};

DRIVER_INIT(command_error_drv)
{
    return &command_error_drv_entry;
}

static ErlDrvData
start(ErlDrvPort port, char *command)
{
    chk_cmd_error_eacces(port);
    return (ErlDrvData) port;
}

#ifndef USE_OUTPUTV

static void
output(ErlDrvData drv_data, char *buf, ErlDrvSizeT len)
{
    ErlDrvPort port = (ErlDrvPort) drv_data;
    chk_cmd_error_success_or_eacces(port, buf, len);
}

#else

static void
outputv(ErlDrvData drv_data, ErlIOVec *ev)
{
    char buf[255];
    int vix, len = 0;
    ErlDrvPort port = (ErlDrvPort) drv_data;
    for (vix = 0; vix < ev->vsize; vix++) {
        SysIOVec *iovp = &ev->iov[vix];
        memcpy(&buf[len], iovp->iov_base, iovp->iov_len);
        len += iovp->iov_len;
    }
    chk_cmd_error_success_or_eacces(port, buf, len);
}
#endif

static ErlDrvSSizeT
control(ErlDrvData drv_data,
	unsigned int command, char *buf, 
	ErlDrvSizeT len, char **rbuf, ErlDrvSizeT rlen)
{
    ErlDrvPort port = (ErlDrvPort) drv_data;
    chk_cmd_error_eacces(port);
    *rbuf = NULL;
    return 0;
}

static void
chk_cmd_error_eacces(ErlDrvPort port)
{
    int res = erl_drv_command_error(port, "fail");
    if (res == 0)
        driver_failure_atom(port, "unexpected_success");
    else if (res != EACCES)
        driver_failure_atom(port, "unexpected_failure");
}

static void
chk_cmd_error_success_or_eacces(ErlDrvPort port, char *err, int len)
{
    int res;
    char error[256];
    if (len > 255)
        len = 255;
    memcpy(error, err, len);
    error[len] = '\0';
    res = erl_drv_command_error(port, error);
    if (res != 0 && res != EACCES)
        driver_failure_atom(port, "unexpected_failure");
}
