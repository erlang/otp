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

#include "erl_driver.h"
#include <string.h>

static void stop(ErlDrvData drv_data);
static ErlDrvData start(ErlDrvPort port,
			char *command);
static void output(ErlDrvData drv_data,
		   char *buf, ErlDrvSizeT len);

static ErlDrvEntry send_term_local_drv_entry = { 
    NULL /* init */,
    start,
    stop,
    output,
    NULL /* ready_input */,
    NULL /* ready_output */,
    "send_term_local_drv",
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
    NULL /* handle2 */,
    NULL /* handle_monitor */
};

DRIVER_INIT(send_term_local_drv)
{
    return &send_term_local_drv_entry;
}

static void stop(ErlDrvData drv_data)
{
}

static ErlDrvData start(ErlDrvPort port,
			char *command)
{

    return (ErlDrvData) port;
}

static void output(ErlDrvData drv_data,
		   char *buf, ErlDrvSizeT len)
{
    ErlDrvPort port = (ErlDrvPort) drv_data;
    ErlDrvTermData term_port = driver_mk_port(port);
    ErlDrvTermData caller = driver_caller(port);
    int res;
    ErlDrvTermData spec[] = {
	ERL_DRV_PORT, term_port,
	ERL_DRV_EXT2TERM, (ErlDrvTermData) buf, len,
	ERL_DRV_TUPLE, 2
    };
    if (0 >= erl_drv_send_term(term_port, caller,
                               spec, sizeof(spec)/sizeof(spec[0]))) {
        char *bad_term = "bad_term_error";
        ErlDrvTermData spec[] = {
            ERL_DRV_PORT, term_port,
            ERL_DRV_STRING, (ErlDrvTermData) bad_term, strlen(bad_term),
            ERL_DRV_TUPLE, 2
        };
        if (0 >= erl_drv_send_term(term_port, caller, spec,
                                   sizeof(spec)/sizeof(spec[0]))) {
            driver_failure_atom(port, "failed_to_bad_term_error");
        }
    }
}
