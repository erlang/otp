/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2012-2016. All Rights Reserved.
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
#ifdef __WIN32__
#include <windows.h>
#else
#include <unistd.h>
#endif
#include <stdio.h>
#include <string.h>

static void stop(ErlDrvData drv_data);
static ErlDrvData start(ErlDrvPort port,
			char *command);
static void output(ErlDrvData drv_data,
		   char *buf, ErlDrvSizeT len);
static ErlDrvSSizeT control(ErlDrvData drv_data,
			    unsigned int command,
			    char *buf, ErlDrvSizeT len,
			    char **rbuf, ErlDrvSizeT rlen);

static ErlDrvEntry consume_timeslice_drv_entry = { 
    NULL /* init */,
    start,
    stop,
    output,
    NULL /* ready_input */,
    NULL /* ready_output */,
    "consume_timeslice_drv",
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
    NULL /* handle_monitor */
};

typedef struct {
    ErlDrvPort port;
    ErlDrvTermData tport;
    ErlDrvTermData cmd_msg[6];
    int consume_timeslice;
} consume_timeslice_data_t;


DRIVER_INIT(consume_timeslice_drv)
{
    return &consume_timeslice_drv_entry;
}

static void stop(ErlDrvData drv_data)
{
    driver_free((void *) drv_data);
}

static ErlDrvData start(ErlDrvPort port,
			char *command)
{
    consume_timeslice_data_t *ctsd;

    ctsd = driver_alloc(sizeof(consume_timeslice_data_t));
    if (!ctsd)
	return ERL_DRV_ERROR_GENERAL;

    ctsd->port = port;
    ctsd->tport = driver_mk_port(port);
    ctsd->consume_timeslice = 0;

    ctsd->cmd_msg[0] = ERL_DRV_PORT;
    ctsd->cmd_msg[1] = ctsd->tport;
    ctsd->cmd_msg[2] = ERL_DRV_ATOM;
    ctsd->cmd_msg[3] = driver_mk_atom("command");
    ctsd->cmd_msg[4] = ERL_DRV_TUPLE;
    ctsd->cmd_msg[5] = (ErlDrvTermData) 2;

    return (ErlDrvData) ctsd;
}

static void output(ErlDrvData drv_data,
		   char *buf, ErlDrvSizeT len)
{
    consume_timeslice_data_t *ctsd = (consume_timeslice_data_t *) drv_data;
    int res;

    if (ctsd->consume_timeslice) {
	int res = erl_drv_consume_timeslice(ctsd->port, 50);
	if (res < 0) {
	    driver_failure_atom(ctsd->port, "erl_drv_consume_timeslice() failed");
	    return;
	}
    }

    res = erl_drv_output_term(ctsd->tport,
			      ctsd->cmd_msg,
			      sizeof(ctsd->cmd_msg)/sizeof(ErlDrvTermData));
    if (res <= 0) {
	driver_failure_atom(ctsd->port, "erl_drv_output_term() failed");
	return;
    }
}
static ErlDrvSSizeT control(ErlDrvData drv_data,
			    unsigned int command,
			    char *buf, ErlDrvSizeT len,
			    char **rbuf, ErlDrvSizeT rlen)
{
    consume_timeslice_data_t *ctsd = (consume_timeslice_data_t *) drv_data;
    int res;
    char *res_str;
    ErlDrvSSizeT res_len;

    switch (command) {
    case 'E':
	ctsd->consume_timeslice = 1;
	res_str = "enabled";
	break;
    case 'D':
	ctsd->consume_timeslice = 0;
	res_str = "disabled";
	break;
    case 'S':
#ifdef __WIN32__
	Sleep((DWORD) 1000);
#else
	sleep(1);
#endif
	res_str = "sleeped";
	break;
    default:
	res_str = "what?";
	break;
    }

    res_len = strlen(res_str);
    if (res_len > rlen) {
	char *abuf = driver_alloc(sizeof(char)*res_len);
	if (!abuf) {
	    driver_failure_atom(ctsd->port, "driver_alloc() failed");
	    return 0;
	}
	*rbuf = abuf;
    }

    memcpy((void *) *rbuf, (void *) res_str, res_len);

    return res_len;
}
