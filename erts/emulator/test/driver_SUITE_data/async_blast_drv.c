/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2011-2016. All Rights Reserved.
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

#define NO_ASYNC_JOBS 10000

static void stop(ErlDrvData drv_data);
static ErlDrvData start(ErlDrvPort port,
			char *command);
static void output(ErlDrvData drv_data,
		   char *buf, ErlDrvSizeT len);
static void ready_async(ErlDrvData drv_data,
			ErlDrvThreadData thread_data);

static ErlDrvEntry async_blast_drv_entry = { 
    NULL /* init */,
    start,
    stop,
    output,
    NULL /* ready_input */,
    NULL /* ready_output */,
    "async_blast_drv",
    NULL /* finish */,
    NULL /* handle */,
    NULL /* control */,
    NULL /* timeout */,
    NULL /* outputv */,
    ready_async,
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
    ErlDrvTermData port_id;
    ErlDrvTermData caller;    
    int counter;
} async_blast_data_t;


DRIVER_INIT(async_blast_drv)
{
    return &async_blast_drv_entry;
}

static void stop(ErlDrvData drv_data)
{
    driver_free((void *) drv_data);
}

static ErlDrvData start(ErlDrvPort port,
			char *command)
{
    async_blast_data_t *abd;

    abd = driver_alloc(sizeof(async_blast_data_t));
    if (!abd)
	return ERL_DRV_ERROR_GENERAL;

    abd->port = port;
    abd->port_id = driver_mk_port(port);
    abd->counter = 0;
    return (ErlDrvData) abd;
}

static void async_invoke(void *data)
{

}
#include <stdio.h>

static void ready_async(ErlDrvData drv_data,
			ErlDrvThreadData thread_data)
{
    async_blast_data_t *abd = (async_blast_data_t *) drv_data;
    if (--abd->counter == 0) {
	ErlDrvTermData spec[] = {
	    ERL_DRV_PORT, abd->port_id,
	    ERL_DRV_ATOM, driver_mk_atom("done"),
	    ERL_DRV_TUPLE, 2
	};
	erl_drv_send_term(abd->port_id, abd->caller,
			  spec, sizeof(spec)/sizeof(spec[0]));
    }
}

static void output(ErlDrvData drv_data,
		   char *buf, ErlDrvSizeT len)
{
    async_blast_data_t *abd = (async_blast_data_t *) drv_data;
    if (abd->counter == 0) {
	int i;
	abd->caller = driver_caller(abd->port);
	abd->counter = NO_ASYNC_JOBS;
	for (i = 0; i < NO_ASYNC_JOBS; i++) {
	    if (0 > driver_async(abd->port, NULL, async_invoke, NULL, NULL)) {
		driver_failure_atom(abd->port, "driver_async_failed");
		break;
	    }
	}
    }
}
