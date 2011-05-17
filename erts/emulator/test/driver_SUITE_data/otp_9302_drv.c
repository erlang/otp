/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2011. All Rights Reserved.
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
#ifdef __WIN32__
#include <windows.h>
#endif
#include "erl_driver.h"

static ErlDrvData start(ErlDrvPort port,
			char *command);
static void output(ErlDrvData drv_data,
		   char *buf, int len);
static void ready_async(ErlDrvData drv_data,
			ErlDrvThreadData thread_data);

static ErlDrvEntry otp_9302_drv_entry = { 
    NULL /* init */,
    start,
    NULL /* stop */,
    output,
    NULL /* ready_input */,
    NULL /* ready_output */,
    "otp_9302_drv",
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

DRIVER_INIT(otp_9302_drv)
{
    return &otp_9302_drv_entry;
}

static ErlDrvData start(ErlDrvPort port,
			char *command)
{
    return (ErlDrvData) port;
}

typedef struct {
    ErlDrvPort port;
    ErlDrvTermData receiver;
    int block;
    int cancel;
    int eoj;
} Otp9302AsyncData;

static void async_invoke(void *data)
{
    Otp9302AsyncData *adata = (Otp9302AsyncData *) data;
    char *what = (adata->block
		  ? "block"
		  : (adata->cancel
		     ? "cancel"
		     : (adata->eoj
			? "end_of_jobs"
			: "job")));
    ErlDrvTermData spec[] = {
	ERL_DRV_PORT, driver_mk_port(adata->port),
	ERL_DRV_ATOM, driver_mk_atom(what),
	ERL_DRV_TUPLE, 2
    };
    driver_send_term(adata->port, adata->receiver,
		     spec, sizeof(spec)/sizeof(spec[0]));
    if (adata->block) {
#ifdef __WIN32__
	Sleep((DWORD) 2000);
#else
	sleep(2);
#endif
    }
}

static void output(ErlDrvData drv_data,
		   char *buf, int len)
{
    ErlDrvPort port = (ErlDrvPort) drv_data;
    ErlDrvTermData caller = driver_caller(port);
    unsigned int key = (unsigned int) port;
    long id[5];
    Otp9302AsyncData *ad[5];
    int i;

    for (i = 0; i < sizeof(ad)/sizeof(ad[0]); i++) {
	ad[i] = driver_alloc(sizeof(Otp9302AsyncData));
	if (!ad[i])
	    abort();

	ad[i]->port = port;
	ad[i]->receiver = caller;
	ad[i]->block = 0;
	ad[i]->eoj = 0;
	ad[i]->cancel = 0;
    }
    ad[0]->block = 1;
    ad[2]->cancel = 1;
    ad[4]->eoj = 1;
    for (i = 0; i < sizeof(id)/sizeof(id[0]); i++)
	id[i] = driver_async(port, &key, async_invoke, ad[i], driver_free);
    if (id[2] > 0)
	driver_async_cancel(id[2]);
}

static void ready_async(ErlDrvData drv_data,
			ErlDrvThreadData thread_data)
{
    if ((void *) thread_data)
	driver_free((void *) thread_data);
}

