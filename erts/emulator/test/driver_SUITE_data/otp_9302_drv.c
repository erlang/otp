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
#ifdef __WIN32__
#include <windows.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include "erl_driver.h"

static void stop(ErlDrvData drv_data);
static ErlDrvData start(ErlDrvPort port,
			char *command);
static void output(ErlDrvData drv_data,
		   char *buf, ErlDrvSizeT len);
static void ready_async(ErlDrvData drv_data,
			ErlDrvThreadData thread_data);

static ErlDrvEntry otp_9302_drv_entry = { 
    NULL /* init */,
    start,
    stop,
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

typedef struct Otp9302AsyncData_ Otp9302AsyncData;

typedef struct {
    ErlDrvMutex *mtx;
    Otp9302AsyncData *start;
    Otp9302AsyncData *end;
} Otp9302MsgQ;

typedef struct {
    ErlDrvPort port;
    int smp;
    Otp9302MsgQ msgq;
} Otp9302Data;

struct Otp9302AsyncData_ {
    Otp9302AsyncData *next;
    ErlDrvPort port;
    int smp;
    int refc;
    int block;
    struct {
	ErlDrvTermData port;
	ErlDrvTermData receiver;
	ErlDrvTermData msg;
    } term_data;
    Otp9302MsgQ *msgq;
};


DRIVER_INIT(otp_9302_drv)
{
    return &otp_9302_drv_entry;
}

static void stop(ErlDrvData drv_data)
{
    Otp9302Data *data = (Otp9302Data *) drv_data;
    if (data->msgq.mtx)
	erl_drv_mutex_destroy(data->msgq.mtx);
    driver_free(data);
}

static ErlDrvData start(ErlDrvPort port,
			char *command)
{
    Otp9302Data *data;
    ErlDrvSysInfo sys_info;

    data = driver_alloc(sizeof(Otp9302Data));
    if (!data)
	return ERL_DRV_ERROR_GENERAL;

    data->port = port;

    driver_system_info(&sys_info, sizeof(ErlDrvSysInfo));
    data->smp = sys_info.smp_support;

    data->msgq.mtx = NULL;
    if (!data->smp) {
	data->msgq.start = NULL;
	data->msgq.end = NULL;
	if (sys_info.thread_support) {
	    data->msgq.mtx = erl_drv_mutex_create("");
	    if (!data->msgq.mtx) {
		driver_free(data);
		return ERL_DRV_ERROR_GENERAL;
	    }
	}
    }

    return (ErlDrvData) data;
}

static void send_reply(Otp9302AsyncData *adata)
{
    ErlDrvTermData spec[] = {
	ERL_DRV_PORT, adata->term_data.port,
	ERL_DRV_ATOM, adata->term_data.msg,
	ERL_DRV_TUPLE, 2
    };
    erl_drv_send_term(adata->term_data.port, adata->term_data.receiver,
		      spec, sizeof(spec)/sizeof(spec[0]));
}

static void enqueue_reply(Otp9302AsyncData *adata)
{
    Otp9302MsgQ *msgq = adata->msgq;
    adata->next = NULL;
    adata->refc++;
    if (msgq->mtx)
	erl_drv_mutex_lock(msgq->mtx);
    if (msgq->end)
	msgq->end->next = adata;
    else
	msgq->end = msgq->start = adata;
    msgq->end = adata;
    if (msgq->mtx)
	erl_drv_mutex_unlock(msgq->mtx);
}

static void dequeue_replies(Otp9302AsyncData *adata)
{
    Otp9302MsgQ *msgq = adata->msgq;
    if (msgq->mtx)
	erl_drv_mutex_lock(msgq->mtx);
    if (--adata->refc == 0)
	driver_free(adata);
    while (msgq->start) {
	send_reply(msgq->start);
	adata = msgq->start;
	msgq->start = msgq->start->next;
	if (--adata->refc == 0)
	    driver_free(adata);
    }
    msgq->start = msgq->end = NULL;
    if (msgq->mtx)
	erl_drv_mutex_unlock(msgq->mtx);
}

static void async_invoke(void *data)
{
    Otp9302AsyncData *adata = (Otp9302AsyncData *) data;
    if (adata->block) {
#ifdef __WIN32__
	Sleep((DWORD) 2000);
#else
	sleep(2);
#endif
    }
    if (adata->smp)
	send_reply(adata);
    else
	enqueue_reply(adata);
}

static void ready_async(ErlDrvData drv_data,
			ErlDrvThreadData thread_data)
{
    Otp9302AsyncData *adata = (Otp9302AsyncData *) thread_data;
    if (adata->smp)
	driver_free(adata);
    else
	dequeue_replies(adata);
}

static void output(ErlDrvData drv_data,
		   char *buf, ErlDrvSizeT len)
{
    Otp9302Data *data = (Otp9302Data *) drv_data;
    ErlDrvTermData td_port = driver_mk_port(data->port);
    ErlDrvTermData td_receiver = driver_caller(data->port);
    ErlDrvTermData td_job = driver_mk_atom("job");
    unsigned int key = (unsigned int) (ErlDrvSInt) data->port;
    long id[5];
    Otp9302AsyncData *ad[5];
    int i;

    for (i = 0; i < sizeof(ad)/sizeof(ad[0]); i++) {
	ad[i] = driver_alloc(sizeof(Otp9302AsyncData));
	if (!ad[i])
	    abort();

	ad[i]->smp = data->smp;
	ad[i]->port = data->port;
	ad[i]->block = 0;
	ad[i]->refc = 1;
	ad[i]->term_data.port = td_port;
	ad[i]->term_data.receiver = td_receiver;
	ad[i]->term_data.msg = td_job;
	ad[i]->msgq = &data->msgq;
    }
    ad[0]->block = 1;
    ad[0]->term_data.msg = driver_mk_atom("block");
    ad[2]->term_data.msg = driver_mk_atom("cancel");
    ad[4]->term_data.msg = driver_mk_atom("end_of_jobs");
    for (i = 0; i < sizeof(id)/sizeof(id[0]); i++)
	id[i] = driver_async(data->port, &key, async_invoke, ad[i], driver_free);
}
