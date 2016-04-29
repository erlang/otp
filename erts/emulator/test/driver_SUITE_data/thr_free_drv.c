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
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include "erl_driver.h"

#define BLOCKS_PER_THREAD 100000
#define NO_THREADS 10
#define BLOCKS_PER_CTRL 1000

typedef struct {
    ErlDrvMutex *mtx;
    ErlDrvCond *cnd;
    int b;
    int *go;
    int *skip;
    void *blocks[BLOCKS_PER_THREAD];
} test_thread_data;

typedef struct {
    ErlDrvPort port;
    int b;
    int go;
    int skip;
    test_thread_data ttd[NO_THREADS+1];
    ErlDrvTid tids[NO_THREADS+1];    
} test_data;

static ErlDrvData start(ErlDrvPort port, char *command);
static void stop(ErlDrvData data);
static ErlDrvSSizeT control(ErlDrvData drv_data, unsigned int command, char *buf,
			    ErlDrvSizeT len, char **rbuf, ErlDrvSizeT rlen);

static ErlDrvEntry thr_free_drv_entry = { 
    NULL /* init */,
    start,
    stop,
    NULL /* output */,
    NULL /* ready_input */,
    NULL /* ready_output */,
    "thr_free_drv",
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

DRIVER_INIT(thr_free_drv)
{
    return &thr_free_drv_entry;
}

void *
test_thread(void *vttd)
{
    test_thread_data *ttd = (test_thread_data *) vttd;
    int i, skip;

    erl_drv_mutex_lock(ttd->mtx);

    while (!*ttd->go)
	erl_drv_cond_wait(ttd->cnd, ttd->mtx);
    skip = *ttd->skip;
    erl_drv_mutex_unlock(ttd->mtx);

    if (!skip) {
	for (i = 0; i < BLOCKS_PER_THREAD; i++)
	    driver_free(ttd->blocks[i]);
    }
    return NULL;
}

ErlDrvData start(ErlDrvPort port, char *command)
{
    int join = 0, t, b, res;
    test_thread_data *ttd;
    test_data *td = driver_alloc(sizeof(test_data));
    if (!td)
	return ERL_DRV_ERROR_GENERAL;
    ttd = td->ttd;
    for (b = 0; b < BLOCKS_PER_THREAD; b++)
	for (t = 0; t <= NO_THREADS; t++)
	    ttd[t].blocks[b] = NULL;
    ttd[0].mtx = NULL;
    ttd[0].cnd = NULL;

    for (b = 0; b < BLOCKS_PER_THREAD; b++) {
	for (t = 0; t <= NO_THREADS; t++) {
	    ttd[t].blocks[b] = driver_alloc(1);
	    if (ttd[t].blocks[b] == NULL)
		goto fail;
	}
    }

    td->b = -1;
    td->go = 0;
    td->skip = 0;

    ttd[0].mtx = erl_drv_mutex_create("test_mutex");
    if (!ttd[0].mtx)
	goto fail;
    ttd[0].cnd = erl_drv_cond_create("test_cnd");
    if (!ttd[0].cnd)
	goto fail;
    ttd[0].go = &td->go;
    ttd[0].skip = &td->skip;

    for (t = 1; t <= NO_THREADS; t++) {
	ttd[t].mtx = ttd[0].mtx;
	ttd[t].cnd = ttd[0].cnd;
	ttd[t].go = ttd[0].go;
	ttd[t].skip = ttd[0].skip;
	res = erl_drv_thread_create("test_thread",
				    &td->tids[t],
				    test_thread,
				    &ttd[t],
				    NULL);
	if (res != 0)
	    goto fail;
	join = t;
    }

    td->port = port;

    return (ErlDrvData) td;

fail:

    if (join) {
	erl_drv_mutex_lock(ttd[0].mtx);
	td->go = 1;
	td->skip = 1;
	erl_drv_cond_broadcast(ttd[0].cnd);
	erl_drv_mutex_unlock(ttd[0].mtx);
	for (t = 1; t <= join; t++)
	    erl_drv_thread_join(td->tids[t], NULL);
    }

    if (ttd[0].mtx)
	erl_drv_mutex_destroy(ttd[0].mtx);
    if (ttd[0].cnd)
	erl_drv_cond_destroy(ttd[0].cnd);

    for (b = 0; b < BLOCKS_PER_THREAD; b++) {
	for (t = 0; t <= NO_THREADS; t++) {
	    if (ttd[t].blocks[b] != NULL)
		driver_free(ttd[t].blocks[b]);
	}
    }
    driver_free(td);
    return ERL_DRV_ERROR_GENERAL;
}

static void stop(ErlDrvData drv_data)
{
    test_data *td = (test_data *) drv_data;
    int t, b;
    for (t = 1; t <= NO_THREADS; t++)
	erl_drv_thread_join(td->tids[t], NULL);
    for (b = 0; b < BLOCKS_PER_THREAD; b++) {
	if (td->ttd[0].blocks[b])
	    driver_free(td->ttd[0].blocks[b]);
    }
    erl_drv_mutex_destroy(td->ttd[0].mtx);
    erl_drv_cond_destroy(td->ttd[0].cnd);
    driver_free(td);
}

static ErlDrvSSizeT control(ErlDrvData drv_data, unsigned int command, char *buf,
			    ErlDrvSizeT len, char **rbuf, ErlDrvSizeT rlen)
{
    test_data *td = (test_data *) drv_data;
    char *result = "failure";
    int i, b;
    int res;
    ErlDrvSSizeT result_len;

    if (td->b == -1) {
	erl_drv_mutex_lock(td->ttd[0].mtx);
	td->go = 1;
	erl_drv_cond_broadcast(td->ttd[0].cnd);
	erl_drv_mutex_unlock(td->ttd[0].mtx);
	td->b = 0;
    }

    for (i = 0, b = td->b; i < BLOCKS_PER_CTRL && b < BLOCKS_PER_THREAD; i++, b++) {
	driver_free(td->ttd[0].blocks[b]);
	td->ttd[0].blocks[b] = NULL;
    }

    td->b = b;
    if (b >= BLOCKS_PER_THREAD)
	result = "done";
    else
	result = "more";

    result_len = strlen(result);
    if (result_len <= rlen) {
	memcpy(*rbuf, result, result_len);
	return result_len;
    }
    else {
	*rbuf = driver_alloc(result_len);
	if (!*rbuf) {
	    driver_failure_posix(td->port, ENOMEM);
	    return 0;
	}
	else {
	    memcpy(*rbuf, result, result_len);
	    return result_len;
	}
    }
}
