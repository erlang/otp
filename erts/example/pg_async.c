/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2006-2016. All Rights Reserved.
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
 * Purpose: A driver using libpq to connect to Postgres
 * from erlang, a sample for the driver documentation
 */

#include <erl_driver.h>

#include <libpq-fe.h>

#include <ei.h>

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "pg_encode.h"

/* Driver interface declarations */
static ErlDrvData start(ErlDrvPort port, char *command);
static void stop(ErlDrvData drv_data);
static int control(ErlDrvData drv_data, unsigned int command, char *buf, 
		   int len, char **rbuf, int rlen); 
static void ready_io(ErlDrvData drv_data, ErlDrvEvent event);

static ErlDrvEntry pq_driver_entry = {
    NULL,			/* init */
    start, 
    stop, 
    NULL,			/* output */
    ready_io,			/* ready_input */
    ready_io,			/* ready_output */ 
    "pg_async",                 /* the name of the driver */
    NULL,			/* finish */
    NULL,			/* handle */
    control, 
    NULL,			/* timeout */
    NULL,			/* outputv */
    NULL,			/* ready_async */
    NULL,			/* flush */
    NULL,			/* call */
    NULL			/* event */
};

typedef struct our_data_t {
    PGconn* conn;
    ErlDrvPort port;
    int socket;
    int connecting;
} our_data_t;

/* Keep the following definitions in alignment with the FUNC_LIST
 * in erl_pq_sync.erl
 */

#define DRV_CONNECT             'C'
#define DRV_DISCONNECT          'D'
#define DRV_SELECT              'S'

/* #define L     fprintf(stderr, "%d\r\n", __LINE__) */

/* INITIALIZATION AFTER LOADING */

/* 
 * This is the init function called after this driver has been loaded.
 * It must *not* be declared static. Must return the address to 
 * the driver entry.
 */
DRIVER_INIT(pq_drv)
{
    return &pq_driver_entry;
}

static char* get_s(const char* buf, int len);
static int do_connect(const char *s, our_data_t* data);
static int do_disconnect(our_data_t* data);
static int do_select(const char* s, our_data_t* data);

/* DRIVER INTERFACE */
static ErlDrvData start(ErlDrvPort port, char *command)
{ 
    our_data_t* data = driver_alloc(sizeof(our_data_t));
    data->port = port;
    data->conn = NULL;
    return (ErlDrvData)data;
}

static void stop(ErlDrvData drv_data)
{
    do_disconnect((our_data_t*)drv_data);
}

static int control(ErlDrvData drv_data, unsigned int command, char *buf,
		   int len, char **rbuf, int rlen)
{
    int r;
    char* s = get_s(buf, len);
    our_data_t* data = (our_data_t*)drv_data;
    switch (command) {
    case DRV_CONNECT:     r = do_connect(s, data);  break;
    case DRV_DISCONNECT:  r = do_disconnect(data);  break;
    case DRV_SELECT:      r = do_select(s, data);   break;
    default:              r = -1;	break;
    }
    driver_free(s);
    return r;
}

static int do_connect(const char *s, our_data_t* data)
{
    PGconn* conn = PQconnectStart(s);
    if (PQstatus(conn) == CONNECTION_BAD) {
	ei_x_buff x;
	ei_x_new_with_version(&x);
        encode_error(&x, conn);
	PQfinish(conn);
	conn = NULL;
	driver_output(data->port, x.buff, x.index);
	ei_x_free(&x);
    }
    PQconnectPoll(conn);
    int socket = PQsocket(conn);
    data->socket = socket;
    driver_select(data->port, (ErlDrvEvent)socket, DO_READ, 1);
    driver_select(data->port, (ErlDrvEvent)socket, DO_WRITE, 1);
    data->conn = conn;
    data->connecting = 1;
    return 0;
}

static int do_disconnect(our_data_t* data)
{
    ei_x_buff x;
    driver_select(data->port, (ErlDrvEvent)data->socket, DO_READ, 0);
    driver_select(data->port, (ErlDrvEvent)data->socket, DO_WRITE, 0);
    PQfinish(data->conn);
    data->conn = NULL;
    ei_x_new_with_version(&x);
    encode_ok(&x);
    driver_output(data->port, x.buff, x.index);
    ei_x_free(&x);
    return 0;
}

static int do_select(const char* s, our_data_t* data)
{
    data->connecting = 0;
    PGconn* conn = data->conn;
    /* if there's an error return it now */
    if (PQsendQuery(conn, s) == 0) {
	ei_x_buff x;
	ei_x_new_with_version(&x);
	encode_error(&x, conn);
	driver_output(data->port, x.buff, x.index);
	ei_x_free(&x);
    }
    /* else wait for ready_output to get results */
    return 0;
}

static void ready_io(ErlDrvData drv_data, ErlDrvEvent event)
{
    PGresult* res = NULL;
    our_data_t* data = (our_data_t*)drv_data;
    PGconn* conn = data->conn;
    ei_x_buff x;
    ei_x_new_with_version(&x);
    if (data->connecting) {
	ConnStatusType status;
	PQconnectPoll(conn);
	status = PQstatus(conn);
	if (status == CONNECTION_OK)
	    encode_ok(&x);
	else if (status == CONNECTION_BAD)
	    encode_error(&x, conn);
    } else {
	PQconsumeInput(conn);
	if (PQisBusy(conn))
	    return;
	res = PQgetResult(conn);
	encode_result(&x, res, conn);
	PQclear(res);
	for (;;) {
	    res = PQgetResult(conn);
	    if (res == NULL)
		break;
	    PQclear(res);
	}
    }
    if (x.index > 1) {
	driver_output(data->port, x.buff, x.index);
	if (data->connecting) 
	    driver_select(data->port, (ErlDrvEvent)data->socket, DO_WRITE, 0);
    }
    ei_x_free(&x);
}

/* utilities */
static char* get_s(const char* buf, int len)
{
    char* result;
    if (len < 1 || len > 1000) return NULL;
    result = driver_alloc(len+1);
    memcpy(result, buf, len);
    result[len] = '\0';
    return result;
}
