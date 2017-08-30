/* ``Licensed under the Apache License, Version 2.0 (the "License");
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
 * The Initial Developer of the Original Code is Ericsson Utvecklings AB.
 * Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
 * AB. All Rights Reserved.''
 * 
 *     $Id$
 */

#include <stdlib.h>
#include <string.h>
#include "erl_driver.h"

static int init();
static void stop(ErlDrvData drv_data);
static void finish();
static void flush(ErlDrvData drv_data);
static ErlDrvData start(ErlDrvPort port, char *command);
static void output(ErlDrvData drv_data, char *buf, ErlDrvSizeT len);
static void outputv(ErlDrvData drv_data, ErlIOVec *ev);
static ErlDrvSSizeT control(ErlDrvData drv_data,
			    unsigned int command, char *buf,
			    ErlDrvSizeT len, char **rbuf, ErlDrvSizeT rlen);
static ErlDrvSSizeT call(ErlDrvData drv_data,
			 unsigned int command,
			 char *buf, ErlDrvSizeT len,
			 char **rbuf, ErlDrvSizeT rlen,
			 unsigned int *flags);

static ErlDrvEntry caller_drv_entry = { 
    init,
    start,
    stop,
    output,
    NULL /* ready_input */,
    NULL /* ready_output */,
    "caller_drv",
    finish,
    NULL /* handle */,
    control,
    NULL /* timeout */,
    outputv,
    NULL /* ready_async */,
    flush,
    call,
    NULL /* event */,
    ERL_DRV_EXTENDED_MARKER,
    ERL_DRV_EXTENDED_MAJOR_VERSION,
    ERL_DRV_EXTENDED_MINOR_VERSION,
    ERL_DRV_FLAG_USE_PORT_LOCKING,
    NULL /* handle2 */,
    NULL /* handle_monitor */
};

DRIVER_INIT(caller_drv)
{
    char buf[10];
    size_t bufsz = sizeof(buf);
    char *use_outputv;
    use_outputv = (erl_drv_getenv("CALLER_DRV_USE_OUTPUTV", buf, &bufsz) == 0
		   ? buf
		   : "false");
    if (strcmp(use_outputv, "true") != 0)
	caller_drv_entry.outputv = NULL;
    return &caller_drv_entry;
}

void
send_caller(ErlDrvData drv_data, char *func)
{
    int res;
    ErlDrvPort port = (ErlDrvPort) drv_data;
    ErlDrvTermData msg[] = {
	ERL_DRV_ATOM,	driver_mk_atom("caller"),
	ERL_DRV_PORT,	driver_mk_port(port),
	ERL_DRV_ATOM,	driver_mk_atom(func),
	ERL_DRV_PID,	driver_caller(port),
	ERL_DRV_TUPLE,	(ErlDrvTermData) 4
    };
    res = erl_drv_output_term(driver_mk_port(port), msg, sizeof(msg)/sizeof(ErlDrvTermData));
    if (res <= 0)
	driver_failure_atom(port, "erl_drv_output_term failed");
}

static int
init() {
    return 0;
}

static void
stop(ErlDrvData drv_data)
{

}

static void
flush(ErlDrvData drv_data)
{

}

static void
finish()
{

}

static ErlDrvData
start(ErlDrvPort port, char *command)
{
    send_caller((ErlDrvData) port, "start");
    return (ErlDrvData) port;
}

static void
output(ErlDrvData drv_data, char *buf, ErlDrvSizeT len)
{
    send_caller(drv_data, "output");
}

static void
outputv(ErlDrvData drv_data, ErlIOVec *ev)
{
    send_caller(drv_data, "outputv");
}

static ErlDrvSSizeT
control(ErlDrvData drv_data,
	unsigned int command, char *buf, 
	ErlDrvSizeT len, char **rbuf, ErlDrvSizeT rlen)
{
    send_caller(drv_data, "control");
    return 0;
}

static ErlDrvSSizeT
call(ErlDrvData drv_data,
     unsigned int command,
     char *buf, ErlDrvSizeT len,
     char **rbuf, ErlDrvSizeT rlen,
     unsigned int *flags)
{
    /* echo call */
    if (len > rlen)
	*rbuf = driver_alloc(len);
    memcpy((void *) *rbuf, (void *) buf, len);
    send_caller(drv_data, "call");
    return len;
}
