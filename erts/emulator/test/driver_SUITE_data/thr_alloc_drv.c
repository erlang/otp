/* ``The contents of this file are subject to the Erlang Public License,
 * Version 1.1, (the "License"); you may not use this file except in
 * compliance with the License. You should have received a copy of the
 * Erlang Public License along with this software. If not, it can be
 * retrieved via the world wide web at http://www.erlang.org/.
 * 
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
 * the License for the specific language governing rights and limitations
 * under the License.
 * 
 * The Initial Developer of the Original Code is Ericsson Utvecklings AB.
 * Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
 * AB. All Rights Reserved.''
 * 
 *     $Id$
 */
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include "erl_driver.h"

ErlDrvData start(ErlDrvPort port, char *command);
int control(ErlDrvData drv_data, unsigned int command, char *buf,
	    int len, char **rbuf, int rlen);

static int call(ErlDrvData drv_data,
		unsigned int command,
		char *buf, int len,
		char **rbuf, int rlen,
		unsigned int *flags);

static ErlDrvEntry thr_alloc_drv_entry = { 
    NULL /* init */,
    start,
    NULL /* stop */,
    NULL /* output */,
    NULL /* ready_input */,
    NULL /* ready_output */,
    "thr_alloc_drv",
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

DRIVER_INIT(thr_alloc_drv)
{
    return &thr_alloc_drv_entry;
}

void *
test_thread(void *vsize)
{
    int i;
    int size = (int) (long) vsize;
    void *mem;
    mem = driver_alloc(size);
    if (mem)
	driver_free(mem);
}

ErlDrvData start(ErlDrvPort port, char *command)
{
    return (ErlDrvData) port;
}

int control(ErlDrvData drv_data, unsigned int command, char *buf,
	    int len, char **rbuf, int rlen)
{
    ErlDrvPort port = (ErlDrvPort) drv_data;
    char *result = "failure";
    int result_len;
    if (len <= 20) {
	int res;
	ErlDrvTid tid;
	char ibuf[21];
	int size;
	memcpy((void *) ibuf, buf, len);
	ibuf[len] = '\0';
	size = atoi(ibuf);
	if (size > 0) {
	    res = erl_drv_thread_create("test_thread",
					&tid,
					test_thread,
					(void *) (long) size,
					NULL);
	    if (res == 0) {
		res = erl_drv_thread_join(tid, NULL);
		if (res == 0) 
		    result = "ok";
	    }
	    if (res != 0)
		driver_failure_posix(port, res);
	}
    }

    result_len = strlen(result);
    if (result_len <= rlen) {
	memcpy(*rbuf, result, result_len);
	return result_len;
    }
    else {
	*rbuf = driver_alloc(result_len);
	if (!*rbuf) {
	    driver_failure_posix(port, ENOMEM);
	    return 0;
	}
	else {
	    memcpy(*rbuf, result, result_len);
	    return result_len;
	}
    }
}
