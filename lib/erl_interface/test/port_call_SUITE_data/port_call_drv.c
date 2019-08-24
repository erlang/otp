/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2001-2016. All Rights Reserved.
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

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "erl_interface.h"
#include "erl_driver.h"

static ErlDrvPort my_erlang_port;
static ErlDrvData echo_start(ErlDrvPort, char *);
static void from_erlang(ErlDrvData, char*, ErlDrvSizeT);
static ErlDrvSSizeT do_call(ErlDrvData drv_data, unsigned int command,
			    char *buf, ErlDrvSizeT len, char **rbuf,
			    ErlDrvSizeT rlen, unsigned *ret_flags);
static ErlDrvEntry echo_driver_entry = { 
    NULL,			/* Init */
    echo_start,
    NULL,			/* Stop */
    from_erlang,
    NULL,			/* Ready input */
    NULL,			/* Ready output */
    "port_call_drv",
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    do_call,
    NULL,
    ERL_DRV_EXTENDED_MARKER,
    ERL_DRV_EXTENDED_MAJOR_VERSION,
    ERL_DRV_EXTENDED_MINOR_VERSION,
    0,
    NULL,
    NULL,
    NULL,
};

DRIVER_INIT(echo_drv)
{
    return &echo_driver_entry;
}

static ErlDrvData
echo_start(ErlDrvPort port, char *buf)
{
    return (ErlDrvData) port;
}

static void
from_erlang(ErlDrvData data, char *buf, ErlDrvSizeT count)
{
    driver_output((ErlDrvPort) data, buf, count);
}

static ErlDrvSSizeT
do_call(ErlDrvData drv_data, unsigned int command, char *buf, 
	  ErlDrvSizeT len, char **rbuf, ErlDrvSizeT rlen, unsigned *ret_flags) 
{
    int nlen;
    ei_x_buff x;

    switch (command) {
    case 0:
	*rbuf = buf;
	*ret_flags |= DRIVER_CALL_KEEP_BUFFER;
	return len;
    case 1:
	ei_x_new(&x);
	ei_x_format(&x, "{[], a, [], b, c}");
	nlen = x.index;
	if (nlen > rlen) {
	    *rbuf =driver_alloc(nlen);
	}
	memcpy(*rbuf,x.buff,nlen);
	ei_x_free(&x);
	return nlen;
    case 2:
	ei_x_new(&x);
	ei_x_encode_version(&x);	
	ei_x_encode_tuple_header(&x,2);
	ei_x_encode_atom(&x,"return");
	ei_x_append_buf(&x,buf+1,len-1);
	nlen = x.index;
	if (nlen > rlen) {
	    *rbuf =driver_alloc(nlen);
	}
	memcpy(*rbuf,x.buff,nlen);
	ei_x_free(&x);
	return nlen;
    default:
	return -1;
    }
}

