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

#include <stdio.h>
#include <string.h>
#include "erl_driver.h"

static int call(ErlDrvData drv_data,
		unsigned int command,
		char *buf, int len,
		char **rbuf, int rlen,
		unsigned int *flags);

static ErlDrvEntry otp_6879_drv_entry = { 
    NULL /* init */,
    NULL /* start */,
    NULL /* stop */,
    NULL /* output */,
    NULL /* ready_input */,
    NULL /* ready_output */,
    "otp_6879_drv",
    NULL /* finish */,
    NULL /* handle */,
    NULL /* control */,
    NULL /* timeout */,
    NULL /* outputv */,
    NULL /* ready_async */,
    NULL /* flush */,
    call,
    NULL /* event */,
    ERL_DRV_EXTENDED_MARKER,
    ERL_DRV_EXTENDED_MAJOR_VERSION,
    ERL_DRV_EXTENDED_MINOR_VERSION,
    ERL_DRV_FLAG_USE_PORT_LOCKING,
    NULL /* handle2 */,
    NULL /* handle_monitor */
};

DRIVER_INIT(otp_6879_drv)
{
    return &otp_6879_drv_entry;
}


static int call(ErlDrvData drv_data,
		unsigned int command,
		char *buf, int len,
		char **rbuf, int rlen,
		unsigned int *flags)
{
    /* echo call */
    if (len > rlen)
	*rbuf = driver_alloc(len);
    memcpy((void *) *rbuf, (void *) buf, len);
    return len;
}
