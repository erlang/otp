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

#include <stdio.h>
#include <string.h>
#include "erl_driver.h"
#include "ei.h"

static ErlDrvSSizeT call(ErlDrvData drv_data,
			 unsigned int command,
			 char *buf, ErlDrvSizeT len,
			 char **rbuf, ErlDrvSizeT rlen,
			 unsigned int *flags);

static ErlDrvEntry call_local_drv_entry = { 
    NULL /* init */,
    NULL /* start */,
    NULL /* stop */,
    NULL /* output */,
    NULL /* ready_input */,
    NULL /* ready_output */,
    "call_local_drv",
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

DRIVER_INIT(call_local_drv)
{
    return &call_local_drv_entry;
}


static ErlDrvSSizeT call(ErlDrvData drv_data,
			 unsigned int command,
			 char *buf, ErlDrvSizeT len,
			 char **rbuf, ErlDrvSizeT rlen,
			 unsigned int *flags)
{
    ei_x_buff xbuf;
    void *bin1 = NULL, *bin2 = NULL, *bin3 = NULL;
    char *lext1, *lext2, *lext3;
    int vsn, arity, type, ix, lix, res, err, size;
    long size1, size2, size3;
    ErlDrvSSizeT ret_size = (ErlDrvSSizeT) ERL_DRV_ERROR_GENERAL;

    xbuf.buff = NULL;

    ei_init();

    ix = 0;
    res = ei_decode_version(buf, &ix, &vsn);
    if (res != 0 || vsn != 131)
        goto error;

    res = ei_decode_tuple_header(buf, &ix, &arity);
    if (res != 0)
        goto error;

    /* External term 1 */
    res = ei_get_type(buf, &ix, &type, &size);
    if (res != 0 && type != ERL_BINARY_EXT)
        goto error;

    size1 = size;
    bin1 = driver_alloc(size1);

    res = ei_decode_binary(buf, &ix, bin1, &size1);
    if (res != 0 && type != ERL_BINARY_EXT)
        goto error;

    lext1 = bin1;
    lix = 0;
    res = ei_decode_version(lext1, &lix, &vsn);
    if (res != 0 || vsn != 131)
        goto error;
    lext1 += lix;
    size1 -= lix;

    /* External term 2 */
    res = ei_get_type(buf, &ix, &type, &size);
    if (res != 0 && type != ERL_BINARY_EXT)
        goto error;

    size2 = size;
    bin2 = driver_alloc(size2);

    res = ei_decode_binary(buf, &ix, bin2, &size2);
    if (res != 0 && type != ERL_BINARY_EXT)
        goto error;

    lext2 = bin2;
    lix = 0;
    res = ei_decode_version(lext2, &lix, &vsn);
    if (res != 0 || vsn != 131)
        goto error;
    lext2 += lix;
    size2 -= lix;

    /* External term 3 */
    res = ei_get_type(buf, &ix, &type, &size);
    if (res != 0 && type != ERL_BINARY_EXT)
        goto error;

    size3 = size;
    bin3 = driver_alloc(size3);

    res = ei_decode_binary(buf, &ix, bin3, &size3);
    if (res != 0 && type != ERL_BINARY_EXT)
        goto error;

    lext3 = bin3;
    lix = 0;
    res = ei_decode_version(lext3, &lix, &vsn);
    if (res != 0 || vsn != 131)
        goto error;
    lext3 += lix;
    size3 -= lix;

    /* encode result */

    res = ei_x_new_with_version(&xbuf);
    if (res != 0)
        goto error;

    res = ei_x_encode_tuple_header(&xbuf, 7);
    if (res != 0)
        goto error;

    res = ei_x_encode_atom(&xbuf, "call_result");
    if (res != 0)
        goto error;

    res = ei_x_append_buf(&xbuf, lext1, size1);
    if (res != 0)
        goto error;

    res = ei_x_encode_long(&xbuf, 4711);
    if (res != 0)
        goto error;

    res = ei_x_append_buf(&xbuf, lext2, size2);
    if (res != 0)
        goto error;

    res = ei_x_encode_long(&xbuf, 17);
    if (res != 0)
        goto error;

    res = ei_x_append_buf(&xbuf, lext3, size3);
    if (res != 0)
        goto error;

    res = ei_x_encode_string(&xbuf, "end_of_data");
    if (res != 0)
        goto error;

    /* success */
    ret_size = xbuf.index;
    *rbuf = driver_alloc(ret_size);
    memcpy((void *) *rbuf, (void *) xbuf.buff, ret_size);

error:

    if (bin1)
        driver_free(bin1);
    if (bin2)
        driver_free(bin2);
    if (bin3)
        driver_free(bin3);

    if (xbuf.buff)
        ei_x_free(&xbuf);

    return ret_size;
}
