/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2017. All Rights Reserved.
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

/* Tests whether erl_drv_putenv/erl_drv_getenv work correctly and reflect
 * changes to os:putenv/getenv. */

#include <string.h>
#include <stdio.h>

#include "erl_driver.h"

static ErlDrvSSizeT env_drv_ctl(ErlDrvData drv_data, unsigned int cmd,
        char* buf, ErlDrvSizeT len, char** rbuf, ErlDrvSizeT rsize);

static ErlDrvEntry env_drv_entry = { 
    NULL /* init */,
    NULL /* start */,
    NULL /* stop */,
    NULL /* output */,
    NULL /* ready_input */,
    NULL /* ready_output */,
    "env_drv",
    NULL /* finish */,
    NULL /* handle */,
    env_drv_ctl,
    NULL /* timeout */,
    NULL /* outputv*/,
    NULL /* ready_async */,
    NULL /* flush */,
    NULL /* call*/,
    NULL /* event */,
    ERL_DRV_EXTENDED_MARKER,
    ERL_DRV_EXTENDED_MAJOR_VERSION,
    ERL_DRV_EXTENDED_MINOR_VERSION,
    ERL_DRV_FLAG_USE_PORT_LOCKING,
    NULL /* handle2 */,
    NULL /* handle_monitor */
};

DRIVER_INIT(env_drv) {
    return &env_drv_entry;
}

static int test_putenv(ErlDrvData drv_data, char *buf, ErlDrvSizeT len) {
    char key[256], value[256];
    int key_len, value_len;

    key_len = buf[0];
    value_len = buf[1];

    sprintf(key, "%.*s", key_len, &buf[2]);
    sprintf(value, "%.*s", value_len, &buf[2 + key_len]);

    return erl_drv_putenv(key, value);
}

static int test_getenv(ErlDrvData drv_data, char *buf, ErlDrvSizeT len) {
    char expected_value[256], stored_value[256], key[256];
    int expected_value_len, key_len;
    size_t stored_value_len;
    int res;

    key_len = buf[0];
    sprintf(key, "%.*s", key_len, &buf[2]);

    expected_value_len = buf[1];
    sprintf(expected_value, "%.*s", expected_value_len, &buf[2 + key_len]);

    stored_value_len = sizeof(stored_value);
    res = erl_drv_getenv(key, stored_value, &stored_value_len);

    if(res == 0) {
        return strcmp(stored_value, expected_value) != 0;
    } else if(res == 1) {
        return 127;
    }

    return 255;
}

static ErlDrvSSizeT env_drv_ctl(ErlDrvData drv_data, unsigned int cmd,
        char* buf, ErlDrvSizeT len, char** rbuf, ErlDrvSizeT rsize) {

    if(cmd == 0) {
        (**rbuf) = (char)test_putenv(drv_data, buf, len);
    } else {
        (**rbuf) = (char)test_getenv(drv_data, buf, len);
    }

    return 1;
}
