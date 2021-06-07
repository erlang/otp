/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2021. All Rights Reserved.
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

#include "erl_driver.h"

static void stop(ErlDrvData drv_data);
static ErlDrvData start(ErlDrvPort port,
			char *command);
static void output(ErlDrvData drv_data,
		   char *buf, ErlDrvSizeT len);
static void flush(ErlDrvData drv_data);
static void timeout(ErlDrvData drv_data);
static void process_exit(ErlDrvData drv_data, ErlDrvMonitor *monitor);

static ErlDrvEntry unlink_signal_entry = { 
    NULL /* init */,
    start,
    stop,
    output,
    NULL /* ready_input */,
    NULL /* ready_output */,
    "unlink_signal_drv",
    NULL /* finish */,
    NULL /* handle */,
    NULL /* control */,
    timeout,
    NULL /* outputv */,
    NULL /* ready_async */,
    flush,
    NULL /* call */,
    NULL /* event */,
    ERL_DRV_EXTENDED_MARKER,
    ERL_DRV_EXTENDED_MAJOR_VERSION,
    ERL_DRV_EXTENDED_MINOR_VERSION,
    ERL_DRV_FLAG_USE_PORT_LOCKING,
    NULL /* handle2 */,
    process_exit,
    NULL /* stop_select */
};

DRIVER_INIT(unlink_signal_entry)
{
    return &unlink_signal_entry;
}

typedef struct {
    ErlDrvData port;
    int timeout_count;
} us_drv_state;

static void stop(ErlDrvData drv_data)
{
    driver_free((void *) drv_data);
}

static ErlDrvData start(ErlDrvPort port,
			char *command)
{
    us_drv_state *state = (us_drv_state *) driver_alloc(sizeof(us_drv_state));
    state->port = port;
    state->timeout_count = 0;
    return (ErlDrvData) state;
}

static void output(ErlDrvData drv_data,
		   char *buf, ErlDrvSizeT len)
{
    us_drv_state *state = (us_drv_state *) drv_data;
    driver_set_timer(state->port, 2);
}

static void flush(ErlDrvData drv_data)
{
    us_drv_state *state = (us_drv_state *) drv_data;
    driver_set_timer(state->port, 5);
}

static void timeout(ErlDrvData drv_data)
{
    us_drv_state *state = (us_drv_state *) drv_data;
    state->timeout_count++;
    if (state->timeout_count == 1) {
        int i, limit;
        ErlDrvTermData connected = driver_connected(state->port);
        /*
         * Prevent completion of port termination, so that connected
         * process will be able to send an unlink-ack signal to the
         * port...
         */
        driver_enq(state->port, "x", 1);
        limit = (int) (((unsigned)state->port) % 1000);
        /*
         * Spam connected process with various amounts of monitor,
         * demonitor signals...
         */
        for (i = 0; i < limit; i++) {
            ErlDrvMonitor *monitor = driver_alloc(sizeof(ErlDrvMonitor));
            driver_monitor_process(state->port, connected, monitor);
            driver_demonitor_process(state->port, monitor);
            driver_free(monitor);
        }
        /* driver_exit() will send an unlink signal to conneced process... */
        driver_exit(state->port, 0);
    }
    else {
        /* Let port complete termination.. */
        driver_deq(state->port, 1);
    }
}

static void
process_exit(ErlDrvData drv_data, ErlDrvMonitor *monitor)
{
    driver_free(monitor);
}
