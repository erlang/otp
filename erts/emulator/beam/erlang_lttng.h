/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1996-2018. All Rights Reserved.
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

#ifdef USE_LTTNG
#undef TRACEPOINT_PROVIDER
#define TRACEPOINT_PROVIDER org_erlang_otp

#undef TRACEPOINT_INCLUDE
#define TRACEPOINT_INCLUDE "erlang_lttng.h"

#if !defined(__ERLANG_LTTNG_H__) || defined(TRACEPOINT_HEADER_MULTI_READ)
#define __ERLANG_LTTNG_H__

#include <lttng/tracepoint.h>

/* Schedulers */

TRACEPOINT_EVENT(
    org_erlang_otp,
    scheduler_poll,
    TP_ARGS(
        int, id,
        int, runnable
    ),
    TP_FIELDS(
        ctf_integer(int, scheduler, id)
        ctf_integer(int, runnable, runnable)
    )
)

#ifndef LTTNG_CARRIER_STATS
#define LTTNG_CARRIER_STATS
typedef struct {
    unsigned long no;
    unsigned long size;
} lttng_stat_values_t;

typedef struct {
    lttng_stat_values_t carriers;
    lttng_stat_values_t blocks;
} lttng_carrier_stats_t;
#endif


/* Port and Driver Scheduling */

TRACEPOINT_EVENT(
    org_erlang_otp,
    driver_start,
    TP_ARGS(
        char*, pid,
        char*, driver,
        char*, port
    ),
    TP_FIELDS(
        ctf_string(pid, pid)
        ctf_string(driver, driver)
        ctf_string(port, port)
    )
)

TRACEPOINT_EVENT(
    org_erlang_otp,
    driver_init,
    TP_ARGS(
        char*, driver,
        int, major,
        int, minor,
        int, flags
    ),
    TP_FIELDS(
        ctf_string(driver, driver)
        ctf_integer(int, major, major)
        ctf_integer(int, minor, minor)
        ctf_integer(int, flags, flags)
    )
)

TRACEPOINT_EVENT(
    org_erlang_otp,
    driver_outputv,
    TP_ARGS(
        char*, pid,
        char*, port,
        char*, driver,
        size_t, bytes
    ),
    TP_FIELDS(
        ctf_string(pid, pid)
        ctf_string(port, port)
        ctf_string(driver, driver)
        ctf_integer(size_t, bytes, bytes)
    )
)

TRACEPOINT_EVENT(
    org_erlang_otp,
    driver_output,
    TP_ARGS(
        char*, pid,
        char*, port,
        char*, driver,
        size_t, bytes
    ),
    TP_FIELDS(
        ctf_string(pid, pid)
        ctf_string(port, port)
        ctf_string(driver, driver)
        ctf_integer(size_t, bytes, bytes)
    )
)

TRACEPOINT_EVENT(
    org_erlang_otp,
    driver_ready_input,
    TP_ARGS(
        char*, pid,
        char*, port,
        char*, driver
    ),
    TP_FIELDS(
        ctf_string(pid, pid)
        ctf_string(port, port)
        ctf_string(driver, driver)
    )
)

TRACEPOINT_EVENT(
    org_erlang_otp,
    driver_ready_output,
    TP_ARGS(
        char*, pid,
        char*, port,
        char*, driver
    ),
    TP_FIELDS(
        ctf_string(pid, pid)
        ctf_string(port, port)
        ctf_string(driver, driver)
    )
)

TRACEPOINT_EVENT(
    org_erlang_otp,
    driver_timeout,
    TP_ARGS(
        char*, pid,
        char*, port,
        char*, driver
    ),
    TP_FIELDS(
        ctf_string(pid, pid)
        ctf_string(port, port)
        ctf_string(driver, driver)
    )
)

TRACEPOINT_EVENT(
    org_erlang_otp,
    driver_stop_select,
    TP_ARGS(
        char*, driver
    ),
    TP_FIELDS(
        ctf_string(driver, driver)
    )
)

TRACEPOINT_EVENT(
    org_erlang_otp,
    driver_flush,
    TP_ARGS(
        char*, pid,
        char*, port,
        char*, driver
    ),
    TP_FIELDS(
        ctf_string(pid, pid)
        ctf_string(port, port)
        ctf_string(driver, driver)
    )
)

TRACEPOINT_EVENT(
    org_erlang_otp,
    driver_stop,
    TP_ARGS(
        char*, pid,
        char*, port,
        char*, driver
    ),
    TP_FIELDS(
        ctf_string(pid, pid)
        ctf_string(port, port)
        ctf_string(driver, driver)
    )
)

TRACEPOINT_EVENT(
    org_erlang_otp,
    driver_process_exit,
    TP_ARGS(
        char*, pid,
        char*, port,
        char*, driver
    ),
    TP_FIELDS(
        ctf_string(pid, pid)
        ctf_string(port, port)
        ctf_string(driver, driver)
    )
)

TRACEPOINT_EVENT(
    org_erlang_otp,
    driver_ready_async,
    TP_ARGS(
        char*, pid,
        char*, port,
        char*, driver
    ),
    TP_FIELDS(
        ctf_string(pid, pid)
        ctf_string(port, port)
        ctf_string(driver, driver)
    )
)

TRACEPOINT_EVENT(
    org_erlang_otp,
    driver_finish,
    TP_ARGS(
        char*, driver
    ),
    TP_FIELDS(
        ctf_string(driver, driver)
    )
)

TRACEPOINT_EVENT(
    org_erlang_otp,
    driver_call,
    TP_ARGS(
        char*, pid,
        char*, port,
        char*, driver,
        unsigned int, command,
        size_t, bytes
    ),
    TP_FIELDS(
        ctf_string(pid, pid)
        ctf_string(port, port)
        ctf_string(driver, driver)
        ctf_integer(unsigned int, command, command)
        ctf_integer(size_t, bytes, bytes)
    )
)

TRACEPOINT_EVENT(
    org_erlang_otp,
    driver_control,
    TP_ARGS(
        char*, pid,
        char*, port,
        char*, driver,
        unsigned int, command,
        size_t, bytes
    ),
    TP_FIELDS(
        ctf_string(pid, pid)
        ctf_string(port, port)
        ctf_string(driver, driver)
        ctf_integer(unsigned int, command, command)
        ctf_integer(size_t, bytes, bytes)
    )
)

/* Async pool */

TRACEPOINT_EVENT(
    org_erlang_otp,
    aio_pool_get,
    TP_ARGS(
        char*, port,
        int, length
    ),
    TP_FIELDS(
        ctf_string(port, port)
        ctf_integer(int, length, length)
    )
)

TRACEPOINT_EVENT(
    org_erlang_otp,
    aio_pool_put,
    TP_ARGS(
        char*, port,
        int, length
    ),
    TP_FIELDS(
        ctf_string(port, port)
        ctf_integer(int, length, length)
    )
)


/* Memory Allocator */

TRACEPOINT_EVENT(
    org_erlang_otp,
    carrier_create,
    TP_ARGS(
        const char*, type,
        int, instance,
        unsigned long, size,
        lttng_carrier_stats_t *, mbcs,
        lttng_carrier_stats_t *, sbcs
    ),
    TP_FIELDS(
        ctf_string(type, type)
        ctf_integer(int, instance, instance)
        ctf_integer(unsigned long, size, size)
        ctf_integer(unsigned long, mbc_carriers, mbcs->carriers.no)
        ctf_integer(unsigned long, mbc_carriers_size, mbcs->carriers.size)
        ctf_integer(unsigned long, mbc_blocks, mbcs->blocks.no)
        ctf_integer(unsigned long, mbc_blocks_size, mbcs->blocks.size)
        ctf_integer(unsigned long, sbc_carriers, sbcs->carriers.no)
        ctf_integer(unsigned long, sbc_carriers_size, sbcs->carriers.size)
        ctf_integer(unsigned long, sbc_blocks, sbcs->blocks.no)
        ctf_integer(unsigned long, sbc_blocks_size, sbcs->blocks.size)
    )
)


TRACEPOINT_EVENT(
    org_erlang_otp,
    carrier_destroy,
    TP_ARGS(
        const char*, type,
        int, instance,
        unsigned long, size,
        lttng_carrier_stats_t *, mbcs,
        lttng_carrier_stats_t *, sbcs
    ),
    TP_FIELDS(
        ctf_string(type, type)
        ctf_integer(int, instance, instance)
        ctf_integer(unsigned long, size, size)
        ctf_integer(unsigned long, mbc_carriers, mbcs->carriers.no)
        ctf_integer(unsigned long, mbc_carriers_size, mbcs->carriers.size)
        ctf_integer(unsigned long, mbc_blocks, mbcs->blocks.no)
        ctf_integer(unsigned long, mbc_blocks_size, mbcs->blocks.size)
        ctf_integer(unsigned long, sbc_carriers, sbcs->carriers.no)
        ctf_integer(unsigned long, sbc_carriers_size, sbcs->carriers.size)
        ctf_integer(unsigned long, sbc_blocks, sbcs->blocks.no)
        ctf_integer(unsigned long, sbc_blocks_size, sbcs->blocks.size)
    )
)

TRACEPOINT_EVENT(
    org_erlang_otp,
    carrier_pool_put,
    TP_ARGS(
        const char*, name,
        int, instance,
        unsigned long, size
    ),
    TP_FIELDS(
        ctf_string(type, name)
        ctf_integer(int, instance, instance)
        ctf_integer(unsigned long, size, size)
    )
)

TRACEPOINT_EVENT(
    org_erlang_otp,
    carrier_pool_get,
    TP_ARGS(
        const char*, name,
        int, instance,
        unsigned long, size
    ),
    TP_FIELDS(
        ctf_string(type, name)
        ctf_integer(int, instance, instance)
        ctf_integer(unsigned long, size, size)
    )
)

#endif /* __ERLANG_LTTNG_H__ */
#include <lttng/tracepoint-event.h>
#endif /* USE_LTTNG */
