/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1996-2016. All Rights Reserved.
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
#define TRACEPOINT_PROVIDER com_ericsson_otp

#undef TRACEPOINT_INCLUDE
#define TRACEPOINT_INCLUDE "erlang_lttng.h"

#if !defined(__ERLANG_LTTNG_H__) || defined(TRACEPOINT_HEADER_MULTI_READ)
#define __ERLANG_LTTNG_H__

#include <lttng/tracepoint.h>

/* Schedulers */

TRACEPOINT_EVENT(
    com_ericsson_otp,
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


/* Memory Allocator */

TRACEPOINT_EVENT(
    com_ericsson_otp,
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
        ctf_float(float, mbc_ratio, (float)mbcs->blocks.size/(float)mbcs->carriers.size)
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
    com_ericsson_otp,
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
        ctf_float(float, mbc_ratio, (float)mbcs->blocks.size/(float)mbcs->carriers.size)
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
    com_ericsson_otp,
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
    com_ericsson_otp,
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
