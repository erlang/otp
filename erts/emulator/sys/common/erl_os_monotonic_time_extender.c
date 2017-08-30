/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2015-2016. All Rights Reserved.
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

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif
#include "erl_os_monotonic_time_extender.h"

#ifdef USE_THREADS

static void *os_monotonic_time_extender(void *vstatep)
{
    ErtsOsMonotonicTimeExtendState *state = (ErtsOsMonotonicTimeExtendState *) vstatep;
    long sleep_time = state->check_interval*1000;
    Uint32 (*raw_os_mtime)(void) = state->raw_os_monotonic_time;
    Uint32 last_msb = 0;

    while (1) {
	Uint32 msb = (*raw_os_mtime)() & (((Uint32) 1) << 31);

	if (msb != last_msb) {
	    int ix = ((int) (last_msb >> 31)) & 1;
	    Uint32 xtnd = (Uint32) erts_atomic32_read_nob(&state->extend[ix]);
	    erts_atomic32_set_nob(&state->extend[ix], (erts_aint32_t) (xtnd + 1));
	    last_msb = msb;
	}
	erts_milli_sleep(sleep_time);
    }

    erts_exit(ERTS_ABORT_EXIT, "os_monotonic_time_extender thread terminating");
    return NULL;
}

static erts_tid_t os_monotonic_extender_tid;
#endif

void
erts_init_os_monotonic_time_extender(ErtsOsMonotonicTimeExtendState *statep,
				     Uint32 (*raw_os_monotonic_time)(void),
				     int check_seconds)
{
#ifdef USE_THREADS
    statep->raw_os_monotonic_time = raw_os_monotonic_time;
    erts_atomic32_init_nob(&statep->extend[0], (erts_aint32_t) 0);
    erts_atomic32_init_nob(&statep->extend[1], (erts_aint32_t) 0);
    statep->check_interval = check_seconds;

#else
    statep->extend[0] = (Uint32) 0;
    statep->extend[1] = (Uint32) 0;
    statep->last_msb = (ErtsMonotonicTime) 0;
#endif
}

void
erts_late_init_os_monotonic_time_extender(ErtsOsMonotonicTimeExtendState *statep)
{
#ifdef USE_THREADS
    erts_thr_opts_t thr_opts = ERTS_THR_OPTS_DEFAULT_INITER;
    thr_opts.detached = 1;
    thr_opts.suggested_stack_size = 4;

#if 0
    thr_opts.name = "os_monotonic_time_extender";
#endif

    erts_thr_create(&os_monotonic_extender_tid,
		    os_monotonic_time_extender,
		    (void*) statep,
		    &thr_opts);
#endif
}
