/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2015. All Rights Reserved.
 * 
 * The contents of this file are subject to the Erlang Public License,
 * Version 1.1, (the "License"); you may not use this file except in
 * compliance with the License. You should have received a copy of the
 * Erlang Public License along with this software. If not, it can be
 * retrieved online at http://www.erlang.org/.
 * 
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
 * the License for the specific language governing rights and limitations
 * under the License.
 * 
 * %CopyrightEnd%
 */

#ifndef ERL_OS_MONOTONIC_TIME_EXTENDER_H__
#define ERL_OS_MONOTONIC_TIME_EXTENDER_H__

#include "sys.h"
#include "erl_threads.h"

typedef struct {
#ifdef USE_THREADS
    Uint32 (*raw_os_monotonic_time)(void);
    erts_atomic32_t extend[2];
    int check_interval;
#else
    Uint32 extend[2];
    ErtsMonotonicTime last_msb;
#endif
} ErtsOsMonotonicTimeExtendState;

#ifdef USE_THREADS
#  define ERTS_CHK_EXTEND_OS_MONOTONIC_TIME(S, RT) ((void) 1)
#  define ERTS_EXTEND_OS_MONOTONIC_TIME(S, RT)				\
    ((((ErtsMonotonicTime)						\
       erts_atomic32_read_nob(&((S)->extend[((int) ((RT) >> 31)) & 1]))) \
      << 32)								\
     + (RT))
#else
#  define ERTS_CHK_EXTEND_OS_MONOTONIC_TIME(S, RT)			\
    do {								\
	Uint32 msb__ = (RT) & (((Uint32) 1) << 31);			\
	if (msb__ != (S)->last_msb) {					\
	    int ix__ = ((int) ((S)->last_msb >> 31)) & 1;		\
	    (S)->extend[ix__]++;					\
	    (S)->last_msb = msb;					\
	}								\
    } while (0)
#  define ERTS_EXTEND_OS_MONOTONIC_TIME(S, RT)				\
    ((((ErtsMonotonicTime) (S)->extend[((int) ((RT) >> 31)) & 1]) << 32) + (RT))
#endif

void
erts_init_os_monotonic_time_extender(ErtsOsMonotonicTimeExtendState *statep,
				     Uint32 (*raw_os_monotonic_time)(void),
				     int check_seconds);
void
erts_late_init_os_monotonic_time_extender(ErtsOsMonotonicTimeExtendState *statep);

#endif
