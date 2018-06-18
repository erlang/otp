/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2015-2018. All Rights Reserved.
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

#ifndef ERL_OS_MONOTONIC_TIME_EXTENDER_H__
#define ERL_OS_MONOTONIC_TIME_EXTENDER_H__

#include "sys.h"
#include "erl_threads.h"

typedef struct {
    Uint32 (*raw_os_monotonic_time)(void);
    erts_atomic32_t extend[2];
    int check_interval;
} ErtsOsMonotonicTimeExtendState;

#  define ERTS_EXTEND_OS_MONOTONIC_TIME(S, RT)				\
    ((((ErtsMonotonicTime)						\
       erts_atomic32_read_nob(&((S)->extend[((int) ((RT) >> 31)) & 1]))) \
      << 32)								\
     + (RT))

void
erts_init_os_monotonic_time_extender(ErtsOsMonotonicTimeExtendState *statep,
				     Uint32 (*raw_os_monotonic_time)(void),
				     int check_seconds);
void
erts_late_init_os_monotonic_time_extender(ErtsOsMonotonicTimeExtendState *statep);

#endif
