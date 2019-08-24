/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2006-2016. All Rights Reserved.
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

#ifndef ERL_MISC_UTILS_H_
#define ERL_MISC_UTILS_H_

#include "erl_errno.h"

typedef struct erts_cpu_info_t_ erts_cpu_info_t;
typedef struct {
    int node;
    int processor;
    int processor_node;
    int core;
    int thread;
    int logical;
} erts_cpu_topology_t;

erts_cpu_info_t *erts_cpu_info_create(void);
void erts_cpu_info_destroy(erts_cpu_info_t *cpuinfo);
int erts_cpu_info_update(erts_cpu_info_t *cpuinfo);
int erts_get_cpu_configured(erts_cpu_info_t *cpuinfo);
int erts_get_cpu_online(erts_cpu_info_t *cpuinfo);
int erts_get_cpu_available(erts_cpu_info_t *cpuinfo);
char *erts_get_unbind_from_cpu_str(erts_cpu_info_t *cpuinfo);
int erts_get_available_cpu(erts_cpu_info_t *cpuinfo, int no);
int erts_get_cpu_topology_size(erts_cpu_info_t *cpuinfo);
int erts_get_cpu_topology(erts_cpu_info_t *cpuinfo,
			  erts_cpu_topology_t *topology);
int erts_is_cpu_available(erts_cpu_info_t *cpuinfo, int id);
int erts_bind_to_cpu(erts_cpu_info_t *cpuinfo, int cpu);
int erts_unbind_from_cpu(erts_cpu_info_t *cpuinfo);
int erts_unbind_from_cpu_str(char *str);

int erts_milli_sleep(long);

#ifdef __WIN32__
int erts_map_win_error_to_errno(DWORD win_error);
int erts_get_last_win_errno(void);
#endif

#if defined(__APPLE__) && defined(__MACH__) && !defined(__DARWIN__)
#define __DARWIN__ 1
#endif

/*
 * ERTS_PREMATURE_TIMEOUT() expects time units
 * 1000 (millisec), 1000000 (microsec), or
 * 1000000000 (nanosec). Might not work properly
 * otherwise.
 */
#undef ERTS_USE_PREMATURE_TIMEOUT
#undef ERTS_PREMATURE_TIMEOUT

#if defined(__DARWIN__)
#define ERTS_USE_PREMATURE_TIMEOUT 1
#define ERTS_PREMATURE_TIMEOUT(TMO, TU)		\
    ((TMO) >= 1 * ((TU) / 1000)			\
     ? ((TMO) >= 20 * ((TU) / 1000)		\
	? 15 * ((TU) / 1000)			\
	: ((TMO) >= 5 * ((TU) / 1000)		\
	   ? 3 * ((TU) / 1000)			\
	   : 5 * ((TU) / 10000)))		\
     : 0)

#else
#define ERTS_USE_PREMATURE_TIMEOUT 0
#define ERTS_PREMATURE_TIMEOUT(TMO, TU) (0)
#endif

#endif /* #ifndef ERL_MISC_UTILS_H_ */
