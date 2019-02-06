/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2010-2017. All Rights Reserved.
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

/*
 * Description: Internal ethread exports
 * Author: Rickard Green
 */

#ifndef ETHR_INTERNAL_H__
#define ETHR_INTERNAL_H__

#include "erl_misc_utils.h"

extern ethr_memory_allocators ethr_mem__;
extern erts_cpu_info_t *ethr_cpu_info__;
extern size_t ethr_pagesize__;
extern size_t ethr_min_stack_size__; /* kilo words */
extern size_t ethr_max_stack_size__; /* kilo words */
extern int ethr_not_completely_inited__;
extern int ethr_not_inited__;

extern void *(*ethr_thr_prepare_func__)(void);
extern void (*ethr_thr_parent_func__)(void *);
extern void (*ethr_thr_child_func__)(void *);

#define ETHR_PAGE_ALIGN(SZ) \
  (((((size_t) (SZ)) - 1)/ethr_pagesize__ + 1)*ethr_pagesize__)
#define ETHR_B2KW(B) ((((size_t) (B)) - 1)/(sizeof(void *)*1024) + 1)
#define ETHR_KW2B(KW) (((size_t) (KW))*sizeof(void *)*1024)

#undef ETHR_STACK_GUARD_SIZE 
#ifdef ETHR_HAVE_PTHREAD_ATTR_SETGUARDSIZE
#  define ETHR_STACK_GUARD_SIZE (ethr_pagesize__)
#endif

/* implemented in lib_src/<thr-lib>/ethread.c */
int ethr_set_tse__(ethr_ts_event *tsep);
ethr_ts_event *ethr_get_tse__(void);
ETHR_PROTO_NORETURN__ ethr_abort__(void);
#ifdef ETHR_WIN32_THREADS
int ethr_win_get_errno__(void);
#endif

#ifdef ETHR_INCLUDE_MONOTONIC_CLOCK__
#undef ETHR_HAVE_ETHR_GET_MONOTONIC_TIME
#if defined(ETHR_HAVE_CLOCK_GETTIME_MONOTONIC)		\
    || defined(ETHR_HAVE_MACH_CLOCK_GET_TIME)		\
    || defined(ETHR_HAVE_GETHRTIME)
#ifdef ETHR_TIME_WITH_SYS_TIME
#  include <time.h>
#  include <sys/time.h>
#else
#  ifdef ETHR_HAVE_SYS_TIME_H
#    include <sys/time.h>
#  else
#    include <time.h>
#  endif
#endif
#ifdef ETHR_HAVE_MACH_CLOCK_GET_TIME
#include <mach/clock.h>
#include <mach/mach.h>
#endif
#define ETHR_HAVE_ETHR_GET_MONOTONIC_TIME
ethr_sint64_t ethr_get_monotonic_time(void);
int ethr_get_monotonic_time_is_broken(void);
#endif
#endif /* ETHR_INCLUDE_MONOTONIC_CLOCK__ */

void ethr_init_event__(void);

/* implemented in lib_src/common/ethread_aux.c */
int ethr_init_common__(ethr_init_data *id);
int ethr_late_init_common__(ethr_late_init_data *lid);
void ethr_run_exit_handlers__(void);
void ethr_ts_event_destructor__(void *vtsep);
void ethr_set_stacklimit__(char *prev_c, size_t stacksize);

#if defined(ETHR_X86_RUNTIME_CONF__)
void ethr_x86_cpuid__(int *eax, int *ebx, int *ecx, int *edx);
#endif

#endif
