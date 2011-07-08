/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2010-2011. All Rights Reserved.
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

/* implemented in lib_src/common/ethread_aux.c */
int ethr_init_common__(ethr_init_data *id);
int ethr_late_init_common__(ethr_late_init_data *lid);
void ethr_run_exit_handlers__(void);
void ethr_ts_event_destructor__(void *vtsep);

#if defined(ETHR_X86_RUNTIME_CONF__)
int ethr_x86_have_cpuid__(void);
void ethr_x86_cpuid__(int *eax, int *ebx, int *ecx, int *edx);
#endif

#endif
