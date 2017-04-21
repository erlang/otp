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

#ifndef ERL_HL_TIMER_H__
#define ERL_HL_TIMER_H__

typedef struct ErtsBifTimer_ ErtsBifTimers;
typedef struct ErtsHLTimerService_ ErtsHLTimerService;

#include "sys.h"
#include "erl_process.h"
#define ERL_PORT_GET_PORT_TYPE_ONLY__
#include "erl_port.h"
#undef ERL_PORT_GET_PORT_TYPE_ONLY__
#include "erl_message.h"
#include "erl_alloc_types.h"

#define ERTS_PTMR_NONE ((erts_aint_t) NULL)
#define ERTS_PTMR_TIMEDOUT (ERTS_PTMR_NONE + ((erts_aint_t) 1))

#define ERTS_PTMR_INIT(P) \
    erts_smp_atomic_init_nob(&(P)->common.timer, ERTS_PTMR_NONE)
#define ERTS_PTMR_IS_SET(P) \
    (ERTS_PTMR_NONE != erts_smp_atomic_read_nob(&(P)->common.timer))
#define ERTS_PTMR_IS_TIMED_OUT(P) \
    (ERTS_PTMR_TIMEDOUT == erts_smp_atomic_read_nob(&(P)->common.timer))

#define ERTS_PTMR_CLEAR(P)					\
    do {							\
	ASSERT(ERTS_PTMR_IS_TIMED_OUT((P)));			\
	erts_smp_atomic_set_nob(&(P)->common.timer,		\
				ERTS_PTMR_NONE);		\
    } while (0)

size_t erts_timer_type_size(ErtsAlcType_t type);
int erts_set_proc_timer_term(Process *, Eterm);
void erts_set_proc_timer_uword(Process *, UWord);
void erts_cancel_proc_timer(Process *);
void erts_set_port_timer(Port *, Sint64);
void erts_cancel_port_timer(Port *);
Sint64 erts_read_port_timer(Port *);
int erts_cancel_bif_timers(Process *, ErtsBifTimers **, void **);
int erts_detach_accessor_bif_timers(Process *, ErtsBifTimers *, void **);
ErtsHLTimerService *erts_create_timer_service(void);
void erts_hl_timer_init(void);
void erts_start_timer_callback(ErtsMonotonicTime,
			       void (*)(void *),
			       void *);
#ifdef ERTS_SMP
void
erts_handle_canceled_timers(void *vesdp,
			    int *need_thr_progress,
			    ErtsThrPrgrVal *thr_prgr_p,
			    int *need_more_work);
#endif

Uint erts_bif_timer_memory_size(void);
void erts_print_bif_timer_info(fmtfn_t to, void *to_arg);

void erts_debug_bif_timer_foreach(void (*func)(Eterm,
					       Eterm,
					       ErlHeapFragment *,
					       void *),
				  void *arg);
void
erts_debug_callback_timer_foreach(void (*tclbk)(void *),
				  void (*func)(void *,
					       ErtsMonotonicTime,
					       void *),
				  void *arg);
#endif /* ERL_HL_TIMER_H__ */
