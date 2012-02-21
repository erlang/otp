/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2000-2011. All Rights Reserved.
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


#ifndef _BEAM_BP_H
#define _BEAM_BP_H

#include "sys.h"
#include "erl_vm.h"
#include "global.h"

typedef struct {
    Eterm pid;
    Sint  count;
    Uint  s_time;
    Uint  us_time;
} bp_data_time_item_t;

typedef struct {
    Uint n;
    Uint used;
    bp_data_time_item_t *item;
} bp_time_hash_t;

typedef struct bp_data_time {     /* Call time */
    Uint n;
    bp_time_hash_t *hash;
    erts_refc_t refc;
} BpDataTime;

typedef struct {
    Uint ms;
    Uint s;
    Uint us;
    BeamInstr *pc;
} process_breakpoint_time_t; /* used within psd */

typedef struct {
    erts_smp_atomic_t acount;
    erts_refc_t refc;
} BpCount;

typedef struct generic_bp_data {
    Uint flags;
    Binary* local_ms;		/* Match spec for local call trace */
    Binary* meta_ms;		/* Match spec for meta trace */
    erts_smp_atomic_t tracer_pid; /* For meta trace */
    BpCount* count;		  /* For call count */
    BpDataTime* time;		  /* For time trace */
} GenericBpData;

typedef struct generic_bp {
    BeamInstr orig_instr;
    GenericBpData data[1];
} GenericBp;

#define ERTS_BP_CALL_TIME_SCHEDULE_IN      (0)
#define ERTS_BP_CALL_TIME_SCHEDULE_OUT     (1)
#define ERTS_BP_CALL_TIME_SCHEDULE_EXITING (2)

#ifdef ERTS_SMP
#define bp_sched2ix_proc(p) ((p)->scheduler_data->no - 1)
#else
#define bp_sched2ix_proc(p) (0)
#endif

enum erts_break_op{
    erts_break_nop   =  0, /* Must be false */
    erts_break_set   = !0, /* Must be true */
    erts_break_reset,
    erts_break_stop
};



/*
** Function interface exported from beam_bp.c
*/

void erts_bp_init(void);

int erts_set_trace_break(Eterm mfa[3], int specified, Binary *match_spec);
int erts_clear_trace_break(Eterm mfa[3], int specified);
int erts_set_mtrace_break(Eterm mfa[3], int specified, Binary *match_spec,
			  Eterm tracer_pid);
int erts_clear_mtrace_break(Eterm mfa[3], int specified);
void erts_set_mtrace_bif(BeamInstr *pc, Binary *match_spec,
			 Eterm tracer_pid);
void erts_clear_mtrace_bif(BeamInstr *pc);
int erts_set_debug_break(Eterm mfa[3], int specified);
int erts_clear_debug_break(Eterm mfa[3], int specified);
int erts_set_count_break(Eterm mfa[3], int specified, enum erts_break_op);
int erts_clear_count_break(Eterm mfa[3], int specified);


int erts_clear_break(Eterm mfa[3], int specified);
int erts_clear_module_break(Module *modp);
int erts_clear_function_break(Module *modp, BeamInstr *pc);

BeamInstr erts_generic_breakpoint(Process* c_p, BeamInstr* I, Eterm* reg);
BeamInstr erts_trace_break(Process *p, BeamInstr *pc, Eterm *args,
		      Uint32 *ret_flags, Eterm *tracer_pid);
Uint32 erts_bif_mtrace(Process *p, BeamInstr *pc, Eterm *args,
		       int local, Eterm *tracer_pid);

int erts_is_trace_break(BeamInstr *pc, Binary **match_spec_ret);
int erts_is_mtrace_break(BeamInstr *pc, Binary **match_spec_ret,
			 Eterm *tracer_pid_rte);
int erts_is_mtrace_bif(BeamInstr *pc, Binary **match_spec_ret,
		       Eterm *tracer_pid_ret);
int erts_is_native_break(BeamInstr *pc);
int erts_is_count_break(BeamInstr *pc, Uint *count_ret);
int erts_is_time_break(Process *p, BeamInstr *pc, Eterm *call_time);

void erts_trace_time_call(Process* c_p, BeamInstr* pc, BpDataTime* bdt);
void erts_trace_time_return(Process* c_p, BeamInstr* pc);
void erts_schedule_time_break(Process *p, Uint out);
int erts_set_time_break(Eterm mfa[3], int specified, enum erts_break_op);
int erts_clear_time_break(Eterm mfa[3], int specified);

int erts_is_time_trace_bif(Process *p, BeamInstr *pc, Eterm *call_time);
void erts_set_time_trace_bif(BeamInstr *pc, enum erts_break_op);
void erts_clear_time_trace_bif(BeamInstr *pc);
BpDataTime* erts_get_active_time_break(BeamInstr *pc);

BeamInstr *erts_find_local_func(Eterm mfa[3]);

ERTS_GLB_INLINE Uint erts_bp_sched2ix(void);

#if ERTS_GLB_INLINE_INCL_FUNC_DEF
ERTS_GLB_INLINE Uint erts_bp_sched2ix(void)
{
#ifdef ERTS_SMP
    ErtsSchedulerData *esdp;
    esdp = erts_get_scheduler_data();
    return esdp->no - 1;
#else
    return 0;
#endif
}
#endif

#endif /* _BEAM_BP_H */
