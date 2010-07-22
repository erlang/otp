/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2000-2010. All Rights Reserved.
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



/* A couple of gotchas:
 *
 * The breakpoint structure from BeamInstr,
 * In beam_emu where the instruction counter pointer, I (or pc),
 * points to the *current* instruction. At that time, if the instruction
 * is a breakpoint instruction the pc looks like the following,
 *
 * I[-5]   | op_i_func_info_IaaI |        scheduler specific entries
 * I[-4]   |    BpData** bpa     |  --> | BpData * bdas1 | ... | BpData * bdasN |
 * I[-3]   |  Tagged Module      |            |                    |
 * I[-2]   |  Tagged Function    |            V                    V
 * I[-1]   |  Arity              |          BpData -> BpData -> BpData -> BpData
 * I[0]    | The bp instruction  |            ^       * the bp wheel *      |
 *                                            |------------------------------
 *
 * Common struct to all bp_data_*
 *
 * 1) The type of bp_data structure in the ring is deduced from the
 *    orig_instr field of the structure _before_ in the ring, except for
 *    the first structure in the ring that has its instruction in
 *    pc[0] of the code to execute.
 *    This is valid as long as you don't search for the function while it is
 *    being executed by something else. Or is in the middle of its rotation for
 *    any other reason.
 *    A key, the bp beam instruction, is included for this reason.
 *
 * 2) pc[-4][sched_id - 1] points to the _last_ structure in the ring before the
 *    breakpoints are being executed.
 *
 * So, as an example, when a breakpointed function starts to execute,
 * the first instruction that is a breakpoint instruction at pc[0] finds
 * its data at ((BpData **) pc[-4][sched_id - 1])->next and has to cast that pointer
 * to the correct bp_data type.
*/

typedef struct bp_data {
    struct bp_data *next; /* Doubly linked ring pointers */
    struct bp_data *prev; /* -"-                         */
    BeamInstr orig_instr;      /* The original instruction to execute */
    BeamInstr this_instr;      /* key */
} BpData;
/*
** All the following bp_data_.. structs must begin the same way
*/

typedef struct bp_data_trace {
    struct bp_data *next;
    struct bp_data *prev;
    BeamInstr       orig_instr;
    BeamInstr       this_instr;   /* key */
    Binary         *match_spec;
    Eterm           tracer_pid;
} BpDataTrace;

typedef struct bp_data_debug {
    struct bp_data *next;
    struct bp_data *prev;
    BeamInstr       orig_instr;
    BeamInstr       this_instr;   /* key */
} BpDataDebug;

typedef struct bp_data_count {    /* Call count */
    struct bp_data *next;
    struct bp_data *prev;
    BeamInstr       orig_instr;
    BeamInstr       this_instr;   /* key */
    erts_smp_atomic_t acount;
} BpDataCount;

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
    struct bp_data *next;
    struct bp_data *prev;
    BeamInstr       orig_instr;
    BeamInstr       this_instr;   /* key */
    Uint	    pause;
    Uint	    n;
    bp_time_hash_t  *hash;
} BpDataTime;

typedef struct {
    Uint ms;
    Uint s;
    Uint us;
    BeamInstr *pc;
} process_breakpoint_time_t; /* used within psd */

extern erts_smp_spinlock_t erts_bp_lock;

#define ERTS_BP_CALL_TIME_SCHEDULE_IN      (0)
#define ERTS_BP_CALL_TIME_SCHEDULE_OUT     (1)
#define ERTS_BP_CALL_TIME_SCHEDULE_EXITING (2)

#define ERTS_BP_CALL_TIME_CALL      (0)
#define ERTS_BP_CALL_TIME_RETURN    (1)
#define ERTS_BP_CALL_TIME_TAIL_CALL (2)

#ifdef ERTS_SMP
#define ErtsSmpBPLock(BDC) erts_smp_spin_lock(&erts_bp_lock)
#define ErtsSmpBPUnlock(BDC) erts_smp_spin_unlock(&erts_bp_lock)
#else
#define ErtsSmpBPLock(BDC)
#define ErtsSmpBPUnlock(BDC)
#endif

ERTS_INLINE Uint bp_sched2ix(void);

#ifdef ERTS_SMP
#define bp_sched2ix_proc(p) ((p)->scheduler_data->no - 1)
#else
#define bp_sched2ix_proc(p) (0)
#endif

#define ErtsCountBreak(p, pc,instr_result)                       \
do {                                                             \
    BpData **bds = (BpData **) (pc)[-4];                         \
    BpDataCount *bdc = NULL;                                     \
    Uint ix = bp_sched2ix_proc( (p) );                           \
    long count = 0;                                              \
                                                                 \
    ASSERT((pc)[-5] == (BeamInstr) BeamOp(op_i_func_info_IaaI)); \
    ASSERT(bds);                                                 \
    bdc = (BpDataCount *) bds[ix];                               \
    bdc = (BpDataCount *) bdc->next;                             \
    ASSERT(bdc);                                                 \
    bds[ix] = (BpData *) bdc;                                    \
    count = erts_smp_atomic_read(&bdc->acount);                  \
    if (count >= 0)  erts_smp_atomic_inc(&bdc->acount);          \
    *(instr_result) = bdc->orig_instr;                           \
} while (0)

#define ErtsBreakSkip(p, pc,instr_result)                        \
do {                                                             \
    BpData **bds = (BpData **) (pc)[-4];                         \
    BpData *bd = NULL;                                           \
    Uint ix = bp_sched2ix_proc( (p) );                           \
                                                                 \
    ASSERT((pc)[-5] == (BeamInstr) BeamOp(op_i_func_info_IaaI)); \
    ASSERT(bds);                                                 \
    bd = bds[ix];                                                \
    ASSERT(bd);                                                  \
    bd = bd->next;                                               \
    ASSERT(bd);                                                  \
    bds[ix] = bd;                                                \
    *(instr_result) = bd->orig_instr;                            \
} while (0)

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

int erts_set_trace_break(Eterm mfa[3], int specified, Binary *match_spec,
			 Eterm tracer_pid);
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

BeamInstr erts_trace_break(Process *p, BeamInstr *pc, Eterm *args,
		      Uint32 *ret_flags, Eterm *tracer_pid);
Uint32 erts_bif_mtrace(Process *p, BeamInstr *pc, Eterm *args,
		       int local, Eterm *tracer_pid);

int erts_is_trace_break(BeamInstr *pc, Binary **match_spec_ret,
			Eterm *tracer_pid_ret);
int erts_is_mtrace_break(BeamInstr *pc, Binary **match_spec_ret,
			 Eterm *tracer_pid_rte);
int erts_is_mtrace_bif(BeamInstr *pc, Binary **match_spec_ret,
		       Eterm *tracer_pid_ret);
int erts_is_native_break(BeamInstr *pc);
int erts_is_count_break(BeamInstr *pc, Sint *count_ret);
int erts_is_time_break(Process *p, BeamInstr *pc, Eterm *call_time);

void erts_trace_time_break(Process *p, BeamInstr *pc, BpDataTime *bdt, Uint type);
void erts_schedule_time_break(Process *p, Uint out);
int erts_set_time_break(Eterm mfa[3], int specified, enum erts_break_op);
int erts_clear_time_break(Eterm mfa[3], int specified);

int erts_is_time_trace_bif(Process *p, BeamInstr *pc, Eterm *call_time);
void erts_set_time_trace_bif(BeamInstr *pc, enum erts_break_op);
void erts_clear_time_trace_bif(BeamInstr *pc);
BpData *erts_get_time_break(Process *p, BeamInstr *pc);

BeamInstr *erts_find_local_func(Eterm mfa[3]);

#endif /* _BEAM_BP_H */
