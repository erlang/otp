/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2000-2009. All Rights Reserved.
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



/*
** Common struct to all bp_data_*
**
** Two gotchas: 
**
** 1) The type of bp_data structure in the ring is deduced from the 
**    orig_instr field of the structure _before_ in the ring, except for 
**    the first structure in the ring that has its instruction in
**    pc[0] of the code to execute.
**
** 2) pc[-4] points to the _last_ structure in the ring before the
**    breakpoints are being executed.
**
** So, as an example, when a breakpointed function starts to execute,
** the first instruction that is a breakpoint instruction at pc[0] finds
** its data at ((BpData *) pc[-4])->next and has to cast that pointer 
** to the correct bp_data type.
*/
typedef struct bp_data {
    struct bp_data *next; /* Doubly linked ring pointers */
    struct bp_data *prev; /* -"-                         */
    Uint orig_instr;      /* The original instruction to execute */
} BpData;
/*
** All the following bp_data_.. structs must begin the same way
*/

typedef struct bp_data_trace {
    struct bp_data *next;
    struct bp_data *prev;
    Uint            orig_instr;
    Binary         *match_spec;
    Eterm           tracer_pid;
} BpDataTrace;

typedef struct bp_data_debug {
    struct bp_data *next;
    struct bp_data *prev;
    Uint            orig_instr;
} BpDataDebug;

typedef struct bp_data_count { /* Call count */
    struct bp_data *next;
    struct bp_data *prev;
    Uint            orig_instr;
    Sint            count;
} BpDataCount;

extern erts_smp_spinlock_t erts_bp_lock;

#ifdef ERTS_SMP
#define ErtsSmpBPLock(BDC) erts_smp_spin_lock(&erts_bp_lock)
#define ErtsSmpBPUnlock(BDC) erts_smp_spin_unlock(&erts_bp_lock)
#else
#define ErtsSmpBPLock(BDC)
#define ErtsSmpBPUnlock(BDC)
#endif

#define ErtsCountBreak(pc,instr_result)                     \
do {                                                        \
    BpDataCount *bdc = (BpDataCount *) (pc)[-4];            \
                                                            \
    ASSERT((pc)[-5] == (Uint) BeamOp(op_i_func_info_IaaI)); \
    ASSERT(bdc);                                            \
    bdc = (BpDataCount *) bdc->next;                        \
    ASSERT(bdc);                                            \
    (pc)[-4] = (Uint) bdc;                                  \
    ErtsSmpBPLock(bdc);                                     \
    if (bdc->count >= 0) bdc->count++;                      \
    ErtsSmpBPUnlock(bdc);                                   \
    *(instr_result) = bdc->orig_instr;                      \
} while (0)

#define ErtsBreakSkip(pc,instr_result)                      \
do {                                                        \
    BpData *bd = (BpData *) (pc)[-4];                       \
                                                            \
    ASSERT((pc)[-5] == (Uint) BeamOp(op_i_func_info_IaaI)); \
    ASSERT(bd);                                             \
    bd = bd->next;                                          \
    ASSERT(bd);                                             \
    (pc)[-4] = (Uint) bd;                                   \
    *(instr_result) = bd->orig_instr;                       \
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
void erts_set_mtrace_bif(Uint *pc, Binary *match_spec, 
			 Eterm tracer_pid);
void erts_clear_mtrace_bif(Uint *pc);
int erts_set_debug_break(Eterm mfa[3], int specified);
int erts_clear_debug_break(Eterm mfa[3], int specified);
int erts_set_count_break(Eterm mfa[3], int specified, enum erts_break_op);
int erts_clear_count_break(Eterm mfa[3], int specified);


int erts_clear_break(Eterm mfa[3], int specified);
int erts_clear_module_break(Module *modp);
int erts_clear_function_break(Module *modp, Uint *pc);

Uint erts_trace_break(Process *p, Uint *pc, Eterm *args, 
		      Uint32 *ret_flags, Eterm *tracer_pid);
Uint32 erts_bif_mtrace(Process *p, Uint *pc, Eterm *args, 
		       int local, Eterm *tracer_pid);

int erts_is_trace_break(Uint *pc, Binary **match_spec_ret, 
			Eterm *tracer_pid_ret);
int erts_is_mtrace_break(Uint *pc, Binary **match_spec_ret, 
			 Eterm *tracer_pid_rte);
int erts_is_mtrace_bif(Uint *pc, Binary **match_spec_ret, 
		       Eterm *tracer_pid_ret);
int erts_is_native_break(Uint *pc);
int erts_is_count_break(Uint *pc, Sint *count_ret);

Uint *erts_find_local_func(Eterm mfa[3]);

#endif /* _BEAM_BP_H */
