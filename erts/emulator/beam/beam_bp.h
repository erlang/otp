/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2000-2024. All Rights Reserved.
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


#ifndef _BEAM_BP_H
#define _BEAM_BP_H

#include "sys.h"
#include "erl_vm.h"
#include "global.h"

/* Use the same signed long long for both call_time and call_memory.
* There is not much value in future-proofing until there is a need
* to support anything other than a simple 8-byte number. When such
* a use-case is identified, this type could be turned into a union.
*/
typedef ErtsMonotonicTime BpDataAccumulator;

typedef struct {
    Eterm pid;
    Sint  count;
    BpDataAccumulator accumulator;
} bp_data_trace_item_t;

typedef struct {
    Uint n;
    Uint used;
    bp_data_trace_item_t *item;
} bp_trace_hash_t;

typedef struct bp_data_time {     /* Call time, Memory trace */
    Uint n;
    bp_trace_hash_t *hash;
    erts_refc_t refc;
} BpDataCallTrace;

typedef struct process_breakpoint_trace_t {
    struct process_breakpoint_trace_t *next;
    ErtsTraceSession *session;

    const ErtsCodeInfo *ci;
    BpDataAccumulator accumulator;
    BpDataAccumulator allocated;        /* adjustment for GC and messages on the heap */
} process_breakpoint_trace_t; /* used within psd */

typedef struct {
    erts_atomic_t acount;
    erts_refc_t refc;
} BpCount;

typedef struct {
    erts_atomic_t tracer;
    erts_refc_t refc;
} BpMetaTracer;

typedef struct GenericBpData {
    Uint flags;
    Binary* local_ms;		/* Match spec for local call trace */
    Binary* meta_ms;		/* Match spec for meta trace */
    BpMetaTracer* meta_tracer;	/* Meta tracer */
    BpCount* count;		/* For call count */
    BpDataCallTrace* time;	/* For time trace */
    BpDataCallTrace* memory;	/* For memory trace */
} GenericBpData;

#define ERTS_NUM_BP_IX 2

typedef struct GenericBp {
    BeamInstr orig_instr;
    GenericBpData data[ERTS_NUM_BP_IX];
    
    ErtsTraceSession *session;
    struct GenericBp *next;
    // ToDo make union
    struct GenericBp *next_to_free;
    struct GenericBp *to_insert;
} GenericBp;

extern ErtsTraceSession* erts_staging_trace_session;

#define ERTS_BP_CALL_TIME_SCHEDULE_IN      (0)
#define ERTS_BP_CALL_TIME_SCHEDULE_OUT     (1)
#define ERTS_BP_CALL_TIME_SCHEDULE_EXITING (2)

extern erts_mtx_t erts_dirty_bp_ix_mtx;

enum erts_break_op{
    ERTS_BREAK_NOP   =  0, /* Must be false */
    ERTS_BREAK_SET   = !0, /* Must be true */
    ERTS_BREAK_RESTART,
    ERTS_BREAK_PAUSE
};

typedef Uint32 ErtsBpIndex;

typedef struct {
    const ErtsCodeInfo *code_info;
    Module* mod;
} BpFunction;

typedef struct {
    Uint matched;		/* Number matched */
    BpFunction* matching;	/* Matching functions */
} BpFunctions;

/*
** Function interface exported from beam_bp.c
*/

void erts_bp_init(void);

void erts_prepare_bp_staging(void);
void erts_commit_staged_bp(void);

ERTS_GLB_INLINE ErtsBpIndex erts_active_bp_ix(void);
ERTS_GLB_INLINE ErtsBpIndex erts_staging_bp_ix(void);

void erts_bp_match_functions(BpFunctions* f, ErtsCodeMFA *mfa, int specified);
void erts_bp_match_export(BpFunctions* f, ErtsCodeMFA *mfa, int specified);
void erts_bp_free_matched_functions(BpFunctions* f);

void erts_install_breakpoints(BpFunctions* f);
void erts_install_additional_session_bp(ErtsCodeInfo* ci_rw);
Uint erts_sum_all_session_flags(ErtsCodeInfo *ci_rw);
void erts_uninstall_breakpoints(BpFunctions* f);

void erts_consolidate_local_bp_data(BpFunctions* f);
void erts_consolidate_export_bp_data(BpFunctions* f);
void erts_free_breakpoints(void);

void erts_set_trace_break(BpFunctions *f, Binary *match_spec);
void erts_clear_trace_break(BpFunctions *f);

void erts_set_export_trace(ErtsCodeInfo *ci, Binary *match_spec);
void erts_clear_export_trace(ErtsCodeInfo *ci);

void erts_set_mtrace_break(BpFunctions *f, Binary *match_spec, ErtsTracer tracer);
void erts_clear_mtrace_break(BpFunctions *f);

void erts_set_debug_break(BpFunctions *f);
void erts_clear_debug_break(BpFunctions *f);
void erts_set_count_break(BpFunctions *f,
                          enum erts_break_op);
void erts_clear_count_break(BpFunctions *f);


void erts_clear_all_breaks(BpFunctions* f);
int erts_clear_module_break(Module *modp);
void erts_clear_all_export_break(Module *modp, Export *ep);

BeamInstr erts_generic_breakpoint(Process* c_p, ErtsCodeInfo *ci, Eterm* reg);
BeamInstr erts_trace_break(Process *p, ErtsCodeInfo *ci, Eterm *args,
                           Uint32 *ret_flags, ErtsTracer *tracer);

int erts_is_trace_break(ErtsTraceSession *session, const ErtsCodeInfo *ci,
                        Binary **match_spec_ret, int local);
int erts_is_mtrace_break(ErtsTraceSession *session, const ErtsCodeInfo *ci,
                         Binary **match_spec_ret, ErtsTracer *tracer_ret);

int erts_is_count_break(ErtsTraceSession *session, const ErtsCodeInfo *ci,
                        Uint *count_ret);
int erts_is_call_break(Process *p, ErtsTraceSession *session, int is_time,
                       const ErtsCodeInfo *ci, Eterm *call_time);

void erts_call_trace_return(Process* c_p, const ErtsCodeInfo *ci,
                            Eterm bp_flags_term, Eterm session_weak_id);
void erts_schedule_time_break(Process *p, Uint out);
ERTS_GLB_INLINE void erts_adjust_memory_break(Process *p, Sint adjustment);
ERTS_GLB_INLINE void erts_adjust_message_break(Process *p, Eterm message);
void erts_set_time_break(BpFunctions *f, enum erts_break_op);
void erts_set_memory_break(BpFunctions *f, enum erts_break_op);
void erts_clear_time_break(BpFunctions *f);
void erts_clear_memory_break(BpFunctions *f);
Eterm erts_make_bp_session_list(ErtsHeapFactory*, const ErtsCodeInfo*,
                                Eterm tail);

const ErtsCodeInfo *erts_find_local_func(const ErtsCodeMFA *mfa);

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

extern erts_atomic32_t erts_active_bp_index;
extern erts_atomic32_t erts_staging_bp_index;

ERTS_GLB_INLINE ErtsBpIndex erts_active_bp_ix(void)
{
    return erts_atomic32_read_nob(&erts_active_bp_index);
}

ERTS_GLB_INLINE ErtsBpIndex erts_staging_bp_ix(void)
{
    return erts_atomic32_read_nob(&erts_staging_bp_index);
}

ERTS_GLB_INLINE
void erts_adjust_memory_break(Process *p, Sint adjustment)
{
    process_breakpoint_trace_t * pbt;
    for (pbt = ERTS_PROC_GET_CALL_MEMORY(p); pbt; pbt = pbt->next)
        pbt->allocated += adjustment;
}

ERTS_GLB_INLINE
void erts_adjust_message_break(Process *p, Eterm message)
{
    process_breakpoint_trace_t * pbt;
    for (pbt = ERTS_PROC_GET_CALL_MEMORY(p); pbt; pbt = pbt->next)
        pbt->allocated += size_object(message);
}

#endif

#endif /* _BEAM_BP_H */
