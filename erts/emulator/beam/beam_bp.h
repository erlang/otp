/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2000-2017. All Rights Reserved.
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

typedef struct {
    Eterm pid;
    Sint  count;
    ErtsMonotonicTime time;
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
    ErtsMonotonicTime time;
    ErtsCodeInfo *ci;
} process_breakpoint_time_t; /* used within psd */

typedef struct {
    erts_atomic_t acount;
    erts_refc_t refc;
} BpCount;

typedef struct {
    erts_atomic_t tracer;
    erts_refc_t refc;
} BpMetaTracer;

typedef struct generic_bp_data {
    Uint flags;
    Binary* local_ms;		/* Match spec for local call trace */
    Binary* meta_ms;		/* Match spec for meta trace */
    BpMetaTracer* meta_tracer;	/* Meta tracer */
    BpCount* count;		/* For call count */
    BpDataTime* time;		/* For time trace */
} GenericBpData;

#define ERTS_NUM_BP_IX 2

typedef struct generic_bp {
    BeamInstr orig_instr;
    GenericBpData data[ERTS_NUM_BP_IX];
} GenericBp;

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
    ErtsCodeInfo *ci;
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
void erts_uninstall_breakpoints(BpFunctions* f);
void erts_consolidate_bp_data(BpFunctions* f, int local);
void erts_consolidate_bif_bp_data(void);

void erts_set_trace_break(BpFunctions *f, Binary *match_spec);
void erts_clear_trace_break(BpFunctions *f);

void erts_set_call_trace_bif(ErtsCodeInfo *ci, Binary *match_spec, int local);
void erts_clear_call_trace_bif(ErtsCodeInfo *ci, int local);

void erts_set_mtrace_break(BpFunctions *f, Binary *match_spec,
			  ErtsTracer tracer);
void erts_clear_mtrace_break(BpFunctions *f);
void erts_set_mtrace_bif(ErtsCodeInfo *ci, Binary *match_spec,
			 ErtsTracer tracer);
void erts_clear_mtrace_bif(ErtsCodeInfo *ci);

void erts_set_debug_break(BpFunctions *f);
void erts_clear_debug_break(BpFunctions *f);
void erts_set_count_break(BpFunctions *f, enum erts_break_op);
void erts_clear_count_break(BpFunctions *f);


void erts_clear_all_breaks(BpFunctions* f);
int erts_clear_module_break(Module *modp);
void erts_clear_export_break(Module *modp, ErtsCodeInfo* ci);

BeamInstr erts_generic_breakpoint(Process* c_p, ErtsCodeInfo *ci, Eterm* reg);
BeamInstr erts_trace_break(Process *p, ErtsCodeInfo *ci, Eterm *args,
                           Uint32 *ret_flags, ErtsTracer *tracer);

int erts_is_trace_break(ErtsCodeInfo *ci, Binary **match_spec_ret, int local);
int erts_is_mtrace_break(ErtsCodeInfo *ci, Binary **match_spec_ret,
			 ErtsTracer *tracer_ret);
int erts_is_mtrace_bif(ErtsCodeInfo *ci, Binary **match_spec_ret,
		       ErtsTracer *tracer_ret);
int erts_is_native_break(ErtsCodeInfo *ci);
int erts_is_count_break(ErtsCodeInfo *ci, Uint *count_ret);
int erts_is_time_break(Process *p, ErtsCodeInfo *ci, Eterm *call_time);

void erts_trace_time_call(Process* c_p, ErtsCodeInfo *ci, BpDataTime* bdt);
void erts_trace_time_return(Process* c_p, ErtsCodeInfo *ci);
void erts_schedule_time_break(Process *p, Uint out);
void erts_set_time_break(BpFunctions *f, enum erts_break_op);
void erts_clear_time_break(BpFunctions *f);

int erts_is_time_trace_bif(Process *p, ErtsCodeInfo *ci, Eterm *call_time);
void erts_set_time_trace_bif(ErtsCodeInfo *ci, enum erts_break_op);
void erts_clear_time_trace_bif(ErtsCodeInfo *ci);

ErtsCodeInfo *erts_find_local_func(ErtsCodeMFA *mfa);

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
#endif

#endif /* _BEAM_BP_H */
