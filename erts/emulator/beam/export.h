/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1996-2018. All Rights Reserved.
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

#ifndef __EXPORT_H__
#define __EXPORT_H__

#include "sys.h"
#include "index.h"
#include "code_ix.h"

/*
** Export entry
*/

typedef struct export
{
    void* addressv[ERTS_NUM_CODE_IX];  /* Pointer to code for function. */

    /* This is a small trampoline function that can be used for lazy code
     * loading, global call tracing, and so on. It's only valid when
     * addressv points to it and should otherwise be left zeroed.
     *
     * Needless to say, the order of the fields below is significant. */
    ErtsCodeInfo info;
    union {
        BeamInstr op;           /* Union discriminant. */

        struct {
            BeamInstr op;       /* op_apply_bif */
            BeamInstr func;     /* A direct pointer to the BIF */
        } bif;

        struct {
            BeamInstr op;       /* op_i_generic_breakpoint */
            BeamInstr address;  /* Address of the original function */
        } breakpoint;

        /* This is used when a module refers to (imports) a function that
         * hasn't been loaded yet. Upon loading we create an export entry which
         * redirects to the error_handler so that the appropriate module will
         * be loaded when called (or crash).
         *
         * This is also used when a module has an on_load callback as we need
         * to defer all calls until the callback returns. `deferred` contains
         * the address of the original function in this case, and there's an
         * awkward condiditon where `deferred` may be set while op is zero. See
         * erlang:finish_after_on_load/2 for details. */
        struct {
            BeamInstr op;       /* op_call_error_handler, or 0 during the last
                                 * phase of code loading when on_load is
                                 * present. See above. */
            BeamInstr deferred;
        } not_loaded;

        struct {
            BeamInstr op;       /* op_trace_jump_W */
            BeamInstr address;  /* Address of the traced function */
        } trace;

        BeamInstr raw[2];       /* For use in address comparisons, should not
                                 * be tampered directly. */
    } trampoline;
} Export;

#ifdef DEBUG
#define DBG_CHECK_EXPORT(EP, CX) \
    do { \
        if((EP)->addressv[CX] == (EP)->trampoline.raw) { \
            /* The entry currently points at the trampoline, so the
             * instructions must be valid. */ \
            ASSERT(((BeamIsOpCode((EP)->trampoline.op, op_apply_bif)) && \
                    (EP)->trampoline.bif.func != 0) || \
                   ((BeamIsOpCode((EP)->trampoline.op, op_i_generic_breakpoint)) && \
                    (EP)->trampoline.breakpoint.address != 0) || \
                   ((BeamIsOpCode((EP)->trampoline.op, op_trace_jump_W)) && \
                    (EP)->trampoline.trace.address != 0) || \
                   /* (EP)->trampoline.not_loaded.deferred may be zero. */ \
                   (BeamIsOpCode((EP)->trampoline.op, op_call_error_handler))); \
        } \
    } while(0)
#else
#define DBG_CHECK_EXPORT(EP, CX) ((void)(EP), (void)(CX))
#endif

void init_export_table(void);
void export_info(fmtfn_t, void *);

ERTS_GLB_INLINE Export* erts_active_export_entry(Eterm m, Eterm f, unsigned a);
Export* erts_export_put(Eterm mod, Eterm func, unsigned int arity);

Export* erts_export_get_or_make_stub(Eterm, Eterm, unsigned);

Export *export_list(int,ErtsCodeIndex);
int export_list_size(ErtsCodeIndex);
int export_table_sz(void);
int export_entries_sz(void);
Export *export_get(Export*);
void export_start_staging(void);
void export_end_staging(int commit);

extern erts_mtx_t export_staging_lock;
#define export_staging_lock()	erts_mtx_lock(&export_staging_lock)
#define export_staging_unlock()	erts_mtx_unlock(&export_staging_lock)

#include "beam_load.h" /* For em_* extern declarations */ 

#define ExportIsBuiltIn(EntryPtr) \
    (((EntryPtr)->addressv[erts_active_code_ix()] == (EntryPtr)->trampoline.raw) && \
     (BeamIsOpCode((EntryPtr)->trampoline.op, op_apply_bif)))

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

ERTS_GLB_INLINE Export*
erts_active_export_entry(Eterm m, Eterm f, unsigned int a)
{
    extern Export* erts_find_export_entry(Eterm m, Eterm f, unsigned a, ErtsCodeIndex);
    return erts_find_export_entry(m, f, a, erts_active_code_ix());
}

#endif /* ERTS_GLB_INLINE_INCL_FUNC_DEF */

#endif /* __EXPORT_H__ */

