/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1996-2024. All Rights Reserved.
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

#ifdef BEAMASM
#define OP_PAD BeamInstr __pad[1];
#else
#define OP_PAD
#endif

typedef struct export_
{
    /* !! WARNING !!
     *
     * The JIT has a special calling convention for export entries, assuming
     * the entry itself is in a certain register. Blindly setting `c_p->i` to
     * one of the addresses in `dispatch` will crash the emulator when the
     * entry is traced, which is unlikely to be caught in our tests.
     *
     * Use the `BIF_TRAP` macros if at all possible, and be _very_ careful when
     * accessing this field directly.
     *
     * See `BeamAssembler::emit_setup_dispatchable_call` for details. */
    ErtsDispatchable dispatch;

    /* Index into bif_table[], or -1 if not a BIF. */
    int bif_number;
    /* Non-zero if this is a BIF that's traced. */
    int is_bif_traced;

    /* Globally shared external fun for this export entry. This is always a
     * literal. */
    Eterm lambda;

    /* This is a small trampoline function that can be used for lazy code
     * loading, global call tracing, and so on. It's only valid when
     * addresses points to it and should otherwise be left zeroed.
     *
     * Needless to say, the order of the fields below is significant. */
    ErtsCodeInfo info;
    union {
        struct {
            OP_PAD
            BeamInstr op;
        } common;

        struct {
            OP_PAD
            BeamInstr op;       /* op_call_bif_W */
            BeamInstr address;  /* Address of the C function */
        } bif;

        struct {
            OP_PAD
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
            OP_PAD
            BeamInstr op;       /* op_call_error_handler, or 0 during the last
                                 * phase of code loading when on_load is
                                 * present. See above. */
            BeamInstr deferred;
        } not_loaded;

    } trampoline;
} Export;

#if defined(DEBUG)
#define DBG_CHECK_EXPORT(EP, CX) \
    do { \
        if(erts_is_export_trampoline_active((EP), (CX))) { \
            /* The entry currently points at the trampoline, so the \
             * instructions must be valid. */ \
            ASSERT(((BeamIsOpCode((EP)->trampoline.common.op, op_i_generic_breakpoint)) && \
                    (EP)->trampoline.breakpoint.address != 0) || \
                   /* (EP)->trampoline.not_loaded.deferred may be zero. */ \
                   (BeamIsOpCode((EP)->trampoline.common.op, op_call_error_handler))); \
        } \
    } while(0)
#else
#define DBG_CHECK_EXPORT(EP, CX)
#endif

void init_export_table(void);
void export_info(fmtfn_t, void *);

ERTS_GLB_INLINE void erts_activate_export_trampoline(Export *ep, int code_ix);
ERTS_GLB_INLINE int erts_is_export_trampoline_active(Export *ep, int code_ix);

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

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

ERTS_GLB_INLINE void erts_activate_export_trampoline(Export *ep, int code_ix) {
    ErtsCodePtr trampoline_address;

#ifdef BEAMASM
    extern ErtsCodePtr beam_export_trampoline;
    trampoline_address = beam_export_trampoline;
#else
    trampoline_address = (ErtsCodePtr)&ep->trampoline;
#endif

    ep->dispatch.addresses[code_ix] = trampoline_address;
}

ERTS_GLB_INLINE int erts_is_export_trampoline_active(Export *ep, int code_ix) {
    ErtsCodePtr trampoline_address;

#ifdef BEAMASM
    extern ErtsCodePtr beam_export_trampoline;
    trampoline_address = beam_export_trampoline;
#else
    trampoline_address = (ErtsCodePtr)&ep->trampoline;
#endif

    return ep->dispatch.addresses[code_ix] == trampoline_address;
}

ERTS_GLB_INLINE Export*
erts_active_export_entry(Eterm m, Eterm f, unsigned int a)
{
    extern Export* erts_find_export_entry(Eterm m, Eterm f, unsigned a, ErtsCodeIndex);
    return erts_find_export_entry(m, f, a, erts_active_code_ix());
}

#endif /* ERTS_GLB_INLINE_INCL_FUNC_DEF */

#endif /* __EXPORT_H__ */

