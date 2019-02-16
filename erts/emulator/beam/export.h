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

    ErtsCodeInfo info; /* MUST be just before beam[] */

    /*
     * beam[0]: This entry is 0 unless the 'addressv' field points to it.
     *          Threaded code instruction to load function
     *		(em_call_error_handler), execute BIF (em_apply_bif),
     *		or a breakpoint instruction (op_i_generic_breakpoint).
     * beam[1]: Function pointer to BIF function (for BIFs only),
     *		or pointer to threaded code if the module has an
     *		on_load function that has not been run yet, or pointer
     *          to code if function beam[0] is a breakpoint instruction.
     *		Otherwise: 0.
     */
    BeamInstr beam[2];
} Export;


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
#define ExportIsBuiltIn(EntryPtr) 			\
(((EntryPtr)->addressv[erts_active_code_ix()] == (EntryPtr)->beam) && \
 (BeamIsOpCode((EntryPtr)->beam[0], op_apply_bif)))

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

ERTS_GLB_INLINE Export*
erts_active_export_entry(Eterm m, Eterm f, unsigned int a)
{
    extern Export* erts_find_export_entry(Eterm m, Eterm f, unsigned a, ErtsCodeIndex);
    return erts_find_export_entry(m, f, a, erts_active_code_ix());
}

#endif /* ERTS_GLB_INLINE_INCL_FUNC_DEF */

#endif /* __EXPORT_H__ */

