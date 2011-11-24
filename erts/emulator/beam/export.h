/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1996-2010. All Rights Reserved.
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

#ifndef __EXPORT_H__
#define __EXPORT_H__

#ifndef __SYS_H__
#include "sys.h"
#endif

#ifndef __INDEX_H__
#include "index.h"
#endif

/*
** Export entry
*/
typedef struct export
{
    IndexSlot slot; /* MUST BE LOCATED AT TOP OF STRUCT!!! */
    void* address;		/* Pointer to code for function. */
    struct binary* match_prog_set; /* Match program for tracing. */

    BeamInstr fake_op_func_info_for_hipe[2]; /* MUST be just before code[] */
    /*
     * code[0]: Tagged atom for module.
     * code[1]: Tagged atom for function.
     * code[2]: Arity (untagged integer).
     * code[3]: This entry is 0 unless the 'address' field points to it.
     *          Threaded code instruction to load function
     *		(em_call_error_handler), execute BIF (em_apply_bif,
     *		em_apply_apply), or call a traced function
     *		(em_call_traced_function).
     * code[4]: Function pointer to BIF function (for BIFs only)
     *		or pointer to threaded code if the module has an
     *		on_load function that has not been run yet.
     *		Otherwise: 0.
     */
    BeamInstr code[5];
} Export;


void init_export_table(void);
void export_info(int, void *);

Export* erts_find_export_entry(Eterm m, Eterm f, unsigned int a);
Export* erts_export_put(Eterm mod, Eterm func, unsigned int arity);


Export* erts_export_get_or_make_stub(Eterm, Eterm, unsigned);
void erts_export_consolidate(void);

Export *export_list(int);
int export_list_size(void);
int export_table_sz(void);
Export *export_get(Export*);

#include "beam_load.h" /* For em_* extern declarations */ 
#define ExportIsBuiltIn(EntryPtr) 			\
(((EntryPtr)->address == (EntryPtr)->code + 3) && 	\
 ((EntryPtr)->code[3] == (BeamInstr) em_apply_bif))

#endif
