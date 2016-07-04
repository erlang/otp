/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1996-2016. All Rights Reserved.
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

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "erl_vm.h"
#include "global.h"
#include "export.h"
#include "hash.h"

#define EXPORT_INITIAL_SIZE   4000
#define EXPORT_LIMIT  (512*1024)

#define EXPORT_HASH(m,f,a) ((atom_val(m) * atom_val(f)) ^ (a))

#ifdef DEBUG
#  define IF_DEBUG(x) x
#else
#  define IF_DEBUG(x)
#endif

static IndexTable export_tables[ERTS_NUM_CODE_IX];  /* Active not locked */

static erts_smp_atomic_t total_entries_bytes;

#include "erl_smp.h"

/* This lock protects the staging export table from concurrent access
 * AND it protects the staging table from becoming active.
 */
erts_smp_mtx_t export_staging_lock;

extern BeamInstr* em_call_error_handler;
extern BeamInstr* em_call_traced_function;

struct export_entry
{
    IndexSlot slot; /* MUST BE LOCATED AT TOP OF STRUCT!!! */
    Export* ep;
};

/* Helper struct that brings things together in one allocation
*/
struct export_blob
{
    Export exp;
    struct export_entry entryv[ERTS_NUM_CODE_IX];
    /* Note that entryv is not indexed by "code_ix".
     */
};

/* Helper struct only used as template
*/
struct export_templ
{
    struct export_entry entry;
    Export exp;
};

static struct export_blob* entry_to_blob(struct export_entry* ee)
{
    return ErtsContainerStruct(ee->ep, struct export_blob, exp);
}

void
export_info(int to, void *to_arg)
{
#ifdef ERTS_SMP
    int lock = !ERTS_IS_CRASH_DUMPING;
    if (lock)
	export_staging_lock();
#endif
    index_info(to, to_arg, &export_tables[erts_active_code_ix()]);
    hash_info(to, to_arg, &export_tables[erts_staging_code_ix()].htable);
#ifdef ERTS_SMP
    if (lock)
	export_staging_unlock();
#endif
}


static HashValue
export_hash(struct export_entry* ee)
{
    Export* x = ee->ep;
    return EXPORT_HASH(x->code[0], x->code[1], x->code[2]);
}

static int
export_cmp(struct export_entry* tmpl_e, struct export_entry* obj_e)
{
    Export* tmpl = tmpl_e->ep;
    Export* obj = obj_e->ep;
    return !(tmpl->code[0] == obj->code[0] &&
	     tmpl->code[1] == obj->code[1] &&
	     tmpl->code[2] == obj->code[2]);
}


static struct export_entry*
export_alloc(struct export_entry* tmpl_e)
{
    struct export_blob* blob;
    unsigned ix;

    if (tmpl_e->slot.index == -1) {  /* Template, allocate blob */
	Export* tmpl = tmpl_e->ep;
	Export* obj;

	blob = (struct export_blob*) erts_alloc(ERTS_ALC_T_EXPORT, sizeof(*blob));
	erts_smp_atomic_add_nob(&total_entries_bytes, sizeof(*blob));
	obj = &blob->exp;
	obj->fake_op_func_info_for_hipe[0] = 0;
	obj->fake_op_func_info_for_hipe[1] = 0;
	obj->code[0] = tmpl->code[0];
	obj->code[1] = tmpl->code[1];
	obj->code[2] = tmpl->code[2];
	obj->code[3] = (BeamInstr) em_call_error_handler;
	obj->code[4] = 0;

	for (ix=0; ix<ERTS_NUM_CODE_IX; ix++) {
	    obj->addressv[ix] = obj->code+3;

	    blob->entryv[ix].slot.index = -1;
	    blob->entryv[ix].ep = &blob->exp;
	}
	ix = 0;
    }
    else { /* Existing entry in another table, use free entry in blob */
	blob = entry_to_blob(tmpl_e);
	for (ix = 0; blob->entryv[ix].slot.index >= 0; ix++) {
	    ASSERT(ix < ERTS_NUM_CODE_IX);
	}
    }
    return &blob->entryv[ix];
}

static void
export_free(struct export_entry* obj)
{
    struct export_blob* blob = entry_to_blob(obj);
    int i;
    obj->slot.index = -1;
    for (i=0; i < ERTS_NUM_CODE_IX; i++) {
	if (blob->entryv[i].slot.index >= 0) {
	    return;
	}
    }
    erts_free(ERTS_ALC_T_EXPORT, blob);
    erts_smp_atomic_add_nob(&total_entries_bytes, -sizeof(*blob));
}

void
init_export_table(void)
{
    HashFunctions f;
    int i;

    erts_smp_mtx_init(&export_staging_lock, "export_tab");
    erts_smp_atomic_init_nob(&total_entries_bytes, 0);

    f.hash = (H_FUN) export_hash;
    f.cmp  = (HCMP_FUN) export_cmp;
    f.alloc = (HALLOC_FUN) export_alloc;
    f.free = (HFREE_FUN) export_free;
    f.meta_alloc = (HMALLOC_FUN) erts_alloc;
    f.meta_free = (HMFREE_FUN) erts_free;
    f.meta_print = (HMPRINT_FUN) erts_print;

    for (i=0; i<ERTS_NUM_CODE_IX; i++) {
	erts_index_init(ERTS_ALC_T_EXPORT_TABLE, &export_tables[i], "export_list",
			EXPORT_INITIAL_SIZE, EXPORT_LIMIT, f);
    }
}

/*
 * Return a pointer to the export entry for the given function,
 * or NULL otherwise.  Notes:
 *
 * 1) BIFs have export entries and can be called through
 *    a wrapper in the export entry.
 * 2) Functions referenced by a loaded module, but not yet loaded
 *    also have export entries.  The export entry contains
 *    a wrapper which invokes the error handler if a function is
 *    called through such an export entry.
 * 3) This function is suitable for the implementation of erlang:apply/3.
 */
extern Export* /* inline-helper */
erts_find_export_entry(Eterm m, Eterm f, unsigned int a,ErtsCodeIndex code_ix);

Export*
erts_find_export_entry(Eterm m, Eterm f, unsigned int a, ErtsCodeIndex code_ix)
{
    HashValue hval = EXPORT_HASH((BeamInstr) m, (BeamInstr) f, (BeamInstr) a);
    int ix;
    HashBucket* b;

    ix = hval % export_tables[code_ix].htable.size;
    b = export_tables[code_ix].htable.bucket[ix];

    /*
     * Note: We have inlined the code from hash.c for speed.
     */
	
    while (b != (HashBucket*) 0) {
	Export* ep = ((struct export_entry*) b)->ep;
	if (ep->code[0] == m && ep->code[1] == f && ep->code[2] == a) {
	    return ep;
	}
	b = b->next;
    }
    return NULL;
}

static struct export_entry* init_template(struct export_templ* templ,
					  Eterm m, Eterm f, unsigned a)
{
    templ->entry.ep = &templ->exp;
    templ->entry.slot.index = -1;
    templ->exp.code[0] = m;
    templ->exp.code[1] = f;
    templ->exp.code[2] = a;
    return &templ->entry;
}


/*
 * Find the export entry for a loaded function.
 * Returns a NULL pointer if the given function is not loaded, or
 * a pointer to the export entry.
 *
 * Note: This function never returns export entries for BIFs
 * or functions which are not yet loaded.  This makes it suitable
 * for use by the erlang:function_exported/3 BIF or whenever you
 * cannot depend on the error_handler.
 */

Export*
erts_find_function(Eterm m, Eterm f, unsigned int a, ErtsCodeIndex code_ix)
{
    struct export_templ templ;
    struct export_entry* ee;

    ee = hash_get(&export_tables[code_ix].htable, init_template(&templ, m, f, a));
    if (ee == NULL ||
	(ee->ep->addressv[code_ix] == ee->ep->code+3 &&
	 ee->ep->code[3] != (BeamInstr) BeamOp(op_i_generic_breakpoint))) {
	return NULL;
    }
    return ee->ep;
}

/*
 * Returns a pointer to an existing export entry for a MFA,
 * or creates a new one and returns the pointer.
 *
 * This function acts on the staging export table. It should only be used
 * to load new code.
 */

Export*
erts_export_put(Eterm mod, Eterm func, unsigned int arity)
{
    ErtsCodeIndex code_ix = erts_staging_code_ix();
    struct export_templ templ;
    struct export_entry* ee;

    ASSERT(is_atom(mod));
    ASSERT(is_atom(func));
    export_staging_lock();
    ee = (struct export_entry*) index_put_entry(&export_tables[code_ix],
						init_template(&templ, mod, func, arity));
    export_staging_unlock();
    return ee->ep;
}

/*
 * Find the existing export entry for M:F/A. Failing that, create a stub
 * export entry (making a call through it will cause the error_handler to
 * be called).
 *
 * Stub export entries will be placed in the loader export table.
 */

Export*
erts_export_get_or_make_stub(Eterm mod, Eterm func, unsigned int arity)
{
    ErtsCodeIndex code_ix;
    Export* ep;
    IF_DEBUG(int retrying = 0;)
    
    ASSERT(is_atom(mod));
    ASSERT(is_atom(func));

    do {
	code_ix = erts_active_code_ix();
	ep = erts_find_export_entry(mod, func, arity, code_ix);
	if (ep == 0) {
	    /*
	     * The code is not loaded (yet). Put the export in the staging
	     * export table, to avoid having to lock the active export table.
	     */
	    export_staging_lock();
	    if (erts_active_code_ix() == code_ix) {
		struct export_templ templ;
	        struct export_entry* entry;

		IndexTable* tab = &export_tables[erts_staging_code_ix()];
		init_template(&templ, mod, func, arity);
		entry = (struct export_entry *) index_put_entry(tab, &templ.entry);
		ep = entry->ep;
		ASSERT(ep);
	    }
	    else { /* race */
		ASSERT(!retrying);
		IF_DEBUG(retrying = 1);
	    }
	    export_staging_unlock();
	}
    } while (!ep);
    return ep;
}

Export *export_list(int i, ErtsCodeIndex code_ix)
{
    return ((struct export_entry*) erts_index_lookup(&export_tables[code_ix], i))->ep;
}

int export_list_size(ErtsCodeIndex code_ix)
{
    return export_tables[code_ix].entries;
}

int export_table_sz(void)
{
    int i, bytes = 0;

    export_staging_lock();
    for (i=0; i<ERTS_NUM_CODE_IX; i++) {
	bytes += index_table_sz(&export_tables[i]);
    }
    export_staging_unlock();
    return bytes;
}
int export_entries_sz(void)
{
    return erts_smp_atomic_read_nob(&total_entries_bytes);
}
Export *export_get(Export *e)
{
    struct export_entry ee;
    struct export_entry* entry;

    ee.ep = e;
    entry = (struct export_entry*)hash_get(&export_tables[erts_active_code_ix()].htable, &ee);
    return entry ? entry->ep : NULL;
}

IF_DEBUG(static ErtsCodeIndex debug_start_load_ix = 0;)


void export_start_staging(void)
{
    ErtsCodeIndex dst_ix = erts_staging_code_ix();
    ErtsCodeIndex src_ix = erts_active_code_ix();
    IndexTable* dst = &export_tables[dst_ix];
    IndexTable* src = &export_tables[src_ix];
    struct export_entry* src_entry;
#ifdef DEBUG
    struct export_entry* dst_entry;
#endif
    int i;

    ASSERT(dst_ix != src_ix);
    ASSERT(debug_start_load_ix == -1);

    export_staging_lock();
    /*
     * Insert all entries in src into dst table
     */
    for (i = 0; i < src->entries; i++) {
	src_entry = (struct export_entry*) erts_index_lookup(src, i);
        src_entry->ep->addressv[dst_ix] = src_entry->ep->addressv[src_ix];
#ifdef DEBUG
	dst_entry = (struct export_entry*) 
#endif
	    index_put_entry(dst, src_entry);
	ASSERT(entry_to_blob(src_entry) == entry_to_blob(dst_entry));
    }
    export_staging_unlock();

    IF_DEBUG(debug_start_load_ix = dst_ix);
}

void export_end_staging(int commit)
{
    ASSERT(debug_start_load_ix == erts_staging_code_ix());
    IF_DEBUG(debug_start_load_ix = -1);
}

