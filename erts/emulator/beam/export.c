/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1996-2011. All Rights Reserved.
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

#define EXPORT_HASH(m,f,a) ((m)*(f)+(a))

static IndexTable export_table;	/* Not locked. */
static Hash secondary_export_table; /* Locked. */

#include "erl_smp.h"

static erts_smp_rwmtx_t export_table_lock; /* Locks the secondary export table. */

#define export_read_lock()	erts_smp_rwmtx_rlock(&export_table_lock)
#define export_read_unlock()	erts_smp_rwmtx_runlock(&export_table_lock)
#define export_write_lock()	erts_smp_rwmtx_rwlock(&export_table_lock)
#define export_write_unlock()	erts_smp_rwmtx_rwunlock(&export_table_lock)

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
    struct export_entry entry;
};

void
export_info(int to, void *to_arg)
{
#ifdef ERTS_SMP
    int lock = !ERTS_IS_CRASH_DUMPING;
    if (lock)
	export_read_lock();
#endif
    index_info(to, to_arg, &export_table);
    hash_info(to, to_arg, &secondary_export_table);
#ifdef ERTS_SMP
    if (lock)
	export_read_unlock();
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

    Export* tmpl = tmpl_e->ep;
    struct export_blob* blob =
	(struct export_blob*) erts_alloc(ERTS_ALC_T_EXPORT, sizeof(*blob));
    Export* obj = &blob->exp;
    
    obj->fake_op_func_info_for_hipe[0] = 0;
    obj->fake_op_func_info_for_hipe[1] = 0;
    obj->code[0] = tmpl->code[0];
    obj->code[1] = tmpl->code[1];
    obj->code[2] = tmpl->code[2];
    blob->entry.slot.index = -1;
    obj->address = obj->code+3;
    obj->code[3] = (BeamInstr) em_call_error_handler;
    obj->code[4] = 0;
    obj->match_prog_set = NULL;

    blob->entry.ep = &blob->exp;
    return &blob->entry;
}

/*SVERK
static void 
export_free(struct export_entry* obj)
{
    erts_free(ERTS_ALC_T_EXPORT,  (void*) obj);
}
*/

void
init_export_table(void)
{
    HashFunctions f;
    erts_smp_rwmtx_opt_t rwmtx_opt = ERTS_SMP_RWMTX_OPT_DEFAULT_INITER;
    rwmtx_opt.type = ERTS_SMP_RWMTX_TYPE_FREQUENT_READ;
    rwmtx_opt.lived = ERTS_SMP_RWMTX_LONG_LIVED;

    erts_smp_rwmtx_init_opt(&export_table_lock, &rwmtx_opt, "export_tab");

    f.hash = (H_FUN) export_hash;
    f.cmp  = (HCMP_FUN) export_cmp;
    f.alloc = (HALLOC_FUN) export_alloc;
    f.free = (HFREE_FUN) NULL; /*SVERK export_free;*/

    erts_index_init(ERTS_ALC_T_EXPORT_TABLE, &export_table, "export_list",
		    EXPORT_INITIAL_SIZE, EXPORT_LIMIT, f);
    hash_init(ERTS_ALC_T_EXPORT_TABLE, &secondary_export_table,
	      "secondary_export_table", 50, f);
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

Export*
erts_find_export_entry(Eterm m, Eterm f, unsigned int a)
{
    HashValue hval = EXPORT_HASH((BeamInstr) m, (BeamInstr) f, (BeamInstr) a);
    int ix;
    HashBucket* b;

    ix = hval % export_table.htable.size;
    b = export_table.htable.bucket[ix];

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

static struct export_entry* init_template(struct export_blob* blob,
					  Eterm m, Eterm f, unsigned a)
{
    blob->entry.ep = &blob->exp;
    blob->exp.code[0] = m;
    blob->exp.code[1] = f;
    blob->exp.code[2] = a;
    return &blob->entry;
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
erts_find_function(Eterm m, Eterm f, unsigned int a)
{
    struct export_blob blob;
    struct export_entry* ee;

    ee = hash_get(&export_table.htable, init_template(&blob, m, f, a));
    if (ee == NULL || (ee->ep->address == ee->ep->code+3 &&
		       ee->ep->code[3] != (BeamInstr) em_call_traced_function)) {
	return NULL;
    }
    return ee->ep;
}

/*
 * Returns a pointer to an existing export entry for a MFA,
 * or creates a new one and returns the pointer.
 *
 * This function provides unlocked write access to the main export
 * table. It should only be used during start up or when
 * all other threads are blocked.
 */

Export*
erts_export_put(Eterm mod, Eterm func, unsigned int arity)
{
    struct export_blob blob;
    int ix;

    ERTS_SMP_LC_ASSERT(erts_initialized == 0
		       || erts_smp_thr_progress_is_blocking());
    ASSERT(is_atom(mod));
    ASSERT(is_atom(func));
    ix = index_put(&export_table, init_template(&blob, mod, func, arity));
    return ((struct export_entry*) erts_index_lookup(&export_table, ix))->ep;
}

/*
 * Find the existing export entry for M:F/A. Failing that, create a stub
 * export entry (making a call through it will cause the error_handler to
 * be called).
 *
 * Stub export entries will be placed in the secondary export table.
 * erts_export_consolidate() will move all stub export entries into the
 * main export table (will be done the next time code is loaded).
 */

Export*
erts_export_get_or_make_stub(Eterm mod, Eterm func, unsigned int arity)
{
    struct export_blob blob;
    Export* ep;
    
    ASSERT(is_atom(mod));
    ASSERT(is_atom(func));
    
    ep = erts_find_export_entry(mod, func, arity);
    if (ep == 0) {
	struct export_entry* entry;
	/*
	 * The code is not loaded (yet). Put the export in the secondary
	 * export table, to avoid having to lock the main export table.
	 */
	export_write_lock();
	entry = (struct export_entry *) hash_put(&secondary_export_table,
						 init_template(&blob, mod, func, arity));
	export_write_unlock();
	ep = entry->ep;
    }
    return ep;
}

/*
 * To be called before loading code (with other threads blocked).
 * This function will move all export entries from the secondary
 * export table into the primary.
 */
void
erts_export_consolidate(void)
{
#ifdef DEBUG
    HashInfo hi;
#endif

    ERTS_SMP_LC_ASSERT(erts_initialized == 0
		       || erts_smp_thr_progress_is_blocking());

    export_write_lock();
    erts_index_merge(&secondary_export_table, &export_table);
    erts_hash_merge(&secondary_export_table, &export_table.htable);
    export_write_unlock();
#ifdef DEBUG
    hash_get_info(&hi, &export_table.htable);
    ASSERT(export_table.entries == hi.objs);
#endif
}

Export *export_list(int i)
{
    return ((struct export_entry*) erts_index_lookup(&export_table, i))->ep;
}

int export_list_size(void)
{
    return export_table.entries;
}

int export_table_sz(void)
{
    return index_table_sz(&export_table);
}

Export *export_get(Export *e)
{
    struct export_entry ee;
    struct export_entry* entry;
    ee.ep = e;
    entry = (struct export_entry*)hash_get(&export_table.htable, &ee);
    return entry ? entry->ep : NULL;
}
