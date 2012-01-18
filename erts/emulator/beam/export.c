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

#ifdef DEBUG
#  define IF_DEBUG(x) x
#else
#  define IF_DEBUG(x)
#endif

static IndexTable export_tables[ERTS_NUM_CODE_IX];  /* Active not locked */
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
    unsigned top_ix;  /*SVERK atomic? */
    struct export_entry entryv[ERTS_NUM_CODE_IX];
};

/* Helper struct only used as template
*/
struct export_templ
{
    struct export_entry entry;
    Export exp;
};


void
export_info(int to, void *to_arg)
{
#ifdef ERTS_SMP
    int lock = !ERTS_IS_CRASH_DUMPING;
    if (lock)
	export_read_lock();
#endif
    index_info(to, to_arg, &export_tables[erts_active_code_ix()]);
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
    int i;

    obj->fake_op_func_info_for_hipe[0] = 0;
    obj->fake_op_func_info_for_hipe[1] = 0;
    obj->code[0] = tmpl->code[0];
    obj->code[1] = tmpl->code[1];
    obj->code[2] = tmpl->code[2];
    obj->code[3] = (BeamInstr) em_call_error_handler;
    obj->code[4] = 0;
    obj->match_prog_set = NULL;

    for (i=0; i<ERTS_NUM_CODE_IX; i++) {
	obj->addressv[i] = obj->code+3;

	blob->entryv[i].slot.index = -1;
	blob->entryv[i].ep = &blob->exp;
    }
    blob->top_ix = 0;
    return &blob->entryv[blob->top_ix];
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
    int i;
    rwmtx_opt.type = ERTS_SMP_RWMTX_TYPE_FREQUENT_READ;
    rwmtx_opt.lived = ERTS_SMP_RWMTX_LONG_LIVED;

    erts_smp_rwmtx_init_opt(&export_table_lock, &rwmtx_opt, "export_tab");

    f.hash = (H_FUN) export_hash;
    f.cmp  = (HCMP_FUN) export_cmp;
    f.alloc = (HALLOC_FUN) export_alloc;
    f.free = (HFREE_FUN) NULL; /*SVERK export_free;*/

    for (i=0; i<ERTS_NUM_CODE_IX; i++) {
	erts_index_init(ERTS_ALC_T_EXPORT_TABLE, &export_tables[i], "export_list",
			EXPORT_INITIAL_SIZE, EXPORT_LIMIT, f);
    }
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
erts_active_export_entry(Eterm m, Eterm f, unsigned int a)
{
    return erts_find_export_entry(m, f, a, erts_active_code_ix());
}

static void sverk_break(void)
{
}

Export*
erts_find_export_entry(Eterm m, Eterm f, unsigned int a,
		       ErtsCodeIndex code_ix)
{
    HashValue hval = EXPORT_HASH((BeamInstr) m, (BeamInstr) f, (BeamInstr) a);
    int ix;
    HashBucket* b;

    if (ERTS_IS_ATOM_STR("gen_event",m) && ERTS_IS_ATOM_STR("add_handler",f)) {
	sverk_break();
    }

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

    if (ERTS_IS_ATOM_STR("gen_event",m) && ERTS_IS_ATOM_STR("add_handler",f)) {
	sverk_break();
    }

    ee = hash_get(&export_tables[code_ix].htable, init_template(&templ, m, f, a));
    if (ee == NULL || (ee->ep->addressv[code_ix] == ee->ep->code+3 &&
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
    ErtsCodeIndex code_ix = erts_loader_code_ix();
    struct export_templ templ;
    int ix;

    if (ERTS_IS_ATOM_STR("gen_event",mod) && ERTS_IS_ATOM_STR("add_handler",func)) {
	sverk_break();
    }

    ASSERT(is_atom(mod));
    ASSERT(is_atom(func));
    ix = index_put(&export_tables[code_ix], init_template(&templ, mod, func, arity));
    return ((struct export_entry*) erts_index_lookup(&export_tables[code_ix], ix))->ep;
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
    Export* ep;
    
    ASSERT(is_atom(mod));
    ASSERT(is_atom(func));
    
    ep = erts_active_export_entry(mod, func, arity);
    if (ep == 0) {
	struct export_templ templ;
	struct export_entry* entry;
	/*
	 * The code is not loaded (yet). Put the export in the secondary
	 * export table, to avoid having to lock the main export table.
	 */
	export_write_lock();
	entry = (struct export_entry *) hash_put(&secondary_export_table,
						 init_template(&templ, mod, func, arity));
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
erts_export_consolidate(ErtsCodeIndex code_ix)
{
#ifdef DEBUG
    HashInfo hi;
#endif

    /*SVERK: Not sure if this is the way to go.
             Maye we should always merge into loader ix,
             or can loader table act as secondary_export_table?*/

    ERTS_SMP_LC_ASSERT((erts_is_code_ix_locked()
			&& code_ix == erts_loader_code_ix())
		       || erts_initialized == 0
		       || erts_smp_thr_progress_is_blocking());

    export_write_lock();
    erts_index_merge(&secondary_export_table, &export_tables[code_ix]);
    erts_hash_merge(&secondary_export_table, &export_tables[code_ix].htable);
    export_write_unlock();
#ifdef DEBUG
    hash_get_info(&hi, &export_tables[code_ix].htable);
    ASSERT(export_tables[code_ix].entries == hi.objs);
#endif
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
    return index_table_sz(&export_tables[erts_active_code_ix()]);
}

Export *export_get(Export *e)
{
    struct export_entry ee;
    struct export_entry* entry;

    if (ERTS_IS_ATOM_STR("gen_event",e->code[0]) && ERTS_IS_ATOM_STR("add_handler",e->code[1])) {
	sverk_break();
    }
    ee.ep = e;
    entry = (struct export_entry*)hash_get(&export_tables[erts_active_code_ix()].htable, &ee);
    return entry ? entry->ep : NULL;
}

static struct export_entry*
export_dummy_alloc(struct export_entry* entry)
{
    return entry;
}

static struct export_blob* entry_to_blob(struct export_entry* ee)
{
    return (struct export_blob*)
        ((char*)ee->ep - offsetof(struct export_blob,exp));
}

IF_DEBUG(static ErtsCodeIndex debug_start_load_ix = 0;)

void export_start_load(void)
{
    ErtsCodeIndex dst_ix = erts_loader_code_ix();
    ErtsCodeIndex src_ix = erts_active_code_ix();
    IndexTable* dst = &export_tables[dst_ix];
    IndexTable* src = &export_tables[src_ix];
    struct export_entry* src_entry;
    struct export_entry* dst_entry;
    struct export_blob* blob;
    int i;

    ASSERT(dst_ix != src_ix);
    ASSERT(dst->entries <= src->entries);
    ASSERT(debug_start_load_ix == -1);

    /*
     * Make sure our existing entries are up to date
     */
    for (i = 0; i < dst->entries; i++) {
	src_entry = (struct export_entry*) erts_index_lookup(src, i);
	blob = entry_to_blob(src_entry);
	blob->exp.addressv[dst_ix] = blob->exp.addressv[src_ix];
    }

    /*
     * Insert all new entries from active table
     */

    /* Trick hash_put (called by index_put) to insert existing entries. */
    dst->htable.fun.alloc = (HALLOC_FUN) &export_dummy_alloc;

    for (i = dst->entries; i < src->entries; i++) {
	src_entry = (struct export_entry*) erts_index_lookup(src, i);
	blob = entry_to_blob(src_entry);
	dst_entry = &blob->entryv[++blob->top_ix];
	blob->exp.addressv[dst_ix] = blob->exp.addressv[src_ix];
	ASSERT(blob->top_ix < ERTS_NUM_CODE_IX);
	ASSERT(dst_entry->ep == &blob->exp);
	ASSERT(dst_entry->slot.index == -1);
	index_put(dst, dst_entry);
    }

    dst->htable.fun.alloc = (HALLOC_FUN) &export_alloc; /* restore */

    /*SVERK Remember dst->entries in order to purge on abort */

    IF_DEBUG(debug_start_load_ix = dst_ix);
}

void export_end_load(int commit)
{
    ASSERT(debug_start_load_ix == erts_loader_code_ix());

    /*SVERK Purge if abort */

    IF_DEBUG(debug_start_load_ix = -1);
}
