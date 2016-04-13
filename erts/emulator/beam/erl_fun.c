/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2000-2016. All Rights Reserved.
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
#include "erl_fun.h"
#include "hash.h"

static Hash erts_fun_table;

#include "erl_smp.h"

static erts_smp_rwmtx_t erts_fun_table_lock;

#define erts_fun_read_lock()	erts_smp_rwmtx_rlock(&erts_fun_table_lock)
#define erts_fun_read_unlock()	erts_smp_rwmtx_runlock(&erts_fun_table_lock)
#define erts_fun_write_lock()	erts_smp_rwmtx_rwlock(&erts_fun_table_lock)
#define erts_fun_write_unlock()	erts_smp_rwmtx_rwunlock(&erts_fun_table_lock)

static HashValue fun_hash(ErlFunEntry* obj);
static int fun_cmp(ErlFunEntry* obj1, ErlFunEntry* obj2);
static ErlFunEntry* fun_alloc(ErlFunEntry* template);
static void fun_free(ErlFunEntry* obj);

/*
 * The address field of every fun that has no loaded code will point
 * to unloaded_fun[]. The -1 in unloaded_fun[0] will be interpreted
 * as an illegal arity when attempting to call a fun.
 */
static BeamInstr unloaded_fun_code[3] = {NIL, -1, 0};
static BeamInstr* unloaded_fun = unloaded_fun_code + 2;

void
erts_init_fun_table(void)
{
    HashFunctions f;
    erts_smp_rwmtx_opt_t rwmtx_opt = ERTS_SMP_RWMTX_OPT_DEFAULT_INITER;
    rwmtx_opt.type = ERTS_SMP_RWMTX_TYPE_FREQUENT_READ;
    rwmtx_opt.lived = ERTS_SMP_RWMTX_LONG_LIVED;

    erts_smp_rwmtx_init_opt(&erts_fun_table_lock, &rwmtx_opt, "fun_tab");

    f.hash = (H_FUN) fun_hash;
    f.cmp  = (HCMP_FUN) fun_cmp;
    f.alloc = (HALLOC_FUN) fun_alloc;
    f.free = (HFREE_FUN) fun_free;
    f.meta_alloc = (HMALLOC_FUN) erts_alloc;
    f.meta_free = (HMFREE_FUN) erts_free;
    f.meta_print = (HMPRINT_FUN) erts_print;

    hash_init(ERTS_ALC_T_FUN_TABLE, &erts_fun_table, "fun_table", 16, f);
}

void
erts_fun_info(int to, void *to_arg)
{
    int lock = !ERTS_IS_CRASH_DUMPING;
    if (lock)
	erts_fun_read_lock();
    hash_info(to, to_arg, &erts_fun_table);
    if (lock)
	erts_fun_read_unlock();
}

int erts_fun_table_sz(void)
{
    int sz;
    int lock = !ERTS_IS_CRASH_DUMPING;
    if (lock)
	erts_fun_read_lock();
    sz = hash_table_sz(&erts_fun_table);
    if (lock)
	erts_fun_read_unlock();
    return sz;
}

ErlFunEntry*
erts_put_fun_entry(Eterm mod, int uniq, int index)
{
    ErlFunEntry template;
    ErlFunEntry* fe;
    erts_aint_t refc;
    ASSERT(is_atom(mod));
    template.old_uniq = uniq;
    template.old_index = index;
    template.module = mod;
    erts_fun_write_lock();
    fe = (ErlFunEntry *) hash_put(&erts_fun_table, (void*) &template);
    sys_memset(fe->uniq, 0, sizeof(fe->uniq));
    fe->index = 0;
    refc = erts_refc_inctest(&fe->refc, 0);
    if (refc < 2) /* New or pending delete */
	erts_refc_inc(&fe->refc, 1);
    erts_fun_write_unlock();
    return fe;
}

ErlFunEntry*
erts_put_fun_entry2(Eterm mod, int old_uniq, int old_index,
		    byte* uniq, int index, int arity)
{
    ErlFunEntry template;
    ErlFunEntry* fe;
    erts_aint_t refc;

    ASSERT(is_atom(mod));
    template.old_uniq = old_uniq;
    template.old_index = old_index;
    template.module = mod;
    erts_fun_write_lock();
    fe = (ErlFunEntry *) hash_put(&erts_fun_table, (void*) &template);
    sys_memcpy(fe->uniq, uniq, sizeof(fe->uniq));
    fe->index = index;
    fe->arity = arity;
    refc = erts_refc_inctest(&fe->refc, 0);
    if (refc < 2) /* New or pending delete */
	erts_refc_inc(&fe->refc, 1);
    erts_fun_write_unlock();
    return fe;
}

struct my_key {
    Eterm mod;
    byte* uniq;
    int index;
    ErlFunEntry* fe;
};

ErlFunEntry*
erts_get_fun_entry(Eterm mod, int uniq, int index)
{
    ErlFunEntry template;
    ErlFunEntry *ret;

    ASSERT(is_atom(mod));
    template.old_uniq = uniq;
    template.old_index = index;
    template.module = mod;
    erts_fun_read_lock();
    ret = (ErlFunEntry *) hash_get(&erts_fun_table, (void*) &template);
    if (ret) {
	erts_aint_t refc = erts_refc_inctest(&ret->refc, 1);
	if (refc < 2) /* Pending delete */
	    erts_refc_inc(&ret->refc, 1);
    }
    erts_fun_read_unlock();
    return ret;
}

static void
erts_erase_fun_entry_unlocked(ErlFunEntry* fe)
{
    hash_erase(&erts_fun_table, (void *) fe);
}

void
erts_erase_fun_entry(ErlFunEntry* fe)
{
    erts_fun_write_lock();
#ifdef ERTS_SMP
    /*
     * We have to check refc again since someone might have looked up
     * the fun entry and incremented refc after last check.
     */
    if (erts_refc_dectest(&fe->refc, -1) <= 0)
#endif
    {
	if (fe->address != unloaded_fun)
	    erts_exit(ERTS_ERROR_EXIT,
		     "Internal error: "
		     "Invalid reference count found on #Fun<%T.%d.%d>: "
		     " About to erase fun still referred by code.\n",
		     fe->module, fe->old_index, fe->old_uniq);
	erts_erase_fun_entry_unlocked(fe);
    }
    erts_fun_write_unlock();
}

void
erts_cleanup_funs_on_purge(BeamInstr* start, BeamInstr* end)
{
    int limit;
    HashBucket** bucket;
    ErlFunEntry* to_delete = NULL;
    int i;

    erts_fun_write_lock();
    limit = erts_fun_table.size;
    bucket = erts_fun_table.bucket;
    for (i = 0; i < limit; i++) {
	HashBucket* b = bucket[i];

	while (b) {
	    ErlFunEntry* fe = (ErlFunEntry *) b;
	    BeamInstr* addr = fe->address;

	    if (start <= addr && addr < end) {
		fe->address = unloaded_fun;
		if (erts_refc_dectest(&fe->refc, 0) == 0) {
		    fe->address = (void *) to_delete;
		    to_delete = fe;
		}
	    }
	    b = b->next;
	}
    }

    while (to_delete != NULL) {
	ErlFunEntry* next = (ErlFunEntry *) to_delete->address;
	erts_erase_fun_entry_unlocked(to_delete);
	to_delete = next;
    }
    erts_fun_write_unlock();
}

void
erts_dump_fun_entries(int to, void *to_arg)
{
    int limit;
    HashBucket** bucket;
    int i;
    int lock = !ERTS_IS_CRASH_DUMPING;


    if (lock)
	erts_fun_read_lock();
    limit = erts_fun_table.size;
    bucket = erts_fun_table.bucket;
    for (i = 0; i < limit; i++) {
	HashBucket* b = bucket[i];

	while (b) {
	    ErlFunEntry* fe = (ErlFunEntry *) b;
	    erts_print(to, to_arg, "=fun\n");
	    erts_print(to, to_arg, "Module: %T\n", fe->module);
	    erts_print(to, to_arg, "Uniq: %d\n", fe->old_uniq);
	    erts_print(to, to_arg, "Index: %d\n",fe->old_index);
	    erts_print(to, to_arg, "Address: %p\n", fe->address);
#ifdef HIPE
	    erts_print(to, to_arg, "Native_address: %p\n", fe->native_address);
#endif
	    erts_print(to, to_arg, "Refc: %ld\n", erts_refc_read(&fe->refc, 1));
	    b = b->next;
	}
    }
    if (lock)
	erts_fun_read_unlock();
}

static HashValue
fun_hash(ErlFunEntry* obj)
{
    return (HashValue) (obj->old_uniq ^ obj->old_index ^ atom_val(obj->module));
}

static int
fun_cmp(ErlFunEntry* obj1, ErlFunEntry* obj2)
{
    return !(obj1->module == obj2->module && 
	     obj1->old_uniq == obj2->old_uniq &&
	     obj1->old_index == obj2->old_index);
}

static ErlFunEntry*
fun_alloc(ErlFunEntry* template)
{
    ErlFunEntry* obj = (ErlFunEntry *) erts_alloc(ERTS_ALC_T_FUN_ENTRY,
						  sizeof(ErlFunEntry));

    obj->old_uniq = template->old_uniq;
    obj->old_index = template->old_index;
    obj->module = template->module;
    erts_refc_init(&obj->refc, -1);
    obj->address = unloaded_fun;
#ifdef HIPE
    obj->native_address = NULL;
#endif
    return obj;
}

static void
fun_free(ErlFunEntry* obj)
{
    erts_free(ERTS_ALC_T_FUN_ENTRY, (void *) obj);
}
