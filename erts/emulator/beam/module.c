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
#include "module.h"
#include "beam_catches.h"

#ifdef DEBUG
#  define IF_DEBUG(x) x
#else
#  define IF_DEBUG(x)
#endif

#define MODULE_SIZE   50
#define MODULE_LIMIT  (64*1024)

static IndexTable module_tables[ERTS_NUM_CODE_IX];

erts_rwmtx_t the_old_code_rwlocks[ERTS_NUM_CODE_IX];

static erts_atomic_t tot_module_bytes;

/* SMP note: Active module table lookup and current module instance can be
 *           read without any locks. Old module instances are protected by
 *           "the_old_code_rwlocks" as purging is done on active module table.
 *           Staging table is protected by the "code_ix lock". 
 */

void module_info(fmtfn_t to, void *to_arg)
{
    index_info(to, to_arg, &module_tables[erts_active_code_ix()]);
}


static HashValue module_hash(Module* x)
{
    return (HashValue) x->module;
}


static int module_cmp(Module* tmpl, Module* obj)
{
    return tmpl->module != obj->module;
}

void erts_module_instance_init(struct erl_module_instance* modi)
{
    modi->code_hdr = 0;
    modi->code_length = 0;
    modi->catches = BEAM_CATCHES_NIL;
    modi->nif = NULL;
    modi->num_breakpoints = 0;
    modi->num_traced_exports = 0;
#ifdef HIPE
    modi->hipe_code = NULL;
#endif
}

static Module* module_alloc(Module* tmpl)
{
    Module* obj = (Module*) erts_alloc(ERTS_ALC_T_MODULE, sizeof(Module));
    erts_atomic_add_nob(&tot_module_bytes, sizeof(Module));

    obj->module = tmpl->module;
    obj->slot.index = -1;
    erts_module_instance_init(&obj->curr);
    erts_module_instance_init(&obj->old);
    obj->on_load = 0;
    DBG_TRACE_MFA(make_atom(obj->module), 0, 0, "module_alloc");
    return obj;
}

static void module_free(Module* mod)
{
    erts_free(ERTS_ALC_T_MODULE, mod);
    erts_atomic_add_nob(&tot_module_bytes, -sizeof(Module));
}

void init_module_table(void)
{
    HashFunctions f;
    int i;

    f.hash = (H_FUN) module_hash;
    f.cmp  = (HCMP_FUN) module_cmp;
    f.alloc = (HALLOC_FUN) module_alloc;
    f.free = (HFREE_FUN) module_free;
    f.meta_alloc = (HMALLOC_FUN) erts_alloc;
    f.meta_free = (HMFREE_FUN) erts_free;
    f.meta_print = (HMPRINT_FUN) erts_print;

    for (i = 0; i < ERTS_NUM_CODE_IX; i++) {
	erts_index_init(ERTS_ALC_T_MODULE_TABLE, &module_tables[i], "module_code",
			MODULE_SIZE, MODULE_LIMIT, f);
    }

    for (i=0; i<ERTS_NUM_CODE_IX; i++) {
        erts_rwmtx_init(&the_old_code_rwlocks[i], "old_code", make_small(i),
            ERTS_LOCK_FLAGS_PROPERTY_STATIC | ERTS_LOCK_FLAGS_CATEGORY_GENERIC);
    }
    erts_atomic_init_nob(&tot_module_bytes, 0);
}


Module*
erts_get_module(Eterm mod, ErtsCodeIndex code_ix)
{
    Module e;
    int index;
    IndexTable* mod_tab;

    ASSERT(is_atom(mod));

    mod_tab = &module_tables[code_ix];

    e.module = atom_val(mod);
    index = index_get(mod_tab, (void*) &e);
    if (index == -1) {
	return NULL;
    } else {
	return (Module*) erts_index_lookup(mod_tab, index);
    }
}


static Module* put_module(Eterm mod, IndexTable* mod_tab)
{
    Module e;
    int oldsz, newsz;
    Module* res;

    ASSERT(is_atom(mod));
    e.module = atom_val(mod);
    oldsz = index_table_sz(mod_tab);
    res = (Module*) index_put_entry(mod_tab, (void*) &e);
    newsz = index_table_sz(mod_tab);
    erts_atomic_add_nob(&tot_module_bytes, (newsz - oldsz));
    return res;
}

Module*
erts_put_module(Eterm mod)
{
    ERTS_LC_ASSERT(erts_initialized == 0
		       || erts_has_code_write_permission());

    return put_module(mod, &module_tables[erts_staging_code_ix()]);
}

Module *module_code(int i, ErtsCodeIndex code_ix)
{
    return (Module*) erts_index_lookup(&module_tables[code_ix], i);
}

int module_code_size(ErtsCodeIndex code_ix)
{
    return module_tables[code_ix].entries;
}

int module_table_sz(void)
{
    return erts_atomic_read_nob(&tot_module_bytes);
}

#ifdef DEBUG
static ErtsCodeIndex dbg_load_code_ix = 0;
#endif

static int entries_at_start_staging = 0;

static ERTS_INLINE void copy_module(Module* dst_mod, Module* src_mod)
{
    dst_mod->curr = src_mod->curr;
    dst_mod->old = src_mod->old;
    dst_mod->on_load = src_mod->on_load;
}

void module_start_staging(void)
{
    IndexTable* src = &module_tables[erts_active_code_ix()];
    IndexTable* dst = &module_tables[erts_staging_code_ix()];
    Module* src_mod;
    Module* dst_mod;
    int i, oldsz, newsz;

    ASSERT(dbg_load_code_ix == -1);
    ASSERT(dst->entries <= src->entries);

    /*
     * Make sure our existing modules are up-to-date
     */
    for (i = 0; i < dst->entries; i++) {
	src_mod = (Module*) erts_index_lookup(src, i);
	dst_mod = (Module*) erts_index_lookup(dst, i);
	ASSERT(src_mod->module == dst_mod->module);
        copy_module(dst_mod, src_mod);
    }

    /*
     * Copy all new modules from active table
     */
    oldsz = index_table_sz(dst);
    for (i = dst->entries; i < src->entries; i++) {
	src_mod = (Module*) erts_index_lookup(src, i);
	dst_mod = (Module*) index_put_entry(dst, src_mod);
	ASSERT(dst_mod != src_mod);

        copy_module(dst_mod, src_mod);
    }
    newsz = index_table_sz(dst);
    erts_atomic_add_nob(&tot_module_bytes, (newsz - oldsz));

    entries_at_start_staging = dst->entries;
    IF_DEBUG(dbg_load_code_ix = erts_staging_code_ix());
}

void module_end_staging(int commit)
{
    ASSERT(dbg_load_code_ix == erts_staging_code_ix());

    if (!commit) { /* abort */
	IndexTable* tab = &module_tables[erts_staging_code_ix()];
	int oldsz, newsz;

	ASSERT(entries_at_start_staging <= tab->entries);
	oldsz = index_table_sz(tab);
	index_erase_latest_from(tab, entries_at_start_staging);
	newsz = index_table_sz(tab);
	erts_atomic_add_nob(&tot_module_bytes, (newsz - oldsz));
    }

    IF_DEBUG(dbg_load_code_ix = -1);
}
