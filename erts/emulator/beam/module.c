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
#include "module.h"

#ifdef DEBUG
#  define IF_DEBUG(x) x
#else
#  define IF_DEBUG(x)
#endif

#define MODULE_SIZE   50
#define MODULE_LIMIT  (64*1024)

static IndexTable module_tables[ERTS_NUM_CODE_IX];

/*
 * SMP note: We don't need to look accesses to the module table because
 * there is one only scheduler thread when we update it.
 */

#include "erl_smp.h"

void module_info(int to, void *to_arg)
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


static Module* module_alloc(Module* tmpl)
{
    Module* obj = (Module*) erts_alloc(ERTS_ALC_T_MODULE, sizeof(Module));

    obj->module = tmpl->module;
    obj->curr.code = 0;
    obj->old.code = 0;
    obj->curr.code_length = 0;
    obj->old.code_length = 0;
    obj->slot.index = -1;
    obj->curr.nif = NULL;
    obj->old.nif = NULL;
    return obj;
}

static void module_free(Module* mod)
{
    erts_free(ERTS_ALC_T_MODULE, mod);
}

void init_module_table(void)
{
    HashFunctions f;
    int i;

    f.hash = (H_FUN) module_hash;
    f.cmp  = (HCMP_FUN) module_cmp;
    f.alloc = (HALLOC_FUN) module_alloc;
    f.free = (HFREE_FUN) module_free;

    for (i = 0; i < ERTS_NUM_CODE_IX; i++) {
	erts_index_init(ERTS_ALC_T_MODULE_TABLE, &module_tables[i], "module_code",
			MODULE_SIZE, MODULE_LIMIT, f);
    }
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

Module*
erts_put_module(Eterm mod)
{
    Module e;
    IndexTable* mod_tab;

    ASSERT(is_atom(mod));
    ERTS_SMP_LC_ASSERT(erts_initialized == 0
		       || erts_is_code_ix_locked()
		       || erts_smp_thr_progress_is_blocking());

    mod_tab = &module_tables[erts_staging_code_ix()];
    e.module = atom_val(mod);
    return (Module*) index_put_entry(mod_tab, (void*) &e);
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
    return index_table_sz(&module_tables[erts_active_code_ix()]);
}

#ifdef DEBUG
static ErtsCodeIndex dbg_load_code_ix = 0;
#endif

static int entries_at_start_load = 0;

void module_start_staging(void)
{
    IndexTable* src = &module_tables[erts_active_code_ix()];
    IndexTable* dst = &module_tables[erts_staging_code_ix()];
    Module* src_mod;
    Module* dst_mod;
    int i;

    ASSERT(dbg_load_code_ix == -1);
    ASSERT(dst->entries <= src->entries);

    /*
     * Make sure our existing modules are up-to-date
     */
    for (i = 0; i < dst->entries; i++) {
	src_mod = (Module*) erts_index_lookup(src, i);
	dst_mod = (Module*) erts_index_lookup(dst, i);
	ASSERT(src_mod->module == dst_mod->module);

	dst_mod->curr = src_mod->curr;
	dst_mod->old = src_mod->old;
    }

    /*
     * Copy all new modules from active table
     */
    for (i = dst->entries; i < src->entries; i++) {
	src_mod = (Module*) erts_index_lookup(src, i);
	dst_mod = (Module*) index_put_entry(dst, src_mod);
	ASSERT(dst_mod != src_mod);

	dst_mod->curr = src_mod->curr;
	dst_mod->old = src_mod->old;
    }
    entries_at_start_load = dst->entries;

    IF_DEBUG(dbg_load_code_ix = erts_staging_code_ix());
}

void module_end_staging(int commit)
{
    ASSERT(dbg_load_code_ix == erts_staging_code_ix());

    if (!commit) { /* abort */
	IndexTable* tab = &module_tables[erts_staging_code_ix()];

	ASSERT(entries_at_start_load <= tab->entries);
	index_erase_latest_from(tab, entries_at_start_load);
    }

    IF_DEBUG(dbg_load_code_ix = -1);
}
