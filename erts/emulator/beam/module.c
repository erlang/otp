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

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "erl_vm.h"
#include "global.h"
#include "module.h"

#define MODULE_SIZE   50
#define MODULE_LIMIT  (64*1024)

static IndexTable module_table;

/*
 * SMP note: We don't need to look accesses to the module table because
 * there is one only scheduler thread when we update it.
 */

#include "erl_smp.h"

void module_info(int to, void *to_arg)
{
    index_info(to, to_arg, &module_table);
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
    obj->code = 0;
    obj->old_code = 0;
    obj->code_length = 0;
    obj->old_code_length = 0;
    obj->slot.index = -1;
    obj->nif = NULL;
    obj->old_nif = NULL;
    return obj;
}


void init_module_table(void)
{
    HashFunctions f;

    f.hash = (H_FUN) module_hash;
    f.cmp  = (HCMP_FUN) module_cmp;
    f.alloc = (HALLOC_FUN) module_alloc;
    f.free = 0;

    erts_index_init(ERTS_ALC_T_MODULE_TABLE, &module_table, "module_code",
		    MODULE_SIZE, MODULE_LIMIT, f);
}

Module*
erts_get_module(Eterm mod)
{
    Module e;
    int index;

    ASSERT(is_atom(mod));
    e.module = atom_val(mod);
    index = index_get(&module_table, (void*) &e);
    if (index == -1) {
	return NULL;
    } else {
	return (Module*) erts_index_lookup(&module_table, index);
    }
}

Module*
erts_put_module(Eterm mod)
{
    Module e;
    int index;

    ASSERT(is_atom(mod));
    ERTS_SMP_LC_ASSERT(erts_initialized == 0 || erts_smp_is_system_blocked(0));
    e.module = atom_val(mod);
    index = index_put(&module_table, (void*) &e);
    return (Module*) erts_index_lookup(&module_table, index);
}

Module *module_code(int i)
{
    return (Module*) erts_index_lookup(&module_table, i);
}

int module_code_size(void)
{
    return module_table.entries;
}

int module_table_sz(void)
{
    return index_table_sz(&module_table);
}
