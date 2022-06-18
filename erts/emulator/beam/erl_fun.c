/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2000-2021. All Rights Reserved.
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
#include "beam_common.h"

/* Container structure for fun entries, allowing us to start `ErlFunEntry` with
 * a field other than its `HashBucket`. */
typedef struct erl_fun_entry_container {
    /* !! MUST BE THE FIRST FIELD !! */
    HashBucket bucket;

    ErlFunEntry entry;
} ErlFunEntryContainer;

static Hash erts_fun_table;

static erts_rwmtx_t erts_fun_table_lock;

#define erts_fun_read_lock()	erts_rwmtx_rlock(&erts_fun_table_lock)
#define erts_fun_read_unlock()	erts_rwmtx_runlock(&erts_fun_table_lock)
#define erts_fun_write_lock()	erts_rwmtx_rwlock(&erts_fun_table_lock)
#define erts_fun_write_unlock()	erts_rwmtx_rwunlock(&erts_fun_table_lock)

static HashValue fun_hash(ErlFunEntryContainer* obj);
static int fun_cmp(ErlFunEntryContainer* obj1, ErlFunEntryContainer* obj2);
static ErlFunEntryContainer* fun_alloc(ErlFunEntryContainer* template);
static void fun_free(ErlFunEntryContainer* obj);

void
erts_init_fun_table(void)
{
    HashFunctions f;
    erts_rwmtx_opt_t rwmtx_opt = ERTS_RWMTX_OPT_DEFAULT_INITER;
    rwmtx_opt.type = ERTS_RWMTX_TYPE_FREQUENT_READ;
    rwmtx_opt.lived = ERTS_RWMTX_LONG_LIVED;

    erts_rwmtx_init_opt(&erts_fun_table_lock, &rwmtx_opt, "fun_tab", NIL,
        ERTS_LOCK_FLAGS_PROPERTY_STATIC | ERTS_LOCK_FLAGS_CATEGORY_GENERIC);

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
erts_fun_info(fmtfn_t to, void *to_arg)
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
erts_put_fun_entry2(Eterm mod, int old_uniq, int old_index,
                    const byte* uniq, int index, int arity)
{
    ErlFunEntryContainer template;
    ErlFunEntryContainer *fc;
    ErlFunEntry *tp;
    erts_aint_t refc;

    tp = &template.entry;

    /* All fields are copied from the template when inserting a new entry. */
    ASSERT(is_atom(mod));
    tp->old_index = old_index;
    tp->old_uniq = old_uniq;
    tp->index = index;
    tp->module = mod;
    tp->arity = arity;

    sys_memcpy(tp->uniq, uniq, sizeof(tp->uniq));

    erts_fun_write_lock();
    fc = (ErlFunEntryContainer*)hash_put(&erts_fun_table, (void*)&template);
    refc = erts_refc_inctest(&fc->entry.refc, 0);
    if (refc < 2) {
        /* New or pending delete */
        erts_refc_inc(&fc->entry.refc, 1);
    }
    erts_fun_write_unlock();

    return &fc->entry;
}

const ErtsCodeMFA *erts_get_fun_mfa(const ErlFunEntry *fe) {
    ErtsCodePtr address = fe->dispatch.addresses[0];

    if (address != beam_unloaded_fun) {
        return erts_find_function_from_pc(address);
    }

    return NULL;
}

void erts_set_fun_code(ErlFunEntry *fe, ErtsCodePtr address) {
    int i;

    for (i = 0; i < ERTS_ADDRESSV_SIZE; i++) {
        fe->dispatch.addresses[i] = address;
    }
}

int erts_is_fun_loaded(const ErlFunEntry* fe) {
    return fe->dispatch.addresses[0] != beam_unloaded_fun;
}

static void
erts_erase_fun_entry_unlocked(ErlFunEntry* fe)
{
    ErlFunEntryContainer *fc = ErtsContainerStruct(fe, ErlFunEntryContainer,
                                                   entry);

    hash_erase(&erts_fun_table, (void *) fc);
}

void
erts_erase_fun_entry(ErlFunEntry* fe)
{
    erts_fun_write_lock();
    /*
     * We have to check refc again since someone might have looked up
     * the fun entry and incremented refc after last check.
     */
    if (erts_refc_dectest(&fe->refc, -1) <= 0)
    {
        if (erts_is_fun_loaded(fe)) {
            erts_exit(ERTS_ERROR_EXIT,
                "Internal error: "
                "Invalid reference count found on #Fun<%T.%d.%d>: "
                " About to erase fun still referred by code.\n",
                fe->module, fe->old_index, fe->old_uniq);
        }
        erts_erase_fun_entry_unlocked(fe);
    }
    erts_fun_write_unlock();
}

static void fun_purge_foreach(ErlFunEntryContainer *fc,
                              struct erl_module_instance* modp)
{
    const char *fun_addr, *mod_start;
    ErlFunEntry *fe = &fc->entry;

    fun_addr = (const char*)fe->dispatch.addresses[0];
    mod_start = (const char*)modp->code_hdr;

    if (ErtsInArea(fun_addr, mod_start, modp->code_length)) {
        fe->pend_purge_address = fe->dispatch.addresses[0];
        ERTS_THR_WRITE_MEMORY_BARRIER;

        erts_set_fun_code(fe, beam_unloaded_fun);

        erts_purge_state_add_fun(fe);
    }
}

void
erts_fun_purge_prepare(struct erl_module_instance* modp)
{
    erts_fun_read_lock();
    hash_foreach(&erts_fun_table, (HFOREACH_FUN)fun_purge_foreach, modp);
    erts_fun_read_unlock();
}

void
erts_fun_purge_abort_prepare(ErlFunEntry **funs, Uint no)
{
    Uint ix;

    for (ix = 0; ix < no; ix++) {
        ErlFunEntry *fe = funs[ix];

        if (fe->dispatch.addresses[0] == beam_unloaded_fun) {
            erts_set_fun_code(fe, fe->pend_purge_address);
        }
    }
}

void
erts_fun_purge_abort_finalize(ErlFunEntry **funs, Uint no)
{
    Uint ix;

    for (ix = 0; ix < no; ix++) {
        funs[ix]->pend_purge_address = NULL;
    }
}

void
erts_fun_purge_complete(ErlFunEntry **funs, Uint no)
{
    Uint ix;

    for (ix = 0; ix < no; ix++) {
	ErlFunEntry *fe = funs[ix];
	fe->pend_purge_address = NULL;
	if (erts_refc_dectest(&fe->refc, 0) == 0)
	    erts_erase_fun_entry(fe);
    }
    ERTS_THR_WRITE_MEMORY_BARRIER;
}


ErlFunThing *erts_new_export_fun_thing(Eterm **hpp, Export *exp, int arity)
{
    ErlFunThing *funp;

    funp = (ErlFunThing*)(*hpp);
    *hpp += ERL_FUN_SIZE;

    funp->thing_word = HEADER_FUN;
    funp->next = NULL;
    funp->entry.exp = exp;
    funp->num_free = 0;
    funp->creator = am_external;
    funp->arity = arity;

#ifdef DEBUG
    {
        const ErtsCodeMFA *mfa = &exp->info.mfa;
        ASSERT(arity == mfa->arity);
    }
#endif

    return funp;
}

ErlFunThing *erts_new_local_fun_thing(Process *p, ErlFunEntry *fe,
                                      int arity, int num_free)
{
    ErlFunThing *funp;

    funp = (ErlFunThing*) p->htop;
    p->htop += ERL_FUN_SIZE + num_free;
    erts_refc_inc(&fe->refc, 2);

    funp->thing_word = HEADER_FUN;
    funp->next = MSO(p).first;
    MSO(p).first = (struct erl_off_heap_header*) funp;
    funp->entry.fun = fe;
    funp->num_free = num_free;
    funp->creator = p->common.id;
    funp->arity = arity;

#ifdef DEBUG
    {
        const ErtsCodeMFA *mfa = erts_get_fun_mfa(fe);
        ASSERT(funp->arity == mfa->arity - num_free);
        ASSERT(arity == fe->arity);
    }
#endif

    return funp;
}


struct dump_fun_foreach_args {
    fmtfn_t to;
    void *to_arg;
};

static void
dump_fun_foreach(ErlFunEntryContainer *fc, struct dump_fun_foreach_args *args)
{
    ErlFunEntry *fe = &fc->entry;

    erts_print(args->to, args->to_arg, "=fun\n");
    erts_print(args->to, args->to_arg, "Module: %T\n", fe->module);
    erts_print(args->to, args->to_arg, "Uniq: %d\n", fe->old_uniq);
    erts_print(args->to, args->to_arg, "Index: %d\n",fe->old_index);
    erts_print(args->to, args->to_arg, "Address: %p\n", fe->dispatch.addresses[0]);
    erts_print(args->to, args->to_arg, "Refc: %ld\n", erts_refc_read(&fe->refc, 1));
}

void
erts_dump_fun_entries(fmtfn_t to, void *to_arg)
{
    struct dump_fun_foreach_args args = {to, to_arg};
    int lock = !ERTS_IS_CRASH_DUMPING;

    if (lock)
	erts_fun_read_lock();
    hash_foreach(&erts_fun_table, (HFOREACH_FUN)dump_fun_foreach, &args);
    if (lock)
	erts_fun_read_unlock();
}

static HashValue
fun_hash(ErlFunEntryContainer* obj)
{
    ErlFunEntry *fe = &obj->entry;

    return (HashValue) (fe->old_uniq ^ fe->index ^ atom_val(fe->module));
}

static int
fun_cmp(ErlFunEntryContainer* obj1, ErlFunEntryContainer* obj2)
{
    ErlFunEntry* fe1 = &obj1->entry;
    ErlFunEntry* fe2 = &obj2->entry;

    return !(fe1->old_index == fe2->old_index &&
             fe1->old_uniq == fe2->old_uniq &&
             fe1->module == fe2->module &&
             fe1->index == fe2->index &&
             fe1->arity == fe2->arity &&
             !sys_memcmp(fe1->uniq, fe2->uniq, sizeof(fe1->uniq)));
}

static ErlFunEntryContainer*
fun_alloc(ErlFunEntryContainer* template)
{
    ErlFunEntryContainer* obj;

    obj = (ErlFunEntryContainer *) erts_alloc(ERTS_ALC_T_FUN_ENTRY,
                                              sizeof(ErlFunEntryContainer));

    sys_memcpy(obj, template, sizeof(ErlFunEntryContainer));

    erts_refc_init(&obj->entry.refc, -1);

    erts_set_fun_code(&obj->entry, beam_unloaded_fun);

    obj->entry.pend_purge_address = NULL;

    return obj;
}

static void
fun_free(ErlFunEntryContainer* obj)
{
    erts_free(ERTS_ALC_T_FUN_ENTRY, (void *) obj);
}
