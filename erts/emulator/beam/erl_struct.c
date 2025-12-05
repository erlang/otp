/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2024. All Rights Reserved.
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
#include "erl_struct.h"
#include "global.h"
#include "index.h"

#include "bif.h"

#include "beam_common.h"
#include "erl_map.h"

#define STRUCT_INITIAL_SIZE   4000
#define STRUCT_LIMIT          (512*1024)

#define STRUCT_HASH(module, name)                                             \
    ((atom_val(module) * atom_val(name)))

#ifdef DEBUG
#  define IF_DEBUG(x) x
#else
#  define IF_DEBUG(x)
#endif

/* Note: the active table is never locked */
static IndexTable struct_tables[ERTS_NUM_CODE_IX];

static erts_atomic_t total_entries_bytes;

/* This lock protects the staging struct table from concurrent access
 * AND it protects the staging table from becoming active. */
erts_mtx_t struct_staging_lock;

struct struct_hash_entry
{
    IndexSlot slot; /* MUST BE LOCATED AT TOP OF STRUCT!!! */
    ErtsStructEntry *sp;
};

/* Helper struct that brings things together in one allocation
*/
struct struct_blob
{
    ErtsStructEntry str; /* MUST BE LOCATED AT TOP OF STRUCT!!! */
    struct struct_hash_entry entryv[ERTS_NUM_CODE_IX];
    /* Note that entryv is not indexed by "code_ix". */
};

/* Helper struct only used as template
*/
struct struct_templ
{
    struct struct_hash_entry entry;
    ErtsStructEntry str;
};

static struct struct_blob *entry_to_blob(struct struct_hash_entry *se)
{
    return ErtsContainerStruct(se->sp, struct struct_blob, str);
}

static HashValue
struct_hash(struct struct_hash_entry *se)
{
    ErtsStructEntry *str = se->sp;
    return STRUCT_HASH(str->module, str->name);
}

static int
struct_cmp(struct struct_hash_entry* tmpl_e, struct struct_hash_entry* obj_e)
{
    const ErtsStructEntry *tmpl = tmpl_e->sp, *obj = obj_e->sp;

    return !(tmpl->module == obj->module &&
             tmpl->name == obj->name);
}

static struct struct_hash_entry*
struct_alloc(struct struct_hash_entry* tmpl_e)
{
    struct struct_blob* blob;
    unsigned ix;

    if (tmpl_e->slot.index == -1) {
        /* Template, allocate blob */
        ErtsStructEntry *tmpl = tmpl_e->sp;
        ErtsStructEntry *obj;

        /* Force-align the address of struct entries so that they can safely be
         * placed in a small integer. */
        blob = (struct struct_blob*)
            erts_alloc_permanent_aligned(ERTS_ALC_T_STRUCT, sizeof(*blob),
                                         (1 << _TAG_IMMED1_SIZE));
        erts_atomic_add_nob(&total_entries_bytes, sizeof(*blob));
        obj = &blob->str;

        ASSERT(((UWord)obj) % (1 << _TAG_PRIMARY_SIZE) == 0);

        obj->module = tmpl->module;
        obj->name = tmpl->name;

        for (ix = 0; ix < ERTS_NUM_CODE_IX; ix++) {
            blob->entryv[ix].slot.index = -1;
            blob->entryv[ix].sp = &blob->str;

            obj->definitions[ix] = THE_NON_VALUE;
        }

        ix = 0;
    } else {
        /* Existing entry in another table, use free entry in blob */
        blob = entry_to_blob(tmpl_e);

        for (ix = 0; blob->entryv[ix].slot.index >= 0; ix++) {
            ASSERT(ix < ERTS_NUM_CODE_IX);
        }
    }

    return &blob->entryv[ix];
}

static void
struct_free(struct struct_hash_entry* obj)
{
    struct struct_blob* blob = entry_to_blob(obj);
    int i;

    obj->slot.index = -1;

    for (i=0; i < ERTS_NUM_CODE_IX; i++) {
        if (blob->entryv[i].slot.index >= 0) {
            return;
        }
    }

    erts_free(ERTS_ALC_T_EXPORT, blob);
    erts_atomic_add_nob(&total_entries_bytes, -sizeof(*blob));
}

void erts_struct_init_table(void)
{
    HashFunctions f;
    int i;

    erts_mtx_init(&struct_staging_lock, "struct_tab", NIL,
        ERTS_LOCK_FLAGS_PROPERTY_STATIC | ERTS_LOCK_FLAGS_CATEGORY_GENERIC);
    erts_atomic_init_nob(&total_entries_bytes, 0);

    f.hash = (H_FUN) struct_hash;
    f.cmp  = (HCMP_FUN) struct_cmp;
    f.alloc = (HALLOC_FUN) struct_alloc;
    f.free = (HFREE_FUN) struct_free;
    f.meta_alloc = (HMALLOC_FUN) erts_alloc;
    f.meta_free = (HMFREE_FUN) erts_free;
    f.meta_print = (HMPRINT_FUN) erts_print;

    for (i = 0; i < ERTS_NUM_CODE_IX; i++) {
        erts_index_init(ERTS_ALC_T_STRUCT_TABLE, &struct_tables[i],
                        "struct_list", STRUCT_INITIAL_SIZE, STRUCT_LIMIT, f);
    }
}

static struct struct_hash_entry* init_template(struct struct_templ* templ,
                                          Eterm module, Eterm name)
{
    templ->entry.sp = &templ->str;
    templ->entry.slot.index = -1;
    templ->str.module = module;
    templ->str.name = name;
    return &templ->entry;
}

/* Declared extern in header */
ErtsStructEntry *erts_struct_find_entry(Eterm module,
                                   Eterm name,
                                   ErtsCodeIndex code_ix);

ErtsStructEntry *erts_struct_find_entry(Eterm module,
                                   Eterm name,
                                   ErtsCodeIndex code_ix)
{
    struct struct_templ templ;
    struct struct_hash_entry* ee;

    ASSERT(code_ix != erts_staging_code_ix());

    ee = hash_get(&struct_tables[code_ix].htable,
                  init_template(&templ, module, name));

    if (ee) {
        return ee->sp;
    }

    return NULL;
}

ErtsStructEntry *erts_struct_put(Eterm module, Eterm name)
{
    ErtsCodeIndex code_ix = erts_staging_code_ix();
    struct struct_templ templ;
    struct struct_hash_entry* ee;

    ASSERT(is_atom(module));
    ASSERT(is_atom(name));

    erts_struct_staging_lock();

    ee = (struct struct_hash_entry*)
        index_put_entry(&struct_tables[code_ix],
                        init_template(&templ, module, name));

    erts_struct_staging_unlock();

    return ee->sp;
}

ErtsStructEntry *erts_struct_get_or_make_stub(Eterm module,
                                              Eterm name)
{
    ErtsCodeIndex code_ix;
    ErtsStructEntry *sp;
    IF_DEBUG(int retrying = 0;)

    ASSERT(is_atom(module));
    ASSERT(is_atom(name));

    do {
        code_ix = erts_active_code_ix();
        sp = erts_struct_find_entry(module, name, code_ix);

        if (sp == NULL) {
            /* The code is not loaded (yet). Put the struct in the staging
             * struct table, to avoid having to lock the active struct
             * table. */
            erts_struct_staging_lock();

            if (code_ix == erts_active_code_ix()) {
                struct struct_templ templ;
                struct struct_hash_entry* entry;

                IndexTable *tab = &struct_tables[erts_staging_code_ix()];

                init_template(&templ, module, name);
                entry = (struct struct_hash_entry *)
                    index_put_entry(tab, &templ.entry);
                sp = entry->sp;

                ASSERT(sp);
            } else {
                /* race */
                ASSERT(!retrying);
                IF_DEBUG(retrying = 1);
            }

            erts_struct_staging_unlock();
        }
    } while (!sp);

    return sp;
}

IF_DEBUG(static ErtsCodeIndex debug_struct_load_ix = 0;)

void erts_struct_start_staging(void)
{
    ErtsCodeIndex dst_ix = erts_staging_code_ix();
    ErtsCodeIndex src_ix = erts_active_code_ix();
    IndexTable* dst = &struct_tables[dst_ix];
    IndexTable* src = &struct_tables[src_ix];
    int i;

    ASSERT(dst_ix != src_ix);
    ASSERT(debug_struct_load_ix == ~0);

    erts_struct_staging_lock();

    /* Insert all entries in src into dst table */
    for (i = 0; i < src->entries; i++) {
        struct struct_hash_entry* src_entry;
        ErtsStructEntry *sp;

        src_entry = (struct struct_hash_entry*) erts_index_lookup(src, i);
        sp = src_entry->sp;

        sp->definitions[dst_ix] = sp->definitions[src_ix];

#ifndef DEBUG
        index_put_entry(dst, src_entry);
#else /* DEBUG */
        {
            struct struct_hash_entry* dst_entry =
                (struct struct_hash_entry*)index_put_entry(dst, src_entry);
            ASSERT(entry_to_blob(src_entry) == entry_to_blob(dst_entry));
        }
#endif
    }

    erts_struct_staging_unlock();

    IF_DEBUG(debug_struct_load_ix = dst_ix);
}

void erts_struct_end_staging(int commit)
{
    ASSERT(debug_struct_load_ix == erts_staging_code_ix());
    IF_DEBUG(debug_struct_load_ix = ~0);
}

bool erl_is_native_record(Eterm src, Eterm mod, Eterm name) {
    ErtsStructDefinition *defp;
    ErtsStructInstance *instance;

    ASSERT(is_struct(src));
    instance = (ErtsStructInstance*)struct_val(src);
    defp = (ErtsStructDefinition*)boxed_val(instance->struct_definition);

    return defp->module == mod && defp->name == name;
}

bool erl_get_record_elements(Process* p, Eterm* reg, Eterm src,
                             Uint size, const Eterm* elems) {
    /* Struct term, Key */
    ErtsStructDefinition *defp;
    ErtsStructInstance *instance;
    const Eterm *elems_end;
    Eterm *values;
    int field_count;
    Eterm* E = p->stop;

    ASSERT(is_struct(src));

    instance = (ErtsStructInstance*)struct_val(src);
    field_count = header_arity(instance->thing_word) - 1;
    defp = (ErtsStructDefinition*)boxed_val(instance->struct_definition);
    values = instance->values;

    elems_end = elems + size;

    for (int i = 0; i < field_count; i++) {
        if (elems[0] == defp->fields[i].key) {
            PUT_TERM_REG(values[i], elems[1]);
            elems += 2;
            if (elems >= elems_end) {
                return true;
            }
        }
    }

    return false;
}

Eterm erl_create_native_record(Process* p, Eterm* reg, Eterm id, Uint live,
                               Uint size, const Eterm* new_p) {
    /* Module, Name */
    Eterm module, name;
    ErtsStructEntry *entry;
    Uint code_ix;
    Eterm* tuple_ptr = boxed_val(id);
    Uint local = reg[live];

    module = tuple_ptr[1];
    name = tuple_ptr[2];

    code_ix = erts_active_code_ix();
    entry = erts_struct_find_entry(module,
                                   name,
                                   code_ix);

    if (entry != NULL) {
        Eterm def = entry->definitions[code_ix];

        if (is_value(def)) {
            ErtsStructDefinition *defp;
            ErtsStructInstance *instance;
            int field_count;
            Eterm *hp;
            Eterm* E;
            Uint num_words_needed;
            Eterm res;
            Eterm sentinel = NIL;
            const Eterm *new_end = new_p + size;

            defp = (ErtsStructDefinition*)boxed_val(def);

            if (!local && defp->is_exported == am_false) {
                goto badrecord;
            }

            field_count = (header_arity(defp->thing_word) - sizeof(ErtsStructDefinition)/sizeof(Eterm) + 1) / 2;

            num_words_needed = sizeof(*instance)/sizeof(Eterm) + field_count;
            if (HeapWordsLeft(p) < num_words_needed) {
                erts_garbage_collect(p, num_words_needed, reg, live);
            }
            hp = p->htop;
            E = p->stop;

            instance = (ErtsStructInstance*)hp;
            res = make_struct(hp);

            instance->thing_word = MAKE_STRUCT_HEADER(field_count);
            instance->struct_definition = def;

            hp = (Eterm*) &(instance->values);

            if (new_p == new_end) {
                new_p = &sentinel;
            }

            for (int i = 0; i < field_count; i++) {
                if (new_p[0] == defp->fields[i].key) {
                    GetSource(new_p[1], *hp);
                    hp++;
                    new_p += 2;
                    if (new_p >= new_end) {
                        new_p = &sentinel;
                    }
                } else {
                    Eterm value = defp->fields[i].value;
                    if (is_catch(value)) {
                        p->fvalue = defp->fields[i].key;
                        p->freason = EXC_NOVALUE;
                        return THE_NON_VALUE;
                    }
                    *hp++ = value;
                }
            }

            if (new_p != &sentinel) {
                p->fvalue = new_p[0];
                p->freason = EXC_BADFIELD;
                return THE_NON_VALUE;
            }

            p->htop += num_words_needed;

            return res;
        }
    }

 badrecord:
    p->fvalue = local ? name : id;
    p->freason = EXC_BADRECORD;
    return THE_NON_VALUE;
}

Eterm erl_update_native_record(Process* p, Eterm* reg, Eterm src,
                               Uint live, Uint size, const Eterm* new_p) {
    ErtsStructDefinition *defp;
    ErtsStructInstance *instance, *old_instance;
    Eterm *old_values;
    int field_count;
    Eterm *hp;
    Eterm* E;
    Uint num_words_needed;
    Eterm res;
    Eterm sentinel = NIL;
    const Eterm *new_end = new_p + size;

    ASSERT(size != 0);

    field_count = struct_field_count(src);
    old_instance = (ErtsStructInstance*)struct_val(src);
    defp = (ErtsStructDefinition*) tuple_val(old_instance->struct_definition);

    num_words_needed = sizeof(*instance)/sizeof(Eterm) + field_count;
    if (HeapWordsLeft(p) < num_words_needed) {
        reg[live] = src;
        erts_garbage_collect(p, num_words_needed, reg, live+1);
        src = reg[live];
        old_instance = (ErtsStructInstance*)struct_val(src);
        defp = (ErtsStructDefinition*) tuple_val(old_instance->struct_definition);
    }

    hp = p->htop;
    E = p->stop;
    res = make_struct(hp);

    instance = (ErtsStructInstance*)hp;
    instance->thing_word = old_instance->thing_word;
    instance->struct_definition = old_instance->struct_definition;

    old_values = old_instance->values;
    hp = (Eterm*) instance->values;

    ASSERT(new_p < new_end);
    for (int i = 0; i < field_count; i++) {
        if (new_p[0] != defp->fields[i].key) {
            *hp++ = old_values[i];
        } else {
            GetSource(new_p[1], *hp);
            hp++;
            new_p += 2;
            if (new_p >= new_end) {
                new_p = &sentinel;
            }
        }
    }

    if (new_p != &sentinel) {
        p->fvalue = new_p[0];
        p->freason = EXC_BADFIELD;
        return THE_NON_VALUE;
    }

    p->htop += num_words_needed;

    return res;
}

bool erl_is_record_accessible(Eterm src, Eterm mod) {
    ErtsStructInstance *instance;
    ErtsStructDefinition *defp;

    ASSERT(is_struct(src));

    instance = (ErtsStructInstance*) struct_val(src);
    defp = (ErtsStructDefinition*) tuple_val(instance->struct_definition);

    return defp->is_exported == am_true || defp->module == mod;
}

Eterm erl_get_record_field(Process* p, Eterm src, Eterm mod, Eterm id, Eterm field) {
    ErtsStructInstance *instance;
    ErtsStructDefinition *defp;
    Eterm *values;
    int field_count;

    if (is_not_struct(src)) {
    badrecord:
        p->fvalue = src;
        p->freason = EXC_BADRECORD;
        return THE_NON_VALUE;
    }

    instance = (ErtsStructInstance*) struct_val(src);
    defp = (ErtsStructDefinition*) tuple_val(instance->struct_definition);

    if (id == am_Underscore) {
        ;
    } else {
        Eterm module, name;
        Eterm* tuple_ptr = boxed_val(id);

        ASSERT(is_tuple(id));

        module = tuple_ptr[1];
        name = tuple_ptr[2];

        if (defp->module != module || defp->name != name) {
            /* Record name mismatch. */
            goto badrecord;
        }
    }

    if (!(defp->is_exported == am_true || defp->module == mod)) {
        goto badrecord;
    }

    field_count = struct_field_count(src);
    values = instance->values;

    for (int i = 0; i < field_count; i++) {
        if (field == defp->fields[i].key) {
            return values[i];
        }
    }

    p->fvalue = field;
    p->freason = EXC_BADFIELD;
    return THE_NON_VALUE;

}

/*
 * Here follows the BIFs in the record module.
 */

BIF_RETTYPE records_create_4(BIF_ALIST_4) {
    /* Module, Name */
    Eterm module, name;
    ErtsStructEntry *entry;
    Uint code_ix;

    if (is_not_atom(BIF_ARG_1) ||
        is_not_atom(BIF_ARG_2)) {
        BIF_P->fvalue = BIF_ARG_2;
        BIF_ERROR(BIF_P, EXC_BADRECORD);
    }
    module = BIF_ARG_1;
    name = BIF_ARG_2;

    code_ix = erts_active_code_ix();
    entry = erts_struct_find_entry(module,
                                   name,
                                   code_ix);

    if (entry != NULL) {
        Eterm def = entry->definitions[code_ix];

        if (def != THE_NON_VALUE) {
            ErtsStructDefinition *defp;
            ErtsStructInstance *instance;
            int field_count;
            Eterm *hp;
            Eterm *hp_end;
            Uint num_words_needed;
            Eterm res;

            const Eterm *vs;
            flatmap_t *mp;
            Eterm *ks;
            Uint n;

            defp = (ErtsStructDefinition*)boxed_val(def);
            field_count = (header_arity(defp->thing_word) - sizeof(ErtsStructDefinition)/sizeof(Eterm) + 1) / 2;
            num_words_needed = sizeof(*instance)/sizeof(Eterm) + field_count;
            hp = HAlloc(BIF_P, num_words_needed);

            instance = (ErtsStructInstance*)hp;
            res = make_struct(hp);

            instance->thing_word = MAKE_STRUCT_HEADER(field_count);
            instance->struct_definition = def;

            hp = (Eterm*) &(instance->values);
            hp_end = hp + field_count;

            if (is_not_map(BIF_ARG_3)) {
                BIF_P->fvalue = BIF_ARG_3;
                BIF_ERROR(BIF_P, BADMAP);
            } else if (is_flatmap(BIF_ARG_3)) {
                const Eterm *ks_end;
                Eterm sentinel = NIL;

                mp = (flatmap_t *)flatmap_val(BIF_ARG_3);
                ks = flatmap_get_keys(mp);
                vs = flatmap_get_values(mp);
                n  = flatmap_get_size(mp);

                ks_end = ks + n;

                if (ks == ks_end) {
                    ks = &sentinel;
                }

                for (int i = 0; i < field_count; i++) {
                    if (ks[0] == defp->fields[i].key) {
                        *hp++ = *vs;
                        ks++, vs++;
                        if (ks >= ks_end) {
                            ks = &sentinel;
                        }
                    } else {
                        Eterm value = defp->fields[i].value;
                        if (is_catch(value)) {
                            HRelease(BIF_P, hp_end, hp);
                            BIF_P->fvalue = defp->fields[i].key;
                            BIF_ERROR(BIF_P, EXC_NOVALUE);
                        }
                        *hp++ = value;
                    }
                }

                if (ks != &sentinel) {
                    HRelease(BIF_P, hp_end, hp);
                    BIF_P->fvalue = ks[0];
                    BIF_ERROR(BIF_P, EXC_BADFIELD);
                    return THE_NON_VALUE;
                }
            } else {
                /* TODO: Need to handle a large map. */
                HRelease(BIF_P, hp_end, hp);
                BIF_ERROR(BIF_P, EXC_SYSTEM_LIMIT);
            }

            BIF_RET(res);
        }
    }

    /* No canonical struct definition, bail out. */
    BIF_P->fvalue = BIF_ARG_2;
    BIF_ERROR(BIF_P, EXC_BADRECORD);
}

BIF_RETTYPE records_update_4(BIF_ALIST_4) {
    /* Module, Name */
    Eterm module, name;
    ErtsStructDefinition *defp;
    ErtsStructInstance *instance, *old_instance;
    Eterm *old_values;
    int field_count;
    Eterm *hp, *hp_end;
    Uint num_words_needed;
    Eterm res;

    if (is_not_struct(BIF_ARG_1)) {
        BIF_P->fvalue = BIF_ARG_1;
        BIF_ERROR(BIF_P, EXC_BADRECORD);
    }

    field_count = struct_field_count(BIF_ARG_1);
    old_instance = (ErtsStructInstance*)struct_val(BIF_ARG_1);
    defp = (ErtsStructDefinition*) tuple_val(old_instance->struct_definition);

    if (BIF_ARG_3 == am_Underscore) {
        ;
    } else {
        if (is_not_atom(BIF_ARG_2) ||
            is_not_atom(BIF_ARG_3)) {
            BIF_P->fvalue = BIF_ARG_3;
            BIF_ERROR(BIF_P, EXC_BADRECORD);
        }

        module = BIF_ARG_2;
        name = BIF_ARG_3;

        if (defp->module != module || defp->name != name) {
            /* Record name mismatch. */
            BIF_P->fvalue = BIF_ARG_3;
            BIF_ERROR(BIF_P, EXC_BADRECORD);
        }
    }

    if (is_not_map(BIF_ARG_4)) {
        BIF_P->fvalue = BIF_ARG_4;
        BIF_ERROR(BIF_P, BADMAP);
    }

    if (erts_map_size(BIF_ARG_4) == 0) {
        return BIF_ARG_1;
    }

    num_words_needed = sizeof(*instance)/sizeof(Eterm) + field_count;

    hp = HAlloc(BIF_P, num_words_needed);
    hp_end = hp + num_words_needed;

    res = make_struct(hp);

    instance = (ErtsStructInstance*)hp;
    instance->thing_word = old_instance->thing_word;
    instance->struct_definition = old_instance->struct_definition;

    old_values = old_instance->values;
    hp = (Eterm*) &(instance->values);

    if (is_flatmap(BIF_ARG_4)) {
        const Eterm *ks_end;
        Eterm sentinel = NIL;
        flatmap_t *mp;
        const Eterm *vs;
        Eterm *ks;
        Uint n;

        /* We KNOW that the keys for a flatmap have the same order as
         * a native record. */
        mp = (flatmap_t *)flatmap_val(BIF_ARG_4);
        ks = flatmap_get_keys(mp);
        vs = flatmap_get_values(mp);
        n  = flatmap_get_size(mp);

        ks_end = ks + n;

        ASSERT(ks < ks_end);
        for (int i = 0; i < field_count; i++) {
            if (ks[0] != defp->fields[i].key) {
                *hp++ = old_values[i];
            } else {
                *hp++ = vs[0];
                ks++, vs++;
                if (ks == ks_end) {
                    ks = &sentinel;
                }
            }
        }

        if (ks != &sentinel) {
            HRelease(BIF_P, hp_end, hp);
            BIF_P->fvalue = ks[0];
            BIF_ERROR(BIF_P, EXC_BADFIELD);
            return THE_NON_VALUE;
        }
    } else {
        /* TODO: Need to handle a large map. */
        HRelease(BIF_P, hp_end, hp);
        BIF_ERROR(BIF_P, EXC_SYSTEM_LIMIT);
    }

    BIF_RET(res);
}

BIF_RETTYPE records_get_2(BIF_ALIST_2) {
    /* Struct term, Key */
    ErtsStructDefinition *defp;
    int field_count;
    Eterm obj, *objp;
    Eterm key;

    key = BIF_ARG_1;
    obj = BIF_ARG_2;

    if (is_not_struct(obj) ||
        is_not_atom(key)) {
        BIF_ERROR(BIF_P, BADARG);
    }

    objp = struct_val(obj);
    field_count = header_arity(objp[0]) - 1;
    defp = (ErtsStructDefinition*)boxed_val(objp[1]);

    for (int i = 0; i < field_count; i++) {
        if (eq(key, defp->fields[i].key)) {
            BIF_RET(objp[2 + i]);
        }
    }

    BIF_ERROR(BIF_P, BADARG);
}

BIF_RETTYPE records_get_module_1(BIF_ALIST_1) {
    ErtsStructInstance *instance;
    ErtsStructDefinition *defp;

    if (is_not_struct(BIF_ARG_1)) {
        BIF_ERROR(BIF_P, BADARG);
    }
    instance = (ErtsStructInstance*) struct_val(BIF_ARG_1);
    defp = (ErtsStructDefinition*) tuple_val(instance->struct_definition);
    BIF_RET(defp->module);
}

BIF_RETTYPE records_get_name_1(BIF_ALIST_1) {
    ErtsStructInstance *instance;
    ErtsStructDefinition *defp;

    if (is_not_struct(BIF_ARG_1)) {
        BIF_ERROR(BIF_P, BADARG);
    }

    instance = (ErtsStructInstance*) struct_val(BIF_ARG_1);
    defp = (ErtsStructDefinition*) tuple_val(instance->struct_definition);
    BIF_RET(defp->name);
}

BIF_RETTYPE records_get_field_names_1(BIF_ALIST_1) {
    Eterm obj;
    ErtsStructInstance *instance;
    ErtsStructDefinition *defp;
    int field_count;
    Eterm *hp;
    Eterm list;
    Eterm *order;

    obj = BIF_ARG_1;
    if (is_not_struct(obj)) {
        BIF_P->fvalue = BIF_ARG_1;
        BIF_ERROR(BIF_P, EXC_BADRECORD);
    }

    field_count = struct_field_count(BIF_ARG_1);
    instance = (ErtsStructInstance*) struct_val(BIF_ARG_1);
    defp = (ErtsStructDefinition*) tuple_val(instance->struct_definition);
    order = tuple_val(defp->field_order) + 1;

    hp = HAlloc(BIF_P, field_count * 2);
    list = NIL;
    while (field_count--) {
        list = CONS(hp, defp->fields[unsigned_val(order[field_count])].key, list);
        hp += 2;
    }

    BIF_RET(list);
}

BIF_RETTYPE records_is_exported_1(BIF_ALIST_1) {
    Eterm obj;
    ErtsStructInstance *instance;
    ErtsStructDefinition *defp;

    obj = BIF_ARG_1;
    if (is_not_struct(obj)) {
        BIF_P->fvalue = BIF_ARG_1;
        BIF_ERROR(BIF_P, EXC_BADRECORD);
    }

    instance = (ErtsStructInstance*) struct_val(BIF_ARG_1);
    defp = (ErtsStructDefinition*) tuple_val(instance->struct_definition);
    BIF_RET(defp->is_exported);
}
