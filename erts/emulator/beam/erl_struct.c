/*
 * %CopyrightBegin%
 *
 * SPDX-License-Identifier: Apache-2.0
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

static HashValue
record_hash(ErtsStructEntry *str)
{
    return STRUCT_HASH(str->module, str->name);
}

static int
record_cmp(const ErtsStructEntry *tmpl, const ErtsStructEntry *obj) {
    return !(tmpl->module == obj->module &&
             tmpl->name == obj->name);
}

static void
record_init(ErtsStructEntry *obj, const ErtsStructEntry *tmpl)
{
    obj->module = tmpl->module;
    obj->name = tmpl->name;

    for (int ix = 0; ix < ERTS_NUM_CODE_IX; ix++) {
        obj->definitions[ix] = THE_NON_VALUE;
    }
}

static void
record_stage(ErtsStructEntry *obj,
             ErtsCodeIndex src_ix,
             ErtsCodeIndex dst_ix) {
    obj->definitions[dst_ix] = obj->definitions[src_ix];
}

#define ERTS_CODE_STAGED_PREFIX record
#define ERTS_CODE_STAGED_OBJECT_TYPE ErtsStructEntry
#define ERTS_CODE_STAGED_OBJECT_HASH record_hash
#define ERTS_CODE_STAGED_OBJECT_COMPARE record_cmp
#define ERTS_CODE_STAGED_OBJECT_INITIALIZE record_init
#define ERTS_CODE_STAGED_OBJECT_STAGE record_stage
#define ERTS_CODE_STAGED_OBJECT_ALLOC_TYPE ERTS_ALC_T_STRUCT
#define ERTS_CODE_STAGED_TABLE_ALLOC_TYPE ERTS_ALC_T_STRUCT_TABLE
#define ERTS_CODE_STAGED_TABLE_INITIAL_SIZE STRUCT_INITIAL_SIZE
#define ERTS_CODE_STAGED_TABLE_LIMIT STRUCT_LIMIT

#define ERTS_CODE_STAGED_WANT_GET
#define ERTS_CODE_STAGED_WANT_FOREACH

#include "erl_code_staged.h"

void erts_struct_init_table(void)
{
    record_staged_init();
}

/* Declared extern in header */
const ErtsStructEntry *erts_struct_find_entry(Eterm module,
                                              Eterm name,
                                              ErtsCodeIndex code_ix);

Eterm erts_canonical_record_def(ErtsStructDefinition *defp) {
    const ErtsStructEntry *entry;
    ErtsCodeIndex code_ix;
    Eterm cons;
    Eterm canonical_def;
    ErtsStructDefinition *canonical_p;
    Eterm *order_def, *order_canonical;
    int field_count;
    Eterm result = make_boxed((Eterm *)defp);

    code_ix = erts_active_code_ix();
    entry = erts_struct_find_entry(defp->module, defp->name, code_ix);

    if (entry == NULL) {
        return result;
    }

    cons = entry->definitions[code_ix];
    if (is_non_value(cons)) {
        return result;
    }

    canonical_def = CAR(list_val(cons));

    canonical_p = (ErtsStructDefinition*)boxed_val(canonical_def);
    if (defp->is_exported != canonical_p->is_exported) {
        return result;
    }

    order_def = tuple_val(defp->field_order);
    order_canonical = tuple_val(canonical_p->field_order);

    if (order_def[0] != order_canonical[0]) {
        return result;
    }
    field_count = arityval(order_def[0]);
    order_def++, order_canonical++;

    for (int i = 0; i < field_count; i++) {
        if (defp->keys[i] != canonical_p->keys[i] ||
            order_def[i] != order_canonical[i]) {
            return result;
        }
    }

    return canonical_def;
}

static void
init_record_template(record_template_t *template, Eterm module, Eterm name) {
    ErtsStructEntry *object = record_staged_init_template(template);

    object->module = module;
    object->name = name;
}

const ErtsStructEntry *erts_struct_find_entry(Eterm module,
                                              Eterm name,
                                              ErtsCodeIndex code_ix) {
    record_template_t template;

    init_record_template(&template, module, name);
    return record_staged_get(&template, code_ix);
}

ErtsStructEntry *
erts_struct_put(Eterm module, Eterm name) {
    record_template_t template;

    init_record_template(&template, module, name);
    return record_staged_upsert(&template);
}

const ErtsStructEntry *
erts_struct_get_or_make_stub(Eterm module, Eterm name) {
    record_template_t template;

    init_record_template(&template, module, name);
    return record_staged_upsert(&template);
}

struct record_module_delete_args {
    Eterm module;
    ErtsCodeIndex code_ix;
};

static void record_module_delete_foreach(ErtsStructEntry *obj, void *args_)
{
    struct record_module_delete_args *args = args_;
    
    if (obj->module == args->module) {
        obj->definitions[args->code_ix] = THE_NON_VALUE;
    }
}

void erts_record_module_delete(Eterm module)
{
    ErtsCodeIndex staging_ix = erts_staging_code_ix();
    struct record_module_delete_args args = {module, staging_ix};

    ERTS_LC_ASSERT(erts_has_code_stage_permission());

    record_staged_foreach(record_module_delete_foreach, &args, staging_ix);
}

void erts_struct_start_staging(void)
{
    record_staged_start_staging();
}

void erts_struct_end_staging(int commit)
{
    record_staged_end_staging(commit);
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
        if (elems[0] == defp->keys[i]) {
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
    const ErtsStructEntry *entry;
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
        Eterm cons = entry->definitions[code_ix];

        if (is_value(cons)) {
            Eterm def;
            ErtsStructDefinition *defp;
            ErtsStructInstance *instance;
            int field_count;
            Eterm *hp;
            Eterm* E;
            Uint num_words_needed;
            Eterm res;
            Eterm sentinel = NIL;
            const Eterm *new_end = new_p + size;
            Eterm *def_values;

            def = CAR(list_val(cons));
            defp = (ErtsStructDefinition*)boxed_val(def);
            def_values = tuple_val(CDR(list_val(cons))) + 1;

            if (!local && defp->is_exported == am_false) {
                goto badrecord;
            }

            field_count = (header_arity(defp->thing_word) -
                           sizeof(ErtsStructDefinition)/sizeof(Eterm) + 1);

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
                if (new_p[0] == defp->keys[i]) {
                    GetSource(new_p[1], *hp);
                    hp++;
                    new_p += 2;
                    if (new_p >= new_end) {
                        new_p = &sentinel;
                    }
                } else {
                    Eterm value = def_values[i];
                    if (is_catch(value)) {
                        p->fvalue = defp->keys[i];
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
        if (new_p[0] != defp->keys[i]) {
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
        if (field == defp->keys[i]) {
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

struct erl_record_field {
    Eterm key;
    Eterm value;
};

static int record_compare(const struct erl_record_field *a, const struct erl_record_field *b) {
    Sint res = erts_cmp_flatmap_keys(a->key, b->key);

    if (res < 0) {
        return -1;
    } else if (res > 0) {
        return 1;
    }

    return 0;
}

BIF_RETTYPE records_create_4(BIF_ALIST_4) {
    Eterm module, name;
    const ErtsStructEntry *entry;
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
        Eterm cons = entry->definitions[code_ix];

        if (is_value(cons)) {
            Eterm def;
            ErtsStructDefinition *defp;
            ErtsStructInstance *instance;
            int field_count;
            Eterm *hp;
            Eterm *hp_end;
            Uint num_words_needed;
            Eterm res;
            Eterm *vs;
            flatmap_t *mp;
            Eterm *ks;
            Uint n;
            Eterm *def_values;

            def = CAR(list_val(cons));
            defp = (ErtsStructDefinition*)boxed_val(def);
            def_values = tuple_val(CDR(list_val(cons))) + 1;

            defp = (ErtsStructDefinition*)boxed_val(def);
            field_count = (header_arity(defp->thing_word) -
                           sizeof(ErtsStructDefinition)/sizeof(Eterm) + 1);
            num_words_needed = sizeof(*instance)/sizeof(Eterm) + field_count;
            hp = HAlloc(BIF_P, num_words_needed);

            instance = (ErtsStructInstance*)hp;
            res = make_struct(hp);

            instance->thing_word = MAKE_STRUCT_HEADER(field_count);
            instance->struct_definition = def;

            hp = (Eterm*) &(instance->values);
            hp_end = hp + field_count;

            if (is_not_map(BIF_ARG_3)) {
                HRelease(BIF_P, hp_end, hp);
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
                    if (ks[0] == defp->keys[i]) {
                        *hp++ = *vs;
                        ks++, vs++;
                        if (ks >= ks_end) {
                            ks = &sentinel;
                        }
                    } else {
                        Eterm value = def_values[i];
                        if (is_catch(value)) {
                            HRelease(BIF_P, hp_end, (Eterm *)instance);
                            BIF_P->fvalue = defp->keys[i];
                            BIF_ERROR(BIF_P, EXC_NOVALUE);
                        }
                        *hp++ = value;
                    }
                }

                if (ks != &sentinel) {
                    HRelease(BIF_P, hp_end, (Eterm *)instance);
                    BIF_P->fvalue = ks[0];
                    BIF_ERROR(BIF_P, EXC_BADFIELD);
                }
            } else {
                DECLARE_WSTACK(wstack);
                int j;
                Eterm *kv;
                struct erl_record_field *fields;
                const struct erl_record_field *fields_end;
                struct erl_record_field sentinel = { NIL, NIL };
                void *tmp_array;

                ASSERT(is_hashmap(BIF_ARG_3));
                n = hashmap_size(BIF_ARG_3);
                hashmap_iterator_init(&wstack, BIF_ARG_3, 0);
                tmp_array = erts_alloc(ERTS_ALC_T_TMP, n * sizeof(struct erl_record_field));
                fields = (struct erl_record_field*)tmp_array;

                j = 0;
                while ((kv=hashmap_iterator_next(&wstack)) != NULL) {
                    fields[j].key = CAR(kv);
                    fields[j].value = CDR(kv);
                    j++;
                }
                DESTROY_WSTACK(wstack);

                qsort((void *) fields, n, sizeof(struct erl_record_field),
                      (int (*)(const void *, const void *)) record_compare);

                fields_end = fields + n;

                for (int i = 0; i < field_count; i++) {
                    if (fields[0].key == defp->keys[i]) {
                        *hp++ = fields[0].value;
                        fields++;
                        if (fields >= fields_end) {
                            fields = &sentinel;
                        }
                    } else {
                        Eterm value = def_values[i];
                        if (is_catch(value)) {
                            HRelease(BIF_P, hp_end, (Eterm *)instance);
                            BIF_P->fvalue = defp->keys[i];
                            erts_free(ERTS_ALC_T_TMP, tmp_array);
                            BIF_ERROR(BIF_P, EXC_NOVALUE);
                        }
                        *hp++ = value;
                    }
                }

                if (fields != &sentinel) {
                    HRelease(BIF_P, hp_end, (Eterm *)instance);
                    BIF_P->fvalue = fields[0].key;
                    erts_free(ERTS_ALC_T_TMP, tmp_array);
                    BIF_ERROR(BIF_P, EXC_BADFIELD);
                }

                erts_free(ERTS_ALC_T_TMP, tmp_array);
            }

            BIF_RET(res);
        }
    }

    /* No canonical struct definition, bail out. */
    BIF_P->fvalue = BIF_ARG_2;
    BIF_ERROR(BIF_P, EXC_BADRECORD);
}

BIF_RETTYPE records_update_4(BIF_ALIST_4) {
    Eterm module, name;
    ErtsStructDefinition *defp;
    ErtsStructInstance *instance, *old_instance;
    Eterm *old_values;
    int field_count;
    Eterm *hp, *hp_end;
    Uint num_words_needed;
    Eterm res;
    Uint n;

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

        /* We KNOW that the keys for a flatmap have the same order as
         * a native record. */
        mp = (flatmap_t *)flatmap_val(BIF_ARG_4);
        ks = flatmap_get_keys(mp);
        vs = flatmap_get_values(mp);
        n  = flatmap_get_size(mp);

        ks_end = ks + n;

        ASSERT(ks < ks_end);
        for (int i = 0; i < field_count; i++) {
            if (ks[0] != defp->keys[i]) {
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
            HRelease(BIF_P, hp_end, (Eterm *)instance);
            BIF_P->fvalue = ks[0];
            BIF_ERROR(BIF_P, EXC_BADFIELD);
        }
    } else {
        DECLARE_WSTACK(wstack);
        int j;
        Eterm *kv;
        struct erl_record_field *fields;
        const struct erl_record_field *fields_end;
        struct erl_record_field sentinel = { NIL, NIL };
        void *tmp_array;

        ASSERT(is_hashmap(BIF_ARG_4));
        n = hashmap_size(BIF_ARG_4);
        hashmap_iterator_init(&wstack, BIF_ARG_4, 0);
        tmp_array = erts_alloc(ERTS_ALC_T_TMP, n * sizeof(struct erl_record_field));
        fields = (struct erl_record_field*)tmp_array;

        j = 0;
        while ((kv=hashmap_iterator_next(&wstack)) != NULL) {
            fields[j].key = CAR(kv);
            fields[j].value = CDR(kv);
            j++;
        }
        DESTROY_WSTACK(wstack);

        qsort((void *) fields, n, sizeof(struct erl_record_field),
              (int (*)(const void *, const void *)) record_compare);

        fields_end = fields + n;

        for (int i = 0; i < field_count; i++) {
            if (fields[0].key == defp->keys[i]) {
                *hp++ = fields[0].value;
                fields++;
                if (fields >= fields_end) {
                    fields = &sentinel;
                }
            }
        }

        if (fields != &sentinel) {
            HRelease(BIF_P, hp_end, (Eterm *)instance);
            BIF_P->fvalue = fields[0].key;
            erts_free(ERTS_ALC_T_TMP, tmp_array);
            BIF_ERROR(BIF_P, EXC_BADFIELD);
        }

        erts_free(ERTS_ALC_T_TMP, tmp_array);
    }

    BIF_RET(res);
}

BIF_RETTYPE records_get_2(BIF_ALIST_2) {
    ErtsStructInstance *instance;
    ErtsStructDefinition *defp;
    int field_count;
    Eterm record;
    Eterm key;
    Eterm *values;

    key = BIF_ARG_1;
    record = BIF_ARG_2;

    if (is_not_struct(record) || is_not_atom(key)) {
        BIF_ERROR(BIF_P, BADARG);
    }

    field_count = struct_field_count(record);
    instance = (ErtsStructInstance*) struct_val(record);
    defp = (ErtsStructDefinition*) tuple_val(instance->struct_definition);
    values = instance->values;

    for (int i = 0; i < field_count; i++) {
        if (eq(key, defp->keys[i])) {
            BIF_RET(values[i]);
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
        list = CONS(hp, defp->keys[unsigned_val(order[field_count])], list);
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
