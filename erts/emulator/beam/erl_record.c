/*
 * %CopyrightBegin%
 *
 * SPDX-License-Identifier: Apache-2.0
 *
 * Copyright Ericsson AB 2024-2026. All Rights Reserved.
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
#include "erl_record.h"
#include "global.h"
#include "index.h"

#include "bif.h"

#include "beam_common.h"
#include "erl_map.h"
#include "big.h"

#define RECORD_INITIAL_SIZE   4000
#define RECORD_LIMIT          (512*1024)

#define RECORD_HASH(module, name)                                             \
    ((atom_val(module) * atom_val(name)))

#ifdef DEBUG
#  define IF_DEBUG(x) x
#else
#  define IF_DEBUG(x)
#endif

static HashValue
record_hash(ErtsRecordEntry *str)
{
    return RECORD_HASH(str->module, str->name);
}

static int
record_cmp(const ErtsRecordEntry *tmpl, const ErtsRecordEntry *obj) {
    return !(tmpl->module == obj->module &&
             tmpl->name == obj->name);
}

static void
record_init(ErtsRecordEntry *obj, const ErtsRecordEntry *tmpl)
{
    obj->module = tmpl->module;
    obj->name = tmpl->name;

    for (int ix = 0; ix < ERTS_NUM_CODE_IX; ix++) {
        obj->definitions[ix] = THE_NON_VALUE;
    }
}

static void
record_stage(ErtsRecordEntry *obj,
             ErtsCodeIndex src_ix,
             ErtsCodeIndex dst_ix) {
    obj->definitions[dst_ix] = obj->definitions[src_ix];
}

#define ERTS_CODE_STAGED_PREFIX record
#define ERTS_CODE_STAGED_OBJECT_TYPE ErtsRecordEntry
#define ERTS_CODE_STAGED_OBJECT_HASH record_hash
#define ERTS_CODE_STAGED_OBJECT_COMPARE record_cmp
#define ERTS_CODE_STAGED_OBJECT_INITIALIZE record_init
#define ERTS_CODE_STAGED_OBJECT_STAGE record_stage
#define ERTS_CODE_STAGED_OBJECT_ALLOC_TYPE ERTS_ALC_T_RECORD
#define ERTS_CODE_STAGED_TABLE_ALLOC_TYPE ERTS_ALC_T_RECORD_TABLE
#define ERTS_CODE_STAGED_TABLE_INITIAL_SIZE RECORD_INITIAL_SIZE
#define ERTS_CODE_STAGED_TABLE_LIMIT RECORD_LIMIT

#define ERTS_CODE_STAGED_WANT_GET
#define ERTS_CODE_STAGED_WANT_FOREACH

#include "erl_code_staged.h"

void erts_record_init_table(void)
{
    record_staged_init();
}

/* Declared extern in header */
const ErtsRecordEntry *erts_record_find_entry(Eterm module,
                                              Eterm name,
                                              ErtsCodeIndex code_ix);

Eterm erts_canonical_record_def(ErtsRecordDefinition *defp) {
    const ErtsRecordEntry *entry;
    ErtsCodeIndex code_ix;
    Eterm cons;
    Eterm canonical_def;
    ErtsRecordDefinition *canonical_p;
    Eterm *order_def, *order_canonical;
    int field_count;
    Eterm result = make_tuple((Eterm *)defp);

    code_ix = erts_active_code_ix();
    entry = erts_record_find_entry(defp->module, defp->name, code_ix);

    if (entry == NULL) {
        return result;
    }

    cons = entry->definitions[code_ix];
    if (is_non_value(cons)) {
        return result;
    }

    canonical_def = CAR(list_val(cons));

    canonical_p = (ErtsRecordDefinition*)tuple_val(canonical_def);
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
    ErtsRecordEntry *object = record_staged_init_template(template);

    object->module = module;
    object->name = name;
}

const ErtsRecordEntry *erts_record_find_entry(Eterm module,
                                              Eterm name,
                                              ErtsCodeIndex code_ix) {
    record_template_t template;

    init_record_template(&template, module, name);
    return record_staged_get(&template, code_ix);
}

ErtsRecordEntry *
erts_record_put(Eterm module, Eterm name) {
    record_template_t template;

    init_record_template(&template, module, name);
    return record_staged_upsert(&template);
}

struct record_module_delete_args {
    Eterm module;
    ErtsCodeIndex code_ix;
};

static void record_module_delete_foreach(ErtsRecordEntry *obj, void *args_)
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

void erts_record_start_staging(void)
{
    record_staged_start_staging();
}

void erts_record_end_staging(int commit)
{
    record_staged_end_staging(commit);
}

bool erl_is_native_record(Eterm src, Eterm mod, Eterm name) {
    ErtsRecordDefinition *defp;

    ASSERT(is_record(src));
    defp = RECORD_DEF_P(RECORD_INST_P(src));

    return defp->module == mod && defp->name == name;
}

bool erl_get_record_elements(Process* p, Eterm* reg, Eterm src,
                             Uint size, const Eterm* elems) {
    ErtsRecordDefinition *defp;
    ErtsRecordInstance *instance;
    const Eterm *elems_end;
    Eterm *values;
    int field_count;
    Eterm* E = p->stop;

    ASSERT(is_record(src));

    instance = RECORD_INST_P(src);
    field_count = RECORD_INST_FIELD_COUNT(instance);
    defp = RECORD_DEF_P(instance);
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

Eterm erl_create_local_native_record(Process* p, Eterm* reg,
                                     Eterm cons, Uint live,
                                     Uint size,
                                     const Eterm* new_p) {
    Eterm def;
    ErtsRecordDefinition *defp;
    ErtsRecordInstance *instance;
    int field_count;
    Eterm *hp;
    Eterm* E;
    Uint num_words_needed;
    Eterm res;
    Eterm sentinel = NIL;
    const Eterm *new_end = new_p + size;
    Eterm *def_values;

    def = CAR(list_val(cons));
    defp = (ErtsRecordDefinition*)tuple_val(def);
    def_values = tuple_val(CDR(list_val(cons))) + 1;

    field_count = RECORD_DEF_FIELD_COUNT(defp);

    num_words_needed = RECORD_INST_SIZE(field_count);
    if (HeapWordsLeft(p) < num_words_needed) {
        erts_garbage_collect(p, num_words_needed, reg, live);
    }
    hp = p->htop;
    E = p->stop;

    instance = (ErtsRecordInstance*)hp;
    res = make_record(hp);

    instance->thing_word = MAKE_RECORD_HEADER(field_count);
    instance->record_definition = def;

    hp = (Eterm*) &(instance->values);

    if (new_p == new_end) {
        new_p = &sentinel;
    }

    p->freason = EXC_NORMAL;
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
                if (is_value(res)) {
                    /* Delay this error. */
                    p->fvalue = defp->keys[i];
                    p->freason = EXC_NOVALUE;
                    res = THE_NON_VALUE;
                }
                value = NIL;
            }
            *hp++ = value;
        }
    }

    /* A `badfield` error has higher priority than a
     * `no_value` error. */
    if (new_p != &sentinel) {
        p->freason = EXC_BADFIELD;
        p->fvalue = new_p[0];
        res = THE_NON_VALUE;
    }

    if (is_value(res)) {
        p->htop += num_words_needed;
    } else {
        Eterm *hp = HAlloc(p, 3 + 3);
        Eterm tmp;

        tmp = TUPLE2(hp, defp->module, defp->name);
        hp += 3;
        tmp = TUPLE2(hp, tmp, p->fvalue);
        hp += 3;
        p->fvalue = tmp;
        return THE_NON_VALUE;
    }

    return res;
}

Eterm erl_create_native_record(Process* p, Eterm* reg, Eterm id, Uint live,
                               Uint size, const Eterm* new_p) {
    /* Module, Name */
    Eterm module, name;
    const ErtsRecordEntry *entry;
    Uint code_ix;
    Eterm* tuple_ptr = boxed_val(id);

    module = tuple_ptr[1];
    name = tuple_ptr[2];

    code_ix = erts_active_code_ix();
    entry = erts_record_find_entry(module,
                                   name,
                                   code_ix);

    if (entry != NULL) {
        Eterm cons = entry->definitions[code_ix];

        if (is_value(cons)) {
            Eterm def;
            ErtsRecordDefinition *defp;

            def = CAR(list_val(cons));
            defp = (ErtsRecordDefinition*)tuple_val(def);
            if (defp->is_exported == am_true) {
                return erl_create_local_native_record(p, reg, cons,
                                                      live, size, new_p);
            }
        }
    }

    p->fvalue = id;
    p->freason = EXC_BADRECORD;
    return THE_NON_VALUE;
}

Eterm erl_update_native_record(Process* p, Eterm* reg, Eterm src,
                               Uint live, Uint size, const Eterm* new_p) {
    ErtsRecordDefinition *defp;
    ErtsRecordInstance *instance, *old_instance;
    Eterm *old_values;
    int field_count;
    Eterm *hp;
    Eterm* E;
    Uint num_words_needed;
    Eterm res;
    Eterm sentinel = NIL;
    const Eterm *new_end = new_p + size;

    ASSERT(size != 0);

    old_instance = RECORD_INST_P(src);
    field_count = RECORD_INST_FIELD_COUNT(old_instance);
    defp = RECORD_DEF_P(old_instance);

    num_words_needed = RECORD_INST_SIZE(field_count);
    if (HeapWordsLeft(p) < num_words_needed) {
        reg[live] = src;
        erts_garbage_collect(p, num_words_needed, reg, live+1);
        src = reg[live];
        old_instance = RECORD_INST_P(src);
        defp = RECORD_DEF_P(old_instance);
    }

    hp = p->htop;
    E = p->stop;
    res = make_record(hp);

    instance = (ErtsRecordInstance*)hp;
    instance->thing_word = old_instance->thing_word;
    instance->record_definition = old_instance->record_definition;

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
        Eterm tmp;

        hp = HAlloc(p, 3 + 3);
        tmp = TUPLE2(hp, defp->module, defp->name);
        hp += 3;
        tmp = TUPLE2(hp, tmp, new_p[0]);
        hp += 3;
        p->fvalue = tmp;
        p->freason = EXC_BADFIELD;
        return THE_NON_VALUE;
    }

    p->htop += num_words_needed;

    return res;
}

bool erl_is_record_accessible(Eterm src) {
    ErtsRecordDefinition *defp;

    ASSERT(is_record(src));
    defp = RECORD_DEF_P(RECORD_INST_P(src));

    return defp->is_exported == am_true;
}

bool erl_is_wildcard_record_accessible(Eterm src, Eterm module) {
    ErtsRecordDefinition *defp;

    ASSERT(is_record(src));
    defp = RECORD_DEF_P(RECORD_INST_P(src));

    return defp->is_exported == am_true || defp->module == module;
}

Eterm erl_get_local_record_field(Process* p, Eterm src, Eterm name, Eterm field) {
    ErtsRecordInstance *instance;
    ErtsRecordDefinition *defp;
    Eterm *values;
    int field_count;
    Eterm *hp;
    Eterm tmp;

    if (is_not_record(src)) {
    badrecord:
        p->fvalue = src;
        p->freason = EXC_BADRECORD;
        return THE_NON_VALUE;
    }

    instance = RECORD_INST_P(src);
    defp = RECORD_DEF_P(instance);

    if (name != am_Underscore && defp->name != name) {
        /* Record name mismatch. */
        goto badrecord;
    }

    field_count = RECORD_INST_FIELD_COUNT(instance);
    values = instance->values;

    for (int i = 0; i < field_count; i++) {
        if (field == defp->keys[i]) {
            return values[i];
        }
    }

    hp = HAlloc(p, 3 + 3);
    tmp = TUPLE2(hp, defp->module, defp->name);
    hp += 3;
    tmp = TUPLE2(hp, tmp, field);
    hp += 3;
    p->fvalue = tmp;
    p->freason = EXC_BADFIELD;
    return THE_NON_VALUE;
}

Eterm erl_get_record_field(Process* p, Eterm src, Eterm id, Eterm field) {
    ErtsRecordInstance *instance;
    ErtsRecordDefinition *defp;
    Eterm *values;
    int field_count;
    Eterm *hp;
    Eterm tmp;

    if (is_not_record(src)) {
    badrecord:
        p->fvalue = src;
        p->freason = EXC_BADRECORD;
        return THE_NON_VALUE;
    }

    instance = RECORD_INST_P(src);
    defp = RECORD_DEF_P(instance);

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

    if (defp->is_exported == am_false) {
        goto badrecord;
    }

    field_count = RECORD_INST_FIELD_COUNT(instance);
    values = instance->values;

    for (int i = 0; i < field_count; i++) {
        if (field == defp->keys[i]) {
            return values[i];
        }
    }

    hp = HAlloc(p, 3 + 3);
    tmp = TUPLE2(hp, defp->module, defp->name);
    hp += 3;
    tmp = TUPLE2(hp, tmp, field);
    hp += 3;
    p->fvalue = tmp;
    p->freason = EXC_BADFIELD;
    return THE_NON_VALUE;
}

/*
 * Here follows the BIFs in the record module.
 */

struct erl_record_field {
    Eterm key;
    Eterm value;
    Uint order;
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
    Eterm module, name, fs, opts;
    Sint field_count;
    ErtsRecordDefinition *defp;
    ErtsRecordInstance *instance;
    Eterm *hp, *hp_end;
    Uint num_words_needed;
    Eterm res;
    Eterm *vs;
    Eterm *ks;
    const Eterm *option;
    Eterm *order;
    Eterm order_tuple;
    Uint32 hash;
    Uint hash_tuple_size;
    Eterm tagged_hash;
    Eterm is_exported;
    struct erl_record_field *fields;
#if !defined(ARCH_64)
    Eterm *big_buf;
#endif

    module = BIF_ARG_1;
    name = BIF_ARG_2;
    fs = BIF_ARG_3;
    opts = BIF_ARG_4;

    if (is_not_atom(module) || is_not_atom(name)) {
        BIF_P->fvalue = BIF_ARG_2;
        BIF_ERROR(BIF_P, EXC_BADRECORD);
    }

    if (is_not_map(opts)) {
        BIF_P->fvalue = opts;
        BIF_ERROR(BIF_P, BADMAP);
    } else if (erts_map_size(opts) != 1) {
        BIF_ERROR(BIF_P, BADARG);
    }

    option = erts_maps_get(am_is_exported, opts);
    if (option == NULL) {
        BIF_ERROR(BIF_P, BADARG);
    }
    is_exported = *option;
    if (is_exported != am_false && is_exported != am_true) {
        BIF_ERROR(BIF_P, BADARG);
    }

    field_count = erts_list_length(fs);
    if (field_count < 0) {
        BIF_ERROR(BIF_P, BADARG);
    }

    num_words_needed = RECORD_INST_SIZE(field_count) +
        RECORD_DEF_SIZE(field_count);
#if !defined(ARCH_64)
    num_words_needed += BIG_UINT_HEAP_SIZE;
#endif

    if (field_count > 0) {
        /* Room for order tuple. */
        num_words_needed += field_count + 1;
    }

    hp = HAlloc(BIF_P, num_words_needed);
    hp_end = hp + num_words_needed;

#if !defined(ARCH_64)
    big_buf = hp;
    ASSERT(BIG_UINT_HEAP_SIZE == 2);
    *hp++ = NIL;
    *hp++ = NIL;
#endif

    order = (Eterm *)hp;
    if (field_count == 0) {
        defp = (ErtsRecordDefinition*)hp;
    } else {
        defp = (ErtsRecordDefinition*)(hp + field_count + 1);
    }

    defp->thing_word = make_arityval(RECORD_DEF_SIZE(field_count) - 1);
    defp->module = module;
    defp->name = name;
    defp->is_exported = is_exported;

    ks = &defp->keys[0];

    instance = (ErtsRecordInstance*)(ks + field_count);
    res = make_record((Eterm *)instance);
    instance->thing_word = MAKE_RECORD_HEADER(field_count);
    instance->record_definition = make_tuple((Eterm *)defp);

    vs = &instance->values[0];

    fields = (struct erl_record_field*) erts_alloc(ERTS_ALC_T_TMP, field_count *
                                                   sizeof(struct erl_record_field));

    for (Sint i = 0; i < field_count; i++) {
        Eterm *cons = list_val(fs);
        Eterm pair = CAR(cons);
        Eterm key;
        Eterm *val;

        if (is_not_tuple(pair)) {
        badarg:
            HRelease(BIF_P, hp_end, hp);
            erts_free(ERTS_ALC_T_TMP, fields);
            BIF_ERROR(BIF_P, BADARG);
        }

        val = tuple_val(pair);
        if (val[0] != make_arityval(2)) {
            goto badarg;
        }
        key = val[1];

        if (is_not_atom(key)) {
            Eterm tmp;
            HRelease(BIF_P, hp_end, hp);
            erts_free(ERTS_ALC_T_TMP, fields);
            hp = HAlloc(BIF_P, 3 + 3);
            tmp = TUPLE2(hp, module, name);
            hp += 3;
            tmp = TUPLE2(hp, tmp, key);
            hp += 3;
            BIF_P->fvalue = tmp;
            BIF_ERROR(BIF_P, EXC_BADFIELD);
        }

        ks[i] = key;
        fields[i].key = key;
        fields[i].value = val[2];
        fields[i].order = i;
        fs = CDR(cons);
    }

    hash_tuple_size = defp->keys - &defp->hash - 1 + field_count;
    defp->hash = make_arityval(hash_tuple_size);

    hash = make_hash2(make_tuple((Eterm *)&defp->hash));
#if defined(ARCH_64)
    tagged_hash = make_small(hash);
#else
    if (IS_USMALL(0, hash)) {
        tagged_hash = make_small(hash);
    } else {
        tagged_hash = uint_to_big(hash, big_buf);
    }
#endif
    defp->hash = tagged_hash;

    if (field_count == 0) {
        order_tuple = ERTS_GLOBAL_LIT_EMPTY_TUPLE;
    } else {
        qsort(fields, field_count, sizeof(struct erl_record_field),
              (int (*)(const void *, const void *)) record_compare);

        for (Sint i = 1; i < field_count; i++) {
            if (fields[i-1].key == fields[i].key) {
                HRelease(BIF_P, hp_end, hp);
                erts_free(ERTS_ALC_T_TMP, fields);
                BIF_ERROR(BIF_P, BADARG);
            }
        }

        order_tuple = make_tuple(order);
        *order++ = make_arityval(field_count);
        for (int i = 0; i < field_count; i++) {
            order[fields[i].order] = make_small(i);
            defp->keys[i] = fields[i].key;
            vs[i] = fields[i].value;
        }
    }

    erts_free(ERTS_ALC_T_TMP, fields);

    defp->field_order = order_tuple;
    instance->record_definition = erts_canonical_record_def(defp);

    BIF_RET(res);
}

BIF_RETTYPE records_update_4(BIF_ALIST_4) {
    Eterm module, name;
    ErtsRecordDefinition *defp;
    ErtsRecordInstance *instance, *old_instance;
    Eterm *old_values;
    int field_count;
    Eterm *hp;
    Uint num_words_needed;
    Eterm res;
    Uint n;

    if (is_not_record(BIF_ARG_1)) {
        BIF_P->fvalue = BIF_ARG_1;
        BIF_ERROR(BIF_P, EXC_BADRECORD);
    }

    old_instance = RECORD_INST_P(BIF_ARG_1);
    field_count = RECORD_INST_FIELD_COUNT(old_instance);
    defp = RECORD_DEF_P(old_instance);

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

    num_words_needed = RECORD_INST_SIZE(field_count);

    hp = HAlloc(BIF_P, num_words_needed);

    res = make_record(hp);

    instance = (ErtsRecordInstance*)hp;
    instance->thing_word = old_instance->thing_word;
    instance->record_definition = old_instance->record_definition;

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
            res = ks[0];
            goto badfield;
        }
    } else {
        DECLARE_WSTACK(wstack);
        int j;
        Eterm *kv;
        struct erl_record_field *fields;
        const struct erl_record_field *fields_end;
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
            if (fields[0].key != defp->keys[i]) {
                *hp++ = old_values[i];
            } else {
                *hp++ = fields[0].value;
                fields++;
                if (fields >= fields_end) {
                    i++;
                    while (i < field_count) {
                        *hp++ = old_values[i];
                        i++;
                    }
                    erts_free(ERTS_ALC_T_TMP, tmp_array);
                    BIF_RET(res);
                }
            }
        }

        res = fields[0].key;
        erts_free(ERTS_ALC_T_TMP, tmp_array);
        goto badfield;
    }

    BIF_RET(res);

 badfield:
    {
        Eterm tmp;

        hp = HAlloc(BIF_P, 3 + 3);
        tmp = TUPLE2(hp, defp->module, defp->name);
        hp += 3;
        tmp = TUPLE2(hp, tmp, res);
        hp += 3;
        BIF_P->fvalue = tmp;
        BIF_ERROR(BIF_P, EXC_BADFIELD);
    }
}

BIF_RETTYPE records_get_2(BIF_ALIST_2) {
    ErtsRecordInstance *instance;
    ErtsRecordDefinition *defp;
    int field_count;
    Eterm record;
    Eterm key;
    Eterm *values;

    key = BIF_ARG_1;
    record = BIF_ARG_2;

    if (is_not_record(record) || is_not_atom(key)) {
        BIF_ERROR(BIF_P, BADARG);
    }

    instance = RECORD_INST_P(record);
    field_count = RECORD_INST_FIELD_COUNT(instance);
    defp = RECORD_DEF_P(instance);
    values = instance->values;

    for (int i = 0; i < field_count; i++) {
        if (eq(key, defp->keys[i])) {
            BIF_RET(values[i]);
        }
    }

    BIF_ERROR(BIF_P, BADARG);
}

BIF_RETTYPE records_get_module_1(BIF_ALIST_1) {
    ErtsRecordDefinition *defp;

    if (is_not_record(BIF_ARG_1)) {
        BIF_ERROR(BIF_P, BADARG);
    }

    defp = RECORD_DEF_P(RECORD_INST_P(BIF_ARG_1));
    BIF_RET(defp->module);
}

BIF_RETTYPE records_get_name_1(BIF_ALIST_1) {
    ErtsRecordDefinition *defp;

    if (is_not_record(BIF_ARG_1)) {
        BIF_ERROR(BIF_P, BADARG);
    }

    defp = RECORD_DEF_P(RECORD_INST_P(BIF_ARG_1));
    BIF_RET(defp->name);
}

BIF_RETTYPE records_get_field_names_1(BIF_ALIST_1) {
    Eterm obj;
    ErtsRecordInstance *instance;
    ErtsRecordDefinition *defp;
    int field_count;
    Eterm *hp;
    Eterm list;
    Eterm *order;

    obj = BIF_ARG_1;
    if (is_not_record(obj)) {
        BIF_P->fvalue = BIF_ARG_1;
        BIF_ERROR(BIF_P, EXC_BADRECORD);
    }

    instance = RECORD_INST_P(BIF_ARG_1);
    field_count = RECORD_INST_FIELD_COUNT(instance);
    defp = RECORD_DEF_P(instance);
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
    ErtsRecordDefinition *defp;

    obj = BIF_ARG_1;
    if (is_not_record(obj)) {
        BIF_P->fvalue = BIF_ARG_1;
        BIF_ERROR(BIF_P, EXC_BADRECORD);
    }

    defp = RECORD_DEF_P(RECORD_INST_P(BIF_ARG_1));
    BIF_RET(defp->is_exported);
}
