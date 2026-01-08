/*
 * %CopyrightBegin%
 *
 * SPDX-License-Identifier: Apache-2.0
 *
 * Copyright Ericsson AB 1996-2025. All Rights Reserved.
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

#ifndef ERTS_CODE_STAGED_PREFIX
#  error Missing definition of ERTS_CODE_STAGED_PREFIX
#endif
#ifndef ERTS_CODE_STAGED_OBJECT_TYPE
#  error Missing definition of ERTS_CODE_STAGED_OBJECT_TYPE
#endif
#ifndef ERTS_CODE_STAGED_OBJECT_HASH
#  error Missing definition of ERTS_CODE_STAGED_OBJECT_HASH
#endif
#ifndef ERTS_CODE_STAGED_OBJECT_COMPARE
#  error Missing definition of ERTS_CODE_STAGED_OBJECT_COMPARE
#endif
#ifndef ERTS_CODE_STAGED_OBJECT_INITIALIZE
#  error Missing definition of ERTS_CODE_STAGED_OBJECT_INITIALIZE
#endif
#ifndef ERTS_CODE_STAGED_OBJECT_STAGE
#  error Missing definition of ERTS_CODE_STAGED_OBJECT_STAGE
#endif
#ifndef ERTS_CODE_STAGED_OBJECT_ALLOC_TYPE
#  error Missing definition of ERTS_CODE_STAGED_OBJECT_ALLOC_TYPE
#endif
#ifndef ERTS_CODE_STAGED_TABLE_ALLOC_TYPE
#  error Missing definition of ERTS_CODE_STAGED_TABLE_ALLOC_TYPE
#endif
#ifndef ERTS_CODE_STAGED_TABLE_INITIAL_SIZE
#  error Missing definition of ERTS_CODE_STAGED_TABLE_INITIAL_SIZE
#endif
#ifndef ERTS_CODE_STAGED_TABLE_LIMIT
#  error Missing definition of ERTS_CODE_STAGED_TABLE_LIMIT
#endif

#undef ERTS_CODE_STAGED_PREFIX_STRING__2
#define ERTS_CODE_STAGED_PREFIX_STRING__2(x) \
    #x
#undef ERTS_CODE_STAGED_PREFIX_STRING__1
#define ERTS_CODE_STAGED_PREFIX_STRING__1(x) \
    ERTS_CODE_STAGED_PREFIX_STRING__2(x)
#undef ERTS_CODE_STAGED_PREFIX_STRING
#define ERTS_CODE_STAGED_PREFIX_STRING \
    ERTS_CODE_STAGED_PREFIX_STRING__1(ERTS_CODE_STAGED_PREFIX)

#undef ERTS_CODE_STAGED_CONCAT_MACRO_VALUES___
#define ERTS_CODE_STAGED_CONCAT_MACRO_VALUES___(X, Y) \
    X ## Y
#undef ERTS_CODE_STAGED_CONCAT_MACRO_VALUES__
#define ERTS_CODE_STAGED_CONCAT_MACRO_VALUES__(X, Y) \
    ERTS_CODE_STAGED_CONCAT_MACRO_VALUES___(X, Y)

#undef ERTS_CODE_STAGED_BLOB_T__
#define ERTS_CODE_STAGED_BLOB_T__ \
    ERTS_CODE_STAGED_CONCAT_MACRO_VALUES__(ERTS_CODE_STAGED_PREFIX, _blob_t)

#undef ERTS_CODE_STAGED_ENTRY_T__
#define ERTS_CODE_STAGED_ENTRY_T__ \
    ERTS_CODE_STAGED_CONCAT_MACRO_VALUES__(ERTS_CODE_STAGED_PREFIX, _entry_t)

#undef ERTS_CODE_STAGED_TEMPLATE_T__
#define ERTS_CODE_STAGED_TEMPLATE_T__ \
    ERTS_CODE_STAGED_CONCAT_MACRO_VALUES__(ERTS_CODE_STAGED_PREFIX, _template_t)

#undef ERTS_CODE_STAGED_FUNC__
#define ERTS_CODE_STAGED_FUNC__(Name) \
    ERTS_CODE_STAGED_CONCAT_MACRO_VALUES__(ERTS_CODE_STAGED_PREFIX,           \
                                           _staged_ ## Name)

typedef struct {
    IndexSlot slot; /* MUST BE LOCATED AT TOP OF STRUCT!!! */
    ERTS_CODE_STAGED_OBJECT_TYPE *object;
} ERTS_CODE_STAGED_ENTRY_T__;

typedef struct {
    ERTS_CODE_STAGED_OBJECT_TYPE object;
    ERTS_CODE_STAGED_ENTRY_T__ entryv[ERTS_NUM_CODE_IX];
} ERTS_CODE_STAGED_BLOB_T__;

typedef struct {
    ERTS_CODE_STAGED_ENTRY_T__ entry;
    ERTS_CODE_STAGED_OBJECT_TYPE object;
} ERTS_CODE_STAGED_TEMPLATE_T__;

/* Active code index is not locked */
static IndexTable ERTS_CODE_STAGED_CONCAT_MACRO_VALUES__
    (ERTS_CODE_STAGED_PREFIX, _tables)[ERTS_NUM_CODE_IX]; 

static erts_rwmtx_t ERTS_CODE_STAGED_CONCAT_MACRO_VALUES__
    (ERTS_CODE_STAGED_PREFIX, _rwmutex);

#ifdef ERTS_CODE_STAGED_WANT_ENTRY_BYTES
static erts_atomic_t ERTS_CODE_STAGED_CONCAT_MACRO_VALUES__
    (ERTS_CODE_STAGED_PREFIX, _total_entries_bytes);
#endif

#ifdef DEBUG
static int ERTS_CODE_STAGED_CONCAT_MACRO_VALUES__
    (ERTS_CODE_STAGED_PREFIX, _debug_stage_ix);
#endif

static ERTS_CODE_STAGED_BLOB_T__*
ERTS_CODE_STAGED_FUNC__(entry_to_blob)(ERTS_CODE_STAGED_ENTRY_T__ *entry)
{
    return ErtsContainerStruct(entry->object,
                               ERTS_CODE_STAGED_BLOB_T__,
                               object);
}

/* This lock must be taken in `erts_commit_staging_code_ix` to guarantee that
 * upserts do not break. */
void ERTS_CODE_STAGED_FUNC__(write_lock)(void);
void ERTS_CODE_STAGED_FUNC__(write_lock)(void)
{
    erts_rwmtx_t * const lock = &ERTS_CODE_STAGED_CONCAT_MACRO_VALUES__
                                 (ERTS_CODE_STAGED_PREFIX, _rwmutex);
    erts_rwmtx_rwlock(lock);
}

void ERTS_CODE_STAGED_FUNC__(write_unlock)(void);
void ERTS_CODE_STAGED_FUNC__(write_unlock)(void)
{
    erts_rwmtx_t * const lock = &ERTS_CODE_STAGED_CONCAT_MACRO_VALUES__
                                 (ERTS_CODE_STAGED_PREFIX, _rwmutex);
    erts_rwmtx_rwunlock(lock);
}

static void
ERTS_CODE_STAGED_FUNC__(read_lock)(void)
{
    erts_rwmtx_t * const lock = &ERTS_CODE_STAGED_CONCAT_MACRO_VALUES__
                                 (ERTS_CODE_STAGED_PREFIX, _rwmutex);
    erts_rwmtx_rlock(lock);
}

static void
ERTS_CODE_STAGED_FUNC__(read_unlock)(void)
{
    erts_rwmtx_t * const lock = &ERTS_CODE_STAGED_CONCAT_MACRO_VALUES__
                                 (ERTS_CODE_STAGED_PREFIX, _rwmutex);
    erts_rwmtx_runlock(lock);
}

static void
ERTS_CODE_STAGED_FUNC__(info)(fmtfn_t to, void *to_arg)
{
    IndexTable * const tables = ERTS_CODE_STAGED_CONCAT_MACRO_VALUES__
                                 (ERTS_CODE_STAGED_PREFIX, _tables);
    const int lock = !ERTS_IS_CRASH_DUMPING;

    if (lock) {
        ERTS_CODE_STAGED_FUNC__(write_lock)();
    }

    index_info(to, to_arg, &tables[erts_active_code_ix()]);
    hash_info(to, to_arg, &tables[erts_staging_code_ix()].htable);

    if (lock) {
        ERTS_CODE_STAGED_FUNC__(write_unlock)();
    }
}

static HashValue
ERTS_CODE_STAGED_FUNC__(hash)(ERTS_CODE_STAGED_ENTRY_T__ *entry)
{
    return ERTS_CODE_STAGED_OBJECT_HASH(entry->object);
}

static int
ERTS_CODE_STAGED_FUNC__(cmp)(ERTS_CODE_STAGED_ENTRY_T__ *lhs,
                        ERTS_CODE_STAGED_ENTRY_T__ *rhs)
{
    return ERTS_CODE_STAGED_OBJECT_COMPARE(lhs->object, rhs->object);
}

static ERTS_CODE_STAGED_OBJECT_TYPE *
ERTS_CODE_STAGED_FUNC__(init_template)(ERTS_CODE_STAGED_TEMPLATE_T__ *template)
{
    template->entry.object = &template->object;
    template->entry.slot.index = -1;
    return &template->object;
}

static ERTS_CODE_STAGED_OBJECT_TYPE *
ERTS_CODE_STAGED_FUNC__(upsert)(ERTS_CODE_STAGED_TEMPLATE_T__ *template)
{
    IndexTable * const tables = ERTS_CODE_STAGED_CONCAT_MACRO_VALUES__
                                 (ERTS_CODE_STAGED_PREFIX, _tables);
    ERTS_CODE_STAGED_ENTRY_T__ *entry;
    int requires_insertion = 0;
#ifdef DEBUG
    int retry_after_race = 0;
#endif

    do {
        ErtsCodeIndex active_ix = erts_active_code_ix();

        entry = hash_get(&tables[active_ix].htable, &template->entry);
        if (entry != NULL) {
            break;
        }

        /* The entry is not active yet, so we need to consult or insert
         * into the staging table, which is protected by the staging
         * lock.
         *
         * To be absolutely certain that we're working on the _current_
         * staging table, we take the lock and double-check the active code
         * index in case we've raced with a code index change. This works
         * because code index changes acquire the same lock. */
        if (!requires_insertion) {
            ERTS_CODE_STAGED_FUNC__(read_lock)();
        } else {
            ERTS_CODE_STAGED_FUNC__(write_lock)();
        }

        if (active_ix == erts_active_code_ix()) {
            ErtsCodeIndex staging_ix = erts_staging_code_ix();

            if (!requires_insertion) {
                entry = (ERTS_CODE_STAGED_ENTRY_T__*)
                    hash_get(&tables[staging_ix].htable,
                             &template->entry);
                ERTS_CODE_STAGED_FUNC__(read_unlock)();

                /* If the staging table did not contain the entry, try to
                 * insert it under a write lock on the next pass. */
                requires_insertion = (entry == NULL);
            } else {
                entry = (ERTS_CODE_STAGED_ENTRY_T__*)
                    index_put_entry(&tables[staging_ix],
                                    &template->entry);
                ERTS_CODE_STAGED_FUNC__(write_unlock)();
            }
        } else {
            /* Raced with code index change, try again. This can only happen
             * once per thread progress tick. */
#ifdef DEBUG
            ASSERT(retry_after_race == 0);
            retry_after_race = 1;
#endif

            if (!requires_insertion) {
                ERTS_CODE_STAGED_FUNC__(read_unlock)();
            } else {
                ERTS_CODE_STAGED_FUNC__(write_unlock)();
            }
        }
    } while (entry == NULL);

    return entry->object;
}

static ERTS_CODE_STAGED_ENTRY_T__ *
ERTS_CODE_STAGED_FUNC__(alloc)(ERTS_CODE_STAGED_ENTRY_T__ *template)
{
    ERTS_CODE_STAGED_BLOB_T__ *blob;

    if (template->slot.index == -1) {
        /* Template, allocate blob */
        blob = (ERTS_CODE_STAGED_BLOB_T__*)
            erts_alloc(ERTS_CODE_STAGED_OBJECT_ALLOC_TYPE, sizeof(*blob));

#ifdef ERTS_CODE_STAGED_WANT_ENTRY_BYTES
        erts_atomic_add_nob(&ERTS_CODE_STAGED_CONCAT_MACRO_VALUES__
                             (ERTS_CODE_STAGED_PREFIX, _total_entries_bytes),
                            sizeof(*blob));
#endif

        ERTS_CODE_STAGED_OBJECT_INITIALIZE(&blob->object, template->object);

        for (int ix = 0; ix < ERTS_NUM_CODE_IX; ix++) {
            blob->entryv[ix].slot.index = -1;
            blob->entryv[ix].object = &blob->object;
        }

        return &blob->entryv[0];
    } else {
        /* Existing entry in another table, use free entry in blob */
        int ix;

        blob = ERTS_CODE_STAGED_FUNC__(entry_to_blob)(template);
        for (ix = 0; blob->entryv[ix].slot.index >= 0; ix++) {
            ASSERT(ix < ERTS_NUM_CODE_IX);
        }

        return &blob->entryv[ix];
    }
}

static void
ERTS_CODE_STAGED_FUNC__(free)(ERTS_CODE_STAGED_ENTRY_T__ *entry)
{
    ERTS_CODE_STAGED_BLOB_T__ *blob =
        ERTS_CODE_STAGED_FUNC__(entry_to_blob)(entry);

    entry->slot.index = -1;
    for (int ix = 0; ix < ERTS_NUM_CODE_IX; ix++) {
        if (blob->entryv[ix].slot.index >= 0) {
            return;
        }
    }

#ifdef ERTS_CODE_STAGED_WANT_ENTRY_BYTES
    erts_atomic_add_nob(&ERTS_CODE_STAGED_CONCAT_MACRO_VALUES__
                         (ERTS_CODE_STAGED_PREFIX, _total_entries_bytes),
                        -sizeof(*blob));
#endif

    erts_free(ERTS_CODE_STAGED_OBJECT_ALLOC_TYPE, blob);
}

static void
ERTS_CODE_STAGED_FUNC__(init)(void)
{
    IndexTable * const tables = ERTS_CODE_STAGED_CONCAT_MACRO_VALUES__
                                 (ERTS_CODE_STAGED_PREFIX, _tables);
    HashFunctions f;

    erts_rwmtx_opt_t rwmtx_opt = ERTS_RWMTX_OPT_DEFAULT_INITER;
    rwmtx_opt.type = ERTS_RWMTX_TYPE_FREQUENT_READ;
    rwmtx_opt.lived = ERTS_RWMTX_LONG_LIVED;

    erts_rwmtx_init_opt(&ERTS_CODE_STAGED_CONCAT_MACRO_VALUES__
                         (ERTS_CODE_STAGED_PREFIX, _rwmutex),
                        &rwmtx_opt,
                        ERTS_CODE_STAGED_PREFIX_STRING "_staging_lock",
                        NIL,
                        (ERTS_LOCK_FLAGS_PROPERTY_STATIC |
                         ERTS_LOCK_FLAGS_CATEGORY_GENERIC));

#ifdef ERTS_CODE_STAGED_WANT_ENTRY_BYTES
    erts_atomic_init_nob(&ERTS_CODE_STAGED_CONCAT_MACRO_VALUES__
                          (ERTS_CODE_STAGED_PREFIX, _total_entries_bytes),
                         0);
#endif

    f.hash = (H_FUN) ERTS_CODE_STAGED_FUNC__(hash);
    f.cmp  = (HCMP_FUN) ERTS_CODE_STAGED_FUNC__(cmp);
    f.alloc = (HALLOC_FUN) ERTS_CODE_STAGED_FUNC__(alloc);
    f.free = (HFREE_FUN) ERTS_CODE_STAGED_FUNC__(free);
    f.meta_alloc = (HMALLOC_FUN)erts_alloc;
    f.meta_free = (HMFREE_FUN)erts_free;
    f.meta_print = (HMPRINT_FUN)erts_print;

    for (int ix = 0; ix < ERTS_NUM_CODE_IX; ix++) {
        erts_index_init(ERTS_CODE_STAGED_TABLE_ALLOC_TYPE,
                        &tables[ix],
                        ERTS_CODE_STAGED_PREFIX_STRING "_staged_index",
                        ERTS_CODE_STAGED_TABLE_INITIAL_SIZE,
                        ERTS_CODE_STAGED_TABLE_LIMIT,
                        f);
    }
}

static void
ERTS_CODE_STAGED_FUNC__(start_staging)(void)
{
    IndexTable * const tables = ERTS_CODE_STAGED_CONCAT_MACRO_VALUES__
                                 (ERTS_CODE_STAGED_PREFIX, _tables);
    const ErtsCodeIndex dst_ix = erts_staging_code_ix();
    const ErtsCodeIndex src_ix = erts_active_code_ix();
    IndexTable *dst = &tables[dst_ix];
    IndexTable *src = &tables[src_ix];

    ASSERT(dst_ix != src_ix);
    ASSERT(ERTS_CODE_STAGED_CONCAT_MACRO_VALUES__
           (ERTS_CODE_STAGED_PREFIX, _debug_stage_ix) == ~0);

    ERTS_CODE_STAGED_FUNC__(write_lock)();

    /* Insert all entries in src into dst table */
    for (int ix = 0; ix < src->entries; ix++) {
        ERTS_CODE_STAGED_ENTRY_T__ *src_entry;

        src_entry = (ERTS_CODE_STAGED_ENTRY_T__*)erts_index_lookup(src, ix);
        ERTS_CODE_STAGED_OBJECT_STAGE(src_entry->object, src_ix, dst_ix);

#ifndef DEBUG
        index_put_entry(dst, src_entry);
#else /* DEBUG */
        {
            ERTS_CODE_STAGED_ENTRY_T__* dst_entry =
                (ERTS_CODE_STAGED_ENTRY_T__*)index_put_entry(dst, src_entry);
            ASSERT(ERTS_CODE_STAGED_FUNC__(entry_to_blob)(src_entry)
                    == ERTS_CODE_STAGED_FUNC__(entry_to_blob)(dst_entry));
        }
#endif
    }

    ERTS_CODE_STAGED_FUNC__(write_unlock)();

#ifdef DEBUG
    ERTS_CODE_STAGED_CONCAT_MACRO_VALUES__
     (ERTS_CODE_STAGED_PREFIX, _debug_stage_ix) = dst_ix;
#endif
}

static void
ERTS_CODE_STAGED_FUNC__(end_staging)(int commit)
{
#ifdef DEBUG
    ASSERT(ERTS_CODE_STAGED_CONCAT_MACRO_VALUES__
           (ERTS_CODE_STAGED_PREFIX, _debug_stage_ix) == erts_staging_code_ix());
    ERTS_CODE_STAGED_CONCAT_MACRO_VALUES__
     (ERTS_CODE_STAGED_PREFIX, _debug_stage_ix) = ~0;
#endif
}

#ifdef ERTS_CODE_STAGED_WANT_GET
static const ERTS_CODE_STAGED_OBJECT_TYPE *
ERTS_CODE_STAGED_FUNC__(get)(ERTS_CODE_STAGED_TEMPLATE_T__ *template,
                        ErtsCodeIndex code_ix)
{
    IndexTable * const tables = ERTS_CODE_STAGED_CONCAT_MACRO_VALUES__
                                 (ERTS_CODE_STAGED_PREFIX, _tables);
    ERTS_CODE_STAGED_ENTRY_T__ *entry;

    entry = hash_get(&tables[code_ix].htable, &template->entry);
    if (entry) {
        return entry->object;
    }

    return NULL;
}
#endif

#ifdef ERTS_CODE_STAGED_WANT_PUT
static ERTS_CODE_STAGED_OBJECT_TYPE *
ERTS_CODE_STAGED_FUNC__(put)(ERTS_CODE_STAGED_TEMPLATE_T__ *template)
{
    IndexTable * const tables = ERTS_CODE_STAGED_CONCAT_MACRO_VALUES__
                                 (ERTS_CODE_STAGED_PREFIX, _tables);
    const ErtsCodeIndex code_ix = erts_staging_code_ix();
    ERTS_CODE_STAGED_ENTRY_T__ *entry;

    ERTS_CODE_STAGED_FUNC__(write_lock)();
    entry = (ERTS_CODE_STAGED_ENTRY_T__*)index_put_entry(&tables[code_ix],
                                                    &template->entry);
    ERTS_CODE_STAGED_FUNC__(write_unlock)();

    return entry->object;
}
#endif

#if defined(ERTS_CODE_STAGED_WANT_FOREACH) || \
    defined(ERTS_CODE_STAGED_WANT_FOREACH_ACTIVE)
static void
ERTS_CODE_STAGED_FUNC__(foreach)(
    void (*callback)(ERTS_CODE_STAGED_OBJECT_TYPE *object,
                     void *arg),
    void *arg,
    ErtsCodeIndex code_ix)
{
    IndexTable * const tables = ERTS_CODE_STAGED_CONCAT_MACRO_VALUES__
                                 (ERTS_CODE_STAGED_PREFIX, _tables);
    IndexTable *table = &tables[code_ix];

    for (int ix = 0; ix < table->entries; ix++) {
        ERTS_CODE_STAGED_ENTRY_T__ *src_entry =
            (ERTS_CODE_STAGED_ENTRY_T__*)erts_index_lookup(table, ix);
        callback(src_entry->object, arg);
    }
}
#endif

#ifdef ERTS_CODE_STAGED_WANT_FOREACH_ACTIVE
static void
ERTS_CODE_STAGED_FUNC__(foreach_active)(
    void (*callback)(ERTS_CODE_STAGED_OBJECT_TYPE *object,
                     void *arg),
    void *arg)
{
    ERTS_CODE_STAGED_FUNC__(foreach)(callback, arg, erts_active_code_ix());
}
#endif

#ifdef ERTS_CODE_STAGED_WANT_LIST
static ERTS_CODE_STAGED_OBJECT_TYPE *
ERTS_CODE_STAGED_FUNC__(list)(int i, ErtsCodeIndex code_ix)
{
    IndexTable * const tables = ERTS_CODE_STAGED_CONCAT_MACRO_VALUES__
                                 (ERTS_CODE_STAGED_PREFIX, _tables);
    ERTS_CODE_STAGED_ENTRY_T__ *entry =
        (ERTS_CODE_STAGED_ENTRY_T__*)erts_index_lookup(&tables[code_ix], i);
    return entry->object;
}
#endif

#ifdef ERTS_CODE_STAGED_WANT_LIST_SIZE
static int
ERTS_CODE_STAGED_FUNC__(list_size)(ErtsCodeIndex code_ix)
{
    IndexTable * const tables = ERTS_CODE_STAGED_CONCAT_MACRO_VALUES__
                                (ERTS_CODE_STAGED_PREFIX, _tables);
    return erts_index_num_entries(&tables[code_ix]);
}
#endif

#ifdef ERTS_CODE_STAGED_WANT_TABLE_SIZE
static int
ERTS_CODE_STAGED_FUNC__(table_size)(void)
{
    IndexTable * const tables = ERTS_CODE_STAGED_CONCAT_MACRO_VALUES__
                                (ERTS_CODE_STAGED_PREFIX, _tables);
    const int lock = !ERTS_IS_CRASH_DUMPING;
    UWord size = 0;

    if (lock) {
        ERTS_CODE_STAGED_FUNC__(read_lock)();
    }

    for (int ix = 0; ix < ERTS_NUM_CODE_IX; ix++) {
        size += index_table_sz(&tables[ix]);
    }

    if (lock) {
        ERTS_CODE_STAGED_FUNC__(read_unlock)();
    }

    return size;
}
#endif

#ifdef ERTS_CODE_STAGED_WANT_ENTRY_BYTES
static int
ERTS_CODE_STAGED_FUNC__(entry_bytes)(void)
{
    return erts_atomic_read_nob(&ERTS_CODE_STAGED_CONCAT_MACRO_VALUES__
                                 (ERTS_CODE_STAGED_PREFIX,
                                  _total_entries_bytes));
}
#endif
