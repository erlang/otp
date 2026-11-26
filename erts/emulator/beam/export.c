/*
 * %CopyrightBegin%
 *
 * SPDX-License-Identifier: Apache-2.0
 *
 * Copyright Ericsson AB 1996-2026. All Rights Reserved.
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
#include "export.h"
#include "hash.h"
#include "jit/beam_asm.h"
#include "erl_global_literals.h"
#include "erl_term_hashing.h"

#define EXPORT_LIMIT          (512*1024)

#ifdef DEBUG
#  define IF_DEBUG(x) x
#else
#  define IF_DEBUG(x)
#endif

/* A static open-addressed table from {M, F, A} to Export*,
 * together with an auxiliary static ExportList.
 *
 * Each 16-byte slot holds a key (the MFA packed into a word on 64bit arch, the
 * MFA itself on 32bit arch) and the Export*, four to a typical 64B cache line,
 * so when kept at max 70% load, the vast majority of the answers for both hit
 * and miss are available in a single memory read.
 *
 * Because allocations are 8B-aligned, the low 3 bits of a pointer are
 * guaranteed zero, hence these bits of the Export* are reused to tell which
 * code indexes' tables hold it, and are set under the write lock as it's added
 * to each. Making a stub ignores them, as any export is either active or in the
 * staging table. Finding an entry answers from its code index's bit, found or
 * not: only the staging table grows, and the bits of the others were set before
 * the code barrier that activated them.
 *
 * On 64bit arch, MFAs that don't pack (atom indexes of 2^28 or more, and
 * false:false/0, which are arguably unlikely) are kept in a hash table under
 * the lock instead, with their position in the list.
 *
 * The auxiliary static ExportList keeps all exports in the order they were
 * created, and the list of each code index is always a prefix of it: its first
 * `exports.lengths[ix]` exports. That holds because exports are never removed,
 * are only created in the staging table, and the staging table is brought up to
 * the whole list before an export is created in it or code is staged in it.
 *
 * A reader that hasn't synchronized with a commit may see its code index
 * before the bits set for it, and is then answered as the previous code
 * index would. That is a valid answer for such a reader, and cheaper than
 * ordering every code index read. */

static erts_rwmtx_t export_rwmutex;

#define EXPORT_LIST_PAGE_SHIFT 10
#define EXPORT_LIST_PAGE_SIZE (1 << EXPORT_LIST_PAGE_SHIFT)
#define EXPORT_LIST_PAGE_MASK (EXPORT_LIST_PAGE_SIZE - 1)

typedef struct {
    Export ***pages;  /* `limit` / page size, each allocated when first used */
    Uint count;       /* under the write lock */
    int limit;        /* a whole number of pages */
    erts_atomic_t lengths[ERTS_NUM_CODE_IX]; /* code index ix's table is the first lengths[ix] */
} ExportList;

static ExportList exports = {.limit = EXPORT_LIMIT};
static erts_atomic_t export_bytes;

#ifdef DEBUG
/* Boot ends staging without starting it, with both code indexes 0. */
static int export_debug_stage_ix = 0;
#endif

static ERTS_INLINE Export *export_list_at(Uint i)
{
    return exports.pages[i >> EXPORT_LIST_PAGE_SHIFT]
                        [i & EXPORT_LIST_PAGE_MASK];
}

/* This lock must be taken in `erts_commit_staging_code_ix` so that the
 * staging code index can't change under a writer. */
void export_staged_write_lock(void);
void export_staged_write_lock(void)
{
    erts_rwmtx_rwlock(&export_rwmutex);
}

void export_staged_write_unlock(void);
void export_staged_write_unlock(void)
{
    erts_rwmtx_rwunlock(&export_rwmutex);
}

static void create_shared_lambda(Export *export)
{
    ErlFunThing *lambda;
    struct erl_off_heap_header **ohp;

    lambda = (ErlFunThing*)erts_global_literal_allocate(ERL_FUN_SIZE, &ohp);

    lambda->thing_word = MAKE_FUN_HEADER(export->info.mfa.arity, 0, 1);
    lambda->entry.exp = export;

    export->lambda = make_fun(lambda);

    erts_global_literal_register(&export->lambda);
}

static void export_init(Export *dst, const ErtsCodeMFA *mfa)
{
    sys_memset(&dst->info.u, 0, sizeof(dst->info.u));
    dst->info.gen_bp = NULL;
    dst->info.mfa = *mfa;
    dst->bif_number = -1;
    dst->is_bif_traced = 0;

    create_shared_lambda(dst);

    sys_memset(&dst->trampoline, 0, sizeof(dst->trampoline));

    if (BeamOpsAreInitialized()) {
        dst->trampoline.common.op = BeamOpCodeAddr(op_call_error_handler);
    }

    for (int ix = 0; ix < ERTS_NUM_CODE_IX; ix++) {
        erts_activate_export_trampoline(dst, ix);
    }

#ifdef BEAMASM
    dst->dispatch.addresses[ERTS_SAVE_CALLS_CODE_IX] =
        beam_save_calls_export;
#endif
}

static void export_stage(Export *export,
                         ErtsCodeIndex src_ix,
                         ErtsCodeIndex dst_ix)
{
    ErtsDispatchable *dispatch = &export->dispatch;
    dispatch->addresses[dst_ix] = dispatch->addresses[src_ix];
}

/* On 64-bit a key is the MFA packed into one word, which takes atom indexes
 * below 2^28 and keeps a slot at 16 bytes. On 32-bit a slot of four words
 * holds the MFA itself in the same 16 bytes, so every MFA has a key and the
 * side table stays empty. */
#ifdef ARCH_64
#  define EXPORT_KEY_PACKED 1
#else
#  define EXPORT_KEY_PACKED 0
#endif

#define EXPORT_ACCEL_MAX_LOAD 70 /* percent */
#define EXPORT_ACCEL_INITIAL_SIZE (1 << 13)
/* Exports are at least 8-byte aligned, leaving a bit per code index. */
#define EXPORT_ACCEL_IX_BITS ((erts_aint_t)((1 << ERTS_NUM_CODE_IX) - 1))

#if EXPORT_KEY_PACKED
#define EXPORT_ACCEL_ATOM_BITS 28

/* m << 36 | f << 8 | a, or 0 when that doesn't fit */
typedef Uint64 ExportKey;

typedef struct {
    erts_atomic_t key;    /* 0 when the slot is empty, written last */
    erts_atomic_t export; /* Export* | code index bits */
} ExportAccelSlot;
#else
/* The MFA itself. No atom is 0, so a module of 0 marks an empty slot. */
typedef ErtsCodeMFA ExportKey;

typedef struct {
    erts_atomic_t module; /* 0 when the slot is empty, written last */
    erts_atomic_t function;
    erts_atomic_t arity;
    erts_atomic_t export; /* Export* | code index bits */
} ExportAccelSlot;
#endif

/* Written only when created, as every lookup reads it. */
typedef struct {
    ExportAccelSlot *slots; /* cache line aligned, within this allocation */
    Uint size;
    int shift;              /* 64 - log2(size) */
    ErtsThrPrgrLaterOp later_op;
} ExportAccel;

/* The current ExportAccel*, alone on its cache line for the same reason. */
static union {
    erts_atomic_t accel;
    char align__[ERTS_CACHE_LINE_SIZE];
} export_accel erts_align_attribute(ERTS_CACHE_LINE_SIZE);

static Uint export_accel_used; /* under the write lock */

static ERTS_INLINE ExportKey export_key(Eterm m, Eterm f, Uint a)
{
#if EXPORT_KEY_PACKED
    const Uint64 mv = atom_val(m), fv = atom_val(f);

    if (ERTS_UNLIKELY(mv >= (1 << EXPORT_ACCEL_ATOM_BITS) ||
                      fv >= (1 << EXPORT_ACCEL_ATOM_BITS) || a > 255)) {
        return 0;
    }

    return (mv << 36) | (fv << 8) | a;
#else
    const ExportKey key = {m, f, a};

    return key;
#endif
}

/* Whether the export is in the index rather than the side table. */
static ERTS_INLINE int export_key_indexed(ExportKey key)
{
#if EXPORT_KEY_PACKED
    return key != 0;
#else
    (void)key;
    return 1;
#endif
}

static ERTS_INLINE Uint64 export_key_hash(ExportKey key)
{
#if EXPORT_KEY_PACKED
    return erts_ihash_mix64(key);
#else
    /* On 32-bit 26 + 26 + 8 bits:  Atom indexes are below 2^26 on 32-bit,
     * so this is lossless, the key is packed only to hash it, never stored. */

    return erts_ihash_mix64(((Uint64)atom_val(key.module) << 34) ^
                            ((Uint64)atom_val(key.function) << 8) ^ key.arity);
#endif
}

static ERTS_INLINE int export_slot_used(ExportAccelSlot *slot)
{
#if EXPORT_KEY_PACKED
    return erts_atomic_read_nob(&slot->key) != 0;
#else
    return erts_atomic_read_nob(&slot->module) != 0;
#endif
}

/* Only for a used slot. */
static ERTS_INLINE ExportKey export_slot_key(ExportAccelSlot *slot)
{
#if EXPORT_KEY_PACKED
    return erts_atomic_read_nob(&slot->key);
#else
    return export_key(erts_atomic_read_nob(&slot->module),
                      erts_atomic_read_nob(&slot->function),
                      erts_atomic_read_nob(&slot->arity));
#endif
}

/* The key goes last, with a release barrier,
 * so a reader that sees it sees the whole slot. */
static ERTS_INLINE void export_slot_publish(ExportAccelSlot *slot,
                                            ExportKey key, erts_aint_t word)
{
    erts_atomic_set_nob(&slot->export, word);
#if EXPORT_KEY_PACKED
    erts_atomic_set_relb(&slot->key, (erts_aint_t)key);
#else
    erts_atomic_set_nob(&slot->function, key.function);
    erts_atomic_set_nob(&slot->arity, key.arity);
    erts_atomic_set_relb(&slot->module, key.module);
#endif
}

/* Returns the slot holding `key`, or the empty one where it would go. Only for
 * writers, under the write lock. */
static ERTS_INLINE ExportAccelSlot *export_accel_slot(ExportAccel *accel,
                                                     ExportKey key)
{
    ExportAccelSlot *const slots = accel->slots;
    const Uint mask = accel->size - 1;

    /* The index is never full, so this terminates. */
    for (Uint ix = export_key_hash(key) >> accel->shift; ; ix = (ix + 1) & mask) {
        ExportAccelSlot *slot = &slots[ix];
#if EXPORT_KEY_PACKED
        const Uint64 slot_key = erts_atomic_read_nob(&slot->key);

        if (slot_key == key || slot_key == 0) {
            return slot;
        }
#else
        const Eterm module = erts_atomic_read_nob(&slot->module);

        if (module == 0 ||
            (module == key.module &&
             erts_atomic_read_nob(&slot->function) == key.function &&
             erts_atomic_read_nob(&slot->arity) == key.arity)) {
            return slot;
        }
#endif
    }
}

/* The slot's Export* and code index bits, or 0. Lock-free, so each key is read
 * once: a slot found empty may be filled right after, with another key. The
 * index's fields are read into locals as each acquire is a compiler barrier
 * that would otherwise reload them on every probe. */
static ERTS_INLINE erts_aint_t export_accel_lookup(ExportKey key)
{
    const ExportAccel *accel =
        (ExportAccel*)erts_atomic_read_ddrb(&export_accel.accel);
    ExportAccelSlot *const slots = accel->slots;
    const Uint mask = accel->size - 1;

    /* The index is never full, so this terminates. */
    for (Uint ix = export_key_hash(key) >> accel->shift; ; ix = (ix + 1) & mask) {
        ExportAccelSlot *slot = &slots[ix];
#if EXPORT_KEY_PACKED
        const Uint64 slot_key = erts_atomic_read_acqb(&slot->key);

        if (slot_key == key) {
            return erts_atomic_read_nob(&slot->export);
        } else if (slot_key == 0) {
            return 0;
        }
#else
        const Eterm module = erts_atomic_read_acqb(&slot->module);

        if (module == 0) {
            return 0;
        } else if (module == key.module &&
                   erts_atomic_read_nob(&slot->function) == key.function &&
                   erts_atomic_read_nob(&slot->arity) == key.arity) {
            return erts_atomic_read_nob(&slot->export);
        }
#endif
    }
}

/* The Export* if the table of `code_ix` holds it, or NULL. */
static ERTS_INLINE Export *export_accel_find(ExportKey key, ErtsCodeIndex code_ix)
{
    erts_aint_t word = export_accel_lookup(key);

    return (word & ((erts_aint_t)1 << code_ix)) ?
        (Export*)(word & ~EXPORT_ACCEL_IX_BITS) : NULL;
}

static Uint export_accel_bytes(Uint size)
{
    return sizeof(ExportAccel) + ERTS_CACHE_LINE_SIZE +
           size * sizeof(ExportAccelSlot);
}

/* An empty index of `size` slots, a power of two. */
static ExportAccel *export_accel_alloc(Uint size)
{
    ExportAccel *accel;

    ASSERT(size != 0 && (size & (size - 1)) == 0);

    accel = erts_alloc(ERTS_ALC_T_EXPORT_TABLE, export_accel_bytes(size));
    accel->slots = (ExportAccelSlot*)
        (((UWord)(accel + 1) + ERTS_CACHE_LINE_MASK) & ~ERTS_CACHE_LINE_MASK);
    accel->size = size;
    /* hash >> shift keeps the top log2(size) bits of the hash. */
    accel->shift = 64 - (erts_fit_in_bits_uint(size) - 1);

    for (Uint ix = 0; ix < size; ix++) {
        ExportAccelSlot *slot = &accel->slots[ix];

#if EXPORT_KEY_PACKED
        erts_atomic_init_nob(&slot->key, 0);
#else
        erts_atomic_init_nob(&slot->module, 0);
        erts_atomic_init_nob(&slot->function, 0);
        erts_atomic_init_nob(&slot->arity, 0);
#endif
        erts_atomic_init_nob(&slot->export, 0);
    }

    return accel;
}

static void export_accel_free(void *accel)
{
    erts_free(ERTS_ALC_T_EXPORT_TABLE, accel);
}

static void export_accel_free_later(void *vaccel)
{
    ExportAccel *accel = (ExportAccel*)vaccel;

    erts_schedule_thr_prgr_later_cleanup_op(export_accel_free, accel,
                                            &accel->later_op,
                                            export_accel_bytes(accel->size));
}

/* Must be called with the write lock held, as must everything that writes.
 * Returns whether the key was new. */
static int export_accel_store(ExportAccel *accel, ExportKey key, erts_aint_t word)
{
    ExportAccelSlot *slot = export_accel_slot(accel, key);
    erts_aint_t old = erts_atomic_read_nob(&slot->export);

    if (!export_slot_used(slot)) {
        export_slot_publish(slot, key, word);
        return 1;
    } else if ((old | word) != old) {
        ASSERT((old & ~EXPORT_ACCEL_IX_BITS) == (word & ~EXPORT_ACCEL_IX_BITS));
        erts_atomic_set_relb(&slot->export, old | word);
    }

    return 0;
}

/* Adds a new export, or the bit of `code_ix` to one already there. */
static void export_accel_insert(Export *export, ExportKey key,
                                ErtsCodeIndex code_ix)
{
    ExportAccel *accel = (ExportAccel*)erts_atomic_read_nob(&export_accel.accel);

    if (export_accel_used >= (accel->size / 100) * EXPORT_ACCEL_MAX_LOAD) {
        ExportAccel *old = accel;

        accel = export_accel_alloc(old->size * 2);
        for (Uint ix = 0; ix < old->size; ix++) {
            ExportAccelSlot *slot = &old->slots[ix];

            if (export_slot_used(slot)) {
                export_accel_store(accel, export_slot_key(slot),
                                   erts_atomic_read_nob(&slot->export));
            }
        }

        erts_atomic_set_relb(&export_accel.accel, (erts_aint_t)accel);
        if (!erts_initialized) {
            export_accel_free(old); /* No other thread runs yet. */
        } else {
            /* Thread progress operations must be scheduled from a managed
             * thread, and this can run on any. */
            erts_schedule_misc_aux_work(1, export_accel_free_later, old);
        }
    }

    ASSERT(((UWord)export & EXPORT_ACCEL_IX_BITS) == 0);
    export_accel_used += export_accel_store(accel, key,
                                            (erts_aint_t)export | (1 << code_ix));
}

/* The exports whose keys don't pack, on 64-bit only. */
typedef struct {
    HashBucket bucket; /* MUST BE LOCATED AT TOP OF STRUCT!!! */
    const ErtsCodeMFA *mfa;
    Export *export;
    Uint position;     /* in the list */
} ExportSideEntry;

static Hash export_side;

static HashValue export_side_hash(ExportSideEntry *entry)
{
    return (atom_val(entry->mfa->module) * atom_val(entry->mfa->function)) ^
           entry->mfa->arity;
}

static int export_side_cmp(ExportSideEntry *lhs, ExportSideEntry *rhs)
{
    return !(lhs->mfa->module == rhs->mfa->module &&
             lhs->mfa->function == rhs->mfa->function &&
             lhs->mfa->arity == rhs->mfa->arity);
}

static ExportSideEntry *export_side_alloc(ExportSideEntry *template)
{
    ExportSideEntry *entry = erts_alloc(ERTS_ALC_T_EXPORT, sizeof(*entry));

    entry->export = template->export;
    entry->mfa = &template->export->info.mfa;
    entry->position = template->position;
    erts_atomic_add_nob(&export_bytes, sizeof(*entry));

    return entry;
}

static void export_side_free(ExportSideEntry *entry)
{
    erts_free(ERTS_ALC_T_EXPORT, entry); /* Never called. */
}

/* Must be called with the lock held, in either mode. */
static ExportSideEntry *export_side_get(const ErtsCodeMFA *mfa)
{
    ExportSideEntry template;

    template.mfa = mfa;
    return (ExportSideEntry*)hash_get(&export_side, &template);
}

/* The export in any table, or NULL. Must be called with the lock held, and
 * looks at the current index rather than one read before taking it. */
static Export *export_get_locked(const ErtsCodeMFA *mfa, ExportKey key)
{
    if (export_key_indexed(key)) {
        return (Export*)(export_accel_lookup(key) & ~EXPORT_ACCEL_IX_BITS);
    } else {
        ExportSideEntry *entry = export_side_get(mfa);

        return entry ? entry->export : NULL;
    }
}

/* Brings the table of `code_ix` up to the whole list. Must be called with
 * the write lock held, on the staging table. */
static void export_catch_up(ErtsCodeIndex code_ix)
{
    const Uint from = erts_atomic_read_nob(&exports.lengths[code_ix]);

    if (from == exports.count) {
        return;
    }

    for (Uint i = from; i < exports.count; i++) {
        Export *export = export_list_at(i);
        const ErtsCodeMFA *mfa = &export->info.mfa;
        ExportKey key = export_key(mfa->module, mfa->function, mfa->arity);

        if (export_key_indexed(key)) {
            export_accel_insert(export, key, code_ix);
        }
    }

    erts_atomic_set_relb(&exports.lengths[code_ix], exports.count);
}

/* Creates the export in the staging table, which must be caught up. Must be
 * called with the write lock held, after a miss under it. */
static Export *export_create(const ErtsCodeMFA *mfa, ExportKey key)
{
    const ErtsCodeIndex staging_code_ix = erts_staging_code_ix();
    const Uint ix = exports.count;
    Export *export;

    ASSERT(erts_atomic_read_nob(&exports.lengths[staging_code_ix]) == ix);

    if ((ix & EXPORT_LIST_PAGE_MASK) == 0) {
        if (ix >= exports.limit) {
            /* A core dump is unnecessary */
            erts_exit(ERTS_DUMP_EXIT, "no more index entries in %s (max=%d)\n",
                      "export_list", exports.limit);
        }

        exports.pages[ix >> EXPORT_LIST_PAGE_SHIFT] =
            erts_alloc(ERTS_ALC_T_EXPORT_TABLE,
                       EXPORT_LIST_PAGE_SIZE * sizeof(Export*));
    }

    export = erts_alloc(ERTS_ALC_T_EXPORT, sizeof(Export));
    erts_atomic_add_nob(&export_bytes, sizeof(Export));
    export_init(export, mfa);

    exports.pages[ix >> EXPORT_LIST_PAGE_SHIFT][ix & EXPORT_LIST_PAGE_MASK] =
        export;
    exports.count = ix + 1;

    if (export_key_indexed(key)) {
        export_accel_insert(export, key, staging_code_ix);
    } else {
        ExportSideEntry template;

        template.mfa = mfa;
        template.export = export;
        template.position = ix;
        hash_put(&export_side, &template);
    }

    /* Iteration may begin as soon as this is seen. */
    erts_atomic_set_relb(&exports.lengths[staging_code_ix], exports.count);

    return export;
}

void
init_export_table(int limit)
{
    erts_rwmtx_opt_t rwmtx_opt = ERTS_RWMTX_OPT_DEFAULT_INITER;
    HashFunctions f;
    Uint pages;

    if (limit > 0) {
        exports.limit = limit;
    }

    /* Round up to a whole number of pages, which is the limit reported. */
    pages = ((Uint)exports.limit + EXPORT_LIST_PAGE_SIZE - 1) /
            EXPORT_LIST_PAGE_SIZE;
    if (pages * EXPORT_LIST_PAGE_SIZE <= INT_MAX) {
        exports.limit = (int)(pages * EXPORT_LIST_PAGE_SIZE);
    }

    rwmtx_opt.type = ERTS_RWMTX_TYPE_FREQUENT_READ;
    rwmtx_opt.lived = ERTS_RWMTX_LONG_LIVED;
    erts_rwmtx_init_opt(&export_rwmutex, &rwmtx_opt, "export_staging_lock",
                        NIL, (ERTS_LOCK_FLAGS_PROPERTY_STATIC |
                              ERTS_LOCK_FLAGS_CATEGORY_GENERIC));

    exports.pages = erts_alloc(ERTS_ALC_T_EXPORT_TABLE,
                               pages * sizeof(Export**));
    exports.count = 0;
    for (int ix = 0; ix < ERTS_NUM_CODE_IX; ix++) {
        erts_atomic_init_nob(&exports.lengths[ix], 0);
    }
    erts_atomic_init_nob(&export_bytes, 0);

    f.hash = (H_FUN)export_side_hash;
    f.cmp = (HCMP_FUN)export_side_cmp;
    f.alloc = (HALLOC_FUN)export_side_alloc;
    f.free = (HFREE_FUN)export_side_free;
    f.meta_alloc = (HMALLOC_FUN)erts_alloc;
    f.meta_free = (HMFREE_FUN)erts_free;
    f.meta_print = (HMPRINT_FUN)erts_print;
    ERTS_CT_ASSERT(offsetof(ExportSideEntry, bucket) == 0);
    hash_init(ERTS_ALC_T_EXPORT_TABLE, &export_side, "export_side", 16, f);

    ERTS_CT_ASSERT(ERTS_NUM_CODE_IX <= 3);
    erts_atomic_init_nob(&export_accel.accel,
                         (erts_aint_t)export_accel_alloc(EXPORT_ACCEL_INITIAL_SIZE));
}

int erts_export_table_limit(void)
{
    return exports.limit;
}

/* Checks the index against the list, for code_SUITE: each export must be
 * found under its own key, once, with the bit of every table that holds it,
 * and each slot must hold an Export and no other bits. Returns
 * {Size, Used, Bad}. */
Eterm erts_export_accel_check(Process *p)
{
    Uint in_tables[ERTS_NUM_CODE_IX] = {0};
    Uint bits[ERTS_NUM_CODE_IX] = {0};
    Uint lengths[ERTS_NUM_CODE_IX];
    Uint used = 0;
    Uint indexed = 0;
    Uint bad = 0;
    ExportAccel *accel;
    Eterm *hp, res;

    export_staged_write_lock();
    accel = (ExportAccel*)erts_atomic_read_nob(&export_accel.accel);

    for (int ix = 0; ix < ERTS_NUM_CODE_IX; ix++) {
        lengths[ix] = erts_atomic_read_nob(&exports.lengths[ix]);
        bad += lengths[ix] > exports.count;
    }

    for (Uint i = 0; i < exports.count; i++) {
        Export *export = export_list_at(i);
        const ErtsCodeMFA *mfa = &export->info.mfa;
        ExportKey key = export_key(mfa->module, mfa->function, mfa->arity);

        if (export_key_indexed(key)) {
            erts_aint_t word = export_accel_lookup(key);

            indexed++;
            bad += (word & ~EXPORT_ACCEL_IX_BITS) != (erts_aint_t)export;
            for (int ix = 0; ix < ERTS_NUM_CODE_IX; ix++) {
                if (i < lengths[ix]) {
                    in_tables[ix]++;
                    bad += !(word & (1 << ix));
                }
            }
        } else {
            ExportSideEntry *entry = export_side_get(&export->info.mfa);

            bad += entry == NULL || entry->export != export ||
                   entry->position != i;
        }
    }

    for (Uint i = 0; i < accel->size; i++) {
        if (export_slot_used(&accel->slots[i])) {
            erts_aint_t word = erts_atomic_read_nob(&accel->slots[i].export);

            used++;
            bad += !(word & EXPORT_ACCEL_IX_BITS);
            for (int ix = 0; ix < ERTS_NUM_CODE_IX; ix++) {
                bits[ix] += (word >> ix) & 1;
            }
        }
    }

    for (int ix = 0; ix < ERTS_NUM_CODE_IX; ix++) {
        bad += bits[ix] != in_tables[ix];
    }
    bad += used != export_accel_used || used != indexed;

    hp = HAlloc(p, 4);
    res = TUPLE3(hp, make_small(accel->size), make_small(export_accel_used),
                 make_small(bad));
    export_staged_write_unlock();

    return res;
}

void
export_info(fmtfn_t to, void *to_arg)
{
    const int lock = !ERTS_IS_CRASH_DUMPING;

    if (lock) {
        erts_rwmtx_rlock(&export_rwmutex);
    }

    {
        ExportAccel *accel =
            (ExportAccel*)erts_atomic_read_nob(&export_accel.accel);
        Uint depth = 0;

        /* The longest probe, counted from each key's home slot. */
        for (Uint i = 0; i < accel->size; i++) {
            ExportAccelSlot *slot = &accel->slots[i];

            if (export_slot_used(slot)) {
                Uint home = export_key_hash(export_slot_key(slot)) >> accel->shift;
                depth = MAX(depth, ((i - home) & (accel->size - 1)) + 1);
            }
        }

        erts_print(to, to_arg, "=hash_table:%s\n", "export_index");
        erts_print(to, to_arg, "size: %d\n", (int)accel->size);
        erts_print(to, to_arg, "used: %d\n", (int)export_accel_used);
        erts_print(to, to_arg, "objs: %d\n", (int)export_accel_used);
        erts_print(to, to_arg, "depth: %d\n", (int)depth);
    }
    hash_info(to, to_arg, &export_side);
    erts_print(to, to_arg, "=index_table:%s\n", "export_list");
    erts_print(to, to_arg, "size: %d\n",
               (int)((exports.count + EXPORT_LIST_PAGE_MASK) &
                     ~EXPORT_LIST_PAGE_MASK));
    erts_print(to, to_arg, "limit: %d\n", exports.limit);
    erts_print(to, to_arg, "entries: %d\n",
               export_list_size(erts_active_code_ix()));

    if (lock) {
        erts_rwmtx_runlock(&export_rwmutex);
    }
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
const Export *erts_find_export_entry(Eterm m, Eterm f, unsigned a,
                                     ErtsCodeIndex code_ix);

/* The slow paths take the MFA by value so that the fast paths don't build one
 * on their stack. */
static ERTS_NOINLINE const Export *export_find_slow(Eterm m, Eterm f, unsigned a,
                                                     ErtsCodeIndex code_ix)
{
    const ErtsCodeMFA mfa = {m, f, a};
    ExportSideEntry *entry;
    const Export *export = NULL;

    erts_rwmtx_rlock(&export_rwmutex);
    entry = export_side_get(&mfa);
    if (entry != NULL &&
        entry->position < erts_atomic_read_nob(&exports.lengths[code_ix])) {
        export = entry->export;
    }
    erts_rwmtx_runlock(&export_rwmutex);

    return export;
}

const Export *erts_find_export_entry(Eterm m, Eterm f, unsigned a,
                                     ErtsCodeIndex code_ix)
{
    ExportKey key = export_key(m, f, a);

    if (ERTS_LIKELY(export_key_indexed(key))) {
        return export_accel_find(key, code_ix);
    }

    return export_find_slow(m, f, a, code_ix);
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
const Export *erts_find_function(Eterm m, Eterm f, unsigned int a,
                                 ErtsCodeIndex code_ix)
{
    const Export *export = erts_find_export_entry(m, f, a, code_ix);

    if (export == NULL
        || (erts_is_export_trampoline_active(export, code_ix) &&
            !BeamIsOpCode(export->trampoline.common.op,
                          op_i_generic_breakpoint))) {
        return NULL;
    }

    return export;
}

/*
 * Returns a pointer to an existing export entry for a MFA,
 * or creates a new one and returns the pointer.
 *
 * This function acts on the staging export table. It should only be used
 * to load new code.
 */

Export *erts_export_put(Eterm mod, Eterm func, unsigned int arity)
{
    const ErtsCodeMFA mfa = {mod, func, arity};
    ExportKey key = export_key(mod, func, arity);
    Export *export;

    ASSERT(is_atom(mod));
    ASSERT(is_atom(func));

    export_staged_write_lock();
    export_catch_up(erts_staging_code_ix());
    export = export_get_locked(&mfa, key);
    if (export == NULL) {
        export = export_create(&mfa, key);
    }
    export_staged_write_unlock();

    return export;
}

/*
 * Find the existing export entry for M:F/A. Failing that, create a stub
 * export entry (making a call through it will cause the error_handler to
 * be called).
 *
 * Stub export entries will be placed in the staging export table.
 */

static ERTS_NOINLINE Export *export_get_or_make_stub_slow(Eterm mod, Eterm func,
                                                           unsigned int arity)
{
    const ErtsCodeMFA mfa = {mod, func, arity};
    ExportKey key = export_key(mod, func, arity);
    Export *export;

    if (!export_key_indexed(key)) {
        erts_rwmtx_rlock(&export_rwmutex);
        export = export_get_locked(&mfa, key);
        erts_rwmtx_runlock(&export_rwmutex);

        if (export != NULL) {
            return export;
        }
    }

    /* The lock-free lookup may have probed an index that has since grown, so
     * look again under the lock before accidentally creating a second Export */
    export_staged_write_lock();
    export = export_get_locked(&mfa, key);
    if (export == NULL) {
        export_catch_up(erts_staging_code_ix());
        export = export_create(&mfa, key);
    }
    export_staged_write_unlock();

    return export;
}

Export *erts_export_get_or_make_stub(Eterm mod, Eterm func, unsigned int arity)
{
    ExportKey key = export_key(mod, func, arity);

    ASSERT(is_atom(mod));
    ASSERT(is_atom(func));

    /* Any table will do: every export is either active or staged. */
    if (ERTS_LIKELY(export_key_indexed(key))) {
        Export *export =
            (Export*)(export_accel_lookup(key) & ~EXPORT_ACCEL_IX_BITS);

        if (ERTS_LIKELY(export != NULL)) {
            return export;
        }
    }

    return export_get_or_make_stub_slow(mod, func, arity);
}

Export *export_list(int i, ErtsCodeIndex code_ix)
{
    ASSERT(i >= 0 && i < erts_atomic_read_nob(&exports.lengths[code_ix]));
    return export_list_at(i);
}

int export_list_size(ErtsCodeIndex code_ix)
{
    return (int)erts_atomic_read_acqb(&exports.lengths[code_ix]);
}

int export_table_sz(void)
{
    const int lock = !ERTS_IS_CRASH_DUMPING;
    Uint pages, size;

    if (lock) {
        erts_rwmtx_rlock(&export_rwmutex);
    }

    pages = (exports.count + EXPORT_LIST_PAGE_MASK) >> EXPORT_LIST_PAGE_SHIFT;
    size = ((Uint)exports.limit >> EXPORT_LIST_PAGE_SHIFT) * sizeof(Export**) +
           pages * EXPORT_LIST_PAGE_SIZE * sizeof(Export*) +
           hash_table_sz(&export_side);
    size += export_accel_bytes(
        ((ExportAccel*)erts_atomic_read_nob(&export_accel.accel))->size);

    if (lock) {
        erts_rwmtx_runlock(&export_rwmutex);
    }

    return (int)size;
}

int export_entries_sz(void)
{
    return (int)erts_atomic_read_nob(&export_bytes);
}

const Export *export_get(const Export *e)
{
    return erts_find_export_entry(e->info.mfa.module, e->info.mfa.function,
                                  e->info.mfa.arity, erts_active_code_ix());
}

void export_start_staging(void)
{
    const ErtsCodeIndex dst_ix = erts_staging_code_ix();
    const ErtsCodeIndex src_ix = erts_active_code_ix();

    ASSERT(dst_ix != src_ix);
    ASSERT(export_debug_stage_ix == ~0);

    export_staged_write_lock();

    export_catch_up(dst_ix);
    for (Uint i = 0, n = erts_atomic_read_nob(&exports.lengths[src_ix]);
         i < n; i++) {
        export_stage(export_list_at(i), src_ix, dst_ix);
    }

    export_staged_write_unlock();

    IF_DEBUG(export_debug_stage_ix = dst_ix);
}

void export_end_staging(int commit)
{
    (void)commit;
    ASSERT(export_debug_stage_ix == erts_staging_code_ix());
    IF_DEBUG(export_debug_stage_ix = ~0);
}
