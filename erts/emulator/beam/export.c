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

#define EXPORT_INITIAL_SIZE   4000
#define EXPORT_LIMIT          (512*1024)

static int export_limit = EXPORT_LIMIT;

#ifdef DEBUG
#  define IF_DEBUG(x) x
#else
#  define IF_DEBUG(x)
#endif

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

static HashValue export_hash(const Export *export)
{
    return (atom_val(export->info.mfa.module) *
            atom_val(export->info.mfa.function)) ^
           export->info.mfa.arity;
}

static int export_cmp(const Export *lhs, const Export *rhs)
{
    return !(lhs->info.mfa.module == rhs->info.mfa.module &&
             lhs->info.mfa.function == rhs->info.mfa.function &&
             lhs->info.mfa.arity == rhs->info.mfa.arity);
}

static void export_init(Export *dst, const Export *template)
{
    sys_memset(&dst->info.u, 0, sizeof(dst->info.u));
    dst->info.gen_bp = NULL;
    dst->info.mfa.module = template->info.mfa.module;
    dst->info.mfa.function = template->info.mfa.function;
    dst->info.mfa.arity = template->info.mfa.arity;
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

#ifdef ARCH_64
#  define ERTS_EXPORT_ACCELERATOR 1
#else
#  define ERTS_EXPORT_ACCELERATOR 0
#endif

#if ERTS_EXPORT_ACCELERATOR
/* A single, shared and flat open-addressed index from {M, F, A} to Export*.
 * Each 16-byte slot holds a packed key and the Export*, four to a cache line,
 * so a lookup usually reads one cache line of the index instead of the chain
 * behind a hash table bucket. Keys whose atom indices don't fit aren't indexed.
 *
 * The low 3 bits of the Export* tell which code indexes' tables hold it, and
 * are set under the write lock as it's put into each. Making a stub ignores
 * them, as `upsert` returns the staging table's entry when the active table
 * misses. Finding an entry answers from its code index's bit, found or not:
 * only the staging table is written to, and the bits of the others were set
 * before the code barrier that activated them. Both look here before building
 * a template for the hash tables, which only keys that don't pack need.
 *
 * Nothing is ever removed, so there are no deletes and bits are never
 * cleared. Readers don't lock: a slot's Export* is written before its key,
 * which is published with a release store, and a grown index is published
 * once complete. The old one is freed after thread progress, so lookups must
 * be made on managed threads or under an unmanaged thread progress delay.
 *
 * A reader that hasn't synchronized with a commit may see its code index
 * before the bits set for it, and is then answered as the previous code
 * index would. That is a valid answer for such a reader, and cheaper than
 * ordering every code index read. */

#define EXPORT_ACCEL_ATOM_BITS 28
#define EXPORT_ACCEL_MAX_LOAD 70 /* percent */
#define EXPORT_ACCEL_INITIAL_SIZE (1 << 13)
/* Exports are at least 8-byte aligned, leaving a bit per code index. */
#define EXPORT_ACCEL_IX_BITS ((erts_aint_t)((1 << ERTS_NUM_CODE_IX) - 1))

typedef struct {
    erts_atomic_t key;    /* Packed key, 0 when the slot is empty */
    erts_atomic_t export; /* Export* | code index bits */
} ExportAccelSlot;

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

/* Returns 0 when the MFA can't be packed, which includes false:false/0, so 0
 * is never a stored key. */
static ERTS_INLINE Uint64 export_accel_key(const ErtsCodeMFA *mfa)
{
    const Uint64 m = atom_val(mfa->module), f = atom_val(mfa->function);

    if (ERTS_UNLIKELY(m >= (1 << EXPORT_ACCEL_ATOM_BITS) ||
                      f >= (1 << EXPORT_ACCEL_ATOM_BITS) || mfa->arity > 255)) {
        return 0;
    }

    return (m << 36) | (f << 8) | mfa->arity;
}

/* Returns the slot holding `key`, or the empty one where it would go. Only for
 * writers, under the write lock. */
static ERTS_INLINE ExportAccelSlot *export_accel_slot(ExportAccel *accel,
                                                     Uint64 key)
{
    ExportAccelSlot *const slots = accel->slots;
    const Uint mask = accel->size - 1;

    /* The index is never full, so this terminates. */
    for (Uint ix = erts_ihash_mix64(key) >> accel->shift; ; ix = (ix + 1) & mask) {
        const Uint64 slot_key = erts_atomic_read_nob(&slots[ix].key);

        if (slot_key == key || slot_key == 0) {
            return &slots[ix];
        }
    }
}

/* The slot's Export* and code index bits, or 0. Lock-free, so each key is read
 * once: a slot found empty may be filled right after, with another key. The
 * index's fields are read into locals as each acquire is a compiler barrier
 * that would otherwise reload them on every probe. */
static ERTS_INLINE erts_aint_t export_accel_lookup(Uint64 key)
{
    const ExportAccel *accel =
        (ExportAccel*)erts_atomic_read_ddrb(&export_accel.accel);
    ExportAccelSlot *const slots = accel->slots;
    const Uint mask = accel->size - 1;

    /* The index is never full, so this terminates. */
    for (Uint ix = erts_ihash_mix64(key) >> accel->shift; ; ix = (ix + 1) & mask) {
        const Uint64 slot_key = erts_atomic_read_acqb(&slots[ix].key);

        if (slot_key == key) {
            return erts_atomic_read_nob(&slots[ix].export);
        } else if (slot_key == 0) {
            return 0;
        }
    }
}

/* The Export* if the table of `code_ix` holds it, or NULL. */
static ERTS_INLINE Export *export_accel_find(Uint64 key, ErtsCodeIndex code_ix)
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
        erts_atomic_init_nob(&accel->slots[ix].key, 0);
        erts_atomic_init_nob(&accel->slots[ix].export, 0);
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
static int export_accel_store(ExportAccel *accel, Uint64 key, erts_aint_t word)
{
    ExportAccelSlot *slot = export_accel_slot(accel, key);
    erts_aint_t old = erts_atomic_read_nob(&slot->export);

    if (erts_atomic_read_nob(&slot->key) == 0) {
        erts_atomic_set_nob(&slot->export, word);
        erts_atomic_set_relb(&slot->key, (erts_aint_t)key);
        return 1;
    } else if ((old | word) != old) {
        ASSERT((old & ~EXPORT_ACCEL_IX_BITS) == (word & ~EXPORT_ACCEL_IX_BITS));
        erts_atomic_set_relb(&slot->export, old | word);
    }

    return 0;
}

static void export_accel_insert(Export *export, ErtsCodeIndex code_ix)
{
    ExportAccel *accel = (ExportAccel*)erts_atomic_read_nob(&export_accel.accel);
    Uint64 key = export_accel_key(&export->info.mfa);

    if (key == 0) {
        return;
    }

    if (export_accel_used >= (accel->size / 100) * EXPORT_ACCEL_MAX_LOAD) {
        ExportAccel *old = accel;

        accel = export_accel_alloc(old->size * 2);
        for (Uint ix = 0; ix < old->size; ix++) {
            Uint64 old_key = erts_atomic_read_nob(&old->slots[ix].key);

            if (old_key != 0) {
                export_accel_store(accel, old_key,
                                   erts_atomic_read_nob(&old->slots[ix].export));
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

#  define ERTS_CODE_STAGED_ON_INSERT export_accel_insert
#endif

#define ERTS_CODE_STAGED_PREFIX export
#define ERTS_CODE_STAGED_OBJECT_TYPE Export
#define ERTS_CODE_STAGED_OBJECT_HASH export_hash
#define ERTS_CODE_STAGED_OBJECT_COMPARE export_cmp
#define ERTS_CODE_STAGED_OBJECT_INITIALIZE export_init
#define ERTS_CODE_STAGED_OBJECT_STAGE export_stage
#define ERTS_CODE_STAGED_OBJECT_ALLOC_TYPE ERTS_ALC_T_EXPORT
#define ERTS_CODE_STAGED_TABLE_ALLOC_TYPE ERTS_ALC_T_EXPORT_TABLE
#define ERTS_CODE_STAGED_TABLE_INITIAL_SIZE EXPORT_INITIAL_SIZE
#define ERTS_CODE_STAGED_TABLE_LIMIT export_limit

#define ERTS_CODE_STAGED_WANT_GET
#define ERTS_CODE_STAGED_WANT_PUT
#define ERTS_CODE_STAGED_WANT_LIST
#define ERTS_CODE_STAGED_WANT_LIST_SIZE
#define ERTS_CODE_STAGED_WANT_ENTRY_BYTES
#define ERTS_CODE_STAGED_WANT_TABLE_SIZE
#define ERTS_CODE_STAGED_WANT_INFO

#include "erl_code_staged.h"

void
init_export_table(int limit)
{
    if (limit > 0) {
        export_limit = limit;
    }
    export_staged_init();
#if ERTS_EXPORT_ACCELERATOR
    ERTS_CT_ASSERT(ERTS_NUM_CODE_IX <= 3);
    erts_atomic_init_nob(&export_accel.accel,
                         (erts_aint_t)export_accel_alloc(EXPORT_ACCEL_INITIAL_SIZE));
#endif
}

int erts_export_table_limit(void)
{
    return export_tables[erts_active_code_ix()].limit;
}

/* Checks the index against the hash tables, for code_SUITE: each entry that
 * can be packed must be found with the bit of its table, and each slot must
 * hold an Export and no other bits. Returns {Size, Used, Bad} or false. */
Eterm erts_export_accel_check(Process *p)
{
#if ERTS_EXPORT_ACCELERATOR
    Uint in_tables[ERTS_NUM_CODE_IX] = {0}, bits[ERTS_NUM_CODE_IX] = {0};
    Uint used = 0, bad = 0;
    ExportAccel *accel;
    Eterm *hp, res;

    export_staged_write_lock();
    accel = (ExportAccel*)erts_atomic_read_nob(&export_accel.accel);

    for (int ix = 0; ix < ERTS_NUM_CODE_IX; ix++) {
        for (int i = 0; i < export_tables[ix].entries; i++) {
            Export *export =
                ((export_entry_t*)erts_index_lookup(&export_tables[ix], i))->object;
            Uint64 key = export_accel_key(&export->info.mfa);
            erts_aint_t word = key ? export_accel_lookup(key) : 0;

            in_tables[ix] += (key != 0);
            bad += key && ((word & ~EXPORT_ACCEL_IX_BITS) != (erts_aint_t)export ||
                           !(word & (1 << ix)));
        }
    }

    for (Uint i = 0; i < accel->size; i++) {
        if (erts_atomic_read_nob(&accel->slots[i].key) != 0) {
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
    bad += used != export_accel_used;

    hp = HAlloc(p, 4);
    res = TUPLE3(hp, make_small(accel->size), make_small(export_accel_used),
                 make_small(bad));
    export_staged_write_unlock();

    return res;
#else
    (void)p;
    return am_false;
#endif
}

void
export_info(fmtfn_t to, void *to_arg)
{
    export_staged_info(to, to_arg);
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

static ERTS_NOINLINE const Export *export_find_slow(Eterm m, Eterm f, unsigned a,
                                                     ErtsCodeIndex code_ix)
{
    export_template_t template;
    Export *object;

    object = export_staged_init_template(&template);
    object->info.mfa.module = m;
    object->info.mfa.function = f;
    object->info.mfa.arity = a;

    return export_staged_get(&template, code_ix);
}

const Export *erts_find_export_entry(Eterm m, Eterm f, unsigned a,
                                     ErtsCodeIndex code_ix)
{
#if ERTS_EXPORT_ACCELERATOR
    const ErtsCodeMFA mfa = {m, f, a};
    Uint64 key = export_accel_key(&mfa);

    if (ERTS_LIKELY(key != 0)) {
        const Export *export = export_accel_find(key, code_ix);
#ifdef DEBUG
        /* Only the staging table is written to. A reader racing with a
         * commit may see the new code index before the bits set for it, so
         * look again after a barrier before comparing. */
        if (code_ix != erts_staging_code_ix()) {
            const Export *in_table = export_find_slow(m, f, a, code_ix);

            if (export != in_table) {
                ERTS_THR_MEMORY_BARRIER;
                ASSERT(export_accel_find(key, code_ix) == in_table);
            }
        }
#endif
        return export;
    }
#endif

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
    export_template_t template;
    Export *object;

    ASSERT(is_atom(mod));
    ASSERT(is_atom(func));

    object = export_staged_init_template(&template);
    object->info.mfa.module = mod;
    object->info.mfa.function = func;
    object->info.mfa.arity = arity;

    return export_staged_put(&template);
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
    export_template_t template;
    Export *object;

    object = export_staged_init_template(&template);
    object->info.mfa.module = mod;
    object->info.mfa.function = func;
    object->info.mfa.arity = arity;

    return export_staged_upsert(&template);
}

Export *erts_export_get_or_make_stub(Eterm mod, Eterm func, unsigned int arity)
{
#if ERTS_EXPORT_ACCELERATOR
    const ErtsCodeMFA mfa = {mod, func, arity};
    Uint64 key = export_accel_key(&mfa);
#endif

    ASSERT(is_atom(mod));
    ASSERT(is_atom(func));

#if ERTS_EXPORT_ACCELERATOR
    /* Any table will do, as `upsert` returns the staging table's entry when
     * the active one misses. */
    if (ERTS_LIKELY(key != 0)) {
        Export *export =
            (Export*)(export_accel_lookup(key) & ~EXPORT_ACCEL_IX_BITS);

        if (ERTS_LIKELY(export != NULL)) {
            return export;
        }
    }
#endif

    return export_get_or_make_stub_slow(mod, func, arity);
}

Export *export_list(int i, ErtsCodeIndex code_ix)
{
    return export_staged_list(i, code_ix);
}

int export_list_size(ErtsCodeIndex code_ix)
{
    return export_staged_list_size(code_ix);
}

int export_table_sz(void)
{
    int size = export_staged_table_size();

#if ERTS_EXPORT_ACCELERATOR
    size += export_accel_bytes(
        ((ExportAccel*)erts_atomic_read_nob(&export_accel.accel))->size);
#endif

    return size;
}

int export_entries_sz(void)
{
    return export_staged_entry_bytes();
}

const Export *export_get(const Export *e)
{
    return erts_find_export_entry(e->info.mfa.module, e->info.mfa.function,
                                  e->info.mfa.arity, erts_active_code_ix());
}

void export_start_staging(void)
{
    export_staged_start_staging();
}

void export_end_staging(int commit)
{
    export_staged_end_staging(commit);
}
