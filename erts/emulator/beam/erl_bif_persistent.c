/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2018. All Rights Reserved.
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

/*
 * Purpose: Implement persistent term storage.
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "erl_vm.h"
#include "global.h"
#include "erl_process.h"
#include "error.h"
#include "erl_driver.h"
#include "bif.h"
#include "erl_map.h"
#include "erl_binary.h"

/*
 * Parameters for the hash table.
 */
#define INITIAL_SIZE 8
#define LOAD_FACTOR ((Uint)50)
#define MUST_GROW(t) (((Uint)100) * t->num_entries >= LOAD_FACTOR * t->allocated)
#define MUST_SHRINK(t) (((Uint)200) * t->num_entries <= LOAD_FACTOR * t->allocated && \
                        t->allocated > INITIAL_SIZE)

typedef struct hash_table {
    Uint allocated;
    Uint num_entries;
    Uint mask;
    Uint first_to_delete;
    Uint num_to_delete;
    erts_atomic_t refc;
    struct hash_table* delete_next;
    ErtsThrPrgrLaterOp thr_prog_op;
    Eterm term[1];
} HashTable;

typedef struct trap_data {
    HashTable* table;
    Uint idx;
    Uint remaining;
    Uint memory;    /* Used by info/0 to count used memory */
} TrapData;

typedef enum {
    ERTS_PERSISTENT_TERM_CPY_PLACE_START,
    ERTS_PERSISTENT_TERM_CPY_PLACE_1,
    ERTS_PERSISTENT_TERM_CPY_PLACE_2,
    ERTS_PERSISTENT_TERM_CPY_PLACE_3
} ErtsPersistentTermCpyTableLocation;

typedef enum {
    ERTS_PERSISTENT_TERM_CPY_NO_REHASH = 0,
    ERTS_PERSISTENT_TERM_CPY_REHASH = 1,
    ERTS_PERSISTENT_TERM_CPY_TEMP = 2
} ErtsPersistentTermCpyTableType;

typedef struct {
    HashTable* old_table; /* in param */
    Uint new_size; /* in param */
    ErtsPersistentTermCpyTableType copy_type; /* in param */
    Uint max_iterations; /* in param */
    ErtsPersistentTermCpyTableLocation location; /* in/out param */
    Uint iterations_done; /* in/out param */
    Uint total_iterations_done; /* in/out param */
    HashTable* new_table; /* out param */
} ErtsPersistentTermCpyTableCtx;

typedef enum {
    PUT2_TRAP_LOCATION_NEW_KEY,
    PUT2_TRAP_LOCATION_REPLACE_VALUE
} ErtsPersistentTermPut2TrapLocation;

typedef struct {
    ErtsPersistentTermPut2TrapLocation trap_location;
    Eterm key;
    Eterm term;
    Uint entry_index;
    HashTable* hash_table;
    Eterm heap[3];
    Eterm tuple;
    ErtsPersistentTermCpyTableCtx cpy_ctx;
} ErtsPersistentTermPut2Context;

typedef enum {
    ERASE1_TRAP_LOCATION_TMP_COPY,
    ERASE1_TRAP_LOCATION_FINAL_COPY
} ErtsPersistentTermErase1TrapLocation;

typedef struct {
    ErtsPersistentTermErase1TrapLocation trap_location;
    Eterm key;
    HashTable* old_table;
    HashTable* new_table;
    Uint entry_index;
    Eterm old_term;
    HashTable* tmp_table;
    ErtsPersistentTermCpyTableCtx cpy_ctx;
} ErtsPersistentTermErase1Context;

/*
 * Declarations of local functions.
 */

static HashTable* create_initial_table(void);
static Uint lookup(HashTable* hash_table, Eterm key);
static HashTable* copy_table(ErtsPersistentTermCpyTableCtx* ctx);
static int try_seize_update_permission(Process* c_p);
static void release_update_permission(int release_updater);
static void table_updater(void* table);
static void table_deleter(void* hash_table);
static void dec_table_refc(Process* c_p, HashTable* old_table);
static void delete_table(Process* c_p, HashTable* table);
static void mark_for_deletion(HashTable* hash_table, Uint entry_index);
static ErtsLiteralArea* term_to_area(Eterm tuple);
static void suspend_updater(Process* c_p);
static Eterm do_get_all(Process* c_p, TrapData* trap_data, Eterm res);
static Eterm do_info(Process* c_p, TrapData* trap_data);
static void append_to_delete_queue(HashTable* table);
static HashTable* next_to_delete(void);
static Eterm alloc_trap_data(Process* c_p);
static int cleanup_trap_data(Binary *bp);

/*
 * Traps
 */

static Export persistent_term_get_all_export;
static BIF_RETTYPE persistent_term_get_all_trap(BIF_ALIST_2);
static Export persistent_term_info_export;
static BIF_RETTYPE persistent_term_info_trap(BIF_ALIST_1);

/*
 * Pointer to the current hash table.
 */

static erts_atomic_t the_hash_table;

/*
 * Queue of processes waiting to update the hash table.
 */

struct update_queue_item {
    Process *p;
    struct update_queue_item* next;
};

static erts_mtx_t update_table_permission_mtx;
static struct update_queue_item* update_queue = NULL;
static Process* updater_process = NULL;

/* Protected by update_table_permission_mtx */
static ErtsThrPrgrLaterOp thr_prog_op;

/*
 * Queue of hash tables to be deleted.
 */

static erts_mtx_t delete_queue_mtx;
static HashTable* delete_queue_head = NULL;
static HashTable** delete_queue_tail = &delete_queue_head;

/*
 * The following variables are only used during crash dumping. They
 * are initialized by erts_init_persistent_dumping().
 */

ErtsLiteralArea** erts_persistent_areas;
Uint erts_num_persistent_areas;

void erts_init_bif_persistent_term(void)
{
    HashTable* hash_table;

    /*
     * Initialize the mutex protecting updates.
     */

    erts_mtx_init(&update_table_permission_mtx,
                  "update_persistent_term_permission",
                  NIL,
                  ERTS_LOCK_FLAGS_PROPERTY_STATIC |
                  ERTS_LOCK_FLAGS_CATEGORY_GENERIC);

    /*
     * Initialize delete queue.
     */

    erts_mtx_init(&delete_queue_mtx,
                  "persistent_term_delete_permission",
                  NIL,
                  ERTS_LOCK_FLAGS_PROPERTY_STATIC |
                  ERTS_LOCK_FLAGS_CATEGORY_GENERIC);

    /*
     * Allocate a small initial hash table.
     */

    hash_table = create_initial_table();
    erts_atomic_init_nob(&the_hash_table, (erts_aint_t)hash_table);

    /*
     * Initialize export entry for traps
     */

    erts_init_trap_export(&persistent_term_get_all_export,
			  am_persistent_term, am_get_all_trap, 2,
			  &persistent_term_get_all_trap);
    erts_init_trap_export(&persistent_term_info_export,
			  am_persistent_term, am_info_trap, 1,
			  &persistent_term_info_trap);
}

/*
 * Macro used for trapping in persistent_term_put_2 and
 * persistent_term_erase_1
 */
#define TRAPPING_COPY_TABLE(TABLE_DEST, OLD_TABLE, NEW_SIZE, COPY_TYPE, LOC_NAME, TRAP_CODE) \
    do {                                                                \
        ctx->cpy_ctx = (ErtsPersistentTermCpyTableCtx){                 \
            .old_table = OLD_TABLE,                                     \
            .new_size = NEW_SIZE,                                       \
            .copy_type = COPY_TYPE,                                     \
            .location = ERTS_PERSISTENT_TERM_CPY_PLACE_START            \
        };                                                              \
        L_ ## LOC_NAME:                                                 \
        ctx->cpy_ctx.max_iterations = MAX(1, max_iterations);           \
        TABLE_DEST = copy_table(&ctx->cpy_ctx);                         \
        iterations_until_trap -= ctx->cpy_ctx.total_iterations_done;    \
        if (TABLE_DEST == NULL) {                                       \
            ctx->trap_location = LOC_NAME;                              \
            erts_set_gc_state(BIF_P, 0);                                \
            BUMP_ALL_REDS(BIF_P);                                       \
            TRAP_CODE;                                                  \
        }                                                               \
    } while (0)

static int persistent_term_put_2_ctx_bin_dtor(Binary *context_bin)
{
    ErtsPersistentTermPut2Context* ctx = ERTS_MAGIC_BIN_DATA(context_bin);
    if (ctx->cpy_ctx.new_table != NULL) {
        erts_free(ERTS_ALC_T_PERSISTENT_TERM, ctx->cpy_ctx.new_table);
        release_update_permission(0);
    }
    return 1;
}
/*
 * A linear congruential generator that is used in the debug emulator
 * to trap after a random number of iterations in
 * persistent_term_put_2 and persistent_term_erase_1.
 *
 * https://en.wikipedia.org/wiki/Linear_congruential_generator
 */
#define GET_SMALL_RANDOM_INT(SEED)              \
    (1103515245 * (SEED) + 12345)  % 227

BIF_RETTYPE persistent_term_put_2(BIF_ALIST_2)
{
    static const Uint ITERATIONS_PER_RED = 32;
    ErtsPersistentTermPut2Context* ctx;
    Eterm state_mref = THE_NON_VALUE;
    long iterations_until_trap;
    long max_iterations;
#define PUT_TRAP_CODE                                                   \
    BIF_TRAP2(bif_export[BIF_persistent_term_put_2], BIF_P, state_mref, BIF_ARG_2)
#define TRAPPING_COPY_TABLE_PUT(TABLE_DEST, OLD_TABLE, NEW_SIZE, COPY_TYPE, LOC_NAME) \
    TRAPPING_COPY_TABLE(TABLE_DEST, OLD_TABLE, NEW_SIZE, COPY_TYPE, LOC_NAME, PUT_TRAP_CODE)

#ifdef DEBUG
        (void)ITERATIONS_PER_RED;
        iterations_until_trap = max_iterations =
            GET_SMALL_RANDOM_INT(ERTS_BIF_REDS_LEFT(BIF_P) + (Uint)&ctx);
#else
        iterations_until_trap = max_iterations =
            ITERATIONS_PER_RED * ERTS_BIF_REDS_LEFT(BIF_P);
#endif
    if (is_internal_magic_ref(BIF_ARG_1) &&
        (ERTS_MAGIC_BIN_DESTRUCTOR(erts_magic_ref2bin(BIF_ARG_1)) ==
         persistent_term_put_2_ctx_bin_dtor)) {
        /* Restore state after a trap */
        Binary* state_bin;
        state_mref = BIF_ARG_1;
        state_bin = erts_magic_ref2bin(state_mref);
        ctx = ERTS_MAGIC_BIN_DATA(state_bin);
        ASSERT(BIF_P->flags & F_DISABLE_GC);
        erts_set_gc_state(BIF_P, 1);
        switch (ctx->trap_location) {
        case PUT2_TRAP_LOCATION_NEW_KEY:
            goto L_PUT2_TRAP_LOCATION_NEW_KEY;
        case PUT2_TRAP_LOCATION_REPLACE_VALUE:
            goto L_PUT2_TRAP_LOCATION_REPLACE_VALUE;
        }
    } else {
        /* Save state in magic bin in case trapping is necessary */
        Eterm* hp;
        Binary* state_bin = erts_create_magic_binary(sizeof(ErtsPersistentTermPut2Context),
                                                     persistent_term_put_2_ctx_bin_dtor);
        hp = HAlloc(BIF_P, ERTS_MAGIC_REF_THING_SIZE);
        state_mref = erts_mk_magic_ref(&hp, &MSO(BIF_P), state_bin);
        ctx = ERTS_MAGIC_BIN_DATA(state_bin);
        /*
         * IMPORTANT: The following field is used to detect if
         * persistent_term_put_2_ctx_bin_dtor needs to free memory
         */
        ctx->cpy_ctx.new_table = NULL;
    }


    if (!try_seize_update_permission(BIF_P)) {
	ERTS_BIF_YIELD2(bif_export[BIF_persistent_term_put_2],
                        BIF_P, BIF_ARG_1, BIF_ARG_2);
    }
    ctx->hash_table = (HashTable *) erts_atomic_read_nob(&the_hash_table);

    ctx->key = BIF_ARG_1;
    ctx->term = BIF_ARG_2;

    ctx->entry_index = lookup(ctx->hash_table, ctx->key);

    ctx->heap[0] = make_arityval(2);
    ctx->heap[1] = ctx->key;
    ctx->heap[2] = ctx->term;
    ctx->tuple = make_tuple(ctx->heap);

    if (is_nil(ctx->hash_table->term[ctx->entry_index])) {
        Uint new_size = ctx->hash_table->allocated;
        if (MUST_GROW(ctx->hash_table)) {
            new_size *= 2;
        }
        TRAPPING_COPY_TABLE_PUT(ctx->hash_table,
                                ctx->hash_table,
                                new_size,
                                ERTS_PERSISTENT_TERM_CPY_NO_REHASH,
                                PUT2_TRAP_LOCATION_NEW_KEY);
        ctx->entry_index = lookup(ctx->hash_table, ctx->key);
        ctx->hash_table->num_entries++;
    } else {
        Eterm tuple = ctx->hash_table->term[ctx->entry_index];
        Eterm old_term;

        ASSERT(is_tuple_arity(tuple, 2));
        old_term = boxed_val(tuple)[2];
        if (EQ(ctx->term, old_term)) {
            /* Same value. No need to update anything. */
            release_update_permission(0);
            BIF_RET(am_ok);
        } else {
            /* Mark the old term for deletion. */
            mark_for_deletion(ctx->hash_table, ctx->entry_index);
            TRAPPING_COPY_TABLE_PUT(ctx->hash_table,
                                    ctx->hash_table,
                                    ctx->hash_table->allocated,
                                    ERTS_PERSISTENT_TERM_CPY_NO_REHASH,
                                    PUT2_TRAP_LOCATION_REPLACE_VALUE);
        }
    }

    {
        Uint term_size;
        Uint lit_area_size;
        ErlOffHeap code_off_heap;
        ErtsLiteralArea* literal_area;
        erts_shcopy_t info;
        Eterm* ptr;
        /*
         * Preserve internal sharing in the term by using the
         * sharing-preserving functions. However, literals must
         * be copied in case the module holding them are unloaded.
         */
        INITIALIZE_SHCOPY(info);
        info.copy_literals = 1;
        term_size = copy_shared_calculate(ctx->tuple, &info);
        ERTS_INIT_OFF_HEAP(&code_off_heap);
        lit_area_size = ERTS_LITERAL_AREA_ALLOC_SIZE(term_size);
        literal_area = erts_alloc(ERTS_ALC_T_LITERAL, lit_area_size);
        ptr = &literal_area->start[0];
        literal_area->end = ptr + term_size;
        ctx->tuple = copy_shared_perform(ctx->tuple, term_size, &info, &ptr, &code_off_heap);
        ASSERT(tuple_val(ctx->tuple) == literal_area->start);
        literal_area->off_heap = code_off_heap.first;
        DESTROY_SHCOPY(info);
        erts_set_literal_tag(&ctx->tuple, literal_area->start, term_size);
        ctx->hash_table->term[ctx->entry_index] = ctx->tuple;

        erts_schedule_thr_prgr_later_op(table_updater, ctx->hash_table, &thr_prog_op);
        suspend_updater(BIF_P);
    }
    BUMP_REDS(BIF_P, (max_iterations - iterations_until_trap) / ITERATIONS_PER_RED);
    ERTS_BIF_YIELD_RETURN(BIF_P, am_ok);
}

BIF_RETTYPE persistent_term_get_0(BIF_ALIST_0)
{
    HashTable* hash_table = (HashTable *) erts_atomic_read_nob(&the_hash_table);
    TrapData* trap_data;
    Eterm res = NIL;
    Eterm magic_ref;
    Binary* mbp;

    magic_ref = alloc_trap_data(BIF_P);
    mbp = erts_magic_ref2bin(magic_ref);
    trap_data = ERTS_MAGIC_BIN_DATA(mbp);
    trap_data->table = hash_table;
    trap_data->idx = 0;
    trap_data->remaining = hash_table->num_entries;
    res = do_get_all(BIF_P, trap_data, res);
    if (trap_data->remaining == 0) {
        BUMP_REDS(BIF_P, hash_table->num_entries);
        trap_data->table = NULL; /* Prevent refc decrement */
        BIF_RET(res);
    } else {
        /*
         * Increment the ref counter to prevent an update operation (by put/2
         * or erase/1) to delete this hash table.
         */
        erts_atomic_inc_nob(&hash_table->refc);
        BUMP_ALL_REDS(BIF_P);
        BIF_TRAP2(&persistent_term_get_all_export, BIF_P, magic_ref, res);
    }
}

BIF_RETTYPE persistent_term_get_1(BIF_ALIST_1)
{
    Eterm key = BIF_ARG_1;
    HashTable* hash_table = (HashTable *) erts_atomic_read_nob(&the_hash_table);
    Uint entry_index;
    Eterm term;

    entry_index = lookup(hash_table, key);
    term = hash_table->term[entry_index];
    if (is_boxed(term)) {
        ASSERT(is_tuple_arity(term, 2));
        BIF_RET(tuple_val(term)[2]);
    }
    BIF_ERROR(BIF_P, BADARG);
}

BIF_RETTYPE persistent_term_get_2(BIF_ALIST_2)
{
    Eterm key = BIF_ARG_1;
    Eterm result = BIF_ARG_2;
    HashTable* hash_table = (HashTable *) erts_atomic_read_nob(&the_hash_table);
    Uint entry_index;
    Eterm term;

    entry_index = lookup(hash_table, key);
    term = hash_table->term[entry_index];
    if (is_boxed(term)) {
        ASSERT(is_tuple_arity(term, 2));
        result = tuple_val(term)[2];
    }
    BIF_RET(result);
}

static int persistent_term_erase_1_ctx_bin_dtor(Binary *context_bin)
{
    ErtsPersistentTermErase1Context* ctx = ERTS_MAGIC_BIN_DATA(context_bin);
    if (ctx->cpy_ctx.new_table != NULL) {
        if (ctx->cpy_ctx.copy_type == ERTS_PERSISTENT_TERM_CPY_TEMP) {
            erts_free(ERTS_ALC_T_PERSISTENT_TERM_TMP, ctx->cpy_ctx.new_table);
        } else {
            erts_free(ERTS_ALC_T_PERSISTENT_TERM, ctx->cpy_ctx.new_table);
        }
        if (ctx->tmp_table != NULL) {
            erts_free(ERTS_ALC_T_PERSISTENT_TERM_TMP, ctx->tmp_table);
        }
        release_update_permission(0);
    }
    return 1;
}

BIF_RETTYPE persistent_term_erase_1(BIF_ALIST_1)
{
    static const Uint ITERATIONS_PER_RED = 32;
    ErtsPersistentTermErase1Context* ctx;
    Eterm state_mref = THE_NON_VALUE;
    long iterations_until_trap;
    long max_iterations;
#ifdef DEBUG
        (void)ITERATIONS_PER_RED;
        iterations_until_trap = max_iterations =
            GET_SMALL_RANDOM_INT(ERTS_BIF_REDS_LEFT(BIF_P) + (Uint)&ctx);
#else
        iterations_until_trap = max_iterations =
            ITERATIONS_PER_RED * ERTS_BIF_REDS_LEFT(BIF_P);
#endif
#define ERASE_TRAP_CODE                                                 \
        BIF_TRAP1(bif_export[BIF_persistent_term_erase_1], BIF_P, state_mref);
#define TRAPPING_COPY_TABLE_ERASE(TABLE_DEST, OLD_TABLE, NEW_SIZE, REHASH, LOC_NAME) \
        TRAPPING_COPY_TABLE(TABLE_DEST, OLD_TABLE, NEW_SIZE, REHASH, LOC_NAME, ERASE_TRAP_CODE)
    if (is_internal_magic_ref(BIF_ARG_1) &&
        (ERTS_MAGIC_BIN_DESTRUCTOR(erts_magic_ref2bin(BIF_ARG_1)) ==
         persistent_term_erase_1_ctx_bin_dtor)) {
        /* Restore the state after a trap */
        Binary* state_bin;
        state_mref = BIF_ARG_1;
        state_bin = erts_magic_ref2bin(state_mref);
        ctx = ERTS_MAGIC_BIN_DATA(state_bin);
        ASSERT(BIF_P->flags & F_DISABLE_GC);
        erts_set_gc_state(BIF_P, 1);
        switch (ctx->trap_location) {
        case ERASE1_TRAP_LOCATION_TMP_COPY:
            goto L_ERASE1_TRAP_LOCATION_TMP_COPY;
        case ERASE1_TRAP_LOCATION_FINAL_COPY:
            goto L_ERASE1_TRAP_LOCATION_FINAL_COPY;
        }
    } else {
        /* Save state in magic bin in case trapping is necessary */
        Eterm* hp;
        Binary* state_bin = erts_create_magic_binary(sizeof(ErtsPersistentTermErase1Context),
                                                     persistent_term_erase_1_ctx_bin_dtor);
        hp = HAlloc(BIF_P, ERTS_MAGIC_REF_THING_SIZE);
        state_mref = erts_mk_magic_ref(&hp, &MSO(BIF_P), state_bin);
        ctx = ERTS_MAGIC_BIN_DATA(state_bin);
        /*
         * IMPORTANT: The following two fields are used to detect if
         * persistent_term_erase_1_ctx_bin_dtor needs to free memory
         */
        ctx->cpy_ctx.new_table = NULL;
        ctx->tmp_table = NULL;
    }
    if (!try_seize_update_permission(BIF_P)) {
	ERTS_BIF_YIELD1(bif_export[BIF_persistent_term_erase_1],
                        BIF_P, BIF_ARG_1);
    }

    ctx->key = BIF_ARG_1;
    ctx->old_table = (HashTable *) erts_atomic_read_nob(&the_hash_table);
    ctx->entry_index = lookup(ctx->old_table, ctx->key);
    ctx->old_term = ctx->old_table->term[ctx->entry_index];
    if (is_boxed(ctx->old_term)) {
        Uint new_size;
        /*
         * Since we don't use any delete markers, we must rehash
         * the table when deleting terms to ensure that all terms
         * can still be reached if there are hash collisions.
         * We can't rehash in place and it would not be safe to modify
         * the old table yet, so we will first need a new
         * temporary table copy of the same size as the old one.
         */

        ASSERT(is_tuple_arity(ctx->old_term, 2));
        TRAPPING_COPY_TABLE_ERASE(ctx->tmp_table,
                                  ctx->old_table,
                                  ctx->old_table->allocated,
                                  ERTS_PERSISTENT_TERM_CPY_TEMP,
                                  ERASE1_TRAP_LOCATION_TMP_COPY);

        /*
         * Delete the term from the temporary table. Then copy the
         * temporary table to a new table, rehashing the entries
         * while copying.
         */

        ctx->tmp_table->term[ctx->entry_index] = NIL;
        ctx->tmp_table->num_entries--;
        new_size = ctx->tmp_table->allocated;
        if (MUST_SHRINK(ctx->tmp_table)) {
            new_size /= 2;
        }
        TRAPPING_COPY_TABLE_ERASE(ctx->new_table,
                                  ctx->tmp_table,
                                  new_size,
                                  ERTS_PERSISTENT_TERM_CPY_REHASH,
                                  ERASE1_TRAP_LOCATION_FINAL_COPY);
        erts_free(ERTS_ALC_T_PERSISTENT_TERM_TMP, ctx->tmp_table);
        /*
         * IMPORTANT: Memory management depends on that ctx->tmp_table
         * is set to NULL on the line below
         */
        ctx->tmp_table = NULL;

        mark_for_deletion(ctx->old_table, ctx->entry_index);
        erts_schedule_thr_prgr_later_op(table_updater, ctx->new_table, &thr_prog_op);
        suspend_updater(BIF_P);
        BUMP_REDS(BIF_P, (max_iterations - iterations_until_trap) / ITERATIONS_PER_RED);
        ERTS_BIF_YIELD_RETURN(BIF_P, am_true);
    }

    /*
     * Key is not present. Nothing to do.
     */

    ASSERT(is_nil(ctx->old_term));
    release_update_permission(0);
    BIF_RET(am_false);
}

BIF_RETTYPE erts_internal_erase_persistent_terms_0(BIF_ALIST_0)
{
    HashTable* old_table;
    HashTable* new_table;

    if (!try_seize_update_permission(BIF_P)) {
	ERTS_BIF_YIELD0(bif_export[BIF_erts_internal_erase_persistent_terms_0],
                        BIF_P);
    }
    old_table = (HashTable *) erts_atomic_read_nob(&the_hash_table);
    old_table->first_to_delete = 0;
    old_table->num_to_delete = old_table->allocated;
    new_table = create_initial_table();
    erts_schedule_thr_prgr_later_op(table_updater, new_table, &thr_prog_op);
    suspend_updater(BIF_P);
    ERTS_BIF_YIELD_RETURN(BIF_P, am_true);
}

BIF_RETTYPE persistent_term_info_0(BIF_ALIST_0)
{
    HashTable* hash_table = (HashTable *) erts_atomic_read_nob(&the_hash_table);
    TrapData* trap_data;
    Eterm res = NIL;
    Eterm magic_ref;
    Binary* mbp;

    magic_ref = alloc_trap_data(BIF_P);
    mbp = erts_magic_ref2bin(magic_ref);
    trap_data = ERTS_MAGIC_BIN_DATA(mbp);
    trap_data->table = hash_table;
    trap_data->idx = 0;
    trap_data->remaining = hash_table->num_entries;
    trap_data->memory = 0;
    res = do_info(BIF_P, trap_data);
    if (trap_data->remaining == 0) {
        BUMP_REDS(BIF_P, hash_table->num_entries);
        trap_data->table = NULL; /* Prevent refc decrement */
        BIF_RET(res);
    } else {
        /*
         * Increment the ref counter to prevent an update operation (by put/2
         * or erase/1) to delete this hash table.
         */
        erts_atomic_inc_nob(&hash_table->refc);
        BUMP_ALL_REDS(BIF_P);
        BIF_TRAP2(&persistent_term_info_export, BIF_P, magic_ref, res);
    }
}

Uint
erts_persistent_term_count(void)
{
    HashTable* hash_table = (HashTable *) erts_atomic_read_nob(&the_hash_table);
    return hash_table->num_entries;
}

void
erts_init_persistent_dumping(void)
{
    HashTable* hash_table = (HashTable *) erts_atomic_read_nob(&the_hash_table);
    ErtsLiteralArea** area_p;
    Uint i;

    /*
     * Overwrite the array of Eterms in the current hash table
     * with pointers to literal areas.
     */

    erts_persistent_areas = (ErtsLiteralArea **) hash_table->term;
    erts_num_persistent_areas = hash_table->num_entries;
    area_p = erts_persistent_areas;
    for (i = 0; i < hash_table->allocated; i++) {
        Eterm term = hash_table->term[i];

        if (is_boxed(term)) {
            *area_p++ = term_to_area(term);
        }
    }
}

/*
 * Local functions.
 */

static HashTable*
create_initial_table(void)
{
    HashTable* hash_table;
    int i;

    hash_table = (HashTable *) erts_alloc(ERTS_ALC_T_PERSISTENT_TERM,
                                          sizeof(HashTable)+sizeof(Eterm) *
                                          (INITIAL_SIZE-1));
    hash_table->allocated = INITIAL_SIZE;
    hash_table->num_entries = 0;
    hash_table->mask = INITIAL_SIZE-1;
    hash_table->first_to_delete = 0;
    hash_table->num_to_delete = 0;
    erts_atomic_init_nob(&hash_table->refc, (erts_aint_t)1);
    for (i = 0; i < INITIAL_SIZE; i++) {
        hash_table->term[i] = NIL;
    }
    return hash_table;
}

static BIF_RETTYPE
persistent_term_get_all_trap(BIF_ALIST_2)
{
    TrapData* trap_data;
    Eterm res = BIF_ARG_2;
    Uint bump_reds;
    Binary* mbp;

    ASSERT(is_list(BIF_ARG_2));
    mbp = erts_magic_ref2bin(BIF_ARG_1);
    trap_data = ERTS_MAGIC_BIN_DATA(mbp);
    bump_reds = trap_data->remaining;
    res = do_get_all(BIF_P, trap_data, res);
    ASSERT(is_list(res));
    if (trap_data->remaining > 0) {
        BUMP_ALL_REDS(BIF_P);
        BIF_TRAP2(&persistent_term_get_all_export, BIF_P, BIF_ARG_1, res);
    } else {
        /*
         * Decrement ref count (and possibly delete the hash table
         * and associated literal area).
         */
        dec_table_refc(BIF_P, trap_data->table);
        trap_data->table = NULL; /* Prevent refc decrement */
        BUMP_REDS(BIF_P, bump_reds);
        BIF_RET(res);
    }
}

static Eterm
do_get_all(Process* c_p, TrapData* trap_data, Eterm res)
{
    HashTable* hash_table;
    Uint remaining;
    Uint idx;
    Uint max_iter;
    Uint i;
    Eterm* hp;
    Uint heap_size;
    struct copy_term {
        Uint key_size;
        Eterm* tuple_ptr;
    } *copy_data;

    hash_table = trap_data->table;
    idx = trap_data->idx;
#if defined(DEBUG) || defined(VALGRIND)
    max_iter = 50;
#else
    max_iter = ERTS_BIF_REDS_LEFT(c_p);
#endif
    remaining = trap_data->remaining < max_iter ?
        trap_data->remaining : max_iter;
    trap_data->remaining -= remaining;

    copy_data = (struct copy_term *) erts_alloc(ERTS_ALC_T_TMP,
                                                remaining *
                                                sizeof(struct copy_term));
    i = 0;
    heap_size = (2 + 3) * remaining;
    while (remaining != 0) {
        Eterm term = hash_table->term[idx];
        if (is_tuple(term)) {
            Uint key_size;
            Eterm* tup_val;

            ASSERT(is_tuple_arity(term, 2));
            tup_val = tuple_val(term);
            key_size = size_object(tup_val[1]);
            copy_data[i].key_size = key_size;
            copy_data[i].tuple_ptr = tup_val;
            heap_size += key_size;
            i++;
            remaining--;
        }
        idx++;
    }
    trap_data->idx = idx;

    hp = HAlloc(c_p, heap_size);
    remaining = i;
    for (i = 0; i < remaining; i++) {
        Eterm* tuple_ptr;
        Uint key_size;
        Eterm key;
        Eterm tup;

        tuple_ptr = copy_data[i].tuple_ptr;
        key_size = copy_data[i].key_size;
        key = copy_struct(tuple_ptr[1], key_size, &hp, &c_p->off_heap);
        tup = TUPLE2(hp, key, tuple_ptr[2]);
        hp += 3;
        res = CONS(hp, tup, res);
        hp += 2;
    }
    erts_free(ERTS_ALC_T_TMP, copy_data);
    return res;
}

static BIF_RETTYPE
persistent_term_info_trap(BIF_ALIST_1)
{
    TrapData* trap_data = (TrapData *) BIF_ARG_1;
    Eterm res;
    Uint bump_reds;
    Binary* mbp;

    mbp = erts_magic_ref2bin(BIF_ARG_1);
    trap_data = ERTS_MAGIC_BIN_DATA(mbp);
    bump_reds = trap_data->remaining;
    res = do_info(BIF_P, trap_data);
    if (trap_data->remaining > 0) {
        ASSERT(res == am_ok);
        BUMP_ALL_REDS(BIF_P);
        BIF_TRAP1(&persistent_term_info_export, BIF_P, BIF_ARG_1);
    } else {
        /*
         * Decrement ref count (and possibly delete the hash table
         * and associated literal area).
         */
        dec_table_refc(BIF_P, trap_data->table);
        trap_data->table = NULL; /* Prevent refc decrement */
        BUMP_REDS(BIF_P, bump_reds);
        ASSERT(is_map(res));
        BIF_RET(res);
    }
}

#define DECL_AM(S) Eterm AM_ ## S = am_atom_put(#S, sizeof(#S) - 1)

static Eterm
do_info(Process* c_p, TrapData* trap_data)
{
    HashTable* hash_table;
    Uint remaining;
    Uint idx;
    Uint max_iter;

    hash_table = trap_data->table;
    idx = trap_data->idx;
#if defined(DEBUG) || defined(VALGRIND)
    max_iter = 50;
#else
    max_iter = ERTS_BIF_REDS_LEFT(c_p);
#endif
    remaining = trap_data->remaining < max_iter ? trap_data->remaining : max_iter;
    trap_data->remaining -= remaining;
    while (remaining != 0) {
        if (is_boxed(hash_table->term[idx])) {
            ErtsLiteralArea* area;
            area = term_to_area(hash_table->term[idx]);
            trap_data->memory += sizeof(ErtsLiteralArea) +
                sizeof(Eterm) * (area->end - area->start - 1);
            remaining--;
        }
        idx++;
    }
    trap_data->idx = idx;
    if (trap_data->remaining > 0) {
        return am_ok;           /* Dummy return value */
    } else {
        Eterm* hp;
        Eterm count_term;
        Eterm memory_term;
        Eterm res;
        Uint memory;
        Uint hsz = MAP_SZ(2);

        memory = sizeof(HashTable) + (trap_data->table->allocated-1) *
            sizeof(Eterm) + trap_data->memory;
        (void) erts_bld_uint(NULL, &hsz, hash_table->num_entries);
        (void) erts_bld_uint(NULL, &hsz, memory);
        hp = HAlloc(c_p, hsz);
	count_term = erts_bld_uint(&hp, NULL, hash_table->num_entries);
	memory_term = erts_bld_uint(&hp, NULL, memory);
        res = MAP2(hp, am_count, count_term, am_memory, memory_term);
        return res;
    }
}

#undef DECL_AM

static Eterm
alloc_trap_data(Process* c_p)
{
    Binary* mbp = erts_create_magic_binary(sizeof(TrapData),
                                           cleanup_trap_data);
    Eterm* hp;

    hp = HAlloc(c_p, ERTS_MAGIC_REF_THING_SIZE);
    return erts_mk_magic_ref(&hp, &MSO(c_p), mbp);
}

static int
cleanup_trap_data(Binary *bp)
{
    TrapData* trap_data = ERTS_MAGIC_BIN_DATA(bp);

    if (trap_data->table) {
        /*
         * The process has been killed and is now exiting.
         * Decrement the reference counter for the table.
         */
        dec_table_refc(NULL, trap_data->table);
    }
    return 1;
}

static Uint
lookup(HashTable* hash_table, Eterm key)
{
    Uint mask = hash_table->mask;
    Eterm* table = hash_table->term;
    Uint32 idx = make_internal_hash(key, 0);
    Eterm term;

    do {
        idx++;
        term = table[idx & mask];
    } while (is_boxed(term) && !EQ(key, (tuple_val(term))[1]));
    return idx & mask;
}

static HashTable*
copy_table(ErtsPersistentTermCpyTableCtx* ctx)
{
    Uint old_size = ctx->old_table->allocated;
    Uint i;
    ErtsAlcType_t alloc_type;
    ctx->total_iterations_done = 0;
    switch(ctx->location) {
    case ERTS_PERSISTENT_TERM_CPY_PLACE_1: goto L_copy_table_place_1;
    case ERTS_PERSISTENT_TERM_CPY_PLACE_2: goto L_copy_table_place_2;
    case ERTS_PERSISTENT_TERM_CPY_PLACE_3: goto L_copy_table_place_3;
    case ERTS_PERSISTENT_TERM_CPY_PLACE_START:
        ctx->iterations_done = 0;
    }
    if (ctx->copy_type == ERTS_PERSISTENT_TERM_CPY_TEMP) {
        alloc_type = ERTS_ALC_T_PERSISTENT_TERM_TMP;
    } else {
        alloc_type = ERTS_ALC_T_PERSISTENT_TERM;
    }
    ctx->new_table = (HashTable *) erts_alloc(alloc_type,
                                              sizeof(HashTable) +
                                              sizeof(Eterm) * (ctx->new_size-1));
    if (ctx->old_table->allocated == ctx->new_size &&
        (ctx->copy_type == ERTS_PERSISTENT_TERM_CPY_NO_REHASH ||
         ctx->copy_type == ERTS_PERSISTENT_TERM_CPY_TEMP)) {
        /*
         * Same size and no key deleted. Make an exact copy of the table.
         */
        *ctx->new_table = *ctx->old_table;
    L_copy_table_place_1:
        for (i = ctx->iterations_done;
             i < MIN(ctx->iterations_done + ctx->max_iterations,
                     ctx->new_size);
             i++) {
            ctx->new_table->term[i] = ctx->old_table->term[i];
        }
        ctx->total_iterations_done = (i - ctx->iterations_done);
        if (i < ctx->new_size) {
            ctx->iterations_done = i;
            ctx->location = ERTS_PERSISTENT_TERM_CPY_PLACE_1;
            return NULL;
        }
        ctx->iterations_done = 0;
    } else {
        /*
         * The size of the table has changed or an element has been
         * deleted. Must rehash, by inserting all old terms into the
         * new (empty) table.
         */
        ctx->new_table->allocated = ctx->new_size;
        ctx->new_table->num_entries = ctx->old_table->num_entries;
        ctx->new_table->mask = ctx->new_size - 1;
    L_copy_table_place_2:
        for (i = ctx->iterations_done;
             i < MIN(ctx->iterations_done + ctx->max_iterations,
                     ctx->new_size);
             i++) {
            ctx->new_table->term[i] = NIL;
        }
        ctx->total_iterations_done = (i - ctx->iterations_done);
        ctx->max_iterations -= ctx->total_iterations_done;
        if (i < ctx->new_size) {
            ctx->iterations_done = i;
            ctx->location = ERTS_PERSISTENT_TERM_CPY_PLACE_2;
            return NULL;
        }
        ctx->iterations_done = 0;
    L_copy_table_place_3:
        for (i = ctx->iterations_done;
             i < MIN(ctx->iterations_done + ctx->max_iterations,
                     old_size);
             i++) {
            if (is_tuple(ctx->old_table->term[i])) {
                Eterm key = tuple_val(ctx->old_table->term[i])[1];
                Uint entry_index = lookup(ctx->new_table, key);
                ASSERT(is_nil(ctx->new_table->term[entry_index]));
                ctx->new_table->term[entry_index] = ctx->old_table->term[i];
            }
        }
        ctx->total_iterations_done += (i - ctx->iterations_done);
        if (i < old_size) {
            ctx->iterations_done = i;
            ctx->location = ERTS_PERSISTENT_TERM_CPY_PLACE_3;
            return NULL;
        }
        ctx->iterations_done = 0;
    }
    ctx->new_table->first_to_delete = 0;
    ctx->new_table->num_to_delete = 0;
    erts_atomic_init_nob(&ctx->new_table->refc, (erts_aint_t)1);
    {
        HashTable* new_table = ctx->new_table;
        /*
         * IMPORTANT: Memory management depends on that ctx->new_table is
         * set to NULL on the line below
         */
        ctx->new_table = NULL;
        return new_table;
    }
}

static void
mark_for_deletion(HashTable* hash_table, Uint entry_index)
{
    hash_table->first_to_delete = entry_index;
    hash_table->num_to_delete = 1;
}

static ErtsLiteralArea*
term_to_area(Eterm tuple)
{
    ASSERT(is_tuple_arity(tuple, 2));
    return (ErtsLiteralArea *) (((char *) tuple_val(tuple)) -
                                offsetof(ErtsLiteralArea, start));
}

static void
table_updater(void* data)
{
    HashTable* old_table;
    HashTable* new_table;

    old_table = (HashTable *) erts_atomic_read_nob(&the_hash_table);
    new_table = (HashTable *) data;
    ASSERT(new_table->num_to_delete == 0);
    erts_atomic_set_nob(&the_hash_table, (erts_aint_t)new_table);
    append_to_delete_queue(old_table);
    erts_schedule_thr_prgr_later_op(table_deleter,
                                    old_table,
                                    &old_table->thr_prog_op);
    release_update_permission(1);
}

static void
table_deleter(void* data)
{
    HashTable* old_table = (HashTable *) data;

    dec_table_refc(NULL, old_table);
}

static void
dec_table_refc(Process* c_p, HashTable* old_table)
{
    erts_aint_t refc = erts_atomic_dec_read_nob(&old_table->refc);

    if (refc == 0) {
        HashTable* to_delete;

        while ((to_delete = next_to_delete()) != NULL) {
            delete_table(c_p, to_delete);
        }
    }
}

static void
delete_table(Process* c_p, HashTable* table)
{
    Uint idx = table->first_to_delete;
    Uint n = table->num_to_delete;

    /*
     * There are no longer any references to this hash table.
     *
     * Any literals pointed for deletion can be queued for
     * deletion and the table itself can be deallocated.
     */

#ifdef DEBUG
    if (n == 1) {
        ASSERT(is_tuple_arity(table->term[idx], 2));
    }
#endif

    while (n > 0) {
        Eterm term = table->term[idx];

        if (is_tuple_arity(term, 2)) {
            if (is_immed(tuple_val(term)[2])) {
                erts_release_literal_area(term_to_area(term));
            } else {
                erts_queue_release_literals(c_p, term_to_area(term));
            }
        }
        idx++, n--;
    }
    erts_free(ERTS_ALC_T_PERSISTENT_TERM, table);
}

/*
 * Caller *must* yield if this function returns 0.
 */

static int
try_seize_update_permission(Process* c_p)
{
    int success;

    ASSERT(!erts_thr_progress_is_blocking()); /* to avoid deadlock */
    ASSERT(c_p != NULL);

    erts_mtx_lock(&update_table_permission_mtx);
    ASSERT(updater_process != c_p);
    success = (updater_process == NULL);
    if (success) {
        updater_process = c_p;
    } else {
        struct update_queue_item* qitem;
        qitem = erts_alloc(ERTS_ALC_T_PERSISTENT_LOCK_Q, sizeof(*qitem));
        qitem->p = c_p;
        erts_proc_inc_refc(c_p);
        qitem->next = update_queue;
        update_queue = qitem;
        erts_suspend(c_p, ERTS_PROC_LOCK_MAIN, NULL);
    }
    erts_mtx_unlock(&update_table_permission_mtx);
    return success;
}

static void
release_update_permission(int release_updater)
{
    erts_mtx_lock(&update_table_permission_mtx);
    ASSERT(updater_process != NULL);

    if (release_updater) {
        erts_proc_lock(updater_process, ERTS_PROC_LOCK_STATUS);
        if (!ERTS_PROC_IS_EXITING(updater_process)) {
            erts_resume(updater_process, ERTS_PROC_LOCK_STATUS);
        }
        erts_proc_unlock(updater_process, ERTS_PROC_LOCK_STATUS);
        erts_proc_dec_refc(updater_process);
    }
    updater_process = NULL;

    while (update_queue != NULL) { /* Unleash the entire herd */
	struct update_queue_item* qitem = update_queue;
	erts_proc_lock(qitem->p, ERTS_PROC_LOCK_STATUS);
	if (!ERTS_PROC_IS_EXITING(qitem->p)) {
	    erts_resume(qitem->p, ERTS_PROC_LOCK_STATUS);
	}
	erts_proc_unlock(qitem->p, ERTS_PROC_LOCK_STATUS);
	update_queue = qitem->next;
	erts_proc_dec_refc(qitem->p);
	erts_free(ERTS_ALC_T_PERSISTENT_LOCK_Q, qitem);
    }
    erts_mtx_unlock(&update_table_permission_mtx);
}

static void
suspend_updater(Process* c_p)
{
#ifdef DEBUG
    ASSERT(c_p != NULL);
    erts_mtx_lock(&update_table_permission_mtx);
    ASSERT(updater_process == c_p);
    erts_mtx_unlock(&update_table_permission_mtx);
#endif
    erts_proc_inc_refc(c_p);
    erts_suspend(c_p, ERTS_PROC_LOCK_MAIN, NULL);
}

static void
append_to_delete_queue(HashTable* table)
{
    erts_mtx_lock(&delete_queue_mtx);
    table->delete_next = NULL;
    *delete_queue_tail = table;
    delete_queue_tail = &table->delete_next;
    erts_mtx_unlock(&delete_queue_mtx);
}

static HashTable*
next_to_delete(void)
{
    HashTable* table;

    erts_mtx_lock(&delete_queue_mtx);
    table = delete_queue_head;
    if (table) {
        if (erts_atomic_read_nob(&table->refc)) {
            /*
             * This hash table is still referenced. Hash tables
             * must be deleted in order, so we return a NULL
             * pointer.
             */
            table = NULL;
        } else {
            /*
             * Remove the first hash table from the queue.
             */
            delete_queue_head = table->delete_next;
            if (delete_queue_head == NULL) {
                delete_queue_tail = &delete_queue_head;
            }
        }
    }
    erts_mtx_unlock(&delete_queue_mtx);
    return table;
}

/*
 * test/debug functionality follow...
 */

static Uint accessed_literal_areas_size;
static Uint accessed_no_literal_areas;
static ErtsLiteralArea **accessed_literal_areas;

int
erts_debug_have_accessed_literal_area(ErtsLiteralArea *lap)
{
    Uint i;
    for (i = 0; i < accessed_no_literal_areas; i++) {
        if (accessed_literal_areas[i] == lap)
            return !0;
    }
    return 0;
}

void
erts_debug_save_accessed_literal_area(ErtsLiteralArea *lap)
{
    if (accessed_no_literal_areas == accessed_literal_areas_size) {
        accessed_literal_areas_size += 10;
        accessed_literal_areas = erts_realloc(ERTS_ALC_T_TMP,
                                              accessed_literal_areas,
                                              (sizeof(ErtsLiteralArea *)
                                               *accessed_literal_areas_size));
    }
    accessed_literal_areas[accessed_no_literal_areas++] = lap;
}

static void debug_foreach_off_heap(HashTable *tbl, void (*func)(ErlOffHeap *, void *), void *arg)
{
    int i;
    
    for (i = 0; i < tbl->allocated; i++) {
        Eterm term = tbl->term[i];
        if (is_tuple_arity(term, 2)) {
            ErtsLiteralArea *lap = term_to_area(term);
            ErlOffHeap oh;
            if (!erts_debug_have_accessed_literal_area(lap)) {
                ERTS_INIT_OFF_HEAP(&oh);
                oh.first = lap->off_heap;
                (*func)(&oh, arg);
                erts_debug_save_accessed_literal_area(lap);
            }
        }
    }
}

struct debug_la_oh {
    void (*func)(ErlOffHeap *, void *);
    void *arg;
};

static void debug_handle_table(void *vfap,
                               ErtsThrPrgrVal val,
                               void *vtbl)
{
    struct debug_la_oh *fap = vfap;
    HashTable *tbl = vtbl;
    debug_foreach_off_heap(tbl, fap->func, fap->arg);
}

void
erts_debug_foreach_persistent_term_off_heap(void (*func)(ErlOffHeap *, void *), void *arg)
{
    HashTable *tbl;
    struct debug_la_oh fa;
    accessed_no_literal_areas = 0;
    accessed_literal_areas_size = 10;
    accessed_literal_areas = erts_alloc(ERTS_ALC_T_TMP,
                                        (sizeof(ErtsLiteralArea *)
                                         * accessed_literal_areas_size));
    
    tbl = (HashTable *) erts_atomic_read_nob(&the_hash_table);
    debug_foreach_off_heap(tbl, func, arg);
    erts_mtx_lock(&delete_queue_mtx);
    for (tbl = delete_queue_head; tbl; tbl = tbl->delete_next)
        debug_foreach_off_heap(tbl, func, arg);
    erts_mtx_unlock(&delete_queue_mtx);
    fa.func = func;
    fa.arg = arg;
    erts_debug_later_op_foreach(table_updater,
                                debug_handle_table,
                                (void *) &fa);
    erts_debug_later_op_foreach(table_deleter,
                                debug_handle_table,
                                (void *) &fa);
    erts_debug_foreach_release_literal_area_off_heap(func, arg);
    
    erts_free(ERTS_ALC_T_TMP, accessed_literal_areas);
    accessed_no_literal_areas = 0;
    accessed_literal_areas_size = 0;
    accessed_literal_areas = NULL;
}

