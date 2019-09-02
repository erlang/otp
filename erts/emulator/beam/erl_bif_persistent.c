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
 * The limit for the number of persistent terms before
 * a warning is issued.
 */

#define WARNING_LIMIT 20000
#define XSTR(s) STR(s)
#define STR(s) #s

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

/*
 * Declarations of local functions.
 */

static HashTable* create_initial_table(void);
static Uint lookup(HashTable* hash_table, Eterm key);
static HashTable* copy_table(HashTable* old_table, Uint new_size, int rehash);
static HashTable* tmp_table_copy(HashTable* old_table);
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
static int issued_warning = 0;

/*
 * Queue of hash tables to be deleted.
 */

static erts_mtx_t delete_queue_mtx;
static HashTable* delete_queue_head = NULL;
static HashTable** delete_queue_tail = &delete_queue_head;

/*
 * The following variables are only used during crash dumping. They
 * are intialized by erts_init_persistent_dumping().
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

BIF_RETTYPE persistent_term_put_2(BIF_ALIST_2)
{
    Eterm key;
    Eterm term;
    Eterm heap[3];
    Eterm tuple;
    HashTable* hash_table;
    Uint term_size;
    Uint lit_area_size;
    ErlOffHeap code_off_heap;
    ErtsLiteralArea* literal_area;
    erts_shcopy_t info;
    Eterm* ptr;
    Uint entry_index;

    if (!try_seize_update_permission(BIF_P)) {
	ERTS_BIF_YIELD2(bif_export[BIF_persistent_term_put_2],
                        BIF_P, BIF_ARG_1, BIF_ARG_2);
    }

    hash_table = (HashTable *) erts_atomic_read_nob(&the_hash_table);

    key = BIF_ARG_1;
    term = BIF_ARG_2;

    entry_index = lookup(hash_table, key);

    heap[0] = make_arityval(2);
    heap[1] = key;
    heap[2] = term;
    tuple = make_tuple(heap);

    if (is_nil(hash_table->term[entry_index])) {
        Uint size = hash_table->allocated;
        if (MUST_GROW(hash_table)) {
            size *= 2;
        }
        hash_table = copy_table(hash_table, size, 0);
        entry_index = lookup(hash_table, key);
        hash_table->num_entries++;
    } else {
        Eterm tuple = hash_table->term[entry_index];
        Eterm old_term;

        ASSERT(is_tuple_arity(tuple, 2));
        old_term = boxed_val(tuple)[2];
        if (EQ(term, old_term)) {
            /* Same value. No need to update anything. */
            release_update_permission(0);
            BIF_RET(am_ok);
        } else {
            /* Mark the old term for deletion. */
            mark_for_deletion(hash_table, entry_index);
            hash_table = copy_table(hash_table, hash_table->allocated, 0);
        }
    }

    /*
     * Preserve internal sharing in the term by using the
     * sharing-preserving functions. However, literals must
     * be copied in case the module holding them are unloaded.
     */
    INITIALIZE_SHCOPY(info);
    info.copy_literals = 1;
    term_size = copy_shared_calculate(tuple, &info);
    ERTS_INIT_OFF_HEAP(&code_off_heap);
    lit_area_size = ERTS_LITERAL_AREA_ALLOC_SIZE(term_size);
    literal_area = erts_alloc(ERTS_ALC_T_LITERAL, lit_area_size);
    ptr = &literal_area->start[0];
    literal_area->end = ptr + term_size;
    tuple = copy_shared_perform(tuple, term_size, &info, &ptr, &code_off_heap);
    ASSERT(tuple_val(tuple) == literal_area->start);
    literal_area->off_heap = code_off_heap.first;
    DESTROY_SHCOPY(info);
    erts_set_literal_tag(&tuple, literal_area->start, term_size);
    hash_table->term[entry_index] = tuple;

    erts_schedule_thr_prgr_later_op(table_updater, hash_table, &thr_prog_op);
    suspend_updater(BIF_P);

    /*
     * Issue a warning once if the warning limit has been exceeded.
     */

    if (hash_table->num_entries > WARNING_LIMIT && issued_warning == 0) {
        static char w[] =
            "More than " XSTR(WARNING_LIMIT) " persistent terms "
            "have been created.\n"
            "It is recommended to avoid creating an excessive number of\n"
            "persistent terms, as creation and deletion of persistent terms\n"
            "will be slower as the number of persistent terms increases.\n";
        issued_warning = 1;
	erts_send_warning_to_logger_str(BIF_P->group_leader, w);
    }

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

BIF_RETTYPE persistent_term_erase_1(BIF_ALIST_1)
{
    Eterm key = BIF_ARG_1;
    HashTable* old_table;
    HashTable* new_table;
    Uint entry_index;
    Eterm old_term;

    if (!try_seize_update_permission(BIF_P)) {
	ERTS_BIF_YIELD1(bif_export[BIF_persistent_term_erase_1],
                        BIF_P, BIF_ARG_1);
    }

    old_table = (HashTable *) erts_atomic_read_nob(&the_hash_table);
    entry_index = lookup(old_table, key);
    old_term = old_table->term[entry_index];
    if (is_boxed(old_term)) {
        Uint new_size;
        HashTable* tmp_table;

        /*
         * Since we don't use any delete markers, we must rehash
         * the table when deleting terms to ensure that all terms
         * can still be reached if there are hash collisions.
         * We can't rehash in place and it would not be safe to modify
         * the old table yet, so we will first need a new
         * temporary table copy of the same size as the old one.
         */

        ASSERT(is_tuple_arity(old_term, 2));
        tmp_table = tmp_table_copy(old_table);

        /*
         * Delete the term from the temporary table. Then copy the
         * temporary table to a new table, rehashing the entries
         * while copying.
         */

        tmp_table->term[entry_index] = NIL;
        tmp_table->num_entries--;
        new_size = tmp_table->allocated;
        if (MUST_SHRINK(tmp_table)) {
            new_size /= 2;
        }
        new_table = copy_table(tmp_table, new_size, 1);
        erts_free(ERTS_ALC_T_TMP, tmp_table);

        mark_for_deletion(old_table, entry_index);
        erts_schedule_thr_prgr_later_op(table_updater, new_table, &thr_prog_op);
        suspend_updater(BIF_P);
        ERTS_BIF_YIELD_RETURN(BIF_P, am_true);
    }

    /*
     * Key is not present. Nothing to do.
     */

    ASSERT(is_nil(old_term));
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
tmp_table_copy(HashTable* old_table)
{
    Uint size = old_table->allocated;
    HashTable* tmp_table;
    Uint i;

    tmp_table = (HashTable *) erts_alloc(ERTS_ALC_T_TMP,
                                         sizeof(HashTable) +
                                         sizeof(Eterm) * (size-1));
    *tmp_table = *old_table;
    for (i = 0; i < size; i++) {
        tmp_table->term[i] = old_table->term[i];
    }
    return tmp_table;
}

static HashTable*
copy_table(HashTable* old_table, Uint new_size, int rehash)
{
    HashTable* new_table;
    Uint old_size = old_table->allocated;
    Uint i;

    new_table = (HashTable *) erts_alloc(ERTS_ALC_T_PERSISTENT_TERM,
                                         sizeof(HashTable) +
                                         sizeof(Eterm) * (new_size-1));
    if (old_table->allocated == new_size && !rehash) {
        /*
         * Same size and no key deleted. Make an exact copy of the table.
         */
        *new_table = *old_table;
        for (i = 0; i < new_size; i++) {
            new_table->term[i] = old_table->term[i];
        }
    } else {
        /*
         * The size of the table has changed or an element has been
         * deleted. Must rehash, by inserting all old terms into the
         * new (empty) table.
         */
        new_table->allocated = new_size;
        new_table->num_entries = old_table->num_entries;
        new_table->mask = new_size - 1;
        for (i = 0; i < new_size; i++) {
            new_table->term[i] = NIL;
        }
        for (i = 0; i < old_size; i++) {
            if (is_tuple(old_table->term[i])) {
                Eterm key = tuple_val(old_table->term[i])[1];
                Uint entry_index = lookup(new_table, key);
                ASSERT(is_nil(new_table->term[entry_index]));
                new_table->term[entry_index] = old_table->term[i];
            }
        }
    }
    new_table->first_to_delete = 0;
    new_table->num_to_delete = 0;
    erts_atomic_init_nob(&new_table->refc, (erts_aint_t)1);
    return new_table;
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
