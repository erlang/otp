/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2019. All Rights Reserved.
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
 * Author: Kjell Winblad
 */

#include "erl_flxctr.h"

static int reader_groups_array_size = 0;
#define ERTS_FLXCTR_DECENTRALIZED_NO_SLOTS (reader_groups_array_size)

static int erts_flxctr_read_ctx_bin_dtor(Binary *context_bin);
static int erts_flxctr_wait_dtor(Binary *context_bin);

typedef struct {
    ErtsThrPrgrLaterOp later_op;
    Process* process;
    ErtsFlxCtrDecentralizedCtrArray* array;
    ErtsFlxCtrDecentralizedCtrArray* next_array;
    ErtsAlcType_t alloc_type;
    int nr_of_counters;
    Sint result[ERTS_FLXCTR_ATOMICS_PER_CACHE_LINE];
} DecentralizedReadSnapshotInfo;

typedef enum {
    ERTS_FLXCTR_SNAPSHOT_NOT_ONGOING = 0,
    ERTS_FLXCTR_SNAPSHOT_ONGOING = 1,
    ERTS_FLXCTR_SNAPSHOT_ONGOING_TP_THREAD_DO_FREE = 2
} erts_flxctr_snapshot_status;

static void
thr_prg_wake_up_and_count(void* bin_p)
{
    Binary* bin = bin_p;
    DecentralizedReadSnapshotInfo* info = ERTS_MAGIC_BIN_DATA(bin);
    Process* p = info->process;
    ErtsFlxCtrDecentralizedCtrArray* array = info->array;
    ErtsFlxCtrDecentralizedCtrArray* next = info->next_array;
    int i, sched;
    /* Reset result array */
    for (i = 0; i < info->nr_of_counters; i++) {
        info->result[i] = 0;
    }
    /* Read result from snapshot */
    for (sched = 0; sched < ERTS_FLXCTR_DECENTRALIZED_NO_SLOTS; sched++) {
        for (i = 0; i < info->nr_of_counters; i++) {
            info->result[i] = info->result[i] +
                erts_atomic_read_nob(&array->array[sched].counters[i]);
        }
    }
    /* Update the next decentralized counter array */
    for (i = 0; i < info->nr_of_counters; i++) {
        erts_atomic_add_nob(&next->array[0].counters[i], info->result[i]);
    }
    /* Announce that the snapshot is done */
    {
    Sint expected = ERTS_FLXCTR_SNAPSHOT_ONGOING;
    if (expected != erts_atomic_cmpxchg_mb(&next->snapshot_status,
                                           ERTS_FLXCTR_SNAPSHOT_NOT_ONGOING,
                                           expected)) {
        /* The CAS failed which means that this thread need to free the next array. */
        erts_free(info->alloc_type, next->block_start);
    }
    }
    /* Resume the process that requested the snapshot */
    erts_proc_lock(p, ERTS_PROC_LOCK_STATUS);
    if (!ERTS_PROC_IS_EXITING(p)) {
        erts_resume(p, ERTS_PROC_LOCK_STATUS);
    }
    /* Free the memory that is no longer needed */
    erts_free(info->alloc_type, array->block_start);
    erts_proc_unlock(p, ERTS_PROC_LOCK_STATUS);
    erts_proc_dec_refc(p);
    erts_bin_release(bin);
}

typedef struct {
    ErtsThrPrgrLaterOp later_op;
    Process* process;
} ErtsFlxCtrWakeUpLaterInfo;

static void
thr_prg_wake_up_later(void* bin_p)
{
    Binary* bin = bin_p;
    ErtsFlxCtrWakeUpLaterInfo* info = ERTS_MAGIC_BIN_DATA(bin);
    Process* p = info->process;
    /* Resume the requesting process */
    erts_proc_lock(p, ERTS_PROC_LOCK_STATUS);
    if (!ERTS_PROC_IS_EXITING(p)) {
        erts_resume(p, ERTS_PROC_LOCK_STATUS);
    }
    erts_proc_unlock(p, ERTS_PROC_LOCK_STATUS);
    /* Free data */
    erts_proc_dec_refc(p);
    erts_bin_release(bin);
}

static
int erts_flxctr_read_ctx_bin_dtor(Binary *context_bin) {
    (void)context_bin;
    return 1;
}

static
int erts_flxctr_wait_dtor(Binary *context_bin) {
    (void)context_bin;
    return 1;
}

static void suspend_until_thr_prg(Process* p)
{
    Binary* state_bin;
    ErtsFlxCtrWakeUpLaterInfo* info;
    state_bin = erts_create_magic_binary(sizeof(ErtsFlxCtrWakeUpLaterInfo),
                                         erts_flxctr_wait_dtor);
    info = ERTS_MAGIC_BIN_DATA(state_bin);
    info->process = p;
    erts_refc_inctest(&state_bin->intern.refc, 1);
    erts_suspend(p, ERTS_PROC_LOCK_MAIN, NULL);
    erts_proc_inc_refc(p);
    ERTS_VBUMP_ALL_REDS(p);
    erts_schedule_thr_prgr_later_op(thr_prg_wake_up_later, state_bin, &info->later_op);
}


static ErtsFlxCtrDecentralizedCtrArray*
create_decentralized_ctr_array(ErtsAlcType_t alloc_type, Uint nr_of_counters) {
    /* Allocate an ErtsFlxCtrDecentralizedCtrArray and make sure that
       the array field is located at the start of a cache line */
    char* bytes =
        erts_alloc(alloc_type,
                   sizeof(ErtsFlxCtrDecentralizedCtrArray) +
                   (sizeof(ErtsFlxCtrDecentralizedCtrArrayElem) *
                    ERTS_FLXCTR_DECENTRALIZED_NO_SLOTS) +
                   ERTS_CACHE_LINE_SIZE);
    void* block_start = bytes;
    int bytes_to_next_cacheline_border;
    ErtsFlxCtrDecentralizedCtrArray* array;
    int i, sched;
    bytes = &bytes[offsetof(ErtsFlxCtrDecentralizedCtrArray, array)];
    bytes_to_next_cacheline_border =
        ERTS_CACHE_LINE_SIZE - (((Uint)bytes) % ERTS_CACHE_LINE_SIZE);
    array = (ErtsFlxCtrDecentralizedCtrArray*)
        (&bytes[bytes_to_next_cacheline_border -
                (int)offsetof(ErtsFlxCtrDecentralizedCtrArray, array)]);
    ASSERT(((Uint)array->array) % ERTS_CACHE_LINE_SIZE == 0);
    ASSERT(((Uint)array - (Uint)block_start) <= ERTS_CACHE_LINE_SIZE);
    /* Initialize fields */
    erts_atomic_init_nob(&array->snapshot_status, ERTS_FLXCTR_SNAPSHOT_ONGOING);
    for (sched = 0; sched < ERTS_FLXCTR_DECENTRALIZED_NO_SLOTS; sched++) {
        for (i = 0; i < nr_of_counters; i++) {
            erts_atomic_init_nob(&array->array[sched].counters[i], 0);
        }
    }
    array->block_start = block_start;
    return array;
}

void erts_flxctr_setup(int decentralized_counter_groups)
{
    reader_groups_array_size = decentralized_counter_groups+1;
}

void erts_flxctr_init(ErtsFlxCtr* c,
                      int is_decentralized,
                      Uint nr_of_counters,
                      ErtsAlcType_t alloc_type)
{
    ASSERT(nr_of_counters <= ERTS_FLXCTR_ATOMICS_PER_CACHE_LINE);
    c->is_decentralized = is_decentralized;
    c->nr_of_counters = nr_of_counters;
    if (c->is_decentralized) {
        ErtsFlxCtrDecentralizedCtrArray* array =
            create_decentralized_ctr_array(alloc_type, nr_of_counters);
        erts_atomic_set_nob(&array->snapshot_status,
                            ERTS_FLXCTR_SNAPSHOT_NOT_ONGOING);
        erts_atomic_init_nob(&c->u.counters_ptr, (Sint)array);
        ASSERT(((Uint)array->array) % ERTS_CACHE_LINE_SIZE == 0);
    } else {
        int i;
        for (i = 0; i < nr_of_counters; i++) {
            erts_atomic_init_nob(&c->u.counters[i], 0);
        }
    }
}

void erts_flxctr_destroy(ErtsFlxCtr* c, ErtsAlcType_t type)
{
    if (c->is_decentralized) {
        if (erts_flxctr_is_snapshot_ongoing(c)) {
            ErtsFlxCtrDecentralizedCtrArray* array =
                ERTS_FLXCTR_GET_CTR_ARRAY_PTR(c);
            /* Try to delegate the resposibilty of freeing to
               thr_prg_wake_up_and_count */
            Sint expected = ERTS_FLXCTR_SNAPSHOT_ONGOING;
            if (expected !=
                erts_atomic_cmpxchg_mb(&array->snapshot_status,
                                       ERTS_FLXCTR_SNAPSHOT_ONGOING_TP_THREAD_DO_FREE,
                                       expected)) {
                /* The delegation was unsuccessful which means that no
                   snapshot is ongoing anymore and the freeing needs
                   to be done here */
                ERTS_ASSERT(!erts_flxctr_is_snapshot_ongoing(c));
                erts_free(type, array->block_start);
            }
        } else {
            erts_free(type, ERTS_FLXCTR_GET_CTR_ARRAY_PTR(c)->block_start);
        }
    }
}

ErtsFlxCtrSnapshotResult
erts_flxctr_snapshot(ErtsFlxCtr* c,
                     ErtsAlcType_t alloc_type,
                     Process* p)
{
    if (c->is_decentralized) {
        ErtsFlxCtrDecentralizedCtrArray* array = ERTS_FLXCTR_GET_CTR_ARRAY_PTR(c);
        if (erts_flxctr_is_snapshot_ongoing(c)) {
            /* Let the caller try again later */
            ErtsFlxCtrSnapshotResult res =
                {.type = ERTS_FLXCTR_TRY_AGAIN_AFTER_TRAP};
            suspend_until_thr_prg(p);
            return res;
        } else {
            Eterm* hp;
            Binary* state_bin;
            Eterm state_mref;
            DecentralizedReadSnapshotInfo* info;
            ErtsFlxCtrDecentralizedCtrArray* new_array =
                create_decentralized_ctr_array(alloc_type, c->nr_of_counters);
            int success =
                ((Sint)array) == erts_atomic_cmpxchg_mb(&c->u.counters_ptr,
                                                        (Sint)new_array,
                                                        (Sint)array);
            if (!success) {
                /* Let the caller try again later */
                ErtsFlxCtrSnapshotResult res =
                    {.type = ERTS_FLXCTR_TRY_AGAIN_AFTER_TRAP};
                suspend_until_thr_prg(p);
                erts_free(alloc_type, new_array->block_start);
                return res;
            }
            /* Create binary with info about the operation that can be
               sent to the caller and to a thread progress function */
            state_bin =
                erts_create_magic_binary(sizeof(DecentralizedReadSnapshotInfo),
                                         erts_flxctr_read_ctx_bin_dtor);
            hp = HAlloc(p, ERTS_MAGIC_REF_THING_SIZE);
            state_mref = erts_mk_magic_ref(&hp, &MSO(p), state_bin);
            info = ERTS_MAGIC_BIN_DATA(state_bin);
            info->alloc_type = alloc_type;
            info->array = array;
            info->next_array = new_array;
            info->process = p;
            info->nr_of_counters = c->nr_of_counters;
            erts_proc_inc_refc(p);
            erts_refc_inctest(&state_bin->intern.refc, 2);
            erts_suspend(p, ERTS_PROC_LOCK_MAIN, NULL);
            ERTS_VBUMP_ALL_REDS(p);
            erts_schedule_thr_prgr_later_op(thr_prg_wake_up_and_count,
                                            state_bin,
                                            &info->later_op);
            {
                ErtsFlxCtrSnapshotResult res = {
                    .type = ERTS_FLXCTR_GET_RESULT_AFTER_TRAP,
                    .trap_resume_state = state_mref};
                return res;
            }
        }
    } else {
        ErtsFlxCtrSnapshotResult res;
        int i;
        res.type = ERTS_FLXCTR_DONE;
        for (i = 0; i < c->nr_of_counters; i++){
            res.result[i] = erts_flxctr_read_centralized(c, i);
        }
        return res;
    }
}


Sint erts_flxctr_get_snapshot_result_after_trap(Eterm result_holder,
                                            Uint counter_nr)
{
    Binary* bin = erts_magic_ref2bin(result_holder);
    DecentralizedReadSnapshotInfo* data = ERTS_MAGIC_BIN_DATA(bin);;
    return data->result[counter_nr];
}

int erts_flxctr_is_snapshot_result(Eterm term)
{
    if (is_internal_magic_ref(term)) {
        Binary* bin = erts_magic_ref2bin(term);
        return ERTS_MAGIC_BIN_DESTRUCTOR(bin) ==  erts_flxctr_read_ctx_bin_dtor;
    } else return 0;
}

Sint erts_flxctr_read_approx(ErtsFlxCtr* c,
                             Uint counter_nr)
{
    if (c->is_decentralized) {
        ErtsFlxCtrDecentralizedCtrArray* counter = ERTS_FLXCTR_GET_CTR_ARRAY_PTR(c);
        Sint sum = 0;
        int sched;
        for (sched = 0; sched < ERTS_FLXCTR_DECENTRALIZED_NO_SLOTS; sched++) {
            sum = sum + erts_atomic_read_nob(&counter->array[sched].counters[counter_nr]);
        }
        return sum;
    } else {
        return erts_flxctr_read_centralized(c, counter_nr);
    }
}

int erts_flxctr_is_snapshot_ongoing(ErtsFlxCtr* c)
{
    return c->is_decentralized &&
        (ERTS_FLXCTR_SNAPSHOT_NOT_ONGOING !=
         erts_atomic_read_acqb(&ERTS_FLXCTR_GET_CTR_ARRAY_PTR(c)->snapshot_status));
}

int erts_flxctr_suspend_until_thr_prg_if_snapshot_ongoing(ErtsFlxCtr* c, Process* p)
{
    if (erts_flxctr_is_snapshot_ongoing(c)) {
        suspend_until_thr_prg(p);
        return 1;
    } else {
        return 0;
    }
}

void erts_flxctr_reset(ErtsFlxCtr* c,
                       Uint counter_nr)
{
    if (c->is_decentralized) {
        int sched;
        ErtsFlxCtrDecentralizedCtrArray* counter =
            ERTS_FLXCTR_GET_CTR_ARRAY_PTR(c);
        for (sched = 0; sched < ERTS_FLXCTR_DECENTRALIZED_NO_SLOTS; sched++) {
            erts_atomic_set_nob(&counter->array[sched].counters[counter_nr], 0);
        }
    } else {
        erts_atomic_set_nob(&c->u.counters[counter_nr], 0);
    }
}


void erts_flxctr_set_slot(int group) {
    ErtsSchedulerData *esdp = erts_get_scheduler_data();
    esdp->flxctr_slot_no = group;
}
