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

/**
 * @file erl_flxctr.h
 *
 * @brief This file contains the API of a flexible counter. The
 * counter can be configured during its initialization to be
 * centralized or decentralized. The centralized configuration makes
 * it possible to read the counter value extremely efficiently, but
 * updates of the counter value can easily cause contention. The
 * decentralized configuration has the reverse trade-off (i.e.,
 * updates are efficient and scalable but reading the counter value is
 * slow and may cause contention).
 *
 * @author Kjell Winblad
 */

#ifndef ERL_FLXCTR_H__
#define ERL_FLXCTR_H__

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif
#include "sys.h"
#include "erl_vm.h"
#include "global.h"
#include "error.h"
#include "bif.h"
#include "big.h"
#include "erl_binary.h"
#include "bif.h"
#include <stddef.h>

/* Public Interface */

#define ERTS_MAX_FLXCTR_GROUPS 256
#define ERTS_FLXCTR_ATOMICS_PER_CACHE_LINE (ERTS_CACHE_LINE_SIZE / sizeof(erts_atomic_t))

typedef struct {
    int nr_of_counters;
    int is_decentralized;
    union {
        erts_atomic_t counters_ptr;
        erts_atomic_t counters[1];
    } u;
} ErtsFlxCtr;

#define ERTS_FLXCTR_NR_OF_EXTRA_BYTES(NR_OF_COUNTERS)   \
    ((NR_OF_COUNTERS-1) * sizeof(erts_atomic_t))

/* Called by early_init */
void erts_flxctr_setup(int decentralized_counter_groups);

/**
 * @brief Initializes an ErtsFlxCtr. The macro
 * ERTS_FLXCTR_NR_OF_EXTRA_BYTES should be used to determine how much
 * extra space that needs to be allocated directly after the
 * ErtsFlxCtr when is_decentralized is set to zero.  Each ErtsFlxCtr
 * instance may contain up to ERTS_FLXCTR_ATOMICS_PER_CACHE_LINE
 * counters. These counters are numbered from zero to
 * (ERTS_FLXCTR_ATOMICS_PER_CACHE_LINE-1). Most of the functions in
 * this module take a parameter named counter_nr that controls which
 * of the ERTS_FLXCTR_ATOMICS_PER_CACHE_LINE counters in the given
 * ErtsFlxCtr that should be operated on.
 *
 * @param c The counter to initialize
 * @param is_decentralized Non-zero value to make c decentralized
 * @param nr_of_counters The number of counters included in c
 *                       (max ERTS_FLXCTR_ATOMICS_PER_CACHE_LINE)
 * @param alloc_type 
 */
void erts_flxctr_init(ErtsFlxCtr* c,
                      int is_decentralized,
                      Uint nr_of_counters,
                      ErtsAlcType_t alloc_type);

/**
 * @brief Destroys an initialized counter.
 *
 * @param c The counter that should be destroyed
 * @param alloc_type The allocation type (needs to be the same as the
 *                   one passed to erts_flxctr_init when c was
 *                   initialized)
 */
void erts_flxctr_destroy(ErtsFlxCtr* c, ErtsAlcType_t alloc_type);

/**
 * @brief Adds to_add to the counter with counter_nr in c
 *
 * @param c the ErtsFlxCtr to operate on
 * @param counter_nr The number of the counter in c to modify
 * @param to_add The amount that should be added to the specified counter
 */
ERTS_GLB_INLINE
void erts_flxctr_add(ErtsFlxCtr* c,
                     Uint counter_nr,
                     int to_add);

/**
 * @brief Increases the specified counter by 1
 *
 * @param c The ErtsFlxCtr instance to operate on
 * @param counter_nr The number of the counter within c to operate on
 */
ERTS_GLB_INLINE
void erts_flxctr_inc(ErtsFlxCtr* c,
                     Uint counter_nr);

/**
 * @brief Decreases the specified counter by 1
 */
ERTS_GLB_INLINE
void erts_flxctr_dec(ErtsFlxCtr* c,
                     Uint counter_nr);

/**
 * @brief This function tries to return the current value of the
 * specified counter but may return an incorrect result if the counter
 * is decentralized and other threads are accessing the counter
 * concurrently.
 *
 * @param c The ErtsFlxCtr instance to operate on
 * @param counter_nr The number of the counter within c to operate on
 *
 * @return A snapshot of the specifed counter if c is centralized or a
 *         possibly incorrect estimate of the counter value if c is
 *         decentralized
 */
Sint erts_flxctr_read_approx(ErtsFlxCtr* c,
                             Uint counter_nr);

/**
 * @brief This function can only be used together with an ErtsFlxCtr
 * that is configured to be centralized. The function increments the
 * specified counter by 1 and returns the value of the counter after
 * the increment.
 */
ERTS_GLB_INLINE
Sint erts_flxctr_inc_read_centralized(ErtsFlxCtr* c,
                                      Uint counter_nr);

/**
 * @brief This function can only be used together with a ErtsFlxCtr
 * that is configured to be centralized. The function decrements the
 * specified counter by 1 and returns the value of the counter after
 * the operation.
 */
ERTS_GLB_INLINE
Sint erts_flxctr_dec_read_centralized(ErtsFlxCtr* c,
                                      Uint counter_nr);

/**
 * @brief This function can only be used together with an ErtsFlxCtr
 * that is configured to be centralized. The function returns the
 * current value of the specified counter.
 */
ERTS_GLB_INLINE
Sint erts_flxctr_read_centralized(ErtsFlxCtr* c,
                                  Uint counter_nr);


typedef enum {
    ERTS_FLXCTR_TRY_AGAIN_AFTER_TRAP,
    ERTS_FLXCTR_DONE,
    ERTS_FLXCTR_GET_RESULT_AFTER_TRAP
} ErtsFlxctrSnapshotResultType;

typedef struct {
    ErtsFlxctrSnapshotResultType type;
    Eterm trap_resume_state;
    Sint result[ERTS_FLXCTR_ATOMICS_PER_CACHE_LINE];
} ErtsFlxCtrSnapshotResult;

/**
 * @brief This function initiates an atomic snapshot of an ErtsFlxCtr
 * to read out the values of one or more of the counters that are
 * stored in the given ErtsFlxCtr. The caller needs to perform
 * different actions after the return of this function depending on
 * the value of the type field in the returned struct:
 *
 * - The caller needs to trap and try again after the trap if the
 *   return value has the type ERTS_FLXCTR_TRY_AGAIN_AFTER_TRAP.
 *
 * - The caller can get the result directly from the result field of
 *   the returned struct if the return value has the type
 *   ERTS_FLXCTR_DONE. The value at index i in the result field
 *   correspond to counter number i.
 *
 * - Finally, if the return value has the type
 *   ERTS_FLXCTR_GET_RESULT_AFTER_TRAP, then the caller needs to save
 *   the value of the field trap_resume_state from the returned struct
 *   and trap. After the trap, the values of the counters can be
 *   obtained by using the function
 *   erts_flxctr_get_snapshot_result_after_trap. Note that the
 *   function erts_flxctr_is_snapshot_result can be used to check if a
 *   value is obtained from the trap_resume_state field in the
 *   returned struct (this can be useful when the calling function
 *   wakes up again after the trap).
 *
 * The snapshot operation that is initiated by this function should be
 * considered to be ongoing from the issuing of this function until a
 * struct with the type field set to ERTS_FLXCTR_DONE has been
 * returned from the function or until the caller of this function has
 * woken up after trapping.
 *
 * @param c The ErtsFlxCtr that the snapshot shall be taken from
 * @param alloc_type The allocation type (needs to be the same as the
 *                   type passed to erts_flxctr_init when c was
 *                   initialized)
 * @param p The Erlang process that is doing the call
 *
 * @return See the description above
 *
 */
ErtsFlxCtrSnapshotResult
erts_flxctr_snapshot(ErtsFlxCtr* c,
                     ErtsAlcType_t alloc_type,
                     Process* p);

/**
 * @brief Checks if the parameter term is a snapshot result (i.e.,
 * something obtained from the trap_resume_state field of an
 * ErtsFlxCtrSnapshotResult struct that has been returned from
 * erts_flxctr_snapshot).
 *
 * @param term The term to check 
 *
 * @return A nonzero value iff the term is a snapshot result
 */
int erts_flxctr_is_snapshot_result(Eterm term);

/**
 * @brief Returns the result of a snapshot for a counter given a
 * snapshot result returned by a call to erts_flxctr_snapshot (i.e.,
 * the value stored in the trap_resume_state field of a struct
 * returned by erts_flxctr_snapshot). The caller needs to trap between
 * the return of erts_flxctr_snapshot and the call to this function.
 */
Sint erts_flxctr_get_snapshot_result_after_trap(Eterm trap_resume_state,
                                                Uint counter_nr);

/**
 * @brief Resets the specified counter to 0. This function is unsafe
 * to call while a snapshot operation may be active (initiated with
 * the erts_flxctr_snapshot function).
 */
void erts_flxctr_reset(ErtsFlxCtr* c,
                       Uint counter_nr);

/**
 * @brief Checks if a snapshot operation is active (snapshots are
 * initiated with the erts_flxctr_snapshot function).
 *
 * @return nonzero value iff a snapshot was active at some point
 * between the invocation and return of the function
 */
int erts_flxctr_is_snapshot_ongoing(ErtsFlxCtr* c);

/**
 * @brief This function checks if a snapshot operation is ongoing
 * (snapshots are initiated with the erts_flxctr_snapshot function)
 * and suspend the given process until thread progress has happened if
 * it detected an ongoing snapshot operation. The caller needs to trap
 * if a non-zero value is returned.
 *
 * @param c The ErtsFlxCtr to check
 * @param p The calling process
 *
 * @return nonzero value if the given process has got suspended
 */
int erts_flxctr_suspend_until_thr_prg_if_snapshot_ongoing(ErtsFlxCtr* c, Process* p);

/* End: Public Interface */

/* Internal Declarations */

#define ERTS_FLXCTR_GET_CTR_ARRAY_PTR(C)                                \
    ((ErtsFlxCtrDecentralizedCtrArray*) erts_atomic_read_acqb(&(C)->u.counters_ptr))
#define ERTS_FLXCTR_GET_CTR_PTR(C, SCHEDULER_ID, COUNTER_ID)            \
    &(ERTS_FLXCTR_GET_CTR_ARRAY_PTR(C))->array[SCHEDULER_ID].counters[COUNTER_ID]


typedef union {
    erts_atomic_t counters[ERTS_FLXCTR_ATOMICS_PER_CACHE_LINE];
    char pad[ERTS_CACHE_LINE_SIZE];
} ErtsFlxCtrDecentralizedCtrArrayElem;

typedef struct ErtsFlxCtrDecentralizedCtrArray {
    void* block_start;
    erts_atomic_t snapshot_status;
    ErtsFlxCtrDecentralizedCtrArrayElem array[];
} ErtsFlxCtrDecentralizedCtrArray;

void erts_flxctr_set_slot(int group);

ERTS_GLB_INLINE
int erts_flxctr_get_slot_index(void);

/* End: Internal Declarations */


/* Implementation of inlined functions */

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

ERTS_GLB_INLINE
int erts_flxctr_get_slot_index(void)
{
    ErtsSchedulerData *esdp = erts_get_scheduler_data();
    ASSERT(esdp && !ERTS_SCHEDULER_IS_DIRTY(esdp));
    ASSERT(esdp->flxctr_slot_no > 0);
    return esdp->flxctr_slot_no;
}

ERTS_GLB_INLINE
void erts_flxctr_add(ErtsFlxCtr* c,
                     Uint counter_nr,
                     int to_add)
{
    ASSERT(counter_nr < c->nr_of_counters);
    if (c->is_decentralized) {
        erts_atomic_add_nob(ERTS_FLXCTR_GET_CTR_PTR(c,
                                                    erts_flxctr_get_slot_index(),
                                                    counter_nr),
                            to_add);
    } else {
        erts_atomic_add_nob(&c->u.counters[counter_nr], to_add);
    }
}

ERTS_GLB_INLINE
void erts_flxctr_inc(ErtsFlxCtr* c,
                     Uint counter_nr)
{
    ASSERT(counter_nr < c->nr_of_counters);
    if (c->is_decentralized) {
        erts_atomic_inc_nob(ERTS_FLXCTR_GET_CTR_PTR(c,
                                                    erts_flxctr_get_slot_index(),
                                                    counter_nr));
    } else {
        erts_atomic_inc_read_nob(&c->u.counters[counter_nr]);
    }
}

ERTS_GLB_INLINE
void erts_flxctr_dec(ErtsFlxCtr* c,
                     Uint counter_nr)
{
    ASSERT(counter_nr < c->nr_of_counters);
    if (c->is_decentralized) {
        erts_atomic_dec_nob(ERTS_FLXCTR_GET_CTR_PTR(c,
                                                    erts_flxctr_get_slot_index(),
                                                    counter_nr));
    } else {
        erts_atomic_dec_nob(&c->u.counters[counter_nr]);
    }
}

ERTS_GLB_INLINE
Sint erts_flxctr_inc_read_centralized(ErtsFlxCtr* c,
                                      Uint counter_nr)
{
    ASSERT(counter_nr < c->nr_of_counters);
    ASSERT(!c->is_decentralized);
    return erts_atomic_inc_read_nob(&c->u.counters[counter_nr]);
}

ERTS_GLB_INLINE
Sint erts_flxctr_dec_read_centralized(ErtsFlxCtr* c,
                                      Uint counter_nr)
{
    ASSERT(counter_nr < c->nr_of_counters);
    ASSERT(!c->is_decentralized);
    return erts_atomic_dec_read_nob(&c->u.counters[counter_nr]);
}

ERTS_GLB_INLINE
Sint erts_flxctr_read_centralized(ErtsFlxCtr* c,
                                  Uint counter_nr)
{
    ASSERT(counter_nr < c->nr_of_counters);
    ASSERT(!c->is_decentralized);   
    return erts_atomic_read_nob(&((erts_atomic_t*)(c->u.counters))[counter_nr]);
}

#endif /* #if ERTS_GLB_INLINE_INCL_FUNC_DEF */

#endif /* ERL_FLXCTR_H__ */
