/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2012-2023. All Rights Reserved.
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

#include "code_ix.h"
#include "global.h"
#include "beam_catches.h"



#if 0
# define CIX_TRACE(text) erts_fprintf(stderr, "CIX_TRACE: " text " act=%u load=%u\r\n", erts_active_code_ix(), erts_staging_code_ix())
#else
# define CIX_TRACE(text)
#endif

#if defined(BEAMASM) && defined(ERTS_THR_INSTRUCTION_BARRIER)
#    define CODE_IX_ISSUE_INSTRUCTION_BARRIERS
#endif

/* If we need to issue a code barrier when thread progress is blocked, we use
 * this counter to signal all managed threads to execute an instruction barrier
 * when thread progress is unblocked. */
erts_atomic32_t outstanding_blocking_code_barriers;

erts_atomic32_t the_active_code_index;
erts_atomic32_t the_staging_code_index;

static int code_writing_seized = 0;
static Process* code_writing_process = NULL;
struct code_write_queue_item {
    Process *p;
    void (*aux_func)(void *);
    void *aux_arg;
    struct code_write_queue_item* next;
};
static struct code_write_queue_item* code_write_queue = NULL;
static erts_mtx_t code_write_permission_mtx;

#ifdef ERTS_ENABLE_LOCK_CHECK
static erts_tsd_key_t has_code_write_permission;
#endif

#ifdef DEBUG
static erts_tsd_key_t needs_code_barrier;
#endif

void erts_code_ix_init(void)
{
    /* We start emulator by initializing preloaded modules
     * single threaded with active and staging set both to zero.
     * Preloading is finished by a commit that will set things straight.
     */
    erts_atomic32_init_nob(&outstanding_blocking_code_barriers, 0);
    erts_atomic32_init_nob(&the_active_code_index, 0);
    erts_atomic32_init_nob(&the_staging_code_index, 0);
    erts_mtx_init(&code_write_permission_mtx, "code_write_permission", NIL,
        ERTS_LOCK_FLAGS_PROPERTY_STATIC | ERTS_LOCK_FLAGS_CATEGORY_GENERIC);
#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_tsd_key_create(&has_code_write_permission,
                        "erts_has_code_write_permission");
#endif
#ifdef DEBUG
    erts_tsd_key_create(&needs_code_barrier,
                        "erts_needs_code_barrier");
#endif
    CIX_TRACE("init");
}

void erts_start_staging_code_ix(int num_new)
{
    beam_catches_start_staging();
    export_start_staging();
    module_start_staging();
    erts_start_staging_ranges(num_new);
    CIX_TRACE("start");
}


void erts_end_staging_code_ix(void)
{
    beam_catches_end_staging(1);
    export_end_staging(1);
    module_end_staging(1);
    erts_end_staging_ranges(1);
    CIX_TRACE("end");
}

void erts_commit_staging_code_ix(void)
{
    ErtsCodeIndex ix;
    /* We need to this lock as we are now making the staging export table active */
    export_staging_lock();
    ix = erts_staging_code_ix();
    erts_atomic32_set_nob(&the_active_code_index, ix);
    ix = (ix + 1) % ERTS_NUM_CODE_IX;
    erts_atomic32_set_nob(&the_staging_code_index, ix);
    export_staging_unlock();
    erts_tracer_nif_clear();
    CIX_TRACE("activate");
}

void erts_abort_staging_code_ix(void)
{
    beam_catches_end_staging(0);
    export_end_staging(0);
    module_end_staging(0);
    erts_end_staging_ranges(0);
    CIX_TRACE("abort");
}

static int try_seize_cwp(Process* c_p,
                         void (*aux_func)(void *),
                         void *aux_arg);

#if defined(DEBUG) || defined(ADDRESS_SANITIZER)
#    define CWP_DBG_FORCE_TRAP
#endif

/*
 * Calller _must_ yield if we return 0
 */
int erts_try_seize_code_write_permission(Process* c_p)
{
    ASSERT(c_p != NULL);

#ifdef CWP_DBG_FORCE_TRAP
    if (!(c_p->flags & F_DBG_FORCED_TRAP)) {
        c_p->flags |= F_DBG_FORCED_TRAP;
        return 0;
    } else {
        /* back from forced trap */
        c_p->flags &= ~F_DBG_FORCED_TRAP;
    }
#endif

    return try_seize_cwp(c_p, NULL, NULL);
}

int erts_try_seize_code_write_permission_aux(void (*aux_func)(void *),
                                             void *aux_arg)
{
    ASSERT(aux_func != NULL);
    return try_seize_cwp(NULL, aux_func, aux_arg);
}

static int try_seize_cwp(Process* c_p,
                         void (*aux_func)(void *),
                         void *aux_arg)
{
    int success;
    ASSERT(!erts_thr_progress_is_blocking()); /* to avoid deadlock */

    erts_mtx_lock(&code_write_permission_mtx);
    success = !code_writing_seized;
    if (success) {
        code_writing_seized = 1;
	code_writing_process = c_p;
#ifdef ERTS_ENABLE_LOCK_CHECK
	erts_tsd_set(has_code_write_permission, (void *) 1);
#endif
    }
    else { /* Already locked */
	struct code_write_queue_item* qitem;
        qitem = erts_alloc(ERTS_ALC_T_CODE_IX_LOCK_Q, sizeof(*qitem));
        if (c_p) {
            ASSERT(code_writing_process != c_p);
            qitem->p = c_p;
            qitem->aux_func = NULL;
            qitem->aux_arg = NULL;
            erts_proc_inc_refc(c_p);
            erts_suspend(c_p, ERTS_PROC_LOCK_MAIN, NULL);
        }
        else {
            qitem->p = NULL;
            qitem->aux_func = aux_func;
            qitem->aux_arg = aux_arg;
        }
        qitem->next = code_write_queue;
        code_write_queue = qitem;
    }
   erts_mtx_unlock(&code_write_permission_mtx);
   return success;
}

void erts_release_code_write_permission(void)
{
    erts_mtx_lock(&code_write_permission_mtx);
    ERTS_LC_ASSERT(erts_has_code_write_permission());
    while (code_write_queue != NULL) { /* unleash the entire herd */
	struct code_write_queue_item* qitem = code_write_queue;
        if (qitem->p) {
            erts_proc_lock(qitem->p, ERTS_PROC_LOCK_STATUS);
            if (!ERTS_PROC_IS_EXITING(qitem->p)) {
                erts_resume(qitem->p, ERTS_PROC_LOCK_STATUS);
            }
            erts_proc_unlock(qitem->p, ERTS_PROC_LOCK_STATUS);
            erts_proc_dec_refc(qitem->p);
        }
        else { /* aux work*/
            ErtsSchedulerData *esdp = erts_get_scheduler_data();
            ASSERT(esdp && esdp->type == ERTS_SCHED_NORMAL);
            erts_schedule_misc_aux_work((int) esdp->no,
                                        qitem->aux_func,
                                        qitem->aux_arg);
        }
        code_write_queue = qitem->next;
	erts_free(ERTS_ALC_T_CODE_IX_LOCK_Q, qitem);
    }
    code_writing_seized = 0;
    code_writing_process = NULL;
#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_tsd_set(has_code_write_permission, (void *) 0);
#endif
    erts_mtx_unlock(&code_write_permission_mtx);
}

#ifdef ERTS_ENABLE_LOCK_CHECK
int erts_has_code_write_permission(void)
{
    return code_writing_seized && erts_tsd_get(has_code_write_permission);
}
#endif

#ifdef DEBUG
void erts_debug_require_code_barrier(void) {
    erts_tsd_set(needs_code_barrier, (void*)(1));
}

void erts_debug_check_code_barrier(void) {
    ASSERT(erts_tsd_get(needs_code_barrier) == (void*)0);
}

static void erts_debug_unrequire_code_barrier(void) {
    erts_tsd_set(needs_code_barrier, (void*)(0));
}
#endif

static void schedule_code_barrier_later_op(void *barrier_) {
    ErtsCodeBarrier *barrier = (ErtsCodeBarrier*)barrier_;

    if (barrier->size == 0) {
        erts_schedule_thr_prgr_later_op(barrier->later_function,
                                        barrier->later_data,
                                        &barrier->later_op);
    } else {
        erts_schedule_thr_prgr_later_cleanup_op(barrier->later_function,
                                                barrier->later_data,
                                                &barrier->later_op,
                                                barrier->size);
    }
}

#ifdef CODE_IX_ISSUE_INSTRUCTION_BARRIERS
static void issue_instruction_barrier(void *barrier_) {
    ErtsCodeBarrier *barrier = (ErtsCodeBarrier*)barrier_;

    ERTS_THR_INSTRUCTION_BARRIER;

    if (erts_refc_dectest(&barrier->pending_schedulers, 0) == 0) {
#    ifdef ERTS_ENABLE_LOCK_CHECK
        ErtsSchedulerData *initial_esdp = (ErtsSchedulerData*)barrier->esdp;

        /* HACK: Dodges a broken lock-checking assertion, which requires that
         * the _thread_ that takes a code permission is also the one that
         * releases it.
         *
         * This has never held since processes can migrate between schedulers,
         * but we have to roll with the punches. Commit the code on the
         * scheduler that called `erts_schedule_code_barrier` in the hopes that
         * it's the right one.
         *
         * This has been fixed in `master`. */
        if (initial_esdp && initial_esdp != erts_get_scheduler_data()) {
            erts_schedule_misc_aux_work(initial_esdp->no,
                                        schedule_code_barrier_later_op,
                                        barrier);
        } else {
            schedule_code_barrier_later_op(barrier);
        }
#    else
        schedule_code_barrier_later_op(barrier);
#    endif
    }
}
#endif

void erts_schedule_code_barrier(ErtsCodeBarrier *barrier,
                                void (*later_function)(void *),
                                void *later_data) {
    erts_schedule_code_barrier_cleanup(barrier, later_function, later_data, 0);
}

void erts_schedule_code_barrier_cleanup(ErtsCodeBarrier *barrier,
                                        void (*later_function)(void *),
                                        void *later_data,
                                        UWord size)
{
#ifdef DEBUG
    erts_debug_unrequire_code_barrier();
#endif

    barrier->esdp = erts_get_scheduler_data();
    barrier->later_function = later_function;
    barrier->later_data = later_data;
    barrier->size = size;

#ifdef CODE_IX_ISSUE_INSTRUCTION_BARRIERS
    /* Issue instruction barriers on all normal schedulers, ensuring that they
     * won't execute old code.
     *
     * The last scheduler to run the barrier gets the honor of scheduling a
     * thread progress op to run the `later_function`. */
    erts_refc_init(&barrier->pending_schedulers,
                   (erts_aint_t)erts_no_schedulers);
    erts_schedule_multi_misc_aux_work(1, 1, erts_no_schedulers,
                                      issue_instruction_barrier,
                                      barrier);
    issue_instruction_barrier(barrier);
#else
    schedule_code_barrier_later_op(barrier);
#endif
}

#ifdef CODE_IX_ISSUE_INSTRUCTION_BARRIERS
static ErtsThrPrgrLaterOp global_code_barrier_lop;

static void decrement_blocking_code_barriers(void *ignored) {
    (void)ignored;
    erts_atomic32_dec_nob(&outstanding_blocking_code_barriers);
}

static void schedule_blocking_code_barriers(void *ignored) {
    ERTS_THR_INSTRUCTION_BARRIER;

    /* Tell all managed threads to execute an instruction barrier as soon as we
     * unblock thread progress, and schedule a thread progress job to clear the
     * counter.
     *
     * Note that we increment and decrement instead of setting and clearing
     * since we might execute several blocking barriers in the same tick. */
    erts_atomic32_inc_nob(&outstanding_blocking_code_barriers);
    erts_schedule_thr_prgr_later_op(decrement_blocking_code_barriers,
                                    NULL,
                                    &global_code_barrier_lop);
}
#endif

void erts_blocking_code_barrier()
{
#ifdef DEBUG
    erts_debug_unrequire_code_barrier();
#endif

    ERTS_LC_ASSERT(erts_thr_progress_is_blocking());

#ifdef CODE_IX_ISSUE_INSTRUCTION_BARRIERS
    schedule_blocking_code_barriers(NULL);
#endif
}

void erts_code_ix_finalize_wait() {
#ifdef CODE_IX_ISSUE_INSTRUCTION_BARRIERS
    if (erts_atomic32_read_nob(&outstanding_blocking_code_barriers) != 0) {
        ERTS_THR_INSTRUCTION_BARRIER;
    }
#endif
}
