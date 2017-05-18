/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2008-2017. All Rights Reserved.
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

#ifdef ERTS_ENABLE_LOCK_COUNT

#include "sys.h"

#include "erl_lock_count.h"
#include "erl_thr_progress.h"

#define LCNT_MAX_CARRIER_ENTRIES 255

/* - Exported global - */

Uint16 erts_lcnt_rt_options = ERTS_LCNT_OPT_PROCLOCK | ERTS_LCNT_OPT_LOCATION;

/* - Locals that are shared with the header implementation - */

int lcnt_initialization_completed__ = 0;

ethr_tsd_key lcnt_thr_data_key__;

const int lcnt_log2_tab64__[64] = {
    63,  0, 58,  1, 59, 47, 53,  2,
    60, 39, 48, 27, 54, 33, 42,  3,
    61, 51, 37, 40, 49, 18, 28, 20,
    55, 30, 34, 11, 43, 14, 22,  4,
    62, 57, 46, 52, 38, 26, 32, 41,
    50, 36, 17, 19, 29, 10, 13, 21,
    56, 45, 25, 31, 35, 16,  9, 12,
    44, 24, 15,  8, 23,  7,  6,  5};

/* - Local variables - */

static erts_lcnt_lock_info_list_t lcnt_current_lock_list;
static erts_lcnt_lock_info_list_t lcnt_deleted_lock_list;

static erts_lcnt_time_t lcnt_timer_start;

/* local functions */

static void lcnt_clear_stats(erts_lcnt_lock_info_t *info) {
    size_t i;

    for(i = 0; i < ERTS_LCNT_MAX_LOCK_LOCATIONS; i++) {
        erts_lcnt_lock_stats_t *stats = &info->location_stats[i];

        sys_memzero(&stats->wait_time_histogram, sizeof(stats->wait_time_histogram));

        stats->total_time_waited.s = 0;
        stats->total_time_waited.ns = 0;

        stats->times_waited = 0;

        stats->file = NULL;
        stats->line = 0;

        ethr_atomic_set(&stats->attempts, 0);
        ethr_atomic_set(&stats->collisions, 0);
    }

    info->location_count = 1;
}

static lcnt_thread_data_t__ *lcnt_thread_data_alloc(void) {
    lcnt_thread_data_t__ *eltd =
        (lcnt_thread_data_t__*)malloc(sizeof(lcnt_thread_data_t__));

    if(!eltd) {
        ERTS_INTERNAL_ERROR("Failed to allocate lcnt thread data.");
    }

    eltd->timer_set = 0;
    eltd->lock_in_conflict = 0;

    return eltd;
}

/* debug */

#if 0
static char* lock_opt(Uint16 flag) {
    if ((flag & ERTS_LCNT_LO_WRITE) && (flag & ERTS_LCNT_LO_READ)) return "rw";
    if  (flag & ERTS_LCNT_LO_READ )                                return "r ";
    if  (flag & ERTS_LCNT_LO_WRITE)                                return " w";
    return "--";
}

static void print_lock_x(erts_lcnt_lock_info_t *info, Uint16 flag, char *action) {
    ethr_sint_t w_state, r_state;
    char *type;

    if (strcmp(info->name, "run_queue") != 0) return;
    type = erts_lcnt_lock_type(info->flag);
    r_state = ethr_atomic_read(&info->r_state);
    w_state = ethr_atomic_read(&info->w_state);

    if (info->flag & flag) {
        erts_fprintf(stderr,"%10s [%24s] [r/w state %4ld/%4ld] %2s id %T\r\n",
                action,
                info->name,
                r_state,
                w_state,
                type,
                info->id);
    }
}
#endif

/* - List operations -
 *
 * Info entries are kept in a doubly linked list where each entry is locked
 * with its neighbors rather than a global lock. Deletion is rather quick, but
 * insertion is still serial since the head becomes a de facto global lock.
 *
 * We rely on ad-hoc spinlocks to avoid "recursing" into this module. */

#define LCNT_SPINLOCK_YIELD_ITERATIONS 50

#define LCNT_SPINLOCK_HELPER_INIT \
    Uint failed_spin_count = 0;

#define LCNT_SPINLOCK_HELPER_YIELD \
    do { \
        failed_spin_count++; \
        if(!(failed_spin_count % LCNT_SPINLOCK_YIELD_ITERATIONS)) { \
            erts_thr_yield(); \
        } else { \
            ERTS_SPIN_BODY; \
        } \
    } while(0)

static void lcnt_unlock_list_entry(erts_lcnt_lock_info_t *info) {
    ethr_atomic32_set_relb(&info->lock, 0);
}

static int lcnt_try_lock_list_entry(erts_lcnt_lock_info_t *info) {
    return ethr_atomic32_cmpxchg_acqb(&info->lock, 1, 0) == 0;
}

static void lcnt_lock_list_entry(erts_lcnt_lock_info_t *info) {
    LCNT_SPINLOCK_HELPER_INIT;

    while(!lcnt_try_lock_list_entry(info)) {
        LCNT_SPINLOCK_HELPER_YIELD;
    }
}

static void lcnt_lock_list_entry_with_neighbors(erts_lcnt_lock_info_t *info) {
    LCNT_SPINLOCK_HELPER_INIT;

    for(;;) {
        if(!lcnt_try_lock_list_entry(info))
            goto retry_after_entry_failed;
        if(!lcnt_try_lock_list_entry(info->next))
            goto retry_after_next_failed;
        if(!lcnt_try_lock_list_entry(info->prev))
            goto retry_after_prev_failed;

        return;

    retry_after_prev_failed:
        lcnt_unlock_list_entry(info->next);
    retry_after_next_failed:
        lcnt_unlock_list_entry(info);
    retry_after_entry_failed:
        LCNT_SPINLOCK_HELPER_YIELD;
    }
}

static void lcnt_unlock_list_entry_with_neighbors(erts_lcnt_lock_info_t *info) {
    lcnt_unlock_list_entry(info->prev);
    lcnt_unlock_list_entry(info->next);
    lcnt_unlock_list_entry(info);
}

static void lcnt_insert_list_entry(erts_lcnt_lock_info_list_t *list, erts_lcnt_lock_info_t *info) {
    erts_lcnt_lock_info_t *next, *prev;

    prev = &list->head;

    lcnt_lock_list_entry(prev);

    next = prev->next;

    lcnt_lock_list_entry(next);

    info->next = next;
    info->prev = prev;

    prev->next = info;
    next->prev = info;

    lcnt_unlock_list_entry(next);
    lcnt_unlock_list_entry(prev);
}

static void lcnt_insert_list_carrier(erts_lcnt_lock_info_list_t *list,
                                     erts_lcnt_lock_info_carrier_t *carrier) {
    erts_lcnt_lock_info_t *next, *prev;
    size_t i;

    for(i = 0; i < carrier->entry_count; i++) {
        erts_lcnt_lock_info_t *info = &carrier->entries[i];

        info->prev = &carrier->entries[i - 1];
        info->next = &carrier->entries[i + 1];
    }

    prev = &list->head;

    lcnt_lock_list_entry(prev);

    next = prev->next;

    lcnt_lock_list_entry(next);

    next->prev = &carrier->entries[carrier->entry_count - 1];
    carrier->entries[carrier->entry_count - 1].next = next;

    prev->next = &carrier->entries[0];
    carrier->entries[0].prev = prev;

    lcnt_unlock_list_entry(next);
    lcnt_unlock_list_entry(prev);
}

static void lcnt_init_list(erts_lcnt_lock_info_list_t *list) {
    /* Ensure that ref_count operations explode when touching the sentinels in
     * DEBUG mode. */
    ethr_atomic_init(&(list->head.ref_count), -1);
    ethr_atomic_init(&(list->tail.ref_count), -1);

    ethr_atomic32_init(&(list->head.lock), 0);
    (list->head).next = &list->tail;
    (list->head).prev = &list->tail;

    ethr_atomic32_init(&(list->tail.lock), 0);
    (list->tail).next = &list->head;
    (list->tail).prev = &list->head;
}

/* - Carrier operations - */

int lcnt_thr_progress_unmanaged_delay__(void) {
    return erts_thr_progress_unmanaged_delay();
}

void lcnt_thr_progress_unmanaged_continue__(int handle) {
    return erts_thr_progress_unmanaged_continue(handle);
}

static void lcnt_deallocate_carrier_malloc(erts_lcnt_lock_info_carrier_t *carrier) {
    ASSERT(ethr_atomic_read(&carrier->ref_count) == 0);
    free(carrier);
}

static void lcnt_deallocate_carrier_erts(erts_lcnt_lock_info_carrier_t *carrier) {
    ASSERT(ethr_atomic_read(&carrier->ref_count) == 0);
    erts_free(ERTS_ALC_T_LCNT_CARRIER, (void*)carrier);
}

static void lcnt_thr_prg_cleanup_carrier(void *data) {
    erts_lcnt_lock_info_carrier_t *carrier = data;
    size_t entry_count, i;

    /* carrier->entry_count will be replaced with garbage if it's deallocated
     * on the final iteration, so we'll tuck it away to get a clean exit. */
    entry_count = carrier->entry_count;

    for(i = 0; i < entry_count; i++) {
        ASSERT(ethr_atomic_read(&carrier->ref_count) >= (entry_count - i));

        erts_lcnt_release_lock_info(&carrier->entries[i]);
    }
}

static void lcnt_schedule_carrier_cleanup(void *data) {
    ErtsSchedulerData *esdp = erts_get_scheduler_data();

    /* We can't issue cleanup jobs on anything other than normal schedulers, so
     * we move to the first scheduler if required. */

    if(!esdp || esdp->type != ERTS_SCHED_NORMAL) {
        erts_schedule_misc_aux_work(1, &lcnt_schedule_carrier_cleanup, data);
    } else {
        erts_lcnt_lock_info_carrier_t *carrier = data;
        size_t carrier_size;

        carrier_size = sizeof(erts_lcnt_lock_info_carrier_t) +
                       sizeof(erts_lcnt_lock_info_t) * carrier->entry_count;

        erts_schedule_thr_prgr_later_cleanup_op(&lcnt_thr_prg_cleanup_carrier,
            data, (ErtsThrPrgrLaterOp*)&carrier->release_entries, carrier_size);
    }
}

static void lcnt_info_deallocate(erts_lcnt_lock_info_t *info) {
    lcnt_release_carrier__(info->carrier);
}

static void lcnt_info_dispose(erts_lcnt_lock_info_t *info) {
    ASSERT(ethr_atomic_read(&info->ref_count) == 0);

    if(erts_lcnt_rt_options & ERTS_LCNT_OPT_COPYSAVE) {
        ethr_atomic_set(&info->ref_count, 1);

        /* Move straight to deallocation the next time around. */
        info->dispose = &lcnt_info_deallocate;

        lcnt_insert_list_entry(&lcnt_deleted_lock_list, info);
    } else {
        lcnt_info_deallocate(info);
    }
}

static void lcnt_lock_info_init_helper(erts_lcnt_lock_info_t *info) {
#ifdef DEBUG
    ethr_atomic_init(&info->flowstate, 0);
#endif

    ethr_atomic_init(&info->ref_count, 1);
    ethr_atomic32_init(&info->lock, 0);

    ethr_atomic_init(&info->r_state, 0);
    ethr_atomic_init(&info->w_state, 0);

    info->dispose = &lcnt_info_dispose;

    lcnt_clear_stats(info);
}

erts_lcnt_lock_info_carrier_t *erts_lcnt_create_lock_info_carrier(int entry_count) {
    erts_lcnt_lock_info_carrier_t *result;
    size_t carrier_size, i;

    ASSERT(entry_count > 0 && entry_count <= LCNT_MAX_CARRIER_ENTRIES);

    carrier_size = sizeof(erts_lcnt_lock_info_carrier_t) +
                   sizeof(erts_lcnt_lock_info_t) * entry_count;

    if(lcnt_initialization_completed__) {
        result = (erts_lcnt_lock_info_carrier_t*)erts_alloc(ERTS_ALC_T_LCNT_CARRIER, carrier_size);
        result->deallocate = &lcnt_deallocate_carrier_erts;
    } else {
        result = (erts_lcnt_lock_info_carrier_t*)malloc(carrier_size);
        result->deallocate = &lcnt_deallocate_carrier_malloc;
    }

    ethr_atomic_init(&result->ref_count, entry_count);

    result->entry_count = entry_count;

    for(i = 0; i < entry_count; i++) {
        erts_lcnt_lock_info_t *info = &result->entries[i];

        lcnt_lock_info_init_helper(info);

        info->carrier = result;
    }

    return result;
}

void erts_lcnt_install(erts_lcnt_ref_t *ref, erts_lcnt_lock_info_carrier_t *carrier) {
    ethr_sint_t swapped_carrier;

    swapped_carrier = ethr_atomic_cmpxchg_mb(ref, (ethr_sint_t)carrier, (ethr_sint_t)NULL);

    if(swapped_carrier != (ethr_sint_t)NULL) {
#ifdef DEBUG
        ASSERT(ethr_atomic_read(&carrier->ref_count) == carrier->entry_count);
        ethr_atomic_set(&carrier->ref_count, 0);
#endif

        carrier->deallocate(carrier);
    } else {
        lcnt_insert_list_carrier(&lcnt_current_lock_list, carrier);
    }
}

void erts_lcnt_uninstall(erts_lcnt_ref_t *ref) {
    ethr_sint_t previous_carrier, swapped_carrier;

    previous_carrier = ethr_atomic_read(ref);
    swapped_carrier = ethr_atomic_cmpxchg_mb(ref, (ethr_sint_t)NULL, previous_carrier);

    if(previous_carrier && previous_carrier == swapped_carrier) {
        lcnt_schedule_carrier_cleanup((void*)previous_carrier);
    }
}

/* - Initialization - */

void erts_lcnt_pre_thr_init() {
    /* Ensure that the dependency hack mentioned in the header doesn't
     * explode at runtime. */
    ERTS_CT_ASSERT(sizeof(LcntThrPrgrLaterOp) >= sizeof(ErtsThrPrgrLaterOp));
    ERTS_CT_ASSERT(ERTS_THR_PRGR_DHANDLE_MANAGED == 
        (ErtsThrPrgrDelayHandle)LCNT_THR_PRGR_DHANDLE_MANAGED);

    lcnt_init_list(&lcnt_current_lock_list);
    lcnt_init_list(&lcnt_deleted_lock_list);
}

void erts_lcnt_post_thr_init() {
    /* ASSUMPTION: this is safe since it runs prior to the creation of other
     * threads (Directly after ethread init). */

    ethr_tsd_key_create(&lcnt_thr_data_key__, "lcnt_data");

    erts_lcnt_thread_setup();
}

void erts_lcnt_late_init() {
    /* Set start timer and zero all statistics */
    erts_lcnt_clear_counters();
    erts_thr_install_exit_handler(erts_lcnt_thread_exit_handler);

    /* It's safe to use erts_alloc and thread progress past this point. */
    lcnt_initialization_completed__ = 1;
}

void erts_lcnt_thread_setup() {
    lcnt_thread_data_t__ *eltd = lcnt_thread_data_alloc();

    ASSERT(eltd);

    ethr_tsd_set(lcnt_thr_data_key__, eltd);
}

void erts_lcnt_thread_exit_handler() {
    lcnt_thread_data_t__ *eltd = lcnt_get_thread_data__();

    if (eltd) {
        free(eltd);
    }
}

/* - BIF interface - */

void erts_lcnt_retain_lock_info(erts_lcnt_lock_info_t *info) {
#ifdef DEBUG
    ASSERT(ethr_atomic_inc_read_acqb(&info->ref_count) >= 2);
#else
    ethr_atomic_inc_acqb(&info->ref_count);
#endif
}

void erts_lcnt_release_lock_info(erts_lcnt_lock_info_t *info) {
    ethr_sint_t count;

    /* We need to acquire the lock before decrementing ref_count to avoid
     * racing with list iteration; there's a short window between reading the
     * reference to info and increasing its ref_count. */
    lcnt_lock_list_entry_with_neighbors(info);

    count = ethr_atomic_dec_read(&info->ref_count);

    ASSERT(count >= 0);

    if(count > 0) {
        lcnt_unlock_list_entry_with_neighbors(info);
    } else {
        (info->next)->prev = info->prev;
        (info->prev)->next = info->next;

        lcnt_unlock_list_entry_with_neighbors(info);

        info->dispose(info);
    }
}

Uint16 erts_lcnt_set_rt_opt(Uint16 opt) {
    Uint16 prev;
    prev = (erts_lcnt_rt_options & opt);
    erts_lcnt_rt_options |= opt;
    return prev;
}

Uint16 erts_lcnt_clear_rt_opt(Uint16 opt) {
    Uint16 prev;
    prev = (erts_lcnt_rt_options & opt);
    erts_lcnt_rt_options &= ~opt;
    return prev;
}

void erts_lcnt_clear_counters(void) {
    erts_lcnt_lock_info_t *iterator;

    lcnt_time__(&lcnt_timer_start);

    iterator = NULL;
    while(erts_lcnt_iterate_list(&lcnt_current_lock_list, &iterator)) {
        lcnt_clear_stats(iterator);
    }

    iterator = NULL;
    while(erts_lcnt_iterate_list(&lcnt_deleted_lock_list, &iterator)) {
        erts_lcnt_release_lock_info(iterator);
    }
}

erts_lcnt_data_t erts_lcnt_get_data(void) {
    erts_lcnt_time_t timer_stop;
    erts_lcnt_data_t result;

    lcnt_time__(&timer_stop);

    result.timer_start = lcnt_timer_start;

    result.current_locks = &lcnt_current_lock_list;
    result.deleted_locks = &lcnt_deleted_lock_list;

    lcnt_time_diff__(&result.duration, &timer_stop, &result.timer_start);

    return result;
}

const char *erts_lcnt_lock_type(Uint16 type) {
    switch(type & ERTS_LCNT_LT_ALL) {
        case ERTS_LCNT_LT_SPINLOCK:   return "spinlock";
        case ERTS_LCNT_LT_RWSPINLOCK: return "rw_spinlock";
        case ERTS_LCNT_LT_MUTEX:      return "mutex";
        case ERTS_LCNT_LT_RWMUTEX:    return "rw_mutex";
        case ERTS_LCNT_LT_PROCLOCK:   return "proclock";
        default:                      return "";
    }
}

int erts_lcnt_iterate_list(erts_lcnt_lock_info_list_t *list, erts_lcnt_lock_info_t **iterator) {
    erts_lcnt_lock_info_t *current, *next;

    current = *iterator ? *iterator : &list->head;

    ASSERT(current != &list->tail);

    lcnt_lock_list_entry(current);

    next = current->next;

    if(next != &list->tail) {
        erts_lcnt_retain_lock_info(next);
    }

    lcnt_unlock_list_entry(current);

    if(current != &list->head) {
        erts_lcnt_release_lock_info(current);
    }

    *iterator = next;

    return next != &list->tail;
}

#endif /* #ifdef ERTS_ENABLE_LOCK_COUNT */
