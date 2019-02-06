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

/**
 * @description Statistics for locks.
 * @file erl_lock_count.h
 *
 * @author Björn-Egil Dahlberg
 * @author John Högberg
 *
 * Conceptual representation:
 *
 *     - set name
 *     | - id (the unique lock)
 *     | | - lock type
 *     | | - statistics
 *     | | | - location (file and line number)
 *     | | | - attempts
 *     | | | - collisions (including trylock busy)
 *     | | | - timer (time spent in waiting for lock)
 *     | | | - n_timer (collisions excluding trylock busy)
 *     | | | - histogram
 *     | | | | - # 0 = log2(lock wait_time ns)
 *     | | | | - ...
 *     | | | | - # n = log2(lock wait_time ns)
 *
 * Each instance of a lock is the unique lock, i.e. set and id in that set.
 * For each lock there is a set of statistics with where and what impact
 * the lock acquisition had.
 */

#ifndef ERTS_LOCK_COUNT_H__
#define ERTS_LOCK_COUNT_H__

#ifdef  ERTS_ENABLE_LOCK_COUNT
#ifndef ERTS_ENABLE_LOCK_POSITION
/** @brief Controls whether _x variants of mtx functions are used. */
#define ERTS_ENABLE_LOCK_POSITION 1
#endif

#include "sys.h"
#include "ethread.h"

#include "erl_term.h"
#include "erl_lock_flags.h"

#define ERTS_LCNT_MAX_LOCK_LOCATIONS  (5)

#define ERTS_LCNT_HISTOGRAM_MAX_NS    (((unsigned long)1LL << 28) - 1)
#if 0 || defined(ERTS_HAVE_OS_MONOTONIC_TIME_SUPPORT)
#define ERTS_LCNT_HISTOGRAM_SLOT_SIZE (30)
#define ERTS_LCNT_HISTOGRAM_RSHIFT    (0)
#else
#define ERTS_LCNT_HISTOGRAM_SLOT_SIZE (20)
#define ERTS_LCNT_HISTOGRAM_RSHIFT    (10)
#endif

typedef struct {
    unsigned long s;
    unsigned long ns;
} erts_lcnt_time_t;

typedef struct {
    /* @brief log2 array of nano seconds occurences */
    Uint32 ns[ERTS_LCNT_HISTOGRAM_SLOT_SIZE];
} erts_lcnt_hist_t;

typedef struct {
    /** @brief In which file the lock was taken. May be NULL. */
    const char *file;
    /** @brief Line number in \c file */
    unsigned int line;

    /* "attempts" and "collisions" need to be atomic since try_lock busy does
     * not acquire a lock and there is no post action to rectify the
     * situation. */

    ethr_atomic_t attempts;
    ethr_atomic_t collisions;

    erts_lcnt_time_t total_time_waited;
    Uint64 times_waited;

    erts_lcnt_hist_t wait_time_histogram;
} erts_lcnt_lock_stats_t;

typedef struct lcnt_lock_info_t_ {
    erts_lock_flags_t flags;
    const char *name;
    /** @brief Id if possible, must be an immediate */
    Eterm id;

    /* The first entry is reserved as a fallback for when location information
     * is missing, and when the lock is used in more than (MAX_LOCK_LOCATIONS
     * - 1) different places. */
    erts_lcnt_lock_stats_t location_stats[ERTS_LCNT_MAX_LOCK_LOCATIONS];
    unsigned int location_count;

    /* -- Everything below is internal to this module ---------------------- */

    /* Lock states; rw locks uses both states, other locks only uses w_state */

    /** @brief Write state. 0 = not taken, otherwise n threads waiting */
    ethr_atomic_t w_state;
    /** @brief Read state. 0 = not taken, > 0 -> writes will wait */
    ethr_atomic_t r_state;

    struct lcnt_lock_info_t_ *prev;
    struct lcnt_lock_info_t_ *next;

    /** @brief Used in place of erts_refc_t to avoid a circular dependency. */
    ethr_atomic_t ref_count;
    ethr_atomic32_t lock;

    /** @brief Deletion hook called once \c ref_count reaches 0; may defer
     * deletion by modifying \c ref_count. */
    void (*dispose)(struct lcnt_lock_info_t_ *);

    struct lcnt_lock_info_carrier_ *carrier;
} erts_lcnt_lock_info_t;

typedef struct lcnt_lock_info_list_ {
    erts_lcnt_lock_info_t head;
    erts_lcnt_lock_info_t tail;
} erts_lcnt_lock_info_list_t;

typedef struct {
    erts_lcnt_time_t timer_start; /**< Time of last clear */
    erts_lcnt_time_t duration; /**< Time since last clear */

    erts_lcnt_lock_info_list_t *current_locks;
    erts_lcnt_lock_info_list_t *deleted_locks;
} erts_lcnt_data_t;

typedef struct lcnt_lock_info_carrier_ erts_lcnt_lock_info_carrier_t;

typedef ethr_atomic_t erts_lcnt_ref_t;

/* -- Globals -------------------------------------------------------------- */

/** @brief Checks whether counting is enabled for any of the given
 * categories. */
#define erts_lcnt_check_enabled(flags) \
    (lcnt_category_mask__ & flags)

/* -- Lock operations ------------------------------------------------------
 *
 * All of these will nop if there's nothing "installed" on the given reference,
 * in order to transparently support enable/disable at runtime. */

/** @brief Records that a lock is being acquired. */
ERTS_GLB_FORCE_INLINE
void erts_lcnt_lock(erts_lcnt_ref_t *ref);

/** @copydoc erts_lcnt_lock
 * @param option Notes whether the lock is a read or write lock. */
ERTS_GLB_FORCE_INLINE
void erts_lcnt_lock_opt(erts_lcnt_ref_t *ref, erts_lock_options_t option);

/** @brief Records that a lock has been acquired. */
ERTS_GLB_FORCE_INLINE
void erts_lcnt_lock_post(erts_lcnt_ref_t *ref);

/** @copydoc erts_lcnt_lock_post.
 * @param file The name of the file where the lock was acquired.
 * @param line The line at which the lock was acquired. */
ERTS_GLB_FORCE_INLINE
void erts_lcnt_lock_post_x(erts_lcnt_ref_t *ref, char *file, unsigned int line);

/** @brief Records that a lock has been released. */
ERTS_GLB_FORCE_INLINE
void erts_lcnt_unlock(erts_lcnt_ref_t *ref);

/** @copydoc erts_lcnt_unlock_opt
 * @param option Whether the lock is a read or write lock. */
ERTS_GLB_FORCE_INLINE
void erts_lcnt_unlock_opt(erts_lcnt_ref_t *ref, erts_lock_options_t option);

/** @brief Rectifies the case where a lock wasn't actually a lock operation.
 *
 * Only used for process locks at the moment. */
ERTS_GLB_FORCE_INLINE
void erts_lcnt_lock_unacquire(erts_lcnt_ref_t *ref);

/** @brief Records the result of a trylock, placing the queried lock status in
 * \c result. */
ERTS_GLB_FORCE_INLINE
void erts_lcnt_trylock(erts_lcnt_ref_t *ref, int result);

/** @copydoc erts_lcnt_trylock
 * @param option Whether the lock is a read or write lock. */
ERTS_GLB_FORCE_INLINE
void erts_lcnt_trylock_opt(erts_lcnt_ref_t *ref, int result, erts_lock_options_t option);

/* Indexed variants of the standard lock operations, for use when a single
 * reference contains many counters (eg. process locks).
 *
 * erts_lcnt_open_ref must be used to safely extract the installed carrier,
 * which must released with erts_lcnt_close_reference on success.
 *
 * Refer to \c erts_lcnt_lock for example usage. */

ERTS_GLB_INLINE
void erts_lcnt_lock_idx(erts_lcnt_lock_info_carrier_t *carrier, int index);
ERTS_GLB_INLINE
void erts_lcnt_lock_opt_idx(erts_lcnt_lock_info_carrier_t *carrier, int index, erts_lock_options_t option);

ERTS_GLB_INLINE
void erts_lcnt_lock_post_idx(erts_lcnt_lock_info_carrier_t *carrier, int index);
ERTS_GLB_INLINE
void erts_lcnt_lock_post_x_idx(erts_lcnt_lock_info_carrier_t *carrier, int index, char *file, unsigned int line);

ERTS_GLB_INLINE
void erts_lcnt_lock_unacquire_idx(erts_lcnt_lock_info_carrier_t *carrier, int index);

ERTS_GLB_INLINE
void erts_lcnt_unlock_idx(erts_lcnt_lock_info_carrier_t *carrier, int index);
ERTS_GLB_INLINE
void erts_lcnt_unlock_opt_idx(erts_lcnt_lock_info_carrier_t *carrier, int index, erts_lock_options_t option);

ERTS_GLB_INLINE
void erts_lcnt_trylock_idx(erts_lcnt_lock_info_carrier_t *carrier, int index, int result);
ERTS_GLB_INLINE
void erts_lcnt_trylock_opt_idx(erts_lcnt_lock_info_carrier_t *carrier, int index, int result, erts_lock_options_t option);

/* -- Reference operations ------------------------------------------------- */

/** @brief Registers a lock counter reference; this must be called prior to
 * using any other functions in this module. */
ERTS_GLB_INLINE
void erts_lcnt_init_ref(erts_lcnt_ref_t *ref);

/** @brief As \c erts_lcnt_init_ref, but also enables lock counting right
 * away if appropriate to reduce noise.
 * @param id An immediate erlang term with whatever extra data you want to
 * identify this lock with. */
ERTS_GLB_INLINE
void erts_lcnt_init_ref_x(erts_lcnt_ref_t *ref, const char *name,
    Eterm id, erts_lock_flags_t flags);

/** @brief Checks whether counting is enabled on the given reference. */
ERTS_GLB_FORCE_INLINE
int erts_lcnt_check_ref_installed(erts_lcnt_ref_t *ref);

/** @brief Convenience macro to re/enable counting on an already initialized
 * reference. Don't forget to specify the lock type in \c flags! */
#define erts_lcnt_install_new_lock_info(ref, name, id, flags) \
    if(!erts_lcnt_check_ref_installed(ref)) { \
        erts_lcnt_lock_info_carrier_t *__carrier; \
        __carrier = erts_lcnt_create_lock_info_carrier(1);\
        erts_lcnt_init_lock_info_idx(__carrier, 0, name, id, flags); \
        erts_lcnt_install(ref, __carrier);\
    } while(0)

erts_lcnt_lock_info_carrier_t *erts_lcnt_create_lock_info_carrier(int count);

/* @brief Initializes the lock info at the given index.
 * @param id An immediate erlang term with whatever extra data you want to
 * identify this lock with.
 * @param flags The flags the lock itself was initialized with. Keep in mind
 * that all locks in a carrier must share the same category/static property. */
ERTS_GLB_INLINE
void erts_lcnt_init_lock_info_idx(erts_lcnt_lock_info_carrier_t *carrier, int index,
    const char *name, Eterm id, erts_lock_flags_t flags);

/** @brief Atomically installs the given lock counters. Nops (and releases the
 * provided carrier) if something was already installed. */
void erts_lcnt_install(erts_lcnt_ref_t *ref, erts_lcnt_lock_info_carrier_t *carrier);

/** @brief Atomically removes the currently installed lock counters. Nops if
 * nothing was installed. */
void erts_lcnt_uninstall(erts_lcnt_ref_t *ref);

ERTS_GLB_FORCE_INLINE
int erts_lcnt_open_ref(erts_lcnt_ref_t *ref, int *handle, erts_lcnt_lock_info_carrier_t **result);

ERTS_GLB_FORCE_INLINE
void erts_lcnt_close_ref(int handle, erts_lcnt_lock_info_carrier_t *carrier);

/* -- Module initialization ------------------------------------------------ */

void erts_lcnt_pre_thr_init(void);
void erts_lcnt_post_thr_init(void);
void erts_lcnt_late_init(void);

/* @brief Called after everything in the system has been initialized, including
 * the schedulers. This is mainly a backwards compatibility shim for matching
 * the old lcnt behavior where all lock counting was enabled by default. */
void erts_lcnt_post_startup(void);

void erts_lcnt_thread_setup(void);
void erts_lcnt_thread_exit_handler(void);

/* -- BIF interface -------------------------------------------------------- */

/** @brief Safely iterates through all entries in the given list.
 *
 * The referenced item will be valid until the next call to
 * \c erts_lcnt_iterate_list after which point it may be destroyed; call
 * erts_lcnt_retain_lock_info if you wish to hang on to it beyond that point.
 *
 * Iteration can be cancelled by calling erts_lcnt_release_lock_info on the
 * iterator and breaking out of the loop.
 *
 * @param iterator The iteration variable; set the pointee to NULL to start
 * iteration.
 * @return 1 while the iterator is valid, 0 at the end of the list. */
int erts_lcnt_iterate_list(erts_lcnt_lock_info_list_t *list, erts_lcnt_lock_info_t **iterator);

/** @brief Clears the counter state of all locks, and releases all locks
 * preserved through erts_lcnt_set_preserve_info (if any). */
void erts_lcnt_clear_counters(void);

/** @brief Retrieves the global lock counter state.
 *
 * Note that the lists may be modified while you're mucking around with them.
 * Always use \c erts_lcnt_iterate_list to enumerate them. */
erts_lcnt_data_t erts_lcnt_get_data(void);

void erts_lcnt_retain_lock_info(erts_lcnt_lock_info_t *info);
void erts_lcnt_release_lock_info(erts_lcnt_lock_info_t *info);

/** @brief Sets whether to preserve the info of destroyed/uninstalled locks.
 *
 * This option makes no distinction whether the lock was destroyed or if lock
 * counting was simply disabled, so erts_lcnt_set_category_mask must not be
 * used while this option is active. */
void erts_lcnt_set_preserve_info(int enable);

int erts_lcnt_get_preserve_info(void);

/** @brief Updates the category mask, enabling or disabling counting on the
 * affected locks as necessary.
 *
 * This is not guaranteed to find all existing locks; only those that are
 * flagged as static locks and those reachable through other means can be
 * altered. */
void erts_lcnt_set_category_mask(erts_lock_flags_t mask);

erts_lock_flags_t erts_lcnt_get_category_mask(void);

/* -- Inline implementation ------------------------------------------------ */

/* The following is a hack to get the things we need from erl_thr_progress.h,
 * which we can't #include without dependency hell breaking loose.
 *
 * The size of LcntThrPrgrLaterOp and value of the constant are verified at
 * compile-time in erts_lcnt_pre_thr_init. */

int lcnt_thr_progress_unmanaged_delay__(void);
void lcnt_thr_progress_unmanaged_continue__(int handle);
typedef struct { Uint64 _[4]; } LcntThrPrgrLaterOp;
#define LCNT_THR_PRGR_DHANDLE_MANAGED -1

struct lcnt_lock_info_carrier_ {
    ethr_atomic_t ref_count;

    LcntThrPrgrLaterOp release_entries;

    unsigned char entry_count;
    erts_lcnt_lock_info_t entries[];
};

typedef struct {
    erts_lcnt_time_t timer; /* timer */
    int timer_set;          /* bool */
    int lock_in_conflict;   /* bool */
} lcnt_thread_data_t__;

extern const int lcnt_log2_tab64__[];

extern ethr_tsd_key lcnt_thr_data_key__;
extern erts_lock_flags_t lcnt_category_mask__;

#ifdef DEBUG
extern int lcnt_initialization_completed__;
#endif

void lcnt_register_static_lock__(erts_lcnt_ref_t *reference, const char *name, Eterm id,
    erts_lock_flags_t flags);

void lcnt_deallocate_carrier__(erts_lcnt_lock_info_carrier_t *carrier);

ERTS_GLB_INLINE
int lcnt_log2__(Uint64 v);

ERTS_GLB_INLINE
void lcnt_update_wait_histogram__(erts_lcnt_hist_t *hist, erts_lcnt_time_t *time_waited);

ERTS_GLB_INLINE
void lcnt_update_stats__(erts_lcnt_lock_stats_t *stats, int lock_in_conflict, erts_lcnt_time_t *time_waited);

ERTS_GLB_INLINE
erts_lcnt_lock_stats_t *lcnt_get_lock_stats__(erts_lcnt_lock_info_t *info, char *file, unsigned int line);

ERTS_GLB_INLINE
void lcnt_dec_lock_state__(ethr_atomic_t *l_state);

ERTS_GLB_INLINE
void lcnt_time__(erts_lcnt_time_t *time);

ERTS_GLB_INLINE
void lcnt_time_add__(erts_lcnt_time_t *t, erts_lcnt_time_t *d);

ERTS_GLB_INLINE
void lcnt_time_diff__(erts_lcnt_time_t *d, erts_lcnt_time_t *t1, erts_lcnt_time_t *t0);

ERTS_GLB_INLINE
void lcnt_retain_carrier__(erts_lcnt_lock_info_carrier_t *carrier);

ERTS_GLB_INLINE
void lcnt_release_carrier__(erts_lcnt_lock_info_carrier_t *carrier);

ERTS_GLB_INLINE
lcnt_thread_data_t__ *lcnt_get_thread_data__(void);

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

ERTS_GLB_INLINE
void lcnt_time__(erts_lcnt_time_t *time) {
    /*
     * erts_sys_hrtime() is the highest resolution
     * we could find, it may or may not be monotonic...
     */
    ErtsMonotonicTime mtime = erts_sys_hrtime();
    time->s  = (unsigned long) (mtime / 1000000000LL);
    time->ns = (unsigned long) (mtime - 1000000000LL*time->s);
}

/* difference d must be non-negative */

ERTS_GLB_INLINE
void lcnt_time_add__(erts_lcnt_time_t *t, erts_lcnt_time_t *d) {
    t->s  += d->s;
    t->ns += d->ns;

    t->s  += t->ns / 1000000000LL;
    t->ns  = t->ns % 1000000000LL;
}

ERTS_GLB_INLINE
void lcnt_time_diff__(erts_lcnt_time_t *d, erts_lcnt_time_t *t1, erts_lcnt_time_t *t0) {
    long ds;
    long dns;

    ds  = t1->s  - t0->s;
    dns = t1->ns - t0->ns;

    /* the difference should not be able to get bigger than 1 sec in ns*/

    if (dns < 0) {
        ds  -= 1;
        dns += 1000000000LL;
    }

    ASSERT(ds >= 0);

    d->s  = ds;
    d->ns = dns;
}

ERTS_GLB_INLINE
int lcnt_log2__(Uint64 v) {
    v |= v >> 1;
    v |= v >> 2;
    v |= v >> 4;
    v |= v >> 8;
    v |= v >> 16;
    v |= v >> 32;

    return lcnt_log2_tab64__[((Uint64)((v - (v >> 1))*0x07EDD5E59A4E28C2)) >> 58];
}

ERTS_GLB_INLINE
void lcnt_update_wait_histogram__(erts_lcnt_hist_t *hist, erts_lcnt_time_t *time_waited) {
    int idx;

    if(time_waited->s > 0 || time_waited->ns > ERTS_LCNT_HISTOGRAM_MAX_NS) {
        idx = ERTS_LCNT_HISTOGRAM_SLOT_SIZE - 1;
    } else {
        unsigned long r = time_waited->ns >> ERTS_LCNT_HISTOGRAM_RSHIFT;

        idx = r ? lcnt_log2__(r) : 0;
    }

    hist->ns[idx]++;
}

ERTS_GLB_INLINE
void lcnt_update_stats__(erts_lcnt_lock_stats_t *stats, int lock_in_conflict, erts_lcnt_time_t *time_waited) {
    ethr_atomic_inc(&stats->attempts);

    if(lock_in_conflict) {
        ethr_atomic_inc(&stats->collisions);
    }

    if(time_waited) {
        stats->times_waited++;

        lcnt_time_add__(&stats->total_time_waited, time_waited);
        lcnt_update_wait_histogram__(&stats->wait_time_histogram, time_waited);
    }
}

/* If we were installed while the lock was held, r/w_state will be 0 and we
 * can't tell which unlock or unacquire operation was the last. To get around
 * this we assume that all excess operations go *towards* zero rather than down
 * to zero, eventually becoming consistent with the actual state once the lock
 * is fully released.
 *
 * Conflicts might not be counted until the recorded state is fully consistent
 * with the actual state, but there should be no other ill effects. */

ERTS_GLB_INLINE
void lcnt_dec_lock_state__(ethr_atomic_t *l_state) {
    ethr_sint_t state = ethr_atomic_dec_read_acqb(l_state);

    /* We can not assume that state is >= -1 here; unlock and unacquire might
     * bring it below -1 and race to increment it back. */

    if(state < 0) {
        ethr_atomic_inc_acqb(l_state);
    }
}

ERTS_GLB_INLINE
erts_lcnt_lock_stats_t *lcnt_get_lock_stats__(erts_lcnt_lock_info_t *info, char *file, unsigned int line) {
    unsigned int i;

    ASSERT(info->location_count >= 1 && info->location_count <= ERTS_LCNT_MAX_LOCK_LOCATIONS);

    for(i = 0; i < info->location_count; i++) {
        erts_lcnt_lock_stats_t *stats = &info->location_stats[i];

        if(stats->file == file && stats->line == line) {
            return stats;
        }
    }

    if(info->location_count < ERTS_LCNT_MAX_LOCK_LOCATIONS) {
        erts_lcnt_lock_stats_t *stats = &info->location_stats[info->location_count];

        stats->file = file;
        stats->line = line;

        info->location_count++;

        return stats;
    }

    return &info->location_stats[0];
}

ERTS_GLB_INLINE
lcnt_thread_data_t__ *lcnt_get_thread_data__(void) {
    lcnt_thread_data_t__ *eltd = (lcnt_thread_data_t__ *)ethr_tsd_get(lcnt_thr_data_key__);

    ASSERT(eltd);

    return eltd;
}

ERTS_GLB_FORCE_INLINE
int erts_lcnt_open_ref(erts_lcnt_ref_t *ref, int *handle, erts_lcnt_lock_info_carrier_t **result) {
    if(ERTS_LIKELY(!erts_lcnt_check_ref_installed(ref))) {
        return 0;
    }

    ASSERT(lcnt_initialization_completed__);

    (*handle) = lcnt_thr_progress_unmanaged_delay__();
    (*result) = (erts_lcnt_lock_info_carrier_t*)ethr_atomic_read(ref);

    if(*result) {
        if(*handle != LCNT_THR_PRGR_DHANDLE_MANAGED) {
            lcnt_retain_carrier__(*result);
            lcnt_thr_progress_unmanaged_continue__(*handle);
        }

        return 1;
    } else if(*handle != LCNT_THR_PRGR_DHANDLE_MANAGED) {
        lcnt_thr_progress_unmanaged_continue__(*handle);
    }

    return 0;
}

ERTS_GLB_FORCE_INLINE
void erts_lcnt_close_ref(int handle, erts_lcnt_lock_info_carrier_t *carrier) {
    if(handle != LCNT_THR_PRGR_DHANDLE_MANAGED) {
        lcnt_release_carrier__(carrier);
    }
}

ERTS_GLB_INLINE
void erts_lcnt_init_ref(erts_lcnt_ref_t *ref) {
    ethr_atomic_init(ref, (ethr_sint_t)NULL);
}

ERTS_GLB_INLINE
void erts_lcnt_init_ref_x(erts_lcnt_ref_t *ref, const char *name,
                          Eterm id, erts_lock_flags_t flags) {
    erts_lcnt_init_ref(ref);

    if(flags & ERTS_LOCK_FLAGS_PROPERTY_STATIC) {
        lcnt_register_static_lock__(ref, name, id, flags);
    }

    if(erts_lcnt_check_enabled(flags)) {
        erts_lcnt_install_new_lock_info(ref, name, id, flags);
    }
}

ERTS_GLB_FORCE_INLINE
int erts_lcnt_check_ref_installed(erts_lcnt_ref_t *ref) {
    return (!!*ethr_atomic_addr(ref));
}

ERTS_GLB_FORCE_INLINE
void erts_lcnt_lock(erts_lcnt_ref_t *ref) {
    erts_lcnt_lock_info_carrier_t *carrier;
    int handle;

    if(erts_lcnt_open_ref(ref, &handle, &carrier)) {
        erts_lcnt_lock_idx(carrier, 0);

        erts_lcnt_close_ref(handle, carrier);
    }
}

ERTS_GLB_FORCE_INLINE
void erts_lcnt_lock_opt(erts_lcnt_ref_t *ref, erts_lock_options_t option) {
    erts_lcnt_lock_info_carrier_t *carrier;
    int handle;

    if(erts_lcnt_open_ref(ref, &handle, &carrier)) {
        erts_lcnt_lock_opt_idx(carrier, 0, option);

        erts_lcnt_close_ref(handle, carrier);
    }
}

ERTS_GLB_FORCE_INLINE
void erts_lcnt_lock_post(erts_lcnt_ref_t *ref) {
    erts_lcnt_lock_info_carrier_t *carrier;
    int handle;

    if(erts_lcnt_open_ref(ref, &handle, &carrier)) {
        erts_lcnt_lock_post_idx(carrier, 0);

        erts_lcnt_close_ref(handle, carrier);
    }
}

ERTS_GLB_FORCE_INLINE
void erts_lcnt_lock_post_x(erts_lcnt_ref_t *ref, char *file, unsigned int line) {
    erts_lcnt_lock_info_carrier_t *carrier;
    int handle;

    if(erts_lcnt_open_ref(ref, &handle, &carrier)) {
        erts_lcnt_lock_post_x_idx(carrier, 0, file, line);

        erts_lcnt_close_ref(handle, carrier);
    }
}

ERTS_GLB_FORCE_INLINE
void erts_lcnt_lock_unacquire(erts_lcnt_ref_t *ref) {
    erts_lcnt_lock_info_carrier_t *carrier;
    int handle;

    if(erts_lcnt_open_ref(ref, &handle, &carrier)) {
        erts_lcnt_lock_unacquire_idx(carrier, 0);

        erts_lcnt_close_ref(handle, carrier);
    }
}

ERTS_GLB_FORCE_INLINE
void erts_lcnt_unlock(erts_lcnt_ref_t *ref) {
    erts_lcnt_lock_info_carrier_t *carrier;
    int handle;

    if(erts_lcnt_open_ref(ref, &handle, &carrier)) {
        erts_lcnt_unlock_idx(carrier, 0);

        erts_lcnt_close_ref(handle, carrier);
    }
}

ERTS_GLB_FORCE_INLINE
void erts_lcnt_unlock_opt(erts_lcnt_ref_t *ref, erts_lock_options_t option) {
    erts_lcnt_lock_info_carrier_t *carrier;
    int handle;

    if(erts_lcnt_open_ref(ref, &handle, &carrier)) {
        erts_lcnt_unlock_opt_idx(carrier, 0, option);

        erts_lcnt_close_ref(handle, carrier);
    }
}

ERTS_GLB_FORCE_INLINE
void erts_lcnt_trylock(erts_lcnt_ref_t *ref, int result) {
    erts_lcnt_lock_info_carrier_t *carrier;
    int handle;

    if(erts_lcnt_open_ref(ref, &handle, &carrier)) {
        erts_lcnt_trylock_idx(carrier, 0, result);

        erts_lcnt_close_ref(handle, carrier);
    }
}

ERTS_GLB_FORCE_INLINE
void erts_lcnt_trylock_opt(erts_lcnt_ref_t *ref, int result, erts_lock_options_t option) {
    erts_lcnt_lock_info_carrier_t *carrier;
    int handle;

    if(erts_lcnt_open_ref(ref, &handle, &carrier)) {
        erts_lcnt_trylock_opt_idx(carrier, 0, result, option);

        erts_lcnt_close_ref(handle, carrier);
    }
}

ERTS_GLB_INLINE
void erts_lcnt_lock_idx(erts_lcnt_lock_info_carrier_t *carrier, int index) {
    erts_lcnt_lock_opt_idx(carrier, index, ERTS_LOCK_OPTIONS_WRITE);
}

ERTS_GLB_INLINE
void erts_lcnt_lock_opt_idx(erts_lcnt_lock_info_carrier_t *carrier, int index, erts_lock_options_t option) {
    erts_lcnt_lock_info_t *info = &carrier->entries[index];

    lcnt_thread_data_t__ *eltd = lcnt_get_thread_data__();

    ASSERT(index < carrier->entry_count);

    ASSERT((option & ERTS_LOCK_OPTIONS_READ) || (option & ERTS_LOCK_OPTIONS_WRITE));

    if(option & ERTS_LOCK_OPTIONS_WRITE) {
        ethr_sint_t w_state, r_state;

        w_state = ethr_atomic_inc_read(&info->w_state) - 1;
        r_state = ethr_atomic_read(&info->r_state);

        /* We cannot acquire w_lock if either w or r are taken */
        eltd->lock_in_conflict = (w_state > 0) || (r_state > 0);
    } else {
        ethr_sint_t w_state = ethr_atomic_read(&info->w_state);

        /* We cannot acquire r_lock if w_lock is taken */
        eltd->lock_in_conflict = (w_state > 0);
    }

    if(option & ERTS_LOCK_OPTIONS_READ) {
        ASSERT(info->flags & ERTS_LOCK_FLAGS_PROPERTY_READ_WRITE);
        ethr_atomic_inc(&info->r_state);
    }

    if(eltd->lock_in_conflict) {
        /* Only set the timer if nobody else has it. This should only happen
         * when proc_locks acquires several locks "atomically." All other locks
         * will block the thread when locked (w_state > 0) */
        if(eltd->timer_set == 0) {
            lcnt_time__(&eltd->timer);
        }

        eltd->timer_set++;
    }
}

ERTS_GLB_INLINE
void erts_lcnt_lock_post_idx(erts_lcnt_lock_info_carrier_t *carrier, int index) {
    erts_lcnt_lock_post_x_idx(carrier, index, NULL, 0);
}

ERTS_GLB_INLINE
void erts_lcnt_lock_post_x_idx(erts_lcnt_lock_info_carrier_t *carrier, int index, char *file, unsigned int line) {
    erts_lcnt_lock_info_t *info = &carrier->entries[index];

    lcnt_thread_data_t__ *eltd = lcnt_get_thread_data__();
    erts_lcnt_lock_stats_t *stats;

    ASSERT(index < carrier->entry_count);

    /* If the lock was in conflict, update the time spent waiting. */
    stats = lcnt_get_lock_stats__(info, file, line);
    if(eltd->timer_set) {
        erts_lcnt_time_t time_wait;
        erts_lcnt_time_t timer;

        lcnt_time__(&timer);

        lcnt_time_diff__(&time_wait, &timer, &eltd->timer);
        lcnt_update_stats__(stats, eltd->lock_in_conflict, &time_wait);

        eltd->timer_set--;

        ASSERT(eltd->timer_set >= 0);
    } else {
        lcnt_update_stats__(stats, eltd->lock_in_conflict, NULL);
    }
}

ERTS_GLB_INLINE
void erts_lcnt_unlock_idx(erts_lcnt_lock_info_carrier_t *carrier, int index) {
    ASSERT(index < carrier->entry_count);

    erts_lcnt_unlock_opt_idx(carrier, index, ERTS_LOCK_OPTIONS_WRITE);
}

ERTS_GLB_INLINE
void erts_lcnt_unlock_opt_idx(erts_lcnt_lock_info_carrier_t *carrier, int index, erts_lock_options_t option) {
    erts_lcnt_lock_info_t *info = &carrier->entries[index];

    ASSERT(index < carrier->entry_count);

    ASSERT((option & ERTS_LOCK_OPTIONS_READ) || (option & ERTS_LOCK_OPTIONS_WRITE));

    if(option & ERTS_LOCK_OPTIONS_WRITE) {
        lcnt_dec_lock_state__(&info->w_state);
    }

    if(option & ERTS_LOCK_OPTIONS_READ) {
        ASSERT(info->flags & ERTS_LOCK_FLAGS_PROPERTY_READ_WRITE);
        lcnt_dec_lock_state__(&info->r_state);
    }
}

ERTS_GLB_INLINE
void erts_lcnt_lock_unacquire_idx(erts_lcnt_lock_info_carrier_t *carrier, int index) {
    erts_lcnt_lock_info_t *info = &carrier->entries[index];

    ASSERT(index < carrier->entry_count);

    lcnt_dec_lock_state__(&info->w_state);
}

ERTS_GLB_INLINE
void erts_lcnt_trylock_idx(erts_lcnt_lock_info_carrier_t *carrier, int index, int result) {
    ASSERT(index < carrier->entry_count);

    erts_lcnt_trylock_opt_idx(carrier, index, result, ERTS_LOCK_OPTIONS_WRITE);
}

ERTS_GLB_INLINE
void erts_lcnt_trylock_opt_idx(erts_lcnt_lock_info_carrier_t *carrier, int index, int result, erts_lock_options_t option) {
    erts_lcnt_lock_info_t *info = &carrier->entries[index];

    ASSERT(index < carrier->entry_count);

    ASSERT((option & ERTS_LOCK_OPTIONS_READ) || (option & ERTS_LOCK_OPTIONS_WRITE));

    if(result != EBUSY) {
        if(option & ERTS_LOCK_OPTIONS_WRITE) {
            ethr_atomic_inc(&info->w_state);
        }

        if(option & ERTS_LOCK_OPTIONS_READ) {
            ASSERT(info->flags & ERTS_LOCK_FLAGS_PROPERTY_READ_WRITE);
            ethr_atomic_inc(&info->r_state);
        }

        lcnt_update_stats__(&info->location_stats[0], 0, NULL);
    } else {
        ethr_atomic_inc(&info->location_stats[0].attempts);
        ethr_atomic_inc(&info->location_stats[0].collisions);
    }
}

ERTS_GLB_INLINE
void erts_lcnt_init_lock_info_idx(erts_lcnt_lock_info_carrier_t *carrier, int index,
                                  const char *name, Eterm id, erts_lock_flags_t flags) {
    erts_lcnt_lock_info_t *info = &carrier->entries[index];

    ASSERT(is_immed(id));

    ASSERT(flags & ERTS_LOCK_FLAGS_MASK_TYPE);
    ASSERT(flags & ERTS_LOCK_FLAGS_MASK_CATEGORY);

    info->flags = flags;
    info->name = name;
    info->id = id;
}

ERTS_GLB_INLINE
void lcnt_retain_carrier__(erts_lcnt_lock_info_carrier_t *carrier) {
#ifdef DEBUG
    ASSERT(ethr_atomic_inc_read_acqb(&carrier->ref_count) >= 2);
#else
    ethr_atomic_inc_acqb(&carrier->ref_count);
#endif
}

ERTS_GLB_INLINE
void lcnt_release_carrier__(erts_lcnt_lock_info_carrier_t *carrier) {
    ethr_sint_t count = ethr_atomic_dec_read_relb(&carrier->ref_count);

    ASSERT(count >= 0);

    if(count == 0) {
        lcnt_deallocate_carrier__(carrier);
    }
}

#endif

#endif /* ifdef  ERTS_ENABLE_LOCK_COUNT  */
#endif /* ifndef ERTS_LOCK_COUNT_H__     */
