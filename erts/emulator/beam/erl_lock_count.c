/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2008-2016. All Rights Reserved.
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
 * Description:	Statistics for locks.
 *
 * Author:	Björn-Egil Dahlberg
 * Date:	2008-07-03
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

/* Needed for VxWorks va_arg */
#include "sys.h"

#ifdef ERTS_ENABLE_LOCK_COUNT

#include "erl_lock_count.h"
#include "ethread.h"
#include "erl_term.h"
#include "atom.h"
#include <stdio.h>

/* globals, dont access these without locks or blocks */

ethr_mutex lcnt_data_lock;
erts_lcnt_data_t *erts_lcnt_data;
Uint16 erts_lcnt_rt_options;
erts_lcnt_time_t timer_start;
const char *str_undefined = "undefined";

static ethr_tsd_key lcnt_thr_data_key;
static int lcnt_n_thr;
static erts_lcnt_thread_data_t *lcnt_thread_data[2048];

/* local functions */

static ERTS_INLINE void lcnt_lock(void) {
    ethr_mutex_lock(&lcnt_data_lock); 
}

static ERTS_INLINE void lcnt_unlock(void) {
    ethr_mutex_unlock(&lcnt_data_lock); 
}

const int log2_tab64[64] = {
    63,  0, 58,  1, 59, 47, 53,  2,
    60, 39, 48, 27, 54, 33, 42,  3,
    61, 51, 37, 40, 49, 18, 28, 20,
    55, 30, 34, 11, 43, 14, 22,  4,
    62, 57, 46, 52, 38, 26, 32, 41,
    50, 36, 17, 19, 29, 10, 13, 21,
    56, 45, 25, 31, 35, 16,  9, 12,
    44, 24, 15,  8, 23,  7,  6,  5};

static ERTS_INLINE int lcnt_log2(Uint64 v) {
    v |= v >> 1;
    v |= v >> 2;
    v |= v >> 4;
    v |= v >> 8;
    v |= v >> 16;
    v |= v >> 32;
    return log2_tab64[((Uint64)((v - (v >> 1))*0x07EDD5E59A4E28C2)) >> 58];
}

static char* lcnt_lock_type(Uint16 flag) {
    switch(flag & ERTS_LCNT_LT_ALL) {
        case ERTS_LCNT_LT_SPINLOCK:   return "spinlock";
        case ERTS_LCNT_LT_RWSPINLOCK: return "rw_spinlock";
        case ERTS_LCNT_LT_MUTEX:      return "mutex";
        case ERTS_LCNT_LT_RWMUTEX:    return "rw_mutex";
        case ERTS_LCNT_LT_PROCLOCK:   return "proclock";
        default:                      return "";
    }
}

static void lcnt_clear_stats(erts_lcnt_lock_stats_t *stats) {
    ethr_atomic_set(&stats->tries, 0);
    ethr_atomic_set(&stats->colls, 0);
    stats->timer.s  = 0;
    stats->timer.ns = 0;
    stats->timer_n  = 0;
    stats->file     = (char *)str_undefined;
    stats->line     = 0;
    sys_memzero(stats->hist.ns, sizeof(stats->hist.ns));
}

static void lcnt_time(erts_lcnt_time_t *time) {
    /*
     * erts_sys_hrtime() is the highest resolution
     * we could find, it may or may not be monotonic...
     */
    ErtsMonotonicTime mtime = erts_sys_hrtime();
    time->s  = (unsigned long) (mtime / 1000000000LL);
    time->ns = (unsigned long) (mtime - 1000000000LL*time->s);
}

static void lcnt_time_diff(erts_lcnt_time_t *d, erts_lcnt_time_t *t1, erts_lcnt_time_t *t0) {
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

/* difference d must be non-negative */

static void lcnt_time_add(erts_lcnt_time_t *t, erts_lcnt_time_t *d) {
    t->s  += d->s;
    t->ns += d->ns;

    t->s  += t->ns / 1000000000LL;
    t->ns  = t->ns % 1000000000LL;
}

static erts_lcnt_thread_data_t *lcnt_thread_data_alloc(void) {
    erts_lcnt_thread_data_t *eltd;

    eltd = (erts_lcnt_thread_data_t*)malloc(sizeof(erts_lcnt_thread_data_t));
    if (!eltd) {
        ERTS_INTERNAL_ERROR("Lock counter failed to allocate memory!");
    }
    eltd->timer_set = 0;
    eltd->lock_in_conflict = 0;

    eltd->id = lcnt_n_thr++;
    /* set thread data to array */
    lcnt_thread_data[eltd->id] = eltd;

    return eltd;
} 

static erts_lcnt_thread_data_t *lcnt_get_thread_data(void) {
    return (erts_lcnt_thread_data_t *)ethr_tsd_get(lcnt_thr_data_key);
}

/* debug */

#if 0
static char* lock_opt(Uint16 flag) {
    if ((flag & ERTS_LCNT_LO_WRITE) && (flag & ERTS_LCNT_LO_READ)) return "rw";
    if  (flag & ERTS_LCNT_LO_READ )                                return "r ";
    if  (flag & ERTS_LCNT_LO_WRITE)                                return " w";
    return "--";
}

static void print_lock_x(erts_lcnt_lock_t *lock, Uint16 flag, char *action) {
    erts_aint_t w_state, r_state;
    char *type;

    if (strcmp(lock->name, "run_queue") != 0) return;
    type = lcnt_lock_type(lock->flag);
    r_state = ethr_atomic_read(&lock->r_state);
    w_state = ethr_atomic_read(&lock->w_state);

    if (lock->flag & flag) {
        erts_fprintf(stderr,"%10s [%24s] [r/w state %4ld/%4ld] %2s id %T\r\n",
                action,
                lock->name,
                r_state,
                w_state,
                type,
                lock->id);
    }
}
#endif

static erts_lcnt_lock_stats_t *lcnt_get_lock_stats(erts_lcnt_lock_t *lock, char *file, unsigned int line) {
    unsigned int i;
    erts_lcnt_lock_stats_t *stats = NULL;

    if (erts_lcnt_rt_options & ERTS_LCNT_OPT_LOCATION) {
        for (i = 0; i < lock->n_stats; i++) {
            if ((lock->stats[i].file == file) && (lock->stats[i].line == line)) {
                return &(lock->stats[i]);
            }
        }
        if (lock->n_stats < ERTS_LCNT_MAX_LOCK_LOCATIONS) {
            stats = &lock->stats[lock->n_stats];
            lock->n_stats++;
            stats->file = file;
            stats->line = line;
            return stats;
        }
    }
    return &lock->stats[0];
}

static void lcnt_update_stats_hist(erts_lcnt_hist_t *hist, erts_lcnt_time_t *time_wait) {
    int idx;
    unsigned long r;

    if (time_wait->s > 0 || time_wait->ns > ERTS_LCNT_HISTOGRAM_MAX_NS) {
        idx = ERTS_LCNT_HISTOGRAM_SLOT_SIZE - 1;
    } else {
        r = time_wait->ns >> ERTS_LCNT_HISTOGRAM_RSHIFT;
        if (r) idx = lcnt_log2(r);
        else idx = 0;
    }
    hist->ns[idx]++;
}

static void lcnt_update_stats(erts_lcnt_lock_stats_t *stats, int lock_in_conflict,
        erts_lcnt_time_t *time_wait) {

    ethr_atomic_inc(&stats->tries);

    if (lock_in_conflict)
        ethr_atomic_inc(&stats->colls);

    if (time_wait) {
        lcnt_time_add(&(stats->timer), time_wait);
        stats->timer_n++;
        lcnt_update_stats_hist(&stats->hist,time_wait);
    }
}

/* interface */

void erts_lcnt_init() {
    erts_lcnt_thread_data_t *eltd = NULL;

    /* init lock */
    if (ethr_mutex_init(&lcnt_data_lock) != 0) abort();

    /* init tsd */    
    lcnt_n_thr = 0;
    ethr_tsd_key_create(&lcnt_thr_data_key,"lcnt_data");

    lcnt_lock();

    erts_lcnt_rt_options = ERTS_LCNT_OPT_LOCATION | ERTS_LCNT_OPT_PROCLOCK;
    eltd = lcnt_thread_data_alloc();
    ethr_tsd_set(lcnt_thr_data_key, eltd);

    /* init lcnt structure */
    erts_lcnt_data = (erts_lcnt_data_t*)malloc(sizeof(erts_lcnt_data_t));
    if (!erts_lcnt_data) {
        ERTS_INTERNAL_ERROR("Lock counter failed to allocate memory!");
    }
    erts_lcnt_data->current_locks = erts_lcnt_list_init();
    erts_lcnt_data->deleted_locks = erts_lcnt_list_init();

    lcnt_unlock();

    /* set start timer and zero statistics */
    erts_lcnt_clear_counters();
}

void erts_lcnt_late_init() {
    erts_thr_install_exit_handler(erts_lcnt_thread_exit_handler);
}

/* list operations */

/* BEGIN ASSUMPTION: lcnt_data_lock taken */

erts_lcnt_lock_list_t *erts_lcnt_list_init(void) {
    erts_lcnt_lock_list_t *list;

    list = (erts_lcnt_lock_list_t*)malloc(sizeof(erts_lcnt_lock_list_t));
    if (!list) {
        ERTS_INTERNAL_ERROR("Lock counter failed to allocate memory!");
    }
    list->head = NULL;
    list->tail = NULL;
    list->n    = 0;
    return list;
}

/* only do this on the list with the deleted locks! */
void erts_lcnt_list_clear(erts_lcnt_lock_list_t *list) {
    erts_lcnt_lock_t *lock = NULL,
                     *next = NULL;

    lock = list->head;

    while(lock != NULL) {
        next = lock->next;
        free(lock);
        lock = next;
    }

    list->head = NULL;
    list->tail = NULL;
    list->n    = 0;
}

void erts_lcnt_list_insert(erts_lcnt_lock_list_t *list, erts_lcnt_lock_t *lock) {
    erts_lcnt_lock_t *tail = NULL;

    tail = list->tail;
    if (tail) {
        tail->next = lock;
        lock->prev = tail;
    } else {
        list->head = lock;
        lock->prev = NULL;
        ASSERT(!lock->next);
    }
    lock->next = NULL;
    list->tail = lock;

    list->n++;
}

void erts_lcnt_list_delete(erts_lcnt_lock_list_t *list, erts_lcnt_lock_t *lock) {
    if (lock->next) lock->next->prev = lock->prev;
    if (lock->prev) lock->prev->next = lock->next;
    if (list->head == lock) list->head = lock->next;
    if (list->tail == lock) list->tail = lock->prev;

    lock->prev = NULL;
    lock->next = NULL;
    list->n--;
}
/* END ASSUMPTION: lcnt_data_lock taken */


/* lock operations */

/* interface to erl_threads.h */
/* only lock on init and destroy, all others should use atomics */
void erts_lcnt_init_lock(erts_lcnt_lock_t *lock, char *name, Uint16 flag ) { 
    erts_lcnt_init_lock_x(lock, name, flag, NIL);
}

void erts_lcnt_init_lock_x(erts_lcnt_lock_t *lock, char *name, Uint16 flag, Eterm id) { 
    int i;
    if (name == NULL) { ERTS_LCNT_CLEAR_FLAG(lock); return; }
    lcnt_lock();

    lock->next = NULL;
    lock->prev = NULL;
    lock->flag = flag;
    lock->name = name;
    lock->id   = id;

    ethr_atomic_init(&lock->r_state, 0);
    ethr_atomic_init(&lock->w_state, 0);
#ifdef DEBUG
    ethr_atomic_init(&lock->flowstate, 0);
#endif

    lock->n_stats = 1;

    for (i = 0; i < ERTS_LCNT_MAX_LOCK_LOCATIONS; i++) {
        lcnt_clear_stats(&lock->stats[i]);
    }

    erts_lcnt_list_insert(erts_lcnt_data->current_locks, lock);
    lcnt_unlock();
}
/* init empty, instead of zero struct */
void erts_lcnt_init_lock_empty(erts_lcnt_lock_t *lock) {
    lock->next = NULL;
    lock->prev = NULL;
    lock->flag = 0;
    lock->name = NULL;
    lock->id   = NIL;
    ethr_atomic_init(&lock->r_state, 0);
    ethr_atomic_init(&lock->w_state, 0);
#ifdef DEBUG
    ethr_atomic_init(&lock->flowstate, 0);
#endif
    lock->n_stats = 0;
    sys_memzero(lock->stats, sizeof(lock->stats));
}
/* destroy lock */
void erts_lcnt_destroy_lock(erts_lcnt_lock_t *lock) {
    if (ERTS_LCNT_IS_LOCK_INVALID(lock)) return;
    lcnt_lock();

    if (erts_lcnt_rt_options & ERTS_LCNT_OPT_COPYSAVE) {
        erts_lcnt_lock_t *deleted_lock;
        /* copy structure and insert the copy */
        deleted_lock = (erts_lcnt_lock_t*)malloc(sizeof(erts_lcnt_lock_t));
        if (!deleted_lock) {
            ERTS_INTERNAL_ERROR("Lock counter failed to allocate memory!");
        }
        memcpy(deleted_lock, lock, sizeof(erts_lcnt_lock_t));
        deleted_lock->next = NULL;
        deleted_lock->prev = NULL;
        erts_lcnt_list_insert(erts_lcnt_data->deleted_locks, deleted_lock);
    }
    /* delete original */
    erts_lcnt_list_delete(erts_lcnt_data->current_locks, lock);
    ERTS_LCNT_CLEAR_FLAG(lock);

    lcnt_unlock();
}

/* lock */

void erts_lcnt_lock_opt(erts_lcnt_lock_t *lock, Uint16 option) {
    erts_aint_t r_state = 0, w_state = 0;
    erts_lcnt_thread_data_t *eltd;

    if (erts_lcnt_rt_options & ERTS_LCNT_OPT_SUSPEND) return;
    if (ERTS_LCNT_IS_LOCK_INVALID(lock)) return;

    eltd = lcnt_get_thread_data();
    ASSERT(eltd);

    w_state = ethr_atomic_read(&lock->w_state);

    if (option & ERTS_LCNT_LO_WRITE) {
        r_state = ethr_atomic_read(&lock->r_state);
        ethr_atomic_inc( &lock->w_state);
    }
    if (option & ERTS_LCNT_LO_READ) {
        ethr_atomic_inc( &lock->r_state);
    }

    /* we cannot acquire w_lock if either w or r are taken */
    /* we cannot acquire r_lock if w_lock is taken */	

    if ((w_state > 0) || (r_state > 0)) {
        eltd->lock_in_conflict = 1;
        if (eltd->timer_set == 0) {
            lcnt_time(&eltd->timer);
        }
        eltd->timer_set++;
    } else {
        eltd->lock_in_conflict = 0;
    }
}

void erts_lcnt_lock(erts_lcnt_lock_t *lock) {
    erts_aint_t w_state;
    erts_lcnt_thread_data_t *eltd;

    if (erts_lcnt_rt_options & ERTS_LCNT_OPT_SUSPEND) return;
    if (ERTS_LCNT_IS_LOCK_INVALID(lock)) return;

    w_state = ethr_atomic_read(&lock->w_state);
    ethr_atomic_inc(&lock->w_state);
    eltd = lcnt_get_thread_data();

    ASSERT(eltd);

    if (w_state > 0) {
        eltd->lock_in_conflict = 1;
        /* only set the timer if nobody else has it
         * This should only happen when proc_locks aquires several locks
         * 'atomicly'. All other locks will block the thread if w_state > 0
         * i.e. locked.
         */
        if (eltd->timer_set == 0) {
            lcnt_time(&eltd->timer);
        }
        eltd->timer_set++;
    } else {
        eltd->lock_in_conflict = 0;
    }
}

/* if a lock wasn't really a lock operation, bad bad process locks */

void erts_lcnt_lock_unaquire(erts_lcnt_lock_t *lock) {
    /* should check if this thread was "waiting" */
    if (erts_lcnt_rt_options & ERTS_LCNT_OPT_SUSPEND) return;
    if (ERTS_LCNT_IS_LOCK_INVALID(lock)) return;

    ethr_atomic_dec(&lock->w_state);
}

/*
 * erts_lcnt_lock_post
 *
 * Used when we get a lock (i.e. directly after a lock operation)
 * if the timer was set then we had to wait for the lock
 * lock_post will calculate the wait time.
 */

void erts_lcnt_lock_post(erts_lcnt_lock_t *lock) {
    erts_lcnt_lock_post_x(lock, (char*)str_undefined, 0);
}

void erts_lcnt_lock_post_x(erts_lcnt_lock_t *lock, char *file, unsigned int line) {
    erts_lcnt_thread_data_t *eltd;
    erts_lcnt_time_t timer;
    erts_lcnt_time_t time_wait;
    erts_lcnt_lock_stats_t *stats;
#ifdef DEBUG
    erts_aint_t flowstate;
#endif

    if (erts_lcnt_rt_options & ERTS_LCNT_OPT_SUSPEND) return;
    if (ERTS_LCNT_IS_LOCK_INVALID(lock)) return;

#ifdef DEBUG
    if (!(lock->flag & (ERTS_LCNT_LT_RWMUTEX | ERTS_LCNT_LT_RWSPINLOCK))) {
        flowstate = ethr_atomic_read(&lock->flowstate);
        ASSERT(flowstate == 0);
        ethr_atomic_inc(&lock->flowstate);
    }
#endif

    eltd = lcnt_get_thread_data();

    ASSERT(eltd);

    /* if lock was in conflict, time it */
    stats = lcnt_get_lock_stats(lock, file, line);
    if (eltd->timer_set) {
        lcnt_time(&timer);

        lcnt_time_diff(&time_wait, &timer, &(eltd->timer));
        lcnt_update_stats(stats, eltd->lock_in_conflict, &time_wait);
        eltd->timer_set--;
        ASSERT(eltd->timer_set >= 0);
    } else {
        lcnt_update_stats(stats, eltd->lock_in_conflict, NULL);
    }

}

/* unlock */

void erts_lcnt_unlock_opt(erts_lcnt_lock_t *lock, Uint16 option) {
    if (erts_lcnt_rt_options & ERTS_LCNT_OPT_SUSPEND) return;
    if (ERTS_LCNT_IS_LOCK_INVALID(lock)) return;
    if (option & ERTS_LCNT_LO_WRITE) ethr_atomic_dec(&lock->w_state);
    if (option & ERTS_LCNT_LO_READ ) ethr_atomic_dec(&lock->r_state);
}

void erts_lcnt_unlock(erts_lcnt_lock_t *lock) {
    if (erts_lcnt_rt_options & ERTS_LCNT_OPT_SUSPEND) return;
    if (ERTS_LCNT_IS_LOCK_INVALID(lock)) return;
#ifdef DEBUG
    {
        erts_aint_t w_state;  
        erts_aint_t flowstate;

        /* flowstate */
        flowstate = ethr_atomic_read(&lock->flowstate);
        ASSERT(flowstate == 1);
        ethr_atomic_dec(&lock->flowstate);

        /* write state */
        w_state = ethr_atomic_read(&lock->w_state);
        ASSERT(w_state > 0);
    }
#endif
    ethr_atomic_dec(&lock->w_state);
}

/* trylock */

void erts_lcnt_trylock_opt(erts_lcnt_lock_t *lock, int res, Uint16 option) {
    if (erts_lcnt_rt_options & ERTS_LCNT_OPT_SUSPEND) return;
    if (ERTS_LCNT_IS_LOCK_INVALID(lock)) return;
    /* Determine lock_state via res instead of state */
    if (res != EBUSY) {
        if (option & ERTS_LCNT_LO_WRITE) ethr_atomic_inc(&lock->w_state);
        if (option & ERTS_LCNT_LO_READ ) ethr_atomic_inc(&lock->r_state);
        lcnt_update_stats(&(lock->stats[0]), 0, NULL);
    } else {
        ethr_atomic_inc(&lock->stats[0].tries);
        ethr_atomic_inc(&lock->stats[0].colls);
    }
}


void erts_lcnt_trylock(erts_lcnt_lock_t *lock, int res) {
    /* Determine lock_state via res instead of state */
    if (erts_lcnt_rt_options & ERTS_LCNT_OPT_SUSPEND) return;
    if (ERTS_LCNT_IS_LOCK_INVALID(lock)) return;
    if (res != EBUSY) {
#ifdef DEBUG
        {
            erts_aint_t flowstate;
            flowstate = ethr_atomic_read(&lock->flowstate);
            ASSERT(flowstate == 0);
            ethr_atomic_inc( &lock->flowstate);
        }
#endif
        ethr_atomic_inc(&lock->w_state);
        lcnt_update_stats(&(lock->stats[0]), 0, NULL);
    } else {
        ethr_atomic_inc(&lock->stats[0].tries);
        ethr_atomic_inc(&lock->stats[0].colls);
    }
}

/* thread operations */

void erts_lcnt_thread_setup(void) {
    erts_lcnt_thread_data_t *eltd;

    lcnt_lock();
    /* lock for thread id global update */
    eltd = lcnt_thread_data_alloc();
    lcnt_unlock();
    ASSERT(eltd);
    ethr_tsd_set(lcnt_thr_data_key, eltd);
}

void erts_lcnt_thread_exit_handler() {
    erts_lcnt_thread_data_t *eltd;

    eltd = ethr_tsd_get(lcnt_thr_data_key);

    if (eltd) {
        free(eltd);
    }
}

/* bindings for bifs */

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
    erts_lcnt_lock_t *lock;
    erts_lcnt_lock_list_t *list;
    erts_lcnt_lock_stats_t *stats;
    int i;

    lcnt_lock();

    list = erts_lcnt_data->current_locks;

    for (lock = list->head; lock != NULL; lock = lock->next) {
        for( i = 0; i < ERTS_LCNT_MAX_LOCK_LOCATIONS; i++) {
            stats = &lock->stats[i];
            lcnt_clear_stats(stats);
        }
        lock->n_stats = 1;
    }

    /* empty deleted locks in lock list */
    erts_lcnt_list_clear(erts_lcnt_data->deleted_locks);

    lcnt_time(&timer_start);

    lcnt_unlock();
}

erts_lcnt_data_t *erts_lcnt_get_data(void) {
    erts_lcnt_time_t timer_stop;

    lcnt_lock();

    lcnt_time(&timer_stop);
    lcnt_time_diff(&(erts_lcnt_data->duration), &timer_stop, &timer_start);

    lcnt_unlock();

    return erts_lcnt_data;
}

char *erts_lcnt_lock_type(Uint16 type) {
    return lcnt_lock_type(type);
}

#endif /* ifdef ERTS_ENABLE_LOCK_COUNT */
