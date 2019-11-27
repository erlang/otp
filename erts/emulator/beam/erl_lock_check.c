/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2005-2018. All Rights Reserved.
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
 * Description: A lock checker that checks that each thread acquires
 *              locks according to a predefined global lock order. The
 *              global lock order is used to prevent deadlocks. If the
 *              lock order is violated, an error message is printed
 *              and the emulator aborts. The lock checker is only
 *              intended to be enabled when debugging.
 *
 * Author: Rickard Green
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

/* Needed for VxWorks va_arg */
#include "sys.h"

#ifdef ERTS_ENABLE_LOCK_CHECK

#include "erl_lock_check.h"
#include "erl_term.h"
#include "erl_threads.h"
#include "erl_atom_table.h"
#include "erl_utils.h"

typedef struct {
    char *name;
    char *internal_order;
} erts_lc_lock_order_t;

/*
 * Global lock order for locks in the emulator.
 *
 * Locks early (low indexes) in the 'erts_lock_order' array should be
 * locked before locks late (high indexes) in the array. Each lock has
 * a name which is set on initialization. If multiple locks with the
 * same name are used, either an immediate Erlang term (e.g. internal
 * pid) or the address of the lock is used for internal lock order.
 * The immediate Erlang term used for internal lock order is also set
 * on initialization. Locks with small immediate Erlang terms should
 * be locked before locks with large immediate Erlang terms, and
 * locks with small addresses should be locked before locks with
 * large addresses. The immediate terms and adresses (boxed pointers)
 * are compared as unsigned integers not as Erlang terms.
 *
 * Once a spinlock or rw(spin)lock has been locked, the thread is not
 * allowed to lock mutexes, rwmutexes or process locks until all
 * spinlocks and rwlocks have been unlocked. This restriction is not
 * reflected by the lock order below, but the lock checker will still
 * check for violations of this restriction.
 */
static erts_lc_lock_order_t erts_lock_order[] = {
    /*
     *	"Lock name"				"Internal lock order
     *  					 description (NULL
     *						 if only one lock use
     *						 the lock name)"
     */
    {   "NO LOCK",                              NULL                    },
    {	"driver_lock",				"driver_name"		},
    {	"port_lock",				"port_id"		},
    {	"port_data_lock",			"address"		},
    {	"reg_tab",				NULL			},
    {	"proc_main",				"pid"			},
    {   "old_code",                             "address"               },
#ifdef HIPE
    {	"hipe_mfait_lock",			NULL			},
#endif
    {	"nodes_monitors",			NULL			},
    {	"meta_name_tab",	         	"address"		},
    {	"db_tab",				"address"		},
    {	"db_tab_fix",				"address"		},
    {	"db_hash_slot",				"address"		},
    {	"erl_db_catree_base_node",		NULL		        },
    {	"erl_db_catree_route_node",		"index"		        },
    {	"resource_monitors",			"address"	        },
    {   "driver_list",                          NULL                    },
    {	"dist_entry",				"address"		},
    {	"proc_msgq",				"pid"			},
    {	"proc_btm",				"pid"			},
    {	"dist_entry_links",			"address"		},
    {   "update_persistent_term_permission",    NULL                    },
    {   "persistent_term_delete_permission",    NULL                    },
    {   "code_write_permission",                NULL                    },
    {	"purge_state",		      		NULL			},
    {	"proc_status",				"pid"			},
    {	"proc_trace",				"pid"			},
    {	"node_table",				NULL			},
    {	"dist_table",				NULL			},
    {	"sys_tracers",				NULL			},
    {	"export_tab",				NULL			},
    {	"fun_tab",				NULL			},
    {	"environ",				NULL			},
    {	"release_literal_areas",		NULL			},
    {	"drv_ev_state_grow",			NULL,   		},
    {	"drv_ev_state",				"address"		},
    {	"safe_hash",				"address"		},
    {   "state_prealloc",                       NULL                    },
    {	"schdlr_sspnd",				NULL			},
    {	"migration_info_update",		NULL			},
    {	"run_queue",				"address"		},
    {   "dirty_run_queue_sleep_list",		"address"		},
    {	"dirty_gc_info",			NULL			},
    {	"dirty_break_point_index",		NULL			},
    {	"process_table",			NULL			},
    {	"cpu_info",				NULL			},
    {	"pollset",				"address"		},
#ifdef __WIN32__
    {	"pollwaiter",				"address"		},
    {   "break_waiter_lock",                    NULL                    },
#endif /* __WIN32__ */
    {	"alcu_init_atoms",			NULL			},
    {	"mseg_init_atoms",			NULL			},
    {	"mmap_init_atoms",			NULL			},
    {	"drv_tsd",				NULL			},
    {	"async_enq_mtx",			NULL			},
    {   "msacc_list_mutex",                     NULL                    },
    {   "msacc_unmanaged_mutex",                NULL                    },
    {	"atom_tab",				NULL			},
    {	"dist_entry_out_queue",			"address"		},
    {	"port_sched_lock",			"port_id"		},
    {	"sys_msg_q", 				NULL			},
    {	"tracer_mtx", 				NULL			},
    {   "port_table",                           NULL                    },
    {	"magic_ref_table",			"address"		},
    {	"mtrace_op",				NULL			},
    {	"instr_x",				NULL			},
    {	"instr",				NULL			},
    {	"alcu_allocator",			"index"			},
    {	"mseg",					NULL			},
    {	"get_time",				NULL			},
    {	"get_corrected_time",			NULL			},
    {	"runtime",				NULL			},
    {	"pix_lock",				"address"		},
    {	"sched_stat",				NULL			},
    {	"async_init_mtx",			NULL			},
#ifdef __WIN32__
#ifdef DEBUG
    {   "save_ops_lock",                        NULL                    },
#endif
#endif
    {	"mtrace_buf",				NULL			},
    {	"os_monotonic_time",			NULL			},
    {	"erts_alloc_hard_debug",		NULL			},
    {	"hard_dbg_mseg",		        NULL	                },
    {	"erts_mmap",				NULL			}
};

#define ERTS_LOCK_ORDER_SIZE \
  (sizeof(erts_lock_order)/sizeof(erts_lc_lock_order_t))

#define LOCK_IS_TYPE_ORDER_VIOLATION(LCK_FLG, LCKD_FLG) \
    (((LCKD_FLG) & ERTS_LOCK_FLAGS_MASK_TYPE) == ERTS_LOCK_FLAGS_TYPE_SPINLOCK \
        && \
     ((LCK_FLG) & ERTS_LOCK_FLAGS_MASK_TYPE) != ERTS_LOCK_FLAGS_TYPE_SPINLOCK)

static __decl_noreturn void  __noreturn lc_abort(void);

static const char *rw_op_str(erts_lock_options_t options)
{
    if(options == ERTS_LOCK_OPTIONS_WRITE) {
        ERTS_INTERNAL_ERROR("Only write flag present");
    }

    return erts_lock_options_get_short_desc(options);
}

typedef struct lc_locked_lock_t_ lc_locked_lock_t;
struct lc_locked_lock_t_ {
    lc_locked_lock_t *next;
    lc_locked_lock_t *prev;
    UWord extra;
    Sint16 id;
    char *file;
    unsigned int line;
    erts_lock_flags_t flags;
    erts_lock_options_t taken_options;
    /*
     * Pointer back to the lock instance if it exists or NULL for proc locks.
     * If set, we use it to allow trylock of other lock instance
     * but with identical lock order as an already locked lock.
     */
    erts_lc_lock_t *lck;
};

typedef struct {
    lc_locked_lock_t *first;
    lc_locked_lock_t *last;
} lc_locked_lock_list_t;

typedef union lc_free_block_t_ lc_free_block_t;
union lc_free_block_t_ {
    lc_free_block_t *next;
    lc_locked_lock_t lock;
};

typedef struct {
    /*
     * m[X][Y] & 1 if we locked X directly after Y was locked.
     * m[X][Y] & 2 if we locked X indirectly after Y was locked.
     * m[X][0] = 1 if we locked X when nothing else was locked.
     * m[0][] is unused as it would represent locking "NO LOCK"
     */
    char m[ERTS_LOCK_ORDER_SIZE][ERTS_LOCK_ORDER_SIZE];

} lc_matrix_t;

static lc_matrix_t tot_lc_matrix;

#define ERTS_LC_FB_CHUNK_SIZE 10

typedef struct lc_alloc_chunk_t_ lc_alloc_chunk_t;
struct lc_alloc_chunk_t_ {
    lc_alloc_chunk_t* next;
    lc_free_block_t array[ERTS_LC_FB_CHUNK_SIZE];
};

typedef struct lc_thread_t_ lc_thread_t;
struct lc_thread_t_ {
    char *thread_name;
    int emu_thread;
    erts_tid_t tid;
    lc_thread_t *next;
    lc_thread_t *prev;
    lc_locked_lock_list_t locked;
    lc_locked_lock_list_t required;
    lc_free_block_t *free_blocks;
    lc_alloc_chunk_t *chunks;
    lc_matrix_t matrix;
};

static ethr_tsd_key locks_key;

static lc_thread_t *lc_threads = NULL;
static ethr_spinlock_t lc_threads_lock;

static ERTS_INLINE void
lc_lock_threads(void)
{
    ethr_spin_lock(&lc_threads_lock);
}

static ERTS_INLINE void
lc_unlock_threads(void)
{
    ethr_spin_unlock(&lc_threads_lock);
}

static ERTS_INLINE void lc_free(lc_thread_t* thr, lc_locked_lock_t *p)
{
    lc_free_block_t *fb = (lc_free_block_t *) p;
#ifdef DEBUG
    sys_memset((void *) p, 0xdf, sizeof(lc_free_block_t));
#endif
    fb->next = thr->free_blocks;
    thr->free_blocks = fb;
}

static lc_locked_lock_t *lc_core_alloc(lc_thread_t* thr)
{
    int i;
    lc_alloc_chunk_t* chunk;
    lc_free_block_t* fbs;
    chunk = (lc_alloc_chunk_t*) malloc(sizeof(lc_alloc_chunk_t));
    if (!chunk) {
        ERTS_INTERNAL_ERROR("Lock checker failed to allocate memory!");
    }
    chunk->next = thr->chunks;
    thr->chunks = chunk;

    fbs = chunk->array;
    for (i = 1; i < ERTS_LC_FB_CHUNK_SIZE - 1; i++) {
#ifdef DEBUG
	sys_memset((void *) &fbs[i], 0xdf, sizeof(lc_free_block_t));
#endif
	fbs[i].next = &fbs[i+1];
    }
#ifdef DEBUG
    sys_memset((void *) &fbs[ERTS_LC_FB_CHUNK_SIZE-1],
	   0xdf, sizeof(lc_free_block_t));
#endif
    fbs[ERTS_LC_FB_CHUNK_SIZE-1].next = thr->free_blocks;
    thr->free_blocks = &fbs[1];
    return &fbs[0].lock;
}

static ERTS_INLINE lc_locked_lock_t *lc_alloc(lc_thread_t* thr)
{
    lc_locked_lock_t *res;
    if (!thr->free_blocks)
	res = lc_core_alloc(thr);
    else {
	res = &thr->free_blocks->lock;
	thr->free_blocks = thr->free_blocks->next;
    }
    return res;
}


static lc_thread_t *
create_thread_data(char *thread_name)
{
    lc_thread_t *thr = malloc(sizeof(lc_thread_t));
    if (!thr)
	ERTS_INTERNAL_ERROR("Lock checker failed to allocate memory!");

    thr->thread_name = strdup(thread_name ? thread_name : "unknown");
    if (!thr->thread_name)
	ERTS_INTERNAL_ERROR("Lock checker failed to allocate memory!");

    thr->emu_thread = 0;
    thr->tid = erts_thr_self();
    thr->required.first = NULL;
    thr->required.last = NULL;
    thr->locked.first = NULL;
    thr->locked.last = NULL;
    thr->prev = NULL;
    thr->free_blocks = NULL;
    thr->chunks = NULL;
    sys_memzero(&thr->matrix, sizeof(thr->matrix));

    lc_lock_threads();
    thr->next = lc_threads;
    if (lc_threads)
	lc_threads->prev = thr;
    lc_threads = thr;
    lc_unlock_threads();
    erts_tsd_set(locks_key, (void *) thr);
    return thr;
}

static void collect_matrix(lc_matrix_t*);

static void
destroy_thread_data(lc_thread_t *thr)
{
    ASSERT(thr->thread_name);
    free((void *) thr->thread_name);
    ASSERT(thr->required.first == NULL);
    ASSERT(thr->required.last == NULL);
    ASSERT(thr->locked.first == NULL);
    ASSERT(thr->locked.last == NULL);

    lc_lock_threads();
    if (thr->prev)
	thr->prev->next = thr->next;
    else {
	ASSERT(lc_threads == thr);
	lc_threads = thr->next;
    }
    if (thr->next)
	thr->next->prev = thr->prev;

    collect_matrix(&thr->matrix);

    lc_unlock_threads();

    while (thr->chunks) {
        lc_alloc_chunk_t* free_me = thr->chunks;
        thr->chunks = thr->chunks->next;
        free(free_me);
    }

    free((void *) thr);
}

static ERTS_INLINE lc_thread_t *
get_my_locked_locks(void)
{
    return erts_tsd_get(locks_key);
}

static ERTS_INLINE lc_thread_t *
make_my_locked_locks(void)
{
    lc_thread_t *thr = get_my_locked_locks();
    if (thr)
	return thr;
    else
	return create_thread_data(NULL);
}

static ERTS_INLINE lc_locked_lock_t *
new_locked_lock(lc_thread_t* thr,
                erts_lc_lock_t *lck, erts_lock_options_t options,
		char *file, unsigned int line)
{
    lc_locked_lock_t *ll = lc_alloc(thr);
    ll->next = NULL;
    ll->prev = NULL;
    ll->id = lck->id;
    ll->extra = lck->extra;
    ll->file = file;
    ll->line = line;
    ll->flags = lck->flags;
    ll->taken_options = options;
    if ((lck->flags & ERTS_LOCK_FLAGS_MASK_TYPE) == ERTS_LOCK_FLAGS_TYPE_PROCLOCK)
        ll->lck = NULL;
    else
        ll->lck = lck;
    return ll;
}

static void
raw_print_lock(char *prefix, Sint16 id, Wterm extra, erts_lock_flags_t flags,
	       char* file, unsigned int line, char *suffix)
{
    char *lname = (1 <= id && id < ERTS_LOCK_ORDER_SIZE
		   ? erts_lock_order[id].name
		   : "unknown");
    erts_fprintf(stderr,"%s'%s:",prefix,lname);

    if (is_not_immed(extra))
      erts_fprintf(stderr,"%p",_unchecked_boxed_val(extra));
    else
      erts_fprintf(stderr,"%T",extra);
    erts_fprintf(stderr,"[%s]",erts_lock_flags_get_type_name(flags));

    if (file)
      erts_fprintf(stderr,"(%s:%d)",file,line);

    erts_fprintf(stderr,"'(%s)%s",rw_op_str(flags),suffix);
}

static void
print_lock2(char *prefix, Sint16 id, Wterm extra, erts_lock_flags_t flags, char *suffix)
{
  raw_print_lock(prefix, id, extra, flags, NULL, 0, suffix);
}

static void
print_lock(char *prefix, erts_lc_lock_t *lck, char *suffix)
{
  raw_print_lock(prefix, lck->id, lck->extra, lck->flags, NULL, 0, suffix);
}

static void
print_curr_locks(lc_thread_t *thr)
{
    lc_locked_lock_t *ll;
    if (!thr || !thr->locked.first)
	erts_fprintf(stderr,
		     "Currently no locks are locked by the %s thread.\n",
		     thr->thread_name);
    else {
	erts_fprintf(stderr,
		     "Currently these locks are locked by the %s thread:\n",
		     thr->thread_name);
	for (ll = thr->locked.first; ll; ll = ll->next)
	  raw_print_lock("  ", ll->id, ll->extra, ll->flags,
			 ll->file, ll->line, "\n");
    }
}

static void
print_lock_order(void)
{
    int i;
    erts_fprintf(stderr, "Lock order:\n");
    for (i = 1; i < ERTS_LOCK_ORDER_SIZE; i++) {
	if (erts_lock_order[i].internal_order)
	    erts_fprintf(stderr,
			 "  %s:%s\n",
			 erts_lock_order[i].name,
			 erts_lock_order[i].internal_order);
	else
	    erts_fprintf(stderr, "  %s\n", erts_lock_order[i].name);
    }
}

static void
uninitialized_lock(void)
{
    erts_fprintf(stderr, "Performing operations on uninitialized lock!\n");
    print_curr_locks(get_my_locked_locks());
    lc_abort();
}

static void
lock_twice(char *prefix, lc_thread_t *thr, erts_lc_lock_t *lck,
	   erts_lock_options_t options)
{
    erts_fprintf(stderr, "%s (%s)", prefix, rw_op_str(options));
    print_lock(" ", lck, " lock which is already locked by thread!\n");
    print_curr_locks(thr);
    lc_abort();
}

static void
unlock_op_mismatch(lc_thread_t *thr, erts_lc_lock_t *lck,
		   erts_lock_options_t options)
{
    erts_fprintf(stderr, "Unlocking (%s) ", rw_op_str(options));
    print_lock("", lck, " lock which mismatch previous lock operation!\n");
    print_curr_locks(thr);
    lc_abort();
}

static void
unlock_of_not_locked(lc_thread_t *thr, erts_lc_lock_t *lck)
{
    print_lock("Unlocking ", lck, " lock which is not locked by thread!\n");
    print_curr_locks(thr);
    lc_abort();
}

static void
lock_order_violation(lc_thread_t *thr, erts_lc_lock_t *lck)
{
    print_lock("Lock order violation occured when locking ", lck, "!\n");
    print_curr_locks(thr);
    print_lock_order();
    lc_abort();
}

static void
type_order_violation(char *op, lc_thread_t *thr,
		     erts_lc_lock_t *lck)
{
    erts_fprintf(stderr, "Lock type order violation occured when ");
    print_lock(op, lck, "!\n");
    ASSERT(thr);
    print_curr_locks(thr);
    lc_abort();
}

static void
lock_mismatch(lc_thread_t *thr, int exact,
	      int failed_have, erts_lc_lock_t *have, int have_len,
	      int failed_have_not, erts_lc_lock_t *have_not, int have_not_len)
{
    int i;
    erts_fprintf(stderr, "Lock mismatch found!\n");
    if (failed_have >= 0) {
	ASSERT(have && have_len > failed_have);
	print_lock2("At least the ",
		   have[failed_have].id, have[failed_have].extra, 0,
		   " lock is not locked when it should have been\n");
    }
    else if (failed_have_not >= 0) {
	ASSERT(have_not && have_not_len > failed_have_not);
	print_lock2("At least the ",
		    have_not[failed_have_not].id,
		    have_not[failed_have_not].extra,
		    0,
		    " lock is locked when it should not have been\n");
    }
    if (exact) {
	if (!have || have_len <= 0)
	    erts_fprintf(stderr,
			 "Thread should not have any locks locked at all\n");
	else {
	    erts_fprintf(stderr,
			 "Thread should have these and only these locks "
			 "locked:\n");
	    for (i = 0; i < have_len; i++)
		print_lock2("  ", have[i].id, have[i].extra, 0, "\n");
	}
    }
    else {
	if (have && have_len > 0) {
	    erts_fprintf(stderr,
			 "Thread should at least have these locks locked:\n");
	    for (i = 0; i < have_len; i++)
		print_lock2("  ", have[i].id, have[i].extra, 0, "\n");
	}
	if (have_not && have_not_len > 0) {
	    erts_fprintf(stderr,
			 "Thread should at least not have these locks "
			 "locked:\n");
	    for (i = 0; i < have_not_len; i++)
		print_lock2("  ", have_not[i].id, have_not[i].extra, 0, "\n");
	}
    }
    print_curr_locks(thr);
    lc_abort();
}

static void
unlock_of_required_lock(lc_thread_t *thr, erts_lc_lock_t *lck)
{
    print_lock("Unlocking required ", lck, " lock!\n");
    print_curr_locks(thr);
    lc_abort();
}

static void
unrequire_of_not_required_lock(lc_thread_t *thr, erts_lc_lock_t *lck)
{
    print_lock("Unrequire on ", lck, " lock not required!\n");
    print_curr_locks(thr);
    lc_abort();
}

static void
require_twice(lc_thread_t *thr, erts_lc_lock_t *lck)
{
    print_lock("Require on ", lck, " lock already required!\n");
    print_curr_locks(thr);
    lc_abort();
}

static void
required_not_locked(lc_thread_t *thr, erts_lc_lock_t *lck)
{
    print_lock("Required ", lck, " lock not locked!\n");
    print_curr_locks(thr);
    lc_abort();
}


static void
thread_exit_handler(void)
{
    lc_thread_t *thr = get_my_locked_locks();
    if (thr) {
	if (thr->locked.first) {
	    erts_fprintf(stderr,
			 "Thread exiting while having locked locks!\n");
	    print_curr_locks(thr);
	    lc_abort();
	}
	destroy_thread_data(thr);
	/* erts_tsd_set(locks_key, NULL); */
    }
}

static __decl_noreturn void
lc_abort(void)
{
#ifdef __WIN32__
    DebugBreak();
#else
    abort();
#endif
}

void
erts_lc_set_thread_name(char *thread_name)
{
    lc_thread_t *thr = get_my_locked_locks();
    if (!thr)
	thr = create_thread_data(thread_name);
    else {
	ASSERT(thr->thread_name);
	free((void *) thr->thread_name);
	thr->thread_name = strdup(thread_name ? thread_name : "unknown");
	if (!thr->thread_name)
	    ERTS_INTERNAL_ERROR("strdup failed");
    }
    thr->emu_thread = 1;
}

int
erts_lc_is_emu_thr(void)
{
    lc_thread_t *thr = get_my_locked_locks();
    return thr->emu_thread;
}

int
erts_lc_assert_failed(char *file, int line, char *assertion)
{
    erts_fprintf(stderr, "%s:%d: Lock check assertion \"%s\" failed!\n",
		 file, line, assertion);
    print_curr_locks(get_my_locked_locks());
    lc_abort();
    return 0;
}

void erts_lc_fail(char *fmt, ...)
{
    va_list args;
    erts_fprintf(stderr, "Lock check failed: ");
    va_start(args, fmt);
    erts_vfprintf(stderr, fmt, args);
    va_end(args);
    erts_fprintf(stderr, "\n");
    print_curr_locks(get_my_locked_locks());
    lc_abort();
}


Sint16
erts_lc_get_lock_order_id(char *name)
{
    int i;

    if (!name || name[0] == '\0')
	erts_fprintf(stderr, "Missing lock name\n");
    else {
	for (i = 0; i < ERTS_LOCK_ORDER_SIZE; i++)
	    if (sys_strcmp(erts_lock_order[i].name, name) == 0)
		return i;
	erts_fprintf(stderr,
		     "Lock name '%s' missing in lock order "
		     "(update erl_lock_check.c)\n",
		     name);
    }
    lc_abort();
    return (Sint16) -1;
}

static int
lc_is_term_order(Sint16 id)
{
    return erts_lock_order[id].internal_order != NULL
        && sys_strcmp(erts_lock_order[id].internal_order, "term") == 0;
}


static int compare_locked_by_id(lc_locked_lock_t *locked_lock, erts_lc_lock_t *comparand)
{
    if(locked_lock->id < comparand->id) {
        return -1;
    } else if(locked_lock->id > comparand->id) {
        return 1;
    }

    return 0;
}

static int compare_locked_by_id_extra(lc_locked_lock_t *ll, erts_lc_lock_t *comparand)
{
    int order = compare_locked_by_id(ll, comparand);

    if(order) {
        return order;
    }
    if (ll->flags & ERTS_LOCK_FLAGS_PROPERTY_TERM_ORDER) {
        ASSERT(!is_header(ll->extra) && !is_header(comparand->extra));
        return CMP(ll->extra, comparand->extra);
    }

    if(ll->extra < comparand->extra) {
        return -1;
    } else if(ll->extra > comparand->extra) {
        return 1;
    }
    return 0;
}

typedef int (*locked_compare_func)(lc_locked_lock_t *, erts_lc_lock_t *);

/* Searches through a list of taken locks, bailing when it hits an entry whose
 * order relative to the search template is the opposite of the one at the
 * start of the search. (*closest_neighbor) is either set to the exact match,
 * or the one closest to it in the sort order. */
static int search_locked_list(locked_compare_func compare,
                              lc_locked_lock_t *locked_locks,
                              erts_lc_lock_t *search_template,
                              lc_locked_lock_t **closest_neighbor)
{
    lc_locked_lock_t *iterator = locked_locks;

    (*closest_neighbor) = iterator;

    if(iterator) {
        int relative_order = compare(iterator, search_template);

        if(relative_order < 0) {
            while((iterator = iterator->next) != NULL) {
                relative_order = compare(iterator, search_template);

                if(relative_order >= 0) {
                    (*closest_neighbor) = iterator;
                    break;
                }
            }
        } else if(relative_order > 0) {
            while((iterator = iterator->prev) != NULL) {
                relative_order = compare(iterator, search_template);

                if(relative_order <= 0) {
                    (*closest_neighbor) = iterator;
                    break;
                }
            }
        }

        return relative_order == 0;
    }

    return 0;
}

/* Searches for a lock in the given list that matches search_template, and sets
 * (*locked_locks) to the closest lock in the sort order. */
static int
find_lock(lc_locked_lock_t **locked_locks, erts_lc_lock_t *search_template)
{
    lc_locked_lock_t *closest_neighbor;
    int found_lock;

    found_lock = search_locked_list(compare_locked_by_id_extra,
                                    (*locked_locks),
                                    search_template,
                                    &closest_neighbor);

    (*locked_locks) = closest_neighbor;

    if(found_lock) {
        erts_lock_options_t relevant_options;
        erts_lock_flags_t relevant_flags;

        /* We only care about the options and flags that are set in the
         * template. */
        relevant_options = (closest_neighbor->taken_options & search_template->taken_options);
        relevant_flags = (closest_neighbor->flags & search_template->flags);

        return search_template->taken_options == relevant_options &&
               search_template->flags == relevant_flags;
    }

    return 0;
}

/* Searches for a lock in the given list by id, and sets (*locked_locks) to the
 * closest lock in the sort order. */
static int
find_id(lc_locked_lock_t **locked_locks, Sint16 id)
{
    lc_locked_lock_t *closest_neighbor;
    erts_lc_lock_t search_template;
    int found_lock;

    search_template.id = id;

    found_lock = search_locked_list(compare_locked_by_id,
                                    (*locked_locks),
                                    &search_template,
                                    &closest_neighbor);

    (*locked_locks) = closest_neighbor;

    return found_lock;
}

void
erts_lc_have_locks(int *resv, erts_lc_lock_t *locks, int len)
{
    lc_thread_t *thr = get_my_locked_locks();
    int i;

    if (!thr) {
	for (i = 0; i < len; i++)
	    resv[i] = 0;
    }
    else {
	lc_locked_lock_t *ll = thr->locked.first;
	for (i = 0; i < len; i++)
	    resv[i] = find_lock(&ll, &locks[i]);
    }
}

void
erts_lc_have_lock_ids(int *resv, int *ids, int len)
{
    lc_thread_t *thr = get_my_locked_locks();
    int i;

    if (!thr) {
	for (i = 0; i < len; i++)
	    resv[i] = 0;
    }
    else {
	lc_locked_lock_t *ll = thr->locked.first;
	for (i = 0; i < len; i++)
	    resv[i] = find_id(&ll, ids[i]);
    }
}

void
erts_lc_check(erts_lc_lock_t *have, int have_len,
	      erts_lc_lock_t *have_not, int have_not_len)
{
    int i;
    lc_thread_t *thr = get_my_locked_locks();
    lc_locked_lock_t *ll;
    
    if (have && have_len > 0) {
	if (!thr)
	    lock_mismatch(NULL, 0,
			  -1, have, have_len,
			  -1, have_not, have_not_len);
	ll = thr->locked.first;
	for (i = 0; i < have_len; i++) {
	    if (!find_lock(&ll, &have[i]))
		lock_mismatch(thr, 0,
			      i, have, have_len,
			      -1, have_not, have_not_len);
	}
    }
    if (have_not && have_not_len > 0 && thr) {
	ll = thr->locked.first;
	for (i = 0; i < have_not_len; i++) {
	    if (find_lock(&ll, &have_not[i]))
		lock_mismatch(thr, 0,
			      -1, have, have_len,
			      i, have_not, have_not_len);
	}
    }
}

void
erts_lc_check_exact(erts_lc_lock_t *have, int have_len)
{
    lc_thread_t *thr = get_my_locked_locks();
    if (!thr) {
	if (have && have_len > 0)
	    lock_mismatch(NULL, 1,
			  -1, have, have_len,
			  -1, NULL, 0);
    }
    else {
	int i;
	lc_locked_lock_t *ll = thr->locked.first;
	for (i = 0; i < have_len; i++) {
	    if (!find_lock(&ll, &have[i]))
		lock_mismatch(thr, 1,
			      i, have, have_len,
			      -1, NULL, 0);
	}
	for (i = 0, ll = thr->locked.first; ll; ll = ll->next)
	    i++;
	if (i != have_len)
	    lock_mismatch(thr, 1,
			  -1, have, have_len,
			  -1, NULL, 0);
    }
}

void
erts_lc_check_no_locked_of_type(erts_lock_flags_t type)
{
    lc_thread_t *thr = get_my_locked_locks();
    if (thr) {
	lc_locked_lock_t *ll = thr->locked.first;
	for (ll = thr->locked.first; ll; ll = ll->next) {
	    if ((ll->flags & ERTS_LOCK_FLAGS_MASK_TYPE) == type) {
		erts_fprintf(stderr,
			     "Locked lock of type %s found which isn't "
			     "allowed here!\n",
			     erts_lock_flags_get_type_name(ll->flags));
		print_curr_locks(thr);
		lc_abort();
	    }
	}
    }
}

int
erts_lc_trylock_force_busy_flg(erts_lc_lock_t *lck, erts_lock_options_t options)
{
#ifdef ERTS_LC_DO_NOT_FORCE_BUSY_TRYLOCK_ON_LOCK_ORDER_VIOLATION
    return 0;
#else
    /*
     * Force busy trylock if locking doesn't follow lock order.
     * This in order to make sure that caller can handle
     * the situation without causing a lock order violation.
     */
    lc_thread_t *thr;

    if (lck->inited != ERTS_LC_INITITALIZED)
	uninitialized_lock();

    if (lck->id < 0)
	return 0;

    thr = get_my_locked_locks();

    if (!thr || !thr->locked.first) {
	ASSERT(!thr || !thr->locked.last);
	return 0;
    }
    else {
        lc_locked_lock_t *ll;
        int order;

	ASSERT(thr->locked.last);

#if 0 /* Ok when trylocking I guess... */
	if (LOCK_IS_TYPE_ORDER_VIOLATION(lck->flags, thr->locked.last->flags))
	    type_order_violation("trylocking ", thr, lck);
#endif

        ll = thr->locked.last;
        order = compare_locked_by_id_extra(ll, lck);

	if (order < 0)
            return 0;

	/*
	 * TryLock order violation
	 */

        /* Check that we are not trying to lock this lock twice */
        do {
            if (order == 0 && (ll->lck == lck || !ll->lck))
                lock_twice("Trylocking", thr, lck, options);
            ll = ll->prev;
            if (!ll)
                break;
            order = compare_locked_by_id_extra(ll, lck);
        } while (order >= 0);

#ifndef ERTS_LC_ALLWAYS_FORCE_BUSY_TRYLOCK_ON_LOCK_ORDER_VIOLATION
	/* We only force busy if a lock order violation would occur
	   and when on an even millisecond. */
	{
	    SysTimeval tv;
	    sys_gettimeofday(&tv);

	    if ((tv.tv_usec / 1000) & 1)
		return 0;
	}
#endif

	return 1;
    }
#endif
}

void erts_lc_trylock_flg_x(int locked, erts_lc_lock_t *lck, erts_lock_options_t options,
			   char *file, unsigned int line)
{
    lc_thread_t *thr;
    lc_locked_lock_t *ll;

    if (lck->inited != ERTS_LC_INITITALIZED)
	uninitialized_lock();

    if (lck->id < 0)
	return;

    thr = make_my_locked_locks();
    ll = locked ? new_locked_lock(thr, lck, options, file, line) : NULL;

    if (!thr->locked.last) {
	ASSERT(!thr->locked.first);
	if (locked)
	    thr->locked.first = thr->locked.last = ll;
    }
    else {
	lc_locked_lock_t *tl_lck;
#if 0 /* Ok when trylocking I guess... */
	if (LOCK_IS_TYPE_ORDER_VIOLATION(lck->flags, thr->locked.last->flags))
	    type_order_violation("trylocking ", thr, lck);
#endif

	for (tl_lck = thr->locked.last; tl_lck; tl_lck = tl_lck->prev) {
            int order = compare_locked_by_id_extra(tl_lck, lck);
	    if (order <= 0) {
                if (order == 0 && (tl_lck->lck == lck || !tl_lck->lck))
                    lock_twice("Trylocking", thr, lck, options);
		if (locked) {
		    ll->next = tl_lck->next;
		    ll->prev = tl_lck;
		    if (tl_lck->next)
			tl_lck->next->prev = ll;
		    else
			thr->locked.last = ll;
		    tl_lck->next = ll;
		}
		return;
	    }
	}

	if (locked) {
	    ll->next = thr->locked.first;
	    thr->locked.first->prev = ll;
	    thr->locked.first = ll;
	}
    }

}

void erts_lc_require_lock_flg(erts_lc_lock_t *lck, erts_lock_options_t options,
			      char *file, unsigned int line)
{
    lc_thread_t *thr = make_my_locked_locks();
    lc_locked_lock_t *ll = thr->locked.first;
    if (!find_lock(&ll, lck))
	required_not_locked(thr, lck);
    ll = new_locked_lock(thr, lck, options, file, line);
    if (!thr->required.last) {
	ASSERT(!thr->required.first);
	ll->next = ll->prev = NULL;
	thr->required.first = thr->required.last = ll;
    }
    else {
	lc_locked_lock_t *l_lck2;
	ASSERT(thr->required.first);
	for (l_lck2 = thr->required.last;
	     l_lck2;
	     l_lck2 = l_lck2->prev) {
            int order = compare_locked_by_id_extra(l_lck2, lck);
	    if (order < 0)
		break;
	    if (order == 0)
		require_twice(thr, lck);
	}
	if (!l_lck2) {
	    ll->next = thr->required.first;
	    ll->prev = NULL;
	    thr->required.first->prev = ll;
	    thr->required.first = ll;
	}
	else {
	    ll->next = l_lck2->next;
	    if (ll->next) {
		ASSERT(thr->required.last != l_lck2);
		ll->next->prev = ll;
	    }
	    else {
		ASSERT(thr->required.last == l_lck2);
		thr->required.last = ll;
	    }
	    ll->prev = l_lck2;
	    l_lck2->next = ll;
	}
    }
}

void erts_lc_unrequire_lock_flg(erts_lc_lock_t *lck, erts_lock_options_t options)
{
    lc_thread_t *thr = make_my_locked_locks();
    lc_locked_lock_t *ll = thr->locked.first;
    if (!find_lock(&ll, lck))
	required_not_locked(thr, lck);
    ll = thr->required.first;
    if (!find_lock(&ll, lck))
	unrequire_of_not_required_lock(thr, lck);
    if (ll->prev) {
	ASSERT(thr->required.first != ll);
	ll->prev->next = ll->next;
    }
    else {
	ASSERT(thr->required.first == ll);
	thr->required.first = ll->next;
    }
    if (ll->next) {
	ASSERT(thr->required.last != ll);
	ll->next->prev = ll->prev;
    }
    else {
	ASSERT(thr->required.last == ll);
	thr->required.last = ll->prev;
    }
    lc_free(thr, ll);
}

void erts_lc_lock_flg_x(erts_lc_lock_t *lck, erts_lock_options_t options,
			char *file, unsigned int line)
{
    lc_thread_t *thr;
    lc_locked_lock_t *new_ll;
    int order;

    if (lck->inited != ERTS_LC_INITITALIZED)
	uninitialized_lock();

    if (lck->id < 0)
	return;

    thr = make_my_locked_locks();
    new_ll = new_locked_lock(thr, lck, options, file, line);

    if (!thr->locked.last) {
	ASSERT(!thr->locked.first);
	thr->locked.last = thr->locked.first = new_ll;
        ASSERT(0 < lck->id && lck->id < ERTS_LOCK_ORDER_SIZE);
        thr->matrix.m[lck->id][0] = 1;
        return;
    }
    order = compare_locked_by_id_extra(thr->locked.last, lck);
    if (order < 0) {
        lc_locked_lock_t* ll;
	if (LOCK_IS_TYPE_ORDER_VIOLATION(lck->flags, thr->locked.last->flags)) {
	    type_order_violation("locking ", thr, lck);
        }

        ASSERT(0 < lck->id && lck->id < ERTS_LOCK_ORDER_SIZE);
        ll = thr->locked.last;
        thr->matrix.m[lck->id][ll->id] |= 1;
        for (ll = ll->prev; ll; ll = ll->prev) {
            ASSERT(0 < ll->id && ll->id < ERTS_LOCK_ORDER_SIZE);
            thr->matrix.m[lck->id][ll->id] |= 2;
        }

	new_ll->prev = thr->locked.last;
	thr->locked.last->next = new_ll;
	thr->locked.last = new_ll;
    }
    else if (order == 0)
	lock_twice("Locking", thr, lck, options);
    else
	lock_order_violation(thr, lck);
}

void erts_lc_unlock_flg(erts_lc_lock_t *lck, erts_lock_options_t options)
{
    lc_thread_t *thr;
    lc_locked_lock_t *ll;

    if (lck->inited != ERTS_LC_INITITALIZED)
	uninitialized_lock();

    if (lck->id < 0)
	return;

    thr = get_my_locked_locks();

    if (thr) {
	ll = thr->required.first;
	if (find_lock(&ll, lck))
	    unlock_of_required_lock(thr, lck);
    }

    for (ll = thr ? thr->locked.last : NULL; ll; ll = ll->prev) {
	if (ll->id == lck->id && ll->extra == lck->extra) {
	    if ((ll->taken_options & ERTS_LOCK_OPTIONS_RDWR) != options)
		unlock_op_mismatch(thr, lck, options);
	    if (ll->prev)
		ll->prev->next = ll->next;
	    else
		thr->locked.first = ll->next;
	    if (ll->next)
		ll->next->prev = ll->prev;
	    else
		thr->locked.last = ll->prev;
	    lc_free(thr, ll);
	    return;
	}
    }
    
    unlock_of_not_locked(thr, lck);
}

void erts_lc_might_unlock_flg(erts_lc_lock_t *lck, erts_lock_options_t options)
{
    lc_thread_t *thr;
    lc_locked_lock_t *ll;

    if (lck->inited != ERTS_LC_INITITALIZED)
	uninitialized_lock();

    if (lck->id < 0)
	return;

    thr = get_my_locked_locks();

    if (thr) {
	ll = thr->required.first;
	if (find_lock(&ll, lck))
	    unlock_of_required_lock(thr, lck);
    }

    ll = thr->locked.first;
    if (!find_lock(&ll, lck))
	unlock_of_not_locked(thr, lck);
}

int
erts_lc_trylock_force_busy(erts_lc_lock_t *lck)
{
    return erts_lc_trylock_force_busy_flg(lck, 0);
}

void
erts_lc_trylock_x(int locked, erts_lc_lock_t *lck, char *file, unsigned int line)
{
    erts_lc_trylock_flg_x(locked, lck, 0, file, line);
}

void
erts_lc_lock_x(erts_lc_lock_t *lck, char *file, unsigned int line)
{
    erts_lc_lock_flg_x(lck, 0, file, line);
}

void
erts_lc_unlock(erts_lc_lock_t *lck)
{
    erts_lc_unlock_flg(lck, 0);
}

void erts_lc_might_unlock(erts_lc_lock_t *lck)
{
    erts_lc_might_unlock_flg(lck, 0);
}

void erts_lc_require_lock(erts_lc_lock_t *lck, char *file, unsigned int line)
{
    erts_lc_require_lock_flg(lck, 0, file, line);
}

void erts_lc_unrequire_lock(erts_lc_lock_t *lck)
{
    erts_lc_unrequire_lock_flg(lck, 0);
}

void
erts_lc_init_lock(erts_lc_lock_t *lck, char *name, erts_lock_flags_t flags)
{
    lck->id = erts_lc_get_lock_order_id(name);
    lck->extra = (UWord) &lck->extra;
    ASSERT(is_not_immed(lck->extra));
    lck->flags = flags;
    lck->taken_options = 0;
    lck->inited = ERTS_LC_INITITALIZED;
}

void
erts_lc_init_lock_x(erts_lc_lock_t *lck, char *name, erts_lock_flags_t flags, Eterm extra)
{
    lck->id = erts_lc_get_lock_order_id(name);
    lck->extra = extra;
    lck->flags = flags;
    if (lc_is_term_order(lck->id)) {
        lck->flags |= ERTS_LOCK_FLAGS_PROPERTY_TERM_ORDER;
        ASSERT(!is_header(lck->extra));
    }
    else
        ASSERT(is_immed(lck->extra));
    lck->taken_options = 0;
    lck->inited = ERTS_LC_INITITALIZED;
}

void
erts_lc_destroy_lock(erts_lc_lock_t *lck)
{
    if (lck->inited != ERTS_LC_INITITALIZED)
	uninitialized_lock();

    lck->inited = 0;
    lck->id = -1;
    lck->extra = THE_NON_VALUE;
    lck->flags = 0;
    lck->taken_options = 0;
}

void
erts_lc_init(void)
{
    if (ethr_spinlock_init(&lc_threads_lock) != 0)
	ERTS_INTERNAL_ERROR("spinlock_init failed");

    erts_tsd_key_create(&locks_key,"erts_lock_check_key");
}

void
erts_lc_late_init(void)
{
    erts_thr_install_exit_handler(thread_exit_handler);
}


/*
 * erts_lc_pll(): print locked locks...
 */
void
erts_lc_pll(void)
{
    print_curr_locks(get_my_locked_locks());
}

static void collect_matrix(lc_matrix_t* matrix)
{
    int i, j;
    for (i = 1; i < ERTS_LOCK_ORDER_SIZE; i++) {
        for (j = 0; j <= i; j++) {
            tot_lc_matrix.m[i][j] |= matrix->m[i][j];
        }
#ifdef DEBUG
        for ( ; j < ERTS_LOCK_ORDER_SIZE; j++) {
            ASSERT(matrix->m[i][j] == 0);
        }
#endif
    }
}

Eterm
erts_lc_dump_graph(void)
{
    const char* basename = "lc_graph.";
    char filename[40];
    lc_matrix_t* tot = &tot_lc_matrix;
    lc_thread_t* thr;
    int i, j, name_max = 0;
    FILE* ff;

    lc_lock_threads();
    for (thr = lc_threads; thr; thr = thr->next) {
        collect_matrix(&thr->matrix);
    }
    lc_unlock_threads();

    sys_strcpy(filename, basename);
    sys_get_pid(filename + strlen(basename),
                sizeof(filename) - strlen(basename));
    ff = fopen(filename, "w");
    if (!ff)
        return am_error;

    for (i = 1; i < ERTS_LOCK_ORDER_SIZE; i++) {
        int len = strlen(erts_lock_order[i].name);
        if (name_max < len)
            name_max = len;
    }
    fputs("%This file was generated by erts_debug:lc_graph()\n\n", ff);
    fputs("%{ThisLockName, ThisLockId, LockedDirectlyBeforeThis, LockedIndirectlyBeforeThis}\n", ff);
    fprintf(ff, "[{%*s, %2d}", name_max, "\"NO LOCK\"", 0);
    for (i = 1; i < ERTS_LOCK_ORDER_SIZE; i++) {
        char* delim = "";
        fprintf(ff, ",\n {%*s, %2d, [", name_max, erts_lock_order[i].name, i);
        for (j = 0; j < ERTS_LOCK_ORDER_SIZE; j++) {
            if (tot->m[i][j] & 1) {
                fprintf(ff, "%s%d", delim, j);
                delim = ",";
            }
        }
        fprintf(ff, "], [");
        delim = "";
        for (j = 0; j < ERTS_LOCK_ORDER_SIZE; j++) {
            if (tot->m[i][j] == 2) {
                fprintf(ff, "%s%d", delim, j);
                delim = ",";
            }
        }
        fputs("]}", ff);
    }
    fputs("].", ff);
    fclose(ff);
    erts_fprintf(stderr, "Created file '%s' in current working directory\n",
                 filename);
    return am_ok;
}

#endif /* #ifdef ERTS_ENABLE_LOCK_CHECK */
