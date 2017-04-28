/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2009-2016. All Rights Reserved.
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
/* Erlang Native InterFace
 */

/*
 * Environment contains a pointer to currently executing process.
 * In the dirty case this pointer do however not point to the
 * actual process structure of the executing process, but instead
 * a "shadow process structure". This in order to be able to handle
 * heap allocation without the need to acquire the main lock on
 * the process.
 *
 * The dirty process is allowed to allocate on the heap without
 * the main lock, i.e., incrementing htop, but is not allowed to
 * modify mbuf, offheap, etc without the main lock. The dirty
 * process moves mbuf list and offheap list of the shadow process
 * structure into the real structure when the dirty nif call
 * completes.
 */


#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "erl_nif.h"

#include "sys.h"
#include "global.h"
#include "erl_binary.h"
#include "bif.h"
#include "error.h"
#include "big.h"
#include "erl_map.h"
#include "beam_bp.h"
#include "erl_thr_progress.h"
#include "dtrace-wrapper.h"
#include "erl_process.h"
#include "erl_bif_unique.h"
#include "erl_utils.h"
#undef ERTS_WANT_NFUNC_SCHED_INTERNALS__
#define ERTS_WANT_NFUNC_SCHED_INTERNALS__
#include "erl_nfunc_sched.h"
#if defined(USE_DYNAMIC_TRACE) && (defined(USE_DTRACE) || defined(USE_SYSTEMTAP))
#define HAVE_USE_DTRACE 1
#endif

#include <limits.h>
#include <stddef.h> /* offsetof */


/* Information about a loaded nif library.
 * Each successful call to erlang:load_nif will allocate an instance of
 * erl_module_nif. Two calls opening the same library will thus have the same
 * 'handle'.
 */
struct erl_module_nif {
    void* priv_data;
    void* handle;             /* "dlopen" */
    struct enif_entry_t entry;
    erts_refc_t rt_cnt;       /* number of resource types */
    erts_refc_t rt_dtor_cnt;  /* number of resource types with destructors */
    Module* mod;           /* Can be NULL if orphan with dtor-resources left */ 

    ErlNifFunc _funcs_copy_[1];  /* only used for old libs */
};

typedef ERL_NIF_TERM (*NativeFunPtr)(ErlNifEnv*, int, const ERL_NIF_TERM[]);

#ifdef DEBUG
#  define READONLY_CHECK
#  define ERTS_DBG_NIF_NOT_SCHED_MARKER ((void *) (UWord) 1)
#endif
#ifdef READONLY_CHECK
#  define ADD_READONLY_CHECK(ENV,PTR,SIZE) add_readonly_check(ENV,PTR,SIZE)
static void add_readonly_check(ErlNifEnv*, unsigned char* ptr, unsigned sz);
#else
#  define ADD_READONLY_CHECK(ENV,PTR,SIZE) ((void)0)
#endif

#ifdef ERTS_NIF_ASSERT_IN_ENV
#  define ASSERT_IN_ENV(ENV, TERM, NR, TYPE) dbg_assert_in_env(ENV, TERM, NR, TYPE, __func__)
static void dbg_assert_in_env(ErlNifEnv*, Eterm term, int nr, const char* type, const char* func);
#  include "erl_gc.h"
#else
#  define ASSERT_IN_ENV(ENV, TERM, NR, TYPE)
#endif

#ifdef DEBUG
static int is_offheap(const ErlOffHeap* off_heap);
#endif

#ifdef USE_VM_PROBES
void dtrace_nifenv_str(ErlNifEnv *, char *);
#endif

#define MIN_HEAP_FRAG_SZ 200
static Eterm* alloc_heap_heavy(ErlNifEnv* env, size_t need, Eterm* hp);

static ERTS_INLINE int
is_scheduler(void)
{
    ErtsSchedulerData *esdp = erts_get_scheduler_data();
    if (!esdp)
	return 0;
    if (ERTS_SCHEDULER_IS_DIRTY(esdp))
	return -1;
    return 1;
}

static ERTS_INLINE void
execution_state(ErlNifEnv *env, Process **c_pp, int *schedp)
{
    if (schedp)
	*schedp = is_scheduler();
    if (c_pp) {
	if (!env || env->proc->common.id == ERTS_INVALID_PID)
	    *c_pp = NULL;
	else {
	    Process *c_p = env->proc;

	    if (!(c_p->static_flags & ERTS_STC_FLG_SHADOW_PROC)) {
		ERTS_SMP_LC_ASSERT(erts_proc_lc_my_proc_locks(c_p)
				   & ERTS_PROC_LOCK_MAIN);
	    }
	    else {
		c_p = env->proc->next;
		ASSERT(is_scheduler() < 0);
		ASSERT(c_p && env->proc->common.id == c_p->common.id);
	    }

	    *c_pp = c_p;

	    ASSERT(!(c_p->static_flags & ERTS_STC_FLG_SHADOW_PROC));
	}
    }
}

static ERTS_INLINE Eterm* alloc_heap(ErlNifEnv* env, size_t need)
{
    Eterm* hp = env->hp;
    env->hp += need;
    if (env->hp <= env->hp_end) {
	return hp;
    }
    return alloc_heap_heavy(env, need, hp);
}

static Eterm* alloc_heap_heavy(ErlNifEnv* env, size_t need, Eterm* hp)
{
    env->hp = hp;
    if (env->heap_frag == NULL) {
	ASSERT(HEAP_LIMIT(env->proc) == env->hp_end);
        ASSERT(env->hp + need > env->hp_end);
	HEAP_TOP(env->proc) = env->hp;
    }
    else {
	Uint usz = env->hp - env->heap_frag->mem;
	env->proc->mbuf_sz += usz - env->heap_frag->used_size;
	env->heap_frag->used_size = usz;
	ASSERT(env->heap_frag->used_size <= env->heap_frag->alloc_size);
    }
    hp = erts_heap_alloc(env->proc, need, MIN_HEAP_FRAG_SZ);
    env->heap_frag = MBUF(env->proc);
    env->hp = hp + need;
    env->hp_end = env->heap_frag->mem + env->heap_frag->alloc_size;

    return hp;
}

#if SIZEOF_LONG != ERTS_SIZEOF_ETERM
static ERTS_INLINE void ensure_heap(ErlNifEnv* env, size_t may_need)
{
    if (env->hp + may_need > env->hp_end) {
	alloc_heap_heavy(env, may_need, env->hp);
	env->hp -= may_need;
    }
}
#endif

void erts_pre_nif(ErlNifEnv* env, Process* p, struct erl_module_nif* mod_nif,
                  Process* tracee)
{
    env->mod_nif = mod_nif;
    env->proc = p;
    env->hp = HEAP_TOP(p);
    env->hp_end = HEAP_LIMIT(p);
    env->heap_frag = NULL;
    env->fpe_was_unmasked = erts_block_fpe();
    env->tmp_obj_list = NULL;
    env->exception_thrown = 0;
    env->tracee = tracee;

    ASSERT(p->common.id != ERTS_INVALID_PID);

#ifdef ERTS_NIF_ASSERT_IN_ENV
    env->dbg_disable_assert_in_env = 0;
#endif
#if defined(DEBUG) && defined(ERTS_DIRTY_SCHEDULERS)
    {
	ErtsSchedulerData *esdp = erts_get_scheduler_data();
	ASSERT(esdp);

	if (!ERTS_SCHEDULER_IS_DIRTY(esdp)) {
	    erts_aint32_t state = erts_smp_atomic32_read_nob(&p->state);

	    ASSERT(p->scheduler_data == esdp);
	    ASSERT((state & (ERTS_PSFLG_RUNNING
			     | ERTS_PSFLG_RUNNING_SYS))
		   && !(state & (ERTS_PSFLG_DIRTY_RUNNING
				 | ERTS_PSFLG_DIRTY_RUNNING_SYS)));
	}
    }
#endif
}

static void full_cache_env(ErlNifEnv *env);
static void cache_env(ErlNifEnv* env);
static void full_flush_env(ErlNifEnv *env);
static void flush_env(ErlNifEnv* env);

/* Temporary object header, auto-deallocated when NIF returns
 * or when independent environment is cleared.
 */
struct enif_tmp_obj_t {
    struct enif_tmp_obj_t* next;
    void (*dtor)(struct enif_tmp_obj_t*);
    ErtsAlcType_t allocator;
    /*char data[];*/
};

static ERTS_INLINE void free_tmp_objs(ErlNifEnv* env)
{
    while (env->tmp_obj_list != NULL) {
	struct enif_tmp_obj_t* free_me = env->tmp_obj_list;
	env->tmp_obj_list = free_me->next;
	free_me->dtor(free_me);
    }
}

void erts_post_nif(ErlNifEnv* env)
{
    erts_unblock_fpe(env->fpe_was_unmasked);
    full_flush_env(env);
    free_tmp_objs(env);
    env->exiting = ERTS_PROC_IS_EXITING(env->proc);
}


/*
 * Initialize a NifExport struct. Create it if needed and store it in the
 * proc. The direct_fp function is what will be invoked by op_call_nif, and
 * the indirect_fp function, if not NULL, is what the direct_fp function
 * will call. If the allocated NifExport isn't enough to hold all of argv,
 * allocate a larger one. Save 'current' and registers if first time this
 * call is scheduled.
 */

static ERTS_INLINE ERL_NIF_TERM
schedule(ErlNifEnv* env, NativeFunPtr direct_fp, NativeFunPtr indirect_fp,
	 Eterm mod, Eterm func_name, int argc, const ERL_NIF_TERM argv[])
{
    NifExport *ep;
    Process *c_p, *dirty_shadow_proc;

    execution_state(env, &c_p, NULL);
    if (c_p == env->proc)
	dirty_shadow_proc = NULL;
    else
	dirty_shadow_proc = env->proc;

    ERTS_SMP_LC_ASSERT(ERTS_PROC_LOCK_MAIN & erts_proc_lc_my_proc_locks(c_p));

    ep = erts_nif_export_schedule(c_p, dirty_shadow_proc,
				  c_p->current,
				  c_p->cp,
				  (BeamInstr) em_call_nif,
				  direct_fp, indirect_fp,
				  mod, func_name,
				  argc, (const Eterm *) argv);
    if (!ep->m) {
	/* First time this call is scheduled... */
	erts_refc_inc(&env->mod_nif->rt_dtor_cnt, 1);
	ep->m = env->mod_nif;
    }
    return (ERL_NIF_TERM) THE_NON_VALUE;
}

#ifdef ERTS_DIRTY_SCHEDULERS

static ERL_NIF_TERM dirty_nif_finalizer(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM dirty_nif_exception(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

int
erts_call_dirty_nif(ErtsSchedulerData *esdp, Process *c_p, BeamInstr *I, Eterm *reg)
{
    int exiting;
    ERL_NIF_TERM *argv = (ERL_NIF_TERM *) reg;
    NifExport *nep = ERTS_I_BEAM_OP_TO_NIF_EXPORT(I);
    ErtsCodeMFA *codemfa = erts_code_to_codemfa(I);
    NativeFunPtr dirty_nif = (NativeFunPtr) I[1];
    ErlNifEnv env;
    ERL_NIF_TERM result;
#ifdef DEBUG
    erts_aint32_t state = erts_smp_atomic32_read_nob(&c_p->state);

    ASSERT(nep == ERTS_PROC_GET_NIF_TRAP_EXPORT(c_p));

    ASSERT(!c_p->scheduler_data);
    ASSERT((state & ERTS_PSFLG_DIRTY_RUNNING)
	&& !(state & (ERTS_PSFLG_RUNNING|ERTS_PSFLG_RUNNING_SYS)));
    ASSERT(esdp);

    nep->func = ERTS_DBG_NIF_NOT_SCHED_MARKER;
#endif

    erts_pre_nif(&env, c_p, nep->m, NULL);

    env.proc = erts_make_dirty_shadow_proc(esdp, c_p);

    env.proc->freason = EXC_NULL;
    env.proc->fvalue = NIL;
    env.proc->ftrace = NIL;
    env.proc->i = c_p->i;

    ASSERT(ERTS_SCHEDULER_IS_DIRTY(erts_proc_sched_data(c_p)));

    erts_smp_atomic32_read_band_mb(&c_p->state, ~(ERTS_PSFLG_DIRTY_CPU_PROC
						   | ERTS_PSFLG_DIRTY_IO_PROC));

    erts_smp_proc_unlock(c_p, ERTS_PROC_LOCK_MAIN);

    result = (*dirty_nif)(&env, codemfa->arity, argv); /* Call dirty NIF */

    erts_smp_proc_lock(c_p, ERTS_PROC_LOCK_MAIN);

    ASSERT(env.proc->static_flags & ERTS_STC_FLG_SHADOW_PROC);
    ASSERT(env.proc->next == c_p);

    exiting = ERTS_PROC_IS_EXITING(c_p);

    if (!exiting) {
	if (env.exception_thrown) {
	schedule_exception:
	    schedule(&env, dirty_nif_exception, NULL,
		     am_erts_internal, am_dirty_nif_exception,
		     1, &env.proc->fvalue);
	}
	else if (is_value(result)) {
	    schedule(&env, dirty_nif_finalizer, NULL,
		     am_erts_internal, am_dirty_nif_finalizer,
		     1, &result);
	}
	else if (env.proc->freason != TRAP) { /* user returned garbage... */
	    ERTS_DECL_AM(badreturn);
	    (void) enif_raise_exception(&env, AM_badreturn);
	    goto schedule_exception;
	}
	else {
	    /* Rescheduled by dirty NIF call... */
	    ASSERT(nep->func != ERTS_DBG_NIF_NOT_SCHED_MARKER);
	}
	c_p->i = env.proc->i;
	c_p->arity = env.proc->arity;
    }

#ifdef DEBUG
    if (nep->func == ERTS_DBG_NIF_NOT_SCHED_MARKER)
	nep->func = NULL;
#endif

    erts_unblock_fpe(env.fpe_was_unmasked);
    full_flush_env(&env);
    free_tmp_objs(&env);

    return exiting;
}

#endif

static void full_flush_env(ErlNifEnv* env)
{
    flush_env(env);
#ifdef ERTS_DIRTY_SCHEDULERS
    if (env->proc->static_flags & ERTS_STC_FLG_SHADOW_PROC)
	/* Dirty nif call using shadow process struct */
	erts_flush_dirty_shadow_proc(env->proc);
#endif
}

static void full_cache_env(ErlNifEnv* env)
{    
#ifdef ERTS_DIRTY_SCHEDULERS
    if (env->proc->static_flags & ERTS_STC_FLG_SHADOW_PROC)
	erts_cache_dirty_shadow_proc(env->proc);
#endif
    cache_env(env);
}

/* Flush out our cached heap pointers to allow an ordinary HAlloc
*/
static void flush_env(ErlNifEnv* env)
{
    if (env->heap_frag == NULL) {
	ASSERT(env->hp_end == HEAP_LIMIT(env->proc));
	ASSERT(env->hp >= HEAP_TOP(env->proc));
	ASSERT(env->hp <= HEAP_LIMIT(env->proc));	
	HEAP_TOP(env->proc) = env->hp;
    }
    else {
	Uint usz;
	ASSERT(env->hp_end != HEAP_LIMIT(env->proc));
	ASSERT(env->hp_end - env->hp <= env->heap_frag->alloc_size);
	usz = env->hp - env->heap_frag->mem;
	env->proc->mbuf_sz += usz - env->heap_frag->used_size;
	env->heap_frag->used_size = usz;
	ASSERT(env->heap_frag->used_size <= env->heap_frag->alloc_size);
    }
}

/* Restore cached heap pointers to allow alloc_heap again.
*/
static void cache_env(ErlNifEnv* env)
{
    env->heap_frag = MBUF(env->proc);
    if (env->heap_frag == NULL) {
	ASSERT(env->hp_end == HEAP_LIMIT(env->proc));
	ASSERT(env->hp <= HEAP_TOP(env->proc));
	ASSERT(env->hp <= HEAP_LIMIT(env->proc));	
	env->hp = HEAP_TOP(env->proc);
    }
    else {
	env->hp = env->heap_frag->mem + env->heap_frag->used_size;
	env->hp_end = env->heap_frag->mem + env->heap_frag->alloc_size;
    }
}
void* enif_priv_data(ErlNifEnv* env)
{
    return env->mod_nif->priv_data;
}

void* enif_alloc(size_t size)
{
    return erts_alloc_fnf(ERTS_ALC_T_NIF, (Uint) size);
}

void* enif_realloc(void* ptr, size_t size)
{
    return erts_realloc_fnf(ERTS_ALC_T_NIF, ptr, size);
}

void enif_free(void* ptr)
{
    erts_free(ERTS_ALC_T_NIF, ptr);
}

struct enif_msg_environment_t
{
    ErlNifEnv env;
    Process phony_proc;
};

static ERTS_INLINE void
setup_nif_env(struct enif_msg_environment_t* msg_env,
              struct erl_module_nif* mod,
              Process* tracee)
{
    Eterm* phony_heap = (Eterm*) msg_env; /* dummy non-NULL ptr */

    msg_env->env.hp = phony_heap;
    msg_env->env.hp_end = phony_heap;
    msg_env->env.heap_frag = NULL;
    msg_env->env.mod_nif = mod;
    msg_env->env.tmp_obj_list = NULL;
    msg_env->env.proc = &msg_env->phony_proc;
    msg_env->env.exception_thrown = 0;
    memset(&msg_env->phony_proc, 0, sizeof(Process));
    HEAP_START(&msg_env->phony_proc) = phony_heap;
    HEAP_TOP(&msg_env->phony_proc) = phony_heap;
    HEAP_LIMIT(&msg_env->phony_proc) = phony_heap;
    HEAP_END(&msg_env->phony_proc) = phony_heap;
    MBUF(&msg_env->phony_proc) = NULL;
    msg_env->phony_proc.common.id = ERTS_INVALID_PID;
    msg_env->env.tracee = tracee;

#ifdef FORCE_HEAP_FRAGS
    msg_env->phony_proc.space_verified = 0;
    msg_env->phony_proc.space_verified_from = NULL;
#endif
#ifdef ERTS_NIF_ASSERT_IN_ENV
    msg_env->env.dbg_disable_assert_in_env = 0;
#endif
}

ErlNifEnv* enif_alloc_env(void)
{
    struct enif_msg_environment_t* msg_env =
	erts_alloc_fnf(ERTS_ALC_T_NIF, sizeof(struct enif_msg_environment_t));
    setup_nif_env(msg_env, NULL, NULL);
    return &msg_env->env;
}
void enif_free_env(ErlNifEnv* env)
{
    enif_clear_env(env);
    erts_free(ERTS_ALC_T_NIF, env);
}

static ERTS_INLINE void pre_nif_noproc(struct enif_msg_environment_t* msg_env,
                                       struct erl_module_nif* mod,
                                       Process* tracee)
{
    setup_nif_env(msg_env, mod, tracee);
    msg_env->env.fpe_was_unmasked = erts_block_fpe();
}

static ERTS_INLINE void post_nif_noproc(struct enif_msg_environment_t* msg_env)
{
    erts_unblock_fpe(msg_env->env.fpe_was_unmasked);
    enif_clear_env(&msg_env->env);
}

static ERTS_INLINE void clear_offheap(ErlOffHeap* oh)
{
    oh->first = NULL;
    oh->overhead = 0;
}

void enif_clear_env(ErlNifEnv* env)
{
    struct enif_msg_environment_t* menv = (struct enif_msg_environment_t*)env;
    Process* p = &menv->phony_proc;
    ASSERT(p == menv->env.proc);
    ASSERT(p->common.id == ERTS_INVALID_PID);
    ASSERT(MBUF(p) == menv->env.heap_frag);
    if (MBUF(p) != NULL) {
	erts_cleanup_offheap(&MSO(p));
	clear_offheap(&MSO(p));
	free_message_buffer(MBUF(p));
	MBUF(p) = NULL;
	menv->env.heap_frag = NULL;
    }
    ASSERT(HEAP_TOP(p) == HEAP_END(p));
    menv->env.hp = menv->env.hp_end = HEAP_TOP(p);
    
    ASSERT(!is_offheap(&MSO(p)));
    free_tmp_objs(env);
}

#ifdef ERTS_SMP
#ifdef DEBUG
static int enif_send_delay = 0;
#define ERTS_FORCE_ENIF_SEND_DELAY() (enif_send_delay++ % 2 == 0)
#else
#ifdef ERTS_PROC_LOCK_OWN_IMPL
#define ERTS_FORCE_ENIF_SEND_DELAY() 0
#else
/*
 * We always schedule messages if we do not use our own
 * process lock implementation, as if we try to do a trylock on
 * a lock that might already be locked by the same thread.
 * And what happens then with different mutex implementations
 * is not always guaranteed.
 */
#define ERTS_FORCE_ENIF_SEND_DELAY() 1
#endif
#endif

int erts_flush_trace_messages(Process *c_p, ErtsProcLocks c_p_locks)
{
    ErlTraceMessageQueue *msgq, **last_msgq;
    int reds = 0;

    erts_smp_proc_lock(c_p, ERTS_PROC_LOCK_TRACE);

    msgq = c_p->trace_msg_q;

    if (!msgq)
        goto error;

    do {
        Process* rp;
        ErtsProcLocks rp_locks;
        ErtsMessage *first, **last;
        Uint len;

        first = msgq->first;
        last = msgq->last;
        len = msgq->len;
        msgq->first = NULL;
        msgq->last = &msgq->first;
        msgq->len = 0;
        erts_smp_proc_unlock(c_p, ERTS_PROC_LOCK_TRACE);

        ASSERT(len != 0);

        rp = erts_proc_lookup(msgq->receiver);
        if (rp) {
            rp_locks = 0;
            if (rp->common.id == c_p->common.id)
                rp_locks = c_p_locks;
            erts_queue_messages(rp, rp_locks, first, last, len, c_p->common.id);
            if (rp->common.id == c_p->common.id)
                rp_locks &= ~c_p_locks;
            if (rp_locks)
                erts_smp_proc_unlock(rp, rp_locks);
            reds += len;
        } else {
            erts_cleanup_messages(first);
        }
        reds += 1;
        erts_smp_proc_lock(c_p, ERTS_PROC_LOCK_TRACE);
        msgq = msgq->next;
    } while (msgq);

    last_msgq = &c_p->trace_msg_q;

    while (*last_msgq) {
        msgq = *last_msgq;
        if (msgq->len == 0) {
            *last_msgq = msgq->next;
            erts_free(ERTS_ALC_T_TRACE_MSG_QUEUE, msgq);
        } else {
            last_msgq = &msgq->next;
        }
    }

error:
    erts_smp_proc_unlock(c_p, ERTS_PROC_LOCK_TRACE);

    return reds;
}

#endif

int enif_send(ErlNifEnv* env, const ErlNifPid* to_pid,
	      ErlNifEnv* msg_env, ERL_NIF_TERM msg)
{
    struct enif_msg_environment_t* menv = (struct enif_msg_environment_t*)msg_env;
    ErtsProcLocks rp_locks = 0;
#ifdef ERTS_SMP
    ErtsProcLocks lc_locks = 0;
#endif
    Process* rp;
    Process* c_p;
    ErtsMessage *mp;
    Eterm receiver = to_pid->pid;
    int scheduler;

    execution_state(env, &c_p, &scheduler);

#ifndef ERTS_SMP
    if (!scheduler) {
	erts_exit(ERTS_ABORT_EXIT,
		  "enif_send: called from non-scheduler thread on non-SMP VM");
	return 0;
   }
#endif

    if (scheduler > 0) { /* Normal scheduler */
	rp = erts_proc_lookup(receiver);
	if (!rp)
	    return 0;
    }
    else {
	if (c_p) {
	    ASSERT(scheduler < 0); /* Dirty scheduler */
	    if (ERTS_PROC_IS_EXITING(c_p))
		return 0;

	    if (env->proc->static_flags & ERTS_STC_FLG_SHADOW_PROC) {
		erts_smp_proc_lock(c_p, ERTS_PROC_LOCK_MAIN);
	    }
	}

	rp = erts_pid2proc_opt(c_p, ERTS_PROC_LOCK_MAIN,
			       receiver, rp_locks,
			       ERTS_P2P_FLG_INC_REFC);
	if (!rp) {
	    if (c_p && (env->proc->static_flags & ERTS_STC_FLG_SHADOW_PROC))
		erts_smp_proc_unlock(c_p, ERTS_PROC_LOCK_MAIN);
	    return 0;
	}
    }

    if (c_p == rp)
	rp_locks = ERTS_PROC_LOCK_MAIN;

    if (menv) {
        flush_env(msg_env);
        mp = erts_alloc_message(0, NULL);
        mp->data.heap_frag = menv->env.heap_frag;
        ASSERT(mp->data.heap_frag == MBUF(&menv->phony_proc));
        if (mp->data.heap_frag != NULL) {
            /* Move all offheap's from phony proc to the first fragment.
               Quick and dirty... */
            ASSERT(!is_offheap(&mp->data.heap_frag->off_heap));
            mp->data.heap_frag->off_heap = MSO(&menv->phony_proc);
            clear_offheap(&MSO(&menv->phony_proc));
            menv->env.heap_frag = NULL;
            MBUF(&menv->phony_proc) = NULL;
        }
    } else {
        erts_literal_area_t litarea;
	ErlOffHeap *ohp;
        Eterm *hp;
        Uint sz;
        INITIALIZE_LITERAL_PURGE_AREA(litarea);
        sz = size_object_litopt(msg, &litarea);
	if (c_p && !env->tracee) {
	    full_flush_env(env);
	    mp = erts_alloc_message_heap(rp, &rp_locks, sz, &hp, &ohp);
	    full_cache_env(env);
	}
	else {
	    erts_aint_t state = erts_smp_atomic32_read_nob(&rp->state);
	    if (state & ERTS_PSFLG_OFF_HEAP_MSGQ) {
		mp = erts_alloc_message(sz, &hp);
		ohp = sz == 0 ? NULL : &mp->hfrag.off_heap;
	    }
	    else {
		ErlHeapFragment *bp = new_message_buffer(sz);
		mp = erts_alloc_message(0, NULL);
		mp->data.heap_frag = bp;
		hp = bp->mem;
		ohp = &bp->off_heap;
	    }
	}
        msg = copy_struct_litopt(msg, sz, &hp, ohp, &litarea);
    }

    ERL_MESSAGE_TERM(mp) = msg;

    if (!env || !env->tracee) {

        if (c_p && IS_TRACED_FL(c_p, F_TRACE_SEND)) {
	    full_flush_env(env);
            trace_send(c_p, receiver, msg);
	    full_cache_env(env);
	}
    }
#ifdef ERTS_SMP
    else {
        /* This clause is taken when the nif is called in the context
           of a traced process. We do not know which locks we have
           so we have to do a try lock and if that fails we enqueue
           the message in a special trace message output queue of the
           tracee */
        ErlTraceMessageQueue *msgq;
        Process *t_p = env->tracee;


        erts_smp_proc_lock(t_p, ERTS_PROC_LOCK_TRACE);

        msgq = t_p->trace_msg_q;

        while (msgq != NULL) {
            if (msgq->receiver == receiver) {
                break;
            }
            msgq = msgq->next;
        }

#ifdef ERTS_ENABLE_LOCK_CHECK
        lc_locks = erts_proc_lc_my_proc_locks(rp);
        rp_locks |= lc_locks;
#endif
        if (ERTS_FORCE_ENIF_SEND_DELAY() || msgq ||
            rp_locks & ERTS_PROC_LOCK_MSGQ ||
            erts_smp_proc_trylock(rp, ERTS_PROC_LOCK_MSGQ) == EBUSY) {

            if (!msgq) {
                msgq = erts_alloc(ERTS_ALC_T_TRACE_MSG_QUEUE,
                                  sizeof(ErlTraceMessageQueue));
                msgq->receiver = receiver;
                msgq->first = mp;
                msgq->last = &mp->next;
                msgq->len = 1;

                /* Insert in linked list */
                msgq->next = t_p->trace_msg_q;
                t_p->trace_msg_q = msgq;

                erts_smp_proc_unlock(t_p, ERTS_PROC_LOCK_TRACE);

		erts_schedule_flush_trace_messages(t_p, 0);
            } else {
                msgq->len++;
                *msgq->last = mp;
                msgq->last = &mp->next;
                erts_smp_proc_unlock(t_p, ERTS_PROC_LOCK_TRACE);
            }
            goto done;
        } else {
            erts_smp_proc_unlock(t_p, ERTS_PROC_LOCK_TRACE);
            rp_locks &= ~ERTS_PROC_LOCK_TRACE;
            rp_locks |= ERTS_PROC_LOCK_MSGQ;
        }
    }
#endif /* ERTS_SMP */

    erts_queue_message(rp, rp_locks, mp, msg,
                       c_p ? c_p->common.id : am_undefined);

#ifdef ERTS_SMP
done:
    if (c_p == rp)
	rp_locks &= ~ERTS_PROC_LOCK_MAIN;
    if (rp_locks & ~lc_locks)
	erts_smp_proc_unlock(rp, rp_locks & ~lc_locks);
    if (c_p && (env->proc->static_flags & ERTS_STC_FLG_SHADOW_PROC))
	erts_smp_proc_unlock(c_p, ERTS_PROC_LOCK_MAIN);
#endif
    if (scheduler <= 0)
	erts_proc_dec_refc(rp);

    return 1;
}

int
enif_port_command(ErlNifEnv *env, const ErlNifPort* to_port,
                  ErlNifEnv *msg_env, ERL_NIF_TERM msg)
{
    int iflags = (erts_port_synchronous_ops
		  ? ERTS_PORT_SFLGS_INVALID_DRIVER_LOOKUP
		  : ERTS_PORT_SFLGS_INVALID_LOOKUP);
    int scheduler;
    Process *c_p;
    Port *prt;
    int res;

    if (!env)
	erts_exit(ERTS_ABORT_EXIT, "enif_port_command: env == NULL");

    execution_state(env, &c_p, &scheduler);

    if (!c_p)
	c_p = env->proc;

    if (scheduler > 0)
	prt = erts_port_lookup(to_port->port_id, iflags);
    else {
#ifdef ERTS_SMP
	if (ERTS_PROC_IS_EXITING(c_p))
	    return 0;
	prt = erts_thr_port_lookup(to_port->port_id, iflags);
#else
        erts_exit(ERTS_ABORT_EXIT,
		  "enif_port_command: called from non-scheduler "
                  "thread on non-SMP VM");
#endif
    }

    if (!prt)
	res = 0;
    else
        res = erts_port_output_async(prt, c_p->common.id, msg);

    if (scheduler <= 0)
	erts_port_dec_refc(prt);

    return res;
}

ERL_NIF_TERM enif_make_copy(ErlNifEnv* dst_env, ERL_NIF_TERM src_term)
{
    Uint sz;
    Eterm* hp;
    /*
     * No preserved sharing allowed as long as literals are also preserved.
     * Process independent environment can not be reached by purge.
     */
    sz = size_object(src_term);
    hp = alloc_heap(dst_env, sz);
    return copy_struct(src_term, sz, &hp, &MSO(dst_env->proc));
}


#ifdef DEBUG
static int is_offheap(const ErlOffHeap* oh)
{
    return oh->first != NULL;
}
#endif

ErlNifPid* enif_self(ErlNifEnv* caller_env, ErlNifPid* pid)
{
    if (caller_env->proc->common.id == ERTS_INVALID_PID)
        return NULL;
    pid->pid = caller_env->proc->common.id;
    return pid;
}

int enif_get_local_pid(ErlNifEnv* env, ERL_NIF_TERM term, ErlNifPid* pid)
{
    if (is_internal_pid(term)) {
        pid->pid=term;
        return 1;
    }
    return 0;
}

int enif_get_local_port(ErlNifEnv* env, ERL_NIF_TERM term, ErlNifPort* port)
{
    if (is_internal_port(term)) {
        port->port_id=term;
        return 1;
    }
    return 0;
}

int enif_is_atom(ErlNifEnv* env, ERL_NIF_TERM term)
{
    return is_atom(term);
}

int enif_is_binary(ErlNifEnv* env, ERL_NIF_TERM term)
{
    return is_binary(term) && (binary_bitsize(term) % 8 == 0);
}

int enif_is_empty_list(ErlNifEnv* env, ERL_NIF_TERM term)
{
    return is_nil(term);
}

int enif_is_fun(ErlNifEnv* env, ERL_NIF_TERM term)
{
    return is_fun(term);
}

int enif_is_pid(ErlNifEnv* env, ERL_NIF_TERM term)
{
    return is_pid(term);
}

int enif_is_port(ErlNifEnv* env, ERL_NIF_TERM term)
{
    return is_port(term);
}

int enif_is_ref(ErlNifEnv* env, ERL_NIF_TERM term)
{
    return is_ref(term);
}

int enif_is_tuple(ErlNifEnv* env, ERL_NIF_TERM term)
{
    return is_tuple(term);
}

int enif_is_list(ErlNifEnv* env, ERL_NIF_TERM term)
{
    return is_list(term) || is_nil(term);
}

int enif_is_exception(ErlNifEnv* env, ERL_NIF_TERM term)
{
    return env->exception_thrown && term == THE_NON_VALUE;
}

int enif_is_number(ErlNifEnv* env, ERL_NIF_TERM term)
{
    return is_number(term);
}

static ERTS_INLINE int is_proc_bound(ErlNifEnv* env)
{
    return env->mod_nif != NULL;
}

static void aligned_binary_dtor(struct enif_tmp_obj_t* obj)
{
    erts_free_aligned_binary_bytes_extra((byte*)obj, obj->allocator);
}

int enif_inspect_binary(ErlNifEnv* env, Eterm bin_term, ErlNifBinary* bin)
{
    ErtsAlcType_t allocator = is_proc_bound(env) ? ERTS_ALC_T_TMP : ERTS_ALC_T_NIF;
    union {
	struct enif_tmp_obj_t* tmp;
	byte* raw_ptr;
    }u;

    if (is_binary(bin_term)) {
        ProcBin *pb = (ProcBin*) binary_val(bin_term);
        if (pb->thing_word == HEADER_SUB_BIN) {
            ErlSubBin* sb = (ErlSubBin*) pb;
            pb = (ProcBin*) binary_val(sb->orig);
        }
        if (pb->thing_word == HEADER_PROC_BIN && pb->flags)
            erts_emasculate_writable_binary(pb);
    }
    u.tmp = NULL;
    bin->data = erts_get_aligned_binary_bytes_extra(bin_term, &u.raw_ptr, allocator,
						    sizeof(struct enif_tmp_obj_t));
    if (bin->data == NULL) {
	return 0;
    }
    if (u.tmp != NULL) {
	u.tmp->allocator = allocator;
	u.tmp->next = env->tmp_obj_list;
	u.tmp->dtor = &aligned_binary_dtor;
	env->tmp_obj_list = u.tmp;
    }
    bin->bin_term = bin_term;
    bin->size = binary_size(bin_term);
    bin->ref_bin = NULL;
    ADD_READONLY_CHECK(env, bin->data, bin->size);
    return 1;
}

static void tmp_alloc_dtor(struct enif_tmp_obj_t* obj)
{
    erts_free(obj->allocator,  obj);
}

int enif_inspect_iolist_as_binary(ErlNifEnv* env, Eterm term, ErlNifBinary* bin)
{
    struct enif_tmp_obj_t* tobj;
    ErtsAlcType_t allocator;
    ErlDrvSizeT sz;
    if (is_binary(term)) {
	return enif_inspect_binary(env,term,bin);
    }
    if (is_nil(term)) {
	bin->data = (unsigned char*) &bin->data; /* dummy non-NULL */
	bin->size = 0;
	bin->bin_term = THE_NON_VALUE;
	bin->ref_bin = NULL;
	return 1;
    }
    if (erts_iolist_size(term, &sz)) {
	return 0;
    }

    allocator = is_proc_bound(env) ? ERTS_ALC_T_TMP : ERTS_ALC_T_NIF;
    tobj = erts_alloc(allocator, sz + sizeof(struct enif_tmp_obj_t));
    tobj->allocator = allocator;
    tobj->next = env->tmp_obj_list;
    tobj->dtor = &tmp_alloc_dtor;
    env->tmp_obj_list = tobj;

    bin->data = (unsigned char*) &tobj[1]; 
    bin->size = sz;
    bin->bin_term = THE_NON_VALUE;
    bin->ref_bin = NULL;
    erts_iolist_to_buf(term, (char*) bin->data, sz);
    ADD_READONLY_CHECK(env, bin->data, bin->size); 
    return 1;
}

int enif_alloc_binary(size_t size, ErlNifBinary* bin)
{
    Binary* refbin;

    refbin = erts_bin_drv_alloc_fnf(size); /* BUGBUG: alloc type? */
    if (refbin == NULL) {
	return 0; /* The NIF must take action */
    }

    bin->size = size;
    bin->data = (unsigned char*) refbin->orig_bytes;
    bin->bin_term = THE_NON_VALUE;
    bin->ref_bin = refbin;
    return 1;
}

int enif_realloc_binary(ErlNifBinary* bin, size_t size)
{
    if (bin->ref_bin != NULL) {
	Binary* oldbin;
	Binary* newbin;
    
	oldbin = (Binary*) bin->ref_bin; 
	newbin = (Binary *) erts_bin_realloc_fnf(oldbin, size);
	if (!newbin) {
	    return 0;
	}    
	bin->ref_bin = newbin;
	bin->data = (unsigned char*) newbin->orig_bytes;
	bin->size = size;
    }
    else {
	unsigned char* old_data = bin->data;
	size_t cpy_sz = (size < bin->size ? size : bin->size);  
	enif_alloc_binary(size, bin);
	sys_memcpy(bin->data, old_data, cpy_sz); 
    }
    return 1;
}


void enif_release_binary(ErlNifBinary* bin)
{
    if (bin->ref_bin != NULL) {
	Binary* refbin = bin->ref_bin;
	ASSERT(bin->bin_term == THE_NON_VALUE);
        erts_bin_release(refbin);
    }
#ifdef DEBUG
    bin->data = NULL;
    bin->bin_term = THE_NON_VALUE;
    bin->ref_bin = NULL;
#endif
}

unsigned char* enif_make_new_binary(ErlNifEnv* env, size_t size,
				    ERL_NIF_TERM* termp)
{
    flush_env(env);
    *termp = new_binary(env->proc, NULL, size);
    cache_env(env);
    return binary_bytes(*termp);
}

int enif_term_to_binary(ErlNifEnv *dst_env, ERL_NIF_TERM term,
                        ErlNifBinary *bin)
{
    Sint size;
    byte *bp;
    Binary* refbin;

    size = erts_encode_ext_size(term);
    if (!enif_alloc_binary(size, bin))
        return 0;

    refbin = bin->ref_bin;

    bp = bin->data;

    erts_encode_ext(term, &bp);

    bin->size = bp - bin->data;
    refbin->orig_size = bin->size;

    ASSERT(bin->data + bin->size == bp);

    return 1;
}

size_t enif_binary_to_term(ErlNifEnv *dst_env,
                           const unsigned char* data,
                           size_t data_sz,
                           ERL_NIF_TERM *term,
                           ErlNifBinaryToTerm opts)
{
    Sint size;
    ErtsHeapFactory factory;
    byte *bp = (byte*) data;

    ERTS_CT_ASSERT(ERL_NIF_BIN2TERM_SAFE == ERTS_DIST_EXT_BTT_SAFE);

    if (opts & ~ERL_NIF_BIN2TERM_SAFE) {
        return 0;
    }
    if ((size = erts_decode_ext_size(bp, data_sz)) < 0)
        return 0;

    if (size > 0) {
        flush_env(dst_env);
        erts_factory_proc_prealloc_init(&factory, dst_env->proc, size);
    } else {
        erts_factory_dummy_init(&factory);
    }

    *term = erts_decode_ext(&factory, &bp, (Uint32)opts);

    if (is_non_value(*term)) {
        return 0;
    }
    erts_factory_close(&factory);
    cache_env(dst_env);

    ASSERT(bp > data);
    return bp - data;
}

int enif_is_identical(Eterm lhs, Eterm rhs)
{
    return EQ(lhs,rhs);
}

int enif_compare(Eterm lhs, Eterm rhs)
{
    Sint result = CMP(lhs,rhs);

    if (result < 0) {
        return -1;
    } else if (result > 0) {
        return 1;
    }

    return result;
}

ErlNifUInt64 enif_hash(ErlNifHash type, Eterm term, ErlNifUInt64 salt)
{
    switch (type) {
        case ERL_NIF_INTERNAL_HASH:
            return make_internal_hash(term, (Uint32) salt);
        case ERL_NIF_PHASH2:
            /* It appears that make_hash2 doesn't always react to seasoning
             * as well as it should. Therefore, let's make it ignore the salt
             * value and declare salted uses of phash2 as unsupported.
             */
            return make_hash2(term) & ((1 << 27) - 1);
        default:
            return 0;
    }
}

int enif_get_tuple(ErlNifEnv* env, Eterm tpl, int* arity, const Eterm** array)
{
    Eterm* ptr;
    if (is_not_tuple(tpl)) {
	return 0;
    }
    ptr = tuple_val(tpl);
    *arity = arityval(*ptr);
    *array = ptr+1;
    return 1;
}

int enif_get_string(ErlNifEnv *env, ERL_NIF_TERM list, char* buf, unsigned len,
		    ErlNifCharEncoding encoding)
{
    Eterm* listptr;
    int n = 0;

    ASSERT(encoding == ERL_NIF_LATIN1);
    if (len < 1) {
	return 0;
    }
    while (is_not_nil(list)) { 	    
	if (is_not_list(list)) {
	    buf[n] = '\0';
	    return 0;
	}
	listptr = list_val(list);
    
	if (!is_byte(*listptr)) {
	    buf[n] = '\0';
	    return 0;
	}
	buf[n++] = unsigned_val(*listptr);
	if (n >= len) {
	    buf[n-1] = '\0'; /* truncate */
	    return -len;
	}
	list = CDR(listptr);
    }
    buf[n] = '\0';
    return n + 1;
}

Eterm enif_make_binary(ErlNifEnv* env, ErlNifBinary* bin)
{
    if (bin->bin_term != THE_NON_VALUE) {
	return bin->bin_term;
    }
    else if (bin->ref_bin != NULL) {
	Binary* bptr = bin->ref_bin;
	ProcBin* pb;
	Eterm bin_term;
	
	/* !! Copy-paste from new_binary() !! */
	pb = (ProcBin *) alloc_heap(env, PROC_BIN_SIZE);
	pb->thing_word = HEADER_PROC_BIN;
	pb->size = bptr->orig_size;
	pb->next = MSO(env->proc).first;
	MSO(env->proc).first = (struct erl_off_heap_header*) pb;
	pb->val = bptr;
	pb->bytes = (byte*) bptr->orig_bytes;
	pb->flags = 0;
	
	OH_OVERHEAD(&(MSO(env->proc)), pb->size / sizeof(Eterm));
	bin_term = make_binary(pb);	
	if (erts_refc_read(&bptr->intern.refc, 1) == 1) {
	    /* Total ownership transfer */
	    bin->ref_bin = NULL;
	    bin->bin_term = bin_term;
	}
	return bin_term;
    }
    else {
	flush_env(env);
	bin->bin_term = new_binary(env->proc, bin->data, bin->size);
	cache_env(env);
	return bin->bin_term;
    }
}

Eterm enif_make_sub_binary(ErlNifEnv* env, ERL_NIF_TERM bin_term,
			   size_t pos, size_t size)
{
    ErlSubBin* sb;
    Eterm orig;
    Uint offset, bit_offset, bit_size; 
#ifdef DEBUG
    size_t src_size;

    ASSERT(is_binary(bin_term));
    src_size = binary_size(bin_term);
    ASSERT(pos <= src_size);
    ASSERT(size <= src_size);
    ASSERT(pos + size <= src_size);   
#endif
    sb = (ErlSubBin*) alloc_heap(env, ERL_SUB_BIN_SIZE);
    ERTS_GET_REAL_BIN(bin_term, orig, offset, bit_offset, bit_size);
    sb->thing_word = HEADER_SUB_BIN;
    sb->size = size;
    sb->offs = offset + pos;
    sb->orig = orig;
    sb->bitoffs = bit_offset;
    sb->bitsize = 0;
    sb->is_writable = 0;
    return make_binary(sb);
}


Eterm enif_make_badarg(ErlNifEnv* env)
{
    return enif_raise_exception(env, am_badarg);
}

Eterm enif_raise_exception(ErlNifEnv* env, ERL_NIF_TERM reason)
{
    env->exception_thrown = 1;
    env->proc->fvalue = reason;
    BIF_ERROR(env->proc, EXC_ERROR);
}

int enif_has_pending_exception(ErlNifEnv* env, ERL_NIF_TERM* reason)
{
    if (env->exception_thrown && reason != NULL)
	*reason = env->proc->fvalue;
    return env->exception_thrown;
}

int enif_get_atom(ErlNifEnv* env, Eterm atom, char* buf, unsigned len,
		  ErlNifCharEncoding encoding)
{
    Atom* ap;
    ASSERT(encoding == ERL_NIF_LATIN1);
    if (is_not_atom(atom) || len==0) {
	return 0;
    }
    ap = atom_tab(atom_val(atom));

    if (ap->latin1_chars < 0 || ap->latin1_chars >= len) {
	return 0;
    }
    if (ap->latin1_chars == ap->len) {
	sys_memcpy(buf, ap->name, ap->len);
    }
    else {
	int dlen = erts_utf8_to_latin1((byte*)buf, ap->name, ap->len);
	ASSERT(dlen == ap->latin1_chars); (void)dlen;
    }
    buf[ap->latin1_chars] = '\0';
    return ap->latin1_chars + 1;
}

int enif_get_int(ErlNifEnv* env, Eterm term, int* ip)
{
#if SIZEOF_INT ==  ERTS_SIZEOF_ETERM
    return term_to_Sint(term, (Sint*)ip);
#elif (SIZEOF_LONG ==  ERTS_SIZEOF_ETERM) || \
  (SIZEOF_LONG_LONG ==  ERTS_SIZEOF_ETERM)
    Sint i;
    if (!term_to_Sint(term, &i) || i < INT_MIN || i > INT_MAX) {
	return 0;
    }
    *ip = (int) i;
    return 1;
#else
#  error Unknown word size 
#endif     
}

int enif_get_uint(ErlNifEnv* env, Eterm term, unsigned* ip)
{
#if SIZEOF_INT == ERTS_SIZEOF_ETERM
    return term_to_Uint(term, (Uint*)ip);
#elif (SIZEOF_LONG == ERTS_SIZEOF_ETERM) || \
  (SIZEOF_LONG_LONG ==  ERTS_SIZEOF_ETERM)
    Uint i;
    if (!term_to_Uint(term, &i) || i > UINT_MAX) {
	return 0;
    }
    *ip = (unsigned) i;
    return 1;
#endif     
}

int enif_get_long(ErlNifEnv* env, Eterm term, long* ip)
{
#if SIZEOF_LONG == ERTS_SIZEOF_ETERM
    return term_to_Sint(term, ip);
#elif SIZEOF_LONG == 8
    return term_to_Sint64(term, ip);
#elif SIZEOF_LONG == SIZEOF_INT
    int tmp,ret;
    ret = enif_get_int(env,term,&tmp);
    if (ret) {
      *ip = (long) tmp;
    }
    return ret;
#else
#  error Unknown long word size 
#endif     
}

int enif_get_ulong(ErlNifEnv* env, Eterm term, unsigned long* ip)
{
#if SIZEOF_LONG == ERTS_SIZEOF_ETERM
    return term_to_Uint(term, ip);
#elif SIZEOF_LONG == 8
    return term_to_Uint64(term, ip);
#elif SIZEOF_LONG == SIZEOF_INT
    int ret;
    unsigned int tmp;
    ret = enif_get_uint(env,term,&tmp);
    if (ret) {
      *ip = (unsigned long) tmp;
    }
    return ret;
#else
#  error Unknown long word size 
#endif     
}

#if HAVE_INT64 && SIZEOF_LONG != 8
int enif_get_int64(ErlNifEnv* env, ERL_NIF_TERM term, ErlNifSInt64* ip)
{
    return term_to_Sint64(term, ip);
}

int enif_get_uint64(ErlNifEnv* env, ERL_NIF_TERM term, ErlNifUInt64* ip)
{
    return term_to_Uint64(term, ip);
}
#endif /* HAVE_INT64 && SIZEOF_LONG != 8 */

int enif_get_double(ErlNifEnv* env, ERL_NIF_TERM term, double* dp)
{
    FloatDef f;
    if (is_not_float(term)) {
	return 0;
    }
    GET_DOUBLE(term, f);
    *dp = f.fd;
    return 1;
}

int enif_get_atom_length(ErlNifEnv* env, Eterm atom, unsigned* len,
			 ErlNifCharEncoding enc)
{
    Atom* ap;
    ASSERT(enc == ERL_NIF_LATIN1);
    if (is_not_atom(atom)) return 0;
    ap = atom_tab(atom_val(atom));
    if (ap->latin1_chars < 0) {
	return 0;
    }
    *len = ap->latin1_chars;
    return 1;
}

int enif_get_list_cell(ErlNifEnv* env, Eterm term, Eterm* head, Eterm* tail)
{
    Eterm* val;
    if (is_not_list(term)) return 0;
    val = list_val(term);
    *head = CAR(val);
    *tail = CDR(val);
    return 1;
}

int enif_get_list_length(ErlNifEnv* env, Eterm term, unsigned* len)
{
    Sint i;
    Uint u;

    if ((i = erts_list_length(term)) < 0) return 0;
    u = (Uint)i;
    if ((unsigned)u != u) return 0;
    *len = u;
    return 1;
}

ERL_NIF_TERM enif_make_int(ErlNifEnv* env, int i)
{
#if SIZEOF_INT == ERTS_SIZEOF_ETERM
    return IS_SSMALL(i) ? make_small(i) : small_to_big(i,alloc_heap(env,2));
#elif (SIZEOF_LONG == ERTS_SIZEOF_ETERM) || \
  (SIZEOF_LONG_LONG == ERTS_SIZEOF_ETERM)
    return make_small(i);
#endif
}

ERL_NIF_TERM enif_make_uint(ErlNifEnv* env, unsigned i)
{
#if SIZEOF_INT == ERTS_SIZEOF_ETERM
    return IS_USMALL(0,i) ? make_small(i) : uint_to_big(i,alloc_heap(env,2));
#elif (SIZEOF_LONG ==  ERTS_SIZEOF_ETERM) || \
  (SIZEOF_LONG_LONG ==  ERTS_SIZEOF_ETERM)
    return make_small(i);
#endif
}

ERL_NIF_TERM enif_make_long(ErlNifEnv* env, long i)
{
    if (IS_SSMALL(i)) {
	return make_small(i);
    }
#if SIZEOF_LONG == ERTS_SIZEOF_ETERM
    return small_to_big(i, alloc_heap(env,2));
#elif SIZEOF_LONG_LONG ==  ERTS_SIZEOF_ETERM
    return make_small(i);
#elif SIZEOF_LONG == 8
    ensure_heap(env,3);
    return erts_sint64_to_big(i, &env->hp);
#endif
}

ERL_NIF_TERM enif_make_ulong(ErlNifEnv* env, unsigned long i)
{
    if (IS_USMALL(0,i)) {
	return make_small(i);
    }
#if SIZEOF_LONG == ERTS_SIZEOF_ETERM
    return uint_to_big(i,alloc_heap(env,2));
#elif SIZEOF_LONG_LONG ==  ERTS_SIZEOF_ETERM
    return make_small(i);
#elif SIZEOF_LONG == 8
    ensure_heap(env,3);
    return erts_uint64_to_big(i, &env->hp);    
#endif
}

#if HAVE_INT64 && SIZEOF_LONG != 8
ERL_NIF_TERM enif_make_int64(ErlNifEnv* env, ErlNifSInt64 i)
{
    Uint* hp;
    Uint need = 0;
    erts_bld_sint64(NULL, &need, i);
    hp = alloc_heap(env, need);
    return erts_bld_sint64(&hp, NULL, i);
}

ERL_NIF_TERM enif_make_uint64(ErlNifEnv* env, ErlNifUInt64 i)
{
    Uint* hp;
    Uint need = 0;
    erts_bld_uint64(NULL, &need, i);
    hp = alloc_heap(env, need);
    return erts_bld_uint64(&hp, NULL, i);
}
#endif /* HAVE_INT64 && SIZEOF_LONG != 8 */

ERL_NIF_TERM enif_make_double(ErlNifEnv* env, double d)
{
    Eterm* hp;
    FloatDef f;

    if (!erts_isfinite(d))
        return enif_make_badarg(env);
    hp = alloc_heap(env,FLOAT_SIZE_OBJECT);
    f.fd = d;
    PUT_DOUBLE(f, hp);
    return make_float(hp);
}

ERL_NIF_TERM enif_make_atom(ErlNifEnv* env, const char* name)
{
    return enif_make_atom_len(env, name, sys_strlen(name));
}

ERL_NIF_TERM enif_make_atom_len(ErlNifEnv* env, const char* name, size_t len)
{
    if (len > MAX_ATOM_CHARACTERS)
        return enif_make_badarg(env);
    return erts_atom_put((byte*)name, len, ERTS_ATOM_ENC_LATIN1, 1);
}

int enif_make_existing_atom(ErlNifEnv* env, const char* name, ERL_NIF_TERM* atom,
			    ErlNifCharEncoding enc)
{
    return enif_make_existing_atom_len(env, name, sys_strlen(name), atom, enc);
}

int enif_make_existing_atom_len(ErlNifEnv* env, const char* name, size_t len,
				ERL_NIF_TERM* atom, ErlNifCharEncoding encoding)
{
    ASSERT(encoding == ERL_NIF_LATIN1);
    if (len > MAX_ATOM_CHARACTERS)
        return 0;
    return erts_atom_get(name, len, atom, ERTS_ATOM_ENC_LATIN1);
}

ERL_NIF_TERM enif_make_tuple(ErlNifEnv* env, unsigned cnt, ...)
{
#ifdef ERTS_NIF_ASSERT_IN_ENV
    int nr = 0;
#endif
    Eterm* hp = alloc_heap(env,cnt+1);
    Eterm ret = make_tuple(hp);
    va_list ap;

    *hp++ = make_arityval(cnt);
    va_start(ap,cnt);
    while (cnt--) {
        Eterm elem = va_arg(ap,Eterm);
        ASSERT_IN_ENV(env, elem, ++nr, "tuple");
	*hp++ = elem;
    }
    va_end(ap);
    return ret;
}

ERL_NIF_TERM enif_make_tuple_from_array(ErlNifEnv* env, const ERL_NIF_TERM arr[], unsigned cnt)
{
#ifdef ERTS_NIF_ASSERT_IN_ENV
    int nr = 0;
#endif
    Eterm* hp = alloc_heap(env,cnt+1);
    Eterm ret = make_tuple(hp);
    const Eterm* src = arr;

    *hp++ = make_arityval(cnt);
    while (cnt--) {
        ASSERT_IN_ENV(env, *src, ++nr, "tuple");
	*hp++ = *src++;	   
    }
    return ret;
}

ERL_NIF_TERM enif_make_list_cell(ErlNifEnv* env, Eterm car, Eterm cdr)
{
    Eterm* hp = alloc_heap(env,2);
    Eterm ret = make_list(hp);

    ASSERT_IN_ENV(env, car, 0, "head of list cell");
    ASSERT_IN_ENV(env, cdr, 0, "tail of list cell");
    CAR(hp) = car;
    CDR(hp) = cdr;
    return ret;
}

ERL_NIF_TERM enif_make_list(ErlNifEnv* env, unsigned cnt, ...)
{
    if (cnt == 0) {
	return NIL;
    }
    else {
#ifdef ERTS_NIF_ASSERT_IN_ENV
        int nr = 0;
#endif
	Eterm* hp = alloc_heap(env,cnt*2);
	Eterm ret = make_list(hp);
	Eterm* last = &ret;
	va_list ap;

	va_start(ap,cnt);
	while (cnt--) {
            Eterm term = va_arg(ap,Eterm);
	    *last = make_list(hp);
            ASSERT_IN_ENV(env, term, ++nr, "list");
	    *hp = term;
	    last = ++hp;
	    ++hp;
	}
	va_end(ap);
	*last = NIL;
	return ret;
    }
}

ERL_NIF_TERM enif_make_list_from_array(ErlNifEnv* env, const ERL_NIF_TERM arr[], unsigned cnt)
{
#ifdef ERTS_NIF_ASSERT_IN_ENV
    int nr = 0;
#endif
    Eterm* hp = alloc_heap(env,cnt*2);
    Eterm ret = make_list(hp);
    Eterm* last = &ret;
    const Eterm* src = arr;

    while (cnt--) {
        Eterm term = *src++;
	*last = make_list(hp);
        ASSERT_IN_ENV(env, term, ++nr, "list");
	*hp = term;
	last = ++hp;
	++hp;
    }
    *last = NIL;
    return ret;
}

ERL_NIF_TERM enif_make_string(ErlNifEnv* env, const char* string,
			      ErlNifCharEncoding encoding)
{
    return enif_make_string_len(env, string, sys_strlen(string), encoding);
}

ERL_NIF_TERM enif_make_string_len(ErlNifEnv* env, const char* string,
				  size_t len, ErlNifCharEncoding encoding)
{
    Eterm* hp = alloc_heap(env,len*2);
    ASSERT(encoding == ERL_NIF_LATIN1);
    return erts_bld_string_n(&hp,NULL,string,len);
}

ERL_NIF_TERM enif_make_ref(ErlNifEnv* env)
{
    Eterm* hp = alloc_heap(env, ERTS_REF_THING_SIZE);
    return erts_make_ref_in_buffer(hp);
}

void enif_system_info(ErlNifSysInfo *sip, size_t si_size)
{
    driver_system_info(sip, si_size);
}

int enif_make_reverse_list(ErlNifEnv* env, ERL_NIF_TERM term, ERL_NIF_TERM *list)
{
    Eterm *listptr, ret, *hp;

    ret = NIL;

    while (is_not_nil(term)) {
	if (is_not_list(term)) {
	    return 0;
	}
	hp = alloc_heap(env, 2);
	listptr = list_val(term);
	ret = CONS(hp, CAR(listptr), ret);
	term = CDR(listptr);
    }
    *list = ret;
    return 1;
}

int enif_is_current_process_alive(ErlNifEnv* env)
{
    Process *c_p;
    int scheduler;

    execution_state(env, &c_p, &scheduler);

    if (!c_p)
	erts_exit(ERTS_ABORT_EXIT,
		  "enif_is_current_process_alive: "
                  "Invalid environment");

    if (!scheduler)
	erts_exit(ERTS_ABORT_EXIT, "enif_is_current_process_alive: "
		  "called from non-scheduler thread");

    return !ERTS_PROC_IS_EXITING(c_p);
}

int enif_is_process_alive(ErlNifEnv* env, ErlNifPid *proc)
{
    int scheduler;

    execution_state(env, NULL, &scheduler);

    if (scheduler > 0)
	return !!erts_proc_lookup(proc->pid);
    else {
#ifdef ERTS_SMP
	Process* rp = erts_pid2proc_opt(NULL, 0, proc->pid, 0,
					ERTS_P2P_FLG_INC_REFC);
	if (rp)
	    erts_proc_dec_refc(rp);
	return !!rp;
#else
	erts_exit(ERTS_ABORT_EXIT, "enif_is_process_alive: "
		  "called from non-scheduler thread "
                  "in non-smp emulator");
	return 0;
#endif
    }
}

int enif_is_port_alive(ErlNifEnv *env, ErlNifPort *port)
{
    int scheduler;
    Uint32 iflags = (erts_port_synchronous_ops
		     ? ERTS_PORT_SFLGS_INVALID_DRIVER_LOOKUP
		     : ERTS_PORT_SFLGS_INVALID_LOOKUP);

    execution_state(env, NULL, &scheduler);

    if (scheduler > 0)
	return !!erts_port_lookup(port->port_id, iflags);
    else {
#ifdef ERTS_SMP
	Port *prt = erts_thr_port_lookup(port->port_id, iflags);
	if (prt)
	    erts_port_dec_refc(prt);
	return !!prt;
#else
	erts_exit(ERTS_ABORT_EXIT, "enif_is_port_alive: "
		  "called from non-scheduler thread "
                  "in non-smp emulator");
	return 0;
#endif
    }
}

ERL_NIF_TERM
enif_now_time(ErlNifEnv *env)
{
    Uint mega, sec, micro;
    Eterm *hp;
    get_now(&mega, &sec, &micro);
    hp = alloc_heap(env, 4);
    return TUPLE3(hp, make_small(mega), make_small(sec), make_small(micro));
}

ERL_NIF_TERM
enif_cpu_time(ErlNifEnv *env)
{
#ifdef HAVE_ERTS_NOW_CPU
    Uint mega, sec, micro;
    Eterm *hp;
    erts_get_now_cpu(&mega, &sec, &micro);
    hp = alloc_heap(env, 4);
    return TUPLE3(hp, make_small(mega), make_small(sec), make_small(micro));
#else
    return enif_make_badarg(env);
#endif
}

ERL_NIF_TERM
enif_make_unique_integer(ErlNifEnv *env, ErlNifUniqueInteger properties)
{
    int monotonic = properties & ERL_NIF_UNIQUE_MONOTONIC;
    int positive = properties & ERL_NIF_UNIQUE_POSITIVE;
    Eterm *hp;
    Uint hsz;

    if (monotonic) {
        Sint64 raw_unique = erts_raw_get_unique_monotonic_integer();
        hsz = erts_raw_unique_monotonic_integer_heap_size(raw_unique, positive);
        hp = alloc_heap(env, hsz);
        return erts_raw_make_unique_monotonic_integer_value(&hp, raw_unique, positive);
    } else {
        Uint64 raw_unique[ERTS_UNIQUE_INT_RAW_VALUES];
        erts_raw_get_unique_integer(raw_unique);
        hsz = erts_raw_unique_integer_heap_size(raw_unique, positive);
        hp = alloc_heap(env, hsz);
        return erts_raw_make_unique_integer(&hp, raw_unique, positive);
    }
}

ErlNifMutex* enif_mutex_create(char *name) { return erl_drv_mutex_create(name); }
void enif_mutex_destroy(ErlNifMutex *mtx) {  erl_drv_mutex_destroy(mtx); }
int enif_mutex_trylock(ErlNifMutex *mtx) { return erl_drv_mutex_trylock(mtx); }
void enif_mutex_lock(ErlNifMutex *mtx) { erl_drv_mutex_lock(mtx); }
void enif_mutex_unlock(ErlNifMutex *mtx) { erl_drv_mutex_unlock(mtx); }
ErlNifCond* enif_cond_create(char *name) { return erl_drv_cond_create(name); }
void enif_cond_destroy(ErlNifCond *cnd) { erl_drv_cond_destroy(cnd); }
void enif_cond_signal(ErlNifCond *cnd) { erl_drv_cond_signal(cnd); }
void enif_cond_broadcast(ErlNifCond *cnd) { erl_drv_cond_broadcast(cnd); }
void enif_cond_wait(ErlNifCond *cnd, ErlNifMutex *mtx) { erl_drv_cond_wait(cnd,mtx); }
ErlNifRWLock* enif_rwlock_create(char *name) { return erl_drv_rwlock_create(name); }
void enif_rwlock_destroy(ErlNifRWLock *rwlck) { erl_drv_rwlock_destroy(rwlck); }
int enif_rwlock_tryrlock(ErlNifRWLock *rwlck) { return erl_drv_rwlock_tryrlock(rwlck); }
void enif_rwlock_rlock(ErlNifRWLock *rwlck) { erl_drv_rwlock_rlock(rwlck); }
void enif_rwlock_runlock(ErlNifRWLock *rwlck) { erl_drv_rwlock_runlock(rwlck); }
int enif_rwlock_tryrwlock(ErlNifRWLock *rwlck) { return erl_drv_rwlock_tryrwlock(rwlck); }
void enif_rwlock_rwlock(ErlNifRWLock *rwlck) { erl_drv_rwlock_rwlock(rwlck); }
void enif_rwlock_rwunlock(ErlNifRWLock *rwlck) { erl_drv_rwlock_rwunlock(rwlck); }
int enif_tsd_key_create(char *name, ErlNifTSDKey *key) { return erl_drv_tsd_key_create(name,key); }
void enif_tsd_key_destroy(ErlNifTSDKey key) { erl_drv_tsd_key_destroy(key); }
void enif_tsd_set(ErlNifTSDKey key, void *data) { erl_drv_tsd_set(key,data); }
void* enif_tsd_get(ErlNifTSDKey key) { return erl_drv_tsd_get(key); }
ErlNifThreadOpts* enif_thread_opts_create(char *name) { return (ErlNifThreadOpts*) erl_drv_thread_opts_create(name); }
void enif_thread_opts_destroy(ErlNifThreadOpts *opts) { erl_drv_thread_opts_destroy((ErlDrvThreadOpts*)opts); }
int enif_thread_create(char *name, ErlNifTid *tid, void* (*func)(void *),
		       void *args, ErlNifThreadOpts *opts) {
    return erl_drv_thread_create(name,tid,func,args,(ErlDrvThreadOpts*)opts);
}
ErlNifTid enif_thread_self(void) { return erl_drv_thread_self(); }
int enif_equal_tids(ErlNifTid tid1, ErlNifTid tid2) { return erl_drv_equal_tids(tid1,tid2); }
void enif_thread_exit(void *resp) { erl_drv_thread_exit(resp); }
int enif_thread_join(ErlNifTid tid, void **respp) { return erl_drv_thread_join(tid,respp); }
int enif_getenv(const char *key, char *value, size_t *value_size) { return erl_drv_getenv(key, value, value_size); }

ErlNifTime enif_monotonic_time(ErlNifTimeUnit time_unit)
{
    return (ErlNifTime) erts_napi_monotonic_time((int) time_unit);
}

ErlNifTime enif_time_offset(ErlNifTimeUnit time_unit)
{
    return (ErlNifTime) erts_napi_time_offset((int) time_unit);
}

ErlNifTime
enif_convert_time_unit(ErlNifTime val,
		       ErlNifTimeUnit from,
		       ErlNifTimeUnit to)
{
    return (ErlNifTime) erts_napi_convert_time_unit((ErtsMonotonicTime) val,
						    (int) from,
						    (int) to);
}

int enif_fprintf(void* filep, const char* format, ...) 
{ 
    int ret;
    va_list arglist;
    va_start(arglist, format);
    ret = erts_vfprintf((FILE*)filep, format, arglist);
    va_end(arglist);
    return ret;
}    

int enif_snprintf(char *buffer, size_t size, const char* format, ...) 
{ 
    int ret;
    va_list arglist;
    va_start(arglist, format);
    ret = erts_vsnprintf(buffer, size, format, arglist);
    va_end(arglist);
    return ret;
}

/***********************************************************
 **       Memory managed (GC'ed) "resource" objects       **
 ***********************************************************/

/* dummy node in circular list */
struct enif_resource_type_t resource_type_list; 

static ErlNifResourceType* find_resource_type(Eterm module, Eterm name)
{
    ErlNifResourceType* type;
    for (type = resource_type_list.next;
	 type != &resource_type_list;
	 type = type->next) {

	if (type->module == module && type->name == name) {
	    return type;
	}
    }
    return NULL;
}

#define in_area(ptr,start,nbytes) \
    ((UWord)((char*)(ptr) - (char*)(start)) < (nbytes))


static void close_lib(struct erl_module_nif* lib)
{
    ASSERT(lib != NULL);
    ASSERT(lib->handle != NULL);
    ASSERT(erts_refc_read(&lib->rt_dtor_cnt,0) == 0);

    if (lib->entry.unload != NULL) {
	struct enif_msg_environment_t msg_env;
        pre_nif_noproc(&msg_env, lib, NULL);
	lib->entry.unload(&msg_env.env, lib->priv_data);
        post_nif_noproc(&msg_env);
    }
    if (!erts_is_static_nif(lib->handle))
      erts_sys_ddll_close(lib->handle);
    lib->handle = NULL;
}

static void steal_resource_type(ErlNifResourceType* type)
{
    struct erl_module_nif* lib = type->owner;

    if (type->dtor != NULL
	&& erts_refc_dectest(&lib->rt_dtor_cnt, 0) == 0
	&& lib->mod == NULL) {
	/* last type with destructor gone, close orphan lib */

	close_lib(lib);
    }
    if (erts_refc_dectest(&lib->rt_cnt, 0) == 0
	&& lib->mod == NULL) {
	erts_free(ERTS_ALC_T_NIF, lib);
    }
}

/* The opened_rt_list is used by enif_open_resource_type()
 * in order to rollback "creates" and "take-overs" in case the load fails.
 */
struct opened_resource_type
{
    struct opened_resource_type* next;

    ErlNifResourceFlags op;
    ErlNifResourceType* type;
    ErlNifResourceTypeInit new_callbacks;
};
static struct opened_resource_type* opened_rt_list = NULL;

static
ErlNifResourceType* open_resource_type(ErlNifEnv* env,
                                       const char* name_str,
                                       const ErlNifResourceTypeInit* init,
                                       ErlNifResourceFlags flags,
                                       ErlNifResourceFlags* tried,
                                       size_t sizeof_init)
{
    ErlNifResourceType* type = NULL;
    ErlNifResourceFlags op = flags;
    Eterm module_am, name_am;

    ASSERT(erts_smp_thr_progress_is_blocking());
    module_am = make_atom(env->mod_nif->mod->module);
    name_am = enif_make_atom(env, name_str);

    type = find_resource_type(module_am, name_am);
    if (type == NULL) {
	if (flags & ERL_NIF_RT_CREATE) {
	    type = erts_alloc(ERTS_ALC_T_NIF,
			      sizeof(struct enif_resource_type_t));
	    type->module = module_am;
	    type->name = name_am;
	    erts_refc_init(&type->refc, 1);
	    op = ERL_NIF_RT_CREATE;
	#ifdef DEBUG
	    type->dtor = (void*)1;
	    type->owner = (void*)2;
	    type->prev = (void*)3;
	    type->next = (void*)4;
	#endif
	}
    }
    else {
	if (flags & ERL_NIF_RT_TAKEOVER) {
	    op = ERL_NIF_RT_TAKEOVER;
	}
	else {
	    type = NULL;
	}
    }
    if (type != NULL) {
	struct opened_resource_type* ort = erts_alloc(ERTS_ALC_T_TMP,
						sizeof(struct opened_resource_type));
	ort->op = op;
	ort->type = type;
        sys_memzero(&ort->new_callbacks, sizeof(ErlNifResourceTypeInit));
        ASSERT(sizeof_init > 0 && sizeof_init <= sizeof(ErlNifResourceTypeInit));
        sys_memcpy(&ort->new_callbacks, init, sizeof_init);
	ort->next = opened_rt_list;
	opened_rt_list = ort;
    }
    if (tried != NULL) {
	*tried = op;
    }
    return type;
}

ErlNifResourceType*
enif_open_resource_type(ErlNifEnv* env,
                        const char* module_str,
                        const char* name_str,
			ErlNifResourceDtor* dtor,
			ErlNifResourceFlags flags,
			ErlNifResourceFlags* tried)
{
    ErlNifResourceTypeInit init =  {dtor, NULL};
    ASSERT(module_str == NULL); /* for now... */
    return open_resource_type(env, name_str, &init, flags, tried,
                              sizeof(init));
}

ErlNifResourceType*
enif_open_resource_type_x(ErlNifEnv* env,
                          const char* name_str,
                          const ErlNifResourceTypeInit* init,
                          ErlNifResourceFlags flags,
                          ErlNifResourceFlags* tried)
{
    return open_resource_type(env, name_str, init, flags, tried,
                              env->mod_nif->entry.sizeof_ErlNifResourceTypeInit);
}

static void commit_opened_resource_types(struct erl_module_nif* lib)
{
    while (opened_rt_list) {
	struct opened_resource_type* ort = opened_rt_list;

	ErlNifResourceType* type = ort->type;

	if (ort->op == ERL_NIF_RT_CREATE) {
	    type->prev = &resource_type_list;
	    type->next = resource_type_list.next;
	    type->next->prev = type;
	    type->prev->next = type;
	}
	else { /* ERL_NIF_RT_TAKEOVER */
	    steal_resource_type(type);
	}

	type->owner = lib;
	type->dtor = ort->new_callbacks.dtor;
        type->stop = ort->new_callbacks.stop;
        type->down = ort->new_callbacks.down;

	if (type->dtor != NULL) {
	    erts_refc_inc(&lib->rt_dtor_cnt, 1);
	}
	erts_refc_inc(&lib->rt_cnt, 1);

	opened_rt_list = ort->next;
	erts_free(ERTS_ALC_T_TMP, ort);
    }
}

static void rollback_opened_resource_types(void)
{
    while (opened_rt_list) {
	struct opened_resource_type* ort = opened_rt_list;

	if (ort->op == ERL_NIF_RT_CREATE) {
	    erts_free(ERTS_ALC_T_NIF, ort->type);
	}

	opened_rt_list = ort->next;
	erts_free(ERTS_ALC_T_TMP, ort);
    }
}

struct destroy_monitor_ctx
{
    ErtsResource* resource;
    int exiting_procs;
    int scheduler;
};

static void destroy_one_monitor(ErtsMonitor* mon, void* context)
{
    struct destroy_monitor_ctx* ctx = (struct destroy_monitor_ctx*) context;
    Process* rp;
    ErtsMonitor *rmon = NULL;
    int is_exiting;

    ASSERT(mon->type == MON_ORIGIN);
    ASSERT(is_internal_pid(mon->u.pid));
    ASSERT(is_internal_ref(mon->ref));

    if (ctx->scheduler > 0) { /* Normal scheduler */
        rp = erts_proc_lookup(mon->u.pid);
    }
    else {
#ifdef ERTS_SMP
        rp = erts_proc_lookup_inc_refc(mon->u.pid);
#else
        ASSERT(!"nif monitor destruction in non-scheduler thread");
        rp = NULL;
#endif
    }

    if (!rp) {
        is_exiting = 1;
    }
    if (rp) {
        erts_smp_proc_lock(rp, ERTS_PROC_LOCK_LINK);
        if (ERTS_PROC_IS_EXITING(rp)) {
            is_exiting = 1;
        } else {
            rmon = erts_remove_monitor(&ERTS_P_MONITORS(rp), mon->ref);
            ASSERT(rmon);
            is_exiting = 0;
        }
        erts_smp_proc_unlock(rp, ERTS_PROC_LOCK_LINK);
#ifdef ERTS_SMP
        if (ctx->scheduler <= 0)
            erts_proc_dec_refc(rp);
#endif
    }
    if (is_exiting) {
        ctx->resource->monitors->pending_failed_fire++;
    }

    /* ToDo: Delay destruction after monitor_locks */
    if (rmon) {
        ASSERT(rmon->type == MON_NIF_TARGET);
        ASSERT(rmon->u.resource == ctx->resource);
        erts_destroy_monitor(rmon);
    }
    erts_destroy_monitor(mon);
}

static void destroy_all_monitors(ErtsMonitor* monitors, ErtsResource* resource)
{
    struct destroy_monitor_ctx ctx;

    execution_state(NULL, NULL, &ctx.scheduler);

    ctx.resource = resource;
    erts_sweep_monitors(monitors, &destroy_one_monitor, &ctx);
}


#ifdef ERTS_SMP
#  define NIF_RESOURCE_DTOR &nif_resource_dtor
#else
#  define NIF_RESOURCE_DTOR &nosmp_nif_resource_dtor_prologue

/*
 * NO-SMP: Always run resource destructor on scheduler thread 
 *         as we may have to remove process monitors.
 */
static int nif_resource_dtor(Binary*);

static void nosmp_nif_resource_dtor_scheduled(void* vbin)
{
    erts_bin_free((Binary*)vbin);
}

static int nosmp_nif_resource_dtor_prologue(Binary* bin)
{
    if (is_scheduler()) {
        return nif_resource_dtor(bin);
    }
    else {
        erts_schedule_misc_aux_work(1, nosmp_nif_resource_dtor_scheduled, bin);
        return 0; /* do not free */
    }
}

#endif /* !ERTS_SMP */

static int nif_resource_dtor(Binary* bin)
{
    ErtsResource* resource = (ErtsResource*) ERTS_MAGIC_BIN_UNALIGNED_DATA(bin);
    ErlNifResourceType* type = resource->type;
    ASSERT(ERTS_MAGIC_BIN_DESTRUCTOR(bin) == NIF_RESOURCE_DTOR);

    if (resource->monitors) {
        ErtsResourceMonitors* rm = resource->monitors;

        ASSERT(type->down);
        erts_smp_mtx_lock(&rm->lock);
        ASSERT(erts_refc_read(&bin->intern.refc, 0) == 0);
        if (rm->root) {
            ASSERT(!rm->is_dying);
            destroy_all_monitors(rm->root, resource);
            rm->root = NULL;
        }
        if (rm->pending_failed_fire) {
            /*
             * Resource death struggle prolonged to serve exiting process(es).
             * Destructor will be called again when last exiting process
             * tries to fire its MON_NIF_TARGET monitor (and fails).
             *
             * This resource is doomed. It has no "real" references and
             * should get not get called upon to do anything except the
             * final destructor call.
             *
             * We keep refc at 0 and use a separate counter for exiting
             * processes to avoid resource getting revived by "dec_term".
             */
            ASSERT(!rm->is_dying);
            rm->is_dying = 1;
            erts_smp_mtx_unlock(&rm->lock);
            return 0;
        }
        erts_smp_mtx_unlock(&rm->lock);
        erts_smp_mtx_destroy(&rm->lock);
    }

    if (type->dtor != NULL) {
        struct enif_msg_environment_t msg_env;
        pre_nif_noproc(&msg_env, type->owner, NULL);
	type->dtor(&msg_env.env, resource->data);
        post_nif_noproc(&msg_env);
    }
    if (erts_refc_dectest(&type->refc, 0) == 0) {
	ASSERT(type->next == NULL);
	ASSERT(type->owner != NULL);
	ASSERT(type->owner->mod == NULL);
	steal_resource_type(type);
	erts_free(ERTS_ALC_T_NIF, type);
    }
    return 1;
}

void erts_resource_stop(ErtsResource* resource, ErlNifEvent e,
                        int is_direct_call)
{
    struct enif_msg_environment_t msg_env;
    ASSERT(resource->type->stop);
    pre_nif_noproc(&msg_env, resource->type->owner, NULL);
    resource->type->stop(&msg_env.env, resource->data, e, is_direct_call);
    post_nif_noproc(&msg_env);
}

void erts_fire_nif_monitor(ErtsResource* resource, Eterm pid, Eterm ref)
{
    ErtsMonitor* rmon;
    ErtsBinary* bin = ERTS_MAGIC_BIN_FROM_UNALIGNED_DATA(resource);
    struct enif_msg_environment_t msg_env;
    ErlNifPid nif_pid;
    ErlNifMonitor nif_monitor;
    ErtsResourceMonitors* rmp = resource->monitors;

    ASSERT(rmp);
    ASSERT(resource->type->down);

    erts_smp_mtx_lock(&rmp->lock);
    rmon = erts_remove_monitor(&rmp->root, ref);
    if (!rmon) {
        int free_me = (--rmp->pending_failed_fire == 0) && rmp->is_dying;
        ASSERT(rmp->pending_failed_fire >= 0);
        erts_smp_mtx_unlock(&rmp->lock);

        if (free_me) {
            ASSERT(erts_refc_read(&bin->binary.intern.refc, 0) == 0);
            erts_bin_free(&bin->binary);
        }
        return;
    }
    ASSERT(!rmp->is_dying);
    if (erts_refc_inc_unless(&bin->binary.intern.refc, 0, 0) == 0) {
        /*
         * Racing resource destruction. 
         * To avoid a more complex refc-dance with destructing thread
         * we avoid calling 'down' and just silently remove the monitor.
         * This can happen even for non smp as destructor calls may be scheduled.
         */
        erts_smp_mtx_unlock(&rmp->lock);
    }
    else {
        erts_smp_mtx_unlock(&rmp->lock);

        ASSERT(rmon->u.pid == pid);
        erts_ref_to_driver_monitor(ref, &nif_monitor);
        nif_pid.pid = pid;
        pre_nif_noproc(&msg_env, resource->type->owner, NULL);
        resource->type->down(&msg_env.env, resource->data, &nif_pid, &nif_monitor);
        post_nif_noproc(&msg_env);

        erts_bin_release(&bin->binary);
    }
    erts_destroy_monitor(rmon);
}

void* enif_alloc_resource(ErlNifResourceType* type, size_t data_sz)
{
    size_t magic_sz = offsetof(ErtsResource,data);
    Binary* bin;
    ErtsResource* resource;
    size_t monitors_offs;

    if (type->down) {
        /* Put ErtsResourceMonitors after user data and properly aligned */
        monitors_offs = ((data_sz + ERTS_ALLOC_ALIGN_BYTES - 1)
                         & ~((size_t)ERTS_ALLOC_ALIGN_BYTES - 1));
        magic_sz += monitors_offs + sizeof(ErtsResourceMonitors);
    }
    else {
        ERTS_UNDEF(monitors_offs, 0);
        magic_sz += data_sz;
    }
    bin = erts_create_magic_binary_x(magic_sz, NIF_RESOURCE_DTOR,
                                     ERTS_ALC_T_BINARY,
                                     1); /* unaligned */
    resource = ERTS_MAGIC_BIN_UNALIGNED_DATA(bin);

    ASSERT(type->owner && type->next && type->prev); /* not allowed in load/upgrade */
    resource->type = type;
    erts_refc_inc(&bin->intern.refc, 1);
#ifdef DEBUG
    erts_refc_init(&resource->nif_refc, 1);
#endif
    erts_refc_inc(&resource->type->refc, 2);
    if (type->down) {
        resource->monitors = (ErtsResourceMonitors*) (resource->data + monitors_offs);
        erts_smp_mtx_init(&resource->monitors->lock, "resource_monitors");
        resource->monitors->root = NULL;
        resource->monitors->pending_failed_fire = 0;
        resource->monitors->is_dying = 0;
        resource->monitors->user_data_sz = data_sz;
    }
    else {
        resource->monitors = NULL;
    }
    return resource->data;
}

void enif_release_resource(void* obj)
{
    ErtsResource* resource = DATA_TO_RESOURCE(obj);
    ErtsBinary* bin = ERTS_MAGIC_BIN_FROM_UNALIGNED_DATA(resource);

    ASSERT(ERTS_MAGIC_BIN_DESTRUCTOR(bin) == NIF_RESOURCE_DTOR);
    ASSERT(!(resource->monitors && resource->monitors->is_dying));
#ifdef DEBUG
    erts_refc_dec(&resource->nif_refc, 0);
#endif
    erts_bin_release(&bin->binary);
}

void enif_keep_resource(void* obj)
{
    ErtsResource* resource = DATA_TO_RESOURCE(obj);
    ErtsBinary* bin = ERTS_MAGIC_BIN_FROM_UNALIGNED_DATA(resource);

    ASSERT(ERTS_MAGIC_BIN_DESTRUCTOR(bin) == NIF_RESOURCE_DTOR);
    ASSERT(!(resource->monitors && resource->monitors->is_dying));
#ifdef DEBUG
    erts_refc_inc(&resource->nif_refc, 1);
#endif
    erts_refc_inc(&bin->binary.intern.refc, 2);
}

Eterm erts_bld_resource_ref(Eterm** hpp, ErlOffHeap* oh, ErtsResource* resource)
{
    ErtsBinary* bin = ERTS_MAGIC_BIN_FROM_UNALIGNED_DATA(resource);
    ASSERT(!(resource->monitors && resource->monitors->is_dying));
    return erts_mk_magic_ref(hpp, oh, &bin->binary);
}

ERL_NIF_TERM enif_make_resource(ErlNifEnv* env, void* obj)
{
    ErtsResource* resource = DATA_TO_RESOURCE(obj);
    ErtsBinary* bin = ERTS_MAGIC_BIN_FROM_UNALIGNED_DATA(resource);
    Eterm* hp = alloc_heap(env, ERTS_MAGIC_REF_THING_SIZE);
    ASSERT(!(resource->monitors && resource->monitors->is_dying));
    return erts_mk_magic_ref(&hp, &MSO(env->proc), &bin->binary);
}

ERL_NIF_TERM enif_make_resource_binary(ErlNifEnv* env, void* obj,
				       const void* data, size_t size)
{
    ErtsResource* resource = DATA_TO_RESOURCE(obj);
    ErtsBinary* bin = ERTS_MAGIC_BIN_FROM_UNALIGNED_DATA(resource);
    ErlOffHeap *ohp = &MSO(env->proc);
    Eterm* hp = alloc_heap(env,PROC_BIN_SIZE);
    ProcBin* pb = (ProcBin *) hp;

    pb->thing_word = HEADER_PROC_BIN;
    pb->size = size;
    pb->next = ohp->first;
    ohp->first = (struct erl_off_heap_header*) pb;
    pb->val = &bin->binary;
    pb->bytes = (byte*) data;
    pb->flags = 0;

    OH_OVERHEAD(ohp, size / sizeof(Eterm));
    erts_refc_inc(&bin->binary.intern.refc, 1);

    return make_binary(hp);
}

int enif_get_resource(ErlNifEnv* env, ERL_NIF_TERM term, ErlNifResourceType* type,
		      void** objp)
{
    Binary* mbin;
    ErtsResource* resource;
    if (is_internal_magic_ref(term))
	mbin = erts_magic_ref2bin(term);
    else {
        Eterm *hp;
        if (!is_binary(term))
            return 0;
        hp = binary_val(term);
        if (thing_subtag(*hp) != REFC_BINARY_SUBTAG)
            return 0;
        /*
        if (((ProcBin *) hp)->size != 0) {	
            return 0; / * Or should we allow "resource binaries" as handles? * /
        }
        */
        mbin = ((ProcBin *) hp)->val;
        if (!(mbin->intern.flags & BIN_FLAG_MAGIC))
            return 0;
    }
    resource = (ErtsResource*) ERTS_MAGIC_BIN_UNALIGNED_DATA(mbin);
    if (ERTS_MAGIC_BIN_DESTRUCTOR(mbin) != NIF_RESOURCE_DTOR
	|| resource->type != type) {	
	return 0;
    }
    *objp = resource->data;
    return 1;
}

size_t enif_sizeof_resource(void* obj)
{
    ErtsResource* resource = DATA_TO_RESOURCE(obj);
    if (resource->monitors) {
        return resource->monitors->user_data_sz;
    }
    else {
        Binary* bin = &ERTS_MAGIC_BIN_FROM_UNALIGNED_DATA(resource)->binary;
        return ERTS_MAGIC_BIN_UNALIGNED_DATA_SIZE(bin) - offsetof(ErtsResource,data);
    }
}


void* enif_dlopen(const char* lib,
		  void (*err_handler)(void*,const char*), void* err_arg)
{
    ErtsSysDdllError errdesc = ERTS_SYS_DDLL_ERROR_INIT;
    void* handle;
    void* init_func;
    if (erts_sys_ddll_open(lib, &handle, &errdesc) == ERL_DE_NO_ERROR) {
	if (erts_sys_ddll_load_nif_init(handle, &init_func, &errdesc) == ERL_DE_NO_ERROR) {
	    erts_sys_ddll_call_nif_init(init_func);
	}
    }
    else {
	if (err_handler != NULL) {
	    (*err_handler)(err_arg, errdesc.str);
	}
	handle = NULL;
    }
    erts_sys_ddll_free_error(&errdesc);
    return handle;
}

void* enif_dlsym(void* handle, const char* symbol,
		 void (*err_handler)(void*,const char*), void* err_arg)
{
    ErtsSysDdllError errdesc = ERTS_SYS_DDLL_ERROR_INIT;
    void* ret;
    if (erts_sys_ddll_sym2(handle, symbol, &ret, &errdesc) != ERL_DE_NO_ERROR) {
	if (err_handler != NULL) {
	    (*err_handler)(err_arg, errdesc.str);
	}
	erts_sys_ddll_free_error(&errdesc);
	return NULL;
    }
    return ret;
}

int enif_consume_timeslice(ErlNifEnv* env, int percent)
{
    Process *proc;
    Sint reds;

    execution_state(env, &proc, NULL);

    ASSERT(is_proc_bound(env) && percent >= 1 && percent <= 100);
    if (percent < 1) percent = 1;
    else if (percent > 100) percent = 100;

    reds = ((CONTEXT_REDS+99) / 100) * percent;
    ASSERT(reds > 0 && reds <= CONTEXT_REDS);
    BUMP_REDS(proc, reds);
    return ERTS_BIF_REDS_LEFT(proc) == 0;
}

static ERTS_INLINE void
nif_export_cleanup_nif_mod(NifExport *ep)
{
    if (erts_refc_dectest(&ep->m->rt_dtor_cnt, 0) == 0 && ep->m->mod == NULL)
	close_lib(ep->m);
    ep->m = NULL;
}

void
erts_nif_export_cleanup_nif_mod(NifExport *ep)
{
    nif_export_cleanup_nif_mod(ep);
}

static ERTS_INLINE void
nif_export_restore(Process *c_p, NifExport *ep, Eterm res)
{
    erts_nif_export_restore(c_p, ep, res);
    ASSERT(ep->m);
    nif_export_cleanup_nif_mod(ep);
}


#ifdef ERTS_DIRTY_SCHEDULERS

/*
 * Finalize a dirty NIF call. This function is scheduled to cause the VM to
 * switch the process off a dirty scheduler thread and back onto a regular
 * scheduler thread, and then return the result from the dirty NIF. It also
 * restores the original NIF MFA when necessary based on the value of
 * ep->func set by execute_dirty_nif via init_nif_sched_data -- non-NULL
 * means restore, NULL means do not restore.
 */
static ERL_NIF_TERM
dirty_nif_finalizer(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    Process* proc;
    NifExport* ep;

    execution_state(env, &proc, NULL);

    ASSERT(argc == 1);
    ASSERT(!ERTS_SCHEDULER_IS_DIRTY(erts_proc_sched_data(proc)));
    ep = (NifExport*) ERTS_PROC_GET_NIF_TRAP_EXPORT(proc);
    ASSERT(ep);
    nif_export_restore(proc, ep, argv[0]);
    return argv[0];
}

/* Finalize a dirty NIF call that raised an exception.  Otherwise same as
 * the dirty_nif_finalizer() function.
 */
static ERL_NIF_TERM
dirty_nif_exception(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM ret;
    Process* proc;
    NifExport* ep;
    Eterm exception;

    execution_state(env, &proc, NULL);

    ASSERT(argc == 1);
    ASSERT(!ERTS_SCHEDULER_IS_DIRTY(erts_proc_sched_data(proc)));
    ep = (NifExport*) ERTS_PROC_GET_NIF_TRAP_EXPORT(proc);
    ASSERT(ep);
    exception = argv[0]; /* argv overwritten by restore below... */
    nif_export_cleanup_nif_mod(ep);
    ret = enif_raise_exception(env, exception);

    /* Restore orig info for error and clear nif export in handle_error() */
    proc->freason |= EXF_RESTORE_NIF;
    return ret;
}

/*
 * Dirty NIF scheduling wrapper function. Schedule a dirty NIF to execute.
 * The dirty scheduler thread type (CPU or I/O) is indicated in flags
 * parameter.
 */
static ERTS_INLINE ERL_NIF_TERM
schedule_dirty_nif(ErlNifEnv* env, int flags, NativeFunPtr fp,
		   Eterm func_name, int argc, const ERL_NIF_TERM argv[])
{
    Process* proc;

    ASSERT(is_atom(func_name));
    ASSERT(fp);

    ASSERT(flags==ERL_NIF_DIRTY_JOB_IO_BOUND || flags==ERL_NIF_DIRTY_JOB_CPU_BOUND);

    execution_state(env, &proc, NULL);

    (void) erts_smp_atomic32_read_bset_nob(&proc->state,
					   (ERTS_PSFLG_DIRTY_CPU_PROC
					    | ERTS_PSFLG_DIRTY_IO_PROC),
					   (flags == ERL_NIF_DIRTY_JOB_CPU_BOUND
					    ? ERTS_PSFLG_DIRTY_CPU_PROC
					    : ERTS_PSFLG_DIRTY_IO_PROC));

    return schedule(env, fp, NULL, proc->current->module, func_name, argc, argv);
}

static ERTS_INLINE ERL_NIF_TERM
static_schedule_dirty_nif(ErlNifEnv* env, erts_aint32_t dirty_psflg,
			     int argc, const ERL_NIF_TERM argv[])
{
    Process *proc;
    NifExport *ep;
    Eterm mod, func;
    NativeFunPtr fp;

    execution_state(env, &proc, NULL);

    /*
     * Called in order to schedule statically determined
     * dirty NIF calls...
     *
     * Note that 'current' does not point into a NifExport
     * structure; only a structure with similar
     * parts (located in code).
     */

    ep = ErtsContainerStruct(proc->current, NifExport, exp.info.mfa);
    mod = proc->current->module;
    func = proc->current->function;
    fp = (NativeFunPtr) ep->func;

    ASSERT(is_atom(mod) && is_atom(func));
    ASSERT(fp);

    (void) erts_smp_atomic32_read_bset_nob(&proc->state,
					   (ERTS_PSFLG_DIRTY_CPU_PROC
					    | ERTS_PSFLG_DIRTY_IO_PROC),
					   dirty_psflg);

    return schedule(env, fp, NULL, mod, func, argc, argv);
}

static ERL_NIF_TERM
static_schedule_dirty_io_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return static_schedule_dirty_nif(env, ERTS_PSFLG_DIRTY_IO_PROC, argc, argv);
}

static ERL_NIF_TERM
static_schedule_dirty_cpu_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return static_schedule_dirty_nif(env, ERTS_PSFLG_DIRTY_CPU_PROC, argc, argv);
}

#endif /* ERTS_DIRTY_SCHEDULERS */

/*
 * NIF execution wrapper used by enif_schedule_nif() for regular NIFs. It
 * calls the actual NIF, restores original NIF MFA if necessary, and
 * then returns the NIF result.
 */
static ERL_NIF_TERM
execute_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    Process* proc;
    NativeFunPtr fp;
    NifExport* ep;
    ERL_NIF_TERM result;

    execution_state(env, &proc, NULL);

    ep = ErtsContainerStruct(proc->current, NifExport, exp.info.mfa);
    fp = ep->func;
    ASSERT(ep);
    ASSERT(!env->exception_thrown);

    fp = (NativeFunPtr) ep->func;

#ifdef DEBUG
    ep->func = ERTS_DBG_NIF_NOT_SCHED_MARKER;
#endif

    result = (*fp)(env, argc, argv);

    ASSERT(ep == ERTS_PROC_GET_NIF_TRAP_EXPORT(proc));

    if (is_value(result) || proc->freason != TRAP) {
	/* Done (not rescheduled)... */
	ASSERT(ep->func == ERTS_DBG_NIF_NOT_SCHED_MARKER);
	if (!env->exception_thrown)
	    nif_export_restore(proc, ep, result);
	else {
	    nif_export_cleanup_nif_mod(ep);
	    /*
	     * Restore orig info for error and clear nif
	     * export in handle_error()
	     */
	    proc->freason |= EXF_RESTORE_NIF;
	}
    }

#ifdef DEBUG
    if (ep->func == ERTS_DBG_NIF_NOT_SCHED_MARKER)
	ep->func = NULL;
#endif

    return result;
}

ERL_NIF_TERM
enif_schedule_nif(ErlNifEnv* env, const char* fun_name, int flags,
		  ERL_NIF_TERM (*fp)(ErlNifEnv*, int, const ERL_NIF_TERM[]),
		  int argc, const ERL_NIF_TERM argv[])
{
    Process* proc;
    ERL_NIF_TERM fun_name_atom, result;
    int scheduler;

    if (argc > MAX_ARG)
	return enif_make_badarg(env);
    fun_name_atom = enif_make_atom(env, fun_name);
    if (enif_is_exception(env, fun_name_atom))
	return fun_name_atom;

    execution_state(env, &proc, &scheduler);
    if (scheduler <= 0) {
	if (scheduler == 0)
	    enif_make_badarg(env);
	erts_smp_proc_lock(proc, ERTS_PROC_LOCK_MAIN);
    }

    if (flags == 0)
	result = schedule(env, execute_nif, fp, proc->current->module,
			  fun_name_atom, argc, argv);
    else if (!(flags & ~(ERL_NIF_DIRTY_JOB_IO_BOUND|ERL_NIF_DIRTY_JOB_CPU_BOUND))) {
#ifdef ERTS_DIRTY_SCHEDULERS
	result = schedule_dirty_nif(env, flags, fp, fun_name_atom, argc, argv);
#else
        result = enif_raise_exception(env, am_notsup);
#endif
    }
    else
	result = enif_make_badarg(env);

    if (scheduler < 0)
	erts_smp_proc_unlock(proc, ERTS_PROC_LOCK_MAIN);

    return result;
}

int
enif_thread_type(void)
{
    ErtsSchedulerData *esdp = erts_get_scheduler_data();

    if (!esdp)
	return ERL_NIF_THR_UNDEFINED;

    switch (esdp->type) {
    case ERTS_SCHED_NORMAL:
	return ERL_NIF_THR_NORMAL_SCHEDULER;
#ifdef ERTS_DIRTY_SCHEDULERS
    case ERTS_SCHED_DIRTY_CPU:
	return ERL_NIF_THR_DIRTY_CPU_SCHEDULER;
    case ERTS_SCHED_DIRTY_IO:
        return ERL_NIF_THR_DIRTY_IO_SCHEDULER;
#endif
    default:
        ERTS_INTERNAL_ERROR("Invalid scheduler type");
	return -1;
    }
}

/* Maps */

int enif_is_map(ErlNifEnv* env, ERL_NIF_TERM term)
{
    return is_map(term);
}

int enif_get_map_size(ErlNifEnv* env, ERL_NIF_TERM term, size_t *size)
{
    if (is_flatmap(term)) {
	flatmap_t *mp;
	mp    = (flatmap_t*)flatmap_val(term);
	*size = flatmap_get_size(mp);
	return 1;
    }
    else if (is_hashmap(term)) {
        *size = hashmap_size(term);
        return 1;
    }
    return 0;
}

ERL_NIF_TERM enif_make_new_map(ErlNifEnv* env)
{
    Eterm* hp = alloc_heap(env,MAP_HEADER_FLATMAP_SZ+1);
    Eterm tup;
    flatmap_t *mp;

    tup   = make_tuple(hp);
    *hp++ = make_arityval(0);
    mp    = (flatmap_t*)hp;
    mp->thing_word = MAP_HEADER_FLATMAP;
    mp->size = 0;
    mp->keys = tup;

    return make_flatmap(mp);
}

int enif_make_map_put(ErlNifEnv* env,
	              Eterm map_in,
		      Eterm key,
		      Eterm value,
		      Eterm *map_out)
{
    if (!is_map(map_in)) {
	return 0;
    }
    ASSERT_IN_ENV(env, map_in, 0, "old map");
    ASSERT_IN_ENV(env, key, 0, "key");
    ASSERT_IN_ENV(env, value, 0, "value");

    flush_env(env);
    *map_out = erts_maps_put(env->proc, key, value, map_in);
    cache_env(env);
    return 1;
}

int enif_get_map_value(ErlNifEnv* env,
	               Eterm map,
		       Eterm key,
		       Eterm *value)
{
    const Eterm *ret;
    if (!is_map(map)) {
	return 0;
    }
    ret = erts_maps_get(key, map);
    if (ret) {
        *value = *ret;
        return 1;
    }
    return 0;
}

int enif_make_map_update(ErlNifEnv* env,
	                 Eterm map_in,
			 Eterm key,
			 Eterm value,
			 Eterm *map_out)
{
    int res;
    if (!is_map(map_in)) {
	return 0;
    }

    ASSERT_IN_ENV(env, map_in, 0, "old map");
    ASSERT_IN_ENV(env, key, 0, "key");
    ASSERT_IN_ENV(env, value, 0, "value");

    flush_env(env);
    res = erts_maps_update(env->proc, key, value, map_in, map_out);
    cache_env(env);
    return res;
}

int enif_make_map_remove(ErlNifEnv* env,
	                 Eterm map_in,
			 Eterm key,
			 Eterm *map_out)
{
    if (!is_map(map_in)) {
	return 0;
    }
    flush_env(env);
    (void) erts_maps_take(env->proc, key, map_in, map_out, NULL);
    cache_env(env);
    return 1;
}

int enif_map_iterator_create(ErlNifEnv *env,
	                     Eterm map,
			     ErlNifMapIterator *iter,
			     ErlNifMapIteratorEntry entry)
{
    if (is_flatmap(map)) {
	flatmap_t *mp = (flatmap_t*)flatmap_val(map);
	size_t offset;

	switch (entry) {
	    case ERL_NIF_MAP_ITERATOR_FIRST: offset = 0; break;
	    case ERL_NIF_MAP_ITERATOR_LAST: offset = flatmap_get_size(mp) - 1; break;
	    default: goto error;
	}

	/* empty maps are ok but will leave the iterator
	 * in bad shape.
	 */

	iter->map     = map;
	iter->u.flat.ks = ((Eterm *)flatmap_get_keys(mp)) + offset;
	iter->u.flat.vs = ((Eterm *)flatmap_get_values(mp)) + offset;
	iter->size    = flatmap_get_size(mp);
	iter->idx     = offset + 1;

	return 1;
    }
    else if (is_hashmap(map)) {
        iter->map = map;
        iter->size = hashmap_size(map);
        iter->u.hash.wstack = erts_alloc(ERTS_ALC_T_NIF, sizeof(ErtsDynamicWStack));
        WSTACK_INIT(iter->u.hash.wstack, ERTS_ALC_T_NIF);

        switch (entry) {
	    case ERL_NIF_MAP_ITERATOR_FIRST:
                iter->idx = 1;
                hashmap_iterator_init(&iter->u.hash.wstack->ws, map, 0);
                iter->u.hash.kv = hashmap_iterator_next(&iter->u.hash.wstack->ws);
                break;
	    case ERL_NIF_MAP_ITERATOR_LAST:
                iter->idx = hashmap_size(map);
                hashmap_iterator_init(&iter->u.hash.wstack->ws, map, 1);
                iter->u.hash.kv = hashmap_iterator_prev(&iter->u.hash.wstack->ws);
                break;
	    default:
                goto error;
	}
        ASSERT(!!iter->u.hash.kv == (iter->idx >= 1 &&
                                     iter->idx <= iter->size));
        return 1;
    }
error:
#ifdef DEBUG
    iter->map = THE_NON_VALUE;
#endif
    return 0;
}

void enif_map_iterator_destroy(ErlNifEnv *env, ErlNifMapIterator *iter)
{
    if (is_hashmap(iter->map)) {
        WSTACK_DESTROY(iter->u.hash.wstack->ws);
        erts_free(ERTS_ALC_T_NIF, iter->u.hash.wstack);
    }
    else
        ASSERT(is_flatmap(iter->map));

#ifdef DEBUG
    iter->map = THE_NON_VALUE;
#endif
}

int enif_map_iterator_is_tail(ErlNifEnv *env, ErlNifMapIterator *iter)
{
    ASSERT(iter);
    if (is_flatmap(iter->map)) {
        ASSERT(iter->idx >= 0);
        ASSERT(iter->idx <= flatmap_get_size(flatmap_val(iter->map)) + 1);
        return (iter->size == 0 || iter->idx > iter->size);
    }
    else {
        ASSERT(is_hashmap(iter->map));
        return iter->idx > iter->size;
    }
}

int enif_map_iterator_is_head(ErlNifEnv *env, ErlNifMapIterator *iter)
{
    ASSERT(iter);
    if (is_flatmap(iter->map)) {
        ASSERT(iter->idx >= 0);
        ASSERT(iter->idx <= flatmap_get_size(flatmap_val(iter->map)) + 1);
        return (iter->size == 0 || iter->idx == 0);
    }
    else {
        ASSERT(is_hashmap(iter->map));
        return iter->idx == 0;
    }
}


int enif_map_iterator_next(ErlNifEnv *env, ErlNifMapIterator *iter)
{
    ASSERT(iter);
    if (is_flatmap(iter->map)) {
        if (iter->idx <= iter->size) {
            iter->idx++;
            iter->u.flat.ks++;
            iter->u.flat.vs++;
        }
        return (iter->idx <= iter->size);
    }
    else {
        ASSERT(is_hashmap(iter->map));

        if (iter->idx <= hashmap_size(iter->map)) {
            if (iter->idx < 1) {
                hashmap_iterator_init(&iter->u.hash.wstack->ws, iter->map, 0);
            }
            iter->u.hash.kv = hashmap_iterator_next(&iter->u.hash.wstack->ws);
            iter->idx++;
            ASSERT(!!iter->u.hash.kv == (iter->idx <= iter->size));
        }
        return iter->idx <= iter->size;
    }
}

int enif_map_iterator_prev(ErlNifEnv *env, ErlNifMapIterator *iter)
{
    ASSERT(iter);
    if (is_flatmap(iter->map)) {
        if (iter->idx > 0) {
            iter->idx--;
            iter->u.flat.ks--;
            iter->u.flat.vs--;
        }
        return iter->idx > 0;
    }
    else {
        ASSERT(is_hashmap(iter->map));

        if (iter->idx > 0) {
            if (iter->idx > iter->size) {
                hashmap_iterator_init(&iter->u.hash.wstack->ws, iter->map, 1);
            }
            iter->u.hash.kv = hashmap_iterator_prev(&iter->u.hash.wstack->ws);
            iter->idx--;
            ASSERT(!!iter->u.hash.kv == (iter->idx > 0));
        }
        return iter->idx > 0;
    }
}

int enif_map_iterator_get_pair(ErlNifEnv *env,
			       ErlNifMapIterator *iter,
			       Eterm *key,
			       Eterm *value)
{
    ASSERT(iter);
    if (is_flatmap(iter->map)) {
        if (iter->idx > 0 && iter->idx <= iter->size) {
            ASSERT(iter->u.flat.ks >= flatmap_get_keys(flatmap_val(iter->map)) &&
                   iter->u.flat.ks  < (flatmap_get_keys(flatmap_val(iter->map)) + flatmap_get_size(flatmap_val(iter->map))));
            ASSERT(iter->u.flat.vs >= flatmap_get_values(flatmap_val(iter->map)) &&
                   iter->u.flat.vs  < (flatmap_get_values(flatmap_val(iter->map)) + flatmap_get_size(flatmap_val(iter->map))));
            *key   = *(iter->u.flat.ks);
            *value = *(iter->u.flat.vs);
            return 1;
        }
    }
    else {
        ASSERT(is_hashmap(iter->map));
        if (iter->idx > 0 && iter->idx <= iter->size) {
            *key   = CAR(iter->u.hash.kv);
            *value = CDR(iter->u.hash.kv);
            return 1;
        }
    }
    return 0;
}

int enif_monitor_process(ErlNifEnv* env, void* obj, const ErlNifPid* target_pid,
                         ErlNifMonitor* monitor)
{
    int scheduler;
    ErtsResource* rsrc = DATA_TO_RESOURCE(obj);
    Process *rp;
    Eterm tmp[ERTS_REF_THING_SIZE];
    Eterm ref;
    int retval;

    ASSERT(ERTS_MAGIC_BIN_FROM_UNALIGNED_DATA(rsrc)->magic_binary.destructor
           == NIF_RESOURCE_DTOR);
    ASSERT(!(rsrc->monitors && rsrc->monitors->is_dying));
    ASSERT(!rsrc->monitors == !rsrc->type->down);


    if (!rsrc->monitors) {
        ASSERT(!rsrc->type->down);
        return -1;
    }
    ASSERT(rsrc->type->down);

    execution_state(env, NULL, &scheduler);

#ifdef ERTS_SMP
    if (scheduler > 0) /* Normal scheduler */
        rp = erts_proc_lookup_raw(target_pid->pid);
    else
        rp = erts_proc_lookup_raw_inc_refc(target_pid->pid);
#else
    if (scheduler <= 0) {
        erts_exit(ERTS_ABORT_EXIT, "enif_monitor_process: called from "
                  "non-scheduler thread on non-SMP VM");
    }
    rp = erts_proc_lookup(target_pid->pid);
#endif

    if (!rp)
        return 1;

    ref = erts_make_ref_in_buffer(tmp);

    erts_smp_mtx_lock(&rsrc->monitors->lock);
    erts_smp_proc_lock(rp, ERTS_PROC_LOCK_LINK);
    if (ERTS_PSFLG_FREE & erts_smp_atomic32_read_nob(&rp->state)) {
        retval = 1;
    }
    else {
        erts_add_monitor(&rsrc->monitors->root, MON_ORIGIN, ref, rp->common.id, NIL);
        erts_add_monitor(&ERTS_P_MONITORS(rp), MON_NIF_TARGET, ref, (UWord)rsrc, NIL);
        retval = 0;
    }
    erts_smp_proc_unlock(rp, ERTS_PROC_LOCK_LINK);
    erts_smp_mtx_unlock(&rsrc->monitors->lock);

#ifdef ERTS_SMP
    if (scheduler <= 0)
        erts_proc_dec_refc(rp);
#endif
    if (monitor)
        erts_ref_to_driver_monitor(ref,monitor);

    return retval;
}

int enif_demonitor_process(ErlNifEnv* env, void* obj, const ErlNifMonitor* monitor)
{
    int scheduler;
    ErtsResource* rsrc = DATA_TO_RESOURCE(obj);
#ifdef DEBUG
    ErtsBinary* bin = ERTS_MAGIC_BIN_FROM_UNALIGNED_DATA(rsrc);
#endif
    Process *rp;
    ErtsMonitor *mon;
    ErtsMonitor *rmon = NULL;
    Eterm ref_heap[ERTS_REF_THING_SIZE];
    Eterm ref;
    int is_exiting;

    ASSERT(bin->magic_binary.destructor == NIF_RESOURCE_DTOR);
    ASSERT(!(rsrc->monitors && rsrc->monitors->is_dying));

    execution_state(env, NULL, &scheduler);

    ref = erts_driver_monitor_to_ref(ref_heap, monitor);

    erts_smp_mtx_lock(&rsrc->monitors->lock);
    mon = erts_remove_monitor(&rsrc->monitors->root, ref);

    if (mon == NULL) {
        erts_smp_mtx_unlock(&rsrc->monitors->lock);
        return 1;
    }

    ASSERT(mon->type == MON_ORIGIN);
    ASSERT(is_internal_pid(mon->u.pid));

#ifdef ERTS_SMP
    if (scheduler > 0) /* Normal scheduler */
        rp = erts_proc_lookup(mon->u.pid);
    else
        rp = erts_proc_lookup_inc_refc(mon->u.pid);
#else
    if (scheduler <= 0) {
        erts_exit(ERTS_ABORT_EXIT, "enif_demonitor_process: called from "
                  "non-scheduler thread on non-SMP VM");
    }
    rp = erts_proc_lookup(mon->u.pid);
#endif

    if (!rp) {
        is_exiting = 1;
    }
    else {
        erts_smp_proc_lock(rp, ERTS_PROC_LOCK_LINK);
        if (ERTS_PROC_IS_EXITING(rp)) {
            is_exiting = 1;
        } else {
            rmon = erts_remove_monitor(&ERTS_P_MONITORS(rp), ref);
            ASSERT(rmon);
            is_exiting = 0;
        }
        erts_smp_proc_unlock(rp, ERTS_PROC_LOCK_LINK);

#ifdef ERTS_SMP
        if (scheduler <= 0)
            erts_proc_dec_refc(rp);
#endif
    }
    if (is_exiting) {
        rsrc->monitors->pending_failed_fire++;
    }
    erts_smp_mtx_unlock(&rsrc->monitors->lock);

    if (rmon) {
        ASSERT(rmon->type == MON_NIF_TARGET);
        ASSERT(rmon->u.resource == rsrc);
        erts_destroy_monitor(rmon);
    }
    erts_destroy_monitor(mon);

    return 0;
}

int enif_compare_monitors(const ErlNifMonitor *monitor1,
                          const ErlNifMonitor *monitor2)
{
    return sys_memcmp((void *) monitor1, (void *) monitor2,
                      ERTS_REF_THING_SIZE*sizeof(Eterm));
}

/***************************************************************************
 **                              load_nif/2                               **
 ***************************************************************************/


static ErtsCodeInfo** get_func_pp(BeamCodeHeader* mod_code, Eterm f_atom, unsigned arity)
{
    int n = (int) mod_code->num_functions;
    int j;
    for (j = 0; j < n; ++j) {
	ErtsCodeInfo* ci = mod_code->functions[j];
	ASSERT(ci->op == (BeamInstr) BeamOp(op_i_func_info_IaaI));
	if (f_atom == ci->mfa.function
	    && arity == ci->mfa.arity) {
	    return mod_code->functions+j;
	}
    }
    return NULL;
}

static Eterm mkatom(const char *str)
{
    return am_atom_put(str, sys_strlen(str));
}

static struct tainted_module_t
{
    struct tainted_module_t* next;
    Eterm module_atom;
}*first_tainted_module = NULL;

static void add_taint(Eterm mod_atom)
{
    struct tainted_module_t* t;
    for (t=first_tainted_module ; t!=NULL; t=t->next) {
	if (t->module_atom == mod_atom) {
	    return;
	}
    }
    t = erts_alloc_fnf(ERTS_ALC_T_TAINT, sizeof(*t));
    if (t != NULL) {
	t->module_atom = mod_atom;
	t->next = first_tainted_module;
	first_tainted_module = t;
    }
}

Eterm erts_nif_taints(Process* p)
{
    struct tainted_module_t* t;
    unsigned cnt = 0;
    Eterm list = NIL;
    Eterm* hp;
    for (t=first_tainted_module ; t!=NULL; t=t->next) {
	cnt++;
    }
    hp = HAlloc(p,cnt*2);
    for (t=first_tainted_module ; t!=NULL; t=t->next) {
	list = CONS(hp, t->module_atom, list);
	hp += 2;
    }
    return list;
}

void erts_print_nif_taints(fmtfn_t to, void* to_arg)
{
    struct tainted_module_t* t;
    const char* delim = "";
    for (t=first_tainted_module ; t!=NULL; t=t->next) {
	const Atom* atom = atom_tab(atom_val(t->module_atom));
	erts_cbprintf(to,to_arg,"%s%.*s", delim, atom->len, atom->name);
	delim = ",";
    }
    erts_cbprintf(to,to_arg,"\n");
}


static Eterm load_nif_error(Process* p, const char* atom, const char* format, ...)
{
    erts_dsprintf_buf_t* dsbufp = erts_create_tmp_dsbuf(0);
    Eterm ret;
    Eterm* hp;
    Eterm** hpp = NULL;
    Uint sz = 0;
    Uint* szp = &sz;
    va_list arglist;   

    va_start(arglist, format);
    erts_vdsprintf(dsbufp, format, arglist);
    va_end(arglist);
    
    for (;;) {
	Eterm txt = erts_bld_string_n(hpp, &sz, dsbufp->str, dsbufp->str_len);
	ret = erts_bld_tuple(hpp, szp, 2, am_error,
			     erts_bld_tuple(hpp, szp, 2, mkatom(atom), txt));
	if (hpp != NULL) {
	    break;
	}
	hp = HAlloc(p,sz);
	hpp = &hp;
	szp = NULL;
    }
    erts_destroy_tmp_dsbuf(dsbufp);
    return ret;
}

#define AT_LEAST_VERSION(E,MAJ,MIN) \
    (((E)->major * 0x100 + (E)->minor) >= ((MAJ) * 0x100 + (MIN)))

/*
 * Allocate erl_module_nif and make a _modern_ copy of the lib entry.
 */
static struct erl_module_nif* create_lib(const ErlNifEntry* src)
{
    struct erl_module_nif* lib;
    ErlNifEntry* dst;
    Uint bytes = offsetof(struct erl_module_nif, _funcs_copy_);

    if (!AT_LEAST_VERSION(src, 2, 7))
        bytes += src->num_of_funcs * sizeof(ErlNifFunc);

    lib = erts_alloc(ERTS_ALC_T_NIF, bytes);
    dst = &lib->entry;

    sys_memcpy(dst, src, offsetof(ErlNifEntry, vm_variant));

    if (AT_LEAST_VERSION(src, 2, 1)) {
        dst->vm_variant = src->vm_variant;
    } else {
        dst->vm_variant = "beam.vanilla";
    }
    if (AT_LEAST_VERSION(src, 2, 7)) {
        dst->options = src->options;
    } else {
        /*
         * Make a modern copy of the ErlNifFunc array
         */
        struct ErlNifFunc_V1 {
            const char* name;
            unsigned arity;
            ERL_NIF_TERM (*fptr)(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
        }*src_funcs = (struct ErlNifFunc_V1*) src->funcs;
        int i;
        for (i = 0; i < src->num_of_funcs; ++i) {
            sys_memcpy(&lib->_funcs_copy_[i], &src_funcs[i], sizeof(*src_funcs));
            lib->_funcs_copy_[i].flags = 0;
        }
        dst->funcs = lib->_funcs_copy_;
        dst->options = 0;
    }
    if (AT_LEAST_VERSION(src, 2, 12)) {
        dst->sizeof_ErlNifResourceTypeInit = src->sizeof_ErlNifResourceTypeInit;
    } else {
        dst->sizeof_ErlNifResourceTypeInit = 0;
    }
    return lib;
};


BIF_RETTYPE load_nif_2(BIF_ALIST_2)
{
    static const char bad_lib[] = "bad_lib";
    static const char upgrade[] = "upgrade";
    char* lib_name = NULL;
    void* handle = NULL;
    void* init_func = NULL;
    ErlNifEntry* entry = NULL;
    ErlNifEnv env;
    int i, err, encoding;
    Module* module_p;
    Eterm mod_atom;
    const Atom* mod_atomp;
    Eterm f_atom;
    ErtsCodeMFA* caller;
    ErtsSysDdllError errdesc = ERTS_SYS_DDLL_ERROR_INIT;
    Eterm ret = am_ok;
    int veto;
    struct erl_module_nif* lib = NULL;
    struct erl_module_instance* this_mi;
    struct erl_module_instance* prev_mi;

    if (BIF_P->flags & F_HIPE_MODE) {
	ret = load_nif_error(BIF_P, "notsup", "Calling load_nif from HiPE compiled "
			     "modules not supported");
	BIF_RET(ret);
    }

    encoding = erts_get_native_filename_encoding();
    if (encoding == ERL_FILENAME_WIN_WCHAR) {
        /* Do not convert the lib name to utf-16le yet, do that in win32 specific code */
        /* since lib_name is used in error messages */
        encoding = ERL_FILENAME_UTF8;
    }
    lib_name = erts_convert_filename_to_encoding(BIF_ARG_1, NULL, 0,
                                                 ERTS_ALC_T_TMP, 1, 0, encoding,
						 NULL, 0);
    if (!lib_name) {
	BIF_ERROR(BIF_P, BADARG);
    }

    if (!erts_try_seize_code_write_permission(BIF_P)) {
	erts_free(ERTS_ALC_T_TMP, lib_name);
	ERTS_BIF_YIELD2(bif_export[BIF_load_nif_2],
			BIF_P, BIF_ARG_1, BIF_ARG_2);
    }

    /* Block system (is this the right place to do it?) */
    erts_smp_proc_unlock(BIF_P, ERTS_PROC_LOCK_MAIN);
    erts_smp_thr_progress_block();

    /* Find calling module */
    ASSERT(BIF_P->current != NULL);
    ASSERT(BIF_P->current->module == am_erlang
	   && BIF_P->current->function == am_load_nif 
	   && BIF_P->current->arity == 2);
    caller = find_function_from_pc(BIF_P->cp);
    ASSERT(caller != NULL);
    mod_atom = caller->module;
    ASSERT(is_atom(mod_atom));
    module_p = erts_get_module(mod_atom, erts_active_code_ix());
    ASSERT(module_p != NULL);

    mod_atomp = atom_tab(atom_val(mod_atom));
    init_func = erts_static_nif_get_nif_init((char*)mod_atomp->name, mod_atomp->len);
    if (init_func != NULL)
      handle = init_func;

    this_mi = &module_p->curr;
    prev_mi = &module_p->old;
    if (in_area(caller, module_p->old.code_hdr, module_p->old.code_length)) {
	ret = load_nif_error(BIF_P, "old_code", "Calling load_nif from old "
			     "module '%T' not allowed", mod_atom);
	goto error;
    } else if (module_p->on_load) {
	ASSERT(module_p->on_load->code_hdr->on_load_function_ptr);
	if (module_p->curr.code_hdr) {
	    prev_mi = &module_p->curr;
	} else {
	    prev_mi = &module_p->old;
	}
	this_mi = module_p->on_load;
    }

    if (this_mi->nif != NULL) {
        ret = load_nif_error(BIF_P,"reload","NIF library already loaded"
                             " (reload disallowed since OTP 20).");
    }
    else if (init_func == NULL &&
             (err=erts_sys_ddll_open(lib_name, &handle, &errdesc)) != ERL_DE_NO_ERROR) {
	const char slogan[] = "Failed to load NIF library";
	if (strstr(errdesc.str, lib_name) != NULL) {
	    ret = load_nif_error(BIF_P, "load_failed", "%s: '%s'", slogan, errdesc.str);
	}
	else {
	    ret = load_nif_error(BIF_P, "load_failed", "%s %s: '%s'", slogan, lib_name, errdesc.str);
	}
    }
    else if (init_func == NULL &&
	     erts_sys_ddll_load_nif_init(handle, &init_func, &errdesc) != ERL_DE_NO_ERROR) {
	ret  = load_nif_error(BIF_P, bad_lib, "Failed to find library init"
			      " function: '%s'", errdesc.str);
	
    }
    else if ((add_taint(mod_atom),
	      (entry = erts_sys_ddll_call_nif_init(init_func)) == NULL)) {
	ret = load_nif_error(BIF_P, bad_lib, "Library init-call unsuccessful");
    }
    else if (entry->major < ERL_NIF_MIN_REQUIRED_MAJOR_VERSION_ON_LOAD
	     || (ERL_NIF_MAJOR_VERSION < entry->major
		 || (ERL_NIF_MAJOR_VERSION == entry->major
		     && ERL_NIF_MINOR_VERSION < entry->minor))
	     || (entry->major==2 && entry->minor == 5)) { /* experimental maps */
	
	ret = load_nif_error(BIF_P, bad_lib, "Library version (%d.%d) not compatible (with %d.%d).",
			     entry->major, entry->minor, ERL_NIF_MAJOR_VERSION, ERL_NIF_MINOR_VERSION);
    }   
    else if (AT_LEAST_VERSION(entry, 2, 1)
	     && sys_strcmp(entry->vm_variant, ERL_NIF_VM_VARIANT) != 0) {
	ret = load_nif_error(BIF_P, bad_lib, "Library (%s) not compiled for "
			     "this vm variant (%s).",
			     entry->vm_variant, ERL_NIF_VM_VARIANT);
    }
    else if (!erts_is_atom_str((char*)entry->name, mod_atom, 1)) {
	ret = load_nif_error(BIF_P, bad_lib, "Library module name '%s' does not"
			     " match calling module '%T'", entry->name, mod_atom);
    }
    else {
        lib = create_lib(entry);
        entry = &lib->entry; /* Use a guaranteed modern lib entry from now on */

        lib->handle = handle;
        erts_refc_init(&lib->rt_cnt, 0);
        erts_refc_init(&lib->rt_dtor_cnt, 0);
        ASSERT(opened_rt_list == NULL);
        lib->mod = module_p;

        for (i=0; i < entry->num_of_funcs && ret==am_ok; i++) {
	    ErtsCodeInfo** ci_pp;
            ErlNifFunc* f = &entry->funcs[i];

	    if (!erts_atom_get(f->name, sys_strlen(f->name), &f_atom, ERTS_ATOM_ENC_LATIN1)
		|| (ci_pp = get_func_pp(this_mi->code_hdr, f_atom, f->arity))==NULL) {
		ret = load_nif_error(BIF_P,bad_lib,"Function not found %T:%s/%u",
				     mod_atom, f->name, f->arity);
	    }
	    else if (f->flags) {
		/*
		 * If the flags field is non-zero and this emulator was
		 * built with dirty scheduler support, check that the flags
		 * value is legal. But if this emulator was built without
		 * dirty scheduler support, treat a non-zero flags field as
		 * a load error.
		 */
#ifdef ERTS_DIRTY_SCHEDULERS
		if (f->flags != ERL_NIF_DIRTY_JOB_IO_BOUND && f->flags != ERL_NIF_DIRTY_JOB_CPU_BOUND)
		    ret = load_nif_error(BIF_P, bad_lib, "Illegal flags field value %d for NIF %T:%s/%u",
					 f->flags, mod_atom, f->name, f->arity);
#else
		ret = load_nif_error(BIF_P, bad_lib, "NIF %T:%s/%u requires a runtime with dirty scheduler support.",
				     mod_atom, f->name, f->arity);
#endif
	    }
	    else if (erts_codeinfo_to_code(ci_pp[1]) - erts_codeinfo_to_code(ci_pp[0])
                     < BEAM_NIF_MIN_FUNC_SZ)
	    {
		ret = load_nif_error(BIF_P,bad_lib,"No explicit call to load_nif"
				     " in module (%T:%s/%u too small)",
				     mod_atom, f->name, f->arity);
	    }
	    /*erts_fprintf(stderr, "Found NIF %T:%s/%u\r\n",
	      mod_atom, f->name, f->arity);*/
	}
    }

    if (ret != am_ok) {
	goto error;
    }

    /* Call load or upgrade:
     */

    env.mod_nif = lib;

    lib->priv_data = NULL;
    if (prev_mi->nif != NULL) { /**************** Upgrade ***************/
        void* prev_old_data = prev_mi->nif->priv_data;
        if (entry->upgrade == NULL) {
            ret = load_nif_error(BIF_P, upgrade, "Upgrade not supported by this NIF library.");
            goto error;
        }
        erts_pre_nif(&env, BIF_P, lib, NULL);
        veto = entry->upgrade(&env, &lib->priv_data, &prev_mi->nif->priv_data, BIF_ARG_2);
        erts_post_nif(&env);
        if (veto) {
            prev_mi->nif->priv_data = prev_old_data;
            ret = load_nif_error(BIF_P, upgrade, "Library upgrade-call unsuccessful (%d).", veto);
        }
    }
    else if (entry->load != NULL) { /********* Initial load ***********/
        erts_pre_nif(&env, BIF_P, lib, NULL);
        veto = entry->load(&env, &lib->priv_data, BIF_ARG_2);
        erts_post_nif(&env);
        if (veto) {
            ret = load_nif_error(BIF_P, "load", "Library load-call unsuccessful (%d).", veto);
        }
    }
    if (ret == am_ok) {
        commit_opened_resource_types(lib);

	/*
	** Everything ok, patch the beam code with op_call_nif
	*/

	this_mi->nif = lib;
	for (i=0; i < entry->num_of_funcs; i++)
	{
            ErlNifFunc* f = &entry->funcs[i];
	    ErtsCodeInfo* ci;
            BeamInstr *code_ptr;

	    erts_atom_get(f->name, sys_strlen(f->name), &f_atom, ERTS_ATOM_ENC_LATIN1);
	    ci = *get_func_pp(this_mi->code_hdr, f_atom, f->arity);
            code_ptr = erts_codeinfo_to_code(ci);

	    if (ci->u.gen_bp == NULL) {
		code_ptr[0] = (BeamInstr) BeamOp(op_call_nif);
	    }
	    else { /* Function traced, patch the original instruction word */
		GenericBp* g = ci->u.gen_bp;
		ASSERT(code_ptr[0] ==
		       (BeamInstr) BeamOp(op_i_generic_breakpoint));
		g->orig_instr = (BeamInstr) BeamOp(op_call_nif);
	    }
#ifdef ERTS_DIRTY_SCHEDULERS
	    if (f->flags) {
		code_ptr[3] = (BeamInstr) f->fptr;
		code_ptr[1] = (f->flags == ERL_NIF_DIRTY_JOB_IO_BOUND) ?
		    (BeamInstr) static_schedule_dirty_io_nif :
		    (BeamInstr) static_schedule_dirty_cpu_nif;
	    }
	    else
#endif
		code_ptr[1] = (BeamInstr) f->fptr;
	    code_ptr[2] = (BeamInstr) lib;
	}
    }
    else {
    error:
	rollback_opened_resource_types();
	ASSERT(ret != am_ok);
        if (lib != NULL) {
	    erts_free(ERTS_ALC_T_NIF, lib);
	}
	if (handle != NULL && !erts_is_static_nif(handle)) {
	    erts_sys_ddll_close(handle);
	}
	erts_sys_ddll_free_error(&errdesc);
    }

    erts_smp_thr_progress_unblock();
    erts_smp_proc_lock(BIF_P, ERTS_PROC_LOCK_MAIN);
    erts_release_code_write_permission();
    erts_free(ERTS_ALC_T_TMP, lib_name);

    BIF_RET(ret);
}


void
erts_unload_nif(struct erl_module_nif* lib)
{
    ErlNifResourceType* rt;
    ErlNifResourceType* next;
    ASSERT(erts_smp_thr_progress_is_blocking());
    ASSERT(lib != NULL);
    ASSERT(lib->mod != NULL);

    erts_tracer_nif_clear();

    for (rt = resource_type_list.next;
	 rt != &resource_type_list;
	 rt = next) {

	next = rt->next;
	if (rt->owner == lib) {
	    rt->next->prev = rt->prev;
	    rt->prev->next = rt->next;
	    rt->next = NULL;
	    rt->prev = NULL;
	    if (erts_refc_dectest(&rt->refc, 0) == 0) {
		if (rt->dtor != NULL) {
		    erts_refc_dec(&lib->rt_dtor_cnt, 0);
		}
		erts_refc_dec(&lib->rt_cnt, 0);
		erts_free(ERTS_ALC_T_NIF, rt);
	    }
	}
    }
    if (erts_refc_read(&lib->rt_dtor_cnt, 0) == 0) {
	close_lib(lib);
	if (erts_refc_read(&lib->rt_cnt, 0) == 0) {
	    erts_free(ERTS_ALC_T_NIF, lib);
	    return;
	}
    }
    else {
	ASSERT(erts_refc_read(&lib->rt_cnt, 1) > 0);
    }
    lib->mod = NULL;   /* orphan lib */
}	

void erl_nif_init()
{
    ERTS_CT_ASSERT((offsetof(ErtsResource,data) % 8)
                   == ERTS_MAGIC_BIN_BYTES_TO_ALIGN);

    resource_type_list.next = &resource_type_list;
    resource_type_list.prev = &resource_type_list;
    resource_type_list.dtor = NULL;
    resource_type_list.owner = NULL;
    resource_type_list.module = THE_NON_VALUE;
    resource_type_list.name = THE_NON_VALUE;

}

int erts_nif_get_funcs(struct erl_module_nif* mod,
                       ErlNifFunc **funcs)
{
    *funcs = mod->entry.funcs;
    return mod->entry.num_of_funcs;
}

Eterm erts_nif_call_function(Process *p, Process *tracee,
                             struct erl_module_nif* mod,
                             ErlNifFunc *fun, int argc, Eterm *argv)
{
    Eterm nif_result;
#ifdef DEBUG
    /* Verify that function is part of this module */
    int i;
    for (i = 0; i < mod->entry.num_of_funcs; i++)
        if (fun == &(mod->entry.funcs[i]))
            break;
    ASSERT(i < mod->entry.num_of_funcs);
    if (p)
        ERTS_SMP_LC_ASSERT(erts_proc_lc_my_proc_locks(p) & ERTS_PROC_LOCK_MAIN
                           || erts_smp_thr_progress_is_blocking());
#endif
    if (p) {
        /* This is almost a normal nif call like in beam_emu,
           except that any heap consumed by the nif will be
           released without checking if anything in it is live.
           This is because we cannot do a GC here as we don't know
           the number of live registers that have to be preserved.
           This means that any heap part of the returned term may
           not be used outside this function. */
        struct enif_environment_t env;
        ErlHeapFragment *orig_hf = MBUF(p);
        ErlOffHeap orig_oh = MSO(p);
        Eterm *orig_htop = HEAP_TOP(p);
        ASSERT(is_internal_pid(p->common.id));
        MBUF(p) = NULL;
        clear_offheap(&MSO(p));

        erts_pre_nif(&env, p, mod, tracee);
#ifdef ERTS_NIF_ASSERT_IN_ENV
        env.dbg_disable_assert_in_env = 1;
#endif
        nif_result = (*fun->fptr)(&env, argc, argv);
        if (env.exception_thrown)
            nif_result = THE_NON_VALUE;
        erts_post_nif(&env);

        /* Free any offheap and heap fragments created in nif */
        if (MSO(p).first) {
            erts_cleanup_offheap(&MSO(p));
            clear_offheap(&MSO(p));
        }
        if (MBUF(p))
            free_message_buffer(MBUF(p));

        /* restore original heap fragment list */
        MBUF(p) = orig_hf;
        MSO(p) = orig_oh;
        HEAP_TOP(p) = orig_htop;
    } else {
        /* Nif call was done without a process context,
           so we create a phony one. */
        struct enif_msg_environment_t msg_env;
        pre_nif_noproc(&msg_env, mod, tracee);
#ifdef ERTS_NIF_ASSERT_IN_ENV
        msg_env.env.dbg_disable_assert_in_env = 1;
#endif
        nif_result = (*fun->fptr)(&msg_env.env, argc, argv);
        if (msg_env.env.exception_thrown)
            nif_result = THE_NON_VALUE;
        post_nif_noproc(&msg_env);
    }

    return nif_result;
}

#ifdef USE_VM_PROBES
void dtrace_nifenv_str(ErlNifEnv *env, char *process_buf)
{
    dtrace_pid_str(env->proc->common.id, process_buf);
}
#endif

#ifdef READONLY_CHECK
/* Use checksums to assert that NIFs do not write into inspected binaries
*/
static void readonly_check_dtor(struct enif_tmp_obj_t*);
static unsigned calc_checksum(unsigned char* ptr, unsigned size);

struct readonly_check_t
{
    struct enif_tmp_obj_t hdr;
    unsigned char* ptr;
    unsigned size;
    unsigned checksum;
};
static void add_readonly_check(ErlNifEnv* env, unsigned char* ptr, unsigned sz)
{
    ErtsAlcType_t allocator = is_proc_bound(env) ? ERTS_ALC_T_TMP : ERTS_ALC_T_NIF;
    struct readonly_check_t* obj = erts_alloc(allocator, 
					      sizeof(struct readonly_check_t));
    obj->hdr.allocator = allocator;
    obj->hdr.next = env->tmp_obj_list;
    env->tmp_obj_list = &obj->hdr;
    obj->hdr.dtor = &readonly_check_dtor;
    obj->ptr = ptr;
    obj->size = sz;
    obj->checksum = calc_checksum(ptr, sz);    
}
static void readonly_check_dtor(struct enif_tmp_obj_t* o)
{
    struct readonly_check_t* obj = (struct readonly_check_t*) o;
    unsigned chksum = calc_checksum(obj->ptr, obj->size);
    if (chksum != obj->checksum) { 
	fprintf(stderr, "\r\nReadonly data written by NIF, checksums differ"
		" %x != %x\r\nABORTING\r\n", chksum, obj->checksum);
	abort();
    }
    erts_free(obj->hdr.allocator,  obj);
}
static unsigned calc_checksum(unsigned char* ptr, unsigned size)
{
    unsigned i, sum = 0;
    for (i=0; i<size; i++) {
	sum ^= ptr[i] << ((i % 4)*8);
    }
    return sum;
}

#endif /* READONLY_CHECK */

#ifdef ERTS_NIF_ASSERT_IN_ENV
static void dbg_assert_in_env(ErlNifEnv* env, Eterm term,
                              int nr, const char* type, const char* func)
{
    Uint saved_used_size;
    Eterm* real_htop;

    if (is_immed(term)
        || (is_non_value(term) && env->exception_thrown)
        || erts_is_literal(term, ptr_val(term)))
        return;

    if (env->dbg_disable_assert_in_env) {
        /*
         * Trace nifs may cheat as built terms are discarded after return.
         * ToDo: Check if 'term' is part of argv[].
         */
        return;
    }

    if (env->heap_frag) {
        ASSERT(env->heap_frag == MBUF(env->proc));
        ASSERT(env->hp >= env->heap_frag->mem);
        ASSERT(env->hp <= env->heap_frag->mem + env->heap_frag->alloc_size);
        saved_used_size = env->heap_frag->used_size;
        env->heap_frag->used_size = env->hp - env->heap_frag->mem;
        real_htop = NULL;
    }
    else {
        real_htop = env->hp;
    }
    if (!erts_dbg_within_proc(ptr_val(term), env->proc, real_htop)) {
        fprintf(stderr, "\r\nFAILED ASSERTION in %s:\r\n", func);
        if (nr) {
            fprintf(stderr, "Term #%d of the %s is not from same ErlNifEnv.",
                    nr, type);
        }
        else {
            fprintf(stderr, "The %s is not from the same ErlNifEnv.", type);
        }
        fprintf(stderr, "\r\nABORTING\r\n");
        abort();
    }
    if (env->heap_frag) {
        env->heap_frag->used_size = saved_used_size;
    }
}
#endif

#ifdef HAVE_USE_DTRACE

#define MESSAGE_BUFSIZ 1024

static void get_string_maybe(ErlNifEnv *env, const ERL_NIF_TERM term,
		      char **ptr, char *buf, int bufsiz)
{
    ErlNifBinary str_bin;

    if (!enif_inspect_iolist_as_binary(env, term, &str_bin) ||
        str_bin.size > bufsiz) {
        *ptr = NULL;
    } else {
        memcpy(buf, (char *) str_bin.data, str_bin.size);
        buf[str_bin.size] = '\0';
        *ptr = buf;
    }
}

ERL_NIF_TERM erl_nif_user_trace_s1(ErlNifEnv* env, int argc,
                                   const ERL_NIF_TERM argv[])
{
    ErlNifBinary message_bin;
    DTRACE_CHARBUF(messagebuf, MESSAGE_BUFSIZ + 1);

    if (DTRACE_ENABLED(user_trace_s1)) {
	if (!enif_inspect_iolist_as_binary(env, argv[0], &message_bin) ||
	    message_bin.size > MESSAGE_BUFSIZ) {
	    return am_badarg;
	}
	memcpy(messagebuf, (char *) message_bin.data, message_bin.size);
        messagebuf[message_bin.size] = '\0';
	DTRACE1(user_trace_s1, messagebuf);
	return am_true;
    } else {
	return am_false;
    }
}

ERL_NIF_TERM erl_nif_user_trace_i4s4(ErlNifEnv* env, int argc,
                                     const ERL_NIF_TERM argv[])
{
    DTRACE_CHARBUF(procbuf, 32 + 1);
    DTRACE_CHARBUF(user_tagbuf, MESSAGE_BUFSIZ + 1);
    char *utbuf = NULL;
    ErlNifSInt64 i1, i2, i3, i4;
    DTRACE_CHARBUF(messagebuf1, MESSAGE_BUFSIZ + 1);
    DTRACE_CHARBUF(messagebuf2, MESSAGE_BUFSIZ + 1);
    DTRACE_CHARBUF(messagebuf3, MESSAGE_BUFSIZ + 1);
    DTRACE_CHARBUF(messagebuf4, MESSAGE_BUFSIZ + 1);
    char *mbuf1 = NULL, *mbuf2 = NULL, *mbuf3 = NULL, *mbuf4 = NULL;
    
    if (DTRACE_ENABLED(user_trace_i4s4)) {
	dtrace_nifenv_str(env, procbuf);
        get_string_maybe(env, argv[0], &utbuf, user_tagbuf, MESSAGE_BUFSIZ);
        if (! enif_get_int64(env, argv[1], &i1))
            i1 = 0;
        if (! enif_get_int64(env, argv[2], &i2))
            i2 = 0;
        if (! enif_get_int64(env, argv[3], &i3))
            i3 = 0;
        if (! enif_get_int64(env, argv[4], &i4))
            i4 = 0;
        get_string_maybe(env, argv[5], &mbuf1, messagebuf1, MESSAGE_BUFSIZ);
        get_string_maybe(env, argv[6], &mbuf2, messagebuf2, MESSAGE_BUFSIZ);
        get_string_maybe(env, argv[7], &mbuf3, messagebuf3, MESSAGE_BUFSIZ);
        get_string_maybe(env, argv[8], &mbuf4, messagebuf4, MESSAGE_BUFSIZ);
	DTRACE10(user_trace_i4s4, procbuf, utbuf,
		 i1, i2, i3, i4, mbuf1, mbuf2, mbuf3, mbuf4);
	return am_true;
    } else {
	return am_false;
    }
}

#define DTRACE10_LABEL(name, label, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9) \
    erlang_##name##label((a0), (a1), (a2), (a3), (a4), (a5), (a6), (a7), (a8), (a9))
#define N_STATEMENT(the_label) \
   case the_label: \
      if (DTRACE_ENABLED(user_trace_n##the_label)) { \
          dtrace_nifenv_str(env, procbuf); \
          get_string_maybe(env, argv[1], &utbuf, user_tagbuf, MESSAGE_BUFSIZ); \
          if (! enif_get_int64(env, argv[2], &i1)) \
              i1 = 0; \
          if (! enif_get_int64(env, argv[3], &i2)) \
              i2 = 0; \
          if (! enif_get_int64(env, argv[4], &i3)) \
              i3 = 0; \
          if (! enif_get_int64(env, argv[5], &i4)) \
              i4 = 0; \
          get_string_maybe(env, argv[6], &mbuf1, messagebuf1, MESSAGE_BUFSIZ); \
          get_string_maybe(env, argv[7], &mbuf2, messagebuf2, MESSAGE_BUFSIZ); \
          get_string_maybe(env, argv[8], &mbuf3, messagebuf3, MESSAGE_BUFSIZ); \
          get_string_maybe(env, argv[9], &mbuf4, messagebuf4, MESSAGE_BUFSIZ); \
          DTRACE10_LABEL(user_trace_n, the_label, procbuf, utbuf,    \
                         i1, i2, i3, i4, mbuf1, mbuf2, mbuf3, mbuf4); \
          return am_true; \
      } else { \
          return am_false; \
      } \
      break

ERL_NIF_TERM erl_nif_user_trace_n(ErlNifEnv* env, int argc,
				  const ERL_NIF_TERM argv[])
{
    DTRACE_CHARBUF(procbuf, 32 + 1);
    DTRACE_CHARBUF(user_tagbuf, MESSAGE_BUFSIZ + 1);
    char *utbuf = NULL;
    ErlNifSInt64 i1, i2, i3, i4;
    DTRACE_CHARBUF(messagebuf1, MESSAGE_BUFSIZ + 1);
    DTRACE_CHARBUF(messagebuf2, MESSAGE_BUFSIZ + 1);
    DTRACE_CHARBUF(messagebuf3, MESSAGE_BUFSIZ + 1);
    DTRACE_CHARBUF(messagebuf4, MESSAGE_BUFSIZ + 1);
    char *mbuf1 = NULL, *mbuf2 = NULL, *mbuf3 = NULL, *mbuf4 = NULL;
    ErlNifSInt64 label = 0;

    if (! enif_get_int64(env, argv[0], &label) || label < 0 || label > 1023) {
	return am_badarg;
    }
    switch (label) {
        N_STATEMENT(0);
        N_STATEMENT(1);
        N_STATEMENT(2);
        N_STATEMENT(3);
        N_STATEMENT(4);
        N_STATEMENT(5);
        N_STATEMENT(6);
        N_STATEMENT(7);
        N_STATEMENT(8);
        N_STATEMENT(9);
        N_STATEMENT(10);
        N_STATEMENT(11);
        N_STATEMENT(12);
        N_STATEMENT(13);
        N_STATEMENT(14);
        N_STATEMENT(15);
        N_STATEMENT(16);
        N_STATEMENT(17);
        N_STATEMENT(18);
        N_STATEMENT(19);
        N_STATEMENT(20);
        N_STATEMENT(21);
        N_STATEMENT(22);
        N_STATEMENT(23);
        N_STATEMENT(24);
        N_STATEMENT(25);
        N_STATEMENT(26);
        N_STATEMENT(27);
        N_STATEMENT(28);
        N_STATEMENT(29);
        N_STATEMENT(30);
        N_STATEMENT(31);
        N_STATEMENT(32);
        N_STATEMENT(33);
        N_STATEMENT(34);
        N_STATEMENT(35);
        N_STATEMENT(36);
        N_STATEMENT(37);
        N_STATEMENT(38);
        N_STATEMENT(39);
        N_STATEMENT(40);
        N_STATEMENT(41);
        N_STATEMENT(42);
        N_STATEMENT(43);
        N_STATEMENT(44);
        N_STATEMENT(45);
        N_STATEMENT(46);
        N_STATEMENT(47);
        N_STATEMENT(48);
        N_STATEMENT(49);
        N_STATEMENT(50);
        N_STATEMENT(51);
        N_STATEMENT(52);
        N_STATEMENT(53);
        N_STATEMENT(54);
        N_STATEMENT(55);
        N_STATEMENT(56);
        N_STATEMENT(57);
        N_STATEMENT(58);
        N_STATEMENT(59);
        N_STATEMENT(60);
        N_STATEMENT(61);
        N_STATEMENT(62);
        N_STATEMENT(63);
        N_STATEMENT(64);
        N_STATEMENT(65);
        N_STATEMENT(66);
        N_STATEMENT(67);
        N_STATEMENT(68);
        N_STATEMENT(69);
        N_STATEMENT(70);
        N_STATEMENT(71);
        N_STATEMENT(72);
        N_STATEMENT(73);
        N_STATEMENT(74);
        N_STATEMENT(75);
        N_STATEMENT(76);
        N_STATEMENT(77);
        N_STATEMENT(78);
        N_STATEMENT(79);
        N_STATEMENT(80);
        N_STATEMENT(81);
        N_STATEMENT(82);
        N_STATEMENT(83);
        N_STATEMENT(84);
        N_STATEMENT(85);
        N_STATEMENT(86);
        N_STATEMENT(87);
        N_STATEMENT(88);
        N_STATEMENT(89);
        N_STATEMENT(90);
        N_STATEMENT(91);
        N_STATEMENT(92);
        N_STATEMENT(93);
        N_STATEMENT(94);
        N_STATEMENT(95);
        N_STATEMENT(96);
        N_STATEMENT(97);
        N_STATEMENT(98);
        N_STATEMENT(99);
        N_STATEMENT(100);
        N_STATEMENT(101);
        N_STATEMENT(102);
        N_STATEMENT(103);
        N_STATEMENT(104);
        N_STATEMENT(105);
        N_STATEMENT(106);
        N_STATEMENT(107);
        N_STATEMENT(108);
        N_STATEMENT(109);
        N_STATEMENT(110);
        N_STATEMENT(111);
        N_STATEMENT(112);
        N_STATEMENT(113);
        N_STATEMENT(114);
        N_STATEMENT(115);
        N_STATEMENT(116);
        N_STATEMENT(117);
        N_STATEMENT(118);
        N_STATEMENT(119);
        N_STATEMENT(120);
        N_STATEMENT(121);
        N_STATEMENT(122);
        N_STATEMENT(123);
        N_STATEMENT(124);
        N_STATEMENT(125);
        N_STATEMENT(126);
        N_STATEMENT(127);
        N_STATEMENT(128);
        N_STATEMENT(129);
        N_STATEMENT(130);
        N_STATEMENT(131);
        N_STATEMENT(132);
        N_STATEMENT(133);
        N_STATEMENT(134);
        N_STATEMENT(135);
        N_STATEMENT(136);
        N_STATEMENT(137);
        N_STATEMENT(138);
        N_STATEMENT(139);
        N_STATEMENT(140);
        N_STATEMENT(141);
        N_STATEMENT(142);
        N_STATEMENT(143);
        N_STATEMENT(144);
        N_STATEMENT(145);
        N_STATEMENT(146);
        N_STATEMENT(147);
        N_STATEMENT(148);
        N_STATEMENT(149);
        N_STATEMENT(150);
        N_STATEMENT(151);
        N_STATEMENT(152);
        N_STATEMENT(153);
        N_STATEMENT(154);
        N_STATEMENT(155);
        N_STATEMENT(156);
        N_STATEMENT(157);
        N_STATEMENT(158);
        N_STATEMENT(159);
        N_STATEMENT(160);
        N_STATEMENT(161);
        N_STATEMENT(162);
        N_STATEMENT(163);
        N_STATEMENT(164);
        N_STATEMENT(165);
        N_STATEMENT(166);
        N_STATEMENT(167);
        N_STATEMENT(168);
        N_STATEMENT(169);
        N_STATEMENT(170);
        N_STATEMENT(171);
        N_STATEMENT(172);
        N_STATEMENT(173);
        N_STATEMENT(174);
        N_STATEMENT(175);
        N_STATEMENT(176);
        N_STATEMENT(177);
        N_STATEMENT(178);
        N_STATEMENT(179);
        N_STATEMENT(180);
        N_STATEMENT(181);
        N_STATEMENT(182);
        N_STATEMENT(183);
        N_STATEMENT(184);
        N_STATEMENT(185);
        N_STATEMENT(186);
        N_STATEMENT(187);
        N_STATEMENT(188);
        N_STATEMENT(189);
        N_STATEMENT(190);
        N_STATEMENT(191);
        N_STATEMENT(192);
        N_STATEMENT(193);
        N_STATEMENT(194);
        N_STATEMENT(195);
        N_STATEMENT(196);
        N_STATEMENT(197);
        N_STATEMENT(198);
        N_STATEMENT(199);
        N_STATEMENT(200);
        N_STATEMENT(201);
        N_STATEMENT(202);
        N_STATEMENT(203);
        N_STATEMENT(204);
        N_STATEMENT(205);
        N_STATEMENT(206);
        N_STATEMENT(207);
        N_STATEMENT(208);
        N_STATEMENT(209);
        N_STATEMENT(210);
        N_STATEMENT(211);
        N_STATEMENT(212);
        N_STATEMENT(213);
        N_STATEMENT(214);
        N_STATEMENT(215);
        N_STATEMENT(216);
        N_STATEMENT(217);
        N_STATEMENT(218);
        N_STATEMENT(219);
        N_STATEMENT(220);
        N_STATEMENT(221);
        N_STATEMENT(222);
        N_STATEMENT(223);
        N_STATEMENT(224);
        N_STATEMENT(225);
        N_STATEMENT(226);
        N_STATEMENT(227);
        N_STATEMENT(228);
        N_STATEMENT(229);
        N_STATEMENT(230);
        N_STATEMENT(231);
        N_STATEMENT(232);
        N_STATEMENT(233);
        N_STATEMENT(234);
        N_STATEMENT(235);
        N_STATEMENT(236);
        N_STATEMENT(237);
        N_STATEMENT(238);
        N_STATEMENT(239);
        N_STATEMENT(240);
        N_STATEMENT(241);
        N_STATEMENT(242);
        N_STATEMENT(243);
        N_STATEMENT(244);
        N_STATEMENT(245);
        N_STATEMENT(246);
        N_STATEMENT(247);
        N_STATEMENT(248);
        N_STATEMENT(249);
        N_STATEMENT(250);
        N_STATEMENT(251);
        N_STATEMENT(252);
        N_STATEMENT(253);
        N_STATEMENT(254);
        N_STATEMENT(255);
        N_STATEMENT(256);
        N_STATEMENT(257);
        N_STATEMENT(258);
        N_STATEMENT(259);
        N_STATEMENT(260);
        N_STATEMENT(261);
        N_STATEMENT(262);
        N_STATEMENT(263);
        N_STATEMENT(264);
        N_STATEMENT(265);
        N_STATEMENT(266);
        N_STATEMENT(267);
        N_STATEMENT(268);
        N_STATEMENT(269);
        N_STATEMENT(270);
        N_STATEMENT(271);
        N_STATEMENT(272);
        N_STATEMENT(273);
        N_STATEMENT(274);
        N_STATEMENT(275);
        N_STATEMENT(276);
        N_STATEMENT(277);
        N_STATEMENT(278);
        N_STATEMENT(279);
        N_STATEMENT(280);
        N_STATEMENT(281);
        N_STATEMENT(282);
        N_STATEMENT(283);
        N_STATEMENT(284);
        N_STATEMENT(285);
        N_STATEMENT(286);
        N_STATEMENT(287);
        N_STATEMENT(288);
        N_STATEMENT(289);
        N_STATEMENT(290);
        N_STATEMENT(291);
        N_STATEMENT(292);
        N_STATEMENT(293);
        N_STATEMENT(294);
        N_STATEMENT(295);
        N_STATEMENT(296);
        N_STATEMENT(297);
        N_STATEMENT(298);
        N_STATEMENT(299);
        N_STATEMENT(300);
        N_STATEMENT(301);
        N_STATEMENT(302);
        N_STATEMENT(303);
        N_STATEMENT(304);
        N_STATEMENT(305);
        N_STATEMENT(306);
        N_STATEMENT(307);
        N_STATEMENT(308);
        N_STATEMENT(309);
        N_STATEMENT(310);
        N_STATEMENT(311);
        N_STATEMENT(312);
        N_STATEMENT(313);
        N_STATEMENT(314);
        N_STATEMENT(315);
        N_STATEMENT(316);
        N_STATEMENT(317);
        N_STATEMENT(318);
        N_STATEMENT(319);
        N_STATEMENT(320);
        N_STATEMENT(321);
        N_STATEMENT(322);
        N_STATEMENT(323);
        N_STATEMENT(324);
        N_STATEMENT(325);
        N_STATEMENT(326);
        N_STATEMENT(327);
        N_STATEMENT(328);
        N_STATEMENT(329);
        N_STATEMENT(330);
        N_STATEMENT(331);
        N_STATEMENT(332);
        N_STATEMENT(333);
        N_STATEMENT(334);
        N_STATEMENT(335);
        N_STATEMENT(336);
        N_STATEMENT(337);
        N_STATEMENT(338);
        N_STATEMENT(339);
        N_STATEMENT(340);
        N_STATEMENT(341);
        N_STATEMENT(342);
        N_STATEMENT(343);
        N_STATEMENT(344);
        N_STATEMENT(345);
        N_STATEMENT(346);
        N_STATEMENT(347);
        N_STATEMENT(348);
        N_STATEMENT(349);
        N_STATEMENT(350);
        N_STATEMENT(351);
        N_STATEMENT(352);
        N_STATEMENT(353);
        N_STATEMENT(354);
        N_STATEMENT(355);
        N_STATEMENT(356);
        N_STATEMENT(357);
        N_STATEMENT(358);
        N_STATEMENT(359);
        N_STATEMENT(360);
        N_STATEMENT(361);
        N_STATEMENT(362);
        N_STATEMENT(363);
        N_STATEMENT(364);
        N_STATEMENT(365);
        N_STATEMENT(366);
        N_STATEMENT(367);
        N_STATEMENT(368);
        N_STATEMENT(369);
        N_STATEMENT(370);
        N_STATEMENT(371);
        N_STATEMENT(372);
        N_STATEMENT(373);
        N_STATEMENT(374);
        N_STATEMENT(375);
        N_STATEMENT(376);
        N_STATEMENT(377);
        N_STATEMENT(378);
        N_STATEMENT(379);
        N_STATEMENT(380);
        N_STATEMENT(381);
        N_STATEMENT(382);
        N_STATEMENT(383);
        N_STATEMENT(384);
        N_STATEMENT(385);
        N_STATEMENT(386);
        N_STATEMENT(387);
        N_STATEMENT(388);
        N_STATEMENT(389);
        N_STATEMENT(390);
        N_STATEMENT(391);
        N_STATEMENT(392);
        N_STATEMENT(393);
        N_STATEMENT(394);
        N_STATEMENT(395);
        N_STATEMENT(396);
        N_STATEMENT(397);
        N_STATEMENT(398);
        N_STATEMENT(399);
        N_STATEMENT(400);
        N_STATEMENT(401);
        N_STATEMENT(402);
        N_STATEMENT(403);
        N_STATEMENT(404);
        N_STATEMENT(405);
        N_STATEMENT(406);
        N_STATEMENT(407);
        N_STATEMENT(408);
        N_STATEMENT(409);
        N_STATEMENT(410);
        N_STATEMENT(411);
        N_STATEMENT(412);
        N_STATEMENT(413);
        N_STATEMENT(414);
        N_STATEMENT(415);
        N_STATEMENT(416);
        N_STATEMENT(417);
        N_STATEMENT(418);
        N_STATEMENT(419);
        N_STATEMENT(420);
        N_STATEMENT(421);
        N_STATEMENT(422);
        N_STATEMENT(423);
        N_STATEMENT(424);
        N_STATEMENT(425);
        N_STATEMENT(426);
        N_STATEMENT(427);
        N_STATEMENT(428);
        N_STATEMENT(429);
        N_STATEMENT(430);
        N_STATEMENT(431);
        N_STATEMENT(432);
        N_STATEMENT(433);
        N_STATEMENT(434);
        N_STATEMENT(435);
        N_STATEMENT(436);
        N_STATEMENT(437);
        N_STATEMENT(438);
        N_STATEMENT(439);
        N_STATEMENT(440);
        N_STATEMENT(441);
        N_STATEMENT(442);
        N_STATEMENT(443);
        N_STATEMENT(444);
        N_STATEMENT(445);
        N_STATEMENT(446);
        N_STATEMENT(447);
        N_STATEMENT(448);
        N_STATEMENT(449);
        N_STATEMENT(450);
        N_STATEMENT(451);
        N_STATEMENT(452);
        N_STATEMENT(453);
        N_STATEMENT(454);
        N_STATEMENT(455);
        N_STATEMENT(456);
        N_STATEMENT(457);
        N_STATEMENT(458);
        N_STATEMENT(459);
        N_STATEMENT(460);
        N_STATEMENT(461);
        N_STATEMENT(462);
        N_STATEMENT(463);
        N_STATEMENT(464);
        N_STATEMENT(465);
        N_STATEMENT(466);
        N_STATEMENT(467);
        N_STATEMENT(468);
        N_STATEMENT(469);
        N_STATEMENT(470);
        N_STATEMENT(471);
        N_STATEMENT(472);
        N_STATEMENT(473);
        N_STATEMENT(474);
        N_STATEMENT(475);
        N_STATEMENT(476);
        N_STATEMENT(477);
        N_STATEMENT(478);
        N_STATEMENT(479);
        N_STATEMENT(480);
        N_STATEMENT(481);
        N_STATEMENT(482);
        N_STATEMENT(483);
        N_STATEMENT(484);
        N_STATEMENT(485);
        N_STATEMENT(486);
        N_STATEMENT(487);
        N_STATEMENT(488);
        N_STATEMENT(489);
        N_STATEMENT(490);
        N_STATEMENT(491);
        N_STATEMENT(492);
        N_STATEMENT(493);
        N_STATEMENT(494);
        N_STATEMENT(495);
        N_STATEMENT(496);
        N_STATEMENT(497);
        N_STATEMENT(498);
        N_STATEMENT(499);
        N_STATEMENT(500);
        N_STATEMENT(501);
        N_STATEMENT(502);
        N_STATEMENT(503);
        N_STATEMENT(504);
        N_STATEMENT(505);
        N_STATEMENT(506);
        N_STATEMENT(507);
        N_STATEMENT(508);
        N_STATEMENT(509);
        N_STATEMENT(510);
        N_STATEMENT(511);
        N_STATEMENT(512);
        N_STATEMENT(513);
        N_STATEMENT(514);
        N_STATEMENT(515);
        N_STATEMENT(516);
        N_STATEMENT(517);
        N_STATEMENT(518);
        N_STATEMENT(519);
        N_STATEMENT(520);
        N_STATEMENT(521);
        N_STATEMENT(522);
        N_STATEMENT(523);
        N_STATEMENT(524);
        N_STATEMENT(525);
        N_STATEMENT(526);
        N_STATEMENT(527);
        N_STATEMENT(528);
        N_STATEMENT(529);
        N_STATEMENT(530);
        N_STATEMENT(531);
        N_STATEMENT(532);
        N_STATEMENT(533);
        N_STATEMENT(534);
        N_STATEMENT(535);
        N_STATEMENT(536);
        N_STATEMENT(537);
        N_STATEMENT(538);
        N_STATEMENT(539);
        N_STATEMENT(540);
        N_STATEMENT(541);
        N_STATEMENT(542);
        N_STATEMENT(543);
        N_STATEMENT(544);
        N_STATEMENT(545);
        N_STATEMENT(546);
        N_STATEMENT(547);
        N_STATEMENT(548);
        N_STATEMENT(549);
        N_STATEMENT(550);
        N_STATEMENT(551);
        N_STATEMENT(552);
        N_STATEMENT(553);
        N_STATEMENT(554);
        N_STATEMENT(555);
        N_STATEMENT(556);
        N_STATEMENT(557);
        N_STATEMENT(558);
        N_STATEMENT(559);
        N_STATEMENT(560);
        N_STATEMENT(561);
        N_STATEMENT(562);
        N_STATEMENT(563);
        N_STATEMENT(564);
        N_STATEMENT(565);
        N_STATEMENT(566);
        N_STATEMENT(567);
        N_STATEMENT(568);
        N_STATEMENT(569);
        N_STATEMENT(570);
        N_STATEMENT(571);
        N_STATEMENT(572);
        N_STATEMENT(573);
        N_STATEMENT(574);
        N_STATEMENT(575);
        N_STATEMENT(576);
        N_STATEMENT(577);
        N_STATEMENT(578);
        N_STATEMENT(579);
        N_STATEMENT(580);
        N_STATEMENT(581);
        N_STATEMENT(582);
        N_STATEMENT(583);
        N_STATEMENT(584);
        N_STATEMENT(585);
        N_STATEMENT(586);
        N_STATEMENT(587);
        N_STATEMENT(588);
        N_STATEMENT(589);
        N_STATEMENT(590);
        N_STATEMENT(591);
        N_STATEMENT(592);
        N_STATEMENT(593);
        N_STATEMENT(594);
        N_STATEMENT(595);
        N_STATEMENT(596);
        N_STATEMENT(597);
        N_STATEMENT(598);
        N_STATEMENT(599);
        N_STATEMENT(600);
        N_STATEMENT(601);
        N_STATEMENT(602);
        N_STATEMENT(603);
        N_STATEMENT(604);
        N_STATEMENT(605);
        N_STATEMENT(606);
        N_STATEMENT(607);
        N_STATEMENT(608);
        N_STATEMENT(609);
        N_STATEMENT(610);
        N_STATEMENT(611);
        N_STATEMENT(612);
        N_STATEMENT(613);
        N_STATEMENT(614);
        N_STATEMENT(615);
        N_STATEMENT(616);
        N_STATEMENT(617);
        N_STATEMENT(618);
        N_STATEMENT(619);
        N_STATEMENT(620);
        N_STATEMENT(621);
        N_STATEMENT(622);
        N_STATEMENT(623);
        N_STATEMENT(624);
        N_STATEMENT(625);
        N_STATEMENT(626);
        N_STATEMENT(627);
        N_STATEMENT(628);
        N_STATEMENT(629);
        N_STATEMENT(630);
        N_STATEMENT(631);
        N_STATEMENT(632);
        N_STATEMENT(633);
        N_STATEMENT(634);
        N_STATEMENT(635);
        N_STATEMENT(636);
        N_STATEMENT(637);
        N_STATEMENT(638);
        N_STATEMENT(639);
        N_STATEMENT(640);
        N_STATEMENT(641);
        N_STATEMENT(642);
        N_STATEMENT(643);
        N_STATEMENT(644);
        N_STATEMENT(645);
        N_STATEMENT(646);
        N_STATEMENT(647);
        N_STATEMENT(648);
        N_STATEMENT(649);
        N_STATEMENT(650);
        N_STATEMENT(651);
        N_STATEMENT(652);
        N_STATEMENT(653);
        N_STATEMENT(654);
        N_STATEMENT(655);
        N_STATEMENT(656);
        N_STATEMENT(657);
        N_STATEMENT(658);
        N_STATEMENT(659);
        N_STATEMENT(660);
        N_STATEMENT(661);
        N_STATEMENT(662);
        N_STATEMENT(663);
        N_STATEMENT(664);
        N_STATEMENT(665);
        N_STATEMENT(666);
        N_STATEMENT(667);
        N_STATEMENT(668);
        N_STATEMENT(669);
        N_STATEMENT(670);
        N_STATEMENT(671);
        N_STATEMENT(672);
        N_STATEMENT(673);
        N_STATEMENT(674);
        N_STATEMENT(675);
        N_STATEMENT(676);
        N_STATEMENT(677);
        N_STATEMENT(678);
        N_STATEMENT(679);
        N_STATEMENT(680);
        N_STATEMENT(681);
        N_STATEMENT(682);
        N_STATEMENT(683);
        N_STATEMENT(684);
        N_STATEMENT(685);
        N_STATEMENT(686);
        N_STATEMENT(687);
        N_STATEMENT(688);
        N_STATEMENT(689);
        N_STATEMENT(690);
        N_STATEMENT(691);
        N_STATEMENT(692);
        N_STATEMENT(693);
        N_STATEMENT(694);
        N_STATEMENT(695);
        N_STATEMENT(696);
        N_STATEMENT(697);
        N_STATEMENT(698);
        N_STATEMENT(699);
        N_STATEMENT(700);
        N_STATEMENT(701);
        N_STATEMENT(702);
        N_STATEMENT(703);
        N_STATEMENT(704);
        N_STATEMENT(705);
        N_STATEMENT(706);
        N_STATEMENT(707);
        N_STATEMENT(708);
        N_STATEMENT(709);
        N_STATEMENT(710);
        N_STATEMENT(711);
        N_STATEMENT(712);
        N_STATEMENT(713);
        N_STATEMENT(714);
        N_STATEMENT(715);
        N_STATEMENT(716);
        N_STATEMENT(717);
        N_STATEMENT(718);
        N_STATEMENT(719);
        N_STATEMENT(720);
        N_STATEMENT(721);
        N_STATEMENT(722);
        N_STATEMENT(723);
        N_STATEMENT(724);
        N_STATEMENT(725);
        N_STATEMENT(726);
        N_STATEMENT(727);
        N_STATEMENT(728);
        N_STATEMENT(729);
        N_STATEMENT(730);
        N_STATEMENT(731);
        N_STATEMENT(732);
        N_STATEMENT(733);
        N_STATEMENT(734);
        N_STATEMENT(735);
        N_STATEMENT(736);
        N_STATEMENT(737);
        N_STATEMENT(738);
        N_STATEMENT(739);
        N_STATEMENT(740);
        N_STATEMENT(741);
        N_STATEMENT(742);
        N_STATEMENT(743);
        N_STATEMENT(744);
        N_STATEMENT(745);
        N_STATEMENT(746);
        N_STATEMENT(747);
        N_STATEMENT(748);
        N_STATEMENT(749);
        N_STATEMENT(750);
        N_STATEMENT(751);
        N_STATEMENT(752);
        N_STATEMENT(753);
        N_STATEMENT(754);
        N_STATEMENT(755);
        N_STATEMENT(756);
        N_STATEMENT(757);
        N_STATEMENT(758);
        N_STATEMENT(759);
        N_STATEMENT(760);
        N_STATEMENT(761);
        N_STATEMENT(762);
        N_STATEMENT(763);
        N_STATEMENT(764);
        N_STATEMENT(765);
        N_STATEMENT(766);
        N_STATEMENT(767);
        N_STATEMENT(768);
        N_STATEMENT(769);
        N_STATEMENT(770);
        N_STATEMENT(771);
        N_STATEMENT(772);
        N_STATEMENT(773);
        N_STATEMENT(774);
        N_STATEMENT(775);
        N_STATEMENT(776);
        N_STATEMENT(777);
        N_STATEMENT(778);
        N_STATEMENT(779);
        N_STATEMENT(780);
        N_STATEMENT(781);
        N_STATEMENT(782);
        N_STATEMENT(783);
        N_STATEMENT(784);
        N_STATEMENT(785);
        N_STATEMENT(786);
        N_STATEMENT(787);
        N_STATEMENT(788);
        N_STATEMENT(789);
        N_STATEMENT(790);
        N_STATEMENT(791);
        N_STATEMENT(792);
        N_STATEMENT(793);
        N_STATEMENT(794);
        N_STATEMENT(795);
        N_STATEMENT(796);
        N_STATEMENT(797);
        N_STATEMENT(798);
        N_STATEMENT(799);
        N_STATEMENT(800);
        N_STATEMENT(801);
        N_STATEMENT(802);
        N_STATEMENT(803);
        N_STATEMENT(804);
        N_STATEMENT(805);
        N_STATEMENT(806);
        N_STATEMENT(807);
        N_STATEMENT(808);
        N_STATEMENT(809);
        N_STATEMENT(810);
        N_STATEMENT(811);
        N_STATEMENT(812);
        N_STATEMENT(813);
        N_STATEMENT(814);
        N_STATEMENT(815);
        N_STATEMENT(816);
        N_STATEMENT(817);
        N_STATEMENT(818);
        N_STATEMENT(819);
        N_STATEMENT(820);
        N_STATEMENT(821);
        N_STATEMENT(822);
        N_STATEMENT(823);
        N_STATEMENT(824);
        N_STATEMENT(825);
        N_STATEMENT(826);
        N_STATEMENT(827);
        N_STATEMENT(828);
        N_STATEMENT(829);
        N_STATEMENT(830);
        N_STATEMENT(831);
        N_STATEMENT(832);
        N_STATEMENT(833);
        N_STATEMENT(834);
        N_STATEMENT(835);
        N_STATEMENT(836);
        N_STATEMENT(837);
        N_STATEMENT(838);
        N_STATEMENT(839);
        N_STATEMENT(840);
        N_STATEMENT(841);
        N_STATEMENT(842);
        N_STATEMENT(843);
        N_STATEMENT(844);
        N_STATEMENT(845);
        N_STATEMENT(846);
        N_STATEMENT(847);
        N_STATEMENT(848);
        N_STATEMENT(849);
        N_STATEMENT(850);
        N_STATEMENT(851);
        N_STATEMENT(852);
        N_STATEMENT(853);
        N_STATEMENT(854);
        N_STATEMENT(855);
        N_STATEMENT(856);
        N_STATEMENT(857);
        N_STATEMENT(858);
        N_STATEMENT(859);
        N_STATEMENT(860);
        N_STATEMENT(861);
        N_STATEMENT(862);
        N_STATEMENT(863);
        N_STATEMENT(864);
        N_STATEMENT(865);
        N_STATEMENT(866);
        N_STATEMENT(867);
        N_STATEMENT(868);
        N_STATEMENT(869);
        N_STATEMENT(870);
        N_STATEMENT(871);
        N_STATEMENT(872);
        N_STATEMENT(873);
        N_STATEMENT(874);
        N_STATEMENT(875);
        N_STATEMENT(876);
        N_STATEMENT(877);
        N_STATEMENT(878);
        N_STATEMENT(879);
        N_STATEMENT(880);
        N_STATEMENT(881);
        N_STATEMENT(882);
        N_STATEMENT(883);
        N_STATEMENT(884);
        N_STATEMENT(885);
        N_STATEMENT(886);
        N_STATEMENT(887);
        N_STATEMENT(888);
        N_STATEMENT(889);
        N_STATEMENT(890);
        N_STATEMENT(891);
        N_STATEMENT(892);
        N_STATEMENT(893);
        N_STATEMENT(894);
        N_STATEMENT(895);
        N_STATEMENT(896);
        N_STATEMENT(897);
        N_STATEMENT(898);
        N_STATEMENT(899);
        N_STATEMENT(900);
        N_STATEMENT(901);
        N_STATEMENT(902);
        N_STATEMENT(903);
        N_STATEMENT(904);
        N_STATEMENT(905);
        N_STATEMENT(906);
        N_STATEMENT(907);
        N_STATEMENT(908);
        N_STATEMENT(909);
        N_STATEMENT(910);
        N_STATEMENT(911);
        N_STATEMENT(912);
        N_STATEMENT(913);
        N_STATEMENT(914);
        N_STATEMENT(915);
        N_STATEMENT(916);
        N_STATEMENT(917);
        N_STATEMENT(918);
        N_STATEMENT(919);
        N_STATEMENT(920);
        N_STATEMENT(921);
        N_STATEMENT(922);
        N_STATEMENT(923);
        N_STATEMENT(924);
        N_STATEMENT(925);
        N_STATEMENT(926);
        N_STATEMENT(927);
        N_STATEMENT(928);
        N_STATEMENT(929);
        N_STATEMENT(930);
        N_STATEMENT(931);
        N_STATEMENT(932);
        N_STATEMENT(933);
        N_STATEMENT(934);
        N_STATEMENT(935);
        N_STATEMENT(936);
        N_STATEMENT(937);
        N_STATEMENT(938);
        N_STATEMENT(939);
        N_STATEMENT(940);
        N_STATEMENT(941);
        N_STATEMENT(942);
        N_STATEMENT(943);
        N_STATEMENT(944);
        N_STATEMENT(945);
        N_STATEMENT(946);
        N_STATEMENT(947);
        N_STATEMENT(948);
        N_STATEMENT(949);
        N_STATEMENT(950);
    }
    return am_error;          /* NOTREACHED, shut up the compiler */
}

#endif /* HAVE_USE_DTRACE */
