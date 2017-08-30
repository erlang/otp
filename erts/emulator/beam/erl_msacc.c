/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2014-2015. All Rights Reserved.
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
 * Description: Microstate accounting.
 *
 *              We keep track of the different states that the
 *              Erlang VM threads are in, in order to provide
 *              performance/debugging statistics. There is a
 *              small overhead in enabling this, but in the big
 *              scheme of things it should be negligible.
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#define ERTS_MSACC_STATE_STRINGS 1

#include "sys.h"
#include "global.h"
#include "erl_threads.h"
#include "erl_bif_unique.h"
#include "erl_map.h"
#include "erl_msacc.h"

#if ERTS_ENABLE_MSACC

static Eterm erts_msacc_gather_stats(ErtsMsAcc *msacc, Eterm **hpp, Uint *szp);
static void erts_msacc_reset(ErtsMsAcc *msacc);
static ErtsMsAcc* get_msacc(void);

#ifdef USE_THREADS
erts_tsd_key_t ERTS_WRITE_UNLIKELY(erts_msacc_key);
#else
ErtsMsAcc *ERTS_WRITE_UNLIKELY(erts_msacc) = NULL;
#endif
int ERTS_WRITE_UNLIKELY(erts_msacc_enabled);

static Eterm *erts_msacc_state_atoms = NULL;
static erts_rwmtx_t msacc_mutex;
static ErtsMsAcc *msacc_managed = NULL;
#ifdef USE_THREADS
static ErtsMsAcc *msacc_unmanaged = NULL;
static Uint msacc_unmanaged_count = 0;
#endif

/* we have to split initiation as atoms are not inited in early init */
void erts_msacc_early_init(void) {
#ifndef ERTS_MSACC_ALWAYS_ON
    erts_msacc_enabled = 0;
#endif
    erts_rwmtx_init(&msacc_mutex,"msacc_list_mutex");
#ifdef USE_THREADS
    erts_tsd_key_create(&erts_msacc_key,"erts_msacc_key");
#else
    erts_msacc = NULL;
#endif
}

void erts_msacc_init(void) {
    int i;
    erts_msacc_state_atoms = erts_alloc(ERTS_ALC_T_MSACC,
                                        sizeof(Eterm)*ERTS_MSACC_STATE_COUNT);
    for (i = 0; i < ERTS_MSACC_STATE_COUNT; i++) {
        erts_msacc_state_atoms[i] = am_atom_put(erts_msacc_states[i],
                                                strlen(erts_msacc_states[i]));
    }
}

void erts_msacc_init_thread(char *type, int id, int managed) {
    ErtsMsAcc *msacc;

    msacc = erts_alloc(ERTS_ALC_T_MSACC, sizeof(ErtsMsAcc));

    msacc->type = strdup(type);
    msacc->id = make_small(id);
    msacc->unmanaged = !managed;
    msacc->tid = erts_thr_self();
    msacc->perf_counter = 0;

#ifdef USE_THREADS
    erts_rwmtx_rwlock(&msacc_mutex);
    if (!managed) {
        erts_mtx_init(&msacc->mtx,"msacc_unmanaged_mutex");
        msacc->next = msacc_unmanaged;
        msacc_unmanaged = msacc;
        msacc_unmanaged_count++;
        ERTS_MSACC_TSD_SET(msacc);
    } else {
        msacc->next = msacc_managed;
        msacc_managed = msacc;
    }
    erts_rwmtx_rwunlock(&msacc_mutex);
#else
    msacc_managed = msacc;
#endif

    erts_msacc_reset(msacc);

#ifdef ERTS_MSACC_ALWAYS_ON
    ERTS_MSACC_TSD_SET(msacc);
    msacc->perf_counter = erts_sys_perf_counter();
    msacc->state = ERTS_MSACC_STATE_OTHER;
#endif
}

/*
 * Creates a structure looking like this
 * #{ type => scheduler, id => 1, counters => #{ State1 => Counter1 ... StateN => CounterN}}
 */
static
Eterm erts_msacc_gather_stats(ErtsMsAcc *msacc, Eterm **hpp, Uint *szp) {
    int i;
    Eterm *hp;
    Eterm key, state_key, state_map;
    Eterm res = THE_NON_VALUE;
    flatmap_t *map;

    if (szp) {
      *szp += MAP_HEADER_FLATMAP_SZ + 1 + 2*(3);
      *szp += MAP_HEADER_FLATMAP_SZ + 1 + 2*(ERTS_MSACC_STATE_COUNT);
      for (i = 0; i < ERTS_MSACC_STATE_COUNT; i++) {
          (void)erts_bld_sint64(NULL,szp,(Sint64)msacc->perf_counters[i]);
#ifdef ERTS_MSACC_STATE_COUNTERS
          (void)erts_bld_uint64(NULL,szp,msacc->state_counters[i]);
          *szp += 3; /* tuple to put state+perf counter in */
#endif
      }
    }

    if (hpp) {
        Eterm counters[ERTS_MSACC_STATE_COUNT];
        hp = *hpp;
        for (i = 0; i < ERTS_MSACC_STATE_COUNT; i++) {
            Eterm counter = erts_bld_sint64(&hp,NULL,(Sint64)msacc->perf_counters[i]);
#ifdef ERTS_MSACC_STATE_COUNTERS
            Eterm counter__ = erts_bld_uint64(&hp,NULL,msacc->state_counters[i]);
            counters[i] = TUPLE2(hp,counter,counter__);
            hp += 3;
#else
            counters[i] = counter;
#endif
        }

      key = TUPLE3(hp,am_counters,am_id,am_type);
      hp += 4;

      state_key = make_tuple(hp);
      hp[0] = make_arityval(ERTS_MSACC_STATE_COUNT);
      
      for (i = 0; i < ERTS_MSACC_STATE_COUNT; i++)
        hp[1+i] = erts_msacc_state_atoms[i];
      hp += 1 + ERTS_MSACC_STATE_COUNT;

      map = (flatmap_t*)hp;
      hp += MAP_HEADER_FLATMAP_SZ;
      map->thing_word = MAP_HEADER_FLATMAP;
      map->size = ERTS_MSACC_STATE_COUNT;
      map->keys = state_key;
      for (i = 0; i < ERTS_MSACC_STATE_COUNT; i++)
            hp[i] = counters[i];
      hp += ERTS_MSACC_STATE_COUNT;
      state_map = make_flatmap(map);

      map = (flatmap_t*)hp;
      hp += MAP_HEADER_FLATMAP_SZ;
      map->thing_word = MAP_HEADER_FLATMAP;
      map->size = 3;
      map->keys = key;
      hp[0] = state_map;
      hp[1] = msacc->id;
      hp[2] = am_atom_put(msacc->type,strlen(msacc->type));
      hp += 3;

      *hpp = hp;
      res = make_flatmap(map);
    }

    return res;
}

typedef struct {
    int action;
    Process *proc;
    Eterm ref;
    Eterm ref_heap[REF_THING_SIZE];
    Uint req_sched;
    erts_smp_atomic32_t refc;
} ErtsMSAccReq;

static ErtsMsAcc* get_msacc(void) {
    ErtsMsAcc *msacc;
    erts_rwmtx_rlock(&msacc_mutex);
    msacc = msacc_managed;
    while (!erts_equal_tids(msacc->tid,erts_thr_self())) {
        msacc = msacc->next;
        ASSERT(msacc != NULL);
    }
    erts_rwmtx_runlock(&msacc_mutex);
    return msacc;
}

static void send_reply(ErtsMsAcc *msacc, ErtsMSAccReq *msaccrp) {
    ErtsSchedulerData *esdp = erts_get_scheduler_data();
    Process *rp = msaccrp->proc;
    ErtsMessage *msgp = NULL;
    Eterm **hpp, *hp;
    Eterm ref_copy = NIL, msg;
    Uint sz, *szp;
    ErlOffHeap *ohp = NULL;
    ErtsProcLocks rp_locks = (esdp && msaccrp->req_sched == esdp->no
                              ? ERTS_PROC_LOCK_MAIN : 0);

    sz = 0;
    hpp = NULL;
    szp = &sz;

    if (msacc->unmanaged) erts_mtx_lock(&msacc->mtx);

    while (1) {
	if (hpp)
            ref_copy = STORE_NC(hpp, ohp, msaccrp->ref);
	else
            *szp += REF_THING_SIZE;

	if (msaccrp->action != ERTS_MSACC_GATHER)
            msg = ref_copy;
	else {
            msg = erts_msacc_gather_stats(msacc, hpp, szp);
            msg = erts_bld_tuple(hpp, szp, 2, ref_copy, msg);
	}
	if (hpp)
            break;

	msgp = erts_alloc_message_heap(rp, &rp_locks, sz, &hp, &ohp);
        hpp = &hp;
	szp = NULL;
    }

    if (msacc->unmanaged) erts_mtx_unlock(&msacc->mtx);

    erts_queue_message(rp, rp_locks, msgp, msg, am_system);

    if (esdp && msaccrp->req_sched == esdp->no)
	rp_locks &= ~ERTS_PROC_LOCK_MAIN;

    if (rp_locks)
	erts_smp_proc_unlock(rp, rp_locks);

}

static void
reply_msacc(void *vmsaccrp)
{
    ErtsMsAcc *msacc = ERTS_MSACC_TSD_GET();
    ErtsMSAccReq *msaccrp = (ErtsMSAccReq *) vmsaccrp;

    ASSERT(!msacc || !msacc->unmanaged);

    if (msaccrp->action == ERTS_MSACC_ENABLE && !msacc) {
        msacc = get_msacc();

        msacc->perf_counter = erts_sys_perf_counter();

        msacc->state = ERTS_MSACC_STATE_OTHER;

        ERTS_MSACC_TSD_SET(msacc);

    } else if (msaccrp->action == ERTS_MSACC_DISABLE && msacc) {
        ERTS_MSACC_TSD_SET(NULL);
    } else if (msaccrp->action == ERTS_MSACC_RESET) {
        msacc = msacc ? msacc : get_msacc();
        erts_msacc_reset(msacc);
    } else if (msaccrp->action == ERTS_MSACC_GATHER && !msacc) {
        msacc = get_msacc();
    }

    ASSERT(!msacc || !msacc->unmanaged);

    send_reply(msacc, msaccrp);

    erts_proc_dec_refc(msaccrp->proc);

    if (erts_smp_atomic32_dec_read_nob(&msaccrp->refc) == 0)
      erts_free(ERTS_ALC_T_MSACC, vmsaccrp);
}

static void erts_msacc_reset(ErtsMsAcc *msacc) {
  int i;
  if (msacc->unmanaged) erts_mtx_lock(&msacc->mtx);

  for (i = 0; i < ERTS_MSACC_STATE_COUNT; i++) {
      msacc->perf_counters[i] = 0;
#ifdef ERTS_MSACC_STATE_COUNTERS
      msacc->state_counters[i] = 0;
#endif
  }

  if (msacc->unmanaged) erts_mtx_unlock(&msacc->mtx);
}

#endif /* ERTS_ENABLE_MSACC */


/*
 * This function is responsible for enabling, disabling, resetting and
 * gathering data related to microstate accounting.
 *
 * Managed threads and unmanaged threads are handled differently.
 *   - managed threads get a misc_aux job telling them to switch on msacc
 *   - unmanaged have some fields protected by a mutex that has to be taken
 *     before any values can be updated
 *
 * For performance reasons there is also a global value erts_msacc_enabled
 * that controls the state of all threads. Statistics gathering is only on
 * if erts_msacc_enabled && msacc is true.
 */
Eterm
erts_msacc_request(Process *c_p, int action, Eterm *threads)
{
#ifdef ERTS_ENABLE_MSACC
    ErtsMsAcc *msacc =  ERTS_MSACC_TSD_GET();
    ErtsSchedulerData *esdp = erts_proc_sched_data(c_p);
    Eterm ref;
    ErtsMSAccReq *msaccrp;
    Eterm *hp;


#ifdef ERTS_MSACC_ALWAYS_ON
    if (action == ERTS_MSACC_ENABLE || action == ERTS_MSACC_DISABLE)
        return THE_NON_VALUE;
#else
    /* take care of double enable, and double disable here */
    if (msacc && action == ERTS_MSACC_ENABLE) {
        return THE_NON_VALUE;
    } else if (!msacc && action == ERTS_MSACC_DISABLE) {
        return THE_NON_VALUE;
    }
#endif

    ref = erts_make_ref(c_p);

    msaccrp = erts_alloc(ERTS_ALC_T_MSACC, sizeof(ErtsMSAccReq));
    hp = &msaccrp->ref_heap[0];

    msaccrp->action = action;
    msaccrp->proc = c_p;
    msaccrp->ref = STORE_NC(&hp, NULL, ref);
    msaccrp->req_sched = esdp->no;

#ifdef ERTS_SMP
    *threads = erts_no_schedulers;
    *threads += 1; /* aux thread */
#else
    *threads = 1;
#endif

    erts_smp_atomic32_init_nob(&msaccrp->refc,(erts_aint32_t)*threads);

    erts_proc_add_refc(c_p, *threads);

    if (erts_no_schedulers > 1)
	erts_schedule_multi_misc_aux_work(1,
                                          erts_no_schedulers,
                                          reply_msacc,
                                          (void *) msaccrp);
#ifdef ERTS_SMP
    /* aux thread */
    erts_schedule_misc_aux_work(0, reply_msacc, (void *) msaccrp);
#endif

#ifdef USE_THREADS
    /* Manage unmanaged threads */
    switch (action) {
    case ERTS_MSACC_GATHER: {
        Uint unmanaged_count;
        ErtsMsAcc *msacc, **unmanaged;
        int i = 0;

        /* we copy a list of pointers here so that we do not have to have
           the msacc_mutex when sending messages */
        erts_rwmtx_rlock(&msacc_mutex);
        unmanaged_count = msacc_unmanaged_count;
        unmanaged = erts_alloc(ERTS_ALC_T_MSACC,
                               sizeof(ErtsMsAcc*)*unmanaged_count);

        for (i = 0, msacc = msacc_unmanaged;
             i < unmanaged_count;
             i++, msacc = msacc->next) {
            unmanaged[i] = msacc;
        }
        erts_rwmtx_runlock(&msacc_mutex);

        for (i = 0; i < unmanaged_count; i++) {
            erts_mtx_lock(&unmanaged[i]->mtx);
            if (unmanaged[i]->perf_counter) {
                ErtsSysPerfCounter perf_counter;
                /* if enabled update stats */
                perf_counter = erts_sys_perf_counter();
                unmanaged[i]->perf_counters[unmanaged[i]->state] +=
                    perf_counter - unmanaged[i]->perf_counter;
                unmanaged[i]->perf_counter = perf_counter;
            }
            erts_mtx_unlock(&unmanaged[i]->mtx);
            send_reply(unmanaged[i],msaccrp);
        }
        erts_free(ERTS_ALC_T_MSACC,unmanaged);
        /* We have just sent unmanaged_count messages, so bump no of threads */
        *threads += unmanaged_count;
        break;
    }
    case ERTS_MSACC_RESET: {
        ErtsMsAcc *msacc;
        erts_rwmtx_rlock(&msacc_mutex);
        for (msacc = msacc_unmanaged; msacc != NULL; msacc = msacc->next)
            erts_msacc_reset(msacc);
        erts_rwmtx_runlock(&msacc_mutex);
        break;
    }
    case ERTS_MSACC_ENABLE: {
        erts_rwmtx_rlock(&msacc_mutex);
        for (msacc = msacc_unmanaged; msacc != NULL; msacc = msacc->next) {
            erts_mtx_lock(&msacc->mtx);
            msacc->perf_counter = erts_sys_perf_counter();
            /* we assume the unmanaged thread is sleeping */
            msacc->state = ERTS_MSACC_STATE_SLEEP;
            erts_mtx_unlock(&msacc->mtx);
        }
        erts_rwmtx_runlock(&msacc_mutex);
        break;
    }
    case ERTS_MSACC_DISABLE: {
        ErtsSysPerfCounter perf_counter;
        erts_rwmtx_rlock(&msacc_mutex);
        /* make sure to update stats with latest results */
        for (msacc = msacc_unmanaged; msacc != NULL; msacc = msacc->next) {
            erts_mtx_lock(&msacc->mtx);
            perf_counter = erts_sys_perf_counter();
            msacc->perf_counters[msacc->state] += perf_counter - msacc->perf_counter;
            msacc->perf_counter = 0;
            erts_mtx_unlock(&msacc->mtx);
        }
        erts_rwmtx_runlock(&msacc_mutex);
        break;
    }
    default: { ASSERT(0); }
    }

#endif

    *threads = make_small(*threads);

    reply_msacc((void *) msaccrp);

#ifndef ERTS_MSACC_ALWAYS_ON
    /* enable/disable the global value */
    if (action == ERTS_MSACC_ENABLE) {
        erts_msacc_enabled = 1;
    } else if (action == ERTS_MSACC_DISABLE) {
        erts_msacc_enabled = 0;
    }
#endif

    return ref;
#else
    return THE_NON_VALUE;
#endif
}
