/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2010-2017. All Rights Reserved.
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
 * Description:	CPU topology and related functionality
 *
 * Author: 	Rickard Green
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include <ctype.h>

#include "global.h"
#include "error.h"
#include "bif.h"
#include "erl_cpu_topology.h"
#include "erl_flxctr.h"

#define ERTS_MAX_READER_GROUPS 64

/*
 * Cpu topology hierarchy.
 */
#define ERTS_TOPOLOGY_NODE		0
#define ERTS_TOPOLOGY_PROCESSOR		1
#define ERTS_TOPOLOGY_PROCESSOR_NODE	2
#define ERTS_TOPOLOGY_CORE		3
#define ERTS_TOPOLOGY_THREAD		4
#define ERTS_TOPOLOGY_LOGICAL		5

#define ERTS_TOPOLOGY_MAX_DEPTH		6

typedef struct {
    int bind_id;
    int bound_id;
} ErtsCpuBindData;

static erts_cpu_info_t *cpuinfo;

static int max_main_threads;
static int reader_groups;
static int decentralized_counter_groups;

static ErtsCpuBindData *scheduler2cpu_map;
static erts_rwmtx_t cpuinfo_rwmtx;

typedef enum {
    ERTS_CPU_BIND_UNDEFINED,
    ERTS_CPU_BIND_SPREAD,
    ERTS_CPU_BIND_PROCESSOR_SPREAD,
    ERTS_CPU_BIND_THREAD_SPREAD,
    ERTS_CPU_BIND_THREAD_NO_NODE_PROCESSOR_SPREAD,
    ERTS_CPU_BIND_NO_NODE_PROCESSOR_SPREAD,
    ERTS_CPU_BIND_NO_NODE_THREAD_SPREAD,
    ERTS_CPU_BIND_NO_SPREAD,
    ERTS_CPU_BIND_NONE
} ErtsCpuBindOrder;

#define ERTS_CPU_BIND_DEFAULT_BIND \
  ERTS_CPU_BIND_THREAD_NO_NODE_PROCESSOR_SPREAD

static int no_cpu_groups_callbacks;
static ErtsCpuBindOrder cpu_bind_order;

static erts_cpu_topology_t *user_cpudata;
static int user_cpudata_size;
static erts_cpu_topology_t *system_cpudata;
static int system_cpudata_size;

typedef struct {
    int level[ERTS_TOPOLOGY_MAX_DEPTH+1];
} erts_avail_cput;

typedef struct {
    int id;
    int sub_levels;
    int cpu_groups;
} erts_cpu_groups_count_t;

typedef struct {
    int logical;
    int cpu_group;
} erts_cpu_groups_map_array_t;

typedef struct erts_cpu_groups_callback_list_t_ erts_cpu_groups_callback_list_t;
struct erts_cpu_groups_callback_list_t_ {
    erts_cpu_groups_callback_list_t *next;
    erts_cpu_groups_callback_t callback;
    void *arg;
};

typedef struct erts_cpu_groups_map_t_ erts_cpu_groups_map_t;
struct erts_cpu_groups_map_t_ {
    erts_cpu_groups_map_t *next;
    int groups;
    erts_cpu_groups_map_array_t *array;
    int size;
    int logical_processors;
    erts_cpu_groups_callback_list_t *callback_list;
};

typedef struct {
    erts_cpu_groups_callback_t callback;
    int ix;
    void *arg;
} erts_cpu_groups_callback_call_t;

static erts_cpu_groups_map_t *cpu_groups_maps;

static erts_cpu_groups_map_t *reader_groups_map;

static erts_cpu_groups_map_t *decentralized_counter_groups_map;

#define ERTS_TOPOLOGY_CG ERTS_TOPOLOGY_MAX_DEPTH

#define ERTS_MAX_CPU_TOPOLOGY_ID ((int) 0xffff)

static void cpu_bind_order_sort(erts_cpu_topology_t *cpudata,
				int size,
				ErtsCpuBindOrder bind_order,
				int mk_seq);
static void write_schedulers_bind_change(erts_cpu_topology_t *cpudata, int size);

static void reader_groups_callback(int, ErtsSchedulerData *, int, void *);
static void flxctr_groups_callback(int, ErtsSchedulerData *, int, void *);
static erts_cpu_groups_map_t *add_cpu_groups(int groups,
					     erts_cpu_groups_callback_t callback,
					     void *arg);
static void update_cpu_groups_maps(void);
static void make_cpu_groups_map(erts_cpu_groups_map_t *map, int test);
static int cpu_groups_lookup(erts_cpu_groups_map_t *map,
			     ErtsSchedulerData *esdp);

static void create_tmp_cpu_topology_copy(erts_cpu_topology_t **cpudata,
					 int *cpudata_size);
static void destroy_tmp_cpu_topology_copy(erts_cpu_topology_t *cpudata);

static int
int_cmp(const void *vx, const void *vy)
{
    return *((int *) vx) - *((int *) vy);
}

static int
cpu_spread_order_cmp(const void *vx, const void *vy)
{
    erts_cpu_topology_t *x = (erts_cpu_topology_t *) vx;
    erts_cpu_topology_t *y = (erts_cpu_topology_t *) vy;

    if (x->thread != y->thread)
	return x->thread - y->thread;
    if (x->core != y->core)
	return x->core - y->core;
    if (x->processor_node != y->processor_node)
	return x->processor_node - y->processor_node;
    if (x->processor != y->processor)
	return x->processor - y->processor;
    if (x->node != y->node)
	return x->node - y->node;
    return 0;
}

static int
cpu_processor_spread_order_cmp(const void *vx, const void *vy)
{
    erts_cpu_topology_t *x = (erts_cpu_topology_t *) vx;
    erts_cpu_topology_t *y = (erts_cpu_topology_t *) vy;

    if (x->thread != y->thread)
	return x->thread - y->thread;
    if (x->processor_node != y->processor_node)
	return x->processor_node - y->processor_node;
    if (x->core != y->core)
	return x->core - y->core;
    if (x->node != y->node)
	return x->node - y->node;
    if (x->processor != y->processor)
	return x->processor - y->processor;
    return 0;
}

static int
cpu_thread_spread_order_cmp(const void *vx, const void *vy)
{
    erts_cpu_topology_t *x = (erts_cpu_topology_t *) vx;
    erts_cpu_topology_t *y = (erts_cpu_topology_t *) vy;

    if (x->thread != y->thread)
	return x->thread - y->thread;
    if (x->node != y->node)
	return x->node - y->node;
    if (x->processor != y->processor)
	return x->processor - y->processor;
    if (x->processor_node != y->processor_node)
	return x->processor_node - y->processor_node;
    if (x->core != y->core)
	return x->core - y->core;
    return 0;
}

static int
cpu_thread_no_node_processor_spread_order_cmp(const void *vx, const void *vy)
{
    erts_cpu_topology_t *x = (erts_cpu_topology_t *) vx;
    erts_cpu_topology_t *y = (erts_cpu_topology_t *) vy;

    if (x->thread != y->thread)
	return x->thread - y->thread;
    if (x->node != y->node)
	return x->node - y->node;
    if (x->core != y->core)
	return x->core - y->core;
    if (x->processor != y->processor)
	return x->processor - y->processor;
    return 0;
}

static int
cpu_no_node_processor_spread_order_cmp(const void *vx, const void *vy)
{
    erts_cpu_topology_t *x = (erts_cpu_topology_t *) vx;
    erts_cpu_topology_t *y = (erts_cpu_topology_t *) vy;

    if (x->node != y->node)
	return x->node - y->node;
    if (x->thread != y->thread)
	return x->thread - y->thread;
    if (x->core != y->core)
	return x->core - y->core;
    if (x->processor != y->processor)
	return x->processor - y->processor;
    return 0;
}

static int
cpu_no_node_thread_spread_order_cmp(const void *vx, const void *vy)
{
    erts_cpu_topology_t *x = (erts_cpu_topology_t *) vx;
    erts_cpu_topology_t *y = (erts_cpu_topology_t *) vy;

    if (x->node != y->node)
	return x->node - y->node;
    if (x->thread != y->thread)
	return x->thread - y->thread;
    if (x->processor != y->processor)
	return x->processor - y->processor;
    if (x->core != y->core)
	return x->core - y->core;
    return 0;
}

static int
cpu_no_spread_order_cmp(const void *vx, const void *vy)
{
    erts_cpu_topology_t *x = (erts_cpu_topology_t *) vx;
    erts_cpu_topology_t *y = (erts_cpu_topology_t *) vy;

    if (x->node != y->node)
	return x->node - y->node;
    if (x->processor != y->processor)
	return x->processor - y->processor;
    if (x->processor_node != y->processor_node)
	return x->processor_node - y->processor_node;
    if (x->core != y->core)
	return x->core - y->core;
    if (x->thread != y->thread)
	return x->thread - y->thread;
    return 0;
}

static ERTS_INLINE void
make_cpudata_id_seq(erts_cpu_topology_t *cpudata, int size, int no_node)
{
    int ix;
    int node = -1;
    int processor = -1;
    int processor_node = -1;
    int processor_node_node = -1;
    int core = -1;
    int thread = -1;
    int old_node = -1;
    int old_processor = -1;
    int old_processor_node = -1;
    int old_core = -1;
    int old_thread = -1;

    for (ix = 0; ix < size; ix++) {
	if (!no_node || cpudata[ix].node >= 0) {
	    if (old_node == cpudata[ix].node)
		cpudata[ix].node = node;
	    else {
		old_node = cpudata[ix].node;
		old_processor = processor = -1;
		if (!no_node)
		    old_processor_node = processor_node = -1;
		old_core = core = -1;
		old_thread = thread = -1;
		if (no_node || cpudata[ix].node >= 0)
		    cpudata[ix].node = ++node;
	    }
	}
	if (old_processor == cpudata[ix].processor)
	    cpudata[ix].processor = processor;
	else {
	    old_processor = cpudata[ix].processor;
	    if (!no_node)
		processor_node_node = old_processor_node = processor_node = -1;
	    old_core = core = -1;
	    old_thread = thread = -1;
	    cpudata[ix].processor = ++processor;
	}
	if (no_node && cpudata[ix].processor_node < 0)
	    old_processor_node = -1;
	else {
	    if (old_processor_node == cpudata[ix].processor_node) {
		if (no_node)
		    cpudata[ix].node = cpudata[ix].processor_node = node;
		else {
		    if (processor_node_node >= 0)
			cpudata[ix].node = processor_node_node;
		    cpudata[ix].processor_node = processor_node;
		}
	    }
	    else {
		old_processor_node = cpudata[ix].processor_node;
		old_core = core = -1;
		old_thread = thread = -1;
		if (no_node)
		    cpudata[ix].node = cpudata[ix].processor_node = ++node;
		else {
		    cpudata[ix].node = processor_node_node = ++node;
		    cpudata[ix].processor_node = ++processor_node;
		}
	    }
	}
	if (!no_node && cpudata[ix].processor_node < 0)
	    cpudata[ix].processor_node = 0;
	if (old_core == cpudata[ix].core)
	    cpudata[ix].core = core;
	else {
	    old_core = cpudata[ix].core;
	    old_thread = thread = -1;
	    cpudata[ix].core = ++core;
	}
	if (old_thread == cpudata[ix].thread)
	    cpudata[ix].thread = thread;
	else
	    old_thread = cpudata[ix].thread = ++thread;
    }
}

static void
cpu_bind_order_sort(erts_cpu_topology_t *cpudata,
		    int size,
		    ErtsCpuBindOrder bind_order,
		    int mk_seq)
{
    if (size > 1) {
	int no_node = 0;
	int (*cmp_func)(const void *, const void *);
	switch (bind_order) {
	case ERTS_CPU_BIND_SPREAD:
	    cmp_func = cpu_spread_order_cmp;
	    break;
	case ERTS_CPU_BIND_PROCESSOR_SPREAD:
	    cmp_func = cpu_processor_spread_order_cmp;
	    break;
	case ERTS_CPU_BIND_THREAD_SPREAD:
	    cmp_func = cpu_thread_spread_order_cmp;
	    break;
	case ERTS_CPU_BIND_THREAD_NO_NODE_PROCESSOR_SPREAD:
	    no_node = 1;
	    cmp_func = cpu_thread_no_node_processor_spread_order_cmp;
	    break;
	case ERTS_CPU_BIND_NO_NODE_PROCESSOR_SPREAD:
	    no_node = 1;
	    cmp_func = cpu_no_node_processor_spread_order_cmp;
	    break;
	case ERTS_CPU_BIND_NO_NODE_THREAD_SPREAD:
	    no_node = 1;
	    cmp_func = cpu_no_node_thread_spread_order_cmp;
	    break;
	case ERTS_CPU_BIND_NO_SPREAD:
	    cmp_func = cpu_no_spread_order_cmp;
	    break;
	default:
	    cmp_func = NULL;
	    erts_exit(ERTS_ABORT_EXIT,
		     "Bad cpu bind type: %d\n",
		     (int) cpu_bind_order);
	    break;
	}

	if (mk_seq)
	    make_cpudata_id_seq(cpudata, size, no_node);

	qsort(cpudata, size, sizeof(erts_cpu_topology_t), cmp_func);
    }
}

static int
processor_order_cmp(const void *vx, const void *vy)
{
    erts_cpu_topology_t *x = (erts_cpu_topology_t *) vx;
    erts_cpu_topology_t *y = (erts_cpu_topology_t *) vy;

    if (x->processor != y->processor)
	return x->processor - y->processor;
    if (x->node != y->node)
	return x->node - y->node;
    if (x->processor_node != y->processor_node)
	return x->processor_node - y->processor_node;
    if (x->core != y->core)
	return x->core - y->core;
    if (x->thread != y->thread)
	return x->thread - y->thread;
    return 0;
}

void
erts_sched_check_cpu_bind_prep_suspend(ErtsSchedulerData *esdp)
{
    erts_cpu_groups_map_t *cgm;
    erts_cpu_groups_callback_list_t *cgcl;
    erts_cpu_groups_callback_call_t *cgcc;
    int cgcc_ix;

    /* Unbind from cpu */
    erts_rwmtx_rwlock(&cpuinfo_rwmtx);
    if (scheduler2cpu_map[esdp->no].bound_id >= 0
	&& erts_unbind_from_cpu(cpuinfo) == 0) {
	esdp->cpu_id = scheduler2cpu_map[esdp->no].bound_id = -1;
    }

    cgcc = erts_alloc(ERTS_ALC_T_TMP,
		      (no_cpu_groups_callbacks
		       * sizeof(erts_cpu_groups_callback_call_t)));
    cgcc_ix = 0;
    for (cgm = cpu_groups_maps; cgm; cgm = cgm->next) {
	for (cgcl = cgm->callback_list; cgcl; cgcl = cgcl->next) {
	    cgcc[cgcc_ix].callback = cgcl->callback;
	    cgcc[cgcc_ix].ix = cpu_groups_lookup(cgm, esdp);
	    cgcc[cgcc_ix].arg = cgcl->arg;
	    cgcc_ix++;
	}
    }
    ASSERT(no_cpu_groups_callbacks == cgcc_ix);
    erts_rwmtx_rwunlock(&cpuinfo_rwmtx);

    for (cgcc_ix = 0; cgcc_ix < no_cpu_groups_callbacks; cgcc_ix++)
	cgcc[cgcc_ix].callback(1,
			       esdp,
			       cgcc[cgcc_ix].ix,
			       cgcc[cgcc_ix].arg);

    erts_free(ERTS_ALC_T_TMP, cgcc);

    if (esdp->no <= max_main_threads)
	erts_thr_set_main_status(0, 0);

}

void
erts_sched_check_cpu_bind_post_suspend(ErtsSchedulerData *esdp)
{
    ERTS_LC_ASSERT(erts_lc_runq_is_locked(esdp->run_queue));

    if (esdp->no <= max_main_threads)
	erts_thr_set_main_status(1, (int) esdp->no);

    /* Make sure we check if we should bind to a cpu or not... */
    (void) ERTS_RUNQ_FLGS_SET(esdp->run_queue, ERTS_RUNQ_FLG_CHK_CPU_BIND);
}


void
erts_sched_check_cpu_bind(ErtsSchedulerData *esdp)
{
    int res, cpu_id, cgcc_ix;
    erts_cpu_groups_map_t *cgm;
    erts_cpu_groups_callback_list_t *cgcl;
    erts_cpu_groups_callback_call_t *cgcc;
    erts_runq_unlock(esdp->run_queue);
    erts_rwmtx_rwlock(&cpuinfo_rwmtx);
    cpu_id = scheduler2cpu_map[esdp->no].bind_id;
    if (cpu_id >= 0 && cpu_id != scheduler2cpu_map[esdp->no].bound_id) {
	res = erts_bind_to_cpu(cpuinfo, cpu_id);
	if (res == 0)
	    esdp->cpu_id = scheduler2cpu_map[esdp->no].bound_id = cpu_id;
	else {
	    erts_dsprintf_buf_t *dsbufp = erts_create_logger_dsbuf();
	    erts_dsprintf(dsbufp, "Scheduler %d failed to bind to cpu %d: %s\n",
			  (int) esdp->no, cpu_id, erl_errno_id(-res));
	    erts_send_error_to_logger_nogl(dsbufp);
	    if (scheduler2cpu_map[esdp->no].bound_id >= 0)
		goto unbind;
	}
    }
    else if (cpu_id < 0) {
    unbind:
	/* Get rid of old binding */
	res = erts_unbind_from_cpu(cpuinfo);
	if (res == 0)
	    esdp->cpu_id = scheduler2cpu_map[esdp->no].bound_id = -1;
	else if (res != -ENOTSUP) {
	    erts_dsprintf_buf_t *dsbufp = erts_create_logger_dsbuf();
	    erts_dsprintf(dsbufp, "Scheduler %d failed to unbind from cpu %d: %s\n",
			  (int) esdp->no, cpu_id, erl_errno_id(-res));
	    erts_send_error_to_logger_nogl(dsbufp);
	}
    }

    cgcc = erts_alloc(ERTS_ALC_T_TMP,
		      (no_cpu_groups_callbacks
		       * sizeof(erts_cpu_groups_callback_call_t)));
    cgcc_ix = 0;
    for (cgm = cpu_groups_maps; cgm; cgm = cgm->next) {
	for (cgcl = cgm->callback_list; cgcl; cgcl = cgcl->next) {
	    cgcc[cgcc_ix].callback = cgcl->callback;
	    cgcc[cgcc_ix].ix = cpu_groups_lookup(cgm, esdp);
	    cgcc[cgcc_ix].arg = cgcl->arg;
	    cgcc_ix++;
	}
    }

    ASSERT(no_cpu_groups_callbacks == cgcc_ix);
    erts_rwmtx_rwunlock(&cpuinfo_rwmtx);

    for (cgcc_ix = 0; cgcc_ix < no_cpu_groups_callbacks; cgcc_ix++)
	cgcc[cgcc_ix].callback(0,
			       esdp,
			       cgcc[cgcc_ix].ix,
			       cgcc[cgcc_ix].arg);

    erts_free(ERTS_ALC_T_TMP, cgcc);

    erts_runq_lock(esdp->run_queue);
}

void
erts_sched_init_check_cpu_bind(ErtsSchedulerData *esdp)
{
    int cgcc_ix;
    erts_cpu_groups_map_t *cgm;
    erts_cpu_groups_callback_list_t *cgcl;
    erts_cpu_groups_callback_call_t *cgcc;

    erts_rwmtx_rlock(&cpuinfo_rwmtx);

    cgcc = erts_alloc(ERTS_ALC_T_TMP,
		      (no_cpu_groups_callbacks
		       * sizeof(erts_cpu_groups_callback_call_t)));
    cgcc_ix = 0;
    for (cgm = cpu_groups_maps; cgm; cgm = cgm->next) {
	for (cgcl = cgm->callback_list; cgcl; cgcl = cgcl->next) {
	    cgcc[cgcc_ix].callback = cgcl->callback;
	    cgcc[cgcc_ix].ix = cpu_groups_lookup(cgm, esdp);
	    cgcc[cgcc_ix].arg = cgcl->arg;
	    cgcc_ix++;
	}
    }

    ASSERT(no_cpu_groups_callbacks == cgcc_ix);
    erts_rwmtx_runlock(&cpuinfo_rwmtx);

    for (cgcc_ix = 0; cgcc_ix < no_cpu_groups_callbacks; cgcc_ix++)
	cgcc[cgcc_ix].callback(0,
			       esdp,
			       cgcc[cgcc_ix].ix,
			       cgcc[cgcc_ix].arg);

    erts_free(ERTS_ALC_T_TMP, cgcc);

    if (esdp->no <= max_main_threads)
	erts_thr_set_main_status(1, (int) esdp->no);
}

static void
write_schedulers_bind_change(erts_cpu_topology_t *cpudata, int size)
{
    int s_ix = 1;
    int cpu_ix;

    ERTS_LC_ASSERT(erts_lc_rwmtx_is_rwlocked(&cpuinfo_rwmtx));

    if (cpu_bind_order != ERTS_CPU_BIND_NONE && size) {

	cpu_bind_order_sort(cpudata, size, cpu_bind_order, 1);

	for (cpu_ix = 0; cpu_ix < size && s_ix <= erts_no_schedulers; cpu_ix++)
	    if (erts_is_cpu_available(cpuinfo, cpudata[cpu_ix].logical))
		scheduler2cpu_map[s_ix++].bind_id = cpudata[cpu_ix].logical;
    }

    if (s_ix <= erts_no_schedulers)
	for (; s_ix <= erts_no_schedulers; s_ix++)
	    scheduler2cpu_map[s_ix].bind_id = -1;
}

int
erts_init_scheduler_bind_type_string(char *how)
{
    ErtsCpuBindOrder order;

    if (sys_strcmp(how, "u") == 0)
	order = ERTS_CPU_BIND_NONE;
    else if (sys_strcmp(how, "db") == 0)
	order = ERTS_CPU_BIND_DEFAULT_BIND;
    else if (sys_strcmp(how, "s") == 0)
	order = ERTS_CPU_BIND_SPREAD;
    else if (sys_strcmp(how, "ps") == 0)
	order = ERTS_CPU_BIND_PROCESSOR_SPREAD;
    else if (sys_strcmp(how, "ts") == 0)
	order = ERTS_CPU_BIND_THREAD_SPREAD;
    else if (sys_strcmp(how, "tnnps") == 0)
	order = ERTS_CPU_BIND_THREAD_NO_NODE_PROCESSOR_SPREAD;
    else if (sys_strcmp(how, "nnps") == 0)
	order = ERTS_CPU_BIND_NO_NODE_PROCESSOR_SPREAD;
    else if (sys_strcmp(how, "nnts") == 0)
	order = ERTS_CPU_BIND_NO_NODE_THREAD_SPREAD;
    else if (sys_strcmp(how, "ns") == 0)
	order = ERTS_CPU_BIND_NO_SPREAD;
    else
	return ERTS_INIT_SCHED_BIND_TYPE_ERROR_BAD_TYPE;

    if (order != ERTS_CPU_BIND_NONE) {
	if (erts_bind_to_cpu(cpuinfo, -1) == -ENOTSUP)
	    return ERTS_INIT_SCHED_BIND_TYPE_NOT_SUPPORTED;
	else if (!system_cpudata && !user_cpudata)
	    return ERTS_INIT_SCHED_BIND_TYPE_ERROR_NO_CPU_TOPOLOGY;
    }

    cpu_bind_order = order;

    return ERTS_INIT_SCHED_BIND_TYPE_SUCCESS;
}

static Eterm
bound_schedulers_term(ErtsCpuBindOrder order)
{
    switch (order) {
    case ERTS_CPU_BIND_SPREAD: {
	ERTS_DECL_AM(spread);
	return AM_spread;
    }
    case ERTS_CPU_BIND_PROCESSOR_SPREAD: {
	ERTS_DECL_AM(processor_spread);
	return AM_processor_spread;
    }
    case ERTS_CPU_BIND_THREAD_SPREAD: {
	ERTS_DECL_AM(thread_spread);
	return AM_thread_spread;
    }
    case ERTS_CPU_BIND_THREAD_NO_NODE_PROCESSOR_SPREAD: {
	ERTS_DECL_AM(thread_no_node_processor_spread);
	return AM_thread_no_node_processor_spread;
    }
    case ERTS_CPU_BIND_NO_NODE_PROCESSOR_SPREAD: {
	ERTS_DECL_AM(no_node_processor_spread);
	return AM_no_node_processor_spread;
    }
    case ERTS_CPU_BIND_NO_NODE_THREAD_SPREAD: {
	ERTS_DECL_AM(no_node_thread_spread);
	return AM_no_node_thread_spread;
    }
    case ERTS_CPU_BIND_NO_SPREAD: {
	ERTS_DECL_AM(no_spread);
	return AM_no_spread;
    }
    case ERTS_CPU_BIND_NONE: {
	ERTS_DECL_AM(unbound);
	return AM_unbound;
    }
    default:
	ASSERT(0);
	return THE_NON_VALUE;
    }
}

Eterm
erts_bound_schedulers_term(Process *c_p)
{
    ErtsCpuBindOrder order;
    erts_rwmtx_rlock(&cpuinfo_rwmtx);
    order = cpu_bind_order;
    erts_rwmtx_runlock(&cpuinfo_rwmtx);
    return bound_schedulers_term(order);
}

Eterm
erts_bind_schedulers(Process *c_p, Eterm how)
{
    int notify = 0;
    Eterm res;
    erts_cpu_topology_t *cpudata;
    int cpudata_size;
    ErtsCpuBindOrder old_cpu_bind_order;

    erts_rwmtx_rwlock(&cpuinfo_rwmtx);

    if (erts_bind_to_cpu(cpuinfo, -1) == -ENOTSUP) {
	if (cpu_bind_order == ERTS_CPU_BIND_NONE
	    && ERTS_IS_ATOM_STR("unbound", how)) {
	    res = bound_schedulers_term(ERTS_CPU_BIND_NONE);
	    goto done;
	}
	ERTS_BIF_PREP_ERROR(res, c_p, EXC_NOTSUP);
    }
    else {

	old_cpu_bind_order = cpu_bind_order;

	if (ERTS_IS_ATOM_STR("default_bind", how))
	    cpu_bind_order = ERTS_CPU_BIND_DEFAULT_BIND;
	else if (ERTS_IS_ATOM_STR("spread", how))
	    cpu_bind_order = ERTS_CPU_BIND_SPREAD;
	else if (ERTS_IS_ATOM_STR("processor_spread", how))
	    cpu_bind_order = ERTS_CPU_BIND_PROCESSOR_SPREAD;
	else if (ERTS_IS_ATOM_STR("thread_spread", how))
	    cpu_bind_order = ERTS_CPU_BIND_THREAD_SPREAD;
	else if (ERTS_IS_ATOM_STR("thread_no_node_processor_spread", how))
	    cpu_bind_order = ERTS_CPU_BIND_THREAD_NO_NODE_PROCESSOR_SPREAD;
	else if (ERTS_IS_ATOM_STR("no_node_processor_spread", how))
	    cpu_bind_order = ERTS_CPU_BIND_NO_NODE_PROCESSOR_SPREAD;
	else if (ERTS_IS_ATOM_STR("no_node_thread_spread", how))
	    cpu_bind_order = ERTS_CPU_BIND_NO_NODE_THREAD_SPREAD;
	else if (ERTS_IS_ATOM_STR("no_spread", how))
	    cpu_bind_order = ERTS_CPU_BIND_NO_SPREAD;
	else if (ERTS_IS_ATOM_STR("unbound", how))
	    cpu_bind_order = ERTS_CPU_BIND_NONE;
	else {
	    cpu_bind_order = old_cpu_bind_order;
	    ERTS_BIF_PREP_ERROR(res, c_p, BADARG);
	    goto done;
	}

	create_tmp_cpu_topology_copy(&cpudata, &cpudata_size);

	if (!cpudata) {
	    cpu_bind_order = old_cpu_bind_order;
	    ERTS_BIF_PREP_ERROR(res, c_p, BADARG);
	    goto done;
	}

	write_schedulers_bind_change(cpudata, cpudata_size);
	notify = 1;

	destroy_tmp_cpu_topology_copy(cpudata);
    
	res = bound_schedulers_term(old_cpu_bind_order);
    }

 done:

    erts_rwmtx_rwunlock(&cpuinfo_rwmtx);

    if (notify)
	erts_sched_notify_check_cpu_bind();

    return res;
}

int
erts_sched_bind_atthrcreate_prepare(void)
{
    ErtsSchedulerData *esdp = erts_get_scheduler_data();
    return esdp != NULL && erts_is_scheduler_bound(esdp);
}

int
erts_sched_bind_atthrcreate_child(int unbind)
{
    int res = 0;
    if (unbind) {
	erts_rwmtx_rlock(&cpuinfo_rwmtx);
	res = erts_unbind_from_cpu(cpuinfo);
	erts_rwmtx_runlock(&cpuinfo_rwmtx);
    }
    return res;
}

void
erts_sched_bind_atthrcreate_parent(int unbind)
{

}

int
erts_sched_bind_atfork_prepare(void)
{
    ErtsSchedulerData *esdp = erts_get_scheduler_data();
    int unbind = esdp != NULL && erts_is_scheduler_bound(esdp);
    if (unbind)
	erts_rwmtx_rlock(&cpuinfo_rwmtx);
    return unbind;
}

int
erts_sched_bind_atfork_child(int unbind)
{
    if (unbind) {
	ERTS_LC_ASSERT(erts_lc_rwmtx_is_rlocked(&cpuinfo_rwmtx)
			   || erts_lc_rwmtx_is_rwlocked(&cpuinfo_rwmtx));
	return erts_unbind_from_cpu(cpuinfo);
    }
    return 0;
}

void
erts_sched_bind_atfork_parent(int unbind)
{
    if (unbind)
	erts_rwmtx_runlock(&cpuinfo_rwmtx);
}

Eterm
erts_fake_scheduler_bindings(Process *p, Eterm how)
{
    ErtsCpuBindOrder fake_cpu_bind_order;
    erts_cpu_topology_t *cpudata;
    int cpudata_size;
    Eterm res;

    if (ERTS_IS_ATOM_STR("default_bind", how))
	fake_cpu_bind_order = ERTS_CPU_BIND_DEFAULT_BIND;
    else if (ERTS_IS_ATOM_STR("spread", how))
	fake_cpu_bind_order = ERTS_CPU_BIND_SPREAD;
    else if (ERTS_IS_ATOM_STR("processor_spread", how))
	fake_cpu_bind_order = ERTS_CPU_BIND_PROCESSOR_SPREAD;
    else if (ERTS_IS_ATOM_STR("thread_spread", how))
	fake_cpu_bind_order = ERTS_CPU_BIND_THREAD_SPREAD;
    else if (ERTS_IS_ATOM_STR("thread_no_node_processor_spread", how))
	fake_cpu_bind_order = ERTS_CPU_BIND_THREAD_NO_NODE_PROCESSOR_SPREAD;
    else if (ERTS_IS_ATOM_STR("no_node_processor_spread", how))
	fake_cpu_bind_order = ERTS_CPU_BIND_NO_NODE_PROCESSOR_SPREAD;
    else if (ERTS_IS_ATOM_STR("no_node_thread_spread", how))
	fake_cpu_bind_order = ERTS_CPU_BIND_NO_NODE_THREAD_SPREAD;
    else if (ERTS_IS_ATOM_STR("no_spread", how))
	fake_cpu_bind_order = ERTS_CPU_BIND_NO_SPREAD;
    else if (ERTS_IS_ATOM_STR("unbound", how))
	fake_cpu_bind_order = ERTS_CPU_BIND_NONE;
    else {
	ERTS_BIF_PREP_ERROR(res, p, BADARG);
	return res;
    }

    erts_rwmtx_rlock(&cpuinfo_rwmtx);
    create_tmp_cpu_topology_copy(&cpudata, &cpudata_size);
    erts_rwmtx_runlock(&cpuinfo_rwmtx);

    if (!cpudata || fake_cpu_bind_order == ERTS_CPU_BIND_NONE)
	ERTS_BIF_PREP_RET(res, am_false);
    else {
	int i;
	Eterm *hp;
	
	cpu_bind_order_sort(cpudata, cpudata_size, fake_cpu_bind_order, 1);

#ifdef ERTS_FAKE_SCHED_BIND_PRINT_SORTED_CPU_DATA

	erts_fprintf(stderr, "node:          ");
	for (i = 0; i < cpudata_size; i++)
	    erts_fprintf(stderr, " %2d", cpudata[i].node);
	erts_fprintf(stderr, "\n");
	erts_fprintf(stderr, "processor:     ");
	for (i = 0; i < cpudata_size; i++)
	    erts_fprintf(stderr, " %2d", cpudata[i].processor);
	erts_fprintf(stderr, "\n");
	if (fake_cpu_bind_order != ERTS_CPU_BIND_THREAD_NO_NODE_PROCESSOR_SPREAD
	    && fake_cpu_bind_order != ERTS_CPU_BIND_NO_NODE_PROCESSOR_SPREAD
	    && fake_cpu_bind_order != ERTS_CPU_BIND_NO_NODE_THREAD_SPREAD) {
	    erts_fprintf(stderr, "processor_node:");
	    for (i = 0; i < cpudata_size; i++)
		erts_fprintf(stderr, " %2d", cpudata[i].processor_node);
	    erts_fprintf(stderr, "\n");
	}
	erts_fprintf(stderr, "core:          ");
	for (i = 0; i < cpudata_size; i++)
	    erts_fprintf(stderr, " %2d", cpudata[i].core);
	erts_fprintf(stderr, "\n");
	erts_fprintf(stderr, "thread:        ");
	for (i = 0; i < cpudata_size; i++)
	    erts_fprintf(stderr, " %2d", cpudata[i].thread);
	erts_fprintf(stderr, "\n");
	erts_fprintf(stderr, "logical:       ");
	for (i = 0; i < cpudata_size; i++)
	    erts_fprintf(stderr, " %2d", cpudata[i].logical);
	erts_fprintf(stderr, "\n");
#endif

	hp = HAlloc(p, cpudata_size+1);
	ERTS_BIF_PREP_RET(res, make_tuple(hp));
	*hp++ = make_arityval((Uint) cpudata_size);
	for (i = 0; i < cpudata_size; i++)
	    *hp++ = make_small((Uint) cpudata[i].logical);
    }

    destroy_tmp_cpu_topology_copy(cpudata);

    return res;
}

Eterm
erts_get_schedulers_binds(Process *c_p)
{
    int ix;
    ERTS_DECL_AM(unbound);
    Eterm *hp = HAlloc(c_p, erts_no_schedulers+1);
    Eterm res = make_tuple(hp);

    *(hp++) = make_arityval(erts_no_schedulers);
    erts_rwmtx_rlock(&cpuinfo_rwmtx);
    for (ix = 1; ix <= erts_no_schedulers; ix++)
	*(hp++) = (scheduler2cpu_map[ix].bound_id >= 0
		   ? make_small(scheduler2cpu_map[ix].bound_id)
		   : AM_unbound);
    erts_rwmtx_runlock(&cpuinfo_rwmtx);
    return res;
}

/*
 * CPU topology
 */

typedef struct {
    int *id;
    int used;
    int size;
} ErtsCpuTopIdSeq;

typedef struct {
    ErtsCpuTopIdSeq logical;
    ErtsCpuTopIdSeq thread;
    ErtsCpuTopIdSeq core;
    ErtsCpuTopIdSeq processor_node;
    ErtsCpuTopIdSeq processor;
    ErtsCpuTopIdSeq node;
} ErtsCpuTopEntry;

static void
init_cpu_top_entry(ErtsCpuTopEntry *cte)
{
    int size = 10;
    cte->logical.id = erts_alloc(ERTS_ALC_T_TMP_CPU_IDS,
				 sizeof(int)*size);
    cte->logical.size = size;
    cte->thread.id = erts_alloc(ERTS_ALC_T_TMP_CPU_IDS,
				sizeof(int)*size);
    cte->thread.size = size;
    cte->core.id = erts_alloc(ERTS_ALC_T_TMP_CPU_IDS,
			      sizeof(int)*size);
    cte->core.size = size;
    cte->processor_node.id = erts_alloc(ERTS_ALC_T_TMP_CPU_IDS,
					sizeof(int)*size);
    cte->processor_node.size = size;
    cte->processor.id = erts_alloc(ERTS_ALC_T_TMP_CPU_IDS,
				   sizeof(int)*size);
    cte->processor.size = size;
    cte->node.id = erts_alloc(ERTS_ALC_T_TMP_CPU_IDS,
			      sizeof(int)*size);
    cte->node.size = size;
}

static void
destroy_cpu_top_entry(ErtsCpuTopEntry *cte)
{
    erts_free(ERTS_ALC_T_TMP_CPU_IDS, cte->logical.id);
    erts_free(ERTS_ALC_T_TMP_CPU_IDS, cte->thread.id);
    erts_free(ERTS_ALC_T_TMP_CPU_IDS, cte->core.id);
    erts_free(ERTS_ALC_T_TMP_CPU_IDS, cte->processor_node.id);
    erts_free(ERTS_ALC_T_TMP_CPU_IDS, cte->processor.id);
    erts_free(ERTS_ALC_T_TMP_CPU_IDS, cte->node.id);
}

static int
get_cput_value_or_range(int *v, int *vr, char **str)
{
    long l;
    char *c = *str;
    errno = 0;
    if (!isdigit((unsigned char)*c))
	return ERTS_INIT_CPU_TOPOLOGY_INVALID_ID;
    l = strtol(c, &c, 10);
    if (errno != 0 || l < 0 || ERTS_MAX_CPU_TOPOLOGY_ID < l)
	return ERTS_INIT_CPU_TOPOLOGY_INVALID_ID;
    *v = (int) l;
    if (*c == '-') {
	c++;
	if (!isdigit((unsigned char)*c))
	    return ERTS_INIT_CPU_TOPOLOGY_INVALID_ID_RANGE;
	l = strtol(c, &c, 10);
	if (errno != 0 || l < 0 || ERTS_MAX_CPU_TOPOLOGY_ID < l)
	    return ERTS_INIT_CPU_TOPOLOGY_INVALID_ID_RANGE;
	*vr = (int) l;
    }
    *str = c;
    return ERTS_INIT_CPU_TOPOLOGY_OK;
}

static int
get_cput_id_seq(ErtsCpuTopIdSeq *idseq, char **str)
{
    int ix = 0;
    int need_size = 0;
    char *c = *str;

    while (1) {
	int res;
	int val;
	int nids;
	int val_range = -1;
	res = get_cput_value_or_range(&val, &val_range, &c);
	if (res != ERTS_INIT_CPU_TOPOLOGY_OK)
	    return res;
	if (val_range < 0 || val_range == val)
	    nids = 1;
	else {
	    if (val_range > val)
		nids = val_range - val + 1;
	    else
		nids = val - val_range + 1;
	}
	need_size += nids;
	if (need_size > idseq->size) {
	    idseq->size = need_size + 10;
	    idseq->id = erts_realloc(ERTS_ALC_T_TMP_CPU_IDS,
				      idseq->id,
				      sizeof(int)*idseq->size);
	}
	if (nids == 1)
	    idseq->id[ix++] = val;
	else if (val_range > val) {
	    for (; val <= val_range; val++)
		idseq->id[ix++] = val;
	}
	else {
	    for (; val >= val_range; val--)
		idseq->id[ix++] = val;
	}
	if (*c != ',')
	    break;
	c++;
    }
    *str = c;
    idseq->used = ix;
    return ERTS_INIT_CPU_TOPOLOGY_OK;
}

static int
get_cput_entry(ErtsCpuTopEntry *cput, char **str)
{
    int h;
    char *c = *str;

    cput->logical.used = 0;
    cput->thread.id[0] = 0;
    cput->thread.used = 1;
    cput->core.id[0] = 0;
    cput->core.used = 1;
    cput->processor_node.id[0] = -1;
    cput->processor_node.used = 1;
    cput->processor.id[0] = 0;
    cput->processor.used = 1;
    cput->node.id[0] = -1;
    cput->node.used = 1;

    h = ERTS_TOPOLOGY_MAX_DEPTH;
    while (*c != ':' && *c != '\0') {
	int res;
	ErtsCpuTopIdSeq *idseqp;
	switch (*c++) {
	case 'L':
	    if (h <= ERTS_TOPOLOGY_LOGICAL)
		return ERTS_INIT_CPU_TOPOLOGY_INVALID_HIERARCHY;
	    idseqp = &cput->logical;
	    h = ERTS_TOPOLOGY_LOGICAL;
	    break;
	case 't':
	case 'T':
	    if (h <= ERTS_TOPOLOGY_THREAD)
		return ERTS_INIT_CPU_TOPOLOGY_INVALID_HIERARCHY;
	    idseqp = &cput->thread;
	    h = ERTS_TOPOLOGY_THREAD;
	    break;
	case 'c':
	case 'C':
	    if (h <= ERTS_TOPOLOGY_CORE)
		return ERTS_INIT_CPU_TOPOLOGY_INVALID_HIERARCHY;
	    idseqp = &cput->core;
	    h = ERTS_TOPOLOGY_CORE;
	    break;
	case 'p':
	case 'P':
	    if (h <= ERTS_TOPOLOGY_PROCESSOR)
		return ERTS_INIT_CPU_TOPOLOGY_INVALID_HIERARCHY;
	    idseqp = &cput->processor;
	    h = ERTS_TOPOLOGY_PROCESSOR;
	    break;
	case 'n':
	case 'N':
	    if (h <= ERTS_TOPOLOGY_PROCESSOR) {
	    do_node:
		if (h <= ERTS_TOPOLOGY_NODE)
		    return ERTS_INIT_CPU_TOPOLOGY_INVALID_HIERARCHY;
		idseqp = &cput->node;
		h = ERTS_TOPOLOGY_NODE;
	    }
	    else {
		int p_node = 0;
		char *p_chk = c;
		while (*p_chk != '\0' && *p_chk != ':') {
		    if (*p_chk == 'p' || *p_chk == 'P') {
			p_node = 1;
			break;
		    }
		    p_chk++;
		}
		if (!p_node)
		    goto do_node;
		if (h <= ERTS_TOPOLOGY_PROCESSOR_NODE)
		    return ERTS_INIT_CPU_TOPOLOGY_INVALID_HIERARCHY;
		idseqp = &cput->processor_node;
		h = ERTS_TOPOLOGY_PROCESSOR_NODE;
	    }
	    break;
	default:
	    return ERTS_INIT_CPU_TOPOLOGY_INVALID_ID_TYPE;
	}
	res = get_cput_id_seq(idseqp, &c);
	if (res != ERTS_INIT_CPU_TOPOLOGY_OK)
		return res;
    }

    if (cput->logical.used < 1)
	return ERTS_INIT_CPU_TOPOLOGY_MISSING_LID;

    if (*c == ':') {
	c++;
    }

    if (cput->thread.used != 1
	&& cput->thread.used != cput->logical.used)
	return ERTS_INIT_CPU_TOPOLOGY_INVALID_ID_RANGE;
    if (cput->core.used != 1
	&& cput->core.used != cput->logical.used)
	return ERTS_INIT_CPU_TOPOLOGY_INVALID_ID_RANGE;
    if (cput->processor_node.used != 1
	&& cput->processor_node.used != cput->logical.used)
	return ERTS_INIT_CPU_TOPOLOGY_INVALID_ID_RANGE;
    if (cput->processor.used != 1
	&& cput->processor.used != cput->logical.used)
	return ERTS_INIT_CPU_TOPOLOGY_INVALID_ID_RANGE;
    if (cput->node.used != 1
	&& cput->node.used != cput->logical.used)
	return ERTS_INIT_CPU_TOPOLOGY_INVALID_ID_RANGE;

    *str = c;
    return ERTS_INIT_CPU_TOPOLOGY_OK;
}

static int
verify_topology(erts_cpu_topology_t *cpudata, int size)
{
    if (size > 0) {
	int *logical;
	int node, processor, no_nodes, i;

	/* Verify logical ids */
	logical = erts_alloc(ERTS_ALC_T_TMP, sizeof(int)*size);

	for (i = 0; i < size; i++)
	    logical[i] = cpudata[i].logical;

	qsort(logical, size, sizeof(int), int_cmp);
	for (i = 0; i < size-1; i++) {
	    if (logical[i] == logical[i+1]) {
		erts_free(ERTS_ALC_T_TMP, logical);
		return ERTS_INIT_CPU_TOPOLOGY_NOT_UNIQUE_LIDS;
	    }
	}

	erts_free(ERTS_ALC_T_TMP, logical);

	qsort(cpudata, size, sizeof(erts_cpu_topology_t), processor_order_cmp);

	/* Verify unique entities */

	for (i = 1; i < size; i++) {
	    if (cpudata[i-1].processor == cpudata[i].processor
		&& cpudata[i-1].node == cpudata[i].node
		&& (cpudata[i-1].processor_node
		    == cpudata[i].processor_node)
		&& cpudata[i-1].core == cpudata[i].core
		&& cpudata[i-1].thread == cpudata[i].thread) {
		return ERTS_INIT_CPU_TOPOLOGY_NOT_UNIQUE_ENTITIES;
	    }
	}

	/* Verify numa nodes */
	node = cpudata[0].node;
	processor = cpudata[0].processor;
	no_nodes = cpudata[0].node < 0 && cpudata[0].processor_node < 0;
	for (i = 1; i < size; i++) {
	    if (no_nodes) {
		if (cpudata[i].node >= 0 || cpudata[i].processor_node >= 0)
		    return ERTS_INIT_CPU_TOPOLOGY_INVALID_NODES;
	    }
	    else {
		if (cpudata[i].processor == processor && cpudata[i].node != node)
		    return ERTS_INIT_CPU_TOPOLOGY_INVALID_NODES;
		node = cpudata[i].node;
		processor = cpudata[i].processor;
		if (node >= 0 && cpudata[i].processor_node >= 0)
		    return ERTS_INIT_CPU_TOPOLOGY_INVALID_NODES;
		if (node < 0 && cpudata[i].processor_node < 0)
		    return ERTS_INIT_CPU_TOPOLOGY_INVALID_NODES;
	    }
	}
    }

    return ERTS_INIT_CPU_TOPOLOGY_OK;
}

int
erts_init_cpu_topology_string(char *topology_str)
{
    ErtsCpuTopEntry cput;
    int need_size;
    char *c;
    int ix;
    int error = ERTS_INIT_CPU_TOPOLOGY_OK;

    if (user_cpudata)
	erts_free(ERTS_ALC_T_CPUDATA, user_cpudata);
    user_cpudata_size = 10;

    user_cpudata = erts_alloc(ERTS_ALC_T_CPUDATA,
			      (sizeof(erts_cpu_topology_t)
			       * user_cpudata_size));

    init_cpu_top_entry(&cput);

    ix = 0;
    need_size = 0;

    c = topology_str;
    if (*c == '\0') {
	error = ERTS_INIT_CPU_TOPOLOGY_MISSING;
	goto fail;
    }
    do {
	int r;
	error = get_cput_entry(&cput, &c);
	if (error != ERTS_INIT_CPU_TOPOLOGY_OK)
	    goto fail;
	need_size += cput.logical.used;
	if (user_cpudata_size < need_size) {
	    user_cpudata_size = need_size + 10;
	    user_cpudata = erts_realloc(ERTS_ALC_T_CPUDATA,
					user_cpudata,
					(sizeof(erts_cpu_topology_t)
					 * user_cpudata_size));
	}

	ASSERT(cput.thread.used == 1
	       || cput.thread.used == cput.logical.used);
	ASSERT(cput.core.used == 1
	       || cput.core.used == cput.logical.used);
	ASSERT(cput.processor_node.used == 1
	       || cput.processor_node.used == cput.logical.used);
	ASSERT(cput.processor.used == 1
	       || cput.processor.used == cput.logical.used);
	ASSERT(cput.node.used == 1
	       || cput.node.used == cput.logical.used);

	for (r = 0; r < cput.logical.used; r++) {
	    user_cpudata[ix].logical = cput.logical.id[r];
	    user_cpudata[ix].thread =
		cput.thread.id[cput.thread.used == 1 ? 0 : r];
	    user_cpudata[ix].core =
		cput.core.id[cput.core.used == 1 ? 0 : r];
	    user_cpudata[ix].processor_node =
		cput.processor_node.id[cput.processor_node.used == 1 ? 0 : r];
	    user_cpudata[ix].processor =
		cput.processor.id[cput.processor.used == 1 ? 0 : r];
	    user_cpudata[ix].node =
		cput.node.id[cput.node.used == 1 ? 0 : r];
	    ix++;
	}
    } while (*c != '\0');

    if (user_cpudata_size != ix) {
	user_cpudata_size = ix;
	user_cpudata = erts_realloc(ERTS_ALC_T_CPUDATA,
				    user_cpudata,
				    (sizeof(erts_cpu_topology_t)
				     * user_cpudata_size));
    }

    error = verify_topology(user_cpudata, user_cpudata_size);
    if (error == ERTS_INIT_CPU_TOPOLOGY_OK) {
	destroy_cpu_top_entry(&cput);
	return ERTS_INIT_CPU_TOPOLOGY_OK;
    }

 fail:
    if (user_cpudata)
	erts_free(ERTS_ALC_T_CPUDATA, user_cpudata);
    user_cpudata_size = 0;
    destroy_cpu_top_entry(&cput);
    return error;
}

#define ERTS_GET_CPU_TOPOLOGY_ERROR		-1
#define ERTS_GET_USED_CPU_TOPOLOGY		0
#define ERTS_GET_DETECTED_CPU_TOPOLOGY		1
#define ERTS_GET_DEFINED_CPU_TOPOLOGY		2

static Eterm get_cpu_topology_term(Process *c_p, int type);

Eterm
erts_set_cpu_topology(Process *c_p, Eterm term)
{
    erts_cpu_topology_t *cpudata = NULL;
    int cpudata_size = 0;
    Eterm res;

    erts_rwmtx_rwlock(&cpuinfo_rwmtx);
    res = get_cpu_topology_term(c_p, ERTS_GET_USED_CPU_TOPOLOGY);
    if (term == am_undefined) {
	if (user_cpudata)
	    erts_free(ERTS_ALC_T_CPUDATA, user_cpudata);
	user_cpudata = NULL;
	user_cpudata_size = 0;

	if (cpu_bind_order != ERTS_CPU_BIND_NONE && system_cpudata) {
	    cpudata_size = system_cpudata_size;
	    cpudata = erts_alloc(ERTS_ALC_T_TMP,
				 (sizeof(erts_cpu_topology_t)
				  * cpudata_size));

	    sys_memcpy((void *) cpudata,
		       (void *) system_cpudata,
		       sizeof(erts_cpu_topology_t)*cpudata_size);
	}
    }
    else if (is_not_list(term)) {
    error:
	erts_rwmtx_rwunlock(&cpuinfo_rwmtx);
	res = THE_NON_VALUE;
	goto done;
    }
    else {
	Eterm list = term;
	int ix = 0;

	cpudata_size = 100;
	cpudata = erts_alloc(ERTS_ALC_T_TMP,
			     (sizeof(erts_cpu_topology_t)
			      * cpudata_size));

	while (is_list(list)) {
	    Eterm *lp = list_val(list);
	    Eterm cpu = CAR(lp);
	    Eterm* tp;
	    Sint id;
		
	    if (is_not_tuple(cpu))
		goto error;

	    tp = tuple_val(cpu);

	    if (arityval(tp[0]) != 7 || tp[1] != am_cpu)
		goto error;

	    if (ix >= cpudata_size) {
		cpudata_size += 100;
		cpudata = erts_realloc(ERTS_ALC_T_TMP,
				       cpudata,
				       (sizeof(erts_cpu_topology_t)
					* cpudata_size));
	    }

	    id = signed_val(tp[2]);
	    if (id < -1 || ERTS_MAX_CPU_TOPOLOGY_ID < id)
		goto error;
	    cpudata[ix].node = (int) id;

	    id = signed_val(tp[3]);
	    if (id < -1 || ERTS_MAX_CPU_TOPOLOGY_ID < id)
		goto error;
	    cpudata[ix].processor = (int) id;

	    id = signed_val(tp[4]);
	    if (id < -1 || ERTS_MAX_CPU_TOPOLOGY_ID < id)
		goto error;
	    cpudata[ix].processor_node = (int) id;

	    id = signed_val(tp[5]);
	    if (id < -1 || ERTS_MAX_CPU_TOPOLOGY_ID < id)
		goto error;
	    cpudata[ix].core = (int) id;

	    id = signed_val(tp[6]);
	    if (id < -1 || ERTS_MAX_CPU_TOPOLOGY_ID < id)
		goto error;
	    cpudata[ix].thread = (int) id;

	    id = signed_val(tp[7]);
	    if (id < -1 || ERTS_MAX_CPU_TOPOLOGY_ID < id)
		goto error;
	    cpudata[ix].logical = (int) id;

	    list = CDR(lp);
	    ix++;
	}

	if (is_not_nil(list))
	    goto error;
	
	cpudata_size = ix;

	if (ERTS_INIT_CPU_TOPOLOGY_OK != verify_topology(cpudata, cpudata_size))
	    goto error;

	if (user_cpudata_size != cpudata_size) {
	    if (user_cpudata)
		erts_free(ERTS_ALC_T_CPUDATA, user_cpudata);
	    user_cpudata = erts_alloc(ERTS_ALC_T_CPUDATA,
				      sizeof(erts_cpu_topology_t)*cpudata_size);
	    user_cpudata_size = cpudata_size;
	}

	sys_memcpy((void *) user_cpudata,
		   (void *) cpudata,
		   sizeof(erts_cpu_topology_t)*cpudata_size);
    }

    update_cpu_groups_maps();

    write_schedulers_bind_change(cpudata, cpudata_size);

    erts_rwmtx_rwunlock(&cpuinfo_rwmtx);
    erts_sched_notify_check_cpu_bind();

 done:

    if (cpudata)
	erts_free(ERTS_ALC_T_TMP, cpudata);

    return res;
}

static void
create_tmp_cpu_topology_copy(erts_cpu_topology_t **cpudata, int *cpudata_size)
{
    if (user_cpudata) {
	*cpudata_size = user_cpudata_size;
	*cpudata = erts_alloc(ERTS_ALC_T_TMP,
			      (sizeof(erts_cpu_topology_t)
			       * (*cpudata_size)));
	sys_memcpy((void *) *cpudata,
		   (void *) user_cpudata,
		   sizeof(erts_cpu_topology_t)*(*cpudata_size));
    }
    else if (system_cpudata) {
	*cpudata_size = system_cpudata_size;
	*cpudata = erts_alloc(ERTS_ALC_T_TMP,
			      (sizeof(erts_cpu_topology_t)
			       * (*cpudata_size)));
	sys_memcpy((void *) *cpudata,
		   (void *) system_cpudata,
		   sizeof(erts_cpu_topology_t)*(*cpudata_size));
    }
    else {
	*cpudata = NULL;
	*cpudata_size = 0;
    }
}

static void
destroy_tmp_cpu_topology_copy(erts_cpu_topology_t *cpudata)
{
    if (cpudata)
	erts_free(ERTS_ALC_T_TMP, cpudata);
}


static Eterm
bld_topology_term(Eterm **hpp,
		  Uint *hszp,
		  erts_cpu_topology_t *cpudata,
		  int size)
{
    Eterm res = NIL;
    int i;

    if (size == 0)
	return am_undefined;

    for (i = size-1; i >= 0; i--) {
	res = erts_bld_cons(hpp,
			    hszp,
			    erts_bld_tuple(hpp,
					   hszp,
					   7,
					   am_cpu,
					   make_small(cpudata[i].node),
					   make_small(cpudata[i].processor),
					   make_small(cpudata[i].processor_node),
					   make_small(cpudata[i].core),
					   make_small(cpudata[i].thread),
					   make_small(cpudata[i].logical)),
			    res);
    }
    return res;
}

static Eterm
get_cpu_topology_term(Process *c_p, int type)
{
#ifdef DEBUG
    Eterm *hp_end;
#endif
    Eterm *hp;
    Uint hsz;
    Eterm res = THE_NON_VALUE;
    erts_cpu_topology_t *cpudata = NULL;
    int size = 0;

    switch (type) {
    case ERTS_GET_USED_CPU_TOPOLOGY:
	if (user_cpudata)
	    goto defined;
	else
	    goto detected;
    case ERTS_GET_DETECTED_CPU_TOPOLOGY:
    detected:
	if (!system_cpudata)
	    res = am_undefined;
	else {
	    size = system_cpudata_size;
	    cpudata = erts_alloc(ERTS_ALC_T_TMP,
				 (sizeof(erts_cpu_topology_t)
				  * size));
	    sys_memcpy((void *) cpudata,
		       (void *) system_cpudata,
		       sizeof(erts_cpu_topology_t)*size);
	}
	break;
    case ERTS_GET_DEFINED_CPU_TOPOLOGY:
    defined:
	if (!user_cpudata)
	    res = am_undefined;
	else {
	    size = user_cpudata_size;
	    cpudata = user_cpudata;
	}
	break;
    default:
	erts_exit(ERTS_ABORT_EXIT, "Bad cpu topology type: %d\n", type);
	break;
    }

    if (res == am_undefined) {
	ASSERT(!cpudata);
	return res;
    }

    hsz = 0;

    bld_topology_term(NULL, &hsz,
		      cpudata, size);

    hp = HAlloc(c_p, hsz);

#ifdef DEBUG
    hp_end = hp + hsz;
#endif

    res = bld_topology_term(&hp, NULL,
			    cpudata, size);

    ASSERT(hp_end == hp);

    if (cpudata && cpudata != system_cpudata && cpudata != user_cpudata)
	erts_free(ERTS_ALC_T_TMP, cpudata);

    return res;
}

Eterm
erts_get_cpu_topology_term(Process *c_p, Eterm which)
{
    Eterm res;
    int type;
    erts_rwmtx_rlock(&cpuinfo_rwmtx);
    if (ERTS_IS_ATOM_STR("used", which))
	type = ERTS_GET_USED_CPU_TOPOLOGY;
    else if (ERTS_IS_ATOM_STR("detected", which))
	type = ERTS_GET_DETECTED_CPU_TOPOLOGY;
    else if (ERTS_IS_ATOM_STR("defined", which))
	type = ERTS_GET_DEFINED_CPU_TOPOLOGY;
    else
	type = ERTS_GET_CPU_TOPOLOGY_ERROR;
    if (type == ERTS_GET_CPU_TOPOLOGY_ERROR)
	res = THE_NON_VALUE;
    else
	res = get_cpu_topology_term(c_p, type);
    erts_rwmtx_runlock(&cpuinfo_rwmtx);
    return res;
}

static void
get_logical_processors(int *conf, int *onln, int *avail)
{
    if (conf)
	*conf = erts_get_cpu_configured(cpuinfo);
    if (onln)
	*onln = erts_get_cpu_online(cpuinfo);
    if (avail)
	*avail = erts_get_cpu_available(cpuinfo);
}

void
erts_get_logical_processors(int *conf, int *onln, int *avail)
{
    erts_rwmtx_rlock(&cpuinfo_rwmtx);
    get_logical_processors(conf, onln, avail);
    erts_rwmtx_runlock(&cpuinfo_rwmtx);
}

void
erts_pre_early_init_cpu_topology(int *max_dcg_p,
                                 int *max_rg_p,
				 int *conf_p,
				 int *onln_p,
				 int *avail_p)
{
    cpu_groups_maps = NULL;
    no_cpu_groups_callbacks = 0;
    *max_rg_p = ERTS_MAX_READER_GROUPS;
    *max_dcg_p = ERTS_MAX_FLXCTR_GROUPS;
    cpuinfo = erts_cpu_info_create();
    get_logical_processors(conf_p, onln_p, avail_p);
}

void
erts_early_init_cpu_topology(int no_schedulers,
			     int *max_main_threads_p,
			     int max_reader_groups,
			     int *reader_groups_p,
                             int max_decentralized_counter_groups,
                             int *decentralized_counter_groups_p)
{
    user_cpudata = NULL;
    user_cpudata_size = 0;

    system_cpudata_size = erts_get_cpu_topology_size(cpuinfo);
    system_cpudata = erts_alloc(ERTS_ALC_T_CPUDATA,
				(sizeof(erts_cpu_topology_t)
				 * system_cpudata_size));

    cpu_bind_order = ERTS_CPU_BIND_UNDEFINED;

    if (!erts_get_cpu_topology(cpuinfo, system_cpudata)
	|| ERTS_INIT_CPU_TOPOLOGY_OK != verify_topology(system_cpudata,
							system_cpudata_size)) {
	erts_free(ERTS_ALC_T_CPUDATA, system_cpudata);
	system_cpudata = NULL;
	system_cpudata_size = 0;
    }

    max_main_threads = erts_get_cpu_configured(cpuinfo);
    if (max_main_threads > no_schedulers || max_main_threads < 0)
	max_main_threads = no_schedulers;
    *max_main_threads_p = max_main_threads;

    decentralized_counter_groups = max_main_threads;
    if (decentralized_counter_groups <= 1 || max_decentralized_counter_groups <= 1)
	decentralized_counter_groups = 1;
    if (decentralized_counter_groups > max_decentralized_counter_groups)
	decentralized_counter_groups = max_decentralized_counter_groups;
    *decentralized_counter_groups_p = decentralized_counter_groups;
    reader_groups = max_main_threads;
    if (reader_groups <= 1 || max_reader_groups <= 1)
	reader_groups = 0;
    if (reader_groups > max_reader_groups)
	reader_groups = max_reader_groups;
    *reader_groups_p = reader_groups;
}

void
erts_init_cpu_topology(void)
{
    int ix;

    erts_rwmtx_init(&cpuinfo_rwmtx, "cpu_info", NIL,
        ERTS_LOCK_FLAGS_PROPERTY_STATIC | ERTS_LOCK_FLAGS_CATEGORY_GENERIC);
    erts_rwmtx_rwlock(&cpuinfo_rwmtx);

    scheduler2cpu_map = erts_alloc(ERTS_ALC_T_CPUDATA,
				   (sizeof(ErtsCpuBindData)
				    * (erts_no_schedulers+1)));
    for (ix = 1; ix <= erts_no_schedulers; ix++) {
	scheduler2cpu_map[ix].bind_id = -1;
	scheduler2cpu_map[ix].bound_id = -1;
    }

    if (cpu_bind_order == ERTS_CPU_BIND_UNDEFINED)
	cpu_bind_order = ERTS_CPU_BIND_NONE;

    reader_groups_map = add_cpu_groups(reader_groups,
				       reader_groups_callback,
				       NULL);
    decentralized_counter_groups_map = add_cpu_groups(decentralized_counter_groups,
                                                      flxctr_groups_callback,
                                                      NULL);

    if (cpu_bind_order == ERTS_CPU_BIND_NONE)
	erts_rwmtx_rwunlock(&cpuinfo_rwmtx);
    else {
	erts_cpu_topology_t *cpudata;
	int cpudata_size;
	create_tmp_cpu_topology_copy(&cpudata, &cpudata_size);
	write_schedulers_bind_change(cpudata, cpudata_size);
	erts_rwmtx_rwunlock(&cpuinfo_rwmtx);
	erts_sched_notify_check_cpu_bind();
	destroy_tmp_cpu_topology_copy(cpudata);
    }
}

int
erts_update_cpu_info(void)
{
    int changed;
    erts_rwmtx_rwlock(&cpuinfo_rwmtx);
    changed = erts_cpu_info_update(cpuinfo);
    if (changed) {
	erts_cpu_topology_t *cpudata;
	int cpudata_size;

	if (system_cpudata)
	    erts_free(ERTS_ALC_T_CPUDATA, system_cpudata);

	system_cpudata_size = erts_get_cpu_topology_size(cpuinfo);
	if (!system_cpudata_size)
	    system_cpudata = NULL;
	else {
	    system_cpudata = erts_alloc(ERTS_ALC_T_CPUDATA,
					(sizeof(erts_cpu_topology_t)
					 * system_cpudata_size));

	    if (!erts_get_cpu_topology(cpuinfo, system_cpudata)
		|| (ERTS_INIT_CPU_TOPOLOGY_OK
		    != verify_topology(system_cpudata,
				       system_cpudata_size))) {
		erts_free(ERTS_ALC_T_CPUDATA, system_cpudata);
		system_cpudata = NULL;
		system_cpudata_size = 0;
	    }
	}

	update_cpu_groups_maps();

	create_tmp_cpu_topology_copy(&cpudata, &cpudata_size);
	write_schedulers_bind_change(cpudata, cpudata_size);
	destroy_tmp_cpu_topology_copy(cpudata);
    }
    erts_rwmtx_rwunlock(&cpuinfo_rwmtx);
    if (changed)
	erts_sched_notify_check_cpu_bind();
    return changed;
}

/*
 * reader groups map
 */

void
reader_groups_callback(int suspending,
		       ErtsSchedulerData *esdp,
		       int group,
		       void *unused)
{
    if (reader_groups && esdp->no <= max_main_threads)
	erts_rwmtx_set_reader_group(suspending ? 0 : group+1);
}

void
flxctr_groups_callback(int suspending,
		       ErtsSchedulerData *esdp,
		       int group,
		       void *unused)
{
    erts_flxctr_set_slot(suspending ? 0 : group+1);
}

static Eterm get_cpu_groups_map(Process *c_p,
				erts_cpu_groups_map_t *map,
				int offset);
Eterm
erts_debug_reader_groups_map(Process *c_p, int groups)
{
    Eterm res;
    erts_cpu_groups_map_t test;

    test.array = NULL;
    test.groups = groups;
    make_cpu_groups_map(&test, 1);
    if (!test.array)
	res = NIL;
    else {
	res = get_cpu_groups_map(c_p, &test, 1);
	erts_free(ERTS_ALC_T_TMP, test.array);
    }
    return res;
}


Eterm
erts_get_reader_groups_map(Process *c_p)
{
    Eterm res;
    erts_rwmtx_rlock(&cpuinfo_rwmtx);
    res = get_cpu_groups_map(c_p, reader_groups_map, 1);
    erts_rwmtx_runlock(&cpuinfo_rwmtx);
    return res;
}

Eterm
erts_get_decentralized_counter_groups_map(Process *c_p)
{
    Eterm res;
    erts_rwmtx_rlock(&cpuinfo_rwmtx);
    res = get_cpu_groups_map(c_p, decentralized_counter_groups_map, 1);
    erts_rwmtx_runlock(&cpuinfo_rwmtx);
    return res;
}

/*
 * CPU groups
 */

static Eterm
get_cpu_groups_map(Process *c_p,
		   erts_cpu_groups_map_t *map,
		   int offset)
{
#ifdef DEBUG
    Eterm *endp;
#endif
    Eterm res = NIL, tuple;
    Eterm *hp;
    int i;

    hp = HAlloc(c_p, map->logical_processors*(2+3));
#ifdef DEBUG
    endp = hp + map->logical_processors*(2+3);
#endif
    for (i = map->size - 1; i >= 0; i--) {
	if (map->array[i].logical >= 0) {
	    tuple = TUPLE2(hp,
			   make_small(map->array[i].logical),
			   make_small(map->array[i].cpu_group + offset));
	    hp += 3;
	    res = CONS(hp, tuple, res);
	    hp += 2;
	}
    }
    ASSERT(hp == endp);
    return res;
}

static void
make_available_cpu_topology(erts_avail_cput *no,
			    erts_avail_cput *avail,
			    erts_cpu_topology_t *cpudata,
			    int *size,
			    int test)
{
    int len = *size;
    erts_cpu_topology_t last;
    int a, i, j;

    no->level[ERTS_TOPOLOGY_NODE] = -1;
    no->level[ERTS_TOPOLOGY_PROCESSOR] = -1;
    no->level[ERTS_TOPOLOGY_PROCESSOR_NODE] = -1;
    no->level[ERTS_TOPOLOGY_CORE] = -1;
    no->level[ERTS_TOPOLOGY_THREAD] = -1;
    no->level[ERTS_TOPOLOGY_LOGICAL] = -1;

    last.node = INT_MIN;
    last.processor = INT_MIN;
    last.processor_node = INT_MIN;
    last.core = INT_MIN;
    last.thread = INT_MIN;
    last.logical = INT_MIN;

    a = 0;

    for (i = 0; i < len; i++) {

	if (!test && !erts_is_cpu_available(cpuinfo, cpudata[i].logical))
	    continue;

	if (last.node != cpudata[i].node)
	    goto node;
	if (last.processor != cpudata[i].processor)
	    goto processor;
	if (last.processor_node != cpudata[i].processor_node)
	    goto processor_node;
	if (last.core != cpudata[i].core)
	    goto core;
	ASSERT(last.thread != cpudata[i].thread);
	goto thread;

    node:
	no->level[ERTS_TOPOLOGY_NODE]++;
    processor:
	no->level[ERTS_TOPOLOGY_PROCESSOR]++;
    processor_node:
	no->level[ERTS_TOPOLOGY_PROCESSOR_NODE]++;
    core:
	no->level[ERTS_TOPOLOGY_CORE]++;
    thread:
	no->level[ERTS_TOPOLOGY_THREAD]++;

	no->level[ERTS_TOPOLOGY_LOGICAL]++;

	for (j = 0; j < ERTS_TOPOLOGY_LOGICAL; j++)
	    avail[a].level[j] = no->level[j];

	avail[a].level[ERTS_TOPOLOGY_LOGICAL] = cpudata[i].logical;
	avail[a].level[ERTS_TOPOLOGY_CG] = 0;

	ASSERT(last.logical != cpudata[i].logical);

	last = cpudata[i];
	a++;
    }

    no->level[ERTS_TOPOLOGY_NODE]++;
    no->level[ERTS_TOPOLOGY_PROCESSOR]++;
    no->level[ERTS_TOPOLOGY_PROCESSOR_NODE]++;
    no->level[ERTS_TOPOLOGY_CORE]++;
    no->level[ERTS_TOPOLOGY_THREAD]++;
    no->level[ERTS_TOPOLOGY_LOGICAL]++;

    *size = a;
}

static void
cpu_group_insert(erts_cpu_groups_map_t *map,
		 int logical, int cpu_group)
{
    int start = logical % map->size;
    int ix = start;

    do {
	if (map->array[ix].logical < 0) {
	    map->array[ix].logical = logical;
	    map->array[ix].cpu_group = cpu_group;
	    return;
	}
	ix++;
	if (ix == map->size)
	    ix = 0;
    } while (ix != start);

    erts_exit(ERTS_ABORT_EXIT, "Reader groups map full\n");
}


static int
sub_levels(erts_cpu_groups_count_t *cgc, int level, int aix,
	   int avail_sz, erts_avail_cput *avail)
{
    int sub_level = level+1;
    int last = -1;
    cgc->sub_levels = 0;

    do {
	if (last != avail[aix].level[sub_level]) {
	    cgc->sub_levels++;
	    last = avail[aix].level[sub_level];
	}
	aix++;
    }
    while (aix < avail_sz && cgc->id == avail[aix].level[level]);
    cgc->cpu_groups = 0;
    return aix;
}

static int
write_cpu_groups(int *cgp, erts_cpu_groups_count_t *cgcp,
		    int level, int a,
		    int avail_sz, erts_avail_cput *avail)
{
    int cg = *cgp;
    int sub_level = level+1;
    int sl_per_gr = cgcp->sub_levels / cgcp->cpu_groups;
    int xsl = cgcp->sub_levels % cgcp->cpu_groups;
    int sls = 0;
    int last = -1;
    int xsl_cg_lim = (cgcp->cpu_groups - xsl) + cg + 1;

    ASSERT(level < 0 || avail[a].level[level] == cgcp->id);

    do {
	if (last != avail[a].level[sub_level]) {
	    if (!sls) {
		sls = sl_per_gr;
		cg++;
		if (cg >= xsl_cg_lim)
		    sls++;
	    }
	    last = avail[a].level[sub_level];
	    sls--;
	}
	avail[a].level[ERTS_TOPOLOGY_CG] = cg;
	a++;
    } while (a < avail_sz && (level < 0
			      || avail[a].level[level] == cgcp->id));

    ASSERT(cgcp->cpu_groups == cg - *cgp);

    *cgp = cg;

    return a;
}

static int
cg_count_sub_levels_compare(const void *vx, const void *vy)
{
    erts_cpu_groups_count_t *x = (erts_cpu_groups_count_t *) vx;
    erts_cpu_groups_count_t *y = (erts_cpu_groups_count_t *) vy;
    if (x->sub_levels != y->sub_levels)
	return y->sub_levels - x->sub_levels;
    return x->id - y->id;
}

static int
cg_count_id_compare(const void *vx, const void *vy)
{
    erts_cpu_groups_count_t *x = (erts_cpu_groups_count_t *) vx;
    erts_cpu_groups_count_t *y = (erts_cpu_groups_count_t *) vy;
    return x->id - y->id;
}

static void
make_cpu_groups_map(erts_cpu_groups_map_t *map, int test)
{
    int i, spread_level, avail_sz;
    erts_avail_cput no, *avail;
    erts_cpu_topology_t *cpudata;
    ErtsAlcType_t alc_type = (test
			      ? ERTS_ALC_T_TMP
			      : ERTS_ALC_T_CPU_GRPS_MAP);

    if (map->array)
	erts_free(alc_type, map->array);

    map->array = NULL;
    map->logical_processors = 0;
    map->size = 0;

    if (!map->groups)
	return;

    create_tmp_cpu_topology_copy(&cpudata, &avail_sz);

    if (!cpudata)
	return;

    cpu_bind_order_sort(cpudata,
			avail_sz,
			ERTS_CPU_BIND_NO_SPREAD,
			1);

    avail = erts_alloc(ERTS_ALC_T_TMP,
		       sizeof(erts_avail_cput)*avail_sz);

    make_available_cpu_topology(&no, avail, cpudata,
				&avail_sz, test);

    destroy_tmp_cpu_topology_copy(cpudata);

    map->size = avail_sz*2+1;

    map->array = erts_alloc(alc_type,
			    (sizeof(erts_cpu_groups_map_array_t)
			     * map->size));;
    map->logical_processors = avail_sz;

    for (i = 0; i < map->size; i++) {
	map->array[i].logical = -1;
	map->array[i].cpu_group = -1;
    }

    spread_level = ERTS_TOPOLOGY_CORE;
    for (i = ERTS_TOPOLOGY_NODE; i < ERTS_TOPOLOGY_THREAD; i++) {
	if (no.level[i] > map->groups) {
	    spread_level = i;
	    break;
	}
    }

    if (no.level[spread_level] <= map->groups) {
	int a, cg, last = -1;
	cg = -1;
	ASSERT(spread_level == ERTS_TOPOLOGY_CORE);
	for (a = 0; a < avail_sz; a++) {
	    if (last != avail[a].level[spread_level]) {
		cg++;
		last = avail[a].level[spread_level];
	    }
	    cpu_group_insert(map,
			     avail[a].level[ERTS_TOPOLOGY_LOGICAL],
			     cg);
	}
    }
    else { /* map->groups < no.level[spread_level] */
	erts_cpu_groups_count_t *cg_count;
	int a, cg, tl, toplevels;

	tl = spread_level-1;

	if (spread_level == ERTS_TOPOLOGY_NODE)
	    toplevels = 1;
	else
	    toplevels = no.level[tl];

	cg_count = erts_alloc(ERTS_ALC_T_TMP,
			      toplevels*sizeof(erts_cpu_groups_count_t));

	if (toplevels == 1) {
	    cg_count[0].id = 0;
	    cg_count[0].sub_levels = no.level[spread_level];
	    cg_count[0].cpu_groups = map->groups;
	}
	else {
	    int cgs_per_tl, cgs;
	    cgs = map->groups;
	    cgs_per_tl = cgs / toplevels;

	    a = 0;
	    for (i = 0; i < toplevels; i++) {
		cg_count[i].id = avail[a].level[tl];
		a = sub_levels(&cg_count[i], tl, a, avail_sz, avail);
	    }

	    qsort(cg_count,
		  toplevels,
		  sizeof(erts_cpu_groups_count_t),
		  cg_count_sub_levels_compare);

	    for (i = 0; i < toplevels; i++) {
		if (cg_count[i].sub_levels < cgs_per_tl) {
		    cg_count[i].cpu_groups = cg_count[i].sub_levels;
		    cgs -= cg_count[i].sub_levels;
		}
		else {
		    cg_count[i].cpu_groups = cgs_per_tl;
		    cgs -= cgs_per_tl;
		}
	    }

	    while (cgs > 0) {
		for (i = 0; i < toplevels; i++) {
		    if (cg_count[i].sub_levels == cg_count[i].cpu_groups)
			break;
		    else {
			cg_count[i].cpu_groups++;
			if (--cgs == 0)
			    break;
		    }
		}
	    }

	    qsort(cg_count,
		  toplevels,
		  sizeof(erts_cpu_groups_count_t),
		  cg_count_id_compare);
	}

	a = i = 0;
	cg = -1;
	while (a < avail_sz) {
	    a = write_cpu_groups(&cg, &cg_count[i], tl,
				 a, avail_sz, avail);
	    i++;
	}

	ASSERT(map->groups == cg + 1);

	for (a = 0; a < avail_sz; a++)
	    cpu_group_insert(map,
			     avail[a].level[ERTS_TOPOLOGY_LOGICAL],
			     avail[a].level[ERTS_TOPOLOGY_CG]);

	erts_free(ERTS_ALC_T_TMP, cg_count);
    }

    erts_free(ERTS_ALC_T_TMP, avail);
}

static erts_cpu_groups_map_t *
add_cpu_groups(int groups,
	       erts_cpu_groups_callback_t callback,
	       void *arg)
{
    int use_groups = groups;
    erts_cpu_groups_callback_list_t *cgcl;
    erts_cpu_groups_map_t *cgm;

    ERTS_LC_ASSERT(erts_lc_rwmtx_is_rwlocked(&cpuinfo_rwmtx));

    if (use_groups > max_main_threads)
	use_groups = max_main_threads;

    if (!use_groups)
	return NULL;

    no_cpu_groups_callbacks++;
    cgcl = erts_alloc(ERTS_ALC_T_CPU_GRPS_MAP,
		      sizeof(erts_cpu_groups_callback_list_t));
    cgcl->callback = callback;
    cgcl->arg = arg;

    for (cgm = cpu_groups_maps; cgm; cgm = cgm->next) {
	if (cgm->groups == use_groups) {
	    cgcl->next = cgm->callback_list;
	    cgm->callback_list = cgcl;
	    return cgm;
	}
    }


    cgm = erts_alloc(ERTS_ALC_T_CPU_GRPS_MAP,
		     sizeof(erts_cpu_groups_map_t));
    cgm->next = cpu_groups_maps;
    cgm->groups = use_groups;
    cgm->array = NULL;
    cgm->size = 0;
    cgm->logical_processors = 0;
    cgm->callback_list = cgcl;

    cgcl->next = NULL;

    make_cpu_groups_map(cgm, 0);

    cpu_groups_maps = cgm;

    return cgm;
}

static int
cpu_groups_lookup(erts_cpu_groups_map_t *map,
		  ErtsSchedulerData *esdp)
{
    int start, logical, ix;

    ERTS_LC_ASSERT(erts_lc_rwmtx_is_rlocked(&cpuinfo_rwmtx)
		       || erts_lc_rwmtx_is_rwlocked(&cpuinfo_rwmtx));

    if (esdp->cpu_id < 0)
	return (((int) esdp->no) - 1) % map->groups;

    logical = esdp->cpu_id;
    start = logical % map->size;
    ix = start;

    do {
	if (map->array[ix].logical == logical) {
	    int group = map->array[ix].cpu_group;
	    ASSERT(0 <= group && group < map->groups);
	    return group;
	}
	ix++;
	if (ix == map->size)
	    ix = 0;
    } while (ix != start);

    erts_exit(ERTS_ABORT_EXIT, "Logical cpu id %d not found\n", logical);
}

static void
update_cpu_groups_maps(void)
{
    erts_cpu_groups_map_t *cgm;
    ERTS_LC_ASSERT(erts_lc_rwmtx_is_rwlocked(&cpuinfo_rwmtx));

    for (cgm = cpu_groups_maps; cgm; cgm = cgm->next)
	make_cpu_groups_map(cgm, 0);
}
