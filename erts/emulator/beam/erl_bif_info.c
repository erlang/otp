/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1999-2020. All Rights Reserved.
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

#define ERTS_WANT_MEM_MAPPERS
#include "sys.h"
#include "erl_vm.h"
#include "global.h"
#include "erl_process.h"
#include "error.h"
#include "erl_driver.h"
#include "erl_nif.h"
#include "bif.h"
#include "big.h"
#include "erl_version.h"
#include "erl_compile_flags.h"
#include "erl_db_util.h"
#include "erl_message.h"
#include "erl_binary.h"
#include "erl_db.h"
#include "erl_mtrace.h"
#include "dist.h"
#include "erl_gc.h"
#include "erl_cpu_topology.h"
#include "erl_async.h"
#include "erl_thr_progress.h"
#include "erl_bif_unique.h"
#include "erl_map.h"
#include "erl_check_io.h"
#define ERTS_PTAB_WANT_DEBUG_FUNCS__
#include "erl_ptab.h"
#include "erl_time.h"
#include "erl_proc_sig_queue.h"
#include "erl_alloc_util.h"
#include "erl_global_literals.h"

#ifdef ERTS_ENABLE_LOCK_COUNT
#include "erl_lock_count.h"
#endif

#ifdef VALGRIND
#  include <valgrind/valgrind.h>
#  include <valgrind/memcheck.h>
#endif
#ifdef ADDRESS_SANITIZER
#  include <sanitizer/lsan_interface.h>
#endif

static Export* alloc_info_trap = NULL;
static Export* alloc_sizes_trap = NULL;
static Export* gather_io_bytes_trap = NULL;

static Export *gather_sched_wall_time_res_trap;
static Export *gather_msacc_res_trap;
static Export *gather_gc_info_res_trap;
static Export *gather_system_check_res_trap;

static Export *is_process_alive_trap;
static Export *get_internal_state_blocked;

#define DECL_AM(S) Eterm AM_ ## S = am_atom_put(#S, sizeof(#S) - 1)

static char otp_version[] = ERLANG_OTP_VERSION;
/* Keep erts_system_version as a global variable for easy access from a core */
static char erts_system_version[] = ("Erlang/OTP " ERLANG_OTP_RELEASE
				     "%s"
				     " [erts-" ERLANG_VERSION "]"
#ifndef OTP_RELEASE
#ifdef ERLANG_GIT_VERSION
				     " [source-" ERLANG_GIT_VERSION "]"
#else
				     " [source]"
#endif
#endif	
#ifdef ARCH_64
				     " [64-bit]"
#endif
				     " [smp:%beu:%beu]"
				     " [ds:%beu:%beu:%beu]"
#if defined(ERTS_DIRTY_SCHEDULERS_TEST)
				     " [dirty-schedulers-TEST]"
#endif
				     " [async-threads:%d]"
#ifdef BEAMASM
#ifdef NATIVE_ERLANG_STACK
				     " [jit:ns%s]"
#else
				     " [jit%s]"
#endif
#endif
#ifdef ET_DEBUG
#if ET_DEBUG
				     " [type-assertions]"
#endif
#endif	
#ifdef DEBUG
				     " [debug-compiled]"
#endif	
#ifdef ERTS_ENABLE_LOCK_CHECK
				     " [lock-checking]"
#endif
#ifdef ERTS_ENABLE_LOCK_COUNT
				     " [lock-counting]"
#endif
#ifdef ERTS_OPCODE_COUNTER_SUPPORT
				     " [instruction-counting]"
#endif
#ifdef VALGRIND
				     " [valgrind-compiled]"
#endif
#ifdef ADDRESS_SANITIZER
				     " [address-sanitizer]"
#endif
#ifdef ERTS_FRMPTR
				     " [frame-pointer]"
#endif
#ifdef USE_LTTNG
				     " [lttng]"
#endif
#ifdef USE_DTRACE
				     " [dtrace]"
#endif
#ifdef USE_SYSTEMTAP
				     " [systemtap]"
#endif
#ifdef SHCOPY
				     " [sharing-preserving]"
#endif
				     "\n");

#define ASIZE(a) (sizeof(a)/sizeof(a[0]))

#if defined(HAVE_SOLARIS_SPARC_PERFMON)
# include <sys/ioccom.h>
# define PERFMON_SETPCR			_IOW('P', 1, unsigned long long)
# define PERFMON_GETPCR			_IOR('P', 2, unsigned long long)
#endif

static Eterm
current_function(Process* p, ErtsHeapFactory *hfact, Process* rp,
                 int full_info, Uint reserve_size, int flags);
static Eterm
current_stacktrace(Process* p, ErtsHeapFactory *hfact, Process* rp,
                   Uint reserve_size, int flags);

Eterm
erts_bld_bin_list(Uint **hpp, Uint *szp, ErlOffHeap* oh, Eterm tail)
{
    union erl_off_heap_ptr u;
    Eterm res = tail;
    Eterm tuple;
    struct erts_tmp_aligned_offheap tmp;

    for (u.hdr = oh->first; u.hdr; u.hdr = u.hdr->next) {
        erts_align_offheap(&u, &tmp);
	if (u.hdr->thing_word == HEADER_PROC_BIN) {
	    Eterm val = erts_bld_uword(hpp, szp, (UWord) u.pb->val);
	    Eterm orig_size = erts_bld_uint(hpp, szp, u.pb->val->orig_size);
    
	    if (szp)
		*szp += 4+2;
	    if (hpp) {
		Uint refc = (Uint) erts_refc_read(&u.pb->val->intern.refc, 1);
		tuple = TUPLE3(*hpp, val, orig_size, make_small(refc));
		res = CONS(*hpp + 4, tuple, res);
		*hpp += 4+2;
	    }
	}
    }
    return res;
}

static Eterm
bld_magic_ref_bin_list(Uint **hpp, Uint *szp, ErlOffHeap* oh)
{
    struct erl_off_heap_header* ohh;
    Eterm res = NIL;
    Eterm tuple;

    for (ohh = oh->first; ohh; ohh = ohh->next) {
	if (is_ref_thing_header((*((Eterm *) ohh)))) {
            ErtsMRefThing *mrtp = (ErtsMRefThing *) ohh;
	    Eterm val = erts_bld_uword(hpp, szp, (UWord) mrtp->mb);
	    Eterm orig_size = erts_bld_uint(hpp, szp, mrtp->mb->orig_size);
    
	    if (szp)
		*szp += 4+2;
	    if (hpp) {
		Uint refc = (Uint) erts_refc_read(&mrtp->mb->intern.refc, 1);
		tuple = TUPLE3(*hpp, val, orig_size, make_small(refc));
		res = CONS(*hpp + 4, tuple, res);
		*hpp += 4+2;
	    }
	}
    }
    return res;
}


/*
  make_monitor_list:
  returns a list of records..
  -record(erl_monitor, {
            type, % process | port | time_offset | dist_process | resource
                  % | node | nodes | suspend
            dir, % origin | target
	    ref, % reference or []
	    pid, % Process or nodename
	    extra % registered name, integer or []
          }).
*/

static int do_calc_mon_size(ErtsMonitor *mon, void *vpsz, Sint reds)
{
    ErtsMonitorData *mdp = erts_monitor_to_data(mon);
    Uint *psz = vpsz;
    *psz += is_immed(mdp->ref) ? 0 : NC_HEAP_SIZE(mdp->ref);

    if (mon->type == ERTS_MON_TYPE_RESOURCE && erts_monitor_is_target(mon))
        *psz += erts_resource_ref_size(mon->other.ptr);
    else
        *psz += is_immed(mon->other.item) ? 0 : NC_HEAP_SIZE(mon->other.item);

    *psz += 9; /* CONS + 6-tuple */
    return 1;
}

typedef struct {
    Process *p;
    Eterm *hp;
    Eterm res;
    Eterm tag;
} MonListContext;

static int do_make_one_mon_element(ErtsMonitor *mon, void * vpmlc, Sint reds)
{
    ErtsMonitorData *mdp = erts_monitor_to_data(mon);
    MonListContext *pmlc = vpmlc;
    Eterm tup, t, d, r, p, x;

    r = is_immed(mdp->ref) ? mdp->ref : STORE_NC(&(pmlc->hp), &MSO(pmlc->p), mdp->ref);
    if (mon->type == ERTS_MON_TYPE_RESOURCE && erts_monitor_is_target(mon))
        p = erts_bld_resource_ref(&(pmlc->hp), &MSO(pmlc->p), mon->other.ptr);
    else
        p = (is_immed(mon->other.item)
             ? mon->other.item
             : STORE_NC(&(pmlc->hp), &MSO(pmlc->p), mon->other.item));

    if (mon->flags & ERTS_ML_FLG_NAME)
        x = ((ErtsMonitorDataExtended *) mdp)->u.name;
    else if (erts_monitor_is_target(mon))
        x = NIL;
    else if (mon->type == ERTS_MON_TYPE_NODE || mon->type == ERTS_MON_TYPE_NODES)
        x = make_small(((ErtsMonitorDataExtended *) mdp)->u.refc);
    else
        x = NIL;

    switch (mon->type) {
    case ERTS_MON_TYPE_PROC:
        t = am_process;
        break;
    case ERTS_MON_TYPE_PORT:
        t = am_port;
        break;
    case ERTS_MON_TYPE_TIME_OFFSET:
        t = am_time_offset;
        break;
    case ERTS_MON_TYPE_DIST_PROC: {
        ERTS_DECL_AM(dist_process);
        t = AM_dist_process;
        break;
    }
    case ERTS_MON_TYPE_RESOURCE: {
        ERTS_DECL_AM(resource);
        t = AM_resource;
        break;
    }
    case ERTS_MON_TYPE_NODE:
        t = am_node;
        break;
    case ERTS_MON_TYPE_NODES: {
        ERTS_DECL_AM(nodes);
        t = AM_nodes;
        break;
    }
    case ERTS_MON_TYPE_SUSPEND:
        t = am_suspend;
        break;
    default:
        ERTS_INTERNAL_ERROR("Unknown monitor type");
        t = am_error;
        break;
    }
    if (erts_monitor_is_target(mon)) {
        ERTS_DECL_AM(target);
        d = AM_target;
    }
    else {
        ERTS_DECL_AM(origin);
        d = AM_origin;
    }
    tup = TUPLE6(pmlc->hp, pmlc->tag, t, d, r, p, x);
    pmlc->hp += 7;
    pmlc->res = CONS(pmlc->hp, tup, pmlc->res);
    pmlc->hp += 2;
    return 1;
}

static Eterm 
make_monitor_list(Process *p, int tree, ErtsMonitor *root, Eterm tail)
{
    DECL_AM(erl_monitor);
    Uint sz = 0;
    MonListContext mlc;
    void (*foreach)(ErtsMonitor *,
                    ErtsMonitorFunc,
                    void *);

    foreach = tree ? erts_monitor_tree_foreach : erts_monitor_list_foreach;

    (*foreach)(root, do_calc_mon_size, &sz);
    if (sz == 0)
	return tail;
    mlc.p = p;
    mlc.hp = HAlloc(p,sz);
    mlc.res = tail;
    mlc.tag = AM_erl_monitor;
    (*foreach)(root, do_make_one_mon_element, &mlc);
    return mlc.res;
}

/*
  make_link_list:
  returns a list of records..
  -record(erl_link, {
            type, % process | port | dist_process
	    pid, % Process or port
            state, % linked | unlinking
            id % (address)
          }).
*/

static int calc_lnk_size(ErtsLink *lnk, void *vpsz, Sint reds)
{
    Uint *psz = vpsz;
    Uint sz = 0;
    UWord addr;

    if (lnk->type == ERTS_LNK_TYPE_DIST_PROC)
        addr = (UWord) erts_link_to_elink(lnk);
    else
        addr = (UWord) lnk;

    (void) erts_bld_uword(NULL, &sz, (UWord) addr);

    *psz += sz;
    *psz += is_immed(lnk->other.item) ? 0 : size_object(lnk->other.item);
    *psz += 8; /* CONS + 5-tuple */
    return 1;
}

typedef struct {
    Process *p;
    Eterm *hp;
    Eterm res;
    Eterm tag;
} LnkListContext;

static int make_one_lnk_element(ErtsLink *lnk, void * vpllc, Sint reds)
{
    LnkListContext *pllc = vpllc;
    Eterm tup, t, pid, id, state;
    UWord addr;
    ERTS_DECL_AM(linked);
    ERTS_DECL_AM(unlinking);

    if (lnk->type == ERTS_LNK_TYPE_DIST_PROC) {
        ErtsELink *elnk = erts_link_to_elink(lnk);
        state = elnk->unlinking ? AM_unlinking : AM_linked;
        addr = (UWord) elnk;
    }
    else {
        ErtsILink *ilnk = (ErtsILink *) lnk;
        state = ilnk->unlinking ? AM_unlinking : AM_linked;
        addr = (UWord) ilnk;
    }

    id = erts_bld_uword(&pllc->hp, NULL, (UWord) addr);

    if (is_immed(lnk->other.item))
        pid = lnk->other.item;
    else {
        Uint sz = size_object(lnk->other.item);
        pid = copy_struct(lnk->other.item, sz, &(pllc->hp), &MSO(pllc->p));
    }

    switch (lnk->type) {
    case ERTS_LNK_TYPE_PROC:
        t = am_process;
        break;
    case ERTS_LNK_TYPE_PORT:
        t = am_port;
        break;
    case ERTS_LNK_TYPE_DIST_PROC: {
        ERTS_DECL_AM(dist_process);
        t = AM_dist_process;
        break;
    }
    default:
        ERTS_INTERNAL_ERROR("Unkown link type");
        t = am_undefined;
        break;
    }

    tup = TUPLE5(pllc->hp, pllc->tag, t, pid, state, id);
    pllc->hp += 6;
    pllc->res = CONS(pllc->hp, tup, pllc->res);
    pllc->hp += 2;
    return 1;
}

static Eterm 
make_link_list(Process *p, int tree, ErtsLink *root, Eterm tail)
{
    DECL_AM(erl_link);
    Uint sz = 0;
    LnkListContext llc;
    void (*foreach)(ErtsLink *,
                    ErtsLinkFunc,
                    void *);

    foreach = tree ? erts_link_tree_foreach : erts_link_list_foreach;

    (*foreach)(root, calc_lnk_size, (void *) &sz);
    if (sz == 0) {
	return tail;
    }
    llc.p = p;
    llc.hp = HAlloc(p,sz);
    llc.res = tail;
    llc.tag = AM_erl_link;
    (*foreach)(root, make_one_lnk_element, (void *) &llc);
    return llc.res;
}

int
erts_print_system_version(fmtfn_t to, void *arg, Process *c_p)
{
    int i, rc = -1;
    char *rc_str = "";
    char rc_buf[100];
    char *ov = otp_version;
    Uint total, online, active;
    Uint dirty_cpu, dirty_cpu_onln, dirty_io;

    erts_schedulers_state(&total, &online, &active,
			  &dirty_cpu, &dirty_cpu_onln, NULL,
			  &dirty_io, NULL);
    for (i = 0; i < sizeof(otp_version)-4; i++) {
	if (ov[i] == '-' && ov[i+1] == 'r' && ov[i+2] == 'c')
	    rc = atoi(&ov[i+3]);
    }
    if (rc >= 0) {
	if (rc == 0)
	    rc_str = " [DEVELOPMENT]";
	else {
	    erts_snprintf(rc_buf, sizeof(rc_buf), " [RELEASE CANDIDATE %d]", rc);
	    rc_str = rc_buf;
	}
    }
    return erts_print(to, arg, erts_system_version,
		      rc_str
		      , total, online
		      , dirty_cpu, dirty_cpu_onln, dirty_io
		      , erts_async_max_threads
              , (erts_frame_layout == ERTS_FRAME_LAYOUT_FP_RA ? ":fp" : "")
	);
}

typedef struct {
    /* {Entity,Node} = {monitor.Name,monitor.Pid} for external by name
     * {Entity,Node} = {monitor.Pid,NIL} for external/external by pid
     * {Entity,Node} = {monitor.Name,erlang:node()} for internal by name 
     * {Entity,Node} = {monitor.resource,MON_NIF_TARGET}*/
    union {
	Eterm term;
	ErtsResource* resource;
    }entity;
    int named;
    Uint16 type;
    Eterm node;
    /* pid is actual target being monitored, no matter pid/port or name */
    Eterm pid;
} MonitorInfo;

typedef struct {
    MonitorInfo *mi;
    Uint mi_i;
    Uint mi_max;
    int sz;
} MonitorInfoCollection;

#define INIT_MONITOR_INFOS(MIC) do {		\
    (MIC).mi = NULL;				\
    (MIC).mi_i = (MIC).mi_max = 0;		\
    (MIC).sz = 0;                               \
} while(0)

#define MI_INC 50
#define EXTEND_MONITOR_INFOS(MICP)					\
do {									\
    if ((MICP)->mi_i >= (MICP)->mi_max) {				\
	(MICP)->mi = ((MICP)->mi ? erts_realloc(ERTS_ALC_T_TMP,		\
						(MICP)->mi,		\
						((MICP)->mi_max+MI_INC)	\
						* sizeof(MonitorInfo))	\
		      : erts_alloc(ERTS_ALC_T_TMP,			\
				   MI_INC*sizeof(MonitorInfo)));	\
	(MICP)->mi_max += MI_INC;					\
    }									\
 } while (0)
#define DESTROY_MONITOR_INFOS(MIC)			\
do {							\
    if ((MIC).mi != NULL) {				\
	erts_free(ERTS_ALC_T_TMP, (void *) (MIC).mi);	\
    }							\
 } while (0)

static int collect_one_link(ErtsLink *lnk, void *vmicp, Sint reds)
{
    MonitorInfoCollection *micp = vmicp;
    if (lnk->type != ERTS_LNK_TYPE_DIST_PROC) {
        if (((ErtsILink *) lnk)->unlinking)
            return 1;
    }
    else {
        ErtsELink *elnk = erts_link_to_elink(lnk);
        if (elnk->unlinking)
            return 1;
    }
    EXTEND_MONITOR_INFOS(micp);
    micp->mi[micp->mi_i].entity.term = lnk->other.item;
    micp->sz += 2 + NC_HEAP_SIZE(lnk->other.item);
    micp->mi_i++;
    return 1;
} 

static int collect_one_origin_monitor(ErtsMonitor *mon, void *vmicp, Sint reds)
{
    if (erts_monitor_is_origin(mon)) {
        MonitorInfoCollection *micp = vmicp;
 
        EXTEND_MONITOR_INFOS(micp);

        micp->mi[micp->mi_i].type = mon->type;

        switch (mon->type) {
        case ERTS_MON_TYPE_PROC:
        case ERTS_MON_TYPE_PORT:
        case ERTS_MON_TYPE_DIST_PROC:
        case ERTS_MON_TYPE_TIME_OFFSET:
            if (mon->flags & ERTS_ML_FLG_SPAWN_PENDING)
                break; /* Not an active monitor... */
            if (!(mon->flags & ERTS_ML_FLG_NAME)) {
                micp->mi[micp->mi_i].named = 0;
                micp->mi[micp->mi_i].entity.term = mon->other.item;
                micp->mi[micp->mi_i].node = NIL;
                if (is_not_atom(mon->other.item))
                    micp->sz += NC_HEAP_SIZE(mon->other.item);
            }
            else {
                ErtsMonitorDataExtended *mdep;
                micp->mi[micp->mi_i].named = !0;
                mdep = (ErtsMonitorDataExtended *) erts_monitor_to_data(mon);
                micp->mi[micp->mi_i].entity.term = mdep->u.name;
                if (mdep->dist)
                    micp->mi[micp->mi_i].node = mdep->dist->nodename;
                else
                    micp->mi[micp->mi_i].node = erts_this_dist_entry->sysname;
                micp->sz += 3; /* need one 2-tuple */
            }

            /* have always pid at hand, to assist with figuring out if its a port or
             * a process, when we monitored by name and process_info is requested.
             * See: erl_bif_info.c:process_info_aux section for am_monitors */
            micp->mi[micp->mi_i].pid = mon->other.item;

            micp->mi_i++;
            micp->sz += 2 + 3; /* For a cons cell and a 2-tuple */
            break;
        default:
            break;
        }
    }
    return 1;
}

static int collect_one_target_monitor(ErtsMonitor *mon, void *vmicp, Sint reds)
{
    MonitorInfoCollection *micp = vmicp;
 
    if (erts_monitor_is_target(mon)) {

        EXTEND_MONITOR_INFOS(micp);
  
        micp->mi[micp->mi_i].type = mon->type;
        micp->mi[micp->mi_i].named = !!(mon->flags & ERTS_ML_FLG_NAME);
        switch (mon->type) {

        case ERTS_MON_TYPE_PROC:
        case ERTS_MON_TYPE_PORT:
        case ERTS_MON_TYPE_DIST_PROC:

            micp->mi[micp->mi_i].entity.term = mon->other.item;
            micp->mi[micp->mi_i].node = NIL;
            micp->sz += NC_HEAP_SIZE(mon->other.item);

            micp->sz += 2; /* cons */;
            micp->mi_i++;
            break;

        case ERTS_MON_TYPE_RESOURCE:

            micp->mi[micp->mi_i].entity.resource = mon->other.ptr;
            micp->mi[micp->mi_i].node = NIL;
            micp->sz += erts_resource_ref_size(mon->other.ptr);

            micp->sz += 2; /* cons */;
            micp->mi_i++;
            break;

        default:
            break;
        }
    }
    return 1;
}

typedef struct {
    ErtsMonitorSuspend **smi;
    Uint smi_i;
    Uint smi_max;
    Uint sz;
} ErtsSuspendMonitorInfoCollection;

#define ERTS_INIT_SUSPEND_MONITOR_INFOS(SMIC) do {		        \
    (SMIC).smi = NULL;							\
    (SMIC).smi_i = (SMIC).smi_max = 0;					\
    (SMIC).sz = 0;                               			\
} while(0)

#define ERTS_SMI_INC 50
#define ERTS_EXTEND_SUSPEND_MONITOR_INFOS(SMICP)			\
do {									\
    if ((SMICP)->smi_i >= (SMICP)->smi_max) {				\
	(SMICP)->smi = ((SMICP)->smi					\
			? erts_realloc(ERTS_ALC_T_TMP,			\
				       (SMICP)->smi,			\
				       ((SMICP)->smi_max		\
					+ ERTS_SMI_INC)			\
				       * sizeof(ErtsMonitorSuspend *))	\
			: erts_alloc(ERTS_ALC_T_TMP,			\
				     ERTS_SMI_INC			\
				     * sizeof(ErtsMonitorSuspend *)));	\
	(SMICP)->smi_max += ERTS_SMI_INC;				\
    }									\
 } while (0)

#define ERTS_DESTROY_SUSPEND_MONITOR_INFOS(SMIC)			\
do {									\
    if ((SMIC).smi != NULL) {						\
	erts_free(ERTS_ALC_T_TMP, (void *) (SMIC).smi);			\
    }									\
 } while (0)

static int
collect_one_suspend_monitor(ErtsMonitor *mon, void *vsmicp, Sint reds)
{
    if (mon->type == ERTS_MON_TYPE_SUSPEND) {
        Sint count;
        erts_aint_t mstate;
        ErtsMonitorSuspend *msp;
        ErtsSuspendMonitorInfoCollection *smicp;

        msp = (ErtsMonitorSuspend *) erts_monitor_to_data(mon);
        smicp = vsmicp;

	ERTS_EXTEND_SUSPEND_MONITOR_INFOS(smicp);

	smicp->smi[smicp->smi_i] = msp;
	smicp->sz += 2 /* cons */ + 4 /* 3-tuple */;

        mstate = erts_atomic_read_nob(&msp->state);

        count = (Sint) (mstate & ERTS_MSUSPEND_STATE_COUNTER_MASK);
	if (!IS_SSMALL(count))
	    smicp->sz += BIG_UINT_HEAP_SIZE;

	smicp->smi_i++;
    }
    return 1;
}

/*
 * process_info/[1,2]
 */

/*
 * All valid process_info arguments.
 */

#define ERTS_PI_IX_REGISTERED_NAME                      0
#define ERTS_PI_IX_CURRENT_FUNCTION                     1
#define ERTS_PI_IX_INITIAL_CALL                         2
#define ERTS_PI_IX_STATUS                               3
#define ERTS_PI_IX_MESSAGES                             4
#define ERTS_PI_IX_MESSAGE_QUEUE_LEN                    5
#define ERTS_PI_IX_LINKS                                6
#define ERTS_PI_IX_MONITORS                             7
#define ERTS_PI_IX_MONITORED_BY                         8
#define ERTS_PI_IX_DICTIONARY                           9
#define ERTS_PI_IX_TRAP_EXIT                            10
#define ERTS_PI_IX_ERROR_HANDLER                        11
#define ERTS_PI_IX_HEAP_SIZE                            12
#define ERTS_PI_IX_STACK_SIZE                           13
#define ERTS_PI_IX_MEMORY                               14
#define ERTS_PI_IX_GARBAGE_COLLECTION                   15
#define ERTS_PI_IX_GROUP_LEADER                         16
#define ERTS_PI_IX_REDUCTIONS                           17
#define ERTS_PI_IX_PRIORITY                             18
#define ERTS_PI_IX_TRACE                                19
#define ERTS_PI_IX_BINARY                               20
#define ERTS_PI_IX_SEQUENTIAL_TRACE_TOKEN               21
#define ERTS_PI_IX_CATCHLEVEL                           22
#define ERTS_PI_IX_BACKTRACE                            23
#define ERTS_PI_IX_LAST_CALLS                           24
#define ERTS_PI_IX_TOTAL_HEAP_SIZE                      25
#define ERTS_PI_IX_SUSPENDING                           26
#define ERTS_PI_IX_MIN_HEAP_SIZE                        27
#define ERTS_PI_IX_MIN_BIN_VHEAP_SIZE                   28
#define ERTS_PI_IX_MAX_HEAP_SIZE                        29
#define ERTS_PI_IX_CURRENT_LOCATION                     30
#define ERTS_PI_IX_CURRENT_STACKTRACE                   31
#define ERTS_PI_IX_MESSAGE_QUEUE_DATA                   32
#define ERTS_PI_IX_GARBAGE_COLLECTION_INFO              33
#define ERTS_PI_IX_MAGIC_REF                            34
#define ERTS_PI_IX_FULLSWEEP_AFTER                      35

#define ERTS_PI_FLAG_SINGELTON                          (1 << 0)
#define ERTS_PI_FLAG_ALWAYS_WRAP                        (1 << 1)
#define ERTS_PI_FLAG_WANT_MSGS                          (1 << 2)
#define ERTS_PI_FLAG_NEED_MSGQ_LEN                      (1 << 3)
#define ERTS_PI_FLAG_FORCE_SIG_SEND                     (1 << 4)
#define ERTS_PI_FLAG_REQUEST_FOR_OTHER                  (1 << 5)

#define ERTS_PI_UNRESERVE(RS, SZ) \
    (ASSERT((RS) >= (SZ)), (RS) -= (SZ))


typedef struct {
    Eterm name;
    Uint reserve_size;
    int flags;
    ErtsProcLocks locks;
} ErtsProcessInfoArgs;

static ErtsProcessInfoArgs pi_args[] = {
    {am_registered_name, 0, 0, ERTS_PROC_LOCK_MAIN},
    {am_current_function, 4, ERTS_PI_FLAG_FORCE_SIG_SEND, ERTS_PROC_LOCK_MAIN},
    {am_initial_call, 4, 0, ERTS_PROC_LOCK_MAIN},
    {am_status, 0, 0, 0},
    {am_messages, 0, ERTS_PI_FLAG_WANT_MSGS|ERTS_PI_FLAG_NEED_MSGQ_LEN|ERTS_PI_FLAG_FORCE_SIG_SEND, ERTS_PROC_LOCK_MAIN},
    {am_message_queue_len, 0, ERTS_PI_FLAG_NEED_MSGQ_LEN, ERTS_PROC_LOCK_MAIN},
    {am_links, 0, ERTS_PI_FLAG_FORCE_SIG_SEND, ERTS_PROC_LOCK_MAIN},
    {am_monitors, 0, ERTS_PI_FLAG_FORCE_SIG_SEND, ERTS_PROC_LOCK_MAIN},
    {am_monitored_by, 0, ERTS_PI_FLAG_FORCE_SIG_SEND, ERTS_PROC_LOCK_MAIN},
    {am_dictionary, 0, ERTS_PI_FLAG_FORCE_SIG_SEND, ERTS_PROC_LOCK_MAIN},
    {am_trap_exit, 0, 0, ERTS_PROC_LOCK_MAIN},
    {am_error_handler, 0, 0, ERTS_PROC_LOCK_MAIN},
    {am_heap_size, 0, 0, ERTS_PROC_LOCK_MAIN},
    {am_stack_size, 0, 0, ERTS_PROC_LOCK_MAIN},
    {am_memory, 0, ERTS_PI_FLAG_NEED_MSGQ_LEN|ERTS_PI_FLAG_FORCE_SIG_SEND, ERTS_PROC_LOCK_MAIN},
    {am_garbage_collection, 3+2 + 3+2 + 3+2 + 3+2 + 3+2 + ERTS_MAX_HEAP_SIZE_MAP_SZ, 0, ERTS_PROC_LOCK_MAIN},
    {am_group_leader, 0, 0, ERTS_PROC_LOCK_MAIN},
    {am_reductions, 0, 0, ERTS_PROC_LOCK_MAIN},
    {am_priority, 0, 0, 0},
    {am_trace, 0, 0, ERTS_PROC_LOCK_MAIN},
    {am_binary, 0, ERTS_PI_FLAG_FORCE_SIG_SEND, ERTS_PROC_LOCK_MAIN},
    {am_sequential_trace_token, 0, 0, ERTS_PROC_LOCK_MAIN},
    {am_catchlevel, 0, 0, ERTS_PROC_LOCK_MAIN},
    {am_backtrace, 0, ERTS_PI_FLAG_FORCE_SIG_SEND, ERTS_PROC_LOCK_MAIN},
    {am_last_calls, 0, 0, ERTS_PROC_LOCK_MAIN},
    {am_total_heap_size, 0, ERTS_PI_FLAG_NEED_MSGQ_LEN|ERTS_PI_FLAG_FORCE_SIG_SEND, ERTS_PROC_LOCK_MAIN},
    {am_suspending, 0, ERTS_PI_FLAG_FORCE_SIG_SEND, 0},
    {am_min_heap_size, 0, 0, ERTS_PROC_LOCK_MAIN},
    {am_min_bin_vheap_size, 0, 0, ERTS_PROC_LOCK_MAIN},
    {am_max_heap_size, 0, 0, ERTS_PROC_LOCK_MAIN},
    {am_current_location, 0, ERTS_PI_FLAG_FORCE_SIG_SEND, ERTS_PROC_LOCK_MAIN},
    {am_current_stacktrace, 0, ERTS_PI_FLAG_FORCE_SIG_SEND, ERTS_PROC_LOCK_MAIN},
    {am_message_queue_data, 0, 0, ERTS_PROC_LOCK_MAIN},
    {am_garbage_collection_info, ERTS_PROCESS_GC_INFO_MAX_SIZE, 0, ERTS_PROC_LOCK_MAIN},
    {am_magic_ref, 0, ERTS_PI_FLAG_FORCE_SIG_SEND, ERTS_PROC_LOCK_MAIN},
    {am_fullsweep_after, 0, 0, ERTS_PROC_LOCK_MAIN}
};

#define ERTS_PI_ARGS ((int) (sizeof(pi_args)/sizeof(pi_args[0])))

#ifdef DEBUG
#  define ERTS_PI_DEF_ARR_SZ 2
#else
#  define ERTS_PI_DEF_ARR_SZ ERTS_PI_ARGS
#endif

static ERTS_INLINE Eterm
pi_ix2arg(int ix)
{
    if (ix < 0 || ERTS_PI_ARGS <= ix)
	return am_undefined;
    return pi_args[ix].name;
}

static ERTS_INLINE int
pi_ix2flags(int ix)
{
    if (ix < 0 || ERTS_PI_ARGS <= ix)
	return 0;
    return pi_args[ix].flags;
}

static ERTS_INLINE Uint
pi_ix2rsz(int ix)
{
    if (ix < 0 || ERTS_PI_ARGS <= ix)
	return 0;
    return pi_args[ix].reserve_size;
}

static ERTS_INLINE ErtsProcLocks
pi_ix2locks(int ix)
{
    if (ix < 0 || ERTS_PI_ARGS <= ix)
	return 0;
    return pi_args[ix].locks;
}

static ERTS_INLINE int
pi_arg2ix(Eterm arg)
{
    switch (arg) {
    case am_registered_name:
        return ERTS_PI_IX_REGISTERED_NAME;
    case am_current_function:
        return ERTS_PI_IX_CURRENT_FUNCTION;
    case am_initial_call:
        return ERTS_PI_IX_INITIAL_CALL;
    case am_status:
        return ERTS_PI_IX_STATUS;
    case am_messages:
        return ERTS_PI_IX_MESSAGES;
    case am_message_queue_len:
        return ERTS_PI_IX_MESSAGE_QUEUE_LEN;
    case am_links:
        return ERTS_PI_IX_LINKS;
    case am_monitors:
        return ERTS_PI_IX_MONITORS;
    case am_monitored_by:
        return ERTS_PI_IX_MONITORED_BY;
    case am_dictionary:
        return ERTS_PI_IX_DICTIONARY;
    case am_trap_exit:
        return ERTS_PI_IX_TRAP_EXIT;
    case am_error_handler:
        return ERTS_PI_IX_ERROR_HANDLER;
    case am_heap_size:
        return ERTS_PI_IX_HEAP_SIZE;
    case am_stack_size:
        return ERTS_PI_IX_STACK_SIZE;
    case am_memory:
        return ERTS_PI_IX_MEMORY;
    case am_garbage_collection:
        return ERTS_PI_IX_GARBAGE_COLLECTION;
    case am_group_leader:
        return ERTS_PI_IX_GROUP_LEADER;
    case am_reductions:
        return ERTS_PI_IX_REDUCTIONS;
    case am_priority:
        return ERTS_PI_IX_PRIORITY;
    case am_trace:
        return ERTS_PI_IX_TRACE;
    case am_binary:
        return ERTS_PI_IX_BINARY;
    case am_sequential_trace_token:
        return ERTS_PI_IX_SEQUENTIAL_TRACE_TOKEN;
    case am_catchlevel:
        return ERTS_PI_IX_CATCHLEVEL;
    case am_backtrace:
        return ERTS_PI_IX_BACKTRACE;
    case am_last_calls:
        return ERTS_PI_IX_LAST_CALLS;
    case am_total_heap_size:
        return ERTS_PI_IX_TOTAL_HEAP_SIZE;
    case am_suspending:
        return ERTS_PI_IX_SUSPENDING;
    case am_min_heap_size:
        return ERTS_PI_IX_MIN_HEAP_SIZE;
    case am_min_bin_vheap_size:
        return ERTS_PI_IX_MIN_BIN_VHEAP_SIZE;
    case am_max_heap_size:
        return ERTS_PI_IX_MAX_HEAP_SIZE;
    case am_current_location:
        return ERTS_PI_IX_CURRENT_LOCATION;
    case am_current_stacktrace:
        return ERTS_PI_IX_CURRENT_STACKTRACE;
    case am_message_queue_data:
        return ERTS_PI_IX_MESSAGE_QUEUE_DATA;
    case am_garbage_collection_info:
	return ERTS_PI_IX_GARBAGE_COLLECTION_INFO;
    case am_magic_ref:
        return ERTS_PI_IX_MAGIC_REF;
    case am_fullsweep_after:
        return ERTS_PI_IX_FULLSWEEP_AFTER;
    default:
        return -1;
    }
}

static Eterm pi_1_keys[] = {
    am_registered_name,
    am_current_function,
    am_initial_call,
    am_status,
    am_message_queue_len,
    am_links,
    am_dictionary,
    am_trap_exit,
    am_error_handler,
    am_priority,
    am_group_leader,
    am_total_heap_size,
    am_heap_size,
    am_stack_size,
    am_reductions,
    am_garbage_collection,
    am_suspending
};

#define ERTS_PI_1_NO_OF_KEYS (sizeof(pi_1_keys)/sizeof(Eterm))

static Eterm pi_1_keys_list;
static Eterm pi_1_keys_list_heap[2*ERTS_PI_1_NO_OF_KEYS];

static void
process_info_init(void)
{
    Eterm *hp = &pi_1_keys_list_heap[0];
    int i;

    pi_1_keys_list = NIL;

    for (i = ERTS_PI_1_NO_OF_KEYS-1; i >= 0; i--) {
	pi_1_keys_list = CONS(hp, pi_1_keys[i], pi_1_keys_list);
	hp += 2;
    }

#ifdef DEBUG
    { /* Make sure the process_info argument mappings are consistent */
	int ix;
	for (ix = 0; ix < ERTS_PI_ARGS; ix++) {
	    ASSERT(pi_arg2ix(pi_ix2arg(ix)) == ix);
	}
    }
#endif

}

static BIF_RETTYPE
process_info_aux(Process *c_p,
                 ErtsHeapFactory *hfact,
		 Process *rp,
		 ErtsProcLocks rp_locks,
		 int item_ix,
		 int flags,
                 Uint *reserve_sizep,
                 Uint *reds);

Eterm
erts_process_info(Process *c_p,
                  ErtsHeapFactory *hfact,
                  Process *rp,
                  ErtsProcLocks rp_locks,
                  int *item_ix,
                  int item_ix_len,
                  int flags,
                  Uint reserve_size,
                  Uint *reds)
{
    Eterm res;
    Eterm part_res[ERTS_PI_ARGS];
    int item_ix_ix, ix;

    if (ERTS_PI_FLAG_SINGELTON & flags) {
        ASSERT(item_ix_len == 1);
	res = process_info_aux(c_p, hfact, rp, rp_locks, item_ix[0],
                               flags, &reserve_size, reds);
        return res;
    }

    for (ix = 0; ix < ERTS_PI_ARGS; ix++)
	part_res[ix] = THE_NON_VALUE;

    /*
     * We always handle 'messages' first if it should be part
     * of the result. This since if both 'messages' and
     * 'message_queue_len' are wanted, 'messages' may
     * change the result of 'message_queue_len' (in case
     * the queue contain bad distribution messages).
     */
    if (flags & ERTS_PI_FLAG_WANT_MSGS) {
	ix = pi_arg2ix(am_messages);
	ASSERT(part_res[ix] == THE_NON_VALUE);
	res = process_info_aux(c_p, hfact, rp, rp_locks, ix,
                               flags, &reserve_size, reds);
	ASSERT(res != am_undefined);
	ASSERT(res != THE_NON_VALUE);
        part_res[ix] = res;
    }

    for (item_ix_ix = item_ix_len - 1; item_ix_ix >= 0; item_ix_ix--) {
	ix = item_ix[item_ix_ix];
	if (part_res[ix] == THE_NON_VALUE) {
	    res = process_info_aux(c_p, hfact, rp, rp_locks, ix,
                                   flags, &reserve_size, reds);
            ASSERT(res != am_undefined);
	    ASSERT(res != THE_NON_VALUE);
            part_res[ix] = res;
	}
    }

    res = NIL;

    for (item_ix_ix = item_ix_len - 1; item_ix_ix >= 0; item_ix_ix--) {
	ix = item_ix[item_ix_ix];
	ASSERT(part_res[ix] != THE_NON_VALUE);
	/*
	 * If we should ignore the value of registered_name,
	 * its value is nil. For more info, see comment in the
	 * beginning of process_info_aux().
	 */
	if (is_nil(part_res[ix])) {
	    ASSERT(!(flags & ERTS_PI_FLAG_ALWAYS_WRAP));
	    ASSERT(pi_ix2arg(ix) == am_registered_name);
	}
	else {
            Eterm *hp;
            ERTS_PI_UNRESERVE(reserve_size, 2);
            hp = erts_produce_heap(hfact, 2, reserve_size);
	    res = CONS(hp, part_res[ix], res);
	}
    }

    return res;
}

static void
pi_setup_grow(int **arr, int *def_arr, Uint *sz, int ix);

static BIF_RETTYPE
process_info_bif(Process *c_p, Eterm pid, Eterm opt, int always_wrap, int pi2)
{
    ErtsHeapFactory hfact;
    int def_arr[ERTS_PI_DEF_ARR_SZ];
    int *item_ix = &def_arr[0];
    Process *rp = NULL;
    erts_aint32_t state;
    BIF_RETTYPE ret;
    Uint reds = 0;
    ErtsProcLocks locks = 0;
    int flags;
    Uint reserve_size;
    int len;
    Eterm res;

    ERTS_CT_ASSERT(ERTS_PI_DEF_ARR_SZ > 0);

    if (c_p->common.id == pid) {
        int local_only = c_p->sig_qs.flags & FS_LOCAL_SIGS_ONLY;
        int sres, sreds, reds_left;

        reds_left = ERTS_BIF_REDS_LEFT(c_p);
        sreds = reds_left;

        if (!local_only) {
            erts_proc_lock(c_p, ERTS_PROC_LOCK_MSGQ);
            erts_proc_sig_fetch(c_p);
            erts_proc_unlock(c_p, ERTS_PROC_LOCK_MSGQ);
        }

        sres = erts_proc_sig_handle_incoming(c_p, &state, &sreds, sreds, !0);

        BUMP_REDS(c_p, (int) sreds);
        reds_left -= sreds;

        if (state & ERTS_PSFLG_EXITING) {
            c_p->sig_qs.flags &= ~FS_LOCAL_SIGS_ONLY;
            goto exited;
        }
        if (!sres | (reds_left <= 0)) {
            /*
             * More signals to handle or out of reds; need
             * to yield and continue. Prevent fetching of
             * more signals by setting local-sigs-only flag.
             */
            c_p->sig_qs.flags |= FS_LOCAL_SIGS_ONLY;
            goto yield;
        }

        c_p->sig_qs.flags &= ~FS_LOCAL_SIGS_ONLY;
    }

    if (is_atom(opt)) {
	int ix = pi_arg2ix(opt);
        item_ix[0] = ix;
        len = 1;
        locks = pi_ix2locks(ix);
        reserve_size = 3 + pi_ix2rsz(ix);
        flags = ERTS_PI_FLAG_SINGELTON;
        flags |= pi_ix2flags(ix);
        if (ix < 0)
            goto badarg;
    }
    else {
        Eterm list = opt;
        Uint size = ERTS_PI_DEF_ARR_SZ;

        len = 0;
        reserve_size = 0;
        locks = 0;
        flags = 0;

        while (is_list(list)) {
            Eterm *consp = list_val(list);
            Eterm arg = CAR(consp);
            int ix = pi_arg2ix(arg);
            if (ix < 0)
                goto badarg;

            if (len >= size)
                pi_setup_grow(&item_ix, def_arr, &size, len);

            item_ix[len++] = ix;

            locks |= pi_ix2locks(ix);
            flags |= pi_ix2flags(ix);
            reserve_size += pi_ix2rsz(ix);
            reserve_size += 3; /* 2-tuple */
            reserve_size += 2; /* cons */

            list = CDR(consp);
        }

        if (is_not_nil(list))
            goto badarg;
    }

    if (is_not_internal_pid(pid)) {
        if (is_external_pid(pid)
            && external_pid_dist_entry(pid) == erts_this_dist_entry)
            goto undefined;
        goto badarg;
    }

    if (always_wrap)
        flags |= ERTS_PI_FLAG_ALWAYS_WRAP;

    if (c_p->common.id == pid) {
        rp = c_p;
        if (locks & ~ERTS_PROC_LOCK_MAIN)
            erts_proc_lock(c_p, locks & ~ERTS_PROC_LOCK_MAIN);
        locks |= ERTS_PROC_LOCK_MAIN;
    }
    else {
        if (flags & ERTS_PI_FLAG_FORCE_SIG_SEND)
            goto send_signal;
        state = ERTS_PSFLG_RUNNING; /* fail state... */
        rp = erts_try_lock_sig_free_proc(pid, locks, &state);
        if (!rp)
            goto undefined;
        if (rp == ERTS_PROC_LOCK_BUSY) {
            rp = NULL;
            goto send_signal;
        }
        if (state & ERTS_PSFLG_EXITING) {
            if (locks)
                erts_proc_unlock(rp, locks);
            locks = 0;
            /* wait for it to terminate properly... */
            goto send_signal;
        }
        if (flags & ERTS_PI_FLAG_NEED_MSGQ_LEN) {
            ASSERT(locks & ERTS_PROC_LOCK_MAIN);
            erts_proc_lock(rp, ERTS_PROC_LOCK_MSGQ);
            erts_proc_sig_fetch(rp);
            if (c_p->sig_qs.cont) {
                erts_proc_unlock(rp, locks|ERTS_PROC_LOCK_MSGQ);
                locks = 0;
                goto send_signal;
            }
            erts_proc_unlock(rp, ERTS_PROC_LOCK_MSGQ);
        }
    }

    erts_factory_proc_init(&hfact, c_p);

    res = erts_process_info(c_p, &hfact, rp, locks, item_ix, len,
                            flags, reserve_size, &reds);

    erts_factory_close(&hfact);

    if (reds > INT_MAX/2)
        reds = INT_MAX/2;
    BUMP_REDS(c_p, (int) reds);

    state = erts_atomic32_read_acqb(&rp->state);
    if (state & (ERTS_PSFLG_EXITING|ERTS_PSFLG_FREE)) {
        if (state & ERTS_PSFLG_FREE) {
            ASSERT(!locks);
            goto undefined;
        }
        if (locks)
            erts_proc_unlock(rp, locks);
        locks = 0;
        /* wait for it to terminate properly... */
        goto send_signal;
    }

    if (c_p == rp || !ERTS_PROC_HAS_INCOMING_SIGNALS(c_p))
        ERTS_BIF_PREP_RET(ret, res);
    else
        ERTS_BIF_PREP_HANDLE_SIGNALS_RETURN(ret, c_p, res);

done:

    if (c_p == rp)
        locks &= ~ERTS_PROC_LOCK_MAIN;

    if (locks && rp)
	erts_proc_unlock(rp, locks);

    if (item_ix != def_arr)
        erts_free(ERTS_ALC_T_TMP, item_ix);

    return ret;

badarg:
    ERTS_BIF_PREP_ERROR(ret, c_p, BADARG);
    goto done;

undefined:
    ERTS_BIF_PREP_RET(ret, am_undefined);
    goto done;

exited:
    ERTS_BIF_PREP_EXITED(ret, c_p);
    goto done;

yield:
    if (pi2)
        ERTS_BIF_PREP_YIELD2(ret, BIF_TRAP_EXPORT(BIF_process_info_2), c_p, pid, opt);
    else
        ERTS_BIF_PREP_YIELD1(ret, BIF_TRAP_EXPORT(BIF_process_info_1), c_p, pid);
    goto done;

send_signal: {
        Eterm ref = erts_make_ref(c_p);
        int enqueued, need_msgq_len;
        flags |= ERTS_PI_FLAG_REQUEST_FOR_OTHER;
        need_msgq_len = (flags & ERTS_PI_FLAG_NEED_MSGQ_LEN);
        /*
         * Set save pointer to the end of the message queue so we wont
         * have to scan the whole* message queue for the result. Note
         * that caller unconditionally has to enter a receive only
         * matching messages containing 'ref', or restore save pointer.
         */
        erts_msgq_set_save_end(c_p);
        enqueued = erts_proc_sig_send_process_info_request(c_p, pid, item_ix,
                                                           len, need_msgq_len,
                                                           flags, reserve_size,
                                                           ref);
        if (!enqueued) {
            /* Restore save pointer... */
	    erts_msgq_set_save_first(c_p);
            goto undefined;
        }
        ERTS_BIF_PREP_TRAP1(ret, erts_await_result, c_p, ref);
        goto done;
    }
}

static void
pi_setup_grow(int **arr, int *def_arr, Uint *sz, int ix)
{
    *sz = (ix+1) + ERTS_PI_DEF_ARR_SZ;
    if (*arr != def_arr)
        *arr = erts_realloc(ERTS_ALC_T_TMP, *arr, (*sz)*sizeof(int));
    else {
        int *new_arr = erts_alloc(ERTS_ALC_T_TMP, (*sz)*sizeof(int));
        sys_memcpy((void *) new_arr, (void *) def_arr,
                   sizeof(int)*ERTS_PI_DEF_ARR_SZ);
        *arr = new_arr;
    }
}


BIF_RETTYPE process_info_2(BIF_ALIST_2)
{
    return process_info_bif(BIF_P, BIF_ARG_1, BIF_ARG_2, !is_atom(BIF_ARG_2), !0);
}

BIF_RETTYPE process_info_1(BIF_ALIST_1)
{
    return process_info_bif(BIF_P, BIF_ARG_1, pi_1_keys_list, 0, 0);
}

Eterm
process_info_aux(Process *c_p,
                 ErtsHeapFactory *hfact,
		 Process *rp,
		 ErtsProcLocks rp_locks,
		 int item_ix,
		 int flags,
                 Uint *reserve_sizep,
                 Uint *reds)
{
    Eterm *hp;
    Eterm res = NIL;
    Uint reserved;
    Uint reserve_size = *reserve_sizep;

#ifdef ERTS_ENABLE_LOCK_CHECK
    ErtsProcLocks locks = erts_proc_lc_my_proc_locks(rp);

    switch (item_ix) {
    case ERTS_PI_IX_STATUS:
    case ERTS_PI_IX_PRIORITY:
    case ERTS_PI_IX_SUSPENDING:
        ERTS_LC_ASSERT((locks & ~ERTS_PROC_LOCK_MAIN) == 0);
        break;
    default:
        ERTS_LC_ASSERT(locks == ERTS_PROC_LOCK_MAIN);
        break;
    }
#endif

    reserved = pi_ix2rsz(item_ix);
    ERTS_PI_UNRESERVE(reserve_size, reserved);

    (*reds)++;

    ASSERT(rp);

    /*
     * Q: Why this ERTS_PI_FLAG_ALWAYS_WRAP flag?
     *
     * A: registered_name is strange. If process has no registered name,
     *    process_info(Pid, registered_name) returns [], and
     *    the result of process_info(Pid) has no {registered_name, Name}
     *    tuple in the resulting list. This is inconsistent with all other
     *    options, but we do not dare to change it.
     *
     *    When process_info/2 is called with a list as second argument,
     *    registered_name behaves as it should, i.e. a
     *    {registered_name, []} will appear in the resulting list.
     *
     *    If ERTS_PI_FLAG_ALWAYS_WRAP is set, process_info_aux() always
     *    wrap the result in a key two tuple. 
     */

    switch (item_ix) {

    case ERTS_PI_IX_REGISTERED_NAME:
	if (rp->common.u.alive.reg)
	    res = rp->common.u.alive.reg->name;
        else {
	    if (flags & ERTS_PI_FLAG_ALWAYS_WRAP)
		res = NIL;
	    else
		return NIL;
	}
	break;

    case ERTS_PI_IX_CURRENT_FUNCTION:
	res = current_function(c_p, hfact, rp, 0,
                               reserve_size, flags);
	break;

    case ERTS_PI_IX_CURRENT_LOCATION:
	res = current_function(c_p, hfact, rp, 1,
                               reserve_size, flags);
	break;

    case ERTS_PI_IX_CURRENT_STACKTRACE:
	res = current_stacktrace(c_p, hfact, rp, reserve_size, flags);
	break;

    case ERTS_PI_IX_INITIAL_CALL:
        hp = erts_produce_heap(hfact, 4, reserve_size);
	res = TUPLE3(hp,
		     rp->u.initial.module,
		     rp->u.initial.function,
		     make_small(rp->u.initial.arity));
	hp += 4;
	break;

    case ERTS_PI_IX_STATUS: {
        erts_aint32_t state = erts_atomic32_read_nob(&rp->state);
        res = erts_process_state2status(state);
        if (res == am_running && (state & ERTS_PSFLG_RUNNING_SYS)) {
            ASSERT(c_p == rp);
            ASSERT(flags & ERTS_PI_FLAG_REQUEST_FOR_OTHER);
            if (!(state & (ERTS_PSFLG_SYS_TASKS
                           | ERTS_PSFLG_ACTIVE
                           | ERTS_PSFLG_SIG_Q
                           | ERTS_PSFLG_SIG_IN_Q))) {
                /*
                 * We are servicing a process-info request from
                 * another process. If that other process could
                 * have inspected our state itself, we would have
                 * been in the 'waiting' state.
                 */
                res = am_waiting;
            }
        }
	break;
    }

    case ERTS_PI_IX_MESSAGES: {
        ASSERT(flags & ERTS_PI_FLAG_NEED_MSGQ_LEN);
	if (rp->sig_qs.len == 0 || (ERTS_TRACE_FLAGS(rp) & F_SENSITIVE))
            res = NIL;
        else {
            int info_on_self = !(flags & ERTS_PI_FLAG_REQUEST_FOR_OTHER);
	    ErtsMessageInfo *mip;
	    Sint i, len;
	    Uint heap_need;

	    mip = erts_alloc(ERTS_ALC_T_TMP,
			     rp->sig_qs.len*sizeof(ErtsMessageInfo));

	    /*
	     * Note that message queue may shrink when calling
	     * erts_proc_sig_prep_msgq_for_inspection() since it removes
	     * corrupt distribution messages.
	     */
	    heap_need = erts_proc_sig_prep_msgq_for_inspection(c_p, rp,
                                                               rp_locks,
                                                               info_on_self,
                                                               mip);
            len = rp->sig_qs.len;

	    heap_need += len*2; /* Cons cells */

            reserve_size += heap_need;

	    /* Build list of messages... */
	    for (i = len - 1, res = NIL; i >= 0; i--) {
		Eterm msg = ERL_MESSAGE_TERM(mip[i].msgp);
		Uint sz = mip[i].size;

                ERTS_PI_UNRESERVE(reserve_size, sz+2);
                hp = erts_produce_heap(hfact, sz+2, reserve_size);

		if (sz != 0)
		    msg = copy_struct(msg, sz, &hp, hfact->off_heap);

		res = CONS(hp, msg, res);
		hp += 2;
	    }

            *reds += (Uint) len / 4;

	    erts_free(ERTS_ALC_T_TMP, mip);
	}
	break;
    }

    case ERTS_PI_IX_MESSAGE_QUEUE_LEN: {
        Sint len = rp->sig_qs.len;
        ASSERT(flags & ERTS_PI_FLAG_NEED_MSGQ_LEN);
        ASSERT(len >= 0);
        if (len <= MAX_SMALL)
            res = make_small(len);
        else {
            hp = erts_produce_heap(hfact, BIG_UINT_HEAP_SIZE, reserve_size);
            res = uint_to_big((Uint) len, hp);
        }
	break;
    }

    case ERTS_PI_IX_LINKS: {
	MonitorInfoCollection mic;
	int i;
	Eterm item;

	INIT_MONITOR_INFOS(mic);

	erts_link_tree_foreach(ERTS_P_LINKS(rp), collect_one_link, (void *) &mic);

        reserve_size += mic.sz;
	res = NIL;
	for (i = 0; i < mic.mi_i; i++) {
            Eterm item_src = mic.mi[i].entity.term;
            Uint sz = NC_HEAP_SIZE(item_src) + 2;
            ERTS_PI_UNRESERVE(reserve_size, sz);
            hp = erts_produce_heap(hfact, sz, reserve_size);
	    item = STORE_NC(&hp, hfact->off_heap, item_src); 
	    res = CONS(hp, item, res);
	}

        *reds += (Uint) mic.mi_i / 4;

	DESTROY_MONITOR_INFOS(mic);
	break;
    }

    case ERTS_PI_IX_MONITORS: {
	MonitorInfoCollection mic;
        int i;

	INIT_MONITOR_INFOS(mic);
        erts_monitor_tree_foreach(ERTS_P_MONITORS(rp),
                                  collect_one_origin_monitor,
                                  (void *) &mic);

        reserve_size += mic.sz;
	res = NIL;
	for (i = 0; i < mic.mi_i; i++) {
	    if (mic.mi[i].named) {
		/* Monitor by name. 
                 * Build {process|port, {Name, Node}} and cons it.
		 */
		Eterm t1, t2;
                /* If pid is an atom, then it is a remote named monitor, which
                   has to be a process */
                Eterm m_type = is_port(mic.mi[i].pid) ? am_port : am_process;
                ASSERT(is_pid(mic.mi[i].pid)
                    || is_port(mic.mi[i].pid)
                    || is_atom(mic.mi[i].pid));

                ERTS_PI_UNRESERVE(reserve_size, 3+3+2);
                hp = erts_produce_heap(hfact, 3+3+2, reserve_size);

		t1 = TUPLE2(hp, mic.mi[i].entity.term, mic.mi[i].node);
		hp += 3;
                t2 = TUPLE2(hp, m_type, t1);
		hp += 3;
		res = CONS(hp, t2, res);
	    }
	    else {
                /* Build {process|port|time_offset, Pid|clock_service} and cons it. */
		Eterm t;
		Eterm pid;
                Eterm m_type;
                Eterm pid_src = mic.mi[i].entity.term;
                Uint sz = is_atom(pid_src) ? 0 : NC_HEAP_SIZE(pid_src);
                sz += 3 + 2;

                ERTS_PI_UNRESERVE(reserve_size, sz);
                hp = erts_produce_heap(hfact, sz, reserve_size);

                pid = (is_atom(pid_src)
                       ? pid_src
                       : STORE_NC(&hp, hfact->off_heap, pid_src));

                switch (mic.mi[i].type) {
                case ERTS_MON_TYPE_PORT:
                    m_type = am_port;
                    break;
                case ERTS_MON_TYPE_TIME_OFFSET:
                    m_type = am_time_offset;
                    break;
                default:
                    m_type = am_process;
                    break;
                }

                ASSERT(is_pid(mic.mi[i].pid)
                    || is_port(mic.mi[i].pid));

                t = TUPLE2(hp, m_type, pid);
		hp += 3;
		res = CONS(hp, t, res);
	    }
	}

        *reds += (Uint) mic.mi_i / 4;

        DESTROY_MONITOR_INFOS(mic);
	break;
    }

    case ERTS_PI_IX_MONITORED_BY: {
	MonitorInfoCollection mic;
	int i;
	Eterm item;

	INIT_MONITOR_INFOS(mic);
        erts_monitor_list_foreach(ERTS_P_LT_MONITORS(rp),
                                  collect_one_target_monitor,
                                  (void *) &mic);
        erts_monitor_tree_foreach(ERTS_P_MONITORS(rp),
                                  collect_one_target_monitor,
                                  (void *) &mic);

        reserve_size += mic.sz;

	res = NIL;
	for (i = 0; i < mic.mi_i; ++i) {
            Uint sz = 2;

            if (mic.mi[i].type == ERTS_MON_TYPE_RESOURCE)
                sz += erts_resource_ref_size(mic.mi[i].entity.resource);
            else
                sz += NC_HEAP_SIZE(mic.mi[i].entity.term);

            ERTS_PI_UNRESERVE(reserve_size, sz);
            hp = erts_produce_heap(hfact, sz, reserve_size);

            if (mic.mi[i].type == ERTS_MON_TYPE_RESOURCE)
                item = erts_bld_resource_ref(&hp,
                                             hfact->off_heap,
                                             mic.mi[i].entity.resource);
            else
                item = STORE_NC(&hp,
                                hfact->off_heap,
                                mic.mi[i].entity.term);
	    res = CONS(hp, item, res);
	}

        *reds += (Uint) mic.mi_i / 4;

	DESTROY_MONITOR_INFOS(mic);
	break;
    }

    case ERTS_PI_IX_SUSPENDING: {
	ErtsSuspendMonitorInfoCollection smic;
	int i;

	ERTS_INIT_SUSPEND_MONITOR_INFOS(smic);

        erts_monitor_tree_foreach(ERTS_P_MONITORS(rp),
                                  collect_one_suspend_monitor,
                                  (void *) &smic);

        reserve_size += smic.sz;

	res = NIL;
	for (i = 0; i < smic.smi_i; i++) {
            ErtsMonitorSuspend *msp;
            erts_aint_t mstate;
	    Sint ci;
            Eterm ct, active, pending, item;
            Uint sz = 4 + 2;

            msp = smic.smi[i];
            mstate = erts_atomic_read_nob(&msp->state);

            ci = (Sint) (mstate & ERTS_MSUSPEND_STATE_COUNTER_MASK);
            if (!IS_SSMALL(ci))
                sz += BIG_UINT_HEAP_SIZE;

            ERTS_PI_UNRESERVE(reserve_size, sz);
            hp = erts_produce_heap(hfact, sz, reserve_size);

            if (IS_SSMALL(ci))
                ct = make_small(ci);
            else {
                ct = small_to_big(ci, hp);
                hp += BIG_UINT_HEAP_SIZE;
            }

            if (mstate & ERTS_MSUSPEND_STATE_FLG_ACTIVE) {
                active = ct;
                pending = make_small(0);
            }
            else {
                active = make_small(0);
                pending = ct;
            }

            ASSERT(is_internal_pid(msp->md.origin.other.item));

	    item = TUPLE3(hp, msp->md.origin.other.item, active, pending);
	    hp += 4;
	    res = CONS(hp, item, res);
	}

        *reds += (Uint) smic.smi_i / 4;

	ERTS_DESTROY_SUSPEND_MONITOR_INFOS(smic);

	break;
    }

    case ERTS_PI_IX_DICTIONARY:
	if (!rp->dictionary || (ERTS_TRACE_FLAGS(rp) & F_SENSITIVE)) {
	    res = NIL;
	} else {
            Uint num = rp->dictionary->numElements;
	    res = erts_dictionary_copy(hfact, rp->dictionary, reserve_size);
            *reds += (Uint) num / 4;
	}

	break;

    case ERTS_PI_IX_TRAP_EXIT:
        res = (rp->flags & F_TRAP_EXIT) ? am_true : am_false;
	break;

    case ERTS_PI_IX_ERROR_HANDLER:
	res = erts_proc_get_error_handler(rp);
	break;

    case ERTS_PI_IX_HEAP_SIZE: {
	Uint hsz = 0;
	(void) erts_bld_uint(NULL, &hsz, HEAP_SIZE(rp));
        hp = erts_produce_heap(hfact, hsz, reserve_size);
	res = erts_bld_uint(&hp, NULL, HEAP_SIZE(rp));
	break;
    }

    case ERTS_PI_IX_FULLSWEEP_AFTER: {
	Uint hsz = 0;
	(void) erts_bld_uint(NULL, &hsz, MAX_GEN_GCS(rp));
        hp = erts_produce_heap(hfact, hsz, reserve_size);
	res = erts_bld_uint(&hp, NULL, MAX_GEN_GCS(rp));
	break;
    }

    case ERTS_PI_IX_MIN_HEAP_SIZE: {
	Uint hsz = 0;
	(void) erts_bld_uint(NULL, &hsz, MIN_HEAP_SIZE(rp));
        hp = erts_produce_heap(hfact, hsz, reserve_size);
	res = erts_bld_uint(&hp, NULL, MIN_HEAP_SIZE(rp));
	break;
    }

    case ERTS_PI_IX_MIN_BIN_VHEAP_SIZE: {
	Uint hsz = 0;
	(void) erts_bld_uint(NULL, &hsz, MIN_VHEAP_SIZE(rp));
        hp = erts_produce_heap(hfact, hsz, reserve_size);
	res = erts_bld_uint(&hp, NULL, MIN_VHEAP_SIZE(rp));
	break;
    }

    case ERTS_PI_IX_MAX_HEAP_SIZE: {
	Uint hsz = 0;
	(void) erts_max_heap_size_map(MAX_HEAP_SIZE_GET(rp),
                                      MAX_HEAP_SIZE_FLAGS_GET(rp),
                                      NULL, &hsz);
        hp = erts_produce_heap(hfact, hsz, reserve_size);
	res = erts_max_heap_size_map(MAX_HEAP_SIZE_GET(rp),
                                     MAX_HEAP_SIZE_FLAGS_GET(rp),
                                     &hp, NULL);
	break;
    }

    case ERTS_PI_IX_TOTAL_HEAP_SIZE: {
	Uint total_heap_size;
	Uint hsz = 0;

	total_heap_size = rp->heap_sz;
	if (rp->old_hend && rp->old_heap)
	    total_heap_size += rp->old_hend - rp->old_heap;

	total_heap_size += rp->mbuf_sz;

        if (rp->sig_qs.flags & FS_ON_HEAP_MSGQ) {
            ErtsMessage *mp;
            ASSERT(flags & ERTS_PI_FLAG_NEED_MSGQ_LEN);
            for (mp = rp->sig_qs.first; mp; mp = mp->next) {
		if (ERTS_SIG_IS_RECV_MARKER(mp))
		    continue;
                ASSERT(ERTS_SIG_IS_MSG(mp));
                if (mp->data.attached)
                    total_heap_size += erts_msg_attached_data_size(mp);
            }
            *reds += (Uint) rp->sig_qs.len / 4;
        }

	(void) erts_bld_uint(NULL, &hsz, total_heap_size);
        hp = erts_produce_heap(hfact, hsz, reserve_size);
	res = erts_bld_uint(&hp, NULL, total_heap_size);
	break;
    }

    case ERTS_PI_IX_STACK_SIZE: {
	Uint stack_size = STACK_START(rp) - rp->stop;
	Uint hsz = 0;
	(void) erts_bld_uint(NULL, &hsz, stack_size);
        hp = erts_produce_heap(hfact, hsz, reserve_size);
	res = erts_bld_uint(&hp, NULL, stack_size);
	break;
    }

    case ERTS_PI_IX_MEMORY: { /* Memory consumed in bytes */
	Uint hsz = 0;
	Uint size = erts_process_memory(rp, 0);
	(void) erts_bld_uint(NULL, &hsz, size);
        hp = erts_produce_heap(hfact, hsz, reserve_size);
	res = erts_bld_uint(&hp, NULL, size);

        ASSERT(flags & ERTS_PI_FLAG_NEED_MSGQ_LEN);
        *reds += (Uint) rp->sig_qs.len / 4;

	break;
    }

    case ERTS_PI_IX_GARBAGE_COLLECTION: {
        DECL_AM(minor_gcs);
        Eterm t;
        Uint map_sz = 0;

        erts_max_heap_size_map(MAX_HEAP_SIZE_GET(rp), MAX_HEAP_SIZE_FLAGS_GET(rp), NULL, &map_sz);

        hp = erts_produce_heap(hfact, 3+2 + 3+2 + 3+2 + 3+2 + 3+2 + map_sz, reserve_size);

	t = TUPLE2(hp, AM_minor_gcs, make_small(GEN_GCS(rp))); hp += 3;
	res = CONS(hp, t, NIL); hp += 2;
	t = TUPLE2(hp, am_fullsweep_after, make_small(MAX_GEN_GCS(rp))); hp += 3;
	res = CONS(hp, t, res); hp += 2;

	t = TUPLE2(hp, am_min_heap_size, make_small(MIN_HEAP_SIZE(rp))); hp += 3;
	res = CONS(hp, t, res); hp += 2;
	t = TUPLE2(hp, am_min_bin_vheap_size, make_small(MIN_VHEAP_SIZE(rp))); hp += 3;
	res = CONS(hp, t, res); hp += 2;

        t = erts_max_heap_size_map(MAX_HEAP_SIZE_GET(rp), MAX_HEAP_SIZE_FLAGS_GET(rp), &hp, NULL);

	t = TUPLE2(hp, am_max_heap_size, t); hp += 3;
	res = CONS(hp, t, res); hp += 2;
	break;
    }

    case ERTS_PI_IX_GARBAGE_COLLECTION_INFO: {
        Uint sz = 0, actual_sz = 0;

        erts_process_gc_info(rp, &sz, NULL, 0, 0);

        hp = erts_produce_heap(hfact, sz, reserve_size);
        res = erts_process_gc_info(rp, &actual_sz, &hp, 0, 0);

        break;
    }

    case ERTS_PI_IX_GROUP_LEADER: {
	int sz = NC_HEAP_SIZE(rp->group_leader);
        hp = erts_produce_heap(hfact, sz, reserve_size);
	res = STORE_NC(&hp, hfact->off_heap, rp->group_leader);
	break;
    }

    case ERTS_PI_IX_REDUCTIONS: {
	Uint reds = rp->reds + erts_current_reductions(c_p, rp);
	Uint hsz = 0;
	(void) erts_bld_uint(NULL, &hsz, reds);
        hp = erts_produce_heap(hfact, hsz, reserve_size);
	res = erts_bld_uint(&hp, NULL, reds);
	break;
    }

    case ERTS_PI_IX_PRIORITY: {
        erts_aint32_t state = erts_atomic32_read_nob(&rp->state);
        if (ERTS_PSFLG_EXITING & state)
            return am_undefined;
	res = erts_get_process_priority(state);
	break;
    }

    case ERTS_PI_IX_TRACE:
	res = make_small(ERTS_TRACE_FLAGS(rp) & TRACEE_FLAGS);
	break;

    case ERTS_PI_IX_BINARY: {
        ErlHeapFragment *hfrag;
        Uint sz;

        res = NIL;
        sz = 0;

        (void)erts_bld_bin_list(NULL, &sz, &MSO(rp), NIL);
        for (hfrag = rp->mbuf; hfrag != NULL; hfrag = hfrag->next) {
            (void)erts_bld_bin_list(NULL, &sz, &hfrag->off_heap, NIL);
        }

        hp = erts_produce_heap(hfact, sz, reserve_size);

        res = erts_bld_bin_list(&hp, NULL, &MSO(rp), NIL);
        for (hfrag = rp->mbuf; hfrag != NULL; hfrag = hfrag->next) {
            res = erts_bld_bin_list(&hp, NULL, &hfrag->off_heap, res);
        }

        break;
    }

    case ERTS_PI_IX_SEQUENTIAL_TRACE_TOKEN: {
        Uint sz = size_object(rp->seq_trace_token);
        hp = erts_produce_heap(hfact, sz, reserve_size);
        res = copy_struct(rp->seq_trace_token, sz, &hp, hfact->off_heap);
	break;
    }

    case ERTS_PI_IX_CATCHLEVEL:
	res = make_small(catchlevel(rp));
	break;

    case ERTS_PI_IX_BACKTRACE: {
	erts_dsprintf_buf_t *dsbufp = erts_create_tmp_dsbuf(0);
	erts_stack_dump(ERTS_PRINT_DSBUF, (void *) dsbufp, rp);
	res = erts_heap_factory_new_binary(hfact, (byte *) dsbufp->str,
                                           dsbufp->str_len, reserve_size);
	erts_destroy_tmp_dsbuf(dsbufp);
	break;
    }

    case ERTS_PI_IX_LAST_CALLS: {
	struct saved_calls *scb = ERTS_PROC_GET_SAVED_CALLS_BUF(rp);
	if (!scb) {
	    res = am_false;
	} else {
	    /*
	     * One cons cell and a 3-struct, and a 2-tuple.
	     * Might be less than that, if there are sends, receives or timeouts,
	     * so we must do a HRelease() to avoid creating holes.
	     */
	    Sint needed = scb->n*(2+4);
	    Eterm term, list;
	    int i, j;
            Export *exp;

            reserve_size += needed;

	    list = NIL;
	    for (i = 0; i < scb->n; i++) {
                Uint sz;
		j = scb->cur - i - 1;
		if (j < 0)
		    j += scb->len;

                sz = 2;
                exp = scb->ct[j];
                if (exp != &exp_send && exp != &exp_receive && exp != &exp_timeout)
                    sz += 4;

                needed -= sz;
                ERTS_PI_UNRESERVE(reserve_size, sz);
                hp = erts_produce_heap(hfact, sz, reserve_size);

		if (exp == &exp_send)
		    term = am_send;
		else if (exp == &exp_receive)
		    term = am_receive;
		else if (exp == &exp_timeout)
		    term = am_timeout;
		else {
		    term = TUPLE3(hp,
				  scb->ct[j]->info.mfa.module,
				  scb->ct[j]->info.mfa.function,
				  make_small(scb->ct[j]->info.mfa.arity));
		    hp += 4;
		}
		list = CONS(hp, term, list);
	    }

            ASSERT(needed >= 0);
            if (needed > 0)
                reserve_size -= needed;

	    res = list;
	}
	break;
    }

    case ERTS_PI_IX_MESSAGE_QUEUE_DATA:
	switch (rp->sig_qs.flags & (FS_OFF_HEAP_MSGQ|FS_ON_HEAP_MSGQ)) {
	case FS_OFF_HEAP_MSGQ:
	    res = am_off_heap;
	    break;
	case FS_ON_HEAP_MSGQ:
	    res = am_on_heap;
	    break;
	default:
	    res = am_error;
	    ERTS_INTERNAL_ERROR("Inconsistent message queue management state");
	    break;
	}
	break;

    case ERTS_PI_IX_MAGIC_REF: {
	Uint sz = 0;
	(void) bld_magic_ref_bin_list(NULL, &sz, &MSO(rp));
        hp = erts_produce_heap(hfact, sz, 0);
	res = bld_magic_ref_bin_list(&hp, NULL, &MSO(rp));

        *reds += (Uint) 10;
	break;
    }

    default:
	return THE_NON_VALUE; /* will produce badarg */

    }

    ERTS_PI_UNRESERVE(reserve_size, 3);
    *reserve_sizep = reserve_size;
    hp = erts_produce_heap(hfact, 3, reserve_size);

    return TUPLE2(hp, pi_ix2arg(item_ix), res);
}
#undef MI_INC

static Eterm
current_function(Process *c_p, ErtsHeapFactory *hfact, Process* rp,
                 int full_info, Uint reserve_size, int flags)
{
    Eterm* hp;
    Eterm res;
    FunctionInfo fi;

    if (rp->current == NULL) {
	erts_lookup_function_info(&fi, rp->i, full_info);
	rp->current = fi.mfa;
    } else if (full_info) {
	erts_lookup_function_info(&fi, rp->i, full_info);
	if (fi.mfa == NULL) {
	    /* Use the current function without location info */
	    erts_set_current_function(&fi, rp->current);
	}
    }

    if (c_p == rp && !(flags & ERTS_PI_FLAG_REQUEST_FOR_OTHER)) {
        ErtsCodePtr return_address;
        FunctionInfo caller_fi;

        /*
         * The current function is erlang:process_info/{1,2}, and we've
         * historically returned the *calling* function in that case. We
         * therefore use the continuation pointer stored at the top of the
         * stack instead, which is safe since process_info is a "heavy" BIF
         * that is only called through its export entry.
         */
        return_address = erts_printable_return_address(rp, STACK_TOP(rp));

        erts_lookup_function_info(&caller_fi, return_address, full_info);
        if (caller_fi.mfa) {
            fi = caller_fi;
            rp->current = caller_fi.mfa;
        }
    }

    /*
     * Return the result.
     */
    if (rp->current == NULL) {
	res = am_undefined;
    } else if (full_info) {
        hp = erts_produce_heap(hfact, fi.needed, reserve_size);
        erts_build_mfa_item(&fi, hp, am_true, &res, NIL);
    } else {
        hp = erts_produce_heap(hfact, 4, reserve_size);
	res = TUPLE3(hp, rp->current->module,
		     rp->current->function,
                     make_small(rp->current->arity));
    }
    return res;
}

static Eterm
current_stacktrace(Process *p, ErtsHeapFactory *hfact, Process* rp,
                   Uint reserve_size, int flags)
{
    Uint sz;
    struct StackTrace* s;
    int depth;
    FunctionInfo* stk;
    FunctionInfo* stkp;
    Uint heap_size;
    int i;
    Eterm* hp;
    Eterm mfa;
    Eterm res = NIL;

    depth = erts_backtrace_depth;
    sz = offsetof(struct StackTrace, trace) + sizeof(ErtsCodePtr) * depth;
    s = (struct StackTrace *) erts_alloc(ERTS_ALC_T_TMP, sz);
    s->depth = 0;
    s->pc = NULL;

    /* We skip current pc when requesting our own stack trace since it will
     * inevitably point to process_info/1,2 */
    if ((p != rp || (flags & ERTS_PI_FLAG_REQUEST_FOR_OTHER)) &&
        depth > 0 && rp->i) {
        s->trace[s->depth++] = rp->i;
        depth--;
    }
    erts_save_stacktrace(rp, s, depth);

    depth = s->depth;
    stk = stkp = (FunctionInfo *) erts_alloc(ERTS_ALC_T_TMP,
					     depth*sizeof(FunctionInfo));
    heap_size = 3;
    for (i = 0; i < depth; i++) {
	erts_lookup_function_info(stkp, s->trace[i], 1);
	if (stkp->mfa) {
	    heap_size += stkp->needed + 2;
	    stkp++;
	}
    }

    reserve_size += heap_size;

    /*
     * We intentionally produce heap in small chunks
     * (for more info see process_info_aux()).
     */
    while (stkp > stk) {
	stkp--;
        sz = stkp->needed + 2;
        ERTS_PI_UNRESERVE(reserve_size, sz);
        hp = erts_produce_heap(hfact, sz, reserve_size);
        hp = erts_build_mfa_item(stkp, hp, am_true, &mfa, NIL);
	res = CONS(hp, mfa, res);
    }

    erts_free(ERTS_ALC_T_TMP, stk);
    erts_free(ERTS_ALC_T_TMP, s);
    return res;
}

#if defined(VALGRIND) || defined(ADDRESS_SANITIZER)
static int iolist_to_tmp_buf(Eterm iolist, char** bufp)
{
    ErlDrvSizeT buf_size = 1024; /* Try with 1KB first */
    char *buf = erts_alloc(ERTS_ALC_T_TMP, buf_size);
    ErlDrvSizeT r = erts_iolist_to_buf(iolist, (char*) buf, buf_size - 1);
    if (ERTS_IOLIST_TO_BUF_FAILED(r)) {
        erts_free(ERTS_ALC_T_TMP, (void *) buf);
        if (erts_iolist_size(iolist, &buf_size)) {
            return 0;
        }
        buf_size++;
        buf = erts_alloc(ERTS_ALC_T_TMP, buf_size);
        r = erts_iolist_to_buf(iolist, (char*) buf, buf_size - 1);
        ASSERT(r == buf_size - 1);
    }
    buf[buf_size - 1 - r] = '\0';
    *bufp = buf;
    return 1;
}
#endif

/*
 * This function takes care of calls to erlang:system_info/1 when the argument
 * is a tuple.
 */
static BIF_RETTYPE
info_1_tuple(Process* BIF_P,	/* Pointer to current process. */
	     Eterm* tp,		/* Pointer to first element in tuple */
	     int arity)		/* Arity of tuple (untagged). */
{
    Eterm ret;
    Eterm sel;

    sel = *tp++;

    if (sel == am_memory_internal) {
	switch (arity) {
	case 3:
	    if (erts_request_alloc_info(BIF_P, tp[0], tp[1], 1, 1))
		return am_true;
	default:
	    goto badarg;
	}
    }
    else if (sel == am_allocator_sizes) {
	switch (arity) {
	case 2:
	    ERTS_BIF_PREP_TRAP1(ret, alloc_sizes_trap, BIF_P, *tp);
	    return ret;
	case 3:
	    if (erts_request_alloc_info(BIF_P, tp[0], tp[1], 1, 0))
		return am_true;
	default:
	    goto badarg;
	}
    }
    else if (sel == am_wordsize && arity == 2) {
	if (tp[0] == am_internal) {
	    return make_small(sizeof(Eterm));
	}
	if (tp[0] == am_external) {
	    return make_small(sizeof(UWord));
	}
	goto badarg;
    } else if (sel == am_allocator) {
	switch (arity) {
	case 2:
	    ERTS_BIF_PREP_TRAP1(ret, alloc_info_trap, BIF_P, *tp);
	    return ret;
	case 3:
	    if (erts_request_alloc_info(BIF_P, tp[0], tp[1], 0, 0))
		return am_true;
	default:
	    goto badarg;
	}
    } else if (ERTS_IS_ATOM_STR("internal_cpu_topology", sel) && arity == 2) {
	return erts_get_cpu_topology_term(BIF_P, *tp);
    } else if (ERTS_IS_ATOM_STR("cpu_topology", sel) && arity == 2) {
	Eterm res = erts_get_cpu_topology_term(BIF_P, *tp);
	if (res == THE_NON_VALUE)
	    goto badarg;
	ERTS_BIF_PREP_TRAP1(ret, erts_format_cpu_topology_trap, BIF_P, res);
	return ret;
    } else if (ERTS_IS_ATOM_STR("memory_checker", sel)) {
        if (arity == 2 && ERTS_IS_ATOM_STR("test_leak", *tp)) {
#if defined(VALGRIND) || defined(ADDRESS_SANITIZER)
            erts_alloc(ERTS_ALC_T_HEAP , 100);
#endif
            BIF_RET(am_ok);
        }
        else if (arity == 2 && ERTS_IS_ATOM_STR("test_overflow", *tp)) {
            static int test[2];
            BIF_RET(make_small(test[2]));
        }
#if defined(VALGRIND) || defined(ADDRESS_SANITIZER)
	if (arity == 2 && *tp == am_running) {
#  if defined(VALGRIND)
	    if (RUNNING_ON_VALGRIND)
		BIF_RET(ERTS_MAKE_AM("valgrind"));
#  elif defined(ADDRESS_SANITIZER)
	    BIF_RET(ERTS_MAKE_AM("asan"));
#  endif
	}
	else if (arity == 2 && ERTS_IS_ATOM_STR("check_leaks", *tp)) {
#  if defined(VALGRIND)
#    ifdef VALGRIND_DO_ADDED_LEAK_CHECK
	    VALGRIND_DO_ADDED_LEAK_CHECK;
#    else
	    VALGRIND_DO_LEAK_CHECK;
#    endif
	    BIF_RET(am_ok);
#  elif defined(ADDRESS_SANITIZER)
	    __lsan_do_recoverable_leak_check();
	    BIF_RET(am_ok);
#  endif
        }
#  if defined(VALGRIND)
	if (arity == 3 && tp[0] == am_print && is_list(tp[1])) {
            char* buf;
            if (!iolist_to_tmp_buf(tp[1], &buf))
                goto badarg;
            VALGRIND_PRINTF("%s\n", buf);
	    erts_free(ERTS_ALC_T_TMP, (void *) buf);
	    BIF_RET(am_true);
	}
#  endif
#  if defined(ADDRESS_SANITIZER)
        if (arity == 3 && ERTS_IS_ATOM_STR("log",tp[0]) && is_list(tp[1])) {
            static char *active_log = NULL;
            static int active_log_len;
            Eterm ret = NIL;
            char* buf;
            if (!iolist_to_tmp_buf(tp[1], &buf))
                goto badarg;
            erts_rwmtx_rwlock(&erts_dist_table_rwmtx); /* random lock abuse */
            __sanitizer_set_report_path(buf);
            if (active_log) {
                Eterm *hp = HAlloc(BIF_P, 2 * active_log_len);
                ret = erts_bld_string_n(&hp, 0, active_log, active_log_len);
                erts_free(ERTS_ALC_T_DEBUG, active_log);
            }
            active_log_len = sys_strlen(buf);
            active_log = erts_alloc(ERTS_ALC_T_DEBUG, active_log_len + 1);
            sys_memcpy(active_log, buf, active_log_len + 1);
            erts_rwmtx_rwunlock(&erts_dist_table_rwmtx);
            erts_free(ERTS_ALC_T_TMP, (void *) buf);
            BIF_RET(ret);
        }
#  endif
#endif
#if defined(__GNUC__) && defined(HAVE_SOLARIS_SPARC_PERFMON)
    } else if (ERTS_IS_ATOM_STR("ultrasparc_set_pcr", sel)) {
	unsigned long long tmp;
	int fd;
	int rc;

	if (arity != 2 || !is_small(*tp)) {
	    goto badarg;
	}
	tmp = signed_val(*tp);
	if ((fd = open("/dev/perfmon", O_RDONLY)) == -1) {
	    BIF_RET(am_false);
	}
	rc = ioctl(fd, PERFMON_SETPCR, &tmp);
	close(fd);
	if (rc < 0) {
	    BIF_RET(am_false);
	}
	BIF_RET(am_true);
#endif
    }

 badarg:
    ERTS_BIF_PREP_ERROR(ret, BIF_P, BADARG);

    return ret;
}

#define INFO_DSBUF_INC_SZ 256

static erts_dsprintf_buf_t *
grow_info_dsbuf(erts_dsprintf_buf_t *dsbufp, size_t need)
{
    size_t size;
    size_t free_size = dsbufp->size - dsbufp->str_len;

    ASSERT(dsbufp);

    if (need <= free_size)
	return dsbufp;
    size = need - free_size + INFO_DSBUF_INC_SZ;
    size = ((size + INFO_DSBUF_INC_SZ - 1)/INFO_DSBUF_INC_SZ)*INFO_DSBUF_INC_SZ;
    size += dsbufp->size;
    ASSERT(dsbufp->str_len + need <= size);
    dsbufp->str = (char *) erts_realloc(ERTS_ALC_T_INFO_DSBUF,
					(void *) dsbufp->str,
					size);
    dsbufp->size = size;
    return dsbufp;
}

static erts_dsprintf_buf_t *
erts_create_info_dsbuf(Uint size)
{
    Uint init_size = size ? size : INFO_DSBUF_INC_SZ;
    erts_dsprintf_buf_t init = ERTS_DSPRINTF_BUF_INITER(grow_info_dsbuf);
    erts_dsprintf_buf_t *dsbufp = erts_alloc(ERTS_ALC_T_INFO_DSBUF,
					     sizeof(erts_dsprintf_buf_t));
    sys_memcpy((void *) dsbufp, (void *) &init, sizeof(erts_dsprintf_buf_t));
    dsbufp->str = (char *) erts_alloc(ERTS_ALC_T_INFO_DSBUF, init_size);
    dsbufp->str[0] = '\0';
    dsbufp->size = init_size;
    return dsbufp;
}

static void
erts_destroy_info_dsbuf(erts_dsprintf_buf_t *dsbufp)
{
    if (dsbufp->str)
	erts_free(ERTS_ALC_T_INFO_DSBUF, (void *) dsbufp->str);
    erts_free(ERTS_ALC_T_INFO_DSBUF, (void *) dsbufp);
}

static Eterm
c_compiler_used(Eterm **hpp, Uint *szp)
{

#if defined(__GNUC__)
#  if defined(__GNUC_MINOR__) && defined(__GNUC_PATCHLEVEL__)
#    define ERTS_GNUC_VSN_NUMS 3
#  elif defined(__GNUC_MINOR__)
#    define ERTS_GNUC_VSN_NUMS 2
#  else
#    define ERTS_GNUC_VSN_NUMS 1
#  endif
    return erts_bld_tuple(hpp,
			  szp,
			  2,
			  erts_bld_atom(hpp, szp, "gnuc"),
#if ERTS_GNUC_VSN_NUMS > 1
			  erts_bld_tuple(hpp,
					 szp,
					 ERTS_GNUC_VSN_NUMS,
#endif
					 erts_bld_uint(hpp, szp,
						       (Uint) __GNUC__)
#ifdef __GNUC_MINOR__
					 ,
					 erts_bld_uint(hpp, szp,
						       (Uint) __GNUC_MINOR__)
#ifdef __GNUC_PATCHLEVEL__
					 ,
					 erts_bld_uint(hpp, szp,
						       (Uint) __GNUC_PATCHLEVEL__)
#endif
#endif
#if ERTS_GNUC_VSN_NUMS > 1
			     )
#endif
	);

#elif defined(_MSC_VER)
    return erts_bld_tuple(hpp,
			  szp,
			  2,
			  erts_bld_atom(hpp, szp, "msc"),
			  erts_bld_uint(hpp, szp, (Uint) _MSC_VER));

#else
    return erts_bld_tuple(hpp,
			  szp,
			  2,
			  am_undefined,
			  am_undefined);
#endif

}

static int is_snif_term(Eterm module_atom) {
    int i;
    Atom *a = atom_tab(atom_val(module_atom));
    char *aname = (char *) a->name;

    /* if a->name has a '.' then the bif (snif) is bogus i.e a package */
    for (i = 0; i < a->len; i++) {
	if (aname[i] == '.')
	    return 0;
    }

    return 1;
}

static Eterm build_snif_term(Eterm **hpp, Uint *szp, int ix, Eterm res) {
    Eterm tup;
    tup = erts_bld_tuple(hpp, szp, 3, bif_table[ix].module, bif_table[ix].name, make_small(bif_table[ix].arity));
    res = erts_bld_cons( hpp, szp, tup, res);
    return res;
}

static Eterm build_snifs_term(Eterm **hpp, Uint *szp, Eterm res) {
    int i;
    for (i = 0; i < BIF_SIZE; i++) {
	if (is_snif_term(bif_table[i].module)) {
	    res = build_snif_term(hpp, szp, i, res);
	}
    }
    return res;
}

BIF_RETTYPE system_info_1(BIF_ALIST_1)
{
    Eterm res;
    Eterm* hp;
    Eterm val;
    int i;

    if (is_tuple(BIF_ARG_1)) {
	Eterm* tp = tuple_val(BIF_ARG_1);
	Uint arity = *tp++;
	return info_1_tuple(BIF_P, tp, arityval(arity));
    } else if (BIF_ARG_1 == am_scheduler_id) {
	ErtsSchedulerData *esdp = erts_proc_sched_data(BIF_P);
	BIF_RET(make_small(esdp->no));
    } else if (BIF_ARG_1 == am_compat_rel) {
	ASSERT(erts_compat_rel > 0);
	BIF_RET(make_small(erts_compat_rel));
    } else if (BIF_ARG_1 == am_multi_scheduling) {
	{
	    int msb = erts_is_multi_scheduling_blocked();
	    BIF_RET(!msb
		    ? am_enabled
		    : (msb > 0
		       ? am_blocked
		       : am_blocked_normal));
	}
    } else if (BIF_ARG_1 == am_build_type || BIF_ARG_1 == am_emu_type) {
#if defined(DEBUG)
	ERTS_DECL_AM(debug);
	BIF_RET(AM_debug);
#elif defined(ERTS_GCOV)
	ERTS_DECL_AM(gcov);
	BIF_RET(AM_gcov);
#elif defined(VALGRIND)
	ERTS_DECL_AM(valgrind);
	BIF_RET(AM_valgrind);
#elif defined(ADDRESS_SANITIZER)
	ERTS_DECL_AM(asan);
	BIF_RET(AM_asan);
#elif defined(GPROF)
	ERTS_DECL_AM(gprof);
	BIF_RET(AM_gprof);
#elif defined(ERTS_ENABLE_LOCK_COUNT)
	ERTS_DECL_AM(lcnt);
	BIF_RET(AM_lcnt);
#elif defined(ERTS_FRMPTR)
	ERTS_DECL_AM(frmptr);
	BIF_RET(AM_frmptr);
#else
	BIF_RET(am_opt);
#endif
    } else if (BIF_ARG_1 == am_emu_flavor) {
#if defined(BEAMASM)
	ERTS_DECL_AM(jit);
	BIF_RET(AM_jit);
#else
        ERTS_DECL_AM(emu);
	BIF_RET(AM_emu);
#endif
    } else if (BIF_ARG_1 == am_time_offset) {
	switch (erts_time_offset_state()) {
	case ERTS_TIME_OFFSET_PRELIMINARY: {
	    ERTS_DECL_AM(preliminary);
	    BIF_RET(AM_preliminary);
	}
	case ERTS_TIME_OFFSET_FINAL: {
	    ERTS_DECL_AM(final);
	    BIF_RET(AM_final);
	}
	case ERTS_TIME_OFFSET_VOLATILE: {
	    ERTS_DECL_AM(volatile);
	    BIF_RET(AM_volatile);
	}
	default:
	    ERTS_INTERNAL_ERROR("Invalid time offset state");
	}
    } else if (ERTS_IS_ATOM_STR("os_monotonic_time_source", BIF_ARG_1)) {
	BIF_RET(erts_monotonic_time_source(BIF_P));
    } else if (ERTS_IS_ATOM_STR("os_system_time_source", BIF_ARG_1)) {
	BIF_RET(erts_system_time_source(BIF_P));
    } else if (ERTS_IS_ATOM_STR("time_correction", BIF_ARG_1)) {
	BIF_RET(erts_has_time_correction() ? am_true : am_false);
    } else if (ERTS_IS_ATOM_STR("start_time", BIF_ARG_1)) {
	BIF_RET(erts_get_monotonic_start_time(BIF_P));
    } else if (ERTS_IS_ATOM_STR("end_time", BIF_ARG_1)) {
	BIF_RET(erts_get_monotonic_end_time(BIF_P));
    } else if (ERTS_IS_ATOM_STR("time_warp_mode", BIF_ARG_1)) {
	switch (erts_time_warp_mode()) {
	case ERTS_NO_TIME_WARP_MODE: {
	    ERTS_DECL_AM(no_time_warp);
	    BIF_RET(AM_no_time_warp);
	}
	case ERTS_SINGLE_TIME_WARP_MODE: {
	    ERTS_DECL_AM(single_time_warp);
	    BIF_RET(AM_single_time_warp);
	}
	case ERTS_MULTI_TIME_WARP_MODE: {
	    ERTS_DECL_AM(multi_time_warp);
	    BIF_RET(AM_multi_time_warp);
	}
	default:
	    ERTS_INTERNAL_ERROR("Invalid time warp mode");
	}
    } else if (BIF_ARG_1 == am_allocated_areas) {
	res = erts_allocated_areas(NULL, NULL, BIF_P);
	BIF_RET(res);
    } else if (ERTS_IS_ATOM_STR("hipe_architecture", BIF_ARG_1)) {
	BIF_RET(am_undefined);
    } else if (BIF_ARG_1 == am_trace_control_word) {
	BIF_RET(db_get_trace_control_word(BIF_P));
    } else if (ERTS_IS_ATOM_STR("ets_realloc_moves", BIF_ARG_1)) {
 	BIF_RET((erts_ets_realloc_always_moves) ? am_true : am_false);
    } else if (ERTS_IS_ATOM_STR("ets_always_compress", BIF_ARG_1)) {
	BIF_RET((erts_ets_always_compress) ? am_true : am_false);
    } else if (ERTS_IS_ATOM_STR("snifs", BIF_ARG_1)) {
	Uint size = 0;
	Uint *szp;

	szp = &size;
	build_snifs_term(NULL, szp, NIL);
	hp = HAlloc(BIF_P, size);
	res = build_snifs_term(&hp, NULL, NIL);
	BIF_RET(res);
    } else if (BIF_ARG_1 == am_sequential_tracer) {
	ErtsTracer seq_tracer = erts_get_system_seq_tracer();
        val = erts_tracer_to_term(BIF_P, seq_tracer);
	hp = HAlloc(BIF_P, 3);
	res = TUPLE2(hp, am_sequential_tracer, val);
	BIF_RET(res);
    } else if (BIF_ARG_1 == am_garbage_collection){
	Uint val = (Uint) erts_atomic32_read_nob(&erts_max_gen_gcs);
	Eterm tup;
	hp = HAlloc(BIF_P, 3+2 + 3+2 + 3+2 + 3+2);

	tup = TUPLE2(hp, am_fullsweep_after, make_small(val)); hp += 3;
	res = CONS(hp, tup, NIL); hp += 2;

	tup = TUPLE2(hp, am_min_heap_size, make_small(H_MIN_SIZE)); hp += 3;
	res = CONS(hp, tup, res); hp += 2;

	tup = TUPLE2(hp, am_min_bin_vheap_size, make_small(BIN_VH_MIN_SIZE)); hp += 3;
	res = CONS(hp, tup, res); hp += 2;

	tup = TUPLE2(hp, am_max_heap_size, make_small(H_MAX_SIZE)); hp += 3;
	res = CONS(hp, tup, res); hp += 2;

	BIF_RET(res);
    } else if (BIF_ARG_1 == am_fullsweep_after){
	Uint val = (Uint) erts_atomic32_read_nob(&erts_max_gen_gcs);
	hp = HAlloc(BIF_P, 3);
	res = TUPLE2(hp, am_fullsweep_after, make_small(val));
	BIF_RET(res);
    } else if (BIF_ARG_1 == am_min_heap_size) {
	hp = HAlloc(BIF_P, 3);
	res = TUPLE2(hp, am_min_heap_size,make_small(H_MIN_SIZE));
	BIF_RET(res);
    } else if (BIF_ARG_1 == am_max_heap_size) {
        Uint sz = 0;
        erts_max_heap_size_map(H_MAX_SIZE, H_MAX_FLAGS, NULL, &sz);
	hp = HAlloc(BIF_P, sz);
	res = erts_max_heap_size_map(H_MAX_SIZE, H_MAX_FLAGS, &hp, NULL);
	BIF_RET(res);
    } else if (BIF_ARG_1 == am_min_bin_vheap_size) {
	hp = HAlloc(BIF_P, 3);
	res = TUPLE2(hp, am_min_bin_vheap_size,make_small(BIN_VH_MIN_SIZE));
	BIF_RET(res);
    } else if (BIF_ARG_1 == am_process_count) {
	BIF_RET(make_small(erts_ptab_count(&erts_proc)));
    } else if (BIF_ARG_1 == am_process_limit) {
	BIF_RET(make_small(erts_ptab_max(&erts_proc)));
    } else if (BIF_ARG_1 == am_port_count) {
	BIF_RET(make_small(erts_ptab_count(&erts_port)));
    } else if (BIF_ARG_1 == am_port_limit) {
	BIF_RET(make_small(erts_ptab_max(&erts_port)));
    } else if (BIF_ARG_1 == am_info
	       || BIF_ARG_1 == am_procs
	       || BIF_ARG_1 == am_loaded
	       || BIF_ARG_1 == am_dist) {
	erts_dsprintf_buf_t *dsbufp = erts_create_info_dsbuf(0);

	/* Need to be the only thread running... */
	erts_proc_unlock(BIF_P, ERTS_PROC_LOCK_MAIN);
        BIF_P->scheduler_data->current_process = NULL;
	erts_thr_progress_block();

	if (BIF_ARG_1 == am_info)
	    info(ERTS_PRINT_DSBUF, (void *) dsbufp);
	else if (BIF_ARG_1 == am_procs)
	    process_info(ERTS_PRINT_DSBUF, (void *) dsbufp);
	else if (BIF_ARG_1 == am_loaded)
	    loaded(ERTS_PRINT_DSBUF, (void *) dsbufp);
	else
	    distribution_info(ERTS_PRINT_DSBUF, (void *) dsbufp);

	erts_thr_progress_unblock();
	erts_proc_lock(BIF_P, ERTS_PROC_LOCK_MAIN);
       BIF_P->scheduler_data->current_process = BIF_P;

	ASSERT(dsbufp && dsbufp->str);
	res = new_binary(BIF_P, (byte *) dsbufp->str, dsbufp->str_len);
	erts_destroy_info_dsbuf(dsbufp);
	BIF_RET(res);
    } else if (ERTS_IS_ATOM_STR("dist_ctrl", BIF_ARG_1)) {
	DistEntry *dep;
	i = 0;
        erts_rwmtx_rlock(&erts_dist_table_rwmtx);
	for (dep = erts_visible_dist_entries; dep; dep = dep->next) 
	    ++i;
	for (dep = erts_hidden_dist_entries; dep; dep = dep->next)
	    ++i;
	hp = HAlloc(BIF_P,i*(3+2));
	res = NIL;
	for (dep = erts_hidden_dist_entries; dep; dep = dep->next) {
	    Eterm tpl;
	    ASSERT(is_immed(dep->cid));
	    tpl = TUPLE2(hp, dep->sysname, dep->cid);
	    hp +=3;
	    res = CONS(hp, tpl, res);
	    hp += 2;
	}
	for (dep = erts_visible_dist_entries; dep; dep = dep->next) {
	    Eterm tpl;
	    ASSERT(is_immed(dep->cid));
	    tpl = TUPLE2(hp, dep->sysname, dep->cid);
	    hp +=3;
	    res = CONS(hp, tpl, res);
	    hp += 2;
	}
        erts_rwmtx_runlock(&erts_dist_table_rwmtx);
	BIF_RET(res);
    } else if (BIF_ARG_1 == am_system_version) {
	erts_dsprintf_buf_t *dsbufp = erts_create_tmp_dsbuf(0);
	erts_print_system_version(ERTS_PRINT_DSBUF, (void *) dsbufp, BIF_P);
	hp = HAlloc(BIF_P, dsbufp->str_len*2);
	res = buf_to_intlist(&hp, dsbufp->str, dsbufp->str_len, NIL);
	erts_destroy_tmp_dsbuf(dsbufp);
	BIF_RET(res);
    } else if (BIF_ARG_1 == am_system_architecture) {
	hp = HAlloc(BIF_P, 2*(sizeof(ERLANG_ARCHITECTURE)-1));
	BIF_RET(buf_to_intlist(&hp,
			       ERLANG_ARCHITECTURE,
			       sizeof(ERLANG_ARCHITECTURE)-1,
			       NIL));
    } 
    else if (BIF_ARG_1 == am_os_type) {
	BIF_RET(erts_get_global_literal(ERTS_LIT_OS_TYPE));
    }
    else if (BIF_ARG_1 == am_allocator) {
	BIF_RET(erts_allocator_options((void *) BIF_P));
    }
    else if (BIF_ARG_1 == am_thread_pool_size) {
	extern int erts_async_max_threads;
	int n;
	
	n = erts_async_max_threads;
	BIF_RET(make_small(n));
    }
    else if (BIF_ARG_1 == am_alloc_util_allocators) {
	BIF_RET(erts_alloc_util_allocators((void *) BIF_P));
    }
    else if (BIF_ARG_1 == am_os_version) {
	BIF_RET(erts_get_global_literal(ERTS_LIT_OS_VERSION));
    }
    else if (BIF_ARG_1 == am_version) {
	int n = sys_strlen(ERLANG_VERSION);
	hp = HAlloc(BIF_P, ((sizeof ERLANG_VERSION)-1) * 2);
	BIF_RET(buf_to_intlist(&hp, ERLANG_VERSION, n, NIL));
    }
    else if (BIF_ARG_1 == am_machine) {
	int n = sys_strlen(EMULATOR);
	hp = HAlloc(BIF_P, n*2);
	BIF_RET(buf_to_intlist(&hp, EMULATOR, n, NIL));
    }
    else if (BIF_ARG_1 == am_garbage_collection) {
	BIF_RET(am_generational);
#ifdef ERTS_OPCODE_COUNTER_SUPPORT
    } else if (BIF_ARG_1 == am_instruction_counts) {
#ifdef DEBUG
	Eterm *endp;
#endif
	Eterm *hp, **hpp;
	Uint hsz, *hszp;
	int i;

	hpp = NULL;
	hsz = 0;
	hszp = &hsz;

    bld_instruction_counts:

	res = NIL;
	for (i = num_instructions-1; i >= 0; i--) {
	    res = erts_bld_cons(hpp, hszp,
				erts_bld_tuple(hpp, hszp, 2,
					       erts_atom_put((byte *)opc[i].name,
							     sys_strlen(opc[i].name),
							     ERTS_ATOM_ENC_LATIN1,
							     1),
					       erts_bld_uint(hpp, hszp,
							     erts_instr_count[i])),
				res);
	}

	if (!hpp) {
	    hp = HAlloc(BIF_P, hsz);
	    hpp = &hp;
#ifdef DEBUG
	    endp = hp + hsz;
#endif
	    hszp = NULL;
	    goto bld_instruction_counts;
	}

	ASSERT(endp == hp);

	BIF_RET(res);
#endif /* #ifndef ERTS_OPCODE_COUNTER_SUPPORT */
    } else if (BIF_ARG_1 == am_wordsize) {
	return make_small(sizeof(Eterm));
    } else if (BIF_ARG_1 == am_endian) {
#if defined(WORDS_BIGENDIAN)
	return am_big;
#else
	return am_little;
#endif
    } else if (BIF_ARG_1 == am_heap_sizes) {
	return erts_heap_sizes(BIF_P);
    } else if (BIF_ARG_1 == am_heap_type) {
	return am_private;
    } else if (ERTS_IS_ATOM_STR("cpu_topology", BIF_ARG_1)) {
	res = erts_get_cpu_topology_term(BIF_P, am_used);
	BIF_TRAP1(erts_format_cpu_topology_trap, BIF_P, res);
    } else if (ERTS_IS_ATOM_STR("update_cpu_info", BIF_ARG_1)) {
	if (erts_update_cpu_info()) {
	    ERTS_DECL_AM(changed);
	    BIF_RET(AM_changed);
	}
	else {
	    ERTS_DECL_AM(unchanged);
	    BIF_RET(AM_unchanged);
	}
#if defined(__GNUC__) && defined(HAVE_SOLARIS_SPARC_PERFMON)
    } else if (ERTS_IS_ATOM_STR("ultrasparc_read_tick1", BIF_ARG_1)) {
	register unsigned high asm("%l0");
	register unsigned low asm("%l1");

	hp = HAlloc(BIF_P, 5);
	asm volatile (".word 0xa3410000;" /* rd %tick, %l1 */
		      ".word 0xa1347020" /* srlx  %l1, 0x20, %l0 */
		      : "=r" (high), "=r" (low));
	res = TUPLE4(hp, make_small(high >> 16),
		     make_small(high & 0xFFFF),
		     make_small(low >> 16),
		     make_small(low & 0xFFFF));
	BIF_RET(res);
    } else if (ERTS_IS_ATOM_STR("ultrasparc_read_tick2", BIF_ARG_1)) {
	register unsigned high asm("%l0");
	register unsigned low asm("%l1");

	asm volatile (".word 0xa3410000;" /* rd %tick, %l1 */
		      ".word 0xa1347020" /* srlx  %l1, 0x20, %l0 */
		      : "=r" (high), "=r" (low));
	hp = HAlloc(BIF_P, 5);
	res = TUPLE4(hp, make_small(high >> 16),
		     make_small(high & 0xFFFF),
		     make_small(low >> 16),
		     make_small(low & 0xFFFF));
	BIF_RET(res);
    } else if (ERTS_IS_ATOM_STR("ultrasparc_read_pic1", BIF_ARG_1)) {
	register unsigned high asm("%l0");
	register unsigned low asm("%l1");

	hp = HAlloc(BIF_P, 5);
	asm volatile (".word 0xa3444000;" /* rd %asr17, %l1 */
		      ".word 0xa1347020" /* srlx  %l1, 0x20, %l0 */
		      : "=r" (high), "=r" (low));
	res = TUPLE4(hp, make_small(high >> 16),
		     make_small(high & 0xFFFF),
		     make_small(low >> 16),
		     make_small(low & 0xFFFF));
	BIF_RET(res);
    } else if (ERTS_IS_ATOM_STR("ultrasparc_read_pic2", BIF_ARG_1)) {
	register unsigned high asm("%l0");
	register unsigned low asm("%l1");

	asm volatile (".word 0xa3444000;" /* rd %asr17, %l1 */
		      ".word 0xa1347020" /* srlx  %l1, 0x20, %l0 */
		      : "=r" (high), "=r" (low));
	hp = HAlloc(BIF_P, 5);
	res = TUPLE4(hp, make_small(high >> 16),
		     make_small(high & 0xFFFF),
		     make_small(low >> 16),
		     make_small(low & 0xFFFF));
	BIF_RET(res);
#endif
    } else if (BIF_ARG_1 == am_threads) {
	return am_true;
    } else if (BIF_ARG_1 == am_creation) {
        Uint hsz = 0;
        erts_bld_uint(NULL, &hsz, erts_this_node->creation);
        hp = hsz ? HAlloc(BIF_P, hsz) : NULL;
        BIF_RET(erts_bld_uint(&hp, NULL, erts_this_node->creation));
    } else if (BIF_ARG_1 == am_break_ignored) {
      extern int ignore_break;
      if (ignore_break) 
	return am_true; 
      else
	return am_false;
    }
    /* Arguments that are unusual follow ... */
    else if (ERTS_IS_ATOM_STR("logical_processors", BIF_ARG_1)) {
	int no;
	erts_get_logical_processors(&no, NULL, NULL, NULL);
	if (no > 0)
	    BIF_RET(make_small((Uint) no));
	else {
	    DECL_AM(unknown);
	    BIF_RET(AM_unknown);
	}
    }
    else if (ERTS_IS_ATOM_STR("logical_processors_online", BIF_ARG_1)) {
	int no;
	erts_get_logical_processors(NULL, &no, NULL, NULL);
	if (no > 0)
	    BIF_RET(make_small((Uint) no));
	else {
	    DECL_AM(unknown);
	    BIF_RET(AM_unknown);
	}
    }
    else if (ERTS_IS_ATOM_STR("logical_processors_available", BIF_ARG_1)) {
	int no;
	erts_get_logical_processors(NULL, NULL, &no, NULL);
	if (no > 0)
	    BIF_RET(make_small((Uint) no));
	else {
	    DECL_AM(unknown);
	    BIF_RET(AM_unknown);
	}
    }
    else if (ERTS_IS_ATOM_STR("cpu_quota", BIF_ARG_1)) {
	int no;
	erts_get_logical_processors(NULL, NULL, NULL, &no);
	if (no > 0)
	    BIF_RET(make_small((Uint) no));
	else {
	    DECL_AM(unknown);
	    BIF_RET(AM_unknown);
	}
    } else if (ERTS_IS_ATOM_STR("otp_release", BIF_ARG_1)) {
	int n = sizeof(ERLANG_OTP_RELEASE)-1;
	hp = HAlloc(BIF_P, 2*n);
	BIF_RET(buf_to_intlist(&hp, ERLANG_OTP_RELEASE, n, NIL));
    } else if (ERTS_IS_ATOM_STR("driver_version", BIF_ARG_1)) {
	char buf[42];
	int n = erts_snprintf(buf, 42, "%d.%d",
			      ERL_DRV_EXTENDED_MAJOR_VERSION,
			      ERL_DRV_EXTENDED_MINOR_VERSION);
	hp = HAlloc(BIF_P, 2*n);
	BIF_RET(buf_to_intlist(&hp, buf, n, NIL));
    } else if (ERTS_IS_ATOM_STR("nif_version", BIF_ARG_1)) {
	char buf[42];
	int n = erts_snprintf(buf, 42, "%d.%d",
			      ERL_NIF_MAJOR_VERSION,
			      ERL_NIF_MINOR_VERSION);
	hp = HAlloc(BIF_P, 2*n);
	BIF_RET(buf_to_intlist(&hp, buf, n, NIL));
    } else if (ERTS_IS_ATOM_STR("smp_support", BIF_ARG_1)) {
	BIF_RET(am_true);
    } else if (ERTS_IS_ATOM_STR("scheduler_bind_type", BIF_ARG_1)) {
	BIF_RET(erts_bound_schedulers_term(BIF_P));
    } else if (ERTS_IS_ATOM_STR("scheduler_bindings", BIF_ARG_1)) {
	BIF_RET(erts_get_schedulers_binds(BIF_P));
    } else if (ERTS_IS_ATOM_STR("constant_pool_support", BIF_ARG_1)) {
	BIF_RET(am_true);
    } else if (ERTS_IS_ATOM_STR("schedulers", BIF_ARG_1)
	       || ERTS_IS_ATOM_STR("schedulers_total", BIF_ARG_1)) {
	res = make_small(erts_no_schedulers);
	BIF_RET(res);
    } else if (ERTS_IS_ATOM_STR("schedulers_state", BIF_ARG_1)) {
	Eterm *hp;
	Uint total, online, active;
	erts_schedulers_state(&total, &online, &active,
			      NULL, NULL, NULL, NULL, NULL);
	hp = HAlloc(BIF_P, 4);
	res = TUPLE3(hp,
		     make_small(total),
		     make_small(online),
		     make_small(active));
	BIF_RET(res);
    } else if (ERTS_IS_ATOM_STR("schedulers_state", BIF_ARG_1)) {
	Eterm *hp;
	Uint total, online, active;
	erts_schedulers_state(&total, &online, &active,
			      NULL, NULL, NULL, NULL, NULL);
	hp = HAlloc(BIF_P, 4);
	res = TUPLE3(hp,
		     make_small(total),
		     make_small(online),
		     make_small(active));
	BIF_RET(res);
    } else if (ERTS_IS_ATOM_STR("all_schedulers_state", BIF_ARG_1)) {
	Eterm *hp, tpl;
	Uint sz, total, online, active,
	    dirty_cpu_total, dirty_cpu_online, dirty_cpu_active,
	    dirty_io_total, dirty_io_active;
	erts_schedulers_state(&total, &online, &active,
			      &dirty_cpu_total, &dirty_cpu_online, &dirty_cpu_active,
			      &dirty_io_total, &dirty_io_active);

	sz = 2+5;
	if (dirty_cpu_total)
	    sz += 2+5;
	if (dirty_io_total)
	    sz += 2+5;

	hp = HAlloc(BIF_P, sz);

	res = NIL;
	if (dirty_io_total) {
	    tpl = TUPLE4(hp,
			 am_dirty_io,
			 make_small(dirty_io_total),
			 make_small(dirty_io_total),
			 make_small(dirty_io_active));
	    hp += 5;
	    res = CONS(hp, tpl, res);
	    hp += 2;
	}
	if (dirty_cpu_total) {
	    tpl = TUPLE4(hp,
			 am_dirty_cpu,
			 make_small(dirty_cpu_total),
			 make_small(dirty_cpu_online),
			 make_small(dirty_cpu_active));
	    hp += 5;
	    res = CONS(hp, tpl, res);
	    hp += 2;
	}
	tpl = TUPLE4(hp,
		     am_normal,
		     make_small(total),
		     make_small(online),
		     make_small(active));
	hp += 5;
	res = CONS(hp, tpl, res);
	BIF_RET(res);
    } else if (ERTS_IS_ATOM_STR("schedulers_online", BIF_ARG_1)) {
	Uint online;
	erts_schedulers_state(NULL, &online, NULL, NULL, NULL, NULL, NULL, NULL);
	BIF_RET(make_small(online));
    } else if (ERTS_IS_ATOM_STR("schedulers_active", BIF_ARG_1)) {
	Uint active;
	erts_schedulers_state(NULL, NULL, &active, NULL, NULL, NULL, NULL, NULL);
	BIF_RET(make_small(active));
    } else if (ERTS_IS_ATOM_STR("dirty_cpu_schedulers", BIF_ARG_1)) {
	Uint dirty_cpu;
	erts_schedulers_state(NULL, NULL, NULL, &dirty_cpu, NULL, NULL, NULL, NULL);
	BIF_RET(make_small(dirty_cpu));
    } else if (ERTS_IS_ATOM_STR("dirty_cpu_schedulers_online", BIF_ARG_1)) {
	Uint dirty_cpu_onln;
	erts_schedulers_state(NULL, NULL, NULL, NULL, &dirty_cpu_onln, NULL, NULL, NULL);
	BIF_RET(make_small(dirty_cpu_onln));
    } else if (ERTS_IS_ATOM_STR("dirty_io_schedulers", BIF_ARG_1)) {
	Uint dirty_io;
	erts_schedulers_state(NULL, NULL, NULL, NULL, NULL, NULL, &dirty_io, NULL);
	BIF_RET(make_small(dirty_io));
    } else if (ERTS_IS_ATOM_STR("run_queues", BIF_ARG_1)) {
	res = make_small(erts_no_run_queues);
	BIF_RET(res);
    } else if (ERTS_IS_ATOM_STR("port_parallelism", BIF_ARG_1)) {
	res = erts_port_parallelism ? am_true : am_false;
	BIF_RET(res);
    } else if (ERTS_IS_ATOM_STR("c_compiler_used", BIF_ARG_1)) {
	Eterm *hp = NULL;
	Uint sz = 0;
	(void) c_compiler_used(NULL, &sz);
	if (sz)
	    hp = HAlloc(BIF_P, sz);
	BIF_RET(c_compiler_used(&hp, NULL));
    } else if (ERTS_IS_ATOM_STR("stop_memory_trace", BIF_ARG_1)) {
	erts_mtrace_stop();
	BIF_RET(am_true);
    } else if (ERTS_IS_ATOM_STR("context_reductions", BIF_ARG_1)) {
	BIF_RET(make_small(CONTEXT_REDS));
    } else if (ERTS_IS_ATOM_STR("kernel_poll", BIF_ARG_1)) {
#if ERTS_ENABLE_KERNEL_POLL
	BIF_RET(am_true);
#else
	BIF_RET(am_false);
#endif    
    } else if (ERTS_IS_ATOM_STR("lock_checking", BIF_ARG_1)) {
#ifdef ERTS_ENABLE_LOCK_CHECK
	BIF_RET(am_true);
#else
	BIF_RET(am_false);
#endif
    } else if (ERTS_IS_ATOM_STR("lock_counting", BIF_ARG_1)) {
#ifdef ERTS_ENABLE_LOCK_COUNT
	BIF_RET(am_true);
#else
	BIF_RET(am_false);
#endif
    } else if (ERTS_IS_ATOM_STR("debug_compiled", BIF_ARG_1)) {
#ifdef DEBUG
	BIF_RET(am_true);
#else
	BIF_RET(am_false);
#endif
    } else if (ERTS_IS_ATOM_STR("check_io", BIF_ARG_1)) {
	BIF_RET(erts_check_io_info(BIF_P));
    } else if (ERTS_IS_ATOM_STR("multi_scheduling_blockers", BIF_ARG_1)) {
	if (erts_no_schedulers == 1)
	    BIF_RET(NIL);
	else
	    BIF_RET(erts_multi_scheduling_blockers(BIF_P, 0));
    } else if (ERTS_IS_ATOM_STR("normal_multi_scheduling_blockers", BIF_ARG_1)) {
	if (erts_no_schedulers == 1)
	    BIF_RET(NIL);
	else
	    BIF_RET(erts_multi_scheduling_blockers(BIF_P, 1));
    } else if (ERTS_IS_ATOM_STR("modified_timing_level", BIF_ARG_1)) {
	BIF_RET(ERTS_USE_MODIFIED_TIMING()
		? make_small(erts_modified_timing_level)
		: am_undefined);
    } else if (ERTS_IS_ATOM_STR("port_tasks", BIF_ARG_1)) {
	BIF_RET(am_true);
    } else if (ERTS_IS_ATOM_STR("io_thread", BIF_ARG_1)) {
	BIF_RET(am_false);
    } else if (ERTS_IS_ATOM_STR("scheduling_statistics", BIF_ARG_1)) {
	BIF_RET(erts_sched_stat_term(BIF_P, 0));
    } else if (ERTS_IS_ATOM_STR("total_scheduling_statistics", BIF_ARG_1)) {
	BIF_RET(erts_sched_stat_term(BIF_P, 1));
    } else if (ERTS_IS_ATOM_STR("taints", BIF_ARG_1)) {
	BIF_RET(erts_nif_taints(BIF_P));
    } else if (ERTS_IS_ATOM_STR("reader_groups_map", BIF_ARG_1)) {
	BIF_RET(erts_get_reader_groups_map(BIF_P));
    } else if (ERTS_IS_ATOM_STR("decentralized_counter_groups_map", BIF_ARG_1)) {
	BIF_RET(erts_get_decentralized_counter_groups_map(BIF_P));
    } else if (ERTS_IS_ATOM_STR("dist_buf_busy_limit", BIF_ARG_1)) {
	Uint hsz = 0;

 	(void) erts_bld_uint(NULL, &hsz, erts_dist_buf_busy_limit);
	hp = hsz ? HAlloc(BIF_P, hsz) : NULL;
	res = erts_bld_uint(&hp, NULL, erts_dist_buf_busy_limit);
	BIF_RET(res);
    } else if (ERTS_IS_ATOM_STR("delayed_node_table_gc", BIF_ARG_1)) {
	Uint hsz = 0;
	Uint dntgc = erts_delayed_node_table_gc();
	if (dntgc == ERTS_NODE_TAB_DELAY_GC_INFINITY)
	    BIF_RET(am_infinity);
 	(void) erts_bld_uint(NULL, &hsz, dntgc);
	hp = hsz ? HAlloc(BIF_P, hsz) : NULL;
	res = erts_bld_uint(&hp, NULL, dntgc);
	BIF_RET(res);
    } else if (ERTS_IS_ATOM_STR("ethread_info", BIF_ARG_1)) {
	BIF_RET(erts_get_ethread_info(BIF_P));
    }
    else if (ERTS_IS_ATOM_STR("ethread_used_tse", BIF_ARG_1)) {
        Uint64 no = (Uint64) ethr_no_used_tse();
        Uint hsz = 0;
        erts_bld_uint64(NULL, &hsz, no);
        hp = hsz ? HAlloc(BIF_P, hsz) : NULL;
        res = erts_bld_uint64(&hp, NULL, no);
        BIF_RET(res);
    }
    else if (ERTS_IS_ATOM_STR("emu_args", BIF_ARG_1)) {
	BIF_RET(erts_get_emu_args(BIF_P));
    }
    else if (ERTS_IS_ATOM_STR("beam_jump_table", BIF_ARG_1)) {
	BIF_RET(erts_beam_jump_table() ? am_true : am_false);
    }
    else if (ERTS_IS_ATOM_STR("dynamic_trace", BIF_ARG_1)) {
#if defined(USE_DTRACE)
	DECL_AM(dtrace);
	BIF_RET(AM_dtrace);
#elif defined(USE_SYSTEMTAP)
	DECL_AM(systemtap);
	BIF_RET(AM_systemtap);
#elif defined(USE_LTTNG)
	DECL_AM(lttng);
	BIF_RET(AM_lttng);
#else
	BIF_RET(am_none);
#endif
    }	    
    else if (ERTS_IS_ATOM_STR("dynamic_trace_probes", BIF_ARG_1)) {
#if defined(USE_VM_PROBES)
	BIF_RET(am_true);
#else
	BIF_RET(am_false);
#endif	
    }
    else if (ERTS_IS_ATOM_STR("thread_progress", BIF_ARG_1)) {
	erts_thr_progress_dbg_print_state();
	BIF_RET(am_true);
    }
    else if (BIF_ARG_1 == am_message_queue_data) {
	switch (erts_default_spo_flags & (SPO_ON_HEAP_MSGQ|SPO_OFF_HEAP_MSGQ)) {
	case SPO_OFF_HEAP_MSGQ:
	    BIF_RET(am_off_heap);
	case SPO_ON_HEAP_MSGQ:
	    BIF_RET(am_on_heap);
	default:
	    ERTS_INTERNAL_ERROR("Inconsistent message queue management state");
	    BIF_RET(am_error);
	}
    }
    else if (ERTS_IS_ATOM_STR("compile_info",BIF_ARG_1)) {
	Uint  sz;
	Eterm res = NIL, tup, text;
	Eterm *hp = HAlloc(BIF_P, 3*(2 + 3) + /* three 2-tuples and three cons */
		2*(sys_strlen(erts_build_flags_CONFIG_H) +
		   sys_strlen(erts_build_flags_CFLAGS) +
		   sys_strlen(erts_build_flags_LDFLAGS)));

	sz   = sys_strlen(erts_build_flags_CONFIG_H);
	text = buf_to_intlist(&hp, erts_build_flags_CONFIG_H, sz, NIL);
	tup  = TUPLE2(hp, am_config_h, text); hp += 3;
	res  = CONS(hp, tup, res); hp += 2;

	sz   = sys_strlen(erts_build_flags_CFLAGS);
	text = buf_to_intlist(&hp, erts_build_flags_CFLAGS, sz, NIL);
	tup  = TUPLE2(hp, am_cflags, text); hp += 3;
	res  = CONS(hp, tup, res); hp += 2;

	sz   = sys_strlen(erts_build_flags_LDFLAGS);
	text = buf_to_intlist(&hp, erts_build_flags_LDFLAGS, sz, NIL);
	tup  = TUPLE2(hp, am_ldflags, text); hp += 3;
	res  = CONS(hp, tup, res); hp += 2;

	BIF_RET(res);
    }
    else if (ERTS_IS_ATOM_STR("ets_limit",BIF_ARG_1)) {
        BIF_RET(make_small(erts_db_get_max_tabs()));
    }
    else if (ERTS_IS_ATOM_STR("ets_count",BIF_ARG_1)) {
        BIF_RET(make_small(erts_ets_table_count()));
    }
    else if (ERTS_IS_ATOM_STR("atom_limit",BIF_ARG_1)) {
        BIF_RET(make_small(erts_get_atom_limit()));
    }
    else if (ERTS_IS_ATOM_STR("atom_count",BIF_ARG_1)) {
        BIF_RET(make_small(atom_table_size()));
    }
    else if (ERTS_IS_ATOM_STR("tolerant_timeofday",BIF_ARG_1)) {
	if (erts_has_time_correction()
	    && erts_time_offset_state() == ERTS_TIME_OFFSET_FINAL) {
	    BIF_RET(am_enabled);
	}
	BIF_RET(am_disabled);
    }
    else if (ERTS_IS_ATOM_STR("eager_check_io",BIF_ARG_1)) {
	BIF_RET(am_true);
    }
    else if (ERTS_IS_ATOM_STR("literal_test",BIF_ARG_1)) {
#ifdef ERTS_HAVE_IS_IN_LITERAL_RANGE
#ifdef ARCH_64
	DECL_AM(range);
	BIF_RET(AM_range);
#else /* ARCH_32 */
	DECL_AM(range_bitmask);
	BIF_RET(AM_range_bitmask);
#endif /* ARCH_32 */
#else  /* ! ERTS_HAVE_IS_IN_LITERAL_RANGE */
	DECL_AM(tag);
	BIF_RET(AM_tag);
#endif
    } else if (ERTS_IS_ATOM_STR("system_logger", BIF_ARG_1)) {
        BIF_RET(erts_get_system_logger());
    }

    BIF_ERROR(BIF_P, BADARG);
}

static int monitor_size(ErtsMonitor *mon, void *vsz, Sint reds)
{
    *((Uint *) vsz) = erts_monitor_size(mon);
    return 1;
}

static int link_size(ErtsMonitor *lnk, void *vsz, Sint reds)
{
    *((Uint *) vsz) = erts_link_size(lnk);
    return 1;
}

/**********************************************************************/ 
/* Return information on ports */
/* Info:
**    id          Port index
**    connected   (Pid)
**    links       List of pids
**    name        String
**    input       Number of bytes input from port program
**    output      Number of bytes output to the port program
**    os_pid      The child's process ID
*/

Eterm
erts_bld_port_info(Eterm **hpp, ErlOffHeap *ohp, Uint *szp, Port *prt,
                   Eterm item)
{
    Eterm res = THE_NON_VALUE;

    ERTS_LC_ASSERT(erts_lc_is_port_locked(prt));

    if (item == am_id) {
	if (hpp)
	    res = make_small(internal_port_index(prt->common.id));
	if (szp) {
	    res = am_true;
	    goto done;
	}
    }
    else if (item == am_links) {
	MonitorInfoCollection mic;
	int i;
	Eterm item;

	INIT_MONITOR_INFOS(mic);

	erts_link_tree_foreach(ERTS_P_LINKS(prt), collect_one_link, (void *) &mic);

	if (szp)
	    *szp += mic.sz;

	if (hpp) {
	    res = NIL;
	    for (i = 0; i < mic.mi_i; i++) {
		item = STORE_NC(hpp, ohp, mic.mi[i].entity.term);
		res = CONS(*hpp, item, res);
		*hpp += 2;
	    }
	}

	DESTROY_MONITOR_INFOS(mic);

	if (szp) {
	    res = am_true;
	    goto done;
	}
    }
    else if (item == am_monitors) {
	MonitorInfoCollection mic;
	int i;

	INIT_MONITOR_INFOS(mic);
        erts_monitor_tree_foreach(ERTS_P_MONITORS(prt),
                                  collect_one_origin_monitor,
                                  (void *) &mic);

	if (szp)
	    *szp += mic.sz;

	if (hpp) {
	    res = NIL;
	    for (i = 0; i < mic.mi_i; i++) {
		Eterm t;

                ASSERT(mic.mi[i].type == ERTS_MON_TYPE_PORT);
                ASSERT(is_internal_pid(mic.mi[i].entity.term));
                t = TUPLE2(*hpp, am_process, mic.mi[i].entity.term);
		*hpp += 3;
		res = CONS(*hpp, t, res);
		*hpp += 2;
	    }
        } // hpp
	DESTROY_MONITOR_INFOS(mic);

	if (szp) {
	    res = am_true;
	    goto done;
	}
    }
    else if (item == am_monitored_by) {
        MonitorInfoCollection mic;
        int i;
        Eterm item;

        INIT_MONITOR_INFOS(mic);
        erts_monitor_list_foreach(ERTS_P_LT_MONITORS(prt),
                                  collect_one_target_monitor,
                                  (void *) &mic);
        erts_monitor_tree_foreach(ERTS_P_MONITORS(prt),
                                  collect_one_target_monitor,
                                  (void *) &mic);
        if (szp)
            *szp += mic.sz;

        if (hpp) {
            res = NIL;
            for (i = 0; i < mic.mi_i; ++i) {
                ASSERT(mic.mi[i].type != ERTS_MON_TYPE_RESOURCE);
                item = STORE_NC(hpp, ohp, mic.mi[i].entity.term);
                res = CONS(*hpp, item, res);
                *hpp += 2;
            }
        } // hpp
        DESTROY_MONITOR_INFOS(mic);

        if (szp) {
            res = am_true;
            goto done;
        }
    }
    else if (item == am_name) {
	int count = sys_strlen(prt->name);

	if (hpp)
	    res = buf_to_intlist(hpp, prt->name, count, NIL);

	if (szp) {
	    *szp += 2*count;
	    res = am_true;
	    goto done;
	}
    }
    else if (item == am_connected) {
	if (hpp)
	    res = ERTS_PORT_GET_CONNECTED(prt); /* internal pid */
	if (szp) {
	    res = am_true;
	    goto done;
	}
    }
    else if (item == am_input) {
	res = erts_bld_uint(hpp, szp, prt->bytes_in);
	if (szp) {
	    res = am_true;
	    goto done;
	}
    }
    else if (item == am_output) {
	res = erts_bld_uint(hpp, szp, prt->bytes_out);
	if (szp) {
	    res = am_true;
	    goto done;
	}
    }
    else if (item == am_os_pid) {
	res = (prt->os_pid < 0
	       ? am_undefined
	       : erts_bld_uword(hpp, szp, (UWord) prt->os_pid));
	if (szp) {
	    res = am_true;
	    goto done;
	}
    }
    else if (item == am_registered_name) {
	RegProc *reg = prt->common.u.alive.reg;
	if (reg) {
	    res = reg->name;
	    if (szp) {
		res = am_true;
		goto done;
	    }
	}
	else {
	    if (szp)
		return am_undefined;
	    return NIL;
	}
    }
    else if (item == am_memory) {
	/* All memory consumed in bytes (the Port struct should not be
	   included though).
	 */
	Uint size = 0;

        erts_link_tree_foreach(ERTS_P_LINKS(prt),
                               link_size, (void *) &size);
        erts_monitor_tree_foreach(ERTS_P_MONITORS(prt),
                                  monitor_size, (void *) &size);
        erts_monitor_list_foreach(ERTS_P_LT_MONITORS(prt),
                                  monitor_size, (void *) &size);

	size += erts_port_data_size(prt);

	if (prt->linebuf)
	    size += sizeof(LineBuf) + prt->linebuf->ovsiz;

	/* ... */


	/* All memory allocated by the driver should be included, but it is
	   hard to retrieve... */
	
	res = erts_bld_uint(hpp, szp, size);
	if (szp) {
	    res = am_true;
	    goto done;
	}
    }
    else if (item == am_queue_size) {
	Uint ioq_size = erts_port_ioq_size(prt);
	res = erts_bld_uint(hpp, szp, ioq_size);
	if (szp) {
	    res = am_true;
	    goto done;
	}
    }
    else if (ERTS_IS_ATOM_STR("locking", item)) {
	if (hpp) {
	    if (erts_atomic32_read_nob(&prt->state)
		& ERTS_PORT_SFLG_PORT_SPECIFIC_LOCK) {
		DECL_AM(port_level);
		ASSERT(prt->drv_ptr->flags
		       & ERL_DRV_FLAG_USE_PORT_LOCKING);
		res = AM_port_level;
	    }
	    else {
		DECL_AM(driver_level);
		ASSERT(!(prt->drv_ptr->flags
			 & ERL_DRV_FLAG_USE_PORT_LOCKING));
		res = AM_driver_level;
	    }
	}
	if (szp) {
	    res = am_true;
	    goto done;
	}
    }
    else if (item == am_parallelism) {
	if (szp) {
	    res = am_true;
	    goto done;
	}
	res = ((ERTS_PTS_FLG_PARALLELISM &
		erts_atomic32_read_nob(&prt->sched.flags))
	       ? am_true
	       : am_false);
    }
    else {
	if (szp)
	    return am_false;
	return THE_NON_VALUE;
    }

done:
    if (szp)
	*szp += 3;
    if (hpp) {
	res = TUPLE2(*hpp, item, res);
	*hpp += 3;
    }

    return res;
}

BIF_RETTYPE
fun_info_2(BIF_ALIST_2)
{
    Process* p = BIF_P;
    Eterm fun = BIF_ARG_1;
    Eterm what = BIF_ARG_2;
    Eterm* hp;
    Eterm val;

    if (is_fun(fun)) {
	ErlFunThing* funp = (ErlFunThing *) fun_val(fun);

	switch (what) {
	case am_type:
	    hp = HAlloc(p, 3);
	    val = am_local;
	    break;
	case am_pid:
	    hp = HAlloc(p, 3);
	    val = funp->creator;
	    break;
	case am_module:
	    hp = HAlloc(p, 3);
	    val = funp->fe->module;
	    break;
	case am_new_index:
	    hp = HAlloc(p, 3);
	    val = make_small(funp->fe->index);
	    break;
	case am_new_uniq:
	    val = new_binary(p, funp->fe->uniq, 16);
	    hp = HAlloc(p, 3);
	    break;
	case am_index:
	    hp = HAlloc(p, 3);
	    val = make_small(funp->fe->old_index);
	    break;
	case am_uniq:
	    hp = HAlloc(p, 3);
	    val = make_small(funp->fe->old_uniq);
	    break;
	case am_env:
	    {
		Uint num_free = funp->num_free;
		int i;

		hp = HAlloc(p, 3 + 2*num_free);
		val = NIL;
		for (i = num_free-1; i >= 0; i--) {
		    val = CONS(hp, funp->env[i], val);
		    hp += 2;
		}
	    }
	    break;
	case am_refc:
	    val = erts_make_integer(erts_atomic_read_nob(&funp->fe->refc), p);
	    hp = HAlloc(p, 3);
	    break;
	case am_arity:
	    hp = HAlloc(p, 3);
	    val = make_small(funp->arity);
	    break;
	case am_name:
            {
                const ErtsCodeMFA *mfa = erts_get_fun_mfa(funp->fe);
                hp = HAlloc(p, 3);
                val = mfa->function;
            }
            break;
	default:
	    goto error;
	}
    } else if (is_export(fun)) {
	Export* exp = (Export *) ((UWord) (export_val(fun))[1]);
	switch (what) {
	case am_type:
	    hp = HAlloc(p, 3);
	    val = am_external;
	    break;
	case am_pid:
	    hp = HAlloc(p, 3);
	    val = am_undefined;
	    break;
	case am_module:
	    hp = HAlloc(p, 3);
	    val = exp->info.mfa.module;
	    break;
	case am_new_index:
	    hp = HAlloc(p, 3);
	    val = am_undefined;
	    break;
	case am_new_uniq:
	    hp = HAlloc(p, 3);
	    val = am_undefined;
	    break;
	case am_index:
	    hp = HAlloc(p, 3);
	    val = am_undefined;
	    break;
	case am_uniq:
	    hp = HAlloc(p, 3);
	    val = am_undefined;
	    break;
	case am_env:
	    hp = HAlloc(p, 3);
	    val = NIL;
	    break;
	case am_refc:
	    hp = HAlloc(p, 3);
	    val = am_undefined;
	    break;
	case am_arity:
	    hp = HAlloc(p, 3);
	    val = make_small(exp->info.mfa.arity);
	    break;
	case am_name:
	    hp = HAlloc(p, 3);
	    val = exp->info.mfa.function;
	    break;
	default:
	    goto error;
	}
    } else {
    error:
	BIF_ERROR(p, BADARG);
    }
    return TUPLE2(hp, what, val);
}

BIF_RETTYPE
fun_info_mfa_1(BIF_ALIST_1)
{
    Process* p = BIF_P;
    Eterm fun = BIF_ARG_1;
    Eterm* hp;

    if (is_fun(fun)) {
        const ErtsCodeMFA *mfa;
        ErlFunThing* funp;

        funp = (ErlFunThing *) fun_val(fun);
        mfa = erts_get_fun_mfa(funp->fe);

        hp = HAlloc(p, 4);
        BIF_RET(TUPLE3(hp,
                       (funp->fe)->module,
                       mfa->function,
                       make_small(funp->arity)));
    } else if (is_export(fun)) {
	Export* exp = (Export *) ((UWord) (export_val(fun))[1]);
	hp = HAlloc(p, 4);
	BIF_RET(TUPLE3(hp,exp->info.mfa.module,
                       exp->info.mfa.function,
                       make_small(exp->info.mfa.arity)));
    }
    BIF_ERROR(p, BADARG);
}

BIF_RETTYPE erts_internal_is_process_alive_2(BIF_ALIST_2)
{
    if (!is_internal_pid(BIF_ARG_1) || !is_internal_ordinary_ref(BIF_ARG_2))
        BIF_ERROR(BIF_P, BADARG);
    if (!erts_proc_sig_send_is_alive_request(BIF_P, BIF_ARG_1, BIF_ARG_2)) {
        if (ERTS_PROC_HAS_INCOMING_SIGNALS(BIF_P))
            ERTS_BIF_HANDLE_SIGNALS_RETURN(BIF_P, am_ok);
    }
    BIF_RET(am_ok);
}

BIF_RETTYPE is_process_alive_1(BIF_ALIST_1) 
{
    if (is_internal_pid(BIF_ARG_1)) {
        erts_aint32_t state;
        Process *rp;

        if (BIF_ARG_1 == BIF_P->common.id)
            BIF_RET(am_true);

        rp = erts_proc_lookup_raw(BIF_ARG_1);
        if (!rp)
            BIF_RET(am_false);

        state = erts_atomic32_read_acqb(&rp->state);
        if (state & (ERTS_PSFLG_EXITING
                     | ERTS_PSFLG_SIG_Q
                     | ERTS_PSFLG_SIG_IN_Q)) {
            /*
             * If in exiting state, trap out and send 'is alive'
             * request and wait for it to complete termination.
             *
             * If process has signals enqueued, we need to
             * send it an 'is alive' request via its signal
             * queue in order to ensure that signal order is
             * preserved (we may earlier have sent it an
             * exit signal that has not been processed yet).
             */
            BIF_TRAP1(is_process_alive_trap, BIF_P, BIF_ARG_1);
        }

        BIF_RET(am_true);
    }

   if (is_external_pid(BIF_ARG_1)) {
       if (external_pid_dist_entry(BIF_ARG_1) == erts_this_dist_entry)
	   BIF_RET(am_false); /* A pid from an old incarnation of this node */
   }

   BIF_ERROR(BIF_P, BADARG);
}

static Eterm
process_display(Process *c_p, void *arg, int *redsp, ErlHeapFragment **bpp)
{
    if (redsp)
        *redsp = 1;

    if (ERTS_PROC_IS_EXITING(c_p))
        return am_badarg;

    erts_proc_lock(c_p, ERTS_PROC_LOCKS_ALL_MINOR);
    erts_stack_dump(ERTS_PRINT_STDERR, NULL, c_p);
    erts_proc_unlock(c_p, ERTS_PROC_LOCKS_ALL_MINOR);

    return am_true;
}


BIF_RETTYPE erts_internal_process_display_2(BIF_ALIST_2)
{
    Eterm res;

    if (BIF_ARG_2 != am_backtrace)
        BIF_RET(am_badopt);

    if (BIF_P->common.id == BIF_ARG_1) {
        res = process_display(BIF_P, NULL, NULL, NULL);
        BIF_RET(res);
    }

    if (is_not_internal_pid(BIF_ARG_1))
        BIF_RET(am_badarg);

    res = erts_proc_sig_send_rpc_request(BIF_P, BIF_ARG_1,
                                         !0,
                                         process_display,
                                         NULL);
    if (is_non_value(res))
        BIF_RET(am_badarg);

    BIF_RET(res);
}

/* this is a general call which return some possibly useful information */

BIF_RETTYPE statistics_1(BIF_ALIST_1)
{
    Eterm res;
    Eterm* hp;

    if (BIF_ARG_1 == am_scheduler_wall_time) {
	res = erts_sched_wall_time_request(BIF_P, 0, 0, 1, 0);
	if (is_non_value(res))
	    BIF_RET(am_undefined);
	BIF_TRAP1(gather_sched_wall_time_res_trap, BIF_P, res);
    } else if (BIF_ARG_1 == am_scheduler_wall_time_all) {
	res = erts_sched_wall_time_request(BIF_P, 0, 0, 1, 1);
	if (is_non_value(res))
	    BIF_RET(am_undefined);
	BIF_TRAP1(gather_sched_wall_time_res_trap, BIF_P, res);
    } else if ((BIF_ARG_1 == am_total_active_tasks)
	       | (BIF_ARG_1 == am_total_run_queue_lengths)
               | (BIF_ARG_1 == am_total_active_tasks_all)
	       | (BIF_ARG_1 == am_total_run_queue_lengths_all)) {
	Uint no = erts_run_queues_len(NULL, 0,
                                      ((BIF_ARG_1 == am_total_active_tasks)
                                       | (BIF_ARG_1 == am_total_active_tasks_all)),
                                      ((BIF_ARG_1 == am_total_active_tasks_all)
                                       | (BIF_ARG_1 == am_total_run_queue_lengths_all)));
	if (IS_USMALL(0, no))
	    res = make_small(no);
	else {
	    Eterm *hp = HAlloc(BIF_P, BIG_UINT_HEAP_SIZE);
	    res = uint_to_big(no, hp);
	}
	BIF_RET(res);
    } else if ((BIF_ARG_1 == am_active_tasks)
           | (BIF_ARG_1 == am_run_queue_lengths)
           | (BIF_ARG_1 == am_active_tasks_all)
           | (BIF_ARG_1 == am_run_queue_lengths_all)) {
	Eterm res, *hp, **hpp;
	Uint sz, *szp;
        int incl_dirty_io = ((BIF_ARG_1 == am_active_tasks_all)
                             | (BIF_ARG_1 == am_run_queue_lengths_all));
        int no_qs = (erts_no_run_queues + ERTS_NUM_DIRTY_CPU_RUNQS +
                     (incl_dirty_io ? ERTS_NUM_DIRTY_IO_RUNQS : 0));
	Uint *qszs = erts_alloc(ERTS_ALC_T_TMP,sizeof(Uint)*no_qs*2);
        (void) erts_run_queues_len(qszs, 0,
                                   ((BIF_ARG_1 == am_active_tasks)
                                    | (BIF_ARG_1 == am_active_tasks_all)),
                                   incl_dirty_io);
	sz = 0;
	szp = &sz;
	hpp = NULL;
	while (1) {
	    int i;
	    for (i = 0; i < no_qs; i++)
		qszs[no_qs+i] = erts_bld_uint(hpp, szp, qszs[i]);
	    res = erts_bld_list(hpp, szp, no_qs, &qszs[no_qs]);
	    if (hpp) {
		erts_free(ERTS_ALC_T_TMP, qszs);
		BIF_RET(res);
	    }
	    hp = HAlloc(BIF_P, sz);
	    szp = NULL;
	    hpp = &hp;
	}
#ifdef ERTS_ENABLE_MSACC
    } else if (BIF_ARG_1 == am_microstate_accounting) {
        Eterm threads;
        res = erts_msacc_request(BIF_P, ERTS_MSACC_GATHER, &threads);
	if (is_non_value(res))
	    BIF_RET(am_undefined);
	BIF_TRAP2(gather_msacc_res_trap, BIF_P, res, threads);
#endif
    } else if (BIF_ARG_1 == am_context_switches) {
	Eterm cs = erts_make_integer(erts_get_total_context_switches(), BIF_P);
	hp = HAlloc(BIF_P, 3);
	res = TUPLE2(hp, cs, SMALL_ZERO);
	BIF_RET(res);
    } else if (BIF_ARG_1 == am_garbage_collection) {
	res = erts_gc_info_request(BIF_P);
	if (is_non_value(res))
	    BIF_RET(am_undefined);
	BIF_TRAP1(gather_gc_info_res_trap, BIF_P, res);
    } else if (BIF_ARG_1 == am_reductions) {
	Uint reds;
	Uint diff;
	Uint hsz = 3;
	Eterm b1, b2;

	erts_get_total_reductions(&reds, &diff);
	(void) erts_bld_uint(NULL, &hsz, reds);
	(void) erts_bld_uint(NULL, &hsz, diff);
	hp = HAlloc(BIF_P, hsz);
	b1 = erts_bld_uint(&hp, NULL, reds);
	b2 = erts_bld_uint(&hp, NULL, diff);
	res = TUPLE2(hp, b1, b2); 
	BIF_RET(res);
    } else if (BIF_ARG_1 == am_exact_reductions) {
	Uint reds;
	Uint diff;
	Uint hsz = 3;
	Eterm b1, b2;

	erts_get_exact_total_reductions(BIF_P, &reds, &diff);
	(void) erts_bld_uint(NULL, &hsz, reds);
	(void) erts_bld_uint(NULL, &hsz, diff);
	hp = HAlloc(BIF_P, hsz);
	b1 = erts_bld_uint(&hp, NULL, reds);
	b2 = erts_bld_uint(&hp, NULL, diff);
	res = TUPLE2(hp, b1, b2); 
	BIF_RET(res);
    } else if (BIF_ARG_1 == am_runtime) {
	ErtsMonotonicTime u1, u2;
	Eterm b1, b2;
        Uint hsz;
	erts_runtime_elapsed_both(&u1, NULL, &u2, NULL);
        hsz = 3; /* 2-tuple */
        (void) erts_bld_monotonic_time(NULL, &hsz, u1);
        (void) erts_bld_monotonic_time(NULL, &hsz, u2);
	hp = HAlloc(BIF_P, hsz);
        b1 = erts_bld_monotonic_time(&hp, NULL, u1);
        b2 = erts_bld_monotonic_time(&hp, NULL, u2);
	res = TUPLE2(hp, b1, b2);
	BIF_RET(res);
    } else if (BIF_ARG_1 ==  am_run_queue) {
	res = erts_run_queues_len(NULL, 1, 0, 0);
	BIF_RET(make_small(res));
    } else if (BIF_ARG_1 == am_wall_clock) {
	ErtsMonotonicTime w1, w2;
	Eterm b1, b2;
        Uint hsz;
	erts_wall_clock_elapsed_both(&w1, &w2);
        hsz = 3; /* 2-tuple */
        (void) erts_bld_monotonic_time(NULL, &hsz, w1);
        (void) erts_bld_monotonic_time(NULL, &hsz, w2);
	hp = HAlloc(BIF_P, hsz);
        b1 = erts_bld_monotonic_time(&hp, NULL, w1);
        b2 = erts_bld_monotonic_time(&hp, NULL, w2);
	res = TUPLE2(hp, b1, b2);
	BIF_RET(res);
    } else if (BIF_ARG_1 == am_io) {
	Eterm ref = erts_request_io_bytes(BIF_P);
	BIF_TRAP2(gather_io_bytes_trap, BIF_P, ref, make_small(erts_no_schedulers));
    }
    else if (ERTS_IS_ATOM_STR("run_queues", BIF_ARG_1)) {
	Eterm res, *hp, **hpp;
	Uint sz, *szp;
	int no_qs = erts_no_run_queues + ERTS_NUM_DIRTY_RUNQS;
	Uint *qszs = erts_alloc(ERTS_ALC_T_TMP,sizeof(Uint)*no_qs*2);
	(void) erts_run_queues_len(qszs, 0, 0, 1);
	sz = 0;
	szp = &sz;
	hpp = NULL;
	while (1) {
	    int i;
	    for (i = 0; i < no_qs; i++)
		qszs[no_qs+i] = erts_bld_uint(hpp, szp, qszs[i]);
	    res = erts_bld_tuplev(hpp, szp, no_qs, &qszs[no_qs]);
	    if (hpp) {
		erts_free(ERTS_ALC_T_TMP, qszs);
		BIF_RET(res);
	    }
	    hp = HAlloc(BIF_P, sz);
	    szp = NULL;
	    hpp = &hp;
	}
    }
    BIF_ERROR(BIF_P, BADARG);
}

BIF_RETTYPE error_logger_warning_map_0(BIF_ALIST_0)
{
    BIF_RET(erts_error_logger_warnings);
}

static erts_atomic_t available_internal_state;

static int empty_magic_ref_destructor(Binary *bin)
{
    return 1;
}

BIF_RETTYPE erts_debug_get_internal_state_1(BIF_ALIST_1)
{
    /*
     * NOTE: Only supposed to be used for testing, and debugging.
     */

    if (!erts_atomic_read_nob(&available_internal_state)) {
	BIF_ERROR(BIF_P, EXC_UNDEF);
    }

    if (is_atom(BIF_ARG_1)) {
	if (ERTS_IS_ATOM_STR("reds_left", BIF_ARG_1)) {
	    /* Used by (emulator) */
	    BIF_RET(make_small((Uint) ERTS_BIF_REDS_LEFT(BIF_P)));
	}
	else if (ERTS_IS_ATOM_STR("node_and_dist_references", BIF_ARG_1)) {
	    /* Used by node_container_SUITE (emulator) */
            BIF_TRAP1(get_internal_state_blocked, BIF_P, BIF_ARG_1);
	}
	else if (ERTS_IS_ATOM_STR("monitoring_nodes", BIF_ARG_1)) {
	    BIF_RET(erts_processes_monitoring_nodes(BIF_P));
	}
	else if (ERTS_IS_ATOM_STR("next_pid", BIF_ARG_1)
		 || ERTS_IS_ATOM_STR("next_port", BIF_ARG_1)) {
	    /* Used by node_container_SUITE (emulator) */
	    Sint res;
	    if (ERTS_IS_ATOM_STR("next_pid", BIF_ARG_1))
		res = erts_ptab_test_next_id(&erts_proc, 0, 0);
	    else
		res = erts_ptab_test_next_id(&erts_port, 0, 0);
	    if (res < 0)
		BIF_RET(am_false);
	    BIF_RET(erts_make_integer(res, BIF_P));
	}
	else if (ERTS_IS_ATOM_STR("DbTable_words", BIF_ARG_1)) {
	    /* Used by ets_SUITE (stdlib) */
	    size_t words = (sizeof(DbTable) + sizeof(Uint) - 1)/sizeof(Uint);
            Eterm* hp = HAlloc(BIF_P ,3);
	    BIF_RET(TUPLE2(hp, make_small((Uint) words),
                           erts_ets_hash_sizeof_ext_segtab()));
	}
	else if (ERTS_IS_ATOM_STR("check_io_debug", BIF_ARG_1)) {
	    /* Used by driver_SUITE (emulator) */
	    Uint sz, *szp;
	    Eterm res, *hp, **hpp;
	    int no_errors;
	    ErtsCheckIoDebugInfo ciodi = {0};
#ifdef HAVE_ERTS_CHECK_IO_DEBUG
	    erts_proc_unlock(BIF_P,ERTS_PROC_LOCK_MAIN);
	    no_errors = erts_check_io_debug(&ciodi);
	    erts_proc_lock(BIF_P,ERTS_PROC_LOCK_MAIN);
#else
	    no_errors = 0;
#endif
	    sz = 0;
	    szp = &sz;
	    hpp = NULL;
	    while (1) {
		res = erts_bld_tuple(hpp, szp, 4,
				     erts_bld_uint(hpp, szp,
						   (Uint) no_errors),
				     erts_bld_uint(hpp, szp,
						   (Uint) ciodi.no_used_fds),
				     erts_bld_uint(hpp, szp,
						   (Uint) ciodi.no_driver_select_structs),
                                     erts_bld_uint(hpp, szp,
                                                   (Uint) ciodi.no_enif_select_structs));
		if (hpp)
		    break;
		hp = HAlloc(BIF_P, sz);
		szp = NULL;
		hpp = &hp;
	    }
	    BIF_RET(res);
	}
	else if (ERTS_IS_ATOM_STR("process_info_args", BIF_ARG_1)) {
	    /* Used by process_SUITE (emulator) */
	    int i;
	    Eterm res = NIL;
	    Uint *hp = HAlloc(BIF_P, 2*ERTS_PI_ARGS);
	    for (i = ERTS_PI_ARGS-1; i >= 0; i--) {
		res = CONS(hp, pi_args[i].name, res);
		hp += 2;
	    }
	    BIF_RET(res);
	}
	else if (ERTS_IS_ATOM_STR("processes", BIF_ARG_1)) {
	    /* Used by process_SUITE (emulator) */
	    BIF_RET(erts_debug_ptab_list(BIF_P, &erts_proc));
	}
	else if (ERTS_IS_ATOM_STR("processes_bif_info", BIF_ARG_1)) {
	    /* Used by process_SUITE (emulator) */
	    BIF_RET(erts_debug_ptab_list_bif_info(BIF_P, &erts_proc));
	}
	else if (ERTS_IS_ATOM_STR("max_atom_out_cache_index", BIF_ARG_1)) {
	    /* Used by distribution_SUITE (emulator) */
	    BIF_RET(make_small((Uint) erts_debug_max_atom_out_cache_index()));
	}
	else if (ERTS_IS_ATOM_STR("nbalance", BIF_ARG_1)) {
	    Uint n;
	    erts_proc_unlock(BIF_P, ERTS_PROC_LOCK_MAIN);
	    n = erts_debug_nbalance();
	    erts_proc_lock(BIF_P, ERTS_PROC_LOCK_MAIN);
	    BIF_RET(erts_make_integer(n, BIF_P));
	}
	else if (ERTS_IS_ATOM_STR("available_internal_state", BIF_ARG_1)) {
	    BIF_RET(am_true);
	}
	else if (ERTS_IS_ATOM_STR("force_heap_frags", BIF_ARG_1)) {
#ifdef FORCE_HEAP_FRAGS
	    BIF_RET(am_true);
#else
	    BIF_RET(am_false);
#endif
	}
	else if (ERTS_IS_ATOM_STR("memory", BIF_ARG_1)) {
	    Eterm res;
	    erts_proc_unlock(BIF_P, ERTS_PROC_LOCK_MAIN);
	    erts_thr_progress_block();
	    erts_proc_lock(BIF_P, ERTS_PROC_LOCK_MAIN);
	    res = erts_memory(NULL, NULL, BIF_P, THE_NON_VALUE);
	    erts_thr_progress_unblock();
	    BIF_RET(res);
	}
        else if (ERTS_IS_ATOM_STR("mmap", BIF_ARG_1)) {
            BIF_RET(erts_mmap_debug_info(BIF_P));
        }
	else if (ERTS_IS_ATOM_STR("unique_monotonic_integer_state", BIF_ARG_1)) {
	    BIF_RET(erts_debug_get_unique_monotonic_integer_state(BIF_P));
	}
	else if (ERTS_IS_ATOM_STR("min_unique_monotonic_integer", BIF_ARG_1)) {
	    Sint64 value = erts_get_min_unique_monotonic_integer();
	    if (IS_SSMALL(value))
		BIF_RET(make_small(value));
	    else {
		Uint hsz = ERTS_SINT64_HEAP_SIZE(value);
		Eterm *hp = HAlloc(BIF_P, hsz);
		BIF_RET(erts_sint64_to_big(value, &hp));
	    }
	}
	else if (ERTS_IS_ATOM_STR("min_unique_integer", BIF_ARG_1)) {
	    Sint64 value = erts_get_min_unique_integer();
	    if (IS_SSMALL(value))
		BIF_RET(make_small(value));
	    else {
		Uint hsz = ERTS_SINT64_HEAP_SIZE(value);
		Eterm *hp = HAlloc(BIF_P, hsz);
		BIF_RET(erts_sint64_to_big(value, &hp));
	    }
	}
        else if (ERTS_IS_ATOM_STR("stack_check", BIF_ARG_1)) {
            UWord size;
            char c;
            if (erts_is_above_stack_limit(&c))
                size = erts_check_stack_recursion_downwards(&c, &c);
            else
                size = erts_check_stack_recursion_upwards(&c, &c);
	    if (IS_SSMALL(size))
		BIF_RET(make_small(size));
	    else {
		Uint hsz = BIG_UWORD_HEAP_SIZE(size);
		Eterm *hp = HAlloc(BIF_P, hsz);
		BIF_RET(uword_to_big(size, hp));
	    }
        } else if (ERTS_IS_ATOM_STR("scheduler_dump", BIF_ARG_1)) {
#if defined(ERTS_HAVE_TRY_CATCH) && defined(ERTS_SYS_SUSPEND_SIGNAL)
            BIF_RET(am_true);
#else
            BIF_RET(am_false);
#endif
        }
        else if (ERTS_IS_ATOM_STR("lc_graph", BIF_ARG_1)) {
#ifdef ERTS_ENABLE_LOCK_CHECK
            Eterm res = erts_lc_dump_graph();
            BIF_RET(res);
#else
            BIF_RET(am_notsup);
#endif
        }
        else if (ERTS_IS_ATOM_STR("flxctr_memory_usage", BIF_ARG_1)) {
            Sint mem = erts_flxctr_debug_memory_usage();
            if (mem == -1) {
                BIF_RET(am_notsup);
            } else {
		Uint hsz = BIG_UWORD_HEAP_SIZE((UWord)mem);
		Eterm *hp = HAlloc(BIF_P, hsz);
		BIF_RET(uword_to_big((UWord)mem, hp));
            }
        }
        else if (ERTS_IS_ATOM_STR("persistent_term", BIF_ARG_1)) {
            BIF_RET(erts_debug_persistent_term_xtra_info(BIF_P));
        }
    }
    else if (is_tuple(BIF_ARG_1)) {
	Eterm* tp = tuple_val(BIF_ARG_1);
	switch (arityval(tp[0])) {
	case 2: {
	    if (ERTS_IS_ATOM_STR("node_and_dist_references", tp[1])) {
                if (tp[2] == am_blocked
                    && erts_is_multi_scheduling_blocked() > 0) {
                    Eterm res = erts_get_node_and_dist_references(BIF_P);
                    BIF_RET(res);
                }
            }
	    else if (ERTS_IS_ATOM_STR("process_status", tp[1])) {
		/* Used by timer process_SUITE, timer_bif_SUITE, and
		   node_container_SUITE (emulator) */
		if (is_internal_pid(tp[2])) {
		    BIF_RET(erts_process_status(NULL, tp[2]));
		}
	    }
            else if (ERTS_IS_ATOM_STR("connection_id", tp[1])) {
                DistEntry *dep;
                Eterm *hp, res;
                Uint con_id, hsz = 0;
                if (!is_atom(tp[2]))
                    BIF_ERROR(BIF_P, BADARG);
                dep = erts_sysname_to_connected_dist_entry(tp[2]);
                if (!dep)
                    BIF_ERROR(BIF_P, BADARG);
                erts_de_rlock(dep);
                con_id = (Uint) dep->connection_id;
                erts_de_runlock(dep);
                (void) erts_bld_uint(NULL, &hsz, con_id);
                hp = hsz ? HAlloc(BIF_P, hsz) : NULL;
                res = erts_bld_uint(&hp, NULL, con_id);
                BIF_RET(res);
            }
	    else if (ERTS_IS_ATOM_STR("link_list", tp[1])) {
		/* Used by erl_link_SUITE (emulator) */
		if(is_internal_pid(tp[2])) {
                    erts_aint32_t state;
		    Eterm res;
		    Process *p;
                    int sigs_done;

		    p = erts_pid2proc(BIF_P,
				      ERTS_PROC_LOCK_MAIN,
				      tp[2],
				      ERTS_PROC_LOCK_MAIN);
		    if (!p) {
			ERTS_ASSERT_IS_NOT_EXITING(BIF_P);
			BIF_RET(am_undefined);
		    }
                    
                    erts_proc_lock(p, ERTS_PROC_LOCK_MSGQ);
                    erts_proc_sig_fetch(p);
                    erts_proc_unlock(p, ERTS_PROC_LOCK_MSGQ);
                    do {
                        int reds = CONTEXT_REDS;
                        sigs_done = erts_proc_sig_handle_incoming(p,
                                                                  &state,
                                                                  &reds,
                                                                  CONTEXT_REDS,
                                                                  !0);
                    } while (!sigs_done && !(state & ERTS_PSFLG_EXITING));

                    if (!(state & ERTS_PSFLG_EXITING))
                        res = make_link_list(BIF_P, 1, ERTS_P_LINKS(p), NIL);
                    else if (BIF_P == p)
                        ERTS_BIF_EXITED(BIF_P);
                    else
                        res = am_undefined;
                    if (BIF_P != p)
                        erts_proc_unlock(p, ERTS_PROC_LOCK_MAIN);
		    BIF_RET(res);
		}
		else if(is_internal_port(tp[2])) {
		    Eterm res;
		    Port *p = erts_id2port_sflgs(tp[2],
						 BIF_P,
						 ERTS_PROC_LOCK_MAIN,
						 ERTS_PORT_SFLGS_INVALID_LOOKUP);
		    if(!p)
			BIF_RET(am_undefined);
		    res = make_link_list(BIF_P, 1, ERTS_P_LINKS(p), NIL);
		    erts_port_release(p);
		    BIF_RET(res);
		}
		else if(is_node_name_atom(tp[2])) {
		    DistEntry *dep = erts_find_dist_entry(tp[2]);
		    if(dep) {
			Eterm res = NIL;
                        if (dep->mld) {
                            erts_mtx_lock(&dep->mld->mtx);
                            res = make_link_list(BIF_P, 0, dep->mld->links, NIL);
                            erts_mtx_unlock(&dep->mld->mtx);
                        }
			BIF_RET(res);
		    } else {
			BIF_RET(am_undefined);
		    }
		}
	    }
	    else if (ERTS_IS_ATOM_STR("monitor_list", tp[1])) {
		/* Used by erl_link_SUITE (emulator) */
		if(is_internal_pid(tp[2])) {
                    erts_aint32_t state;
		    Process *p;
		    Eterm res;
                    int sigs_done;

		    p = erts_pid2proc(BIF_P,
				      ERTS_PROC_LOCK_MAIN,
				      tp[2],
				      ERTS_PROC_LOCK_MAIN);
		    if (!p) {
			ERTS_ASSERT_IS_NOT_EXITING(BIF_P);
			BIF_RET(am_undefined);
		    }
                    
                    erts_proc_lock(p, ERTS_PROC_LOCK_MSGQ);
                    erts_proc_sig_fetch(p);
                    erts_proc_unlock(p, ERTS_PROC_LOCK_MSGQ);
                    do {
                        int reds = CONTEXT_REDS;
                        sigs_done = erts_proc_sig_handle_incoming(p,
                                                                  &state,
                                                                  &reds,
                                                                  CONTEXT_REDS,
                                                                  !0);
                    } while (!sigs_done && !(state & ERTS_PSFLG_EXITING));

                    if (!(state & ERTS_PSFLG_EXITING)) {
                        res = make_monitor_list(BIF_P, 1, ERTS_P_MONITORS(p), NIL);
                        res = make_monitor_list(BIF_P, 0, ERTS_P_LT_MONITORS(p), res);
                    }
                    else {
                        if (BIF_P == p)
                            ERTS_BIF_EXITED(BIF_P);
                        else
                            res = am_undefined;
                    }
                    if (BIF_P != p)
                        erts_proc_unlock(p, ERTS_PROC_LOCK_MAIN);
		    BIF_RET(res);
		} else if(is_node_name_atom(tp[2])) {
		    DistEntry *dep = erts_find_dist_entry(tp[2]);
		    if(dep) {
			Eterm ml = NIL;
                        if (dep->mld) {
                            erts_mtx_lock(&dep->mld->mtx);
                            ml = make_monitor_list(BIF_P, 1, dep->mld->orig_name_monitors, NIL);
                            ml = make_monitor_list(BIF_P, 0, dep->mld->monitors, ml);
                            erts_mtx_unlock(&dep->mld->mtx);
                        }
			BIF_RET(ml);
		    } else {
			BIF_RET(am_undefined);
		    }
		}
	    }
	    else if (ERTS_IS_ATOM_STR("channel_number", tp[1])) {
		Eterm res;
		DistEntry *dep = erts_find_dist_entry(tp[2]);
		if (!dep)
		    res = am_undefined;
		else {
		    Uint cno = dist_entry_channel_no(dep);
		    res = make_small(cno);
		}
		BIF_RET(res);
	    }
	    else if (ERTS_IS_ATOM_STR("binary_info", tp[1])) {
		Eterm bin = tp[2];
		if (is_binary(bin)) {
		    Eterm real_bin = bin;
		    Eterm res = am_true;
		    ErlSubBin* sb = (ErlSubBin *) binary_val(real_bin);

		    if (sb->thing_word == HEADER_SUB_BIN) {
			real_bin = sb->orig;
		    }
		    if (*binary_val(real_bin) == HEADER_PROC_BIN) {
			ProcBin* pb;
			Binary* val;
			Eterm SzTerm;
			Uint hsz = 3 + 5;
			Eterm* hp;
			DECL_AM(refc_binary);

			pb = (ProcBin *) binary_val(real_bin);
			val = pb->val;
			(void) erts_bld_uint(NULL, &hsz, pb->size);
			(void) erts_bld_uint(NULL, &hsz, val->orig_size);
			hp = HAlloc(BIF_P, hsz);

			/* Info about the Binary* object */
			SzTerm = erts_bld_uint(&hp, NULL, val->orig_size);
			res = TUPLE2(hp, am_binary, SzTerm);
			hp += 3;

			/* Info about the ProcBin* object */
			SzTerm = erts_bld_uint(&hp, NULL, pb->size);
			res = TUPLE4(hp, AM_refc_binary, SzTerm,
				     res, make_small(pb->flags));
		    } else {	/* heap binary */
			DECL_AM(heap_binary);
			res = AM_heap_binary;
		    }
		    BIF_RET(res);
		}
	    }
	    else if (ERTS_IS_ATOM_STR("dist_ctrl", tp[1])) {
		Eterm res = am_undefined;
		DistEntry *dep = erts_sysname_to_connected_dist_entry(tp[2]);
		if (dep) {
		    erts_de_rlock(dep);
		    if (is_internal_port(dep->cid) || is_internal_pid(dep->cid))
			res = dep->cid;
		    erts_de_runlock(dep);
		}
		BIF_RET(res);
	    }
	    else if (ERTS_IS_ATOM_STR("atom_out_cache_index", tp[1])) {
		/* Used by distribution_SUITE (emulator) */
		if (is_atom(tp[2])) {
		    BIF_RET(make_small(
				(Uint)
				erts_debug_atom_to_out_cache_index(tp[2])));
		}
	    }
	    else if (ERTS_IS_ATOM_STR("fake_scheduler_bindings", tp[1])) {
		return erts_fake_scheduler_bindings(BIF_P, tp[2]);
	    }
	    else if (ERTS_IS_ATOM_STR("reader_groups_map", tp[1])) {
		Sint groups;
		if (is_not_small(tp[2]))
		    BIF_ERROR(BIF_P, BADARG);
		groups = signed_val(tp[2]);
		if (groups < (Sint) 1 || groups > (Sint) INT_MAX)
		    BIF_ERROR(BIF_P, BADARG);

		BIF_RET(erts_debug_reader_groups_map(BIF_P, (int) groups));
	    }
	    else if (ERTS_IS_ATOM_STR("internal_hash", tp[1])) {
		Uint hash = (Uint) make_internal_hash(tp[2], 0);
		Uint hsz = 0;
		Eterm* hp;
		erts_bld_uint(NULL, &hsz, hash);
		hp = HAlloc(BIF_P,hsz);
		return erts_bld_uint(&hp, NULL, hash);
	    }
	    else if (ERTS_IS_ATOM_STR("atom", tp[1])) {
		Uint ix;
		if (!term_to_Uint(tp[2], &ix))
		    BIF_ERROR(BIF_P, BADARG);
		while (ix >= atom_table_size()) {
		    char tmp[20];
		    erts_snprintf(tmp, sizeof(tmp), "am%x", atom_table_size());
		    erts_atom_put((byte *) tmp, sys_strlen(tmp), ERTS_ATOM_ENC_LATIN1, 1);
		}
		return make_atom(ix);
	    }
	    else if (ERTS_IS_ATOM_STR("magic_ref", tp[1])) {
                Binary *bin;
                UWord bin_addr, refc;
                Eterm bin_addr_term, refc_term, test_type;
                Uint sz;
                Eterm *hp;
                if (!is_internal_magic_ref(tp[2])) {
                    if (is_internal_ordinary_ref(tp[2])) {
                        ErtsORefThing *rtp;
                        rtp = (ErtsORefThing *) internal_ref_val(tp[2]);
                        if (erts_is_ref_numbers_magic(rtp->num))
                            BIF_RET(am_true);
                    }
                    BIF_RET(am_false);
                }
                bin = erts_magic_ref2bin(tp[2]);
                refc = erts_refc_read(&bin->intern.refc, 1);
                bin_addr = (UWord) bin;
                sz = 4;
                erts_bld_uword(NULL, &sz, bin_addr);
                erts_bld_uword(NULL, &sz, refc);
                hp = HAlloc(BIF_P, sz);
                bin_addr_term = erts_bld_uword(&hp, NULL, bin_addr);
                refc_term = erts_bld_uword(&hp, NULL, refc);
                test_type = (ERTS_MAGIC_BIN_DESTRUCTOR(bin) == empty_magic_ref_destructor
                             ? am_true : am_false);
                BIF_RET(TUPLE3(hp, bin_addr_term, refc_term, test_type));
	    }

	    break;
	}
	case 3: {
	    if (ERTS_IS_ATOM_STR("check_time_config", tp[1])) {
		int res, time_correction;
		ErtsTimeWarpMode time_warp_mode;
		if (tp[2] == am_true)
		    time_correction = !0;
		else if (tp[2] == am_false)
		    time_correction = 0;
		else
		    break;
		if (ERTS_IS_ATOM_STR("no_time_warp", tp[3]))
		    time_warp_mode = ERTS_NO_TIME_WARP_MODE;
		else if (ERTS_IS_ATOM_STR("single_time_warp", tp[3]))
		    time_warp_mode = ERTS_SINGLE_TIME_WARP_MODE;
		else if (ERTS_IS_ATOM_STR("multi_time_warp", tp[3]))
		    time_warp_mode = ERTS_MULTI_TIME_WARP_MODE;
		else
		    break;
		res = erts_check_time_adj_support(time_correction,
						  time_warp_mode);
		BIF_RET(res ? am_true : am_false);
	    }
	    else if (ERTS_IS_ATOM_STR("make_unique_integer", tp[1])) {
	      Eterm res = erts_debug_make_unique_integer(BIF_P,
							 tp[2],
							 tp[3]);
	      if (is_non_value(res))
		  break;
	      BIF_RET(res);
	    }
            else if (ERTS_IS_ATOM_STR("term_to_binary", tp[1])) {
                return erts_debug_term_to_binary(BIF_P, tp[2], tp[3]);
            }
	    break;
	}
	default:
	    break;
	}
    }
    BIF_ERROR(BIF_P, BADARG);
}

BIF_RETTYPE erts_internal_is_system_process_1(BIF_ALIST_1)
{
    if (is_internal_pid(BIF_ARG_1)) {
	Process *rp = erts_proc_lookup(BIF_ARG_1);
	if (rp && (rp->static_flags & ERTS_STC_FLG_SYSTEM_PROC))
	    BIF_RET(am_true);
	BIF_RET(am_false);
    }

    if (is_external_pid(BIF_ARG_1)
	&& external_pid_dist_entry(BIF_ARG_1) == erts_this_dist_entry) {
	BIF_RET(am_false);
    }

    BIF_ERROR(BIF_P, BADARG);
}

BIF_RETTYPE erts_internal_system_check_1(BIF_ALIST_1)
{
    Eterm res;
    if (ERTS_IS_ATOM_STR("schedulers", BIF_ARG_1)) {
	res = erts_system_check_request(BIF_P);
	if (is_non_value(res))
	    BIF_RET(am_undefined);
	BIF_TRAP1(gather_system_check_res_trap, BIF_P, res);
    }

    BIF_ERROR(BIF_P, BADARG);
}

#if defined(VALGRIND) && defined(__GNUC__)
/* Force noinline for valgrind suppression */
static void broken_halt_test(Eterm bif_arg_2) __attribute__((noinline));
#endif

static void broken_halt_test(Eterm bif_arg_2)
{
    /* Ugly ugly code used by bif_SUITE:erlang_halt/1 */
#if defined(ERTS_HAVE_TRY_CATCH)
    erts_get_scheduler_data()->run_queue = NULL;
#endif
    erts_exit(ERTS_DUMP_EXIT, "%T", bif_arg_2);
}

static void
test_multizero_timeout_in_timeout3(void *vproc)
{
    Process *proc = (Process *) vproc;
    ErtsMessage *mp = erts_alloc_message(0, NULL);
    ERTS_DECL_AM(multizero_timeout_in_timeout_done);
    erts_queue_message(proc, 0, mp, AM_multizero_timeout_in_timeout_done, am_system);
    erts_proc_dec_refc(proc);
}

static void
test_multizero_timeout_in_timeout2(void *vproc)
{
    erts_start_timer_callback(0, test_multizero_timeout_in_timeout3, vproc);
}

static void
test_multizero_timeout_in_timeout(void *vproc)
{
    erts_start_timer_callback(0, test_multizero_timeout_in_timeout2, vproc);
}

BIF_RETTYPE erts_debug_set_internal_state_2(BIF_ALIST_2)
{
    /*
     * NOTE: Only supposed to be used for testing, and debugging.
     */
    if (ERTS_IS_ATOM_STR("available_internal_state", BIF_ARG_1)
	&& (BIF_ARG_2 == am_true || BIF_ARG_2 == am_false)) {
	erts_aint_t on = (erts_aint_t) (BIF_ARG_2 == am_true);
	erts_aint_t prev_on = erts_atomic_xchg_nob(&available_internal_state, on);
	if (on) {
	    erts_dsprintf_buf_t *dsbufp = erts_create_logger_dsbuf();
	    erts_dsprintf(dsbufp, "Process %T ", BIF_P->common.id);
	    if (erts_is_alive)
		erts_dsprintf(dsbufp, "on node %T ", erts_this_node->sysname);
	    erts_dsprintf(dsbufp,
			  "enabled access to the emulator internal state.\n");
	    erts_dsprintf(dsbufp,
			  "NOTE: This is an erts internal test feature and "
			  "should *only* be used by OTP test-suites.\n");
	    erts_send_warning_to_logger(BIF_P->group_leader, dsbufp);
	}
	BIF_RET(prev_on ? am_true : am_false);
    }

    if (!erts_atomic_read_nob(&available_internal_state)) {
	BIF_ERROR(BIF_P, EXC_UNDEF);
    }

    if (is_atom(BIF_ARG_1)) {
	
	if (ERTS_IS_ATOM_STR("reds_left", BIF_ARG_1)) {
	    Sint reds;
	    if (term_to_Sint(BIF_ARG_2, &reds) != 0) {
		if (0 <= reds && reds <= CONTEXT_REDS) {
		    if (!ERTS_PROC_GET_SAVED_CALLS_BUF(BIF_P))
			BIF_P->fcalls = reds;
		    else
			BIF_P->fcalls = reds - CONTEXT_REDS;
                    BIF_P->scheduler_data->virtual_reds = 0;
		}
		BIF_RET(am_true);
	    }
	}
	else if (ERTS_IS_ATOM_STR("block", BIF_ARG_1)
		 || ERTS_IS_ATOM_STR("sleep", BIF_ARG_1)) {
	    int block = ERTS_IS_ATOM_STR("block", BIF_ARG_1);
	    Sint ms;
	    if (term_to_Sint(BIF_ARG_2, &ms) != 0) {
		if (ms > 0) {
		    erts_proc_unlock(BIF_P, ERTS_PROC_LOCK_MAIN);
		    if (block)
			erts_thr_progress_block();
		    while (erts_milli_sleep((long) ms) != 0);
		    if (block)
			erts_thr_progress_unblock();
		    erts_proc_lock(BIF_P, ERTS_PROC_LOCK_MAIN);
		}
		BIF_RET(am_true);
	    }
	}
	else if (ERTS_IS_ATOM_STR("block_scheduler", BIF_ARG_1)) {
	    Sint ms;
	    if (term_to_Sint(BIF_ARG_2, &ms) != 0) {
		if (ms > 0) {
		    erts_proc_unlock(BIF_P, ERTS_PROC_LOCK_MAIN);
		    while (erts_milli_sleep((long) ms) != 0);
		    erts_proc_lock(BIF_P, ERTS_PROC_LOCK_MAIN);
		}
		BIF_RET(am_true);
	    }
	}
	else if (ERTS_IS_ATOM_STR("next_pid", BIF_ARG_1)
		 || ERTS_IS_ATOM_STR("next_port", BIF_ARG_1)) {
	    /* Used by node_container_SUITE (emulator) */
	    Uint next;

	    if (term_to_Uint(BIF_ARG_2, &next) != 0) {
		Sint res;

		if (ERTS_IS_ATOM_STR("next_pid", BIF_ARG_1))
		    res = erts_ptab_test_next_id(&erts_proc, 1, next);
		else
		    res = erts_ptab_test_next_id(&erts_port, 1, next);
		if (res < 0)
		    BIF_RET(am_false);
		BIF_RET(erts_make_integer(res, BIF_P));
	    }
	}
	else if (ERTS_IS_ATOM_STR("force_gc", BIF_ARG_1)) {
	    /* Used by signal_SUITE (emulator) */
	    Process *rp = erts_pid2proc(BIF_P, ERTS_PROC_LOCK_MAIN,
					BIF_ARG_2, ERTS_PROC_LOCK_MAIN);
	    if (!rp) {
		BIF_RET(am_false);
	    }
	    else {
		ERTS_FORCE_GC(BIF_P);
		BIF_RET(am_true);
	    }
	}
	else if (ERTS_IS_ATOM_STR("gc_state", BIF_ARG_1)) {
	    /* Used by process_SUITE (emulator) */
	    int res, enable;

	    switch (BIF_ARG_2) {
	    case am_true: enable = 1; break;
	    case am_false: enable = 0; break;
	    default: BIF_ERROR(BIF_P, BADARG); break;
	    }
 
            res = (BIF_P->flags & F_DISABLE_GC) ? am_false : am_true;
	    erts_set_gc_state(BIF_P, enable);
	    BIF_RET(res);
	}
        else if (ERTS_IS_ATOM_STR("inconsistent_heap", BIF_ARG_1)) {
            /* Used by code_SUITE (emulator) */
            if (am_start == BIF_ARG_2) {
                Eterm broken_term;
                Eterm *hp;

                ERTS_ASSERT(!(BIF_P->flags & F_DISABLE_GC));
                erts_set_gc_state(BIF_P, 0);

                hp = HAlloc(BIF_P, 2);
                hp[0] = make_arityval(1234);
                hp[1] = THE_NON_VALUE;

                broken_term = make_tuple(hp);

                BIF_RET(broken_term);
            } else {
                Eterm broken_term;
                Eterm *hp;

                broken_term = BIF_ARG_2;

                hp = tuple_val(broken_term);
                ERTS_ASSERT(hp[0] == make_arityval(1234));
                ERTS_ASSERT(hp[1] == THE_NON_VALUE);
                hp[0] = make_arityval(1);
                hp[1] = am_ok;

                ERTS_ASSERT(BIF_P->flags & F_DISABLE_GC);
                erts_set_gc_state(BIF_P, 1);

                BIF_RET(am_ok);
            }
        }
        else if (ERTS_IS_ATOM_STR("colliding_names", BIF_ARG_1)) {
	    /* Used by ets_SUITE (stdlib) */
	    if (is_tuple(BIF_ARG_2)) {
                Eterm* tpl = tuple_val(BIF_ARG_2);
                Uint cnt;
                if (arityval(tpl[0]) == 2 && is_atom(tpl[1]) && 
                    term_to_Uint(tpl[2], &cnt)) {
                    BIF_RET(erts_ets_colliding_names(BIF_P,tpl[1],cnt));
                }
	    }
	}
	else if (ERTS_IS_ATOM_STR("binary_loop_limit", BIF_ARG_1)) {
	    /* Used by binary_module_SUITE (stdlib) */
	    Uint max_loops;
	    if (is_atom(BIF_ARG_2) && ERTS_IS_ATOM_STR("default", BIF_ARG_2)) {
		max_loops = erts_binary_set_loop_limit(-1);
		BIF_RET(make_small(max_loops));
	    } else if (term_to_Uint(BIF_ARG_2, &max_loops) != 0) {
		max_loops = erts_binary_set_loop_limit(max_loops);
		BIF_RET(make_small(max_loops));
	    }
	}
	else if (ERTS_IS_ATOM_STR("re_loop_limit", BIF_ARG_1)) {
	    /* Used by re_SUITE (stdlib) */
	    Uint max_loops;
	    if (is_atom(BIF_ARG_2) && ERTS_IS_ATOM_STR("default", BIF_ARG_2)) {
		max_loops = erts_re_set_loop_limit(-1);
		BIF_RET(make_small(max_loops));
	    } else if (term_to_Uint(BIF_ARG_2, &max_loops) != 0) {
		max_loops = erts_re_set_loop_limit(max_loops);
		BIF_RET(make_small(max_loops));
	    }
	}
	else if (ERTS_IS_ATOM_STR("unicode_loop_limit", BIF_ARG_1)) {
	    /* Used by unicode_SUITE (stdlib) */
	    Uint max_loops;
	    if (is_atom(BIF_ARG_2) && ERTS_IS_ATOM_STR("default", BIF_ARG_2)) {
		max_loops = erts_unicode_set_loop_limit(-1);
		BIF_RET(make_small(max_loops));
	    } else if (term_to_Uint(BIF_ARG_2, &max_loops) != 0) {
		max_loops = erts_unicode_set_loop_limit(max_loops);
		BIF_RET(make_small(max_loops));
	    }
	}
	else if (ERTS_IS_ATOM_STR("test_long_gc_sleep", BIF_ARG_1)) {
	    if (term_to_Uint(BIF_ARG_2, &erts_test_long_gc_sleep) > 0)
		BIF_RET(am_true);
	}
	else if (ERTS_IS_ATOM_STR("abort", BIF_ARG_1)) {
	    erts_exit(ERTS_ABORT_EXIT, "%T\n", BIF_ARG_2);
	}
	else if (ERTS_IS_ATOM_STR("kill_dist_connection", BIF_ARG_1)) {
	    DistEntry *dep = erts_sysname_to_connected_dist_entry(BIF_ARG_2);
	    if (!dep)
		BIF_RET(am_false);
	    else {
		Uint32 con_id;
		erts_de_rlock(dep);
		con_id = dep->connection_id;
		erts_de_runlock(dep);
		erts_kill_dist_connection(dep, con_id);
		BIF_RET(am_true);
	    }
	}
	else if (ERTS_IS_ATOM_STR("wait", BIF_ARG_1)) {
            int flag = 0;
	    if (ERTS_IS_ATOM_STR("deallocations", BIF_ARG_2))
                flag = ERTS_DEBUG_WAIT_COMPLETED_DEALLOCATIONS;
            else if (ERTS_IS_ATOM_STR("timer_cancellations", BIF_ARG_2))
		flag = ERTS_DEBUG_WAIT_COMPLETED_TIMER_CANCELLATIONS;
            else if (ERTS_IS_ATOM_STR("aux_work", BIF_ARG_2))
                flag = ERTS_DEBUG_WAIT_COMPLETED_AUX_WORK;
            else if (ERTS_IS_ATOM_STR("thread_progress", BIF_ARG_2))
                flag = ERTS_DEBUG_WAIT_COMPLETED_THREAD_PROGRESS;

            if (flag) {
                if (erts_debug_wait_completed(BIF_P, flag))
                    ERTS_BIF_YIELD_RETURN(BIF_P, am_ok);
                else
                    BIF_ERROR(BIF_P, SYSTEM_LIMIT);
            }
	}
        else if (ERTS_IS_ATOM_STR("broken_halt", BIF_ARG_1)) {
            erts_proc_unlock(BIF_P, ERTS_PROC_LOCK_MAIN);
            broken_halt_test(BIF_ARG_2);
        }
	else if (ERTS_IS_ATOM_STR("unique_monotonic_integer_state", BIF_ARG_1)) {
	    int res = erts_debug_set_unique_monotonic_integer_state(BIF_ARG_2);
	    BIF_RET(res ? am_true : am_false);
	}
	else if (ERTS_IS_ATOM_STR("node_tab_delayed_delete", BIF_ARG_1)) {
	    /* node_container_SUITE */
	    Sint64 msecs;
	    if (term_to_Sint64(BIF_ARG_2, &msecs)) {
		/* Negative value restore original value... */
		erts_proc_unlock(BIF_P, ERTS_PROC_LOCK_MAIN);
		erts_debug_test_node_tab_delayed_delete(msecs);
		erts_proc_lock(BIF_P, ERTS_PROC_LOCK_MAIN);
		BIF_RET(am_ok);
	    }
	}
        else if (ERTS_IS_ATOM_STR("fill_heap", BIF_ARG_1)) {
            UWord left = HeapWordsLeft(BIF_P);
            if (left > 1) {
                Eterm* hp = HAlloc(BIF_P, left);
                *hp = make_pos_bignum_header(left - 1);
            }
            if (BIF_ARG_2 == am_true) {
                FLAGS(BIF_P) |= F_NEED_FULLSWEEP;
            }
            BIF_RET(am_ok);
        }
        else if (ERTS_IS_ATOM_STR("make", BIF_ARG_1)) {
            if (ERTS_IS_ATOM_STR("magic_ref", BIF_ARG_2)) {
                Binary *bin = erts_create_magic_binary(0, empty_magic_ref_destructor);
                UWord bin_addr = (UWord) bin;
                Eterm bin_addr_term, magic_ref, res;
                Eterm *hp;
                Uint sz = ERTS_MAGIC_REF_THING_SIZE + 3;
                erts_bld_uword(NULL, &sz, bin_addr);
                hp = HAlloc(BIF_P, sz);
                bin_addr_term = erts_bld_uword(&hp, NULL, bin_addr);
                magic_ref = erts_mk_magic_ref(&hp, &BIF_P->off_heap, bin);
                res = TUPLE2(hp, magic_ref, bin_addr_term);
                BIF_RET(res);
            }
        }
        else if (ERTS_IS_ATOM_STR("binary", BIF_ARG_1)) {
            Sint64 size;
            if (term_to_Sint64(BIF_ARG_2, &size)) {
                Binary* refbin = erts_bin_drv_alloc_fnf(size);
                if (!refbin)
                    BIF_RET(am_false);
                sys_memset(refbin->orig_bytes, 0, size);
                BIF_RET(erts_build_proc_bin(&MSO(BIF_P),
                                            HAlloc(BIF_P, PROC_BIN_SIZE),
                                            refbin));
            }
        }
        else if (ERTS_IS_ATOM_STR("ets_force_trap", BIF_ARG_1)) {
#ifdef ETS_DBG_FORCE_TRAP
            erts_ets_dbg_force_trap = (BIF_ARG_2 == am_true) ? 1 : 0;
            BIF_RET(am_ok);
#else
            BIF_RET(am_notsup);
#endif
        }
        else if (ERTS_IS_ATOM_STR("ets_force_split", BIF_ARG_1)) {
            if (is_tuple(BIF_ARG_2)) {
                Eterm* tpl = tuple_val(BIF_ARG_2);

                if (erts_ets_force_split(tpl[1], tpl[2] == am_true))
                    BIF_RET(am_ok);
            }
        }
        else if (ERTS_IS_ATOM_STR("ets_debug_random_split_join", BIF_ARG_1)) {
            if (is_tuple(BIF_ARG_2)) {
                Eterm* tpl = tuple_val(BIF_ARG_2);
                if (erts_ets_debug_random_split_join(tpl[1], tpl[2] == am_true))
                    BIF_RET(am_ok);
            }
        }
        else if (ERTS_IS_ATOM_STR("mbuf", BIF_ARG_1)) {
            Uint sz = size_object(BIF_ARG_2);
            ErlHeapFragment* frag = new_message_buffer(sz);
            Eterm *hp = frag->mem;
            Eterm copy = copy_struct(BIF_ARG_2, sz, &hp, &frag->off_heap);
            frag->next = BIF_P->mbuf;
            BIF_P->mbuf = frag;
            BIF_P->mbuf_sz += sz;
            BIF_RET(copy);
        }
        else if (ERTS_IS_ATOM_STR("remove_hopefull_dflags", BIF_ARG_1)) {
            int old_val, new_val;

            switch (BIF_ARG_2) {
            case am_true: new_val = !0; break;
            case am_false: new_val = 0; break;
            default: BIF_ERROR(BIF_P, BADARG); break;
            }

            erts_proc_unlock(BIF_P, ERTS_PROC_LOCK_MAIN);
            erts_thr_progress_block();
            
            old_val = erts_dflags_test_remove_hopefull_flags;
            erts_dflags_test_remove_hopefull_flags = new_val;
            
            erts_thr_progress_unblock();
            erts_proc_lock(BIF_P, ERTS_PROC_LOCK_MAIN);

            BIF_RET(old_val ? am_true : am_false);
        }
        else if (ERTS_IS_ATOM_STR("code_write_permission", BIF_ARG_1)) {
            /*
             * Warning: This is a unsafe way of seizing the "lock"
             * as there is no automatic unlock if caller terminates.
             */
            switch(BIF_ARG_2) {
            case am_true:
                if (!erts_try_seize_code_write_permission(BIF_P)) {
                    ERTS_BIF_YIELD2(BIF_TRAP_EXPORT(BIF_erts_debug_set_internal_state_2),
                                    BIF_P, BIF_ARG_1, BIF_ARG_2);
                }
                BIF_RET(am_true);
            case am_false:
                erts_release_code_write_permission();
                BIF_RET(am_true);
            }
        }
        else if (ERTS_IS_ATOM_STR("multizero_timeout_in_timeout", BIF_ARG_1)) {
            Sint64 timeout;
            if (term_to_Sint64(BIF_ARG_2, &timeout)) {
                if (timeout < 0)
                    timeout = 0;
                erts_proc_inc_refc(BIF_P);
                erts_start_timer_callback((ErtsMonotonicTime) timeout,
                                          test_multizero_timeout_in_timeout,
                                          (void *) BIF_P);
                BIF_RET(am_ok);
            }
        }
    }

    BIF_ERROR(BIF_P, BADARG);
}

Eterm
erts_get_ethread_info(Process *c_p)
{
    Uint sz, *szp;
    Eterm res, *hp, **hpp, *end_hp = NULL;

    sz = 0;
    szp = &sz;
    hpp = NULL;

    while (1) {
	Eterm tup, list, name;
#if defined(ETHR_NATIVE_ATOMIC32_IMPL)	  \
    || defined(ETHR_NATIVE_ATOMIC64_IMPL)	\
    || defined(ETHR_NATIVE_DW_ATOMIC_IMPL)
	char buf[1024];
	int i;
	char **str;
#endif

	res = NIL;

#ifdef ETHR_X86_MEMBAR_H__

	tup = erts_bld_tuple(hpp, szp, 2,
			     erts_bld_string(hpp, szp, "sse2"),
#ifdef ETHR_X86_RUNTIME_CONF_HAVE_SSE2__
			     erts_bld_string(hpp, szp,
					     (ETHR_X86_RUNTIME_CONF_HAVE_SSE2__
					      ? "yes" : "no"))
#else
			     erts_bld_string(hpp, szp, "yes")
#endif
	    );
	res = erts_bld_cons(hpp, szp, tup, res);

	tup = erts_bld_tuple(hpp, szp, 2,
			     erts_bld_string(hpp, szp,
					     "x86"
#ifdef ARCH_64
					     "_64"
#endif
					     " OOO"),
			     erts_bld_string(hpp, szp,
#ifdef ETHR_X86_OUT_OF_ORDER
					     "yes"
#else
					     "no"
#endif
				 ));

	res = erts_bld_cons(hpp, szp, tup, res);
#endif

#ifdef ETHR_SPARC_V9_MEMBAR_H__

	tup = erts_bld_tuple(hpp, szp, 2,
			     erts_bld_string(hpp, szp, "Sparc V9"),
			     erts_bld_string(hpp, szp,
#if defined(ETHR_SPARC_TSO)
					     "TSO"
#elif defined(ETHR_SPARC_PSO)
					     "PSO"
#elif defined(ETHR_SPARC_RMO)
					     "RMO"
#else
					     "undefined"
#endif
				 ));

	res = erts_bld_cons(hpp, szp, tup, res);

#endif

#ifdef ETHR_PPC_MEMBAR_H__

	tup = erts_bld_tuple(hpp, szp, 2,
			     erts_bld_string(hpp, szp, "lwsync"),
			     erts_bld_string(hpp, szp,
#if defined(ETHR_PPC_HAVE_LWSYNC)
					     "yes"
#elif defined(ETHR_PPC_HAVE_NO_LWSYNC)
					     "no"
#elif defined(ETHR_PPC_RUNTIME_CONF_HAVE_LWSYNC__)
					     ETHR_PPC_RUNTIME_CONF_HAVE_LWSYNC__ ? "yes" : "no"
#else
					     "undefined"
#endif
				 ));

	res = erts_bld_cons(hpp, szp, tup, res);

#endif

	tup = erts_bld_tuple(hpp, szp, 2,
			     erts_bld_string(hpp, szp, "Native rw-spinlocks"),
#ifdef ETHR_NATIVE_RWSPINLOCK_IMPL
			     erts_bld_string(hpp, szp, ETHR_NATIVE_RWSPINLOCK_IMPL)
#else
			     erts_bld_string(hpp, szp, "no")
#endif
	    );
	res = erts_bld_cons(hpp, szp, tup, res);

	tup = erts_bld_tuple(hpp, szp, 2,
			     erts_bld_string(hpp, szp, "Native spinlocks"),
#ifdef ETHR_NATIVE_SPINLOCK_IMPL
			     erts_bld_string(hpp, szp, ETHR_NATIVE_SPINLOCK_IMPL)
#else
			     erts_bld_string(hpp, szp, "no")
#endif
	    );
	res = erts_bld_cons(hpp, szp, tup, res);


	list = NIL;
#ifdef ETHR_NATIVE_DW_ATOMIC_IMPL
	if (ethr_have_native_dw_atomic()) {
	    name = erts_bld_string(hpp, szp, ETHR_NATIVE_DW_ATOMIC_IMPL);
	    str = ethr_native_dw_atomic_ops();
	    for (i = 0; str[i]; i++) {
		erts_snprintf(buf, sizeof(buf), "ethr_native_dw_atomic_%s()", str[i]);
		list = erts_bld_cons(hpp, szp,
				     erts_bld_string(hpp, szp, buf),
				     list);
	    }
	    str = ethr_native_su_dw_atomic_ops();
	    for (i = 0; str[i]; i++) {
		erts_snprintf(buf, sizeof(buf), "ethr_native_su_dw_atomic_%s()", str[i]);
		list = erts_bld_cons(hpp, szp,
				     erts_bld_string(hpp, szp, buf),
				     list);
	    }
	}
	else 
#endif
	    name = erts_bld_string(hpp, szp, "no");

	tup = erts_bld_tuple(hpp, szp, 3,
			     erts_bld_string(hpp, szp, "Double word native atomics"),
			     name,
			     list);
	res = erts_bld_cons(hpp, szp, tup, res);

	list = NIL;
#ifdef ETHR_NATIVE_ATOMIC64_IMPL
	name = erts_bld_string(hpp, szp, ETHR_NATIVE_ATOMIC64_IMPL);
	str = ethr_native_atomic64_ops();
	for (i = 0; str[i]; i++) {
	    erts_snprintf(buf, sizeof(buf), "ethr_native_atomic64_%s()", str[i]);
	    list = erts_bld_cons(hpp, szp,
				 erts_bld_string(hpp, szp, buf),
				 list);
	}
#else
	name = erts_bld_string(hpp, szp, "no");
#endif
	tup = erts_bld_tuple(hpp, szp, 3,
			     erts_bld_string(hpp, szp, "64-bit native atomics"),
			     name,
			     list);
	res = erts_bld_cons(hpp, szp, tup, res);

	list = NIL;
#ifdef ETHR_NATIVE_ATOMIC32_IMPL
	name = erts_bld_string(hpp, szp, ETHR_NATIVE_ATOMIC32_IMPL);
	str = ethr_native_atomic32_ops();
	for (i = 0; str[i]; i++) {
	    erts_snprintf(buf, sizeof(buf), "ethr_native_atomic32_%s()", str[i]);
	    list = erts_bld_cons(hpp, szp,
				erts_bld_string(hpp, szp, buf),
				list);
	}
#else
	name = erts_bld_string(hpp, szp, "no");
#endif
	tup = erts_bld_tuple(hpp, szp, 3,
			     erts_bld_string(hpp, szp, "32-bit native atomics"),
			     name,
			     list);
	res = erts_bld_cons(hpp, szp, tup, res);

	if (hpp) {
	    HRelease(c_p, end_hp, *hpp)
	    return res;
	}

	hp = HAlloc(c_p, sz);
	end_hp = hp + sz;
	hpp = &hp;
	szp = NULL;
    }
}

static BIF_RETTYPE
gather_histograms_helper(Process * c_p, Eterm arg_tuple,
                         int gather(Process *, int, int, int, UWord, Eterm))
{
    SWord hist_start, hist_width, sched_id;
    int msg_count, alloc_num;
    Eterm *args;

    /* This is an internal BIF, so the error checking is mostly left to erlang
     * code. */

    ASSERT(is_tuple_arity(arg_tuple, 5));
    args = tuple_val(arg_tuple);

    for (alloc_num = ERTS_ALC_A_MIN; alloc_num <= ERTS_ALC_A_MAX; alloc_num++) {
        if(erts_is_atom_str(ERTS_ALC_A2AD(alloc_num), args[1], 0)) {
            break;
        }
    }

    if (alloc_num > ERTS_ALC_A_MAX) {
        BIF_ERROR(c_p, BADARG);
    }

    sched_id = signed_val(args[2]);
    hist_width = signed_val(args[3]);
    hist_start = signed_val(args[4]);

    if (sched_id < 0 || sched_id > erts_no_schedulers) {
        BIF_ERROR(c_p, BADARG);
    }

    msg_count = gather(c_p, alloc_num, sched_id, hist_width, hist_start, args[5]);

    BIF_RET(make_small(msg_count));
}

BIF_RETTYPE erts_internal_gather_alloc_histograms_1(BIF_ALIST_1)
{
    return gather_histograms_helper(BIF_P, BIF_ARG_1,
                                    erts_alcu_gather_alloc_histograms);
}

BIF_RETTYPE erts_internal_gather_carrier_info_1(BIF_ALIST_1)
{
    return gather_histograms_helper(BIF_P, BIF_ARG_1,
                                    erts_alcu_gather_carrier_info);
}


/* Builds a list of all functions in the given module:
 *     [{Name, Arity},...] */
static Eterm
functions_in_module(Process* p, const BeamCodeHeader* code_hdr)
{
    int i;
    Uint num_functions;
    Uint need;
    Eterm* hp;
    Eterm* hp_end;
    Eterm result = NIL;

    num_functions = code_hdr->num_functions;
    need = 5*num_functions;
    hp = HAlloc(p, need);
    hp_end = hp + need;
    for (i = num_functions-1; i >= 0 ; i--) {
        const ErtsCodeInfo* ci = code_hdr->functions[i];
        Eterm tuple;

        /*
         * If the function name is [], this entry is a stub for
         * a BIF that should be ignored.
         */
        ASSERT(is_atom(ci->mfa.function) || is_nil(ci->mfa.function));
        if (is_atom(ci->mfa.function)) {
            tuple = TUPLE2(hp, ci->mfa.function, make_small(ci->mfa.arity));
            hp += 3;

            result = CONS(hp, tuple, result);
            hp += 2;
        }
    }
    HRelease(p, hp_end, hp);
    return result;
}

/* Builds a list of all NIFs in the given module:
 *     [{Name, Arity},...] */
static Eterm
nifs_in_module(Process* p, Eterm module)
{
    Eterm nif_list, *hp;
    Module *mod;

    mod = erts_get_module(module, erts_active_code_ix());
    nif_list = NIL;

    if (mod->curr.nif != NULL) {
        int func_count, func_ix;
        ErlNifFunc *funcs;

        func_count = erts_nif_get_funcs(mod->curr.nif, &funcs);
        hp = HAlloc(p, func_count * 5);

        for (func_ix = func_count - 1; func_ix >= 0; func_ix--) {
            Eterm name, arity, pair;
            ErlNifFunc *func;

            func = &funcs[func_ix];

            name = am_atom_put(func->name, sys_strlen(func->name));
            arity = make_small(func->arity);

            pair = TUPLE2(hp, name, arity);
            hp += 3;

            nif_list = CONS(hp, pair, nif_list);
            hp += 2;
        }
    }

    return nif_list;
}

/* Returns 'true' if mod has any native compiled functions, otherwise 'false' */
static Eterm
has_native(const BeamCodeHeader *code_hdr)
{
    return am_false;
}

/* Builds a list of all functions including native addresses.
 *     [{Name,Arity,NativeAddress},...] */
static Eterm
native_addresses(Process* p, const BeamCodeHeader* code_hdr)
{
    return NIL;
}

/* Builds a list of all exported functions in the given module:
 *     [{Name, Arity},...] */
static Eterm
exported_from_module(Process* p, ErtsCodeIndex code_ix, Eterm mod)
{
    int i, num_exps;
    Eterm* hp = NULL;
    Eterm* hend = NULL;
    Eterm result = NIL;

    num_exps = export_list_size(code_ix);
    for (i = 0; i < num_exps; i++) {
        Export* ep = export_list(i,code_ix);
        
        if (ep->info.mfa.module == mod) {
            Eterm tuple;

            if (erts_is_export_trampoline_active(ep, code_ix) &&
                BeamIsOpCode(ep->trampoline.common.op, op_call_error_handler)) {
                /* There is a call to the function, but it does not exist. */ 
                continue;
            }

            if (hp == hend) {
                int need = 10 * 5;
                hp = HAlloc(p, need);
                hend = hp + need;
            }

            tuple = TUPLE2(hp, ep->info.mfa.function,
                           make_small(ep->info.mfa.arity));
            hp += 3;

            result = CONS(hp, tuple, result);
            hp += 2;
        }
    }

    HRelease(p, hend,hp);
    return result;
}

/* Returns a list of all attributes for the module. */
static Eterm
attributes_for_module(Process* p, const BeamCodeHeader* code_hdr)
{
    const byte* ext;
    Eterm result = NIL;

    ext = code_hdr->attr_ptr;
    if (ext != NULL) {
        ErtsHeapFactory factory;

        erts_factory_proc_prealloc_init(&factory, p,
                                        code_hdr->attr_size_on_heap);

        result = erts_decode_ext(&factory, &ext, 0);

        if (is_value(result)) {
            erts_factory_close(&factory);
        }
    }
    return result;
}

/* Returns a list containing compilation information. */
static Eterm
compilation_info_for_module(Process* p, const BeamCodeHeader* code_hdr)
{
    const byte* ext;
    Eterm result = NIL;

    ext = code_hdr->compile_ptr;
    if (ext != NULL) {
        ErtsHeapFactory factory;

        erts_factory_proc_prealloc_init(&factory, p,
                                        code_hdr->compile_size_on_heap);

        result = erts_decode_ext(&factory, &ext, 0);

        if (is_value(result)) {
            erts_factory_close(&factory);
        }
    }

    return result;
}

/* Returns the MD5 checksum for a module */
static Eterm
md5_of_module(Process* p, const BeamCodeHeader* code_hdr)
{
    return new_binary(p, code_hdr->md5_ptr, MD5_SIZE);
}

static Eterm
get_module_info(Process* p, ErtsCodeIndex code_ix,
                const BeamCodeHeader* code_hdr,
                Eterm module, Eterm what)
{
    if (what == am_module) {
        return module;
    } else if (what == am_md5) {
        return md5_of_module(p, code_hdr);
    } else if (what == am_exports) {
        return exported_from_module(p, code_ix, module);
    } else if (what == am_functions) {
        return functions_in_module(p, code_hdr);
    } else if (what == am_nifs) {
        return nifs_in_module(p, module);
    } else if (what == am_attributes) {
        return attributes_for_module(p, code_hdr);
    } else if (what == am_compile) {
        return compilation_info_for_module(p, code_hdr);
    } else if (what == am_native_addresses) {
        return native_addresses(p, code_hdr);
    } else if (what == am_native) {
        return has_native(code_hdr);
    }

    return THE_NON_VALUE;
}

static Eterm
module_info_0(Process* p, Eterm module)
{
    Module* modp;
    ErtsCodeIndex code_ix = erts_active_code_ix();
    const BeamCodeHeader* code_hdr;
    Eterm *hp;
    Eterm list = NIL;
    Eterm tup;

    if (is_not_atom(module)) {
	return THE_NON_VALUE;
    }

    modp = erts_get_module(module, code_ix);
    if (modp == NULL) {
	return THE_NON_VALUE;
    }

    code_hdr = modp->curr.code_hdr;
    if (code_hdr == NULL) {
        return THE_NON_VALUE;
    }

#define BUILD_INFO(What) \
    tup = get_module_info(p, code_ix, code_hdr, module, What); \
    hp = HAlloc(p, 5); \
    tup = TUPLE2(hp, What, tup); \
    hp += 3; \
    list = CONS(hp, tup, list)

    BUILD_INFO(am_md5);
    BUILD_INFO(am_compile);
    BUILD_INFO(am_attributes);
    BUILD_INFO(am_exports);
    BUILD_INFO(am_module);
#undef BUILD_INFO
    return list;
}

static Eterm
module_info_1(Process* p, Eterm module, Eterm what)
{
    Module* modp;
    ErtsCodeIndex code_ix = erts_active_code_ix();
    const BeamCodeHeader* code_hdr;

    if (is_not_atom(module)) {
        return THE_NON_VALUE;
    }

    modp = erts_get_module(module, code_ix);
    if (modp == NULL) {
        return THE_NON_VALUE;
    }

    code_hdr = modp->curr.code_hdr;
    if (code_hdr == NULL) {
        return THE_NON_VALUE;
    }

    return get_module_info(p, code_ix, code_hdr, module, what);
}

BIF_RETTYPE get_module_info_1(BIF_ALIST_1)
{
    Eterm ret = module_info_0(BIF_P, BIF_ARG_1);

    if (is_non_value(ret)) {
        BIF_ERROR(BIF_P, BADARG);
    }

    BIF_RET(ret);
}

BIF_RETTYPE get_module_info_2(BIF_ALIST_2)
{
    Eterm ret = module_info_1(BIF_P, BIF_ARG_1, BIF_ARG_2);

    if (is_non_value(ret)) {
        BIF_ERROR(BIF_P, BADARG);
    }

    BIF_RET(ret);
}

#ifdef ERTS_ENABLE_LOCK_COUNT

typedef struct {
    /* info->location_count may increase between size calculation and term
     * building, so we cap it at the value sampled in lcnt_build_result_vector.
     *
     * Shrinking is safe though. */
    int max_location_count;
    erts_lcnt_lock_info_t *info;
} lcnt_sample_t;

typedef struct lcnt_sample_vector_ {
    lcnt_sample_t *elements;
    size_t size;
} lcnt_sample_vector_t;

static lcnt_sample_vector_t lcnt_build_sample_vector(erts_lcnt_lock_info_list_t *list) {
    erts_lcnt_lock_info_t *iterator;
    lcnt_sample_vector_t result;
    size_t allocated_entries;

    allocated_entries = 64;
    result.size = 0;

    result.elements = erts_alloc(ERTS_ALC_T_LCNT_VECTOR,
        allocated_entries * sizeof(lcnt_sample_t));

    iterator = NULL;
    while(erts_lcnt_iterate_list(list, &iterator)) {
        erts_lcnt_retain_lock_info(iterator);

        result.elements[result.size].max_location_count = iterator->location_count;
        result.elements[result.size].info = iterator;

        result.size++;

        if(result.size >= allocated_entries) {
            allocated_entries *= 2;

            result.elements = erts_realloc(ERTS_ALC_T_LCNT_VECTOR, result.elements,
                allocated_entries * sizeof(lcnt_sample_t));
        }
    }

    return result;
}

static void lcnt_destroy_sample_vector(lcnt_sample_vector_t *vector) {
    size_t i;

    for(i = 0; i < vector->size; i++) {
        erts_lcnt_release_lock_info(vector->elements[i].info);
    }

    erts_free(ERTS_ALC_T_LCNT_VECTOR, vector->elements);
}

/* The size of an integer is not guaranteed to be constant since we're walking
 * over live data, and may cross over into bignum territory between size calc
 * and the actual build. This takes care of that through always assuming the
 * worst, but needs to be fixed up with HRelease once the final term has been
 * built. */
static ERTS_INLINE Eterm bld_unstable_uint64(Uint **hpp, Uint *szp, Uint64 ui) {
    Eterm res = THE_NON_VALUE;

    if(szp) {
        *szp += ERTS_UINT64_HEAP_SIZE(~((Uint64) 0));
    }

    if(hpp) {
        if (IS_USMALL(0, ui)) {
            res = make_small(ui);
        } else {
            res = erts_uint64_to_big(ui, hpp);
        }
    }

    return res;
}

static Eterm lcnt_build_lock_stats_term(Eterm **hpp, Uint *szp, erts_lcnt_lock_stats_t *stats, Eterm res) {
    unsigned int  i;
    const char *file;

    Eterm af, uil;
    Eterm uit, uic;
    Eterm uits, uitns, uitn;
    Eterm tt, tstat, tloc, t;
    Eterm thist, vhist[ERTS_LCNT_HISTOGRAM_SLOT_SIZE];

    /* term:
     *  [{{file, line},
         {tries, colls, {seconds, nanoseconds, n_blocks}},
     *   { .. histogram .. }] */

    file = stats->file ? stats->file : "undefined";

    af    = erts_atom_put((byte *)file, sys_strlen(file), ERTS_ATOM_ENC_LATIN1, 1);
    uil   = erts_bld_uint( hpp, szp, stats->line);
    tloc  = erts_bld_tuple(hpp, szp, 2, af, uil);

    uit   = bld_unstable_uint64(hpp, szp, (Uint)ethr_atomic_read(&stats->attempts));
    uic   = bld_unstable_uint64(hpp, szp, (Uint)ethr_atomic_read(&stats->collisions));

    uits  = bld_unstable_uint64(hpp, szp, stats->total_time_waited.s);
    uitns = bld_unstable_uint64(hpp, szp, stats->total_time_waited.ns);
    uitn  = bld_unstable_uint64(hpp, szp, stats->times_waited);
    tt    = erts_bld_tuple(hpp, szp, 3, uits, uitns, uitn);

    tstat = erts_bld_tuple(hpp, szp, 3, uit, uic, tt);

    for(i = 0; i < ERTS_LCNT_HISTOGRAM_SLOT_SIZE; i++) {
        vhist[i] = bld_unstable_uint64(hpp, szp, stats->wait_time_histogram.ns[i]);
    }

    thist  = erts_bld_tuplev(hpp, szp, ERTS_LCNT_HISTOGRAM_SLOT_SIZE, vhist);

    t   = erts_bld_tuple(hpp, szp, 3, tloc, tstat, thist);
    res = erts_bld_cons( hpp, szp, t, res);

    return res;
}

static Eterm lcnt_pretty_print_lock_id(erts_lcnt_lock_info_t *info) {
    Eterm id = info->id;

    if((info->flags & ERTS_LOCK_FLAGS_MASK_TYPE) == ERTS_LOCK_FLAGS_TYPE_PROCLOCK) {
        /* Use registered names as id's for process locks if available. Thread
         * progress is delayed since we may be running on a dirty scheduler. */
        ErtsThrPrgrDelayHandle delay_handle;
        Process *process;

        delay_handle = erts_thr_progress_unmanaged_delay();

        process = erts_proc_lookup(info->id);
        if (process && process->common.u.alive.reg) {
            id = process->common.u.alive.reg->name;
        }

        erts_thr_progress_unmanaged_continue(delay_handle);
    } else if(info->flags & ERTS_LOCK_FLAGS_CATEGORY_ALLOCATOR) {
        if(is_small(id) && !sys_strcmp(info->name, "alcu_allocator")) {
            const char *name = (const char*)ERTS_ALC_A2AD(signed_val(id));
            id = erts_atom_put((byte*)name, sys_strlen(name), ERTS_ATOM_ENC_LATIN1, 1);
        }
    }

    return id;
}

static Eterm lcnt_build_lock_term(Eterm **hpp, Uint *szp, lcnt_sample_t *sample, Eterm res) {
    erts_lcnt_lock_info_t *info = sample->info;

    Eterm name, type, id, stats = NIL, t;
    const char *lock_desc;
    int i;

    /* term: [{name, id, type, stats()}] */

    ASSERT(info->name);
    
    lock_desc = erts_lock_flags_get_type_name(info->flags);

    type  = erts_atom_put((byte*)lock_desc, sys_strlen(lock_desc), ERTS_ATOM_ENC_LATIN1, 1);
    name  = erts_atom_put((byte*)info->name, sys_strlen(info->name), ERTS_ATOM_ENC_LATIN1, 1);

    /* Only attempt to resolve ids when actually emitting the term. This ought
     * to be safe since all immediates are the same size. */
    if(hpp != NULL) {
        id = lcnt_pretty_print_lock_id(info);
    } else {
        id = NIL;
    }

    for(i = 0; i < MIN(info->location_count, sample->max_location_count); i++) {
        stats = lcnt_build_lock_stats_term(hpp, szp, &(info->location_stats[i]), stats);
    }

    t   = erts_bld_tuple(hpp, szp, 4, name, id, type, stats);
    res = erts_bld_cons(hpp, szp, t, res);

    return res;
}

static Eterm lcnt_build_result_term(Eterm **hpp, Uint *szp, erts_lcnt_time_t *duration,
                                    lcnt_sample_vector_t *current_locks,
                                    lcnt_sample_vector_t *deleted_locks, Eterm res) {
    const char *str_duration = "duration";
    const char *str_locks = "locks";

    Eterm dts, dtns, tdt, adur, tdur, aloc, lloc = NIL, tloc;
    size_t i;

    /* term: [{'duration', {seconds, nanoseconds}}, {'locks', locks()}] */

    /* duration tuple */ 
    dts  = bld_unstable_uint64(hpp, szp, duration->s);
    dtns = bld_unstable_uint64(hpp, szp, duration->ns);
    tdt  = erts_bld_tuple(hpp, szp, 2, dts, dtns);

    adur = erts_atom_put((byte *)str_duration, sys_strlen(str_duration), ERTS_ATOM_ENC_LATIN1, 1);
    tdur = erts_bld_tuple(hpp, szp, 2, adur, tdt);
   
    /* lock tuple */
    aloc = erts_atom_put((byte *)str_locks, sys_strlen(str_locks), ERTS_ATOM_ENC_LATIN1, 1);

    for(i = 0; i < current_locks->size; i++) {
        lloc = lcnt_build_lock_term(hpp, szp, &current_locks->elements[i], lloc);
    }

    for(i = 0; i < deleted_locks->size; i++) {
        lloc = lcnt_build_lock_term(hpp, szp, &deleted_locks->elements[i], lloc);
    }

    tloc = erts_bld_tuple(hpp, szp, 2, aloc, lloc);

    res  = erts_bld_cons(hpp, szp, tloc, res);
    res  = erts_bld_cons(hpp, szp, tdur, res);

    return res;
}

static struct {
    const char *name;
    erts_lock_flags_t flag;
} lcnt_category_map[] = {
        {"allocator", ERTS_LOCK_FLAGS_CATEGORY_ALLOCATOR},
        {"db", ERTS_LOCK_FLAGS_CATEGORY_DB},
        {"debug", ERTS_LOCK_FLAGS_CATEGORY_DEBUG},
        {"distribution", ERTS_LOCK_FLAGS_CATEGORY_DISTRIBUTION},
        {"generic", ERTS_LOCK_FLAGS_CATEGORY_GENERIC},
        {"io", ERTS_LOCK_FLAGS_CATEGORY_IO},
        {"process", ERTS_LOCK_FLAGS_CATEGORY_PROCESS},
        {"scheduler", ERTS_LOCK_FLAGS_CATEGORY_SCHEDULER},
        {NULL, 0}
    };

static erts_lock_flags_t lcnt_atom_to_lock_category(Eterm atom) {
    int i = 0;

    for(i = 0; lcnt_category_map[i].name != NULL; i++) {
        if(erts_is_atom_str(lcnt_category_map[i].name, atom, 0)) {
            return lcnt_category_map[i].flag;
        }
    }

    return 0;
}

static Eterm lcnt_build_category_list(Eterm **hpp, Uint *szp, erts_lock_flags_t mask) {
    Eterm res;
    int i;

    res = NIL;

    for(i = 0; lcnt_category_map[i].name != NULL; i++) {
        if(mask & lcnt_category_map[i].flag) {
            Eterm category = erts_atom_put((byte*)lcnt_category_map[i].name,
                                           sys_strlen(lcnt_category_map[i].name),
                                           ERTS_ATOM_ENC_UTF8, 0);

            res = erts_bld_cons(hpp, szp, category, res);
        }
    }

    return res;
}

#endif

BIF_RETTYPE erts_debug_lcnt_clear_0(BIF_ALIST_0)
{
#ifndef ERTS_ENABLE_LOCK_COUNT
    BIF_RET(am_error);
#else
    erts_lcnt_clear_counters();

    BIF_RET(am_ok);
#endif
}

BIF_RETTYPE erts_debug_lcnt_collect_0(BIF_ALIST_0)
{
#ifndef ERTS_ENABLE_LOCK_COUNT
    BIF_RET(am_error);
#else
    lcnt_sample_vector_t current_locks, deleted_locks;
    erts_lcnt_data_t data;

    Eterm *term_heap_start, *term_heap_end;
    Uint term_heap_size = 0;
    Eterm result;

    data = erts_lcnt_get_data();

    current_locks = lcnt_build_sample_vector(data.current_locks);
    deleted_locks = lcnt_build_sample_vector(data.deleted_locks);

    lcnt_build_result_term(NULL, &term_heap_size, &data.duration,
        &current_locks, &deleted_locks, NIL);

    term_heap_start = HAlloc(BIF_P, term_heap_size);
    term_heap_end = term_heap_start;

    result = lcnt_build_result_term(&term_heap_end, NULL,
        &data.duration, &current_locks, &deleted_locks, NIL);

    HRelease(BIF_P, term_heap_start + term_heap_size, term_heap_end);

    lcnt_destroy_sample_vector(&current_locks);
    lcnt_destroy_sample_vector(&deleted_locks);

    BIF_RET(result);
#endif
}

BIF_RETTYPE erts_debug_lcnt_control_1(BIF_ALIST_1)
{
#ifdef ERTS_ENABLE_LOCK_COUNT
    if(ERTS_IS_ATOM_STR("mask", BIF_ARG_1)) {
        erts_lock_flags_t mask;
        Eterm *term_heap_block;
        Uint term_heap_size;

        mask = erts_lcnt_get_category_mask();
        term_heap_size = 0;

        lcnt_build_category_list(NULL, &term_heap_size, mask);

        term_heap_block = HAlloc(BIF_P, term_heap_size);

        BIF_RET(lcnt_build_category_list(&term_heap_block, NULL, mask));
    } else if(ERTS_IS_ATOM_STR("copy_save", BIF_ARG_1)) {
        if(erts_lcnt_get_preserve_info()) {
            BIF_RET(am_true);
        }

        BIF_RET(am_false);
    }
#endif
    BIF_ERROR(BIF_P, BADARG);
}

BIF_RETTYPE erts_debug_lcnt_control_2(BIF_ALIST_2)
{
#ifdef ERTS_ENABLE_LOCK_COUNT
    if(ERTS_IS_ATOM_STR("mask", BIF_ARG_1)) {
        erts_lock_flags_t category_mask = 0;
        Eterm categories = BIF_ARG_2;

        if(!(is_list(categories) || is_nil(categories))) {
            BIF_ERROR(BIF_P, BADARG);
        }

        while(is_list(categories)) {
            Eterm *cell = list_val(categories);
            erts_lock_flags_t category;

            category = lcnt_atom_to_lock_category(CAR(cell));

            if(!category) {
                Eterm *hp = HAlloc(BIF_P, 4);

                BIF_RET(TUPLE3(hp, am_error, am_badarg, CAR(cell)));
            }

            category_mask |= category;
            categories = CDR(cell);
        }

        erts_lcnt_set_category_mask(category_mask);

        BIF_RET(am_ok);
    } else if(BIF_ARG_2 == am_true || BIF_ARG_2 == am_false) {
        int enabled = (BIF_ARG_2 == am_true);

        if(ERTS_IS_ATOM_STR("copy_save", BIF_ARG_1)) {
            erts_lcnt_set_preserve_info(enabled);

            BIF_RET(am_ok);
        }
    }
#endif
    BIF_ERROR(BIF_P, BADARG);
}

static void os_info_init(void)
{
    Eterm type = erts_atom_put((byte *) os_type, sys_strlen(os_type), ERTS_ATOM_ENC_LATIN1, 1);
    Eterm flav;
    int major, minor, build;
    char* buf = erts_alloc(ERTS_ALC_T_TMP, 1024); /* More than enough */
    Eterm* hp;
    Eterm tuple;

    os_flavor(buf, 1024);
    flav = erts_atom_put((byte *) buf, sys_strlen(buf), ERTS_ATOM_ENC_LATIN1, 1);
    erts_free(ERTS_ALC_T_TMP, (void *) buf);
    hp = erts_alloc_global_literal(ERTS_LIT_OS_TYPE, 3);
    tuple = TUPLE2(hp, type, flav);
    erts_register_global_literal(ERTS_LIT_OS_TYPE, tuple);

    hp = erts_alloc_global_literal(ERTS_LIT_OS_VERSION, 4);
    os_version(&major, &minor, &build);
    tuple = TUPLE3(hp,
                   make_small(major),
                   make_small(minor),
                   make_small(build));
    erts_register_global_literal(ERTS_LIT_OS_VERSION, tuple);
}

void
erts_bif_info_init(void)
{
    erts_atomic_init_nob(&available_internal_state, 0);

    alloc_info_trap = erts_export_put(am_erlang, am_alloc_info, 1);
    alloc_sizes_trap = erts_export_put(am_erlang, am_alloc_sizes, 1);
    gather_sched_wall_time_res_trap
	= erts_export_put(am_erts_internal, am_gather_sched_wall_time_result, 1);
    gather_gc_info_res_trap
	= erts_export_put(am_erlang, am_gather_gc_info_result, 1);
    gather_io_bytes_trap
	= erts_export_put(am_erts_internal, am_gather_io_bytes, 2);
    gather_msacc_res_trap
	= erts_export_put(am_erts_internal, am_gather_microstate_accounting_result, 2);
    gather_system_check_res_trap
	= erts_export_put(am_erts_internal, am_gather_system_check_result, 1);

    is_process_alive_trap = erts_export_put(am_erts_internal, am_is_process_alive, 1);
    
    get_internal_state_blocked = erts_export_put(am_erts_internal,
                                                 am_get_internal_state_blocked,
                                                 1);


    process_info_init();
    os_info_init();
}
