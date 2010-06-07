/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2002-2010. All Rights Reserved.
 *
 * The contents of this file are subject to the Erlang Public License,
 * Version 1.1, (the "License"); you may not use this file except in
 * compliance with the License. You should have received a copy of the
 * Erlang Public License along with this software. If not, it can be
 * retrieved online at http://www.erlang.org/.
 *
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
 * the License for the specific language governing rights and limitations
 * under the License.
 *
 * %CopyrightEnd%
 */


/*
 * Description:	Management of memory allocators.
 *
 * Author: 	Rickard Green
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif
#define ERTS_ALLOC_C__
#define ERTS_ALC_INTERNAL__
#include "sys.h"
#define ERL_THREADS_EMU_INTERNAL__
#include "erl_threads.h"
#include "global.h"
#include "erl_db.h"
#include "erl_binary.h"
#include "erl_bits.h"
#include "erl_instrument.h"
#include "erl_mseg.h"
#ifdef ELIB_ALLOC_IS_CLIB
#include "erl_version.h"
#endif
#include "erl_monitors.h"
#include "erl_bif_timer.h"
#if defined(ERTS_ALC_T_DRV_SEL_D_STATE) || defined(ERTS_ALC_T_DRV_EV_D_STATE)
#include "erl_check_io.h"
#endif

#define GET_ERL_GF_ALLOC_IMPL
#include "erl_goodfit_alloc.h"
#define GET_ERL_BF_ALLOC_IMPL
#include "erl_bestfit_alloc.h"
#define GET_ERL_AF_ALLOC_IMPL
#include "erl_afit_alloc.h"

#define ERTS_ALC_DEFAULT_MAX_THR_PREF 16

#if defined(SMALL_MEMORY) || defined(PURIFY) || defined(VALGRIND)
#define AU_ALLOC_DEFAULT_ENABLE(X)	0
#else
#define AU_ALLOC_DEFAULT_ENABLE(X)	(X)
#endif

#ifdef DEBUG
static Uint install_debug_functions(void);
#endif
extern void elib_ensure_initialized(void);

ErtsAllocatorFunctions_t erts_allctrs[ERTS_ALC_A_MAX+1];
ErtsAllocatorInfo_t erts_allctrs_info[ERTS_ALC_A_MAX+1];
ErtsAllocatorThrSpec_t erts_allctr_thr_spec[ERTS_ALC_A_MAX+1];

#define ERTS_MIN(A, B) ((A) < (B) ? (A) : (B))
#define ERTS_MAX(A, B) ((A) > (B) ? (A) : (B))

typedef union {
    GFAllctr_t gfa;
    char align_gfa[ERTS_ALC_CACHE_LINE_ALIGN_SIZE(sizeof(GFAllctr_t))];
    BFAllctr_t bfa;
    char align_bfa[ERTS_ALC_CACHE_LINE_ALIGN_SIZE(sizeof(BFAllctr_t))];
    AFAllctr_t afa;
    char align_afa[ERTS_ALC_CACHE_LINE_ALIGN_SIZE(sizeof(AFAllctr_t))];
} ErtsAllocatorState_t;

static ErtsAllocatorState_t sl_alloc_state;
static ErtsAllocatorState_t std_alloc_state;
static ErtsAllocatorState_t ll_alloc_state;
static ErtsAllocatorState_t temp_alloc_state;
static ErtsAllocatorState_t eheap_alloc_state;
static ErtsAllocatorState_t binary_alloc_state;
static ErtsAllocatorState_t ets_alloc_state;
static ErtsAllocatorState_t driver_alloc_state;

ErtsAlcType_t erts_fix_core_allocator_ix;
#ifdef ERTS_ALC_N_MIN_A_FIXED_SIZE
static void *(*fix_core_allocator)(ErtsAlcType_t, void *, Uint);
static void *fix_core_extra;
static void *fix_core_alloc(Uint size)
{
    void *res;
    res = (*fix_core_allocator)(ERTS_ALC_T_UNDEF, fix_core_extra, size);
    if (erts_mtrace_enabled)
	erts_mtrace_crr_alloc(res,
			      ERTS_ALC_A_FIXED_SIZE,
			      erts_fix_core_allocator_ix,
			      size);
    return res;
}
#endif

enum allctr_type {
    GOODFIT,
    BESTFIT,
    AFIT
};

struct au_init {
    int enable;
    int thr_spec;
    enum allctr_type	atype;
    struct {
	AllctrInit_t	util;
	GFAllctrInit_t	gf;
	BFAllctrInit_t	bf;
	AFAllctrInit_t	af;
    } init;
    struct {
	int mmbcs;
	int lmbcs;
	int smbcs;
	int mmmbc;
    } default_;
};

#define DEFAULT_ALLCTR_INIT {		\
    ERTS_DEFAULT_ALLCTR_INIT,		\
    ERTS_DEFAULT_GF_ALLCTR_INIT,	\
    ERTS_DEFAULT_BF_ALLCTR_INIT,	\
    ERTS_DEFAULT_AF_ALLCTR_INIT		\
}

typedef struct {
    int erts_alloc_config;
#if HAVE_ERTS_MSEG
    ErtsMsegInit_t mseg;
#endif
    int trim_threshold;
    int top_pad;
    AlcUInit_t alloc_util;
    struct {
	int stat;
	int map;
	char *mtrace;
	char *nodename;
    } instr;
    struct au_init sl_alloc;
    struct au_init std_alloc;
    struct au_init ll_alloc;
    struct au_init temp_alloc;
    struct au_init eheap_alloc;
    struct au_init binary_alloc;
    struct au_init ets_alloc;
    struct au_init driver_alloc;
} erts_alc_hndl_args_init_t;

#define ERTS_AU_INIT__ {0, 0, GOODFIT, DEFAULT_ALLCTR_INIT, {1,1,1,1}}

#define SET_DEFAULT_ALLOC_OPTS(IP)					\
do {									\
    struct au_init aui__ = ERTS_AU_INIT__;				\
    sys_memcpy((void *) (IP), (void *) &aui__, sizeof(struct au_init));	\
} while (0)

static void
set_default_sl_alloc_opts(struct au_init *ip)
{
    SET_DEFAULT_ALLOC_OPTS(ip);
    ip->enable			= AU_ALLOC_DEFAULT_ENABLE(1);
    ip->thr_spec		= 1;
    ip->atype			= GOODFIT;
    ip->init.util.name_prefix	= "sl_";
    ip->init.util.mmmbc		= 5;
    ip->init.util.alloc_no	= ERTS_ALC_A_SHORT_LIVED;
#ifndef SMALL_MEMORY
    ip->init.util.mmbcs 	= 128*1024; /* Main carrier size */
#else
    ip->init.util.mmbcs 	= 32*1024; /* Main carrier size */
#endif
    ip->init.util.ts 		= ERTS_ALC_MTA_SHORT_LIVED;
    ip->init.util.rsbcst	= 80;
}

static void
set_default_std_alloc_opts(struct au_init *ip)
{
    SET_DEFAULT_ALLOC_OPTS(ip);
    ip->enable			= AU_ALLOC_DEFAULT_ENABLE(1);
    ip->thr_spec		= 1;
    ip->atype			= BESTFIT;
    ip->init.util.name_prefix	= "std_";
    ip->init.util.mmmbc		= 5;
    ip->init.util.alloc_no	= ERTS_ALC_A_STANDARD;
#ifndef SMALL_MEMORY
    ip->init.util.mmbcs 	= 128*1024; /* Main carrier size */
#else
    ip->init.util.mmbcs 	= 32*1024; /* Main carrier size */
#endif
    ip->init.util.ts 		= ERTS_ALC_MTA_STANDARD;
}

static void
set_default_ll_alloc_opts(struct au_init *ip)
{
    SET_DEFAULT_ALLOC_OPTS(ip);
    ip->enable			= AU_ALLOC_DEFAULT_ENABLE(1);
    ip->thr_spec		= 0;
    ip->atype			= BESTFIT;
    ip->init.bf.ao		= 1;
    ip->init.util.ramv		= 0;
    ip->init.util.mmsbc		= 0;
    ip->init.util.mmmbc		= 0;
    ip->init.util.sbct		= ~((UWord) 0);
    ip->init.util.name_prefix	= "ll_";
    ip->init.util.alloc_no	= ERTS_ALC_A_LONG_LIVED;
#ifndef SMALL_MEMORY
    ip->init.util.mmbcs 	= 2*1024*1024; /* Main carrier size */
#else
    ip->init.util.mmbcs 	= 1*1024*1024; /* Main carrier size */
#endif
    ip->init.util.ts 		= ERTS_ALC_MTA_LONG_LIVED;
    ip->init.util.asbcst	= 0;
    ip->init.util.rsbcst	= 0;
    ip->init.util.rsbcmt	= 0;
    ip->init.util.rmbcmt	= 0;
}

static void
set_default_temp_alloc_opts(struct au_init *ip)
{
    SET_DEFAULT_ALLOC_OPTS(ip);
    ip->enable			= AU_ALLOC_DEFAULT_ENABLE(1);
    ip->thr_spec		= 1;
    ip->atype			= AFIT;
    ip->init.util.name_prefix	= "temp_";
    ip->init.util.alloc_no	= ERTS_ALC_A_TEMPORARY;
#ifndef SMALL_MEMORY
    ip->init.util.mmbcs 	= 128*1024; /* Main carrier size */
#else
    ip->init.util.mmbcs 	= 32*1024; /* Main carrier size */
#endif
    ip->init.util.ts 		= ERTS_ALC_MTA_TEMPORARY;
    ip->init.util.rsbcst	= 90;
    ip->init.util.rmbcmt	= 100;
}

static void
set_default_eheap_alloc_opts(struct au_init *ip)
{
    SET_DEFAULT_ALLOC_OPTS(ip);
    ip->enable			= AU_ALLOC_DEFAULT_ENABLE(1);
    ip->thr_spec		= 1;
    ip->atype			= GOODFIT;
    ip->init.util.mmmbc		= 100;
    ip->init.util.name_prefix	= "eheap_";
    ip->init.util.alloc_no	= ERTS_ALC_A_EHEAP;
#ifndef SMALL_MEMORY
    ip->init.util.mmbcs 	= 512*1024; /* Main carrier size */
#else
    ip->init.util.mmbcs 	= 256*1024; /* Main carrier size */
#endif
    ip->init.util.ts 		= ERTS_ALC_MTA_EHEAP;
    ip->init.util.rsbcst	= 50;
}

static void
set_default_binary_alloc_opts(struct au_init *ip)
{
    SET_DEFAULT_ALLOC_OPTS(ip);
    ip->enable			= AU_ALLOC_DEFAULT_ENABLE(1);
    ip->thr_spec		= 1;
    ip->atype			= BESTFIT;
    ip->init.util.mmmbc		= 50;
    ip->init.util.name_prefix	= "binary_";
    ip->init.util.alloc_no	= ERTS_ALC_A_BINARY;
#ifndef SMALL_MEMORY
    ip->init.util.mmbcs 	= 128*1024; /* Main carrier size */
#else
    ip->init.util.mmbcs 	= 32*1024; /* Main carrier size */
#endif
    ip->init.util.ts 		= ERTS_ALC_MTA_BINARY;
}

static void
set_default_ets_alloc_opts(struct au_init *ip)
{
    SET_DEFAULT_ALLOC_OPTS(ip);
    ip->enable			= AU_ALLOC_DEFAULT_ENABLE(1);
    ip->thr_spec		= 1;
    ip->atype			= BESTFIT;
    ip->init.util.mmmbc		= 100;
    ip->init.util.name_prefix	= "ets_";
    ip->init.util.alloc_no	= ERTS_ALC_A_ETS;
#ifndef SMALL_MEMORY
    ip->init.util.mmbcs 	= 128*1024; /* Main carrier size */
#else
    ip->init.util.mmbcs 	= 32*1024; /* Main carrier size */
#endif
    ip->init.util.ts 		= ERTS_ALC_MTA_ETS;
}

static void
set_default_driver_alloc_opts(struct au_init *ip)
{
    SET_DEFAULT_ALLOC_OPTS(ip);
    ip->enable			= AU_ALLOC_DEFAULT_ENABLE(1);
    ip->thr_spec		= 1;
    ip->atype			= BESTFIT;
    ip->init.util.name_prefix	= "driver_";
    ip->init.util.alloc_no	= ERTS_ALC_A_DRIVER;
#ifndef SMALL_MEMORY
    ip->init.util.mmbcs 	= 128*1024; /* Main carrier size */
#else
    ip->init.util.mmbcs 	= 32*1024; /* Main carrier size */
#endif
    ip->init.util.ts 		= ERTS_ALC_MTA_DRIVER;
}

#ifdef ERTS_SMP

static void
adjust_tpref(struct au_init *ip, int no_sched)
{
    if (ip->thr_spec) {
	Uint allocs;
	if (ip->thr_spec < 0) {/* User specified amount */
	    allocs = abs(ip->thr_spec);
	    if (allocs > no_sched)
		allocs = no_sched;
	}
	else if (no_sched > ERTS_ALC_DEFAULT_MAX_THR_PREF)
	    allocs = ERTS_ALC_DEFAULT_MAX_THR_PREF;
	else 
	    allocs = no_sched;
	if (allocs <= 1)
	    ip->thr_spec = 0;
	else {
	    ip->thr_spec = (int) allocs;
	    ip->thr_spec *= -1; /* thread preferred */

	    /* If default ... */

	    /* ... shrink main multi-block carrier size */
	    if (ip->default_.mmbcs)
		ip->init.util.mmbcs /= ERTS_MIN(4, allocs);
	    /* ... shrink largest multi-block carrier size */
	    if (ip->default_.lmbcs)
		ip->init.util.lmbcs /= ERTS_MIN(2, allocs);
	    /* ... shrink smallest multi-block carrier size */
	    if (ip->default_.smbcs)
		ip->init.util.smbcs /= ERTS_MIN(4, allocs);
	    /* ... and more than three allocators shrink
	       max mseg multi-block carriers */
	    if (ip->default_.mmmbc && allocs > 2) {
		ip->init.util.mmmbc /= ERTS_MIN(4, allocs - 1);
		if (ip->init.util.mmmbc < 3)
		    ip->init.util.mmmbc = 3;
	    }
	}
    }
}

#endif

static void handle_args(int *, char **, erts_alc_hndl_args_init_t *);

static void
set_au_allocator(ErtsAlcType_t alctr_n, struct au_init *init);

static void
start_au_allocator(ErtsAlcType_t alctr_n,
		   struct au_init *init,
		   ErtsAllocatorState_t *state);

static void
refuse_af_strategy(struct au_init *init)
{
    if (init->atype == AFIT)
	init->atype = GOODFIT;
}

static void init_thr_ix(int static_ixs);

void
erts_alloc_init(int *argc, char **argv, ErtsAllocInitOpts *eaiop)
{
    UWord extra_block_size = 0;
    int i;
    erts_alc_hndl_args_init_t init = {
	0,
#if HAVE_ERTS_MSEG
	ERTS_MSEG_INIT_DEFAULT_INITIALIZER,
#endif
	ERTS_DEFAULT_TRIM_THRESHOLD,
	ERTS_DEFAULT_TOP_PAD,
	ERTS_DEFAULT_ALCU_INIT
    };

    erts_sys_alloc_init();
    init_thr_ix(erts_no_schedulers);
    erts_init_utils_mem();

    set_default_sl_alloc_opts(&init.sl_alloc);
    set_default_std_alloc_opts(&init.std_alloc);
    set_default_ll_alloc_opts(&init.ll_alloc);
    set_default_temp_alloc_opts(&init.temp_alloc);
    set_default_eheap_alloc_opts(&init.eheap_alloc);
    set_default_binary_alloc_opts(&init.binary_alloc);
    set_default_ets_alloc_opts(&init.ets_alloc);
    set_default_driver_alloc_opts(&init.driver_alloc);

    if (argc && argv)
	handle_args(argc, argv, &init);

    if (erts_no_schedulers <= 1) {
	init.sl_alloc.thr_spec = 0;
	init.std_alloc.thr_spec = 0;
	init.ll_alloc.thr_spec = 0;
	init.eheap_alloc.thr_spec = 0;
	init.binary_alloc.thr_spec = 0;
	init.ets_alloc.thr_spec = 0;
	init.driver_alloc.thr_spec = 0;
    }

    if (init.erts_alloc_config) {
	/* Adjust flags that erts_alloc_config won't like */
	init.temp_alloc.thr_spec = 0;
	init.sl_alloc.thr_spec = 0;
	init.std_alloc.thr_spec = 0;
	init.ll_alloc.thr_spec = 0;
	init.eheap_alloc.thr_spec = 0;
	init.binary_alloc.thr_spec = 0;
	init.ets_alloc.thr_spec = 0;
	init.driver_alloc.thr_spec = 0;
    }

#ifdef ERTS_SMP
    /* Only temp_alloc can use thread specific interface */
    if (init.temp_alloc.thr_spec)
	init.temp_alloc.thr_spec = erts_no_schedulers;

    /* Others must use thread preferred interface */
    adjust_tpref(&init.sl_alloc, erts_no_schedulers);
    adjust_tpref(&init.std_alloc, erts_no_schedulers);
    adjust_tpref(&init.ll_alloc, erts_no_schedulers);
    adjust_tpref(&init.eheap_alloc, erts_no_schedulers);
    adjust_tpref(&init.binary_alloc, erts_no_schedulers);
    adjust_tpref(&init.ets_alloc, erts_no_schedulers);
    adjust_tpref(&init.driver_alloc, erts_no_schedulers);

#else
    /* No thread specific if not smp */
    init.temp_alloc.thr_spec = 0;
#endif

    /*
     * The following allocators cannot be run with afit strategy.
     * Make sure they don't...
     */
    refuse_af_strategy(&init.sl_alloc);
    refuse_af_strategy(&init.std_alloc);
    refuse_af_strategy(&init.ll_alloc);
    refuse_af_strategy(&init.eheap_alloc);
    refuse_af_strategy(&init.binary_alloc);
    refuse_af_strategy(&init.ets_alloc);
    refuse_af_strategy(&init.driver_alloc);

#ifdef ERTS_SMP 
    if (!init.temp_alloc.thr_spec)
	refuse_af_strategy(&init.temp_alloc);
#endif

    erts_mtrace_pre_init();
#if HAVE_ERTS_MSEG
    erts_mseg_init(&init.mseg);
#endif
    erts_alcu_init(&init.alloc_util);
    erts_afalc_init();
    erts_bfalc_init();
    erts_gfalc_init();

    for (i = ERTS_ALC_A_MIN; i <= ERTS_ALC_A_MAX; i++) {
	erts_allctrs[i].alloc		= NULL;
	erts_allctrs[i].realloc		= NULL;
	erts_allctrs[i].free		= NULL;
	erts_allctrs[i].extra		= NULL;
	erts_allctrs_info[i].alloc_util	= 0;
	erts_allctrs_info[i].enabled	= 0;
	erts_allctrs_info[i].thr_spec	= 0;
	erts_allctrs_info[i].extra	= NULL;
    }

#ifdef ERTS_ALC_N_MIN_A_FIXED_SIZE
#if !defined(PURIFY) && !defined(VALGRIND)
    erts_allctrs[ERTS_ALC_A_FIXED_SIZE].alloc		= erts_fix_alloc;
    erts_allctrs[ERTS_ALC_A_FIXED_SIZE].realloc		= erts_fix_realloc;
    erts_allctrs[ERTS_ALC_A_FIXED_SIZE].free		= erts_fix_free;
    erts_allctrs_info[ERTS_ALC_A_FIXED_SIZE].enabled	= 1;
#else
    erts_allctrs[ERTS_ALC_A_FIXED_SIZE].alloc		= erts_sys_alloc;
    erts_allctrs[ERTS_ALC_A_FIXED_SIZE].realloc		= erts_sys_realloc;
    erts_allctrs[ERTS_ALC_A_FIXED_SIZE].free		= erts_sys_free;
    erts_allctrs_info[ERTS_ALC_A_FIXED_SIZE].enabled	= 0;
#endif
#endif

    erts_allctrs[ERTS_ALC_A_SYSTEM].alloc		= erts_sys_alloc;
    erts_allctrs[ERTS_ALC_A_SYSTEM].realloc		= erts_sys_realloc;
    erts_allctrs[ERTS_ALC_A_SYSTEM].free		= erts_sys_free;
    erts_allctrs_info[ERTS_ALC_A_SYSTEM].enabled	= 1;

    set_au_allocator(ERTS_ALC_A_TEMPORARY, &init.temp_alloc);
    set_au_allocator(ERTS_ALC_A_SHORT_LIVED, &init.sl_alloc);
    set_au_allocator(ERTS_ALC_A_STANDARD, &init.std_alloc);
    set_au_allocator(ERTS_ALC_A_LONG_LIVED, &init.ll_alloc);
    set_au_allocator(ERTS_ALC_A_EHEAP, &init.eheap_alloc);
    set_au_allocator(ERTS_ALC_A_BINARY, &init.binary_alloc);
    set_au_allocator(ERTS_ALC_A_ETS, &init.ets_alloc);
    set_au_allocator(ERTS_ALC_A_DRIVER, &init.driver_alloc);

    for (i = ERTS_ALC_A_MIN; i <= ERTS_ALC_A_MAX; i++) {
	if (!erts_allctrs[i].alloc)
	    erl_exit(ERTS_ABORT_EXIT,
		     "Missing alloc function for %s\n", ERTS_ALC_A2AD(i));
	if (!erts_allctrs[i].realloc)
	    erl_exit(ERTS_ABORT_EXIT,
		     "Missing realloc function for %s\n", ERTS_ALC_A2AD(i));
	if (!erts_allctrs[i].free)
	    erl_exit(ERTS_ABORT_EXIT,
		     "Missing free function for %s\n", ERTS_ALC_A2AD(i));
    }

    sys_alloc_opt(SYS_ALLOC_OPT_TRIM_THRESHOLD, init.trim_threshold);
    sys_alloc_opt(SYS_ALLOC_OPT_TOP_PAD, init.top_pad);
    if (erts_allctrs_info[ERTS_FIX_CORE_ALLOCATOR].enabled)
	erts_fix_core_allocator_ix = ERTS_FIX_CORE_ALLOCATOR;
    else
	erts_fix_core_allocator_ix = ERTS_ALC_A_SYSTEM;

    erts_mtrace_init(init.instr.mtrace, init.instr.nodename);

    start_au_allocator(ERTS_ALC_A_TEMPORARY,
		       &init.temp_alloc,
		       &temp_alloc_state);

    start_au_allocator(ERTS_ALC_A_SHORT_LIVED,
		       &init.sl_alloc,
		       &sl_alloc_state);

    start_au_allocator(ERTS_ALC_A_STANDARD,
		       &init.std_alloc,
		       &std_alloc_state);

    start_au_allocator(ERTS_ALC_A_LONG_LIVED,
		       &init.ll_alloc,
		       &ll_alloc_state);

    start_au_allocator(ERTS_ALC_A_EHEAP,
		       &init.eheap_alloc,
		       &eheap_alloc_state);

    start_au_allocator(ERTS_ALC_A_BINARY,
		       &init.binary_alloc,
		       &binary_alloc_state);

    start_au_allocator(ERTS_ALC_A_ETS,
		       &init.ets_alloc,
		       &ets_alloc_state);

    start_au_allocator(ERTS_ALC_A_DRIVER,
		       &init.driver_alloc,
		       &driver_alloc_state);

    fix_core_allocator	= erts_allctrs[erts_fix_core_allocator_ix].alloc;
    fix_core_extra	= erts_allctrs[erts_fix_core_allocator_ix].extra;

    erts_mtrace_install_wrapper_functions();
    extra_block_size += erts_instr_init(init.instr.stat, init.instr.map);

#ifdef DEBUG
    extra_block_size += install_debug_functions();
#endif

#ifdef ERTS_ALC_N_MIN_A_FIXED_SIZE

    erts_init_fix_alloc(extra_block_size, fix_core_alloc);


#if !defined(PURIFY) && !defined(VALGRIND)
    erts_set_fix_size(ERTS_ALC_T_PROC,		sizeof(Process));
    erts_set_fix_size(ERTS_ALC_T_DB_TABLE,	sizeof(DbTable));
    erts_set_fix_size(ERTS_ALC_T_ATOM,		sizeof(Atom));
    erts_set_fix_size(ERTS_ALC_T_EXPORT,	sizeof(Export));
    erts_set_fix_size(ERTS_ALC_T_MODULE,	sizeof(Module));
    erts_set_fix_size(ERTS_ALC_T_REG_PROC,	sizeof(RegProc));
    erts_set_fix_size(ERTS_ALC_T_MONITOR_SH,	ERTS_MONITOR_SH_SIZE*sizeof(Uint));
    erts_set_fix_size(ERTS_ALC_T_NLINK_SH,	ERTS_LINK_SH_SIZE*sizeof(Uint));
    erts_set_fix_size(ERTS_ALC_T_FUN_ENTRY,	sizeof(ErlFunEntry));
#ifdef ERTS_ALC_T_DRV_EV_D_STATE
    erts_set_fix_size(ERTS_ALC_T_DRV_EV_D_STATE,
		      sizeof(ErtsDrvEventDataState));
#endif
#ifdef ERTS_ALC_T_DRV_SEL_D_STATE
    erts_set_fix_size(ERTS_ALC_T_DRV_SEL_D_STATE,
		      sizeof(ErtsDrvSelectDataState));
#endif
#endif
#endif

}

static void
set_au_allocator(ErtsAlcType_t alctr_n, struct au_init *init)
{
    ErtsAllocatorFunctions_t *af = &erts_allctrs[alctr_n];
    ErtsAllocatorInfo_t *ai = &erts_allctrs_info[alctr_n];
    ErtsAllocatorThrSpec_t *tspec = &erts_allctr_thr_spec[alctr_n];

    if (!init->enable) {
	af->alloc = erts_sys_alloc;
	af->realloc = erts_sys_realloc;
	af->free = erts_sys_free;
	af->extra = NULL;
	ai->alloc_util = 0;
	ai->enabled = 0;
	ai->extra = NULL;
	return;
    }

    tspec->enabled = 0;
    tspec->all_thr_safe = 0;
    ai->thr_spec = 0;
#ifdef USE_THREADS
    if (init->thr_spec) {
	if (init->thr_spec > 0) {
	    af->alloc = erts_alcu_alloc_thr_spec;
	    if (init->init.util.ramv)
		af->realloc = erts_alcu_realloc_mv_thr_spec;
	    else
		af->realloc = erts_alcu_realloc_thr_spec;
	    af->free = erts_alcu_free_thr_spec;
	}
	else {
	    af->alloc = erts_alcu_alloc_thr_pref;
	    if (init->init.util.ramv)
		af->realloc = erts_alcu_realloc_mv_thr_pref;
	    else
		af->realloc = erts_alcu_realloc_thr_pref;
	    af->free = erts_alcu_free_thr_pref;
	    tspec->all_thr_safe = 1;
	}

	tspec->enabled	= 1;
	tspec->size	= abs(init->thr_spec) + 1;

	ai->thr_spec	= tspec->size;
    }
    else if (init->init.util.ts) {
	af->alloc = erts_alcu_alloc_ts;
	if (init->init.util.ramv)
	    af->realloc = erts_alcu_realloc_mv_ts;
	else
	    af->realloc = erts_alcu_realloc_ts;
	af->free = erts_alcu_free_ts;
    }
    else
#endif
    {
	af->alloc = erts_alcu_alloc;
	if (init->init.util.ramv)
	    af->realloc = erts_alcu_realloc_mv;
	else
	    af->realloc = erts_alcu_realloc;
	af->free = erts_alcu_free;
    }
    af->extra	= NULL;
    ai->alloc_util	= 1;
    ai->enabled		= 1;
}

static void
start_au_allocator(ErtsAlcType_t alctr_n,
		   struct au_init *init,
		   ErtsAllocatorState_t *state)
{
    int i;
    int size = 1;
    void *as0;
    enum allctr_type atype;
    ErtsAllocatorFunctions_t *af = &erts_allctrs[alctr_n];
    ErtsAllocatorInfo_t *ai = &erts_allctrs_info[alctr_n];
    ErtsAllocatorThrSpec_t *tspec = &erts_allctr_thr_spec[alctr_n];

    if (!init->enable)
	return;

    if (init->thr_spec) {
	void *states = erts_sys_alloc(0,
				      NULL,
				      ((sizeof(Allctr_t *)
					* (tspec->size + 1))
				       + (sizeof(ErtsAllocatorState_t)
					  * tspec->size)
				       + ERTS_CACHE_LINE_SIZE - 1));
	if (!states)
	    erl_exit(ERTS_ABORT_EXIT,
		     "Failed to allocate allocator states for %salloc\n",
		     init->init.util.name_prefix);
	tspec->allctr = (Allctr_t **) states;
	states = ((char *) states) + sizeof(Allctr_t *) * (tspec->size + 1);
	states = ((((UWord) states) & ERTS_CACHE_LINE_MASK)
		  ? (void *) ((((UWord) states) & ~ERTS_CACHE_LINE_MASK)
			      + ERTS_CACHE_LINE_SIZE)
		  : (void *) states);
	tspec->allctr[0] = init->thr_spec > 0 ? (Allctr_t *) state : (Allctr_t *) NULL;
	size = tspec->size;
	for (i = 1; i < size; i++)
	    tspec->allctr[i] = (Allctr_t *)
		&((ErtsAllocatorState_t *) states)[i-1];
    }

    for (i = 0; i < size; i++) {
	void *as;
	atype = init->atype;

	if (!init->thr_spec)
	    as0 = state;
	else {
	    as0 = (void *) tspec->allctr[i];
	    if (!as0)
		continue;
	    if (i == 0) {
		if (atype == AFIT)
		    atype = GOODFIT;
		init->init.util.ts = 1;
	    }
	    else {
		if (init->thr_spec < 0) {
		    init->init.util.ts = 1;
		    init->init.util.tspec = 0;
		    init->init.util.tpref = -1*init->thr_spec;
		}
		else {
		    init->init.util.ts = 0;
		    init->init.util.tspec = init->thr_spec + 1;
		    init->init.util.tpref = 0;
		}   
	    }
	}

	switch (atype) {
	case GOODFIT:
	    as = (void *) erts_gfalc_start((GFAllctr_t *) as0,
					   &init->init.gf,
					   &init->init.util);
	    break;
	case BESTFIT:
	    as = (void *) erts_bfalc_start((BFAllctr_t *) as0,
					   &init->init.bf,
					   &init->init.util);
	    break;
	case AFIT:
	    as = (void *) erts_afalc_start((AFAllctr_t *) as0,
					   &init->init.af,
					   &init->init.util);
	    break;
	default:
	    as = NULL;
	    ASSERT(0);
	}

	if (!as)
	    erl_exit(ERTS_ABORT_EXIT,
		     "Failed to start %salloc\n", init->init.util.name_prefix);

	ASSERT(as == (void *) as0);
	af->extra = as;
    }

    if (init->thr_spec) {
	af->extra = tspec;
	init->init.util.ts = 1;
    }

    ai->extra = af->extra;
}


static void bad_param(char *param_start, char *param_end)
{
    size_t len = param_end - param_start;
    char param[100];
    if (len > 99)
	len = 99;
    sys_memcpy((void *) param, (void *) param_start, len);
    param[len] = '\0';
    erts_fprintf(stderr, "bad \"%s\" parameter\n", param);
    erts_usage();
}

static void bad_value(char *param_start, char *param_end, char *value)
{
    size_t len = param_end - param_start;
    char param[100];
    if (len > 99)
	len = 99;
    sys_memcpy((void *) param, (void *) param_start, len);
    param[len] = '\0';
    erts_fprintf(stderr, "bad \"%s\" value: %s\n", param, value);
    erts_usage();
}

/* Get arg marks argument as handled by
   putting NULL in argv */
static char *
get_value(char* rest, char** argv, int* ip)
{
    char *param = argv[*ip]+1;
    argv[*ip] = NULL;
    if (*rest == '\0') {
	char *next = argv[*ip + 1];
	if (next[0] == '-'
	    && next[1] == '-'
	    &&  next[2] == '\0') {
	    bad_value(param, rest, "");
	}
	(*ip)++;
	argv[*ip] = NULL;
	return next;
    }
    return rest;
}

static ERTS_INLINE int
has_prefix(const char *prefix, const char *string)
{
    int i;
    for (i = 0; prefix[i]; i++)
	if (prefix[i] != string[i])
	    return 0;
    return 1;
}

static int
get_bool_value(char *param_end, char** argv, int* ip)
{
    char *param = argv[*ip]+1;
    char *value = get_value(param_end, argv, ip);
    if (strcmp(value, "true") == 0)
	return 1;
    else if (strcmp(value, "false") == 0)
	return 0;
    else
	bad_value(param, param_end, value);
    return -1;
}

static Uint
get_kb_value(char *param_end, char** argv, int* ip)
{
    Sint tmp;
    Uint max = ((~((Uint) 0))/1024) + 1;
    char *rest;
    char *param = argv[*ip]+1;
    char *value = get_value(param_end, argv, ip);
    errno = 0;
    tmp = (Sint) strtol(value, &rest, 10);
    if (errno != 0 || rest == value || tmp < 0 || max < ((Uint) tmp))
	bad_value(param, param_end, value);
    if (max == (Uint) tmp)
	return ~((Uint) 0);
    else
	return ((Uint) tmp)*1024;
}

static Uint
get_amount_value(char *param_end, char** argv, int* ip)
{
    Sint tmp;
    char *rest;
    char *param = argv[*ip]+1;
    char *value = get_value(param_end, argv, ip);
    errno = 0;
    tmp = (Sint) strtol(value, &rest, 10);
    if (errno != 0 || rest == value || tmp < 0)
	bad_value(param, param_end, value);
    return (Uint) tmp;
}

static int
get_bool_or_possitive_amount_value(int *bool, Uint *amount,
				   char *param_end, char** argv, int* ip)
{
    char *param = argv[*ip]+1;
    char *value = get_value(param_end, argv, ip);
    if (strcmp(value, "true") == 0) {
	*bool = 1; 
	return 1;
    }
    else if (strcmp(value, "false") == 0) {
	*bool = 0; 
	return 1;
    }
    else {
	Sint tmp;
	char *rest;
	errno = 0;
	tmp = (Sint) strtol(value, &rest, 10);
	if (errno != 0 || rest == value || tmp <= 0) {
	    bad_value(param, param_end, value);
	    return -1;
	}
	*amount = (Uint) tmp;
	return 0;
    }
}

static void
handle_au_arg(struct au_init *auip,
	      char* sub_param,
	      char** argv,
	      int* ip)
{
    char *param = argv[*ip]+1;

    switch (sub_param[0]) {
    case 'a':
	if(has_prefix("asbcst", sub_param)) {
	    auip->init.util.asbcst = get_kb_value(sub_param + 6, argv, ip);
	}
	else if(has_prefix("as", sub_param)) {
	    char *alg = get_value(sub_param + 2, argv, ip);
	    if (strcmp("bf", alg) == 0) {
		auip->atype = BESTFIT;
		auip->init.bf.ao = 0;
	    }
	    else if (strcmp("aobf", alg) == 0) {
		auip->atype = BESTFIT;
		auip->init.bf.ao = 1;
	    }
	    else if (strcmp("gf", alg) == 0) {
		auip->atype = GOODFIT;
	    }
	    else if (strcmp("af", alg) == 0) {
		auip->atype = AFIT;
	    }
	    else {
		bad_value(param, sub_param + 1, alg);
	    }
	}
	else
	    goto bad_switch;
	break;
    case 'e':
	auip->enable = get_bool_value(sub_param+1, argv, ip);
	break;
    case 'l':
	if (has_prefix("lmbcs", sub_param)) {
	    auip->default_.lmbcs = 0;
	    auip->init.util.lmbcs = get_kb_value(sub_param + 5, argv, ip);
	}
	else
	    goto bad_switch;
	break;
    case 'm':
	if (has_prefix("mbcgs", sub_param)) {
	    auip->init.util.mbcgs = get_amount_value(sub_param + 5, argv, ip);

	}
	else if (has_prefix("mbsd", sub_param)) {
	    auip->init.gf.mbsd = get_amount_value(sub_param + 4, argv, ip);
	    if (auip->init.gf.mbsd < 1)
		auip->init.gf.mbsd = 1;
	}
	else if (has_prefix("mmbcs", sub_param)) {
	    auip->default_.mmbcs = 0;
	    auip->init.util.mmbcs = get_kb_value(sub_param + 5, argv, ip);
	}
	else if (has_prefix("mmmbc", sub_param)) {
	    auip->default_.mmmbc = 0;
	    auip->init.util.mmmbc = get_amount_value(sub_param + 5, argv, ip);
	}
	else if (has_prefix("mmsbc", sub_param)) {
	    auip->init.util.mmsbc = get_amount_value(sub_param + 5, argv, ip);
	}
	else
	    goto bad_switch;
	break;
    case 'r':
	if(has_prefix("rsbcmt", sub_param)) {
	    auip->init.util.rsbcmt = get_amount_value(sub_param + 6, argv, ip);
	    if (auip->init.util.rsbcmt > 100)
		auip->init.util.rsbcmt = 100;
	}
	else if(has_prefix("rsbcst", sub_param)) {
	    auip->init.util.rsbcst = get_amount_value(sub_param + 6, argv, ip);
	    if (auip->init.util.rsbcst > 100)
		auip->init.util.rsbcst = 100;
	}
	else if (has_prefix("rmbcmt", sub_param)) {
	    auip->init.util.rmbcmt = get_amount_value(sub_param + 6, argv, ip);
	    if (auip->init.util.rmbcmt > 100)
		auip->init.util.rmbcmt = 100;
	}
	else if (has_prefix("ramv", sub_param)) {
	    auip->init.util.ramv = get_bool_value(sub_param + 4, argv, ip);
	}
	else
	    goto bad_switch;
	break;
    case 's':
	if(has_prefix("sbct", sub_param)) {
	    auip->init.util.sbct = get_kb_value(sub_param + 4, argv, ip);
	}
	else if (has_prefix("smbcs", sub_param)) {
	    auip->default_.smbcs = 0;
	    auip->init.util.smbcs = get_kb_value(sub_param + 5, argv, ip);
	}
	else
	    goto bad_switch;
	break;
    case 't': {
	Uint no;
	int enable;
	int res = get_bool_or_possitive_amount_value(&enable,
						     &no,
						     sub_param+1,
						     argv,
						     ip);
	if (res > 0)
	    auip->thr_spec = enable ? 1 : 0;
	else if (res == 0) {
	    int allocs = (int) no;
	    if (allocs < 0)
		allocs = INT_MIN;
	    else {
		allocs *= -1;
	    }
	    auip->thr_spec = allocs;
	}
	break;
    }
    default:
    bad_switch:
	bad_param(param, sub_param);
    }
}

static void
handle_args(int *argc, char **argv, erts_alc_hndl_args_init_t *init)
{
    struct au_init *aui[] = {
	&init->binary_alloc,
	&init->std_alloc,
	&init->ets_alloc,
	&init->eheap_alloc,
	&init->ll_alloc,
	&init->driver_alloc,
	&init->sl_alloc,
	&init->temp_alloc
    };
    int aui_sz = (int) sizeof(aui)/sizeof(aui[0]);
    char *arg;
    char *rest;
    int i, j;

    i = 1;

    ASSERT(argc && argv && init);

    while (i < *argc) {
	if(argv[i][0] == '-') {
	    char *param = argv[i]+1;
	    switch (argv[i][1]) {
	    case 'M':
		switch (argv[i][2]) {
		case 'B':
		    handle_au_arg(&init->binary_alloc, &argv[i][3], argv, &i);
		    break;
		case 'D':
		    handle_au_arg(&init->std_alloc, &argv[i][3], argv, &i);
		    break;
		case 'E':
		    handle_au_arg(&init->ets_alloc, &argv[i][3], argv, &i);
		    break;
		case 'F': /* fix_alloc */
		    if (has_prefix("e", param+2)) {
			arg = get_value(param+3, argv, &i);
			if (strcmp("true", arg) != 0)
			    bad_value(param, param+3, arg);
		    }
		    else
			bad_param(param, param+2);
		    break;
		case 'H':
		    handle_au_arg(&init->eheap_alloc, &argv[i][3], argv, &i);
		    break;
		case 'L':
		    handle_au_arg(&init->ll_alloc, &argv[i][3], argv, &i);
		    break;
		case 'M':
		    if (has_prefix("amcbf", argv[i]+3)) {
#if HAVE_ERTS_MSEG
			init->mseg.amcbf =
#endif
			    get_kb_value(argv[i]+8, argv, &i);
		    }
		    else if (has_prefix("rmcbf", argv[i]+3)) {
#if HAVE_ERTS_MSEG
			init->mseg.rmcbf =
#endif
			    get_amount_value(argv[i]+8, argv, &i);
		    }
		    else if (has_prefix("mcs", argv[i]+3)) {
#if HAVE_ERTS_MSEG
			init->mseg.mcs =
#endif
			    get_amount_value(argv[i]+6, argv, &i);
		    }
		    else if (has_prefix("cci", argv[i]+3)) {
#if HAVE_ERTS_MSEG
			init->mseg.cci =
#endif
			    get_amount_value(argv[i]+6, argv, &i);
		    }
		    else {
			bad_param(param, param+2);
		    }
		    break;
		case 'R':
		    handle_au_arg(&init->driver_alloc, &argv[i][3], argv, &i);
		    break;
		case 'S':
		    handle_au_arg(&init->sl_alloc, &argv[i][3], argv, &i);
		    break;
		case 'T':
		    handle_au_arg(&init->temp_alloc, &argv[i][3], argv, &i);
		    break;
		case 'Y': { /* sys_alloc */
		    if (has_prefix("tt", param+2)) {
			/* set trim threshold */
			arg = get_value(param+4, argv, &i);
			errno = 0;
			init->trim_threshold = (int) strtol(arg, &rest, 10);
			if (errno != 0
			    || rest == arg
			    || init->trim_threshold < 0
			    || (INT_MAX/1024) < init->trim_threshold) {
			    bad_value(param, param+4, arg);
			}
			VERBOSE(DEBUG_SYSTEM,
                                ("using trim threshold: %d\n",
                                 init->trim_threshold));
			init->trim_threshold *= 1024;
		    }
		    else if (has_prefix("tp", param+2)) {
			/* set top pad */
			arg = get_value(param+4, argv, &i);
			errno = 0;
			init->top_pad = (int) strtol(arg, &rest, 10);
			if (errno != 0
			    || rest == arg
			    || init->top_pad < 0
			    || (INT_MAX/1024) < init->top_pad) {
			    bad_value(param, param+4, arg);
			}
			VERBOSE(DEBUG_SYSTEM,
                                ("using top pad: %d\n",init->top_pad));
			init->top_pad *= 1024;
		    }
		    else if (has_prefix("m", param+2)) {
			/* Has been handled by erlexec */
			(void) get_value(param+3, argv, &i);
		    }
		    else if (has_prefix("e", param+2)) {
			arg = get_value(param+3, argv, &i);
			if (strcmp("true", arg) != 0)
			    bad_value(param, param+3, arg);
		    }
		    else
			bad_param(param, param+2);
		    break;
		}
		case 'e':
		    switch (argv[i][3]) {
		    case 'a': {
			int a;
			arg = get_value(argv[i]+4, argv, &i);
			if (strcmp("min", arg) == 0) {
			    for (a = 0; a < aui_sz; a++)
				aui[a]->enable = 0;
			}
			else if (strcmp("max", arg) == 0) {
			    for (a = 0; a < aui_sz; a++)
				aui[a]->enable = 1;
			}
			else if (strcmp("config", arg) == 0) {
			    init->erts_alloc_config = 1;
			}
			else if (strcmp("r9c", arg) == 0
				 || strcmp("r10b", arg) == 0
				 || strcmp("r11b", arg) == 0) {
			    set_default_sl_alloc_opts(&init->sl_alloc);
			    set_default_std_alloc_opts(&init->std_alloc);
			    set_default_ll_alloc_opts(&init->ll_alloc);
			    set_default_temp_alloc_opts(&init->temp_alloc);
			    set_default_eheap_alloc_opts(&init->eheap_alloc);
			    set_default_binary_alloc_opts(&init->binary_alloc);
			    set_default_ets_alloc_opts(&init->ets_alloc);
			    set_default_driver_alloc_opts(&init->driver_alloc);

			    init->driver_alloc.enable = 0;
			    if (strcmp("r9c", arg) == 0) {
				init->sl_alloc.enable = 0;
				init->std_alloc.enable = 0;
				init->binary_alloc.enable = 0;
				init->ets_alloc.enable = 0;
			    }

			    for (a = 0; a < aui_sz; a++) {
				aui[a]->thr_spec = 0;
				aui[a]->init.util.ramv = 0;
				aui[a]->init.util.mmmbc = 10;
				aui[a]->init.util.lmbcs = 5*1024*1024;
			    }
			}
			else {
			    bad_param(param, param+3);
			}
			break;
		    }
		    default:
			bad_param(param, param+1);
		    }
		    break;
		case 'i':
		    switch (argv[i][3]) {
		    case 's':
			arg = get_value(argv[i]+4, argv, &i);
			if (strcmp("true", arg) == 0)
			    init->instr.stat = 1;
			else if (strcmp("false", arg) == 0)
			    init->instr.stat = 0;
			else
			    bad_value(param, param+3, arg);
			break;
		    case 'm':
			arg = get_value(argv[i]+4, argv, &i);
			if (strcmp("true", arg) == 0)
			    init->instr.map = 1;
			else if (strcmp("false", arg) == 0)
			    init->instr.map = 0;
			else
			    bad_value(param, param+3, arg);
			break;
		    case 't':
			init->instr.mtrace = get_value(argv[i]+4, argv, &i);
			break;
		    default:
			bad_param(param, param+2);
		    }
		    break;
		case 'u':
		    if (has_prefix("ycs", argv[i]+3)) {
			init->alloc_util.ycs
			    = get_kb_value(argv[i]+6, argv, &i);
		    }
		    else if (has_prefix("mmc", argv[i]+3)) {
			init->alloc_util.mmc
			    = get_amount_value(argv[i]+6, argv, &i);
		    }
		    else {
			int a;
			int start = i;
			char *param = argv[i];
			char *val = i+1 < *argc ? argv[i+1] : NULL;

			for (a = 0; a < aui_sz; a++) {
			    if (a > 0) {
				ASSERT(i == start || i == start+1);
				argv[start] = param;
				if (i != start)
				    argv[start + 1] = val;
				i = start;
			    }
			    handle_au_arg(aui[a], &argv[i][3], argv, &i);
			}
		    }
		    break;
		default:
		    bad_param(param, param+1);
		}
		break;
	    case '-':
		if (argv[i][2] == '\0') {
		    /* End of system flags reached */
		    if (init->instr.mtrace
			/* || init->instr.stat
			   || init->instr.map */) {
			while (i < *argc) {
			    if(strcmp(argv[i], "-sname") == 0
			       || strcmp(argv[i], "-name") == 0) {
				if (i + 1 <*argc) {
				    init->instr.nodename = argv[i+1];
				    break;
				}
			    }
			    i++;
			}
		    }
		    goto args_parsed;
		}
		break;
	    default:
		break;
	    }
	}
	i++;
    }

 args_parsed:
    /* Handled arguments have been marked with NULL. Slide arguments
       not handled towards the beginning of argv. */
    for (i = 0, j = 0; i < *argc; i++) {
	if (argv[i])
	    argv[j++] = argv[i];
    }
    *argc = j;
    
}

static char *type_no_str(ErtsAlcType_t n)
{

#if ERTS_ALC_N_MIN != 0
    if (n < ERTS_ALC_N_MIN)
	return NULL;
#endif
    if (n > ERTS_ALC_N_MAX)
	return NULL;
    return (char *) ERTS_ALC_N2TD(n);
}

#define type_str(T) type_no_str(ERTS_ALC_T2N((T)))

erts_tsd_key_t thr_ix_key;
erts_spinlock_t alloc_thr_ix_lock;
int last_thr_ix;
int first_dyn_thr_ix;

static void
init_thr_ix(int static_ixs)
{
    erts_tsd_key_create(&thr_ix_key);
    erts_spinlock_init(&alloc_thr_ix_lock, "alloc_thr_ix_lock");
    last_thr_ix = -4711;
    first_dyn_thr_ix = static_ixs+1;
}

int
erts_alc_get_thr_ix(void)
{
    int ix = (int)(long) erts_tsd_get(thr_ix_key);
    if (ix == 0) {
	erts_spin_lock(&alloc_thr_ix_lock);
	last_thr_ix++;
	if (last_thr_ix < 0) 
	    last_thr_ix = first_dyn_thr_ix;
	ix = last_thr_ix;
	erts_spin_unlock(&alloc_thr_ix_lock);
	erts_tsd_set(thr_ix_key, (void *)(long) ix);
    }
    ASSERT(ix > 0);
    return ix;
}

void erts_alloc_reg_scheduler_id(Uint id)
{
    int ix = (int) id;
    ASSERT(0 < ix && ix <= first_dyn_thr_ix);
    ASSERT(0 == (int) (long) erts_tsd_get(thr_ix_key));
    erts_tsd_set(thr_ix_key, (void *)(long) ix);
}

__decl_noreturn void
erts_alc_fatal_error(int error, int func, ErtsAlcType_t n, ...)
{
    char buf[10];
    char *t_str;
    char *allctr_str;

    ASSERT(n >= ERTS_ALC_N_MIN);
    ASSERT(n <= ERTS_ALC_N_MAX);


    if (n < ERTS_ALC_N_MIN || ERTS_ALC_N_MAX < n)
	allctr_str = "UNKNOWN";
    else {
	ErtsAlcType_t a = ERTS_ALC_T2A(ERTS_ALC_N2T(n));
	if (erts_allctrs_info[a].enabled)
	    allctr_str = (char *) ERTS_ALC_A2AD(a);
	else
	    allctr_str = (char *) ERTS_ALC_A2AD(ERTS_ALC_A_SYSTEM);
    }

    t_str = type_no_str(n);
    if (!t_str) {
	sprintf(buf, "%d", (int) n);
	t_str = buf;
    }

    switch (error) {
    case ERTS_ALC_E_NOTSUP: {
	char *op_str;
	switch (func) {
	case ERTS_ALC_O_ALLOC:		op_str = "alloc";	break;
	case ERTS_ALC_O_REALLOC:	op_str = "realloc";	break;
	case ERTS_ALC_O_FREE:		op_str = "free";	break;
	default:			op_str = "UNKNOWN";	break;
	}
	erl_exit(ERTS_ABORT_EXIT,
		 "%s: %s operation not supported (memory type: \"%s\")\n",
		 allctr_str, op_str, t_str);
	break;
    }
    case ERTS_ALC_E_NOMEM: {
	Uint size;
	va_list argp;
	char *op = func == ERTS_ALC_O_REALLOC ? "reallocate" : "allocate";
	

	va_start(argp, n);
	size = va_arg(argp, Uint);
	va_end(argp);
	erl_exit(1,
		 "%s: Cannot %s %lu bytes of memory (of type \"%s\").\n",
		 allctr_str, op, size, t_str);
	break;
    }
    case ERTS_ALC_E_NOALLCTR:
	erl_exit(ERTS_ABORT_EXIT,
		 "erts_alloc: Unknown allocator type: %d\n",
		 ERTS_ALC_T2A(ERTS_ALC_N2T(n)));
	break;
    default:
	erl_exit(ERTS_ABORT_EXIT, "erts_alloc: Unknown error: %d\n", error);
	break;
    }
}

__decl_noreturn void
erts_alloc_enomem(ErtsAlcType_t type, Uint size)
{
    erts_alloc_n_enomem(ERTS_ALC_T2N(type), size);
}

__decl_noreturn void
erts_alloc_n_enomem(ErtsAlcType_t n, Uint size)
{
    erts_alc_fatal_error(ERTS_ALC_E_NOMEM, ERTS_ALC_O_ALLOC, n, size);
}

__decl_noreturn void
erts_realloc_enomem(ErtsAlcType_t type, void *ptr, Uint size)
{
    erts_realloc_n_enomem(ERTS_ALC_T2N(type), ptr, size);
}

__decl_noreturn void
erts_realloc_n_enomem(ErtsAlcType_t n, void *ptr, Uint size)
{
    erts_alc_fatal_error(ERTS_ALC_E_NOMEM, ERTS_ALC_O_REALLOC, n, size);
}

static ERTS_INLINE Uint
alcu_size(ErtsAlcType_t ai)
{
    Uint res = 0;

    ASSERT(erts_allctrs_info[ai].enabled);
    ASSERT(erts_allctrs_info[ai].alloc_util);

    if (!erts_allctrs_info[ai].thr_spec) {
	Allctr_t *allctr = erts_allctrs_info[ai].extra;
	AllctrSize_t asize;
	erts_alcu_current_size(allctr, &asize);
	res += asize.blocks;
    }
    else {
	ErtsAllocatorThrSpec_t *tspec = &erts_allctr_thr_spec[ai];
	int i;

	ASSERT(tspec->all_thr_safe);

	ASSERT(tspec->enabled);

	for (i = tspec->size - 1; i >= 0; i--) {
	    Allctr_t *allctr = tspec->allctr[i];
	    AllctrSize_t asize;
	    if (allctr) {
		erts_alcu_current_size(allctr, &asize);
		res += asize.blocks;
	    }
	}
    }

    return res;
}

Eterm
erts_memory(int *print_to_p, void *print_to_arg, void *proc, Eterm earg)
{
#define ERTS_MEM_NEED_ALL_ALCU (!erts_instr_stat && want_tot_or_sys)
    ErtsFixInfo efi;
    struct {
	int total;
	int processes;
	int processes_used;
	int system;
	int atom;
	int atom_used;
	int binary;
	int code;
	int ets;
	int maximum;
    } want = {0};
    struct {
	Uint total;
	Uint processes;
	Uint processes_used;
	Uint system;
	Uint atom;
	Uint atom_used;
	Uint binary;
	Uint code;
	Uint ets;
	Uint maximum;
    } size = {0};
    Eterm atoms[sizeof(size)/sizeof(Uint)];
    Uint *uintps[sizeof(size)/sizeof(Uint)];
    Eterm euints[sizeof(size)/sizeof(Uint)];
    int need_atom;
    int want_tot_or_sys;
    int length;
    Eterm res = THE_NON_VALUE;
    ErtsAlcType_t ai;
    int only_one_value = 0;

    /* Figure out whats wanted... */

    length = 0;
    if (is_non_value(earg)) { /* i.e. wants all */
	want.total = 1;
	atoms[length] = am_total;
	uintps[length++] = &size.total;

	want.processes = 1;
	atoms[length] = am_processes;
	uintps[length++] = &size.processes;

	want.processes_used = 1;
	atoms[length] = am_processes_used;
	uintps[length++] = &size.processes_used;

	want.system = 1;
	atoms[length] = am_system;
	uintps[length++] = &size.system;

	want.atom = 1;
	atoms[length] = am_atom;
	uintps[length++] = &size.atom;

	want.atom_used = 1;
	atoms[length] = am_atom_used;
	uintps[length++] = &size.atom_used;

	want.binary = 1;
	atoms[length] = am_binary;
	uintps[length++] = &size.binary;

	want.code = 1;
	atoms[length] = am_code;
	uintps[length++] = &size.code;

	want.ets = 1;
	atoms[length] = am_ets;
	uintps[length++] = &size.ets;

	want.maximum = erts_instr_stat;
	if (want.maximum) {
	    atoms[length] = am_maximum;
	    uintps[length++] = &size.maximum;
	}

    }
    else {
	DeclareTmpHeapNoproc(tmp_heap,2);
	Eterm wanted_list;

	if (is_nil(earg))
	    return NIL;

	UseTmpHeapNoproc(2);
	if (is_not_atom(earg))
	    wanted_list = earg;
	else {
	    wanted_list = CONS(&tmp_heap[0], earg, NIL);
	    only_one_value = 1;
	}
	    
	while (is_list(wanted_list)) {
	    switch (CAR(list_val(wanted_list))) {
	    case am_total:
		if (!want.total) {
		    want.total = 1;
		    atoms[length] = am_total;
		    uintps[length++] = &size.total;
		}
		break;
	    case am_processes:
		if (!want.processes) {
		    want.processes = 1;
		    atoms[length] = am_processes;
		    uintps[length++] = &size.processes;
		}
		break;
	    case am_processes_used:
		if (!want.processes_used) {
		    want.processes_used = 1;
		    atoms[length] = am_processes_used;
		    uintps[length++] = &size.processes_used;
		}
		break;
	    case am_system:
		if (!want.system) {
		    want.system = 1;
		    atoms[length] = am_system;
		    uintps[length++] = &size.system;
		}
		break;
	    case am_atom:
		if (!want.atom) {
		    want.atom = 1;
		    atoms[length] = am_atom;
		    uintps[length++] = &size.atom;
		}
		break;
	    case am_atom_used:
		if (!want.atom_used) {
		    want.atom_used = 1;
		    atoms[length] = am_atom_used;
		    uintps[length++] = &size.atom_used;
		}
		break;
	    case am_binary:
		if (!want.binary) {
		    want.binary = 1;
		    atoms[length] = am_binary;
		    uintps[length++] = &size.binary;
		}
		break;
	    case am_code:
		if (!want.code) {
		    want.code = 1;
		    atoms[length] = am_code;
		    uintps[length++] = &size.code;
		}
		break;
	    case am_ets:
		if (!want.ets) {
		    want.ets = 1;
		    atoms[length] = am_ets;
		    uintps[length++] = &size.ets;
		}
		break;
	    case am_maximum:
		if (erts_instr_stat) {
		    if (!want.maximum) {
			want.maximum = 1;
			atoms[length] = am_maximum;
			uintps[length++] = &size.maximum;
		    }
		} else {
		    UnUseTmpHeapNoproc(2);
		    return am_badarg;
		}
		break;
	    default:
		UnUseTmpHeapNoproc(2);
		return am_badarg;
	    }
	    wanted_list = CDR(list_val(wanted_list));
	}
	UnUseTmpHeapNoproc(2);
	if (is_not_nil(wanted_list))
	    return am_badarg;
    }

    /* All alloc_util allocators *have* to be enabled */
    
    for (ai = ERTS_ALC_A_MIN; ai <= ERTS_ALC_A_MAX; ai++) {
	switch (ai) {
	case ERTS_ALC_A_SYSTEM:
	case ERTS_ALC_A_FIXED_SIZE:
	    break;
	default:
	    if (!erts_allctrs_info[ai].enabled
		|| !erts_allctrs_info[ai].alloc_util) {
		return am_notsup;
	    }
	    break;
	}
    }

    ASSERT(length <= sizeof(atoms)/sizeof(Eterm));
    ASSERT(length <= sizeof(euints)/sizeof(Eterm));
    ASSERT(length <= sizeof(uintps)/sizeof(Uint));


    if (proc) {
	ERTS_SMP_LC_ASSERT(ERTS_PROC_LOCK_MAIN
			   == erts_proc_lc_my_proc_locks(proc));
	/* We'll need locks early in the lock order */
	erts_smp_proc_unlock(proc, ERTS_PROC_LOCK_MAIN);
    }

    /* Calculate values needed... */

    want_tot_or_sys = want.total || want.system;
    need_atom = ERTS_MEM_NEED_ALL_ALCU || want.atom;

    if (ERTS_MEM_NEED_ALL_ALCU) {
	size.total = 0;

	for (ai = ERTS_ALC_A_MIN; ai <= ERTS_ALC_A_MAX; ai++) {
	    if (erts_allctrs_info[ai].alloc_util) {
		Uint *save;
		Uint asz;
		switch (ai) {
		case ERTS_ALC_A_TEMPORARY:
		     /*
		      * Often not thread safe and usually never
		      * contain any allocated memory.
		      */
		    continue;
		case ERTS_ALC_A_EHEAP:
		    save = &size.processes;
		    break;
		case ERTS_ALC_A_ETS:
		    save = &size.ets;
		    break;
		case ERTS_ALC_A_BINARY:
		    save = &size.binary;
		    break;
		default:
		    save = NULL;
		    break;
		}
		asz = alcu_size(ai);
		if (save)
		    *save = asz;
		size.total += asz;
	    }
	}
    }



    if (want_tot_or_sys || want.processes || want.processes_used) {
	Uint tmp;

	if (ERTS_MEM_NEED_ALL_ALCU)
	    tmp = size.processes;
	else
	    tmp = alcu_size(ERTS_ALC_A_EHEAP);
	tmp += erts_max_processes*sizeof(Process*);
#ifdef HYBRID
	tmp += erts_max_processes*sizeof(Process*);
#endif
	tmp += erts_bif_timer_memory_size();
	tmp += erts_tot_link_lh_size();

	size.processes = size.processes_used = tmp;

	erts_fix_info(ERTS_ALC_T_NLINK_SH, &efi);
	size.processes += efi.total;
	size.processes_used += efi.used;

	erts_fix_info(ERTS_ALC_T_MONITOR_SH, &efi);
	size.processes += efi.total;
	size.processes_used += efi.used;

	erts_fix_info(ERTS_ALC_T_PROC, &efi);
	size.processes += efi.total;
	size.processes_used += efi.used;

	erts_fix_info(ERTS_ALC_T_REG_PROC, &efi);
	size.processes += efi.total;
	size.processes_used += efi.used;

    }

    if (want.atom || want.atom_used) {
	Uint reserved_atom_space, atom_space;
	erts_atom_get_text_space_sizes(&reserved_atom_space, &atom_space);
	size.atom = size.atom_used = atom_table_sz();
	erts_fix_info(ERTS_ALC_T_ATOM, &efi);

	if (want.atom) {
	    size.atom += reserved_atom_space;
	    size.atom += efi.total;
	}

	if (want.atom_used) {
	    size.atom_used += atom_space;
	    size.atom_used += efi.used;
	}
    }

    if (!ERTS_MEM_NEED_ALL_ALCU && want.binary)
	size.binary = alcu_size(ERTS_ALC_A_BINARY);

    if (want.code) {
	size.code = module_table_sz();
	erts_fix_info(ERTS_ALC_T_MODULE, &efi);
	size.code += efi.used;
	size.code += export_table_sz();
	erts_fix_info(ERTS_ALC_T_EXPORT, &efi);
	size.code += efi.used;
	size.code += erts_fun_table_sz();
	erts_fix_info(ERTS_ALC_T_FUN_ENTRY, &efi);
	size.code += efi.used;
	size.code += allocated_modules*sizeof(Range);
	size.code += erts_total_code_size;
    }

    if (want.ets) {
	if (!ERTS_MEM_NEED_ALL_ALCU)
	    size.ets = alcu_size(ERTS_ALC_A_ETS);
	size.ets += erts_get_ets_misc_mem_size();
    }

    if (erts_instr_stat && (want_tot_or_sys || want.maximum)) {
	if (want_tot_or_sys) {
	    size.total = erts_instr_get_total();
	    size.system = size.total - size.processes;
	}
	size.maximum = erts_instr_get_max_total();
    }
    else if (want_tot_or_sys) {
	size.system = size.total - size.processes;
    }

    if (print_to_p) {
	int i;
	int to = *print_to_p;
	void *arg = print_to_arg;

	/* Print result... */
	erts_print(to, arg, "=memory\n");
	for (i = 0; i < length; i++)
	    erts_print(to, arg, "%T: %bpu\n", atoms[i], *uintps[i]);
    }

    if (proc) {
	/* Build erlang term result... */
	Uint *hp;
	Uint hsz;

	erts_smp_proc_lock(proc, ERTS_PROC_LOCK_MAIN);

	if (only_one_value) {
	    ASSERT(length == 1);
	    hsz = 0;
	    erts_bld_uint(NULL, &hsz, *uintps[0]);
	    hp = hsz ? HAlloc((Process *) proc, hsz) : NULL;
	    res = erts_bld_uint(&hp, NULL, *uintps[0]);
	}
	else {
	    Uint **hpp = NULL;
	    Uint *hszp = &hsz;
	    hsz = 0;

	    while (1) {
		int i;
		for (i = 0; i < length; i++)
		    euints[i] = erts_bld_uint(hpp, hszp, *uintps[i]);
		res = erts_bld_2tup_list(hpp, hszp, length, atoms, euints);
		if (hpp)
		    break;
		hp = HAlloc((Process *) proc, hsz);
		hpp = &hp;
		hszp = NULL;
	    }
	}
    }

    return res;

#undef ERTS_MEM_NEED_ALL_ALCU
}

struct aa_values {
    Uint arity;
    const char *name;
    Uint ui[2];
};

Eterm
erts_allocated_areas(int *print_to_p, void *print_to_arg, void *proc)
{
#define MAX_AA_VALUES \
  (20 + (ERTS_ALC_N_MAX_A_FIXED_SIZE - ERTS_ALC_N_MIN_A_FIXED_SIZE + 1))

    struct aa_values values[MAX_AA_VALUES];
    Eterm res = THE_NON_VALUE;
    int i, length;
    ErtsFixInfo efi;
    Uint reserved_atom_space, atom_space;

    if (proc) {
	ERTS_SMP_LC_ASSERT(ERTS_PROC_LOCK_MAIN
			   == erts_proc_lc_my_proc_locks(proc));

	/* We'll need locks early in the lock order */
	erts_smp_proc_unlock(proc, ERTS_PROC_LOCK_MAIN);
    }

    i = 0;

    if (erts_instr_stat) {
	values[i].arity = 2;
	values[i].name = "total";
	values[i].ui[0] = erts_instr_get_total();
	i++;

	values[i].arity = 2;
	values[i].name = "maximum";
	values[i].ui[0] = erts_instr_get_max_total();
	i++;
    }

    values[i].arity = 2;
    values[i].name = "sys_misc";
    values[i].ui[0] = erts_sys_misc_mem_sz();
    i++;

    values[i].arity = 2;
    values[i].name = "static";
    values[i].ui[0] = 
	erts_max_ports*sizeof(Port)		/* Port table */
	+ erts_timer_wheel_memory_size()	/* Timer wheel */
#ifdef SYS_TMP_BUF_SIZE
	+ SYS_TMP_BUF_SIZE		/* tmp_buf in sys on vxworks & ose */
#endif
	;
    i++;

    erts_atom_get_text_space_sizes(&reserved_atom_space, &atom_space);

    values[i].arity = 3;
    values[i].name = "atom_space";
    values[i].ui[0] = reserved_atom_space;
    values[i].ui[1] = atom_space;
    i++;

    values[i].arity = 2;
    values[i].name = "atom_table";
    values[i].ui[0] = atom_table_sz();
    i++;

    values[i].arity = 2;
    values[i].name = "module_table";
    values[i].ui[0] = module_table_sz();
    i++;

    values[i].arity = 2;
    values[i].name = "export_table";
    values[i].ui[0] = export_table_sz();
    i++;

    values[i].arity = 2;
    values[i].name = "register_table";
    values[i].ui[0] = process_reg_sz();
    i++;

    values[i].arity = 2;
    values[i].name = "fun_table";
    values[i].ui[0] = erts_fun_table_sz();
    i++;

    values[i].arity = 2;
    values[i].name = "module_refs";
    values[i].ui[0] = allocated_modules*sizeof(Range);
    i++;

    values[i].arity = 2;
    values[i].name = "loaded_code";
    values[i].ui[0] = erts_total_code_size;
    i++;

    values[i].arity = 2;
    values[i].name = "dist_table";
    values[i].ui[0] = erts_dist_table_size();
    i++;

    values[i].arity = 2;
    values[i].name = "node_table";
    values[i].ui[0] = erts_node_table_size();
    i++;

    values[i].arity = 2;
    values[i].name = "bits_bufs_size";
    values[i].ui[0] = erts_bits_bufs_size();
    i++;

    values[i].arity = 2;
    values[i].name = "bif_timer";
    values[i].ui[0] = erts_bif_timer_memory_size();
    i++;

    values[i].arity = 2;
    values[i].name = "link_lh";
    values[i].ui[0] = erts_tot_link_lh_size();
    i++;

    {
	Uint n;

	for (n = ERTS_ALC_N_MIN_A_FIXED_SIZE;
	     n <= ERTS_ALC_N_MAX_A_FIXED_SIZE;
	     n++) {
	    erts_fix_info(ERTS_ALC_N2T(n), &efi);

	    values[i].arity = 3;
	    values[i].name = ERTS_ALC_N2TD(n);
	    values[i].ui[0] = efi.total;
	    values[i].ui[1] = efi.used;
	    i++;
	}    

    }

    length = i;
    ASSERT(length <= MAX_AA_VALUES);

    if (print_to_p) {
	/* Print result... */
	int to = *print_to_p;
	void *arg = print_to_arg;

	erts_print(to, arg, "=allocated_areas\n");
	for (i = 0; i < length; i++) {
	    switch (values[i].arity) {
	    case 2:
		erts_print(to, arg, "%s: %bpu\n",
			   values[i].name, values[i].ui[0]);
		break;
	    case 3:
		erts_print(to, arg, "%s: %bpu %bpu\n",
			   values[i].name, values[i].ui[0], values[i].ui[1]);
		break;
	    default:
		erts_print(to, arg, "ERROR: internal_error\n");
		ASSERT(0);
		return am_internal_error;
	    }
	}
    }

    if (proc) {
	/* Build erlang term result... */
	Eterm tuples[MAX_AA_VALUES];
	Uint *hp;
	Uint **hpp;
	Uint hsz;
	Uint *hszp;

	erts_smp_proc_lock(proc, ERTS_PROC_LOCK_MAIN);

	hpp = NULL;
	hsz = 0;
	hszp = &hsz;

	while (1) {
	    int i;
	    for (i = 0; i < length; i++) {
		Eterm atom;
		if (hpp)
		    atom = am_atom_put(values[i].name,
				       (int) strlen(values[i].name));
		else
		    atom = am_true;

		switch (values[i].arity) {
		case 2:
		    tuples[i] = erts_bld_tuple(hpp, hszp, 2,
					       atom,
					       erts_bld_uint(hpp, hszp,
							     values[i].ui[0]));
		    break;
		case 3:
		    tuples[i] = erts_bld_tuple(hpp, hszp, 3,
					       atom,
					       erts_bld_uint(hpp, hszp,
							     values[i].ui[0]),
					       erts_bld_uint(hpp, hszp,
							     values[i].ui[1]));
		    break;
		default:
		    ASSERT(0);
		    return am_internal_error;
		}
	    }
	    res = erts_bld_list(hpp, hszp, length, tuples);
	    if (hpp)
		break;
	    hp = HAlloc((Process *) proc, hsz);
	    hpp = &hp;
	    hszp = NULL;
	}
    }

    return res;
#undef MAX_AA_VALUES
}

Eterm
erts_alloc_util_allocators(void *proc)
{
    Eterm res;
    Uint *hp;
    Uint sz;
    int i;
    /*
     * Currently all allocators except sys_alloc and fix_alloc are
     * alloc_util allocators.
     */
    sz = ((ERTS_ALC_A_MAX + 1 - ERTS_ALC_A_MIN) - 2)*2;
    ASSERT(sz > 0);
    hp = HAlloc((Process *) proc, sz);
    res = NIL;
    for (i = ERTS_ALC_A_MAX; i >= ERTS_ALC_A_MIN; i--) {
	switch (i) {
	case ERTS_ALC_A_SYSTEM:
	case ERTS_ALC_A_FIXED_SIZE:
	    break;
	default: {
	    char *alc_str = (char *) ERTS_ALC_A2AD(i);
	    Eterm alc = am_atom_put(alc_str, sys_strlen(alc_str));
	    res = CONS(hp, alc, res);
	    hp += 2;
	    break;
	}
	}
    }
    return res;
}

Eterm
erts_allocator_info_term(void *proc, Eterm which_alloc, int only_sz)
{
#define ERTS_AIT_RET(R) \
  do { res = (R); goto done; } while (0)
#define ERTS_AIT_HALLOC(P, S) \
  do { hp = HAlloc((P), (S)); hp_end = hp + (S); } while (0)

    ErtsAlcType_t i;
    Uint sz = 0;
    Uint *hp = NULL;
    Uint *hp_end = NULL;
    Eterm res = am_undefined;

    if (is_not_atom(which_alloc))
	goto done;

    for (i = ERTS_ALC_A_MIN; i <= ERTS_ALC_A_MAX; i++) {
	if (erts_is_atom_str((char *) ERTS_ALC_A2AD(i), which_alloc)) {
	    if (!erts_allctrs_info[i].enabled)
		ERTS_AIT_RET(am_false);
	    else {
		if (erts_allctrs_info[i].alloc_util) {
		    Eterm ires, tmp;
		    Eterm **hpp;
		    Uint *szp;
		    Eterm (*info_func)(Allctr_t *,
				       int,
				       int *,
				       void *,
				       Uint **,
				       Uint *);

		    info_func = (only_sz
				 ? erts_alcu_sz_info
				 : erts_alcu_info);

		    if (erts_allctrs_info[i].thr_spec) {
			ErtsAllocatorThrSpec_t *tspec = &erts_allctr_thr_spec[i];
			int j;
			int block_system = !tspec->all_thr_safe;

			if (block_system) {
			    erts_smp_proc_unlock(proc, ERTS_PROC_LOCK_MAIN);
			    erts_smp_block_system(0);
			}
			ASSERT(tspec->enabled);

			szp = &sz;
			hpp = NULL;

			while (1) {
			    ires = NIL;
			    for (j = tspec->size - 1; j >= 0; j--) {
				Allctr_t *allctr = tspec->allctr[j];
				if (allctr) {
				    tmp = erts_bld_tuple(hpp,
							 szp,
							 3,
							 erts_bld_atom(hpp,
								       szp,
								       "instance"),
							 make_small((Uint) j),
							 (*info_func)(allctr,
								      hpp != NULL,
								      NULL,
								      NULL,
								      hpp,
								      szp));
				    ires = erts_bld_cons(hpp, szp, tmp, ires);
				}
			    }
			    if (hpp)
				break;
			    ERTS_AIT_HALLOC((Process *) proc, sz);
			    hpp = &hp;
			    szp = NULL;
			}

			if (block_system) {
			    erts_smp_release_system();
			    erts_smp_proc_lock(proc, ERTS_PROC_LOCK_MAIN);
			}
		    }
		    else {
			Allctr_t *allctr = erts_allctrs_info[i].extra;
			szp = &sz;
			hpp = NULL;
			while (1) {
			    ires = NIL;
			    tmp = erts_bld_tuple(hpp,
						 szp,
						 3,
						 erts_bld_atom(hpp,
							       szp,
							       "instance"),
						 make_small((Uint) 0),
						 (*info_func)(allctr,
							      hpp != NULL,
							      NULL,
							      NULL,
							      hpp,
							      szp));
			    ires = erts_bld_cons(hpp, szp, tmp, ires);
			    if (hpp)
				break;
			    ERTS_AIT_HALLOC((Process *) proc, sz);
			    hpp = &hp;
			    szp = NULL;
			}
		    }
		    ERTS_AIT_RET(ires);
		}
		else {
		    Eterm *szp, **hpp;
		    
		    switch (i) {
		    case ERTS_ALC_A_SYSTEM: {
			SysAllocStat sas;
			Eterm opts_am;
			Eterm opts;
			Eterm as[4]; /* Ok even if !HEAP_ON_C_STACK, not really heap data on stack */
			Eterm ts[4]; /* Ok even if !HEAP_ON_C_STACK, not really heap data on stack */
			int l;

			if (only_sz)
			    ERTS_AIT_RET(NIL);

			sys_alloc_stat(&sas);
			opts_am = am_atom_put("options", 7);

			szp = &sz;
			hpp = NULL;

		    restart_sys_alloc:
			l = 0;
			as[l] = am_atom_put("e", 1);
			ts[l++] = am_true;
#ifdef ELIB_ALLOC_IS_CLIB
			as[l] = am_atom_put("m", 1);
			ts[l++] = am_atom_put("elib", 4);
#else
			as[l] = am_atom_put("m", 1);
			ts[l++] = am_atom_put("libc", 4);
#endif
			if(sas.trim_threshold >= 0) {
			    as[l] = am_atom_put("tt", 2);
			    ts[l++] = erts_bld_uint(hpp, szp,
						    (Uint) sas.trim_threshold);
			}
			if(sas.top_pad >= 0) {
			    as[l] = am_atom_put("tp", 2);
			    ts[l++] = erts_bld_uint(hpp, szp, (Uint) sas.top_pad);
			}

			opts = erts_bld_2tup_list(hpp, szp, l, as, ts);
			res = erts_bld_2tup_list(hpp, szp, 1, &opts_am, &opts);
			
			if (szp) {
			    ERTS_AIT_HALLOC((Process *) proc, sz);
			    szp = NULL;
			    hpp = &hp;
			    goto restart_sys_alloc;
			}
			ERTS_AIT_RET(res);
		    }
		    case ERTS_ALC_A_FIXED_SIZE: {
			ErtsAlcType_t n;
			Eterm as[2], vs[2];

			if (only_sz)
			    ERTS_AIT_RET(NIL);

			as[0] = am_atom_put("options", 7);
			as[1] = am_atom_put("pools", 5);

			szp = &sz;
			hpp = NULL;

		    restart_fix_alloc:

			vs[0] = erts_bld_cons(hpp, szp,
					      erts_bld_tuple(hpp, szp, 2, 
							     am_atom_put("e",
									 1),
							     am_true),
					      NIL);

			vs[1] = NIL;
			for (n = ERTS_ALC_N_MIN_A_FIXED_SIZE;
			     n <= ERTS_ALC_N_MAX_A_FIXED_SIZE;
			     n++) {
			    ErtsFixInfo efi;
			    erts_fix_info(ERTS_ALC_N2T(n), &efi);

			    vs[1] = erts_bld_cons(
				hpp, szp,
				erts_bld_tuple(
				    hpp, szp, 3,
				    am_atom_put((char *) ERTS_ALC_N2TD(n),
						strlen(ERTS_ALC_N2TD(n))),
				    erts_bld_uint(hpp, szp, efi.total),
				    erts_bld_uint(hpp, szp, efi.used)),
				vs[1]);

			}
			
			res = erts_bld_2tup_list(hpp, szp, 2, as, vs);
			if (szp) {
			    ERTS_AIT_HALLOC((Process *) proc, sz);
			    szp = NULL;
			    hpp = &hp;
			    goto restart_fix_alloc;
			}
			ERTS_AIT_RET(res);
		    }
		    default:
			ASSERT(0);
			goto done;
		    }
		}
	    }
	}
    }

    if (ERTS_IS_ATOM_STR("mseg_alloc", which_alloc)) {
#if HAVE_ERTS_MSEG
	if (only_sz)
	    ERTS_AIT_RET(NIL);
	erts_mseg_info(NULL, NULL, 0, NULL, &sz);
	if (sz)
	    ERTS_AIT_HALLOC((Process *) proc, sz);
	ERTS_AIT_RET(erts_mseg_info(NULL, NULL, 1, &hp, NULL));
#else
	ERTS_AIT_RET(am_false);
#endif

    }
    else if (ERTS_IS_ATOM_STR("alloc_util", which_alloc)) {
	if (only_sz)
	    ERTS_AIT_RET(NIL);
	erts_alcu_au_info_options(NULL, NULL, NULL, &sz);
	if (sz)
	    ERTS_AIT_HALLOC((Process *) proc, sz);
	ERTS_AIT_RET(erts_alcu_au_info_options(NULL, NULL, &hp, NULL));
    }

 done:
    if (hp) {
	ASSERT(hp_end >= hp);
	HRelease((Process *) proc, hp_end, hp);
    }
    return res;

#undef ERTS_AIT_RET
#undef ERTS_AIT_HALLOC
}

void
erts_allocator_info(int to, void *arg)
{
    ErtsAlcType_t a;

    ERTS_SMP_LC_ASSERT(erts_smp_is_system_blocked(0)
		       || (ERTS_IS_CRASH_DUMPING
			   && erts_smp_is_system_blocked(ERTS_BS_FLG_ALLOW_GC)));

    for (a = ERTS_ALC_A_MIN; a <= ERTS_ALC_A_MAX; a++) {
	int ai;
	for (ai = 0; ai == 0 || ai < erts_allctrs_info[a].thr_spec; ai++) {
	    if (erts_allctrs_info[a].thr_spec) {
		if (!erts_allctr_thr_spec[a].allctr[ai])
		    continue;
		erts_print(to, arg, "=allocator:%s[%d]\n",
			   ERTS_ALC_A2AD(a), ai);
	    }
	    else {
		erts_print(to, arg, "=allocator:%s\n", ERTS_ALC_A2AD(a));
	    }
	    if (!erts_allctrs_info[a].enabled)
		erts_print(to, arg, "option e: false\n");
	    else {
		if (erts_allctrs_info[a].alloc_util) {
		    void *as;
		    if (!erts_allctrs_info[a].thr_spec)
			as = erts_allctrs_info[a].extra;
		    else {
			ASSERT(erts_allctr_thr_spec[a].enabled);
			as = erts_allctr_thr_spec[a].allctr[ai];
		    }
		    /* Binary alloc has its own thread safety... */
		    erts_alcu_info(as, 0, &to, arg, NULL, NULL);
		}
		else {
		    switch (a) {
		    case ERTS_ALC_A_SYSTEM: {
			SysAllocStat sas;
			erts_print(to, arg, "option e: true\n");
#ifdef ELIB_ALLOC_IS_CLIB
			erts_print(to, arg, "option m: elib\n");
#else
			erts_print(to, arg, "option m: libc\n");
#endif
			sys_alloc_stat(&sas);
			if(sas.trim_threshold >= 0)
			    erts_print(to, arg, "option tt: %d\n", sas.trim_threshold);
			if(sas.top_pad >= 0)
			    erts_print(to, arg, "option tp: %d\n", sas.top_pad);
			break;
		    }
		    case ERTS_ALC_A_FIXED_SIZE: {
			ErtsAlcType_t n;
			erts_print(to, arg, "option e: true\n");

			for (n = ERTS_ALC_N_MIN_A_FIXED_SIZE;
			     n <= ERTS_ALC_N_MAX_A_FIXED_SIZE;
			     n++) {
			    ErtsFixInfo efi;
			    erts_fix_info(ERTS_ALC_N2T(n), &efi);
			    erts_print(to, arg, "%s: %lu %lu\n",
				       ERTS_ALC_N2TD(n),
				       efi.total,
				       efi.used);
			}
			break;
		    }
		    default:
			ASSERT(0);
			break;
		    }
		}
	    }
	}
    }

#if HAVE_ERTS_MSEG
    erts_print(to, arg, "=allocator:mseg_alloc\n");
    erts_mseg_info(&to, arg, 0, NULL, NULL);
#endif

    erts_print(to, arg, "=allocator:alloc_util\n");
    erts_alcu_au_info_options(&to, arg, NULL, NULL);

    erts_print(to, arg, "=allocator:instr\n");
    erts_print(to, arg, "option m: %s\n",
	       erts_instr_memory_map ? "true" : "false");
    erts_print(to, arg, "option s: %s\n",
	       erts_instr_stat ? "true" : "false");
    erts_print(to, arg, "option t: %s\n",
	       erts_mtrace_enabled ? "true" : "false");

}

Eterm
erts_allocator_options(void *proc)
{
#if HAVE_ERTS_MSEG
    int use_mseg = 0;
#endif
    Uint sz, *szp, *hp, **hpp;
    Eterm res, features, settings;
    Eterm atoms[ERTS_ALC_A_MAX-ERTS_ALC_A_MIN+5];
    Uint terms[ERTS_ALC_A_MAX-ERTS_ALC_A_MIN+5];
    int a, length;
    SysAllocStat sas;
    Uint *endp = NULL;

    sys_alloc_stat(&sas);

    /* First find out the heap size needed ... */
    hpp = NULL;
    szp = &sz;
    sz = 0;

 bld_term:

    length = 0;
    features = NIL;
    settings = NIL;

    for (a = ERTS_ALC_A_MIN; a <= ERTS_ALC_A_MAX; a++) {
	Eterm tmp = NIL;
	atoms[length] = am_atom_put((char *) ERTS_ALC_A2AD(a),
				    strlen(ERTS_ALC_A2AD(a)));
	if (erts_allctrs_info[a].enabled) {
	    if (erts_allctrs_info[a].alloc_util) {
		Allctr_t *allctr;
#if HAVE_ERTS_MSEG
		use_mseg++;
#endif
		if (erts_allctr_thr_spec[a].enabled)
		    allctr = erts_allctr_thr_spec[a].allctr[1];
		else
		    allctr = erts_allctrs_info[a].extra;
		tmp = erts_alcu_info_options(allctr, NULL, NULL, hpp, szp);
	    }
	    else {
		int l = 0;
		Eterm as[4];
		Eterm ts[4];

		as[l] = am_atom_put("e", 1);
		ts[l++] = am_true;

		switch (a) {
		case ERTS_ALC_A_SYSTEM:
#ifdef ELIB_ALLOC_IS_CLIB
		    as[l] = am_atom_put("m", 1);
		    ts[l++] = am_atom_put("elib", 4);
#else
		    as[l] = am_atom_put("m", 1);
		    ts[l++] = am_atom_put("libc", 4);
#endif
		    if(sas.trim_threshold >= 0) {
			as[l] = am_atom_put("tt", 2);
			ts[l++] = erts_bld_uint(hpp, szp,
						(Uint) sas.trim_threshold);
		    }
		    if(sas.top_pad >= 0) {
			as[l] = am_atom_put("tp", 2);
			ts[l++] = erts_bld_uint(hpp, szp, (Uint) sas.top_pad);
		    }
		    break;
		default:
		    break;
		}

		tmp = erts_bld_2tup_list(hpp, szp, l, as, ts);

	    }

	}
	else {
	    Eterm atom = am_atom_put("e", 1);
	    Eterm term = am_false;
	    tmp = erts_bld_2tup_list(hpp, szp, 1, &atom, &term);
	}

	terms[length++] = tmp;

    }

#if HAVE_ERTS_MSEG
    if (use_mseg) {
	atoms[length] = am_atom_put("mseg_alloc", 10);
	terms[length++] = erts_mseg_info_options(NULL, NULL, hpp, szp);
    }
#endif

    atoms[length] = am_atom_put("alloc_util", 10); 
    terms[length++] = erts_alcu_au_info_options(NULL, NULL, hpp, szp);

    {
	Eterm o[3], v[3];
	o[0] = am_atom_put("m", 1);
	v[0] = erts_instr_memory_map ? am_true : am_false;
	o[1] = am_atom_put("s", 1);
	v[1] = erts_instr_stat ? am_true : am_false;
	o[2] = am_atom_put("t", 1);
	v[2] = erts_mtrace_enabled ? am_true : am_false;

	atoms[length] = am_atom_put("instr", 5); 
	terms[length++] = erts_bld_2tup_list(hpp, szp, 3, o, v);
    }

    settings = erts_bld_2tup_list(hpp, szp, length, atoms, terms);

    length = 0;

    for (a = ERTS_ALC_A_MIN; a <= ERTS_ALC_A_MAX; a++) {
	if (erts_allctrs_info[a].enabled) {
	    terms[length++] = am_atom_put((char *) ERTS_ALC_A2AD(a),
					  strlen(ERTS_ALC_A2AD(a)));
	}
    }

#if HAVE_ERTS_MSEG
    if (use_mseg)
	terms[length++] = am_atom_put("mseg_alloc", 10);
#endif

    features = length ? erts_bld_list(hpp, szp, length, terms) : NIL;

#if defined(ELIB_ALLOC_IS_CLIB)
    {
	Eterm version;
	int i;
	int ver[5];
	i = sscanf(ERLANG_VERSION,
		   "%d.%d.%d.%d.%d",
		   &ver[0], &ver[1], &ver[2], &ver[3], &ver[4]);

	version = NIL;
	for(i--; i >= 0; i--)
	  version = erts_bld_cons(hpp, szp, make_small(ver[i]), version);

	res = erts_bld_tuple(hpp, szp, 4,
			     am_elib_malloc, version, features, settings);
    }
#elif defined(__GLIBC__)
    {
	Eterm AM_glibc = am_atom_put("glibc", 5);
	Eterm version;

	version = erts_bld_cons(hpp,
				szp,
				make_small(__GLIBC__),
#ifdef __GLIBC_MINOR__
				erts_bld_cons(hpp,
					      szp,
					      make_small(__GLIBC_MINOR__),
					      NIL)
#else
				NIL
#endif
	    );

	res = erts_bld_tuple(hpp, szp, 4,
			     AM_glibc, version, features, settings);
    }

#else /* unknown allocator */

    res = erts_bld_tuple(hpp, szp, 4,
			 am_undefined, NIL, features, settings);

#endif

    if (szp) {
	/* ... and then build the term */
	hp = HAlloc((Process *) proc, sz);
	endp = hp + sz;
	hpp = &hp;
	szp = NULL;
	goto bld_term;
    }

    ASSERT(endp >= hp);
    HRelease((Process *) proc, endp, hp);

    return res;
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * Deprecated functions                                                    *
 *                                                                         *
 * These functions are still defined since "non-OTP linked in drivers" may *
 * contain (illegal) calls to them.                                        *
\*                                                                         */

/* --- DO *NOT* USE THESE FUNCTIONS --- */

void *sys_alloc(Uint sz)
{ return erts_alloc_fnf(ERTS_ALC_T_UNDEF, sz); }
void *sys_realloc(void *ptr, Uint sz)
{ return erts_realloc_fnf(ERTS_ALC_T_UNDEF, ptr, sz); }
void sys_free(void *ptr)
{ erts_free(ERTS_ALC_T_UNDEF, ptr); }
void *safe_alloc(Uint sz)
{ return erts_alloc(ERTS_ALC_T_UNDEF, sz); }
void *safe_realloc(void *ptr, Uint sz)
{ return erts_realloc(ERTS_ALC_T_UNDEF, ptr, sz); }


/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * NOTE: erts_alc_test() is only supposed to be used for testing.            *
 *                                                                           *
 * Keep alloc_SUITE_data/allocator_test.h updated if changes are made        *
 * to erts_alc_test()                                                        *
\*                                                                           */
#define ERTS_ALC_TEST_ABORT erl_exit(ERTS_ABORT_EXIT, "%s:%d: Internal error\n")

unsigned long erts_alc_test(unsigned long op,
			    unsigned long a1,
			    unsigned long a2,
			    unsigned long a3)
{
    switch (op >> 8) {
    case 0x0:	return erts_alcu_test(op,  a1, a2);
    case 0x1:	return erts_gfalc_test(op, a1, a2);
    case 0x2:	return erts_bfalc_test(op, a1, a2);
    case 0x3:	return erts_afalc_test(op, a1, a2);
    case 0x4:	return erts_mseg_test(op,  a1, a2, a3);
    case 0xf:
	switch (op) {
	case 0xf00:
#ifdef USE_THREADS
	    if (((Allctr_t *) a1)->thread_safe)
		return (unsigned long) erts_alcu_alloc_ts(ERTS_ALC_T_UNDEF,
							  (void *) a1,
							  (Uint) a2);
	    else
#endif
		return (unsigned long) erts_alcu_alloc(ERTS_ALC_T_UNDEF,
						       (void *) a1,
						       (Uint) a2);
	case 0xf01:
#ifdef USE_THREADS
	    if (((Allctr_t *) a1)->thread_safe)
		return (unsigned long) erts_alcu_realloc_ts(ERTS_ALC_T_UNDEF,
							    (void *) a1,
							    (void *) a2,
							    (Uint) a3);
	    else
#endif
		return (unsigned long) erts_alcu_realloc(ERTS_ALC_T_UNDEF,
							 (void *) a1,
							 (void *) a2,
							 (Uint) a3);
	case 0xf02:
#ifdef USE_THREADS
	    if (((Allctr_t *) a1)->thread_safe)
		erts_alcu_free_ts(ERTS_ALC_T_UNDEF, (void *) a1, (void *) a2);
	    else
#endif
		erts_alcu_free(ERTS_ALC_T_UNDEF, (void *) a1, (void *) a2);
	    return 0;
	case 0xf03: {
	    Allctr_t *allctr;
	    struct au_init init;

	    SET_DEFAULT_ALLOC_OPTS(&init);
	    init.enable = 1;
	    init.atype = GOODFIT;
	    init.init.util.name_prefix = (char *) a1;
	    init.init.util.ts = a2 ? 1 : 0;

	    if ((char **) a3) {
		char **argv = (char **) a3;
		int i = 0;
		while (argv[i]) {
		    if (argv[i][0] == '-' && argv[i][1] == 't')
			handle_au_arg(&init, &argv[i][2], argv, &i);
		    else
			return (unsigned long) NULL;
		    i++;
		}
	    }

	    switch (init.atype) {
	    case GOODFIT:
		allctr = erts_gfalc_start((GFAllctr_t *)
					  erts_alloc(ERTS_ALC_T_UNDEF,
						     sizeof(GFAllctr_t)),
					  &init.init.gf,
					  &init.init.util);
		break;
	    case BESTFIT:
		allctr = erts_bfalc_start((BFAllctr_t *)
					  erts_alloc(ERTS_ALC_T_UNDEF,
						     sizeof(BFAllctr_t)),
					  &init.init.bf,
					  &init.init.util);
		break;
	    case AFIT:
		allctr = erts_afalc_start((AFAllctr_t *)
					  erts_alloc(ERTS_ALC_T_UNDEF,
							    sizeof(AFAllctr_t)),
					  &init.init.af,
					  &init.init.util);
		break;
	    default:
		ASSERT(0);
		allctr = NULL;
		break;
	    }

	    return (unsigned long) allctr;
	}
	case 0xf04:
	    erts_alcu_stop((Allctr_t *) a1);
	    erts_free(ERTS_ALC_T_UNDEF, (void *) a1);
	    break;
#ifdef USE_THREADS
	case 0xf05: return (unsigned long) 1;
	case 0xf06: return (unsigned long) ((Allctr_t *) a1)->thread_safe;
#ifdef ETHR_NO_FORKSAFETY
	case 0xf07: return (unsigned long) 0;
#else
	case 0xf07: return (unsigned long) ((Allctr_t *) a1)->thread_safe;
#endif
	case 0xf08: {
	    ethr_mutex *mtx = erts_alloc(ERTS_ALC_T_UNDEF, sizeof(ethr_mutex));
	    if (ethr_mutex_init(mtx) != 0)
		ERTS_ALC_TEST_ABORT;
	    return (unsigned long) mtx;
	}
	case 0xf09: {
	    ethr_mutex *mtx = (ethr_mutex *) a1;
	    if (ethr_mutex_destroy(mtx) != 0)
		ERTS_ALC_TEST_ABORT;
	    erts_free(ERTS_ALC_T_UNDEF, (void *) mtx);
	    break;
	}
	case 0xf0a:
	    if (ethr_mutex_lock((ethr_mutex *) a1) != 0)
		ERTS_ALC_TEST_ABORT;
	    break;
	case 0xf0b:
	    if (ethr_mutex_unlock((ethr_mutex *) a1) != 0)
		ERTS_ALC_TEST_ABORT;
	    break;
	case 0xf0c: {
	    ethr_cond *cnd = erts_alloc(ERTS_ALC_T_UNDEF, sizeof(ethr_cond));
	    if (ethr_cond_init(cnd) != 0)
		ERTS_ALC_TEST_ABORT;
	    return (unsigned long) cnd;
	}
	case 0xf0d: {
	    ethr_cond *cnd = (ethr_cond *) a1;
	    if (ethr_cond_destroy(cnd) != 0)
		ERTS_ALC_TEST_ABORT;
	    erts_free(ERTS_ALC_T_UNDEF, (void *) cnd);
	    break;
	}
	case 0xf0e:
	    if (ethr_cond_broadcast((ethr_cond *) a1) != 0)
		ERTS_ALC_TEST_ABORT;
	    break;
	case 0xf0f: {
	    int res;
	    do {
		res = ethr_cond_wait((ethr_cond *) a1, (ethr_mutex *) a2);
	    } while (res == EINTR);
	    if (res != 0)
		ERTS_ALC_TEST_ABORT;
	    break;
	}
	case 0xf10: {
	    ethr_tid *tid = erts_alloc(ERTS_ALC_T_UNDEF, sizeof(ethr_tid));
	    if (ethr_thr_create(tid,
				(void * (*)(void *)) a1,
				(void *) a2,
				NULL) != 0)
		ERTS_ALC_TEST_ABORT;
	    return (unsigned long) tid;
	}
	case 0xf11: {
	    ethr_tid *tid = (ethr_tid *) a1;
	    if (ethr_thr_join(*tid, NULL) != 0)
		ERTS_ALC_TEST_ABORT;
	    erts_free(ERTS_ALC_T_UNDEF, (void *) tid);
	    break;
	}
	case 0xf12:
	    ethr_thr_exit((void *) a1);
	    ERTS_ALC_TEST_ABORT;
	    break;
#endif /* #ifdef USE_THREADS */
	default:
	    break;
	}
	return (unsigned long) 0;
    default:
	break;
    }

    ASSERT(0);
    return ~((unsigned long) 0);
}

#ifdef DEBUG
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * Debug stuff                                                               *
\*                                                                           */

#if 0
#define PRINT_OPS
#else
#undef PRINT_OPS
#endif


#define FENCE_SZ		(3*sizeof(UWord))

#if defined(ARCH_64)
#define FENCE_PATTERN 0xABCDEF97ABCDEF97
#else
#define FENCE_PATTERN 0xABCDEF97
#endif

#define TYPE_PATTERN_MASK  ERTS_ALC_N_MASK
#define TYPE_PATTERN_SHIFT 16

#define FIXED_FENCE_PATTERN_MASK \
  (~((UWord) (TYPE_PATTERN_MASK << TYPE_PATTERN_SHIFT)))
#define FIXED_FENCE_PATTERN \
  (FENCE_PATTERN & FIXED_FENCE_PATTERN_MASK)

#define MK_PATTERN(T) \
  (FIXED_FENCE_PATTERN | (((T) & TYPE_PATTERN_MASK) << TYPE_PATTERN_SHIFT))

#define GET_TYPE_OF_PATTERN(P) \
  (((P) >> TYPE_PATTERN_SHIFT) & TYPE_PATTERN_MASK)


#ifdef  ERTS_ALLOC_UTIL_HARD_DEBUG
static void *check_memory_fence(void *ptr, Uint *size, ErtsAlcType_t n, int func);

void check_allocated_block( Uint type, void *blk)
{
    Uint dummy;
    check_memory_fence(blk, &dummy, ERTS_ALC_T2N(type), ERTS_ALC_O_FREE);
}

void check_allocators(void)
{
    int i;
    if (!erts_initialized)
	return;
    for (i = ERTS_ALC_A_MIN; i <= ERTS_ALC_A_MAX; ++i) {
	if (erts_allctrs_info[i].alloc_util) {
	    ErtsAllocatorFunctions_t *real_af = (ErtsAllocatorFunctions_t *) erts_allctrs[i].extra;
	    Allctr_t *allctr = real_af->extra;
	    Carrier_t *ct;
#ifdef USE_THREADS
	if (allctr->thread_safe)
	    erts_mtx_lock(&allctr->mutex);
#endif

	    if (allctr->check_mbc) {
		for (ct = allctr->mbc_list.first; ct; ct = ct->next) {
		    fprintf(stderr,"Checking allocator %d\r\n",i);
		    allctr->check_mbc(allctr,ct);
		}
	    }
#ifdef USE_THREADS
	if (allctr->thread_safe)
	    erts_mtx_unlock(&allctr->mutex);
#endif
	}
    }
}
#endif

static void *
set_memory_fence(void *ptr, Uint sz, ErtsAlcType_t n)
{
    UWord *ui_ptr;
    UWord pattern;

    if (!ptr)
	return NULL;

    ui_ptr = (UWord *) ptr;
    pattern = MK_PATTERN(n);
    
    *(ui_ptr++) = sz;
    *(ui_ptr++) = pattern;
    memcpy((void *) (((char *) ui_ptr)+sz), (void *) &pattern, sizeof(UWord));

    return (void *) ui_ptr;
}

static void *
check_memory_fence(void *ptr, Uint *size, ErtsAlcType_t n, int func)
{
    Uint sz;
    Uint found_type;
    UWord pre_pattern;
    UWord post_pattern;
    UWord *ui_ptr;

    if (!ptr)
	return NULL;

    ui_ptr = (UWord *) ptr;
    pre_pattern = *(--ui_ptr);
    *size = sz = *(--ui_ptr);

    found_type = GET_TYPE_OF_PATTERN(pre_pattern);
    if (pre_pattern != MK_PATTERN(n)) {
	if ((FIXED_FENCE_PATTERN_MASK & pre_pattern) != FIXED_FENCE_PATTERN)
	    erl_exit(ERTS_ABORT_EXIT,
		     "ERROR: Fence at beginning of memory block (p=0x%u) "
		     "clobbered.\n",
		     (unsigned long) ptr);
    }

    memcpy((void *) &post_pattern, (void *) (((char *)ptr)+sz), sizeof(UWord));

    if (post_pattern != MK_PATTERN(n)
	|| pre_pattern != post_pattern) {
	char fbuf[10];
	char obuf[10];
	char *ftype;
	char *otype;
	char *op_str;

	if ((FIXED_FENCE_PATTERN_MASK & post_pattern) != FIXED_FENCE_PATTERN)
	    erl_exit(ERTS_ABORT_EXIT,
		     "ERROR: Fence at end of memory block (p=0x%u, sz=%u) "
		     "clobbered.\n",
		     (unsigned long) ptr, (unsigned long) sz);
	if (found_type != GET_TYPE_OF_PATTERN(post_pattern))
	    erl_exit(ERTS_ABORT_EXIT,
		     "ERROR: Fence around memory block (p=0x%u, sz=%u) "
		     "clobbered.\n",
		     (unsigned long) ptr, (unsigned long) sz);

	ftype = type_no_str(found_type);
	if (!ftype) {
	    sprintf(fbuf, "%d", (int) found_type);
	    ftype = fbuf;
	}
	otype = type_no_str(n);
	if (!otype) {
	    sprintf(obuf, "%d", (int) n);
	    otype = obuf;
	}

	switch (func) {
	case ERTS_ALC_O_ALLOC:		op_str = "allocated";	break;
	case ERTS_ALC_O_REALLOC:	op_str = "reallocated";	break;
	case ERTS_ALC_O_FREE:		op_str = "freed";	break;
	default:			op_str = "???";		break;
	}

	erl_exit(ERTS_ABORT_EXIT,
		 "ERROR: Memory block (p=0x%u, sz=%u) allocated as type \"%s\","
		 " but %s as type \"%s\".\n",
		 (unsigned long) ptr, (unsigned long) sz, ftype, op_str, otype);
    }

    return (void *) ui_ptr;
}

static ErtsAllocatorFunctions_t real_allctrs[ERTS_ALC_A_MAX+1];

static void *
debug_alloc(ErtsAlcType_t n, void *extra, Uint size)
{
    ErtsAllocatorFunctions_t *real_af = (ErtsAllocatorFunctions_t *) extra;
    Uint dsize;
    void *res;

    ASSERT(ERTS_ALC_N_MIN <= n && n <= ERTS_ALC_N_MAX);
    dsize = size + FENCE_SZ;
    res = (*real_af->alloc)(n, real_af->extra, dsize);

    res = set_memory_fence(res, size, n);

#ifdef PRINT_OPS
    fprintf(stderr, "0x%lx = alloc(%s, %lu)\r\n",
	    (Uint) res, ERTS_ALC_N2TD(n), size);
#endif

    return res;
}


static void *
debug_realloc(ErtsAlcType_t n, void *extra, void *ptr, Uint size)
{
    ErtsAllocatorFunctions_t *real_af = (ErtsAllocatorFunctions_t *) extra;
    Uint dsize;
    Uint old_size;
    void *dptr;
    void *res;

    ASSERT(ERTS_ALC_N_MIN <= n && n <= ERTS_ALC_N_MAX);

    dsize = size + FENCE_SZ;
    dptr = check_memory_fence(ptr, &old_size, n, ERTS_ALC_O_REALLOC);

    if (old_size > size)
	sys_memset((void *) (((char *) ptr) + size),
		   0xf,
		   sizeof(Uint) + old_size - size);

    res = (*real_af->realloc)(n, real_af->extra, dptr, dsize);
    
    res = set_memory_fence(res, size, n);

#ifdef PRINT_OPS
    fprintf(stderr, "0x%lx = realloc(%s, 0x%lx, %lu)\r\n",
	    (Uint) res, ERTS_ALC_N2TD(n), (Uint) ptr, size);
#endif

    return res;
}

static void
debug_free(ErtsAlcType_t n, void *extra, void *ptr)
{
    ErtsAllocatorFunctions_t *real_af = (ErtsAllocatorFunctions_t *) extra;
    void *dptr;
    Uint size;

    ASSERT(ERTS_ALC_N_MIN <= n && n <= ERTS_ALC_N_MAX);

    dptr = check_memory_fence(ptr, &size, n, ERTS_ALC_O_FREE);

    sys_memset((void *) dptr, n, size + FENCE_SZ);

    (*real_af->free)(n, real_af->extra, dptr);

#ifdef PRINT_OPS
    fprintf(stderr, "free(%s, 0x%lx)\r\n", ERTS_ALC_N2TD(n), (Uint) ptr);
#endif

}

static Uint
install_debug_functions(void)
{
    int i;
    ASSERT(sizeof(erts_allctrs) == sizeof(real_allctrs));

    sys_memcpy((void *)real_allctrs,(void *)erts_allctrs,sizeof(erts_allctrs));

    for (i = ERTS_ALC_A_MIN; i <= ERTS_ALC_A_MAX; i++) {
	erts_allctrs[i].alloc	= debug_alloc;
	erts_allctrs[i].realloc	= debug_realloc;
	erts_allctrs[i].free	= debug_free;
	erts_allctrs[i].extra	= (void *) &real_allctrs[i];
    }
    return FENCE_SZ;
}



#endif /* #ifdef DEBUG */
