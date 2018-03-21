/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2002-2018. All Rights Reserved.
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
 * Description:	Management of memory allocators.
 *
 * Author: 	Rickard Green
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif
#define ERTS_ALLOC_C__
#define ERTS_ALC_INTERNAL__
#define ERTS_WANT_MEM_MAPPERS
#include "sys.h"
#define ERL_THREADS_EMU_INTERNAL__
#include "erl_threads.h"
#include "global.h"
#include "erl_db.h"
#include "erl_binary.h"
#include "erl_bits.h"
#include "erl_instrument.h"
#include "erl_mseg.h"
#include "erl_monitor_link.h"
#include "erl_hl_timer.h"
#include "erl_cpu_topology.h"
#include "erl_thr_queue.h"
#include "erl_nfunc_sched.h"
#if defined(ERTS_ALC_T_DRV_SEL_D_STATE) || defined(ERTS_ALC_T_DRV_EV_D_STATE)
#include "erl_check_io.h"
#endif
#include "erl_bif_unique.h"

#define GET_ERL_GF_ALLOC_IMPL
#include "erl_goodfit_alloc.h"
#define GET_ERL_BF_ALLOC_IMPL
#include "erl_bestfit_alloc.h"
#define GET_ERL_AF_ALLOC_IMPL
#include "erl_afit_alloc.h"
#define GET_ERL_AOFF_ALLOC_IMPL
#include "erl_ao_firstfit_alloc.h"


#if ERTS_MAX_NO_OF_SCHEDULERS > ERTS_AU_MAX_PREF_ALLOC_INSTANCES
#  error "Too many schedulers; cannot create that many pref alloc instances"
#endif

#define ERTS_ALC_FIX_TYPE_IX(T) \
  (ERTS_ALC_T2N((T)) - ERTS_ALC_N_MIN_A_FIXED_SIZE)

#define ERTS_ALC_DEFAULT_MAX_THR_PREF ERTS_MAX_NO_OF_SCHEDULERS

#if defined(SMALL_MEMORY) || defined(PURIFY) || defined(VALGRIND)
#define AU_ALLOC_DEFAULT_ENABLE(X)	0
#else
#define AU_ALLOC_DEFAULT_ENABLE(X)	(X)
#endif

#define ERTS_ALC_DEFAULT_ENABLED_ACUL 60
#define ERTS_ALC_DEFAULT_ENABLED_ACUL_EHEAP_ALLOC 45
#define ERTS_ALC_DEFAULT_ENABLED_ACUL_LL_ALLOC 85

#define ERTS_ALC_DEFAULT_ACUL ERTS_ALC_DEFAULT_ENABLED_ACUL
#define ERTS_ALC_DEFAULT_ACUL_EHEAP_ALLOC ERTS_ALC_DEFAULT_ENABLED_ACUL_EHEAP_ALLOC
#define ERTS_ALC_DEFAULT_ACUL_LL_ALLOC ERTS_ALC_DEFAULT_ENABLED_ACUL_LL_ALLOC


#ifdef DEBUG
static Uint install_debug_functions(void);
#if 0
#define HARD_DEBUG
#ifdef __GNUC__
#warning "* * * * * * * * * * * * * *"
#warning "* HARD DEBUG IS ENABLED!  *"
#warning "* * * * * * * * * * * * * *"
#endif
#endif
#endif

static int lock_all_physical_memory = 0;

ErtsAllocatorFunctions_t ERTS_WRITE_UNLIKELY(erts_allctrs[ERTS_ALC_A_MAX+1]);
ErtsAllocatorInfo_t erts_allctrs_info[ERTS_ALC_A_MAX+1];
ErtsAllocatorThrSpec_t ERTS_WRITE_UNLIKELY(erts_allctr_thr_spec[ERTS_ALC_A_MAX+1]);

#define ERTS_MIN(A, B) ((A) < (B) ? (A) : (B))
#define ERTS_MAX(A, B) ((A) > (B) ? (A) : (B))

typedef union {
    GFAllctr_t gfa;
    char align_gfa[ERTS_ALC_CACHE_LINE_ALIGN_SIZE(sizeof(GFAllctr_t))];
    BFAllctr_t bfa;
    char align_bfa[ERTS_ALC_CACHE_LINE_ALIGN_SIZE(sizeof(BFAllctr_t))];
    AFAllctr_t afa;
    char align_afa[ERTS_ALC_CACHE_LINE_ALIGN_SIZE(sizeof(AFAllctr_t))];
    AOFFAllctr_t aoffa;
    char align_aoffa[ERTS_ALC_CACHE_LINE_ALIGN_SIZE(sizeof(AOFFAllctr_t))];
} ErtsAllocatorState_t;

static ErtsAllocatorState_t std_alloc_state;
static ErtsAllocatorState_t ll_alloc_state;
static ErtsAllocatorState_t sl_alloc_state;
static ErtsAllocatorState_t temp_alloc_state;
static ErtsAllocatorState_t eheap_alloc_state;
static ErtsAllocatorState_t binary_alloc_state;
static ErtsAllocatorState_t ets_alloc_state;
static ErtsAllocatorState_t driver_alloc_state;
static ErtsAllocatorState_t fix_alloc_state;
static ErtsAllocatorState_t literal_alloc_state;
#ifdef ERTS_ALC_A_EXEC
static ErtsAllocatorState_t exec_alloc_state;
#endif
static ErtsAllocatorState_t test_alloc_state;

enum {
    ERTS_ALC_INFO_A_ALLOC_UTIL = ERTS_ALC_A_MAX + 1,
    ERTS_ALC_INFO_A_MSEG_ALLOC,
    ERTS_ALC_INFO_A_ERTS_MMAP,
    ERTS_ALC_INFO_A_DISABLED_EXEC,  /* fake a disabled "exec_alloc" */
    ERTS_ALC_INFO_A_END
};

typedef struct {
    erts_atomic32_t refc;
    int only_sz;
    int internal;
    Uint req_sched;
    Process *proc;
    ErtsIRefStorage iref;
    int allocs[ERTS_ALC_INFO_A_END - ERTS_ALC_A_MIN + 1];
} ErtsAllocInfoReq;

ERTS_SCHED_PREF_QUICK_ALLOC_IMPL(aireq,
                                 ErtsAllocInfoReq,
                                 5,
                                 ERTS_ALC_T_AINFO_REQ)

ErtsAlcType_t erts_fix_core_allocator_ix;

enum allctr_type {
    GOODFIT,
    BESTFIT,
    AFIT,
    FIRSTFIT
};

struct au_init {
    int enable;
    int thr_spec;
    int disable_allowed;
    int thr_spec_allowed;
    int carrier_migration_allowed;
    enum allctr_type	atype;
    struct {
	AllctrInit_t	util;
	GFAllctrInit_t	gf;
	BFAllctrInit_t	bf;
	AFAllctrInit_t	af;
	AOFFAllctrInit_t aoff;
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
    ERTS_DEFAULT_AF_ALLCTR_INIT,	\
    ERTS_DEFAULT_AOFF_ALLCTR_INIT       \
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
    struct au_init fix_alloc;
    struct au_init literal_alloc;
    struct au_init exec_alloc;
    struct au_init test_alloc;
} erts_alc_hndl_args_init_t;

#define ERTS_AU_INIT__ {0, 0, 1, 1, 1, GOODFIT, DEFAULT_ALLCTR_INIT, {1,1,1,1}}

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
    ip->init.util.alloc_no	= ERTS_ALC_A_SHORT_LIVED;
#ifndef SMALL_MEMORY
    ip->init.util.mmbcs 	= 128*1024; /* Main carrier size */
#else
    ip->init.util.mmbcs 	= 32*1024; /* Main carrier size */
#endif
    ip->init.util.ts 		= ERTS_ALC_MTA_SHORT_LIVED;
    ip->init.util.rsbcst	= 80;
    ip->init.util.acul		= ERTS_ALC_DEFAULT_ACUL;
}

static void
set_default_std_alloc_opts(struct au_init *ip)
{
    SET_DEFAULT_ALLOC_OPTS(ip);
    ip->enable			= AU_ALLOC_DEFAULT_ENABLE(1);
    ip->thr_spec		= 1;
    ip->atype			= BESTFIT;
    ip->init.util.name_prefix	= "std_";
    ip->init.util.alloc_no	= ERTS_ALC_A_STANDARD;
#ifndef SMALL_MEMORY
    ip->init.util.mmbcs 	= 128*1024; /* Main carrier size */
#else
    ip->init.util.mmbcs 	= 32*1024; /* Main carrier size */
#endif
    ip->init.util.ts 		= ERTS_ALC_MTA_STANDARD;
    ip->init.util.acul		= ERTS_ALC_DEFAULT_ACUL;
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
    ip->init.util.acul		= ERTS_ALC_DEFAULT_ACUL_LL_ALLOC;
}

static void
set_default_literal_alloc_opts(struct au_init *ip)
{
    SET_DEFAULT_ALLOC_OPTS(ip);
    ip->enable			= 1;
    ip->thr_spec		= 0;
    ip->disable_allowed         = 0;
    ip->thr_spec_allowed        = 0;
    ip->carrier_migration_allowed = 0;
    ip->atype			= BESTFIT;
    ip->init.bf.ao		= 1;
    ip->init.util.ramv		= 0;
    ip->init.util.mmsbc		= 0;
    ip->init.util.sbct		= ~((UWord) 0);
    ip->init.util.name_prefix	= "literal_";
    ip->init.util.alloc_no	= ERTS_ALC_A_LITERAL;
#ifndef SMALL_MEMORY
    ip->init.util.mmbcs 	= 1024*1024; /* Main carrier size */
#else
    ip->init.util.mmbcs 	= 256*1024; /* Main carrier size */
#endif
    ip->init.util.ts 		= ERTS_ALC_MTA_LITERAL;
    ip->init.util.asbcst	= 0;
    ip->init.util.rsbcst	= 0;
    ip->init.util.rsbcmt	= 0;
    ip->init.util.rmbcmt	= 0;
    ip->init.util.acul		= 0;

#if defined(ARCH_32)
# if HAVE_ERTS_MSEG
    ip->init.util.mseg_alloc   = &erts_alcu_literal_32_mseg_alloc;
    ip->init.util.mseg_realloc = &erts_alcu_literal_32_mseg_realloc;
    ip->init.util.mseg_dealloc = &erts_alcu_literal_32_mseg_dealloc;
# endif
    ip->init.util.sys_alloc    = &erts_alcu_literal_32_sys_alloc;
    ip->init.util.sys_realloc  = &erts_alcu_literal_32_sys_realloc;
    ip->init.util.sys_dealloc  = &erts_alcu_literal_32_sys_dealloc;
#elif defined(ARCH_64)
# ifdef ERTS_HAVE_OS_PHYSICAL_MEMORY_RESERVATION
    ip->init.util.mseg_alloc    = &erts_alcu_mmapper_mseg_alloc;
    ip->init.util.mseg_realloc  = &erts_alcu_mmapper_mseg_realloc;
    ip->init.util.mseg_dealloc  = &erts_alcu_mmapper_mseg_dealloc;
    ip->init.util.mseg_mmapper  = &erts_literal_mmapper;
# endif
#else
# error Unknown architecture
#endif
}

#ifdef ERTS_ALC_A_EXEC
static void
set_default_exec_alloc_opts(struct au_init *ip)
{
    SET_DEFAULT_ALLOC_OPTS(ip);
    ip->enable			= 1;
    ip->thr_spec		= 0;
    ip->disable_allowed         = 0;
    ip->thr_spec_allowed        = 0;
    ip->carrier_migration_allowed = 0;
    ip->atype			= BESTFIT;
    ip->init.bf.ao		= 1;
    ip->init.util.ramv		= 0;
    ip->init.util.mmsbc		= 0;
    ip->init.util.sbct		= ~((UWord) 0);
    ip->init.util.name_prefix	= "exec_";
    ip->init.util.alloc_no	= ERTS_ALC_A_EXEC;
    ip->init.util.mmbcs 	= 0; /* No main carrier */
    ip->init.util.ts 		= ERTS_ALC_MTA_EXEC;
    ip->init.util.asbcst	= 0;
    ip->init.util.rsbcst	= 0;
    ip->init.util.rsbcmt	= 0;
    ip->init.util.rmbcmt	= 0;
    ip->init.util.acul		= 0;

    ip->init.util.mseg_alloc    = &erts_alcu_exec_mseg_alloc;
    ip->init.util.mseg_realloc  = &erts_alcu_exec_mseg_realloc;
    ip->init.util.mseg_dealloc  = &erts_alcu_exec_mseg_dealloc;
}
#endif /* ERTS_ALC_A_EXEC */

static void
set_default_temp_alloc_opts(struct au_init *ip)
{
    SET_DEFAULT_ALLOC_OPTS(ip);
    ip->enable			= AU_ALLOC_DEFAULT_ENABLE(1);
    ip->thr_spec		= 1;
    ip->disable_allowed         = 0;
    ip->carrier_migration_allowed = 0;
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
    ip->init.util.name_prefix	= "eheap_";
    ip->init.util.alloc_no	= ERTS_ALC_A_EHEAP;
#ifndef SMALL_MEMORY
    ip->init.util.mmbcs 	= 512*1024; /* Main carrier size */
#else
    ip->init.util.mmbcs 	= 256*1024; /* Main carrier size */
#endif
    ip->init.util.ts 		= ERTS_ALC_MTA_EHEAP;
    ip->init.util.rsbcst	= 50;
    ip->init.util.acul		= ERTS_ALC_DEFAULT_ACUL_EHEAP_ALLOC;
}

static void
set_default_binary_alloc_opts(struct au_init *ip)
{
    SET_DEFAULT_ALLOC_OPTS(ip);
    ip->enable			= AU_ALLOC_DEFAULT_ENABLE(1);
    ip->thr_spec		= 1;
    ip->atype			= BESTFIT;
    ip->init.util.name_prefix	= "binary_";
    ip->init.util.alloc_no	= ERTS_ALC_A_BINARY;
#ifndef SMALL_MEMORY
    ip->init.util.mmbcs 	= 128*1024; /* Main carrier size */
#else
    ip->init.util.mmbcs 	= 32*1024; /* Main carrier size */
#endif
    ip->init.util.ts 		= ERTS_ALC_MTA_BINARY;
    ip->init.util.acul		= ERTS_ALC_DEFAULT_ACUL;
}

static void
set_default_ets_alloc_opts(struct au_init *ip)
{
    SET_DEFAULT_ALLOC_OPTS(ip);
    ip->enable			= AU_ALLOC_DEFAULT_ENABLE(1);
    ip->thr_spec		= 1;
    ip->atype			= BESTFIT;
    ip->init.util.name_prefix	= "ets_";
    ip->init.util.alloc_no	= ERTS_ALC_A_ETS;
#ifndef SMALL_MEMORY
    ip->init.util.mmbcs 	= 128*1024; /* Main carrier size */
#else
    ip->init.util.mmbcs 	= 32*1024; /* Main carrier size */
#endif
    ip->init.util.ts 		= ERTS_ALC_MTA_ETS;
    ip->init.util.acul		= ERTS_ALC_DEFAULT_ACUL;
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
    ip->init.util.acul		= ERTS_ALC_DEFAULT_ACUL;
}

static void
set_default_fix_alloc_opts(struct au_init *ip,
			   size_t *fix_type_sizes)
{
    SET_DEFAULT_ALLOC_OPTS(ip);
    ip->enable			= AU_ALLOC_DEFAULT_ENABLE(1);
    ip->thr_spec		= 1;
    ip->atype			= BESTFIT;
    ip->init.bf.ao = 1;
    ip->init.util.name_prefix	= "fix_";
    ip->init.util.fix_type_size	= fix_type_sizes;
    ip->init.util.alloc_no	= ERTS_ALC_A_FIXED_SIZE;
#ifndef SMALL_MEMORY
    ip->init.util.mmbcs 	= 128*1024; /* Main carrier size */
#else
    ip->init.util.mmbcs 	= 128*1024; /* Main carrier size */
#endif
    ip->init.util.ts 		= ERTS_ALC_MTA_FIXED_SIZE;
    ip->init.util.acul		= ERTS_ALC_DEFAULT_ACUL;
}

static void
set_default_test_alloc_opts(struct au_init *ip)
{
    SET_DEFAULT_ALLOC_OPTS(ip);
    ip->enable			= 0; /* Disabled by default */
    ip->thr_spec		= -1 * erts_no_schedulers;
    ip->atype			= FIRSTFIT;
    ip->init.aoff.crr_order     = FF_AOFF;
    ip->init.aoff.blk_order     = FF_BF;
    ip->init.util.name_prefix	= "test_";
    ip->init.util.alloc_no	= ERTS_ALC_A_TEST;
    ip->init.util.mmbcs 	= 0; /* Main carrier size */
    ip->init.util.ts 		= ERTS_ALC_MTA_TEST;
    ip->init.util.acul		= ERTS_ALC_DEFAULT_ACUL;

    /* Use a constant minimal MBC size */
#if ERTS_SA_MB_CARRIERS
    ip->init.util.smbcs = ERTS_SACRR_UNIT_SZ;
    ip->init.util.lmbcs = ERTS_SACRR_UNIT_SZ;
    ip->init.util.sbct  = ERTS_SACRR_UNIT_SZ;
#else
    ip->init.util.smbcs = 1 << 12;
    ip->init.util.lmbcs = 1 << 12;
    ip->init.util.sbct  = 1 << 12;
#endif
}



static void
adjust_tpref(struct au_init *ip, int no_sched)
{
    if (ip->thr_spec) {
	ip->thr_spec = no_sched;
	ip->thr_spec *= -1; /* thread preferred */

	/* If default ... */

	/* ... shrink main multi-block carrier size */
	if (ip->default_.mmbcs)
	    ip->init.util.mmbcs /= ERTS_MIN(4, no_sched);
	/* ... shrink largest multi-block carrier size */
	if (ip->default_.lmbcs)
	    ip->init.util.lmbcs /= ERTS_MIN(2, no_sched);
	/* ... shrink smallest multi-block carrier size */
	if (ip->default_.smbcs)
	    ip->init.util.smbcs /= ERTS_MIN(4, no_sched);
    }
}


static void handle_args(int *, char **, erts_alc_hndl_args_init_t *);

static void
set_au_allocator(ErtsAlcType_t alctr_n, struct au_init *init, int ncpu);

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

#ifdef HARD_DEBUG
static void hdbg_init(void);
#endif

static void adjust_fix_alloc_sizes(UWord extra_block_size)
{
    
    if (extra_block_size && erts_allctrs_info[ERTS_ALC_A_FIXED_SIZE].enabled) {
	int j;

	if (erts_allctrs_info[ERTS_ALC_A_FIXED_SIZE].thr_spec) {
	    int i;
	    ErtsAllocatorThrSpec_t* tspec;

	    tspec = &erts_allctr_thr_spec[ERTS_ALC_A_FIXED_SIZE];	
	    ASSERT(tspec->enabled);

	    for (i=0; i < tspec->size; i++) {
		Allctr_t* allctr = tspec->allctr[i];
		for (j=0; j < ERTS_ALC_NO_FIXED_SIZES; ++j) {
		    allctr->fix[j].type_size += extra_block_size;
		}
	    }
	}
	else
	{
	    Allctr_t* allctr = erts_allctrs_info[ERTS_ALC_A_FIXED_SIZE].extra;
	    for (j=0; j < ERTS_ALC_NO_FIXED_SIZES; ++j) {
		allctr->fix[j].type_size += extra_block_size;
	    }	
	}
    }
}

static ERTS_INLINE int
strategy_support_carrier_migration(struct au_init *auip)
{
    /*
     * Currently only aoff* and ageff* support carrier
     * migration, i.e, type AOFIRSTFIT.
     */
    return auip->atype == FIRSTFIT;
}

static ERTS_INLINE void
adjust_carrier_migration_support(struct au_init *auip)
{
    if (auip->init.util.acul) {
	auip->thr_spec = -1; /* Need thread preferred */

	/*
	 * If strategy cannot handle carrier migration,
	 * default to a strategy that can...
	 */
	if (!strategy_support_carrier_migration(auip)) {
	    /* Default to aoffcbf */
	    auip->atype = FIRSTFIT;
	    auip->init.aoff.crr_order = FF_AOFF;
	    auip->init.aoff.blk_order = FF_BF;
	}
    }
}

void
erts_alloc_init(int *argc, char **argv, ErtsAllocInitOpts *eaiop)
{
    UWord extra_block_size = 0;
    int i, ncpu;
    erts_alc_hndl_args_init_t init = {
	0,
#if HAVE_ERTS_MSEG
	ERTS_MSEG_INIT_DEFAULT_INITIALIZER,
#endif
	ERTS_DEFAULT_TRIM_THRESHOLD,
	ERTS_DEFAULT_TOP_PAD,
	ERTS_DEFAULT_ALCU_INIT,
    };
    size_t fix_type_sizes[ERTS_ALC_NO_FIXED_SIZES] = {0};

    fix_type_sizes[ERTS_ALC_FIX_TYPE_IX(ERTS_ALC_T_PROC)]
	= sizeof(Process);
    fix_type_sizes[ERTS_ALC_FIX_TYPE_IX(ERTS_ALC_T_MONITOR)]
	= sizeof(ErtsMonitorDataHeap);
    fix_type_sizes[ERTS_ALC_FIX_TYPE_IX(ERTS_ALC_T_LINK)]
	= sizeof(ErtsLinkData);
    fix_type_sizes[ERTS_ALC_FIX_TYPE_IX(ERTS_ALC_T_DRV_SEL_D_STATE)]
	= sizeof(ErtsDrvSelectDataState);
    fix_type_sizes[ERTS_ALC_FIX_TYPE_IX(ERTS_ALC_T_NIF_SEL_D_STATE)]
        = sizeof(ErtsNifSelectDataState);
    fix_type_sizes[ERTS_ALC_FIX_TYPE_IX(ERTS_ALC_T_MSG_REF)]
	= sizeof(ErtsMessageRef);
    fix_type_sizes[ERTS_ALC_FIX_TYPE_IX(ERTS_ALC_T_THR_Q_EL_SL)]
	= sizeof(ErtsThrQElement_t);
    fix_type_sizes[ERTS_ALC_FIX_TYPE_IX(ERTS_ALC_T_LL_PTIMER)]
	= erts_timer_type_size(ERTS_ALC_T_LL_PTIMER);
    fix_type_sizes[ERTS_ALC_FIX_TYPE_IX(ERTS_ALC_T_HL_PTIMER)]
	= erts_timer_type_size(ERTS_ALC_T_HL_PTIMER);
    fix_type_sizes[ERTS_ALC_FIX_TYPE_IX(ERTS_ALC_T_BIF_TIMER)]
	= erts_timer_type_size(ERTS_ALC_T_BIF_TIMER);
    fix_type_sizes[ERTS_ALC_FIX_TYPE_IX(ERTS_ALC_T_NIF_EXP_TRACE)]
	= sizeof(NifExportTrace);
    fix_type_sizes[ERTS_ALC_FIX_TYPE_IX(ERTS_ALC_T_MREF_NSCHED_ENT)]
	= sizeof(ErtsNSchedMagicRefTableEntry);
    fix_type_sizes[ERTS_ALC_FIX_TYPE_IX(ERTS_ALC_T_MINDIRECTION)]
	= ERTS_MAGIC_BIN_UNALIGNED_SIZE(sizeof(ErtsMagicIndirectionWord));

#ifdef HARD_DEBUG
    hdbg_init();
#endif

    lock_all_physical_memory = 0;

    ncpu = eaiop->ncpu;
    if (ncpu < 1)
	ncpu = 1;

    erts_tsd_key_create(&erts_allctr_prelock_tsd_key,
			"erts_allctr_prelock_tsd_key");

    erts_sys_alloc_init();
    erts_init_utils_mem();

    set_default_sl_alloc_opts(&init.sl_alloc);
    set_default_std_alloc_opts(&init.std_alloc);
    set_default_ll_alloc_opts(&init.ll_alloc);
    set_default_temp_alloc_opts(&init.temp_alloc);
    set_default_eheap_alloc_opts(&init.eheap_alloc);
    set_default_binary_alloc_opts(&init.binary_alloc);
    set_default_ets_alloc_opts(&init.ets_alloc);
    set_default_driver_alloc_opts(&init.driver_alloc);
    set_default_fix_alloc_opts(&init.fix_alloc,
			       fix_type_sizes);
    set_default_literal_alloc_opts(&init.literal_alloc);
#ifdef ERTS_ALC_A_EXEC
    set_default_exec_alloc_opts(&init.exec_alloc);
#endif
    set_default_test_alloc_opts(&init.test_alloc);

    if (argc && argv)
	handle_args(argc, argv, &init);

    if (lock_all_physical_memory) {
#ifdef HAVE_MLOCKALL
	errno = 0;
	if (mlockall(MCL_CURRENT|MCL_FUTURE) != 0) {
	    int err = errno;
	    char *errstr = err ? strerror(err) : "unknown";
	    erts_exit(1, "Failed to lock physical memory: %s (%d)\n",
		     errstr, err);
	}
#else
	erts_exit(1, "Failed to lock physical memory: Not supported\n");
#endif
    }


    /* Make adjustments for carrier migration support */
    init.temp_alloc.init.util.acul = 0;
    adjust_carrier_migration_support(&init.sl_alloc);
    adjust_carrier_migration_support(&init.std_alloc);
    adjust_carrier_migration_support(&init.ll_alloc);
    adjust_carrier_migration_support(&init.eheap_alloc);
    adjust_carrier_migration_support(&init.binary_alloc);
    adjust_carrier_migration_support(&init.ets_alloc);
    adjust_carrier_migration_support(&init.driver_alloc);
    adjust_carrier_migration_support(&init.fix_alloc);
    adjust_carrier_migration_support(&init.literal_alloc);
#ifdef ERTS_ALC_A_EXEC
    adjust_carrier_migration_support(&init.exec_alloc);
#endif

    if (init.erts_alloc_config) {
	/* Adjust flags that erts_alloc_config won't like */

	/* No thread specific instances */
	init.temp_alloc.thr_spec = 0;
	init.sl_alloc.thr_spec = 0;
	init.std_alloc.thr_spec = 0;
	init.ll_alloc.thr_spec = 0;
	init.eheap_alloc.thr_spec = 0;
	init.binary_alloc.thr_spec = 0;
	init.ets_alloc.thr_spec = 0;
	init.driver_alloc.thr_spec = 0;
	init.fix_alloc.thr_spec = 0;
        init.literal_alloc.thr_spec = 0;
#ifdef ERTS_ALC_A_EXEC
        init.exec_alloc.thr_spec = 0;
#endif

	/* No carrier migration */
	init.temp_alloc.init.util.acul = 0;
	init.sl_alloc.init.util.acul = 0;
	init.std_alloc.init.util.acul = 0;
	init.ll_alloc.init.util.acul = 0;
	init.eheap_alloc.init.util.acul = 0;
	init.binary_alloc.init.util.acul = 0;
	init.ets_alloc.init.util.acul = 0;
	init.driver_alloc.init.util.acul = 0;
	init.fix_alloc.init.util.acul = 0;
        init.literal_alloc.init.util.acul = 0;
#ifdef ERTS_ALC_A_EXEC
        init.exec_alloc.init.util.acul = 0;
#endif
    }

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
    adjust_tpref(&init.fix_alloc, erts_no_schedulers);
    adjust_tpref(&init.literal_alloc, erts_no_schedulers);
#ifdef ERTS_ALC_A_EXEC
    adjust_tpref(&init.exec_alloc, erts_no_schedulers);
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
    refuse_af_strategy(&init.fix_alloc);
    refuse_af_strategy(&init.literal_alloc);
#ifdef ERTS_ALC_A_EXEC
    refuse_af_strategy(&init.exec_alloc);
#endif

    if (!init.temp_alloc.thr_spec)
	refuse_af_strategy(&init.temp_alloc);

    erts_mtrace_pre_init();
#if HAVE_ERTS_MSEG
    init.mseg.nos = erts_no_schedulers;
    erts_mseg_init(&init.mseg);
#endif

    erts_alcu_init(&init.alloc_util);
    erts_afalc_init();
    erts_bfalc_init();
    erts_gfalc_init();
    erts_aoffalc_init();

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

    erts_allctrs[ERTS_ALC_A_SYSTEM].alloc		= erts_sys_alloc;
    erts_allctrs[ERTS_ALC_A_SYSTEM].realloc		= erts_sys_realloc;
    erts_allctrs[ERTS_ALC_A_SYSTEM].free		= erts_sys_free;
    erts_allctrs_info[ERTS_ALC_A_SYSTEM].enabled	= 1;

    set_au_allocator(ERTS_ALC_A_TEMPORARY, &init.temp_alloc, ncpu);
    set_au_allocator(ERTS_ALC_A_SHORT_LIVED, &init.sl_alloc, ncpu);
    set_au_allocator(ERTS_ALC_A_STANDARD, &init.std_alloc, ncpu);
    set_au_allocator(ERTS_ALC_A_LONG_LIVED, &init.ll_alloc, ncpu);
    set_au_allocator(ERTS_ALC_A_EHEAP, &init.eheap_alloc, ncpu);
    set_au_allocator(ERTS_ALC_A_BINARY, &init.binary_alloc, ncpu);
    set_au_allocator(ERTS_ALC_A_ETS, &init.ets_alloc, ncpu);
    set_au_allocator(ERTS_ALC_A_DRIVER, &init.driver_alloc, ncpu);
    set_au_allocator(ERTS_ALC_A_FIXED_SIZE, &init.fix_alloc, ncpu);
    set_au_allocator(ERTS_ALC_A_LITERAL, &init.literal_alloc, ncpu);
#ifdef ERTS_ALC_A_EXEC
    set_au_allocator(ERTS_ALC_A_EXEC, &init.exec_alloc, ncpu);
#endif
    set_au_allocator(ERTS_ALC_A_TEST, &init.test_alloc, ncpu);

    for (i = ERTS_ALC_A_MIN; i <= ERTS_ALC_A_MAX; i++) {
	if (!erts_allctrs[i].alloc)
	    erts_exit(ERTS_ABORT_EXIT,
		     "Missing alloc function for %s\n", ERTS_ALC_A2AD(i));
	if (!erts_allctrs[i].realloc)
	    erts_exit(ERTS_ABORT_EXIT,
		     "Missing realloc function for %s\n", ERTS_ALC_A2AD(i));
	if (!erts_allctrs[i].free)
	    erts_exit(ERTS_ABORT_EXIT,
		     "Missing free function for %s\n", ERTS_ALC_A2AD(i));
    }

    sys_alloc_opt(SYS_ALLOC_OPT_TRIM_THRESHOLD, init.trim_threshold);
    sys_alloc_opt(SYS_ALLOC_OPT_TOP_PAD, init.top_pad);

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

    start_au_allocator(ERTS_ALC_A_FIXED_SIZE,
		       &init.fix_alloc,
		       &fix_alloc_state);
    start_au_allocator(ERTS_ALC_A_LITERAL,
                       &init.literal_alloc,
                       &literal_alloc_state);
#ifdef ERTS_ALC_A_EXEC
    start_au_allocator(ERTS_ALC_A_EXEC,
                       &init.exec_alloc,
                       &exec_alloc_state);
#endif
    start_au_allocator(ERTS_ALC_A_TEST,
		       &init.test_alloc,
		       &test_alloc_state);

    erts_mtrace_install_wrapper_functions();
    extra_block_size += erts_instr_init(init.instr.stat, init.instr.map);

    init_aireq_alloc();

#ifdef DEBUG
    extra_block_size += install_debug_functions();
#endif
    adjust_fix_alloc_sizes(extra_block_size);
}

void
erts_alloc_late_init(void)
{

}

static void *
erts_realloc_fixed_size(ErtsAlcType_t type, void *extra, void *p, Uint size)
{
    erts_exit(ERTS_ABORT_EXIT,
	     "Attempt to reallocate a block of the fixed size type %s\n",
	     ERTS_ALC_T2TD(type));
}


static void
set_au_allocator(ErtsAlcType_t alctr_n, struct au_init *init, int ncpu)
{
    ErtsAllocatorFunctions_t *af = &erts_allctrs[alctr_n];
    ErtsAllocatorInfo_t *ai = &erts_allctrs_info[alctr_n];
    ErtsAllocatorThrSpec_t *tspec = &erts_allctr_thr_spec[alctr_n];

    /*
     * Some allocators are forced on if halfword heap is used.
     */
    if (init->init.util.force)
	init->enable = 1;

    tspec->enabled = 0;
    tspec->dd = 0;
    tspec->aix = alctr_n;
    tspec->size	= 0;
    ai->thr_spec = 0;

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

    if (init->thr_spec) {
	if (init->thr_spec > 0) {
	    af->alloc = erts_alcu_alloc_thr_spec;
	    if (init->init.util.fix_type_size)
		af->realloc = erts_realloc_fixed_size;
	    else if (init->init.util.ramv)
		af->realloc = erts_alcu_realloc_mv_thr_spec;
	    else
		af->realloc = erts_alcu_realloc_thr_spec;
	    af->free = erts_alcu_free_thr_spec;
	}
	else {
	    af->alloc = erts_alcu_alloc_thr_pref;
	    if (init->init.util.fix_type_size)
		af->realloc = erts_realloc_fixed_size;
	    else if (init->init.util.ramv)
		af->realloc = erts_alcu_realloc_mv_thr_pref;
	    else
		af->realloc = erts_alcu_realloc_thr_pref;
	    af->free = erts_alcu_free_thr_pref;
	    tspec->dd = 1;
	}

	tspec->enabled	= 1;
	tspec->size	= abs(init->thr_spec) + 1;

	ai->thr_spec	= tspec->size;
    }
    else
	if (init->init.util.ts) {
	af->alloc = erts_alcu_alloc_ts;
	if (init->init.util.fix_type_size)
	    af->realloc = erts_realloc_fixed_size;
	else if (init->init.util.ramv)
	    af->realloc = erts_alcu_realloc_mv_ts;
	else
	    af->realloc = erts_alcu_realloc_ts;
	af->free = erts_alcu_free_ts;
    }
    else
    {
        erts_exit(ERTS_ABORT_EXIT, "%salloc is not thread safe\n",
                 init->init.util.name_prefix);
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
    ErtsAlcFixList_t *fix_lists = NULL;
    size_t fix_list_size = 0;

    if (!init->enable)
	return;

    if (init->thr_spec) {
	char *states = erts_sys_alloc(0,
				      NULL,
				      ((sizeof(Allctr_t *)
					* (tspec->size + 1))
				       + (sizeof(ErtsAllocatorState_t)
					  * tspec->size)
				       + ERTS_CACHE_LINE_SIZE - 1));
	if (!states)
	    erts_exit(ERTS_ABORT_EXIT,
		     "Failed to allocate allocator states for %salloc\n",
		     init->init.util.name_prefix);
	tspec->allctr = (Allctr_t **) states;
	states += sizeof(Allctr_t *) * (tspec->size + 1);
	states = ((((UWord) states) & ERTS_CACHE_LINE_MASK)
		  ? (char *) ((((UWord) states) & ~ERTS_CACHE_LINE_MASK)
			      + ERTS_CACHE_LINE_SIZE)
		  : (char *) states);
	tspec->allctr[0] = (Allctr_t *) state;
	size = tspec->size;
	for (i = 1; i < size; i++)
	    tspec->allctr[i] = (Allctr_t *)
		&((ErtsAllocatorState_t *) states)[i-1];
    }

    if (init->init.util.fix_type_size) {
	size_t tot_fix_list_size;
	fix_list_size = sizeof(ErtsAlcFixList_t)*ERTS_ALC_NO_FIXED_SIZES;
	fix_list_size = ERTS_ALC_CACHE_LINE_ALIGN_SIZE(fix_list_size);
	tot_fix_list_size = fix_list_size;
	if (init->thr_spec)
	    tot_fix_list_size *= tspec->size;
	fix_lists = erts_sys_alloc(0,
				   NULL,
				   (tot_fix_list_size
				    + ERTS_CACHE_LINE_SIZE - 1));
	if (!fix_lists)
	    erts_exit(ERTS_ABORT_EXIT,
		     "Failed to allocate fix lists for %salloc\n",
		     init->init.util.name_prefix);

	if (((UWord) fix_lists) & ERTS_CACHE_LINE_MASK)
		fix_lists = ((ErtsAlcFixList_t *)
		       ((((UWord) fix_lists) & ~ERTS_CACHE_LINE_MASK)
			+ ERTS_CACHE_LINE_SIZE));
    }

    for (i = 0; i < size; i++) {
	Allctr_t *as;
	atype = init->atype;

	if (!init->thr_spec)
	    as0 = state;
	else {
	    as0 = (void *) tspec->allctr[i];
	    if (!as0)
		continue;
	    if (init->thr_spec < 0) {
		init->init.util.ts = i == 0;
		init->init.util.tspec = 0;
		init->init.util.tpref = -1*init->thr_spec + 1;
	    }
	    else {
		if (i != 0)
		    init->init.util.ts = 0;
		else {
		    if (atype == AFIT)
			atype = GOODFIT;
		    init->init.util.ts = 1;
		}
		init->init.util.tspec = init->thr_spec + 1;
		init->init.util.tpref = 0;
	    }   
	}

	if (fix_lists) {
	    init->init.util.fix = fix_lists;
	    fix_lists = ((ErtsAlcFixList_t *)
			 (((char *) fix_lists) + fix_list_size));
	}

	init->init.util.ix = i;

	switch (atype) {
	case GOODFIT:
	    as = erts_gfalc_start((GFAllctr_t *) as0,
					   &init->init.gf,
					   &init->init.util);
	    break;
	case BESTFIT:
	    as = erts_bfalc_start((BFAllctr_t *) as0,
					   &init->init.bf,
					   &init->init.util);
	    break;
	case AFIT:
	    as = erts_afalc_start((AFAllctr_t *) as0,
					   &init->init.af,
					   &init->init.util);
	    break;
	case FIRSTFIT:
	    as = erts_aoffalc_start((AOFFAllctr_t *) as0,
					     &init->init.aoff,
					     &init->init.util);
	    break;

	default:
	    as = NULL;
	    ASSERT(0);
	}

	if (!as)
	    erts_exit(ERTS_ABORT_EXIT,
		     "Failed to start %salloc\n", init->init.util.name_prefix);

	ASSERT(as == (void *) as0);
	af->extra = as;
    }

    if (init->thr_spec)
	af->extra = tspec;
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
    if (sys_strcmp(value, "true") == 0)
	return 1;
    else if (sys_strcmp(value, "false") == 0)
	return 0;
    else
	bad_value(param, param_end, value);
    return -1;
}

static Uint kb_to_bytes(Sint kb, Uint *bytes)
{
    const Uint max = ((~((Uint) 0))/1024) + 1;

    if (kb < 0 || (Uint)kb > max)
        return 0;
    if ((Uint)kb == max)
        *bytes = ~((Uint) 0);
    else
        *bytes = ((Uint) kb)*1024;
    return 1;
}

static Uint
get_kb_value(char *param_end, char** argv, int* ip)
{
    Sint tmp;
    Uint bytes = 0;
    char *rest;
    char *param = argv[*ip]+1;
    char *value = get_value(param_end, argv, ip);
    errno = 0;
    tmp = (Sint) ErtsStrToSint(value, &rest, 10);
    if (errno != 0 || rest == value || !kb_to_bytes(tmp, &bytes))
	bad_value(param, param_end, value);
    return bytes;
}

static UWord
get_mb_value(char *param_end, char** argv, int* ip)
{
    SWord tmp;
    UWord max = ((~((UWord) 0))/(1024*1024)) + 1;
    char *rest;
    char *param = argv[*ip]+1;
    char *value = get_value(param_end, argv, ip);
    errno = 0;
    tmp = (SWord) ErtsStrToSint(value, &rest, 10);
    if (errno != 0 || rest == value || tmp < 0 || max < ((UWord) tmp))
	bad_value(param, param_end, value);
    if (max == (UWord) tmp)
	return ~((UWord) 0);
    else
	return ((UWord) tmp)*1024*1024;
}


#if 0
static Uint
get_byte_value(char *param_end, char** argv, int* ip)
{
    Sint tmp;
    char *rest;
    char *param = argv[*ip]+1;
    char *value = get_value(param_end, argv, ip);
    errno = 0;
    tmp = (Sint) ErtsStrToSint(value, &rest, 10);
    if (errno != 0 || rest == value || tmp < 0)
	bad_value(param, param_end, value);
    return (Uint) tmp;
}
#endif

static Uint
get_amount_value(char *param_end, char** argv, int* ip)
{
    Sint tmp;
    char *rest;
    char *param = argv[*ip]+1;
    char *value = get_value(param_end, argv, ip);
    errno = 0;
    tmp = (Sint) ErtsStrToSint(value, &rest, 10);
    if (errno != 0 || rest == value || tmp < 0)
	bad_value(param, param_end, value);
    return (Uint) tmp;
}

static Uint
get_acul_value(struct au_init *auip, char *param_end, char** argv, int* ip)
{
    Sint tmp;
    char *rest;
    char *param = argv[*ip]+1;
    char *value = get_value(param_end, argv, ip);
    if (sys_strcmp(value, "de") == 0) {
	switch (auip->init.util.alloc_no) {
	case ERTS_ALC_A_LONG_LIVED:
	    return ERTS_ALC_DEFAULT_ENABLED_ACUL_LL_ALLOC;
	case ERTS_ALC_A_EHEAP:
	    return ERTS_ALC_DEFAULT_ENABLED_ACUL_EHEAP_ALLOC;
	default:
	    return ERTS_ALC_DEFAULT_ENABLED_ACUL;
	}
    }
    errno = 0;
    tmp = (Sint) ErtsStrToSint(value, &rest, 10);
    if (errno != 0 || rest == value || tmp < 0 || 100 < tmp)
	bad_value(param, param_end, value);
    return (Uint) tmp;
}

static void
handle_au_arg(struct au_init *auip,
	      char* sub_param,
	      char** argv,
	      int* ip,
	      int u_switch)
{
    char *param = argv[*ip]+1;

    switch (sub_param[0]) {
    case 'a':
        if (sub_param[1] == 'c') { /* Migration parameters "ac*" */
            UWord value;
            UWord* wp;
            if (!auip->carrier_migration_allowed && !u_switch)
                goto bad_switch;

            if (has_prefix("acul", sub_param)) {
                value = get_acul_value(auip, sub_param + 4, argv, ip);
                wp = &auip->init.util.acul;
            }
            else if (has_prefix("acnl", sub_param)) {
                value = get_amount_value(sub_param + 4, argv, ip);
                wp = &auip->init.util.acnl;
            }
            else if (has_prefix("acfml", sub_param)) {
                value = get_amount_value(sub_param + 5, argv, ip);
                wp = &auip->init.util.acfml;
            }
            else
                goto bad_switch;

            if (auip->carrier_migration_allowed)
                *wp = value;
        }
	else if(has_prefix("asbcst", sub_param)) {
	    auip->init.util.asbcst = get_kb_value(sub_param + 6, argv, ip);
	}
	else if(has_prefix("as", sub_param)) {
	    char *alg = get_value(sub_param + 2, argv, ip);
	    if (sys_strcmp("bf", alg) == 0) {
		auip->atype = BESTFIT;
		auip->init.bf.ao = 0;
	    }
	    else if (sys_strcmp("aobf", alg) == 0) {
		auip->atype = BESTFIT;
		auip->init.bf.ao = 1;
	    }
	    else if (sys_strcmp("gf", alg) == 0) {
		auip->atype = GOODFIT;
	    }
	    else if (sys_strcmp("af", alg) == 0) {
		auip->atype = AFIT;
	    }
	    else if (sys_strcmp("aoff", alg) == 0) {
		auip->atype = FIRSTFIT;
		auip->init.aoff.crr_order = FF_AOFF;
		auip->init.aoff.blk_order = FF_AOFF;
	    }
	    else if (sys_strcmp("aoffcbf", alg) == 0) {
		auip->atype = FIRSTFIT;
		auip->init.aoff.crr_order = FF_AOFF;
		auip->init.aoff.blk_order = FF_BF;
	    }
	    else if (sys_strcmp("aoffcaobf", alg) == 0) {
		auip->atype = FIRSTFIT;
		auip->init.aoff.crr_order = FF_AOFF;
		auip->init.aoff.blk_order = FF_AOBF;
	    }
            else if (sys_strcmp("ageffcaoff", alg) == 0) {
                auip->atype = FIRSTFIT;
		auip->init.aoff.crr_order = FF_AGEFF;
		auip->init.aoff.blk_order = FF_AOFF;
            }
            else if (sys_strcmp("ageffcbf", alg) == 0) {
                auip->atype = FIRSTFIT;
		auip->init.aoff.crr_order = FF_AGEFF;
		auip->init.aoff.blk_order = FF_BF;
            }
            else if (sys_strcmp("ageffcaobf", alg) == 0) {
                auip->atype = FIRSTFIT;
		auip->init.aoff.crr_order = FF_AGEFF;
		auip->init.aoff.blk_order = FF_AOBF;
            }
	    else {
		bad_value(param, sub_param + 1, alg);
	    }
	    if (!strategy_support_carrier_migration(auip))
		auip->init.util.acul = 0;
	}
	else
	    goto bad_switch;
	break;
    case 'e': {
	int e = get_bool_value(sub_param + 1, argv, ip);
        if (!auip->disable_allowed && !e) {
            if (!u_switch)
                bad_value(param, sub_param + 1, "false");
	    else
		ASSERT(auip->enable); /* ignore */
        }
	else auip->enable = e;
	break;
    }
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
	int res = get_bool_value(sub_param+1, argv, ip);
	if (res > 0) {
	    if (!auip->thr_spec_allowed) {
		if (!u_switch)
                    bad_value(param, sub_param + 1, "true");
		else
		    ASSERT(!auip->thr_spec); /* ignore */
	    }
	    else
		auip->thr_spec = 1;
	    break;
	}
	else if (res == 0) {
	    auip->thr_spec = 0;
	    auip->init.util.acul = 0;
	    break;
	}
	goto bad_switch;
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
	&init->fix_alloc,
	&init->sl_alloc
	/* test_alloc not affected by +Mea??? or +Mu???  */
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
		    handle_au_arg(&init->binary_alloc, &argv[i][3], argv, &i, 0);
		    break;
                case 'I':
                    if (has_prefix("scs", argv[i]+3)) {
#if HAVE_ERTS_MSEG
			init->mseg.literal_mmap.scs =
#endif
			    get_mb_value(argv[i]+6, argv, &i);
		    }
                    else
                        handle_au_arg(&init->literal_alloc, &argv[i][3], argv, &i, 0);
		    break;
                case 'X':
                    if (has_prefix("scs", argv[i]+3)) {
                        /* Ignore obsolete */
                        (void) get_mb_value(argv[i]+6, argv, &i);
                    }
                    else
                        handle_au_arg(&init->exec_alloc, &argv[i][3], argv, &i, 0);
                    break;
		case 'D':
		    handle_au_arg(&init->std_alloc, &argv[i][3], argv, &i, 0);
		    break;
		case 'E':
		    handle_au_arg(&init->ets_alloc, &argv[i][3], argv, &i, 0);
		    break;
		case 'F':
		    handle_au_arg(&init->fix_alloc, &argv[i][3], argv, &i, 0);
		    break;
		case 'H':
		    handle_au_arg(&init->eheap_alloc, &argv[i][3], argv, &i, 0);
		    break;
		case 'L':
		    handle_au_arg(&init->ll_alloc, &argv[i][3], argv, &i, 0);
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
		    else if (has_prefix("scs", argv[i]+3)) {
#if HAVE_ERTS_MSEG
			init->mseg.dflt_mmap.scs =
#endif
			    get_mb_value(argv[i]+6, argv, &i);
		    }
		    else if (has_prefix("sco", argv[i]+3)) {
#if HAVE_ERTS_MSEG
			init->mseg.dflt_mmap.sco =
#endif
			    get_bool_value(argv[i]+6, argv, &i);
		    }
		    else if (has_prefix("scrpm", argv[i]+3)) {
#if HAVE_ERTS_MSEG
			init->mseg.dflt_mmap.scrpm =
#endif
			    get_bool_value(argv[i]+8, argv, &i);
		    }
		    else if (has_prefix("scrfsd", argv[i]+3)) {
#if HAVE_ERTS_MSEG
			init->mseg.dflt_mmap.scrfsd =
#endif
			    get_amount_value(argv[i]+9, argv, &i);
		    }
		    else {
			bad_param(param, param+2);
		    }
		    break;
		case 'R':
		    handle_au_arg(&init->driver_alloc, &argv[i][3], argv, &i, 0);
		    break;
		case 'S':
		    handle_au_arg(&init->sl_alloc, &argv[i][3], argv, &i, 0);
		    break;
		case 'T':
		    handle_au_arg(&init->temp_alloc, &argv[i][3], argv, &i, 0);
		    break;
		case 'Z':
		    handle_au_arg(&init->test_alloc, &argv[i][3], argv, &i, 0);
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
			if (sys_strcmp("true", arg) != 0)
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
			if (sys_strcmp("min", arg) == 0) {
			    for (a = 0; a < aui_sz; a++)
				aui[a]->enable = 0;
			}
			else if (sys_strcmp("max", arg) == 0) {
			    for (a = 0; a < aui_sz; a++)
				aui[a]->enable = 1;
			}
			else if (sys_strcmp("config", arg) == 0) {
			    init->erts_alloc_config = 1;
			}
			else if (sys_strcmp("r9c", arg) == 0
				 || sys_strcmp("r10b", arg) == 0
				 || sys_strcmp("r11b", arg) == 0) {
			    set_default_sl_alloc_opts(&init->sl_alloc);
			    set_default_std_alloc_opts(&init->std_alloc);
			    set_default_ll_alloc_opts(&init->ll_alloc);
			    set_default_temp_alloc_opts(&init->temp_alloc);
			    set_default_eheap_alloc_opts(&init->eheap_alloc);
			    set_default_binary_alloc_opts(&init->binary_alloc);
			    set_default_ets_alloc_opts(&init->ets_alloc);
			    set_default_driver_alloc_opts(&init->driver_alloc);
			    set_default_driver_alloc_opts(&init->fix_alloc);

			    init->driver_alloc.enable = 0;
			    if (sys_strcmp("r9c", arg) == 0) {
				init->sl_alloc.enable = 0;
				init->std_alloc.enable = 0;
				init->binary_alloc.enable = 0;
				init->ets_alloc.enable = 0;
			    }

			    for (a = 0; a < aui_sz; a++) {
				aui[a]->thr_spec = 0;
				aui[a]->init.util.acul = 0;
				aui[a]->init.util.ramv = 0;
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
			if (sys_strcmp("true", arg) == 0)
			    init->instr.stat = 1;
			else if (sys_strcmp("false", arg) == 0)
			    init->instr.stat = 0;
			else
			    bad_value(param, param+3, arg);
			break;
		    case 'm':
			arg = get_value(argv[i]+4, argv, &i);
			if (sys_strcmp("true", arg) == 0)
			    init->instr.map = 1;
			else if (sys_strcmp("false", arg) == 0)
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
		case 'l':
		    if (has_prefix("pm", param+2)) {
			arg = get_value(argv[i]+5, argv, &i);
			if (sys_strcmp("all", arg) == 0)
			    lock_all_physical_memory = 1;
			else if (sys_strcmp("no", arg) == 0)
			    lock_all_physical_memory = 0;
			else
			    bad_value(param, param+4, arg);
			break;
		    }
		    bad_param(param, param+2);
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
		    else if (has_prefix("sac", argv[i]+3)) {
			init->alloc_util.sac
			    = get_bool_value(argv[i]+6, argv, &i);
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
			    handle_au_arg(aui[a], &argv[i][3], argv, &i, 1);
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
			    if(sys_strcmp(argv[i], "-sname") == 0
			       || sys_strcmp(argv[i], "-name") == 0) {
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

void
erts_alloc_register_scheduler(void *vesdp)
{
    ErtsSchedulerData *esdp = (ErtsSchedulerData *) vesdp;
    int ix = (int) esdp->no;
    int aix;

    ASSERT(!ERTS_SCHEDULER_IS_DIRTY(esdp));
    for (aix = ERTS_ALC_A_MIN; aix <= ERTS_ALC_A_MAX; aix++) {
	ErtsAllocatorThrSpec_t *tspec = &erts_allctr_thr_spec[aix];
	esdp->alloc_data.deallctr[aix] = NULL;
	esdp->alloc_data.pref_ix[aix] = -1;
	if (tspec->enabled) {
	    if (!tspec->dd)
		esdp->alloc_data.pref_ix[aix] = ix;
	    else {
		Allctr_t *allctr = tspec->allctr[ix];
		ASSERT(allctr);
		esdp->alloc_data.deallctr[aix] = allctr;
		esdp->alloc_data.pref_ix[aix] = ix;
	    }
	}
    }
}

void
erts_alloc_scheduler_handle_delayed_dealloc(void *vesdp,
					    int *need_thr_progress,
					    ErtsThrPrgrVal *thr_prgr_p,
					    int *more_work)
{
    ErtsSchedulerData *esdp = (ErtsSchedulerData *) vesdp;
    int aix;
    for (aix = ERTS_ALC_A_MIN; aix <= ERTS_ALC_A_MAX; aix++) {
	Allctr_t *allctr;
	if (esdp)
	    allctr = esdp->alloc_data.deallctr[aix];
	else {
	    ErtsAllocatorThrSpec_t *tspec = &erts_allctr_thr_spec[aix];
	    if (tspec->enabled && tspec->dd)
		allctr = tspec->allctr[0];
	    else
		allctr = NULL;
	}
	if (allctr) {
	    erts_alcu_check_delayed_dealloc(allctr,
					    1,
					    need_thr_progress,
					    thr_prgr_p,
					    more_work);
	}
    }
}

erts_aint32_t
erts_alloc_fix_alloc_shrink(int ix, erts_aint32_t flgs)
{
    ErtsAllocatorThrSpec_t *tspec;
    tspec = &erts_allctr_thr_spec[ERTS_ALC_A_FIXED_SIZE];
    if (erts_allctrs_info[ERTS_ALC_A_FIXED_SIZE].thr_spec && tspec->enabled)
	return erts_alcu_fix_alloc_shrink(tspec->allctr[ix], flgs);
    if (ix == 0 && erts_allctrs_info[ERTS_ALC_A_FIXED_SIZE].extra)
	return erts_alcu_fix_alloc_shrink(
	    erts_allctrs_info[ERTS_ALC_A_FIXED_SIZE].extra, flgs);
    return 0;
}

static void
no_verify(Allctr_t *allctr)
{

}

erts_alloc_verify_func_t
erts_alloc_get_verify_unused_temp_alloc(Allctr_t **allctr)
{
    if (erts_allctrs_info[ERTS_ALC_A_TEMPORARY].alloc_util
	&& erts_allctrs_info[ERTS_ALC_A_TEMPORARY].thr_spec) {
	ErtsAllocatorThrSpec_t *tspec;
	int ix = ERTS_ALC_GET_THR_IX();
	tspec = &erts_allctr_thr_spec[ERTS_ALC_A_TEMPORARY];

	if (ix < tspec->size) {
	    *allctr = tspec->allctr[ix];
	    return erts_alcu_verify_unused;
	}
    }

    *allctr = NULL;
    return no_verify;
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
	erts_snprintf(buf, sizeof(buf), "%d", (int) n);
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
	erts_exit(ERTS_ABORT_EXIT,
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
	erts_exit(ERTS_DUMP_EXIT,
		 "%s: Cannot %s %lu bytes of memory (of type \"%s\").\n",
		 allctr_str, op, size, t_str);
	break;
    }
    case ERTS_ALC_E_NOALLCTR:
	erts_exit(ERTS_ABORT_EXIT,
		 "erts_alloc: Unknown allocator type: %d\n",
		 ERTS_ALC_T2A(ERTS_ALC_N2T(n)));
	break;
    default:
	erts_exit(ERTS_ABORT_EXIT, "erts_alloc: Unknown error: %d\n", error);
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

static ERTS_INLINE UWord
alcu_size(ErtsAlcType_t ai, ErtsAlcUFixInfo_t *fi, int fisz)
{
    UWord res = 0;

    ASSERT(erts_allctrs_info[ai].enabled);
    ASSERT(erts_allctrs_info[ai].alloc_util);

    if (!erts_allctrs_info[ai].thr_spec) {
	Allctr_t *allctr = erts_allctrs_info[ai].extra;
	AllctrSize_t asize;
	erts_alcu_current_size(allctr, &asize, fi, fisz);
	res += asize.blocks;
    }
    else {
	ErtsAllocatorThrSpec_t *tspec = &erts_allctr_thr_spec[ai];
	int i;

	ASSERT(tspec->enabled);

	for (i = tspec->size - 1; i >= 0; i--) {
	    Allctr_t *allctr = tspec->allctr[i];
	    AllctrSize_t asize;
	    if (allctr) {
		erts_alcu_current_size(allctr, &asize, fi, fisz);
		res += asize.blocks;
	    }
	}
    }

    return res;
}

static ERTS_INLINE void
add_fix_values(UWord *ap, UWord *up, ErtsAlcUFixInfo_t *fi, ErtsAlcType_t type)
{
    int ix = ERTS_ALC_T2N(type) - ERTS_ALC_N_MIN_A_FIXED_SIZE;
    ASSERT(0 <= ix && ix < ERTS_ALC_NO_FIXED_SIZES);

    *ap += (UWord) fi[ix].allocated;
    *up += (UWord) fi[ix].used;
}

Eterm
erts_memory(fmtfn_t *print_to_p, void *print_to_arg, void *proc, Eterm earg)
{
/*
 * NOTE! When updating this function, make sure to also update
 *       erlang:memory/[0,1] in $ERL_TOP/erts/preloaded/src/erlang.erl
 */
#define ERTS_MEM_NEED_ALL_ALCU (!erts_instr_stat && want_tot_or_sys)
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
	UWord total;
	UWord processes;
	UWord processes_used;
	UWord system;
	UWord atom;
	UWord atom_used;
	UWord binary;
	UWord code;
	UWord ets;
	UWord maximum;
    } size = {0};
    Eterm atoms[sizeof(size)/sizeof(UWord)];
    UWord *uintps[sizeof(size)/sizeof(UWord)];
    Eterm euints[sizeof(size)/sizeof(UWord)];
    int want_tot_or_sys;
    int length;
    Eterm res = THE_NON_VALUE;
    ErtsAlcType_t ai;
    int only_one_value = 0;
    ErtsAlcUFixInfo_t fi[ERTS_ALC_NO_FIXED_SIZES] = {{0,0}};

    ERTS_LC_ASSERT(erts_thr_progress_is_blocking());

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

    /* All alloc_util allocators *have* to be enabled, except test_alloc */
    
    for (ai = ERTS_ALC_A_MIN; ai <= ERTS_ALC_A_MAX; ai++) {
	switch (ai) {
	case ERTS_ALC_A_SYSTEM:
        case ERTS_ALC_A_TEST:
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
    ASSERT(length <= sizeof(uintps)/sizeof(UWord));


    if (proc) {
	ERTS_LC_ASSERT(ERTS_PROC_LOCK_MAIN
			   == erts_proc_lc_my_proc_locks(proc));
	/* We'll need locks early in the lock order */
	erts_proc_unlock(proc, ERTS_PROC_LOCK_MAIN);
    }

    /* Calculate values needed... */

    want_tot_or_sys = want.total || want.system;

    if (ERTS_MEM_NEED_ALL_ALCU) {
	size.total = 0;

	for (ai = ERTS_ALC_A_MIN; ai <= ERTS_ALC_A_MAX; ai++) {
	    if (erts_allctrs_info[ai].alloc_util) {
		UWord *save;
		UWord asz;
		switch (ai) {
		case ERTS_ALC_A_TEMPORARY:
		     /*
		      * Often not thread safe and usually never
		      * contain any allocated memory.
		      */
		    continue;
                case ERTS_ALC_A_TEST:
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
		case ERTS_ALC_A_FIXED_SIZE:
		    asz = alcu_size(ai, fi, ERTS_ALC_NO_FIXED_SIZES);
		    size.total += asz;
		    continue;
		default:
		    save = NULL;
		    break;
		}
		asz = alcu_size(ai, NULL, 0);
		if (save)
		    *save = asz;
		size.total += asz;
	    }
	}
    }



    if (want_tot_or_sys || want.processes || want.processes_used) {
	UWord tmp;

	if (ERTS_MEM_NEED_ALL_ALCU)
	    tmp = size.processes;
	else {
	    alcu_size(ERTS_ALC_A_FIXED_SIZE,
		      fi, ERTS_ALC_NO_FIXED_SIZES);
	    tmp = alcu_size(ERTS_ALC_A_EHEAP, NULL, 0);
	}
	tmp += erts_ptab_mem_size(&erts_proc);
	tmp += erts_bif_timer_memory_size();

	size.processes = size.processes_used = tmp;

	add_fix_values(&size.processes,
		       &size.processes_used,
		       fi,
		       ERTS_ALC_T_PROC);
	add_fix_values(&size.processes,
		       &size.processes_used,
		       fi,
		       ERTS_ALC_T_MONITOR);
	add_fix_values(&size.processes,
		       &size.processes_used,
		       fi,
		       ERTS_ALC_T_LINK);
	add_fix_values(&size.processes,
		       &size.processes_used,
		       fi,
		       ERTS_ALC_T_MSG_REF);
	add_fix_values(&size.processes,
		       &size.processes_used,
		       fi,
		       ERTS_ALC_T_LL_PTIMER);
	add_fix_values(&size.processes,
		       &size.processes_used,
		       fi,
		       ERTS_ALC_T_HL_PTIMER);
	add_fix_values(&size.processes,
		       &size.processes_used,
		       fi,
		       ERTS_ALC_T_BIF_TIMER);
	add_fix_values(&size.processes,
		       &size.processes_used,
		       fi,
		       ERTS_ALC_T_NIF_EXP_TRACE);
    }

    if (want.atom || want.atom_used) {
	Uint reserved_atom_space, atom_space;
	erts_atom_get_text_space_sizes(&reserved_atom_space, &atom_space);
	size.atom = size.atom_used = atom_table_sz();

	if (want.atom)
	    size.atom += reserved_atom_space;

	if (want.atom_used)
	    size.atom_used += atom_space;
    }

    if (!ERTS_MEM_NEED_ALL_ALCU && want.binary)
	size.binary = alcu_size(ERTS_ALC_A_BINARY, NULL, 0);

    if (want.code) {
	size.code = module_table_sz();
	size.code += export_table_sz();
	size.code += export_entries_sz();
	size.code += erts_fun_table_sz();
	size.code += erts_ranges_sz();
	size.code += erts_total_code_size;
    }

    if (want.ets) {
	if (!ERTS_MEM_NEED_ALL_ALCU)
	    size.ets = alcu_size(ERTS_ALC_A_ETS, NULL, 0);
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
	fmtfn_t to = *print_to_p;
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

	erts_proc_lock(proc, ERTS_PROC_LOCK_MAIN);

	if (only_one_value) {
	    ASSERT(length == 1);
	    hsz = 0;
	    erts_bld_uword(NULL, &hsz, *uintps[0]);
	    hp = hsz ? HAlloc((Process *) proc, hsz) : NULL;
	    res = erts_bld_uword(&hp, NULL, *uintps[0]);
	}
	else {
	    Uint **hpp = NULL;
	    Uint *hszp = &hsz;
	    hsz = 0;

	    while (1) {
		int i;
		for (i = 0; i < length; i++)
		    euints[i] = erts_bld_uword(hpp, hszp, *uintps[i]);
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
erts_allocated_areas(fmtfn_t *print_to_p, void *print_to_arg, void *proc)
{
#define MAX_AA_VALUES (24)
    struct aa_values values[MAX_AA_VALUES];
    Eterm res = THE_NON_VALUE;
    int i, length;
    Uint reserved_atom_space, atom_space;

    if (proc) {
	ERTS_LC_ASSERT(ERTS_PROC_LOCK_MAIN
			   == erts_proc_lc_my_proc_locks(proc));

	/* We'll need locks early in the lock order */
	erts_proc_unlock(proc, ERTS_PROC_LOCK_MAIN);
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
	sizeof(ErtsPTab)*2			/* proc & port tables */
	+ erts_timer_wheel_memory_size();	/* Timer wheel */
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
    values[i].name = "export_list";
    values[i].ui[0] = export_entries_sz();
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
    values[i].ui[0] = erts_ranges_sz();
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
    values[i].name = "process_table";
    values[i].ui[0] = erts_ptab_mem_size(&erts_proc);
    i++;

    values[i].arity = 2;
    values[i].name = "port_table";
    values[i].ui[0] = erts_ptab_mem_size(&erts_port);
    i++;

    values[i].arity = 2;
    values[i].name = "ets_misc";
    values[i].ui[0] = erts_get_ets_misc_mem_size();
    i++;

    length = i;
    ASSERT(length <= MAX_AA_VALUES);

    if (print_to_p) {
	/* Print result... */
	fmtfn_t to = *print_to_p;
	void *arg = print_to_arg;

	erts_print(to, arg, "=allocated_areas\n");
	for (i = 0; i < length; i++) {
	    switch (values[i].arity) {
	    case 2:
		erts_print(to, arg, "%s: %beu\n",
			   values[i].name, values[i].ui[0]);
		break;
	    case 3:
		erts_print(to, arg, "%s: %beu %beu\n",
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

	erts_proc_lock(proc, ERTS_PROC_LOCK_MAIN);

	hpp = NULL;
	hsz = 0;
	hszp = &hsz;

	while (1) {
	    int i;
	    for (i = 0; i < length; i++) {
		Eterm atom;
		if (hpp)
		    atom = am_atom_put(values[i].name,
				       (int) sys_strlen(values[i].name));
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
     * Currently all allocators except sys_alloc are
     * alloc_util allocators.
     * Also hide test_alloc which is disabled by default
     * and only intended for our own testing.
     */
    sz = ((ERTS_ALC_A_MAX + 1 - ERTS_ALC_A_MIN) - 2)*2;
    ASSERT(sz > 0);
    hp = HAlloc((Process *) proc, sz);
    res = NIL;
    for (i = ERTS_ALC_A_MAX; i >= ERTS_ALC_A_MIN; i--) {
	switch (i) {
	case ERTS_ALC_A_SYSTEM:
        case ERTS_ALC_A_TEST:
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

void
erts_allocator_info(fmtfn_t to, void *arg)
{
    ErtsAlcType_t a;

    ERTS_LC_ASSERT(erts_thr_progress_is_blocking());

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
		    erts_alcu_info(as, 0, 0, &to, arg, NULL, NULL);
		}
		else {
		    switch (a) {
		    case ERTS_ALC_A_SYSTEM: {
			SysAllocStat sas;
			erts_print(to, arg, "option e: true\n");
			erts_print(to, arg, "option m: libc\n");
			sys_alloc_stat(&sas);
			if(sas.trim_threshold >= 0)
			    erts_print(to, arg, "option tt: %d\n", sas.trim_threshold);
			if(sas.top_pad >= 0)
			    erts_print(to, arg, "option tp: %d\n", sas.top_pad);
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
    {
	struct erts_mmap_info_struct emis;
	int max = (int) erts_no_schedulers;
	int i;
	for (i = 0; i <= max; i++) {
	    erts_print(to, arg, "=allocator:mseg_alloc[%d]\n", i);
	    erts_mseg_info(i, &to, arg, 0, 0, NULL, NULL);
	}
	erts_print(to, arg, "=allocator:erts_mmap.default_mmap\n");
	erts_mmap_info(&erts_dflt_mmapper, &to, arg, NULL, NULL, &emis);
#if defined(ARCH_64) && defined(ERTS_HAVE_OS_PHYSICAL_MEMORY_RESERVATION)
        erts_print(to, arg, "=allocator:erts_mmap.literal_mmap\n");
        erts_mmap_info(&erts_literal_mmapper, &to, arg, NULL, NULL, &emis);
#endif
    }
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
    Eterm atoms[ERTS_ALC_A_MAX-ERTS_ALC_A_MIN+7];
    Uint terms[ERTS_ALC_A_MAX-ERTS_ALC_A_MIN+7];
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
				    sys_strlen(ERTS_ALC_A2AD(a)));
	if (erts_allctrs_info[a].enabled) {
	    if (erts_allctrs_info[a].alloc_util) {
		Allctr_t *allctr;
#if HAVE_ERTS_MSEG
		use_mseg++;
#endif
		if (erts_allctr_thr_spec[a].enabled)
		    allctr = erts_allctr_thr_spec[a].allctr[0];
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
		    as[l] = am_atom_put("m", 1);
		    ts[l++] = am_atom_put("libc", 4);
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
	terms[length++] = erts_mseg_info_options(0, NULL, NULL, hpp, szp);
    }
#endif

    atoms[length] = am_atom_put("alloc_util", 10); 
    terms[length++] = erts_alcu_au_info_options(NULL, NULL, hpp, szp);

#if HAVE_ERTS_MMAP
    atoms[length] = ERTS_MAKE_AM("erts_mmap");
    terms[length++] = erts_mmap_info_options(&erts_dflt_mmapper, NULL, NULL,
                                             NULL, hpp, szp);
#endif
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

    atoms[length] = am_atom_put("lock_physical_memory", 20);
    terms[length++] = (lock_all_physical_memory
		       ? am_atom_put("all", 3)
		       : am_atom_put("no", 2));

    settings = erts_bld_2tup_list(hpp, szp, length, atoms, terms);

    length = 0;

    for (a = ERTS_ALC_A_MIN; a <= ERTS_ALC_A_MAX; a++) {
	if (erts_allctrs_info[a].enabled) {
	    terms[length++] = am_atom_put((char *) ERTS_ALC_A2AD(a),
					  sys_strlen(ERTS_ALC_A2AD(a)));
	}
    }

#if HAVE_ERTS_MSEG
    if (use_mseg)
	terms[length++] = am_atom_put("mseg_alloc", 10);
#endif
#if ERTS_HAVE_ERTS_SYS_ALIGNED_ALLOC
    terms[length++] = am_atom_put("sys_aligned_alloc", 17);
#endif
#if defined(ARCH_64) && defined(ERTS_HAVE_OS_PHYSICAL_MEMORY_RESERVATION)
    terms[length++] = ERTS_MAKE_AM("literal_mmap");
#endif
    features = length ? erts_bld_list(hpp, szp, length, terms) : NIL;

#if defined(__GLIBC__)
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

void *erts_alloc_permanent_cache_aligned(ErtsAlcType_t type, Uint size)
{
    UWord v = (UWord) erts_alloc(type, size + (ERTS_CACHE_LINE_SIZE-1)
#ifdef VALGRIND
				  + sizeof(UWord)
#endif
				 );

#ifdef VALGRIND
    {   /* Link them to avoid Leak_PossiblyLost */
	static UWord* first_in_list = NULL;
        *(UWord**)v = first_in_list;
	first_in_list = (UWord*) v;
	v += sizeof(UWord);
    }
#endif

    if (v & ERTS_CACHE_LINE_MASK) {
	v = (v & ~ERTS_CACHE_LINE_MASK) + ERTS_CACHE_LINE_SIZE;
    }
    ASSERT((v & ERTS_CACHE_LINE_MASK) == 0);
    return (void*)v;
}

static void
reply_alloc_info(void *vair)
{
    ErtsAllocInfoReq *air = (ErtsAllocInfoReq *) vair;
    Uint sched_id = erts_get_scheduler_id();
    int global_instances = air->req_sched == sched_id;
    ErtsProcLocks rp_locks;
    Process *rp = air->proc;
    Eterm ref_copy = NIL, ai_list, msg = NIL;
    Eterm *hp = NULL, *hp_start = NULL, *hp_end = NULL;
    Eterm **hpp;
    Uint sz, *szp;
    ErlOffHeap *ohp = NULL;
    ErtsMessage *mp = NULL;
#if HAVE_ERTS_MMAP
    struct erts_mmap_info_struct mmap_info_dflt;
# if defined(ARCH_64) && defined(ERTS_HAVE_OS_PHYSICAL_MEMORY_RESERVATION)
    struct erts_mmap_info_struct mmap_info_literal;
# endif
#endif
    int i;
    Eterm (*info_func)(Allctr_t *,
		       int,
		       int,
		       fmtfn_t *,
		       void *,
		       Uint **,
		       Uint *) = (air->only_sz
				  ? erts_alcu_sz_info
				  : erts_alcu_info);

    rp_locks = air->req_sched == sched_id ? ERTS_PROC_LOCK_MAIN : 0;

    sz = 0;
    hpp = NULL;
    szp = &sz;

    while (1) {

	if (hpp)
	    ref_copy = erts_iref_storage_make_ref(&air->iref,
                                                  hpp, ohp, 0);
	else
	    *szp += erts_iref_storage_heap_size(&air->iref);

	ai_list = NIL;
	for (i = 0; air->allocs[i] != ERTS_ALC_A_INVALID; i++);
	for (i--; i >= 0; i--) {
	    int ai = air->allocs[i];
	    Allctr_t *allctr;
	    Eterm ainfo;
	    Eterm alloc_atom;
	    if (global_instances) {
		switch (ai) {
		case ERTS_ALC_A_SYSTEM: {
		    alloc_atom = erts_bld_atom(hpp, szp, "sys_alloc");
		    ainfo = NIL;
		    if (!air->only_sz) {
			SysAllocStat sas;
			if (hpp)
			    sys_alloc_stat(&sas);
			if (szp) {
			    /* ensure ehough heap */
			    sas.top_pad = INT_MAX;
			    sas.trim_threshold = INT_MAX;
			}
			if (sas.top_pad >= 0) {
			    ainfo = erts_bld_cons(
				hpp, szp,
				erts_bld_tuple(
				    hpp, szp, 2,
				    erts_bld_atom(hpp, szp, "tp"),
				    erts_bld_uint(
					hpp, szp,
					(Uint) sas.top_pad)),
				ainfo);
			}
			if (sas.trim_threshold >= 0) {
			    ainfo = erts_bld_cons(
				hpp, szp,
				erts_bld_tuple(
				    hpp, szp, 2,
				    erts_bld_atom(hpp, szp, "tt"),
				    erts_bld_uint(
					hpp, szp,
					(Uint) sas.trim_threshold)),
				ainfo);
			}
			ainfo = erts_bld_cons(hpp, szp,
					      erts_bld_tuple(
						  hpp, szp, 2,
						  erts_bld_atom(hpp, szp,
								"m"),
						  erts_bld_atom(hpp, szp,
								"libc")),
					      ainfo);
			ainfo = erts_bld_cons(hpp, szp,
					      erts_bld_tuple(
						  hpp, szp, 2,
						  erts_bld_atom(hpp, szp,
								"e"),
						  am_true),
					      ainfo);
			ainfo = erts_bld_tuple(hpp, szp, 2,
					       erts_bld_atom(hpp, szp,
							     "options"),
					       ainfo);
			ainfo = erts_bld_cons(hpp, szp,ainfo,NIL);
		    }
		    ainfo = erts_bld_tuple(hpp, szp, 3,
					   alloc_atom,
					   make_small(0),
					   ainfo);
		    break;
		}
		case ERTS_ALC_INFO_A_ALLOC_UTIL:
		    alloc_atom = erts_bld_atom(hpp, szp, "alloc_util");
		    ainfo = (air->only_sz
			     ? NIL
			     : erts_alcu_au_info_options(NULL, NULL,
							 hpp, szp));
		    ainfo = erts_bld_tuple(hpp, szp, 3,
					   alloc_atom,
					   make_small(0),
					   ainfo);
		    break;
                case ERTS_ALC_INFO_A_ERTS_MMAP:
                    alloc_atom = erts_bld_atom(hpp, szp, "erts_mmap");
#if HAVE_ERTS_MMAP
                    ainfo = (air->only_sz ? NIL :
                             erts_mmap_info(&erts_dflt_mmapper, NULL, NULL,
                                            hpp, szp, &mmap_info_dflt));
                    ainfo = erts_bld_tuple3(hpp, szp,
                                            alloc_atom,
                                            erts_bld_atom(hpp,szp,"default_mmap"),
                                            ainfo);
#  if defined(ARCH_64) && defined(ERTS_HAVE_OS_PHYSICAL_MEMORY_RESERVATION)
                    ai_list = erts_bld_cons(hpp, szp,
                                            ainfo, ai_list);
                    ainfo = (air->only_sz ? NIL :
                             erts_mmap_info(&erts_literal_mmapper, NULL, NULL,
                                            hpp, szp, &mmap_info_literal));
                    ainfo = erts_bld_tuple3(hpp, szp,
                                            alloc_atom,
                                            erts_bld_atom(hpp,szp,"literal_mmap"),
                                            ainfo);
#  endif
#else  /* !HAVE_ERTS_MMAP */
                    ainfo = erts_bld_tuple2(hpp, szp, alloc_atom,
                                            am_false);
#endif
                    break;
		case ERTS_ALC_INFO_A_MSEG_ALLOC:
		    alloc_atom = erts_bld_atom(hpp, szp, "mseg_alloc");
#if HAVE_ERTS_MSEG
		    ainfo = erts_mseg_info(0, NULL, NULL, hpp != NULL,
                                           air->only_sz, hpp, szp);
		    ainfo = erts_bld_tuple3(hpp, szp,
                                            alloc_atom,
                                            make_small(0),
                                            ainfo);

#else
		    ainfo = erts_bld_tuple2(hpp, szp, alloc_atom,
                                            am_false);
#endif
		    break;
#ifndef ERTS_ALC_A_EXEC
		case ERTS_ALC_INFO_A_DISABLED_EXEC:
		    alloc_atom = erts_bld_atom(hpp, szp, "exec_alloc");
		    ainfo = erts_bld_tuple2(hpp, szp, alloc_atom, am_false);
		    break;
#endif
		default:
		    alloc_atom = erts_bld_atom(hpp, szp,
					       (char *) ERTS_ALC_A2AD(ai));
		    if (!erts_allctrs_info[ai].enabled) 
			ainfo = erts_bld_tuple(hpp, szp, 2, alloc_atom,
					       am_false);
		    else if (erts_allctrs_info[ai].alloc_util) {
			if (erts_allctrs_info[ai].thr_spec)
			    allctr = erts_allctr_thr_spec[ai].allctr[0];
			else
			    allctr = erts_allctrs_info[ai].extra;
			ainfo = info_func(allctr, air->internal, hpp != NULL,
					  NULL, NULL, hpp, szp);
			ainfo = erts_bld_tuple(hpp, szp, 3, alloc_atom,
					       make_small(0), ainfo);
		    }
		    else {
			erts_exit(ERTS_ABORT_EXIT, "%s:%d: internal error\n",
				 __FILE__, __LINE__);
		    }
		}
		ai_list = erts_bld_cons(hpp, szp,
					ainfo, ai_list);
	    }
	    switch (ai) {
	    case ERTS_ALC_A_SYSTEM:
            case ERTS_ALC_INFO_A_ALLOC_UTIL:
	    case ERTS_ALC_INFO_A_ERTS_MMAP:
	    case ERTS_ALC_INFO_A_DISABLED_EXEC:
		break;
	    case ERTS_ALC_INFO_A_MSEG_ALLOC:
#if HAVE_ERTS_MSEG
		alloc_atom = erts_bld_atom(hpp, szp, "mseg_alloc");
		ainfo = erts_mseg_info(sched_id, NULL, NULL,
                                       hpp != NULL, air->only_sz, hpp, szp);
		ainfo = erts_bld_tuple(hpp, szp, 3,
				       alloc_atom,
				       make_small(sched_id),
				       ainfo);
		ai_list = erts_bld_cons(hpp, szp, ainfo, ai_list);
#endif
		break;
	    default:
		if (erts_allctrs_info[ai].thr_spec) {
		    alloc_atom = erts_bld_atom(hpp, szp,
					       (char *) ERTS_ALC_A2AD(ai));
		    allctr = erts_allctr_thr_spec[ai].allctr[sched_id];
		    ainfo = info_func(allctr, air->internal, hpp != NULL, NULL,
				      NULL, hpp, szp);
		    ai_list = erts_bld_cons(hpp, szp,
					    erts_bld_tuple(
						hpp, szp,
						3,
						alloc_atom,
						make_small(sched_id),
						ainfo),
					    ai_list);
		}
		break;
	    }
	    msg = erts_bld_tuple(hpp, szp,
				 3,
				 ref_copy,
				 make_small(sched_id),
				 ai_list);

	}
	if (hpp)
	    break;

	mp = erts_alloc_message_heap(rp, &rp_locks, sz, &hp, &ohp);
	hp_start = hp;
	hp_end = hp + sz;
	szp = NULL;
	hpp = &hp;
    }

    if (hp != hp_end)
	erts_shrink_message_heap(&mp, rp, hp_start, hp, hp_end, &msg, 1);

    erts_queue_message(rp, rp_locks, mp, msg, am_system);

    if (air->req_sched == sched_id)
	rp_locks &= ~ERTS_PROC_LOCK_MAIN;
 
    erts_proc_unlock(rp, rp_locks);
    erts_proc_dec_refc(rp);

    if (erts_atomic32_dec_read_nob(&air->refc) == 0) {
        erts_iref_storage_clean(&air->iref);
	aireq_free(air);
    }
}

int
erts_request_alloc_info(struct process *c_p,
			Eterm ref,
			Eterm allocs,
			int only_sz,
			int internal)
{
    ErtsAllocInfoReq *air = aireq_alloc();
    Eterm req_ai[ERTS_ALC_INFO_A_END] = {0};
    Eterm alist;
    int airix = 0, ai;

    air->req_sched = erts_get_scheduler_id();

    air->only_sz = only_sz;

    air->internal = internal;

    air->proc = c_p;

    if (is_not_internal_ref(ref))
	return 0;

    erts_iref_storage_save(&air->iref, ref);

    if (is_not_list(allocs))
	return 0;

    alist = allocs;

    while (is_list(alist)) {
	int saved = 0;
	Eterm* consp = list_val(alist);
	Eterm alloc = CAR(consp);

	for (ai = ERTS_ALC_A_MIN; ai <= ERTS_ALC_A_MAX; ai++)
	    if (erts_is_atom_str(erts_alc_a2ad[ai], alloc, 0))
		goto save_alloc;
	if (erts_is_atom_str("mseg_alloc", alloc, 0)) {
	    ai = ERTS_ALC_INFO_A_MSEG_ALLOC;
	    goto save_alloc;
	}
        if (erts_is_atom_str("erts_mmap", alloc, 0)) {
            ai = ERTS_ALC_INFO_A_ERTS_MMAP;
            goto save_alloc;
        }
#ifndef ERTS_ALC_A_EXEC
	if (erts_is_atom_str("exec_alloc", alloc, 0)) {
	    ai = ERTS_ALC_INFO_A_DISABLED_EXEC;
	    goto save_alloc;
	}
#endif
	if (erts_is_atom_str("alloc_util", alloc, 0)) {
	    ai = ERTS_ALC_INFO_A_ALLOC_UTIL;
	save_alloc:
	    if (req_ai[ai])
		return 0;
	    air->allocs[airix++] = ai;
	    req_ai[ai] = 1;
	    saved = 1;
	}

	if (!saved)
	    return 0;

	alist = CDR(consp);
    }

    if (is_not_nil(alist))
	return 0;

    air->allocs[airix] = ERTS_ALC_A_INVALID;

    erts_atomic32_init_nob(&air->refc,
			       (erts_aint32_t) erts_no_schedulers);

    erts_proc_add_refc(c_p, (Sint) erts_no_schedulers);

    if (erts_no_schedulers > 1)
	erts_schedule_multi_misc_aux_work(1,
					  erts_no_schedulers,
					  reply_alloc_info,
					  (void *) air);

    reply_alloc_info((void *) air);

    return 1;
}

Eterm erts_alloc_set_dyn_param(Process* c_p, Eterm tuple)
{
    ErtsAllocatorThrSpec_t *tspec;
    ErtsAlcType_t ai;
    Allctr_t* allctr;
    Eterm* tp;
    Eterm res;

    if (!is_tuple_arity(tuple, 3))
        goto badarg;

    tp = tuple_val(tuple);

    /*
     * Ex: {ets_alloc, sbct, 256000}
     */
    if (!is_atom(tp[1]) || !is_atom(tp[2]) || !is_integer(tp[3]))
        goto badarg;

    for (ai = ERTS_ALC_A_MIN; ai <= ERTS_ALC_A_MAX; ai++)
        if (erts_is_atom_str(erts_alc_a2ad[ai], tp[1], 0))
            break;

    if (ai > ERTS_ALC_A_MAX)
        goto badarg;

    if (!erts_allctrs_info[ai].enabled ||
        !erts_allctrs_info[ai].alloc_util) {
        return am_notsup;
    }

    if (tp[2] == am_sbct) {
        Uint sbct;
        int i, ok;

        if (!term_to_Uint(tp[3], &sbct))
            goto badarg;

        tspec = &erts_allctr_thr_spec[ai];
        if (tspec->enabled) {
            ok = 0;
            for (i = 0; i < tspec->size; i++) {
                allctr = tspec->allctr[i];
                ok |= allctr->try_set_dyn_param(allctr, am_sbct, sbct);
            }
        }
        else {
            allctr = erts_allctrs_info[ai].extra;
            ok = allctr->try_set_dyn_param(allctr, am_sbct, sbct);
        }
        return ok ? am_ok : am_notsup;
    }
    return am_notsup;

badarg:
    ERTS_BIF_PREP_ERROR(res, c_p, EXC_BADARG);
    return res;
}

/* 
 * The allocator wrapper prelocking stuff below is about the locking order.
 * It only affects wrappers (erl_mtrace.c and erl_instrument.c) that keep locks
 * during alloc/realloc/free.
 *
 * Some query functions in erl_alloc_util.c lock the allocator mutex and then
 * use erts_printf that in turn may call the sys allocator through the wrappers.
 * To avoid breaking locking order these query functions first "pre-locks" all
 * allocator wrappers.
 */
ErtsAllocatorWrapper_t *erts_allctr_wrappers;
int erts_allctr_wrapper_prelocked = 0;
erts_tsd_key_t erts_allctr_prelock_tsd_key;

void erts_allctr_wrapper_prelock_init(ErtsAllocatorWrapper_t* wrapper)
{
    ASSERT(wrapper->lock && wrapper->unlock);
    wrapper->next = erts_allctr_wrappers;
    erts_allctr_wrappers = wrapper;
}

void erts_allctr_wrapper_pre_lock(void)
{
    if (erts_allctr_wrappers) {
	ErtsAllocatorWrapper_t* wrapper = erts_allctr_wrappers;
	for ( ; wrapper; wrapper = wrapper->next) {
	    wrapper->lock();
	}
	ASSERT(!erts_allctr_wrapper_prelocked);
	erts_allctr_wrapper_prelocked = 1;
	erts_tsd_set(erts_allctr_prelock_tsd_key, (void*)1);
    }
}

void erts_allctr_wrapper_pre_unlock(void)
{
    if (erts_allctr_wrappers) {
	ErtsAllocatorWrapper_t* wrapper = erts_allctr_wrappers;
	
	erts_allctr_wrapper_prelocked = 0;
	erts_tsd_set(erts_allctr_prelock_tsd_key, (void*)0);
	for ( ; wrapper; wrapper = wrapper->next) {
	    wrapper->unlock();
	}
    }
}


/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * NOTE: erts_alc_test() is only supposed to be used for testing.            *
 *                                                                           *
 * Keep alloc_SUITE_data/allocator_test.h updated if changes are made        *
 * to erts_alc_test()                                                        *
\*                                                                           */
#define ERTS_ALC_TEST_ABORT erts_exit(ERTS_ABORT_EXIT, "%s:%d: Internal error\n")

UWord erts_alc_test(UWord op, UWord a1, UWord a2, UWord a3)
{
    switch (op >> 8) {
    case 0x0:	return erts_alcu_test(op,  a1, a2);
    case 0x1:	return erts_gfalc_test(op, a1, a2);
    case 0x2:	return erts_bfalc_test(op, a1, a2);
    case 0x3:	return erts_afalc_test(op, a1, a2);
    case 0x4:	return erts_mseg_test(op,  a1, a2, a3);
    case 0x5:	return erts_aoffalc_test(op, a1, a2);
    case 0xf:
	switch (op) {
	case 0xf00:
	    if (((Allctr_t *) a1)->thread_safe)
		return (UWord) erts_alcu_alloc_ts(ERTS_ALC_T_UNDEF,
							  (void *) a1,
							  (Uint) a2);
	    else
		return (UWord) erts_alcu_alloc(ERTS_ALC_T_UNDEF,
						       (void *) a1,
						       (Uint) a2);
	case 0xf01:
	    if (((Allctr_t *) a1)->thread_safe)
		return (UWord) erts_alcu_realloc_ts(ERTS_ALC_T_UNDEF,
							    (void *) a1,
							    (void *) a2,
							    (Uint) a3);
	    else
		return (UWord) erts_alcu_realloc(ERTS_ALC_T_UNDEF,
							 (void *) a1,
							 (void *) a2,
							 (Uint) a3);
	case 0xf02:
	    if (((Allctr_t *) a1)->thread_safe)
		erts_alcu_free_ts(ERTS_ALC_T_UNDEF, (void *) a1, (void *) a2);
	    else
		erts_alcu_free(ERTS_ALC_T_UNDEF, (void *) a1, (void *) a2);
	    return 0;
	case 0xf03: {
	    Allctr_t *allctr;
	    struct au_init init;

	    SET_DEFAULT_ALLOC_OPTS(&init);
	    init.enable = 1;
	    init.atype = GOODFIT;
	    init.init.util.name_prefix = (char *) a1;
	    init.init.util.ts = 1;
	    if ((char **) a3) {
		char **argv = (char **) a3;
		int i = 0;
		while (argv[i]) {
		    if (argv[i][0] == '-' && argv[i][1] == 't')
			handle_au_arg(&init, &argv[i][2], argv, &i, 0);
		    else
			return (UWord) NULL;
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
	    case FIRSTFIT:
		allctr = erts_aoffalc_start((AOFFAllctr_t *)
					  erts_alloc(ERTS_ALC_T_UNDEF,
						     sizeof(AOFFAllctr_t)),
					  &init.init.aoff,
					  &init.init.util);
		break;

	    default:
		ASSERT(0);
		allctr = NULL;
		break;
	    }

	    return (UWord) allctr;
	}
	case 0xf04:
	    erts_alcu_stop((Allctr_t *) a1);
	    erts_free(ERTS_ALC_T_UNDEF, (void *) a1);
	    break;
	case 0xf05: return (UWord) 1;
	case 0xf06: return (UWord) ((Allctr_t *) a1)->thread_safe;
#ifdef ETHR_NO_FORKSAFETY
	case 0xf07: return (UWord) 0;
#else
	case 0xf07: return (UWord) ((Allctr_t *) a1)->thread_safe;
#endif
	case 0xf08: {
	    ethr_mutex *mtx = erts_alloc(ERTS_ALC_T_UNDEF, sizeof(ethr_mutex));
	    if (ethr_mutex_init(mtx) != 0)
		ERTS_ALC_TEST_ABORT;
	    return (UWord) mtx;
	}
	case 0xf09: {
	    ethr_mutex *mtx = (ethr_mutex *) a1;
	    if (ethr_mutex_destroy(mtx) != 0)
		ERTS_ALC_TEST_ABORT;
	    erts_free(ERTS_ALC_T_UNDEF, (void *) mtx);
	    break;
	}
	case 0xf0a:
	    ethr_mutex_lock((ethr_mutex *) a1);
	    break;
	case 0xf0b:
	    ethr_mutex_unlock((ethr_mutex *) a1);
	    break;
	case 0xf0c: {
	    ethr_cond *cnd = erts_alloc(ERTS_ALC_T_UNDEF, sizeof(ethr_cond));
	    if (ethr_cond_init(cnd) != 0)
		ERTS_ALC_TEST_ABORT;
	    return (UWord) cnd;
	}
	case 0xf0d: {
	    ethr_cond *cnd = (ethr_cond *) a1;
	    if (ethr_cond_destroy(cnd) != 0)
		ERTS_ALC_TEST_ABORT;
	    erts_free(ERTS_ALC_T_UNDEF, (void *) cnd);
	    break;
	}
	case 0xf0e:
	    ethr_cond_broadcast((ethr_cond *) a1);
	    break;
	case 0xf0f: {
	    int res;
	    do {
		res = ethr_cond_wait((ethr_cond *) a1, (ethr_mutex *) a2);
	    } while (res == EINTR);
	    break;
	}
	case 0xf10: {
	    ethr_tid *tid = erts_alloc(ERTS_ALC_T_UNDEF, sizeof(ethr_tid));
	    if (ethr_thr_create(tid,
				(void * (*)(void *)) a1,
				(void *) a2,
				NULL) != 0)
		ERTS_ALC_TEST_ABORT;
	    return (UWord) tid;
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
	case 0xf13: return (UWord) 1;
	case 0xf14: return (UWord) erts_alloc(ERTS_ALC_T_TEST, (Uint)a1);

	case 0xf15: erts_free(ERTS_ALC_T_TEST, (void*)a1); return 0;

        case 0xf16: return (UWord) erts_realloc(ERTS_ALC_T_TEST, (void*)a1, (Uint)a2);

	case 0xf17: {
            Uint extra_hdr_sz = UNIT_CEILING((Uint)a1);
	    ErtsAllocatorThrSpec_t* ts = &erts_allctr_thr_spec[ERTS_ALC_A_TEST];
	    Uint offset = ts->allctr[0]->mbc_header_size;
	    void* orig_creating_mbc = ts->allctr[0]->creating_mbc;
	    void* orig_destroying_mbc = ts->allctr[0]->destroying_mbc;
	    void* new_creating_mbc = *(void**)a2; /* inout arg */
	    void* new_destroying_mbc = *(void**)a3; /* inout arg */
	    int i;

	    for (i=0; i < ts->size; i++) {
		Allctr_t* ap = ts->allctr[i];
		if (ap->mbc_header_size != offset
		    || ap->creating_mbc != orig_creating_mbc
		    || ap->destroying_mbc != orig_destroying_mbc
		    || ap->mbc_list.first != NULL)
		    return -1;
	    }
	    for (i=0; i < ts->size; i++) {
		ts->allctr[i]->mbc_header_size += extra_hdr_sz;
		ts->allctr[i]->creating_mbc = new_creating_mbc;
		ts->allctr[i]->destroying_mbc = new_destroying_mbc;
	    }
	    *(void**)a2 = orig_creating_mbc;
	    *(void**)a3 = orig_destroying_mbc;
	    return offset;
	}
	case 0xf18: {
	    ErtsAllocatorThrSpec_t* ts = &erts_allctr_thr_spec[ERTS_ALC_A_TEST];
	    return ts->allctr[0]->largest_mbc_size;
	}
	default:
	    break;
	}
	return (UWord) 0;
    default:
	break;
    }

    ASSERT(0);
    return ~((UWord) 0);
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

#ifdef HARD_DEBUG
#define FENCE_SZ		(4*sizeof(UWord))
#else
#define FENCE_SZ		(3*sizeof(UWord))
#endif

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

#ifdef HARD_DEBUG

#define ERL_ALC_HDBG_MAX_MBLK 100000
#define ERTS_ALC_O_CHECK -1

typedef struct hdbg_mblk_ hdbg_mblk;
struct hdbg_mblk_ {
    hdbg_mblk *next;
    hdbg_mblk *prev;
    void *p;
    Uint s;
    ErtsAlcType_t n;
};

static hdbg_mblk hdbg_mblks[ERL_ALC_HDBG_MAX_MBLK];

static hdbg_mblk *free_hdbg_mblks;
static hdbg_mblk *used_hdbg_mblks;
static erts_mtx_t hdbg_mblk_mtx;

static void
hdbg_init(void)
{
    int i;
    for (i = 0; i < ERL_ALC_HDBG_MAX_MBLK-1; i++)
	hdbg_mblks[i].next = &hdbg_mblks[i+1];
    hdbg_mblks[ERL_ALC_HDBG_MAX_MBLK-1].next = NULL;
    free_hdbg_mblks = &hdbg_mblks[0];
    used_hdbg_mblks = NULL;
    erts_mtx_init(&hdbg_mblk_mtx, "erts_alloc_hard_debug", NIL,
        ERTS_LOCK_FLAGS_PROPERTY_STATIC | ERTS_LOCK_FLAGS_CATEGORY_ALLOCATOR);
}

static void *check_memory_fence(void *ptr,
				Uint *size,
				ErtsAlcType_t n,
				int func);
void erts_hdbg_chk_blks(void);

void
erts_hdbg_chk_blks(void)
{
    hdbg_mblk *mblk;

    erts_mtx_lock(&hdbg_mblk_mtx);
    for (mblk = used_hdbg_mblks; mblk; mblk = mblk->next) {
	Uint sz;
	check_memory_fence(mblk->p, &sz, mblk->n, ERTS_ALC_O_CHECK);
	ASSERT(sz == mblk->s);
    }
    erts_mtx_unlock(&hdbg_mblk_mtx);
}

static hdbg_mblk *
hdbg_alloc(void *p, Uint s, ErtsAlcType_t n)
{
    hdbg_mblk *mblk;

    erts_mtx_lock(&hdbg_mblk_mtx);
    mblk = free_hdbg_mblks;
    if (!mblk) {
	erts_fprintf(stderr,
		     "Ran out of debug blocks; please increase "
		     "ERL_ALC_HDBG_MAX_MBLK=%d and recompile!\n",
		     ERL_ALC_HDBG_MAX_MBLK);
	abort();
    }
    free_hdbg_mblks = mblk->next;

    mblk->p = p;
    mblk->s = s;
    mblk->n = n;

    mblk->next = used_hdbg_mblks;
    mblk->prev = NULL;
    if (used_hdbg_mblks)
	used_hdbg_mblks->prev = mblk;
    used_hdbg_mblks = mblk;
    erts_mtx_unlock(&hdbg_mblk_mtx);
    return (void *) mblk;
}

static void
hdbg_free(hdbg_mblk *mblk)
{
    erts_mtx_lock(&hdbg_mblk_mtx);
    if (mblk->next)
	mblk->next->prev = mblk->prev;
    if (mblk->prev)
	mblk->prev->next = mblk->next;
    else
	used_hdbg_mblks = mblk->next;

    mblk->next = free_hdbg_mblks;
    free_hdbg_mblks = mblk;
    erts_mtx_unlock(&hdbg_mblk_mtx);
}

#endif

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
	if (allctr->thread_safe)
	    erts_mtx_lock(&allctr->mutex);

	    if (allctr->check_mbc) {
		for (ct = allctr->mbc_list.first; ct; ct = ct->next) {
		    fprintf(stderr,"Checking allocator %d\r\n",i);
		    allctr->check_mbc(allctr,ct);
		}
	    }
	if (allctr->thread_safe)
	    erts_mtx_unlock(&allctr->mutex);
	}
    }
}
#endif

static void *
set_memory_fence(void *ptr, Uint sz, ErtsAlcType_t n)
{
    UWord *ui_ptr;
    UWord pattern;
#ifdef HARD_DEBUG
    hdbg_mblk **mblkpp;
#endif

    if (!ptr)
	return NULL;

    ui_ptr = (UWord *) ptr;
    pattern = MK_PATTERN(n);

#ifdef HARD_DEBUG
    mblkpp = (hdbg_mblk **) ui_ptr++;
#endif

    *(ui_ptr++) = sz;
    *(ui_ptr++) = pattern;
    sys_memcpy((void *) (((char *) ui_ptr)+sz), (void *) &pattern, sizeof(UWord));

#ifdef HARD_DEBUG
    *mblkpp = hdbg_alloc((void *) ui_ptr, sz, n);
#endif

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
#ifdef HARD_DEBUG
    hdbg_mblk *mblk;
#endif

    if (!ptr)
	return NULL;

    ui_ptr = (UWord *) ptr;
    pre_pattern = *(--ui_ptr);
    *size = sz = *(--ui_ptr);
#ifdef HARD_DEBUG
    mblk = (hdbg_mblk *) *(--ui_ptr);
#endif

    found_type = GET_TYPE_OF_PATTERN(pre_pattern);
    if (pre_pattern != MK_PATTERN(n)) {
	if ((FIXED_FENCE_PATTERN_MASK & pre_pattern) != FIXED_FENCE_PATTERN)
	    erts_exit(ERTS_ABORT_EXIT,
		     "ERROR: Fence at beginning of memory block (p=0x%u) "
		     "clobbered.\n",
		     (UWord) ptr);
    }

    sys_memcpy((void *) &post_pattern, (void *) (((char *)ptr)+sz), sizeof(UWord));

    if (post_pattern != MK_PATTERN(n)
	|| pre_pattern != post_pattern) {
	char fbuf[10];
	char obuf[10];
	char *ftype;
	char *otype;
	char *op_str;

	if ((FIXED_FENCE_PATTERN_MASK & post_pattern) != FIXED_FENCE_PATTERN)
	    erts_exit(ERTS_ABORT_EXIT,
		     "ERROR: Fence at end of memory block (p=0x%u, sz=%u) "
		     "clobbered.\n",
		     (UWord) ptr, (UWord) sz);
	if (found_type != GET_TYPE_OF_PATTERN(post_pattern))
	    erts_exit(ERTS_ABORT_EXIT,
		     "ERROR: Fence around memory block (p=0x%u, sz=%u) "
		     "clobbered.\n",
		     (UWord) ptr, (UWord) sz);

	ftype = type_no_str(found_type);
	if (!ftype) {
	    erts_snprintf(fbuf, sizeof(fbuf), "%d", (int) found_type);
	    ftype = fbuf;
	}
	otype = type_no_str(n);
	if (!otype) {
	    erts_snprintf(obuf, sizeof(obuf), "%d", (int) n);
	    otype = obuf;
	}

	switch (func) {
	case ERTS_ALC_O_ALLOC:		op_str = "allocated";	break;
	case ERTS_ALC_O_REALLOC:	op_str = "reallocated";	break;
	case ERTS_ALC_O_FREE:		op_str = "freed";	break;
	default:			op_str = "???";		break;
	}

	erts_exit(ERTS_ABORT_EXIT,
		 "ERROR: Memory block (p=0x%u, sz=%u) allocated as type \"%s\","
		 " but %s as type \"%s\".\n",
		 (UWord) ptr, (UWord) sz, ftype, op_str, otype);
    }

#ifdef HARD_DEBUG
    switch (func) {
    case ERTS_ALC_O_REALLOC:
    case ERTS_ALC_O_FREE:
	hdbg_free(mblk);
	break;
    default:
	break;
    }
#endif

    return (void *) ui_ptr;
}

static ErtsAllocatorFunctions_t real_allctrs[ERTS_ALC_A_MAX+1];

static void *
debug_alloc(ErtsAlcType_t n, void *extra, Uint size)
{
    ErtsAllocatorFunctions_t *real_af = (ErtsAllocatorFunctions_t *) extra;
    Uint dsize;
    void *res;

#ifdef HARD_DEBUG
    erts_hdbg_chk_blks();
#endif

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

#ifdef HARD_DEBUG
    erts_hdbg_chk_blks();
#endif

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
    int free_pattern = n;

    ASSERT(ERTS_ALC_N_MIN <= n && n <= ERTS_ALC_N_MAX);

    dptr = check_memory_fence(ptr, &size, n, ERTS_ALC_O_FREE);

#ifdef ERTS_ALC_A_EXEC
# if defined(__i386__) || defined(__x86_64__)
    if (ERTS_ALC_T2A(ERTS_ALC_N2T(n)) == ERTS_ALC_A_EXEC) {
        free_pattern = 0x0f; /* Illegal instruction */
    }
# endif
#endif
    sys_memset((void *) dptr, free_pattern, size + FENCE_SZ);

    (*real_af->free)(n, real_af->extra, dptr);

#ifdef PRINT_OPS
    fprintf(stderr, "free(%s, 0x%lx)\r\n", ERTS_ALC_N2TD(n), (Uint) ptr);
#endif

#ifdef HARD_DEBUG
    erts_hdbg_chk_blks();
#endif

}

static Uint
install_debug_functions(void)
{
    int i;
    ERTS_CT_ASSERT(sizeof(erts_allctrs) == sizeof(real_allctrs));

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
