/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2002-2013. All Rights Reserved.
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

#ifndef ERL_ALLOC_UTIL__
#define ERL_ALLOC_UTIL__

#define ERTS_ALCU_VSN_STR "3.0"

#include "erl_alloc_types.h"
#ifdef USE_THREADS
#define ERL_THREADS_EMU_INTERNAL__
#include "erl_threads.h"
#endif

#include "erl_mseg.h"
#include "lttng-wrapper.h"

#define ERTS_AU_PREF_ALLOC_BITS 11
#define ERTS_AU_MAX_PREF_ALLOC_INSTANCES (1 << ERTS_AU_PREF_ALLOC_BITS)

typedef struct Allctr_t_ Allctr_t;

typedef struct {
    UWord ycs;
    UWord mmc;
    int   sac;
} AlcUInit_t;

typedef struct {
    char *name_prefix;
    ErtsAlcType_t alloc_no;
    int force;
    int ix;
    int ts;
    int tspec;
    int tpref;
    int ramv;
    UWord sbct;
    UWord asbcst;
    UWord rsbcst;
    UWord rsbcmt;
    UWord rmbcmt;
    UWord mmbcs;
    UWord mmsbc;
    UWord mmmbc;
    UWord lmbcs;
    UWord smbcs;
    UWord mbcgs;
    int acul;

    void *fix;
    size_t *fix_type_size;

#if HAVE_ERTS_MSEG
    void* (*mseg_alloc)(Allctr_t*, Uint *size_p, Uint flags);
    void* (*mseg_realloc)(Allctr_t*, void *seg, Uint old_size, Uint *new_size_p);
    void  (*mseg_dealloc)(Allctr_t*, void *seg, Uint size, Uint flags);
#endif
    void* (*sys_alloc)(Allctr_t *allctr, Uint size, int superalign);
    void* (*sys_realloc)(Allctr_t *allctr, void *ptr, Uint size, Uint old_size, int superalign);
    void  (*sys_dealloc)(Allctr_t *allctr, void *ptr, Uint size, int superalign);
} AllctrInit_t;

typedef struct {
    UWord blocks;
    UWord carriers;
} AllctrSize_t;

typedef struct {
    UWord allocated;
    UWord used;
} ErtsAlcUFixInfo_t;

#ifndef SMALL_MEMORY

#define ERTS_DEFAULT_ALCU_INIT {                                           \
    1024*1024,		/* (bytes)  ycs:    sys_alloc carrier size       */\
    ~((UWord) 0),	/* (amount) mmc:    max mseg carriers            */\
    1			/* (bool)   sac:    sys_alloc carriers           */\
}

#define ERTS_DEFAULT_ALLCTR_INIT {                                         \
    NULL,                                                                  \
    ERTS_ALC_A_INVALID,	/* (number) alloc_no: allocator number           */\
    0,			/* (bool)   force:  force enabled                */\
    0,			/* (number) ix: instance index                   */\
    1,			/* (bool)   ts:     thread safe                  */\
    0,			/* (bool)   tspec:  thread specific              */\
    0,			/* (bool)   tpref:  thread preferred             */\
    0,			/* (bool)   ramv:   realloc always moves         */\
    512*1024,		/* (bytes)  sbct:   sbc threshold                */\
    2*1024*2024,	/* (amount) asbcst: abs sbc shrink threshold     */\
    20,			/* (%)      rsbcst: rel sbc shrink threshold     */\
    80,			/* (%)      rsbcmt: rel sbc move threshold       */\
    50,			/* (%)      rmbcmt: rel mbc move threshold       */\
    1024*1024,		/* (bytes)  mmbcs:  main multiblock carrier size */\
    256,		/* (amount) mmsbc:  max mseg sbcs                */\
    ~((UWord) 0),	/* (amount) mmmbc:  max mseg mbcs                */ \
    10*1024*1024,	/* (bytes)  lmbcs:  largest mbc size             */\
    1024*1024,		/* (bytes)  smbcs:  smallest mbc size            */\
    10,			/* (amount) mbcgs:  mbc growth stages            */\
    0,			/* (%)      acul:  abandon carrier utilization limit */\
    /* --- Data not options -------------------------------------------- */\
    NULL,		/* (ptr)    fix                                  */\
    NULL		/* (ptr)    fix_type_size                        */\
}

#else /* if SMALL_MEMORY */

#define ERTS_DEFAULT_ALCU_INIT {                                           \
    128*1024,		/* (bytes)  ycs:    sys_alloc carrier size       */\
    1024,      		/* (amount) mmc:    max mseg carriers            */\
    1			/* (bool)   sac:    sys_alloc carriers           */\
}

#define ERTS_DEFAULT_ALLCTR_INIT {                                         \
    NULL,                                                                  \
    ERTS_ALC_A_INVALID,	/* (number) alloc_no: allocator number           */\
    0,			/* (bool)   force:  force enabled                */\
    0,			/* (number) ix: instance index                   */\
    1,			/* (bool)   ts:     thread safe                  */\
    0,			/* (bool)   tspec:  thread specific              */\
    0,			/* (bool)   tpref:  thread preferred             */\
    0,			/* (bool)   ramv:   realloc always moves         */\
    64*1024,		/* (bytes)  sbct:   sbc threshold                */\
    2*1024*2024,	/* (amount) asbcst: abs sbc shrink threshold     */\
    20,			/* (%)      rsbcst: rel sbc shrink threshold     */\
    80,			/* (%)      rsbcmt: rel sbc move threshold       */\
    128*1024,		/* (bytes)  mmbcs:  main multiblock carrier size */\
    256,		/* (amount) mmsbc:  max mseg sbcs                */\
    ~((UWord) 0),	/* (amount) mmmbc:  max mseg mbcs                */ \
    1024*1024,		/* (bytes)  lmbcs:  largest mbc size             */\
    128*1024,		/* (bytes)  smbcs:  smallest mbc size            */\
    10,			/* (amount) mbcgs:  mbc growth stages            */\
    0,			/* (%)      acul:  abandon carrier utilization limit */\
    /* --- Data not options -------------------------------------------- */\
    NULL,		/* (ptr)    fix                                  */\
    NULL		/* (ptr)    fix_type_size                        */\
}

#endif

void *	erts_alcu_alloc(ErtsAlcType_t, void *, Uint);
void *	erts_alcu_realloc(ErtsAlcType_t, void *, void *, Uint);
void *	erts_alcu_realloc_mv(ErtsAlcType_t, void *, void *, Uint);
void	erts_alcu_free(ErtsAlcType_t, void *, void *);
#ifdef USE_THREADS
void *	erts_alcu_alloc_ts(ErtsAlcType_t, void *, Uint);
void *	erts_alcu_realloc_ts(ErtsAlcType_t, void *, void *, Uint);
void *	erts_alcu_realloc_mv_ts(ErtsAlcType_t, void *, void *, Uint);
void	erts_alcu_free_ts(ErtsAlcType_t, void *, void *);
#ifdef ERTS_SMP
void *	erts_alcu_alloc_thr_spec(ErtsAlcType_t, void *, Uint);
void *	erts_alcu_realloc_thr_spec(ErtsAlcType_t, void *, void *, Uint);
void *	erts_alcu_realloc_mv_thr_spec(ErtsAlcType_t, void *, void *, Uint);
void	erts_alcu_free_thr_spec(ErtsAlcType_t, void *, void *);
void *	erts_alcu_alloc_thr_pref(ErtsAlcType_t, void *, Uint);
void *	erts_alcu_realloc_thr_pref(ErtsAlcType_t, void *, void *, Uint);
void *	erts_alcu_realloc_mv_thr_pref(ErtsAlcType_t, void *, void *, Uint);
void	erts_alcu_free_thr_pref(ErtsAlcType_t, void *, void *);
#endif
#endif
Eterm	erts_alcu_au_info_options(int *, void *, Uint **, Uint *);
Eterm	erts_alcu_info_options(Allctr_t *, int *, void *, Uint **, Uint *);
Eterm	erts_alcu_sz_info(Allctr_t *, int, int, int *, void *, Uint **, Uint *);
Eterm	erts_alcu_info(Allctr_t *, int, int, int *, void *, Uint **, Uint *);
void	erts_alcu_init(AlcUInit_t *);
void    erts_alcu_current_size(Allctr_t *, AllctrSize_t *,
			       ErtsAlcUFixInfo_t *, int);
#ifdef ERTS_SMP
void    erts_alcu_check_delayed_dealloc(Allctr_t *, int, int *, ErtsThrPrgrVal *, int *);
#endif
erts_aint32_t erts_alcu_fix_alloc_shrink(Allctr_t *, erts_aint32_t);

#ifdef ARCH_32
extern UWord erts_literal_vspace_map[];
# define ERTS_VSPACE_WORD_BITS (sizeof(UWord)*8)
#endif

void* erts_alcu_mseg_alloc(Allctr_t*, Uint *size_p, Uint flags);
void* erts_alcu_mseg_realloc(Allctr_t*, void *seg, Uint old_size, Uint *new_size_p);
void  erts_alcu_mseg_dealloc(Allctr_t*, void *seg, Uint size, Uint flags);

#if HAVE_ERTS_MSEG
# if defined(ARCH_32)
void* erts_alcu_literal_32_mseg_alloc(Allctr_t*, Uint *size_p, Uint flags);
void* erts_alcu_literal_32_mseg_realloc(Allctr_t*, void *seg, Uint old_size, Uint *new_size_p);
void  erts_alcu_literal_32_mseg_dealloc(Allctr_t*, void *seg, Uint size, Uint flags);

# elif defined(ARCH_64) && defined(ERTS_HAVE_OS_PHYSICAL_MEMORY_RESERVATION)
void* erts_alcu_literal_64_mseg_alloc(Allctr_t*, Uint *size_p, Uint flags);
void* erts_alcu_literal_64_mseg_realloc(Allctr_t*, void *seg, Uint old_size, Uint *new_size_p);
void  erts_alcu_literal_64_mseg_dealloc(Allctr_t*, void *seg, Uint size, Uint flags);
# endif
#endif /* HAVE_ERTS_MSEG */

void* erts_alcu_sys_alloc(Allctr_t*, Uint size, int superalign);
void* erts_alcu_sys_realloc(Allctr_t*, void *ptr, Uint size, Uint old_size, int superalign);
void  erts_alcu_sys_dealloc(Allctr_t*, void *ptr, Uint size, int superalign);
#ifdef ARCH_32
void* erts_alcu_literal_32_sys_alloc(Allctr_t*, Uint size, int superalign);
void* erts_alcu_literal_32_sys_realloc(Allctr_t*, void *ptr, Uint size, Uint old_size, int superalign);
void  erts_alcu_literal_32_sys_dealloc(Allctr_t*, void *ptr, Uint size, int superalign);
#endif

#endif /* !ERL_ALLOC_UTIL__ */

#if defined(GET_ERL_ALLOC_UTIL_IMPL) && !defined(ERL_ALLOC_UTIL_IMPL__)
#define ERL_ALLOC_UTIL_IMPL__

#define ERTS_ALCU_FLG_FAIL_REALLOC_MOVE		(((Uint32) 1) << 0)

#undef ERTS_ALLOC_UTIL_HARD_DEBUG
#ifdef DEBUG
#  if 0
#    define ERTS_ALLOC_UTIL_HARD_DEBUG
#  endif
#endif

#undef MIN
#undef MAX
#define MIN(X, Y) ((X) < (Y) ? (X) : (Y))
#define MAX(X, Y) ((X) > (Y) ? (X) : (Y))
#define FLOOR(X, I) (((X)/(I))*(I))
#define CEILING(X, I)  ((((X) - 1)/(I) + 1)*(I))

#undef  WORD_MASK
#define INV_WORD_MASK	((UWord) (sizeof(UWord) - 1))
#define WORD_MASK	(~INV_WORD_MASK)
#define WORD_FLOOR(X)	((X) & WORD_MASK)
#define WORD_CEILING(X)	WORD_FLOOR((X) + INV_WORD_MASK)

#undef  UNIT_MASK
#define INV_UNIT_MASK	((UWord) (sizeof(Unit_t) - 1))
#define UNIT_MASK	(~INV_UNIT_MASK)
#define UNIT_FLOOR(X)	((X) & UNIT_MASK)
#define UNIT_CEILING(X)	UNIT_FLOOR((X) + INV_UNIT_MASK)

#define FLG_MASK		INV_UNIT_MASK
#define SBC_BLK_SZ_MASK         UNIT_MASK
#define MBC_FBLK_SZ_MASK        UNIT_MASK
#define CARRIER_SZ_MASK         UNIT_MASK

#if ERTS_HAVE_MSEG_SUPER_ALIGNED \
    || (!HAVE_ERTS_MSEG && ERTS_HAVE_ERTS_SYS_ALIGNED_ALLOC)
#  ifdef MSEG_ALIGN_BITS
#    define ERTS_SUPER_ALIGN_BITS MSEG_ALIGN_BITS
#  else
#    define ERTS_SUPER_ALIGN_BITS 18
#  endif
#  ifdef ARCH_64 
#    define MBC_ABLK_OFFSET_BITS   24
#  else
#    define MBC_ABLK_OFFSET_BITS   9
     /* Affects hard limits for sbct and lmbcs documented in erts_alloc.xml */
#  endif
#  define ERTS_SACRR_UNIT_SHIFT		ERTS_SUPER_ALIGN_BITS
#  define ERTS_SACRR_UNIT_SZ		(1 << ERTS_SACRR_UNIT_SHIFT)
#  define ERTS_SACRR_UNIT_MASK		((~(UWord)0) << ERTS_SACRR_UNIT_SHIFT)
#  define ERTS_SACRR_UNIT_FLOOR(X)	((X) & ERTS_SACRR_UNIT_MASK)
#  define ERTS_SACRR_UNIT_CEILING(X)	ERTS_SACRR_UNIT_FLOOR((X) + ~ERTS_SACRR_UNIT_MASK)
#  define ERTS_SA_MB_CARRIERS 1
#else
#  define ERTS_SA_MB_CARRIERS 0
#  define MBC_ABLK_OFFSET_BITS   0 /* no carrier offset in block header */
#endif
#if ERTS_HAVE_MSEG_SUPER_ALIGNED && !ERTS_HAVE_ERTS_SYS_ALIGNED_ALLOC
#  define ERTS_SUPER_ALIGNED_MSEG_ONLY 1
#else
#  define ERTS_SUPER_ALIGNED_MSEG_ONLY 0
#endif

#if MBC_ABLK_OFFSET_BITS
#  define MBC_ABLK_OFFSET_SHIFT  (sizeof(UWord)*8 - MBC_ABLK_OFFSET_BITS)
#  define MBC_ABLK_OFFSET_MASK   (~((UWord)0) << MBC_ABLK_OFFSET_SHIFT)
#  define MBC_ABLK_SZ_MASK	(~MBC_ABLK_OFFSET_MASK & ~FLG_MASK)
#else
#  define MBC_ABLK_SZ_MASK	(~FLG_MASK)
#endif

#define MBC_ABLK_SZ(B) (ASSERT(!is_sbc_blk(B)), (B)->bhdr & MBC_ABLK_SZ_MASK)
#define MBC_FBLK_SZ(B) (ASSERT(!is_sbc_blk(B)), (B)->bhdr & MBC_FBLK_SZ_MASK)
#define SBC_BLK_SZ(B) (ASSERT(is_sbc_blk(B)), (B)->bhdr & SBC_BLK_SZ_MASK)

#define CARRIER_SZ(C) \
  ((C)->chdr & CARRIER_SZ_MASK)

typedef union {char c[ERTS_ALLOC_ALIGN_BYTES]; long l; double d;} Unit_t;

#ifdef ERTS_SMP

typedef struct ErtsDoubleLink_t_ {
    struct ErtsDoubleLink_t_ *next;
    struct ErtsDoubleLink_t_ *prev;
}ErtsDoubleLink_t;

typedef struct {
    erts_atomic_t next;
    erts_atomic_t prev;
    Allctr_t *orig_allctr;      /* read-only while carrier is alive */
    ErtsThrPrgrVal thr_prgr;
    erts_atomic_t max_size;
    UWord abandon_limit;
    UWord blocks;
    UWord blocks_size;
    ErtsDoubleLink_t abandoned; /* node in pooled_list or traitor_list */
} ErtsAlcCPoolData_t;

#endif

typedef struct Carrier_t_ Carrier_t;
struct Carrier_t_ {
    UWord chdr;
    Carrier_t *next;
    Carrier_t *prev;
    erts_smp_atomic_t allctr;
#ifdef ERTS_SMP
    ErtsAlcCPoolData_t cpool; /* Overwritten by block if sbc */
#endif
};

#define ERTS_ALC_CARRIER_TO_ALLCTR(C) \
  ((Allctr_t *) (erts_smp_atomic_read_nob(&(C)->allctr) & ~FLG_MASK))

typedef struct {
    Carrier_t *first;
    Carrier_t *last;
} CarrierList_t;

typedef struct {
    UWord bhdr;
#if !MBC_ABLK_OFFSET_BITS
    Carrier_t *carrier;
#else
    union {
	Carrier_t *carrier;     /* if free */
	char       udata__[1];  /* if allocated */
    }u;
#endif
} Block_t;

#define THIS_FREE_BLK_HDR_FLG 	(((UWord) 1) << 0)
#define PREV_FREE_BLK_HDR_FLG 	(((UWord) 1) << 1)
#define LAST_BLK_HDR_FLG 	(((UWord) 1) << 2)

#define SBC_BLK_HDR_FLG /* Special flag combo for (allocated) SBC blocks */\
    (THIS_FREE_BLK_HDR_FLG | PREV_FREE_BLK_HDR_FLG | LAST_BLK_HDR_FLG)

/*
 * FREE_LAST_MBC_BLK_HDR_FLGS is a special flag combo used for
 * distinguishing empty mbc's from allocated blocks in
 * handle_delayed_dealloc().
 */
#define FREE_LAST_MBC_BLK_HDR_FLGS (THIS_FREE_BLK_HDR_FLG | LAST_BLK_HDR_FLG)

#define IS_FREE_LAST_MBC_BLK(B) \
    (((B)->bhdr & FLG_MASK) == FREE_LAST_MBC_BLK_HDR_FLGS)

#define IS_SBC_BLK(B) (((B)->bhdr & FLG_MASK) == SBC_BLK_HDR_FLG)
#define IS_MBC_BLK(B) (!IS_SBC_BLK((B)))
#define IS_FREE_BLK(B) (ASSERT(IS_MBC_BLK(B)), \
			(B)->bhdr & THIS_FREE_BLK_HDR_FLG)

#if MBC_ABLK_OFFSET_BITS
#  define FBLK_TO_MBC(B) (ASSERT(IS_MBC_BLK(B) && IS_FREE_BLK(B)), \
			  (B)->u.carrier)
#  define ABLK_TO_MBC(B) \
    (ASSERT(IS_MBC_BLK(B) && !IS_FREE_BLK(B)), \
     (Carrier_t*)((ERTS_SACRR_UNIT_FLOOR((UWord)(B)) - \
		  (((B)->bhdr >> MBC_ABLK_OFFSET_SHIFT) << ERTS_SACRR_UNIT_SHIFT))))
#  define BLK_TO_MBC(B) (IS_FREE_BLK(B) ? FBLK_TO_MBC(B) : ABLK_TO_MBC(B))
#else
#  define FBLK_TO_MBC(B) ((B)->carrier)
#  define ABLK_TO_MBC(B) ((B)->carrier)
#  define BLK_TO_MBC(B)  ((B)->carrier)
#endif
#define MBC_BLK_SZ(B) (IS_FREE_BLK(B) ? MBC_FBLK_SZ(B) : MBC_ABLK_SZ(B))

typedef UWord FreeBlkFtr_t; /* Footer of a free block */

typedef Uint64 CallCounter_t;

typedef struct {
    UWord		no;
    UWord		size;
} StatValues_t;

typedef struct {
    union {
	struct {
	    StatValues_t	mseg;
	    StatValues_t	sys_alloc;
	} norm;
    } curr;
    StatValues_t	max;
    StatValues_t	max_ever;
    struct {
	StatValues_t	curr;
	StatValues_t	max;
	StatValues_t	max_ever;
    } blocks;
} CarriersStats_t;

#ifdef USE_LTTNG_VM_TRACEPOINTS
#define LTTNG_CARRIER_STATS_TO_LTTNG_STATS(CSP, LSP)            \
    do {                                                        \
        (LSP)->carriers.size = (CSP)->curr.norm.mseg.size       \
                             + (CSP)->curr.norm.sys_alloc.size; \
        (LSP)->carriers.no   = (CSP)->curr.norm.mseg.no         \
                             + (CSP)->curr.norm.sys_alloc.no;   \
        (LSP)->blocks.size   = (CSP)->blocks.curr.size;         \
        (LSP)->blocks.no     = (CSP)->blocks.curr.no;           \
    } while (0)
#endif

#ifdef ERTS_SMP

typedef union ErtsAllctrDDBlock_t_ ErtsAllctrDDBlock_t;

union ErtsAllctrDDBlock_t_ {
    erts_atomic_t atmc_next;
    ErtsAllctrDDBlock_t *ptr_next;
};

typedef struct {
    ErtsAllctrDDBlock_t marker;
    erts_atomic_t last;
    erts_atomic_t um_refc[2];
    erts_atomic32_t um_refc_ix;
} ErtsDDTail_t;

typedef struct {
    /*
     * This structure needs to be cache line aligned for best
     * performance.
     */
    union {
	/* Modified by threads returning memory to this allocator */
	ErtsDDTail_t data;
	char align__[ERTS_ALC_CACHE_LINE_ALIGN_SIZE(sizeof(ErtsDDTail_t))];
    } tail;
    /*
     * Everything below this point is *only* accessed by the
     * thread owning the allocator.
     */
    struct {
	ErtsAllctrDDBlock_t *first;
	ErtsAllctrDDBlock_t *unref_end;
	struct {
	    ErtsThrPrgrVal thr_progress;
	    int thr_progress_reached;
	    int um_refc_ix;
	    ErtsAllctrDDBlock_t *unref_end;
	} next;
	int used_marker;
    } head;
} ErtsAllctrDDQueue_t;

#endif

typedef struct {
    size_t type_size;
    SWord list_size;
    void *list;
    union {
	struct {
	    SWord max_used;
	    SWord limit;
	    SWord allocated;
	    SWord used;
	} nocpool;
	struct {
	    int min_list_size;
	    int shrink_list;
	    UWord allocated;
	    UWord used;
	} cpool;
    } u;
} ErtsAlcFixList_t;

struct Allctr_t_ {
#ifdef ERTS_SMP
    struct {
	/*
	 * We want the queue at the beginning of
	 * the Allctr_t struct, due to cache line
	 * alignment reasons.
	 */
	ErtsAllctrDDQueue_t q;
	int		use;
	int		ix;
    } dd;
#endif

    /* Allocator name prefix */
    char *		name_prefix;

    /* Allocator number */
    ErtsAlcType_t	alloc_no;

    /* Instance index */
    int			ix;

    /* Alloc, realloc and free names as atoms */
    struct {
	Eterm		alloc;
	Eterm		realloc;
	Eterm		free;
    } name;

    /* Version string */
    char *		vsn_str;

    /* Options */
    int			t;
    int			ramv;
    Uint		sbc_threshold;
    Uint		sbc_move_threshold;
    Uint		mbc_move_threshold;
    Uint		main_carrier_size;
    Uint		max_mseg_sbcs;
    Uint		max_mseg_mbcs;
    Uint		largest_mbc_size;
    Uint		smallest_mbc_size;
    Uint		mbc_growth_stages;

#if HAVE_ERTS_MSEG
    ErtsMsegOpt_t	mseg_opt;
#endif

    /* */
    Uint		mbc_header_size;
    Uint		min_mbc_size;
    Uint		min_mbc_first_free_size;
    Uint		min_block_size;
    UWord               crr_set_flgs;
    UWord               crr_clr_flgs;

    /* Carriers */
    CarrierList_t	mbc_list;
    CarrierList_t	sbc_list;
#ifdef ERTS_SMP
    struct {
	/* pooled_list, traitor list and dc_list contain only
           carriers _created_ by this allocator */
	ErtsDoubleLink_t pooled_list;
	ErtsDoubleLink_t traitor_list;
	CarrierList_t	 dc_list;

	UWord		abandon_limit;
	int		disable_abandon;
	int		check_limit_count;
	int		util_limit;
	struct {
	    erts_atomic_t	blocks_size;
	    erts_atomic_t	no_blocks;
	    erts_atomic_t	carriers_size;
	    erts_atomic_t	no_carriers;
	} stat;
    } cpool;
#endif

    /* Main carrier (if there is one) */
    Carrier_t *		main_carrier;

    /* Callback functions (first 4 are mandatory) */
    Block_t *		(*get_free_block)	(Allctr_t *, Uint,
						 Block_t *, Uint);
    void		(*link_free_block)	(Allctr_t *, Block_t *);
    void		(*unlink_free_block)	(Allctr_t *, Block_t *);
    Eterm		(*info_options)		(Allctr_t *, char *, int *,
						 void *, Uint **, Uint *);

    Uint		(*get_next_mbc_size)	(Allctr_t *);
    void		(*creating_mbc)		(Allctr_t *, Carrier_t *);
    void		(*destroying_mbc)	(Allctr_t *, Carrier_t *);

    /* The three callbacks below are needed to support carrier migration */
    void		(*add_mbc)		(Allctr_t *, Carrier_t *);
    void		(*remove_mbc)	        (Allctr_t *, Carrier_t *);
    UWord		(*largest_fblk_in_mbc)  (Allctr_t *, Carrier_t *);

#if HAVE_ERTS_MSEG
    void*               (*mseg_alloc)(Allctr_t*, Uint *size_p, Uint flags);
    void*               (*mseg_realloc)(Allctr_t*, void *seg, Uint old_size, Uint *new_size_p);
    void                (*mseg_dealloc)(Allctr_t*, void *seg, Uint size, Uint flags);
#endif
    void*               (*sys_alloc)(Allctr_t *allctr, Uint size, int superalign);
    void*               (*sys_realloc)(Allctr_t *allctr, void *ptr, Uint size, Uint old_size, int superalign);
    void                (*sys_dealloc)(Allctr_t *allctr, void *ptr, Uint size, int superalign);

    void		(*init_atoms)		(void);

#ifdef ERTS_ALLOC_UTIL_HARD_DEBUG
    void		(*check_block)		(Allctr_t *, Block_t *,  int);
    void		(*check_mbc)		(Allctr_t *, Carrier_t *);
#endif

    int			fix_n_base;
    int			fix_shrink_scheduled;
    ErtsAlcFixList_t	*fix;

#ifdef USE_THREADS
    /* Mutex for this allocator */
    erts_mtx_t		mutex;
    int			thread_safe;
    struct {
	Allctr_t	*prev;
	Allctr_t	*next;
    } ts_list;

#endif

    int			atoms_initialized;

    int			stopped;

    /* Some statistics ... */
    struct {
	CallCounter_t	this_alloc;
	CallCounter_t	this_free;
	CallCounter_t	this_realloc;
	CallCounter_t	mseg_alloc;
	CallCounter_t	mseg_dealloc;
	CallCounter_t	mseg_realloc;
	CallCounter_t	sys_alloc;
	CallCounter_t	sys_free;
	CallCounter_t	sys_realloc;
    } calls;

    CarriersStats_t	sbcs;
    CarriersStats_t	mbcs;
    
#ifdef DEBUG
#ifdef USE_THREADS
    struct {
	int saved_tid;
	erts_tid_t tid;
    } debug;
#endif
#endif
};

int	erts_alcu_start(Allctr_t *, AllctrInit_t *);
void	erts_alcu_stop(Allctr_t *);

void	erts_alcu_verify_unused(Allctr_t *);
void	erts_alcu_verify_unused_ts(Allctr_t *allctr);

UWord	erts_alcu_test(UWord, UWord, UWord);

void erts_alcu_assert_failed(char* expr, char* file, int line, char *func);

#ifdef DEBUG
int is_sbc_blk(Block_t*);
#endif


#endif /* #if defined(GET_ERL_ALLOC_UTIL_IMPL)
	      && !defined(ERL_ALLOC_UTIL_IMPL__) */

