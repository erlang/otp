/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2002-2012. All Rights Reserved.
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
 * Description:	A memory allocator utility. This utility provides
 *              management of (multiple) memory segments, coalescing
 *              of free blocks, etc. Allocators are implemented by
 *              implementing a callback-interface which is called by
 *              this utility. The only task the callback-module has to
 *              perform is to supervise the free blocks.
 *
 * Author: 	Rickard Green
 */

/*
 * Alloc util will enforce 8 byte alignment if sys_alloc and mseg_alloc at
 * least enforces 8 byte alignment. If sys_alloc only enforces 4 byte
 * alignment then alloc util will do so too. 
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "global.h"
#include "big.h"
#include "erl_mtrace.h"
#define GET_ERL_ALLOC_UTIL_IMPL
#include "erl_alloc_util.h"
#include "erl_mseg.h"
#include "erl_threads.h"
#include "erl_thr_progress.h"

#ifdef ERTS_ENABLE_LOCK_COUNT
#include "erl_lock_count.h"
#endif

#if defined(ERTS_ALLOC_UTIL_HARD_DEBUG) && defined(__GNUC__)
#warning "* * * * * * * * * *"
#warning "* * * * * * * * * *"
#warning "* * NOTE:       * *"
#warning "* * Hard debug  * *"
#warning "* * is enabled! * *"
#warning "* * * * * * * * * *"
#warning "* * * * * * * * * *"
#endif

#define ERTS_ALCU_DD_OPS_LIM_HIGH 20
#define ERTS_ALCU_DD_OPS_LIM_LOW 2

/* Fix alloc limit */
#define ERTS_ALCU_FIX_MAX_LIST_SZ 1000
#define ERTS_ALC_FIX_MAX_SHRINK_OPS 30

#define ALLOC_ZERO_EQ_NULL 0

static int atoms_initialized = 0;
static int initialized = 0;

int erts_have_sbmbc_alloc;

#if HAVE_ERTS_MSEG

#define INV_MSEG_UNIT_MASK	((UWord) (mseg_unit_size - 1))
#define MSEG_UNIT_MASK		(~INV_MSEG_UNIT_MASK)
#define MSEG_UNIT_FLOOR(X)	((X) & MSEG_UNIT_MASK)
#define MSEG_UNIT_CEILING(X)	MSEG_UNIT_FLOOR((X) + INV_MSEG_UNIT_MASK)

#endif

#define INV_SYS_ALLOC_CARRIER_MASK	((UWord) (sys_alloc_carrier_size - 1))
#define SYS_ALLOC_CARRIER_MASK		(~INV_SYS_ALLOC_CARRIER_MASK)
#define SYS_ALLOC_CARRIER_FLOOR(X)	((X) & SYS_ALLOC_CARRIER_MASK)
#define SYS_ALLOC_CARRIER_CEILING(X) \
  SYS_ALLOC_CARRIER_FLOOR((X) + INV_SYS_ALLOC_CARRIER_MASK)

#undef ASSERT
#define ASSERT ASSERT_EXPR

#if 0
/* Can be useful for debugging */
#define MBC_REALLOC_ALWAYS_MOVES
#endif


/* alloc_util global parameters */
static Uint sys_alloc_carrier_size;
#if HAVE_ERTS_MSEG
static Uint max_mseg_carriers;
static Uint mseg_unit_size;
#endif

#define ONE_GIGA (1000000000)

#define INC_CC(CC) ((CC).no == ONE_GIGA - 1				\
		    ? ((CC).giga_no++, (CC).no = 0)			\
		    : (CC).no++)

#define DEC_CC(CC) ((CC).no == 0					\
		    ? ((CC).giga_no--, (CC).no = ONE_GIGA - 1)		\
		    : (CC).no--)

/* ... */

/* Blocks ... */

#define SBC_BLK_FTR_FLG		(((UWord) 1) << 0)
#define UNUSED1_BLK_FTR_FLG	(((UWord) 1) << 1)
#define UNUSED2_BLK_FTR_FLG	(((UWord) 1) << 2)

#define ABLK_HDR_SZ (sizeof(Block_t))
#define FBLK_FTR_SZ (sizeof(UWord))

#define UMEMSZ2BLKSZ(AP, SZ)						\
  (ABLK_HDR_SZ + (SZ) <= (AP)->min_block_size				\
   ? (AP)->min_block_size						\
   : UNIT_CEILING(ABLK_HDR_SZ + (SZ)))

#define UMEM2BLK(P) ((Block_t *) (((char *) (P)) - ABLK_HDR_SZ))
#define BLK2UMEM(P) ((void *)    (((char *) (P)) + ABLK_HDR_SZ))

#define PREV_BLK_SZ(B) \
  ((UWord) (*(((UWord *) (B)) - 1) & SZ_MASK))

#define SET_BLK_SZ_FTR(B, SZ) \
  (*((UWord *) (((char *) (B)) + (SZ) - sizeof(UWord))) = (SZ))

#define THIS_FREE_BLK_HDR_FLG 	(((UWord) 1) << 0)
#define PREV_FREE_BLK_HDR_FLG 	(((UWord) 1) << 1)
#define LAST_BLK_HDR_FLG 	(((UWord) 1) << 2)

#define SET_BLK_SZ(B, SZ) \
  (ASSERT(((SZ) & FLG_MASK) == 0), \
   (*((Block_t *) (B)) = ((*((Block_t *) (B)) & FLG_MASK) | (SZ))))
#define SET_BLK_FREE(B) \
  (*((Block_t *) (B)) |= THIS_FREE_BLK_HDR_FLG)
#define SET_BLK_ALLOCED(B) \
  (*((Block_t *) (B)) &= ~THIS_FREE_BLK_HDR_FLG)
#define SET_PREV_BLK_FREE(B) \
  (*((Block_t *) (B)) |= PREV_FREE_BLK_HDR_FLG)
#define SET_PREV_BLK_ALLOCED(B) \
  (*((Block_t *) (B)) &= ~PREV_FREE_BLK_HDR_FLG)
#define SET_LAST_BLK(B) \
  (*((Block_t *) (B)) |= LAST_BLK_HDR_FLG)
#define SET_NOT_LAST_BLK(B) \
  (*((Block_t *) (B)) &= ~LAST_BLK_HDR_FLG)

#define SBH_THIS_FREE		THIS_FREE_BLK_HDR_FLG
#define SBH_THIS_ALLOCED	((UWord) 0)
#define SBH_PREV_FREE		PREV_FREE_BLK_HDR_FLG
#define SBH_PREV_ALLOCED	((UWord) 0)
#define SBH_LAST_BLK		LAST_BLK_HDR_FLG
#define SBH_NOT_LAST_BLK	((UWord) 0)

#define SET_BLK_HDR(B, Sz, F) \
  (ASSERT(((Sz) & FLG_MASK) == 0), *((Block_t *) (B)) = ((Sz) | (F)))

#define BLK_UMEM_SZ(B) \
  (BLK_SZ(B) - (ABLK_HDR_SZ))
#define IS_PREV_BLK_FREE(B) \
  (*((Block_t *) (B)) & PREV_FREE_BLK_HDR_FLG)
#define IS_PREV_BLK_ALLOCED(B) \
  (!IS_PREV_BLK_FREE((B)))
#define IS_FREE_BLK(B) \
  (*((Block_t *) (B)) & THIS_FREE_BLK_HDR_FLG)
#define IS_ALLOCED_BLK(B) \
  (!IS_FREE_BLK((B)))  
#define IS_LAST_BLK(B) \
  (*((Block_t *) (B)) & LAST_BLK_HDR_FLG)
#define IS_NOT_LAST_BLK(B) \
  (!IS_LAST_BLK((B)))

#define GET_LAST_BLK_HDR_FLG(B) \
  (*((Block_t*) (B)) & LAST_BLK_HDR_FLG)
#define GET_THIS_FREE_BLK_HDR_FLG(B) \
  (*((Block_t*) (B)) & THIS_FREE_BLK_HDR_FLG)
#define GET_PREV_FREE_BLK_HDR_FLG(B) \
  (*((Block_t*) (B)) & PREV_FREE_BLK_HDR_FLG)
#define GET_BLK_HDR_FLGS(B) \
  (*((Block_t*) (B)) & FLG_MASK)

#define IS_FIRST_BLK(B) \
  (IS_PREV_BLK_FREE((B)) && (PREV_BLK_SZ((B)) == 0))
#define IS_NOT_FIRST_BLK(B) \
  (!IS_FIRST_BLK((B)))

#define SET_SBC_BLK_FTR(FTR) \
  ((FTR) = (0 | SBC_BLK_FTR_FLG))
#define SET_MBC_BLK_FTR(FTR) \
  ((FTR) = 0)

#define IS_SBC_BLK(B) \
  (IS_PREV_BLK_FREE((B)) && (((UWord *) (B))[-1] & SBC_BLK_FTR_FLG))
#define IS_MBC_BLK(B) \
  (!IS_SBC_BLK((B)))

#define NXT_BLK(B) \
  ((Block_t *) (((char *) (B)) + BLK_SZ((B))))
#define PREV_BLK(B) \
  ((Block_t *) (((char *) (B)) - PREV_BLK_SZ((B))))

/* Carriers ... */

#define MSEG_CARRIER_HDR_FLAG		(((UWord) 1) << 0)
#define SBC_CARRIER_HDR_FLAG		(((UWord) 1) << 1)

#define SCH_SYS_ALLOC			0
#define SCH_MSEG			MSEG_CARRIER_HDR_FLAG
#define SCH_MBC				0
#define SCH_SBC				SBC_CARRIER_HDR_FLAG

#define SET_CARRIER_HDR(C, Sz, F) \
  (ASSERT(((Sz) & FLG_MASK) == 0), (C)->chdr = ((Sz) | (F)))

#define BLK2SBC(AP, B) \
  ((Carrier_t *) (((char *) (B)) - (AP)->sbc_header_size))
#define FBLK2MBC(AP, B) \
  ((Carrier_t *) (((char *) (B)) - (AP)->mbc_header_size))

#define MBC2FBLK(AP, P) \
  ((Block_t *) (((char *) (P)) + (AP)->mbc_header_size))
#define SBC2BLK(AP, P) \
  ((Block_t *) (((char *) (P)) + (AP)->sbc_header_size))
#define SBC2UMEM(AP, P) \
  ((void *) (((char *) (P)) + ((AP)->sbc_header_size + ABLK_HDR_SZ)))

#define IS_MSEG_CARRIER(C) \
  ((C)->chdr & MSEG_CARRIER_HDR_FLAG)
#define IS_SYS_ALLOC_CARRIER(C) \
  (!IS_MSEG_CARRIER((C)))
#define IS_SB_CARRIER(C) \
  ((C)->chdr & SBC_CARRIER_HDR_FLAG)
#define IS_MB_CARRIER(C) \
  (!IS_SB_CARRIER((C)))

#define SET_MSEG_CARRIER(C) \
  ((C)->chdr |= MSEG_CARRIER_HDR_FLAG)
#define SET_SYS_ALLOC_CARRIER(C) \
  ((C)->chdr &= ~MSEG_CARRIER_HDR_FLAG)
#define SET_SB_CARRIER(C) \
  ((C)->chdr |= SBC_CARRIER_HDR_FLAG)
#define SET_MB_CARRIER(C) \
  ((C)->chdr &= ~SBC_CARRIER_HDR_FLAG)

#define SET_CARRIER_SZ(C, SZ) \
  (ASSERT(((SZ) & FLG_MASK) == 0), \
   ((C)->chdr = ((C)->chdr & FLG_MASK) | (SZ)))

#define CFLG_SBC				(1 << 0)
#define CFLG_MBC				(1 << 1)
#define CFLG_FORCE_MSEG				(1 << 2)
#define CFLG_FORCE_SYS_ALLOC			(1 << 3)
#define CFLG_FORCE_SIZE				(1 << 4)
#define CFLG_MAIN_CARRIER			(1 << 5)

#ifdef ERTS_ALLOC_UTIL_HARD_DEBUG
static void check_blk_carrier(Allctr_t *, Block_t *);
#define HARD_CHECK_BLK_CARRIER(A, B)	check_blk_carrier((A), (B))
#else
#define HARD_CHECK_BLK_CARRIER(A, B)
#endif

/* Statistics updating ... */

#ifdef DEBUG
#define DEBUG_CHECK_CARRIER_NO_SZ(AP)					\
    ASSERT(((AP)->sbcs.curr.norm.mseg.no				\
	    && (AP)->sbcs.curr.norm.mseg.size)				\
	   || (!(AP)->sbcs.curr.norm.mseg.no				\
	       && !(AP)->sbcs.curr.norm.mseg.size));			\
    ASSERT(((AP)->sbcs.curr.norm.sys_alloc.no				\
	    && (AP)->sbcs.curr.norm.sys_alloc.size)			\
	   || (!(AP)->sbcs.curr.norm.sys_alloc.no			\
	       && !(AP)->sbcs.curr.norm.sys_alloc.size));		\
    ASSERT(((AP)->mbcs.curr.norm.mseg.no				\
	    && (AP)->mbcs.curr.norm.mseg.size)				\
	   || (!(AP)->mbcs.curr.norm.mseg.no				\
	       && !(AP)->mbcs.curr.norm.mseg.size));			\
    ASSERT(((AP)->mbcs.curr.norm.sys_alloc.no				\
	    && (AP)->mbcs.curr.norm.sys_alloc.size)			\
	   || (!(AP)->mbcs.curr.norm.sys_alloc.no			\
	       && !(AP)->mbcs.curr.norm.sys_alloc.size));		\
    ASSERT(((AP)->sbmbcs.curr.small_block.no				\
	    && (AP)->sbmbcs.curr.small_block.size)			\
	   || (!(AP)->sbmbcs.curr.small_block.no			\
	       && !(AP)->sbmbcs.curr.small_block.size))

#else
#define DEBUG_CHECK_CARRIER_NO_SZ(AP)
#endif

#define STAT_SBC_ALLOC(AP, BSZ)						\
    (AP)->sbcs.blocks.curr.size += (BSZ);				\
    if ((AP)->sbcs.blocks.max.size < (AP)->sbcs.blocks.curr.size)	\
	(AP)->sbcs.blocks.max.size = (AP)->sbcs.blocks.curr.size;	\
    if ((AP)->sbcs.max.no < ((AP)->sbcs.curr.norm.mseg.no		\
			     + (AP)->sbcs.curr.norm.sys_alloc.no))	\
	(AP)->sbcs.max.no = ((AP)->sbcs.curr.norm.mseg.no		\
			     + (AP)->sbcs.curr.norm.sys_alloc.no);	\
    if ((AP)->sbcs.max.size < ((AP)->sbcs.curr.norm.mseg.size		\
			       + (AP)->sbcs.curr.norm.sys_alloc.size))	\
	(AP)->sbcs.max.size = ((AP)->sbcs.curr.norm.mseg.size		\
			       + (AP)->sbcs.curr.norm.sys_alloc.size)

#define STAT_MSEG_SBC_ALLOC(AP, CSZ, BSZ)				\
do {									\
    (AP)->sbcs.curr.norm.mseg.no++;					\
    (AP)->sbcs.curr.norm.mseg.size += (CSZ);				\
    STAT_SBC_ALLOC((AP), (BSZ));					\
    DEBUG_CHECK_CARRIER_NO_SZ((AP));					\
} while (0)

#define STAT_SYS_ALLOC_SBC_ALLOC(AP, CSZ, BSZ)				\
do {									\
    (AP)->sbcs.curr.norm.sys_alloc.no++;				\
    (AP)->sbcs.curr.norm.sys_alloc.size += (CSZ);			\
    STAT_SBC_ALLOC((AP), (BSZ));					\
    DEBUG_CHECK_CARRIER_NO_SZ((AP));					\
} while (0)


#define STAT_SBC_FREE(AP, BSZ)						\
    ASSERT((AP)->sbcs.blocks.curr.size >= (BSZ));			\
    (AP)->sbcs.blocks.curr.size -= (BSZ)

#define STAT_MSEG_SBC_FREE(AP, CSZ, BSZ)				\
do {									\
    ASSERT((AP)->sbcs.curr.norm.mseg.no > 0);				\
    (AP)->sbcs.curr.norm.mseg.no--;					\
    ASSERT((AP)->sbcs.curr.norm.mseg.size >= (CSZ));			\
    (AP)->sbcs.curr.norm.mseg.size -= (CSZ);				\
    STAT_SBC_FREE((AP), (BSZ));						\
    DEBUG_CHECK_CARRIER_NO_SZ((AP));					\
} while (0)

#define STAT_SYS_ALLOC_SBC_FREE(AP, CSZ, BSZ)				\
do {									\
    ASSERT((AP)->sbcs.curr.norm.sys_alloc.no > 0);			\
    (AP)->sbcs.curr.norm.sys_alloc.no--;				\
    ASSERT((AP)->sbcs.curr.norm.sys_alloc.size >= (CSZ));		\
    (AP)->sbcs.curr.norm.sys_alloc.size -= (CSZ);			\
    STAT_SBC_FREE((AP), (BSZ));						\
    DEBUG_CHECK_CARRIER_NO_SZ((AP));					\
} while (0)

#define STAT_MBC_ALLOC(AP)						\
    if ((AP)->mbcs.max.no < ((AP)->mbcs.curr.norm.mseg.no		\
			     + (AP)->mbcs.curr.norm.sys_alloc.no))	\
	(AP)->mbcs.max.no = ((AP)->mbcs.curr.norm.mseg.no		\
			     + (AP)->mbcs.curr.norm.sys_alloc.no);	\
    if ((AP)->mbcs.max.size < ((AP)->mbcs.curr.norm.mseg.size		\
			       + (AP)->mbcs.curr.norm.sys_alloc.size))	\
	(AP)->mbcs.max.size = ((AP)->mbcs.curr.norm.mseg.size		\
			       + (AP)->mbcs.curr.norm.sys_alloc.size)


#define STAT_SBMBC_ALLOC(AP, CSZ)					\
do {									\
    (AP)->sbmbcs.curr.small_block.no++;					\
    (AP)->sbmbcs.curr.small_block.size += (CSZ);			\
    if ((AP)->sbmbcs.max.no < (AP)->sbmbcs.curr.small_block.no)		\
	(AP)->sbmbcs.max.no = (AP)->sbmbcs.curr.small_block.no;		\
    if ((AP)->sbmbcs.max.size < (AP)->sbmbcs.curr.small_block.size)	\
	(AP)->sbmbcs.max.size = (AP)->sbmbcs.curr.small_block.size;	\
    DEBUG_CHECK_CARRIER_NO_SZ((AP));					\
} while (0)

#define STAT_MSEG_MBC_ALLOC(AP, CSZ)					\
do {									\
    (AP)->mbcs.curr.norm.mseg.no++;					\
    (AP)->mbcs.curr.norm.mseg.size += (CSZ);				\
    STAT_MBC_ALLOC((AP));						\
    DEBUG_CHECK_CARRIER_NO_SZ((AP));					\
} while (0)

#define STAT_SYS_ALLOC_MBC_ALLOC(AP, CSZ)				\
do {									\
    (AP)->mbcs.curr.norm.sys_alloc.no++;				\
    (AP)->mbcs.curr.norm.sys_alloc.size += (CSZ);			\
    STAT_MBC_ALLOC((AP));						\
    DEBUG_CHECK_CARRIER_NO_SZ((AP));					\
} while (0)

#define STAT_SBMBC_FREE(AP, CSZ)					\
do {									\
    ASSERT((AP)->sbmbcs.curr.small_block.no > 0);			\
    (AP)->sbmbcs.curr.small_block.no--;					\
    ASSERT((AP)->sbmbcs.curr.small_block.size >= (CSZ));		\
    (AP)->sbmbcs.curr.small_block.size -= (CSZ);			\
    DEBUG_CHECK_CARRIER_NO_SZ((AP));					\
} while (0)

#define STAT_MSEG_MBC_FREE(AP, CSZ)					\
do {									\
    ASSERT((AP)->mbcs.curr.norm.mseg.no > 0);				\
    (AP)->mbcs.curr.norm.mseg.no--;					\
    ASSERT((AP)->mbcs.curr.norm.mseg.size >= (CSZ));			\
    (AP)->mbcs.curr.norm.mseg.size -= (CSZ);				\
    DEBUG_CHECK_CARRIER_NO_SZ((AP));					\
} while (0)

#define STAT_SYS_ALLOC_MBC_FREE(AP, CSZ)				\
do {									\
    ASSERT((AP)->mbcs.curr.norm.sys_alloc.no > 0);			\
    (AP)->mbcs.curr.norm.sys_alloc.no--;				\
    ASSERT((AP)->mbcs.curr.norm.sys_alloc.size >= (CSZ));		\
    (AP)->mbcs.curr.norm.sys_alloc.size -= (CSZ);			\
    DEBUG_CHECK_CARRIER_NO_SZ((AP));					\
} while (0)

#define STAT_MBC_BLK_ALLOC(AP, BSZ, FLGS)	       			\
do {									\
    CarriersStats_t *cstats__ = (((FLGS) & ERTS_ALCU_FLG_SBMBC)		\
				 ? &(AP)->sbmbcs			\
				 : &(AP)->mbcs);			\
    cstats__->blocks.curr.no++;						\
    if (cstats__->blocks.max.no < cstats__->blocks.curr.no)		\
	cstats__->blocks.max.no = cstats__->blocks.curr.no;		\
    cstats__->blocks.curr.size += (BSZ);				\
    if (cstats__->blocks.max.size < cstats__->blocks.curr.size)		\
	cstats__->blocks.max.size = cstats__->blocks.curr.size;		\
} while (0)

#define STAT_MBC_BLK_FREE(AP, BSZ, FLGS)				\
do {									\
    CarriersStats_t *cstats__ = (((FLGS) & ERTS_ALCU_FLG_SBMBC)		\
				 ? &(AP)->sbmbcs			\
				 : &(AP)->mbcs);			\
    ASSERT(cstats__->blocks.curr.no > 0);				\
    cstats__->blocks.curr.no--;						\
    ASSERT(cstats__->blocks.curr.size >= (BSZ));			\
    cstats__->blocks.curr.size -= (BSZ);				\
} while (0)

/* Debug stuff... */
#ifdef DEBUG
static UWord carrier_alignment;
#define DEBUG_SAVE_ALIGNMENT(C)						\
do {									\
    UWord algnmnt__ = sizeof(Unit_t) - (((UWord) (C)) % sizeof(Unit_t));\
    carrier_alignment = MIN(carrier_alignment, algnmnt__);		\
    ASSERT(((UWord) (C)) % sizeof(UWord) == 0);				\
} while (0)
#define DEBUG_CHECK_ALIGNMENT(P)					\
do {									\
    ASSERT(sizeof(Unit_t) - (((UWord) (P)) % sizeof(Unit_t))		\
	   >= carrier_alignment);					\
    ASSERT(((UWord) (P)) % sizeof(UWord) == 0);				\
} while (0)

#else
#define DEBUG_SAVE_ALIGNMENT(C)
#define DEBUG_CHECK_ALIGNMENT(P)
#endif

#ifdef DEBUG
#ifdef USE_THREADS
#define ERTS_ALCU_DBG_CHK_THR_ACCESS(A)					\
do {									\
    if (!(A)->thread_safe) {						\
	if (!(A)->debug.saved_tid) {					\
	    (A)->debug.tid = erts_thr_self();				\
	    (A)->debug.saved_tid = 1;					\
	}								\
	else {								\
	    ERTS_SMP_LC_ASSERT(						\
		ethr_equal_tids((A)->debug.tid, erts_thr_self())	\
		|| erts_thr_progress_is_blocking());			\
	}								\
    }									\
} while (0)
#else
#define ERTS_ALCU_DBG_CHK_THR_ACCESS(A)
#endif
#else
#define ERTS_ALCU_DBG_CHK_THR_ACCESS(A)
#endif


static void make_name_atoms(Allctr_t *allctr);

static Block_t *create_carrier(Allctr_t *, Uint, UWord);
static void destroy_carrier(Allctr_t *, Block_t *);
static void mbc_free(Allctr_t *allctr, void *p);


/* mseg ... */

#if HAVE_ERTS_MSEG

static ERTS_INLINE void *
alcu_mseg_alloc(Allctr_t *allctr, Uint *size_p)
{
    void *res;

    res = erts_mseg_alloc_opt(allctr->alloc_no, size_p, &allctr->mseg_opt);
    INC_CC(allctr->calls.mseg_alloc);
    return res;
}

static ERTS_INLINE void *
alcu_mseg_realloc(Allctr_t *allctr, void *seg, Uint old_size, Uint *new_size_p)
{
    void *res;

    res = erts_mseg_realloc_opt(allctr->alloc_no, seg, old_size, new_size_p,
				&allctr->mseg_opt);
    INC_CC(allctr->calls.mseg_realloc);
    return res;
}

static ERTS_INLINE void
alcu_mseg_dealloc(Allctr_t *allctr, void *seg, Uint size)
{
    erts_mseg_dealloc_opt(allctr->alloc_no, seg, size, &allctr->mseg_opt);
    INC_CC(allctr->calls.mseg_dealloc);
}

#endif

static ERTS_INLINE void *
alcu_sys_alloc(Allctr_t *allctr, Uint size)
{
    void *res;

    res = erts_sys_alloc(0, NULL, size);
    INC_CC(allctr->calls.sys_alloc);
    if (erts_mtrace_enabled)
	erts_mtrace_crr_alloc(res, allctr->alloc_no, ERTS_ALC_A_SYSTEM, size);
    return res;
}

static ERTS_INLINE void *
alcu_sys_realloc(Allctr_t *allctr, void *ptr, Uint size)
{
    void *res;

    res = erts_sys_realloc(0, NULL, ptr, size);
    INC_CC(allctr->calls.sys_realloc);
    if (erts_mtrace_enabled)
	erts_mtrace_crr_realloc(res,
				allctr->alloc_no,
				ERTS_ALC_A_SYSTEM,
				ptr,
				size);
    return res;
}

static ERTS_INLINE void
alcu_sys_free(Allctr_t *allctr, void *ptr)
{
    erts_sys_free(0, NULL, ptr);
    INC_CC(allctr->calls.sys_free);
    if (erts_mtrace_enabled)
	erts_mtrace_crr_free(allctr->alloc_no, ERTS_ALC_A_SYSTEM, ptr);
}

static Uint
get_next_mbc_size(Allctr_t *allctr)
{
    Uint size;
    int cs = (allctr->mbcs.curr.norm.mseg.no
	      + allctr->mbcs.curr.norm.sys_alloc.no
	      - (allctr->main_carrier ? 1 : 0));

    ASSERT(cs >= 0);
    ASSERT(allctr->largest_mbc_size >= allctr->smallest_mbc_size);

    if (cs >= allctr->mbc_growth_stages)
	size = allctr->largest_mbc_size;
    else
	size = ((cs*(allctr->largest_mbc_size - allctr->smallest_mbc_size)
		 / allctr->mbc_growth_stages)
		+ allctr->smallest_mbc_size);

    if (size < allctr->min_mbc_size)
	size = allctr->min_mbc_size;

    return size;
}

static ERTS_INLINE void
link_carrier(CarrierList_t *cl, Carrier_t *crr)
{
    crr->next = NULL;
    if (!cl->last) {
	ASSERT(!cl->first);
	cl->first = cl->last = crr;
	crr->prev = NULL;
    }
    else {
	ASSERT(cl->first);
	ASSERT(!cl->first->prev);
	ASSERT(cl->last);
	ASSERT(!cl->last->next);
	crr->prev = cl->last;
	cl->last->next = crr;
	cl->last = crr;
    }
    ASSERT(crr->next != crr);
    ASSERT(crr->prev != crr);
}

static ERTS_INLINE void
relink_carrier(CarrierList_t *cl, Carrier_t *crr)
{
    if (crr->next) {
	if (crr->next->prev != crr)
	    crr->next->prev = crr;
    }
    else if (cl->last != crr)
	cl->last = crr;

    if (crr->prev) {
	if (crr->prev->next != crr)
	    crr->prev->next = crr;
    }
    else if (cl->first != crr)
	cl->first = crr;
}

static ERTS_INLINE void
unlink_carrier(CarrierList_t *cl, Carrier_t *crr)
{
    ASSERT(crr->next != crr);
    ASSERT(crr->prev != crr);

    if (cl->first == crr) {
	ASSERT(!crr->prev);
	cl->first = crr->next;
    }
    else {
	ASSERT(crr->prev);
	crr->prev->next = crr->next;
    }

    if (cl->last == crr) {
	ASSERT(!crr->next);
	cl->last = crr->prev;
    }
    else {
	ASSERT(crr->next);
	crr->next->prev = crr->prev;
    }
}

static Block_t *create_sbmbc(Allctr_t *allctr, Uint umem_sz);
static void destroy_sbmbc(Allctr_t *allctr, Block_t *blk);
static Block_t *create_carrier(Allctr_t *, Uint, UWord);
static void destroy_carrier(Allctr_t *, Block_t *);

#if 0
#define ERTS_DBG_CHK_FIX_LIST(A, FIX, IX, B)			\
    do { if ((FIX)) chk_fix_list((A), (FIX), (IX), (B)); } while (0)
static void
chk_fix_list(Allctr_t *allctr, ErtsAlcFixList_t *fix, int ix, int before)
{
    void *p;
    int n;
    for (n = 0, p = fix[ix].list; p; p = *((void **) p))
	n++;
    if (n != fix[ix].list_size) {
	erts_fprintf(stderr, "FOUND IT ts=%d, sched=%d, ix=%d, n=%d, ls=%d %s!\n",
		     allctr->thread_safe, allctr->ix, ix, n, fix[ix].list_size, before ? "before" : "after");
	abort();
    }
}
#else
#define ERTS_DBG_CHK_FIX_LIST(A, FIX, IX, B)
#endif

erts_aint32_t
erts_alcu_fix_alloc_shrink(Allctr_t *allctr, erts_aint32_t flgs)
{
    int all_empty = 1;
    erts_aint32_t res = 0;
    int ix, o;
    ErtsAlcFixList_t *fix = allctr->fix;
    int flush = flgs == 0;

#ifdef USE_THREADS
    if (allctr->thread_safe)
	erts_mtx_lock(&allctr->mutex);
#endif

    for (ix = 0; ix < ERTS_ALC_NO_FIXED_SIZES; ix++) {
	ERTS_DBG_CHK_FIX_LIST(allctr, fix, ix, 1);
	if (flgs & ERTS_SSI_AUX_WORK_FIX_ALLOC_LOWER_LIM) {
	    fix[ix].limit = fix[ix].max_used;
	    if (fix[ix].limit < fix[ix].used)
		fix[ix].limit = fix[ix].used;
	    fix[ix].max_used = fix[ix].used;
	    ASSERT(fix[ix].limit >= 0);

	}
	if (flush) {
	    fix[ix].limit = 0;
	    fix[ix].max_used = fix[ix].used;
	    ASSERT(fix[ix].limit >= 0);
	}
	for (o = 0; o < ERTS_ALC_FIX_MAX_SHRINK_OPS || flush; o++) {
	    Block_t *blk;
	    void *ptr;

	    if (!flush && fix[ix].limit >= fix[ix].allocated)
		break;
	    if (fix[ix].list_size == 0)
		break;
	    ptr = fix[ix].list;
	    fix[ix].list = *((void **) ptr);
	    fix[ix].list_size--;	    

	    blk = UMEM2BLK(ptr);

	    if (IS_SBC_BLK(blk))
		destroy_carrier(allctr, blk);
	    else
		mbc_free(allctr, ptr);

	    fix[ix].allocated--;
	}
	if (fix[ix].list_size != 0) {
	    if (fix[ix].limit < fix[ix].allocated)
		res |= ERTS_SSI_AUX_WORK_FIX_ALLOC_DEALLOC;
	    all_empty = 0;
	}
	ERTS_DBG_CHK_FIX_LIST(allctr, fix, ix, 0);
    }

    if (all_empty && allctr->fix_shrink_scheduled) {
	allctr->fix_shrink_scheduled = 0;
	erts_set_aux_work_timeout(allctr->ix,
				  (ERTS_SSI_AUX_WORK_FIX_ALLOC_LOWER_LIM
				   | ERTS_SSI_AUX_WORK_FIX_ALLOC_DEALLOC),
				  0);
    }

#ifdef USE_THREADS
    if (allctr->thread_safe)
	erts_mtx_unlock(&allctr->mutex);
#endif

    return res;
}

#ifdef ERTS_SMP

#define ERTS_ALCU_DD_FIX_TYPE_OFFS \
  ((sizeof(ErtsAllctrDDBlock_t)-1)/sizeof(UWord) + 1)

#define ERTS_AU_PREF_ALLOC_IX_MASK \
   ((((UWord) 1) << ERTS_AU_PREF_ALLOC_BITS) - 1)
#define ERTS_AU_PREF_ALLOC_SIZE_MASK \
   ((((UWord) 1) << (sizeof(UWord)*8 - ERTS_AU_PREF_ALLOC_BITS)) - 1)

static ERTS_INLINE int
get_pref_allctr(void *extra, Allctr_t **allctr)
{
    ErtsAllocatorThrSpec_t *tspec = (ErtsAllocatorThrSpec_t *) extra;
    int pref_ix;

    pref_ix = ERTS_ALC_GET_THR_IX();

    ASSERT(sizeof(UWord) == sizeof(Allctr_t *));
    ASSERT(0 <= pref_ix && pref_ix < tspec->size);

    *allctr = tspec->allctr[pref_ix];
    return pref_ix;
}

static ERTS_INLINE void *
get_used_allctr(void *extra, void *p, Allctr_t **allctr, UWord *sizep)
{
    ErtsAllocatorThrSpec_t *tspec = (ErtsAllocatorThrSpec_t *) extra;
    void *ptr = (void *) (((char *) p) - sizeof(UWord));
    UWord ainfo = *((UWord *) ptr);
    int aix = (int) (ainfo & ERTS_AU_PREF_ALLOC_IX_MASK);
    *allctr = tspec->allctr[aix];
    if (sizep)
	*sizep = ((ainfo >> ERTS_AU_PREF_ALLOC_BITS)
		  & ERTS_AU_PREF_ALLOC_SIZE_MASK);
    return ptr;
}

static ERTS_INLINE void *
put_used_allctr(void *p, int ix, UWord size)
{
    UWord ainfo = (size >= ERTS_AU_PREF_ALLOC_SIZE_MASK
		   ? ERTS_AU_PREF_ALLOC_SIZE_MASK
		   : size);
    ainfo <<= ERTS_AU_PREF_ALLOC_BITS;
    ainfo |= (UWord) ix;
    *((UWord *) p) = ainfo;
    return (void *) (((char *) p) + sizeof(UWord));
}

static void
init_dd_queue(ErtsAllctrDDQueue_t *ddq)
{
    erts_atomic_init_nob(&ddq->tail.data.marker.atmc_next, ERTS_AINT_NULL);
    erts_atomic_init_nob(&ddq->tail.data.last,
			 (erts_aint_t) &ddq->tail.data.marker);
    erts_atomic_init_nob(&ddq->tail.data.um_refc[0], 0);
    erts_atomic_init_nob(&ddq->tail.data.um_refc[1], 0);
    erts_atomic32_init_nob(&ddq->tail.data.um_refc_ix, 0);
    ddq->head.first = &ddq->tail.data.marker;
    ddq->head.unref_end = &ddq->tail.data.marker;
    ddq->head.next.thr_progress = erts_thr_progress_current();
    ddq->head.next.thr_progress_reached = 1;
    ddq->head.next.um_refc_ix = 1;
    ddq->head.next.unref_end = &ddq->tail.data.marker;
    ddq->head.used_marker = 1;
}

static ERTS_INLINE erts_aint_t
ddq_managed_thread_enqueue(ErtsAllctrDDQueue_t *ddq, void *ptr)
{
    erts_aint_t first_ilast, ilast, itmp;
    ErtsAllctrDDBlock_t *this = ptr;

    erts_atomic_init_nob(&this->atmc_next, ERTS_AINT_NULL);

    /* Enqueue at end of list... */

    first_ilast = ilast = erts_atomic_read_nob(&ddq->tail.data.last);
    while (1) {
	ErtsAllctrDDBlock_t *last = (ErtsAllctrDDBlock_t *) ilast;
	itmp = erts_atomic_cmpxchg_mb(&last->atmc_next,
				       (erts_aint_t) this,
				       ERTS_AINT_NULL);
	if (itmp == ERTS_AINT_NULL)
	    break;
	ilast = itmp;
    }

    /* Move last pointer forward... */
    while (1) {
	if (erts_atomic_read_rb(&this->atmc_next) != ERTS_AINT_NULL) {
	    ilast = erts_atomic_read_rb(&ddq->tail.data.last);
	    if (first_ilast != ilast) {
		/* Someone else will move it forward */
		return ilast;
	    }
	}
	itmp = erts_atomic_cmpxchg_mb(&ddq->tail.data.last,
				      (erts_aint_t) this,
				      ilast);
	if (ilast == itmp)
	    return (erts_aint_t) this;
	ilast = itmp;
    }
}

static ERTS_INLINE int
ddq_enqueue(ErtsAlcType_t type, ErtsAllctrDDQueue_t *ddq, void *ptr)
{
    erts_aint_t ilast;
    int um_refc_ix = 0;
    int managed_thread = erts_thr_progress_is_managed_thread();
    if (!managed_thread) {
	um_refc_ix = erts_atomic32_read_acqb(&ddq->tail.data.um_refc_ix);
	while (1) {
	    int tmp_um_refc_ix;
	    erts_atomic_inc_acqb(&ddq->tail.data.um_refc[um_refc_ix]);
	    tmp_um_refc_ix = erts_atomic32_read_acqb(&ddq->tail.data.um_refc_ix);
	    if (tmp_um_refc_ix == um_refc_ix)
		break;
	    erts_atomic_dec_relb(&ddq->tail.data.um_refc[um_refc_ix]);
	    um_refc_ix = tmp_um_refc_ix;
	}
    }

    ilast = ddq_managed_thread_enqueue(ddq, ptr);

    if (!managed_thread)
	erts_atomic_dec_relb(&ddq->tail.data.um_refc[um_refc_ix]);
    return ilast == (erts_aint_t) ptr;
}

static ERTS_INLINE void *
ddq_dequeue(ErtsAllctrDDQueue_t *ddq)
{
    ErtsAllctrDDBlock_t *blk;

    if (ddq->head.first == ddq->head.unref_end)
	return NULL;

    blk = ddq->head.first;
    if (blk == &ddq->tail.data.marker) {
	ASSERT(ddq->head.used_marker);
	ddq->head.used_marker = 0;
	blk = ((ErtsAllctrDDBlock_t *)
	       erts_atomic_read_nob(&blk->atmc_next));
	if (blk == ddq->head.unref_end) {
	    ddq->head.first = blk;
	    return NULL;
	}
    }

    ddq->head.first = ((ErtsAllctrDDBlock_t *)
		       erts_atomic_read_nob(&blk->atmc_next));

    ASSERT(ddq->head.first);

    return (void *) blk;
}

static int
ddq_check_incoming(ErtsAllctrDDQueue_t *ddq)
{
    erts_aint_t ilast = erts_atomic_read_nob(&ddq->tail.data.last);
    if (((ErtsAllctrDDBlock_t *) ilast) == &ddq->tail.data.marker
	&& ddq->head.first == &ddq->tail.data.marker) {
	/* Nothing more to do... */
	return 0;
    }

    if (ddq->head.next.thr_progress_reached
	|| erts_thr_progress_has_reached(ddq->head.next.thr_progress)) {
	int um_refc_ix;
	ddq->head.next.thr_progress_reached = 1;
	um_refc_ix = ddq->head.next.um_refc_ix;
	if (erts_atomic_read_acqb(&ddq->tail.data.um_refc[um_refc_ix]) == 0) {
	    /* Move unreferenced end pointer forward... */

	    ddq->head.unref_end = ddq->head.next.unref_end;

	    if (!ddq->head.used_marker
		&& ddq->head.unref_end == (ErtsAllctrDDBlock_t *) ilast) {
		ddq->head.used_marker = 1;
		ilast = ddq_managed_thread_enqueue(ddq, &ddq->tail.data.marker);
	    }

	    if (ddq->head.unref_end == (ErtsAllctrDDBlock_t *) ilast)
		ERTS_THR_MEMORY_BARRIER;
	    else {
		ddq->head.next.unref_end = (ErtsAllctrDDBlock_t *) ilast;
		ddq->head.next.thr_progress = erts_thr_progress_later();
		erts_atomic32_set_relb(&ddq->tail.data.um_refc_ix,
				       um_refc_ix);
		ddq->head.next.um_refc_ix = um_refc_ix == 0 ? 1 : 0;
		ddq->head.next.thr_progress_reached = 0;
	    }
	}
    }
    return 1;
}

static ERTS_INLINE void
store_earliest_thr_prgr(ErtsThrPrgrVal *prev_val, ErtsAllctrDDQueue_t *ddq)
{
    if (!ddq->head.next.thr_progress_reached
	&& (*prev_val == ERTS_THR_PRGR_INVALID
	    || erts_thr_progress_cmp(ddq->head.next.thr_progress,
				     *prev_val) < 0)) {
	*prev_val = ddq->head.next.thr_progress;
    }
}

static ERTS_INLINE int
handle_delayed_dealloc(Allctr_t *allctr,
		       int allctr_locked,
		       int use_limit,
		       int ops_limit,
		       int *need_thr_progress,
		       ErtsThrPrgrVal *thr_prgr_p,
		       int *need_more_work)
{
    int need_thr_prgr = 0;
    int need_mr_wrk = 0;
    int have_checked_incoming = 0;
    int ops = 0;
    ErtsAlcFixList_t *fix;
    int res;
    ErtsAllctrDDQueue_t *ddq;

    if (allctr->thread_safe && !allctr_locked)
	erts_mtx_lock(&allctr->mutex);

    ERTS_ALCU_DBG_CHK_THR_ACCESS(allctr);

    fix = allctr->fix;

    ddq = &allctr->dd.q;

    res = 0;

    while (1) {
	Block_t *blk;
	void *ptr;
	int ix;

	if (use_limit && ++ops > ops_limit) {
	    if (ddq->head.first != ddq->head.unref_end) {
		need_mr_wrk = 1;
		if (need_more_work)
		    *need_more_work |= 1;
	    }
	    break;
	}

    dequeue:
	ptr = ddq_dequeue(ddq);
	if (!ptr) {
	    if (have_checked_incoming)
		break;
	    need_thr_prgr = ddq_check_incoming(ddq);
	    if (need_thr_progress) {
		*need_thr_progress |= need_thr_prgr;
		if (need_thr_prgr)
		    store_earliest_thr_prgr(thr_prgr_p, ddq);

	    }
	    have_checked_incoming = 1;
	    goto dequeue;
	}

	res = 1;

	INC_CC(allctr->calls.this_free);

	if (fix) {
	    ErtsAlcType_t type;

	    type = (ErtsAlcType_t) ((UWord *) ptr)[ERTS_ALCU_DD_FIX_TYPE_OFFS];
	    ix = type - ERTS_ALC_N_MIN_A_FIXED_SIZE;
	    ERTS_DBG_CHK_FIX_LIST(allctr, fix, ix, 1);
	    fix[ix].used--;
	    if (fix[ix].allocated < fix[ix].limit
		&& fix[ix].list_size < ERTS_ALCU_FIX_MAX_LIST_SZ) {
		*((void **) ptr) = fix[ix].list;
		fix[ix].list = ptr;
		fix[ix].list_size++;
		if (!allctr->fix_shrink_scheduled) {
		    allctr->fix_shrink_scheduled = 1;
		    erts_set_aux_work_timeout(
			allctr->ix,
			(ERTS_SSI_AUX_WORK_FIX_ALLOC_LOWER_LIM
			 | ERTS_SSI_AUX_WORK_FIX_ALLOC_DEALLOC),
			1);
		}
		ERTS_DBG_CHK_FIX_LIST(allctr, fix, ix, 0);
		continue;
	    }
	    fix[ix].allocated--;
	    if (fix[ix].list && fix[ix].allocated > fix[ix].limit) {
		blk = UMEM2BLK(ptr);
		if (IS_SBC_BLK(blk))
		    destroy_carrier(allctr, blk);
		else
		    mbc_free(allctr, ptr);
		ptr = fix[ix].list;
		fix[ix].list = *((void **) ptr);
		fix[ix].list_size--;
		fix[ix].allocated--;
	    }
	}

	blk = UMEM2BLK(ptr);

	if (IS_SBC_BLK(blk))
	    destroy_carrier(allctr, blk);
	else
	    mbc_free(allctr, ptr);
	ERTS_DBG_CHK_FIX_LIST(allctr, fix, ix, 0);
    }

    if (need_thr_progress && !(need_thr_prgr | need_mr_wrk)) {
	need_thr_prgr = ddq_check_incoming(ddq);
	*need_thr_progress |= need_thr_prgr;
	if (need_thr_prgr)
	    store_earliest_thr_prgr(thr_prgr_p, ddq);
    }

    if (allctr->thread_safe && !allctr_locked)
	erts_mtx_unlock(&allctr->mutex);
   return res;
}

static ERTS_INLINE void
enqueue_dealloc_other_instance(ErtsAlcType_t type, Allctr_t *allctr, void *ptr)
{
    if (allctr->fix)
	((UWord *) ptr)[ERTS_ALCU_DD_FIX_TYPE_OFFS] = (UWord) type;

    if (ddq_enqueue(type, &allctr->dd.q, ptr))
	erts_alloc_notify_delayed_dealloc(allctr->ix);
}

#endif

#ifdef ERTS_SMP
void
erts_alcu_check_delayed_dealloc(Allctr_t *allctr,
				int limit,
				int *need_thr_progress,
				ErtsThrPrgrVal *thr_prgr_p,
				int *more_work)
{
    handle_delayed_dealloc(allctr,
			   0,
			   limit,
			   ERTS_ALCU_DD_OPS_LIM_HIGH,
			   need_thr_progress,
			   thr_prgr_p,
			   more_work);
}
#endif

#define ERTS_ALCU_HANDLE_DD_IN_OP(Allctr, Locked) \
    handle_delayed_dealloc((Allctr), (Locked), 1, \
			   ERTS_ALCU_DD_OPS_LIM_LOW, NULL, NULL, NULL)

/* Multi block carrier alloc/realloc/free ... */

/* NOTE! mbc_alloc() may in case of memory shortage place the requested
 * block in a sbc.
 */
static ERTS_INLINE void *
mbc_alloc_block(Allctr_t *allctr, Uint size, Uint *blk_szp, Uint32 *alcu_flgsp)
{
    Block_t *blk;
    Uint get_blk_sz;
    Uint sbmbct;

    ASSERT(size);
    ASSERT(size < allctr->sbc_threshold);

    *blk_szp = get_blk_sz = UMEMSZ2BLKSZ(allctr, size);

    sbmbct = allctr->sbmbc_threshold;
    if (sbmbct) {
	if (get_blk_sz < sbmbct) {
	    *alcu_flgsp |= ERTS_ALCU_FLG_SBMBC;
	    if (get_blk_sz + allctr->min_block_size > sbmbct) {
		/* Since we use block size to determine if blocks are
		   located in sbmbc or not... */
		get_blk_sz += allctr->min_block_size;
	    }
	}
    }

#ifdef ERTS_SMP
    if (allctr->dd.use)
	ERTS_ALCU_HANDLE_DD_IN_OP(allctr, 1);
#endif

    blk = (*allctr->get_free_block)(allctr, get_blk_sz, NULL, 0, *alcu_flgsp);

#ifdef ERTS_SMP
    if (!blk && allctr->dd.use) {
	if (ERTS_ALCU_HANDLE_DD_IN_OP(allctr, 1))
	    blk = (*allctr->get_free_block)(allctr, get_blk_sz, NULL, 0,
					    *alcu_flgsp);
    }
#endif

    if (!blk) {
	if ((*alcu_flgsp) & ERTS_ALCU_FLG_SBMBC) 
	    blk = create_sbmbc(allctr, get_blk_sz);
	else {
#if HALFWORD_HEAP
	    blk = create_carrier(allctr, get_blk_sz, CFLG_MBC|CFLG_FORCE_MSEG);
#else
	    blk = create_carrier(allctr, get_blk_sz, CFLG_MBC);
	    if (!blk) {
		/* Emergency! We couldn't create the carrier as we wanted.
		   Try to place it in a sys_alloced sbc. */
		blk = create_carrier(allctr,
				     size,
				     (CFLG_SBC
				      | CFLG_FORCE_SIZE
				      | CFLG_FORCE_SYS_ALLOC));
	    }
#endif
	}
    }

#ifdef ERTS_ALLOC_UTIL_HARD_DEBUG
    if (IS_MBC_BLK(blk)) {
	(*allctr->link_free_block)(allctr, blk, *alcu_flgsp);
	HARD_CHECK_BLK_CARRIER(allctr, blk);
	(*allctr->unlink_free_block)(allctr, blk, *alcu_flgsp);
    }
#endif

    return blk;
}

static ERTS_INLINE void
mbc_alloc_finalize(Allctr_t *allctr,
		   Block_t *blk,
		   Uint org_blk_sz,
		   UWord flags,
		   Uint want_blk_sz,
		   int valid_blk_info,
		   Uint32 alcu_flgs)
{
    Uint blk_sz;
    Uint nxt_blk_sz;
    Block_t *nxt_blk;
    UWord prev_free_flg = flags & PREV_FREE_BLK_HDR_FLG;

    ASSERT(org_blk_sz >= want_blk_sz);
    ASSERT(blk);

#ifdef DEBUG
    nxt_blk = NULL;
#endif

    if (org_blk_sz - allctr->min_block_size >= want_blk_sz) {
	/* Shrink block... */
	blk_sz = want_blk_sz;
	nxt_blk_sz = org_blk_sz - blk_sz;
	SET_BLK_HDR(blk,
		    blk_sz,
		    SBH_THIS_ALLOCED|SBH_NOT_LAST_BLK|prev_free_flg);

	nxt_blk = NXT_BLK(blk);
	SET_BLK_HDR(nxt_blk,
		    nxt_blk_sz,
		    (SBH_THIS_FREE
		     | SBH_PREV_ALLOCED
		     | (flags & LAST_BLK_HDR_FLG)));

	if (!(flags & LAST_BLK_HDR_FLG)) {
	    SET_BLK_SZ_FTR(nxt_blk, nxt_blk_sz);
	    if (!valid_blk_info) {
		Block_t *nxt_nxt_blk = NXT_BLK(nxt_blk);
		SET_PREV_BLK_FREE(nxt_nxt_blk);
	    }
	}
	(*allctr->link_free_block)(allctr, nxt_blk, alcu_flgs);

	ASSERT(IS_NOT_LAST_BLK(blk));
	ASSERT(IS_FREE_BLK(nxt_blk));
	ASSERT((flags & LAST_BLK_HDR_FLG)
	       ? IS_LAST_BLK(nxt_blk)
	       : IS_NOT_LAST_BLK(nxt_blk));
	ASSERT((flags & LAST_BLK_HDR_FLG)
	       || nxt_blk == PREV_BLK(NXT_BLK(nxt_blk)));
	ASSERT((flags & LAST_BLK_HDR_FLG)
	       || IS_PREV_BLK_FREE(NXT_BLK(nxt_blk)));
	ASSERT(nxt_blk_sz == BLK_SZ(nxt_blk));
	ASSERT(nxt_blk_sz % sizeof(Unit_t) == 0);
	ASSERT(nxt_blk_sz >= allctr->min_block_size);
    }
    else {
	blk_sz = org_blk_sz;
	if (flags & LAST_BLK_HDR_FLG) {
	    if (valid_blk_info)
		SET_BLK_ALLOCED(blk);
	    else
		SET_BLK_HDR(blk,
			    blk_sz,
			    SBH_THIS_ALLOCED|SBH_LAST_BLK|prev_free_flg);
	}
	else {
	    if (valid_blk_info)
		SET_BLK_ALLOCED(blk);
	    else
		SET_BLK_HDR(blk,
			    blk_sz,
			    SBH_THIS_ALLOCED|SBH_NOT_LAST_BLK|prev_free_flg);
	    nxt_blk = NXT_BLK(blk);
	    SET_PREV_BLK_ALLOCED(nxt_blk);
	}

	ASSERT((flags & LAST_BLK_HDR_FLG)
	       ? IS_LAST_BLK(blk)
	       : IS_NOT_LAST_BLK(blk));
    }

    STAT_MBC_BLK_ALLOC(allctr, blk_sz, alcu_flgs);

    ASSERT(IS_ALLOCED_BLK(blk));
    ASSERT(blk_sz == BLK_SZ(blk));
    ASSERT(blk_sz % sizeof(Unit_t) == 0);
    ASSERT(blk_sz >= allctr->min_block_size);
    ASSERT(blk_sz >= want_blk_sz);
    ASSERT(IS_MBC_BLK(blk));

    ASSERT(!nxt_blk || IS_PREV_BLK_ALLOCED(nxt_blk));
    ASSERT(!nxt_blk || IS_MBC_BLK(nxt_blk));

    HARD_CHECK_BLK_CARRIER(allctr, blk);
}

static void *
mbc_alloc(Allctr_t *allctr, Uint size)
{
    Block_t *blk;
    Uint blk_sz;
    Uint32 alcu_flgs = 0;
    blk = mbc_alloc_block(allctr, size, &blk_sz, &alcu_flgs);
    if (!blk)
	return NULL;
    if (IS_MBC_BLK(blk))
	mbc_alloc_finalize(allctr,
			   blk,
			   BLK_SZ(blk),
			   GET_BLK_HDR_FLGS(blk),
			   blk_sz,
			   1,
			   alcu_flgs);
    return BLK2UMEM(blk);
}

static void
mbc_free(Allctr_t *allctr, void *p)
{
    Uint is_first_blk;
    Uint is_last_blk;
    Uint32 alcu_flgs = 0;
    Uint blk_sz;
    Block_t *blk;
    Block_t *nxt_blk;


    ASSERT(p);

    blk = UMEM2BLK(p);
    blk_sz = BLK_SZ(blk);
    if (blk_sz < allctr->sbmbc_threshold)
	alcu_flgs |= ERTS_ALCU_FLG_SBMBC;

    ASSERT(IS_MBC_BLK(blk));
    ASSERT(blk_sz >= allctr->min_block_size);

    HARD_CHECK_BLK_CARRIER(allctr, blk);

    STAT_MBC_BLK_FREE(allctr, blk_sz, alcu_flgs);

    is_first_blk = IS_FIRST_BLK(blk);
    is_last_blk = IS_LAST_BLK(blk);

    if (!is_first_blk && IS_PREV_BLK_FREE(blk)) {
	/* Coalesce with previous block... */
	blk = PREV_BLK(blk);
	(*allctr->unlink_free_block)(allctr, blk, alcu_flgs);

	blk_sz += BLK_SZ(blk);
	is_first_blk = IS_FIRST_BLK(blk);
	SET_BLK_SZ(blk, blk_sz);
    }
    else {
	SET_BLK_FREE(blk);
    }

    if (is_last_blk)
	SET_LAST_BLK(blk);
    else {
	nxt_blk = NXT_BLK(blk);
	if (IS_FREE_BLK(nxt_blk)) {
	    /* Coalesce with next block... */
	    (*allctr->unlink_free_block)(allctr, nxt_blk, alcu_flgs);
	    blk_sz += BLK_SZ(nxt_blk);
	    SET_BLK_SZ(blk, blk_sz);

	    is_last_blk = IS_LAST_BLK(nxt_blk);
	    if (is_last_blk) 
		SET_LAST_BLK(blk);
	    else {
		SET_NOT_LAST_BLK(blk);
		SET_BLK_SZ_FTR(blk, blk_sz);
	    }
	}
	else {
	    SET_PREV_BLK_FREE(nxt_blk);
	    SET_NOT_LAST_BLK(blk);
	    SET_BLK_SZ_FTR(blk, blk_sz);
	}

    }

    ASSERT(is_last_blk  ? IS_LAST_BLK(blk)  : IS_NOT_LAST_BLK(blk));
    ASSERT(is_first_blk ? IS_FIRST_BLK(blk) : IS_NOT_FIRST_BLK(blk));
    ASSERT(IS_FREE_BLK(blk));
    ASSERT(is_first_blk || IS_PREV_BLK_ALLOCED(blk));
    ASSERT(is_last_blk  || IS_PREV_BLK_FREE(NXT_BLK(blk)));
    ASSERT(blk_sz == BLK_SZ(blk));
    ASSERT(is_last_blk || blk == PREV_BLK(NXT_BLK(blk)));
    ASSERT(blk_sz % sizeof(Unit_t) == 0);
    ASSERT(IS_MBC_BLK(blk));

    if (is_first_blk
	&& is_last_blk
	&& allctr->main_carrier != FBLK2MBC(allctr, blk)) {
	if (alcu_flgs & ERTS_ALCU_FLG_SBMBC)
	    destroy_sbmbc(allctr, blk);
	else
	    destroy_carrier(allctr, blk);
    }
    else {
	(*allctr->link_free_block)(allctr, blk, alcu_flgs);
	HARD_CHECK_BLK_CARRIER(allctr, blk);
    }
}

static void *
mbc_realloc(Allctr_t *allctr, void *p, Uint size, Uint32 alcu_flgs)
{
    void *new_p;
    Uint old_blk_sz;
    Block_t *blk;
#ifndef MBC_REALLOC_ALWAYS_MOVES
    Block_t *new_blk, *cand_blk;
    Uint cand_blk_sz;
    Uint blk_sz, get_blk_sz;
    Block_t *nxt_blk;
    Uint nxt_blk_sz;
    Uint is_last_blk;
#endif /* #ifndef MBC_REALLOC_ALWAYS_MOVES */

#ifdef ERTS_SMP
    if (allctr->dd.use)
	ERTS_ALCU_HANDLE_DD_IN_OP(allctr, 1);
#endif

    ASSERT(p);
    ASSERT(size);
    ASSERT(size < allctr->sbc_threshold);

    blk = (Block_t *) UMEM2BLK(p);
    old_blk_sz = BLK_SZ(blk);

    ASSERT(old_blk_sz >= allctr->min_block_size);

#ifdef MBC_REALLOC_ALWAYS_MOVES
    if (alcu_flgs & ERTS_ALCU_FLG_FAIL_REALLOC_MOVE)
	return NULL;
#else /* !MBC_REALLOC_ALWAYS_MOVES */
    get_blk_sz = blk_sz = UMEMSZ2BLKSZ(allctr, size);
    if ((alcu_flgs & ERTS_ALCU_FLG_SBMBC)
	&& (blk_sz + allctr->min_block_size > allctr->sbmbc_threshold)) {
	/* Since we use block size to determine if blocks are
	   located in sbmbc or not... */
	get_blk_sz = blk_sz + allctr->min_block_size;
    }

    ASSERT(IS_ALLOCED_BLK(blk));
    ASSERT(IS_MBC_BLK(blk));

    is_last_blk = IS_LAST_BLK(blk);

    if (old_blk_sz == blk_sz)
	return p;
    else if (blk_sz < old_blk_sz) {
	/* Shrink block... */
	Block_t *nxt_nxt_blk;
	Uint diff_sz_val = old_blk_sz - blk_sz;
	Uint old_blk_sz_val = old_blk_sz;

	if (get_blk_sz >= old_blk_sz)
	    return p;

	if (diff_sz_val >= (~((Uint) 0) / 100)) {
	    /* div both by 128 */
	    old_blk_sz_val >>= 7;
	    diff_sz_val >>= 7;
	}

	/* Avoid fragmentation by moving the block if it is shrunk much */
	if (100*diff_sz_val > allctr->mbc_move_threshold*old_blk_sz_val) {
	    if (alcu_flgs & ERTS_ALCU_FLG_FAIL_REALLOC_MOVE)
		return NULL;

	    cand_blk_sz = old_blk_sz;
	    if (!IS_PREV_BLK_FREE(blk) || IS_FIRST_BLK(blk))
		cand_blk = blk;
	    else {
		cand_blk = PREV_BLK(blk);
		cand_blk_sz += PREV_BLK_SZ(blk);
	    }
	    if (!is_last_blk) {
		nxt_blk = NXT_BLK(blk);
		if (IS_FREE_BLK(nxt_blk))
		    cand_blk_sz += BLK_SZ(nxt_blk);
	    }

	    new_blk = (*allctr->get_free_block)(allctr,
						get_blk_sz,
						cand_blk,
						cand_blk_sz,
						alcu_flgs);
	    if (new_blk || cand_blk != blk)
		goto move_into_new_blk;
	}

	/* Shrink at current location */

	nxt_blk_sz = old_blk_sz - blk_sz;

	if ((is_last_blk || IS_ALLOCED_BLK(NXT_BLK(blk)))
	    && (nxt_blk_sz < allctr->min_block_size))
	    return p;

	HARD_CHECK_BLK_CARRIER(allctr, blk);

	SET_BLK_SZ(blk, blk_sz);
	SET_NOT_LAST_BLK(blk);

	nxt_blk = NXT_BLK(blk);
	SET_BLK_HDR(nxt_blk,
		    nxt_blk_sz,
		    SBH_THIS_FREE|SBH_PREV_ALLOCED|SBH_NOT_LAST_BLK);

	STAT_MBC_BLK_FREE(allctr, old_blk_sz, alcu_flgs);
	STAT_MBC_BLK_ALLOC(allctr, blk_sz, alcu_flgs);

	ASSERT(BLK_SZ(blk) >= allctr->min_block_size);

	if (is_last_blk)
	    SET_LAST_BLK(nxt_blk);
	else {
	    nxt_nxt_blk = NXT_BLK(nxt_blk);
	    if (IS_FREE_BLK(nxt_nxt_blk)) {
		/* Coalesce with next free block... */
		nxt_blk_sz += BLK_SZ(nxt_nxt_blk);
		(*allctr->unlink_free_block)(allctr, nxt_nxt_blk, alcu_flgs);
		SET_BLK_SZ(nxt_blk, nxt_blk_sz);

		is_last_blk = IS_LAST_BLK(nxt_nxt_blk);
		if (is_last_blk)
		    SET_LAST_BLK(nxt_blk);
		else
		    SET_BLK_SZ_FTR(nxt_blk, nxt_blk_sz);
	    }
	    else {
		SET_BLK_SZ_FTR(nxt_blk, nxt_blk_sz);
		SET_PREV_BLK_FREE(nxt_nxt_blk);
	    }
	}

	(*allctr->link_free_block)(allctr, nxt_blk, alcu_flgs);


	ASSERT(IS_ALLOCED_BLK(blk));
	ASSERT(blk_sz == BLK_SZ(blk));
	ASSERT(blk_sz % sizeof(Unit_t) == 0);
	ASSERT(blk_sz >= allctr->min_block_size);
	ASSERT(blk_sz >= size + ABLK_HDR_SZ);
	ASSERT(IS_MBC_BLK(blk));
    
	ASSERT(IS_FREE_BLK(nxt_blk));
	ASSERT(IS_PREV_BLK_ALLOCED(nxt_blk));
	ASSERT(nxt_blk_sz == BLK_SZ(nxt_blk));
	ASSERT(nxt_blk_sz % sizeof(Unit_t) == 0);
	ASSERT(nxt_blk_sz >= allctr->min_block_size);
	ASSERT(IS_MBC_BLK(nxt_blk));
	ASSERT(is_last_blk ? IS_LAST_BLK(nxt_blk) : IS_NOT_LAST_BLK(nxt_blk));
	ASSERT(is_last_blk || nxt_blk == PREV_BLK(NXT_BLK(nxt_blk)));
	ASSERT(is_last_blk || IS_PREV_BLK_FREE(NXT_BLK(nxt_blk)));

	HARD_CHECK_BLK_CARRIER(allctr, blk);

	return p;
    }

    /* Need larger block... */

    if (!is_last_blk) {
	nxt_blk = NXT_BLK(blk);
	nxt_blk_sz = BLK_SZ(nxt_blk);
	if (IS_FREE_BLK(nxt_blk) && get_blk_sz <= old_blk_sz + nxt_blk_sz) {
	    /* Grow into next block... */

	    HARD_CHECK_BLK_CARRIER(allctr, blk);

	    (*allctr->unlink_free_block)(allctr, nxt_blk, alcu_flgs);
	    nxt_blk_sz -= blk_sz - old_blk_sz;

	    is_last_blk = IS_LAST_BLK(nxt_blk);
	    if (nxt_blk_sz < allctr->min_block_size) {
		blk_sz += nxt_blk_sz;

		SET_BLK_SZ(blk, blk_sz);

		if (is_last_blk) {
		    SET_LAST_BLK(blk);
#ifdef DEBUG
		    nxt_blk = NULL;
#endif
		}
		else {
		    nxt_blk = NXT_BLK(blk);
		    SET_PREV_BLK_ALLOCED(nxt_blk);
#ifdef DEBUG
		    is_last_blk = IS_LAST_BLK(nxt_blk);
		    nxt_blk_sz = BLK_SZ(nxt_blk);
#endif
		}
	    }
	    else {
		SET_BLK_SZ(blk, blk_sz);

		nxt_blk = NXT_BLK(blk);
		SET_BLK_HDR(nxt_blk,
			    nxt_blk_sz,
			    SBH_THIS_FREE|SBH_PREV_ALLOCED|SBH_NOT_LAST_BLK);

		if (is_last_blk)
		    SET_LAST_BLK(nxt_blk);
		else
		    SET_BLK_SZ_FTR(nxt_blk, nxt_blk_sz);

		(*allctr->link_free_block)(allctr, nxt_blk, alcu_flgs);

		ASSERT(IS_FREE_BLK(nxt_blk));
	    }

	    STAT_MBC_BLK_FREE(allctr, old_blk_sz, alcu_flgs);
	    STAT_MBC_BLK_ALLOC(allctr, blk_sz, alcu_flgs);


	    ASSERT(IS_ALLOCED_BLK(blk));
	    ASSERT(blk_sz == BLK_SZ(blk));
	    ASSERT(blk_sz % sizeof(Unit_t) == 0);
	    ASSERT(blk_sz >= allctr->min_block_size);
	    ASSERT(blk_sz >= size + ABLK_HDR_SZ);
	    ASSERT(IS_MBC_BLK(blk));

	    ASSERT(!nxt_blk || IS_PREV_BLK_ALLOCED(nxt_blk));
	    ASSERT(!nxt_blk || nxt_blk_sz == BLK_SZ(nxt_blk));
	    ASSERT(!nxt_blk || nxt_blk_sz % sizeof(Unit_t) == 0);
	    ASSERT(!nxt_blk || nxt_blk_sz >= allctr->min_block_size);
	    ASSERT(!nxt_blk || IS_MBC_BLK(nxt_blk));
	    ASSERT(!nxt_blk || (is_last_blk
				? IS_LAST_BLK(nxt_blk)
				: IS_NOT_LAST_BLK(nxt_blk)));
	    ASSERT(!nxt_blk || is_last_blk
		   || IS_ALLOCED_BLK(nxt_blk)
		   || nxt_blk == PREV_BLK(NXT_BLK(nxt_blk)));
	    ASSERT(!nxt_blk || is_last_blk
		   || IS_ALLOCED_BLK(nxt_blk)
		   || IS_PREV_BLK_FREE(NXT_BLK(nxt_blk)));

	    HARD_CHECK_BLK_CARRIER(allctr, blk);

	    return p;
	}
    }

    if (alcu_flgs & ERTS_ALCU_FLG_FAIL_REALLOC_MOVE)
	return NULL;

    /* Need to grow in another block */

    if (!IS_PREV_BLK_FREE(blk) || IS_FIRST_BLK(blk)) {
	cand_blk = NULL;
	cand_blk_sz = 0;
    }
    else {
	cand_blk = PREV_BLK(blk);
	cand_blk_sz = old_blk_sz + PREV_BLK_SZ(blk);

	if (!is_last_blk) {
	    nxt_blk = NXT_BLK(blk);
	    if (IS_FREE_BLK(nxt_blk))
		cand_blk_sz += BLK_SZ(nxt_blk);
	}
    }

    if (cand_blk_sz < get_blk_sz) {
	/* We wont fit in cand_blk get a new one */
#endif /* !MBC_REALLOC_ALWAYS_MOVES */

	new_p = mbc_alloc(allctr, size);
	if (!new_p)
	    return NULL;
	sys_memcpy(new_p, p, MIN(size, old_blk_sz - ABLK_HDR_SZ));
	mbc_free(allctr, p);

	return new_p;

#ifndef MBC_REALLOC_ALWAYS_MOVES

    }
    else {
	/* We will at least fit in cand_blk */

	new_blk = (*allctr->get_free_block)(allctr,
					    get_blk_sz,
					    cand_blk,
					    cand_blk_sz,
					    alcu_flgs);
    move_into_new_blk:
	/*
	 * new_blk, and cand_blk have to be correctly set
	 * when jumping to this label.
	 */

	if (new_blk) {
	    mbc_alloc_finalize(allctr,
			       new_blk,
			       BLK_SZ(new_blk),
			       GET_BLK_HDR_FLGS(new_blk),
			       blk_sz,
			       1,
			       alcu_flgs);
	    new_p = BLK2UMEM(new_blk);
	    sys_memcpy(new_p, p, MIN(size, old_blk_sz - ABLK_HDR_SZ));
	    mbc_free(allctr, p);
	    return new_p;
	}
	else {
	    Uint new_blk_sz;
	    UWord new_blk_flgs;
	    Uint prev_blk_sz;
	    Uint blk_cpy_sz;

	    ASSERT(IS_PREV_BLK_FREE(blk));
	    ASSERT(cand_blk == PREV_BLK(blk));

	    prev_blk_sz = PREV_BLK_SZ(blk);
	    new_blk = cand_blk;
	    new_blk_sz = prev_blk_sz + old_blk_sz;
	    new_blk_flgs = GET_BLK_HDR_FLGS(new_blk);

	    HARD_CHECK_BLK_CARRIER(allctr, blk);

	    (*allctr->unlink_free_block)(allctr, new_blk, alcu_flgs); /* prev */

	    if (is_last_blk) 
		new_blk_flgs |= LAST_BLK_HDR_FLG;
	    else {
		nxt_blk = NXT_BLK(blk);
		if (IS_FREE_BLK(nxt_blk)) {
		    new_blk_flgs |= GET_LAST_BLK_HDR_FLG(nxt_blk);
		    new_blk_sz += BLK_SZ(nxt_blk);
		    (*allctr->unlink_free_block)(allctr, nxt_blk, alcu_flgs);
		}
	    }

	    /*
	     * Copy user-data then update new blocks in mbc_alloc_finalize().
	     * mbc_alloc_finalize() may write headers at old location of
	     * user data; therfore, order is important.
	     */

	    new_p = BLK2UMEM(new_blk);
	    blk_cpy_sz = MIN(blk_sz, old_blk_sz);

	    if (prev_blk_sz >= blk_cpy_sz)
		sys_memcpy(new_p, p, blk_cpy_sz - ABLK_HDR_SZ);
	    else
		sys_memmove(new_p, p, blk_cpy_sz - ABLK_HDR_SZ);

	    mbc_alloc_finalize(allctr,
			       new_blk,
			       new_blk_sz,
			       new_blk_flgs,
			       blk_sz,
			       0,
			       alcu_flgs);

	    STAT_MBC_BLK_FREE(allctr, old_blk_sz, alcu_flgs);

	    return new_p;
	}
    }
#endif /* !MBC_REALLOC_ALWAYS_MOVES */
}

#ifdef DEBUG

#if HAVE_ERTS_MSEG
#define ASSERT_MSEG_UNIT_SIZE_MULTIPLE(CSZ) ASSERT((CSZ) % mseg_unit_size == 0)
#else
#define ASSERT_MSEG_UNIT_SIZE_MULTIPLE(CSZ)
#endif

#define CHECK_1BLK_CARRIER(A, SBC, MSEGED, C, CSZ, B, BSZ)		\
do {									\
    ASSERT(IS_FIRST_BLK((B)));						\
    ASSERT(IS_LAST_BLK((B)));						\
    ASSERT((CSZ) == CARRIER_SZ((C)));					\
    ASSERT((BSZ) == BLK_SZ((B)));					\
    ASSERT((BSZ) % sizeof(Unit_t) == 0);				\
    if ((SBC)) {							\
	ASSERT(IS_SBC_BLK((B)));					\
	ASSERT(IS_SB_CARRIER((C)));					\
    }									\
    else {								\
	ASSERT(IS_MBC_BLK((B)));					\
	ASSERT(IS_MB_CARRIER((C)));					\
    }									\
    if ((MSEGED)) {							\
	ASSERT(IS_MSEG_CARRIER((C)));					\
	ASSERT_MSEG_UNIT_SIZE_MULTIPLE((CSZ));				\
    }									\
    else {								\
	ASSERT(IS_SYS_ALLOC_CARRIER((C)));					\
	ASSERT((CSZ) % sizeof(Unit_t) == 0);				\
    }									\
} while (0)

#else
#define CHECK_1BLK_CARRIER(A, SBC, MSEGED, C, CSZ, B, BSZ)
#endif

static Block_t *
create_sbmbc(Allctr_t *allctr, Uint umem_sz)
{
    Block_t *blk;
    Uint blk_sz;
    Uint crr_sz = allctr->sbmbc_size;
    Carrier_t *crr;

#if HALFWORD_HEAP
    if (allctr->mseg_opt.low_mem)
	crr = erts_alloc(ERTS_ALC_T_SBMBC_LOW, crr_sz);
    else
#endif
	crr = erts_alloc(ERTS_ALC_T_SBMBC, crr_sz);

    INC_CC(allctr->calls.sbmbc_alloc);
    SET_CARRIER_HDR(crr, crr_sz, SCH_SYS_ALLOC|SCH_MBC);

    blk = MBC2FBLK(allctr, crr);

#ifdef ERTS_ALLOC_UTIL_HARD_DEBUG
    if (allctr->mbc_header_size % sizeof(Unit_t) == 0)
	crr_sz -= sizeof(UWord);
#endif

    blk_sz = UNIT_FLOOR(crr_sz - allctr->mbc_header_size);

    SET_MBC_BLK_FTR(((UWord *) blk)[-1]);
    SET_BLK_HDR(blk, blk_sz, SBH_THIS_FREE|SBH_PREV_FREE|SBH_LAST_BLK);

#ifdef ERTS_ALLOC_UTIL_HARD_DEBUG
    *((Carrier_t **) NXT_BLK(blk)) = crr;
#endif

    link_carrier(&allctr->sbmbc_list, crr);

#ifdef ERTS_ALLOC_UTIL_HARD_DEBUG
    if (allctr->mbc_header_size % sizeof(Unit_t) == 0)
	crr_sz += sizeof(UWord);
#endif

    STAT_SBMBC_ALLOC(allctr, crr_sz);
    CHECK_1BLK_CARRIER(allctr, 0, 0, crr, crr_sz, blk, blk_sz);
#ifdef ERTS_ALLOC_UTIL_HARD_DEBUG
    if (allctr->mbc_header_size % sizeof(Unit_t) == 0)
	crr_sz -= sizeof(UWord);
#endif
    if (allctr->creating_mbc)
	(*allctr->creating_mbc)(allctr, crr, ERTS_ALCU_FLG_SBMBC);

    DEBUG_SAVE_ALIGNMENT(crr);
    return blk;
}

static void
destroy_sbmbc(Allctr_t *allctr, Block_t *blk)
{
    Uint crr_sz;
    Carrier_t *crr;

    ASSERT(IS_FIRST_BLK(blk));

    ASSERT(IS_MBC_BLK(blk));

    crr = FBLK2MBC(allctr, blk);
    crr_sz = CARRIER_SZ(crr);

#ifdef DEBUG
    if (!allctr->stopped) {
	ASSERT(IS_LAST_BLK(blk));

#ifdef ERTS_ALLOC_UTIL_HARD_DEBUG
	(*allctr->link_free_block)(allctr, blk, ERTS_ALCU_FLG_SBMBC);
	HARD_CHECK_BLK_CARRIER(allctr, blk);
	(*allctr->unlink_free_block)(allctr, blk, ERTS_ALCU_FLG_SBMBC);
#endif
    }
#endif

    STAT_SBMBC_FREE(allctr, crr_sz);

    unlink_carrier(&allctr->sbmbc_list, crr);
    if (allctr->destroying_mbc)
	(*allctr->destroying_mbc)(allctr, crr, ERTS_ALCU_FLG_SBMBC);

    INC_CC(allctr->calls.sbmbc_free);

#if HALFWORD_HEAP
    if (allctr->mseg_opt.low_mem)
	erts_free(ERTS_ALC_T_SBMBC_LOW, crr);
    else
#endif
	erts_free(ERTS_ALC_T_SBMBC, crr);
}

static Block_t *
create_carrier(Allctr_t *allctr, Uint umem_sz, UWord flags)
{
    Block_t *blk;
    Carrier_t *crr;
    Uint blk_sz, bcrr_sz, crr_sz;
#if HAVE_ERTS_MSEG
    int have_tried_sys_alloc = 0, have_tried_mseg = 0;
#endif
#ifdef DEBUG
    int is_mseg = 0;
#endif

    ASSERT((flags & CFLG_SBC && !(flags & CFLG_MBC))
	   || (flags & CFLG_MBC && !(flags & CFLG_SBC)));

    blk_sz = UMEMSZ2BLKSZ(allctr, umem_sz);

#if HAVE_ERTS_MSEG

    if (flags & CFLG_FORCE_SYS_ALLOC)
	goto try_sys_alloc;
    if (flags & CFLG_FORCE_MSEG)
	goto try_mseg;
    if (erts_mseg_no(&allctr->mseg_opt) >= max_mseg_carriers)
	goto try_sys_alloc;
    if (flags & CFLG_SBC) {
	if (allctr->sbcs.curr.norm.mseg.no >= allctr->max_mseg_sbcs)
	    goto try_sys_alloc;
    }
    else {
	if (allctr->mbcs.curr.norm.mseg.no >= allctr->max_mseg_mbcs)
	    goto try_sys_alloc;
    }

 try_mseg:

    if (flags & CFLG_SBC) {
	crr_sz = blk_sz + allctr->sbc_header_size;
    }
    else {
	crr_sz = (*allctr->get_next_mbc_size)(allctr);
	if (crr_sz < allctr->mbc_header_size + blk_sz)
	    crr_sz = allctr->mbc_header_size + blk_sz;
#ifdef ERTS_ALLOC_UTIL_HARD_DEBUG
	if (allctr->mbc_header_size % sizeof(Unit_t) == 0)
	    crr_sz += sizeof(UWord);
#endif
    }
    crr_sz = MSEG_UNIT_CEILING(crr_sz);
    ASSERT(crr_sz % mseg_unit_size == 0);

    crr = (Carrier_t *) alcu_mseg_alloc(allctr, &crr_sz);
    if (!crr) {
	have_tried_mseg = 1;
	if (!(have_tried_sys_alloc || flags & CFLG_FORCE_MSEG))
	    goto try_sys_alloc;
	return NULL;
    }

#ifdef DEBUG
    is_mseg = 1;
#endif
    if (flags & CFLG_SBC) {
	SET_CARRIER_HDR(crr, crr_sz, SCH_MSEG|SCH_SBC);
	STAT_MSEG_SBC_ALLOC(allctr, crr_sz, blk_sz);
	goto sbc_final_touch;
    }
    else {
	SET_CARRIER_HDR(crr, crr_sz, SCH_MSEG|SCH_MBC);
	STAT_MSEG_MBC_ALLOC(allctr, crr_sz);
	goto mbc_final_touch;
    }

 try_sys_alloc:
#endif /* #if HAVE_ERTS_MSEG */

    if (flags & CFLG_SBC) {
	bcrr_sz = blk_sz + allctr->sbc_header_size;
    }
    else {
	bcrr_sz = allctr->mbc_header_size + blk_sz;
	if (!(flags & CFLG_MAIN_CARRIER)
	    && bcrr_sz < allctr->smallest_mbc_size)
	    bcrr_sz = allctr->smallest_mbc_size;
#ifdef ERTS_ALLOC_UTIL_HARD_DEBUG
	if (allctr->mbc_header_size % sizeof(Unit_t) == 0)
	    bcrr_sz += sizeof(UWord);
#endif

    }

    crr_sz = (flags & CFLG_FORCE_SIZE
	      ? UNIT_CEILING(bcrr_sz)
	      : SYS_ALLOC_CARRIER_CEILING(bcrr_sz));

    crr = (Carrier_t *) alcu_sys_alloc(allctr, crr_sz);
	
    if (!crr) {
	if (crr_sz > UNIT_CEILING(bcrr_sz)) {
	    crr_sz = UNIT_CEILING(bcrr_sz);
	    crr = (Carrier_t *) alcu_sys_alloc(allctr, crr_sz);
	}
	if (!crr) {
#if HAVE_ERTS_MSEG
	    have_tried_sys_alloc = 1;
	    if (!(have_tried_mseg || flags & CFLG_FORCE_SYS_ALLOC))
		goto try_mseg;
#endif
	    return NULL;
	}
    }
    if (flags & CFLG_SBC) {
	SET_CARRIER_HDR(crr, crr_sz, SCH_SYS_ALLOC|SCH_SBC);
	STAT_SYS_ALLOC_SBC_ALLOC(allctr, crr_sz, blk_sz);

#if HAVE_ERTS_MSEG
    sbc_final_touch:
#endif

	blk = SBC2BLK(allctr, crr);

	SET_SBC_BLK_FTR(((UWord *) blk)[-1]);
	SET_BLK_HDR(blk, blk_sz, SBH_THIS_ALLOCED|SBH_PREV_FREE|SBH_LAST_BLK);

	link_carrier(&allctr->sbc_list, crr);

	CHECK_1BLK_CARRIER(allctr, 1, is_mseg, crr, crr_sz, blk, blk_sz);

    }
    else {
	SET_CARRIER_HDR(crr, crr_sz, SCH_SYS_ALLOC|SCH_MBC);
	STAT_SYS_ALLOC_MBC_ALLOC(allctr, crr_sz);

#if HAVE_ERTS_MSEG
    mbc_final_touch:
#endif

	blk = MBC2FBLK(allctr, crr);

#ifdef ERTS_ALLOC_UTIL_HARD_DEBUG
	if (allctr->mbc_header_size % sizeof(Unit_t) == 0)
	    crr_sz -= sizeof(UWord);
#endif

	blk_sz = UNIT_FLOOR(crr_sz - allctr->mbc_header_size);

	SET_MBC_BLK_FTR(((UWord *) blk)[-1]);
	SET_BLK_HDR(blk, blk_sz, SBH_THIS_FREE|SBH_PREV_FREE|SBH_LAST_BLK);

#ifdef ERTS_ALLOC_UTIL_HARD_DEBUG
	*((Carrier_t **) NXT_BLK(blk)) = crr;
#endif

	if (flags & CFLG_MAIN_CARRIER) {
	    ASSERT(!allctr->main_carrier);
	    allctr->main_carrier = crr;
	}

	link_carrier(&allctr->mbc_list, crr);

#ifdef ERTS_ALLOC_UTIL_HARD_DEBUG
	if (allctr->mbc_header_size % sizeof(Unit_t) == 0)
	    crr_sz += sizeof(UWord);
#endif
	CHECK_1BLK_CARRIER(allctr, 0, is_mseg, crr, crr_sz, blk, blk_sz);
#ifdef ERTS_ALLOC_UTIL_HARD_DEBUG
	if (allctr->mbc_header_size % sizeof(Unit_t) == 0)
	    crr_sz -= sizeof(UWord);
#endif
	if (allctr->creating_mbc)
	    (*allctr->creating_mbc)(allctr, crr, 0);

    }

    DEBUG_SAVE_ALIGNMENT(crr);
    return blk;
}

static Block_t *
resize_carrier(Allctr_t *allctr, Block_t *old_blk, Uint umem_sz, UWord flags)
{
    Block_t *new_blk;
    Carrier_t *new_crr, *old_crr;
    UWord create_flags;
    Uint old_crr_sz, old_blk_sz, new_blk_sz, new_crr_sz;
    Uint new_bcrr_sz;

    if (flags & CFLG_MBC) {
	ASSERT(0);
	return NULL;
    }

    ASSERT(flags & CFLG_SBC);
    create_flags = flags|CFLG_SBC;

    HARD_CHECK_BLK_CARRIER(allctr, old_blk);

    old_blk_sz = BLK_SZ(old_blk);
    old_crr = BLK2SBC(allctr, old_blk);
    old_crr_sz = CARRIER_SZ(old_crr);
    ASSERT(IS_SB_CARRIER(old_crr));
    ASSERT(IS_SBC_BLK(old_blk));

    new_blk_sz = UMEMSZ2BLKSZ(allctr, umem_sz);

#if HAVE_ERTS_MSEG

    if (IS_MSEG_CARRIER(old_crr)) {
	STAT_MSEG_SBC_FREE(allctr, old_crr_sz, old_blk_sz);

	if (!(flags & CFLG_FORCE_SYS_ALLOC)) {

	    new_crr_sz = new_blk_sz + allctr->sbc_header_size;
	    new_crr_sz = MSEG_UNIT_CEILING(new_crr_sz);
	    new_crr = (Carrier_t *) alcu_mseg_realloc(allctr,
						      old_crr,
						      old_crr_sz,
						      &new_crr_sz);
	    if (new_crr) {
		SET_CARRIER_SZ(new_crr, new_crr_sz);
		new_blk = SBC2BLK(allctr, new_crr);
		SET_BLK_SZ(new_blk, new_blk_sz);
		STAT_MSEG_SBC_ALLOC(allctr, new_crr_sz, new_blk_sz);
		relink_carrier(&allctr->sbc_list, new_crr);
		CHECK_1BLK_CARRIER(allctr, 1, 1, new_crr, new_crr_sz,
				   new_blk, new_blk_sz);
		DEBUG_SAVE_ALIGNMENT(new_crr);
		return new_blk;
	    }
	    create_flags |= CFLG_FORCE_SYS_ALLOC; /* since mseg_realloc()
						     failed */
	}

	new_blk = create_carrier(allctr, umem_sz, create_flags);
	if (new_blk) {
	    sys_memcpy((void *) BLK2UMEM(new_blk),
		       (void *) BLK2UMEM(old_blk),
		       MIN(new_blk_sz, old_blk_sz) - ABLK_HDR_SZ);
	    unlink_carrier(&allctr->sbc_list, old_crr);
	    alcu_mseg_dealloc(allctr, old_crr, old_crr_sz);
	}
	else {
	    /* Old carrier unchanged; restore stat */
	    STAT_MSEG_SBC_ALLOC(allctr, old_crr_sz, old_blk_sz);
	}

	return new_blk;
    }
    else {
	if (!(flags & CFLG_FORCE_MSEG)) {
#endif /* #if HAVE_ERTS_MSEG */
	    new_bcrr_sz = new_blk_sz + allctr->sbc_header_size;
	    new_crr_sz = (flags & CFLG_FORCE_SIZE
			  ? UNIT_CEILING(new_bcrr_sz)
			  : SYS_ALLOC_CARRIER_CEILING(new_bcrr_sz));

	    new_crr = (Carrier_t *) alcu_sys_realloc(allctr,
						     (void *) old_crr,
						     new_crr_sz);
	    if (new_crr) {
	    sys_realloc_success:
		SET_CARRIER_SZ(new_crr, new_crr_sz);
		new_blk = SBC2BLK(allctr, new_crr);
		SET_BLK_SZ(new_blk, new_blk_sz);
		STAT_SYS_ALLOC_SBC_FREE(allctr, old_crr_sz, old_blk_sz);
		STAT_SYS_ALLOC_SBC_ALLOC(allctr, new_crr_sz, new_blk_sz);
		relink_carrier(&allctr->sbc_list, new_crr);
		CHECK_1BLK_CARRIER(allctr, 1, 0, new_crr, new_crr_sz,
				   new_blk, new_blk_sz);
		DEBUG_SAVE_ALIGNMENT(new_crr);
		return new_blk;
	    }
	    else if (new_crr_sz > UNIT_CEILING(new_bcrr_sz)) {
		new_crr_sz = new_blk_sz + allctr->sbc_header_size;
		new_crr_sz = UNIT_CEILING(new_crr_sz);
		new_crr = (Carrier_t *) alcu_sys_realloc(allctr,
							 (void *) old_crr,
							 new_crr_sz);
		if (new_crr)
		    goto sys_realloc_success;
	    }

#if !HAVE_ERTS_MSEG
	    return NULL;
#else
	    create_flags |= CFLG_FORCE_MSEG; /* Since sys_realloc() failed */
	}

	STAT_SYS_ALLOC_SBC_FREE(allctr, old_crr_sz, old_blk_sz);

	new_blk = create_carrier(allctr, umem_sz, create_flags);
	if (new_blk) {
	    sys_memcpy((void *) BLK2UMEM(new_blk),
		       (void *) BLK2UMEM(old_blk),
		       MIN(new_blk_sz, old_blk_sz) - ABLK_HDR_SZ);
	    unlink_carrier(&allctr->sbc_list, old_crr);
	    alcu_sys_free(allctr, old_crr);
	}
	else {
	    /* Old carrier unchanged; restore... */
	    STAT_SYS_ALLOC_SBC_ALLOC(allctr, old_crr_sz, old_blk_sz);
	}
	DEBUG_SAVE_ALIGNMENT(new_crr);
	return new_blk;
    }
#endif
}

static void
destroy_carrier(Allctr_t *allctr, Block_t *blk)
{
    Uint crr_sz;
    Carrier_t *crr;
#if HAVE_ERTS_MSEG
    Uint is_mseg = 0;
#endif

    ASSERT(IS_FIRST_BLK(blk));

    if (IS_SBC_BLK(blk)) {
	Uint blk_sz = BLK_SZ(blk);
	crr = BLK2SBC(allctr, blk);
	crr_sz = CARRIER_SZ(crr);

	ASSERT(IS_LAST_BLK(blk));

	HARD_CHECK_BLK_CARRIER(allctr, blk);

#if HAVE_ERTS_MSEG
	if (IS_MSEG_CARRIER(crr)) {
	    is_mseg++;
	    ASSERT(crr_sz % mseg_unit_size == 0);
	    STAT_MSEG_SBC_FREE(allctr, crr_sz, blk_sz);
	}
	else
#endif
	    STAT_SYS_ALLOC_SBC_FREE(allctr, crr_sz, blk_sz);

	unlink_carrier(&allctr->sbc_list, crr);

    }
    else {
	crr = FBLK2MBC(allctr, blk);
	crr_sz = CARRIER_SZ(crr);

#ifdef DEBUG
	if (!allctr->stopped) {
	    ASSERT(IS_LAST_BLK(blk));

#ifdef ERTS_ALLOC_UTIL_HARD_DEBUG
	    (*allctr->link_free_block)(allctr, blk, 0);
	    HARD_CHECK_BLK_CARRIER(allctr, blk);
	    (*allctr->unlink_free_block)(allctr, blk, 0);
#endif
	}
#endif

#if HAVE_ERTS_MSEG
	if (IS_MSEG_CARRIER(crr)) {
	    is_mseg++;
	    ASSERT(crr_sz % mseg_unit_size == 0);
	    STAT_MSEG_MBC_FREE(allctr, crr_sz);
	}
	else
#endif
	    STAT_SYS_ALLOC_MBC_FREE(allctr, crr_sz);

	unlink_carrier(&allctr->mbc_list, crr);
	if (allctr->destroying_mbc)
	    (*allctr->destroying_mbc)(allctr, crr, 0);
    }


#if HAVE_ERTS_MSEG
    if (is_mseg) {
	alcu_mseg_dealloc(allctr, crr, crr_sz);
    }
    else
#endif
	alcu_sys_free(allctr, crr);
}


/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * Info stuff                                                              *
\*                                                                         */

static struct {
    Eterm versions;

    Eterm options;
    Eterm e;
    Eterm t;
    Eterm ramv;
#if HALFWORD_HEAP
    Eterm low;
#endif
    Eterm sbct;
#if HAVE_ERTS_MSEG
    Eterm asbcst;
    Eterm rsbcst;
#endif
    Eterm rsbcmt;
    Eterm rmbcmt;
    Eterm mmbcs;
    Eterm msbclt;
#if HAVE_ERTS_MSEG
    Eterm mmsbc;
    Eterm mmmbc;
#endif
    Eterm lmbcs;
    Eterm smbcs;
    Eterm mbcgs;
    Eterm sbmbcs;
    Eterm sbmbct;

#if HAVE_ERTS_MSEG
    Eterm mmc;
#endif
    Eterm ycs;

    /* Eterm sbmbcs; */

    Eterm fix_types;

    Eterm mbcs;
    Eterm sbcs;

    Eterm sys_alloc_carriers_size;
#if HAVE_ERTS_MSEG
    Eterm mseg_alloc_carriers_size;
#endif
    Eterm carriers_size;
    Eterm sys_alloc_carriers;
#if HAVE_ERTS_MSEG
    Eterm mseg_alloc_carriers;
#endif
    Eterm carriers;
    Eterm blocks_size;
    Eterm blocks;

    Eterm calls;
    Eterm sys_alloc;
    Eterm sys_free;
    Eterm sys_realloc;
#if HAVE_ERTS_MSEG
    Eterm mseg_alloc;
    Eterm mseg_dealloc;
    Eterm mseg_realloc;
#endif
    Eterm sbmbc_alloc;
    Eterm sbmbc_free;
#ifdef DEBUG
    Eterm end_of_atoms;
#endif
} am;

static Eterm fix_type_atoms[ERTS_ALC_NO_FIXED_SIZES];

static ERTS_INLINE void atom_init(Eterm *atom, char *name)
{
    *atom = am_atom_put(name, strlen(name));
}
#define AM_INIT(AM) atom_init(&am.AM, #AM)

static erts_mtx_t init_atoms_mtx;

static void
init_atoms(Allctr_t *allctr)
{

#ifdef USE_THREADS
    if (allctr && allctr->thread_safe)
	erts_mtx_unlock(&allctr->mutex);
#endif

    erts_mtx_lock(&init_atoms_mtx);

    if (!atoms_initialized) {
	int ix;
#ifdef DEBUG
	Eterm *atom;

	for (atom = (Eterm *) &am; atom <= &am.end_of_atoms; atom++) {
	    *atom = THE_NON_VALUE;
	}
#endif

	AM_INIT(versions);

	AM_INIT(options);
	AM_INIT(e);
	AM_INIT(t);
	AM_INIT(ramv);
#if HALFWORD_HEAP
	AM_INIT(low);
#endif
	AM_INIT(sbct);
#if HAVE_ERTS_MSEG
	AM_INIT(asbcst);
	AM_INIT(rsbcst);
#endif
	AM_INIT(rsbcmt);
	AM_INIT(rmbcmt);
	AM_INIT(mmbcs);
	AM_INIT(msbclt);
#if HAVE_ERTS_MSEG
	AM_INIT(mmsbc);
	AM_INIT(mmmbc);
#endif
	AM_INIT(lmbcs);
	AM_INIT(smbcs);
	AM_INIT(mbcgs);
	AM_INIT(sbmbcs);
	AM_INIT(sbmbct);

#if HAVE_ERTS_MSEG
	AM_INIT(mmc);
#endif
	AM_INIT(ycs);

	/*AM_INIT(sbmbcs);*/

	AM_INIT(fix_types);

	AM_INIT(mbcs);
	AM_INIT(sbcs);

	AM_INIT(sys_alloc_carriers_size);
#if HAVE_ERTS_MSEG
	AM_INIT(mseg_alloc_carriers_size);
#endif
	AM_INIT(carriers_size);
	AM_INIT(sys_alloc_carriers);
#if HAVE_ERTS_MSEG
	AM_INIT(mseg_alloc_carriers);
#endif
	AM_INIT(carriers);
	AM_INIT(blocks_size);
	AM_INIT(blocks);

	AM_INIT(calls);
	AM_INIT(sys_alloc);
	AM_INIT(sys_free);
	AM_INIT(sys_realloc);
#if HAVE_ERTS_MSEG
	AM_INIT(mseg_alloc);
	AM_INIT(mseg_dealloc);
	AM_INIT(mseg_realloc);
#endif
	AM_INIT(sbmbc_free);
	AM_INIT(sbmbc_alloc);

#ifdef DEBUG
	for (atom = (Eterm *) &am; atom < &am.end_of_atoms; atom++) {
	    ASSERT(*atom != THE_NON_VALUE);
	}
#endif

	for (ix = 0; ix < ERTS_ALC_NO_FIXED_SIZES; ix++) {
	    ErtsAlcType_t n = ERTS_ALC_N_MIN_A_FIXED_SIZE + ix;
	    char *name = (char *) ERTS_ALC_N2TD(n);
	    size_t len = strlen(name);
	    fix_type_atoms[ix] = am_atom_put(name, len);
	}
    }

    
    if (allctr) {

	make_name_atoms(allctr);

	(*allctr->init_atoms)();

#ifdef USE_THREADS
	if (allctr->thread_safe)
	    erts_mtx_lock(&allctr->mutex);
#endif
    	allctr->atoms_initialized = 1;
    }

    atoms_initialized = 1;
    erts_mtx_unlock(&init_atoms_mtx);

}

static ERTS_INLINE void
ensure_atoms_initialized(Allctr_t *allctr)
{
    if (!allctr || !allctr->atoms_initialized)
	init_atoms(allctr);
}

#define bld_uint	erts_bld_uint
#define bld_cons	erts_bld_cons
#define bld_tuple	erts_bld_tuple
#define bld_string	erts_bld_string

/*
 * bld_unstable_uint() (instead bld_uint()) is used when values may
 * change between size check and actual build. This because a value
 * that would fit a small when size check is done may need to be built
 * as a big when the actual build is performed. Caller is required to
 * HRelease after build.
 */
static ERTS_INLINE Eterm
bld_unstable_uint(Uint **hpp, Uint *szp, Uint ui)
{
    Eterm res = THE_NON_VALUE;
    if (szp)
	*szp += BIG_UINT_HEAP_SIZE;
    if (hpp) {
	if (IS_USMALL(0, ui))
	    res = make_small(ui);
	else {
	    res = uint_to_big(ui, *hpp);
	    *hpp += BIG_UINT_HEAP_SIZE;
	}
    }
    return res;
}

static ERTS_INLINE void
add_2tup(Uint **hpp, Uint *szp, Eterm *lp, Eterm el1, Eterm el2)
{
    *lp = bld_cons(hpp, szp, bld_tuple(hpp, szp, 2, el1, el2), *lp);
}

static ERTS_INLINE void
add_3tup(Uint **hpp, Uint *szp, Eterm *lp, Eterm el1, Eterm el2, Eterm el3)
{
    *lp = bld_cons(hpp, szp, bld_tuple(hpp, szp, 3, el1, el2, el3), *lp);
}

static ERTS_INLINE void
add_4tup(Uint **hpp, Uint *szp, Eterm *lp,
	 Eterm el1, Eterm el2, Eterm el3, Eterm el4)
{
    *lp =
	bld_cons(hpp, szp, bld_tuple(hpp, szp, 4, el1, el2, el3, el4), *lp);
}

static Eterm
sz_info_fix(Allctr_t *allctr,
	    int *print_to_p,
	    void *print_to_arg,
	    Uint **hpp,
	    Uint *szp)
{
    Eterm res;
    int ix;
    ErtsAlcFixList_t *fix = allctr->fix;

    ASSERT(fix);

    res = NIL;

    for (ix = ERTS_ALC_NO_FIXED_SIZES-1; ix >= 0; ix--) {
	ErtsAlcType_t n = ix + ERTS_ALC_N_MIN_A_FIXED_SIZE;
	Uint alloced = (fix[ix].type_size * fix[ix].allocated);
	Uint used = fix[ix].type_size*fix[ix].used;

	if (print_to_p) {
	    int to = *print_to_p;
	    void *arg = print_to_arg;
	    erts_print(to,
		       arg,
		       "fix type: %s %bpu %bpu\n",
		       (char *) ERTS_ALC_N2TD(n),
		       alloced,
		       used);
	}

	if (hpp || szp) {
	    add_3tup(hpp, szp, &res,
		     fix_type_atoms[ix],
		     bld_unstable_uint(hpp, szp, alloced),
		     bld_unstable_uint(hpp, szp, used));
	}
    }

    return res;
}

static Eterm
sz_info_carriers(Allctr_t *allctr,
		 CarriersStats_t *cs,
		 char *prefix,
		 int *print_to_p,
		 void *print_to_arg,
		 Uint **hpp,
		 Uint *szp)
{
    Eterm res = THE_NON_VALUE;
    Uint curr_size = (cs == &allctr->sbmbcs
		      ? cs->curr.small_block.size
		      : cs->curr.norm.mseg.size + cs->curr.norm.sys_alloc.size);

    if (print_to_p) {
	int to = *print_to_p;
	void *arg = print_to_arg;
	erts_print(to,
		   arg,
		   "%sblocks size: %bpu %bpu %bpu\n",
		   prefix,
		   cs->blocks.curr.size,
		   cs->blocks.max.size,
		   cs->blocks.max_ever.size);
	erts_print(to,
		   arg,
		   "%scarriers size: %beu %bpu %bpu\n",
		   prefix,
		   curr_size,
		   cs->max.size,
		   cs->max_ever.size);
    }

    if (hpp || szp) {
	res = NIL;
	add_4tup(hpp, szp, &res,
		 am.carriers_size,
		 bld_unstable_uint(hpp, szp, curr_size),
		 bld_unstable_uint(hpp, szp, cs->max.size),
		 bld_unstable_uint(hpp, szp, cs->max_ever.size));
	add_4tup(hpp, szp, &res,
		 am.blocks_size,
		 bld_unstable_uint(hpp, szp, cs->blocks.curr.size),
		 bld_unstable_uint(hpp, szp, cs->blocks.max.size),
		 bld_unstable_uint(hpp, szp, cs->blocks.max_ever.size));
    }

    return res;
}

static Eterm
info_carriers(Allctr_t *allctr,
	      CarriersStats_t *cs,
	      char *prefix,
	      int *print_to_p,
	      void *print_to_arg,
	      Uint **hpp,
	      Uint *szp)
{
    Eterm res = THE_NON_VALUE;
    Uint curr_no, curr_size;
    int small_block = cs == &allctr->sbmbcs;

    if (small_block) {
	curr_no = cs->curr.small_block.no;
	curr_size = cs->curr.small_block.size;
    }
    else {
	curr_no = cs->curr.norm.mseg.no + cs->curr.norm.sys_alloc.no;
	curr_size = cs->curr.norm.mseg.size + cs->curr.norm.sys_alloc.size;
    }

    if (print_to_p) {
	int to = *print_to_p;
	void *arg = print_to_arg;
	erts_print(to,
		   arg,
		   "%sblocks: %bpu %bpu %bpu\n",
		   prefix,
		   cs->blocks.curr.no,
		   cs->blocks.max.no,
		   cs->blocks.max_ever.no);
	erts_print(to,
		   arg,
		   "%sblocks size: %bpu %bpu %bpu\n",
		   prefix,
		   cs->blocks.curr.size,
		   cs->blocks.max.size,
		   cs->blocks.max_ever.size);
	erts_print(to,
		   arg,
		   "%scarriers: %beu %bpu %bpu\n",
		   prefix,
		   curr_no,
		   cs->max.no,
		   cs->max_ever.no);
	if (!small_block) {
#if HAVE_ERTS_MSEG
	    erts_print(to,
		       arg,
		       "%smseg carriers: %bpu\n",
		       prefix,
		       cs->curr.norm.mseg.no);
#endif
	    erts_print(to,
		       arg,
		       "%ssys_alloc carriers: %bpu\n",
		       prefix,
		       cs->curr.norm.sys_alloc.no);
	}
	erts_print(to,
		   arg,
		   "%scarriers size: %beu %bpu %bpu\n",
		   prefix,
		   curr_size,
		   cs->max.size,
		   cs->max_ever.size);
	if (!small_block) {
#if HAVE_ERTS_MSEG
	    erts_print(to,
		       arg,
		       "%smseg carriers size: %bpu\n",
		       prefix,
		       cs->curr.norm.mseg.size);
#endif
	    erts_print(to,
		       arg,
		       "%ssys_alloc carriers size: %bpu\n",
		       prefix,
		       cs->curr.norm.sys_alloc.size);
	}
    }

    if (hpp || szp) {
	res = NIL;
	if (!small_block) {
	    add_2tup(hpp, szp, &res,
		     am.sys_alloc_carriers_size,
		     bld_unstable_uint(hpp, szp, cs->curr.norm.sys_alloc.size));
#if HAVE_ERTS_MSEG
	    add_2tup(hpp, szp, &res,
		     am.mseg_alloc_carriers_size,
		     bld_unstable_uint(hpp, szp, cs->curr.norm.mseg.size));
#endif
	}
	add_4tup(hpp, szp, &res,
		 am.carriers_size,
		 bld_unstable_uint(hpp, szp, curr_size),
		 bld_unstable_uint(hpp, szp, cs->max.size),
		 bld_unstable_uint(hpp, szp, cs->max_ever.size));
	if (!small_block) {
	    add_2tup(hpp, szp, &res,
		     am.sys_alloc_carriers,
		     bld_unstable_uint(hpp, szp, cs->curr.norm.sys_alloc.no));
#if HAVE_ERTS_MSEG
	    add_2tup(hpp, szp, &res,
		     am.mseg_alloc_carriers,
		     bld_unstable_uint(hpp, szp, cs->curr.norm.mseg.no));
#endif
	}
	add_4tup(hpp, szp, &res,
		 am.carriers,
		 bld_unstable_uint(hpp, szp, curr_no),
		 bld_unstable_uint(hpp, szp, cs->max.no),
		 bld_unstable_uint(hpp, szp, cs->max_ever.no));
	add_4tup(hpp, szp, &res,
		 am.blocks_size,
		 bld_unstable_uint(hpp, szp, cs->blocks.curr.size),
		 bld_unstable_uint(hpp, szp, cs->blocks.max.size),
		 bld_unstable_uint(hpp, szp, cs->blocks.max_ever.size));
	add_4tup(hpp, szp, &res,
		 am.blocks,
		 bld_unstable_uint(hpp, szp, cs->blocks.curr.no),
		 bld_unstable_uint(hpp, szp, cs->blocks.max.no),
		 bld_unstable_uint(hpp, szp, cs->blocks.max_ever.no));
    }

    return res;
}

static void
make_name_atoms(Allctr_t *allctr)
{
    char alloc[] = "alloc";
    char realloc[] = "realloc";
    char free[] = "free";
    char buf[MAX_ATOM_LENGTH];
    size_t prefix_len = strlen(allctr->name_prefix);

    if (prefix_len > MAX_ATOM_LENGTH + sizeof(realloc) - 1)
	erl_exit(1,"Too long allocator name: %salloc\n",allctr->name_prefix);

    memcpy((void *) buf, (void *) allctr->name_prefix, prefix_len);

    memcpy((void *) &buf[prefix_len], (void *) alloc, sizeof(alloc) - 1);
    allctr->name.alloc = am_atom_put(buf, prefix_len + sizeof(alloc) - 1);

    memcpy((void *) &buf[prefix_len], (void *) realloc, sizeof(realloc) - 1);
    allctr->name.realloc = am_atom_put(buf, prefix_len + sizeof(realloc) - 1);

    memcpy((void *) &buf[prefix_len], (void *) free, sizeof(free) - 1);
    allctr->name.free = am_atom_put(buf, prefix_len + sizeof(free) - 1);

}

static Eterm
info_calls(Allctr_t *allctr,
	   int *print_to_p,
	   void *print_to_arg,
	   Uint **hpp,
	   Uint *szp)
{
    Eterm res = THE_NON_VALUE;


    if (print_to_p) {

#define PRINT_CC_4(TO, TOA, NAME, CC)					\
    if ((CC).giga_no == 0)						\
	erts_print(TO, TOA, "%s calls: %b32u\n", NAME, CC.no);		\
    else								\
	erts_print(TO, TOA, "%s calls: %b32u%09lu\n", NAME, CC.giga_no, CC.no)

#define PRINT_CC_5(TO, TOA, PRFX, NAME, CC)				\
    if ((CC).giga_no == 0)						\
	erts_print(TO, TOA, "%s%s calls: %b32u\n",PRFX,NAME,CC.no);	\
    else								\
	erts_print(TO, TOA, "%s%s calls: %b32u%09lu\n",PRFX,NAME,CC.giga_no,CC.no)

	char *prefix = allctr->name_prefix;
	int to = *print_to_p;
	void *arg = print_to_arg;

	PRINT_CC_5(to, arg, prefix, "alloc",        allctr->calls.this_alloc);
	PRINT_CC_5(to, arg, prefix, "free",         allctr->calls.this_free);
	PRINT_CC_5(to, arg, prefix, "realloc",      allctr->calls.this_realloc);

	PRINT_CC_4(to, arg,         "sbmbc_alloc",  allctr->calls.sbmbc_alloc);
	PRINT_CC_4(to, arg,         "sbmbc_free",   allctr->calls.sbmbc_free);

#if HAVE_ERTS_MSEG
	PRINT_CC_4(to, arg,         "mseg_alloc",   allctr->calls.mseg_alloc);
	PRINT_CC_4(to, arg,         "mseg_dealloc", allctr->calls.mseg_dealloc);
	PRINT_CC_4(to, arg,         "mseg_realloc", allctr->calls.mseg_realloc);
#endif

	PRINT_CC_4(to, arg,         "sys_alloc",    allctr->calls.sys_alloc);
	PRINT_CC_4(to, arg,         "sys_free",     allctr->calls.sys_free);
	PRINT_CC_4(to, arg,         "sys_realloc",  allctr->calls.sys_realloc);

#undef PRINT_CC_4
#undef PRINT_CC_5

    }


    if (hpp || szp) {

	ASSERT(allctr->name.alloc   != THE_NON_VALUE);
	ASSERT(allctr->name.realloc != THE_NON_VALUE);
	ASSERT(allctr->name.free    != THE_NON_VALUE);

	res = NIL;

	add_3tup(hpp, szp, &res,
		 am.sys_realloc,
		 bld_unstable_uint(hpp, szp, allctr->calls.sys_realloc.giga_no),
		 bld_unstable_uint(hpp, szp, allctr->calls.sys_realloc.no));
	add_3tup(hpp, szp, &res,
		 am.sys_free,
		 bld_unstable_uint(hpp, szp, allctr->calls.sys_free.giga_no),
		 bld_unstable_uint(hpp, szp, allctr->calls.sys_free.no));
	add_3tup(hpp, szp, &res,
		 am.sys_alloc,
		 bld_unstable_uint(hpp, szp, allctr->calls.sys_alloc.giga_no),
		 bld_unstable_uint(hpp, szp, allctr->calls.sys_alloc.no));
#if HAVE_ERTS_MSEG
	add_3tup(hpp, szp, &res,
		 am.mseg_realloc,
		 bld_unstable_uint(hpp, szp, allctr->calls.mseg_realloc.giga_no),
		 bld_unstable_uint(hpp, szp, allctr->calls.mseg_realloc.no));
	add_3tup(hpp, szp, &res,
		 am.mseg_dealloc,
		 bld_unstable_uint(hpp, szp, allctr->calls.mseg_dealloc.giga_no),
		 bld_unstable_uint(hpp, szp, allctr->calls.mseg_dealloc.no));
	add_3tup(hpp, szp, &res,
		 am.mseg_alloc,
		 bld_unstable_uint(hpp, szp, allctr->calls.mseg_alloc.giga_no),
		 bld_unstable_uint(hpp, szp, allctr->calls.mseg_alloc.no));
#endif
	add_3tup(hpp, szp, &res,
		 am.sbmbc_free,
		 bld_unstable_uint(hpp, szp, allctr->calls.sbmbc_free.giga_no),
		 bld_unstable_uint(hpp, szp, allctr->calls.sbmbc_free.no));
	add_3tup(hpp, szp, &res,
		 am.sbmbc_alloc,
		 bld_unstable_uint(hpp, szp, allctr->calls.sbmbc_alloc.giga_no),
		 bld_unstable_uint(hpp, szp, allctr->calls.sbmbc_alloc.no));
	add_3tup(hpp, szp, &res,
		 allctr->name.realloc,
		 bld_unstable_uint(hpp, szp, allctr->calls.this_realloc.giga_no),
		 bld_unstable_uint(hpp, szp, allctr->calls.this_realloc.no));
	add_3tup(hpp, szp, &res,
		 allctr->name.free,
		 bld_unstable_uint(hpp, szp, allctr->calls.this_free.giga_no),
		 bld_unstable_uint(hpp, szp, allctr->calls.this_free.no));
	add_3tup(hpp, szp, &res,
		 allctr->name.alloc,
		 bld_unstable_uint(hpp, szp, allctr->calls.this_alloc.giga_no),
		 bld_unstable_uint(hpp, szp, allctr->calls.this_alloc.no));
    }

    return res;
}

static Eterm
info_options(Allctr_t *allctr,
             int *print_to_p,
	     void *print_to_arg,
	     Uint **hpp,
	     Uint *szp)
{
    Eterm res = THE_NON_VALUE;

    if (!allctr) {
	if (print_to_p)
	    erts_print(*print_to_p, print_to_arg, "option e: false\n");
	if (hpp || szp) {
	    res = NIL;
	    add_2tup(hpp, szp, &res, am.e, am_false);
	}
	return res;
    }

    if (print_to_p) {
	char topt[21]; /* Enough for any 64-bit integer */
	if (allctr->t)
	    erts_snprintf(&topt[0], sizeof(topt), "%d", allctr->t);
	else
	    erts_snprintf(&topt[0], sizeof(topt), "false");
	erts_print(*print_to_p,
		   print_to_arg,
		   "option e: true\n"
		   "option t: %s\n"
		   "option ramv: %s\n"
#if HALFWORD_HEAP
		   "option low: %s\n"
#endif
		   "option sbct: %beu\n"
#if HAVE_ERTS_MSEG
		   "option asbcst: %bpu\n"
		   "option rsbcst: %bpu\n"
#endif
		   "option rsbcmt: %beu\n"
		   "option rmbcmt: %beu\n"
		   "option mmbcs: %beu\n"
#if HAVE_ERTS_MSEG
		   "option mmsbc: %beu\n"
		   "option mmmbc: %beu\n"
#endif
		   "option lmbcs: %beu\n"
		   "option smbcs: %beu\n"
		   "option mbcgs: %beu\n"
		   "option sbmbcs: %beu\n"
		   "option sbmbct: %beu\n",
		   topt,
		   allctr->ramv ? "true" : "false",
#if HALFWORD_HEAP
		   allctr->mseg_opt.low_mem ? "true" : "false",
#endif
		   allctr->sbc_threshold,
#if HAVE_ERTS_MSEG
		   allctr->mseg_opt.abs_shrink_th,
		   allctr->mseg_opt.rel_shrink_th,
#endif
		   allctr->sbc_move_threshold,
		   allctr->mbc_move_threshold,
		   allctr->main_carrier_size,
#if HAVE_ERTS_MSEG
		   allctr->max_mseg_sbcs,
		   allctr->max_mseg_mbcs,
#endif
		   allctr->largest_mbc_size,
		   allctr->smallest_mbc_size,
		   allctr->mbc_growth_stages,
		   allctr->sbmbc_size,
		   allctr->sbmbc_threshold);
    }

    res = (*allctr->info_options)(allctr, "option ", print_to_p, print_to_arg,
				  hpp, szp);

    if (hpp || szp) {
	add_2tup(hpp, szp, &res,
		 am.sbmbct,
		 bld_uint(hpp, szp, allctr->sbmbc_threshold));
	add_2tup(hpp, szp, &res,
		 am.sbmbcs,
		 bld_uint(hpp, szp, allctr->sbmbc_size));
	add_2tup(hpp, szp, &res,
		 am.mbcgs,
		 bld_uint(hpp, szp, allctr->mbc_growth_stages));
	add_2tup(hpp, szp, &res,
		 am.smbcs,
		 bld_uint(hpp, szp, allctr->smallest_mbc_size));
	add_2tup(hpp, szp, &res,
		 am.lmbcs,
		 bld_uint(hpp, szp, allctr->largest_mbc_size));
#if HAVE_ERTS_MSEG
	add_2tup(hpp, szp, &res,
		 am.mmsbc,
		 bld_uint(hpp, szp, allctr->max_mseg_sbcs));
	add_2tup(hpp, szp, &res,
		 am.mmmbc,
		 bld_uint(hpp, szp, allctr->max_mseg_mbcs));
#endif
	add_2tup(hpp, szp, &res,
		 am.mmbcs,
		 bld_uint(hpp, szp, allctr->main_carrier_size));
	add_2tup(hpp, szp, &res,
		 am.rmbcmt,
		 bld_uint(hpp, szp, allctr->mbc_move_threshold));
	add_2tup(hpp, szp, &res,
		 am.rsbcmt,
		 bld_uint(hpp, szp, allctr->sbc_move_threshold));
#if HAVE_ERTS_MSEG
	add_2tup(hpp, szp, &res,
		 am.rsbcst,
		 bld_uint(hpp, szp, allctr->mseg_opt.rel_shrink_th));
	add_2tup(hpp, szp, &res,
		 am.asbcst,
		 bld_uint(hpp, szp, allctr->mseg_opt.abs_shrink_th));
#endif
	add_2tup(hpp, szp, &res,
		 am.sbct,
		 bld_uint(hpp, szp, allctr->sbc_threshold));
#if HALFWORD_HEAP
	add_2tup(hpp, szp, &res, am.low, allctr->mseg_opt.low_mem ? am_true : am_false);
#endif
	add_2tup(hpp, szp, &res, am.ramv, allctr->ramv ? am_true : am_false);
	add_2tup(hpp, szp, &res, am.t, (allctr->t ? am_true : am_false));
	add_2tup(hpp, szp, &res, am.e, am_true);
    }

    return res;
}


static ERTS_INLINE void
update_max_ever_values(CarriersStats_t *cs)
{
    if (cs->max_ever.no < cs->max.no)
	cs->max_ever.no = cs->max.no;
    if (cs->max_ever.size < cs->max.size)
	cs->max_ever.size = cs->max.size;
    if (cs->blocks.max_ever.no < cs->blocks.max.no)
	cs->blocks.max_ever.no = cs->blocks.max.no;
    if (cs->blocks.max_ever.size < cs->blocks.max.size)
	cs->blocks.max_ever.size = cs->blocks.max.size;
}

static ERTS_INLINE void
reset_max_values(CarriersStats_t *cs)
{
    cs->max.no = cs->curr.norm.mseg.no + cs->curr.norm.sys_alloc.no;
    cs->max.size = cs->curr.norm.mseg.size + cs->curr.norm.sys_alloc.size;
    cs->blocks.max.no = cs->blocks.curr.no;
    cs->blocks.max.size = cs->blocks.curr.size;
}


/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * Exported functions                                                      *
\*                                                                         */

Eterm
erts_alcu_au_info_options(int *print_to_p, void *print_to_arg,
			  Uint **hpp, Uint *szp)
{
    Eterm res = THE_NON_VALUE;    

    if (print_to_p) {

	erts_print(*print_to_p,
		   print_to_arg,
#if HAVE_ERTS_MSEG
		   "option mmc: %beu\n"
#endif
		   "option ycs: %beu\n",
#if HAVE_ERTS_MSEG
		   max_mseg_carriers,
#endif
		   sys_alloc_carrier_size);
    }

    if (hpp || szp) {
	res = NIL;
	ensure_atoms_initialized(NULL);
	add_2tup(hpp, szp, &res,
		 am.ycs,
		 bld_uint(hpp, szp, sys_alloc_carrier_size));
#if HAVE_ERTS_MSEG
	add_2tup(hpp, szp, &res,
		 am.mmc,
		 bld_uint(hpp, szp, max_mseg_carriers));
#endif
    }

    return res;
}


Eterm
erts_alcu_info_options(Allctr_t *allctr,
		       int *print_to_p,
		       void *print_to_arg,
		       Uint **hpp,
		       Uint *szp)
{
    Eterm res;


#ifdef USE_THREADS
    if (allctr->thread_safe)
	erts_mtx_lock(&allctr->mutex);
#endif
    if (hpp || szp)
	ensure_atoms_initialized(allctr);
    res = info_options(allctr, print_to_p, print_to_arg, hpp, szp);
#ifdef USE_THREADS
    if (allctr->thread_safe)
	erts_mtx_unlock(&allctr->mutex);
#endif
    return res;
}

/* ----------------------------------------------------------------------- */

Eterm
erts_alcu_sz_info(Allctr_t *allctr,
		  int begin_max_period,
		  int *print_to_p,
		  void *print_to_arg,
		  Uint **hpp,
		  Uint *szp)
{
    Eterm res, sbmbcs, mbcs, sbcs, fix = THE_NON_VALUE;

    res  = THE_NON_VALUE;

    if (!allctr) {
	if (print_to_p)
	    erts_print(*print_to_p, print_to_arg, "false\n");
	if (szp)
	    *szp = 0;
	return am_false;
    }

#ifdef USE_THREADS
    if (allctr->thread_safe)
	erts_mtx_lock(&allctr->mutex);
#endif

    ERTS_ALCU_DBG_CHK_THR_ACCESS(allctr);

    if (hpp || szp)
	ensure_atoms_initialized(allctr);

    /* Update sbc values not continously updated */
    allctr->sbcs.blocks.curr.no
	= allctr->sbcs.curr.norm.mseg.no + allctr->sbcs.curr.norm.sys_alloc.no;
    allctr->sbcs.blocks.max.no = allctr->sbcs.max.no;

    update_max_ever_values(&allctr->sbmbcs);
    update_max_ever_values(&allctr->mbcs);
    update_max_ever_values(&allctr->sbcs);

    if (allctr->fix)
	fix = sz_info_fix(allctr, print_to_p, print_to_arg, hpp, szp);
    sbmbcs = sz_info_carriers(allctr, &allctr->sbmbcs, "sbmbcs ", print_to_p,
			      print_to_arg, hpp, szp);
    mbcs = sz_info_carriers(allctr, &allctr->mbcs, "mbcs ", print_to_p,
			    print_to_arg, hpp, szp);
    sbcs = sz_info_carriers(allctr, &allctr->sbcs, "sbcs ", print_to_p,
			    print_to_arg, hpp, szp);

    if (hpp || szp) {
	res = NIL;
	add_2tup(hpp, szp, &res, am.sbcs, sbcs);
	add_2tup(hpp, szp, &res, am.mbcs, mbcs);
	add_2tup(hpp, szp, &res, am.sbmbcs, sbmbcs);
	if (allctr->fix)
	    add_2tup(hpp, szp, &res, am.fix_types, fix);
    }

    if (begin_max_period) {
	reset_max_values(&allctr->sbmbcs);
	reset_max_values(&allctr->mbcs);
	reset_max_values(&allctr->sbcs);
    }


#ifdef USE_THREADS
    if (allctr->thread_safe)
	erts_mtx_unlock(&allctr->mutex);
#endif

    return res;
}

Eterm
erts_alcu_info(Allctr_t *allctr,
	       int begin_max_period,
	       int *print_to_p,
	       void *print_to_arg,
	       Uint **hpp,
	       Uint *szp)
{
    Eterm res, sett, sbmbcs, mbcs, sbcs, calls, fix = THE_NON_VALUE;

    res  = THE_NON_VALUE;

    if (!allctr) {
	if (print_to_p)
	    erts_print(*print_to_p, print_to_arg, "false\n");
	if (szp)
	    *szp = 0;
	return am_false;
    }

#ifdef USE_THREADS
    if (allctr->thread_safe)
	erts_mtx_lock(&allctr->mutex);
#endif

    ERTS_ALCU_DBG_CHK_THR_ACCESS(allctr);

    if (hpp || szp)
	ensure_atoms_initialized(allctr);

    /* Update sbc values not continously updated */
    allctr->sbcs.blocks.curr.no
	= allctr->sbcs.curr.norm.mseg.no + allctr->sbcs.curr.norm.sys_alloc.no;
    allctr->sbcs.blocks.max.no = allctr->sbcs.max.no;

    update_max_ever_values(&allctr->sbmbcs);
    update_max_ever_values(&allctr->mbcs);
    update_max_ever_values(&allctr->sbcs);

    if (print_to_p) {
	erts_print(*print_to_p,
		   print_to_arg,
		   "versions: %s %s\n",
		   allctr->vsn_str,
		   ERTS_ALCU_VSN_STR);
    }

    sett = info_options(allctr, print_to_p, print_to_arg, hpp, szp);
    if (allctr->fix)
	fix = sz_info_fix(allctr, print_to_p, print_to_arg, hpp, szp);
    sbmbcs = info_carriers(allctr, &allctr->sbmbcs, "sbmbcs ", print_to_p,
			   print_to_arg, hpp, szp);
    mbcs = info_carriers(allctr, &allctr->mbcs, "mbcs ", print_to_p,
			 print_to_arg, hpp, szp);
    sbcs = info_carriers(allctr, &allctr->sbcs, "sbcs ", print_to_p,
			 print_to_arg, hpp, szp);
    calls = info_calls(allctr, print_to_p, print_to_arg, hpp, szp);

    if (hpp || szp) {
	res = NIL;

	add_2tup(hpp, szp, &res, am.calls, calls);
	add_2tup(hpp, szp, &res, am.sbcs, sbcs);
	add_2tup(hpp, szp, &res, am.mbcs, mbcs);
	add_2tup(hpp, szp, &res, am.sbmbcs, sbmbcs);
	if (allctr->fix)
	    add_2tup(hpp, szp, &res, am.fix_types, fix);
	add_2tup(hpp, szp, &res, am.options, sett);
	add_3tup(hpp, szp, &res,
		 am.versions,
		 bld_string(hpp, szp, allctr->vsn_str),
		 bld_string(hpp, szp, ERTS_ALCU_VSN_STR));;
    }

    if (begin_max_period) {
	reset_max_values(&allctr->sbmbcs);
	reset_max_values(&allctr->mbcs);
	reset_max_values(&allctr->sbcs);
    }


#ifdef USE_THREADS
    if (allctr->thread_safe)
	erts_mtx_unlock(&allctr->mutex);
#endif

    return res;
}


void
erts_alcu_current_size(Allctr_t *allctr, AllctrSize_t *size, ErtsAlcUFixInfo_t *fi, int fisz)
{

#ifdef USE_THREADS
    if (allctr->thread_safe)
	erts_mtx_lock(&allctr->mutex);
#endif

    size->carriers = allctr->mbcs.curr.norm.mseg.size;
    size->carriers += allctr->mbcs.curr.norm.sys_alloc.size;
    size->carriers += allctr->sbmbcs.curr.small_block.size;
    size->carriers += allctr->sbcs.curr.norm.mseg.size;
    size->carriers += allctr->sbcs.curr.norm.sys_alloc.size;

    size->blocks = allctr->mbcs.blocks.curr.size;
    size->blocks += allctr->sbmbcs.blocks.curr.size;
    size->blocks += allctr->sbcs.blocks.curr.size;

    if (fi) {
	int ix;
	for (ix = 0; ix < fisz; ix++) {
	    if (allctr->fix) {
		fi[ix].allocated += (allctr->fix[ix].type_size
				     * allctr->fix[ix].allocated);
		fi[ix].used += (allctr->fix[ix].type_size
				* allctr->fix[ix].used);
	    }
	}
    }

#ifdef USE_THREADS
    if (allctr->thread_safe)
	erts_mtx_unlock(&allctr->mutex);
#endif
}

/* ----------------------------------------------------------------------- */

static ERTS_INLINE void *
do_erts_alcu_alloc(ErtsAlcType_t type, void *extra, Uint size)
{
    Allctr_t *allctr = (Allctr_t *) extra; 
    void *res;
    ErtsAlcFixList_t *fix;

    ASSERT(initialized);

    ASSERT(allctr);

    ERTS_SMP_LC_ASSERT(!allctr->thread_safe
		       || erts_lc_mtx_is_locked(&allctr->mutex));

    ERTS_ALCU_DBG_CHK_THR_ACCESS(allctr);

#if ALLOC_ZERO_EQ_NULL
    if (!size)
	return NULL;
#endif

    INC_CC(allctr->calls.this_alloc);

    fix = allctr->fix;
    if (fix) {
	int ix = type - ERTS_ALC_N_MIN_A_FIXED_SIZE;
	ERTS_DBG_CHK_FIX_LIST(allctr, fix, ix, 1);
	fix[ix].used++;
	res = fix[ix].list;
	if (res) {
	    fix[ix].list_size--;
	    fix[ix].list = *((void **) res);
	    if (fix[ix].list && fix[ix].allocated > fix[ix].limit) {
		void *p = fix[ix].list;
		Block_t *blk;
		fix[ix].list = *((void **) p);
		fix[ix].list_size--;
		blk = UMEM2BLK(p);
		if (IS_SBC_BLK(blk))
		    destroy_carrier(allctr, blk);
		else
		    mbc_free(allctr, p);
		fix[ix].allocated--;
	    }
	    ERTS_DBG_CHK_FIX_LIST(allctr, fix, ix, 0);
	    return res;
	}
	if (size < 2*sizeof(UWord))
	    size += sizeof(UWord);
	if (fix[ix].limit < fix[ix].used)
	    fix[ix].limit = fix[ix].used;
	if (fix[ix].max_used < fix[ix].used)
	    fix[ix].max_used = fix[ix].used;
	fix[ix].allocated++;
    }

    if (size >= allctr->sbc_threshold) {
	Block_t *blk;
#ifdef ERTS_SMP
	if (allctr->dd.use)
	    ERTS_ALCU_HANDLE_DD_IN_OP(allctr, 1);
#endif
#if HALFWORD_HEAP
	blk = create_carrier(allctr, size,
			     CFLG_SBC | CFLG_FORCE_MSEG);
#else
	blk = create_carrier(allctr, size, CFLG_SBC);
#endif
	res = blk ? BLK2UMEM(blk) : NULL;
    }
    else
	res = mbc_alloc(allctr, size);

    if (!res && fix) {
	int ix = type - ERTS_ALC_N_MIN_A_FIXED_SIZE;
	fix[ix].allocated--;
	fix[ix].used--;
    }
    return res;
}

void *erts_alcu_alloc(ErtsAlcType_t type, void *extra, Uint size)
{
    void *res;
    res = do_erts_alcu_alloc(type, extra, size);
    DEBUG_CHECK_ALIGNMENT(res);
    return res;
}


#ifdef USE_THREADS

void *
erts_alcu_alloc_ts(ErtsAlcType_t type, void *extra, Uint size)
{
    Allctr_t *allctr = (Allctr_t *) extra;
    void *res;
    erts_mtx_lock(&allctr->mutex);
    res = do_erts_alcu_alloc(type, extra, size);

    DEBUG_CHECK_ALIGNMENT(res);

    erts_mtx_unlock(&allctr->mutex);
    return res;
}

#ifdef ERTS_SMP

void *
erts_alcu_alloc_thr_spec(ErtsAlcType_t type, void *extra, Uint size)
{
    ErtsAllocatorThrSpec_t *tspec = (ErtsAllocatorThrSpec_t *) extra;
    int ix;
    Allctr_t *allctr;
    void *res;

    ix = ERTS_ALC_GET_THR_IX();

    ASSERT(0 <= ix && ix < tspec->size);

    allctr = tspec->allctr[ix];

    if (allctr->thread_safe)
	erts_mtx_lock(&allctr->mutex);

    res = do_erts_alcu_alloc(type, allctr, size);

    if (allctr->thread_safe)
	erts_mtx_unlock(&allctr->mutex);

    DEBUG_CHECK_ALIGNMENT(res);

    return res;
}

void *
erts_alcu_alloc_thr_pref(ErtsAlcType_t type, void *extra, Uint size)
{
    int pref_ix;
    Allctr_t *pref_allctr;
    void *res;

    pref_ix = get_pref_allctr(extra, &pref_allctr);

    if (pref_allctr->thread_safe)
	erts_mtx_lock(&pref_allctr->mutex);

    ERTS_ALCU_DBG_CHK_THR_ACCESS(pref_allctr);

    res = do_erts_alcu_alloc(type, pref_allctr, size + sizeof(UWord));
    if (pref_allctr->thread_safe)
	erts_mtx_unlock(&pref_allctr->mutex);

    if (res)
	res = put_used_allctr(res, pref_ix, size);

    DEBUG_CHECK_ALIGNMENT(res);


    return res;
}

#endif

#endif

/* ------------------------------------------------------------------------- */

static ERTS_INLINE void
do_erts_alcu_free(ErtsAlcType_t type, void *extra, void *p)
{
    int ix;
    Allctr_t *allctr = (Allctr_t *) extra; 
    ASSERT(initialized);

    ASSERT(allctr);

    ERTS_SMP_LC_ASSERT(!allctr->thread_safe
		       || erts_lc_mtx_is_locked(&allctr->mutex));

    ERTS_ALCU_DBG_CHK_THR_ACCESS(allctr);

    if (p) {
	ErtsAlcFixList_t *fix = allctr->fix;
	Block_t *blk;

	INC_CC(allctr->calls.this_free);

	if (fix) {
	    ix = type - ERTS_ALC_N_MIN_A_FIXED_SIZE;
	    ERTS_DBG_CHK_FIX_LIST(allctr, fix, ix, 1);
	    fix[ix].used--;
	    if (fix[ix].allocated < fix[ix].limit
		&& fix[ix].list_size < ERTS_ALCU_FIX_MAX_LIST_SZ) {
		*((void **) p) = fix[ix].list;
		fix[ix].list = p;
		fix[ix].list_size++;
		if (!allctr->fix_shrink_scheduled) {
		    allctr->fix_shrink_scheduled = 1;
		    erts_set_aux_work_timeout(
			allctr->ix,
			(ERTS_SSI_AUX_WORK_FIX_ALLOC_LOWER_LIM
			 | ERTS_SSI_AUX_WORK_FIX_ALLOC_DEALLOC),
			1);
		}
		ERTS_DBG_CHK_FIX_LIST(allctr, fix, ix, 0);
		return;
	    }
	    fix[ix].allocated--;
	    if (fix[ix].list && fix[ix].allocated > fix[ix].limit) {
		blk = UMEM2BLK(p);
		if (IS_SBC_BLK(blk))
		    destroy_carrier(allctr, blk);
		else
		    mbc_free(allctr, p);
		p = fix[ix].list;
		fix[ix].list = *((void **) p);
		fix[ix].list_size--;
		fix[ix].allocated--;
	    }
	}

	blk = UMEM2BLK(p);
	if (IS_SBC_BLK(blk))
	    destroy_carrier(allctr, blk);
	else
	    mbc_free(allctr, p);
	ERTS_DBG_CHK_FIX_LIST(allctr, fix, ix, 0);
    }
}

void erts_alcu_free(ErtsAlcType_t type, void *extra, void *p)
{
    do_erts_alcu_free(type, extra, p);
}

#ifdef USE_THREADS

void
erts_alcu_free_ts(ErtsAlcType_t type, void *extra, void *p)
{
    Allctr_t *allctr = (Allctr_t *) extra;
    erts_mtx_lock(&allctr->mutex);
    do_erts_alcu_free(type, extra, p);
    erts_mtx_unlock(&allctr->mutex);
}

#ifdef ERTS_SMP

void
erts_alcu_free_thr_spec(ErtsAlcType_t type, void *extra, void *p)
{
    ErtsAllocatorThrSpec_t *tspec = (ErtsAllocatorThrSpec_t *) extra;
    int ix;
    Allctr_t *allctr;

    ix = ERTS_ALC_GET_THR_IX();

    ASSERT(0 <= ix && ix < tspec->size);

    allctr = tspec->allctr[ix];

    if (allctr->thread_safe)
	erts_mtx_lock(&allctr->mutex);

    do_erts_alcu_free(type, allctr, p);

    if (allctr->thread_safe)
	erts_mtx_unlock(&allctr->mutex);
}

void
erts_alcu_free_thr_pref(ErtsAlcType_t type, void *extra, void *p)
{
    if (p) {
	Allctr_t *pref_allctr, *used_allctr;
	void *ptr;

	get_pref_allctr(extra, &pref_allctr);
	ptr = get_used_allctr(extra, p, &used_allctr, NULL);
	if (pref_allctr != used_allctr)
	    enqueue_dealloc_other_instance(type, used_allctr, ptr);
	else {
	    if (used_allctr->thread_safe)
		erts_mtx_lock(&used_allctr->mutex);
	    ERTS_ALCU_DBG_CHK_THR_ACCESS(used_allctr);
	    do_erts_alcu_free(type, used_allctr, ptr);
	    if (used_allctr->thread_safe)
		erts_mtx_unlock(&used_allctr->mutex);
	}
    }
}

#endif

#endif

/* ------------------------------------------------------------------------- */

static ERTS_INLINE void *
do_erts_alcu_realloc(ErtsAlcType_t type,
		     void *extra,
		     void *p,
		     Uint size,
		     Uint32 alcu_flgs)
{
    Allctr_t *allctr = (Allctr_t *) extra; 
    Block_t *blk;
    void *res;

    ASSERT(initialized);

    ASSERT(allctr);

    ERTS_SMP_LC_ASSERT(!allctr->thread_safe
		       || erts_lc_mtx_is_locked(&allctr->mutex));

    ERTS_ALCU_DBG_CHK_THR_ACCESS(allctr);

    if (!p) {
	res = do_erts_alcu_alloc(type, extra, size);
	INC_CC(allctr->calls.this_realloc);
	DEC_CC(allctr->calls.this_alloc);
	return res;
    }

#if ALLOC_ZERO_EQ_NULL
    if (!size) {
	ASSERT(p);
	do_erts_alcu_free(type, extra, p);
	INC_CC(allctr->calls.this_realloc);
	DEC_CC(allctr->calls.this_free);
	return NULL;
    }
#endif

    INC_CC(allctr->calls.this_realloc);
    
    blk = UMEM2BLK(p);

    if (allctr->sbmbc_threshold > 0) {
	Uint old_sz, new_sz, lim;
	lim = allctr->sbmbc_threshold;
	old_sz = BLK_SZ(blk);
	new_sz = UMEMSZ2BLKSZ(allctr, size);
	if ((old_sz < lim && lim <= new_sz)
	    || (new_sz < lim && lim <= old_sz)) {
	    /* *Need* to move it... */

	    INC_CC(allctr->calls.this_realloc);
	    res = do_erts_alcu_alloc(type, extra, size);
	    DEC_CC(allctr->calls.this_alloc);

	    sys_memcpy(res, p, MIN(size, old_sz - ABLK_HDR_SZ));

	    do_erts_alcu_free(type, extra, p);
	    DEC_CC(allctr->calls.this_free);
	    return res;
	}
	if (old_sz < lim)
	    alcu_flgs |= ERTS_ALCU_FLG_SBMBC;
    }

    if (size < allctr->sbc_threshold) {
	if (IS_MBC_BLK(blk))
	    res = mbc_realloc(allctr, p, size, alcu_flgs);
	else {
	    Uint used_sz = allctr->sbc_header_size + ABLK_HDR_SZ + size;
	    Uint crr_sz;
	    Uint diff_sz_val;
	    Uint crr_sz_val;

#if HAVE_ERTS_MSEG
	    if (IS_SYS_ALLOC_CARRIER(BLK2SBC(allctr, blk)))
#endif
		crr_sz = SYS_ALLOC_CARRIER_CEILING(used_sz);
#if HAVE_ERTS_MSEG
	    else
		crr_sz = MSEG_UNIT_CEILING(used_sz);
#endif
	    diff_sz_val = crr_sz - used_sz;
	    if (diff_sz_val < (~((Uint) 0) / 100))
		crr_sz_val = crr_sz;
	    else {
		/* div both by 128 */
		crr_sz_val = crr_sz >> 7;
		/* A sys_alloc carrier could potentially be
		   smaller than 128 bytes (but not likely) */
		if (crr_sz_val == 0)
		    goto do_carrier_resize;
		diff_sz_val >>= 7;
	    }
		
	    if (100*diff_sz_val < allctr->sbc_move_threshold*crr_sz_val)
		/* Data won't be copied into a new carrier... */
		goto do_carrier_resize;
	    else if (alcu_flgs & ERTS_ALCU_FLG_FAIL_REALLOC_MOVE)
		return NULL;

	    res = mbc_alloc(allctr, size);
	    if (res) {
		sys_memcpy((void*) res,
			   (void*) p,
			   MIN(BLK_SZ(blk) - ABLK_HDR_SZ, size));
		destroy_carrier(allctr, blk);
	    }
	}
    }
    else {
	Block_t *new_blk;
#ifdef ERTS_SMP
	if (allctr->dd.use)
	    ERTS_ALCU_HANDLE_DD_IN_OP(allctr, 1);
#endif
	if(IS_SBC_BLK(blk)) {
	do_carrier_resize:
#if HALFWORD_HEAP
	    new_blk = resize_carrier(allctr, blk, size, CFLG_SBC | CFLG_FORCE_MSEG);
#else
	    new_blk = resize_carrier(allctr, blk, size, CFLG_SBC);
#endif
	    res = new_blk ? BLK2UMEM(new_blk) : NULL;
	}
	else if (alcu_flgs & ERTS_ALCU_FLG_FAIL_REALLOC_MOVE)
	    return NULL;
	else {
#if HALFWORD_HEAP
	    new_blk = create_carrier(allctr, size, CFLG_SBC | CFLG_FORCE_MSEG);
#else
	    new_blk = create_carrier(allctr, size, CFLG_SBC);
#endif
	    if (new_blk) {
		res = BLK2UMEM(new_blk);
		sys_memcpy((void *) res,
			   (void *) p,
			   MIN(BLK_SZ(blk) - ABLK_HDR_SZ, size));
		mbc_free(allctr, p);
	    }
	    else
		res = NULL;
	}
    }

    return res;
}

void *
erts_alcu_realloc(ErtsAlcType_t type, void *extra, void *p, Uint size)
{
    void *res;
    res = do_erts_alcu_realloc(type, extra, p, size, 0);
    DEBUG_CHECK_ALIGNMENT(res);
    return res;
}

void *
erts_alcu_realloc_mv(ErtsAlcType_t type, void *extra, void *p, Uint size)
{
    void *res;
    res = do_erts_alcu_alloc(type, extra, size);
    if (!res)
	res = erts_alcu_realloc(type, extra, p, size);
    else {
	Block_t *blk;
	size_t cpy_size;

	blk = UMEM2BLK(p);
	cpy_size = BLK_SZ(blk) - ABLK_HDR_SZ;
	if (cpy_size > size)
	    cpy_size = size;
	sys_memcpy(res, p, cpy_size);
	do_erts_alcu_free(type, extra, p);
    }
    DEBUG_CHECK_ALIGNMENT(res);
    return res;
}


#ifdef USE_THREADS

void *
erts_alcu_realloc_ts(ErtsAlcType_t type, void *extra, void *ptr, Uint size)
{
    Allctr_t *allctr = (Allctr_t *) extra;
    void *res;
    erts_mtx_lock(&allctr->mutex);
    res = do_erts_alcu_realloc(type, extra, ptr, size, 0);
    erts_mtx_unlock(&allctr->mutex);
    DEBUG_CHECK_ALIGNMENT(res);
    return res;
}

void *
erts_alcu_realloc_mv_ts(ErtsAlcType_t type, void *extra, void *p, Uint size)
{
    Allctr_t *allctr = (Allctr_t *) extra;
    void *res;
    erts_mtx_lock(&allctr->mutex);
    res = do_erts_alcu_alloc(type, extra, size);
    if (!res)
	res = erts_alcu_realloc_ts(type, extra, p, size);
    else {
	Block_t *blk;
	size_t cpy_size;

	blk = UMEM2BLK(p);
	cpy_size = BLK_SZ(blk) - ABLK_HDR_SZ;
	if (cpy_size > size)
	    cpy_size = size;
	sys_memcpy(res, p, cpy_size);
	do_erts_alcu_free(type, extra, p);
    }
    erts_mtx_unlock(&allctr->mutex);
    DEBUG_CHECK_ALIGNMENT(res);
    return res;
}

#ifdef ERTS_SMP

void *
erts_alcu_realloc_thr_spec(ErtsAlcType_t type, void *extra,
			   void *ptr, Uint size)
{
    ErtsAllocatorThrSpec_t *tspec = (ErtsAllocatorThrSpec_t *) extra;
    int ix;
    Allctr_t *allctr;
    void *res;

    ix = ERTS_ALC_GET_THR_IX();

    ASSERT(0 <= ix && ix < tspec->size);

    allctr = tspec->allctr[ix];

    if (allctr->thread_safe)
	erts_mtx_lock(&allctr->mutex);

    res = do_erts_alcu_realloc(type, allctr, ptr, size, 0);

    if (allctr->thread_safe)
	erts_mtx_unlock(&allctr->mutex);

    DEBUG_CHECK_ALIGNMENT(res);

    return res;
}

void *
erts_alcu_realloc_mv_thr_spec(ErtsAlcType_t type, void *extra,
			      void *ptr, Uint size)
{
    ErtsAllocatorThrSpec_t *tspec = (ErtsAllocatorThrSpec_t *) extra;
    int ix;
    Allctr_t *allctr;
    void *res;

    ix = ERTS_ALC_GET_THR_IX();

    ASSERT(0 <= ix && ix < tspec->size);

    allctr = tspec->allctr[ix];

    if (allctr->thread_safe)
	erts_mtx_lock(&allctr->mutex);

    res = do_erts_alcu_alloc(type, allctr, size);
    if (!res) {
	if (allctr->thread_safe)
	    erts_mtx_unlock(&allctr->mutex);
	res = erts_alcu_realloc_thr_spec(type, allctr, ptr, size);
    }
    else {
	Block_t *blk;
	size_t cpy_size;

	blk = UMEM2BLK(ptr);
	cpy_size = BLK_SZ(blk) - ABLK_HDR_SZ;
	if (cpy_size > size)
	    cpy_size = size;
	sys_memcpy(res, ptr, cpy_size);
	do_erts_alcu_free(type, allctr, ptr);
	if (allctr->thread_safe)
	    erts_mtx_unlock(&allctr->mutex);
    }

    DEBUG_CHECK_ALIGNMENT(res);

    return res;
}

static ERTS_INLINE void *
realloc_thr_pref(ErtsAlcType_t type, void *extra, void *p, Uint size,
		 int force_move)
{
    int pref_ix;
    void *ptr, *res;
    Allctr_t *pref_allctr, *used_allctr;
    UWord old_user_size;

    if (!p)
	return erts_alcu_alloc_thr_pref(type, extra, size);

    pref_ix = get_pref_allctr(extra, &pref_allctr);
    ptr = get_used_allctr(extra, p, &used_allctr, &old_user_size);

    ASSERT(used_allctr && pref_allctr);

    if (!force_move && used_allctr == pref_allctr) {
	if (used_allctr->thread_safe)
	    erts_mtx_lock(&used_allctr->mutex);
	ERTS_ALCU_DBG_CHK_THR_ACCESS(used_allctr);
	res = do_erts_alcu_realloc(type,
				   used_allctr,
				   ptr,
				   size + sizeof(UWord),
				   0);
	if (used_allctr->thread_safe)
	    erts_mtx_unlock(&used_allctr->mutex);
	if (res)
	    res = put_used_allctr(res, pref_ix, size);
    }
    else {
	if (pref_allctr->thread_safe)
	    erts_mtx_lock(&pref_allctr->mutex);
	res = do_erts_alcu_alloc(type, pref_allctr, size + sizeof(UWord));
	if (pref_allctr->thread_safe && (!force_move
					 || used_allctr != pref_allctr))
	    erts_mtx_unlock(&pref_allctr->mutex);
	if (res) {
	    Block_t *blk;
	    size_t cpy_size;

	    res = put_used_allctr(res, pref_ix, size);

	    DEBUG_CHECK_ALIGNMENT(res);

	    blk = UMEM2BLK(ptr);
	    if (old_user_size != ERTS_AU_PREF_ALLOC_SIZE_MASK)
		cpy_size = old_user_size;
	    else {
		if (used_allctr->thread_safe && (!force_move
						 || used_allctr != pref_allctr))
		    erts_mtx_lock(&used_allctr->mutex);
		ERTS_SMP_LC_ASSERT(!used_allctr->thread_safe ||
				   erts_lc_mtx_is_locked(&used_allctr->mutex));
		cpy_size = BLK_SZ(blk);
		if (used_allctr->thread_safe && (!force_move
						 || used_allctr != pref_allctr))
		    erts_mtx_unlock(&used_allctr->mutex);
		cpy_size -= ABLK_HDR_SZ + sizeof(UWord);
	    }
	    if (cpy_size > size)
		cpy_size = size;
	    sys_memcpy(res, p, cpy_size);

	    if (!force_move || used_allctr != pref_allctr)
		enqueue_dealloc_other_instance(type, used_allctr, ptr);
	    else {
		do_erts_alcu_free(type, used_allctr, ptr);
		ASSERT(pref_allctr == used_allctr);
		if (pref_allctr->thread_safe)
		    erts_mtx_unlock(&pref_allctr->mutex);
	    }
	}
    }

    return res;
}

void *
erts_alcu_realloc_thr_pref(ErtsAlcType_t type, void *extra, void *p, Uint size)
{
    return realloc_thr_pref(type, extra, p, size, 0);
}

void *
erts_alcu_realloc_mv_thr_pref(ErtsAlcType_t type, void *extra,
			      void *p, Uint size)
{
    return realloc_thr_pref(type, extra, p, size, 1);
}

#endif

#endif

/* ------------------------------------------------------------------------- */

int
erts_alcu_start(Allctr_t *allctr, AllctrInit_t *init)
{
    /* erts_alcu_start assumes that allctr has been zeroed */

    if (!initialized)
	goto error;

#if HAVE_ERTS_MSEG
    sys_memcpy((void *) &allctr->mseg_opt,
	       (void *) &erts_mseg_default_opt,
	       sizeof(ErtsMsegOpt_t));
#ifdef ERTS_SMP
    if (init->tspec || init->tpref)
	allctr->mseg_opt.sched_spec = 1;
#endif
# if HALFWORD_HEAP
    allctr->mseg_opt.low_mem = init->low_mem;
# endif
#endif

    allctr->name_prefix			= init->name_prefix;
    if (!allctr->name_prefix)
	goto error;

    allctr->ix				= init->ix;
    allctr->alloc_no			= init->alloc_no;
    if (allctr->alloc_no < ERTS_ALC_A_MIN
	|| ERTS_ALC_A_MAX < allctr->alloc_no)
	allctr->alloc_no = ERTS_ALC_A_INVALID;

    if (!allctr->vsn_str)
	goto error;

    allctr->name.alloc			= THE_NON_VALUE;
    allctr->name.realloc		= THE_NON_VALUE;
    allctr->name.free			= THE_NON_VALUE;

    if (init->tspec)
	allctr->t			= init->tspec;
    else if (init->tpref)
	allctr->t			= init->tpref;
    else
	allctr->t			= 0;

    allctr->ramv			= init->ramv;
    allctr->main_carrier_size		= init->mmbcs;
    allctr->sbc_threshold		= init->sbct;
#if HAVE_ERTS_MSEG
    allctr->mseg_opt.abs_shrink_th	= init->asbcst;
    allctr->mseg_opt.rel_shrink_th	= init->rsbcst;
#endif
    allctr->sbc_move_threshold		= init->rsbcmt;
    allctr->mbc_move_threshold		= init->rmbcmt;
#if HAVE_ERTS_MSEG
    allctr->max_mseg_sbcs		= init->mmsbc;
    allctr->max_mseg_mbcs		= init->mmmbc;
#endif

    allctr->largest_mbc_size		= MAX(init->lmbcs, init->smbcs);
    allctr->smallest_mbc_size		= init->smbcs;
    allctr->mbc_growth_stages		= MAX(1, init->mbcgs);

    if (allctr->min_block_size < ABLK_HDR_SZ)
	goto error;
    allctr->min_block_size		= UNIT_CEILING(allctr->min_block_size
						       + sizeof(UWord));
#if ERTS_SMP
    if (init->tpref) {
	Uint sz = sizeof(Block_t);
	sz += ERTS_ALCU_DD_FIX_TYPE_OFFS*sizeof(UWord);
	if (init->fix)
	    sz += sizeof(UWord);
	sz = UNIT_CEILING(sz);
	if (sz > allctr->min_block_size)
	    allctr->min_block_size = sz;
    }
#endif



    allctr->sbmbc_threshold		= init->sbmbct;

    if (!erts_have_sbmbc_alloc
	|| ERTS_IS_SBMBC_ALLOCATOR_NO__(allctr->alloc_no))
	allctr->sbmbc_threshold = 0;

    if (!allctr->sbmbc_threshold)
	allctr->sbmbc_size = 0;
    else {
	Uint min_size;
	allctr->sbmbc_size = init->sbmbcs;
	min_size = allctr->sbmbc_threshold;
	min_size += allctr->min_block_size;
	min_size += allctr->mbc_header_size;
	if (allctr->sbmbc_size < min_size)
	    allctr->sbmbc_size = min_size;
    }


#if HAVE_ERTS_MSEG
    if (allctr->mseg_opt.abs_shrink_th > ~((UWord) 0) / 100)
	allctr->mseg_opt.abs_shrink_th = ~((UWord) 0) / 100;
#endif

#ifdef USE_THREADS
    if (init->ts) {
	allctr->thread_safe = 1;
	
#ifdef ERTS_ENABLE_LOCK_COUNT
	erts_mtx_init_x_opt(&allctr->mutex,
			    ERTS_IS_SBMBC_ALLOCATOR_NO__(allctr->alloc_no)
			    ? "sbmbc_alloc"
			    : "alcu_allocator",
			    make_small(allctr->alloc_no),
			    ERTS_LCNT_LT_ALLOC);
#else
	erts_mtx_init_x(&allctr->mutex,
			ERTS_IS_SBMBC_ALLOCATOR_NO__(allctr->alloc_no)
			? "sbmbc_alloc"
			: "alcu_allocator",
			make_small(allctr->alloc_no));
#endif /*ERTS_ENABLE_LOCK_COUNT*/
	
#ifdef DEBUG
	allctr->debug.saved_tid = 0;
#endif
    }
#endif

    if(!allctr->get_free_block
       || !allctr->link_free_block
       || !allctr->unlink_free_block
       || !allctr->info_options)
	goto error;

    if (!allctr->get_next_mbc_size)
	allctr->get_next_mbc_size = get_next_mbc_size;

    if (allctr->mbc_header_size < sizeof(Carrier_t))
	goto error;
#ifdef ERTS_SMP
    allctr->dd.use = 0;
    if (init->tpref) {
	allctr->mbc_header_size = (UNIT_CEILING(allctr->mbc_header_size
						+ FBLK_FTR_SZ
						+ ABLK_HDR_SZ
						+ sizeof(UWord))
				   - ABLK_HDR_SZ
				   - sizeof(UWord));
	allctr->sbc_header_size = (UNIT_CEILING(sizeof(Carrier_t)
						+ FBLK_FTR_SZ
						+ ABLK_HDR_SZ
						+ sizeof(UWord))
				   - ABLK_HDR_SZ
				   - sizeof(UWord));

	allctr->dd.use = 1;
	init_dd_queue(&allctr->dd.q);
    }
    else
#endif
    {
	allctr->mbc_header_size = (UNIT_CEILING(allctr->mbc_header_size
						+ FBLK_FTR_SZ
						+ ABLK_HDR_SZ)
				   - ABLK_HDR_SZ);
	allctr->sbc_header_size = (UNIT_CEILING(sizeof(Carrier_t)
						+ FBLK_FTR_SZ
						+ ABLK_HDR_SZ)
				   - ABLK_HDR_SZ);
    }

    if (allctr->main_carrier_size) {
	Block_t *blk;

#if HALFWORD_HEAP
	blk = create_carrier(allctr,
			     allctr->main_carrier_size,
			     CFLG_MBC
			     | CFLG_FORCE_SIZE
			     | CFLG_FORCE_MSEG
			     | CFLG_MAIN_CARRIER);
#else
	blk = create_carrier(allctr,
			     allctr->main_carrier_size,
			     CFLG_MBC
			     | CFLG_FORCE_SIZE
			     | CFLG_FORCE_SYS_ALLOC
			     | CFLG_MAIN_CARRIER);
#endif
	if (!blk)
	    goto error;

	(*allctr->link_free_block)(allctr, blk, 0);

	HARD_CHECK_BLK_CARRIER(allctr, blk);

    }

    if (init->fix) {
	int i;
	allctr->fix = init->fix;
	allctr->fix_shrink_scheduled = 0;
	for (i = 0; i < ERTS_ALC_NO_FIXED_SIZES; i++) {
	    allctr->fix[i].max_used = 0;
	    allctr->fix[i].limit = 0;
	    allctr->fix[i].type_size = init->fix_type_size[i];
	    allctr->fix[i].list_size = 0;
	    allctr->fix[i].list = NULL;
	    allctr->fix[i].allocated = 0;
	    allctr->fix[i].used = 0;
	}
    }

    return 1;

 error:

#ifdef USE_THREADS
    if (allctr->thread_safe)
	erts_mtx_destroy(&allctr->mutex);
#endif

    return 0;

}

/* ------------------------------------------------------------------------- */

void
erts_alcu_stop(Allctr_t *allctr)
{
    allctr->stopped = 1;

    while (allctr->sbc_list.first)
	destroy_carrier(allctr, SBC2BLK(allctr, allctr->sbc_list.first));
    while (allctr->mbc_list.first)
	destroy_carrier(allctr, MBC2FBLK(allctr, allctr->mbc_list.first));
    while (allctr->sbmbc_list.first)
	destroy_sbmbc(allctr, MBC2FBLK(allctr, allctr->sbmbc_list.first));

#ifdef USE_THREADS
    if (allctr->thread_safe)
	erts_mtx_destroy(&allctr->mutex);
#endif

}

/* ------------------------------------------------------------------------- */

void
erts_alcu_init(AlcUInit_t *init)
{

#if HAVE_ERTS_MSEG
    mseg_unit_size = erts_mseg_unit_size();

    if (mseg_unit_size % sizeof(Unit_t)) /* A little paranoid... */
	erl_exit(-1,
		 "Mseg unit size (%d) not evenly divideble by "
		 "internal unit size of alloc_util (%d)\n",
		 mseg_unit_size,
		 sizeof(Unit_t));

    max_mseg_carriers = init->mmc;
    sys_alloc_carrier_size = MSEG_UNIT_CEILING(init->ycs);
#else /* #if HAVE_ERTS_MSEG */
    sys_alloc_carrier_size = ((init->ycs + 4095) / 4096) * 4096;
#endif

#ifdef DEBUG
    carrier_alignment = sizeof(Unit_t);
#endif

    erts_mtx_init(&init_atoms_mtx, "alcu_init_atoms");

    atoms_initialized = 0;
    initialized = 1;
}


/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * NOTE:  erts_alcu_test() is only supposed to be used for testing.          *
 *                                                                           *
 * Keep alloc_SUITE_data/allocator_test.h updated if changes are made        *
 * to erts_alcu_test()                                                       *
\*                                                                           */

unsigned long
erts_alcu_test(unsigned long op, unsigned long a1, unsigned long a2)
{
    switch (op) {
    case 0x000:	return (unsigned long) BLK_SZ((Block_t *) a1);
    case 0x001:	return (unsigned long) BLK_UMEM_SZ((Block_t *) a1);
    case 0x002:	return (unsigned long) IS_PREV_BLK_FREE((Block_t *) a1);
    case 0x003:	return (unsigned long) IS_FREE_BLK((Block_t *) a1);
    case 0x004:	return (unsigned long) IS_LAST_BLK((Block_t *) a1);
    case 0x005:	return (unsigned long) UMEM2BLK((void *) a1);
    case 0x006:	return (unsigned long) BLK2UMEM((Block_t *) a1);
    case 0x007:	return (unsigned long) IS_SB_CARRIER((Carrier_t *) a1);
    case 0x008:	return (unsigned long) IS_SBC_BLK((Block_t *) a1);
    case 0x009:	return (unsigned long) IS_MB_CARRIER((Carrier_t *) a1);
    case 0x00a:	return (unsigned long) IS_MSEG_CARRIER((Carrier_t *) a1);
    case 0x00b:	return (unsigned long) CARRIER_SZ((Carrier_t *) a1);
    case 0x00c:	return (unsigned long) SBC2BLK((Allctr_t *) a1,
					       (Carrier_t *) a2);
    case 0x00d:	return (unsigned long) BLK2SBC((Allctr_t *) a1,
					       (Block_t *) a2);
    case 0x00e:	return (unsigned long) MBC2FBLK((Allctr_t *) a1,
						(Carrier_t *) a2);
    case 0x00f:	return (unsigned long) FBLK2MBC((Allctr_t *) a1,
						(Block_t *) a2);
    case 0x010:	return (unsigned long) ((Allctr_t *) a1)->mbc_list.first;
    case 0x011:	return (unsigned long) ((Allctr_t *) a1)->mbc_list.last;
    case 0x012:	return (unsigned long) ((Allctr_t *) a1)->sbc_list.first;
    case 0x013:	return (unsigned long) ((Allctr_t *) a1)->sbc_list.last;
    case 0x014:	return (unsigned long) ((Carrier_t *) a1)->next;
    case 0x015:	return (unsigned long) ((Carrier_t *) a1)->prev;
    case 0x016:	return (unsigned long) ABLK_HDR_SZ; 
    case 0x017:	return (unsigned long) ((Allctr_t *) a1)->min_block_size;
    case 0x018:	return (unsigned long) NXT_BLK((Block_t *) a1);
    case 0x019:	return (unsigned long) PREV_BLK((Block_t *) a1);
    case 0x01a: return (unsigned long) IS_FIRST_BLK((Block_t *) a1);
    case 0x01b: return (unsigned long) sizeof(Unit_t);
    default:	ASSERT(0); return ~((unsigned long) 0);
    }
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * Debug functions                                                           *
\*                                                                           */

void
erts_alcu_verify_unused(Allctr_t *allctr)
{
    UWord no;

    no = allctr->sbcs.curr.norm.mseg.no;
    no += allctr->sbcs.curr.norm.sys_alloc.no;
    no += allctr->mbcs.blocks.curr.no;
    no += allctr->sbmbcs.blocks.curr.no;

    if (no) {
	UWord sz = allctr->sbcs.blocks.curr.size;
	sz += allctr->mbcs.blocks.curr.size;
	sz += allctr->sbmbcs.blocks.curr.size;
	erl_exit(ERTS_ABORT_EXIT,
		 "%salloc() used when expected to be unused!\n"
		 "Total amount of blocks allocated: %bpu\n"
		 "Total amount of bytes allocated: %bpu\n",
		 allctr->name_prefix, no, sz);
    }
}

void
erts_alcu_verify_unused_ts(Allctr_t *allctr)
{
#ifdef USE_THREADS
    erts_mtx_lock(&allctr->mutex);
#endif
    erts_alcu_verify_unused(allctr);
#ifdef USE_THREADS
    erts_mtx_unlock(&allctr->mutex);
#endif
}

#ifdef ERTS_ALLOC_UTIL_HARD_DEBUG

static void
check_blk_carrier(Allctr_t *allctr, Block_t *iblk)
{
    Carrier_t *crr;
    CarrierList_t *cl;

    if (IS_SBC_BLK(iblk)) {
	Carrier_t *sbc = BLK2SBC(allctr, iblk);

	ASSERT(SBC2BLK(allctr, sbc) == iblk);
	ASSERT(IS_ALLOCED_BLK(iblk));
	ASSERT(IS_FIRST_BLK(iblk));
	ASSERT(IS_LAST_BLK(iblk));
	ASSERT(CARRIER_SZ(sbc) - allctr->sbc_header_size >= BLK_SZ(iblk));
#if HAVE_ERTS_MSEG
	if (IS_MSEG_CARRIER(sbc)) {
	    ASSERT(CARRIER_SZ(sbc) % mseg_unit_size == 0);
	}
#endif
	crr = sbc;
	cl = &allctr->sbc_list;
    }
    else {
	Carrier_t *mbc = NULL;
	Block_t *prev_blk = NULL;
	Block_t *blk;
	char *carrier_end;
	Uint is_free_blk;
	Uint tot_blk_sz;
	Uint blk_sz;

	blk = iblk;
	tot_blk_sz = 0;

	while (1) {

	    if (prev_blk) {
		ASSERT(NXT_BLK(prev_blk) == blk);
		if (IS_FREE_BLK(prev_blk)) {
		    ASSERT(IS_PREV_BLK_FREE(blk));
		    ASSERT(prev_blk == PREV_BLK(blk));
		}
		else {
		    ASSERT(IS_PREV_BLK_ALLOCED(blk));
		}
	    }

	    if (mbc) {
		if (blk == iblk)
		    break;
		ASSERT(((Block_t *) mbc) < blk && blk < iblk);
	    }
	    else
		ASSERT(blk >= iblk);


	    ASSERT(IS_MBC_BLK(blk));

	    blk_sz = BLK_SZ(blk);

	    ASSERT(blk_sz % sizeof(Unit_t) == 0);
	    ASSERT(blk_sz >= allctr->min_block_size);

	    tot_blk_sz += blk_sz;

	    is_free_blk = (int) IS_FREE_BLK(blk);
	    if(is_free_blk) {
		if (IS_NOT_LAST_BLK(blk))
		    ASSERT(*((UWord *) (((char *) blk)+blk_sz-sizeof(UWord)))
			   == blk_sz);
	    }

	    if (allctr->check_block)
		(*allctr->check_block)(allctr, blk, (int) is_free_blk);

	    if (IS_LAST_BLK(blk)) {
		carrier_end = ((char *) NXT_BLK(blk));
		mbc = *((Carrier_t **) NXT_BLK(blk));
		prev_blk = NULL;
		blk = MBC2FBLK(allctr, mbc);
		ASSERT(IS_FIRST_BLK(blk));
	    }
	    else {
		prev_blk = blk;
		blk = NXT_BLK(blk);
	    }
	}

	ASSERT(IS_MB_CARRIER(mbc));
	ASSERT((((char *) mbc)
		+ allctr->mbc_header_size
		+ tot_blk_sz) == carrier_end);
	ASSERT(((char *) mbc) + CARRIER_SZ(mbc) - sizeof(Unit_t) <= carrier_end
	       && carrier_end <= ((char *) mbc) + CARRIER_SZ(mbc));

	if (allctr->check_mbc)
	    (*allctr->check_mbc)(allctr, mbc);

#if HAVE_ERTS_MSEG
	if (IS_MSEG_CARRIER(mbc)) {
	    ASSERT(CARRIER_SZ(mbc) % mseg_unit_size == 0);
	}
#endif
	crr = mbc;
	cl = &allctr->mbc_list;
    }

#if 0 /* FIXIT sbmbc */
    if (cl->first == crr) {
	ASSERT(!crr->prev);
    }
    else {
	ASSERT(crr->prev);
	ASSERT(crr->prev->next == crr);
    }
    if (cl->last == crr) {
	ASSERT(!crr->next);
    }
    else {
	ASSERT(crr->next);
	ASSERT(crr->next->prev == crr);
    }
#endif
}

#endif
