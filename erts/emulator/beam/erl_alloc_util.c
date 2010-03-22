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

#define ALLOC_ZERO_EQ_NULL 0

static int atoms_initialized = 0;
static int initialized = 0;


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

#define ERTS_ALCU_FLG_FAIL_REALLOC_MOVE ((UWord) 1)

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
    ASSERT(((AP)->sbcs.curr_mseg.no && (AP)->sbcs.curr_mseg.size)	\
	   || (!(AP)->sbcs.curr_mseg.no && !(AP)->sbcs.curr_mseg.size));\
    ASSERT(((AP)->sbcs.curr_sys_alloc.no && (AP)->sbcs.curr_sys_alloc.size)\
	   || (!(AP)->sbcs.curr_sys_alloc.no && !(AP)->sbcs.curr_sys_alloc.size));\
    ASSERT(((AP)->mbcs.curr_mseg.no && (AP)->mbcs.curr_mseg.size)	\
	   || (!(AP)->mbcs.curr_mseg.no && !(AP)->mbcs.curr_mseg.size));\
    ASSERT(((AP)->mbcs.curr_sys_alloc.no && (AP)->mbcs.curr_sys_alloc.size)\
	   || (!(AP)->mbcs.curr_sys_alloc.no && !(AP)->mbcs.curr_sys_alloc.size))

#else
#define DEBUG_CHECK_CARRIER_NO_SZ(AP)
#endif

#define STAT_SBC_ALLOC(AP, BSZ)						\
    (AP)->sbcs.blocks.curr.size += (BSZ);				\
    if ((AP)->sbcs.blocks.max.size < (AP)->sbcs.blocks.curr.size)	\
	(AP)->sbcs.blocks.max.size = (AP)->sbcs.blocks.curr.size;	\
    if ((AP)->sbcs.max.no < ((AP)->sbcs.curr_mseg.no			\
			     + (AP)->sbcs.curr_sys_alloc.no))		\
	(AP)->sbcs.max.no = ((AP)->sbcs.curr_mseg.no			\
			     + (AP)->sbcs.curr_sys_alloc.no);		\
    if ((AP)->sbcs.max.size < ((AP)->sbcs.curr_mseg.size		\
			       + (AP)->sbcs.curr_sys_alloc.size))	\
	(AP)->sbcs.max.size = ((AP)->sbcs.curr_mseg.size		\
			       + (AP)->sbcs.curr_sys_alloc.size)

#define STAT_MSEG_SBC_ALLOC(AP, CSZ, BSZ)				\
do {									\
    (AP)->sbcs.curr_mseg.no++;						\
    (AP)->sbcs.curr_mseg.size += (CSZ);					\
    STAT_SBC_ALLOC((AP), (BSZ));					\
    DEBUG_CHECK_CARRIER_NO_SZ((AP));					\
} while (0)

#define STAT_SYS_ALLOC_SBC_ALLOC(AP, CSZ, BSZ)				\
do {									\
    (AP)->sbcs.curr_sys_alloc.no++;					\
    (AP)->sbcs.curr_sys_alloc.size += (CSZ);				\
    STAT_SBC_ALLOC((AP), (BSZ));					\
    DEBUG_CHECK_CARRIER_NO_SZ((AP));					\
} while (0)


#define STAT_SBC_FREE(AP, BSZ)						\
    ASSERT((AP)->sbcs.blocks.curr.size >= (BSZ));			\
    (AP)->sbcs.blocks.curr.size -= (BSZ)

#define STAT_MSEG_SBC_FREE(AP, CSZ, BSZ)				\
do {									\
    ASSERT((AP)->sbcs.curr_mseg.no > 0);				\
    (AP)->sbcs.curr_mseg.no--;						\
    ASSERT((AP)->sbcs.curr_mseg.size >= (CSZ));				\
    (AP)->sbcs.curr_mseg.size -= (CSZ);					\
    STAT_SBC_FREE((AP), (BSZ));						\
    DEBUG_CHECK_CARRIER_NO_SZ((AP));					\
} while (0)

#define STAT_SYS_ALLOC_SBC_FREE(AP, CSZ, BSZ)				\
do {									\
    ASSERT((AP)->sbcs.curr_sys_alloc.no > 0);				\
    (AP)->sbcs.curr_sys_alloc.no--;					\
    ASSERT((AP)->sbcs.curr_sys_alloc.size >= (CSZ));			\
    (AP)->sbcs.curr_sys_alloc.size -= (CSZ);				\
    STAT_SBC_FREE((AP), (BSZ));						\
    DEBUG_CHECK_CARRIER_NO_SZ((AP));					\
} while (0)

#define STAT_MBC_ALLOC(AP)						\
    if ((AP)->mbcs.max.no < ((AP)->mbcs.curr_mseg.no			\
			     + (AP)->mbcs.curr_sys_alloc.no))		\
	(AP)->mbcs.max.no = ((AP)->mbcs.curr_mseg.no			\
			     + (AP)->mbcs.curr_sys_alloc.no);		\
    if ((AP)->mbcs.max.size < ((AP)->mbcs.curr_mseg.size		\
			       + (AP)->mbcs.curr_sys_alloc.size))	\
	(AP)->mbcs.max.size = ((AP)->mbcs.curr_mseg.size		\
			       + (AP)->mbcs.curr_sys_alloc.size)


#define STAT_MSEG_MBC_ALLOC(AP, CSZ)					\
do {									\
    (AP)->mbcs.curr_mseg.no++;						\
    (AP)->mbcs.curr_mseg.size += (CSZ);					\
    STAT_MBC_ALLOC((AP));						\
    DEBUG_CHECK_CARRIER_NO_SZ((AP));					\
} while (0)

#define STAT_SYS_ALLOC_MBC_ALLOC(AP, CSZ)				\
do {									\
    (AP)->mbcs.curr_sys_alloc.no++;					\
    (AP)->mbcs.curr_sys_alloc.size += (CSZ);				\
    STAT_MBC_ALLOC((AP));						\
    DEBUG_CHECK_CARRIER_NO_SZ((AP));					\
} while (0)

#define STAT_MSEG_MBC_FREE(AP, CSZ)					\
do {									\
    ASSERT((AP)->mbcs.curr_mseg.no > 0);				\
    (AP)->mbcs.curr_mseg.no--;						\
    ASSERT((AP)->mbcs.curr_mseg.size >= (CSZ));				\
    (AP)->mbcs.curr_mseg.size -= (CSZ);					\
    DEBUG_CHECK_CARRIER_NO_SZ((AP));					\
} while (0)

#define STAT_SYS_ALLOC_MBC_FREE(AP, CSZ)				\
do {									\
    ASSERT((AP)->mbcs.curr_sys_alloc.no > 0);				\
    (AP)->mbcs.curr_sys_alloc.no--;					\
    ASSERT((AP)->mbcs.curr_sys_alloc.size >= (CSZ));			\
    (AP)->mbcs.curr_sys_alloc.size -= (CSZ);				\
    DEBUG_CHECK_CARRIER_NO_SZ((AP));					\
} while (0)

#define STAT_MBC_BLK_ALLOC(AP, BSZ)					\
do {									\
    (AP)->mbcs.blocks.curr.no++;					\
    if ((AP)->mbcs.blocks.max.no < (AP)->mbcs.blocks.curr.no)		\
	(AP)->mbcs.blocks.max.no = (AP)->mbcs.blocks.curr.no;		\
    (AP)->mbcs.blocks.curr.size += (BSZ);				\
    if ((AP)->mbcs.blocks.max.size < (AP)->mbcs.blocks.curr.size)	\
	(AP)->mbcs.blocks.max.size = (AP)->mbcs.blocks.curr.size;	\
} while (0)

#define STAT_MBC_BLK_FREE(AP, BSZ)					\
do {									\
    ASSERT((AP)->mbcs.blocks.curr.no > 0);				\
    (AP)->mbcs.blocks.curr.no--;					\
    ASSERT((AP)->mbcs.blocks.curr.size >= (BSZ));			\
    (AP)->mbcs.blocks.curr.size -= (BSZ);				\
} while (0)

/* Debug stuff... */
#ifdef DEBUG
static UWord carrier_alignment;
#define DEBUG_SAVE_ALIGNMENT(C)						\
do {									\
    UWord algnmnt__ = sizeof(Unit_t) - (((UWord) (C)) % sizeof(Unit_t));	\
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
#define ERTS_ALCU_DBG_CHK_THR_SPEC(A)					\
do {									\
    if (!(A)->thread_safe) {						\
	if (!(A)->debug.saved_tid)					\
	    (A)->debug.tid = erts_thr_self();				\
	else {								\
	    ASSERT(ethr_equal_tids((A)->debug.tid, erts_thr_self()));	\
	}								\
    }									\
} while (0)
#else
#define ERTS_ALCU_DBG_CHK_THR_SPEC(A)
#endif
#else
#define ERTS_ALCU_DBG_CHK_THR_SPEC(A)
#endif


static void make_name_atoms(Allctr_t *allctr);


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
    int cs = (allctr->mbcs.curr_mseg.no
	      + allctr->mbcs.curr_sys_alloc.no
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


static Block_t *create_carrier(Allctr_t *, Uint, UWord);
static void destroy_carrier(Allctr_t *, Block_t *);

/* Multi block carrier alloc/realloc/free ... */

/* NOTE! mbc_alloc() may in case of memory shortage place the requested
 * block in a sbc.
 */
static ERTS_INLINE void *
mbc_alloc_block(Allctr_t *allctr, Uint size, Uint *blk_szp)
{
    Block_t *blk;

    ASSERT(size);
    ASSERT(size < allctr->sbc_threshold);

    *blk_szp = UMEMSZ2BLKSZ(allctr, size);

    blk = (*allctr->get_free_block)(allctr, *blk_szp, NULL, 0);

#if HALFWORD_HEAP
    if (!blk) {
	blk = create_carrier(allctr, *blk_szp, CFLG_MBC|CFLG_FORCE_MSEG);
    }
#else
    if (!blk) {
	blk = create_carrier(allctr, *blk_szp, CFLG_MBC);
	if (!blk) {
	    /* Emergency! We couldn't create the carrier as we wanted.
	       Try to place it in a sys_alloced sbc. */
	    blk = create_carrier(allctr,
				 size,
				 CFLG_SBC|CFLG_FORCE_SIZE|CFLG_FORCE_SYS_ALLOC);
	}
    }
#endif

#ifdef ERTS_ALLOC_UTIL_HARD_DEBUG
    if (IS_MBC_BLK(blk)) {
	(*allctr->link_free_block)(allctr, blk);
	HARD_CHECK_BLK_CARRIER(allctr, blk);
	(*allctr->unlink_free_block)(allctr, blk);
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
		   int valid_blk_info)
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
	(*allctr->link_free_block)(allctr, nxt_blk);

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

    STAT_MBC_BLK_ALLOC(allctr, blk_sz);

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
    blk = mbc_alloc_block(allctr, size, &blk_sz);
    if (!blk)
	return NULL;
    if (IS_MBC_BLK(blk))
	mbc_alloc_finalize(allctr,
			   blk,
			   BLK_SZ(blk),
			   GET_BLK_HDR_FLGS(blk),
			   blk_sz,
			   1);
    return BLK2UMEM(blk);
}

static void
mbc_free(Allctr_t *allctr, void *p)
{
    Uint is_first_blk;
    Uint is_last_blk;
    Uint blk_sz;
    Block_t *blk;
    Block_t *nxt_blk;


    ASSERT(p);

    blk = UMEM2BLK(p);
    blk_sz = BLK_SZ(blk);

    ASSERT(IS_MBC_BLK(blk));
    ASSERT(blk_sz >= allctr->min_block_size);

    HARD_CHECK_BLK_CARRIER(allctr, blk);

    STAT_MBC_BLK_FREE(allctr, blk_sz);

    is_first_blk = IS_FIRST_BLK(blk);
    is_last_blk = IS_LAST_BLK(blk);

    if (!is_first_blk && IS_PREV_BLK_FREE(blk)) {
	/* Coalesce with previous block... */
	blk = PREV_BLK(blk);
	(*allctr->unlink_free_block)(allctr, blk);

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
	    (*allctr->unlink_free_block)(allctr, nxt_blk);
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
	&& allctr->main_carrier != FBLK2MBC(allctr, blk))
	destroy_carrier(allctr, blk);
    else {
	(*allctr->link_free_block)(allctr, blk);
	HARD_CHECK_BLK_CARRIER(allctr, blk);
    }
}

static void *
mbc_realloc(Allctr_t *allctr, void *p, Uint size, UWord flgs)
{
    void *new_p;
    Uint old_blk_sz;
    Block_t *blk;
#ifndef MBC_REALLOC_ALWAYS_MOVES
    Block_t *new_blk, *cand_blk;
    Uint cand_blk_sz;
    Uint blk_sz;
    Block_t *nxt_blk;
    Uint nxt_blk_sz;
    Uint is_last_blk;
#endif /* #ifndef MBC_REALLOC_ALWAYS_MOVES */

    ASSERT(p);
    ASSERT(size);
    ASSERT(size < allctr->sbc_threshold);

    blk = (Block_t *) UMEM2BLK(p);
    old_blk_sz = BLK_SZ(blk);

    ASSERT(old_blk_sz >= allctr->min_block_size);

#ifdef MBC_REALLOC_ALWAYS_MOVES
    if (flgs & ERTS_ALCU_FLG_FAIL_REALLOC_MOVE)
	return NULL;
#else /* !MBC_REALLOC_ALWAYS_MOVES */
    blk_sz = UMEMSZ2BLKSZ(allctr, size);

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

	if (diff_sz_val >= (~((Uint) 0) / 100)) {
	    /* div both by 128 */
	    old_blk_sz_val >>= 7;
	    diff_sz_val >>= 7;
	}

	/* Avoid fragmentation by moving the block if it is shrunk much */
	if (100*diff_sz_val > allctr->mbc_move_threshold*old_blk_sz_val) {
	    if (flgs & ERTS_ALCU_FLG_FAIL_REALLOC_MOVE)
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
						blk_sz,
						cand_blk,
						cand_blk_sz);

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

	STAT_MBC_BLK_FREE(allctr, old_blk_sz);
	STAT_MBC_BLK_ALLOC(allctr, blk_sz);

	ASSERT(BLK_SZ(blk) >= allctr->min_block_size);

	if (is_last_blk)
	    SET_LAST_BLK(nxt_blk);
	else {
	    nxt_nxt_blk = NXT_BLK(nxt_blk);
	    if (IS_FREE_BLK(nxt_nxt_blk)) {
		/* Coalesce with next free block... */
		nxt_blk_sz += BLK_SZ(nxt_nxt_blk);
		(*allctr->unlink_free_block)(allctr, nxt_nxt_blk);
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

	(*allctr->link_free_block)(allctr, nxt_blk);


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
	if (IS_FREE_BLK(nxt_blk) && blk_sz <= old_blk_sz + nxt_blk_sz) {
	    /* Grow into next block... */

	    HARD_CHECK_BLK_CARRIER(allctr, blk);

	    (*allctr->unlink_free_block)(allctr, nxt_blk);
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

		(*allctr->link_free_block)(allctr, nxt_blk);

		ASSERT(IS_FREE_BLK(nxt_blk));
	    }

	    STAT_MBC_BLK_FREE(allctr, old_blk_sz);
	    STAT_MBC_BLK_ALLOC(allctr, blk_sz);


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

    if (flgs & ERTS_ALCU_FLG_FAIL_REALLOC_MOVE)
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

    if (cand_blk_sz < blk_sz) {
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
					    blk_sz,
					    cand_blk,
					    cand_blk_sz);
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
			       1);
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

	    (*allctr->unlink_free_block)(allctr, new_blk); /* prev */

	    if (is_last_blk) 
		new_blk_flgs |= LAST_BLK_HDR_FLG;
	    else {
		nxt_blk = NXT_BLK(blk);
		if (IS_FREE_BLK(nxt_blk)) {
		    new_blk_flgs |= GET_LAST_BLK_HDR_FLG(nxt_blk);
		    new_blk_sz += BLK_SZ(nxt_blk);
		    (*allctr->unlink_free_block)(allctr, nxt_blk);
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
			       0);

	    STAT_MBC_BLK_FREE(allctr, old_blk_sz);

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
    if (erts_mseg_no() >= max_mseg_carriers)
	goto try_sys_alloc;
    if (flags & CFLG_SBC) {
	if (allctr->sbcs.curr_mseg.no >= allctr->max_mseg_sbcs)
	    goto try_sys_alloc;
    }
    else {
	if (allctr->mbcs.curr_mseg.no >= allctr->max_mseg_mbcs)
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
	if (sizeof(Unit_t) == sizeof(UWord))
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
	if (sizeof(Unit_t) == sizeof(UWord))
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
	if (sizeof(Unit_t) == sizeof(UWord))
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
	if (sizeof(Unit_t) == sizeof(UWord))
	    crr_sz += sizeof(UWord);
#endif
	CHECK_1BLK_CARRIER(allctr, 0, is_mseg, crr, crr_sz, blk, blk_sz);
#ifdef ERTS_ALLOC_UTIL_HARD_DEBUG
	if (sizeof(Unit_t) == sizeof(UWord))
	    crr_sz -= sizeof(UWord);
#endif
	if (allctr->creating_mbc)
	    (*allctr->creating_mbc)(allctr, crr);

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
	    (*allctr->link_free_block)(allctr, blk);
	    HARD_CHECK_BLK_CARRIER(allctr, blk);
	    (*allctr->unlink_free_block)(allctr, blk);
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
	    (*allctr->destroying_mbc)(allctr, crr);
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

#if HAVE_ERTS_MSEG
    Eterm mmc;
#endif
    Eterm ycs;

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
#ifdef DEBUG
    Eterm end_of_atoms;
#endif
} am;

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

#if HAVE_ERTS_MSEG
	AM_INIT(mmc);
#endif
	AM_INIT(ycs);

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

#ifdef DEBUG
	for (atom = (Eterm *) &am; atom < &am.end_of_atoms; atom++) {
	    ASSERT(*atom != THE_NON_VALUE);
	}
#endif
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
sz_info_carriers(Allctr_t *allctr,
		 CarriersStats_t *cs,
		 char *prefix,
		 int *print_to_p,
		 void *print_to_arg,
		 Uint **hpp,
		 Uint *szp)
{
    Eterm res = THE_NON_VALUE;
    Uint curr_size = cs->curr_mseg.size + cs->curr_sys_alloc.size;

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
		   "%scarriers size: %bpu %bpu %bpu\n",
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
    Uint curr_no   = cs->curr_mseg.no   + cs->curr_sys_alloc.no;
    Uint curr_size = cs->curr_mseg.size + cs->curr_sys_alloc.size;

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
		   "%scarriers: %bpu %bpu %bpu\n",
		   prefix,
		   curr_no,
		   cs->max.no,
		   cs->max_ever.no);
#if HAVE_ERTS_MSEG
	erts_print(to,
		   arg,
		   "%smseg carriers: %bpu\n",
		   prefix,
		   cs->curr_mseg.no);
#endif
	erts_print(to,
		   arg,
		   "%ssys_alloc carriers: %bpu\n",
		   prefix,
		   cs->curr_sys_alloc.no);
	erts_print(to,
		   arg,
		   "%scarriers size: %bpu %bpu %bpu\n",
		   prefix,
		   curr_size,
		   cs->max.size,
		   cs->max_ever.size);
#if HAVE_ERTS_MSEG
	erts_print(to,
		   arg,
		   "%smseg carriers size: %bpu\n",
		   prefix,
		   cs->curr_mseg.size);
#endif
	erts_print(to,
		   arg,
		   "%ssys_alloc carriers size: %bpu\n",
		   prefix,
		   cs->curr_sys_alloc.size);
    }

    if (hpp || szp) {
	res = NIL;
	add_2tup(hpp, szp, &res,
		 am.sys_alloc_carriers_size,
		 bld_unstable_uint(hpp, szp, cs->curr_sys_alloc.size));
#if HAVE_ERTS_MSEG
	add_2tup(hpp, szp, &res,
		 am.mseg_alloc_carriers_size,
		 bld_unstable_uint(hpp, szp, cs->curr_mseg.size));
#endif
	add_4tup(hpp, szp, &res,
		 am.carriers_size,
		 bld_unstable_uint(hpp, szp, curr_size),
		 bld_unstable_uint(hpp, szp, cs->max.size),
		 bld_unstable_uint(hpp, szp, cs->max_ever.size));
	add_2tup(hpp, szp, &res,
		 am.sys_alloc_carriers,
		 bld_unstable_uint(hpp, szp, cs->curr_sys_alloc.no));
#if HAVE_ERTS_MSEG
	add_2tup(hpp, szp, &res,
		 am.mseg_alloc_carriers,
		 bld_unstable_uint(hpp, szp, cs->curr_mseg.no));
#endif
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
	erts_print(TO, TOA, "%s calls: %bpu\n", NAME, CC.no);		\
    else								\
	erts_print(TO, TOA, "%s calls: %bpu%09lu\n", NAME, CC.giga_no, CC.no)

#define PRINT_CC_5(TO, TOA, PRFX, NAME, CC)				\
    if ((CC).giga_no == 0)						\
	erts_print(TO, TOA, "%s%s calls: %bpu\n",PRFX,NAME,CC.no);	\
    else								\
	erts_print(TO, TOA, "%s%s calls: %bpu%09lu\n",PRFX,NAME,CC.giga_no,CC.no)

	char *prefix = allctr->name_prefix;
	int to = *print_to_p;
	void *arg = print_to_arg;

	PRINT_CC_5(to, arg, prefix, "alloc",        allctr->calls.this_alloc);
	PRINT_CC_5(to, arg, prefix, "free",         allctr->calls.this_free);
	PRINT_CC_5(to, arg, prefix, "realloc",      allctr->calls.this_realloc);

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
		   "option sbct: %bpu\n"
#if HAVE_ERTS_MSEG
		   "option asbcst: %bpu\n"
		   "option rsbcst: %bpu\n"
#endif
		   "option rsbcmt: %bpu\n"
		   "option rmbcmt: %bpu\n"
		   "option mmbcs: %bpu\n"
#if HAVE_ERTS_MSEG
		   "option mmsbc: %bpu\n"
		   "option mmmbc: %bpu\n"
#endif
		   "option lmbcs: %bpu\n"
		   "option smbcs: %bpu\n"
		   "option mbcgs: %bpu\n",
		   topt,
		   allctr->ramv ? "true" : "false",
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
		   allctr->mbc_growth_stages);
    }

    res = (*allctr->info_options)(allctr, "option ", print_to_p, print_to_arg,
				  hpp, szp);

    if (hpp || szp) {
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
	add_2tup(hpp, szp, &res, am.ramv, allctr->ramv ? am_true : am_false);
	add_2tup(hpp, szp, &res, am.t, (allctr->t
					? bld_uint(hpp, szp, (Uint) allctr->t)
					: am_false));
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
    cs->max.no			= cs->curr_mseg.no   + cs->curr_sys_alloc.no;
    cs->max.size		= cs->curr_mseg.size + cs->curr_sys_alloc.size;
    cs->blocks.max.no		= cs->blocks.curr.no;
    cs->blocks.max.size		= cs->blocks.curr.size;
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
		   "option mmc: %bpu\n"
#endif
		   "option ycs: %bpu\n",
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
    Eterm res, mbcs, sbcs;

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

    if (hpp || szp)
	ensure_atoms_initialized(allctr);

    /* Update sbc values not continously updated */
    allctr->sbcs.blocks.curr.no
	= allctr->sbcs.curr_mseg.no + allctr->sbcs.curr_sys_alloc.no;
    allctr->sbcs.blocks.max.no = allctr->sbcs.max.no;

    update_max_ever_values(&allctr->mbcs);
    update_max_ever_values(&allctr->sbcs);

    mbcs  = sz_info_carriers(allctr, &allctr->mbcs, "mbcs ", print_to_p,
			     print_to_arg, hpp, szp);
    sbcs  = sz_info_carriers(allctr, &allctr->sbcs, "sbcs ", print_to_p,
			     print_to_arg, hpp, szp);

    if (hpp || szp) {
	res = NIL;
	add_2tup(hpp, szp, &res, am.sbcs, sbcs);
	add_2tup(hpp, szp, &res, am.mbcs, mbcs);
    }

    if (begin_max_period) {
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
    Eterm res, sett, mbcs, sbcs, calls;

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

    if (hpp || szp)
	ensure_atoms_initialized(allctr);

    /* Update sbc values not continously updated */
    allctr->sbcs.blocks.curr.no
	= allctr->sbcs.curr_mseg.no + allctr->sbcs.curr_sys_alloc.no;
    allctr->sbcs.blocks.max.no = allctr->sbcs.max.no;

    update_max_ever_values(&allctr->mbcs);
    update_max_ever_values(&allctr->sbcs);

    if (print_to_p) {
	erts_print(*print_to_p,
		   print_to_arg,
		   "versions: %s %s\n",
		   allctr->vsn_str,
		   ERTS_ALCU_VSN_STR);
    }

    sett  = info_options(allctr, print_to_p, print_to_arg, hpp, szp);
    mbcs  = info_carriers(allctr, &allctr->mbcs, "mbcs ", print_to_p,
			  print_to_arg, hpp, szp);
    sbcs  = info_carriers(allctr, &allctr->sbcs, "sbcs ", print_to_p,
			  print_to_arg, hpp, szp);
    calls = info_calls(allctr, print_to_p, print_to_arg, hpp, szp);

    if (hpp || szp) {
	res = NIL;

	add_2tup(hpp, szp, &res, am.calls, calls);
	add_2tup(hpp, szp, &res, am.sbcs, sbcs);
	add_2tup(hpp, szp, &res, am.mbcs, mbcs);
	add_2tup(hpp, szp, &res, am.options, sett);
	add_3tup(hpp, szp, &res,
		 am.versions,
		 bld_string(hpp, szp, allctr->vsn_str),
		 bld_string(hpp, szp, ERTS_ALCU_VSN_STR));;
    }

    if (begin_max_period) {
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
erts_alcu_current_size(Allctr_t *allctr, AllctrSize_t *size)
{

#ifdef USE_THREADS
    if (allctr->thread_safe)
	erts_mtx_lock(&allctr->mutex);
#endif

    size->carriers = allctr->mbcs.curr_mseg.size;
    size->carriers += allctr->mbcs.curr_sys_alloc.size;
    size->carriers += allctr->sbcs.curr_mseg.size;
    size->carriers += allctr->sbcs.curr_sys_alloc.size;

    size->blocks = allctr->mbcs.blocks.curr.size;
    size->blocks += allctr->sbcs.blocks.curr.size;

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

    ASSERT(initialized);

    ASSERT(allctr);

    ERTS_ALCU_DBG_CHK_THR_SPEC(allctr);

#if ALLOC_ZERO_EQ_NULL
    if (!size)
	return NULL;
#endif

    INC_CC(allctr->calls.this_alloc);

    if (size >= allctr->sbc_threshold) {
#if HALFWORD_HEAP
	Block_t *blk = create_carrier(allctr, size,
				      CFLG_SBC | CFLG_FORCE_MSEG);
#else
	Block_t *blk = create_carrier(allctr, size, CFLG_SBC);
#endif
	res = blk ? BLK2UMEM(blk) : NULL;
    }
    else
	res = mbc_alloc(allctr, size);

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

void *
erts_alcu_alloc_thr_spec(ErtsAlcType_t type, void *extra, Uint size)
{
    ErtsAllocatorThrSpec_t *tspec = (ErtsAllocatorThrSpec_t *) extra;
    int ix = erts_alc_get_thr_ix();
    Allctr_t *allctr;
    int unlock;
    void *res;

    ASSERT(ix > 0);
    if (ix < tspec->size) {
	allctr = tspec->allctr[ix];
	unlock = 0;
    }
    else {
	allctr = tspec->allctr[0];
	unlock = 1;
	erts_mtx_lock(&allctr->mutex);
    }

    res = do_erts_alcu_alloc(type, allctr, size);

    if (unlock)
	erts_mtx_unlock(&allctr->mutex);

    DEBUG_CHECK_ALIGNMENT(res);

    return res;
}

void *
erts_alcu_alloc_thr_pref(ErtsAlcType_t type, void *extra, Uint size)
{
    ErtsAllocatorThrSpec_t *tspec = (ErtsAllocatorThrSpec_t *) extra;
    int ix = erts_alc_get_thr_ix();
    Allctr_t *allctr;
    void *res;

    ASSERT(sizeof(UWord) == sizeof(Allctr_t *));
    ASSERT(ix > 0);
    if (ix >= tspec->size)
	ix = (ix % (tspec->size - 1)) + 1;
    allctr = tspec->allctr[ix];
    erts_mtx_lock(&allctr->mutex);
    res = do_erts_alcu_alloc(type, allctr, size + sizeof(UWord));
    if (res) {
	*((Allctr_t **) res) = allctr;
	res = (void *) (((char *) res) + sizeof(UWord));
    }
    erts_mtx_unlock(&allctr->mutex);
    DEBUG_CHECK_ALIGNMENT(res);
    return res;
}

#endif

/* ------------------------------------------------------------------------- */

static ERTS_INLINE void
do_erts_alcu_free(ErtsAlcType_t type, void *extra, void *p)
{
    Allctr_t *allctr = (Allctr_t *) extra; 
    ASSERT(initialized);

    ASSERT(allctr);

    ERTS_ALCU_DBG_CHK_THR_SPEC(allctr);

    if (p) {
	Block_t *blk;

	INC_CC(allctr->calls.this_free);

	blk = UMEM2BLK(p);
	if (IS_SBC_BLK(blk))
	    destroy_carrier(allctr, blk);
	else
	    mbc_free(allctr, p);
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

void
erts_alcu_free_thr_spec(ErtsAlcType_t type, void *extra, void *p)
{
    ErtsAllocatorThrSpec_t *tspec = (ErtsAllocatorThrSpec_t *) extra;
    int ix = erts_alc_get_thr_ix();
    int unlock;
    Allctr_t *allctr;

    ASSERT(ix > 0);
    if (ix < tspec->size) {
	allctr = tspec->allctr[ix];
	unlock = 0;
    }
    else {
	allctr = tspec->allctr[0];
	unlock = 1;
	erts_mtx_lock(&allctr->mutex);
    }

    do_erts_alcu_free(type, allctr, p);
    if (unlock)
	erts_mtx_unlock(&allctr->mutex);
}

void
erts_alcu_free_thr_pref(ErtsAlcType_t type, void *unused, void *p)
{
    if (p) {
	void *ptr = (void *) (((char *) p) - sizeof(UWord));
	Allctr_t *allctr = *((Allctr_t **) ptr);
	erts_mtx_lock(&allctr->mutex);
	do_erts_alcu_free(type, allctr, ptr);
	erts_mtx_unlock(&allctr->mutex);
    }
}

#endif

/* ------------------------------------------------------------------------- */

static ERTS_INLINE void *
do_erts_alcu_realloc(ErtsAlcType_t type,
		     void *extra,
		     void *p,
		     Uint size,
		     UWord flgs)
{
    Allctr_t *allctr = (Allctr_t *) extra; 
    Block_t *blk;
    void *res;

    ASSERT(initialized);

    ASSERT(allctr);

    ERTS_ALCU_DBG_CHK_THR_SPEC(allctr);

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

    if (size < allctr->sbc_threshold) {
	if (IS_MBC_BLK(blk))
	    res = mbc_realloc(allctr, p, size, flgs);
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
	    else if (flgs & ERTS_ALCU_FLG_FAIL_REALLOC_MOVE)
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
	if(IS_SBC_BLK(blk)) {
	do_carrier_resize:
#if HALFWORD_HEAP
	    new_blk = resize_carrier(allctr, blk, size, CFLG_SBC | CFLG_FORCE_MSEG);
#else
	    new_blk = resize_carrier(allctr, blk, size, CFLG_SBC);
#endif
	    res = new_blk ? BLK2UMEM(new_blk) : NULL;
	}
	else if (flgs & ERTS_ALCU_FLG_FAIL_REALLOC_MOVE)
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

void *
erts_alcu_realloc_thr_spec(ErtsAlcType_t type, void *extra,
			   void *ptr, Uint size)
{
    ErtsAllocatorThrSpec_t *tspec = (ErtsAllocatorThrSpec_t *) extra;
    int ix = erts_alc_get_thr_ix();
    Allctr_t *allctr;
    int unlock;
    void *res;

    ASSERT(ix > 0);
    if (ix < tspec->size) {
	allctr = tspec->allctr[ix];
	unlock = 0;
    }
    else {
	allctr = tspec->allctr[0];
	unlock = 1;
	erts_mtx_lock(&allctr->mutex);
    }

    res = do_erts_alcu_realloc(type, allctr, ptr, size, 0);

    if (unlock)
	erts_mtx_unlock(&allctr->mutex);

    DEBUG_CHECK_ALIGNMENT(res);

    return res;
}

void *
erts_alcu_realloc_mv_thr_spec(ErtsAlcType_t type, void *extra,
			      void *ptr, Uint size)
{
    ErtsAllocatorThrSpec_t *tspec = (ErtsAllocatorThrSpec_t *) extra;
    int ix = erts_alc_get_thr_ix();
    Allctr_t *allctr;
    int unlock;
    void *res;

    ASSERT(ix > 0);
    if (ix < tspec->size) {
	allctr = tspec->allctr[ix];
	unlock = 0;
    }
    else {
	allctr = tspec->allctr[0];
	unlock = 1;
	erts_mtx_lock(&allctr->mutex);
    }


    res = do_erts_alcu_alloc(type, allctr, size);
    if (!res) {
	if (unlock)
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
	if (unlock)
	    erts_mtx_unlock(&allctr->mutex);
    }

    DEBUG_CHECK_ALIGNMENT(res);

    return res;
}

void *
erts_alcu_realloc_thr_pref(ErtsAlcType_t type, void *extra, void *p, Uint size)
{
    ErtsAllocatorThrSpec_t *tspec = (ErtsAllocatorThrSpec_t *) extra;
    int ix;
    void *ptr, *res;
    Allctr_t *pref_allctr, *used_allctr;

    if (!p)
	return erts_alcu_alloc_thr_pref(type, extra, size);

    ptr = (void *) (((char *) p) - sizeof(UWord));
    used_allctr = *((Allctr_t **) ptr);

    ix = erts_alc_get_thr_ix();
    ASSERT(ix > 0);
    if (ix >= tspec->size)
	ix = (ix % (tspec->size - 1)) + 1;
    pref_allctr = tspec->allctr[ix];
    ASSERT(used_allctr && pref_allctr);

    erts_mtx_lock(&used_allctr->mutex);
    res = do_erts_alcu_realloc(type,
			       used_allctr,
			       ptr,
			       size + sizeof(UWord),
			       (pref_allctr != used_allctr
				? ERTS_ALCU_FLG_FAIL_REALLOC_MOVE
				: 0));
    erts_mtx_unlock(&used_allctr->mutex);
    if (res) {
	ASSERT(used_allctr == *((Allctr_t **) res));
	res = (void *) (((char *) res) + sizeof(UWord));
	DEBUG_CHECK_ALIGNMENT(res);
    }
    else {
	erts_mtx_lock(&pref_allctr->mutex);
	res = do_erts_alcu_alloc(type, pref_allctr, size + sizeof(UWord));
	erts_mtx_unlock(&pref_allctr->mutex);
	if (res) {
	    Block_t *blk;
	    size_t cpy_size;

	    *((Allctr_t **) res) = pref_allctr;
	    res = (void *) (((char *) res) + sizeof(UWord));

	    DEBUG_CHECK_ALIGNMENT(res);

	    erts_mtx_lock(&used_allctr->mutex);
	    blk = UMEM2BLK(ptr);
	    cpy_size = BLK_SZ(blk) - ABLK_HDR_SZ - sizeof(UWord);
	    if (cpy_size > size)
		cpy_size = size;
	    sys_memcpy(res, p, cpy_size);
	    do_erts_alcu_free(type, used_allctr, ptr);
	    erts_mtx_unlock(&used_allctr->mutex);
	}
    }

    return res;
}


void *
erts_alcu_realloc_mv_thr_pref(ErtsAlcType_t type, void *extra,
			      void *p, Uint size)
{
    ErtsAllocatorThrSpec_t *tspec = (ErtsAllocatorThrSpec_t *) extra;
    int ix;
    void *ptr, *res;
    Allctr_t *pref_allctr, *used_allctr;

    if (!p)
	return erts_alcu_alloc_thr_pref(type, extra, size);

    ptr = (void *) (((char *) p) - sizeof(UWord));
    used_allctr = *((Allctr_t **) ptr);

    ix = erts_alc_get_thr_ix();
    ASSERT(ix > 0);
    if (ix >= tspec->size)
	ix = (ix % (tspec->size - 1)) + 1;
    pref_allctr = tspec->allctr[ix];
    ASSERT(used_allctr && pref_allctr);

    erts_mtx_lock(&pref_allctr->mutex);
    res = do_erts_alcu_alloc(type, pref_allctr, size + sizeof(UWord));
    if (!res) {
	erts_mtx_unlock(&pref_allctr->mutex);
	res = erts_alcu_realloc_thr_pref(type, extra, p, size);
    }
    else {
	Block_t *blk;
	size_t cpy_size;
	Allctr_t *allctr;

	*((Allctr_t **) res) = pref_allctr;
	res = (void *) (((char *) res) + sizeof(UWord));

	DEBUG_CHECK_ALIGNMENT(res);

	if (used_allctr == pref_allctr)
	    allctr = pref_allctr;
	else {
	    erts_mtx_unlock(&pref_allctr->mutex);
	    allctr = used_allctr;
	    erts_mtx_lock(&allctr->mutex);
	}

	blk = UMEM2BLK(ptr);
	cpy_size = BLK_SZ(blk) - ABLK_HDR_SZ - sizeof(UWord);
	if (cpy_size > size)
	    cpy_size = size;
	sys_memcpy(res, p, cpy_size);
	do_erts_alcu_free(type, allctr, ptr);
	erts_mtx_unlock(&allctr->mutex);
    }

    return res;
}

#endif

/* ------------------------------------------------------------------------- */

int
erts_alcu_start(Allctr_t *allctr, AllctrInit_t *init)
{
    /* erts_alcu_start assumes that allctr has been zeroed */

    if (!initialized)
	goto error;

#if HAVE_ERTS_MSEG
    {
	ErtsMsegOpt_t mseg_opt = ERTS_MSEG_DEFAULT_OPT_INITIALIZER;
    
	sys_memcpy((void *) &allctr->mseg_opt,
		   (void *) &mseg_opt,
		   sizeof(ErtsMsegOpt_t));
    }
#endif

    allctr->name_prefix			= init->name_prefix;
    if (!allctr->name_prefix)
	goto error;

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

#if HAVE_ERTS_MSEG
    if (allctr->mseg_opt.abs_shrink_th > ~((UWord) 0) / 100)
	allctr->mseg_opt.abs_shrink_th = ~((UWord) 0) / 100;
#endif

#ifdef USE_THREADS
    if (init->ts) {
	allctr->thread_safe = 1;
	
#ifdef ERTS_ENABLE_LOCK_COUNT
	erts_mtx_init_x_opt(&allctr->mutex,
			"alcu_allocator",
			make_small(allctr->alloc_no),
			ERTS_LCNT_LT_ALLOC);
#else
	erts_mtx_init_x(&allctr->mutex,
			"alcu_allocator",
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
#ifdef USE_THREADS
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

	(*allctr->link_free_block)(allctr, blk);

	HARD_CHECK_BLK_CARRIER(allctr, blk);

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
		carrier_end = ((char *) NXT_BLK(blk)) + sizeof(UWord);
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
		+ tot_blk_sz
		+ sizeof(UWord)) == carrier_end);
	ASSERT(((char *) mbc) + CARRIER_SZ(mbc) == carrier_end);

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
}

#endif
