/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2002-2016. All Rights Reserved.
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
#include "lttng-wrapper.h"

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

#ifndef ERTS_MSEG_FLG_2POW
#  define ERTS_MSEG_FLG_2POW 0
#endif
#ifndef ERTS_MSEG_FLG_NONE
#  define ERTS_MSEG_FLG_NONE 0
#endif

static int atoms_initialized = 0;
static int initialized = 0;

#define INV_SYS_ALLOC_CARRIER_MASK	((UWord) (sys_alloc_carrier_size - 1))
#define SYS_ALLOC_CARRIER_MASK		(~INV_SYS_ALLOC_CARRIER_MASK)
#define SYS_ALLOC_CARRIER_FLOOR(X)	((X) & SYS_ALLOC_CARRIER_MASK)
#define SYS_ALLOC_CARRIER_CEILING(X) \
  SYS_ALLOC_CARRIER_FLOOR((X) + INV_SYS_ALLOC_CARRIER_MASK)

#if 0
/* Can be useful for debugging */
#define MBC_REALLOC_ALWAYS_MOVES
#endif


/* alloc_util global parameters */
static Uint sys_alloc_carrier_size;
#if HAVE_ERTS_MSEG
static Uint max_mseg_carriers;
#endif
static int allow_sys_alloc_carriers;

#define ONE_GIGA (1000000000)

#define ERTS_ALC_CC_GIGA_VAL(CC) ((CC) / ONE_GIGA)
#define ERTS_ALC_CC_VAL(CC) ((CC) % ONE_GIGA)

#define INC_CC(CC) ((CC)++)

#define DEC_CC(CC) ((CC)--)

/* Multi block carrier (MBC) memory layout in R16: 

Empty MBC:
[Carrier_t|pad|Block_t L0T|fhdr| free... ]

MBC after allocating first block:
[Carrier_t|pad|Block_t 000|        udata        |pad|Block_t L0T|fhdr| free... ]

MBC after allocating second block:
[Carrier_t|pad|Block_t 000|        udata        |pad|Block_t 000|   udata   |pad|Block_t L0T|fhdr| free... ]

MBC after deallocating first block:
[Carrier_t|pad|Block_t 00T|fhdr| free  |FreeBlkFtr_t|Block_t 0P0|   udata   |pad|Block_t L0T|fhdr| free... ]


    udata = Allocated user data
    pad   = Padding to ensure correct alignment for user data
    fhdr  = Allocator specific header to keep track of free block
    free  = Unused free memory
    T     = This block is free (THIS_FREE_BLK_HDR_FLG)
    P     = Previous block is free (PREV_FREE_BLK_HDR_FLG)
    L     = Last block in carrier (LAST_BLK_HDR_FLG)
*/

/* Single block carrier (SBC):
[Carrier_t|pad|Block_t 111| udata... ]
*/


/* Blocks ... */

#define UNUSED0_BLK_FTR_FLG	(((UWord) 1) << 0)
#define UNUSED1_BLK_FTR_FLG	(((UWord) 1) << 1)
#define UNUSED2_BLK_FTR_FLG	(((UWord) 1) << 2)

#if MBC_ABLK_OFFSET_BITS
#  define ABLK_HDR_SZ (offsetof(Block_t,u))
#else
#  define ABLK_HDR_SZ (sizeof(Block_t))
#endif
#define FBLK_FTR_SZ (sizeof(FreeBlkFtr_t))

#define UMEMSZ2BLKSZ(AP, SZ)						\
  (ABLK_HDR_SZ + (SZ) <= (AP)->min_block_size				\
   ? (AP)->min_block_size						\
   : UNIT_CEILING(ABLK_HDR_SZ + (SZ)))

#define UMEM2BLK(P) ((Block_t *) (((char *) (P)) - ABLK_HDR_SZ))
#define BLK2UMEM(P) ((void *)    (((char *) (P)) + ABLK_HDR_SZ))

#define PREV_BLK_SZ(B) 		((UWord) (((FreeBlkFtr_t *)(B))[-1]))

#define SET_BLK_SZ_FTR(B, SZ) \
  (((FreeBlkFtr_t *) (((char *) (B)) + (SZ)))[-1] = (SZ))

#define SET_MBC_ABLK_SZ(B, SZ) \
  (ASSERT(((SZ) & FLG_MASK) == 0), \
   (B)->bhdr = (((B)->bhdr) & ~MBC_ABLK_SZ_MASK) | (SZ))
#define SET_MBC_FBLK_SZ(B, SZ) \
  (ASSERT(((SZ) & FLG_MASK) == 0), \
   (B)->bhdr = (((B)->bhdr) & ~MBC_FBLK_SZ_MASK) | (SZ))
#define SET_SBC_BLK_SZ(B, SZ) \
  (ASSERT(((SZ) & FLG_MASK) == 0), \
   (B)->bhdr = (((B)->bhdr) & ~SBC_BLK_SZ_MASK) | (SZ))
#define SET_PREV_BLK_FREE(AP,B) \
  (ASSERT(!IS_MBC_FIRST_BLK(AP,B)), \
   ASSERT(!IS_FREE_BLK(B)), \
   (B)->bhdr |= PREV_FREE_BLK_HDR_FLG)
#define SET_PREV_BLK_ALLOCED(B) \
  ((B)->bhdr &= ~PREV_FREE_BLK_HDR_FLG)
#define SET_LAST_BLK(B) \
  ((B)->bhdr |= LAST_BLK_HDR_FLG)
#define SET_NOT_LAST_BLK(B) \
  ((B)->bhdr &= ~LAST_BLK_HDR_FLG)

#define SBH_THIS_FREE		THIS_FREE_BLK_HDR_FLG
#define SBH_PREV_FREE		PREV_FREE_BLK_HDR_FLG
#define SBH_LAST_BLK		LAST_BLK_HDR_FLG


#if MBC_ABLK_OFFSET_BITS

#  define MBC_SZ_MAX_LIMIT ((((UWord)1 << MBC_ABLK_OFFSET_BITS) - 1) << ERTS_SUPER_ALIGN_BITS)

#  define BLK_CARRIER_OFFSET(B, C) (((char*)(B) - (char*)(C)) >> ERTS_SACRR_UNIT_SHIFT)

#  define SET_MBC_ABLK_HDR(B, Sz, F, C) \
    (ASSERT(((Sz) & ~MBC_ABLK_SZ_MASK) == 0), \
     ASSERT(!((UWord)(F) & (~FLG_MASK|THIS_FREE_BLK_HDR_FLG))), \
     (B)->bhdr = ((Sz) | (F) | (BLK_CARRIER_OFFSET(B,C) << MBC_ABLK_OFFSET_SHIFT)))

#  define SET_MBC_FBLK_HDR(B, Sz, F, C) \
    (ASSERT(((Sz) & ~MBC_FBLK_SZ_MASK) == 0), \
     ASSERT(((UWord)(F) & (~FLG_MASK|THIS_FREE_BLK_HDR_FLG|PREV_FREE_BLK_HDR_FLG)) == THIS_FREE_BLK_HDR_FLG), \
     (B)->bhdr = ((Sz) | (F)), \
     (B)->u.carrier = (C))

#  define IS_MBC_FIRST_ABLK(AP,B) \
  ((((UWord)(B) & ~ERTS_SACRR_UNIT_MASK) == MBC_HEADER_SIZE(AP)) \
   && ((B)->bhdr & MBC_ABLK_OFFSET_MASK) == 0)

#  define IS_MBC_FIRST_FBLK(AP,B) \
  ((char*)(B) == (char*)((B)->u.carrier) + MBC_HEADER_SIZE(AP))

#  define IS_MBC_FIRST_BLK(AP,B) \
  (IS_FREE_BLK(B) ? IS_MBC_FIRST_FBLK(AP,B) : IS_MBC_FIRST_ABLK(AP,B))

#  define SET_BLK_FREE(B) \
  (ASSERT(!IS_PREV_BLK_FREE(B)), \
   (B)->u.carrier = ABLK_TO_MBC(B), \
   (B)->bhdr |= THIS_FREE_BLK_HDR_FLG, \
   (B)->bhdr &= (MBC_ABLK_SZ_MASK|FLG_MASK))

#  define SET_BLK_ALLOCED(B) \
  (ASSERT(((B)->bhdr & (MBC_ABLK_OFFSET_MASK|THIS_FREE_BLK_HDR_FLG)) == THIS_FREE_BLK_HDR_FLG), \
   (B)->bhdr &= ~THIS_FREE_BLK_HDR_FLG, \
   (B)->bhdr |= (BLK_CARRIER_OFFSET(B,(B)->u.carrier) << MBC_ABLK_OFFSET_SHIFT))

#else /* !MBC_ABLK_OFFSET_BITS */

#  define MBC_SZ_MAX_LIMIT ((UWord)~0)

#  define SET_MBC_ABLK_HDR(B, Sz, F, C) \
    (ASSERT(((Sz) & FLG_MASK) == 0), \
     ASSERT(!((UWord)(F) & (~FLG_MASK|THIS_FREE_BLK_HDR_FLG))), \
     ASSERT((UWord)(F) < SBC_BLK_HDR_FLG), \
     (B)->bhdr = ((Sz) | (F)), \
     (B)->carrier = (C))

#  define SET_MBC_FBLK_HDR(B, Sz, F, C) \
    (ASSERT(((Sz) & FLG_MASK) == 0), \
     ASSERT(((UWord)(F) & (~FLG_MASK|THIS_FREE_BLK_HDR_FLG|PREV_FREE_BLK_HDR_FLG)) == THIS_FREE_BLK_HDR_FLG), \
     (B)->bhdr = ((Sz) | (F)), \
     (B)->carrier = (C))

#  define IS_MBC_FIRST_BLK(AP,B) \
  ((char*)(B) == (char*)((B)->carrier) + MBC_HEADER_SIZE(AP))
#  define IS_MBC_FIRST_ABLK(AP,B) IS_MBC_FIRST_BLK(AP,B)
#  define IS_MBC_FIRST_FBLK(AP,B) IS_MBC_FIRST_BLK(AP,B)

#  define SET_BLK_FREE(B) \
  (ASSERT(!IS_PREV_BLK_FREE(B)), \
   (B)->bhdr |= THIS_FREE_BLK_HDR_FLG)

#  define SET_BLK_ALLOCED(B) \
  ((B)->bhdr &= ~THIS_FREE_BLK_HDR_FLG)

#endif /* !MBC_ABLK_OFFSET_BITS */

#define SET_SBC_BLK_HDR(B, Sz) \
  (ASSERT(((Sz) & FLG_MASK) == 0), (B)->bhdr = ((Sz) | (SBC_BLK_HDR_FLG)))


#define BLK_UMEM_SZ(B) \
  (BLK_SZ(B) - (ABLK_HDR_SZ))
#define IS_PREV_BLK_FREE(B) \
  ((B)->bhdr & PREV_FREE_BLK_HDR_FLG)
#define IS_PREV_BLK_ALLOCED(B) \
  (!IS_PREV_BLK_FREE((B)))
#define IS_ALLOCED_BLK(B) \
  (!IS_FREE_BLK((B)))  
#define IS_LAST_BLK(B) \
  ((B)->bhdr & LAST_BLK_HDR_FLG)
#define IS_NOT_LAST_BLK(B) \
  (!IS_LAST_BLK((B)))

#define GET_LAST_BLK_HDR_FLG(B) \
  ((B)->bhdr & LAST_BLK_HDR_FLG)
#define GET_THIS_FREE_BLK_HDR_FLG(B) \
  ((B)->bhdr & THIS_FREE_BLK_HDR_FLG)
#define GET_PREV_FREE_BLK_HDR_FLG(B) \
  ((B)->bhdr & PREV_FREE_BLK_HDR_FLG)
#define GET_BLK_HDR_FLGS(B) \
  ((B)->bhdr & FLG_MASK)

#define NXT_BLK(B) \
  (ASSERT(IS_MBC_BLK(B)), \
   (Block_t *) (((char *) (B)) + MBC_BLK_SZ((B))))
#define PREV_BLK(B) \
  ((Block_t *) (((char *) (B)) - PREV_BLK_SZ((B))))

#define BLK_AFTER(B,Sz) \
  ((Block_t *) (((char *) (B)) + (Sz)))

#define BLK_SZ(B) ((B)->bhdr & (((B)->bhdr & THIS_FREE_BLK_HDR_FLG) ? MBC_FBLK_SZ_MASK : MBC_ABLK_SZ_MASK))

/* Carriers ... */

/* #define ERTS_ALC_CPOOL_DEBUG */

#if defined(DEBUG) && !defined(ERTS_ALC_CPOOL_DEBUG)
#  define ERTS_ALC_CPOOL_DEBUG
#endif

#ifndef ERTS_SMP
#  undef ERTS_ALC_CPOOL_DEBUG
#endif

#ifdef ERTS_ALC_CPOOL_DEBUG
#  define ERTS_ALC_CPOOL_ASSERT(A)				\
    ((void) ((A)						\
	     ? 1						\
	     : (erts_alcu_assert_failed(#A,			\
					(char *) __FILE__,	\
					__LINE__,		\
					(char *) __func__),	\
		0)))
#else
#  define ERTS_ALC_CPOOL_ASSERT(A) ((void) 1)
#endif

#ifdef ERTS_SMP
#define ERTS_ALC_IS_CPOOL_ENABLED(A)	((A)->cpool.util_limit)
#else
#define ERTS_ALC_IS_CPOOL_ENABLED(A)	(0)
#endif

#ifdef ERTS_SMP

#define ERTS_ALC_CPOOL_MAX_DISABLE_ABANDON		1000
#define ERTS_ALC_CPOOL_ALLOC_OP_INC			8
#define ERTS_ALC_CPOOL_FREE_OP_DEC			10

#define ERTS_ALC_CPOOL_ALLOC_OP(A)						\
do {										\
    if ((A)->cpool.disable_abandon < ERTS_ALC_CPOOL_MAX_DISABLE_ABANDON) { 	\
	(A)->cpool.disable_abandon += ERTS_ALC_CPOOL_ALLOC_OP_INC;		\
	if ((A)->cpool.disable_abandon > ERTS_ALC_CPOOL_MAX_DISABLE_ABANDON)	\
	    (A)->cpool.disable_abandon = ERTS_ALC_CPOOL_MAX_DISABLE_ABANDON;	\
    }										\
} while (0)


#if ERTS_ALC_CPOOL_ALLOC_OP_INC >= ERTS_ALC_CPOOL_FREE_OP_DEC
#  error "Implementation assume ERTS_ALC_CPOOL_ALLOC_OP_INC < ERTS_ALC_CPOOL_FREE_OP_DEC"
#endif

#define ERTS_ALC_CPOOL_REALLOC_OP(A)						\
do {										\
    if ((A)->cpool.disable_abandon) {						\
	(A)->cpool.disable_abandon -= (ERTS_ALC_CPOOL_FREE_OP_DEC		\
				       - ERTS_ALC_CPOOL_ALLOC_OP_INC);		\
	if ((A)->cpool.disable_abandon < 0)					\
	    (A)->cpool.disable_abandon = 0;					\
    }										\
} while (0)

#define ERTS_ALC_CPOOL_FREE_OP(A)						\
do {										\
    if ((A)->cpool.disable_abandon) {						\
	(A)->cpool.disable_abandon -= ERTS_ALC_CPOOL_FREE_OP_DEC;		\
	if ((A)->cpool.disable_abandon < 0)					\
	    (A)->cpool.disable_abandon = 0;					\
    }										\
} while (0)

#else
#define ERTS_ALC_CPOOL_ALLOC_OP(A)
#define ERTS_ALC_CPOOL_REALLOC_OP(A)
#define ERTS_ALC_CPOOL_FREE_OP(A)
#endif

#define ERTS_CRR_ALCTR_FLG_IN_POOL	(((erts_aint_t) 1) << 0)
#define ERTS_CRR_ALCTR_FLG_BUSY		(((erts_aint_t) 1) << 1)
#define ERTS_CRR_ALCTR_FLG_MASK (ERTS_CRR_ALCTR_FLG_IN_POOL | \
                                 ERTS_CRR_ALCTR_FLG_BUSY)

#ifdef ERTS_SMP
#define SBC_HEADER_SIZE	   						\
    (UNIT_CEILING(offsetof(Carrier_t, cpool)                            \
	          + ABLK_HDR_SZ)	                                \
     - ABLK_HDR_SZ)
#else
#define SBC_HEADER_SIZE	   						\
    (UNIT_CEILING(sizeof(Carrier_t)					\
		  + ABLK_HDR_SZ)					\
     - ABLK_HDR_SZ)
#endif
#define MBC_HEADER_SIZE(AP) ((AP)->mbc_header_size)


#define MSEG_CARRIER_HDR_FLAG		(((UWord) 1) << 0)
#define SBC_CARRIER_HDR_FLAG		(((UWord) 1) << 1)

#define SCH_SYS_ALLOC			0
#define SCH_MSEG			MSEG_CARRIER_HDR_FLAG
#define SCH_MBC				0
#define SCH_SBC				SBC_CARRIER_HDR_FLAG

#define SET_CARRIER_HDR(C, Sz, F, AP) \
  (ASSERT(((Sz) & FLG_MASK) == 0), (C)->chdr = ((Sz) | (F)), \
   erts_smp_atomic_init_nob(&(C)->allctr, (erts_aint_t) (AP)))

#define BLK_TO_SBC(B) \
  ((Carrier_t *) (((char *) (B)) - SBC_HEADER_SIZE))
#define FIRST_BLK_TO_MBC(AP, B) \
  ((Carrier_t *) (((char *) (B)) - MBC_HEADER_SIZE(AP)))

#define MBC_TO_FIRST_BLK(AP, P) \
  ((Block_t *) (((char *) (P)) + MBC_HEADER_SIZE(AP)))
#define SBC2BLK(AP, P) \
  ((Block_t *) (((char *) (P)) + SBC_HEADER_SIZE))
#define SBC2UMEM(AP, P) \
  ((void *) (((char *) (P)) + (SBC_HEADER_SIZE + ABLK_HDR_SZ)))

#define IS_MSEG_CARRIER(C) \
  ((C)->chdr & MSEG_CARRIER_HDR_FLAG)
#define IS_SYS_ALLOC_CARRIER(C) \
  (!IS_MSEG_CARRIER((C)))
#define IS_SB_CARRIER(C) \
  ((C)->chdr & SBC_CARRIER_HDR_FLAG)
#define IS_MB_CARRIER(C) \
  (!IS_SB_CARRIER((C)))

#define SET_CARRIER_SZ(C, SZ) \
  (ASSERT(((SZ) & FLG_MASK) == 0), \
   ((C)->chdr = ((C)->chdr & FLG_MASK) | (SZ)))

#define CFLG_SBC				(1 << 0)
#define CFLG_MBC				(1 << 1)
#define CFLG_FORCE_MSEG				(1 << 2)
#define CFLG_FORCE_SYS_ALLOC			(1 << 3)
#define CFLG_FORCE_SIZE				(1 << 4)
#define CFLG_MAIN_CARRIER			(1 << 5)
#define CFLG_NO_CPOOL				(1 << 6)

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
	       && !(AP)->mbcs.curr.norm.sys_alloc.size));

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

#define STAT_MBC_CPOOL_FETCH(AP, CRR)					\
do {									\
    UWord csz__ = CARRIER_SZ((CRR));					\
    if (IS_MSEG_CARRIER((CRR)))						\
	STAT_MSEG_MBC_ALLOC((AP), csz__);				\
    else								\
	STAT_SYS_ALLOC_MBC_ALLOC((AP), csz__);				\
    (AP)->mbcs.blocks.curr.no += (CRR)->cpool.blocks;			\
    if ((AP)->mbcs.blocks.max.no < (AP)->mbcs.blocks.curr.no)		\
	(AP)->mbcs.blocks.max.no = (AP)->mbcs.blocks.curr.no;		\
    (AP)->mbcs.blocks.curr.size += (CRR)->cpool.blocks_size;		\
    if ((AP)->mbcs.blocks.max.size < (AP)->mbcs.blocks.curr.size)	\
	(AP)->mbcs.blocks.max.size = (AP)->mbcs.blocks.curr.size;	\
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

#define STAT_MBC_CPOOL_INSERT(AP, CRR)					\
do {									\
    UWord csz__ = CARRIER_SZ((CRR));					\
    if (IS_MSEG_CARRIER((CRR)))						\
	STAT_MSEG_MBC_FREE((AP), csz__);				\
    else								\
	STAT_SYS_ALLOC_MBC_FREE((AP), csz__);				\
    ERTS_ALC_CPOOL_ASSERT((AP)->mbcs.blocks.curr.no			\
			  >= (CRR)->cpool.blocks);			\
    (AP)->mbcs.blocks.curr.no -= (CRR)->cpool.blocks;			\
    ERTS_ALC_CPOOL_ASSERT((AP)->mbcs.blocks.curr.size 			\
			  >= (CRR)->cpool.blocks_size);			\
    (AP)->mbcs.blocks.curr.size -= (CRR)->cpool.blocks_size;		\
} while (0)

#ifdef ERTS_SMP
#define STAT_MBC_BLK_ALLOC_CRR(CRR, BSZ)				\
do {									\
    (CRR)->cpool.blocks++;						\
    (CRR)->cpool.blocks_size += (BSZ);					\
} while (0)
#else
#define STAT_MBC_BLK_ALLOC_CRR(CRR, BSZ) ((void) (CRR)) /* Get rid of warning */
#endif

#define STAT_MBC_BLK_ALLOC(AP, CRR, BSZ, FLGS)	       			\
do {									\
    CarriersStats_t *cstats__ = &(AP)->mbcs;			        \
    cstats__->blocks.curr.no++;						\
    if (cstats__->blocks.max.no < cstats__->blocks.curr.no)		\
	cstats__->blocks.max.no = cstats__->blocks.curr.no;		\
    cstats__->blocks.curr.size += (BSZ);				\
    if (cstats__->blocks.max.size < cstats__->blocks.curr.size)		\
	cstats__->blocks.max.size = cstats__->blocks.curr.size;		\
    STAT_MBC_BLK_ALLOC_CRR((CRR), (BSZ));				\
} while (0)

static ERTS_INLINE int
stat_cpool_mbc_blk_free(Allctr_t *allctr,
			Carrier_t *crr,
			Carrier_t **busy_pcrr_pp,
			UWord blksz)
{
#ifdef ERTS_SMP

    ERTS_ALC_CPOOL_ASSERT(crr->cpool.blocks > 0);
    crr->cpool.blocks--;
    ERTS_ALC_CPOOL_ASSERT(crr->cpool.blocks_size >= blksz);
    crr->cpool.blocks_size -= blksz;

    if (!busy_pcrr_pp || !*busy_pcrr_pp)
	return 0;

    ERTS_ALC_CPOOL_ASSERT(crr == *busy_pcrr_pp);

#ifdef ERTS_ALC_CPOOL_DEBUG
    ERTS_ALC_CPOOL_ASSERT(
	erts_atomic_dec_read_nob(&allctr->cpool.stat.no_blocks) >= 0);
    ERTS_ALC_CPOOL_ASSERT(
	erts_atomic_add_read_nob(&allctr->cpool.stat.blocks_size,
				 -((erts_aint_t) blksz)) >= 0);
#else
    erts_atomic_dec_nob(&allctr->cpool.stat.no_blocks);
    erts_atomic_add_nob(&allctr->cpool.stat.blocks_size,
			-((erts_aint_t) blksz));
#endif

    return 1;
#else
    return 0;
#endif
}

#define STAT_MBC_BLK_FREE(AP, CRR, BPCRRPP, BSZ, FLGS)			\
do {									\
    if (!stat_cpool_mbc_blk_free((AP), (CRR), (BPCRRPP), (BSZ))) {	\
	CarriersStats_t *cstats__ = &(AP)->mbcs;			\
	ASSERT(cstats__->blocks.curr.no > 0);				\
	cstats__->blocks.curr.no--;					\
	ASSERT(cstats__->blocks.curr.size >= (BSZ));			\
	cstats__->blocks.curr.size -= (BSZ);				\
    }									\
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
# ifdef ERTS_SMP
#  define IS_ACTUALLY_BLOCKING (erts_thr_progress_is_blocking())
# else
#  define IS_ACTUALLY_BLOCKING 0
# endif
#define ERTS_ALCU_DBG_CHK_THR_ACCESS(A)					\
do {									\
    if (!(A)->thread_safe && !IS_ACTUALLY_BLOCKING) {                   \
	if (!(A)->debug.saved_tid) {                                    \
	    (A)->debug.tid = erts_thr_self();				\
	    (A)->debug.saved_tid = 1;					\
	}								\
	else {								\
	    ERTS_SMP_LC_ASSERT(						\
		ethr_equal_tids((A)->debug.tid, erts_thr_self()));	\
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
static void destroy_carrier(Allctr_t *, Block_t *, Carrier_t **);
static void mbc_free(Allctr_t *allctr, void *p, Carrier_t **busy_pcrr_pp);
static void dealloc_block(Allctr_t *, void *, ErtsAlcFixList_t *, int);

/* internal data... */

#if 0

static ERTS_INLINE void *
internal_alloc(UWord size)
{
    void *res = erts_sys_alloc(0, NULL, size);
    if (!res)
	erts_alloc_enomem(ERTS_ALC_T_UNDEF, size);
    return res;
}

static ERTS_INLINE void *
internal_realloc(void *ptr, UWord size)
{
    void *res = erts_sys_realloc(0, NULL, ptr, size);
    if (!res)
	erts_alloc_enomem(ERTS_ALC_T_UNDEF, size);
    return res;
}

static ERTS_INLINE void
internal_free(void *ptr)
{
    erts_sys_free(0, NULL, ptr);
}

#endif

#ifdef ARCH_32

/*
 * Bit vector for the entire 32-bit virtual address space
 * with one bit for each super aligned memory segment.
 */

#define VSPACE_MAP_BITS  (1 << (32 - ERTS_MMAP_SUPERALIGNED_BITS))
#define VSPACE_MAP_SZ    (VSPACE_MAP_BITS / ERTS_VSPACE_WORD_BITS)

static ERTS_INLINE void set_bit(UWord* map, Uint ix)
{
    ASSERT(ix / ERTS_VSPACE_WORD_BITS < VSPACE_MAP_SZ);
    map[ix / ERTS_VSPACE_WORD_BITS]
        |= ((UWord)1 << (ix % ERTS_VSPACE_WORD_BITS));
}

static ERTS_INLINE void clr_bit(UWord* map, Uint ix)
{
    ASSERT(ix / ERTS_VSPACE_WORD_BITS < VSPACE_MAP_SZ);
    map[ix / ERTS_VSPACE_WORD_BITS]
        &= ~((UWord)1 << (ix % ERTS_VSPACE_WORD_BITS));
}

static ERTS_INLINE int is_bit_set(UWord* map, Uint ix)
{
    ASSERT(ix / ERTS_VSPACE_WORD_BITS < VSPACE_MAP_SZ);
    return map[ix / ERTS_VSPACE_WORD_BITS]
        & ((UWord)1 << (ix % ERTS_VSPACE_WORD_BITS));
}

UWord erts_literal_vspace_map[VSPACE_MAP_SZ];

static void set_literal_range(void* start, Uint size)
{
    Uint ix = (UWord)start >> ERTS_MMAP_SUPERALIGNED_BITS;
    Uint n = size >> ERTS_MMAP_SUPERALIGNED_BITS;

    ASSERT(!((UWord)start & ERTS_INV_SUPERALIGNED_MASK));
    ASSERT(!((UWord)size & ERTS_INV_SUPERALIGNED_MASK));
    ASSERT(n);
    while (n--) {
        ASSERT(!is_bit_set(erts_literal_vspace_map, ix));
        set_bit(erts_literal_vspace_map, ix);
        ix++;
    }
}

static void clear_literal_range(void* start, Uint size)
{
    Uint ix = (UWord)start >> ERTS_MMAP_SUPERALIGNED_BITS;
    Uint n = size >> ERTS_MMAP_SUPERALIGNED_BITS;

    ASSERT(!((UWord)start & ERTS_INV_SUPERALIGNED_MASK));
    ASSERT(!((UWord)size & ERTS_INV_SUPERALIGNED_MASK));
    ASSERT(n);
    while (n--) {
        ASSERT(is_bit_set(erts_literal_vspace_map, ix));
        clr_bit(erts_literal_vspace_map, ix);
        ix++;
    }
}

#endif /* ARCH_32 */

/* mseg ... */

#if HAVE_ERTS_MSEG

void*
erts_alcu_mseg_alloc(Allctr_t *allctr, Uint *size_p, Uint flags)
{
    void *res;
    UWord size = (UWord) *size_p;
    res = erts_mseg_alloc_opt(allctr->alloc_no, &size, flags, &allctr->mseg_opt);
    *size_p = (Uint) size;
    INC_CC(allctr->calls.mseg_alloc);
    return res;
}

void*
erts_alcu_mseg_realloc(Allctr_t *allctr, void *seg,
                       Uint old_size, Uint *new_size_p)
{
    void *res;
    UWord new_size = (UWord) *new_size_p;
    res = erts_mseg_realloc_opt(allctr->alloc_no, seg, (UWord) old_size, &new_size,
				ERTS_MSEG_FLG_NONE, &allctr->mseg_opt);
    *new_size_p = (Uint) new_size;
    INC_CC(allctr->calls.mseg_realloc);
    return res;
}

void
erts_alcu_mseg_dealloc(Allctr_t *allctr, void *seg, Uint size, Uint flags)
{
    erts_mseg_dealloc_opt(allctr->alloc_no, seg, (UWord) size, flags, &allctr->mseg_opt);
    INC_CC(allctr->calls.mseg_dealloc);
}


#if defined(ARCH_32)

void*
erts_alcu_literal_32_mseg_alloc(Allctr_t *allctr, Uint *size_p, Uint flags)
{
    void* res;
    Uint sz = ERTS_SUPERALIGNED_CEILING(*size_p);
    ERTS_LC_ASSERT(allctr->alloc_no == ERTS_ALC_A_LITERAL &&
                   allctr->t == 0);
    ERTS_SMP_LC_ASSERT(allctr->thread_safe);

    res = erts_alcu_mseg_alloc(allctr, &sz, flags);
    if (res) {
        set_literal_range(res, sz);
        *size_p = sz;
    }
    return res;
}

void*
erts_alcu_literal_32_mseg_realloc(Allctr_t *allctr, void *seg,
                                  Uint old_size, Uint *new_size_p)
{
    void* res;
    Uint new_sz = ERTS_SUPERALIGNED_CEILING(*new_size_p);
    ERTS_LC_ASSERT(allctr->alloc_no == ERTS_ALC_A_LITERAL &&
                   allctr->t == 0);
    ERTS_SMP_LC_ASSERT(allctr->thread_safe);

    if (seg && old_size)
        clear_literal_range(seg, old_size);
    res = erts_alcu_mseg_realloc(allctr, seg, old_size, &new_sz);
    if (res) {
        set_literal_range(res, new_sz);
        *new_size_p = new_sz;
    }
    return res;
}

void
erts_alcu_literal_32_mseg_dealloc(Allctr_t *allctr, void *seg, Uint size,
                               Uint flags)
{
    ERTS_LC_ASSERT(allctr->alloc_no == ERTS_ALC_A_LITERAL &&
                   allctr->t == 0);
    ERTS_SMP_LC_ASSERT(allctr->thread_safe);

    erts_alcu_mseg_dealloc(allctr, seg, size, flags);

    clear_literal_range(seg, size);
}

#elif defined(ARCH_64) && defined(ERTS_HAVE_OS_PHYSICAL_MEMORY_RESERVATION)

/* Used by literal allocator that has its own mmapper (super carrier) */
void*
erts_alcu_mmapper_mseg_alloc(Allctr_t *allctr, Uint *size_p, Uint flags)
{
    void* res;
    UWord size = (UWord) *size_p;
    Uint32 mmap_flags = ERTS_MMAPFLG_SUPERCARRIER_ONLY;
    if (flags & ERTS_MSEG_FLG_2POW)
        mmap_flags |= ERTS_MMAPFLG_SUPERALIGNED;

    res = erts_mmap(allctr->mseg_mmapper, mmap_flags, &size);
    *size_p = (Uint)size;
    INC_CC(allctr->calls.mseg_alloc);
    return res;
}

void*
erts_alcu_mmapper_mseg_realloc(Allctr_t *allctr, void *seg,
                               Uint old_size, Uint *new_size_p)
{
    void *res;
    UWord new_size = (UWord) *new_size_p;
    res = erts_mremap(allctr->mseg_mmapper, ERTS_MSEG_FLG_NONE, seg, old_size, &new_size);
    *new_size_p = (Uint) new_size;
    INC_CC(allctr->calls.mseg_realloc);
    return res;
}

void
erts_alcu_mmapper_mseg_dealloc(Allctr_t *allctr, void *seg, Uint size,
                               Uint flags)
{
    Uint32 mmap_flags = ERTS_MMAPFLG_SUPERCARRIER_ONLY;
    if (flags & ERTS_MSEG_FLG_2POW)
        mmap_flags |= ERTS_MMAPFLG_SUPERALIGNED;

    erts_munmap(allctr->mseg_mmapper, mmap_flags, seg, (UWord)size);
    INC_CC(allctr->calls.mseg_dealloc);
}
#endif /* ARCH_64 && ERTS_HAVE_OS_PHYSICAL_MEMORY_RESERVATION */

#endif /* HAVE_ERTS_MSEG */

void*
erts_alcu_sys_alloc(Allctr_t *allctr, Uint* size_p, int superalign)
{
    void *res;
    const Uint size = *size_p;
#if ERTS_SA_MB_CARRIERS && ERTS_HAVE_ERTS_SYS_ALIGNED_ALLOC
    if (superalign)
	res = erts_sys_aligned_alloc(ERTS_SACRR_UNIT_SZ, size);
    else
#endif
	res = erts_sys_alloc(0, NULL, size);
    INC_CC(allctr->calls.sys_alloc);
    if (erts_mtrace_enabled)
	erts_mtrace_crr_alloc(res, allctr->alloc_no, ERTS_ALC_A_SYSTEM, size);
    return res;
}

void*
erts_alcu_sys_realloc(Allctr_t *allctr, void *ptr, Uint *size_p, Uint old_size, int superalign)
{
    void *res;
    const Uint size = *size_p;

#if ERTS_SA_MB_CARRIERS && ERTS_HAVE_ERTS_SYS_ALIGNED_ALLOC
    if (superalign)
	res = erts_sys_aligned_realloc(ERTS_SACRR_UNIT_SZ, ptr, size, old_size);
    else
#endif
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

void
erts_alcu_sys_dealloc(Allctr_t *allctr, void *ptr, Uint size, int superalign)
{
#if ERTS_SA_MB_CARRIERS && ERTS_HAVE_ERTS_SYS_ALIGNED_ALLOC
    if (superalign)
	erts_sys_aligned_free(ERTS_SACRR_UNIT_SZ, ptr);
    else
#endif
	erts_sys_free(0, NULL, ptr);
    INC_CC(allctr->calls.sys_free);
    if (erts_mtrace_enabled)
	erts_mtrace_crr_free(allctr->alloc_no, ERTS_ALC_A_SYSTEM, ptr);
}

#ifdef ARCH_32

void*
erts_alcu_literal_32_sys_alloc(Allctr_t *allctr, Uint* size_p, int superalign)
{
    void* res;
    Uint size = ERTS_SUPERALIGNED_CEILING(*size_p);
    ERTS_LC_ASSERT(allctr->alloc_no == ERTS_ALC_A_LITERAL &&
                   allctr->t == 0);
    ERTS_SMP_LC_ASSERT(allctr->thread_safe);

    res = erts_alcu_sys_alloc(allctr, &size, 1);
    if (res) {
        set_literal_range(res, size);
        *size_p = size;
    }
    return res;
}

void*
erts_alcu_literal_32_sys_realloc(Allctr_t *allctr, void *ptr, Uint* size_p, Uint old_size, int superalign)
{
    void* res;
    Uint size = ERTS_SUPERALIGNED_CEILING(*size_p);

    ERTS_LC_ASSERT(allctr->alloc_no == ERTS_ALC_A_LITERAL &&
                   allctr->t == 0);
    ERTS_SMP_LC_ASSERT(allctr->thread_safe);

    if (ptr && old_size)
        clear_literal_range(ptr, old_size);
    res = erts_alcu_sys_realloc(allctr, ptr, &size, old_size, 1);
    if (res) {
        set_literal_range(res, size);
        *size_p = size;
    }
    return res;
}

void
erts_alcu_literal_32_sys_dealloc(Allctr_t *allctr, void *ptr, Uint size, int superalign)
{
    ERTS_LC_ASSERT(allctr->alloc_no == ERTS_ALC_A_LITERAL &&
                   allctr->t == 0);
    ERTS_SMP_LC_ASSERT(allctr->thread_safe);

    erts_alcu_sys_dealloc(allctr, ptr, size, 1);

    clear_literal_range(ptr, size);
}

#endif /* ARCH_32 */

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

#ifdef ERTS_SMP

#ifdef DEBUG
static int is_in_list(ErtsDoubleLink_t* sentinel, ErtsDoubleLink_t* node)
{
    ErtsDoubleLink_t* p;

    ASSERT(node != sentinel);
    for (p = sentinel->next; p != sentinel; p = p->next) {
	if (p == node)
	    return 1;
    }
    return 0;
}
#endif /* DEBUG */

static ERTS_INLINE void
link_edl_after(ErtsDoubleLink_t* after_me, ErtsDoubleLink_t* node)
{
    ErtsDoubleLink_t* before_me = after_me->next;
    ASSERT(node != after_me && node != before_me);
    node->next = before_me;
    node->prev = after_me;
    before_me->prev = node;
    after_me->next = node;
}

static ERTS_INLINE void
link_edl_before(ErtsDoubleLink_t* before_me, ErtsDoubleLink_t* node)
{
    ErtsDoubleLink_t* after_me = before_me->prev;
    ASSERT(node != before_me && node != after_me);
    node->next = before_me;
    node->prev = after_me;
    before_me->prev = node;
    after_me->next = node;
}

static ERTS_INLINE void
unlink_edl(ErtsDoubleLink_t* node)
{
    node->next->prev = node->prev;
    node->prev->next = node->next;
}

static ERTS_INLINE void
relink_edl_before(ErtsDoubleLink_t* before_me, ErtsDoubleLink_t* node)
{
    if (node != before_me && node != before_me->prev) {
	unlink_edl(node);
	link_edl_before(before_me, node);
    }
}

static ERTS_INLINE int is_abandoned(Carrier_t *crr)
{
    return crr->cpool.abandoned.next != NULL;
}

static ERTS_INLINE void
link_abandoned_carrier(ErtsDoubleLink_t* list, Carrier_t *crr)
{
    ASSERT(!is_abandoned(crr));

    link_edl_after(list, &crr->cpool.abandoned);

    ASSERT(crr->cpool.abandoned.next != &crr->cpool.abandoned);
    ASSERT(crr->cpool.abandoned.prev != &crr->cpool.abandoned);
}

static ERTS_INLINE void
unlink_abandoned_carrier(Carrier_t *crr)
{
    ASSERT(is_in_list(&crr->cpool.orig_allctr->cpool.pooled_list,
		      &crr->cpool.abandoned) ||
	   is_in_list(&crr->cpool.orig_allctr->cpool.traitor_list,
		      &crr->cpool.abandoned));

    unlink_edl(&crr->cpool.abandoned);

    crr->cpool.abandoned.next = NULL;
    crr->cpool.abandoned.prev = NULL;
}

static ERTS_INLINE void
clear_busy_pool_carrier(Allctr_t *allctr, Carrier_t *crr)
{
    if (crr) {
	erts_aint_t max_size;
	erts_aint_t new_val;

	max_size = (erts_aint_t) allctr->largest_fblk_in_mbc(allctr, crr);
	erts_atomic_set_nob(&crr->cpool.max_size, max_size);

	new_val = (((erts_aint_t) allctr)|ERTS_CRR_ALCTR_FLG_IN_POOL);

#ifdef ERTS_ALC_CPOOL_DEBUG
	{
	    erts_aint_t old_val = new_val|ERTS_CRR_ALCTR_FLG_BUSY;

	    ERTS_ALC_CPOOL_ASSERT(old_val
				  == erts_smp_atomic_xchg_relb(&crr->allctr,
							       new_val));
	}
#else
	erts_smp_atomic_set_relb(&crr->allctr, new_val);
#endif
    }
}

#endif /* ERTS_SMP */

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

static void *mbc_alloc(Allctr_t *allctr, Uint size);

#ifdef ERTS_SMP
typedef struct {
    ErtsAllctrDDBlock_t ddblock__; /* must be first */
    ErtsAlcType_t fix_type;
} ErtsAllctrFixDDBlock_t;
#endif

#define ERTS_ALC_FIX_NO_UNUSE (((ErtsAlcType_t) 1) << ERTS_ALC_N_BITS)

static ERTS_INLINE void
dealloc_fix_block(Allctr_t *allctr,
		  ErtsAlcType_t type,
		  void *ptr,
		  ErtsAlcFixList_t *fix,
		  int dec_cc_on_redirect)
{
#ifdef ERTS_SMP
    /* May be redirected... */
    ASSERT((type & ERTS_ALC_FIX_NO_UNUSE) == 0);
    ((ErtsAllctrFixDDBlock_t *) ptr)->fix_type = type | ERTS_ALC_FIX_NO_UNUSE;
#endif
    dealloc_block(allctr, ptr, fix, dec_cc_on_redirect);
}

static ERTS_INLINE void
sched_fix_shrink(Allctr_t *allctr, int on)
{
    if (on && !allctr->fix_shrink_scheduled) {
	allctr->fix_shrink_scheduled = 1;
	erts_set_aux_work_timeout(allctr->ix,
				  (ERTS_SSI_AUX_WORK_FIX_ALLOC_LOWER_LIM
				   | ERTS_SSI_AUX_WORK_FIX_ALLOC_DEALLOC),
				  1);
    }
    else if (!on && allctr->fix_shrink_scheduled) {
	allctr->fix_shrink_scheduled = 0;
	erts_set_aux_work_timeout(allctr->ix,
				  (ERTS_SSI_AUX_WORK_FIX_ALLOC_LOWER_LIM
				   | ERTS_SSI_AUX_WORK_FIX_ALLOC_DEALLOC),
				  0);
    }
}

static ERTS_INLINE void
fix_cpool_check_shrink(Allctr_t *allctr,
		       ErtsAlcType_t type,
		       ErtsAlcFixList_t *fix,
		       Carrier_t **busy_pcrr_pp)
{
    if (fix->u.cpool.shrink_list > 0) {
	if (fix->list_size == 0)
	    fix->u.cpool.shrink_list = 0;
	else {
	    void *p;
#ifdef ERTS_SMP
	    if (busy_pcrr_pp) {
		clear_busy_pool_carrier(allctr, *busy_pcrr_pp);
		*busy_pcrr_pp = NULL;
	    }
#endif
	    fix->u.cpool.shrink_list--;
	    p = fix->list;
	    fix->list = *((void **) p);
	    fix->list_size--;
	    if (fix->u.cpool.min_list_size > fix->list_size)
		fix->u.cpool.min_list_size = fix->list_size;

	    dealloc_fix_block(allctr, type, p, fix, 0);
	}
    }
}

static ERTS_INLINE void *
fix_cpool_alloc(Allctr_t *allctr, ErtsAlcType_t type, Uint size)
{
    void *res;
    ErtsAlcFixList_t *fix;

    ASSERT(ERTS_ALC_N_MIN_A_FIXED_SIZE <= type
	   && type <= ERTS_ALC_N_MAX_A_FIXED_SIZE);

    fix = &allctr->fix[type - ERTS_ALC_N_MIN_A_FIXED_SIZE];

    res = fix->list;
    if (res) {
	fix->list = *((void **) res);
	fix->list_size--;
	if (fix->u.cpool.min_list_size > fix->list_size)
	    fix->u.cpool.min_list_size = fix->list_size;
	fix->u.cpool.used++;
	fix_cpool_check_shrink(allctr, type, fix, NULL);
	return res;
    }
    if (size < 2*sizeof(UWord))
	size += sizeof(UWord);
    if (size >= allctr->sbc_threshold) {
	Block_t *blk;
	blk = create_carrier(allctr, size, CFLG_SBC);
	res = blk ? BLK2UMEM(blk) : NULL;
    }
    else
	res = mbc_alloc(allctr, size);
    if (res) {
	fix->u.cpool.used++;
	fix->u.cpool.allocated++;
    }
    return res;
}

static ERTS_INLINE void
fix_cpool_free(Allctr_t *allctr,
	       ErtsAlcType_t type,
	       void *p,
	       Carrier_t **busy_pcrr_pp,
	       int unuse)
{
    ErtsAlcFixList_t *fix;

    ASSERT(ERTS_ALC_N_MIN_A_FIXED_SIZE <= type
	   && type <= ERTS_ALC_N_MAX_A_FIXED_SIZE);

    fix = &allctr->fix[type - ERTS_ALC_N_MIN_A_FIXED_SIZE];

    if (unuse)
	fix->u.cpool.used--;

    if ((!busy_pcrr_pp || !*busy_pcrr_pp)
	&& !fix->u.cpool.shrink_list
	&& fix->list_size < ERTS_ALCU_FIX_MAX_LIST_SZ) {
	*((void **) p) = fix->list;
	fix->list = p;
	fix->list_size++;
	sched_fix_shrink(allctr, 1);
    }
    else {
	Block_t *blk = UMEM2BLK(p);
	if (IS_SBC_BLK(blk))
	    destroy_carrier(allctr, blk, NULL);
	else
	    mbc_free(allctr, p, busy_pcrr_pp);
	fix->u.cpool.allocated--;
	fix_cpool_check_shrink(allctr, type, fix, busy_pcrr_pp);
    }
}

static ERTS_INLINE erts_aint32_t
fix_cpool_alloc_shrink(Allctr_t *allctr, erts_aint32_t flgs)
{
    int all_empty = 1;
    erts_aint32_t res = 0;
    int ix, o;
    int flush = flgs == 0;

#ifdef USE_THREADS
    if (allctr->thread_safe)
	erts_mtx_lock(&allctr->mutex);
#endif

    for (ix = 0; ix < ERTS_ALC_NO_FIXED_SIZES; ix++) {
	ErtsAlcFixList_t *fix = &allctr->fix[ix];
	ErtsAlcType_t type;
	ERTS_DBG_CHK_FIX_LIST(allctr, fix, ix, 1);
	if (flush)
	    fix->u.cpool.shrink_list = fix->list_size;
	else if (flgs & ERTS_SSI_AUX_WORK_FIX_ALLOC_LOWER_LIM) {
	    fix->u.cpool.shrink_list = fix->u.cpool.min_list_size;
	    fix->u.cpool.min_list_size = fix->list_size;
	}
	type = (ErtsAlcType_t) (ix + ERTS_ALC_N_MIN_A_FIXED_SIZE);
	for (o = 0; o < ERTS_ALC_FIX_MAX_SHRINK_OPS || flush; o++) {
	    void *ptr;

	    if (fix->u.cpool.shrink_list == 0)
		break;
	    if (fix->list_size == 0) {
		fix->u.cpool.shrink_list = 0;
		break;
	    }
	    ptr = fix->list;
	    fix->list = *((void **) ptr);
	    fix->list_size--;
	    fix->u.cpool.shrink_list--;
	    dealloc_fix_block(allctr, type, ptr, fix, 0);
	}
	if (fix->u.cpool.min_list_size > fix->list_size)
	    fix->u.cpool.min_list_size = fix->list_size;
	if (fix->list_size != 0) {
	    if (fix->u.cpool.shrink_list > 0)
		res |= ERTS_SSI_AUX_WORK_FIX_ALLOC_DEALLOC;
	    all_empty = 0;
	}
    }

    if (all_empty)
	sched_fix_shrink(allctr, 0);

#ifdef USE_THREADS
    if (allctr->thread_safe)
	erts_mtx_unlock(&allctr->mutex);
#endif

    return res;
}

static ERTS_INLINE void *
fix_nocpool_alloc(Allctr_t *allctr, ErtsAlcType_t type, Uint size)
{
    ErtsAlcFixList_t *fix;
    void *res;

    ASSERT(ERTS_ALC_N_MIN_A_FIXED_SIZE <= type
	   && type <= ERTS_ALC_N_MAX_A_FIXED_SIZE);

    fix = &allctr->fix[type - ERTS_ALC_N_MIN_A_FIXED_SIZE];

    ERTS_DBG_CHK_FIX_LIST(allctr, fix, ix, 1);
    fix->u.nocpool.used++;
    res = fix->list;
    if (res) {
	fix->list_size--;
	fix->list = *((void **) res);
	if (fix->list && fix->u.nocpool.allocated > fix->u.nocpool.limit) {
	    Block_t *blk;
	    void *p = fix->list;
	    fix->list = *((void **) p);
	    fix->list_size--;
	    blk = UMEM2BLK(p);
	    if (IS_SBC_BLK(blk))
		destroy_carrier(allctr, blk, NULL);
	    else
		mbc_free(allctr, p, NULL);
	    fix->u.nocpool.allocated--;
	}
	ERTS_DBG_CHK_FIX_LIST(allctr, fix, ix, 0);
	return res;
    }
    if (size < 2*sizeof(UWord))
	size += sizeof(UWord);
    if (fix->u.nocpool.limit < fix->u.nocpool.used)
	fix->u.nocpool.limit = fix->u.nocpool.used;
    if (fix->u.nocpool.max_used < fix->u.nocpool.used)
	fix->u.nocpool.max_used = fix->u.nocpool.used;
    fix->u.nocpool.allocated++;

    if (size >= allctr->sbc_threshold) {
	Block_t *blk;
	blk = create_carrier(allctr, size, CFLG_SBC);
	res = blk ? BLK2UMEM(blk) : NULL;
    }
    else
	res = mbc_alloc(allctr, size);

    if (!res) {
	fix->u.nocpool.allocated--;
	fix->u.nocpool.used--;
    }
    return res;
}

static ERTS_INLINE void
fix_nocpool_free(Allctr_t *allctr,
		 ErtsAlcType_t type,
		 void *p)
{
    Block_t *blk;
    ErtsAlcFixList_t *fix;

    ASSERT(ERTS_ALC_N_MIN_A_FIXED_SIZE <= type
	   && type <= ERTS_ALC_N_MAX_A_FIXED_SIZE);

    fix = &allctr->fix[type - ERTS_ALC_N_MIN_A_FIXED_SIZE];

    ERTS_DBG_CHK_FIX_LIST(allctr, fix, ix, 1);
    fix->u.nocpool.used--;
    if (fix->u.nocpool.allocated < fix->u.nocpool.limit
	&& fix->list_size < ERTS_ALCU_FIX_MAX_LIST_SZ) {
	*((void **) p) = fix->list;
	fix->list = p;
	fix->list_size++;
	sched_fix_shrink(allctr, 1);
	ERTS_DBG_CHK_FIX_LIST(allctr, fix, ix, 0);
	return;
    }
    fix->u.nocpool.allocated--;
    if (fix->list && fix->u.nocpool.allocated > fix->u.nocpool.limit) {
	blk = UMEM2BLK(p);
	if (IS_SBC_BLK(blk))
	    destroy_carrier(allctr, blk, NULL);
	else
	    mbc_free(allctr, p, NULL);
	p = fix->list;
	fix->list = *((void **) p);
	fix->list_size--;
	fix->u.nocpool.allocated--;
    }

    blk = UMEM2BLK(p);
    if (IS_SBC_BLK(blk))
	destroy_carrier(allctr, blk, NULL);
    else
	mbc_free(allctr, p, NULL);
    ERTS_DBG_CHK_FIX_LIST(allctr, fix, ix, 0);
}

static ERTS_INLINE erts_aint32_t
fix_nocpool_alloc_shrink(Allctr_t *allctr, erts_aint32_t flgs)
{
    int all_empty = 1;
    erts_aint32_t res = 0;
    int ix, o;
    int flush = flgs == 0;

#ifdef USE_THREADS
    if (allctr->thread_safe)
	erts_mtx_lock(&allctr->mutex);
#endif

    for (ix = 0; ix < ERTS_ALC_NO_FIXED_SIZES; ix++) {
	ErtsAlcFixList_t *fix = &allctr->fix[ix];
	ERTS_DBG_CHK_FIX_LIST(allctr, fix, ix, 1);
	if (flgs & ERTS_SSI_AUX_WORK_FIX_ALLOC_LOWER_LIM) {
	    fix->u.nocpool.limit = fix->u.nocpool.max_used;
	    if (fix->u.nocpool.limit < fix->u.nocpool.used)
		fix->u.nocpool.limit = fix->u.nocpool.used;
	    fix->u.nocpool.max_used = fix->u.nocpool.used;
	    ASSERT(fix->u.nocpool.limit >= 0);

	}
	if (flush) {
	    fix->u.nocpool.limit = 0;
	    fix->u.nocpool.max_used = fix->u.nocpool.used;
	    ASSERT(fix->u.nocpool.limit >= 0);
	}
	for (o = 0; o < ERTS_ALC_FIX_MAX_SHRINK_OPS || flush; o++) {
	    void *ptr;

	    if (!flush && fix->u.nocpool.limit >= fix->u.nocpool.allocated)
		break;
	    if (fix->list_size == 0)
		break;
	    ptr = fix->list;
	    fix->list = *((void **) ptr);
	    fix->list_size--;
	    dealloc_block(allctr, ptr, NULL, 0);
	    fix->u.nocpool.allocated--;
	}
	if (fix->list_size != 0) {
	    if (fix->u.nocpool.limit < fix->u.nocpool.allocated)
		res |= ERTS_SSI_AUX_WORK_FIX_ALLOC_DEALLOC;
	    all_empty = 0;
	}
	ERTS_DBG_CHK_FIX_LIST(allctr, fix, ix, 0);
    }

    if (all_empty)
	sched_fix_shrink(allctr, 0);

#ifdef USE_THREADS
    if (allctr->thread_safe)
	erts_mtx_unlock(&allctr->mutex);
#endif

    return res;
}

erts_aint32_t
erts_alcu_fix_alloc_shrink(Allctr_t *allctr, erts_aint32_t flgs)
{
    if (ERTS_ALC_IS_CPOOL_ENABLED(allctr))
	return fix_cpool_alloc_shrink(allctr, flgs);
    else
	return fix_nocpool_alloc_shrink(allctr, flgs);
}

static void dealloc_carrier(Allctr_t *allctr, Carrier_t *crr, int superaligned);

static ERTS_INLINE void
dealloc_mbc(Allctr_t *allctr, Carrier_t *crr)
{
    ASSERT(IS_MB_CARRIER(crr));
    if (allctr->destroying_mbc)
        allctr->destroying_mbc(allctr, crr);

    dealloc_carrier(allctr, crr, 1);
}

#ifdef ERTS_SMP

static ERTS_INLINE Allctr_t*
get_pref_allctr(void *extra)
{
    ErtsAllocatorThrSpec_t *tspec = (ErtsAllocatorThrSpec_t *) extra;
    int pref_ix;

    pref_ix = ERTS_ALC_GET_THR_IX();

    ERTS_CT_ASSERT(sizeof(UWord) == sizeof(Allctr_t *));
    ASSERT(0 <= pref_ix && pref_ix < tspec->size);

    return tspec->allctr[pref_ix];
}

#define ERTS_ALC_TS_PREF_LOCK_IF_USED	(1)
#define ERTS_ALC_TS_PREF_LOCK_NO	(0)

/* SMP note:
 * get_used_allctr() must be safe WITHOUT locking the allocator while
 * concurrent threads may be updating adjacent blocks.
 * We rely on getting a consistent result (without atomic op) when reading
 * the block header word even if a concurrent thread is updating
 * the "PREV_FREE" flag bit. 
 */
static ERTS_INLINE Allctr_t*
get_used_allctr(Allctr_t *pref_allctr, int pref_lock, void *p, UWord *sizep,
		Carrier_t **busy_pcrr_pp)
{
    Block_t* blk = UMEM2BLK(p);
    Carrier_t *crr;
    erts_aint_t iallctr;
    Allctr_t *used_allctr;

    *busy_pcrr_pp = NULL;

    if (IS_SBC_BLK(blk)) {
	crr = BLK_TO_SBC(blk);
	if (sizep)
	    *sizep = SBC_BLK_SZ(blk) - ABLK_HDR_SZ;  
	iallctr = erts_smp_atomic_read_dirty(&crr->allctr);
    }
    else {
	crr = ABLK_TO_MBC(blk);

	if (sizep)
	    *sizep = MBC_ABLK_SZ(blk) - ABLK_HDR_SZ;
	if (!ERTS_ALC_IS_CPOOL_ENABLED(pref_allctr))
	    iallctr = erts_smp_atomic_read_dirty(&crr->allctr);
	else {
	    int locked_pref_allctr = 0;
	    iallctr = erts_smp_atomic_read_ddrb(&crr->allctr);

	    if (ERTS_ALC_TS_PREF_LOCK_IF_USED == pref_lock
		&& pref_allctr->thread_safe) {
		used_allctr = (Allctr_t *) (iallctr & ~ERTS_CRR_ALCTR_FLG_MASK);
		if (pref_allctr == used_allctr) {
		    erts_mtx_lock(&pref_allctr->mutex);
		    locked_pref_allctr = 1;
		}
	    }

	    while ((iallctr & ((~ERTS_CRR_ALCTR_FLG_MASK)|ERTS_CRR_ALCTR_FLG_IN_POOL))
		   == (((erts_aint_t) pref_allctr)|ERTS_CRR_ALCTR_FLG_IN_POOL)) {
		erts_aint_t act;

		ERTS_ALC_CPOOL_ASSERT(!(iallctr & ERTS_CRR_ALCTR_FLG_BUSY));
		act = erts_smp_atomic_cmpxchg_ddrb(&crr->allctr,
						   iallctr|ERTS_CRR_ALCTR_FLG_BUSY,
						   iallctr);
		if (act == iallctr) {
		    *busy_pcrr_pp = crr;
		    break;
		}
		iallctr = act;
	    }

	    used_allctr = (Allctr_t *) (iallctr & ~ERTS_CRR_ALCTR_FLG_MASK);

	    if (ERTS_ALC_TS_PREF_LOCK_IF_USED == pref_lock) {
		if (locked_pref_allctr && used_allctr != pref_allctr) {
		    /* Was taken out of pool; now owned by someone else */
		    erts_mtx_unlock(&pref_allctr->mutex);
		}
	    }

	    ERTS_ALC_CPOOL_ASSERT(
		(((iallctr & ~ERTS_CRR_ALCTR_FLG_MASK) == (erts_aint_t) pref_allctr)
		 ? (((iallctr & ERTS_CRR_ALCTR_FLG_MASK) == ERTS_CRR_ALCTR_FLG_IN_POOL)
		    || ((iallctr & ERTS_CRR_ALCTR_FLG_MASK) == 0))
		 : 1));

	    return used_allctr;
	}
    }

    used_allctr = (Allctr_t *) (iallctr & ~ERTS_CRR_ALCTR_FLG_MASK);

    if (ERTS_ALC_TS_PREF_LOCK_IF_USED == pref_lock
	&& used_allctr == pref_allctr
	&& pref_allctr->thread_safe) {
	erts_mtx_lock(&pref_allctr->mutex);
    }

    return used_allctr;
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

static ERTS_INLINE int
ddq_managed_thread_enqueue(ErtsAllctrDDQueue_t *ddq, void *ptr, int cinit)
{
    erts_aint_t itmp;
    ErtsAllctrDDBlock_t *enq, *this = ptr;

    erts_atomic_init_nob(&this->atmc_next, ERTS_AINT_NULL);
    /* Enqueue at end of list... */

    enq = (ErtsAllctrDDBlock_t *) erts_atomic_read_nob(&ddq->tail.data.last);
    itmp = erts_atomic_cmpxchg_relb(&enq->atmc_next,
				    (erts_aint_t) this,
				    ERTS_AINT_NULL);
    if (itmp == ERTS_AINT_NULL) {
	/* We are required to move last pointer */
#ifdef DEBUG
	ASSERT(ERTS_AINT_NULL == erts_atomic_read_nob(&this->atmc_next));
	ASSERT(((erts_aint_t) enq)
	       == erts_atomic_xchg_relb(&ddq->tail.data.last,
					(erts_aint_t) this));
#else
	erts_atomic_set_relb(&ddq->tail.data.last, (erts_aint_t) this);
#endif
	return 1;
    }
    else {
	/*
	 * We *need* to insert element somewhere in between the
	 * last element we read earlier and the actual last element.
	 */
	int i = cinit;

	while (1) {
	    erts_aint_t itmp2;
	    erts_atomic_set_nob(&this->atmc_next, itmp);
	    itmp2 = erts_atomic_cmpxchg_relb(&enq->atmc_next,
					     (erts_aint_t) this,
					     itmp);
	    if (itmp == itmp2)
		return 0; /* inserted this */
	    if ((i & 1) == 0)
		itmp = itmp2;
	    else {
		enq = (ErtsAllctrDDBlock_t *) itmp2;
		itmp = erts_atomic_read_acqb(&enq->atmc_next);
		ASSERT(itmp != ERTS_AINT_NULL);
	    }
	    i++;
	}
    }
}

static ERTS_INLINE erts_aint_t
check_insert_marker(ErtsAllctrDDQueue_t *ddq, erts_aint_t ilast)
{
    if (!ddq->head.used_marker
	&& ddq->head.unref_end == (ErtsAllctrDDBlock_t *) ilast) {
	erts_aint_t itmp;
	ErtsAllctrDDBlock_t *last = (ErtsAllctrDDBlock_t *) ilast;

	erts_atomic_init_nob(&ddq->tail.data.marker.atmc_next, ERTS_AINT_NULL);
	itmp = erts_atomic_cmpxchg_relb(&last->atmc_next,
					(erts_aint_t) &ddq->tail.data.marker,
					ERTS_AINT_NULL);
	if (itmp == ERTS_AINT_NULL) {
	    ilast = (erts_aint_t) &ddq->tail.data.marker;
	    ddq->head.used_marker = !0;
	    erts_atomic_set_relb(&ddq->tail.data.last, ilast);
	}
    }
    return ilast;
}

static ERTS_INLINE int
ddq_enqueue(ErtsAllctrDDQueue_t *ddq, void *ptr, int cinit)
{
    int last_elem;
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

    last_elem = ddq_managed_thread_enqueue(ddq, ptr, cinit);

    if (!managed_thread)
	erts_atomic_dec_relb(&ddq->tail.data.um_refc[um_refc_ix]);
    return last_elem;
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
	if (erts_atomic_read_nob(&ddq->tail.data.um_refc[um_refc_ix]) == 0) {
	    /* Move unreferenced end pointer forward... */

	    ETHR_MEMBAR(ETHR_LoadLoad|ETHR_LoadStore);

	    ddq->head.unref_end = ddq->head.next.unref_end;

	    ilast = check_insert_marker(ddq, ilast);

	    if (ddq->head.unref_end != (ErtsAllctrDDBlock_t *) ilast) {
		ddq->head.next.unref_end = (ErtsAllctrDDBlock_t *) ilast;
		ddq->head.next.thr_progress = erts_thr_progress_later(NULL);
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

static void
check_pending_dealloc_carrier(Allctr_t *allctr,
			      int *need_thr_progress,
			      ErtsThrPrgrVal *thr_prgr_p,
			      int *need_more_work);

static void
handle_delayed_fix_dealloc(Allctr_t *allctr, void *ptr)
{
    ErtsAlcType_t type;

    type = ((ErtsAllctrFixDDBlock_t *) ptr)->fix_type;

    ASSERT(ERTS_ALC_N_MIN_A_FIXED_SIZE
	   <= (type & ~ERTS_ALC_FIX_NO_UNUSE));
    ASSERT((type & ~ERTS_ALC_FIX_NO_UNUSE)
	   <= ERTS_ALC_N_MAX_A_FIXED_SIZE);

    if (!ERTS_ALC_IS_CPOOL_ENABLED(allctr))
	fix_nocpool_free(allctr, (type & ~ERTS_ALC_FIX_NO_UNUSE), ptr);
    else {
	Block_t *blk = UMEM2BLK(ptr);
	Carrier_t *busy_pcrr_p;
	Allctr_t *used_allctr;

	if (IS_SBC_BLK(blk)) {
	    busy_pcrr_p = NULL;
	    goto doit;
	}

	used_allctr = get_used_allctr(allctr, ERTS_ALC_TS_PREF_LOCK_NO, ptr,
				      NULL, &busy_pcrr_p);
	if (used_allctr == allctr) {
	doit:
	    fix_cpool_free(allctr, (type & ~ERTS_ALC_FIX_NO_UNUSE),
			   ptr, &busy_pcrr_p,
			   !(type & ERTS_ALC_FIX_NO_UNUSE));
	    clear_busy_pool_carrier(allctr, busy_pcrr_p);
	}
	else {
	    /* Carrier migrated; need to redirect block to new owner... */
	    int cinit = used_allctr->dd.ix - allctr->dd.ix;

	    ERTS_ALC_CPOOL_ASSERT(!busy_pcrr_p);

	    DEC_CC(allctr->calls.this_free);

	    ((ErtsAllctrFixDDBlock_t *) ptr)->fix_type = type;
	    if (ddq_enqueue(&used_allctr->dd.q, ptr, cinit))
		erts_alloc_notify_delayed_dealloc(used_allctr->ix);
	}
    }
}

static void
schedule_dealloc_carrier(Allctr_t *allctr, Carrier_t *crr);

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

	blk = UMEM2BLK(ptr);
	if (IS_FREE_LAST_MBC_BLK(blk)) {
	    /*
	     * A multiblock carrier that previously has been migrated away
	     * from us and now is back to be deallocated. For more info
	     * see schedule_dealloc_carrier().
	     *
	     * Note that we cannot use FBLK_TO_MBC(blk) since it
	     * data has been overwritten by the queue.
	     */
	    Carrier_t *crr = FIRST_BLK_TO_MBC(allctr, blk);

	     /* Restore word overwritten by the dd-queue as it will be read
	      * if this carrier is pulled from dc_list by cpool_fetch()
	      */
	    ERTS_ALC_CPOOL_ASSERT(FBLK_TO_MBC(blk) != crr);
	    ERTS_CT_ASSERT(sizeof(ErtsAllctrDDBlock_t) == sizeof(void*));
#ifdef MBC_ABLK_OFFSET_BITS
	    blk->u.carrier = crr;
#else
	    blk->carrier = crr;
#endif

	    ERTS_ALC_CPOOL_ASSERT(ERTS_ALC_IS_CPOOL_ENABLED(allctr));
	    ERTS_ALC_CPOOL_ASSERT(allctr == crr->cpool.orig_allctr);
	    ERTS_ALC_CPOOL_ASSERT(((erts_aint_t) allctr)
				  != (erts_smp_atomic_read_nob(&crr->allctr)
				      & ~ERTS_CRR_ALCTR_FLG_MASK));

	    erts_smp_atomic_set_nob(&crr->allctr, ((erts_aint_t) allctr));

	    schedule_dealloc_carrier(allctr, crr);
	}
	else {

	    INC_CC(allctr->calls.this_free);

	    if (fix)
		handle_delayed_fix_dealloc(allctr, ptr);
	    else
		dealloc_block(allctr, ptr, NULL, 1);
	}
    }

    if (need_thr_progress && !(need_thr_prgr | need_mr_wrk)) {
	need_thr_prgr = ddq_check_incoming(ddq);
	*need_thr_progress |= need_thr_prgr;
	if (need_thr_prgr)
	    store_earliest_thr_prgr(thr_prgr_p, ddq);
    }

    if (ERTS_ALC_IS_CPOOL_ENABLED(allctr))
	check_pending_dealloc_carrier(allctr,
				      need_thr_progress,
				      thr_prgr_p,
				      need_more_work);

    if (allctr->thread_safe && !allctr_locked)
	erts_mtx_unlock(&allctr->mutex);
   return res;
}

static ERTS_INLINE void
enqueue_dealloc_other_instance(ErtsAlcType_t type,
			       Allctr_t *allctr,
			       void *ptr,
			       int cinit)
{
    if (allctr->fix)
	((ErtsAllctrFixDDBlock_t*) ptr)->fix_type = type;

    if (ddq_enqueue(&allctr->dd.q, ptr, cinit))
	erts_alloc_notify_delayed_dealloc(allctr->ix);
}

#endif

#ifdef ERTS_SMP
static void
set_new_allctr_abandon_limit(Allctr_t *allctr);
static void
abandon_carrier(Allctr_t *allctr, Carrier_t *crr);


static ERTS_INLINE void
check_abandon_carrier(Allctr_t *allctr, Block_t *fblk, Carrier_t **busy_pcrr_pp)
{
    Carrier_t *crr;

    if (busy_pcrr_pp && *busy_pcrr_pp)
	return;

    if (!ERTS_ALC_IS_CPOOL_ENABLED(allctr))
	return;

    allctr->cpool.check_limit_count--;
    if (--allctr->cpool.check_limit_count <= 0)
	set_new_allctr_abandon_limit(allctr);

    if (!erts_thr_progress_is_managed_thread())
	return;

    if (allctr->cpool.disable_abandon)
	return;

    if (allctr->mbcs.blocks.curr.size > allctr->cpool.abandon_limit)
	return;


    crr = FBLK_TO_MBC(fblk);

    if (allctr->main_carrier == crr)
	return;

    if (crr->cpool.blocks_size > crr->cpool.abandon_limit)
	return;

    if (crr->cpool.thr_prgr != ERTS_THR_PRGR_INVALID
	&& !erts_thr_progress_has_reached(crr->cpool.thr_prgr))
	return;

    abandon_carrier(allctr, crr);
}

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

#define ERTS_ALCU_HANDLE_DD_IN_OP(Allctr, Locked)			\
    handle_delayed_dealloc((Allctr), (Locked), 1, 			\
			   ERTS_ALCU_DD_OPS_LIM_LOW, NULL, NULL, NULL)

static void
dealloc_block(Allctr_t *allctr, void *ptr, ErtsAlcFixList_t *fix, int dec_cc_on_redirect)
{
    Block_t *blk = UMEM2BLK(ptr);

    ERTS_SMP_LC_ASSERT(!allctr->thread_safe
		       || erts_lc_mtx_is_locked(&allctr->mutex));

    if (IS_SBC_BLK(blk)) {
	destroy_carrier(allctr, blk, NULL);
#ifdef ERTS_SMP
	if (fix && ERTS_ALC_IS_CPOOL_ENABLED(allctr)) {
	    ErtsAlcType_t type = ((ErtsAllctrFixDDBlock_t *) ptr)->fix_type;
	    if (!(type & ERTS_ALC_FIX_NO_UNUSE))
		fix->u.cpool.used--;
	    fix->u.cpool.allocated--;
	}
#endif
    }
#ifndef ERTS_SMP
    else
	mbc_free(allctr, ptr, NULL);
#else
    else if (!ERTS_ALC_IS_CPOOL_ENABLED(allctr))
	mbc_free(allctr, ptr, NULL);
    else {
	Carrier_t *busy_pcrr_p;
	Allctr_t *used_allctr;
	used_allctr = get_used_allctr(allctr, ERTS_ALC_TS_PREF_LOCK_NO, ptr,
				      NULL, &busy_pcrr_p);
	if (used_allctr == allctr) {
	    if (fix) {
		ErtsAlcType_t type = ((ErtsAllctrFixDDBlock_t *) ptr)->fix_type;
		if (!(type & ERTS_ALC_FIX_NO_UNUSE))
		    fix->u.cpool.used--;
		fix->u.cpool.allocated--;
	    }
	    mbc_free(allctr, ptr, &busy_pcrr_p);
	    clear_busy_pool_carrier(allctr, busy_pcrr_p);
	}
	else {
	    /* Carrier migrated; need to redirect block to new owner... */
	    int cinit = used_allctr->dd.ix - allctr->dd.ix;

	    ERTS_ALC_CPOOL_ASSERT(!busy_pcrr_p);

	    if (dec_cc_on_redirect)
		DEC_CC(allctr->calls.this_free);
	    if (ddq_enqueue(&used_allctr->dd.q, ptr, cinit))
		erts_alloc_notify_delayed_dealloc(used_allctr->ix);
	}
    }
#endif
}

/* Multi block carrier alloc/realloc/free ... */

/* NOTE! mbc_alloc() may in case of memory shortage place the requested
 * block in a sbc.
 */
static ERTS_INLINE void *
mbc_alloc_block(Allctr_t *allctr, Uint size, Uint *blk_szp)
{
    Block_t *blk;
    Uint get_blk_sz;

    ASSERT(size);
    ASSERT(size < allctr->sbc_threshold);

    *blk_szp = get_blk_sz = UMEMSZ2BLKSZ(allctr, size);

    blk = (*allctr->get_free_block)(allctr, get_blk_sz, NULL, 0);

    if (!blk) {
	blk = create_carrier(allctr, get_blk_sz, CFLG_MBC);
#if !ERTS_SUPER_ALIGNED_MSEG_ONLY
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
		   Carrier_t *crr,
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
	SET_MBC_ABLK_HDR(blk, blk_sz, prev_free_flg, crr);

	nxt_blk = BLK_AFTER(blk, blk_sz);
	SET_MBC_FBLK_HDR(nxt_blk, nxt_blk_sz,
			 SBH_THIS_FREE|(flags & LAST_BLK_HDR_FLG),
			 crr);

	if (!(flags & LAST_BLK_HDR_FLG)) {
	    SET_BLK_SZ_FTR(nxt_blk, nxt_blk_sz);
	    if (!valid_blk_info) {
		Block_t *nxt_nxt_blk = BLK_AFTER(nxt_blk, nxt_blk_sz);
		SET_PREV_BLK_FREE(allctr, nxt_nxt_blk);
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
	ASSERT(nxt_blk_sz == MBC_BLK_SZ(nxt_blk));
	ASSERT(nxt_blk_sz % sizeof(Unit_t) == 0);
	ASSERT(nxt_blk_sz >= allctr->min_block_size);
	ASSERT(ABLK_TO_MBC(blk) == crr);
	ASSERT(FBLK_TO_MBC(nxt_blk) == crr);
    }
    else {
	ASSERT(org_blk_sz <= MBC_ABLK_SZ_MASK);
	blk_sz = org_blk_sz;
	if (flags & LAST_BLK_HDR_FLG) {
	    if (valid_blk_info)
		SET_BLK_ALLOCED(blk);
	    else
		SET_MBC_ABLK_HDR(blk, blk_sz, SBH_LAST_BLK|prev_free_flg, crr);
	}
	else {
	    if (valid_blk_info)
		SET_BLK_ALLOCED(blk);
	    else
		SET_MBC_ABLK_HDR(blk, blk_sz, prev_free_flg, crr);
	    nxt_blk = BLK_AFTER(blk, blk_sz);
	    SET_PREV_BLK_ALLOCED(nxt_blk);
	}

	ASSERT((flags & LAST_BLK_HDR_FLG)
	       ? IS_LAST_BLK(blk)
	       : IS_NOT_LAST_BLK(blk));
	ASSERT(ABLK_TO_MBC(blk) == crr);
    }

    ERTS_ALC_CPOOL_ALLOC_OP(allctr);
    STAT_MBC_BLK_ALLOC(allctr, crr, blk_sz, alcu_flgs);

    ASSERT(IS_ALLOCED_BLK(blk));
    ASSERT(blk_sz == MBC_BLK_SZ(blk));
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
			   MBC_FBLK_SZ(blk),
			   GET_BLK_HDR_FLGS(blk),
			   FBLK_TO_MBC(blk),
			   blk_sz,
			   1);
    return BLK2UMEM(blk);
}

static void
mbc_free(Allctr_t *allctr, void *p, Carrier_t **busy_pcrr_pp)
{
    Uint is_first_blk;
    Uint is_last_blk;
    Uint blk_sz;
    Block_t *blk;
    Block_t *nxt_blk;
    Carrier_t *crr;

    ASSERT(p);

    blk = UMEM2BLK(p);
    blk_sz = MBC_ABLK_SZ(blk);

    ASSERT(IS_MBC_BLK(blk));
    ASSERT(blk_sz >= allctr->min_block_size);

    HARD_CHECK_BLK_CARRIER(allctr, blk);

    crr = ABLK_TO_MBC(blk);

    ERTS_ALC_CPOOL_FREE_OP(allctr);
    STAT_MBC_BLK_FREE(allctr, crr, busy_pcrr_pp, blk_sz, alcu_flgs);

    is_first_blk = IS_MBC_FIRST_ABLK(allctr, blk);
    is_last_blk = IS_LAST_BLK(blk);

    if (IS_PREV_BLK_FREE(blk)) {
	ASSERT(!is_first_blk); 
	/* Coalesce with previous block... */
	blk = PREV_BLK(blk);
	(*allctr->unlink_free_block)(allctr, blk);

	blk_sz += MBC_FBLK_SZ(blk);
	is_first_blk = IS_MBC_FIRST_FBLK(allctr, blk);
	SET_MBC_FBLK_SZ(blk, blk_sz);
    }
    else {
	SET_BLK_FREE(blk);
    }

    if (is_last_blk)
	SET_LAST_BLK(blk);
    else {
	nxt_blk = BLK_AFTER(blk, blk_sz);
	if (IS_FREE_BLK(nxt_blk)) {
	    /* Coalesce with next block... */
	    (*allctr->unlink_free_block)(allctr, nxt_blk);
	    blk_sz += MBC_FBLK_SZ(nxt_blk);
	    SET_MBC_FBLK_SZ(blk, blk_sz);

	    is_last_blk = IS_LAST_BLK(nxt_blk);
	    if (is_last_blk) 
		SET_LAST_BLK(blk);
	    else {
		SET_NOT_LAST_BLK(blk);
		SET_BLK_SZ_FTR(blk, blk_sz);
	    }
	}
	else {
	    SET_PREV_BLK_FREE(allctr, nxt_blk);
	    SET_NOT_LAST_BLK(blk);
	    SET_BLK_SZ_FTR(blk, blk_sz);
	}

    }

    ASSERT(IS_FREE_BLK(blk));
    ASSERT(!is_last_blk  == !IS_LAST_BLK(blk));
    ASSERT(!is_first_blk == !IS_MBC_FIRST_FBLK(allctr, blk));
    ASSERT(is_first_blk || IS_PREV_BLK_ALLOCED(blk));
    ASSERT(is_last_blk  || IS_PREV_BLK_FREE(NXT_BLK(blk)));
    ASSERT(blk_sz == MBC_BLK_SZ(blk));
    ASSERT(is_last_blk || blk == PREV_BLK(NXT_BLK(blk)));
    ASSERT(blk_sz % sizeof(Unit_t) == 0);
    ASSERT(IS_MBC_BLK(blk));

    if (is_first_blk
	&& is_last_blk
	&& allctr->main_carrier != FIRST_BLK_TO_MBC(allctr, blk)) {
	destroy_carrier(allctr, blk, busy_pcrr_pp);
    }
    else {
	(*allctr->link_free_block)(allctr, blk);
	HARD_CHECK_BLK_CARRIER(allctr, blk);
#ifdef ERTS_SMP
	check_abandon_carrier(allctr, blk, busy_pcrr_pp);
#endif
    }
}

static void *
mbc_realloc(Allctr_t *allctr, void *p, Uint size, Uint32 alcu_flgs,
	    Carrier_t **busy_pcrr_pp)
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

    ASSERT(p);
    ASSERT(size);
    ASSERT(size < allctr->sbc_threshold);

    blk = (Block_t *) UMEM2BLK(p);
    old_blk_sz = MBC_ABLK_SZ(blk);

    ASSERT(old_blk_sz >= allctr->min_block_size);

#ifdef MBC_REALLOC_ALWAYS_MOVES
    if (alcu_flgs & ERTS_ALCU_FLG_FAIL_REALLOC_MOVE)
	return NULL;
#else /* !MBC_REALLOC_ALWAYS_MOVES */

#ifdef ERTS_SMP
    if (busy_pcrr_pp && *busy_pcrr_pp)
	goto realloc_move; /* Don't want to use carrier in pool */
#endif

    get_blk_sz = blk_sz = UMEMSZ2BLKSZ(allctr, size);

    ASSERT(IS_ALLOCED_BLK(blk));
    ASSERT(IS_MBC_BLK(blk));

    is_last_blk = IS_LAST_BLK(blk);

    if (old_blk_sz == blk_sz)
	return p;
    else if (blk_sz < old_blk_sz) {
	/* Shrink block... */
	Carrier_t* crr;
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
	    if (!IS_PREV_BLK_FREE(blk)) {
		cand_blk = blk;
	    }
	    else {
		ASSERT(!IS_MBC_FIRST_ABLK(allctr, blk));
		cand_blk = PREV_BLK(blk);
		cand_blk_sz += PREV_BLK_SZ(blk);
	    }
	    if (!is_last_blk) {
		nxt_blk = BLK_AFTER(blk, old_blk_sz);
		if (IS_FREE_BLK(nxt_blk))
		    cand_blk_sz += MBC_FBLK_SZ(nxt_blk);
	    }

	    new_blk = (*allctr->get_free_block)(allctr,
						get_blk_sz,
						cand_blk,
						cand_blk_sz);
	    if (new_blk || cand_blk != blk)
		goto move_into_new_blk;
	}

	/* Shrink at current location */

	nxt_blk_sz = old_blk_sz - blk_sz;

	if ((is_last_blk || IS_ALLOCED_BLK(BLK_AFTER(blk,old_blk_sz)))
	    && (nxt_blk_sz < allctr->min_block_size))
	    return p;

	HARD_CHECK_BLK_CARRIER(allctr, blk);

	nxt_nxt_blk = BLK_AFTER(blk, old_blk_sz);

	SET_MBC_ABLK_SZ(blk, blk_sz);
	SET_NOT_LAST_BLK(blk);

	nxt_blk = BLK_AFTER(blk, blk_sz);

	crr = ABLK_TO_MBC(blk);

	ERTS_ALC_CPOOL_REALLOC_OP(allctr);
	STAT_MBC_BLK_FREE(allctr, crr, NULL, old_blk_sz, alcu_flgs);
	STAT_MBC_BLK_ALLOC(allctr, crr, blk_sz, alcu_flgs);

	ASSERT(MBC_BLK_SZ(blk) >= allctr->min_block_size);

	if (!is_last_blk) {
	    if (IS_FREE_BLK(nxt_nxt_blk)) {
		/* Coalesce with next free block... */
		nxt_blk_sz += MBC_FBLK_SZ(nxt_nxt_blk);
		(*allctr->unlink_free_block)(allctr, nxt_nxt_blk);

		is_last_blk = GET_LAST_BLK_HDR_FLG(nxt_nxt_blk);
	    }
	    else {
		SET_PREV_BLK_FREE(allctr, nxt_nxt_blk);
	    }
	    SET_BLK_SZ_FTR(nxt_blk, nxt_blk_sz);
	}

	SET_MBC_FBLK_HDR(nxt_blk, nxt_blk_sz,
			SBH_THIS_FREE | (is_last_blk ? SBH_LAST_BLK : 0),
			crr);

	(*allctr->link_free_block)(allctr, nxt_blk);


	ASSERT(IS_ALLOCED_BLK(blk));
	ASSERT(blk_sz == MBC_BLK_SZ(blk));
	ASSERT(blk_sz % sizeof(Unit_t) == 0);
	ASSERT(blk_sz >= allctr->min_block_size);
	ASSERT(blk_sz >= size + ABLK_HDR_SZ);
	ASSERT(IS_MBC_BLK(blk));
    
	ASSERT(IS_FREE_BLK(nxt_blk));
	ASSERT(IS_PREV_BLK_ALLOCED(nxt_blk));
	ASSERT(nxt_blk_sz == MBC_BLK_SZ(nxt_blk));
	ASSERT(nxt_blk_sz % sizeof(Unit_t) == 0);
	ASSERT(nxt_blk_sz >= allctr->min_block_size);
	ASSERT(IS_MBC_BLK(nxt_blk));
	ASSERT(is_last_blk ? IS_LAST_BLK(nxt_blk) : IS_NOT_LAST_BLK(nxt_blk));
	ASSERT(is_last_blk || nxt_blk == PREV_BLK(NXT_BLK(nxt_blk)));
	ASSERT(is_last_blk || IS_PREV_BLK_FREE(NXT_BLK(nxt_blk)));
	ASSERT(FBLK_TO_MBC(nxt_blk) == crr);
		    
	HARD_CHECK_BLK_CARRIER(allctr, blk);

#ifdef ERTS_SMP
	check_abandon_carrier(allctr, nxt_blk, NULL);
#endif

	return p;
    }

    /* Need larger block... */

    if (!is_last_blk) {
	nxt_blk = BLK_AFTER(blk, old_blk_sz);
	nxt_blk_sz = MBC_BLK_SZ(nxt_blk);
	if (IS_FREE_BLK(nxt_blk) && get_blk_sz <= old_blk_sz + nxt_blk_sz) {
	    Carrier_t* crr = ABLK_TO_MBC(blk);
	    /* Grow into next block... */

	    HARD_CHECK_BLK_CARRIER(allctr, blk);

	    (*allctr->unlink_free_block)(allctr, nxt_blk);
	    nxt_blk_sz -= blk_sz - old_blk_sz;

	    is_last_blk = IS_LAST_BLK(nxt_blk);
	    if (nxt_blk_sz < allctr->min_block_size) {
		blk_sz += nxt_blk_sz;

		SET_MBC_ABLK_SZ(blk, blk_sz);

		if (is_last_blk) {
		    SET_LAST_BLK(blk);
#ifdef DEBUG
		    nxt_blk = NULL;
#endif
		}
		else {
		    nxt_blk = BLK_AFTER(blk, blk_sz);
		    SET_PREV_BLK_ALLOCED(nxt_blk);
#ifdef DEBUG
		    is_last_blk = IS_LAST_BLK(nxt_blk);
		    nxt_blk_sz = MBC_BLK_SZ(nxt_blk);
#endif
		}
	    }
	    else {
		SET_MBC_ABLK_SZ(blk, blk_sz);

		nxt_blk = BLK_AFTER(blk, blk_sz);
		SET_MBC_FBLK_HDR(nxt_blk, nxt_blk_sz, SBH_THIS_FREE, crr);

		if (is_last_blk)
		    SET_LAST_BLK(nxt_blk);
		else
		    SET_BLK_SZ_FTR(nxt_blk, nxt_blk_sz);

		(*allctr->link_free_block)(allctr, nxt_blk);

		ASSERT(IS_FREE_BLK(nxt_blk));
		ASSERT(FBLK_TO_MBC(nxt_blk) == crr);
	    }

	    ERTS_ALC_CPOOL_REALLOC_OP(allctr);
	    STAT_MBC_BLK_FREE(allctr, crr, NULL, old_blk_sz, alcu_flgs);
	    STAT_MBC_BLK_ALLOC(allctr, crr, blk_sz, alcu_flgs);

	    ASSERT(IS_ALLOCED_BLK(blk));
	    ASSERT(blk_sz == MBC_BLK_SZ(blk));
	    ASSERT(blk_sz % sizeof(Unit_t) == 0);
	    ASSERT(blk_sz >= allctr->min_block_size);
	    ASSERT(blk_sz >= size + ABLK_HDR_SZ);
	    ASSERT(IS_MBC_BLK(blk));

	    ASSERT(!nxt_blk || IS_PREV_BLK_ALLOCED(nxt_blk));
	    ASSERT(!nxt_blk || nxt_blk_sz == MBC_BLK_SZ(nxt_blk));
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

    if (!IS_PREV_BLK_FREE(blk)) {
	cand_blk = NULL;
	cand_blk_sz = 0;
    }
    else {
	ASSERT(!IS_MBC_FIRST_ABLK(allctr, blk));
	cand_blk = PREV_BLK(blk);
	cand_blk_sz = old_blk_sz + PREV_BLK_SZ(blk);

	if (!is_last_blk) {
	    nxt_blk = BLK_AFTER(blk, old_blk_sz);
	    if (IS_FREE_BLK(nxt_blk))
		cand_blk_sz += MBC_FBLK_SZ(nxt_blk);
	}
    }

    if (cand_blk_sz < get_blk_sz) {
	/* We wont fit in cand_blk get a new one */
#ifdef ERTS_SMP
    realloc_move:
#endif
#endif /* !MBC_REALLOC_ALWAYS_MOVES */

	new_p = mbc_alloc(allctr, size);
	if (!new_p)
	    return NULL;
	sys_memcpy(new_p, p, MIN(size, old_blk_sz - ABLK_HDR_SZ));
	mbc_free(allctr, p, busy_pcrr_pp);

	return new_p;

#ifndef MBC_REALLOC_ALWAYS_MOVES

    }
    else {
	/* We will at least fit in cand_blk */

	new_blk = (*allctr->get_free_block)(allctr,
					    get_blk_sz,
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
			       MBC_FBLK_SZ(new_blk),
			       GET_BLK_HDR_FLGS(new_blk),
			       FBLK_TO_MBC(new_blk),
			       blk_sz,
			       1);
	    new_p = BLK2UMEM(new_blk);
	    sys_memcpy(new_p, p, MIN(size, old_blk_sz - ABLK_HDR_SZ));
	    mbc_free(allctr, p, NULL);
	    return new_p;
	}
	else {
	    Carrier_t* crr;
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
		nxt_blk = BLK_AFTER(blk, old_blk_sz);
		if (IS_FREE_BLK(nxt_blk)) {
		    new_blk_flgs |= GET_LAST_BLK_HDR_FLG(nxt_blk);
		    new_blk_sz += MBC_FBLK_SZ(nxt_blk);
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
	    crr = FBLK_TO_MBC(new_blk);

	    if (prev_blk_sz >= blk_cpy_sz)
		sys_memcpy(new_p, p, blk_cpy_sz - ABLK_HDR_SZ);
	    else
		sys_memmove(new_p, p, blk_cpy_sz - ABLK_HDR_SZ);

	    mbc_alloc_finalize(allctr,
			       new_blk,
			       new_blk_sz,
			       new_blk_flgs,
			       crr,
			       blk_sz,
			       0);

	    ERTS_ALC_CPOOL_FREE_OP(allctr);
	    STAT_MBC_BLK_FREE(allctr, crr, NULL, old_blk_sz, alcu_flgs);

	    return new_p;
	}
    }
#endif /* !MBC_REALLOC_ALWAYS_MOVES */
}

#ifdef ERTS_SMP

#define ERTS_ALC_MAX_DEALLOC_CARRIER		10
#define ERTS_ALC_CPOOL_MAX_FETCH_INSPECT	20
#define ERTS_ALC_CPOOL_MAX_TRAITOR_INSPECT	10
#define ERTS_ALC_CPOOL_CHECK_LIMIT_COUNT	100
#define ERTS_ALC_CPOOL_MAX_FAILED_STAT_READS	3

#define ERTS_ALC_CPOOL_PTR_MOD_MRK		(((erts_aint_t) 1) << 0)
#define ERTS_ALC_CPOOL_PTR_DEL_MRK		(((erts_aint_t) 1) << 1)

#define ERTS_ALC_CPOOL_PTR_MRKS \
    (ERTS_ALC_CPOOL_PTR_MOD_MRK | ERTS_ALC_CPOOL_PTR_DEL_MRK)

/*
 * When setting multiple mod markers we always
 * set mod markers in pointer order and always
 * on next pointers before prev pointers.
 */

typedef union {
    ErtsAlcCPoolData_t sentinel;
    char align__[ERTS_ALC_CACHE_LINE_ALIGN_SIZE(sizeof(ErtsAlcCPoolData_t))];
} ErtsAlcCrrPool_t;

#if ERTS_ALC_A_INVALID != 0
#  error "Carrier pool implementation assumes ERTS_ALC_A_INVALID == 0"
#endif
#if ERTS_ALC_A_MIN <= ERTS_ALC_A_INVALID
#  error "Carrier pool implementation assumes ERTS_ALC_A_MIN > ERTS_ALC_A_INVALID"
#endif

/*
 * The pool is only allowed to be manipulated by managed
 * threads except in the alloc_SUITE:cpool case. In this
 * test case carrier_pool[ERTS_ALC_A_INVALID] will be
 * used.
 */

static ErtsAlcCrrPool_t carrier_pool[ERTS_ALC_A_MAX+1] erts_align_attribute(ERTS_CACHE_LINE_SIZE);

#define ERTS_ALC_CPOOL_MAX_BACKOFF (1 << 8)

static int
backoff(int n)
{
    int i;

    for (i = 0; i < n; i++)
	ERTS_SPIN_BODY;

    if (n >= ERTS_ALC_CPOOL_MAX_BACKOFF)
	return ERTS_ALC_CPOOL_MAX_BACKOFF;
    else
	return n << 1;
}

static int
cpool_dbg_is_in_pool(Allctr_t *allctr, Carrier_t *crr)
{
    ErtsAlcCPoolData_t *sentinel = &carrier_pool[allctr->alloc_no].sentinel;
    ErtsAlcCPoolData_t *cpdp = sentinel;
    Carrier_t *tmp_crr;

    while (1) {
	cpdp = (ErtsAlcCPoolData_t *) (erts_atomic_read_ddrb(&cpdp->next) & ~FLG_MASK);
	if (cpdp == sentinel)
	    return 0;
	tmp_crr = (Carrier_t *) (((char *) cpdp) - offsetof(Carrier_t, cpool));
	if (tmp_crr == crr)
	    return 1;
    }
}

static int
cpool_is_empty(Allctr_t *allctr)
{
    ErtsAlcCPoolData_t *sentinel = &carrier_pool[allctr->alloc_no].sentinel;
    return ((erts_atomic_read_rb(&sentinel->next) == (erts_aint_t) sentinel)
	    && (erts_atomic_read_rb(&sentinel->prev) == (erts_aint_t) sentinel));
}

static ERTS_INLINE ErtsAlcCPoolData_t *
cpool_aint2cpd(erts_aint_t aint)
{
    return (ErtsAlcCPoolData_t *) (aint & ~ERTS_ALC_CPOOL_PTR_MRKS);
}

static ERTS_INLINE erts_aint_t
cpool_read(erts_atomic_t *aptr)
{
    return erts_atomic_read_acqb(aptr);
}

static ERTS_INLINE void
cpool_init(erts_atomic_t *aptr, erts_aint_t val)
{
    erts_atomic_set_nob(aptr, val);
}

static ERTS_INLINE void
cpool_set_mod_marked(erts_atomic_t *aptr, erts_aint_t new, erts_aint_t old)
{
#ifdef ERTS_ALC_CPOOL_DEBUG
    erts_aint_t act = erts_atomic_xchg_relb(aptr, new);
    ERTS_ALC_CPOOL_ASSERT(act == (old | ERTS_ALC_CPOOL_PTR_MOD_MRK));
#else
    erts_atomic_set_relb(aptr, new);
#endif
}


static ERTS_INLINE erts_aint_t
cpool_try_mod_mark_exp(erts_atomic_t *aptr, erts_aint_t exp)
{
    ERTS_ALC_CPOOL_ASSERT((exp & ERTS_ALC_CPOOL_PTR_MOD_MRK) == 0);
    return erts_atomic_cmpxchg_nob(aptr, exp | ERTS_ALC_CPOOL_PTR_MOD_MRK, exp);
}

static ERTS_INLINE erts_aint_t
cpool_mod_mark_exp(erts_atomic_t *aptr, erts_aint_t exp)
{
    int b;
    erts_aint_t act;
    ERTS_ALC_CPOOL_ASSERT((exp & ERTS_ALC_CPOOL_PTR_MOD_MRK) == 0);
    while (1) {
	act = erts_atomic_cmpxchg_nob(aptr,
				      exp | ERTS_ALC_CPOOL_PTR_MOD_MRK,
				      exp);
	if (act == exp)
	    return exp;
	b = 1;
	do {
	    if ((act & ~ERTS_ALC_CPOOL_PTR_MOD_MRK) != exp)
		return act;
	    b = backoff(b);
	    act = erts_atomic_read_nob(aptr);
	} while (act != exp);
    }
}

static ERTS_INLINE erts_aint_t
cpool_mod_mark(erts_atomic_t *aptr)
{
    int b;
    erts_aint_t act, exp;
    act = cpool_read(aptr);
    while (1) {
	b = 1;
	while (act & ERTS_ALC_CPOOL_PTR_MOD_MRK) {
	    b = backoff(b);
	    act = erts_atomic_read_nob(aptr);
	}
	exp = act;
	act = erts_atomic_cmpxchg_acqb(aptr,
				       exp | ERTS_ALC_CPOOL_PTR_MOD_MRK,
				       exp);
	if (act == exp)
	    return exp;
    }
}

static void
cpool_insert(Allctr_t *allctr, Carrier_t *crr)
{
    ErtsAlcCPoolData_t *cpd1p, *cpd2p;
    erts_aint_t val;
    ErtsAlcCPoolData_t *sentinel = &carrier_pool[allctr->alloc_no].sentinel;

    ERTS_ALC_CPOOL_ASSERT(allctr->alloc_no == ERTS_ALC_A_INVALID /* testcase */
			  || erts_thr_progress_is_managed_thread());
    ERTS_ALC_CPOOL_ASSERT(erts_smp_atomic_read_nob(&crr->allctr)
			  == (erts_aint_t) allctr);

    erts_atomic_add_nob(&allctr->cpool.stat.blocks_size,
			(erts_aint_t) crr->cpool.blocks_size);
    erts_atomic_add_nob(&allctr->cpool.stat.no_blocks,
			(erts_aint_t) crr->cpool.blocks);
    erts_atomic_add_nob(&allctr->cpool.stat.carriers_size,
			(erts_aint_t) CARRIER_SZ(crr));
    erts_atomic_inc_nob(&allctr->cpool.stat.no_carriers);

    /*
     * We search in 'next' direction and begin by passing
     * one element before trying to insert. This in order to
     * avoid contention with threads fetching elements.
     */

    val = cpool_read(&sentinel->next);

    /* Find a predecessor to be, and set mod marker on its next ptr */

    while (1) {
	cpd1p = cpool_aint2cpd(val);
	if (cpd1p == sentinel) {
	    val = cpool_mod_mark(&cpd1p->next);
	    break;
	}
	val = cpool_read(&cpd1p->next);
	if (!(val & ERTS_ALC_CPOOL_PTR_MRKS)) {
	    erts_aint_t tmp = cpool_try_mod_mark_exp(&cpd1p->next, val);
	    if (tmp == val) {
		val = tmp;
		break;
	    }
	    val = tmp;
	}
    }

    /* Set mod marker on prev ptr of the to be successor */

    cpd2p = cpool_aint2cpd(val);

    cpool_init(&crr->cpool.next, (erts_aint_t) cpd2p);
    cpool_init(&crr->cpool.prev, (erts_aint_t) cpd1p);

    val = (erts_aint_t) cpd1p;

    while (1) {
	int b;
	erts_aint_t tmp;

	tmp = cpool_mod_mark_exp(&cpd2p->prev, val);
	if (tmp == val)
	    break;
	b = 1;
	do {
	    b = backoff(b);
	    tmp = cpool_read(&cpd2p->prev);
	} while (tmp != val);
    }

    /* Write pointers to this element in successor and predecessor */

    cpool_set_mod_marked(&cpd1p->next,
			 (erts_aint_t) &crr->cpool,
			 (erts_aint_t) cpd2p);
    cpool_set_mod_marked(&cpd2p->prev,
			 (erts_aint_t) &crr->cpool,
			 (erts_aint_t) cpd1p);

    erts_smp_atomic_set_wb(&crr->allctr,
			   ((erts_aint_t) allctr)|ERTS_CRR_ALCTR_FLG_IN_POOL);
    LTTNG3(carrier_pool_put, ERTS_ALC_A2AD(allctr->alloc_no), allctr->ix, CARRIER_SZ(crr));
}

static void
cpool_delete(Allctr_t *allctr, Allctr_t *prev_allctr, Carrier_t *crr)
{
    ErtsAlcCPoolData_t *cpd1p, *cpd2p;
    erts_aint_t val;
#ifdef ERTS_ALC_CPOOL_DEBUG
    ErtsAlcCPoolData_t *sentinel = &carrier_pool[allctr->alloc_no].sentinel;
#endif

    ERTS_ALC_CPOOL_ASSERT(allctr->alloc_no == ERTS_ALC_A_INVALID /* testcase */
			  || erts_thr_progress_is_managed_thread());
    ERTS_ALC_CPOOL_ASSERT(sentinel != &crr->cpool);

    /* Set mod marker on next ptr of our predecessor */

    val = (erts_aint_t) &crr->cpool;
    while (1) {
	erts_aint_t tmp;
	cpd1p = cpool_aint2cpd(cpool_read(&crr->cpool.prev));
	tmp = cpool_mod_mark_exp(&cpd1p->next, val);
	if (tmp == val)
	    break;
    }

    /* Set mod marker on our next ptr */

    val = cpool_mod_mark(&crr->cpool.next);

    /* Set mod marker on the prev ptr of our successor */

    cpd2p = cpool_aint2cpd(val);

    val = (erts_aint_t) &crr->cpool;

    while (1) {
	int b;
	erts_aint_t tmp;

	tmp = cpool_mod_mark_exp(&cpd2p->prev, val);
	if (tmp == val)
	    break;
	b = 1;
	do {
	    b = backoff(b);
	    tmp = cpool_read(&cpd2p->prev);
	} while (tmp != val);
    }

    /* Set mod marker on our prev ptr */

    val = (erts_aint_t) cpd1p;

    while (1) {
	int b;
	erts_aint_t tmp;

	tmp = cpool_mod_mark_exp(&crr->cpool.prev, val);
	if (tmp == val)
	    break;
	b = 1;
	do {
	    b = backoff(b);
	    tmp = cpool_read(&cpd2p->prev);
	} while (tmp != val);
    }

    /* Write pointers past this element in predecessor and successor */

    cpool_set_mod_marked(&cpd1p->next,
			 (erts_aint_t) cpd2p,
			 (erts_aint_t) &crr->cpool);
    cpool_set_mod_marked(&cpd2p->prev,
			 (erts_aint_t) cpd1p,
			 (erts_aint_t) &crr->cpool);

    /* Repleace mod markers with delete markers on this element */
    cpool_set_mod_marked(&crr->cpool.next,
			 ((erts_aint_t) cpd2p) | ERTS_ALC_CPOOL_PTR_DEL_MRK,
			 ((erts_aint_t) cpd2p) | ERTS_ALC_CPOOL_PTR_MOD_MRK);
    cpool_set_mod_marked(&crr->cpool.prev,
			 ((erts_aint_t) cpd1p) | ERTS_ALC_CPOOL_PTR_DEL_MRK,
			 ((erts_aint_t) cpd1p) | ERTS_ALC_CPOOL_PTR_MOD_MRK);

    crr->cpool.thr_prgr = erts_thr_progress_later(NULL);

    erts_atomic_add_nob(&prev_allctr->cpool.stat.blocks_size,
			-((erts_aint_t) crr->cpool.blocks_size));
    erts_atomic_add_nob(&prev_allctr->cpool.stat.no_blocks,
			-((erts_aint_t) crr->cpool.blocks));
    erts_atomic_add_nob(&prev_allctr->cpool.stat.carriers_size,
			-((erts_aint_t) CARRIER_SZ(crr)));
    erts_atomic_dec_wb(&prev_allctr->cpool.stat.no_carriers);

}

static Carrier_t *
cpool_fetch(Allctr_t *allctr, UWord size)
{
    int i, i_stop, has_passed_sentinel;
    Carrier_t *crr;
    ErtsAlcCPoolData_t *cpdp;
    ErtsAlcCPoolData_t *cpool_entrance;
    ErtsAlcCPoolData_t *sentinel;
    ErtsDoubleLink_t* dl;
    ErtsDoubleLink_t* first_old_traitor;

    ERTS_ALC_CPOOL_ASSERT(allctr->alloc_no == ERTS_ALC_A_INVALID /* testcase */
			  || erts_thr_progress_is_managed_thread());

    i = ERTS_ALC_CPOOL_MAX_FETCH_INSPECT;
    first_old_traitor = allctr->cpool.traitor_list.next;
    cpool_entrance = NULL;

    LTTNG3(carrier_pool_get, ERTS_ALC_A2AD(allctr->alloc_no), allctr->ix, (unsigned long)size);
    /*
     * Search my own pooled_list,
     * i.e my abandoned carriers that were in the pool last time I checked.
     */

    dl = allctr->cpool.pooled_list.next;
    while(dl != &allctr->cpool.pooled_list) {
	erts_aint_t exp, act;
	crr = (Carrier_t *) (((char *) dl) - offsetof(Carrier_t, cpool.abandoned));

	ASSERT(!is_in_list(&allctr->cpool.traitor_list, dl));
	ASSERT(crr->cpool.orig_allctr == allctr);
	dl = dl->next;
	exp = erts_smp_atomic_read_rb(&crr->allctr);
	if ((exp & ERTS_CRR_ALCTR_FLG_MASK) == ERTS_CRR_ALCTR_FLG_IN_POOL
	    && erts_atomic_read_nob(&crr->cpool.max_size) >= size) {
	    /* Try to fetch it... */
	    act = erts_smp_atomic_cmpxchg_mb(&crr->allctr,
					     (erts_aint_t) allctr,
					     exp);
	    if (act == exp) {
		cpool_delete(allctr, ((Allctr_t *) (act & ~ERTS_CRR_ALCTR_FLG_MASK)), crr);
		unlink_abandoned_carrier(crr);

		/* Move sentinel to continue next search from here */
		relink_edl_before(dl, &allctr->cpool.pooled_list);
		return crr;
	    }
	    exp = act;
	}
	if (exp & ERTS_CRR_ALCTR_FLG_IN_POOL) {
	    if (!cpool_entrance)
		cpool_entrance = &crr->cpool;
	}
	else { /* Not in pool, move to traitor_list */
	    unlink_abandoned_carrier(crr);
	    link_abandoned_carrier(&allctr->cpool.traitor_list, crr);
	}
	if (--i <= 0) {
	    /* Move sentinel to continue next search from here */
	    relink_edl_before(dl, &allctr->cpool.pooled_list);
	    return NULL;
	}
    }

    /* Now search traitor_list.
     * i.e carriers employed by other allocators last time I checked.
     * They might have been abandoned since then.
     */

    i_stop = (i < ERTS_ALC_CPOOL_MAX_TRAITOR_INSPECT ?
	      0 : i - ERTS_ALC_CPOOL_MAX_TRAITOR_INSPECT);
    dl = first_old_traitor;
    while(dl != &allctr->cpool.traitor_list) {
	erts_aint_t exp, act;
	crr = (Carrier_t *) (((char *) dl) - offsetof(Carrier_t, cpool.abandoned));
	ASSERT(dl != &allctr->cpool.pooled_list);
	ASSERT(crr->cpool.orig_allctr == allctr);
	dl = dl->next;
	exp = erts_smp_atomic_read_rb(&crr->allctr);
	if (exp & ERTS_CRR_ALCTR_FLG_IN_POOL) {
	    if (!(exp & ERTS_CRR_ALCTR_FLG_BUSY)
		&& erts_atomic_read_nob(&crr->cpool.max_size) >= size) {
		/* Try to fetch it... */
		act = erts_smp_atomic_cmpxchg_mb(&crr->allctr,
						 (erts_aint_t) allctr,
						 exp);
		if (act == exp) {
		    cpool_delete(allctr, ((Allctr_t *) (act & ~ERTS_CRR_ALCTR_FLG_MASK)), crr);
		    unlink_abandoned_carrier(crr);

		    /* Move sentinel to continue next search from here */
		    relink_edl_before(dl, &allctr->cpool.traitor_list);
		    return crr;
		}
		exp = act;
	    }
	    if (exp & ERTS_CRR_ALCTR_FLG_IN_POOL) {
		if (!cpool_entrance)
		    cpool_entrance = &crr->cpool;

		/* Move to pooled_list */
		unlink_abandoned_carrier(crr);
		link_abandoned_carrier(&allctr->cpool.pooled_list, crr);
	    }
	}
	if (--i <= i_stop) {
	    /* Move sentinel to continue next search from here */
	    relink_edl_before(dl, &allctr->cpool.traitor_list);
	    if (i > 0)
		break;
	    else
		return NULL;
	}
    }

    /*
     * Finally search the shared pool and try employ foreign carriers
     */

    sentinel = &carrier_pool[allctr->alloc_no].sentinel;
    if (cpool_entrance) {
	/* We saw a pooled carried above, use it as entrance into the pool
	 */
	cpdp = cpool_entrance;
    }
    else {
	/* No pooled carried seen above. Start search at cpool sentinel,
	 * but begin by passing one element before trying to fetch.
	 * This in order to avoid contention with threads inserting elements.
	 */
	cpool_entrance = sentinel;
	cpdp = cpool_aint2cpd(cpool_read(&cpool_entrance->prev));
	if (cpdp == sentinel)
	    goto check_dc_list;
    }

    has_passed_sentinel = 0;
    while (1) {
	erts_aint_t exp;
	cpdp = cpool_aint2cpd(cpool_read(&cpdp->prev));
	if (cpdp == cpool_entrance) {
	    if (cpool_entrance == sentinel) {
		cpdp = cpool_aint2cpd(cpool_read(&cpdp->prev));
		if (cpdp == sentinel)
		    break;
	    }
	    i = 0; /* Last one to inspect */
	}
	else if (cpdp == sentinel) {
	    if (has_passed_sentinel) {
		/* We been here before. cpool_entrance must have been removed */
		break;
	    }
	    cpdp = cpool_aint2cpd(cpool_read(&cpdp->prev));
	    if (cpdp == sentinel)
                break;
	    has_passed_sentinel = 1;
	}
	crr = (Carrier_t *)(((char *)cpdp) - offsetof(Carrier_t, cpool));
	exp = erts_smp_atomic_read_rb(&crr->allctr);
	if (((exp & (ERTS_CRR_ALCTR_FLG_MASK)) == ERTS_CRR_ALCTR_FLG_IN_POOL)
	    && (erts_atomic_read_nob(&cpdp->max_size) >= size)) {
	    erts_aint_t act;
	    /* Try to fetch it... */
	    act = erts_smp_atomic_cmpxchg_mb(&crr->allctr,
					     (erts_aint_t) allctr,
					     exp);
	    if (act == exp) {
		cpool_delete(allctr, ((Allctr_t *) (act & ~ERTS_CRR_ALCTR_FLG_MASK)), crr);
		if (crr->cpool.orig_allctr == allctr) {
		    unlink_abandoned_carrier(crr);
		}
		return crr;
	    }
	}
	if (--i <= 0)
	    return NULL;
    }

check_dc_list:
    /* Last; check our own pending dealloc carrier list... */
    crr = allctr->cpool.dc_list.last;
    while (crr) {
	if (erts_atomic_read_nob(&crr->cpool.max_size) >= size) {
	    Block_t* blk;
	    unlink_carrier(&allctr->cpool.dc_list, crr);
#ifdef ERTS_ALC_CPOOL_DEBUG
	    ERTS_ALC_CPOOL_ASSERT(erts_smp_atomic_xchg_nob(&crr->allctr,
							   ((erts_aint_t) allctr))
				  == (((erts_aint_t) allctr) & ~ERTS_CRR_ALCTR_FLG_MASK));
#else
	    erts_smp_atomic_set_nob(&crr->allctr, ((erts_aint_t) allctr));
#endif
	    blk = MBC_TO_FIRST_BLK(allctr, crr);
	    ASSERT(FBLK_TO_MBC(blk) == crr);
	    allctr->link_free_block(allctr, blk);
	    return crr;
	}
	crr = crr->prev;
	if (--i <= 0)
	    return NULL;
    }

    return NULL;
}

static void
check_pending_dealloc_carrier(Allctr_t *allctr,
			      int *need_thr_progress,
			      ErtsThrPrgrVal *thr_prgr_p,
			      int *need_more_work)
{
    Carrier_t *crr = allctr->cpool.dc_list.first;

    if (crr) {
	ErtsThrPrgrVal current = erts_thr_progress_current();
	int i = 0;

	do {
	    Carrier_t *dcrr;

	    if (!erts_thr_progress_has_reached_this(current, crr->cpool.thr_prgr))
		break;

	    dcrr = crr;
	    crr = crr->next;
	    dealloc_mbc(allctr, dcrr);
	    i++;
	} while (crr && i < ERTS_ALC_MAX_DEALLOC_CARRIER);

	allctr->cpool.dc_list.first = crr;
	if (!crr)
	    allctr->cpool.dc_list.last = NULL;
	else {
	    crr->prev = NULL;

	    if (need_more_work) {
		ERTS_ALC_CPOOL_ASSERT(need_thr_progress && thr_prgr_p);
		if (erts_thr_progress_has_reached_this(current, crr->cpool.thr_prgr))
		    *need_more_work = 1;
		else {
		    *need_thr_progress = 1;
		    if (*thr_prgr_p == ERTS_THR_PRGR_INVALID
			|| erts_thr_progress_cmp(crr->cpool.thr_prgr,
						 *thr_prgr_p) < 0) {
			*thr_prgr_p = crr->cpool.thr_prgr;
		    }
		}
	    }
	}
    }
}

static void
schedule_dealloc_carrier(Allctr_t *allctr, Carrier_t *crr)
{
    Allctr_t *orig_allctr;
    Block_t *blk;
    int check_pending_dealloc;
    erts_aint_t max_size;

    ASSERT(IS_MB_CARRIER(crr));

    if (!ERTS_ALC_IS_CPOOL_ENABLED(allctr)) {
	dealloc_mbc(allctr, crr);
	return;
    }

    orig_allctr = crr->cpool.orig_allctr;

    if (allctr != orig_allctr) {
	int cinit = orig_allctr->dd.ix - allctr->dd.ix;

	/*
	 * We send the carrier to its origin for deallocation.
	 * This in order:
	 * - not to complicate things for the thread specific
	 *   instances of mseg_alloc, and
	 * - to ensure that we always only reuse empty carriers
	 *   originating from our own thread specific mseg_alloc
	 *   instance which is beneficial on NUMA systems.
	 *
	 * The receiver will recognize that this is a carrier to
	 * deallocate (and not a block which is the common case)
	 * since the block is an mbc block that is free and last
	 * in the carrier.
	 */
	blk = MBC_TO_FIRST_BLK(allctr, crr);
	ERTS_ALC_CPOOL_ASSERT(IS_FREE_LAST_MBC_BLK(blk));

	ERTS_ALC_CPOOL_ASSERT(IS_MBC_FIRST_ABLK(allctr, blk));
	ERTS_ALC_CPOOL_ASSERT(crr == FBLK_TO_MBC(blk));
	ERTS_ALC_CPOOL_ASSERT(crr == FIRST_BLK_TO_MBC(allctr, blk));
	ERTS_ALC_CPOOL_ASSERT(((erts_aint_t) allctr)
			      == (erts_smp_atomic_read_nob(&crr->allctr)
				  & ~ERTS_CRR_ALCTR_FLG_MASK));

	if (ddq_enqueue(&orig_allctr->dd.q, BLK2UMEM(blk), cinit))
	    erts_alloc_notify_delayed_dealloc(orig_allctr->ix);
	return;
    }

    if (is_abandoned(crr))
	unlink_abandoned_carrier(crr);

    if (crr->cpool.thr_prgr == ERTS_THR_PRGR_INVALID
	|| erts_thr_progress_has_reached(crr->cpool.thr_prgr)) {
	dealloc_mbc(allctr, crr);
	return;
    }

    blk = MBC_TO_FIRST_BLK(allctr, crr);
    ASSERT(IS_FREE_LAST_MBC_BLK(blk));
    max_size = (erts_aint_t) MBC_FBLK_SZ(blk);
    erts_atomic_set_nob(&crr->cpool.max_size, max_size);

    crr->next = NULL;
    crr->prev = allctr->cpool.dc_list.last;
    if (allctr->cpool.dc_list.last) {
	check_pending_dealloc = 1;
	allctr->cpool.dc_list.last->next = crr;
    }
    else {
	check_pending_dealloc = 0;
	allctr->cpool.dc_list.first = crr;
    }
    allctr->cpool.dc_list.last = crr;
    if (check_pending_dealloc)
	check_pending_dealloc_carrier(allctr, NULL, NULL, NULL);
    erts_alloc_ensure_handle_delayed_dealloc_call(allctr->ix);
}

static ERTS_INLINE void
cpool_init_carrier_data(Allctr_t *allctr, Carrier_t *crr)
{
    erts_atomic_init_nob(&crr->cpool.next, ERTS_AINT_NULL);
    erts_atomic_init_nob(&crr->cpool.prev, ERTS_AINT_NULL);
    crr->cpool.orig_allctr = allctr;
    crr->cpool.thr_prgr = ERTS_THR_PRGR_INVALID;
    erts_atomic_init_nob(&crr->cpool.max_size, 0);
    crr->cpool.blocks = 0;
    crr->cpool.blocks_size = 0;
    if (!ERTS_ALC_IS_CPOOL_ENABLED(allctr))
	crr->cpool.abandon_limit = 0;
    else {
	UWord csz = CARRIER_SZ(crr);
	UWord limit = csz*allctr->cpool.util_limit;
	if (limit > csz)
	    limit /= 100;
	else
	    limit = (csz/100)*allctr->cpool.util_limit;
	crr->cpool.abandon_limit = limit;
    }
    crr->cpool.abandoned.next = NULL;
    crr->cpool.abandoned.prev = NULL;
}

static void
set_new_allctr_abandon_limit(Allctr_t *allctr)
{
    UWord limit;
    UWord csz;

    allctr->cpool.check_limit_count = ERTS_ALC_CPOOL_CHECK_LIMIT_COUNT;

    csz = allctr->mbcs.curr.norm.mseg.size;
    csz += allctr->mbcs.curr.norm.sys_alloc.size;

    limit = csz*allctr->cpool.util_limit;
    if (limit > csz)
	limit /= 100;
    else
	limit = (csz/100)*allctr->cpool.util_limit;

    allctr->cpool.abandon_limit = limit;
}

static void
abandon_carrier(Allctr_t *allctr, Carrier_t *crr)
{
    erts_aint_t max_size;

    STAT_MBC_CPOOL_INSERT(allctr, crr);

    unlink_carrier(&allctr->mbc_list, crr);
    if (crr->cpool.orig_allctr == allctr) {
	link_abandoned_carrier(&allctr->cpool.pooled_list, crr);
    }

    allctr->remove_mbc(allctr, crr);

    max_size = (erts_aint_t) allctr->largest_fblk_in_mbc(allctr, crr);
    erts_atomic_set_nob(&crr->cpool.max_size, max_size);

    cpool_insert(allctr, crr);

    set_new_allctr_abandon_limit(allctr);
}

static void
cpool_read_stat(Allctr_t *allctr, UWord *nocp, UWord *cszp, UWord *nobp, UWord *bszp)
{
    int i;
    UWord noc = 0, csz = 0, nob = 0, bsz = 0;

    /*
     * We try to get consistent values, but after
     * ERTS_ALC_CPOOL_MAX_FAILED_STAT_READS failed
     * tries we give up and present what we got...
     */
    for (i = 0; i <= ERTS_ALC_CPOOL_MAX_FAILED_STAT_READS; i++) {
	UWord tnoc, tcsz, tnob, tbsz;

	tnoc = (UWord) (nocp
			? erts_atomic_read_nob(&allctr->cpool.stat.no_carriers)
			: 0);
	tcsz = (UWord) (cszp
			? erts_atomic_read_nob(&allctr->cpool.stat.carriers_size)
			: 0);
	tnob = (UWord) (nobp
			? erts_atomic_read_nob(&allctr->cpool.stat.no_blocks)
			: 0);
	tbsz = (UWord) (bszp
			? erts_atomic_read_nob(&allctr->cpool.stat.blocks_size)
			: 0);
	if (tnoc == noc && tcsz == csz && tnob == nob && tbsz == bsz)
	    break;
	noc = tnoc;
	csz = tcsz;
	nob = tnob;
	bsz = tbsz;
	ERTS_THR_READ_MEMORY_BARRIER;
    }

    if (nocp)
	*nocp = noc;
    if (cszp)
	*cszp = csz;
    if (nobp)
	*nobp = nob;
    if (bszp)
	*bszp = bsz;
}


#endif /* ERTS_SMP */

#ifdef DEBUG

#if ERTS_SA_MB_CARRIERS
#define ASSERT_ERTS_SACRR_UNIT_SIZE_MULTIPLE(CSZ) ASSERT((CSZ) % ERTS_SACRR_UNIT_SZ == 0)
#else
#define ASSERT_ERTS_SACRR_UNIT_SIZE_MULTIPLE(CSZ)
#endif

static void CHECK_1BLK_CARRIER(Allctr_t* A, int SBC, int MSEGED, Carrier_t* C,
			       UWord CSZ, Block_t* B, UWord BSZ) 
{
    ASSERT(IS_LAST_BLK((B)));
    ASSERT((CSZ) == CARRIER_SZ((C)));
    ASSERT((BSZ) % sizeof(Unit_t) == 0);
    if ((SBC)) {
	ASSERT((BSZ) == SBC_BLK_SZ((B)));
	ASSERT((char*)B == (char*)C + SBC_HEADER_SIZE);
	ASSERT(IS_SBC_BLK((B)));
	ASSERT(IS_SB_CARRIER((C)));
    }
    else {
	ASSERT(IS_FREE_BLK(B));
	ASSERT((BSZ) == MBC_FBLK_SZ((B)));
	ASSERT(IS_MBC_FIRST_FBLK(A, (B)));
	ASSERT(IS_MBC_BLK((B)));
	ASSERT(IS_MB_CARRIER((C)));
	ASSERT(FBLK_TO_MBC(B) == (C));
	if ((MSEGED)) {
	    ASSERT_ERTS_SACRR_UNIT_SIZE_MULTIPLE((CSZ));
	}
    }
    if ((MSEGED)) {
	ASSERT(IS_MSEG_CARRIER((C)));
    }
    else {
	ASSERT(IS_SYS_ALLOC_CARRIER((C)));
	ASSERT((CSZ) % sizeof(Unit_t) == 0);
    }
}

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
    Uint mseg_flags;
#endif
#ifdef DEBUG
    int is_mseg = 0;
#endif

    if ((ERTS_SUPER_ALIGNED_MSEG_ONLY && (flags & CFLG_MBC))
	|| !allow_sys_alloc_carriers) {
	flags |= CFLG_FORCE_MSEG;
	flags &= ~CFLG_FORCE_SYS_ALLOC;
#if !HAVE_ERTS_MSEG
	return NULL;
#endif
    }
    flags |= allctr->crr_set_flgs;
    flags &= ~allctr->crr_clr_flgs;

    ASSERT((flags & CFLG_SBC && !(flags & CFLG_MBC))
	   || (flags & CFLG_MBC && !(flags & CFLG_SBC)));

    ASSERT(!(flags & CFLG_FORCE_MSEG && flags & CFLG_FORCE_SYS_ALLOC));

    if (umem_sz > (ERTS_UINT_MAX - ERTS_UINT_MAX/100)) {
	/* Do an overly conservative _overflow_ check here so we don't
	 * have to deal with it from here on. I guess we could be more accurate
	 * but I don't think the need to allocate over 99% of the address space
	 * will ever arise on any machine, neither 32 nor 64 bit.
	 */
	return NULL;
    }

    if (flags & CFLG_MAIN_CARRIER) {
        ASSERT(flags & CFLG_MBC);
        ASSERT(flags & CFLG_NO_CPOOL);
        ASSERT(umem_sz == allctr->main_carrier_size);
        ERTS_UNDEF(blk_sz, 0);

        if (allctr->main_carrier_size < allctr->min_mbc_size)
            allctr->main_carrier_size = allctr->min_mbc_size;
        crr_sz = bcrr_sz = allctr->main_carrier_size;
    }
    else {
        ERTS_UNDEF(bcrr_sz, 0);
	ERTS_UNDEF(crr_sz, 0);
        blk_sz = UMEMSZ2BLKSZ(allctr, umem_sz);
    }

#ifdef ERTS_SMP
    allctr->cpool.disable_abandon = ERTS_ALC_CPOOL_MAX_DISABLE_ABANDON;

    if ((flags & (CFLG_MBC|CFLG_NO_CPOOL)) == CFLG_MBC
	&& ERTS_ALC_IS_CPOOL_ENABLED(allctr)
	&& erts_thr_progress_is_managed_thread()) {
	crr = cpool_fetch(allctr, blk_sz);
	if (crr) {
	    STAT_MBC_CPOOL_FETCH(allctr, crr);
	    link_carrier(&allctr->mbc_list, crr);
	    (*allctr->add_mbc)(allctr, crr);
	    blk = (*allctr->get_free_block)(allctr, blk_sz, NULL, 0);
	    ASSERT(blk);
	    return blk;
	}
    }
#endif

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
#if !ERTS_SUPER_ALIGNED_MSEG_ONLY
    else {
	if (allctr->mbcs.curr.norm.mseg.no >= allctr->max_mseg_mbcs)
	    goto try_sys_alloc;
    }
#endif

 try_mseg:

    if (flags & CFLG_SBC) {
	crr_sz = blk_sz + SBC_HEADER_SIZE;
	mseg_flags = ERTS_MSEG_FLG_NONE;
    }
    else {
        if (!(flags & CFLG_MAIN_CARRIER)) {
            crr_sz = (*allctr->get_next_mbc_size)(allctr);
            if (crr_sz < MBC_HEADER_SIZE(allctr) + blk_sz)
                crr_sz = MBC_HEADER_SIZE(allctr) + blk_sz;
        }
        mseg_flags = ERTS_MSEG_FLG_2POW;
    }

    crr = (Carrier_t *) allctr->mseg_alloc(allctr, &crr_sz, mseg_flags);
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
	SET_CARRIER_HDR(crr, crr_sz, SCH_MSEG|SCH_SBC, allctr);
	STAT_MSEG_SBC_ALLOC(allctr, crr_sz, blk_sz);
	goto sbc_final_touch;
    }
    else {
#ifndef ARCH_64
	ASSERT(crr_sz <= MBC_SZ_MAX_LIMIT);
#endif
	SET_CARRIER_HDR(crr, crr_sz, SCH_MSEG|SCH_MBC, allctr);
	STAT_MSEG_MBC_ALLOC(allctr, crr_sz);
	goto mbc_final_touch;
    }

 try_sys_alloc:

#endif /* #if HAVE_ERTS_MSEG */

    if (flags & CFLG_SBC) {
	bcrr_sz = blk_sz + SBC_HEADER_SIZE;
    }
    else if (!(flags & CFLG_MAIN_CARRIER)) {
	bcrr_sz = MBC_HEADER_SIZE(allctr) + blk_sz;
	if (bcrr_sz < allctr->smallest_mbc_size)
            bcrr_sz = allctr->smallest_mbc_size;
    }

    crr_sz = (flags & CFLG_FORCE_SIZE
	      ? UNIT_CEILING(bcrr_sz)
	      : SYS_ALLOC_CARRIER_CEILING(bcrr_sz));

    crr = (Carrier_t *) allctr->sys_alloc(allctr, &crr_sz, flags & CFLG_MBC);
	
    if (!crr) {
	if (crr_sz > UNIT_CEILING(bcrr_sz)) {
	    crr_sz = UNIT_CEILING(bcrr_sz);
	    crr = (Carrier_t *) allctr->sys_alloc(allctr, &crr_sz, flags & CFLG_MBC);
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
	SET_CARRIER_HDR(crr, crr_sz, SCH_SYS_ALLOC|SCH_SBC, allctr);
	STAT_SYS_ALLOC_SBC_ALLOC(allctr, crr_sz, blk_sz);

#if HAVE_ERTS_MSEG
    sbc_final_touch:
#endif

	blk = SBC2BLK(allctr, crr);

	SET_SBC_BLK_HDR(blk, blk_sz);

	link_carrier(&allctr->sbc_list, crr);

	CHECK_1BLK_CARRIER(allctr, 1, is_mseg, crr, crr_sz, blk, blk_sz);

    }
    else {
	SET_CARRIER_HDR(crr, crr_sz, SCH_SYS_ALLOC|SCH_MBC, allctr);
	STAT_SYS_ALLOC_MBC_ALLOC(allctr, crr_sz);

#if HAVE_ERTS_MSEG
    mbc_final_touch:
#endif

	blk = MBC_TO_FIRST_BLK(allctr, crr);

	blk_sz = UNIT_FLOOR(crr_sz - MBC_HEADER_SIZE(allctr));

	SET_MBC_FBLK_HDR(blk, blk_sz, SBH_THIS_FREE|SBH_LAST_BLK, crr);

	if (flags & CFLG_MAIN_CARRIER) {
	    ASSERT(!allctr->main_carrier);
	    allctr->main_carrier = crr;
	}

#ifdef ERTS_SMP
	cpool_init_carrier_data(allctr, crr);
#endif

	link_carrier(&allctr->mbc_list, crr);

	CHECK_1BLK_CARRIER(allctr, 0, is_mseg, crr, crr_sz, blk, blk_sz);
	if (allctr->creating_mbc)
	    (*allctr->creating_mbc)(allctr, crr);

    }

#ifdef USE_LTTNG_VM_TRACEPOINTS
    if (LTTNG_ENABLED(carrier_create)) {
        lttng_decl_carrier_stats(mbc_stats);
        lttng_decl_carrier_stats(sbc_stats);
        LTTNG_CARRIER_STATS_TO_LTTNG_STATS(&(allctr->mbcs), mbc_stats);
        LTTNG_CARRIER_STATS_TO_LTTNG_STATS(&(allctr->sbcs), sbc_stats);
        LTTNG5(carrier_create,
                ERTS_ALC_A2AD(allctr->alloc_no),
                allctr->ix,
                crr_sz,
                mbc_stats,
                sbc_stats);
    }
#endif

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

    old_blk_sz = SBC_BLK_SZ(old_blk);
    old_crr = BLK_TO_SBC(old_blk);
    old_crr_sz = CARRIER_SZ(old_crr);
    ASSERT(IS_SB_CARRIER(old_crr));
    ASSERT(IS_SBC_BLK(old_blk));

    new_blk_sz = UMEMSZ2BLKSZ(allctr, umem_sz);

#if HAVE_ERTS_MSEG

    if (IS_MSEG_CARRIER(old_crr)) {
	STAT_MSEG_SBC_FREE(allctr, old_crr_sz, old_blk_sz);

	if (!(flags & CFLG_FORCE_SYS_ALLOC)) {

	    new_crr_sz = new_blk_sz + SBC_HEADER_SIZE;
	    new_crr_sz = ERTS_SACRR_UNIT_CEILING(new_crr_sz);
	    new_crr = (Carrier_t *) allctr->mseg_realloc(allctr,
						      old_crr,
						      old_crr_sz,
						      &new_crr_sz);
	    if (new_crr) {
		SET_CARRIER_SZ(new_crr, new_crr_sz);
		new_blk = SBC2BLK(allctr, new_crr);
		SET_SBC_BLK_SZ(new_blk, new_blk_sz);
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
            allctr->mseg_dealloc(allctr, old_crr, old_crr_sz, ERTS_MSEG_FLG_NONE);
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
	    new_bcrr_sz = new_blk_sz + SBC_HEADER_SIZE;
	    new_crr_sz = (flags & CFLG_FORCE_SIZE
			  ? UNIT_CEILING(new_bcrr_sz)
			  : SYS_ALLOC_CARRIER_CEILING(new_bcrr_sz));

	    new_crr = (Carrier_t *) allctr->sys_realloc(allctr,
						     (void *) old_crr,
						     &new_crr_sz,
						     old_crr_sz,
						     0);
	    if (new_crr) {
	    sys_realloc_success:
		SET_CARRIER_SZ(new_crr, new_crr_sz);
		new_blk = SBC2BLK(allctr, new_crr);
		SET_SBC_BLK_SZ(new_blk, new_blk_sz);
		STAT_SYS_ALLOC_SBC_FREE(allctr, old_crr_sz, old_blk_sz);
		STAT_SYS_ALLOC_SBC_ALLOC(allctr, new_crr_sz, new_blk_sz);
		relink_carrier(&allctr->sbc_list, new_crr);
		CHECK_1BLK_CARRIER(allctr, 1, 0, new_crr, new_crr_sz,
				   new_blk, new_blk_sz);
		DEBUG_SAVE_ALIGNMENT(new_crr);
		return new_blk;
	    }
	    else if (new_crr_sz > UNIT_CEILING(new_bcrr_sz)) {
		new_crr_sz = new_blk_sz + SBC_HEADER_SIZE;
		new_crr_sz = UNIT_CEILING(new_crr_sz);
		new_crr = (Carrier_t *) allctr->sys_realloc(allctr,
							 (void *) old_crr,
							 &new_crr_sz,
							 old_crr_sz,
							 0);
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
	    allctr->sys_dealloc(allctr, old_crr, CARRIER_SZ(old_crr), 0);
	}
	else {
	    /* Old carrier unchanged; restore... */
	    STAT_SYS_ALLOC_SBC_ALLOC(allctr, old_crr_sz, old_blk_sz);
	}
	return new_blk;
    }
#endif
}

static void
dealloc_carrier(Allctr_t *allctr, Carrier_t *crr, int superaligned)
{
#if HAVE_ERTS_MSEG
    if (IS_MSEG_CARRIER(crr))
	allctr->mseg_dealloc(allctr, crr, CARRIER_SZ(crr),
			  (superaligned
			   ? ERTS_MSEG_FLG_2POW
			   : ERTS_MSEG_FLG_NONE));
    else
#endif
	allctr->sys_dealloc(allctr, crr, CARRIER_SZ(crr), superaligned);
}

static void
destroy_carrier(Allctr_t *allctr, Block_t *blk, Carrier_t **busy_pcrr_pp)
{
    Uint crr_sz;
    Carrier_t *crr;

    if (IS_SBC_BLK(blk)) {
	Uint blk_sz = SBC_BLK_SZ(blk);
	crr = BLK_TO_SBC(blk);
	crr_sz = CARRIER_SZ(crr);

	ASSERT(IS_LAST_BLK(blk));

	HARD_CHECK_BLK_CARRIER(allctr, blk);

#if HAVE_ERTS_MSEG
	if (IS_MSEG_CARRIER(crr)) {
	    STAT_MSEG_SBC_FREE(allctr, crr_sz, blk_sz);
	}
	else
#endif
	    STAT_SYS_ALLOC_SBC_FREE(allctr, crr_sz, blk_sz);

	unlink_carrier(&allctr->sbc_list, crr);

	dealloc_carrier(allctr, crr, 0);
    }
    else {
	ASSERT(IS_MBC_FIRST_FBLK(allctr, blk));
	crr = FIRST_BLK_TO_MBC(allctr, blk);
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

#ifdef ERTS_SMP
	if (busy_pcrr_pp && *busy_pcrr_pp) {
	    ERTS_ALC_CPOOL_ASSERT(*busy_pcrr_pp == crr);
	    *busy_pcrr_pp = NULL;
	    ERTS_ALC_CPOOL_ASSERT(erts_smp_atomic_read_nob(&crr->allctr)
				  == (((erts_aint_t) allctr)
				      | ERTS_CRR_ALCTR_FLG_IN_POOL
				      | ERTS_CRR_ALCTR_FLG_BUSY));
	    erts_smp_atomic_set_nob(&crr->allctr, ((erts_aint_t) allctr));
	    cpool_delete(allctr, allctr, crr);
	}
	else
#endif
	{
	    unlink_carrier(&allctr->mbc_list, crr);
#if HAVE_ERTS_MSEG
	    if (IS_MSEG_CARRIER(crr)) {
		ASSERT(crr_sz % ERTS_SACRR_UNIT_SZ == 0);
		STAT_MSEG_MBC_FREE(allctr, crr_sz);
	    }
	    else
#endif
		STAT_SYS_ALLOC_MBC_FREE(allctr, crr_sz);

            if (allctr->remove_mbc)
                allctr->remove_mbc(allctr, crr);
	}

#ifdef USE_LTTNG_VM_TRACEPOINTS
        if (LTTNG_ENABLED(carrier_destroy)) {
            lttng_decl_carrier_stats(mbc_stats);
            lttng_decl_carrier_stats(sbc_stats);
            LTTNG_CARRIER_STATS_TO_LTTNG_STATS(&(allctr->mbcs), mbc_stats);
            LTTNG_CARRIER_STATS_TO_LTTNG_STATS(&(allctr->sbcs), sbc_stats);
            LTTNG5(carrier_destroy,
                ERTS_ALC_A2AD(allctr->alloc_no),
                allctr->ix,
                crr_sz,
                mbc_stats,
                sbc_stats);
        }
#endif

#ifdef ERTS_SMP
	schedule_dealloc_carrier(allctr, crr);
#else
	dealloc_mbc(allctr, crr);
#endif
    }
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
    Eterm acul;

#if HAVE_ERTS_MSEG
    Eterm mmc;
#endif
    Eterm ycs;
    Eterm sac;

    Eterm fix_types;

    Eterm mbcs;
#ifdef ERTS_SMP
    Eterm mbcs_pool;
#endif
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
	AM_INIT(acul);

#if HAVE_ERTS_MSEG
	AM_INIT(mmc);
#endif
	AM_INIT(ycs);
	AM_INIT(sac);

	AM_INIT(fix_types);

	AM_INIT(mbcs);
#ifdef ERTS_SMP
	AM_INIT(mbcs_pool);
#endif
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

	for (ix = 0; ix < ERTS_ALC_NO_FIXED_SIZES; ix++) {
	    ErtsAlcType_t n = ERTS_ALC_N_MIN_A_FIXED_SIZE + ix;
	    char *name = (char *) ERTS_ALC_N2TD(n);
	    size_t len = strlen(name);
	    fix_type_atoms[ix] = am_atom_put(name, len);
	}
    }
    
    if (allctr && !allctr->atoms_initialized) {

	make_name_atoms(allctr);

	(*allctr->init_atoms)();

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
 *
 * Note, bld_unstable_uint() should have been called bld_unstable_uword()
 * but we do not want to rename it...
 */
static ERTS_INLINE Eterm
bld_unstable_uint(Uint **hpp, Uint *szp, UWord ui)
{
    Eterm res = THE_NON_VALUE;
    if (szp)
	*szp += BIG_UWORD_HEAP_SIZE(~((UWord) 0));
    if (hpp) {
	if (IS_USMALL(0, ui))
	    res = make_small(ui);
	else {
	    res = uword_to_big(ui, *hpp);
	    *hpp += BIG_UWORD_HEAP_SIZE(ui);
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

static ERTS_INLINE void
add_fix_types(Allctr_t *allctr, int internal, Uint **hpp, Uint *szp,
	      Eterm *lp, Eterm fix)
{
    if (allctr->fix) {
	if (!ERTS_ALC_IS_CPOOL_ENABLED(allctr))
	    add_2tup(hpp, szp, lp, am.fix_types, fix);
	else if (internal)
	    add_3tup(hpp, szp, lp,
		     am.fix_types,
		     erts_bld_uword(hpp, szp, ~((UWord) 0)),
		     fix);
    }
}

static Eterm
sz_info_fix(Allctr_t *allctr,
	    int internal,
	    int *print_to_p,
	    void *print_to_arg,
	    Uint **hpp,
	    Uint *szp)
{
    Eterm res;
    int ix;

    ASSERT(allctr->fix);

    res = NIL;

    if (ERTS_ALC_IS_CPOOL_ENABLED(allctr)) {

	if (internal) {
	    for (ix = ERTS_ALC_NO_FIXED_SIZES-1; ix >= 0; ix--) {
		ErtsAlcFixList_t *fix = &allctr->fix[ix];
		UWord alloced = fix->type_size * fix->u.cpool.allocated;
		UWord used = fix->type_size * fix->u.cpool.used;

		if (print_to_p) {
		    int to = *print_to_p;
		    void *arg = print_to_arg;
		    erts_print(to,
			       arg,
			       "fix type internal: %s %bpu %bpu\n",
			       (char *) ERTS_ALC_N2TD(ERTS_ALC_N_MIN_A_FIXED_SIZE
						      + ix),
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
	}
    }
    else {

	for (ix = ERTS_ALC_NO_FIXED_SIZES-1; ix >= 0; ix--) {
	    ErtsAlcFixList_t *fix = &allctr->fix[ix];
	    UWord alloced = fix->type_size * fix->u.nocpool.allocated;
	    UWord used = fix->type_size*fix->u.nocpool.used;

	    if (print_to_p) {
		int to = *print_to_p;
		void *arg = print_to_arg;
		erts_print(to,
			   arg,
			   "fix type: %s %bpu %bpu\n",
			   (char *) ERTS_ALC_N2TD(ERTS_ALC_N_MIN_A_FIXED_SIZE
						  + ix),
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
    UWord curr_size = cs->curr.norm.mseg.size + cs->curr.norm.sys_alloc.size;

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

#ifdef ERTS_SMP

static Eterm
info_cpool(Allctr_t *allctr,
	   int sz_only,
	   char *prefix,
	   int *print_to_p,
	   void *print_to_arg,
	   Uint **hpp,
	   Uint *szp)
{
    Eterm res = THE_NON_VALUE;
    UWord noc, csz, nob, bsz;

    noc = csz = nob = bsz = ~0;
    if (print_to_p || hpp) {
	if (sz_only)
	    cpool_read_stat(allctr, NULL, &csz, NULL, &bsz);
	else
	    cpool_read_stat(allctr, &noc, &csz, &nob, &bsz);
    }

    if (print_to_p) {
	int to = *print_to_p;
	void *arg = print_to_arg;
	if (!sz_only)
	    erts_print(to, arg, "%sblocks: %bpu\n", prefix, nob);
	erts_print(to, arg, "%sblocks size: %bpu\n", prefix, bsz);
	if (!sz_only)
	    erts_print(to, arg, "%scarriers: %bpu\n", prefix, noc);
	erts_print(to, arg, "%scarriers size: %bpu\n", prefix, csz);
    }

    if (hpp || szp) {
	res = NIL;
	add_2tup(hpp, szp, &res,
		 am.carriers_size,
		 bld_unstable_uint(hpp, szp, csz));
	if (!sz_only)
	    add_2tup(hpp, szp, &res,
		     am.carriers,
		     bld_unstable_uint(hpp, szp, noc));
	add_2tup(hpp, szp, &res,
		 am.blocks_size,
		 bld_unstable_uint(hpp, szp, bsz));
	if (!sz_only)
	    add_2tup(hpp, szp, &res,
		     am.blocks,
		     bld_unstable_uint(hpp, szp, nob));
    }

    return res;
}

#endif /* ERTS_SMP */

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
    UWord curr_no, curr_size;
    
    curr_no = cs->curr.norm.mseg.no + cs->curr.norm.sys_alloc.no;
    curr_size = cs->curr.norm.mseg.size + cs->curr.norm.sys_alloc.size;

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
		   cs->curr.norm.mseg.no);
#endif
	erts_print(to,
		   arg,
		   "%ssys_alloc carriers: %bpu\n",
		   prefix,
		   cs->curr.norm.sys_alloc.no);
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
		   cs->curr.norm.mseg.size);
#endif
	erts_print(to,
		   arg,
		   "%ssys_alloc carriers size: %bpu\n",
		   prefix,
		   cs->curr.norm.sys_alloc.size);
    }

    if (hpp || szp) {
	res = NIL;
	add_2tup(hpp, szp, &res,
		 am.sys_alloc_carriers_size,
		 bld_unstable_uint(hpp, szp, cs->curr.norm.sys_alloc.size));
#if HAVE_ERTS_MSEG
	add_2tup(hpp, szp, &res,
		 am.mseg_alloc_carriers_size,
		 bld_unstable_uint(hpp, szp, cs->curr.norm.mseg.size));
#endif
	add_4tup(hpp, szp, &res,
		 am.carriers_size,
		 bld_unstable_uint(hpp, szp, curr_size),
		 bld_unstable_uint(hpp, szp, cs->max.size),
		 bld_unstable_uint(hpp, szp, cs->max_ever.size));
	add_2tup(hpp, szp, &res,
		 am.sys_alloc_carriers,
		 bld_unstable_uint(hpp, szp, cs->curr.norm.sys_alloc.no));
#if HAVE_ERTS_MSEG
	add_2tup(hpp, szp, &res,
		 am.mseg_alloc_carriers,
		 bld_unstable_uint(hpp, szp, cs->curr.norm.mseg.no));
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
    char buf[MAX_ATOM_CHARACTERS];
    size_t prefix_len = strlen(allctr->name_prefix);

    if (prefix_len > MAX_ATOM_CHARACTERS + sizeof(realloc) - 1)
	erts_exit(ERTS_ERROR_EXIT,"Too long allocator name: %salloc\n",allctr->name_prefix);

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
	erts_print(TO, TOA, "%s calls: %b64u\n", NAME, CC)

#define PRINT_CC_5(TO, TOA, PRFX, NAME, CC)				\
	erts_print(TO, TOA, "%s%s calls: %b64u\n",PRFX,NAME,CC)

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
		 bld_unstable_uint(hpp, szp, ERTS_ALC_CC_GIGA_VAL(allctr->calls.sys_realloc)),
		 bld_unstable_uint(hpp, szp, ERTS_ALC_CC_VAL(allctr->calls.sys_realloc)));
	add_3tup(hpp, szp, &res,
		 am.sys_free,
		 bld_unstable_uint(hpp, szp, ERTS_ALC_CC_GIGA_VAL(allctr->calls.sys_free)),
		 bld_unstable_uint(hpp, szp, ERTS_ALC_CC_VAL(allctr->calls.sys_free)));
	add_3tup(hpp, szp, &res,
		 am.sys_alloc,
		 bld_unstable_uint(hpp, szp, ERTS_ALC_CC_GIGA_VAL(allctr->calls.sys_alloc)),
		 bld_unstable_uint(hpp, szp, ERTS_ALC_CC_VAL(allctr->calls.sys_alloc)));
#if HAVE_ERTS_MSEG
	add_3tup(hpp, szp, &res,
		 am.mseg_realloc,
		 bld_unstable_uint(hpp, szp, ERTS_ALC_CC_GIGA_VAL(allctr->calls.mseg_realloc)),
		 bld_unstable_uint(hpp, szp, ERTS_ALC_CC_VAL(allctr->calls.mseg_realloc)));
	add_3tup(hpp, szp, &res,
		 am.mseg_dealloc,
		 bld_unstable_uint(hpp, szp, ERTS_ALC_CC_GIGA_VAL(allctr->calls.mseg_dealloc)),
		 bld_unstable_uint(hpp, szp, ERTS_ALC_CC_VAL(allctr->calls.mseg_dealloc)));
	add_3tup(hpp, szp, &res,
		 am.mseg_alloc,
		 bld_unstable_uint(hpp, szp, ERTS_ALC_CC_GIGA_VAL(allctr->calls.mseg_alloc)),
		 bld_unstable_uint(hpp, szp, ERTS_ALC_CC_VAL(allctr->calls.mseg_alloc)));
#endif
	add_3tup(hpp, szp, &res,
		 allctr->name.realloc,
		 bld_unstable_uint(hpp, szp, ERTS_ALC_CC_GIGA_VAL(allctr->calls.this_realloc)),
		 bld_unstable_uint(hpp, szp, ERTS_ALC_CC_VAL(allctr->calls.this_realloc)));
	add_3tup(hpp, szp, &res,
		 allctr->name.free,
		 bld_unstable_uint(hpp, szp, ERTS_ALC_CC_GIGA_VAL(allctr->calls.this_free)),
		 bld_unstable_uint(hpp, szp, ERTS_ALC_CC_VAL(allctr->calls.this_free)));
	add_3tup(hpp, szp, &res,
		 allctr->name.alloc,
		 bld_unstable_uint(hpp, szp, ERTS_ALC_CC_GIGA_VAL(allctr->calls.this_alloc)),
		 bld_unstable_uint(hpp, szp, ERTS_ALC_CC_VAL(allctr->calls.this_alloc)));
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
    int acul;

    if (!allctr) {
	if (print_to_p)
	    erts_print(*print_to_p, print_to_arg, "option e: false\n");
	if (hpp || szp) {
	    res = NIL;
	    add_2tup(hpp, szp, &res, am.e, am_false);
	}
	return res;
    }

#ifdef ERTS_SMP
    acul = allctr->cpool.util_limit;
#else
    acul = 0;
#endif

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
		   "option acul: %d\n",
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
		   allctr->mbc_growth_stages,
		   acul);
    }

    res = (*allctr->info_options)(allctr, "option ", print_to_p, print_to_arg,
				  hpp, szp);

    if (hpp || szp) {
	add_2tup(hpp, szp, &res,
		 am.acul,
		 bld_uint(hpp, szp, (UWord) acul));
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
		   "option ycs: %beu\n"
		   "option sac: %s\n",
#if HAVE_ERTS_MSEG
		   max_mseg_carriers,
#endif
		   sys_alloc_carrier_size,
		   allow_sys_alloc_carriers ? "true" : "false");
    }

    if (hpp || szp) {
	res = NIL;
	ensure_atoms_initialized(NULL);
	add_2tup(hpp, szp, &res,
		 am.sac,
		 allow_sys_alloc_carriers ? am_true : am_false);
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

    if (hpp || szp)
	ensure_atoms_initialized(allctr);

#ifdef USE_THREADS
    if (allctr->thread_safe) {
	erts_allctr_wrapper_pre_lock();
	erts_mtx_lock(&allctr->mutex);
    }
#endif
    res = info_options(allctr, print_to_p, print_to_arg, hpp, szp);
#ifdef USE_THREADS
    if (allctr->thread_safe) { 
	erts_mtx_unlock(&allctr->mutex);
	erts_allctr_wrapper_pre_unlock();
    }
#endif
    return res;
}

/* ----------------------------------------------------------------------- */

Eterm
erts_alcu_sz_info(Allctr_t *allctr,
		  int internal,
		  int begin_max_period,
		  int *print_to_p,
		  void *print_to_arg,
		  Uint **hpp,
		  Uint *szp)
{
    Eterm res, mbcs, sbcs, fix = THE_NON_VALUE;
#ifdef ERTS_SMP
    Eterm mbcs_pool;
#endif

    res  = THE_NON_VALUE;

    if (!allctr) {
	if (print_to_p)
	    erts_print(*print_to_p, print_to_arg, "false\n");
	if (szp)
	    *szp = 0;
	return am_false;
    }

    if (hpp || szp)
	ensure_atoms_initialized(allctr);

#ifdef USE_THREADS
    if (allctr->thread_safe) {
	erts_allctr_wrapper_pre_lock();
	erts_mtx_lock(&allctr->mutex);
    }
#endif

    ERTS_ALCU_DBG_CHK_THR_ACCESS(allctr);

    /* Update sbc values not continously updated */
    allctr->sbcs.blocks.curr.no
	= allctr->sbcs.curr.norm.mseg.no + allctr->sbcs.curr.norm.sys_alloc.no;
    allctr->sbcs.blocks.max.no = allctr->sbcs.max.no;

    update_max_ever_values(&allctr->mbcs);
    update_max_ever_values(&allctr->sbcs);

    if (allctr->fix)
	fix = sz_info_fix(allctr, internal, print_to_p, print_to_arg, hpp, szp);
    mbcs = sz_info_carriers(allctr, &allctr->mbcs, "mbcs ", print_to_p,
			    print_to_arg, hpp, szp);
#ifdef ERTS_SMP
    if (ERTS_ALC_IS_CPOOL_ENABLED(allctr))
	mbcs_pool = info_cpool(allctr, 1, "mbcs_pool ", print_to_p,
			       print_to_arg, hpp, szp);
    else
	mbcs_pool = THE_NON_VALUE; /* shut up annoying warning... */
#endif
    sbcs = sz_info_carriers(allctr, &allctr->sbcs, "sbcs ", print_to_p,
			    print_to_arg, hpp, szp);

    if (hpp || szp) {
	res = NIL;
	add_2tup(hpp, szp, &res, am.sbcs, sbcs);
#ifdef ERTS_SMP
	if (ERTS_ALC_IS_CPOOL_ENABLED(allctr))
	    add_2tup(hpp, szp, &res, am.mbcs_pool, mbcs_pool);
#endif
	add_2tup(hpp, szp, &res, am.mbcs, mbcs);
	add_fix_types(allctr, internal, hpp, szp, &res, fix);
    }

    if (begin_max_period) {
	reset_max_values(&allctr->mbcs);
	reset_max_values(&allctr->sbcs);
    }


#ifdef USE_THREADS
    if (allctr->thread_safe) {
	erts_mtx_unlock(&allctr->mutex);
	erts_allctr_wrapper_pre_unlock();
    }
#endif

    return res;
}


Eterm
erts_alcu_info(Allctr_t *allctr,
	       int internal,
	       int begin_max_period,
	       int *print_to_p,
	       void *print_to_arg,
	       Uint **hpp,
	       Uint *szp)
{
    Eterm res, sett, mbcs, sbcs, calls, fix = THE_NON_VALUE;
#ifdef ERTS_SMP
    Eterm mbcs_pool;
#endif

    res  = THE_NON_VALUE;

    if (!allctr) {
	if (print_to_p)
	    erts_print(*print_to_p, print_to_arg, "false\n");
	if (szp)
	    *szp = 0;
	return am_false;
    }

    if (hpp || szp)
	ensure_atoms_initialized(allctr);

#ifdef USE_THREADS
    if (allctr->thread_safe) {
	erts_allctr_wrapper_pre_lock();
	erts_mtx_lock(&allctr->mutex);
    }
#endif

    ERTS_ALCU_DBG_CHK_THR_ACCESS(allctr);

    /* Update sbc values not continously updated */
    allctr->sbcs.blocks.curr.no
	= allctr->sbcs.curr.norm.mseg.no + allctr->sbcs.curr.norm.sys_alloc.no;
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

    sett = info_options(allctr, print_to_p, print_to_arg, hpp, szp);
    if (allctr->fix)
	fix = sz_info_fix(allctr, internal, print_to_p, print_to_arg, hpp, szp);
    mbcs = info_carriers(allctr, &allctr->mbcs, "mbcs ", print_to_p,
			 print_to_arg, hpp, szp);
#ifdef ERTS_SMP
    if (ERTS_ALC_IS_CPOOL_ENABLED(allctr))
	mbcs_pool = info_cpool(allctr, 0, "mbcs_pool ", print_to_p,
			       print_to_arg, hpp, szp);
    else
	mbcs_pool = THE_NON_VALUE; /* shut up annoying warning... */
#endif
    sbcs = info_carriers(allctr, &allctr->sbcs, "sbcs ", print_to_p,
			 print_to_arg, hpp, szp);
    calls = info_calls(allctr, print_to_p, print_to_arg, hpp, szp);

    if (hpp || szp) {
	res = NIL;

	add_2tup(hpp, szp, &res, am.calls, calls);
	add_2tup(hpp, szp, &res, am.sbcs, sbcs);
#ifdef ERTS_SMP
	if (ERTS_ALC_IS_CPOOL_ENABLED(allctr))
	    add_2tup(hpp, szp, &res, am.mbcs_pool, mbcs_pool);
#endif
	add_2tup(hpp, szp, &res, am.mbcs, mbcs);
	add_fix_types(allctr, internal, hpp, szp, &res, fix);
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
    if (allctr->thread_safe) {
	erts_mtx_unlock(&allctr->mutex);
	erts_allctr_wrapper_pre_unlock();
    }
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
    size->carriers += allctr->sbcs.curr.norm.mseg.size;
    size->carriers += allctr->sbcs.curr.norm.sys_alloc.size;

    size->blocks = allctr->mbcs.blocks.curr.size;
    size->blocks += allctr->sbcs.blocks.curr.size;

#ifdef ERTS_SMP
    if (ERTS_ALC_IS_CPOOL_ENABLED(allctr)) {
	UWord csz, bsz;
	cpool_read_stat(allctr, NULL, &csz, NULL, &bsz);
	size->blocks += bsz;
	size->carriers += csz;
    }
#endif

    if (fi) {
	int ix;
	for (ix = 0; ix < fisz; ix++) {
	    if (allctr->fix) {
		if (ERTS_ALC_IS_CPOOL_ENABLED(allctr)) {
		    fi[ix].allocated += (allctr->fix[ix].type_size
					 * allctr->fix[ix].u.cpool.allocated);
		    fi[ix].used += (allctr->fix[ix].type_size
				    * allctr->fix[ix].u.cpool.used);
		}
		else {
		    fi[ix].allocated += (allctr->fix[ix].type_size
					 * allctr->fix[ix].u.nocpool.allocated);
		    fi[ix].used += (allctr->fix[ix].type_size
				    * allctr->fix[ix].u.nocpool.used);
		}
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

    if (allctr->fix) {
	if (ERTS_ALC_IS_CPOOL_ENABLED(allctr))
	    return fix_cpool_alloc(allctr, type, size);
	else
	    return fix_nocpool_alloc(allctr, type, size);
    }

    if (size >= allctr->sbc_threshold) {
	Block_t *blk;
	blk = create_carrier(allctr, size, CFLG_SBC);
	res = blk ? BLK2UMEM(blk) : NULL;
    }
    else
	res = mbc_alloc(allctr, size);

    return res;
}

void *erts_alcu_alloc(ErtsAlcType_t type, void *extra, Uint size)
{
    void *res;
#ifdef ERTS_SMP
    ASSERT(!"This is not thread safe");
#elif defined(USE_THREADS)
    ASSERT(erts_equal_tids(erts_main_thread, erts_thr_self()));
#endif
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
    Allctr_t *pref_allctr;
    void *res;

    pref_allctr = get_pref_allctr(extra);

    if (pref_allctr->thread_safe)
	erts_mtx_lock(&pref_allctr->mutex);

#ifdef ERTS_SMP
    ASSERT(pref_allctr->dd.use);
    ERTS_ALCU_HANDLE_DD_IN_OP(pref_allctr, 1);
#endif

    ERTS_ALCU_DBG_CHK_THR_ACCESS(pref_allctr);

    res = do_erts_alcu_alloc(type, pref_allctr, size);

#ifdef ERTS_SMP
    if (!res && ERTS_ALCU_HANDLE_DD_IN_OP(pref_allctr, 1)) {
	/* Cleaned up a bit more; try one more time... */
	res = do_erts_alcu_alloc(type, pref_allctr, size);
    }
#endif

    if (pref_allctr->thread_safe)
	erts_mtx_unlock(&pref_allctr->mutex);

    DEBUG_CHECK_ALIGNMENT(res);


    return res;
}

#endif

#endif

/* ------------------------------------------------------------------------- */

static ERTS_INLINE void
do_erts_alcu_free(ErtsAlcType_t type, void *extra, void *p,
		  Carrier_t **busy_pcrr_pp)
{
    Allctr_t *allctr = (Allctr_t *) extra; 
    ASSERT(initialized);

    ASSERT(allctr);

    ERTS_SMP_LC_ASSERT(!allctr->thread_safe
		       || erts_lc_mtx_is_locked(&allctr->mutex));

    ERTS_ALCU_DBG_CHK_THR_ACCESS(allctr);

    if (p) {

	INC_CC(allctr->calls.this_free);

	if (allctr->fix) {
	    if (ERTS_ALC_IS_CPOOL_ENABLED(allctr))
		fix_cpool_free(allctr, type, p, busy_pcrr_pp, 1);
	    else
		fix_nocpool_free(allctr, type, p);
	}
	else {
	    Block_t *blk = UMEM2BLK(p);
	    if (IS_SBC_BLK(blk))
		destroy_carrier(allctr, blk, NULL);
	    else
		mbc_free(allctr, p, busy_pcrr_pp);
	}
    }
}

void erts_alcu_free(ErtsAlcType_t type, void *extra, void *p)
{
    do_erts_alcu_free(type, extra, p, NULL);
}

#ifdef USE_THREADS

void
erts_alcu_free_ts(ErtsAlcType_t type, void *extra, void *p)
{
    Allctr_t *allctr = (Allctr_t *) extra;
    erts_mtx_lock(&allctr->mutex);
    do_erts_alcu_free(type, extra, p, NULL);
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

    do_erts_alcu_free(type, allctr, p, NULL);

    if (allctr->thread_safe)
	erts_mtx_unlock(&allctr->mutex);
}

void
erts_alcu_free_thr_pref(ErtsAlcType_t type, void *extra, void *p)
{
    if (p) {
	Carrier_t *busy_pcrr_p;
	Allctr_t *pref_allctr, *used_allctr;

	pref_allctr = get_pref_allctr(extra);
	used_allctr = get_used_allctr(pref_allctr, ERTS_ALC_TS_PREF_LOCK_IF_USED,
				      p, NULL, &busy_pcrr_p);
	if (pref_allctr != used_allctr)
	    enqueue_dealloc_other_instance(type,
					   used_allctr,
					   p,
					   (used_allctr->dd.ix
					    - pref_allctr->dd.ix));
	else {
	    ERTS_ALCU_DBG_CHK_THR_ACCESS(used_allctr);
	    do_erts_alcu_free(type, used_allctr, p, &busy_pcrr_p);
	    clear_busy_pool_carrier(used_allctr, busy_pcrr_p);
	    if (pref_allctr->thread_safe)
		erts_mtx_unlock(&pref_allctr->mutex);
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
		     Uint32 alcu_flgs,
		     Carrier_t **busy_pcrr_pp)
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
	do_erts_alcu_free(type, extra, p, busy_pcrr_pp);
	INC_CC(allctr->calls.this_realloc);
	DEC_CC(allctr->calls.this_free);
	return NULL;
    }
#endif

    INC_CC(allctr->calls.this_realloc);
    
    blk = UMEM2BLK(p);

    if (size < allctr->sbc_threshold) {
	if (IS_MBC_BLK(blk))
	    res = mbc_realloc(allctr, p, size, alcu_flgs, busy_pcrr_pp);
	else {
	    Uint used_sz = SBC_HEADER_SIZE + ABLK_HDR_SZ + size;
	    Uint crr_sz;
	    Uint diff_sz_val;
	    Uint crr_sz_val;

#if HAVE_ERTS_MSEG
	    if (IS_SYS_ALLOC_CARRIER(BLK_TO_SBC(blk)))
#endif
		crr_sz = SYS_ALLOC_CARRIER_CEILING(used_sz);
#if HAVE_ERTS_MSEG
	    else
		crr_sz = ERTS_SACRR_UNIT_CEILING(used_sz);
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
			   MIN(SBC_BLK_SZ(blk) - ABLK_HDR_SZ, size));
		destroy_carrier(allctr, blk, NULL);
	    }
	}
    }
    else {
	Block_t *new_blk;
	if(IS_SBC_BLK(blk)) {
	do_carrier_resize:
	    new_blk = resize_carrier(allctr, blk, size, CFLG_SBC);
	    res = new_blk ? BLK2UMEM(new_blk) : NULL;
	}
	else if (alcu_flgs & ERTS_ALCU_FLG_FAIL_REALLOC_MOVE)
	    return NULL;
	else {
	    new_blk = create_carrier(allctr, size, CFLG_SBC);
	    if (new_blk) {
		res = BLK2UMEM(new_blk);
		sys_memcpy((void *) res,
			   (void *) p,
			   MIN(MBC_ABLK_SZ(blk) - ABLK_HDR_SZ, size));
		mbc_free(allctr, p, busy_pcrr_pp);
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
    res = do_erts_alcu_realloc(type, extra, p, size, 0, NULL);
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
	do_erts_alcu_free(type, extra, p, NULL);
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
    res = do_erts_alcu_realloc(type, extra, ptr, size, 0, NULL);
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
	do_erts_alcu_free(type, extra, p, NULL);
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

    res = do_erts_alcu_realloc(type, allctr, ptr, size, 0, NULL);

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
	do_erts_alcu_free(type, allctr, ptr, NULL);
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
    void *res;
    Allctr_t *pref_allctr, *used_allctr;
    UWord old_user_size;
    Carrier_t *busy_pcrr_p;
#ifdef ERTS_SMP
    int retried;
#endif

    if (!p)
	return erts_alcu_alloc_thr_pref(type, extra, size);

    pref_allctr = get_pref_allctr(extra);

    if (pref_allctr->thread_safe)
	erts_mtx_lock(&pref_allctr->mutex);

#ifdef ERTS_SMP
    ASSERT(pref_allctr->dd.use);
    ERTS_ALCU_HANDLE_DD_IN_OP(pref_allctr, 1);
    retried = 0;
restart:
#endif

    used_allctr = get_used_allctr(pref_allctr, ERTS_ALC_TS_PREF_LOCK_NO,
				  p, &old_user_size, &busy_pcrr_p);

    ASSERT(used_allctr && pref_allctr);

    if (!force_move && used_allctr == pref_allctr) {
	ERTS_ALCU_DBG_CHK_THR_ACCESS(used_allctr);
	res = do_erts_alcu_realloc(type,
				   used_allctr,
				   p,
				   size,
				   0,
				   &busy_pcrr_p);
	clear_busy_pool_carrier(used_allctr, busy_pcrr_p);
#ifdef ERTS_SMP
	if (!res && !retried && ERTS_ALCU_HANDLE_DD_IN_OP(pref_allctr, 1)) {
	    /* Cleaned up a bit more; try one more time... */
	    retried = 1;
	    goto restart;
	}
#endif	    
	if (pref_allctr->thread_safe)
	    erts_mtx_unlock(&pref_allctr->mutex);
    }
    else {
	res = do_erts_alcu_alloc(type, pref_allctr, size);
	if (!res)
	    goto unlock_ts_return;
	else {

	    DEBUG_CHECK_ALIGNMENT(res);

	    if (used_allctr != pref_allctr) {
		if (pref_allctr->thread_safe)
		    erts_mtx_unlock(&pref_allctr->mutex);

		sys_memcpy(res, p, MIN(size, old_user_size));

		enqueue_dealloc_other_instance(type,
					       used_allctr,
					       p,
					       (used_allctr->dd.ix
						- pref_allctr->dd.ix));
	    }
	    else {

		sys_memcpy(res, p, MIN(size, old_user_size));

		do_erts_alcu_free(type, used_allctr, p, &busy_pcrr_p);
		ASSERT(pref_allctr == used_allctr);
		clear_busy_pool_carrier(used_allctr, busy_pcrr_p);

	    unlock_ts_return:
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

    if (((UWord)allctr & ERTS_CRR_ALCTR_FLG_MASK) != 0) {
        erts_exit(ERTS_ABORT_EXIT, "%s:%d:erts_alcu_start: Alignment error\n",
                 __FILE__, __LINE__);
    }

    if (!initialized)
	goto error;

#if HAVE_ERTS_MSEG
    sys_memcpy((void *) &allctr->mseg_opt,
	       (void *) &erts_mseg_default_opt,
	       sizeof(ErtsMsegOpt_t));
#ifdef ERTS_SMP
    if (init->tspec || init->tpref)
	allctr->mseg_opt.sched_spec = 1;
#endif /* ERTS_SMP */
#endif /* HAVE_ERTS_MSEG */

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

#if HAVE_ERTS_MSEG
    allctr->mseg_opt.abs_shrink_th	= init->asbcst;
    allctr->mseg_opt.rel_shrink_th	= init->rsbcst;
#endif
    allctr->sbc_move_threshold		= init->rsbcmt;
    allctr->mbc_move_threshold		= init->rmbcmt;
#if HAVE_ERTS_MSEG
    allctr->max_mseg_sbcs		= init->mmsbc;
# if ERTS_SUPER_ALIGNED_MSEG_ONLY
    allctr->max_mseg_mbcs		= ~(Uint)0;
# else
    allctr->max_mseg_mbcs		= init->mmmbc;
# endif
#endif

    allctr->largest_mbc_size		= MAX(init->lmbcs, init->smbcs);
#ifndef ARCH_64
    if (allctr->largest_mbc_size > MBC_SZ_MAX_LIMIT) {
	allctr->largest_mbc_size = MBC_SZ_MAX_LIMIT;
    }
#endif
    allctr->smallest_mbc_size		= init->smbcs;
    allctr->mbc_growth_stages		= MAX(1, init->mbcgs);

    if (allctr->min_block_size < ABLK_HDR_SZ)
	goto error;
    allctr->min_block_size		= UNIT_CEILING(allctr->min_block_size
						       + sizeof(FreeBlkFtr_t));
#if ERTS_SMP
    if (init->tpref) {
	Uint sz = ABLK_HDR_SZ;
	sz += (init->fix ? 
	       sizeof(ErtsAllctrFixDDBlock_t) : sizeof(ErtsAllctrDDBlock_t));
	sz = UNIT_CEILING(sz);
	if (sz > allctr->min_block_size)
	    allctr->min_block_size = sz;
    }

    allctr->cpool.pooled_list.next  = &allctr->cpool.pooled_list;
    allctr->cpool.pooled_list.prev  = &allctr->cpool.pooled_list;
    allctr->cpool.traitor_list.next = &allctr->cpool.traitor_list;
    allctr->cpool.traitor_list.prev = &allctr->cpool.traitor_list;
    allctr->cpool.dc_list.first = NULL;
    allctr->cpool.dc_list.last = NULL;
    allctr->cpool.abandon_limit = 0;
    allctr->cpool.disable_abandon = 0;
    erts_atomic_init_nob(&allctr->cpool.stat.blocks_size, 0);
    erts_atomic_init_nob(&allctr->cpool.stat.no_blocks, 0);
    erts_atomic_init_nob(&allctr->cpool.stat.carriers_size, 0);
    erts_atomic_init_nob(&allctr->cpool.stat.no_carriers, 0);
    allctr->cpool.check_limit_count = ERTS_ALC_CPOOL_CHECK_LIMIT_COUNT;
    allctr->cpool.util_limit = init->ts ? 0 : init->acul;
#endif

    allctr->sbc_threshold		= init->sbct;
#ifndef ARCH_64
    if (allctr->sbc_threshold > 0) {
	Uint max_mbc_block_sz = UNIT_CEILING(allctr->sbc_threshold - 1 + ABLK_HDR_SZ); 
	if (max_mbc_block_sz + UNIT_FLOOR(allctr->min_block_size - 1) > MBC_ABLK_SZ_MASK
	    || max_mbc_block_sz < allctr->sbc_threshold) { /* wrap around */
	    /* 
	     * By limiting sbc_threshold to (hard limit - min_block_size)
	     * we avoid having to split off free "residue blocks"
	     * smaller than min_block_size.
	     */
	    max_mbc_block_sz = MBC_ABLK_SZ_MASK - UNIT_FLOOR(allctr->min_block_size - 1);
	    allctr->sbc_threshold = max_mbc_block_sz - ABLK_HDR_SZ + 1;
	}
    }
#endif

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
			    ERTS_LCNT_LT_ALLOC,1);
#else
	erts_mtx_init_x(&allctr->mutex,
			"alcu_allocator",
			make_small(allctr->alloc_no),1);
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
	allctr->dd.use = 1;
	init_dd_queue(&allctr->dd.q);
	allctr->dd.ix = init->ix;
    }
#endif
    allctr->mbc_header_size = (UNIT_CEILING(allctr->mbc_header_size
					    + ABLK_HDR_SZ)
			       - ABLK_HDR_SZ);

    if (init->sys_alloc) {
        ASSERT(init->sys_realloc && init->sys_dealloc);
        allctr->sys_alloc   = init->sys_alloc;
        allctr->sys_realloc = init->sys_realloc;
        allctr->sys_dealloc = init->sys_dealloc;
    }
    else {
        ASSERT(!init->sys_realloc && !init->sys_dealloc);
        allctr->sys_alloc   = &erts_alcu_sys_alloc;
        allctr->sys_realloc = &erts_alcu_sys_realloc;
        allctr->sys_dealloc = &erts_alcu_sys_dealloc;
    }
#if HAVE_ERTS_MSEG
    if (init->mseg_alloc) {
        ASSERT(init->mseg_realloc && init->mseg_dealloc);
        allctr->mseg_alloc   = init->mseg_alloc;
        allctr->mseg_realloc = init->mseg_realloc;
        allctr->mseg_dealloc = init->mseg_dealloc;
        allctr->mseg_mmapper = init->mseg_mmapper;
    }
    else {
        ASSERT(!init->mseg_realloc && !init->mseg_dealloc);
        allctr->mseg_alloc   = &erts_alcu_mseg_alloc;
        allctr->mseg_realloc = &erts_alcu_mseg_realloc;
        allctr->mseg_dealloc = &erts_alcu_mseg_dealloc;
    }
    /* If a custom carrier alloc function is specified, make sure it's used */
    if (init->mseg_alloc && !init->sys_alloc) {
        allctr->crr_set_flgs = CFLG_FORCE_MSEG;
        allctr->crr_clr_flgs = CFLG_FORCE_SYS_ALLOC;
    }
    else if (!init->mseg_alloc && init->sys_alloc) {
        allctr->crr_set_flgs = CFLG_FORCE_SYS_ALLOC;
        allctr->crr_clr_flgs = CFLG_FORCE_MSEG;
    }
#endif

    if (allctr->main_carrier_size) {
	Block_t *blk;

	blk = create_carrier(allctr,
			     allctr->main_carrier_size,
                             (ERTS_SUPER_ALIGNED_MSEG_ONLY
                              ? CFLG_FORCE_MSEG : CFLG_FORCE_SYS_ALLOC)
                             | CFLG_MBC
			     | CFLG_FORCE_SIZE
			     | CFLG_NO_CPOOL
			     | CFLG_MAIN_CARRIER);
	if (!blk) {
#ifdef USE_THREADS
	  if (allctr->thread_safe)
	    erts_mtx_destroy(&allctr->mutex);
#endif
	  erts_exit(ERTS_ABORT_EXIT,
	    "Failed to create main carrier for %salloc\n",
	    init->name_prefix);
	}

	(*allctr->link_free_block)(allctr, blk);

	HARD_CHECK_BLK_CARRIER(allctr, blk);

    }

    if (init->fix) {
	int i;
	allctr->fix = init->fix;
	allctr->fix_shrink_scheduled = 0;
	for (i = 0; i < ERTS_ALC_NO_FIXED_SIZES; i++) {
	    allctr->fix[i].type_size = init->fix_type_size[i];
	    allctr->fix[i].list_size = 0;
	    allctr->fix[i].list = NULL;
#ifdef ERTS_SMP
	    ASSERT(allctr->fix[i].type_size >= sizeof(ErtsAllctrFixDDBlock_t));
#endif
	    if (ERTS_ALC_IS_CPOOL_ENABLED(allctr)) {
		allctr->fix[i].u.cpool.min_list_size = 0;
		allctr->fix[i].u.cpool.shrink_list = 0;
		allctr->fix[i].u.cpool.allocated = 0;
		allctr->fix[i].u.cpool.used = 0;
	    }
	    else {
		allctr->fix[i].u.nocpool.max_used = 0;
		allctr->fix[i].u.nocpool.limit = 0;
		allctr->fix[i].u.nocpool.allocated = 0;
		allctr->fix[i].u.nocpool.used = 0;
	    }
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
	destroy_carrier(allctr, SBC2BLK(allctr, allctr->sbc_list.first), NULL);
    while (allctr->mbc_list.first)
	destroy_carrier(allctr, MBC_TO_FIRST_BLK(allctr, allctr->mbc_list.first), NULL);

#ifdef USE_THREADS
    if (allctr->thread_safe)
	erts_mtx_destroy(&allctr->mutex);
#endif

}

/* ------------------------------------------------------------------------- */

void
erts_alcu_init(AlcUInit_t *init)
{
#ifdef ERTS_SMP
    int i;
    for (i = 0; i <= ERTS_ALC_A_MAX; i++) {
	ErtsAlcCPoolData_t *sentinel = &carrier_pool[i].sentinel;
	erts_atomic_init_nob(&sentinel->next, (erts_aint_t) sentinel);
	erts_atomic_init_nob(&sentinel->prev, (erts_aint_t) sentinel);
    }
#endif
    ERTS_CT_ASSERT(SBC_BLK_SZ_MASK == MBC_FBLK_SZ_MASK); /* see BLK_SZ */
#if HAVE_ERTS_MSEG
    ASSERT(erts_mseg_unit_size() == ERTS_SACRR_UNIT_SZ);
    max_mseg_carriers = init->mmc;
    sys_alloc_carrier_size = ERTS_SACRR_UNIT_CEILING(init->ycs);
#else /* #if HAVE_ERTS_MSEG */
    sys_alloc_carrier_size = ((init->ycs + 4095) / 4096) * 4096;
#endif
    allow_sys_alloc_carriers = init->sac;

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

UWord
erts_alcu_test(UWord op, UWord a1, UWord a2)
{
    switch (op) {
    case 0x000:	return (UWord) BLK_SZ((Block_t *) a1);
    case 0x001:	return (UWord) BLK_UMEM_SZ((Block_t *) a1);
    case 0x002:	return (UWord) IS_PREV_BLK_FREE((Block_t *) a1);
    case 0x003:	return (UWord) IS_FREE_BLK((Block_t *) a1);
    case 0x004:	return (UWord) IS_LAST_BLK((Block_t *) a1);
    case 0x005:	return (UWord) UMEM2BLK((void *) a1);
    case 0x006:	return (UWord) BLK2UMEM((Block_t *) a1);
    case 0x007:	return (UWord) IS_SB_CARRIER((Carrier_t *) a1);
    case 0x008:	return (UWord) IS_SBC_BLK((Block_t *) a1);
    case 0x009:	return (UWord) IS_MB_CARRIER((Carrier_t *) a1);
    case 0x00a:	return (UWord) IS_MSEG_CARRIER((Carrier_t *) a1);
    case 0x00b:	return (UWord) CARRIER_SZ((Carrier_t *) a1);
    case 0x00c:	return (UWord) SBC2BLK((Allctr_t *) a1,
					       (Carrier_t *) a2);
    case 0x00d:	return (UWord) BLK_TO_SBC((Block_t *) a2);
    case 0x00e:	return (UWord) MBC_TO_FIRST_BLK((Allctr_t *) a1,
						(Carrier_t *) a2);
    case 0x00f:	return (UWord) FIRST_BLK_TO_MBC((Allctr_t *) a1,
						(Block_t *) a2);
    case 0x010:	return (UWord) ((Allctr_t *) a1)->mbc_list.first;
    case 0x011:	return (UWord) ((Allctr_t *) a1)->mbc_list.last;
    case 0x012:	return (UWord) ((Allctr_t *) a1)->sbc_list.first;
    case 0x013:	return (UWord) ((Allctr_t *) a1)->sbc_list.last;
    case 0x014:	return (UWord) ((Carrier_t *) a1)->next;
    case 0x015:	return (UWord) ((Carrier_t *) a1)->prev;
    case 0x016:	return (UWord) ABLK_HDR_SZ; 
    case 0x017:	return (UWord) ((Allctr_t *) a1)->min_block_size;
    case 0x018:	return (UWord) NXT_BLK((Block_t *) a1);
    case 0x019:	return (UWord) PREV_BLK((Block_t *) a1);
    case 0x01a: return (UWord) IS_MBC_FIRST_BLK((Allctr_t*)a1, (Block_t *) a2);
    case 0x01b: return (UWord) sizeof(Unit_t);
    case 0x01c: return (UWord) BLK_TO_MBC((Block_t*) a1);
    case 0x01d: ((Allctr_t*) a1)->add_mbc((Allctr_t*)a1, (Carrier_t*)a2); break;
    case 0x01e: ((Allctr_t*) a1)->remove_mbc((Allctr_t*)a1, (Carrier_t*)a2); break;
#ifdef ERTS_SMP
    case 0x01f: return (UWord) sizeof(ErtsAlcCrrPool_t);
    case 0x020:
	SET_CARRIER_HDR((Carrier_t *) a2, 0, SCH_SYS_ALLOC|SCH_MBC, (Allctr_t *) a1);
	cpool_init_carrier_data((Allctr_t *) a1, (Carrier_t *) a2);
	return (UWord) a2;
    case 0x021:
	cpool_insert((Allctr_t *) a1, (Carrier_t *) a2);
	return (UWord) a2;
    case 0x022:
	cpool_delete((Allctr_t *) a1, (Allctr_t *) a1, (Carrier_t *) a2);
	return (UWord) a2;
    case 0x023: return (UWord) cpool_is_empty((Allctr_t *) a1);
    case 0x024: return (UWord) cpool_dbg_is_in_pool((Allctr_t *) a1, (Carrier_t *) a2);
#else
    case 0x01f: return (UWord) 0;
    case 0x020: return (UWord) 0;
    case 0x021: return (UWord) 0;
    case 0x022: return (UWord) 0;
    case 0x023: return (UWord) 0;
    case 0x024: return (UWord) 0;
#endif
    case 0x025: /* UMEM2BLK_TEST*/
#ifdef DEBUG
# ifdef HARD_DEBUG
	return (UWord)UMEM2BLK(a1-3*sizeof(UWord));
# else
	return (UWord)UMEM2BLK(a1-2*sizeof(UWord));
# endif
#else
	return (UWord)UMEM2BLK(a1);
#endif

    default:	ASSERT(0); return ~((UWord) 0);
    }
    return 0;
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * Debug functions                                                           *
\*                                                                           */

void
erts_alcu_assert_failed(char* expr, char* file, int line, char *func)
{
    fflush(stdout);
    fprintf(stderr, "%s:%d:%s(): Assertion failed: %s\n",
	    file, line, func, expr);
    fflush(stderr);
#if defined(__WIN__) || defined(__WIN32__)
    DebugBreak();
#else
    abort();
#endif
}

void
erts_alcu_verify_unused(Allctr_t *allctr)
{
    UWord no;

    no = allctr->sbcs.curr.norm.mseg.no;
    no += allctr->sbcs.curr.norm.sys_alloc.no;
    no += allctr->mbcs.blocks.curr.no;

    if (no) {
	UWord sz = allctr->sbcs.blocks.curr.size;
	sz += allctr->mbcs.blocks.curr.size;
	erts_exit(ERTS_ABORT_EXIT,
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

#ifdef DEBUG
int is_sbc_blk(Block_t* blk)
{
    return IS_SBC_BLK(blk);
}
#endif

#ifdef ERTS_ALLOC_UTIL_HARD_DEBUG

static void
check_blk_carrier(Allctr_t *allctr, Block_t *iblk)
{
    Carrier_t *crr;
    CarrierList_t *cl;

    if (IS_SBC_BLK(iblk)) {
	Carrier_t *sbc = BLK_TO_SBC(iblk);

	ASSERT(SBC2BLK(allctr, sbc) == iblk);
	ASSERT(CARRIER_SZ(sbc) - SBC_HEADER_SIZE >= SBC_BLK_SZ(iblk));
#if HAVE_ERTS_MSEG
	if (IS_MSEG_CARRIER(sbc)) {
	    ASSERT(CARRIER_SZ(sbc) % ERTS_SACRR_UNIT_SZ == 0);
	}
#endif
	crr = sbc;
	cl = &allctr->sbc_list;
    }
    else {
	Block_t *prev_blk = NULL;
	Block_t *blk;
	char *carrier_end;
	Uint is_free_blk;
	Uint tot_blk_sz;
	Uint blk_sz;
	int has_wrapped_around = 0;

	blk = iblk;
	tot_blk_sz = 0;
	crr = BLK_TO_MBC(blk);
	ASSERT(IS_MB_CARRIER(crr));

	/* Step around the carrier one whole lap starting at 'iblk'
	 */
	while (1) {
	    ASSERT(IS_MBC_BLK(blk));
	    ASSERT(BLK_TO_MBC(blk) == crr);

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

	    if (has_wrapped_around) {
		ASSERT(((Block_t *) crr) < blk);
		if (blk == iblk)
		    break;
		ASSERT(blk < iblk);
	    }
	    else
		ASSERT(blk >= iblk);

	    blk_sz = MBC_BLK_SZ(blk);

	    ASSERT(blk_sz % sizeof(Unit_t) == 0);
	    ASSERT(blk_sz >= allctr->min_block_size);

	    tot_blk_sz += blk_sz;

	    is_free_blk = (int) IS_FREE_BLK(blk);
	    ASSERT(!is_free_blk
		   || IS_LAST_BLK(blk)
		   || PREV_BLK_SZ(((char *) blk)+blk_sz) == blk_sz);

	    if (allctr->check_block)
		(*allctr->check_block)(allctr, blk, (int) is_free_blk);

	    if (IS_LAST_BLK(blk)) {
		carrier_end = ((char *) NXT_BLK(blk));
		has_wrapped_around = 1;
		prev_blk = NULL;
		blk = MBC_TO_FIRST_BLK(allctr, crr);
		ASSERT(IS_MBC_FIRST_BLK(allctr,blk));
	    }
	    else {
		prev_blk = blk;
		blk = NXT_BLK(blk);
	    }
	}
	
	ASSERT((((char *) crr)
		+ MBC_HEADER_SIZE(allctr)
		+ tot_blk_sz) == carrier_end);
	ASSERT(((char *) crr) + CARRIER_SZ(crr) - sizeof(Unit_t) <= carrier_end
	       && carrier_end <= ((char *) crr) + CARRIER_SZ(crr));

	if (allctr->check_mbc)
	    (*allctr->check_mbc)(allctr, crr);

#if HAVE_ERTS_MSEG
	if (IS_MSEG_CARRIER(crr)) {
	    ASSERT(CARRIER_SZ(crr) % ERTS_SACRR_UNIT_SZ == 0);
	}
#endif
	cl = &allctr->mbc_list;
    }

#ifdef DEBUG
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

#endif /* ERTS_ALLOC_UTIL_HARD_DEBUG */

