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
#include "erl_mmap.h"
#include "erl_mtrace.h"
#define GET_ERL_ALLOC_UTIL_IMPL
#include "erl_alloc_util.h"
#include "erl_mseg.h"
#include "erl_threads.h"
#include "erl_thr_progress.h"
#include "erl_bif_unique.h"
#include "erl_nif.h"

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
#define SYS_PAGE_SIZE                   (sys_page_size)
#define SYS_PAGE_SZ_MASK                ((UWord)(SYS_PAGE_SIZE - 1))

#if 0
/* Can be useful for debugging */
#define MBC_REALLOC_ALWAYS_MOVES
#endif

/* alloc_util global parameters */
static Uint sys_alloc_carrier_size;
static Uint sys_page_size;

#if HAVE_ERTS_MSEG
static Uint max_mseg_carriers;
#endif
static int allow_sys_alloc_carriers;

#define ONE_GIGA (1000000000)

#define ERTS_ALC_CC_GIGA_VAL(CC) ((CC) / ONE_GIGA)
#define ERTS_ALC_CC_VAL(CC) ((CC) % ONE_GIGA)

#define INC_CC(CC) ((CC)++)

#define DEC_CC(CC) ((CC)--)

/* Multi block carrier (MBC) memory layout in OTP 22:

Empty MBC:
[Carrier_t|pad|Block_t L0T0|fhdr| free... ]

MBC after allocating first block:
[Carrier_t|pad|Block_t 0000|        udata        |pad|Block_t L0T0|fhdr| free... ]

MBC after allocating second block:
[Carrier_t|pad|Block_t 0000|        udata        |pad|Block_t 0000|   udata   |pad|Block_t L0T0|fhdr| free... ]

MBC after deallocating first block:
[Carrier_t|pad|Block_t 00T0|fhdr| free  |FreeBlkFtr_t|Block_t 0P00|   udata   |pad|Block_t L0T0|fhdr| free... ]

MBC after allocating first block, with allocation tagging enabled:
[Carrier_t|pad|Block_t 000A|        udata        |atag|pad|Block_t L0T0|fhdr| free... ]

    udata = Allocated user data
    atag  = A tag with basic metadata about this allocation
    pad   = Padding to ensure correct alignment for user data
    fhdr  = Allocator specific header to keep track of free block
    free  = Unused free memory
    T     = This block is free (THIS_FREE_BLK_HDR_FLG)
    P     = Previous block is free (PREV_FREE_BLK_HDR_FLG)
    L     = Last block in carrier (LAST_BLK_HDR_FLG)
    A     = Block has an allocation tag footer, only valid for allocated blocks
            (ATAG_BLK_HDR_FLG)
*/

/* Single block carrier (SBC):
[Carrier_t|pad|Block_t 1110| udata... ]
[Carrier_t|pad|Block_t 111A| udata | atag]
*/

/* Allocation tags ...
 *
 * These are added to the footer of every block when enabled. Currently they
 * consist of the allocation type and an atom identifying the allocating
 * driver/nif (or 'system' if that can't be determined), but the format is not
 * supposed to be set in stone.
 *
 * The packing scheme requires that the atom values are small enough to fit
 * into a word with ERTS_ALC_N_BITS to spare. Users must check for overflow
 * before MAKE_ATAG(). */

typedef UWord alcu_atag_t;

#define MAKE_ATAG(IdAtom, TypeNum) \
    (ASSERT((TypeNum) >= ERTS_ALC_N_MIN && (TypeNum) <= ERTS_ALC_N_MAX), \
     ASSERT(atom_val(IdAtom) <= MAX_ATAG_ATOM_ID), \
     (atom_val(IdAtom) << ERTS_ALC_N_BITS) | (TypeNum))

#define ATAG_ID(AT) (make_atom((AT) >> ERTS_ALC_N_BITS))
#define ATAG_TYPE(AT) ((AT) & ERTS_ALC_N_MASK)

#define MAX_ATAG_ATOM_ID (ERTS_UWORD_MAX >> ERTS_ALC_N_BITS)

#define DBG_IS_VALID_ATAG(AT) \
    (ATAG_TYPE(AT) >= ERTS_ALC_N_MIN && \
     ATAG_TYPE(AT) <= ERTS_ALC_N_MAX && \
     ATAG_ID(AT) <= MAX_ATAG_ATOM_ID)

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

#define BLK_HAS_ATAG(B) \
    (!!((B)->bhdr & ATAG_BLK_HDR_FLG))

#define GET_BLK_ATAG(B) \
    (ASSERT(BLK_HAS_ATAG(B)), \
     ((alcu_atag_t *) (((char *) (B)) + (BLK_SZ(B))))[-1])
#define SET_BLK_ATAG(B, T) \
    ((B)->bhdr |= ATAG_BLK_HDR_FLG, \
     ((alcu_atag_t *) (((char *) (B)) + (BLK_SZ(B))))[-1] = (T))

#define BLK_ATAG_SZ(AP) ((AP)->atags ? sizeof(alcu_atag_t) : 0)

#define UMEMSZ2BLKSZ(AP, SZ)						\
  (ABLK_HDR_SZ + BLK_ATAG_SZ(AP) + (SZ) <= (AP)->min_block_size		\
   ? (AP)->min_block_size						\
   : UNIT_CEILING(ABLK_HDR_SZ + BLK_ATAG_SZ(AP) + (SZ)))

#define UMEM2BLK(P) ((Block_t *) (((char *) (P)) - ABLK_HDR_SZ))
#define BLK2UMEM(P) ((void *)    (((char *) (P)) + ABLK_HDR_SZ))

#define PREV_BLK_SZ(B) 		((UWord) (((FreeBlkFtr_t *)(B))[-1]))

#define SET_BLK_SZ_FTR(B, SZ) \
  (((FreeBlkFtr_t *) (((char *) (B)) + (SZ)))[-1] = (SZ))

#define SET_MBC_ABLK_SZ(B, SZ) \
  (ASSERT(((SZ) & BLK_FLG_MASK) == 0), \
   (B)->bhdr = (((B)->bhdr) & ~MBC_ABLK_SZ_MASK) | (SZ))
#define SET_MBC_FBLK_SZ(B, SZ) \
  (ASSERT(((SZ) & BLK_FLG_MASK) == 0), \
   (B)->bhdr = (((B)->bhdr) & ~MBC_FBLK_SZ_MASK) | (SZ))
#define SET_SBC_BLK_SZ(B, SZ) \
  (ASSERT(((SZ) & BLK_FLG_MASK) == 0), \
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
     ASSERT(!((UWord)(F) & (~BLK_FLG_MASK|THIS_FREE_BLK_HDR_FLG))), \
     (B)->bhdr = ((Sz) | (F) | (BLK_CARRIER_OFFSET(B,C) << MBC_ABLK_OFFSET_SHIFT)))

#  define SET_MBC_FBLK_HDR(B, Sz, F, C) \
    (ASSERT(((Sz) & ~MBC_FBLK_SZ_MASK) == 0), \
     ASSERT(((UWord)(F) & (~BLK_FLG_MASK|THIS_FREE_BLK_HDR_FLG|PREV_FREE_BLK_HDR_FLG)) == THIS_FREE_BLK_HDR_FLG), \
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
   (B)->bhdr &= (MBC_ABLK_SZ_MASK|LAST_BLK_HDR_FLG), \
   (B)->bhdr |= THIS_FREE_BLK_HDR_FLG)

#  define SET_BLK_ALLOCED(B) \
  (ASSERT(((B)->bhdr & (MBC_ABLK_OFFSET_MASK|THIS_FREE_BLK_HDR_FLG)) == THIS_FREE_BLK_HDR_FLG), \
   (B)->bhdr &= ~THIS_FREE_BLK_HDR_FLG, \
   (B)->bhdr |= (BLK_CARRIER_OFFSET(B,(B)->u.carrier) << MBC_ABLK_OFFSET_SHIFT))

#else /* !MBC_ABLK_OFFSET_BITS */

#  define MBC_SZ_MAX_LIMIT ((UWord)~0)

#  define SET_MBC_ABLK_HDR(B, Sz, F, C) \
    (ASSERT(((Sz) & BLK_FLG_MASK) == 0), \
     ASSERT(((F) & ~BLK_FLG_MASK) == 0), \
     ASSERT(!((UWord)(F) & (~BLK_FLG_MASK|THIS_FREE_BLK_HDR_FLG))), \
     (B)->bhdr = ((Sz) | (F)), \
     (B)->carrier = (C))

#  define SET_MBC_FBLK_HDR(B, Sz, F, C) \
    (ASSERT(((Sz) & BLK_FLG_MASK) == 0), \
     ASSERT(((F) & ~BLK_FLG_MASK) == 0), \
     ASSERT(((UWord)(F) & (~BLK_FLG_MASK|THIS_FREE_BLK_HDR_FLG|PREV_FREE_BLK_HDR_FLG)) == THIS_FREE_BLK_HDR_FLG), \
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
  (ASSERT(((Sz) & BLK_FLG_MASK) == 0), (B)->bhdr = ((Sz) | (SBC_BLK_HDR_FLG)))


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
  ((B)->bhdr & BLK_FLG_MASK)

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

#define ERTS_ALC_IS_CPOOL_ENABLED(A)	((A)->cpool.util_limit)


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


#define ERTS_CRR_ALCTR_FLG_IN_POOL	(((erts_aint_t) 1) << 0)
#define ERTS_CRR_ALCTR_FLG_BUSY		(((erts_aint_t) 1) << 1)
#define ERTS_CRR_ALCTR_FLG_HOMECOMING	(((erts_aint_t) 1) << 2)
#define ERTS_CRR_ALCTR_FLG_MASK (ERTS_CRR_ALCTR_FLG_IN_POOL | \
                                 ERTS_CRR_ALCTR_FLG_BUSY |    \
                                 ERTS_CRR_ALCTR_FLG_HOMECOMING)

#define SBC_HEADER_SIZE	   						\
    (UNIT_CEILING(offsetof(Carrier_t, cpool)                            \
	          + ABLK_HDR_SZ)	                                \
     - ABLK_HDR_SZ)
#define MBC_HEADER_SIZE(AP) ((AP)->mbc_header_size)


#define MSEG_CARRIER_HDR_FLAG		(((UWord) 1) << 0)
#define SBC_CARRIER_HDR_FLAG		(((UWord) 1) << 1)

#define SCH_SYS_ALLOC			0
#define SCH_MSEG			MSEG_CARRIER_HDR_FLAG
#define SCH_MBC				0
#define SCH_SBC				SBC_CARRIER_HDR_FLAG

#define SET_CARRIER_HDR(C, Sz, F, AP) \
  (ASSERT(((Sz) & CRR_FLG_MASK) == 0), (C)->chdr = ((Sz) | (F)), \
   erts_atomic_init_nob(&(C)->allctr, (erts_aint_t) (AP)))

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
  (ASSERT(((SZ) & CRR_FLG_MASK) == 0), \
   ((C)->chdr = ((C)->chdr & CRR_FLG_MASK) | (SZ)))

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
    set_new_allctr_abandon_limit(AP);                                   \
    (AP)->mbcs.blocks.curr.no += (CRR)->cpool.blocks[(AP)->alloc_no];   \
    if ((AP)->mbcs.blocks.max.no < (AP)->mbcs.blocks.curr.no)		\
	(AP)->mbcs.blocks.max.no = (AP)->mbcs.blocks.curr.no;		\
    (AP)->mbcs.blocks.curr.size +=                                      \
       (CRR)->cpool.blocks_size[(AP)->alloc_no];                        \
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

#define STAT_MBC_FREE(AP, CRR)                                               \
do {                                                                         \
    UWord csz__ = CARRIER_SZ((CRR));                                         \
    if (IS_MSEG_CARRIER((CRR))) {                                            \
        STAT_MSEG_MBC_FREE((AP), csz__);                                     \
    } else {                                                                 \
        STAT_SYS_ALLOC_MBC_FREE((AP), csz__);                                \
    }                                                                        \
    set_new_allctr_abandon_limit(AP);                                        \
} while (0)

#define STAT_MBC_ABANDON(AP, CRR)                                            \
do {                                                                         \
    STAT_MBC_FREE(AP, CRR);                                                  \
    ERTS_ALC_CPOOL_ASSERT((AP)->mbcs.blocks.curr.no                          \
                          >= (CRR)->cpool.blocks[(AP)->alloc_no]);           \
    (AP)->mbcs.blocks.curr.no -= (CRR)->cpool.blocks[(AP)->alloc_no];        \
    ERTS_ALC_CPOOL_ASSERT((AP)->mbcs.blocks.curr.size                        \
                          >= (CRR)->cpool.blocks_size[(AP)->alloc_no]);      \
    (AP)->mbcs.blocks.curr.size -= (CRR)->cpool.blocks_size[(AP)->alloc_no]; \
} while (0)

#define STAT_MBC_BLK_ALLOC_CRR(AP, CRR, BSZ)				\
do {									\
    (CRR)->cpool.blocks[(AP)->alloc_no]++;				\
    (CRR)->cpool.blocks_size[(AP)->alloc_no] += (BSZ);			\
    (CRR)->cpool.total_blocks_size += (BSZ);				\
} while (0)

#define STAT_MBC_BLK_ALLOC(AP, CRR, BSZ, FLGS)	       			\
do {									\
    CarriersStats_t *cstats__ = &(AP)->mbcs;			        \
    cstats__->blocks.curr.no++;						\
    if (cstats__->blocks.max.no < cstats__->blocks.curr.no)		\
	cstats__->blocks.max.no = cstats__->blocks.curr.no;		\
    cstats__->blocks.curr.size += (BSZ);				\
    if (cstats__->blocks.max.size < cstats__->blocks.curr.size)		\
	cstats__->blocks.max.size = cstats__->blocks.curr.size;		\
    STAT_MBC_BLK_ALLOC_CRR((AP), (CRR), (BSZ));				\
} while (0)

static ERTS_INLINE int
stat_cpool_mbc_blk_free(Allctr_t *allctr,
                        ErtsAlcType_t type,
			Carrier_t *crr,
			Carrier_t **busy_pcrr_pp,
			UWord blksz)
{
    Allctr_t *orig_allctr;
    int alloc_no;

    alloc_no = ERTS_ALC_T2A(type);

    ERTS_ALC_CPOOL_ASSERT(crr->cpool.blocks[alloc_no] > 0);
    crr->cpool.blocks[alloc_no]--;
    ERTS_ALC_CPOOL_ASSERT(crr->cpool.blocks_size[alloc_no] >= blksz);
    crr->cpool.blocks_size[alloc_no] -= blksz;
    ERTS_ALC_CPOOL_ASSERT(crr->cpool.total_blocks_size >= blksz);
    crr->cpool.total_blocks_size -= blksz;

    if (allctr->alloc_no == alloc_no && (!busy_pcrr_pp || !*busy_pcrr_pp)) {
        /* This is a local block, so we should not update the pool
         * statistics. */
        return 0;
    }

    /* This is either a foreign block that's been fetched from the pool, or any
     * block that's in the pool. The carrier's owner keeps the statistics for
     * both pooled and foreign blocks. */

    orig_allctr = crr->cpool.orig_allctr;

    ERTS_ALC_CPOOL_ASSERT(alloc_no != allctr->alloc_no ||
        (crr == *busy_pcrr_pp && allctr == orig_allctr));

#ifdef ERTS_ALC_CPOOL_DEBUG
    ERTS_ALC_CPOOL_ASSERT(
	erts_atomic_dec_read_nob(&orig_allctr->cpool.stat.no_blocks[alloc_no]) >= 0);
    ERTS_ALC_CPOOL_ASSERT(
	erts_atomic_add_read_nob(&orig_allctr->cpool.stat.blocks_size[alloc_no],
				 -((erts_aint_t) blksz)) >= 0);
#else
    erts_atomic_dec_nob(&orig_allctr->cpool.stat.no_blocks[alloc_no]);
    erts_atomic_add_nob(&orig_allctr->cpool.stat.blocks_size[alloc_no],
			-((erts_aint_t) blksz));
#endif

    return 1;
}

#define STAT_MBC_BLK_FREE(AP, TYPE, CRR, BPCRRPP, BSZ, FLGS)               \
do {                                                                       \
    if (!stat_cpool_mbc_blk_free((AP), (TYPE), (CRR), (BPCRRPP), (BSZ))) { \
        CarriersStats_t *cstats__ = &(AP)->mbcs;                           \
        ASSERT(cstats__->blocks.curr.no > 0);                              \
        cstats__->blocks.curr.no--;                                        \
        ASSERT(cstats__->blocks.curr.size >= (BSZ));                       \
        cstats__->blocks.curr.size -= (BSZ);                               \
    }                                                                      \
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
#  define IS_ACTUALLY_BLOCKING (erts_thr_progress_is_blocking())
#define ERTS_ALCU_DBG_CHK_THR_ACCESS(A)					\
do {									\
    if (!(A)->thread_safe && !IS_ACTUALLY_BLOCKING) {                   \
	if (!(A)->debug.saved_tid) {                                    \
	    (A)->debug.tid = erts_thr_self();				\
	    (A)->debug.saved_tid = 1;					\
	}								\
	else {								\
	    ERTS_LC_ASSERT(						\
		ethr_equal_tids((A)->debug.tid, erts_thr_self()));	\
	}								\
    }									\
} while (0)
#else
#define ERTS_ALCU_DBG_CHK_THR_ACCESS(A)
#endif

static void make_name_atoms(Allctr_t *allctr);

static Block_t *create_carrier(Allctr_t *, Uint, UWord);
static void destroy_carrier(Allctr_t *, Block_t *, Carrier_t **);
static void mbc_free(Allctr_t *allctr, ErtsAlcType_t type, void *p, Carrier_t **busy_pcrr_pp);
static void dealloc_block(Allctr_t *, ErtsAlcType_t, Uint32, void *, ErtsAlcFixList_t *);

static alcu_atag_t determine_alloc_tag(Allctr_t *allocator, ErtsAlcType_t type)
{
    ErtsSchedulerData *esdp;
    Eterm id;

    ERTS_CT_ASSERT(_unchecked_atom_val(am_system) <= MAX_ATAG_ATOM_ID);
    ASSERT(allocator->atags);

    esdp = erts_get_scheduler_data();
    id = am_system;

    if (esdp) {
        if (esdp->current_nif) {
            Module *mod = erts_nif_get_module((esdp->current_nif)->mod_nif);

            /* Mod can be NULL if a resource destructor allocates memory after
             * the module has been unloaded. */
            if (mod) {
                id = make_atom(mod->module);
            }
        } else if (esdp->current_port) {
            Port *p = esdp->current_port;
            id = (p->drv_ptr)->name_atom;
        }

        /* We fall back to 'system' if we can't pack the driver/NIF name into
         * the tag. This may be a bit misleading but we've made no promises
         * that the information is complete.
         *
         * This can only happen on 32-bit emulators when a new driver/NIF has
         * been loaded *after* 16 million atoms have been used, and supporting
         * that fringe case is not worth an extra word. 64-bit emulators are
         * unaffected since the atom cache limits atom indexes to 32 bits. */
        if(MAX_ATOM_TABLE_SIZE > MAX_ATAG_ATOM_ID) {
            if (atom_val(id) > MAX_ATAG_ATOM_ID) {
                id = am_system;
            }
        }
    }

    return MAKE_ATAG(id, ERTS_ALC_T2N(type));
}

static void set_alloc_tag(Allctr_t *allocator, void *p, alcu_atag_t tag)
{
    Block_t *block;

    ASSERT(DBG_IS_VALID_ATAG(tag));
    ASSERT(allocator->atags && p);
    (void)allocator;

    block = UMEM2BLK(p);

    SET_BLK_ATAG(block, tag);
}

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

#ifdef DEBUG

static ERTS_INLINE int is_bit_set(UWord* map, Uint ix)
{
    ASSERT(ix / ERTS_VSPACE_WORD_BITS < VSPACE_MAP_SZ);
    return map[ix / ERTS_VSPACE_WORD_BITS]
        & ((UWord)1 << (ix % ERTS_VSPACE_WORD_BITS));
}

#endif

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

static void*
erts_alcu_mseg_alloc(Allctr_t *allctr, Uint *size_p, Uint flags)
{
    void *res;
    UWord size = (UWord) *size_p;
    res = erts_mseg_alloc_opt(allctr->alloc_no, &size, flags, &allctr->mseg_opt);
    *size_p = (Uint) size;
    INC_CC(allctr->calls.mseg_alloc);
    return res;
}

static void*
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

static void
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
    ERTS_LC_ASSERT(allctr->thread_safe);

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
    ERTS_LC_ASSERT(allctr->thread_safe);

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
    ERTS_LC_ASSERT(allctr->thread_safe);

    erts_alcu_mseg_dealloc(allctr, seg, size, flags);

    clear_literal_range(seg, size);
}

#elif defined(ARCH_64) && defined(ERTS_HAVE_OS_PHYSICAL_MEMORY_RESERVATION)

/* For allocators that have their own mmapper (super carrier),
 * like literal_alloc.
 */
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

#if defined(ERTS_ALC_A_EXEC)

/*
 * For exec_alloc that need memory with PROT_EXEC
 */
void*
erts_alcu_exec_mseg_alloc(Allctr_t *allctr, Uint *size_p, Uint flags)
{
    void* res = erts_alcu_mseg_alloc(allctr, size_p, flags);

    if (res) {
        int r = mprotect(res, *size_p, PROT_EXEC | PROT_READ | PROT_WRITE);
        ASSERT(r == 0); (void)r;
    }
    return res;
}

void*
erts_alcu_exec_mseg_realloc(Allctr_t *allctr, void *seg,
                            Uint old_size, Uint *new_size_p)
{
    void *res;

    if (seg && old_size) {
        int r = mprotect(seg, old_size, PROT_READ | PROT_WRITE);
        ASSERT(r == 0); (void)r;
    }
    res = erts_alcu_mseg_realloc(allctr, seg, old_size, new_size_p);
    if (res) {
        int r = mprotect(res, *new_size_p, PROT_EXEC | PROT_READ | PROT_WRITE);
        ASSERT(r == 0); (void)r;
    }
    return res;
}

void
erts_alcu_exec_mseg_dealloc(Allctr_t *allctr, void *seg, Uint size, Uint flags)
{
    int r = mprotect(seg, size, PROT_READ | PROT_WRITE);
    ASSERT(r == 0); (void)r;
    erts_alcu_mseg_dealloc(allctr, seg, size, flags);
}
#endif /* ERTS_ALC_A_EXEC */

#endif /* HAVE_ERTS_MSEG */

static void*
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

static void*
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

static void
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
    ERTS_LC_ASSERT(allctr->thread_safe);

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
    ERTS_LC_ASSERT(allctr->thread_safe);

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
    ERTS_LC_ASSERT(allctr->thread_safe);

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
#ifdef DEBUG
    crr->next = crr;
    crr->prev = crr;
#endif
}

static ERTS_INLINE int is_abandoned(Carrier_t *crr)
{
    return crr->cpool.state != ERTS_MBC_IS_HOME;
}

static ERTS_INLINE void
unlink_abandoned_carrier(Carrier_t *crr)
{
    if (crr->cpool.state == ERTS_MBC_WAS_POOLED) {
        aoff_remove_pooled_mbc(crr->cpool.orig_allctr, crr);
    }
}

static ERTS_INLINE void
clear_busy_pool_carrier(Allctr_t *allctr, Carrier_t *crr)
{
    if (crr) {
	erts_aint_t max_size;
	erts_aint_t iallctr;

	max_size = (erts_aint_t) allctr->largest_fblk_in_mbc(allctr, crr);
	erts_atomic_set_nob(&crr->cpool.max_size, max_size);

        iallctr = erts_atomic_read_nob(&crr->allctr);
        ERTS_ALC_CPOOL_ASSERT((iallctr & ~ERTS_CRR_ALCTR_FLG_HOMECOMING)
                              == ((erts_aint_t)allctr |
                                  ERTS_CRR_ALCTR_FLG_IN_POOL |
                                  ERTS_CRR_ALCTR_FLG_BUSY));

	iallctr &= ~ERTS_CRR_ALCTR_FLG_BUSY;
	erts_atomic_set_relb(&crr->allctr, iallctr);
    }
}


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

static ERTS_INLINE Allctr_t *get_pref_allctr(void *extra);
static void *mbc_alloc(Allctr_t *allctr, Uint size);

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
	    if (busy_pcrr_pp) {
		clear_busy_pool_carrier(allctr, *busy_pcrr_pp);
		*busy_pcrr_pp = NULL;
	    }
	    fix->u.cpool.shrink_list--;
	    p = fix->list;
	    fix->list = *((void **) p);
	    fix->list_size--;
	    if (fix->u.cpool.min_list_size > fix->list_size)
		fix->u.cpool.min_list_size = fix->list_size;

	    dealloc_block(allctr, type, DEALLOC_FLG_FIX_SHRINK, p, fix);
	}
    }
}

static ERTS_INLINE void *
fix_cpool_alloc(Allctr_t *allctr, ErtsAlcType_t type, Uint size)
{
    void *res;
    ErtsAlcFixList_t *fix;

    fix = &allctr->fix[ERTS_ALC_FIX_TYPE_IX(type)];
    ASSERT(type == fix->type && size == fix->type_size);
    ASSERT(size >= sizeof(ErtsAllctrDDBlock_t));

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
               Uint32 flags,
	       void *p,
	       Carrier_t **busy_pcrr_pp)
{
    ErtsAlcFixList_t *fix;
    Allctr_t *fix_allctr;

    /* If this isn't a fix allocator we need to update the fix list of our
     * neighboring fix_alloc to keep the statistics consistent. */
    if (!allctr->fix) {
        ErtsAllocatorThrSpec_t *tspec = &erts_allctr_thr_spec[ERTS_ALC_A_FIXED_SIZE];
        fix_allctr = get_pref_allctr(tspec);
        ASSERT(!fix_allctr->thread_safe);
        ASSERT(allctr != fix_allctr);
    }
    else {
        fix_allctr = allctr;
    }

    ASSERT(ERTS_ALC_IS_CPOOL_ENABLED(fix_allctr));
    ASSERT(ERTS_ALC_IS_CPOOL_ENABLED(allctr));

    fix = &fix_allctr->fix[ERTS_ALC_FIX_TYPE_IX(type)];
    ASSERT(type == fix->type);

    if (!(flags & DEALLOC_FLG_FIX_SHRINK)) {
        fix->u.cpool.used--;
    }

    /* We don't want foreign blocks to be long-lived, so we skip recycling if
     * allctr != fix_allctr. */
    if (allctr == fix_allctr
        && (!busy_pcrr_pp || !*busy_pcrr_pp)
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
	    mbc_free(allctr, type, p, busy_pcrr_pp);
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

    if (allctr->thread_safe)
	erts_mtx_lock(&allctr->mutex);

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
	type = ERTS_ALC_N2T((ErtsAlcType_t) (ix + ERTS_ALC_N_MIN_A_FIXED_SIZE));
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
	    dealloc_block(allctr, type, DEALLOC_FLG_FIX_SHRINK, ptr, fix);
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

    if (allctr->thread_safe)
	erts_mtx_unlock(&allctr->mutex);

    return res;
}

static ERTS_INLINE void *
fix_nocpool_alloc(Allctr_t *allctr, ErtsAlcType_t type, Uint size)
{
    ErtsAlcFixList_t *fix;
    void *res;

    fix = &allctr->fix[ERTS_ALC_FIX_TYPE_IX(type)];
    ASSERT(type == fix->type && size == fix->type_size);
    ASSERT(size >= sizeof(ErtsAllctrDDBlock_t));

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
		mbc_free(allctr, type, p, NULL);
	    fix->u.nocpool.allocated--;
	}
	ERTS_DBG_CHK_FIX_LIST(allctr, fix, ix, 0);
	return res;
    }
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

    fix = &allctr->fix[ERTS_ALC_T2N(type) - ERTS_ALC_N_MIN_A_FIXED_SIZE];
    ASSERT(fix->type == type);

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
	    mbc_free(allctr, type, p, NULL);
	p = fix->list;
	fix->list = *((void **) p);
	fix->list_size--;
	fix->u.nocpool.allocated--;
    }

    blk = UMEM2BLK(p);
    if (IS_SBC_BLK(blk))
	destroy_carrier(allctr, blk, NULL);
    else
	mbc_free(allctr, type, p, NULL);
    ERTS_DBG_CHK_FIX_LIST(allctr, fix, ix, 0);
}

static ERTS_INLINE erts_aint32_t
fix_nocpool_alloc_shrink(Allctr_t *allctr, erts_aint32_t flgs)
{
    int all_empty = 1;
    erts_aint32_t res = 0;
    int ix, o;
    int flush = flgs == 0;

    if (allctr->thread_safe)
	erts_mtx_lock(&allctr->mutex);

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
	    dealloc_block(allctr, fix->type, 0, ptr, NULL);
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

    if (allctr->thread_safe)
	erts_mtx_unlock(&allctr->mutex);

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


static UWord allctr_abandon_limit(Allctr_t *allctr);
static void set_new_allctr_abandon_limit(Allctr_t*);
static void abandon_carrier(Allctr_t*, Carrier_t*);
static void poolify_my_carrier(Allctr_t*, Carrier_t*);
static void enqueue_homecoming(Allctr_t*, Carrier_t*);

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
	iallctr = erts_atomic_read_dirty(&crr->allctr);
    }
    else {
	crr = ABLK_TO_MBC(blk);

	if (sizep)
	    *sizep = MBC_ABLK_SZ(blk) - ABLK_HDR_SZ;
	if (!ERTS_ALC_IS_CPOOL_ENABLED(pref_allctr))
	    iallctr = erts_atomic_read_dirty(&crr->allctr);
	else {
	    int locked_pref_allctr = 0;
	    iallctr = erts_atomic_read_ddrb(&crr->allctr);

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
                if (iallctr & ERTS_CRR_ALCTR_FLG_HOMECOMING) {
                    /*
                     * This carrier has just been given back to us by writing
                     * to crr->allctr with a write barrier (see abandon_carrier).
                     *
                     * We need a mathing read barrier to guarantee a correct view
                     * of the carrier for deallocation work.
                     */
                    act = erts_atomic_cmpxchg_rb(&crr->allctr,
                                                 iallctr|ERTS_CRR_ALCTR_FLG_BUSY,
                                                 iallctr);
                }
                else {
                    act = erts_atomic_cmpxchg_ddrb(&crr->allctr,
                                                   iallctr|ERTS_CRR_ALCTR_FLG_BUSY,
                                                   iallctr);
                }
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
    erts_atomic_init_nob(&ddq->tail.data.marker.u.atmc_next, ERTS_AINT_NULL);
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

    erts_atomic_init_nob(&this->u.atmc_next, ERTS_AINT_NULL);
    /* Enqueue at end of list... */

    enq = (ErtsAllctrDDBlock_t *) erts_atomic_read_nob(&ddq->tail.data.last);
    itmp = erts_atomic_cmpxchg_relb(&enq->u.atmc_next,
				    (erts_aint_t) this,
				    ERTS_AINT_NULL);
    if (itmp == ERTS_AINT_NULL) {
	/* We are required to move last pointer */
#ifdef DEBUG
	ASSERT(ERTS_AINT_NULL == erts_atomic_read_nob(&this->u.atmc_next));
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
	    erts_atomic_set_nob(&this->u.atmc_next, itmp);
	    itmp2 = erts_atomic_cmpxchg_relb(&enq->u.atmc_next,
					     (erts_aint_t) this,
					     itmp);
	    if (itmp == itmp2)
		return 0; /* inserted this */
	    if ((i & 1) == 0)
		itmp = itmp2;
	    else {
		enq = (ErtsAllctrDDBlock_t *) itmp2;
		itmp = erts_atomic_read_acqb(&enq->u.atmc_next);
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

	erts_atomic_init_nob(&ddq->tail.data.marker.u.atmc_next, ERTS_AINT_NULL);
	itmp = erts_atomic_cmpxchg_relb(&last->u.atmc_next,
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
	       erts_atomic_read_nob(&blk->u.atmc_next));
	if (blk == ddq->head.unref_end) {
	    ddq->head.first = blk;
	    return NULL;
	}
    }

    ddq->head.first = ((ErtsAllctrDDBlock_t *)
		       erts_atomic_read_nob(&blk->u.atmc_next));

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
handle_delayed_fix_dealloc(Allctr_t *allctr, ErtsAlcType_t type, Uint32 flags,
                           void *ptr)
{
    ASSERT(ERTS_ALC_IS_FIX_TYPE(type));

    if (!ERTS_ALC_IS_CPOOL_ENABLED(allctr))
	fix_nocpool_free(allctr, type, ptr);
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
	    fix_cpool_free(allctr, type, flags, ptr, &busy_pcrr_p);
	    clear_busy_pool_carrier(allctr, busy_pcrr_p);
	}
	else {
	    /* Carrier migrated; need to redirect block to new owner... */
            ErtsAllctrDDBlock_t *dd_block;
            int cinit;

            dd_block = (ErtsAllctrDDBlock_t*)ptr;
            dd_block->flags = flags;
            dd_block->type = type;

            ERTS_ALC_CPOOL_ASSERT(!busy_pcrr_p);

            DEC_CC(allctr->calls.this_free);

            cinit = used_allctr->dd.ix - allctr->dd.ix;

	    if (ddq_enqueue(&used_allctr->dd.q, ptr, cinit))
		erts_alloc_notify_delayed_dealloc(used_allctr->ix);
	}
    }
}

static void schedule_dealloc_carrier(Allctr_t*, Carrier_t*);
static void dealloc_my_carrier(Allctr_t*, Carrier_t*);


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
    int res;
    ErtsAllctrDDQueue_t *ddq;

    if (allctr->thread_safe && !allctr_locked)
	erts_mtx_lock(&allctr->mutex);

    ERTS_ALCU_DBG_CHK_THR_ACCESS(allctr);

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
	if (blk->bhdr == HOMECOMING_MBC_BLK_HDR) {
	    /*
	     * A multiblock carrier that previously has been migrated away
             * from us, was sent back to us either because
             * - it became empty and we need to deallocated it, or
             * - it was inserted into the pool and we need to update our pooled_tree
	     */
	    Carrier_t *crr = ErtsContainerStruct(blk, Carrier_t,
                                                 cpool.homecoming_dd.blk);
            Block_t* first_blk = MBC_TO_FIRST_BLK(allctr, crr);
            erts_aint_t iallctr;

	    ERTS_ALC_CPOOL_ASSERT(ERTS_ALC_IS_CPOOL_ENABLED(allctr));
	    ERTS_ALC_CPOOL_ASSERT(allctr == crr->cpool.orig_allctr);

            iallctr = erts_atomic_read_nob(&crr->allctr);
            ASSERT(iallctr & ERTS_CRR_ALCTR_FLG_HOMECOMING);
            while (1) {
                if ((iallctr & (~ERTS_CRR_ALCTR_FLG_MASK |
                                ERTS_CRR_ALCTR_FLG_IN_POOL))
                    == (erts_aint_t)allctr) {
                    /*
                     * Carrier is home (mine and not in pool)
                     */
                    ASSERT(!(iallctr & ERTS_CRR_ALCTR_FLG_BUSY));
                    erts_atomic_set_nob(&crr->allctr, (erts_aint_t)allctr);
                    if (IS_FREE_LAST_MBC_BLK(first_blk))
                        dealloc_my_carrier(allctr, crr);
                    else
                        ASSERT(crr->cpool.state == ERTS_MBC_IS_HOME);
                }
                else {
                    erts_aint_t exp = iallctr;
                    erts_aint_t want = iallctr & ~ERTS_CRR_ALCTR_FLG_HOMECOMING;

                    iallctr = erts_atomic_cmpxchg_nob(&crr->allctr,
                                                          want,
                                                          exp);
                    if (iallctr != exp)
                        continue; /* retry */

                    ASSERT(crr->cpool.state != ERTS_MBC_IS_HOME);
                    unlink_abandoned_carrier(crr);
                    if (iallctr & ERTS_CRR_ALCTR_FLG_IN_POOL)
                        poolify_my_carrier(allctr, crr);
                    else
                        crr->cpool.state = ERTS_MBC_WAS_TRAITOR;
                }
                break;
            }
	}
	else {
            ErtsAllctrDDBlock_t *dd_block;
            ErtsAlcType_t type;
            Uint32 flags;
        
            dd_block = (ErtsAllctrDDBlock_t*)ptr;
            flags = dd_block->flags;
            type = dd_block->type;

            flags |= DEALLOC_FLG_REDIRECTED;

            ASSERT(IS_SBC_BLK(blk) || (ABLK_TO_MBC(blk) !=
                                       ErtsContainerStruct(blk, Carrier_t,
                                                           cpool.homecoming_dd.blk)));

	    INC_CC(allctr->calls.this_free);

	    if (ERTS_ALC_IS_FIX_TYPE(type)) {
		handle_delayed_fix_dealloc(allctr, type, flags, ptr);
	    } else {
		dealloc_block(allctr, type, flags, ptr, NULL);
            }
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
    ErtsAllctrDDBlock_t *dd_block = ((ErtsAllctrDDBlock_t*)ptr);

    dd_block->type = type;
    dd_block->flags = 0;

    if (ddq_enqueue(&allctr->dd.q, ptr, cinit))
	erts_alloc_notify_delayed_dealloc(allctr->ix);
}

static ERTS_INLINE void
update_pooled_tree(Allctr_t *allctr, Carrier_t *crr, Uint blk_sz)
{
    if (allctr == crr->cpool.orig_allctr && crr->cpool.state == ERTS_MBC_WAS_POOLED) {
	/*
	 * Update pooled_tree with a potentially new (larger) max_sz
         */
        AOFF_RBTree_t* crr_node = &crr->cpool.pooled;
        if (blk_sz > crr_node->hdr.bhdr) {
            crr_node->hdr.bhdr = blk_sz;
            erts_aoff_larger_max_size(crr_node);
        }
    }
}

static ERTS_INLINE void
check_abandon_carrier(Allctr_t *allctr, Block_t *fblk, Carrier_t **busy_pcrr_pp)
{
    Carrier_t *crr;
    UWord ncrr_in_pool, largest_fblk;

    if (!ERTS_ALC_IS_CPOOL_ENABLED(allctr))
	return;

    ASSERT(allctr->cpool.abandon_limit == allctr_abandon_limit(allctr));
    ASSERT(erts_thr_progress_is_managed_thread());

    if (allctr->cpool.disable_abandon)
	return;

    if (allctr->mbcs.blocks.curr.size > allctr->cpool.abandon_limit)
	return;

    ncrr_in_pool = erts_atomic_read_nob(&allctr->cpool.stat.no_carriers);
    if (ncrr_in_pool >= allctr->cpool.in_pool_limit)
        return;

    crr = FBLK_TO_MBC(fblk);

    if (allctr->main_carrier == crr)
	return;

    if (crr->cpool.total_blocks_size > crr->cpool.abandon_limit)
	return;

    if (crr->cpool.thr_prgr != ERTS_THR_PRGR_INVALID
        && !erts_thr_progress_has_reached(crr->cpool.thr_prgr))
        return;

    largest_fblk = allctr->largest_fblk_in_mbc(allctr, crr);
    if (largest_fblk < allctr->cpool.fblk_min_limit)
        return;

    erts_atomic_set_nob(&crr->cpool.max_size, largest_fblk);
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

#define ERTS_ALCU_HANDLE_DD_IN_OP(Allctr, Locked)			\
    handle_delayed_dealloc((Allctr), (Locked), 1, 			\
			   ERTS_ALCU_DD_OPS_LIM_LOW, NULL, NULL, NULL)

static void
dealloc_block(Allctr_t *allctr, ErtsAlcType_t type, Uint32 flags, void *ptr,
              ErtsAlcFixList_t *fix)
{
    Block_t *blk = UMEM2BLK(ptr);

    ASSERT(!fix || type == fix->type);

    ERTS_LC_ASSERT(!allctr->thread_safe
		       || erts_lc_mtx_is_locked(&allctr->mutex));

    if (IS_SBC_BLK(blk)) {
	destroy_carrier(allctr, blk, NULL);
	if (fix && ERTS_ALC_IS_CPOOL_ENABLED(allctr)) {
	    if (!(flags & DEALLOC_FLG_FIX_SHRINK))
		fix->u.cpool.used--;
	    fix->u.cpool.allocated--;
	}
    }
    else if (!ERTS_ALC_IS_CPOOL_ENABLED(allctr))
	mbc_free(allctr, type, ptr, NULL);
    else {
	Carrier_t *busy_pcrr_p;
	Allctr_t *used_allctr;

	used_allctr = get_used_allctr(allctr, ERTS_ALC_TS_PREF_LOCK_NO, ptr,
				      NULL, &busy_pcrr_p);
	if (used_allctr == allctr) {
	    if (fix) {
	        if (!(flags & DEALLOC_FLG_FIX_SHRINK))
		    fix->u.cpool.used--;
		fix->u.cpool.allocated--;
	    }
	    mbc_free(allctr, type, ptr, &busy_pcrr_p);
	    clear_busy_pool_carrier(allctr, busy_pcrr_p);
	}
	else {
	    /* Carrier migrated; need to redirect block to new owner... */
            ErtsAllctrDDBlock_t *dd_block;
            int cinit;

            dd_block = (ErtsAllctrDDBlock_t*)ptr;
            dd_block->flags = flags;
            dd_block->type = type;

            ERTS_ALC_CPOOL_ASSERT(!busy_pcrr_p);

            if (flags & DEALLOC_FLG_REDIRECTED)
                DEC_CC(allctr->calls.this_free);

            cinit = used_allctr->dd.ix - allctr->dd.ix;

	    if (ddq_enqueue(&used_allctr->dd.q, ptr, cinit))
		erts_alloc_notify_delayed_dealloc(used_allctr->ix);
	}
    }
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

typedef struct {
    char *ptr;
    UWord size;
} ErtsMemDiscardRegion;

/* Construct a discard region for the user memory of a free block, letting the
 * OS reclaim its physical memory when required.
 *
 * Note that we're ignoring both the footer and everything that comes before
 * the minimum block size as the allocator uses those areas to manage the
 * block. */
static void ERTS_INLINE
mem_discard_start(Allctr_t *allocator, Block_t *block,
                  ErtsMemDiscardRegion *out)
{
    UWord size = BLK_SZ(block);

    ASSERT(size >= allocator->min_block_size);

    if (size > (allocator->min_block_size + FBLK_FTR_SZ)) {
        out->size = size - allocator->min_block_size - FBLK_FTR_SZ;
    } else {
        out->size = 0;
    }

    out->ptr = (char*)block + allocator->min_block_size;
}

/* Expands a discard region into a neighboring free block, allowing us to
 * discard the block header and first page.
 *
 * This is very important in small-allocation scenarios where no single block
 * is large enough to be discarded on its own. */
static void ERTS_INLINE
mem_discard_coalesce(Allctr_t *allocator, Block_t *neighbor,
                     ErtsMemDiscardRegion *region)
{
    char *neighbor_start;

    ASSERT(IS_FREE_BLK(neighbor));

    neighbor_start = (char*)neighbor;

    if (region->ptr >= neighbor_start) {
        char *region_start_page;

        region_start_page = region->ptr - SYS_PAGE_SIZE;
        region_start_page = (char*)((UWord)region_start_page & ~SYS_PAGE_SZ_MASK);

        /* Expand if our first page begins within the previous free block's
         * unused data. */
        if (region_start_page >= (neighbor_start + allocator->min_block_size)) {
            region->size += (region->ptr - region_start_page) - FBLK_FTR_SZ;
            region->ptr = region_start_page;
        }
    } else {
        char *region_end_page;
        UWord neighbor_size;

        ASSERT(region->ptr <= neighbor_start);

        region_end_page = region->ptr + region->size + SYS_PAGE_SIZE;
        region_end_page = (char*)((UWord)region_end_page & ~SYS_PAGE_SZ_MASK);

        neighbor_size = BLK_SZ(neighbor) - FBLK_FTR_SZ;

        /* Expand if our last page ends anywhere within the next free block,
         * sans the footer we'll inherit. */
        if (region_end_page < neighbor_start + neighbor_size) {
            region->size += region_end_page - (region->ptr + region->size);
        }
    }
}

static void ERTS_INLINE
mem_discard_finish(Allctr_t *allocator, Block_t *block,
                   ErtsMemDiscardRegion *region)
{
#ifdef DEBUG
    char *block_start, *block_end;
    UWord block_size;

    block_size = BLK_SZ(block);

    /* Ensure that the region is completely covered by the legal area of the
     * free block. This must hold even when the region is too small to be
     * discarded. */
    if (region->size > 0) {
        ASSERT(block_size > allocator->min_block_size + FBLK_FTR_SZ);

        block_start = (char*)block + allocator->min_block_size;
        block_end = (char*)block + block_size - FBLK_FTR_SZ;

        ASSERT(region->size == 0 ||
            (region->ptr + region->size <= block_end &&
             region->ptr >= block_start &&
             region->size <= block_size));
    }
#else
    (void)allocator;
    (void)block;
#endif

    if (region->size > SYS_PAGE_SIZE) {
        UWord align_offset, size;
        char *ptr;

        align_offset = SYS_PAGE_SIZE - ((UWord)region->ptr & SYS_PAGE_SZ_MASK);

        size = (region->size - align_offset) & ~SYS_PAGE_SZ_MASK;
        ptr = region->ptr + align_offset;

        if (size > 0) {
            ASSERT(!((UWord)ptr & SYS_PAGE_SZ_MASK));
            ASSERT(!(size & SYS_PAGE_SZ_MASK));

            erts_mem_discard(ptr, size);
        }
    }
}

static void
carrier_mem_discard_free_blocks(Allctr_t *allocator, Carrier_t *carrier)
{
    static const int MAX_BLOCKS_TO_DISCARD = 100;
    Block_t *block;
    int i;

    block = allocator->first_fblk_in_mbc(allocator, carrier);
    i = 0;

    while (block != NULL && i < MAX_BLOCKS_TO_DISCARD) {
        ErtsMemDiscardRegion region;

        ASSERT(IS_FREE_BLK(block));

        mem_discard_start(allocator, block, &region);
        mem_discard_finish(allocator, block, &region);

        block = allocator->next_fblk_in_mbc(allocator, carrier, block);
        i++;
    }
}

static void
mbc_free(Allctr_t *allctr, ErtsAlcType_t type, void *p, Carrier_t **busy_pcrr_pp)
{
    ErtsMemDiscardRegion discard_region = {0};
    int discard;
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

#ifndef DEBUG
    /* We want to mark freed blocks as reclaimable to the OS, but it's a fairly
     * expensive operation which doesn't do much good if we use it again soon
     * after, so we limit it to deallocations on pooled carriers. */
    discard = busy_pcrr_pp && *busy_pcrr_pp;
#else
    /* Always discard in debug mode, regardless of whether we're in the pool or
     * not. */
    discard = 1;
#endif

    if (discard) {
        mem_discard_start(allctr, blk, &discard_region);
    }

    HARD_CHECK_BLK_CARRIER(allctr, blk);

    crr = ABLK_TO_MBC(blk);

    ERTS_ALC_CPOOL_FREE_OP(allctr);

    STAT_MBC_BLK_FREE(allctr, type, crr, busy_pcrr_pp, blk_sz, alcu_flgs);

    is_first_blk = IS_MBC_FIRST_ABLK(allctr, blk);
    is_last_blk = IS_LAST_BLK(blk);

    if (IS_PREV_BLK_FREE(blk)) {
	ASSERT(!is_first_blk); 
	/* Coalesce with previous block... */
	blk = PREV_BLK(blk);
	(*allctr->unlink_free_block)(allctr, blk);

        if (discard) {
            mem_discard_coalesce(allctr, blk, &discard_region);
        }

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

            if (discard) {
                mem_discard_coalesce(allctr, nxt_blk, &discard_region);
            }

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

    if (is_first_blk && is_last_blk && crr != allctr->main_carrier) {
        destroy_carrier(allctr, blk, busy_pcrr_pp);
    }
    else {
	(*allctr->link_free_block)(allctr, blk);
	HARD_CHECK_BLK_CARRIER(allctr, blk);

        if (discard) {
            mem_discard_finish(allctr, blk, &discard_region);
        }

        if (busy_pcrr_pp && *busy_pcrr_pp) {
            update_pooled_tree(allctr, crr, blk_sz);
        } else {
            check_abandon_carrier(allctr, blk, busy_pcrr_pp);
        }
    }
}

static void *
mbc_realloc(Allctr_t *allctr, ErtsAlcType_t type, void *p, Uint size,
            Uint32 alcu_flgs, Carrier_t **busy_pcrr_pp)
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

    if (busy_pcrr_pp && *busy_pcrr_pp) {
        /*
         * Don't want to use carrier in pool
         */
        new_p = mbc_alloc(allctr, size);
        if (!new_p)
            return NULL;
        new_blk = UMEM2BLK(new_p);
        ASSERT(!(IS_MBC_BLK(new_blk) && ABLK_TO_MBC(new_blk) == *busy_pcrr_pp));
        sys_memcpy(new_p, p, MIN(size, old_blk_sz - ABLK_HDR_SZ));
        mbc_free(allctr, type, p, busy_pcrr_pp);
        return new_p;
    }

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
	STAT_MBC_BLK_FREE(allctr, type, crr, NULL, old_blk_sz, alcu_flgs);
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

	check_abandon_carrier(allctr, nxt_blk, NULL);

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
	    STAT_MBC_BLK_FREE(allctr, type, crr, NULL, old_blk_sz, alcu_flgs);
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

#endif /* !MBC_REALLOC_ALWAYS_MOVES */

	new_p = mbc_alloc(allctr, size);
	if (!new_p)
	    return NULL;
	sys_memcpy(new_p, p, MIN(size, old_blk_sz - ABLK_HDR_SZ));
	mbc_free(allctr, type, p, busy_pcrr_pp);

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
	    mbc_free(allctr, type, p, NULL);
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
	    STAT_MBC_BLK_FREE(allctr, type, crr, NULL, old_blk_sz, alcu_flgs);

	    return new_p;
	}
    }
#endif /* !MBC_REALLOC_ALWAYS_MOVES */
}


#define ERTS_ALC_MAX_DEALLOC_CARRIER		10
#define ERTS_ALC_CPOOL_MAX_FETCH_INSPECT	100
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

/* The pools are only allowed to be manipulated by managed threads except in
 * the alloc_SUITE:cpool test, where only test_carrier_pool is used. */

static ErtsAlcCrrPool_t firstfit_carrier_pool;
static ErtsAlcCrrPool_t test_carrier_pool;

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
    ErtsAlcCPoolData_t *sentinel = allctr->cpool.sentinel;
    ErtsAlcCPoolData_t *cpdp = sentinel;
    Carrier_t *tmp_crr;

    while (1) {
	cpdp = (ErtsAlcCPoolData_t *) (erts_atomic_read_ddrb(&cpdp->next) & ~CRR_FLG_MASK);
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
    ErtsAlcCPoolData_t *sentinel = allctr->cpool.sentinel;
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
    ErtsAlcCPoolData_t *sentinel = allctr->cpool.sentinel;
    Allctr_t *orig_allctr = crr->cpool.orig_allctr;

    ERTS_ALC_CPOOL_ASSERT(allctr->alloc_no == ERTS_ALC_A_TEST /* testcase */
			  || erts_thr_progress_is_managed_thread());

    {
        int alloc_no = allctr->alloc_no;

        ERTS_ALC_CPOOL_ASSERT(
            erts_atomic_read_nob(&orig_allctr->cpool.stat.blocks_size[alloc_no]) >= 0 &&
            crr->cpool.blocks_size[alloc_no] >= 0);

        ERTS_ALC_CPOOL_ASSERT(
            erts_atomic_read_nob(&orig_allctr->cpool.stat.no_blocks[alloc_no]) >= 0 &&
            crr->cpool.blocks[alloc_no] >= 0);

        /* We only modify the counter for our current type since the others are
         * conceptually still in the pool. */
        erts_atomic_add_nob(&orig_allctr->cpool.stat.blocks_size[alloc_no],
                            ((erts_aint_t) crr->cpool.blocks_size[alloc_no]));
        erts_atomic_add_nob(&orig_allctr->cpool.stat.no_blocks[alloc_no],
                            ((erts_aint_t) crr->cpool.blocks[alloc_no]));
    }

    erts_atomic_add_nob(&orig_allctr->cpool.stat.carriers_size,
			(erts_aint_t) CARRIER_SZ(crr));
    erts_atomic_inc_nob(&orig_allctr->cpool.stat.no_carriers);

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

    LTTNG3(carrier_pool_put, ERTS_ALC_A2AD(allctr->alloc_no), allctr->ix, CARRIER_SZ(crr));
}

static void
cpool_delete(Allctr_t *allctr, Allctr_t *prev_allctr, Carrier_t *crr)
{
    ErtsAlcCPoolData_t *cpd1p, *cpd2p;
    erts_aint_t val;
#ifdef ERTS_ALC_CPOOL_DEBUG
    ErtsAlcCPoolData_t *sentinel = allctr->cpool.sentinel;
#endif

    ERTS_ALC_CPOOL_ASSERT(allctr->alloc_no == ERTS_ALC_A_TEST /* testcase */
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

    {
        Allctr_t *orig_allctr = crr->cpool.orig_allctr;
        int alloc_no = allctr->alloc_no;

        ERTS_ALC_CPOOL_ASSERT(orig_allctr == prev_allctr);

        ERTS_ALC_CPOOL_ASSERT(crr->cpool.blocks_size[alloc_no] <=
            erts_atomic_read_nob(&orig_allctr->cpool.stat.blocks_size[alloc_no]));

        ERTS_ALC_CPOOL_ASSERT(crr->cpool.blocks[alloc_no] <=
            erts_atomic_read_nob(&orig_allctr->cpool.stat.no_blocks[alloc_no]));

        /* We only modify the counters for our current type since the others
         * were, conceptually, never taken out of the pool. */
        erts_atomic_add_nob(&orig_allctr->cpool.stat.blocks_size[alloc_no],
                            -((erts_aint_t) crr->cpool.blocks_size[alloc_no]));
        erts_atomic_add_nob(&orig_allctr->cpool.stat.no_blocks[alloc_no],
                            -((erts_aint_t) crr->cpool.blocks[alloc_no]));

        erts_atomic_add_nob(&orig_allctr->cpool.stat.carriers_size,
			-((erts_aint_t) CARRIER_SZ(crr)));
        erts_atomic_dec_wb(&orig_allctr->cpool.stat.no_carriers);
    }

}

static Carrier_t *
cpool_fetch(Allctr_t *allctr, UWord size)
{
    int i, seen_sentinel;
    Carrier_t *crr;
    Carrier_t *reinsert_crr = NULL;
    ErtsAlcCPoolData_t *cpdp;
    ErtsAlcCPoolData_t *cpool_entrance = NULL;
    ErtsAlcCPoolData_t *sentinel;

    ERTS_ALC_CPOOL_ASSERT(allctr->alloc_no == ERTS_ALC_A_TEST /* testcase */
			  || erts_thr_progress_is_managed_thread());

    i = ERTS_ALC_CPOOL_MAX_FETCH_INSPECT;

    LTTNG3(carrier_pool_get, ERTS_ALC_A2AD(allctr->alloc_no), allctr->ix, (unsigned long)size);
    /*
     * Search my own pooled_tree,
     * i.e my abandoned carriers that were in the pool last time I checked.
     */
    do {
        erts_aint_t exp, act;

        crr = aoff_lookup_pooled_mbc(allctr, size);
        if (!crr)
            break;

        ASSERT(crr->cpool.state == ERTS_MBC_WAS_POOLED);
        ASSERT(crr->cpool.orig_allctr == allctr);

        aoff_remove_pooled_mbc(allctr, crr);

        exp = erts_atomic_read_nob(&crr->allctr);
        if (exp & ERTS_CRR_ALCTR_FLG_IN_POOL) {
            ASSERT((exp & ~ERTS_CRR_ALCTR_FLG_MASK) == (erts_aint_t)allctr);
            if (erts_atomic_read_nob(&crr->cpool.max_size) < size) {
                /*
                 * This carrier has been fetched and inserted back again
                 * by a foreign allocator. That's why it has a stale search size.
                 */
                ASSERT(exp & ERTS_CRR_ALCTR_FLG_HOMECOMING);
                crr->cpool.pooled.hdr.bhdr = erts_atomic_read_nob(&crr->cpool.max_size);
                aoff_add_pooled_mbc(allctr, crr);
                INC_CC(allctr->cpool.stat.skip_size);
                continue;
            }
            else if (exp & ERTS_CRR_ALCTR_FLG_BUSY) {
                /*
                 * This must be our own carrier as part of a realloc call.
                 * Skip it to make things simpler.
                 * Must wait to re-insert to not be found again by lookup.
                 */
                ASSERT(!reinsert_crr);
                reinsert_crr = crr;
                INC_CC(allctr->cpool.stat.skip_busy);
                continue;
            }

            /* Try to fetch it... */
            act = erts_atomic_cmpxchg_mb(&crr->allctr,
                                         exp & ~ERTS_CRR_ALCTR_FLG_IN_POOL,
                                         exp);
            if (act == exp) {
                cpool_delete(allctr, allctr, crr);
                crr->cpool.state = ERTS_MBC_IS_HOME;

                if (reinsert_crr)
                    aoff_add_pooled_mbc(allctr, reinsert_crr);
                return crr;
            }
            exp = act;
            INC_CC(allctr->cpool.stat.skip_race);
        }
        else
            INC_CC(allctr->cpool.stat.skip_not_pooled);

        /* Not in pool anymore */
        ASSERT(!(exp & ERTS_CRR_ALCTR_FLG_BUSY));
        crr->cpool.state = ERTS_MBC_WAS_TRAITOR;

    }while (--i > 0);

    if (reinsert_crr)
        aoff_add_pooled_mbc(allctr, reinsert_crr);

    /*
     * Try find a nice cpool_entrance
     */
    while (allctr->cpool.pooled_tree) {
        erts_aint_t iallctr;

        crr = ErtsContainerStruct(allctr->cpool.pooled_tree, Carrier_t, cpool.pooled);
        iallctr = erts_atomic_read_nob(&crr->allctr);
        if (iallctr & ERTS_CRR_ALCTR_FLG_IN_POOL) {
            cpool_entrance = &crr->cpool;
            break;
        }
        /* Not in pool anymore */
        ASSERT(!(iallctr & ERTS_CRR_ALCTR_FLG_BUSY));
        aoff_remove_pooled_mbc(allctr, crr);
        crr->cpool.state = ERTS_MBC_WAS_TRAITOR;

        if (--i <= 0) {
            INC_CC(allctr->cpool.stat.fail_pooled);
            return NULL;
        }
    }


    /*
     * Finally search the shared pool and try employ foreign carriers
     */
    sentinel = allctr->cpool.sentinel;
    if (cpool_entrance) {
        /*
         * We saw a pooled carried above, use it as entrance into the pool
	 */
    }
    else {
        /*
         * No pooled carrier seen above. Start search at cpool sentinel,
	 * but begin by passing one element before trying to fetch.
	 * This in order to avoid contention with threads inserting elements.
	 */
        cpool_entrance = cpool_aint2cpd(cpool_read(&sentinel->prev));
	if (cpool_entrance == sentinel)
	    goto check_dc_list;
    }

    cpdp = cpool_entrance;
    seen_sentinel = 0;
    do {
	erts_aint_t exp;
	cpdp = cpool_aint2cpd(cpool_read(&cpdp->prev));
        if (cpdp == sentinel) {
	    if (seen_sentinel) {
		/* We been here before. cpool_entrance must have been removed */
                INC_CC(allctr->cpool.stat.entrance_removed);
		break;
	    }
            seen_sentinel = 1;
            continue;
	}
        ASSERT(cpdp != cpool_entrance || seen_sentinel);

	crr = ErtsContainerStruct(cpdp, Carrier_t, cpool);
	exp = erts_atomic_read_rb(&crr->allctr);

        if (erts_atomic_read_nob(&cpdp->max_size) < size) {
            INC_CC(allctr->cpool.stat.skip_size);
        }
        else if ((exp & (ERTS_CRR_ALCTR_FLG_IN_POOL | ERTS_CRR_ALCTR_FLG_BUSY))
                  == ERTS_CRR_ALCTR_FLG_IN_POOL) {
	    erts_aint_t act;
            erts_aint_t want = (((erts_aint_t) allctr)
                                | (exp & ERTS_CRR_ALCTR_FLG_HOMECOMING));
            /* Try to fetch it... */
	    act = erts_atomic_cmpxchg_mb(&crr->allctr, want, exp);
	    if (act == exp) {
		cpool_delete(allctr, ((Allctr_t *) (act & ~ERTS_CRR_ALCTR_FLG_MASK)), crr);
		if (crr->cpool.orig_allctr == allctr) {
		    unlink_abandoned_carrier(crr);
                    crr->cpool.state = ERTS_MBC_IS_HOME;
                }
		return crr;
	    }
	}

        if (exp & ERTS_CRR_ALCTR_FLG_BUSY)
            INC_CC(allctr->cpool.stat.skip_busy);
        else
            INC_CC(allctr->cpool.stat.skip_race);

	if (--i <= 0) {
            INC_CC(allctr->cpool.stat.fail_shared);
	    return NULL;
        }
    }while (cpdp != cpool_entrance);

check_dc_list:
    /* Last; check our own pending dealloc carrier list... */
    crr = allctr->cpool.dc_list.last;
    while (crr) {
	if (erts_atomic_read_nob(&crr->cpool.max_size) >= size) {
	    Block_t* blk;
	    unlink_carrier(&allctr->cpool.dc_list, crr);
	    ERTS_ALC_CPOOL_ASSERT(erts_atomic_read_nob(&crr->allctr)
                                  == ((erts_aint_t) allctr));
	    blk = MBC_TO_FIRST_BLK(allctr, crr);
	    ASSERT(FBLK_TO_MBC(blk) == crr);
	    allctr->link_free_block(allctr, blk);
	    return crr;
	}
	crr = crr->prev;
	if (--i <= 0) {
            INC_CC(allctr->cpool.stat.fail_pend_dealloc);
	    return NULL;
        }
    }

    if (i != ERTS_ALC_CPOOL_MAX_FETCH_INSPECT)
        INC_CC(allctr->cpool.stat.fail);

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

    ASSERT(IS_MB_CARRIER(crr));

    if (!ERTS_ALC_IS_CPOOL_ENABLED(allctr)) {
	dealloc_mbc(allctr, crr);
	return;
    }

    orig_allctr = crr->cpool.orig_allctr;

    if (allctr == orig_allctr) {
        if (!(erts_atomic_read_nob(&crr->allctr) & ERTS_CRR_ALCTR_FLG_HOMECOMING)) {
            dealloc_my_carrier(allctr, crr);
        }
        /*else
         * Carrier was abandoned earlier by other thread and
         * is still waiting for us in dd-queue.
         * handle_delayed_dealloc() will handle it when crr is dequeued.
         */
    }
    else {
	/*
	 * We send the carrier to its origin for deallocation.
	 * This in order:
	 * - not to complicate things for the thread specific
	 *   instances of mseg_alloc, and
	 * - to ensure that we always only reuse empty carriers
	 *   originating from our own thread specific mseg_alloc
	 *   instance which is beneficial on NUMA systems.
	 */
        erts_aint_t iallctr;
#ifdef ERTS_ALC_CPOOL_DEBUG
	Block_t* first_blk = MBC_TO_FIRST_BLK(allctr, crr);
	ERTS_ALC_CPOOL_ASSERT(IS_FREE_LAST_MBC_BLK(first_blk));

	ERTS_ALC_CPOOL_ASSERT(IS_MBC_FIRST_ABLK(allctr, first_blk));
	ERTS_ALC_CPOOL_ASSERT(crr == FBLK_TO_MBC(first_blk));
	ERTS_ALC_CPOOL_ASSERT(crr == FIRST_BLK_TO_MBC(allctr, first_blk));
	ERTS_ALC_CPOOL_ASSERT((erts_atomic_read_nob(&crr->allctr)
                               & ~ERTS_CRR_ALCTR_FLG_HOMECOMING)
                              == (erts_aint_t) allctr);
#endif

        iallctr = (erts_aint_t)orig_allctr | ERTS_CRR_ALCTR_FLG_HOMECOMING;
        if (!(erts_atomic_xchg_nob(&crr->allctr, iallctr)
              & ERTS_CRR_ALCTR_FLG_HOMECOMING)) {
            enqueue_homecoming(allctr, crr);
        }
    }
}

static void dealloc_my_carrier(Allctr_t *allctr, Carrier_t *crr)
{
    Block_t *blk;
    int check_pending_dealloc;
    erts_aint_t max_size;

    ERTS_ALC_CPOOL_ASSERT(allctr == crr->cpool.orig_allctr);
    if (is_abandoned(crr)) {
        unlink_abandoned_carrier(crr);
        crr->cpool.state = ERTS_MBC_IS_HOME;
    }

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
    crr->cpool.homecoming_dd.blk.bhdr = HOMECOMING_MBC_BLK_HDR;
    erts_atomic_init_nob(&crr->cpool.next, ERTS_AINT_NULL);
    erts_atomic_init_nob(&crr->cpool.prev, ERTS_AINT_NULL);
    crr->cpool.orig_allctr = allctr;
    crr->cpool.thr_prgr = ERTS_THR_PRGR_INVALID;
    erts_atomic_init_nob(&crr->cpool.max_size, 0);
    sys_memset(&crr->cpool.blocks_size, 0, sizeof(crr->cpool.blocks_size));
    sys_memset(&crr->cpool.blocks, 0, sizeof(crr->cpool.blocks));
    crr->cpool.total_blocks_size = 0;
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
    crr->cpool.state = ERTS_MBC_IS_HOME;
}



static UWord
allctr_abandon_limit(Allctr_t *allctr)
{
    UWord limit;
    UWord csz;

    csz = allctr->mbcs.curr.norm.mseg.size;
    csz += allctr->mbcs.curr.norm.sys_alloc.size;

    limit = csz*allctr->cpool.util_limit;
    if (limit > csz)
	limit /= 100;
    else
	limit = (csz/100)*allctr->cpool.util_limit;

    return limit;
}

static void ERTS_INLINE
set_new_allctr_abandon_limit(Allctr_t *allctr)
{
    allctr->cpool.abandon_limit = allctr_abandon_limit(allctr);
}

static void
abandon_carrier(Allctr_t *allctr, Carrier_t *crr)
{
    erts_aint_t iallctr;

    STAT_MBC_ABANDON(allctr, crr);

    unlink_carrier(&allctr->mbc_list, crr);
    allctr->remove_mbc(allctr, crr);

    /* Mark our free blocks as unused and reclaimable to the OS. */
    carrier_mem_discard_free_blocks(allctr, crr);

    cpool_insert(allctr, crr);


    iallctr = erts_atomic_read_nob(&crr->allctr);
    if (allctr == crr->cpool.orig_allctr) {
        /* preserve HOMECOMING flag */
        ASSERT((iallctr & ~ERTS_CRR_ALCTR_FLG_HOMECOMING) == (erts_aint_t)allctr);
        erts_atomic_set_wb(&crr->allctr, iallctr | ERTS_CRR_ALCTR_FLG_IN_POOL);
        poolify_my_carrier(allctr, crr);
    }
    else {
        ASSERT((iallctr & ~ERTS_CRR_ALCTR_FLG_HOMECOMING) == (erts_aint_t)allctr);
        iallctr = ((erts_aint_t)crr->cpool.orig_allctr |
                   ERTS_CRR_ALCTR_FLG_HOMECOMING |
                   ERTS_CRR_ALCTR_FLG_IN_POOL);
        if (!(erts_atomic_xchg_wb(&crr->allctr, iallctr)
              & ERTS_CRR_ALCTR_FLG_HOMECOMING)) {

            enqueue_homecoming(allctr, crr);
        }
    }
}

static void
enqueue_homecoming(Allctr_t* allctr, Carrier_t* crr)
{
    Allctr_t* orig_allctr = crr->cpool.orig_allctr;
    const int cinit = orig_allctr->dd.ix - allctr->dd.ix;
    Block_t* dd_blk = &crr->cpool.homecoming_dd.blk;

    /*
     * The receiver will recognize this as a carrier
     * (and not a block which is the common case)
     * since the block header is HOMECOMING_MBC_BLK_HDR.
     */
    ASSERT(dd_blk->bhdr == HOMECOMING_MBC_BLK_HDR);
    if (ddq_enqueue(&orig_allctr->dd.q, BLK2UMEM(dd_blk), cinit))
        erts_alloc_notify_delayed_dealloc(orig_allctr->ix);
}

static void
poolify_my_carrier(Allctr_t *allctr, Carrier_t *crr)
{
    ERTS_ALC_CPOOL_ASSERT(allctr == crr->cpool.orig_allctr);

    crr->cpool.pooled.hdr.bhdr = erts_atomic_read_nob(&crr->cpool.max_size);
    aoff_add_pooled_mbc(allctr, crr);
    crr->cpool.state = ERTS_MBC_WAS_POOLED;
}

static void
cpool_read_stat(Allctr_t *allctr, int alloc_no,
                UWord *nocp, UWord *cszp, UWord *nobp, UWord *bszp)
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
			? erts_atomic_read_nob(&allctr->cpool.stat.no_blocks[alloc_no])
			: 0);
	tbsz = (UWord) (bszp
			? erts_atomic_read_nob(&allctr->cpool.stat.blocks_size[alloc_no])
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

    allctr->cpool.disable_abandon = ERTS_ALC_CPOOL_MAX_DISABLE_ABANDON;

    if ((flags & (CFLG_MBC|CFLG_NO_CPOOL)) == CFLG_MBC
	&& ERTS_ALC_IS_CPOOL_ENABLED(allctr)
	&& erts_thr_progress_is_managed_thread()) {
	crr = cpool_fetch(allctr, blk_sz);
	if (crr) {
	    STAT_MBC_CPOOL_FETCH(allctr, crr);
            INC_CC(allctr->cpool.stat.fetch);
	    link_carrier(&allctr->mbc_list, crr);
	    (*allctr->add_mbc)(allctr, crr);
	    blk = (*allctr->get_free_block)(allctr, blk_sz, NULL, 0);
	    ASSERT(blk);
	    return blk;
	}
    }

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
        set_new_allctr_abandon_limit(allctr);

	blk = MBC_TO_FIRST_BLK(allctr, crr);

	blk_sz = UNIT_FLOOR(crr_sz - MBC_HEADER_SIZE(allctr));

	SET_MBC_FBLK_HDR(blk, blk_sz, SBH_THIS_FREE|SBH_LAST_BLK, crr);

	if (flags & CFLG_MAIN_CARRIER) {
	    ASSERT(!allctr->main_carrier);
	    allctr->main_carrier = crr;
	}

	cpool_init_carrier_data(allctr, crr);

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

	if (busy_pcrr_pp && *busy_pcrr_pp) {
            erts_aint_t iallctr = erts_atomic_read_nob(&crr->allctr);
	    ERTS_ALC_CPOOL_ASSERT(*busy_pcrr_pp == crr);
            ERTS_ALC_CPOOL_ASSERT((iallctr & ~ERTS_CRR_ALCTR_FLG_HOMECOMING)
                                  == (((erts_aint_t) allctr)
                                      | ERTS_CRR_ALCTR_FLG_IN_POOL
                                      | ERTS_CRR_ALCTR_FLG_BUSY));
            ERTS_ALC_CPOOL_ASSERT(allctr == crr->cpool.orig_allctr);

            *busy_pcrr_pp = NULL;
	    erts_atomic_set_nob(&crr->allctr,
                                (iallctr & ~(ERTS_CRR_ALCTR_FLG_IN_POOL |
                                             ERTS_CRR_ALCTR_FLG_BUSY)));
	    cpool_delete(allctr, allctr, crr);
	}
	else
	{
	    unlink_carrier(&allctr->mbc_list, crr);
            STAT_MBC_FREE(allctr, crr);
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
                CARRIER_SZ(crr),
                mbc_stats,
                sbc_stats);
        }
#endif

	schedule_dealloc_carrier(allctr, crr);
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
    Eterm atags;
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
    Eterm acnl;
    Eterm acfml;

#if HAVE_ERTS_MSEG
    Eterm mmc;
#endif
    Eterm ycs;
    Eterm sac;

    Eterm fix_types;

    Eterm mbcs;
    Eterm mbcs_pool;
    Eterm fetch;
    Eterm fail_pooled;
    Eterm fail_shared;
    Eterm fail_pend_dealloc;
    Eterm fail;
    Eterm skip_size;
    Eterm skip_busy;
    Eterm skip_not_pooled;
    Eterm skip_homecoming;
    Eterm skip_race;
    Eterm entrance_removed;
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

    Eterm foreign_blocks;

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

static Eterm alloc_type_atoms[ERTS_ALC_N_MAX + 1];

static ERTS_INLINE void atom_init(Eterm *atom, char *name)
{
    *atom = am_atom_put(name, sys_strlen(name));
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
	AM_INIT(atags);
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
        AM_INIT(acnl);
        AM_INIT(acfml);

#if HAVE_ERTS_MSEG
	AM_INIT(mmc);
#endif
	AM_INIT(ycs);
	AM_INIT(sac);

	AM_INIT(fix_types);

	AM_INIT(mbcs);
	AM_INIT(mbcs_pool);
	AM_INIT(fetch);
        AM_INIT(fail_pooled);
        AM_INIT(fail_shared);
        AM_INIT(fail_pend_dealloc);
        AM_INIT(fail);
        AM_INIT(skip_size);
        AM_INIT(skip_busy);
        AM_INIT(skip_not_pooled);
        AM_INIT(skip_homecoming);
        AM_INIT(skip_race);
        AM_INIT(entrance_removed);
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
	AM_INIT(foreign_blocks);

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

        for (ix = ERTS_ALC_N_MIN; ix <= ERTS_ALC_N_MAX; ix++) {
            const char *name = ERTS_ALC_N2TD(ix);
            size_t len = sys_strlen(name);

            alloc_type_atoms[ix] = am_atom_put(name, len);
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
	    fmtfn_t *print_to_p,
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
		    fmtfn_t to = *print_to_p;
		    void *arg = print_to_arg;
		    erts_print(to,
			       arg,
			       "fix type internal: %s %bpu %bpu\n",
			       (char *) ERTS_ALC_T2TD(fix->type),
			       alloced,
			       used);
		}

		if (hpp || szp) {
		    add_3tup(hpp, szp, &res,
			     alloc_type_atoms[ERTS_ALC_T2N(fix->type)],
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
		fmtfn_t to = *print_to_p;
		void *arg = print_to_arg;
		erts_print(to,
			   arg,
			   "fix type: %s %bpu %bpu\n",
			   (char *) ERTS_ALC_T2TD(fix->type),
			   alloced,
			   used);
	    }

	    if (hpp || szp) {
		add_3tup(hpp, szp, &res,
			 alloc_type_atoms[ERTS_ALC_T2N(fix->type)],
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
		 fmtfn_t *print_to_p,
		 void *print_to_arg,
		 Uint **hpp,
		 Uint *szp)
{
    Eterm res = THE_NON_VALUE;
    UWord curr_size = cs->curr.norm.mseg.size + cs->curr.norm.sys_alloc.size;

    if (print_to_p) {
	fmtfn_t to = *print_to_p;
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
info_cpool(Allctr_t *allctr,
	   int sz_only,
	   char *prefix,
	   fmtfn_t *print_to_p,
	   void *print_to_arg,
	   Uint **hpp,
	   Uint *szp)
{
    Eterm res = THE_NON_VALUE;
    UWord noc, csz, nob, bsz;

    noc = csz = nob = bsz = ~0;
    if (print_to_p || hpp) {
	if (sz_only)
	    cpool_read_stat(allctr, allctr->alloc_no, NULL, &csz, NULL, &bsz);
	else
	    cpool_read_stat(allctr, allctr->alloc_no, &noc, &csz, &nob, &bsz);
    }

    if (print_to_p) {
	fmtfn_t to = *print_to_p;
	void *arg = print_to_arg;
	if (!sz_only)
	    erts_print(to, arg, "%sblocks: %bpu\n", prefix, nob);
	erts_print(to, arg, "%sblocks size: %bpu\n", prefix, bsz);
	if (!sz_only)
	    erts_print(to, arg, "%scarriers: %bpu\n", prefix, noc);
	erts_print(to, arg, "%scarriers size: %bpu\n", prefix, csz);
    }

    if (hpp || szp) {
        Eterm foreign_blocks;
        int i;

        foreign_blocks = NIL;
	res = NIL;

      if (!sz_only) {
        add_3tup(hpp, szp, &res, am.fail_pooled,
                 bld_unstable_uint(hpp, szp, ERTS_ALC_CC_GIGA_VAL(allctr->cpool.stat.fail_pooled)),
                 bld_unstable_uint(hpp, szp, ERTS_ALC_CC_VAL(allctr->cpool.stat.fail_pooled)));

        add_3tup(hpp, szp, &res, am.fail_shared,
                 bld_unstable_uint(hpp, szp, ERTS_ALC_CC_GIGA_VAL(allctr->cpool.stat.fail_shared)),
                 bld_unstable_uint(hpp, szp, ERTS_ALC_CC_VAL(allctr->cpool.stat.fail_shared)));

        add_3tup(hpp, szp, &res, am.fail_pend_dealloc,
                 bld_unstable_uint(hpp, szp, ERTS_ALC_CC_GIGA_VAL(allctr->cpool.stat.fail_pend_dealloc)),
                 bld_unstable_uint(hpp, szp, ERTS_ALC_CC_VAL(allctr->cpool.stat.fail_pend_dealloc)));

        add_3tup(hpp, szp, &res, am.fail,
                 bld_unstable_uint(hpp, szp, ERTS_ALC_CC_GIGA_VAL(allctr->cpool.stat.fail)),
                 bld_unstable_uint(hpp, szp, ERTS_ALC_CC_VAL(allctr->cpool.stat.fail)));

        add_3tup(hpp, szp, &res, am.fetch,
                 bld_unstable_uint(hpp, szp, ERTS_ALC_CC_GIGA_VAL(allctr->cpool.stat.fetch)),
                 bld_unstable_uint(hpp, szp, ERTS_ALC_CC_VAL(allctr->cpool.stat.fetch)));

        add_3tup(hpp, szp, &res, am.skip_size,
                 bld_unstable_uint(hpp, szp, ERTS_ALC_CC_GIGA_VAL(allctr->cpool.stat.skip_size)),
                 bld_unstable_uint(hpp, szp, ERTS_ALC_CC_VAL(allctr->cpool.stat.skip_size)));

        add_3tup(hpp, szp, &res, am.skip_busy,
                 bld_unstable_uint(hpp, szp, ERTS_ALC_CC_GIGA_VAL(allctr->cpool.stat.skip_busy)),
                 bld_unstable_uint(hpp, szp, ERTS_ALC_CC_VAL(allctr->cpool.stat.skip_busy)));

        add_3tup(hpp, szp, &res, am.skip_not_pooled,
                 bld_unstable_uint(hpp, szp, ERTS_ALC_CC_GIGA_VAL(allctr->cpool.stat.skip_not_pooled)),
                 bld_unstable_uint(hpp, szp, ERTS_ALC_CC_VAL(allctr->cpool.stat.skip_not_pooled)));

        add_3tup(hpp, szp, &res, am.skip_homecoming,
                 bld_unstable_uint(hpp, szp, ERTS_ALC_CC_GIGA_VAL(allctr->cpool.stat.skip_homecoming)),
                 bld_unstable_uint(hpp, szp, ERTS_ALC_CC_VAL(allctr->cpool.stat.skip_homecoming)));

        add_3tup(hpp, szp, &res, am.skip_race,
                 bld_unstable_uint(hpp, szp, ERTS_ALC_CC_GIGA_VAL(allctr->cpool.stat.skip_race)),
                 bld_unstable_uint(hpp, szp, ERTS_ALC_CC_VAL(allctr->cpool.stat.skip_race)));

        add_3tup(hpp, szp, &res, am.entrance_removed,
                 bld_unstable_uint(hpp, szp, ERTS_ALC_CC_GIGA_VAL(allctr->cpool.stat.entrance_removed)),
                 bld_unstable_uint(hpp, szp, ERTS_ALC_CC_VAL(allctr->cpool.stat.entrance_removed)));
       }

	add_2tup(hpp, szp, &res,
		 am.carriers_size,
		 bld_unstable_uint(hpp, szp, csz));

        if (!sz_only) {
            add_2tup(hpp, szp, &res,
                     am.carriers,
                     bld_unstable_uint(hpp, szp, noc));
        }

	add_2tup(hpp, szp, &res,
		 am.blocks_size,
		 bld_unstable_uint(hpp, szp, bsz));

	if (!sz_only) {
	    add_2tup(hpp, szp, &res,
		     am.blocks,
		     bld_unstable_uint(hpp, szp, nob));
        }

        for (i = ERTS_ALC_A_MIN; i <= ERTS_ALC_A_MAX; i++) {
            const char *name_str;
            Eterm name, info;

            if (i == allctr->alloc_no) {
                continue;
            }

            cpool_read_stat(allctr, i, NULL, NULL, &nob, &bsz);

            if (bsz == 0 && (nob == 0 || sz_only)) {
                continue;
            }

            name_str = ERTS_ALC_A2AD(i);
            info = NIL;

            add_2tup(hpp, szp, &info,
                     am.blocks_size,
                     bld_unstable_uint(hpp, szp, bsz));

            if (!sz_only) {
                add_2tup(hpp, szp, &info,
                     am.blocks,
                     bld_unstable_uint(hpp, szp, nob));
            }

            name = am_atom_put(name_str, sys_strlen(name_str));

            add_2tup(hpp, szp, &foreign_blocks, name, info);
        }

        add_2tup(hpp, szp, &res, am.foreign_blocks, foreign_blocks);
    }

    return res;
}


static Eterm
info_carriers(Allctr_t *allctr,
	      CarriersStats_t *cs,
	      char *prefix,
	      fmtfn_t *print_to_p,
	      void *print_to_arg,
	      Uint **hpp,
	      Uint *szp)
{
    Eterm res = THE_NON_VALUE;
    UWord curr_no, curr_size;
    
    curr_no = cs->curr.norm.mseg.no + cs->curr.norm.sys_alloc.no;
    curr_size = cs->curr.norm.mseg.size + cs->curr.norm.sys_alloc.size;

    if (print_to_p) {
	fmtfn_t to = *print_to_p;
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
    size_t prefix_len = sys_strlen(allctr->name_prefix);

    if (prefix_len > MAX_ATOM_CHARACTERS + sizeof(realloc) - 1)
	erts_exit(ERTS_ERROR_EXIT,"Too long allocator name: %salloc\n",allctr->name_prefix);

    sys_memcpy((void *) buf, (void *) allctr->name_prefix, prefix_len);

    sys_memcpy((void *) &buf[prefix_len], (void *) alloc, sizeof(alloc) - 1);
    allctr->name.alloc = am_atom_put(buf, prefix_len + sizeof(alloc) - 1);

    sys_memcpy((void *) &buf[prefix_len], (void *) realloc, sizeof(realloc) - 1);
    allctr->name.realloc = am_atom_put(buf, prefix_len + sizeof(realloc) - 1);

    sys_memcpy((void *) &buf[prefix_len], (void *) free, sizeof(free) - 1);
    allctr->name.free = am_atom_put(buf, prefix_len + sizeof(free) - 1);

}

static Eterm
info_calls(Allctr_t *allctr,
	   fmtfn_t *print_to_p,
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
	fmtfn_t to = *print_to_p;
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
             fmtfn_t *print_to_p,
	     void *print_to_arg,
	     Uint **hpp,
	     Uint *szp)
{
    Eterm res = THE_NON_VALUE;
    UWord acul, acnl, acfml;

    if (!allctr) {
	if (print_to_p)
	    erts_print(*print_to_p, print_to_arg, "option e: false\n");
	if (hpp || szp) {
	    res = NIL;
	    add_2tup(hpp, szp, &res, am.e, am_false);
	}
	return res;
    }

    acul = allctr->cpool.util_limit;
    acnl = allctr->cpool.in_pool_limit;
    acfml = allctr->cpool.fblk_min_limit;

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
		   "option atags: %s\n"
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
		   "option acul: %bpu\n",
		   topt,
		   allctr->ramv ? "true" : "false",
		   allctr->atags ? "true" : "false",
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
                 am.acfml,
                 bld_uint(hpp, szp, acfml));
        add_2tup(hpp, szp, &res,
                 am.acnl,
                 bld_uint(hpp, szp, acnl));
	add_2tup(hpp, szp, &res,
		 am.acul,
		 bld_uint(hpp, szp, acul));
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
		 am_sbct,
		 bld_uint(hpp, szp, allctr->sbc_threshold));
	add_2tup(hpp, szp, &res, am.ramv, allctr->ramv ? am_true : am_false);
	add_2tup(hpp, szp, &res, am.atags, allctr->atags ? am_true : am_false);
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
erts_alcu_au_info_options(fmtfn_t *print_to_p, void *print_to_arg,
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
		       fmtfn_t *print_to_p,
		       void *print_to_arg,
		       Uint **hpp,
		       Uint *szp)
{
    Eterm res;

    if (hpp || szp)
	ensure_atoms_initialized(allctr);

    if (allctr->thread_safe) {
	erts_allctr_wrapper_pre_lock();
	erts_mtx_lock(&allctr->mutex);
    }
    res = info_options(allctr, print_to_p, print_to_arg, hpp, szp);
    if (allctr->thread_safe) { 
	erts_mtx_unlock(&allctr->mutex);
	erts_allctr_wrapper_pre_unlock();
    }
    return res;
}

/* ----------------------------------------------------------------------- */

Eterm
erts_alcu_sz_info(Allctr_t *allctr,
		  int internal,
		  int begin_max_period,
		  fmtfn_t *print_to_p,
		  void *print_to_arg,
		  Uint **hpp,
		  Uint *szp)
{
    Eterm res, mbcs, sbcs, fix = THE_NON_VALUE;
    Eterm mbcs_pool;

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

    if (allctr->thread_safe) {
	erts_allctr_wrapper_pre_lock();
	erts_mtx_lock(&allctr->mutex);
    }

    ERTS_ALCU_DBG_CHK_THR_ACCESS(allctr);

    /* Update sbc values not continuously updated */
    allctr->sbcs.blocks.curr.no
	= allctr->sbcs.curr.norm.mseg.no + allctr->sbcs.curr.norm.sys_alloc.no;
    allctr->sbcs.blocks.max.no = allctr->sbcs.max.no;

    update_max_ever_values(&allctr->mbcs);
    update_max_ever_values(&allctr->sbcs);

    if (allctr->fix)
	fix = sz_info_fix(allctr, internal, print_to_p, print_to_arg, hpp, szp);
    mbcs = sz_info_carriers(allctr, &allctr->mbcs, "mbcs ", print_to_p,
			    print_to_arg, hpp, szp);
    if (ERTS_ALC_IS_CPOOL_ENABLED(allctr))
	mbcs_pool = info_cpool(allctr, 1, "mbcs_pool ", print_to_p,
			       print_to_arg, hpp, szp);
    else
	mbcs_pool = THE_NON_VALUE; /* shut up annoying warning... */
    sbcs = sz_info_carriers(allctr, &allctr->sbcs, "sbcs ", print_to_p,
			    print_to_arg, hpp, szp);

    if (hpp || szp) {
	res = NIL;
	add_2tup(hpp, szp, &res, am.sbcs, sbcs);
	if (ERTS_ALC_IS_CPOOL_ENABLED(allctr))
	    add_2tup(hpp, szp, &res, am.mbcs_pool, mbcs_pool);
	add_2tup(hpp, szp, &res, am.mbcs, mbcs);
	add_fix_types(allctr, internal, hpp, szp, &res, fix);
    }

    if (begin_max_period) {
	reset_max_values(&allctr->mbcs);
	reset_max_values(&allctr->sbcs);
    }


    if (allctr->thread_safe) {
	erts_mtx_unlock(&allctr->mutex);
	erts_allctr_wrapper_pre_unlock();
    }

    return res;
}


Eterm
erts_alcu_info(Allctr_t *allctr,
	       int internal,
	       int begin_max_period,
	       fmtfn_t *print_to_p,
	       void *print_to_arg,
	       Uint **hpp,
	       Uint *szp)
{
    Eterm res, sett, mbcs, sbcs, calls, fix = THE_NON_VALUE;
    Eterm mbcs_pool;

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

    if (allctr->thread_safe) {
	erts_allctr_wrapper_pre_lock();
	erts_mtx_lock(&allctr->mutex);
    }

    ERTS_ALCU_DBG_CHK_THR_ACCESS(allctr);

    /* Update sbc values not continuously updated */
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
    if (ERTS_ALC_IS_CPOOL_ENABLED(allctr))
	mbcs_pool = info_cpool(allctr, 0, "mbcs_pool ", print_to_p,
			       print_to_arg, hpp, szp);
    else
	mbcs_pool = THE_NON_VALUE; /* shut up annoying warning... */
    sbcs = info_carriers(allctr, &allctr->sbcs, "sbcs ", print_to_p,
			 print_to_arg, hpp, szp);
    calls = info_calls(allctr, print_to_p, print_to_arg, hpp, szp);

    if (hpp || szp) {
	res = NIL;

	add_2tup(hpp, szp, &res, am.calls, calls);
	add_2tup(hpp, szp, &res, am.sbcs, sbcs);
	if (ERTS_ALC_IS_CPOOL_ENABLED(allctr))
	    add_2tup(hpp, szp, &res, am.mbcs_pool, mbcs_pool);
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


    if (allctr->thread_safe) {
	erts_mtx_unlock(&allctr->mutex);
	erts_allctr_wrapper_pre_unlock();
    }

    return res;
}

void
erts_alcu_foreign_size(Allctr_t *allctr, ErtsAlcType_t alloc_no, AllctrSize_t *size)
{
    if (ERTS_ALC_IS_CPOOL_ENABLED(allctr)) {
        UWord csz, bsz;
        cpool_read_stat(allctr, alloc_no, NULL, &csz, NULL, &bsz);
        size->carriers = csz;
        size->blocks = bsz;
    } else {
        size->carriers = 0;
        size->blocks = 0;
    }
}

void
erts_alcu_current_size(Allctr_t *allctr, AllctrSize_t *size, ErtsAlcUFixInfo_t *fi, int fisz)
{

    if (allctr->thread_safe)
	erts_mtx_lock(&allctr->mutex);

    size->carriers = allctr->mbcs.curr.norm.mseg.size;
    size->carriers += allctr->mbcs.curr.norm.sys_alloc.size;
    size->carriers += allctr->sbcs.curr.norm.mseg.size;
    size->carriers += allctr->sbcs.curr.norm.sys_alloc.size;

    size->blocks = allctr->mbcs.blocks.curr.size;
    size->blocks += allctr->sbcs.blocks.curr.size;

    if (ERTS_ALC_IS_CPOOL_ENABLED(allctr)) {
	UWord csz, bsz;
	cpool_read_stat(allctr, allctr->alloc_no, NULL, &csz, NULL, &bsz);
	size->blocks += bsz;
	size->carriers += csz;
    }

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

    if (allctr->thread_safe)
	erts_mtx_unlock(&allctr->mutex);
}

/* ----------------------------------------------------------------------- */

static ERTS_INLINE void *
do_erts_alcu_alloc(ErtsAlcType_t type, Allctr_t *allctr, Uint size)
{
    void *res;

    ASSERT(initialized);

    ASSERT(allctr);

    ERTS_LC_ASSERT(!allctr->thread_safe
		       || erts_lc_mtx_is_locked(&allctr->mutex));

    ERTS_ALCU_DBG_CHK_THR_ACCESS(allctr);

    /* Reject sizes that can't fit into the header word. */
    if (size > ~BLK_FLG_MASK) {
        return NULL;
    }

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
    Allctr_t *allctr = (Allctr_t *) extra;
    void *res;

    ASSERT(!"This is not thread safe");

    res = do_erts_alcu_alloc(type, allctr, size);

    if (allctr->atags && res) {
        set_alloc_tag(allctr, res, determine_alloc_tag(allctr, type));
    }

    DEBUG_CHECK_ALIGNMENT(res);

    return res;
}



void *
erts_alcu_alloc_ts(ErtsAlcType_t type, void *extra, Uint size)
{
    Allctr_t *allctr = (Allctr_t *) extra;
    alcu_atag_t tag = 0;
    void *res;

    if (allctr->atags) {
        tag = determine_alloc_tag(allctr, type);
    }

    erts_mtx_lock(&allctr->mutex);

    res = do_erts_alcu_alloc(type, allctr, size);

    if (allctr->atags && res) {
        set_alloc_tag(allctr, res, tag);
    }

    erts_mtx_unlock(&allctr->mutex);

    DEBUG_CHECK_ALIGNMENT(res);

    return res;
}


void *
erts_alcu_alloc_thr_spec(ErtsAlcType_t type, void *extra, Uint size)
{
    ErtsAllocatorThrSpec_t *tspec = (ErtsAllocatorThrSpec_t *) extra;
    int ix;
    alcu_atag_t tag = 0;
    Allctr_t *allctr;
    void *res;

    ix = ERTS_ALC_GET_THR_IX();

    ASSERT(0 <= ix && ix < tspec->size);

    allctr = tspec->allctr[ix];

    if (allctr->atags) {
        tag = determine_alloc_tag(allctr, type);
    }

    if (allctr->thread_safe)
	erts_mtx_lock(&allctr->mutex);

    res = do_erts_alcu_alloc(type, allctr, size);

    if (allctr->atags && res) {
        set_alloc_tag(allctr, res, tag);
    }

    if (allctr->thread_safe)
	erts_mtx_unlock(&allctr->mutex);

    DEBUG_CHECK_ALIGNMENT(res);

    return res;
}

void *
erts_alcu_alloc_thr_pref(ErtsAlcType_t type, void *extra, Uint size)
{
    Allctr_t *pref_allctr;
    alcu_atag_t tag = 0;
    void *res;

    pref_allctr = get_pref_allctr(extra);

    if (pref_allctr->atags) {
        tag = determine_alloc_tag(pref_allctr, type);
    }

    if (pref_allctr->thread_safe)
	erts_mtx_lock(&pref_allctr->mutex);

    ASSERT(pref_allctr->dd.use);
    ERTS_ALCU_HANDLE_DD_IN_OP(pref_allctr, 1);

    ERTS_ALCU_DBG_CHK_THR_ACCESS(pref_allctr);

    res = do_erts_alcu_alloc(type, pref_allctr, size);

    if (!res && ERTS_ALCU_HANDLE_DD_IN_OP(pref_allctr, 1)) {
	/* Cleaned up a bit more; try one more time... */
	res = do_erts_alcu_alloc(type, pref_allctr, size);
    }

    if (pref_allctr->atags && res) {
        set_alloc_tag(pref_allctr, res, tag);
    }

    if (pref_allctr->thread_safe)
	erts_mtx_unlock(&pref_allctr->mutex);

    DEBUG_CHECK_ALIGNMENT(res);

    return res;
}



/* ------------------------------------------------------------------------- */

static ERTS_INLINE void
do_erts_alcu_free(ErtsAlcType_t type, Allctr_t *allctr, void *p,
		  Carrier_t **busy_pcrr_pp)
{
    ASSERT(initialized);

    ASSERT(allctr);

    ERTS_LC_ASSERT(!allctr->thread_safe
		       || erts_lc_mtx_is_locked(&allctr->mutex));

    ERTS_ALCU_DBG_CHK_THR_ACCESS(allctr);

    if (p) {
	INC_CC(allctr->calls.this_free);

        if (ERTS_ALC_IS_FIX_TYPE(type)) {
	    if (ERTS_ALC_IS_CPOOL_ENABLED(allctr))
		fix_cpool_free(allctr, type, 0, p, busy_pcrr_pp);
	    else
		fix_nocpool_free(allctr, type, p);
	}
	else {
	    Block_t *blk = UMEM2BLK(p);
	    if (IS_SBC_BLK(blk))
		destroy_carrier(allctr, blk, NULL);
	    else
		mbc_free(allctr, type, p, busy_pcrr_pp);
	}
    }
}

void erts_alcu_free(ErtsAlcType_t type, void *extra, void *p)
{
    Allctr_t *allctr = (Allctr_t *) extra;
    do_erts_alcu_free(type, allctr, p, NULL);
}


void
erts_alcu_free_ts(ErtsAlcType_t type, void *extra, void *p)
{
    Allctr_t *allctr = (Allctr_t *) extra;
    erts_mtx_lock(&allctr->mutex);
    do_erts_alcu_free(type, allctr, p, NULL);
    erts_mtx_unlock(&allctr->mutex);
}


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
	if (pref_allctr != used_allctr) {
	    enqueue_dealloc_other_instance(type,
                                           used_allctr,
                                           p,
                                           (used_allctr->dd.ix
                                            - pref_allctr->dd.ix));
        }
	else {
	    ERTS_ALCU_DBG_CHK_THR_ACCESS(used_allctr);
	    do_erts_alcu_free(type, used_allctr, p, &busy_pcrr_p);
	    clear_busy_pool_carrier(used_allctr, busy_pcrr_p);
	    if (pref_allctr->thread_safe)
		erts_mtx_unlock(&pref_allctr->mutex);
	}
    }
}



/* ------------------------------------------------------------------------- */

static ERTS_INLINE void *
do_erts_alcu_realloc(ErtsAlcType_t type,
		     Allctr_t *allctr,
		     void *p,
		     Uint size,
		     Uint32 alcu_flgs,
		     Carrier_t **busy_pcrr_pp)
{
    Block_t *blk;
    void *res;

    ASSERT(initialized);

    ASSERT(allctr);

    ERTS_LC_ASSERT(!allctr->thread_safe
		       || erts_lc_mtx_is_locked(&allctr->mutex));

    ERTS_ALCU_DBG_CHK_THR_ACCESS(allctr);

    if (!p) {
	res = do_erts_alcu_alloc(type, allctr, size);
	INC_CC(allctr->calls.this_realloc);
	DEC_CC(allctr->calls.this_alloc);
	return res;
    }

    /* Reject sizes that can't fit into the header word. */
    if (size > ~BLK_FLG_MASK) {
        return NULL;
    }

#if ALLOC_ZERO_EQ_NULL
    if (!size) {
	ASSERT(p);
	do_erts_alcu_free(type, allctr, p, busy_pcrr_pp);
	INC_CC(allctr->calls.this_realloc);
	DEC_CC(allctr->calls.this_free);
	return NULL;
    }
#endif

    INC_CC(allctr->calls.this_realloc);
    
    blk = UMEM2BLK(p);

    if (size < allctr->sbc_threshold) {
	if (IS_MBC_BLK(blk))
	    res = mbc_realloc(allctr, type, p, size, alcu_flgs, busy_pcrr_pp);
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
		mbc_free(allctr, type, p, busy_pcrr_pp);
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
    Allctr_t *allctr = (Allctr_t *)extra;
    void *res;

    res = do_erts_alcu_realloc(type, allctr, p, size, 0, NULL);

    DEBUG_CHECK_ALIGNMENT(res);

    if (allctr->atags && res) {
        set_alloc_tag(allctr, res, determine_alloc_tag(allctr, type));
    }

    return res;
}

void *
erts_alcu_realloc_mv(ErtsAlcType_t type, void *extra, void *p, Uint size)
{
    Allctr_t *allctr = (Allctr_t *)extra;
    void *res;

    res = do_erts_alcu_alloc(type, allctr, size);
    if (!res)
        res = do_erts_alcu_realloc(type, allctr, p, size, 0, NULL);
    else {
	Block_t *blk;
	size_t cpy_size;

	blk = UMEM2BLK(p);
	cpy_size = BLK_SZ(blk) - ABLK_HDR_SZ;
	if (cpy_size > size)
	    cpy_size = size;
	sys_memcpy(res, p, cpy_size);
	do_erts_alcu_free(type, allctr, p, NULL);
    }

    DEBUG_CHECK_ALIGNMENT(res);

    if (allctr->atags && res) {
        set_alloc_tag(allctr, res, determine_alloc_tag(allctr, type));
    }

    return res;
}


void *
erts_alcu_realloc_ts(ErtsAlcType_t type, void *extra, void *ptr, Uint size)
{
    Allctr_t *allctr = (Allctr_t *) extra;
    alcu_atag_t tag = 0;
    void *res;

    if (allctr->atags) {
        tag = determine_alloc_tag(allctr, type);
    }

    erts_mtx_lock(&allctr->mutex);

    res = do_erts_alcu_realloc(type, allctr, ptr, size, 0, NULL);

    if (allctr->atags && res) {
        set_alloc_tag(allctr, res, tag);
    }

    erts_mtx_unlock(&allctr->mutex);

    DEBUG_CHECK_ALIGNMENT(res);

    return res;
}

void *
erts_alcu_realloc_mv_ts(ErtsAlcType_t type, void *extra, void *p, Uint size)
{
    Allctr_t *allctr = (Allctr_t *) extra;
    alcu_atag_t tag = 0;
    void *res;

    if (allctr->atags) {
        tag = determine_alloc_tag(allctr, type);
    }

    erts_mtx_lock(&allctr->mutex);
    res = do_erts_alcu_alloc(type, allctr, size);
    if (!res)
	res = do_erts_alcu_realloc(type, allctr, p, size, 0, NULL);
    else {
	Block_t *blk;
	size_t cpy_size;

	blk = UMEM2BLK(p);
	cpy_size = BLK_SZ(blk) - ABLK_HDR_SZ;
	if (cpy_size > size)
	    cpy_size = size;
	sys_memcpy(res, p, cpy_size);
	do_erts_alcu_free(type, allctr, p, NULL);
    }

    if (allctr->atags && res) {
        set_alloc_tag(allctr, res, tag);
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
    int ix;
    alcu_atag_t tag = 0;
    Allctr_t *allctr;
    void *res;

    ix = ERTS_ALC_GET_THR_IX();

    ASSERT(0 <= ix && ix < tspec->size);

    allctr = tspec->allctr[ix];

    if (allctr->atags) {
        tag = determine_alloc_tag(allctr, type);
    }

    if (allctr->thread_safe)
	erts_mtx_lock(&allctr->mutex);

    res = do_erts_alcu_realloc(type, allctr, ptr, size, 0, NULL);

    if (allctr->atags && res) {
        set_alloc_tag(allctr, res, tag);
    }

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
    alcu_atag_t tag = 0;
    Allctr_t *allctr;
    void *res;

    ix = ERTS_ALC_GET_THR_IX();

    ASSERT(0 <= ix && ix < tspec->size);

    allctr = tspec->allctr[ix];

    if (allctr->atags) {
        tag = determine_alloc_tag(allctr, type);
    }

    if (allctr->thread_safe)
	erts_mtx_lock(&allctr->mutex);

    res = do_erts_alcu_alloc(type, allctr, size);
    if (!res) {
        res = do_erts_alcu_realloc(type, allctr, ptr, size, 0, NULL);
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
    }

    if (allctr->atags && res) {
        set_alloc_tag(allctr, res, tag);
    }

    if (allctr->thread_safe)
        erts_mtx_unlock(&allctr->mutex);

    DEBUG_CHECK_ALIGNMENT(res);

    return res;
}

static ERTS_INLINE void *
realloc_thr_pref(ErtsAlcType_t type, Allctr_t *pref_allctr, void *p, Uint size,
		 int force_move)
{
    void *res;
    Allctr_t *used_allctr;
    UWord old_user_size;
    Carrier_t *busy_pcrr_p;
    alcu_atag_t tag = 0;
    int retried;

    if (pref_allctr->atags) {
        tag = determine_alloc_tag(pref_allctr, type);
    }

    if (pref_allctr->thread_safe)
	erts_mtx_lock(&pref_allctr->mutex);

    ASSERT(pref_allctr->dd.use);
    ERTS_ALCU_HANDLE_DD_IN_OP(pref_allctr, 1);
    retried = 0;
restart:

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
	if (!res && !retried && ERTS_ALCU_HANDLE_DD_IN_OP(pref_allctr, 1)) {
	    /* Cleaned up a bit more; try one more time... */
	    retried = 1;
	    goto restart;
	}

        if (pref_allctr->atags && res) {
            set_alloc_tag(pref_allctr, res, tag);
        }

	if (pref_allctr->thread_safe)
	    erts_mtx_unlock(&pref_allctr->mutex);
    }
    else {
	res = do_erts_alcu_alloc(type, pref_allctr, size);
	if (!res)
	    goto unlock_ts_return;
	else {
            if (pref_allctr->atags) {
                set_alloc_tag(pref_allctr, res, tag);
            }

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

    DEBUG_CHECK_ALIGNMENT(res);

    return res;
}

void *
erts_alcu_realloc_thr_pref(ErtsAlcType_t type, void *extra, void *p, Uint size)
{
    if (p) {
        Allctr_t *pref_allctr = get_pref_allctr(extra);

        return realloc_thr_pref(type, pref_allctr, p, size, 0);
    }

    return erts_alcu_alloc_thr_pref(type, extra, size);
}

void *
erts_alcu_realloc_mv_thr_pref(ErtsAlcType_t type, void *extra,
			      void *p, Uint size)
{
    if (p) {
        Allctr_t *pref_allctr = get_pref_allctr(extra);

        return realloc_thr_pref(type, pref_allctr, p, size, 1);
    }

    return erts_alcu_alloc_thr_pref(type, extra, size);
}



static Uint adjust_sbct(Allctr_t* allctr, Uint sbct)
{
#ifndef ARCH_64
    if (sbct > 0) {
	Uint max_mbc_block_sz = UNIT_CEILING(sbct - 1 + ABLK_HDR_SZ);
	if (max_mbc_block_sz + UNIT_FLOOR(allctr->min_block_size - 1) > MBC_ABLK_SZ_MASK
	    || max_mbc_block_sz < sbct) { /* wrap around */
	    /*
	     * By limiting sbc_threshold to (hard limit - min_block_size)
	     * we avoid having to split off free "residue blocks"
	     * smaller than min_block_size.
	     */
	    max_mbc_block_sz = MBC_ABLK_SZ_MASK - UNIT_FLOOR(allctr->min_block_size - 1);
	    sbct = max_mbc_block_sz - ABLK_HDR_SZ + 1;
	}
    }
#endif
    return sbct;
}

int erts_alcu_try_set_dyn_param(Allctr_t* allctr, Eterm param, Uint value)
{
    const Uint MIN_DYN_SBCT = 4000;  /* a lame catastrophe prevention */

    if (param == am_sbct && value >= MIN_DYN_SBCT) {
        allctr->sbc_threshold = adjust_sbct(allctr, value);
        return 1;
    }
    return 0;
}

/* ------------------------------------------------------------------------- */

int
erts_alcu_start(Allctr_t *allctr, AllctrInit_t *init)
{
    /* erts_alcu_start assumes that allctr has been zeroed */
    int i;

    if (((UWord)allctr & ERTS_CRR_ALCTR_FLG_MASK) != 0) {
        erts_exit(ERTS_ABORT_EXIT, "%s:%d:erts_alcu_start: Alignment error\n",
                 __FILE__, __LINE__);
    }

    /* The various fields packed into the header word must not overlap */
    ERTS_CT_ASSERT(!(MBC_ABLK_OFFSET_MASK & MBC_ABLK_SZ_MASK));
    ERTS_CT_ASSERT(!(MBC_ABLK_OFFSET_MASK & BLK_FLG_MASK));
    ERTS_CT_ASSERT(!(MBC_ABLK_SZ_MASK & BLK_FLG_MASK));
    ERTS_CT_ASSERT(!(MBC_FBLK_SZ_MASK & BLK_FLG_MASK));
    ERTS_CT_ASSERT(!(SBC_BLK_SZ_MASK & BLK_FLG_MASK));
    ERTS_CT_ASSERT(!(CRR_SZ_MASK & CRR_FLG_MASK));

    if (!initialized)
	goto error;

#if HAVE_ERTS_MSEG
    sys_memcpy((void *) &allctr->mseg_opt,
	       (void *) &erts_mseg_default_opt,
	       sizeof(ErtsMsegOpt_t));
    if (init->tspec || init->tpref)
	allctr->mseg_opt.sched_spec = 1;
#endif /* HAVE_ERTS_MSEG */

    allctr->name_prefix			= init->name_prefix;
    if (!allctr->name_prefix)
	goto error;

    allctr->ix				= init->ix;
    allctr->alloc_no			= init->alloc_no;
    allctr->alloc_strat			= init->alloc_strat;

    ASSERT(allctr->alloc_no >= ERTS_ALC_A_MIN &&
           allctr->alloc_no <= ERTS_ALC_A_MAX);

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
    allctr->atags                       = init->atags;
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
    if (init->tpref) {
	Uint sz = ABLK_HDR_SZ;
	sz += sizeof(ErtsAllctrDDBlock_t);
	sz = UNIT_CEILING(sz);
	if (sz > allctr->min_block_size)
	    allctr->min_block_size = sz;
    }

    allctr->cpool.pooled_tree = NULL;
    allctr->cpool.dc_list.first = NULL;
    allctr->cpool.dc_list.last = NULL;
    allctr->cpool.abandon_limit = 0;
    allctr->cpool.disable_abandon = 0;
    for (i = ERTS_ALC_A_MIN; i <= ERTS_ALC_A_MAX; i++) {
        erts_atomic_init_nob(&allctr->cpool.stat.blocks_size[i], 0);
        erts_atomic_init_nob(&allctr->cpool.stat.no_blocks[i], 0);
    }
    erts_atomic_init_nob(&allctr->cpool.stat.carriers_size, 0);
    erts_atomic_init_nob(&allctr->cpool.stat.no_carriers, 0);
    if (!init->ts && init->acul && init->acnl) {
        ASSERT(allctr->add_mbc);
        ASSERT(allctr->remove_mbc);
        ASSERT(allctr->largest_fblk_in_mbc);
        ASSERT(allctr->first_fblk_in_mbc);
        ASSERT(allctr->next_fblk_in_mbc);

        allctr->cpool.util_limit = init->acul;
        allctr->cpool.in_pool_limit = init->acnl;
        allctr->cpool.fblk_min_limit = init->acfml;

        if (allctr->alloc_strat == ERTS_ALC_S_FIRSTFIT) {
            allctr->cpool.sentinel = &firstfit_carrier_pool.sentinel;
        }
        else if (allctr->alloc_no != ERTS_ALC_A_TEST) {
            ERTS_INTERNAL_ERROR("Impossible carrier migration config.");
        }
    }
    else {
        allctr->cpool.util_limit = 0;
        allctr->cpool.in_pool_limit = 0;
        allctr->cpool.fblk_min_limit = 0;
    }

    /* The invasive tests don't really care whether the pool is enabled or not,
     * so we need to set this unconditionally for this allocator type. */
    if (allctr->alloc_no == ERTS_ALC_A_TEST) {
        allctr->cpool.sentinel = &test_carrier_pool.sentinel;
    }

    allctr->sbc_threshold = adjust_sbct(allctr, init->sbct);

#if HAVE_ERTS_MSEG
    if (allctr->mseg_opt.abs_shrink_th > ~((UWord) 0) / 100)
	allctr->mseg_opt.abs_shrink_th = ~((UWord) 0) / 100;
#endif

    if (init->ts) {
	allctr->thread_safe = 1;

        erts_mtx_init(&allctr->mutex, "alcu_allocator", make_small(allctr->alloc_no),
            ERTS_LOCK_FLAGS_CATEGORY_ALLOCATOR);

#ifdef DEBUG
	allctr->debug.saved_tid = 0;
#endif
    }

    if(!allctr->get_free_block
       || !allctr->link_free_block
       || !allctr->unlink_free_block
       || !allctr->info_options)
	goto error;

    if (!allctr->get_next_mbc_size)
	allctr->get_next_mbc_size = get_next_mbc_size;

    if (allctr->mbc_header_size < sizeof(Carrier_t))
	goto error;
    allctr->dd.use = 0;
    if (init->tpref) {
	allctr->dd.use = 1;
	init_dd_queue(&allctr->dd.q);
	allctr->dd.ix = init->ix;
    }
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

    allctr->try_set_dyn_param = &erts_alcu_try_set_dyn_param;

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
	  if (allctr->thread_safe)
	    erts_mtx_destroy(&allctr->mutex);
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
	    allctr->fix[i].type = ERTS_ALC_N2T(i + ERTS_ALC_N_MIN_A_FIXED_SIZE);
	    allctr->fix[i].list_size = 0;
	    allctr->fix[i].list = NULL;
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

    if (allctr->thread_safe)
	erts_mtx_destroy(&allctr->mutex);

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

    if (allctr->thread_safe)
	erts_mtx_destroy(&allctr->mutex);

}

/* ------------------------------------------------------------------------- */

void
erts_alcu_init(AlcUInit_t *init)
{
    ErtsAlcCPoolData_t *sentinel;

    sentinel = &firstfit_carrier_pool.sentinel;
    erts_atomic_init_nob(&sentinel->next, (erts_aint_t) sentinel);
    erts_atomic_init_nob(&sentinel->prev, (erts_aint_t) sentinel);

    sentinel = &test_carrier_pool.sentinel;
    erts_atomic_init_nob(&sentinel->next, (erts_aint_t) sentinel);
    erts_atomic_init_nob(&sentinel->prev, (erts_aint_t) sentinel);

    ERTS_CT_ASSERT(SBC_BLK_SZ_MASK == MBC_FBLK_SZ_MASK); /* see BLK_SZ */
#if HAVE_ERTS_MSEG
    ASSERT(erts_mseg_unit_size() == ERTS_SACRR_UNIT_SZ);
    max_mseg_carriers = init->mmc;
    sys_alloc_carrier_size = ERTS_SACRR_UNIT_CEILING(init->ycs);
#else /* #if HAVE_ERTS_MSEG */
    sys_alloc_carrier_size = ((init->ycs + 4095) / 4096) * 4096;
#endif
    allow_sys_alloc_carriers = init->sac;

    sys_page_size = erts_sys_get_page_size();

#ifdef DEBUG
    carrier_alignment = sizeof(Unit_t);
#endif

    erts_mtx_init(&init_atoms_mtx, "alcu_init_atoms", NIL,
        ERTS_LOCK_FLAGS_PROPERTY_STATIC | ERTS_LOCK_FLAGS_CATEGORY_ALLOCATOR);

    atoms_initialized = 0;
    initialized = 1;
}

/* ------------------------------------------------------------------------- */

/* Allocation histograms and carrier information is gathered by walking through
 * all carriers associated with each allocator instance. This is done as
 * aux_yield_work on the scheduler that owns each instance.
 *
 * Yielding is implemented by temporarily inserting a "dummy carrier" at the
 * last position. It's permanently "busy" so it won't get picked up by someone
 * else when in the carrier pool, and we never make the employer aware of it
 * through callbacks so we can't accidentally allocate on it.
 *
 * Plain malloc/free is used to guarantee we won't allocate with the allocator
 * we're scanning. */

/* Yield between carriers once this many blocks have been processed. Note that
 * a single carrier scan may exceed this figure. */
#ifndef DEBUG
    #define BLOCKSCAN_REDUCTIONS (8000)
#else
    #define BLOCKSCAN_REDUCTIONS (400)
#endif

/* Abort a single carrier scan after this many blocks to prevent really large
 * MBCs from blocking forever. */
#define BLOCKSCAN_BAILOUT_THRESHOLD (16000)

typedef struct alcu_blockscan {
    /* A per-scheduler list used when multiple scans have been queued. The
     * current scanner will always run until completion/abort before moving on
     * to the next. */
    struct alcu_blockscan *scanner_queue;

    Allctr_t *allocator;
    Process *process;

    int (*current_op)(struct alcu_blockscan *scanner);
    int (*next_op)(struct alcu_blockscan *scanner);
    int reductions;

    ErtsAlcCPoolData_t *cpool_cursor;
    CarrierList_t *current_clist;
    Carrier_t *clist_cursor;
    Carrier_t dummy_carrier;

    /* Called if the process that started this job dies before we're done. */
    void (*abort)(void *user_data);

    /* Called on each carrier. The callback must return the number of blocks
     * scanned to yield properly between carriers.
     *
     * Note that it's not possible to "yield back" into a carrier. */
    int (*scan)(Allctr_t *, void *user_data, Carrier_t *);

    /* Called when all carriers have been scanned. The callback may return
     * non-zero to yield. */
    int (*finish)(void *user_data);

    void *user_data;
} blockscan_t;

static Carrier_t *blockscan_restore_clist_cursor(blockscan_t *state)
{
    Carrier_t *cursor = state->clist_cursor;

    ASSERT(state->clist_cursor == (state->current_clist)->first ||
           state->clist_cursor == &state->dummy_carrier);

    if (cursor == &state->dummy_carrier) {
        cursor = cursor->next;

        unlink_carrier(state->current_clist, state->clist_cursor);
    }

    return cursor;
}

static void blockscan_save_clist_cursor(blockscan_t *state, Carrier_t *after)
{
    ASSERT(state->clist_cursor == (state->current_clist)->first ||
           state->clist_cursor == &state->dummy_carrier);

    state->clist_cursor = &state->dummy_carrier;

    (state->clist_cursor)->next = after->next;
    (state->clist_cursor)->prev = after;

    relink_carrier(state->current_clist, state->clist_cursor);
}

static int blockscan_clist_yielding(blockscan_t *state)
{
    Carrier_t *cursor = blockscan_restore_clist_cursor(state);

    if (ERTS_PROC_IS_EXITING(state->process)) {
        return 0;
    }

    while (cursor) {
        /* Skip dummy carriers inserted by another (concurrent) block scan.
         * This can happen when scanning thread-safe allocators from multiple
         * schedulers. */
        if (CARRIER_SZ(cursor) > 0) {
            int blocks_scanned = state->scan(state->allocator,
                                             state->user_data,
                                             cursor);

            state->reductions -= blocks_scanned;

            if (state->reductions <= 0) {
                blockscan_save_clist_cursor(state, cursor);
                return 1;
            }
        }

        cursor = cursor->next;
    }

    return 0;
}

static ErtsAlcCPoolData_t *blockscan_restore_cpool_cursor(blockscan_t *state)
{
    ErtsAlcCPoolData_t *cursor;

    cursor = cpool_aint2cpd(cpool_read(&(state->cpool_cursor)->next));

    if (state->cpool_cursor == &state->dummy_carrier.cpool) {
        cpool_delete(state->allocator, state->allocator, &state->dummy_carrier);
    }

    return cursor;
}

static void blockscan_save_cpool_cursor(blockscan_t *state,
                                        ErtsAlcCPoolData_t *after)
{
    ErtsAlcCPoolData_t *dummy_carrier, *prev_carrier, *next_carrier;

    dummy_carrier = &state->dummy_carrier.cpool;

    next_carrier = cpool_aint2cpd(cpool_mod_mark(&after->next));
    prev_carrier = cpool_aint2cpd(cpool_mod_mark(&next_carrier->prev));

    cpool_init(&dummy_carrier->next, (erts_aint_t)next_carrier);
    cpool_init(&dummy_carrier->prev, (erts_aint_t)prev_carrier);

    cpool_set_mod_marked(&prev_carrier->next,
                         (erts_aint_t)dummy_carrier,
                         (erts_aint_t)next_carrier);
    cpool_set_mod_marked(&next_carrier->prev,
                         (erts_aint_t)dummy_carrier,
                         (erts_aint_t)prev_carrier);

    state->cpool_cursor = dummy_carrier;
}

static int blockscan_cpool_yielding(blockscan_t *state)
{
    ErtsAlcCPoolData_t *sentinel, *cursor;

    sentinel = (state->allocator)->cpool.sentinel;
    cursor = blockscan_restore_cpool_cursor(state);

    if (ERTS_PROC_IS_EXITING(state->process)) {
        return 0;
    }

    while (cursor != sentinel) {
        Carrier_t *carrier;
        erts_aint_t exp;

        /* When a deallocation happens on a pooled carrier it will be routed to
         * its owner, so the only way to be sure that it isn't modified while
         * scanning is to skip all carriers that aren't ours. The deallocations
         * deferred to us will get handled when we're done. */
        while (cursor->orig_allctr != state->allocator) {
            cursor = cpool_aint2cpd(cpool_read(&cursor->next));

            if (cursor == sentinel) {
                return 0;
            }
        }

        carrier = ErtsContainerStruct(cursor, Carrier_t, cpool);
        exp = erts_atomic_read_rb(&carrier->allctr);

        if (exp & ERTS_CRR_ALCTR_FLG_IN_POOL) {
            ASSERT(state->allocator == (Allctr_t*)(exp & ~ERTS_CRR_ALCTR_FLG_MASK));
            ASSERT(!(exp & ERTS_CRR_ALCTR_FLG_BUSY));

            if (erts_atomic_cmpxchg_acqb(&carrier->allctr,
                                         exp | ERTS_CRR_ALCTR_FLG_BUSY,
                                         exp) == exp) {
                /* Skip dummy carriers inserted by another (concurrent) block
                 * scan. This can happen when scanning thread-safe allocators
                 * from multiple schedulers. */
                if (CARRIER_SZ(carrier) > 0) {
                    int blocks_scanned = state->scan(state->allocator,
                                                     state->user_data,
                                                     carrier);

                    state->reductions -= blocks_scanned;

                    if (state->reductions <= 0) {
                        blockscan_save_cpool_cursor(state, cursor);
                        erts_atomic_set_relb(&carrier->allctr, exp);

                        return 1;
                    }
                }

                erts_atomic_set_relb(&carrier->allctr, exp);
            }
        }

        cursor = cpool_aint2cpd(cpool_read(&cursor->next));
    }

    return 0;
}

/* */

static int blockscan_finish(blockscan_t *state)
{
    if (ERTS_PROC_IS_EXITING(state->process)) {
        state->abort(state->user_data);
        return 0;
    }

    state->current_op = blockscan_finish;

    return state->finish(state->user_data);
}

static void blockscan_lock_helper(blockscan_t *state) {
    if ((state->allocator)->thread_safe) {
        /* Locked scans have to be as short as possible. */
        state->reductions = 1;

        erts_mtx_lock(&(state->allocator)->mutex);
    } else {
        state->reductions = BLOCKSCAN_REDUCTIONS;
    }
}

static void blockscan_unlock_helper(blockscan_t *state) {
    if ((state->allocator)->thread_safe) {
        erts_mtx_unlock(&(state->allocator)->mutex);
    }
}

static int blockscan_sweep_sbcs(blockscan_t *state)
{
    blockscan_lock_helper(state);

    if (state->current_op != blockscan_sweep_sbcs) {
        SET_CARRIER_HDR(&state->dummy_carrier, 0, SCH_SBC, state->allocator);
        state->current_clist = &(state->allocator)->sbc_list;
        state->clist_cursor = (state->current_clist)->first;
    }

    state->current_op = blockscan_sweep_sbcs;
    state->next_op = blockscan_finish;

    if (blockscan_clist_yielding(state)) {
        state->next_op = state->current_op;
    }
    
    blockscan_unlock_helper(state);

    return 1;
}

static int blockscan_sweep_mbcs(blockscan_t *state)
{
    blockscan_lock_helper(state);

    if (state->current_op != blockscan_sweep_mbcs) {
        SET_CARRIER_HDR(&state->dummy_carrier, 0, SCH_MBC, state->allocator);
        state->current_clist = &(state->allocator)->mbc_list;
        state->clist_cursor = (state->current_clist)->first;
    }

    state->current_op = blockscan_sweep_mbcs;
    state->next_op = blockscan_sweep_sbcs;

    if (blockscan_clist_yielding(state)) {
        state->next_op = state->current_op;
    }

    blockscan_unlock_helper(state);

    return 1;
}

static int blockscan_sweep_cpool(blockscan_t *state)
{
    blockscan_lock_helper(state);

    if (state->current_op != blockscan_sweep_cpool) {
        SET_CARRIER_HDR(&state->dummy_carrier, 0, SCH_MBC, state->allocator);
        state->cpool_cursor = (state->allocator)->cpool.sentinel;
    }

    state->current_op = blockscan_sweep_cpool;
    state->next_op = blockscan_sweep_mbcs;

    if (blockscan_cpool_yielding(state)) {
        state->next_op = state->current_op;
    }

    blockscan_unlock_helper(state);

    return 1;
}

static int blockscan_get_specific_allocator(int allocator_num,
                                            int sched_id,
                                            Allctr_t **out)
{
    ErtsAllocatorInfo_t *ai;
    Allctr_t *allocator;

    ASSERT(allocator_num >= ERTS_ALC_A_MIN &&
           allocator_num <= ERTS_ALC_A_MAX);
    ASSERT(sched_id >= 0 && sched_id <= erts_no_schedulers);

    ai = &erts_allctrs_info[allocator_num];

    if (!ai->enabled || !ai->alloc_util) {
        return 0;
    }

    if (!ai->thr_spec) {
        if (sched_id != 0) {
            /* Only thread-specific allocators can be scanned on a specific
             * scheduler. */
            return 0;
        }

        allocator = (Allctr_t*)ai->extra;
        ASSERT(allocator->thread_safe);
    } else {
        ErtsAllocatorThrSpec_t *tspec = (ErtsAllocatorThrSpec_t*)ai->extra;

        ASSERT(sched_id < tspec->size);

        allocator = tspec->allctr[sched_id];
    }

    *out = allocator;

    return 1;
}

static void blockscan_sched_trampoline(void *arg)
{
    ErtsAlcuBlockscanYieldData *yield;
    ErtsSchedulerData *esdp;
    blockscan_t *scanner;

    esdp = erts_get_scheduler_data();
    scanner = (blockscan_t*)arg;

    yield = ERTS_SCHED_AUX_YIELD_DATA(esdp, alcu_blockscan);

    ASSERT((yield->last == NULL) == (yield->current == NULL));

    if (yield->last != NULL) {
        blockscan_t *prev_scanner = yield->last;

        ASSERT(prev_scanner->scanner_queue == NULL);

        prev_scanner->scanner_queue = scanner;
    } else {
        yield->current = scanner;
    }

    scanner->scanner_queue = NULL;
    yield->last = scanner;

    erts_notify_new_aux_yield_work(esdp);
}

static void blockscan_dispatch(blockscan_t *scanner, Process *owner,
                               Allctr_t *allocator, int sched_id)
{
    ASSERT(erts_get_scheduler_id() != 0);

    if (sched_id == 0) {
        /* Global instances are always handled on the current scheduler. */
        sched_id = ERTS_ALC_GET_THR_IX();
        ASSERT(allocator->thread_safe);
    }

    scanner->allocator = allocator;
    scanner->process = owner;

    erts_proc_inc_refc(scanner->process);

    cpool_init_carrier_data(scanner->allocator, &scanner->dummy_carrier);
    erts_atomic_init_nob(&(scanner->dummy_carrier).allctr,
                         (erts_aint_t)allocator | ERTS_CRR_ALCTR_FLG_BUSY);

    if (ERTS_ALC_IS_CPOOL_ENABLED(scanner->allocator)) {
        scanner->next_op = blockscan_sweep_cpool;
    } else {
        scanner->next_op = blockscan_sweep_mbcs;
    }

    /* Aux yield jobs can only be set up while running on the scheduler that
     * services them, so we move there before continuing.
     *
     * We can't drive the scan itself through this since the scheduler will
     * always finish *all* misc aux work in one go which makes it impossible to
     * yield. */
    erts_schedule_misc_aux_work(sched_id, blockscan_sched_trampoline, scanner);
}

int erts_handle_yielded_alcu_blockscan(ErtsSchedulerData *esdp,
                                       ErtsAlcuBlockscanYieldData *yield)
{
    blockscan_t *scanner = yield->current;

    (void)esdp;

    ASSERT((yield->last == NULL) == (yield->current == NULL));

    if (scanner) {
        if (scanner->next_op(scanner)) {
            return 1;
        }

        ASSERT(ERTS_PROC_IS_EXITING(scanner->process) ||
               scanner->current_op == blockscan_finish);

        yield->current = scanner->scanner_queue;

        if (yield->current == NULL) {
            ASSERT(scanner == yield->last);
            yield->last = NULL;
        }

        erts_proc_dec_refc(scanner->process);

        /* Plain free is intentional. */
        free(scanner);

        return yield->current != NULL;
    }

    return 0;
}

void erts_alcu_sched_spec_data_init(ErtsSchedulerData *esdp)
{
    ErtsAlcuBlockscanYieldData *yield;

    yield = ERTS_SCHED_AUX_YIELD_DATA(esdp, alcu_blockscan);

    yield->current = NULL;
    yield->last = NULL;
}

/* ------------------------------------------------------------------------- */

static ERTS_INLINE int u64_log2(Uint64 v)
{
    static const int log2_tab64[64] = {
        63,  0, 58,  1, 59, 47, 53,  2,
        60, 39, 48, 27, 54, 33, 42,  3,
        61, 51, 37, 40, 49, 18, 28, 20,
        55, 30, 34, 11, 43, 14, 22,  4,
        62, 57, 46, 52, 38, 26, 32, 41,
        50, 36, 17, 19, 29, 10, 13, 21,
        56, 45, 25, 31, 35, 16,  9, 12,
        44, 24, 15,  8, 23,  7,  6,  5};

    v |= v >> 1;
    v |= v >> 2;
    v |= v >> 4;
    v |= v >> 8;
    v |= v >> 16;
    v |= v >> 32;

    return log2_tab64[((Uint64)((v - (v >> 1))*0x07EDD5E59A4E28C2)) >> 58];
}

/* ------------------------------------------------------------------------- */

typedef struct hist_tree__ {
    struct hist_tree__ *parent;
    struct hist_tree__ *left;
    struct hist_tree__ *right;

    int is_red;

    alcu_atag_t tag;
    UWord histogram[1];
} hist_tree_t;

#define ERTS_RBT_PREFIX hist_tree
#define ERTS_RBT_T hist_tree_t
#define ERTS_RBT_KEY_T UWord
#define ERTS_RBT_FLAGS_T int
#define ERTS_RBT_INIT_EMPTY_TNODE(T) ((void)0)
#define ERTS_RBT_IS_RED(T) ((T)->is_red)
#define ERTS_RBT_SET_RED(T) ((T)->is_red = 1)
#define ERTS_RBT_IS_BLACK(T) (!ERTS_RBT_IS_RED(T))
#define ERTS_RBT_SET_BLACK(T) ((T)->is_red = 0)
#define ERTS_RBT_GET_FLAGS(T) ((T)->is_red)
#define ERTS_RBT_SET_FLAGS(T, F) ((T)->is_red = F)
#define ERTS_RBT_GET_PARENT(T) ((T)->parent)
#define ERTS_RBT_SET_PARENT(T, P) ((T)->parent = P)
#define ERTS_RBT_GET_RIGHT(T) ((T)->right)
#define ERTS_RBT_SET_RIGHT(T, R) ((T)->right = (R))
#define ERTS_RBT_GET_LEFT(T) ((T)->left)
#define ERTS_RBT_SET_LEFT(T, L) ((T)->left = (L))
#define ERTS_RBT_GET_KEY(T) ((T)->tag)
#define ERTS_RBT_IS_LT(KX, KY) (KX < KY)
#define ERTS_RBT_IS_EQ(KX, KY) (KX == KY)
#define ERTS_RBT_WANT_FOREACH_DESTROY_YIELDING
#define ERTS_RBT_WANT_FOREACH_DESTROY
#define ERTS_RBT_WANT_INSERT
#define ERTS_RBT_WANT_LOOKUP
#define ERTS_RBT_UNDEF

#include "erl_rbtree.h"

typedef struct {
    blockscan_t common;

    ErtsIRefStorage iref;
    Process *process;

    hist_tree_rbt_yield_state_t hist_tree_yield;
    hist_tree_t *hist_tree;
    UWord hist_count;

    UWord hist_slot_start;
    int hist_slot_count;

    UWord unscanned_size;

    ErtsHeapFactory msg_factory;
    int building_result;
    Eterm result_list;
} gather_ahist_t;

static void gather_ahist_update(gather_ahist_t *state, UWord tag, UWord size)
{
    hist_tree_t *hist_node;
    UWord size_interval;
    int hist_slot;

    hist_node = hist_tree_rbt_lookup(state->hist_tree, tag);

    if (hist_node == NULL) {
        /* Plain calloc is intentional. */
        hist_node = (hist_tree_t*)calloc(1, sizeof(hist_tree_t) +
                                            (state->hist_slot_count - 1) *
                                            sizeof(hist_node->histogram[0]));
        hist_node->tag = tag;

        hist_tree_rbt_insert(&state->hist_tree, hist_node);
        state->hist_count++;
    }

    size_interval = (size / state->hist_slot_start);
    size_interval = u64_log2(size_interval + 1);

    hist_slot = MIN(size_interval, state->hist_slot_count - 1);

    hist_node->histogram[hist_slot]++;
}

static int gather_ahist_scan(Allctr_t *allocator,
                             void *user_data,
                             Carrier_t *carrier)
{
    gather_ahist_t *state;
    int blocks_scanned;
    Block_t *block;

    state = (gather_ahist_t*)user_data;
    blocks_scanned = 1;

    if (IS_SB_CARRIER(carrier)) {
        alcu_atag_t tag;

        block = SBC2BLK(allocator, carrier);

        if (BLK_HAS_ATAG(block)) {
            tag = GET_BLK_ATAG(block);

            ASSERT(DBG_IS_VALID_ATAG(tag));

            gather_ahist_update(state, tag, SBC_BLK_SZ(block));
        }
    } else {
        UWord scanned_bytes = MBC_HEADER_SIZE(allocator);

        ASSERT(IS_MB_CARRIER(carrier));

        block = MBC_TO_FIRST_BLK(allocator, carrier);

        while (1) {
            UWord block_size = MBC_BLK_SZ(block);

            if (IS_ALLOCED_BLK(block) && BLK_HAS_ATAG(block)) {
                alcu_atag_t tag = GET_BLK_ATAG(block);

                ASSERT(DBG_IS_VALID_ATAG(tag));

                gather_ahist_update(state, tag, block_size);
            }

            scanned_bytes += block_size;

            if (blocks_scanned >= BLOCKSCAN_BAILOUT_THRESHOLD) {
                state->unscanned_size += CARRIER_SZ(carrier) - scanned_bytes;
                break;
            } else if (IS_LAST_BLK(block)) {
                break;
            }

            block = NXT_BLK(block);
            blocks_scanned++;
        }
    }

    return blocks_scanned;
}

static int gather_ahist_append_result(hist_tree_t *node, void *arg, Sint reds)
{
    gather_ahist_t *state = (gather_ahist_t*)arg;

    Eterm histogram_tuple, tag_tuple;

    Eterm *hp;
    int ix;

    ASSERT(state->building_result);

    hp = erts_produce_heap(&state->msg_factory, 7 + state->hist_slot_count, 0);

    hp[0] = make_arityval(state->hist_slot_count);

    for (ix = 0; ix < state->hist_slot_count; ix++) {
        hp[1 + ix] = make_small(node->histogram[ix]);
    }

    histogram_tuple = make_tuple(hp);
    hp += 1 + state->hist_slot_count;

    hp[0] = make_arityval(3);
    hp[1] = ATAG_ID(node->tag);
    hp[2] = alloc_type_atoms[ATAG_TYPE(node->tag)];
    hp[3] = histogram_tuple;

    tag_tuple = make_tuple(hp);
    hp += 4;

    state->result_list = CONS(hp, tag_tuple, state->result_list);

    /* Plain free is intentional. */
    free(node);
    return 1;
}

static void gather_ahist_send(gather_ahist_t *state)
{
    Eterm result_tuple, unscanned_size, task_ref;

    Uint term_size;
    Eterm *hp;

    ASSERT((state->result_list == NIL) ^ (state->hist_count > 0));
    ASSERT(state->building_result);

    term_size = 4 + erts_iref_storage_heap_size(&state->iref);
    term_size += IS_USMALL(0, state->unscanned_size) ? 0 : BIG_UINT_HEAP_SIZE;

    hp = erts_produce_heap(&state->msg_factory, term_size, 0);

    task_ref = erts_iref_storage_make_ref(&state->iref, &hp,
        &(state->msg_factory.message)->hfrag.off_heap, 0);

    unscanned_size = bld_unstable_uint(&hp, NULL, state->unscanned_size);

    hp[0] = make_arityval(3);
    hp[1] = task_ref;
    hp[2] = unscanned_size;
    hp[3] = state->result_list;

    result_tuple = make_tuple(hp);

    erts_factory_trim_and_close(&state->msg_factory, &result_tuple, 1);

    erts_queue_message(state->process, 0, state->msg_factory.message,
                       result_tuple, am_system);
}

static int gather_ahist_finish(void *arg)
{
    gather_ahist_t *state = (gather_ahist_t*)arg;

    if (!state->building_result) {
        ErtsMessage *message;
        Uint minimum_size;
        Eterm *hp;

        /* {Ref, unscanned size, [{Tag, {Histogram}} | Rest]} */
        minimum_size = 4 + erts_iref_storage_heap_size(&state->iref) +
                       state->hist_count * (7 + state->hist_slot_count);

        message = erts_alloc_message(minimum_size, &hp);
        erts_factory_selfcontained_message_init(&state->msg_factory,
                                                message, hp);

        ERTS_RBT_YIELD_STAT_INIT(&state->hist_tree_yield);

        state->result_list = NIL;
        state->building_result = 1;
    }

    if (!hist_tree_rbt_foreach_destroy_yielding(&state->hist_tree,
                                                &gather_ahist_append_result,
                                                state,
                                                &state->hist_tree_yield,
                                                BLOCKSCAN_REDUCTIONS)) {
        return 1;
    }

    gather_ahist_send(state);

    return 0;
}

static int gather_ahist_destroy_result(hist_tree_t *node, void *arg, Sint reds)
{
    (void)arg;
    free(node);
    return 1;
}

static void gather_ahist_abort(void *arg)
{
    gather_ahist_t *state = (gather_ahist_t*)arg;

    if (state->building_result) {
        erts_factory_undo(&state->msg_factory);
    }

    hist_tree_rbt_foreach_destroy(&state->hist_tree,
                                  &gather_ahist_destroy_result,
                                  NULL);
}

int erts_alcu_gather_alloc_histograms(Process *p, int allocator_num,
                                      int sched_id, int hist_width,
                                      UWord hist_start, Eterm ref)
{
    gather_ahist_t *gather_state;
    blockscan_t *scanner;
    Allctr_t *allocator;

    ASSERT(is_internal_ref(ref));

    if (!blockscan_get_specific_allocator(allocator_num,
                                          sched_id,
                                          &allocator)) {
        return 0;
    }

    ensure_atoms_initialized(allocator);

    /* Plain calloc is intentional. */
    gather_state = (gather_ahist_t*)calloc(1, sizeof(gather_ahist_t));
    scanner = &gather_state->common;

    scanner->abort = gather_ahist_abort;
    scanner->scan = gather_ahist_scan;
    scanner->finish = gather_ahist_finish;
    scanner->user_data = gather_state;

    erts_iref_storage_save(&gather_state->iref, ref);
    gather_state->hist_slot_start = hist_start;
    gather_state->hist_slot_count = hist_width;
    gather_state->process = p;

    blockscan_dispatch(scanner, p, allocator, sched_id);

    return 1;
}

/* ------------------------------------------------------------------------- */

typedef struct chist_node__ {
    struct chist_node__ *next;

    UWord carrier_size;
    UWord unscanned_size;
    UWord allocated_size;

    /* BLOCKSCAN_BAILOUT_THRESHOLD guarantees we won't overflow this or the
     * counters in the free block histogram. */
    int allocated_count;
    int flags;

    int histogram[1];
} chist_node_t;

typedef struct {
    blockscan_t common;

    ErtsIRefStorage iref;
    Process *process;

    Eterm allocator_desc;

    chist_node_t *info_list;
    UWord info_count;

    UWord hist_slot_start;
    int hist_slot_count;

    ErtsHeapFactory msg_factory;
    int building_result;
    Eterm result_list;
} gather_cinfo_t;

static int gather_cinfo_scan(Allctr_t *allocator,
                             void *user_data,
                             Carrier_t *carrier)
{
    gather_cinfo_t *state;
    chist_node_t *node;
    int blocks_scanned;
    Block_t *block;

    state = (gather_cinfo_t*)user_data;
    node = calloc(1, sizeof(chist_node_t) +
                     (state->hist_slot_count - 1) *
                     sizeof(node->histogram[0]));
    blocks_scanned = 1;

    /* ERTS_CRR_ALCTR_FLG_BUSY is ignored since we've set it ourselves and it
     * would be misleading to include it. */
    node->flags = erts_atomic_read_rb(&carrier->allctr) &
                  (ERTS_CRR_ALCTR_FLG_MASK & ~ERTS_CRR_ALCTR_FLG_BUSY);
    node->carrier_size = CARRIER_SZ(carrier);

    if (IS_SB_CARRIER(carrier)) {
        UWord block_size;

        block = SBC2BLK(allocator, carrier);
        block_size = SBC_BLK_SZ(block);

        node->allocated_size = block_size;
        node->allocated_count = 1;
    } else {
        UWord scanned_bytes = MBC_HEADER_SIZE(allocator);

        block = MBC_TO_FIRST_BLK(allocator, carrier);

        while (1) {
            UWord block_size = MBC_BLK_SZ(block);

            scanned_bytes += block_size;

            if (IS_ALLOCED_BLK(block)) {
                node->allocated_size += block_size;
                node->allocated_count++;
            } else {
                UWord size_interval;
                int hist_slot;

                size_interval = (block_size / state->hist_slot_start);
                size_interval = u64_log2(size_interval + 1);

                hist_slot = MIN(size_interval, state->hist_slot_count - 1);

                node->histogram[hist_slot]++;
            }

            if (blocks_scanned >= BLOCKSCAN_BAILOUT_THRESHOLD) {
                node->unscanned_size += CARRIER_SZ(carrier) - scanned_bytes;
                break;
            } else if (IS_LAST_BLK(block)) {
                break;
            }

            block = NXT_BLK(block);
            blocks_scanned++;
        }
    }

    node->next = state->info_list;
    state->info_list = node;
    state->info_count++;

    return blocks_scanned;
}

static void gather_cinfo_append_result(gather_cinfo_t *state,
                                       chist_node_t *info)
{
    Eterm carrier_size, unscanned_size, allocated_size;
    Eterm histogram_tuple, carrier_tuple;

    Uint term_size;
    Eterm *hp;
    int ix;

    ASSERT(state->building_result);

    term_size = 11 + state->hist_slot_count;
    term_size += IS_USMALL(0, info->carrier_size) ? 0 : BIG_UINT_HEAP_SIZE;
    term_size += IS_USMALL(0, info->unscanned_size) ? 0 : BIG_UINT_HEAP_SIZE;
    term_size += IS_USMALL(0, info->allocated_size) ? 0 : BIG_UINT_HEAP_SIZE;

    hp = erts_produce_heap(&state->msg_factory, term_size, 0);

    hp[0] = make_arityval(state->hist_slot_count);

    for (ix = 0; ix < state->hist_slot_count; ix++) {
        hp[1 + ix] = make_small(info->histogram[ix]);
    }

    histogram_tuple = make_tuple(hp);
    hp += 1 + state->hist_slot_count;

    carrier_size = bld_unstable_uint(&hp, NULL, info->carrier_size);
    unscanned_size = bld_unstable_uint(&hp, NULL, info->unscanned_size);
    allocated_size = bld_unstable_uint(&hp, NULL, info->allocated_size);

    hp[0] = make_arityval(7);
    hp[1] = state->allocator_desc;
    hp[2] = carrier_size;
    hp[3] = unscanned_size;
    hp[4] = allocated_size;
    hp[5] = make_small(info->allocated_count);
    hp[6] = (info->flags & ERTS_CRR_ALCTR_FLG_IN_POOL) ? am_true : am_false;
    hp[7] = histogram_tuple;

    carrier_tuple = make_tuple(hp);
    hp += 8;

    state->result_list = CONS(hp, carrier_tuple, state->result_list);

    free(info);
}

static void gather_cinfo_send(gather_cinfo_t *state)
{
    Eterm result_tuple, task_ref;

    int term_size;
    Eterm *hp;

    ASSERT((state->result_list == NIL) ^ (state->info_count > 0));
    ASSERT(state->building_result);

    term_size = 3 + erts_iref_storage_heap_size(&state->iref);
    hp = erts_produce_heap(&state->msg_factory, term_size, 0);

    task_ref = erts_iref_storage_make_ref(&state->iref, &hp,
        &(state->msg_factory.message)->hfrag.off_heap, 0);

    hp[0] = make_arityval(2);
    hp[1] = task_ref;
    hp[2] = state->result_list;

    result_tuple = make_tuple(hp);

    erts_factory_trim_and_close(&state->msg_factory, &result_tuple, 1);

    erts_queue_message(state->process, 0, state->msg_factory.message,
                       result_tuple, am_system);
}

static int gather_cinfo_finish(void *arg)
{
    gather_cinfo_t *state = (gather_cinfo_t*)arg;
    int reductions = BLOCKSCAN_REDUCTIONS;

    if (!state->building_result) {
        ErtsMessage *message;
        Uint minimum_size;
        Eterm *hp;

        /* {Ref, [{Carrier size, unscanned size, allocated size,
         *         allocated block count, {Free block histogram}} | Rest]} */
        minimum_size = 3 + erts_iref_storage_heap_size(&state->iref) +
                       state->info_count * (11 + state->hist_slot_count);

        message = erts_alloc_message(minimum_size, &hp);
        erts_factory_selfcontained_message_init(&state->msg_factory,
                                                message, hp);

        state->result_list = NIL;
        state->building_result = 1;
    }

    while (state->info_list) {
        chist_node_t *current = state->info_list;
        state->info_list = current->next;

        gather_cinfo_append_result(state, current);

        if (reductions-- <= 0) {
            return 1;
        }
    }

    gather_cinfo_send(state);

    return 0;
}

static void gather_cinfo_abort(void *arg)
{
    gather_cinfo_t *state = (gather_cinfo_t*)arg;

    if (state->building_result) {
        erts_factory_undo(&state->msg_factory);
    }

    while (state->info_list) {
        chist_node_t *current = state->info_list;
        state->info_list = current->next;

        free(current);
    }
}

int erts_alcu_gather_carrier_info(struct process *p, int allocator_num,
                                  int sched_id, int hist_width,
                                  UWord hist_start, Eterm ref)
{
    gather_cinfo_t *gather_state;
    blockscan_t *scanner;

    const char *allocator_desc;
    Allctr_t *allocator;

    ASSERT(is_internal_ref(ref));

    if (!blockscan_get_specific_allocator(allocator_num,
                                          sched_id,
                                          &allocator)) {
        return 0;
    }

    allocator_desc = ERTS_ALC_A2AD(allocator_num);

    /* Plain calloc is intentional. */
    gather_state = (gather_cinfo_t*)calloc(1, sizeof(gather_cinfo_t));
    scanner = &gather_state->common;

    scanner->abort = gather_cinfo_abort;
    scanner->scan = gather_cinfo_scan;
    scanner->finish = gather_cinfo_finish;
    scanner->user_data = gather_state;

    gather_state->allocator_desc = erts_atom_put((byte *)allocator_desc,
                                                 sys_strlen(allocator_desc),
                                                 ERTS_ATOM_ENC_LATIN1, 1);
    erts_iref_storage_save(&gather_state->iref, ref);
    gather_state->hist_slot_start = hist_start * 2;
    gather_state->hist_slot_count = hist_width;
    gather_state->process = p;

    blockscan_dispatch(scanner, p, allocator, sched_id);

    return 1;
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
    erts_mtx_lock(&allctr->mutex);
    erts_alcu_verify_unused(allctr);
    erts_mtx_unlock(&allctr->mutex);
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

#ifdef ERTS_ENABLE_LOCK_COUNT

static void lcnt_enable_allocator_lock_count(Allctr_t *allocator, int enable) {
    if(!allocator->thread_safe) {
        return;
    }

    if(enable) {
        erts_lcnt_install_new_lock_info(&allocator->mutex.lcnt,
            "alcu_allocator", make_small(allocator->alloc_no),
            ERTS_LOCK_TYPE_MUTEX | ERTS_LOCK_FLAGS_CATEGORY_ALLOCATOR);
    } else {
        erts_lcnt_uninstall(&allocator->mutex.lcnt);
    }
}

static void lcnt_update_thread_spec_locks(ErtsAllocatorThrSpec_t *tspec, int enable) {
    if(tspec->enabled) {
        int i;

        for(i = 0; i < tspec->size; i++) {
            lcnt_enable_allocator_lock_count(tspec->allctr[i], enable);
        }
    }
}

void erts_lcnt_update_allocator_locks(int enable) {
    int i;

    for(i = ERTS_ALC_A_MIN; i < ERTS_ALC_A_MAX; i++) {
        ErtsAllocatorInfo_t *ai = &erts_allctrs_info[i];

        if(ai->enabled && ai->alloc_util) {
            if(ai->thr_spec) {
                lcnt_update_thread_spec_locks((ErtsAllocatorThrSpec_t*)ai->extra, enable);
            } else {
                lcnt_enable_allocator_lock_count((Allctr_t*)ai->extra, enable);
            }
        }
    }
}
#endif /* ERTS_ENABLE_LOCK_COUNT */
