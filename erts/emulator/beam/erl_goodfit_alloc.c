/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2003-2016. All Rights Reserved.
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
 * Description:	A "good fit" allocator. Segregated free-lists with a
 *              maximum search depth are used in order to find a good
 *              fit fast. Each free-list contains blocks of sizes in a
 *              specific range. First the free-list
 *              covering the desired size is searched if it is not empty.
 *              This search is stopped when the maximum search depth has
 *              been reached. If no free block was found in the free-list
 *              covering the desired size, the next non-empty free-list
 *              covering larger sizes is searched. The maximum search
 *              depth is by default 3. The insert and delete operations
 *              are O(1) and the search operation is O(n) where n is the
 *              maximum search depth, i.e. by default the all operations
 *              are O(1).
 *
 *              This module is a callback-module for erl_alloc_util.c
 *
 * Author: 	Rickard Green
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif
#include "global.h"
#define GET_ERL_GF_ALLOC_IMPL
#include "erl_goodfit_alloc.h"

#define MIN_MBC_SZ			(16*1024)
#define MIN_MBC_FIRST_FREE_SZ		(4*1024)

#define MAX_SUB_MASK_IX \
  ((((UWord)1) << (NO_OF_BKT_IX_BITS - SUB_MASK_IX_SHIFT)) - 1)
#define MAX_SUB_BKT_IX ((((UWord)1) << SUB_MASK_IX_SHIFT) - 1)
#define MAX_BKT_IX (NO_OF_BKTS - 1)

#define MIN_BLK_SZ  UNIT_CEILING(sizeof(GFFreeBlock_t) + sizeof(UWord))

#define IX2SBIX(IX) ((IX) & (~(~((UWord)0) << SUB_MASK_IX_SHIFT)))
#define IX2SMIX(IX) ((IX) >> SUB_MASK_IX_SHIFT)
#define MAKE_BKT_IX(SMIX, SBIX)	\
  ((((UWord)(SMIX)) << SUB_MASK_IX_SHIFT) | ((UWord)(SBIX)))

#define SET_BKT_MASK_IX(BM, IX)						\
do {									\
    int sub_mask_ix__ = IX2SMIX((IX));					\
    (BM).main |= (((UWord) 1) << sub_mask_ix__);				\
    (BM).sub[sub_mask_ix__] |= (((UWord)1) << IX2SBIX((IX)));		\
} while (0)

#define UNSET_BKT_MASK_IX(BM, IX)					\
do {									\
    int sub_mask_ix__ = IX2SMIX((IX));					\
    (BM).sub[sub_mask_ix__] &= ~(((UWord)1) << IX2SBIX((IX)));		\
    if (!(BM).sub[sub_mask_ix__])					\
	(BM).main &= ~(((UWord)1) << sub_mask_ix__);			\
} while (0)

/* Buckets ... */

#define BKT_INTRVL_A		(1*sizeof(Unit_t))
#define BKT_INTRVL_B		(16*sizeof(Unit_t))
#define BKT_INTRVL_C		(96*sizeof(Unit_t))

#define BKT_MIN_SIZE_A		MIN_BLK_SZ
#define BKT_MIN_SIZE_B		(BKT_MAX_SIZE_A + 1)
#define BKT_MIN_SIZE_C		(BKT_MAX_SIZE_B + 1)
#define BKT_MIN_SIZE_D		(BKT_MAX_SIZE_C + 1)

#define BKT_MAX_SIZE_A		((NO_OF_BKTS/4)*BKT_INTRVL_A+BKT_MIN_SIZE_A-1)
#define BKT_MAX_SIZE_B		((NO_OF_BKTS/4)*BKT_INTRVL_B+BKT_MIN_SIZE_B-1)
#define BKT_MAX_SIZE_C		((NO_OF_BKTS/4)*BKT_INTRVL_C+BKT_MIN_SIZE_C-1)


#define BKT_MAX_IX_A		((NO_OF_BKTS*1)/4 - 1)
#define BKT_MAX_IX_B		((NO_OF_BKTS*2)/4 - 1)
#define BKT_MAX_IX_C		((NO_OF_BKTS*3)/4 - 1)
#define BKT_MAX_IX_D		((NO_OF_BKTS*4)/4 - 1)

#define BKT_MIN_IX_A		(0)
#define BKT_MIN_IX_B		(BKT_MAX_IX_A + 1)
#define BKT_MIN_IX_C		(BKT_MAX_IX_B + 1)
#define BKT_MIN_IX_D		(BKT_MAX_IX_C + 1)


#define BKT_IX_(BAP, SZ)						\
  ((SZ) <= BKT_MAX_SIZE_A						\
   ? (((SZ) - BKT_MIN_SIZE_A)/BKT_INTRVL_A + BKT_MIN_IX_A)		\
   : ((SZ) <= BKT_MAX_SIZE_B						\
      ? (((SZ) - BKT_MIN_SIZE_B)/BKT_INTRVL_B + BKT_MIN_IX_B)		\
      : ((SZ) <= BKT_MAX_SIZE_C						\
	 ? (((SZ) - BKT_MIN_SIZE_C)/BKT_INTRVL_C + BKT_MIN_IX_C)	\
	 : ((SZ) <= (BAP)->bkt_max_size_d				\
	    ? (((SZ) - BKT_MIN_SIZE_D)/(BAP)->bkt_intrvl_d + BKT_MIN_IX_D)\
	    : (NO_OF_BKTS - 1)))))

#define BKT_MIN_SZ_(BAP, IX)						\
  ((IX) <= BKT_MAX_IX_A							\
   ? (((IX) - BKT_MIN_IX_A)*BKT_INTRVL_A + BKT_MIN_SIZE_A)		\
   : ((IX) <= BKT_MAX_IX_B						\
      ? (((IX) - BKT_MIN_IX_B)*BKT_INTRVL_B + BKT_MIN_SIZE_B)		\
      : ((IX) <= BKT_MAX_IX_C						\
	 ? (((IX) - BKT_MIN_IX_C)*BKT_INTRVL_C + BKT_MIN_SIZE_C)	\
	 : (((IX) - BKT_MIN_IX_D)*(BAP)->bkt_intrvl_d + BKT_MIN_SIZE_D))))

#ifdef DEBUG

static int
BKT_IX(GFAllctr_t *gfallctr, Uint size)
{
    int ix;
    ASSERT(size >= MIN_BLK_SZ);

    ix = BKT_IX_(gfallctr, size);

    ASSERT(0 <= ix && ix <= BKT_MAX_IX_D);

    return ix;
}

static Uint
BKT_MIN_SZ(GFAllctr_t *gfallctr, int ix)
{
    Uint size;
    ASSERT(0 <= ix && ix <= BKT_MAX_IX_D);

    size = BKT_MIN_SZ_(gfallctr, ix);

#ifdef ERTS_ALLOC_UTIL_HARD_DEBUG
    ASSERT(ix == BKT_IX(gfallctr, size));
    ASSERT(size == MIN_BLK_SZ || ix - 1 == BKT_IX(gfallctr, size - 1));
#endif

    return size;
}

#else

#define BKT_IX BKT_IX_
#define BKT_MIN_SZ BKT_MIN_SZ_

#endif


/* Prototypes of callback functions */
static Block_t *	get_free_block		(Allctr_t *, Uint,
						 Block_t *, Uint);
static void		link_free_block		(Allctr_t *, Block_t *);
static void		unlink_free_block	(Allctr_t *, Block_t *);
static void		update_last_aux_mbc	(Allctr_t *, Carrier_t *);
static Eterm		info_options		(Allctr_t *, char *, int *,
						 void *, Uint **, Uint *);
static void		init_atoms		(void);

#ifdef ERTS_ALLOC_UTIL_HARD_DEBUG
static void		check_block		(Allctr_t *, Block_t *,  int);
static void		check_mbc		(Allctr_t *, Carrier_t *);
#endif

static int atoms_initialized = 0;

void
erts_gfalc_init(void)
{
    atoms_initialized = 0;
}


Allctr_t *
erts_gfalc_start(GFAllctr_t *gfallctr,
		 GFAllctrInit_t *gfinit,
		 AllctrInit_t *init)
{
    struct {
	int dummy;
	GFAllctr_t allctr;
    } zero = {0};
    /* The struct with a dummy element first is used in order to avoid (an
       incorrect) gcc warning. gcc warns if {0} is used as initializer of
       a struct when the first member is a struct (not if, for example,
       the third member is a struct). */

    Allctr_t *allctr = (Allctr_t *) gfallctr;

    sys_memcpy((void *) gfallctr, (void *) &zero.allctr, sizeof(GFAllctr_t));

    allctr->mbc_header_size		= sizeof(Carrier_t);
    allctr->min_mbc_size		= MIN_MBC_SZ;
    allctr->min_mbc_first_free_size	= MIN_MBC_FIRST_FREE_SZ;
    allctr->min_block_size		= sizeof(GFFreeBlock_t);


    allctr->vsn_str = ERTS_ALC_GF_ALLOC_VSN_STR;

    /* Callback functions */

    allctr->get_free_block		= get_free_block;
    allctr->link_free_block		= link_free_block;
    allctr->unlink_free_block		= unlink_free_block;
    allctr->info_options		= info_options;

    allctr->get_next_mbc_size		= NULL;
    allctr->creating_mbc		= update_last_aux_mbc;
    allctr->destroying_mbc		= update_last_aux_mbc;
    allctr->add_mbc		        = NULL;
    allctr->remove_mbc		        = NULL;
    allctr->largest_fblk_in_mbc         = NULL;
    allctr->init_atoms			= init_atoms;

#ifdef ERTS_ALLOC_UTIL_HARD_DEBUG
    allctr->check_block			= check_block;
    allctr->check_mbc			= check_mbc;
#endif

    allctr->atoms_initialized		= 0;

    if (init->sbct > BKT_MIN_SIZE_D-1)
	gfallctr->bkt_intrvl_d =
	    UNIT_CEILING(((3*(init->sbct - BKT_MIN_SIZE_D - 1)
			   /(NO_OF_BKTS/4 - 1)) + 1)
			 / 2);
    if (gfallctr->bkt_intrvl_d < BKT_INTRVL_C)
	gfallctr->bkt_intrvl_d = BKT_INTRVL_C;
    gfallctr->bkt_max_size_d = ((NO_OF_BKTS/4)*gfallctr->bkt_intrvl_d
				+ BKT_MIN_SIZE_D
				- 1);

    gfallctr->max_blk_search		= (Uint) MAX(1, gfinit->mbsd);

    if (!erts_alcu_start(allctr, init))
	return NULL;

    if (allctr->min_block_size != MIN_BLK_SZ)
	return NULL;

    return allctr;
}

static int
find_bucket(BucketMask_t *bmask, int min_index)
{
    int min, mid, max;
    int sub_mask_ix, sub_bkt_ix;
    int ix = -1;

#undef  GET_MIN_BIT
#define GET_MIN_BIT(MinBit, BitMask, Min, Max)		\
    min = (Min);					\
    max = (Max);					\
    while(max != min) {					\
	mid = ((max - min) >> 1) + min;			\
	if((BitMask)					\
	   & (~(~((UWord) 0) << (mid + 1)))		\
	   & (~((UWord) 0) << min))			\
	    max = mid;					\
	else						\
	    min = mid + 1;				\
    }							\
    (MinBit) = min


    ASSERT(bmask->main < (((UWord) 1) << (MAX_SUB_MASK_IX+1)));

    sub_mask_ix = IX2SMIX(min_index);

    if ((bmask->main & (~((UWord) 0) << sub_mask_ix)) == 0)
	return -1;

    /* There exists a non empty bucket; find it... */

    if (bmask->main & (((UWord) 1) << sub_mask_ix)) {
	sub_bkt_ix = IX2SBIX(min_index);
	if ((bmask->sub[sub_mask_ix] & (~((UWord) 0) << sub_bkt_ix)) == 0) {
	    sub_mask_ix++;
	    sub_bkt_ix = 0;
	    if ((bmask->main & (~((UWord) 0)<< sub_mask_ix)) == 0)
		return -1;
	}
	else
	    goto find_sub_bkt_ix;
    }
    else {
	sub_mask_ix++;
	sub_bkt_ix = 0;
    }

    ASSERT(sub_mask_ix <= MAX_SUB_MASK_IX);
    /* Has to be a bit > sub_mask_ix */
    ASSERT(bmask->main & (~((UWord) 0) << (sub_mask_ix)));
    GET_MIN_BIT(sub_mask_ix, bmask->main, sub_mask_ix, MAX_SUB_MASK_IX);

 find_sub_bkt_ix:
    ASSERT(sub_mask_ix <= MAX_SUB_MASK_IX);
    ASSERT(sub_bkt_ix <= MAX_SUB_BKT_IX);

    if ((bmask->sub[sub_mask_ix] & (((UWord) 1) << sub_bkt_ix)) == 0) {
	ASSERT(sub_mask_ix + 1 <= MAX_SUB_BKT_IX);
	/* Has to be a bit > sub_bkt_ix */
	ASSERT(bmask->sub[sub_mask_ix] & (~((UWord) 0) << sub_bkt_ix));

	GET_MIN_BIT(sub_bkt_ix,
		    bmask->sub[sub_mask_ix],
		    sub_bkt_ix+1,
		    MAX_SUB_BKT_IX);

	ASSERT(sub_bkt_ix <= MAX_SUB_BKT_IX);
    }

    ix = MAKE_BKT_IX(sub_mask_ix, sub_bkt_ix);

    ASSERT(0 <= ix && ix < NO_OF_BKTS); 

    return ix;

#undef  GET_MIN_BIT

}

static Block_t *
search_bucket(Allctr_t *allctr, int ix, Uint size)
{
    int i;
    Uint min_sz;
    Uint blk_sz;
    Uint cand_sz = 0;
    UWord max_blk_search;
    GFFreeBlock_t *blk;
    GFFreeBlock_t *cand = NULL;
    int blk_on_lambc;
    int cand_on_lambc = 0;
    GFAllctr_t *gfallctr = (GFAllctr_t *) allctr;

    ASSERT(0 <= ix && ix <= NO_OF_BKTS - 1);

    if (!gfallctr->buckets[ix])
	return NULL;

    min_sz = BKT_MIN_SZ(gfallctr, ix);
    if (min_sz < size)
	min_sz = size;

    max_blk_search = gfallctr->max_blk_search;
    for (blk = gfallctr->buckets[ix], i = 0;
	 blk && i < max_blk_search;
	 blk = blk->next, i++) {

	blk_sz = MBC_FBLK_SZ(&blk->block_head);
	blk_on_lambc = (((char *) blk) < gfallctr->last_aux_mbc_end
			&& gfallctr->last_aux_mbc_start <= ((char *) blk));

	if (blk_sz == min_sz && !blk_on_lambc)
	    return (Block_t *) blk;

	if (blk_sz >= min_sz
	    && (!cand
		|| (!blk_on_lambc && (cand_on_lambc || blk_sz < cand_sz))
		|| (blk_on_lambc && cand_on_lambc && blk_sz < cand_sz))) {
	    cand_sz = blk_sz;
	    cand = blk;
	    cand_on_lambc = blk_on_lambc;
	}

    }
    return (Block_t *) cand;
}

static Block_t *
get_free_block(Allctr_t *allctr, Uint size,
	       Block_t *cand_blk, Uint cand_size)
{
    GFAllctr_t *gfallctr = (GFAllctr_t *) allctr;
    int unsafe_bi, min_bi;
    Block_t *blk;

    ASSERT(!cand_blk || cand_size >= size);

    unsafe_bi = BKT_IX(gfallctr, size);
    
    min_bi = find_bucket(&gfallctr->bucket_mask, unsafe_bi);
    if (min_bi < 0)
	return NULL;

    if (min_bi == unsafe_bi) {
	blk = search_bucket(allctr, min_bi, size);
	if (blk) {
	    if (cand_blk && cand_size <= MBC_FBLK_SZ(blk))
		return NULL; /* cand_blk was better */
	    unlink_free_block(allctr, blk);
	    return blk;
	}
	if (min_bi < NO_OF_BKTS - 1) {
	    min_bi = find_bucket(&gfallctr->bucket_mask, min_bi + 1);
	    if (min_bi < 0)
		return NULL;
	}
	else
	    return NULL;
    }
    else {
	ASSERT(min_bi > unsafe_bi);
    }

    /* We are guaranteed to find a block that fits in this bucket */
    blk = search_bucket(allctr, min_bi, size);
    ASSERT(blk);
    if (cand_blk && cand_size <= MBC_FBLK_SZ(blk))
	return NULL; /* cand_blk was better */
    unlink_free_block(allctr, blk);
    return blk;
}



static void
link_free_block(Allctr_t *allctr, Block_t *block)
{
    GFAllctr_t *gfallctr = (GFAllctr_t *) allctr;
    GFFreeBlock_t *blk = (GFFreeBlock_t *) block;
    Uint sz = MBC_FBLK_SZ(&blk->block_head);
    int i = BKT_IX(gfallctr, sz);

    ASSERT(sz >= MIN_BLK_SZ);

    SET_BKT_MASK_IX(gfallctr->bucket_mask, i);

    blk->prev = NULL;
    blk->next = gfallctr->buckets[i];
    if (blk->next) {
	ASSERT(!blk->next->prev);
	blk->next->prev = blk;
    }
    gfallctr->buckets[i] = blk;
}

static void
unlink_free_block(Allctr_t *allctr, Block_t *block)
{
    GFAllctr_t *gfallctr = (GFAllctr_t *) allctr;
    GFFreeBlock_t *blk = (GFFreeBlock_t *) block;
    Uint sz = MBC_FBLK_SZ(&blk->block_head);
    int i = BKT_IX(gfallctr, sz);

    if (!blk->prev) {
	ASSERT(gfallctr->buckets[i] == blk);
	gfallctr->buckets[i] = blk->next;
    }
    else
	blk->prev->next = blk->next;
    if (blk->next)
	blk->next->prev = blk->prev;

    if (!gfallctr->buckets[i])
	UNSET_BKT_MASK_IX(gfallctr->bucket_mask, i);
}

static void
update_last_aux_mbc(Allctr_t *allctr, Carrier_t *mbc)
{
    GFAllctr_t *gfallctr = (GFAllctr_t *) allctr;

    if (gfallctr->last_aux_mbc_start != (char *) allctr->mbc_list.last) {

	if (allctr->mbc_list.last
	    && allctr->main_carrier != allctr->mbc_list.last) {
	    gfallctr->last_aux_mbc_start = (char *) allctr->mbc_list.last;
	    gfallctr->last_aux_mbc_end = (((char *) allctr->mbc_list.last)
					  + CARRIER_SZ(allctr->mbc_list.last));
	}
	else {
	    gfallctr->last_aux_mbc_start = NULL;
	    gfallctr->last_aux_mbc_end = NULL;
	}

    }
}

static struct {
    Eterm mbsd;
    Eterm as;
    Eterm gf;
#ifdef DEBUG
    Eterm end_of_atoms;
#endif
} am;

static void ERTS_INLINE atom_init(Eterm *atom, char *name)
{
    *atom = am_atom_put(name, strlen(name));
}
#define AM_INIT(AM) atom_init(&am.AM, #AM)

static void
init_atoms(void)
{
#ifdef DEBUG
    Eterm *atom;
#endif

    if (atoms_initialized)
	return;

#ifdef DEBUG
    for (atom = (Eterm *) &am; atom <= &am.end_of_atoms; atom++) {
	*atom = THE_NON_VALUE;
    }
#endif

    AM_INIT(mbsd);
    AM_INIT(as);
    AM_INIT(gf);

#ifdef DEBUG
    for (atom = (Eterm *) &am; atom < &am.end_of_atoms; atom++) {
	ASSERT(*atom != THE_NON_VALUE);
    }
#endif

    atoms_initialized = 1;
}

#define bld_uint	erts_bld_uint
#define bld_cons	erts_bld_cons
#define bld_tuple	erts_bld_tuple

static ERTS_INLINE void
add_2tup(Uint **hpp, Uint *szp, Eterm *lp, Eterm el1, Eterm el2)
{
    *lp = bld_cons(hpp, szp, bld_tuple(hpp, szp, 2, el1, el2), *lp);
}

static Eterm
info_options(Allctr_t *allctr,
	     char *prefix,
	     int *print_to_p,
	     void *print_to_arg,
	     Uint **hpp,
	     Uint *szp)
{
    GFAllctr_t *gfallctr = (GFAllctr_t *) allctr;
    Eterm res = THE_NON_VALUE;

    if (print_to_p) {
	erts_print(*print_to_p,
		   print_to_arg,
		   "%smbsd: %lu\n"
		   "%sas: gf\n",
		   prefix, gfallctr->max_blk_search,
		   prefix);
    }

    if (hpp || szp) {
	
	if (!atoms_initialized)
	    erts_exit(ERTS_ERROR_EXIT, "%s:%d: Internal error: Atoms not initialized",
		     __FILE__, __LINE__);

	res = NIL;
	add_2tup(hpp, szp, &res, am.as, am.gf);
	add_2tup(hpp, szp, &res,
		 am.mbsd,
		 bld_uint(hpp, szp, gfallctr->max_blk_search));
    }

    return res;
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * NOTE:  erts_gfalc_test() is only supposed to be used for testing.         *
 *                                                                           *
 * Keep alloc_SUITE_data/allocator_test.h updated if changes are made        *
 * to erts_gfalc_test()                                                      *
\*                                                                           */

UWord
erts_gfalc_test(UWord op, UWord a1, UWord a2)
{
    switch (op) {
    case 0x100:	return (UWord) BKT_IX((GFAllctr_t *) a1, (Uint) a2);
    case 0x101:	return (UWord) BKT_MIN_SZ((GFAllctr_t *) a1, (int) a2);
    case 0x102:	return (UWord) NO_OF_BKTS;
    case 0x103:	return (UWord)
		    find_bucket(&((GFAllctr_t *) a1)->bucket_mask, (int) a2);
    default:	ASSERT(0); return ~((UWord) 0);
    }
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * Debug functions                                                           *
\*                                                                           */

#ifdef ERTS_ALLOC_UTIL_HARD_DEBUG
void
check_block(Allctr_t *allctr, Block_t * blk, int free_block)
{
    GFAllctr_t *gfallctr = (GFAllctr_t *) allctr;
    int i;
    int bi;
    int found;
    GFFreeBlock_t *fblk;

    if(free_block) {
	Uint blk_sz = is_sbc_blk(blk) ? SBC_BLK_SZ(blk) : MBC_BLK_SZ(blk);
	bi = BKT_IX(gfallctr, blk_sz);

	ASSERT(gfallctr->bucket_mask.main & (((UWord) 1) << IX2SMIX(bi)));
	ASSERT(gfallctr->bucket_mask.sub[IX2SMIX(bi)]
	       & (((UWord) 1) << IX2SBIX(bi)));
		
	found = 0;
	for (fblk = gfallctr->buckets[bi]; fblk; fblk = fblk->next)
	    if (blk == (Block_t *) fblk)
		found++;
	ASSERT(found == 1);
    }
    else
	bi = -1;

    found = 0;
    for (i = 0; i < NO_OF_BKTS; i++) {
	if (i == bi)
	    continue; /* Already checked */
	for (fblk = gfallctr->buckets[i]; fblk; fblk = fblk->next)
	    if (blk == (Block_t *) fblk)
		found++;
    }

    ASSERT(found == 0);

}

void
check_mbc(Allctr_t *allctr, Carrier_t *mbc)
{
    GFAllctr_t *gfallctr = (GFAllctr_t *) allctr;
    int bi;

    for(bi = 0; bi < NO_OF_BKTS; bi++) {
	if ((gfallctr->bucket_mask.main & (((UWord) 1) << IX2SMIX(bi)))
	    && (gfallctr->bucket_mask.sub[IX2SMIX(bi)]
		& (((UWord) 1) << IX2SBIX(bi)))) {
	    ASSERT(gfallctr->buckets[bi] != NULL);
	}
	else {
	    ASSERT(gfallctr->buckets[bi] == NULL);
	}
    }
}

#endif
