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
 * Description:	A fast allocator intended for temporary allocation.
 *              When allocating, only the first block in the free list
 *              is inspected, if this block doesn't fit a new carrier
 *              is created. NOTE: this allocator can behave really bad
 *              if misused.
 *              
 *              This module is a callback-module for erl_alloc_util.c
 *
 * Author: 	Rickard Green
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif
#include "global.h"
#define GET_ERL_AF_ALLOC_IMPL
#include "erl_afit_alloc.h"

struct AFFreeBlock_t_ {
    Block_t block_head;
    AFFreeBlock_t *prev;
    AFFreeBlock_t *next;
};
#define AF_BLK_SZ(B) MBC_FBLK_SZ(&(B)->block_head)

#define MIN_MBC_SZ		(16*1024)
#define MIN_MBC_FIRST_FREE_SZ	(4*1024)

/* Prototypes of callback functions */
static Block_t *	get_free_block		(Allctr_t *, Uint, Block_t *, Uint);
static void		link_free_block		(Allctr_t *, Block_t *);
static void		unlink_free_block	(Allctr_t *, Block_t *);


static Eterm		info_options		(Allctr_t *, char *, fmtfn_t *,
						 void *arg, Uint **, Uint *);
static void		init_atoms		(void);

static int atoms_initialized = 0;

void
erts_afalc_init(void)
{
    atoms_initialized = 0;
}

Allctr_t *
erts_afalc_start(AFAllctr_t *afallctr,
		 AFAllctrInit_t *afinit,
		 AllctrInit_t *init)
{
    struct {
	int dummy;
	AFAllctr_t allctr;
    } zero = {0};
    /* The struct with a dummy element first is used in order to avoid (an
       incorrect) gcc warning. gcc warns if {0} is used as initializer of
       a struct when the first member is a struct (not if, for example,
       the third member is a struct). */

    Allctr_t *allctr = (Allctr_t *) afallctr;

    sys_memcpy((void *) afallctr, (void *) &zero.allctr, sizeof(AFAllctr_t));

    allctr->mbc_header_size		= sizeof(Carrier_t);
    allctr->min_mbc_size		= MIN_MBC_SZ;
    allctr->min_mbc_first_free_size	= MIN_MBC_FIRST_FREE_SZ;
    allctr->min_block_size		= sizeof(AFFreeBlock_t);
    allctr->vsn_str			= ERTS_ALC_AF_ALLOC_VSN_STR;

    /* Callback functions */
    allctr->get_free_block		= get_free_block;
    allctr->link_free_block		= link_free_block;
    allctr->unlink_free_block		= unlink_free_block;
    allctr->info_options		= info_options;

    allctr->get_next_mbc_size		= NULL;
    allctr->creating_mbc		= NULL;
    allctr->destroying_mbc		= NULL;
    allctr->add_mbc                     = NULL;
    allctr->remove_mbc                  = NULL;
    allctr->largest_fblk_in_mbc         = NULL;
    allctr->init_atoms			= init_atoms;

#ifdef ERTS_ALLOC_UTIL_HARD_DEBUG
    allctr->check_block			= NULL;
    allctr->check_mbc			= NULL;
#endif

    allctr->atoms_initialized		= 0;

    if (!erts_alcu_start(allctr, init))
	return NULL;

    return allctr;
}

static Block_t *
get_free_block(Allctr_t *allctr, Uint size, Block_t *cand_blk, Uint cand_size)
{
    AFAllctr_t *afallctr = (AFAllctr_t *) allctr;

    ASSERT(!cand_blk || cand_size >= size);

    if (afallctr->free_list && AF_BLK_SZ(afallctr->free_list) >= size) {
	AFFreeBlock_t *res = afallctr->free_list;	
	afallctr->free_list = res->next;
	if (res->next)
	    res->next->prev = NULL;
	return (Block_t *) res;
    }
    else
	return NULL;
}

static void
link_free_block(Allctr_t *allctr, Block_t *block)
{
    AFFreeBlock_t *blk = (AFFreeBlock_t *) block;
    AFAllctr_t *afallctr = (AFAllctr_t *) allctr;

    if (afallctr->free_list && AF_BLK_SZ(afallctr->free_list) > AF_BLK_SZ(blk)) {
	blk->next = afallctr->free_list->next;
	blk->prev = afallctr->free_list;
	afallctr->free_list->next = blk;
    }
    else {
	blk->next = afallctr->free_list;
	blk->prev = NULL;
	afallctr->free_list = blk;
    }

    if (blk->next)
	blk->next->prev = blk;
}

static void
unlink_free_block(Allctr_t *allctr, Block_t *block)
{
    AFFreeBlock_t *blk = (AFFreeBlock_t *) block;
    AFAllctr_t *afallctr = (AFAllctr_t *) allctr;

    if (blk->prev)
	blk->prev->next = blk->next;
    else
	afallctr->free_list = blk->next;
    if (blk->next)
	blk->next->prev = blk->prev;
}


static struct {
    Eterm as;
    Eterm af;
#ifdef DEBUG
    Eterm end_of_atoms;
#endif
} am;

static void ERTS_INLINE atom_init(Eterm *atom, char *name)
{
    *atom = am_atom_put(name, sys_strlen(name));
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

    AM_INIT(as);
    AM_INIT(af);

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
	     fmtfn_t *print_to_p,
	     void *print_to_arg,
	     Uint **hpp,
	     Uint *szp)
{
    Eterm res = THE_NON_VALUE;

    if (print_to_p) {
	erts_print(*print_to_p, print_to_arg, "%sas: af\n", prefix);
    }

    if (hpp || szp) {
	
	if (!atoms_initialized)
	    erts_exit(ERTS_ERROR_EXIT, "%s:%d: Internal error: Atoms not initialized",
		     __FILE__, __LINE__);;

	res = NIL;
	add_2tup(hpp, szp, &res, am.as, am.af);
    }

    return res;
}



/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * NOTE:  erts_afalc_test() is only supposed to be used for testing.         *
 *                                                                           *
 * Keep alloc_SUITE_data/allocator_test.h updated if changes are made        *
 * to erts_afalc_test()                                                      *
\*                                                                           */

UWord
erts_afalc_test(UWord op, UWord a1, UWord a2)
{
    switch (op) {
    default:	ASSERT(0); return ~((UWord) 0);
    }
}
