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


#ifndef ERL_GOODFIT_ALLOC__
#define ERL_GOODFIT_ALLOC__

#include "erl_alloc_util.h"

#define ERTS_ALC_GF_ALLOC_VSN_STR "2.1"

typedef struct GFAllctr_t_ GFAllctr_t;

typedef struct {
    UWord mbsd;
} GFAllctrInit_t;

#define ERTS_DEFAULT_GF_ALLCTR_INIT {                                      \
    3,			/* (amount) mbsd:   max (mbc) block search depth */\
}

void erts_gfalc_init(void);
Allctr_t *erts_gfalc_start(GFAllctr_t *, GFAllctrInit_t *, AllctrInit_t *);

#endif /* #ifndef ERL_GOODFIT_ALLOC__ */



#if defined(GET_ERL_GF_ALLOC_IMPL) && !defined(ERL_GF_ALLOC_IMPL__)
#define ERL_GF_ALLOC_IMPL__

#define GET_ERL_ALLOC_UTIL_IMPL
#include "erl_alloc_util.h"

#define NO_OF_BKT_IX_BITS (8)
#if defined(ARCH_64)
#  define SUB_MASK_IX_SHIFT (6)
#else
#  define SUB_MASK_IX_SHIFT (5)
#endif
#define NO_OF_BKTS (((UWord) 1) << NO_OF_BKT_IX_BITS)
#define NO_OF_SUB_MASKS (NO_OF_BKTS/(((UWord) 1) << SUB_MASK_IX_SHIFT))

typedef struct {
    UWord main;
    UWord sub[NO_OF_SUB_MASKS];
} BucketMask_t;

typedef struct GFFreeBlock_t_ GFFreeBlock_t;
struct GFFreeBlock_t_ {
    Block_t block_head;
    GFFreeBlock_t *prev;
    GFFreeBlock_t *next;
};

struct GFAllctr_t_ {
    Allctr_t		allctr; /* Has to be first! */

    char *		last_aux_mbc_start;
    char *		last_aux_mbc_end;
    UWord		bkt_max_size_d;
    UWord		bkt_intrvl_d;
    BucketMask_t	bucket_mask;
    GFFreeBlock_t *	buckets[NO_OF_BKTS];
    UWord 		max_blk_search;

};

UWord erts_gfalc_test(UWord, UWord, UWord);

#endif /* #if defined(GET_ERL_GF_ALLOC_IMPL)
	      && !defined(ERL_GF_ALLOC_IMPL__) */
