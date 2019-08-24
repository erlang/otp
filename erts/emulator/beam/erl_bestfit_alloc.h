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


#ifndef ERL_BESTFIT_ALLOC__
#define ERL_BESTFIT_ALLOC__

#include "erl_alloc_util.h"

#define ERTS_ALC_BF_ALLOC_VSN_STR "0.9"
#define ERTS_ALC_AOBF_ALLOC_VSN_STR "0.9"

typedef struct BFAllctr_t_ BFAllctr_t;

typedef struct {
    int ao;
} BFAllctrInit_t;

#define ERTS_DEFAULT_BF_ALLCTR_INIT {                                      \
    0					/* (bool) ao:   address order    */\
}

void erts_bfalc_init(void);
Allctr_t *erts_bfalc_start(BFAllctr_t *, BFAllctrInit_t *, AllctrInit_t *);

#endif /* #ifndef ERL_BESTFIT_ALLOC__ */



#if defined(GET_ERL_BF_ALLOC_IMPL) && !defined(ERL_BF_ALLOC_IMPL__)
#define ERL_BF_ALLOC_IMPL__

#define GET_ERL_ALLOC_UTIL_IMPL
#include "erl_alloc_util.h"

typedef struct RBTree_t_ RBTree_t;

struct BFAllctr_t_ {
    Allctr_t		allctr; /* Has to be first! */

    RBTree_t *		mbc_root;
    int 		address_order;
};

UWord erts_bfalc_test(UWord, UWord, UWord);

#endif /* #if defined(GET_ERL_BF_ALLOC_IMPL)
	      && !defined(ERL_BF_ALLOC_IMPL__) */
