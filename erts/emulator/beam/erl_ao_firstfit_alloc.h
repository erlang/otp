/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2003-2018. All Rights Reserved.
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


#ifndef ERL_AO_FIRSTFIT_ALLOC__
#define ERL_AO_FIRSTFIT_ALLOC__

#include "erl_alloc_util.h"

#define ERTS_ALC_AOFF_ALLOC_VSN_STR "0.9"

typedef struct AOFFAllctr_t_ AOFFAllctr_t;

enum AOFFSortOrder {
    FF_AGEFF = 0,    /* carrier trees only */
    FF_AOFF  = 1,
    FF_AOBF  = 2,    /* block trees only */
    FF_BF    = 3,    /* block trees only */

    FF_CHAOS = -1    /* A test-specific sort order that picks any of the above
                      * after instance id. Used to test that carriers created
                      * under one order will work fine after being migrated
                      * to another. */
};

typedef struct {
    enum AOFFSortOrder blk_order;
    enum AOFFSortOrder crr_order;
} AOFFAllctrInit_t;

#define ERTS_DEFAULT_AOFF_ALLCTR_INIT {0/*dummy*/}

void erts_aoffalc_init(void);
Allctr_t *erts_aoffalc_start(AOFFAllctr_t *, AOFFAllctrInit_t*, AllctrInit_t *);

#endif /* #ifndef ERL_AO_FIRSTFIT_ALLOC__ */



#if defined(GET_ERL_AOFF_ALLOC_IMPL) && !defined(ERL_AOFF_ALLOC_IMPL__)
#define ERL_AOFF_ALLOC_IMPL__

#define GET_ERL_ALLOC_UTIL_IMPL
#include "erl_alloc_util.h"

struct AOFFAllctr_t_ {
    Allctr_t		allctr; /* Has to be first! */

    struct AOFF_RBTree_t_* mbc_root;
    enum AOFFSortOrder blk_order;
    enum AOFFSortOrder crr_order;
};

UWord erts_aoffalc_test(UWord, UWord, UWord);

#endif /* #if defined(GET_ERL_AOFF_ALLOC_IMPL)
	      && !defined(ERL_AOFF_ALLOC_IMPL__) */
