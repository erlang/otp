/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2003-2011. All Rights Reserved.
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


#ifndef ERL_AO_FIRSTFIT_ALLOC__
#define ERL_AO_FIRSTFIT_ALLOC__

#include "erl_alloc_util.h"

#define ERTS_ALC_AOFF_ALLOC_VSN_STR "0.9"

typedef struct AOFFAllctr_t_ AOFFAllctr_t;

typedef struct {
    int dummy;
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
    struct AOFF_RBTree_t_* sbmbc_root;
};

unsigned long erts_aoffalc_test(unsigned long, unsigned long, unsigned long);

#endif /* #if defined(GET_ERL_AOFF_ALLOC_IMPL)
	      && !defined(ERL_AOFF_ALLOC_IMPL__) */
