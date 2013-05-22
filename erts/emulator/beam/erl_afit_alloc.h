/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2003-2013. All Rights Reserved.
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


#ifndef ERL_AFIT_ALLOC__
#define ERL_AFIT_ALLOC__

#include "erl_alloc_util.h"

#define ERTS_ALC_AF_ALLOC_VSN_STR "0.9"

typedef struct AFAllctr_t_ AFAllctr_t;

typedef struct {
    int dummy;
} AFAllctrInit_t;

#define ERTS_DEFAULT_AF_ALLCTR_INIT {                                      \
    0					/* dummy                         */\
}

void erts_afalc_init(void);
Allctr_t *erts_afalc_start(AFAllctr_t *, AFAllctrInit_t *, AllctrInit_t *);

#endif /* #ifndef ERL_BESTFIT_ALLOC__ */



#if defined(GET_ERL_AF_ALLOC_IMPL) && !defined(ERL_AF_ALLOC_IMPL__)
#define ERL_AF_ALLOC_IMPL__

#define GET_ERL_ALLOC_UTIL_IMPL
#include "erl_alloc_util.h"

typedef struct AFFreeBlock_t_ AFFreeBlock_t;

struct AFAllctr_t_ {
    Allctr_t		allctr; /* Has to be first! */

    AFFreeBlock_t *	free_list;
};

UWord erts_afalc_test(UWord, UWord, UWord);

#endif /* #if defined(GET_ERL_AF_ALLOC_IMPL)
	      && !defined(ERL_AF_ALLOC_IMPL__) */
