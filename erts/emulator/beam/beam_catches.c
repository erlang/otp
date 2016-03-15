/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2000-2016. All Rights Reserved.
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

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
#include "sys.h"
#include "beam_catches.h"
#include "global.h"

/* R14B04 has about 380 catches when starting erlang */
#define DEFAULT_TABSIZE (1024)
typedef struct {
    BeamInstr *cp;
    unsigned cdr;
} beam_catch_t;

#ifdef DEBUG
#  define IF_DEBUG(x) x
#else
#  define IF_DEBUG(x)
#endif

struct bc_pool {
    int free_list;
    unsigned high_mark;
    unsigned tabsize;
    beam_catch_t *beam_catches;
    /* 
     * Note that the 'beam_catches' area is shared by pools. Used slots
     * are readonly as long as the module is not purgable. The free-list is
     * protected by the code_ix lock.
     */

    IF_DEBUG(int is_staging;)
};

static struct bc_pool bccix[ERTS_NUM_CODE_IX];

void beam_catches_init(void)
{
    int i;

    bccix[0].tabsize   = DEFAULT_TABSIZE;
    bccix[0].free_list = -1;
    bccix[0].high_mark = 0;
    bccix[0].beam_catches = erts_alloc(ERTS_ALC_T_CODE,
				     sizeof(beam_catch_t)*DEFAULT_TABSIZE);
    IF_DEBUG(bccix[0].is_staging = 0);
    for (i=1; i<ERTS_NUM_CODE_IX; i++) {
	bccix[i] = bccix[i-1];
    }
     /* For initial load: */
    IF_DEBUG(bccix[erts_staging_code_ix()].is_staging = 1);
}


static void gc_old_vec(beam_catch_t* vec)
{
    int i;
    for (i=0; i<ERTS_NUM_CODE_IX; i++) {
	if (bccix[i].beam_catches == vec) {
	    return;
	}
    }
    erts_free(ERTS_ALC_T_CODE, vec);
}


void beam_catches_start_staging(void)
{
    ErtsCodeIndex dst = erts_staging_code_ix();
    ErtsCodeIndex src = erts_active_code_ix();
    beam_catch_t* prev_vec = bccix[dst].beam_catches;

    ASSERT(!bccix[src].is_staging && !bccix[dst].is_staging);

    bccix[dst] = bccix[src];
    gc_old_vec(prev_vec);
    IF_DEBUG(bccix[dst].is_staging = 1);
}

void beam_catches_end_staging(int commit)
{
    IF_DEBUG(bccix[erts_staging_code_ix()].is_staging = 0);
}

unsigned beam_catches_cons(BeamInstr *cp, unsigned cdr)
{
    int i;
    struct bc_pool* p = &bccix[erts_staging_code_ix()];

    ASSERT(p->is_staging);
    /*
     * Allocate from free_list while it is non-empty.
     * If free_list is empty, allocate at high_mark.
     */
    if (p->free_list >= 0) {
	i = p->free_list;
	p->free_list = p->beam_catches[i].cdr;
    }
    else {
	if (p->high_mark >= p->tabsize) {
	    /* No free slots and table is full: realloc table */
	    beam_catch_t* prev_vec = p->beam_catches;
	    unsigned newsize = p->tabsize*2;

	    p->beam_catches = erts_alloc(ERTS_ALC_T_CODE,
					 newsize*sizeof(beam_catch_t));
	    sys_memcpy(p->beam_catches, prev_vec,
		       p->tabsize*sizeof(beam_catch_t));
	    gc_old_vec(prev_vec);
	    p->tabsize = newsize;
	}
	i = p->high_mark++;
    }

    p->beam_catches[i].cp  = cp;
    p->beam_catches[i].cdr = cdr;

    return i;
}

BeamInstr *beam_catches_car(unsigned i)
{
    struct bc_pool* p = &bccix[erts_active_code_ix()];

    if (i >= p->tabsize ) {
	erts_exit(ERTS_ERROR_EXIT, "beam_catches_delmod: index %#x is out of range\r\n", i);
    }
    return p->beam_catches[i].cp;
}

void beam_catches_delmod(unsigned head, BeamInstr *code, unsigned code_bytes,
			 ErtsCodeIndex code_ix)
{
    struct bc_pool* p = &bccix[code_ix];
    unsigned i, cdr;

    ASSERT((code_ix == erts_active_code_ix()) != bccix[erts_staging_code_ix()].is_staging);
    for(i = head; i != (unsigned)-1;) {
	if (i >= p->tabsize) {
	    erts_exit(ERTS_ERROR_EXIT, "beam_catches_delmod: index %#x is out of range\r\n", i);
	}
	if( (char*)p->beam_catches[i].cp - (char*)code >= code_bytes ) {
	    erts_exit(ERTS_ERROR_EXIT,
		    "beam_catches_delmod: item %#x has cp %p which is not "
		    "in module's range [%p,%p[\r\n",
		    i, p->beam_catches[i].cp, code, ((char*)code + code_bytes));
	}
	p->beam_catches[i].cp = 0;
	cdr = p->beam_catches[i].cdr;
	p->beam_catches[i].cdr = p->free_list;
	p->free_list = i;
	i = cdr;
    }
}
