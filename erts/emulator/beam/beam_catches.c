/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2000-2011. All Rights Reserved.
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

static int free_list;
static unsigned high_mark;
static unsigned tabsize;
static beam_catch_t *beam_catches;

void beam_catches_init(void)
{
    tabsize   = DEFAULT_TABSIZE;
    free_list = -1;
    high_mark = 0;

    beam_catches = erts_alloc(ERTS_ALC_T_CODE, sizeof(beam_catch_t)*DEFAULT_TABSIZE);
}

unsigned beam_catches_cons(BeamInstr *cp, unsigned cdr)
{
    int i;

    /*
     * Allocate from free_list while it is non-empty.
     * If free_list is empty, allocate at high_mark.
     *
     * This avoids the need to initialise the free list in
     * beam_catches_init(), which would cost O(TABSIZ) time.
     */
    if( free_list >= 0 ) {
	i = free_list;
	free_list = beam_catches[i].cdr;
    } else if( high_mark < tabsize ) {
	i = high_mark;
	high_mark++;
    } else {
	/* No free slots and table is full: realloc table */
	tabsize      = 2*tabsize;
	beam_catches = erts_realloc(ERTS_ALC_T_CODE, beam_catches, sizeof(beam_catch_t)*tabsize);
	i = high_mark;
	high_mark++;
    }

    beam_catches[i].cp  = cp;
    beam_catches[i].cdr = cdr;

    return i;
}

BeamInstr *beam_catches_car(unsigned i)
{
    if( i >= tabsize ) {
	erl_exit(1, "beam_catches_delmod: index %#x is out of range\r\n", i);
    }
    return beam_catches[i].cp;
}

void beam_catches_delmod(unsigned head, BeamInstr *code, unsigned code_bytes)
{
    unsigned i, cdr;

    for(i = head; i != (unsigned)-1;) {
	if( i >= tabsize ) {
	    erl_exit(1, "beam_catches_delmod: index %#x is out of range\r\n", i);
	}
	if( (char*)beam_catches[i].cp - (char*)code >= code_bytes ) {
	    erl_exit(1,
		    "beam_catches_delmod: item %#x has cp %#lx which is not "
		    "in module's range [%#lx,%#lx[\r\n",
		    i, (long)beam_catches[i].cp,
		    (long)code, (long)((char*)code + code_bytes));
	}
	beam_catches[i].cp = 0;
	cdr = beam_catches[i].cdr;
	beam_catches[i].cdr = free_list;
	free_list = i;
	i = cdr;
    }
}
