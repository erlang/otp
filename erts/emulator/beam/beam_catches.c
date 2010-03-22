/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2000-2010. All Rights Reserved.
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

/* XXX: should use dynamic reallocation */
#define TABSIZ (16*1024)
static struct {
    BeamInstr *cp;
    unsigned cdr;
} beam_catches[TABSIZ];

static int free_list;
static unsigned high_mark;

void beam_catches_init(void)
{
    free_list = -1;
    high_mark = 0;
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
    if( (i = free_list) >= 0 ) {
	free_list = beam_catches[i].cdr;
    } else if( (i = high_mark) < TABSIZ ) {
	high_mark = i + 1;
    } else {
	fprintf(stderr, "beam_catches_cons: no free slots :-(\r\n");
	exit(1);
    }

    beam_catches[i].cp = cp;
    beam_catches[i].cdr = cdr;

    return i;
}

BeamInstr *beam_catches_car(unsigned i)
{
    if( i >= TABSIZ ) {
	fprintf(stderr,
		"beam_catches_car: index %#x is out of range\r\n", i);
	abort();
    }
    return beam_catches[i].cp;
}

void beam_catches_delmod(unsigned head, BeamInstr *code, unsigned code_bytes)
{
    unsigned i, cdr;

    for(i = head; i != (unsigned)-1;) {
	if( i >= TABSIZ ) {
	    fprintf(stderr,
		    "beam_catches_delmod: index %#x is out of range\r\n", i);
	    abort();
	}
	if( (char*)beam_catches[i].cp - (char*)code >= code_bytes ) {
	    fprintf(stderr,
		    "beam_catches_delmod: item %#x has cp %#lx which is not "
		    "in module's range [%#lx,%#lx[\r\n",
		    i, (long)beam_catches[i].cp,
		    (long)code, (long)((char*)code + code_bytes));
	    abort();
	}
	beam_catches[i].cp = 0;
	cdr = beam_catches[i].cdr;
	beam_catches[i].cdr = free_list;
	free_list = i;
	i = cdr;
    }
}
