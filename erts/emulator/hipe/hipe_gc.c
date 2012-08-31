/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2004-2012. All Rights Reserved.
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
/*
 * GC support procedures
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
#include "global.h"

#include "erl_gc.h"

#include "hipe_stack.h"
#include "hipe_gc.h"

Eterm *fullsweep_nstack(Process *p, Eterm *n_htop)
{
    /* known nstack walk state */
    Eterm *nsp;
    Eterm *nsp_end;
    const struct sdesc *sdesc;
    unsigned int sdesc_size;
    unsigned long ra;
    unsigned int i;
    unsigned int mask;
    /* arch-specific nstack walk state */
    struct nstack_walk_state walk_state;

    /* fullsweep-specific state */
    char *src, *oh;
    Uint src_size, oh_size;

    if (!p->hipe.nstack) {
	ASSERT(!p->hipe.nsp && !p->hipe.nstend);
	return n_htop;
    }
    if (!nstack_walk_init_check(p))
	return n_htop;

    ASSERT(p->hipe.nsp && p->hipe.nstend);
    nsp = nstack_walk_nsp_begin(p);
    nsp_end = p->hipe.nstgraylim;
    if (nsp_end)
	nstack_walk_kill_trap(p, nsp_end);
    nsp_end = nstack_walk_nsp_end(p);

    sdesc = nstack_walk_init_sdesc(p, &walk_state);

    src = (char*)HEAP_START(p);
    src_size = (char*)HEAP_TOP(p) - src;
    oh = (char*)OLD_HEAP(p);
    oh_size = (char*)OLD_HTOP(p) - oh;

    for (;;) {
	if (nstack_walk_nsp_reached_end(nsp, nsp_end)) {
	    if (nsp == nsp_end) {
		if (nsp) {
		    /* see the HIGH_WATER update in fullsweep_heap() */
		    p->hipe.nstblacklim = nsp; /* nsp == nsp_end */
		    nstack_walk_update_trap(p, walk_state.sdesc0);
		}
		return n_htop;
	    }
	    fprintf(stderr, "%s: passed end of stack\r\n", __FUNCTION__);
	    break;
	}
	sdesc_size = nstack_walk_frame_size(sdesc);
	i = 0;
	mask = sdesc->livebits[0];
	for (;;) {
	    if (mask & 1) {
		Eterm *nsp_i = nstack_walk_frame_index(nsp, i);
		Eterm gval = *nsp_i;
		if (is_boxed(gval)) {
		    Eterm *ptr = boxed_val(gval);
		    Eterm val = *ptr;
		    if (IS_MOVED_BOXED(val)) {
			ASSERT(is_boxed(val));
			*nsp_i = val;
		    } else if (in_area(ptr, src, src_size) ||
			       in_area(ptr, oh, oh_size)) {
			MOVE_BOXED(ptr, val, n_htop, nsp_i);
		    }
		} else if (is_list(gval)) {
		    Eterm *ptr = list_val(gval);
		    Eterm val = *ptr;
		    if (IS_MOVED_CONS(val)) {
			*nsp_i = ptr[1];
		    } else if (in_area(ptr, src, src_size) ||
			       in_area(ptr, oh, oh_size)) {
			ASSERT(within(ptr, p));
			MOVE_CONS(ptr, val, n_htop, nsp_i);
		    }
		}
	    }
	    if (++i >= sdesc_size)
		break;
	    if (i & 31)
		mask >>= 1;
	    else
		mask = sdesc->livebits[i >> 5];
	}
	ra = nstack_walk_frame_ra(nsp, sdesc);
	sdesc = hipe_find_sdesc(ra);
	nsp = nstack_walk_next_frame(nsp, sdesc_size);
    }
    abort();
}

void gensweep_nstack(Process *p, Eterm **ptr_old_htop, Eterm **ptr_n_htop)
{
    /* known nstack walk state */
    Eterm *nsp;
    Eterm *nsp_end;
    const struct sdesc *sdesc;
    unsigned int sdesc_size;
    unsigned long ra;
    unsigned int i;
    unsigned int mask;
    /* arch-specific nstack walk state */
    struct nstack_walk_state walk_state;

    /* gensweep-specific state */
    Eterm *old_htop, *n_htop;
    char *heap;
    Uint heap_size, mature_size;

    if (!p->hipe.nstack) {
	ASSERT(!p->hipe.nsp && !p->hipe.nstend);
	return;
    }
    if (!nstack_walk_init_check(p))
	return;

    ASSERT(p->hipe.nsp && p->hipe.nstend);
    nsp = nstack_walk_nsp_begin(p);
    nsp_end = p->hipe.nstgraylim;
    if (nsp_end) {
	/* if gray limit passed black limit, reset black limit */
	if (nstack_walk_gray_passed_black(nsp_end, p->hipe.nstblacklim))
	    p->hipe.nstblacklim = nsp_end;
	nstack_walk_kill_trap(p, nsp_end);
	nsp_end = p->hipe.nstblacklim;
    } else
	nsp_end = nstack_walk_nsp_end(p);

    sdesc = nstack_walk_init_sdesc(p, &walk_state);

    old_htop = *ptr_old_htop;
    n_htop = *ptr_n_htop;
    heap = (char*)HEAP_START(p);
    heap_size = (char*)HEAP_TOP(p) - heap;
    mature_size = (char*)HIGH_WATER(p) - heap;

    for (;;) {
	if (nstack_walk_nsp_reached_end(nsp, nsp_end)) {
	    if (nsp == nsp_end) {
		*ptr_old_htop = old_htop;
		*ptr_n_htop = n_htop;
		if (nsp) {
		    /* see the HIGH_WATER update in gen_gc() */
		    if (HEAP_START(p) != HIGH_WATER(p)) {
			p->hipe.nstblacklim =
			    p->hipe.nstgraylim
			    ? p->hipe.nstgraylim
			    : nsp; /* nsp == nsp_end */
		    } else {
			/* blacklim = graylim ? blacklim : end */
			if (!p->hipe.nstgraylim)
			    p->hipe.nstblacklim = nsp; /* nsp == nsp_end */
		    }
		    nstack_walk_update_trap(p, walk_state.sdesc0);
		}
		return;
	    }
	    fprintf(stderr, "%s: passed end of stack\r\n", __FUNCTION__);
	    break;
	}
	sdesc_size = nstack_walk_frame_size(sdesc);
	i = 0;
	mask = sdesc->livebits[0];
	for (;;) {
	    if (mask & 1) {
		Eterm *nsp_i = nstack_walk_frame_index(nsp, i);
		Eterm gval = *nsp_i;
		if (is_boxed(gval)) {
		    Eterm *ptr = boxed_val(gval);
		    Eterm val = *ptr;
		    if (IS_MOVED_BOXED(val)) {
			ASSERT(is_boxed(val));
			*nsp_i = val;
		    } else if (in_area(ptr, heap, mature_size)) {
			MOVE_BOXED(ptr, val, old_htop, nsp_i);
		    } else if (in_area(ptr, heap, heap_size)) {
			ASSERT(within(ptr, p));
			MOVE_BOXED(ptr, val, n_htop, nsp_i);
		    }
		} else if (is_list(gval)) {
		    Eterm *ptr = list_val(gval);
		    Eterm val = *ptr;
		    if (IS_MOVED_CONS(val)) {
			*nsp_i = ptr[1];
		    } else if (in_area(ptr, heap, mature_size)) {
			MOVE_CONS(ptr, val, old_htop, nsp_i);
		    } else if (in_area(ptr, heap, heap_size)) {
			ASSERT(within(ptr, p));
			MOVE_CONS(ptr, val, n_htop, nsp_i);
		    }
		}
	    }
	    if (++i >= sdesc_size)
		break;
	    if (i & 31)
		mask >>= 1;
	    else
		mask = sdesc->livebits[i >> 5];
	}
	ra = nstack_walk_frame_ra(nsp, sdesc);
	sdesc = hipe_find_sdesc(ra);
	nsp = nstack_walk_next_frame(nsp, sdesc_size);
    }
    abort();
}
