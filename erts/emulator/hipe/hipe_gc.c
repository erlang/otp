/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2004-2016. All Rights Reserved.
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
 * GC support procedures
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#define ERL_WANT_GC_INTERNALS__

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

    ASSERT(!p->hipe.gc_is_unsafe);

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
		    } else if (!erts_is_literal(gval, ptr)) {
			MOVE_BOXED(ptr, val, n_htop, nsp_i);
		    }
		} else if (is_list(gval)) {
		    Eterm *ptr = list_val(gval);
		    Eterm val = *ptr;
		    if (IS_MOVED_CONS(val)) {
			*nsp_i = ptr[1];
		    } else if (!erts_is_literal(gval, ptr)) {
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
    char *oh;
    Uint oh_size;

    /* gensweep-specific state */
    Eterm *old_htop, *n_htop;
    char *mature;
    Uint mature_size;

    ASSERT(!p->hipe.gc_is_unsafe);

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
    mature = (char *) (p->abandoned_heap ? p->abandoned_heap : p->heap);
    mature_size = (char*)HIGH_WATER(p) - mature;
    oh = (char*)OLD_HEAP(p);
    oh_size = (char*)OLD_HTOP(p) - oh;

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
		    } else if (ErtsInArea(ptr, mature, mature_size)) {
			MOVE_BOXED(ptr, val, old_htop, nsp_i);
		    } else if (ErtsInYoungGen(gval, ptr, oh, oh_size)) {
			ASSERT(within(ptr, p));
			MOVE_BOXED(ptr, val, n_htop, nsp_i);
		    }
		} else if (is_list(gval)) {
		    Eterm *ptr = list_val(gval);
		    Eterm val = *ptr;
		    if (IS_MOVED_CONS(val)) {
			*nsp_i = ptr[1];
		    } else if (ErtsInArea(ptr, mature, mature_size)) {
			MOVE_CONS(ptr, val, old_htop, nsp_i);
		    } else if (ErtsInYoungGen(gval, ptr, oh, oh_size)) {
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

Eterm *sweep_literals_nstack(Process *p, Eterm *old_htop, char *area,
			     Uint area_size)
{
    /* known nstack walk state */
    Eterm *nsp;
    Eterm *nsp_end;
    const struct sdesc *sdesc;
    /* arch-specific nstack walk state */
    struct nstack_walk_state walk_state;

    ASSERT(!p->hipe.gc_is_unsafe);

    if (!p->hipe.nstack) {
	ASSERT(!p->hipe.nsp && !p->hipe.nstend);
	return old_htop;
    }
    if (!nstack_walk_init_check(p))
	return old_htop;

    ASSERT(p->hipe.nsp && p->hipe.nstend);
    nsp = nstack_walk_nsp_begin(p);
    nsp_end = nstack_walk_nsp_end(p);
    sdesc = nstack_walk_init_sdesc_ignore_trap(p, &walk_state);

    while (!nstack_walk_nsp_reached_end(nsp, nsp_end)) {
	unsigned long ra;
	unsigned sdesc_size = nstack_walk_frame_size(sdesc);
	unsigned i = 0;
	unsigned mask = sdesc->livebits[0];
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
		    } else if (ErtsInArea(ptr, area, area_size)) {
			MOVE_BOXED(ptr, val, old_htop, nsp_i);
		    }
		} else if (is_list(gval)) {
		    Eterm *ptr = list_val(gval);
		    Eterm val = *ptr;
		    if (IS_MOVED_CONS(val)) {
			*nsp_i = ptr[1];
		    } else if (ErtsInArea(ptr, area, area_size)) {
			MOVE_CONS(ptr, val, old_htop, nsp_i);
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
	if (ra == (unsigned long)nbif_stack_trap_ra)
	    ra = (unsigned long)p->hipe.ngra;
	sdesc = hipe_find_sdesc(ra);
	nsp = nstack_walk_next_frame(nsp, sdesc_size);
    }
    return old_htop;
}

int
nstack_any_heap_ref_ptrs(Process *rp, char* mod_start, Uint mod_size)
{
    Eterm *nsp;
    Eterm *nsp_end;
    const struct sdesc *sdesc;
    /* arch-specific nstack walk state */
    struct nstack_walk_state walk_state;

    ASSERT(!rp->hipe.gc_is_unsafe);

    if (!rp->hipe.nstack || !nstack_walk_init_check(rp)) return 0;
    ASSERT(rp->hipe.nsp && rp->hipe.nstend);
    nsp = nstack_walk_nsp_begin(rp);
    nsp_end = nstack_walk_nsp_end(rp);
    sdesc = nstack_walk_init_sdesc_ignore_trap(rp, &walk_state);

    while (!nstack_walk_nsp_reached_end(nsp, nsp_end)) {
	unsigned long ra;
	unsigned sdesc_size = nstack_walk_frame_size(sdesc);
	unsigned i = 0;
	unsigned mask = sdesc->livebits[0];
	for (;;) {
	    if (mask & 1) {
		Eterm *nsp_i = nstack_walk_frame_index(nsp, i);
		Eterm val = *nsp_i;
		switch (primary_tag(val)) {
		case TAG_PRIMARY_BOXED:
		case TAG_PRIMARY_LIST:
		    if (ErtsInArea(val, mod_start, mod_size)) {
			return 1;
		    }
		    break;
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
	if (ra == (unsigned long)nbif_stack_trap_ra)
	    ra = (unsigned long)rp->hipe.ngra;
	sdesc = hipe_find_sdesc(ra);
	nsp = nstack_walk_next_frame(nsp, sdesc_size);
    }
    return 0;
}
