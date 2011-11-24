/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2004-2011. All Rights Reserved.
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
 * Stack walking helpers for native stack GC procedures.
 */
#ifndef HIPE_X86_GC_H
#define HIPE_X86_GC_H

#include "hipe_x86_asm.h"	/* for NR_ARG_REGS */

/* uncomment to simulate & test what the initial PowerPC port will do */
//#define SKIP_YOUNGEST_FRAME

struct nstack_walk_state {
#ifdef SKIP_YOUNGEST_FRAME
    const struct sdesc *sdesc0;	/* .sdesc0 must be a pointer rvalue */
#else
    struct sdesc sdesc0[1];	/* .sdesc0 must be a pointer rvalue */
#endif
};

static inline int nstack_walk_init_check(const Process *p)
{
#ifdef SKIP_YOUNGEST_FRAME
    if (!p->hipe.nsp || p->hipe.nsp == p->hipe.nstend)
	return 0;
#endif
    return 1;
}

static inline Eterm *nstack_walk_nsp_begin(const Process *p)
{
#ifdef SKIP_YOUNGEST_FRAME
    unsigned int nstkarity = p->hipe.narity - NR_ARG_REGS;
    if ((int)nstkarity < 0)
	nstkarity = 0;
    return p->hipe.nsp + 1 + nstkarity;
#else
    return p->hipe.nsp;
#endif
}

static inline const struct sdesc*
nstack_walk_init_sdesc(const Process *p, struct nstack_walk_state *state)
{
#ifdef SKIP_YOUNGEST_FRAME
    const struct sdesc *sdesc = hipe_find_sdesc(p->hipe.nsp[0]);
    state->sdesc0 = sdesc;
    return sdesc;
#else
    unsigned int nstkarity = p->hipe.narity - NR_ARG_REGS;
    if ((int)nstkarity < 0)
	nstkarity = 0;
    state->sdesc0[0].summary = (0 << 9) | (0 << 8) | nstkarity;
    state->sdesc0[0].livebits[0] = 0;
    /* XXX: this appears to prevent a gcc-4.1.1 bug on x86 */
    __asm__ __volatile__("" : : "m"(*state) : "memory");
    return &state->sdesc0[0];
#endif
}

static inline void nstack_walk_update_trap(Process *p, const struct sdesc *sdesc0)
{
#ifdef SKIP_YOUNGEST_FRAME
    Eterm *nsp = p->hipe.nsp;
    p->hipe.nsp = nstack_walk_nsp_begin(p);
    hipe_update_stack_trap(p, sdesc0);
    p->hipe.nsp = nsp;
#else
    hipe_update_stack_trap(p, sdesc0);
#endif
}

static inline Eterm *nstack_walk_nsp_end(const Process *p)
{
    return p->hipe.nstend;
}

static inline void nstack_walk_kill_trap(Process *p, Eterm *nsp_end)
{
    /* remove gray/white boundary trap */
    for (;;) {
	--nsp_end;
	if (nsp_end[0] == (unsigned long)nbif_stack_trap_ra) {
	    nsp_end[0] = (unsigned long)p->hipe.ngra;
	    break;
	}
    }
}

static inline int nstack_walk_gray_passed_black(const Eterm *gray, const Eterm *black)
{
    return gray > black;
}

static inline int nstack_walk_nsp_reached_end(const Eterm *nsp, const Eterm *nsp_end)
{
    return nsp >= nsp_end;
}

static inline unsigned int nstack_walk_frame_size(const struct sdesc *sdesc)
{
    return sdesc_fsize(sdesc) + 1 + sdesc_arity(sdesc);
}

static inline Eterm *nstack_walk_frame_index(Eterm *nsp, unsigned int i)
{
    return &nsp[i];
}

static inline unsigned long
nstack_walk_frame_ra(const Eterm *nsp, const struct sdesc *sdesc)
{
    return nsp[sdesc_fsize(sdesc)];
}

static inline Eterm *nstack_walk_next_frame(Eterm *nsp, unsigned int sdesc_size)
{
    return nsp + sdesc_size;
}

#endif /* HIPE_X86_GC_H */
