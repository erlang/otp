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
 * Stack walking helpers for native stack GC procedures.
 */
#ifndef HIPE_X86_GC_H
#define HIPE_X86_GC_H

#include "hipe_x86_asm.h"	/* for NR_ARG_REGS */

/* uncomment to simulate & test what the initial PowerPC port will do */
/* #define SKIP_YOUNGEST_FRAME */

struct nstack_walk_state {
#ifdef SKIP_YOUNGEST_FRAME
    const struct hipe_sdesc *sdesc0;	/* .sdesc0 must be a pointer rvalue */
#else
    struct hipe_sdesc sdesc0[1];	/* .sdesc0 must be a pointer rvalue */
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

static inline const struct hipe_sdesc*
nstack_walk_init_sdesc(const Process *p, struct nstack_walk_state *state)
{
#ifdef SKIP_YOUNGEST_FRAME
    const struct hipe_sdesc *sdesc = hipe_find_sdesc(p->hipe.nsp[0]);
    state->sdesc0 = sdesc;
    return sdesc;
#else
    state->sdesc0[0].bucket.hvalue = 0; /* for nstack_any_cps_in_segment */
    state->sdesc0[0].fsize = 0;
    state->sdesc0[0].has_exnra = 0;
    state->sdesc0[0].stk_nargs = (p->hipe.narity < NR_ARG_REGS ? 0 :
                                  p->hipe.narity - NR_ARG_REGS);
    state->sdesc0[0].livebits[0] = 0;
    state->sdesc0[0].m_aix = 0;
    state->sdesc0[0].f_aix = atom_val(am_undefined);
    state->sdesc0[0].a     = 0;
    /* XXX: this appears to prevent a gcc-4.1.1 bug on x86 */
    __asm__ __volatile__("" : : "m"(*state) : "memory");
    return &state->sdesc0[0];
#endif
}

static inline const struct hipe_sdesc*
nstack_walk_init_sdesc_ignore_trap(const Process *p,
				   struct nstack_walk_state *state)
{
#ifdef SKIP_YOUNGEST_FRAME
    unsigned long ra = p->hipe.nsp[0];
    const struct hipe_sdesc *sdesc;
    if (ra == (unsigned long)nbif_stack_trap_ra)
	ra = (unsigned long)p->hipe.ngra;
    sdesc = hipe_find_sdesc(ra);
    state->sdesc0 = sdesc;
    return sdesc;
#else
    return nstack_walk_init_sdesc(p, state);
#endif
}

static inline void nstack_walk_update_trap(Process *p, const struct hipe_sdesc *sdesc0)
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

static inline unsigned int nstack_walk_frame_size(const struct hipe_sdesc *sdesc)
{
    return sdesc_fsize(sdesc) + 1 + sdesc_arity(sdesc);
}

static inline Eterm *nstack_walk_frame_index(Eterm *nsp, unsigned int i)
{
    return &nsp[i];
}

static inline unsigned long
nstack_walk_frame_ra(const Eterm *nsp, const struct hipe_sdesc *sdesc)
{
    return nsp[sdesc_fsize(sdesc)];
}

static inline Eterm *nstack_walk_next_frame(Eterm *nsp, unsigned int sdesc_size)
{
    return nsp + sdesc_size;
}

#endif /* HIPE_X86_GC_H */
