/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2001-2016. All Rights Reserved.
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
#include "global.h"
#include "bif.h"
#include "hipe_stack.h"
#ifdef __x86_64__
#include "hipe_amd64_asm.h"	/* for NR_ARG_REGS */
#else
#include "hipe_x86_asm.h"	/* for NR_ARG_REGS */
#endif

extern void nbif_fail(void);
extern void nbif_stack_trap_ra(void);

/*
 * hipe_print_nstack() is called from hipe_bifs:show_nstack/1.
 */
static void print_slot(Eterm *sp, unsigned int live)
{
    Eterm val = *sp;
    printf(" | 0x%0*lx | 0x%0*lx | ",
	   2*(int)sizeof(long), (unsigned long)sp,
	   2*(int)sizeof(long), val);
    if (live)
	erts_printf("%.30T", val);
    printf("\r\n");
}

void hipe_print_nstack(Process *p)
{
    Eterm *nsp;
    Eterm *nsp_end;
    struct sdesc sdesc0;
    const struct sdesc *sdesc1;
    const struct sdesc *sdesc;
    unsigned long ra;
    unsigned long exnra;
    unsigned int mask;
    unsigned int sdesc_size;
    unsigned int i;
    unsigned int nstkarity;
    static const char dashes[2*sizeof(long)+5] = {
	[0 ... 2*sizeof(long)+3] = '-'
    };

    nsp = p->hipe.nsp;
    nsp_end = p->hipe.nstend;

    nstkarity = p->hipe.narity - NR_ARG_REGS;
    if ((int)nstkarity < 0)
	nstkarity = 0;
    sdesc0.summary = nstkarity;
    sdesc0.livebits[0] = ~1;
    sdesc = &sdesc0;

    printf(" |      NATIVE  STACK      |\r\n");
    printf(" |%s|%s|\r\n", dashes, dashes);
    printf(" | %*s | 0x%0*lx |\r\n",
	   2+2*(int)sizeof(long), "heap",
	   2*(int)sizeof(long), (unsigned long)p->heap);
    printf(" | %*s | 0x%0*lx |\r\n",
	   2+2*(int)sizeof(long), "high_water",
	   2*(int)sizeof(long), (unsigned long)p->high_water);
    printf(" | %*s | 0x%0*lx |\r\n",
	   2+2*(int)sizeof(long), "hend",
	   2*(int)sizeof(long), (unsigned long)p->htop);
    printf(" | %*s | 0x%0*lx |\r\n",
	   2+2*(int)sizeof(long), "old_heap",
	   2*(int)sizeof(long), (unsigned long)p->old_heap);
    printf(" | %*s | 0x%0*lx |\r\n",
	   2+2*(int)sizeof(long), "old_hend",
	   2*(int)sizeof(long), (unsigned long)p->old_hend);
    printf(" | %*s | 0x%0*lx |\r\n",
	   2+2*(int)sizeof(long), "nsp",
	   2*(int)sizeof(long), (unsigned long)p->hipe.nsp);
    printf(" | %*s | 0x%0*lx |\r\n",
	   2+2*(int)sizeof(long), "nstend",
	   2*(int)sizeof(long), (unsigned long)p->hipe.nstend);
    printf(" | %*s| 0x%0*lx |\r\n",
	   2+2*(int)sizeof(long)+1, "nstblacklim",
	   2*(int)sizeof(long), (unsigned long)p->hipe.nstblacklim);
    printf(" | %*s | 0x%0*lx |\r\n",
	   2+2*(int)sizeof(long), "nstgraylim",
	   2*(int)sizeof(long), (unsigned long)p->hipe.nstgraylim);
    printf(" | %*s | 0x%0*x |\r\n",
	   2+2*(int)sizeof(long), "narity",
	   2*(int)sizeof(long), p->hipe.narity);
    printf(" |%s|%s|\r\n", dashes, dashes);
    printf(" | %*s | %*s |\r\n",
	   2+2*(int)sizeof(long), "Address",
	   2+2*(int)sizeof(long), "Contents");

    for (;;) {
	printf(" |%s|%s|\r\n", dashes, dashes);
	if (nsp >= nsp_end) {
	    if (nsp == nsp_end)
		return;
	    fprintf(stderr, "%s: passed end of stack\r\n", __FUNCTION__);
	    break;
	}
	ra = nsp[sdesc_fsize(sdesc)];
	if (ra == (unsigned long)nbif_stack_trap_ra)
	    sdesc1 = hipe_find_sdesc((unsigned long)p->hipe.ngra);
	else
	    sdesc1 = hipe_find_sdesc(ra);
	sdesc_size = sdesc_fsize(sdesc) + 1 + sdesc_arity(sdesc);
	i = 0;
	mask = sdesc->livebits[0];
	for (;;) {
	    if (i == sdesc_fsize(sdesc)) {
		printf(" | 0x%0*lx | 0x%0*lx | ",
		       2*(int)sizeof(long), (unsigned long)&nsp[i],
		       2*(int)sizeof(long), ra);
		if (ra == (unsigned long)nbif_stack_trap_ra)
		    printf("STACK TRAP, ORIG RA 0x%lx", (unsigned long)p->hipe.ngra);
		else
		    printf("NATIVE RA");
		if ((exnra = sdesc_exnra(sdesc1)) != 0)
		    printf(", EXNRA 0x%lx", exnra);
		printf("\r\n");
	    } else
		print_slot(&nsp[i], (mask & 1));
	    if (++i >= sdesc_size)
		break;
	    if (i & 31)
		mask >>= 1;
	    else
		mask = sdesc->livebits[i >> 5];
	}
	nsp += sdesc_size;
	sdesc = sdesc1;
    }
    abort();
}

#define MINSTACK	128
#define NSKIPFRAMES	4

void hipe_update_stack_trap(Process *p, const struct sdesc *sdesc)
{
    Eterm *nsp;
    Eterm *nsp_end;
    unsigned long ra;
    int n;

    nsp = p->hipe.nsp;
    nsp_end = p->hipe.nstend;
    if ((unsigned long)((char*)nsp_end - (char*)nsp) < MINSTACK*sizeof(Eterm*)) {
	p->hipe.nstgraylim = NULL;
	return;
    }
    n = NSKIPFRAMES;
    for (;;) {
	nsp += sdesc_fsize(sdesc);
	if (nsp >= nsp_end) {
	    p->hipe.nstgraylim = NULL;
	    return;
	}
	ra = nsp[0];
	if (--n <= 0)
	    break;
	nsp += 1 + sdesc_arity(sdesc);
	sdesc = hipe_find_sdesc(ra);
    }
    p->hipe.nstgraylim = nsp + 1 + sdesc_arity(sdesc);
    p->hipe.ngra = (void(*)(void))ra;
    nsp[0] = (unsigned long)nbif_stack_trap_ra;
}

/*
 * hipe_handle_stack_trap() is called when the mutator returns to
 * nbif_stack_trap_ra, which marks the gray/white stack boundary frame.
 * The gray/white boundary is moved back one or more frames.
 *
 * The function head below is "interesting".
 */
void (*hipe_handle_stack_trap(Process *p))(void)
{
    void (*ngra)(void) = p->hipe.ngra;
    const struct sdesc *sdesc = hipe_find_sdesc((unsigned long)ngra);
    hipe_update_stack_trap(p, sdesc);
    return ngra;
}

/*
 * hipe_find_handler() is called from hipe_handle_exception() to locate
 * the current exception handler's PC and SP.
 * The native stack MUST contain a stack frame as it appears on
 * entry to a function (return address, actuals, caller's frame).
 * p->hipe.narity MUST contain the arity (number of actuals).
 * On exit, p->hipe.u.ncallee is set to the handler's PC and p->hipe.nsp
 * is set to its SP (low address of its stack frame).
 */
void hipe_find_handler(Process *p)
{
    Eterm *nsp;
    Eterm *nsp_end;
    unsigned long ra;
    unsigned long exnra;
    unsigned int arity;
    const struct sdesc *sdesc;
    unsigned int nstkarity;

    nsp = p->hipe.nsp;
    nsp_end = p->hipe.nstend;
    nstkarity = p->hipe.narity - NR_ARG_REGS;
    if ((int)nstkarity < 0)
	nstkarity = 0;
    arity = nstkarity;

    while (nsp < nsp_end) {
	ra = nsp[0];
	if (ra == (unsigned long)nbif_stack_trap_ra)
	    ra = (unsigned long)p->hipe.ngra;
	sdesc = hipe_find_sdesc(ra);
	/* nsp = nsp + 1 + arity + sdesc_fsize(sdesc); */
	nsp += 1;		/* skip ra */
	nsp += arity;		/* skip actuals */
	if ((exnra = sdesc_exnra(sdesc)) != 0 &&
	    (p->catches >= 0 ||
	     exnra == (unsigned long)nbif_fail)) {
	    p->hipe.u.ncallee = (void(*)(void)) exnra;
	    p->hipe.nsp = nsp;
	    p->hipe.narity = 0;
	    /* update the gray/white boundary if we threw past it */
	    if (p->hipe.nstgraylim && nsp >= p->hipe.nstgraylim)
		hipe_update_stack_trap(p, sdesc);
	    return;
	}
	nsp += sdesc_fsize(sdesc);
	arity = sdesc_arity(sdesc);
    }
    fprintf(stderr, "%s: no native CATCH found!\r\n", __FUNCTION__);
    abort();
}

int hipe_fill_stacktrace(Process *p, int depth, Eterm **trace)
{
    Eterm *nsp;
    Eterm *nsp_end;
    unsigned long ra, prev_ra;
    unsigned int arity;
    const struct sdesc *sdesc;
    unsigned int nstkarity;
    int i;

    if (depth < 1)
	return 0;

    nsp = p->hipe.nsp;
    nsp_end = p->hipe.nstend;
    nstkarity = p->hipe.narity - NR_ARG_REGS;
    if ((int)nstkarity < 0)
	nstkarity = 0;
    arity = nstkarity;

    prev_ra = 0;
    i = 0;
    while (nsp < nsp_end) {	/* INV: i < depth */
	ra = nsp[0];
	if (ra == (unsigned long)nbif_stack_trap_ra)
	    ra = (unsigned long)p->hipe.ngra;
	if (ra != prev_ra) {
	    trace[i] = (Eterm*)ra;
	    ++i;
	    if (i == depth)
		break;
	    prev_ra = ra;
	}
	sdesc = hipe_find_sdesc(ra);
	nsp += 1 + arity + sdesc_fsize(sdesc);
	arity = sdesc_arity(sdesc);
    }
    return i;
}
