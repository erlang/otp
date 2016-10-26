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


#ifndef HIPE_X86_GLUE_H
#define HIPE_X86_GLUE_H

#include "hipe_x86_asm.h"	/* for NR_ARG_REGS and LEAF_WORDS */

/* Emulated code recursively calls native code.
   The return address is `nbif_return', which is exported so that
   tailcalls from native to emulated code can be identified. */
extern unsigned int x86_call_to_native(Process*);
extern void nbif_return(void);

/* Native-mode stubs for calling emulated-mode closures. */
extern void nbif_ccallemu0(void);
extern void nbif_ccallemu1(void);
extern void nbif_ccallemu2(void);
extern void nbif_ccallemu3(void);
extern void nbif_ccallemu4(void);
extern void nbif_ccallemu5(void);
extern void nbif_ccallemu6(void);

/* Default exception handler for native code. */
extern void nbif_fail(void);

/* Emulated code returns to its native code caller. */
extern unsigned int x86_return_to_native(Process*);

/* Emulated code tailcalls native code. */
extern unsigned int x86_tailcall_to_native(Process*);

/* Emulated code throws an exception to its native code caller. */
extern unsigned int x86_throw_to_native(Process*);

static __inline__ unsigned int max(unsigned int x, unsigned int y)
{
    return (x > y) ? x : y;
}

static __inline__ void hipe_arch_glue_init(void)
{
    static struct hipe_sdesc_with_exnra nbif_return_sdesc;

    nbif_return_sdesc.exnra = (unsigned long)nbif_fail;
    nbif_return_sdesc.sdesc.bucket.hvalue = (unsigned long)nbif_return;
    nbif_return_sdesc.sdesc.fsize = 0;
    nbif_return_sdesc.sdesc.has_exnra = 1;
    nbif_return_sdesc.sdesc.stk_nargs = 0;
    nbif_return_sdesc.sdesc.m_aix = atom_val(am_Empty);
    nbif_return_sdesc.sdesc.f_aix = atom_val(am_return);
    nbif_return_sdesc.sdesc.a     = 0;

    hipe_init_sdesc_table(&nbif_return_sdesc.sdesc);
}

/* PRE: arity <= NR_ARG_REGS */
static __inline__ void
hipe_write_x86_regs(Process *p, unsigned int arity, Eterm reg[])
{
#if NR_ARG_REGS > 0
    int i;
    for (i = arity; --i >= 0;)
	p->def_arg_reg[i] = reg[i];
#endif
}

/* PRE: arity <= NR_ARG_REGS */
static __inline__ void
hipe_read_x86_regs(Process *p, unsigned int arity, Eterm reg[])
{
#if NR_ARG_REGS > 0
    int i;
    for (i = arity; --i >= 0;)
	reg[i] = p->def_arg_reg[i];
#endif
}

static __inline__ void
hipe_push_x86_params(Process *p, unsigned int arity, Eterm reg[])
{
    unsigned int i;

    i = arity;
    if (i > NR_ARG_REGS) {
	Eterm *nsp = p->hipe.nsp;
	i = NR_ARG_REGS;
	do {
	    *--nsp = reg[i++];
	} while (i < arity);
	p->hipe.nsp = nsp;
	i = NR_ARG_REGS;
    }
    /* INV: i <= NR_ARG_REGS */
    hipe_write_x86_regs(p, i, reg);
}

static __inline__ void
hipe_pop_x86_params(Process *p, unsigned int arity, Eterm reg[])
{
    unsigned int i;

    i = arity;
    if (i > NR_ARG_REGS) {
	Eterm *nsp = p->hipe.nsp;
	do {
	    reg[--i] = *nsp++;
	} while (i > NR_ARG_REGS);
	p->hipe.nsp = nsp;
	/* INV: i == NR_ARG_REGS */
    }
    /* INV: i <= NR_ARG_REGS */
    hipe_read_x86_regs(p, i, reg);
}

/* BEAM recursively calls native code. */
static __inline__ unsigned int
hipe_call_to_native(Process *p, unsigned int arity, Eterm reg[])
{
    int nstkargs;

    /* Note that call_to_native() needs two words on the stack:
       one for the nbif_return return address, and one for the
       callee's return address should it need to call inc_stack_0. */
    if ((nstkargs = arity - NR_ARG_REGS) < 0)
	nstkargs = 0;
    hipe_check_nstack(p, max(nstkargs+1+1, LEAF_WORDS));
    hipe_push_x86_params(p, arity, reg);	/* needs nstkargs words */
    return x86_call_to_native(p);		/* needs 1+1 words */
}

/* Native called BEAM, which now tailcalls native. */
static __inline__ unsigned int
hipe_tailcall_to_native(Process *p, unsigned int arity, Eterm reg[])
{
    int nstkargs;

    if ((nstkargs = arity - NR_ARG_REGS) < 0)
	nstkargs = 0;
    /* +1 so callee can call inc_stack_0 */
    hipe_check_nstack(p, max(nstkargs+1, LEAF_WORDS));
    if (nstkargs) {
	Eterm nra;
	nra = *(p->hipe.nsp++);
	hipe_push_x86_params(p, arity, reg);
	*--(p->hipe.nsp) = nra;
    } else
	hipe_write_x86_regs(p, arity, reg);
    return x86_tailcall_to_native(p);
}

/* BEAM called native, which has returned. Clean up. */
static __inline__ void hipe_return_from_native(Process *p) { }

/* BEAM called native, which has thrown an exception. Clean up. */
static __inline__ void hipe_throw_from_native(Process *p) { }

/* BEAM called native, which now calls BEAM.
   Move the parameters to reg[].
   Return zero if this is a tailcall, non-zero if the call is recursive.
   If tailcall, also clean up native stub continuation. */
static __inline__ int
hipe_call_from_native_is_recursive(Process *p, Eterm reg[])
{
    Eterm nra;

    nra = *(p->hipe.nsp++);
    hipe_pop_x86_params(p, p->arity, reg);
    if (nra != (Eterm)nbif_return) {
	*--(p->hipe.nsp) = nra;
	return 1;
    }
    return 0;
}

/* BEAM called native, which called BIF that returned trap
 * Discard bif parameters.
 * If tailcall, also clean up native stub continuation. */
static __inline__ int
hipe_trap_from_native_is_recursive(Process *p)
{
    Eterm nra = *(p->hipe.nsp++);

    if (p->hipe.narity > NR_ARG_REGS) {
	p->hipe.nsp += (p->hipe.narity - NR_ARG_REGS);
    }
    if (nra != (Eterm)nbif_return) {
	*--(p->hipe.nsp) = nra;
	return 1;
    }
    return 0;
}

/* Native called BIF. Is it a recursive call?
   i.e should we return back to native when BIF is done? */
static __inline__ int
hipe_bifcall_from_native_is_recursive(Process *p)
{
    return (*p->hipe.nsp != (Eterm)nbif_return);
}


/* Native makes a call which needs to unload the parameters.
   This differs from hipe_call_from_native_is_recursive() in
   that it doesn't check for or pop the BEAM-calls-native frame.
   It's currently only used in the implementation of apply. */
static __inline__ void
hipe_pop_params(Process *p, unsigned int arity, Eterm reg[])
{
    if (arity > NR_ARG_REGS) {
	/* for apply/3 this will only happen if we configure
	   the runtime system with fewer argument registers
	   than default (i.e., 3) */
	Eterm nra = *(p->hipe.nsp++);
	hipe_pop_x86_params(p, arity, reg);
	*--(p->hipe.nsp) = nra;
    } else {
	/* arity <= NR_ARG_REGS so we optimise and
	   use hipe_read_x86_regs() directly */
	hipe_read_x86_regs(p, arity, reg);
    }
}

/* Native called BEAM, which now returns back to native. */
static __inline__ unsigned int hipe_return_to_native(Process *p)
{
    return x86_return_to_native(p);
}

/* Native called BEAM, which now throws an exception back to native. */
static __inline__ unsigned int hipe_throw_to_native(Process *p)
{
    return x86_throw_to_native(p);
}

/* Return the address of a stub switching a native closure call to BEAM. */
static __inline__ void *hipe_closure_stub_address(unsigned int arity)
{
#if NR_ARG_REGS == 0
    return nbif_ccallemu0;
#else	/* > 0 */
    switch (arity) {
      case 0:	return nbif_ccallemu0;
#if NR_ARG_REGS == 1
      default:	return nbif_ccallemu1;
#else	/* > 1 */
      case 1:	return nbif_ccallemu1;
#if NR_ARG_REGS == 2
      default:	return nbif_ccallemu2;
#else	/* > 2 */
      case 2:	return nbif_ccallemu2;
#if NR_ARG_REGS == 3
      default:	return nbif_ccallemu3;
#else	/* > 3 */
      case 3:	return nbif_ccallemu3;
#if NR_ARG_REGS == 4
      default:	return nbif_ccallemu4;
#else	/* > 4 */
      case 4:	return nbif_ccallemu4;
#if NR_ARG_REGS == 5
      default:	return nbif_ccallemu5;
#else	/* > 5 */
      case 5:	return nbif_ccallemu5;
#if NR_ARG_REGS == 6
      default:	return nbif_ccallemu6;
#else
#error "NR_ARG_REGS > 6 NOT YET IMPLEMENTED"
#endif	/* > 6 */
#endif	/* > 5 */
#endif	/* > 4 */
#endif	/* > 3 */
#endif	/* > 2 */
#endif	/* > 1 */
    }
#endif	/* > 0 */
}

#endif /* HIPE_X86_GLUE_H */
