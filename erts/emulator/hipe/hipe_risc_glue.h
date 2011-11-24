/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2008-2011. All Rights Reserved.
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


#ifndef HIPE_RISC_GLUE_H
#define HIPE_RISC_GLUE_H

/* arch wrapper does:
 * #include "hipe_${arch}_asm.h"	// for NR_ARG_REGS, ${ARCH}_LEAF_WORDS
 * #define NR_LEAF_WORDS		${ARCH}_LEAF_WORDS
 * #define HIPE_ARCH_CALL_TO_NATIVE	hipe_${arch}_call_to_native
 * #define HIPE_ARCH_RETURN_TO_NATIVE	hipe_${arch}_return_to_native
 * #define HIPE_ARCH_TAILCALL_TO_NATIVE	hipe_${arch}_tailcall_to_native
 * #define HIPE_ARCH_THROW_TO_NATIVE	hipe_${arch}_throw_to_native
 * #include "hipe_risc_glue.h"
 */

/* Emulated code recursively calls native code.
   The return address is `nbif_return', which is exported so that
   tailcalls from native to emulated code can be identified. */
unsigned int HIPE_ARCH_CALL_TO_NATIVE(Process*);
AEXTERN(void,nbif_return,(void));

/* Native-mode stubs for calling emulated-mode closures. */
AEXTERN(void,nbif_ccallemu0,(void));
AEXTERN(void,nbif_ccallemu1,(void));
AEXTERN(void,nbif_ccallemu2,(void));
AEXTERN(void,nbif_ccallemu3,(void));
AEXTERN(void,nbif_ccallemu4,(void));
AEXTERN(void,nbif_ccallemu5,(void));
AEXTERN(void,nbif_ccallemu6,(void));

/* Default exception handler for native code. */
AEXTERN(void,nbif_fail,(void));

/* Emulated code returns to its native code caller. */
unsigned int HIPE_ARCH_RETURN_TO_NATIVE(Process*);

/* Emulated code tailcalls native code. */
unsigned int HIPE_ARCH_TAILCALL_TO_NATIVE(Process*);

/* Emulated code throws an exception to its native code caller. */
unsigned int HIPE_ARCH_THROW_TO_NATIVE(Process*);

static __inline__ unsigned int max(unsigned int x, unsigned int y)
{
    return (x > y) ? x : y;
}

static __inline__ void hipe_arch_glue_init(void)
{
    static struct sdesc_with_exnra nbif_return_sdesc = {
	.exnra = (unsigned long)&nbif_fail,
	.sdesc = {
	    .bucket = { .hvalue = (unsigned long)&nbif_return },
	    .summary = (1<<8),
	},
    };
    hipe_init_sdesc_table(&nbif_return_sdesc.sdesc);
}

static __inline__ void hipe_push_risc_nra_frame(Process *p)
{
    p->hipe.nsp -= 1;
    p->hipe.nsp[0] = (Eterm)p->hipe.nra;
}

static __inline__ void hipe_pop_risc_nra_frame(Process *p)
{
    p->hipe.nra = (void(*)(void))p->hipe.nsp[0];
    p->hipe.nsp += 1;
}

/* PRE: arity <= NR_ARG_REGS */
static __inline__ void
hipe_write_risc_regs(Process *p, unsigned int arity, Eterm reg[])
{
#if NR_ARG_REGS > 0
    int i;
    for (i = arity; --i >= 0;)
	p->def_arg_reg[i] = reg[i];
#endif
}

/* PRE: arity <= NR_ARG_REGS */
static __inline__ void
hipe_read_risc_regs(Process *p, unsigned int arity, Eterm reg[])
{
#if NR_ARG_REGS > 0
    int i;
    for (i = arity; --i >= 0;)
	reg[i] = p->def_arg_reg[i];
#endif
}

static __inline__ void
hipe_push_risc_params(Process *p, unsigned int arity, Eterm reg[])
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
    hipe_write_risc_regs(p, i, reg);
}

static __inline__ void
hipe_pop_risc_params(Process *p, unsigned int arity, Eterm reg[])
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
    hipe_read_risc_regs(p, i, reg);
}

/* BEAM recursively calls native code. */
static __inline__ unsigned int
hipe_call_to_native(Process *p, unsigned int arity, Eterm reg[])
{
    int nstkargs;

    if ((nstkargs = arity - NR_ARG_REGS) < 0)
	nstkargs = 0;
    hipe_check_nstack(p, max(nstkargs + 1, NR_LEAF_WORDS));
    hipe_push_risc_nra_frame(p);			/* needs 1 word */
    hipe_push_risc_params(p, arity, reg);	/* needs nstkargs words */
    return HIPE_ARCH_CALL_TO_NATIVE(p);
}

/* Native called BEAM, which now tailcalls native. */
static __inline__ unsigned int
hipe_tailcall_to_native(Process *p, unsigned int arity, Eterm reg[])
{
    int nstkargs;

    if ((nstkargs = arity - NR_ARG_REGS) < 0)
	nstkargs = 0;
    hipe_check_nstack(p, max(nstkargs, NR_LEAF_WORDS));
    hipe_push_risc_params(p, arity, reg);	/* needs nstkargs words */
    return HIPE_ARCH_TAILCALL_TO_NATIVE(p);
}

/* BEAM called native, which has returned. Clean up. */
static __inline__ void hipe_return_from_native(Process *p)
{
    hipe_pop_risc_nra_frame(p);
}

/* BEAM called native, which has thrown an exception. Clean up. */
static __inline__ void hipe_throw_from_native(Process *p)
{
    hipe_pop_risc_nra_frame(p);
}

/* BEAM called native, which now calls BEAM.
   Move the parameters to reg[].
   Return zero if this is a tailcall, non-zero if the call is recursive.
   If tailcall, also clean up native stub continuation. */
static __inline__ int
hipe_call_from_native_is_recursive(Process *p, Eterm reg[])
{
    hipe_pop_risc_params(p, p->arity, reg);
    if (p->hipe.nra != (void(*)(void))&nbif_return)
	return 1;
    hipe_pop_risc_nra_frame(p);
    return 0;
}

/* BEAM called native, which called BIF that returned trap
 * Discard bif parameters.
 * If tailcall, also clean up native stub continuation. */
static __inline__ int
hipe_trap_from_native_is_recursive(Process *p)
{
    if (p->hipe.narity > NR_ARG_REGS) {
	p->hipe.nsp += (p->hipe.narity - NR_ARG_REGS);
    }
    if (p->hipe.nra != (void(*)(void))&nbif_return)
	return 1;
    hipe_pop_risc_nra_frame(p);
    return 0;
}


/* Native makes a call which needs to unload the parameters.
   This differs from hipe_call_from_native_is_recursive() in
   that it doesn't check for or pop the BEAM-calls-native frame.
   It's currently only used in the implementation of apply. */
static __inline__ void
hipe_pop_params(Process *p, unsigned int arity, Eterm reg[])
{
    hipe_pop_risc_params(p, arity, reg);
}

/* Native called BEAM, which now returns back to native. */
static __inline__ unsigned int hipe_return_to_native(Process *p)
{
    return HIPE_ARCH_RETURN_TO_NATIVE(p);
}

/* Native called BEAM, which now throws an exception back to native. */
static __inline__ unsigned int hipe_throw_to_native(Process *p)
{
    return HIPE_ARCH_THROW_TO_NATIVE(p);
}

/* Return the address of a stub switching a native closure call to BEAM. */
static __inline__ const void *hipe_closure_stub_address(unsigned int arity)
{
#if NR_ARG_REGS == 0
    return &nbif_ccallemu0;
#else	/* > 0 */
    switch (arity) {
      case 0:	return &nbif_ccallemu0;
#if NR_ARG_REGS == 1
      default:	return &nbif_ccallemu1;
#else	/* > 1 */
      case 1:	return &nbif_ccallemu1;
#if NR_ARG_REGS == 2
      default:	return &nbif_ccallemu2;
#else	/* > 2 */
      case 2:	return &nbif_ccallemu2;
#if NR_ARG_REGS == 3
      default:	return &nbif_ccallemu3;
#else	/* > 3 */
      case 3:	return &nbif_ccallemu3;
#if NR_ARG_REGS == 4
      default:	return &nbif_ccallemu4;
#else	/* > 4 */
      case 4:	return &nbif_ccallemu4;
#if NR_ARG_REGS == 5
      default:	return &nbif_ccallemu5;
#else	/* > 5 */
      case 5:	return &nbif_ccallemu5;
#if NR_ARG_REGS == 6
      default:	return &nbif_ccallemu6;
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

#endif /* HIPE_RISC_GLUE_H */
