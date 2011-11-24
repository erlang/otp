/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2001-2011. All Rights Reserved.
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
 * HiPE-specific process fields
 */
#ifndef HIPE_PROCESS_H
#define HIPE_PROCESS_H

#include "erl_alloc.h"

struct hipe_process_state {
    Eterm *nsp;			/* Native stack pointer. */
    Eterm *nstack;		/* Native stack block start. */
    Eterm *nstend;		/* Native stack block end (start+size). */
    /* XXX: ncallee and closure could share space in a union */
    void (*ncallee)(void);	/* Native code callee (label) to invoke. */
    Eterm closure;		/* Used to pass a closure from native code. */
    Eterm *nstgraylim;		/* Gray/white stack boundary. */
    Eterm *nstblacklim;		/* Black/gray stack boundary. Must exist if
				   graylim exists. Ignored if no graylim. */
    void (*ngra)(void);		/* Saved original RA from graylim frame. */
#if defined(__i386__) || defined(__x86_64__)
    Eterm *ncsp;		/* Saved C stack pointer. */
#endif
#if defined(__sparc__) || defined(__powerpc__) || defined(__ppc__) || defined(__powerpc64__) || defined(__arm__)
    void (*nra)(void);		/* Native code return address. */
#endif
    unsigned int narity;	/* Arity of BIF call, for stack walks. */
#ifdef NO_FPE_SIGNALS
    double float_result;        /* to be checked for inf/NaN by hipe_emulate_fpe */ 
#endif
#if defined(ERTS_ENABLE_LOCK_CHECK) && defined(ERTS_SMP)
    void (*bif_callee)(void);   /* When calling BIF's via debug wrapper */
#endif
};

extern void hipe_arch_print_pcb(struct hipe_process_state *p);

static __inline__ void hipe_init_process(struct hipe_process_state *p)
{
    p->nsp = NULL;
    p->nstack = NULL;
    p->nstend = NULL;
    p->nstgraylim = NULL;
    p->nstblacklim = NULL;
    p->ngra = NULL;
#if defined(__sparc__) || defined(__powerpc__) || defined(__ppc__) || defined(__powerpc64__) || defined(__arm__)
    p->nra = NULL;
#endif
    p->narity = 0;
}

static __inline__ void hipe_delete_process(struct hipe_process_state *p)
{
    if (p->nstack)
	erts_free(ERTS_ALC_T_HIPE, (void*)p->nstack);
}

#ifdef ERTS_SMP
struct hipe_process_state_smp {
    int have_receive_locks;
};

static __inline__ void hipe_init_process_smp(struct hipe_process_state_smp *p)
{
    p->have_receive_locks = 0;
}
#endif

#endif /* HIPE_PROCESS_H */
