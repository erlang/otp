/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2016. All Rights Reserved.
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
#  include "config.h"
#endif

#define ERTS_WANT_NFUNC_SCHED_INTERNALS__

#include "global.h"
#include "erl_process.h"
#include "bif.h"
#include "erl_nfunc_sched.h"
#include "erl_trace.h"

NifExport *
erts_new_proc_nif_export(Process *c_p, int argc)
{
    size_t size;
    int i;
    NifExport *nep, *old_nep;

    size = sizeof(NifExport) + (argc-1)*sizeof(Eterm);
    nep = erts_alloc(ERTS_ALC_T_NIF_TRAP_EXPORT, size);

    for (i = 0; i < ERTS_NUM_CODE_IX; i++)
	nep->exp.addressv[i] = &nep->exp.beam[0];

    nep->argc = -1; /* unused marker */
    nep->argv_size = argc;
    nep->trace = NULL;
    old_nep = ERTS_PROC_SET_NIF_TRAP_EXPORT(c_p, nep);
    if (old_nep) {
	ASSERT(!nep->trace);
	erts_free(ERTS_ALC_T_NIF_TRAP_EXPORT, old_nep);
    }
    return nep;
}

void
erts_destroy_nif_export(Process *p)
{
    NifExport *nep = ERTS_PROC_SET_NIF_TRAP_EXPORT(p, NULL);
    if (nep) {
	if (nep->m)
	    erts_nif_export_cleanup_nif_mod(nep);
	erts_free(ERTS_ALC_T_NIF_TRAP_EXPORT, nep);
    }
}

void
erts_nif_export_save_trace(Process *c_p, NifExport *nep, int applying,
			   Export* ep, BeamInstr *cp, Uint32 flags,
			   Uint32 flags_meta, BeamInstr* I,
			   ErtsTracer meta_tracer)
{
    NifExportTrace *netp;
    ASSERT(nep && nep->argc >= 0);
    ASSERT(!nep->trace);
    netp = erts_alloc(ERTS_ALC_T_NIF_EXP_TRACE,
		      sizeof(NifExportTrace));
    netp->applying = applying;
    netp->ep = ep;
    netp->cp = cp;
    netp->flags = flags;
    netp->flags_meta = flags_meta;
    netp->I = I;
    netp->meta_tracer = NIL;
    erts_tracer_update(&netp->meta_tracer, meta_tracer);
    nep->trace = netp;
}

void
erts_nif_export_restore_trace(Process *c_p, Eterm result, NifExport *nep)
{
    NifExportTrace *netp = nep->trace;
    nep->trace = NULL;
    erts_bif_trace_epilogue(c_p, result, netp->applying, netp->ep,
			    netp->cp, netp->flags, netp->flags_meta,
			    netp->I, netp->meta_tracer);
    erts_tracer_update(&netp->meta_tracer, NIL);
    erts_free(ERTS_ALC_T_NIF_EXP_TRACE, netp);
}

NifExport *
erts_nif_export_schedule(Process *c_p, Process *dirty_shadow_proc,
			 ErtsCodeMFA *mfa, BeamInstr *pc,
			 BeamInstr instr,
			 void *dfunc, void *ifunc,
			 Eterm mod, Eterm func,
			 int argc, const Eterm *argv)
{
    Process *used_proc;
    ErtsSchedulerData *esdp;
    Eterm* reg;
    NifExport* nep;
    int i;

    ERTS_LC_ASSERT(erts_proc_lc_my_proc_locks(c_p)
		       & ERTS_PROC_LOCK_MAIN);

    if (dirty_shadow_proc) {
	esdp = erts_get_scheduler_data();
	ASSERT(esdp && ERTS_SCHEDULER_IS_DIRTY(esdp));

	used_proc = dirty_shadow_proc;
    }
    else {
	esdp = erts_proc_sched_data(c_p);
	ASSERT(esdp && !ERTS_SCHEDULER_IS_DIRTY(esdp));

	used_proc = c_p;
	ERTS_VBUMP_ALL_REDS(c_p);
    }

    reg = esdp->x_reg_array;

    if (mfa)
	nep = erts_get_proc_nif_export(c_p, (int) mfa->arity);
    else {
	/* If no mfa, this is not the first schedule... */
	nep = ERTS_PROC_GET_NIF_TRAP_EXPORT(c_p);
	ASSERT(nep && nep->argc >= 0);
    }

    if (nep->argc < 0) {
	/*
	 * First schedule; save things that might
	 * need to be restored...
	 */
	for (i = 0; i < (int) mfa->arity; i++)
	    nep->argv[i] = reg[i];
	nep->pc = pc;
	nep->cp = c_p->cp;
	nep->mfa = mfa;
	nep->current = c_p->current;
	ASSERT(argc >= 0);
	nep->argc = (int) mfa->arity;
	nep->m = NULL;

	ASSERT(!erts_check_nif_export_in_area(c_p,
					      (char *) nep,
					      (sizeof(NifExport)
					       + (sizeof(Eterm)
						  *(nep->argc-1)))));
    }
    /* Copy new arguments into register array if necessary... */
    if (reg != argv) {
	for (i = 0; i < argc; i++)
	    reg[i] = argv[i];
    }
    ASSERT(is_atom(mod) && is_atom(func));
    nep->exp.info.mfa.module = mod;
    nep->exp.info.mfa.function = func;
    nep->exp.info.mfa.arity = (Uint) argc;
    nep->exp.beam[0] = (BeamInstr) instr; /* call_nif || apply_bif */
    nep->exp.beam[1] = (BeamInstr) dfunc;
    nep->func = ifunc;
    used_proc->arity = argc;
    used_proc->freason = TRAP;
    used_proc->i = (BeamInstr*) nep->exp.addressv[0];
    return nep;
}
