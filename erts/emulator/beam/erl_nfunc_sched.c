/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2016-2018. All Rights Reserved.
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
    NifExport *nep, *old_nep;
    size_t size;

    size = sizeof(NifExport) + (argc-1)*sizeof(Eterm);
    nep = erts_alloc(ERTS_ALC_T_NIF_TRAP_EXPORT, size);

    nep->argc = -1; /* unused marker */
    nep->argv_size = argc;
    old_nep = ERTS_PROC_SET_NIF_TRAP_EXPORT(c_p, nep);
    if (old_nep) {
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
    nep->trampoline.info.mfa.module = mod;
    nep->trampoline.info.mfa.function = func;
    nep->trampoline.info.mfa.arity = (Uint) argc;
    nep->trampoline.call_op = (BeamInstr) instr; /* call_nif || apply_bif */
    nep->trampoline.dfunc = (BeamInstr) dfunc;
    nep->func = ifunc;
    used_proc->arity = argc;
    used_proc->freason = TRAP;
    used_proc->i = (BeamInstr*)&nep->trampoline.call_op;
    return nep;
}
