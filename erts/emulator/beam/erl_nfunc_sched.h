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

#ifndef ERL_NFUNC_SCHED_H__
#define ERL_NFUNC_SCHED_H__

#include "erl_process.h"
#include "bif.h"
#include "error.h"

typedef struct {
    int applying;
    Export* ep;
    BeamInstr *cp;
    Uint32 flags;
    Uint32 flags_meta;
    BeamInstr* I;
    ErtsTracer meta_tracer;
} NifExportTrace;

/*
 * NIF exports need a few more items than the Export struct provides,
 * including the erl_module_nif* and a NIF function pointer, so the
 * NifExport below adds those. The Export member must be first in the
 * struct. A number of values are stored for error handling purposes
 * only.
 *
 * 'argc' is >= 0 when NifExport is in use, and < 0 when not.
 */

typedef struct {
    Export exp;
    struct erl_module_nif* m; /* NIF module, or NULL if BIF */
    void *func;		/* Indirect NIF or BIF to execute (may be unused) */
    ErtsCodeMFA *current;/* Current as set when originally called */
    NifExportTrace *trace;
    /* --- The following is only used on error --- */
    BeamInstr *pc;	/* Program counter */
    BeamInstr *cp;	/* Continuation pointer */
    ErtsCodeMFA *mfa;	/* MFA of original call */
    int argc;		/* Number of arguments in original call */
    int argv_size;	/* Allocated size of argv */
    Eterm argv[1];	/* Saved arguments from the original call */
} NifExport;

NifExport *erts_new_proc_nif_export(Process *c_p, int argc);
void erts_nif_export_save_trace(Process *c_p, NifExport *nep, int applying,
				Export* ep, BeamInstr *cp, Uint32 flags,
				Uint32 flags_meta, BeamInstr* I,
				ErtsTracer meta_tracer);
void erts_nif_export_restore_trace(Process *c_p, Eterm result, NifExport *nep);
void erts_destroy_nif_export(Process *p);
NifExport *erts_nif_export_schedule(Process *c_p, Process *dirty_shadow_proc,
				    ErtsCodeMFA *mfa, BeamInstr *pc,
				    BeamInstr instr,
				    void *dfunc, void *ifunc,
				    Eterm mod, Eterm func,
				    int argc, const Eterm *argv);
void erts_nif_export_cleanup_nif_mod(NifExport *ep); /* erl_nif.c */
ERTS_GLB_INLINE NifExport *erts_get_proc_nif_export(Process *c_p, int extra);
ERTS_GLB_INLINE int erts_setup_nif_export_rootset(Process* proc, Eterm** objv,
						  Uint* nobj);
ERTS_GLB_INLINE int erts_check_nif_export_in_area(Process *p,
						  char *start, Uint size);
ERTS_GLB_INLINE void erts_nif_export_restore(Process *c_p, NifExport *ep,
					     Eterm result);
ERTS_GLB_INLINE void erts_nif_export_restore_error(Process* c_p, BeamInstr **pc,
						   Eterm *reg, ErtsCodeMFA **nif_mfa);
ERTS_GLB_INLINE int erts_nif_export_check_save_trace(Process *c_p, Eterm result,
						     int applying, Export* ep,
						     BeamInstr *cp, Uint32 flags,
						     Uint32 flags_meta, BeamInstr* I,
						     ErtsTracer meta_tracer);
ERTS_GLB_INLINE Process *erts_proc_shadow2real(Process *c_p);

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

ERTS_GLB_INLINE NifExport *
erts_get_proc_nif_export(Process *c_p, int argc)
{
    NifExport *nep = ERTS_PROC_GET_NIF_TRAP_EXPORT(c_p);
    if (!nep || (nep->argc < 0 && nep->argv_size < argc))
	return erts_new_proc_nif_export(c_p, argc);
    return nep;
}

/*
 * If a process has saved arguments, they need to be part of the GC
 * rootset. The function below is called from setup_rootset() in
 * erl_gc.c. Any exception term saved in the NifExport is also made
 * part of the GC rootset here; it always resides in rootset[0].
 */
ERTS_GLB_INLINE int
erts_setup_nif_export_rootset(Process* proc, Eterm** objv, Uint* nobj)
{
    NifExport* ep = (NifExport*) ERTS_PROC_GET_NIF_TRAP_EXPORT(proc);

    if (!ep || ep->argc <= 0)
	return 0;

    *objv = ep->argv;
    *nobj = ep->argc;
    return 1;
}

/*
 * Check if nif export points into code area...
 */
ERTS_GLB_INLINE int
erts_check_nif_export_in_area(Process *p, char *start, Uint size)
{
    NifExport *nep = ERTS_PROC_GET_NIF_TRAP_EXPORT(p);
    if (!nep || nep->argc < 0)
	return 0;
    if (ErtsInArea(nep->pc, start, size))
	return 1;
    if (ErtsInArea(nep->cp, start, size))
	return 1;
    if (ErtsInArea(nep->mfa, start, size))
	return 1;
    if (ErtsInArea(nep->current, start, size))
	return 1;
    return 0;
}

ERTS_GLB_INLINE void
erts_nif_export_restore(Process *c_p, NifExport *ep, Eterm result)
{
    ASSERT(!ERTS_SCHEDULER_IS_DIRTY(erts_get_scheduler_data()));
    ERTS_LC_ASSERT(!(c_p->static_flags
			 & ERTS_STC_FLG_SHADOW_PROC));
    ERTS_LC_ASSERT(erts_proc_lc_my_proc_locks(c_p)
		       & ERTS_PROC_LOCK_MAIN);

    c_p->current = ep->current;
    ep->argc = -1; /* Unused nif-export marker... */
    if (ep->trace)
	erts_nif_export_restore_trace(c_p, result, ep);
}

ERTS_GLB_INLINE void
erts_nif_export_restore_error(Process* c_p, BeamInstr **pc,
			      Eterm *reg, ErtsCodeMFA **nif_mfa)
{
    NifExport *nep = (NifExport *) ERTS_PROC_GET_NIF_TRAP_EXPORT(c_p);
    int ix;

    ASSERT(nep);
    *pc = nep->pc;
    c_p->cp = nep->cp;
    *nif_mfa = nep->mfa;
    for (ix = 0; ix < nep->argc; ix++)
	reg[ix] = nep->argv[ix];
    erts_nif_export_restore(c_p, nep, THE_NON_VALUE);
}

ERTS_GLB_INLINE int
erts_nif_export_check_save_trace(Process *c_p, Eterm result,
				 int applying, Export* ep,
				 BeamInstr *cp, Uint32 flags,
				 Uint32 flags_meta, BeamInstr* I,
				 ErtsTracer meta_tracer)
{
    if (is_non_value(result) && c_p->freason == TRAP) {
	NifExport *nep = ERTS_PROC_GET_NIF_TRAP_EXPORT(c_p);
	if (nep && nep->argc >= 0) {
	    erts_nif_export_save_trace(c_p, nep, applying, ep,
				       cp, flags, flags_meta,
				       I, meta_tracer);
	    return 1;
	}
    }
    return 0;
}

ERTS_GLB_INLINE Process *
erts_proc_shadow2real(Process *c_p)
{
    if (c_p->static_flags & ERTS_STC_FLG_SHADOW_PROC) {
	Process *real_c_p = c_p->next;
	ASSERT(ERTS_SCHEDULER_IS_DIRTY(erts_get_scheduler_data()));
	ASSERT(real_c_p->common.id == c_p->common.id);
	return real_c_p;
    }
    ASSERT(!ERTS_SCHEDULER_IS_DIRTY(erts_get_scheduler_data()));
    return c_p;
}

#endif /* ERTS_GLB_INLINE_INCL_FUNC_DEF */

#endif /* ERL_NFUNC_SCHED_H__ */

#if defined(ERTS_WANT_NFUNC_SCHED_INTERNALS__) && !defined(ERTS_NFUNC_SCHED_INTERNALS__)
#define ERTS_NFUNC_SCHED_INTERNALS__

#define ERTS_I_BEAM_OP_TO_NIF_EXPORT(I)					\
    (ASSERT(BeamIsOpCode(*(I), op_apply_bif) ||                         \
            BeamIsOpCode(*(I), op_call_nif)),                           \
     ((NifExport *) (((char *) (I)) - offsetof(NifExport, exp.beam[0]))))


#include "erl_message.h"
#include <stddef.h>

ERTS_GLB_INLINE void erts_flush_dirty_shadow_proc(Process *sproc);
ERTS_GLB_INLINE void erts_cache_dirty_shadow_proc(Process *sproc);
ERTS_GLB_INLINE Process *erts_make_dirty_shadow_proc(ErtsSchedulerData *esdp,
						     Process *c_p);

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

ERTS_GLB_INLINE void
erts_flush_dirty_shadow_proc(Process *sproc)
{
    Process *c_p = sproc->next;

    ASSERT(sproc->common.id == c_p->common.id);
    ERTS_LC_ASSERT(erts_proc_lc_my_proc_locks(c_p)
		       & ERTS_PROC_LOCK_MAIN);

    ASSERT(c_p->stop == sproc->stop);
    ASSERT(c_p->hend == sproc->hend);
    ASSERT(c_p->heap == sproc->heap);
    ASSERT(c_p->abandoned_heap == sproc->abandoned_heap);
    ASSERT(c_p->heap_sz == sproc->heap_sz);
    ASSERT(c_p->high_water == sproc->high_water);
    ASSERT(c_p->old_heap == sproc->old_heap);
    ASSERT(c_p->old_htop == sproc->old_htop);
    ASSERT(c_p->old_hend == sproc->old_hend);

    ASSERT(c_p->htop <= sproc->htop && sproc->htop <= c_p->stop);

    c_p->htop = sproc->htop;

    if (!c_p->mbuf)
	c_p->mbuf = sproc->mbuf;
    else if (sproc->mbuf) {
	ErlHeapFragment *bp;
	for (bp = sproc->mbuf; bp->next; bp = bp->next)
	    ASSERT(!bp->off_heap.first);
	bp->next = c_p->mbuf;
	c_p->mbuf = sproc->mbuf;
    }

    c_p->mbuf_sz += sproc->mbuf_sz;

    if (!c_p->off_heap.first)
	c_p->off_heap.first = sproc->off_heap.first;
    else if (sproc->off_heap.first) {
	struct erl_off_heap_header *ohhp;
	for (ohhp = sproc->off_heap.first; ohhp->next; ohhp = ohhp->next)
	    ;
	ohhp->next = c_p->off_heap.first;
	c_p->off_heap.first = sproc->off_heap.first;
    }

    c_p->off_heap.overhead += sproc->off_heap.overhead;
}

ERTS_GLB_INLINE void
erts_cache_dirty_shadow_proc(Process *sproc)
{
    Process *c_p = sproc->next;
    ASSERT(c_p);
    ASSERT(sproc->common.id == c_p->common.id);
    ERTS_LC_ASSERT(erts_proc_lc_my_proc_locks(c_p)
		       & ERTS_PROC_LOCK_MAIN);

    sproc->htop = c_p->htop;
    sproc->stop = c_p->stop;
    sproc->hend = c_p->hend;
    sproc->heap = c_p->heap;
    sproc->abandoned_heap = c_p->abandoned_heap;
    sproc->heap_sz = c_p->heap_sz;
    sproc->high_water = c_p->high_water;
    sproc->old_hend = c_p->old_hend;
    sproc->old_htop = c_p->old_htop;
    sproc->old_heap = c_p->old_heap;
    sproc->mbuf = NULL;
    sproc->mbuf_sz = 0;
    ERTS_INIT_OFF_HEAP(&sproc->off_heap);
}

ERTS_GLB_INLINE Process *
erts_make_dirty_shadow_proc(ErtsSchedulerData *esdp, Process *c_p)
{
    Process *sproc;

    ASSERT(ERTS_SCHEDULER_IS_DIRTY(esdp));

    sproc = esdp->dirty_shadow_process;
    ASSERT(sproc);
    ASSERT(sproc->static_flags & ERTS_STC_FLG_SHADOW_PROC);
    ASSERT(erts_atomic32_read_nob(&sproc->state)
	   == (ERTS_PSFLG_ACTIVE
	       | ERTS_PSFLG_DIRTY_RUNNING
	       | ERTS_PSFLG_PROXY));

    sproc->next = c_p;
    sproc->common.id = c_p->common.id;

    erts_cache_dirty_shadow_proc(sproc);

    return sproc;
}

#endif /* ERTS_GLB_INLINE_INCL_FUNC_DEF */


#endif /* defined(ERTS_WANT_NFUNC_SCHED_INTERNALS__) && !defined(ERTS_NFUNC_SCHED_INTERNALS__) */

