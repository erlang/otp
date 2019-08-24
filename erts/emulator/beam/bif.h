/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1996-2018. All Rights Reserved.
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

#ifndef __BIF_H__
#define __BIF_H__

extern Export *erts_await_result;
extern Export* erts_format_cpu_topology_trap;
extern Export *erts_convert_time_unit_trap;

#define BIF_RETTYPE Eterm

#define BIF_P A__p

#define BIF_ALIST Process* A__p, Eterm* BIF__ARGS, BeamInstr *A__I
#define BIF_CALL_ARGS A__p, BIF__ARGS, A__I

#define BIF_ALIST_0 BIF_ALIST
#define BIF_ALIST_1 BIF_ALIST
#define BIF_ALIST_2 BIF_ALIST
#define BIF_ALIST_3 BIF_ALIST
#define BIF_ALIST_4 BIF_ALIST

#define BIF_ARG_1  (BIF__ARGS[0])
#define BIF_ARG_2  (BIF__ARGS[1])
#define BIF_ARG_3  (BIF__ARGS[2])
#define BIF_ARG_4  (BIF__ARGS[3])

#define BIF_I A__I

/* NBIF_* is for bif calls from native code... */

#define NBIF_ALIST Process* A__p, Eterm* BIF__ARGS
#define NBIF_CALL_ARGS A__p, BIF__ARGS

#define NBIF_ALIST_0 NBIF_ALIST
#define NBIF_ALIST_1 NBIF_ALIST
#define NBIF_ALIST_2 NBIF_ALIST
#define NBIF_ALIST_3 NBIF_ALIST
#define NBIF_ALIST_4 NBIF_ALIST

typedef BIF_RETTYPE (*ErtsBifFunc)(BIF_ALIST);

#define ERTS_IS_PROC_OUT_OF_REDS(p)		\
    ((p)->fcalls > 0				\
     ? 0					\
     : (!ERTS_PROC_GET_SAVED_CALLS_BUF((p))	\
	? (p)->fcalls == 0			\
	: ((p)->fcalls == -CONTEXT_REDS)))

#define BUMP_ALL_REDS(p) do {			\
    if (!ERTS_PROC_GET_SAVED_CALLS_BUF((p))) 	\
	(p)->fcalls = 0; 			\
    else 					\
	(p)->fcalls = -CONTEXT_REDS;		\
    ASSERT(ERTS_BIF_REDS_LEFT((p)) == 0);	\
} while(0)

#define ERTS_VBUMP_ALL_REDS_INTERNAL(p, fcalls)				\
do {									\
    if (!ERTS_PROC_GET_SAVED_CALLS_BUF((p))) {				\
	if ((fcalls) > 0)						\
	    erts_proc_sched_data((p))->virtual_reds += (fcalls);	\
	(fcalls) = 0;							\
    }									\
    else {								\
	if ((fcalls) > -CONTEXT_REDS)					\
	    erts_proc_sched_data((p))->virtual_reds			\
		+= ((fcalls) - (-CONTEXT_REDS));			\
	(fcalls) = -CONTEXT_REDS;					\
    }									\
} while(0)

#define ERTS_VBUMP_ALL_REDS(p) \
    ERTS_VBUMP_ALL_REDS_INTERNAL((p), (p)->fcalls)

#define BUMP_REDS(p, gc) do {			   \
     ASSERT(p);		 			   \
     ERTS_LC_ASSERT(ERTS_PROC_LOCK_MAIN & erts_proc_lc_my_proc_locks(p));\
     (p)->fcalls -= (gc); 			   \
     if ((p)->fcalls < 0) { 			   \
	if (!ERTS_PROC_GET_SAVED_CALLS_BUF((p)))   \
           (p)->fcalls = 0; 			   \
	else if ((p)->fcalls < -CONTEXT_REDS)      \
           (p)->fcalls = -CONTEXT_REDS; 	   \
     } 						   \
} while(0)


#define ERTS_VBUMP_REDS(p, reds)					\
do {									\
    if (!ERTS_PROC_GET_SAVED_CALLS_BUF((p))) {				\
	if ((p)->fcalls >= reds) {					\
	    (p)->fcalls -= reds;					\
	    erts_proc_sched_data((p))->virtual_reds += reds;		\
	}								\
	else {								\
	    if ((p)->fcalls > 0)					\
		erts_proc_sched_data((p))->virtual_reds += (p)->fcalls;	\
	    (p)->fcalls = 0;						\
	}								\
    }									\
    else {								\
	if ((p)->fcalls >= reds - CONTEXT_REDS) {			\
	    (p)->fcalls -= reds;					\
	    erts_proc_sched_data((p))->virtual_reds += reds;		\
	}								\
	else {								\
	    if ((p)->fcalls > -CONTEXT_REDS)				\
		erts_proc_sched_data((p))->virtual_reds			\
		    += (p)->fcalls - (-CONTEXT_REDS);			\
	    (p)->fcalls = -CONTEXT_REDS;				\
	}								\
    }									\
} while(0)

#define ERTS_VBUMP_LEAVE_REDS_INTERNAL(P, Reds, FCalls)			\
    do {								\
	if (ERTS_PROC_GET_SAVED_CALLS_BUF((P))) {			\
	    int nreds__ = ((int)(Reds)) - CONTEXT_REDS;			\
	    if ((FCalls) > nreds__) {					\
		erts_proc_sched_data((P))->virtual_reds			\
		    += (FCalls) - nreds__;				\
		(FCalls) = nreds__;					\
	    }								\
	}								\
	else {								\
	    if ((FCalls) > (Reds)) {					\
		erts_proc_sched_data((P))->virtual_reds			\
		    += (FCalls) - (Reds);				\
		(FCalls) = (Reds);					\
	    }								\
	}								\
    } while (0)

#define ERTS_VBUMP_LEAVE_REDS(P, Reds)					\
    ERTS_VBUMP_LEAVE_REDS_INTERNAL(P, Reds, (P)->fcalls)

#define ERTS_REDS_LEFT(p, FCalls)					\
  (ERTS_PROC_GET_SAVED_CALLS_BUF((p))					\
   ? ((FCalls) > -CONTEXT_REDS ? ((FCalls) - (-CONTEXT_REDS)) : 0)	\
   : ((FCalls) > 0 ? (FCalls) : 0))

#define ERTS_BIF_REDS_LEFT(p) ERTS_REDS_LEFT(p, p->fcalls)

#define BIF_RET2(x, gc) do {			\
    BUMP_REDS(BIF_P, (gc));			\
    return (x);					\
} while(0)

#define BIF_RET(x) return (x)

#define ERTS_BIF_PREP_RET(Ret, Val) ((Ret) = (Val))

#define BIF_ERROR(p,r) do { 			\
    (p)->freason = r; 				\
    return THE_NON_VALUE; 			\
} while(0)

#define ERTS_BIF_ERROR_TRAPPED0(Proc, Reason, Bif)		\
do {								\
    (Proc)->freason = (Reason);					\
    (Proc)->current = &(Bif)->info.mfa;                         \
    return THE_NON_VALUE; 					\
} while (0)

#define ERTS_BIF_ERROR_TRAPPED1(Proc, Reason, Bif, A0)		\
do {								\
    Eterm* reg = erts_proc_sched_data((Proc))->x_reg_array;	\
    (Proc)->freason = (Reason);					\
    (Proc)->current = &(Bif)->info.mfa;                         \
    reg[0] = (Eterm) (A0);					\
    return THE_NON_VALUE; 					\
} while (0)

#define ERTS_BIF_ERROR_TRAPPED2(Proc, Reason, Bif, A0, A1)	\
do {								\
    Eterm* reg = erts_proc_sched_data((Proc))->x_reg_array;	\
    (Proc)->freason = (Reason);					\
    (Proc)->current = &(Bif)->info.mfa;                         \
    reg[0] = (Eterm) (A0);					\
    reg[1] = (Eterm) (A1);					\
    return THE_NON_VALUE; 					\
} while (0)

#define ERTS_BIF_ERROR_TRAPPED3(Proc, Reason, Bif, A0, A1, A2)	\
do {								\
    Eterm* reg = erts_proc_sched_data((Proc))->x_reg_array;	\
    (Proc)->freason = (Reason);					\
    (Proc)->current = &(Bif)->info.mfa;                         \
    reg[0] = (Eterm) (A0);					\
    reg[1] = (Eterm) (A1);					\
    reg[2] = (Eterm) (A2);					\
    return THE_NON_VALUE; 					\
} while (0)

#define ERTS_BIF_PREP_ERROR(Ret, Proc, Reason)	\
do {						\
    (Proc)->freason = (Reason);			\
    (Ret) = THE_NON_VALUE;			\
} while (0)

#define ERTS_BIF_PREP_ERROR_TRAPPED0(Ret, Proc, Reason, Bif)	\
do {								\
    (Proc)->freason = (Reason);					\
    (Proc)->current = &(Bif)->info.mfa;                         \
    (Ret) = THE_NON_VALUE;					\
} while (0)

#define ERTS_BIF_PREP_ERROR_TRAPPED1(Ret, Proc, Reason, Bif, A0) \
do {								\
    Eterm* reg = erts_proc_sched_data((Proc))->x_reg_array;	\
    (Proc)->freason = (Reason);					\
    (Proc)->current = &(Bif)->info.mfa;                         \
    reg[0] = (Eterm) (A0);					\
    (Ret) = THE_NON_VALUE;					\
} while (0)

#define ERTS_BIF_PREP_ERROR_TRAPPED2(Ret, Proc, Reason, Bif, A0, A1) \
do {								\
    Eterm* reg = erts_proc_sched_data((Proc))->x_reg_array;	\
    (Proc)->freason = (Reason);					\
    (Proc)->current = &(Bif)->info.mfa;                         \
    reg[0] = (Eterm) (A0);					\
    reg[1] = (Eterm) (A1);					\
    (Ret) = THE_NON_VALUE;					\
} while (0)

#define ERTS_BIF_PREP_ERROR_TRAPPED3(Ret, Proc, Reason, Bif, A0, A1, A2) \
do {								\
    Eterm* reg = erts_proc_sched_data((Proc))->x_reg_array;	\
    (Proc)->freason = (Reason);					\
    (Proc)->current = &(Bif)->info.mfa;                         \
    reg[0] = (Eterm) (A0);					\
    reg[1] = (Eterm) (A1);					\
    reg[2] = (Eterm) (A2);					\
    (Ret) = THE_NON_VALUE;					\
} while (0)

#define ERTS_BIF_PREP_TRAP0(Ret, Trap, Proc)	\
do {						\
    (Proc)->arity = 0;				\
    (Proc)->i = (BeamInstr*) ((Trap)->addressv[erts_active_code_ix()]);	\
    (Proc)->freason = TRAP;			\
    (Ret) = THE_NON_VALUE;			\
} while (0)

#define ERTS_BIF_PREP_TRAP1(Ret, Trap, Proc, A0)		\
do {								\
    Eterm* reg = erts_proc_sched_data((Proc))->x_reg_array;	\
    (Proc)->arity = 1;						\
    reg[0] = (Eterm) (A0);					\
    (Proc)->i = (BeamInstr*) ((Trap)->addressv[erts_active_code_ix()]); \
    (Proc)->freason = TRAP;					\
    (Ret) = THE_NON_VALUE;					\
} while (0)

#define ERTS_BIF_PREP_TRAP2(Ret, Trap, Proc, A0, A1)		\
do {								\
    Eterm* reg = erts_proc_sched_data((Proc))->x_reg_array;	\
    (Proc)->arity = 2;						\
    reg[0] = (Eterm) (A0);					\
    reg[1] = (Eterm) (A1);					\
    (Proc)->i = (BeamInstr*) ((Trap)->addressv[erts_active_code_ix()]); \
    (Proc)->freason = TRAP;					\
    (Ret) = THE_NON_VALUE;					\
} while (0)

#define ERTS_BIF_PREP_TRAP3(Ret, Trap, Proc, A0, A1, A2)	\
do {								\
    Eterm* reg = erts_proc_sched_data((Proc))->x_reg_array;	\
    (Proc)->arity = 3;						\
    reg[0] = (Eterm) (A0);					\
    reg[1] = (Eterm) (A1);					\
    reg[2] = (Eterm) (A2);					\
    (Proc)->i = (BeamInstr*) ((Trap)->addressv[erts_active_code_ix()]); \
    (Proc)->freason = TRAP;					\
    (Ret) = THE_NON_VALUE;					\
} while (0)

#define ERTS_BIF_PREP_TRAP4(Ret, Trap, Proc, A0, A1, A2, A3)	\
do {								\
    Eterm* reg = erts_proc_sched_data((Proc))->x_reg_array;	\
    (Proc)->arity = 4;						\
    reg[0] = (Eterm) (A0);					\
    reg[1] = (Eterm) (A1);					\
    reg[2] = (Eterm) (A2);					\
    reg[3] = (Eterm) (A3);					\
    (Proc)->i = (BeamInstr*) ((Trap)->addressv[erts_active_code_ix()]); \
    (Proc)->freason = TRAP;					\
    (Ret) = THE_NON_VALUE;					\
} while (0)

#define ERTS_BIF_PREP_TRAP3_NO_RET(Trap, Proc, A0, A1, A2)\
do {							\
    Eterm* reg = erts_proc_sched_data((Proc))->x_reg_array;	\
    (Proc)->arity = 3;					\
    reg[0] = (Eterm) (A0);		\
    reg[1] = (Eterm) (A1);		\
    reg[2] = (Eterm) (A2);		\
    (Proc)->i = (BeamInstr*) ((Trap)->addressv[erts_active_code_ix()]); \
    (Proc)->freason = TRAP;				\
} while (0)

#define BIF_TRAP0(Trap_, p) do {                  \
      (p)->arity = 0;				\
      (p)->i = (BeamInstr*) ((Trap_)->addressv[erts_active_code_ix()]);	\
      (p)->freason = TRAP;			\
      return THE_NON_VALUE;			\
 } while(0)

#define BIF_TRAP1(Trap_, p, A0) do {				\
      Eterm* reg = erts_proc_sched_data((p))->x_reg_array;	\
      (p)->arity = 1;						\
      reg[0] = (A0);						\
      (p)->i = (BeamInstr*) ((Trap_)->addressv[erts_active_code_ix()]); \
      (p)->freason = TRAP;					\
      return THE_NON_VALUE;					\
 } while(0)

#define BIF_TRAP2(Trap_, p, A0, A1) do {			\
      Eterm* reg = erts_proc_sched_data((p))->x_reg_array;	\
      (p)->arity = 2;						\
      reg[0] = (A0);						\
      reg[1] = (A1);						\
      (p)->i = (BeamInstr*) ((Trap_)->addressv[erts_active_code_ix()]); \
      (p)->freason = TRAP;					\
      return THE_NON_VALUE;					\
 } while(0)

#define BIF_TRAP3(Trap_, p, A0, A1, A2) do {			\
      Eterm* reg = erts_proc_sched_data((p))->x_reg_array;	\
      (p)->arity = 3;						\
      reg[0] = (A0);						\
      reg[1] = (A1);						\
      reg[2] = (A2);						\
      (p)->i = (BeamInstr*) ((Trap_)->addressv[erts_active_code_ix()]); \
      (p)->freason = TRAP;					\
      return THE_NON_VALUE;					\
 } while(0)

#define BIF_TRAP4(Trap_, p, A0, A1, A2, A3) do {		\
      Eterm* reg = erts_proc_sched_data((p))->x_reg_array;	\
      (p)->arity = 4;						\
      reg[0] = (A0);						\
      reg[1] = (A1);						\
      reg[2] = (A2);						\
      reg[3] = (A3);						\
      (p)->i = (BeamInstr*) ((Trap_)->addressv[erts_active_code_ix()]); \
      (p)->freason = TRAP;					\
      return THE_NON_VALUE;					\
 } while(0)

#define BIF_TRAP_CODE_PTR_0(p, Code_) do {	\
      (p)->arity = 0;				\
      (p)->i = (BeamInstr*) (Code_);		\
      (p)->freason = TRAP;			\
      return THE_NON_VALUE;			\
 } while(0)

#define BIF_TRAP_CODE_PTR_(p, Code_) do {	\
      (p)-> i = (BeamInstr*) (Code_);		\
      (p)->freason = TRAP;			\
      return THE_NON_VALUE;			\
 } while(0)

extern Export bif_return_trap_export;
#define ERTS_BIF_PREP_YIELD_RETURN_X(RET, P, VAL, OP)			\
do {									\
    ERTS_VBUMP_ALL_REDS(P);						\
    ERTS_BIF_PREP_TRAP2(RET, &bif_return_trap_export, (P), (VAL), (OP));\
} while (0)

#define ERTS_BIF_PREP_YIELD_RETURN(RET, P, VAL) \
  ERTS_BIF_PREP_YIELD_RETURN_X(RET, (P), (VAL), am_undefined)

#define ERTS_BIF_YIELD_RETURN_X(P, VAL, OP)				\
do {									\
    ERTS_VBUMP_ALL_REDS(P);						\
    BIF_TRAP2(&bif_return_trap_export, (P), (VAL), (OP));		\
} while (0)

#define ERTS_BIF_RETURN_YIELD(P) ERTS_VBUMP_ALL_REDS((P))

#define ERTS_BIF_YIELD_RETURN(P, VAL) \
  ERTS_BIF_YIELD_RETURN_X((P), (VAL), am_undefined)

#define ERTS_BIF_PREP_YIELD0(RET, TRP, P)				\
do {									\
    ERTS_VBUMP_ALL_REDS((P));						\
    ERTS_BIF_PREP_TRAP0(RET, (TRP), (P));				\
} while (0)

#define ERTS_BIF_PREP_YIELD1(RET, TRP, P, A0)				\
do {									\
    ERTS_VBUMP_ALL_REDS((P));						\
    ERTS_BIF_PREP_TRAP1(RET, (TRP), (P), (A0));				\
} while (0)

#define ERTS_BIF_PREP_YIELD2(RET, TRP, P, A0, A1)			\
do {									\
    ERTS_VBUMP_ALL_REDS((P));						\
    ERTS_BIF_PREP_TRAP2(RET, (TRP), (P), (A0), (A1));			\
} while (0)

#define ERTS_BIF_PREP_YIELD3(RET, TRP, P, A0, A1, A2)			\
do {									\
    ERTS_VBUMP_ALL_REDS((P));						\
    ERTS_BIF_PREP_TRAP3(RET, (TRP), (P), (A0), (A1), (A2));		\
} while (0)

#define ERTS_BIF_PREP_YIELD4(RET, TRP, P, A0, A1, A2, A3)		\
do {									\
    ERTS_VBUMP_ALL_REDS((P));						\
    ERTS_BIF_PREP_TRAP4(RET, (TRP), (P), (A0), (A1), (A2), (A3));       \
} while (0)

#define ERTS_BIF_YIELD0(TRP, P)						\
do {									\
    ERTS_VBUMP_ALL_REDS((P));						\
    BIF_TRAP0((TRP), (P));                                              \
} while (0)

#define ERTS_BIF_YIELD1(TRP, P, A0)					\
do {									\
    ERTS_VBUMP_ALL_REDS((P));						\
    BIF_TRAP1((TRP), (P), (A0));					\
} while (0)

#define ERTS_BIF_YIELD2(TRP, P, A0, A1)					\
do {									\
    ERTS_VBUMP_ALL_REDS((P));						\
    BIF_TRAP2((TRP), (P), (A0), (A1));					\
} while (0)

#define ERTS_BIF_YIELD3(TRP, P, A0, A1, A2)				\
do {									\
    ERTS_VBUMP_ALL_REDS((P));						\
    BIF_TRAP3((TRP), (P), (A0), (A1), (A2));				\
} while (0)

#define ERTS_BIF_YIELD4(TRP, P, A0, A1, A2, A3)				\
do {									\
    ERTS_VBUMP_ALL_REDS((P));						\
    BIF_TRAP4((TRP), (P), (A0), (A1), (A2), (A3));                      \
} while (0)

#define ERTS_BIF_PREP_EXITED(RET, PROC)	                                \
do {                                                                    \
    KILL_CATCHES((PROC));                                               \
    ERTS_BIF_PREP_ERROR((RET), (PROC), EXTAG_EXIT);                     \
} while (0)

#define ERTS_BIF_EXITED(PROC)		\
do {					\
    KILL_CATCHES((PROC));		\
    BIF_ERROR((PROC), EXTAG_EXIT);	\
} while (0)

#define ERTS_BIF_CHK_EXITED(PROC)	\
do {					\
    if (ERTS_PROC_IS_EXITING((PROC)))	\
	ERTS_BIF_EXITED((PROC));	\
} while (0)

int erts_call_dirty_bif(ErtsSchedulerData *esdp, Process *c_p,
			BeamInstr *I, Eterm *reg);

BIF_RETTYPE
erts_schedule_bif(Process *proc,
		  Eterm *argv,
		  BeamInstr *i,
		  ErtsBifFunc dbf,
		  ErtsSchedType sched_type,
		  Eterm mod,
		  Eterm func,
		  int argc);

ERTS_GLB_INLINE BIF_RETTYPE
erts_reschedule_bif(Process *proc,
		    Eterm *argv,
		    BeamInstr *i,
		    ErtsBifFunc dbf,
		    ErtsSchedType sched_type);

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

ERTS_GLB_INLINE BIF_RETTYPE
erts_reschedule_bif(Process *proc,
		    Eterm *argv,
		    BeamInstr *i,
		    ErtsBifFunc dbf,
		    ErtsSchedType sched_type)
{
    return erts_schedule_bif(proc, argv, i, dbf, sched_type,
			     THE_NON_VALUE, THE_NON_VALUE, -1);
}

#endif /* ERTS_GLB_INLINE_INCL_FUNC_DEF */

#ifdef ERL_WANT_HIPE_BIF_WRAPPER__

#ifndef HIPE

#define HIPE_WRAPPER_BIF_DISABLE_GC(BIF_NAME, ARITY)

#else

#include "erl_fun.h"
#include "hipe_mode_switch.h"

/*
 * Hipe wrappers used by native code for BIFs that disable GC while trapping.
 * Also add usage of the wrapper in ../hipe/hipe_bif_list.m4
 *
 * Problem:
 * When native code calls a BIF that traps, hipe_mode_switch will push a
 * "trap frame" on the Erlang stack in order to find its way back from beam_emu
 * back to native caller when finally done. If GC is disabled and stack/heap
 * is full there is no place to push the "trap frame".
 *
 * Solution:
 * We reserve space on stack for the "trap frame" here before the BIF is called.
 * If the BIF does not trap, the space is reclaimed here before returning.
 * If the BIF traps, hipe_push_beam_trap_frame() will detect that a "trap frame"
 * already is reserved and use it.
 */


#define HIPE_WRAPPER_BIF_DISABLE_GC(BIF_NAME, ARITY)			\
BIF_RETTYPE								\
nbif_impl_hipe_wrapper_ ## BIF_NAME ## _ ## ARITY (NBIF_ALIST);		\
BIF_RETTYPE								\
nbif_impl_hipe_wrapper_ ## BIF_NAME ## _ ## ARITY (NBIF_ALIST)		\
{									\
    BIF_RETTYPE  res;							\
    hipe_reserve_beam_trap_frame(BIF_P, BIF__ARGS, ARITY);		\
    res = nbif_impl_ ## BIF_NAME ## _ ## ARITY (NBIF_CALL_ARGS);	\
    if (is_value(res) || BIF_P->freason != TRAP) {			\
	hipe_unreserve_beam_trap_frame(BIF_P);				\
    }									\
    return res;								\
}

#endif

#endif /* ERL_WANT_HIPE_BIF_WRAPPER__ */

#include "erl_bif_table.h"

#endif
