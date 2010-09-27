/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1996-2010. All Rights Reserved.
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

#ifndef __BIF_H__
#define __BIF_H__

extern Export* erts_format_cpu_topology_trap;

#define BIF_RETTYPE Eterm

#define BIF_P A__p

#define BIF_ALIST_0 Process* A__p
#define BIF_ALIST_1 Process* A__p, Eterm A_1
#define BIF_ALIST_2 Process* A__p, Eterm A_1, Eterm A_2
#define BIF_ALIST_3 Process* A__p, Eterm A_1, Eterm A_2, Eterm A_3

#define BIF_ARG_1  A_1
#define BIF_ARG_2  A_2
#define BIF_ARG_3  A_3

#define BUMP_ALL_REDS(p) do {			\
    if (!ERTS_PROC_GET_SAVED_CALLS_BUF((p))) 	\
	(p)->fcalls = 0; 			\
    else 					\
	(p)->fcalls = -CONTEXT_REDS;		\
} while(0)


#define ERTS_VBUMP_ALL_REDS(p)						\
do {									\
    if (!ERTS_PROC_GET_SAVED_CALLS_BUF((p))) {				\
	if ((p)->fcalls > 0)						\
	    ERTS_PROC_GET_SCHDATA((p))->virtual_reds += (p)->fcalls;	\
	(p)->fcalls = 0;						\
    }									\
    else {								\
	if ((p)->fcalls > -CONTEXT_REDS)				\
	    ERTS_PROC_GET_SCHDATA((p))->virtual_reds			\
		+= ((p)->fcalls - (-CONTEXT_REDS));			\
	(p)->fcalls = -CONTEXT_REDS;					\
    }									\
} while(0)

#define BUMP_REDS(p, gc) do {			   \
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
	    ERTS_PROC_GET_SCHDATA((p))->virtual_reds += reds;		\
	}								\
	else {								\
	    if ((p)->fcalls > 0)					\
		ERTS_PROC_GET_SCHDATA((p))->virtual_reds += (p)->fcalls;\
	    (p)->fcalls = 0;						\
	}								\
    }									\
    else {								\
	if ((p)->fcalls >= reds - CONTEXT_REDS) {			\
	    (p)->fcalls -= reds;					\
	    ERTS_PROC_GET_SCHDATA((p))->virtual_reds += reds;		\
	}								\
	else {								\
	    if ((p)->fcalls > -CONTEXT_REDS)				\
		ERTS_PROC_GET_SCHDATA((p))->virtual_reds		\
		    += (p)->fcalls - (-CONTEXT_REDS);			\
	    (p)->fcalls = -CONTEXT_REDS;				\
	}								\
    }									\
} while(0)

#define ERTS_BIF_REDS_LEFT(p)						\
  (ERTS_PROC_GET_SAVED_CALLS_BUF((p))					\
   ? ((p)->fcalls > -CONTEXT_REDS ? ((p)->fcalls - (-CONTEXT_REDS)) : 0)\
   : ((p)->fcalls > 0 ? (p)->fcalls : 0))

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

#define ERTS_BIF_PREP_ERROR(Ret, Proc, Reason)	\
do {						\
    (Proc)->freason = (Reason);			\
    (Ret) = THE_NON_VALUE;			\
} while (0)


#define ERTS_BIF_PREP_TRAP0(Ret, Trap, Proc)		\
do {							\
    (Proc)->arity = 0;					\
    *((UWord *) (UWord) ((Proc)->def_arg_reg + 3)) = (UWord) ((Trap)->address);	\
    (Proc)->freason = TRAP;				\
    (Ret) = THE_NON_VALUE;				\
} while (0)

#define ERTS_BIF_PREP_TRAP1(Ret, Trap, Proc, A0)	\
do {							\
    (Proc)->arity = 1;					\
    (Proc)->def_arg_reg[0] = (Eterm) (A0);		\
    *((UWord *) (UWord) ((Proc)->def_arg_reg + 3)) = (UWord) ((Trap)->address);	\
    (Proc)->freason = TRAP;				\
    (Ret) = THE_NON_VALUE;				\
} while (0)

#define ERTS_BIF_PREP_TRAP2(Ret, Trap, Proc, A0, A1)	\
do {							\
    (Proc)->arity = 2;					\
    (Proc)->def_arg_reg[0] = (Eterm) (A0);		\
    (Proc)->def_arg_reg[1] = (Eterm) (A1);		\
    *((UWord *) (UWord) ((Proc)->def_arg_reg + 3)) = (UWord) ((Trap)->address);	\
    (Proc)->freason = TRAP;				\
    (Ret) = THE_NON_VALUE;				\
} while (0)

#define ERTS_BIF_PREP_TRAP3(Ret, Trap, Proc, A0, A1, A2)\
do {							\
    (Proc)->arity = 3;					\
    (Proc)->def_arg_reg[0] = (Eterm) (A0);		\
    (Proc)->def_arg_reg[1] = (Eterm) (A1);		\
    (Proc)->def_arg_reg[2] = (Eterm) (A2);		\
    *((UWord *) (UWord) ((Proc)->def_arg_reg + 3)) = (UWord) ((Trap)->address);	\
    (Proc)->freason = TRAP;				\
    (Ret) = THE_NON_VALUE;				\
} while (0)

#define BIF_TRAP0(p, Trap_) do {			\
      (p)->arity = 0;					\
      *((UWord *) (UWord) ((p)->def_arg_reg + 3)) = (UWord) ((Trap_)->address);	\
      (p)->freason = TRAP;				\
      return THE_NON_VALUE;				\
 } while(0)

#define BIF_TRAP1(Trap_, p, A0) do {			\
      (p)->arity = 1;					\
      (p)->def_arg_reg[0] = (A0);			\
      *((UWord *) (UWord) ((p)->def_arg_reg + 3)) = (UWord) ((Trap_)->address);	\
      (p)->freason = TRAP;				\
      return THE_NON_VALUE;				\
 } while(0)

#define BIF_TRAP2(Trap_, p, A0, A1) do {		\
      (p)->arity = 2;					\
      (p)->def_arg_reg[0] = (A0);			\
      (p)->def_arg_reg[1] = (A1);			\
      *((UWord *) (UWord) ((p)->def_arg_reg + 3)) = (UWord) ((Trap_)->address);	\
      (p)->freason = TRAP;				\
      return THE_NON_VALUE;				\
 } while(0)

#define BIF_TRAP3(Trap_, p, A0, A1, A2) do {		\
      (p)->arity = 3;					\
      (p)->def_arg_reg[0] = (A0);			\
      (p)->def_arg_reg[1] = (A1);			\
      (p)->def_arg_reg[2] = (A2);			\
      *((UWord *) (UWord) ((p)->def_arg_reg + 3)) = (UWord) ((Trap_)->address);	\
      (p)->freason = TRAP;				\
      return THE_NON_VALUE;				\
 } while(0)

#define BIF_TRAP_CODE_PTR_0(p, Code_) do {		\
      (p)->arity = 0;					\
      *((UWord *) (UWord) ((p)->def_arg_reg + 3)) = (UWord) (Code_);	\
      (p)->freason = TRAP;				\
      return THE_NON_VALUE;				\
 } while(0)

#define BIF_TRAP_CODE_PTR_(p, Code_) do {		\
      *((UWord *) (UWord) ((p)->def_arg_reg + 3)) = (UWord) (Code_);	\
      (p)->freason = TRAP;				\
      return THE_NON_VALUE;				\
 } while(0)

extern Export bif_return_trap_export;
#ifdef DEBUG
#define ERTS_BIF_PREP_YIELD_RETURN_X(RET, P, VAL, DEBUG_VAL)		\
do {									\
    ERTS_VBUMP_ALL_REDS(P);						\
    ERTS_BIF_PREP_TRAP2(RET, &bif_return_trap_export, (P), (VAL),	\
			(DEBUG_VAL));					\
} while (0)
#else
#define ERTS_BIF_PREP_YIELD_RETURN_X(RET, P, VAL, DEBUG_VAL)		\
do {									\
    ERTS_VBUMP_ALL_REDS(P);						\
    ERTS_BIF_PREP_TRAP1(RET, &bif_return_trap_export, (P), (VAL));	\
} while (0)
#endif

#define ERTS_BIF_PREP_YIELD_RETURN(RET, P, VAL) \
  ERTS_BIF_PREP_YIELD_RETURN_X(RET, (P), (VAL), am_undefined)

#ifdef DEBUG
#define ERTS_BIF_YIELD_RETURN_X(P, VAL, DEBUG_VAL)			\
do {									\
    ERTS_VBUMP_ALL_REDS(P);						\
    BIF_TRAP2(&bif_return_trap_export, (P), (VAL), (DEBUG_VAL));	\
} while (0)
#else
#define ERTS_BIF_YIELD_RETURN_X(P, VAL, DEBUG_VAL)			\
do {									\
    ERTS_VBUMP_ALL_REDS(P);						\
    BIF_TRAP1(&bif_return_trap_export, (P), (VAL));			\
} while (0)
#endif

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

#define ERTS_BIF_YIELD0(TRP, P)						\
do {									\
    ERTS_VBUMP_ALL_REDS((P));						\
    BIF_TRAP0((TRP), (P));						\
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

#define ERTS_BIF_EXITED(PROC)		\
do {					\
    KILL_CATCHES((PROC));		\
    BIF_ERROR((PROC), EXC_EXIT);	\
} while (0)

#define ERTS_BIF_CHK_EXITED(PROC)	\
do {					\
    if (ERTS_PROC_IS_EXITING((PROC)))	\
	ERTS_BIF_EXITED((PROC));	\
} while (0)

#ifdef ERTS_SMP
#define ERTS_SMP_BIF_CHK_PENDING_EXIT(P, L)				\
do {									\
    ERTS_SMP_LC_ASSERT((L) == erts_proc_lc_my_proc_locks((P)));		\
    ERTS_SMP_LC_ASSERT(ERTS_PROC_LOCK_MAIN & (L));			\
    if (!((L) & ERTS_PROC_LOCK_STATUS))					\
	erts_smp_proc_lock((P), ERTS_PROC_LOCK_STATUS);			\
    if (ERTS_PROC_PENDING_EXIT((P))) {					\
	erts_handle_pending_exit((P), (L)|ERTS_PROC_LOCK_STATUS);	\
	erts_smp_proc_unlock((P),					\
			     (((L)|ERTS_PROC_LOCK_STATUS)		\
			      & ~ERTS_PROC_LOCK_MAIN));			\
	ERTS_BIF_EXITED((P));						\
    }									\
    if (!((L) & ERTS_PROC_LOCK_STATUS))					\
	erts_smp_proc_unlock((P), ERTS_PROC_LOCK_STATUS);		\
} while (0)
#else
#define ERTS_SMP_BIF_CHK_PENDING_EXIT(P, L)
#endif

/*
 * The ERTS_BIF_*_AWAIT_X_*_TRAP makros either exits the caller, or
 * sets up a trap to erlang:await_proc_exit/3.
 *
 * The caller is acquired to hold the 'main' lock on C_P. No other locks
 * are allowed to be held.
 */

#define ERTS_BIF_PREP_AWAIT_X_DATA_TRAP(RET, C_P, PID, DATA)		\
do {									\
    erts_bif_prep_await_proc_exit_data_trap((C_P), (PID), (DATA));	\
    (RET) = THE_NON_VALUE;						\
} while (0)

#define ERTS_BIF_PREP_AWAIT_X_REASON_TRAP(RET, C_P, PID)		\
do {									\
    erts_bif_prep_await_proc_exit_reason_trap((C_P), (PID));		\
    (RET) = THE_NON_VALUE;						\
} while (0)

#define ERTS_BIF_PREP_AWAIT_X_APPLY_TRAP(RET, C_P, PID, M, F, A, AN)	\
do {									\
    erts_bif_prep_await_proc_exit_apply_trap((C_P), (PID),		\
					     (M), (F), (A), (AN));	\
    (RET) = THE_NON_VALUE;						\
} while (0)

#define ERTS_BIF_AWAIT_X_DATA_TRAP(C_P, PID, DATA)			\
do {									\
    erts_bif_prep_await_proc_exit_data_trap((C_P), (PID), (DATA));	\
    return THE_NON_VALUE;						\
} while (0)

#define ERTS_BIF_AWAIT_X_REASON_TRAP(C_P, PID)				\
do {									\
    erts_bif_prep_await_proc_exit_reason_trap((C_P), (PID));		\
    return THE_NON_VALUE;						\
} while (0)

#define ERTS_BIF_AWAIT_X_APPLY_TRAP(C_P, PID, M, F, A, AN)		\
do {									\
    erts_bif_prep_await_proc_exit_apply_trap((C_P), (PID),		\
					     (M), (F), (A), (AN));	\
    return THE_NON_VALUE;						\
} while (0)

void
erts_bif_prep_await_proc_exit_data_trap(Process *c_p,
					Eterm pid,
					Eterm data);
void
erts_bif_prep_await_proc_exit_reason_trap(Process *c_p,
					  Eterm pid);
void
erts_bif_prep_await_proc_exit_apply_trap(Process *c_p,
					 Eterm pid,
					 Eterm module,
					 Eterm function,
					 Eterm args[],
					 int nargs);

#include "erl_bif_table.h"

#endif
