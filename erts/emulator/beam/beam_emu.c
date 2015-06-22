/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1996-2014. All Rights Reserved.
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

#include <stddef.h> /* offsetof() */
#include "sys.h"
#include "erl_vm.h"
#include "global.h"
#include "erl_process.h"
#include "error.h"
#include "bif.h"
#include "big.h"
#include "beam_load.h"
#include "erl_binary.h"
#include "erl_map.h"
#include "erl_bits.h"
#include "dist.h"
#include "beam_bp.h"
#include "beam_catches.h"
#include "erl_thr_progress.h"
#ifdef HIPE
#include "hipe_mode_switch.h"
#include "hipe_bif1.h"
#endif
#include "dtrace-wrapper.h"

/* #define HARDDEBUG 1 */

#if defined(NO_JUMP_TABLE)
#  define OpCase(OpCode)    case op_##OpCode
#  define CountCase(OpCode) case op_count_##OpCode
#  define OpCode(OpCode)    ((Uint*)op_##OpCode)
#  define Goto(Rel) {Go = (int)(UWord)(Rel); goto emulator_loop;}
#  define LabelAddr(Addr) &&##Addr
#else
#  define OpCase(OpCode)    lb_##OpCode
#  define CountCase(OpCode) lb_count_##OpCode
#  define Goto(Rel) goto *((void *)Rel)
#  define LabelAddr(Label) &&Label
#  define OpCode(OpCode)  (&&lb_##OpCode)
#endif

#ifdef ERTS_ENABLE_LOCK_CHECK
#  ifdef ERTS_SMP
#    define PROCESS_MAIN_CHK_LOCKS(P)					\
do {									\
    if ((P)) {								\
	erts_proc_lc_chk_only_proc_main((P));				\
    }									\
    else								\
	erts_lc_check_exact(NULL, 0);					\
    	ERTS_SMP_LC_ASSERT(!erts_thr_progress_is_blocking());		\
} while (0)
#    define ERTS_SMP_REQ_PROC_MAIN_LOCK(P) \
        if ((P)) erts_proc_lc_require_lock((P), ERTS_PROC_LOCK_MAIN,\
					   __FILE__, __LINE__)
#    define ERTS_SMP_UNREQ_PROC_MAIN_LOCK(P) \
        if ((P)) erts_proc_lc_unrequire_lock((P), ERTS_PROC_LOCK_MAIN)
#  else
#    define ERTS_SMP_REQ_PROC_MAIN_LOCK(P)
#    define ERTS_SMP_UNREQ_PROC_MAIN_LOCK(P)
#    define PROCESS_MAIN_CHK_LOCKS(P) erts_lc_check_exact(NULL, 0)
#  endif
#else
#  define PROCESS_MAIN_CHK_LOCKS(P)
#  define ERTS_SMP_REQ_PROC_MAIN_LOCK(P)
#  define ERTS_SMP_UNREQ_PROC_MAIN_LOCK(P)
#endif

/*
 * Define macros for deep checking of terms.
 */

#if defined(HARDDEBUG)

#  define CHECK_TERM(T) size_object(T)

#  define CHECK_ARGS(PC)                 \
do {                                     \
  int i_;                                \
  int Arity_ = PC[-1];                   \
  if (Arity_ > 0) {                      \
	CHECK_TERM(r(0));                \
  }                                      \
  for (i_ = 1; i_ < Arity_; i_++) {      \
	CHECK_TERM(x(i_));               \
  }                                      \
} while (0)
    
#else
#  define CHECK_TERM(T) ASSERT(!is_CP(T))
#  define CHECK_ARGS(T)
#endif

#ifndef MAX
#define MAX(x, y) (((x) > (y)) ? (x) : (y))
#endif

#define GET_BIF_ADDRESS(p) ((BifFunction) (((Export *) p)->code[4]))
#define TermWords(t) (((t) / (sizeof(BeamInstr)/sizeof(Eterm))) + !!((t) % (sizeof(BeamInstr)/sizeof(Eterm))))


/*
 * We reuse some of fields in the save area in the process structure.
 * This is safe to do, since this space is only activly used when
 * the process is switched out.
 */
#define REDS_IN(p)  ((p)->def_arg_reg[5])

/*
 * Add a byte offset to a pointer to Eterm.  This is useful when the
 * the loader has precalculated a byte offset.
 */
#define ADD_BYTE_OFFSET(ptr, offset) \
   ((Eterm *) (((unsigned char *)ptr) + (offset)))

/* We don't check the range if an ordinary switch is used */
#ifdef NO_JUMP_TABLE
#define VALID_INSTR(IP) ((UWord)(IP) < (NUMBER_OF_OPCODES*2+10))
#else
#define VALID_INSTR(IP) \
   ((SWord)LabelAddr(emulator_loop) <= (SWord)(IP) && \
    (SWord)(IP) < (SWord)LabelAddr(end_emulator_loop))
#endif /* NO_JUMP_TABLE */

#define SET_CP(p, ip)           \
   ASSERT(VALID_INSTR(*(ip)));  \
   (p)->cp = (ip)

#define SET_I(ip) \
   ASSERT(VALID_INSTR(* (Eterm *)(ip))); \
   I = (ip)

#define FetchArgs(S1, S2) tmp_arg1 = (S1); tmp_arg2 = (S2)

/*
 * Store a result into a register given a destination descriptor.
 */

#define StoreResult(Result, DestDesc)               \
  do {                                              \
    Eterm stb_reg;                                  \
    stb_reg = (DestDesc);                           \
    CHECK_TERM(Result);                             \
    switch (beam_reg_tag(stb_reg)) {                \
    case R_REG_DEF:                                 \
      r(0) = (Result); break;                       \
    case X_REG_DEF:                                 \
      xb(x_reg_offset(stb_reg)) = (Result); break;  \
    default:                                        \
      yb(y_reg_offset(stb_reg)) = (Result); break;  \
    }                                               \
  } while (0)

#define StoreSimpleDest(Src, Dest) Dest = (Src)

/*
 * Store a result into a register and execute the next instruction.
 * Dst points to the word with a destination descriptor, which MUST
 * be just before the next instruction.
 */
 
#define StoreBifResult(Dst, Result)                          \
  do {                                                       \
    BeamInstr* stb_next;                                         \
    Eterm stb_reg;                                           \
    stb_reg = Arg(Dst);                                      \
    I += (Dst) + 2;                                          \
    stb_next = (BeamInstr *) *I;                                 \
    CHECK_TERM(Result);                                      \
    switch (beam_reg_tag(stb_reg)) {                         \
    case R_REG_DEF:                                          \
      r(0) = (Result); Goto(stb_next);                       \
    case X_REG_DEF:                                          \
      xb(x_reg_offset(stb_reg)) = (Result); Goto(stb_next);  \
    default:                                                 \
      yb(y_reg_offset(stb_reg)) = (Result); Goto(stb_next);  \
    }                                                        \
  } while (0)

#define ClauseFail() goto jump_f

#define SAVE_CP(X)				\
   do {						\
      *(X) = make_cp(c_p->cp);			\
      c_p->cp = 0;				\
   } while(0)

#define RESTORE_CP(X)		SET_CP(c_p, (BeamInstr *) cp_val(*(X)))

#define ISCATCHEND(instr) ((Eterm *) *(instr) == OpCode(catch_end_y))

/*
 * Special Beam instructions.
 */

BeamInstr beam_apply[2];
BeamInstr beam_exit[1];
BeamInstr beam_continue_exit[1];

BeamInstr* em_call_error_handler;
BeamInstr* em_apply_bif;
BeamInstr* em_call_nif;


/* NOTE These should be the only variables containing trace instructions.
**      Sometimes tests are form the instruction value, and sometimes
**      for the refering variable (one of these), and rouge references
**      will most likely cause chaos.
*/
BeamInstr beam_return_to_trace[1];   /* OpCode(i_return_to_trace) */
BeamInstr beam_return_trace[1];      /* OpCode(i_return_trace) */
BeamInstr beam_exception_trace[1];   /* UGLY also OpCode(i_return_trace) */
BeamInstr beam_return_time_trace[1]; /* OpCode(i_return_time_trace) */


/*
 * All Beam instructions in numerical order.
 */

#ifndef NO_JUMP_TABLE
void** beam_ops;
#endif

#define SWAPIN             \
    HTOP = HEAP_TOP(c_p);  \
    E = c_p->stop

#define SWAPOUT            \
    HEAP_TOP(c_p) = HTOP;  \
    c_p->stop = E

/*
 * Use LIGHT_SWAPOUT when the called function
 * will call HeapOnlyAlloc() (and never HAlloc()).
 */
#ifdef DEBUG
#  /* The stack pointer is used in an assertion. */
#  define LIGHT_SWAPOUT SWAPOUT
#else
#  define LIGHT_SWAPOUT HEAP_TOP(c_p) = HTOP
#endif

/*
 * Use LIGHT_SWAPIN when we know that c_p->stop cannot
 * have been updated (i.e. if there cannot have been
 * a garbage-collection).
 */

#define LIGHT_SWAPIN HTOP = HEAP_TOP(c_p)

#ifdef FORCE_HEAP_FRAGS
#  define HEAP_SPACE_VERIFIED(Words) do { \
      c_p->space_verified = (Words);	  \
      c_p->space_verified_from = HTOP;	  \
    }while(0)
#else
#  define HEAP_SPACE_VERIFIED(Words) ((void)0)
#endif

#define PRE_BIF_SWAPOUT(P)						\
     HEAP_TOP((P)) = HTOP;  						\
     (P)->stop = E;  							\
     PROCESS_MAIN_CHK_LOCKS((P));					\
     ERTS_SMP_UNREQ_PROC_MAIN_LOCK((P))

#define db(N) (N)
#define tb(N) (N)
#define xb(N) (*(Eterm *) (((unsigned char *)reg) + (N)))
#define yb(N) (*(Eterm *) (((unsigned char *)E) + (N)))
#define fb(N) (*(double *) (((unsigned char *)&(freg[0].fd)) + (N)))
#define Qb(N) (N)
#define Ib(N) (N)
#define x(N) reg[N]
#define y(N) E[N]
#define r(N) x##N

/*
 * Makes sure that there are StackNeed + HeapNeed + 1 words available
 * on the combined heap/stack segment, then allocates StackNeed + 1
 * words on the stack and saves CP.
 *
 * M is number of live registers to preserve during garbage collection
 */

#define AH(StackNeed, HeapNeed, M) \
  do { \
     int needed; \
     needed = (StackNeed) + 1; \
     if (E - HTOP < (needed + (HeapNeed))) { \
           SWAPOUT; \
           reg[0] = r(0); \
           PROCESS_MAIN_CHK_LOCKS(c_p); \
           FCALLS -= erts_garbage_collect(c_p, needed + (HeapNeed), reg, (M)); \
           ERTS_VERIFY_UNUSED_TEMP_ALLOC(c_p); \
           PROCESS_MAIN_CHK_LOCKS(c_p); \
           r(0) = reg[0]; \
           SWAPIN; \
     } \
     E -= needed; \
     SAVE_CP(E); \
  } while (0)

#define Allocate(Ns, Live) AH(Ns, 0, Live)

#define AllocateZero(Ns, Live)             \
 do { Eterm* ptr;                          \
      int i = (Ns);                        \
      AH(i, 0, Live);                      \
      for (ptr = E + i; ptr > E; ptr--) {  \
	 make_blank(*ptr);                 \
     }                                     \
  } while (0)

#define AllocateHeap(Ns, Nh, Live) AH(Ns, Nh, Live)

#define AllocateHeapZero(Ns, Nh, Live)     \
 do { Eterm* ptr;                          \
      int i = (Ns);                        \
      AH(i, Nh, Live);                     \
      for (ptr = E + i; ptr > E; ptr--) {  \
	 make_blank(*ptr);                 \
     }                                     \
  } while (0)

#define AllocateInit(Ns, Live, Y) \
  do { AH(Ns, 0, Live); make_blank(Y); } while (0)

/*
 * Like the AH macro, but allocates no additional heap space.
 */

#define A(StackNeed, M) AH(StackNeed, 0, M)

#define D(N)             \
     RESTORE_CP(E);      \
     E += (N) + 1;



#define TestBinVHeap(VNh, Nh, Live)                             		\
  do {                                                          		\
    unsigned need = (Nh);                                       		\
    if ((E - HTOP < need) || (MSO(c_p).overhead + (VNh) >= BIN_VHEAP_SZ(c_p))) {\
       SWAPOUT;                                                 		\
       reg[0] = r(0);                                           		\
       PROCESS_MAIN_CHK_LOCKS(c_p);                             		\
       FCALLS -= erts_garbage_collect(c_p, need, reg, (Live));  		\
       ERTS_VERIFY_UNUSED_TEMP_ALLOC(c_p);					\
       PROCESS_MAIN_CHK_LOCKS(c_p);                             		\
       r(0) = reg[0];                                           		\
       SWAPIN;                                                  		\
    }                                                           		\
    HEAP_SPACE_VERIFIED(need);                                                  \
  } while (0)



/*
 * Check if Nh words of heap are available; if not, do a garbage collection.
 * Live is number of active argument registers to be preserved.
 */

#define TestHeap(Nh, Live)                                      \
  do {                                                          \
    unsigned need = (Nh);                                       \
    if (E - HTOP < need) {                                      \
       SWAPOUT;                                                 \
       reg[0] = r(0);                                           \
       PROCESS_MAIN_CHK_LOCKS(c_p);                             \
       FCALLS -= erts_garbage_collect(c_p, need, reg, (Live));  \
       ERTS_VERIFY_UNUSED_TEMP_ALLOC(c_p);			\
       PROCESS_MAIN_CHK_LOCKS(c_p);                             \
       r(0) = reg[0];                                           \
       SWAPIN;                                                  \
    }                                                           \
    HEAP_SPACE_VERIFIED(need);                             \
  } while (0)

/*
 * Check if Nh words of heap are available; if not, do a garbage collection.
 * Live is number of active argument registers to be preserved.
 * Takes special care to preserve Extra if a garbage collection occurs.
 */

#define TestHeapPreserve(Nh, Live, Extra)				\
  do {									\
    unsigned need = (Nh);						\
    if (E - HTOP < need) {						\
       SWAPOUT;								\
       reg[0] = r(0);							\
       reg[Live] = Extra;						\
       PROCESS_MAIN_CHK_LOCKS(c_p);					\
       FCALLS -= erts_garbage_collect(c_p, need, reg, (Live)+1);	\
       ERTS_VERIFY_UNUSED_TEMP_ALLOC(c_p);				\
       PROCESS_MAIN_CHK_LOCKS(c_p);					\
       if (Live > 0) {							\
	   r(0) = reg[0];						\
       }								\
       Extra = reg[Live];						\
       SWAPIN;								\
    }									\
    HEAP_SPACE_VERIFIED(need);                                      \
  } while (0)

#define TestHeapPutList(Need, Reg)		\
  do {						\
     TestHeap((Need), 1);			\
     PutList(Reg, r(0), r(0), StoreSimpleDest);	\
     CHECK_TERM(r(0));				\
  } while (0)

#define Init(N) make_blank(yb(N))

#define Init2(Y1, Y2) do { make_blank(Y1); make_blank(Y2); } while (0)
#define Init3(Y1, Y2, Y3) \
   do { make_blank(Y1); make_blank(Y2); make_blank(Y3); } while (0)

#define MakeFun(FunP, NumFree)					\
  do {								\
     SWAPOUT;							\
     reg[0] = r(0);						\
     r(0) = new_fun(c_p, reg, (ErlFunEntry *) FunP, NumFree);	\
     SWAPIN;							\
  } while (0)

#define PutTuple(Dst, Arity)			\
 do {						\
   Dst = make_tuple(HTOP);			\
   pt_arity = (Arity);				\
 } while (0)

/*
 * Check that we haven't used the reductions and jump to function pointed to by
 * the I register.  If we are out of reductions, do a context switch.
 */

#define DispatchMacro()				\
  do {						\
     BeamInstr* dis_next;				\
     dis_next = (BeamInstr *) *I;			\
     CHECK_ARGS(I);				\
     if (FCALLS > 0 || FCALLS > neg_o_reds) {	\
        FCALLS--;				\
        Goto(dis_next);				\
     } else {					\
	goto context_switch;			\
     }						\
 } while (0)

#define DispatchMacroFun()			\
  do {						\
     BeamInstr* dis_next;				\
     dis_next = (BeamInstr *) *I;			\
     CHECK_ARGS(I);				\
     if (FCALLS > 0 || FCALLS > neg_o_reds) {	\
        FCALLS--;				\
        Goto(dis_next);				\
     } else {					\
	goto context_switch_fun;		\
     }						\
 } while (0)

#define DispatchMacrox()					\
  do {								\
     if (FCALLS > 0) {						\
        Eterm* dis_next;					\
        SET_I(((Export *) Arg(0))->addressv[erts_active_code_ix()]); \
        dis_next = (Eterm *) *I;				\
        FCALLS--;						\
        CHECK_ARGS(I);						\
        Goto(dis_next);						\
     } else if (ERTS_PROC_GET_SAVED_CALLS_BUF(c_p)		\
		&& FCALLS > neg_o_reds) {			\
        goto save_calls1;					\
     } else {							\
        SET_I(((Export *) Arg(0))->addressv[erts_active_code_ix()]); \
        CHECK_ARGS(I);						\
	goto context_switch;					\
     }								\
 } while (0)

#ifdef DEBUG
/*
 * To simplify breakpoint setting, put the code in one place only and jump to it.
 */
#  define Dispatch() goto do_dispatch
#  define Dispatchx() goto do_dispatchx
#  define Dispatchfun() goto do_dispatchfun
#else
/*
 * Inline for speed.
 */
#  define Dispatch() DispatchMacro()
#  define Dispatchx() DispatchMacrox()
#  define Dispatchfun() DispatchMacroFun()
#endif

#define Self(R) R = c_p->common.id
#define Node(R) R = erts_this_node->sysname

#define Arg(N)       I[(N)+1]
#define Next(N)                \
    I += (N) + 1;              \
    ASSERT(VALID_INSTR(*I));   \
    Goto(*I)

#define PreFetch(N, Dst) do { Dst = (BeamInstr *) *(I + N + 1); } while (0)
#define NextPF(N, Dst)         \
    I += N + 1;                \
    ASSERT(VALID_INSTR(Dst));  \
    Goto(Dst)

#define GetR(pos, tr) \
   do { \
     tr = Arg(pos); \
     switch (beam_reg_tag(tr)) { \
     case R_REG_DEF: tr = r(0); break; \
     case X_REG_DEF: tr = xb(x_reg_offset(tr)); break; \
     case Y_REG_DEF: ASSERT(y_reg_offset(tr) >= 1); tr = yb(y_reg_offset(tr)); break; \
     } \
     CHECK_TERM(tr); \
   } while (0)

#define GetArg1(N, Dst) GetR((N), Dst)

#define GetArg2(N, Dst1, Dst2)     \
   do {                            \
     GetR(N, Dst1);                \
     GetR((N)+1, Dst2);            \
   } while (0)

#define PutList(H, T, Dst, Store)  \
  do {                             \
   HTOP[0] = (H); HTOP[1] = (T);   \
   Store(make_list(HTOP), Dst);    \
   HTOP += 2;                      \
  } while (0)

#define Move(Src, Dst, Store)      \
   do {                            \
       Eterm term = (Src);         \
       Store(term, Dst);           \
   } while (0)

#define Move2(S1, D1, S2, D2) D1 = (S1); D2 = (S2)
#define Move3(S1, D1, S2, D2, S3, D3) D1 = (S1); D2 = (S2); D3 = (S3)

#define MoveGenDest(src, dstp) \
   if ((dstp) == NULL) { r(0) = (src); } else { *(dstp) = src; }

#define MoveReturn(Src, Dest)       \
    (Dest) = (Src);                 \
    I = c_p->cp;                    \
    ASSERT(VALID_INSTR(*c_p->cp));  \
    c_p->cp = 0;                    \
    CHECK_TERM(r(0));               \
    Goto(*I)

#define DeallocateReturn(Deallocate)       \
  do {                                     \
    int words_to_pop = (Deallocate);       \
    SET_I((BeamInstr *) cp_val(*E));                     \
    E = ADD_BYTE_OFFSET(E, words_to_pop);  \
    CHECK_TERM(r(0));                      \
    Goto(*I);                              \
  } while (0)

#define MoveDeallocateReturn(Src, Dest, Deallocate)  \
    (Dest) = (Src);                                  \
    DeallocateReturn(Deallocate)

#define MoveCall(Src, Dest, CallDest, Size)	\
    (Dest) = (Src);				\
    SET_CP(c_p, I+Size+1);			\
    SET_I((BeamInstr *) CallDest);			\
    Dispatch();

#define MoveCallLast(Src, Dest, CallDest, Deallocate)	\
    (Dest) = (Src);					\
    RESTORE_CP(E);					\
    E = ADD_BYTE_OFFSET(E, (Deallocate));		\
    SET_I((BeamInstr *) CallDest);				\
    Dispatch();

#define MoveCallOnly(Src, Dest, CallDest)	\
    (Dest) = (Src);				\
    SET_I((BeamInstr *) CallDest);			\
    Dispatch();

#define MoveJump(Src)				\
     r(0) = (Src);				\
     SET_I((BeamInstr *) Arg(0));		\
     Goto(*I);

#define GetList(Src, H, T) do {			\
   Eterm* tmp_ptr = list_val(Src);		\
   H = CAR(tmp_ptr);				\
   T = CDR(tmp_ptr); } while (0)

#define GetTupleElement(Src, Element, Dest)				\
  do {									\
    tmp_arg1 = (Eterm) COMPRESS_POINTER(((unsigned char *) tuple_val(Src)) + 	\
				(Element));				\
    (Dest) = (*(Eterm *) EXPAND_POINTER(tmp_arg1));			\
  } while (0)

#define ExtractNextElement(Dest)					  \
    tmp_arg1 += sizeof(Eterm);						  \
    (Dest) = (* (Eterm *) (((unsigned char *) EXPAND_POINTER(tmp_arg1))))

#define ExtractNextElement2(Dest)				\
  do {								\
    Eterm* ene_dstp = &(Dest);					\
    ene_dstp[0] = ((Eterm *) EXPAND_POINTER(tmp_arg1))[1];	\
    ene_dstp[1] = ((Eterm *) EXPAND_POINTER(tmp_arg1))[2];	\
    tmp_arg1 += sizeof(Eterm) + sizeof(Eterm);			\
  } while (0)

#define ExtractNextElement3(Dest)		\
  do {						\
    Eterm* ene_dstp = &(Dest);			\
    ene_dstp[0] = ((Eterm *) EXPAND_POINTER(tmp_arg1))[1];	\
    ene_dstp[1] = ((Eterm *) EXPAND_POINTER(tmp_arg1))[2];	\
    ene_dstp[2] = ((Eterm *) EXPAND_POINTER(tmp_arg1))[3];	\
    tmp_arg1 += 3*sizeof(Eterm);		\
  } while (0)

#define ExtractNextElement4(Dest)		\
  do {						\
    Eterm* ene_dstp = &(Dest);			\
    ene_dstp[0] = ((Eterm *) EXPAND_POINTER(tmp_arg1))[1];	\
    ene_dstp[1] = ((Eterm *) EXPAND_POINTER(tmp_arg1))[2];	\
    ene_dstp[2] = ((Eterm *) EXPAND_POINTER(tmp_arg1))[3];	\
    ene_dstp[3] = ((Eterm *) EXPAND_POINTER(tmp_arg1))[4];	\
    tmp_arg1 += 4*sizeof(Eterm);		\
  } while (0)

#define ExtractElement(Element, Dest)		\
  do {						\
     tmp_arg1 += (Element);			\
     (Dest) = (* (Eterm *) EXPAND_POINTER(tmp_arg1));		\
  } while (0)

#define EqualImmed(X, Y, Action) if (X != Y) { Action; }
#define NotEqualImmed(X, Y, Action) if (X == Y) { Action; }
#define EqualExact(X, Y, Action) if (!EQ(X,Y)) { Action; }
#define IsLessThan(X, Y, Action) if (CMP_GE(X, Y)) { Action; }
#define IsGreaterEqual(X, Y, Action) if (CMP_LT(X, Y)) { Action; }

#define IsFloat(Src, Fail) if (is_not_float(Src)) { Fail; }

#define IsInteger(Src, Fail) if (is_not_integer(Src)) { Fail; }

#define IsNumber(X, Fail) if (is_not_integer(X) && is_not_float(X)) { Fail; }

#define IsAtom(Src, Fail) if (is_not_atom(Src)) { Fail; }

#define IsIntegerAllocate(Src, Need, Alive, Fail)  \
    if (is_not_integer(Src)) { Fail; }             \
    A(Need, Alive)

#define IsNil(Src, Fail) if (is_not_nil(Src)) { Fail; }

#define IsList(Src, Fail) if (is_not_list(Src) && is_not_nil(Src)) { Fail; }

#define IsNonemptyList(Src, Fail) if (is_not_list(Src)) { Fail; }

#define IsNonemptyListAllocate(Src, Need, Alive, Fail)  \
    if (is_not_list(Src)) { Fail; }                     \
    A(Need, Alive)

#define IsNonemptyListTestHeap(Src, Need, Alive, Fail)  \
    if (is_not_list(Src)) { Fail; }                     \
    TestHeap(Need, Alive)

#define IsTuple(X, Action) if (is_not_tuple(X)) Action

#define IsArity(Pointer, Arity, Fail)					  \
    if (*(Eterm *)							  \
	EXPAND_POINTER(tmp_arg1 = (Eterm) 				  \
		       COMPRESS_POINTER(tuple_val(Pointer))) != (Arity))  \
    { 									  \
        Fail; 								  \
    }

#define IsMap(Src, Fail) if (!is_map(Src)) { Fail; }

#define GetMapElement(Src, Key, Dst, Fail)	\
  do {						\
     Eterm _res = get_map_element(Src, Key);	\
     if (is_non_value(_res)) {			\
        Fail;					\
     }						\
     Dst = _res;				\
  } while (0)

#define GetMapElementHash(Src, Key, Hx, Dst, Fail)	\
  do {							\
     Eterm _res = get_map_element_hash(Src, Key, Hx);	\
     if (is_non_value(_res)) {				\
        Fail;						\
     }							\
     Dst = _res;					\
  } while (0)

#define IsFunction(X, Action)			\
  do {						\
     if ( !(is_any_fun(X)) ) {			\
          Action;				\
     }						\
  } while (0)

#define IsFunction2(F, A, Action)			\
  do {							\
     if (erl_is_function(c_p, F, A) != am_true ) {	\
          Action;					\
     }							\
  } while (0)

#define IsTupleOfArity(Src, Arity, Fail)				      \
  do {									      \
    if (is_not_tuple(Src) || 						      \
	*(Eterm *)							      \
	EXPAND_POINTER(tmp_arg1 = 					      \
		       (Eterm) COMPRESS_POINTER(tuple_val(Src))) != Arity) { \
        Fail;								      \
    }									      \
  } while (0)

#define IsBoolean(X, Fail) if ((X) != am_true && (X) != am_false) { Fail; }

#define IsBinary(Src, Fail) \
 if (is_not_binary(Src) || binary_bitsize(Src) != 0) { Fail; }

#define IsBitstring(Src, Fail) \
  if (is_not_binary(Src)) { Fail; }

#if defined(ARCH_64) && !HALFWORD_HEAP
#define BsSafeMul(A, B, Fail, Target)		\
   do { Uint64 _res = (A) * (B);		\
      if (_res / B != A) { Fail; }		\
      Target = _res;				\
   } while (0)
#else
#define BsSafeMul(A, B, Fail, Target)			\
   do { Uint64 _res = (Uint64)(A) * (Uint64)(B);	\
      if ((_res >> (8*sizeof(Uint))) != 0) { Fail; }	\
      Target = _res;					\
   } while (0)
#endif

#define BsGetFieldSize(Bits, Unit, Fail, Target)	\
   do {							\
      Sint _signed_size; Uint _uint_size;		\
      if (is_small(Bits)) {				\
        _signed_size = signed_val(Bits);		\
         if (_signed_size < 0) { Fail; }		\
         _uint_size = (Uint) _signed_size;		\
      } else {						\
         if (!term_to_Uint(Bits, &temp_bits)) { Fail; }	\
         _uint_size = temp_bits;			\
      }							\
      BsSafeMul(_uint_size, Unit, Fail, Target);	\
   } while (0)

#define BsGetUncheckedFieldSize(Bits, Unit, Fail, Target)	\
   do {								\
      Sint _signed_size; Uint _uint_size;			\
      if (is_small(Bits)) {					\
        _signed_size = signed_val(Bits);			\
         if (_signed_size < 0) { Fail; }			\
         _uint_size = (Uint) _signed_size;			\
      } else {							\
         if (!term_to_Uint(Bits, &temp_bits)) { Fail; }		\
         _uint_size = (Uint) temp_bits;				\
      }								\
      Target = _uint_size * Unit;				\
   } while (0)

#define BsGetFloat2(Ms, Live, Sz, Flags, Dst, Store, Fail)		\
 do {									\
   ErlBinMatchBuffer *_mb;						\
   Eterm _result; Sint _size;						\
   if (!is_small(Sz) || (_size = unsigned_val(Sz)) > 64) { Fail; }	\
   _size *= ((Flags) >> 3);						\
   TestHeap(FLOAT_SIZE_OBJECT, Live);					\
   _mb = ms_matchbuffer(Ms);						\
   LIGHT_SWAPOUT;							\
   _result = erts_bs_get_float_2(c_p, _size, (Flags), _mb);		\
   LIGHT_SWAPIN;							\
   HEAP_SPACE_VERIFIED(0);                                          \
   if (is_non_value(_result)) { Fail; }					\
   else { Store(_result, Dst); }					\
 } while (0)

#define BsGetBinaryImm_2(Ms, Live, Sz, Flags, Dst, Store, Fail)	\
  do {								\
    ErlBinMatchBuffer *_mb;					\
    Eterm _result;						\
    TestHeap(heap_bin_size(ERL_ONHEAP_BIN_LIMIT), Live);	\
    _mb = ms_matchbuffer(Ms);					\
    LIGHT_SWAPOUT;						\
    _result = erts_bs_get_binary_2(c_p, (Sz), (Flags), _mb);	\
    LIGHT_SWAPIN;						\
    HEAP_SPACE_VERIFIED(0);                                 \
    if (is_non_value(_result)) { Fail; }			\
    else { Store(_result, Dst); }				\
  } while (0)

#define BsGetBinary_2(Ms, Live, Sz, Flags, Dst, Store, Fail)	\
  do {								\
    ErlBinMatchBuffer *_mb;					\
    Eterm _result; Uint _size;					\
    BsGetFieldSize(Sz, ((Flags) >> 3), Fail, _size);		\
    TestHeap(ERL_SUB_BIN_SIZE, Live);				\
    _mb = ms_matchbuffer(Ms);					\
    LIGHT_SWAPOUT;						\
    _result = erts_bs_get_binary_2(c_p, _size, (Flags), _mb);	\
    LIGHT_SWAPIN;						\
    HEAP_SPACE_VERIFIED(0);                                 \
    if (is_non_value(_result)) { Fail; }			\
    else { Store(_result, Dst); }				\
  } while (0)

#define BsGetBinaryAll_2(Ms, Live, Unit, Dst, Store, Fail)	\
  do {								\
    ErlBinMatchBuffer *_mb;					\
    Eterm _result;						\
    TestHeap(ERL_SUB_BIN_SIZE, Live);				\
    _mb = ms_matchbuffer(Ms);					\
    if (((_mb->size - _mb->offset) % Unit) == 0) {		\
      LIGHT_SWAPOUT;						\
      _result = erts_bs_get_binary_all_2(c_p, _mb);		\
      LIGHT_SWAPIN;						\
      HEAP_SPACE_VERIFIED(0);                               \
      ASSERT(is_value(_result));				\
      Store(_result, Dst);					\
    } else {                                                    \
	HEAP_SPACE_VERIFIED(0);                             \
	Fail; }						        \
 } while (0)

#define BsSkipBits2(Ms, Bits, Unit, Fail)			\
 do {								\
   ErlBinMatchBuffer *_mb;					\
   size_t new_offset;						\
   Uint _size;							\
    _mb = ms_matchbuffer(Ms);					\
   BsGetFieldSize(Bits, Unit, Fail, _size);			\
   new_offset = _mb->offset + _size;				\
   if (new_offset <= _mb->size) { _mb->offset = new_offset; }	\
   else { Fail; }						\
 } while (0)

#define BsSkipBitsAll2(Ms, Unit, Fail)		\
 do {						\
    ErlBinMatchBuffer *_mb;			\
   _mb = ms_matchbuffer(Ms);			\
   if (((_mb->size - _mb->offset) % Unit) == 0) {_mb->offset = _mb->size; } \
   else { Fail; }					\
 } while (0)

#define BsSkipBitsImm2(Ms, Bits, Fail)				\
 do {								\
   ErlBinMatchBuffer *_mb;					\
   size_t new_offset;						\
   _mb = ms_matchbuffer(Ms);					\
   new_offset = _mb->offset + (Bits);				\
   if (new_offset <= _mb->size) { _mb->offset = new_offset; }	\
   else { Fail; }						\
 } while (0)

#define NewBsPutIntegerImm(Sz, Flags, Src)					\
 do {									\
    if (!erts_new_bs_put_integer(ERL_BITS_ARGS_3((Src), (Sz), (Flags)))) { goto badarg; }	\
 } while (0)

#define NewBsPutInteger(Sz, Flags, Src)						\
 do {										\
    Sint _size;									\
    BsGetUncheckedFieldSize(Sz, ((Flags) >> 3), goto badarg, _size);		\
    if (!erts_new_bs_put_integer(ERL_BITS_ARGS_3((Src), _size, (Flags))))	\
       { goto badarg; }								\
 } while (0)

#define NewBsPutFloatImm(Sz, Flags, Src)					\
 do {									\
    if (!erts_new_bs_put_float(c_p, (Src), (Sz), (Flags))) { goto badarg; }	\
 } while (0)

#define NewBsPutFloat(Sz, Flags, Src)						\
 do {										\
    Sint _size;									\
    BsGetUncheckedFieldSize(Sz, ((Flags) >> 3), goto badarg, _size);		\
    if (!erts_new_bs_put_float(c_p, (Src), _size, (Flags))) { goto badarg; }	\
 } while (0)

#define NewBsPutBinary(Sz, Flags, Src)							\
 do {											\
    Sint _size;										\
    BsGetUncheckedFieldSize(Sz, ((Flags) >> 3), goto badarg, _size);			\
    if (!erts_new_bs_put_binary(ERL_BITS_ARGS_2((Src), _size))) { goto badarg; }	\
 } while (0)

#define NewBsPutBinaryImm(Sz, Src)				        \
 do {							        \
    if (!erts_new_bs_put_binary(ERL_BITS_ARGS_2((Src), (Sz)))) { goto badarg; }	\
 } while (0)

#define NewBsPutBinaryAll(Src, Unit)							\
 do {											\
    if (!erts_new_bs_put_binary_all(ERL_BITS_ARGS_2((Src), (Unit)))) { goto badarg; }	\
 } while (0)


#define IsPort(Src, Fail) if (is_not_port(Src)) { Fail; }
#define IsPid(Src, Fail) if (is_not_pid(Src)) { Fail; }
#define IsRef(Src, Fail) if (is_not_ref(Src)) { Fail; }

/*
 * process_main() is already huge, so we want to avoid inlining
 * into it. Especially functions that are seldom used.
 */
#ifdef __GNUC__
#  define NOINLINE __attribute__((__noinline__))
#else
#  define NOINLINE
#endif


/*
 * The following functions are called directly by process_main().
 * Don't inline them.
 */
static BifFunction translate_gc_bif(void* gcf) NOINLINE;
static BeamInstr* handle_error(Process* c_p, BeamInstr* pc,
			       Eterm* reg, BifFunction bf) NOINLINE;
static BeamInstr* call_error_handler(Process* p, BeamInstr* ip,
				     Eterm* reg, Eterm func) NOINLINE;
static BeamInstr* fixed_apply(Process* p, Eterm* reg, Uint arity) NOINLINE;
static BeamInstr* apply(Process* p, Eterm module, Eterm function,
			Eterm args, Eterm* reg) NOINLINE;
static BeamInstr* call_fun(Process* p, int arity,
			   Eterm* reg, Eterm args) NOINLINE;
static BeamInstr* apply_fun(Process* p, Eterm fun,
			    Eterm args, Eterm* reg) NOINLINE;
static Eterm new_fun(Process* p, Eterm* reg,
		     ErlFunEntry* fe, int num_free) NOINLINE;
static Eterm new_map(Process* p, Eterm* reg, BeamInstr* I) NOINLINE;
static Eterm update_map_assoc(Process* p, Eterm* reg,
			      Eterm map, BeamInstr* I) NOINLINE;
static Eterm update_map_exact(Process* p, Eterm* reg,
			      Eterm map, BeamInstr* I) NOINLINE;
static Eterm get_map_element(Eterm map, Eterm key);
static Eterm get_map_element_hash(Eterm map, Eterm key, Uint32 hx);

/*
 * Functions not directly called by process_main(). OK to inline.
 */
static BeamInstr* next_catch(Process* c_p, Eterm *reg);
static void terminate_proc(Process* c_p, Eterm Value);
static Eterm add_stacktrace(Process* c_p, Eterm Value, Eterm exc);
static void save_stacktrace(Process* c_p, BeamInstr* pc, Eterm* reg,
			     BifFunction bf, Eterm args);
static struct StackTrace * get_trace_from_exc(Eterm exc);
static Eterm make_arglist(Process* c_p, Eterm* reg, int a);

void
init_emulator(void)
{
    process_main();
}

/*
 * On certain platforms, make sure that the main variables really are placed
 * in registers.
 */

#if defined(__GNUC__) && defined(sparc) && !defined(DEBUG)
#  define REG_x0 asm("%l0")
#  define REG_xregs asm("%l1")
#  define REG_htop asm("%l2")
#  define REG_stop asm("%l3")
#  define REG_I asm("%l4")
#  define REG_fcalls asm("%l5")
#  define REG_tmp_arg1 asm("%l6")
#  define REG_tmp_arg2 asm("%l7")
#else
#  define REG_x0
#  define REG_xregs
#  define REG_htop
#  define REG_stop
#  define REG_I
#  define REG_fcalls
#  define REG_tmp_arg1
#  define REG_tmp_arg2
#endif

#ifdef USE_VM_PROBES
#  define USE_VM_CALL_PROBES
#endif

#ifdef USE_VM_CALL_PROBES

#define DTRACE_LOCAL_CALL(p, m, f, a)					\
    if (DTRACE_ENABLED(local_function_entry)) {				\
        DTRACE_CHARBUF(process_name, DTRACE_TERM_BUF_SIZE);		\
        DTRACE_CHARBUF(mfa, DTRACE_TERM_BUF_SIZE);			\
        int depth = STACK_START(p) - STACK_TOP(p);			\
        dtrace_fun_decode(p, m, f, a,					\
                          process_name, mfa);				\
        DTRACE3(local_function_entry, process_name, mfa, depth);	\
    }

#define DTRACE_GLOBAL_CALL(p, m, f, a)					\
    if (DTRACE_ENABLED(global_function_entry)) {			\
        DTRACE_CHARBUF(process_name, DTRACE_TERM_BUF_SIZE);		\
        DTRACE_CHARBUF(mfa, DTRACE_TERM_BUF_SIZE);			\
        int depth = STACK_START(p) - STACK_TOP(p);			\
        dtrace_fun_decode(p, m, f, a,					\
                          process_name, mfa);				\
        DTRACE3(global_function_entry, process_name, mfa, depth);	\
    }

#define DTRACE_RETURN(p, m, f, a)                               \
    if (DTRACE_ENABLED(function_return)) {                      \
        DTRACE_CHARBUF(process_name, DTRACE_TERM_BUF_SIZE);     \
        DTRACE_CHARBUF(mfa, DTRACE_TERM_BUF_SIZE);              \
        int depth = STACK_START(p) - STACK_TOP(p);              \
        dtrace_fun_decode(p, m, f, a,                           \
                          process_name, mfa);                   \
        DTRACE3(function_return, process_name, mfa, depth);     \
    }

#define DTRACE_BIF_ENTRY(p, m, f, a)                            \
    if (DTRACE_ENABLED(bif_entry)) {                            \
        DTRACE_CHARBUF(process_name, DTRACE_TERM_BUF_SIZE);     \
        DTRACE_CHARBUF(mfa, DTRACE_TERM_BUF_SIZE);              \
        dtrace_fun_decode(p, m, f, a,                           \
                          process_name, mfa);                   \
        DTRACE2(bif_entry, process_name, mfa);                  \
    }

#define DTRACE_BIF_RETURN(p, m, f, a)                           \
    if (DTRACE_ENABLED(bif_return)) {                           \
        DTRACE_CHARBUF(process_name, DTRACE_TERM_BUF_SIZE);     \
        DTRACE_CHARBUF(mfa, DTRACE_TERM_BUF_SIZE);              \
        dtrace_fun_decode(p, m, f, a,                           \
                          process_name, mfa);                   \
        DTRACE2(bif_return, process_name, mfa);                 \
    }

#define DTRACE_NIF_ENTRY(p, m, f, a)                            \
    if (DTRACE_ENABLED(nif_entry)) {                            \
        DTRACE_CHARBUF(process_name, DTRACE_TERM_BUF_SIZE);     \
        DTRACE_CHARBUF(mfa, DTRACE_TERM_BUF_SIZE);              \
        dtrace_fun_decode(p, m, f, a,                           \
                          process_name, mfa);                   \
        DTRACE2(nif_entry, process_name, mfa);                  \
    }

#define DTRACE_NIF_RETURN(p, m, f, a)                           \
    if (DTRACE_ENABLED(nif_return)) {                           \
        DTRACE_CHARBUF(process_name, DTRACE_TERM_BUF_SIZE);     \
        DTRACE_CHARBUF(mfa, DTRACE_TERM_BUF_SIZE);              \
        dtrace_fun_decode(p, m, f, a,                           \
                          process_name, mfa);                   \
        DTRACE2(nif_return, process_name, mfa);                 \
    }

#define DTRACE_GLOBAL_CALL_FROM_EXPORT(p,e)                                                    \
    do {                                                                                       \
        if (DTRACE_ENABLED(global_function_entry)) {                                           \
            BeamInstr* fp = (BeamInstr *) (((Export *) (e))->addressv[erts_active_code_ix()]); \
            DTRACE_GLOBAL_CALL((p), (Eterm)fp[-3], (Eterm)fp[-2], fp[-1]);                     \
        }                                                                                      \
    } while(0)

#define DTRACE_RETURN_FROM_PC(p)                                                        \
    do {                                                                                \
        BeamInstr* fp;                                                                  \
        if (DTRACE_ENABLED(function_return) && (fp = find_function_from_pc((p)->cp))) { \
            DTRACE_RETURN((p), (Eterm)fp[0], (Eterm)fp[1], (Uint)fp[2]);                \
        }                                                                               \
    } while(0)

#else /* USE_VM_PROBES */
#define DTRACE_LOCAL_CALL(p, m, f, a)        do {} while (0)
#define DTRACE_GLOBAL_CALL(p, m, f, a)       do {} while (0)
#define DTRACE_GLOBAL_CALL_FROM_EXPORT(p, e) do {} while (0)
#define DTRACE_RETURN(p, m, f, a)            do {} while (0)
#define DTRACE_RETURN_FROM_PC(p)             do {} while (0)
#define DTRACE_BIF_ENTRY(p, m, f, a)         do {} while (0)
#define DTRACE_BIF_RETURN(p, m, f, a)        do {} while (0)
#define DTRACE_NIF_ENTRY(p, m, f, a)         do {} while (0)
#define DTRACE_NIF_RETURN(p, m, f, a)        do {} while (0)
#endif /* USE_VM_PROBES */

/*
 * process_main() is called twice:
 * The first call performs some initialisation, including exporting
 * the instructions' C labels to the loader.
 * The second call starts execution of BEAM code. This call never returns.
 */
void process_main(void)
{
    static int init_done = 0;
    Process* c_p = NULL;
    int reds_used;
#ifdef DEBUG
    ERTS_DECLARE_DUMMY(Eterm pid);
#endif

    /*
     * X register zero; also called r(0)
     */
    register Eterm x0 REG_x0 = NIL;

    /* Pointer to X registers: x(1)..x(N); reg[0] is used when doing GC,
     * in all other cases x0 is used.
     */
    register Eterm* reg REG_xregs = NULL;

    /*
     * Top of heap (next free location); grows upwards.
     */
    register Eterm* HTOP REG_htop = NULL;

    /* Stack pointer.  Grows downwards; points
     * to last item pushed (normally a saved
     * continuation pointer).
     */
    register Eterm* E REG_stop = NULL;

    /*
     * Pointer to next threaded instruction.
     */
    register BeamInstr *I REG_I = NULL;

    /* Number of reductions left.  This function
     * returns to the scheduler when FCALLS reaches zero.
     */
    register Sint FCALLS REG_fcalls = 0;

    /*
     * Temporaries used for picking up arguments for instructions.
     */
    register Eterm tmp_arg1 REG_tmp_arg1 = NIL;
    register Eterm tmp_arg2 REG_tmp_arg2 = NIL;
#if HEAP_ON_C_STACK
    Eterm tmp_big[2];           /* Temporary buffer for small bignums if HEAP_ON_C_STACK. */
#else
    Eterm *tmp_big;		/* Temporary buffer for small bignums if !HEAP_ON_C_STACK. */
#endif

    /*
     * X registers and floating point registers are located in
     * scheduler specific data.
     */
    register FloatDef *freg;

    /*
     * For keeping the negative old value of 'reds' when call saving is active.
     */
    int neg_o_reds = 0;

    Eterm (*arith_func)(Process* p, Eterm* reg, Uint live);

#ifdef ERTS_OPCODE_COUNTER_SUPPORT
    static void* counting_opcodes[] = { DEFINE_COUNTING_OPCODES };
#else
#ifndef NO_JUMP_TABLE
    static void* opcodes[] = { DEFINE_OPCODES };
#else
    int Go;
#endif
#endif

    Uint temp_bits; /* Temporary used by BsSkipBits2 & BsGetInteger2 */

    Eterm pt_arity;		/* Used by do_put_tuple */

    Uint64 start_time = 0;          /* Monitor long schedule */
    BeamInstr* start_time_i = NULL;

    ERL_BITS_DECLARE_STATEP; /* Has to be last declaration */


    /*
     * Note: In this function, we attempt to place rarely executed code towards
     * the end of the function, in the hope that the cache hit rate will be better.
     * The initialization code is only run once, so it is at the very end.
     *
     * Note: c_p->arity must be set to reflect the number of useful terms in
     * c_p->arg_reg before calling the scheduler.
     */
    if (!init_done) {
       /* This should only be reached during the init phase when only the main
        * process is running. I.e. there is no race for init_done.
        */
	init_done = 1;
	goto init_emulator;
    }

    c_p = NULL;
    reds_used = 0;

    goto do_schedule1;

 do_schedule:
    reds_used = REDS_IN(c_p) - FCALLS;
 do_schedule1:

    if (start_time != 0) {
        Sint64 diff = erts_timestamp_millis() - start_time;
	if (diff > 0 && (Uint) diff >  erts_system_monitor_long_schedule
#ifdef ERTS_DIRTY_SCHEDULERS
	    && !ERTS_SCHEDULER_IS_DIRTY(c_p->scheduler_data)
#endif
	    ) {
	    BeamInstr *inptr = find_function_from_pc(start_time_i);
	    BeamInstr *outptr = find_function_from_pc(c_p->i);
	    monitor_long_schedule_proc(c_p,inptr,outptr,(Uint) diff);
	}
    }

    PROCESS_MAIN_CHK_LOCKS(c_p);
    ERTS_SMP_UNREQ_PROC_MAIN_LOCK(c_p);
#if HALFWORD_HEAP
    ASSERT(erts_get_scheduler_data()->num_tmp_heap_used == 0);
#endif
    ERTS_VERIFY_UNUSED_TEMP_ALLOC(c_p);
    c_p = schedule(c_p, reds_used);
    ERTS_VERIFY_UNUSED_TEMP_ALLOC(c_p);
    start_time = 0;
#ifdef DEBUG
    pid = c_p->common.id; /* Save for debugging purpouses */
#endif
    ERTS_SMP_REQ_PROC_MAIN_LOCK(c_p);
    PROCESS_MAIN_CHK_LOCKS(c_p);

    if (erts_system_monitor_long_schedule != 0) {
	start_time = erts_timestamp_millis();
	start_time_i = c_p->i;
    }

    reg = ERTS_PROC_GET_SCHDATA(c_p)->x_reg_array;
    freg = ERTS_PROC_GET_SCHDATA(c_p)->f_reg_array;
#if !HEAP_ON_C_STACK
    tmp_big = ERTS_PROC_GET_SCHDATA(c_p)->beam_emu_tmp_heap;
#endif
    ERL_BITS_RELOAD_STATEP(c_p);
    {
	int reds;
	Eterm* argp;
	BeamInstr *next;
	int i;

	argp = c_p->arg_reg;
	for (i = c_p->arity - 1; i > 0; i--) {
	    reg[i] = argp[i];
	    CHECK_TERM(reg[i]);
	}

	/*
	 * We put the original reduction count in the process structure, to reduce
	 * the code size (referencing a field in a struct through a pointer stored
	 * in a register gives smaller code than referencing a global variable).
	 */

	SET_I(c_p->i);

	reds = c_p->fcalls;
	if (ERTS_PROC_GET_SAVED_CALLS_BUF(c_p)
	    && (ERTS_TRACE_FLAGS(c_p) & F_SENSITIVE) == 0) {
	    neg_o_reds = -reds;
	    FCALLS = REDS_IN(c_p) = 0;
	} else {
	    neg_o_reds = 0;
	    FCALLS = REDS_IN(c_p) = reds;
	}

	next = (BeamInstr *) *I;
	r(0) = c_p->arg_reg[0];
#ifdef HARDDEBUG
	if (c_p->arity > 0) {
	    CHECK_TERM(r(0));
	}
#endif
	SWAPIN;
	ASSERT(VALID_INSTR(next));

#ifdef USE_VM_PROBES
        if (DTRACE_ENABLED(process_scheduled)) {
            DTRACE_CHARBUF(process_buf, DTRACE_TERM_BUF_SIZE);
            DTRACE_CHARBUF(fun_buf, DTRACE_TERM_BUF_SIZE);
            dtrace_proc_str(c_p, process_buf);

            if (ERTS_PROC_IS_EXITING(c_p)) {
                strcpy(fun_buf, "<exiting>");
            } else {
                BeamInstr *fptr = find_function_from_pc(c_p->i);
                if (fptr) {
                    dtrace_fun_decode(c_p, (Eterm)fptr[0],
                                      (Eterm)fptr[1], (Uint)fptr[2],
                                      NULL, fun_buf);
                } else {
                    erts_snprintf(fun_buf, sizeof(DTRACE_CHARBUF_NAME(fun_buf)),
                                  "<unknown/%p>", next);
                }
            }

            DTRACE2(process_scheduled, process_buf, fun_buf);
        }
#endif
	Goto(next);
    }

#if defined(DEBUG) || defined(NO_JUMP_TABLE)
 emulator_loop:
#endif

#ifdef NO_JUMP_TABLE
    switch (Go) {
#endif
#include "beam_hot.h"

#define STORE_ARITH_RESULT(res) StoreBifResult(2, (res));
#define ARITH_FUNC(name) erts_gc_##name

	{
	    Eterm increment_reg_val;
	    Eterm increment_val;
	    Uint live;
	    Eterm result;

	OpCase(i_increment_yIId):
	    increment_reg_val = yb(Arg(0));
	    goto do_increment;

	OpCase(i_increment_xIId):
	    increment_reg_val = xb(Arg(0));
	    goto do_increment;

	OpCase(i_increment_rIId):
	    increment_reg_val = r(0);
	    I--;

	do_increment:
	    increment_val = Arg(1);
	    if (is_small(increment_reg_val)) {
		Sint i = signed_val(increment_reg_val) + increment_val;
		ASSERT(MY_IS_SSMALL(i) == IS_SSMALL(i));
		if (MY_IS_SSMALL(i)) {
		    result = make_small(i);
		store_result:
		    StoreBifResult(3, result);
		}
	    }

	    live = Arg(2);
	    SWAPOUT;
	    reg[0] = r(0);
	    reg[live] = increment_reg_val;
	    reg[live+1] = make_small(increment_val);
	    result = erts_gc_mixed_plus(c_p, reg, live);
	    r(0) = reg[0];
	    SWAPIN;
	    ERTS_HOLE_CHECK(c_p);
	    if (is_value(result)) {
		goto store_result;
	    }
	    ASSERT(c_p->freason != BADMATCH || is_value(c_p->fvalue));
	    goto find_func_info;
	}

#define DO_BIG_ARITH(Func,Arg1,Arg2)     \
    do {                                 \
        Uint live = Arg(1);              \
        SWAPOUT;                         \
        reg[0] = r(0);                   \
        reg[live] = (Arg1);              \
        reg[live+1] = (Arg2);            \
        result = (Func)(c_p, reg, live); \
        r(0) = reg[0];                   \
        SWAPIN;                          \
        ERTS_HOLE_CHECK(c_p);            \
        if (is_value(result)) {          \
            StoreBifResult(4,result);    \
        }                                \
        goto lb_Cl_error;                \
    } while(0)

 OpCase(i_plus_jIxxd):
 {
     Eterm result;

     if (is_both_small(xb(Arg(2)), xb(Arg(3)))) {
	 Sint i = signed_val(xb(Arg(2))) + signed_val(xb(Arg(3)));
	 ASSERT(MY_IS_SSMALL(i) == IS_SSMALL(i));
	 if (MY_IS_SSMALL(i)) {
	     result = make_small(i);
             StoreBifResult(4, result);
	 }
     }
     DO_BIG_ARITH(ARITH_FUNC(mixed_plus), xb(Arg(2)), xb(Arg(3)));
 }

 OpCase(i_plus_jId):
 {
     Eterm result;

     if (is_both_small(tmp_arg1, tmp_arg2)) {
	 Sint i = signed_val(tmp_arg1) + signed_val(tmp_arg2);
	 ASSERT(MY_IS_SSMALL(i) == IS_SSMALL(i));
	 if (MY_IS_SSMALL(i)) {
	     result = make_small(i);
	     STORE_ARITH_RESULT(result);
	 }
     }
     arith_func = ARITH_FUNC(mixed_plus);
     goto do_big_arith2;
 }

 OpCase(i_minus_jIxxd):
 {
     Eterm result;

     if (is_both_small(xb(Arg(2)), xb(Arg(3)))) {
	 Sint i = signed_val(xb(Arg(2))) - signed_val(xb(Arg(3)));
	 ASSERT(MY_IS_SSMALL(i) == IS_SSMALL(i));
	 if (MY_IS_SSMALL(i)) {
	     result = make_small(i);
             StoreBifResult(4, result);
	 }
     }
     DO_BIG_ARITH(ARITH_FUNC(mixed_minus), xb(Arg(2)), xb(Arg(3)));
 }

 OpCase(i_minus_jId):
 {
     Eterm result;

     if (is_both_small(tmp_arg1, tmp_arg2)) {
	 Sint i = signed_val(tmp_arg1) - signed_val(tmp_arg2);
	 ASSERT(MY_IS_SSMALL(i) == IS_SSMALL(i));
	 if (MY_IS_SSMALL(i)) {
	     result = make_small(i);
	     STORE_ARITH_RESULT(result);
	 }
     }
     arith_func = ARITH_FUNC(mixed_minus);
     goto do_big_arith2;
 }

 OpCase(i_is_lt_f):
    if (CMP_GE(tmp_arg1, tmp_arg2)) {
	ClauseFail();
    }
    Next(1);

 OpCase(i_is_ge_f):
    if (CMP_LT(tmp_arg1, tmp_arg2)) {
	ClauseFail();
    }
    Next(1);

 OpCase(i_is_eq_f):
    if (CMP_NE(tmp_arg1, tmp_arg2)) {
	ClauseFail();
    }
    Next(1);

 OpCase(i_is_ne_f):
    if (CMP_EQ(tmp_arg1, tmp_arg2)) {
	ClauseFail();
    }
    Next(1);

 OpCase(i_is_eq_exact_f):
    if (!EQ(tmp_arg1, tmp_arg2)) {
	ClauseFail();
    }
    Next(1);

    {
	Eterm is_eq_exact_lit_val;

    OpCase(i_is_eq_exact_literal_xfc):
	is_eq_exact_lit_val = xb(Arg(0));
	I++;
	goto do_is_eq_exact_literal;

    OpCase(i_is_eq_exact_literal_yfc):
	is_eq_exact_lit_val = yb(Arg(0));
	I++;
	goto do_is_eq_exact_literal;

    OpCase(i_is_eq_exact_literal_rfc):
	is_eq_exact_lit_val = r(0);

    do_is_eq_exact_literal:
	if (!eq(Arg(1), is_eq_exact_lit_val)) {
	    ClauseFail();
	}
	Next(2);
    }

    {
	Eterm is_ne_exact_lit_val;

    OpCase(i_is_ne_exact_literal_xfc):
	is_ne_exact_lit_val = xb(Arg(0));
	I++;
	goto do_is_ne_exact_literal;

    OpCase(i_is_ne_exact_literal_yfc):
	is_ne_exact_lit_val = yb(Arg(0));
	I++;
	goto do_is_ne_exact_literal;

    OpCase(i_is_ne_exact_literal_rfc):
	is_ne_exact_lit_val = r(0);

    do_is_ne_exact_literal:
	if (eq(Arg(1), is_ne_exact_lit_val)) {
	    ClauseFail();
	}
	Next(2);
    }

 OpCase(move_window3_xxxy): {
     BeamInstr *next;
     Eterm xt0, xt1, xt2;
     Eterm *y = (Eterm *)(((unsigned char *)E) + (Arg(3)));
     PreFetch(4, next);
     xt0  = xb(Arg(0));
     xt1  = xb(Arg(1));
     xt2  = xb(Arg(2));
     y[0] = xt0;
     y[1] = xt1;
     y[2] = xt2;
     NextPF(4, next);
 }
 OpCase(move_window4_xxxxy): {
     BeamInstr *next;
     Eterm xt0, xt1, xt2, xt3;
     Eterm *y = (Eterm *)(((unsigned char *)E) + (Arg(4)));
     PreFetch(5, next);
     xt0  = xb(Arg(0));
     xt1  = xb(Arg(1));
     xt2  = xb(Arg(2));
     xt3  = xb(Arg(3));
     y[0] = xt0;
     y[1] = xt1;
     y[2] = xt2;
     y[3] = xt3;
     NextPF(5, next);
 }
 OpCase(move_window5_xxxxxy): {
     BeamInstr *next;
     Eterm xt0, xt1, xt2, xt3, xt4;
     Eterm *y = (Eterm *)(((unsigned char *)E) + (Arg(5)));
     PreFetch(6, next);
     xt0  = xb(Arg(0));
     xt1  = xb(Arg(1));
     xt2  = xb(Arg(2));
     xt3  = xb(Arg(3));
     xt4  = xb(Arg(4));
     y[0] = xt0;
     y[1] = xt1;
     y[2] = xt2;
     y[3] = xt3;
     y[4] = xt4;
     NextPF(6, next);
 }

 OpCase(i_move_call_only_fcr): {
     r(0) = Arg(1);
 }
 /* FALL THROUGH */
 OpCase(i_call_only_f): {
     SET_I((BeamInstr *) Arg(0));
     DTRACE_LOCAL_CALL(c_p, (Eterm)I[-3], (Eterm)I[-2], I[-1]);
     Dispatch();
 }

 OpCase(i_move_call_last_fPcr): {
     r(0) = Arg(2);
 }
 /* FALL THROUGH */
 OpCase(i_call_last_fP): {
     RESTORE_CP(E);
     E = ADD_BYTE_OFFSET(E, Arg(1));
     SET_I((BeamInstr *) Arg(0));
     DTRACE_LOCAL_CALL(c_p, (Eterm)I[-3], (Eterm)I[-2], I[-1]);
     Dispatch();
 }

 OpCase(i_move_call_crf): {
     r(0) = Arg(0);
     I++;
 }
 /* FALL THROUGH */
 OpCase(i_call_f): {
     SET_CP(c_p, I+2);
     SET_I((BeamInstr *) Arg(0));
     DTRACE_LOCAL_CALL(c_p, (Eterm)I[-3], (Eterm)I[-2], I[-1]);
     Dispatch();
 }

 OpCase(i_move_call_ext_last_ePcr): {
     r(0) = Arg(2);
 }
 /* FALL THROUGH */
 OpCase(i_call_ext_last_eP):
    RESTORE_CP(E);
    E = ADD_BYTE_OFFSET(E, Arg(1));

    /*
     * Note: The pointer to the export entry is never NULL; if the module
     * is not loaded, it points to code which will invoke the error handler
     * (see lb_call_error_handler below).
     */
    DTRACE_GLOBAL_CALL_FROM_EXPORT(c_p, Arg(0));
    Dispatchx();

 OpCase(i_move_call_ext_cre): {
     r(0) = Arg(0);
     I++;
 }
 /* FALL THROUGH */
 OpCase(i_call_ext_e):
    SET_CP(c_p, I+2);
    DTRACE_GLOBAL_CALL_FROM_EXPORT(c_p, Arg(0));
    Dispatchx();

 OpCase(i_move_call_ext_only_ecr): {
     r(0) = Arg(1);
 }
 /* FALL THROUGH */
 OpCase(i_call_ext_only_e):
    DTRACE_GLOBAL_CALL_FROM_EXPORT(c_p, Arg(0));
    Dispatchx();

 OpCase(init_y): {
     BeamInstr *next;

     PreFetch(1, next);
     make_blank(yb(Arg(0)));
     NextPF(1, next);
 }

 OpCase(i_trim_I): {
     BeamInstr *next;
     Uint words;
     Uint cp;

     words = Arg(0);
     cp = E[0];
     PreFetch(1, next);
     E += words;
     E[0] = cp;
     NextPF(1, next);
 }

 OpCase(move_x1_c): {
	x(1) = Arg(0);
	Next(1);
    }

 OpCase(move_x2_c): {
	x(2) = Arg(0);
	Next(1);
    }

 OpCase(return): {
    SET_I(c_p->cp);
    DTRACE_RETURN_FROM_PC(c_p);
    /*
     * We must clear the CP to make sure that a stale value do not
     * create a false module dependcy preventing code upgrading.
     * It also means that we can use the CP in stack backtraces.
     */
    c_p->cp = 0;
    CHECK_TERM(r(0));
    HEAP_SPACE_VERIFIED(0);
    Goto(*I);
 }

    /*
     * Send is almost a standard call-BIF with two arguments, except for:
     *    1) It cannot be traced.
     *	  2) There is no pointer to the send_2 function stored in
     *       the instruction.
     */

 OpCase(send): {
     BeamInstr *next;
     Eterm result;

     PRE_BIF_SWAPOUT(c_p);
     c_p->fcalls = FCALLS - 1;
     reg[0] = r(0);
     result = erl_send(c_p, r(0), x(1));
     PreFetch(0, next);
     ERTS_VERIFY_UNUSED_TEMP_ALLOC(c_p);
     ERTS_SMP_REQ_PROC_MAIN_LOCK(c_p);
     PROCESS_MAIN_CHK_LOCKS(c_p);
     if (c_p->mbuf || MSO(c_p).overhead >= BIN_VHEAP_SZ(c_p)) {
	 result = erts_gc_after_bif_call(c_p, result, reg, 2);
	 r(0) = reg[0];
	 E = c_p->stop;
     }
     HTOP = HEAP_TOP(c_p);
     FCALLS = c_p->fcalls;
     if (is_value(result)) {
	 r(0) = result;
	 CHECK_TERM(r(0));
	 NextPF(0, next);
     } else if (c_p->freason == TRAP) {
	 SET_CP(c_p, I+1);
	 SET_I(c_p->i);
	 SWAPIN;
	 r(0) = reg[0];
	 Dispatch();
     }
     goto find_func_info;
 }

    {
	Eterm element_index;
	Eterm element_tuple;

    OpCase(i_element_xjsd):
	element_tuple = xb(Arg(0));
	I++;
	goto do_element;

    OpCase(i_element_yjsd):
	element_tuple = yb(Arg(0));
	I++;
	goto do_element;

    OpCase(i_element_rjsd):
	element_tuple = r(0);
	/* Fall through */

    do_element:
	GetArg1(1, element_index);
	if (is_small(element_index) && is_tuple(element_tuple)) {
	    Eterm* tp = tuple_val(element_tuple);

	    if ((signed_val(element_index) >= 1) &&
		(signed_val(element_index) <= arityval(*tp))) {
		Eterm result = tp[signed_val(element_index)];
		StoreBifResult(2, result);
	    }
	}
    }
 /* Fall through */

 OpCase(badarg_j):
 badarg:
    c_p->freason = BADARG;
    goto lb_Cl_error;

    {
	Eterm fast_element_tuple;

    OpCase(i_fast_element_rjId):
	fast_element_tuple = r(0);

    do_fast_element:
	if (is_tuple(fast_element_tuple)) {
	    Eterm* tp = tuple_val(fast_element_tuple);
	    Eterm pos = Arg(1);	/* Untagged integer >= 1 */
	    if (pos <= arityval(*tp)) {
		Eterm result = tp[pos];
		StoreBifResult(2, result);
	    }
	}
     goto badarg;

    OpCase(i_fast_element_xjId):
     fast_element_tuple = xb(Arg(0));
     I++;
     goto do_fast_element;

    OpCase(i_fast_element_yjId):
     fast_element_tuple = yb(Arg(0));
     I++;
     goto do_fast_element;
 }

 OpCase(catch_yf):
     c_p->catches++;
     yb(Arg(0)) = Arg(1);
     Next(2);

 OpCase(catch_end_y): {
     c_p->catches--;
     make_blank(yb(Arg(0)));
     if (is_non_value(r(0))) {
	 if (x(1) == am_throw) {
	     r(0) = x(2);
	 } else {
	     if (x(1) == am_error) {
	         SWAPOUT;
		 x(2) = add_stacktrace(c_p, x(2), x(3));
		 SWAPIN;
	     }
	     /* only x(2) is included in the rootset here */
	     if (E - HTOP < 3 || c_p->mbuf) {	/* Force GC in case add_stacktrace()
						 * created heap fragments */
		 SWAPOUT;
		 PROCESS_MAIN_CHK_LOCKS(c_p);
		 FCALLS -= erts_garbage_collect(c_p, 3, reg+2, 1);
		 ERTS_VERIFY_UNUSED_TEMP_ALLOC(c_p);
		 PROCESS_MAIN_CHK_LOCKS(c_p);
		 SWAPIN;
	     }
	     r(0) = TUPLE2(HTOP, am_EXIT, x(2));
	     HTOP += 3;
	 }
     }
     CHECK_TERM(r(0));
     Next(1);
 }

 OpCase(try_end_y): {
     c_p->catches--;
     make_blank(yb(Arg(0)));
     if (is_non_value(r(0))) {
	 r(0) = x(1);
	 x(1) = x(2);
	 x(2) = x(3);
     }
     Next(1);
 }

 /*
  * Skeleton for receive statement:
  *
  *             recv_mark L1                     Optional
  *             call make_ref/monitor            Optional
  *             ...
  *             recv_set L1                      Optional
  *      L1:          <-------------------+
  *                   <-----------+       |
  *     	     	       	  |   	  |
  *             loop_rec L2 ------+---+   |
  *             ...               |   |   |
  *             remove_message 	  |   |	  |
  *             jump L3           |   |   |
  *		...	          |   |   |
  *		loop_rec_end L1 --+   |   |
  *      L2:          <---------------+   |
  *	   	wait L1  -----------------+      or wait_timeout
  *		timeout
  *
  *	 L3:    Code after receive...
  *
  *
  */

 OpCase(recv_mark_f): {
     /*
      * Save the current position in message buffer and the
      * the label for the loop_rec/2 instruction for the
      * the receive statement.
      */
     c_p->msg.mark = (BeamInstr *) Arg(0);
     c_p->msg.saved_last = c_p->msg.last;
     Next(1);
 }

 OpCase(i_recv_set): {
     /*
      * If the mark is valid (points to the loop_rec/2
      * instruction that follows), we know that the saved
      * position points to the first message that could
      * possibly be matched out.
      *
      * If the mark is invalid, we do nothing, meaning that
      * we will look through all messages in the message queue.
      */
     if (c_p->msg.mark == (BeamInstr *) (I+1)) {
	 c_p->msg.save = c_p->msg.saved_last;
     }
     I++;
     /* Fall through to the loop_rec/2 instruction */
 }

    /*
     * Pick up the next message and place it in x(0).
     * If no message, jump to a wait or wait_timeout instruction.
     */
 OpCase(i_loop_rec_fr):
 {
     BeamInstr *next;
     ErlMessage* msgp;

 loop_rec__:

     PROCESS_MAIN_CHK_LOCKS(c_p);

     msgp = PEEK_MESSAGE(c_p);

     if (!msgp) {
#ifdef ERTS_SMP
	 erts_smp_proc_lock(c_p, ERTS_PROC_LOCKS_MSG_RECEIVE);
	 /* Make sure messages wont pass exit signals... */
	 if (ERTS_PROC_PENDING_EXIT(c_p)) {
	     erts_smp_proc_unlock(c_p, ERTS_PROC_LOCKS_MSG_RECEIVE);
	     SWAPOUT;
	     goto do_schedule; /* Will be rescheduled for exit */
	 }
	 ERTS_SMP_MSGQ_MV_INQ2PRIVQ(c_p);
	 msgp = PEEK_MESSAGE(c_p);
	 if (msgp)
	     erts_smp_proc_unlock(c_p, ERTS_PROC_LOCKS_MSG_RECEIVE);
	 else
#endif
	 {
	     SET_I((BeamInstr *) Arg(0));
	     Goto(*I);		/* Jump to a wait or wait_timeout instruction */
	 }
     }
     ErtsMoveMsgAttachmentIntoProc(msgp, c_p, E, HTOP, FCALLS,
				   {
				       SWAPOUT;
				       reg[0] = r(0);
				       PROCESS_MAIN_CHK_LOCKS(c_p);
				   },
				   {
				       ERTS_VERIFY_UNUSED_TEMP_ALLOC(c_p);
				       PROCESS_MAIN_CHK_LOCKS(c_p);
				       r(0) = reg[0];
				       SWAPIN;
				   });
     if (is_non_value(ERL_MESSAGE_TERM(msgp))) {
	 /*
	  * A corrupt distribution message that we weren't able to decode;
	  * remove it...
	  */
	 ASSERT(!msgp->data.attached);
         /* TODO: Add DTrace probe for this bad message situation? */
	 UNLINK_MESSAGE(c_p, msgp);
	 free_message(msgp);
	 goto loop_rec__;
     }
     PreFetch(1, next);
     r(0) = ERL_MESSAGE_TERM(msgp);
     NextPF(1, next);
 }

 /*
  * Remove a (matched) message from the message queue.
  */
 OpCase(remove_message): {
     BeamInstr *next;
     ErlMessage* msgp;

     PROCESS_MAIN_CHK_LOCKS(c_p);

     PreFetch(0, next);
     msgp = PEEK_MESSAGE(c_p);

     if (ERTS_PROC_GET_SAVED_CALLS_BUF(c_p)) {
	 save_calls(c_p, &exp_receive);
     }
     if (ERL_MESSAGE_TOKEN(msgp) == NIL) {
#ifdef USE_VM_PROBES
	 if (DT_UTAG(c_p) != NIL) {
	     if (DT_UTAG_FLAGS(c_p) & DT_UTAG_PERMANENT) {
		 SEQ_TRACE_TOKEN(c_p) = am_have_dt_utag;
#ifdef DTRACE_TAG_HARDDEBUG
		 if (DT_UTAG_FLAGS(c_p) & DT_UTAG_SPREADING) 
		     erts_fprintf(stderr,
				  "Dtrace -> (%T) stop spreading "
				  "tag %T with message %T\r\n",
				  c_p->common.id,DT_UTAG(c_p),ERL_MESSAGE_TERM(msgp));
#endif
	     } else {
#ifdef DTRACE_TAG_HARDDEBUG
		 erts_fprintf(stderr,
			      "Dtrace -> (%T) kill tag %T with "
			      "message %T\r\n",
			      c_p->common.id,DT_UTAG(c_p),ERL_MESSAGE_TERM(msgp));
#endif
		 DT_UTAG(c_p) = NIL;
		 SEQ_TRACE_TOKEN(c_p) = NIL;
	     }
	 } else {
#endif
	     SEQ_TRACE_TOKEN(c_p) = NIL;
#ifdef USE_VM_PROBES
	 }
	 DT_UTAG_FLAGS(c_p) &= ~DT_UTAG_SPREADING;
#endif
     } else if (ERL_MESSAGE_TOKEN(msgp) != am_undefined) {
	 Eterm msg;
	 SEQ_TRACE_TOKEN(c_p) = ERL_MESSAGE_TOKEN(msgp);
#ifdef USE_VM_PROBES
	 if (ERL_MESSAGE_TOKEN(msgp) == am_have_dt_utag) {
	     if (DT_UTAG(c_p) == NIL) {
		 DT_UTAG(c_p) = ERL_MESSAGE_DT_UTAG(msgp);
	     }
	     DT_UTAG_FLAGS(c_p) |= DT_UTAG_SPREADING;
#ifdef DTRACE_TAG_HARDDEBUG
	     erts_fprintf(stderr,
			  "Dtrace -> (%T) receive tag (%T) "
			  "with message %T\r\n",
			  c_p->common.id, DT_UTAG(c_p), ERL_MESSAGE_TERM(msgp));
#endif
	 } else {
#endif
	     ASSERT(is_tuple(SEQ_TRACE_TOKEN(c_p)));
	     ASSERT(SEQ_TRACE_TOKEN_ARITY(c_p) == 5);
	     ASSERT(is_small(SEQ_TRACE_TOKEN_SERIAL(c_p)));
	     ASSERT(is_small(SEQ_TRACE_TOKEN_LASTCNT(c_p)));
	     ASSERT(is_small(SEQ_TRACE_TOKEN_FLAGS(c_p)));
	     ASSERT(is_pid(SEQ_TRACE_TOKEN_SENDER(c_p)));
	     c_p->seq_trace_lastcnt = unsigned_val(SEQ_TRACE_TOKEN_SERIAL(c_p));
	     if (c_p->seq_trace_clock < unsigned_val(SEQ_TRACE_TOKEN_SERIAL(c_p))) {
		 c_p->seq_trace_clock = unsigned_val(SEQ_TRACE_TOKEN_SERIAL(c_p));
	     }
	     msg = ERL_MESSAGE_TERM(msgp);
	     seq_trace_output(SEQ_TRACE_TOKEN(c_p), msg, SEQ_TRACE_RECEIVE, 
			      c_p->common.id, c_p);
#ifdef USE_VM_PROBES
	 }
#endif
     }
#ifdef USE_VM_PROBES
     if (DTRACE_ENABLED(message_receive)) {
         Eterm token2 = NIL;
         DTRACE_CHARBUF(receiver_name, DTRACE_TERM_BUF_SIZE);
         Sint tok_label = 0;
         Sint tok_lastcnt = 0;
         Sint tok_serial = 0;

         dtrace_proc_str(c_p, receiver_name);
         token2 = SEQ_TRACE_TOKEN(c_p);
         if (token2 != NIL && token2 != am_have_dt_utag) {
             tok_label = signed_val(SEQ_TRACE_T_LABEL(token2));
             tok_lastcnt = signed_val(SEQ_TRACE_T_LASTCNT(token2));
             tok_serial = signed_val(SEQ_TRACE_T_SERIAL(token2));
         }
         DTRACE6(message_receive,
                 receiver_name, size_object(ERL_MESSAGE_TERM(msgp)),
                 c_p->msg.len - 1, tok_label, tok_lastcnt, tok_serial);
     }
#endif
     UNLINK_MESSAGE(c_p, msgp);
     JOIN_MESSAGE(c_p);
     CANCEL_TIMER(c_p);
     free_message(msgp);

     ERTS_VERIFY_UNUSED_TEMP_ALLOC(c_p);
     PROCESS_MAIN_CHK_LOCKS(c_p);

     NextPF(0, next);
 }

    /*
     * Advance the save pointer to the next message (the current
     * message didn't match), then jump to the loop_rec instruction.
     */
 OpCase(loop_rec_end_f): {
     SET_I((BeamInstr *) Arg(0));
     SAVE_MESSAGE(c_p);
     goto loop_rec__;
 }
    /*
     * Prepare to wait for a message or a timeout, whichever occurs first.
     *
     * Note: In order to keep the compatibility between 32 and 64 bits
     * emulators, only timeout values that can be represented in 32 bits
     * (unsigned) or less are allowed.
     */


 OpCase(i_wait_timeout_fs): {
     erts_smp_proc_lock(c_p, ERTS_PROC_LOCKS_MSG_RECEIVE);

     /* Fall through */
 }
 OpCase(i_wait_timeout_locked_fs): {
     Eterm timeout_value;

     /*
      * If we have already set the timer, we must NOT set it again.  Therefore,
      * we must test the F_INSLPQUEUE flag as well as the F_TIMO flag.
      */
     if (c_p->flags & (F_INSLPQUEUE | F_TIMO)) {
	 goto wait2;
     }
     GetArg1(1, timeout_value);
     if (timeout_value != make_small(0)) {

	 if (timeout_value == am_infinity)
	     c_p->flags |= F_TIMO;
	 else {
	     int tres = erts_set_proc_timer_term(c_p, timeout_value);
	     if (tres == 0) {
		 /*
		  * The timer routiner will set c_p->i to the value in
		  * c_p->def_arg_reg[0].  Note that it is safe to use this
		  * location because there are no living x registers in
		  * a receive statement.
		  * Note that for the halfword emulator, the two first elements
		  * of the array are used.
		  */
		 BeamInstr** pi = (BeamInstr**) c_p->def_arg_reg;
		 *pi = I+3;
	     }
	     else { /* Wrong time */
	     OpCase(i_wait_error_locked): {
		     erts_smp_proc_unlock(c_p, ERTS_PROC_LOCKS_MSG_RECEIVE);
		     /* Fall through */
		 }
	     OpCase(i_wait_error): {
		     c_p->freason = EXC_TIMEOUT_VALUE;
		     goto find_func_info;
		 }
	     }
	 }

	 /*
	  * Prepare to wait indefinitely for a new message to arrive
	  * (or the time set above if falling through from above).
	  *
	  * When a new message arrives, control will be transferred
	  * the loop_rec instruction (at label L1).  In case of
	  * of timeout, control will be transferred to the timeout
	  * instruction following the wait_timeout instruction.
	  */

	 OpCase(wait_locked_f):
	 OpCase(wait_f):

	 wait2: {
	     c_p->i = (BeamInstr *) Arg(0); /* L1 */
	     SWAPOUT;
	     c_p->arity = 0;
	     erts_smp_atomic32_read_band_relb(&c_p->state, ~ERTS_PSFLG_ACTIVE);
	     ASSERT(!ERTS_PROC_IS_EXITING(c_p));
	     erts_smp_proc_unlock(c_p, ERTS_PROC_LOCKS_MSG_RECEIVE);
	     c_p->current = NULL;
	     goto do_schedule;
	 }
	 OpCase(wait_unlocked_f): {
	     erts_smp_proc_lock(c_p, ERTS_PROC_LOCKS_MSG_RECEIVE);
	     goto wait2;
	 }
     }
     erts_smp_proc_unlock(c_p, ERTS_PROC_LOCKS_MSG_RECEIVE);
     Next(2);
 }

 OpCase(i_wait_timeout_fI): {
     erts_smp_proc_lock(c_p, ERTS_PROC_LOCKS_MSG_RECEIVE);
 }

 OpCase(i_wait_timeout_locked_fI):
 {
     /*
      * If we have already set the timer, we must NOT set it again.  Therefore,
      * we must test the F_INSLPQUEUE flag as well as the F_TIMO flag.
      */
     if ((c_p->flags & (F_INSLPQUEUE | F_TIMO)) == 0) {
	 BeamInstr** p = (BeamInstr **) c_p->def_arg_reg;
	 *p = I+3;
	 erts_set_proc_timer_uword(c_p, Arg(1));
     }
     goto wait2;
 }

    /*
     * A timeout has occurred.  Reset the save pointer so that the next
     * receive statement will examine the first message first.
     */
 OpCase(timeout_locked): {
     erts_smp_proc_unlock(c_p, ERTS_PROC_LOCKS_MSG_RECEIVE);
 }

 OpCase(timeout): {
     BeamInstr *next;

     PreFetch(0, next);
     if (IS_TRACED_FL(c_p, F_TRACE_RECEIVE)) {
	 trace_receive(c_p, am_timeout);
     }
     if (ERTS_PROC_GET_SAVED_CALLS_BUF(c_p)) {
	 save_calls(c_p, &exp_timeout);
     }
     c_p->flags &= ~F_TIMO;
     JOIN_MESSAGE(c_p);
     NextPF(0, next);
 }

 {
     Eterm select_val2;

 OpCase(i_select_tuple_arity2_yfAAff):
     select_val2 = yb(Arg(0));
     goto do_select_tuple_arity2;

 OpCase(i_select_tuple_arity2_xfAAff):
     select_val2 = xb(Arg(0));
     goto do_select_tuple_arity2;

 OpCase(i_select_tuple_arity2_rfAAff):
     select_val2 = r(0);
     I--;

 do_select_tuple_arity2:
     if (is_not_tuple(select_val2)) {
	 goto select_val2_fail;
     }
     select_val2 = *tuple_val(select_val2);
     goto do_select_val2;

 OpCase(i_select_val2_yfccff):
     select_val2 = yb(Arg(0));
     goto do_select_val2;

 OpCase(i_select_val2_xfccff):
     select_val2 = xb(Arg(0));
     goto do_select_val2;

 OpCase(i_select_val2_rfccff):
     select_val2 = r(0);
     I--;

 do_select_val2:
     if (select_val2 == Arg(2)) {
	 I += 3;
     } else if (select_val2 == Arg(3)) {
	 I += 4;
     }

 select_val2_fail:
     SET_I((BeamInstr *) Arg(1));
     Goto(*I);
 }

 {
     Eterm select_val;

 OpCase(i_select_tuple_arity_xfI):
     select_val = xb(Arg(0));
     goto do_select_tuple_arity;

 OpCase(i_select_tuple_arity_yfI):
     select_val = yb(Arg(0));
     goto do_select_tuple_arity;

 OpCase(i_select_tuple_arity_rfI):
     select_val = r(0);
     I--;

 do_select_tuple_arity:
     if (is_tuple(select_val)) {
	 select_val = *tuple_val(select_val);
	 goto do_linear_search;
     }
     SET_I((BeamInstr *) Arg(1));
     Goto(*I);

 OpCase(i_select_val_lins_xfI):
     select_val = xb(Arg(0));
     goto do_linear_search;

 OpCase(i_select_val_lins_yfI):
     select_val = yb(Arg(0));
     goto do_linear_search;

 OpCase(i_select_val_lins_rfI):
     select_val = r(0);
     I--;

 do_linear_search: {
     BeamInstr *vs = &Arg(3);
     int ix = 0;

     for(;;) {
	 if (vs[ix+0] >= select_val) { ix += 0; break; }
	 if (vs[ix+1] >= select_val) { ix += 1; break; }
	 ix += 2;
     }

     if (vs[ix] == select_val) {
	 I += ix + Arg(2) + 2;
     }

     SET_I((BeamInstr *) Arg(1));
     Goto(*I);
   }

 OpCase(i_select_val_bins_xfI):
     select_val = xb(Arg(0));
     goto do_binary_search;

 OpCase(i_select_val_bins_yfI):
     select_val = yb(Arg(0));
     goto do_binary_search;
     
 OpCase(i_select_val_bins_rfI):
     select_val = r(0);
     I--;

 do_binary_search:
 {
     struct Pairs {
	 BeamInstr val;
	 BeamInstr* addr;
     };
     struct Pairs* low;
     struct Pairs* high;
     struct Pairs* mid;
     int bdiff; /* int not long because the arrays aren't that large */

     low = (struct Pairs *) &Arg(3);
     high = low + Arg(2);

     /* The pointer subtraction (high-low) below must produce
      * a signed result, because high could be < low. That
      * requires the compiler to insert quite a bit of code.
      *
      * However, high will be > low so the result will be
      * positive. We can use that knowledge to optimise the
      * entire sequence, from the initial comparison to the
      * computation of mid.
      *
      * -- Mikael Pettersson, Acumem AB
      *
      * Original loop control code:
      *
      * while (low < high) {
      *    mid = low + (high-low) / 2;
      *
      */
     while ((bdiff = (int)((char*)high - (char*)low)) > 0) {
	 unsigned int boffset = ((unsigned int)bdiff >> 1) & ~(sizeof(struct Pairs)-1);

	 mid = (struct Pairs*)((char*)low + boffset);
	 if (select_val < mid->val) {
	     high = mid;
	 } else if (select_val > mid->val) {
	     low = mid + 1;
	 } else {
	     SET_I(mid->addr);
	     Goto(*I);
	 }
     }
     SET_I((BeamInstr *) Arg(1));
     Goto(*I);
 }
 }

 {
     Eterm jump_on_val_zero_index;
     
 OpCase(i_jump_on_val_zero_yfI):
     jump_on_val_zero_index = yb(Arg(0));
     goto do_jump_on_val_zero_index;

 OpCase(i_jump_on_val_zero_xfI):
     jump_on_val_zero_index = xb(Arg(0));
     goto do_jump_on_val_zero_index;

 OpCase(i_jump_on_val_zero_rfI):
     jump_on_val_zero_index = r(0);
     I--;

 do_jump_on_val_zero_index:
     if (is_small(jump_on_val_zero_index)) {
	 jump_on_val_zero_index = signed_val(jump_on_val_zero_index);
	 if (jump_on_val_zero_index < Arg(2)) {
	     SET_I((BeamInstr *) (&Arg(3))[jump_on_val_zero_index]);
	     Goto(*I);
	 }
     }
     SET_I((BeamInstr *) Arg(1));
     Goto(*I);
 }

 {
     Eterm jump_on_val_index;

 
 OpCase(i_jump_on_val_yfII):
     jump_on_val_index = yb(Arg(0));
     goto do_jump_on_val_index;

 OpCase(i_jump_on_val_xfII):
     jump_on_val_index = xb(Arg(0));
     goto do_jump_on_val_index;

 OpCase(i_jump_on_val_rfII):
     jump_on_val_index = r(0);
     I--;

 do_jump_on_val_index:
     if (is_small(jump_on_val_index)) {
	 jump_on_val_index = (Uint) (signed_val(jump_on_val_index) - Arg(3));
	 if (jump_on_val_index < Arg(2)) {
	     SET_I((BeamInstr *) (&Arg(4))[jump_on_val_index]);
	     Goto(*I);
	 }
     }
     SET_I((BeamInstr *) Arg(1));
     Goto(*I);
 }

 do_put_tuple: {
     Eterm* hp = HTOP;

     *hp++ = make_arityval(pt_arity);

     do {
	 Eterm term = *I++;
	 switch (term & _TAG_IMMED1_MASK) {
	 case (R_REG_DEF << _TAG_PRIMARY_SIZE) | TAG_PRIMARY_HEADER:
	     *hp++ = r(0);
	     break;
	 case (X_REG_DEF << _TAG_PRIMARY_SIZE) | TAG_PRIMARY_HEADER:
	     *hp++ = x(term >> _TAG_IMMED1_SIZE);
	     break;
	 case (Y_REG_DEF << _TAG_PRIMARY_SIZE) | TAG_PRIMARY_HEADER:
	     *hp++ = y(term >> _TAG_IMMED1_SIZE);
	     break;
	 default:
	     *hp++ = term;
	     break;
	 }
     } while (--pt_arity != 0);
     HTOP = hp;
     Goto(*I);
 }

 OpCase(new_map_dII): {
     Eterm res;

     x(0) = r(0);
     SWAPOUT;
     res = new_map(c_p, reg, I-1);
     SWAPIN;
     r(0) = x(0);
     StoreResult(res, Arg(0));
     Next(3+Arg(2));
 }

#define PUT_TERM_REG(term, desc)				\
do {								\
    switch ((desc) & _TAG_IMMED1_MASK) {			\
    case (R_REG_DEF << _TAG_PRIMARY_SIZE) | TAG_PRIMARY_HEADER:	\
	r(0) = (term);						\
	break;							\
    case (X_REG_DEF << _TAG_PRIMARY_SIZE) | TAG_PRIMARY_HEADER:	\
	x((desc) >> _TAG_IMMED1_SIZE) = (term);			\
	break;							\
    case (Y_REG_DEF << _TAG_PRIMARY_SIZE) | TAG_PRIMARY_HEADER:	\
	y((desc) >> _TAG_IMMED1_SIZE) = (term);			\
	break;							\
    default:							\
	ASSERT(0);						\
	break;							\
    }								\
} while(0)

 OpCase(i_get_map_elements_fsI): {
    Eterm map;
    BeamInstr *fs;
    Uint sz, n;

    GetArg1(1, map);

    /* this instruction assumes Arg1 is a map,
     * i.e. that it follows a test is_map if needed.
     */

    n  = (Uint)Arg(2) / 3;
    fs = &Arg(3); /* pattern fields and target registers */

    if (is_flatmap(map)) {
	flatmap_t *mp;
	Eterm *ks;
	Eterm *vs;

	mp = (flatmap_t *)flatmap_val(map);
	sz = flatmap_get_size(mp);

	if (sz == 0) {
	    ClauseFail();
	}

	ks = flatmap_get_keys(mp);
	vs = flatmap_get_values(mp);

	while(sz) {
	    if (EQ((Eterm) fs[0], *ks)) {
		PUT_TERM_REG(*vs, fs[1]);
		n--;
		fs += 3;
		/* no more values to fetch, we are done */
		if (n == 0) {
		    I = fs;
		    Next(-1);
		}
	    }
	    ks++, sz--, vs++;
	}

	ClauseFail();
    } else {
	const Eterm *v;
	Uint32 hx;
	ASSERT(is_hashmap(map));
	while(n--) {
	    hx = fs[2];
	    ASSERT(hx == hashmap_make_hash((Eterm)fs[0]));
	    if ((v = erts_hashmap_get(hx, (Eterm)fs[0], map)) == NULL) {
		ClauseFail();
	    }
	    PUT_TERM_REG(*v, fs[1]);
	    fs += 3;
	}
	I = fs;
	Next(-1);
    }
 }
#undef PUT_TERM_REG

 OpCase(update_map_assoc_jsdII): {
     Eterm res;
     Eterm map;

     GetArg1(1, map);
     x(0) = r(0);
     SWAPOUT;
     res = update_map_assoc(c_p, reg, map, I);
     SWAPIN;
     if (is_value(res)) {
	 r(0) = x(0);
	 StoreResult(res, Arg(2));
	 Next(5+Arg(4));
     } else {
	 /*
	  * This can only happen if the code was compiled
	  * with the compiler in OTP 17.
	  */
	 c_p->freason = BADMAP;
	 c_p->fvalue = map;
	 goto lb_Cl_error;
     }
 }

 OpCase(update_map_exact_jsdII): {
     Eterm res;
     Eterm map;

     GetArg1(1, map);
     x(0) = r(0);
     SWAPOUT;
     res = update_map_exact(c_p, reg, map, I);
     SWAPIN;
     if (is_value(res)) {
	 r(0) = x(0);
	 StoreResult(res, Arg(2));
	 Next(5+Arg(4));
     } else {
	 goto lb_Cl_error;
     }
 }


    /*
     * All guards with zero arguments have special instructions:
     * 	self/0
     * 	node/0
     *
     * All other guard BIFs take one or two arguments.
     */

    /*
     * Guard BIF in head.  On failure, ignore the error and jump
     * to the code for the next clause.  We don't support tracing
     * of guard BIFs.
     */

 OpCase(bif1_fbsd):
    {
	Eterm (*bf)(Process*, Eterm*);
	Eterm tmp_reg[1];
	Eterm result;

	GetArg1(2, tmp_reg[0]);
	bf = (BifFunction) Arg(1);
	c_p->fcalls = FCALLS;
	PROCESS_MAIN_CHK_LOCKS(c_p);
	ASSERT(!ERTS_PROC_IS_EXITING(c_p));
	result = (*bf)(c_p, tmp_reg);
	ASSERT(!ERTS_PROC_IS_EXITING(c_p) || is_non_value(result));
	ERTS_VERIFY_UNUSED_TEMP_ALLOC(c_p);
	PROCESS_MAIN_CHK_LOCKS(c_p);
	ERTS_HOLE_CHECK(c_p);
	FCALLS = c_p->fcalls;
	if (is_value(result)) {
	    StoreBifResult(3, result);
	}
	SET_I((BeamInstr *) Arg(0));
	Goto(*I);
    }

    /*
     * Guard BIF in body.  It can fail like any BIF.  No trace support.
     */

 OpCase(bif1_body_bsd):
    {
	Eterm (*bf)(Process*, Eterm*);

	Eterm tmp_reg[1];
	Eterm result;

	GetArg1(1, tmp_reg[0]);
	bf = (BifFunction) Arg(0);
	c_p->fcalls = FCALLS;
	PROCESS_MAIN_CHK_LOCKS(c_p);
	ASSERT(!ERTS_PROC_IS_EXITING(c_p));
	result = (*bf)(c_p, tmp_reg);
	ASSERT(!ERTS_PROC_IS_EXITING(c_p) || is_non_value(result));
	ERTS_VERIFY_UNUSED_TEMP_ALLOC(c_p);
	PROCESS_MAIN_CHK_LOCKS(c_p);
	ERTS_HOLE_CHECK(c_p);
	FCALLS = c_p->fcalls;
	if (is_value(result)) {
	    StoreBifResult(2, result);
	}
	reg[0] = tmp_reg[0];
	SWAPOUT;
	I = handle_error(c_p, I, reg, bf);
	goto post_error_handling;
    }

 OpCase(i_gc_bif1_jIsId):
    {
	typedef Eterm (*GcBifFunction)(Process*, Eterm*, Uint);
	GcBifFunction bf;
	Eterm arg;
	Eterm result;
	Uint live = (Uint) Arg(3);

	GetArg1(2, arg);
	reg[0] = r(0);
	reg[live] = arg;
	bf = (GcBifFunction) Arg(1);
	c_p->fcalls = FCALLS;
	SWAPOUT;
	PROCESS_MAIN_CHK_LOCKS(c_p);
	ERTS_SMP_UNREQ_PROC_MAIN_LOCK(c_p);
	result = (*bf)(c_p, reg, live);
	ERTS_VERIFY_UNUSED_TEMP_ALLOC(c_p);
	ERTS_SMP_REQ_PROC_MAIN_LOCK(c_p);
	PROCESS_MAIN_CHK_LOCKS(c_p);
	SWAPIN;
	r(0) = reg[0];
	ERTS_HOLE_CHECK(c_p);
	FCALLS = c_p->fcalls;
	if (is_value(result)) {
	    StoreBifResult(4, result);
	}
	if (Arg(0) != 0) {
	    SET_I((BeamInstr *) Arg(0));
	    Goto(*I);
	}
	reg[0] = arg;
	I = handle_error(c_p, I, reg, translate_gc_bif((void *) bf));
	goto post_error_handling;
    }

 OpCase(i_gc_bif2_jIId): /* Note, one less parameter than the i_gc_bif1
			    and i_gc_bif3 */
    {
	typedef Eterm (*GcBifFunction)(Process*, Eterm*, Uint);
	GcBifFunction bf;
	Eterm result;
	Uint live = (Uint) Arg(2);

	reg[0] = r(0);
	reg[live++] = tmp_arg1;
	reg[live] = tmp_arg2;
	bf = (GcBifFunction) Arg(1);
	c_p->fcalls = FCALLS;
	SWAPOUT;
	PROCESS_MAIN_CHK_LOCKS(c_p);
	ERTS_SMP_UNREQ_PROC_MAIN_LOCK(c_p);
	result = (*bf)(c_p, reg, live);
	ERTS_VERIFY_UNUSED_TEMP_ALLOC(c_p);
	ERTS_SMP_REQ_PROC_MAIN_LOCK(c_p);
	PROCESS_MAIN_CHK_LOCKS(c_p);
	SWAPIN;
	r(0) = reg[0];
	ERTS_HOLE_CHECK(c_p);
	FCALLS = c_p->fcalls;
	if (is_value(result)) {
	    StoreBifResult(3, result);
	}
	if (Arg(0) != 0) {
	    SET_I((BeamInstr *) Arg(0));
	    Goto(*I);
	}
	reg[0] = tmp_arg1;
	reg[1] = tmp_arg2;
	I = handle_error(c_p, I, reg, translate_gc_bif((void *) bf));
	goto post_error_handling;
    }

 OpCase(i_gc_bif3_jIsId):
    {
	typedef Eterm (*GcBifFunction)(Process*, Eterm*, Uint);
	GcBifFunction bf;
	Eterm arg;
	Eterm result;
	Uint live = (Uint) Arg(3);

	GetArg1(2, arg);
	reg[0] = r(0);
	reg[live++] = arg;
	reg[live++] = tmp_arg1;
	reg[live] = tmp_arg2;
	bf = (GcBifFunction) Arg(1);
	c_p->fcalls = FCALLS;
	SWAPOUT;
	PROCESS_MAIN_CHK_LOCKS(c_p);
	ERTS_SMP_UNREQ_PROC_MAIN_LOCK(c_p);
	result = (*bf)(c_p, reg, live);
	ERTS_VERIFY_UNUSED_TEMP_ALLOC(c_p);
	ERTS_SMP_REQ_PROC_MAIN_LOCK(c_p);
	PROCESS_MAIN_CHK_LOCKS(c_p);
	SWAPIN;
	r(0) = reg[0];
	ERTS_HOLE_CHECK(c_p);
	FCALLS = c_p->fcalls;
	if (is_value(result)) {
	    StoreBifResult(4, result);
	}
	if (Arg(0) != 0) {
	    SET_I((BeamInstr *) Arg(0));
	    Goto(*I);
	}
	reg[0] = arg;
	reg[1] = tmp_arg1;
	reg[2] = tmp_arg2;
	I = handle_error(c_p, I, reg, translate_gc_bif((void *) bf));
	goto post_error_handling;
    }

 /*
  * Guards bifs and, or, xor in guards.
  */
 OpCase(i_bif2_fbd):
    {
	Eterm tmp_reg[2] = {tmp_arg1, tmp_arg2};
	Eterm (*bf)(Process*, Eterm*);
	Eterm result;

	bf = (BifFunction) Arg(1);
	c_p->fcalls = FCALLS;
	PROCESS_MAIN_CHK_LOCKS(c_p);
	ASSERT(!ERTS_PROC_IS_EXITING(c_p));
	result = (*bf)(c_p, tmp_reg);
	ASSERT(!ERTS_PROC_IS_EXITING(c_p) || is_non_value(result));
	ERTS_VERIFY_UNUSED_TEMP_ALLOC(c_p);
	PROCESS_MAIN_CHK_LOCKS(c_p);
	ERTS_HOLE_CHECK(c_p);
	FCALLS = c_p->fcalls;
	if (is_value(result)) {
	    StoreBifResult(2, result);
	}
	SET_I((BeamInstr *) Arg(0));
	Goto(*I);
    }

 /*
  * Guards bifs and, or, xor, relational operators in body.
  */
 OpCase(i_bif2_body_bd):
    {
	Eterm tmp_reg[2] = {tmp_arg1, tmp_arg2};
	Eterm (*bf)(Process*, Eterm*);
	Eterm result;

	bf = (BifFunction) Arg(0);
	PROCESS_MAIN_CHK_LOCKS(c_p);
	ASSERT(!ERTS_PROC_IS_EXITING(c_p));
	result = (*bf)(c_p, tmp_reg);
	ASSERT(!ERTS_PROC_IS_EXITING(c_p) || is_non_value(result));
	ERTS_VERIFY_UNUSED_TEMP_ALLOC(c_p);
	PROCESS_MAIN_CHK_LOCKS(c_p);
	ERTS_HOLE_CHECK(c_p);
	if (is_value(result)) {
	    ASSERT(!is_CP(result));
	    StoreBifResult(1, result);
	}
	reg[0] = tmp_arg1;
	reg[1] = tmp_arg2;
	SWAPOUT;
	I = handle_error(c_p, I, reg, bf);
	goto post_error_handling;
    }

    /*
     * The most general BIF call.  The BIF may build any amount of data
     * on the heap.  The result is always returned in r(0).
     */
 OpCase(call_bif_e):
    {
	Eterm (*bf)(Process*, Eterm*, BeamInstr*) = GET_BIF_ADDRESS(Arg(0));
	Eterm result;
	BeamInstr *next;

	PRE_BIF_SWAPOUT(c_p);
	c_p->fcalls = FCALLS - 1;
	if (FCALLS <= 0) {
	   save_calls(c_p, (Export *) Arg(0));
	}
	PreFetch(1, next);
	ASSERT(!ERTS_PROC_IS_EXITING(c_p));
	ERTS_VERIFY_UNUSED_TEMP_ALLOC(c_p);
	reg[0] = r(0);
	result = (*bf)(c_p, reg, I);
	ASSERT(!ERTS_PROC_IS_EXITING(c_p) || is_non_value(result));
	ERTS_VERIFY_UNUSED_TEMP_ALLOC(c_p);
	ERTS_HOLE_CHECK(c_p);
	ERTS_SMP_REQ_PROC_MAIN_LOCK(c_p);
	PROCESS_MAIN_CHK_LOCKS(c_p);
	if (c_p->mbuf || MSO(c_p).overhead >= BIN_VHEAP_SZ(c_p)) {
	    Uint arity = ((Export *)Arg(0))->code[2];
	    result = erts_gc_after_bif_call(c_p, result, reg, arity);
	    E = c_p->stop;
	}
	HTOP = HEAP_TOP(c_p);
	FCALLS = c_p->fcalls;
	if (is_value(result)) {
	    r(0) = result;
	    CHECK_TERM(r(0));
	    NextPF(1, next);
	} else if (c_p->freason == TRAP) {
	    SET_CP(c_p, I+2);
	    SET_I(c_p->i);
	    SWAPIN;
	    r(0) = reg[0];
	    Dispatch();
	}

	/*
	 * Error handling.  SWAPOUT is not needed because it was done above.
	 */
	ASSERT(c_p->stop == E);
	I = handle_error(c_p, I, reg, bf);
	goto post_error_handling;
    }

 /*
  * Arithmetic operations.
  */

 OpCase(i_times_jId):
 {
     arith_func = ARITH_FUNC(mixed_times);
     goto do_big_arith2;
 }

 OpCase(i_m_div_jId):
 {
     arith_func = ARITH_FUNC(mixed_div);
     goto do_big_arith2;
 }

 OpCase(i_int_div_jId):
 {
     Eterm result;

     if (tmp_arg2 == SMALL_ZERO) {
	 goto badarith;
     } else if (is_both_small(tmp_arg1, tmp_arg2)) {
	 Sint ires = signed_val(tmp_arg1) / signed_val(tmp_arg2);
	 if (MY_IS_SSMALL(ires)) {
	     result = make_small(ires);
	     STORE_ARITH_RESULT(result);
	 }
     }
     arith_func = ARITH_FUNC(int_div);
     goto do_big_arith2;
 }

 OpCase(i_rem_jIxxd):
 {
     Eterm result;

     if (xb(Arg(3)) == SMALL_ZERO) {
	 goto badarith;
     } else if (is_both_small(xb(Arg(2)), xb(Arg(3)))) {
	 result = make_small(signed_val(xb(Arg(2))) % signed_val(xb(Arg(3))));
         StoreBifResult(4, result);
     }
     DO_BIG_ARITH(ARITH_FUNC(int_rem),xb(Arg(2)),xb(Arg(3)));
 }

 OpCase(i_rem_jId):
 {
     Eterm result;

     if (tmp_arg2 == SMALL_ZERO) {
	 goto badarith;
     } else if (is_both_small(tmp_arg1, tmp_arg2)) {
	 result = make_small(signed_val(tmp_arg1) % signed_val(tmp_arg2));
	 STORE_ARITH_RESULT(result);
     } else {
	 arith_func = ARITH_FUNC(int_rem);
	 goto do_big_arith2;
     }
 }

 OpCase(i_band_jIxcd):
 {
     Eterm result;

     if (is_both_small(xb(Arg(2)), Arg(3))) {
         /*
          * No need to untag -- TAG & TAG == TAG.
          */
         result = xb(Arg(2)) & Arg(3);
         StoreBifResult(4, result);
     }
     DO_BIG_ARITH(ARITH_FUNC(band),xb(Arg(2)),Arg(3));
 }

 OpCase(i_band_jId):
 {
     Eterm result;

     if (is_both_small(tmp_arg1, tmp_arg2)) {
	 /*
	  * No need to untag -- TAG & TAG == TAG.
	  */
	 result = tmp_arg1 & tmp_arg2;
	 STORE_ARITH_RESULT(result);
     }
     arith_func = ARITH_FUNC(band);
     goto do_big_arith2;
 }

#undef DO_BIG_ARITH

 do_big_arith2:
 {
     Eterm result;
     Uint live = Arg(1);

     SWAPOUT;
     reg[0] = r(0);
     reg[live] = tmp_arg1;
     reg[live+1] = tmp_arg2;
     result = arith_func(c_p, reg, live);
     r(0) = reg[0];
     SWAPIN;
     ERTS_HOLE_CHECK(c_p);
     if (is_value(result)) {
	 STORE_ARITH_RESULT(result);
     }
     goto lb_Cl_error;
 }

 /*
  * An error occured in an arithmetic operation or test that could
  * appear either in a head or in a body.
  * In a head, execution should continue at failure address in Arg(0).
  * In a body, Arg(0) == 0 and an exception should be raised.
  */
 lb_Cl_error: {
     if (Arg(0) != 0) {
	 OpCase(jump_f): {
	 jump_f:
	     SET_I((BeamInstr *) Arg(0));
	     Goto(*I);
	 }
     }
     ASSERT(c_p->freason != BADMATCH || is_value(c_p->fvalue));
     goto find_func_info;
 }

 OpCase(i_bor_jId):
 {
     Eterm result;

     if (is_both_small(tmp_arg1, tmp_arg2)) {
	 /*
	  * No need to untag -- TAG | TAG == TAG.
	  */
	 result = tmp_arg1 | tmp_arg2;
	 STORE_ARITH_RESULT(result);
     }
     arith_func = ARITH_FUNC(bor);
     goto do_big_arith2;
 }

 OpCase(i_bxor_jId):
 {
     Eterm result;

     if (is_both_small(tmp_arg1, tmp_arg2)) {
	 /*
	  * We could extract the tag from one argument, but a tag extraction
	  * could mean a shift.  Therefore, play it safe here.
	  */
	 result = make_small(signed_val(tmp_arg1) ^ signed_val(tmp_arg2));
	 STORE_ARITH_RESULT(result);
     }
     arith_func = ARITH_FUNC(bxor);
     goto do_big_arith2;
 }

 {
     Sint i;
     Sint ires;
     Eterm* bigp;

     OpCase(i_bsr_jId):
	 if (is_small(tmp_arg2)) {
	     i = -signed_val(tmp_arg2);
	     if (is_small(tmp_arg1)) {
		 goto small_shift;
	     } else if (is_big(tmp_arg1)) {
		 if (i == 0) {
		     StoreBifResult(2, tmp_arg1);
		 }
		 goto big_shift;
	     }
	 } else if (is_big(tmp_arg2)) {
	     /*
	      * N bsr NegativeBigNum == N bsl MAX_SMALL
	      * N bsr PositiveBigNum == N bsl MIN_SMALL
	      */
	     tmp_arg2 = make_small(bignum_header_is_neg(*big_val(tmp_arg2)) ?
				   MAX_SMALL : MIN_SMALL);
	     goto do_bsl;
	}
     goto badarith;
     
     OpCase(i_bsl_jId):
 do_bsl:
	 if (is_small(tmp_arg2)) {
	     i = signed_val(tmp_arg2);

	     if (is_small(tmp_arg1)) {
	     small_shift:
		 ires = signed_val(tmp_arg1);
	     
		 if (i == 0 || ires == 0) {
		     StoreBifResult(2, tmp_arg1);
		 } else if (i < 0)  { /* Right shift */
		     i = -i;
		     if (i >= SMALL_BITS-1) {
			 tmp_arg1 = (ires < 0) ? SMALL_MINUS_ONE : SMALL_ZERO;
		     } else {
			 tmp_arg1 = make_small(ires >> i);
		     }
		     StoreBifResult(2, tmp_arg1);
		 } else if (i < SMALL_BITS-1) { /* Left shift */
		     if ((ires > 0 && ((~(Uint)0 << ((SMALL_BITS-1)-i)) & ires) == 0) ||
			 ((~(Uint)0 << ((SMALL_BITS-1)-i)) & ~ires) == 0) {
			 tmp_arg1 = make_small(ires << i);
			 StoreBifResult(2, tmp_arg1);
		     }
		 }
		 tmp_arg1 = small_to_big(ires, tmp_big);

	     big_shift:
		 if (i > 0) {	/* Left shift. */
		     ires = big_size(tmp_arg1) + (i / D_EXP);
		 } else {	/* Right shift. */
		     ires = big_size(tmp_arg1);
		     if (ires <= (-i / D_EXP))
			 ires = 3; /* ??? */
		     else
			 ires -= (-i / D_EXP);
		 }
		 {
		     ires = BIG_NEED_SIZE(ires+1);
		     /*
		      * Slightly conservative check the size to avoid
		      * allocating huge amounts of memory for bignums that 
		      * clearly would overflow the arity in the header
		      * word.
		      */
		     if (ires-8 > BIG_ARITY_MAX) {
			 c_p->freason = SYSTEM_LIMIT;
			 goto lb_Cl_error;
		     }
		     TestHeapPreserve(ires+1, Arg(1), tmp_arg1);
		     bigp = HTOP;
		     tmp_arg1 = big_lshift(tmp_arg1, i, bigp);
		     if (is_big(tmp_arg1)) {
			 HTOP += bignum_header_arity(*HTOP) + 1;
		     }
		     HEAP_SPACE_VERIFIED(0);
		     if (is_nil(tmp_arg1)) {
			 /*
			  * This result must have been only slight larger
			  * than allowed since it wasn't caught by the
			  * previous test.
			  */
			 c_p->freason = SYSTEM_LIMIT;
			 goto lb_Cl_error;
		     }
		     ERTS_HOLE_CHECK(c_p);
		     StoreBifResult(2, tmp_arg1);
		 }
	     } else if (is_big(tmp_arg1)) {
		 if (i == 0) {
		     StoreBifResult(2, tmp_arg1);
		 }
		 goto big_shift;
	     }
	 } else if (is_big(tmp_arg2)) {
	     if (bignum_header_is_neg(*big_val(tmp_arg2))) {
		 /*
		  * N bsl NegativeBigNum is either 0 or -1, depending on
		  * the sign of N. Since we don't believe this case
		  * is common, do the calculation with the minimum
		  * amount of code.
		  */
		 tmp_arg2 = make_small(MIN_SMALL);
		 goto do_bsl;
	     } else if (is_small(tmp_arg1) || is_big(tmp_arg1)) {
		 /*
		  * N bsl PositiveBigNum is too large to represent.
		  */
		 c_p->freason = SYSTEM_LIMIT;
		 goto lb_Cl_error;
	     }
	     /* Fall through if the left argument is not an integer. */
	 }
     /*
      * One or more non-integer arguments.
      */
     goto badarith;
 }

 OpCase(i_int_bnot_jsId):
 {
     Eterm bnot_val;

     GetArg1(1, bnot_val);
     if (is_small(bnot_val)) {
	 bnot_val = make_small(~signed_val(bnot_val));
     } else {
	 Uint live = Arg(2);
	 SWAPOUT;
	 reg[0] = r(0);
	 reg[live] = bnot_val;
	 bnot_val = erts_gc_bnot(c_p, reg, live);
	 r(0) = reg[0];
	 SWAPIN;
	 ERTS_HOLE_CHECK(c_p);
	 if (is_nil(bnot_val)) {
	     goto lb_Cl_error;
	 }
     }
     StoreBifResult(3, bnot_val);
 }

 badarith:
    c_p->freason = BADARITH;
    goto lb_Cl_error;

 OpCase(i_apply): {
     BeamInstr *next;
     SWAPOUT;
     next = apply(c_p, r(0), x(1), x(2), reg);
     SWAPIN;
     if (next != NULL) {
	 r(0) = reg[0];
	 SET_CP(c_p, I+1);
	 SET_I(next);
	 Dispatch();
     }
     I = handle_error(c_p, I, reg, apply_3);
     goto post_error_handling;
 }

 OpCase(i_apply_last_P): {
     BeamInstr *next;
     SWAPOUT;
     next = apply(c_p, r(0), x(1), x(2), reg);
     SWAPIN;
     if (next != NULL) {
	 r(0) = reg[0];
	 SET_CP(c_p, (BeamInstr *) EXPAND_POINTER(E[0]));
	 E = ADD_BYTE_OFFSET(E, Arg(0));
	 SET_I(next);
	 Dispatch();
     }
     I = handle_error(c_p, I, reg, apply_3);
     goto post_error_handling;
 }

 OpCase(i_apply_only): {
     BeamInstr *next;
     SWAPOUT;
     next = apply(c_p, r(0), x(1), x(2), reg);
     SWAPIN;
     if (next != NULL) {
	 r(0) = reg[0];
	 SET_I(next);
	 Dispatch();
     }
     I = handle_error(c_p, I, reg, apply_3);
     goto post_error_handling;
 }

 OpCase(apply_I): {
     BeamInstr *next;

     reg[0] = r(0);
     SWAPOUT;
     next = fixed_apply(c_p, reg, Arg(0));
     SWAPIN;
     if (next != NULL) {
	 r(0) = reg[0];
	 SET_CP(c_p, I+2);
	 SET_I(next);
	 Dispatch();
     }
     I = handle_error(c_p, I, reg, apply_3);
     goto post_error_handling;
 }

 OpCase(apply_last_IP): {
     BeamInstr *next;

     reg[0] = r(0);
     SWAPOUT;
     next = fixed_apply(c_p, reg, Arg(0));
     SWAPIN;
     if (next != NULL) {
	 r(0) = reg[0];
	 SET_CP(c_p, (BeamInstr *) EXPAND_POINTER(E[0]));
	 E = ADD_BYTE_OFFSET(E, Arg(1));
	 SET_I(next);
	 Dispatch();
     }
     I = handle_error(c_p, I, reg, apply_3);
     goto post_error_handling;
 }

 OpCase(i_apply_fun): {
     BeamInstr *next;

     SWAPOUT;
     next = apply_fun(c_p, r(0), x(1), reg);
     SWAPIN;
     if (next != NULL) {
	 r(0) = reg[0];
	 SET_CP(c_p, I+1);
	 SET_I(next);
	 Dispatchfun();
     }
     goto find_func_info;
 }

 OpCase(i_apply_fun_last_P): {
     BeamInstr *next;

     SWAPOUT;
     next = apply_fun(c_p, r(0), x(1), reg);
     SWAPIN;
     if (next != NULL) {
	 r(0) = reg[0];
	 SET_CP(c_p, (BeamInstr *) EXPAND_POINTER(E[0]));
	 E = ADD_BYTE_OFFSET(E, Arg(0));
	 SET_I(next);
	 Dispatchfun();
     }
     goto find_func_info;
 }

 OpCase(i_apply_fun_only): {
     BeamInstr *next;

     SWAPOUT;
     next = apply_fun(c_p, r(0), x(1), reg);
     SWAPIN;
     if (next != NULL) {
	 r(0) = reg[0];
	 SET_I(next);
	 Dispatchfun();
     }
     goto find_func_info;
 }

 OpCase(i_call_fun_I): {
     BeamInstr *next;

     SWAPOUT;
     reg[0] = r(0);

     next = call_fun(c_p, Arg(0), reg, THE_NON_VALUE);
     SWAPIN;
     if (next != NULL) {
	 r(0) = reg[0];
	 SET_CP(c_p, I+2);
	 SET_I(next);
	 Dispatchfun();
     }
     goto find_func_info;
 }

 OpCase(i_call_fun_last_IP): {
     BeamInstr *next;

     SWAPOUT;
     reg[0] = r(0);
     next = call_fun(c_p, Arg(0), reg, THE_NON_VALUE);
     SWAPIN;
     if (next != NULL) {
	r(0) = reg[0];
	SET_CP(c_p, (BeamInstr *) EXPAND_POINTER(E[0]));
	E = ADD_BYTE_OFFSET(E, Arg(1));
	SET_I(next);
	Dispatchfun();
     }
     goto find_func_info;
 }

#ifdef DEBUG
    /*
     * Set a breakpoint here to get control just after a call instruction.
     * I points to the first instruction in the called function.
     *
     * In gdb, use 'call dis(I-5, 1)' to show the name of the function.
     */
 do_dispatch:
     DispatchMacro();

 do_dispatchx:
     DispatchMacrox();

 do_dispatchfun:
     DispatchMacroFun();

#endif

    /*
     * Jumped to from the Dispatch() macro when the reductions are used up.
     *
     * Since the I register points just beyond the FuncBegin instruction, we
     * can get the module, function, and arity for the function being
     * called from I[-3], I[-2], and I[-1] respectively.
     */
 context_switch_fun:
    c_p->arity = I[-1] + 1;
    goto context_switch2;

 context_switch:
    c_p->arity = I[-1];

 context_switch2:		/* Entry for fun calls. */
    c_p->current = I-3;		/* Pointer to Mod, Func, Arity */

 {
     Eterm* argp;
     int i;

     /*
      * Make sure that there is enough room for the argument registers to be saved.
      */
     if (c_p->arity > c_p->max_arg_reg) {
	 /*
	  * Yes, this is an expensive operation, but you only pay it the first
	  * time you call a function with more than 6 arguments which is
	  * scheduled out.  This is better than paying for 26 words of wasted
	  * space for most processes which never call functions with more than
	  * 6 arguments.
	  */
	 Uint size = c_p->arity * sizeof(c_p->arg_reg[0]);
	 if (c_p->arg_reg != c_p->def_arg_reg) {
	     c_p->arg_reg = (Eterm *) erts_realloc(ERTS_ALC_T_ARG_REG,
						   (void *) c_p->arg_reg,
						   size);
	 } else {
	     c_p->arg_reg = (Eterm *) erts_alloc(ERTS_ALC_T_ARG_REG, size);
	 }
	 c_p->max_arg_reg = c_p->arity;
     }

     /*
      * Since REDS_IN(c_p) is stored in the save area (c_p->arg_reg) we must read it
      * now before saving registers.
      *
      * The '+ 1' compensates for the last increment which was not done
      * (beacuse the code for the Dispatch() macro becomes shorter that way).
      */

     reds_used = REDS_IN(c_p) - FCALLS + 1;
     
     /*
      * Save the argument registers and everything else.
      */

     argp = c_p->arg_reg;
     for (i = c_p->arity - 1; i > 0; i--) {
	 argp[i] = reg[i];
     }
     c_p->arg_reg[0] = r(0);
     SWAPOUT;
     c_p->i = I;
     goto do_schedule1;
 }

 OpCase(set_tuple_element_sdP): {
     Eterm element;
     Eterm tuple;
     BeamInstr *next;
     Eterm* p;
     
     PreFetch(3, next);
     GetArg2(0, element, tuple);
     ASSERT(is_tuple(tuple));
     p = (Eterm *) ((unsigned char *) tuple_val(tuple) + Arg(2));
     *p = element;
     NextPF(3, next);
 }

 OpCase(i_is_ne_exact_f):
    if (EQ(tmp_arg1, tmp_arg2)) {
	ClauseFail();
    }
    Next(1);

 OpCase(normal_exit): {
     SWAPOUT;
     c_p->freason = EXC_NORMAL;
     c_p->arity = 0;		/* In case this process will ever be garbed again. */
     ERTS_SMP_UNREQ_PROC_MAIN_LOCK(c_p);
     erts_do_exit_process(c_p, am_normal);
     ERTS_SMP_REQ_PROC_MAIN_LOCK(c_p);
     goto do_schedule;
 }

 OpCase(continue_exit): {
     ERTS_SMP_UNREQ_PROC_MAIN_LOCK(c_p);
     erts_continue_exit_process(c_p);
     ERTS_SMP_REQ_PROC_MAIN_LOCK(c_p);
     goto do_schedule;
 }

 OpCase(raise_ss): {
     /* This was not done very well in R10-0; then, we passed the tag in
	the first argument and hoped that the existing c_p->ftrace was
	still correct. But the ftrace-object already includes the tag
	(or rather, the freason). Now, we pass the original ftrace in
	the first argument. We also handle atom tags in the first
	argument for backwards compatibility.
     */
     Eterm raise_val1;
     Eterm raise_val2;
     GetArg2(0, raise_val1, raise_val2);
     c_p->fvalue = raise_val2;
     if (c_p->freason == EXC_NULL) {
       /* a safety check for the R10-0 case; should not happen */
       c_p->ftrace = NIL;
       c_p->freason = EXC_ERROR;
     }
     /* for R10-0 code, keep existing c_p->ftrace and hope it's correct */
     switch (raise_val1) {
     case am_throw:
       c_p->freason = EXC_THROWN & ~EXF_SAVETRACE;
       break;
     case am_error:
       c_p->freason = EXC_ERROR & ~EXF_SAVETRACE;
       break;
     case am_exit:
       c_p->freason = EXC_EXIT & ~EXF_SAVETRACE;
       break;
     default:
       {/* R10-1 and later
	   XXX note: should do sanity check on given trace if it can be
	   passed from a user! Currently only expecting generated calls.
	*/
	 struct StackTrace *s;
	 c_p->ftrace = raise_val1;
	 s = get_trace_from_exc(raise_val1);
	 if (s == NULL) {
	   c_p->freason = EXC_ERROR;
	 } else {
	   c_p->freason = PRIMARY_EXCEPTION(s->freason);
	 }
       }
     }
     goto find_func_info;
 }

    {
	Eterm badmatch_val;

    OpCase(badmatch_y):
	badmatch_val = yb(Arg(0));
	goto do_badmatch;

    OpCase(badmatch_x):
	badmatch_val = xb(Arg(0));
	goto do_badmatch;

    OpCase(badmatch_r):
	badmatch_val = r(0);

    do_badmatch:
	c_p->fvalue = badmatch_val;
	c_p->freason = BADMATCH;
    }
 /* Fall through here */

 find_func_info: {
     reg[0] = r(0);
     SWAPOUT;
     I = handle_error(c_p, I, reg, NULL);
     goto post_error_handling;
 }

 OpCase(call_error_handler):
    /*
     * At this point, I points to the code[3] in the export entry for
     * a function which is not loaded.
     *
     * code[0]: Module
     * code[1]: Function
     * code[2]: Arity
     * code[3]: &&call_error_handler
     * code[4]: Not used
     */
    SWAPOUT;
    reg[0] = r(0);
    I = call_error_handler(c_p, I-3, reg, am_undefined_function);
    r(0) = reg[0];
    SWAPIN;
    if (I) {
	Goto(*I);
    }

 /* Fall through */
 OpCase(error_action_code): {
    handle_error:
     reg[0] = r(0);
     SWAPOUT;
     I = handle_error(c_p, NULL, reg, NULL);
 post_error_handling:
     if (I == 0) {
	 goto do_schedule;
     } else {
	 r(0) = reg[0];
	 ASSERT(!is_value(r(0)));
	 if (c_p->mbuf) {
	     erts_garbage_collect(c_p, 0, reg+1, 3);
	 }
	 SWAPIN;
	 Goto(*I);
     }
 }

    {
	Eterm nif_bif_result;
	Eterm bif_nif_arity;

    OpCase(call_nif):
	{
	    /*
	     * call_nif is always first instruction in function:
	     *
	     * I[-3]: Module
	     * I[-2]: Function
	     * I[-1]: Arity
	     * I[0]: &&call_nif
	     * I[1]: Function pointer to NIF function
	     * I[2]: Pointer to erl_module_nif
	     * I[3]: Function pointer to dirty NIF
	     */
	    BifFunction vbf;

	    DTRACE_NIF_ENTRY(c_p, (Eterm)I[-3], (Eterm)I[-2], (Uint)I[-1]);
	    c_p->current = I-3; /* current and vbf set to please handle_error */ 
	    SWAPOUT;
	    c_p->fcalls = FCALLS - 1;
	    PROCESS_MAIN_CHK_LOCKS(c_p);
	    bif_nif_arity = I[-1];
	    ERTS_SMP_UNREQ_PROC_MAIN_LOCK(c_p);

	    ASSERT(!ERTS_PROC_IS_EXITING(c_p));
	    {
		typedef Eterm NifF(struct enif_environment_t*, int argc, Eterm argv[]);
		NifF* fp = vbf = (NifF*) I[1];
		struct enif_environment_t env;
		erts_pre_nif(&env, c_p, (struct erl_module_nif*)I[2]);
		reg[0] = r(0);
		nif_bif_result = (*fp)(&env, bif_nif_arity, reg);
		if (env.exception_thrown)
		    nif_bif_result = THE_NON_VALUE;
		erts_post_nif(&env);
	    }
	    ASSERT(!ERTS_PROC_IS_EXITING(c_p) || is_non_value(nif_bif_result));
	    PROCESS_MAIN_CHK_LOCKS(c_p);
	    ERTS_VERIFY_UNUSED_TEMP_ALLOC(c_p);

	    DTRACE_NIF_RETURN(c_p, (Eterm)I[-3], (Eterm)I[-2], (Uint)I[-1]);
	    goto apply_bif_or_nif_epilogue;
	 
	OpCase(apply_bif):
	    /*
	     * At this point, I points to the code[3] in the export entry for
	     * the BIF:
	     *
	     * code[0]: Module
	     * code[1]: Function
	     * code[2]: Arity
	     * code[3]: &&apply_bif
	     * code[4]: Function pointer to BIF function
	     */

	    c_p->current = I-3;	/* In case we apply process_info/1,2 or load_nif/1 */
	    c_p->i = I;		/* In case we apply check_process_code/2. */
	    c_p->arity = 0;		/* To allow garbage collection on ourselves
					 * (check_process_code/2).
					 */
	    DTRACE_BIF_ENTRY(c_p, (Eterm)I[-3], (Eterm)I[-2], (Uint)I[-1]);

	    SWAPOUT;
	    c_p->fcalls = FCALLS - 1;
	    vbf = (BifFunction) Arg(0);
	    PROCESS_MAIN_CHK_LOCKS(c_p);
	    bif_nif_arity = I[-1];
            ASSERT(bif_nif_arity <= 4);
	    ERTS_SMP_UNREQ_PROC_MAIN_LOCK(c_p);
	    reg[0] = r(0);
	    {
		Eterm (*bf)(Process*, Eterm*, BeamInstr*) = vbf;
		ASSERT(!ERTS_PROC_IS_EXITING(c_p));
		nif_bif_result = (*bf)(c_p, reg, I);
		ASSERT(!ERTS_PROC_IS_EXITING(c_p) ||
		       is_non_value(nif_bif_result));
		ERTS_VERIFY_UNUSED_TEMP_ALLOC(c_p);
		PROCESS_MAIN_CHK_LOCKS(c_p);
	    }

	    DTRACE_BIF_RETURN(c_p, (Eterm)I[-3], (Eterm)I[-2], (Uint)I[-1]);

	apply_bif_or_nif_epilogue:
	    ERTS_SMP_REQ_PROC_MAIN_LOCK(c_p);
	    ERTS_HOLE_CHECK(c_p);
	    if (c_p->mbuf) {
		nif_bif_result = erts_gc_after_bif_call(c_p, nif_bif_result,
						  reg, bif_nif_arity);
	    }
	    SWAPIN;  /* There might have been a garbage collection. */
	    FCALLS = c_p->fcalls;
	    if (is_value(nif_bif_result)) {
		r(0) = nif_bif_result;
		CHECK_TERM(r(0));
		SET_I(c_p->cp);
		c_p->cp = 0;
		Goto(*I);
	    } else if (c_p->freason == TRAP) {
		SET_I(c_p->i);
		r(0) = reg[0];
		if (c_p->flags & F_HIBERNATE_SCHED) {
		    c_p->flags &= ~F_HIBERNATE_SCHED;
		    goto do_schedule;
		}
		Dispatch();
	    }
	    I = handle_error(c_p, c_p->cp, reg, vbf);
	    goto post_error_handling;
	}
    }

 OpCase(i_get_sd):
    {
	Eterm arg;
	Eterm result;

	GetArg1(0, arg);
	result = erts_pd_hash_get(c_p, arg);
	StoreBifResult(1, result);
    }

    {
	Eterm case_end_val;

    OpCase(case_end_x):
	case_end_val = xb(Arg(0));
	goto do_case_end;

    OpCase(case_end_y):
	case_end_val = yb(Arg(0));
	goto do_case_end;

    OpCase(case_end_r):
	case_end_val = r(0);

    do_case_end:
	c_p->fvalue = case_end_val;
	c_p->freason = EXC_CASE_CLAUSE;
	goto find_func_info;
    }

 OpCase(if_end):
    c_p->freason = EXC_IF_CLAUSE;
    goto find_func_info;

 OpCase(i_func_info_IaaI): {
     c_p->freason = EXC_FUNCTION_CLAUSE;
     c_p->current = I + 2;
     goto handle_error;
 }

 OpCase(try_case_end_s):
    {
	Eterm try_case_end_val;
	GetArg1(0, try_case_end_val);
	c_p->fvalue = try_case_end_val;
	c_p->freason = EXC_TRY_CLAUSE;
	goto find_func_info;
    }

 /*
  * Construction of binaries using new instructions.
  */
 {
     Eterm new_binary;
     Eterm num_bits_term;
     Uint num_bits;
     Uint alloc;
     Uint num_bytes;

     OpCase(i_bs_init_bits_heap_IIId): {
	 num_bits = Arg(0);
	 alloc = Arg(1);
	 I++;
	 goto do_bs_init_bits_known;
     }
     
     OpCase(i_bs_init_bits_IId): {
	 num_bits = Arg(0);
	 alloc = 0;
	 goto do_bs_init_bits_known;
     }

     OpCase(i_bs_init_bits_fail_heap_IjId): {
	 /* tmp_arg1 was fetched by an i_fetch instruction */
	 num_bits_term = tmp_arg1;
	 alloc = Arg(0);
	 I++;
	 goto do_bs_init_bits;
     }

     OpCase(i_bs_init_bits_fail_rjId): {
	 num_bits_term = r(0);
	 alloc = 0;
	 goto do_bs_init_bits;
     }
     OpCase(i_bs_init_bits_fail_yjId): {
	 num_bits_term = yb(Arg(0));
	 I++;
	 alloc = 0;
	 goto do_bs_init_bits;
     }
     OpCase(i_bs_init_bits_fail_xjId): {
	 num_bits_term = xb(Arg(0));
	 I++;
	 alloc = 0;
     /* FALL THROUGH */
     }

     /* num_bits_term = Term for number of bits to build (small/big)
      * alloc = Number of words to allocate on heap
      * Operands: Fail Live Dst
      */

 do_bs_init_bits:
     if (is_small(num_bits_term)) {
	 Sint size = signed_val(num_bits_term);
	 if (size < 0) {
	     goto badarg;
	 }
	 num_bits = (Uint) size;
     } else {
	 Uint bits;

	 if (!term_to_Uint(num_bits_term, &bits)) {
	     c_p->freason = bits;
	     goto lb_Cl_error;

	 }
	 num_bits = (Eterm) bits;
     }

     /* num_bits = Number of bits to build
      * alloc = Number of extra words to allocate on heap
      * Operands: NotUsed Live Dst
      */
 do_bs_init_bits_known:
     num_bytes = ((Uint64)num_bits+(Uint64)7) >> 3;
     if (num_bits & 7) {
	 alloc += ERL_SUB_BIN_SIZE;
     }
     if (num_bytes <= ERL_ONHEAP_BIN_LIMIT) {
	 alloc += heap_bin_size(num_bytes);
     } else {
	 alloc += PROC_BIN_SIZE;
     }
     TestHeap(alloc, Arg(1));

     /* num_bits = Number of bits to build
      * num_bytes = Number of bytes to allocate in the binary
      * alloc = Total number of words to allocate on heap
      * Operands: NotUsed NotUsed Dst
      */
     if (num_bytes <= ERL_ONHEAP_BIN_LIMIT) {
	 ErlHeapBin* hb;

	 erts_bin_offset = 0;
	 erts_writable_bin = 0;
	 hb = (ErlHeapBin *) HTOP;
	 HTOP += heap_bin_size(num_bytes);
	 hb->thing_word = header_heap_bin(num_bytes);
	 hb->size = num_bytes;
	 erts_current_bin = (byte *) hb->data;
	 new_binary = make_binary(hb);

     do_bits_sub_bin:
	 if (num_bits & 7) {
	     ErlSubBin* sb;

	     sb = (ErlSubBin *) HTOP;
	     HTOP += ERL_SUB_BIN_SIZE;
	     sb->thing_word = HEADER_SUB_BIN;
	     sb->size = num_bytes - 1;
	     sb->bitsize = num_bits & 7;
	     sb->offs = 0;
	     sb->bitoffs = 0;
	     sb->is_writable = 0;
	     sb->orig = new_binary;
	     new_binary = make_binary(sb);
	 }
	 HEAP_SPACE_VERIFIED(0);
	 StoreBifResult(2, new_binary);
     } else {
	 Binary* bptr;
	 ProcBin* pb;

	 erts_bin_offset = 0;
	 erts_writable_bin = 0;

	 /*
	  * Allocate the binary struct itself.
	  */
	 bptr = erts_bin_nrml_alloc(num_bytes);
	 erts_refc_init(&bptr->refc, 1);
	 erts_current_bin = (byte *) bptr->orig_bytes;

	 /*
	  * Now allocate the ProcBin on the heap.
	  */
	 pb = (ProcBin *) HTOP;
	 HTOP += PROC_BIN_SIZE;
	 pb->thing_word = HEADER_PROC_BIN;
	 pb->size = num_bytes;
	 pb->next = MSO(c_p).first;
	 MSO(c_p).first = (struct erl_off_heap_header*) pb;
	 pb->val = bptr;
	 pb->bytes = (byte*) bptr->orig_bytes;
	 pb->flags = 0;
	 OH_OVERHEAD(&(MSO(c_p)), pb->size / sizeof(Eterm));
	 new_binary = make_binary(pb);
	 goto do_bits_sub_bin;
     }
 }

 {
     OpCase(i_bs_init_fail_heap_IjId): {
	 /* tmp_arg1 was fetched by an i_fetch instruction */
	 tmp_arg2 = Arg(0);
	 I++;
	 goto do_bs_init;
     }

     OpCase(i_bs_init_fail_rjId): {
	 tmp_arg1 = r(0);
	 tmp_arg2 = 0;
	 goto do_bs_init;
     }

     OpCase(i_bs_init_fail_yjId): {
	 tmp_arg1 = yb(Arg(0));
	 tmp_arg2 = 0;
	 I++;
	 goto do_bs_init;
     }

     OpCase(i_bs_init_fail_xjId): {
	 tmp_arg1 = xb(Arg(0));
	 tmp_arg2 = 0;
	 I++;
     }
	 /* FALL THROUGH */
     do_bs_init:
         if (is_small(tmp_arg1)) {
	     Sint size = signed_val(tmp_arg1);
	     if (size < 0) {
		 goto badarg;
	     }
	     tmp_arg1 = (Eterm) size;
	 } else {
	     Uint bytes;

	     if (!term_to_Uint(tmp_arg1, &bytes)) {
		 c_p->freason = bytes;
		 goto lb_Cl_error;
	     }
	     if ((bytes >> (8*sizeof(Uint)-3)) != 0) {
		 goto system_limit;
	     }
	     tmp_arg1 = (Eterm) bytes;
	 }
	 if (tmp_arg1 <= ERL_ONHEAP_BIN_LIMIT) {
	     goto do_heap_bin_alloc;
	 } else {
	     goto do_proc_bin_alloc;
	 }


     OpCase(i_bs_init_heap_IIId): {
	 tmp_arg1 = Arg(0);
	 tmp_arg2 = Arg(1);
	 I++;
	 goto do_proc_bin_alloc;
     }

     OpCase(i_bs_init_IId): {
	 tmp_arg1 = Arg(0);
	 tmp_arg2 = 0;
     }
     /* FALL THROUGH */
     do_proc_bin_alloc: {
	 Binary* bptr;
	 ProcBin* pb;

	 erts_bin_offset = 0;
	 erts_writable_bin = 0;
	 TestBinVHeap(tmp_arg1 / sizeof(Eterm), 
	 	      tmp_arg2 + PROC_BIN_SIZE + ERL_SUB_BIN_SIZE, Arg(1));

	 /*
	  * Allocate the binary struct itself.
	  */
	 bptr = erts_bin_nrml_alloc(tmp_arg1);
	 erts_refc_init(&bptr->refc, 1);
	 erts_current_bin = (byte *) bptr->orig_bytes;

	 /*
	  * Now allocate the ProcBin on the heap.
	  */
	 pb = (ProcBin *) HTOP;
	 HTOP += PROC_BIN_SIZE;
	 pb->thing_word = HEADER_PROC_BIN;
	 pb->size = tmp_arg1;
	 pb->next = MSO(c_p).first;
	 MSO(c_p).first = (struct erl_off_heap_header*) pb;
	 pb->val = bptr;
	 pb->bytes = (byte*) bptr->orig_bytes;
	 pb->flags = 0;
	 
	 OH_OVERHEAD(&(MSO(c_p)), tmp_arg1 / sizeof(Eterm));

	 StoreBifResult(2, make_binary(pb));
     }

     OpCase(i_bs_init_heap_bin_heap_IIId): {
	 tmp_arg1 = Arg(0);
	 tmp_arg2 = Arg(1);
	 I++;
	 goto do_heap_bin_alloc;
     }

     OpCase(i_bs_init_heap_bin_IId): {
	 tmp_arg1 = Arg(0);
	 tmp_arg2 = 0;
     }
     /* Fall through */
     do_heap_bin_alloc:
	 {
	     ErlHeapBin* hb;
	     Uint bin_need;

	     bin_need = heap_bin_size(tmp_arg1);
	     erts_bin_offset = 0;
	     erts_writable_bin = 0;
	     TestHeap(bin_need+tmp_arg2+ERL_SUB_BIN_SIZE, Arg(1));
	     hb = (ErlHeapBin *) HTOP;
	     HTOP += bin_need;
	     hb->thing_word = header_heap_bin(tmp_arg1);
	     hb->size = tmp_arg1;
	     erts_current_bin = (byte *) hb->data;
	     tmp_arg1 = make_binary(hb);
	     StoreBifResult(2, tmp_arg1);
	 }
 }

 OpCase(i_bs_add_jId): {
     Uint Unit = Arg(1);
     if (is_both_small(tmp_arg1, tmp_arg2)) {
	 Sint Arg1 = signed_val(tmp_arg1);
	 Sint Arg2 = signed_val(tmp_arg2);

	 if (Arg1 >= 0 && Arg2 >= 0) {
	     BsSafeMul(Arg2, Unit, goto system_limit, tmp_arg1);
	     tmp_arg1 += Arg1;

	 store_bs_add_result:
	     if (MY_IS_SSMALL((Sint) tmp_arg1)) {
		 tmp_arg1 = make_small(tmp_arg1);
	     } else {
		 /*
		  * May generate a heap fragment, but in this
		  * particular case it is OK, since the value will be
		  * stored into an x register (the GC will scan x
		  * registers for references to heap fragments) and
		  * there is no risk that value can be stored into a
		  * location that is not scanned for heap-fragment
		  * references (such as the heap).
		  */
		 SWAPOUT;
		 tmp_arg1 = erts_make_integer(tmp_arg1, c_p);
		 HTOP = HEAP_TOP(c_p);
	     }
	     StoreBifResult(2, tmp_arg1);
	 }
	 goto badarg;
     } else {
	 Uint a;
	 Uint b;
	 Uint c;

	 /*
	  * Now we know that one of the arguments is
	  * not a small. We must convert both arguments
	  * to Uints and check for errors at the same time.
	  *
	  * Error checking is tricky.
	  *
	  * If one of the arguments is not numeric or
	  * not positive, the error reason is BADARG.
	  *
	  * Otherwise if both arguments are numeric,
	  * but at least one argument does not fit in
	  * an Uint, the reason is SYSTEM_LIMIT.
	  */

	 if (!term_to_Uint(tmp_arg1, &a)) {
	     if (a == BADARG) {
		 goto badarg;
	     }
	     if (!term_to_Uint(tmp_arg2, &b)) {
		 c_p->freason = b;
		 goto lb_Cl_error;
	     }
	     goto system_limit;
	 } else if (!term_to_Uint(tmp_arg2, &b)) {
	     c_p->freason = b;
	     goto lb_Cl_error;
	 }

	 /*
	  * The arguments are now correct and stored in a and b.
	  */
	 
	 BsSafeMul(b, Unit, goto system_limit, c);
	 tmp_arg1 = a + c;
	 if (tmp_arg1 < a) {
	     /*
	      * If the result is less than one of the
	      * arguments, there must have been an overflow.
	      */
	     goto system_limit;
	 }
	 goto store_bs_add_result;
     }
     /* No fallthrough */
     ASSERT(0);
 }

 OpCase(bs_put_string_II):
    {
	BeamInstr *next;
	PreFetch(2, next);
	erts_new_bs_put_string(ERL_BITS_ARGS_2((byte *) Arg(1), Arg(0)));
	NextPF(2, next);
    }

 /*
  * tmp_arg1 = Number of bytes to build
  * tmp_arg2 = Source binary
  * Operands: Fail ExtraHeap Live Unit Dst
  */

 OpCase(i_bs_append_jIIId): {
     Uint live = Arg(2);
     Uint res;

     SWAPOUT;
     reg[0] = r(0);
     reg[live] = tmp_arg2;
     res = erts_bs_append(c_p, reg, live, tmp_arg1, Arg(1), Arg(3));
     r(0) = reg[0];
     SWAPIN;
     if (is_non_value(res)) {
	 /* c_p->freason is already set (may be either BADARG or SYSTEM_LIMIT). */
	 goto lb_Cl_error;
     }
     StoreBifResult(4, res);
 }

 /*
  * tmp_arg1 = Number of bytes to build
  * tmp_arg2 = Source binary
  * Operands: Fail Unit Dst
  */
 OpCase(i_bs_private_append_jId): {
     Eterm res;

     res = erts_bs_private_append(c_p, tmp_arg2, tmp_arg1, Arg(1));
     if (is_non_value(res)) {
	 /* c_p->freason is already set (may be either BADARG or SYSTEM_LIMIT). */
	 goto lb_Cl_error;
     }
     StoreBifResult(2, res);
 }

 /*
  * tmp_arg1 = Initial size of writable binary
  * Operands: Live Dst
  */
 OpCase(bs_init_writable): {
     SWAPOUT;
     r(0) = erts_bs_init_writable(c_p, r(0));
     SWAPIN;
     Next(0);
 }

 /*
  * Calculate the number of bytes needed to encode the source
  * operarand to UTF-8. If the source operand is invalid (e.g. wrong
  * type or range) we return a nonsense integer result (0 or 4). We
  * can get away with that because we KNOW that bs_put_utf8 will do
  * full error checking.
  */
 OpCase(i_bs_utf8_size_sd): {
     Eterm arg;
     Eterm result;

     GetArg1(0, arg);
     if (arg < make_small(0x80UL)) {
	 result = make_small(1);
     } else if (arg < make_small(0x800UL)) {
	 result = make_small(2);
     } else if (arg < make_small(0x10000UL)) {
	 result = make_small(3);
     } else {
	 result = make_small(4);
     }
     StoreBifResult(1, result);
 }

 OpCase(i_bs_put_utf8_js): {
     Eterm arg;

     GetArg1(1, arg);
     if (!erts_bs_put_utf8(ERL_BITS_ARGS_1(arg))) {
	 goto badarg;
     }
     Next(2);
 }

 /*
  * Calculate the number of bytes needed to encode the source
  * operarand to UTF-8. If the source operand is invalid (e.g. wrong
  * type or range) we return a nonsense integer result (2 or 4). We
  * can get away with that because we KNOW that bs_put_utf16 will do
  * full error checking.
  */

 OpCase(i_bs_utf16_size_sd): {
     Eterm arg;
     Eterm result = make_small(2);

     GetArg1(0, arg);
     if (arg >= make_small(0x10000UL)) {
	 result = make_small(4);
     }
     StoreBifResult(1, result);
 }

 OpCase(i_bs_put_utf16_jIs): {
     Eterm arg;

     GetArg1(2, arg);
     if (!erts_bs_put_utf16(ERL_BITS_ARGS_2(arg, Arg(1)))) {
	 goto badarg;
     }
     Next(3);
 }

 /*
  * Only used for validating a value about to be stored in a binary.
  */
 OpCase(i_bs_validate_unicode_js): {
     Eterm val;

     GetArg1(1, val);

     /*
      * There is no need to untag the integer, but it IS necessary
      * to make sure it is small (if the term is a bignum, it could
      * slip through the test, and there is no further test that
      * would catch it, since bit syntax construction silently masks
      * too big numbers).
      */
     if (is_not_small(val) || val > make_small(0x10FFFFUL) ||
	 (make_small(0xD800UL) <= val && val <= make_small(0xDFFFUL))) {
	 goto badarg;
     }
     Next(2);
 }

 /*
  * Only used for validating a value matched out. 
  *
  * tmp_arg1 = Integer to validate
  * tmp_arg2 = Match context
  */
 OpCase(i_bs_validate_unicode_retract_j): {
     /*
      * There is no need to untag the integer, but it IS necessary
      * to make sure it is small (a bignum pointer could fall in
      * the valid range).
      */
     if (is_not_small(tmp_arg1) || tmp_arg1 > make_small(0x10FFFFUL) ||
	 (make_small(0xD800UL) <= tmp_arg1 &&
	  tmp_arg1 <= make_small(0xDFFFUL))) {
	 ErlBinMatchBuffer *mb = ms_matchbuffer(tmp_arg2);

	 mb->offset -= 32;
	 goto badarg;
     }
     Next(1);
 }

 /*
  * Matching of binaries.
  */

 {
     Eterm header;
     BeamInstr *next;
     Uint slots;
     Eterm context;

     OpCase(i_bs_start_match2_rfIId): {
	 context = r(0);

     do_start_match:
	 slots = Arg(2);
	 if (!is_boxed(context)) {
	     ClauseFail();
	 }
	 PreFetch(4, next);
	 header = *boxed_val(context);
	 if (header_is_bin_matchstate(header)) {
	     ErlBinMatchState* ms = (ErlBinMatchState *) boxed_val(context);
	     Uint actual_slots = HEADER_NUM_SLOTS(header);
	     ms->save_offset[0] = ms->mb.offset;
	     if (actual_slots < slots) {
		 ErlBinMatchState* dst;
		 Uint live = Arg(1);
		 Uint wordsneeded = ERL_BIN_MATCHSTATE_SIZE(slots);

		 TestHeapPreserve(wordsneeded, live, context);
		 ms = (ErlBinMatchState *) boxed_val(context);
		 dst = (ErlBinMatchState *) HTOP;
		 *dst = *ms;
		 *HTOP = HEADER_BIN_MATCHSTATE(slots);
		 HTOP += wordsneeded;
		 HEAP_SPACE_VERIFIED(0);
		 StoreResult(make_matchstate(dst), Arg(3));
	     }
	 } else if (is_binary_header(header)) {
	     Eterm result;
	     Uint live = Arg(1);
	     Uint wordsneeded = ERL_BIN_MATCHSTATE_SIZE(slots);
	     TestHeapPreserve(wordsneeded, live, context);
	     HEAP_TOP(c_p) = HTOP;
#ifdef DEBUG
	     c_p->stop = E;	/* Needed for checking in HeapOnlyAlloc(). */
#endif
	     result = erts_bs_start_match_2(c_p, context, slots);
	     HTOP = HEAP_TOP(c_p);
	     HEAP_SPACE_VERIFIED(0);
	     if (is_non_value(result)) {
		 ClauseFail();
	     } else {
		 StoreResult(result, Arg(3));
	     }
	 } else {
	     ClauseFail();
	 }
	 NextPF(4, next);
     }
     OpCase(i_bs_start_match2_xfIId): {
	 context = xb(Arg(0));
	 I++;
	 goto do_start_match;
     }
     OpCase(i_bs_start_match2_yfIId): {
	 context = yb(Arg(0));
	 I++;
	 goto do_start_match;
     }
 }

 OpCase(bs_test_zero_tail2_fr): {
     BeamInstr *next;
     ErlBinMatchBuffer *_mb;
     
     PreFetch(1, next);
     _mb = (ErlBinMatchBuffer*) ms_matchbuffer(r(0));
     if (_mb->size != _mb->offset) {
	 ClauseFail();
     }
     NextPF(1, next);
 }

 OpCase(bs_test_zero_tail2_fx): {
     BeamInstr *next;
     ErlBinMatchBuffer *_mb;
     
     PreFetch(2, next);
     _mb = (ErlBinMatchBuffer*) ms_matchbuffer(xb(Arg(1)));
     if (_mb->size != _mb->offset) {
	 ClauseFail();
     }
     NextPF(2, next);
 }

 OpCase(bs_test_tail_imm2_frI): {
     BeamInstr *next;
     ErlBinMatchBuffer *_mb;
     PreFetch(2, next);
     _mb = ms_matchbuffer(r(0));
     if (_mb->size - _mb->offset != Arg(1)) {
	 ClauseFail();
     }
     NextPF(2, next);
 }
 OpCase(bs_test_tail_imm2_fxI): {
     BeamInstr *next;
     ErlBinMatchBuffer *_mb;
     PreFetch(3, next);
     _mb = ms_matchbuffer(xb(Arg(1)));
     if (_mb->size - _mb->offset != Arg(2)) {
	 ClauseFail();
     }
     NextPF(3, next);
 }

 OpCase(bs_test_unit_frI): {
     BeamInstr *next;
     ErlBinMatchBuffer *_mb;
     PreFetch(2, next);
     _mb = ms_matchbuffer(r(0));
     if ((_mb->size - _mb->offset) % Arg(1)) {
	 ClauseFail();
     }
     NextPF(2, next);
 }
 OpCase(bs_test_unit_fxI): {
     BeamInstr *next;
     ErlBinMatchBuffer *_mb;
     PreFetch(3, next);
     _mb = ms_matchbuffer(xb(Arg(1)));
     if ((_mb->size - _mb->offset) % Arg(2)) {
	 ClauseFail();
     }
     NextPF(3, next);
 }

 OpCase(bs_test_unit8_fr): {
     BeamInstr *next;
     ErlBinMatchBuffer *_mb;
     PreFetch(1, next);
     _mb = ms_matchbuffer(r(0));
     if ((_mb->size - _mb->offset) & 7) {
	 ClauseFail();
     }
     NextPF(1, next);
 }
 OpCase(bs_test_unit8_fx): {
     BeamInstr *next;
     ErlBinMatchBuffer *_mb;
     PreFetch(2, next);
     _mb = ms_matchbuffer(xb(Arg(1)));
     if ((_mb->size - _mb->offset) & 7) {
	 ClauseFail();
     }
     NextPF(2, next);
 }

 {
     Eterm bs_get_integer8_context;

 OpCase(i_bs_get_integer_8_rfd): {
	 bs_get_integer8_context = r(0);
	 goto do_bs_get_integer_8;
     }

 OpCase(i_bs_get_integer_8_xfd): {
	 bs_get_integer8_context = xb(Arg(0));
	 I++;
     }

 do_bs_get_integer_8: {
	 ErlBinMatchBuffer *_mb;
	 Eterm _result;
	 _mb = ms_matchbuffer(bs_get_integer8_context);
	 if (_mb->size - _mb->offset < 8) {
	     ClauseFail();
	 }
	 if (BIT_OFFSET(_mb->offset) != 0) {
	     _result = erts_bs_get_integer_2(c_p, 8, 0, _mb);
	 } else {
	     _result = make_small(_mb->base[BYTE_OFFSET(_mb->offset)]);
	     _mb->offset += 8;
	 }
	 StoreBifResult(1, _result);
     }
 }

 {
     Eterm bs_get_integer_16_context;

 OpCase(i_bs_get_integer_16_rfd):
     bs_get_integer_16_context = r(0);
     goto do_bs_get_integer_16;

 OpCase(i_bs_get_integer_16_xfd):
     bs_get_integer_16_context = xb(Arg(0));
     I++;

 do_bs_get_integer_16:
     {
	 ErlBinMatchBuffer *_mb;
	 Eterm _result;
	 _mb = ms_matchbuffer(bs_get_integer_16_context);
	 if (_mb->size - _mb->offset < 16) {
	     ClauseFail(); 
	 }
	 if (BIT_OFFSET(_mb->offset) != 0) {
	     _result = erts_bs_get_integer_2(c_p, 16, 0, _mb);
	 } else {
	     _result = make_small(get_int16(_mb->base+BYTE_OFFSET(_mb->offset)));
	     _mb->offset += 16;
	 }
	 StoreBifResult(1, _result);
     }
 }

 {
     Eterm bs_get_integer_32_context;

 OpCase(i_bs_get_integer_32_rfId):
     bs_get_integer_32_context = r(0);
     goto do_bs_get_integer_32;

     
 OpCase(i_bs_get_integer_32_xfId):
     bs_get_integer_32_context = xb(Arg(0));
     I++;


 do_bs_get_integer_32:
     {
	 ErlBinMatchBuffer *_mb;
	 Uint32 _integer;
	 Eterm _result;
	 _mb = ms_matchbuffer(bs_get_integer_32_context);
	 if (_mb->size - _mb->offset < 32) { ClauseFail(); }
	 if (BIT_OFFSET(_mb->offset) != 0) {
	     _integer = erts_bs_get_unaligned_uint32(_mb);
	 } else {
	     _integer = get_int32(_mb->base + _mb->offset/8);
	 }
	 _mb->offset += 32;
#if !defined(ARCH_64) || HALFWORD_HEAP
	 if (IS_USMALL(0, _integer)) {
#endif
	     _result = make_small(_integer);
#if !defined(ARCH_64) || HALFWORD_HEAP
	 } else {
	     TestHeap(BIG_UINT_HEAP_SIZE, Arg(1));
	     _result = uint_to_big((Uint) _integer, HTOP);
	     HTOP += BIG_UINT_HEAP_SIZE;
	     HEAP_SPACE_VERIFIED(0);
	 }
#endif
	 StoreBifResult(2, _result);
     }
 }

 /* Operands: Size Live Fail Flags Dst */
 OpCase(i_bs_get_integer_imm_rIIfId): {
     tmp_arg1 = r(0);
     /* Operands: Size Live Fail Flags Dst */
     goto do_bs_get_integer_imm_test_heap;
 }

 /* Operands: x(Reg) Size Live Fail Flags Dst */
 OpCase(i_bs_get_integer_imm_xIIfId): {
     tmp_arg1 = xb(Arg(0));
     I++;
     /* Operands: Size Live Fail Flags Dst */
     goto do_bs_get_integer_imm_test_heap;
 }

 /*
  * tmp_arg1 = match context
  * Operands: Size Live Fail Flags Dst
  */
 do_bs_get_integer_imm_test_heap: {
     Uint wordsneeded;
     tmp_arg2 = Arg(0);
     wordsneeded = 1+WSIZE(NBYTES(tmp_arg2));
     TestHeapPreserve(wordsneeded, Arg(1), tmp_arg1);
     I += 2;
     /* Operands: Fail Flags Dst */
     goto do_bs_get_integer_imm;
 }

 /* Operands: Size Fail Flags Dst */
 OpCase(i_bs_get_integer_small_imm_rIfId): {
     tmp_arg1 = r(0);
     tmp_arg2 = Arg(0);
     I++;
     /* Operands: Fail Flags Dst */
     goto do_bs_get_integer_imm;
 }

 /* Operands: x(Reg) Size Fail Flags Dst */
 OpCase(i_bs_get_integer_small_imm_xIfId): {
     tmp_arg1 = xb(Arg(0));
     tmp_arg2 = Arg(1);
     I += 2;
     /* Operands: Fail Flags Dst */
     goto do_bs_get_integer_imm;
 }
 
 /*
  * tmp_arg1 = match context
  * tmp_arg2 = size of field
  * Operands: Fail Flags Dst
  */
 do_bs_get_integer_imm: {
     ErlBinMatchBuffer* mb;
     Eterm result;

     mb = ms_matchbuffer(tmp_arg1);
     LIGHT_SWAPOUT;
     result = erts_bs_get_integer_2(c_p, tmp_arg2, Arg(1), mb);
     LIGHT_SWAPIN;
     HEAP_SPACE_VERIFIED(0);
     if (is_non_value(result)) {
	 ClauseFail();
     }
     StoreBifResult(2, result);
 }

 /*
  * tmp_arg1 = Match context
  * tmp_arg2 = Size field
  * Operands: Fail Live FlagsAndUnit Dst
  */
 OpCase(i_bs_get_integer_fIId): {
     Uint flags;
     Uint size;
     ErlBinMatchBuffer* mb;
     Eterm result;

     flags = Arg(2);
     BsGetFieldSize(tmp_arg2, (flags >> 3), ClauseFail(), size);
     if (size >= SMALL_BITS) {
	 Uint wordsneeded;
	 /* check bits size before potential gc.
	  * We do not want a gc and then realize we don't need
	  * the allocated space (i.e. if the op fails)
	  *
	  * remember to reacquire the matchbuffer after gc.
	  */

	 mb = ms_matchbuffer(tmp_arg1);
	 if (mb->size - mb->offset < size) {
	     ClauseFail();
	 }
	 wordsneeded = 1+WSIZE(NBYTES((Uint) size));
	 TestHeapPreserve(wordsneeded, Arg(1), tmp_arg1);
     }
     mb = ms_matchbuffer(tmp_arg1);
     LIGHT_SWAPOUT;
     result = erts_bs_get_integer_2(c_p, size, flags, mb);
     LIGHT_SWAPIN;
     HEAP_SPACE_VERIFIED(0);
     if (is_non_value(result)) {
	 ClauseFail();
     }
     StoreBifResult(3, result);
 }

 {
     Eterm get_utf8_context;

     /* Operands: MatchContext Fail Dst */
 OpCase(i_bs_get_utf8_rfd): {
	 get_utf8_context = r(0);
	 goto do_bs_get_utf8;
     }

 OpCase(i_bs_get_utf8_xfd): {
	 get_utf8_context = xb(Arg(0));
	 I++;
     }

     /*
      * get_utf8_context = match_context
      * Operands: Fail Dst
      */

 do_bs_get_utf8: {
	 Eterm result = erts_bs_get_utf8(ms_matchbuffer(get_utf8_context));
	 if (is_non_value(result)) {
	     ClauseFail();
	 }
	 StoreBifResult(1, result);
     }
 }

 {
     Eterm get_utf16_context;

     /* Operands: MatchContext Fail Flags Dst */
 OpCase(i_bs_get_utf16_rfId): {
	 get_utf16_context = r(0);
	 goto do_bs_get_utf16;
     }

 OpCase(i_bs_get_utf16_xfId): {
	 get_utf16_context = xb(Arg(0));
	 I++;
     }

     /*
      * get_utf16_context = match_context
      * Operands: Fail Flags Dst
      */
 do_bs_get_utf16: {
	 Eterm result = erts_bs_get_utf16(ms_matchbuffer(get_utf16_context),
					  Arg(1));
	 if (is_non_value(result)) {
	     ClauseFail();
	 }
	 StoreBifResult(2, result);
     }
 }

 {
     Eterm context_to_binary_context;
     ErlBinMatchBuffer* mb;
     ErlSubBin* sb;
     Uint size;
     Uint offs;
     Uint orig;
     Uint hole_size;

     OpCase(bs_context_to_binary_r): {
	 context_to_binary_context = x0;
	 I -= 2;
	 goto do_context_to_binary;
     }

     /* Unfortunately, inlining can generate this instruction. */
     OpCase(bs_context_to_binary_y): {
	 context_to_binary_context = yb(Arg(0));
	 goto do_context_to_binary0;
     }

     OpCase(bs_context_to_binary_x): {
	 context_to_binary_context = xb(Arg(0));
     
     do_context_to_binary0:
	 I--;
     }

 do_context_to_binary:
     if (is_boxed(context_to_binary_context) &&
	 header_is_bin_matchstate(*boxed_val(context_to_binary_context))) {
	 ErlBinMatchState* ms;
	 ms = (ErlBinMatchState *) boxed_val(context_to_binary_context);
	 mb = &ms->mb;
	 offs = ms->save_offset[0];
	 size = mb->size - offs;
	 goto do_bs_get_binary_all_reuse_common;
     }
     Next(2);

     OpCase(i_bs_get_binary_all_reuse_rfI): {
	 context_to_binary_context = x0;
	 goto do_bs_get_binary_all_reuse;
     }

     OpCase(i_bs_get_binary_all_reuse_xfI): {
	 context_to_binary_context = xb(Arg(0));
	 I++;
     }

 do_bs_get_binary_all_reuse:
     mb = ms_matchbuffer(context_to_binary_context);
     size = mb->size - mb->offset;
     if (size % Arg(1) != 0) {
	 ClauseFail();
     }
     offs = mb->offset;

 do_bs_get_binary_all_reuse_common:
     orig = mb->orig;
     sb = (ErlSubBin *) boxed_val(context_to_binary_context);
     hole_size = 1 + header_arity(sb->thing_word) - ERL_SUB_BIN_SIZE;
     sb->thing_word = HEADER_SUB_BIN;
     sb->size = BYTE_OFFSET(size);
     sb->bitsize = BIT_OFFSET(size);
     sb->offs = BYTE_OFFSET(offs);
     sb->bitoffs = BIT_OFFSET(offs);
     sb->is_writable = 0;
     sb->orig = orig;
     if (hole_size) {
	 sb[1].thing_word = make_pos_bignum_header(hole_size-1);
     }
     Next(2);
 }

 {
     Eterm match_string_context;

     OpCase(i_bs_match_string_rfII): {
	 match_string_context = r(0);
	 goto do_bs_match_string;
     }
     OpCase(i_bs_match_string_xfII): {
	 match_string_context = xb(Arg(0));
	 I++;
     }

 do_bs_match_string:
     {
	 BeamInstr *next;
	 byte* bytes;
	 Uint bits;
	 ErlBinMatchBuffer* mb;
	 Uint offs;

	 PreFetch(3, next);
	 bits = Arg(1);
	 bytes = (byte *) Arg(2);
	 mb = ms_matchbuffer(match_string_context);
	 if (mb->size - mb->offset < bits) {
	     ClauseFail();
	 }
	 offs = mb->offset & 7;
	 if (offs == 0 && (bits & 7) == 0) {
	     if (sys_memcmp(bytes, mb->base+(mb->offset>>3), bits>>3)) {
		 ClauseFail();
	     }
	 } else if (erts_cmp_bits(bytes, 0, mb->base+(mb->offset>>3), mb->offset & 7, bits)) {
	     ClauseFail();
	 }
	 mb->offset += bits;
	 NextPF(3, next);
     }
 }

 OpCase(i_bs_save2_rI): {
     BeamInstr *next;
     ErlBinMatchState *_ms;
     PreFetch(1, next);
     _ms = (ErlBinMatchState*) boxed_val((Eterm) r(0));
     _ms->save_offset[Arg(0)] = _ms->mb.offset;
     NextPF(1, next);
 }
 OpCase(i_bs_save2_xI): {
     BeamInstr *next;
     ErlBinMatchState *_ms;
     PreFetch(2, next);
     _ms = (ErlBinMatchState*) boxed_val((Eterm) xb(Arg(0)));
     _ms->save_offset[Arg(1)] = _ms->mb.offset;
     NextPF(2, next);
 }

 OpCase(i_bs_restore2_rI): {
     BeamInstr *next;
     ErlBinMatchState *_ms;
     PreFetch(1, next);
     _ms = (ErlBinMatchState*) boxed_val((Eterm) r(0));
     _ms->mb.offset = _ms->save_offset[Arg(0)];
     NextPF(1, next);
 }
 OpCase(i_bs_restore2_xI): {
     BeamInstr *next;
     ErlBinMatchState *_ms;
     PreFetch(2, next);
     _ms = (ErlBinMatchState*) boxed_val((Eterm) xb(Arg(0)));
     _ms->mb.offset = _ms->save_offset[Arg(1)];
     NextPF(2, next);
 }

#include "beam_cold.h"


 /*
  * This instruction is probably never used (because it is combined with a
  * a return). However, a future compiler might for some reason emit a
  * deallocate not followed by a return, and that should work.
  */
 OpCase(deallocate_I): {
     BeamInstr *next;

     PreFetch(1, next);
     D(Arg(0));
     NextPF(1, next);
 }

    /*
     * Trace and debugging support.
     */

 OpCase(return_trace): {
     BeamInstr* code = (BeamInstr *) (UWord) E[0];
     
     SWAPOUT;		/* Needed for shared heap */
     ERTS_SMP_UNREQ_PROC_MAIN_LOCK(c_p);
     erts_trace_return(c_p, code, r(0), E+1/*Process tracer*/);
     ERTS_SMP_REQ_PROC_MAIN_LOCK(c_p);
     SWAPIN;
     c_p->cp = NULL;
     SET_I((BeamInstr *) cp_val(E[2]));
     E += 3;
     Goto(*I);
 }

 OpCase(i_generic_breakpoint): {
     BeamInstr real_I;
     ASSERT(I[-5] == (BeamInstr) BeamOp(op_i_func_info_IaaI));
     SWAPOUT;
     reg[0] = r(0);
     real_I = erts_generic_breakpoint(c_p, I, reg);
     r(0) = reg[0];
     SWAPIN;
     ASSERT(VALID_INSTR(real_I));
     Goto(real_I);
 }

 OpCase(i_return_time_trace): {
     BeamInstr *pc = (BeamInstr *) (UWord) E[0];
     SWAPOUT;
     erts_trace_time_return(c_p, pc);
     SWAPIN;
     c_p->cp = NULL;
     SET_I((BeamInstr *) cp_val(E[1]));
     E += 2;
     Goto(*I);
 }

 OpCase(i_return_to_trace): {
     if (IS_TRACED_FL(c_p, F_TRACE_RETURN_TO)) {
	 Uint *cpp = (Uint*) E;
	 for(;;) {
	     ASSERT(is_CP(*cpp));
	     if (*cp_val(*cpp) == (BeamInstr) OpCode(return_trace)) {
		 do ++cpp; while(is_not_CP(*cpp));
		 cpp += 2;
	     } else if (*cp_val(*cpp) == (BeamInstr) OpCode(i_return_to_trace)) {
		 do ++cpp; while(is_not_CP(*cpp));
	     } else break;
	 }
	 SWAPOUT;		/* Needed for shared heap */
	 ERTS_SMP_UNREQ_PROC_MAIN_LOCK(c_p);
	 erts_trace_return_to(c_p, cp_val(*cpp));
	 ERTS_SMP_REQ_PROC_MAIN_LOCK(c_p);
	 SWAPIN;
     }
     c_p->cp = NULL;
     SET_I((BeamInstr *) cp_val(E[0]));
     E += 1;
     Goto(*I);
 }

 /*
  * New floating point instructions.
  */

 OpCase(fmove_ql): {
     Eterm fr = Arg(1);
     BeamInstr *next;

     PreFetch(2, next);
     GET_DOUBLE(Arg(0), *(FloatDef*)ADD_BYTE_OFFSET(freg, fr));
     NextPF(2, next);
 }

 OpCase(fmove_dl): {
     Eterm targ1;
     Eterm fr = Arg(1);
     BeamInstr *next;

     PreFetch(2, next);
     GetR(0, targ1);
     /* Arg(0) == HEADER_FLONUM */
     GET_DOUBLE(targ1, *(FloatDef*)ADD_BYTE_OFFSET(freg, fr));
     NextPF(2, next);
 }

 OpCase(fmove_ld): {
     Eterm fr = Arg(0);
     Eterm dest = make_float(HTOP);

     PUT_DOUBLE(*(FloatDef*)ADD_BYTE_OFFSET(freg, fr), HTOP);
     HTOP += FLOAT_SIZE_OBJECT;
     StoreBifResult(1, dest);
 }

 OpCase(fconv_dl): {
     Eterm targ1;
     Eterm fr = Arg(1);
     BeamInstr *next;

     GetR(0, targ1);
     PreFetch(2, next);
     if (is_small(targ1)) {
	 fb(fr) = (double) signed_val(targ1);
     } else if (is_big(targ1)) {
	 if (big_to_double(targ1, &fb(fr)) < 0) {
	     goto fbadarith;
	 }
     } else if (is_float(targ1)) {
	 GET_DOUBLE(targ1, *(FloatDef*)ADD_BYTE_OFFSET(freg, fr));
     } else {
	 goto fbadarith;
     }
     NextPF(2, next);
 }

#ifdef NO_FPE_SIGNALS
     OpCase(fclearerror):
     OpCase(i_fcheckerror):
	 erl_exit(1, "fclearerror/i_fcheckerror without fpe signals (beam_emu)");
#  define ERTS_NO_FPE_CHECK_INIT ERTS_FP_CHECK_INIT
#  define ERTS_NO_FPE_ERROR ERTS_FP_ERROR
#else
#  define ERTS_NO_FPE_CHECK_INIT(p)
#  define ERTS_NO_FPE_ERROR(p, a, b)

     OpCase(fclearerror): {
	 BeamInstr *next;

	 PreFetch(0, next);
	 ERTS_FP_CHECK_INIT(c_p);
	 NextPF(0, next);
     }

     OpCase(i_fcheckerror): {
	 BeamInstr *next;

	 PreFetch(0, next);
	 ERTS_FP_ERROR(c_p, freg[0].fd, goto fbadarith);
	 NextPF(0, next);
     }
#endif


 OpCase(i_fadd_lll): {
     BeamInstr *next;

     PreFetch(3, next);
     ERTS_NO_FPE_CHECK_INIT(c_p);
     fb(Arg(2)) = fb(Arg(0)) + fb(Arg(1));
     ERTS_NO_FPE_ERROR(c_p, fb(Arg(2)), goto fbadarith);
     NextPF(3, next);
 }
 OpCase(i_fsub_lll): {
     BeamInstr *next;

     PreFetch(3, next);
     ERTS_NO_FPE_CHECK_INIT(c_p);
     fb(Arg(2)) = fb(Arg(0)) - fb(Arg(1));
     ERTS_NO_FPE_ERROR(c_p, fb(Arg(2)), goto fbadarith);
     NextPF(3, next);
 }
 OpCase(i_fmul_lll): {
     BeamInstr *next;

     PreFetch(3, next);
     ERTS_NO_FPE_CHECK_INIT(c_p);
     fb(Arg(2)) = fb(Arg(0)) * fb(Arg(1));
     ERTS_NO_FPE_ERROR(c_p, fb(Arg(2)), goto fbadarith);
     NextPF(3, next);
 }
 OpCase(i_fdiv_lll): {
     BeamInstr *next;

     PreFetch(3, next);
     ERTS_NO_FPE_CHECK_INIT(c_p);
     fb(Arg(2)) = fb(Arg(0)) / fb(Arg(1));
     ERTS_NO_FPE_ERROR(c_p, fb(Arg(2)), goto fbadarith);
     NextPF(3, next);
 }
 OpCase(i_fnegate_ll): {
     BeamInstr *next;

     PreFetch(2, next);
     ERTS_NO_FPE_CHECK_INIT(c_p);
     fb(Arg(1)) = -fb(Arg(0));
     ERTS_NO_FPE_ERROR(c_p, fb(Arg(1)), goto fbadarith);
     NextPF(2, next);

 fbadarith:
     c_p->freason = BADARITH;
     goto find_func_info;
 }

#ifdef HIPE
 {
     unsigned cmd;

     OpCase(hipe_trap_call): {
	 /*
	  * I[-5]: &&lb_i_func_info_IaaI
	  * I[-4]: Native code callee (inserted by HiPE)
	  * I[-3]: Module (tagged atom)
	  * I[-2]: Function (tagged atom)
	  * I[-1]: Arity (untagged integer)
	  * I[ 0]: &&lb_hipe_trap_call
	  * ... remainder of original BEAM code
	  */
	 ASSERT(I[-5] == (Uint) OpCode(i_func_info_IaaI));
	 c_p->hipe.u.ncallee = (void(*)(void)) I[-4];
	 cmd = HIPE_MODE_SWITCH_CMD_CALL | (I[-1] << 8);
	 ++hipe_trap_count;
	 goto L_hipe_mode_switch;
     }
     OpCase(hipe_trap_call_closure): {
       ASSERT(I[-5] == (Uint) OpCode(i_func_info_IaaI));
       c_p->hipe.u.ncallee = (void(*)(void)) I[-4];
       cmd = HIPE_MODE_SWITCH_CMD_CALL_CLOSURE | (I[-1] << 8);
       ++hipe_trap_count;
       goto L_hipe_mode_switch;
     }
     OpCase(hipe_trap_return): {
	 cmd = HIPE_MODE_SWITCH_CMD_RETURN;
	 goto L_hipe_mode_switch;
     }
     OpCase(hipe_trap_throw): {
	 cmd = HIPE_MODE_SWITCH_CMD_THROW;
	 goto L_hipe_mode_switch;
     }
     OpCase(hipe_trap_resume): {
	 cmd = HIPE_MODE_SWITCH_CMD_RESUME;
	 goto L_hipe_mode_switch;
     }
 L_hipe_mode_switch:
     /* XXX: this abuse of def_arg_reg[] is horrid! */
     SWAPOUT;
     c_p->fcalls = FCALLS;
     c_p->def_arg_reg[4] = -neg_o_reds;
     reg[0] = r(0);
     c_p = hipe_mode_switch(c_p, cmd, reg);
     reg = ERTS_PROC_GET_SCHDATA(c_p)->x_reg_array;
     freg = ERTS_PROC_GET_SCHDATA(c_p)->f_reg_array;
     ERL_BITS_RELOAD_STATEP(c_p);
     neg_o_reds = -c_p->def_arg_reg[4];
     FCALLS = c_p->fcalls;
     SWAPIN;
     switch( c_p->def_arg_reg[3] ) { /* Halfword wont work with hipe yet! */
       case HIPE_MODE_SWITCH_RES_RETURN:
	 ASSERT(is_value(reg[0]));
	 MoveReturn(reg[0], r(0));
       case HIPE_MODE_SWITCH_RES_CALL_EXPORTED:
	 c_p->i = c_p->hipe.u.callee_exp->addressv[erts_active_code_ix()];
	 /*fall through*/
       case HIPE_MODE_SWITCH_RES_CALL_BEAM:
	 SET_I(c_p->i);
	 r(0) = reg[0];
	 Dispatch();
       case HIPE_MODE_SWITCH_RES_CALL_CLOSURE:
	 /* This can be used to call any function value, but currently it's
	    only used to call closures referring to unloaded modules. */
	 {
	     BeamInstr *next;

	     next = call_fun(c_p, c_p->arity - 1, reg, THE_NON_VALUE);
	     SWAPIN;
	     if (next != NULL) {
		 r(0) = reg[0];
		 SET_I(next);
		 Dispatchfun();
	     }
	     goto find_func_info;
	 }
       case HIPE_MODE_SWITCH_RES_THROW:
	 c_p->cp = NULL;
	 I = handle_error(c_p, I, reg, NULL);
	 goto post_error_handling;
       default:
	 erl_exit(1, "hipe_mode_switch: result %u\n", c_p->def_arg_reg[3]);
     }
 }
 OpCase(hipe_call_count): {
     /*
      * I[-5]: &&lb_i_func_info_IaaI
      * I[-4]: pointer to struct hipe_call_count (inserted by HiPE)
      * I[-3]: Module (tagged atom)
      * I[-2]: Function (tagged atom)
      * I[-1]: Arity (untagged integer)
      * I[ 0]: &&lb_hipe_call_count
      * ... remainder of original BEAM code
      */
     struct hipe_call_count *hcc = (struct hipe_call_count*)I[-4];
     ASSERT(I[-5] == (Uint) OpCode(i_func_info_IaaI));
     ASSERT(hcc != NULL);
     ASSERT(VALID_INSTR(hcc->opcode));
     ++(hcc->count);
     Goto(hcc->opcode);
 }
#endif /* HIPE */

 OpCase(i_yield):
 {
     /* This is safe as long as REDS_IN(c_p) is never stored 
      * in c_p->arg_reg[0]. It is currently stored in c_p->def_arg_reg[5],
      * which may be c_p->arg_reg[5], which is close, but no banana.
      */
     c_p->arg_reg[0] = am_true;
     c_p->arity = 1; /* One living register (the 'true' return value) */
     SWAPOUT;
     c_p->i = I + 1; /* Next instruction */
     c_p->current = NULL;
     goto do_schedule;
 }

 OpCase(i_hibernate): {
     SWAPOUT;
     if (erts_hibernate(c_p, r(0), x(1), x(2), reg)) {
	 c_p->flags &= ~F_HIBERNATE_SCHED;
	 goto do_schedule;
     } else {
	 I = handle_error(c_p, I, reg, hibernate_3);
	 goto post_error_handling;
     }
 }

 OpCase(i_debug_breakpoint): {
     SWAPOUT;
     reg[0] = r(0);
     I = call_error_handler(c_p, I-3, reg, am_breakpoint);
     r(0) = reg[0];
     SWAPIN;
     if (I) {
	 Goto(*I);
     }
     goto handle_error;
 }


 OpCase(system_limit_j):
 system_limit:
    c_p->freason = SYSTEM_LIMIT;
    goto lb_Cl_error;


#ifdef ERTS_OPCODE_COUNTER_SUPPORT
    DEFINE_COUNTING_LABELS;
#endif

#ifndef NO_JUMP_TABLE
#ifdef DEBUG
 end_emulator_loop:
#endif
#endif

 OpCase(int_code_end):
 OpCase(label_L):
 OpCase(on_load):
 OpCase(line_I):
    erl_exit(1, "meta op\n");

    /*
     * One-time initialization of Beam emulator.
     */

 init_emulator:
 {
     int i;
     Export* ep;

#ifndef NO_JUMP_TABLE
#ifdef ERTS_OPCODE_COUNTER_SUPPORT
#ifdef DEBUG
     counting_opcodes[op_catch_end_y] = LabelAddr(lb_catch_end_y);
#endif
     counting_opcodes[op_i_func_info_IaaI] = LabelAddr(lb_i_func_info_IaaI);
     beam_ops = counting_opcodes;
#else /* #ifndef ERTS_OPCODE_COUNTER_SUPPORT */
     beam_ops = opcodes;
#endif /* ERTS_OPCODE_COUNTER_SUPPORT */
#endif /* NO_JUMP_TABLE */
     
     em_call_error_handler = OpCode(call_error_handler);
     em_apply_bif = OpCode(apply_bif);
     em_call_nif = OpCode(call_nif);

     beam_apply[0]             = (BeamInstr) OpCode(i_apply);
     beam_apply[1]             = (BeamInstr) OpCode(normal_exit);
     beam_exit[0]              = (BeamInstr) OpCode(error_action_code);
     beam_continue_exit[0]     = (BeamInstr) OpCode(continue_exit);
     beam_return_to_trace[0]   = (BeamInstr) OpCode(i_return_to_trace);
     beam_return_trace[0]      = (BeamInstr) OpCode(return_trace);
     beam_exception_trace[0]   = (BeamInstr) OpCode(return_trace); /* UGLY */
     beam_return_time_trace[0] = (BeamInstr) OpCode(i_return_time_trace);

     /*
      * Enter all BIFs into the export table.
      */
     for (i = 0; i < BIF_SIZE; i++) {
	 ep = erts_export_put(bif_table[i].module,
			      bif_table[i].name,
			      bif_table[i].arity);
	 bif_export[i] = ep;
	 ep->code[3] = (BeamInstr) OpCode(apply_bif);
	 ep->code[4] = (BeamInstr) bif_table[i].f;
	 /* XXX: set func info for bifs */
	 ep->fake_op_func_info_for_hipe[0] = (BeamInstr) BeamOp(op_i_func_info_IaaI);
     }

     return;
 }
#ifdef NO_JUMP_TABLE
 default:
    erl_exit(1, "unexpected op code %d\n",Go);
  }
#endif
    return;			/* Never executed */

  save_calls1:
    {
	Eterm* dis_next;

	save_calls(c_p, (Export *) Arg(0));

	SET_I(((Export *) Arg(0))->addressv[erts_active_code_ix()]);

	dis_next = (Eterm *) *I;
	FCALLS--;
	Goto(dis_next);
    }
}

static BifFunction
translate_gc_bif(void* gcf)
{
    if (gcf == erts_gc_length_1) {
	return length_1;
    } else if (gcf == erts_gc_size_1) {
	return size_1;
    } else if (gcf == erts_gc_bit_size_1) {
	return bit_size_1;
    } else if (gcf == erts_gc_byte_size_1) {
	return byte_size_1;
    } else if (gcf == erts_gc_map_size_1) {
	return map_size_1;
    } else if (gcf == erts_gc_abs_1) {
	return abs_1;
    } else if (gcf == erts_gc_float_1) {
	return float_1;
    } else if (gcf == erts_gc_round_1) {
	return round_1;
    } else if (gcf == erts_gc_trunc_1) {
	return round_1;
    } else if (gcf == erts_gc_binary_part_2) {
	return binary_part_2;
    } else if (gcf == erts_gc_binary_part_3) {
	return binary_part_3;
    } else {
	erl_exit(1, "bad gc bif");
    }
}

/*
 * Mapping from the error code 'class tag' to atoms.
 */
Eterm exception_tag[NUMBER_EXC_TAGS] = {
  am_error,	/* 0 */
  am_exit,	/* 1 */
  am_throw,	/* 2 */
};

/*
 * Mapping from error code 'index' to atoms.
 */
Eterm error_atom[NUMBER_EXIT_CODES] = {
  am_internal_error,	/* 0 */
  am_normal,		/* 1 */
  am_internal_error,	/* 2 */
  am_badarg,		/* 3 */
  am_badarith,		/* 4 */
  am_badmatch,		/* 5 */
  am_function_clause,	/* 6 */
  am_case_clause,	/* 7 */
  am_if_clause,		/* 8 */
  am_undef,		/* 9 */
  am_badfun,		/* 10 */
  am_badarity,		/* 11 */
  am_timeout_value,	/* 12 */
  am_noproc,		/* 13 */
  am_notalive,		/* 14 */
  am_system_limit,	/* 15 */
  am_try_clause,	/* 16 */
  am_notsup,		/* 17 */
  am_badmap,		/* 18 */
  am_badkey,		/* 19 */
};

/*
 * To fully understand the error handling, one must keep in mind that
 * when an exception is thrown, the search for a handler can jump back
 * and forth between Beam and native code. Upon each mode switch, a
 * dummy handler is inserted so that if an exception reaches that point,
 * the handler is invoked (like any handler) and transfers control so
 * that the search for a real handler is continued in the other mode.
 * Therefore, c_p->freason and c_p->fvalue must still hold the exception
 * info when the handler is executed, but normalized so that creation of
 * error terms and saving of the stack trace is only done once, even if
 * we pass through the error handling code several times.
 *
 * When a new exception is raised, the current stack trace information
 * is quick-saved in a small structure allocated on the heap. Depending
 * on how the exception is eventually caught (perhaps by causing the
 * current process to terminate), the saved information may be used to
 * create a symbolic (human-readable) representation of the stack trace
 * at the point of the original exception.
 */

static BeamInstr*
handle_error(Process* c_p, BeamInstr* pc, Eterm* reg, BifFunction bf)
{
    Eterm* hp;
    Eterm Value = c_p->fvalue;
    Eterm Args = am_true;
    c_p->i = pc;    /* In case we call erl_exit(). */

    ASSERT(c_p->freason != TRAP); /* Should have been handled earlier. */

    /*
     * Check if we have an arglist for the top level call. If so, this
     * is encoded in Value, so we have to dig out the real Value as well
     * as the Arglist.
     */
    if (c_p->freason & EXF_ARGLIST) {
	  Eterm* tp;
	  ASSERT(is_tuple(Value));
	  tp = tuple_val(Value);
	  Value = tp[1];
	  Args = tp[2];
    }

    /*
     * Save the stack trace info if the EXF_SAVETRACE flag is set. The
     * main reason for doing this separately is to allow throws to later
     * become promoted to errors without losing the original stack
     * trace, even if they have passed through one or more catch and
     * rethrow. It also makes the creation of symbolic stack traces much
     * more modular.
     */
    if (c_p->freason & EXF_SAVETRACE) {
        save_stacktrace(c_p, pc, reg, bf, Args);
    }

    /*
     * Throws that are not caught are turned into 'nocatch' errors
     */
    if ((c_p->freason & EXF_THROWN) && (c_p->catches <= 0) ) {
	hp = HAlloc(c_p, 3);
        Value = TUPLE2(hp, am_nocatch, Value);
        c_p->freason = EXC_ERROR;
    }

    /* Get the fully expanded error term */
    Value = expand_error_value(c_p, c_p->freason, Value);

    /* Save final error term and stabilize the exception flags so no
       further expansion is done. */
    c_p->fvalue = Value;
    c_p->freason = PRIMARY_EXCEPTION(c_p->freason);

    /* Find a handler or die */
    if ((c_p->catches > 0 || IS_TRACED_FL(c_p, F_EXCEPTION_TRACE))
	&& !(c_p->freason & EXF_PANIC)) {
	BeamInstr *new_pc;
        /* The Beam handler code (catch_end or try_end) checks reg[0]
	   for THE_NON_VALUE to see if the previous code finished
	   abnormally. If so, reg[1], reg[2] and reg[3] should hold the
	   exception class, term and trace, respectively. (If the
	   handler is just a trap to native code, these registers will
	   be ignored.) */
	reg[0] = THE_NON_VALUE;
	reg[1] = exception_tag[GET_EXC_CLASS(c_p->freason)];
	reg[2] = Value;
	reg[3] = c_p->ftrace;
        if ((new_pc = next_catch(c_p, reg))) {
	    c_p->cp = 0;	/* To avoid keeping stale references. */
	    return new_pc;
	}
	if (c_p->catches > 0) erl_exit(1, "Catch not found");
    }
    ERTS_SMP_UNREQ_PROC_MAIN_LOCK(c_p);
    terminate_proc(c_p, Value);
    ERTS_SMP_REQ_PROC_MAIN_LOCK(c_p);
    return NULL;
}

/*
 * Find the nearest catch handler
 */
static BeamInstr*
next_catch(Process* c_p, Eterm *reg) {
    int active_catches = c_p->catches > 0;
    int have_return_to_trace = 0;
    Eterm *ptr, *prev, *return_to_trace_ptr = NULL;

    BeamInstr i_return_trace      = beam_return_trace[0];
    BeamInstr i_return_to_trace   = beam_return_to_trace[0];
    BeamInstr i_return_time_trace = beam_return_time_trace[0];

    ptr = prev = c_p->stop;
    ASSERT(is_CP(*ptr));
    ASSERT(ptr <= STACK_START(c_p));
    if (ptr == STACK_START(c_p)) return NULL;
    if ((is_not_CP(*ptr) || (*cp_val(*ptr) != i_return_trace &&
			     *cp_val(*ptr) != i_return_to_trace &&
			     *cp_val(*ptr) != i_return_time_trace ))
	&& c_p->cp) {
	/* Can not follow cp here - code may be unloaded */
	BeamInstr *cpp = c_p->cp;
	if (cpp == beam_exception_trace) {
	    erts_trace_exception(c_p, cp_val(ptr[0]),
				 reg[1], reg[2], ptr+1);
	    /* Skip return_trace parameters */
	    ptr += 2;
	} else if (cpp == beam_return_trace) {
	    /* Skip return_trace parameters */
	    ptr += 2;
	} else if (cpp == beam_return_time_trace) {
	    /* Skip return_trace parameters */
	    ptr += 1;
	} else if (cpp == beam_return_to_trace) {
	    have_return_to_trace = !0; /* Record next cp */
	}
    }
    while (ptr < STACK_START(c_p)) {
	if (is_catch(*ptr)) {
	    if (active_catches) goto found_catch;
	    ptr++;
	}
	else if (is_CP(*ptr)) {
	    prev = ptr;
	    if (*cp_val(*prev) == i_return_trace) {
		/* Skip stack frame variables */
		while (++ptr, ptr < STACK_START(c_p) && is_not_CP(*ptr)) {
		    if (is_catch(*ptr) && active_catches) goto found_catch;
		}
		if (cp_val(*prev) == beam_exception_trace) {
		    erts_trace_exception(c_p, cp_val(ptr[0]),
					 reg[1], reg[2], ptr+1);
		}
		/* Skip return_trace parameters */
		ptr += 2;
	    } else if (*cp_val(*prev) == i_return_to_trace) {
		/* Skip stack frame variables */
		while (++ptr, ptr < STACK_START(c_p) && is_not_CP(*ptr)) {
		    if (is_catch(*ptr) && active_catches) goto found_catch;
		}
		have_return_to_trace = !0; /* Record next cp */
		return_to_trace_ptr = NULL;
	    } else if (*cp_val(*prev) == i_return_time_trace) {
		/* Skip stack frame variables */
		while (++ptr, ptr < STACK_START(c_p) && is_not_CP(*ptr)) {
		    if (is_catch(*ptr) && active_catches) goto found_catch;
		}
		/* Skip return_trace parameters */
		ptr += 1;
	    } else {
		if (have_return_to_trace) {
		    /* Record this cp as possible return_to trace cp */
		    have_return_to_trace = 0;
		    return_to_trace_ptr = ptr;
		} else return_to_trace_ptr = NULL;
		ptr++;
	    }
	} else ptr++;
    }
    return NULL;
    
 found_catch:
    ASSERT(ptr < STACK_START(c_p));
    c_p->stop = prev;
    if (IS_TRACED_FL(c_p, F_TRACE_RETURN_TO) && return_to_trace_ptr) {
	/* The stackframe closest to the catch contained an
	 * return_to_trace entry, so since the execution now
	 * continues after the catch, a return_to trace message 
	 * would be appropriate.
	 */
	erts_trace_return_to(c_p, cp_val(*return_to_trace_ptr));
    }
    return catch_pc(*ptr);
}

/*
 * Terminating the process when an exception is not caught
 */
static void
terminate_proc(Process* c_p, Eterm Value)
{
    Eterm *hp;
    Eterm Args = NIL;

    /* Add a stacktrace if this is an error. */
    if (GET_EXC_CLASS(c_p->freason) == EXTAG_ERROR) {
        Value = add_stacktrace(c_p, Value, c_p->ftrace);
    }
    /* EXF_LOG is a primary exception flag */
    if (c_p->freason & EXF_LOG) {
	int alive = erts_is_alive;
	erts_dsprintf_buf_t *dsbufp = erts_create_logger_dsbuf();

        /* Build the format message */
	erts_dsprintf(dsbufp, "Error in process ~p ");
	if (alive)
	    erts_dsprintf(dsbufp, "on node ~p ");
	erts_dsprintf(dsbufp, "with exit value:~n~p~n");

        /* Build the args in reverse order */
	hp = HAlloc(c_p, 2);
	Args = CONS(hp, Value, Args);
	if (alive) {
	    hp = HAlloc(c_p, 2);
	    Args = CONS(hp, erts_this_node->sysname, Args);
	}
	hp = HAlloc(c_p, 2);
	Args = CONS(hp, c_p->common.id, Args);

	erts_send_error_term_to_logger(c_p->group_leader, dsbufp, Args);
    }
    /*
     * If we use a shared heap, the process will be garbage-collected.
     * Must zero c_p->arity to indicate that there are no live registers.
     */
    c_p->arity = 0;
    erts_do_exit_process(c_p, Value);
}

/*
 * Build and add a symbolic stack trace to the error value.
 */
static Eterm
add_stacktrace(Process* c_p, Eterm Value, Eterm exc) {
    Eterm Where = build_stacktrace(c_p, exc);
    Eterm* hp = HAlloc(c_p, 3);
    return TUPLE2(hp, Value, Where);
}

/*
 * Forming the correct error value from the internal error code.
 * This does not update c_p->fvalue or c_p->freason.
 */
Eterm
expand_error_value(Process* c_p, Uint freason, Eterm Value) {
    Eterm* hp;
    Uint r;

    r = GET_EXC_INDEX(freason);
    ASSERT(r < NUMBER_EXIT_CODES); /* range check */
    ASSERT(is_value(Value));

    switch (r) {
    case (GET_EXC_INDEX(EXC_PRIMARY)):
        /* Primary exceptions use fvalue as it is */
	break;
    case (GET_EXC_INDEX(EXC_BADMATCH)):
    case (GET_EXC_INDEX(EXC_CASE_CLAUSE)):
    case (GET_EXC_INDEX(EXC_TRY_CLAUSE)):
    case (GET_EXC_INDEX(EXC_BADFUN)):
    case (GET_EXC_INDEX(EXC_BADARITY)):
    case (GET_EXC_INDEX(EXC_BADMAP)):
    case (GET_EXC_INDEX(EXC_BADKEY)):
        /* Some common exceptions: value -> {atom, value} */
        ASSERT(is_value(Value));
	hp = HAlloc(c_p, 3);
	Value = TUPLE2(hp, error_atom[r], Value);
	break;
    default:
        /* Other exceptions just use an atom as descriptor */
        Value = error_atom[r];
	break;
    }
#ifdef DEBUG
    ASSERT(Value != am_internal_error);
#endif
    return Value;
}

/*
 * Quick-saving the stack trace in an internal form on the heap. Note
 * that c_p->ftrace will point to a cons cell which holds the given args
 * and the saved data (encoded as a bignum).
 *
 * There is an issue with line number information. Line number
 * information is associated with the address *before* an operation
 * that may fail or be stored stored on the stack. But continuation
 * pointers point after its call instruction, not before. To avoid
 * finding the wrong line number, we'll need to adjust them so that
 * they point at the beginning of the call instruction or inside the
 * call instruction. Since its impractical to point at the beginning,
 * we'll do the simplest thing and decrement the continuation pointers
 * by one.
 *
 * Here is an example of what can go wrong. Without the adjustment
 * of continuation pointers, the call at line 42 below would seem to
 * be at line 43:
 *
 * line 42
 * call ...
 * line 43
 * gc_bif ...
 *
 * (It would be much better to put the arglist - when it exists - in the
 * error value instead of in the actual trace; e.g. '{badarg, Args}'
 * instead of using 'badarg' with Args in the trace. The arglist may
 * contain very large values, and right now they will be kept alive as
 * long as the stack trace is live. Preferably, the stack trace should
 * always be small, so that it does not matter if it is long-lived.
 * However, it is probably not possible to ever change the format of
 * error terms.)
 */

static void
save_stacktrace(Process* c_p, BeamInstr* pc, Eterm* reg, BifFunction bf,
		Eterm args) {
    struct StackTrace* s;
    int sz;
    int depth = erts_backtrace_depth;    /* max depth (never negative) */
    if (depth > 0) {
	/* There will always be a current function */
	depth --;
    }

    /* Create a container for the exception data */
    sz = (offsetof(struct StackTrace, trace) + sizeof(BeamInstr *)*depth
          + sizeof(Eterm) - 1) / sizeof(Eterm);
    s = (struct StackTrace *) HAlloc(c_p, 1 + sz);
    /* The following fields are inside the bignum */
    s->header = make_pos_bignum_header(sz);
    s->freason = c_p->freason;
    s->depth = 0;

    /*
     * If the failure was in a BIF other than 'error', 'exit' or
     * 'throw', find the bif-table index and save the argument
     * registers by consing up an arglist.
     */
    if (bf != NULL && bf != error_1 && bf != error_2 &&
	bf != exit_1 && bf != throw_1) {
        int i;
	int a = 0;
	for (i = 0; i < BIF_SIZE; i++) {
	    if (bf == bif_table[i].f || bf == bif_table[i].traced) {
		Export *ep = bif_export[i];
		s->current = ep->code;
	        a = bif_table[i].arity;
		break;
	    }
	}
	if (i >= BIF_SIZE) {
	    /* 
	     * The Bif does not really exist (no BIF entry).  It is a
	     * TRAP and traps are called through apply_bif, which also
	     * sets c_p->current (luckily).
	     * OR it is a NIF called by call_nif where current is also set.
	     */
	    ASSERT(c_p->current);
	    s->current = c_p->current;
	    a = s->current[2];
	}
	/* Save first stack entry */
	ASSERT(pc);
	if (depth > 0) {
	    s->trace[s->depth++] = pc;
	    depth--;
	}
	/* Save second stack entry if CP is valid and different from pc */
	if (depth > 0 && c_p->cp != 0 && c_p->cp != pc) {
	    s->trace[s->depth++] = c_p->cp - 1;
	    depth--;
	}
	s->pc = NULL;
	args = make_arglist(c_p, reg, a); /* Overwrite CAR(c_p->ftrace) */
    } else {
	s->current = c_p->current;
        /* 
	 * For a function_clause error, the arguments are in the beam
	 * registers, c_p->cp is valid, and c_p->current is set.
	 */
	if ( (GET_EXC_INDEX(s->freason)) ==
	     (GET_EXC_INDEX(EXC_FUNCTION_CLAUSE)) ) {
	    int a;
	    ASSERT(s->current);
	    a = s->current[2];
	    args = make_arglist(c_p, reg, a); /* Overwrite CAR(c_p->ftrace) */
	    /* Save first stack entry */
	    ASSERT(c_p->cp);
	    if (depth > 0) {
		s->trace[s->depth++] = c_p->cp - 1;
		depth--;
	    }
	    s->pc = NULL; /* Ignore pc */
	} else {
	    if (depth > 0 && c_p->cp != 0 && c_p->cp != pc) {
		s->trace[s->depth++] = c_p->cp - 1;
		depth--;
	    }
	    s->pc = pc;
	}
    }

    /* Package args and stack trace */
    {
	Eterm *hp;
	hp = HAlloc(c_p, 2);
	c_p->ftrace = CONS(hp, args, make_big((Eterm *) s));
    }

    /* Save the actual stack trace */
    erts_save_stacktrace(c_p, s, depth);
}

void
erts_save_stacktrace(Process* p, struct StackTrace* s, int depth)
{
    if (depth > 0) {
	Eterm *ptr;
	BeamInstr *prev = s->depth ? s->trace[s->depth-1] : NULL;
	BeamInstr i_return_trace = beam_return_trace[0];
	BeamInstr i_return_to_trace = beam_return_to_trace[0];

	/*
	 * Traverse the stack backwards and add all unique continuation
	 * pointers to the buffer, up to the maximum stack trace size.
	 * 
	 * Skip trace stack frames.
	 */
	ptr = p->stop;
	if (ptr < STACK_START(p) &&
	    (is_not_CP(*ptr)|| (*cp_val(*ptr) != i_return_trace &&
				*cp_val(*ptr) != i_return_to_trace)) &&
	    p->cp) {
	    /* Cannot follow cp here - code may be unloaded */
	    BeamInstr *cpp = p->cp;
	    if (cpp == beam_exception_trace || cpp == beam_return_trace) {
		/* Skip return_trace parameters */
		ptr += 2;
	    } else if (cpp == beam_return_to_trace) {
		/* Skip return_to_trace parameters */
		ptr += 1;
	    }
	}
	while (ptr < STACK_START(p) && depth > 0) {
	    if (is_CP(*ptr)) {
		if (*cp_val(*ptr) == i_return_trace) {
		    /* Skip stack frame variables */
		    do ++ptr; while (is_not_CP(*ptr));
		    /* Skip return_trace parameters */
		    ptr += 2;
		} else if (*cp_val(*ptr) == i_return_to_trace) {
		    /* Skip stack frame variables */
		    do ++ptr; while (is_not_CP(*ptr));
		} else {
		    BeamInstr *cp = cp_val(*ptr);
		    if (cp != prev) {
			/* Record non-duplicates only */
			prev = cp;
			s->trace[s->depth++] = cp - 1;
			depth--;
		    }
		    ptr++;
		}
	    } else ptr++;
	}
    }
}

/*
 * Getting the relevant fields from the term pointed to by ftrace
 */

static struct StackTrace *get_trace_from_exc(Eterm exc) {
    if (exc == NIL) {
	return NULL;
    } else {
	ASSERT(is_list(exc));
	return (struct StackTrace *) big_val(CDR(list_val(exc)));
    }
}

static Eterm get_args_from_exc(Eterm exc) {
    if (exc == NIL) {
	return NIL;
    } else {
	ASSERT(is_list(exc));
	return CAR(list_val(exc));
    }
}

static int is_raised_exc(Eterm exc) {
    if (exc == NIL) {
        return 0;
    } else {
        ASSERT(is_list(exc));
        return bignum_header_is_neg(*big_val(CDR(list_val(exc))));
    }
}

/*
 * Creating a list with the argument registers
 */
static Eterm
make_arglist(Process* c_p, Eterm* reg, int a) {
    Eterm args = NIL;
    Eterm* hp = HAlloc(c_p, 2*a);
    while (a > 0) {
        args = CONS(hp, reg[a-1], args);
	hp += 2;
	a--;
    }
    return args;
}

/*
 * Building a symbolic representation of a saved stack trace. Note that
 * the exception object 'exc', unless NIL, points to a cons cell which
 * holds the given args and the quick-saved data (encoded as a bignum).
 *
 * If the bignum is negative, the given args is a complete stacktrace.
 */
Eterm
build_stacktrace(Process* c_p, Eterm exc) {
    struct StackTrace* s;
    Eterm  args;
    int    depth;
    FunctionInfo fi;
    FunctionInfo* stk;
    FunctionInfo* stkp;
    Eterm res = NIL;
    Uint heap_size;
    Eterm* hp;
    Eterm mfa;
    int i;

    if (! (s = get_trace_from_exc(exc))) {
        return NIL;
    }
#ifdef HIPE
    if (s->freason & EXF_NATIVE) {
	return hipe_build_stacktrace(c_p, s);
    }
#endif
    if (is_raised_exc(exc)) {
	return get_args_from_exc(exc);
    }

    /*
     * Find the current function. If the saved s->pc is null, then the
     * saved s->current should already contain the proper value.
     */
    if (s->pc != NULL) {
	erts_lookup_function_info(&fi, s->pc, 1);
    } else if (GET_EXC_INDEX(s->freason) ==
	       GET_EXC_INDEX(EXC_FUNCTION_CLAUSE)) {
	erts_lookup_function_info(&fi, s->current, 1);
    } else {
	erts_set_current_function(&fi, s->current);
    }

    /*
     * If fi.current is still NULL, default to the initial function
     * (e.g. spawn_link(erlang, abs, [1])).
     */
    if (fi.current == NULL) {
	erts_set_current_function(&fi, c_p->u.initial);
	args = am_true; /* Just in case */
    } else {
	args = get_args_from_exc(exc);
    }

    /*
     * Look up all saved continuation pointers and calculate
     * needed heap space.
     */
    depth = s->depth;
    stk = stkp = (FunctionInfo *) erts_alloc(ERTS_ALC_T_TMP,
				      depth*sizeof(FunctionInfo));
    heap_size = fi.needed + 2;
    for (i = 0; i < depth; i++) {
	erts_lookup_function_info(stkp, s->trace[i], 1);
	if (stkp->current) {
	    heap_size += stkp->needed + 2;
	    stkp++;
	}
    }

    /*
     * Allocate heap space and build the stacktrace.
     */
    hp = HAlloc(c_p, heap_size);
    while (stkp > stk) {
	stkp--;
	hp = erts_build_mfa_item(stkp, hp, am_true, &mfa);
	res = CONS(hp, mfa, res);
	hp += 2;
    }
    hp = erts_build_mfa_item(&fi, hp, args, &mfa);
    res = CONS(hp, mfa, res);

    erts_free(ERTS_ALC_T_TMP, (void *) stk);
    return res;
}

static BeamInstr*
call_error_handler(Process* p, BeamInstr* fi, Eterm* reg, Eterm func)
{
    Eterm* hp;
    Export* ep;
    int arity;
    Eterm args;
    Uint sz;
    int i;

    /*
     * Search for the error_handler module.
     */
    ep = erts_find_function(erts_proc_get_error_handler(p), func, 3,
			    erts_active_code_ix());
    if (ep == NULL) {		/* No error handler */
	p->current = fi;
	p->freason = EXC_UNDEF;
	return 0;
    }

    /*
     * Create a list with all arguments in the x registers.
     */

    arity = fi[2];
    sz = 2 * arity;
    if (HeapWordsLeft(p) < sz) {
	erts_garbage_collect(p, sz, reg, arity);
    }
    hp = HEAP_TOP(p);
    HEAP_TOP(p) += sz;
    args = NIL;
    for (i = arity-1; i >= 0; i--) {
	args = CONS(hp, reg[i], args);
	hp += 2;
    }

    /*
     * Set up registers for call to error_handler:<func>/3.
     */
    reg[0] = fi[0];
    reg[1] = fi[1];
    reg[2] = args;
    return ep->addressv[erts_active_code_ix()];
}

static Export*
apply_setup_error_handler(Process* p, Eterm module, Eterm function, Uint arity, Eterm* reg)
{
    Export* ep;

    /*
     * Find the export table index for the error handler. Return NULL if
     * there is no error handler module.
     */

    if ((ep = erts_active_export_entry(erts_proc_get_error_handler(p),
				     am_undefined_function, 3)) == NULL) {
	return NULL;
    } else {
	int i;
	Uint sz = 2*arity;
	Eterm* hp;
	Eterm args = NIL;
	
	/*
	 * Always copy args from registers to a new list; this ensures
	 * that we have the same behaviour whether or not this was
	 * called from apply or fixed_apply (any additional last
	 * THIS-argument will be included, assuming that arity has been
	 * properly adjusted).
	 */

	if (HeapWordsLeft(p) < sz) {
	    erts_garbage_collect(p, sz, reg, arity);
	}
	hp = HEAP_TOP(p);
	HEAP_TOP(p) += sz;
	for (i = arity-1; i >= 0; i--) {
	    args = CONS(hp, reg[i], args);
	    hp += 2;
	}
	reg[0] = module;
	reg[1] = function;
	reg[2] = args;
    }

    return ep;
}

static BeamInstr*
apply(Process* p, Eterm module, Eterm function, Eterm args, Eterm* reg)
{
    int arity;
    Export* ep;
    Eterm tmp, this;

    /*
     * Check the arguments which should be of the form apply(Module,
     * Function, Arguments) where Function is an atom and
     * Arguments is an arity long list of terms.
     */
    if (is_not_atom(function)) {
	/*
	 * No need to test args here -- done below.
	 */
    error:
	p->freason = BADARG;

    error2:
	reg[0] = module;
	reg[1] = function;
	reg[2] = args;
	return 0;
    }

    /* The module argument may be either an atom or an abstract module
     * (currently implemented using tuples, but this might change).
     */
    this = THE_NON_VALUE;
    if (is_not_atom(module)) {
	Eterm* tp;

        if (is_not_tuple(module)) goto error;
        tp = tuple_val(module);
        if (arityval(tp[0]) < 1) goto error;
        this = module;
        module = tp[1];
        if (is_not_atom(module)) goto error;
    }
    
    /*
     * Walk down the 3rd parameter of apply (the argument list) and copy
     * the parameters to the x registers (reg[]). If the module argument
     * was an abstract module, add 1 to the function arity and put the
     * module argument in the n+1st x register as a THIS reference.
     */

    tmp = args;
    arity = 0;
    while (is_list(tmp)) {
	if (arity < (MAX_REG - 1)) {
	    reg[arity++] = CAR(list_val(tmp));
	    tmp = CDR(list_val(tmp));
	} else {
	    p->freason = SYSTEM_LIMIT;
	    goto error2;
	}
    }
    if (is_not_nil(tmp)) {	/* Must be well-formed list */
	goto error;
    }
    if (this != THE_NON_VALUE) {
        reg[arity++] = this;
    }

    /*
     * Get the index into the export table, or failing that the export
     * entry for the error handler.
     *
     * Note: All BIFs have export entries; thus, no special case is needed.
     */

    if ((ep = erts_active_export_entry(module, function, arity)) == NULL) {
	if ((ep = apply_setup_error_handler(p, module, function, arity, reg)) == NULL) goto error;
    } else if (ERTS_PROC_GET_SAVED_CALLS_BUF(p)) {
	save_calls(p, ep);
    }
    DTRACE_GLOBAL_CALL_FROM_EXPORT(p, ep);
    return ep->addressv[erts_active_code_ix()];
}

static BeamInstr*
fixed_apply(Process* p, Eterm* reg, Uint arity)
{
    Export* ep;
    Eterm module;
    Eterm function;

    module = reg[arity];    /* The THIS pointer already in place */
    function = reg[arity+1];

    if (is_not_atom(function)) {
    error:
	p->freason = BADARG;
	reg[0] = module;
	reg[1] = function;
	reg[2] = NIL;
	return 0;
    }

    /* The module argument may be either an atom or an abstract module
     * (currently implemented using tuples, but this might change).
     */
    if (is_not_atom(module)) {
	Eterm* tp;
        if (is_not_tuple(module)) goto error;
        tp = tuple_val(module);
        if (arityval(tp[0]) < 1) goto error;
        module = tp[1];
        if (is_not_atom(module)) goto error;
        ++arity;
    }
    
    /*
     * Get the index into the export table, or failing that the export
     * entry for the error handler module.
     *
     * Note: All BIFs have export entries; thus, no special case is needed.
     */

    if ((ep = erts_active_export_entry(module, function, arity)) == NULL) {
	if ((ep = apply_setup_error_handler(p, module, function, arity, reg)) == NULL)
	    goto error;
    } else if (ERTS_PROC_GET_SAVED_CALLS_BUF(p)) {
	save_calls(p, ep);
    }
    DTRACE_GLOBAL_CALL_FROM_EXPORT(p, ep);
    return ep->addressv[erts_active_code_ix()];
}

int
erts_hibernate(Process* c_p, Eterm module, Eterm function, Eterm args, Eterm* reg)
{
    int arity;
    Eterm tmp;

    if (is_not_atom(module) || is_not_atom(function)) {
	/*
	 * No need to test args here -- done below.
	 */
    error:
	c_p->freason = BADARG;

    error2:
	reg[0] = module;
	reg[1] = function;
	reg[2] = args;
	return 0;
    }

    arity = 0;
    tmp = args;
    while (is_list(tmp)) {
	if (arity < MAX_REG) {
	    tmp = CDR(list_val(tmp));
	    arity++;
	} else {
	    c_p->freason = SYSTEM_LIMIT;
	    goto error2;
	}
    }
    if (is_not_nil(tmp)) {	/* Must be well-formed list */
	goto error;
    }

    /*
     * At this point, arguments are known to be good.
     */

    if (c_p->arg_reg != c_p->def_arg_reg) {
	/* Save some memory */
	erts_free(ERTS_ALC_T_ARG_REG, c_p->arg_reg);
	c_p->arg_reg = c_p->def_arg_reg;
	c_p->max_arg_reg = sizeof(c_p->def_arg_reg)/sizeof(c_p->def_arg_reg[0]);
    }

#ifdef USE_VM_PROBES
    if (DTRACE_ENABLED(process_hibernate)) {
        DTRACE_CHARBUF(process_name, DTRACE_TERM_BUF_SIZE);
        DTRACE_CHARBUF(mfa, DTRACE_TERM_BUF_SIZE);
        dtrace_fun_decode(c_p, module, function, arity,
                          process_name, mfa);
        DTRACE2(process_hibernate, process_name, mfa);
    }
#endif
    /*
     * Arrange for the process to be resumed at the given MFA with
     * the stack cleared.
     */
    c_p->arity = 3;
    c_p->arg_reg[0] = module;
    c_p->arg_reg[1] = function;
    c_p->arg_reg[2] = args;
    c_p->stop = STACK_START(c_p);
    c_p->catches = 0;
    c_p->i = beam_apply;
    c_p->cp = (BeamInstr *) beam_apply+1;

    /*
     * If there are no waiting messages, garbage collect and
     * shrink the heap. 
     */
    erts_smp_proc_lock(c_p, ERTS_PROC_LOCK_MSGQ|ERTS_PROC_LOCK_STATUS);
    ERTS_SMP_MSGQ_MV_INQ2PRIVQ(c_p);
    if (!c_p->msg.len) {
	erts_smp_proc_unlock(c_p, ERTS_PROC_LOCK_MSGQ|ERTS_PROC_LOCK_STATUS);
	c_p->fvalue = NIL;
	PROCESS_MAIN_CHK_LOCKS(c_p);
	erts_garbage_collect_hibernate(c_p);
	ERTS_VERIFY_UNUSED_TEMP_ALLOC(c_p);
	PROCESS_MAIN_CHK_LOCKS(c_p);
	erts_smp_proc_lock(c_p, ERTS_PROC_LOCK_MSGQ|ERTS_PROC_LOCK_STATUS);
#ifdef ERTS_SMP
	ERTS_SMP_MSGQ_MV_INQ2PRIVQ(c_p);
	if (!c_p->msg.len)
#endif
	    erts_smp_atomic32_read_band_relb(&c_p->state, ~ERTS_PSFLG_ACTIVE);
	ASSERT(!ERTS_PROC_IS_EXITING(c_p));
    }
    erts_smp_proc_unlock(c_p, ERTS_PROC_LOCK_MSGQ|ERTS_PROC_LOCK_STATUS);
    c_p->current = bif_export[BIF_hibernate_3]->code;
    c_p->flags |= F_HIBERNATE_SCHED; /* Needed also when woken! */
    return 1;
}

static BeamInstr*
call_fun(Process* p,		/* Current process. */
	 int arity,		/* Number of arguments for Fun. */
	 Eterm* reg,		/* Contents of registers. */
	 Eterm args)		/* THE_NON_VALUE or pre-built list of arguments. */
{
    Eterm fun = reg[arity];
    Eterm hdr;
    int i;
    Eterm* hp;

    if (!is_boxed(fun)) {
	goto badfun;
    }
    hdr = *boxed_val(fun);

    if (is_fun_header(hdr)) {
	ErlFunThing* funp = (ErlFunThing *) fun_val(fun);
	ErlFunEntry* fe;
	BeamInstr* code_ptr;
	Eterm* var_ptr;
	int actual_arity;
	unsigned num_free;

	fe = funp->fe;
	num_free = funp->num_free;
	code_ptr = fe->address;
	actual_arity = (int) code_ptr[-1];

	if (actual_arity == arity+num_free) {
	    DTRACE_LOCAL_CALL(p, (Eterm)code_ptr[-3],
			(Eterm)code_ptr[-2],
			code_ptr[-1]);
	    if (num_free == 0) {
		return code_ptr;
	    } else {
		var_ptr = funp->env;
		reg += arity;
		i = 0;
		do {
		    reg[i] = var_ptr[i];
		    i++;
		} while (i < num_free);
		reg[i] = fun;
		return code_ptr;
	    }
	    return code_ptr;
	} else {
	    /*
	     * Something wrong here. First build a list of the arguments.
	     */

	    if (is_non_value(args)) {
		Uint sz = 2 * arity;
		args = NIL;
		if (HeapWordsLeft(p) < sz) {
		    erts_garbage_collect(p, sz, reg, arity+1);
		    fun = reg[arity];
		}
		hp = HEAP_TOP(p);
		HEAP_TOP(p) += sz;
		for (i = arity-1; i >= 0; i--) {
		    args = CONS(hp, reg[i], args);
		    hp += 2;
		}
	    }

	    if (actual_arity >= 0) {
		/*
		 * There is a fun defined, but the call has the wrong arity.
		 */
		hp = HAlloc(p, 3);
		p->freason = EXC_BADARITY;
		p->fvalue = TUPLE2(hp, fun, args);
		return NULL;
	    } else {
		Export* ep;
		Module* modp;
		Eterm module;
		ErtsCodeIndex code_ix = erts_active_code_ix();

		/*
		 * No arity. There is no module loaded that defines the fun,
		 * either because the fun is newly created from the external
		 * representation (the module has never been loaded),
		 * or the module defining the fun has been unloaded.
		 */
		module = fe->module;
		if ((modp = erts_get_module(module, code_ix)) != NULL
		    && modp->curr.code != NULL) {
		    /*
		     * There is a module loaded, but obviously the fun is not
		     * defined in it. We must not call the error_handler
		     * (or we will get into an infinite loop).
		     */
		    goto badfun;
		}
		
		/*
		 * No current code for this module. Call the error_handler module
		 * to attempt loading the module.
		 */

		ep = erts_find_function(erts_proc_get_error_handler(p),
					am_undefined_lambda, 3, code_ix);
		if (ep == NULL) {	/* No error handler */
		    p->current = NULL;
		    p->freason = EXC_UNDEF;
		    return NULL;
		}
		reg[0] = module;
		reg[1] = fun;
		reg[2] = args;
		reg[3] = NIL;
		return ep->addressv[erts_active_code_ix()];
	    }
	}
    } else if (is_export_header(hdr)) {
	Export *ep;
	int actual_arity;

	ep = *((Export **) (export_val(fun) + 1));
	actual_arity = (int) ep->code[2];

	if (arity == actual_arity) {
	    DTRACE_GLOBAL_CALL(p, ep->code[0], ep->code[1], (Uint)ep->code[2]);
	    return ep->addressv[erts_active_code_ix()];
	} else {
	    /*
	     * Wrong arity. First build a list of the arguments.
	     */  

	    if (is_non_value(args)) {
		args = NIL;
		hp = HAlloc(p, arity*2);
		for (i = arity-1; i >= 0; i--) {
		    args = CONS(hp, reg[i], args);
		    hp += 2;
		}
	    }

	    hp = HAlloc(p, 3);
	    p->freason = EXC_BADARITY;
	    p->fvalue = TUPLE2(hp, fun, args);
	    return NULL;
	}
    } else {
    badfun:
	p->current = NULL;
	p->freason = EXC_BADFUN;
	p->fvalue = fun;
	return NULL;
    }
}

static BeamInstr*
apply_fun(Process* p, Eterm fun, Eterm args, Eterm* reg)
{
    int arity;
    Eterm tmp;

    /*
     * Walk down the 3rd parameter of apply (the argument list) and copy
     * the parameters to the x registers (reg[]).
     */

    tmp = args;
    arity = 0;
    while (is_list(tmp)) {
	if (arity < MAX_REG-1) {
	    reg[arity++] = CAR(list_val(tmp));
	    tmp = CDR(list_val(tmp));
	} else {
	    p->freason = SYSTEM_LIMIT;
	    return NULL;
	}
    }

    if (is_not_nil(tmp)) {	/* Must be well-formed list */
	p->freason = EXC_UNDEF;
	return NULL;
    }
    reg[arity] = fun;
    return call_fun(p, arity, reg, args);
}



static Eterm
new_fun(Process* p, Eterm* reg, ErlFunEntry* fe, int num_free)
{
    unsigned needed = ERL_FUN_SIZE + num_free;
    ErlFunThing* funp;
    Eterm* hp;
    int i;

    if (HEAP_LIMIT(p) - HEAP_TOP(p) <= needed) {
	PROCESS_MAIN_CHK_LOCKS(p);
	erts_garbage_collect(p, needed, reg, num_free);
	ERTS_VERIFY_UNUSED_TEMP_ALLOC(p);
	PROCESS_MAIN_CHK_LOCKS(p);
    }
    hp = p->htop;
    p->htop = hp + needed;
    funp = (ErlFunThing *) hp;
    hp = funp->env;
    erts_refc_inc(&fe->refc, 2);
    funp->thing_word = HEADER_FUN;
    funp->next = MSO(p).first;
    MSO(p).first = (struct erl_off_heap_header*) funp;
    funp->fe = fe;
    funp->num_free = num_free;
    funp->creator = p->common.id;
#ifdef HIPE
    funp->native_address = fe->native_address;
#endif
    funp->arity = (int)fe->address[-1] - num_free;
    for (i = 0; i < num_free; i++) {
	*hp++ = reg[i];
    }
    return make_fun(funp);
}

static Eterm get_map_element(Eterm map, Eterm key)
{
    Uint32 hx;
    const Eterm *vs;
    if (is_flatmap(map)) {
	flatmap_t *mp;
	Eterm *ks;
	Uint i;
	Uint n;

	mp = (flatmap_t *)flatmap_val(map);
	ks = flatmap_get_keys(mp);
	vs = flatmap_get_values(mp);
	n  = flatmap_get_size(mp);
	if (is_immed(key)) {
	    for (i = 0; i < n; i++) {
		if (ks[i] == key) {
		    return vs[i];
		}
	    }
	} else {
	    for (i = 0; i < n; i++) {
		if (EQ(ks[i], key)) {
		    return vs[i];
		}
	    }
	}
	return THE_NON_VALUE;
    }
    ASSERT(is_hashmap(map));
    hx = hashmap_make_hash(key);
    vs = erts_hashmap_get(hx,key,map);
    return vs ? *vs : THE_NON_VALUE;
}

static Eterm get_map_element_hash(Eterm map, Eterm key, Uint32 hx)
{
    const Eterm *vs;

    if (is_flatmap(map)) {
	flatmap_t *mp;
	Eterm *ks;
	Uint i;
	Uint n;

	mp = (flatmap_t *)flatmap_val(map);
	ks = flatmap_get_keys(mp);
	vs = flatmap_get_values(mp);
	n  = flatmap_get_size(mp);
	if (is_immed(key)) {
	    for (i = 0; i < n; i++) {
		if (ks[i] == key) {
		    return vs[i];
		}
	    }
	} else {
	    for (i = 0; i < n; i++) {
		if (EQ(ks[i], key)) {
		    return vs[i];
		}
	    }
	}
	return THE_NON_VALUE;
    }

    ASSERT(is_hashmap(map));
    ASSERT(hx == hashmap_make_hash(key));
    vs = erts_hashmap_get(hx, key, map);
    return vs ? *vs : THE_NON_VALUE;
}

#define GET_TERM(term, dest)					\
do {								\
    Eterm src = (Eterm)(term);					\
    switch (src & _TAG_IMMED1_MASK) {				\
    case (R_REG_DEF << _TAG_PRIMARY_SIZE) | TAG_PRIMARY_HEADER:	\
	dest = x(0);						\
	break;							\
    case (X_REG_DEF << _TAG_PRIMARY_SIZE) | TAG_PRIMARY_HEADER:	\
	dest = x(src >> _TAG_IMMED1_SIZE);			\
	break;							\
    case (Y_REG_DEF << _TAG_PRIMARY_SIZE) | TAG_PRIMARY_HEADER:	\
	dest = y(src >> _TAG_IMMED1_SIZE);			\
	break;							\
    default:							\
	dest = src;						\
	break;							\
    }								\
} while(0)


static Eterm
new_map(Process* p, Eterm* reg, BeamInstr* I)
{
    Uint n = Arg(3);
    Uint i;
    Uint need = n + 1 /* hdr */ + 1 /*size*/ + 1 /* ptr */ + 1 /* arity */;
    Eterm keys;
    Eterm *mhp,*thp;
    Eterm *E;
    BeamInstr *ptr;
    flatmap_t *mp;
    ErtsHeapFactory factory;

    ptr = &Arg(4);

    if (n > 2*MAP_SMALL_MAP_LIMIT) {
        Eterm res;
	if (HeapWordsLeft(p) < n) {
	    erts_garbage_collect(p, n, reg, Arg(2));
	}

	mhp = p->htop;
	thp = p->htop;
	E   = p->stop;

	for (i = 0; i < n/2; i++) {
	    GET_TERM(*ptr++, *mhp++);
	    GET_TERM(*ptr++, *mhp++);
	}

	p->htop = mhp;

        erts_factory_proc_init(&factory, p);
        res = erts_hashmap_from_array(&factory, thp, n/2, 0);
        erts_factory_close(&factory);
        if (p->mbuf) {
            Uint live = Arg(2);
            reg[live] = res;
            erts_garbage_collect(p, 0, reg, live+1);
            res       = reg[live];
            E = p->stop;
        }
        return res;
    }

    if (HeapWordsLeft(p) < need) {
	erts_garbage_collect(p, need, reg, Arg(2));
    }

    thp    = p->htop;
    mhp    = thp + 1 + n/2;
    E      = p->stop;
    keys   = make_tuple(thp);
    *thp++ = make_arityval(n/2);

    mp = (flatmap_t *)mhp; mhp += MAP_HEADER_FLATMAP_SZ;
    mp->thing_word = MAP_HEADER_FLATMAP;
    mp->size = n/2;
    mp->keys = keys;

    for (i = 0; i < n/2; i++) {
	GET_TERM(*ptr++, *thp++);
	GET_TERM(*ptr++, *mhp++);
    }
    p->htop = mhp;
    return make_flatmap(mp);
}

static Eterm
update_map_assoc(Process* p, Eterm* reg, Eterm map, BeamInstr* I)
{
    Uint n;
    Uint num_old;
    Uint num_updates;
    Uint need;
    flatmap_t *old_mp, *mp;
    Eterm res;
    Eterm* hp;
    Eterm* E;
    Eterm* old_keys;
    Eterm* old_vals;
    BeamInstr* new_p;
    Eterm new_key;
    Eterm* kp;

    new_p = &Arg(5);
    num_updates = Arg(4) / 2;

    if (is_not_flatmap(map)) {
	Uint32 hx;
	Eterm val;

	/* apparently the compiler does not emit is_map instructions,
	 * bad compiler */

	if (is_not_hashmap(map))
	    return THE_NON_VALUE;

	res = map;
	E = p->stop;
	while(num_updates--) {
	    /* assoc can't fail */
	    GET_TERM(new_p[0], new_key);
	    GET_TERM(new_p[1], val);
	    hx = hashmap_make_hash(new_key);

	    res = erts_hashmap_insert(p, hx, new_key, val, res,  0);
	    if (p->mbuf) {
		Uint live = Arg(3);
		reg[live] = res;
		erts_garbage_collect(p, 0, reg, live+1);
		res       = reg[live];
		E = p->stop;
	    }

	    new_p += 2;
	}
	return res;
    }

    old_mp  = (flatmap_t *) flatmap_val(map);
    num_old = flatmap_get_size(old_mp);

    /*
     * If the old map is empty, create a new map.
     */

    if (num_old == 0) {
	return new_map(p, reg, I+1);
    }

    /*
     * Allocate heap space for the worst case (i.e. all keys in the
     * update list are new).
     */

    need = 2*(num_old+num_updates) + 1 + MAP_HEADER_FLATMAP_SZ;
    if (HeapWordsLeft(p) < need) {
	Uint live = Arg(3);
	reg[live] = map;
	erts_garbage_collect(p, need, reg, live+1);
	map      = reg[live];
	old_mp   = (flatmap_t *)flatmap_val(map);
    }

    /*
     * Build the skeleton for the map, ready to be filled in.
     *
     * +-----------------------------------+
     * | (Space for aritvyal for keys)     | <-----------+
     * +-----------------------------------+		 |
     * | (Space for key 1)		   |		 |    <-- kp
     * +-----------------------------------+		 |
     *        .				    		 |
     *        .				    		 |
     *        .				    		 |
     * +-----------------------------------+		 |
     * | (Space for last key)		   |		 |
     * +-----------------------------------+		 |
     * | MAP_HEADER			   |		 |
     * +-----------------------------------+		 |
     * | (Space for number of keys/values) |		 |
     * +-----------------------------------+		 |
     * | Boxed tuple pointer            >----------------+
     * +-----------------------------------+
     * | (Space for value 1)		   |                  <-- hp
     * +-----------------------------------+
     */

    E = p->stop;
    kp = p->htop + 1;		/* Point to first key */
    hp = kp + num_old + num_updates;

    res = make_flatmap(hp);
    mp = (flatmap_t *)hp;
    hp += MAP_HEADER_FLATMAP_SZ;
    mp->thing_word = MAP_HEADER_FLATMAP;
    mp->keys = make_tuple(kp-1);

    old_vals = flatmap_get_values(old_mp);
    old_keys = flatmap_get_keys(old_mp);

    GET_TERM(*new_p, new_key);
    n = num_updates;

    /*
     * Fill in keys and values, until we run out of either updates
     * or old values and keys.
     */

    for (;;) {
	Eterm key;
	Sint c;

	ASSERT(kp < (Eterm *)mp);
	key = *old_keys;
	if ((c = CMP_TERM(key, new_key)) < 0) {
	    /* Copy old key and value */
	    *kp++ = key;
	    *hp++ = *old_vals;
	    old_keys++, old_vals++, num_old--;
	} else {		/* Replace or insert new */
	    GET_TERM(new_p[1], *hp++);
	    if (c > 0) {	/* If new new key */
		*kp++ = new_key;
	    } else {		/* If replacement */
		*kp++ = key;
		old_keys++, old_vals++, num_old--;
	    }
	    n--;
	    if (n == 0) {
		break;
	    } else {
		new_p += 2;
		GET_TERM(*new_p, new_key);
	    }
	}
	if (num_old == 0) {
	    break;
	}
    }

    /*
     * At this point, we have run out of either old keys and values,
     * or the update list. In other words, at least of one n and
     * num_old must be zero.
     */

    if (n > 0) {
	/*
	 * All old keys and values have been copied, but there
	 * are still new keys and values in the update list that
	 * must be copied.
	 */
	ASSERT(num_old == 0);
	while (n-- > 0) {
	    GET_TERM(new_p[0], *kp++);
	    GET_TERM(new_p[1], *hp++);
	    new_p += 2;
	}
    } else {
	/*
	 * All updates are now done. We may still have old
	 * keys and values that we must copy.
	 */
	ASSERT(n == 0);
	while (num_old-- > 0) {
	    ASSERT(kp < (Eterm *)mp);
	    *kp++ = *old_keys++;
	    *hp++ = *old_vals++;
	}
    }

    /*
     * Calculate how many values that are unused at the end of the
     * key tuple and fill it out with a bignum header.
     */
    if ((n = (Eterm *)mp - kp) > 0) {
	*kp = make_pos_bignum_header(n-1);
    }

    /*
     * Fill in the size of the map in both the key tuple and in the map.
     */

    n = kp - p->htop - 1;	/* Actual number of keys/values */
    *p->htop = make_arityval(n);
    p->htop  = hp;
    mp->size = n;

    /* The expensive case, need to build a hashmap */
    if (n > MAP_SMALL_MAP_LIMIT) {
	res = erts_hashmap_from_ks_and_vs(p,flatmap_get_keys(mp),flatmap_get_values(mp),n);
	if (p->mbuf) {
	    Uint live = Arg(3);
	    reg[live] = res;
	    erts_garbage_collect(p, 0, reg, live+1);
	    res       = reg[live];
	}
    }
    return res;
}

/*
 * Update values for keys that already exist in the map.
 */

static Eterm
update_map_exact(Process* p, Eterm* reg, Eterm map, BeamInstr* I)
{
    Uint n;
    Uint i;
    Uint num_old;
    Uint need;
    flatmap_t *old_mp, *mp;
    Eterm res;
    Eterm* hp;
    Eterm* E;
    Eterm* old_keys;
    Eterm* old_vals;
    BeamInstr* new_p;
    Eterm new_key;

    new_p = &Arg(5);
    n = Arg(4) / 2;		/* Number of values to be updated */
    ASSERT(n > 0);

    if (is_not_flatmap(map)) {
	Uint32 hx;
	Eterm val;

	/* apparently the compiler does not emit is_map instructions,
	 * bad compiler */

	if (is_not_hashmap(map)) {
	    p->freason = BADMAP;
	    p->fvalue = map;
	    return THE_NON_VALUE;
	}

	res = map;
	E = p->stop;
	while(n--) {
	    GET_TERM(new_p[0], new_key);
	    GET_TERM(new_p[1], val);
	    hx = hashmap_make_hash(new_key);

	    res = erts_hashmap_insert(p, hx, new_key, val, res,  1);
	    if (is_non_value(res)) {
		p->fvalue = new_key;
		p->freason = BADKEY;
		return res;
	    }

	    if (p->mbuf) {
		Uint live = Arg(3);
		reg[live] = res;
		erts_garbage_collect(p, 0, reg, live+1);
		res       = reg[live];
		E = p->stop;
	    }

	    new_p += 2;
	}
	return res;
    }

    old_mp = (flatmap_t *) flatmap_val(map);
    num_old = flatmap_get_size(old_mp);

    /*
     * If the old map is empty, fail.
     */

    if (num_old == 0) {
	E = p->stop;
	p->freason = BADKEY;
	GET_TERM(new_p[0], p->fvalue);
	return THE_NON_VALUE;
    }

    /*
     * Allocate the exact heap space needed.
     */

    need = num_old + MAP_HEADER_FLATMAP_SZ;
    if (HeapWordsLeft(p) < need) {
	Uint live = Arg(3);
	reg[live] = map;
	erts_garbage_collect(p, need, reg, live+1);
	map      = reg[live];
	old_mp   = (flatmap_t *)flatmap_val(map);
    }

    /*
     * Update map, keeping the old key tuple.
     */

    hp = p->htop;
    E = p->stop;

    old_vals = flatmap_get_values(old_mp);
    old_keys = flatmap_get_keys(old_mp);

    res = make_flatmap(hp);
    mp = (flatmap_t *)hp;
    hp += MAP_HEADER_FLATMAP_SZ;
    mp->thing_word = MAP_HEADER_FLATMAP;
    mp->size = num_old;
    mp->keys = old_mp->keys;

    /* Get array of key/value pairs to be updated */
    GET_TERM(*new_p, new_key);

    /* Update all values */
    for (i = 0; i < num_old; i++) {
	if (!EQ(*old_keys, new_key)) {
	    /* Not same keys */
	    *hp++ = *old_vals;
	} else {
	    GET_TERM(new_p[1], *hp);
	    hp++;
	    n--;
	    if (n == 0) {
		/*
		 * All updates done. Copy remaining values
		 * and return the result.
		 */
		for (i++, old_vals++; i < num_old; i++) {
		    *hp++ = *old_vals++;
		}
		ASSERT(hp == p->htop + need);
		p->htop = hp;
		return res;
	    } else {
		new_p += 2;
		GET_TERM(*new_p, new_key);
	    }
	}
	old_vals++, old_keys++;
    }

    /*
     * Updates left. That means that at least one the keys in the
     * update list did not previously exist.
     */
    ASSERT(hp == p->htop + need);
    p->freason = BADKEY;
    p->fvalue = new_key;
    return THE_NON_VALUE;
}
#undef GET_TERM

int catchlevel(Process *p)
{
    return p->catches;
}

/*
 * Check if the given function is built-in (i.e. a BIF implemented in C).
 *
 * Returns 0 if not built-in, and a non-zero value if built-in.
 */

int
erts_is_builtin(Eterm Mod, Eterm Name, int arity)
{
    Export e;
    Export* ep;

    e.code[0] = Mod;
    e.code[1] = Name;
    e.code[2] = arity;

    if ((ep = export_get(&e)) == NULL) {
	return 0;
    }
    return ep->addressv[erts_active_code_ix()] == ep->code+3
	&& (ep->code[3] == (BeamInstr) em_apply_bif);
}


/*
 * Return the current number of reductions for the given process.
 * To get the total number of reductions, p->reds must be added.
 */

Uint
erts_current_reductions(Process *current, Process *p)
{
    if (current != p) {
	return 0;
    } else if (current->fcalls < 0 && ERTS_PROC_GET_SAVED_CALLS_BUF(current)) {
	return -current->fcalls;
    } else {
	return REDS_IN(current) - current->fcalls;
    }
}

int
erts_beam_jump_table(void)
{
#if defined(NO_JUMP_TABLE)
    return 0;
#else
    return 1;
#endif
}
