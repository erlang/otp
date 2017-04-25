/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1996-2016. All Rights Reserved.
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
#include "erl_nfunc_sched.h"
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
    if ((P))								\
	erts_proc_lc_chk_only_proc_main((P));				\
    ERTS_SMP_LC_ASSERT(!erts_thr_progress_is_blocking());		\
} while (0)
#    define ERTS_SMP_REQ_PROC_MAIN_LOCK(P)				\
do {									\
    if ((P))								\
	erts_proc_lc_require_lock((P), ERTS_PROC_LOCK_MAIN,		\
				  __FILE__, __LINE__);			\
} while (0)
#    define ERTS_SMP_UNREQ_PROC_MAIN_LOCK(P)				\
do {									\
    if ((P))								\
	erts_proc_lc_unrequire_lock((P), ERTS_PROC_LOCK_MAIN);		\
} while (0)
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
  for (i_ = 0; i_ < Arity_; i_++) {      \
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

#define GET_BIF_MODULE(p)  (p->info.mfa.module)
#define GET_BIF_FUNCTION(p)  (p->info.mfa.function)
#define GET_BIF_ARITY(p)  (p->info.mfa.arity)
#define GET_BIF_ADDRESS(p) ((BifFunction) (p->beam[1]))
#define TermWords(t) (((t) / (sizeof(BeamInstr)/sizeof(Eterm))) + !!((t) % (sizeof(BeamInstr)/sizeof(Eterm))))


/*
 * We reuse some of fields in the save area in the process structure.
 * This is safe to do, since this space is only actively used when
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

/*
 * Register target (X or Y register).
 */
#define REG_TARGET(Target) (*(((Target) & 1) ? &yb(Target-1) : &xb(Target)))

/*
 * Store a result into a register given a destination descriptor.
 */

#define StoreResult(Result, DestDesc)		\
  do {						\
    Eterm stb_reg;				\
    stb_reg = (DestDesc);			\
    CHECK_TERM(Result);				\
    REG_TARGET(stb_reg) = (Result);		\
  } while (0)

#define StoreSimpleDest(Src, Dest) Dest = (Src)

/*
 * Store a result into a register and execute the next instruction.
 * Dst points to the word with a destination descriptor, which MUST
 * be just before the next instruction.
 */
 
#define StoreBifResult(Dst, Result)		\
  do {						\
    BeamInstr* stb_next;			\
    Eterm stb_reg;				\
    stb_reg = Arg(Dst);				\
    I += (Dst) + 2;				\
    stb_next = (BeamInstr *) *I;		\
    CHECK_TERM(Result);				\
    REG_TARGET(stb_reg) = (Result);		\
    Goto(stb_next);				\
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
BeamInstr* em_call_bif_e;


/* NOTE These should be the only variables containing trace instructions.
**      Sometimes tests are form the instruction value, and sometimes
**      for the referring variable (one of these), and rouge references
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

#define HEAVY_SWAPIN       \
    SWAPIN;		   \
    FCALLS = c_p->fcalls

#define HEAVY_SWAPOUT      \
    SWAPOUT;		   \
    c_p->fcalls = FCALLS

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
#define r(N) x(N)

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
           PROCESS_MAIN_CHK_LOCKS(c_p); \
           FCALLS -= erts_garbage_collect_nobump(c_p, needed + (HeapNeed), \
						 reg, (M), FCALLS);	\
           ERTS_VERIFY_UNUSED_TEMP_ALLOC(c_p); \
           PROCESS_MAIN_CHK_LOCKS(c_p); \
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
       PROCESS_MAIN_CHK_LOCKS(c_p);                             		\
       FCALLS -= erts_garbage_collect_nobump(c_p, need, reg, (Live), FCALLS);	\
       ERTS_VERIFY_UNUSED_TEMP_ALLOC(c_p);					\
       PROCESS_MAIN_CHK_LOCKS(c_p);                             		\
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
       PROCESS_MAIN_CHK_LOCKS(c_p);                             \
       FCALLS -= erts_garbage_collect_nobump(c_p, need, reg, (Live), FCALLS); \
       ERTS_VERIFY_UNUSED_TEMP_ALLOC(c_p);			\
       PROCESS_MAIN_CHK_LOCKS(c_p);                             \
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
       reg[Live] = Extra;						\
       PROCESS_MAIN_CHK_LOCKS(c_p);					\
       FCALLS -= erts_garbage_collect_nobump(c_p, need, reg, (Live)+1, FCALLS); \
       ERTS_VERIFY_UNUSED_TEMP_ALLOC(c_p);				\
       PROCESS_MAIN_CHK_LOCKS(c_p);					\
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
     HEAVY_SWAPOUT;						\
     r(0) = new_fun(c_p, reg, (ErlFunEntry *) FunP, NumFree);	\
     HEAVY_SWAPIN;						\
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

#define GetR(pos, tr)				\
   do {						\
     tr = Arg(pos);				\
     switch (loader_tag(tr)) {			\
     case LOADER_X_REG:				\
        tr = x(loader_x_reg_index(tr));		\
        break;					\
     case LOADER_Y_REG:				\
        ASSERT(loader_y_reg_index(tr) >= 1);	\
        tr = y(loader_y_reg_index(tr));		\
        break;					\
     }						\
     CHECK_TERM(tr);				\
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

#define Swap(R1, R2)				\
  do {						\
    Eterm V = R1;				\
    R1 = R2;					\
    R2 = V;					\
  } while (0)

#define SwapTemp(R1, R2, Tmp)			\
  do {						\
    Eterm V = R1;				\
    R1 = R2;					\
    R2 = Tmp = V;				\
  } while (0)

#define Move(Src, Dst, Store)      \
   do {                            \
       Eterm term = (Src);         \
       Store(term, Dst);           \
   } while (0)

#define Move2Par(S1, D1, S2, D2)		\
  do {						\
      Eterm V1, V2;				\
      V1 = (S1); V2 = (S2); D1 = V1; D2 = V2;	\
  } while (0)

#define MoveShift(Src, SD, D)			\
  do {						\
    Eterm V;					\
    V = Src; D = SD; SD = V;			\
  } while (0)

#define MoveDup(Src, D1, D2)			\
  do {						\
    D1 = D2 = (Src);				\
  } while (0)

#define Move3(S1, D1, S2, D2, S3, D3) D1 = (S1); D2 = (S2); D3 = (S3)

#define MoveWindow3(S1, S2, S3, D)		\
  do {						\
      Eterm xt0, xt1, xt2;			\
      Eterm *y = &D;				\
      xt0  = S1;				\
      xt1  = S2;				\
      xt2  = S3;				\
      y[0] = xt0;				\
      y[1] = xt1;				\
      y[2] = xt2;				\
 } while (0)

#define MoveWindow4(S1, S2, S3, S4, D)		\
  do {						\
      Eterm xt0, xt1, xt2, xt3;			\
      Eterm *y = &D;				\
      xt0  = S1;				\
      xt1  = S2;				\
      xt2  = S3;				\
      xt3  = S4;				\
      y[0] = xt0;				\
      y[1] = xt1;				\
      y[2] = xt2;				\
      y[3] = xt3;				\
 } while (0)

#define MoveWindow5(S1, S2, S3, S4, S5, D)	\
  do {						\
      Eterm xt0, xt1, xt2, xt3, xt4;		\
      Eterm *y = &D;				\
      xt0  = S1;				\
      xt1  = S2;				\
      xt2  = S3;				\
      xt3  = S4;				\
      xt4  = S5;				\
      y[0] = xt0;				\
      y[1] = xt1;				\
      y[2] = xt2;				\
      y[3] = xt3;				\
      y[4] = xt4;				\
 } while (0)

#define DispatchReturn                          \
do {                                            \
    if (FCALLS > 0 || FCALLS > neg_o_reds) {	\
        FCALLS--;				\
        Goto(*I);                               \
    }                                           \
    else {					\
        c_p->current = NULL;                    \
        c_p->arity = 1;                         \
        goto context_switch3;			\
    }						\
} while (0)

#define MoveReturn(Src)				\
    x(0) = (Src);				\
    I = c_p->cp;				\
    ASSERT(VALID_INSTR(*c_p->cp));		\
    c_p->cp = 0;				\
    CHECK_TERM(r(0));				\
    DispatchReturn

#define DeallocateReturn(Deallocate)       \
  do {                                     \
    int words_to_pop = (Deallocate);       \
    SET_I((BeamInstr *) cp_val(*E));       \
    E = ADD_BYTE_OFFSET(E, words_to_pop);  \
    CHECK_TERM(r(0));                      \
    DispatchReturn;                        \
  } while (0)

#define MoveDeallocateReturn(Src, Deallocate)	\
    x(0) = (Src);				\
    DeallocateReturn(Deallocate)

#define MoveCall(Src, CallDest, Size)		\
    x(0) = (Src);				\
    SET_CP(c_p, I+Size+1);			\
    SET_I((BeamInstr *) CallDest);		\
    Dispatch();

#define MoveCallLast(Src, CallDest, Deallocate)	\
    x(0) = (Src);				\
    RESTORE_CP(E);				\
    E = ADD_BYTE_OFFSET(E, (Deallocate));	\
    SET_I((BeamInstr *) CallDest);		\
    Dispatch();

#define MoveCallOnly(Src, CallDest)		\
    x(0) = (Src);				\
    SET_I((BeamInstr *) CallDest);		\
    Dispatch();

#define MoveJump(Src)				\
     r(0) = (Src);				\
     SET_I((BeamInstr *) Arg(0));		\
     Goto(*I);

#define GetList(Src, H, T)			\
  do {						\
    Eterm* tmp_ptr = list_val(Src);		\
    Eterm hd, tl;				\
    hd = CAR(tmp_ptr);				\
    tl = CDR(tmp_ptr);				\
    H = hd; T = tl;				\
  } while (0)

#define GetTupleElement(Src, Element, Dest)		\
  do {							\
    Eterm* src;						\
    src = ADD_BYTE_OFFSET(tuple_val(Src), (Element));	\
    (Dest) = *src;					\
  } while (0)

#define GetTupleElement2(Src, Element, Dest)		\
  do {							\
    Eterm* src;						\
    Eterm* dst;						\
    Eterm E1, E2;					\
    src = ADD_BYTE_OFFSET(tuple_val(Src), (Element));	\
    dst = &(Dest);					\
    E1 = src[0];					\
    E2 = src[1];					\
    dst[0] = E1;					\
    dst[1] = E2;					\
  } while (0)

#define GetTupleElement2Y(Src, Element, D1, D2)		\
  do {							\
    Eterm* src;						\
    Eterm E1, E2;					\
    src = ADD_BYTE_OFFSET(tuple_val(Src), (Element));	\
    E1 = src[0];					\
    E2 = src[1];					\
    D1 = E1;						\
    D2 = E2;						\
  } while (0)

#define GetTupleElement3(Src, Element, Dest)		\
  do {							\
    Eterm* src;						\
    Eterm* dst;						\
    Eterm E1, E2, E3;					\
    src = ADD_BYTE_OFFSET(tuple_val(Src), (Element));	\
    dst = &(Dest);					\
    E1 = src[0];					\
    E2 = src[1];					\
    E3 = src[2];					\
    dst[0] = E1;					\
    dst[1] = E2;					\
    dst[2] = E3;					\
  } while (0)

#define EqualImmed(X, Y, Action) if (X != Y) { Action; }
#define NotEqualImmed(X, Y, Action) if (X == Y) { Action; }
#define EqualExact(X, Y, Action) if (!EQ(X,Y)) { Action; }
#define NotEqualExact(X, Y, Action) if (EQ(X,Y)) { Action; }
#define Equal(X, Y, Action) CMP_EQ_ACTION(X,Y,Action)
#define NotEqual(X, Y, Action) CMP_NE_ACTION(X,Y,Action)
#define IsLessThan(X, Y, Action) CMP_LT_ACTION(X,Y,Action)
#define IsGreaterEqual(X, Y, Action) CMP_GE_ACTION(X,Y,Action)

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

#define IsNonemptyListTestHeap(Need, Alive, Fail)  \
    if (is_not_list(x(0))) { Fail; }			\
    TestHeap(Need, Alive)

#define IsNonemptyListGetList(Src, H, T, Fail)	\
    if (is_not_list(Src)) {			\
         Fail;					\
    } else {					\
       Eterm* tmp_ptr = list_val(Src);		\
       Eterm hd, tl;				\
       hd = CAR(tmp_ptr);			\
       tl = CDR(tmp_ptr);			\
       H = hd; T = tl;				\
    }

#define IsTuple(X, Action) if (is_not_tuple(X)) Action

#define IsArity(Pointer, Arity, Fail)		\
    if (*tuple_val(Pointer) != (Arity)) {	\
        Fail;					\
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

#ifdef DEBUG
#define IsTupleOfArity(Src, Arityval, Fail)			\
  do {								\
    if (!(is_tuple(Src) && *tuple_val(Src) == Arityval)) {	\
        Fail;							\
    }								\
  } while (0)
#else
#define IsTupleOfArity(Src, Arityval, Fail)			\
  do {								\
    if (!(is_boxed(Src) && *tuple_val(Src) == Arityval)) {	\
        Fail;							\
    }								\
  } while (0)
#endif

#define IsTaggedTuple(Src,Arityval,Tag,Fail) \
  do {                                       \
    if (!(is_tuple(Src) &&                   \
         (tuple_val(Src))[0] == Arityval &&  \
         (tuple_val(Src))[1] == Tag)) {      \
        Fail;                                \
    }                                        \
  } while (0)

#define IsBoolean(X, Fail) if ((X) != am_true && (X) != am_false) { Fail; }

#define IsBinary(Src, Fail) \
 if (is_not_binary(Src) || binary_bitsize(Src) != 0) { Fail; }

#define IsBitstring(Src, Fail) \
  if (is_not_binary(Src)) { Fail; }

#if defined(ARCH_64)
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
      Uint temp_bits;					\
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
      Uint temp_bits;						\
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
static ErtsCodeMFA *ubif2mfa(void* uf) NOINLINE;
static ErtsCodeMFA *gcbif2mfa(void* gcf) NOINLINE;
static BeamInstr* handle_error(Process* c_p, BeamInstr* pc,
			       Eterm* reg, ErtsCodeMFA* bif_mfa) NOINLINE;
static BeamInstr* call_error_handler(Process* p, ErtsCodeMFA* mfa,
				     Eterm* reg, Eterm func) NOINLINE;
static BeamInstr* fixed_apply(Process* p, Eterm* reg, Uint arity,
			      BeamInstr *I, Uint offs) NOINLINE;
static BeamInstr* apply(Process* p, Eterm module, Eterm function,
			Eterm args, Eterm* reg,
			BeamInstr *I, Uint offs) NOINLINE;
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
			    ErtsCodeMFA *bif_mfa, Eterm args);
static struct StackTrace * get_trace_from_exc(Eterm exc);
static Eterm make_arglist(Process* c_p, Eterm* reg, int a);

void
init_emulator(void)
{
    process_main(0, 0);
}

/*
 * On certain platforms, make sure that the main variables really are placed
 * in registers.
 */

#if defined(__GNUC__) && defined(sparc) && !defined(DEBUG)
#  define REG_xregs asm("%l1")
#  define REG_htop asm("%l2")
#  define REG_stop asm("%l3")
#  define REG_I asm("%l4")
#  define REG_fcalls asm("%l5")
#else
#  define REG_xregs
#  define REG_htop
#  define REG_stop
#  define REG_I
#  define REG_fcalls
#endif

#ifdef USE_VM_PROBES
#  define USE_VM_CALL_PROBES
#endif

#ifdef USE_VM_CALL_PROBES

#define DTRACE_LOCAL_CALL(p, mfa)					\
    if (DTRACE_ENABLED(local_function_entry)) {				\
        DTRACE_CHARBUF(process_name, DTRACE_TERM_BUF_SIZE);		\
        DTRACE_CHARBUF(mfa_buf, DTRACE_TERM_BUF_SIZE);			\
        int depth = STACK_START(p) - STACK_TOP(p);			\
        dtrace_fun_decode(p, mfa, process_name, mfa_buf);               \
        DTRACE3(local_function_entry, process_name, mfa_buf, depth);	\
    }

#define DTRACE_GLOBAL_CALL(p, mfa)					\
    if (DTRACE_ENABLED(global_function_entry)) {			\
        DTRACE_CHARBUF(process_name, DTRACE_TERM_BUF_SIZE);		\
        DTRACE_CHARBUF(mfa_buf, DTRACE_TERM_BUF_SIZE);			\
        int depth = STACK_START(p) - STACK_TOP(p);			\
        dtrace_fun_decode(p, mfa, process_name, mfa_buf);               \
        DTRACE3(global_function_entry, process_name, mfa_buf, depth);	\
    }

#define DTRACE_RETURN(p, mfa)                                    \
    if (DTRACE_ENABLED(function_return)) {                      \
        DTRACE_CHARBUF(process_name, DTRACE_TERM_BUF_SIZE);     \
        DTRACE_CHARBUF(mfa_buf, DTRACE_TERM_BUF_SIZE);          \
        int depth = STACK_START(p) - STACK_TOP(p);              \
        dtrace_fun_decode(p, mfa, process_name, mfa_buf);       \
        DTRACE3(function_return, process_name, mfa_buf, depth); \
    }

#define DTRACE_BIF_ENTRY(p, mfa)                                    \
    if (DTRACE_ENABLED(bif_entry)) {                                \
        DTRACE_CHARBUF(process_name, DTRACE_TERM_BUF_SIZE);         \
        DTRACE_CHARBUF(mfa_buf, DTRACE_TERM_BUF_SIZE);              \
        dtrace_fun_decode(p, mfa, process_name, mfa_buf);           \
        DTRACE2(bif_entry, process_name, mfa_buf);                  \
    }

#define DTRACE_BIF_RETURN(p, mfa)                                   \
    if (DTRACE_ENABLED(bif_return)) {                               \
        DTRACE_CHARBUF(process_name, DTRACE_TERM_BUF_SIZE);         \
        DTRACE_CHARBUF(mfa_buf, DTRACE_TERM_BUF_SIZE);              \
        dtrace_fun_decode(p, mfa, process_name, mfa_buf);           \
        DTRACE2(bif_return, process_name, mfa_buf);                 \
    }

#define DTRACE_NIF_ENTRY(p, mfa)                                        \
    if (DTRACE_ENABLED(nif_entry)) {                                    \
        DTRACE_CHARBUF(process_name, DTRACE_TERM_BUF_SIZE);             \
        DTRACE_CHARBUF(mfa_buf, DTRACE_TERM_BUF_SIZE);                  \
        dtrace_fun_decode(p, mfa, process_name, mfa_buf);               \
        DTRACE2(nif_entry, process_name, mfa_buf);                      \
    }

#define DTRACE_NIF_RETURN(p, mfa)                                       \
    if (DTRACE_ENABLED(nif_return)) {                                   \
        DTRACE_CHARBUF(process_name, DTRACE_TERM_BUF_SIZE);             \
        DTRACE_CHARBUF(mfa_buf, DTRACE_TERM_BUF_SIZE);                  \
        dtrace_fun_decode(p, mfa, process_name, mfa_buf);               \
        DTRACE2(nif_return, process_name, mfa_buf);                     \
    }

#define DTRACE_GLOBAL_CALL_FROM_EXPORT(p,e)                                                    \
    do {                                                                                       \
        if (DTRACE_ENABLED(global_function_entry)) {                                           \
            BeamInstr* fp = (BeamInstr *) (((Export *) (e))->addressv[erts_active_code_ix()]); \
            DTRACE_GLOBAL_CALL((p), erts_code_to_codemfa(fp));          \
        }                                                                                      \
    } while(0)

#define DTRACE_RETURN_FROM_PC(p)                                                        \
    do {                                                                                \
        ErtsCodeMFA* cmfa;                                                                  \
        if (DTRACE_ENABLED(function_return) && (cmfa = find_function_from_pc((p)->cp))) { \
            DTRACE_RETURN((p), cmfa);                               \
        }                                                                               \
    } while(0)

#else /* USE_VM_PROBES */
#define DTRACE_LOCAL_CALL(p, mfa)        do {} while (0)
#define DTRACE_GLOBAL_CALL(p, mfa)       do {} while (0)
#define DTRACE_GLOBAL_CALL_FROM_EXPORT(p, e) do {} while (0)
#define DTRACE_RETURN(p, mfa)            do {} while (0)
#define DTRACE_RETURN_FROM_PC(p)             do {} while (0)
#define DTRACE_BIF_ENTRY(p, mfa)         do {} while (0)
#define DTRACE_BIF_RETURN(p, mfa)        do {} while (0)
#define DTRACE_NIF_ENTRY(p, mfa)         do {} while (0)
#define DTRACE_NIF_RETURN(p, mfa)        do {} while (0)
#endif /* USE_VM_PROBES */

#ifdef DEBUG
#define ERTS_DBG_CHK_REDS(P, FC)					\
    do {								\
	if (ERTS_PROC_GET_SAVED_CALLS_BUF((P))) {			\
	    ASSERT(FC <= 0);						\
	    ASSERT(erts_proc_sched_data(c_p)->virtual_reds		\
		   <= 0 - (FC));					\
	}								\
	else {								\
	    ASSERT(FC <= CONTEXT_REDS);					\
	    ASSERT(erts_proc_sched_data(c_p)->virtual_reds		\
		   <= CONTEXT_REDS - (FC));				\
	}								\
} while (0)
#else
#define ERTS_DBG_CHK_REDS(P, FC)
#endif


/*
 * process_main() is called twice:
 * The first call performs some initialisation, including exporting
 * the instructions' C labels to the loader.
 * The second call starts execution of BEAM code. This call never returns.
 */
void process_main(Eterm * x_reg_array, FloatDef* f_reg_array)
{
    static int init_done = 0;
    Process* c_p = NULL;
    int reds_used;
#ifdef DEBUG
    ERTS_DECLARE_DUMMY(Eterm pid);
#endif

    /* Pointer to X registers: x(1)..x(N); reg[0] is used when doing GC,
     * in all other cases x0 is used.
     */
    register Eterm* reg REG_xregs = x_reg_array;

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
     * X registers and floating point registers are located in
     * scheduler specific data.
     */
    register FloatDef *freg = f_reg_array;

    /*
     * For keeping the negative old value of 'reds' when call saving is active.
     */
    int neg_o_reds = 0;

#ifdef ERTS_OPCODE_COUNTER_SUPPORT
    static void* counting_opcodes[] = { DEFINE_COUNTING_OPCODES };
#else
#ifndef NO_JUMP_TABLE
    static void* opcodes[] = { DEFINE_OPCODES };
#else
    int Go;
#endif
#endif

    Eterm pt_arity;		/* Used by do_put_tuple */

    Uint64 start_time = 0;          /* Monitor long schedule */
    BeamInstr* start_time_i = NULL;

    ERTS_MSACC_DECLARE_CACHE_X() /* a cached value of the tsd pointer for msacc */

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
    ASSERT(c_p->arity < 6);
    ASSERT(c_p->debug_reds_in == REDS_IN(c_p));
    if (!ERTS_PROC_GET_SAVED_CALLS_BUF(c_p))
	reds_used = REDS_IN(c_p) - FCALLS;
    else
	reds_used = REDS_IN(c_p) - (CONTEXT_REDS + FCALLS);
    ASSERT(reds_used >= 0);
 do_schedule1:

    if (start_time != 0) {
        Sint64 diff = erts_timestamp_millis() - start_time;
	if (diff > 0 && (Uint) diff >  erts_system_monitor_long_schedule) {
	    ErtsCodeMFA *inptr = find_function_from_pc(start_time_i);
	    ErtsCodeMFA *outptr = find_function_from_pc(c_p->i);
	    monitor_long_schedule_proc(c_p,inptr,outptr,(Uint) diff);
	}
    }

    PROCESS_MAIN_CHK_LOCKS(c_p);
    ERTS_SMP_UNREQ_PROC_MAIN_LOCK(c_p);
    ERTS_VERIFY_UNUSED_TEMP_ALLOC(c_p);
    c_p = erts_schedule(NULL, c_p, reds_used);
    ASSERT(!(c_p->flags & F_HIPE_MODE));
    ERTS_VERIFY_UNUSED_TEMP_ALLOC(c_p);
    start_time = 0;
#ifdef DEBUG
    pid = c_p->common.id; /* Save for debugging purpouses */
#endif
    ERTS_SMP_REQ_PROC_MAIN_LOCK(c_p);
    PROCESS_MAIN_CHK_LOCKS(c_p);

    ERTS_MSACC_UPDATE_CACHE_X();

    if (erts_system_monitor_long_schedule != 0) {
	start_time = erts_timestamp_millis();
	start_time_i = c_p->i;
    }

    ERL_BITS_RELOAD_STATEP(c_p);
    {
	int reds;
	Eterm* argp;
	BeamInstr *next;
	int i;

	argp = c_p->arg_reg;
	for (i = c_p->arity - 1; i >= 0; i--) {
	    reg[i] = argp[i];
	    CHECK_TERM(reg[i]);
	}

	/*
	 * We put the original reduction count in the process structure, to reduce
	 * the code size (referencing a field in a struct through a pointer stored
	 * in a register gives smaller code than referencing a global variable).
	 */

	SET_I(c_p->i);

	REDS_IN(c_p) = reds = c_p->fcalls;
#ifdef DEBUG
	c_p->debug_reds_in = reds;
#endif

	if (ERTS_PROC_GET_SAVED_CALLS_BUF(c_p)) {
	    neg_o_reds = -CONTEXT_REDS;
	    FCALLS = neg_o_reds + reds;
	} else {
	    neg_o_reds = 0;
	    FCALLS = reds;
	}

	ERTS_DBG_CHK_REDS(c_p, FCALLS);

	next = (BeamInstr *) *I;
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
                ErtsCodeMFA *cmfa = find_function_from_pc(c_p->i);
                if (cmfa) {
                    dtrace_fun_decode(c_p, cmfa,
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

	{
	    Eterm increment_reg_val;
	    Eterm increment_val;
	    Uint live;
	    Eterm result;

	OpCase(i_increment_rIId):
	    increment_reg_val = x(0);
	    I--;
	    goto do_increment;

	OpCase(i_increment_xIId):
	    increment_reg_val = xb(Arg(0));
	    goto do_increment;

	OpCase(i_increment_yIId):
	    increment_reg_val = yb(Arg(0));
	    goto do_increment;

	do_increment:
	    increment_val = Arg(1);
	    if (is_small(increment_reg_val)) {
		Sint i = signed_val(increment_reg_val) + increment_val;
		ASSERT(MY_IS_SSMALL(i) == IS_SSMALL(i));
		if (MY_IS_SSMALL(i)) {
		    result = make_small(i);
		    StoreBifResult(3, result);
		}
	    }

	    live = Arg(2);
	    HEAVY_SWAPOUT;
	    reg[live] = increment_reg_val;
	    reg[live+1] = make_small(increment_val);
	    result = erts_gc_mixed_plus(c_p, reg, live);
	    HEAVY_SWAPIN;
	    ERTS_HOLE_CHECK(c_p);
	    if (is_value(result)) {
		StoreBifResult(3, result);
	    }
	    ASSERT(c_p->freason != BADMATCH || is_value(c_p->fvalue));
	    goto find_func_info;
	}

#define DO_OUTLINED_ARITH_2(name, Op1, Op2)	\
 do {						\
     Eterm result;				\
     Uint live = Arg(1);			\
						\
     HEAVY_SWAPOUT;				\
     reg[live] = Op1;				\
     reg[live+1] = Op2;				\
     result = erts_gc_##name(c_p, reg, live);	\
     HEAVY_SWAPIN;				\
     ERTS_HOLE_CHECK(c_p);			\
     if (is_value(result)) {			\
	 StoreBifResult(4, result);		\
     }						\
     goto lb_Cl_error;				\
 } while (0)

 {
     Eterm PlusOp1, PlusOp2;
     Eterm result;

 OpCase(i_plus_jIxxd):
     PlusOp1 = xb(Arg(2));
     PlusOp2 = xb(Arg(3));
     goto do_plus;

 OpCase(i_plus_jIxyd):
     PlusOp1 = xb(Arg(2));
     PlusOp2 = yb(Arg(3));
     goto do_plus;

 OpCase(i_plus_jIssd):
     GetArg2(2, PlusOp1, PlusOp2);
     goto do_plus;

 do_plus:
     if (is_both_small(PlusOp1, PlusOp2)) {
	 Sint i = signed_val(PlusOp1) + signed_val(PlusOp2);
	 ASSERT(MY_IS_SSMALL(i) == IS_SSMALL(i));
	 if (MY_IS_SSMALL(i)) {
	     result = make_small(i);
             StoreBifResult(4, result);
	 }
     }
     DO_OUTLINED_ARITH_2(mixed_plus, PlusOp1, PlusOp2);
 }

 {
     Eterm MinusOp1, MinusOp2;
     Eterm result;

 OpCase(i_minus_jIxxd):
     MinusOp1 = xb(Arg(2));
     MinusOp2 = xb(Arg(3));
     goto do_minus;

 OpCase(i_minus_jIssd):
     GetArg2(2, MinusOp1, MinusOp2);
     goto do_minus;

 do_minus:
     if (is_both_small(MinusOp1, MinusOp2)) {
	 Sint i = signed_val(MinusOp1) - signed_val(MinusOp2);
	 ASSERT(MY_IS_SSMALL(i) == IS_SSMALL(i));
	 if (MY_IS_SSMALL(i)) {
	     result = make_small(i);
             StoreBifResult(4, result);
	 }
     }
     DO_OUTLINED_ARITH_2(mixed_minus, MinusOp1, MinusOp2);
 }

    {
	Eterm is_eq_exact_lit_val;

    OpCase(i_is_eq_exact_literal_fxc):
	is_eq_exact_lit_val = xb(Arg(1));
	goto do_is_eq_exact_literal;

    OpCase(i_is_eq_exact_literal_fyc):
	is_eq_exact_lit_val = yb(Arg(1));
	goto do_is_eq_exact_literal;

    do_is_eq_exact_literal:
	if (!eq(Arg(2), is_eq_exact_lit_val)) {
	    ClauseFail();
	}
	Next(3);
    }

    {
	Eterm is_ne_exact_lit_val;

    OpCase(i_is_ne_exact_literal_fxc):
	is_ne_exact_lit_val = xb(Arg(1));
	goto do_is_ne_exact_literal;

    OpCase(i_is_ne_exact_literal_fyc):
	is_ne_exact_lit_val = yb(Arg(1));
	goto do_is_ne_exact_literal;

    do_is_ne_exact_literal:
	if (eq(Arg(2), is_ne_exact_lit_val)) {
	    ClauseFail();
	}
	Next(3);
    }

 OpCase(i_move_call_only_fc): {
     r(0) = Arg(1);
 }
 /* FALL THROUGH */
 OpCase(i_call_only_f): {
     SET_I((BeamInstr *) Arg(0));
     DTRACE_LOCAL_CALL(c_p, erts_code_to_codemfa(I));
     Dispatch();
 }

 OpCase(i_move_call_last_fPc): {
     r(0) = Arg(2);
 }
 /* FALL THROUGH */
 OpCase(i_call_last_fP): {
     RESTORE_CP(E);
     E = ADD_BYTE_OFFSET(E, Arg(1));
     SET_I((BeamInstr *) Arg(0));
     DTRACE_LOCAL_CALL(c_p, erts_code_to_codemfa(I));
     Dispatch();
 }

 OpCase(i_move_call_cf): {
     r(0) = Arg(0);
     I++;
 }
 /* FALL THROUGH */
 OpCase(i_call_f): {
     SET_CP(c_p, I+2);
     SET_I((BeamInstr *) Arg(0));
     DTRACE_LOCAL_CALL(c_p, erts_code_to_codemfa(I));
     Dispatch();
 }

 OpCase(i_move_call_ext_last_ePc): {
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

 OpCase(i_move_call_ext_ce): {
     r(0) = Arg(0);
     I++;
 }
 /* FALL THROUGH */
 OpCase(i_call_ext_e):
    SET_CP(c_p, I+2);
    DTRACE_GLOBAL_CALL_FROM_EXPORT(c_p, Arg(0));
    Dispatchx();

 OpCase(i_move_call_ext_only_ec): {
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
    DispatchReturn;
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

     if (!(FCALLS > 0 || FCALLS > neg_o_reds)) {
         /* If we have run out of reductions, we do a context
            switch before calling the bif */
         c_p->arity = 2;
         c_p->current = NULL;
         goto context_switch3;
     }

     PRE_BIF_SWAPOUT(c_p);
     c_p->fcalls = FCALLS - 1;
     result = erl_send(c_p, r(0), x(1));
     PreFetch(0, next);
     ERTS_VERIFY_UNUSED_TEMP_ALLOC(c_p);
     ERTS_SMP_REQ_PROC_MAIN_LOCK(c_p);
     PROCESS_MAIN_CHK_LOCKS(c_p);
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
	 Dispatch();
     }
     goto find_func_info;
 }

    {
	Eterm element_index;
	Eterm element_tuple;

    OpCase(i_element_jxsd):
	element_tuple = xb(Arg(1));
	goto do_element;

    OpCase(i_element_jysd):
	element_tuple = yb(Arg(1));
	goto do_element;

    do_element:
	GetArg1(2, element_index);
	if (is_small(element_index) && is_tuple(element_tuple)) {
	    Eterm* tp = tuple_val(element_tuple);

	    if ((signed_val(element_index) >= 1) &&
		(signed_val(element_index) <= arityval(*tp))) {
		Eterm result = tp[signed_val(element_index)];
		StoreBifResult(3, result);
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

    OpCase(i_fast_element_jxId):
     fast_element_tuple = xb(Arg(1));
     goto do_fast_element;

    OpCase(i_fast_element_jyId):
     fast_element_tuple = yb(Arg(1));
     goto do_fast_element;

    do_fast_element:
	if (is_tuple(fast_element_tuple)) {
	    Eterm* tp = tuple_val(fast_element_tuple);
	    Eterm pos = Arg(2);	/* Untagged integer >= 1 */
	    if (pos <= arityval(*tp)) {
		Eterm result = tp[pos];
		StoreBifResult(3, result);
	    }
	}
     goto badarg;
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
	     if (E - HTOP < 3) {
		 SWAPOUT;
		 PROCESS_MAIN_CHK_LOCKS(c_p);
		 FCALLS -= erts_garbage_collect_nobump(c_p, 3, reg+2, 1, FCALLS);
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
 OpCase(i_loop_rec_f):
 {
     BeamInstr *next;
     ErtsMessage* msgp;

     /*
      * We need to disable GC while matching messages
      * in the queue. This since messages with data outside
      * the heap will be corrupted by a GC.
      */
     ASSERT(!(c_p->flags & F_DELAY_GC));
     c_p->flags |= F_DELAY_GC;

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
	     c_p->flags &= ~F_DELAY_GC;
             c_p->arity = 0;
	     goto do_schedule; /* Will be rescheduled for exit */
	 }
	 ERTS_SMP_MSGQ_MV_INQ2PRIVQ(c_p);
	 msgp = PEEK_MESSAGE(c_p);
	 if (msgp)
	     erts_smp_proc_unlock(c_p, ERTS_PROC_LOCKS_MSG_RECEIVE);
	 else
#endif
	 {
	     c_p->flags &= ~F_DELAY_GC;
	     SET_I((BeamInstr *) Arg(0));
	     Goto(*I);		/* Jump to a wait or wait_timeout instruction */
	 }
     }
     if (is_non_value(ERL_MESSAGE_TERM(msgp))) {
	 SWAPOUT; /* erts_decode_dist_message() may write to heap... */
	 if (!erts_decode_dist_message(c_p, ERTS_PROC_LOCK_MAIN, msgp, 0)) {
	     /*
	      * A corrupt distribution message that we weren't able to decode;
	      * remove it...
	      */
	     /* No swapin should be needed */
	     ASSERT(HTOP == c_p->htop && E == c_p->stop);
	     /* TODO: Add DTrace probe for this bad message situation? */
	     UNLINK_MESSAGE(c_p, msgp);
	     msgp->next = NULL;
	     erts_cleanup_messages(msgp);
	     goto loop_rec__;
	 }
	 SWAPIN;
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
     ErtsMessage* msgp;
     PROCESS_MAIN_CHK_LOCKS(c_p);

     ERTS_CHK_MBUF_SZ(c_p);

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
	     } else {
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
         if (have_seqtrace(token2)) {
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

     erts_save_message_in_proc(c_p, msgp);
     c_p->flags &= ~F_DELAY_GC;

     if (ERTS_IS_GC_DESIRED_INTERNAL(c_p, HTOP, E)) {
	 /*
	  * We want to GC soon but we leave a few
	  * reductions giving the message some time
	  * to turn into garbage.
	  */
	 ERTS_VBUMP_LEAVE_REDS_INTERNAL(c_p, 5, FCALLS);
     }

     ERTS_DBG_CHK_REDS(c_p, FCALLS);
     ERTS_CHK_MBUF_SZ(c_p);

     ERTS_VERIFY_UNUSED_TEMP_ALLOC(c_p);
     PROCESS_MAIN_CHK_LOCKS(c_p);
     NextPF(0, next);
 }

    /*
     * Advance the save pointer to the next message (the current
     * message didn't match), then jump to the loop_rec instruction.
     */
 OpCase(loop_rec_end_f): {

     ASSERT(c_p->flags & F_DELAY_GC);

     SET_I((BeamInstr *) Arg(0));
     SAVE_MESSAGE(c_p);
     if (FCALLS > 0 || FCALLS > neg_o_reds) {
	 FCALLS--;
	 goto loop_rec__;
     }

     c_p->flags &= ~F_DELAY_GC;
     c_p->i = I;
     SWAPOUT;
     c_p->arity = 0;
     c_p->current = NULL;
     goto do_schedule;
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
#ifndef ERTS_SMP
	     if (ERTS_PROC_IS_EXITING(c_p)) {
		 /*
		  * I non smp case:
		  *
		  * Currently executing process might be sent an exit
		  * signal if it is traced by a port that it also is
		  * linked to, and the port terminates during the
		  * trace. In this case we do *not* want to clear
		  * the active flag, which will make the process hang
		  * in limbo forever.
		  */
		 SWAPOUT;
                 c_p->arity = 0;
		 goto do_schedule;
	     }
#endif
	     c_p->i = (BeamInstr *) Arg(0); /* L1 */
	     SWAPOUT;
	     c_p->arity = 0;

	     if (!ERTS_PTMR_IS_TIMED_OUT(c_p))
		 erts_smp_atomic32_read_band_relb(&c_p->state,
						  ~ERTS_PSFLG_ACTIVE);
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
	 trace_receive(c_p, am_clock_service, am_timeout, NULL);
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
	 switch (loader_tag(term)) {
	 case LOADER_X_REG:
	     *hp++ = x(loader_x_reg_index(term));
	     break;
	 case LOADER_Y_REG:
	     *hp++ = y(loader_y_reg_index(term));
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

     HEAVY_SWAPOUT;
     res = new_map(c_p, reg, I-1);
     HEAVY_SWAPIN;
     StoreResult(res, Arg(0));
     Next(3+Arg(2));
 }

#define PUT_TERM_REG(term, desc)		\
do {						\
    switch (loader_tag(desc)) {			\
    case LOADER_X_REG:				\
	x(loader_x_reg_index(desc)) = (term);	\
	break;					\
    case LOADER_Y_REG:				\
	y(loader_y_reg_index(desc)) = (term);	\
	break;					\
    default:					\
	ASSERT(0);				\
	break;					\
    }						\
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
     HEAVY_SWAPOUT;
     res = update_map_assoc(c_p, reg, map, I);
     HEAVY_SWAPIN;
     if (is_value(res)) {
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
     HEAVY_SWAPOUT;
     res = update_map_exact(c_p, reg, map, I);
     HEAVY_SWAPIN;
     if (is_value(res)) {
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
	ErtsBifFunc bf;
	Eterm tmp_reg[1];
	Eterm result;

	GetArg1(2, tmp_reg[0]);
	bf = (BifFunction) Arg(1);
	ERTS_DBG_CHK_REDS(c_p, FCALLS);
	c_p->fcalls = FCALLS;
	PROCESS_MAIN_CHK_LOCKS(c_p);
	ASSERT(!ERTS_PROC_IS_EXITING(c_p));
	ERTS_CHK_MBUF_SZ(c_p);
	result = (*bf)(c_p, tmp_reg, I);
	ERTS_CHK_MBUF_SZ(c_p);
	ASSERT(!ERTS_PROC_IS_EXITING(c_p) || is_non_value(result));
	ERTS_VERIFY_UNUSED_TEMP_ALLOC(c_p);
	PROCESS_MAIN_CHK_LOCKS(c_p);
	ERTS_HOLE_CHECK(c_p);
	FCALLS = c_p->fcalls;
	ERTS_DBG_CHK_REDS(c_p, FCALLS);
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
	ErtsBifFunc bf;

	Eterm tmp_reg[1];
	Eterm result;

	GetArg1(1, tmp_reg[0]);
	bf = (ErtsBifFunc) Arg(0);
	ERTS_DBG_CHK_REDS(c_p, FCALLS);
	c_p->fcalls = FCALLS;
	PROCESS_MAIN_CHK_LOCKS(c_p);
	ASSERT(!ERTS_PROC_IS_EXITING(c_p));
	ERTS_CHK_MBUF_SZ(c_p);
	result = (*bf)(c_p, tmp_reg, I);
	ERTS_CHK_MBUF_SZ(c_p);
	ASSERT(!ERTS_PROC_IS_EXITING(c_p) || is_non_value(result));
	ERTS_VERIFY_UNUSED_TEMP_ALLOC(c_p);
	PROCESS_MAIN_CHK_LOCKS(c_p);
	ERTS_HOLE_CHECK(c_p);
	FCALLS = c_p->fcalls;
	ERTS_DBG_CHK_REDS(c_p, FCALLS);
	if (is_value(result)) {
	    StoreBifResult(2, result);
	}
	reg[0] = tmp_reg[0];
	SWAPOUT;
	I = handle_error(c_p, I, reg, ubif2mfa((void *) bf));
	goto post_error_handling;
    }

 OpCase(i_gc_bif1_jIsId):
    {
	typedef Eterm (*GcBifFunction)(Process*, Eterm*, Uint);
	GcBifFunction bf;
	Eterm result;
	Uint live = (Uint) Arg(3);

	GetArg1(2, x(live));
	bf = (GcBifFunction) Arg(1);
	ERTS_DBG_CHK_REDS(c_p, FCALLS);
	c_p->fcalls = FCALLS;
	SWAPOUT;
	PROCESS_MAIN_CHK_LOCKS(c_p);
	ERTS_SMP_UNREQ_PROC_MAIN_LOCK(c_p);
	ERTS_CHK_MBUF_SZ(c_p);
	result = (*bf)(c_p, reg, live);
	ERTS_CHK_MBUF_SZ(c_p);
	ERTS_VERIFY_UNUSED_TEMP_ALLOC(c_p);
	ERTS_SMP_REQ_PROC_MAIN_LOCK(c_p);
	PROCESS_MAIN_CHK_LOCKS(c_p);
	SWAPIN;
	ERTS_HOLE_CHECK(c_p);
	FCALLS = c_p->fcalls;
	ERTS_DBG_CHK_REDS(c_p, FCALLS);
	if (is_value(result)) {
	    StoreBifResult(4, result);
	}
	if (Arg(0) != 0) {
	    SET_I((BeamInstr *) Arg(0));
	    Goto(*I);
	}
	x(0) = x(live);
	I = handle_error(c_p, I, reg, gcbif2mfa((void *) bf));
	goto post_error_handling;
    }

 OpCase(i_gc_bif2_jIIssd): /* Note, one less parameter than the i_gc_bif1
			    and i_gc_bif3 */
    {
	typedef Eterm (*GcBifFunction)(Process*, Eterm*, Uint);
	GcBifFunction bf;
	Eterm result;
	Uint live = (Uint) Arg(2);

	GetArg2(3, x(live), x(live+1));
	/*
	 * XXX This calling convention does not make sense. 'live'
	 * should point out the first argument, not the second
	 * (i.e. 'live' should not be incremented below).
	 */
	live++;
	bf = (GcBifFunction) Arg(1);
	ERTS_DBG_CHK_REDS(c_p, FCALLS);
	c_p->fcalls = FCALLS;
	SWAPOUT;
	PROCESS_MAIN_CHK_LOCKS(c_p);
	ERTS_SMP_UNREQ_PROC_MAIN_LOCK(c_p);
	ERTS_CHK_MBUF_SZ(c_p);
	result = (*bf)(c_p, reg, live);
	ERTS_CHK_MBUF_SZ(c_p);
	ERTS_VERIFY_UNUSED_TEMP_ALLOC(c_p);
	ERTS_SMP_REQ_PROC_MAIN_LOCK(c_p);
	PROCESS_MAIN_CHK_LOCKS(c_p);
	SWAPIN;
	ERTS_HOLE_CHECK(c_p);
	FCALLS = c_p->fcalls;
	ERTS_DBG_CHK_REDS(c_p, FCALLS);
	if (is_value(result)) {
	    StoreBifResult(5, result);
	}
	if (Arg(0) != 0) {
	    SET_I((BeamInstr *) Arg(0));
	    Goto(*I);
	}
	live--;
	x(0) = x(live);
	x(1) = x(live+1);
	I = handle_error(c_p, I, reg, gcbif2mfa((void *) bf));
	goto post_error_handling;
    }

 OpCase(i_gc_bif3_jIIssd):
    {
	typedef Eterm (*GcBifFunction)(Process*, Eterm*, Uint);
	GcBifFunction bf;
	Eterm result;
	Uint live = (Uint) Arg(2);

	x(live) = x(SCRATCH_X_REG);
	GetArg2(3, x(live+1), x(live+2));
	/*
	 * XXX This calling convention does not make sense. 'live'
	 * should point out the first argument, not the third
	 * (i.e. 'live' should not be incremented below).
	 */
	live += 2;
	bf = (GcBifFunction) Arg(1);
	ERTS_DBG_CHK_REDS(c_p, FCALLS);
	c_p->fcalls = FCALLS;
	SWAPOUT;
	PROCESS_MAIN_CHK_LOCKS(c_p);
	ERTS_SMP_UNREQ_PROC_MAIN_LOCK(c_p);
	ERTS_CHK_MBUF_SZ(c_p);
	result = (*bf)(c_p, reg, live);
	ERTS_CHK_MBUF_SZ(c_p);
	ERTS_VERIFY_UNUSED_TEMP_ALLOC(c_p);
	ERTS_SMP_REQ_PROC_MAIN_LOCK(c_p);
	PROCESS_MAIN_CHK_LOCKS(c_p);
	SWAPIN;
	ERTS_HOLE_CHECK(c_p);
	FCALLS = c_p->fcalls;
	ERTS_DBG_CHK_REDS(c_p, FCALLS);
	if (is_value(result)) {
	    StoreBifResult(5, result);
	}
	if (Arg(0) != 0) {
	    SET_I((BeamInstr *) Arg(0));
	    Goto(*I);
	}
	live -= 2;
	x(0) = x(live);
	x(1) = x(live+1);
	x(2) = x(live+2);
	I = handle_error(c_p, I, reg, gcbif2mfa((void *) bf));
	goto post_error_handling;
    }

 /*
  * Guards bifs and, or, xor in guards.
  */
 OpCase(i_bif2_fbssd):
    {
	Eterm tmp_reg[2];
	ErtsBifFunc bf;
	Eterm result;

	GetArg2(2, tmp_reg[0], tmp_reg[1]);
	bf = (ErtsBifFunc) Arg(1);
	ERTS_DBG_CHK_REDS(c_p, FCALLS);
	c_p->fcalls = FCALLS;
	PROCESS_MAIN_CHK_LOCKS(c_p);
	ASSERT(!ERTS_PROC_IS_EXITING(c_p));
	ERTS_CHK_MBUF_SZ(c_p);
	result = (*bf)(c_p, tmp_reg, I);
	ERTS_CHK_MBUF_SZ(c_p);
	ASSERT(!ERTS_PROC_IS_EXITING(c_p) || is_non_value(result));
	ERTS_VERIFY_UNUSED_TEMP_ALLOC(c_p);
	PROCESS_MAIN_CHK_LOCKS(c_p);
	ERTS_HOLE_CHECK(c_p);
	FCALLS = c_p->fcalls;
	ERTS_DBG_CHK_REDS(c_p, FCALLS);
	if (is_value(result)) {
	    StoreBifResult(4, result);
	}
	SET_I((BeamInstr *) Arg(0));
	Goto(*I);
    }

 /*
  * Guards bifs and, or, xor, relational operators in body.
  */
 OpCase(i_bif2_body_bssd):
    {
	Eterm tmp_reg[2];
	ErtsBifFunc bf;
	Eterm result;

	GetArg2(1, tmp_reg[0], tmp_reg[1]);
	bf = (ErtsBifFunc) Arg(0);
	PROCESS_MAIN_CHK_LOCKS(c_p);
	ASSERT(!ERTS_PROC_IS_EXITING(c_p));
	ERTS_CHK_MBUF_SZ(c_p);
	result = (*bf)(c_p, tmp_reg, I);
	ERTS_CHK_MBUF_SZ(c_p);
	ASSERT(!ERTS_PROC_IS_EXITING(c_p) || is_non_value(result));
	ERTS_VERIFY_UNUSED_TEMP_ALLOC(c_p);
	PROCESS_MAIN_CHK_LOCKS(c_p);
	ERTS_HOLE_CHECK(c_p);
	if (is_value(result)) {
	    ASSERT(!is_CP(result));
	    StoreBifResult(3, result);
	}
	reg[0] = tmp_reg[0];
	reg[1] = tmp_reg[1];
	SWAPOUT;
	I = handle_error(c_p, I, reg, ubif2mfa((void *) bf));
	goto post_error_handling;
    }

    /*
     * The most general BIF call.  The BIF may build any amount of data
     * on the heap.  The result is always returned in r(0).
     */
 OpCase(call_bif_e):
    {
	ErtsBifFunc bf;
	Eterm result;
	BeamInstr *next;
	ErlHeapFragment *live_hf_end;
        Export *export = (Export*)Arg(0);


        if (!((FCALLS - 1) > 0 || (FCALLS-1) > neg_o_reds)) {
            /* If we have run out of reductions, we do a context
               switch before calling the bif */
            c_p->arity = GET_BIF_ARITY(export);
            c_p->current = &export->info.mfa;
            goto context_switch3;
        }

        ERTS_MSACC_SET_BIF_STATE_CACHED_X(
            GET_BIF_MODULE(export), GET_BIF_ADDRESS(export));

	bf = GET_BIF_ADDRESS(export);

	PRE_BIF_SWAPOUT(c_p);
	ERTS_DBG_CHK_REDS(c_p, FCALLS);
	c_p->fcalls = FCALLS - 1;
	if (FCALLS <= 0) {
	   save_calls(c_p, export);
	}
	PreFetch(1, next);
	ASSERT(!ERTS_PROC_IS_EXITING(c_p));
	ERTS_VERIFY_UNUSED_TEMP_ALLOC(c_p);
	live_hf_end = c_p->mbuf;
	ERTS_CHK_MBUF_SZ(c_p);
	result = (*bf)(c_p, reg, I);
	ERTS_CHK_MBUF_SZ(c_p);
	ASSERT(!ERTS_PROC_IS_EXITING(c_p) || is_non_value(result));
	ERTS_VERIFY_UNUSED_TEMP_ALLOC(c_p);
	ERTS_HOLE_CHECK(c_p);
	ERTS_SMP_REQ_PROC_MAIN_LOCK(c_p);
	if (ERTS_IS_GC_DESIRED(c_p)) {
	    Uint arity = GET_BIF_ARITY(export);
	    result = erts_gc_after_bif_call_lhf(c_p, live_hf_end, result, reg, arity);
	    E = c_p->stop;
	}
	PROCESS_MAIN_CHK_LOCKS(c_p);
	HTOP = HEAP_TOP(c_p);
	FCALLS = c_p->fcalls;
	ERTS_DBG_CHK_REDS(c_p, FCALLS);
        /* We have to update the cache if we are enabled in order
           to make sure no book keeping is done after we disabled
           msacc. We don't always do this as it is quite expensive. */
        if (ERTS_MSACC_IS_ENABLED_CACHED_X())
            ERTS_MSACC_UPDATE_CACHE_X();
	ERTS_MSACC_SET_STATE_CACHED_M_X(ERTS_MSACC_STATE_EMULATOR);
	if (is_value(result)) {
	    r(0) = result;
	    CHECK_TERM(r(0));
	    NextPF(1, next);
	} else if (c_p->freason == TRAP) {
	    SET_CP(c_p, I+2);
	    SET_I(c_p->i);
	    SWAPIN;
	    Dispatch();
	}

	/*
	 * Error handling.  SWAPOUT is not needed because it was done above.
	 */
	ASSERT(c_p->stop == E);
	I = handle_error(c_p, I, reg, &export->info.mfa);
	goto post_error_handling;
    }

 /*
  * Arithmetic operations.
  */

 OpCase(i_times_jIssd):
 {
     Eterm Op1, Op2;
     GetArg2(2, Op1, Op2);
     DO_OUTLINED_ARITH_2(mixed_times, Op1, Op2);
 }

 OpCase(i_m_div_jIssd):
 {
     Eterm Op1, Op2;
     GetArg2(2, Op1, Op2);
     DO_OUTLINED_ARITH_2(mixed_div, Op1, Op2);
 }

 OpCase(i_int_div_jIssd):
 {
     Eterm Op1, Op2;

     GetArg2(2, Op1, Op2);
     if (Op2 == SMALL_ZERO) {
	 goto badarith;
     } else if (is_both_small(Op1, Op2)) {
	 Sint ires = signed_val(Op1) / signed_val(Op2);
	 if (MY_IS_SSMALL(ires)) {
	     Eterm result = make_small(ires);
	     StoreBifResult(4, result);
	 }
     }
     DO_OUTLINED_ARITH_2(int_div, Op1, Op2);
 }

 {
     Eterm RemOp1, RemOp2;

 OpCase(i_rem_jIxxd):
     RemOp1 = xb(Arg(2));
     RemOp2 = xb(Arg(3));
     goto do_rem;

 OpCase(i_rem_jIssd):
     GetArg2(2, RemOp1, RemOp2);
     goto do_rem;

 do_rem:
     if (RemOp2 == SMALL_ZERO) {
         goto badarith;
     } else if (is_both_small(RemOp1, RemOp2)) {
         Eterm result = make_small(signed_val(RemOp1) % signed_val(RemOp2));
         StoreBifResult(4, result);
     } else {
	 DO_OUTLINED_ARITH_2(int_rem, RemOp1, RemOp2);
     }
 }

 {
     Eterm BandOp1, BandOp2;

 OpCase(i_band_jIxcd):
     BandOp1 = xb(Arg(2));
     BandOp2 = Arg(3);
     goto do_band;

 OpCase(i_band_jIssd):
     GetArg2(2, BandOp1, BandOp2);
     goto do_band;

 do_band:
     if (is_both_small(BandOp1, BandOp2)) {
         /*
          * No need to untag -- TAG & TAG == TAG.
          */
         Eterm result = BandOp1 & BandOp2;
         StoreBifResult(4, result);
     }
     DO_OUTLINED_ARITH_2(band, BandOp1, BandOp2);
 }

 /*
  * An error occurred in an arithmetic operation or test that could
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

 OpCase(i_bor_jIssd):
 {
     Eterm Op1, Op2;

     GetArg2(2, Op1, Op2);
     if (is_both_small(Op1, Op2)) {
	 /*
	  * No need to untag -- TAG | TAG == TAG.
	  */
	 Eterm result = Op1 | Op2;
	 StoreBifResult(4, result);
     }
     DO_OUTLINED_ARITH_2(bor, Op1, Op2);
 }

 OpCase(i_bxor_jIssd):
 {
     Eterm Op1, Op2;

     GetArg2(2, Op1, Op2);
     if (is_both_small(Op1, Op2)) {
	 /*
          * TAG ^ TAG == 0.
          *
          * Therefore, we perform the XOR operation on the tagged values,
          * and OR in the tag bits.
	  */
	 Eterm result = (Op1 ^ Op2) | make_small(0);
	 StoreBifResult(4, result);
     }
     DO_OUTLINED_ARITH_2(bxor, Op1, Op2);
 }

 {
     Eterm Op1, Op2;
     Sint i;
     Sint ires;
     Eterm* bigp;
     Eterm tmp_big[2];

     OpCase(i_bsr_jIssd):
         GetArg2(2, Op1, Op2);
	 if (is_small(Op2)) {
	     i = -signed_val(Op2);
	     if (is_small(Op1)) {
		 goto small_shift;
	     } else if (is_big(Op1)) {
		 if (i == 0) {
		     StoreBifResult(4, Op1);
		 }
		 ires = big_size(Op1);
		 goto big_shift;
	     }
	 } else if (is_big(Op2)) {
	     /*
	      * N bsr NegativeBigNum == N bsl MAX_SMALL
	      * N bsr PositiveBigNum == N bsl MIN_SMALL
	      */
	     Op2 = make_small(bignum_header_is_neg(*big_val(Op2)) ?
				   MAX_SMALL : MIN_SMALL);
	     goto do_bsl;
	}
     goto badarith;
     
     OpCase(i_bsl_jIssd):
         GetArg2(2, Op1, Op2);
 do_bsl:
	 if (is_small(Op2)) {
	     i = signed_val(Op2);

	     if (is_small(Op1)) {
	     small_shift:
		 ires = signed_val(Op1);
	     
		 if (i == 0 || ires == 0) {
		     StoreBifResult(4, Op1);
		 } else if (i < 0)  { /* Right shift */
		     i = -i;
		     if (i >= SMALL_BITS-1) {
			 Op1 = (ires < 0) ? SMALL_MINUS_ONE : SMALL_ZERO;
		     } else {
			 Op1 = make_small(ires >> i);
		     }
		     StoreBifResult(4, Op1);
		 } else if (i < SMALL_BITS-1) { /* Left shift */
		     if ((ires > 0 && ((~(Uint)0 << ((SMALL_BITS-1)-i)) & ires) == 0) ||
			 ((~(Uint)0 << ((SMALL_BITS-1)-i)) & ~ires) == 0) {
			 Op1 = make_small(ires << i);
			 StoreBifResult(4, Op1);
		     }
		 }
		 ires = 1; /* big_size(small_to_big(Op1)) */

	     big_shift:
		 if (i > 0) {	/* Left shift. */
		     ires += (i / D_EXP);
		 } else {	/* Right shift. */
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
		     TestHeapPreserve(ires+1, Arg(1), Op1);
		     if (is_small(Op1)) {
			 Op1 = small_to_big(signed_val(Op1), tmp_big);
		     }
		     bigp = HTOP;
		     Op1 = big_lshift(Op1, i, bigp);
		     if (is_big(Op1)) {
			 HTOP += bignum_header_arity(*HTOP) + 1;
		     }
		     HEAP_SPACE_VERIFIED(0);
		     if (is_nil(Op1)) {
			 /*
			  * This result must have been only slight larger
			  * than allowed since it wasn't caught by the
			  * previous test.
			  */
			 c_p->freason = SYSTEM_LIMIT;
			 goto lb_Cl_error;
		     }
		     ERTS_HOLE_CHECK(c_p);
		     StoreBifResult(4, Op1);
		 }
	     } else if (is_big(Op1)) {
		 if (i == 0) {
		     StoreBifResult(4, Op1);
		 }
		 ires = big_size(Op1);
		 goto big_shift;
	     }
	 } else if (is_big(Op2)) {
	     if (bignum_header_is_neg(*big_val(Op2))) {
		 /*
		  * N bsl NegativeBigNum is either 0 or -1, depending on
		  * the sign of N. Since we don't believe this case
		  * is common, do the calculation with the minimum
		  * amount of code.
		  */
		 Op2 = make_small(MIN_SMALL);
		 goto do_bsl;
	     } else if (is_small(Op1) || is_big(Op1)) {
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
	 HEAVY_SWAPOUT;
	 reg[live] = bnot_val;
	 bnot_val = erts_gc_bnot(c_p, reg, live);
	 HEAVY_SWAPIN;
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
     HEAVY_SWAPOUT;
     next = apply(c_p, r(0), x(1), x(2), reg, NULL, 0);
     HEAVY_SWAPIN;
     if (next != NULL) {
	 SET_CP(c_p, I+1);
	 SET_I(next);
	 Dispatch();
     }
     I = handle_error(c_p, I, reg, &bif_export[BIF_apply_3]->info.mfa);
     goto post_error_handling;
 }

 OpCase(i_apply_last_P): {
     BeamInstr *next;
     HEAVY_SWAPOUT;
     next = apply(c_p, r(0), x(1), x(2), reg, I, Arg(0));
     HEAVY_SWAPIN;
     if (next != NULL) {
	 SET_CP(c_p, (BeamInstr *) E[0]);
	 E = ADD_BYTE_OFFSET(E, Arg(0));
	 SET_I(next);
	 Dispatch();
     }
     I = handle_error(c_p, I, reg, &bif_export[BIF_apply_3]->info.mfa);
     goto post_error_handling;
 }

 OpCase(i_apply_only): {
     BeamInstr *next;
     HEAVY_SWAPOUT;
     next = apply(c_p, r(0), x(1), x(2), reg, I, 0);
     HEAVY_SWAPIN;
     if (next != NULL) {
	 SET_I(next);
	 Dispatch();
     }
     I = handle_error(c_p, I, reg, &bif_export[BIF_apply_3]->info.mfa);
     goto post_error_handling;
 }

 OpCase(apply_I): {
     BeamInstr *next;

     HEAVY_SWAPOUT;
     next = fixed_apply(c_p, reg, Arg(0), NULL, 0);
     HEAVY_SWAPIN;
     if (next != NULL) {
	 SET_CP(c_p, I+2);
	 SET_I(next);
	 Dispatch();
     }
     I = handle_error(c_p, I, reg, &bif_export[BIF_apply_3]->info.mfa);
     goto post_error_handling;
 }

 OpCase(apply_last_IP): {
     BeamInstr *next;

     HEAVY_SWAPOUT;
     next = fixed_apply(c_p, reg, Arg(0), I, Arg(1));
     HEAVY_SWAPIN;
     if (next != NULL) {
	 SET_CP(c_p, (BeamInstr *) E[0]);
	 E = ADD_BYTE_OFFSET(E, Arg(1));
	 SET_I(next);
	 Dispatch();
     }
     I = handle_error(c_p, I, reg, &bif_export[BIF_apply_3]->info.mfa);
     goto post_error_handling;
 }

 OpCase(i_apply_fun): {
     BeamInstr *next;

     HEAVY_SWAPOUT;
     next = apply_fun(c_p, r(0), x(1), reg);
     HEAVY_SWAPIN;
     if (next != NULL) {
	 SET_CP(c_p, I+1);
	 SET_I(next);
	 Dispatchfun();
     }
     goto find_func_info;
 }

 OpCase(i_apply_fun_last_P): {
     BeamInstr *next;

     HEAVY_SWAPOUT;
     next = apply_fun(c_p, r(0), x(1), reg);
     HEAVY_SWAPIN;
     if (next != NULL) {
	 SET_CP(c_p, (BeamInstr *) E[0]);
	 E = ADD_BYTE_OFFSET(E, Arg(0));
	 SET_I(next);
	 Dispatchfun();
     }
     goto find_func_info;
 }

 OpCase(i_apply_fun_only): {
     BeamInstr *next;

     HEAVY_SWAPOUT;
     next = apply_fun(c_p, r(0), x(1), reg);
     HEAVY_SWAPIN;
     if (next != NULL) {
	 SET_I(next);
	 Dispatchfun();
     }
     goto find_func_info;
 }

 OpCase(i_call_fun_I): {
     BeamInstr *next;

     HEAVY_SWAPOUT;
     next = call_fun(c_p, Arg(0), reg, THE_NON_VALUE);
     HEAVY_SWAPIN;
     if (next != NULL) {
	 SET_CP(c_p, I+2);
	 SET_I(next);
	 Dispatchfun();
     }
     goto find_func_info;
 }

 OpCase(i_call_fun_last_IP): {
     BeamInstr *next;

     HEAVY_SWAPOUT;
     next = call_fun(c_p, Arg(0), reg, THE_NON_VALUE);
     HEAVY_SWAPIN;
     if (next != NULL) {
	SET_CP(c_p, (BeamInstr *) E[0]);
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
    /* Add one for the environment of the fun */
    c_p->arity = erts_code_to_codemfa(I)->arity + 1;
    goto context_switch2;

 context_switch:
    c_p->arity = erts_code_to_codemfa(I)->arity;

 context_switch2: 		/* Entry for fun calls. */
    c_p->current = erts_code_to_codemfa(I);

 context_switch3:

 {
     Eterm* argp;
     int i;

     if (erts_smp_atomic32_read_nob(&c_p->state) & ERTS_PSFLG_EXITING) {
         c_p->i = beam_exit;
         c_p->arity = 0;
         c_p->current = NULL;
         goto do_schedule;
     }

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

     ASSERT(c_p->debug_reds_in == REDS_IN(c_p));
    if (!ERTS_PROC_GET_SAVED_CALLS_BUF(c_p))
	reds_used = REDS_IN(c_p) - FCALLS;
    else
	reds_used = REDS_IN(c_p) - (CONTEXT_REDS + FCALLS);
    ASSERT(reds_used >= 0);

     /*
      * Save the argument registers and everything else.
      */

     argp = c_p->arg_reg;
     for (i = c_p->arity - 1; i >= 0; i--) {
	 argp[i] = reg[i];
     }
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
     GetArg1(0, element);
     tuple = REG_TARGET(Arg(1));
     ASSERT(is_tuple(tuple));
     p = (Eterm *) ((unsigned char *) tuple_val(tuple) + Arg(2));
     *p = element;
     NextPF(3, next);
 }

 OpCase(normal_exit): {
     SWAPOUT;
     c_p->freason = EXC_NORMAL;
     c_p->arity = 0;		/* In case this process will never be garbed again. */
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

 OpCase(i_raise): {
     Eterm raise_trace = x(2);
     Eterm raise_value = x(1);
     struct StackTrace *s;

     c_p->fvalue = raise_value;
     c_p->ftrace = raise_trace;
     s = get_trace_from_exc(raise_trace);
     if (s == NULL) {
	 c_p->freason = EXC_ERROR;
     } else {
	 c_p->freason = PRIMARY_EXCEPTION(s->freason);
     }
     goto find_func_info;
 }

    {
	Eterm badmatch_val;

    OpCase(badmatch_x):
	badmatch_val = xb(Arg(0));
	c_p->fvalue = badmatch_val;
	c_p->freason = BADMATCH;
    }
 /* Fall through here */

 find_func_info: {
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
    HEAVY_SWAPOUT;
    I = call_error_handler(c_p, erts_code_to_codemfa(I),
                           reg, am_undefined_function);
    HEAVY_SWAPIN;
    if (I) {
	Goto(*I);
    }

 /* Fall through */
 OpCase(error_action_code): {
    handle_error:
     SWAPOUT;
     I = handle_error(c_p, NULL, reg, NULL);
 post_error_handling:
     if (I == 0) {
	 goto do_schedule;
     } else {
	 ASSERT(!is_value(r(0)));
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
             *
             * This layout is determined by the NifExport struct
	     */
	    BifFunction vbf;
	    ErlHeapFragment *live_hf_end;
            ErtsCodeMFA *codemfa;

	    ERTS_MSACC_SET_STATE_CACHED_M_X(ERTS_MSACC_STATE_NIF);

	    codemfa = erts_code_to_codemfa(I);

            c_p->current = codemfa; /* current and vbf set to please handle_error */

	    DTRACE_NIF_ENTRY(c_p, codemfa);

	    HEAVY_SWAPOUT;

	    PROCESS_MAIN_CHK_LOCKS(c_p);
	    bif_nif_arity = codemfa->arity;
	    ERTS_SMP_UNREQ_PROC_MAIN_LOCK(c_p);

	    ASSERT(!ERTS_PROC_IS_EXITING(c_p));
	    {
		typedef Eterm NifF(struct enif_environment_t*, int argc, Eterm argv[]);
		NifF* fp = vbf = (NifF*) I[1];
		struct enif_environment_t env;
#ifdef ERTS_SMP
		ASSERT(c_p->scheduler_data);
#endif
		live_hf_end = c_p->mbuf;
		ERTS_CHK_MBUF_SZ(c_p);
		erts_pre_nif(&env, c_p, (struct erl_module_nif*)I[2], NULL);
		nif_bif_result = (*fp)(&env, bif_nif_arity, reg);
		if (env.exception_thrown)
		    nif_bif_result = THE_NON_VALUE;
		erts_post_nif(&env);
		ERTS_CHK_MBUF_SZ(c_p);

		PROCESS_MAIN_CHK_LOCKS(c_p);
		ERTS_VERIFY_UNUSED_TEMP_ALLOC(c_p);
		ERTS_MSACC_SET_STATE_CACHED_M_X(ERTS_MSACC_STATE_EMULATOR);
		ASSERT(!env.exiting);
		ASSERT(!ERTS_PROC_IS_EXITING(c_p));
	    }

	    DTRACE_NIF_RETURN(c_p, codemfa);
	    goto apply_bif_or_nif_epilogue;
	 
	OpCase(apply_bif):
	    /*
	     * At this point, I points to the code[0] in the export entry for
	     * the BIF:
	     *
	     * code[-3]: Module
	     * code[-2]: Function
	     * code[-1]: Arity
	     * code[0]: &&apply_bif
	     * code[1]: Function pointer to BIF function
	     */

            if (!((FCALLS - 1) > 0 || (FCALLS - 1) > neg_o_reds)) {
                /* If we have run out of reductions, we do a context
                   switch before calling the bif */
                goto context_switch;
            }

	    codemfa = erts_code_to_codemfa(I);

            ERTS_MSACC_SET_BIF_STATE_CACHED_X(codemfa->module, (BifFunction)Arg(0));


            /* In case we apply process_info/1,2 or load_nif/1 */
	    c_p->current = codemfa;
	    c_p->i = I;		/* In case we apply check_process_code/2. */
	    c_p->arity = 0;		/* To allow garbage collection on ourselves
					 * (check_process_code/2).
					 */
	    DTRACE_BIF_ENTRY(c_p, codemfa);

	    SWAPOUT;
	    ERTS_DBG_CHK_REDS(c_p, FCALLS - 1);
	    c_p->fcalls = FCALLS - 1;
	    vbf = (BifFunction) Arg(0);
	    PROCESS_MAIN_CHK_LOCKS(c_p);
	    bif_nif_arity = codemfa->arity;
            ASSERT(bif_nif_arity <= 4);
	    ERTS_SMP_UNREQ_PROC_MAIN_LOCK(c_p);
	    ERTS_VERIFY_UNUSED_TEMP_ALLOC(c_p);
	    {
		ErtsBifFunc bf = vbf;
		ASSERT(!ERTS_PROC_IS_EXITING(c_p));
		live_hf_end = c_p->mbuf;
		ERTS_CHK_MBUF_SZ(c_p);
		nif_bif_result = (*bf)(c_p, reg, I);
		ERTS_CHK_MBUF_SZ(c_p);
		ASSERT(!ERTS_PROC_IS_EXITING(c_p) ||
		       is_non_value(nif_bif_result));
		ERTS_VERIFY_UNUSED_TEMP_ALLOC(c_p);
		PROCESS_MAIN_CHK_LOCKS(c_p);
	    }
            /* We have to update the cache if we are enabled in order
               to make sure no book keeping is done after we disabled
               msacc. We don't always do this as it is quite expensive. */
            if (ERTS_MSACC_IS_ENABLED_CACHED_X())
                ERTS_MSACC_UPDATE_CACHE_X();
	    ERTS_MSACC_SET_STATE_CACHED_M_X(ERTS_MSACC_STATE_EMULATOR);
	    DTRACE_BIF_RETURN(c_p, codemfa);

	apply_bif_or_nif_epilogue:
	    ERTS_SMP_REQ_PROC_MAIN_LOCK(c_p);
	    ERTS_HOLE_CHECK(c_p);
	    if (ERTS_IS_GC_DESIRED(c_p)) {
		nif_bif_result = erts_gc_after_bif_call_lhf(c_p, live_hf_end,
							    nif_bif_result,
							    reg, bif_nif_arity);
	    }
	    SWAPIN;  /* There might have been a garbage collection. */
	    FCALLS = c_p->fcalls;
	    ERTS_DBG_CHK_REDS(c_p, FCALLS);
	    if (is_value(nif_bif_result)) {
		r(0) = nif_bif_result;
		CHECK_TERM(r(0));
		SET_I(c_p->cp);
		c_p->cp = 0;
		Goto(*I);
	    } else if (c_p->freason == TRAP) {
		SET_I(c_p->i);
		if (c_p->flags & F_HIBERNATE_SCHED) {
		    c_p->flags &= ~F_HIBERNATE_SCHED;
		    goto do_schedule;
		}
		Dispatch();
	    }
	    I = handle_error(c_p, c_p->cp, reg, c_p->current);
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

 OpCase(i_get_hash_cId):
    {
	Eterm arg;
	Eterm result;

	GetArg1(0, arg);
	result = erts_pd_hash_get_with_hx(c_p, Arg(1), arg);
	StoreBifResult(2, result);
    }

    {
	Eterm case_end_val;

    OpCase(case_end_x):
	case_end_val = xb(Arg(0));
	c_p->fvalue = case_end_val;
	c_p->freason = EXC_CASE_CLAUSE;
	goto find_func_info;
    }

 OpCase(if_end):
    c_p->freason = EXC_IF_CLAUSE;
    goto find_func_info;

 OpCase(i_func_info_IaaI): {
     ErtsCodeInfo *ci = (ErtsCodeInfo*)I;
     c_p->freason = EXC_FUNCTION_CLAUSE;
     c_p->current = &ci->mfa;
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

     OpCase(i_bs_init_bits_fail_heap_sIjId): {
	 GetArg1(0, num_bits_term);
	 alloc = Arg(1);
	 I += 2;
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
     Eterm BsOp1, BsOp2;

     OpCase(i_bs_init_fail_heap_sIjId): {
	 GetArg1(0, BsOp1);
	 BsOp2 = Arg(1);
	 I += 2;
	 goto do_bs_init;
     }

     OpCase(i_bs_init_fail_yjId): {
	 BsOp1 = yb(Arg(0));
	 BsOp2 = 0;
	 I++;
	 goto do_bs_init;
     }

     OpCase(i_bs_init_fail_xjId): {
	 BsOp1 = xb(Arg(0));
	 BsOp2 = 0;
	 I++;
     }
	 /* FALL THROUGH */
     do_bs_init:
         if (is_small(BsOp1)) {
	     Sint size = signed_val(BsOp1);
	     if (size < 0) {
		 goto badarg;
	     }
	     BsOp1 = (Eterm) size;
	 } else {
	     Uint bytes;

	     if (!term_to_Uint(BsOp1, &bytes)) {
		 c_p->freason = bytes;
		 goto lb_Cl_error;
	     }
	     if ((bytes >> (8*sizeof(Uint)-3)) != 0) {
		 goto system_limit;
	     }
	     BsOp1 = (Eterm) bytes;
	 }
	 if (BsOp1 <= ERL_ONHEAP_BIN_LIMIT) {
	     goto do_heap_bin_alloc;
	 } else {
	     goto do_proc_bin_alloc;
	 }


     OpCase(i_bs_init_heap_IIId): {
	 BsOp1 = Arg(0);
	 BsOp2 = Arg(1);
	 I++;
	 goto do_proc_bin_alloc;
     }

     OpCase(i_bs_init_IId): {
	 BsOp1 = Arg(0);
	 BsOp2 = 0;
     }
     /* FALL THROUGH */
     do_proc_bin_alloc: {
	 Binary* bptr;
	 ProcBin* pb;

	 erts_bin_offset = 0;
	 erts_writable_bin = 0;
	 TestBinVHeap(BsOp1 / sizeof(Eterm),
		      BsOp2 + PROC_BIN_SIZE + ERL_SUB_BIN_SIZE, Arg(1));

	 /*
	  * Allocate the binary struct itself.
	  */
	 bptr = erts_bin_nrml_alloc(BsOp1);
	 erts_current_bin = (byte *) bptr->orig_bytes;

	 /*
	  * Now allocate the ProcBin on the heap.
	  */
	 pb = (ProcBin *) HTOP;
	 HTOP += PROC_BIN_SIZE;
	 pb->thing_word = HEADER_PROC_BIN;
	 pb->size = BsOp1;
	 pb->next = MSO(c_p).first;
	 MSO(c_p).first = (struct erl_off_heap_header*) pb;
	 pb->val = bptr;
	 pb->bytes = (byte*) bptr->orig_bytes;
	 pb->flags = 0;
	 
	 OH_OVERHEAD(&(MSO(c_p)), BsOp1 / sizeof(Eterm));

	 StoreBifResult(2, make_binary(pb));
     }

     OpCase(i_bs_init_heap_bin_heap_IIId): {
	 BsOp1 = Arg(0);
	 BsOp2 = Arg(1);
	 I++;
	 goto do_heap_bin_alloc;
     }

     OpCase(i_bs_init_heap_bin_IId): {
	 BsOp1 = Arg(0);
	 BsOp2 = 0;
     }
     /* Fall through */
     do_heap_bin_alloc:
	 {
	     ErlHeapBin* hb;
	     Uint bin_need;

	     bin_need = heap_bin_size(BsOp1);
	     erts_bin_offset = 0;
	     erts_writable_bin = 0;
	     TestHeap(bin_need+BsOp2+ERL_SUB_BIN_SIZE, Arg(1));
	     hb = (ErlHeapBin *) HTOP;
	     HTOP += bin_need;
	     hb->thing_word = header_heap_bin(BsOp1);
	     hb->size = BsOp1;
	     erts_current_bin = (byte *) hb->data;
	     BsOp1 = make_binary(hb);
	     StoreBifResult(2, BsOp1);
	 }
 }

 OpCase(bs_add_jssId): {
     Eterm Op1, Op2;
     Uint Unit = Arg(3);

     GetArg2(1, Op1, Op2);
     if (is_both_small(Op1, Op2)) {
	 Sint Arg1 = signed_val(Op1);
	 Sint Arg2 = signed_val(Op2);

	 if (Arg1 >= 0 && Arg2 >= 0) {
	     BsSafeMul(Arg2, Unit, goto system_limit, Op1);
	     Op1 += Arg1;

	 store_bs_add_result:
	     if (Op1 <= MAX_SMALL) {
		 Op1 = make_small(Op1);
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
		 Op1 = erts_make_integer(Op1, c_p);
		 HTOP = HEAP_TOP(c_p);
	     }
	     StoreBifResult(4, Op1);
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

	 if (!term_to_Uint(Op1, &a)) {
	     if (a == BADARG) {
		 goto badarg;
	     }
	     if (!term_to_Uint(Op2, &b)) {
		 c_p->freason = b;
		 goto lb_Cl_error;
	     }
	     goto system_limit;
	 } else if (!term_to_Uint(Op2, &b)) {
	     c_p->freason = b;
	     goto lb_Cl_error;
	 }

	 /*
	  * The arguments are now correct and stored in a and b.
	  */
	 
	 BsSafeMul(b, Unit, goto system_limit, c);
	 Op1 = a + c;
	 if (Op1 < a) {
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
  * x(SCRATCH_X_REG);
  * Operands: Fail ExtraHeap Live Unit Size Dst
  */

 OpCase(i_bs_append_jIIIsd): {
     Uint live = Arg(2);
     Uint res;
     Eterm Size;

     GetArg1(4, Size);
     HEAVY_SWAPOUT;
     reg[live] = x(SCRATCH_X_REG);
     res = erts_bs_append(c_p, reg, live, Size, Arg(1), Arg(3));
     HEAVY_SWAPIN;
     if (is_non_value(res)) {
	 /* c_p->freason is already set (may be either BADARG or SYSTEM_LIMIT). */
	 goto lb_Cl_error;
     }
     StoreBifResult(5, res);
 }

 /*
  * Operands: Fail Size Src Unit Dst
  */
 OpCase(i_bs_private_append_jIssd): {
     Eterm res;
     Eterm Size, Src;

     GetArg2(2, Size, Src);
     res = erts_bs_private_append(c_p, Src, Size, Arg(1));
     if (is_non_value(res)) {
	 /* c_p->freason is already set (may be either BADARG or SYSTEM_LIMIT). */
	 goto lb_Cl_error;
     }
     StoreBifResult(4, res);
 }

 OpCase(bs_init_writable): {
     HEAVY_SWAPOUT;
     r(0) = erts_bs_init_writable(c_p, r(0));
     HEAVY_SWAPIN;
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

 OpCase(bs_put_utf16_jIs): {
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
  */
 OpCase(i_bs_validate_unicode_retract_jss): {
	Eterm i; 		/* Integer to validate */

	/*
	 * There is no need to untag the integer, but it IS necessary
	 * to make sure it is small (a bignum pointer could fall in
	 * the valid range).
	 */

	GetArg1(1, i);
	if (is_not_small(i) || i > make_small(0x10FFFFUL) ||
	    (make_small(0xD800UL) <= i && i <= make_small(0xDFFFUL))) {
	    Eterm ms;		/* Match context */
	    ErlBinMatchBuffer* mb;

	    GetArg1(2, ms);
	    mb = ms_matchbuffer(ms);
	    mb->offset -= 32;
	    goto badarg;
	}
	Next(3);
    }

 /*
  * Matching of binaries.
  */

 {
     Eterm header;
     BeamInstr *next;
     Uint slots;
     Eterm context;

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

 OpCase(i_bs_get_integer_8_xfd): {
	 ErlBinMatchBuffer *_mb;
	 Eterm _result;
	 bs_get_integer8_context = xb(Arg(0));
	 I++;
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

 OpCase(i_bs_get_integer_16_xfd):
     bs_get_integer_16_context = xb(Arg(0));
     I++;

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

 OpCase(i_bs_get_integer_32_xfId):
     bs_get_integer_32_context = xb(Arg(0));
     I++;

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
#if !defined(ARCH_64)
	 if (IS_USMALL(0, _integer)) {
#endif
	     _result = make_small(_integer);
#if !defined(ARCH_64)
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

 {
     Eterm Ms, Sz;

     /* Operands: x(Reg) Size Live Fail Flags Dst */
 OpCase(i_bs_get_integer_imm_xIIfId): {
	 Uint wordsneeded;
	 Ms = xb(Arg(0));
	 Sz = Arg(1);
	 wordsneeded = 1+WSIZE(NBYTES(Sz));
	 TestHeapPreserve(wordsneeded, Arg(2), Ms);
	 I += 3;
	 /* Operands: Fail Flags Dst */
	 goto do_bs_get_integer_imm;
     }

     /* Operands: x(Reg) Size Fail Flags Dst */
 OpCase(i_bs_get_integer_small_imm_xIfId): {
	 Ms = xb(Arg(0));
	 Sz = Arg(1);
	 I += 2;
	 /* Operands: Fail Flags Dst */
	 goto do_bs_get_integer_imm;
     }
 
     /*
      * Ms = match context
      * Sz = size of field
      * Operands: Fail Flags Dst
      */
 do_bs_get_integer_imm: {
	 ErlBinMatchBuffer* mb;
	 Eterm result;

	 mb = ms_matchbuffer(Ms);
	 LIGHT_SWAPOUT;
	 result = erts_bs_get_integer_2(c_p, Sz, Arg(1), mb);
	 LIGHT_SWAPIN;
	 HEAP_SPACE_VERIFIED(0);
	 if (is_non_value(result)) {
	     ClauseFail();
	 }
	 StoreBifResult(2, result);
     }
 }

 /*
  * Operands: Fail Live FlagsAndUnit Ms Sz Dst
  */
 OpCase(i_bs_get_integer_fIIssd): {
     Uint flags;
     Uint size;
     Eterm Ms;
     Eterm Sz;
     ErlBinMatchBuffer* mb;
     Eterm result;

     flags = Arg(2);
     GetArg2(3, Ms, Sz);
     BsGetFieldSize(Sz, (flags >> 3), ClauseFail(), size);
     if (size >= SMALL_BITS) {
	 Uint wordsneeded;
	 /* Check bits size before potential gc.
	  * We do not want a gc and then realize we don't need
	  * the allocated space (i.e. if the op fails).
	  *
	  * Remember to re-acquire the matchbuffer after gc.
	  */

	 mb = ms_matchbuffer(Ms);
	 if (mb->size - mb->offset < size) {
	     ClauseFail();
	 }
	 wordsneeded = 1+WSIZE(NBYTES((Uint) size));
	 TestHeapPreserve(wordsneeded, Arg(1), Ms);
     }
     mb = ms_matchbuffer(Ms);
     LIGHT_SWAPOUT;
     result = erts_bs_get_integer_2(c_p, size, flags, mb);
     LIGHT_SWAPIN;
     HEAP_SPACE_VERIFIED(0);
     if (is_non_value(result)) {
	 ClauseFail();
     }
     StoreBifResult(5, result);
 }

 {
     Eterm get_utf8_context;

     /* Operands: MatchContext Fail Dst */
 OpCase(i_bs_get_utf8_xfd): {
	 get_utf8_context = xb(Arg(0));
	 I++;
     }

     /*
      * get_utf8_context = match_context
      * Operands: Fail Dst
      */

     {
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
     OpCase(i_bs_get_utf16_xfId): {
	 get_utf16_context = xb(Arg(0));
	 I++;
     }

     /*
      * get_utf16_context = match_context
      * Operands: Fail Flags Dst
      */
     {
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

     OpCase(bs_context_to_binary_x):
	 context_to_binary_context = xb(Arg(0));
	 I--;

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

     OpCase(i_bs_get_binary_all_reuse_xfI): {
	 context_to_binary_context = xb(Arg(0));
	 I++;
     }

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

     OpCase(i_bs_match_string_xfII): {
	 match_string_context = xb(Arg(0));
	 I++;
     }

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

 OpCase(i_bs_save2_xI): {
     BeamInstr *next;
     ErlBinMatchState *_ms;
     PreFetch(2, next);
     _ms = (ErlBinMatchState*) boxed_val((Eterm) xb(Arg(0)));
     _ms->save_offset[Arg(1)] = _ms->mb.offset;
     NextPF(2, next);
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
     ErtsCodeMFA* mfa = (ErtsCodeMFA *)(E[0]);
     
     SWAPOUT;		/* Needed for shared heap */
     ERTS_SMP_UNREQ_PROC_MAIN_LOCK(c_p);
     erts_trace_return(c_p, mfa, r(0), ERTS_TRACER_FROM_ETERM(E+1)/* tracer */);
     ERTS_SMP_REQ_PROC_MAIN_LOCK(c_p);
     SWAPIN;
     c_p->cp = NULL;
     SET_I((BeamInstr *) cp_val(E[2]));
     E += 3;
     Goto(*I);
 }

 OpCase(i_generic_breakpoint): {
     BeamInstr real_I;
     HEAVY_SWAPOUT;
     real_I = erts_generic_breakpoint(c_p, erts_code_to_codeinfo(I), reg);
     HEAVY_SWAPIN;
     ASSERT(VALID_INSTR(real_I));
     Goto(real_I);
 }

 OpCase(i_return_time_trace): {
     BeamInstr *pc = (BeamInstr *) (UWord) E[0];
     SWAPOUT;
     erts_trace_time_return(c_p, erts_code_to_codeinfo(pc));
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
     targ1 = REG_TARGET(Arg(0));
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

     targ1 = REG_TARGET(Arg(0));
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
	 erts_exit(ERTS_ERROR_EXIT, "fclearerror/i_fcheckerror without fpe signals (beam_emu)");
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
#define HIPE_MODE_SWITCH(Cmd)			\
     SWAPOUT;					\
     ERTS_DBG_CHK_REDS(c_p, FCALLS);		\
     c_p->fcalls = FCALLS;			\
     c_p->def_arg_reg[4] = -neg_o_reds;		\
     c_p = hipe_mode_switch(c_p, Cmd, reg);     \
     goto L_post_hipe_mode_switch

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
         ErtsCodeInfo *ci = erts_code_to_codeinfo(I);
	 ASSERT(ci->op == (Uint) OpCode(i_func_info_IaaI));
	 c_p->hipe.u.ncallee = ci->u.ncallee;
	 ++hipe_trap_count;
	 HIPE_MODE_SWITCH(HIPE_MODE_SWITCH_CMD_CALL | (ci->mfa.arity << 8));
     }
     OpCase(hipe_trap_call_closure): {
       ErtsCodeInfo *ci = erts_code_to_codeinfo(I);
       ASSERT(ci->op == (Uint) OpCode(i_func_info_IaaI));
       c_p->hipe.u.ncallee = ci->u.ncallee;
       ++hipe_trap_count;
       HIPE_MODE_SWITCH(HIPE_MODE_SWITCH_CMD_CALL_CLOSURE | (ci->mfa.arity << 8));
     }
     OpCase(hipe_trap_return): {
	 HIPE_MODE_SWITCH(HIPE_MODE_SWITCH_CMD_RETURN);
     }
     OpCase(hipe_trap_throw): {
	 HIPE_MODE_SWITCH(HIPE_MODE_SWITCH_CMD_THROW);
     }
     OpCase(hipe_trap_resume): {
	 HIPE_MODE_SWITCH(HIPE_MODE_SWITCH_CMD_RESUME);
     }
#undef HIPE_MODE_SWITCH

 L_post_hipe_mode_switch:
#ifdef DEBUG
     pid = c_p->common.id; /* may have switched process... */
#endif
     reg = erts_proc_sched_data(c_p)->x_reg_array;
     freg = erts_proc_sched_data(c_p)->f_reg_array;
     ERL_BITS_RELOAD_STATEP(c_p);
     /* XXX: this abuse of def_arg_reg[] is horrid! */
     neg_o_reds = -c_p->def_arg_reg[4];
     FCALLS = c_p->fcalls;
     SWAPIN;
     ERTS_DBG_CHK_REDS(c_p, FCALLS);
     switch( c_p->def_arg_reg[3] ) {
       case HIPE_MODE_SWITCH_RES_RETURN:
	 ASSERT(is_value(reg[0]));
	 SET_I(c_p->cp);
	 c_p->cp = 0;
	 Goto(*I);
       case HIPE_MODE_SWITCH_RES_CALL_EXPORTED:
	 c_p->i = c_p->hipe.u.callee_exp->addressv[erts_active_code_ix()];
	 /*fall through*/
       case HIPE_MODE_SWITCH_RES_CALL_BEAM:
	 SET_I(c_p->i);
	 Dispatch();
       case HIPE_MODE_SWITCH_RES_CALL_CLOSURE:
	 /* This can be used to call any function value, but currently it's
	    only used to call closures referring to unloaded modules. */
	 {
	     BeamInstr *next;

	     next = call_fun(c_p, c_p->arity - 1, reg, THE_NON_VALUE);
	     HEAVY_SWAPIN;
	     if (next != NULL) {
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
	 erts_exit(ERTS_ERROR_EXIT, "hipe_mode_switch: result %u\n", c_p->def_arg_reg[3]);
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
     ErtsCodeInfo *ci = erts_code_to_codeinfo(I);
     struct hipe_call_count *hcc = ci->u.hcc;
     ASSERT(ci->op == (Uint) OpCode(i_func_info_IaaI));
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
     HEAVY_SWAPOUT;
     if (erts_hibernate(c_p, r(0), x(1), x(2), reg)) {
	 FCALLS = c_p->fcalls;
	 c_p->flags &= ~F_HIBERNATE_SCHED;
	 goto do_schedule;
     } else {
	 HEAVY_SWAPIN;
	 I = handle_error(c_p, I, reg, &bif_export[BIF_hibernate_3]->info.mfa);
	 goto post_error_handling;
     }
 }

 /* This is optimised as an instruction because
    it has to be very very fast */
 OpCase(i_perf_counter): {
    BeamInstr* next;
    ErtsSysPerfCounter ts;
    PreFetch(0, next);

    ts = erts_sys_perf_counter();

    if (IS_SSMALL(ts)) {
        r(0) = make_small((Sint)ts);
    } else {
        TestHeap(ERTS_SINT64_HEAP_SIZE(ts),0);
        r(0) = make_big(HTOP);
#if defined(ARCH_32)
        if (ts >= (((Uint64) 1) << 32)) {
            *HTOP = make_pos_bignum_header(2);
            BIG_DIGIT(HTOP, 0) = (Uint) (ts & ((Uint) 0xffffffff));
            BIG_DIGIT(HTOP, 1) = (Uint) ((ts >> 32) & ((Uint) 0xffffffff));
            HTOP += 3;
        }
        else
#endif
        {
                *HTOP = make_pos_bignum_header(1);
                BIG_DIGIT(HTOP, 0) = (Uint) ts;
                HTOP += 2;
        }
    }
    NextPF(0, next);
 }

 OpCase(i_debug_breakpoint): {
     HEAVY_SWAPOUT;
     I = call_error_handler(c_p, erts_code_to_codemfa(I), reg, am_breakpoint);
     HEAVY_SWAPIN;
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
    erts_exit(ERTS_ERROR_EXIT, "meta op\n");

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
     em_call_bif_e = OpCode(call_bif_e);

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
	 ep->beam[0] = (BeamInstr) OpCode(apply_bif);
	 ep->beam[1] = (BeamInstr) bif_table[i].f;
	 /* XXX: set func info for bifs */
	 ep->info.op = (BeamInstr) BeamOp(op_i_func_info_IaaI);
     }

     return;
 }
#ifdef NO_JUMP_TABLE
 default:
    erts_exit(ERTS_ERROR_EXIT, "unexpected op code %d\n",Go);
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

/*
 * erts_dirty_process_main() is what dirty schedulers execute. Since they handle
 * only NIF calls they do not need to be able to execute all BEAM
 * instructions.
 */
void erts_dirty_process_main(ErtsSchedulerData *esdp)
{
#ifdef ERTS_DIRTY_SCHEDULERS
    Process* c_p = NULL;
    ErtsMonotonicTime start_time;
#ifdef DEBUG
    ERTS_DECLARE_DUMMY(Eterm pid);
#endif

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

    ERTS_MSACC_DECLARE_CACHE_X() /* a cached value of the tsd pointer for msacc */

    /*
     * start_time always positive for dirty CPU schedulers,
     * and negative for dirty I/O schedulers.
     */

    if (ERTS_SCHEDULER_IS_DIRTY_CPU(esdp)) {
	start_time = erts_get_monotonic_time(NULL);
	ASSERT(start_time >= 0);
    }
    else {
	start_time = ERTS_SINT64_MIN;
	ASSERT(start_time < 0);
    }

    goto do_dirty_schedule;

 context_switch:
    c_p->current = erts_code_to_codemfa(I);	/* Pointer to Mod, Func, Arity */
    c_p->arity = c_p->current->arity;

    {
	int reds_used;
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
	 * Save the argument registers and everything else.
	 */

	argp = c_p->arg_reg;
	for (i = c_p->arity - 1; i >= 0; i--) {
	    argp[i] = reg[i];
	}
	SWAPOUT;
	c_p->i = I;

    do_dirty_schedule:

	if (start_time < 0) {
	    /*
	     * Dirty I/O scheduler:
	     *   One reduction consumed regardless of
	     *   time spent in the dirty NIF.
	     */
	    reds_used = esdp->virtual_reds + 1;
	}
	else {
	    /*
	     * Dirty CPU scheduler:
	     *   Reductions based on time consumed by
	     *   the dirty NIF.
	     */
	    Sint64 treds;
	    treds = erts_time2reds(start_time,
				   erts_get_monotonic_time(esdp));
	    treds += esdp->virtual_reds;
	    reds_used = treds > INT_MAX ? INT_MAX : (int) treds;
	}

	PROCESS_MAIN_CHK_LOCKS(c_p);
	ERTS_SMP_UNREQ_PROC_MAIN_LOCK(c_p);
	ERTS_VERIFY_UNUSED_TEMP_ALLOC(c_p);
	c_p = erts_schedule(esdp, c_p, reds_used);

	if (start_time >= 0) {
	    start_time = erts_get_monotonic_time(esdp);
	    ASSERT(start_time >= 0);
	}
    }

    ERTS_VERIFY_UNUSED_TEMP_ALLOC(c_p);
#ifdef DEBUG
    pid = c_p->common.id; /* Save for debugging purposes */
#endif
    ERTS_SMP_REQ_PROC_MAIN_LOCK(c_p);
    PROCESS_MAIN_CHK_LOCKS(c_p);

    ASSERT(!(c_p->flags & F_HIPE_MODE));
    ERTS_MSACC_UPDATE_CACHE_X();

    /*
     * Set fcalls even though we ignore it, so we don't
     * confuse code accessing it...
     */
    if (ERTS_PROC_GET_SAVED_CALLS_BUF(c_p))
	c_p->fcalls = 0;
    else
	c_p->fcalls = CONTEXT_REDS;

    if (erts_smp_atomic32_read_nob(&c_p->state) & ERTS_PSFLG_DIRTY_RUNNING_SYS) {
	erts_execute_dirty_system_task(c_p);
	goto do_dirty_schedule;
    }
    else {
	ErtsCodeMFA *codemfa;
	Eterm* argp;
	int i, exiting;

	reg = esdp->x_reg_array;

	argp = c_p->arg_reg;
	for (i = c_p->arity - 1; i >= 0; i--) {
	    reg[i] = argp[i];
	    CHECK_TERM(reg[i]);
	}

	/*
	 * We put the original reduction count in the process structure, to reduce
	 * the code size (referencing a field in a struct through a pointer stored
	 * in a register gives smaller code than referencing a global variable).
	 */

	I = c_p->i;

	SWAPIN;

#ifdef USE_VM_PROBES
        if (DTRACE_ENABLED(process_scheduled)) {
            DTRACE_CHARBUF(process_buf, DTRACE_TERM_BUF_SIZE);
            DTRACE_CHARBUF(fun_buf, DTRACE_TERM_BUF_SIZE);
            dtrace_proc_str(c_p, process_buf);

            if (ERTS_PROC_IS_EXITING(c_p)) {
                strcpy(fun_buf, "<exiting>");
            } else {
                ErtsCodeMFA *cmfa = find_function_from_pc(c_p->i);
                if (cmfa) {
		    dtrace_fun_decode(c_p, cmfa, NULL, fun_buf);
                } else {
                    erts_snprintf(fun_buf, sizeof(DTRACE_CHARBUF_NAME(fun_buf)),
                                  "<unknown/%p>", *I);
                }
            }

            DTRACE2(process_scheduled, process_buf, fun_buf);
        }
#endif

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
	 *
	 * This layout is determined by the NifExport struct
	 */

	ERTS_MSACC_SET_STATE_CACHED_M_X(ERTS_MSACC_STATE_NIF);

	codemfa = erts_code_to_codemfa(I);

	DTRACE_NIF_ENTRY(c_p, codemfa);
	c_p->current = codemfa;
	SWAPOUT;
	PROCESS_MAIN_CHK_LOCKS(c_p);
	ERTS_SMP_UNREQ_PROC_MAIN_LOCK(c_p);

	ASSERT(!ERTS_PROC_IS_EXITING(c_p));
	if (em_apply_bif == (BeamInstr *) *I) {
	    exiting = erts_call_dirty_bif(esdp, c_p, I, reg);
	}
	else {
	    ASSERT(em_call_nif == (BeamInstr *) *I);
	    exiting = erts_call_dirty_nif(esdp, c_p, I, reg);
	}

	ASSERT(!(c_p->flags & F_HIBERNATE_SCHED));

	PROCESS_MAIN_CHK_LOCKS(c_p);
	ERTS_SMP_REQ_PROC_MAIN_LOCK(c_p);
	ERTS_VERIFY_UNUSED_TEMP_ALLOC(c_p);
	ERTS_MSACC_SET_STATE_CACHED_M_X(ERTS_MSACC_STATE_EMULATOR);
	if (exiting)
	    goto do_dirty_schedule;
	ASSERT(!ERTS_PROC_IS_EXITING(c_p));

	DTRACE_NIF_RETURN(c_p, codemfa);
	ERTS_HOLE_CHECK(c_p);
	SWAPIN;
	I = c_p->i;
	goto context_switch;
    }
#endif /* ERTS_DIRTY_SCHEDULERS */
}

static ErtsCodeMFA *
gcbif2mfa(void* gcf)
{
    int i;
    for (i = 0; erts_gc_bifs[i].bif; i++) {
	if (erts_gc_bifs[i].gc_bif == gcf)
	    return &bif_export[erts_gc_bifs[i].exp_ix]->info.mfa;
    }
    erts_exit(ERTS_ERROR_EXIT, "bad gc bif");
    return NULL;
}

static ErtsCodeMFA *
ubif2mfa(void* uf)
{
    int i;
    for (i = 0; erts_u_bifs[i].bif; i++) {
	if (erts_u_bifs[i].bif == uf)
	    return &bif_export[erts_u_bifs[i].exp_ix]->info.mfa;
    }
    erts_exit(ERTS_ERROR_EXIT, "bad u bif");
    return NULL;
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
handle_error(Process* c_p, BeamInstr* pc, Eterm* reg, ErtsCodeMFA *bif_mfa)
{
    Eterm* hp;
    Eterm Value = c_p->fvalue;
    Eterm Args = am_true;

    ASSERT(c_p->freason != TRAP); /* Should have been handled earlier. */

    if (c_p->freason & EXF_RESTORE_NIF)
	erts_nif_export_restore_error(c_p, &pc, reg, &bif_mfa);

#ifdef DEBUG
    if (bif_mfa) {
	/* Verify that bif_mfa does not point into our nif export */
	NifExport *nep = ERTS_PROC_GET_NIF_TRAP_EXPORT(c_p);
	ASSERT(!nep || !ErtsInArea(bif_mfa, (char *)nep, sizeof(NifExport)));
    }
#endif

    c_p->i = pc;    /* In case we call erts_exit(). */

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
        save_stacktrace(c_p, pc, reg, bif_mfa, Args);
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
	if (c_p->catches > 0) erts_exit(ERTS_ERROR_EXIT, "Catch not found");
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
            ErtsCodeMFA *mfa = (ErtsCodeMFA*)cp_val(ptr[0]);
	    erts_trace_exception(c_p, mfa,
				 reg[1], reg[2],
                                 ERTS_TRACER_FROM_ETERM(ptr+1));
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
                    ErtsCodeMFA *mfa = (ErtsCodeMFA*)cp_val(ptr[0]);
		    erts_trace_exception(c_p, mfa,
					 reg[1], reg[2],
                                         ERTS_TRACER_FROM_ETERM(ptr+1));
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
save_stacktrace(Process* c_p, BeamInstr* pc, Eterm* reg,
		ErtsCodeMFA *bif_mfa, Eterm args) {
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
     * If the failure was in a BIF other than 'error/1', 'error/2',
     * 'exit/1' or 'throw/1', save BIF-MFA and save the argument
     * registers by consing up an arglist.
     */
    if (bif_mfa) {
	if (bif_mfa->module == am_erlang) {
	    switch (bif_mfa->function) {
	    case am_error:
		if (bif_mfa->arity == 1 || bif_mfa->arity == 2)
		    goto non_bif_stacktrace;
		break;
	    case am_exit:
		if (bif_mfa->arity == 1)
		    goto non_bif_stacktrace;
		break;
	    case am_throw:
		if (bif_mfa->arity == 1)
		    goto non_bif_stacktrace;
		break;
	    default:
		break;
	    }
	}
	s->current = bif_mfa;
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
	args = make_arglist(c_p, reg, bif_mfa->arity); /* Overwrite CAR(c_p->ftrace) */
    } else {

    non_bif_stacktrace:

	s->current = c_p->current;
        /* 
	 * For a function_clause error, the arguments are in the beam
	 * registers, c_p->cp is valid, and c_p->current is set.
	 */
	if ( (GET_EXC_INDEX(s->freason)) ==
	     (GET_EXC_INDEX(EXC_FUNCTION_CLAUSE)) ) {
	    int a;
	    ASSERT(s->current);
	    a = s->current->arity;
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
	    int trace_cp;
	    if (cpp == beam_exception_trace || cpp == beam_return_trace) {
		/* Skip return_trace parameters */
		ptr += 2;
		trace_cp = 1;
	    } else if (cpp == beam_return_to_trace) {
		/* Skip return_to_trace parameters */
		ptr += 1;
		trace_cp = 1;
	    }
	    else {
		trace_cp = 0;
	    }
	    if (trace_cp && s->pc == cpp) {
		/*
		 * If process 'cp' points to a return/exception trace
		 * instruction and 'cp' has been saved as 'pc' in
		 * stacktrace, we need to update 'pc' in stacktrace
		 * with the actual 'cp' located on the top of the
		 * stack; otherwise, we will lose the top stackframe
		 * when building the stack trace.
		 */
		ASSERT(is_CP(p->stop[0]));
		s->pc = cp_val(p->stop[0]);
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
	erts_lookup_function_info(&fi, erts_codemfa_to_code(s->current), 1);
    } else {
	erts_set_current_function(&fi, s->current);
    }

    depth = s->depth;
    /*
     * If fi.current is still NULL, and we have no
     * stack at all, default to the initial function
     * (e.g. spawn_link(erlang, abs, [1])).
     */
    if (fi.mfa == NULL) {
	if (depth <= 0)
            erts_set_current_function(&fi, &c_p->u.initial);
	args = am_true; /* Just in case */
    } else {
	args = get_args_from_exc(exc);
    }

    /*
     * Look up all saved continuation pointers and calculate
     * needed heap space.
     */
    stk = stkp = (FunctionInfo *) erts_alloc(ERTS_ALC_T_TMP,
				      depth*sizeof(FunctionInfo));
    heap_size = fi.mfa ? fi.needed + 2 : 0;
    for (i = 0; i < depth; i++) {
	erts_lookup_function_info(stkp, s->trace[i], 1);
	if (stkp->mfa) {
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
    if (fi.mfa) {
	hp = erts_build_mfa_item(&fi, hp, args, &mfa);
	res = CONS(hp, mfa, res);
    }

    erts_free(ERTS_ALC_T_TMP, (void *) stk);
    return res;
}

static BeamInstr*
call_error_handler(Process* p, ErtsCodeMFA* mfa, Eterm* reg, Eterm func)
{
    Eterm* hp;
    Export* ep;
    int arity;
    Eterm args;
    Uint sz;
    int i;

    DBG_TRACE_MFA_P(mfa, "call_error_handler");
    /*
     * Search for the error_handler module.
     */
    ep = erts_find_function(erts_proc_get_error_handler(p), func, 3,
			    erts_active_code_ix());
    if (ep == NULL) {		/* No error handler */
	p->current = mfa;
	p->freason = EXC_UNDEF;
	return 0;
    }

    /*
     * Create a list with all arguments in the x registers.
     */

    arity = mfa->arity;
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
    reg[0] = mfa->module;
    reg[1] = mfa->function;
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

static ERTS_INLINE void
apply_bif_error_adjustment(Process *p, Export *ep,
			   Eterm *reg, Uint arity,
			   BeamInstr *I, Uint stack_offset)
{
    /*
     * I is only set when the apply is a tail call, i.e.,
     * from the instructions i_apply_only, i_apply_last_P,
     * and apply_last_IP.
     */
    if (I
	&& ep->beam[0] == (BeamInstr) em_apply_bif
	&& (ep == bif_export[BIF_error_1]
	    || ep == bif_export[BIF_error_2]
	    || ep == bif_export[BIF_exit_1]
	    || ep == bif_export[BIF_throw_1])) {
	/*
	 * We are about to tail apply one of the BIFs
	 * erlang:error/1, erlang:error/2, erlang:exit/1,
	 * or erlang:throw/1. Error handling of these BIFs is
	 * special!
	 *
	 * We need 'p->cp' to point into the calling
	 * function when handling the error after the BIF has
	 * been applied. This in order to get the topmost
	 * stackframe correct. Without the following adjustment,
	 * 'p->cp' will point into the function that called
	 * current function when handling the error. We add a
	 * dummy stackframe in order to achieve this.
	 *
	 * Note that these BIFs unconditionally will cause
	 * an exception to be raised. That is, our modifications
	 * of 'p->cp' as well as the stack will be corrected by
	 * the error handling code.
	 *
	 * If we find an exception/return-to trace continuation
	 * pointer as the topmost continuation pointer, we do not
	 * need to do anything since the information already will
	 * be available for generation of the stacktrace.
	 */
	int apply_only = stack_offset == 0;
	BeamInstr *cpp;

	if (apply_only) {
	    ASSERT(p->cp != NULL);
	    cpp = p->cp;
	}
	else {
	    ASSERT(is_CP(p->stop[0]));
	    cpp = cp_val(p->stop[0]);
	}

	if (cpp != beam_exception_trace
	    && cpp != beam_return_trace
	    && cpp != beam_return_to_trace) {
	    Uint need = stack_offset /* bytes */ / sizeof(Eterm);
	    if (need == 0)
		need = 1; /* i_apply_only */
	    if (p->stop - p->htop < need)
		erts_garbage_collect(p, (int) need, reg, arity+1);
	    p->stop -= need;

	    if (apply_only) {
		/*
		 * Called from the i_apply_only instruction.
		 *
		 * 'p->cp' contains continuation pointer pointing
		 * into the function that called current function.
		 * We push that continuation pointer onto the stack,
		 * and set 'p->cp' to point into current function.
		 */

		p->stop[0] = make_cp(p->cp);
		p->cp = I;
	    }
	    else {
		/*
		 * Called from an i_apply_last_p, or apply_last_IP,
		 * instruction.
		 *
		 * Calling instruction will after we return read
		 * a continuation pointer from the stack and write
		 * it to 'p->cp', and then remove the topmost
		 * stackframe of size 'stack_offset'.
		 *
		 * We have sized the dummy-stackframe so that it
		 * will be removed by the instruction we currently
		 * are executing, and leave the stackframe that
		 * normally would have been removed intact.
		 *
		 */
		p->stop[0] = make_cp(I);
	    }
	}
    }
}

static BeamInstr*
apply(
Process* p, Eterm module, Eterm function, Eterm args, Eterm* reg,
BeamInstr *I, Uint stack_offset)
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

    while (1) {
	Eterm m, f, a;
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

	if (module != am_erlang || function != am_apply)
	    break;

	/* Adjust for multiple apply of apply/3... */

	a = args;
	if (is_list(a)) {
	    Eterm *consp = list_val(a);
	    m = CAR(consp);
	    a = CDR(consp);
	    if (is_list(a)) {
		consp = list_val(a);
		f = CAR(consp);
		a = CDR(consp);
		if (is_list(a)) {
		    consp = list_val(a);
		    a = CAR(consp);
		    if (is_nil(CDR(consp))) {
			/* erlang:apply/3 */
			module = m;
			function = f;
			args = a;
			if (is_not_atom(f))
			    goto error;
			continue;
		    }
		}
	    }
	}
	break; /* != erlang:apply/3 */
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
    apply_bif_error_adjustment(p, ep, reg, arity, I, stack_offset);
    DTRACE_GLOBAL_CALL_FROM_EXPORT(p, ep);
    return ep->addressv[erts_active_code_ix()];
}

static BeamInstr*
fixed_apply(Process* p, Eterm* reg, Uint arity,
	    BeamInstr *I, Uint stack_offset)
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

    /* Handle apply of apply/3... */
    if (module == am_erlang && function == am_apply && arity == 3)
	return apply(p, reg[0], reg[1], reg[2], reg, I, stack_offset);
    
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
    apply_bif_error_adjustment(p, ep, reg, arity, I, stack_offset);
    DTRACE_GLOBAL_CALL_FROM_EXPORT(p, ep);
    return ep->addressv[erts_active_code_ix()];
}

int
erts_hibernate(Process* c_p, Eterm module, Eterm function, Eterm args, Eterm* reg)
{
    int arity;
    Eterm tmp;

#ifndef ERTS_SMP
    if (ERTS_PROC_IS_EXITING(c_p)) {
	/*
	 * I non smp case:
	 *
	 * Currently executing process might be sent an exit
	 * signal if it is traced by a port that it also is
	 * linked to, and the port terminates during the
	 * trace. In this case we do *not* want to clear
	 * the active flag, which will make the process hang
	 * in limbo forever. Get out of here and terminate
	 * the process...
	 */
	return -1;
    }
#endif

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
        ErtsCodeMFA cmfa = { module, function, arity};
        DTRACE_CHARBUF(process_name, DTRACE_TERM_BUF_SIZE);
        DTRACE_CHARBUF(mfa_buf, DTRACE_TERM_BUF_SIZE);
        dtrace_fun_decode(c_p, &cmfa, process_name, mfa_buf);
        DTRACE2(process_hibernate, process_name, mfa_buf);
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
#ifndef ERTS_SMP
	if (ERTS_PROC_IS_EXITING(c_p)) {
	    /*
	     * See comment in the beginning of the function...
	     *
	     * This second test is needed since gc might be traced.
	     */
	    return -1;
	}
#else /* ERTS_SMP */
        ERTS_SMP_MSGQ_MV_INQ2PRIVQ(c_p);
	if (!c_p->msg.len)
#endif
	    erts_smp_atomic32_read_band_relb(&c_p->state, ~ERTS_PSFLG_ACTIVE);
	ASSERT(!ERTS_PROC_IS_EXITING(c_p));
    }
    erts_smp_proc_unlock(c_p, ERTS_PROC_LOCK_MSGQ|ERTS_PROC_LOCK_STATUS);
    c_p->current = &bif_export[BIF_hibernate_3]->info.mfa;
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
	ErlFunEntry* fe = funp->fe;
	BeamInstr* code_ptr = fe->address;
	Eterm* var_ptr;
	unsigned num_free = funp->num_free;
        ErtsCodeMFA *mfa = erts_code_to_codemfa(code_ptr);
	int actual_arity = mfa->arity;

	if (actual_arity == arity+num_free) {
	    DTRACE_LOCAL_CALL(p, mfa);
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

		ERTS_SMP_READ_MEMORY_BARRIER;
		if (fe->pend_purge_address) {
		    /*
		     * The system is currently trying to purge the
		     * module containing this fun. Suspend the process
		     * and let it try again when the purge operation is
		     * done (may succeed or not).
		     */
		    ep = erts_suspend_process_on_pending_purge_lambda(p, fe);
		    ASSERT(ep);
		}
		else {
		    if ((modp = erts_get_module(module, code_ix)) != NULL
			&& modp->curr.code_hdr != NULL) {
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
		}
		reg[0] = module;
		reg[1] = fun;
		reg[2] = args;
		reg[3] = NIL;
		return ep->addressv[code_ix];
	    }
	}
    } else if (is_export_header(hdr)) {
	Export *ep;
	int actual_arity;

	ep = *((Export **) (export_val(fun) + 1));
	actual_arity = ep->info.mfa.arity;

	if (arity == actual_arity) {
	    DTRACE_GLOBAL_CALL(p, &ep->info.mfa);
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
    erts_smp_refc_inc(&fe->refc, 2);
    funp->thing_word = HEADER_FUN;
    funp->next = MSO(p).first;
    MSO(p).first = (struct erl_off_heap_header*) funp;
    funp->fe = fe;
    funp->num_free = num_free;
    funp->creator = p->common.id;
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

#define GET_TERM(term, dest)			\
do {						\
    Eterm src = (Eterm)(term);			\
    switch (loader_tag(src)) {			\
    case LOADER_X_REG:				\
        dest = x(loader_x_reg_index(src));	\
	break;					\
    case LOADER_Y_REG:				\
        dest = y(loader_y_reg_index(src));	\
	break;					\
    default:					\
	dest = src;				\
	break;					\
    }						\
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
        ErtsHeapFactory factory;
        erts_factory_proc_init(&factory, p);
        res = erts_hashmap_from_ks_and_vs(&factory,flatmap_get_keys(mp),
                                          flatmap_get_values(mp),n);
        erts_factory_close(&factory);
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

    if (Mod == am_erlang && Name == am_apply && arity == 3) {
	/*
	 * Special case. apply/3 is built-in (implemented in C),
	 * but implemented in a different way than all other
	 * BIFs.
	 */
	return 1;
    }

    e.info.mfa.module = Mod;
    e.info.mfa.function = Name;
    e.info.mfa.arity = arity;

    if ((ep = export_get(&e)) == NULL) {
	return 0;
    }
    return ep->addressv[erts_active_code_ix()] == ep->beam
	&& (ep->beam[0] == (BeamInstr) em_apply_bif);
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
	return current->fcalls + CONTEXT_REDS;
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
