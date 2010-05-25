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

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include <stddef.h> /* offsetof() */
#include "sys.h"
#include "erl_vm.h"
#include "global.h"
#include "erl_process.h"
#include "erl_nmgc.h"
#include "error.h"
#include "bif.h"
#include "big.h"
#include "beam_load.h"
#include "erl_binary.h"
#include "erl_bits.h"
#include "dist.h"
#include "beam_bp.h"
#include "beam_catches.h"
#ifdef HIPE
#include "hipe_mode_switch.h"
#include "hipe_bif1.h"
#endif

/* #define HARDDEBUG 1 */

#if defined(NO_JUMP_TABLE)
#  define OpCase(OpCode)    case op_##OpCode: lb_##OpCode
#  define CountCase(OpCode) case op_count_##OpCode
#  define OpCode(OpCode)    ((Uint*)op_##OpCode)
#  define Goto(Rel) {Go = (int)(Rel); goto emulator_loop;}
#  define LabelAddr(Addr) &&##Addr
#else
#  define OpCase(OpCode)    lb_##OpCode
#  define CountCase(OpCode) lb_count_##OpCode
#  define Goto(Rel) goto *(Rel)
#  define LabelAddr(Label) &&Label
#  define OpCode(OpCode)  (&&lb_##OpCode)
#endif

#ifdef ERTS_ENABLE_LOCK_CHECK
#  ifdef ERTS_SMP
#    define PROCESS_MAIN_CHK_LOCKS(P)					\
do {									\
    if ((P)) {								\
	erts_pix_lock_t *pix_lock__ = ERTS_PIX2PIXLOCK(internal_pid_index((P)->id));\
	erts_proc_lc_chk_only_proc_main((P));				\
	erts_pix_lock(pix_lock__);					\
	ASSERT(0 < (P)->lock.refc && (P)->lock.refc < erts_no_schedulers*5);\
	erts_pix_unlock(pix_lock__);					\
    }									\
    else								\
	erts_lc_check_exact(NULL, 0);					\
    ERTS_SMP_LC_ASSERT(!ERTS_LC_IS_BLOCKING);				\
} while (0)
#    define ERTS_SMP_REQ_PROC_MAIN_LOCK(P) \
        if ((P)) erts_proc_lc_require_lock((P), ERTS_PROC_LOCK_MAIN)
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
#define VALID_INSTR(IP) (0 <= (int)(IP) && ((int)(IP) < (NUMBER_OF_OPCODES*2+10)))
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

#define ClauseFail() goto lb_jump_f

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
BeamInstr* em_call_traced_function;


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

#ifndef ERTS_SMP /* Not supported with smp emulator */
extern int count_instructions;
#endif

#if defined(HYBRID)
#define SWAPIN             \
    g_htop = global_htop;  \
    g_hend = global_hend;  \
    HTOP = HEAP_TOP(c_p);  \
    E = c_p->stop

#define SWAPOUT            \
    global_htop = g_htop;  \
    global_hend = g_hend;  \
    HEAP_TOP(c_p) = HTOP;  \
    c_p->stop = E

#else
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

#endif

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

#if defined(HYBRID)
#  define POST_BIF_GC_SWAPIN_0(_p, _res)				\
     if (((_p)->mbuf) || (MSO(_p).overhead >= BIN_VHEAP_SZ(_p)) ) {	\
       _res = erts_gc_after_bif_call((_p), (_res), NULL, 0);		\
     }									\
     SWAPIN

#  define POST_BIF_GC_SWAPIN(_p, _res, _regs, _arity)			\
     if (((_p)->mbuf) || (MSO(_p).overhead >= BIN_VHEAP_SZ(_p)) ) {	\
       _regs[0] = r(0);							\
       _res = erts_gc_after_bif_call((_p), (_res), _regs, (_arity));	\
       r(0) = _regs[0];							\
     }									\
     SWAPIN
#else
#  define POST_BIF_GC_SWAPIN_0(_p, _res)				\
     ERTS_SMP_REQ_PROC_MAIN_LOCK((_p));					\
     PROCESS_MAIN_CHK_LOCKS((_p));					\
     if (((_p)->mbuf) || (MSO(_p).overhead >= BIN_VHEAP_SZ(_p)) ) {	\
       _res = erts_gc_after_bif_call((_p), (_res), NULL, 0);		\
       E = (_p)->stop;							\
     }									\
     HTOP = HEAP_TOP((_p))

#  define POST_BIF_GC_SWAPIN(_p, _res, _regs, _arity)			\
     ERTS_SMP_REQ_PROC_MAIN_LOCK((_p));					\
     PROCESS_MAIN_CHK_LOCKS((_p));					\
     if (((_p)->mbuf) || (MSO(_p).overhead >= BIN_VHEAP_SZ(_p)) ) {	\
       _regs[0] = r(0);							\
       _res = erts_gc_after_bif_call((_p), (_res), _regs, (_arity));	\
       r(0) = _regs[0];							\
       E = (_p)->stop;							\
     }									\
     HTOP = HEAP_TOP((_p))
#endif

#define db(N) (N)
#define tb(N) (N)
#define xb(N) (*(Eterm *) (((unsigned char *)reg) + (N)))
#define yb(N) (*(Eterm *) (((unsigned char *)E) + (N)))
#define fb(N) (*(double *) (((unsigned char *)&(freg[0].fd)) + (N)))
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
       PROCESS_MAIN_CHK_LOCKS(c_p);					\
       if (Live > 0) {							\
	   r(0) = reg[0];						\
       }								\
       Extra = reg[Live];						\
       SWAPIN;								\
    }									\
    HEAP_SPACE_VERIFIED(need);                                      \
  } while (0)

#ifdef HYBRID
#ifdef INCREMENTAL
#define TestGlobalHeap(Nh, Live, hp)                                    \
  do {                                                                  \
    unsigned need = (Nh);                                               \
    ASSERT(global_heap <= g_htop && g_htop <= global_hend);             \
    SWAPOUT;                                                            \
    reg[0] = r(0);                                                      \
    FCALLS -= need;                                                     \
    (hp) = IncAlloc(c_p,need,reg,(Live));                               \
    r(0) = reg[0];                                                      \
    SWAPIN;                                                             \
  } while (0)
#else
#define TestGlobalHeap(Nh, Live, hp)                                    \
  do {                                                                  \
    unsigned need = (Nh);                                               \
    ASSERT(global_heap <= g_htop && g_htop <= global_hend);             \
    if (g_hend - g_htop < need) {                                       \
       SWAPOUT;                                                         \
       reg[0] = r(0);                                                   \
       FCALLS -= erts_global_garbage_collect(c_p, need, reg, (Live));   \
       r(0) = reg[0];                                                   \
       SWAPIN;                                                          \
    }                                                                   \
    (hp) = global_htop;                                                 \
  } while (0)
#endif
#endif /* HYBRID */

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
        SET_I(((Export *) Arg(0))->address);			\
        dis_next = (Eterm *) *I;				\
        FCALLS--;						\
        CHECK_ARGS(I);						\
        Goto(dis_next);						\
     } else if (ERTS_PROC_GET_SAVED_CALLS_BUF(c_p)		\
		&& FCALLS > neg_o_reds) {			\
        goto save_calls1;					\
     } else {							\
        SET_I(((Export *) Arg(0))->address);			\
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

#define Self(R) R = c_p->id
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

#define Move2(src1, dst1, src2, dst2) dst1 = (src1); dst2 = (src2)

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

#define PutTuple(Arity, Src, Dest)		\
     ASSERT(is_arity_value(Arity));		\
     Dest = make_tuple(HTOP);			\
     HTOP[0] = (Arity);				\
     HTOP[1] = (Src);				\
     HTOP += 2

#define Put(Word) *HTOP++ = (Word)

#define EqualImmed(X, Y, Action) if (X != Y) { Action; }

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

#define IsFunction(X, Action)			\
  do {						\
     if ( !(is_any_fun(X)) ) {			\
          Action;				\
     }						\
  } while (0)

#define IsFunction2(F, A, Action)		\
  do {						\
     if (is_function_2(c_p, F, A) != am_true ) {\
          Action;				\
     }						\
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

static BifFunction translate_gc_bif(void* gcf);
static BeamInstr* handle_error(Process* c_p, BeamInstr* pc, Eterm* reg, BifFunction bf);
static BeamInstr* next_catch(Process* c_p, Eterm *reg);
static void terminate_proc(Process* c_p, Eterm Value);
static Eterm add_stacktrace(Process* c_p, Eterm Value, Eterm exc);
static void save_stacktrace(Process* c_p, BeamInstr* pc, Eterm* reg,
			     BifFunction bf, Eterm args);
static struct StackTrace * get_trace_from_exc(Eterm exc);
static Eterm make_arglist(Process* c_p, Eterm* reg, int a);
static Eterm call_error_handler(Process* p, BeamInstr* ip, Eterm* reg);
static Eterm call_breakpoint_handler(Process* p, BeamInstr* fi, Eterm* reg);
static BeamInstr* fixed_apply(Process* p, Eterm* reg, Uint arity);
static BeamInstr* apply(Process* p, Eterm module, Eterm function,
		     Eterm args, Eterm* reg);
static int hibernate(Process* c_p, Eterm module, Eterm function,
		     Eterm args, Eterm* reg);
static BeamInstr* call_fun(Process* p, int arity, Eterm* reg, Eterm args);
static BeamInstr* apply_fun(Process* p, Eterm fun, Eterm args, Eterm* reg);
static Eterm new_fun(Process* p, Eterm* reg, ErlFunEntry* fe, int num_free);

#if defined(VXWORKS)
static int init_done;
#endif

void
init_emulator(void)
{
#if defined(VXWORKS)
    init_done = 0;
#endif
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

/*
 * process_main() is called twice:
 * The first call performs some initialisation, including exporting
 * the instructions' C labels to the loader.
 * The second call starts execution of BEAM code. This call never returns.
 */
void process_main(void)
{
#if !defined(VXWORKS)
    static int init_done = 0;
#endif
    Process* c_p = NULL;
    int reds_used;
#ifdef DEBUG
    Eterm pid;
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


#ifdef HYBRID
     Eterm *g_htop;
     Eterm *g_hend;
#endif

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

#ifndef ERTS_SMP
#if !HALFWORD_HEAP
    static Eterm save_reg[ERTS_X_REGS_ALLOCATED];
    /* X registers -- not used directly, but
     * through 'reg', because using it directly
     * needs two instructions on a SPARC,
     * while using it through reg needs only
     * one.
     */
#endif
    /*
     * Floating point registers.
     */
    static FloatDef freg[MAX_REG];
#else
    /* X regisers and floating point registers are located in
     * scheduler specific data.
     */
    register FloatDef *freg;
#endif

    /*
     * For keeping the negative old value of 'reds' when call saving is active.
     */
    int neg_o_reds = 0;

    Eterm (*arith_func)(Process* p, Eterm* reg, Uint live);

#ifndef NO_JUMP_TABLE
    static void* opcodes[] = { DEFINE_OPCODES };
#ifdef ERTS_OPCODE_COUNTER_SUPPORT
    static void* counting_opcodes[] = { DEFINE_COUNTING_OPCODES };
#endif
#else
    int Go;
#endif

    Uint temp_bits; /* Temporary used by BsSkipBits2 & BsGetInteger2 */

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
	init_done = 1;
	goto init_emulator;
    }
#ifndef ERTS_SMP
#if !HALFWORD_HEAP
    reg = save_reg;	/* XXX: probably wastes a register on x86 */
#else
    /* Registers need to be heap allocated (correct memory range) for tracing to work */
    reg = erts_alloc(ERTS_ALC_T_BEAM_REGISTER, ERTS_X_REGS_ALLOCATED * sizeof(Eterm));
#endif
#endif
    c_p = NULL;
    reds_used = 0;
    goto do_schedule1;

 do_schedule:
    reds_used = REDS_IN(c_p) - FCALLS;
 do_schedule1:
    PROCESS_MAIN_CHK_LOCKS(c_p);
    ERTS_SMP_UNREQ_PROC_MAIN_LOCK(c_p);
    c_p = schedule(c_p, reds_used);
#ifdef DEBUG
    pid = c_p->id;
#endif
    ERTS_SMP_REQ_PROC_MAIN_LOCK(c_p);
    PROCESS_MAIN_CHK_LOCKS(c_p);
#ifdef ERTS_SMP
    reg = c_p->scheduler_data->save_reg;
    freg = c_p->scheduler_data->freg;
#endif
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
	    && (c_p->trace_flags & F_SENSITIVE) == 0) {
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

 OpCase(i_move_call_only_fcr): {
     r(0) = Arg(1);
 }
 /* FALL THROUGH */
 OpCase(i_call_only_f): {
     SET_I((BeamInstr *) Arg(0));
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
    Dispatchx();

 OpCase(i_move_call_ext_cre): {
     r(0) = Arg(0);
     I++;
 }
 /* FALL THROUGH */
 OpCase(i_call_ext_e):
    SET_CP(c_p, I+2);
    Dispatchx();

 OpCase(i_move_call_ext_only_ecr): {
     r(0) = Arg(1);
 }
 /* FALL THROUGH */
 OpCase(i_call_ext_only_e):
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

 OpCase(return): {
    SET_I(c_p->cp);
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

 OpCase(test_heap_1_put_list_Iy): {
     BeamInstr *next;

     PreFetch(2, next);
     TestHeap(Arg(0), 1);
     PutList(yb(Arg(1)), r(0), r(0), StoreSimpleDest);
     CHECK_TERM(r(0));
     NextPF(2, next);
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
     result = send_2(c_p, r(0), x(1));
     PreFetch(0, next);
     POST_BIF_GC_SWAPIN(c_p, result, reg, 2);
     FCALLS = c_p->fcalls;
     if (is_value(result)) {
	 r(0) = result;
	 CHECK_TERM(r(0));
	 NextPF(0, next);
     } else if (c_p->freason == TRAP) {
	 SET_CP(c_p, I+1);
	 SET_I(*((BeamInstr **) (BeamInstr) ((c_p)->def_arg_reg + 3)));
	 SWAPIN;
	 r(0) = c_p->def_arg_reg[0];
	 x(1) = c_p->def_arg_reg[1];
	 Dispatch();
     }
     goto find_func_info;
 }

 OpCase(i_element_jssd): {
     Eterm index;
     Eterm tuple;

     /*
      * Inlined version of element/2 for speed.
      */
     GetArg2(1, index, tuple);
     if (is_small(index) && is_tuple(tuple)) {
	 Eterm* tp = tuple_val(tuple);

	 if ((signed_val(index) >= 1) &&
	     (signed_val(index) <= arityval(*tp))) {
	     Eterm result = tp[signed_val(index)];
	     StoreBifResult(3, result);
	 }
     }
 }
 /* Fall through */

 OpCase(badarg_j):
 badarg:
    c_p->freason = BADARG;
    goto lb_Cl_error;

 OpCase(i_fast_element_jIsd): {
     Eterm tuple;

     /*
      * Inlined version of element/2 for even more speed.
      * The first argument is an untagged integer >= 1.
      * The second argument is guaranteed to be a register operand.
      */
     GetArg1(2, tuple);
     if (is_tuple(tuple)) {
	 Eterm* tp = tuple_val(tuple);
	 tmp_arg2 = Arg(1);
	 if (tmp_arg2 <= arityval(*tp)) {
	     Eterm result = tp[tmp_arg2];
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
	     if (E - HTOP < 3 || c_p->mbuf) {	/* Force GC in case add_stacktrace()
						 * created heap fragments */
		 SWAPOUT;
		 PROCESS_MAIN_CHK_LOCKS(c_p);
		 FCALLS -= erts_garbage_collect(c_p, 3, reg+2, 1);
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
	 else {
#endif
	     SET_I((BeamInstr *) Arg(0));
	     Goto(*I);		/* Jump to a wait or wait_timeout instruction */
#ifdef ERTS_SMP
	 }
#endif
     }
     ErtsMoveMsgAttachmentIntoProc(msgp, c_p, E, HTOP, FCALLS,
				   {
				       SWAPOUT;
				       reg[0] = r(0);
				       PROCESS_MAIN_CHK_LOCKS(c_p);
				   },
				   {
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
	 SEQ_TRACE_TOKEN(c_p) = NIL;
     } else if (ERL_MESSAGE_TOKEN(msgp) != am_undefined) {
	 Eterm msg;
	 SEQ_TRACE_TOKEN(c_p) = ERL_MESSAGE_TOKEN(msgp);
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
			  c_p->id, c_p);
     }
     UNLINK_MESSAGE(c_p, msgp);
     JOIN_MESSAGE(c_p);
     CANCEL_TIMER(c_p);
     free_message(msgp);

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
#if !defined(ARCH_64) || HALFWORD_HEAP
	 Uint time_val;
#endif

	 if (is_small(timeout_value) && signed_val(timeout_value) > 0 &&
#if defined(ARCH_64) && !HALFWORD_HEAP
	     ((unsigned_val(timeout_value) >> 32) == 0)
#else
	     1
#endif
	     ) {
	     /*
	      * The timer routiner will set c_p->i to the value in
	      * c_p->def_arg_reg[0].  Note that it is safe to use this
	      * location because there are no living x registers in
	      * a receive statement.
	      * Note that for the halfword emulator, the two first elements
	      * of the array are used.
	      */
	     *((BeamInstr **) (UWord) c_p->def_arg_reg) = I+3;
	     set_timer(c_p, unsigned_val(timeout_value));
	 } else if (timeout_value == am_infinity) {
	     c_p->flags |= F_TIMO;
#if !defined(ARCH_64) || HALFWORD_HEAP
	 } else if (term_to_Uint(timeout_value, &time_val)) {
	     *((BeamInstr **) (UWord) c_p->def_arg_reg) = I+3;
	     set_timer(c_p, time_val);
#endif
	 } else {		/* Wrong time */
	     OpCase(i_wait_error_locked): {
		 erts_smp_proc_unlock(c_p, ERTS_PROC_LOCKS_MSG_RECEIVE);
		 /* Fall through */
	     }
	     OpCase(i_wait_error): {
		 c_p->freason = EXC_TIMEOUT_VALUE;
		 goto find_func_info;
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
	     ASSERT(!ERTS_PROC_IS_EXITING(c_p));
	     c_p->i = (BeamInstr *) Arg(0); /* L1 */
	     SWAPOUT;
	     c_p->arity = 0;
	     c_p->status = P_WAITING;
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
	 *((BeamInstr **) (UWord) c_p->def_arg_reg) = I+3;
	 set_timer(c_p, Arg(1));
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

 OpCase(i_select_val_sfI):
     GetArg1(0, tmp_arg1);

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
	 if (tmp_arg1 < mid->val) {
	     high = mid;
	 } else if (tmp_arg1 > mid->val) {
	     low = mid + 1;
	 } else {
	     SET_I(mid->addr);
	     Goto(*I);
	 }
     }
     SET_I((BeamInstr *) Arg(1));
     Goto(*I);
 }

 OpCase(i_jump_on_val_zero_sfI):
 {
     Eterm index;

     GetArg1(0, index);
     if (is_small(index)) {
	 index = signed_val(index);
	 if (index < Arg(2)) {
	     SET_I((BeamInstr *) (&Arg(3))[index]);
	     Goto(*I);
	 }
     }
     SET_I((BeamInstr *) Arg(1));
     Goto(*I);
 }

 OpCase(i_jump_on_val_sfII):
 {
     Eterm index;

     GetArg1(0, index);
     if (is_small(index)) {
	 index = (Uint) (signed_val(index) - Arg(3));
	 if (index < Arg(2)) {
	     SET_I((BeamInstr *) (&Arg(4))[index]);
	     Goto(*I);
	 }
     }
     SET_I((BeamInstr *) Arg(1));
     Goto(*I);
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
	Eterm (*bf)(Process*, Eterm);
	Eterm arg;
	Eterm result;

	GetArg1(2, arg);
	bf = (BifFunction) Arg(1);
	c_p->fcalls = FCALLS;
	PROCESS_MAIN_CHK_LOCKS(c_p);
	ASSERT(!ERTS_PROC_IS_EXITING(c_p));
	result = (*bf)(c_p, arg);
	ASSERT(!ERTS_PROC_IS_EXITING(c_p) || is_non_value(result));
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
	Eterm (*bf)(Process*, Eterm);

	Eterm arg;
	Eterm result;

	GetArg1(1, arg);
	bf = (BifFunction) Arg(0);
	c_p->fcalls = FCALLS;
	PROCESS_MAIN_CHK_LOCKS(c_p);
	ASSERT(!ERTS_PROC_IS_EXITING(c_p));
	result = (*bf)(c_p, arg);
	ASSERT(!ERTS_PROC_IS_EXITING(c_p) || is_non_value(result));
	PROCESS_MAIN_CHK_LOCKS(c_p);
	ERTS_HOLE_CHECK(c_p);
	FCALLS = c_p->fcalls;
	if (is_value(result)) {
	    StoreBifResult(2, result);
	}
	reg[0] = arg;
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
	Eterm (*bf)(Process*, Eterm, Eterm);
	Eterm result;

	bf = (BifFunction) Arg(1);
	c_p->fcalls = FCALLS;
	PROCESS_MAIN_CHK_LOCKS(c_p);
	ASSERT(!ERTS_PROC_IS_EXITING(c_p));
	result = (*bf)(c_p, tmp_arg1, tmp_arg2);
	ASSERT(!ERTS_PROC_IS_EXITING(c_p) || is_non_value(result));
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
	Eterm (*bf)(Process*, Eterm, Eterm);
	Eterm result;

	bf = (BifFunction) Arg(0);
	PROCESS_MAIN_CHK_LOCKS(c_p);
	ASSERT(!ERTS_PROC_IS_EXITING(c_p));
	result = (*bf)(c_p, tmp_arg1, tmp_arg2);
	ASSERT(!ERTS_PROC_IS_EXITING(c_p) || is_non_value(result));
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
 OpCase(call_bif0_e):
    {
	Eterm (*bf)(Process*, BeamInstr*) = GET_BIF_ADDRESS(Arg(0));

	PRE_BIF_SWAPOUT(c_p);
	c_p->fcalls = FCALLS - 1;
	if (FCALLS <= 0) {
	    save_calls(c_p, (Export *) Arg(0));
	}

	ASSERT(!ERTS_PROC_IS_EXITING(c_p));
	r(0) = (*bf)(c_p, I);
	ASSERT(!ERTS_PROC_IS_EXITING(c_p) || is_non_value(r(0)));
	ERTS_HOLE_CHECK(c_p);
	POST_BIF_GC_SWAPIN_0(c_p, r(0));
	FCALLS = c_p->fcalls;
	if (is_value(r(0))) {
	    CHECK_TERM(r(0));
	    Next(1);
	}
	else if (c_p->freason == TRAP) {
	    goto call_bif_trap3;
	}

	/*
	 * Error handling.  SWAPOUT is not needed because it was done above.
	 */
	ASSERT(c_p->stop == E);
	reg[0] = r(0);
	I = handle_error(c_p, I, reg, bf);
	goto post_error_handling;
    }

 OpCase(call_bif1_e):
    {
	Eterm (*bf)(Process*, Eterm, BeamInstr*) = GET_BIF_ADDRESS(Arg(0));
	Eterm result;
	BeamInstr *next;

	c_p->fcalls = FCALLS - 1;
	if (FCALLS <= 0) {
	    save_calls(c_p, (Export *) Arg(0));
	}
	PreFetch(1, next);
	PRE_BIF_SWAPOUT(c_p);
	ASSERT(!ERTS_PROC_IS_EXITING(c_p));
	result = (*bf)(c_p, r(0), I);
	ASSERT(!ERTS_PROC_IS_EXITING(c_p) || is_non_value(result));
	ERTS_HOLE_CHECK(c_p);
	POST_BIF_GC_SWAPIN(c_p, result, reg, 1);
	FCALLS = c_p->fcalls;
        if (is_value(result)) {
	    r(0) = result;
	    CHECK_TERM(r(0));
	    NextPF(1, next);
	} else if (c_p->freason == TRAP) {
	    goto call_bif_trap3;
	}

	/*
	 * Error handling.  SWAPOUT is not needed because it was done above.
	 */
	ASSERT(c_p->stop == E);
	reg[0] = r(0);
	I = handle_error(c_p, I, reg, bf);
	goto post_error_handling;
    }

 OpCase(call_bif2_e):
    {
	Eterm (*bf)(Process*, Eterm, Eterm, BeamInstr*) = GET_BIF_ADDRESS(Arg(0));
	Eterm result;
	BeamInstr *next;

	PRE_BIF_SWAPOUT(c_p);
	c_p->fcalls = FCALLS - 1;
	if (FCALLS <= 0) {
	   save_calls(c_p, (Export *) Arg(0));
	}
	PreFetch(1, next);
	CHECK_TERM(r(0));
	CHECK_TERM(x(1));
	ASSERT(!ERTS_PROC_IS_EXITING(c_p));
	result = (*bf)(c_p, r(0), x(1), I);
	ASSERT(!ERTS_PROC_IS_EXITING(c_p) || is_non_value(result));
	ERTS_HOLE_CHECK(c_p);
	POST_BIF_GC_SWAPIN(c_p, result, reg, 2);
	FCALLS = c_p->fcalls;
	if (is_value(result)) {
	    r(0) = result;
	    CHECK_TERM(r(0));
	    NextPF(1, next);
	} else if (c_p->freason == TRAP) {
	    goto call_bif_trap3;
	}

	/*
	 * Error handling.  SWAPOUT is not needed because it was done above.
	 */
	ASSERT(c_p->stop == E);
	reg[0] = r(0);
	I = handle_error(c_p, I, reg, bf);
	goto post_error_handling;
    }

 OpCase(call_bif3_e):
    {
	Eterm (*bf)(Process*, Eterm, Eterm, Eterm, BeamInstr*) = GET_BIF_ADDRESS(Arg(0));
	Eterm result;
	BeamInstr *next;

	PRE_BIF_SWAPOUT(c_p);
	c_p->fcalls = FCALLS - 1;
	if (FCALLS <= 0) {
	   save_calls(c_p, (Export *) Arg(0));
	}
	PreFetch(1, next);
	ASSERT(!ERTS_PROC_IS_EXITING(c_p));
	result = (*bf)(c_p, r(0), x(1), x(2), I);
	ASSERT(!ERTS_PROC_IS_EXITING(c_p) || is_non_value(result));
	ERTS_HOLE_CHECK(c_p);
	POST_BIF_GC_SWAPIN(c_p, result, reg, 3);
	FCALLS = c_p->fcalls;
	if (is_value(result)) {
	    r(0) = result;
	    CHECK_TERM(r(0));
	    NextPF(1, next);
	} else if (c_p->freason == TRAP) {
	call_bif_trap3:
	    SET_CP(c_p, I+2);
	    SET_I(*((BeamInstr **) (UWord) ((c_p)->def_arg_reg + 3)));
	    SWAPIN;
	    r(0) = c_p->def_arg_reg[0];
	    x(1) = c_p->def_arg_reg[1];
	    x(2) = c_p->def_arg_reg[2];
	    Dispatch();
	}

	/*
	 * Error handling.  SWAPOUT is not needed because it was done above.
	 */
	ASSERT(c_p->stop == E);
	reg[0] = r(0);
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
     GetArg1(1, tmp_arg1);
     if (is_small(tmp_arg1)) {
	 tmp_arg1 = make_small(~signed_val(tmp_arg1));
     } else {
	 Uint live = Arg(2);
	 SWAPOUT;
	 reg[0] = r(0);
	 reg[live] = tmp_arg1;
	 tmp_arg1 = erts_gc_bnot(c_p, reg, live);
	 r(0) = reg[0];
	 SWAPIN;
	 ERTS_HOLE_CHECK(c_p);
	 if (is_nil(tmp_arg1)) {
	     goto lb_Cl_error;
	 }
     }
     StoreBifResult(3, tmp_arg1);
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
     erts_smp_proc_lock(c_p, ERTS_PROC_LOCK_STATUS);
     if (c_p->status != P_SUSPENDED)
	 erts_add_to_runq(c_p);
     erts_smp_proc_unlock(c_p, ERTS_PROC_LOCK_STATUS);
     goto do_schedule1;
 }

 OpCase(i_select_tuple_arity_sfI):
 {
     GetArg1(0, tmp_arg1);

     if (is_tuple(tmp_arg1)) {
	 tmp_arg1 = *tuple_val(tmp_arg1);
	 goto do_binary_search;
     }
     SET_I((BeamInstr *) Arg(1));
     Goto(*I);
 }     

 OpCase(i_select_big_sf):
    {
	Eterm* bigp;
	Uint arity;
	Eterm* given;
	Uint given_arity;
	Uint given_size;

	GetArg1(0, tmp_arg1);
	if (is_big(tmp_arg1)) {

	    /*
	     * The loader has sorted the bignumbers in descending order
	     * on the arity word.  Therefore, we know that the search
	     * has failed as soon as we encounter an arity word less than
	     * the arity word of the given number.  There is a zero word
	     * (less than any valid arity word) stored after the last bignumber.
	     */

 	    given = big_val(tmp_arg1);
	    given_arity = given[0];
	    given_size = thing_arityval(given_arity);
	    bigp = (Eterm *) &Arg(2);
	    while ((arity = bigp[0]) > given_arity) {
		bigp += (TermWords(thing_arityval(arity) + 1) + 1) * (sizeof(BeamInstr)/sizeof(Eterm));
	    }
	    while (bigp[0] == given_arity) {
		if (memcmp(bigp+1, given+1, sizeof(Eterm)*given_size) == 0) {
		    BeamInstr *tmp =
			((BeamInstr *) (UWord) bigp) + TermWords(given_size + 1);
		    SET_I((BeamInstr *) *tmp);
		    Goto(*I);
		}
		bigp += (TermWords(thing_arityval(arity) + 1) + 1) * (sizeof(BeamInstr)/sizeof(Eterm));
	    }
	}

	/*
	 * Failed.
	 */

	SET_I((BeamInstr *) Arg(1));
	Goto(*I);
    }

#if defined(ARCH_64) && !HALFWORD_HEAP
 OpCase(i_select_float_sfI):
 {
     Uint f;
     int n;
     struct ValLabel {
	 Uint f;
	 BeamInstr* addr;
     };
     struct ValLabel* ptr;

     GetArg1(0, tmp_arg1);
     ASSERT(is_float(tmp_arg1));
     f = float_val(tmp_arg1)[1];
     n = Arg(2);
     ptr = (struct ValLabel *) &Arg(3);
     while (n-- > 0) {
	 if (ptr->f == f) {
	     SET_I(ptr->addr);
	     Goto(*I);
	 }
	 ptr++;
     }
     SET_I((Eterm *) Arg(1));
     Goto(*I);
 }
#else
 OpCase(i_select_float_sfI):
 {
     Uint fpart1;
     Uint fpart2;
     int n;
     struct ValLabel {
	 Uint fpart1;
	 Uint fpart2;
	 BeamInstr* addr;
     };
     struct ValLabel* ptr;

     GetArg1(0, tmp_arg1);
     ASSERT(is_float(tmp_arg1));
     fpart1 = float_val(tmp_arg1)[1];
     fpart2 = float_val(tmp_arg1)[2];

     n = Arg(2);
     ptr = (struct ValLabel *) &Arg(3);
     while (n-- > 0) {
	 if (ptr->fpart1 == fpart1 && ptr->fpart2 == fpart2) {
	     SET_I(ptr->addr);
	     Goto(*I);
	 }
	 ptr++;
     }
     SET_I((BeamInstr *) Arg(1));
     Goto(*I);
 }
#endif

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
     GetArg2(0, tmp_arg1, tmp_arg2);
     c_p->fvalue = tmp_arg2;
     if (c_p->freason == EXC_NULL) {
       /* a safety check for the R10-0 case; should not happen */
       c_p->ftrace = NIL;
       c_p->freason = EXC_ERROR;
     }
     /* for R10-0 code, keep existing c_p->ftrace and hope it's correct */
     switch (tmp_arg1) {
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
	 c_p->ftrace = tmp_arg1;
	 s = get_trace_from_exc(tmp_arg1);
	 if (s == NULL) {
	   c_p->freason = EXC_ERROR;
	 } else {
	   c_p->freason = PRIMARY_EXCEPTION(s->freason);
	 }
       }
     }
     goto find_func_info;
 }

 OpCase(badmatch_s): {
     GetArg1(0, tmp_arg1);
     c_p->fvalue = tmp_arg1;
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
    tmp_arg1 = call_error_handler(c_p, I-3, reg);
    r(0) = reg[0];
    SWAPIN;
    if (tmp_arg1) {
	SET_I(c_p->i);
	Dispatch();
    }

 /* Fall through */
 OpCase(error_action_code): {
 no_error_handler:
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
	   */
     	 BifFunction vbf;

	 c_p->current = I-3; /* current and vbf set to please handle_error */ 
	 SWAPOUT;
	 c_p->fcalls = FCALLS - 1;
	 PROCESS_MAIN_CHK_LOCKS(c_p);
	 tmp_arg2 = I[-1];
	 ERTS_SMP_UNREQ_PROC_MAIN_LOCK(c_p);

	 ASSERT(!ERTS_PROC_IS_EXITING(c_p));
	 {
	     typedef Eterm NifF(struct enif_environment_t*, int argc, Eterm argv[]);
	     NifF* fp = vbf = (NifF*) I[1];
	     struct enif_environment_t env;
	     erts_pre_nif(&env, c_p, (struct erl_module_nif*)I[2]);
	     reg[0] = r(0);
	     tmp_arg1 = (*fp)(&env, tmp_arg2, reg);
	     erts_post_nif(&env);
	 }
	 ASSERT(!ERTS_PROC_IS_EXITING(c_p) || is_non_value(tmp_arg1));
	 PROCESS_MAIN_CHK_LOCKS(c_p);
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
	SWAPOUT;
	c_p->fcalls = FCALLS - 1;
	vbf = (BifFunction) Arg(0);
	PROCESS_MAIN_CHK_LOCKS(c_p);
	tmp_arg2 = I[-1];
	ASSERT(tmp_arg2 <= 3);
	ERTS_SMP_UNREQ_PROC_MAIN_LOCK(c_p);
	switch (tmp_arg2) {
	case 3:
	    {
		Eterm (*bf)(Process*, Eterm, Eterm, Eterm, BeamInstr*) = vbf;
		ASSERT(!ERTS_PROC_IS_EXITING(c_p));
		tmp_arg1 = (*bf)(c_p, r(0), x(1), x(2), I);
		ASSERT(!ERTS_PROC_IS_EXITING(c_p) || is_non_value(tmp_arg1));
		PROCESS_MAIN_CHK_LOCKS(c_p);
	    }
	    break;
	case 2:
	    {
		Eterm (*bf)(Process*, Eterm, Eterm, BeamInstr*) = vbf;
		ASSERT(!ERTS_PROC_IS_EXITING(c_p));
		tmp_arg1 = (*bf)(c_p, r(0), x(1), I);
		ASSERT(!ERTS_PROC_IS_EXITING(c_p) || is_non_value(tmp_arg1));
		PROCESS_MAIN_CHK_LOCKS(c_p);
	    }
	    break;
	case 1:
	    {
		Eterm (*bf)(Process*, Eterm, BeamInstr*) = vbf;
		ASSERT(!ERTS_PROC_IS_EXITING(c_p));
		tmp_arg1 = (*bf)(c_p, r(0), I);
		ASSERT(!ERTS_PROC_IS_EXITING(c_p) || is_non_value(tmp_arg1));
		PROCESS_MAIN_CHK_LOCKS(c_p);
	    }
	    break;
	case 0:
	    {
		Eterm (*bf)(Process*, BeamInstr*) = vbf;
		ASSERT(!ERTS_PROC_IS_EXITING(c_p));
		tmp_arg1 = (*bf)(c_p, I);
		ASSERT(!ERTS_PROC_IS_EXITING(c_p) || is_non_value(tmp_arg1));
		PROCESS_MAIN_CHK_LOCKS(c_p);
		break;
	    }
	}
apply_bif_or_nif_epilogue:
	ERTS_SMP_REQ_PROC_MAIN_LOCK(c_p);
	ERTS_HOLE_CHECK(c_p);
	if (c_p->mbuf) {
	    reg[0] = r(0);
	    tmp_arg1 = erts_gc_after_bif_call(c_p, tmp_arg1, reg, tmp_arg2);
	    r(0) = reg[0];
	}
	SWAPIN;			/* There might have been a garbage collection. */
	FCALLS = c_p->fcalls;
	if (is_value(tmp_arg1)) {
	    r(0) = tmp_arg1;
	    CHECK_TERM(r(0));
	    SET_I(c_p->cp);
	    Goto(*I);
	} else if (c_p->freason == TRAP) {
	    SET_I(*((BeamInstr **) (UWord) ((c_p)->def_arg_reg + 3)));
	    r(0) = c_p->def_arg_reg[0];
	    x(1) = c_p->def_arg_reg[1];
	    x(2) = c_p->def_arg_reg[2];
	    Dispatch();
	}
	reg[0] = r(0);
	I = handle_error(c_p, c_p->cp, reg, vbf);
	goto post_error_handling;
    }

 OpCase(i_get_sd):
    {
	Eterm arg;
	Eterm result;

	GetArg1(0, arg);
	result = erts_pd_hash_get(c_p, arg);
	StoreBifResult(1, result);
    }

 OpCase(case_end_s):
    GetArg1(0, tmp_arg1);
    c_p->fvalue = tmp_arg1;
    c_p->freason = EXC_CASE_CLAUSE;
    goto find_func_info;

 OpCase(if_end):
    c_p->freason = EXC_IF_CLAUSE;
    goto find_func_info;

 OpCase(i_func_info_IaaI): {
     c_p->freason = EXC_FUNCTION_CLAUSE;
     c_p->current = I + 2;
     goto lb_error_action_code;
 }

 OpCase(try_case_end_s):
    GetArg1(0, tmp_arg1);
    c_p->fvalue = tmp_arg1;
    c_p->freason = EXC_TRY_CLAUSE;
    goto find_func_info;

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
     num_bytes = (num_bits+7) >> 3;
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
	 bptr->flags = 0;
	 bptr->orig_size = num_bytes;
	 erts_refc_init(&bptr->refc, 1);
	 erts_current_bin = (byte *) bptr->orig_bytes;

	 /*
	  * Now allocate the ProcBin on the heap.
	  */
	 pb = (ProcBin *) HTOP;
	 HTOP += PROC_BIN_SIZE;
	 pb->thing_word = HEADER_PROC_BIN;
	 pb->size = num_bytes;
	 pb->next = MSO(c_p).mso;
	 MSO(c_p).mso = pb;
	 pb->val = bptr;
	 pb->bytes = (byte*) bptr->orig_bytes;
	 pb->flags = 0;
	 MSO(c_p).overhead += pb->size / sizeof(Eterm);
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
	 bptr->flags = 0;
	 bptr->orig_size = tmp_arg1;
	 erts_refc_init(&bptr->refc, 1);
	 erts_current_bin = (byte *) bptr->orig_bytes;

	 /*
	  * Now allocate the ProcBin on the heap.
	  */
	 pb = (ProcBin *) HTOP;
	 HTOP += PROC_BIN_SIZE;
	 pb->thing_word = HEADER_PROC_BIN;
	 pb->size = tmp_arg1;
	 pb->next = MSO(c_p).mso;
	 MSO(c_p).mso = pb;
	 pb->val = bptr;
	 pb->bytes = (byte*) bptr->orig_bytes;
	 pb->flags = 0;
	 
	 MSO(c_p).overhead += tmp_arg1 / sizeof(Eterm);

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
	 (make_small(0xD800UL) <= val && val <= make_small(0xDFFFUL)) ||
	 val == make_small(0xFFFEUL) || val == make_small(0xFFFFUL)) {
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
	 (make_small(0xD800UL) <= tmp_arg1 && tmp_arg1 <= make_small(0xDFFFUL)) ||
	 tmp_arg1 == make_small(0xFFFEUL) || tmp_arg1 == make_small(0xFFFFUL)) {
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

     OpCase(i_bs_start_match2_rfIId): {
	 tmp_arg1 = r(0);

     do_start_match:
	 slots = Arg(2);
	 if (!is_boxed(tmp_arg1)) {
	     ClauseFail();
	 }
	 PreFetch(4, next);
	 header = *boxed_val(tmp_arg1);
	 if (header_is_bin_matchstate(header)) {
	     ErlBinMatchState* ms = (ErlBinMatchState *) boxed_val(tmp_arg1);
	     Uint actual_slots = HEADER_NUM_SLOTS(header);
	     ms->save_offset[0] = ms->mb.offset;
	     if (actual_slots < slots) {
		 ErlBinMatchState* dst;
		 Uint live = Arg(1);
		 Uint wordsneeded = ERL_BIN_MATCHSTATE_SIZE(slots);

		 TestHeapPreserve(wordsneeded, live, tmp_arg1);
		 ms = (ErlBinMatchState *) boxed_val(tmp_arg1);
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
	     TestHeapPreserve(wordsneeded, live, tmp_arg1);
	     HEAP_TOP(c_p) = HTOP;
#ifdef DEBUG
	     c_p->stop = E;	/* Needed for checking in HeapOnlyAlloc(). */
#endif
	     result = erts_bs_start_match_2(c_p, tmp_arg1, slots);
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
	 tmp_arg1 = xb(Arg(0));
	 I++;
	 goto do_start_match;
     }
     OpCase(i_bs_start_match2_yfIId): {
	 tmp_arg1 = yb(Arg(0));
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

 OpCase(i_bs_get_integer_8_rfd): {
     tmp_arg1 = r(0);
     goto do_bs_get_integer_8;
 }

 OpCase(i_bs_get_integer_8_xfd): {
     tmp_arg1 = xb(Arg(0));
     I++;
 }

 do_bs_get_integer_8: {
     ErlBinMatchBuffer *_mb;
     Eterm _result;
     _mb = ms_matchbuffer(tmp_arg1);
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

 OpCase(i_bs_get_integer_16_rfd): {
     tmp_arg1 = r(0);
     goto do_bs_get_integer_16;
 }

 OpCase(i_bs_get_integer_16_xfd): {
     tmp_arg1 = xb(Arg(0));
     I++;
 }

 do_bs_get_integer_16: {
     ErlBinMatchBuffer *_mb;
     Eterm _result;
     _mb = ms_matchbuffer(tmp_arg1);
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

 OpCase(i_bs_get_integer_32_rfId): {
     tmp_arg1 = r(0);
     goto do_bs_get_integer_32;
 }
     
 OpCase(i_bs_get_integer_32_xfId): {
     tmp_arg1 = xb(Arg(0));
     I++;
 }

 do_bs_get_integer_32: {
     ErlBinMatchBuffer *_mb;
     Uint32 _integer;
     Eterm _result;
     _mb = ms_matchbuffer(tmp_arg1);
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
	 Uint wordsneeded = 1+WSIZE(NBYTES((Uint) size));
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

 /* Operands: MatchContext Fail Dst */
 OpCase(i_bs_get_utf8_rfd): {
     tmp_arg1 = r(0);
     goto do_bs_get_utf8;
 }

 OpCase(i_bs_get_utf8_xfd): {
     tmp_arg1 = xb(Arg(0));
     I++;
 }

 /*
  * tmp_arg1 = match_context
  * Operands: Fail Dst
  */

    do_bs_get_utf8: {
     Eterm result = erts_bs_get_utf8(ms_matchbuffer(tmp_arg1));
     if (is_non_value(result)) {
	 ClauseFail();
     }
     StoreBifResult(1, result);
 }

 /* Operands: MatchContext Fail Flags Dst */
 OpCase(i_bs_get_utf16_rfId): {
     tmp_arg1 = r(0);
     goto do_bs_get_utf16;
 }

 OpCase(i_bs_get_utf16_xfId): {
     tmp_arg1 = xb(Arg(0));
     I++;
 }

 /*
  * tmp_arg1 = match_context
  * Operands: Fail Flags Dst
  */
    do_bs_get_utf16: {
     Eterm result = erts_bs_get_utf16(ms_matchbuffer(tmp_arg1), Arg(1));
     if (is_non_value(result)) {
	 ClauseFail();
     }
     StoreBifResult(2, result);
 }

 {
     ErlBinMatchBuffer* mb;
     ErlSubBin* sb;
     Uint size;
     Uint offs;
     Uint orig;
     Uint hole_size;

     OpCase(bs_context_to_binary_r): {
	 tmp_arg1 = x0;
	 I -= 2;
	 goto do_context_to_binary;
     }

     /* Unfortunately, inlining can generate this instruction. */
     OpCase(bs_context_to_binary_y): {
	 tmp_arg1 = yb(Arg(0));
	 goto do_context_to_binary0;
     }

     OpCase(bs_context_to_binary_x): {
	 tmp_arg1 = xb(Arg(0));
     
     do_context_to_binary0:
	 I--;
     }

 do_context_to_binary:
     if (is_boxed(tmp_arg1) && header_is_bin_matchstate(*boxed_val(tmp_arg1))) {
	 ErlBinMatchState* ms = (ErlBinMatchState *) boxed_val(tmp_arg1);
	 mb = &ms->mb;
	 offs = ms->save_offset[0];
	 size = mb->size - offs;
	 goto do_bs_get_binary_all_reuse_common;
     }
     Next(2);

     OpCase(i_bs_get_binary_all_reuse_rfI): {
	 tmp_arg1 = x0;
	 goto do_bs_get_binary_all_reuse;
     }

     OpCase(i_bs_get_binary_all_reuse_xfI): {
	 tmp_arg1 = xb(Arg(0));
	 I++;
     }

 do_bs_get_binary_all_reuse:
     mb = ms_matchbuffer(tmp_arg1);
     size = mb->size - mb->offset;
     if (size % Arg(1) != 0) {
	 ClauseFail();
     }
     offs = mb->offset;

 do_bs_get_binary_all_reuse_common:
     orig = mb->orig;
     sb = (ErlSubBin *) boxed_val(tmp_arg1);
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
     OpCase(i_bs_match_string_rfII): {
	 tmp_arg1 = r(0);
	 goto do_bs_match_string;
     }
     OpCase(i_bs_match_string_xfII): {
	 tmp_arg1 = xb(Arg(0));
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
	 mb = ms_matchbuffer(tmp_arg1);
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

    /*
     * At this point, I points to the code[3] in the export entry for
     * a trace-enabled function.
     *
     * code[0]: Module
     * code[1]: Function
     * code[2]: Arity
     * code[3]: &&call_traced_function
     * code[4]: Address of function.
     */
 OpCase(call_traced_function): {
     if (IS_TRACED_FL(c_p, F_TRACE_CALLS)) {
	 unsigned offset = offsetof(Export, code) + 3*sizeof(BeamInstr);
	 Export* ep = (Export *) (((char *)I)-offset);
	 Uint32 flags;

	 SWAPOUT;
	 reg[0] = r(0);
	 PROCESS_MAIN_CHK_LOCKS(c_p);
	 ERTS_SMP_UNREQ_PROC_MAIN_LOCK(c_p);
	 flags = erts_call_trace(c_p, ep->code, ep->match_prog_set, reg,
				 0, &c_p->tracer_proc);
	 ERTS_SMP_REQ_PROC_MAIN_LOCK(c_p);
	 PROCESS_MAIN_CHK_LOCKS(c_p);
	 ASSERT(!ERTS_PROC_IS_EXITING(c_p));
	 SWAPIN;
	 
	 if (flags & MATCH_SET_RX_TRACE) {
	     ASSERT(c_p->htop <= E && E <= c_p->hend);
	     if (E - 3 < HTOP) {
		 /* SWAPOUT, SWAPIN was done and r(0) was saved above */
		 PROCESS_MAIN_CHK_LOCKS(c_p);
		 FCALLS -= erts_garbage_collect(c_p, 3, reg, ep->code[2]);
		 PROCESS_MAIN_CHK_LOCKS(c_p);
		 r(0) = reg[0];
		 SWAPIN;
	     }
	     E -= 3;
	     ASSERT(c_p->htop <= E && E <= c_p->hend);
	     ASSERT(is_CP((BeamInstr)(ep->code)));
	     ASSERT(is_internal_pid(c_p->tracer_proc) || 
		    is_internal_port(c_p->tracer_proc));
	     E[2] = make_cp(c_p->cp); /* XXX:PaN - code in lower range on halfword */
	     E[1] = am_true; /* Process tracer */
	     E[0] = make_cp(ep->code);
	     c_p->cp = (flags & MATCH_SET_EXCEPTION_TRACE)
		 ? beam_exception_trace : beam_return_trace;
	     erts_smp_proc_lock(c_p, ERTS_PROC_LOCKS_ALL_MINOR);
	     c_p->trace_flags |= F_EXCEPTION_TRACE;
	     erts_smp_proc_unlock(c_p, ERTS_PROC_LOCKS_ALL_MINOR);
	 }
     }
     SET_I((BeamInstr *)Arg(0));
     Dispatch();
 }

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

 OpCase(i_count_breakpoint): {
     BeamInstr real_I;
     
     ErtsCountBreak(c_p, (BeamInstr *) I, &real_I);
     ASSERT(VALID_INSTR(real_I));
     Goto(real_I);
 }

 /* need to send mfa instead of bdt pointer
  * the pointer might be deallocated.
  */

 OpCase(i_time_breakpoint): {
     BeamInstr real_I;
     BpData **bds = (BpData **) (I)[-4];
     BpDataTime *bdt = NULL;
     Uint ix = 0;
#ifdef ERTS_SMP
     ix = c_p->scheduler_data->no - 1;
#else
     ix = 0;
#endif
     bdt = (BpDataTime *)bds[ix];

     ASSERT((I)[-5] == (BeamInstr) BeamOp(op_i_func_info_IaaI));
     ASSERT(bdt);
     bdt = (BpDataTime *) bdt->next;
     ASSERT(bdt);
     bds[ix] = (BpData *) bdt;
     real_I = bdt->orig_instr;
     ASSERT(VALID_INSTR(real_I));

     if (IS_TRACED_FL(c_p, F_TRACE_CALLS) && !(bdt->pause)) {
	 if (	(*(c_p->cp) == (BeamInstr) OpCode(i_return_time_trace)) ||
		(*(c_p->cp) == (BeamInstr) OpCode(return_trace)) ||
		(*(c_p->cp) == (BeamInstr) OpCode(i_return_to_trace))) {
	     /* This _IS_ a tail recursive call */
	     SWAPOUT;
	     erts_trace_time_break(c_p, I, bdt, ERTS_BP_CALL_TIME_TAIL_CALL);
	     SWAPIN;
	 } else {
	     SWAPOUT;
	     erts_trace_time_break(c_p, I, bdt, ERTS_BP_CALL_TIME_CALL);

	     /* r register needs to be copied to the array
	      * for the garbage collector
	      */
	     ASSERT(c_p->htop <= E && E <= c_p->hend);
	     if (E - 2 < HTOP) {
		 reg[0] = r(0);
		 PROCESS_MAIN_CHK_LOCKS(c_p);
		 FCALLS -= erts_garbage_collect(c_p, 2, reg, I[-1]);
		 PROCESS_MAIN_CHK_LOCKS(c_p);
		 r(0) = reg[0];
	     }
	     SWAPIN;

	     ASSERT(c_p->htop <= E && E <= c_p->hend);

	     E -= 2;
	     E[0] = make_cp(I);
	     E[1] = make_cp(c_p->cp);     /* original return address */
	     c_p->cp = (BeamInstr *) make_cp(beam_return_time_trace);
	 }
     }

     Goto(real_I);
 }

 OpCase(i_return_time_trace): {
     BeamInstr *pc = (BeamInstr *) (UWord) E[0];
     SWAPOUT;
     erts_trace_time_break(c_p, pc, NULL, ERTS_BP_CALL_TIME_RETURN);
     SWAPIN;
     c_p->cp = NULL;
     SET_I((BeamInstr *) cp_val(E[1]));
     E += 2;
     Goto(*I);
 }

 OpCase(i_trace_breakpoint):
     if (! IS_TRACED_FL(c_p, F_TRACE_CALLS)) {
	 BeamInstr real_I;
	 
	 ErtsBreakSkip(c_p, (BeamInstr *) I, &real_I);
	 Goto(real_I);
     }
 /* Fall through to next case */
 OpCase(i_mtrace_breakpoint): {
     BeamInstr real_I;
     Uint32 flags;
     Eterm tracer_pid;
     BeamInstr *cpp;
     int return_to_trace = 0, need = 0;
     flags = 0;
     SWAPOUT;
     reg[0] = r(0);

     if (*(c_p->cp) == (BeamInstr) OpCode(return_trace)) {
	 cpp = (BeamInstr*)&E[2];
     } else if (*(c_p->cp) == (BeamInstr) OpCode(i_return_to_trace)) {
	 return_to_trace = !0;
	 cpp = (BeamInstr*)&E[0];
     } else if (*(c_p->cp) == (BeamInstr) OpCode(i_return_time_trace)) {
	 return_to_trace = !0;
	 cpp = (BeamInstr*)&E[0];
     } else {
	 cpp = NULL;
     }
     if (cpp) {
	 /* This _IS_ a tail recursive call, if there are
	  * return_trace and/or i_return_to_trace stackframes
	  * on the stack, they are not intermixed with y registers
	  */
	 BeamInstr *cp_save = c_p->cp;
	 for (;;) {
	     ASSERT(is_CP(*cpp));
	     if (*cp_val(*cpp) == (BeamInstr) OpCode(return_trace)) {
		 cpp += 3;
	     } else if (*cp_val(*cpp) == (BeamInstr) OpCode(i_return_to_trace)) {
		 return_to_trace = !0;
		 cpp += 1;
	     } else if (*cp_val(*cpp) == (BeamInstr) OpCode(i_return_time_trace)) {
		 cpp += 2;
	     } else
		 break;
	 }
	 c_p->cp = (BeamInstr *) cp_val(*cpp);
	 ASSERT(is_CP(*cpp));
	 ERTS_SMP_UNREQ_PROC_MAIN_LOCK(c_p);
	 real_I = erts_trace_break(c_p, I, reg, &flags, &tracer_pid);
	 ERTS_SMP_REQ_PROC_MAIN_LOCK(c_p);
	 SWAPIN;		/* Needed by shared heap. */
	 c_p->cp = cp_save;
     } else {
	 ERTS_SMP_UNREQ_PROC_MAIN_LOCK(c_p);
	 real_I = erts_trace_break(c_p, I, reg, &flags, &tracer_pid);
	 ERTS_SMP_REQ_PROC_MAIN_LOCK(c_p);
	 SWAPIN;		/* Needed by shared heap. */
     }

     ASSERT(!ERTS_PROC_IS_EXITING(c_p));

     if ((flags & MATCH_SET_RETURN_TO_TRACE) && !return_to_trace) {
	 need += 1;
     }
     if (flags & MATCH_SET_RX_TRACE) {
	 need += 3;
     }
     if (need) {
	 ASSERT(c_p->htop <= E && E <= c_p->hend);
	 if (E - need < HTOP) {
	     /* SWAPOUT was done and r(0) was saved above */
	     PROCESS_MAIN_CHK_LOCKS(c_p);
	     FCALLS -= erts_garbage_collect(c_p, need, reg, I[-1]);
	     PROCESS_MAIN_CHK_LOCKS(c_p);
	     r(0) = reg[0];
	     SWAPIN;
	 }
     }
     if ((flags & MATCH_SET_RETURN_TO_TRACE) && !return_to_trace) {
	 E -= 1;
	 ASSERT(c_p->htop <= E && E <= c_p->hend);
	 E[0] = make_cp(c_p->cp);
	 c_p->cp = (BeamInstr *) beam_return_to_trace;
     }
     if (flags & MATCH_SET_RX_TRACE) {
	 E -= 3;
	 ASSERT(c_p->htop <= E && E <= c_p->hend);
	 ASSERT(is_CP((Eterm) (UWord) (I - 3)));
	 ASSERT(am_true == tracer_pid || 
		is_internal_pid(tracer_pid) || is_internal_port(tracer_pid));
	 E[2] = make_cp(c_p->cp);
	 E[1] = tracer_pid;
	 E[0] = make_cp(I - 3); /* We ARE at the beginning of an 
				   instruction,
				   the funcinfo is above i. */
	 c_p->cp =
	     (flags & MATCH_SET_EXCEPTION_TRACE)
		     ? beam_exception_trace : beam_return_trace;
	 erts_smp_proc_lock(c_p, ERTS_PROC_LOCKS_ALL_MINOR);
	 c_p->trace_flags |= F_EXCEPTION_TRACE;
	 erts_smp_proc_unlock(c_p, ERTS_PROC_LOCKS_ALL_MINOR);
     }
     Goto(real_I);
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
  * Instructions for allocating on the message area.
  */

 OpCase(i_global_cons):
 {
     BeamInstr *next;
#ifdef HYBRID
     Eterm *hp;

     PreFetch(0,next);
     TestGlobalHeap(2,2,hp);
     hp[0] = r(0);
     hp[1] = x(1);
     r(0) = make_list(hp);
#ifndef INCREMENTAL
     global_htop += 2;
#endif
     NextPF(0,next);
#else
     PreFetch(0,next);
     c_p->freason = EXC_INTERNAL_ERROR;
     goto find_func_info;
#endif
 }

 OpCase(i_global_tuple):
 {
     BeamInstr *next;
     int len;
#ifdef HYBRID
     Eterm list;
     Eterm *hp;
#endif

     if ((len = list_length(r(0))) < 0) {
         goto badarg;
     }

     PreFetch(0,next);
#ifdef HYBRID
     TestGlobalHeap(len + 1,1,hp);
     list = r(0);
     r(0) = make_tuple(hp);
     *hp++ = make_arityval(len);
     while(is_list(list))
     {
         Eterm* cons = list_val(list);
         *hp++ = CAR(cons);
         list = CDR(cons);
     }
#ifndef INCREMENTAL
     global_htop += len + 1;
#endif
     NextPF(0,next);
#else
     c_p->freason = EXC_INTERNAL_ERROR;
     goto find_func_info;
#endif
 }

 OpCase(i_global_copy):
 {
     BeamInstr *next;
     PreFetch(0,next);
#ifdef HYBRID
     if (!IS_CONST(r(0)))
     {
         BM_SWAP_TIMER(system,copy);
         SWAPOUT;
         reg[0] = r(0);
         reg[1] = NIL;
         r(0) = copy_struct_lazy(c_p,r(0),0);
         ASSERT(ma_src_top == 0);
         ASSERT(ma_dst_top == 0);
         ASSERT(ma_offset_top == 0);
         SWAPIN;
         BM_SWAP_TIMER(copy,system);
     }
     NextPF(0,next);
#else
     c_p->freason = EXC_INTERNAL_ERROR;
     goto find_func_info;
#endif
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

 OpCase(fmove_new_ld): {
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

 /*
  * Old allocating fmove.
  */


#ifdef NO_FPE_SIGNALS
     OpCase(fclearerror):
     OpCase(i_fcheckerror):
	 erl_exit(1, "fclearerror/i_fcheckerror without fpe signals (beam_emu)");
#else
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
#  undef ERTS_FP_CHECK_INIT
#  undef ERTS_FP_ERROR
#  define ERTS_FP_CHECK_INIT(p)
#  define ERTS_FP_ERROR(p, a, b)
#endif


 OpCase(i_fadd_lll): {
     BeamInstr *next;

     PreFetch(3, next);
     ERTS_FP_CHECK_INIT(c_p);
     fb(Arg(2)) = fb(Arg(0)) + fb(Arg(1));
     ERTS_FP_ERROR(c_p, fb(Arg(2)), goto fbadarith);
     NextPF(3, next);
 }
 OpCase(i_fsub_lll): {
     BeamInstr *next;

     PreFetch(3, next);
     ERTS_FP_CHECK_INIT(c_p);
     fb(Arg(2)) = fb(Arg(0)) - fb(Arg(1));
     ERTS_FP_ERROR(c_p, fb(Arg(2)), goto fbadarith);
     NextPF(3, next);
 }
 OpCase(i_fmul_lll): {
     BeamInstr *next;

     PreFetch(3, next);
     ERTS_FP_CHECK_INIT(c_p);
     fb(Arg(2)) = fb(Arg(0)) * fb(Arg(1));
     ERTS_FP_ERROR(c_p, fb(Arg(2)), goto fbadarith);
     NextPF(3, next);
 }
 OpCase(i_fdiv_lll): {
     BeamInstr *next;

     PreFetch(3, next);
     ERTS_FP_CHECK_INIT(c_p);
     fb(Arg(2)) = fb(Arg(0)) / fb(Arg(1));
     ERTS_FP_ERROR(c_p, fb(Arg(2)), goto fbadarith);
     NextPF(3, next);
 }
 OpCase(i_fnegate_ll): {
     BeamInstr *next;

     PreFetch(2, next);
     ERTS_FP_CHECK_INIT(c_p);
     fb(Arg(1)) = -fb(Arg(0));
     ERTS_FP_ERROR(c_p, fb(Arg(1)), goto fbadarith);
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
	 c_p->hipe.ncallee = (void(*)(void)) I[-4];
	 cmd = HIPE_MODE_SWITCH_CMD_CALL | (I[-1] << 8);
	 ++hipe_trap_count;
	 goto L_hipe_mode_switch;
     }
     OpCase(hipe_trap_call_closure): {
       ASSERT(I[-5] == (Uint) OpCode(i_func_info_IaaI));
       c_p->hipe.ncallee = (void(*)(void)) I[-4];
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
#ifdef ERTS_SMP
     reg = c_p->scheduler_data->save_reg;
     freg = c_p->scheduler_data->freg;
#endif
     ERL_BITS_RELOAD_STATEP(c_p);
     neg_o_reds = -c_p->def_arg_reg[4];
     FCALLS = c_p->fcalls;
     SWAPIN;
     switch( c_p->def_arg_reg[3] ) { /* XXX:PaN - Halfword wont work with hipe yet... */
       case HIPE_MODE_SWITCH_RES_RETURN:
	 ASSERT(is_value(reg[0]));
	 MoveReturn(reg[0], r(0));
       case HIPE_MODE_SWITCH_RES_CALL:
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
     erts_smp_proc_lock(c_p, ERTS_PROC_LOCK_STATUS);
     erts_add_to_runq(c_p);
     erts_smp_proc_unlock(c_p, ERTS_PROC_LOCK_STATUS);
     c_p->current = NULL;
     goto do_schedule;
 }

 OpCase(i_hibernate): {
     SWAPOUT;
     if (hibernate(c_p, r(0), x(1), x(2), reg)) {
	 goto do_schedule;
     } else {
	 I = handle_error(c_p, I, reg, hibernate_3);
	 goto post_error_handling;
     }
 }

 OpCase(i_debug_breakpoint): {
     SWAPOUT;
     reg[0] = r(0);
     tmp_arg1 = call_breakpoint_handler(c_p, I-3, reg);
     r(0) = reg[0];
     SWAPIN;
     if (tmp_arg1) {
	 SET_I(c_p->i);
	 Dispatch();
     }
     goto no_error_handler;
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
 OpCase(too_old_compiler):
 OpCase(on_load):
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

     /* Are tables correctly generated by beam_makeops? */
     ASSERT(sizeof(counting_opcodes) == sizeof(opcodes));

     if (count_instructions) {
#ifdef DEBUG
	 counting_opcodes[op_catch_end_y] = LabelAddr(lb_catch_end_y);
#endif
	 counting_opcodes[op_i_func_info_IaaI] = LabelAddr(lb_i_func_info_IaaI);
	 beam_ops = counting_opcodes;
     }
     else
#endif /* #ifndef ERTS_OPCODE_COUNTER_SUPPORT */
     {
	 beam_ops = opcodes;
     }
#endif /* NO_JUMP_TABLE */
     
     em_call_error_handler = OpCode(call_error_handler);
     em_call_traced_function = OpCode(call_traced_function);
     em_apply_bif = OpCode(apply_bif);

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
	 ((BeamInstr*)ep->code + 3)[-5] = (BeamInstr) BeamOp(op_i_func_info_IaaI);
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

	SET_I(((Export *) Arg(0))->address);

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
  am_notsup		/* 17 */
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
    /* Add a stacktrace if this is an error. */
    if (GET_EXC_CLASS(c_p->freason) == EXTAG_ERROR) {
        Value = add_stacktrace(c_p, Value, c_p->ftrace);
    }
    /* EXF_LOG is a primary exception flag */
    if (c_p->freason & EXF_LOG) {
	erts_dsprintf_buf_t *dsbufp = erts_create_logger_dsbuf();
	erts_dsprintf(dsbufp, "Error in process %T ", c_p->id);
	if (erts_is_alive)
	    erts_dsprintf(dsbufp, "on node %T ", erts_this_node->sysname);
	erts_dsprintf(dsbufp,"with exit value: %0.*T\n", display_items, Value);
	erts_send_error_to_logger(c_p->group_leader, dsbufp);
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
	    s->trace[s->depth++] = c_p->cp;
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
		s->trace[s->depth++] = c_p->cp;
		depth--;
	    }
	    s->pc = NULL; /* Ignore pc */
	} else {
	    if (depth > 0 && c_p->cp != 0 && c_p->cp != pc) {
		s->trace[s->depth++] = c_p->cp;
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
	ptr = c_p->stop;
	if (ptr < STACK_START(c_p) 
	    && (is_not_CP(*ptr)|| (*cp_val(*ptr) != i_return_trace &&
				   *cp_val(*ptr) != i_return_to_trace))
	    && c_p->cp) {
	    /* Can not follow cp here - code may be unloaded */
	    BeamInstr *cpp = c_p->cp;
	    if (cpp == beam_exception_trace || cpp == beam_return_trace) {
		/* Skip return_trace parameters */
		ptr += 2;
	    } else if (cpp == beam_return_to_trace) {
		/* Skip return_to_trace parameters */
		ptr += 1;
	    }
	}
	while (ptr < STACK_START(c_p) && depth > 0) {
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
			s->trace[s->depth++] = cp;
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
    BeamInstr* current;
#if HALFWORD_HEAP
    BeamInstr current_buff[3];
#endif
    Eterm  Where = NIL;
    Eterm *next_p = &Where;

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
	current = find_function_from_pc(s->pc);
    } else {
	current = s->current;
    }
    /*
     * If current is still NULL, default to the initial function
     * (e.g. spawn_link(erlang, abs, [1])).
     */
    if (current == NULL) {
#if HALFWORD_HEAP
	current = current_buff;
	current[0] = (BeamInstr) c_p->initial[0];
	current[1] = (BeamInstr) c_p->initial[1];
	current[2] = (BeamInstr) c_p->initial[2];
#else
	current = c_p->initial;
#endif
	args = am_true; /* Just in case */
    } else {
	args = get_args_from_exc(exc);
    }

    depth = s->depth;
    
    /*
     * Add the {M,F,A} for the current function 
     * (where A is arity or [Argument]).
     */
    {
	int i;
	Eterm mfa;
	Uint heap_size = 6*(depth+1);
	Eterm* hp = HAlloc(c_p, heap_size);
	Eterm* hp_end = hp + heap_size;

	if (args != am_true) {
	    /* We have an arglist - use it */
	    mfa = TUPLE3(hp, current[0], current[1], args);
	} else {
	    Eterm arity = make_small(current[2]);
	    mfa = TUPLE3(hp, current[0], current[1], arity);
	}
	hp += 4;
	ASSERT(*next_p == NIL);
	*next_p = CONS(hp, mfa, NIL);
	next_p = &CDR(list_val(*next_p));
	hp += 2;

	/* 
	 * Finally, we go through the saved continuation pointers.
	 */
	for (i = 0; i < depth; i++) {
	    BeamInstr *fi = find_function_from_pc((BeamInstr *) s->trace[i]);
	    if (fi == NULL) continue;
	    mfa = TUPLE3(hp, fi[0], fi[1], make_small(fi[2]));
	    hp += 4;
	    ASSERT(*next_p == NIL);
	    *next_p = CONS(hp, mfa, NIL);
	    next_p = &CDR(list_val(*next_p));
	    hp += 2;
	}
	ASSERT(hp <= hp_end);
	HRelease(c_p, hp_end, hp);
    }
    return Where;
}


static Eterm
call_error_handler(Process* p, BeamInstr* fi, Eterm* reg)
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
    ep = erts_find_function(erts_proc_get_error_handler(p),
			    am_undefined_function, 3);
    if (ep == NULL) {		/* No error handler */
	p->current = fi;
	p->freason = EXC_UNDEF;
	return 0;
    }
    p->i = ep->address;

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
     * Set up registers for call to error_handler:undefined_function/3.
     */
    reg[0] = fi[0];
    reg[1] = fi[1];
    reg[2] = args;
    return 1;
}

static Eterm
call_breakpoint_handler(Process* p, BeamInstr* fi, Eterm* reg)
{
    Eterm* hp;
    Export* ep;
    int arity;
    Eterm args;
    Uint sz;
    int i;

    /*
     * Search for error handler module.
     */
    ep = erts_find_function(erts_proc_get_error_handler(p),
			    am_breakpoint, 3);
    if (ep == NULL) {		/* No error handler */
	p->current = fi;
	p->freason = EXC_UNDEF;
	return 0;
    }
    p->i = ep->address;

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
     * Set up registers for call to error_handler:breakpoint/3.
     */
    reg[0] = fi[0];
    reg[1] = fi[1];
    reg[2] = args;
    return 1;
}



static Export*
apply_setup_error_handler(Process* p, Eterm module, Eterm function, Uint arity, Eterm* reg)
{
    Export* ep;

    /*
     * Find the export table index for the error handler. Return NULL if
     * there is no error handler module.
     */

    if ((ep = erts_find_export_entry(erts_proc_get_error_handler(p),
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

    if ((ep = erts_find_export_entry(module, function, arity)) == NULL) {
	if ((ep = apply_setup_error_handler(p, module, function, arity, reg)) == NULL) goto error;
    } else if (ERTS_PROC_GET_SAVED_CALLS_BUF(p)) {
	save_calls(p, ep);
    }

    return ep->address;
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

    if ((ep = erts_find_export_entry(module, function, arity)) == NULL) {
	if ((ep = apply_setup_error_handler(p, module, function, arity, reg)) == NULL)
	    goto error;
    } else if (ERTS_PROC_GET_SAVED_CALLS_BUF(p)) {
	save_calls(p, ep);
    }

    return ep->address;
}

static int
hibernate(Process* c_p, Eterm module, Eterm function, Eterm args, Eterm* reg)
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
    if (c_p->msg.len > 0) {
	erts_add_to_runq(c_p);
    } else {
	erts_smp_proc_unlock(c_p, ERTS_PROC_LOCK_MSGQ|ERTS_PROC_LOCK_STATUS);
	c_p->fvalue = NIL;
	PROCESS_MAIN_CHK_LOCKS(c_p);
	erts_garbage_collect_hibernate(c_p);
	PROCESS_MAIN_CHK_LOCKS(c_p);
	erts_smp_proc_lock(c_p, ERTS_PROC_LOCK_MSGQ|ERTS_PROC_LOCK_STATUS);
	ASSERT(!ERTS_PROC_IS_EXITING(c_p));
	c_p->status = P_WAITING;
#ifdef ERTS_SMP
	ERTS_SMP_MSGQ_MV_INQ2PRIVQ(c_p);
	if (c_p->msg.len > 0)
	    erts_add_to_runq(c_p);
#endif
    }
    erts_smp_proc_unlock(c_p, ERTS_PROC_LOCK_MSGQ|ERTS_PROC_LOCK_STATUS);
    c_p->current = bif_export[BIF_hibernate_3]->code;
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
    Eterm function;
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


		/*
		 * No arity. There is no module loaded that defines the fun,
		 * either because the fun is newly created from the external
		 * representation (the module has never been loaded),
		 * or the module defining the fun has been unloaded.
		 */

		module = fe->module;
		if ((modp = erts_get_module(module)) != NULL && modp->code != NULL) {
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
					am_undefined_lambda, 3);
		if (ep == NULL) {	/* No error handler */
		    p->current = NULL;
		    p->freason = EXC_UNDEF;
		    return NULL;
		}
		reg[0] = module;
		reg[1] = fun;
		reg[2] = args;
		return ep->address;
	    }
	}
    } else if (is_export_header(hdr)) {
	Export *ep;
	int actual_arity;

	ep = *((Export **) (export_val(fun) + 1));
	actual_arity = (int) ep->code[2];

	if (arity == actual_arity) {
	    return ep->address;
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
    } else if (hdr == make_arityval(2)) {
	Eterm* tp;
	Export* ep;
	Eterm module;

	tp = tuple_val(fun);
	module = tp[1];
	function = tp[2];
	if (!is_atom(module) || !is_atom(function)) {
	    goto badfun;
	}
	if ((ep = erts_find_export_entry(module, function, arity)) == NULL) {
	    ep = erts_find_export_entry(erts_proc_get_error_handler(p),
					am_undefined_function, 3);
	    if (ep == NULL) {
		p->freason = EXC_UNDEF;
		return 0;
	    }
	    if (is_non_value(args)) {
		Uint sz = 2 * arity;
		if (HeapWordsLeft(p) < sz) {
		    erts_garbage_collect(p, sz, reg, arity);
		}
		hp = HEAP_TOP(p);
		HEAP_TOP(p) += sz;
		args = NIL;
		while (arity-- > 0) {
		    args = CONS(hp, reg[arity], args);
		    hp += 2;
		}
	    }
	    reg[0] = module;
	    reg[1] = function;
	    reg[2] = args;
	}
	return ep->address;
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
	PROCESS_MAIN_CHK_LOCKS(p);
    }
    hp = p->htop;
    p->htop = hp + needed;
    funp = (ErlFunThing *) hp;
    hp = funp->env;
    erts_refc_inc(&fe->refc, 2);
    funp->thing_word = HEADER_FUN;
#ifndef HYBRID /* FIND ME! */
    funp->next = MSO(p).funs;
    MSO(p).funs = funp;
#endif
    funp->fe = fe;
    funp->num_free = num_free;
    funp->creator = p->id;
#ifdef HIPE
    funp->native_address = fe->native_address;
#endif
    funp->arity = (int)fe->address[-1] - num_free;
    for (i = 0; i < num_free; i++) {
	*hp++ = reg[i];
    }
    return make_fun(funp);
}



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
    return ep->address == ep->code+3 && (ep->code[3] == (BeamInstr) em_apply_bif);
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

