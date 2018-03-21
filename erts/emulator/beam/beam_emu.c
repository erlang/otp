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
#include "erl_proc_sig_queue.h"

/* #define HARDDEBUG 1 */

#if defined(NO_JUMP_TABLE)
#  define OpCase(OpCode)    case op_##OpCode
#  define CountCase(OpCode) case op_count_##OpCode
#  define IsOpCode(InstrWord, OpCode)  (BeamCodeAddr(InstrWord) == (BeamInstr)op_##OpCode)
#  define Goto(Rel)         {Go = BeamCodeAddr(Rel); goto emulator_loop;}
#  define GotoPF(Rel)       Goto(Rel)
#else
#  define OpCase(OpCode)    lb_##OpCode
#  define CountCase(OpCode) lb_count_##OpCode
#  define IsOpCode(InstrWord, OpCode)  (BeamCodeAddr(InstrWord) == (BeamInstr)&&lb_##OpCode)
#  define Goto(Rel)         goto *((void *)BeamCodeAddr(Rel))
#  define GotoPF(Rel)       goto *((void *)Rel)
#  define LabelAddr(Label)  &&Label
#endif

#ifdef ERTS_ENABLE_LOCK_CHECK
#    define PROCESS_MAIN_CHK_LOCKS(P)                   \
do {                                                    \
    if ((P))                                            \
	erts_proc_lc_chk_only_proc_main((P));           \
    ERTS_LC_ASSERT(!erts_thr_progress_is_blocking());   \
} while (0)
#    define ERTS_REQ_PROC_MAIN_LOCK(P)				\
do {                                                            \
    if ((P))                                                    \
	erts_proc_lc_require_lock((P), ERTS_PROC_LOCK_MAIN,     \
				  __FILE__, __LINE__);          \
} while (0)
#    define ERTS_UNREQ_PROC_MAIN_LOCK(P)				\
do {									\
    if ((P))								\
	erts_proc_lc_unrequire_lock((P), ERTS_PROC_LOCK_MAIN);		\
} while (0)
#else
#  define PROCESS_MAIN_CHK_LOCKS(P)
#  define ERTS_REQ_PROC_MAIN_LOCK(P)
#  define ERTS_UNREQ_PROC_MAIN_LOCK(P)
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

#define CHECK_ALIGNED(Dst) ASSERT((((Uint)&Dst) & (sizeof(Uint)-1)) == 0)

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
#  define VALID_INSTR(IP) (BeamCodeAddr(IP) < (NUMBER_OF_OPCODES*2+10))
#else
#  define VALID_INSTR(IP) \
    ((BeamInstr)LabelAddr(emulator_loop) <= BeamCodeAddr(IP) && \
     BeamCodeAddr(IP) < (BeamInstr)LabelAddr(end_emulator_loop))
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

#define REG_TARGET_PTR(Target) (((Target) & 1) ? &yb((Target)-1) : &xb(Target))

/*
 * Special Beam instructions.
 */

BeamInstr beam_apply[2];
BeamInstr beam_exit[1];
BeamInstr beam_continue_exit[1];


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
     ERTS_UNREQ_PROC_MAIN_LOCK((P))

#define db(N) (N)
#define fb(N) ((Sint)(Sint32)(N))
#define jb(N) ((Sint)(Sint32)(N))
#define tb(N) (N)
#define xb(N) (*ADD_BYTE_OFFSET(reg, N))
#define yb(N) (*ADD_BYTE_OFFSET(E, N))
#define Sb(N) (*REG_TARGET_PTR(N))
#define lb(N) (*(double *) (((unsigned char *)&(freg[0].fd)) + (N)))
#define Qb(N) (N)
#define Ib(N) (N)

#define x(N) reg[N]
#define y(N) E[N]
#define r(N) x(N)
#define Q(N) (N*sizeof(Eterm *))
#define l(N) (freg[N].fd)

/*
 * Check that we haven't used the reductions and jump to function pointed to by
 * the I register.  If we are out of reductions, do a context switch.
 */

#define DispatchMacro()				\
  do {						\
     BeamInstr dis_next;                        \
     dis_next = *I;                             \
     CHECK_ARGS(I);				\
     if (FCALLS > 0 || FCALLS > neg_o_reds) {	\
        FCALLS--;				\
        Goto(dis_next);				\
     } else {					\
	goto context_switch;			\
     }						\
 } while (0)                                    \

#define DispatchMacroFun()			\
  do {						\
     BeamInstr dis_next;                        \
     dis_next = *I;                             \
     CHECK_ARGS(I);				\
     if (FCALLS > 0 || FCALLS > neg_o_reds) {	\
        FCALLS--;				\
        Goto(dis_next);				\
     } else {					\
	goto context_switch_fun;		\
     }						\
 } while (0)

#define DispatchMacrox()                                                \
  do {                                                                  \
     if (FCALLS > 0) {                                                  \
        BeamInstr dis_next;                                             \
        SET_I(((Export *) Arg(0))->addressv[erts_active_code_ix()]);    \
        dis_next = *I;                                                  \
        FCALLS--;                                                       \
        CHECK_ARGS(I);                                                  \
        Goto(dis_next);                                                 \
     } else if (ERTS_PROC_GET_SAVED_CALLS_BUF(c_p)                      \
		&& FCALLS > neg_o_reds) {                               \
        goto save_calls1;                                               \
     } else {                                                           \
        SET_I(((Export *) Arg(0))->addressv[erts_active_code_ix()]);    \
        CHECK_ARGS(I);                                                  \
	goto context_switch;                                            \
     }                                                                  \
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

#define Arg(N)       I[(N)+1]

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

#ifdef DEBUG
/* Better static type testing by the C compiler */
#  define BEAM_IS_TUPLE(Src) is_tuple(Src)
#else
/* Better performance */
# define BEAM_IS_TUPLE(Src) is_boxed(Src)
#endif

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
static void init_emulator_finish(void) NOINLINE;
static ErtsCodeMFA *ubif2mfa(void* uf) NOINLINE;
static ErtsCodeMFA *gcbif2mfa(void* gcf) NOINLINE;
static BeamInstr* handle_error(Process* c_p, BeamInstr* pc,
			       Eterm* reg, ErtsCodeMFA* bif_mfa) NOINLINE;
static BeamInstr* call_error_handler(Process* p, ErtsCodeMFA* mfa,
				     Eterm* reg, Eterm func) NOINLINE;
static BeamInstr* fixed_apply(Process* p, Eterm* reg, Uint arity,
			      BeamInstr *I, Uint offs) NOINLINE;
static BeamInstr* apply(Process* p, Eterm* reg,
                        BeamInstr *I, Uint offs) NOINLINE;
static BeamInstr* call_fun(Process* p, int arity,
			   Eterm* reg, Eterm args) NOINLINE;
static BeamInstr* apply_fun(Process* p, Eterm fun,
			    Eterm args, Eterm* reg) NOINLINE;
static Eterm new_fun(Process* p, Eterm* reg,
		     ErlFunEntry* fe, int num_free) NOINLINE;
static Eterm erts_gc_new_map(Process* p, Eterm* reg, Uint live,
                             Uint n, BeamInstr* ptr) NOINLINE;
static Eterm erts_gc_new_small_map_lit(Process* p, Eterm* reg, Eterm keys_literal,
                               Uint live, BeamInstr* ptr) NOINLINE;
static Eterm erts_gc_update_map_assoc(Process* p, Eterm* reg, Uint live,
                              Uint n, BeamInstr* new_p) NOINLINE;
static Eterm erts_gc_update_map_exact(Process* p, Eterm* reg, Uint live,
                              Uint n, Eterm* new_p) NOINLINE;
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
#elif defined(__GNUC__) && defined(__amd64__) && !defined(DEBUG)
#  define REG_xregs asm("%r12")
#  define REG_htop
#  define REG_stop asm("%r13")
#  define REG_I asm("%rbx")
#  define REG_fcalls asm("%r14")
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

#ifdef NO_FPE_SIGNALS
#  define ERTS_NO_FPE_CHECK_INIT ERTS_FP_CHECK_INIT
#  define ERTS_NO_FPE_ERROR ERTS_FP_ERROR
#else
#  define ERTS_NO_FPE_CHECK_INIT(p)
#  define ERTS_NO_FPE_ERROR(p, a, b)
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
    register BeamInstr Go;
#endif
#endif

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
    if (ERTS_UNLIKELY(!init_done)) {
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
    ERTS_UNREQ_PROC_MAIN_LOCK(c_p);
    ERTS_VERIFY_UNUSED_TEMP_ALLOC(c_p);
    c_p = erts_schedule(NULL, c_p, reds_used);
    ASSERT(!(c_p->flags & F_HIPE_MODE));
    ERTS_VERIFY_UNUSED_TEMP_ALLOC(c_p);
    start_time = 0;
#ifdef DEBUG
    pid = c_p->common.id; /* Save for debugging purposes */
#endif
    ERTS_REQ_PROC_MAIN_LOCK(c_p);
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
	BeamInstr next;
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

	next = *I;
	SWAPIN;
	ASSERT(VALID_INSTR(next));

#ifdef USE_VM_PROBES
        if (DTRACE_ENABLED(process_scheduled)) {
            DTRACE_CHARBUF(process_buf, DTRACE_TERM_BUF_SIZE);
            DTRACE_CHARBUF(fun_buf, DTRACE_TERM_BUF_SIZE);
            dtrace_proc_str(c_p, process_buf);

            if (ERTS_PROC_IS_EXITING(c_p)) {
                sys_strcpy(fun_buf, "<exiting>");
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

     if (erts_atomic32_read_nob(&c_p->state) & ERTS_PSFLG_EXITING) {
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

#include "beam_warm.h"

 OpCase(normal_exit): {
     SWAPOUT;
     c_p->freason = EXC_NORMAL;
     c_p->arity = 0; /* In case this process will ever be garbed again. */
     ERTS_UNREQ_PROC_MAIN_LOCK(c_p);
     erts_do_exit_process(c_p, am_normal);
     ERTS_REQ_PROC_MAIN_LOCK(c_p);
     goto do_schedule;
 }

 OpCase(continue_exit): {
     ERTS_UNREQ_PROC_MAIN_LOCK(c_p);
     erts_continue_exit_process(c_p);
     ERTS_REQ_PROC_MAIN_LOCK(c_p);
     goto do_schedule;
 }

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

 OpCase(i_func_info_IaaI): {
     ErtsCodeInfo *ci = (ErtsCodeInfo*)I;
     c_p->freason = EXC_FUNCTION_CLAUSE;
     c_p->current = &ci->mfa;
     goto handle_error;
 }

#include "beam_cold.h"

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

     init_emulator_finish();
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
	BeamInstr dis_next;

	save_calls(c_p, (Export *) Arg(0));

	SET_I(((Export *) Arg(0))->addressv[erts_active_code_ix()]);

	dis_next = *I;
	FCALLS--;
	Goto(dis_next);
    }
}

/*
 * One-time initialization of emulator. Does not need to be
 * in process_main().
 */
static void
init_emulator_finish(void)
{
     int i;
     Export* ep;

#if defined(ARCH_64) && defined(CODE_MODEL_SMALL)
     for (i = 0; i < NUMBER_OF_OPCODES; i++) {
         BeamInstr instr = BeamOpCodeAddr(i);
         if (instr >= (1ull << 32)) {
             erts_exit(ERTS_ERROR_EXIT,
                       "This run-time was supposed be compiled with all code below 2Gb,\n"
                       "but the instruction '%s' is located at %016lx.\n",
                       opc[i].name, instr);
         }
     }
#endif

     beam_apply[0]             = BeamOpCodeAddr(op_i_apply);
     beam_apply[1]             = BeamOpCodeAddr(op_normal_exit);
     beam_exit[0]              = BeamOpCodeAddr(op_error_action_code);
     beam_continue_exit[0]     = BeamOpCodeAddr(op_continue_exit);
     beam_return_to_trace[0]   = BeamOpCodeAddr(op_i_return_to_trace);
     beam_return_trace[0]      = BeamOpCodeAddr(op_return_trace);
     beam_exception_trace[0]   = BeamOpCodeAddr(op_return_trace); /* UGLY */
     beam_return_time_trace[0] = BeamOpCodeAddr(op_i_return_time_trace);

     /*
      * Enter all BIFs into the export table.
      */
     for (i = 0; i < BIF_SIZE; i++) {
	 ep = erts_export_put(bif_table[i].module,
			      bif_table[i].name,
			      bif_table[i].arity);
	 bif_export[i] = ep;
	 ep->beam[0] = BeamOpCodeAddr(op_apply_bif);
	 ep->beam[1] = (BeamInstr) bif_table[i].f;
	 /* XXX: set func info for bifs */
	 ep->info.op = BeamOpCodeAddr(op_i_func_info_IaaI);
     }
}

/*
 * erts_dirty_process_main() is what dirty schedulers execute. Since they handle
 * only NIF calls they do not need to be able to execute all BEAM
 * instructions.
 */
void erts_dirty_process_main(ErtsSchedulerData *esdp)
{
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
	ERTS_UNREQ_PROC_MAIN_LOCK(c_p);
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
    ERTS_REQ_PROC_MAIN_LOCK(c_p);
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

    if (erts_atomic32_read_nob(&c_p->state) & ERTS_PSFLG_DIRTY_RUNNING_SYS) {
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
                sys_strcpy(fun_buf, "<exiting>");
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
	ERTS_UNREQ_PROC_MAIN_LOCK(c_p);

	ASSERT(!ERTS_PROC_IS_EXITING(c_p));
	if (BeamIsOpCode(*I, op_apply_bif)) {
	    exiting = erts_call_dirty_bif(esdp, c_p, I, reg);
	}
	else {
	    ASSERT(BeamIsOpCode(*I, op_call_nif));
            exiting = erts_call_dirty_nif(esdp, c_p, I, reg);
	}

	ASSERT(!(c_p->flags & F_HIBERNATE_SCHED));

	PROCESS_MAIN_CHK_LOCKS(c_p);
	ERTS_REQ_PROC_MAIN_LOCK(c_p);
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
            ERTS_RECV_MARK_CLEAR(c_p); /* No longer safe to use this position */
	    return new_pc;
	}
	if (c_p->catches > 0) erts_exit(ERTS_ERROR_EXIT, "Catch not found");
    }
    ERTS_UNREQ_PROC_MAIN_LOCK(c_p);
    terminate_proc(c_p, Value);
    ERTS_REQ_PROC_MAIN_LOCK(c_p);
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
	&& BeamIsOpCode(ep->beam[0], op_apply_bif)
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
apply(Process* p, Eterm* reg, BeamInstr *I, Uint stack_offset)
{
    int arity;
    Export* ep;
    Eterm tmp;
    Eterm module = reg[0];
    Eterm function = reg[1];
    Eterm args = reg[2];

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

	if (is_not_atom(module)) goto error;

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
     * the parameters to the x registers (reg[]).
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

    if (is_not_atom(module)) goto error;

    /* Handle apply of apply/3... */
    if (module == am_erlang && function == am_apply && arity == 3) {
	return apply(p, reg, I, stack_offset);
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
    apply_bif_error_adjustment(p, ep, reg, arity, I, stack_offset);
    DTRACE_GLOBAL_CALL_FROM_EXPORT(p, ep);
    return ep->addressv[erts_active_code_ix()];
}

int
erts_hibernate(Process* c_p, Eterm* reg)
{
    int arity;
    Eterm tmp;
    Eterm module = reg[0];
    Eterm function = reg[1];
    Eterm args = reg[2];

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
    erts_proc_lock(c_p, ERTS_PROC_LOCK_MSGQ|ERTS_PROC_LOCK_STATUS);
    erts_proc_sig_fetch(c_p);
    if (!c_p->sig_qs.len) {
	erts_proc_unlock(c_p, ERTS_PROC_LOCK_MSGQ|ERTS_PROC_LOCK_STATUS);
	c_p->fvalue = NIL;
	PROCESS_MAIN_CHK_LOCKS(c_p);
	erts_garbage_collect_hibernate(c_p);
	ERTS_VERIFY_UNUSED_TEMP_ALLOC(c_p);
	PROCESS_MAIN_CHK_LOCKS(c_p);
	erts_proc_lock(c_p, ERTS_PROC_LOCK_MSGQ|ERTS_PROC_LOCK_STATUS);
        erts_proc_sig_fetch(c_p);
	if (!c_p->sig_qs.len)
	    erts_atomic32_read_band_relb(&c_p->state, ~ERTS_PSFLG_ACTIVE);
	ASSERT(!ERTS_PROC_IS_EXITING(c_p));
    }
    erts_proc_unlock(c_p, ERTS_PROC_LOCK_MSGQ|ERTS_PROC_LOCK_STATUS);
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

		ERTS_THR_READ_MEMORY_BARRIER;
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
	p->freason = EXC_BADARG;
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
erts_gc_new_map(Process* p, Eterm* reg, Uint live, Uint n, BeamInstr* ptr)
{
    Uint i;
    Uint need = n + 1 /* hdr */ + 1 /*size*/ + 1 /* ptr */ + 1 /* arity */;
    Eterm keys;
    Eterm *mhp,*thp;
    Eterm *E;
    flatmap_t *mp;
    ErtsHeapFactory factory;

    if (n > 2*MAP_SMALL_MAP_LIMIT) {
        Eterm res;
	if (HeapWordsLeft(p) < n) {
	    erts_garbage_collect(p, n, reg, live);
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
	erts_garbage_collect(p, need, reg, live);
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
erts_gc_new_small_map_lit(Process* p, Eterm* reg, Eterm keys_literal,
                          Uint live, BeamInstr* ptr)
{
    Eterm* keys = tuple_val(keys_literal);
    Uint n = arityval(*keys);
    Uint need = n + 1 /* hdr */ + 1 /*size*/ + 1 /* ptr */ + 1 /* arity */;
    Uint i;
    flatmap_t *mp;
    Eterm *mhp;
    Eterm *E;

    ASSERT(n <= MAP_SMALL_MAP_LIMIT);

    if (HeapWordsLeft(p) < need) {
        erts_garbage_collect(p, need, reg, live);
    }

    mhp = p->htop;
    E   = p->stop;

    mp = (flatmap_t *)mhp; mhp += MAP_HEADER_FLATMAP_SZ;
    mp->thing_word = MAP_HEADER_FLATMAP;
    mp->size = n;
    mp->keys = keys_literal;

    for (i = 0; i < n; i++) {
        GET_TERM(*ptr++, *mhp++);
    }

    p->htop = mhp;

    return make_flatmap(mp);
}

static Eterm
erts_gc_update_map_assoc(Process* p, Eterm* reg, Uint live,
                         Uint n, BeamInstr* new_p)
{
    Uint num_old;
    Uint num_updates;
    Uint need;
    flatmap_t *old_mp, *mp;
    Eterm res;
    Eterm* hp;
    Eterm* E;
    Eterm* old_keys;
    Eterm* old_vals;
    Eterm new_key;
    Eterm* kp;
    Eterm map;

    num_updates = n / 2;
    map = reg[live];

    if (is_not_flatmap(map)) {
	Uint32 hx;
	Eterm val;

	ASSERT(is_hashmap(map));
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
	return erts_gc_new_map(p, reg, live, n, new_p);
    }

    /*
     * Allocate heap space for the worst case (i.e. all keys in the
     * update list are new).
     */

    need = 2*(num_old+num_updates) + 1 + MAP_HEADER_FLATMAP_SZ;
    if (HeapWordsLeft(p) < need) {
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
erts_gc_update_map_exact(Process* p, Eterm* reg, Uint live, Uint n, Eterm* new_p)
{
    Uint i;
    Uint num_old;
    Uint need;
    flatmap_t *old_mp, *mp;
    Eterm res;
    Eterm* hp;
    Eterm* E;
    Eterm* old_keys;
    Eterm* old_vals;
    Eterm new_key;
    Eterm map;

    n /= 2;		/* Number of values to be updated */
    ASSERT(n > 0);
    map = reg[live];

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

    if (Mod == am_erlang) {
        /*
         * Special case for built-in functions that are implemented
         * as instructions as opposed to SNIFs.
         */
        if (Name == am_apply && (arity == 2 || arity == 3)) {
            return 1;
        } else if (Name == am_yield && arity == 0) {
            return 1;
        }
    }

    e.info.mfa.module = Mod;
    e.info.mfa.function = Name;
    e.info.mfa.arity = arity;

    if ((ep = export_get(&e)) == NULL) {
	return 0;
    }
    return ep->addressv[erts_active_code_ix()] == ep->beam &&
	BeamIsOpCode(ep->beam[0], op_apply_bif);
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
