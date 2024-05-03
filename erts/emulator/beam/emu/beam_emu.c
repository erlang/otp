/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1996-2023. All Rights Reserved.
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
#include "beam_code.h"
#include "erl_binary.h"
#include "erl_map.h"
#include "erl_bits.h"
#include "dist.h"
#include "beam_bp.h"
#include "beam_catches.h"
#include "erl_thr_progress.h"
#include "erl_nfunc_sched.h"
#include "dtrace-wrapper.h"
#include "erl_proc_sig_queue.h"
#include "beam_common.h"

/* #define HARDDEBUG 1 */

/* The x86 and AMD64 optimization guides recommend that branch targets are
 * paragraph aligned (16 bytes). Neither GCC nor Clang align the addresses of
 * first class labels as used by the OpCase construction when the jump table is
 * enabled. Aligning the OpCase labels looks like a worthwhile optimization, but
 * despite the recommendation in the optimization guides, forcing alignment
 * leads to a ~5% slowdown of the emulator. Therefore alignment of OpCase labels
 * are not attempted.
 */

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

/*
 * Define macros for deep checking of terms.
 */

#define GET_EXPORT_MODULE(p)  ((p)->info.mfa.module)
#define GET_EXPORT_FUNCTION(p)  ((p)->info.mfa.function)
#define GET_EXPORT_ARITY(p)  ((p)->info.mfa.arity)

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

static BeamInstr beam_run_process_[1];
ErtsCodePtr beam_run_process;

static BeamInstr beam_normal_exit_[1];
ErtsCodePtr beam_normal_exit;

static BeamInstr beam_exit_[1];
ErtsCodePtr beam_exit;

static BeamInstr beam_continue_exit_[1];
ErtsCodePtr beam_continue_exit;


/* NOTE These should be the only variables containing trace instructions.
**      Sometimes tests are for the instruction value, and sometimes
**      for the referring variable (one of these), and rogue references
**      will most likely cause chaos.
*/

/* OpCode(i_return_to_trace) */
static BeamInstr beam_return_to_trace_[1];
ErtsCodePtr beam_return_to_trace;

/* OpCode(i_return_trace) */
static BeamInstr beam_return_trace_[1];
ErtsCodePtr beam_return_trace;

/* UGLY also OpCode(i_return_trace) */
static BeamInstr beam_exception_trace_[1];
ErtsCodePtr beam_exception_trace;

/* OpCode(i_call_trace_return) */
static BeamInstr beam_call_trace_return_[1];
ErtsCodePtr beam_call_trace_return;

/* The address field of every fun that has no loaded code will point to
 * beam_unloaded_fun[]. The -1 in beam_unloaded_fun[0] will be interpreted
 * as an illegal arity when attempting to call a fun. */
static BeamInstr unloaded_fun_code[4] = {NIL, NIL, -1, 0};
ErtsCodePtr beam_unloaded_fun = &unloaded_fun_code[3];

/*
 * All Beam instructions in numerical order.
 */

#ifndef NO_JUMP_TABLE
void** beam_ops;
#endif

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

/*
 * Use LIGHT_SWAPOUT when the called function
 * will call HeapOnlyAlloc() (and never HAlloc()).
 */
#ifdef DEBUG
#  /* The stack pointer is used in an assertion. */
#  define LIGHT_SWAPOUT SWAPOUT
#  define DEBUG_SWAPOUT SWAPOUT
#  define DEBUG_SWAPIN  SWAPIN
#else
#  define LIGHT_SWAPOUT HEAP_TOP(c_p) = HTOP
#  define DEBUG_SWAPOUT
#  define DEBUG_SWAPIN
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

#ifdef DEBUG
/* Better static type testing by the C compiler */
#  define BEAM_IS_TUPLE(Src) is_tuple(Src)
#else
/* Better performance */
# define BEAM_IS_TUPLE(Src) is_boxed(Src)
#endif

/*
 * process_main() is already huge, so we want to avoid inlining
 * seldom used functions into it.
 */
static void init_emulator_finish(void) ERTS_NOINLINE;

void
init_emulator(void)
{
    process_main(0);
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

/*
 * process_main() is called twice:
 * The first call performs some initialisation, including exporting
 * the instructions' C labels to the loader.
 * The second call starts execution of BEAM code. This call never returns.
 */
ERTS_NO_RETPOLINE
void process_main(ErtsSchedulerData *esdp)
{
    static int init_done = 0;
    Process* c_p = NULL;
    int reds_used;
#ifdef DEBUG
    ERTS_DECLARE_DUMMY(Eterm pid);
#endif

    /*
     * Pointer to X registers: x(0)..x(N).
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
    register const BeamInstr *I REG_I = NULL;

    /* Number of reductions left.  This function
     * returns to the scheduler when FCALLS reaches zero.
     */
    register Sint FCALLS REG_fcalls = 0;

    /*
     * X registers and floating point registers are located in
     * scheduler specific data.
     */
    register FloatDef *freg = NULL;

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

    Uint bitdata = 0;

    Uint64 start_time = 0;          /* Monitor long schedule */
    ErtsCodePtr start_time_i = NULL;

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

    reg = (esdp->registers)->x_reg_array.d;
    freg = (esdp->registers)->f_reg_array.d;

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
        check_monitor_long_schedule(c_p, start_time, start_time_i);
    }

    PROCESS_MAIN_CHK_LOCKS(c_p);
    ERTS_UNREQ_PROC_MAIN_LOCK(c_p);
    ERTS_VERIFY_UNUSED_TEMP_ALLOC(c_p);
    c_p = erts_schedule(NULL, c_p, reds_used);
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
	BeamInstr next;

        copy_in_registers(c_p, reg);

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
                const ErtsCodeMFA *cmfa = erts_find_function_from_pc(c_p->i);
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
    /*
     * The labels are jumped to from the $DISPATCH() macros when the reductions
     * are used up.
     *
     * Since the I register points just beyond the FuncBegin instruction, we
     * can get the module, function, and arity for the function being
     * called from I[-3], I[-2], and I[-1] respectively.
     */
 context_switch:
    {
        const ErtsCodeMFA *mfa = erts_code_to_codemfa(I);
        c_p->arity = mfa->arity;
        c_p->current = mfa;
    }

 context_switch3:

 {

     if (erts_atomic32_read_nob(&c_p->state) & ERTS_PSFLG_EXITING) {
         c_p->i = beam_exit;
         c_p->arity = 0;
         c_p->current = NULL;
         goto do_schedule;
     }

     /*
      * Since REDS_IN(c_p) is stored in the save area (c_p->arg_reg) we must read it
      * now before saving registers.
      */

     ASSERT(c_p->debug_reds_in == REDS_IN(c_p));
     if (!ERTS_PROC_GET_SAVED_CALLS_BUF(c_p))
         reds_used = REDS_IN(c_p) - FCALLS;
     else
         reds_used = REDS_IN(c_p) - (CONTEXT_REDS + FCALLS);
     ASSERT(reds_used >= 0);

     copy_out_registers(c_p, reg);

     SWAPOUT;
     c_p->i = I;
     goto do_schedule1;
 }

#include "beam_warm.h"

 OpCase(normal_exit): {
     HEAVY_SWAPOUT;
     c_p->freason = EXC_NORMAL;
     c_p->arity = 0; /* In case this process will ever be garbed again. */
     ERTS_UNREQ_PROC_MAIN_LOCK(c_p);
     erts_do_exit_process(c_p, am_normal);
     ERTS_REQ_PROC_MAIN_LOCK(c_p);
     HEAVY_SWAPIN;
     goto do_schedule;
 }

 OpCase(continue_exit): {
     HEAVY_SWAPOUT;
     ERTS_UNREQ_PROC_MAIN_LOCK(c_p);
     erts_continue_exit_process(c_p);
     ERTS_REQ_PROC_MAIN_LOCK(c_p);
     HEAVY_SWAPIN;
     goto do_schedule;
 }

 find_func_info: {
     SWAPOUT;
     I = handle_error(c_p, I, reg, NULL);
     goto post_error_handling;
 }

 OpCase(call_error_handler): {
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
        Export *error_handler;

        HEAVY_SWAPOUT;
        error_handler = call_error_handler(c_p, erts_code_to_codemfa(I),
                                           reg, am_undefined_function);
        HEAVY_SWAPIN;

        if (error_handler) {
            I = error_handler->dispatch.addresses[erts_active_code_ix()];
            Goto(*I);
        }
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
	 ASSERT(!is_value(x(0)));
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
 OpCase(i_nif_padding):
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
}

/*
 * Enter all BIFs into the export table.
 *
 * Note that they will all call the error_handler until their modules have been
 * loaded, which may prevent the system from booting if BIFs from non-preloaded
 * modules are apply/3'd while loading code. Ordinary BIF calls will work fine
 * however since they won't go through export entries.
 */
static void install_bifs(void) {
    int i;

    for (i = 0; i < BIF_SIZE; i++) {
        BifEntry *entry;
        Export *ep;
        int j;

        entry = &bif_table[i];

        ERTS_ASSERT(entry->arity <= MAX_BIF_ARITY);

        ep = erts_export_put(entry->module, entry->name, entry->arity);

        ep->info.u.op = BeamOpCodeAddr(op_i_func_info_IaaI);
        ep->info.mfa.module = entry->module;
        ep->info.mfa.function = entry->name;
        ep->info.mfa.arity = entry->arity;
        ep->bif_number = i;

        memset(&ep->trampoline, 0, sizeof(ep->trampoline));
        ep->trampoline.common.op = BeamOpCodeAddr(op_call_error_handler);

        for (j = 0; j < ERTS_NUM_CODE_IX; j++) {
            erts_activate_export_trampoline(ep, j);
        }

        /* Set up a hidden export entry so we can trap to this BIF without
         * it being seen when tracing. */
        erts_init_trap_export(BIF_TRAP_EXPORT(i),
                              entry->module, entry->name, entry->arity,
                              entry->f);
    }
}

/*
 * One-time initialization of emulator. Does not need to be
 * in process_main().
 */
static void
init_emulator_finish(void)
{
#if defined(ARCH_64) && defined(CODE_MODEL_SMALL)
    int i;

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

    beam_run_process_[0]       = BeamOpCodeAddr(op_i_apply_only);
    beam_run_process = (ErtsCodePtr)&beam_run_process_[0];

    beam_normal_exit_[0]       = BeamOpCodeAddr(op_normal_exit);
    beam_normal_exit = (ErtsCodePtr)&beam_normal_exit_[0];

    beam_exit_[0]              = BeamOpCodeAddr(op_error_action_code);
    beam_exit = (ErtsCodePtr)&beam_exit_[0];

    beam_continue_exit_[0]     = BeamOpCodeAddr(op_continue_exit);
    beam_continue_exit = (ErtsCodePtr)&beam_continue_exit_[0];

    beam_return_to_trace_[0]   = BeamOpCodeAddr(op_i_return_to_trace);
    beam_return_to_trace = (ErtsCodePtr)&beam_return_to_trace_[0];

    beam_return_trace_[0]      = BeamOpCodeAddr(op_return_trace);
    beam_return_trace = (ErtsCodePtr)&beam_return_trace_[0];

    beam_exception_trace_[0]   = BeamOpCodeAddr(op_return_trace); /* UGLY */
    beam_exception_trace = (ErtsCodePtr)&beam_exception_trace_[0];

    beam_call_trace_return_[0] = BeamOpCodeAddr(op_i_call_trace_return);
    beam_call_trace_return = (ErtsCodePtr)&beam_call_trace_return_[0];

    install_bifs();
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

void
erts_prepare_bs_construct_fail_info(Process* c_p, const BeamInstr* p, Eterm reason, Eterm Info, Eterm value)
{
    Eterm* hp;
    Eterm cause_tuple;
    Eterm op;
    Eterm error_info;
    Uint segment;

    segment = p[2] >> 3;

    switch (p[0]) {
    case BSC_APPEND:
    case BSC_PRIVATE_APPEND:
    case BSC_BINARY:
    case BSC_BINARY_FIXED_SIZE:
    case BSC_BINARY_ALL:
        op = am_binary;
        break;
    case BSC_FLOAT:
    case BSC_FLOAT_FIXED_SIZE:
        op = am_float;
        break;
    case BSC_INTEGER:
    case BSC_INTEGER_FIXED_SIZE:
        op = am_integer;
        break;
    case BSC_STRING:
        op = am_string;
        break;
    case BSC_UTF8:
        op = am_utf8;
        break;
    case BSC_UTF16:
        op = am_utf16;
        break;
    case BSC_UTF32:
        op = am_utf32;
        break;
    default:
        op = am_none;
        break;
    }

    hp = HeapFragOnlyAlloc(c_p, MAP3_SZ+4+1);
    cause_tuple = TUPLE4(hp, make_small(segment), op, Info, value);
    hp += 5;
    error_info = MAP3(hp,
                      am_cause, cause_tuple,
                      am_function, am_format_bs_fail,
                      am_module, am_erl_erts_errors);
    c_p->fvalue = error_info;
    c_p->freason = reason | EXF_HAS_EXT_INFO;
}
