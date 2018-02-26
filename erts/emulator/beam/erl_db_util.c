/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1998-2018. All Rights Reserved.
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

/*
 * Common utilities for the different types of db tables.
 * Mostly matching etc.
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif
#include "sys.h"
#include "erl_vm.h"
#include "global.h"
#include "erl_process.h"
#include "error.h"
#define ERTS_WANT_DB_INTERNAL__
#include "erl_db.h"
#include "bif.h"
#include "big.h"
#include "erl_binary.h"
#include "erl_map.h"
#include "erl_thr_progress.h"
#include "erl_proc_sig_queue.h"

#include "erl_db_util.h"


/*
** Flags for the guard bif's
*/

/* These are offsets from the DCOMP_* value */
#define DBIF_GUARD 1
#define DBIF_BODY  0

/* These are the DBIF flag bits corresponding to the DCOMP_* value.
 * If a bit is set, the BIF is allowed in that context. */
#define DBIF_TABLE_GUARD (1 << (DCOMP_TABLE + DBIF_GUARD))
#define DBIF_TABLE_BODY  (1 << (DCOMP_TABLE + DBIF_BODY))
#define DBIF_TRACE_GUARD (1 << (DCOMP_TRACE + DBIF_GUARD))
#define DBIF_TRACE_BODY  (1 << (DCOMP_TRACE + DBIF_BODY))
#define DBIF_ALL \
DBIF_TABLE_GUARD | DBIF_TABLE_BODY | DBIF_TRACE_GUARD | DBIF_TRACE_BODY


#define HEAP_XTRA 100

/*
** Some convenience macros for stacks (DMC == db_match_compile)
*/

#define DMC_DEFAULT_SIZE 25

#define DMC_STACK_TYPE(Type) DMC_##Type##_stack

#define DMC_DECLARE_STACK_TYPE(Type)            \
typedef struct DMC_STACK_TYPE(Type) {		\
    int pos;					\
    int siz;					\
    int bytes;                                  \
    Type *data;					\
    Type def[DMC_DEFAULT_SIZE];		        \
} DMC_STACK_TYPE(Type)


typedef int Dummy;
DMC_DECLARE_STACK_TYPE(Dummy);

static void dmc_stack_grow(DMC_Dummy_stack* s)
{
    int was_bytes = s->bytes;
    s->siz *= 2;
    s->bytes *= 2;
    if (s->data == s->def) {
        s->data = erts_alloc(ERTS_ALC_T_DB_MC_STK, s->bytes);
        sys_memcpy(s->data, s->def, was_bytes);
    }
    else {
        s->data = erts_realloc(ERTS_ALC_T_DB_MC_STK, s->data, s->bytes);
    }
}
    
#define DMC_INIT_STACK(Name) do {       \
    (Name).pos = 0;                     \
    (Name).siz = DMC_DEFAULT_SIZE;      \
    (Name).bytes = sizeof((Name).def);  \
    (Name).data = (Name).def;           \
} while (0)

#define DMC_STACK_DATA(Name) (Name).data

#define DMC_STACK_NUM(Name) (Name).pos

#define DMC_PUSH(On, What)						\
do {									\
    if ((On).pos >= (On).siz)  						\
        dmc_stack_grow((DMC_Dummy_stack*)&(On));                        \
    (On).data[(On).pos++] = What;					\
} while (0)

#define DMC_PUSH2(On, A, B)						\
do {									\
    if ((On).pos+1 >= (On).siz)  					\
        dmc_stack_grow((DMC_Dummy_stack*)&(On));                        \
    (On).data[(On).pos++] = A;					        \
    (On).data[(On).pos++] = B;					        \
} while (0)

#define DMC_POP(From) (From).data[--(From).pos]

#define DMC_TOP(From) (From).data[(From).pos - 1]

#define DMC_EMPTY(Name) ((Name).pos == 0)

#define DMC_PEEK(On, At) (On).data[At]     

#define DMC_POKE(On, At, Value) ((On).data[At] = (Value))

#define DMC_CLEAR(Name) (Name).pos = 0

#define DMC_FREE(Name)							\
do {									\
    if ((Name).def != (Name).data)					\
	erts_free(ERTS_ALC_T_DB_MC_STK, (Name).data);			\
} while (0)


#define TermWords(t) (((t) / (sizeof(UWord)/sizeof(Eterm))) + !!((t) % (sizeof(UWord)/sizeof(Eterm))))

#define add_dmc_err(EINFO, STR, VAR, TERM, SEV) \
       vadd_dmc_err(EINFO, SEV, VAR, STR, TERM)


static ERTS_INLINE Process *
get_proc(Process *cp, Uint32 cp_locks, Eterm id, Uint32 id_locks)
{
    Process *proc = erts_pid2proc(cp, cp_locks, id, id_locks);
    if (!proc && is_atom(id))
	proc = erts_whereis_process(cp, cp_locks, id, id_locks, 0);
    return proc;
}


static Eterm
set_tracee_flags(Process *tracee_p, ErtsTracer tracer,
                 Uint d_flags, Uint e_flags) {
    Eterm ret;
    Uint flags;

    if (ERTS_TRACER_IS_NIL(tracer)) {
	flags = ERTS_TRACE_FLAGS(tracee_p) & ~TRACEE_FLAGS;
    }  else {
	flags = ((ERTS_TRACE_FLAGS(tracee_p) & ~d_flags) | e_flags);
	if (! flags) tracer = erts_tracer_nil;
    }
    ret = ((!ERTS_TRACER_COMPARE(ERTS_TRACER(tracee_p),tracer)
	    || ERTS_TRACE_FLAGS(tracee_p) != flags)
	   ? am_true
	   : am_false);
    erts_tracer_replace(&tracee_p->common, tracer);
    ERTS_TRACE_FLAGS(tracee_p) = flags;

    return ret;
}
/*
** Assuming all locks on tracee_p on entry
**
** Changes ERTS_TRACE_FLAGS(tracee_p) and ERTS_TRACER_PROC(tracee_p)
** according to input disable/enable flags and tracer.
**
** Returns am_true|am_false on success, am_true if value changed,
** returns fail_term on failure. Fails if tracer pid or port is invalid.
*/
static Eterm 
set_match_trace(Process *tracee_p, Eterm fail_term, ErtsTracer tracer,
		Uint d_flags, Uint e_flags) {

    ERTS_LC_ASSERT(
        ERTS_PROC_LOCKS_ALL == erts_proc_lc_my_proc_locks(tracee_p)
        || erts_thr_progress_is_blocking());

    if (ERTS_TRACER_IS_NIL(tracer)
        || erts_is_tracer_enabled(tracer, &tracee_p->common))
        return set_tracee_flags(tracee_p, tracer, d_flags, e_flags);
    return fail_term;
}

/*
**
** Types and enum's (compiled matches)
**
*/

/*
** match VM instructions
*/
typedef enum {
    matchArray, /* Only when parameter is an array (DCOMP_TRACE) */
    matchArrayBind, /* ------------- " ------------ */
    matchTuple,
    matchPushT,
    matchPushL,
    matchPushM,
    matchPop,
    matchSwap,
    matchBind,
    matchCmp,
    matchEqBin,
    matchEqFloat,
    matchEqBig,
    matchEqRef,
    matchEq,
    matchList,
    matchMap,
    matchKey,
    matchSkip,
    matchPushC,
    matchConsA, /* Car is below Cdr */
    matchConsB, /* Cdr is below Car (unusual) */
    matchMkTuple,
    matchMkFlatMap,
    matchMkHashMap,
    matchCall0,
    matchCall1,
    matchCall2,
    matchCall3,
    matchPushV,
    matchPushVResult, /* First variable reference in result */
    matchPushExpr, /* Push the whole expression we're matching ('$_') */
    matchPushArrayAsList, /* Only when parameter is an Array and 
			     not an erlang term  (DCOMP_TRACE) */
    matchPushArrayAsListU, /* As above but unknown size */
    matchTrue,
    matchOr,
    matchAnd,
    matchOrElse,
    matchAndAlso,
    matchJump,
    matchSelf,
    matchWaste,
    matchReturn,
    matchProcessDump,
    matchDisplay,
    matchIsSeqTrace,
    matchSetSeqToken,
    matchGetSeqToken,
    matchSetReturnTrace,
    matchSetExceptionTrace,
    matchCatch,
    matchEnableTrace,
    matchDisableTrace,
    matchEnableTrace2,
    matchDisableTrace2,
    matchTryMeElse,
    matchCaller,
    matchHalt,
    matchSilent,
    matchSetSeqTokenFake,
    matchTrace2,
    matchTrace3
} MatchOps;

/*
** Guard bif's
*/

typedef struct dmc_guard_bif {
    Eterm name; /* atom */
    void *biff;
    /*    BIF_RETTYPE (*biff)(); */
    int arity;
    Uint32 flags;
} DMCGuardBif; 

/*
** Error information (for lint)
*/

/*
** Type declarations for stacks
*/
DMC_DECLARE_STACK_TYPE(Eterm);

DMC_DECLARE_STACK_TYPE(UWord);

DMC_DECLARE_STACK_TYPE(unsigned);

/*
** Data about the heap during compilation
*/

typedef struct DMCVariable {
    int is_bound;
    int is_in_body;
} DMCVariable;

typedef struct DMCHeap {
    int size;
    DMCVariable vars_def[DMC_DEFAULT_SIZE];
    DMCVariable* vars;
    int vars_used;
} DMCHeap;

/*
** Return values from sub compilation steps (guard compilation)
*/

typedef enum dmc_ret { 
    retOk, 
    retFail, 
    retRestart 
} DMCRet; 

/*
** Diverse context information
*/

typedef struct dmc_context {
    int stack_need;
    int stack_used;
    ErlHeapFragment *save;
    ErlHeapFragment *copy;
    Eterm *matchexpr;
    Eterm *guardexpr;
    Eterm *bodyexpr;
    int num_match;
    int current_match;
    Uint cflags;
    int is_guard; /* 1 if in guard, 0 if in body */
    int special; /* 1 if the head in the match was a single expression */ 
    DMCErrInfo *err_info;
} DMCContext;

/*
**
** Global variables 
**
*/

/*
** Internal
*/

/* 
** The pseudo process used by the VM (pam).
*/

#define ERTS_DEFAULT_MS_HEAP_SIZE 128

/* Runtime info about a $-variable
*/
typedef struct MatchVariable {
    Eterm term;
#ifdef DEBUG
    Process* proc;
#endif
} MatchVariable;

typedef struct {
    Process process;
    union {
	Eterm* heap;
	MatchVariable* variables;   /* first on "heap" */
    }u;
    Eterm default_heap[ERTS_DEFAULT_MS_HEAP_SIZE];
} ErtsMatchPseudoProcess;


static erts_tsd_key_t match_pseudo_process_key;

static ERTS_INLINE void
cleanup_match_pseudo_process(ErtsMatchPseudoProcess *mpsp, int keep_heap)
{
    if (mpsp->process.mbuf || mpsp->process.off_heap.first) {
	erts_cleanup_empty_process(&mpsp->process);
    }
#ifdef DEBUG
    else {
	erts_debug_verify_clean_empty_process(&mpsp->process);
    }
#endif
    if (!keep_heap) {
	if (mpsp->u.heap != mpsp->default_heap) {
	    /* Have to be done *after* call to erts_cleanup_empty_process() */
	    erts_free(ERTS_ALC_T_DB_MS_RUN_HEAP, (void *) mpsp->u.heap);
	    mpsp->u.heap = mpsp->default_heap;
	}
#ifdef DEBUG
	else {
	    int i;
	    for (i = 0; i < ERTS_DEFAULT_MS_HEAP_SIZE; i++) {
#if defined(ARCH_64)
		mpsp->default_heap[i] = (Eterm) 0xdeadbeefdeadbeef;
#else
		mpsp->default_heap[i] = (Eterm) 0xdeadbeef;
#endif
	    }
	}
#endif
    }
}

static ErtsMatchPseudoProcess *
create_match_pseudo_process(void)
{
    ErtsMatchPseudoProcess *mpsp;
    mpsp = (ErtsMatchPseudoProcess *)erts_alloc(ERTS_ALC_T_DB_MS_PSDO_PROC,
						sizeof(ErtsMatchPseudoProcess));
    erts_init_empty_process(&mpsp->process);
    mpsp->u.heap = mpsp->default_heap;
    return mpsp;
}

static ERTS_INLINE ErtsMatchPseudoProcess *
get_match_pseudo_process(Process *c_p, Uint heap_size)
{
    ErtsMatchPseudoProcess *mpsp;
    ErtsSchedulerData *esdp;

    esdp = c_p ? c_p->scheduler_data : erts_get_scheduler_data();

    mpsp = esdp ? esdp->match_pseudo_process :
        (ErtsMatchPseudoProcess*) erts_tsd_get(match_pseudo_process_key);

    if (mpsp) {
        ASSERT(mpsp == erts_tsd_get(match_pseudo_process_key));
        ASSERT(mpsp->process.scheduler_data == esdp);
	cleanup_match_pseudo_process(mpsp, 0);
    }
    else {
	ASSERT(erts_tsd_get(match_pseudo_process_key) == NULL);
	mpsp = create_match_pseudo_process();
        if (esdp) {
            esdp->match_pseudo_process = (void *) mpsp;
        }
        mpsp->process.scheduler_data = esdp;
	erts_tsd_set(match_pseudo_process_key, (void *) mpsp);
    }
    if (heap_size > ERTS_DEFAULT_MS_HEAP_SIZE*sizeof(Eterm)) {
	mpsp->u.heap = (Eterm*) erts_alloc(ERTS_ALC_T_DB_MS_RUN_HEAP, heap_size);
    }
    else {
	ASSERT(mpsp->u.heap == mpsp->default_heap);
    }
    return mpsp;
}

static void
destroy_match_pseudo_process(void)
{
    ErtsMatchPseudoProcess *mpsp;
    mpsp = (ErtsMatchPseudoProcess *)erts_tsd_get(match_pseudo_process_key);
    if (mpsp) {
	cleanup_match_pseudo_process(mpsp, 0);
	erts_free(ERTS_ALC_T_DB_MS_PSDO_PROC, (void *) mpsp);
	erts_tsd_set(match_pseudo_process_key, (void *) NULL);
    }
}

static
void
match_pseudo_process_init(void)
{
    erts_tsd_key_create(&match_pseudo_process_key,
			    "erts_match_pseudo_process_key");
    erts_thr_install_exit_handler(destroy_match_pseudo_process);
}

void
erts_match_set_release_result(Process* c_p)
{
    (void) get_match_pseudo_process(c_p, 0); /* Clean it up */
}

/* The trace control word. */

static erts_atomic32_t trace_control_word;

/* This needs to be here, before the bif table... */

static Eterm db_set_trace_control_word_fake_1(BIF_ALIST_1);

/*
** The table of callable bif's, i e guard bif's and 
** some special animals that can provide us with trace
** information. This array is sorted on init.
*/
static DMCGuardBif guard_tab[] =
{
    {
	am_is_atom,
	&is_atom_1,
	1,
	DBIF_ALL
    },
    {
	am_is_float,
	&is_float_1,
	1,
	DBIF_ALL
    },
    {
	am_is_integer,
	&is_integer_1,
	1,
	DBIF_ALL
    },
    {
	am_is_list,
	&is_list_1,
	1,
	DBIF_ALL
    },
    {
	am_is_number,
	&is_number_1,
	1,
	DBIF_ALL
    },
    {
	am_is_pid,
	&is_pid_1,
	1,
	DBIF_ALL
    },
    {
	am_is_port,
	&is_port_1,
	1,
	DBIF_ALL
    },
    {
	am_is_reference,
	&is_reference_1,
	1,
	DBIF_ALL
    },
    {
	am_is_tuple,
	&is_tuple_1,
	1,
	DBIF_ALL
    },
    {
	am_is_map,
	&is_map_1,
	1,
	DBIF_ALL
    },
    {
	am_is_binary,
	&is_binary_1,
	1,
	DBIF_ALL
    },
    {
	am_is_function,
	&is_function_1,
	1,
	DBIF_ALL
    },
    {
	am_is_record,
	&is_record_3,
	3,
	DBIF_ALL
    },
    {
	am_abs,
	&abs_1,
	1,
	DBIF_ALL
    },
    {
	am_element,
	&element_2,
	2,
	DBIF_ALL
    },
    {
	am_hd,
	&hd_1,
	1,
	DBIF_ALL
    },
    {
	am_length,
	&length_1,
	1,
	DBIF_ALL
    },
    {
	am_node,
	&node_1,
	1,
	DBIF_ALL
    },
    {
	am_node,
	&node_0,
	0,
	DBIF_ALL
    },
    {
	am_round,
	&round_1,
	1,
	DBIF_ALL
    },
    {
	am_size,
	&size_1,
	1,
	DBIF_ALL
    },
    {
	am_map_size,
	&map_size_1,
	1,
	DBIF_ALL
    },
    {
	am_bit_size,
	&bit_size_1,
	1,
	DBIF_ALL
    },
    {
	am_tl,
	&tl_1,
	1,
	DBIF_ALL
    },
    {
	am_trunc,
	&trunc_1,
	1,
	DBIF_ALL
    },
    {
	am_float,
	&float_1,
	1,
	DBIF_ALL
    },
    {
	am_Plus,
	&splus_1,
	1,
	DBIF_ALL
    },
    {
	am_Minus,
	&sminus_1,
	1,
	DBIF_ALL
    },
    {
	am_Plus,
	&splus_2,
	2,
	DBIF_ALL
    },
    {
	am_Minus,
	&sminus_2,
	2,
	DBIF_ALL
    },
    {
	am_Times,
	&stimes_2,
	2,
	DBIF_ALL
    },
    {
	am_Div,
	&div_2,
	2,
	DBIF_ALL
    },
    {
	am_div,
	&intdiv_2,
	2,
	DBIF_ALL
    },
    {
	am_rem,
	&rem_2,
	2,
	DBIF_ALL
    },
    {
	am_band,
	&band_2,
	2,
	DBIF_ALL
    },
    {
	am_bor,
	&bor_2,
	2,
	DBIF_ALL
    },
    {
	am_bxor,
	&bxor_2,
	2,
	DBIF_ALL
    },
    {
	am_bnot,
	&bnot_1,
	1,
	DBIF_ALL
    },
    {
	am_bsl,
	&bsl_2,
	2,
	DBIF_ALL
    },
    {
	am_bsr,
	&bsr_2,
	2,
	DBIF_ALL
    },
    {
	am_Gt,
	&sgt_2,
	2,
	DBIF_ALL
    },
    {
	am_Ge,
	&sge_2,
	2,
	DBIF_ALL
    },
    {
	am_Lt,
	&slt_2,
	2,
	DBIF_ALL
    },
    {
	am_Le,
	&sle_2,
	2,
	DBIF_ALL
    },
    {
	am_Eq,
	&seq_2,
	2,
	DBIF_ALL
    },
    {
	am_Eqeq,
	&seqeq_2,
	2,
	DBIF_ALL
    },
    {
	am_Neq,
	&sneq_2,
	2,
	DBIF_ALL
    },
    {
	am_Neqeq,
	&sneqeq_2,
	2,
	DBIF_ALL
    },
    {
	am_not,
	&not_1,
	1,
	DBIF_ALL
    },
    {
	am_xor,
	&xor_2,
	2,
	DBIF_ALL
    },
    {
	am_get_tcw,
	&db_get_trace_control_word_0,
	0,
	DBIF_TRACE_GUARD | DBIF_TRACE_BODY
    },
    {
	am_set_tcw,
	&db_set_trace_control_word_1,
	1,
	DBIF_TRACE_BODY
    },
    {
	am_set_tcw_fake,
	&db_set_trace_control_word_fake_1,
	1,
	DBIF_TRACE_BODY
    }
};

/*
** Exported
*/
Eterm db_am_eot;                /* Atom '$end_of_table' */

/*
** Forward decl's
*/


/*
** ... forwards for compiled matches
*/
/* Utility code */
static DMCGuardBif *dmc_lookup_bif(Eterm t, int arity);
#ifdef DMC_DEBUG
static Eterm dmc_lookup_bif_reversed(void *f);
#endif
static int cmp_uint(void *a, void *b);
static int cmp_guard_bif(void *a, void *b);
static int match_compact(ErlHeapFragment *expr, DMCErrInfo *err_info);
static Uint my_size_object(Eterm t);
static Eterm my_copy_struct(Eterm t, Eterm **hp, ErlOffHeap* off_heap);

/* Guard subroutines */
static void
dmc_rearrange_constants(DMCContext *context, DMC_STACK_TYPE(UWord) *text,
                        int textpos, Eterm *p, Uint nelems);
static DMCRet
dmc_array(DMCContext *context, DMCHeap *heap, DMC_STACK_TYPE(UWord) *text,
          Eterm *p, Uint nelems, int *constant);
/* Guard compilation */
static void do_emit_constant(DMCContext *context, DMC_STACK_TYPE(UWord) *text,
			     Eterm t);
static DMCRet dmc_list(DMCContext *context,
		       DMCHeap *heap,
		       DMC_STACK_TYPE(UWord) *text,
		       Eterm t,
		       int *constant);
static DMCRet dmc_tuple(DMCContext *context,
		       DMCHeap *heap,
		       DMC_STACK_TYPE(UWord) *text,
		       Eterm t,
		       int *constant);
static DMCRet
dmc_map(DMCContext *context, DMCHeap *heap, DMC_STACK_TYPE(UWord) *text,
        Eterm t, int *constant);
static DMCRet dmc_variable(DMCContext *context,
			   DMCHeap *heap,
			   DMC_STACK_TYPE(UWord) *text,
			   Eterm t,
			   int *constant);
static DMCRet dmc_fun(DMCContext *context,
		      DMCHeap *heap,
		      DMC_STACK_TYPE(UWord) *text,
		      Eterm t,
		      int *constant);
static DMCRet dmc_expr(DMCContext *context,
		       DMCHeap *heap,
		       DMC_STACK_TYPE(UWord) *text,
		       Eterm t,
		       int *constant);
static DMCRet compile_guard_expr(DMCContext *context,
				    DMCHeap *heap,
				    DMC_STACK_TYPE(UWord) *text,
				    Eterm t);
/* match expression subroutines */
static DMCRet dmc_one_term(DMCContext *context, 
			   DMCHeap *heap,
			   DMC_STACK_TYPE(Eterm) *stack,
			   DMC_STACK_TYPE(UWord) *text,
			   Eterm c);
static Eterm
dmc_private_copy(DMCContext *context, Eterm c);


#ifdef DMC_DEBUG
static int test_disassemble_next = 0;
void db_match_dis(Binary *prog);
#define TRACE erts_fprintf(stderr,"Trace: %s:%d\n",__FILE__,__LINE__)
#define FENCE_PATTERN_SIZE (1*sizeof(Uint))
#define FENCE_PATTERN 0xDEADBEEFUL
#else
#define TRACE /* Nothing */
#define FENCE_PATTERN_SIZE 0
#endif
static void vadd_dmc_err(DMCErrInfo*, DMCErrorSeverity, int var, const char *str, ...);

static Eterm dpm_array_to_list(Process *psp, Eterm *arr, int arity);

static Eterm match_spec_test(Process *p, Eterm against, Eterm spec, int trace);

static Eterm seq_trace_fake(Process *p, Eterm arg1);

static void db_free_tmp_uncompressed(DbTerm* obj);


/*
** Interface routines.
*/

/*
** Pseudo BIF:s to be callable from the PAM VM.
*/
BIF_RETTYPE db_get_trace_control_word(Process *p)
{
    Uint32 tcw = (Uint32) erts_atomic32_read_acqb(&trace_control_word);
    BIF_RET(erts_make_integer((Uint) tcw, p));
}

BIF_RETTYPE db_get_trace_control_word_0(BIF_ALIST_0)
{
    BIF_RET(db_get_trace_control_word(BIF_P));
}

BIF_RETTYPE db_set_trace_control_word(Process *p, Eterm new)
{
    Uint val;
    Uint32 old_tcw;
    if (!term_to_Uint(new, &val))
	BIF_ERROR(p, BADARG);
    if (val != ((Uint32)val))
	BIF_ERROR(p, BADARG);

    old_tcw = (Uint32) erts_atomic32_xchg_relb(&trace_control_word,
						   (erts_aint32_t) val);
    BIF_RET(erts_make_integer((Uint) old_tcw, p));
}

BIF_RETTYPE db_set_trace_control_word_1(BIF_ALIST_1)
{
    BIF_RET(db_set_trace_control_word(BIF_P, BIF_ARG_1));
}

static Eterm db_set_trace_control_word_fake_1(BIF_ALIST_1)
{
    Process *p = BIF_P;
    Eterm new = BIF_ARG_1;
    Uint val;
    if (!term_to_Uint(new, &val))
	BIF_ERROR(p, BADARG);
    if (val != ((Uint32)val))
	BIF_ERROR(p, BADARG);
    BIF_RET(db_get_trace_control_word(p));
}

/*
** The API used by the tracer (declared in global.h):
*/

/*
** Matchexpr is a list of tuples containing match-code, i e:
**
** Matchexpr = [{Pattern, Guards, Body}, ...]
** Pattern = [ PatternExpr , ...]
** PatternExpr = Constant | PatternTuple | PatternList | Variable
** Constant = Any erlang term
** PatternTuple = { PatternExpr ... }
** PatternList = [ PatternExpr ]
** Variable = '$' ++ <number>
** Guards = [Guard ...]
** Guard = {GuardFunc, GuardExpr, ...}
** GuardExpr = BoundVariable | Guard | GuardList | GuardTuple | ConstExpr
** BoundVariable = Variable (existing in Pattern)  
** GuardList = [ GuardExpr , ... ]
** GuardTuple = {{ GuardExpr, ... }}
** ConstExpr = {const, Constant}
** GuardFunc = is_list | .... | element | ...
** Body = [ BodyExpr, ... ]
** BodyExpr = GuardExpr | { BodyFunc, GuardExpr, ... }
** BodyFunc = return_trace | seq_trace | trace | ...
** - or something like that...
*/


Eterm erts_match_set_get_source(Binary *mpsp)
{
    MatchProg *prog = Binary2MatchProg(mpsp);
    return prog->saved_program;
}

/* This one is for the tracing */
Binary *erts_match_set_compile(Process *p, Eterm matchexpr, Eterm MFA) {
    Binary *bin;
    Uint sz;
    Eterm *hp;
    Uint flags;

    switch (MFA) {
    case am_receive: flags = DCOMP_TRACE; break;
    case am_send:    flags = DCOMP_TRACE | DCOMP_ALLOW_TRACE_OPS; break;
    default:
        flags = DCOMP_TRACE | DCOMP_CALL_TRACE | DCOMP_ALLOW_TRACE_OPS;
    }
    
    bin = db_match_set_compile(p, matchexpr, flags);
    if (bin != NULL) {
	MatchProg *prog = Binary2MatchProg(bin);
	sz = size_object(matchexpr);
	prog->saved_program_buf = new_message_buffer(sz);
	hp = prog->saved_program_buf->mem;
	prog->saved_program = 
	    copy_struct(matchexpr, sz, &hp, 
			&(prog->saved_program_buf->off_heap));
    }
    return bin;
}

Binary *db_match_set_compile(Process *p, Eterm matchexpr, 
			     Uint flags) 
{
    Eterm l;
    Eterm t;
    Eterm l2;
    Eterm *tp;
    Eterm *hp;
    int n = 0;
    int num_heads;
    int i;
    Binary *mps = NULL;
    int compiled = 0;
    Eterm *matches,*guards, *bodies;
    Eterm *buff;
    Eterm sbuff[15];

    if (!is_list(matchexpr))
	return NULL;
    num_heads = 0;
    for (l = matchexpr; is_list(l); l = CDR(list_val(l)))
	++num_heads;

    if (l != NIL) /* proper list... */
	return NULL;

    if (num_heads > 5) {
	buff = erts_alloc(ERTS_ALC_T_DB_TMP,
			  sizeof(Eterm) * num_heads * 3);
    } else {
	buff = sbuff;
    }

    matches = buff;
    guards = buff + num_heads;
    bodies = buff + (num_heads * 2);

    i = 0;
    for (l = matchexpr; is_list(l); l = CDR(list_val(l))) {
	t = CAR(list_val(l));
	if (!is_tuple(t) || arityval((tp = tuple_val(t))[0]) != 3) {
	    goto error;
	}
	if (!(flags & DCOMP_TRACE) || (!is_list(tp[1]) && 
					!is_nil(tp[1]))) {
	    t = tp[1];
	} else {
	    /* This is when tracing, the parameter is a list,
	       that I convert to a tuple and that is matched 
	       against an array (strange, but gives the semantics
	       of matching against a parameter list) */
	    n = 0;
	    for (l2 = tp[1]; is_list(l2); l2 = CDR(list_val(l2))) {
		++n;
	    }
	    if (l2 != NIL) {
		goto error;
	    }
	    hp = HAlloc(p, n + 1);
	    t = make_tuple(hp);
	    *hp++ = make_arityval((Uint) n);
	    l2 = tp[1];
	    while (n--) {
		*hp++ = CAR(list_val(l2));
		l2 = CDR(list_val(l2));
	    }
	}
	matches[i] = t;
	guards[i] = tp[2];
	bodies[i] = tp[3];
	++i;
    }
    if ((mps = db_match_compile(matches, guards, bodies,
				num_heads,
				flags,
				NULL)) == NULL) {
	goto error;
    }
    compiled = 1;
    if (buff != sbuff) {
	erts_free(ERTS_ALC_T_DB_TMP, buff);
    }
    return mps;

error:
    if (compiled) {
	erts_bin_free(mps);
    }
    if (buff != sbuff) {
	erts_free(ERTS_ALC_T_DB_TMP, buff);
    }
    return NULL;
}

/*
 * Compare a matching term 'a' with a constructing term 'b' for equality.
 *
 * Returns true if 'b' is guaranteed to always construct
 * the same term as 'a' has matched.
 */
static int db_match_eq_body(Eterm a, Eterm b, int const_mode)
{
    DECLARE_ESTACK(s);
    Uint arity;
    Eterm *ap, *bp;
    const Eterm CONST_MODE_OFF = THE_NON_VALUE;

    while (1) {
        switch(b & _TAG_PRIMARY_MASK) {
        case TAG_PRIMARY_LIST:
            if (!is_list(a))
                return 0;
            ESTACK_PUSH2(s, CDR(list_val(a)), CDR(list_val(b)));
            a = CAR(list_val(a));
            b = CAR(list_val(b));
            continue; /* loop without pop */

        case TAG_PRIMARY_BOXED:
            if (is_tuple(b)) {
                bp = tuple_val(b);
                if (!const_mode) {
                    if (bp[0] == make_arityval(1) && is_tuple(bp[1])) {
                        b = bp[1]; /* double-tuple syntax */
                    }
                    else if (bp[0] == make_arityval(2) && bp[1] == am_const) {
                        ESTACK_PUSH(s, CONST_MODE_OFF);
                        const_mode = 1;   /* {const, term()} syntax */
                        b = bp[2];
                        continue; /* loop without pop */
                    }
                    else
                        return 0; /* function call or invalid tuple syntax */
                }
                if (!is_tuple(a))
                    return 0;

                ap = tuple_val(a);
                bp = tuple_val(b);
                if (ap[0] != bp[0])
                    return 0;
                arity = arityval(ap[0]);
                if (arity > 0) {
                    a = *(++ap);
                    b = *(++bp);
                    while(--arity) {
                        ESTACK_PUSH2(s, *(++ap), *(++bp));
                    }
                    continue; /* loop without pop */
                }
            }
            else if (is_map(b)) {
                /* We don't know what other pairs the matched map may contain */
                return 0;
            }
            else if (!eq(a,b)) /* other boxed */
                return 0;
            break;

        case TAG_PRIMARY_IMMED1:
            if (a != b || a == am_Underscore || a == am_DollarDollar
                || a == am_DollarUnderscore
                || (const_mode && db_is_variable(a) >= 0)) {

                return 0;
            }
            break;
        default:
            erts_exit(ERTS_ABORT_EXIT, "db_compare: "
                      "Bad object on ESTACK: 0x%bex\n", b);
        }

pop_next:
        if (ESTACK_ISEMPTY(s))
            break; /* done */

        b = ESTACK_POP(s);
        if (b == CONST_MODE_OFF) {
            ASSERT(const_mode);
            const_mode = 0;
            goto pop_next;
        }
        a = ESTACK_POP(s);
    }

    DESTROY_ESTACK(s);
    return 1;
}

/* This is used by select_replace */
int db_match_keeps_key(int keypos, Eterm match, Eterm guard, Eterm body)
{
    Eterm match_key;
    Eterm* body_list;
    Eterm single_body_term;
    Eterm* single_body_term_tpl;
    Eterm single_body_subterm;
    Eterm single_body_subterm_key;
    Eterm* single_body_subterm_key_tpl;
    int const_mode;

    if (!is_list(body)) {
        return 0;
    }

    body_list = list_val(body);
    if (CDR(body_list) != NIL) {
        return 0;
    }

    single_body_term = CAR(body_list);
    if (single_body_term == am_DollarUnderscore) {
        /* same tuple is returned */
        return 1;
    }

    if (!is_tuple(single_body_term)) {
        return 0;
    }

    match_key = db_getkey(keypos, match);
    if (!is_value(match_key)) {
        // can't get key out of match
        return 0;
    }

    single_body_term_tpl = tuple_val(single_body_term);
    if (single_body_term_tpl[0] == make_arityval(2) &&
        single_body_term_tpl[1] == am_const) {
        /* {const, {"ets-tuple constant"}} */
        single_body_subterm = single_body_term_tpl[2];
        const_mode = 1;
    }
    else if (*single_body_term_tpl == make_arityval(1)) {
        /* {{"ets-tuple construction"}} */
        single_body_subterm = single_body_term_tpl[1];
        const_mode = 0;
    }
    else {
        /* not a tuple construction */
        return 0;
    }

    single_body_subterm_key = db_getkey(keypos, single_body_subterm);
    if (!is_value(single_body_subterm_key)) {
        // can't get key out of single body subterm
        return 0;
    }

    if (db_match_eq_body(match_key, single_body_subterm_key, const_mode)) {
        /* tuple with same key is returned */
        return 1;
    }

    if (const_mode) {
        /* constant key did not match */
        return 0;
    }

    if (!is_tuple(single_body_subterm_key)) {
        /* can't possibly be an element instruction */
        return 0;
    }

    single_body_subterm_key_tpl = tuple_val(single_body_subterm_key);
    if (single_body_subterm_key_tpl[0] == make_arityval(3) &&
        single_body_subterm_key_tpl[1] == am_element &&
        single_body_subterm_key_tpl[3] == am_DollarUnderscore &&
        single_body_subterm_key_tpl[2] == make_small(keypos))
    {
        /* {element, KeyPos, '$_'} */
        return 1;
    }

    return 0;
}

Eterm db_match_set_lint(Process *p, Eterm matchexpr, Uint flags) 
{
    Eterm l;
    Eterm t;
    Eterm l2;
    Eterm *tp;
    Eterm *hp;
    DMCErrInfo *err_info = db_new_dmc_err_info();
    Eterm ret;
    int n = 0;
    int num_heads;
    Binary *mp;
    Eterm *matches,*guards, *bodies;
    Eterm sbuff[15];
    Eterm *buff = sbuff;
    int i;

    if (!is_list(matchexpr)) {
	add_dmc_err(err_info, "Match programs are not in a list.",
                    -1, 0UL, dmcError);
	goto done;
    }
    num_heads = 0;
    for (l = matchexpr; is_list(l); l = CDR(list_val(l)))
	++num_heads;

    if (l != NIL)  { /* proper list... */
	add_dmc_err(err_info, "Match programs are not in a proper list.",
                     -1, 0UL, dmcError);
	goto done;
    }

    if (num_heads > 5) {
	buff = erts_alloc(ERTS_ALC_T_DB_TMP,
			  sizeof(Eterm) * num_heads * 3);
    } 

    matches = buff;
    guards = buff + num_heads;
    bodies = buff + (num_heads * 2);

    i = 0;
    for (l = matchexpr; is_list(l); l = CDR(list_val(l))) {
	t = CAR(list_val(l));
	if (!is_tuple(t) || arityval((tp = tuple_val(t))[0]) != 3) {
	    add_dmc_err(err_info, 
			"Match program part is not a tuple of "
			"arity 3.", 
			-1, 0UL, dmcError);
	    goto done;
	}
	if (!(flags & DCOMP_TRACE) || (!is_list(tp[1]) && 
					!is_nil(tp[1]))) {
	    t = tp[1];
	} else {
	    n = 0;
	    for (l2 = tp[1]; is_list(l2); l2 = CDR(list_val(l2))) {
		++n;
	    }
	    if (l2 != NIL) {
		add_dmc_err(err_info, 
			    "Match expression part %T is not a "
			    "proper list.", 
			    -1, tp[1], dmcError);
		
		goto done;
	    }
	    hp = HAlloc(p, n + 1);
	    t = make_tuple(hp);
	    *hp++ = make_arityval((Uint) n);
	    l2 = tp[1];
	    while (n--) {
		*hp++ = CAR(list_val(l2));
		l2 = CDR(list_val(l2));
	    }
	}
	matches[i] = t;
	guards[i] = tp[2];
	bodies[i] = tp[3];
	++i;
    }
    mp = db_match_compile(matches, guards, bodies, num_heads,
			  flags, err_info); 
    if (mp != NULL) {
	erts_bin_free(mp);
    }
done:
    ret = db_format_dmc_err_info(p, err_info);
    db_free_dmc_err_info(err_info);
    if (buff != sbuff) {
	erts_free(ERTS_ALC_T_DB_TMP, buff);
    }
    return ret;
}
    
/* Returns
 *   am_false      if no match or
 *                 if {message,false} has been called,
 *   am_true       if {message,_} has NOT been called or
 *                 if {message,true} has been called,
 *   Msg           if {message,Msg} has been called.
 *
 *   If return value is_not_immed
 *   then erts_match_set_release_result_trace() must be called to release it.
 */
Eterm erts_match_set_run_trace(Process *c_p,
                               Process *self,
                               Binary *mpsp,
                               Eterm *args, int num_args,
                               enum erts_pam_run_flags in_flags,
                               Uint32 *return_flags)
{
    Eterm ret;

    ret = db_prog_match(c_p, self, mpsp, NIL, args, num_args,
			in_flags, return_flags);

    ASSERT(!(is_non_value(ret) && *return_flags));

    if (is_non_value(ret) || ret == am_false) {
        erts_match_set_release_result(c_p);
        return am_false;
    }
    if (is_immed(ret))
        erts_match_set_release_result(c_p);
    return ret;
}

static Eterm erts_match_set_run_ets(Process *p, Binary *mpsp,
				    Eterm args, int num_args,
				    Uint32 *return_flags)
{
    Eterm ret;

    ret = db_prog_match(p, p,
                        mpsp, args, NULL, num_args,
			ERTS_PAM_COPY_RESULT,
			return_flags);
#if defined(HARDDEBUG)
    if (is_non_value(ret)) {
	erts_fprintf(stderr, "Failed\n");
    } else {
	erts_fprintf(stderr, "Returning : %T\n", ret);
    }
#endif
    return ret;
    /* Returns
     *   THE_NON_VALUE if no match
     *   am_false      if {message,false} has been called,
     *   am_true       if {message,_} has not been called or
     *                 if {message,true} has been called,
     *   Msg           if {message,Msg} has been called.
     */
}

/*
** API Used by other erl_db modules.
*/

void db_initialize_util(void){
    qsort(guard_tab, 
	  sizeof(guard_tab) / sizeof(DMCGuardBif), 
	  sizeof(DMCGuardBif), 
	  (int (*)(const void *, const void *)) &cmp_guard_bif);
    match_pseudo_process_init();
    erts_atomic32_init_nob(&trace_control_word, 0);
}



Eterm db_getkey(int keypos, Eterm obj)
{
    if (is_tuple(obj)) {
	Eterm *tptr = tuple_val(obj);
	if (arityval(*tptr) >= keypos)
	    return *(tptr + keypos);
    }
    return THE_NON_VALUE;
}

/*
** Matching compiled (executed by "Pam" :-)
*/

/*
** The actual compiling of the match expression and the guards
*/
Binary *db_match_compile(Eterm *matchexpr, 
			 Eterm *guards, 
			 Eterm *body,
			 int num_progs,
			 Uint flags, 
			 DMCErrInfo *err_info)
{
    DMCHeap heap;
    DMC_STACK_TYPE(Eterm) stack;
    DMC_STACK_TYPE(UWord) text;
    DMCContext context;
    MatchProg *ret = NULL;
    Eterm t;
    Uint i;
    Uint num_iters;
    int structure_checked;
    DMCRet res;
    int current_try_label;
    Binary *bp = NULL;
    unsigned clause_start;

    DMC_INIT_STACK(stack);
    DMC_INIT_STACK(text);

    context.stack_need = context.stack_used = 0;
    context.save = context.copy = NULL;
    context.num_match = num_progs;
    context.matchexpr = matchexpr;
    context.guardexpr = guards;
    context.bodyexpr = body;
    context.err_info = err_info;
    context.cflags = flags;

    heap.size = DMC_DEFAULT_SIZE;
    heap.vars = heap.vars_def;

    /*
    ** Compile the match expression
    */
restart:
    heap.vars_used = 0;
    for (context.current_match = 0; 
	 context.current_match < num_progs; 
	 ++context.current_match) { /* This loop is long, 
				       too long */
	sys_memset(heap.vars, 0, heap.size * sizeof(*heap.vars));
	t = context.matchexpr[context.current_match];
	context.stack_used = 0;
	structure_checked = 0;
	if (context.current_match < num_progs - 1) {
	    DMC_PUSH(text,matchTryMeElse);
	    current_try_label = DMC_STACK_NUM(text);
	    DMC_PUSH(text,0);
	} else {
	    current_try_label = -1;
	}
	clause_start = DMC_STACK_NUM(text); /* the "special" test needs it */
	DMC_PUSH(stack,NIL);
	for (;;) {
	    switch (t & _TAG_PRIMARY_MASK) {
	    case TAG_PRIMARY_BOXED:
                if (is_flatmap(t)) {
                    num_iters = flatmap_get_size(flatmap_val(t));
                    if (!structure_checked) {
                        DMC_PUSH2(text, matchMap, num_iters);
                    }
                    structure_checked = 0;
                    for (i = 0; i < num_iters; ++i) {
                        Eterm key = flatmap_get_keys(flatmap_val(t))[i];
                        if (db_is_variable(key) >= 0) {
                            if (context.err_info) {
                                add_dmc_err(context.err_info,
                                            "Variable found in map key.",
                                            -1, 0UL, dmcError);
                            }
                            goto error;
                        } else if (key == am_Underscore) {
                            if (context.err_info) {
                                add_dmc_err(context.err_info,
                                            "Underscore found in map key.",
                                            -1, 0UL, dmcError);
                            }
                            goto error;
                        }
                        DMC_PUSH2(text, matchKey, dmc_private_copy(&context, key));
                        {
                            int old_stack = ++(context.stack_used);
                            Eterm value = flatmap_get_values(flatmap_val(t))[i];
                            res = dmc_one_term(&context, &heap, &stack, &text,
                                               value);
                            ASSERT(res != retFail);
                            if (res == retRestart) {
                                goto restart;
                            }
                            if (old_stack != context.stack_used) {
                                ASSERT(old_stack + 1 == context.stack_used);
                                DMC_PUSH(text, matchSwap);
                            }
                            if (context.stack_used > context.stack_need) {
                                context.stack_need = context.stack_used;
                            }
                            DMC_PUSH(text, matchPop);
                            --(context.stack_used);
                        }
                    }
                    break;
                }
                if (is_hashmap(t)) {
                    DECLARE_WSTACK(wstack);
                    Eterm *kv;
                    num_iters = hashmap_size(t);
                    if (!structure_checked) {
                        DMC_PUSH2(text, matchMap, num_iters);
                    }
                    structure_checked = 0;

                    hashmap_iterator_init(&wstack, t, 0);

                    while ((kv=hashmap_iterator_next(&wstack)) != NULL) {
                        Eterm key = CAR(kv);
                        Eterm value = CDR(kv);
                        if (db_is_variable(key) >= 0) {
                            if (context.err_info) {
                                add_dmc_err(context.err_info,
                                        "Variable found in map key.",
                                        -1, 0UL, dmcError);
                            }
                            DESTROY_WSTACK(wstack);
                            goto error;
                        } else if (key == am_Underscore) {
                            if (context.err_info) {
                                add_dmc_err(context.err_info,
                                        "Underscore found in map key.",
                                        -1, 0UL, dmcError);
                            }
                            DESTROY_WSTACK(wstack);
                            goto error;
                        }
                        DMC_PUSH2(text, matchKey, dmc_private_copy(&context, key));
                        {
                            int old_stack = ++(context.stack_used);
                            res = dmc_one_term(&context, &heap, &stack, &text,
                                               value);
                            ASSERT(res != retFail);
                            if (res == retRestart) {
                                DESTROY_WSTACK(wstack);
                                goto restart;
                            }
                            if (old_stack != context.stack_used) {
                                ASSERT(old_stack + 1 == context.stack_used);
                                DMC_PUSH(text, matchSwap);
                            }
                            if (context.stack_used > context.stack_need) {
                                context.stack_need = context.stack_used;
                            }
                            DMC_PUSH(text, matchPop);
                            --(context.stack_used);
                        }
                    }
                    DESTROY_WSTACK(wstack);
                    break;
                }
		if (!is_tuple(t)) {
		    goto simple_term;
		}
		num_iters = arityval(*tuple_val(t));
		if (!structure_checked) { /* i.e. we did not 
					     pop it */
		    DMC_PUSH2(text, matchTuple, num_iters);
		}
		structure_checked = 0;
		for (i = 1; i <= num_iters; ++i) {
		    if ((res = dmc_one_term(&context, 
					    &heap, 
					    &stack, 
					    &text, 
					    tuple_val(t)[i]))
			!= retOk) {
			if (res == retRestart) {
			    goto restart; /* restart the 
					     surrounding 
					     loop */
			} else goto error;
		    }	    
		}
		break;
	    case TAG_PRIMARY_LIST:
		if (!structure_checked) {
		    DMC_PUSH(text, matchList);
		}
		structure_checked = 0; /* Whatever it is, we did 
					  not pop it */
		if ((res = dmc_one_term(&context, &heap, &stack, 
					&text, CAR(list_val(t))))
		    != retOk) {
		    if (res == retRestart) {
			goto restart;
		    } else goto error;
		}	    
		t = CDR(list_val(t));
		continue;
	    default: /* Nil and non proper tail end's or 
			single terms as match 
			expressions */
	    simple_term:
		structure_checked = 0;
		if ((res = dmc_one_term(&context, &heap, &stack, 
					&text, t))
		    != retOk) {
		    if (res == retRestart) {
			goto restart;
		    } else goto error;
		}	    
		break;
	    }

	    /* The *program's* stack just *grows* while we are 
	       traversing one composite data structure, we can 
	       check the stack usage here */

	    if (context.stack_used > context.stack_need)
		context.stack_need = context.stack_used;

	    /* We are at the end of one composite data structure, 
	       pop sub structures and emit a matchPop instruction 
	       (or break) */
	    if ((t = DMC_POP(stack)) == NIL) {
		break;
	    } else {
		DMC_PUSH(text, matchPop);
		structure_checked = 1; /* 
					* Checked with matchPushT 
					* or matchPushL
					*/
		--(context.stack_used);
	    }
	}
    
	/* 
	** There is one single top variable in the match expression
	** iff the text is two Uint's and the single instruction
	** is 'matchBind' or it is only a skip.
	*/
	context.special = 
	    (DMC_STACK_NUM(text) == 2 + clause_start && 
	     DMC_PEEK(text,clause_start) == matchBind) || 
	    (DMC_STACK_NUM(text) == 1 + clause_start && 
	     DMC_PEEK(text, clause_start) == matchSkip);

	if (flags & DCOMP_TRACE) {
	    if (context.special) {
		if (DMC_PEEK(text, clause_start) == matchBind) {
		    DMC_POKE(text, clause_start, matchArrayBind);
		} 
	    } else {
		ASSERT(DMC_STACK_NUM(text) >= 1);
		if (DMC_PEEK(text, clause_start) != matchTuple) {
		    /* If it isn't "special" and the argument is 
		       not a tuple, the expression is not valid 
		       when matching an array*/
		    if (context.err_info) {
			add_dmc_err(context.err_info, 
				    "Match head is invalid in "
				    "this context.", 
				    -1, 0UL,
				    dmcError);
		    }
		    goto error;
		}
		DMC_POKE(text, clause_start, matchArray);
	    }
	}


	/*
	** ... and the guards
	*/
	context.is_guard = 1;
	if (compile_guard_expr
	    (&context,
	     &heap,
	     &text,
	     context.guardexpr[context.current_match]) != retOk) 
	    goto error;
	context.is_guard = 0;
	if ((context.cflags & DCOMP_TABLE) && 
	    !is_list(context.bodyexpr[context.current_match])) {
	    if (context.err_info) {
		add_dmc_err(context.err_info, 
			    "Body clause does not return "
			    "anything.", -1, 0UL,
			    dmcError);
	    }
	    goto error;
	}
	if (compile_guard_expr
	    (&context,
	     &heap,
	     &text,
	     context.bodyexpr[context.current_match]) != retOk) 
	    goto error;

	/*
	 * The compilation does not bail out when error information
	 * is requested, so we need to detect that here...
	 */
	if (context.err_info != NULL && 
	    (context.err_info)->error_added) {
	    goto error;
	}


	/* If the matchprogram comes here, the match is 
	   successful */
	DMC_PUSH(text,matchHalt);
	/* Fill in try-me-else label if there is one. */ 
	if (current_try_label >= 0) {
	    DMC_POKE(text, current_try_label, DMC_STACK_NUM(text));
	}
    } /* for (context.current_match = 0 ...) */


    /*
    ** Done compiling
    ** Allocate enough space for the program,
    ** heap size is in 'heap_used', stack size is in 'stack_need'
    ** and text size is simply DMC_STACK_NUM(text).
    ** The "program memory" is allocated like this:
    ** text ----> +-------------+
    **            |             |
    **              ..........
    **            +-------------+
    **
    **  The heap-eheap-stack block of a MatchProg is nowadays allocated
    **  when the match program is run (see db_prog_match()).
    **
    ** heap ----> +-------------+
    **              ..........
    ** eheap ---> +             +
    **              ..........
    ** stack ---> +             +
    **              ..........
    **            +-------------+
    ** The stack is expected to grow towards *higher* adresses.
    ** A special case is when the match expression is a single binding 
    ** (i.e '$1'), then the field single_variable is set to 1.
    */
    bp = erts_create_magic_binary(((sizeof(MatchProg) - sizeof(UWord)) +
				   (DMC_STACK_NUM(text) * sizeof(UWord))),
				  erts_db_match_prog_destructor);
    ret = Binary2MatchProg(bp);
    ret->saved_program_buf = NULL;
    ret->saved_program = NIL;
    ret->term_save = context.save;
    ret->num_bindings = heap.vars_used;
    ret->single_variable = context.special;
    sys_memcpy(ret->text, DMC_STACK_DATA(text), 
	       DMC_STACK_NUM(text) * sizeof(UWord));
    ret->stack_offset = heap.vars_used*sizeof(MatchVariable) + FENCE_PATTERN_SIZE;
    ret->heap_size = ret->stack_offset + context.stack_need * sizeof(Eterm*) + FENCE_PATTERN_SIZE;

#ifdef DMC_DEBUG
    ret->prog_end = ret->text + DMC_STACK_NUM(text);
#endif

    /* 
     * Fall through to cleanup code, but context.save should not be free'd
     */  
    context.save = NULL;
error: /* Here is were we land when compilation failed. */
    if (context.save != NULL) {
	free_message_buffer(context.save);
	context.save = NULL;
    }
    DMC_FREE(stack);
    DMC_FREE(text);
    if (context.copy != NULL) 
	free_message_buffer(context.copy);
    if (heap.vars != heap.vars_def)
	erts_free(ERTS_ALC_T_DB_MS_CMPL_HEAP, (void *) heap.vars);
    return bp;
}

/*
** Free a match program (in a binary)
*/
int erts_db_match_prog_destructor(Binary *bprog)
{
    MatchProg *prog;
    if (bprog == NULL)
	return 1;
    prog = Binary2MatchProg(bprog);
    if (prog->term_save != NULL) {
	free_message_buffer(prog->term_save); 
    }
    if (prog->saved_program_buf != NULL)
	free_message_buffer(prog->saved_program_buf);
    return 1;
}

void
erts_match_prog_foreach_offheap(Binary *bprog,
				void (*func)(ErlOffHeap *, void *),
				void *arg)
{
    MatchProg *prog;
    ErlHeapFragment *tmp;
    if (bprog == NULL)
	return;
    prog = Binary2MatchProg(bprog);
    tmp = prog->term_save; 
    while (tmp) {
	(*func)(&(tmp->off_heap), arg);
	tmp = tmp->next;
    }
    if (prog->saved_program_buf)
	(*func)(&(prog->saved_program_buf->off_heap), arg);
}

/*
** This is not the most efficient way to do it, but it's a rare
** and not especially nice case when this is used.
*/
static Eterm dpm_array_to_list(Process *psp, Eterm *arr, int arity)
{
    Eterm *hp = HAllocX(psp, arity * 2, HEAP_XTRA);
    Eterm ret = NIL;
    while (--arity >= 0) {
	ret = CONS(hp, arr[arity], ret);
	hp += 2;
    }
    return ret;
}

/*
** Execution of the match program, this is Pam.
** May return THE_NON_VALUE, which is a bailout.
** the parameter 'arity' is only used if 'term' is actually an array,
** i.e. 'DCOMP_TRACE' was specified 
*/
Eterm db_prog_match(Process *c_p,
                    Process *self,
                    Binary *bprog,
		    Eterm term,
		    Eterm *termp,
		    int arity,
		    enum erts_pam_run_flags in_flags,
		    Uint32 *return_flags)
{
    MatchProg *prog = Binary2MatchProg(bprog);
    const Eterm *ep, *tp, **sp;
    Eterm t;
    Eterm *esp;
    MatchVariable* variables;
    ErtsCodeMFA *cp;
    const UWord *pc = prog->text;
    Eterm *ehp;
    Eterm ret;
    Uint n;
    int i;
    unsigned do_catch;
    ErtsMatchPseudoProcess *mpsp;
    Process *psp;
    Process* build_proc;
    Process *tmpp;
    Process *current_scheduled;
    ErtsSchedulerData *esdp;
    Eterm (*bif)(Process*, ...);
    Eterm bif_args[3];
    int fail_label;
#ifdef DMC_DEBUG
    Uint *heap_fence;
    Uint *stack_fence;
    Uint save_op;
#endif /* DMC_DEBUG */

    ERTS_UNDEF(n,0);
    ERTS_UNDEF(current_scheduled,NULL);

    ASSERT(c_p || !(in_flags & ERTS_PAM_COPY_RESULT));

    mpsp = get_match_pseudo_process(c_p, prog->heap_size);
    psp = &mpsp->process;

    /* We need to lure the scheduler into believing in the pseudo process, 
       because of floating point exceptions. Do *after* mpsp is set!!! */

    esdp = erts_get_scheduler_data();
    if (esdp)
        current_scheduled = esdp->current_process;
    /* SMP: psp->scheduler_data is set by get_match_pseudo_process */

#ifdef DMC_DEBUG
    save_op = 0;
    heap_fence = (Eterm*)((char*) mpsp->u.heap + prog->stack_offset) - 1;
    stack_fence = (Eterm*)((char*) mpsp->u.heap + prog->heap_size) - 1;
    *heap_fence = FENCE_PATTERN;
    *stack_fence = FENCE_PATTERN;
#endif /* DMC_DEBUG */

#ifdef HARDDEBUG
#define FAIL() {erts_printf("Fail line %d\n",__LINE__); goto fail;}
#else
#define FAIL() goto fail
#endif
#define FAIL_TERM am_EXIT /* The term to set as return when bif fails and
			     do_catch != 0 */

    *return_flags = 0U;
    variables = mpsp->u.variables;

restart:
    ep = &term;
    esp = (Eterm*)((char*)mpsp->u.heap + prog->stack_offset);
    sp = (const Eterm **)esp;
    ret = am_true;
    do_catch = 0;
    fail_label = -1;
    build_proc = psp;
    if (esdp)
        esdp->current_process = psp;

#ifdef DEBUG
    ASSERT(variables == mpsp->u.variables);
    for (i=0; i<prog->num_bindings; i++) {
	variables[i].term = THE_NON_VALUE;
	variables[i].proc = NULL;
    }
#endif

    for (;;) {

    #ifdef DMC_DEBUG
	if (*heap_fence != FENCE_PATTERN) {
	    erts_exit(ERTS_ERROR_EXIT, "Heap fence overwritten in db_prog_match after op "
		     "0x%08x, overwritten with 0x%08x.", save_op, *heap_fence);
	}
	if (*stack_fence != FENCE_PATTERN) {
	    erts_exit(ERTS_ERROR_EXIT, "Stack fence overwritten in db_prog_match after op "
		     "0x%08x, overwritten with 0x%08x.", save_op, 
		     *stack_fence);
	}
	save_op = *pc;
    #endif
	switch (*pc++) {
	case matchTryMeElse:
	    ASSERT(fail_label == -1);
	    fail_label = *pc++;
	    break;
	case matchArray: /* only when DCOMP_TRACE, is always first
			    instruction. */
	    n = *pc++;
	    if ((int) n != arity)
		FAIL();
	    ep = termp;
	    break;
	case matchArrayBind: /* When the array size is unknown. */
	    ASSERT(termp || arity==0);
	    n = *pc++;
	    variables[n].term = dpm_array_to_list(psp, termp, arity);
	    break;
	case matchTuple: /* *ep is a tuple of arity n */
	    if (!is_tuple(*ep))
		FAIL();
	    ep = tuple_val(*ep);
	    n = *pc++;
	    if (arityval(*ep) != n)
		FAIL();
	    ++ep;
	    break;
	case matchPushT: /* *ep is a tuple of arity n, 
			    push ptr to first element */
	    if (!is_tuple(*ep))
		FAIL();
	    tp = tuple_val(*ep);
	    n = *pc++;
	    if (arityval(*tp) != n)
		FAIL();
	    *sp++ = tp + 1;
	    ++ep;
	    break;
	case matchList:
	    if (!is_list(*ep))
		FAIL();
	    ep = list_val(*ep);
	    break;
	case matchPushL:
	    if (!is_list(*ep))
		FAIL();
	    *sp++ = list_val(*ep);
	    ++ep;
	    break;
        case matchMap:
            if (!is_map(*ep)) {
                FAIL();
            }
            n = *pc++;
            if (is_flatmap(*ep)) {
		if (flatmap_get_size(flatmap_val(*ep)) < n) {
		    FAIL();
		}
            } else {
		ASSERT(is_hashmap(*ep));
		if (hashmap_size(*ep) < n) {
		    FAIL();
		}
	    }
            ep = flatmap_val(*ep);
            break;
        case matchPushM:
            if (!is_map(*ep)) {
                FAIL();
            }
            n = *pc++;
            if (is_flatmap(*ep)) {
		if (flatmap_get_size(flatmap_val(*ep)) < n) {
		    FAIL();
		}
	    } else {
		ASSERT(is_hashmap(*ep));
		if (hashmap_size(*ep) < n) {
		    FAIL();
		}
	    }
            *sp++ = flatmap_val(*ep++);
            break;
        case matchKey:
            t = (Eterm) *pc++;
            tp = erts_maps_get(t, make_boxed(ep));
            if (!tp) {
                FAIL();
            }
            *sp++ = ep;
            ep = tp;
            break;
	case matchPop:
	    ep = *(--sp);
	    break;
        case matchSwap:
            tp = sp[-1];
            sp[-1] = sp[-2];
            sp[-2] = tp;
            break;
	case matchBind:
	    n = *pc++;
	    variables[n].term = *ep++;
	    break;
	case matchCmp:
	    n = *pc++;
	    if (!EQ(variables[n].term, *ep))
		FAIL();
	    ++ep;
	    break;
	case matchEqBin:
	    t = (Eterm) *pc++;
	    if (!EQ(t,*ep))
		FAIL();
	    ++ep;
	    break;
	case matchEqFloat:
	    if (!is_float(*ep))
		FAIL();
	    if (sys_memcmp(float_val(*ep) + 1, pc, sizeof(double)))
		FAIL();
	    pc += TermWords(2);
	    ++ep;
	    break;
	case matchEqRef: {
	    Eterm* epc = (Eterm*)pc;
	    if (!is_ref(*ep))
		FAIL();
	    if (!EQ(make_internal_ref(epc), *ep)) {
		FAIL();
	    }
	    i = thing_arityval(*epc);
	    pc += TermWords(i+1);
	    ++ep;
	    break;
	}
	case matchEqBig:
	    if (!is_big(*ep))
		FAIL();
	    tp = big_val(*ep);
	    {
		Eterm *epc = (Eterm *) pc;
		if (*tp != *epc)
		    FAIL();
		i = BIG_ARITY(epc);
		pc += TermWords(i+1);
		while(i--) {
		    if (*++tp != *++epc) {
			FAIL();
		    }
		}
	    }
	    ++ep;
	    break;
	case matchEq:
	    t = (Eterm) *pc++;
	    ASSERT(is_immed(t));
	    if (t != *ep++)
		FAIL();
	    break;
	case matchSkip:
	    ++ep;
	    break;
	/* 
	 * Here comes guard & body instructions
	 */
	case matchPushC: /* Push constant */
	    if ((in_flags & ERTS_PAM_COPY_RESULT)
		&& do_catch && !is_immed(*pc)) {
		*esp++ = copy_object(*pc++, c_p);
	    }
	    else {
		*esp++ = *pc++;
	    }
	    break;
	case matchConsA:
	    ehp = HAllocX(build_proc, 2, HEAP_XTRA);
	    CDR(ehp) = *--esp;
	    CAR(ehp) = esp[-1];
	    esp[-1] = make_list(ehp);
	    break;
	case matchConsB:
	    ehp = HAllocX(build_proc, 2, HEAP_XTRA);
	    CAR(ehp) = *--esp;
	    CDR(ehp) = esp[-1];
	    esp[-1] = make_list(ehp);
	    break;
	case matchMkTuple:
	    n = *pc++;
	    ehp = HAllocX(build_proc, n+1, HEAP_XTRA);
	    t = make_tuple(ehp);
	    *ehp++ = make_arityval(n);
	    while (n--) {
		*ehp++ = *--esp;
	    }
	    *esp++ = t;
	    break;
        case matchMkFlatMap:
            n = *pc++;
            ehp = HAllocX(build_proc, MAP_HEADER_FLATMAP_SZ + n, HEAP_XTRA);
            t = *--esp;
            {
                flatmap_t *m = (flatmap_t *)ehp;
                m->thing_word = MAP_HEADER_FLATMAP;
                m->size = n;
                m->keys = t;
            }
            t = make_flatmap(ehp);
            ehp += MAP_HEADER_FLATMAP_SZ;
            while (n--) {
                *ehp++ = *--esp;
            }
            *esp++ = t;
            break;
        case matchMkHashMap:
            n = *pc++;
            esp -= 2*n;
            ehp = HAllocX(build_proc, 2*n, HEAP_XTRA);
            {
                ErtsHeapFactory factory;
                Uint ix;
                for (ix = 0; ix < 2*n; ix++){
                    ehp[ix] = esp[ix];
                }
                erts_factory_proc_init(&factory, build_proc);
                t = erts_hashmap_from_array(&factory, ehp, n, 0);
                erts_factory_close(&factory);
            }
            *esp++ = t;
            break;
	case matchCall0:
	    bif = (Eterm (*)(Process*, ...)) *pc++;
	    t = (*bif)(build_proc, bif_args);
	    if (is_non_value(t)) {
		if (do_catch)
		    t = FAIL_TERM;
		else
		    FAIL();
	    }
	    *esp++ = t;
	    break;
	case matchCall1:
	    bif = (Eterm (*)(Process*, ...)) *pc++;
	    t = (*bif)(build_proc, esp-1);
	    if (is_non_value(t)) {
		if (do_catch)
		    t = FAIL_TERM;
		else
		    FAIL();
	    }
	    esp[-1] = t;
	    break;
	case matchCall2:
	    bif = (Eterm (*)(Process*, ...)) *pc++;
	    bif_args[0] = esp[-1];
	    bif_args[1] = esp[-2];
	    t = (*bif)(build_proc, bif_args);
	    if (is_non_value(t)) {
		if (do_catch)
		    t = FAIL_TERM;
		else
		    FAIL();
	    }
	    --esp;
	    esp[-1] = t;
	    break;
	case matchCall3:
	    bif = (Eterm (*)(Process*, ...)) *pc++;
	    bif_args[0] = esp[-1];
	    bif_args[1] = esp[-2];
	    bif_args[2] = esp[-3];
	    t = (*bif)(build_proc, bif_args);
	    if (is_non_value(t)) {
		if (do_catch)
		    t = FAIL_TERM;
		else
		    FAIL();
	    }
	    esp -= 2;
	    esp[-1] = t;
	    break;
	case matchPushVResult:
	    if (!(in_flags & ERTS_PAM_COPY_RESULT)) goto case_matchPushV;
	    /* Build copy on callers heap */
	    n = *pc++;
	    ASSERT(is_value(variables[n].term));
	    ASSERT(!variables[n].proc);
	    variables[n].term = copy_object_x(variables[n].term, c_p, HEAP_XTRA);
	    *esp++ = variables[n].term;
	    #ifdef DEBUG
	    variables[n].proc = c_p;
	    #endif
	    break;
	case matchPushV:
	case_matchPushV:
	    n = *pc++;
	    ASSERT(is_value(variables[n].term));
	    *esp++ = variables[n].term;
	    break;
	case matchPushExpr:
	    if (in_flags & ERTS_PAM_COPY_RESULT) {
		Uint sz;
		Eterm* top;
		sz = size_object(term);
		top = HAllocX(build_proc, sz, HEAP_XTRA);
		if (in_flags & ERTS_PAM_CONTIGUOUS_TUPLE) {
		    ASSERT(is_tuple(term));
		    *esp++ = copy_shallow(tuple_val(term), sz, &top, &MSO(build_proc));
		}
		else {
		    *esp++ = copy_struct(term, sz, &top, &MSO(build_proc));
		}
	    }
	    else {
		*esp++ = term;
	    }
	    break;
	case matchPushArrayAsList:
	    n = arity; /* Only happens when 'term' is an array */
	    tp = termp;
	    ehp = HAllocX(build_proc, n*2, HEAP_XTRA);
	    *esp++  = make_list(ehp);
	    while (n--) {
		*ehp++ = *tp++;
		*ehp = make_list(ehp + 1);
		ehp++; /* As pointed out by Mikael Pettersson the expression
			  (*ehp++ = make_list(ehp + 1)) that I previously
			  had written here has undefined behaviour. */
	    }
	    ehp[-1] = NIL;
	    break;
	case matchPushArrayAsListU:
	    /* This instruction is NOT efficient. */
	    *esp++  = dpm_array_to_list(build_proc, termp, arity);
	    break;
	case matchTrue:
	    if (*--esp != am_true)
		FAIL();
	    break;
	case matchOr:
	    n = *pc++;
	    t = am_false;
	    while (n--) {
		if (*--esp == am_true) {
		    t = am_true;
		} else if (*esp != am_false) {
		    esp -= n;
		    if (do_catch) {
			t = FAIL_TERM;
			break;
		    } else {
			FAIL();
		    }
		}
	    }
	    *esp++ = t;
	    break;
	case matchAnd:
	    n = *pc++;
	    t = am_true;
	    while (n--) {
		if (*--esp == am_false) {
		    t = am_false;
		} else if (*esp != am_true) {
		    esp -= n;
		    if (do_catch) {
			t = FAIL_TERM;
			break;
		    } else {
			FAIL();
		    }
		}
	    }
	    *esp++ = t;
	    break;
	case matchOrElse:
	    n = *pc++;
	    if (*--esp == am_true) {
		++esp;
		pc += n;
	    } else if (*esp != am_false) {
		if (do_catch) {
		    *esp++ = FAIL_TERM;
		    pc += n;
		} else {
		    FAIL();
		}
	    }
	    break;
	case matchAndAlso:
	    n = *pc++;
	    if (*--esp == am_false) {
		esp++;
		pc += n;
	    } else if (*esp != am_true) {
		if (do_catch) {
		    *esp++ = FAIL_TERM;
		    pc += n;
		} else {
		    FAIL();
		}
	    }
	    break;
	case matchJump:
	    n = *pc++;
	    pc += n;
	    break;
	case matchSelf:
	    *esp++ = self->common.id;
	    break;
	case matchWaste:
	    --esp;
	    break;
	case matchReturn:
	    ret = *--esp;
	    break;
	case matchProcessDump: {
	    erts_dsprintf_buf_t *dsbufp = erts_create_tmp_dsbuf(0);
            ASSERT(c_p == self);
	    print_process_info(ERTS_PRINT_DSBUF, (void *) dsbufp, c_p);
	    *esp++ = new_binary(build_proc, (byte *)dsbufp->str,
				dsbufp->str_len);
	    erts_destroy_tmp_dsbuf(dsbufp);
	    break;
	}
	case matchDisplay: /* Debugging, not for production! */
	    erts_printf("%T\n", esp[-1]);
	    esp[-1] = am_true;
	    break;
	case matchSetReturnTrace:
	    *return_flags |= MATCH_SET_RETURN_TRACE;
	    *esp++ = am_true;
	    break;
	case matchSetExceptionTrace:
	    *return_flags |= MATCH_SET_EXCEPTION_TRACE;
	    *esp++ = am_true;
	    break;
        case matchIsSeqTrace:
            ASSERT(c_p == self);
            if (have_seqtrace(SEQ_TRACE_TOKEN(c_p)))
		*esp++ = am_true;
	    else
		*esp++ = am_false;
	    break;
	case matchSetSeqToken:
            ASSERT(c_p == self);
            t = erts_seq_trace(c_p, esp[-1], esp[-2], 0);
	    if (is_non_value(t)) {
		esp[-2] = FAIL_TERM;
	    } else {
		esp[-2] = t;
	    }
	    --esp;
	    break;
        case matchSetSeqTokenFake:
            ASSERT(c_p == self);
	    t = seq_trace_fake(c_p, esp[-1]);
	    if (is_non_value(t)) {
		esp[-2] = FAIL_TERM;
	    } else {
		esp[-2] = t;
	    }
	    --esp;
	    break;
        case matchGetSeqToken:
            ASSERT(c_p == self);
            if (have_no_seqtrace(SEQ_TRACE_TOKEN(c_p)))
		*esp++ = NIL;
	    else {
                Eterm token;
                Uint token_sz;

                ASSERT(SEQ_TRACE_TOKEN_ARITY(c_p) == 5);
                ASSERT(is_immed(SEQ_TRACE_TOKEN_FLAGS(c_p)));
                ASSERT(is_immed(SEQ_TRACE_TOKEN_SERIAL(c_p)));
                ASSERT(is_immed(SEQ_TRACE_TOKEN_LASTCNT(c_p)));

                token = SEQ_TRACE_TOKEN(c_p);
                token_sz = size_object(token);

                ehp = HAllocX(build_proc, token_sz, HEAP_XTRA);
                *esp++ = copy_struct(token, token_sz, &ehp, &MSO(build_proc));
	    }
	    break;
        case matchEnableTrace:
            ASSERT(c_p == self);
	    if ( (n = erts_trace_flag2bit(esp[-1]))) {
                erts_proc_lock(c_p, ERTS_PROC_LOCKS_ALL_MINOR);
		set_tracee_flags(c_p, ERTS_TRACER(c_p), 0, n);
                erts_proc_unlock(c_p, ERTS_PROC_LOCKS_ALL_MINOR);
		esp[-1] = am_true;
	    } else {
		esp[-1] = FAIL_TERM;
	    }
	    break;
        case matchEnableTrace2:
            ASSERT(c_p == self);
	    n = erts_trace_flag2bit((--esp)[-1]);
	    esp[-1] = FAIL_TERM;
	    if (n) {
		if ( (tmpp = get_proc(c_p, ERTS_PROC_LOCK_MAIN, esp[0], ERTS_PROC_LOCKS_ALL))) {
		    /* Always take over the tracer of the current process */
		    set_tracee_flags(tmpp, ERTS_TRACER(c_p), 0, n);
                    if (tmpp == c_p)
                        erts_proc_unlock(tmpp, ERTS_PROC_LOCKS_ALL_MINOR);
                    else
                        erts_proc_unlock(tmpp, ERTS_PROC_LOCKS_ALL);
                    esp[-1] = am_true;
		}
	    }
	    break;
        case matchDisableTrace:
            ASSERT(c_p == self);
	    if ( (n = erts_trace_flag2bit(esp[-1]))) {
                erts_proc_lock(c_p, ERTS_PROC_LOCKS_ALL_MINOR);
		set_tracee_flags(c_p, ERTS_TRACER(c_p), n, 0);
                erts_proc_unlock(c_p, ERTS_PROC_LOCKS_ALL_MINOR);
		esp[-1] = am_true;
	    } else {
		esp[-1] = FAIL_TERM;
	    }
	    break;
        case matchDisableTrace2:
            ASSERT(c_p == self);
	    n = erts_trace_flag2bit((--esp)[-1]);
	    esp[-1] = FAIL_TERM;
	    if (n) {
		if ( (tmpp = get_proc(c_p, ERTS_PROC_LOCK_MAIN, esp[0], ERTS_PROC_LOCKS_ALL))) {
		    /* Always take over the tracer of the current process */
		    set_tracee_flags(tmpp, ERTS_TRACER(c_p), n, 0);
                    if (tmpp == c_p)
                        erts_proc_unlock(tmpp, ERTS_PROC_LOCKS_ALL_MINOR);
                    else
                        erts_proc_unlock(tmpp, ERTS_PROC_LOCKS_ALL);
                    esp[-1] = am_true;
		}
	    }
	    break;
        case matchCaller:
            ASSERT(c_p == self);
	    if (!(c_p->cp) || !(cp = find_function_from_pc(c_p->cp))) {
 		*esp++ = am_undefined;
 	    } else {
		ehp = HAllocX(build_proc, 4, HEAP_XTRA);
 		*esp++ = make_tuple(ehp);
		ehp[0] = make_arityval(3);
		ehp[1] = cp->module;
		ehp[2] = cp->function;
		ehp[3] = make_small((Uint) cp->arity);
	    }
	    break;
        case matchSilent:
            ASSERT(c_p == self);
	    --esp;
	    if (in_flags & ERTS_PAM_IGNORE_TRACE_SILENT)
	      break;
	    if (*esp == am_true) {
		erts_proc_lock(c_p, ERTS_PROC_LOCKS_ALL_MINOR);
		ERTS_TRACE_FLAGS(c_p) |= F_TRACE_SILENT;
		erts_proc_unlock(c_p, ERTS_PROC_LOCKS_ALL_MINOR);
	    }
	    else if (*esp == am_false) {
		erts_proc_lock(c_p, ERTS_PROC_LOCKS_ALL_MINOR);
		ERTS_TRACE_FLAGS(c_p) &= ~F_TRACE_SILENT;
		erts_proc_unlock(c_p, ERTS_PROC_LOCKS_ALL_MINOR);
	    }
	    break;
        case matchTrace2:
            ASSERT(c_p == self);
	    {
		/*    disable         enable                                */
		Uint  d_flags  = 0,   e_flags  = 0;  /* process trace flags */
		ErtsTracer tracer = erts_tracer_nil;
		/* XXX Atomicity note: Not fully atomic. Default tracer
		 * is sampled from current process but applied to
		 * tracee and tracer later after releasing main
		 * locks on current process, so ERTS_TRACER_PROC(c_p)
		 * may actually have changed when tracee and tracer
		 * gets updated. I do not think nobody will notice.
		 * It is just the default value that is not fully atomic.
		 * and the real argument settable from match spec
		 * {trace,[],[{{tracer,Tracer}}]} is much, much older.
		 */
		int   cputs = 0;
                erts_tracer_update(&tracer, ERTS_TRACER(c_p));
		
		if (! erts_trace_flags(esp[-1], &d_flags, &tracer, &cputs) ||
		    ! erts_trace_flags(esp[-2], &e_flags, &tracer, &cputs) ||
		    cputs ) {
		    (--esp)[-1] = FAIL_TERM;
                    ERTS_TRACER_CLEAR(&tracer);
		    break;
		}
		erts_proc_lock(c_p, ERTS_PROC_LOCKS_ALL_MINOR);
		(--esp)[-1] = set_match_trace(c_p, FAIL_TERM, tracer,
					      d_flags, e_flags);
		erts_proc_unlock(c_p, ERTS_PROC_LOCKS_ALL_MINOR);
                ERTS_TRACER_CLEAR(&tracer);
	    }
	    break;
        case matchTrace3:
            ASSERT(c_p == self);
	    {
		/*    disable         enable                                */
		Uint  d_flags  = 0,   e_flags  = 0;  /* process trace flags */
		ErtsTracer tracer = erts_tracer_nil;
		/* XXX Atomicity note. Not fully atomic. See above. 
		 * Above it could possibly be solved, but not here.
		 */
		int   cputs = 0;
		Eterm tracee = (--esp)[0];

                erts_tracer_update(&tracer, ERTS_TRACER(c_p));
		
		if (! erts_trace_flags(esp[-1], &d_flags, &tracer, &cputs) ||
		    ! erts_trace_flags(esp[-2], &e_flags, &tracer, &cputs) ||
		    cputs ||
		    ! (tmpp = get_proc(c_p, ERTS_PROC_LOCK_MAIN, 
				       tracee, ERTS_PROC_LOCKS_ALL))) {
		    (--esp)[-1] = FAIL_TERM;
                    ERTS_TRACER_CLEAR(&tracer);
		    break;
		}
		if (tmpp == c_p) {
		    (--esp)[-1] = set_match_trace(c_p, FAIL_TERM, tracer,
						  d_flags, e_flags);
		    erts_proc_unlock(c_p, ERTS_PROC_LOCKS_ALL_MINOR);
		} else {
		    erts_proc_unlock(c_p, ERTS_PROC_LOCK_MAIN);
		    (--esp)[-1] = set_match_trace(tmpp, FAIL_TERM, tracer,
						  d_flags, e_flags);
		    erts_proc_unlock(tmpp, ERTS_PROC_LOCKS_ALL);
		    erts_proc_lock(c_p, ERTS_PROC_LOCK_MAIN);
		}
                ERTS_TRACER_CLEAR(&tracer);
	    }
	    break;
	case matchCatch:  /* Match success, now build result */
	    do_catch = 1;
	    if (in_flags & ERTS_PAM_COPY_RESULT) {
		build_proc = c_p;
                if (esdp)
                    esdp->current_process = c_p;
	    }
	    break;
	case matchHalt:
	    goto success;
	default:
	    erts_exit(ERTS_ERROR_EXIT, "Internal error: unexpected opcode in match program.");
	}
    }
fail:
    *return_flags = 0U;
    if (fail_label >= 0) { /* We failed during a "TryMeElse",
			      lets restart, with the next match
			      program */
	pc = (prog->text) + fail_label;
	cleanup_match_pseudo_process(mpsp, 1);
	goto restart;
    }
    ret = THE_NON_VALUE;
success:

#ifdef DMC_DEBUG
    if (*heap_fence != FENCE_PATTERN) {
	erts_exit(ERTS_ERROR_EXIT, "Heap fence overwritten in db_prog_match after op "
		 "0x%08x, overwritten with 0x%08x.", save_op, *heap_fence);
    }
    if (*stack_fence != FENCE_PATTERN) {
	erts_exit(ERTS_ERROR_EXIT, "Stack fence overwritten in db_prog_match after op "
		 "0x%08x, overwritten with 0x%08x.", save_op, 
		 *stack_fence);
    }
#endif

    if (esdp)
        esdp->current_process = current_scheduled;

    return ret;
#undef FAIL
#undef FAIL_TERM
}


DMCErrInfo *db_new_dmc_err_info(void) 
{
    DMCErrInfo *ret = erts_alloc(ERTS_ALC_T_DB_DMC_ERR_INFO,
				 sizeof(DMCErrInfo));
    ret->var_trans = NULL;
    ret->num_trans = 0;
    ret->error_added = 0;
    ret->first = NULL;
    return ret;
}

Eterm db_format_dmc_err_info(Process *p, DMCErrInfo *ei)
{
    int sl;
    int vnum;
    DMCError *tmp;
    Eterm *shp;
    Eterm ret = NIL;
    Eterm tlist, tpl, sev;
    char buff[DMC_ERR_STR_LEN + 20 /* for the number */];

    for (tmp = ei->first; tmp != NULL; tmp = tmp->next) {
	if (tmp->variable >= 0 && 
	    tmp->variable < ei->num_trans &&
	    ei->var_trans != NULL) {
	    vnum = (int) ei->var_trans[tmp->variable];
	} else {
	    vnum = tmp->variable;
	}
	if (vnum >= 0)
	    erts_snprintf(buff,sizeof(buff)+20,tmp->error_string, vnum);
	else
	    sys_strcpy(buff,tmp->error_string);
	sl = sys_strlen(buff);
	shp = HAlloc(p, sl * 2 + 5);
	sev = (tmp->severity == dmcWarning) ? 
	    am_atom_put("warning",7) :
	    am_error;
	tlist = buf_to_intlist(&shp, buff, sl, NIL);
	tpl = TUPLE2(shp, sev, tlist);
	shp += 3;
	ret = CONS(shp, tpl, ret);
	shp += 2;
    }
    return ret;
}

void db_free_dmc_err_info(DMCErrInfo *ei){
    while (ei->first != NULL) {
	DMCError *ll = ei->first->next;
	erts_free(ERTS_ALC_T_DB_DMC_ERROR, ei->first);
	ei->first = ll;
    }
    if (ei->var_trans)
	erts_free(ERTS_ALC_T_DB_TRANS_TAB, ei->var_trans);
    erts_free(ERTS_ALC_T_DB_DMC_ERR_INFO, ei);
}

/* Calculate integer addition: counter+incr.
** Store bignum in *hpp and increase *hpp accordingly.
** *hpp is assumed to be large enough to hold the result.
*/
Eterm db_add_counter(Eterm** hpp, Wterm counter, Eterm incr)
{
    DeclareTmpHeapNoproc(big_tmp,2);
    Eterm res;
    Sint ires;
    Wterm arg1;
    Wterm arg2;

    if (is_both_small(counter,incr)) {
	ires = signed_val(counter) + signed_val(incr);
	if (IS_SSMALL(ires)) {
	    return make_small(ires);
	} else {
	    res = small_to_big(ires, *hpp);
	    ASSERT(BIG_NEED_SIZE(big_size(res))==2);
	    *hpp += 2;
	    return res;
	}
    }
    else {
	UseTmpHeapNoproc(2);
	switch(NUMBER_CODE(counter, incr)) {
	case SMALL_BIG:
	    arg1 = small_to_big(signed_val(counter), big_tmp);
	    arg2 = incr;
	    break;
	case BIG_SMALL:
	    arg1 = counter;
	    arg2 = small_to_big(signed_val(incr), big_tmp);
	    break;
	case BIG_BIG:
	    arg1 = incr;
	    arg2 = counter;
	    break;
	default:
	    UnUseTmpHeapNoproc(2);
	    return THE_NON_VALUE;
	}
	res = big_plus(arg1, arg2, *hpp);
	if (is_big(res)) {
	    *hpp += BIG_NEED_SIZE(big_size(res));
	}
	UnUseTmpHeapNoproc(2);
	return res;
    }
}

/* Must be called to read elements after db_lookup_dbterm.
** Will decompress if needed.
** HEALFWORD_HEAP:
** 	Will convert from relative to Wterm format if needed.
**      (but only on top level, tuples and lists will still contain rterms)
*/
Wterm db_do_read_element(DbUpdateHandle* handle, Sint position)
{
    Eterm elem = handle->dbterm->tpl[position];
    if (!is_header(elem)) {
	return elem;
    }

    ASSERT(((DbTableCommon*)handle->tb)->compress);
    ASSERT(!(handle->flags & DB_MUST_RESIZE));
    handle->dbterm = db_alloc_tmp_uncompressed(&handle->tb->common,
					       handle->dbterm);
    handle->flags |= DB_MUST_RESIZE;
    return handle->dbterm->tpl[position];
}

/*
** Update one element:
** handle:   Initialized by db_lookup_dbterm()
** position: The tuple position of the elements to be updated.
** newval:   The new value of the element.
** Can not fail.
*/
void db_do_update_element(DbUpdateHandle* handle,
			  Sint position,
			  Eterm newval)
{
    Eterm oldval = handle->dbterm->tpl[position];
    Eterm* newp;
    Eterm* oldp;
    Uint newval_sz;
    Uint oldval_sz;

    if (is_both_immed(newval,oldval)) {
	handle->dbterm->tpl[position] = newval;
    #ifdef DEBUG_CLONE
	if (handle->dbterm->debug_clone) {
	    handle->dbterm->debug_clone[position] = newval;
	}
    #endif
	return;
    }
    if (!(handle->flags & DB_MUST_RESIZE)) {
	if (handle->tb->common.compress) {
	    handle->dbterm = db_alloc_tmp_uncompressed(&handle->tb->common,
						       handle->dbterm);
	    handle->flags |= DB_MUST_RESIZE;
	    oldval = handle->dbterm->tpl[position];
	}
	else {
	    if (is_boxed(newval)) {
		newp = boxed_val(newval);
		switch (*newp & _TAG_HEADER_MASK) {
		case _TAG_HEADER_POS_BIG:
		case _TAG_HEADER_NEG_BIG:
		case _TAG_HEADER_FLOAT:
		case _TAG_HEADER_HEAP_BIN:
		    newval_sz = header_arity(*newp) + 1;
		    if (is_boxed(oldval)) {
			oldp = boxed_val(oldval);
			switch (*oldp & _TAG_HEADER_MASK) {
			case _TAG_HEADER_POS_BIG:
			case _TAG_HEADER_NEG_BIG:
			case _TAG_HEADER_FLOAT:
			case _TAG_HEADER_HEAP_BIN:
			    oldval_sz = header_arity(*oldp) + 1;
			    if (oldval_sz == newval_sz) {
				/* "self contained" terms of same size, do memcpy */
				    sys_memcpy(oldp, newp, newval_sz*sizeof(Eterm));
				return;
			    }
			    goto both_size_set;
			}
		    }
		    goto new_size_set;
		}
	    }
	}
    }
    /* Not possible for simple memcpy or dbterm is already non-contiguous, */
    /* need to realloc... */

    newval_sz = is_immed(newval) ? 0 : size_object(newval);
new_size_set:

    oldval_sz = is_immed(oldval) ? 0 : size_object(oldval);
both_size_set:

    handle->new_size = handle->new_size - oldval_sz + newval_sz;

    /* write new value in old dbterm, finalize will make a flat copy */
    handle->dbterm->tpl[position] = newval;
    handle->flags |= DB_MUST_RESIZE;
}

static ERTS_INLINE byte* db_realloc_term(DbTableCommon* tb, void* old,
					 Uint old_sz, Uint new_sz, Uint offset)
{
    byte* ret;
    if (erts_ets_realloc_always_moves) {
	ret = erts_db_alloc(ERTS_ALC_T_DB_TERM, (DbTable*)tb, new_sz);
	sys_memcpy(ret, old, offset);
	erts_db_free(ERTS_ALC_T_DB_TERM, (DbTable*)tb, old, old_sz);
    } else {
	ret = erts_db_realloc(ERTS_ALC_T_DB_TERM, (DbTable*)tb,
			      old, old_sz, new_sz);
    }
    return ret;
}

/* Allocated size of a compressed dbterm
*/
static ERTS_INLINE Uint db_alloced_size_comp(DbTerm* obj)
{
    return obj->tpl[arityval(*obj->tpl) + 1];
}

void db_free_term(DbTable *tb, void* basep, Uint offset)
{
    DbTerm* db = (DbTerm*) ((byte*)basep + offset);
    Uint size;
    if (tb->common.compress) {
	db_cleanup_offheap_comp(db);
	size = db_alloced_size_comp(db);
    }
    else {
	ErlOffHeap tmp_oh;
	tmp_oh.first = db->first_oh;
	erts_cleanup_offheap(&tmp_oh);
	size = offset + offsetof(DbTerm,tpl) + db->size*sizeof(Eterm);
    }
    erts_db_free(ERTS_ALC_T_DB_TERM, tb, basep, size);
}

static ERTS_INLINE Uint align_up(Uint value, Uint pow2)
{
    ASSERT((pow2 & (pow2-1)) == 0);
    return (value + (pow2-1)) & ~(pow2-1);
}

/* Compressed size of an uncompressed term
*/
static Uint db_size_dbterm_comp(DbTableCommon* tb, Eterm obj)
{
    Eterm* tpl = tuple_val(obj);
    int i;
    Uint size = sizeof(DbTerm)
	+ arityval(*tpl) * sizeof(Eterm)
        + sizeof(Uint); /* "alloc_size" */

    for (i = arityval(*tpl); i>0; i--) {
	if (i != tb->keypos && is_not_immed(tpl[i])) {
	    size += erts_encode_ext_size_ets(tpl[i]);
	}
    }
    size += size_object(tpl[tb->keypos]) * sizeof(Eterm);
    return align_up(size, sizeof(Uint));
}

/* Conversion between top tuple element and pointer to compressed data
*/
static ERTS_INLINE Eterm ext2elem(Eterm* tpl, byte* ext)
{
    return (((Uint)(ext - (byte*)tpl)) << _TAG_PRIMARY_SIZE) | TAG_PRIMARY_HEADER;
}
static ERTS_INLINE byte* elem2ext(Eterm* tpl, Uint ix)
{
    ASSERT(is_header(tpl[ix]));
    return (byte*)tpl + (tpl[ix] >> _TAG_PRIMARY_SIZE);
}

static void* copy_to_comp(DbTableCommon* tb, Eterm obj, DbTerm* dest,
			  Uint alloc_size)
{
    ErlOffHeap tmp_offheap;
    Eterm* src = tuple_val(obj);
    Eterm* tpl = dest->tpl;
    Eterm key = src[tb->keypos];
    int arity = arityval(src[0]);
    union {
	Eterm* ep;
	byte* cp;
	UWord ui;
    }top;
    int i;

    top.ep = tpl+ 1 + arity + 1;
    tpl[0] = src[0];
    tpl[arity + 1] = alloc_size;

    tmp_offheap.first = NULL;
    tpl[tb->keypos] = copy_struct(key, size_object(key), &top.ep, &tmp_offheap);
    dest->first_oh = tmp_offheap.first;
    for (i=1; i<=arity; i++) {
	if (i != tb->keypos) {
	    if (is_immed(src[i])) {
		tpl[i] = src[i];
	    }
	    else {
		tpl[i] = ext2elem(tpl, top.cp);
		top.cp = erts_encode_ext_ets(src[i], top.cp, &dest->first_oh);
	    }
	}
    }

#ifdef DEBUG_CLONE
    {
	Eterm* dbg_top = erts_alloc(ERTS_ALC_T_DB_TERM, dest->size * sizeof(Eterm));
	dest->debug_clone = dbg_top;
	tmp_offheap.first = dest->first_oh;
	copy_struct(obj, dest->size, &dbg_top, &tmp_offheap);
	dest->first_oh = tmp_offheap.first;
	ASSERT(dbg_top == dest->debug_clone + dest->size);
    }
#endif
    return top.cp;
}

/*
** Copy the object into a possibly new DbTerm, 
** offset is the offset of the DbTerm from the start
** of the allocated structure, The possibly realloced and copied
** structure is returned. Make sure (((char *) old) - offset) is a 
** pointer to a ERTS_ALC_T_DB_TERM allocated data area.
*/
void* db_store_term(DbTableCommon *tb, DbTerm* old, Uint offset, Eterm obj)
{
    byte* basep;
    DbTerm* newp;
    Eterm* top;
    int size = size_object(obj);
    ErlOffHeap tmp_offheap;

    if (old != 0) {
	basep = ((byte*) old) - offset;
	tmp_offheap.first  = old->first_oh;
	erts_cleanup_offheap(&tmp_offheap);
	old->first_oh = tmp_offheap.first;
	if (size == old->size) {
	    newp = old;
	}
	else {
	    Uint new_sz = offset + sizeof(DbTerm) + sizeof(Eterm)*(size-1);
	    Uint old_sz = offset + sizeof(DbTerm) + sizeof(Eterm)*(old->size-1);

	    basep = db_realloc_term(tb, basep, old_sz, new_sz, offset);
	    newp = (DbTerm*) (basep + offset);
	}
    }
    else {
	basep = erts_db_alloc(ERTS_ALC_T_DB_TERM, (DbTable *)tb,
			      (offset + sizeof(DbTerm) + sizeof(Eterm)*(size-1)));
	newp = (DbTerm*) (basep + offset);
    }
    newp->size = size;
    top = newp->tpl;
    tmp_offheap.first  = NULL;
    copy_struct(obj, size, &top, &tmp_offheap);
    newp->first_oh = tmp_offheap.first;
#ifdef DEBUG_CLONE
    newp->debug_clone = NULL;
#endif
    return basep;
}


void* db_store_term_comp(DbTableCommon *tb, DbTerm* old, Uint offset, Eterm obj)
{
    Uint new_sz = offset + db_size_dbterm_comp(tb, obj);
    byte* basep;
    DbTerm* newp;
#ifdef DEBUG
    byte* top;
#endif

    ASSERT(tb->compress);
    if (old != 0) {
	Uint old_sz = db_alloced_size_comp(old);
	db_cleanup_offheap_comp(old);

	basep = ((byte*) old) - offset;
	if (new_sz == old_sz) {
	    newp = old;
	}
	else {
	    basep = db_realloc_term(tb, basep, old_sz, new_sz, offset);
	    newp = (DbTerm*) (basep + offset);
	}
    }
    else {
	basep = erts_db_alloc(ERTS_ALC_T_DB_TERM, (DbTable*)tb, new_sz);
	newp = (DbTerm*) (basep + offset);
    }

    newp->size = size_object(obj);
#ifdef DEBUG
    top = 
#endif
	copy_to_comp(tb, obj, newp, new_sz);
    ASSERT(top <= basep + new_sz);

    /* ToDo: Maybe realloc if ((basep+new_sz) - top) > WASTED_SPACE_LIMIT */

    return basep;
}


void db_finalize_resize(DbUpdateHandle* handle, Uint offset)
{
    DbTable* tbl = handle->tb;
    DbTerm* newDbTerm;
    Uint alloc_sz = offset +
	(tbl->common.compress ?
	 db_size_dbterm_comp(&tbl->common, make_tuple(handle->dbterm->tpl)) :
	 sizeof(DbTerm)+sizeof(Eterm)*(handle->new_size-1));
    byte* newp = erts_db_alloc(ERTS_ALC_T_DB_TERM, tbl, alloc_sz);
    byte* oldp = *(handle->bp);

    sys_memcpy(newp, oldp, offset);  /* copy only hash/tree header */
    *(handle->bp) = newp;
    newDbTerm = (DbTerm*) (newp + offset);
    newDbTerm->size = handle->new_size;
#ifdef DEBUG_CLONE
    newDbTerm->debug_clone = NULL;
#endif

    /* make a flat copy */

    if (tbl->common.compress) {
	copy_to_comp(&tbl->common, make_tuple(handle->dbterm->tpl),
		     newDbTerm, alloc_sz);
	db_free_tmp_uncompressed(handle->dbterm);
    }
    else {
	ErlOffHeap tmp_offheap;
	Eterm* tpl = handle->dbterm->tpl;
	Eterm* top = newDbTerm->tpl;

	tmp_offheap.first = NULL;

	{
	    copy_struct(make_tuple(tpl), handle->new_size, &top, &tmp_offheap);
	    newDbTerm->first_oh = tmp_offheap.first;
	    ASSERT((byte*)top == (newp + alloc_sz));
	}
    }
}

Eterm db_copy_from_comp(DbTableCommon* tb, DbTerm* bp, Eterm** hpp,
			     ErlOffHeap* off_heap)
{
    Eterm* hp = *hpp;
    int i, arity = arityval(bp->tpl[0]);
    ErtsHeapFactory factory;

    hp[0] = bp->tpl[0];
    *hpp += arity + 1;

    hp[tb->keypos] = copy_struct(bp->tpl[tb->keypos],
                                 size_object(bp->tpl[tb->keypos]),
                                 hpp, off_heap);

    erts_factory_static_init(&factory, *hpp, bp->size - (arity+1), off_heap);

    for (i=arity; i>0; i--) {
	if (i != tb->keypos) {
	    if (is_immed(bp->tpl[i])) {
		hp[i] = bp->tpl[i];
	    }
	    else {
		hp[i] = erts_decode_ext_ets(&factory,
					    elem2ext(bp->tpl, i));
	    }
	}
    }
    *hpp = factory.hp;
    erts_factory_close(&factory);

    ASSERT((*hpp - hp) <= bp->size);
#ifdef DEBUG_CLONE
    ASSERT(EQ(make_tuple(hp),make_tuple(bp->debug_clone)));
#endif
    return make_tuple(hp);
}

Eterm db_copy_element_from_ets(DbTableCommon* tb, Process* p,
			       DbTerm* obj, Uint pos,
			       Eterm** hpp, Uint extra)
{
    if (is_immed(obj->tpl[pos])) {
	*hpp = HAlloc(p, extra);
	return obj->tpl[pos];
    }
    if (tb->compress && pos != tb->keypos) {
	byte* ext = elem2ext(obj->tpl, pos);
	Sint sz = erts_decode_ext_size_ets(ext, db_alloced_size_comp(obj)) + extra;
	Eterm copy;
        ErtsHeapFactory factory;

        erts_factory_proc_prealloc_init(&factory, p, sz);
        copy = erts_decode_ext_ets(&factory, ext);
	*hpp = erts_produce_heap(&factory, extra, 0);
        erts_factory_close(&factory);
#ifdef DEBUG_CLONE
	ASSERT(EQ(copy, obj->debug_clone[pos]));
#endif
	return copy;
    }
    else {
	Uint sz = size_object(obj->tpl[pos]);
	*hpp = HAlloc(p, sz + extra);
	return copy_struct(obj->tpl[pos], sz, hpp, &MSO(p));
    }
}


/* Our own "cleanup_offheap"
 * as refc-binaries may be unaligned in compressed terms
*/
void db_cleanup_offheap_comp(DbTerm* obj)
{
    union erl_off_heap_ptr u;
    ProcBin tmp;

    for (u.hdr = obj->first_oh; u.hdr; u.hdr = u.hdr->next) {
	if ((UWord)u.voidp % sizeof(Uint) != 0) { /* unaligned ptr */
	    sys_memcpy(&tmp, u.voidp, sizeof(tmp));
	    /* Warning, must pass (void*)-variable to memcpy. Otherwise it will
	       cause Bus error on Sparc due to false compile time assumptions
	       about word aligned memory (type cast is not enough) */
	    u.pb = &tmp;
	}
	switch (thing_subtag(u.hdr->thing_word)) {
	case REFC_BINARY_SUBTAG:
            erts_bin_release(u.pb->val);
	    break;
	case FUN_SUBTAG:
	    ASSERT(u.pb != &tmp);
	    if (erts_refc_dectest(&u.fun->fe->refc, 0) == 0) {
		erts_erase_fun_entry(u.fun->fe);
	    }
	    break;
	case REF_SUBTAG:
	    ASSERT(is_magic_ref_thing(u.hdr));
            erts_bin_release((Binary *)u.mref->mb);
	    break;
	default:
	    ASSERT(is_external_header(u.hdr->thing_word));
	    ASSERT(u.pb != &tmp);
	    erts_deref_node_entry(u.ext->node);
	    break;
	}
    }
#ifdef DEBUG_CLONE
    if (obj->debug_clone != NULL) {
	erts_free(ERTS_ALC_T_DB_TERM, obj->debug_clone);
	obj->debug_clone = NULL;
    }
#endif
}

int db_eq_comp(DbTableCommon* tb, Eterm a, DbTerm* b)
{
    ErlOffHeap tmp_offheap;
    Eterm* allocp;
    Eterm* hp;
    Eterm tmp_b;
    int is_eq;

    ASSERT(tb->compress);
    hp = allocp = erts_alloc(ERTS_ALC_T_TMP, b->size*sizeof(Eterm));
    tmp_offheap.first = NULL;
    tmp_b = db_copy_from_comp(tb, b, &hp, &tmp_offheap);
    is_eq = eq(a,tmp_b);
    erts_cleanup_offheap(&tmp_offheap);
    erts_free(ERTS_ALC_T_TMP, allocp);
    return is_eq;
}

/*
** Check if object represents a "match" variable 
** i.e and atom $N where N is an integer 
**
*/

int db_is_variable(Eterm obj)
{
    byte *b;
    int n;
    int N;

    if (is_not_atom(obj))
        return -1;
    b = atom_tab(atom_val(obj))->name;
    if ((n = atom_tab(atom_val(obj))->len) < 2)
        return -1;
    if (*b++ != '$')
        return -1;
    n--;
    /* Handle first digit */
    if (*b == '0')
        return (n == 1) ? 0 : -1;
    if (*b >= '1' && *b <= '9')
        N = *b++ - '0';
    else
        return -1;
    n--;
    while(n--) {
        if (*b >= '0' && *b <= '9') {
            N = N*10 + (*b - '0');
            b++;
        }
        else
            return -1;
    }
    return N;
}

/* check if node is (or contains) a map
 * return 1 if node contains a map
 * return 0 otherwise
 */

int db_has_map(Eterm node) {
    DECLARE_ESTACK(s);

    ESTACK_PUSH(s,node);
    while (!ESTACK_ISEMPTY(s)) {
	node = ESTACK_POP(s);
        if (is_list(node)) {
	    while (is_list(node)) {
		ESTACK_PUSH(s,CAR(list_val(node)));
		node = CDR(list_val(node));
	    }
	    ESTACK_PUSH(s,node);    /* Non wellformed list or [] */
        } else if (is_tuple(node)) {
            Eterm *tuple = tuple_val(node);
            int arity = arityval(*tuple);
            while(arity--) {
                ESTACK_PUSH(s,*(++tuple));
            }
        } else if is_map(node) {
            DESTROY_ESTACK(s);
            return 1;
        }
    }
    DESTROY_ESTACK(s);
    return 0;
}

/* check if obj is (or contains) a variable */
/* return 1 if obj contains a variable or underscore */
/* return 0 if obj is fully ground                   */

int db_has_variable(Eterm node) {
    DECLARE_ESTACK(s);

    ESTACK_PUSH(s,node);
    while (!ESTACK_ISEMPTY(s)) {
	node = ESTACK_POP(s);
	switch(node & _TAG_PRIMARY_MASK) {
	case TAG_PRIMARY_LIST:
	    while (is_list(node)) {
		ESTACK_PUSH(s,CAR(list_val(node)));
		node = CDR(list_val(node));
	    }
	    ESTACK_PUSH(s,node);    /* Non wellformed list or [] */
	    break;
	case TAG_PRIMARY_BOXED:
	    if (is_tuple(node)) {
		Eterm *tuple = tuple_val(node);
		int arity = arityval(*tuple);
		while(arity--) {
		    ESTACK_PUSH(s,*(++tuple));
		}
            } else if (is_flatmap(node)) {
                Eterm *values = flatmap_get_values(flatmap_val(node));
                Uint size = flatmap_get_size(flatmap_val(node));
                ESTACK_PUSH(s, ((flatmap_t *) flatmap_val(node))->keys);
                while (size--) {
                    ESTACK_PUSH(s, *(values++));
                }
            } else if (is_map(node)) { /* other map-nodes or map-heads */
                Eterm *ptr = hashmap_val(node);
                int i = hashmap_bitcount(MAP_HEADER_VAL(*ptr));
                ptr += MAP_HEADER_ARITY(*ptr);
                while(i--) { ESTACK_PUSH(s, *++ptr); }
            }
	    break;
	case TAG_PRIMARY_IMMED1:
	    if (node == am_Underscore || db_is_variable(node) >= 0) {
		DESTROY_ESTACK(s);
		return 1;
	    }
	    break;
	}
    }
    DESTROY_ESTACK(s);
    return 0;
}

/* 
** Local (static) utilities.
*/

/*
***************************************************************************
** Compiled matches 
***************************************************************************
*/
/*
** Utility to add an error
*/

static void vadd_dmc_err(DMCErrInfo *err_info,
                         DMCErrorSeverity severity,
                         int variable,
                         const char *str,
                         ...)
{
    DMCError *e;
    va_list args;
    va_start(args, str);


    /* Linked in in reverse order, to ease the formatting */
    e = erts_alloc(ERTS_ALC_T_DB_DMC_ERROR, sizeof(DMCError));
    erts_vsnprintf(e->error_string, DMC_ERR_STR_LEN, str, args);
    e->variable = variable;
    e->severity = severity;
    e->next = err_info->first;
#ifdef HARDDEBUG
    erts_fprintf(stderr,"add_dmc_err: %s\n",e->error_string);
#endif
    err_info->first = e;
    if (severity >= dmcError)
	err_info->error_added = 1;

    va_end(args);
}
    

/*
** Handle one term in the match expression (not the guard) 
*/
static DMCRet dmc_one_term(DMCContext *context, 
			   DMCHeap *heap,
			   DMC_STACK_TYPE(Eterm) *stack,
			   DMC_STACK_TYPE(UWord) *text,
			   Eterm c)
{
    Sint n;
    Eterm *hp;
    Uint sz, sz2, sz3;
    Uint i, j;

    switch (c & _TAG_PRIMARY_MASK) {
    case TAG_PRIMARY_IMMED1:
	if ((n = db_is_variable(c)) >= 0) { /* variable */
	    if (n >= heap->size) {
		/*
		** Ouch, big integer in match variable.
		*/
		Eterm *save_hp;
		ASSERT(heap->vars == heap->vars_def);
		sz = sz2 = sz3 = 0;
		for (j = 0; j < context->num_match; ++j) {
		    sz += size_object(context->matchexpr[j]);
		    sz2 += size_object(context->guardexpr[j]);
		    sz3 += size_object(context->bodyexpr[j]);
		}
		context->copy = 
		    new_message_buffer(sz + sz2 + sz3 +
				       context->num_match);
		save_hp = hp = context->copy->mem;
		hp += context->num_match;
		for (j = 0; j < context->num_match; ++j) {
		    context->matchexpr[j] = 
			copy_struct(context->matchexpr[j], 
				    size_object(context->matchexpr[j]), &hp, 
				    &(context->copy->off_heap));
		    context->guardexpr[j] = 
			copy_struct(context->guardexpr[j], 
				    size_object(context->guardexpr[j]), &hp, 
				    &(context->copy->off_heap));
		    context->bodyexpr[j] = 
			copy_struct(context->bodyexpr[j], 
				    size_object(context->bodyexpr[j]), &hp, 
				    &(context->copy->off_heap));
		}
		for (j = 0; j < context->num_match; ++j) {
		    /* the actual expressions can be 
		       atoms in their selves, place them first */
		    *save_hp++ = context->matchexpr[j]; 
		}
		heap->size = match_compact(context->copy, 
					   context->err_info);
		for (j = 0; j < context->num_match; ++j) {
		    /* restore the match terms, as they
		       may be atoms that changed */
		    context->matchexpr[j] = context->copy->mem[j];
		}
		heap->vars = erts_alloc(ERTS_ALC_T_DB_MS_CMPL_HEAP,
					heap->size*sizeof(DMCVariable));
		sys_memset(heap->vars, 0, heap->size * sizeof(DMCVariable));
		DMC_CLEAR(*stack);
		/*DMC_PUSH(*stack,NIL);*/
		DMC_CLEAR(*text);
		return retRestart;
	    }
	    if (heap->vars[n].is_bound) {
		DMC_PUSH2(*text, matchCmp, n);
	    } else { /* Not bound, bind! */
		if (n >= heap->vars_used)
		    heap->vars_used = n + 1;
		DMC_PUSH2(*text, matchBind, n);
		heap->vars[n].is_bound = 1;
	    }
	} else if (c == am_Underscore) {
	    DMC_PUSH(*text, matchSkip);
	} else { /* Any immediate value */
	    DMC_PUSH2(*text, matchEq, (Uint) c);
	}
	break;
    case TAG_PRIMARY_LIST:
	DMC_PUSH(*text, matchPushL);
	++(context->stack_used);
	DMC_PUSH(*stack, c); 
	break;
    case TAG_PRIMARY_BOXED: {
	Eterm hdr = *boxed_val(c);
	switch ((hdr & _TAG_HEADER_MASK) >> _TAG_PRIMARY_SIZE) {
	case (_TAG_HEADER_ARITYVAL >> _TAG_PRIMARY_SIZE):    
	    n = arityval(*tuple_val(c));
	    DMC_PUSH2(*text, matchPushT, n);
	    ++(context->stack_used);
	    DMC_PUSH(*stack, c);
	    break;
        case (_TAG_HEADER_MAP >> _TAG_PRIMARY_SIZE):
            if (is_flatmap(c))
                n = flatmap_get_size(flatmap_val(c));
            else
                n = hashmap_size(c);
            DMC_PUSH2(*text, matchPushM, n);
            ++(context->stack_used);
            DMC_PUSH(*stack, c);
            break;
	case (_TAG_HEADER_REF >> _TAG_PRIMARY_SIZE):
	{
	    Eterm* ref_val = internal_ref_val(c);
	    DMC_PUSH(*text, matchEqRef);
	    n = thing_arityval(ref_val[0]);
	    for (i = 0; i <= n; ++i) {
		DMC_PUSH(*text, ref_val[i]);
	    }
	    break;
	}
	case (_TAG_HEADER_POS_BIG >> _TAG_PRIMARY_SIZE):
	case (_TAG_HEADER_NEG_BIG >> _TAG_PRIMARY_SIZE):
	{
	    Eterm* bval = big_val(c);
	    n = thing_arityval(bval[0]);
	    DMC_PUSH(*text, matchEqBig);
	    for (i = 0; i <= n; ++i) {
		DMC_PUSH(*text, (Uint) bval[i]);
	    }
	    break;
	}
	case (_TAG_HEADER_FLOAT >> _TAG_PRIMARY_SIZE):
	    DMC_PUSH2(*text, matchEqFloat, (Uint) float_val(c)[1]);
#ifdef ARCH_64
	    DMC_PUSH(*text, (Uint) 0);
#else
	    DMC_PUSH(*text, (Uint) float_val(c)[2]);
#endif
	    break;
	default: /* BINARY, FUN, VECTOR, or EXTERNAL */
	    DMC_PUSH2(*text, matchEqBin, dmc_private_copy(context, c));
	    break;
	}
	break;
    }
    default:
	erts_exit(ERTS_ERROR_EXIT, "db_match_compile: "
		 "Bad object on heap: 0x%bex\n", c);
    }
    return retOk;
}

/*
** Make a private copy of a term in a context.
*/

static Eterm
dmc_private_copy(DMCContext *context, Eterm c)
{
    Uint n = size_object(c);
    ErlHeapFragment *tmp_mb = new_message_buffer(n);
    Eterm *hp = tmp_mb->mem;
    Eterm copy = copy_struct(c, n, &hp, &(tmp_mb->off_heap));
    tmp_mb->next = context->save;
    context->save = tmp_mb;
    return copy;
}

/*
** Match guard compilation
*/

static void do_emit_constant(DMCContext *context, DMC_STACK_TYPE(UWord) *text,
			     Eterm t) 
{
	int sz;
	ErlHeapFragment *emb;
	Eterm *hp;
	Eterm tmp;

	if (IS_CONST(t)) {
	    tmp = t;
	} else {
	    sz = my_size_object(t);
	    emb = new_message_buffer(sz);
	    hp = emb->mem;
	    tmp = my_copy_struct(t,&hp,&(emb->off_heap));
	    emb->next = context->save;
	    context->save = emb;
	}
	DMC_PUSH2(*text, matchPushC, (Uint)tmp);
	if (++context->stack_used > context->stack_need)
	    context->stack_need = context->stack_used;
}

#define RETURN_ERROR_X(VAR, ContextP, ConstantF, String, ARG)            \
    (((ContextP)->err_info != NULL)				         \
     ? ((ConstantF) = 0,						 \
        vadd_dmc_err((ContextP)->err_info, dmcError, VAR, String, ARG),  \
        retOk)						                 \
     : retFail)

#define RETURN_ERROR(String, ContextP, ConstantF) \
     return RETURN_ERROR_X(-1, ContextP, ConstantF, String, 0)

#define RETURN_VAR_ERROR(String, N, ContextP, ConstantF) \
     return RETURN_ERROR_X(N, ContextP, ConstantF, String, 0)

#define RETURN_TERM_ERROR(String, T, ContextP, ConstantF) \
     return RETURN_ERROR_X(-1, ContextP, ConstantF, String, T)

#define WARNING(String, ContextP) \
add_dmc_err((ContextP)->err_info, String, -1, 0UL, dmcWarning)

#define VAR_WARNING(String, N, ContextP) \
add_dmc_err((ContextP)->err_info, String, N, 0UL, dmcWarning)

#define TERM_WARNING(String, T, ContextP) \
add_dmc_err((ContextP)->err_info, String, -1, T, dmcWarning)

static DMCRet dmc_list(DMCContext *context,
		       DMCHeap *heap,
		       DMC_STACK_TYPE(UWord) *text,
		       Eterm t,
		       int *constant)
{
    int c1;
    int c2;
    int ret;

    if ((ret = dmc_expr(context, heap, text, CAR(list_val(t)), &c1)) != retOk)
	return ret;

    if ((ret = dmc_expr(context, heap, text, CDR(list_val(t)), &c2)) != retOk)
	return ret;

    if (c1 && c2) {
	*constant = 1;
	return retOk;
    } 
    *constant = 0;
    if (!c1) {
	/* The CAR is not a constant, so if the CDR is, we just push it,
	   otherwise it is already pushed. */
	if (c2)
	    do_emit_constant(context, text, CDR(list_val(t)));
	DMC_PUSH(*text, matchConsA);
    } else { /* !c2 && c1 */
	do_emit_constant(context, text, CAR(list_val(t)));
	DMC_PUSH(*text, matchConsB);
    }
    --context->stack_used; /* Two objects on stack becomes one */
    return retOk;
}

static void
dmc_rearrange_constants(DMCContext *context, DMC_STACK_TYPE(UWord) *text,
                        int textpos, Eterm *p, Uint nelems)
{
    DMC_STACK_TYPE(UWord) instr_save;
    Uint i;

    DMC_INIT_STACK(instr_save);
    while (DMC_STACK_NUM(*text) > textpos) {
        DMC_PUSH(instr_save, DMC_POP(*text));
    }
    for (i = nelems; i--;) {
        do_emit_constant(context, text, p[i]);
    }
    while(!DMC_EMPTY(instr_save)) {
        DMC_PUSH(*text, DMC_POP(instr_save));
    }
    DMC_FREE(instr_save);
}

static DMCRet
dmc_array(DMCContext *context, DMCHeap *heap, DMC_STACK_TYPE(UWord) *text,
          Eterm *p, Uint nelems, int *constant)
{
    int all_constant = 1;
    int textpos = DMC_STACK_NUM(*text);
    Uint i;

    /*
    ** We remember where we started to layout code,
    ** assume all is constant and back up and restart if not so.
    ** The array should be laid out with the last element first,
    ** so we can memcpy it to the eheap.
    */
    for (i = nelems; i--;) {
        DMCRet ret;
        int c;

        ret = dmc_expr(context, heap, text, p[i], &c);
        if (ret != retOk) {
            return ret;
        }
        if (!c && all_constant) {
            all_constant = 0;
            if (i < nelems - 1) {
                dmc_rearrange_constants(context, text, textpos,
                                        p + i + 1, nelems - i - 1);
            }
        } else if (c && !all_constant) {
            do_emit_constant(context, text, p[i]);
        }
    }
    *constant = all_constant;
    return retOk;
}

static DMCRet
dmc_tuple(DMCContext *context, DMCHeap *heap, DMC_STACK_TYPE(UWord) *text,
          Eterm t, int *constant)
{
    int all_constant;
    Eterm *p = tuple_val(t);
    Uint nelems = arityval(*p);
    DMCRet ret;

    ret = dmc_array(context, heap, text, p + 1, nelems, &all_constant);
    if (ret != retOk) {
        return ret;
    }
    if (all_constant) {
        *constant = 1;
        return retOk;
    }
    DMC_PUSH2(*text, matchMkTuple, nelems);
    context->stack_used -= (nelems - 1);
    *constant = 0;
    return retOk;
}

static DMCRet
dmc_map(DMCContext *context, DMCHeap *heap, DMC_STACK_TYPE(UWord) *text,
        Eterm t, int *constant)
{
    int nelems;
    int constant_values;
    DMCRet ret;
    if (is_flatmap(t)) {
        flatmap_t *m = (flatmap_t *)flatmap_val(t);
        Eterm *values = flatmap_get_values(m);

        nelems = flatmap_get_size(m);
        ret = dmc_array(context, heap, text, values, nelems, &constant_values);

        if (ret != retOk) {
            return ret;
        }
        if (constant_values) {
            *constant = 1;
            return retOk;
        }
        DMC_PUSH2(*text, matchPushC, dmc_private_copy(context, m->keys));
        if (++context->stack_used > context->stack_need) {
            context->stack_need = context->stack_used;
        }
        DMC_PUSH2(*text, matchMkFlatMap, nelems);
        context->stack_used -= nelems;
        *constant = 0;
        return retOk;
    } else {
        DECLARE_WSTACK(wstack);
        Eterm *kv;
        int c;

        ASSERT(is_hashmap(t));

        hashmap_iterator_init(&wstack, t, 1);
        constant_values = 1;
        nelems = hashmap_size(t);

        while ((kv=hashmap_iterator_prev(&wstack)) != NULL) {
            if ((ret = dmc_expr(context, heap, text, CDR(kv), &c)) != retOk) {
                DESTROY_WSTACK(wstack);
                return ret;
            }
            if (!c)
                constant_values = 0;
        }

        if (constant_values) {
            *constant = 1;
            DESTROY_WSTACK(wstack);
            return retOk;
        }

        *constant = 0;

        hashmap_iterator_init(&wstack, t, 1);

        while ((kv=hashmap_iterator_prev(&wstack)) != NULL) {
            /* push key */
            if ((ret = dmc_expr(context, heap, text, CAR(kv), &c)) != retOk) {
                DESTROY_WSTACK(wstack);
                return ret;
            }
            if (c) {
                do_emit_constant(context, text, CAR(kv));
            }
            /* push value */
            if ((ret = dmc_expr(context, heap, text, CDR(kv), &c)) != retOk) {
                DESTROY_WSTACK(wstack);
                return ret;
            }
            if (c) {
                do_emit_constant(context, text, CDR(kv));
            }
        }
        DMC_PUSH2(*text, matchMkHashMap, nelems);
        context->stack_used -= nelems;
        DESTROY_WSTACK(wstack);
        return retOk;
    }
}

static DMCRet dmc_whole_expression(DMCContext *context,
				   DMCHeap *heap,
				   DMC_STACK_TYPE(UWord) *text,
				   Eterm t,
				   int *constant)
{
    if (context->cflags & DCOMP_TRACE) {
	/* Hmmm, convert array to list... */
	if (context->special) {
	   DMC_PUSH(*text, matchPushArrayAsListU);
	} else { 
	    ASSERT(is_tuple(context->matchexpr
			    [context->current_match]));
	    DMC_PUSH(*text, matchPushArrayAsList);
	}
    } else {
	DMC_PUSH(*text, matchPushExpr);
    }
    ++context->stack_used;
    if (context->stack_used > context->stack_need)
	context->stack_need = context->stack_used;
    *constant = 0;
    return retOk;
}

/* Figure out which PushV instruction to use.
*/
static void dmc_add_pushv_variant(DMCContext *context, DMCHeap *heap,
				  DMC_STACK_TYPE(UWord) *text, Uint n)
{
    DMCVariable* v = &heap->vars[n];
    MatchOps instr = matchPushV;

    ASSERT(n < heap->vars_used && v->is_bound);
    if (!context->is_guard) {
        if(!v->is_in_body) {
	    instr = matchPushVResult;
	    v->is_in_body = 1;
	}
    }
    DMC_PUSH(*text, instr);
    DMC_PUSH(*text, n);
}

static DMCRet dmc_variable(DMCContext *context,
			   DMCHeap *heap,
			   DMC_STACK_TYPE(UWord) *text,
			   Eterm t,
			   int *constant)
{
    Uint n = db_is_variable(t);

    if (n >= heap->vars_used || !heap->vars[n].is_bound) {
	RETURN_VAR_ERROR("Variable $%%d is unbound.", n, context, *constant);
    }

    dmc_add_pushv_variant(context, heap, text, n);

    ++context->stack_used;
    if (context->stack_used > context->stack_need)
	context->stack_need = context->stack_used;
    *constant = 0;
    return retOk;
}

static DMCRet dmc_all_bindings(DMCContext *context,
			       DMCHeap *heap,
			       DMC_STACK_TYPE(UWord) *text,
			       Eterm t,
			       int *constant)
{
    int i;
    int heap_used = 0;

    DMC_PUSH(*text, matchPushC);
    DMC_PUSH(*text, NIL);
    for (i = heap->vars_used - 1; i >= 0; --i) {
	if (heap->vars[i].is_bound) {
	    dmc_add_pushv_variant(context, heap, text, i);
	    DMC_PUSH(*text, matchConsB);
	    heap_used += 2;
	}
    }
    ++context->stack_used;
    if ((context->stack_used + 1) > context->stack_need)
	context->stack_need = (context->stack_used + 1);
    *constant = 0;
    return retOk;
}

static DMCRet dmc_const(DMCContext *context,
		       DMCHeap *heap,
		       DMC_STACK_TYPE(UWord) *text,
		       Eterm t,
		       int *constant)
{
    Eterm *p = tuple_val(t);
    Uint a = arityval(*p);

    if (a != 2) {
	RETURN_TERM_ERROR("Special form 'const' called with more than one "
			  "argument in %T.", t, context, *constant);
    }
    *constant = 1;
    return retOk;
}

static DMCRet dmc_and(DMCContext *context,
		      DMCHeap *heap,
		      DMC_STACK_TYPE(UWord) *text,
		      Eterm t,
		      int *constant)
{
    Eterm *p = tuple_val(t);
    Uint a = arityval(*p);
    DMCRet ret;
    int i;
    int c;
    
    if (a < 2) {
	RETURN_TERM_ERROR("Special form 'and' called without arguments "
			  "in %T.", t, context, *constant);
    }
    *constant = 0;
    for (i = a; i > 1; --i) {
	if ((ret = dmc_expr(context, heap, text, p[i], &c)) != retOk)
	    return ret;
	if (c) 
	    do_emit_constant(context, text, p[i]);
    }
    DMC_PUSH(*text, matchAnd);
    DMC_PUSH(*text, (Uint) a - 1);
    context->stack_used -= (a - 2);
    return retOk;
}

static DMCRet dmc_or(DMCContext *context,
		     DMCHeap *heap,
		     DMC_STACK_TYPE(UWord) *text,
		     Eterm t,
		     int *constant)
{
    Eterm *p = tuple_val(t);
    Uint a = arityval(*p);
    DMCRet ret;
    int i;
    int c;
    
    if (a < 2) {
	RETURN_TERM_ERROR("Special form 'or' called without arguments "
			  "in %T.", t, context, *constant);
    }
    *constant = 0;
    for (i = a; i > 1; --i) {
	if ((ret = dmc_expr(context, heap, text, p[i], &c)) != retOk)
	    return ret;
	if (c) 
	    do_emit_constant(context, text, p[i]);
    }
    DMC_PUSH(*text, matchOr);
    DMC_PUSH(*text, (Uint) a - 1);
    context->stack_used -= (a - 2);
    return retOk;
}


static DMCRet dmc_andalso(DMCContext *context,
			  DMCHeap *heap,
			  DMC_STACK_TYPE(UWord) *text,
			  Eterm t,
			  int *constant)
{
    Eterm *p = tuple_val(t);
    Uint a = arityval(*p);
    DMCRet ret;
    int i;
    int c;
    Uint lbl;
    Uint lbl_next;
    Uint lbl_val;

    if (a < 2) {
	RETURN_TERM_ERROR("Special form 'andalso' called without"
			  " arguments "
			  "in %T.", t, context, *constant);
    }
    *constant = 0;
    lbl = 0;
    for (i = 2; i <= a; ++i) {
	if ((ret = dmc_expr(context, heap, text, p[i], &c)) != retOk)
	    return ret;
	if (c) 
	    do_emit_constant(context, text, p[i]);
	if (i == a) {
	    DMC_PUSH(*text, matchJump);
	} else {
	    DMC_PUSH(*text, matchAndAlso);
	}
	DMC_PUSH(*text, lbl);
	lbl = DMC_STACK_NUM(*text)-1;
	--(context->stack_used);
    }
    DMC_PUSH(*text, matchPushC);
    DMC_PUSH(*text, am_true);
    lbl_val = DMC_STACK_NUM(*text);
    while (lbl) {
	lbl_next = DMC_PEEK(*text, lbl);
	DMC_POKE(*text, lbl, lbl_val-lbl-1);
	lbl = lbl_next;
    }
    if (++context->stack_used > context->stack_need)
	context->stack_need = context->stack_used;
    return retOk;
}

static DMCRet dmc_orelse(DMCContext *context,
			 DMCHeap *heap,
			 DMC_STACK_TYPE(UWord) *text,
			 Eterm t,
			 int *constant)
{
    Eterm *p = tuple_val(t);
    Uint a = arityval(*p);
    DMCRet ret;
    int i;
    int c;
    Uint lbl;
    Uint lbl_next;
    Uint lbl_val;
    
    if (a < 2) {
	RETURN_TERM_ERROR("Special form 'orelse' called without arguments "
			  "in %T.", t, context, *constant);
    }
    *constant = 0;
    lbl = 0;
    for (i = 2; i <= a; ++i) {
	if ((ret = dmc_expr(context, heap, text, p[i], &c)) != retOk)
	    return ret;
	if (c) 
	    do_emit_constant(context, text, p[i]);
	if (i == a) {
	    DMC_PUSH(*text, matchJump);
	} else {
	    DMC_PUSH(*text, matchOrElse);
	}
	DMC_PUSH(*text, lbl);
	lbl = DMC_STACK_NUM(*text)-1;
	--(context->stack_used);
    }
    DMC_PUSH(*text, matchPushC);
    DMC_PUSH(*text, am_false);
    lbl_val = DMC_STACK_NUM(*text);
    while (lbl) {
	lbl_next = DMC_PEEK(*text, lbl);
	DMC_POKE(*text, lbl, lbl_val-lbl-1);
	lbl = lbl_next;
    }
    if (++context->stack_used > context->stack_need)
	context->stack_need = context->stack_used;
    return retOk;
}

static DMCRet dmc_message(DMCContext *context,
			  DMCHeap *heap,
			  DMC_STACK_TYPE(UWord) *text,
			  Eterm t,
			  int *constant)
{
    Eterm *p = tuple_val(t);
    Uint a = arityval(*p);
    DMCRet ret;
    int c;
    

    if (!(context->cflags & DCOMP_TRACE)) {
	RETURN_ERROR("Special form 'message' used in wrong dialect.",
		     context, 
		     *constant);
    }
    if (context->is_guard) {
	RETURN_ERROR("Special form 'message' called in guard context.",
		     context, 
		     *constant);
    }

    if (a != 2) {
	RETURN_TERM_ERROR("Special form 'message' called with wrong "
			  "number of arguments in %T.", t, context, 
			  *constant);
    }
    *constant = 0;
    if ((ret = dmc_expr(context, heap, text, p[2], &c)) != retOk) {
	return ret;
    }
    if (c) { 
	do_emit_constant(context, text, p[2]);
    }
    DMC_PUSH(*text, matchReturn);
    DMC_PUSH(*text, matchPushC);
    DMC_PUSH(*text, am_true);
    /* Push as much as we remove, stack_need is untouched */
    return retOk;
}

static DMCRet dmc_self(DMCContext *context,
		     DMCHeap *heap,
		     DMC_STACK_TYPE(UWord) *text,
		     Eterm t,
		     int *constant)
{
    Eterm *p = tuple_val(t);
    Uint a = arityval(*p);
    
    if (a != 1) {
	RETURN_TERM_ERROR("Special form 'self' called with arguments "
			  "in %T.", t, context, *constant);
    }
    *constant = 0;
    DMC_PUSH(*text, matchSelf);
    if (++context->stack_used > context->stack_need)
	context->stack_need = context->stack_used;
    return retOk;
}

static DMCRet dmc_return_trace(DMCContext *context,
			       DMCHeap *heap,
			       DMC_STACK_TYPE(UWord) *text,
			       Eterm t,
			       int *constant)
{
    Eterm *p = tuple_val(t);
    Uint a = arityval(*p);
    
    if (!(context->cflags & DCOMP_TRACE)) {
	RETURN_ERROR("Special form 'return_trace' used in wrong dialect.",
		     context, 
		     *constant);
    }
    if (context->is_guard) {
	RETURN_ERROR("Special form 'return_trace' called in "
		     "guard context.", context, *constant);
    }

    if (a != 1) {
	RETURN_TERM_ERROR("Special form 'return_trace' called with "
			  "arguments in %T.", t, context, *constant);
    }
    *constant = 0;
    DMC_PUSH(*text, matchSetReturnTrace); /* Pushes 'true' on the stack */
    if (++context->stack_used > context->stack_need)
	context->stack_need = context->stack_used;
    return retOk;
}

static DMCRet dmc_exception_trace(DMCContext *context,
			       DMCHeap *heap,
			       DMC_STACK_TYPE(UWord) *text,
			       Eterm t,
			       int *constant)
{
    Eterm *p = tuple_val(t);
    Uint a = arityval(*p);
    
    if (!(context->cflags & DCOMP_TRACE)) {
	RETURN_ERROR("Special form 'exception_trace' used in wrong dialect.",
		     context, 
		     *constant);
    }
    if (context->is_guard) {
	RETURN_ERROR("Special form 'exception_trace' called in "
		     "guard context.", context, *constant);
    }

    if (a != 1) {
	RETURN_TERM_ERROR("Special form 'exception_trace' called with "
			  "arguments in %T.", t, context, *constant);
    }
    *constant = 0;
    DMC_PUSH(*text, matchSetExceptionTrace); /* Pushes 'true' on the stack */
    if (++context->stack_used > context->stack_need)
	context->stack_need = context->stack_used;
    return retOk;
}

static int check_trace(const char* op,
                       DMCContext *context,
                       int *constant,
                       int need_cflags,
                       int allow_in_guard,
                       DMCRet* retp)
{
    if (!(context->cflags & DCOMP_TRACE)) {
	*retp = RETURN_ERROR_X(-1, context, *constant, "Special form '%s' "
                               "used in wrong dialect.", op);
        return 0;
    }
    if ((context->cflags & need_cflags) != need_cflags) {
        *retp = RETURN_ERROR_X(-1, context, *constant, "Special form '%s' "
                               "not allow for this trace event.", op);
        return 0;
    }
    if (context->is_guard && !allow_in_guard) {
        *retp = RETURN_ERROR_X(-1, context, *constant, "Special form '%s' "
                               "called in guard context.", op);
        return 0;
    }
    return 1;
}

static DMCRet dmc_is_seq_trace(DMCContext *context,
			       DMCHeap *heap,
			       DMC_STACK_TYPE(UWord) *text,
			       Eterm t,
			       int *constant)
{
    Eterm *p = tuple_val(t);
    Uint a = arityval(*p);
    DMCRet ret;
    
    if (!check_trace("is_seq_trace", context, constant, DCOMP_ALLOW_TRACE_OPS, 1, &ret))
        return ret;

    if (a != 1) {
	RETURN_TERM_ERROR("Special form 'is_seq_trace' called with "
			  "arguments in %T.", t, context, *constant);
    }
    *constant = 0;
    DMC_PUSH(*text, matchIsSeqTrace); 
    /* Pushes 'true' or 'false' on the stack */
    if (++context->stack_used > context->stack_need)
	context->stack_need = context->stack_used;
    return retOk;
}

static DMCRet dmc_set_seq_token(DMCContext *context,
				DMCHeap *heap,
				DMC_STACK_TYPE(UWord) *text,
				Eterm t,
				int *constant)
{
    Eterm *p = tuple_val(t);
    Uint a = arityval(*p);
    DMCRet ret;
    int c;
    
    if (!check_trace("set_seq_trace", context, constant, DCOMP_ALLOW_TRACE_OPS, 0, &ret))
        return ret;

    if (a != 3) {
	RETURN_TERM_ERROR("Special form 'set_seq_token' called with wrong "
			  "number of arguments in %T.", t, context, 
			  *constant);
    }
    *constant = 0;
    if ((ret = dmc_expr(context, heap, text, p[3], &c)) != retOk) {
	return ret;
    }
    if (c) { 
	do_emit_constant(context, text, p[3]);
    }
    if ((ret = dmc_expr(context, heap, text, p[2], &c)) != retOk) {
	return ret;
    }
    if (c) { 
	do_emit_constant(context, text, p[2]);
    }
    if (context->cflags & DCOMP_FAKE_DESTRUCTIVE) {
	DMC_PUSH(*text, matchSetSeqTokenFake);
    } else {
	DMC_PUSH(*text, matchSetSeqToken);
    }
    --context->stack_used; /* Remove two and add one */
    return retOk;
}

static DMCRet dmc_get_seq_token(DMCContext *context,
				DMCHeap *heap,
				DMC_STACK_TYPE(UWord) *text,
				Eterm t,
				int *constant)
{
    Eterm *p = tuple_val(t);
    Uint a = arityval(*p);
    DMCRet ret;

    if (!check_trace("get_seq_token", context, constant, DCOMP_ALLOW_TRACE_OPS, 0, &ret))
        return ret;

    if (a != 1) {
	RETURN_TERM_ERROR("Special form 'get_seq_token' called with "
			  "arguments in %T.", t, context, 
			  *constant);
    }

    *constant = 0;
    DMC_PUSH(*text, matchGetSeqToken);
    if (++context->stack_used > context->stack_need)
 	context->stack_need = context->stack_used;
    return retOk;
}



static DMCRet dmc_display(DMCContext *context,
			  DMCHeap *heap,
			  DMC_STACK_TYPE(UWord) *text,
			  Eterm t,
			  int *constant)
{
    Eterm *p = tuple_val(t);
    Uint a = arityval(*p);
    DMCRet ret;
    int c;
    

    if (!(context->cflags & DCOMP_TRACE)) {
	RETURN_ERROR("Special form 'display' used in wrong dialect.",
		     context, 
		     *constant);
    }
    if (context->is_guard) {
	RETURN_ERROR("Special form 'display' called in guard context.",
		     context, 
		     *constant);
    }

    if (a != 2) {
	RETURN_TERM_ERROR("Special form 'display' called with wrong "
			  "number of arguments in %T.", t, context, 
			  *constant);
    }
    *constant = 0;
    if ((ret = dmc_expr(context, heap, text, p[2], &c)) != retOk) {
	return ret;
    }
    if (c) { 
	do_emit_constant(context, text, p[2]);
    }
    DMC_PUSH(*text, matchDisplay);
    /* Push as much as we remove, stack_need is untouched */
    return retOk;
}

static DMCRet dmc_process_dump(DMCContext *context,
			       DMCHeap *heap,
			       DMC_STACK_TYPE(UWord) *text,
			       Eterm t,
			       int *constant)
{
    Eterm *p = tuple_val(t);
    Uint a = arityval(*p);
    DMCRet ret;

    if (!check_trace("process_dump", context, constant, DCOMP_ALLOW_TRACE_OPS, 0, &ret))
        return ret;

    if (a != 1) {
	RETURN_TERM_ERROR("Special form 'process_dump' called with "
			  "arguments in %T.", t, context, *constant);
    }
    *constant = 0;
    DMC_PUSH(*text, matchProcessDump); /* Creates binary */
    if (++context->stack_used > context->stack_need)
	context->stack_need = context->stack_used;
    return retOk;
}

static DMCRet dmc_enable_trace(DMCContext *context,
			       DMCHeap *heap,
			       DMC_STACK_TYPE(UWord) *text,
			       Eterm t,
			       int *constant)
{
    Eterm *p = tuple_val(t);
    Uint a = arityval(*p);
    DMCRet ret;
    int c;
    
    if (!check_trace("enable_trace", context, constant, DCOMP_ALLOW_TRACE_OPS, 0, &ret))
        return ret;

    switch (a) {
    case 2:
	*constant = 0;
	if ((ret = dmc_expr(context, heap, text, p[2], &c)) != retOk) {
	    return ret;
	}
	if (c) { 
	    do_emit_constant(context, text, p[2]);
	}
	DMC_PUSH(*text, matchEnableTrace);
	/* Push as much as we remove, stack_need is untouched */
	break;
    case 3:
	*constant = 0;
	if ((ret = dmc_expr(context, heap, text, p[3], &c)) != retOk) {
	    return ret;
	}
	if (c) { 
	    do_emit_constant(context, text, p[3]);
	}
	if ((ret = dmc_expr(context, heap, text, p[2], &c)) != retOk) {
	    return ret;
	}
	if (c) { 
	    do_emit_constant(context, text, p[2]);
	}
	DMC_PUSH(*text, matchEnableTrace2);
	--context->stack_used; /* Remove two and add one */
	break;
    default:
	RETURN_TERM_ERROR("Special form 'enable_trace' called with wrong "
			  "number of arguments in %T.", t, context, 
			  *constant);
    }
    return retOk;
}

static DMCRet dmc_disable_trace(DMCContext *context,
				DMCHeap *heap,
				DMC_STACK_TYPE(UWord) *text,
				Eterm t,
				int *constant)
{
    Eterm *p = tuple_val(t);
    Uint a = arityval(*p);
    DMCRet ret;
    int c;

    if (!check_trace("disable_trace", context, constant, DCOMP_ALLOW_TRACE_OPS, 0, &ret))
        return ret;

    switch (a) {
    case 2:
	*constant = 0;
	if ((ret = dmc_expr(context, heap, text, p[2], &c)) != retOk) {
	    return ret;
	}
	if (c) { 
	    do_emit_constant(context, text, p[2]);
	}
	DMC_PUSH(*text, matchDisableTrace);
	/* Push as much as we remove, stack_need is untouched */
	break;
    case 3:
	*constant = 0;
	if ((ret = dmc_expr(context, heap, text, p[3], &c)) != retOk) {
	    return ret;
	}
	if (c) { 
	    do_emit_constant(context, text, p[3]);
	}
	if ((ret = dmc_expr(context, heap, text, p[2], &c)) != retOk) {
	    return ret;
	}
	if (c) { 
	    do_emit_constant(context, text, p[2]);
	}
	DMC_PUSH(*text, matchDisableTrace2);
	--context->stack_used; /* Remove two and add one */
	break;
    default:
	RETURN_TERM_ERROR("Special form 'disable_trace' called with wrong "
			  "number of arguments in %T.", t, context, 
			  *constant);
    }
    return retOk;
}

static DMCRet dmc_trace(DMCContext *context,
			DMCHeap *heap,
			DMC_STACK_TYPE(UWord) *text,
			Eterm t,
			int *constant)
{
    Eterm *p = tuple_val(t);
    Uint a = arityval(*p);
    DMCRet ret;
    int c;
    
    if (!check_trace("trace", context, constant, DCOMP_ALLOW_TRACE_OPS, 0, &ret))
        return ret;

    switch (a) {
    case 3:
	*constant = 0;
	if ((ret = dmc_expr(context, heap, text, p[3], &c)) != retOk) {
	    return ret;
	}
	if (c) { 
	    do_emit_constant(context, text, p[3]);
	}
	if ((ret = dmc_expr(context, heap, text, p[2], &c)) != retOk) {
	    return ret;
	}
	if (c) { 
	    do_emit_constant(context, text, p[2]);
	}
	DMC_PUSH(*text, matchTrace2);
	--context->stack_used; /* Remove two and add one */
	break;
    case 4:
	*constant = 0;
	if ((ret = dmc_expr(context, heap, text, p[4], &c)) != retOk) {
	    return ret;
	}
	if (c) { 
	    do_emit_constant(context, text, p[4]);
	}
	if ((ret = dmc_expr(context, heap, text, p[3], &c)) != retOk) {
	    return ret;
	}
	if (c) { 
	    do_emit_constant(context, text, p[3]);
	}
	if ((ret = dmc_expr(context, heap, text, p[2], &c)) != retOk) {
	    return ret;
	}
	if (c) { 
	    do_emit_constant(context, text, p[2]);
	}
	DMC_PUSH(*text, matchTrace3);
	context->stack_used -= 2; /* Remove three and add one */
	break;
    default:
	RETURN_TERM_ERROR("Special form 'trace' called with wrong "
			  "number of arguments in %T.", t, context, 
			  *constant);
    }
    return retOk;
}



static DMCRet dmc_caller(DMCContext *context,
 			 DMCHeap *heap,
			 DMC_STACK_TYPE(UWord) *text,
 			 Eterm t,
 			 int *constant)
{
    Eterm *p = tuple_val(t);
    Uint a = arityval(*p);
    DMCRet ret;
     
    if (!check_trace("caller", context, constant,
                     (DCOMP_CALL_TRACE|DCOMP_ALLOW_TRACE_OPS), 0, &ret))
        return ret;
  
    if (a != 1) {
 	RETURN_TERM_ERROR("Special form 'caller' called with "
 			  "arguments in %T.", t, context, *constant);
    }
    *constant = 0;
    DMC_PUSH(*text, matchCaller); /* Creates binary */
    if (++context->stack_used > context->stack_need)
 	context->stack_need = context->stack_used;
    return retOk;
}


  
static DMCRet dmc_silent(DMCContext *context,
 			 DMCHeap *heap,
			 DMC_STACK_TYPE(UWord) *text,
 			 Eterm t,
 			 int *constant)
{
    Eterm *p = tuple_val(t);
    Uint a = arityval(*p);
    DMCRet ret;
    int c;
     
    if (!check_trace("silent", context, constant, DCOMP_ALLOW_TRACE_OPS, 0, &ret))
        return ret;
  
    if (a != 2) {
	RETURN_TERM_ERROR("Special form 'silent' called with wrong "
			  "number of arguments in %T.", t, context, 
			  *constant);
    }
    *constant = 0;
    if ((ret = dmc_expr(context, heap, text, p[2], &c)) != retOk) {
	return ret;
    }
    if (c) { 
	do_emit_constant(context, text, p[2]);
    }
    DMC_PUSH(*text, matchSilent);
    DMC_PUSH(*text, matchPushC);
    DMC_PUSH(*text, am_true);
    /* Push as much as we remove, stack_need is untouched */
    return retOk;
}
  


static DMCRet dmc_fun(DMCContext *context,
		       DMCHeap *heap,
		       DMC_STACK_TYPE(UWord) *text,
		       Eterm t,
		       int *constant)
{
    Eterm *p = tuple_val(t);
    Uint a = arityval(*p);
    int c;
    int i;
    DMCRet ret;
    DMCGuardBif *b;
 
    /* Special forms. */
    switch (p[1]) {
    case am_const:
	return dmc_const(context, heap, text, t, constant);
    case am_and:
	return dmc_and(context, heap, text, t, constant);
    case am_or:
	return dmc_or(context, heap, text, t, constant);
    case am_andalso:
    case am_andthen:
	return dmc_andalso(context, heap, text, t, constant);
    case am_orelse:
	return dmc_orelse(context, heap, text, t, constant);
    case am_self:
	return dmc_self(context, heap, text, t, constant);
    case am_message:
	return dmc_message(context, heap, text, t, constant);
    case am_is_seq_trace:
	return dmc_is_seq_trace(context, heap, text, t, constant);
    case am_set_seq_token:
	return dmc_set_seq_token(context, heap, text, t, constant);
    case am_get_seq_token:
	return dmc_get_seq_token(context, heap, text, t, constant);
    case am_return_trace:
	return dmc_return_trace(context, heap, text, t, constant);
    case am_exception_trace:
	return dmc_exception_trace(context, heap, text, t, constant);
    case am_display:
	return dmc_display(context, heap, text, t, constant);
    case am_process_dump:
	return dmc_process_dump(context, heap, text, t, constant);
    case am_enable_trace:
	return dmc_enable_trace(context, heap, text, t, constant);
    case am_disable_trace:
	return dmc_disable_trace(context, heap, text, t, constant);
    case am_trace:
	return dmc_trace(context, heap, text, t, constant);
    case am_caller:
 	return dmc_caller(context, heap, text, t, constant);
    case am_silent:
 	return dmc_silent(context, heap, text, t, constant);
    case am_set_tcw:
	if (context->cflags & DCOMP_FAKE_DESTRUCTIVE) {
	    b = dmc_lookup_bif(am_set_tcw_fake, ((int) a) - 1);
	} else {
	    b = dmc_lookup_bif(p[1], ((int) a) - 1);
	}
	break;
    default:
	b = dmc_lookup_bif(p[1], ((int) a) - 1);
    }


    if (b == NULL) {
	if (context->err_info != NULL) {
	    /* Ugly, should define a better RETURN_TERM_ERROR interface... */
	    char buff[100];
	    erts_snprintf(buff, sizeof(buff),
		    "Function %%T/%d does_not_exist.",
		    (int)a - 1);
	    RETURN_TERM_ERROR(buff, p[1], context, *constant);
	} else {
	    return retFail;
	}
    } 
    ASSERT(b->arity == ((int) a) - 1);
    if (! (b->flags & 
	   (1 << 
	    ((context->cflags & DCOMP_DIALECT_MASK) + 
	      (context->is_guard ? DBIF_GUARD : DBIF_BODY))))) {
	/* Body clause used in wrong context. */
	if (context->err_info != NULL) {
	    /* Ugly, should define a better RETURN_TERM_ERROR interface... */
	    char buff[100];
	    erts_snprintf(buff, sizeof(buff),
		    "Function %%T/%d cannot be called in this context.",
		    (int)a - 1);
	    RETURN_TERM_ERROR(buff, p[1], context, *constant);
	} else {
	    return retFail;
	}
    }	

    *constant = 0;

    for (i = a; i > 1; --i) {
	if ((ret = dmc_expr(context, heap, text, p[i], &c)) != retOk)
	    return ret;
	if (c) 
	    do_emit_constant(context, text, p[i]);
    }
    switch (b->arity) {
    case 0:
	DMC_PUSH(*text, matchCall0);
	break;
    case 1:
	DMC_PUSH(*text, matchCall1);
	break;
    case 2:
	DMC_PUSH(*text, matchCall2);
	break;
    case 3:
	DMC_PUSH(*text, matchCall3);
	break;
    default:
	erts_exit(ERTS_ERROR_EXIT,"ets:match() internal error, "
		 "guard with more than 3 arguments.");
    }
    DMC_PUSH(*text, (UWord) b->biff);
    context->stack_used -= (((int) a) - 2);
    if (context->stack_used > context->stack_need)
 	context->stack_need = context->stack_used;
    return retOk;
}

static DMCRet dmc_expr(DMCContext *context,
		       DMCHeap *heap,
		       DMC_STACK_TYPE(UWord) *text,
		       Eterm t,
		       int *constant)
{
    DMCRet ret;
    Eterm tmp;
    Eterm *p;


    switch (t & _TAG_PRIMARY_MASK) {
    case TAG_PRIMARY_LIST:
	if ((ret = dmc_list(context, heap, text, t, constant)) != retOk)
	    return ret;
	break;
    case TAG_PRIMARY_BOXED:
        if (is_map(t)) {
            return dmc_map(context, heap, text, t, constant);
        }
	if (!is_tuple(t)) {
	    goto simple_term;
	}
	p = tuple_val(t);
#ifdef HARDDEBUG
	erts_fprintf(stderr,"%d %d %d %d\n",arityval(*p),is_tuple(tmp = p[1]),
		     is_atom(p[1]),db_is_variable(p[1]));
#endif
	if (arityval(*p) == 1 && is_tuple(tmp = p[1])) {
	    if ((ret = dmc_tuple(context, heap, text, tmp, constant)) != retOk)
		return ret;
	} else if (arityval(*p) >= 1 && is_atom(p[1]) && 
		   !(db_is_variable(p[1]) >= 0)) {
	    if ((ret = dmc_fun(context, heap, text, t, constant)) != retOk)
		return ret;
	} else
	    RETURN_TERM_ERROR("%T is neither a function call, nor a tuple "
			      "(tuples are written {{ ... }}).", t,
			      context, *constant);
	break;
    case TAG_PRIMARY_IMMED1:
	if (db_is_variable(t) >= 0) {
	    if ((ret = dmc_variable(context, heap, text, t, constant)) 
		!= retOk)
		return ret;
	    break;
	} else if (t == am_DollarUnderscore) {
	    if ((ret = dmc_whole_expression(context, heap, text, t, constant)) 
		!= retOk)
		return ret;
	    break;
	} else if (t == am_DollarDollar) {
	    if ((ret = dmc_all_bindings(context, heap, text, t, constant)) 
		!= retOk)
		return ret;
	    break;
	}	    
	/* Fall through */
    default:
    simple_term:
	*constant = 1;
    }
    return retOk;
}

    
static DMCRet compile_guard_expr(DMCContext *context,
				 DMCHeap *heap,
				 DMC_STACK_TYPE(UWord) *text,
				 Eterm l)
{
    DMCRet ret;
    int constant;
    Eterm t;

    if (l != NIL) {
	if (!is_list(l))
	    RETURN_ERROR("Match expression is not a list.", 
			 context, constant);
	if (!(context->is_guard)) {
	    DMC_PUSH(*text, matchCatch);
	}
	while (is_list(l)) {
	    constant = 0;
	    t = CAR(list_val(l));
	    if ((ret = dmc_expr(context, heap, text, t, &constant)) !=
		retOk)
		return ret;
	    if (constant) {
		do_emit_constant(context, text, t);
	    }
	    l = CDR(list_val(l));
	    if (context->is_guard) {
		DMC_PUSH(*text,matchTrue);
	    } else {
		DMC_PUSH(*text,matchWaste);
	    }
	    --context->stack_used;
	}
	if (l != NIL) 
	    RETURN_ERROR("Match expression is not a proper list.",
			 context, constant);
	if (!(context->is_guard) && (context->cflags & DCOMP_TABLE)) {
	    ASSERT(matchWaste == DMC_TOP(*text));
	    (void) DMC_POP(*text);
	    DMC_PUSH(*text, matchReturn); /* Same impact on stack as 
					     matchWaste */
	}
    }
    return retOk;
}




/*
** Match compilation utility code
*/

/*
** Handling of bif's in match guard expressions
*/

static DMCGuardBif *dmc_lookup_bif(Eterm t, int arity)
{
    /*
    ** Place for optimization, bsearch is slower than inlining it...
    */
    DMCGuardBif node = {0,NULL,0};
    node.name = t;
    node.arity = arity;
    return bsearch(&node, 
		   guard_tab, 
		   sizeof(guard_tab) / sizeof(DMCGuardBif),
		   sizeof(DMCGuardBif), 
		   (int (*)(const void *, const void *)) &cmp_guard_bif); 
}

#ifdef DMC_DEBUG
static Eterm dmc_lookup_bif_reversed(void *f)
{
    int i;
    for (i = 0; i < (sizeof(guard_tab) / sizeof(DMCGuardBif)); ++i)
	if (f == guard_tab[i].biff)
	    return guard_tab[i].name;
    return am_undefined;
}
#endif

/* For sorting. */
static int cmp_uint(void *a, void *b) 
{
    if (*((unsigned *)a) <  *((unsigned *)b))
	return -1;
    else
	return (*((unsigned *)a) >  *((unsigned *)b));
}

static int cmp_guard_bif(void *a, void *b)
{
    int ret;
    if (( ret = ((int) atom_val(((DMCGuardBif *) a)->name)) -
	 ((int) atom_val(((DMCGuardBif *) b)->name)) ) == 0) {
	ret = ((DMCGuardBif *) a)->arity - ((DMCGuardBif *) b)->arity;
    }
    return ret;
}

/*
** Compact the variables in a match expression i e make {$1, $100, $1000} 
** become {$0,$1,$2}.
*/
static int match_compact(ErlHeapFragment *expr, DMCErrInfo *err_info)
{
    int i, j, a, n, x;
    DMC_STACK_TYPE(unsigned) heap;
    Eterm *p;
    char buff[25] = "$"; /* large enough for 64 bit to */
    int ret;

    DMC_INIT_STACK(heap);

    p = expr->mem;
    i = expr->used_size;
    while (i--) {
	if (is_thing(*p)) {
	    a = thing_arityval(*p);
	    ASSERT(a <= i);
	    i -= a;
	    p += a;
	} else if (is_atom(*p) && (n = db_is_variable(*p)) >= 0) {
	    x = DMC_STACK_NUM(heap);
	    for (j = 0; j < x && DMC_PEEK(heap,j) != n; ++j) 
		;
	    
	    if (j == x)
		DMC_PUSH(heap,n);
	}
	++p;
    }
    qsort(DMC_STACK_DATA(heap), DMC_STACK_NUM(heap), sizeof(unsigned), 
	  (int (*)(const void *, const void *)) &cmp_uint);

    if (err_info != NULL) { /* lint needs a translation table */
	err_info->var_trans = erts_alloc(ERTS_ALC_T_DB_TRANS_TAB,
					 sizeof(unsigned)*DMC_STACK_NUM(heap));
	sys_memcpy(err_info->var_trans, DMC_STACK_DATA(heap),
		   DMC_STACK_NUM(heap) * sizeof(unsigned));
	err_info->num_trans = DMC_STACK_NUM(heap);
    }

    p = expr->mem;
    i = expr->used_size;
    while (i--) {
	if (is_thing(*p)) {
	    a = thing_arityval(*p);
	    i -= a;
	    p += a;
	} else if (is_atom(*p) && (n = db_is_variable(*p)) >= 0) {
	    x = DMC_STACK_NUM(heap);
#ifdef HARDDEBUG
	    erts_fprintf(stderr, "%T");
#endif
	    for (j = 0; j < x && DMC_PEEK(heap,j) != n; ++j) 
		;
	    ASSERT(j < x);
	    erts_snprintf(buff+1, sizeof(buff) - 1, "%u", (unsigned) j);
	    /* Yes, writing directly into terms, they ARE off heap */
	    *p = erts_atom_put((byte *) buff, sys_strlen(buff),
			       ERTS_ATOM_ENC_LATIN1, 1);
	}
	++p;
    }
    ret = DMC_STACK_NUM(heap);
    DMC_FREE(heap);
    return ret;
}

/*
** Simple size object that takes care of function calls and constant tuples
*/
static Uint my_size_object(Eterm t) 
{
    Uint sum = 0;
    Eterm tmp;
    Eterm *p;
    switch (t & _TAG_PRIMARY_MASK) {
    case TAG_PRIMARY_LIST:
	sum += 2 + my_size_object(CAR(list_val(t))) + 
	    my_size_object(CDR(list_val(t)));
	break;
    case TAG_PRIMARY_BOXED:
	if ((((*boxed_val(t)) & 
	      _TAG_HEADER_MASK) >> _TAG_PRIMARY_SIZE) !=
	    (_TAG_HEADER_ARITYVAL >> _TAG_PRIMARY_SIZE)) {
	    goto simple_term;
	}

	if (arityval(*tuple_val(t)) == 1 && is_tuple(tmp = tuple_val(t)[1])) {
	    Uint i,n;
	    p = tuple_val(tmp);
	    n = arityval(p[0]);
	    sum += 1 + n;
	    for (i = 1; i <= n; ++i)
		sum += my_size_object(p[i]);
	} else if (arityval(*tuple_val(t)) == 2 &&
		   is_atom(tmp = tuple_val(t)[1]) &&
		   tmp == am_const) {
	    sum += size_object(tuple_val(t)[2]);
	} else {
	    erts_exit(ERTS_ERROR_EXIT,"Internal error, sizing unrecognized object in "
		     "(d)ets:match compilation.");
	}
	break;
    default:
    simple_term:
	sum += size_object(t);
	break;
    }
    return sum;
}

static Eterm my_copy_struct(Eterm t, Eterm **hp, ErlOffHeap* off_heap)
{
    Eterm ret = NIL, a, b;
    Eterm *p;
    Uint sz;
    switch (t & _TAG_PRIMARY_MASK) {
    case TAG_PRIMARY_LIST:
	a = my_copy_struct(CAR(list_val(t)), hp, off_heap);
	b = my_copy_struct(CDR(list_val(t)), hp, off_heap);
	ret = CONS(*hp, a, b);
	*hp += 2;
	break;
    case TAG_PRIMARY_BOXED:
	if (is_tuple(t)) {
	    if (arityval(*tuple_val(t)) == 1 && 
		is_tuple(a = tuple_val(t)[1])) {
		Uint i,n;
		Eterm *savep = *hp;
		ret = make_tuple(savep);
		p = tuple_val(a);
		n = arityval(p[0]);
		*hp += n + 1;
		*savep++ = make_arityval(n);
		for(i = 1; i <= n; ++i) 
		    *savep++ = my_copy_struct(p[i], hp, off_heap);
	    } else if (arityval(*tuple_val(t)) == 2 && 
		       is_atom(a = tuple_val(t)[1]) &&
		       a == am_const) {
		/* A {const, XXX} expression */
		b = tuple_val(t)[2];
		sz = size_object(b);
		ret = copy_struct(b,sz,hp,off_heap);
	    } else {
		erts_exit(ERTS_ERROR_EXIT, "Trying to constant-copy non constant expression "
			 "0x%bex in (d)ets:match compilation.", t);
	    }
	} else {
	    sz = size_object(t);
	    ret = copy_struct(t,sz,hp,off_heap);
	}
	break;
    default:
	ret = t;
    }
    return ret;
}

/*
** Compiled match bif interface
*/
/*
** erlang:match_spec_test(MatchAgainst, MatchSpec, Type) -> 
**   {ok, Return, Flags, Errors} | {error, Errors}
** MatchAgainst -> if Type == trace: list() else tuple()
** MatchSpec -> MatchSpec with body corresponding to Type
** Type -> trace | table (only trace implemented in R5C)
** Return -> if Type == trace TraceReturn else {BodyReturn, VariableBindings}
** TraceReturn -> {true | false | term()} 
** BodyReturn -> term()
** VariableBindings -> [term(), ...] 
** Errors -> [OneError, ...]
** OneError -> {error, string()} | {warning, string()}
** Flags -> [Flag, ...]
** Flag -> return_trace (currently only flag)
*/
BIF_RETTYPE match_spec_test_3(BIF_ALIST_3)
{
    Eterm res;
#ifdef DMC_DEBUG
    if (BIF_ARG_3 == am_atom_put("dis",3)) {
	test_disassemble_next = 1;
	BIF_RET(am_true);
    } else
#endif
    if (BIF_ARG_3 == am_trace) {
	res = match_spec_test(BIF_P, BIF_ARG_1, BIF_ARG_2, 1);
	if (is_value(res)) {
	    BIF_RET(res);
	}
    } else if (BIF_ARG_3 == am_table) {
	res = match_spec_test(BIF_P, BIF_ARG_1, BIF_ARG_2, 0);
	if (is_value(res)) {
	    BIF_RET(res);
	}
    } 
    BIF_ERROR(BIF_P, BADARG);
}

static Eterm match_spec_test(Process *p, Eterm against, Eterm spec, int trace)
{
    Eterm lint_res;
    Binary *mps;
    Eterm res;
    Eterm ret;
    Eterm flg;
    Eterm *hp;
    Eterm *arr;
    int n;
    Eterm l;
    Uint32 ret_flags;
    Uint sz;
    BeamInstr *save_cp;

    if (trace && !(is_list(against) || against == NIL)) {
	return THE_NON_VALUE;
    }
    if (trace) {
        const Uint cflags = (DCOMP_TRACE | DCOMP_FAKE_DESTRUCTIVE |
                             DCOMP_CALL_TRACE | DCOMP_ALLOW_TRACE_OPS);
	lint_res = db_match_set_lint(p, spec, cflags);
	mps = db_match_set_compile(p, spec, cflags);
    } else {
        const Uint cflags = (DCOMP_TABLE | DCOMP_FAKE_DESTRUCTIVE);
	lint_res = db_match_set_lint(p, spec, cflags);
	mps = db_match_set_compile(p, spec, cflags);
    }

    if (mps == NULL) {
	hp = HAlloc(p,3);
	ret = TUPLE2(hp, am_error, lint_res);
    } else {
#ifdef DMC_DEBUG
	if (test_disassemble_next) {
	    test_disassemble_next = 0;
	    db_match_dis(mps);
	}
#endif /* DMC_DEBUG */
	l = against;
	n = 0;
	while (is_list(l)) {
	    ++n;
	    l = CDR(list_val(l));
	}
	if (trace) {
	    if (n)
		arr = erts_alloc(ERTS_ALC_T_DB_TMP, sizeof(Eterm) * n);
	    else 
		arr = NULL;
	    l = against;
	    n = 0;
	    while (is_list(l)) {
		arr[n] = CAR(list_val(l));
		++n;
		l = CDR(list_val(l));
	    }
	    save_cp = p->cp;
	    p->cp = NULL;
	    res = erts_match_set_run_trace(p, p,
                      mps, arr, n,
		      ERTS_PAM_COPY_RESULT|ERTS_PAM_IGNORE_TRACE_SILENT,
		      &ret_flags);
	    p->cp = save_cp;
	} else {
	    n = 0;
	    arr = NULL;
	    res = erts_match_set_run_ets(p, mps, against, n, &ret_flags);
	}
	
	/* We are in the context of a BIF, 
	   {caller} should return 'undefined' */
	if (is_non_value(res)) {
	    res = am_false;
	}
	sz = 0;
	if (ret_flags & MATCH_SET_EXCEPTION_TRACE) sz += 2;
	if (ret_flags & MATCH_SET_RETURN_TRACE) sz += 2;
	hp = HAlloc(p, 5 + sz);
	flg = NIL;
	if (ret_flags & MATCH_SET_EXCEPTION_TRACE) {
	    flg = CONS(hp, am_exception_trace, flg);
	    hp += 2;
	}
	if (ret_flags & MATCH_SET_RETURN_TRACE) {
	    flg = CONS(hp, am_return_trace, flg);
	    hp += 2;
	}
	if (trace && arr != NULL) {
	    erts_free(ERTS_ALC_T_DB_TMP, arr);
	}
	erts_bin_free(mps);
	ret = TUPLE4(hp, am_atom_put("ok",2), res, flg, lint_res);
    }
    return ret;
}

static Eterm seq_trace_fake(Process *p, Eterm arg1)
{
    Eterm result = erl_seq_trace_info(p, arg1);
    if (!is_non_value(result) && is_tuple(result) && *tuple_val(result) == 2) {
	return (tuple_val(result))[2];
    }
    return result;
}

DbTerm* db_alloc_tmp_uncompressed(DbTableCommon* tb, DbTerm* org)
{
    ErlOffHeap tmp_offheap;
    DbTerm* res = erts_alloc(ERTS_ALC_T_TMP,
			     sizeof(DbTerm) + org->size*sizeof(Eterm));
    Eterm* hp = res->tpl;
    tmp_offheap.first = NULL;
    db_copy_from_comp(tb, org, &hp, &tmp_offheap);
    res->first_oh = tmp_offheap.first;
    res->size = org->size;
#ifdef DEBUG_CLONE
    res->debug_clone = NULL;
#endif
    return res;
}

void db_free_tmp_uncompressed(DbTerm* obj)
{
    ErlOffHeap off_heap;
    off_heap.first = obj->first_oh;
    erts_cleanup_offheap(&off_heap);
#ifdef DEBUG_CLONE
    ASSERT(obj->debug_clone == NULL);
#endif
    erts_free(ERTS_ALC_T_TMP, obj);
}

Eterm db_match_dbterm(DbTableCommon* tb, Process* c_p, Binary* bprog,
			     int all, DbTerm* obj, Eterm** hpp, Uint extra)
{
    enum erts_pam_run_flags flags;
    Uint32 dummy;
    Eterm res;

    if (tb->compress) {
	obj = db_alloc_tmp_uncompressed(tb, obj);
    }

    flags = (hpp ?
             ERTS_PAM_COPY_RESULT | ERTS_PAM_CONTIGUOUS_TUPLE :
             ERTS_PAM_TMP_RESULT  | ERTS_PAM_CONTIGUOUS_TUPLE);

    res = db_prog_match(c_p, c_p,
                        bprog, make_tuple(obj->tpl), NULL, 0,
			flags, &dummy);

    if (is_value(res) && hpp!=NULL) {
	*hpp = HAlloc(c_p, extra);
    }

    if (tb->compress) {
	db_free_tmp_uncompressed(obj);
    }
    return res;
}


#ifdef DMC_DEBUG

/*
** Disassemble match program
*/
void db_match_dis(Binary *bp)
{
    MatchProg *prog = Binary2MatchProg(bp);
    UWord *t = prog->text;
    Uint n;
    Eterm p;
    int first;
    ErlHeapFragment *tmp;

    while (t < prog->prog_end) {
	switch (*t) {
	case matchTryMeElse:
	    ++t;
	    n = *t;
	    ++t;
	    erts_printf("TryMeElse\t%beu\n", n);
	    break;
	case matchArray:
	    ++t;
	    n = *t;
	    ++t;
	    erts_printf("Array\t%beu\n", n);
	    break;
	case matchArrayBind:
	    ++t;
	    n = *t;
	    ++t;
	    erts_printf("ArrayBind\t%beu\n", n);
	    break;
	case matchTuple:
	    ++t;
	    n = *t;
	    ++t;
	    erts_printf("Tuple\t%beu\n", n);
	    break;
        case matchMap:
            ++t;
            n = *t;
            ++t;
            erts_printf("Map\t%beu\n", n);
            break;
        case matchKey:
            ++t;
            p = (Eterm) *t;
            ++t;
            erts_printf("Key\t%p (%T)\n", t, p);
            break;
	case matchPushT:
	    ++t;
	    n = *t;
	    ++t;
	    erts_printf("PushT\t%beu\n", n);
	    break;
	case matchPushL:
	    ++t;
	    erts_printf("PushL\n");
	    break;
        case matchPushM:
            ++t;
            n = *t;
            ++t;
            erts_printf("PushM\t%beu\n", n);
            break;
	case matchPop:
	    ++t;
	    erts_printf("Pop\n");
	    break;
        case matchSwap:
            ++t;
            erts_printf("Swap\n");
            break;
	case matchBind:
	    ++t;
	    n = *t;
	    ++t;
	    erts_printf("Bind\t%beu\n", n);
	    break;
	case matchCmp:
	    ++t;
	    n = *t;
	    ++t;
	    erts_printf("Cmp\t%beu\n", n);
	    break;
	case matchEqBin:
	    ++t;
	    p = (Eterm) *t;
	    ++t;
	    erts_printf("EqBin\t%p (%T)\n", t, p);
	    break;
	case matchEqRef:
	    ++t;
	    {
		Uint32 *num;
		int ri;

		if (is_ordinary_ref_thing(t)) {
		    ErtsORefThing *rt = (ErtsORefThing *) t;
		    num = rt->num;
		    t += TermWords(ERTS_REF_THING_SIZE);
		}
		else {
		    ErtsMRefThing *mrt = (ErtsMRefThing *) t;
		    ASSERT(is_magic_ref_thing(t));
		    num = mrt->mb->refn;
		    t += TermWords(ERTS_MAGIC_REF_THING_SIZE);
		}

		erts_printf("EqRef\t(%d) {", (int) ERTS_REF_NUMBERS);
		first = 1;
		for (ri = 0; ri < ERTS_REF_NUMBERS; ++ri) {
		    if (first)
			first = 0;
		    else
			erts_printf(", ");
#if defined(ARCH_64)
		    erts_printf("0x%016bex", num[ri]);
#else
		    erts_printf("0x%08bex", num[ri]);
#endif
		}
	    }
	    erts_printf("}\n");
	    break;
	case matchEqBig:
	    ++t;
	    n = thing_arityval(*t);
	    {
		Eterm *et = (Eterm *) t;
		t += TermWords(n+1);
		erts_printf("EqBig\t(%d) {", (int) n);
		first = 1;
		++n;
		while (n--) {
		    if (first)
			first = 0;
		    else
			erts_printf(", ");
#if defined(ARCH_64)
		    erts_printf("0x%016bex", *et);
#else
		    erts_printf("0x%08bex", *et);
#endif
		++et;
		}
	    }
	    erts_printf("}\n");
	    break;
	case matchEqFloat:
	    ++t;
	    {
		double num;
		sys_memcpy(&num,t,sizeof(double));
		t += TermWords(2);
		erts_printf("EqFloat\t%f\n", num);
	    }
	    break;
	case matchEq:
	    ++t;
	    p = (Eterm) *t;
	    ++t;
	    erts_printf("Eq  \t%T\n", p);
	    break;
	case matchList:
	    ++t;
	    erts_printf("List\n");
	    break;
	case matchHalt:
	    ++t;
	    erts_printf("Halt\n");
	    break;
	case matchSkip:
	    ++t;
	    erts_printf("Skip\n");
	    break;
	case matchPushC:
	    ++t;
	    p = (Eterm) *t;
	    ++t;
	    erts_printf("PushC\t%T\n", p);
	    break;
	case matchConsA:
	    ++t;
	    erts_printf("ConsA\n");
	    break;
	case matchConsB:
	    ++t;
	    erts_printf("ConsB\n");
	    break;
	case matchMkTuple:
	    ++t;
	    n = *t;
	    ++t;
	    erts_printf("MkTuple\t%beu\n", n);
	    break;
        case matchMkFlatMap:
            ++t;
            n = *t;
            ++t;
            erts_printf("MkFlatMap\t%beu\n", n);
            break;
        case matchMkHashMap:
            ++t;
            n = *t;
            ++t;
            erts_printf("MkHashMap\t%beu\n", n);
            break;
	case matchOr:
	    ++t;
	    n = *t;
	    ++t;
	    erts_printf("Or\t%beu\n", n);
	    break;
	case matchAnd:
	    ++t;
	    n = *t;
	    ++t;
	    erts_printf("And\t%beu\n", n);
	    break;
	case matchOrElse:
	    ++t;
	    n = *t;
	    ++t;
	    erts_printf("OrElse\t%beu\n", n);
	    break;
	case matchAndAlso:
	    ++t;
	    n = *t;
	    ++t;
	    erts_printf("AndAlso\t%beu\n", n);
	    break;
	case matchCall0:
	    ++t;
	    p = dmc_lookup_bif_reversed((void *) *t);
	    ++t;
	    erts_printf("Call0\t%T\n", p);
	    break;
	case matchCall1:
	    ++t;
	    p = dmc_lookup_bif_reversed((void *) *t);
	    ++t;
	    erts_printf("Call1\t%T\n", p);
	    break;
	case matchCall2:
	    ++t;
	    p = dmc_lookup_bif_reversed((void *) *t);
	    ++t;
	    erts_printf("Call2\t%T\n", p);
	    break;
	case matchCall3:
	    ++t;
	    p = dmc_lookup_bif_reversed((void *) *t);
	    ++t;
	    erts_printf("Call3\t%T\n", p);
	    break;
	case matchPushV:
	    ++t;
	    n = (Uint) *t;
	    ++t;
	    erts_printf("PushV\t%beu\n", n);
	    break;
	case matchPushVResult:
	    n = (Uint) *++t;
	    ++t;
	    erts_printf("PushVResult\t%beu\n", n);
	    break;
	case matchTrue:
	    ++t;
	    erts_printf("True\n");
	    break;
	case matchPushExpr:
	    ++t;
	    erts_printf("PushExpr\n");
	    break;
	case matchPushArrayAsList:
	    ++t;
	    erts_printf("PushArrayAsList\n");
	    break;
	case matchPushArrayAsListU:
	    ++t;
	    erts_printf("PushArrayAsListU\n");
	    break;
	case matchSelf:
	    ++t;
	    erts_printf("Self\n");
	    break;
	case matchWaste:
	    ++t;
	    erts_printf("Waste\n");
	    break;
	case matchReturn:
	    ++t;
	    erts_printf("Return\n");
	    break;
	case matchProcessDump:
	    ++t;
	    erts_printf("ProcessDump\n");
	    break;
	case matchDisplay:
	    ++t;
	    erts_printf("Display\n");
	    break;
	case matchIsSeqTrace:
	    ++t;
	    erts_printf("IsSeqTrace\n");
	    break;
	case matchSetSeqToken:
	    ++t;
	    erts_printf("SetSeqToken\n");
	    break;
	case matchSetSeqTokenFake:
	    ++t;
	    erts_printf("SetSeqTokenFake\n");
	    break;
	case matchGetSeqToken:
	    ++t;
	    erts_printf("GetSeqToken\n");
	    break;
	case matchSetReturnTrace:
	    ++t;
	    erts_printf("SetReturnTrace\n");
	    break;
	case matchSetExceptionTrace:
	    ++t;
	    erts_printf("SetReturnTrace\n");
	    break;
	case matchCatch:
	    ++t;
	    erts_printf("Catch\n");
	    break;
	case matchEnableTrace:
	    ++t;
	    erts_printf("EnableTrace\n");
	    break;
	case matchDisableTrace:
	    ++t;
	    erts_printf("DisableTrace\n");
	    break;
	case matchEnableTrace2:
	    ++t;
	    erts_printf("EnableTrace2\n");
	    break;
	case matchDisableTrace2:
	    ++t;
	    erts_printf("DisableTrace2\n");
	    break;
	case matchTrace2:
	    ++t;
	    erts_printf("Trace2\n");
	    break;
	case matchTrace3:
	    ++t;
	    erts_printf("Trace3\n");
	    break;
 	case matchCaller:
 	    ++t;
 	    erts_printf("Caller\n");
 	    break;
	default:
	    erts_printf("??? (0x%bpx)\n", *t);
	    ++t;
	    break;
	}
    }
    erts_printf("\n\nterm_save: {");
    first = 1;
    for (tmp = prog->term_save; tmp; tmp = tmp->next) {
	if (first)
	    first = 0;
	else
	    erts_printf(", ");
	erts_printf("%p", tmp);
    }
    erts_printf("}\n");
    erts_printf("num_bindings: %d\n", prog->num_bindings);
    erts_printf("heap_size: %beu\n", prog->heap_size);
    erts_printf("stack_offset: %beu\n", prog->stack_offset);
    erts_printf("text: %p\n", prog->text);
    erts_printf("stack_size: %d (words)\n", prog->heap_size-prog->stack_offset);
    
}

#endif /* DMC_DEBUG */


