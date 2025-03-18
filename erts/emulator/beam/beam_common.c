/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1996-2025. All Rights Reserved.
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
#include "erl_map.h"
#include "bif.h"
#include "erl_proc_sig_queue.h"
#include "erl_nfunc_sched.h"
#include "dist.h"
#include "beam_catches.h"
#include "beam_common.h"
#include "erl_global_literals.h"
#ifdef VALGRIND
#  include <valgrind/memcheck.h>
#endif

#ifdef USE_VM_PROBES
#include "dtrace-wrapper.h"
#endif

static Eterm *get_freason_ptr_from_exc(Eterm exc);
static ErtsCodePtr next_catch(Process* c_p, Eterm *reg);
static void terminate_proc(Process* c_p, Eterm Value);
static void save_stacktrace(Process* c_p, ErtsCodePtr pc, Eterm* reg,
                            const ErtsCodeMFA *bif_mfa, Eterm args);

static Eterm make_arglist(Process* c_p, Eterm* reg, int a);

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
    Eterm* reg = (esdp->registers)->x_reg_array.d;

    /*
     * Top of heap (next free location); grows upwards.
     */
    Eterm* HTOP = NULL;

    /* Stack pointer.  Grows downwards; points
     * to last item pushed (normally a saved
     * continuation pointer).
     */
    Eterm* E = NULL;

    /*
     * Pointer to next threaded instruction.
     */
    const BeamInstr *I = NULL;

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

        if (c_p && ERTS_PROC_GET_PENDING_SUSPEND(c_p))
            erts_proc_sig_handle_pending_suspend(c_p);

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

    ERTS_MSACC_UPDATE_CACHE_X();

    /* Set fcalls even though we ignore it, so we don't
     * confuse code accessing it... */
    c_p->fcalls = CONTEXT_REDS;

#ifndef BEAMASM
    if (ERTS_PROC_GET_SAVED_CALLS_BUF(c_p)) {
        c_p->fcalls = 0;
    }
#endif

    if (erts_atomic32_read_nob(&c_p->state) & ERTS_PSFLG_DIRTY_RUNNING_SYS) {
	erts_execute_dirty_system_task(c_p);
	goto do_dirty_schedule;
    }
    else {
	const ErtsCodeMFA *codemfa;
	Eterm* argp;
	int i, exiting;

	reg = esdp->registers->x_reg_array.d;

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

	I = (const BeamInstr*)c_p->i;

	SWAPIN;

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
	 * This layout is determined by the ErtsNativeFunc struct
	 */

	ERTS_MSACC_SET_STATE_CACHED_X(ERTS_MSACC_STATE_NIF);

	codemfa = erts_code_to_codemfa(I);

	DTRACE_NIF_ENTRY(c_p, codemfa);
	c_p->current = codemfa;
	SWAPOUT;
	PROCESS_MAIN_CHK_LOCKS(c_p);
	ERTS_UNREQ_PROC_MAIN_LOCK(c_p);

	ASSERT(!ERTS_PROC_IS_EXITING(c_p));

        if (BeamIsOpCode(I[0], op_call_bif_W)) {
            exiting = erts_call_dirty_bif(esdp, c_p, I, reg);
        } else {
            ASSERT(BeamIsOpCode(I[0], op_call_nif_WWW));
            exiting = erts_call_dirty_nif(esdp, c_p, I, reg);
        }

	ASSERT(!(c_p->flags & F_HIBERNATE_SCHED));

	PROCESS_MAIN_CHK_LOCKS(c_p);
	ERTS_REQ_PROC_MAIN_LOCK(c_p);
	ERTS_VERIFY_UNUSED_TEMP_ALLOC(c_p);
	ERTS_MSACC_SET_STATE_CACHED_X(ERTS_MSACC_STATE_EMULATOR);
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

void copy_out_registers(Process *c_p, Eterm *reg) {

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
  sys_memcpy(c_p->arg_reg,reg,c_p->arity * sizeof(Eterm));
}

void copy_in_registers(Process *c_p, Eterm *reg) {
#ifdef DEBUG
  int i;
  for (i = 0; i < c_p->arity; i++) {
      CHECK_TERM(c_p->arg_reg[i]);
  }
#endif
  sys_memcpy(reg, c_p->arg_reg, c_p->arity * sizeof(Eterm));

}

void check_monitor_long_schedule(Process *c_p,
                                 Uint64 start_time,
                                 ErtsCodePtr start_time_i) {
    Sint64 diff = erts_timestamp_millis() - start_time;
    if (diff > 0 && (Uint) diff >  erts_system_monitor_long_schedule) {
        const ErtsCodeMFA *inptr = erts_find_function_from_pc(start_time_i);
        const ErtsCodeMFA *outptr = erts_find_function_from_pc(c_p->i);
        monitor_long_schedule_proc(c_p,inptr,outptr,(Uint) diff);
    }
}

ErtsCodeMFA *
ubif2mfa(void* uf)
{
    int i;
    for (i = 0; erts_u_bifs[i].bif; i++) {
	if (erts_u_bifs[i].bif == uf)
	    return &BIF_TRAP_EXPORT(erts_u_bifs[i].exp_ix)->info.mfa;
    }
    erts_exit(ERTS_ERROR_EXIT, "bad u bif: %p\n", uf);
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
  am_badrecord,		/* 20 */
};

/* Returns the return address at E[0] in printable form, skipping tracing in
 * the same manner as gather_stacktrace.
 *
 * This is needed to generate correct stacktraces when throwing errors from
 * instructions that return like an ordinary function, such as call_nif. */
ErtsCodePtr erts_printable_return_address(const Process* p, const Eterm *E) {
    const Eterm *stack_bottom = STACK_START(p);
    const Eterm *scanner = E;

    ASSERT(is_CP(scanner[0]));

    while (scanner < stack_bottom) {
        ErtsCodePtr return_address;

        erts_inspect_frame(scanner, &return_address);

        if (BeamIsReturnTrace(return_address)) {
            scanner += CP_SIZE + BEAM_RETURN_TRACE_FRAME_SZ;
        } else if (BeamIsReturnCallAccTrace(return_address)) {
            scanner += CP_SIZE + BEAM_RETURN_CALL_ACC_TRACE_FRAME_SZ;
        } else if (BeamIsReturnToTrace(return_address)) {
            scanner += CP_SIZE + BEAM_RETURN_TO_TRACE_FRAME_SZ;
        } else {
            return return_address;
        }
    }

    ERTS_ASSERT(!"No continuation pointer on stack");
    return NULL;
}

/*
 * When a new exception is raised, the current stack trace information
 * is quick-saved in a small structure allocated on the heap. Depending
 * on how the exception is eventually caught (perhaps by causing the
 * current process to terminate), the saved information may be used to
 * create a symbolic (human-readable) representation of the stack trace
 * at the point of the original exception.
 */

ErtsCodePtr
handle_error(Process* c_p, ErtsCodePtr pc, Eterm* reg,
             const ErtsCodeMFA *bif_mfa)
{
    Eterm* hp;
    Eterm Value = c_p->fvalue;
    Eterm Args = am_true;

    ASSERT(c_p->freason != TRAP); /* Should have been handled earlier. */

    if (c_p->freason & EXF_RESTORE_NFUNC)
	erts_nfunc_restore_error(c_p, &pc, reg, &bif_mfa);

#ifdef DEBUG
    if (bif_mfa) {
	/* Verify that bif_mfa does not point into our native function wrapper */
	ErtsNativeFunc *nep = ERTS_PROC_GET_NFUNC_TRAP_WRAPPER(c_p);
	ASSERT(!nep || !ErtsInArea(bif_mfa, (char *)nep, sizeof(ErtsNativeFunc)));
    }
#endif

    c_p->i = pc;    /* In case we call erts_exit(). */

    /*
     * Check if we have an arglist and possibly an `error_info` term
     * for the top level call. If so, this is encoded in Value, so we
     * have to dig out the real Value as well as the Arglist and the
     * `error_info` term.
     */

    if (c_p->freason & EXF_ARGLIST) {
	Eterm* tp;
	tp = tuple_val(Value);
	Value = tp[1];
	Args = tp[2];
	switch (arityval(tp[0])) {
	case 2:
	    break;
	case 3:
	    /* Dig out the `error_info` term passed to error/3. */
	    ASSERT(c_p->freason & EXF_HAS_EXT_INFO);
	    c_p->fvalue = tp[3];
	    break;
	default:
	    ASSERT(0);
	    break;
	}
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

    /* Stabilize the exception flags so no further expansion is
       done. */
    c_p->freason = PRIMARY_EXCEPTION(c_p->freason);

    /* Clear out error term from process structure to avoid keeping
       garbage. */
    c_p->fvalue = NIL;

    /* Find a handler or die */
    if ((c_p->catches > 0 || c_p->return_trace_frames > 0)
	&& !(c_p->freason & EXF_PANIC)) {
	ErtsCodePtr new_pc;
        /* The Beam handler code (catch_end or try_end) checks reg[0]
         * for THE_NON_VALUE to see if the previous code finished
         * abnormally. If so, reg[1], reg[2] and reg[3] should hold
         * the term, trace, and exception class, respectively. Note
         * that the handler code will only need to move the class
         * to reg[0] to have all registers correctly set up for the
         * code that follows.
         */
	reg[0] = THE_NON_VALUE;
	reg[1] = Value;
	reg[2] = c_p->ftrace;
	reg[3] = exception_tag[GET_EXC_CLASS(c_p->freason)];
        if ((new_pc = next_catch(c_p, reg))) {

#if defined(BEAMASM) && (defined(NATIVE_ERLANG_STACK) || defined(__aarch64__))
            /* In order to make use of native call and return
             * instructions, when beamasm uses the native stack it
             * doesn't include the CP in the current stack frame,
             * relying on the call and return instructions to do that
             * for us.
             *
             * Therefore, we need to bump the stack pointer as if this were an
             * ordinary return. */

            if (erts_frame_layout == ERTS_FRAME_LAYOUT_FP_RA) {
                FRAME_POINTER(c_p) = (Eterm*)cp_val(c_p->stop[0]);
            }

            c_p->stop += CP_SIZE;
#else
            /* To avoid keeping stale references. */
            c_p->stop[0] = NIL;
#endif
            c_p->ftrace = NIL;
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
static ErtsCodePtr
next_catch(Process* c_p, Eterm *reg) {
    int active_catches = c_p->catches > 0;
    ErtsCodePtr return_address = NULL;
    int have_return_to_trace = 0;
    Eterm *ptr, *prev;
    ErtsCodePtr handler;
#ifdef DEBUG
    ErtsCodePtr dbg_return_to_trace_address = NULL;
#endif

    ptr = prev = c_p->stop;
    ASSERT(ptr < STACK_START(c_p));

    while (ptr < STACK_START(c_p)) {
        Eterm val = ptr[0];

        if (is_catch(val)) {
            if (active_catches) {
                goto found_catch;
            }

            ptr++;
        } else if (is_CP(val)) {
            const Eterm *frame;

            prev = ptr;
            frame = erts_inspect_frame(ptr, &return_address);

            if (BeamIsReturnTrace(return_address)) {
                if (return_address == beam_exception_trace) {
                    ErtsCodeMFA *mfa;

                    mfa = (ErtsCodeMFA*)cp_val(frame[0]);

                    ASSERT_MFA(mfa);
                    erts_trace_exception(c_p, mfa, reg[3], reg[1], frame[1], frame[2]);
                }
                ASSERT(c_p->return_trace_frames > 0);
                c_p->return_trace_frames--;

                ptr += CP_SIZE + BEAM_RETURN_TRACE_FRAME_SZ;
            } else if (BeamIsReturnCallAccTrace(return_address)) {
                ptr += CP_SIZE + BEAM_RETURN_CALL_ACC_TRACE_FRAME_SZ;
            } else if (BeamIsReturnToTrace(return_address)) {
                ErtsTracerRef *ref = get_tracer_ref_from_weak_id(&c_p->common,
                                                                 frame[0]);
                if (ref && IS_SESSION_TRACED_FL(ref, F_TRACE_RETURN_TO)) {
                    ref->flags |= F_TRACE_RETURN_TO_MARK;
                    have_return_to_trace = 1;
                }
                ptr += CP_SIZE + BEAM_RETURN_TO_TRACE_FRAME_SZ;
            } else {
            #ifdef DEBUG
                dbg_return_to_trace_address = return_address;
            #endif
                ptr += CP_SIZE;
            }
        } else {
            ptr++;
        }
    }
    if (have_return_to_trace) {
        ErtsTracerRef *ref;
        for (ref = c_p->common.tracee.first_ref; ref; ref = ref->next) {
            ref->flags &= ~F_TRACE_RETURN_TO_MARK;
        }
    }
    return NULL;

 found_catch:
    ASSERT(ptr < STACK_START(c_p));
    c_p->stop = prev;

    if (have_return_to_trace) {
        ErtsTracerRef *ref;
        /*
         * Execution now continues after catching exception from
         * return_to traced function(s).
         */
        ASSERT(return_address == dbg_return_to_trace_address);

        for (ref = c_p->common.tracee.first_ref; ref; ref = ref->next) {
            if (ref->flags & F_TRACE_RETURN_TO_MARK) {
                ASSERT(IS_SESSION_TRACED_FL(ref, F_TRACE_RETURN_TO));
                erts_trace_return_to(c_p, return_address, ref);
                ref->flags &= ~F_TRACE_RETURN_TO_MARK;
            }
        }
    }

    /* Clear the try_tag or catch_tag in the stack frame so that we
     * don't have to do it in the JITted code for the try_case
     * instruction. (Unfortunately, a catch_end will still need to
     * clear the catch_tag because it is executed even when no
     * exception has occurred.) */
    handler = catch_pc(*ptr);
    *ptr = NIL;
    return handler;
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
    c_p->ftrace = NIL;

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
Eterm
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
    case (GET_EXC_INDEX(EXC_BADRECORD)):
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


static void
gather_stacktrace(Process* p, struct StackTrace* s)
{
    ErtsCodePtr prev;
    Eterm *ptr;

    if (s->depth >= s->max_depth) {
        ASSERT(s->depth == s->max_depth);
        return;
    }

    prev = s->depth ? s->trace[s->depth - 1] : s->pc;
    ptr = p->stop;

    /*
     * Traverse the stack backwards and add all unique continuation
     * pointers to the buffer, up to the maximum stack trace size.
     *
     * Skip trace stack frames.
     */

    ASSERT(ptr >= STACK_TOP(p) && ptr <= STACK_START(p));

    while (ptr < STACK_START(p) && s->depth < s->max_depth) {
        if (is_CP(*ptr)) {
            ErtsCodePtr return_address;

            erts_inspect_frame(ptr, &return_address);

            if (BeamIsReturnTrace(return_address)) {
                ptr += CP_SIZE + BEAM_RETURN_TRACE_FRAME_SZ;
            } else if (BeamIsReturnCallAccTrace(return_address)) {
                ptr += CP_SIZE + BEAM_RETURN_CALL_ACC_TRACE_FRAME_SZ;
            } else if (BeamIsReturnToTrace(return_address)) {
                ptr += CP_SIZE + BEAM_RETURN_TO_TRACE_FRAME_SZ;
            } else {
                if (return_address != prev) {
                    ErtsCodePtr adjusted_address;

                    /* Record non-duplicates only */
                    prev = return_address;

#ifdef BEAMASM
                    /* Some instructions (e.g. call) are shorter than one word,
                     * so we will need to subtract one byte from the pointer
                     * to avoid ending up before the start of the
                     * instruction. */
                    adjusted_address = ((char*)return_address) - 1;
#else
                    /* Subtract one word from the pointer. */
                    adjusted_address = ((char*)return_address) - sizeof(UWord);
#endif

                    s->trace[s->depth++] = adjusted_address;
                }

                ptr += CP_SIZE;
            }
        } else {
            ptr++;
        }
    }
}

/*
 * Quick-saving the stack trace in an internal form on the heap. Note
 * that c_p->ftrace will point to a cons cell which holds the given args
 * and the saved data (encoded as a bignum).
 *
 * There is an issue with line number information. Line number
 * information is associated with the address *before* an operation
 * that may fail or be stored on the stack. But continuation
 * pointers point after its call instruction, not before. To avoid
 * finding the wrong line number, we'll need to adjust them so that
 * they point at the beginning of the call instruction or inside the
 * call instruction. Since its impractical to point at the beginning,
 * we'll do the simplest thing and decrement the continuation pointers
 * by one word in threaded interpreter and by one byte in BEAMASM.
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
save_stacktrace(Process* c_p, ErtsCodePtr pc, Eterm* reg,
                const ErtsCodeMFA *bif_mfa, Eterm args) {
    struct StackTrace* s;
    int sz;
    /* Max depth (never negative), -1 as there is always a current function. */
    const int max_depth = MAX(erts_backtrace_depth - 1, 0);
    Eterm error_info = THE_NON_VALUE;

    /* Create a bignum container for the stack trace */
    sz = (offsetof(struct StackTrace, trace) + sizeof(ErtsCodePtr) * max_depth
          + sizeof(Eterm) - 1) / sizeof(Eterm);
    s = (struct StackTrace *) HAlloc(c_p, sz);
    /* The following fields are inside the bignum */
    s->header = make_pos_bignum_header(sz - 1);
    s->freason = c_p->freason;
    s->depth = 0;
    s->max_depth = max_depth;

    /*
     * If the failure was in a BIF other than 'error/1', 'error/2',
     * 'error/3', 'exit/1', or 'throw/1', save BIF MFA and save the
     * argument registers by consing up an arglist.
     */
    if (bif_mfa) {
	Eterm *hp;
        Eterm format_module = THE_NON_VALUE;

	if (bif_mfa->module == am_erlang) {
	    switch (bif_mfa->function) {
	    case am_error:
		if (bif_mfa->arity == 1 || bif_mfa->arity == 2 || bif_mfa->arity == 3)
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
	if (s->depth < max_depth) {
	    s->trace[s->depth++] = pc;
	}
	s->pc = NULL;

        /*
         * All format_error/2 functions for BIFs must be in separate modules,
         * to allow them to be removed from application systems with tight
         * storage constraints.
         */

        switch (bif_mfa->module) {
            /* Erts */
        case am_atomics:
            format_module = am_erl_erts_errors;
            break;
        case am_counters:
            format_module = am_erl_erts_errors;
            break;
        case am_erlang:
            format_module = am_erl_erts_errors;
            break;
        case am_erts_internal:
            format_module = am_erl_erts_errors;
            break;
        case am_persistent_term:
            format_module = am_erl_erts_errors;
            break;

            /* Kernel */
        case am_code:
            format_module = am_erl_kernel_errors;
            break;
        case am_os:
            format_module = am_erl_kernel_errors;
            break;

            /* STDLIB */
        case am_binary:
            format_module = am_erl_stdlib_errors;
            break;
        case am_ets:
            format_module = am_erl_stdlib_errors;
            break;
        case am_lists:
            format_module = am_erl_stdlib_errors;
            break;
        case am_maps:
            format_module = am_erl_stdlib_errors;
            break;
        case am_math:
            format_module = am_erl_stdlib_errors;
            break;
        case am_re:
            format_module = am_erl_stdlib_errors;
            break;
        case am_unicode:
            format_module = am_erl_stdlib_errors;
            break;

        default:
            ASSERT((c_p->freason & EXF_HAS_EXT_INFO) == 0);
            break;
        }

        if (is_value(format_module)) {
            if (c_p->freason & EXF_HAS_EXT_INFO) {
                hp = HAlloc(c_p, MAP2_SZ);
                error_info = MAP2(hp,
                                  am_cause, c_p->fvalue,
                                  am_module, format_module);
            } else {
                hp = HAlloc(c_p, MAP1_SZ);
                error_info = MAP1(hp, am_module, format_module);
            }
        }

	args = make_arglist(c_p, reg, bif_mfa->arity);
    } else {
        if (c_p->freason & EXF_HAS_EXT_INFO && is_map(c_p->fvalue)) {
            error_info = c_p->fvalue;
        }

    non_bif_stacktrace:
	s->current = c_p->current;

        /*
	 * For a function_clause error, the arguments are in the beam
	 * registers and c_p->current is set.
	 */
	if ( (GET_EXC_INDEX(s->freason)) ==
	     (GET_EXC_INDEX(EXC_FUNCTION_CLAUSE)) ) {
	    int a;
	    ASSERT(s->current);
	    a = s->current->arity;
	    args = make_arglist(c_p, reg, a);
	    s->pc = NULL; /* Ignore pc */
	} else {
	    s->pc = pc;
	}
    }

    if (c_p->freason == EXC_ERROR_3) {
	error_info = c_p->fvalue;
    }

    /* Package args and stack trace */
    {
	Eterm *hp;
	Uint sz = 4;

	if (is_value(error_info)) {
	    sz += 3 + 2;
	}
	hp = HAlloc(c_p, sz);
	if (is_non_value(error_info)) {
	    error_info = NIL;
	} else {
	    error_info = TUPLE2(hp, am_error_info, error_info);
	    hp += 3;
	    error_info = CONS(hp, error_info, NIL);
	    hp += 2;
	}
	c_p->ftrace = TUPLE3(hp, make_big((Eterm *) s), args, error_info);
    }

    /* Save the actual stack trace */
    gather_stacktrace(c_p, s);

#ifdef VALGRIND
    /* Make sure entire bignum is defined in case it shows up in a crash dump */
    {
        const int words_left = s->max_depth - s->depth;
        if (words_left) {
            VALGRIND_MAKE_MEM_DEFINED(&s->trace[s->depth],
                                      words_left * sizeof(ErtsCodePtr));
        }
    }
#endif
}

void
erts_save_stacktrace(Process* p, struct StackTrace* s)
{
    gather_stacktrace(p, s);
}

/*
 * Getting the relevant fields from the term pointed to by ftrace
 */

static struct StackTrace *get_trace_from_exc(Eterm exc) {
    if (exc == NIL) {
	return NULL;
    } else {
	Eterm* tuple_ptr = tuple_val(exc);
	ASSERT(tuple_ptr[0] == make_arityval(3));
	return (struct StackTrace *) big_val(tuple_ptr[1]);
    }
}

void erts_sanitize_freason(Process* c_p, Eterm exc) {
    struct StackTrace *s = get_trace_from_exc(exc);

    if (s == NULL) {
        c_p->freason = EXC_ERROR;
    } else {
        c_p->freason = PRIMARY_EXCEPTION(s->freason);
    }
}

static Eterm get_args_from_exc(Eterm exc) {
    if (exc == NIL) {
	return NIL;
    } else {
	Eterm* tuple_ptr = tuple_val(exc);
	ASSERT(tuple_ptr[0] == make_arityval(3));
	return tuple_ptr[2];
    }
}

static Eterm get_error_info_from_exc(Eterm exc) {
    if (exc == NIL) {
	return NIL;
    } else {
	Eterm* tuple_ptr = tuple_val(exc);
	ASSERT(tuple_ptr[0] == make_arityval(3));
	return tuple_ptr[3];
    }
}

static int is_raised_exc(Eterm exc) {
    if (exc == NIL) {
        return 0;
    } else {
	Eterm* tuple_ptr = tuple_val(exc);
        return bignum_header_is_neg(*big_val(tuple_ptr[1]));
    }
}

static Eterm *get_freason_ptr_from_exc(Eterm exc) {
    static Eterm dummy_freason;
    struct StackTrace* s;

    if (exc == NIL) {
        /*
         * It is not exactly clear when exc can be NIL. Probably only
         * when the exception has been generated from native code.
         * Return a pointer to an Eterm that can be safely written and
         * ignored.
         */
	return &dummy_freason;
    } else {
	Eterm* tuple_ptr = tuple_val(exc);
	ASSERT(tuple_ptr[0] == make_arityval(3));
	s = (struct StackTrace *) big_val(tuple_ptr[1]);
        return &s->freason;
    }
}

int raw_raise(Eterm stacktrace, Eterm exc_class, Eterm value, Process *c_p) {
  Eterm* freason_ptr;

  /*
   * Note that the i_raise instruction will override c_p->freason
   * with the freason field stored inside the StackTrace struct in
   * ftrace. Therefore, we must take care to store the class both
   * inside the StackTrace struct and in c_p->freason (important if
   * the class is different from the class of the original
   * exception).
   */
  freason_ptr = get_freason_ptr_from_exc(stacktrace);

  if (exc_class == am_error) {
    *freason_ptr = c_p->freason = EXC_ERROR & ~EXF_SAVETRACE;
    c_p->fvalue = value;
    c_p->ftrace = stacktrace;
    return 0;
  } else if (exc_class == am_exit) {
    *freason_ptr = c_p->freason = EXC_EXIT & ~EXF_SAVETRACE;
    c_p->fvalue = value;
    c_p->ftrace = stacktrace;
    return 0;
  } else if (exc_class == am_throw) {
    *freason_ptr = c_p->freason = EXC_THROWN & ~EXF_SAVETRACE;
    c_p->fvalue = value;
    c_p->ftrace = stacktrace;
    return 0;
  } else {
    return 1;
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
    Eterm error_info;
    int i;

    if (! (s = get_trace_from_exc(exc))) {
        return NIL;
    } else if (is_raised_exc(exc)) {
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

    error_info = get_error_info_from_exc(exc);

    /*
     * Initialize needed heap.
     */
    heap_size = fi.mfa ? fi.needed + 2 : 0;

    /*
     * Look up all saved continuation pointers and calculate
     * needed heap space.
     */
    stk = stkp = (FunctionInfo *) erts_alloc(ERTS_ALC_T_TMP,
				      depth*sizeof(FunctionInfo));
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
	hp = erts_build_mfa_item(stkp, hp, am_true, &mfa, NIL);
	res = CONS(hp, mfa, res);
	hp += 2;
    }
    if (fi.mfa) {
	hp = erts_build_mfa_item(&fi, hp, args, &mfa, error_info);
	res = CONS(hp, mfa, res);
	hp += 2;
    }

    erts_free(ERTS_ALC_T_TMP, (void *) stk);
    return res;
}

const Export *call_error_handler(Process* p,
                                 const ErtsCodeMFA *mfa,
                                 Eterm* reg,
                                 Eterm func)
{
    const Export* ep;
    Eterm* hp;
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
    return ep;
}

static const Export *
apply_setup_error_handler(Process* p, Eterm module, Eterm function, Uint arity, Eterm* reg)
{
    const Export *ep;

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

        hp = HAlloc(p, sz);
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
apply_bif_error_adjustment(Process *p, const Export *ep,
                           Eterm *reg, Uint arity,
                           ErtsCodePtr I, Uint stack_offset)
{
    int apply_only;
    Uint need;

    need = stack_offset /* bytes */ / sizeof(Eterm);
    apply_only = stack_offset == 0;

    /*
     * I is only set when the apply is a tail call, i.e.,
     * from the instructions i_apply_only, i_apply_last_P,
     * and apply_last_IP.
     */
    if (!(I && (ep->bif_number == BIF_error_1 ||
                ep->bif_number == BIF_error_2 ||
                ep->bif_number == BIF_error_3 ||
                ep->bif_number == BIF_exit_1 ||
                ep->bif_number == BIF_throw_1))) {
        return;
    }

    /*
     * We are about to tail apply one of the BIFs erlang:error/1,
     * erlang:error/2, erlang:error/3, erlang:exit/1, or
     * erlang:throw/1. Error handling of these BIFs is special!
     *
     * We need the topmost continuation pointer to point into the calling
     * function when handling the error after the BIF has been applied. This in
     * order to get the topmost stackframe correct.
     *
     * Note that these BIFs will unconditionally cause an exception to be
     * raised. That is, our modifications of the stack will be corrected by the
     * error handling code.
     */
    if (need == 0) {
        need = CP_SIZE; /* i_apply_only */
    }

    if (HeapWordsLeft(p) < need) {
        erts_garbage_collect(p, (int) need, reg, arity+1);
    }

    if (apply_only) {
        /*
         * Called from the i_apply_only instruction.
         *
         * Push the continuation pointer for the current function to the stack.
         */
        p->stop -= need;

        switch (erts_frame_layout) {
        case ERTS_FRAME_LAYOUT_RA:
            p->stop[0] = make_cp(I);
            break;
        case ERTS_FRAME_LAYOUT_FP_RA:
            p->stop[0] = make_cp(FRAME_POINTER(p));
            p->stop[1] = make_cp(I);
            FRAME_POINTER(p) = &p->stop[0];
            break;
        }
    } else {
        /*
         * Called from an i_apply_last_* instruction.
         *
         * The calling instruction will deallocate a stack frame of size
         * 'stack_offset'.
         *
         * Push the continuation pointer for the current function to the stack,
         * and then add a dummy stackframe for the i_apply_last* instruction
         * to discard.
         */
        switch (erts_frame_layout) {
        case ERTS_FRAME_LAYOUT_RA:
            p->stop[0] = make_cp(I);
            break;
        case ERTS_FRAME_LAYOUT_FP_RA:
            p->stop[0] = make_cp(FRAME_POINTER(p));
            p->stop[1] = make_cp(I);
            FRAME_POINTER(p) = &p->stop[0];
            break;
        }

        p->stop -= need;
    }
}

const Export *
apply(Process* p, Eterm* reg, ErtsCodePtr I, Uint stack_offset)
{
    const Export *ep;
    int arity;
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
	return NULL;
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
    }
    apply_bif_error_adjustment(p, ep, reg, arity, I, stack_offset);
    DTRACE_GLOBAL_CALL_FROM_EXPORT(p, ep);
    return ep;
}

const Export *
fixed_apply(Process* p, Eterm* reg, Uint arity,
            ErtsCodePtr I, Uint stack_offset)
{
    const Export *ep;
    Eterm module;
    Eterm function;

    module = reg[arity];    /* The THIS pointer already in place */
    function = reg[arity+1];

    if (is_not_atom(function)) {
        Eterm bad_args;
    error:
        bad_args = make_arglist(p, reg, arity);

        p->freason = BADARG;
        reg[0] = module;
        reg[1] = function;
        reg[2] = bad_args;

        return NULL;
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
    }

    apply_bif_error_adjustment(p, ep, reg, arity, I, stack_offset);
    DTRACE_GLOBAL_CALL_FROM_EXPORT(p, ep);
    return ep;
}

void erts_hibernate(Process *c_p, Eterm *regs, int arity) {
    const Uint max_default_arg_reg =
        sizeof(c_p->def_arg_reg) / sizeof(c_p->def_arg_reg[0]);

    /* Save some memory if possible. */
    if (arity <= max_default_arg_reg && c_p->arg_reg != c_p->def_arg_reg) {
        erts_free(ERTS_ALC_T_ARG_REG, c_p->arg_reg);
        c_p->max_arg_reg = max_default_arg_reg;
        c_p->arg_reg = c_p->def_arg_reg;
    }

    sys_memcpy(c_p->arg_reg, regs, arity * sizeof(Eterm));
    c_p->arity = arity;

    /*
     * If there are no waiting messages, garbage collect and
     * shrink the heap.
     */
    erts_proc_lock(c_p, ERTS_PROC_LOCK_MSGQ|ERTS_PROC_LOCK_STATUS);
    if (!erts_proc_sig_fetch(c_p)) {
	erts_proc_unlock(c_p, ERTS_PROC_LOCK_MSGQ|ERTS_PROC_LOCK_STATUS);
	c_p->fvalue = NIL;
	PROCESS_MAIN_CHK_LOCKS(c_p);
	erts_garbage_collect_hibernate(c_p);
	ERTS_VERIFY_UNUSED_TEMP_ALLOC(c_p);
	PROCESS_MAIN_CHK_LOCKS(c_p);
	erts_proc_lock(c_p, ERTS_PROC_LOCK_MSGQ|ERTS_PROC_LOCK_STATUS);
	if (!erts_proc_sig_fetch(c_p))
	    erts_atomic32_read_band_relb(&c_p->state, ~ERTS_PSFLG_ACTIVE);
	ASSERT(!ERTS_PROC_IS_EXITING(c_p));
    }

    erts_proc_unlock(c_p, ERTS_PROC_LOCK_MSGQ|ERTS_PROC_LOCK_STATUS);
    c_p->flags |= F_HIBERNATE_SCHED; /* Needed also when woken! */
}

ErtsCodePtr
call_fun(Process* p,    /* Current process. */
         int arity,     /* Number of arguments for Fun. */
         Eterm* reg,    /* Contents of registers. */
         Eterm args)    /* THE_NON_VALUE or pre-built list of arguments. */
{
    ErtsCodeIndex code_ix;
    ErtsCodePtr code_ptr;
    ErlFunThing *funp;
    Eterm fun;

    fun = reg[arity];

    if (is_not_any_fun(fun)) {
        p->current = NULL;
        p->freason = EXC_BADFUN;
        p->fvalue = fun;
        return NULL;
    }

    funp = (ErlFunThing*)fun_val(fun);

    code_ix = erts_active_code_ix();
    code_ptr = (funp->entry.disp)->addresses[code_ix];

    if (ERTS_LIKELY(code_ptr != beam_unloaded_fun &&
                    fun_arity(funp) == arity)) {
        for (int i = 0; i < fun_num_free(funp); i++) {
            reg[i + arity] = funp->env[i];
        }

#ifdef USE_VM_CALL_PROBES
        if (is_local_fun(funp)) {
            DTRACE_LOCAL_CALL(p, erts_code_to_codemfa(code_ptr));
        } else {
            Export *ep = funp->entry.exp;
            DTRACE_GLOBAL_CALL(p, &ep->info.mfa);
        }
#endif

        return code_ptr;
    } else {
        /* Something wrong here. First build a list of the arguments. */
        if (is_non_value(args)) {
            Uint sz = 2 * arity;
            Eterm *hp;

            args = NIL;

            if (HeapWordsLeft(p) < sz) {
                erts_garbage_collect(p, sz, reg, arity+1);

                fun = reg[arity];
                funp = (ErlFunThing*)fun_val(fun);
            }

            hp = HEAP_TOP(p);
            HEAP_TOP(p) += sz;

            for (int i = arity - 1; i >= 0; i--) {
                args = CONS(hp, reg[i], args);
                hp += 2;
            }
        }

        if (fun_arity(funp) != arity) {
            /* There is a fun defined, but the call has the wrong arity. */
            Eterm *hp = HAlloc(p, 3);
            p->freason = EXC_BADARITY;
            p->fvalue = TUPLE2(hp, fun, args);
            return NULL;
        } else {
            const ErlFunEntry *fe;
            const Export *ep;
            Eterm module;
            Module *modp;

            /* There is no module loaded that defines the fun, either because
             * the fun is newly created from the external representation (the
             * module has never been loaded), or the module defining the fun
             * has been unloaded. */
            ASSERT(is_local_fun(funp) && code_ptr == beam_unloaded_fun);
            fe = funp->entry.fun;
            module = fe->module;

            ERTS_THR_READ_MEMORY_BARRIER;
            if (fe->pend_purge_address) {
                /* The system is currently trying to purge the
                 * module containing this fun. Suspend the process
                 * and let it try again when the purge operation is
                 * done (may succeed or not). */
                ep = erts_suspend_process_on_pending_purge_lambda(p, fe);
            } else {
                if ((modp = erts_get_module(module, code_ix)) != NULL
                    && modp->curr.code_hdr != NULL) {
                    /* There is a module loaded, but obviously the fun is
                     * not defined in it. We must not call the error_handler
                     * (or we will get into an infinite loop). */
                    p->current = NULL;
                    p->freason = EXC_BADFUN;
                    p->fvalue = fun;
                    return NULL;
                }

                /* No current code for this module. Call the error_handler
                 * module to attempt loading the module. */

                ep = erts_find_function(erts_proc_get_error_handler(p),
                                        am_undefined_lambda, 3, code_ix);
                if (ep == NULL) {
                    /* No error handler */
                    p->current = NULL;
                    p->freason = EXC_UNDEF;
                    return NULL;
                }
            }

            ASSERT(ep);

            reg[0] = module;
            reg[1] = fun;
            reg[2] = args;
            reg[3] = NIL;

            return ep->dispatch.addresses[code_ix];
        }
    }
}

ErtsCodePtr
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

int
is_function2(Eterm Term, Uint arity)
{
    if (is_any_fun(Term)) {
        ErlFunThing *funp = (ErlFunThing*)fun_val(Term);
        return fun_arity(funp) == arity;
    }

    return 0;
}

Eterm get_map_element(Eterm map, Eterm key)
{
    erts_ihash_t hx;
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

Eterm get_map_element_hash(Eterm map, Eterm key, erts_ihash_t hx)
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


Eterm
erts_gc_new_map(Process* p, Eterm* reg, Uint live,
                Uint n, const Eterm* ptr)
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
    mhp    = thp + (n == 0 ? 0 : 1) + n/2;
    E      = p->stop;
    if (n == 0) {
        keys   = ERTS_GLOBAL_LIT_EMPTY_TUPLE;
    } else {
        keys   = make_tuple(thp);
        *thp++ = make_arityval(n/2);
    }
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

Eterm
erts_gc_new_small_map_lit(Process* p, Eterm* reg, Eterm keys_literal,
                          Uint live, const Eterm* ptr)
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

Eterm
erts_gc_update_map_assoc(Process* p, Eterm* reg, Uint live,
                         Uint n, const Eterm* new_p)
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
    int changed_values = 0;
    int changed_keys = 0;

    num_updates = n / 2;
    map = reg[live];

    if (is_not_flatmap(map)) {
	erts_ihash_t hx;
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
     * | MAP_HEADER_FLATMAP                |
     * +-----------------------------------+
     * | (Space for number of keys/values) |
     * +-----------------------------------+
     * | Boxed tuple pointer            >----------------+
     * +-----------------------------------+             |
     * | (Space for value 1)               |             |    <-- hp
     * +-----------------------------------+             |
     *        .                                          |
     *        .                                          |
     *        .                                          |
     * +-----------------------------------+             |
     * | (Space for last value)	           |             |
     * +-----------------------------------+             |
     * +-----------------------------------+             |
     * | (Space for aritvyal for keys)     | <-----------+
     * +-----------------------------------+
     * | (Space for key 1)                 |                  <-- kp
     * +-----------------------------------+
     *        .
     *        .
     *        .
     * +-----------------------------------+
     * | (Space for last key)              |
     * +-----------------------------------+
     */

    hp = p->htop;
    E = p->stop;

    res = make_flatmap(hp);
    mp = (flatmap_t *)hp;
    hp += MAP_HEADER_FLATMAP_SZ;
    mp->thing_word = MAP_HEADER_FLATMAP;

    kp = hp + num_old + num_updates; /* Point to key tuple. */

    mp->keys = make_tuple(kp);

    kp = kp + 1;                /* Point to first key. */

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

	key = *old_keys;
	if ((c = (key == new_key) ? 0 : erts_cmp_flatmap_keys(key, new_key)) < 0) {
	    /* Copy old key and value */
	    *kp++ = key;
	    *hp++ = *old_vals;
	    old_keys++, old_vals++, num_old--;
	} else {		/* Replace or insert new */
	    GET_TERM(new_p[1], *hp);
	    if (c > 0) {	/* If new key */
		*kp++ = new_key;
                changed_keys = 1;
	    } else {		/* If replacement */
                if (*old_vals != *hp) {
                    changed_values = 1;
                }
		*kp++ = key;
		old_keys++, old_vals++, num_old--;
	    }
            hp++;
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
    } else if (!changed_keys && !changed_values) {
        /*
         * All updates are now done, no new keys were introduced, and
         * all new values were the same as old ones. We can just
         * return the old map and skip committing the new allocation,
         * effectively releasing it.
         */
        ASSERT(n == 0);
        return map;
    } else if (!changed_keys) {
        /*
         * All updates are now done, no new keys were introduced, but
         * some values were changed. We can retain the old key tuple.
         */
        ASSERT(n == 0);
        mp->size = old_mp->size;
        mp->keys = old_mp->keys;
        while (num_old-- > 0) {
            *hp++ = *old_vals++;
        }
        p->htop = hp;
        return res;
    } else {
	/*
	 * All updates are now done. We may still have old
	 * keys and values that we must copy.
	 */
	ASSERT(n == 0);
	while (num_old-- > 0) {
	    *kp++ = *old_keys++;
	    *hp++ = *old_vals++;
	}
    }

    /*
     * Calculate how many values that are unused at the end of the
     * value array and fill it out with a bignum header.
     */
    if ((n = boxed_val(mp->keys) - hp) > 0) {
        ASSERT(n <= num_updates);
	*hp = make_pos_bignum_header(n-1);
    }

    /*
     * Fill in the size of the map in both the key tuple and in the map.
     */

    n = hp - (Eterm *)mp - MAP_HEADER_FLATMAP_SZ;	/* Actual number of keys/values */
    ASSERT(n <= old_mp->size + num_updates);
    mp->size = n;
    *(boxed_val(mp->keys)) = make_arityval(n);
    p->htop  = kp;

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

Eterm
erts_gc_update_map_exact(Process* p, Eterm* reg, Uint live,
                         Uint n, const Eterm* new_p)
{
    Uint i;
    Uint num_old;
    Uint need;
    flatmap_t *old_mp, *mp;
    Eterm res;
    Eterm* old_hp;
    Eterm* hp;
    Eterm* E;
    Eterm* old_keys;
    Eterm* old_vals;
    Eterm new_key;
    Eterm map;
    int changed = 0;

    n /= 2;		/* Number of values to be updated */
    ASSERT(n > 0);
    map = reg[live];

    if (is_not_flatmap(map)) {
	erts_ihash_t hx;
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

    old_hp = p->htop;
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
            if(*hp != *old_vals) changed = 1;
            hp++;
            n--;
	    if (n == 0) {
                /*
                * All updates done. Copy remaining values
                * if any changed or return the original one.
                */
                if(changed) {
		    for (i++, old_vals++; i < num_old; i++) {
		        *hp++ = *old_vals++;
		    }
		    ASSERT(hp == p->htop + need);
		    p->htop = hp;
		    return res;
                } else {
                    p->htop = old_hp;
                    return map;
                }
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
    const Export *ep;
    Export e;

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

    return ep->bif_number != -1;
}


/*
 * Return the current number of reductions consumed by the given process.
 * To get the total number of reductions, p->reds must be added.
 */

Uint
erts_current_reductions(Process *c_p, Process *p)
{
    Sint reds_left;
    if (c_p != p || !(erts_atomic32_read_nob(&c_p->state)
                      & ERTS_PSFLG_RUNNING)) {
	return 0;
#ifndef BEAMASM
    /* BEAMASM doesn't use negative reductions for save_calls. */
    } else if (c_p->fcalls < 0 && ERTS_PROC_GET_SAVED_CALLS_BUF(c_p)) {
	reds_left = c_p->fcalls + CONTEXT_REDS;
#endif
    } else {
        reds_left = c_p->fcalls;
    }
    return REDS_IN(c_p) - reds_left - erts_proc_sched_data(p)->virtual_reds;
}
