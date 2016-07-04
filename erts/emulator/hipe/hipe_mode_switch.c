/*
 * %CopyrightBegin%

 *
 * Copyright Ericsson AB 2001-2016. All Rights Reserved.
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
 * hipe_mode_switch.c
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
#include "sys.h"
#include "erl_vm.h"
#include "global.h"
#include "erl_process.h"
#include "beam_load.h"	/* which includes beam_opcodes.h */
#include "beam_catches.h"
#include "hipe_mode_switch.h"
#include "bif.h"
#include "error.h"
#include "hipe_stack.h"
#include "hipe_bif0.h"	/* hipe_mfa_info_table_init() */

#if defined(ERTS_ENABLE_LOCK_CHECK) && defined(ERTS_SMP)
#    define ERTS_SMP_REQ_PROC_MAIN_LOCK(P) \
        if ((P)) erts_proc_lc_require_lock((P), ERTS_PROC_LOCK_MAIN,	\
					   __FILE__, __LINE__)
#    define ERTS_SMP_UNREQ_PROC_MAIN_LOCK(P) \
        if ((P)) erts_proc_lc_unrequire_lock((P), ERTS_PROC_LOCK_MAIN)
#else
#    define ERTS_SMP_REQ_PROC_MAIN_LOCK(P)
#    define ERTS_SMP_UNREQ_PROC_MAIN_LOCK(P)
#endif


/*
 * Internal debug support.
 * #define HIPE_DEBUG to the desired debug level:
 *	0	no checks
 *	1	check PCB consistency at mode-switches
 *	2	log commands and results at mode-switches
 *	3	log commands, results, and PCB contents at mode-switches
 *
 * TODO: check PCB consistency at native BIF calls
 */
int hipe_modeswitch_debug = 0;
extern BeamInstr beam_exit[];

#define HIPE_DEBUG 0

#if HIPE_DEBUG > 1	/* include DPRINTF() logging */

#define DPRINTF(fmt, args...) \
do { \
    if (hipe_modeswitch_debug > 0) { \
	printf("%s, line %u: " fmt "\r\n", __FUNCTION__, __LINE__ , ##args); \
	fflush(stdout); \
    } \
} while (0)

static const char *code_str(unsigned code)
{
    static const char *cmd_str[] = {
	"call from beam",
	"return from beam",
	"throw from beam",
	"resume from beam",
	"return to beam",
	"call to beam",
	"throw to beam",
	"suspend to beam",
	"wait from native",
	"wait_timeout from native",
	"trap from native",
	"call closure from beam",
	"call closure to beam",
    };
    unsigned cmd = code & 0xFF;

    if (cmd < (sizeof(cmd_str)/sizeof(cmd_str[0])))
	return cmd_str[cmd];
    else
	return "???";
}

#else	/* HIPE_DEBUG > 1 */

#define DPRINTF(fmt, args...)	do{}while(0)

#endif	/* HIPE_DEBUG > 1 */

#if HIPE_DEBUG > 0	/* include HIPE_ASSERT and PCB checking */

static void __noreturn
hipe_abort(const char *expr, const char *file, unsigned line)
{
    erts_exit(ERTS_ERROR_EXIT, "ASSERTION FAILED, file %s, line %u: %s\r\n", file, line, expr);
}

#define HIPE_ASSERT3(expr, file, line) \
do { \
    if (!(expr)) \
	hipe_abort(#expr, file, line); \
} while (0)
#define HIPE_ASSERT(expr)	HIPE_ASSERT3(expr, __FILE__, __LINE__)

void hipe_check_pcb(Process *p, const char *file, unsigned line)
{
#if HIPE_DEBUG > 2
    if (hipe_modeswitch_debug > 0) {
        printf("%s, line %u: p %p = {htop %p, stop %p, nstack %p, nsp %p, nstend %p}\r\n", file, line, p, p->htop, p->stop, p->hipe.nstack, p->hipe.nsp, p->hipe.nstend);
    }
#endif
    HIPE_ASSERT3(p != NULL, file, line);
    HIPE_ASSERT3(p->htop <= p->stop, file, line);
    HIPE_ASSERT3(p->hipe.nstack <= p->hipe.nstend, file, line);
    HIPE_ASSERT3(p->hipe.nsp >= p->hipe.nstack, file, line);
    HIPE_ASSERT3(p->hipe.nsp <= p->hipe.nstend, file, line);
}
#define HIPE_CHECK_PCB(P)	hipe_check_pcb((P), __FILE__, __LINE__)

#else	/* HIPE_DEBUG > 0 */

#define HIPE_ASSERT(expr)	do{}while(0)
#define HIPE_CHECK_PCB(P)	do{}while(0)

#endif	/* HIPE_DEBUG > 0 */

/* ensure that at least nwords words are available on the native stack */

#if defined(__sparc__)
#include "hipe_sparc_glue.h"
#elif defined(__i386__)
#include "hipe_x86_glue.h"
#elif defined(__x86_64__)
#include "hipe_amd64_glue.h"
#elif defined(__powerpc__) || defined(__ppc__) || defined(__powerpc64__)
#include "hipe_ppc_glue.h"
#elif defined(__arm__)
#include "hipe_arm_glue.h"
#endif

#define BeamOpCode(Op)		((Uint)BeamOp(Op))

Uint hipe_beam_pc_return[1];	/* needed in hipe_debug.c */
Uint hipe_beam_pc_throw[1];	/* needed in hipe_debug.c */
Uint hipe_beam_pc_resume[1];	/* needed by hipe_set_timeout() */
Eterm hipe_beam_catch_throw;

void hipe_mode_switch_init(void)
{
    hipe_arch_glue_init();

    hipe_beam_pc_return[0] = BeamOpCode(op_hipe_trap_return);
    hipe_beam_pc_throw[0] = BeamOpCode(op_hipe_trap_throw);
    hipe_beam_pc_resume[0] = BeamOpCode(op_hipe_trap_resume);

    hipe_beam_catch_throw =
	make_catch(beam_catches_cons(hipe_beam_pc_throw, BEAM_CATCHES_NIL));

    hipe_mfa_info_table_init();

#if (defined(__i386__) || defined(__x86_64__)) && defined(__linux__)
    /* Verify that the offset of c-p->hipe does not change.
       The ErLLVM hipe backend depends on it being in a specific
       position. Kostis et al has promised to fix this in upstream
       llvm by OTP 20, so it should be possible to remove these asserts
       after that. */
    ERTS_CT_ASSERT(sizeof(ErtsPTabElementCommon) ==
                   (sizeof(Eterm) +                   /* id */
                    sizeof(((ErtsPTabElementCommon*)0)->refc) +
                    sizeof(ErtsTracer) +              /* tracer */
                    sizeof(Uint) +                    /* trace_flags */
                    sizeof(erts_smp_atomic_t) +       /* timer */
                    sizeof(((ErtsPTabElementCommon*)0)->u)));

    ERTS_CT_ASSERT(offsetof(Process, hipe) ==
                   (sizeof(ErtsPTabElementCommon) +   /* common */
                    sizeof(Eterm*) +                  /* htop */
                    sizeof(Eterm*) +                  /* stop */
                    sizeof(Eterm*) +                  /* heap */
                    sizeof(Eterm*) +                  /* hend */
                    sizeof(Uint) +                    /* heap_sz */
                    sizeof(Uint) +                    /* min_heap_size */
                    sizeof(Uint) +                    /* min_vheap_size */
                    sizeof(volatile unsigned long))); /* fp_exception */
#endif

}

void hipe_set_call_trap(Uint *bfun, void *nfun, int is_closure)
{
    HIPE_ASSERT(bfun[-5] == BeamOpCode(op_i_func_info_IaaI));
    bfun[0] =
	is_closure
	? BeamOpCode(op_hipe_trap_call_closure)
	: BeamOpCode(op_hipe_trap_call);
    bfun[-4] = (Uint)nfun;
}

static __inline__ void
hipe_push_beam_trap_frame(Process *p, Eterm reg[], unsigned arity)
{
    if (&p->stop[1] < p->hend && p->stop[1] == hipe_beam_catch_throw) {
	/* Trap frame already reserved */
	ASSERT(p->stop[0] == NIL);
    }
    else {
	ASSERT(!(p->flags & F_DISABLE_GC));
	if ((p->stop - 2) < p->htop) {
	    DPRINTF("calling gc to increase BEAM stack size");
	    erts_garbage_collect(p, 2, reg, arity);
	    ASSERT(!((p->stop - 2) < p->htop));
	}
	p->stop -= 2;
	p->stop[1] = hipe_beam_catch_throw;
    }
    p->stop[0] = make_cp(p->cp);
    ++p->catches;
    p->cp = hipe_beam_pc_return;
}

static __inline__ void hipe_pop_beam_trap_frame(Process *p)
{
    ASSERT(p->stop[1] == hipe_beam_catch_throw);
    p->cp = cp_val(p->stop[0]);
    --p->catches;
    p->stop += 2;
}

Process *hipe_mode_switch(Process *p, unsigned cmd, Eterm reg[])
{
    unsigned result;
    Eterm reds_in = p->def_arg_reg[5];
    /*
     * Process is in the normal case scheduled out when reduction
     * count reach zero. When "save calls" is enabled reduction
     * count is subtracted with CONTEXT_REDS, i.e. initial reduction
     * count will be zero or less and process is scheduled out
     * when -CONTEXT_REDS is reached.
     *
     * HiPE does not support the "save calls" feature, so we switch
     * to using a positive reduction counter when executing in
     * hipe mode, but need to restore the "save calls" when
     * returning to beam. We also need to hide the save calls buffer
     * from BIFs. We do that by moving the saved calls buf to
     * suspended saved calls buf.
     *
     * Beam has initial reduction count in stored in p->def_arg_reg[5].
     *
     * Beam expects -neg_o_reds to be found in p->def_arg_reg[4]
     * on return to beam.
     */

    {
	struct saved_calls *scb = ERTS_PROC_SET_SAVED_CALLS_BUF(p, NULL);
	if (scb) {
	    reds_in += CONTEXT_REDS;
	    p->fcalls += CONTEXT_REDS;
	    ERTS_PROC_SET_SUSPENDED_SAVED_CALLS_BUF(p, scb);
	}
    }

    p->flags |= F_HIPE_MODE; /* inform bifs where we are comming from... */

    p->i = NULL;
    /* Set current_function to undefined. stdlib hibernate tests rely on it. */
    p->current = NULL;

    DPRINTF("cmd == %#x (%s)", cmd, code_str(cmd));
    HIPE_CHECK_PCB(p);
    p->arity = 0;
    switch (cmd & 0xFF) {
      case HIPE_MODE_SWITCH_CMD_CALL: {
	  /* BEAM calls a native code function */
	  unsigned arity = cmd >> 8;

	  /* p->hipe.u.ncallee set in beam_emu */
	  if (p->cp == hipe_beam_pc_return) {
	    /* Native called BEAM, which now tailcalls native. */
	    hipe_pop_beam_trap_frame(p);
	    result = hipe_tailcall_to_native(p, arity, reg);
	    break;
	  }
	  DPRINTF("calling %#lx/%u", (long)p->hipe.u.ncallee, arity);
	  result = hipe_call_to_native(p, arity, reg);
	  break;
      }
      case HIPE_MODE_SWITCH_CMD_CALL_CLOSURE: {
	  /* BEAM calls a native code closure */
	  unsigned arity = cmd >> 8; /* #formals + #fvs (closure not counted) */
	  Eterm fun;
	  ErlFunThing *funp;

	  /* drop the fvs, move the closure, correct arity */
	  fun = reg[arity];
	  HIPE_ASSERT(is_fun(fun));
	  funp = (ErlFunThing*)fun_val(fun);
	  HIPE_ASSERT(funp->num_free <= arity);
	  arity -= funp->num_free;	/* arity == #formals */
	  reg[arity] = fun;
	  ++arity;	/* correct for having added the closure */
	  /* HIPE_ASSERT(p->hipe.u.ncallee == (void(*)(void))funp->native_address); */

	  /* just like a normal call from now on */

	  /* p->hipe.u.ncallee set in beam_emu */
	  if (p->cp == hipe_beam_pc_return) {
	      /* Native called BEAM, which now tailcalls native. */
	      hipe_pop_beam_trap_frame(p);
	      result = hipe_tailcall_to_native(p, arity, reg);
	      break;
	  }
	  DPRINTF("calling %#lx/%u", (long)p->hipe.u.ncallee, arity);
	  result = hipe_call_to_native(p, arity, reg);
	  break;
      }
      case HIPE_MODE_SWITCH_CMD_THROW: {
	  /* BEAM just executed hipe_beam_pc_throw[] */
	  /* Native called BEAM, which now throws an exception back to native. */
	  DPRINTF("beam throws freason %#lx fvalue %#lx", p->freason, p->fvalue);
	  hipe_pop_beam_trap_frame(p);
      do_throw_to_native:
	  p->def_arg_reg[0] = exception_tag[GET_EXC_CLASS(p->freason)];
	  hipe_find_handler(p);
	  result = hipe_throw_to_native(p);
	  break;
      }
      case HIPE_MODE_SWITCH_CMD_RETURN: {
	  /* BEAM just executed hipe_beam_pc_return[] */
	  /* Native called BEAM, which now returns back to native. */
	  /* pop trap frame off estack */
	  hipe_pop_beam_trap_frame(p);
	  p->def_arg_reg[0] = reg[0];
	  result = hipe_return_to_native(p);
	  break;
      }
    do_resume:
      case HIPE_MODE_SWITCH_CMD_RESUME: {
	  /* BEAM just executed hipe_beam_pc_resume[] */
	  /* BEAM called native, which suspended. */
	  if (p->flags & F_TIMO) {
	      /* XXX: The process will immediately execute 'clear_timeout',
		 repeating these two statements. Remove them? */
	      p->flags &= ~F_TIMO;
	      JOIN_MESSAGE(p);
	      p->def_arg_reg[0] = 0;	/* make_small(0)? */
	  } else
	      p->def_arg_reg[0] = 1;	/* make_small(1)? */
	  result = hipe_return_to_native(p);
	  break;
      }
      default:
	erts_exit(ERTS_ERROR_EXIT, "hipe_mode_switch: cmd %#x\r\n", cmd);
    }
 do_return_from_native:
    DPRINTF("result == %#x (%s)", result, code_str(result));
    HIPE_CHECK_PCB(p);
    switch (result) {
      case HIPE_MODE_SWITCH_RES_RETURN: {
	  hipe_return_from_native(p);
	  reg[0] = p->def_arg_reg[0];
	  DPRINTF("returning with r(0) == %#lx", reg[0]);
	  break;
      }
      case HIPE_MODE_SWITCH_RES_THROW: {
	  DPRINTF("native throws freason %#lx fvalue %#lx", p->freason, p->fvalue);
	  hipe_throw_from_native(p);
	  break;
      }
      case HIPE_MODE_SWITCH_RES_TRAP: {
	  /*
	   * Native code called a BIF, which "failed" with a TRAP to BEAM.
	   * Prior to returning, the BIF stored (see BIF_TRAP<N>):

	   * the callee's address in p->i
	   * the callee's parameters in reg[0..2]
	   * the callee's arity in p->arity (for BEAM gc purposes)
	   *
	   * We need to remove the BIF's parameters from the native
	   * stack: to this end hipe_${ARCH}_glue.S stores the BIF's
	   * arity in p->hipe.narity.
	   *
	   * If the BIF emptied the stack (typically hibernate), p->hipe.nstack
	   * is NULL and there is no need to get rid of stacked parameters.
	   */
	  unsigned int i, is_recursive = 0;

          if (p->hipe.nstack != NULL) {
	      ASSERT(p->hipe.nsp != NULL);
	      is_recursive = hipe_trap_from_native_is_recursive(p);
          }
	  else {
	      /* Some architectures (risc) need this re-reset of nsp as the
	       * BIF wrapper do not detect stack change and causes an obsolete
	       * stack pointer to be saved in p->hipe.nsp before return to us.
	       */
	      p->hipe.nsp = NULL;
	  }

	  /* Schedule next process if current process was hibernated or is waiting
	     for messages */
	  if (p->flags & F_HIBERNATE_SCHED) {
	      p->flags &= ~F_HIBERNATE_SCHED;
	      goto do_schedule;
	  }

	  if (!(erts_smp_atomic32_read_acqb(&p->state) & ERTS_PSFLG_ACTIVE)) {
	      for (i = 0; i < p->arity; ++i)
		  p->arg_reg[i] = reg[i]; 	      
	      goto do_schedule;
	  }

	  if (is_recursive)
	      hipe_push_beam_trap_frame(p, reg, p->arity);
	  
	  result = HIPE_MODE_SWITCH_RES_CALL_BEAM;
	  break;
      }
      case HIPE_MODE_SWITCH_RES_CALL_EXPORTED: {
	  /* Native code calls or tailcalls BEAM.
	   *
	   * p->hipe.u.callee_exp is the callee's export entry
	   * p->arity is the callee's arity
	   * p->def_arg_reg[] contains the register parameters
	   * p->hipe.nsp[] contains the stacked parameters
	   */
	  if (hipe_call_from_native_is_recursive(p, reg)) {
	      /* BEAM called native, which now calls BEAM */
	      hipe_push_beam_trap_frame(p, reg, p->arity);
	  }
	  break;
      }
      case HIPE_MODE_SWITCH_RES_CALL_CLOSURE: {
	  /* Native code calls or tailcalls a closure in BEAM
	   *
	   * In native code a call to a closure of arity n looks like
	   * F(A1, ..., AN, Closure),
	   * The BEAM code for a closure expects to get:
	   * F(A1, ..., AN, FV1, ..., FVM, Closure)
	   *  (Where Ai is argument i and FVj is free variable j)
	   *
	   * p->hipe.u.closure contains the closure
	   * p->def_arg_reg[] contains the register parameters
	   * p->hipe.nsp[] contains the stacked parameters
	   */
	  ErlFunThing *closure;
	  unsigned num_free, arity, i, is_recursive;

	  HIPE_ASSERT(is_fun(p->hipe.u.closure));
	  closure = (ErlFunThing*)fun_val(p->hipe.u.closure);
	  num_free = closure->num_free;
	  arity = closure->fe->arity;

	  /* Store the arity in p->arity for the stack popping. */
	  /* Note: we already have the closure so only need to move arity
	     values to reg[]. However, there are arity+1 parameters in the
	     native code state that need to be removed. */
	  p->arity = arity+1; /* +1 for the closure */

	  /* Get parameters, don't do GC just yet. */
	  is_recursive = hipe_call_from_native_is_recursive(p, reg);

	  if ((Sint)closure->fe->address[-1] < 0) {
	      /* Unloaded. Let beam_emu.c:call_fun() deal with it. */
	      result = HIPE_MODE_SWITCH_RES_CALL_CLOSURE;
	  } else {
	      /* The BEAM code is present. Prepare to call it. */

	      /* Append the free vars after the actual parameters. */
	      for (i = 0; i < num_free; ++i)
		  reg[arity+i] = closure->env[i];

	      /* Update arity to reflect the new parameters. */
	      arity += i;

	      /* Make a call to the closure's BEAM code. */
	      p->i = closure->fe->address;

	      /* Change result code to the faster plain CALL type. */
	      result = HIPE_MODE_SWITCH_RES_CALL_BEAM;
	  }
	  /* Append the closure as the last parameter. Don't increment arity. */
	  reg[arity] = p->hipe.u.closure;

	  if (is_recursive) {
	      /* BEAM called native, which now calls BEAM.
		 Need to put a trap-frame on the beam stack.
		 This may cause GC, which is safe now that
		 the arguments, free vars, and most
		 importantly the closure, all are in reg[]. */
	      hipe_push_beam_trap_frame(p, reg, arity+1);
	  }
	  break;
      }
      case HIPE_MODE_SWITCH_RES_SUSPEND: {
	  p->i = hipe_beam_pc_resume;
	  p->arity = 0;
	  goto do_schedule;
      }
      case HIPE_MODE_SWITCH_RES_WAIT:
      case HIPE_MODE_SWITCH_RES_WAIT_TIMEOUT: {
	  /* same semantics, different debug trace messages */
#ifdef ERTS_SMP
	  /* XXX: BEAM has different entries for the locked and unlocked
	     cases. HiPE doesn't, so we must check dynamically. */
	  if (p->hipe_smp.have_receive_locks)
	      p->hipe_smp.have_receive_locks = 0;
	  else
	      erts_smp_proc_lock(p, ERTS_PROC_LOCKS_MSG_RECEIVE);
#endif
	  p->i = hipe_beam_pc_resume;
	  p->arity = 0;
	  erts_smp_atomic32_read_band_relb(&p->state,
					   ~ERTS_PSFLG_ACTIVE);
	  erts_smp_proc_unlock(p, ERTS_PROC_LOCKS_MSG_RECEIVE);
      do_schedule:
	  {
	      struct saved_calls *scb;

	      scb = ERTS_PROC_SET_SUSPENDED_SAVED_CALLS_BUF(p, NULL);
	      if (scb)
		  ERTS_PROC_SET_SAVED_CALLS_BUF(p, scb);

              /* The process may have died while it was executing,
                 if so we return out from native code to the interpreter */
              if (erts_smp_atomic32_read_nob(&p->state) & ERTS_PSFLG_EXITING)
                  p->i = beam_exit;
#ifdef DEBUG
	      ASSERT(p->debug_reds_in == reds_in);
#endif
	      p->flags &= ~F_HIPE_MODE;

	      ERTS_SMP_UNREQ_PROC_MAIN_LOCK(p);
	      p = erts_schedule(NULL, p, reds_in - p->fcalls);
	      ERTS_SMP_REQ_PROC_MAIN_LOCK(p);
	      ASSERT(!(p->flags & F_HIPE_MODE));
#ifdef ERTS_SMP
	      p->hipe_smp.have_receive_locks = 0;
	      reg = p->scheduler_data->x_reg_array;
#endif
	  }
	  {
	      Eterm *argp;
	      int i;
	  
	      argp = p->arg_reg;
	      for (i = p->arity; --i >= 0;)
		  reg[i] = argp[i];
	  }
	  {
	      struct saved_calls *scb;

	      reds_in = p->fcalls;
	      p->def_arg_reg[5] = reds_in;
#ifdef DEBUG
	      p->debug_reds_in = reds_in;
#endif
	      if (p->i == hipe_beam_pc_resume) {
		  p->flags |= F_HIPE_MODE; /* inform bifs where we are comming from... */
		  scb = ERTS_PROC_SET_SAVED_CALLS_BUF(p, NULL);
		  if (scb)
		      ERTS_PROC_SET_SUSPENDED_SAVED_CALLS_BUF(p, scb);
		  p->i = NULL;
		  p->arity = 0;
		  goto do_resume;
	      }

	      scb = ERTS_PROC_GET_SAVED_CALLS_BUF(p);
	      if (!scb)
		  p->def_arg_reg[4] = 0;
	      else {
		  p->def_arg_reg[4] = CONTEXT_REDS;
		  p->fcalls = -CONTEXT_REDS + reds_in;
	      }
	  }

	  HIPE_CHECK_PCB(p);
	  result = HIPE_MODE_SWITCH_RES_CALL_BEAM;
	  p->def_arg_reg[3] = result;
	  return p;
      }
      case HIPE_MODE_SWITCH_RES_APPLY: {
	  Eterm mfa[3], args;
	  unsigned int arity;
	  void *address;

	  hipe_pop_params(p, 3, &mfa[0]);

	  /* Unroll the arglist onto reg[]. */
	  args = mfa[2];
	  arity = 0;
	  while (is_list(args)) {
	      if (arity < 255) {
		  reg[arity++] = CAR(list_val(args));
		  args = CDR(list_val(args));
	      } else
		  goto do_apply_fail;
	  }
	  if (is_not_nil(args))
	      goto do_apply_fail;

	  /* find a native code entry point for {M,F,A} for a remote call */
	  address = hipe_get_remote_na(mfa[0], mfa[1], arity);
	  if (!address)
		  goto do_apply_fail;
	  p->hipe.u.ncallee = (void(*)(void)) address;
	  result = hipe_tailcall_to_native(p, arity, reg);
	  goto do_return_from_native;
      do_apply_fail:
	  p->freason = BADARG;
	  goto do_throw_to_native;
      }
      default:
	erts_exit(ERTS_ERROR_EXIT, "hipe_mode_switch: result %#x\r\n", result);
    }

    {
	struct saved_calls *scb = ERTS_PROC_SET_SUSPENDED_SAVED_CALLS_BUF(p, NULL);
	if (!scb)
	    p->def_arg_reg[4] = 0;
	else {
	    p->def_arg_reg[4] = CONTEXT_REDS;
	    p->fcalls -= CONTEXT_REDS;
	    ERTS_PROC_SET_SAVED_CALLS_BUF(p, scb);
	}
    }

    HIPE_CHECK_PCB(p);
    p->def_arg_reg[3] = result;
#if NR_ARG_REGS > 5
    /*
     * When NR_ARG_REGS > 5, we need to protect the process' input
     * reduction count (which BEAM stores in def_arg_reg[5]) from
     * being clobbered by the arch glue code.
     */
    p->def_arg_reg[5] = reds_in;
#endif
    p->flags &= ~F_HIPE_MODE;
    return p;
}

#define HIPE_INITIAL_NSTACK_SIZE	128

/* PRE: size is zero or a power of two */
static unsigned hipe_next_nstack_size(unsigned size)
{
    return size ? size * 2 : HIPE_INITIAL_NSTACK_SIZE;
}

#if 0 && defined(HIPE_NSTACK_GROWS_UP)
void hipe_inc_nstack(Process *p)
{
    Eterm *old_nstack = p->hipe.nstack;
    unsigned old_size = p->hipe.nstend - old_nstack;
    unsigned new_size = hipe_next_nstack_size(old_size);
    Eterm *new_nstack = erts_realloc(ERTS_ALC_T_HIPE,
				     (char *) old_nstack,
				     new_size*sizeof(Eterm));
    p->hipe.nstend = new_nstack + new_size;
    if (new_nstack != old_nstack) {
	p->hipe.nsp = new_nstack + (p->hipe.nsp - old_nstack);
	p->hipe.nstack = new_nstack;
	if (p->hipe.nstgraylim)
	    p->hipe.nstgraylim = 
		new_nstack + (p->hipe.nstgraylim - old_nstack);
	if (p->hipe.nstblacklim)
	    p->hipe.nstblacklim = 
		new_nstack + (p->hipe.nstblacklim - old_nstack);
    }
}
#endif

#if defined(HIPE_NSTACK_GROWS_DOWN)
void hipe_inc_nstack(Process *p)
{
    unsigned old_size = p->hipe.nstend - p->hipe.nstack;
    unsigned new_size = hipe_next_nstack_size(old_size);
    Eterm *new_nstack = erts_alloc(ERTS_ALC_T_HIPE, new_size*sizeof(Eterm));
    unsigned used_size = p->hipe.nstend - p->hipe.nsp;

    sys_memcpy(new_nstack+new_size-used_size, p->hipe.nsp, used_size*sizeof(Eterm));
    if (p->hipe.nstgraylim)
	p->hipe.nstgraylim = new_nstack + new_size - (p->hipe.nstend - p->hipe.nstgraylim);
    if (p->hipe.nstblacklim)
	p->hipe.nstblacklim = new_nstack + new_size - (p->hipe.nstend - p->hipe.nstblacklim);
    if (p->hipe.nstack)
	erts_free(ERTS_ALC_T_HIPE, p->hipe.nstack);
    p->hipe.nstack = new_nstack;
    p->hipe.nstend = new_nstack + new_size;
    p->hipe.nsp = new_nstack + new_size - used_size;
}
#endif

void hipe_empty_nstack(Process *p)
{
    if (p->hipe.nstack) {
	erts_free(ERTS_ALC_T_HIPE, p->hipe.nstack);
    }
    p->hipe.nstgraylim = NULL;
    p->hipe.nsp = NULL;
    p->hipe.nstack = NULL;
    p->hipe.nstend = NULL;
}

void hipe_set_closure_stub(ErlFunEntry *fe, unsigned num_free)
{
    unsigned arity;

    arity = fe->arity;
    fe->native_address = (Eterm*) hipe_closure_stub_address(arity);
}

Eterm hipe_build_stacktrace(Process *p, struct StackTrace *s)
{
    int depth, i;
    Uint heap_size;
    Eterm *hp, *hp_end, mfa, m, f, head, *next_p, next;
    const void *ra;
    unsigned int a;

    depth = s->depth;
    if (depth < 1)
	return NIL;

    heap_size = 7 * depth;	/* each [{M,F,A,[]}|_] is 2+5 == 7 words */
    hp = HAlloc(p, heap_size);
    hp_end = hp + heap_size;

    head = NIL;
    next_p = &head;

    for (i = 0; i < depth; ++i) {
	ra = (const void*)s->trace[i];
	if (!hipe_find_mfa_from_ra(ra, &m, &f, &a))
	    continue;
	mfa = TUPLE4(hp, m, f, make_small(a), NIL);
	hp += 5;
	next = CONS(hp, mfa, NIL);
	*next_p = next;
	next_p = &CDR(list_val(next));
	hp += 2;
    }
    HRelease(p, hp_end, hp);
    return head;
}
