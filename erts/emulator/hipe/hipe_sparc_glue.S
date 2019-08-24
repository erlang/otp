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

#define ASM
#include "hipe_sparc_asm.h"
#include "hipe_literals.h"
#include "hipe_mode_switch.h"

	.section ".text"
	.align	4

/*
 * Enter Erlang from C.
 * Switch to a new register window.
 * Create a new frame on the C stack.
 * Save C return address in the frame.
 * Retrieve the process pointer from the C argument registers.
 */
#define ENTER_FROM_C		\
	save	%sp, -112, %sp;	\
	st	%i7, [%sp+96]

/*
 * Return to the calling C function.
 * The return value is in %o0.
 *
 * .flush_exit saves NSP and other cached P state.
 * .suspend_exit also saves RA.
 */
.suspend_exit:
	/* save RA, so we can be resumed */
	st	RA, [P+P_NRA]
.flush_exit:
	/* restore C return address (hoisted to avoid stall) */
	ld	[%sp+96], %i7
	/* flush cached P state */
	SAVE_CACHED_STATE
	/* restore callee-save registers, drop frame, return */
	jmp	%i7+8		/* ret */
	restore	%g0, %o0, %o0	/* kills P, moves our %o0 to caller's %o0 */

/*
 * int hipe_sparc_call_to_native(Process *p);
 * Emulated code recursively calls native code.
 */
	.global	hipe_sparc_call_to_native
	.type	hipe_sparc_call_to_native, #function
	.proc	04		/* ??? */
hipe_sparc_call_to_native:
	ENTER_FROM_C
	/* prepare to call the target */
	ld	[P+P_NCALLEE], TEMP_ARG0
	/* get argument registers */
	LOAD_ARG_REGS
	/* cache some P state in registers */
	RESTORE_CACHED_STATE
/* FALLTHROUGH
 *
 * We export this return address so that hipe_mode_switch() can discover
 * when native code tailcalls emulated code.
 * Note: this is SPARC, so the value in the return address register
 * is the address of the call/jmpl instruction itself.
 */
	.global nbif_return
nbif_return:
	/* call the target */
	jmpl	TEMP_ARG0, RA
	nop
/* FALLTHROUGH
 *
 * This is where native code returns to emulated code.
 */
	st	%o0, [P+P_ARG0]		/* save retval */
	ba	.flush_exit
	mov	HIPE_MODE_SWITCH_RES_RETURN, %o0

/*
 * int hipe_sparc_return_to_native(Process *p);
 * Emulated code returns to its native code caller.
 */
	.global	hipe_sparc_return_to_native
	.type	hipe_sparc_return_to_native, #function
	.proc	04		/* ??? */
hipe_sparc_return_to_native:
	ENTER_FROM_C
	/* restore return address */
	ld	[P+P_NRA], RA
	/* cache some P state in registers */
	RESTORE_CACHED_STATE
	/*
	 * Return using the current return address.
	 * The parameters were popped at the original native-to-emulated
	 * call (hipe_call_from_native_is_recursive), so a plain ret suffices.
	 */
	jmp	RA+8
	ld	[P+P_ARG0], %o0		/* delay slot: get return value */

/*
 * int hipe_sparc_tailcall_to_native(Process *);
 * Emulated code tailcalls native code.
 */
	.global	hipe_sparc_tailcall_to_native
	.type	hipe_sparc_tailcall_to_native, #function
	.proc	04		/* ??? */
hipe_sparc_tailcall_to_native:
	ENTER_FROM_C
	/* prepare to call the target */
	ld	[P+P_NCALLEE], TEMP_ARG0
	/* get argument registers */
	LOAD_ARG_REGS
	/* cache some P state in registers */
	RESTORE_CACHED_STATE
	/* call the target */
	jmp	TEMP_ARG0
	ld	[P+P_NRA], RA	/* delay slot: restore return address */

/*
 * int hipe_sparc_throw_to_native(Process *p);
 * Emulated code throws an exception to its native code caller.
 */
	.align	4
	.global	hipe_sparc_throw_to_native
	.type	hipe_sparc_throw_to_native, #function
	.proc	04		/* ??? */
hipe_sparc_throw_to_native:
	ENTER_FROM_C
	/* prepare to invoke handler */
	ld	[P+P_NCALLEE], TEMP_ARG0	/* set by hipe_find_handler() */
	/* cache some P state in registers */
	RESTORE_CACHED_STATE
	/* invoke the handler */
	jmp	TEMP_ARG0
	nop

/*
 * Native code calls emulated code via a stub
 * which should look as follows:
 *
 * stub for f/N:
 *	sethi %hi(f's export entry address), TEMP_ARG0
 *	mov RA, TEMP_RA		! because the call below clobbers RA (%o7)
 *	or TEMP_ARG0, %lo(f's export entry address), TEMP_ARG0
 *	call nbif_callemu	! clobbers RA!
 *	mov N, TEMP_ARG1	! delay slot: TEMP_ARG1 := ARITY
 *
 * XXX. Different stubs for different number of register parameters?
 */
	.global nbif_callemu
nbif_callemu:
	st	TEMP_ARG0, [P+P_CALLEE_EXP]
	st	TEMP_ARG1, [P+P_ARITY]
	st	TEMP_RA, [P+P_NRA]
	STORE_ARG_REGS
	ba	.flush_exit
	mov	HIPE_MODE_SWITCH_RES_CALL_EXPORTED, %o0

/*
 * nbif_apply
 */
	.global	nbif_apply
nbif_apply:
	STORE_ARG_REGS
	ba	.suspend_exit
	mov	HIPE_MODE_SWITCH_RES_APPLY, %o0

/*
 * Native code calls an emulated-mode closure via a stub defined below.
 *
 * The closure is appended as the last actual parameter, and parameters
 * beyond the first few passed in registers are pushed onto the stack in
 * left-to-right order.
 * Hence, the location of the closure parameter only depends on the number
 * of parameters in registers, not the total number of parameters.
 */
#if NR_ARG_REGS >= 6
	.global	nbif_ccallemu6
nbif_ccallemu6:
	st	ARG5, [P+P_ARG5]
#if NR_ARG_REGS > 6
	mov	ARG6, ARG5
#else
	ld	[NSP+0], ARG5
#endif
	/*FALLTHROUGH*/
#endif

#if NR_ARG_REGS >= 5
	.global	nbif_ccallemu5
nbif_ccallemu5:
	st	ARG4, [P+P_ARG4]
#if NR_ARG_REGS > 5
	mov	ARG5, ARG4
#else
	ld	[NSP+0], ARG4
#endif
	/*FALLTHROUGH*/
#endif

#if NR_ARG_REGS >= 4
	.global	nbif_ccallemu4
nbif_ccallemu4:
	st	ARG3, [P+P_ARG3]
#if NR_ARG_REGS > 4
	mov	ARG4, ARG3
#else
	ld	[NSP+0], ARG3
#endif
	/*FALLTHROUGH*/
#endif

#if NR_ARG_REGS >= 3
	.global	nbif_ccallemu3
nbif_ccallemu3:
	st	ARG2, [P+P_ARG2]
#if NR_ARG_REGS > 3
	mov	ARG3, ARG2
#else
	ld	[NSP+0], ARG2
#endif
	/*FALLTHROUGH*/
#endif

#if NR_ARG_REGS >= 2
	.global	nbif_ccallemu2
nbif_ccallemu2:
	st	ARG1, [P+P_ARG1]
#if NR_ARG_REGS > 2
	mov	ARG2, ARG1
#else
	ld	[NSP+0], ARG1
#endif
	/*FALLTHROUGH*/
#endif

#if NR_ARG_REGS >= 1
	.global	nbif_ccallemu1
nbif_ccallemu1:
	st	ARG0, [P+P_ARG0]
#if NR_ARG_REGS > 1
	mov	ARG1, ARG0
#else
	ld	[NSP+0], ARG0
#endif
	/*FALLTHROUGH*/
#endif

	.global	nbif_ccallemu0
nbif_ccallemu0:
	/* We use %o1 not ARG0 here because ARG0 is not
	   defined when NR_ARG_REGS == 0. */
#if NR_ARG_REGS == 0
	ld	[NSP+0], %o1		/* get the closure */
#endif
	st	%o1, [P+P_CLOSURE]	/* save the closure */
	ba	.suspend_exit
	mov	HIPE_MODE_SWITCH_RES_CALL_CLOSURE, %o0

/*
 * This is where native code suspends.
 */
	.global nbif_suspend_0
nbif_suspend_0:
	ba	.suspend_exit
	mov	HIPE_MODE_SWITCH_RES_SUSPEND, %o0

/*
 * Suspend from a receive (waiting for a message)
 */
	.global nbif_suspend_msg
nbif_suspend_msg:
	ba	.suspend_exit
	mov	HIPE_MODE_SWITCH_RES_WAIT, %o0

/*
 * Suspend from a receive with a timeout (waiting for a message)
 *	if (!(p->flags & F_TIMO)) { suspend }
 *	else { return 0; }
 */
	.global nbif_suspend_msg_timeout
nbif_suspend_msg_timeout:
	ld	[P+P_FLAGS], %o1
	/* this relies on F_TIMO (1<<2) fitting in a simm13 */
	andcc	%o1, F_TIMO, %g0
	bz,a	.suspend_exit
	mov	HIPE_MODE_SWITCH_RES_WAIT_TIMEOUT, %o0	/* delay slot */
	/* timeout has occurred */
	jmp	RA+8
	mov	0, %o0

/*
 * This is the default exception handler for native code.
 */
	.global	nbif_fail
nbif_fail:
	ba	.flush_exit
	mov	HIPE_MODE_SWITCH_RES_THROW, %o0

	.global	nbif_0_gc_after_bif
	.global	nbif_1_gc_after_bif
	.global	nbif_2_gc_after_bif
	.global	nbif_3_gc_after_bif
	.global nbif_4_gc_after_bif
nbif_0_gc_after_bif:
	ba	.gc_after_bif
	mov	0, %o1		/* delay slot */
nbif_1_gc_after_bif:
	ba	.gc_after_bif
	mov	1, %o1		/* delay slot */
nbif_2_gc_after_bif:
	ba	.gc_after_bif
	mov	2, %o1		/* delay slot */
nbif_3_gc_after_bif:
	ba	.gc_after_bif
	mov	3, %o1		/* delay slot */
nbif_4_gc_after_bif:
	mov	4, %o1
	/*FALLTHROUGH*/
.gc_after_bif:
	st	%o1, [P+P_NARITY]
	st	TEMP_RA, [P+P_NRA]
	st	NSP, [P+P_NSP]
	mov	RA, TEMP_RA
	mov	0, %o3			/* Pass 0 in arity */
	mov	0, %o2			/* Pass NULL in regs */
	mov	%o0, %o1
	call	erts_gc_after_bif_call
	mov	P, %o0			/* delay slot */
	mov	TEMP_RA, RA
	ld	[P+P_NRA], TEMP_RA
	jmp	RA+8
	st	%g0, [P+P_NARITY]	/* delay slot */

/*
 * We end up here when a BIF called from native signals an
 * exceptional condition.
 * HP has not been read from P.
 * NSP has not been saved in P.
 * TEMP_LR contains a copy of LR
 */
	.global	nbif_0_simple_exception
nbif_0_simple_exception:
	ba	.nbif_simple_exception
	mov	0, %o1		/* delay slot */
	.global	nbif_1_simple_exception
nbif_1_simple_exception:
	ba	.nbif_simple_exception
	mov	1, %o1		/* delay slot */
	.global	nbif_2_simple_exception
nbif_2_simple_exception:
	ba	.nbif_simple_exception
	mov	2, %o1		/* delay slot */
	.global	nbif_3_simple_exception
nbif_3_simple_exception:
	ba	.nbif_simple_exception
	mov	3, %o1		/* delay slot */
	.global	nbif_4_simple_exception
nbif_4_simple_exception:
	mov	4, %o1
	/*FALLTHROUGH*/
.nbif_simple_exception:
	ld	[P+P_FREASON], %o0
	cmp	%o0, FREASON_TRAP
	beq	.handle_trap
	nop
	/*
	 * Find and invoke catch handler (it must exist).
	 * HP has not been read from P.
	 * NSP has not been saved in P.
	 * TEMP_RA should contain the current call's return address.
	 * %o1 should contain the current call's arity.
	 */
	st	NSP, [P+P_NSP]
	st	TEMP_RA, [P+P_NRA]
	st	%o1, [P+P_NARITY]
	/* find and prepare to invoke the handler */
	call	hipe_handle_exception	/* Note: hipe_handle_exception() conses */
	mov	P, %o0			/* delay slot */
	/* prepare to invoke the handler */
	ld	[P+P_NCALLEE], %o0	/* set by hipe_find_handler() */
	RESTORE_CACHED_STATE
	/* now invoke the handler */
	jmp	%o0
	nop

	/*
	 * A BIF failed with freason TRAP:
	 * - the BIF's arity is in %o1
	 * - the native RA was saved in TEMP_RA before the BIF call
	 * - HP has not been read from P
	 * - NSP has not been saved in P
	 */
.handle_trap:
	mov	HIPE_MODE_SWITCH_RES_TRAP, %o0
.bif_exit:
	/* restore C return address (hoisted to avoid stall) */
	ld	[%sp+96], %i7
	st	NSP, [P+P_NSP]
	st	%o1, [P+P_NARITY]
	st	TEMP_RA, [P+P_NRA]
	jmp	%i7+8
	restore	%g0, %o0, %o0

/*
 * nbif_stack_trap_ra: trap return address for maintaining
 * the gray/white stack boundary
 */
	.global	nbif_stack_trap_ra
nbif_stack_trap_ra:			/* a return address, not a function */
	nop                             /* ditto */
	nop				/* ditto */
	/* This only handles a single return value.
	   If we have more, we need to save them in the PCB. */
	mov	%o0, TEMP_ARG0		/* save retval */
	st	NSP, [P+P_NSP]
	call hipe_handle_stack_trap	/* must not cons */
	mov	P, %o0			/* delay slot */
	mov	%o0, RA			/* original RA */
	jmp	RA+8			/* resume at original RA */
	mov	TEMP_ARG0, %o0		/* delay slot: restore retval */

/*
 * hipe_sparc_inc_stack
 * Caller saved its RA in TEMP_RA (== TEMP1) before calling us.
 */
	.global	hipe_sparc_inc_stack
hipe_sparc_inc_stack:
	STORE_ARG_REGS
	mov	RA, TEMP_ARG0
	st	NSP, [P+P_NSP]
	/* hipe_inc_nstack reads and writes NSP and NSP_LIMIT,
	   but does not access LR/RA, HP, or FCALLS. */
	call	hipe_inc_nstack
	mov	P, %o0		/* delay slot */
	LOAD_ARG_REGS
	/* this relies on LOAD_ARG_REGS not clobbering TEMP_ARG0 */
	jmp	TEMP_ARG0+8
	ld	[P+P_NSP], NSP	/* delay slot */

#if defined(__linux__) && defined(__ELF__)
.section .note.GNU-stack,"",%progbits
#endif
