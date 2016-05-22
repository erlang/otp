/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2004-2016. All Rights Reserved.
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
#include "hipe_ppc_asm.h"
#include "hipe_literals.h"
#include "hipe_mode_switch.h"

	.text
	.p2align 2

#if defined(__powerpc64__)
/*
 * Enter Erlang from C.
 * Create a new frame on the C stack.
 * Save C callee-save registers (r14-r31) in the frame.
 * Save r0 (C return address) in the caller's LR save slot.
 * Retrieve the process pointer from the C argument registers.
 * Return to LR.
 * Do not clobber the C argument registers (r3-r10).
 *
 * Usage: mflr r0 SEMI bl .enter
 */
.enter:
	# Our PPC64 ELF ABI frame must include:
	# - 48 (6*8) bytes for AIX-like linkage area
	# - 64 (8*8) bytes for AIX-like parameter area for
	#   recursive C calls with up to 8 parameter words
	# - padding to make the frame a multiple of 16 bytes
	# - 144 (18*8) bytes for saving r14-r31
	# The final size is 256 bytes.
	# stdu is required for atomic alloc+init
	stdu	r1,-256(r1)	/* 0(r1) contains r1+256 */
	std	r14, 112(r1)
	std	r15, 120(r1)
	std	r16, 128(r1)
	std	r17, 136(r1)
	std	r18, 144(r1)
	std	r19, 152(r1)
	std	r20, 160(r1)
	std	r21, 168(r1)
	std	r22, 176(r1)
	std	r23, 184(r1)
	std	r24, 192(r1)
	std	r25, 200(r1)
	std	r26, 208(r1)
	std	r27, 216(r1)
	std	r28, 224(r1)
	std	r29, 232(r1)
	std	r30, 240(r1)
	std	r31, 248(r1)
	std	r0,  256+16(r1)	/* caller saved LR in r0 */
	mr	P, r3		/* get the process pointer */
	blr

/*
 * Return to the calling C function.
 * The return value is in r3.
 *
 * .nosave_exit saves no state
 * .flush_exit saves NSP and other cached P state.
 * .suspend_exit also saves RA.
 */
.suspend_exit:
	/* save RA, so we can be resumed */
	mflr	r0
	std	r0, P_NRA(P)
.flush_exit:
	/* flush cached P state */
	SAVE_CACHED_STATE
.nosave_exit:
	/* restore callee-save registers, drop frame, return */
	ld	r0, 256+16(r1)
	mtlr	r0
	ld	r14, 112(r1)
	ld	r15, 120(r1)
	ld	r16, 128(r1)
	ld	r17, 136(r1)
	ld	r18, 144(r1)
	ld	r19, 152(r1)
	ld	r20, 160(r1)
	ld	r21, 168(r1)
	ld	r22, 176(r1)
	ld	r23, 184(r1)
	ld	r24, 192(r1)
	ld	r25, 200(r1)
	ld	r26, 208(r1)
	ld	r27, 216(r1)
	ld	r28, 224(r1)
	ld	r29, 232(r1)	/* kills HP */
	ld	r30, 240(r1)	/* kills NSP */
	ld	r31, 248(r1)	/* kills P */
	addi	r1, r1, 256
	blr
#else /* !__powerpc64__ */
/*
 * Enter Erlang from C.
 * Create a new frame on the C stack.
 * Save C callee-save registers (r14-r31) in the frame.
 * Save r0 (C return address) in the frame's LR save slot.
 * Retrieve the process pointer from the C argument registers.
 * Return to LR.
 * Do not clobber the C argument registers (r3-r10).
 *
 * Usage: mflr r0 SEMI bl .enter
 */
.enter:
	# A unified Linux/OSX C frame must include:
	# - 24 bytes for AIX/OSX-like linkage area
	# - 28 bytes for AIX/OSX-like parameter area for
	#   recursive C calls with up to 7 parameter words
	# - 76 bytes for saving r14-r31 and LR
	# - padding to make it a multiple of 16 bytes
	# The final size is 128 bytes.
	# stwu is required for atomic alloc+init
	stwu	r1,-128(r1)	/* 0(r1) contains r1+128 */
	stw	r14, 52(r1)
	stw	r15, 56(r1)
	stw	r16, 60(r1)
	stw	r17, 64(r1)
	stw	r18, 68(r1)
	stw	r19, 72(r1)
	stw	r20, 76(r1)
	stw	r21, 80(r1)
	stw	r22, 84(r1)
	stw	r23, 88(r1)
	stw	r24, 92(r1)
	stw	r25, 96(r1)
	stw	r26, 100(r1)
	stw	r27, 104(r1)
	stw	r28, 108(r1)
	stw	r29, 112(r1)
	stw	r30, 116(r1)
	stw	r31, 120(r1)
	stw	r0,  124(r1)	/* caller saved LR in r0 */
	mr	P, r3		/* get the process pointer */
	blr

/*
 * Return to the calling C function.
 * The return value is in r3.
 *
 * .nosave_exit saves no state
 * .flush_exit saves NSP and other cached P state.
 * .suspend_exit also saves RA.
 */
.suspend_exit:
	/* save RA, so we can be resumed */
	mflr	r0
	stw	r0, P_NRA(P)
.flush_exit:
	/* flush cached P state */
	SAVE_CACHED_STATE
.nosave_exit:
	/* restore callee-save registers, drop frame, return */
	lwz	r0, 124(r1)
	mtlr	r0
	lwz	r14, 52(r1)
	lwz	r15, 56(r1)
	lwz	r16, 60(r1)
	lwz	r17, 64(r1)
	lwz	r18, 68(r1)
	lwz	r19, 72(r1)
	lwz	r20, 76(r1)
	lwz	r21, 80(r1)
	lwz	r22, 84(r1)
	lwz	r23, 88(r1)
	lwz	r24, 92(r1)
	lwz	r25, 96(r1)
	lwz	r26, 100(r1)
	lwz	r27, 104(r1)
	lwz	r28, 108(r1)
	lwz	r29, 112(r1)	/* kills HP */
	lwz	r30, 116(r1)	/* kills NSP */
	lwz	r31, 120(r1)	/* kills P */
	addi	r1, r1, 128
	blr
#endif /* !__powerpc64__ */

/*
 * int hipe_ppc_call_to_native(Process *p);
 * Emulated code recursively calls native code.
 */
	OPD(hipe_ppc_call_to_native)
	GLOBAL(CSYM(hipe_ppc_call_to_native))
CSYM(hipe_ppc_call_to_native):
	/* save C context */
	mflr	r0
	bl	.enter
	/* prepare to call the target */
	LOAD	r0, P_NCALLEE(P)
	mtctr	r0
	/* get argument registers */
	LOAD_ARG_REGS
	/* cache some P state in registers */
	RESTORE_CACHED_STATE
	/* call the target */
	bctrl	/* defines LR (a.k.a. NRA) */
/* FALLTHROUGH
 *
 * We export this return address so that hipe_mode_switch() can discover
 * when native code tailcalls emulated code.
 *
 * This is where native code returns to emulated code.
 */
	GLOBAL(ASYM(nbif_return))
ASYM(nbif_return):
	STORE	r3, P_ARG0(P)			/* save retval */
	li	r3, HIPE_MODE_SWITCH_RES_RETURN
	b	.flush_exit

/*
 * int hipe_ppc_return_to_native(Process *p);
 * Emulated code returns to its native code caller.
 */
	OPD(hipe_ppc_return_to_native)
	GLOBAL(CSYM(hipe_ppc_return_to_native))
CSYM(hipe_ppc_return_to_native):
	/* save C context */
	mflr	r0
	bl	.enter
	/* restore return address */
	LOAD	r0, P_NRA(P)
	mtlr	r0
	/* cache some P state in registers */
	RESTORE_CACHED_STATE
	/* get return value */
	LOAD	r3, P_ARG0(P)
	/*
	 * Return using the current return address.
	 * The parameters were popped at the original native-to-emulated
	 * call (hipe_call_from_native_is_recursive), so a plain ret suffices.
	 */
	blr

/*
 * int hipe_ppc_tailcall_to_native(Process *p);
 * Emulated code tailcalls native code.
 */
	OPD(hipe_ppc_tailcall_to_native)
	GLOBAL(CSYM(hipe_ppc_tailcall_to_native))
CSYM(hipe_ppc_tailcall_to_native):
	/* save C context */
	mflr	r0
	bl	.enter
	/* prepare to call the target */
	LOAD	r0, P_NCALLEE(P)
	mtctr	r0
	/* get argument registers */
	LOAD_ARG_REGS
	/* restore return address */
	LOAD	r0, P_NRA(P)
	mtlr	r0
	/* cache some P state in registers */
	RESTORE_CACHED_STATE
	/* call the target */
	bctr

/*
 * int hipe_ppc_throw_to_native(Process *p);
 * Emulated code throws an exception to its native code caller.
 */
	OPD(hipe_ppc_throw_to_native)
	GLOBAL(CSYM(hipe_ppc_throw_to_native))
CSYM(hipe_ppc_throw_to_native):
	/* save C context */
	mflr	r0
	bl	.enter
	/* prepare to invoke handler */
	LOAD	r0, P_NCALLEE(P)	/* set by hipe_find_handler() */
	mtctr	r0
	/* cache some P state in registers */
	RESTORE_CACHED_STATE
	/* invoke the handler */
	bctr

/*
 * Native code calls emulated code via a stub
 * which should look as follows:
 *
 * stub for f/N:
 *	<set r12 to f's export entry address>
 *	<set r0 to N>
 *	b nbif_callemu
 *
 * The stub may need to create &nbif_callemu as a 32-bit immediate
 * in a scratch register if the branch needs a trampoline. The code
 * for creating a 32-bit immediate in r0 is potentially slower than
 * for other registers (an add must be replaced by an or, and adds
 * are potentially faster than ors), so it is better to use r0 for
 * the arity (a small immediate), making r11 available for trampolines.
 * (See "The PowerPC Compiler Writer's Guide, section 3.2.3.1.)
 *
 * XXX: Different stubs for different number of register parameters?
 */
	GLOBAL(ASYM(nbif_callemu))
ASYM(nbif_callemu):
	STORE	r12, P_CALLEE_EXP(P)
	STORE	r0, P_ARITY(P)
	STORE_ARG_REGS
	li	r3, HIPE_MODE_SWITCH_RES_CALL_EXPORTED
	b	.suspend_exit

/*
 * nbif_apply
 */
	GLOBAL(ASYM(nbif_apply))
ASYM(nbif_apply):
	STORE_ARG_REGS
	li	r3, HIPE_MODE_SWITCH_RES_APPLY
	b	.suspend_exit

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
	GLOBAL(ASYM(nbif_ccallemu6))
ASYM(nbif_ccallemu6):
	STORE	ARG5, P_ARG5(P)
#if NR_ARG_REGS > 6
	mr	ARG5, ARG6
#else
	LOAD	ARG5, 0(NSP)
#endif
	/*FALLTHROUGH*/
#endif

#if NR_ARG_REGS >= 5
	GLOBAL(ASYM(nbif_ccallemu5))
ASYM(nbif_ccallemu5):
	STORE	ARG4, P_ARG4(P)
#if NR_ARG_REGS > 5
	mr	ARG4, ARG5
#else
	LOAD	ARG4, 0(NSP)
#endif
	/*FALLTHROUGH*/
#endif

#if NR_ARG_REGS >= 4
	GLOBAL(ASYM(nbif_ccallemu4))
ASYM(nbif_ccallemu4):
	STORE	ARG3, P_ARG3(P)
#if NR_ARG_REGS > 4
	mr	ARG3, ARG4
#else
	LOAD	ARG3, 0(NSP)
#endif
	/*FALLTHROUGH*/
#endif

#if NR_ARG_REGS >= 3
	GLOBAL(ASYM(nbif_ccallemu3))
ASYM(nbif_ccallemu3):
	STORE	ARG2, P_ARG2(P)
#if NR_ARG_REGS > 3
	mr	ARG2, ARG3
#else
	LOAD	ARG2, 0(NSP)
#endif
	/*FALLTHROUGH*/
#endif

#if NR_ARG_REGS >= 2
	GLOBAL(ASYM(nbif_ccallemu2))
ASYM(nbif_ccallemu2):
	STORE	ARG1, P_ARG1(P)
#if NR_ARG_REGS > 2
	mr	ARG1, ARG2
#else
	LOAD	ARG1, 0(NSP)
#endif
	/*FALLTHROUGH*/
#endif

#if NR_ARG_REGS >= 1
	GLOBAL(ASYM(nbif_ccallemu1))
ASYM(nbif_ccallemu1):
	STORE	ARG0, P_ARG0(P)
#if NR_ARG_REGS > 1
	mr	ARG0, ARG1
#else
	LOAD	ARG0, 0(NSP)
#endif
	/*FALLTHROUGH*/
#endif

	GLOBAL(ASYM(nbif_ccallemu0))
ASYM(nbif_ccallemu0):
	/* We use r4 not ARG0 here because ARG0 is not
	   defined when NR_ARG_REGS == 0. */
#if NR_ARG_REGS == 0
	LOAD	r4, 0(NSP)		/* get the closure */
#endif
	STORE	r4, P_CLOSURE(P)	/* save the closure */
	li	r3, HIPE_MODE_SWITCH_RES_CALL_CLOSURE
	b	.suspend_exit

/*
 * This is where native code suspends.
 */
	GLOBAL(ASYM(nbif_suspend_0))
ASYM(nbif_suspend_0):
	li	r3, HIPE_MODE_SWITCH_RES_SUSPEND
	b	.suspend_exit

/*
 * Suspend from a receive (waiting for a message)
 */
	GLOBAL(ASYM(nbif_suspend_msg))
ASYM(nbif_suspend_msg):
	li	r3, HIPE_MODE_SWITCH_RES_WAIT
	b	.suspend_exit

/*
 * Suspend from a receive with a timeout (waiting for a message)
 *	if (!(p->flags & F_TIMO)) { suspend }
 *	else { return 0; }
 */
	GLOBAL(ASYM(nbif_suspend_msg_timeout))
ASYM(nbif_suspend_msg_timeout):
	LOAD	r4, P_FLAGS(P)
	li	r3, HIPE_MODE_SWITCH_RES_WAIT_TIMEOUT
	/* this relies on F_TIMO (1<<2) fitting in a uimm16 */
	andi.	r0, r4, F_TIMO
	beq-	.suspend_exit			/* sees the CR state from andi. above */
	/* timeout has occurred */
	li	r3, 0
	blr

/*
 * This is the default exception handler for native code.
 */
	GLOBAL(ASYM(nbif_fail))
ASYM(nbif_fail):
	li	r3, HIPE_MODE_SWITCH_RES_THROW
	b	.flush_exit	/* no need to save RA */

	OPD(nbif_0_gc_after_bif)
	OPD(nbif_1_gc_after_bif)
	OPD(nbif_2_gc_after_bif)
	OPD(nbif_3_gc_after_bif)
	OPD(nbif_4_gc_after_bif)
	GLOBAL(CSYM(nbif_0_gc_after_bif))
	GLOBAL(CSYM(nbif_1_gc_after_bif))
	GLOBAL(CSYM(nbif_2_gc_after_bif))
	GLOBAL(CSYM(nbif_3_gc_after_bif))
	GLOBAL(CSYM(nbif_4_gc_after_bif))
CSYM(nbif_0_gc_after_bif):
	li	r4, 0
	b	.gc_after_bif
CSYM(nbif_1_gc_after_bif):
	li	r4, 1
	b	.gc_after_bif
CSYM(nbif_2_gc_after_bif):
	li	r4, 2
	b	.gc_after_bif
CSYM(nbif_3_gc_after_bif):
	li	r4, 3
	b	.gc_after_bif
CSYM(nbif_4_gc_after_bif):
	li	r4, 4
	/*FALLTHROUGH*/
.gc_after_bif:
	stw	r4, P_NARITY(P)		/* Note: narity is a 32-bit field */
	STORE	TEMP_LR, P_NRA(P)
	STORE	NSP, P_NSP(P)
	mflr	TEMP_LR
	li	r6, 0			/* Pass 0 in arity */
	li	r5, 0			/* Pass NULL in regs */
	mr	r4, r3
	mr	r3, P
	bl	CSYM(erts_gc_after_bif_call)
	mtlr	TEMP_LR
	LOAD	TEMP_LR, P_NRA(P)
	li	r4, 0
	stw	r4, P_NARITY(P)		/* Note: narity is a 32-bit field */
	blr

/*
 * We end up here when a BIF called from native signals an
 * exceptional condition.
 * The heap pointer was just read from P.
 * TEMP_LR contains a copy of LR
 */
	OPD(nbif_0_simple_exception)
	GLOBAL(CSYM(nbif_0_simple_exception))
CSYM(nbif_0_simple_exception):
	li	r4, 0
	b	.nbif_simple_exception

	OPD(nbif_1_simple_exception)
	GLOBAL(CSYM(nbif_1_simple_exception))
CSYM(nbif_1_simple_exception):
	li	r4, 1
	b	.nbif_simple_exception

	OPD(nbif_2_simple_exception)
	GLOBAL(CSYM(nbif_2_simple_exception))
CSYM(nbif_2_simple_exception):
	li	r4, 2
	b	.nbif_simple_exception

	OPD(nbif_3_simple_exception)
	GLOBAL(CSYM(nbif_3_simple_exception))
CSYM(nbif_3_simple_exception):
	li	r4, 3
	b	.nbif_simple_exception

	OPD(nbif_4_simple_exception)
	GLOBAL(CSYM(nbif_4_simple_exception))
CSYM(nbif_4_simple_exception):
	li	r4, 4
	/*FALLTHROUGH*/
.nbif_simple_exception:
	LOAD	r3, P_FREASON(P)
	CMPI	r3, FREASON_TRAP
	beq-	.handle_trap
	/*
	 * Find and invoke catch handler (it must exist).
	 * The heap pointer was just read from P.
	 * TEMP_LR should contain the current call's return address.
	 * r4 should contain the current call's arity.
	 */
	STORE	NSP, P_NSP(P)
	STORE	TEMP_LR, P_NRA(P)
	stw	r4, P_NARITY(P)	/* Note: narity is a 32-bit field */
	/* find and prepare to invoke the handler */
	mr	r3, P
	bl	CSYM(hipe_handle_exception)	/* Note: hipe_handle_exception() conses */
	/* prepare to invoke handler */
	LOAD	r0, P_NCALLEE(P)	/* set by hipe_find_handler() */
	mtctr	r0
	RESTORE_CACHED_STATE		/* NSP updated by hipe_find_handler() */
	/* now invoke the handler */
	bctr

	/*
	 * A BIF failed with freason TRAP:
	 * - the BIF's arity is in r4
	 * - the native RA was saved in TEMP_LR before the BIF call
	 * - the native heap/stack/reds registers are saved in P
	 */
.handle_trap:
	li	r3, HIPE_MODE_SWITCH_RES_TRAP
	STORE	NSP, P_NSP(P)
	stw	r4, P_NARITY(P)	/* Note: narity is a 32-bit field */
	STORE	TEMP_LR, P_NRA(P)
	b	.nosave_exit

/*
 * nbif_stack_trap_ra: trap return address for maintaining
 * the gray/white stack boundary
 */
	GLOBAL(ASYM(nbif_stack_trap_ra))
ASYM(nbif_stack_trap_ra):		/* a return address, not a function */
	# This only handles a single return value.
	# If we have more, we need to save them in the PCB.
	mr	TEMP_ARG0, r3		/* save retval */
	STORE	NSP, P_NSP(P)
	mr	r3, P
	bl	CSYM(hipe_handle_stack_trap)	/* must not cons */
	mtctr	r3			/* original RA */
	mr	r3, TEMP_ARG0		/* restore retval */
	bctr				/* resume at original RA */

/*
 * hipe_ppc_inc_stack
 * Caller saved its LR in TEMP_LR (== TEMP1) before calling us.
 */
	GLOBAL(ASYM(hipe_ppc_inc_stack))
ASYM(hipe_ppc_inc_stack):
	STORE_ARG_REGS
	mflr	TEMP_ARG0
	STORE	NSP, P_NSP(P)
	mr	r3, P
	# hipe_inc_nstack reads and writes NSP and NSP_LIMIT,
	# but does not access LR/RA, HP, or FCALLS.
	bl	CSYM(hipe_inc_nstack)
	mtlr	TEMP_ARG0
	LOAD	NSP, P_NSP(P)
	LOAD_ARG_REGS
	blr

#if defined(__linux__) && defined(__ELF__)
.section .note.GNU-stack,"",%progbits
#endif
