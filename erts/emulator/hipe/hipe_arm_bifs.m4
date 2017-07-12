changecom(`/*', `*/')dnl
/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2005-2016. All Rights Reserved.
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


#`define ASM'
include(`hipe/hipe_arm_asm.m4')
#`include' "config.h"
#`include' "hipe_literals.h"

	.text
	.p2align 2
	.arm

`#if defined(ERTS_ENABLE_LOCK_CHECK)
#  define CALL_BIF(F)	ldr r14, =nbif_impl_##F; str r14, [r0, #P_BIF_CALLEE]; bl hipe_debug_bif_wrapper
#else
#  define CALL_BIF(F)	bl	nbif_impl_##F
#endif'

define(TEST_GOT_MBUF,`ldr r1, [P, #P_MBUF]	/* `TEST_GOT_MBUF' */
	cmp r1, #0
	blne nbif_$1_gc_after_bif')

/*
 * standard_bif_interface_1(nbif_name, cbif_name)
 * standard_bif_interface_2(nbif_name, cbif_name)
 * standard_bif_interface_3(nbif_name, cbif_name)
 * standard_bif_interface_4(nbif_name, cbif_name)
 * standard_bif_interface_0(nbif_name, cbif_name)
 *
 * Generate native interface for a BIF with 0-4 parameters and
 * standard failure mode.
 */
define(standard_bif_interface_1,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	.global	$1
$1:
	/* Set up C argument registers. */
	mov	r0, P
	NBIF_ARG(r1,1,0)

	/* Save caller-save registers and call the C function. */
	SAVE_CONTEXT_BIF
	str	r1, [r0, #P_ARG0]	/* Store BIF__ARGS in def_arg_reg[] */
	add	r1, r0, #P_ARG0
	CALL_BIF($2)
	TEST_GOT_MBUF(1)

	/* Restore registers. Check for exception. */
	cmp	r0, #THE_NON_VALUE
	RESTORE_CONTEXT_BIF
	beq	nbif_1_simple_exception
	NBIF_RET(1)
	.ltorg		/* needed by LDR in debug version of `CALL_BIF' */
	.size	$1, .-$1
	.type	$1, %function
#endif')

define(standard_bif_interface_2,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	.global	$1
$1:
	/* Set up C argument registers. */
	mov	r0, P
	NBIF_ARG(r1,2,0)
	NBIF_ARG(r2,2,1)

	/* Save caller-save registers and call the C function. */
	SAVE_CONTEXT_BIF
	str	r1, [r0, #P_ARG0]	/* Store BIF__ARGS in def_arg_reg[] */
	str	r2, [r0, #P_ARG1]
	add	r1, r0, #P_ARG0
	CALL_BIF($2)
	TEST_GOT_MBUF(2)

	/* Restore registers. Check for exception. */
	cmp	r0, #THE_NON_VALUE
	RESTORE_CONTEXT_BIF
	beq	nbif_2_simple_exception
	NBIF_RET(2)
	.ltorg
	.size	$1, .-$1
	.type	$1, %function
#endif')

define(standard_bif_interface_3,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	.global	$1
$1:
	/* Set up C argument registers. */
	mov	r0, P
	NBIF_ARG(r1,3,0)
	NBIF_ARG(r2,3,1)
	NBIF_ARG(r3,3,2)

	/* Save caller-save registers and call the C function. */
	SAVE_CONTEXT_BIF
	str	r1, [r0, #P_ARG0]	/* Store BIF__ARGS in def_arg_reg[] */
	str	r2, [r0, #P_ARG1]
	str	r3, [r0, #P_ARG2]
	add	r1, r0, #P_ARG0
	CALL_BIF($2)
	TEST_GOT_MBUF(3)

	/* Restore registers. Check for exception. */
	cmp	r0, #THE_NON_VALUE
	RESTORE_CONTEXT_BIF
	beq	nbif_3_simple_exception
	NBIF_RET(3)
	.ltorg
	.size	$1, .-$1
	.type	$1, %function
#endif')

define(standard_bif_interface_4,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	.global	$1
$1:
	/* Set up C argument registers. */
	mov	r0, P
	NBIF_ARG(r1,4,0)
	NBIF_ARG(r2,4,1)
	NBIF_ARG(r3,4,2)
	NBIF_ARG(r4,4,3)

	/* Save caller-save registers and call the C function. */
	SAVE_CONTEXT_BIF
	str	r1, [r0, #P_ARG0]	/* Store BIF__ARGS in def_arg_reg[] */
	str	r2, [r0, #P_ARG1]
	str	r3, [r0, #P_ARG2]
	str	r4, [r0, #P_ARG3]
	add	r1, r0, #P_ARG0
	CALL_BIF($2)
	TEST_GOT_MBUF(4)

	/* Restore registers. Check for exception. */
	cmp	r0, #THE_NON_VALUE
	RESTORE_CONTEXT_BIF
	beq	nbif_4_simple_exception
	NBIF_RET(4)
	.ltorg
	.size	$1, .-$1
	.type	$1, %function
#endif')

define(standard_bif_interface_0,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	.global	$1
$1:
	/* Set up C argument registers. */
	mov	r0, P

	/* Save caller-save registers and call the C function. */
	SAVE_CONTEXT_BIF
	/* ignore empty BIF__ARGS */
	CALL_BIF($2)
	TEST_GOT_MBUF(0)

	/* Restore registers. Check for exception. */
	cmp	r0, #THE_NON_VALUE
	RESTORE_CONTEXT_BIF
	beq	nbif_0_simple_exception
	NBIF_RET(0)
	.ltorg
	.size	$1, .-$1
	.type	$1, %function
#endif')

/*
 * gc_bif_interface_0(nbif_name, cbif_name)
 * gc_bif_interface_1(nbif_name, cbif_name)
 * gc_bif_interface_2(nbif_name, cbif_name)
 * gc_bif_interface_3(nbif_name, cbif_name)
 *
 * Generate native interface for a BIF with 0-3 parameters and
 * standard failure mode.
 * The BIF may do a GC.
 */
define(gc_bif_interface_0,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	.global	$1
$1:
	/* Set up C argument registers. */
	mov	r0, P

	/* Save caller-save registers and call the C function. */
	SAVE_CONTEXT_GC
	/* ignore empty BIF__ARGS */
	CALL_BIF($2)
	TEST_GOT_MBUF(0)

	/* Restore registers. */
	RESTORE_CONTEXT_GC
	NBIF_RET(0)
	.size	$1, .-$1
	.type	$1, %function
#endif')

define(gc_bif_interface_1,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	.global	$1
$1:
	/* Set up C argument registers. */
	mov	r0, P
	NBIF_ARG(r1,1,0)

	/* Save caller-save registers and call the C function. */
	SAVE_CONTEXT_GC
        str     r1, [r0, #P_ARG0]       /* Store BIF__ARGS in def_arg_reg[] */
        add     r1, r0, #P_ARG0
	CALL_BIF($2)
	TEST_GOT_MBUF(1)

	/* Restore registers. Check for exception. */
	cmp	r0, #THE_NON_VALUE
	RESTORE_CONTEXT_GC
	beq	nbif_1_simple_exception
	NBIF_RET(1)
	.size	$1, .-$1
	.type	$1, %function
#endif')

define(gc_bif_interface_2,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	.global	$1
$1:
	/* Set up C argument registers. */
	mov	r0, P
	NBIF_ARG(r1,2,0)
	NBIF_ARG(r2,2,1)

	/* Save caller-save registers and call the C function. */
	SAVE_CONTEXT_GC
        str     r1, [r0, #P_ARG0]       /* Store BIF__ARGS in def_arg_reg[] */
        str     r2, [r0, #P_ARG1]
        add     r1, r0, #P_ARG0
	CALL_BIF($2)
	TEST_GOT_MBUF(2)

	/* Restore registers. Check for exception. */
	cmp	r0, #THE_NON_VALUE
	RESTORE_CONTEXT_GC
	beq	nbif_2_simple_exception
	NBIF_RET(2)
	.size	$1, .-$1
	.type	$1, %function
#endif')

define(gc_bif_interface_3,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	.global	$1
$1:
	/* Set up C argument registers. */
	mov	r0, P
	NBIF_ARG(r1,3,0)
	NBIF_ARG(r2,3,1)
	NBIF_ARG(r3,3,2)

	/* Save caller-save registers and call the C function. */
	SAVE_CONTEXT_GC
	str	r1, [r0, #P_ARG0]       /* Store BIF__ARGS in def_arg_reg[] */
	str	r2, [r0, #P_ARG1]
	str	r3, [r0, #P_ARG2]
	add	r1, r0, #P_ARG0
	CALL_BIF($2)
	TEST_GOT_MBUF(3)

	/* Restore registers. Check for exception. */
	cmp	r0, #THE_NON_VALUE
	RESTORE_CONTEXT_GC
	beq	nbif_3_simple_exception
	NBIF_RET(3)
	.size	$1, .-$1
	.type	$1, %function
#endif')

/*
 * gc_nofail_primop_interface_1(nbif_name, cbif_name)
 *
 * Generate native interface for a primop with implicit P
 * parameter, 1 ordinary parameter and no failure mode.
 * The primop may do a GC.
 */
define(gc_nofail_primop_interface_1,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	.global	$1
$1:
	/* Set up C argument registers. */
	mov	r0, P
	NBIF_ARG(r1,1,0)

	/* Save caller-save registers and call the C function. */
	SAVE_CONTEXT_GC
	bl	$2

	/* Restore registers. */
	RESTORE_CONTEXT_GC
	NBIF_RET(1)
	.size	$1, .-$1
	.type	$1, %function
#endif')

/*
 * nofail_primop_interface_0(nbif_name, cbif_name)
 * nofail_primop_interface_1(nbif_name, cbif_name)
 * nofail_primop_interface_2(nbif_name, cbif_name)
 * nofail_primop_interface_3(nbif_name, cbif_name)
 *
 * Generate native interface for a primop with implicit P
 * parameter, 0-3 ordinary parameters and no failure mode.
 * Also used for guard BIFs.
 */
define(nofail_primop_interface_0,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	.global	$1
$1:
	/* Set up C argument registers. */
	mov	r0, P

	/* Save caller-save registers and call the C function. */
	SAVE_CONTEXT_BIF
	bl	$2
	TEST_GOT_MBUF(0)

	/* Restore registers. */
	RESTORE_CONTEXT_BIF
	NBIF_RET(0)
	.size	$1, .-$1
	.type	$1, %function
#endif')

define(nofail_primop_interface_1,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	.global	$1
$1:
	/* Set up C argument registers. */
	mov	r0, P
	NBIF_ARG(r1,1,0)

	/* Save caller-save registers and call the C function. */
	SAVE_CONTEXT_BIF
	bl	$2
	TEST_GOT_MBUF(1)

	/* Restore registers. */
	RESTORE_CONTEXT_BIF
	NBIF_RET(1)
	.size	$1, .-$1
	.type	$1, %function
#endif')

define(nofail_primop_interface_2,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	.global	$1
$1:
	/* Set up C argument registers. */
	mov	r0, P
	NBIF_ARG(r1,2,0)
	NBIF_ARG(r2,2,1)

	/* Save caller-save registers and call the C function. */
	SAVE_CONTEXT_BIF
	bl	$2
	TEST_GOT_MBUF(2)

	/* Restore registers. */
	RESTORE_CONTEXT_BIF
	NBIF_RET(2)
	.size	$1, .-$1
	.type	$1, %function
#endif')

define(nofail_primop_interface_3,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	.global	$1
$1:
	/* Set up C argument registers. */
	mov	r0, P
	NBIF_ARG(r1,3,0)
	NBIF_ARG(r2,3,1)
	NBIF_ARG(r3,3,2)

	/* Save caller-save registers and call the C function. */
	SAVE_CONTEXT_BIF
	bl	$2
	TEST_GOT_MBUF(3)

	/* Restore registers. */
	RESTORE_CONTEXT_BIF
	NBIF_RET(3)
	.size	$1, .-$1
	.type	$1, %function
#endif')

/*
 * nocons_nofail_primop_interface_0(nbif_name, cbif_name)
 * nocons_nofail_primop_interface_1(nbif_name, cbif_name)
 * nocons_nofail_primop_interface_2(nbif_name, cbif_name)
 * nocons_nofail_primop_interface_3(nbif_name, cbif_name)
 * nocons_nofail_primop_interface_5(nbif_name, cbif_name)
 *
 * Generate native interface for a primop with implicit P
 * parameter, 0-3 or 5 ordinary parameters, and no failure mode.
 * The primop cannot CONS or gc.
 */
define(nocons_nofail_primop_interface_0,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	.global	$1
$1:
	/* Set up C argument registers. */
	mov	r0, P

	/* Perform a quick save;call;restore;ret sequence. */
#ifdef __thumb__
	SAVE_CONTEXT_QUICK
	bl	$2
	RESTORE_CONTEXT_QUICK
	NBIF_RET(0)
#else
	QUICK_CALL_RET($2,0)
#endif
	.size	$1, .-$1
	.type	$1, %function
#endif')

define(nocons_nofail_primop_interface_1,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	.global	$1
$1:
	/* Set up C argument registers. */
	mov	r0, P
	NBIF_ARG(r1,1,0)

	/* Perform a quick save;call;restore;ret sequence. */
#ifdef __thumb__
	SAVE_CONTEXT_QUICK
	bl	$2
	RESTORE_CONTEXT_QUICK
	NBIF_RET(1)
#else
	QUICK_CALL_RET($2,1)
#endif
	.size	$1, .-$1
	.type	$1, %function
#endif')

define(nocons_nofail_primop_interface_2,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	.global	$1
$1:
	/* Set up C argument registers. */
	mov	r0, P
	NBIF_ARG(r1,2,0)
	NBIF_ARG(r2,2,1)

	/* Perform a quick save;call;restore;ret sequence. */
#ifdef __thumb__
	SAVE_CONTEXT_QUICK
	bl	$2
	RESTORE_CONTEXT_QUICK
	NBIF_RET(2)
#else
	QUICK_CALL_RET($2,2)
#endif
	.size	$1, .-$1
	.type	$1, %function
#endif')

define(nocons_nofail_primop_interface_3,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	.global	$1
$1:
	/* Set up C argument registers. */
	mov	r0, P
	NBIF_ARG(r1,3,0)
	NBIF_ARG(r2,3,1)
	NBIF_ARG(r3,3,2)

	/* Perform a quick save;call;restore;ret sequence. */
#ifdef __thumb__
	SAVE_CONTEXT_QUICK
	bl	$2
	RESTORE_CONTEXT_QUICK
	NBIF_RET(3)
#else
	QUICK_CALL_RET($2,3)
#endif
	.size	$1, .-$1
	.type	$1, %function
#endif')

define(nocons_nofail_primop_interface_5,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	.global	$1
$1:
	/* Set up C argument stack. */
	NBIF_ARG(r0,5,3)
	str	r0, [sp, #0]
	NBIF_ARG(r0,5,4)
	str	r0, [sp, #4]

	/* Set up C argument registers. */
	mov	r0, P
	NBIF_ARG(r1,5,0)
	NBIF_ARG(r2,5,1)
	NBIF_ARG(r3,5,2)

	/* Perform a quick save;call;restore;ret sequence. */
#ifdef __thumb__
	SAVE_CONTEXT_QUICK
	bl	$2
	RESTORE_CONTEXT_QUICK
	NBIF_RET(5)
#else
	QUICK_CALL_RET($2,5)
#endif
	.size	$1, .-$1
	.type	$1, %function
#endif')

/*
 * noproc_primop_interface_0(nbif_name, cbif_name)
 * noproc_primop_interface_1(nbif_name, cbif_name)
 * noproc_primop_interface_2(nbif_name, cbif_name)
 * noproc_primop_interface_3(nbif_name, cbif_name)
 * noproc_primop_interface_5(nbif_name, cbif_name)
 *
 * Generate native interface for a primop with no implicit P
 * parameter, 0-3 or 5 ordinary parameters, and no failure mode.
 * The primop cannot CONS or gc.
 */
define(noproc_primop_interface_0,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	.global	$1
$1:
	/* Perform a quick save;call;restore;ret sequence. */
#ifdef __thumb__
	SAVE_CONTEXT_QUICK
	bl	$2
	RESTORE_CONTEXT_QUICK
	NBIF_RET(0)
#else
	/* XXX: this case is always trivial; how to suppress the branch? */
	QUICK_CALL_RET($2,0)
#endif
	.size	$1, .-$1
	.type	$1, %function
#endif')

define(noproc_primop_interface_1,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	.global	$1
$1:
	/* Set up C argument registers. */
	NBIF_ARG(r0,1,0)

	/* Perform a quick save;call;restore;ret sequence. */
#ifdef __thumb__
	SAVE_CONTEXT_QUICK
	bl	$2
	RESTORE_CONTEXT_QUICK
	NBIF_RET(1)
#else
	QUICK_CALL_RET($2,1)
#endif
	.size	$1, .-$1
	.type	$1, %function
#endif')

define(noproc_primop_interface_2,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	.global	$1
$1:
	/* Set up C argument registers. */
	NBIF_ARG(r0,2,0)
	NBIF_ARG(r1,2,1)

	/* Perform a quick save;call;restore;ret sequence. */
#ifdef __thumb__
	SAVE_CONTEXT_QUICK
	bl	$2
	RESTORE_CONTEXT_QUICK
	NBIF_RET(2)
#else
	QUICK_CALL_RET($2,2)
#endif
	.size	$1, .-$1
	.type	$1, %function
#endif')

define(noproc_primop_interface_3,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	.global	$1
$1:
	/* Set up C argument registers. */
	NBIF_ARG(r0,3,0)
	NBIF_ARG(r1,3,1)
	NBIF_ARG(r2,3,2)

	/* Perform a quick save;call;restore;ret sequence. */
#ifdef __thumb__
	SAVE_CONTEXT_QUICK
	bl	$2
	RESTORE_CONTEXT_QUICK
	NBIF_RET(3)
#else
	QUICK_CALL_RET($2,3)
#endif
	.size	$1, .-$1
	.type	$1, %function
#endif')

define(noproc_primop_interface_5,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	.global	$1
$1:
	/* Set up C argument registers. */
	NBIF_ARG(r0,5,0)
	NBIF_ARG(r1,5,1)
	NBIF_ARG(r2,5,2)
	NBIF_ARG(r3,5,3)
	NBIF_ARG(r4,5,4)
	str	r4, [sp, #0]

	/* Perform a quick save;call;restore;ret sequence. */
#ifdef __thumb__
	SAVE_CONTEXT_QUICK
	bl	$2
	RESTORE_CONTEXT_QUICK
	NBIF_RET(5)
#else
	QUICK_CALL_RET($2,5)
#endif
	.size	$1, .-$1
	.type	$1, %function
#endif')

include(`hipe/hipe_bif_list.m4')

`#if defined(__linux__) && defined(__ELF__)
.section .note.GNU-stack,"",%progbits
#endif'
