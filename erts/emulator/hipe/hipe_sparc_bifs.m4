changecom(`/*', `*/')dnl
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


#`define ASM'
include(`hipe/hipe_sparc_asm.m4')
#`include' "config.h"
#`include' "hipe_literals.h"

	.section ".text"
	.align	4

`#if defined(ERTS_ENABLE_LOCK_CHECK) && defined(ERTS_SMP)
#  define CALL_BIF(F)	set F, %o7; st %o7, [%o0+P_BIF_CALLEE]; call hipe_debug_bif_wrapper 
#else
#  define CALL_BIF(F)	call	F
#endif'

/*
 * Test for exception. This macro executes its delay slot.
 */
define(TEST_GOT_EXN,`cmp %o0, THE_NON_VALUE	! `TEST_GOT_EXN'
	bz,pn %icc, nbif_$1_simple_exception')

define(TEST_GOT_MBUF,`ld [P+P_MBUF], %o1	! `TEST_GOT_MBUF'
	cmp %o1, 0
	bne 3f
	nop
2:')
define(HANDLE_GOT_MBUF,`
3:	call nbif_$1_gc_after_bif	! `HANDLE_GOT_MBUF'
	nop
	b 2b
	nop')

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
	.global $1
$1:
	/* Set up C argument registers. */
	mov	P, %o0
	NBIF_ARG(%o1,1,0)

	/* Save caller-save registers and call the C function. */
	SAVE_CONTEXT_BIF
	st 	%o1, [%o0+P_ARG0]	! Store BIF__ARGS in def_arg_reg
	add	%o0, P_ARG0, %o1
	CALL_BIF($2)
	nop
	TEST_GOT_MBUF

	/* Restore registers. Check for exception. */
	TEST_GOT_EXN(1)
	RESTORE_CONTEXT_BIF
	NBIF_RET(1)
	HANDLE_GOT_MBUF(1)
	.size	$1, .-$1
	.type	$1, #function
#endif')

define(standard_bif_interface_2,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	.global $1
$1:
	/* Set up C argument registers. */
	mov	P, %o0
	NBIF_ARG(%o1,2,0)
	NBIF_ARG(%o2,2,1)

	/* Save caller-save registers and call the C function. */
	SAVE_CONTEXT_BIF
	st 	%o1, [%o0+P_ARG0]	! Store BIF__ARGS in def_arg_reg
	st 	%o2, [%o0+P_ARG1]
	add	%o0, P_ARG0, %o1
	CALL_BIF($2)
	nop
	TEST_GOT_MBUF

	/* Restore registers. Check for exception. */
	TEST_GOT_EXN(2)
	RESTORE_CONTEXT_BIF
	NBIF_RET(2)
	HANDLE_GOT_MBUF(2)
	.size	$1, .-$1
	.type	$1, #function
#endif')

define(standard_bif_interface_3,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	.global $1
$1:
	/* Set up C argument registers. */
	mov	P, %o0
	NBIF_ARG(%o1,3,0)
	NBIF_ARG(%o2,3,1)
	NBIF_ARG(%o3,3,2)

	/* Save caller-save registers and call the C function. */
	SAVE_CONTEXT_BIF
	st 	%o1, [%o0+P_ARG0]	! Store BIF__ARGS in def_arg_reg
	st 	%o2, [%o0+P_ARG1]
	st 	%o3, [%o0+P_ARG2]
	add	%o0, P_ARG0, %o1
	CALL_BIF($2)
	nop
	TEST_GOT_MBUF

	/* Restore registers. Check for exception. */
	TEST_GOT_EXN(3)
	RESTORE_CONTEXT_BIF
	NBIF_RET(3)
	HANDLE_GOT_MBUF(3)
	.size	$1, .-$1
	.type	$1, #function
#endif')

define(standard_bif_interface_4,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	.global $1
$1:
	/* Set up C argument registers. */
	mov	P, %o0
	NBIF_ARG(%o1,4,0)
	NBIF_ARG(%o2,4,1)
	NBIF_ARG(%o3,4,2)
	NBIF_ARG(%o4,4,3)

	/* Save caller-save registers and call the C function. */
	SAVE_CONTEXT_BIF
	st 	%o1, [%o0+P_ARG0]	! Store BIF__ARGS in def_arg_reg
	st 	%o2, [%o0+P_ARG1]
	st 	%o3, [%o0+P_ARG2]
	st 	%o4, [%o0+P_ARG3]
	add	%o0, P_ARG0, %o1
	CALL_BIF($2)
	nop
	TEST_GOT_MBUF

	/* Restore registers. Check for exception. */
	TEST_GOT_EXN(4)
	RESTORE_CONTEXT_BIF
	NBIF_RET(4)
	HANDLE_GOT_MBUF(4)
	.size	$1, .-$1
	.type	$1, #function
#endif')

define(standard_bif_interface_0,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	.global $1
$1:
	/* Set up C argument registers. */
	mov	P, %o0

	/* Save caller-save registers and call the C function. */
	SAVE_CONTEXT_BIF
	/* ignore empty BIF__ARGS */
	CALL_BIF($2)
	nop
	TEST_GOT_MBUF

	/* Restore registers. Check for exception. */
	TEST_GOT_EXN(0)
	RESTORE_CONTEXT_BIF
	NBIF_RET(0)
	HANDLE_GOT_MBUF(0)
	.size	$1, .-$1
	.type	$1, #function
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
	.global $1
$1:
	/* Set up C argument registers. */
	mov	P, %o0

	/* Save caller-save registers and call the C function. */
	SAVE_CONTEXT_GC
	/* ignore empty BIF__ARGS */
	CALL_BIF($2)
	nop
	TEST_GOT_MBUF

	/* Restore registers. */
	RESTORE_CONTEXT_GC
	NBIF_RET(0)
	HANDLE_GOT_MBUF(0)
	.size	$1, .-$1
	.type	$1, #function
#endif')

define(gc_bif_interface_1,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	.global $1
$1:
	/* Set up C argument registers. */
	mov	P, %o0
	NBIF_ARG(%o1,1,0)

	/* Save caller-save registers and call the C function. */
	SAVE_CONTEXT_GC
	st 	%o1, [%o0+P_ARG0]	! Store BIF__ARGS in def_arg_reg
	add	%o0, P_ARG0, %o1
	CALL_BIF($2)
	nop
	TEST_GOT_MBUF

	/* Restore registers. Check for exception. */
	TEST_GOT_EXN(1)
	RESTORE_CONTEXT_GC
	NBIF_RET(1)
	HANDLE_GOT_MBUF(1)
	.size	$1, .-$1
	.type	$1, #function
#endif')

define(gc_bif_interface_2,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	.global $1
$1:
	/* Set up C argument registers. */
	mov	P, %o0
	NBIF_ARG(%o1,2,0)
	NBIF_ARG(%o2,2,1)

	/* Save caller-save registers and call the C function. */
	SAVE_CONTEXT_GC
	st 	%o1, [%o0+P_ARG0]	! Store BIF__ARGS in def_arg_reg
	st 	%o2, [%o0+P_ARG1]
	add	%o0, P_ARG0, %o1
	CALL_BIF($2)
	nop
	TEST_GOT_MBUF

	/* Restore registers. Check for exception. */
	TEST_GOT_EXN(2)
	RESTORE_CONTEXT_GC
	NBIF_RET(2)
	HANDLE_GOT_MBUF(2)
	.size	$1, .-$1
	.type	$1, #function
#endif')

define(gc_bif_interface_3,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	.global $1
$1:
	/* Set up C argument registers. */
	mov	P, %o0
	NBIF_ARG(%o1,3,0)
	NBIF_ARG(%o2,3,1)
	NBIF_ARG(%o3,3,2)

	/* Save caller-save registers and call the C function. */
	SAVE_CONTEXT_GC
	st 	%o1, [%o0+P_ARG0]	! Store BIF__ARGS in def_arg_reg
	st 	%o2, [%o0+P_ARG1]
	st 	%o3, [%o0+P_ARG2]
	add	%o0, P_ARG0, %o1
	CALL_BIF($2)
	nop
	TEST_GOT_MBUF

	/* Restore registers. Check for exception. */
	TEST_GOT_EXN(3)
	RESTORE_CONTEXT_GC
	NBIF_RET(3)
	HANDLE_GOT_MBUF(3)
	.size	$1, .-$1
	.type	$1, #function
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
	.global $1
$1:
	/* Set up C argument registers. */
	mov	P, %o0
	NBIF_ARG(%o1,1,0)

	/* Save caller-save registers and call the C function. */
	SAVE_CONTEXT_GC
	call	$2
	nop

	/* Restore register. */
	RESTORE_CONTEXT_GC
	NBIF_RET(1)
	.size	$1, .-$1
	.type	$1, #function
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
	.global $1
$1:
	/* Set up C argument registers. */
	mov	P, %o0

	/* Save caller-save registers and call the C function. */
	SAVE_CONTEXT_BIF
	call	$2
	nop
	TEST_GOT_MBUF

	/* Restore registers. */
	RESTORE_CONTEXT_BIF
	NBIF_RET(0)
	HANDLE_GOT_MBUF(0)
	.size	$1, .-$1
	.type	$1, #function
#endif')

define(nofail_primop_interface_1,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	.global $1
$1:
	/* Set up C argument registers. */
	mov	P, %o0
	NBIF_ARG(%o1,1,0)

	/* Save caller-save registers and call the C function. */
	SAVE_CONTEXT_BIF
	call	$2
	nop
	TEST_GOT_MBUF

	/* Restore registers. */
	RESTORE_CONTEXT_BIF
	NBIF_RET(1)
	HANDLE_GOT_MBUF(1)
	.size	$1, .-$1
	.type	$1, #function
#endif')

define(nofail_primop_interface_2,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	.global $1
$1:
	/* Set up C argument registers. */
	mov	P, %o0
	NBIF_ARG(%o1,2,0)
	NBIF_ARG(%o2,2,1)

	/* Save caller-save registers and call the C function. */
	SAVE_CONTEXT_BIF
	call	$2
	nop
	TEST_GOT_MBUF

	/* Restore registers. */
	RESTORE_CONTEXT_BIF
	NBIF_RET(2)
	HANDLE_GOT_MBUF(2)
	.size	$1, .-$1
	.type	$1, #function
#endif')

define(nofail_primop_interface_3,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	.global $1
$1:
	/* Set up C argument registers. */
	mov	P, %o0
	NBIF_ARG(%o1,3,0)
	NBIF_ARG(%o2,3,1)
	NBIF_ARG(%o3,3,2)

	/* Save caller-save registers and call the C function. */
	SAVE_CONTEXT_BIF
	call	$2
	nop
	TEST_GOT_MBUF

	/* Restore registers. */
	RESTORE_CONTEXT_BIF
	NBIF_RET(3)
	HANDLE_GOT_MBUF(3)
	.size	$1, .-$1
	.type	$1, #function
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
	mov	P, %o0

	/* Perform a quick save;call;restore;ret sequence. */
	QUICK_CALL_RET($2,0)
	nop
	.size	$1, .-$1
	.type	$1, #function
#endif')

define(nocons_nofail_primop_interface_1,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	.global	$1
$1:
	/* Set up C argument registers. */
	mov	P, %o0
	NBIF_ARG(%o1,1,0)

	/* Perform a quick save;call;restore;ret sequence. */
	QUICK_CALL_RET($2,1)
	nop
	.size	$1, .-$1
	.type	$1, #function
#endif')

define(nocons_nofail_primop_interface_2,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	.global	$1
$1:
	/* Set up C argument registers. */
	mov	P, %o0
	NBIF_ARG(%o1,2,0)
	NBIF_ARG(%o2,2,1)

	/* Perform a quick save;call;restore;ret sequence. */
	QUICK_CALL_RET($2,2)
	nop
	.size	$1, .-$1
	.type	$1, #function
#endif')

define(nocons_nofail_primop_interface_3,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	.global	$1
$1:
	/* Set up C argument registers. */
	mov	P, %o0
	NBIF_ARG(%o1,3,0)
	NBIF_ARG(%o2,3,1)
	NBIF_ARG(%o3,3,2)

	/* Perform a quick save;call;restore;ret sequence. */
	QUICK_CALL_RET($2,3)
	nop
	.size	$1, .-$1
	.type	$1, #function
#endif')

define(nocons_nofail_primop_interface_5,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	.global	$1
$1:
	/* Set up C argument registers. */
	mov	P, %o0
	NBIF_ARG(%o1,5,0)
	NBIF_ARG(%o2,5,1)
	NBIF_ARG(%o3,5,2)
	NBIF_ARG(%o4,5,3)
	NBIF_ARG(%o5,5,4)

	/* Perform a quick save;call;restore;ret sequence. */
	QUICK_CALL_RET($2,5)
	nop
	.size	$1, .-$1
	.type	$1, #function
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
	.global $1
$1:
	/* XXX: this case is always trivial; how to suppress the branch? */
	/* Perform a quick save;call;restore;ret sequence. */
	QUICK_CALL_RET($2,0)
	nop
	.size	$1, .-$1
	.type	$1, #function
#endif')

define(noproc_primop_interface_1,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	.global $1
$1:
	/* Set up C argument registers. */
	NBIF_ARG(%o0,1,0)

	/* Perform a quick save;call;restore;ret sequence. */
	QUICK_CALL_RET($2,1)
	nop
	.size	$1, .-$1
	.type	$1, #function
#endif')

define(noproc_primop_interface_2,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	.global $1
$1:
	/* Set up C argument registers. */
	NBIF_ARG(%o0,2,0)
	NBIF_ARG(%o1,2,1)

	/* Perform a quick save;call;restore;ret sequence. */
	QUICK_CALL_RET($2,2)
	nop
	.size	$1, .-$1
	.type	$1, #function
#endif')

define(noproc_primop_interface_3,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	.global $1
$1:
	/* Set up C argument registers. */
	NBIF_ARG(%o0,3,0)
	NBIF_ARG(%o1,3,1)
	NBIF_ARG(%o2,3,2)

	/* Perform a quick save;call;restore;ret sequence. */
	QUICK_CALL_RET($2,3)
	nop
	.size	$1, .-$1
	.type	$1, #function
#endif')

define(noproc_primop_interface_5,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	.global $1
$1:
	/* Set up C argument registers. */
	NBIF_ARG(%o0,5,0)
	NBIF_ARG(%o1,5,1)
	NBIF_ARG(%o2,5,2)
	NBIF_ARG(%o3,5,3)
	NBIF_ARG(%o4,5,4)

	/* Perform a quick save;call;restore;ret sequence. */
	QUICK_CALL_RET($2,5)
	nop
	.size	$1, .-$1
	.type	$1, #function
#endif')

include(`hipe/hipe_bif_list.m4')

`#if defined(__linux__) && defined(__ELF__)
.section .note.GNU-stack,"",%progbits
#endif'
