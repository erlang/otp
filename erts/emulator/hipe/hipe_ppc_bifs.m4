changecom(`/*', `*/')dnl
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


#`define ASM'
include(`hipe/hipe_ppc_asm.m4')
#`include' "config.h"
#`include' "hipe_literals.h"

`#if defined(ERTS_ENABLE_LOCK_CHECK)
#  define CALL_BIF(F)	STORE_IA(CSYM(nbif_impl_##F), P_BIF_CALLEE(P), r29); bl CSYM(hipe_debug_bif_wrapper) 
#else
#  define CALL_BIF(F)	bl	CSYM(nbif_impl_##F)
#endif'

	.text
	.p2align 2

define(TEST_GOT_MBUF,`LOAD r4, P_MBUF(P)	/* `TEST_GOT_MBUF' */
	CMPI r4, 0
	bne- 3f
2:')
define(HANDLE_GOT_MBUF,`
3:	bl CSYM(nbif_$1_gc_after_bif)	/* `HANDLE_GOT_MBUF' */
	b 2b')


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
	GLOBAL(ASYM($1))
ASYM($1):
	/* Set up C argument registers. */
	mr	r3, P
	NBIF_ARG(r4,1,0)

	/* Save caller-save registers and call the C function. */
	SAVE_CONTEXT_BIF
	STORE	r4, P_ARG0(r3)		/* Store BIF__ARGS in def_arg_reg[] */
	addi	r4, r3, P_ARG0
	CALL_BIF($2)
	TEST_GOT_MBUF

	/* Restore registers. Check for exception. */
	CMPI	r3, THE_NON_VALUE
	RESTORE_CONTEXT_BIF
	beq-	1f
	NBIF_RET(1)
1:	/* workaround for bc:s small offset operand */
	b	CSYM(nbif_1_simple_exception)
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM($1))
	TYPE_FUNCTION(ASYM($1))
#endif')

define(standard_bif_interface_2,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	GLOBAL(ASYM($1))
ASYM($1):
	/* Set up C argument registers. */
	mr	r3, P
	NBIF_ARG(r4,2,0)
	NBIF_ARG(r5,2,1)

	/* Save caller-save registers and call the C function. */
	SAVE_CONTEXT_BIF
	STORE	r4, P_ARG0(r3)		/* Store BIF__ARGS in def_arg_reg[] */
	STORE	r5, P_ARG1(r3)
	addi	r4, r3, P_ARG0
	CALL_BIF($2)
	TEST_GOT_MBUF

	/* Restore registers. Check for exception. */
	CMPI	r3, THE_NON_VALUE
	RESTORE_CONTEXT_BIF
	beq-	1f
	NBIF_RET(2)
1:	/* workaround for bc:s small offset operand */
	b	CSYM(nbif_2_simple_exception)
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM($1))
	TYPE_FUNCTION(ASYM($1))
#endif')

define(standard_bif_interface_3,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	GLOBAL(ASYM($1))
ASYM($1):
	/* Set up C argument registers. */
	mr	r3, P
	NBIF_ARG(r4,3,0)
	NBIF_ARG(r5,3,1)
	NBIF_ARG(r6,3,2)

	/* Save caller-save registers and call the C function. */
	SAVE_CONTEXT_BIF
	STORE	r4, P_ARG0(r3)		/* Store BIF__ARGS in def_arg_reg[] */
	STORE	r5, P_ARG1(r3)
	STORE	r6, P_ARG2(r3)
	addi	r4, r3, P_ARG0
	CALL_BIF($2)
	TEST_GOT_MBUF

	/* Restore registers. Check for exception. */
	CMPI	r3, THE_NON_VALUE
	RESTORE_CONTEXT_BIF
	beq-	1f
	NBIF_RET(3)
1:	/* workaround for bc:s small offset operand */
	b	CSYM(nbif_3_simple_exception)
	HANDLE_GOT_MBUF(3)
	SET_SIZE(ASYM($1))
	TYPE_FUNCTION(ASYM($1))
#endif')

define(standard_bif_interface_4,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	GLOBAL(ASYM($1))
ASYM($1):
	/* Set up C argument registers. */
	mr	r3, P
	NBIF_ARG(r4,4,0)
	NBIF_ARG(r5,4,1)
	NBIF_ARG(r6,4,2)
	NBIF_ARG(r7,4,3)

	/* Save caller-save registers and call the C function. */
	SAVE_CONTEXT_BIF
	STORE	r4, P_ARG0(r3)		/* Store BIF__ARGS in def_arg_reg[] */
	STORE	r5, P_ARG1(r3)
	STORE	r6, P_ARG2(r3)
	STORE	r7, P_ARG3(r3)
	addi	r4, r3, P_ARG0
	CALL_BIF($2)
	TEST_GOT_MBUF

	/* Restore registers. Check for exception. */
	CMPI	r3, THE_NON_VALUE
	RESTORE_CONTEXT_BIF
	beq-	1f
	NBIF_RET(4)
1:	/* workaround for bc:s small offset operand */
	b	CSYM(nbif_4_simple_exception)
	HANDLE_GOT_MBUF(4)
	SET_SIZE(ASYM($1))
	TYPE_FUNCTION(ASYM($1))
#endif')

define(standard_bif_interface_0,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	GLOBAL(ASYM($1))
ASYM($1):
	/* Set up C argument registers. */
	mr	r3, P

	/* Save caller-save registers and call the C function. */
	SAVE_CONTEXT_BIF
	/* ignore empty BIF__ARGS */
	CALL_BIF($2)
	TEST_GOT_MBUF

	/* Restore registers. Check for exception. */
	CMPI	r3, THE_NON_VALUE
	RESTORE_CONTEXT_BIF
	beq-	1f
	NBIF_RET(0)
1:	/* workaround for bc:s small offset operand */
	b	CSYM(nbif_0_simple_exception)
	HANDLE_GOT_MBUF(0)
	SET_SIZE(ASYM($1))
	TYPE_FUNCTION(ASYM($1))
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
	GLOBAL(ASYM($1))
ASYM($1):
	/* Set up C argument registers. */
	mr	r3, P

	/* Save caller-save registers and call the C function. */
	SAVE_CONTEXT_GC
	/* ignore empty BIF__ARGS */
	CALL_BIF($2)
	TEST_GOT_MBUF

	/* Restore registers. */
	RESTORE_CONTEXT_GC
	NBIF_RET(0)
	HANDLE_GOT_MBUF(0)
	SET_SIZE(ASYM($1))
	TYPE_FUNCTION(ASYM($1))
#endif')

define(gc_bif_interface_1,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	GLOBAL(ASYM($1))
ASYM($1):
	/* Set up C argument registers. */
	mr	r3, P
	NBIF_ARG(r4,1,0)

	/* Save caller-save registers and call the C function. */
	SAVE_CONTEXT_GC
	STORE	r4, P_ARG0(r3)		/* Store BIF__ARGS in def_arg_reg[] */
	addi	r4, r3, P_ARG0
	CALL_BIF($2)
	TEST_GOT_MBUF

	/* Restore registers. Check for exception. */
	CMPI	r3, THE_NON_VALUE
	RESTORE_CONTEXT_GC
	beq-	1f
	NBIF_RET(1)
1:	/* workaround for bc:s small offset operand */
	b	CSYM(nbif_1_simple_exception)
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM($1))
	TYPE_FUNCTION(ASYM($1))
#endif')

define(gc_bif_interface_2,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	GLOBAL(ASYM($1))
ASYM($1):
	/* Set up C argument registers. */
	mr	r3, P
	NBIF_ARG(r4,2,0)
	NBIF_ARG(r5,2,1)

	/* Save caller-save registers and call the C function. */
	SAVE_CONTEXT_GC
	STORE	r4, P_ARG0(r3)		/* Store BIF__ARGS in def_arg_reg[] */
	STORE	r5, P_ARG1(r3)
	addi	r4, r3, P_ARG0
	CALL_BIF($2)
	TEST_GOT_MBUF

	/* Restore registers. Check for exception. */
	CMPI	r3, THE_NON_VALUE
	RESTORE_CONTEXT_GC
	beq-	1f
	NBIF_RET(2)
1:	/* workaround for bc:s small offset operand */
	b	CSYM(nbif_2_simple_exception)
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM($1))
	TYPE_FUNCTION(ASYM($1))
#endif')

define(gc_bif_interface_3,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	GLOBAL(ASYM($1))
ASYM($1):
	/* Set up C argument registers. */
	mr	r3, P
	NBIF_ARG(r4,3,0)
	NBIF_ARG(r5,3,1)
	NBIF_ARG(r6,3,2)

	/* Save caller-save registers and call the C function. */
	SAVE_CONTEXT_GC
	STORE	r4, P_ARG0(r3)		/* Store BIF__ARGS in def_arg_reg[] */
	STORE	r5, P_ARG1(r3)
	STORE	r6, P_ARG2(r3)
	addi	r4, r3, P_ARG0
	CALL_BIF($2)
	TEST_GOT_MBUF

	/* Restore registers. Check for exception. */
	CMPI	r3, THE_NON_VALUE
	RESTORE_CONTEXT_GC
	beq-	1f
	NBIF_RET(3)
1:	/* workaround for bc:s small offset operand */
	b	CSYM(nbif_3_simple_exception)
	HANDLE_GOT_MBUF(3)
	SET_SIZE(ASYM($1))
	TYPE_FUNCTION(ASYM($1))
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
	GLOBAL(ASYM($1))
ASYM($1):
	/* Set up C argument registers. */
	mr	r3, P
	NBIF_ARG(r4,1,0)

	/* Save caller-save registers and call the C function. */
	SAVE_CONTEXT_GC
	bl	CSYM($2)

	/* Restore registers. */
	RESTORE_CONTEXT_GC
	NBIF_RET(1)
	SET_SIZE(ASYM($1))
	TYPE_FUNCTION(ASYM($1))
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
	GLOBAL(ASYM($1))
ASYM($1):
	/* Set up C argument registers. */
	mr	r3, P

	/* Save caller-save registers and call the C function. */
	SAVE_CONTEXT_BIF
	bl	CSYM($2)
	TEST_GOT_MBUF

	/* Restore registers. */
	RESTORE_CONTEXT_BIF
	NBIF_RET(0)
	HANDLE_GOT_MBUF(0)
	SET_SIZE(ASYM($1))
	TYPE_FUNCTION(ASYM($1))
#endif')

define(nofail_primop_interface_1,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	GLOBAL(ASYM($1))
ASYM($1):
	/* Set up C argument registers. */
	mr	r3, P
	NBIF_ARG(r4,1,0)

	/* Save caller-save registers and call the C function. */
	SAVE_CONTEXT_BIF
	bl	CSYM($2)
	TEST_GOT_MBUF

	/* Restore registers. */
	RESTORE_CONTEXT_BIF
	NBIF_RET(1)
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM($1))
	TYPE_FUNCTION(ASYM($1))
#endif')

define(nofail_primop_interface_2,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	GLOBAL(ASYM($1))
ASYM($1):
	/* Set up C argument registers. */
	mr	r3, P
	NBIF_ARG(r4,2,0)
	NBIF_ARG(r5,2,1)

	/* Save caller-save registers and call the C function. */
	SAVE_CONTEXT_BIF
	bl	CSYM($2)
	TEST_GOT_MBUF

	/* Restore registers. */
	RESTORE_CONTEXT_BIF
	NBIF_RET(2)
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM($1))
	TYPE_FUNCTION(ASYM($1))
#endif')

define(nofail_primop_interface_3,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	GLOBAL(ASYM($1))
ASYM($1):
	/* Set up C argument registers. */
	mr	r3, P
	NBIF_ARG(r4,3,0)
	NBIF_ARG(r5,3,1)
	NBIF_ARG(r6,3,2)

	/* Save caller-save registers and call the C function. */
	SAVE_CONTEXT_BIF
	bl	CSYM($2)
	TEST_GOT_MBUF

	/* Restore registers. */
	RESTORE_CONTEXT_BIF
	NBIF_RET(3)
	HANDLE_GOT_MBUF(3)
	SET_SIZE(ASYM($1))
	TYPE_FUNCTION(ASYM($1))
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
	GLOBAL(ASYM($1))
ASYM($1):
	/* Set up C argument registers. */
	mr	r3, P

	/* Perform a quick save;call;restore;ret sequence. */
	QUICK_CALL_RET(CSYM($2),0)
	SET_SIZE(ASYM($1))
	TYPE_FUNCTION(ASYM($1))
#endif')

define(nocons_nofail_primop_interface_1,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	GLOBAL(ASYM($1))
ASYM($1):
	/* Set up C argument registers. */
	mr	r3, P
	NBIF_ARG(r4,1,0)

	/* Perform a quick save;call;restore;ret sequence. */
	QUICK_CALL_RET(CSYM($2),1)
	SET_SIZE(ASYM($1))
	TYPE_FUNCTION(ASYM($1))
#endif')

define(nocons_nofail_primop_interface_2,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	GLOBAL(ASYM($1))
ASYM($1):
	/* Set up C argument registers. */
	mr	r3, P
	NBIF_ARG(r4,2,0)
	NBIF_ARG(r5,2,1)

	/* Perform a quick save;call;restore;ret sequence. */
	QUICK_CALL_RET(CSYM($2),2)
	SET_SIZE(ASYM($1))
	TYPE_FUNCTION(ASYM($1))
#endif')

define(nocons_nofail_primop_interface_3,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	GLOBAL(ASYM($1))
ASYM($1):
	/* Set up C argument registers. */
	mr	r3, P
	NBIF_ARG(r4,3,0)
	NBIF_ARG(r5,3,1)
	NBIF_ARG(r6,3,2)

	/* Perform a quick save;call;restore;ret sequence. */
	QUICK_CALL_RET(CSYM($2),3)
	SET_SIZE(ASYM($1))
	TYPE_FUNCTION(ASYM($1))
#endif')

define(nocons_nofail_primop_interface_5,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	GLOBAL(ASYM($1))
ASYM($1):
	/* Set up C argument registers. */
	mr	r3, P
	NBIF_ARG(r4,5,0)
	NBIF_ARG(r5,5,1)
	NBIF_ARG(r6,5,2)
	NBIF_ARG(r7,5,3)
	NBIF_ARG(r8,5,4)

	/* Perform a quick save;call;restore;ret sequence. */
	QUICK_CALL_RET(CSYM($2),5)
	SET_SIZE(ASYM($1))
	TYPE_FUNCTION(ASYM($1))
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
	GLOBAL(ASYM($1))
ASYM($1):
	/* XXX: this case is always trivial; how to suppress the branch? */
	/* Perform a quick save;call;restore;ret sequence. */
	QUICK_CALL_RET(CSYM($2),0)
	SET_SIZE(ASYM($1))
	TYPE_FUNCTION(ASYM($1))
#endif')

define(noproc_primop_interface_1,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	GLOBAL(ASYM($1))
ASYM($1):
	/* Set up C argument registers. */
	NBIF_ARG(r3,1,0)

	/* Perform a quick save;call;restore;ret sequence. */
	QUICK_CALL_RET(CSYM($2),1)
	SET_SIZE(ASYM($1))
	TYPE_FUNCTION(ASYM($1))
#endif')

define(noproc_primop_interface_2,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	GLOBAL(ASYM($1))
ASYM($1):
	/* Set up C argument registers. */
	NBIF_ARG(r3,2,0)
	NBIF_ARG(r4,2,1)

	/* Perform a quick save;call;restore;ret sequence. */
	QUICK_CALL_RET(CSYM($2),2)
	SET_SIZE(ASYM($1))
	TYPE_FUNCTION(ASYM($1))
#endif')

define(noproc_primop_interface_3,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	GLOBAL(ASYM($1))
ASYM($1):
	/* Set up C argument registers. */
	NBIF_ARG(r3,3,0)
	NBIF_ARG(r4,3,1)
	NBIF_ARG(r5,3,2)

	/* Perform a quick save;call;restore;ret sequence. */
	QUICK_CALL_RET(CSYM($2),3)
	SET_SIZE(ASYM($1))
	TYPE_FUNCTION(ASYM($1))
#endif')

define(noproc_primop_interface_5,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	GLOBAL(ASYM($1))
ASYM($1):
	/* Set up C argument registers. */
	NBIF_ARG(r3,5,0)
	NBIF_ARG(r4,5,1)
	NBIF_ARG(r5,5,2)
	NBIF_ARG(r6,5,3)
	NBIF_ARG(r7,5,4)

	/* Perform a quick save;call;restore;ret sequence. */
	QUICK_CALL_RET(CSYM($2),5)
	SET_SIZE(ASYM($1))
	TYPE_FUNCTION(ASYM($1))
#endif')

include(`hipe/hipe_bif_list.m4')

`#if defined(__linux__) && defined(__ELF__)
.section .note.GNU-stack,"",%progbits
#endif'
