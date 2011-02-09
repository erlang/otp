changecom(`/*', `*/')dnl
/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2001-2011. All Rights Reserved.
 *
 * The contents of this file are subject to the Erlang Public License,
 * Version 1.1, (the "License"); you may not use this file except in
 * compliance with the License. You should have received a copy of the
 * Erlang Public License along with this software. If not, it can be
 * retrieved online at http://www.erlang.org/.
 *
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
 * the License for the specific language governing rights and limitations
 * under the License.
 *
 * %CopyrightEnd%
 */


include(`hipe/hipe_x86_asm.m4')
#`include' "hipe_literals.h"

`#if THE_NON_VALUE == 0
#define TEST_GOT_EXN	testl	%eax,%eax
#else
#define TEST_GOT_EXN	cmpl	$THE_NON_VALUE,%eax
#endif'

`#define TEST_GOT_MBUF		movl P_MBUF(P), %edx; testl %edx, %edx; jnz 3f; 2:
#define JOIN3(A,B,C)		A##B##C
#define HANDLE_GOT_MBUF(ARITY)	3: call JOIN3(nbif_,ARITY,_gc_after_bif); jmp 2b'

/*
 * standard_bif_interface_1(nbif_name, cbif_name)
 * standard_bif_interface_2(nbif_name, cbif_name)
 * standard_bif_interface_3(nbif_name, cbif_name)
 *
 * Generate native interface for a BIF with 1-3 parameters and
 * standard failure mode.
 */
define(standard_bif_interface_1,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	TEXT
	.align	4
	GLOBAL(ASYM($1))
ASYM($1):
	/* copy native stack pointer */
	NBIF_COPY_NSP(1)

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	NBIF_ARG_REG(0,P)
	NBIF_ARG(1,1,0)
	call	CSYM($2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NBIF_RET(1)
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM($1))
	TYPE_FUNCTION(ASYM($1))
#endif')

define(standard_bif_interface_2,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	TEXT
	.align	4
	GLOBAL(ASYM($1))
ASYM($1):
	/* copy native stack pointer */
	NBIF_COPY_NSP(2)

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	NBIF_ARG_REG(0,P)
	NBIF_ARG(1,2,0)
	NBIF_ARG(2,2,1)
	call	CSYM($2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NBIF_RET(2)
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM($1))
	TYPE_FUNCTION(ASYM($1))
#endif')

define(standard_bif_interface_3,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	TEXT
	.align	4
	GLOBAL(ASYM($1))
ASYM($1):
	/* copy native stack pointer */
	NBIF_COPY_NSP(3)

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	NBIF_ARG_REG(0,P)
	NBIF_ARG(1,3,0)
	NBIF_ARG(2,3,1)
	NBIF_ARG(3,3,2)
	call	CSYM($2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_3_simple_exception
	NBIF_RET(3)
	HANDLE_GOT_MBUF(3)
	SET_SIZE(ASYM($1))
	TYPE_FUNCTION(ASYM($1))
#endif')

/*
 * fail_bif_interface_0(nbif_name, cbif_name)
 *
 * Generate native interface for a BIF with 0 parameters and
 * standard failure mode.
 */
define(fail_bif_interface_0,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	TEXT
	.align	4
	GLOBAL(ASYM($1))
ASYM($1):
	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	NBIF_ARG_REG(0,P)
	call	CSYM($2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_0_simple_exception
	NBIF_RET(0)
	HANDLE_GOT_MBUF(0)
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
	TEXT
	.align	4
	GLOBAL(ASYM($1))
ASYM($1):
	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	NBIF_ARG_REG(0,P)
	call	CSYM($2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* return */
	NBIF_RET(0)
	HANDLE_GOT_MBUF(0)
	SET_SIZE(ASYM($1))
	TYPE_FUNCTION(ASYM($1))
#endif')

define(nofail_primop_interface_1,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	TEXT
	.align	4
	GLOBAL(ASYM($1))
ASYM($1):
	/* copy native stack pointer */
	NBIF_COPY_NSP(1)

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	NBIF_ARG_REG(0,P)
	NBIF_ARG(1,1,0)
	call	CSYM($2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* return */
	NBIF_RET(1)
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM($1))
	TYPE_FUNCTION(ASYM($1))
#endif')

define(nofail_primop_interface_2,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	TEXT
	.align	4
	GLOBAL(ASYM($1))
ASYM($1):
	/* copy native stack pointer */
	NBIF_COPY_NSP(2)

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	NBIF_ARG_REG(0,P)
	NBIF_ARG(1,2,0)
	NBIF_ARG(2,2,1)
	call	CSYM($2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* return */
	NBIF_RET(2)
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM($1))
	TYPE_FUNCTION(ASYM($1))
#endif')

define(nofail_primop_interface_3,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	TEXT
	.align	4
	GLOBAL(ASYM($1))
ASYM($1):
	/* copy native stack pointer */
	NBIF_COPY_NSP(3)

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	NBIF_ARG_REG(0,P)
	NBIF_ARG(1,3,0)
	NBIF_ARG(2,3,1)
	NBIF_ARG(3,3,2)
	call	CSYM($2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* return */
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
	TEXT
	.align	4
	GLOBAL(ASYM($1))
ASYM($1):
	/* switch to C stack */
	SWITCH_ERLANG_TO_C_QUICK

	/* make the call on the C stack */
	NBIF_ARG_REG(0,P)
	call	CSYM($2)

	/* switch to native stack */
	SWITCH_C_TO_ERLANG_QUICK

	/* return */
	NBIF_RET(0)
	SET_SIZE(ASYM($1))
	TYPE_FUNCTION(ASYM($1))
#endif')

define(nocons_nofail_primop_interface_1,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	TEXT
	.align	4
	GLOBAL(ASYM($1))
ASYM($1):
	/* copy native stack pointer */
	NBIF_COPY_NSP(1)

	/* switch to C stack */
	SWITCH_ERLANG_TO_C_QUICK

	/* make the call on the C stack */
	NBIF_ARG_REG(0,P)
	NBIF_ARG(1,1,0)
	call	CSYM($2)

	/* switch to native stack */
	SWITCH_C_TO_ERLANG_QUICK

	/* return */
	NBIF_RET(1)
	SET_SIZE(ASYM($1))
	TYPE_FUNCTION(ASYM($1))
#endif')

define(nocons_nofail_primop_interface_2,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	TEXT
	.align	4
	GLOBAL(ASYM($1))
ASYM($1):
	/* copy native stack pointer */
	NBIF_COPY_NSP(2)

	/* switch to C stack */
	SWITCH_ERLANG_TO_C_QUICK

	/* make the call on the C stack */
	NBIF_ARG_REG(0,P)
	NBIF_ARG(1,2,0)
	NBIF_ARG(2,2,1)
	call	CSYM($2)

	/* switch to native stack */
	SWITCH_C_TO_ERLANG_QUICK

	/* return */
	NBIF_RET(2)
	SET_SIZE(ASYM($1))
	TYPE_FUNCTION(ASYM($1))
#endif')

define(nocons_nofail_primop_interface_3,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	TEXT
	.align	4
	GLOBAL(ASYM($1))
ASYM($1):
	/* copy native stack pointer */
	NBIF_COPY_NSP(3)

	/* switch to C stack */
	SWITCH_ERLANG_TO_C_QUICK

	/* make the call on the C stack */
	NBIF_ARG_REG(0,P)
	NBIF_ARG(1,3,0)
	NBIF_ARG(2,3,1)
	NBIF_ARG(3,3,2)
	call	CSYM($2)

	/* switch to native stack */
	SWITCH_C_TO_ERLANG_QUICK

	/* return */
	NBIF_RET(3)
	SET_SIZE(ASYM($1))
	TYPE_FUNCTION(ASYM($1))
#endif')

define(nocons_nofail_primop_interface_5,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	TEXT
	.align	4
	GLOBAL(ASYM($1))
ASYM($1):
	/* copy native stack pointer */
	NBIF_COPY_NSP(5)

	/* switch to C stack */
	SWITCH_ERLANG_TO_C_QUICK

	/* make the call on the C stack */
	NBIF_ARG_REG(0,P)
	NBIF_ARG(1,5,0)
	NBIF_ARG(2,5,1)
	NBIF_ARG(3,5,2)
	NBIF_ARG(4,5,3)
	NBIF_ARG(5,5,4)
	call	CSYM($2)

	/* switch to native stack */
	SWITCH_C_TO_ERLANG_QUICK

	/* return */
	NBIF_RET(5)
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
	TEXT
	.align	4
	GLOBAL(ASYM($1))
ASYM($1):
	/* switch to C stack */
	SWITCH_ERLANG_TO_C_QUICK

	/* make the call on the C stack */
	call	CSYM($2)

	/* switch to native stack */
	SWITCH_C_TO_ERLANG_QUICK

	/* return */
	NBIF_RET(0)
	SET_SIZE(ASYM($1))
	TYPE_FUNCTION(ASYM($1))
#endif')

define(noproc_primop_interface_1,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	TEXT
	.align	4
	GLOBAL(ASYM($1))
ASYM($1):
	/* copy native stack pointer */
	NBIF_COPY_NSP(1)

	/* switch to C stack */
	SWITCH_ERLANG_TO_C_QUICK

	/* make the call on the C stack */
	NBIF_ARG(0,1,0)
	call	CSYM($2)

	/* switch to native stack */
	SWITCH_C_TO_ERLANG_QUICK

	/* return */
	NBIF_RET(1)
	SET_SIZE(ASYM($1))
	TYPE_FUNCTION(ASYM($1))
#endif')

define(noproc_primop_interface_2,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	TEXT
	.align	4
	GLOBAL(ASYM($1))
ASYM($1):
	/* copy native stack pointer */
	NBIF_COPY_NSP(2)

	/* switch to C stack */
	SWITCH_ERLANG_TO_C_QUICK

	/* make the call on the C stack */
	NBIF_ARG(0,2,0)
	NBIF_ARG(1,2,1)
	call	CSYM($2)

	/* switch to native stack */
	SWITCH_C_TO_ERLANG_QUICK

	/* return */
	NBIF_RET(2)
	SET_SIZE(ASYM($1))
	TYPE_FUNCTION(ASYM($1))
#endif')

define(noproc_primop_interface_3,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	TEXT
	.align	4
	GLOBAL(ASYM($1))
ASYM($1):
	/* copy native stack pointer */
	NBIF_COPY_NSP(3)

	/* switch to C stack */
	SWITCH_ERLANG_TO_C_QUICK

	/* make the call on the C stack */
	NBIF_ARG(0,3,0)
	NBIF_ARG(1,3,1)
	NBIF_ARG(2,3,2)
	call	CSYM($2)

	/* switch to native stack */
	SWITCH_C_TO_ERLANG_QUICK

	/* return */
	NBIF_RET(3)
	SET_SIZE(ASYM($1))
	TYPE_FUNCTION(ASYM($1))
#endif')

define(noproc_primop_interface_5,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	TEXT
	.align	4
	GLOBAL(ASYM($1))
ASYM($1):
	/* copy native stack pointer */
	NBIF_COPY_NSP(5)

	/* switch to C stack */
	SWITCH_ERLANG_TO_C_QUICK

	/* make the call on the C stack */
	NBIF_ARG(0,5,0)
	NBIF_ARG(1,5,1)
	NBIF_ARG(2,5,2)
	NBIF_ARG(3,5,3)
	NBIF_ARG(4,5,4)
	call	CSYM($2)

	/* switch to native stack */
	SWITCH_C_TO_ERLANG_QUICK

	/* return */
	NBIF_RET(5)
	SET_SIZE(ASYM($1))
	TYPE_FUNCTION(ASYM($1))
#endif')

/*
 * x86-specific primops.
 */
noproc_primop_interface_0(nbif_handle_fp_exception, erts_restore_fpu)

/*
 * Implement gc_bif_interface_0 as nofail_primop_interface_0.
 */
define(gc_bif_interface_0,`nofail_primop_interface_0($1, $2)')

/*
 * Implement gc_bif_interface_N as standard_bif_interface_N (N=1,2).
 */
define(gc_bif_interface_1,`standard_bif_interface_1($1, $2)')
define(gc_bif_interface_2,`standard_bif_interface_2($1, $2)')

/*
 * Implement gc_nofail_primop_interface_1 as nofail_primop_interface_1.
 */
define(gc_nofail_primop_interface_1,`nofail_primop_interface_1($1, $2)')

include(`hipe/hipe_bif_list.m4')

`#if defined(__linux__) && defined(__ELF__)
.section .note.GNU-stack,"",%progbits
#endif'
