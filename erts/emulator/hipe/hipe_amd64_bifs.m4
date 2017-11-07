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
include(`hipe/hipe_amd64_asm.m4')
#`include' "config.h"
#`include' "hipe_literals.h"


`#if THE_NON_VALUE == 0
#define TEST_GOT_EXN	testq	%rax, %rax
#else
#define TEST_GOT_EXN	cmpq	$THE_NON_VALUE, %rax
#endif'

define(TEST_GOT_MBUF,`movq P_MBUF(P), %rdx	/* `TEST_GOT_MBUF' */
	testq %rdx, %rdx
	jnz 3f	
2:')
define(HANDLE_GOT_MBUF,`
3:	call nbif_$1_gc_after_bif	/* `HANDLE_GOT_MBUF' */
	jmp 2b')

`#if defined(ERTS_ENABLE_LOCK_CHECK)
#  define CALL_BIF(F) \
		movq CSYM(nbif_impl_##F)@GOTPCREL(%rip), %r11; \
		movq %r11, P_BIF_CALLEE(P); \
		call CSYM(hipe_debug_bif_wrapper)
#else
#  define CALL_BIF(F)	call	CSYM(nbif_impl_##F)
#endif'

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
	TEXT
	.align	4
	GLOBAL(ASYM($1))
ASYM($1):
	/* set up the parameters */
	movq	P, %rdi
	NBIF_ARG(%rsi,1,0)

	/* make the call on the C stack */
	SWITCH_ERLANG_TO_C
	pushq	%rsi
	movq	%rsp, %rsi	/* Eterm* BIF__ARGS */
	sub	$(8), %rsp	/* stack frame 16-byte alignment */
	CALL_BIF($2)
	add	$(1*8 + 8), %rsp
	TEST_GOT_MBUF
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
	/* set up the parameters */
	movq	P, %rdi
	NBIF_ARG(%rsi,2,0)
	NBIF_ARG(%rdx,2,1)

	/* make the call on the C stack */
	SWITCH_ERLANG_TO_C
	pushq	%rdx
	pushq 	%rsi
	movq	%rsp, %rsi	/* Eterm* BIF__ARGS */
	CALL_BIF($2)
	add	$(2*8), %rsp
	TEST_GOT_MBUF
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
	/* set up the parameters */
	movq	P, %rdi
	NBIF_ARG(%rsi,3,0)
	NBIF_ARG(%rdx,3,1)
	NBIF_ARG(%rcx,3,2)

	/* make the call on the C stack */
	SWITCH_ERLANG_TO_C
	pushq 	%rcx
	pushq	%rdx
	pushq	%rsi
	movq	%rsp, %rsi	/* Eterm* BIF__ARGS */
	sub	$(8), %rsp	/* stack frame 16-byte alignment */  
	CALL_BIF($2)
	add	$(3*8 + 8), %rsp
	TEST_GOT_MBUF
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_3_simple_exception
	NBIF_RET(3)
	HANDLE_GOT_MBUF(3)
	SET_SIZE(ASYM($1))
	TYPE_FUNCTION(ASYM($1))
#endif')

define(standard_bif_interface_4,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	TEXT
	.align  4
	GLOBAL(ASYM($1))
ASYM($1):
	/* set up the parameters */
	movq	P, %rdi
	NBIF_ARG(%rsi,4,0)
	NBIF_ARG(%rdx,4,1)
	NBIF_ARG(%rcx,4,2)
	NBIF_ARG(%r8,4,3)

	/* make the call on the C stack */
	SWITCH_ERLANG_TO_C
	pushq	%r8
	pushq	%rcx
	pushq	%rdx
	pushq	%rsi
	movq	%rsp, %rsi	/* Eterm* BIF__ARGS */
	CALL_BIF($2)
	add	$(4*8), %rsp
	TEST_GOT_MBUF
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_4_simple_exception
	NBIF_RET(4)
	HANDLE_GOT_MBUF(4)
	SET_SIZE(ASYM($1))
	TYPE_FUNCTION(ASYM($1))
#endif')

define(standard_bif_interface_0,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	TEXT
	.align	4
	GLOBAL(ASYM($1))
ASYM($1):
	/* set up the parameters */
	movq	P, %rdi

	/* make the call on the C stack */
	SWITCH_ERLANG_TO_C
	CALL_BIF($2)
	TEST_GOT_MBUF
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
	/* set up the parameters */
	movq	P, %rdi

	/* make the call on the C stack */
	SWITCH_ERLANG_TO_C
	call	CSYM($2)
	TEST_GOT_MBUF
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
	/* set up the parameters */
	movq	P, %rdi
	NBIF_ARG(%rsi,1,0)

	/* make the call on the C stack */
	SWITCH_ERLANG_TO_C
	call	CSYM($2)
	TEST_GOT_MBUF
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
	/* set up the parameters */
	movq	P, %rdi
	NBIF_ARG(%rsi,2,0)
	NBIF_ARG(%rdx,2,1)

	/* make the call on the C stack */
	SWITCH_ERLANG_TO_C
	call	CSYM($2)
	TEST_GOT_MBUF
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
	/* set up the parameters */
	movq	P, %rdi
	NBIF_ARG(%rsi,3,0)
	NBIF_ARG(%rdx,3,1)
	NBIF_ARG(%rcx,3,2)

	/* make the call on the C stack */
	SWITCH_ERLANG_TO_C
	call	CSYM($2)
	TEST_GOT_MBUF
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
	/* set up the parameters */
	movq	P, %rdi

	/* make the call on the C stack */
	SWITCH_ERLANG_TO_C_QUICK
	call	CSYM($2)
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
	/* set up the parameters */
	movq	P, %rdi
	NBIF_ARG(%rsi,1,0)

	/* make the call on the C stack */
	SWITCH_ERLANG_TO_C_QUICK
	call	CSYM($2)
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
	/* set up the parameters */
	movq	P, %rdi
	NBIF_ARG(%rsi,2,0)
	NBIF_ARG(%rdx,2,1)

	/* make the call on the C stack */
	SWITCH_ERLANG_TO_C_QUICK
	call	CSYM($2)
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
	/* set up the parameters */
	movq	P, %rdi
	NBIF_ARG(%rsi,3,0)
	NBIF_ARG(%rdx,3,1)
	NBIF_ARG(%rcx,3,2)

	/* make the call on the C stack */
	SWITCH_ERLANG_TO_C_QUICK
	call	CSYM($2)
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
	/* set up the parameters */
	movq	P, %rdi
	NBIF_ARG(%rsi,5,0)
	NBIF_ARG(%rdx,5,1)
	NBIF_ARG(%rcx,5,2)
	NBIF_ARG(%r8,5,3)
	NBIF_ARG(%r9,5,4)

	/* make the call on the C stack */
	SWITCH_ERLANG_TO_C_QUICK
	call	CSYM($2)
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
	/* make the call on the C stack */
	SWITCH_ERLANG_TO_C_QUICK
	call	CSYM($2)
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
	/* set up the parameters */
	NBIF_ARG(%rdi,1,0)

	/* make the call on the C stack */
	SWITCH_ERLANG_TO_C_QUICK
	call	CSYM($2)
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
	/* set up the parameters */
	NBIF_ARG(%rdi,2,0)
	NBIF_ARG(%rsi,2,1)

	/* make the call on the C stack */
	SWITCH_ERLANG_TO_C_QUICK
	call	CSYM($2)
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
	/* set up the parameters */
	NBIF_ARG(%rdi,3,0)
	NBIF_ARG(%rsi,3,1)
	NBIF_ARG(%rdx,3,2)

	/* make the call on the C stack */
	SWITCH_ERLANG_TO_C_QUICK
	call	CSYM($2)
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
	/* set up the parameters */
	NBIF_ARG(%rdi,5,0)
	NBIF_ARG(%rsi,5,1)
	NBIF_ARG(%rdx,5,2)
	NBIF_ARG(%rcx,5,3)
	NBIF_ARG(%r8,5,4)

	/* make the call on the C stack */
	SWITCH_ERLANG_TO_C_QUICK
	call	CSYM($2)
	SWITCH_C_TO_ERLANG_QUICK

	/* return */
	NBIF_RET(5)
	SET_SIZE(ASYM($1))
	TYPE_FUNCTION(ASYM($1))
#endif')

/*
 * AMD64-specific primops.
 */
#ifndef NO_FPE_SIGNALS
noproc_primop_interface_0(nbif_handle_fp_exception, erts_restore_fpu)
#endif /* NO_FPE_SIGNALS */

/*
 * Implement gc_bif_interface_N as standard_bif_interface_N.
 */
define(gc_bif_interface_0,`standard_bif_interface_0($1, $2)')
define(gc_bif_interface_1,`standard_bif_interface_1($1, $2)')
define(gc_bif_interface_2,`standard_bif_interface_2($1, $2)')
define(gc_bif_interface_3,`standard_bif_interface_3($1, $2)')

/*
 * Implement gc_nofail_primop_interface_1 as nofail_primop_interface_1.
 */
define(gc_nofail_primop_interface_1,`nofail_primop_interface_1($1, $2)')

include(`hipe/hipe_bif_list.m4')

`#if defined(__linux__) && defined(__ELF__)
.section .note.GNU-stack,"",%progbits
#endif'
