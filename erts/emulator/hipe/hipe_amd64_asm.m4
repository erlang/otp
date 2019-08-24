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


`#ifndef HIPE_AMD64_ASM_H
#define HIPE_AMD64_ASM_H'

dnl
dnl Tunables.
dnl
define(LEAF_WORDS,24)dnl number of stack words for leaf functions
define(NR_ARG_REGS,4)dnl admissible values are 0 to 6, inclusive
define(HP_IN_REGISTER,1)dnl 1 to reserve a global register for HP
define(FCALLS_IN_REGISTER,0)dnl 1 to reserve global register for FCALLS
define(HEAP_LIMIT_IN_REGISTER,0)dnl global for HL
define(SIMULATE_NSP,0)dnl change to 1 to simulate call/ret insns

`#define AMD64_LEAF_WORDS	'LEAF_WORDS
`#define LEAF_WORDS		'LEAF_WORDS
`#define AMD64_NR_ARG_REGS	'NR_ARG_REGS
`#define NR_ARG_REGS		'NR_ARG_REGS

`#define AMD64_HP_IN_REGISTER	'HP_IN_REGISTER
`#if AMD64_HP_IN_REGISTER'
`#define AMD64_HEAP_POINTER 15'
define(HP,%r15)dnl Only change this together with above
`#endif'

`#define AMD64_FCALLS_IN_REGISTER 'FCALLS_IN_REGISTER
`#if AMD64_FCALLS_IN_REGISTER'
`#define AMD64_FCALLS_REGISTER 11'
define(FCALLS,%r11)dnl This goes together with line above
`#endif'

`#define AMD64_HEAP_LIMIT_IN_REGISTER 'HEAP_LIMIT_IN_REGISTER
`#if AMD64_HEAP_LIMIT_IN_REGISTER'
`#define AMD64_HEAP_LIMIT_REGISTER 12'
define(HEAP_LIMIT,%r12)dnl Change this together with line above
`#endif'

`#define AMD64_SIMULATE_NSP	'SIMULATE_NSP


`#ifdef ASM'
/*
 * Only assembler stuff from here on (when included from *.S)
 */

/*
 * Workarounds for Darwin.
 */
ifelse(OPSYS,darwin,``
/* Darwin */
#define TEXT		.text
#define JOIN(X,Y)	X##Y
#define CSYM(NAME)	JOIN(_,NAME)
#define ASYM(NAME)	CSYM(NAME)
#define GLOBAL(NAME)	.globl NAME
#define SET_SIZE(NAME)	/*empty*/
#define TYPE_FUNCTION(NAME)	/*empty*/
'',``
/* Not Darwin */
#define TEXT		.section ".text"
#define CSYM(NAME)	NAME
#define ASYM(NAME)	NAME
#define GLOBAL(NAME)	.global NAME
#define SET_SIZE(NAME)	.size NAME,.-NAME
#define TYPE_FUNCTION(NAME)	.type NAME,@function
'')dnl


/*
 * Reserved registers.
 */
`#define P		%rbp'

`#if AMD64_HP_IN_REGISTER
#define SAVE_HP	        movq 'HP`, P_HP(P)
#define RESTORE_HP	movq P_HP(P), 'HP`
#else
#define SAVE_HP		/*empty*/
#define RESTORE_HP	/*empty*/
#endif'

`#if AMD64_FCALLS_IN_REGISTER
#define SAVE_FCALLS	movq 'FCALLS`, P_FCALLS(P)
#define RESTORE_FCALLS	movq P_FCALLS(P), 'FCALLS`
#else
#define SAVE_FCALLS	/*empty*/
#define RESTORE_FCALLS	/*empty*/
#endif'

`#if AMD64_HEAP_LIMIT_IN_REGISTER
#define RESTORE_HEAP_LIMIT	movq P_HP_LIMIT(P), 'HEAP_LIMIT`
#else
#define RESTORE_HEAP_LIMIT	/*empty*/
#endif'

define(NSP,%rsp)dnl
`#define NSP		'NSP
`#define SAVE_CSP	movq	%rsp, P_CSP(P)
#define RESTORE_CSP	movq	P_CSP(P), %rsp'


/*
 * Debugging macros
 *
 * Keeps track of whether context has been saved in the debug build, allowing us
 * to detect when the garbage collector is called when it shouldn't.
 */
`#ifdef DEBUG
#  define SET_GC_UNSAFE			\
	movq	$1, P_GCUNSAFE(P)
#  define SET_GC_SAFE			\
	movq	$0, P_GCUNSAFE(P)
#else
#  define SET_GC_UNSAFE
#  define SET_GC_SAFE
#endif'

/*
 * Context switching macros.
 */
`#define SWITCH_C_TO_ERLANG_QUICK	\
	SAVE_CSP; \
	movq P_NSP(P), NSP'

`#define SWITCH_ERLANG_TO_C_QUICK	\
	movq NSP, P_NSP(P); \
	RESTORE_CSP'

`#define SAVE_CACHED_STATE	\
	SAVE_HP;		\
	SAVE_FCALLS;		\
	SET_GC_SAFE'

`#define RESTORE_CACHED_STATE	\
	RESTORE_HP;		\
	RESTORE_HEAP_LIMIT;	\
	RESTORE_FCALLS;		\
	SET_GC_UNSAFE'

`#define SWITCH_C_TO_ERLANG	\
	RESTORE_CACHED_STATE;	\
	SWITCH_C_TO_ERLANG_QUICK'

`#define SWITCH_ERLANG_TO_C	\
	SAVE_CACHED_STATE;	\
	SWITCH_ERLANG_TO_C_QUICK'

/*
 * Argument (parameter) registers.
 */

define(defarg,`define(ARG$1,`$2')dnl
#`define ARG'$1	$2'
)dnl

ifelse(eval(NR_ARG_REGS >= 1),0,,
`defarg(0,`%rsi')')dnl
ifelse(eval(NR_ARG_REGS >= 2),0,,
`defarg(1,`%rdx')')dnl
ifelse(eval(NR_ARG_REGS >= 3),0,,
`defarg(2,`%rcx')')dnl
ifelse(eval(NR_ARG_REGS >= 4),0,,
`defarg(3,`%r8')')dnl
ifelse(eval(NR_ARG_REGS >= 5),0,,
`defarg(4,`%r9')')dnl
ifelse(eval(NR_ARG_REGS >= 6),0,,
`defarg(5,`%rdi')')dnl

/*
 * TEMP_RV:
 *	Used in nbif_stack_trap_ra to preserve the return value.
 *	Must be a C callee-save register.
 *	Must be otherwise unused in the return path.
 */
`#define TEMP_RV		%rbx'

dnl XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
dnl X								X
dnl X			hipe_amd64_glue.S support		X
dnl X								X
dnl XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

dnl
dnl LOAD_ARG_REGS
dnl (identical to x86 version except for movq)
dnl
define(LAR_1,`movq P_ARG$1(P), ARG$1 ; ')dnl
define(LAR_N,`ifelse(eval($1 >= 0),0,,`LAR_N(eval($1-1))LAR_1($1)')')dnl
define(LOAD_ARG_REGS,`LAR_N(eval(NR_ARG_REGS-1))')dnl
`#define LOAD_ARG_REGS	'LOAD_ARG_REGS

dnl
dnl STORE_ARG_REGS
dnl (identical to x86 version except for movq)
dnl
define(SAR_1,`movq ARG$1, P_ARG$1(P) ; ')dnl
define(SAR_N,`ifelse(eval($1 >= 0),0,,`SAR_N(eval($1-1))SAR_1($1)')')dnl
define(STORE_ARG_REGS,`SAR_N(eval(NR_ARG_REGS-1))')dnl
`#define STORE_ARG_REGS	'STORE_ARG_REGS

dnl
dnl NSP_CALL(FUN)
dnl Emit a CALL FUN instruction, or simulate it.
dnl FUN must not be an NSP-based memory operand.
dnl
ifelse(eval(SIMULATE_NSP),0,
``#define NSP_CALL(FUN)	call FUN'',
``#define NSP_CALL(FUN)	subq $8,NSP; leaq 1f(%rip),%rax; movq %rax,(NSP); jmp FUN; 1:'')dnl

dnl
dnl NSP_RETN(NPOP)
dnl Emit a RET $NPOP instruction, or simulate it.
dnl NPOP should be non-zero.
dnl
ifelse(eval(SIMULATE_NSP),0,
``#define NSP_RETN(NPOP)	ret $NPOP'',
``#define NSP_RETN(NPOP)	movq (NSP),TEMP_RV; addq $8+NPOP,NSP; jmp *TEMP_RV'')dnl

dnl
dnl NSP_RET0
dnl Emit a RET instruction, or simulate it.
dnl
ifelse(eval(SIMULATE_NSP),0,
``#define NSP_RET0	ret'',
``#define NSP_RET0	movq (NSP),TEMP_RV; addq $8,NSP; jmp *TEMP_RV'')dnl

dnl XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
dnl X								X
dnl X			hipe_amd64_bifs.m4 support		X
dnl X								X
dnl XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

dnl
dnl NBIF_ARG(DST,ARITY,ARGNO)
dnl Access a formal parameter.
dnl It will be a memory load via NSP when ARGNO >= NR_ARG_REGS.
dnl It will be a register move when 0 <= ARGNO < NR_ARG_REGS; if
dnl the source and destination are the same, the move is suppressed.
dnl
dnl This must be called before SWITCH_ERLANG_TO_C{,QUICK}.
dnl This must not be called if the C BIF's arity > 6.
dnl
define(NBIF_MOVE_REG,`ifelse($1,$2,`# movq	$2, $1',`movq	$2, $1')')dnl
define(NBIF_REG_ARG,`NBIF_MOVE_REG($1,ARG$2)')dnl
define(NBIF_STK_LOAD,`movq	$2(NSP), $1')dnl
define(NBIF_STK_ARG,`NBIF_STK_LOAD($1,eval(8*($2-$3)))')dnl
define(NBIF_ARG,`ifelse(eval($3 >= NR_ARG_REGS),0,`NBIF_REG_ARG($1,$3)',`NBIF_STK_ARG($1,$2,$3)')')dnl
`/* #define NBIF_ARG_1_0	'NBIF_ARG(%rsi,1,0)` */'
`/* #define NBIF_ARG_2_0	'NBIF_ARG(%rsi,2,0)` */'
`/* #define NBIF_ARG_2_1	'NBIF_ARG(%rdx,2,1)` */'
`/* #define NBIF_ARG_3_0	'NBIF_ARG(%rsi,3,0)` */'
`/* #define NBIF_ARG_3_1	'NBIF_ARG(%rdx,3,1)` */'
`/* #define NBIF_ARG_3_2	'NBIF_ARG(%rcx,3,2)` */'
`/* #define NBIF_ARG_4_0	'NBIF_ARG(%rsi,4,0)` */'
`/* #define NBIF_ARG_4_1	'NBIF_ARG(%rdx,4,1)` */'
`/* #define NBIF_ARG_4_2	'NBIF_ARG(%rcx,4,2)` */'
`/* #define NBIF_ARG_4_3	'NBIF_ARG(%r8,4,3)` */'
`/* #define NBIF_ARG_5_0	'NBIF_ARG(%rsi,5,0)` */'
`/* #define NBIF_ARG_5_1	'NBIF_ARG(%rdx,5,1)` */'
`/* #define NBIF_ARG_5_2	'NBIF_ARG(%rcx,5,2)` */'
`/* #define NBIF_ARG_5_3	'NBIF_ARG(%r8,5,3)` */'
`/* #define NBIF_ARG_5_4	'NBIF_ARG(%r9,5,4)` */'

dnl XXX: For >6 arity C BIFs, we need:
dnl	NBIF_COPY_NSP(ARITY)
dnl	SWITCH_ERLANG_TO_C
dnl	NBIF_GE6_ARG_MOVE(DSTREG,ARITY,ARGNO)
dnl	pushq NBIF_GE6_ARG_OPND(ARITY,ARGNO) <-- uses NSP copied above

dnl
dnl NBIF_RET(ARITY)
dnl Generates a return from a native BIF, taking care to pop
dnl any stacked formal parameters.
dnl
define(RET_POP,`ifelse(eval($1 > NR_ARG_REGS),0,0,eval(8*($1 - NR_ARG_REGS)))')dnl
define(NBIF_RET_N,`ifelse(eval($1),0,`NSP_RET0',`NSP_RETN($1)')')dnl
define(NBIF_RET,`NBIF_RET_N(eval(RET_POP($1)))')dnl
`/* #define NBIF_RET_0	'NBIF_RET(0)` */'
`/* #define NBIF_RET_1	'NBIF_RET(1)` */'
`/* #define NBIF_RET_2	'NBIF_RET(2)` */'
`/* #define NBIF_RET_3	'NBIF_RET(3)` */'
`/* #define NBIF_RET_4	'NBIF_RET(4)` */'
`/* #define NBIF_RET_5	'NBIF_RET(5)` */'

`#endif /* ASM */'

`#endif /* HIPE_AMD64_ASM_H */'
