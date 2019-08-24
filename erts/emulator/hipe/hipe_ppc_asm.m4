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


`#ifndef HIPE_PPC_ASM_H
#define HIPE_PPC_ASM_H'

/*
 * Tunables.
 */
define(LEAF_WORDS,16)dnl number of stack words for leaf functions
define(NR_ARG_REGS,4)dnl admissible values are 0 to 6, inclusive

`#define PPC_LEAF_WORDS	'LEAF_WORDS
`#define PPC_NR_ARG_REGS	'NR_ARG_REGS
`#define NR_ARG_REGS	'NR_ARG_REGS


`#ifdef ASM'
/*
 * Only assembler stuff from here on (when included from *.S)
 */

/*
 * Handle 32 vs 64-bit.
 */
ifelse(ARCH,ppc64,`
/* 64-bit PowerPC */
define(LOAD,ld)dnl
define(STORE,std)dnl
define(CMPI,cmpdi)dnl
define(WSIZE,8)dnl
`#define STORE_IA(ADDR, DST, TMP)  \
	addis TMP, 0, ADDR@highest 	SEMI\
	ori TMP, TMP, ADDR@higher  	SEMI\
	rldicr TMP, TMP, 32, 31 	SEMI\
	oris TMP, TMP, ADDR@h 		SEMI\
	ori TMP, TMP, ADDR@l		SEMI\
	std TMP, DST'
',`
/* 32-bit PowerPC */
define(LOAD,lwz)dnl
define(STORE,stw)dnl
define(CMPI,cmpwi)dnl
define(WSIZE,4)dnl
`#define STORE_IA(ADDR, DST, TMP)   \
	lis TMP, ADDR@ha 	SEMI\
	addi TMP, TMP, ADDR@l	SEMI\
	stw TMP, DST'
')dnl
`#define LOAD	'LOAD
`#define STORE	'STORE
`#define CMPI	'CMPI


/*
 * Workarounds for Darwin.
 */
ifelse(OPSYS,darwin,``
/* Darwin */
#define JOIN(X,Y)	X##Y
#define CSYM(NAME)	JOIN(_,NAME)
#define ASYM(NAME)	CSYM(NAME)
#define GLOBAL(NAME)	.globl NAME
#define SEMI		@
#define SET_SIZE(NAME)	/*empty*/
#define TYPE_FUNCTION(NAME)	/*empty*/
#define OPD(NAME)	/*empty*/
'',``
/* Not Darwin */''
`ifelse(ARCH,ppc64,``
/* 64-bit */
/*
 * The 64-bit PowerPC ABI requires us to setup Official Procedure Descriptors
 * for functions called from C. These are exported as "func", while the entry
 * point should is exported as ".func". A function pointer in C points to the
 * function descriptor in the opd rather than to the function entry point.
 */
#define JOIN(X,Y)	X##Y
#define CSYM(NAME)	JOIN(.,NAME)
#define OPD(NAME)       			\
	.pushsection .opd, "aw";		\
	.align 3;				\
	.global NAME;				\
NAME:						\
	.quad CSYM(NAME), .TOC.@tocbase, 0;	\
	.type NAME, @function;			\
	.popsection
'',``
/* 32-bit */
#define CSYM(NAME)	NAME
#define OPD(NAME)	/*empty*/
'')'
``#define ASYM(NAME)	NAME
#define GLOBAL(NAME)	.global NAME
#define SEMI		;
#define SET_SIZE(NAME)	.size NAME,.-NAME
#define TYPE_FUNCTION(NAME)	.type NAME,@function
#define lo16(X)		X@l
#define ha16(X)		X@ha

/*
 * Standard register names.
 */
#define r0	0
#define r1	1
#define r2	2
#define r3	3
#define r4	4
#define r5	5
#define r6	6
#define r7	7
#define r8	8
#define r9	9
#define r10	10
#define r11	11
#define r12	12
#define r13	13
#define r14	14
#define r15	15
#define r16	16
#define r17	17
#define r18	18
#define r19	19
#define r20	20
#define r21	21
#define r22	22
#define r23	23
#define r24	24
#define r25	25
#define r26	26
#define r27	27
#define r28	28
#define r29	29
#define r30	30
#define r31	31
'')dnl

/*
 * Reserved registers.
 */
`#define P	r31'
`#define NSP	r30'
`#define HP	r29'
`#define TEMP_LR	r28'

/*
 * Context switching macros.
 *
 * RESTORE_CONTEXT and RESTORE_CONTEXT_QUICK do not affect
 * the condition register.
 */
`#define SAVE_CONTEXT_QUICK	\
	mflr	TEMP_LR'

`#define RESTORE_CONTEXT_QUICK	\
	mtlr	TEMP_LR'

`#define SAVE_CACHED_STATE	\
	STORE	HP, P_HP(P) SEMI\
	STORE	NSP, P_NSP(P)'

`#define RESTORE_CACHED_STATE	\
	LOAD	HP, P_HP(P) SEMI\
	LOAD	NSP, P_NSP(P)'

`#define SAVE_CONTEXT_BIF	\
	mflr	TEMP_LR SEMI	\
	STORE	HP, P_HP(P)'

`#define RESTORE_CONTEXT_BIF	\
	mtlr	TEMP_LR SEMI	\
	LOAD	HP, P_HP(P)'

`#define SAVE_CONTEXT_GC	\
	mflr	TEMP_LR SEMI	\
	STORE	TEMP_LR, P_NRA(P) SEMI	\
	STORE	NSP, P_NSP(P) SEMI	\
	STORE	HP, P_HP(P)'

`#define RESTORE_CONTEXT_GC	\
	mtlr	TEMP_LR SEMI	\
	LOAD	HP, P_HP(P)'

/*
 * Argument (parameter) registers.
 */

define(defarg,`define(ARG$1,`$2')dnl
#`define ARG'$1	$2'
)dnl

ifelse(eval(NR_ARG_REGS >= 1),0,,
`defarg(0,`r4')')dnl
ifelse(eval(NR_ARG_REGS >= 2),0,,
`defarg(1,`r5')')dnl
ifelse(eval(NR_ARG_REGS >= 3),0,,
`defarg(2,`r6')')dnl
ifelse(eval(NR_ARG_REGS >= 4),0,,
`defarg(3,`r7')')dnl
ifelse(eval(NR_ARG_REGS >= 5),0,,
`defarg(4,`r8')')dnl
ifelse(eval(NR_ARG_REGS >= 6),0,,
`defarg(5,`r9')')dnl

/*
 * TEMP_ARG0:
 *	Used in nbif_stack_trap_ra to preserve the return value.
 *	Must be a C callee-save register.
 *	Must be otherwise unused in the return path.
 *
 * TEMP_ARG0:
 *	Used in hipe_ppc_inc_stack to preserve the return address
 *	(TEMP_LR contains the caller's saved return address).
 *	Must be a C callee-save register.
 *	Must be otherwise unused in the call path.
 */
`#define TEMP_ARG0	r27'

dnl XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
dnl X								X
dnl X			hipe_ppc_glue.S support			X
dnl X								X
dnl XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

dnl
dnl LOAD_ARG_REGS
dnl
define(LAR_1,`LOAD ARG$1, P_ARG$1(P) SEMI ')dnl
define(LAR_N,`ifelse(eval($1 >= 0),0,,`LAR_N(eval($1-1))LAR_1($1)')')dnl
define(LOAD_ARG_REGS,`LAR_N(eval(NR_ARG_REGS-1))')dnl
`#define LOAD_ARG_REGS	'LOAD_ARG_REGS

dnl
dnl STORE_ARG_REGS
dnl
define(SAR_1,`STORE ARG$1, P_ARG$1(P) SEMI ')dnl
define(SAR_N,`ifelse(eval($1 >= 0),0,,`SAR_N(eval($1-1))SAR_1($1)')')dnl
define(STORE_ARG_REGS,`SAR_N(eval(NR_ARG_REGS-1))')dnl
`#define STORE_ARG_REGS	'STORE_ARG_REGS

dnl XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
dnl X								X
dnl X			hipe_ppc_bifs.m4 support		X
dnl X								X
dnl XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

dnl
dnl NBIF_ARG(DST,ARITY,ARGNO)
dnl Access a formal parameter.
dnl It will be a memory load via NSP when ARGNO >= NR_ARG_REGS.
dnl It will be a register move when 0 <= ARGNO < NR_ARG_REGS; if
dnl the source and destination are the same, the move is suppressed.
dnl
define(NBIF_MOVE_REG,`ifelse($1,$2,`# mr $1, $2',`mr	$1, $2')')dnl
define(NBIF_REG_ARG,`NBIF_MOVE_REG($1,ARG$2)')dnl
define(NBIF_STK_LOAD,`LOAD	$1, $2(NSP)')dnl
define(NBIF_STK_ARG,`NBIF_STK_LOAD($1,eval(WSIZE*(($2-$3)-1)))')dnl
define(NBIF_ARG,`ifelse(eval($3 >= NR_ARG_REGS),0,`NBIF_REG_ARG($1,$3)',`NBIF_STK_ARG($1,$2,$3)')')dnl
`/* #define NBIF_ARG_1_0	'NBIF_ARG(r3,1,0)` */'
`/* #define NBIF_ARG_2_0	'NBIF_ARG(r3,2,0)` */'
`/* #define NBIF_ARG_2_1	'NBIF_ARG(r3,2,1)` */'
`/* #define NBIF_ARG_3_0	'NBIF_ARG(r3,3,0)` */'
`/* #define NBIF_ARG_3_1	'NBIF_ARG(r3,3,1)` */'
`/* #define NBIF_ARG_3_2	'NBIF_ARG(r3,3,2)` */'
`/* #define NBIF_ARG_4_0	'NBIF_ARG(r3,4,0)` */'
`/* #define NBIF_ARG_4_1	'NBIF_ARG(r3,4,1)` */'
`/* #define NBIF_ARG_4_2	'NBIF_ARG(r3,4,2)` */'
`/* #define NBIF_ARG_4_3	'NBIF_ARG(r3,4,3)` */'
`/* #define NBIF_ARG_5_0	'NBIF_ARG(r3,5,0)` */'
`/* #define NBIF_ARG_5_1	'NBIF_ARG(r3,5,1)` */'
`/* #define NBIF_ARG_5_2	'NBIF_ARG(r3,5,2)` */'
`/* #define NBIF_ARG_5_3	'NBIF_ARG(r3,5,3)` */'
`/* #define NBIF_ARG_5_4	'NBIF_ARG(r3,5,4)` */'

dnl
dnl NBIF_RET(ARITY)
dnl Generates a return from a native BIF, taking care to pop
dnl any stacked formal parameters.
dnl
define(NSP_RETN,`addi	NSP, NSP, $1
	blr')dnl
define(NSP_RET0,`blr')dnl
define(RET_POP,`ifelse(eval($1 > NR_ARG_REGS),0,0,eval(WSIZE*($1 - NR_ARG_REGS)))')dnl
define(NBIF_RET_N,`ifelse(eval($1),0,`NSP_RET0',`NSP_RETN($1)')')dnl
define(NBIF_RET,`NBIF_RET_N(eval(RET_POP($1)))')dnl
`/* #define NBIF_RET_0	'NBIF_RET(0)` */'
`/* #define NBIF_RET_1	'NBIF_RET(1)` */'
`/* #define NBIF_RET_2	'NBIF_RET(2)` */'
`/* #define NBIF_RET_3	'NBIF_RET(3)` */'
`/* #define NBIF_RET_4	'NBIF_RET(4)` */'
`/* #define NBIF_RET_5	'NBIF_RET(5)` */'

dnl
dnl QUICK_CALL_RET(CFUN,ARITY)
dnl Used in nocons_nofail and noproc primop interfaces to optimise
dnl SAVE_CONTEXT_QUICK; bl CFUN; RESTORE_CONTEXT_QUICK; NBIF_RET(ARITY).
dnl
define(NBIF_POP_N,`ifelse(eval($1),0,`',`addi NSP, NSP, $1 SEMI ')')dnl
define(QUICK_CALL_RET,`NBIF_POP_N(eval(RET_POP($2)))b $1')dnl
`/* #define QUICK_CALL_RET_F_0 'QUICK_CALL_RET(F,0)` */'
`/* #define QUICK_CALL_RET_F_1 'QUICK_CALL_RET(F,1)` */'
`/* #define QUICK_CALL_RET_F_2 'QUICK_CALL_RET(F,2)` */'
`/* #define QUICK_CALL_RET_F_3 'QUICK_CALL_RET(F,3)` */'
`/* #define QUICK_CALL_RET_F_5 'QUICK_CALL_RET(F,5)` */'

`#endif /* ASM */'

`#endif /* HIPE_PPC_ASM_H */'
