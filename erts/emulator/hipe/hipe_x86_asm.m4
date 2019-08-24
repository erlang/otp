changecom(`/*', `*/')dnl
/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2002-2016. All Rights Reserved.
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


`#ifndef HIPE_X86_ASM_H
#define HIPE_X86_ASM_H'

/*
 * Tunables.
 */
define(LEAF_WORDS,24)dnl number of stack words for leaf functions
define(NR_ARG_REGS,3)dnl admissible values are 0 to 5, inclusive
define(HP_IN_ESI,1)dnl change to 0 to not reserve a global register for HP
define(SIMULATE_NSP,0)dnl change to 1 to simulate call/ret insns

`#define X86_LEAF_WORDS	'LEAF_WORDS
`#define LEAF_WORDS	'LEAF_WORDS

`#define X86_NR_ARG_REGS	'NR_ARG_REGS
`#define NR_ARG_REGS	'NR_ARG_REGS

`#define X86_HP_IN_ESI	'HP_IN_ESI
`#define X86_SIMULATE_NSP	'SIMULATE_NSP


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
`#define P	%ebp'

`#if X86_HP_IN_ESI
#define SAVE_HP		movl %esi, P_HP(P)
#define RESTORE_HP	movl P_HP(P), %esi
#else
#define SAVE_HP		/*empty*/
#define RESTORE_HP	/*empty*/
#endif'

`#define NSP		%esp
#define SAVE_CSP	movl %esp, P_CSP(P)
#define RESTORE_CSP	movl P_CSP(P), %esp'


/*
 * Context switching macros.
 */
`#define SWITCH_C_TO_ERLANG_QUICK	\
	SAVE_CSP; \
	movl P_NSP(P), NSP'

`#define SWITCH_ERLANG_TO_C_QUICK	\
	movl NSP, P_NSP(P); \
	RESTORE_CSP'

`#define SAVE_CACHED_STATE	\
	SAVE_HP'

`#define RESTORE_CACHED_STATE	\
	RESTORE_HP'

`#define SWITCH_C_TO_ERLANG	\
	RESTORE_CACHED_STATE;	\
	SWITCH_C_TO_ERLANG_QUICK'

`#define SWITCH_ERLANG_TO_C	\
	SAVE_CACHED_STATE;	\
	SWITCH_ERLANG_TO_C_QUICK'


/*
 * Argument (parameter) registers.
 */
ifelse(eval(NR_ARG_REGS >= 1),0,,
``#define ARG0	%eax
'')dnl
ifelse(eval(NR_ARG_REGS >= 2),0,,
``#define ARG1	%edx
'')dnl
ifelse(eval(NR_ARG_REGS >= 3),0,,
``#define ARG2	%ecx
'')dnl
ifelse(eval(NR_ARG_REGS >= 4),0,,
``#define ARG3	%ebx
'')dnl
ifelse(eval(NR_ARG_REGS >= 5),0,,
``#define ARG4	%edi
'')dnl

/*
 * TEMP_RV:
 *	Used in nbif_stack_trap_ra to preserve the return value.
 *	Must be a C callee-save register.
 *	Must be otherwise unused in the return path.
 */
`#define TEMP_RV	%ebx'

/*
 * TEMP_NSP:
 *	Used in BIF wrappers to permit copying stacked parameter from
 *	the native stack to the C stack.
 *	Set up by NBIF_COPY_NSP(arity) and used by NBIF_ARG(arity,argno).
 *	TEMP_NSP may alias the last BIF argument register.
 *	NBIF_COPY_NSP and NBIF_ARG currently fail if ARITY > NR_ARG_REGS!
 */
`#define TEMP_NSP	%edi'

dnl XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
dnl X								X
dnl X			hipe_x86_glue.S support			X
dnl X								X
dnl XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

dnl
dnl LOAD_ARG_REGS
dnl
define(LAR_1,`movl P_ARG$1(P), ARG$1 ; ')dnl
define(LAR_N,`ifelse(eval($1 >= 0),0,,`LAR_N(eval($1-1))LAR_1($1)')')dnl
define(LOAD_ARG_REGS,`LAR_N(eval(NR_ARG_REGS-1))')dnl
`#define LOAD_ARG_REGS	'LOAD_ARG_REGS

dnl
dnl STORE_ARG_REGS
dnl
define(SAR_1,`movl ARG$1, P_ARG$1(P) ; ')dnl
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
``#define NSP_CALL(FUN)	subl $4,NSP; movl $1f,(NSP); jmp FUN; 1:'')dnl

dnl
dnl NSP_RETN(NPOP)
dnl Emit a RET $NPOP instruction, or simulate it.
dnl NPOP should be non-zero.
dnl
ifelse(eval(SIMULATE_NSP),0,
``#define NSP_RETN(NPOP)	ret $NPOP'',
``#define NSP_RETN(NPOP)	movl (NSP),TEMP_RV; addl $4+NPOP,NSP; jmp *TEMP_RV'')dnl

dnl
dnl NSP_RET0
dnl Emit a RET instruction, or simulate it.
dnl
ifelse(eval(SIMULATE_NSP),0,
``#define NSP_RET0	ret'',
``#define NSP_RET0	movl (NSP),TEMP_RV; addl $4,NSP; jmp *TEMP_RV'')dnl

dnl XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
dnl X								X
dnl X			hipe_x86_bifs.m4 support		X
dnl X								X
dnl XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

dnl
dnl NBIF_COPY_NSP(ARITY)
dnl if ARITY > NR_ARG_REGS then TEMP_NSP := %esp.
dnl Allows the stacked formals to be referenced via TEMP_NSP after the stack switch.
dnl
define(NBIF_COPY_NSP,`ifelse(eval($1 > NR_ARG_REGS),0,,`movl	%esp, TEMP_NSP')')dnl
`/* #define NBIF_COPY_NSP_0	'NBIF_COPY_NSP(0)` */'
`/* #define NBIF_COPY_NSP_1	'NBIF_COPY_NSP(1)` */'
`/* #define NBIF_COPY_NSP_2	'NBIF_COPY_NSP(2)` */'
`/* #define NBIF_COPY_NSP_3	'NBIF_COPY_NSP(3)` */'
`/* #define NBIF_COPY_NSP_4	'NBIF_COPY_NSP(4)` */'
`/* #define NBIF_COPY_NSP_5	'NBIF_COPY_NSP(5)` */'

dnl
dnl BASE_OFFSET(N)
dnl Generates a base-register offset operand for the value N.
dnl When N is zero the offset becomes the empty string, as this
dnl may allow the assembler to choose a more compat encoding.
dnl
define(BASE_OFFSET,`ifelse(eval($1),0,`',`$1')')dnl

dnl
dnl NBIF_ARG_OPND(ARITY,ARGNO)
dnl Generates an operand for this formal parameter.
dnl It will be a register operand when 0 <= ARGNO < NR_ARG_REGS.
dnl It will be a memory operand via TEMP_NSP when ARGNO >= NR_ARG_REGS.
dnl
define(NBIF_ARG_OPND,`ifelse(eval($2 >= NR_ARG_REGS),0,`ARG'$2,BASE_OFFSET(eval(($1-NR_ARG_REGS)*4-($2-NR_ARG_REGS)*4))`(TEMP_NSP)')')dnl
`/* #define NBIF_ARG_OPND_1_0	'NBIF_ARG_OPND(1,0)` */'
`/* #define NBIF_ARG_OPND_2_0	'NBIF_ARG_OPND(2,0)` */'
`/* #define NBIF_ARG_OPND_2_1	'NBIF_ARG_OPND(2,1)` */'
`/* #define NBIF_ARG_OPND_3_0	'NBIF_ARG_OPND(3,0)` */'
`/* #define NBIF_ARG_OPND_3_1	'NBIF_ARG_OPND(3,1)` */'
`/* #define NBIF_ARG_OPND_3_2	'NBIF_ARG_OPND(3,2)` */'
`/* #define NBIF_ARG_OPND_4_0	'NBIF_ARG_OPND(4,0)` */'
`/* #define NBIF_ARG_OPND_4_1	'NBIF_ARG_OPND(4,1)` */'
`/* #define NBIF_ARG_OPND_4_2	'NBIF_ARG_OPND(4,2)` */'
`/* #define NBIF_ARG_OPND_4_3	'NBIF_ARG_OPND(4,3)` */'
`/* #define NBIF_ARG_OPND_5_0	'NBIF_ARG_OPND(5,0)` */'
`/* #define NBIF_ARG_OPND_5_1	'NBIF_ARG_OPND(5,1)` */'
`/* #define NBIF_ARG_OPND_5_2	'NBIF_ARG_OPND(5,2)` */'
`/* #define NBIF_ARG_OPND_5_3	'NBIF_ARG_OPND(5,3)` */'
`/* #define NBIF_ARG_OPND_5_4	'NBIF_ARG_OPND(5,4)` */'

dnl
dnl NBIF_ARG_REG(CARGNO,REG)
dnl Generates code to move REG to C argument number CARGNO.
dnl
define(NBIF_ARG_REG,`movl $2,BASE_OFFSET(eval(4*$1))(%esp)')dnl
`/* #define NBIF_ARG_REG_0_P	'NBIF_ARG_REG(0,P)` */'

dnl
dnl NBIF_ARG(CARGNO,ARITY,ARGNO)
dnl Generates code to move Erlang parameter number ARGNO
dnl in a BIF of arity ARITY to C parameter number CARGNO.
dnl
dnl This must be called after NBIF_COPY_NSP(ARITY).
dnl
dnl NBIF_ARG(_,_,ARGNO2) must be called after NBIF_ARG(_,_,ARGNO1)
dnl if ARGNO2 > ARGNO1. (ARG0 may be reused as a temporary register
dnl for Erlang parameters passed on the stack.)
dnl
define(NBIF_ARG_MEM,`movl NBIF_ARG_OPND($2,$3),%eax; NBIF_ARG_REG($1,%eax)')dnl
define(NBIF_ARG,`ifelse(eval($3 >= NR_ARG_REGS),0,`NBIF_ARG_REG($1,`ARG'$3)',`NBIF_ARG_MEM($1,$2,$3)')')dnl

dnl
dnl NBIF_RET(ARITY)
dnl Generates a return from a native BIF, taking care to pop
dnl any stacked formal parameters.
dnl
define(RET_POP,`ifelse(eval($1 > NR_ARG_REGS),0,0,eval(4*($1 - NR_ARG_REGS)))')dnl
define(NBIF_RET_N,`ifelse(eval($1),0,`NSP_RET0',`NSP_RETN($1)')')dnl
define(NBIF_RET,`NBIF_RET_N(eval(RET_POP($1)))')dnl
`/* #define NBIF_RET_0	'NBIF_RET(0)` */'
`/* #define NBIF_RET_1	'NBIF_RET(1)` */'
`/* #define NBIF_RET_2	'NBIF_RET(2)` */'
`/* #define NBIF_RET_3	'NBIF_RET(3)` */'
`/* #define NBIF_RET_4	'NBIF_RET(4)` */'
`/* #define NBIF_RET_5	'NBIF_RET(5)` */'

dnl
dnl STORE_CALLER_SAVE
dnl LOAD_CALLER_SAVE
dnl Used to save and restore C caller-save argument registers around
dnl calls to hipe_inc_nstack. The first 3 arguments registers are C 
dnl caller-save, remaining ones are C callee-save.
dnl
define(NBIF_MIN,`ifelse(eval($1 > $2),0,$1,$2)')dnl
define(NR_CALLER_SAVE,NBIF_MIN(NR_ARG_REGS,3))dnl
define(STORE_CALLER_SAVE,`SAR_N(eval(NR_CALLER_SAVE-1))')dnl
define(LOAD_CALLER_SAVE,`LAR_N(eval(NR_CALLER_SAVE-1))')dnl
`#define STORE_CALLER_SAVE	'STORE_CALLER_SAVE
`#define LOAD_CALLER_SAVE	'LOAD_CALLER_SAVE

`#endif /* ASM */'

`#endif /* HIPE_X86_ASM_H */'
