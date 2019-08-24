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

#ifdef __arm__

#include <stddef.h>	/* offsetof() */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
#include "global.h"
#include "erl_binary.h"

#include "hipe_arch.h"
#include "hipe_native_bif.h"	/* nbif_callemu() */
#include "hipe_bif0.h"

#ifndef __has_builtin
# define __has_builtin(x) 0
#endif

/* Flush dcache and invalidate icache for a range of addresses. */
void hipe_flush_icache_range(void *address, unsigned int nbytes)
{
    void* end = (char*)address + nbytes;

#if ERTS_AT_LEAST_GCC_VSN__(4, 3, 0) || __has_builtin(__builtin___clear_cache)
    __builtin___clear_cache(address, end);
#elif defined(__clang__)
    void __clear_cache(void *start, void *end);
    __clear_cache(address, end);
#elif defined(__linux__)
# if defined(__ARM_EABI__)
    register unsigned long beg __asm__("r0") = (unsigned long)address;
    register unsigned long end __asm__("r1") = (unsigned long)end;
    register unsigned long flg __asm__("r2") = 0;
    register unsigned long scno __asm__("r7") = 0xf0002;
    __asm__ __volatile__("swi 0"	/* sys_cacheflush() */
			 : "=r"(beg)
			 : "0"(beg), "r"(end), "r"(flg), "r"(scno));
# else
    register unsigned long beg __asm__("r0") = (unsigned long)address;
    register unsigned long end __asm__("r1") = (unsigned long)end;
    register unsigned long flg __asm__("r2") = 0;
    __asm__ __volatile__("swi 0x9f0002"	/* sys_cacheflush() */
			 : "=r"(beg)
			 : "0"(beg), "r"(end), "r"(flg));
# endif
#else
# error "Don't know how to flush instruction cache"
#endif
}

void hipe_flush_icache_word(void *address)
{
    hipe_flush_icache_range(address, 4);
}


static int check_callees(Eterm callees)
{
    Eterm *tuple;
    Uint arity;
    Uint i;

    if (is_not_tuple(callees))
	return -1;
    tuple = tuple_val(callees);
    arity = arityval(tuple[0]);
    for (i = 1; i <= arity; ++i) {
	Eterm mfa = tuple[i];
	if (is_atom(mfa))
	    continue;
	if (is_not_tuple(mfa) ||
	    tuple_val(mfa)[0] != make_arityval(3) ||
	    is_not_atom(tuple_val(mfa)[1]) ||
	    is_not_atom(tuple_val(mfa)[2]) ||
	    is_not_small(tuple_val(mfa)[3]) ||
	    unsigned_val(tuple_val(mfa)[3]) > 255)
	    return -1;
    }
    return arity;
}

#define TRAMPOLINE_WORDS 2

static void generate_trampolines(Uint32* address,
                                 int nrcallees, Eterm callees,
                                 Uint32** trampvec)
{
    Uint32* trampoline = address;
    int i;

    for (i = 0; i < nrcallees; ++i) {
        trampoline[0] = 0xE51FF004;     /* ldr pc, [pc,#-4] */
        trampoline[1] = 0;		/* callee's address */
	trampvec[i] = trampoline;
        trampoline += TRAMPOLINE_WORDS;
    }
    hipe_flush_icache_range(address, nrcallees*2*sizeof(Uint32));
}

void *hipe_alloc_code(Uint nrbytes, Eterm callees, Eterm *trampolines, Process *p)
{
    Uint code_words;
    int nrcallees;
    Eterm trampvecbin;
    Uint32 **trampvec;
    Uint32 *address;

    if (nrbytes & 0x3)
	return NULL;
    code_words = nrbytes / sizeof(Uint32);

    nrcallees = check_callees(callees);
    if (nrcallees < 0)
	return NULL;
    trampvecbin = new_binary(p, NULL, nrcallees*sizeof(Uint32*));
    trampvec = (Uint32**)binary_bytes(trampvecbin);

    address = erts_alloc(ERTS_ALC_T_HIPE_EXEC,
                         (code_words + nrcallees*TRAMPOLINE_WORDS)*sizeof(Uint32));

    generate_trampolines(address + code_words, nrcallees, callees, trampvec);
    *trampolines = trampvecbin;
    return address;
}

void  hipe_free_code(void* code, unsigned int bytes)
{
    erts_free(ERTS_ALC_T_HIPE_EXEC, code);
}

/*
 * ARMv5's support for 32-bit immediates is effectively non-existent.
 * Hence, every 32-bit immediate is stored in memory and loaded via
 * a PC-relative addressing mode. Relocation entries refer to those
 * data words, NOT the load instructions, so patching is trivial.
 */
static void patch_imm32(Uint32 *address, unsigned int imm32)
{
    *address = imm32;
    hipe_flush_icache_word(address);
}

void hipe_patch_load_fe(Uint32 *address, Uint value)
{
    patch_imm32(address, value);
}

int hipe_patch_insn(void *address, Uint32 value, Eterm type)
{
    switch (type) {
      case am_closure:
      case am_constant:
      case am_atom:
      case am_c_const:
	break;
      default:
	return -1;
    }
    patch_imm32((Uint32*)address, value);
    return 0;
}

/* Make stub for native code calling exported beam function
*/
void *hipe_make_native_stub(void *callee_exp, unsigned int beamArity)
{
    unsigned int *code;
    int callemu_offset;
    int is_short_jmp;

    /*
     * Native code calls BEAM via a stub looking as follows:
     *
     * mov r0, #beamArity
     * ldr r8, [pc,#0] // callee_exp
     * b nbif_callemu
     * .long callee_exp
     *
     * or if nbif_callemu is too far away:
     *
     * mov r0, #beamArity
     * ldr r8, [pc,#0] // callee_exp
     * ldr pc, [pc,#0] // nbif_callemu
     * .long callee_exp
     * .long nbif_callemu
     *
     * I'm using r0 and r8 since they aren't used for
     * parameter passing in native code.
     */

    code = erts_alloc(ERTS_ALC_T_HIPE_EXEC, 5*sizeof(Uint32));
    if (!code)
	return NULL;
    callemu_offset = ((int)&nbif_callemu - ((int)&code[2] + 8)) >> 2;
    is_short_jmp = (callemu_offset >= -0x00800000 &&
                    callemu_offset <= 0x007FFFFF);
#ifdef DEBUG
    if (is_short_jmp && (callemu_offset % 3)==0) {
        is_short_jmp = 0;
    }
#endif

    /* mov r0, #beamArity */
    code[0] = 0xE3A00000 | (beamArity & 0xFF);
    /* ldr r8, [pc,#0] // callee_exp */
    code[1] = 0xE59F8000;
    if (is_short_jmp) {
        /* b nbif_callemu */
        code[2] = 0xEA000000 | (callemu_offset & 0x00FFFFFF);
    }
    else {
        /* ldr pc, [pc,#0] // nbif_callemu */
        code[2] = 0xE59FF000;
        /* .long nbif_callemu */
        code[4] = (unsigned int)&nbif_callemu;
    }
    /* .long callee_exp */
    code[3] = (unsigned int)callee_exp;

    hipe_flush_icache_range(code, 5*sizeof(Uint32));

    return code;
}

void hipe_free_native_stub(void* stub)
{
    erts_free(ERTS_ALC_T_HIPE_EXEC, stub);
}

static void patch_b(Uint32 *address, Sint32 offset, Uint32 AA)
{
    Uint32 oldI = *address;
    Uint32 newI = (oldI & 0xFF000000) | (offset & 0x00FFFFFF);
    *address = newI;
    hipe_flush_icache_word(address);
}

int hipe_patch_call(void *callAddress, void *destAddress, void *trampoline)
{
    Sint32 destOffset = ((Sint32)destAddress - ((Sint32)callAddress+8)) >> 2;
    if (destOffset >= -0x800000 && destOffset <= 0x7FFFFF) {
	/* The destination is within a [-32MB,+32MB[ range from us.
	   We can reach it with a b/bl instruction.
	   This is typical for nearby Erlang code. */
	patch_b((Uint32*)callAddress, destOffset, 0);
    } else {
	/* The destination is too distant for b/bl.
	   Must do a b/bl to the trampoline. */
	Sint32 trampOffset = ((Sint32)trampoline - ((Sint32)callAddress+8)) >> 2;
	if (trampOffset >= -0x800000 && trampOffset <= 0x7FFFFF) {
	    /* Update the trampoline's address computation.
	       (May be redundant, but we can't tell.) */
	    patch_imm32((Uint32*)trampoline+1, (Uint32)destAddress);
	    /* Update this call site. */
	    patch_b((Uint32*)callAddress, trampOffset, 0);
	} else
	    return -1;
    }
    return 0;
}

void hipe_arch_print_pcb(struct hipe_process_state *p)
{
#define U(n,x) \
    printf(" % 4d | %s | 0x%0*lx | %*s |\r\n", (int)offsetof(struct hipe_process_state,x), n, 2*(int)sizeof(long), (unsigned long)p->x, 2+2*(int)sizeof(long), "")
    U("nra        ", nra);
    U("narity     ", narity);
#undef U
}

#endif /*__arm__*/
