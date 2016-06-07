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


#include <stddef.h>	/* offsetof() */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
#include "global.h"
#include "erl_binary.h"
#include <sys/mman.h>

#include "hipe_arch.h"
#include "hipe_native_bif.h"	/* nbif_callemu() */
#include "hipe_bif0.h"

/* Flush dcache and invalidate icache for a range of addresses. */
void hipe_flush_icache_range(void *address, unsigned int nbytes)
{
#if defined(__ARM_EABI__)
    register unsigned long beg __asm__("r0") = (unsigned long)address;
    register unsigned long end __asm__("r1") = (unsigned long)address + nbytes;
    register unsigned long flg __asm__("r2") = 0;
    register unsigned long scno __asm__("r7") = 0xf0002;
    __asm__ __volatile__("swi 0"	/* sys_cacheflush() */
			 : "=r"(beg)
			 : "0"(beg), "r"(end), "r"(flg), "r"(scno));
#else
    register unsigned long beg __asm__("r0") = (unsigned long)address;
    register unsigned long end __asm__("r1") = (unsigned long)address + nbytes;
    register unsigned long flg __asm__("r2") = 0;
    __asm__ __volatile__("swi 0x9f0002"	/* sys_cacheflush() */
			 : "=r"(beg)
			 : "0"(beg), "r"(end), "r"(flg));
#endif
}

void hipe_flush_icache_word(void *address)
{
    hipe_flush_icache_range(address, 4);
}

/*
 * Management of 32MB code segments for regular code and trampolines.
 */

#define SEGMENT_NRBYTES	(32*1024*1024)	/* named constant, _not_ a tunable */

static struct segment {
    unsigned int *base;		/* [base,base+32MB[ */
    unsigned int *code_pos;	/* INV: base <= code_pos <= tramp_pos  */
    unsigned int *tramp_pos;	/* INV: tramp_pos <= base+32MB */
    /* On ARM we always allocate a trampoline at base+32MB-8 for
       nbif_callemu, so tramp_pos <= base+32MB-8. */
} curseg;

#define in_area(ptr,start,nbytes)	\
	((UWord)((char*)(ptr) - (char*)(start)) < (nbytes))

static void *new_code_mapping(void)
{
    return mmap(0, SEGMENT_NRBYTES,
		PROT_EXEC|PROT_READ|PROT_WRITE,
		MAP_PRIVATE|MAP_ANONYMOUS,
		-1, 0);
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

static unsigned int *try_alloc(Uint nrwords, int nrcallees, Eterm callees, unsigned int **trampvec)
{
    unsigned int *base, *address, *tramp_pos, nrfreewords;
    int trampnr;
    Eterm mfa, m, f;
    unsigned int a, *trampoline;

    m = NIL; f = NIL; a = 0; /* silence stupid compiler warning */
    tramp_pos = curseg.tramp_pos;
    address = curseg.code_pos;
    nrfreewords = tramp_pos - address;
    if (nrwords > nrfreewords)
	return NULL;
    curseg.code_pos = address + nrwords;
    nrfreewords -= nrwords;

    base = curseg.base;
    for (trampnr = 1; trampnr <= nrcallees; ++trampnr) {
	mfa = tuple_val(callees)[trampnr];
	if (is_atom(mfa))
	    trampoline = hipe_primop_get_trampoline(mfa);
	else {
	    m = tuple_val(mfa)[1];
	    f = tuple_val(mfa)[2];
	    a = unsigned_val(tuple_val(mfa)[3]);
	    trampoline = hipe_mfa_get_trampoline(m, f, a);
	}
	if (!in_area(trampoline, base, SEGMENT_NRBYTES)) {
	    if (nrfreewords < 2)
		return NULL;
	    nrfreewords -= 2;
	    tramp_pos = trampoline = tramp_pos - 2;
	    trampoline[0] = 0xE51FF004; /* ldr pc, [pc,#-4] */
	    trampoline[1] = 0;		/* callee's address */
	    hipe_flush_icache_range(trampoline, 2*sizeof(int));
	    if (is_atom(mfa))
		hipe_primop_set_trampoline(mfa, trampoline);
	    else
		hipe_mfa_set_trampoline(m, f, a, trampoline);
	}
	trampvec[trampnr-1] = trampoline;
    }
    curseg.tramp_pos = tramp_pos;
    return address;
}

void *hipe_alloc_code(Uint nrbytes, Eterm callees, Eterm *trampolines, Process *p)
{
    Uint nrwords;
    int nrcallees;
    Eterm trampvecbin;
    unsigned int **trampvec;
    unsigned int *address;
    unsigned int *base;
    struct segment oldseg;

    if (nrbytes & 0x3)
	return NULL;
    nrwords = nrbytes >> 2;

    nrcallees = check_callees(callees);
    if (nrcallees < 0)
	return NULL;
    trampvecbin = new_binary(p, NULL, nrcallees*sizeof(unsigned int*));
    trampvec = (unsigned int**)binary_bytes(trampvecbin);

    address = try_alloc(nrwords, nrcallees, callees, trampvec);
    if (!address) {
	base = new_code_mapping();
	if (base == MAP_FAILED)
	    return NULL;
	oldseg = curseg;
	curseg.base = base;
	curseg.code_pos = base;
	curseg.tramp_pos = (unsigned int*)((char*)base + SEGMENT_NRBYTES);
	curseg.tramp_pos -= 2;
	curseg.tramp_pos[0] = 0xE51FF004;	/* ldr pc, [pc,#-4] */
	curseg.tramp_pos[1] = (unsigned int)&nbif_callemu;

	address = try_alloc(nrwords, nrcallees, callees, trampvec);
	if (!address) {
	    munmap(base, SEGMENT_NRBYTES);
	    curseg = oldseg;
	    return NULL;
	}
	/* commit to new segment, ignore leftover space in old segment */
    }
    *trampolines = trampvecbin;
    return address;
}

static unsigned int *alloc_stub(Uint nrwords, unsigned int **tramp_callemu)
{
    unsigned int *address;
    unsigned int *base;
    struct segment oldseg;

    address = try_alloc(nrwords, 0, NIL, NULL);
    if (!address) {
	base = new_code_mapping();
	if (base == MAP_FAILED)
	    return NULL;
	oldseg = curseg;
	curseg.base = base;
	curseg.code_pos = base;
	curseg.tramp_pos = (unsigned int*)((char*)base + SEGMENT_NRBYTES);
	curseg.tramp_pos -= 2;
	curseg.tramp_pos[0] = 0xE51FF004;	/* ldr pc, [pc,#-4] */
	curseg.tramp_pos[1] = (unsigned int)&nbif_callemu;

	address = try_alloc(nrwords, 0, NIL, NULL);
	if (!address) {
	    munmap(base, SEGMENT_NRBYTES);
	    curseg = oldseg;
	    return NULL;
	}
	/* commit to new segment, ignore leftover space in old segment */
    }
    *tramp_callemu = (unsigned int*)((char*)curseg.base + SEGMENT_NRBYTES) - 2;
    return address;
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
    unsigned int *tramp_callemu;
    int callemu_offset;

    /*
     * Native code calls BEAM via a stub looking as follows:
     *
     * mov r0, #beamArity
     * ldr r8, [pc,#0] // callee_exp
     * b nbif_callemu
     * .long callee_exp
     *
     * I'm using r0 and r8 since they aren't used for
     * parameter passing in native code. The branch to
     * nbif_callemu may need to go via a trampoline.
     * (Trampolines are allowed to modify r12, but they don't.)
     */

    code = alloc_stub(4, &tramp_callemu);
    if (!code)
	return NULL;
    callemu_offset = ((int)&nbif_callemu - ((int)&code[2] + 8)) >> 2;
    if (!(callemu_offset >= -0x00800000 && callemu_offset <= 0x007FFFFF)) {
	callemu_offset = ((int)tramp_callemu - ((int)&code[2] + 8)) >> 2;
	if (!(callemu_offset >= -0x00800000 && callemu_offset <= 0x007FFFFF))
	    abort();
    }

    /* mov r0, #beamArity */
    code[0] = 0xE3A00000 | (beamArity & 0xFF);
    /* ldr r8, [pc,#0] // callee_exp */
    code[1] = 0xE59F8000;
    /* b nbif_callemu */
    code[2] = 0xEA000000 | (callemu_offset & 0x00FFFFFF);
    /* .long callee_exp */
    code[3] = (unsigned int)callee_exp;

    hipe_flush_icache_range(code, 4*sizeof(int));

    return code;
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
