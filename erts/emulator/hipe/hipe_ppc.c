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


#include <stddef.h>	/* offsetof() */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
#include "global.h"
#include "erl_binary.h"

#include "hipe_arch.h"
#include "hipe_native_bif.h"	/* nbif_callemu() */
#include "hipe_bif0.h"

#if !defined(__powerpc64__)
const unsigned int fconv_constant[2] = { 0x43300000, 0x80000000 };
#endif

/* Flush dcache and invalidate icache for a range of addresses. */
void hipe_flush_icache_range(void *address, unsigned int nbytes)
{
    const unsigned int L1_CACHE_SHIFT = 5;
    const unsigned long L1_CACHE_BYTES = 1 << L1_CACHE_SHIFT;
    unsigned long start, p;
    unsigned int nlines, n;

    if (!nbytes)
	return;

    start = (unsigned long)address & ~(L1_CACHE_BYTES-1);
    nlines =
	(((unsigned long)address & (L1_CACHE_BYTES-1))
	 + nbytes
	 + (L1_CACHE_BYTES-1)) >> L1_CACHE_SHIFT;

    p = start;
    n = nlines;
    do {
	asm volatile("dcbst 0,%0" : : "r"(p) : "memory");
	p += L1_CACHE_BYTES;
    } while (--n != 0);
    asm volatile("sync");
    p = start;
    n = nlines;
    do {
	asm volatile("icbi 0,%0" : : "r"(p) : "memory");
	p += L1_CACHE_BYTES;
    } while (--n != 0);
    asm volatile("sync\n\tisync");
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


static void generate_trampolines(Uint32* address,
                                 int nrcallees, Eterm callees,
                                 Uint32** trampvec)
{
    Uint32* trampoline = address;
    int i;

    for (i = 0; i < nrcallees; ++i) {
#if defined(__powerpc64__)
# define TRAMPOLINE_WORDS 7
        trampoline[0] = 0x3D600000; /* addis r11,r0,0 */
        trampoline[1] = 0x616B0000; /* ori r11,r11,0 */
        trampoline[2] = 0x796B07C6; /* rldicr r11,r11,32,31 */
        trampoline[3] = 0x656B0000; /* oris r11,r11,0 */
        trampoline[4] = 0x616B0000; /* ori r11,r11,0 */
        trampoline[5] = 0x7D6903A6; /* mtctr r11 */
        trampoline[6] = 0x4E800420; /* bctr */
#else
# define TRAMPOLINE_WORDS 4
        trampoline[0] = 0x39600000; /* addi r11,r0,0 */
        trampoline[1] = 0x3D6B0000; /* addis r11,r11,0 */
        trampoline[2] = 0x7D6903A6; /* mtctr r11 */
        trampoline[3] = 0x4E800420; /* bctr */
#endif
        trampvec[i] = trampoline;
        trampoline += TRAMPOLINE_WORDS;
    }
    hipe_flush_icache_range(address, nrcallees*TRAMPOLINE_WORDS*sizeof(Uint32));
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

void hipe_free_code(void* code, unsigned int bytes)
{
    erts_free(ERTS_ALC_T_HIPE_EXEC, code);
}

void hipe_free_native_stub(void* stub)
{
    erts_free(ERTS_ALC_T_HIPE_EXEC, stub);
}

static void patch_imm16(Uint32 *address, unsigned int imm16)
{
    unsigned int insn = *address;
    *address = (insn & ~0xFFFF) | (imm16 & 0xFFFF);
    hipe_flush_icache_word(address);
}

#if defined(__powerpc64__)
/*
 * To load a 64-bit immediate value 'val' into Rd (Rd != R0):
 *
 * addis Rd, 0, val@highest // (val >> 48) & 0xFFFF
 * ori Rd, Rd, val@higher   // (val >> 32) & 0xFFFF
 * rldicr Rd, Rd, 32, 31
 * oris Rd, Rd, val@h       // (val >> 16) & 0xFFFF
 * ori Rd, Rd, val@l        // val & 0xFFFF
 */
static void patch_li64(Uint32 *address, Uint64 value)
{
    patch_imm16(address+0, value >> 48);
    patch_imm16(address+1, value >> 32);
    /* rldicr Rd, Rd, 32, 31 */
    patch_imm16(address+3, value >> 16);
    patch_imm16(address+4, value);
}

void hipe_patch_load_fe(Uint *address, Uint value)
{
    patch_li64((Uint32*)address, value);
}

int hipe_patch_insn(void *address, Uint64 value, Eterm type)
{
    switch (type) {
      case am_closure:
      case am_constant:
      case am_atom:
      case am_c_const:
	patch_li64((Uint32*)address, value);
	return 0;
      default:
	return -1;
    }
}

void *hipe_make_native_stub(void *callee_exp, unsigned int beamArity)
{
    Uint32 *code;

    if ((unsigned long)&nbif_callemu & ~0x01FFFFFCUL)
	abort();

    code = erts_alloc(ERTS_ALC_T_HIPE_EXEC, 7*sizeof(Uint32));
    if (!code)
	return NULL;

    /* addis r12,0,callee_exp@highest */
    code[0] = 0x3d800000 | (((unsigned long)callee_exp >> 48) & 0xffff);
    /* ori r12,r12,callee_exp@higher */
    code[1] = 0x618c0000 | (((unsigned long)callee_exp >> 32) & 0xffff);
    /* sldi r12,r12,32 (rldicr r12,r12,32,31) */
    code[2] = 0x798c07c6;
    /* oris r12,r12,callee_exp@h */
    code[3] = 0x658c0000 | (((unsigned long)callee_exp >> 16) & 0xffff);
    /* ori r12,r12,callee_exp@l */
    code[4] = 0x618c0000 | ((unsigned long)callee_exp & 0xffff);
    /* addi r0,0,beamArity */
    code[5] = 0x38000000 | (beamArity & 0x7FFF);
    /* ba nbif_callemu */
    code[6] = 0x48000002 | (unsigned long)&nbif_callemu;

    hipe_flush_icache_range(code, 7*sizeof(Uint32));

    return code;
}
#else	/* !__powerpc64__ */
/*
 * To load a 32-bit immediate value 'val' into Rd (Rd != R0):
 *
 * addi Rd, 0, val@l	// val & 0xFFFF
 * addis Rd, Rd, val@ha // ((val + 0x8000) >> 16) & 0xFFFF
 *
 * The first addi sign-extends the low 16 bits, so if
 * val&(1<<15), the high portion of Rd will be -1 not 0.
 * val@ha compensates by adding 1 if val&(1<<15).
 */
static unsigned int at_ha(unsigned int val)
{
    return ((val + 0x8000) >> 16) & 0xFFFF;
}

static void patch_li(Uint32 *address, Uint32 value)
{
    patch_imm16(address, value);
    patch_imm16(address+1, at_ha(value));
}

void hipe_patch_load_fe(Uint32 *address, Uint value)
{
    patch_li(address, value);
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
    patch_li((Uint32*)address, value);
    return 0;
}

void *hipe_make_native_stub(void *callee_exp, unsigned int beamArity)
{
    Uint32 *code;

    /*
     * Native code calls BEAM via a stub looking as follows:
     *
     * addi r12,0,callee_exp@l
     * addi r0,0,beamArity
     * addis r12,r12,callee_exp@ha
     * ba nbif_callemu
     *
     * I'm using r0 and r12 since the standard SVR4 ABI allows
     * them to be modified during function linkage. Trampolines
     * (for b/bl to distant targets) may modify r11.
     *
     * The runtime system code is linked completely below the
     * 32MB address boundary. Hence the branch to nbif_callemu
     * is done with a 'ba' instruction.
     */

    /* verify that 'ba' can reach nbif_callemu */
    if ((unsigned long)&nbif_callemu & ~0x01FFFFFCUL)
	abort();

    code = erts_alloc(ERTS_ALC_T_HIPE_EXEC, 4*sizeof(Uint32));
    if (!code)
	return NULL;

    /* addi r12,0,callee_exp@l */
    code[0] = 0x39800000 | ((unsigned long)callee_exp & 0xFFFF);
    /* addi r0,0,beamArity */
    code[1] = 0x38000000 | (beamArity & 0x7FFF);
    /* addis r12,r12,callee_exp@ha */
    code[2] = 0x3D8C0000 | at_ha((unsigned long)callee_exp);
    /* ba nbif_callemu */
    code[3] = 0x48000002 | (unsigned long)&nbif_callemu;

    hipe_flush_icache_range(code, 4*sizeof(Uint32));

    return code;
}
#endif	/* !__powerpc64__ */

static void patch_b(Uint32 *address, Sint32 offset, Uint32 AA)
{
    Uint32 oldI = *address;
    Uint32 newI = (oldI & 0xFC000001) | ((offset & 0x00FFFFFF) << 2) | (AA & 2);
    *address = newI;
    hipe_flush_icache_word(address);
}

int hipe_patch_call(void *callAddress, void *destAddress, void *trampoline)
{
    if ((UWord)destAddress == ((UWord)destAddress & 0x01FFFFFC)) {
	/* The destination is in the [0,32MB[ range.
	   We can reach it with a ba/bla instruction.
	   This is the typical case for BIFs and primops.
	   It's also common for trap-to-BEAM stubs (on ppc32). */
	patch_b((Uint32*)callAddress, (Sint32)destAddress >> 2, 2);
    } else {
	SWord destOffset = ((SWord)destAddress - (SWord)callAddress) >> 2;
	if (destOffset >= -0x800000 && destOffset <= 0x7FFFFF) {
	    /* The destination is within a [-32MB,+32MB[ range from us.
	       We can reach it with a b/bl instruction.
	       This is typical for nearby Erlang code. */
	    patch_b((Uint32*)callAddress, (Sint32)destOffset, 0);
	} else {
	    /* The destination is too distant for b/bl/ba/bla.
	       Must do a b/bl to the trampoline. */
	    SWord trampOffset = ((SWord)trampoline - (SWord)callAddress) >> 2;
	    if (trampOffset >= -0x800000 && trampOffset <= 0x7FFFFF) {
		/* Update the trampoline's address computation.
		   (May be redundant, but we can't tell.) */
#if defined(__powerpc64__)
		patch_li64((Uint32*)trampoline, (Uint64)destAddress);
#else
		patch_li((Uint32*)trampoline, (Uint32)destAddress);
#endif
		/* Update this call site. */
		patch_b((Uint32*)callAddress, (Sint32)trampOffset, 0);
	    } else
		return -1;
	}
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
