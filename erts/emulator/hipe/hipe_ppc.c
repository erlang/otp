/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2004-2009. All Rights Reserved.
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
/* $Id$
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

/*
 * Management of 32MB code segments for regular code and trampolines.
 */

#define SEGMENT_NRBYTES	(32*1024*1024)	/* named constant, _not_ a tunable */

static struct segment {
    unsigned int *base;		/* [base,base+32MB[ */
    unsigned int *code_pos;	/* INV: base <= code_pos <= tramp_pos  */
    unsigned int *tramp_pos;	/* INV: tramp_pos <= base+32MB */
} curseg;

#define in_area(ptr,start,nbytes)	\
	((unsigned long)((char*)(ptr) - (char*)(start)) < (nbytes))

/* Darwin breakage */
#if !defined(MAP_ANONYMOUS) && defined(MAP_ANON)
#define MAP_ANONYMOUS MAP_ANON
#endif

#if defined(__powerpc64__)
static void *new_code_mapping(void)
{
    char *map_hint, *map_start;

    /*
     * Allocate a new 32MB code segment in the low 2GB of the address space.
     *
     * This is problematic for several reasons:
     * - Linux/ppc64 lacks the MAP_32BIT flag that Linux/x86-64 has.
     * - The address space hint to mmap is only respected if that
     *   area is available. If it isn't, then mmap falls back to its
     *   defaults, which (according to testing) results in very high
     *   (and thus useless for us) addresses being returned.
     * - Another mapping, presumably the brk, also occupies low addresses.
     *
     * As initial implementation, simply start allocating at the 0.5GB
     * boundary. This leaves plenty of space for the brk before malloc
     * needs to switch to mmap, while allowing for 1.5GB of code.
     *
     * A more robust implementation would be to parse /proc/self/maps,
     * reserve all available space between (say) 0.5GB and 2GB with
     * PROT_NONE MAP_NORESERVE mappings, and then allocate by releasing
     * 32MB segments and re-mapping them properly. This would work on
     * Linux/ppc64, I have no idea how things should be done on Darwin64.
     */
    if (curseg.base)
	map_hint = (char*)curseg.base + SEGMENT_NRBYTES;
    else
	map_hint = (char*)(512*1024*1024); /* 0.5GB */
    map_start = mmap(map_hint, SEGMENT_NRBYTES,
		     PROT_EXEC|PROT_READ|PROT_WRITE,
		     MAP_PRIVATE|MAP_ANONYMOUS,
		     -1, 0);
    if (map_start != MAP_FAILED &&
	(((unsigned long)map_start + (SEGMENT_NRBYTES-1)) & ~0x7FFFFFFFUL)) {
	fprintf(stderr, "mmap with hint %p returned code memory %p\r\n", map_hint, map_start);
	abort();
    }
    return map_start;
}
#else
static void *new_code_mapping(void)
{
    return mmap(0, SEGMENT_NRBYTES,
		PROT_EXEC|PROT_READ|PROT_WRITE,
		MAP_PRIVATE|MAP_ANONYMOUS,
		-1, 0);
}
#endif

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

static unsigned int *try_alloc(Uint nrwords, int nrcallees, Eterm callees, unsigned int **trampvec)
{
    unsigned int *base, *address, *tramp_pos, nrfreewords;
    int trampnr;

    tramp_pos = curseg.tramp_pos;
    address = curseg.code_pos;
    nrfreewords = tramp_pos - address;
    if (nrwords > nrfreewords)
	return NULL;
    curseg.code_pos = address + nrwords;
    nrfreewords -= nrwords;

    base = curseg.base;
    for (trampnr = 1; trampnr <= nrcallees; ++trampnr) {
	Eterm mfa = tuple_val(callees)[trampnr];
	Eterm m = tuple_val(mfa)[1];
	Eterm f = tuple_val(mfa)[2];
	unsigned int a = unsigned_val(tuple_val(mfa)[3]);
	unsigned int *trampoline = hipe_mfa_get_trampoline(m, f, a);
	if (!in_area(trampoline, base, SEGMENT_NRBYTES)) {
	    if (nrfreewords < 4)
		return NULL;
	    nrfreewords -= 4;
	    tramp_pos = trampoline = tramp_pos - 4;
#if defined(__powerpc64__)
	    trampoline[0] = 0x3D600000; /* addis r11,0,0 */
	    trampoline[1] = 0x616B0000; /* ori r11,r11,0 */
#else
	    trampoline[0] = 0x39600000; /* addi r11,r0,0 */
	    trampoline[1] = 0x3D6B0000; /* addis r11,r11,0 */
#endif
	    trampoline[2] = 0x7D6903A6; /* mtctr r11 */
	    trampoline[3] = 0x4E800420; /* bctr */
	    hipe_flush_icache_range(trampoline, 4*sizeof(int));
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

static unsigned int *alloc_stub(Uint nrwords)
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

	address = try_alloc(nrwords, 0, NIL, NULL);
	if (!address) {
	    munmap(base, SEGMENT_NRBYTES);
	    curseg = oldseg;
	    return NULL;
	}
	/* commit to new segment, ignore leftover space in old segment */
    }
    return address;
}

static void patch_imm16(Uint32 *address, unsigned int imm16)
{
    unsigned int insn = *address;
    *address = (insn & ~0xFFFF) | (imm16 & 0xFFFF);
    hipe_flush_icache_word(address);
}

#if defined(__powerpc64__)
static void patch_li64(Uint32 *address, Uint64 value)
{
    patch_imm16(address+0, value >> 48);/* addis r,0,value@highest */
    patch_imm16(address+1, value >> 32);/* ori r,r,value@higher */
    /* sldi r,r,32 */
    patch_imm16(address+3, value >> 16);/* oris r,r,value@h */
    patch_imm16(address+4, value);	/* ori r,r,value@l */
}

static int patch_li31(Uint32 *address, Uint32 value)
{
    if ((value >> 31) != 0)
	return -1;
    patch_imm16(address, value >> 16);	/* addis r,0,value@h */
    patch_imm16(address+1, value);	/* ori r,r,value@l */
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
	patch_li64((Uint32*)address, value);
	return 0;
      case am_atom:
      case am_c_const:
	return patch_li31((Uint32*)address, value);
      default:
	return -1;
    }
}

void *hipe_make_native_stub(void *beamAddress, unsigned int beamArity)
{
    unsigned int *code;

    if ((unsigned long)&nbif_callemu & ~0x01FFFFFCUL)
	abort();

    code = alloc_stub(7);

    /* addis r12,0,beamAddress@highest */
    code[0] = 0x3d800000 | (((unsigned long)beamAddress >> 48) & 0xffff);
    /* ori r12,r12,beamAddress@higher */
    code[1] = 0x618c0000 | (((unsigned long)beamAddress >> 32) & 0xffff);
    /* sldi r12,r12,32 (rldicr r12,r12,32,31) */
    code[2] = 0x798c07c6;
    /* oris r12,r12,beamAddress@h */
    code[3] = 0x658c0000 | (((unsigned long)beamAddress >> 16) & 0xffff);
    /* ori r12,r12,beamAddress@l */
    code[4] = 0x618c0000 | ((unsigned long)beamAddress & 0xffff);
    /* addi r0,0,beamArity */
    code[5] = 0x38000000 | (beamArity & 0x7FFF);
    /* ba nbif_callemu */
    code[6] = 0x48000002 | (unsigned long)&nbif_callemu;

    hipe_flush_icache_range(code, 7*sizeof(int));

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

/* called from hipe_bif0.c:hipe_bifs_make_native_stub_2()
   and hipe_bif0.c:hipe_make_stub() */
void *hipe_make_native_stub(void *beamAddress, unsigned int beamArity)
{
    unsigned int *code;

    /*
     * Native code calls BEAM via a stub looking as follows:
     *
     * addi r12,0,beamAddress@l
     * addi r0,0,beamArity
     * addis r12,r12,beamAddress@ha
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

    code = alloc_stub(4);

    /* addi r12,0,beamAddress@l */
    code[0] = 0x39800000 | ((unsigned long)beamAddress & 0xFFFF);
    /* addi r0,0,beamArity */
    code[1] = 0x38000000 | (beamArity & 0x7FFF);
    /* addis r12,r12,beamAddress@ha */
    code[2] = 0x3D8C0000 | at_ha((unsigned long)beamAddress);
    /* ba nbif_callemu */
    code[3] = 0x48000002 | (unsigned long)&nbif_callemu;

    hipe_flush_icache_range(code, 4*sizeof(int));

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
    if ((Uint32)destAddress == ((Uint32)destAddress & 0x01FFFFFC)) {
	/* The destination is in the [0,32MB[ range.
	   We can reach it with a ba/bla instruction.
	   This is the typical case for BIFs and primops.
	   It's also common for trap-to-BEAM stubs (on ppc32). */
	patch_b((Uint32*)callAddress, (Uint32)destAddress >> 2, 2);
    } else {
	Sint32 destOffset = ((Sint32)destAddress - (Sint32)callAddress) >> 2;
	if (destOffset >= -0x800000 && destOffset <= 0x7FFFFF) {
	    /* The destination is within a [-32MB,+32MB[ range from us.
	       We can reach it with a b/bl instruction.
	       This is typical for nearby Erlang code. */
	    patch_b((Uint32*)callAddress, destOffset, 0);
	} else {
	    /* The destination is too distant for b/bl/ba/bla.
	       Must do a b/bl to the trampoline. */
	    Sint32 trampOffset = ((Sint32)trampoline - (Sint32)callAddress) >> 2;
	    if (trampOffset >= -0x800000 && trampOffset <= 0x7FFFFF) {
		/* Update the trampoline's address computation.
		   (May be redundant, but we can't tell.) */
#if defined(__powerpc64__)
		/* This relies on the fact that we allocate code below 2GB. */
		patch_li31((Uint32*)trampoline, (Uint32)destAddress);
#else
		patch_li((Uint32*)trampoline, (Uint32)destAddress);
#endif
		/* Update this call site. */
		patch_b((Uint32*)callAddress, trampOffset, 0);
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
