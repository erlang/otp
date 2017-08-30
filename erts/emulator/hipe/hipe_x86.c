/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2003-2016. All Rights Reserved.
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
#include <sys/mman.h>

#include "hipe_arch.h"
#include "hipe_native_bif.h"	/* nbif_callemu() */

#undef F_TIMO
#undef THE_NON_VALUE
#undef ERL_FUN_SIZE
#include "hipe_literals.h"

void hipe_patch_load_fe(Uint32 *address, Uint32 value)
{
    /* address points to a disp32 or imm32 operand */
    *address += value;
}

int hipe_patch_insn(void *address, Uint32 value, Eterm type)
{
    switch (type) {
      case am_closure:
      case am_constant:
      case am_atom:
      case am_c_const:
	break;
      case am_x86_abs_pcrel:
	value += (Uint)address;
	break;
      default:
	return -1;
    }
    *(Uint32*)address += value;
    return 0;
}

int hipe_patch_call(void *callAddress, void *destAddress, void *trampoline)
{
    Uint rel32;

    if (trampoline)
	return -1;
    rel32 = (Uint)destAddress - (Uint)callAddress - 4;
    *(Uint32*)callAddress = rel32;
    hipe_flush_icache_word(callAddress);
    return 0;
}

/*
 * Memory allocator for executable code.
 *
 * This is required on x86 because some combinations
 * of Linux kernels and CPU generations default to
 * non-executable memory mappings, causing ordinary
 * malloc() memory to be non-executable.
 */
static unsigned int code_bytes;
static char *code_next;

#if 0	/* change to non-zero to get allocation statistics at exit() */
static unsigned int total_mapped, nr_joins, nr_splits, total_alloc, nr_allocs, nr_large, total_lost;
static unsigned int atexit_done;

static void alloc_code_stats(void)
{
    printf("\r\nalloc_code_stats: %u bytes mapped, %u joins, %u splits, %u bytes allocated, %u average alloc, %u large allocs, %u bytes lost\r\n",
	   total_mapped, nr_joins, nr_splits, total_alloc, nr_allocs ? total_alloc/nr_allocs : 0, nr_large, total_lost);
}

static void atexit_alloc_code_stats(void)
{
    if (!atexit_done) {
	atexit_done = 1;
	(void)atexit(alloc_code_stats);
    }
}

#define ALLOC_CODE_STATS(X)	do{X;}while(0)
#else
#define ALLOC_CODE_STATS(X)	do{}while(0)
#endif

/* FreeBSD 6.1 and Darwin breakage */
#if !defined(MAP_ANONYMOUS) && defined(MAP_ANON)
#define MAP_ANONYMOUS MAP_ANON
#endif

static int morecore(unsigned int alloc_bytes)
{
    unsigned int map_bytes;
    char *map_hint, *map_start;

    /* Page-align the amount to allocate. */
    map_bytes = (alloc_bytes + 4095) & ~4095;

    /* Round up small allocations. */
    if (map_bytes < 1024*1024)
	map_bytes = 1024*1024;
    else
	ALLOC_CODE_STATS(++nr_large);

    /* Create a new memory mapping, ensuring it is executable
       and in the low 2GB of the address space. Also attempt
       to make it adjacent to the previous mapping. */
    map_hint = code_next + code_bytes;
    if ((unsigned long)map_hint & 4095)
	abort();
    map_start = mmap(map_hint, map_bytes,
		     PROT_EXEC|PROT_READ|PROT_WRITE,
		     MAP_PRIVATE|MAP_ANONYMOUS
#ifdef __x86_64__
		     |MAP_32BIT
#endif
		     ,
		     -1, 0);
    if (map_start == MAP_FAILED)
	return -1;

    ALLOC_CODE_STATS(total_mapped += map_bytes);

    /* Merge adjacent mappings, so the trailing portion of the previous
       mapping isn't lost. In practice this is quite successful. */
    if (map_start == map_hint) {
	ALLOC_CODE_STATS(++nr_joins);
	code_bytes += map_bytes;
    } else {
	ALLOC_CODE_STATS(++nr_splits);
	ALLOC_CODE_STATS(total_lost += code_bytes);
	code_next = map_start;
	code_bytes = map_bytes;
    }

    ALLOC_CODE_STATS(atexit_alloc_code_stats());

    return 0;
}

static void *alloc_code(unsigned int alloc_bytes)
{
    void *res;

    /* Align function entries. */
    alloc_bytes = (alloc_bytes + 3) & ~3;

    if (code_bytes < alloc_bytes && morecore(alloc_bytes) != 0)
	return NULL;
    ALLOC_CODE_STATS(++nr_allocs);
    ALLOC_CODE_STATS(total_alloc += alloc_bytes);
    res = code_next;
    code_next += alloc_bytes;
    code_bytes -= alloc_bytes;
    return res;
}

void *hipe_alloc_code(Uint nrbytes, Eterm callees, Eterm *trampolines, Process *p)
{
    if (is_not_nil(callees))
	return NULL;
    *trampolines = NIL;
    return alloc_code(nrbytes);
}

void *hipe_make_native_stub(void *callee_exp, unsigned int beamArity)
{
    /*
     * This creates a native code stub with the following contents:
     *
     * movl $Address, P_CALLEE_EXP(%ebp)
     * movb $Arity, P_ARITY(%ebp)
     * jmp callemu
     *
     * The stub has variable size, depending on whether the P_CALLEE_EXP
     * and P_ARITY offsets fit in 8-bit signed displacements or not.
     * The rel32 offset in the final jmp depends on its actual location,
     * which also depends on the size of the previous instructions.
     * Arity is stored with a movb because (a) Björn tells me arities
     * are <= 255, and (b) a movb is smaller and faster than a movl.
     */
    unsigned int codeSize;
    unsigned char *code, *codep;
    unsigned int callEmuOffset;

    codeSize =	/* 16, 19, or 22 bytes */
	16 +	/* 16 when both offsets are 8-bit */
	(P_CALLEE_EXP >= 128 ? 3 : 0) +
	(P_ARITY >= 128 ? 3 : 0);
    codep = code = alloc_code(codeSize);
    if (!code)
	return NULL;

    /* movl $beamAddress, P_CALLEE_EXP(%ebp); 3 or 6 bytes, plus 4 */
    codep[0] = 0xc7;
#if P_CALLEE_EXP >= 128
    codep[1] = 0x85;	/* disp32[EBP] */
    codep[2] = P_CALLEE_EXP & 0xFF;
    codep[3] = (P_CALLEE_EXP >> 8) & 0xFF;
    codep[4] = (P_CALLEE_EXP >> 16) & 0xFF;
    codep[5] = (P_CALLEE_EXP >> 24) & 0xFF;
    codep += 6;
#else
    codep[1] = 0x45;	/* disp8[EBP] */
    codep[2] = P_CALLEE_EXP;
    codep += 3;
#endif
    codep[0] = ((unsigned int)callee_exp) & 0xFF;
    codep[1] = ((unsigned int)callee_exp >> 8) & 0xFF;
    codep[2] = ((unsigned int)callee_exp >> 16) & 0xFF;
    codep[3] = ((unsigned int)callee_exp >> 24) & 0xFF;
    codep += 4;

    /* movb $beamArity, P_ARITY(%ebp); 3 or 6 bytes */
    codep[0] = 0xc6;
#if P_ARITY >= 128
    codep[1] = 0x85;	/* disp32[EBP] */
    codep[2] = P_ARITY & 0xFF;
    codep[3] = (P_ARITY >> 8) & 0xFF;
    codep[4] = (P_ARITY >> 16) & 0xFF;
    codep[5] = (P_ARITY >> 24) & 0xFF;
    codep += 6;
#else
    codep[1] = 0x45;	/* disp8[EBP] */
    codep[2] = P_ARITY;
    codep += 3;
#endif
    codep[0] = beamArity;
    codep += 1;

    /* jmp callemu; 5 bytes */
    callEmuOffset = (unsigned char*)nbif_callemu - (code + codeSize);
    codep[0] = 0xe9;
    codep[1] = callEmuOffset & 0xFF;
    codep[2] = (callEmuOffset >> 8) & 0xFF;
    codep[3] = (callEmuOffset >> 16) & 0xFF;
    codep[4] = (callEmuOffset >> 24) & 0xFF;
    codep += 5;
    ASSERT(codep == code + codeSize);

    /* I-cache flush? */

    return code;
}

void hipe_arch_print_pcb(struct hipe_process_state *p)
{
#define U(n,x) \
    printf(" % 4d | %s | 0x%08x |            |\r\n", (int)offsetof(struct hipe_process_state,x), n, (unsigned)p->x)
    U("ncsp       ", ncsp);
    U("narity     ", narity);
#undef U
}
