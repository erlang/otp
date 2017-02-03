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

#include "hipe_arch.h"
#include "hipe_native_bif.h"	/* nbif_callemu() */

/* Flush dcache and invalidate icache for a range of addresses. */
void hipe_flush_icache_range(void *address, unsigned int nbytes)
{
    char *a = (char*)address;
    int n = nbytes;

    while (n > 0) {
	hipe_flush_icache_word(a);
	a += 4;
	n -= 4;
    }
}

static void patch_sethi(Uint32 *address, unsigned int imm22)
{
    unsigned int insn = *address;
    *address = (insn & 0xFFC00000) | (imm22 & 0x003FFFFF);
    hipe_flush_icache_word(address);
}

static void patch_ori(Uint32 *address, unsigned int imm10)
{
    /* address points to an OR reg,imm,reg insn */
    unsigned int insn = *address;
    *address = (insn & 0xFFFFE000) | (imm10 & 0x3FF);
    hipe_flush_icache_word(address);
}

static void patch_sethi_ori(Uint32 *address, Uint32 value)
{
    patch_sethi(address, value >> 10);
    patch_ori(address+1, value);
}

void hipe_patch_load_fe(Uint32 *address, Uint32 value)
{
    patch_sethi_ori(address, value);
}

int hipe_patch_insn(void *address, Uint32 value, Eterm type)
{
    switch (type) {
      case am_load_mfa:
      case am_atom:
      case am_constant:
      case am_closure:
      case am_c_const:
	break;
      default:
	return -1;
    }
    patch_sethi_ori((Uint32*)address, value);
    return 0;
}

int hipe_patch_call(void *callAddress, void *destAddress, void *trampoline)
{
    Uint32 relDest, newI;

    ASSERT(trampoline == NULL);

    relDest = (Uint32)((Sint32)destAddress - (Sint32)callAddress);
    newI = (1 << 30) | (relDest >> 2);
    *(Uint32*)callAddress = newI;
    hipe_flush_icache_word(callAddress);
    return 0;
}

static void *alloc_code(unsigned int alloc_bytes)
{
    return erts_alloc(ERTS_ALC_T_HIPE_EXEC, alloc_bytes);
}

void *hipe_alloc_code(Uint nrbytes, Eterm callees, Eterm *trampolines, Process *p)
{
    if (is_not_nil(callees))
	return NULL;
    *trampolines = NIL;
    return alloc_code(nrbytes);
}

void hipe_free_code(void* code, unsigned int nrbytes)
{
    erts_free(ERTS_ALC_T_HIPE_EXEC, code);
}

void *hipe_make_native_stub(void *callee_exp, unsigned int beamArity)
{
    unsigned int *code;
    unsigned int callEmuOffset;
    int i;

    code = alloc_code(5*sizeof(int));
    if (!code)
	return NULL;

    /* sethi %hi(Address), %i4 */
    code[0] = 0x39000000 | (((unsigned int)callee_exp >> 10) & 0x3FFFFF);
    /* or %g0, %o7, %i3 ! mov %o7, %i3 */
    code[1] = 0xB610000F;
    /* or %i4, %lo(Address), %i4 */
    code[2] = 0xB8172000 | ((unsigned int)callee_exp & 0x3FF);
    /* call callemu */
    callEmuOffset = (char*)nbif_callemu - (char*)&code[3];
    code[3] = (1 << 30) | ((callEmuOffset >> 2) & 0x3FFFFFFF);
    /* or %g0, Arity, %i5 ! mov Arity, %i5 */
    code[4] = 0xBA102000 | (beamArity & 0x0FFF);

    /* flush I-cache as if by write_u32() */
    for (i = 0; i < 5; ++i)
	hipe_flush_icache_word(&code[i]);

    return code;
}

void hipe_free_native_stub(void* stub)
{
    erts_free(ERTS_ALC_T_HIPE_EXEC, stub);
}

void hipe_arch_print_pcb(struct hipe_process_state *p)
{
#define U(n,x) \
    printf(" % 4d | %s | 0x%0*lx | %*s |\r\n", (int)offsetof(struct hipe_process_state,x), n, 2*(int)sizeof(long), (unsigned long)p->x, 2+2*(int)sizeof(long), "")
    U("nra        ", nra);
    U("narity     ", narity);
#undef U
}
