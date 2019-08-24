/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2004-2018. All Rights Reserved.
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
#include "error.h"
#include "bif.h"
#include "big.h"	/* term_to_Sint() */
#include "erl_binary.h"

#include "hipe_arch.h"
#include "hipe_bif0.h"
#include "hipe_native_bif.h"	/* nbif_callemu() */

#undef F_TIMO
#undef THE_NON_VALUE
#undef ERL_FUN_SIZE
#include "hipe_literals.h"

static void patch_trampoline(void *trampoline, void *destAddress);

const Uint sse2_fnegate_mask[2] = {0x8000000000000000,0};

void hipe_patch_load_fe(Uint64 *address, Uint64 value)
{
    /* address points to an imm64 operand */
    *address = value;
    hipe_flush_icache_word(address);
}

int hipe_patch_insn(void *address, Uint64 value, Eterm type)
{
    switch (type) {
      case am_closure:
      case am_constant:
      case am_c_const:
	*(Uint64*)address = value;
	break;
      case am_atom:
	/* check that value fits in an unsigned imm32 */
	/* XXX: are we sure it's not really a signed imm32? */
	if ((Uint)(Uint32)value != value)
	    return -1;
	*(Uint32*)address = (Uint32)value;
	break;
      default:
	return -1;
    }
    hipe_flush_icache_word(address);
    return 0;
}

int hipe_patch_call(void *callAddress, void *destAddress, void *trampoline)
{
    Sint64 destOffset = (Sint64)destAddress - (Sint64)callAddress - 4;

    if ((destOffset < -0x80000000L) || (destOffset >= 0x80000000L)) {
        destOffset = (Sint64)trampoline - (Sint64)callAddress - 4;

        if ((destOffset < -0x80000000L) || (destOffset >= 0x80000000L))
            return -1;

        patch_trampoline(trampoline, destAddress);
    }

    *(Uint32*)callAddress = (Uint32)destOffset;
    hipe_flush_icache_word(callAddress);
    return 0;
}

/*
 * Memory allocator for executable code.
 *
 * We use a dedicated allocator for executable code (from OTP 19.0)
 * to make sure the memory we get is executable (PROT_EXEC)
 * and to ensure that executable code ends up in the low 2GB
 * of the address space, as required by HiPE/AMD64's small code model.
 */
static void *alloc_code(unsigned int alloc_bytes)
{
    return erts_alloc(ERTS_ALC_T_HIPE_EXEC, alloc_bytes);
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

#define TRAMPOLINE_BYTES 12

static void generate_trampolines(unsigned char *address,
                                 int nrcallees, Eterm callees,
                                 unsigned char **trampvec)
{
    unsigned char *trampoline = address;
    int i;

    for(i = 0; i < nrcallees; ++i) {
        trampoline[0] = 0x48;           /* movabsq $..., %rax; */
        trampoline[1] = 0xb8;
        *(void**)(trampoline+2) = NULL; /* callee's address */
        trampoline[10] = 0xff;          /* jmpq *%rax */
        trampoline[11] = 0xe0;
        trampvec[i] = trampoline;
        trampoline += TRAMPOLINE_BYTES;
    }
    hipe_flush_icache_range(address, nrcallees*TRAMPOLINE_BYTES);
}

static void patch_trampoline(void *trampoline, void *destAddress)
{
    unsigned char *tp = (unsigned char*) trampoline;

    ASSERT(tp[0] == 0x48 && tp[1] == 0xb8);

    *(void**)(tp+2) = destAddress; /* callee's address */
    hipe_flush_icache_word(tp+2);
}

void *hipe_alloc_code(Uint nrbytes, Eterm callees, Eterm *trampolines, Process *p)
{
    int nrcallees;
    Eterm trampvecbin;
    unsigned char **trampvec;
    unsigned char *address;

    nrcallees = check_callees(callees);
    if (nrcallees < 0)
	return NULL;

    trampvecbin = new_binary(p, NULL, nrcallees*sizeof(unsigned char*));
    trampvec = (unsigned char **)binary_bytes(trampvecbin);

    address = alloc_code(nrbytes + nrcallees*TRAMPOLINE_BYTES);
    generate_trampolines(address + nrbytes, nrcallees, callees, trampvec);
    *trampolines = trampvecbin;
    return address;
}

void hipe_free_code(void* code, unsigned int bytes)
{
    erts_free(ERTS_ALC_T_HIPE_EXEC, code);
}

/* Make stub for native code calling exported beam function.
*/
void *hipe_make_native_stub(void *callee_exp, unsigned int beamArity)
{
    /*
     * This creates a native code stub with the following contents:
     *
     * movq $Address, P_CALLEE_EXP(%ebp)  %% Actually two movl
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

    codeSize =	/* 30, 33, 36, or 39 bytes */
      30 +	/* 30 when all offsets are 8-bit */
      (P_CALLEE_EXP >= 128 ? 3 : 0) +
      ((P_CALLEE_EXP + 4) >= 128 ? 3 : 0) +
      (P_ARITY >= 128 ? 3 : 0);
    codep = code = alloc_code(codeSize);
    if (!code)
	return NULL;

    /* movl $callee_exp, P_CALLEE_EXP(%ebp); 3 or 6 bytes, plus 4 */
    codep[0] = 0xc7;
#if P_CALLEE_EXP >= 128
    codep[1] = 0x85;	/* disp32[EBP] */
    codep[2] =  P_CALLEE_EXP        & 0xFF;
    codep[3] = (P_CALLEE_EXP >>  8) & 0xFF;
    codep[4] = (P_CALLEE_EXP >> 16) & 0xFF;
    codep[5] = (P_CALLEE_EXP >> 24) & 0xFF;
    codep += 6;
#else
    codep[1] = 0x45;	/* disp8[EBP] */
    codep[2] = P_CALLEE_EXP;
    codep += 3;
#endif
    codep[0] = ((unsigned long)callee_exp      ) & 0xFF;
    codep[1] = ((unsigned long)callee_exp >>  8) & 0xFF;
    codep[2] = ((unsigned long)callee_exp >> 16) & 0xFF;
    codep[3] = ((unsigned long)callee_exp >> 24) & 0xFF;
    codep += 4;

    /* movl (shl 32 $callee_exp), P_CALLEE_EXP+4(%ebp); 3 or 6 bytes, plus 4 */
    codep[0] = 0xc7;
#if P_CALLEE_EXP+4 >= 128
    codep[1] = 0x85;	/* disp32[EBP] */
    codep[2] =  (P_CALLEE_EXP+4)        & 0xFF;
    codep[3] = ((P_CALLEE_EXP+4) >>  8) & 0xFF;
    codep[4] = ((P_CALLEE_EXP+4) >> 16) & 0xFF;
    codep[5] = ((P_CALLEE_EXP+4) >> 24) & 0xFF;
    codep += 6;
#else
    codep[1] = 0x45;	/* disp8[EBP] */
    codep[2] =  (P_CALLEE_EXP+4);
    codep += 3;
#endif
    codep[0] = ((unsigned long)callee_exp >> 32) & 0xFF;
    codep[1] = ((unsigned long)callee_exp >> 40) & 0xFF;
    codep[2] = ((unsigned long)callee_exp >> 48) & 0xFF;
    codep[3] = ((unsigned long)callee_exp >> 56) & 0xFF;
    codep += 4;

    /* movb $beamArity, P_ARITY(%ebp); 3 or 6 bytes */
    codep[0] = 0xc6;
#if P_ARITY >= 128
    codep[1] = 0x85;	/* disp32[EBP] */
    codep[2] =  P_ARITY        & 0xFF;
    codep[3] = (P_ARITY >>  8) & 0xFF;
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

    /* jmp callemu; 12 bytes */
    codep[0] = 0x48;
    codep[1] = 0xb8;
    codep += 2;
    *(Uint64*)codep = (Uint64)nbif_callemu;
    codep += 8;
    codep[0] = 0xff;
    codep[1] = 0xe0;
    codep += 2;

    ASSERT(codep == code + codeSize);

    /* I-cache flush? */

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
    U("ncsp       ", ncsp);
    U("narity     ", narity);
#undef U
}
