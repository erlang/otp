/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2001-2016. All Rights Reserved.
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
/*
 * hipe_bif0.c
 *
 * Compiler and linker support.
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
#include "sys.h"
#include "error.h"
#include "erl_vm.h"
#include "global.h"
#include "erl_process.h"
#include "bif.h"
#include "big.h"
#include "beam_load.h"
#include "erl_db.h"
#include "hash.h"
#include "erl_bits.h"
#include "erl_binary.h"
#ifdef HIPE
#include <stddef.h>	/* offsetof() */
#include "hipe_arch.h"
#include "hipe_stack.h"
#include "hipe_mode_switch.h"
#include "hipe_native_bif.h"
#include "hipe_bif0.h"
/* We need hipe_literals.h for HIPE_SYSTEM_CRC, but it redefines
   a few constants. #undef them here to avoid warnings. */
#undef F_TIMO
#undef THE_NON_VALUE
#undef ERL_FUN_SIZE
#include "hipe_literals.h"
#endif

#define BeamOpCode(Op)	((Uint)BeamOp(Op))

int term_to_Sint32(Eterm term, Sint *sp)
{
    Sint val;

    if (!term_to_Sint(term, &val))
	return 0;
    if ((Sint)(Sint32)val != val)
	return 0;
    *sp = val;
    return 1;
}

static Eterm Uint_to_term(Uint x, Process *p)
{
    if (IS_USMALL(0, x)) {
	return make_small(x);
    } else {
	Eterm *hp = HAlloc(p, BIG_UINT_HEAP_SIZE);
	return uint_to_big(x, hp);
    }
}

void *term_to_address(Eterm arg)
{
    Uint u;
    return term_to_Uint(arg, &u) ? (void*)u : NULL;
}

static Eterm address_to_term(const void *address, Process *p)
{
    return Uint_to_term((Uint)address, p);
}

/*
 * BIFs for reading and writing memory. Used internally by HiPE.
 */

BIF_RETTYPE hipe_bifs_write_u8_2(BIF_ALIST_2)
{
    unsigned char *address;

    address = term_to_address(BIF_ARG_1);
    if (!address || is_not_small(BIF_ARG_2))
	BIF_ERROR(BIF_P, BADARG);
    *address = unsigned_val(BIF_ARG_2);
    BIF_RET(NIL);
}

BIF_RETTYPE hipe_bifs_write_u32_2(BIF_ALIST_2)
{
    Uint32 *address;
    Uint value;

    address = term_to_address(BIF_ARG_1);
    if (!address || !hipe_word32_address_ok(address))
	BIF_ERROR(BIF_P, BADARG);
    if (!term_to_Uint(BIF_ARG_2, &value))
	BIF_ERROR(BIF_P, BADARG);
    if ((Uint)(Uint32)value != value)
	BIF_ERROR(BIF_P, BADARG);
    *address = value;
    hipe_flush_icache_word(address);
    BIF_RET(NIL);
}

/*
 * BIFs for mutable bytearrays.
 */
BIF_RETTYPE hipe_bifs_bytearray_2(BIF_ALIST_2)
{
    Sint nelts;
    Eterm bin;

    if (is_not_small(BIF_ARG_1) ||
	(nelts = signed_val(BIF_ARG_1)) < 0 ||
	!is_byte(BIF_ARG_2))
	BIF_ERROR(BIF_P, BADARG);
    bin = new_binary(BIF_P, NULL, nelts);
    memset(binary_bytes(bin), unsigned_val(BIF_ARG_2), nelts);
    BIF_RET(bin);
}

static inline unsigned char *bytearray_lvalue(Eterm bin, Eterm idx)
{
    Sint i;
    unsigned char *bytes;
#ifndef DEBUG
    ERTS_DECLARE_DUMMY(Uint bitoffs);
    ERTS_DECLARE_DUMMY(Uint bitsize);
#else
    Uint bitoffs;
    Uint bitsize;
#endif

    if (is_not_binary(bin) ||
	is_not_small(idx) ||
	(i = unsigned_val(idx)) >= binary_size(bin))
	return NULL;
    ERTS_GET_BINARY_BYTES(bin, bytes, bitoffs, bitsize);
    ASSERT(bitoffs == 0);
    ASSERT(bitsize == 0);
    return bytes + i;
}

BIF_RETTYPE hipe_bifs_bytearray_sub_2(BIF_ALIST_2)
{
    unsigned char *bytep;

    bytep = bytearray_lvalue(BIF_ARG_1, BIF_ARG_2);
    if (!bytep)
	BIF_ERROR(BIF_P, BADARG);
    BIF_RET(make_small(*bytep));
}

BIF_RETTYPE hipe_bifs_bytearray_update_3(BIF_ALIST_3)
{
    unsigned char *bytep;

    bytep = bytearray_lvalue(BIF_ARG_1, BIF_ARG_2);
    if (!bytep || !is_byte(BIF_ARG_3))
	BIF_ERROR(BIF_P, BADARG);
    *bytep = unsigned_val(BIF_ARG_3);
    BIF_RET(BIF_ARG_1);
}

BIF_RETTYPE hipe_bifs_bitarray_2(BIF_ALIST_2)
{
    Sint nbits;
    Uint nbytes;
    Eterm bin;
    int bytemask;

    if (is_not_small(BIF_ARG_1))
	BIF_ERROR(BIF_P, BADARG);
    nbits = signed_val(BIF_ARG_1);
    if (nbits < 0)
	BIF_ERROR(BIF_P, BADARG);
    if (BIF_ARG_2 == am_false)
	bytemask = 0;
    else if (BIF_ARG_2 == am_true)
	bytemask = ~0;
    else
	BIF_ERROR(BIF_P, BADARG);
    nbytes = ((Uint)nbits + ((1 << 3) - 1)) >> 3;
    bin = new_binary(BIF_P, NULL, nbytes);
    memset(binary_bytes(bin), bytemask, nbytes);
    BIF_RET(bin);
}

BIF_RETTYPE hipe_bifs_bitarray_update_3(BIF_ALIST_3)
{
    unsigned char *bytes, bytemask;
    Uint bitnr, bytenr;
    int set;
#ifndef DEBUG
    ERTS_DECLARE_DUMMY(Uint bitoffs);
    ERTS_DECLARE_DUMMY(Uint bitsize);
#else
    Uint bitoffs;
    Uint bitsize;
#endif

    if (is_not_binary(BIF_ARG_1))
	BIF_ERROR(BIF_P, BADARG);
    if (is_not_small(BIF_ARG_2))
	BIF_ERROR(BIF_P, BADARG);
    bitnr = unsigned_val(BIF_ARG_2);
    bytenr = bitnr >> 3;
    if (bytenr >= binary_size(BIF_ARG_1))
	BIF_ERROR(BIF_P, BADARG);
    if (BIF_ARG_3 == am_false)
	set = 0;
    else if (BIF_ARG_3 == am_true)
	set = 1;
    else
	BIF_ERROR(BIF_P, BADARG);
    ERTS_GET_BINARY_BYTES(BIF_ARG_1, bytes, bitoffs, bitsize);
    ASSERT(bitoffs == 0);
    ASSERT(bitsize == 0);
    bytemask = 1 << (bitnr & ((1 << 3) - 1));
    if (set)
	bytes[bytenr] |= bytemask;
    else
	bytes[bytenr] &= ~bytemask;
    BIF_RET(BIF_ARG_1);
}

BIF_RETTYPE hipe_bifs_bitarray_sub_2(BIF_ALIST_2)
{
    unsigned char *bytes, bytemask;
    Uint bitnr, bytenr;
#ifndef DEBUG
    ERTS_DECLARE_DUMMY(Uint bitoffs);
    ERTS_DECLARE_DUMMY(Uint bitsize);
#else
    Uint bitoffs;
    Uint bitsize;
#endif


    if (is_not_binary(BIF_ARG_1))
	BIF_ERROR(BIF_P, BADARG);
    if (is_not_small(BIF_ARG_2))
	BIF_ERROR(BIF_P, BADARG);
    bitnr = unsigned_val(BIF_ARG_2);
    bytenr = bitnr >> 3;
    if (bytenr >= binary_size(BIF_ARG_1))
	BIF_ERROR(BIF_P, BADARG);
    ERTS_GET_BINARY_BYTES(BIF_ARG_1, bytes, bitoffs, bitsize);
    ASSERT(bitoffs == 0);
    ASSERT(bitsize == 0);
    bytemask = 1 << (bitnr & ((1 << 3) - 1));
    if ((bytes[bytenr] & bytemask) == 0)
	BIF_RET(am_false);
    else
	BIF_RET(am_true);
}

/*
 * BIFs for SML-like mutable arrays and reference cells.
 * For now, limited to containing immediate data.
 */
#if 1	/* use bignums as carriers, easier on the gc */
#define make_array_header(sz)	make_pos_bignum_header((sz))
#define array_header_arity(h)	header_arity((h))
#define make_array(hp)		make_big((hp))
#define is_not_array(x)		is_not_big((x))
#define array_val(x)		big_val((x))
#else	/* use tuples as carriers, easier debugging, harder on the gc */
#define make_array_header(sz)	make_arityval((sz))
#define array_header_arity(h)	arityval((h))
#define make_array(hp)		make_tuple((hp))
#define is_not_array(x)		is_not_tuple((x))
#define array_val(x)		tuple_val((x))
#endif
#define array_length(a)		array_header_arity(array_val((a))[0])

BIF_RETTYPE hipe_bifs_array_2(BIF_ALIST_2)
{
    Eterm *hp;
    Sint nelts, i;

    if (is_not_small(BIF_ARG_1) ||
	(nelts = signed_val(BIF_ARG_1)) < 0 ||
	is_not_immed(BIF_ARG_2))
	BIF_ERROR(BIF_P, BADARG);
    if (nelts == 0)	/* bignums must not be empty */
	BIF_RET(make_small(0));
    hp = HAlloc(BIF_P, 1+nelts);
    hp[0] = make_array_header(nelts);
    for (i = 1; i <= nelts; ++i)
	hp[i] = BIF_ARG_2;
    BIF_RET(make_array(hp));
}

BIF_RETTYPE hipe_bifs_array_length_1(BIF_ALIST_1)
{
    if (is_not_array(BIF_ARG_1)) {
	if (BIF_ARG_1 == make_small(0))	/* fixnum 0 represents empty arrays */
	    BIF_RET(make_small(0));
	BIF_ERROR(BIF_P, BADARG);
    }
    BIF_RET(make_small(array_header_arity(array_val(BIF_ARG_1)[0])));
}

BIF_RETTYPE hipe_bifs_array_sub_2(BIF_ALIST_2)
{
    Uint i;

    if (is_not_small(BIF_ARG_2) ||
	is_not_array(BIF_ARG_1) ||
	(i = unsigned_val(BIF_ARG_2)) >= array_length(BIF_ARG_1))
	BIF_ERROR(BIF_P, BADARG);
    BIF_RET(array_val(BIF_ARG_1)[i+1]);
}

BIF_RETTYPE hipe_bifs_array_update_3(BIF_ALIST_3)
{
    Uint i;

    if (is_not_immed(BIF_ARG_3) ||
	is_not_small(BIF_ARG_2) ||
	is_not_array(BIF_ARG_1) ||
	(i = unsigned_val(BIF_ARG_2)) >= array_length(BIF_ARG_1))
	BIF_ERROR(BIF_P, BADARG);
    array_val(BIF_ARG_1)[i+1] = BIF_ARG_3;
    BIF_RET(BIF_ARG_1);
}

BIF_RETTYPE hipe_bifs_ref_1(BIF_ALIST_1)
{
    Eterm *hp;

    if (is_not_immed(BIF_ARG_1))
	BIF_RET(BADARG);
    hp = HAlloc(BIF_P, 1+1);
    hp[0] = make_array_header(1);
    hp[1] = BIF_ARG_1;
    BIF_RET(make_array(hp));
}

BIF_RETTYPE hipe_bifs_ref_get_1(BIF_ALIST_1)
{
    if (is_not_array(BIF_ARG_1) ||
	array_val(BIF_ARG_1)[0] != make_array_header(1))
	BIF_ERROR(BIF_P, BADARG);
    BIF_RET(array_val(BIF_ARG_1)[1]);
}

BIF_RETTYPE hipe_bifs_ref_set_2(BIF_ALIST_2)
{
    if (is_not_immed(BIF_ARG_2) ||
	is_not_array(BIF_ARG_1) ||
	array_val(BIF_ARG_1)[0] != make_array_header(1))
	BIF_ERROR(BIF_P, BADARG);
    array_val(BIF_ARG_1)[1] = BIF_ARG_2;
    BIF_RET(BIF_ARG_1);
}

/*
 * Allocate memory and copy machine code to it.
 */
BIF_RETTYPE hipe_bifs_enter_code_2(BIF_ALIST_2)
{
    Uint nrbytes;
    void *bytes;
    void *address;
    Eterm trampolines;
    Eterm *hp;
#ifndef DEBUG
    ERTS_DECLARE_DUMMY(Uint bitoffs);
    ERTS_DECLARE_DUMMY(Uint bitsize);
#else
    Uint bitoffs;
    Uint bitsize;
#endif

    if (is_not_binary(BIF_ARG_1))
	BIF_ERROR(BIF_P, BADARG);
    nrbytes = binary_size(BIF_ARG_1);
    ERTS_GET_BINARY_BYTES(BIF_ARG_1, bytes, bitoffs, bitsize);
    ASSERT(bitoffs == 0);
    ASSERT(bitsize == 0);
    trampolines = NIL;
    address = hipe_alloc_code(nrbytes, BIF_ARG_2, &trampolines, BIF_P);
    if (!address) {
	Uint nrcallees;

	if (is_tuple(BIF_ARG_2))
	    nrcallees = arityval(tuple_val(BIF_ARG_2)[0]);
	else
	    nrcallees = 0;
	erts_exit(ERTS_ERROR_EXIT, "%s: failed to allocate %lu bytes and %lu trampolines\r\n",
		 __func__, (unsigned long)nrbytes, (unsigned long)nrcallees);
    }
    memcpy(address, bytes, nrbytes);
    hipe_flush_icache_range(address, nrbytes);
    hp = HAlloc(BIF_P, 3);
    hp[0] = make_arityval(2);
    hp[1] = address_to_term(address, BIF_P);
    hp[2] = trampolines;
    BIF_RET(make_tuple(hp));
}

#define IS_POWER_OF_TWO(Val) (((Val) > 0) && (((Val) & ((Val)-1)) == 0))

/*
 * Allocate memory for arbitrary non-Erlang data.
 */
BIF_RETTYPE hipe_bifs_alloc_data_2(BIF_ALIST_2)
{
    Uint align, nrbytes;
    void *block;

    if (is_not_small(BIF_ARG_1) || is_not_small(BIF_ARG_2) ||
	(align = unsigned_val(BIF_ARG_1), !IS_POWER_OF_TWO(align)))
	BIF_ERROR(BIF_P, BADARG);
    nrbytes = unsigned_val(BIF_ARG_2);
    if (nrbytes == 0)
	BIF_RET(make_small(0));
    block = erts_alloc(ERTS_ALC_T_HIPE, nrbytes);
    if ((unsigned long)block & (align-1)) {
	fprintf(stderr, "%s: erts_alloc(%lu) returned %p which is not %lu-byte aligned\r\n",
		__FUNCTION__, (unsigned long)nrbytes, block, (unsigned long)align);
	erts_free(ERTS_ALC_T_HIPE, block);
	BIF_ERROR(BIF_P, EXC_NOTSUP);
    }
    BIF_RET(address_to_term(block, BIF_P));
}

/*
 * Statistics on hipe constants: size of HiPE constants, in words.
 */
unsigned int hipe_constants_size = 0;

BIF_RETTYPE hipe_bifs_constants_size_0(BIF_ALIST_0)
{
    BIF_RET(make_small(hipe_constants_size));
}

/*
 * Merging constant Erlang terms.
 * Uses the constants pool and a hash table of all top-level
 * terms merged so far. (Sub-terms are not merged.)
 */
struct const_term {
    HashBucket bucket;
    Eterm val;		/* tagged pointer to mem[0] */
    Eterm mem[1];	/* variable size */
};

static Hash const_term_table;
static ErlOffHeap const_term_table_off_heap;

static HashValue const_term_hash(void *tmpl)
{
    return make_hash2((Eterm)tmpl);
}

static int const_term_cmp(void *tmpl, void *bucket)
{
    return !eq((Eterm)tmpl, ((struct const_term*)bucket)->val);
}

static void *const_term_alloc(void *tmpl)
{
    Eterm obj;
    Uint size;
    Uint alloc_size;
    Eterm *hp;
    struct const_term *p;

    obj = (Eterm)tmpl;
    ASSERT(is_not_immed(obj));
    size = size_object(obj);
    alloc_size = size + (offsetof(struct const_term, mem)/sizeof(Eterm));
    hipe_constants_size += alloc_size;

    p = (struct const_term*)erts_alloc(ERTS_ALC_T_LITERAL, alloc_size * sizeof(Eterm));

    /* I have absolutely no idea if having a private 'off_heap'
       works or not. _Some_ off_heap object is required for
       REFC_BINARY and FUN values, but _where_ it should be is
       a complete mystery to me. */
    hp = &p->mem[0];
    p->val = copy_struct(obj, size, &hp, &const_term_table_off_heap);

    erts_set_literal_tag(&p->val, &p->mem[0], size);

    return &p->bucket;
}

static void init_const_term_table(void)
{
    HashFunctions f;
    f.hash = (H_FUN) const_term_hash;
    f.cmp = (HCMP_FUN) const_term_cmp;
    f.alloc = (HALLOC_FUN) const_term_alloc;
    f.free = (HFREE_FUN) NULL;
    f.meta_alloc = (HMALLOC_FUN) erts_alloc;
    f.meta_free = (HMFREE_FUN) erts_free;
    f.meta_print = (HMPRINT_FUN) erts_print;
    hash_init(ERTS_ALC_T_HIPE, &const_term_table, "const_term_table", 97, f);
}

BIF_RETTYPE hipe_bifs_merge_term_1(BIF_ALIST_1)
{
    static int init_done = 0;
    struct const_term *p;
    Eterm val;

    val = BIF_ARG_1;
    if (is_not_immed(val)) {
	if (!init_done) {
	    init_const_term_table();
	    init_done = 1;
	}
	p = (struct const_term*)hash_put(&const_term_table, (void*)val);
	val = p->val;
    }
    BIF_RET(val);
}

struct mfa_t {
    Eterm mod;
    Eterm fun;
    Uint  ari;
};

static int term_to_mfa(Eterm term, struct mfa_t *mfa)
{
    Eterm mod, fun, a;
    Uint ari;

    if (is_not_tuple(term))
	return 0;
    if (tuple_val(term)[0] != make_arityval(3))
	return 0;
    mod = tuple_val(term)[1];
    if (is_not_atom(mod))
	return 0;
    mfa->mod = mod;
    fun = tuple_val(term)[2];
    if (is_not_atom(fun))
	return 0;
    mfa->fun = fun;
    a = tuple_val(term)[3];
    if (is_not_small(a))
	return 0;
    ari = unsigned_val(a);
    if (ari > 255)
	return 0;
    mfa->ari = ari;
    return 1;
}

#ifdef DEBUG_LINKER
static void print_mfa(Eterm mod, Eterm fun, unsigned int ari)
{
    erts_printf("%T:%T/%u", mod, fun, ari);
}
#endif

/*
 * Convert {M,F,A} to pointer to first insn after initial func_info.
 */
static Uint *hipe_find_emu_address(Eterm mod, Eterm name, unsigned int arity)
{
    Module *modp;
    BeamCodeHeader* code_hdr;
    int i, n;

    modp = erts_get_module(mod, erts_active_code_ix());
    if (modp == NULL || (code_hdr = modp->curr.code_hdr) == NULL)
	return NULL;
    n = code_hdr->num_functions;
    for (i = 0; i < n; ++i) {
	Uint *code_ptr = (Uint*)code_hdr->functions[i];
	ASSERT(code_ptr[0] == BeamOpCode(op_i_func_info_IaaI));
	if (code_ptr[3] == name && code_ptr[4] == arity)
	    return code_ptr+5;
    }
    return NULL;
}

Uint *hipe_bifs_find_pc_from_mfa(Eterm term)
{
    struct mfa_t mfa;

    if (!term_to_mfa(term, &mfa))
	return NULL;
    return hipe_find_emu_address(mfa.mod, mfa.fun, mfa.ari);
}

BIF_RETTYPE hipe_bifs_fun_to_address_1(BIF_ALIST_1)
{
    Eterm *pc = hipe_bifs_find_pc_from_mfa(BIF_ARG_1);
    if (!pc)
	BIF_ERROR(BIF_P, BADARG);
    BIF_RET(address_to_term(pc, BIF_P));
}

BIF_RETTYPE hipe_bifs_set_native_address_3(BIF_ALIST_3)
{
    Eterm *pc;
    void *address;
    int is_closure;
    struct mfa_t mfa;

    switch (BIF_ARG_3) {
      case am_false:
	is_closure = 0;
	break;
      case am_true:
	is_closure = 1;
	break;
      default:
	BIF_ERROR(BIF_P, BADARG);
    }
    address = term_to_address(BIF_ARG_2);
    if (!address)
	BIF_ERROR(BIF_P, BADARG);

    /* The mfa is needed again later, otherwise we could
       simply have called hipe_bifs_find_pc_from_mfa(). */
    if (!term_to_mfa(BIF_ARG_1, &mfa))
	BIF_ERROR(BIF_P, BADARG);
    pc = hipe_find_emu_address(mfa.mod, mfa.fun, mfa.ari);

    if (pc) {
	hipe_mfa_save_orig_beam_op(mfa.mod, mfa.fun, mfa.ari, pc);
#if HIPE
#ifdef DEBUG_LINKER
	printf("%s: ", __FUNCTION__);
	print_mfa(mfa.mod, mfa.fun, mfa.ari);
	printf(": planting call trap to %p at BEAM pc %p\r\n", address, pc);
#endif
	hipe_set_call_trap(pc, address, is_closure);
	BIF_RET(am_true);
#endif
    }
#ifdef DEBUG_LINKER
    printf("%s: ", __FUNCTION__);
    print_mfa(mfa.mod, mfa.fun, mfa.ari);
    printf(": no BEAM pc found\r\n");
#endif
    BIF_RET(am_false);
}

BIF_RETTYPE hipe_bifs_enter_sdesc_1(BIF_ALIST_1)
{
    struct sdesc *sdesc;

    sdesc = hipe_decode_sdesc(BIF_ARG_1);
    if (!sdesc) {
	fprintf(stderr, "%s: bad sdesc!\r\n", __FUNCTION__);
	BIF_ERROR(BIF_P, BADARG);
    }
    if (hipe_put_sdesc(sdesc) != sdesc) {
	fprintf(stderr, "%s: duplicate entry!\r\n", __FUNCTION__);
	BIF_ERROR(BIF_P, BADARG);
    }
    BIF_RET(NIL);
}

/*
 * Hash table mapping {M,F,A} to nbif address.
 */
struct nbif {
    HashBucket bucket;
    Eterm mod;
    Eterm fun;
    unsigned arity;
    const void *address;
};

static struct nbif nbifs[BIF_SIZE] = {
#define BIF_LIST(MOD,FUN,ARY,CFUN,IX)	\
	{ {0,0}, MOD, FUN, ARY, &nbif_##CFUN },
#include "erl_bif_list.h"
#undef BIF_LIST
};

#define NBIF_HASH(m,f,a)	(atom_val(m) ^ atom_val(f) ^ (a))
static Hash nbif_table;

static HashValue nbif_hash(struct nbif *x)
{
    return NBIF_HASH(x->mod, x->fun, x->arity);
}

static int nbif_cmp(struct nbif *x, struct nbif *y)
{
    return !(x->mod == y->mod && x->fun == y->fun && x->arity == y->arity);
}

static struct nbif *nbif_alloc(struct nbif *x)
{
    return x;	/* pre-allocated */
}

static void init_nbif_table(void)
{
    HashFunctions f;
    int i;

    f.hash = (H_FUN) nbif_hash;
    f.cmp = (HCMP_FUN) nbif_cmp;
    f.alloc = (HALLOC_FUN) nbif_alloc;
    f.free = NULL;
    f.meta_alloc = (HMALLOC_FUN) erts_alloc;
    f.meta_free = (HMFREE_FUN) erts_free;
    f.meta_print = (HMPRINT_FUN) erts_print;

    hash_init(ERTS_ALC_T_NBIF_TABLE, &nbif_table, "nbif_table", 500, f);

    for (i = 0; i < BIF_SIZE; ++i)
	hash_put(&nbif_table, &nbifs[i]);
}

static const void *nbif_address(Eterm mod, Eterm fun, unsigned arity)
{
    struct nbif tmpl;
    struct nbif *nbif;

    tmpl.mod = mod;
    tmpl.fun = fun;
    tmpl.arity = arity;

    nbif = hash_get(&nbif_table, &tmpl);
    return nbif ? nbif->address : NULL;
}

/*
 * hipe_bifs_bif_address(M,F,A) -> address or false
 */
BIF_RETTYPE hipe_bifs_bif_address_3(BIF_ALIST_3)
{
    const void *address;
    static int init_done = 0;

    if (!init_done) {
	init_nbif_table();
	init_done = 1;
    }

    if (is_not_atom(BIF_ARG_1) ||
	is_not_atom(BIF_ARG_2) ||
	is_not_small(BIF_ARG_3) ||
	signed_val(BIF_ARG_3) < 0)
	BIF_RET(am_false);

    address = nbif_address(BIF_ARG_1, BIF_ARG_2, unsigned_val(BIF_ARG_3));
    if (address)
	BIF_RET(address_to_term(address, BIF_P));
    BIF_RET(am_false);
}

/*
 * Hash table mapping primops to their addresses.
 */
struct primop {
    HashBucket bucket;	/* bucket.hvalue == atom_val(name) */
    const void *address;
#if defined(__arm__)
    void *trampoline;
#endif
};

static struct primop primops[] = {
#define PRIMOP_LIST(ATOM,ADDRESS)	{ {0,_unchecked_atom_val(ATOM)}, ADDRESS },
#include "hipe_primops.h"
#undef PRIMOP_LIST
};

static Hash primop_table;

static HashValue primop_hash(void *tmpl)
{
    return ((struct primop*)tmpl)->bucket.hvalue;	/* pre-initialised */
}

static int primop_cmp(void *tmpl, void *bucket)
{
    return 0;	/* hvalue matched so nothing further to do */
}

static void *primop_alloc(void *tmpl)
{
    return tmpl;	/* pre-allocated */
}

static void init_primop_table(void)
{
    HashFunctions f;
    int i;
    static int init_done = 0;

    if (init_done)
	return;
    init_done = 1;

    f.hash = (H_FUN) primop_hash;
    f.cmp = (HCMP_FUN) primop_cmp;
    f.alloc = (HALLOC_FUN) primop_alloc;
    f.free = NULL;
    f.meta_alloc = (HMALLOC_FUN) erts_alloc;
    f.meta_free = (HMFREE_FUN) erts_free;
    f.meta_print = (HMPRINT_FUN) erts_print;

    hash_init(ERTS_ALC_T_HIPE, &primop_table, "primop_table", 50, f);

    for (i = 0; i < sizeof(primops)/sizeof(primops[0]); ++i)
	hash_put(&primop_table, &primops[i]);
}

static struct primop *primop_table_get(Eterm name)
{
    struct primop tmpl;

    init_primop_table();
    tmpl.bucket.hvalue = atom_val(name);
    return hash_get(&primop_table, &tmpl);
}

#if defined(__arm__)
static struct primop *primop_table_put(Eterm name)
{
    struct primop tmpl;

    init_primop_table();
    tmpl.bucket.hvalue = atom_val(name);
    return hash_put(&primop_table, &tmpl);
}

void *hipe_primop_get_trampoline(Eterm name)
{
    struct primop *primop = primop_table_get(name);
    return primop ? primop->trampoline : NULL;
}

void hipe_primop_set_trampoline(Eterm name, void *trampoline)
{
    struct primop *primop = primop_table_put(name);
    primop->trampoline = trampoline;
}
#endif

/*
 * hipe_bifs_primop_address(Atom) -> address or false
 */
BIF_RETTYPE hipe_bifs_primop_address_1(BIF_ALIST_1)
{
    const struct primop *primop;

    if (is_not_atom(BIF_ARG_1))
	BIF_RET(am_false);
    primop = primop_table_get(BIF_ARG_1);
    if (!primop)
	BIF_RET(am_false);
    BIF_RET(address_to_term(primop->address, BIF_P));
}

BIF_RETTYPE hipe_bifs_atom_to_word_1(BIF_ALIST_1)
{
    if (is_not_atom(BIF_ARG_1))
	BIF_ERROR(BIF_P, BADARG);
    BIF_RET(Uint_to_term(BIF_ARG_1, BIF_P));
}

BIF_RETTYPE hipe_bifs_term_to_word_1(BIF_ALIST_1)
{
    BIF_RET(Uint_to_term(BIF_ARG_1, BIF_P));
}

/* XXX: this is really a primop, not a BIF */
BIF_RETTYPE hipe_conv_big_to_float(BIF_ALIST_1)
{
    Eterm res;
    Eterm *hp;
    FloatDef f;

    if (is_not_big(BIF_ARG_1))
	BIF_ERROR(BIF_P, BADARG);
    if (big_to_double(BIF_ARG_1, &f.fd) < 0)
	BIF_ERROR(BIF_P, BADARG);
    hp = HAlloc(BIF_P, FLOAT_SIZE_OBJECT);
    res = make_float(hp);
    PUT_DOUBLE(f, hp);
    BIF_RET(res);
}

#ifdef NO_FPE_SIGNALS

/* 
   This is the current solution to make hipe run without FPE.
   The native code is the same except that a call to this primop
   is made after _every_ float operation to check the result.
   The native fcheckerror still done later will detect if an
   "emulated" FPE has occured.
   We use p->hipe.float_result to avoid passing a 'double' argument,
   which has its own calling convention (on amd64 at least).
   Simple and slow...
*/
void hipe_emulate_fpe(Process* p)
{
    if (!isfinite(p->hipe.float_result)) {
	p->fp_exception = 1;
    }
}
#endif

void hipe_emasculate_binary(Eterm bin)
{
    ProcBin* pb = (ProcBin *) boxed_val(bin);
    ASSERT(pb->thing_word == HEADER_PROC_BIN);
    ASSERT(pb->flags != 0);
    erts_emasculate_writable_binary(pb);
}

/*
 * args: Module, {Uniq, Index, BeamAddress}
 */
BIF_RETTYPE hipe_bifs_get_fe_2(BIF_ALIST_2)
{
    Eterm mod;
    Uint index;
    Uint uniq;
    void *beam_address;
    ErlFunEntry *fe;
    Eterm *tp;

    if (is_not_atom(BIF_ARG_1))
	BIF_ERROR(BIF_P, BADARG);
    mod = BIF_ARG_1;

    if (is_not_tuple(BIF_ARG_2) ||
	(arityval(*tuple_val(BIF_ARG_2)) != 3))
	BIF_ERROR(BIF_P, BADARG);
    tp = tuple_val(BIF_ARG_2);
    if (term_to_Uint(tp[1], &uniq) == 0)
	BIF_ERROR(BIF_P, BADARG);
    if (term_to_Uint(tp[2], &index) == 0)
	BIF_ERROR(BIF_P, BADARG);

    beam_address = term_to_address(tp[3]);
    if (!beam_address)
	BIF_ERROR(BIF_P, BADARG);

    fe = erts_get_fun_entry(mod, uniq, index);
    if (fe == NULL) {
	int i = atom_val(mod);
	char atom_buf[256];

	atom_buf[0] = '\0';
	strncat(atom_buf, (char*)atom_tab(i)->name, atom_tab(i)->len);
	printf("no fun entry for %s %ld:%ld\n", atom_buf, uniq, index);
	BIF_ERROR(BIF_P, BADARG);
    }
    BIF_RET(address_to_term((void *)fe, BIF_P));
}

/*
 * args: FE, Nativecodeaddress
 */
BIF_RETTYPE hipe_bifs_set_native_address_in_fe_2(BIF_ALIST_2)
{
    ErlFunEntry *fe;
    void *native_address;

    fe = (ErlFunEntry *)term_to_address(BIF_ARG_1);
    if (!fe)
	BIF_ERROR(BIF_P, BADARG);
    native_address = term_to_address(BIF_ARG_2);
    if (!native_address)
	BIF_ERROR(BIF_P, BADARG);

    fe->native_address = native_address;
    if (erts_refc_dectest(&fe->refc, 0) == 0)
	erts_erase_fun_entry(fe);
    BIF_RET(am_true);
}

/*
 * MFA info hash table:
 * - maps MFA to native code entry point
 * - the MFAs it calls (refers_to)
 * - the references to it (referred_from)
 * - maps MFA to most recent trampoline [if powerpc or arm]
 */
struct hipe_mfa_info {
    struct {
	unsigned long hvalue;
	struct hipe_mfa_info *next;
    } bucket;
    Eterm m;	/* atom */
    Eterm f;	/* atom */
    unsigned int a;
    void *remote_address;
    void *local_address;
    Eterm *beam_code;
    Uint orig_beam_op;
    struct hipe_mfa_info_list *refers_to;
    struct ref *referred_from;
#if defined(__powerpc__) || defined(__ppc__) || defined(__powerpc64__) || defined(__arm__)
    void *trampoline;
#endif
};

static struct {
    unsigned int log2size;
    unsigned int mask;		/* INV: mask == (1 << log2size)-1 */
    unsigned int used;
    struct hipe_mfa_info **bucket;
    /*
     * The mfa info table is normally updated by the loader,
     * which runs in non-concurrent mode. Unfortunately runtime
     * apply operations (get_na_nofail) update the table if
     * they create a new stub for the mfa, which forces locking.
     * XXX: Redesign apply et al to avoid those updates.
     */
    erts_smp_rwmtx_t lock;
} hipe_mfa_info_table;

static inline void hipe_mfa_info_table_init_lock(void)
{
    erts_smp_rwmtx_init(&hipe_mfa_info_table.lock, "hipe_mfait_lock");
}

static inline void hipe_mfa_info_table_rlock(void)
{
    erts_smp_rwmtx_rlock(&hipe_mfa_info_table.lock);
}

static inline void hipe_mfa_info_table_runlock(void)
{
    erts_smp_rwmtx_runlock(&hipe_mfa_info_table.lock);
}

static inline void hipe_mfa_info_table_rwlock(void)
{
    erts_smp_rwmtx_rwlock(&hipe_mfa_info_table.lock);
}

static inline void hipe_mfa_info_table_rwunlock(void)
{
    erts_smp_rwmtx_rwunlock(&hipe_mfa_info_table.lock);
}

#define HIPE_MFA_HASH(M,F,A)	(atom_val(M) ^ atom_val(F) ^ (A))

static struct hipe_mfa_info **hipe_mfa_info_table_alloc_bucket(unsigned int size)
{
    unsigned long nbytes = size * sizeof(struct hipe_mfa_info*);
    struct hipe_mfa_info **bucket = erts_alloc(ERTS_ALC_T_HIPE, nbytes);
    sys_memzero(bucket, nbytes);
    return bucket;
}

static void hipe_mfa_info_table_grow(void)
{
    unsigned int old_size, new_size, new_mask;
    struct hipe_mfa_info **old_bucket, **new_bucket;
    unsigned int i;

    old_size = 1 << hipe_mfa_info_table.log2size;
    hipe_mfa_info_table.log2size += 1;
    new_size = 1 << hipe_mfa_info_table.log2size;
    new_mask = new_size - 1;
    hipe_mfa_info_table.mask = new_mask;
    old_bucket = hipe_mfa_info_table.bucket;
    new_bucket = hipe_mfa_info_table_alloc_bucket(new_size);
    hipe_mfa_info_table.bucket = new_bucket;
    for (i = 0; i < old_size; ++i) {
	struct hipe_mfa_info *b = old_bucket[i];
	while (b != NULL) {
	    struct hipe_mfa_info *next = b->bucket.next;
	    unsigned int j = b->bucket.hvalue & new_mask;
	    b->bucket.next = new_bucket[j];
	    new_bucket[j] = b;
	    b = next;
	}
    }
    erts_free(ERTS_ALC_T_HIPE, old_bucket);
}

static struct hipe_mfa_info *hipe_mfa_info_table_alloc(Eterm m, Eterm f, unsigned int arity)
{
    struct hipe_mfa_info *res;

    res = (struct hipe_mfa_info*)erts_alloc(ERTS_ALC_T_HIPE, sizeof(*res));
    res->m = m;
    res->f = f;
    res->a = arity;
    res->remote_address = NULL;
    res->local_address = NULL;
    res->beam_code = NULL;
    res->orig_beam_op = 0;
    res->refers_to = NULL;
    res->referred_from = NULL;
#if defined(__powerpc__) || defined(__ppc__) || defined(__powerpc64__) || defined(__arm__)
    res->trampoline = NULL;
#endif

    return res;
}

void hipe_mfa_info_table_init(void)
{
    unsigned int log2size, size;

    log2size = 10;
    size = 1 << log2size;
    hipe_mfa_info_table.log2size = log2size;
    hipe_mfa_info_table.mask = size - 1;
    hipe_mfa_info_table.used = 0;
    hipe_mfa_info_table.bucket = hipe_mfa_info_table_alloc_bucket(size);

    hipe_mfa_info_table_init_lock();
}

static inline struct hipe_mfa_info *hipe_mfa_info_table_get_locked(Eterm m, Eterm f, unsigned int arity)
{
    unsigned long h;
    unsigned int i;
    struct hipe_mfa_info *p;

    h = HIPE_MFA_HASH(m, f, arity);
    i = h & hipe_mfa_info_table.mask;
    p = hipe_mfa_info_table.bucket[i];
    for (; p; p = p->bucket.next) {
        if (p->bucket.hvalue == h) {
            if (p->m == m && p->f == f && p->a == arity)
                return p;
        }
        else ASSERT(!(p->m == m && p->f == f && p->a == arity));
    }
    return NULL;
}

#if 0 /* XXX: unused */
void *hipe_mfa_find_na(Eterm m, Eterm f, unsigned int arity)
{
    const struct hipe_mfa_info *p;

    p = hipe_mfa_info_table_get(m, f, arity);
    return p ? p->address : NULL;
}
#endif

static struct hipe_mfa_info *hipe_mfa_info_table_put_rwlocked(Eterm m, Eterm f, unsigned int arity)
{
    unsigned long h;
    unsigned int i;
    struct hipe_mfa_info *p;
    unsigned int size;

    h = HIPE_MFA_HASH(m, f, arity);
    i = h & hipe_mfa_info_table.mask;
    p = hipe_mfa_info_table.bucket[i];
    for (; p; p = p->bucket.next) {
        if (p->bucket.hvalue == h) {
            if (p->m == m && p->f == f && p->a == arity)
                return p;
        }
        else ASSERT(!(p->m == m && p->f == f && p->a == arity));
    }
    p = hipe_mfa_info_table_alloc(m, f, arity);
    p->bucket.hvalue = h;
    p->bucket.next = hipe_mfa_info_table.bucket[i];
    hipe_mfa_info_table.bucket[i] = p;
    hipe_mfa_info_table.used += 1;
    size = 1 << hipe_mfa_info_table.log2size;
    if (hipe_mfa_info_table.used > (4*size/5))		/* rehash at 80% */
	hipe_mfa_info_table_grow();
    return p;
}

static void hipe_mfa_set_na(Eterm m, Eterm f, unsigned int arity, void *address, int is_exported)
{
    struct hipe_mfa_info *p;

    hipe_mfa_info_table_rwlock();
    p = hipe_mfa_info_table_put_rwlocked(m, f, arity);
#ifdef DEBUG_LINKER
    printf("%s: ", __FUNCTION__);
    print_mfa(m, f, arity);
    printf(": changing address from %p to %p\r\n", p->local_address, address);
#endif
    p->local_address = address;
    if (is_exported)
	p->remote_address = address;
    hipe_mfa_info_table_rwunlock();
}

#if defined(__powerpc__) || defined(__ppc__) || defined(__powerpc64__) || defined(__arm__)
void *hipe_mfa_get_trampoline(Eterm m, Eterm f, unsigned int arity)
{
    struct hipe_mfa_info *p;
    void *trampoline;

    hipe_mfa_info_table_rlock();
    p = hipe_mfa_info_table_get_locked(m, f, arity);
    trampoline = p ? p->trampoline : NULL;
    hipe_mfa_info_table_runlock();
    return trampoline;
}

void hipe_mfa_set_trampoline(Eterm m, Eterm f, unsigned int arity, void *trampoline)
{
    struct hipe_mfa_info *p;

    hipe_mfa_info_table_rwlock();
    p = hipe_mfa_info_table_put_rwlocked(m, f, arity);
    p->trampoline = trampoline;
    hipe_mfa_info_table_rwunlock();
}
#endif

BIF_RETTYPE hipe_bifs_set_funinfo_native_address_3(BIF_ALIST_3)
{
    struct mfa_t mfa;
    void *address;
    int is_exported;

    if (!term_to_mfa(BIF_ARG_1, &mfa))
	BIF_ERROR(BIF_P, BADARG);
    address = term_to_address(BIF_ARG_2);
    if (!address)
	BIF_ERROR(BIF_P, BADARG);
    if (BIF_ARG_3 == am_true)
	is_exported = 1;
    else if (BIF_ARG_3 == am_false)
	is_exported = 0;
    else
	BIF_ERROR(BIF_P, BADARG);
    hipe_mfa_set_na(mfa.mod, mfa.fun, mfa.ari, address, is_exported);
    BIF_RET(NIL);
}

BIF_RETTYPE hipe_bifs_invalidate_funinfo_native_addresses_1(BIF_ALIST_1)
{
    Eterm lst;
    struct mfa_t mfa;
    struct hipe_mfa_info *p;

    hipe_mfa_info_table_rwlock();
    lst = BIF_ARG_1;
    while (is_list(lst)) {
	if (!term_to_mfa(CAR(list_val(lst)), &mfa))
	    break;
	lst = CDR(list_val(lst));
	p = hipe_mfa_info_table_get_locked(mfa.mod, mfa.fun, mfa.ari);
	if (p) {
	    p->remote_address = NULL;
	    p->local_address = NULL;
	    if (p->beam_code) {
#ifdef DEBUG_LINKER
		printf("%s: ", __FUNCTION__);
		print_mfa(mfa.mod, mfa.fun, mfa.ari);
		printf(": removing call trap from BEAM pc %p (new op %#lx)\r\n",
		       p->beam_code, p->orig_beam_op);
#endif
		p->beam_code[0] = p->orig_beam_op;
		p->beam_code = NULL;
		p->orig_beam_op = 0;
	    } else {
#ifdef DEBUG_LINKER
		printf("%s: ", __FUNCTION__);
		print_mfa(mfa.mod, mfa.fun, mfa.ari);
		printf(": no call trap to remove\r\n");
#endif
	    }
	}
    }
    hipe_mfa_info_table_rwunlock();
    if (is_not_nil(lst))
	BIF_ERROR(BIF_P, BADARG);
    BIF_RET(NIL);
}

void hipe_mfa_save_orig_beam_op(Eterm mod, Eterm fun, unsigned int ari, Eterm *pc)
{
    Uint orig_beam_op;
    struct hipe_mfa_info *p;

    orig_beam_op = pc[0];
    if (orig_beam_op != BeamOpCode(op_hipe_trap_call_closure) &&
	orig_beam_op != BeamOpCode(op_hipe_trap_call)) {
	hipe_mfa_info_table_rwlock();
	p = hipe_mfa_info_table_put_rwlocked(mod, fun, ari);
#ifdef DEBUG_LINKER
	printf("%s: ", __FUNCTION__);
	print_mfa(mod, fun, ari);
	printf(": saving orig op %#lx from BEAM pc %p\r\n", orig_beam_op, pc);
#endif
	p->beam_code = pc;
	p->orig_beam_op = orig_beam_op;
	hipe_mfa_info_table_rwunlock();
    } else {
#ifdef DEBUG_LINKER
	printf("%s: ", __FUNCTION__);
	print_mfa(mod, fun, ari);
	printf(": orig op %#lx already saved\r\n", orig_beam_op);
#endif
    }
}

static void *hipe_make_stub(Eterm m, Eterm f, unsigned int arity, int is_remote)
{
    Export *export_entry;
    void *StubAddress;

    ASSERT(is_remote);

    export_entry = erts_export_get_or_make_stub(m, f, arity);
    StubAddress = hipe_make_native_stub(export_entry, arity);
    if (!StubAddress)
	erts_exit(ERTS_ERROR_EXIT, "hipe_make_stub: code allocation failed\r\n");
    return StubAddress;
}

static void *hipe_get_na_try_locked(Eterm m, Eterm f, unsigned int a, int is_remote, struct hipe_mfa_info **pp)
{
    struct hipe_mfa_info *p;
    void *address;

    p = hipe_mfa_info_table_get_locked(m, f, a);
    if (p) {
	/* find address, predicting for a runtime apply call */
	address = p->remote_address;
	if (!is_remote)
	    address = p->local_address;
	if (address)
	    return address;

	/* bummer, install stub, checking if one already existed */
	address = p->remote_address;
	if (address)
	    return address;
    }
    /* Caller must take the slow path with the write lock held, but allow
       it to avoid some work if it already holds the write lock.  */
    if (pp)
	*pp = p;
    return NULL;
}

static void *hipe_get_na_slow_rwlocked(Eterm m, Eterm f, unsigned int a, int is_remote, struct hipe_mfa_info *p)
{
    void *address;

    if (!p)
	p = hipe_mfa_info_table_put_rwlocked(m, f, a);
    address = hipe_make_stub(m, f, a, is_remote);
    /* XXX: how to tell if a BEAM MFA is exported or not? */
    p->remote_address = address;
    return address;
}

static void *hipe_get_na_nofail_rwlocked(Eterm m, Eterm f, unsigned int a, int is_remote)
{
    struct hipe_mfa_info *p;
    void *address;

    address = hipe_get_na_try_locked(m, f, a, is_remote, &p);
    if (address)
	return address;

    address = hipe_get_na_slow_rwlocked(m, f, a, is_remote, p);
    return address;
}

static void *hipe_get_na_nofail(Eterm m, Eterm f, unsigned int a, int is_remote)
{
    void *address;

    hipe_mfa_info_table_rlock();
    address = hipe_get_na_try_locked(m, f, a, is_remote, NULL);
    hipe_mfa_info_table_runlock();
    if (address)
	return address;

    hipe_mfa_info_table_rwlock();
    address = hipe_get_na_slow_rwlocked(m, f, a, is_remote, NULL);
    hipe_mfa_info_table_rwunlock();
    return address;
}

/* used for apply/3 in hipe_mode_switch */
void *hipe_get_remote_na(Eterm m, Eterm f, unsigned int a)
{
    if (is_not_atom(m) || is_not_atom(f) || a > 255)
	return NULL;
    return hipe_get_na_nofail(m, f, a, 1);
}

/* primop, but called like a BIF for error handling purposes */
BIF_RETTYPE hipe_find_na_or_make_stub(BIF_ALIST_3)
{
    Uint arity;
    void *address;

    if (is_not_atom(BIF_ARG_1) || is_not_atom(BIF_ARG_2))
	BIF_ERROR(BIF_P, BADARG);
    arity = unsigned_val(BIF_ARG_3); /* no error check */
    address = hipe_get_na_nofail(BIF_ARG_1, BIF_ARG_2, arity, 1);
    BIF_RET((Eterm)address);	/* semi-Ok */
}

BIF_RETTYPE hipe_bifs_find_na_or_make_stub_2(BIF_ALIST_2)
{
    struct mfa_t mfa;
    void *address;
    int is_remote;

    if (!term_to_mfa(BIF_ARG_1, &mfa))
	BIF_ERROR(BIF_P, BADARG);
    if (BIF_ARG_2 == am_true)
	is_remote = 1;
    else if (BIF_ARG_2 == am_false)
	is_remote = 0;
    else
	BIF_ERROR(BIF_P, BADARG);
    address = hipe_get_na_nofail(mfa.mod, mfa.fun, mfa.ari, is_remote);
    BIF_RET(address_to_term(address, BIF_P));
}

/* primop, but called like a BIF for error handling purposes */
BIF_RETTYPE hipe_nonclosure_address(BIF_ALIST_2)
{
    Eterm hdr, m, f;
    void *address;

    if (!is_boxed(BIF_ARG_1))
	goto badfun;
    hdr = *boxed_val(BIF_ARG_1);
    if (is_export_header(hdr)) {
	Export *ep = (Export*)(export_val(BIF_ARG_1)[1]);
	unsigned int actual_arity = ep->code[2];
	if (actual_arity != BIF_ARG_2)
	    goto badfun;
	m = ep->code[0];
	f = ep->code[1];
    } else
	goto badfun;
    address = hipe_get_na_nofail(m, f, BIF_ARG_2, 1);
    BIF_RET((Eterm)address);

 badfun:
    BIF_P->current = NULL;
    BIF_P->fvalue = BIF_ARG_1;
    BIF_ERROR(BIF_P, EXC_BADFUN);
}

int hipe_find_mfa_from_ra(const void *ra, Eterm *m, Eterm *f, unsigned int *a)
{
    struct hipe_mfa_info *mfa;
    long mfa_offset, ra_offset;
    struct hipe_mfa_info **bucket;
    unsigned int i, nrbuckets;

    /* Note about locking: the table is only updated from the
       loader, which runs with the rest of the system suspended. */
    /* XXX: alas not true; see comment at hipe_mfa_info_table.lock */
    hipe_mfa_info_table_rlock();
    bucket = hipe_mfa_info_table.bucket;
    nrbuckets = 1 << hipe_mfa_info_table.log2size;
    mfa = NULL;
    mfa_offset = LONG_MAX;
    for (i = 0; i < nrbuckets; ++i) {
	struct hipe_mfa_info *b = bucket[i];
	while (b != NULL) {
	    ra_offset = (char*)ra - (char*)b->local_address;
	    if (ra_offset > 0 && ra_offset < mfa_offset) {
		mfa_offset = ra_offset;
		mfa = b;
	    }
	    b = b->bucket.next;
	}
    }
    if (mfa) {
	*m = mfa->m;
	*f = mfa->f;
	*a = mfa->a;
    }
    hipe_mfa_info_table_runlock();
    return mfa ? 1 : 0;
}

/*
 * Patch Reference Handling.
 */
struct hipe_mfa_info_list {
    struct hipe_mfa_info *mfa;
    struct hipe_mfa_info_list *next;
};

struct ref {
    struct hipe_mfa_info *caller_mfa;
    void *address;
    void *trampoline;
    unsigned int flags;
    struct ref *next;
};
#define REF_FLAG_IS_LOAD_MFA		1	/* bit 0: 0 == call, 1 == load_mfa */
#define REF_FLAG_IS_REMOTE		2	/* bit 1: 0 == local, 1 == remote */
#define REF_FLAG_PENDING_REDIRECT	4	/* bit 2: 1 == pending redirect */
#define REF_FLAG_PENDING_REMOVE		8	/* bit 3: 1 == pending remove */

/* add_ref(CalleeMFA, {CallerMFA,Address,'call'|'load_mfa',Trampoline,'remote'|'local'})
 */
BIF_RETTYPE hipe_bifs_add_ref_2(BIF_ALIST_2)
{
    struct mfa_t callee;
    Eterm *tuple;
    struct mfa_t caller;
    void *address;
    void *trampoline;
    unsigned int flags;
    struct hipe_mfa_info *callee_mfa;
    struct hipe_mfa_info *caller_mfa;
    struct hipe_mfa_info_list *refers_to;
    struct ref *ref;

    if (!term_to_mfa(BIF_ARG_1, &callee))
	goto badarg;
    if (is_not_tuple(BIF_ARG_2))
	goto badarg;
    tuple = tuple_val(BIF_ARG_2);
    if (tuple[0] != make_arityval(5))
	goto badarg;
    if (!term_to_mfa(tuple[1], &caller))
	goto badarg;
    address = term_to_address(tuple[2]);
    if (!address)
	goto badarg;
    switch (tuple[3]) {
      case am_call:
	flags = 0;
	break;
      case am_load_mfa:
	flags = REF_FLAG_IS_LOAD_MFA;
	break;
      default:
	goto badarg;
    }
    if (is_nil(tuple[4]))
	trampoline = NULL;
    else {
	trampoline = term_to_address(tuple[4]);
	if (!trampoline)
	    goto badarg;
    }
    switch (tuple[5]) {
      case am_local:
	break;
      case am_remote:
	flags |= REF_FLAG_IS_REMOTE;
	break;
      default:
	goto badarg;
    }
    hipe_mfa_info_table_rwlock();
    callee_mfa = hipe_mfa_info_table_put_rwlocked(callee.mod, callee.fun, callee.ari);
    caller_mfa = hipe_mfa_info_table_put_rwlocked(caller.mod, caller.fun, caller.ari);

    refers_to = erts_alloc(ERTS_ALC_T_HIPE, sizeof(*refers_to));
    refers_to->mfa = callee_mfa;
    refers_to->next = caller_mfa->refers_to;
    caller_mfa->refers_to = refers_to;

    ref = erts_alloc(ERTS_ALC_T_HIPE, sizeof(*ref));
    ref->caller_mfa = caller_mfa;
    ref->address = address;
    ref->trampoline = trampoline;
    ref->flags = flags;
    ref->next = callee_mfa->referred_from;
    callee_mfa->referred_from = ref;
    hipe_mfa_info_table_rwunlock();

    BIF_RET(NIL);

 badarg:
    BIF_ERROR(BIF_P, BADARG);
}

/* Given a CalleeMFA, mark each ref to it as pending-redirect.
 * This ensures that remove_refs_from() won't remove them: any
 * removal is instead done at the end of redirect_referred_from().
 */
BIF_RETTYPE hipe_bifs_mark_referred_from_1(BIF_ALIST_1) /* get_refs_from */
{
    struct mfa_t mfa;
    const struct hipe_mfa_info *p;
    struct ref *ref;

    if (!term_to_mfa(BIF_ARG_1, &mfa))
	BIF_ERROR(BIF_P, BADARG);
    hipe_mfa_info_table_rwlock();
    p = hipe_mfa_info_table_get_locked(mfa.mod, mfa.fun, mfa.ari);
    if (p)
	for (ref = p->referred_from; ref != NULL; ref = ref->next)
	    ref->flags |= REF_FLAG_PENDING_REDIRECT;
    hipe_mfa_info_table_rwunlock();
    BIF_RET(NIL);
}

/* Called by init:restart after unloading all hipe compiled modules
 * to work around bug causing execution of deallocated beam code.
 * Can be removed when delete/purge of native modules works better.
 * Test: Do init:restart in debug compiled vm with hipe compiled kernel. 
 */
static void hipe_purge_all_refs(void)
{
    struct hipe_mfa_info **bucket;
    unsigned int i, nrbuckets;

    hipe_mfa_info_table_rwlock();

    bucket = hipe_mfa_info_table.bucket;
    nrbuckets = 1 << hipe_mfa_info_table.log2size;
    for (i = 0; i < nrbuckets; ++i) {	
	while (bucket[i] != NULL) {
	    struct hipe_mfa_info* mfa = bucket[i];
	    bucket[i] = mfa->bucket.next;

	    while (mfa->refers_to) {
		struct hipe_mfa_info_list *to = mfa->refers_to;
		mfa->refers_to = to->next;
		erts_free(ERTS_ALC_T_HIPE, to);
	    }
	    while (mfa->referred_from) {
		struct ref* from = mfa->referred_from;
		mfa->referred_from = from->next;
		erts_free(ERTS_ALC_T_HIPE, from);
	    }
	    erts_free(ERTS_ALC_T_HIPE, mfa);
	}
    }
    hipe_mfa_info_table_rwunlock();
}

BIF_RETTYPE hipe_bifs_remove_refs_from_1(BIF_ALIST_1)
{
    struct mfa_t mfa;
    struct hipe_mfa_info *caller_mfa, *callee_mfa;
    struct hipe_mfa_info_list *refers_to, *tmp_refers_to;
    struct ref **prev, *ref;

    if (BIF_ARG_1 == am_all) {
	hipe_purge_all_refs();
	BIF_RET(am_ok);
    }

    if (!term_to_mfa(BIF_ARG_1, &mfa))
	BIF_ERROR(BIF_P, BADARG);
    hipe_mfa_info_table_rwlock();
    caller_mfa = hipe_mfa_info_table_get_locked(mfa.mod, mfa.fun, mfa.ari);
    if (caller_mfa) {
	refers_to = caller_mfa->refers_to;
	while (refers_to) {
	    callee_mfa = refers_to->mfa;
	    prev = &callee_mfa->referred_from;
	    ref = *prev;
	    while (ref) {
		if (ref->caller_mfa == caller_mfa) {
		    if (ref->flags & REF_FLAG_PENDING_REDIRECT) {
			ref->flags |= REF_FLAG_PENDING_REMOVE;
			prev = &ref->next;
			ref = ref->next;
		    } else {
			struct ref *tmp = ref;
			ref = ref->next;
			*prev = ref;
			erts_free(ERTS_ALC_T_HIPE, tmp);
		    }
		} else {
		    prev = &ref->next;
		    ref = ref->next;
		}
	    }
	    tmp_refers_to = refers_to;
	    refers_to = refers_to->next;
	    erts_free(ERTS_ALC_T_HIPE, tmp_refers_to);
	}
	caller_mfa->refers_to = NULL;
    }
    hipe_mfa_info_table_rwunlock();
    BIF_RET(am_ok);
}


/* redirect_referred_from(CalleeMFA)
 * Redirect all pending-redirect refs in CalleeMFA's referred_from.
 * Then remove any pending-redirect && pending-remove refs from CalleeMFA's referred_from.
 */
BIF_RETTYPE hipe_bifs_redirect_referred_from_1(BIF_ALIST_1)
{
    struct mfa_t mfa;
    struct hipe_mfa_info *p;
    struct ref **prev, *ref;
    int is_remote, res;
    void *new_address;

    if (!term_to_mfa(BIF_ARG_1, &mfa))
	BIF_ERROR(BIF_P, BADARG);
    hipe_mfa_info_table_rwlock();
    p = hipe_mfa_info_table_get_locked(mfa.mod, mfa.fun, mfa.ari);
    if (p) {
	prev = &p->referred_from;
	ref = *prev;
	while (ref) {
	    if (ref->flags & REF_FLAG_PENDING_REDIRECT) {
		is_remote = ref->flags & REF_FLAG_IS_REMOTE;
		new_address = hipe_get_na_nofail_rwlocked(p->m, p->f, p->a, is_remote);
		if (ref->flags & REF_FLAG_IS_LOAD_MFA)
		    res = hipe_patch_insn(ref->address, (Uint)new_address, am_load_mfa);
		else
		    res = hipe_patch_call(ref->address, new_address, ref->trampoline);
		if (res)
		    fprintf(stderr, "%s: patch failed\r\n", __FUNCTION__);
		ref->flags &= ~REF_FLAG_PENDING_REDIRECT;
		if (ref->flags & REF_FLAG_PENDING_REMOVE) {
		    struct ref *tmp = ref;
		    ref = ref->next;
		    *prev = ref;
		    erts_free(ERTS_ALC_T_HIPE, tmp);
		} else {
		    prev = &ref->next;
		    ref = ref->next;
		}
	    } else {
		prev = &ref->next;
		ref = ref->next;
	    }
	}
    }
    hipe_mfa_info_table_rwunlock();
    BIF_RET(NIL);
}

BIF_RETTYPE hipe_bifs_check_crc_1(BIF_ALIST_1)
{
    Uint crc;

    if (!term_to_Uint(BIF_ARG_1, &crc))
	BIF_ERROR(BIF_P, BADARG);
    if (crc == HIPE_ERTS_CHECKSUM)
	BIF_RET(am_true);
    BIF_RET(am_false);
}

BIF_RETTYPE hipe_bifs_system_crc_0(BIF_ALIST_0)
{
    Uint crc;

    crc = HIPE_SYSTEM_CRC;
    BIF_RET(Uint_to_term(crc, BIF_P));
}

BIF_RETTYPE hipe_bifs_get_rts_param_1(BIF_ALIST_1)
{
    unsigned int is_defined;
    unsigned long value;

    if (is_not_small(BIF_ARG_1))
	BIF_ERROR(BIF_P, BADARG);
    is_defined = 1;
    value = 0;
    switch (unsigned_val(BIF_ARG_1)) {
	RTS_PARAMS_CASES
      default:
	BIF_ERROR(BIF_P, BADARG);
    }
    if (!is_defined)
	BIF_RET(NIL);
    BIF_RET(Uint_to_term(value, BIF_P));
}

void hipe_patch_address(Uint *address, Eterm patchtype, Uint value)
{
    switch (patchtype) {
      case am_load_fe:
	hipe_patch_load_fe(address, value);
	return;
      default:
	fprintf(stderr, "%s: unknown patchtype %#lx\r\n",
		__FUNCTION__, patchtype);
	return;
    }
}

struct modinfo {
    HashBucket bucket;		/* bucket.hvalue == atom_val(the module name) */
    unsigned int code_size;
};

static Hash modinfo_table;

static HashValue modinfo_hash(void *tmpl)
{
    Eterm mod = (Eterm)tmpl;
    return atom_val(mod);
}

static int modinfo_cmp(void *tmpl, void *bucket)
{
    /* bucket->hvalue == modinfo_hash(tmpl), so just return 0 (match) */
    return 0;
}

static void *modinfo_alloc(void *tmpl)
{
    struct modinfo *p;

    p = (struct modinfo*)erts_alloc(ERTS_ALC_T_HIPE, sizeof(*p));
    p->code_size = 0;
    return &p->bucket;
}

static void init_modinfo_table(void)
{
    HashFunctions f;
    static int init_done = 0;

    if (init_done)
	return;
    init_done = 1;
    f.hash = (H_FUN) modinfo_hash;
    f.cmp = (HCMP_FUN) modinfo_cmp;
    f.alloc = (HALLOC_FUN) modinfo_alloc;
    f.free = (HFREE_FUN) NULL;
    f.meta_alloc = (HMALLOC_FUN) erts_alloc;
    f.meta_free = (HMFREE_FUN) erts_free;
    f.meta_print = (HMPRINT_FUN) erts_print;
    hash_init(ERTS_ALC_T_HIPE, &modinfo_table, "modinfo_table", 11, f);
}

BIF_RETTYPE hipe_bifs_update_code_size_3(BIF_ALIST_3)
{
    struct modinfo *p;
    Sint code_size;

    init_modinfo_table();

    if (is_not_atom(BIF_ARG_1) ||
	is_not_small(BIF_ARG_3) ||
	(code_size = signed_val(BIF_ARG_3)) < 0)
	BIF_ERROR(BIF_P, BADARG);

    p = (struct modinfo*)hash_put(&modinfo_table, (void*)BIF_ARG_1);

    if (is_nil(BIF_ARG_2))	/* some MFAs, not whole module */
	p->code_size += code_size;
    else			/* whole module */
	p->code_size = code_size;
    BIF_RET(NIL);
}

BIF_RETTYPE hipe_bifs_code_size_1(BIF_ALIST_1)
{
    struct modinfo *p;
    unsigned int code_size;

    init_modinfo_table();

    if (is_not_atom(BIF_ARG_1))
	BIF_ERROR(BIF_P, BADARG);

    p = (struct modinfo*)hash_get(&modinfo_table, (void*)BIF_ARG_1);

    code_size = p ? p->code_size : 0;
    BIF_RET(make_small(code_size));
}

BIF_RETTYPE hipe_bifs_patch_insn_3(BIF_ALIST_3)
{
    Uint *address, value;

    address = term_to_address(BIF_ARG_1);
    if (!address)
	BIF_ERROR(BIF_P, BADARG);
    if (!term_to_Uint(BIF_ARG_2, &value))
	BIF_ERROR(BIF_P, BADARG);
    if (hipe_patch_insn(address, value, BIF_ARG_3))
	BIF_ERROR(BIF_P, BADARG);
    BIF_RET(NIL);
}

BIF_RETTYPE hipe_bifs_patch_call_3(BIF_ALIST_3)
{
    Uint *callAddress, *destAddress, *trampAddress;

    callAddress = term_to_address(BIF_ARG_1);
    if (!callAddress)
	BIF_ERROR(BIF_P, BADARG);
    destAddress = term_to_address(BIF_ARG_2);
    if (!destAddress)
	BIF_ERROR(BIF_P, BADARG);
    if (is_nil(BIF_ARG_3))
	trampAddress = NULL;
    else {
	trampAddress = term_to_address(BIF_ARG_3);
	if (!trampAddress)
	    BIF_ERROR(BIF_P, BADARG);
    }
    if (hipe_patch_call(callAddress, destAddress, trampAddress))
	BIF_ERROR(BIF_P, BADARG);
    BIF_RET(NIL);
}
