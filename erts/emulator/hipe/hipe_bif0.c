/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2001-2017. All Rights Reserved.
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
#include "hipe_load.h"
/* We need hipe_literals.h for HIPE_SYSTEM_CRC, but it redefines
   a few constants. #undef them here to avoid warnings. */
#undef F_TIMO
#undef THE_NON_VALUE
#undef ERL_FUN_SIZE
#include "hipe_literals.h"
#endif


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
 * BIFs for loading code.
 */

static HipeLoaderState *get_loader_state(Eterm term)
{
    if (!is_internal_magic_ref(term)) return NULL;

    return hipe_get_loader_state(erts_magic_ref2bin(term));
}


/*
 * Allocate memory and copy machine code to it.
 */
BIF_RETTYPE hipe_bifs_enter_code_3(BIF_ALIST_3)
{
    Uint nrbytes;
    void *bytes;
    void *address;
    Eterm trampolines;
    Eterm *hp;
    HipeLoaderState *stp;
#ifndef DEBUG
    ERTS_DECLARE_DUMMY(Uint bitoffs);
    ERTS_DECLARE_DUMMY(Uint bitsize);
#else
    Uint bitoffs;
    Uint bitsize;
#endif

    if (is_not_binary(BIF_ARG_1) ||
	(!(stp = get_loader_state(BIF_ARG_3))))
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
	// XXX: Is there any reason to not just BIF_ERROR, so that the runtime
	// survives?
	erts_exit(ERTS_ERROR_EXIT, "%s: failed to allocate %lu bytes and %lu trampolines\r\n",
		 __func__, (unsigned long)nrbytes, (unsigned long)nrcallees);
    }
    memcpy(address, bytes, nrbytes);
    hipe_flush_icache_range(address, nrbytes);
    stp->text_segment = address;
    stp->text_segment_size = nrbytes;
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
BIF_RETTYPE hipe_bifs_alloc_data_3(BIF_ALIST_3)
{
    Uint align;
    HipeLoaderState *stp;
    void *aligned_block;

    if (is_not_small(BIF_ARG_1) || is_not_small(BIF_ARG_2) ||
	(!(stp = get_loader_state(BIF_ARG_3))) ||
	(align = unsigned_val(BIF_ARG_1), !IS_POWER_OF_TWO(align)))
	BIF_ERROR(BIF_P, BADARG);

    if (stp->data_segment_size || stp->data_segment)
	BIF_ERROR(BIF_P, BADARG);

    stp->data_segment_size = unsigned_val(BIF_ARG_2);
    if (stp->data_segment_size == 0)
	BIF_RET(make_small(0));

    stp->data_segment_size += align-1; /* Make room to align the pointer */
    stp->data_segment = erts_alloc(ERTS_ALC_T_HIPE_LL, stp->data_segment_size);

    /* Align the pointer */
    aligned_block = (void*)((UWord)(stp->data_segment + align - 1)
			    & ~(UWord)(align-1));
    BIF_RET(address_to_term(aligned_block, BIF_P));
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
    hash_init(ERTS_ALC_T_HIPE_LL, &const_term_table, "const_term_table", 97, f);
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

struct hipe_mfa {
    Eterm mod;
    Eterm fun;
    Uint  ari;
};

static int term_to_mfa(Eterm term, struct hipe_mfa *mfa)
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

static ErtsCodeInfo* hipe_find_emu_address(Eterm mod, Eterm name, unsigned int arity)
{
    Module *modp;
    BeamCodeHeader* code_hdr;
    int i, n;

    modp = erts_get_module(mod, erts_active_code_ix());
    if (modp == NULL || (code_hdr = modp->curr.code_hdr) == NULL)
	return NULL;
    n = code_hdr->num_functions;
    for (i = 0; i < n; ++i) {
	ErtsCodeInfo *ci = code_hdr->functions[i];
	ASSERT(BeamIsOpCode(ci->op, op_i_func_info_IaaI));
	if (ci->mfa.function == name && ci->mfa.arity == arity)
	    return ci;
    }
    return NULL;
}

ErtsCodeInfo* hipe_bifs_find_pc_from_mfa(Eterm term)
{
    struct hipe_mfa mfa;

    if (!term_to_mfa(term, &mfa))
	return NULL;
    return hipe_find_emu_address(mfa.mod, mfa.fun, mfa.ari);
}

BIF_RETTYPE hipe_bifs_fun_to_address_1(BIF_ALIST_1)
{
    ErtsCodeInfo* ci = hipe_bifs_find_pc_from_mfa(BIF_ARG_1);
    if (!ci)
	BIF_ERROR(BIF_P, BADARG);
    BIF_RET(address_to_term(erts_codeinfo_to_code(ci), BIF_P));
}

BIF_RETTYPE hipe_bifs_commit_patch_load_1(BIF_ALIST_1)
{
    if (!erts_commit_hipe_patch_load(BIF_ARG_1))
        BIF_ERROR(BIF_P, BADARG);

    BIF_RET(am_ok);
}

BIF_RETTYPE hipe_bifs_set_native_address_3(BIF_ALIST_3)
{
    ErtsCodeInfo *ci;
    void *address;
    int is_closure;
    struct hipe_mfa mfa;

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
    ci = hipe_find_emu_address(mfa.mod, mfa.fun, mfa.ari);

    if (ci) {
	DBG_TRACE_MFA(mfa.mod,mfa.fun,mfa.ari, "set beam call trap at %p -> %p",
                      erts_codeinfo_to_code(ci), address);
	hipe_set_call_trap(ci, address, is_closure);
	BIF_RET(am_true);
    }
    DBG_TRACE_MFA(mfa.mod,mfa.fun,mfa.ari, "failed set call trap to %p, no beam code found", address);
    BIF_RET(am_false);
}

BIF_RETTYPE hipe_bifs_enter_sdesc_2(BIF_ALIST_2)
{
    struct hipe_sdesc *sdesc;
    HipeLoaderState* stp;

    stp = get_loader_state(BIF_ARG_2);
    if (!stp)
	BIF_ERROR(BIF_P, BADARG);

    sdesc = hipe_decode_sdesc(BIF_ARG_1);
    if (!sdesc) {
	fprintf(stderr, "%s: bad sdesc!\r\n", __FUNCTION__);
	BIF_ERROR(BIF_P, BADARG);
    }
    if (hipe_put_sdesc(sdesc) != sdesc) {
	fprintf(stderr, "%s: duplicate entry!\r\n", __FUNCTION__);
	BIF_ERROR(BIF_P, BADARG);
    }

    /*
     * Link into list of sdesc's in same module instance
     */
    sdesc->next_in_modi = stp->new_hipe_sdesc;
    stp->new_hipe_sdesc = sdesc;

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
#define BIF_LIST(MOD,FUN,ARY,BIF,CFUN,IX)	\
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

    hash_init(ERTS_ALC_T_HIPE_LL, &primop_table, "primop_table", 50, f);

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
/* Called via standard_bif_interface_1 */
BIF_RETTYPE nbif_impl_hipe_conv_big_to_float(NBIF_ALIST_1)
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
	printf("no fun entry for %s %ld:%ld\n", atom_buf, (unsigned long)uniq, (unsigned long)index);
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

struct hipe_ref_head {
    struct hipe_ref_head* next;
    struct hipe_ref_head* prev;
};

/*
 * An exported function called from or implemented by native code
 * - maps MFA to native code entry point
 * - all references to it (callers)
 * - maps MFA to most recent trampoline [if powerpc or arm]
 */
struct hipe_mfa_info {
    HashBucket mod2mfa;
    struct {
	unsigned long hvalue;
	struct hipe_mfa_info *next;
    } bucket;
    Eterm m;	/* atom */
    Eterm f;	/* atom */
    unsigned int a : sizeof(int)*8 - 1;
    unsigned int is_stub : 1;       /* if beam or not (yet) loaded */
    void *remote_address;
    void *new_address;
    struct hipe_ref_head callers;   /* sentinel in list of hipe_ref's */
    struct hipe_mfa_info* next_in_mod;
#ifdef DEBUG
    Export* dbg_export;
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
    erts_rwmtx_t lock;
} hipe_mfa_info_table;

Hash mod2mfa_tab; /* map from module atom to list of hipe_mfa_info */

static HashValue mod2mfa_hash(struct hipe_mfa_info* mfa)
{
    return mfa->mod2mfa.hvalue;
}

static int mod2mfa_cmp(HashBucket* tmpl, struct hipe_mfa_info* mfa)
{
    return tmpl->hvalue != mfa->mod2mfa.hvalue;
}

static struct hipe_mfa_info* mod2mfa_alloc(struct hipe_mfa_info* tmpl)
{
    return tmpl; /* hash_put always use mfa itself at template */
}

static void mod2mfa_free(struct hipe_mfa_info* mfa)
{
}

static void mod2mfa_tab_init(void)
{
    HashFunctions f;
    static int init_done = 0;

    if (init_done)
	return;
    init_done = 1;

    f.hash = (H_FUN) mod2mfa_hash;
    f.cmp = (HCMP_FUN) mod2mfa_cmp;
    f.alloc = (HALLOC_FUN) mod2mfa_alloc;
    f.free = (HFREE_FUN) mod2mfa_free;
    f.meta_alloc = (HMALLOC_FUN) erts_alloc;
    f.meta_free = (HMFREE_FUN) erts_free;
    f.meta_print = (HMPRINT_FUN) erts_print;

    hash_init(ERTS_ALC_T_HIPE_LL, &mod2mfa_tab, "mod2mfa_tab", 50, f);
}

static struct hipe_mfa_info* mod2mfa_get(Module* modp)
{
    HashBucket tmpl;
    tmpl.hvalue = modp->module;
    return hash_get(&mod2mfa_tab, &tmpl);
}

static struct hipe_mfa_info* mod2mfa_put(struct hipe_mfa_info* mfa)
{
    mfa->mod2mfa.hvalue = atom_val(mfa->m);
    return hash_put(&mod2mfa_tab, mfa);
}



/*
 * An external native call site M:F(...)
 * to be patched when the callee changes.
 */
struct hipe_ref {
    struct hipe_ref_head head;    /* list of refs to same calleee */
    void *address;
#if defined(__x86_64__) || defined(__arm__) || defined(__powerpc__) || defined(__ppc__) || defined(__powerpc64__)
    void *trampoline;
#endif
    unsigned int flags;
    struct hipe_ref* next_from_modi; /* list of refs from same module instance */
#if defined(DEBUG)
    struct hipe_mfa_info* callee;
    Eterm caller_m, caller_f, caller_a;
#endif
};
#define REF_FLAG_IS_LOAD_MFA		1	/* bit 0: 0 == call, 1 == load_mfa */


static inline void hipe_mfa_info_table_init_lock(void)
{
    erts_rwmtx_init(&hipe_mfa_info_table.lock, "hipe_mfait_lock", NIL,
        ERTS_LOCK_FLAGS_PROPERTY_STATIC | ERTS_LOCK_FLAGS_CATEGORY_GENERIC);
}

static inline void hipe_mfa_info_table_rlock(void)
{
    erts_rwmtx_rlock(&hipe_mfa_info_table.lock);
}

static inline void hipe_mfa_info_table_runlock(void)
{
    erts_rwmtx_runlock(&hipe_mfa_info_table.lock);
}

static inline void hipe_mfa_info_table_rwlock(void)
{
    erts_rwmtx_rwlock(&hipe_mfa_info_table.lock);
}

static inline void hipe_mfa_info_table_rwunlock(void)
{
    erts_rwmtx_rwunlock(&hipe_mfa_info_table.lock);
}

static ERTS_INLINE
struct hipe_mfa_info* mod2mfa_get_safe(Module* modp)
{
    struct hipe_mfa_info* mfa;
    hipe_mfa_info_table_rlock();
    mfa = mod2mfa_get(modp);
    hipe_mfa_info_table_runlock();
    return mfa;
}

#define HIPE_MFA_HASH(M,F,A)	(atom_val(M) ^ atom_val(F) ^ (A))

static struct hipe_mfa_info **hipe_mfa_info_table_alloc_bucket(unsigned int size)
{
    unsigned long nbytes = size * sizeof(struct hipe_mfa_info*);
    struct hipe_mfa_info **bucket = erts_alloc(ERTS_ALC_T_HIPE_LL, nbytes);
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
    erts_free(ERTS_ALC_T_HIPE_LL, old_bucket);
}

static struct hipe_mfa_info *hipe_mfa_info_table_alloc(Eterm m, Eterm f, unsigned int arity)
{
    struct hipe_mfa_info *res;

    res = (struct hipe_mfa_info*)erts_alloc(ERTS_ALC_T_HIPE_LL, sizeof(*res));
    res->m = m;
    res->f = f;
    res->a = arity;
    res->is_stub = 0;
    res->remote_address = NULL;
    res->new_address = NULL;
    res->callers.next = &res->callers;
    res->callers.prev = &res->callers;
    res->next_in_mod = NULL;
#ifdef DEBUG
    res->dbg_export = NULL;
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

    mod2mfa_tab_init();
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

static struct hipe_mfa_info *hipe_mfa_info_table_put_rwlocked(Eterm m, Eterm f, unsigned int arity)
{
    unsigned long h;
    unsigned int i;
    struct hipe_mfa_info *p;
    struct hipe_mfa_info *first_in_mod;
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

    first_in_mod = mod2mfa_put(p);
    if (p != first_in_mod) {
        p->next_in_mod = first_in_mod->next_in_mod;
        first_in_mod->next_in_mod = p;
    }
    else {
        p->next_in_mod = NULL;
    }

    DBG_TRACE_MFA(m,f,arity, "hipe_mfa_info allocated at %p", p);

    return p;
}

static void remove_mfa_info(struct hipe_mfa_info* rm)
{
    unsigned int i;
    struct hipe_mfa_info *p;
    struct hipe_mfa_info **prevp;

    i = rm->bucket.hvalue & hipe_mfa_info_table.mask;
    prevp = &hipe_mfa_info_table.bucket[i];
    for (;;) {
        p = *prevp;
        ASSERT(p);
        if (p == rm) {
            *prevp = p->bucket.next;
            ASSERT(hipe_mfa_info_table.used > 0);
            hipe_mfa_info_table.used--;
            return;
        }
        prevp = &p->bucket.next;
    }
}

static void hipe_mfa_set_na(Eterm m, Eterm f, unsigned int arity, void *address)
{
    struct hipe_mfa_info *p;

    hipe_mfa_info_table_rwlock();
    p = hipe_mfa_info_table_put_rwlocked(m, f, arity);
    DBG_TRACE_MFA(m,f,arity,"set native address in hipe_mfa_info at %p", p);
    p->new_address = address;

    hipe_mfa_info_table_rwunlock();
}

BIF_RETTYPE hipe_bifs_set_funinfo_native_address_3(BIF_ALIST_3)
{
    struct hipe_mfa mfa;
    void *address;

    switch (BIF_ARG_3) {
    case am_true: /* is_exported */
        if (!term_to_mfa(BIF_ARG_1, &mfa))
            BIF_ERROR(BIF_P, BADARG);
        address = term_to_address(BIF_ARG_2);
        if (!address)
            BIF_ERROR(BIF_P, BADARG);
        hipe_mfa_set_na(mfa.mod, mfa.fun, mfa.ari, address);
        break;
    case am_false:
        break; /* ignore local functions */
    default:
	BIF_ERROR(BIF_P, BADARG);
    }
    BIF_RET(NIL);
}


/* Ask if we need to block all threads
 * while loading/deleting code for this module?
 */
int hipe_need_blocking(Module* modp)
{
    struct hipe_mfa_info *p;

    /* Need to block if we have at least one native caller to this module
     * or native code to make unaccessible.
     */
    hipe_mfa_info_table_rlock();
    for (p = mod2mfa_get(modp); p; p = p->next_in_mod) {
        ASSERT(!p->new_address);
        if (p->callers.next != &p->callers || !p->is_stub) {
            break;
	}
    }
    hipe_mfa_info_table_runlock();
    return (p != NULL);
}

static void *hipe_get_na_try_locked(Eterm m, Eterm f, unsigned int a)
{
    struct hipe_mfa_info *p;

    p = hipe_mfa_info_table_get_locked(m, f, a);
    return p ? p->remote_address : NULL;
}

static void *hipe_get_na_slow_rwlocked(Eterm m, Eterm f, unsigned int a)
{
    struct hipe_mfa_info *p = hipe_mfa_info_table_put_rwlocked(m, f, a);

    if (!p->remote_address) {
        Export* export_entry = erts_export_get_or_make_stub(m, f, a);
        void* stubAddress = hipe_make_native_stub(export_entry, a);
        if (!stubAddress)
            erts_exit(ERTS_ERROR_EXIT, "hipe_make_stub: code allocation failed\r\n");

        p->remote_address = stubAddress;
        p->is_stub = 1;
#ifdef DEBUG
        p->dbg_export = export_entry;
#endif
    }
    return p->remote_address;
}

static void *hipe_get_na_nofail(Eterm m, Eterm f, unsigned int a)
{
    void *address;

    hipe_mfa_info_table_rlock();
    address = hipe_get_na_try_locked(m, f, a);
    hipe_mfa_info_table_runlock();
    if (address)
	return address;

    hipe_mfa_info_table_rwlock();
    address = hipe_get_na_slow_rwlocked(m, f, a);
    hipe_mfa_info_table_rwunlock();
    return address;
}

/* used for apply/3 in hipe_mode_switch */
void *hipe_get_remote_na(Eterm m, Eterm f, unsigned int a)
{
    if (is_not_atom(m) || is_not_atom(f) || a > 255)
	return NULL;
    return hipe_get_na_nofail(m, f, a);
}

/* primop, but called like a BIF for error handling purposes */
/* Called via standard_bif_interface_3 */
BIF_RETTYPE nbif_impl_hipe_find_na_or_make_stub(NBIF_ALIST_3)
{
    Uint arity;
    void *address;

    if (is_not_atom(BIF_ARG_1) || is_not_atom(BIF_ARG_2))
	BIF_ERROR(BIF_P, BADARG);
    arity = unsigned_val(BIF_ARG_3); /* no error check */
    address = hipe_get_na_nofail(BIF_ARG_1, BIF_ARG_2, arity);
    BIF_RET((Eterm)address);	/* semi-Ok */
}

BIF_RETTYPE hipe_bifs_find_na_or_make_stub_1(BIF_ALIST_1)
{
    struct hipe_mfa mfa;
    void *address;

    if (!term_to_mfa(BIF_ARG_1, &mfa))
	BIF_ERROR(BIF_P, BADARG);

    address = hipe_get_na_nofail(mfa.mod, mfa.fun, mfa.ari);
    BIF_RET(address_to_term(address, BIF_P));
}

/* primop, but called like a BIF for error handling purposes */
/* Called via standard_bif_interface_2 */
BIF_RETTYPE nbif_impl_hipe_nonclosure_address(NBIF_ALIST_2)
{
    Eterm hdr, m, f;
    void *address;

    if (!is_boxed(BIF_ARG_1))
	goto badfun;
    hdr = *boxed_val(BIF_ARG_1);
    if (is_export_header(hdr)) {
	Export *ep = (Export*)(export_val(BIF_ARG_1)[1]);
	unsigned int actual_arity = ep->info.mfa.arity;
	if (actual_arity != BIF_ARG_2)
	    goto badfun;
	m = ep->info.mfa.module;
	f = ep->info.mfa.function;
    } else
	goto badfun;
    address = hipe_get_na_nofail(m, f, BIF_ARG_2);
    BIF_RET((Eterm)address);

 badfun:
    BIF_P->current = NULL;
    BIF_P->fvalue = BIF_ARG_1;
    BIF_ERROR(BIF_P, EXC_BADFUN);
}

int hipe_find_mfa_from_ra(const void *ra, Eterm *m, Eterm *f, unsigned int *a)
{
    const struct hipe_sdesc* sdesc = hipe_find_sdesc((unsigned long)ra);

    if (!sdesc || sdesc->m_aix == atom_val(am_Empty))
        return 0;

    *m = make_atom(sdesc->m_aix);
    *f = make_atom(sdesc->f_aix);
    *a = sdesc->a;
    return 1;
}


/* add_ref(CalleeMFA, {CallerMFA,Address,'call'|'load_mfa',Trampoline,LoaderState})
 */
BIF_RETTYPE hipe_bifs_add_ref_2(BIF_ALIST_2)
{
    struct hipe_mfa callee;
    Eterm *tuple;
    struct hipe_mfa caller;
    void *address;
    void *trampoline;
    unsigned int flags;
    struct hipe_mfa_info *callee_mfa;
    struct hipe_ref *ref;
    HipeLoaderState* stp;

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
    stp = get_loader_state(tuple[5]);
    if (!stp)
        goto badarg;

    hipe_mfa_info_table_rwlock();
    callee_mfa = hipe_mfa_info_table_put_rwlocked(callee.mod, callee.fun, callee.ari);

    ref = erts_alloc(ERTS_ALC_T_HIPE_LL, sizeof(struct hipe_ref));
    ref->address = address;
#if defined(__x86_64__) || defined(__arm__) || defined(__powerpc__) || defined(__ppc__) || defined(__powerpc64__)
    ref->trampoline = trampoline;
#endif
    ref->flags = flags;

    /*
     * Link into list of refs to same callee
     */
    ASSERT(callee_mfa->callers.next->prev == &callee_mfa->callers);
    ASSERT(callee_mfa->callers.prev->next == &callee_mfa->callers);
    ref->head.next = callee_mfa->callers.next;
    ref->head.prev = &callee_mfa->callers;
    ref->head.next->prev = &ref->head;
    ref->head.prev->next = &ref->head;

    /*
     * Link into list of refs from same module instance
     */
    ref->next_from_modi = stp->new_hipe_refs;
    stp->new_hipe_refs = ref;

#if defined(DEBUG)
    ref->callee = callee_mfa;
    ref->caller_m = caller.mod;
    ref->caller_f = caller.fun;
    ref->caller_a = caller.ari;
#endif
    hipe_mfa_info_table_rwunlock();

    DBG_TRACE_MFA(caller.mod, caller.fun, caller.ari, "add_ref at %p TO %T:%T/%u (from %p)",
		  ref, callee.mod, callee.fun, callee.ari, ref->address);
    DBG_TRACE_MFA(callee.mod, callee.fun, callee.ari, "add_ref at %p FROM %T:%T/%u (from %p)",
		  ref, caller.mod, caller.fun, caller.ari, ref->address);
    BIF_RET(am_ok);

 badarg:
    BIF_ERROR(BIF_P, BADARG);
}


static void unlink_mfa_from_mod(struct hipe_mfa_info* unlink_me)
{
    struct hipe_mfa_info* p;

    p = hash_get(&mod2mfa_tab, unlink_me);
    ASSERT(p);
    if (p == unlink_me) {
        hash_erase(&mod2mfa_tab, p);
        if (p->next_in_mod)
            mod2mfa_put(p->next_in_mod);
    }
    else {
        struct hipe_mfa_info** prevp;

        do {
            prevp = &p->next_in_mod;
            p = *prevp;
            ASSERT(p && p->m == unlink_me->m);
        } while (p != unlink_me);

        *prevp = p->next_in_mod;
    }
}

static void purge_mfa(struct hipe_mfa_info* p)
{
    ASSERT(p->is_stub);
    remove_mfa_info(p);
    hipe_free_native_stub(p->remote_address);
    erts_free(ERTS_ALC_T_HIPE_LL, p);
}

int hipe_purge_need_blocking(Module* modp)
{
    /* SVERK: Verify if this is really necessary */
    if (modp->old.hipe_code) {
        if (modp->old.hipe_code->first_hipe_ref ||
            modp->old.hipe_code->first_hipe_sdesc)
            return 1;
    }
    if (!modp->curr.code_hdr) {
        return mod2mfa_get_safe(modp) != NULL;
    }
    return 0;
}

void hipe_purge_refs(struct hipe_ref* first_ref, Eterm caller_module,
                     int is_blocking)
{
    struct hipe_ref* ref = first_ref;

    ERTS_LC_ASSERT(is_blocking == erts_thr_progress_is_blocking());

    while (ref) {
        struct hipe_ref* free_ref = ref;

        DBG_TRACE_MFA(ref->caller_m, ref->caller_f, ref->caller_a, "PURGE ref at %p to %T:%T/%u", ref,
                      ref->callee->m, ref->callee->f, ref->callee->a);
        DBG_TRACE_MFA(ref->callee->m, ref->callee->f, ref->callee->a, "PURGE ref at %p from %T:%T/%u", ref,
                      ref->caller_m, ref->caller_f, ref->caller_a);
        ASSERT(ref->caller_m == caller_module);

        /*
         * Unlink from other refs to same callee
         */
        ASSERT(ref->head.next->prev == &ref->head);
        ASSERT(ref->head.prev->next == &ref->head);
        ASSERT(ref->head.next != &ref->head);
        ASSERT(ref->head.prev != &ref->head);
        ref->head.next->prev = ref->head.prev;
        ref->head.prev->next = ref->head.next;

        /*
         * Was this the last ref to that callee?
         */
        if (ref->head.next == ref->head.prev) {
            struct hipe_mfa_info* p = ErtsContainerStruct(ref->head.next, struct hipe_mfa_info, callers);
            if (p->is_stub) {
                if (!is_blocking)
                    hipe_mfa_info_table_rwlock();
                unlink_mfa_from_mod(p);
                purge_mfa(p);
                if (!is_blocking)
                    hipe_mfa_info_table_rwunlock();
            }
        }

        ref = ref->next_from_modi;
        erts_free(ERTS_ALC_T_HIPE_LL, free_ref);
    }
}

void hipe_purge_sdescs(struct hipe_sdesc* first_sdesc, Eterm module,
                       int is_blocking)
{
    struct hipe_sdesc* sdesc = first_sdesc;

    ERTS_LC_ASSERT(is_blocking == erts_thr_progress_is_blocking());

    ERTS_LC_ASSERT(is_blocking); /*XXX Fix safe sdesc destruction */

    while (sdesc) {
        struct hipe_sdesc* free_sdesc = sdesc;

        DBG_TRACE_MFA(make_atom(sdesc->m_aix), make_atom(sdesc->f_aix), sdesc->a, "PURGE sdesc at %p", (void*)sdesc->bucket.hvalue);
        ASSERT(make_atom(sdesc->m_aix) == module);

        sdesc = sdesc->next_in_modi;
        hipe_destruct_sdesc(free_sdesc);
    }
}


void hipe_purge_module(Module* modp, int is_blocking)
{
    ASSERT(modp);

    ERTS_LC_ASSERT(is_blocking == erts_thr_progress_is_blocking());

    DBG_TRACE_MFA(make_atom(modp->module), 0, 0, "hipe_purge_module");

    if (modp->old.hipe_code) {
        /*
         * Remove all hipe_ref's (external calls) from the old module instance
         */
        if (modp->old.hipe_code->first_hipe_ref) {
            ERTS_LC_ASSERT(is_blocking);

            hipe_purge_refs(modp->old.hipe_code->first_hipe_ref,
                            make_atom(modp->module), is_blocking);
            modp->old.hipe_code->first_hipe_ref = NULL;
        }

        /*
         * Remove all hipe_sdesc's for the old module instance
         */
        if (modp->old.hipe_code->first_hipe_sdesc) {
            ERTS_LC_ASSERT(is_blocking);

            hipe_purge_sdescs(modp->old.hipe_code->first_hipe_sdesc,
                             make_atom(modp->module), is_blocking);
            modp->old.hipe_code->first_hipe_sdesc = NULL;
        }

        hipe_free_module(modp->old.hipe_code);
        modp->old.hipe_code = NULL;
    }


    /*
     * Remove unreferred hipe_mfa_info's 
     * when all module instances are removed (like in init:restart) 
     */
    if (is_blocking && modp->curr.code_hdr == NULL) {
        struct hipe_mfa_info* was_first = mod2mfa_get(modp);
        struct hipe_mfa_info* is_first = was_first;
        struct hipe_mfa_info** prevp = &is_first;
        struct hipe_mfa_info *p;

        if (was_first) {
            for (p = was_first ; p; p = *prevp) {
                if (p->callers.next == &p->callers) {
                    *prevp = p->next_in_mod;
                    if (p != was_first)
                        purge_mfa(p);
                }
                else
                    prevp = &p->next_in_mod;
            }
            if (was_first != is_first) {
                hash_erase(&mod2mfa_tab, was_first);
                purge_mfa(was_first);
                if (is_first)
                    mod2mfa_put(is_first); 
            }
        }
    }
}


/*
 * Redirect all existing native calls to this module
 */
void hipe_redirect_to_module(Module* modp)
{
    struct hipe_mfa_info *p;
    struct hipe_ref_head* refh;

    ERTS_LC_ASSERT(erts_thr_progress_is_blocking() ||
                   erts_is_multi_scheduling_blocked());

    for (p = mod2mfa_get(modp); p; p = p->next_in_mod) {
        if (p->new_address) {
            if (p->is_stub) {
                hipe_free_native_stub(p->remote_address);
                p->is_stub = 0;
            }
            DBG_TRACE_MFA(p->m, p->f, p->a, "Commit new_address %p", p->new_address);
            p->remote_address = p->new_address;
            p->new_address = NULL;
#ifdef DEBUG
            p->dbg_export = NULL;
#endif
        }
        else if (!p->is_stub) {
            Export* exp = erts_export_get_or_make_stub(p->m, p->f, p->a);
            p->remote_address = hipe_make_native_stub(exp, p->a);
            DBG_TRACE_MFA(p->m, p->f, p->a, "Commit stub %p", p->remote_address);
            if (!p->remote_address)
                erts_exit(ERTS_ERROR_EXIT, "hipe_make_stub: code allocation failed\r\n");
            p->is_stub = 1;
#ifdef DEBUG
            p->dbg_export = exp;
#endif
        }
        else {
            DBG_TRACE_MFA(p->m, p->f, p->a, "Commit no-op, already stub");
            ASSERT(p->remote_address && p->dbg_export);
        }

	DBG_TRACE_MFA(p->m,p->f,p->a,"START REDIRECT towards hipe_mfa_info at %p", p);
	for (refh = p->callers.next; refh != &p->callers; refh = refh->next) {
            struct hipe_ref* ref = (struct hipe_ref*) refh;
	    int res;

	    DBG_TRACE_MFA(p->m,p->f,p->a, "  REDIRECT ref at %p FROM %T:%T/%u (%p -> %p)",
			  ref, ref->caller_m, ref->caller_f, ref->caller_a,
			  ref->address, p->remote_address);

	    DBG_TRACE_MFA(ref->caller_m, ref->caller_f, ref->caller_a,
			  "  REDIRECT ref at %p TO %T:%T/%u (%p -> %p)",
			  ref, p->m,p->f,p->a, ref->address, p->remote_address);

	    if (ref->flags & REF_FLAG_IS_LOAD_MFA)
		res = hipe_patch_insn(ref->address, (Uint)p->remote_address, am_load_mfa);
	    else {
#if defined(__x86_64__) || defined(__arm__) || defined(__powerpc__) || defined(__ppc__) || defined(__powerpc64__)
                void* trampoline = ref->trampoline;
#else
                void* trampoline = NULL;
#endif
		res = hipe_patch_call(ref->address, p->remote_address, trampoline);
            }
	    if (res)
		fprintf(stderr, "%s: patch failed", __FUNCTION__);
	}
	DBG_TRACE_MFA(p->m,p->f,p->a,"DONE REDIRECT towards hipe_mfa_info at %p", p);
    }
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

BIF_RETTYPE hipe_bifs_alloc_loader_state_1(BIF_ALIST_1)
{
    Binary *magic;
    Eterm *hp;
    Eterm res;

    if (is_not_atom(BIF_ARG_1))
	BIF_ERROR(BIF_P, BADARG);

    magic = hipe_alloc_loader_state(BIF_ARG_1);

    if (!magic)
	BIF_ERROR(BIF_P, BADARG);

    hp = HAlloc(BIF_P, ERTS_MAGIC_REF_THING_SIZE);
    res = erts_mk_magic_ref(&hp, &MSO(BIF_P), magic);
    erts_refc_dec(&magic->intern.refc, 1);
    BIF_RET(res);
}
