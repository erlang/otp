/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1996-2010. All Rights Reserved.
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

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "erl_vm.h"
#include "global.h"
#include "erl_process.h"
#include "error.h"
#include "bif.h"
#include "big.h"
#include "erl_binary.h"
#include "erl_bits.h"

#ifdef DEBUG
static int list_to_bitstr_buf(Eterm obj, char* buf, int len);
#else
static int list_to_bitstr_buf(Eterm obj, char* buf);
#endif
static Sint bitstr_list_len(Eterm obj);

void
erts_init_binary(void)
{
    /* Verify Binary alignment... */
    if ((((UWord) &((Binary *) 0)->orig_bytes[0]) % ((UWord) 8)) != 0) {
	/* I assume that any compiler should be able to optimize this
	   away. If not, this test is not very expensive... */
	erl_exit(ERTS_ABORT_EXIT,
		 "Internal error: Address of orig_bytes[0] of a Binary"
		 "is *not* 8-byte aligned\n");
    }
}

/*
 * Create a brand new binary from scratch.
 */

Eterm
new_binary(Process *p, byte *buf, int len)
{
    ProcBin* pb;
    Binary* bptr;

    if (len <= ERL_ONHEAP_BIN_LIMIT) {
	ErlHeapBin* hb = (ErlHeapBin *) HAlloc(p, heap_bin_size(len));
	hb->thing_word = header_heap_bin(len);
	hb->size = len;
	if (buf != NULL) {
	    sys_memcpy(hb->data, buf, len);
	}
	return make_binary(hb);
    }

    /*
     * Allocate the binary struct itself.
     */
    bptr = erts_bin_nrml_alloc(len);
    bptr->flags = 0;
    bptr->orig_size = len;
    erts_refc_init(&bptr->refc, 1);
    if (buf != NULL) {
	sys_memcpy(bptr->orig_bytes, buf, len);
    }

    /*
     * Now allocate the ProcBin on the heap.
     */
    pb = (ProcBin *) HAlloc(p, PROC_BIN_SIZE);
    pb->thing_word = HEADER_PROC_BIN;
    pb->size = len;
    pb->next = MSO(p).mso;
    MSO(p).mso = pb;
    pb->val = bptr;
    pb->bytes = (byte*) bptr->orig_bytes;
    pb->flags = 0;

    /*
     * Miscellanous updates. Return the tagged binary.
     */
    MSO(p).overhead += pb->size / sizeof(Eterm);
    return make_binary(pb);
}

/* 
 * When heap binary is not desired...
 */

Eterm erts_new_mso_binary(Process *p, byte *buf, int len)
{
    ProcBin* pb;
    Binary* bptr;

    /*
     * Allocate the binary struct itself.
     */
    bptr = erts_bin_nrml_alloc(len);
    bptr->flags = 0;
    bptr->orig_size = len;
    erts_refc_init(&bptr->refc, 1);
    if (buf != NULL) {
	sys_memcpy(bptr->orig_bytes, buf, len);
    }

    /*
     * Now allocate the ProcBin on the heap.
     */
    pb = (ProcBin *) HAlloc(p, PROC_BIN_SIZE);
    pb->thing_word = HEADER_PROC_BIN;
    pb->size = len;
    pb->next = MSO(p).mso;
    MSO(p).mso = pb;
    pb->val = bptr;
    pb->bytes = (byte*) bptr->orig_bytes;
    pb->flags = 0;

    /*
     * Miscellanous updates. Return the tagged binary.
     */
    MSO(p).overhead += pb->size / sizeof(Eterm);
    return make_binary(pb);
}

/*
 * Create a brand new binary from scratch on the heap.
 */

Eterm
erts_new_heap_binary(Process *p, byte *buf, int len, byte** datap)
{
    ErlHeapBin* hb = (ErlHeapBin *) HAlloc(p, heap_bin_size(len));

    hb->thing_word = header_heap_bin(len);
    hb->size = len;
    if (buf != NULL) {
	sys_memcpy(hb->data, buf, len);
    }
    *datap = (byte*) hb->data;
    return make_binary(hb);
}

Eterm
erts_realloc_binary(Eterm bin, size_t size)
{
    Eterm* bval = binary_val(bin);

    if (thing_subtag(*bval) == HEAP_BINARY_SUBTAG) {
	ASSERT(size <= binary_size(bin));
	binary_size(bin) = size;
    } else {			/* REFC */
	ProcBin* pb = (ProcBin *) bval;
	Binary* newbin = erts_bin_realloc(pb->val, size);
	newbin->orig_size = size;
	pb->val = newbin;
	pb->size = size;
	pb->bytes = (byte*) newbin->orig_bytes;
	pb->flags = 0;
	bin = make_binary(pb);
    }
    return bin;
}

byte*
erts_get_aligned_binary_bytes_extra(Eterm bin, byte** base_ptr, unsigned extra)
{
    byte* bytes;
    Eterm* real_bin;
    Uint byte_size;
    Uint offs = 0;
    Uint bit_offs = 0;
    
    if (is_not_binary(bin)) {
	return NULL;
    }
    byte_size = binary_size(bin);
    real_bin = binary_val(bin);
    if (*real_bin == HEADER_SUB_BIN) {
	ErlSubBin* sb = (ErlSubBin *) real_bin;
	if (sb->bitsize) {
	    return NULL;
	}
	offs = sb->offs;
	bit_offs = sb->bitoffs;
	real_bin = binary_val(sb->orig);
    }
    if (*real_bin == HEADER_PROC_BIN) {
	bytes = ((ProcBin *) real_bin)->bytes + offs;
    } else {
	bytes = (byte *)(&(((ErlHeapBin *) real_bin)->data)) + offs;
    }
    if (bit_offs) {
	byte* buf = (byte *) erts_alloc(ERTS_ALC_T_TMP, byte_size + extra);
	*base_ptr = buf;
	buf += extra;
	erts_copy_bits(bytes, bit_offs, 1, buf, 0, 1, byte_size*8);	
	bytes = buf;
    }
    return bytes;
}

static Eterm
bin_bytes_to_list(Eterm previous, Eterm* hp, byte* bytes, Uint size, Uint bitoffs)
{
    if (bitoffs == 0) {
	while (size) {
	    previous = CONS(hp, make_small(bytes[--size]), previous);
	    hp += 2;
	}
    } else {
	byte present;
	byte next;
	next = bytes[size];
	while (size) {
	    present = next;
	    next = bytes[--size];
	    previous = CONS(hp, make_small(((present >> (8-bitoffs)) |
					    (next << bitoffs)) & 255), previous);
	    hp += 2;
	}
    }
    return previous;
}


BIF_RETTYPE binary_to_list_1(BIF_ALIST_1)
{
    Eterm real_bin;
    Uint offset;
    Uint size;
    Uint bitsize;
    Uint bitoffs;

    if (is_not_binary(BIF_ARG_1)) {
	goto error;
    }
    size = binary_size(BIF_ARG_1);
    ERTS_GET_REAL_BIN(BIF_ARG_1, real_bin, offset, bitoffs, bitsize);
    if (bitsize != 0) {
	goto error;
    }
    if (size == 0) {
	BIF_RET(NIL);
    } else {
	Eterm* hp = HAlloc(BIF_P, 2 * size);
	byte* bytes = binary_bytes(real_bin)+offset;

	BIF_RET(bin_bytes_to_list(NIL, hp, bytes, size, bitoffs));
    }

    error:
	BIF_ERROR(BIF_P, BADARG);
}

BIF_RETTYPE binary_to_list_3(BIF_ALIST_3)
{
    byte* bytes;
    Uint size;
    Uint bitoffs;
    Uint bitsize;
    Uint i;
    Uint start;
    Uint stop;
    Eterm* hp;

    if (is_not_binary(BIF_ARG_1)) {
	goto error;
    }
    if (!term_to_Uint(BIF_ARG_2, &start) || !term_to_Uint(BIF_ARG_3, &stop)) {
	goto error;
    }
    size = binary_size(BIF_ARG_1);
    ERTS_GET_BINARY_BYTES(BIF_ARG_1, bytes, bitoffs, bitsize);
    if (start < 1 || start > size || stop < 1 ||
	stop > size || stop < start ) {
	goto error;
    }
    i = stop-start+1;
    hp = HAlloc(BIF_P, 2*i);
    BIF_RET(bin_bytes_to_list(NIL, hp, bytes+start-1, i, bitoffs));
    
    error:
	BIF_ERROR(BIF_P, BADARG);
}

BIF_RETTYPE bitstring_to_list_1(BIF_ALIST_1)
{
    Eterm real_bin;
    Uint offset;
    Uint size;
    Uint bitsize;
    Uint bitoffs;
    byte* bytes;
    Eterm previous = NIL;
    Eterm* hp;

    if (is_not_binary(BIF_ARG_1)) {
	BIF_ERROR(BIF_P, BADARG);
    }
    size = binary_size(BIF_ARG_1);
    ERTS_GET_REAL_BIN(BIF_ARG_1, real_bin, offset, bitoffs, bitsize);
    bytes = binary_bytes(real_bin)+offset;
    if (bitsize == 0) {
	hp = HAlloc(BIF_P, 2 * size);
    } else if (size == 0) {
	hp = HAlloc(BIF_P, 2);
	BIF_RET(CONS(hp,BIF_ARG_1,NIL));
    } else {
	ErlSubBin* last;

	hp = HAlloc(BIF_P, ERL_SUB_BIN_SIZE+2+2*size);
	last = (ErlSubBin *) hp;
	last->thing_word = HEADER_SUB_BIN;
	last->size = 0;
	last->bitsize = bitsize;
	last->offs = offset+size;
	last->bitoffs = bitoffs;
	last->orig = real_bin;
	last->is_writable = 0;
	hp += ERL_SUB_BIN_SIZE;
	previous = CONS(hp, make_binary(last), previous);
	hp += 2;
    }
    BIF_RET(bin_bytes_to_list(previous, hp, bytes, size, bitoffs));
}


/* Turn a possibly deep list of ints (and binaries) into */
/* One large binary object                               */

BIF_RETTYPE list_to_binary_1(BIF_ALIST_1)
{
    Eterm bin;
    int i;
    int offset;
    byte* bytes;
    if (is_nil(BIF_ARG_1)) {
	BIF_RET(new_binary(BIF_P,(byte*)"",0));
    }
    if (is_not_list(BIF_ARG_1)) {
	goto error;
    }
    if ((i = io_list_len(BIF_ARG_1)) < 0) {
	goto error;
    }
    bin = new_binary(BIF_P, (byte *)NULL, i);
    bytes = binary_bytes(bin);
    offset = io_list_to_buf(BIF_ARG_1, (char*) bytes, i);
    ASSERT(offset == 0);
    BIF_RET(bin);
    
    error:
	BIF_ERROR(BIF_P, BADARG);
}

/* Turn a possibly deep list of ints (and binaries) into */
/* One large binary object                               */

BIF_RETTYPE iolist_to_binary_1(BIF_ALIST_1)
{
    Eterm bin;
    int i;
    int offset;
    byte* bytes;

    if (is_binary(BIF_ARG_1)) {
	BIF_RET(BIF_ARG_1);
    }
    if (is_nil(BIF_ARG_1)) {
	BIF_RET(new_binary(BIF_P,(byte*)"",0));
    }
    if (is_not_list(BIF_ARG_1)) {
	goto error;
    }
    if ((i = io_list_len(BIF_ARG_1)) < 0) {
	goto error;
    }
    bin = new_binary(BIF_P, (byte *)NULL, i);
    bytes = binary_bytes(bin);
    offset = io_list_to_buf(BIF_ARG_1, (char*) bytes, i);
    ASSERT(offset == 0);
    BIF_RET(bin);
    
    error:
	BIF_ERROR(BIF_P, BADARG);
}

BIF_RETTYPE list_to_bitstring_1(BIF_ALIST_1)
{
    Eterm bin;
    int i,offset;
    byte* bytes;
    ErlSubBin* sb1; 
    Eterm* hp;
    
    if (is_nil(BIF_ARG_1)) {
	BIF_RET(new_binary(BIF_P,(byte*)"",0));
    }
    if (is_not_list(BIF_ARG_1)) {
	goto error;
    }
    if ((i = bitstr_list_len(BIF_ARG_1)) < 0) {
	goto error;
    }
    bin = new_binary(BIF_P, (byte *)NULL, i);
    bytes = binary_bytes(bin);
#ifdef DEBUG
    offset = list_to_bitstr_buf(BIF_ARG_1, (char*) bytes, i);
#else
    offset = list_to_bitstr_buf(BIF_ARG_1, (char*) bytes);
#endif
    ASSERT(offset >= 0);
    if (offset > 0) {
	hp = HAlloc(BIF_P, ERL_SUB_BIN_SIZE);
	sb1 = (ErlSubBin *) hp;
	sb1->thing_word = HEADER_SUB_BIN;
	sb1->size = i-1;
	sb1->offs = 0;
	sb1->orig = bin;
	sb1->bitoffs = 0;
	sb1->bitsize = offset;
	sb1->is_writable = 0;
	hp += ERL_SUB_BIN_SIZE;
	bin = make_binary(sb1);
    }
    
    BIF_RET(bin);
    
    error:
	BIF_ERROR(BIF_P, BADARG);
}

BIF_RETTYPE split_binary_2(BIF_ALIST_2)
{
    Uint pos;
    ErlSubBin* sb1;
    ErlSubBin* sb2;
    size_t orig_size;
    Eterm orig;
    Uint offset;
    Uint bit_offset;
    Uint bit_size;
    Eterm* hp;

    if (is_not_binary(BIF_ARG_1)) {
	goto error;
    }
    if (!term_to_Uint(BIF_ARG_2, &pos)) {
	goto error;
    }
    if ((orig_size = binary_size(BIF_ARG_1)) < pos) {
	goto error;
    }
    hp = HAlloc(BIF_P, 2*ERL_SUB_BIN_SIZE+3);
    ERTS_GET_REAL_BIN(BIF_ARG_1, orig, offset, bit_offset, bit_size);
    sb1 = (ErlSubBin *) hp;
    sb1->thing_word = HEADER_SUB_BIN;
    sb1->size = pos;
    sb1->offs = offset;
    sb1->orig = orig;
    sb1->bitoffs = bit_offset;
    sb1->bitsize = 0;
    sb1->is_writable = 0;
    hp += ERL_SUB_BIN_SIZE;

    sb2 = (ErlSubBin *) hp;
    sb2->thing_word = HEADER_SUB_BIN;
    sb2->size = orig_size - pos;
    sb2->offs = offset + pos;
    sb2->orig = orig;
    sb2->bitoffs = bit_offset;
    sb2->bitsize = bit_size;	/* The extra bits go into the second binary. */
    sb2->is_writable = 0;
    hp += ERL_SUB_BIN_SIZE;

    return TUPLE2(hp, make_binary(sb1), make_binary(sb2));
    
    error:
	BIF_ERROR(BIF_P, BADARG);
}

void
erts_cleanup_mso(ProcBin* pb)
{
    while (pb != NULL) {
	ProcBin* next = pb->next;
	if (erts_refc_dectest(&pb->val->refc, 0) == 0)
	    erts_bin_free(pb->val);
	pb = next;
    }
}

/*
 * Local functions.
 */

/*
 * The input list is assumed to be type-correct and the buffer is
 * assumed to be of sufficient size. Those assumptions are verified in
 * the DEBUG-built emulator.
 */
static int
#ifdef DEBUG
list_to_bitstr_buf(Eterm obj, char* buf, int len)
#else
list_to_bitstr_buf(Eterm obj, char* buf)
#endif
{
    Eterm* objp;
    int offset = 0;
    DECLARE_ESTACK(s);
    goto L_again;
    
    while (!ESTACK_ISEMPTY(s)) {
	obj = ESTACK_POP(s);
    L_again:
	if (is_list(obj)) {
	L_iter_list:
	    objp = list_val(obj);
	    obj = CAR(objp);
	    if (is_byte(obj)) {
		ASSERT(len > 0);
		if (offset == 0) {
		    *buf++ = unsigned_val(obj);
		} else {
		    *buf =  (char)((unsigned_val(obj) >> offset) | 
				   ((*buf >> (8-offset)) << (8-offset)));
		    buf++;
		    *buf = (unsigned_val(obj) << (8-offset));
		}   
#ifdef DEBUG
		len--;
#endif
	    } else if (is_binary(obj)) {
		byte* bptr;
		size_t size = binary_size(obj);
		Uint bitsize;
		Uint bitoffs;
		Uint num_bits;
		
		ASSERT(size <= len);
		ERTS_GET_BINARY_BYTES(obj, bptr, bitoffs, bitsize);
		num_bits = 8*size+bitsize;
		copy_binary_to_buffer(buf, offset, bptr, bitoffs, num_bits);
		offset += bitsize;
		buf += size + (offset>7);
#ifdef DEBUG
		len -= size + (offset>7);
#endif
		offset = offset & 7;
	    } else if (is_list(obj)) {
		ESTACK_PUSH(s, CDR(objp));
		goto L_iter_list; /* on head */
	    } else {
		ASSERT(is_nil(obj));
	    }

	    obj = CDR(objp);
	    if (is_list(obj)) {
		goto L_iter_list; /* on tail */
	    } else if (is_binary(obj)) {
		byte* bptr;
		size_t size = binary_size(obj);
		Uint bitsize;
		Uint bitoffs;
		Uint num_bits;

		ASSERT(size <= len);
		ERTS_GET_BINARY_BYTES(obj, bptr, bitoffs, bitsize);
		num_bits = 8*size+bitsize;
		copy_binary_to_buffer(buf, offset, bptr, bitoffs, num_bits);
		offset += bitsize;
		buf += size+(offset>7);
#ifdef DEBUG
		len -= size+(offset>7);
#endif
		offset = offset & 7;
	    } else {
		ASSERT(is_nil(obj));
	    }
	} else if (is_binary(obj)) {
	    byte* bptr;
	    size_t size = binary_size(obj);
	    Uint bitsize;
	    Uint bitoffs;
	    Uint num_bits;

	    ASSERT(size <= len);
	    ERTS_GET_BINARY_BYTES(obj, bptr, bitoffs, bitsize);
	    num_bits = 8*size+bitsize;
	    copy_binary_to_buffer(buf, offset, bptr, bitoffs, num_bits);
	    offset += bitsize;
	    buf += size + (offset>7);
#ifdef DEBUG
	    len -= size + (offset>7);
#endif
	    offset = offset & 7;
	} else {
	    ASSERT(is_nil(obj));
	}
    }
    
    DESTROY_ESTACK(s);
    return offset;
}

static Sint
bitstr_list_len(Eterm obj)
{
    Eterm* objp;
    Uint len = 0;
    Uint offs = 0;
    DECLARE_ESTACK(s);
    goto L_again;

    while (!ESTACK_ISEMPTY(s)) {
	obj = ESTACK_POP(s);
    L_again:
	if (is_list(obj)) {
	L_iter_list:
	    objp = list_val(obj);
	    /* Head */
	    obj = CAR(objp);
	    if (is_byte(obj)) {
		len++;
	    } else if (is_binary(obj)) {
		len += binary_size(obj);
		offs += binary_bitsize(obj);
	    } else if (is_list(obj)) {
		ESTACK_PUSH(s, CDR(objp));
		goto L_iter_list; /* on head */
	    } else if (is_not_nil(obj)) {
		goto L_type_error;
	    }
	    /* Tail */
	    obj = CDR(objp);
	    if (is_list(obj))
		goto L_iter_list; /* on tail */
	    else if (is_binary(obj)) {
		len += binary_size(obj);
		offs += binary_bitsize(obj);
	    } else if (is_not_nil(obj)) {
		goto L_type_error;
	    }
	} else if (is_binary(obj)) {
	    len += binary_size(obj);
	    offs += binary_bitsize(obj);
	} else if (is_not_nil(obj)) {
	    goto L_type_error;
	}
    }

    DESTROY_ESTACK(s);
    return (Sint) (len + (offs/8) + ((offs % 8) != 0));

 L_type_error:
    DESTROY_ESTACK(s);
    return (Sint) -1;
}

/*
 * The native implementation functions for the module binary.
 * Searching is implemented using aither Boyer-More or Aho-Corasick
 * depending on number of searchstrings (BM if one, AC if more than one).
 * Native implementation is mostly for efficiency, nothing (except binary:referenced_byte_size)
 * really *needs* to be implemented in native code.
 */

/* #define HARDDEBUG */

/*
 * A micro allocator used when building search structures, just a convenience
 * for building structures inside a pre alocated magic binary using conventional
 * malloc-like interface.
 */
static Export binary_match_trap_export;
static BIF_RETTYPE binary_match_trap(BIF_ALIST_3);
static Export binary_matches_trap_export;
static BIF_RETTYPE binary_matches_trap(BIF_ALIST_3);
void erts_init_bif_binary(void)
{
    sys_memset((void *) &binary_match_trap_export, 0, sizeof(Export));
    binary_match_trap_export.address = &binary_match_trap_export.code[3];
    binary_match_trap_export.code[0] = am_erlang;
    binary_match_trap_export.code[1] = am_binary_match_trap;
    binary_match_trap_export.code[2] = 3;
    binary_match_trap_export.code[3] = (BeamInstr) em_apply_bif;
    binary_match_trap_export.code[4] = (BeamInstr) &binary_match_trap;

    sys_memset((void *) &binary_matches_trap_export, 0, sizeof(Export));
    binary_matches_trap_export.address = &binary_matches_trap_export.code[3];
    binary_matches_trap_export.code[0] = am_erlang;
    binary_matches_trap_export.code[1] = am_binary_matches_trap;
    binary_matches_trap_export.code[2] = 3;
    binary_matches_trap_export.code[3] = (BeamInstr) em_apply_bif;
    binary_matches_trap_export.code[4] = (BeamInstr) &binary_matches_trap;

    return;
}

#define MYALIGN(Size) (SIZEOF_VOID_P * (((Size) / SIZEOF_VOID_P) + \
                       !!(((Size) % SIZEOF_VOID_P))))

#ifdef DEBUG
#define CHECK_ALLOCATOR(My) ASSERT((My).current <= ((My).mem + (My).size))
#else
#define CHECK_ALLOCATOR(My) /* nothing */
#endif

typedef struct _my_allocator {
    Uint size;
    byte *current;
    byte *mem;
} MyAllocator;

static void init_my_allocator(MyAllocator *my, Uint siz, byte *array)
{
    ASSERT((siz % SIZEOF_VOID_P) == 0);
    my->size = siz;
    my->mem = array;
    my->current = my->mem;
}

static void *my_alloc(MyAllocator *my, Uint size)
{
    void *ptr = my->current;
    my->current += MYALIGN(size);
    return ptr;
}

/*
 * The search functionality.
 *
 * The search is byte oriented, which works nicely for UTF-8 as well as latin1 data
 */

#define ALPHABET_SIZE 256

typedef struct _ac_node {
#ifdef HARDDEBUG
    Uint32 id;                        /* To identify h pointer targets when dumping */
#endif
    Uint32 d;                         /* Depth in trie, also represents the length
					 (-1) of the matched string if in
					 final set */
    Sint32 final;                     /* Members in final set represent matches.
				       * The set representation is scattered
				       * among the nodes in this way:
				       * >0 -> this represents a member of
				       * the final set, <0 -> member of
				       * final set somewhere in the failure chain,
				       * 0 -> not member of the final set */
    struct _ac_node *h;                /* h(Hode) is the failure function */
    struct _ac_node *g[ALPHABET_SIZE]; /* g(Node,Character) is the
					  transition function */
} ACNode;

typedef struct _ac_trie {
#ifdef HARDDEBUG
    Uint32 idc;
#endif
    Uint32 counter;        /* Number of added patterns */
    ACNode *root;     /* pointer to the root state */
} ACTrie;

typedef struct _bm_data {
    byte *x;
    Sint len;
    Sint *goodshift;
    Sint badshift[ALPHABET_SIZE];
} BMData;

#ifdef HARDDEBUG
static void dump_bm_data(BMData *bm);
static void dump_ac_trie(ACTrie *act);
static void dump_ac_node(ACNode *node, int indent, int ch);
#endif

/*
 * The needed size of binary data for a search structure - given the accumulated
 * string lengths.
 */
#define BM_SIZE(StrLen) 	      /* StrLen: length of searchstring */ \
((MYALIGN(sizeof(Sint) * (StrLen))) + /* goodshift array */                \
 MYALIGN(StrLen) +                    /* searchstring saved */             \
 (MYALIGN(sizeof(BMData))))           /* Structure */

#define AC_SIZE(StrLens) 	      /* StrLens: sum of all searchstring lengths */ \
((MYALIGN(sizeof(ACNode)) *                                                          \
((StrLens)+1)) + 	              /* The actual nodes (including rootnode) */    \
 MYALIGN(sizeof(ACTrie)))             /* Structure */


#ifndef MAX
#define MAX(A,B) (((A) > (B)) ? (A) : B)
#endif

/*
 * Callback for the magic binary
 */
static void cleanup_my_data(Binary *bp)
{
    return;
}

/*
 * Initiate a (allocated) micro allocator and fill in the base
 * for an Aho-Corasick search trie, given the accumulated length of the search strings.
 */
static ACTrie *create_acdata(MyAllocator *my, Uint len,
			     ACNode ***qbuff /* out */,Binary **the_bin /* out */)
{
    Uint datasize = AC_SIZE(len);
    ACTrie *act;
    ACNode *acn;
    Binary *mb = erts_create_magic_binary(datasize,cleanup_my_data);
    byte *data = ERTS_MAGIC_BIN_DATA(mb);

    init_my_allocator(my, datasize, data);
    act = my_alloc(my, sizeof(ACTrie)); /* Important that this is the first
					   allocation */
    act->counter = 0;
    act->root = acn = my_alloc(my, sizeof(ACNode));
    acn->d = 0;
    acn->final = 0;
    acn->h = NULL;
    memset(acn->g, 0, sizeof(ACNode *) * ALPHABET_SIZE);
#ifdef HARDDEBUG
    act->idc = 0;
    acn->id = 0;
#endif
    *qbuff = erts_alloc(ERTS_ALC_T_TMP, sizeof(ACNode *) * len);
    *the_bin = mb;
    return act;
}

/*
 * The same initialization of allocator and basic data for Boyer-More.
 */
static BMData *create_bmdata(MyAllocator *my, byte *x, Uint len, Binary **the_bin /* out */)
{
    Uint datasize = BM_SIZE(len);
    BMData *bmd;
    Binary *mb = erts_create_magic_binary(datasize,cleanup_my_data);
    byte *data = ERTS_MAGIC_BIN_DATA(mb);
    init_my_allocator(my, datasize, data);
    bmd = my_alloc(my, sizeof(BMData));
    bmd->x = my_alloc(my,len);
    memcpy(bmd->x,x,len);
    bmd->len = len;
    bmd->goodshift = my_alloc(my,sizeof(Uint) * len);
    *the_bin = mb;
    return bmd;
}

/*
 * Compilation of search structures
 */

/*
 * Aho Corasick - Build a Trie and fill in the failure functions
 * when all strings are added.
 * The algorithm is nicely described by Dieter Bühler of University of Tübingen:
 * http://www-sr.informatik.uni-tuebingen.de/~buehler/AC/AC.html
 */

/*
 * Helper called ance for each search pattern
 */
static void ac_add_one_pattern(MyAllocator *my, ACTrie *act, byte *x, Uint len)
{
    ACNode *acn = act->root;
    Uint32 n = ++act->counter; /* Always increase conter, even if it's a duplicate
				  as this shall identify the pattern in the
			          final set and eventually be returned to
			          the caller (in Erlang) */
    Uint i = 0;

    while(i < len) {
	if (acn->g[x[i]] != NULL) {
	    /* node exists, continue */
	    acn = acn->g[x[i]];
	    ++i;
	} else {
	    /* allocate a new node */
	    ACNode *nn = my_alloc(my,sizeof(ACNode));
#ifdef HARDDEBUG
	    nn->id = ++(act->idc);
#endif
	    nn->d = i+1;
	    nn->h = act->root;
	    nn->final = 0;
	    memset(nn->g, 0, sizeof(ACNode *) * ALPHABET_SIZE);
	    acn->g[x[i]] = nn;
	    ++i;
	    acn = nn;
	}
    }
    if (acn->final == 0) { /* New pattern, add to final set */
	acn->final = n;
    }
}

/*
 * Called when all search patterns are added.
 */
static void ac_compute_failure_functions(ACTrie *act, ACNode **qbuff)
{
    ACNode *root = act->root;
    ACNode *parent;
    int i;
    int qh = 0,qt = 0;
    ACNode *child, *r;

    /* Set all children of the root to have the root as failure function */
    for (i = 0; i < ALPHABET_SIZE; ++i) {
	if (root->g[i] != NULL) {
	    root->g[i]->h = root;
	    /* Add to que for later traversal */
	    qbuff[qt++] = root->g[i];
	}
    }

    /* So, now we've handled children of the root state, traverse the
       rest of the trie BF... */
    while (qh < qt) {
	parent = qbuff[qh++];
	for (i = 0; i < ALPHABET_SIZE; ++ i) {
	    if ((child = parent->g[i]) != NULL) {
		/* Visit this node to */
		qbuff[qt++] = child;
		/* Search for correct failure function, follow the parents failure
		   function until you find a similar transition funtion to this
		   childs */
		r =  parent->h;
		while (r != NULL && r->g[i] == NULL) {
		    r = r->h;
		}
		if (r == NULL) {
		    /* Replace NULL failures with the root as we go */
		    child->h = (root->g[i] == NULL) ? root : root->g[i];
		} else {
		    child->h = r->g[i];
		    /*
		     * The "final" set is scattered among the nodes. When
		     * the failure function points to a member of the final set,
		     * we have a match, but we might not see it in the current node
		     * if we dont mark it as a special type of final, i.e. foolow
		     * the failure function and you will find a real member of final
		     * set. This is marked with a negative string id and only done if
		     * this node does not represent a member in the final set.
		     */
		    if (!(child->final) && (child->h->final)) {
			child->final = -1;
		    }
		}
	    }
	}
    }
    /* Finally the failure function of the root should point to itself */
    root->h = root;
}

/*
 * The actual searching for needles in the haystack...
 * Find first match using Aho-Coracick Trie
 * return pattern number and fill in mpos + mlen if found, otherwise return 0
 * Return the matching pattern that *starts* first, and ends
 * last (difference when overlapping), hence the candidate thing.
 * Basic AC finds the first end before the first start...
 *
 */
typedef struct {
    ACNode *q;
    Uint pos;
    Uint len;
    ACNode *candidate;
    Uint candidate_start;
} ACFindFirstState;


static void ac_init_find_first_match(ACFindFirstState *state, ACTrie *act, Sint startpos, Uint len)
{
    state->q = act->root;
    state->pos = startpos;
    state->len = len;
    state->candidate = NULL;
    state->candidate_start = 0;
}
#define AC_OK 0
#define AC_NOT_FOUND -1
#define AC_RESTART -2

#define AC_LOOP_FACTOR 1

static int ac_find_first_match(ACFindFirstState *state, byte *haystack,
				Uint *mpos, Uint *mlen, Uint reductions)
{
    ACNode *q = state->q;
    Uint i = state->pos;
    ACNode *candidate = state->candidate, *r;
    Uint len = state->len;
    Uint candidate_start = state->candidate_start;
    Uint rstart;
    register Uint reds = (Uint) reductions;

    while (i < len) {
	if (--reds == 0) {
	    state->q = q;
	    state->pos = i;
	    state->len = len;
	    state->candidate = candidate;
	    state->candidate_start = candidate_start;
	    return AC_RESTART;
	}

	while (q->g[haystack[i]] == NULL && q->h != q) {
	    q = q->h;
	}
	if (q->g[haystack[i]] != NULL) {
	    q = q->g[haystack[i]];
	}
#ifdef HARDDEBUG
	erts_printf("ch = %c, Current: %u\n", (int) haystack[i], (unsigned) q->id);
#endif
	++i;
	if (candidate != NULL && (i - q->d) > candidate_start) {
	    break;
	}
	if (q->final) {
	    r = q;
	    while (r->final < 0)
		r = r->h;
	    rstart = i - r->d;
	    if (candidate == NULL || rstart < candidate_start ||
		(rstart == candidate_start && candidate->d < q->d)) {
		candidate_start = rstart;
		candidate = r;
	    }
	}
    }
    if (!candidate) {
	return AC_NOT_FOUND;
    }
#ifdef HARDDEBUG
    dump_ac_node(candidate,0,'?');
#endif
    *mpos = candidate_start;
    *mlen = candidate->d;
    return AC_OK;
}

typedef struct _findall_data {
    Uint pos;
    Uint len;
#ifdef HARDDEBUG
    Uint id;
#endif
    Eterm epos;
    Eterm elen;
} FindallData;

typedef struct {
    ACNode *q;
    Uint pos;
    Uint len;
    Uint m;
    Uint allocated;
    FindallData *out;
} ACFindAllState;

static void ac_init_find_all(ACFindAllState *state, ACTrie *act, Sint startpos, Uint len)
{
    state->q = act->root;
    state->pos = startpos;
    state->len = len;
    state->m = 0;
    state->allocated = 0;
    state->out = NULL;
}

static void ac_restore_find_all(ACFindAllState *state, char *buff)
{
    memcpy(state,buff,sizeof(ACFindAllState));
    state->out = erts_alloc(ERTS_ALC_T_TMP, sizeof(FindallData) * (state->allocated));
    memcpy(state->out,buff+sizeof(ACFindAllState),sizeof(FindallData)*state->m);
}

static void ac_serialize_find_all(ACFindAllState *state, char *buff)
{
    memcpy(buff,state,sizeof(ACFindAllState));
    memcpy(buff+sizeof(ACFindAllState),state->out,sizeof(FindallData)*state->m);
}

static void ac_clean_find_all(ACFindAllState *state)
{
    if (state->out != NULL) {
	erts_free(ERTS_ALC_T_TMP, state->out);
    }
#ifdef HARDDEBUG
    state->out = NULL;
    state->allocated = 0;
#endif
}

#define SIZEOF_AC_SERIALIZED_FIND_ALL_STATE(S) (sizeof(ACFindAllState)+(sizeof(FindallData)*(S).m))

/*
 * Differs to the find_first function in that it stores all matches and the values
 * arte returned only in the state.
 */
static int ac_find_all_non_overlapping(ACFindAllState *state, byte *haystack, Uint reductions)
{
    ACNode *q = state->q;
    Uint i = state->pos;
    Uint rstart;
    ACNode *r;
    Uint len = state->len;
    Uint m = state->m, save_m;
    Uint allocated = state->allocated;
    FindallData *out = state->out;
    register Uint reds = (Uint) reductions;


    while (i < len) {
	if (--reds == 0) {
	    state->q = q;
	    state->pos = i;
	    state->len = len;
	    state->m = m;
	    state->allocated = allocated;
	    state->out = out;
	    return AC_RESTART;
	}
	while (q->g[haystack[i]] == NULL && q->h != q) {
	    q = q->h;
	}
	if (q->g[haystack[i]] != NULL) {
	    q = q->g[haystack[i]];
	}
	++i;
	if (q->final) {
	    r = q;
	    while (r->final) {
		while (r->final < 0)
		    r = r->h;
#ifdef HARDDEBUG
		erts_printf("Trying to add %u\n",(unsigned) r->final);
#endif
		rstart = i - r->d;
		save_m = m;
		while (m > 0 && (out[m-1].pos > rstart ||
				 (out[m-1].pos == rstart &&
				  out[m-1].len < r->d))) {
#ifdef HARDDEBUG
		    erts_printf("Popping %u\n",(unsigned) out[m-1].id);
#endif
		    --m;
		}
#ifdef HARDDEBUG
		if (m > 0) {
		    erts_printf("Pos %u\n",out[m-1].pos);
		    erts_printf("Len %u\n",out[m-1].len);
		}
		erts_printf("Rstart %u\n",rstart);
#endif
		if (m == 0 ||  out[m-1].pos + out[m-1].len <= rstart) {
		    if (m >= allocated) {
			if (!allocated) {
			    allocated = 10;
			    out = erts_alloc(ERTS_ALC_T_TMP, sizeof(FindallData) * allocated);
			} else {
			    allocated *= 2;
			    out = erts_realloc(ERTS_ALC_T_TMP, out,
					       sizeof(FindallData) * allocated);
			}
		    }
		    out[m].pos = rstart;
		    out[m].len = r->d;
#ifdef HARDDEBUG
		    out[m].id = r->final;
#endif
		    ++m;
#ifdef HARDDEBUG
		    erts_printf("Pushing %u\n",(unsigned) out[m-1].id);
#endif
		} else {
#ifdef HARDDEBUG
		    erts_printf("Backtracking %d steps\n",save_m - m);
#endif
		    m = save_m;
		}
		r = r->h;
	    }
	}
    }
    state->m = m;
    state->out = out;
    return (m == 0) ? AC_NOT_FOUND : AC_OK;
}

/*
 * Boyer More - most obviously implemented more or less exactly as Christian Charras
 * and Thierry Lecroq describes it in "Handbook of Exact String-Matching Algorithms"
 * http://www-igm.univ-mlv.fr/~lecroq/string/
 */

/*
 * Call this to compute badshifts array
 */
static void compute_badshifts(BMData *bmd)
{
    Sint i;
    Sint m = bmd->len;

    for (i = 0; i < ALPHABET_SIZE; ++i) {
	bmd->badshift[i] = m;
    }
    for (i = 0; i < m - 1; ++i) {
	bmd->badshift[bmd->x[i]] = m - i - 1;
    }
}

/* Helper for "compute_goodshifts" */
static void compute_suffixes(byte *x, Sint m, Sint *suffixes)
{
    int f,g,i;

    suffixes[m - 1] = m;

    f = 0; /* To avoid use before set warning */

    g = m - 1;

    for (i = m - 2; i >= 0; --i) {
	if (i > g && suffixes[i + m - f] < i - g) {
	    suffixes[i] = suffixes[i + m - 1 - f];
	} else {
	    if (i < g) {
		g = i;
	    }
	    f = i;
	    while ( g >= 0 && x[g] == x[g + m - 1 - f] ) {
		--g;
	    }
	    suffixes[i] = f - g;
	}
    }
}

/*
 * Call this to compute goodshift array
 */
static void compute_goodshifts(BMData *bmd)
{
    Sint m = bmd->len;
    byte *x = bmd->x;
    Sint i, j;
    Sint *suffixes = erts_alloc(ERTS_ALC_T_TMP, m * sizeof(Uint));

    compute_suffixes(x, m, suffixes);

    for (i = 0; i < m; ++i) {
	bmd->goodshift[i] = m;
    }

    j = 0;

    for (i = m - 1; i >= -1; --i) {
	if (i == -1 || suffixes[i] == i + 1) {
	    while (j < m - 1 - i) {
		if (bmd->goodshift[j] == m) {
		    bmd->goodshift[j] = m - 1 - i;
		}
		++j;
	    }
	}
    }
    for (i = 0; i <= m - 2; ++i) {
	bmd->goodshift[m - 1 - suffixes[i]] = m - 1 - i;
    }
    erts_free(ERTS_ALC_T_TMP, suffixes);
}

typedef struct {
    Sint pos;
    Sint len;
} BMFindFirstState;

#define BM_OK 0 /* used only for find_all */
#define BM_NOT_FOUND -1
#define BM_RESTART -2
#define BM_LOOP_FACTOR 1

static void bm_init_find_first_match(BMFindFirstState *state, Sint startpos, Uint len)
{
    state->pos = startpos;
    state->len = (Sint) len;
}


static Sint bm_find_first_match(BMFindFirstState *state, BMData *bmd, byte *haystack, Uint reductions)
{
    Sint blen = bmd->len;
    Sint len = state->len;
    Sint *gs = bmd->goodshift;
    Sint *bs = bmd->badshift;
    byte *needle = bmd->x;
    Sint i;
    Sint j = state->pos;
    register Uint reds = reductions;

    while (j <= len - blen) {
	if (--reds == 0) {
	    state->pos = j;
	    return BM_RESTART;
	}
	for (i = blen - 1; i >= 0 && needle[i] == haystack[i + j]; --i)
	    ;
	if (i < 0) { /* found */
	    return j;
	}
	j += MAX(gs[i],bs[haystack[i+j]] - blen + 1 + i);
    }
    return BM_NOT_FOUND;
}

typedef struct {
    Sint pos;
    Sint len;
    Uint m;
    Uint allocated;
    FindallData *out;
} BMFindAllState;

static void bm_init_find_all(BMFindAllState *state, Sint startpos, Uint len)
{
    state->pos = startpos;
    state->len = (Sint) len;
    state->m = 0;
    state->allocated = 0;
    state->out = NULL;
}

static void bm_restore_find_all(BMFindAllState *state, char *buff)
{
    memcpy(state,buff,sizeof(BMFindAllState));
    state->out = erts_alloc(ERTS_ALC_T_TMP, sizeof(FindallData) * (state->allocated));
    memcpy(state->out,buff+sizeof(BMFindAllState),sizeof(FindallData)*state->m);
}

static void bm_serialize_find_all(BMFindAllState *state, char *buff)
{
    memcpy(buff,state,sizeof(BMFindAllState));
    memcpy(buff+sizeof(BMFindAllState),state->out,sizeof(FindallData)*state->m);
}

static void bm_clean_find_all(BMFindAllState *state)
{
    if (state->out != NULL) {
	erts_free(ERTS_ALC_T_TMP, state->out);
    }
#ifdef HARDDEBUG
    state->out = NULL;
    state->allocated = 0;
#endif
}

#define SIZEOF_BM_SERIALIZED_FIND_ALL_STATE(S) (sizeof(BMFindAllState)+(sizeof(FindallData)*(S).m))

/*
 * Differs to the find_first function in that it stores all matches and the values
 * arte returned only in the state.
 */
static Sint bm_find_all_non_overlapping(BMFindAllState *state,
					BMData *bmd, byte *haystack, Uint reductions)
{
    Sint blen = bmd->len;
    Sint len = state->len;
    Sint *gs = bmd->goodshift;
    Sint *bs = bmd->badshift;
    byte *needle = bmd->x;
    Sint i;
    Sint j = state->pos;
    Uint m = state->m;
    Uint allocated = state->allocated;
    FindallData *out = state->out;
    register Uint reds = reductions;

    while (j <= len - blen) {
	if (--reds == 0) {
	    state->pos = j;
	    state->m = m;
	    state->allocated = allocated;
	    state->out = out;
	    return BM_RESTART;
	}
	for (i = blen - 1; i >= 0 && needle[i] == haystack[i + j]; --i)
	    ;
	if (i < 0) { /* found */
	    if (m >= allocated) {
		if (!allocated) {
		    allocated = 10;
		    out = erts_alloc(ERTS_ALC_T_TMP, sizeof(FindallData) * allocated);
		} else {
		    allocated *= 2;
		    out = erts_realloc(ERTS_ALC_T_TMP, out,
				       sizeof(FindallData) * allocated);
		}
	    }
	    out[m].pos = j;
	    out[m].len = blen;
	    ++m;
	    j += blen;
	} else {
	    j += MAX(gs[i],bs[haystack[i+j]] - blen + 1 + i);
	}
    }
    state->m = m;
    state->out = out;
    return (m == 0) ? BM_NOT_FOUND : BM_OK;
}

/*
 * Interface functions (i.e. "bif's")
 */

/*
 * Search functionality interfaces
 */

static int do_binary_match_compile(Eterm argument, Eterm *tag, Binary **binp)
{
    Eterm t, b, comp_term = NIL;
    Uint characters;
    Uint words;

    characters = 0;
    words = 0;

    if (is_list(argument)) {
	t = argument;
	while (is_list(t)) {
	    b = CAR(list_val(t));
	    t = CDR(list_val(t));
	    if (!is_binary(b)) {
		goto badarg;
	    }
	    if (binary_bitsize(b) != 0) {
		goto badarg;
	    }
	    ++words;
	    characters += binary_size(b);
	}
	if (is_not_nil(t)) {
	    goto badarg;
	}
	if (words > 1) {
	    comp_term = argument;
	} else {
	    comp_term = CAR(list_val(argument));
	}
    } else if (is_binary(argument)) {
	if (binary_bitsize(argument) != 0) {
	    goto badarg;
	}
	words = 1;
	comp_term = argument;
	characters = binary_size(argument);
    }

    if (characters == 0) {
	goto badarg;
    }
    ASSERT(words > 0);

    if (words == 1) {
	byte *bytes;
	Uint bitoffs, bitsize;
	byte *temp_alloc = NULL;
	MyAllocator my;
	BMData *bmd;
	Binary *bin;

	ERTS_GET_BINARY_BYTES(comp_term, bytes, bitoffs, bitsize);
	if (bitoffs != 0) {
	    bytes = erts_get_aligned_binary_bytes(comp_term, &temp_alloc);
	}
	bmd = create_bmdata(&my, bytes, characters, &bin);
	compute_badshifts(bmd);
	compute_goodshifts(bmd);
	erts_free_aligned_binary_bytes(temp_alloc);
	CHECK_ALLOCATOR(my);
	*tag = am_bm;
	*binp = bin;
	return 0;
    } else {
	ACTrie *act;
	MyAllocator my;
	ACNode **qbuff;
	Binary *bin;

	act = create_acdata(&my, characters, &qbuff, &bin);
	t = comp_term;
	while (is_list(t)) {
	    byte *bytes;
	    Uint bitoffs, bitsize;
	    byte *temp_alloc = NULL;
	    b = CAR(list_val(t));
	    t = CDR(list_val(t));
	    ERTS_GET_BINARY_BYTES(b, bytes, bitoffs, bitsize);
	    if (bitoffs != 0) {
		bytes = erts_get_aligned_binary_bytes(b, &temp_alloc);
	    }
	    ac_add_one_pattern(&my,act,bytes,binary_size(b));
	    erts_free_aligned_binary_bytes(temp_alloc);
	}
	ac_compute_failure_functions(act,qbuff);
	CHECK_ALLOCATOR(my);
	erts_free(ERTS_ALC_T_TMP,qbuff);
	*tag = am_ac;
	*binp = bin;
	return 0;
    }
 badarg:
    return -1;
}

BIF_RETTYPE binary_compile_pattern_1(BIF_ALIST_1)
{
    Binary *bin;
    Eterm tag, ret;
    Eterm *hp;

    if (do_binary_match_compile(BIF_ARG_1,&tag,&bin)) {
	BIF_ERROR(BIF_P,BADARG);
    }
    hp = HAlloc(BIF_P, PROC_BIN_SIZE+3);
    ret = erts_mk_magic_binary_term(&hp, &MSO(BIF_P), bin);
    ret = TUPLE2(hp, tag, ret);
    BIF_RET(ret);
}

#define DO_BIN_MATCH_OK 0
#define DO_BIN_MATCH_BADARG -1
#define DO_BIN_MATCH_RESTART -2

static int do_binary_match(Process *p, Eterm subject, Uint hsstart, Uint hslen,
			   Eterm type, Binary *bin, Eterm state_term, Eterm *res_term)
{
    byte *bytes;
    Uint bitoffs, bitsize;
    byte *temp_alloc = NULL;

    ERTS_GET_BINARY_BYTES(subject, bytes, bitoffs, bitsize);
    if (bitsize != 0) {
	goto badarg;
    }
    if (bitoffs != 0) {
	bytes = erts_get_aligned_binary_bytes(subject, &temp_alloc);
    }
    if (state_term != NIL) {
	Eterm *ptr = big_val(state_term);
	type = ptr[1];
    }

    if (type == am_bm) {
	BMData *bm;
	Sint pos;
	Eterm ret;
	Eterm *hp;
	BMFindFirstState state;
	Uint reds = ERTS_BIF_REDS_LEFT(p) * BM_LOOP_FACTOR;

	bm = (BMData *) ERTS_MAGIC_BIN_DATA(bin);
#ifdef HARDDEBUG
	dump_bm_data(bm);
#endif
	if (state_term == NIL) {
	    bm_init_find_first_match(&state, hsstart, hslen);
	} else {
	    Eterm *ptr = big_val(state_term);
	    memcpy(&state,ptr+2,sizeof(state));
	}
#ifdef HARDDEBUG
	erts_printf("(bm) state->pos = %ld, state->len = %lu\n",state.pos, state.len);
#endif
	pos = bm_find_first_match(&state, bm, bytes, reds);
	if (pos == BM_NOT_FOUND) {
	    ret = am_nomatch;
	} else if (pos == BM_RESTART) {
	    int x = (sizeof(BMFindFirstState) / sizeof(Eterm)) +
		!!(sizeof(BMFindFirstState) % sizeof(Eterm));
#ifdef HARDDEBUG
	    erts_printf("Trap bm!\n");
#endif
	    hp = HAlloc(p,x+2);
	    hp[0] = make_pos_bignum_header(x+1);
	    hp[1] = type;
	    memcpy(hp+2,&state,sizeof(state));
	    *res_term = make_big(hp);
	    erts_free_aligned_binary_bytes(temp_alloc);
	    return DO_BIN_MATCH_RESTART;
	} else {
	    Eterm erlen = erts_make_integer((Uint) bm->len, p);
	    ret = erts_make_integer(pos,p);
	    hp = HAlloc(p,3);
	    ret = TUPLE2(hp, ret, erlen);
	}
	erts_free_aligned_binary_bytes(temp_alloc);
	*res_term = ret;
	return DO_BIN_MATCH_OK;
    } else if (type == am_ac) {
	ACTrie *act;
	Uint pos, rlen;
	int acr;
	ACFindFirstState state;
	Eterm ret;
	Eterm *hp;
	Uint reds = ERTS_BIF_REDS_LEFT(p) * AC_LOOP_FACTOR;

	act = (ACTrie *) ERTS_MAGIC_BIN_DATA(bin);
#ifdef HARDDEBUG
	dump_ac_trie(act);
#endif
	if (state_term == NIL) {
	    ac_init_find_first_match(&state, act, hsstart, hslen);
	} else {
	    Eterm *ptr = big_val(state_term);
	    memcpy(&state,ptr+2,sizeof(state));
	}
	acr = ac_find_first_match(&state, bytes, &pos, &rlen, reds);
	if (acr == AC_NOT_FOUND) {
	    ret = am_nomatch;
	} else if (acr == AC_RESTART) {
	    int x = (sizeof(state) / sizeof(Eterm)) +
		!!(sizeof(BMFindFirstState) % sizeof(Eterm));
#ifdef HARDDEBUG
	    erts_printf("Trap ac!\n");
#endif
	    hp = HAlloc(p,x+2);
	    hp[0] = make_pos_bignum_header(x+1);
	    hp[1] = type;
	    memcpy(hp+2,&state,sizeof(state));
	    *res_term = make_big(hp);
	    erts_free_aligned_binary_bytes(temp_alloc);
	    return DO_BIN_MATCH_RESTART;
	} else {
	    Eterm epos = erts_make_integer(pos+hsstart,p);
	    Eterm erlen = erts_make_integer(rlen,p);
	    hp = HAlloc(p,3);
	    ret = TUPLE2(hp, epos, erlen);
	}
	erts_free_aligned_binary_bytes(temp_alloc);
	*res_term = ret;
	return DO_BIN_MATCH_OK;
    }
 badarg:
    return DO_BIN_MATCH_BADARG;
}

static int do_binary_matches(Process *p, Eterm subject, Uint hsstart, Uint hslen,
			     Eterm type, Binary *bin, Eterm state_term, Eterm *res_term)
{
    byte *bytes;
    Uint bitoffs, bitsize;
    byte *temp_alloc = NULL;

    ERTS_GET_BINARY_BYTES(subject, bytes, bitoffs, bitsize);
    if (bitsize != 0) {
	goto badarg;
    }
    if (bitoffs != 0) {
	bytes = erts_get_aligned_binary_bytes(subject, &temp_alloc);
    }
    if (state_term != NIL) {
	Eterm *ptr = big_val(state_term);
	type = ptr[1];
    }

    if (type == am_bm) {
	BMData *bm;
	Sint pos;
	Eterm ret,tpl;
	Eterm *hp;
	BMFindAllState state;
	Uint reds = ERTS_BIF_REDS_LEFT(p) * BM_LOOP_FACTOR;

	bm = (BMData *) ERTS_MAGIC_BIN_DATA(bin);
#ifdef HARDDEBUG
	dump_bm_data(bm);
#endif
	if (state_term == NIL) {
	    bm_init_find_all(&state, hsstart, hslen);
	} else {
	    Eterm *ptr = big_val(state_term);
	    bm_restore_find_all(&state,(char *) (ptr+2));
	}

	pos = bm_find_all_non_overlapping(&state, bm, bytes, reds);
	if (pos == BM_NOT_FOUND) {
	    ret = am_nomatch;
	} else if (pos == BM_RESTART) {
	    int x = (SIZEOF_BM_SERIALIZED_FIND_ALL_STATE(state) / sizeof(Eterm)) +
		!!(SIZEOF_BM_SERIALIZED_FIND_ALL_STATE(state) % sizeof(Eterm));
#ifdef HARDDEBUG
	    erts_printf("Trap bm!\n");
#endif
	    hp = HAlloc(p,x+2);
	    hp[0] = make_pos_bignum_header(x+1);
	    hp[1] = type;
	    bm_serialize_find_all(&state, (char *) (hp+2));
	    *res_term = make_big(hp);
	    erts_free_aligned_binary_bytes(temp_alloc);
	    bm_clean_find_all(&state);
	    return DO_BIN_MATCH_RESTART;
	} else {
	    FindallData *fad = state.out;
	    int i;
	    for (i = 0; i < state.m; ++i) {
		fad[i].epos = erts_make_integer(fad[i].pos,p);
		fad[i].elen = erts_make_integer(fad[i].len,p);
	    }
	    hp = HAlloc(p,state.m * (3 + 2));
	    ret = NIL;
	    for (i = state.m - 1; i >= 0; --i) {
		tpl = TUPLE2(hp, fad[i].epos, fad[i].elen);
		hp +=3;
		ret = CONS(hp,tpl,ret);
		hp += 2;
	    }
	}
	erts_free_aligned_binary_bytes(temp_alloc);
	bm_clean_find_all(&state);
	*res_term = ret;
	return DO_BIN_MATCH_OK;
    } else if (type == am_ac) {
	ACTrie *act;
	int acr;
	ACFindAllState state;
	Eterm ret,tpl;
	Eterm *hp;
	Uint reds = ERTS_BIF_REDS_LEFT(p) * AC_LOOP_FACTOR;

	act = (ACTrie *) ERTS_MAGIC_BIN_DATA(bin);
#ifdef HARDDEBUG
	dump_ac_trie(act);
#endif
	if (state_term == NIL) {
	    ac_init_find_all(&state, act, hsstart, hslen);
	} else {
	    Eterm *ptr = big_val(state_term);
	    ac_restore_find_all(&state,(char *) (ptr+2));
	}
	acr = ac_find_all_non_overlapping(&state, bytes, reds);
	if (acr == AC_NOT_FOUND) {
	    ret = am_nomatch;
	} else if (acr == AC_RESTART) {
	    int x = (SIZEOF_AC_SERIALIZED_FIND_ALL_STATE(state) / sizeof(Eterm)) +
		!!(SIZEOF_AC_SERIALIZED_FIND_ALL_STATE(state) % sizeof(Eterm));
#ifdef HARDDEBUG
	    erts_printf("Trap ac!\n");
#endif
	    hp = HAlloc(p,x+2);
	    hp[0] = make_pos_bignum_header(x+1);
	    hp[1] = type;
	    ac_serialize_find_all(&state, (char *) (hp+2));
	    *res_term = make_big(hp);
	    erts_free_aligned_binary_bytes(temp_alloc);
	    ac_clean_find_all(&state);
	    return DO_BIN_MATCH_RESTART;
	} else {
	    FindallData *fad = state.out;
	    int i;
	    for (i = 0; i < state.m; ++i) {
		fad[i].epos = erts_make_integer(fad[i].pos,p);
		fad[i].elen = erts_make_integer(fad[i].len,p);
	    }
	    hp = HAlloc(p,state.m * (3 + 2));
	    ret = NIL;
	    for (i = state.m - 1; i >= 0; --i) {
		tpl = TUPLE2(hp, fad[i].epos, fad[i].elen);
		hp +=3;
		ret = CONS(hp,tpl,ret);
		hp += 2;
	    }
	}
	erts_free_aligned_binary_bytes(temp_alloc);
	ac_clean_find_all(&state);
	*res_term = ret;
	return DO_BIN_MATCH_OK;
    }
 badarg:
    return DO_BIN_MATCH_BADARG;
}

static BIF_RETTYPE binary_match_trap(BIF_ALIST_3)
{
    int runres;
    Eterm result;
    Binary *bin = ((ProcBin *) binary_val(BIF_ARG_3))->val;
    runres = do_binary_match(BIF_P,BIF_ARG_1,0,0,NIL,bin,BIF_ARG_2,&result);
    switch (runres) {
    case DO_BIN_MATCH_OK:
	BIF_RET(result);
    case DO_BIN_MATCH_RESTART:
	BUMP_ALL_REDS(BIF_P);
	BIF_TRAP3(&binary_match_trap_export, BIF_P, BIF_ARG_1, result, BIF_ARG_3);
    default:
	goto badarg;
    }
 badarg:
    BIF_ERROR(BIF_P,BADARG);
}

static BIF_RETTYPE binary_matches_trap(BIF_ALIST_3)
{
    int runres;
    Eterm result;
    Binary *bin = ((ProcBin *) binary_val(BIF_ARG_3))->val;
    runres = do_binary_matches(BIF_P,BIF_ARG_1,0,0,NIL,bin,BIF_ARG_2,&result);
    switch (runres) {
    case DO_BIN_MATCH_OK:
	BIF_RET(result);
    case DO_BIN_MATCH_RESTART:
	BUMP_ALL_REDS(BIF_P);
	BIF_TRAP3(&binary_matches_trap_export, BIF_P, BIF_ARG_1, result, BIF_ARG_3);
    default:
	goto badarg;
    }
 badarg:
    BIF_ERROR(BIF_P,BADARG);
}


BIF_RETTYPE binary_match_3(BIF_ALIST_3)
{
    Uint hsstart, hslen;
    Eterm *tp;
    Eterm type;
    Binary *bin;
    Eterm bin_term = NIL;
    int runres;
    Eterm result;

    if (is_not_binary(BIF_ARG_1)) {
	goto badarg;
    }
    if (BIF_ARG_3 == ((Eterm) 0)) {
	/* Invalid term, we're called from binary_match_2... */
	hsstart = 0;
	hslen = binary_size(BIF_ARG_1);
    } else if (is_list(BIF_ARG_3)) {
	Eterm l = BIF_ARG_3;
	while(is_list(l)) {
	    Eterm t = CAR(list_val(l));
	    if (!is_tuple(t)) {
		goto badarg;
	    }
	    tp = tuple_val(t);
	    if (arityval(*tp) != 2) {
		goto badarg;
	    }
	    if (!term_to_Uint(tp[1], &hsstart) || ((hsstart >> 16) >> 16) != 0) {
		goto badarg;
	    }
	    if (!term_to_Uint(tp[2], &hslen) || ((hslen >> 16) >> 16) != 0) {
		goto badarg;
	    }
	    if (hslen < hsstart) {
		goto badarg;
	    }
	    if (hslen > binary_size(BIF_ARG_1)-1) {
		goto badarg; /* XXX:PaN or should we take as much as we have ? */
	    }
	    hslen = hslen + 1 - hsstart;
	    l = CDR(list_val(l));
	}
    } else if (BIF_ARG_3 != NIL) {
	goto badarg;
    }
    if (hslen == 0) {
	BIF_RET(am_nomatch);
    }
    if (is_tuple(BIF_ARG_2)) {
	tp = tuple_val(BIF_ARG_2);
	if (arityval(*tp) != 2 || is_not_atom(tp[1])) {
	    goto badarg;
	}
	if (((tp[1] != am_bm) && (tp[1] != am_ac)) ||
	    !ERTS_TERM_IS_MAGIC_BINARY(tp[2])) {
	    goto badarg;
	}
	type = tp[1];
	bin = ((ProcBin *) binary_val(tp[2]))->val;
	if (ERTS_MAGIC_BIN_DESTRUCTOR(bin) != cleanup_my_data) {
	    goto badarg;
	}
	bin_term = tp[2];
    } else if (do_binary_match_compile(BIF_ARG_2,&type,&bin)) {
	goto badarg;
    }
    runres = do_binary_match(BIF_P,BIF_ARG_1,hsstart,hslen,type,bin,NIL,&result);
    if (runres == DO_BIN_MATCH_RESTART && bin_term == NIL) {
	Eterm *hp = HAlloc(BIF_P, PROC_BIN_SIZE);
	bin_term = erts_mk_magic_binary_term(&hp, &MSO(BIF_P), bin);
    } else if (bin_term == NIL) {
	erts_bin_free(bin);
    }
    switch (runres) {
    case DO_BIN_MATCH_OK:
	BIF_RET(result);
    case DO_BIN_MATCH_RESTART:
	BUMP_ALL_REDS(BIF_P);
	BIF_TRAP3(&binary_match_trap_export, BIF_P, BIF_ARG_1, result, bin_term);
    default:
	goto badarg;
    }
 badarg:
    BIF_ERROR(BIF_P,BADARG);
}

BIF_RETTYPE binary_matches_3(BIF_ALIST_3)
{
    Uint hsstart, hslen;
    Eterm *tp;
    Eterm type;
    Binary *bin;
    Eterm bin_term = NIL;
    int runres;
    Eterm result;

    if (is_not_binary(BIF_ARG_1)) {
	goto badarg;
    }
    if (BIF_ARG_3 == ((Eterm) 0)) {
	/* Invalid term, we're called from binary_matches_2... */
	hsstart = 0;
	hslen = binary_size(BIF_ARG_1);
    } else if (is_list(BIF_ARG_3)) {
	Eterm l = BIF_ARG_3;
	while(is_list(l)) {
	    Eterm t = CAR(list_val(l));
	    if (!is_tuple(t)) {
		goto badarg;
	    }
	    tp = tuple_val(t);
	    if (arityval(*tp) != 2) {
		goto badarg;
	    }
	    if (!term_to_Uint(tp[1], &hsstart) || ((hsstart >> 16) >> 16) != 0) {
		goto badarg;
	    }
	    if (!term_to_Uint(tp[2], &hslen) || ((hslen >> 16) >> 16) != 0) {
		goto badarg;
	    }
	    if (hslen < hsstart) {
		goto badarg;
	    }
	    if (hslen > binary_size(BIF_ARG_1)-1) {
		goto badarg; /* XXX:PaN or should we take as much as we have ? */
	    }
	    hslen = hslen + 1 - hsstart;
	    l = CDR(list_val(l));
	}
    } else if (BIF_ARG_3 != NIL) {
	goto badarg;
    }
    if (hslen == 0) {
	BIF_RET(am_nomatch);
    }
    if (is_tuple(BIF_ARG_2)) {
	tp = tuple_val(BIF_ARG_2);
	if (arityval(*tp) != 2 || is_not_atom(tp[1])) {
	    goto badarg;
	}
	if (((tp[1] != am_bm) && (tp[1] != am_ac)) ||
	    !ERTS_TERM_IS_MAGIC_BINARY(tp[2])) {
	    goto badarg;
	}
	type = tp[1];
	bin = ((ProcBin *) binary_val(tp[2]))->val;
	if (ERTS_MAGIC_BIN_DESTRUCTOR(bin) != cleanup_my_data) {
	    goto badarg;
	}
	bin_term = tp[2];
    } else if (do_binary_match_compile(BIF_ARG_2,&type,&bin)) {
	goto badarg;
    }
    runres = do_binary_matches(BIF_P,BIF_ARG_1,hsstart,hslen,type,bin,NIL,&result);
    if (runres == DO_BIN_MATCH_RESTART && bin_term == NIL) {
	Eterm *hp = HAlloc(BIF_P, PROC_BIN_SIZE);
	bin_term = erts_mk_magic_binary_term(&hp, &MSO(BIF_P), bin);
    } else if (bin_term == NIL) {
	erts_bin_free(bin);
    }
    switch (runres) {
    case DO_BIN_MATCH_OK:
	BIF_RET(result);
    case DO_BIN_MATCH_RESTART:
	BUMP_ALL_REDS(BIF_P);
	BIF_TRAP3(&binary_matches_trap_export, BIF_P, BIF_ARG_1, result, bin_term);
    default:
	goto badarg;
    }
 badarg:
    BIF_ERROR(BIF_P,BADARG);
}


BIF_RETTYPE binary_match_2(BIF_ALIST_2)
{
    return binary_match_3(BIF_P,BIF_ARG_1,BIF_ARG_2,((Eterm) 0));
}


BIF_RETTYPE binary_matches_2(BIF_ALIST_2)
{
    return binary_matches_3(BIF_P,BIF_ARG_1,BIF_ARG_2,((Eterm) 0));
}

/*
 * Hard debug functions (dump) for the search structures
 */

#ifdef HARDDEBUG
static void dump_bm_data(BMData *bm)
{
    int i,j;
    erts_printf("Dumping Boyer-More structure.\n");
    erts_printf("=============================\n");
    erts_printf("Searchstring [%ld]:\n", bm->len);
    erts_printf("<<");
    for (i = 0; i < bm->len; ++i) {
	if (i > 0) {
	    erts_printf(", ");
	}
	erts_printf("%d", (int) bm->x[i]);
	if (bm->x[i] >= 'A') {
	    erts_printf(" ($%c)",(char) bm->x[i]);
	}
    }
    erts_printf(">>\n");
    erts_printf("GoodShift array:\n");
    for (i = 0; i < bm->len; ++i) {
	erts_printf("GoodShift[%d]: %ld\n", i, bm->goodshift[i]);
    }
    erts_printf("BadShift array:\n");
    j = 0;
    for (i = 0; i < ALPHABET_SIZE; i += j) {
	for (j = 0; i + j < ALPHABET_SIZE && j < 6; ++j) {
	    erts_printf("BS[%03d]:%02ld, ", i+j, bm->badshift[i+j]);
	}
	erts_printf("\n");
    }
}

static void dump_ac_node(ACNode *node, int indent, int ch) {
    int i;
    char *spaces = erts_alloc(ERTS_ALC_T_TMP, 10 * indent + 1);
    memset(spaces,' ',10*indent);
    spaces[10*indent] = '\0';
    erts_printf("%s-> %c\n",spaces,ch);
    erts_printf("%sId: %u\n",spaces,(unsigned) node->id);
    erts_printf("%sD: %u\n",spaces,(unsigned)node->d);
    erts_printf("%sFinal: %d\n",spaces,(int)node->final);
    erts_printf("%sFail: %u\n",spaces,(unsigned)node->h->id);
    erts_free(ERTS_ALC_T_TMP,spaces);
    for(i=0;i<ALPHABET_SIZE;++i) {
	if (node->g[i] != NULL && node->g[i] != node) {
	    dump_ac_node(node->g[i],indent+1,i);
	}
    }
}


static void dump_ac_trie(ACTrie *act)
{
    erts_printf("Aho Corasick Trie dump.\n");
    erts_printf("=======================\n");
    erts_printf("Node counter: %u\n", (unsigned) act->idc);
    erts_printf("Searchstring counter: %u\n", (unsigned) act->counter);
    erts_printf("Trie:\n");
    dump_ac_node(act->root, 0, '0');
    return;
}
#endif
