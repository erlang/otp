/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1996-2013. All Rights Reserved.
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
static int list_to_bitstr_buf(Eterm obj, char* buf, Uint len);
#else
static int list_to_bitstr_buf(Eterm obj, char* buf);
#endif
static int bitstr_list_len(Eterm obj, Uint* num_bytes);

void
erts_init_binary(void)
{
    /* Verify Binary alignment... */
    if ((((UWord) &((Binary *) 0)->orig_bytes[0]) % ((UWord) 8)) != 0) {
	/* I assume that any compiler should be able to optimize this
	   away. If not, this test is not very expensive... */
	erl_exit(ERTS_ABORT_EXIT,
		 "Internal error: Address of orig_bytes[0] of a Binary"
		 " is *not* 8-byte aligned\n");
    }
}

/*
 * Create a brand new binary from scratch.
 */

Eterm
new_binary(Process *p, byte *buf, Uint len)
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
    pb->next = MSO(p).first;
    MSO(p).first = (struct erl_off_heap_header*)pb;
    pb->val = bptr;
    pb->bytes = (byte*) bptr->orig_bytes;
    pb->flags = 0;

    /*
     * Miscellanous updates. Return the tagged binary.
     */
    OH_OVERHEAD(&(MSO(p)), pb->size / sizeof(Eterm));
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
    pb->next = MSO(p).first;
    MSO(p).first = (struct erl_off_heap_header*)pb;
    pb->val = bptr;
    pb->bytes = (byte*) bptr->orig_bytes;
    pb->flags = 0;

    /*
     * Miscellanous updates. Return the tagged binary.
     */
    OH_OVERHEAD(&(MSO(p)), pb->size / sizeof(Eterm));
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
erts_get_aligned_binary_bytes_extra(Eterm bin, byte** base_ptr, ErtsAlcType_t allocator, unsigned extra)
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
	byte* buf = (byte *) erts_alloc(allocator, byte_size + extra);
	*base_ptr = buf;
	buf += extra;
	erts_copy_bits(bytes, bit_offs, 1, buf, 0, 1, byte_size*8);	
	bytes = buf;
    }
    return bytes;
}

Eterm
erts_bin_bytes_to_list(Eterm previous, Eterm* hp, byte* bytes, Uint size, Uint bitoffs)
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

BIF_RETTYPE binary_to_integer_1(BIF_ALIST_1)
{
  byte *temp_alloc = NULL;
  char *bytes;
  Uint size;
  Eterm res;

  if ((bytes = (char*)erts_get_aligned_binary_bytes(BIF_ARG_1, &temp_alloc))
      == NULL )
    goto binary_to_integer_1_error;
  
  size = binary_size(BIF_ARG_1);
  
  if ((res = erts_chars_to_integer(BIF_P,bytes,size,10)) != THE_NON_VALUE) {
    erts_free_aligned_binary_bytes(temp_alloc);
    return res;
  }

 binary_to_integer_1_error:
  erts_free_aligned_binary_bytes(temp_alloc);
  BIF_ERROR(BIF_P, BADARG);
}

BIF_RETTYPE binary_to_integer_2(BIF_ALIST_2)
{
  byte *temp_alloc = NULL;
  char *bytes;
  Uint size;
  int base;
  Eterm res;
  
  if (!is_small(BIF_ARG_2))
    BIF_ERROR(BIF_P, BADARG);

  base = signed_val(BIF_ARG_2);
  
  if (base < 2 || base > 36) 
    BIF_ERROR(BIF_P, BADARG);

  if ((bytes = (char*)erts_get_aligned_binary_bytes(BIF_ARG_1, &temp_alloc))
      == NULL )
    goto binary_to_integer_2_error;
  
  size = binary_size(BIF_ARG_1);
  
  if ((res = erts_chars_to_integer(BIF_P,bytes,size,base)) != THE_NON_VALUE) {
    erts_free_aligned_binary_bytes(temp_alloc);
    return res;
  }

 binary_to_integer_2_error:
  
  erts_free_aligned_binary_bytes(temp_alloc);
  BIF_ERROR(BIF_P, BADARG);

}

BIF_RETTYPE integer_to_binary_1(BIF_ALIST_1)
{   
    Uint size;
    Eterm res;

    if (is_not_integer(BIF_ARG_1)) {
	BIF_ERROR(BIF_P, BADARG);
    }

    if (is_small(BIF_ARG_1)) {
	char *c;
	struct Sint_buf ibuf;

	/* Enhancement: If we can calculate the buffer size exactly
	 * we could avoid an unnecessary copy of buffers.
	 * Useful if size determination is faster than a copy.
	 */
	c = Sint_to_buf(signed_val(BIF_ARG_1), &ibuf);
	size = sys_strlen(c);
	res = new_binary(BIF_P, (byte *)c, size);
    } else {
	byte* bytes;
	Uint n = 0;

	/* Here we also have multiple copies of buffers
	 * due to new_binary interface
	 */
	size = big_decimal_estimate(BIF_ARG_1) - 1; /* remove null */
	bytes = (byte*) erts_alloc(ERTS_ALC_T_TMP, sizeof(byte)*size);
	n = erts_big_to_binary_bytes(BIF_ARG_1, (char *)bytes, size);
	res = new_binary(BIF_P, bytes + size - n, n);
	erts_free(ERTS_ALC_T_TMP, (void *) bytes);
    }
    BIF_RET(res);
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

	BIF_RET(erts_bin_bytes_to_list(NIL, hp, bytes, size, bitoffs));
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
    BIF_RET(erts_bin_bytes_to_list(NIL, hp, bytes+start-1, i, bitoffs));
    
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
    BIF_RET(erts_bin_bytes_to_list(previous, hp, bytes, size, bitoffs));
}


/* Turn a possibly deep list of ints (and binaries) into */
/* One large binary object                               */

/*
 * This bif also exists in the binary module, under the name
 * binary:list_to_bin/1, why it's divided into interface and
 * implementation. Also the backend for iolist_to_binary_1.
 */

BIF_RETTYPE erts_list_to_binary_bif(Process *p, Eterm arg)
{
    Eterm bin;
    ErlDrvSizeT size;
    byte* bytes;
#ifdef DEBUG
    ErlDrvSizeT offset;
#endif

    if (is_nil(arg)) {
	BIF_RET(new_binary(p,(byte*)"",0));
    }
    if (is_not_list(arg)) {
	goto error;
    }
    switch (erts_iolist_size(arg, &size)) {
    case ERTS_IOLIST_OVERFLOW: BIF_ERROR(p, SYSTEM_LIMIT);
    case ERTS_IOLIST_TYPE: goto error;
    default: ;
    }
    bin = new_binary(p, (byte *)NULL, size);
    bytes = binary_bytes(bin);
#ifdef DEBUG
    offset = 
#endif
	erts_iolist_to_buf(arg, (char*) bytes, size);

    ASSERT(offset == 0);
    BIF_RET(bin);
    
 error:
    BIF_ERROR(p, BADARG);
}

BIF_RETTYPE list_to_binary_1(BIF_ALIST_1)
{
    return erts_list_to_binary_bif(BIF_P, BIF_ARG_1);
}

/* Turn a possibly deep list of ints (and binaries) into */
/* One large binary object                               */

BIF_RETTYPE iolist_to_binary_1(BIF_ALIST_1)
{
    if (is_binary(BIF_ARG_1)) {
	BIF_RET(BIF_ARG_1);
    }
    return erts_list_to_binary_bif(BIF_P, BIF_ARG_1);
}

BIF_RETTYPE list_to_bitstring_1(BIF_ALIST_1)
{
    Eterm bin;
    Uint sz;
    int offset;
    byte* bytes;
    ErlSubBin* sb1; 
    Eterm* hp;
    
    if (is_nil(BIF_ARG_1)) {
	BIF_RET(new_binary(BIF_P,(byte*)"",0));
    }
    if (is_not_list(BIF_ARG_1)) {
    error:
	BIF_ERROR(BIF_P, BADARG);
    }
    switch (bitstr_list_len(BIF_ARG_1, &sz)) {
    case ERTS_IOLIST_TYPE:
	goto error;
    case ERTS_IOLIST_OVERFLOW:
	BIF_ERROR(BIF_P, SYSTEM_LIMIT);
    }
    bin = new_binary(BIF_P, (byte *)NULL, sz);
    bytes = binary_bytes(bin);
#ifdef DEBUG
    offset = list_to_bitstr_buf(BIF_ARG_1, (char*) bytes, sz);
#else
    offset = list_to_bitstr_buf(BIF_ARG_1, (char*) bytes);
#endif
    ASSERT(offset >= 0);
    if (offset > 0) {
	hp = HAlloc(BIF_P, ERL_SUB_BIN_SIZE);
	sb1 = (ErlSubBin *) hp;
	sb1->thing_word = HEADER_SUB_BIN;
	sb1->size = sz-1;
	sb1->offs = 0;
	sb1->orig = bin;
	sb1->bitoffs = 0;
	sb1->bitsize = offset;
	sb1->is_writable = 0;
	bin = make_binary(sb1);
    }
    
    BIF_RET(bin);
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
list_to_bitstr_buf(Eterm obj, char* buf, Uint len)
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

static int
bitstr_list_len(Eterm obj, Uint* num_bytes)
{
    Eterm* objp;
    Uint len = 0;
    Uint offs = 0;
    DECLARE_ESTACK(s);
    goto L_again;

#define SAFE_ADD(Var, Val)			\
    do {					\
        Uint valvar = (Val);			\
	Var += valvar;				\
	if (Var < valvar) {			\
	    goto L_overflow_error;		\
	}					\
    } while (0)

#define SAFE_ADD_BITSIZE(Var, Bin)					\
    do {								\
	if (*binary_val(Bin) == HEADER_SUB_BIN) {			\
            Uint valvar = ((ErlSubBin *) binary_val(Bin))->bitsize;	\
	    Var += valvar;						\
	    if (Var < valvar) {						\
	         goto L_overflow_error;					\
	    }								\
        }								\
    } while (0)

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
		if (len == 0) {
		    goto L_overflow_error;
		}
	    } else if (is_binary(obj)) {
		SAFE_ADD(len, binary_size(obj));
		SAFE_ADD_BITSIZE(offs, obj);
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
		SAFE_ADD(len, binary_size(obj));
		SAFE_ADD_BITSIZE(offs, obj);
	    } else if (is_not_nil(obj)) {
		goto L_type_error;
	    }
	} else if (is_binary(obj)) {
	    SAFE_ADD(len, binary_size(obj));
	    SAFE_ADD_BITSIZE(offs, obj);
	} else if (is_not_nil(obj)) {
	    goto L_type_error;
	}
    }
#undef SAFE_ADD
#undef SAFE_ADD_BITSIZE

    DESTROY_ESTACK(s);

    /*
     * Make sure that the number of bits in the bitstring will fit
     * in an Uint to ensure that the binary can be matched using
     * the binary syntax.
     */
    if (len << 3 < len) {
	goto L_overflow_error;
    }
    len += (offs >> 3) + ((offs & 7) != 0);
    if (len << 3 < len) {
	goto L_overflow_error;
    }
    *num_bytes = len;
    return ERTS_IOLIST_OK;

 L_type_error:
    DESTROY_ESTACK(s);
    return ERTS_IOLIST_TYPE;

 L_overflow_error:
    DESTROY_ESTACK(s);
    return ERTS_IOLIST_OVERFLOW;
}

