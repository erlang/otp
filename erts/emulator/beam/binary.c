/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1996-2017. All Rights Reserved.
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

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "erl_vm.h"
#include "global.h"
#include "erl_process.h"
#include "error.h"
#define ERL_WANT_HIPE_BIF_WRAPPER__
#include "bif.h"
#undef ERL_WANT_HIPE_BIF_WRAPPER__
#include "big.h"
#include "erl_binary.h"
#include "erl_bits.h"

#define L2B_B2L_MIN_EXEC_REDS (CONTEXT_REDS/4)
#define L2B_B2L_RESCHED_REDS (CONTEXT_REDS/40)

static Export binary_to_list_continue_export;
static Export list_to_binary_continue_export;

static BIF_RETTYPE binary_to_list_continue(BIF_ALIST_1);
static BIF_RETTYPE list_to_binary_continue(BIF_ALIST_1);

void
erts_init_binary(void)
{
    /* Verify Binary alignment... */
    ERTS_CT_ASSERT((offsetof(Binary,orig_bytes) % 8) == 0);
    ERTS_CT_ASSERT((offsetof(ErtsMagicBinary,u.aligned.data) % 8) == 0);

    erts_init_trap_export(&binary_to_list_continue_export,
			  am_erts_internal, am_binary_to_list_continue, 1,
			  &binary_to_list_continue);

    erts_init_trap_export(&list_to_binary_continue_export,
			  am_erts_internal, am_list_to_binary_continue, 1,
			  &list_to_binary_continue);

}

static ERTS_INLINE
Eterm build_proc_bin(ErlOffHeap* ohp, Eterm* hp, Binary* bptr)
{
    ProcBin* pb = (ProcBin *) hp;
    pb->thing_word = HEADER_PROC_BIN;
    pb->size = bptr->orig_size;
    pb->next = ohp->first;
    ohp->first = (struct erl_off_heap_header*)pb;
    pb->val = bptr;
    pb->bytes = (byte*) bptr->orig_bytes;
    pb->flags = 0;
    OH_OVERHEAD(ohp, pb->size / sizeof(Eterm));

    return make_binary(pb);
}

/** @brief Initiate a ProcBin for a full Binary.
 *  @param hp must point to PROC_BIN_SIZE available heap words.
 */
Eterm erts_build_proc_bin(ErlOffHeap* ohp, Eterm* hp, Binary* bptr)
{
    return build_proc_bin(ohp, hp, bptr);
}

/*
 * Create a brand new binary from scratch.
 */
Eterm
new_binary(Process *p, byte *buf, Uint len)
{
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
    if (buf != NULL) {
	sys_memcpy(bptr->orig_bytes, buf, len);
    }

    return build_proc_bin(&MSO(p), HAlloc(p, PROC_BIN_SIZE), bptr);
}

/* 
 * When heap binary is not desired...
 */

Eterm erts_new_mso_binary(Process *p, byte *buf, Uint len)
{
    Binary* bptr;

    /*
     * Allocate the binary struct itself.
     */
    bptr = erts_bin_nrml_alloc(len);
    if (buf != NULL) {
	sys_memcpy(bptr->orig_bytes, buf, len);
    }

    return build_proc_bin(&MSO(p), HAlloc(p, PROC_BIN_SIZE), bptr);
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

#define ERTS_B2L_BYTES_PER_REDUCTION 256

typedef struct {
    Eterm res;
    Eterm *hp;
#ifdef DEBUG
    Eterm *hp_end;
#endif
    byte *bytes;
    Uint size;
    Uint bitoffs;
} ErtsB2LState;

static int b2l_state_destructor(Binary *mbp)
{
    ASSERT(ERTS_MAGIC_BIN_DESTRUCTOR(mbp) == b2l_state_destructor);
    return 1;
}

static BIF_RETTYPE
binary_to_list_chunk(Process *c_p,
		     Eterm mb_eterm,
		     ErtsB2LState* sp,
		     int reds_left,
		     int gc_disabled)
{
    BIF_RETTYPE ret;
    int bump_reds;
    Uint size;
    byte *bytes;

    size = (reds_left + 1)*ERTS_B2L_BYTES_PER_REDUCTION;
    if (size > sp->size)
	size = sp->size;
    bytes = sp->bytes + (sp->size - size);

    bump_reds = (size - 1)/ERTS_B2L_BYTES_PER_REDUCTION + 1;
    BUMP_REDS(c_p, bump_reds);

    ASSERT(is_list(sp->res) || is_nil(sp->res));

    sp->res = erts_bin_bytes_to_list(sp->res,
				     sp->hp,
				     bytes,
				     size,
				     sp->bitoffs);
    sp->size -= size;
    sp->hp += 2*size;

    if (sp->size > 0) {

	if (!gc_disabled)
	    erts_set_gc_state(c_p, 0);

	ASSERT(c_p->flags & F_DISABLE_GC);
	ASSERT(is_value(mb_eterm));
	ERTS_BIF_PREP_TRAP1(ret,
			    &binary_to_list_continue_export,
			    c_p,
			    mb_eterm);
    }
    else {

	ASSERT(sp->hp == sp->hp_end);
	ASSERT(sp->size == 0);

	if (!gc_disabled || !erts_set_gc_state(c_p, 1))
	    ERTS_BIF_PREP_RET(ret, sp->res);
	else
	    ERTS_BIF_PREP_YIELD_RETURN(ret, c_p, sp->res);
	ASSERT(!(c_p->flags & F_DISABLE_GC));
    }

    return ret;
}

static ERTS_INLINE BIF_RETTYPE
binary_to_list(Process *c_p, Eterm *hp, Eterm tail, byte *bytes,
	       Uint size, Uint bitoffs, int reds_left, int one_chunk)
{
    if (one_chunk) {
	Eterm res;
	BIF_RETTYPE ret;
	int bump_reds = (size - 1)/ERTS_B2L_BYTES_PER_REDUCTION + 1;
	BUMP_REDS(c_p, bump_reds);
	res = erts_bin_bytes_to_list(tail, hp, bytes, size, bitoffs);
	ERTS_BIF_PREP_RET(ret, res);
	return ret;
    }
    else {
	Binary *mbp = erts_create_magic_binary(sizeof(ErtsB2LState),
					       b2l_state_destructor);
	ErtsB2LState *sp = ERTS_MAGIC_BIN_DATA(mbp);
	Eterm mb;

	sp->res = tail;
	sp->hp = hp;
#ifdef DEBUG
	sp->hp_end = sp->hp + 2*size;
#endif
	sp->bytes = bytes;
	sp->size = size;
	sp->bitoffs = bitoffs;

	hp = HAlloc(c_p, ERTS_MAGIC_REF_THING_SIZE);
	mb = erts_mk_magic_ref(&hp, &MSO(c_p), mbp);
	return binary_to_list_chunk(c_p, mb, sp, reds_left, 0);
    }
}

static BIF_RETTYPE binary_to_list_continue(BIF_ALIST_1)
{
    Binary *mbp = erts_magic_ref2bin(BIF_ARG_1);

    ASSERT(ERTS_MAGIC_BIN_DESTRUCTOR(mbp) == b2l_state_destructor);
    ASSERT(BIF_P->flags & F_DISABLE_GC);

    return binary_to_list_chunk(BIF_P,
				BIF_ARG_1,
				(ErtsB2LState*) ERTS_MAGIC_BIN_DATA(mbp),
				ERTS_BIF_REDS_LEFT(BIF_P),
				1);
}

HIPE_WRAPPER_BIF_DISABLE_GC(binary_to_list, 1)

BIF_RETTYPE binary_to_list_1(BIF_ALIST_1)
{
    Eterm real_bin;
    Uint offset;
    Uint size;
    Uint bitsize;
    Uint bitoffs;
    int reds_left;
    int one_chunk;

    if (is_not_binary(BIF_ARG_1)) {
	goto error;
    }

    size = binary_size(BIF_ARG_1);
    reds_left = ERTS_BIF_REDS_LEFT(BIF_P);
    one_chunk = size < reds_left*ERTS_B2L_BYTES_PER_REDUCTION;
    if (!one_chunk) {
	if (size < L2B_B2L_MIN_EXEC_REDS*ERTS_B2L_BYTES_PER_REDUCTION) {
	    if (reds_left <= L2B_B2L_RESCHED_REDS) {
		/* Yield and do it with full context reds... */
		ERTS_BIF_YIELD1(bif_export[BIF_binary_to_list_1],
				BIF_P, BIF_ARG_1);
	    }
	    /* Allow a bit more reductions... */
	    one_chunk = 1;
	    reds_left = L2B_B2L_MIN_EXEC_REDS;
	}
    }

    ERTS_GET_REAL_BIN(BIF_ARG_1, real_bin, offset, bitoffs, bitsize);
    if (bitsize != 0) {
	goto error;
    }
    if (size == 0) {
	BIF_RET(NIL);
    } else {
	Eterm* hp = HAlloc(BIF_P, 2 * size);
	byte* bytes = binary_bytes(real_bin)+offset;
	return binary_to_list(BIF_P, hp, NIL, bytes, size,
			      bitoffs, reds_left, one_chunk);
    }

    error:
	BIF_ERROR(BIF_P, BADARG);
}

HIPE_WRAPPER_BIF_DISABLE_GC(binary_to_list, 3)

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
    int reds_left;
    int one_chunk;

    if (is_not_binary(BIF_ARG_1)) {
	goto error;
    }
    if (!term_to_Uint(BIF_ARG_2, &start) || !term_to_Uint(BIF_ARG_3, &stop)) {
	goto error;
    }
    size = binary_size(BIF_ARG_1);
    reds_left = ERTS_BIF_REDS_LEFT(BIF_P);
    one_chunk = size < reds_left*ERTS_B2L_BYTES_PER_REDUCTION;
    if (!one_chunk) {
	if (size < L2B_B2L_MIN_EXEC_REDS*ERTS_B2L_BYTES_PER_REDUCTION) {
	    if (reds_left <= L2B_B2L_RESCHED_REDS) {
		/* Yield and do it with full context reds... */
		ERTS_BIF_YIELD3(bif_export[BIF_binary_to_list_3],
				BIF_P, BIF_ARG_1, BIF_ARG_2, BIF_ARG_3);
	    }
	    /* Allow a bit more reductions... */
	    one_chunk = 1;
	    reds_left = L2B_B2L_MIN_EXEC_REDS;
	}
    }

    ERTS_GET_BINARY_BYTES(BIF_ARG_1, bytes, bitoffs, bitsize);
    if (start < 1 || start > size || stop < 1 ||
	stop > size || stop < start ) {
	goto error;
    }
    i = stop-start+1;
    hp = HAlloc(BIF_P, 2*i);
    return binary_to_list(BIF_P, hp, NIL, bytes+start-1, i,
			  bitoffs, reds_left, one_chunk);
    error:
	BIF_ERROR(BIF_P, BADARG);
}

HIPE_WRAPPER_BIF_DISABLE_GC(bitstring_to_list, 1)

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
    int reds_left;
    int one_chunk;

    if (is_not_binary(BIF_ARG_1)) {
	BIF_ERROR(BIF_P, BADARG);
    }
    size = binary_size(BIF_ARG_1);
    reds_left = ERTS_BIF_REDS_LEFT(BIF_P);
    one_chunk = size < reds_left*ERTS_B2L_BYTES_PER_REDUCTION;
    if (!one_chunk) {
	if (size < L2B_B2L_MIN_EXEC_REDS*ERTS_B2L_BYTES_PER_REDUCTION) {
	    if (reds_left <= L2B_B2L_RESCHED_REDS) {
		/* Yield and do it with full context reds... */
		ERTS_BIF_YIELD1(bif_export[BIF_bitstring_to_list_1],
				BIF_P, BIF_ARG_1);
	    }
	    /* Allow a bit more reductions... */
	    one_chunk = 1;
	    reds_left = L2B_B2L_MIN_EXEC_REDS;
	}
    }
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

    return binary_to_list(BIF_P, hp, previous, bytes, size,
			  bitoffs, reds_left, one_chunk);
}


/* Turn a possibly deep list of ints (and binaries) into */
/* One large binary object                               */

typedef enum {
    ERTS_L2B_OK,
    ERTS_L2B_YIELD,
    ERTS_L2B_TYPE_ERROR,
    ERTS_L2B_OVERFLOW_ERROR
} ErtsL2BResult;

#define ERTS_L2B_STATE_INITER(C_P, ARG, BIF, SZFunc, TBufFunc)	\
    {ERTS_IOLIST2BUF_STATE_INITER((C_P), (ARG)),				\
	    (ARG), THE_NON_VALUE, (BIF), (SZFunc), (TBufFunc)}

#define ERTS_L2B_STATE_MOVE(TO, FROM) \
    sys_memcpy((void *) (TO), (void *) (FROM), sizeof(ErtsL2BState))

typedef struct ErtsL2BState_ ErtsL2BState;

struct ErtsL2BState_ {
    ErtsIOList2BufState buf;
    Eterm arg;
    Eterm bin;
    Export *bif;
    int (*iolist_to_buf_size)(ErtsIOListState *);
    ErlDrvSizeT (*iolist_to_buf)(ErtsIOList2BufState *);
};

static ERTS_INLINE ErtsL2BResult
list_to_binary_engine(ErtsL2BState *sp)
{
    ErlDrvSizeT res;
    Process *c_p = sp->buf.iolist.c_p;

    /*
     * have_size == 0 while sp->iolist_to_buf_size()
     * has not finished the calculation.
     */

    if (!sp->buf.iolist.have_size) {
	switch (sp->iolist_to_buf_size(&sp->buf.iolist)) {
	case ERTS_IOLIST_YIELD:
	    return ERTS_L2B_YIELD;
	case ERTS_IOLIST_OVERFLOW:
	    return ERTS_L2B_OVERFLOW_ERROR;
	case ERTS_IOLIST_TYPE:
	    return ERTS_L2B_TYPE_ERROR;
	case ERTS_IOLIST_OK:
	    break;
	default:
	    ASSERT(0);
	    break;
	}

	ASSERT(sp->buf.iolist.have_size);

	/*
	 * Size calculated... Setup state for
	 * sp->iolist_to_buf_*()
	 */

	sp->bin = new_binary(c_p,
			     (byte *) NULL,
			     sp->buf.iolist.size);

	if (sp->buf.iolist.size == 0)
	    return ERTS_L2B_OK;

	sp->buf.buf = (char *) binary_bytes(sp->bin);
	sp->buf.len = sp->buf.iolist.size;
	sp->buf.iolist.obj = sp->arg;

	if (sp->buf.iolist.reds_left <= 0) {
	    BUMP_ALL_REDS(c_p);
	    return ERTS_L2B_YIELD;
	}
    }

    ASSERT(sp->buf.iolist.size != 0);
    ASSERT(is_value(sp->bin));
    ASSERT(sp->buf.buf);

    res = sp->iolist_to_buf(&sp->buf);

    if (!ERTS_IOLIST_TO_BUF_FAILED(res)) {
	ASSERT(res == 0);
	return ERTS_L2B_OK;
    }

    switch (res) {
    case ERTS_IOLIST_TO_BUF_YIELD:
	return ERTS_L2B_YIELD;
    case ERTS_IOLIST_TO_BUF_OVERFLOW:
	return ERTS_L2B_OVERFLOW_ERROR;
    case ERTS_IOLIST_TO_BUF_TYPE_ERROR:
	return ERTS_L2B_TYPE_ERROR;
    default:
	ERTS_INTERNAL_ERROR("Invalid return value from iolist_to_buf_yielding()");
	return ERTS_L2B_TYPE_ERROR;
    }
}

static int
l2b_state_destructor(Binary *mbp)
{
    ErtsL2BState *sp = ERTS_MAGIC_BIN_DATA(mbp); 
    ASSERT(ERTS_MAGIC_BIN_DESTRUCTOR(mbp) == l2b_state_destructor);
    DESTROY_SAVED_ESTACK(&sp->buf.iolist.estack);
    return 1;
}

static ERTS_INLINE Eterm
l2b_final_touch(Process *c_p, ErtsL2BState *sp)
{
    Eterm *hp;
    ErlSubBin* sbin; 
    if (sp->buf.offset == 0)
	return sp->bin;

    hp = HAlloc(c_p, ERL_SUB_BIN_SIZE);
    ASSERT(sp->buf.offset > 0);
    sbin = (ErlSubBin *) hp;
    sbin->thing_word = HEADER_SUB_BIN;
    sbin->size = sp->buf.iolist.size-1;
    sbin->offs = 0;
    sbin->orig = sp->bin;
    sbin->bitoffs = 0;
    sbin->bitsize = sp->buf.offset;
    sbin->is_writable = 0;
    return make_binary(sbin);
}

static BIF_RETTYPE
list_to_binary_chunk(Eterm mb_eterm,
		     ErtsL2BState* sp,
		     int reds_left,
		     int gc_disabled)
{
    Eterm err = BADARG;
    BIF_RETTYPE ret;
    Process *c_p = sp->buf.iolist.c_p;

    sp->buf.iolist.reds_left = reds_left;
    
    switch (list_to_binary_engine(sp)) {

    case ERTS_L2B_OK: {
	Eterm result = l2b_final_touch(c_p, sp);
	if (!gc_disabled || !erts_set_gc_state(c_p, 1))
	    ERTS_BIF_PREP_RET(ret, result);
	else
	    ERTS_BIF_PREP_YIELD_RETURN(ret, c_p, result);
	ASSERT(!(c_p->flags & F_DISABLE_GC));
	break;
    }
    case ERTS_L2B_YIELD:
	if (!gc_disabled) {
	    /* first yield... */
	    Eterm *hp;
	    Binary *mbp = erts_create_magic_binary(sizeof(ErtsL2BState),
						   l2b_state_destructor);
	    ErtsL2BState *new_sp = ERTS_MAGIC_BIN_DATA(mbp);

	    ERTS_L2B_STATE_MOVE(new_sp, sp);
	    sp = new_sp;

	    hp = HAlloc(c_p, ERTS_MAGIC_REF_THING_SIZE);
	    mb_eterm = erts_mk_magic_ref(&hp, &MSO(c_p), mbp);

	    ASSERT(is_value(mb_eterm));

	    erts_set_gc_state(c_p, 0);
	}

	ASSERT(c_p->flags & F_DISABLE_GC);

	ERTS_BIF_PREP_TRAP1(ret,
			    &list_to_binary_continue_export,
			    c_p,
			    mb_eterm);
	break;

    case ERTS_L2B_OVERFLOW_ERROR:
	err = SYSTEM_LIMIT;
	/* fall through */

    case ERTS_L2B_TYPE_ERROR:
	if (!gc_disabled)
	    ERTS_BIF_PREP_ERROR(ret, c_p, err);
	else {
	    if (erts_set_gc_state(c_p, 1))
		ERTS_VBUMP_ALL_REDS(c_p);

	    ERTS_BIF_PREP_ERROR_TRAPPED1(ret,
					 c_p,
					 err,
					 sp->bif,
					 sp->arg);
	}

	ASSERT(!(c_p->flags & F_DISABLE_GC));
	break;

    default:
	ERTS_INTERNAL_ERROR("Invalid return value from list_to_binary_engine()");
	ERTS_BIF_PREP_ERROR(ret,c_p, EXC_INTERNAL_ERROR);
	break;
    }
    return ret;
}

static BIF_RETTYPE list_to_binary_continue(BIF_ALIST_1)
{
    Binary *mbp = erts_magic_ref2bin(BIF_ARG_1);

    ASSERT(ERTS_MAGIC_BIN_DESTRUCTOR(mbp) == l2b_state_destructor);
    ASSERT(BIF_P->flags & F_DISABLE_GC);

    return list_to_binary_chunk(BIF_ARG_1,
				ERTS_MAGIC_BIN_DATA(mbp),
				ERTS_BIF_REDS_LEFT(BIF_P),
				1);
}

BIF_RETTYPE erts_list_to_binary_bif(Process *c_p, Eterm arg, Export *bif)
{
    int orig_reds_left = ERTS_BIF_REDS_LEFT(c_p);
    BIF_RETTYPE ret;

    if (orig_reds_left < L2B_B2L_MIN_EXEC_REDS) {
	if (orig_reds_left <= L2B_B2L_RESCHED_REDS) {
	    /* Yield and do it with full context reds... */
	    ERTS_BIF_PREP_YIELD1(ret, bif, c_p, arg);
	    return ret;
	}
	/* Allow a bit more reductions... */
	orig_reds_left = L2B_B2L_MIN_EXEC_REDS;
    }

    if (is_nil(arg))
	ERTS_BIF_PREP_RET(ret, new_binary(c_p, (byte *) "", 0));
    else if (is_not_list(arg))
	ERTS_BIF_PREP_ERROR(ret, c_p, BADARG);
    else {
	/* check for [binary()] case */
	Eterm h = CAR(list_val(arg));
	Eterm t = CDR(list_val(arg));
	if (is_binary(h)
	    && is_nil(t)
	    && !(HEADER_SUB_BIN == *(binary_val(h))
		 && (((ErlSubBin *)binary_val(h))->bitoffs != 0
		     || ((ErlSubBin *)binary_val(h))->bitsize != 0))) {
	    ERTS_BIF_PREP_RET(ret, h);
	}
	else {
	    ErtsL2BState state = ERTS_L2B_STATE_INITER(c_p,
						       arg,
						       bif,
						       erts_iolist_size_yielding,
						       erts_iolist_to_buf_yielding);

	    /*
	     * First try to do it all at once without having to use
	     * yielding iolist_to_buf().
	     */
	    state.buf.iolist.reds_left = orig_reds_left;
	    switch (erts_iolist_size_yielding(&state.buf.iolist)) {
	    case ERTS_IOLIST_OK: {
		ErlDrvSizeT size = state.buf.iolist.size;
		Eterm bin;
		char *buf;

		if (size == 0) {
		    ERTS_BIF_PREP_RET(ret, new_binary(c_p, (byte *) NULL, 0));
		    break; /* done */
		}

		bin = new_binary(c_p, (byte *) NULL, size);
		buf = (char *) binary_bytes(bin);

		if (size < ERTS_IOLIST_TO_BUF_BYTES_PER_RED*CONTEXT_REDS) {
		    /* An (over) estimation of reductions  needed */
		    int reds_left = state.buf.iolist.reds_left;
		    int to_buf_reds = orig_reds_left - reds_left;
		    to_buf_reds += size/ERTS_IOLIST_TO_BUF_BYTES_PER_RED;
		    if (to_buf_reds <= reds_left) {
			ErlDrvSizeT res;

			res = erts_iolist_to_buf(arg, buf, size);
			if (res == 0) {
			    BUMP_REDS(c_p, to_buf_reds);
			    ERTS_BIF_PREP_RET(ret, bin);
			    break; /* done */
			}
			if (!ERTS_IOLIST_TO_BUF_FAILED(res))
			    ERTS_INTERNAL_ERROR("iolist_size/iolist_to_buf missmatch");
			if (res == ERTS_IOLIST_TO_BUF_OVERFLOW)
			    goto overflow;
			goto type_error;
		    }
		}
		/*
		 * Since size has been computed list_to_binary_chunk() expects
		 * state prepared for iolist_to_buf.
		 */
		state.bin = bin;
		state.buf.buf = buf;
		state.buf.len = size;
		state.buf.iolist.obj = arg;
		/* Fall through... */
	    }
	    case ERTS_IOLIST_YIELD:
		ret = list_to_binary_chunk(THE_NON_VALUE,
					   &state,
					   state.buf.iolist.reds_left,
					   0);
		break;
	    case ERTS_IOLIST_OVERFLOW:
	    overflow:
		ERTS_BIF_PREP_ERROR(ret, c_p, SYSTEM_LIMIT);
		break;
	    case ERTS_IOLIST_TYPE:
	    type_error:
	    default:
		ERTS_BIF_PREP_ERROR(ret, c_p, BADARG);
		break;
	    }
	}
    }
    return ret;
}

HIPE_WRAPPER_BIF_DISABLE_GC(list_to_binary, 1)

BIF_RETTYPE list_to_binary_1(BIF_ALIST_1)
{
    return erts_list_to_binary_bif(BIF_P, BIF_ARG_1, bif_export[BIF_list_to_binary_1]);
}

HIPE_WRAPPER_BIF_DISABLE_GC(iolist_to_binary, 1)

BIF_RETTYPE iolist_to_binary_1(BIF_ALIST_1)
{
    if (is_binary(BIF_ARG_1)) {
        if (binary_bitsize(BIF_ARG_1) == 0) {
            BIF_RET(BIF_ARG_1);
        }
        BIF_ERROR(BIF_P, BADARG);
    }
    return erts_list_to_binary_bif(BIF_P, BIF_ARG_1, bif_export[BIF_iolist_to_binary_1]);
}

static int bitstr_list_len(ErtsIOListState *);
static ErlDrvSizeT list_to_bitstr_buf_yielding(ErtsIOList2BufState *);
static ErlDrvSizeT list_to_bitstr_buf_not_yielding(ErtsIOList2BufState *);

HIPE_WRAPPER_BIF_DISABLE_GC(list_to_bitstring, 1)

BIF_RETTYPE list_to_bitstring_1(BIF_ALIST_1)
{
    BIF_RETTYPE ret;

    if (is_nil(BIF_ARG_1))
	ERTS_BIF_PREP_RET(ret, new_binary(BIF_P, (byte *) "", 0));
    else if (is_not_list(BIF_ARG_1))
	ERTS_BIF_PREP_ERROR(ret, BIF_P, BADARG);
    else {
	/* check for [bitstring()] case */
	Eterm h = CAR(list_val(BIF_ARG_1));
	Eterm t = CDR(list_val(BIF_ARG_1));
	if (is_binary(h) && is_nil(t)) {
	    ERTS_BIF_PREP_RET(ret, h);
	}
	else {
	    ErtsL2BState state = ERTS_L2B_STATE_INITER(BIF_P,
						       BIF_ARG_1,
						       bif_export[BIF_list_to_bitstring_1],
						       bitstr_list_len,
						       list_to_bitstr_buf_yielding);
	    int orig_reds_left = ERTS_BIF_REDS_LEFT(BIF_P);

	    /*
	     * First try to do it all at once without having to use
	     * yielding list_to_bitstr_buf().
	     */
	    state.buf.iolist.reds_left = orig_reds_left;
	    switch (bitstr_list_len(&state.buf.iolist)) {
	    case ERTS_IOLIST_OK: {
		ErlDrvSizeT size = state.buf.iolist.size;
		
		state.bin = new_binary(BIF_P, (byte *) NULL, size);
		state.buf.buf = (char *) binary_bytes(state.bin);
		state.buf.len = size;
		state.buf.iolist.obj = BIF_ARG_1;

		if (size < ERTS_IOLIST_TO_BUF_BYTES_PER_RED*CONTEXT_REDS) {
		    /* An (over) estimation of reductions needed */
		    int reds_left = state.buf.iolist.reds_left;
		    int to_buf_reds = orig_reds_left - reds_left;
		    to_buf_reds += size/ERTS_IOLIST_TO_BUF_BYTES_PER_RED;
		    if (to_buf_reds <= reds_left) {
			ErlDrvSizeT res;

			res = list_to_bitstr_buf_not_yielding(&state.buf);
			if (res == 0) {
			    Eterm res_bin = l2b_final_touch(BIF_P, &state);
			    BUMP_REDS(BIF_P, to_buf_reds);
			    ERTS_BIF_PREP_RET(ret, res_bin);
			    break; /* done */
			}
			if (!ERTS_IOLIST_TO_BUF_FAILED(res))
			    ERTS_INTERNAL_ERROR("iolist_size/iolist_to_buf missmatch");
			if (res == ERTS_IOLIST_TO_BUF_OVERFLOW)
			    goto overflow;
			goto type_error;
		    }
		}
		/*
		 * Since size has been computed list_to_binary_chunk() expects
		 * the state prepared for list_to_bitstr_buf.
		 */

		/* Fall through... */
	    }
	    case ERTS_IOLIST_YIELD:
		ret = list_to_binary_chunk(THE_NON_VALUE,
					   &state,
					   state.buf.iolist.reds_left,
					   0);
		break;
	    case ERTS_IOLIST_OVERFLOW:
	    overflow:
		ERTS_BIF_PREP_ERROR(ret, BIF_P, SYSTEM_LIMIT);
		break;
	    case ERTS_IOLIST_TYPE:
	    type_error:
	    default:
		ERTS_BIF_PREP_ERROR(ret, BIF_P, BADARG);
		break;
	    }
	}
    }

    return ret;
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

static int
list_to_bitstr_buf_bcopy(ErtsIOList2BufState *state, Eterm obj, int *yield_countp);

/*
 * The input list is assumed to be type-correct and the buffer is
 * assumed to be of sufficient size. Those assumptions are verified in
 * the DEBUG-built emulator.
 */
static ErlDrvSizeT
list_to_bitstr_buf(int yield_support, ErtsIOList2BufState *state)
{

#undef LIST_TO_BITSTR_BUF_BCOPY_DBG
#undef LIST_TO_BITSTR_BUF_BCOPY
#ifdef DEBUG
#define LIST_TO_BITSTR_BUF_BCOPY_DBG					\
    len -= size + (offset>7);
#else
#define LIST_TO_BITSTR_BUF_BCOPY_DBG
#endif
#define LIST_TO_BITSTR_BUF_BCOPY(CONSP)					\
    do {								\
	byte* bptr;							\
	Uint bitsize;							\
	Uint bitoffs;							\
	Uint num_bits;							\
	size_t size = binary_size(obj);					\
	if (yield_support) {						\
	    size_t max_size = ERTS_IOLIST_TO_BUF_BYTES_PER_YIELD_COUNT;	\
	    if (yield_count > 0)					\
		max_size *= yield_count+1;				\
	    if (size > max_size) {					\
		state->objp = CONSP;					\
		goto L_bcopy_yield;					\
	    }								\
	    if (size >= ERTS_IOLIST_TO_BUF_BYTES_PER_YIELD_COUNT) {	\
		int cost = (int) size;					\
		cost /= ERTS_IOLIST_TO_BUF_BYTES_PER_YIELD_COUNT;	\
		yield_count -= cost;					\
	    }								\
	}								\
	ASSERT(size <= len);						\
	ERTS_GET_BINARY_BYTES(obj, bptr, bitoffs, bitsize);		\
	num_bits = 8*size+bitsize;					\
	copy_binary_to_buffer(buf, offset, bptr, bitoffs, num_bits);	\
	offset += bitsize;						\
	buf += size + (offset>7);					\
	LIST_TO_BITSTR_BUF_BCOPY_DBG;					\
	offset = offset & 7;						\
    } while(0)

#ifdef DEBUG
    ErlDrvSizeT len;
#endif
    Eterm obj;
    char *buf;
    Eterm *objp = NULL;
    int offset;
    int init_yield_count = 0, yield_count;
    DECLARE_ESTACK(s);

    obj = state->iolist.obj;
    buf = state->buf;
    offset = state->offset;
#ifdef DEBUG
    len = state->len;
#endif

    if (!yield_support) {
	yield_count = init_yield_count = 0; /* Shut up faulty warning... >:-( */
	goto L_again;
    }
    else {

	if (state->iolist.reds_left <= 0)
	    return ERTS_IOLIST_TO_BUF_YIELD;

	ESTACK_CHANGE_ALLOCATOR(s, ERTS_ALC_T_SAVED_ESTACK);
	init_yield_count = (ERTS_IOLIST_TO_BUF_YIELD_COUNT_PER_RED
			    * state->iolist.reds_left);
	yield_count = init_yield_count;

	if (!state->iolist.estack.start)
	    goto L_again;
	else {
	    int chk_stack;
	    /* Restart; restore state... */
	    ESTACK_RESTORE(s, &state->iolist.estack);

	    if (!state->bcopy.bptr)
		chk_stack = 0;
	    else {
		chk_stack = 1;
		if (list_to_bitstr_buf_bcopy(state, THE_NON_VALUE, &yield_count)) {
		    /* Yield again... */
		    BUMP_ALL_REDS(state->iolist.c_p);
		    state->iolist.reds_left = 0;
		    ESTACK_SAVE(s, &state->iolist.estack);
		    return ERTS_IOLIST_TO_BUF_YIELD;
		}
		buf = state->buf;
		offset = state->offset;
#ifdef DEBUG
		len = state->len;
#endif
	    }

	    objp = state->objp;
	    state->objp = NULL;

	    if (objp)
		goto L_tail;
	    if (!chk_stack)
		goto L_again;
	    /* check stack */
	}
    }
    
    while (!ESTACK_ISEMPTY(s)) {
	obj = ESTACK_POP(s);
    L_again:
	if (is_list(obj)) {
	    while (1) { /* Tail loop */
		while (1) { /* Head loop */
		    if (yield_support && --yield_count <= 0)
			goto L_yield;
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
			LIST_TO_BITSTR_BUF_BCOPY(objp);
		    } else if (is_list(obj)) {
			ESTACK_PUSH(s, CDR(objp));
			continue; /* Head loop */
		    } else {
			ASSERT(is_nil(obj));
		    }
		    break;
		}

	    L_tail:

		obj = CDR(objp);
		if (is_list(obj)) {
		    continue; /* Tail loop */
		} else if (is_binary(obj)) {
		    LIST_TO_BITSTR_BUF_BCOPY(NULL);
		} else {
		    ASSERT(is_nil(obj));
		}
		break;
	    }
	} else if (is_binary(obj)) {
	    LIST_TO_BITSTR_BUF_BCOPY(NULL);
	} else {
	    if (yield_support && --yield_count <= 0)
		goto L_yield;
	    ASSERT(is_nil(obj));
	}
    }
    
    DESTROY_ESTACK(s);

    if (yield_support) {
	int reds;
	CLEAR_SAVED_ESTACK(&state->iolist.estack);
	reds = ((init_yield_count - yield_count - 1)
		/ ERTS_IOLIST_TO_BUF_YIELD_COUNT_PER_RED) + 1;
	BUMP_REDS(state->iolist.c_p, reds);
	state->iolist.reds_left -= reds;
	if (state->iolist.reds_left < 0)
	    state->iolist.reds_left = 0;
    }
    state->buf = buf;
    state->offset = offset;
    return 0;

L_bcopy_yield:

    state->buf = buf;
    state->offset = offset;
#ifdef DEBUG
    state->len = len;
#endif

    if (list_to_bitstr_buf_bcopy(state, obj, &yield_count) == 0)
	ERTS_INTERNAL_ERROR("Missing yield");

    BUMP_ALL_REDS(state->iolist.c_p);
    state->iolist.reds_left = 0;
    ESTACK_SAVE(s, &state->iolist.estack);
    return ERTS_IOLIST_TO_BUF_YIELD;

L_yield:

    BUMP_ALL_REDS(state->iolist.c_p);
    state->iolist.reds_left = 0;
    state->iolist.obj = obj;
    state->buf = buf;
    state->offset = offset;
    ESTACK_SAVE(s, &state->iolist.estack);
#ifdef DEBUG
    state->len = len;
#endif
    return ERTS_IOLIST_TO_BUF_YIELD;


#undef LIST_TO_BITSTR_BUF_BCOPY_DBG
#undef LIST_TO_BITSTR_BUF_BCOPY

}

static ErlDrvSizeT
list_to_bitstr_buf_yielding(ErtsIOList2BufState *state)
{
    return list_to_bitstr_buf(1, state);
}

static ErlDrvSizeT
list_to_bitstr_buf_not_yielding(ErtsIOList2BufState *state)
{
    return list_to_bitstr_buf(0, state);
}

static int
list_to_bitstr_buf_bcopy(ErtsIOList2BufState *state, Eterm obj, int *yield_countp)
{
    int res;
    char *buf = state->buf;
    char *next_buf;
    int offset = state->offset;
    int next_offset;
#ifdef DEBUG
    ErlDrvSizeT len = state->len;
    ErlDrvSizeT next_len;
#endif
    byte* bptr;
    size_t size;
    size_t max_size;
    Uint bitoffs;
    Uint num_bits;
    Uint bitsize;
    int yield_count = *yield_countp;

    if (state->bcopy.bptr) {
	bptr = state->bcopy.bptr;
	size = state->bcopy.size;
	bitoffs = state->bcopy.bitoffs;
	bitsize = state->bcopy.bitsize;
	state->bcopy.bptr = NULL;
    }
    else {

	ASSERT(is_binary(obj));

	size = binary_size(obj);

	ASSERT(size <= len);

	ERTS_GET_BINARY_BYTES(obj, bptr, bitoffs, bitsize);
    }

    max_size = (size_t) ERTS_IOLIST_TO_BUF_BYTES_PER_YIELD_COUNT;
    if (yield_count > 0)
	max_size *= (size_t) (yield_count+1);

    if (size <= max_size) {
	if (size >= ERTS_IOLIST_TO_BUF_BYTES_PER_YIELD_COUNT) {
	    int cost = (int) size;
	    cost /= ERTS_IOLIST_TO_BUF_BYTES_PER_YIELD_COUNT;
	    yield_count -= cost;
	}
	next_offset = offset + bitsize;
	next_buf = buf + size+(next_offset>7);
#ifdef DEBUG
	next_len = len - size+(next_offset>7);
#endif
	next_offset &= 7;
	num_bits = 8*size+bitsize;
	res = 0;
    }
    else {
	ASSERT(0 < max_size && max_size < size);
	yield_count = 0;
	state->bcopy.bptr = bptr + max_size;
	state->bcopy.bitoffs = bitoffs;
	state->bcopy.bitsize = bitsize;
	state->bcopy.size = size - max_size;
	next_buf = buf + max_size;
#ifdef DEBUG
	next_len = len - max_size;
#endif
	next_offset = offset;
	num_bits = 8*max_size;
	size = max_size;
	res = 1;
    }

    copy_binary_to_buffer(buf, offset, bptr, bitoffs, num_bits);

    state->offset = next_offset;
    state->buf = next_buf;
#ifdef DEBUG
    state->len = next_len;
#endif
    *yield_countp = yield_count;

    return res;
}

static int
bitstr_list_len(ErtsIOListState *state)
{
    Eterm* objp;
    Eterm obj;
    Uint len, offs;
    int res, init_yield_count, yield_count;
    DECLARE_ESTACK(s);

    if (state->reds_left <= 0)
	return ERTS_IOLIST_YIELD;

    len = (Uint) state->size;
    offs = state->offs;
    obj = state->obj;

    ESTACK_CHANGE_ALLOCATOR(s, ERTS_ALC_T_SAVED_ESTACK);
    init_yield_count = ERTS_IOLIST_SIZE_YIELDS_COUNT_PER_RED;
    init_yield_count *= state->reds_left;
    yield_count = init_yield_count;
    if (state->estack.start) {
	/* Restart; restore estack... */
	ESTACK_RESTORE(s, &state->estack);
    }

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
	    while (1) { /* Tail loop */
		while (1) { /* Head loop */
		    if (--yield_count <= 0)
			goto L_yield;
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
			continue; /* Head loop */
		    } else if (is_not_nil(obj)) {
			goto L_type_error;
		    }
		    break;
		}
		/* Tail */
		obj = CDR(objp);
		if (is_list(obj))
		    continue; /* Tail loop */
		else if (is_binary(obj)) {
		    SAFE_ADD(len, binary_size(obj));
		    SAFE_ADD_BITSIZE(offs, obj);
		} else if (is_not_nil(obj)) {
		    goto L_type_error;
		}
		break;
	    }
	} else {
	    if (--yield_count <= 0)
		goto L_yield;
	    if (is_binary(obj)) {
		SAFE_ADD(len, binary_size(obj));
		SAFE_ADD_BITSIZE(offs, obj);
	    } else if (is_not_nil(obj)) {
		goto L_type_error;
	    }
	}
    }
#undef SAFE_ADD
#undef SAFE_ADD_BITSIZE

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
    state->size = len;

    res = ERTS_IOLIST_OK;

 L_return: {
	int yc = init_yield_count - yield_count;
	int reds;

	DESTROY_ESTACK(s);
	CLEAR_SAVED_ESTACK(&state->estack);

	reds = (yc - 1)/ERTS_IOLIST_SIZE_YIELDS_COUNT_PER_RED + 1;
	BUMP_REDS(state->c_p, reds);
	state->reds_left -= reds;
	state->size = (ErlDrvSizeT) len;
	state->have_size = 1;
	return res;
    }

 L_overflow_error:
    res = ERTS_IOLIST_OVERFLOW;
    len = 0;
    goto L_return;

 L_type_error:
    res = ERTS_IOLIST_TYPE;
    len = 0;
    goto L_return;

 L_yield:
    BUMP_ALL_REDS(state->c_p);
    state->reds_left = 0;
    state->size = len;
    state->offs = offs;
    state->obj = obj;
    ESTACK_SAVE(s, &state->estack);
    return ERTS_IOLIST_YIELD;
}

