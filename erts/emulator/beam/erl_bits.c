/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1999-2016. All Rights Reserved.
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
#include "bif.h"
#include "big.h"
#include "erl_bits.h"
#include "erl_binary.h"

#ifdef MAX
#undef MAX
#endif
#define MAX(x,y) (((x)>(y))?(x):(y))
#ifdef MIN
#undef MIN
#endif
#define MIN(x,y) (((x)<(y))?(x):(y))

#if defined(WORDS_BIGENDIAN)
# define BIT_ENDIAN_MACHINE 0
#else
# define BIT_ENDIAN_MACHINE BSF_LITTLE
#endif

#define BIT_IS_MACHINE_ENDIAN(x) (((x)&BSF_LITTLE) == BIT_ENDIAN_MACHINE)

/*
 * MAKE_MASK(n) constructs a mask with n bits.
 * Example: MAKE_MASK(3) returns the binary number 00000111.
 */

#define MAKE_MASK(n) ((((Uint) 1) << (n))-1)

/*
 * MASK_BITS assign src to dst, but preserves the dst bits outside the mask.
 */

#define MASK_BITS(src,dst,mask) (((src) & (mask)) | ((dst) & ~(mask)))

static byte get_bit(byte b, size_t a_offs); 

#if defined(ERTS_SMP)
/* the state resides in the current process' scheduler data */
#elif defined(ERL_BITS_REENTRANT)
/* reentrant API but with a hidden single global state, for testing only */
struct erl_bits_state ErlBitsState_;
#else
/* non-reentrant API with a single global state */
struct erl_bits_state ErlBitsState;
#endif

#define byte_buf	(ErlBitsState.byte_buf_)
#define byte_buf_len	(ErlBitsState.byte_buf_len_)

static erts_smp_atomic_t bits_bufs_size;

Uint
erts_bits_bufs_size(void)
{
    return (Uint) erts_smp_atomic_read_nob(&bits_bufs_size);
}

#if !defined(ERTS_SMP)
static
#endif
void
erts_bits_init_state(ERL_BITS_PROTO_0)
{
    byte_buf_len = 1;
    byte_buf = erts_alloc(ERTS_ALC_T_BITS_BUF, byte_buf_len);

    erts_bin_offset = 0;
}

#if defined(ERTS_SMP)
void
erts_bits_destroy_state(ERL_BITS_PROTO_0)
{
    erts_free(ERTS_ALC_T_BITS_BUF, byte_buf);
}
#endif

void
erts_init_bits(void)
{
    ERTS_CT_ASSERT(offsetof(Binary,orig_bytes) % 8 == 0);
    ERTS_CT_ASSERT(offsetof(ErtsMagicBinary,u.aligned.data) % 8 == 0);
    ERTS_CT_ASSERT(offsetof(ErtsBinary,driver.binary.orig_bytes)
                == offsetof(Binary,orig_bytes));

    erts_smp_atomic_init_nob(&bits_bufs_size, 0);
#if defined(ERTS_SMP)
    /* erl_process.c calls erts_bits_init_state() on all state instances */
#else
    ERL_BITS_DECLARE_STATEP;
    erts_bits_init_state(ERL_BITS_ARGS_0);
#endif
}

/*****************************************************************
 ***
 *** New matching binaries functions
 ***
 *****************************************************************/

#define ReadToVariable(v64, Buffer, x)		\
  do{						\
    int _i;					\
    v64 = 0;					\
    for(_i = 0; _i < x; _i++) {			\
      v64 = ((Uint)Buffer[_i] <<(8*_i)) + v64;	\
	}					\
  }while(0)					\

Eterm
erts_bs_start_match_2(Process *p, Eterm Binary, Uint Max)
{
    Eterm Orig;
    Uint offs;
    Uint* hp;
    Uint NeededSize;
    ErlBinMatchState *ms;
    Uint bitoffs;
    Uint bitsize;
    Uint total_bin_size;
    ProcBin* pb;

    ASSERT(is_binary(Binary));
    total_bin_size = binary_size(Binary);
    if ((total_bin_size >> (8*sizeof(Uint)-3)) != 0) {
	return THE_NON_VALUE;
    }
    NeededSize = ERL_BIN_MATCHSTATE_SIZE(Max);
    hp = HeapOnlyAlloc(p, NeededSize);
    ms = (ErlBinMatchState *) hp;
    ERTS_GET_REAL_BIN(Binary, Orig, offs, bitoffs, bitsize);
    pb = (ProcBin *) boxed_val(Orig);
    if (pb->thing_word == HEADER_PROC_BIN && pb->flags != 0) {
	erts_emasculate_writable_binary(pb);
    }
    ms->thing_word = HEADER_BIN_MATCHSTATE(Max);
    (ms->mb).orig = Orig;
    (ms->mb).base = binary_bytes(Orig);
    (ms->mb).offset = ms->save_offset[0] = 8 * offs + bitoffs;
    (ms->mb).size = total_bin_size * 8 + (ms->mb).offset + bitsize;
    return make_matchstate(ms);
}

#ifdef DEBUG
# define CHECK_MATCH_BUFFER(MB) check_match_buffer(MB)

static void check_match_buffer(ErlBinMatchBuffer* mb)
{
    Eterm realbin;
    Uint byteoffs;
    byte* bytes, bitoffs, bitsz;
    ProcBin* pb;
    ERTS_GET_REAL_BIN(mb->orig, realbin, byteoffs, bitoffs, bitsz);
    bytes = binary_bytes(realbin) + byteoffs;
    ERTS_ASSERT(mb->base >= bytes && mb->base <= (bytes + binary_size(mb->orig)));
    pb = (ProcBin *) boxed_val(realbin);
    if (pb->thing_word == HEADER_PROC_BIN)
        ERTS_ASSERT(pb->flags == 0);
}
#else
# define CHECK_MATCH_BUFFER(MB)
#endif

Eterm
erts_bs_get_integer_2(Process *p, Uint num_bits, unsigned flags, ErlBinMatchBuffer* mb)
{
    Uint bytes;
    Uint bits;
    Uint offs;
    byte bigbuf[64];
    byte* LSB;
    byte* MSB;
    Uint* hp;
    Uint words_needed;
    Uint actual;
    Uint v32;
    int sgn = 0;
    Eterm res = THE_NON_VALUE;
	
    if (num_bits == 0) {
	return SMALL_ZERO;
    }
    
    CHECK_MATCH_BUFFER(mb);
    if (mb->size - mb->offset < num_bits) {	/* Asked for too many bits.  */
	return THE_NON_VALUE;
    }

    /*
     * Special cases for field sizes up to the size of Uint.
     */

    if (num_bits <= 8-(offs = BIT_OFFSET(mb->offset))) {
	/*
	 * All bits are in one byte in the binary. We only need
	 * shift them right and mask them.
	 */
	Uint b = mb->base[BYTE_OFFSET(mb->offset)];
	Uint mask = MAKE_MASK(num_bits);
	mb->offset += num_bits;
	b >>= 8 - offs - num_bits;
	b &= mask;
	if ((flags & BSF_SIGNED) && b >> (num_bits-1)) {
	    b |= ~mask;
	}
	return make_small(b);
    } else if (num_bits <= 8) {
	/*
	 * The bits are in two different bytes. It is easiest to
	 * combine the bytes to a word first, and then shift right and
	 * mask to extract the bits.
	 */
	Uint byte_offset = BYTE_OFFSET(mb->offset);
	Uint w = mb->base[byte_offset] << 8 | mb->base[byte_offset+1];
	Uint mask = MAKE_MASK(num_bits);
	mb->offset += num_bits;
	w >>= 16 - offs - num_bits;
	w &= mask;
	if ((flags & BSF_SIGNED) && w >> (num_bits-1)) {
	    w |= ~mask;
	}
	return make_small(w);
    } else if (num_bits < SMALL_BITS && (flags & BSF_LITTLE) == 0) {
	/*
	 * Handle field sizes from 9 up to SMALL_BITS-1 bits, big-endian,
	 * stored in at least two bytes.
	 */
	byte* bp = mb->base + BYTE_OFFSET(mb->offset);
	Uint n;
	Uint w;

	n = num_bits;
	mb->offset += num_bits;

	/*
	 * Handle the most signicant byte if it contains 1 to 7 bits.
	 * It only needs to be masked, not shifted.
	 */
	if (offs == 0) {
	    w = 0;
	} else {
	    Uint num_bits_in_msb = 8 - offs;
	    w = *bp++;
	    n -= num_bits_in_msb;
	    w &= MAKE_MASK(num_bits_in_msb);
	}

	/*
	 * Simply shift whole bytes into the result.
	 */
	switch (BYTE_OFFSET(n)) {
#if defined(ARCH_64)
	case 7: w = (w << 8) | *bp++;
	case 6: w = (w << 8) | *bp++;
	case 5: w = (w << 8) | *bp++;
	case 4: w = (w << 8) | *bp++;
#endif
	case 3: w = (w << 8) | *bp++;
	case 2: w = (w << 8) | *bp++;
	case 1: w = (w << 8) | *bp++;
	}
	n = BIT_OFFSET(n);

	/*
	 * Handle the 1 to 7 bits remaining in the last byte (if any).
	 * They need to be shifted right, but there is no need to mask;
	 * then they can be shifted into the word.
	 */
	if (n > 0) {
	    Uint b = *bp;
	    b >>= 8 - n;
	    w = (w << n) | b;
	}

	/*
	 * Sign extend the result if the field type is 'signed' and the
	 * most significant bit is 1.
	 */
	if ((flags & BSF_SIGNED) != 0 && (w >> (num_bits-1) != 0)) {
	    w |= ~MAKE_MASK(num_bits);
	}
	return make_small(w);
    }

    /*
     * Handle everything else, that is:
     *
     * Big-endian fields >= SMALL_BITS (potentially bignums).
     * Little-endian fields with 9 or more bits.
     */

    bytes = NBYTES(num_bits);
    if ((bits = BIT_OFFSET(num_bits)) == 0) {  /* number of bits in MSB */
	bits = 8;
    }
    offs = 8 - bits;                  /* adjusted offset in MSB */

    if (bytes <= sizeof bigbuf) {
	LSB = bigbuf;
    } else {
	LSB = erts_alloc(ERTS_ALC_T_TMP, bytes);
    }
    MSB = LSB + bytes - 1;

    /*
     * Move bits to temporary buffer. We want the buffer to be stored in
     * little-endian order, since bignums are little-endian.
     */
    
    if (flags & BSF_LITTLE) {
	erts_copy_bits(mb->base, mb->offset, 1, LSB, 0, 1, num_bits);
	*MSB >>= offs;		/* adjust msb */
    } else {
	*MSB = 0;
	erts_copy_bits(mb->base, mb->offset, 1, MSB, offs, -1, num_bits);
    }
    mb->offset += num_bits;

    /*
     * Get the sign bit.
     */
    sgn = 0;
    if ((flags & BSF_SIGNED) && (*MSB & (1<<(bits-1)))) {
	byte* ptr = LSB; 
	byte c = 1;

	/* sign extend MSB */
	*MSB |= ~MAKE_MASK(bits);

	/* two's complement */
	while (ptr <= MSB) {
	    byte pd = ~(*ptr);
	    byte d = pd + c;
	    c = (d < pd);
	    *ptr++ = d;
	}
	sgn = 1;
    }

    /* normalize */
    while ((*MSB == 0) && (MSB > LSB)) {
	MSB--;
	bytes--;
    }

    /* check for guaranteed small num */
    switch (bytes) {
    case 1:
	v32 = LSB[0];
	goto big_small;
    case 2:
	v32 = LSB[0] + (LSB[1]<<8); 
	goto big_small; 
    case 3: 
	v32 = LSB[0] + (LSB[1]<<8) + (LSB[2]<<16); 
	goto big_small;
#if !defined(ARCH_64)
    case 4:
	v32 = (LSB[0] + (LSB[1]<<8) + (LSB[2]<<16) + (LSB[3]<<24));
	if (!IS_USMALL(sgn, v32)) {
	  goto make_big;
	}
#else
    case 4:
      ReadToVariable(v32, LSB, 4);					
      goto big_small;
    case 5:	
      ReadToVariable(v32, LSB, 5);					
      goto big_small;
    case 6:	
      ReadToVariable(v32, LSB, 6);
      goto big_small; 
    case 7:
      ReadToVariable(v32, LSB, 7);
      goto big_small; 
    case 8:
      ReadToVariable(v32, LSB, 8);
      if (!IS_USMALL(sgn, v32)) {
	goto make_big;   
	}
#endif   
    big_small:			/* v32 loaded with value which fits in fixnum */
	if (sgn) {
	    res = make_small(-((Sint)v32));
	} else {
	    res = make_small(v32);
	}
	break;
    make_big:
	hp = HeapOnlyAlloc(p, BIG_UINT_HEAP_SIZE);
	if (sgn) {
	  hp[0] = make_neg_bignum_header(1);
	} else {
	  hp[0] = make_pos_bignum_header(1);
	}
	BIG_DIGIT(hp,0) = v32;
	res = make_big(hp);
	break;
    default:
	words_needed = 1+WSIZE(bytes);
	hp = HeapOnlyAlloc(p, words_needed);
	res = bytes_to_big(LSB, bytes, sgn, hp); 
	if (is_nil(res)) {
	    p->htop = hp;
	    res = THE_NON_VALUE;
	} else if (is_small(res)) {
	    p->htop = hp;
	} else if ((actual = bignum_header_arity(*hp)+1) < words_needed) {
	    p->htop = hp + actual;
	}
	break;
    }

    if (LSB != bigbuf) {
	erts_free(ERTS_ALC_T_TMP, (void *) LSB);
    }
    return res;
}

Eterm
erts_bs_get_binary_2(Process *p, Uint num_bits, unsigned flags, ErlBinMatchBuffer* mb)
{
    ErlSubBin* sb;

    CHECK_MATCH_BUFFER(mb);
    if (mb->size - mb->offset < num_bits) {	/* Asked for too many bits.  */
	return THE_NON_VALUE;
    }

    /*
     * From now on, we can't fail.
     */

    sb = (ErlSubBin *) HeapOnlyAlloc(p, ERL_SUB_BIN_SIZE);
    
    sb->thing_word = HEADER_SUB_BIN;
    sb->orig = mb->orig;
    sb->size = BYTE_OFFSET(num_bits);
    sb->bitsize = BIT_OFFSET(num_bits);
    sb->offs = BYTE_OFFSET(mb->offset);
    sb->bitoffs = BIT_OFFSET(mb->offset);
    sb->is_writable = 0;
    mb->offset += num_bits;
    
    return make_binary(sb);
}

Eterm
erts_bs_get_float_2(Process *p, Uint num_bits, unsigned flags, ErlBinMatchBuffer* mb)
{
    Eterm* hp;
    float f32;
    double f64;
    byte* fptr;
    FloatDef f;

    CHECK_MATCH_BUFFER(mb);
    if (num_bits == 0) {
	f.fd = 0.0;
	hp = HeapOnlyAlloc(p, FLOAT_SIZE_OBJECT);
	PUT_DOUBLE(f, hp);
	return make_float(hp);
    }
    if (mb->size - mb->offset < num_bits) {	/* Asked for too many bits.  */
	return THE_NON_VALUE;
    }
    if (num_bits == 32) {
	fptr = (byte *) &f32;
    } else if (num_bits == 64) {
	fptr = (byte *) &f64;
    } else {
	return THE_NON_VALUE;
    }

    if (BIT_IS_MACHINE_ENDIAN(flags)) {
	erts_copy_bits(mb->base, mb->offset, 1,
		  fptr, 0, 1,
		  num_bits);
    } else {
	erts_copy_bits(mb->base, mb->offset, 1,
		  fptr + NBYTES(num_bits) - 1, 0, -1,
		  num_bits);
    }
    ERTS_FP_CHECK_INIT(p);
    if (num_bits == 32) {
	ERTS_FP_ERROR_THOROUGH(p, f32, return THE_NON_VALUE);
	f.fd = f32;
    } else {
#ifdef DOUBLE_MIDDLE_ENDIAN
	FloatDef ftmp;
	ftmp.fd = f64;
	f.fw[0] = ftmp.fw[1];
	f.fw[1] = ftmp.fw[0];
	ERTS_FP_ERROR_THOROUGH(p, f.fd, return THE_NON_VALUE);
#else
	ERTS_FP_ERROR_THOROUGH(p, f64, return THE_NON_VALUE);
	f.fd = f64;
#endif
    }
    mb->offset += num_bits;
    hp = HeapOnlyAlloc(p, FLOAT_SIZE_OBJECT);
    PUT_DOUBLE(f, hp);
    return make_float(hp);
}

Eterm
erts_bs_get_binary_all_2(Process *p, ErlBinMatchBuffer* mb)
{
    ErlSubBin* sb;
    Uint size;

    CHECK_MATCH_BUFFER(mb);
    size =  mb->size-mb->offset;
    sb = (ErlSubBin *) HeapOnlyAlloc(p, ERL_SUB_BIN_SIZE);
    sb->thing_word = HEADER_SUB_BIN;
    sb->size = BYTE_OFFSET(size);
    sb->bitsize = BIT_OFFSET(size);
    sb->offs = BYTE_OFFSET(mb->offset);
    sb->bitoffs = BIT_OFFSET(mb->offset);
    sb->is_writable = 0;
    sb->orig = mb->orig;
    mb->offset = mb->size;  
    return make_binary(sb);
}

/****************************************************************
 ***
 *** Building binaries
 ***
 ****************************************************************/


/* COPY_VAL:
 * copy sz byte from val to dst buffer, 
 * dst, val are updated!!!
 */

#define COPY_VAL(dst,ddir,val,sz) do { \
   Uint __sz = (sz); \
   while(__sz) { \
     switch(__sz) { \
     default: \
     case 4: *dst = (val&0xff); dst += ddir; val >>= 8; __sz--; \
     case 3: *dst = (val&0xff); dst += ddir; val >>= 8; __sz--; \
     case 2: *dst = (val&0xff); dst += ddir; val >>= 8; __sz--; \
     case 1: *dst = (val&0xff); dst += ddir; val >>= 8; __sz--; \
     } \
   } \
 } while(0)

/* calculate a - *cp (carry)  (store result in b), *cp is updated! */
#define SUBc(a, cp, b) do { \
   byte __x = (a); \
   byte __y = (__x - (*(cp))); \
   (*cp) = (__y > __x); \
   *(b) = ~__y; \
 } while(0)
  
static int
fmt_int(byte *buf, Uint sz, Eterm val, Uint size, Uint flags)
{
    unsigned long offs;

    offs = BIT_OFFSET(size);
    if (is_small(val)) {
	Sint v = signed_val(val);

	ASSERT(size != 0);	  /* Tested by caller */
	if (flags & BSF_LITTLE) { /* Little endian */
	    sz--;
	    COPY_VAL(buf,1,v,sz);
	    *buf = offs ? ((v << (8-offs)) & 0xff) : (v & 0xff);
	} else {		/* Big endian */
	    buf += (sz - 1);
	    if (offs) {
		*buf-- = (v << (8-offs)) & 0xff;
		sz--;
		v >>= offs;
	    }
	    COPY_VAL(buf,-1,v,sz);
	}
    } else if (is_big(val)) {
	int sign   = big_sign(val);
	Uint ds  = big_size(val)*sizeof(ErtsDigit);  /* number of digits bytes */
	ErtsDigit* dp = big_v(val);
	int n = MIN(sz,ds);

	if (size == 0) {
	    return 0;
	}
	if (flags & BSF_LITTLE) {
	    sz -= n;                       /* pad with this amount */
	    if (sign) {
		int c = 1;
		while(n >= sizeof(ErtsDigit)) {
		    ErtsDigit d = *dp++;
		    int i;
		    for(i = 0; i < sizeof(ErtsDigit); ++i) {
			SUBc((d&0xff), &c, buf);
			buf++;
			d >>= 8;
		    }
		    n -= sizeof(ErtsDigit);
		}
		if (n) {
		    ErtsDigit d = *dp;
		    do {
			SUBc((d&0xff), &c, buf);
			buf++;
			d >>= 8;
		    } while (--n > 0);
		}
		/* pad */
		while(sz--) {
		    SUBc(0, &c, buf);
		    buf++;
		}
	    }
	    else {
		while(n >= sizeof(ErtsDigit)) {
		    ErtsDigit d = *dp++;
		    int i;
		    for(i = 0; i < sizeof(ErtsDigit); ++i) {
			*buf++ = (d & 0xff);
			d >>= 8;
		    }
		    n -= sizeof(ErtsDigit);
		}
		if (n) {
		    ErtsDigit d = *dp;
		    do {
			*buf++ = (d & 0xff);
			d >>= 8;
		    } while (--n > 0);
		}
		/* pad */
		while(sz) {
		    *buf++ = 0;
		    sz--;
		}
	    }
	    /* adjust MSB!!! */
	    if (offs) {
		buf--;
		*buf <<= (8 - offs);
	    }
	}
	else {   /* BIG ENDIAN */
	    ErtsDigit acc = 0;
	    ErtsDigit d;

	    buf += (sz - 1);              /* end of buffer */
	    sz -= n;                      /* pad with this amount */
	    offs = offs ? (8-offs) : 0;   /* shift offset */

	    if (sign) { /* SIGNED */
		int c = 1;

		while (n >= sizeof(ErtsDigit)) {
		    int i;

		    d = *dp++;
		    acc |= d << offs;
		    SUBc((acc&0xff), &c, buf);
		    buf--;
		    acc = d >> (8-offs);
		    for (i = 0; i < sizeof(ErtsDigit)-1; ++i) {
			SUBc((acc&0xff), &c, buf);
			buf--;
			acc >>= 8;
		    }
		    n -= sizeof(ErtsDigit);
		}
		if (n) {
		    acc |= ((ErtsDigit)*dp << offs);
		    do {
			SUBc((acc & 0xff), &c, buf);
			buf--;
			acc >>= 8;
		    } while (--n > 0);
		}
		/* pad */
		while(sz--) {
		    SUBc((acc & 0xff), &c, buf);
		    buf--;
		    acc >>= 8;
		}
	    }
	    else { /* UNSIGNED */
		while (n >= sizeof(ErtsDigit)) {
		    int i;

		    d = *dp++;
		    acc |= d << offs;
		    *buf-- = acc;
		    acc = d >> (8-offs);
		    for (i = 0; i < sizeof(ErtsDigit)-1; ++i) {
			*buf-- = acc;
			acc >>= 8;
		    }
		    n -= sizeof(ErtsDigit);
		}
		if (n) {
		    acc |= ((ErtsDigit)*dp << offs);
		    do {
			*buf-- = acc & 0xff;
			acc >>= 8;
		    } while (--n > 0);
		}
		while (sz--) {
		    *buf-- = acc & 0xff;
		    acc >>= 8;
		}
	    }
	}
    } else {			/* Neither small nor big */
	return -1;
    }
    return 0;
}

static void
ERTS_INLINE need_byte_buf(ERL_BITS_PROTO_1(int need))
{
    if (byte_buf_len < need) {
	erts_smp_atomic_add_nob(&bits_bufs_size, need - byte_buf_len);
	byte_buf_len = need;
	byte_buf = erts_realloc(ERTS_ALC_T_BITS_BUF, byte_buf, byte_buf_len);
    }
}

int
erts_new_bs_put_integer(ERL_BITS_PROTO_3(Eterm arg, Uint num_bits, unsigned flags))
{
    Uint bin_offset = erts_bin_offset;
    Uint bit_offset;
    Uint b;
    byte *iptr;

    bit_offset = BIT_OFFSET(bin_offset);
    if (is_small(arg)) {
	Uint rbits = 8 - bit_offset;

	if (num_bits == 0) {
	    return 1;
	} else if (bit_offset + num_bits <= 8) {
	    /*
	     * All bits are in the same byte.
	     */ 
	    iptr = erts_current_bin+BYTE_OFFSET(bin_offset);
	    b = *iptr & (0xff << rbits);
	    b |= (signed_val(arg) & ((1 << num_bits)-1)) << (8-bit_offset-num_bits);
	    *iptr = b;
	} else if (bit_offset == 0) {
	    /*
	     * More than one bit, starting at a byte boundary.
	     * That will be quite efficiently handled by fmt_int().
	     *
	     * (We know that fmt_int() can't fail here.)
	     */
	    (void) fmt_int(erts_current_bin+BYTE_OFFSET(bin_offset),
			   NBYTES(num_bits), arg, num_bits, flags);
	} else if (flags & BSF_LITTLE) {
	    /*
	     * Can't handle unaligned little-endian in a simple way.
	     */
	    goto unaligned;
	} else {		/* Big endian */
	    /*
	     * Big-endian, more than one byte, but not aligned on a byte boundary.
	     * Handle the bits up to the next byte boundary specially,
	     * then let fmt_int() handle the rest.
	     */
	    Uint shift_count = num_bits - rbits;
	    Sint val = signed_val(arg);
	    iptr = erts_current_bin+BYTE_OFFSET(bin_offset);
	    b = *iptr & (0xff << rbits);

	    /*
	     * Shifting with a shift count greater than or equal to the word
	     * size may be a no-op (instead of 0 the result may be the unshifted
	     * value). Therefore, only do the shift and the OR if the shift count
	     * is less than the word size if the number is positive; if negative,
	     * we must simulate the sign extension.
	     */
	    if (shift_count < sizeof(Uint)*8) {
		b |= (val >> shift_count) & ((1 << rbits) - 1);
	    } else if (val < 0) {
		/* Simulate sign extension. */
		b |= (-1) & ((1 << rbits) - 1);
	    }
	    *iptr++ = b;

	    /* fmt_int() can't fail here. */
	    (void) fmt_int(iptr, NBYTES(num_bits-rbits), arg,
			   num_bits-rbits, flags);
	}
    } else if (bit_offset == 0) {
	/*
	 * Big number, aligned on a byte boundary. We can format the
	 * integer directly into the binary.
	 */
	if (fmt_int(erts_current_bin+BYTE_OFFSET(bin_offset),
		    NBYTES(num_bits), arg, num_bits, flags) < 0) {
	    return 0;
	}
    } else {
    unaligned:
	/*
	 * Big number or small little-endian number, not byte-aligned,
	 * or not a number at all.
	 *
	 * We must format the number into a temporary buffer, and then
	 * copy that into the binary.
	 */
	need_byte_buf(ERL_BITS_ARGS_1(NBYTES(num_bits)));
	iptr = byte_buf;
	if (fmt_int(iptr, NBYTES(num_bits), arg, num_bits, flags) < 0) {
	    return 0;
	}
	erts_copy_bits(iptr, 0, 1, erts_current_bin, bin_offset, 1, num_bits);
    }
    erts_bin_offset = bin_offset + num_bits;
    return 1;
}

int
erts_bs_put_utf8(ERL_BITS_PROTO_1(Eterm arg))
{
    Uint bin_offset = erts_bin_offset;
    Uint bit_offset;
    Uint num_bits;
    byte tmp_buf[4];
    byte* dst;
    Sint val;

    if (is_not_small(arg)) {
	return 0;
    }
    val = signed_val(arg);
    if (val < 0) {
	return 0;
    }

    if ((bit_offset = BIT_OFFSET(bin_offset)) == 0) {
	/* We can write directly into the destination binary. */
	dst = erts_current_bin+BYTE_OFFSET(bin_offset);
    } else {
	/* Unaligned destination binary. Must use a temporary buffer. */
	dst = tmp_buf;
    }
    if (val < 0x80) {
	dst[0] = val;
	num_bits = 8;
    } else if (val < 0x800) {
	dst[0] = 0xC0 | (val >> 6);
	dst[1] = 0x80 | (val & 0x3F);
	num_bits = 16;
    } else if (val < 0x10000UL) {
	if (0xD800 <= val && val <= 0xDFFF) {
	    return 0;
	}
	dst[0] = 0xE0 | (val >> 12);
	dst[1] = 0x80 | ((val >> 6) & 0x3F);
	dst[2] = 0x80 | (val & 0x3F);
	num_bits = 24;
    } else if (val < 0x110000) {
	dst[0] = 0xF0 | (val >> 18);
	dst[1] = 0x80 | ((val >> 12) & 0x3F);
	dst[2] = 0x80 | ((val >> 6) & 0x3F);
	dst[3] = 0x80 | (val & 0x3F);
	num_bits = 32;
    } else {
	return 0;
    }

    if (bin_offset != 0) {
	erts_copy_bits(dst, 0, 1, erts_current_bin, bin_offset, 1, num_bits);
    }

    erts_bin_offset += num_bits;

    return 1;
}

int
erts_bs_put_utf16(ERL_BITS_PROTO_2(Eterm arg, Uint flags))
{
    Uint bin_offset = erts_bin_offset;
    Uint bit_offset;
    Uint num_bits;
    byte tmp_buf[4];
    byte* dst;
    Uint val;

    if (is_not_small(arg)) {
	return 0;
    }
    val = unsigned_val(arg);
    if (val > 0x10FFFF || (0xD800 <= val && val <= 0xDFFF)) {
	return 0;
    }

    if ((bit_offset = BIT_OFFSET(bin_offset)) == 0) {
	/* We can write directly into the destination binary. */
	dst = erts_current_bin+BYTE_OFFSET(bin_offset);
    } else {
	/* Unaligned destination binary. Must use a temporary buffer. */
	dst = tmp_buf;
    }

    if (val < 0x10000UL) {
	num_bits = 16;
	if (flags & BSF_LITTLE) {
	    dst[0] = val;
	    dst[1] = val >> 8;
	} else {
	    dst[0] = val >> 8;
	    dst[1] = val;
	}
    } else {
	Uint16 w1, w2;

	num_bits = 32;
	val = val - 0x10000UL;
	w1 = 0xD800 | (val >> 10);
	w2 = 0xDC00 | (val & 0x3FF);
	if (flags & BSF_LITTLE) {
	    dst[0] = w1;
	    dst[1] = w1 >> 8;
	    dst[2] = w2;
	    dst[3] = w2 >> 8;
	} else {
	    dst[0] = w1 >> 8;
	    dst[1] = w1;
	    dst[2] = w2 >> 8;
	    dst[3] = w2;
	}
    }

    if (bin_offset != 0) {
	erts_copy_bits(dst, 0, 1, erts_current_bin, bin_offset, 1, num_bits);
    }

    erts_bin_offset += num_bits;
    return 1;
}
     

int
erts_new_bs_put_binary(ERL_BITS_PROTO_2(Eterm arg, Uint num_bits))
{
    byte *bptr;
    Uint bitoffs;
    Uint bitsize; 

    if (!is_binary(arg)) {
	return 0;
    }
    ERTS_GET_BINARY_BYTES(arg, bptr, bitoffs, bitsize);
    if (num_bits > 8*binary_size(arg)+bitsize) {
	return 0;
    }
    copy_binary_to_buffer(erts_current_bin, erts_bin_offset, bptr, bitoffs, num_bits);
    erts_bin_offset += num_bits;
    return 1;
}

int
erts_new_bs_put_binary_all(ERL_BITS_PROTO_2(Eterm arg, Uint unit))
{
   byte *bptr;
   Uint bitoffs;
   Uint bitsize;
   Uint num_bits;

   /*
    * This type test is not needed if the code was compiled with
    * an R12B or later compiler, since there would have been a
    * call to bit_size/1 or byte_size/1 that would have failed if
    * 'arg' was not a binary. However, in R11B and earlier releases,
    * size/1 was use for calculating the size of the binary, and
    * therefore 'arg' could be a tuple.
    */
   if (!is_binary(arg)) {
       return 0;
   }

   ERTS_GET_BINARY_BYTES(arg, bptr, bitoffs, bitsize);
   num_bits = 8*binary_size(arg)+bitsize;
   if (unit == 8) {
       if (bitsize != 0) {
	   return 0;
       }
   } else if (unit != 1 && num_bits % unit != 0) {
       return 0;
   }
   copy_binary_to_buffer(erts_current_bin, erts_bin_offset, bptr, bitoffs, num_bits);
   erts_bin_offset += num_bits;
   return 1;
}

int
erts_new_bs_put_float(Process *c_p, Eterm arg, Uint num_bits, int flags)
{
    ERL_BITS_DEFINE_STATEP(c_p);

    if (BIT_OFFSET(erts_bin_offset) == 0) {
	Uint32 a;
	Uint32 b;
	
	if (num_bits == 64) {
	    union {
		double f64;
		Uint32 i32[2];
	    } u;

	    if (is_float(arg)) {
		FloatDef *fdp = (FloatDef*)(float_val(arg) + 1);
#ifdef DOUBLE_MIDDLE_ENDIAN
		a = fdp->fw[1];
		b = fdp->fw[0];
#else
		a = fdp->fw[0];
		b = fdp->fw[1];
#endif
	    } else if (is_small(arg)) {
		u.f64 = (double) signed_val(arg);
#ifdef DOUBLE_MIDDLE_ENDIAN
		a = u.i32[1];
		b = u.i32[0];
#else
		a = u.i32[0];
		b = u.i32[1];
#endif
	    } else if (is_big(arg)) {
		if (big_to_double(arg, &u.f64) < 0) {
		    return 0;
		}
#ifdef DOUBLE_MIDDLE_ENDIAN
		a = u.i32[1];
		b = u.i32[0];
#else
		a = u.i32[0];
		b = u.i32[1];
#endif
	    } else {
		return 0;
	    }
	} else if (num_bits == 32) {
	    union {
		float f32;
		Uint32 i32;
	    } u;

	    b = 0;
	    if (is_float(arg)) {
		FloatDef f;
		GET_DOUBLE(arg, f);
		ERTS_FP_CHECK_INIT(c_p);
		u.f32 = f.fd;
		ERTS_FP_ERROR(c_p,u.f32,;);
		a = u.i32;
	    } else if (is_small(arg)) {
		u.f32 = (float) signed_val(arg);
		a = u.i32;
	    } else if (is_big(arg)) {
		double f64;
		if (big_to_double(arg, &f64) < 0) {
		    return 0;
		}
		ERTS_FP_CHECK_INIT(c_p);
		u.f32 = (float) f64;
		ERTS_FP_ERROR(c_p,u.f32,;);
		a = u.i32;
	    } else {
		return 0;
	    }
	} else {
	    return 0;
	}

	if (BIT_IS_MACHINE_ENDIAN(flags)) {
	    byte* t = erts_current_bin+BYTE_OFFSET(erts_bin_offset);
#ifdef WORDS_BIGENDIAN
	    t[0] = a >> 24;
	    t[1] = a >> 16;
	    t[2] = a >> 8;
	    t[3] = a;
	    if (num_bits == 64) {
		t[4] = b >> 24;
		t[5] = b >> 16;
		t[6] = b >> 8;
		t[7] = b;
	    }
#else
	    t[3] = a >> 24;
	    t[2] = a >> 16;
	    t[1] = a >> 8;
	    t[0] = a;
	    if (num_bits == 64) {
		t[7] = b >> 24;
		t[6] = b >> 16;
		t[5] = b >> 8;
		t[4] = b;
	    }
#endif
	} else {
	    byte* t = erts_current_bin+BYTE_OFFSET(erts_bin_offset) + NBYTES(num_bits);
#ifdef WORDS_BIGENDIAN
	    t[-1] = a >> 24;
	    t[-2] = a >> 16;
	    t[-3] = a >> 8;
	    t[-4] = a;
	    if (num_bits == 64) {
		t[-5] = b >> 24;
		t[-6] = b >> 16;
		t[-7] = b >> 8;
		t[-8] = b;
	    }
#else
	    t[-1] = a;
	    t[-2] = a >> 8;
	    t[-3] = a >> 16;
	    t[-4] = a >> 24;
	    if (num_bits == 64) {
		t[-5] = b;
		t[-6] = b >> 8;
		t[-7] = b >> 16;
		t[-8] = b >> 24;
	    }
#endif
	}
    } else {
	byte *bptr;
	double f64;
	float f32;
#ifdef DOUBLE_MIDDLE_ENDIAN
	FloatDef fbuf, ftmp;
#endif
	
	if (num_bits == 64) {
	    if (is_float(arg)) {
#ifdef DOUBLE_MIDDLE_ENDIAN
		FloatDef *fdp = (FloatDef*)(float_val(arg) + 1);
		ftmp = *fdp;
#else
		bptr = (byte *) (float_val(arg) + 1);
#endif
	    } else if (is_small(arg)) {
		f64 = (double) signed_val(arg);
#ifdef DOUBLE_MIDDLE_ENDIAN
		ftmp.fd = f64;
#else
		bptr = (byte *) &f64;
#endif
	    } else if (is_big(arg)) {
		if (big_to_double(arg, &f64) < 0) {
		    return 0;
		}
#ifdef DOUBLE_MIDDLE_ENDIAN
		ftmp.fd = f64;
#else
		bptr = (byte *) &f64;
#endif
	    } else {
		return 0;
	    }
#ifdef DOUBLE_MIDDLE_ENDIAN
	    fbuf.fw[0] = ftmp.fw[1];
	    fbuf.fw[1] = ftmp.fw[0];
	    bptr = fbuf.fb;
#endif
	} else if (num_bits == 32) {
	    if (is_float(arg)) {
		FloatDef f;
		GET_DOUBLE(arg, f);
		ERTS_FP_CHECK_INIT(c_p);
		f32 = f.fd;
		ERTS_FP_ERROR(c_p,f32,;);
		bptr = (byte *) &f32;
	    } else if (is_small(arg)) {
		f32 = (float) signed_val(arg);
		bptr = (byte *) &f32;
	    } else if (is_big(arg)) {
		if (big_to_double(arg, &f64) < 0) {
		    return 0;
		}
		ERTS_FP_CHECK_INIT(c_p);
		f32 = (float) f64;
		ERTS_FP_ERROR(c_p,f32,;);
		bptr = (byte *) &f32;
	    } else {
		return 0;
	    }
	} else {
	    return 0;
	}
	if (BIT_IS_MACHINE_ENDIAN(flags)) {
	    erts_copy_bits(bptr, 0, 1,
		      erts_current_bin,
		      erts_bin_offset, 1, num_bits);
	} else {
	    erts_copy_bits(bptr+NBYTES(num_bits)-1, 0, -1,
			   erts_current_bin, erts_bin_offset, 1,
			   num_bits);
	}
    }
    erts_bin_offset += num_bits;
    return 1;
}

void 
erts_new_bs_put_string(ERL_BITS_PROTO_2(byte* iptr, Uint num_bytes))
{
    if (BIT_OFFSET(erts_bin_offset) != 0) {
	erts_copy_bits(iptr, 0, 1, erts_current_bin, erts_bin_offset, 1, num_bytes*8);
    } else {
	sys_memcpy(erts_current_bin+BYTE_OFFSET(erts_bin_offset), iptr, num_bytes);
    }
    erts_bin_offset += num_bytes*8;
}

Eterm
erts_bs_append(Process* c_p, Eterm* reg, Uint live, Eterm build_size_term,
	    Uint extra_words, Uint unit)
{
    Eterm bin;			/* Given binary */
    Eterm* ptr;
    Eterm hdr;
    ErlSubBin* sb;
    ProcBin* pb;
    Binary* binp;
    Uint heap_need;
    Uint build_size_in_bits;
    Uint used_size_in_bits;
    Uint unsigned_bits;
    ERL_BITS_DEFINE_STATEP(c_p);

    /*
     * Check and untag the requested build size.
     */
    if (is_small(build_size_term)) {
	Sint signed_bits = signed_val(build_size_term);
	if (signed_bits < 0) {
	    goto badarg;
	}
	build_size_in_bits = (Uint) signed_bits;
    } else if (term_to_Uint(build_size_term, &unsigned_bits)) {
	build_size_in_bits = unsigned_bits;
    } else {
	c_p->freason = unsigned_bits;
	return THE_NON_VALUE;
    }

    /*
     * Check the binary argument.
     */
    bin = reg[live];
    if (!is_boxed(bin)) {
    badarg:
	c_p->freason = BADARG;
	return THE_NON_VALUE;
    }
    ptr = boxed_val(bin);
    hdr = *ptr;
    if (!is_binary_header(hdr)) {
	goto badarg;
    }
    if (hdr != HEADER_SUB_BIN) {
	goto not_writable;
    }
    sb = (ErlSubBin *) ptr;
    if (!sb->is_writable) {
	goto not_writable;
    }
    pb = (ProcBin *) boxed_val(sb->orig);
    ASSERT(pb->thing_word == HEADER_PROC_BIN);
    if ((pb->flags & PB_IS_WRITABLE) == 0) {
	goto not_writable;
    }

    /*
     * OK, the binary is writable.
     */

    erts_bin_offset = 8*sb->size + sb->bitsize;
    if (unit > 1) {
	if ((unit == 8 && (erts_bin_offset & 7) != 0) ||
	    (erts_bin_offset % unit) != 0) {
	    goto badarg;
	}
    }
    used_size_in_bits = erts_bin_offset + build_size_in_bits;
    sb->is_writable = 0;	/* Make sure that no one else can write. */
    pb->size = NBYTES(used_size_in_bits);
    pb->flags |= PB_ACTIVE_WRITER;

    /*
     * Reallocate the binary if it is too small.
     */
    binp = pb->val;
    if (binp->orig_size < pb->size) {
	Uint new_size = 2*pb->size;
	binp = erts_bin_realloc(binp, new_size);
	pb->val = binp;
	pb->bytes = (byte *) binp->orig_bytes;
    }
    erts_current_bin = pb->bytes;

    /*
     * Allocate heap space and build a new sub binary.
     */
    reg[live] = sb->orig;
    heap_need = ERL_SUB_BIN_SIZE + extra_words;
    if (c_p->stop - c_p->htop < heap_need) {
	(void) erts_garbage_collect(c_p, heap_need, reg, live+1);
    }
    sb = (ErlSubBin *) c_p->htop;
    c_p->htop += ERL_SUB_BIN_SIZE;
    sb->thing_word = HEADER_SUB_BIN;
    sb->size = BYTE_OFFSET(used_size_in_bits);
    sb->bitsize = BIT_OFFSET(used_size_in_bits);
    sb->offs = 0;
    sb->bitoffs = 0;
    sb->is_writable = 1;
    sb->orig = reg[live];

    return make_binary(sb);

    /*
     * The binary is not writable. We must create a new writable binary and
     * copy the old contents of the binary.
     */
 not_writable:
    {
	Uint used_size_in_bytes; /* Size of old binary + data to be built */
	Uint bin_size;
	Binary* bptr;
	byte* src_bytes;
	Uint bitoffs;
	Uint bitsize;
	Eterm* hp;

	/*
	 * Allocate heap space.
	 */
	heap_need = PROC_BIN_SIZE + ERL_SUB_BIN_SIZE + extra_words;
	if (c_p->stop - c_p->htop < heap_need) {
	    (void) erts_garbage_collect(c_p, heap_need, reg, live+1);
	    bin = reg[live];
	}
	hp = c_p->htop;

	/*
	 * Calculate sizes. The size of the new binary, is the sum of the
	 * build size and the size of the old binary. Allow some room
	 * for growing.
	 */
	ERTS_GET_BINARY_BYTES(bin, src_bytes, bitoffs, bitsize);
	erts_bin_offset = 8*binary_size(bin) + bitsize;
	if (unit > 1) {
	    if ((unit == 8 && (erts_bin_offset & 7) != 0) ||
		(erts_bin_offset % unit) != 0) {
		goto badarg;
	    }
	}
	used_size_in_bits = erts_bin_offset + build_size_in_bits;
	used_size_in_bytes = NBYTES(used_size_in_bits);
	bin_size = 2*used_size_in_bytes;
	bin_size = (bin_size < 256) ? 256 : bin_size;

	/*
	 * Allocate the binary data struct itself.
	 */
	bptr = erts_bin_nrml_alloc(bin_size);
	erts_current_bin = (byte *) bptr->orig_bytes;

	/*
	 * Now allocate the ProcBin on the heap.
	 */
	pb = (ProcBin *) hp;
	hp += PROC_BIN_SIZE;
	pb->thing_word = HEADER_PROC_BIN;
	pb->size = used_size_in_bytes;
	pb->next = MSO(c_p).first;
	MSO(c_p).first = (struct erl_off_heap_header*)pb;
	pb->val = bptr;
	pb->bytes = (byte*) bptr->orig_bytes;
	pb->flags = PB_IS_WRITABLE | PB_ACTIVE_WRITER;
	OH_OVERHEAD(&(MSO(c_p)), pb->size / sizeof(Eterm));

	/*
	 * Now allocate the sub binary and set its size to include the
	 * data about to be built.
	 */
	sb = (ErlSubBin *) hp;
	hp += ERL_SUB_BIN_SIZE;
	sb->thing_word = HEADER_SUB_BIN;
	sb->size = BYTE_OFFSET(used_size_in_bits);
	sb->bitsize = BIT_OFFSET(used_size_in_bits);
	sb->offs = 0;
	sb->bitoffs = 0;
	sb->is_writable = 1;
	sb->orig = make_binary(pb);

	c_p->htop = hp;
	
	/*
	 * Now copy the data into the binary.
	 */
	copy_binary_to_buffer(erts_current_bin, 0, src_bytes, bitoffs, erts_bin_offset);

	return make_binary(sb);
    }
}

Eterm
erts_bs_private_append(Process* p, Eterm bin, Eterm build_size_term, Uint unit)
{
    Eterm* ptr;
    ErlSubBin* sb;
    ProcBin* pb;
    Binary* binp;
    Uint build_size_in_bits;
    Uint pos_in_bits_after_build;
    Uint unsigned_bits;
    ERL_BITS_DEFINE_STATEP(p);

    /*
     * Check and untag the requested build size.
     */
    if (is_small(build_size_term)) {
	Sint signed_bits = signed_val(build_size_term);
	if (signed_bits < 0) {
	    p->freason = BADARG;
	    return THE_NON_VALUE;
	}
	build_size_in_bits = (Uint) signed_bits;
    } else if (term_to_Uint(build_size_term, &unsigned_bits)) {
	build_size_in_bits = unsigned_bits;
    } else {
	p->freason = unsigned_bits;
	return THE_NON_VALUE;
    }

    ptr = boxed_val(bin);
    ASSERT(*ptr == HEADER_SUB_BIN);

    sb = (ErlSubBin *) ptr;
    ASSERT(sb->is_writable);

    pb = (ProcBin *) boxed_val(sb->orig);
    ASSERT(pb->thing_word == HEADER_PROC_BIN);

    /*
     * Calculate new size in bytes.
     */
    erts_bin_offset = 8*sb->size + sb->bitsize;
    pos_in_bits_after_build = erts_bin_offset + build_size_in_bits;
    pb->size = (pos_in_bits_after_build+7) >> 3;
    pb->flags |= PB_ACTIVE_WRITER;

    /*
     * Reallocate the binary if it is too small.
     */
    binp = pb->val;
    if (binp->orig_size < pb->size) {
	Uint new_size = 2*pb->size;

	if (pb->flags & PB_IS_WRITABLE) {
	    /*
	     * This is the normal case - the binary is writable.
	     * There are no other references to the binary, so it
	     * is safe to reallocate it.
	     */
	    binp = erts_bin_realloc(binp, new_size);
	    pb->val = binp;
	    pb->bytes = (byte *) binp->orig_bytes;
	} else {
	    /*
	     * The binary is NOT writable. The only way that is
	     * supposed to happen if is call trace has been turned
	     * on. That means that a trace process now has (or have
	     * had) a reference to the binary, so we are not allowed
	     * to reallocate the binary. Instead, we must allocate a new
	     * binary and copy the contents of the old binary into it.
	     */
	    Binary* bptr = erts_bin_nrml_alloc(new_size);
	    sys_memcpy(bptr->orig_bytes, binp->orig_bytes, binp->orig_size);
	    pb->flags |= PB_IS_WRITABLE | PB_ACTIVE_WRITER;
	    pb->val = bptr;
	    pb->bytes = (byte *) bptr->orig_bytes;
            erts_bin_release(binp);
	}
    }
    erts_current_bin = pb->bytes;

    sb->size = pos_in_bits_after_build >> 3;
    sb->bitsize = pos_in_bits_after_build & 7;
    return bin;
}

Eterm
erts_bs_init_writable(Process* p, Eterm sz)
{
    Uint bin_size = 1024;
    Uint heap_need;
    Binary* bptr;
    ProcBin* pb;
    ErlSubBin* sb;
    Eterm* hp;
    
    if (is_small(sz)) {
	Sint s = signed_val(sz);
	if (s >= 0) {
	    bin_size = (Uint) s;
	}
    }

    /*
     * Allocate heap space.
     */
    heap_need = PROC_BIN_SIZE + ERL_SUB_BIN_SIZE;
    if (p->stop - p->htop < heap_need) {
	(void) erts_garbage_collect(p, heap_need, NULL, 0);
    }
    hp = p->htop;
    
    /*
     * Allocate the binary data struct itself.
     */
    bptr = erts_bin_nrml_alloc(bin_size);
    
    /*
     * Now allocate the ProcBin on the heap.
     */
    pb = (ProcBin *) hp;
    hp += PROC_BIN_SIZE;
    pb->thing_word = HEADER_PROC_BIN;
    pb->size = 0;
    pb->next = MSO(p).first;
    MSO(p).first = (struct erl_off_heap_header*) pb;
    pb->val = bptr;
    pb->bytes = (byte*) bptr->orig_bytes;
    pb->flags = PB_IS_WRITABLE | PB_ACTIVE_WRITER;
    OH_OVERHEAD(&(MSO(p)), pb->size / sizeof(Eterm));
    
    /*
     * Now allocate the sub binary.
     */
    sb = (ErlSubBin *) hp;
    hp += ERL_SUB_BIN_SIZE;
    sb->thing_word = HEADER_SUB_BIN;
    sb->size = 0;
    sb->offs = 0;
    sb->bitsize = 0;
    sb->bitoffs = 0;
    sb->is_writable = 1;
    sb->orig = make_binary(pb);

    p->htop = hp;
    return make_binary(sb);
}

void
erts_emasculate_writable_binary(ProcBin* pb)
{
    Binary* binp;
    Uint unused;

    pb->flags = 0;
    binp = pb->val;
    ASSERT(binp->orig_size >= pb->size);
    unused = binp->orig_size - pb->size;
    /* Our allocators are 8 byte aligned, i.e., shrinking with
       less than 8 bytes will have no real effect */
    if (unused >= 8) {
	binp = erts_bin_realloc(binp, pb->size);
	pb->val = binp;
	pb->bytes = (byte *) binp->orig_bytes;
    }
}

Uint32
erts_bs_get_unaligned_uint32(ErlBinMatchBuffer* mb)
{
    Uint bytes;
    Uint offs;
    byte bigbuf[4];
    byte* LSB;
    byte* MSB;
	
    CHECK_MATCH_BUFFER(mb);
    ASSERT((mb->offset & 7) != 0);
    ASSERT(mb->size - mb->offset >= 32);

    bytes = 4;
    offs = 0;

    LSB = bigbuf;
    MSB = LSB + bytes - 1;

    *MSB = 0;
    erts_copy_bits(mb->base, mb->offset, 1, MSB, offs, -1, 32);
    return LSB[0] | (LSB[1]<<8) | (LSB[2]<<16) | (LSB[3]<<24);
}

void
erts_align_utf8_bytes(ErlBinMatchBuffer* mb, byte* buf)
{
    Uint bits = mb->size - mb->offset;

    /*
     * Copy up to 4 bytes into the supplied buffer.
     */

    ASSERT(bits >= 8);
    if (bits <= 15) {
	bits = 8;
    } else if (bits >= 32) {
	bits = 32;
    } else if (bits >= 24) {
	bits = 24;
    } else {
	bits = 16;
    }
    erts_copy_bits(mb->base, mb->offset, 1, buf, 0, 1, bits);
}

Eterm
erts_bs_get_utf8(ErlBinMatchBuffer* mb)
{
    Eterm result;
    Uint remaining_bits;
    byte* pos;
    byte tmp_buf[4];
    Eterm a, b, c;

    /*
     * Number of trailing bytes for each value of the first byte.
     */
    static const byte erts_trailing_bytes_for_utf8[256] = {
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9, 9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,
	9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9, 9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,
	9,9,1,1,1,1,1,1,1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2, 3,3,3,3,3,3,3,3,9,9,9,9,9,9,9,9
    };

    CHECK_MATCH_BUFFER(mb);

    if ((remaining_bits = mb->size - mb->offset) < 8) {
	return THE_NON_VALUE;
    }
    if (BIT_OFFSET(mb->offset) == 0) {
	pos = mb->base + BYTE_OFFSET(mb->offset);
    } else {
	erts_align_utf8_bytes(mb, tmp_buf);
	pos = tmp_buf;
    }
    result = pos[0];
    switch (erts_trailing_bytes_for_utf8[result]) {
      case 0:
	/* One byte only */
	mb->offset += 8;
	break;
      case 1:
	/* Two bytes */
	if (remaining_bits < 16) {
	    return THE_NON_VALUE;
	}
	a = pos[1];
	if ((a & 0xC0) != 0x80) {
	    return THE_NON_VALUE;
	}
	result = (result << 6) + a - (Eterm) 0x00003080UL;
	mb->offset += 16;
	break;
      case 2:
	/* Three bytes */
	if (remaining_bits < 24) {
	    return THE_NON_VALUE;
	}
	a = pos[1];
	b = pos[2];
	if ((a & 0xC0) != 0x80 || (b & 0xC0) != 0x80 ||
	    (result == 0xE0 && a < 0xA0)) {
	    return THE_NON_VALUE;
	}
	result = (((result << 6) + a) << 6) + b - (Eterm) 0x000E2080UL;
	if (0xD800 <= result && result <= 0xDFFF) {
	    return THE_NON_VALUE;
	}
	mb->offset += 24;
	break;
      case 3:
	/* Four bytes */
	if (remaining_bits < 32) {
	    return THE_NON_VALUE;
	}
	a = pos[1];
	b = pos[2];
	c = pos[3];
	if ((a & 0xC0) != 0x80 || (b & 0xC0) != 0x80 ||
	    (c & 0xC0) != 0x80 ||
	    (result == 0xF0 && a < 0x90)) {
	    return THE_NON_VALUE;
	}
	result = (((((result << 6) + a) << 6) + b) << 6) +
	    c - (Eterm) 0x03C82080UL;
	if (result > 0x10FFFF) {
	    return THE_NON_VALUE;
	}
	mb->offset += 32;
	break;
      default:
	return THE_NON_VALUE;
    }
    return make_small(result);
}

Eterm
erts_bs_get_utf16(ErlBinMatchBuffer* mb, Uint flags)
{
    Uint bit_offset;
    Uint num_bits = mb->size - mb->offset;
    byte* src;
    byte tmp_buf[4];
    Uint16 w1;
    Uint16 w2;

    if (num_bits < 16) {
	return THE_NON_VALUE;
    }

    CHECK_MATCH_BUFFER(mb);
    /*
     * Set up the pointer to the source bytes.
     */
    if ((bit_offset = BIT_OFFSET(mb->offset)) == 0) {
	/* We can access the binary directly because the bytes are aligned. */
	src = mb->base + BYTE_OFFSET(mb->offset);
    } else {
	/*
	 * We must copy the data to a temporary buffer. If possible,
	 * get 4 bytes, otherwise two bytes.
	 */
	Uint n = num_bits < 32 ? 16 : 32;
	erts_copy_bits(mb->base, mb->offset, 1, tmp_buf, 0, 1, n);
	src = tmp_buf;
    }
    
    /*
     * Get the first (and maybe only) 16-bit word. See if we are done.
     */
    if (flags & BSF_LITTLE) {
	w1 = src[0] | (src[1] << 8);
    } else {
	w1 = (src[0] << 8) | src[1];
    }
    if (w1 < 0xD800 || w1 > 0xDFFF) {
	mb->offset += 16;
	return make_small(w1);
    } else if (w1 > 0xDBFF) {
	return THE_NON_VALUE;
    }

    /*
     * Get the second 16-bit word and combine it with the first.
     */
    if (num_bits < 32) {
	return THE_NON_VALUE;
    } else if (flags & BSF_LITTLE) {
	w2 = src[2] | (src[3] << 8);
    } else {
	w2 = (src[2] << 8) | src[3];
    }
    if (!(0xDC00 <= w2 && w2 <= 0xDFFF)) {
	return THE_NON_VALUE;
    }
    mb->offset += 32;
    return make_small((((w1 & 0x3FF) << 10) | (w2 & 0x3FF)) + 0x10000UL);
}

static byte
get_bit(byte b, size_t offs) 
{
    return (b >> (7-offs)) & 1;
}

int
erts_cmp_bits(byte* a_ptr, size_t a_offs, byte* b_ptr, size_t b_offs, size_t size) 
{
    byte a;
    byte b;
    byte a_bit;
    byte b_bit;
    Uint lshift;
    Uint rshift;
    int cmp;
    
    ASSERT(a_offs < 8 && b_offs < 8);

    if (size == 0)
        return 0;

    if (((a_offs | b_offs | size) & 7) == 0) {
	int byte_size = size >> 3;
	return sys_memcmp(a_ptr, b_ptr, byte_size);
    }

    /* Compare bit by bit until a_ptr is aligned on byte boundary */
    a = *a_ptr++;
    b = *b_ptr++;
    if (a_offs) {
	for (;;) {
	    a_bit = get_bit(a, a_offs);
	    b_bit = get_bit(b, b_offs);
	    if ((cmp = (a_bit-b_bit)) != 0) {
		return cmp;
	    }
	    if (--size == 0)
		return 0;

	    b_offs++;
	    if (b_offs == 8) {
		b_offs = 0;
		b = *b_ptr++;
	    }
	    a_offs++;
	    if (a_offs == 8) {
		a_offs = 0;
		a = *a_ptr++;
		break;
	    }
	}
    }

    /* Compare byte by byte as long as at least 8 bits remain */
    if (size >= 8) {
        lshift = b_offs;
        rshift = 8 - lshift;
        for (;;) {
            byte b_cmp = (b << lshift);
            b = *b_ptr++;
            b_cmp |= b >> rshift;
            if ((cmp = (a - b_cmp)) != 0) {
                return cmp;
            }
            size -= 8;
	    if (size < 8)
		break;
            a = *a_ptr++;
        }

	if (size == 0)
	    return 0;
	a = *a_ptr++;
    }

    /* Compare the remaining bits bit by bit */
    if (size > 0) {
        for (;;) {
            a_bit = get_bit(a, a_offs);
            b_bit = get_bit(b, b_offs);
            if ((cmp = (a_bit-b_bit)) != 0) {
                return cmp;
            }
            if (--size == 0)
                return 0;

            a_offs++;
	    ASSERT(a_offs < 8);

            b_offs++;
            if (b_offs == 8) {
                b_offs = 0;
                b = *b_ptr++;
            }
        }
    }

    return 0;
}

/*
 * The basic bit copy operation. Copies n bits from the source buffer to
 * the destination buffer. Depending on the directions, it can reverse the
 * copied bits.
 */


void 
erts_copy_bits(byte* src,	/* Base pointer to source. */
	       size_t soffs,	/* Bit offset for source relative to src. */
	       int sdir,	/* Direction: 1 (forward) or -1 (backward). */
	       byte* dst,	/* Base pointer to destination. */
	       size_t doffs,	/* Bit offset for destination relative to dst. */
	       int ddir,	/* Direction: 1 (forward) or -1 (backward). */
	       size_t n)	/* Number of bits to copy. */
{
    Uint lmask;
    Uint rmask;
    Uint count;
    Uint deoffs;

    if (n == 0) {
	return;
    }

    src += sdir*BYTE_OFFSET(soffs);
    dst += ddir*BYTE_OFFSET(doffs);
    soffs = BIT_OFFSET(soffs);
    doffs = BIT_OFFSET(doffs);
    deoffs = BIT_OFFSET(doffs+n);
    lmask = (doffs) ? MAKE_MASK(8-doffs) : 0;
    rmask = (deoffs) ? (MAKE_MASK(deoffs)<<(8-deoffs)) : 0;

    /*
     * Take care of the case that all bits are in the same byte.
     */

    if (doffs+n < 8) {		/* All bits are in the same byte */
	lmask = (lmask & rmask) ? (lmask & rmask) : (lmask | rmask);

	if (soffs == doffs) {
	    *dst = MASK_BITS(*src,*dst,lmask);
	} else if (soffs > doffs) {
	    Uint bits = (*src << (soffs-doffs));
	    if (soffs+n > 8) {
		src += sdir;
		bits |= (*src >> (8-(soffs-doffs)));
	    }
	    *dst = MASK_BITS(bits,*dst,lmask);
	} else {
	    *dst = MASK_BITS((*src >> (doffs-soffs)),*dst,lmask);
	}
	return;			/* We are done! */
    }

    /*
     * At this point, we know that the bits are in 2 or more bytes.
     */

    count = ((lmask) ? (n - (8 - doffs)) : n) >> 3;

    if (soffs == doffs) {
	/*
	 * The bits are aligned in the same way. We can just copy the bytes
	 * (except for the first and last bytes). Note that the directions
	 * might be different, so we can't just use memcpy().
	 */

	if (lmask) {
	    *dst = MASK_BITS(*src, *dst, lmask);
	    dst += ddir;
	    src += sdir;
	}

	while (count--) {
	    *dst = *src;
	    dst += ddir;
	    src += sdir;
	}

	if (rmask) {
	    *dst = MASK_BITS(*src,*dst,rmask);
	}
    } else {
	Uint bits;
	Uint bits1;
	Uint rshift;
	Uint lshift;

	/*
	 * The tricky case. The bits must be shifted into position.
	 */
	
	if (soffs > doffs) {
	    lshift = (soffs - doffs);
	    rshift = 8 - lshift;
	    bits = *src;
	    if (soffs + n > 8) {
		src += sdir;
	    }
	} else {
	    rshift = (doffs - soffs);
	    lshift = 8 - rshift;
	    bits = 0;
	}
	    
	if (lmask) {
	    bits1 = bits << lshift;
	    bits = *src;
	    src += sdir;
	    bits1 |= (bits >> rshift);
	    *dst = MASK_BITS(bits1,*dst,lmask);
	    dst += ddir;
	}

	while (count--) {
	    bits1 = bits << lshift;
	    bits = *src;
	    src += sdir;
	    *dst = bits1 | (bits >> rshift);
	    dst += ddir;
	}
	
	if (rmask) {
	    bits1 = bits << lshift;
	    if ((rmask << rshift) & 0xff) {
		bits = *src;
		bits1 |= (bits >> rshift);
	    }
	    *dst = MASK_BITS(bits1,*dst,rmask);
	}
    }
}

