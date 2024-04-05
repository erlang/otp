/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1999-2024. All Rights Reserved.
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

#if defined(WORDS_BIGENDIAN)
# define BIT_ENDIAN_MACHINE 0
#else
# define BIT_ENDIAN_MACHINE BSF_LITTLE
#endif

#define BIT_IS_MACHINE_ENDIAN(x) (((x)&BSF_LITTLE) == BIT_ENDIAN_MACHINE)

#if (SIZEOF__FLOAT16 == 2)
typedef _Float16 erlfp16;
#define FP16_FROM_FP64(x) ((_Float16) x)
#define FP16_TO_FP64(x) ((double) x)
#else
typedef Uint16 erlfp16;
#define FP16_FROM_FP64(x) (fp16_ieee_from_fp32_value((float) x))
#define FP16_TO_FP64(x) ((double) fp16_ieee_to_fp32_value(x))
#include "erl_bits_f16.h"
#endif

/*
 * Here is how many bits we can copy in each reduction.
 *
 * At the time of writing of this comment, CONTEXT_REDS was 4000 and
 * BITS_PER_REDUCTION was 1 KiB (8192 bits). The time for copying an
 * unaligned 4000 KiB binary on my computer (which has a 4,2 GHz Intel
 * i7 CPU) was about 5 ms. The time was approximately 4 times lower if
 * the source and destinations binaries were aligned.
 */

#define BITS_PER_REDUCTION (8*1024)

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

/* If the proc bin is larger than 16 MB,
   we only increase by 20% instead of doubling */
#define GROW_PROC_BIN_SIZE(size) \
    (((size) > (1ull << 24)) ? 1.2*(size) : 2*(size))

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

ErlSubBits *erts_bs_start_match_3(Process *p, Eterm bin)
{
    ErlSubBits *sb;
    Uint offset, size;
    byte *base;
    Eterm br_flags;
    BinRef *br;
    Uint *hp;

    ASSERT(is_bitstring(bin));

    hp = HeapOnlyAlloc(p, ERL_SUB_BITS_SIZE);
    sb = (ErlSubBits *) hp;

    ERTS_PIN_BITSTRING(bin, br_flags, br, base, offset, size);

    erl_sub_bits_init(sb,
                      ERL_SUB_BITS_FLAGS_MATCH_CONTEXT,
                      br ? ((Eterm)br | br_flags) : bin,
                      base,
                      offset,
                      size);

    return sb;
}

#ifdef DEBUG
# define CHECK_MATCH_BUFFER(MB) check_match_buffer(MB)

static void check_match_buffer(const ErlSubBits *sb)
{
    Eterm *unboxed = boxed_val(sb->orig);
    const byte *match_base = erl_sub_bits_get_base(sb);
    const byte *orig_base;
    Uint size;

    if (*unboxed == HEADER_BIN_REF) {
        const BinRef *br = (BinRef*)unboxed;
        ASSERT(!((br->val)->intern.flags &
                 (BIN_FLAG_WRITABLE | BIN_FLAG_ACTIVE_WRITER)));
        size = NBITS((br->val)->orig_size);
        orig_base = (byte*)(sb->base_flags & ~ERL_SUB_BITS_FLAG_MASK);
    } else {
        const ErlHeapBits *hb = (ErlHeapBits*)unboxed;
        size = hb->size;
        orig_base = (byte*)hb->data;
    }

    ASSERT(sb->end >= sb->start);
    ASSERT(size >= (sb->end - sb->start));
    ASSERT(match_base >= orig_base && match_base <= (orig_base + NBYTES(size)));
    ASSERT(sb->start <= (size - NBITS(match_base - orig_base)));
}
#else
# define CHECK_MATCH_BUFFER(MB)
#endif

Eterm
erts_bs_get_integer_2(
Process *p, Uint num_bits, unsigned flags, ErlSubBits *sb)
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
    
    CHECK_MATCH_BUFFER(sb);
    if (sb->end - sb->start < num_bits) {	/* Asked for too many bits.  */
	return THE_NON_VALUE;
    }

    /*
     * Special cases for field sizes up to the size of Uint.
     */

    if (num_bits <= 8-(offs = BIT_OFFSET(sb->start))) {
	/*
	 * All bits are in one byte in the binary. We only need
	 * shift them right and mask them.
	 */
	Uint b = *(erl_sub_bits_get_base(sb) + BYTE_OFFSET(sb->start));
	Uint mask = MAKE_MASK(num_bits);
	sb->start += num_bits;
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
	Uint byte_offset = BYTE_OFFSET(sb->start);
	const byte* bp = erl_sub_bits_get_base(sb) + byte_offset;
	Uint w = bp[0] << 8 | bp[1];
	Uint mask = MAKE_MASK(num_bits);
	sb->start += num_bits;
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
	const byte* bp = erl_sub_bits_get_base(sb) + BYTE_OFFSET(sb->start);
	Uint n;
	Uint w;

	n = num_bits;
	sb->start += num_bits;

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
	erts_copy_bits(erl_sub_bits_get_base(sb), sb->start, 1,
                       LSB, 0, 1, num_bits);
	*MSB >>= offs;		/* adjust msb */
    } else {
	*MSB = 0;
	erts_copy_bits(erl_sub_bits_get_base(sb), sb->start, 1,
                       MSB, offs, -1, num_bits);
    }
    sb->start += num_bits;

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
erts_bs_get_binary_2(Process *p, Uint num_bits, unsigned flags, ErlSubBits *sb)
{
    Eterm result;

    CHECK_MATCH_BUFFER(sb);
    if (sb->end - sb->start < num_bits) {
        /* Asked for too many bits.  */
        return THE_NON_VALUE;
    }

    /*
     * From now on, we can't fail.
     */

    result = erts_build_sub_bitstring(&HEAP_TOP(p),
                                      sb->orig & TAG_PTR_MASK__,
                                      (BinRef*)boxed_val(sb->orig),
                                      erl_sub_bits_get_base(sb),
                                      sb->start, num_bits);

    sb->start += num_bits;

    return result;
}

Eterm
erts_bs_get_float_2(Process *p, Uint num_bits, unsigned flags, ErlSubBits *sb)
{
    Eterm* hp;
    erlfp16 f16;
    float f32;
    double f64;
    byte* fptr;
    FloatDef f;

    CHECK_MATCH_BUFFER(sb);
    if (num_bits == 0) {
	f.fd = 0.0;
	hp = HeapOnlyAlloc(p, FLOAT_SIZE_OBJECT);
	PUT_DOUBLE(f, hp);
	return make_float(hp);
    }
    if (sb->end - sb->start < num_bits) {	/* Asked for too many bits.  */
	return THE_NON_VALUE;
    }
    if (num_bits == 16) {
	fptr = (byte *) &f16;
    } else if (num_bits == 32) {
	fptr = (byte *) &f32;
    } else if (num_bits == 64) {
	fptr = (byte *) &f64;
    } else {
	return THE_NON_VALUE;
    }

    if (BIT_IS_MACHINE_ENDIAN(flags)) {
	erts_copy_bits(erl_sub_bits_get_base(sb), sb->start, 1,
		  fptr, 0, 1,
		  num_bits);
    } else {
	erts_copy_bits(erl_sub_bits_get_base(sb), sb->start, 1,
		  fptr + NBYTES(num_bits) - 1, 0, -1,
		  num_bits);
    }
    ERTS_FP_CHECK_INIT(p);
    if (num_bits == 16) {
	f.fd = FP16_TO_FP64(f16);
	ERTS_FP_ERROR_THOROUGH(p, f.fd, return THE_NON_VALUE);
    } else if (num_bits == 32) {
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
    sb->start += num_bits;
    hp = HeapOnlyAlloc(p, FLOAT_SIZE_OBJECT);
    PUT_DOUBLE(f, hp);
    return make_float(hp);
}

Eterm
erts_bs_get_binary_all_2(Process *p, ErlSubBits *sb)
{
    Uint bit_size;
    Eterm result;

    CHECK_MATCH_BUFFER(sb);
    bit_size = sb->end - sb->start;

    result = erts_build_sub_bitstring(&HEAP_TOP(p),
                                      sb->orig & TAG_PTR_MASK__,
                                      (BinRef*)boxed_val(sb->orig),
                                      erl_sub_bits_get_base(sb),
                                      sb->start, bit_size);

    sb->start = sb->end;

    return result;
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

static void
fmt_small(byte *buf, Uint num_bytes, Eterm arg, Uint num_bits, Uint flags)
{
    Uint bit_offset;
    Sint val;

    ASSERT(is_small(arg));
    ASSERT(num_bits != 0);      /* Tested by caller */

    bit_offset = BIT_OFFSET(num_bits);
    val = signed_val(arg);

    if (flags & BSF_LITTLE) { /* Little endian */
        num_bytes--;
        COPY_VAL(buf, 1, val, num_bytes);
        *buf = bit_offset ? (val << (8-bit_offset)) : val;
    } else {		/* Big endian */
        buf += num_bytes - 1;
        if (bit_offset) {
            *buf-- = val << (8-bit_offset);
            num_bytes--;
            val >>= bit_offset;
        }
        COPY_VAL(buf, -1, val, num_bytes);
    }
}

/* calculate a - *cp (carry)  (store result in b), *cp is updated! */
#define SUBc(a, cp, b) do { \
   byte __x = (a); \
   byte __y = (__x - (*(cp))); \
   (*cp) = (__y > __x); \
   *(b) = ~__y; \
 } while(0)

static void
fmt_big(byte *buf, Uint num_bytes, Eterm val, Uint num_bits, Uint flags)
{
    unsigned long offs;
    int sign;
    Uint ds;
    ErtsDigit* dp;
    int n;

    ASSERT(is_big(val));

    if (num_bits == 0) {
        return;
    }

    sign = big_sign(val);
    ds = big_size(val)*sizeof(ErtsDigit); /* number of digits bytes */
    dp = big_v(val);
    n = MIN(num_bytes, ds);

    offs = BIT_OFFSET(num_bits);
    if (flags & BSF_LITTLE) {
        num_bytes -= n;         /* pad with this amount */
        if (sign) {             /* negative */
            int c = 1;
            while (n >= sizeof(ErtsDigit)) {
                ErtsDigit d = *dp++;
                int i;
                for (i = 0; i < sizeof(ErtsDigit); i++) {
                    SUBc(d & 0xff, &c, buf);
                    buf++;
                    d >>= 8;
                }
                n -= sizeof(ErtsDigit);
            }
            if (n) {
                ErtsDigit d = *dp;
                do {
                    SUBc(d & 0xff, &c, buf);
                    buf++;
                    d >>= 8;
                } while (--n > 0);
            }
            /* pad */
            while (num_bytes--) {
                SUBc(0, &c, buf);
                buf++;
            }
        } else {                /* positive */
            while (n >= sizeof(ErtsDigit)) {
                ErtsDigit d = *dp++;
                int i;
                for(i = 0; i < sizeof(ErtsDigit); i++) {
                    *buf++ = d;
                    d >>= 8;
                }
                n -= sizeof(ErtsDigit);
            }
            if (n) {
                ErtsDigit d = *dp;
                do {
                    *buf++ = d;
                    d >>= 8;
                } while (--n > 0);
            }
            /* pad */
            while (num_bytes) {
                *buf++ = 0;
                num_bytes--;
            }
        }

        /* adjust MSB */
        if (offs) {
            buf--;
            *buf <<= (8 - offs);
        }
    } else {   /* BIG ENDIAN */
        ErtsDigit acc = 0;
        ErtsDigit d;

        buf += num_bytes - 1;       /* end of buffer */
        num_bytes -= n;             /* pad with this amount */
        offs = offs ? (8-offs) : 0; /* shift offset */

        if (sign) {             /* negative bignum */
            int c = 1;

            while (n >= sizeof(ErtsDigit)) {
                int i;

                d = *dp++;
                acc |= d << offs;
                SUBc(acc & 0xff, &c, buf);
                buf--;
                acc = d >> (8-offs);
                for (i = 0; i < sizeof(ErtsDigit)-1; i++) {
                    SUBc(acc & 0xff, &c, buf);
                    buf--;
                    acc >>= 8;
                }
                n -= sizeof(ErtsDigit);
            }
            if (n) {
                acc |= ((ErtsDigit)*dp << offs);
                do {
                    SUBc(acc & 0xff, &c, buf);
                    buf--;
                    acc >>= 8;
                } while (--n > 0);
            }
            /* pad */
            while (num_bytes--) {
                SUBc(acc & 0xff, &c, buf);
                buf--;
                acc >>= 8;
            }
        } else {                /* positive bignum */
            while (n >= sizeof(ErtsDigit)) {
                int i;

                d = *dp++;
                acc |= d << offs;
                *buf-- = acc;
                acc = d >> (8-offs);
                for (i = 0; i < sizeof(ErtsDigit)-1; i++) {
                    *buf-- = acc;
                    acc >>= 8;
                }
                n -= sizeof(ErtsDigit);
            }
            if (n) {
                acc |= (*dp << offs);
                do {
                    *buf-- = acc;
                    acc >>= 8;
                } while (--n > 0);
            }
            while (num_bytes--) {
                *buf-- = acc;
                acc >>= 8;
            }
        }
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
	    b |= (signed_val(arg) & ((1 << num_bits)-1)) << (rbits-num_bits);
	    *iptr = b;
	} else if (bit_offset == 0) {
	    /*
	     * More than one bit, starting at a byte boundary.
	     */
            iptr = erts_current_bin + BYTE_OFFSET(bin_offset);
            fmt_small(iptr, NBYTES(num_bits), arg, num_bits, flags);
	} else if (flags & BSF_LITTLE) {
            /*
             * Little endian small in more than one byte, not
             * aligned on a byte boundary.
             */
            Sint val = signed_val(arg);
            Uint rshift = bit_offset;
            Uint lshift = rbits;
            Uint lmask = MAKE_MASK(rbits);
            Uint count = (num_bits - rbits) / 8;
            Uint bits, bits1;

            iptr = erts_current_bin+BYTE_OFFSET(bin_offset);

            if (BIT_OFFSET(num_bits) == 0) {
                bits = val;
                bits1 = bits >> rshift;
                *iptr = MASK_BITS(bits1, *iptr, lmask);
                iptr++;
                val >>= 8;

                while (count--) {
                    bits1 = bits << lshift;
                    bits = val & 0xff;
                    *iptr++ = bits1 | (bits >> rshift);
                    val >>= 8;
                }

                *iptr = bits << lshift;
            } else {
                Sint num_bytes = NBYTES(num_bits) - 1;
                Uint deoffs = BIT_OFFSET(bit_offset + num_bits);

                if (num_bytes-- > 0) {
                    bits = val;
                } else {
                    bits = (val << (8 - BIT_OFFSET(num_bits)));
                }
                bits1 = bits >> rshift;
                *iptr = MASK_BITS(bits1, *iptr, lmask);
                iptr++;
                val >>= 8;

                while (count--) {
                    bits1 = bits << lshift;
                    if (num_bytes-- > 0) {
                        bits = val & 0xff;
                    } else {
                        bits = (val << (8 - BIT_OFFSET(num_bits))) & 0xff;
                    }
                    *iptr++ = bits1 | (bits >> rshift);
                    val >>= 8;
                }

                if (deoffs) {
                    bits1 = bits << lshift;
                    if (rshift < deoffs) {
                        bits = (val << (8 - BIT_OFFSET(num_bits))) & 0xff;
                        bits1 |= bits >> rshift;
                    }
                    *iptr = bits1;
                }
            }
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

            fmt_small(iptr, NBYTES(num_bits-rbits), arg, num_bits-rbits, flags);
	}
    } else if (is_big(arg) && bit_offset == 0) {
	/*
	 * Big number, aligned on a byte boundary. We can format the
	 * integer directly into the binary.
	 */
	fmt_big(erts_current_bin+BYTE_OFFSET(bin_offset),
                NBYTES(num_bits), arg, num_bits, flags);
    } else if (is_big(arg) && bit_offset + num_bits <= 8) {
        /*
         * All bits are in the same byte.
         */
        Uint rbits = 8 - bit_offset;
        Sint sign = big_sign(arg);
        ErtsDigit* dp = big_v(arg);
        Uint val = sign ? -*dp : *dp;

        iptr = erts_current_bin+BYTE_OFFSET(bin_offset);
        b = *iptr & (0xff << rbits);
        b |= (val & ((1 << num_bits)-1)) << (rbits-num_bits);
        *iptr = b;
    } else if (is_big(arg)) {
        /*
         * Big number, not aligned on a byte boundary.
         */
        Uint rshift = bit_offset;
        Uint lshift = 8 - bit_offset;
        Uint deoffs = BIT_OFFSET(bit_offset + num_bits);
        Uint lmask = MAKE_MASK(8 - bit_offset);
        Uint rmask = (deoffs) ? (MAKE_MASK(deoffs)<<(8-deoffs)) : 0;
        Uint count = (num_bits - lshift) / 8;
        Uint bits, bits1;

        ASSERT(num_bits - lshift >= 0);

        /*
         * Format the integer byte-aligned using the binary itself as
         * a temporary buffer.
         */
        iptr = erts_current_bin + BYTE_OFFSET(bin_offset);
        b = *iptr;
        fmt_big(iptr, NBYTES(num_bits), arg, num_bits, flags);

        /*
         * Now restore the overwritten bits of the first byte and
         * shift everything to the right.
         */
        bits = *iptr;
        bits1 = bits >> rshift;
        *iptr = MASK_BITS(bits1, b, lmask);
        iptr++;

        while (count--) {
            bits1 = bits << lshift;
            bits = *iptr;
            *iptr++ = bits1 | (bits >> rshift);
        }

        if (rmask) {
            bits1 = bits << lshift;
            if ((rmask << rshift) & 0xff) {
                bits = *iptr;
                bits1 |= (bits >> rshift);
            }
            *iptr = MASK_BITS(bits1, *iptr, rmask);
        }
    } else {
        /* Not an integer. */
        return 0;
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
erts_new_bs_put_binary(Process *c_p, Eterm arg, Uint num_bits)
{
    ERL_BITS_DEFINE_STATEP(c_p);
    Uint offset, size;
    byte *base;

    if (!is_bitstring(arg)) {
        c_p->fvalue = arg;
        return 0;
    }

    ERTS_GET_BITSTRING(arg, base, offset, size);

    if (num_bits > size) {
        c_p->fvalue = arg;
        return 0;
    }

    copy_binary_to_buffer(erts_current_bin, erts_bin_offset,
                          base, offset, num_bits);
    erts_bin_offset += num_bits;

    BUMP_REDS(c_p, num_bits / BITS_PER_REDUCTION);
    return 1;
}

int
erts_new_bs_put_binary_all(Process *c_p, Eterm arg, Uint unit)
{
    ERL_BITS_DEFINE_STATEP(c_p);
    Uint offset, size;
    byte *base;

    /* This instruction is always preceded by a size calculation that 
     * guarantees that 'arg' is a bitstring. */
    ASSERT(is_bitstring(arg));
    ERTS_GET_BITSTRING(arg, base, offset, size);

    if (unit != 1 && (size % unit) != 0) {
        c_p->fvalue = arg;
        return 0;
    }

    copy_binary_to_buffer(erts_current_bin, erts_bin_offset,
                          base, offset, size);
    erts_bin_offset += size;

    BUMP_REDS(c_p, size / BITS_PER_REDUCTION);
    return 1;
}

/*
 * Returns THE_NON_VALUE on success.
 *
 * On failure, returns whichever was wrong of the value or the size,
 * and sets c_p-fvalue to 'type', 'no_float', or 'invalid'.
 */
Eterm
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
                    c_p->fvalue = am_no_float;
		    return arg;
		}
#ifdef DOUBLE_MIDDLE_ENDIAN
		a = u.i32[1];
		b = u.i32[0];
#else
		a = u.i32[0];
		b = u.i32[1];
#endif
	    } else {
                c_p->fvalue = am_type;
		return arg;
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
                    c_p->fvalue = am_no_float;
		    return arg;
		}
		ERTS_FP_CHECK_INIT(c_p);
		u.f32 = (float) f64;
		ERTS_FP_ERROR(c_p,u.f32,;);
		a = u.i32;
	    } else {
                c_p->fvalue = am_type;
		return arg;
	    }
	} else if (num_bits == 16) {
	    union {
		erlfp16 f16;
		Uint16 i16;
	    } u;

	    b = 0;
	    if (is_float(arg)) {
		FloatDef f;
		GET_DOUBLE(arg, f);
		ERTS_FP_CHECK_INIT(c_p);
		ERTS_FP_ERROR(c_p,f.fd,;);
		u.f16 = FP16_FROM_FP64(f.fd);
		a = u.i16;
	    } else if (is_small(arg)) {
		u.f16 = FP16_FROM_FP64(signed_val(arg));
		a = u.i16;
	    } else if (is_big(arg)) {
		double f64;
		if (big_to_double(arg, &f64) < 0) {
                    c_p->fvalue = am_no_float;
		    return arg;
		}
		ERTS_FP_CHECK_INIT(c_p);
		ERTS_FP_ERROR(c_p,f64,;);
		u.f16 = FP16_FROM_FP64(f64);
		a = u.i16;
	    } else {
                c_p->fvalue = am_type;
		return arg;
	    }
	} else {
            c_p->fvalue = am_invalid;
	    return make_small(num_bits);
	}

	if (BIT_IS_MACHINE_ENDIAN(flags)) {
	    byte* t = erts_current_bin+BYTE_OFFSET(erts_bin_offset);
#ifdef WORDS_BIGENDIAN
	    if (num_bits == 16) {
		t[0] = a >> 8;
		t[1] = a;
	    } else if (num_bits >= 32) {
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
	    }
#else
	    if (num_bits >= 32) {
		t[3] = a >> 24;
		t[2] = a >> 16;
	    }
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
	    if (num_bits == 16) {
		t[-1] = a >> 8;
		t[-2] = a;
	    } else if (num_bits >= 32) {
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
	    }
#else
	    t[-1] = a;
	    t[-2] = a >> 8;
	    if (num_bits >= 32) {
	        t[-3] = a >> 16;
	        t[-4] = a >> 24;
	    }
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
	erlfp16 f16;
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
                    c_p->fvalue = am_no_float;
		    return arg;
		}
#ifdef DOUBLE_MIDDLE_ENDIAN
		ftmp.fd = f64;
#else
		bptr = (byte *) &f64;
#endif
	    } else {
                c_p->fvalue = am_type;
		return arg;
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
                    c_p->fvalue = am_no_float;
		    return arg;
		}
		ERTS_FP_CHECK_INIT(c_p);
		f32 = (float) f64;
		ERTS_FP_ERROR(c_p,f32,;);
		bptr = (byte *) &f32;
	    } else {
                c_p->fvalue = am_type;
		return arg;
	    }
	} else if (num_bits == 16) {
	    if (is_float(arg)) {
		FloatDef f;
		GET_DOUBLE(arg, f);
		ERTS_FP_CHECK_INIT(c_p);
		ERTS_FP_ERROR(c_p,f.fd,;);
		f16 = FP16_FROM_FP64(f.fd);
		bptr = (byte *) &f16;
	    } else if (is_small(arg)) {
		f16 = FP16_FROM_FP64(signed_val(arg));
		bptr = (byte *) &f16;
	    } else if (is_big(arg)) {
		if (big_to_double(arg, &f64) < 0) {
                    c_p->fvalue = am_no_float;
		    return arg;
		}
		ERTS_FP_CHECK_INIT(c_p);
		ERTS_FP_ERROR(c_p,f64,;);
		f16 = FP16_FROM_FP64(f64);
		bptr = (byte *) &f16;
	    } else {
                c_p->fvalue = am_type;
		return arg;
	    }
	} else {
            c_p->fvalue = am_invalid;
	    return make_small(num_bits);
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
    return THE_NON_VALUE;
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

static ERTS_INLINE
void update_wb_overhead(Process *p,
                        const BinRef *bin_ref,
                        Uint old_size,
                        Uint new_size)
{
    ASSERT(new_size >= old_size);
    if (new_size > old_size) {
        const Uint incr = (NBYTES(new_size) / sizeof(Eterm) -
                           NBYTES(old_size) / sizeof(Eterm));
        if (ErtsInBetween(bin_ref, OLD_HEAP(p), OLD_HTOP(p))) {
            p->bin_old_vheap += incr;
        } else {
            OH_OVERHEAD(&MSO(p), incr);
        }
    }
}

static void
build_writable_bitstring(Process *p,
                         Eterm **hpp,
                         Binary *bin,
                         Uint current_size,
                         Uint apparent_size,
                         BinRef **brp,
                         ErlSubBits **sbp)
{
    ErlSubBits *sb;
    BinRef *br;

    sb = (ErlSubBits*)&(*hpp)[0];
    br = (BinRef*)&(*hpp)[ERL_SUB_BITS_SIZE];
    *hpp += ERL_SUB_BITS_SIZE + ERL_BIN_REF_SIZE;

    bin->intern.flags |= BIN_FLAG_WRITABLE | BIN_FLAG_ACTIVE_WRITER;
    bin->intern.apparent_size = NBYTES(apparent_size);

    br->thing_word = HEADER_BIN_REF;
    br->next = p->wrt_bins;
    p->wrt_bins = (struct erl_off_heap_header*)br;
    br->val = bin;

    OH_OVERHEAD(&MSO(p), NBYTES(apparent_size) / sizeof(Eterm));

    erl_sub_bits_init(sb,
                      ERL_SUB_BITS_FLAGS_WRITABLE,
                      make_boxed((Eterm*)br),
                      &bin->orig_bytes[0],
                      0,
                      current_size);

    *brp = br;
    *sbp = sb;
}

/*
 * This function either returns a term or THE_NON_VALUE.
 *
 * If THE_NON_VALUE is returned, it can mean one of two things:
 *
 * - The max_heap_size limit for the process was exceeded and the
 *   process was killed. This situation can be recognized by calling
 *   the ERTS_PROC_IS_EXITING(P) macro. The caller must immediately
 *   pass control to the scheduler.
 *
 * - A BADARG or SYSTEM_LIMIT exception happened. The caller must
 *   raise an exception.
 */
Eterm
erts_bs_append(Process* c_p, Eterm* reg, Uint live, Eterm build_size_term,
               Uint extra_words, Uint unit)
{
    Uint unsigned_bits;
    Uint build_size_in_bits;

    /*
     * Check and untag the requested build size.
     */
    if (is_small(build_size_term)) {
	Sint signed_bits = signed_val(build_size_term);
	if (signed_bits < 0) {
            c_p->freason = BADARG;
            return THE_NON_VALUE;
	}
	build_size_in_bits = (Uint) signed_bits;
    } else if (term_to_Uint(build_size_term, &unsigned_bits)) {
	build_size_in_bits = unsigned_bits;
    } else {
	c_p->freason = unsigned_bits;
	return THE_NON_VALUE;
    }
    return erts_bs_append_checked(c_p, reg, live, build_size_in_bits,
                                  extra_words, unit);
}

/*
 * See erts_bs_append().
 */
Eterm
erts_bs_append_checked(Process* c_p, Eterm* reg, Uint live,
                       Uint build_size_in_bits, Uint extra_words,
                       Uint unit)
{
    Eterm bin;			/* Given binary */
    Eterm* ptr;
    Eterm hdr;
    ErlSubBits* sb;
    BinRef* br;
    Binary* binp;
    Uint heap_need;
    Uint used_size_in_bits;
    ERL_BITS_DEFINE_STATEP(c_p);

    /*
     * Check the binary argument.
     */
    bin = reg[live];
    if (!is_boxed(bin)) {
    type_error:
        c_p->fvalue = am_type;

    badarg:
	c_p->freason = BADARG;
	return THE_NON_VALUE;
    }
    ptr = boxed_val(bin);
    hdr = *ptr;
    if (!is_bitstring_header(hdr)) {
	goto type_error;
    }
    if (hdr != HEADER_SUB_BITS) {
	goto not_writable;
    }
    sb = (ErlSubBits*)ptr;
    if (!erl_sub_bits_is_writable(sb)) {
        goto not_writable;
    }

    br = (BinRef *) boxed_val(sb->orig);
    ASSERT(br->thing_word == HEADER_BIN_REF);
    binp = br->val;

    if ((binp->intern.flags & BIN_FLAG_WRITABLE) == 0) {
        goto not_writable;
    }

    /*
     * OK, the binary is writable.
     */
    ASSERT(sb->start == 0);
    erts_bin_offset = sb->end;
    if (unit > 1) {
	if ((unit == 8 && (erts_bin_offset & 7) != 0) ||
	    (unit != 8 && (erts_bin_offset % unit) != 0)) {
            c_p->fvalue = am_unit;
	    goto badarg;
	}
    }

    if (build_size_in_bits == 0) {
        if (HeapWordsLeft(c_p) < extra_words) {
            (void) erts_garbage_collect(c_p, extra_words, reg, live+1);
            if (ERTS_PROC_IS_EXITING(c_p)) {
                return THE_NON_VALUE;
            }
            bin = reg[live];
        }
	return bin;
    }

    if((ERTS_UINT_MAX - build_size_in_bits) < erts_bin_offset) {
        c_p->fvalue = am_size;
        c_p->freason = SYSTEM_LIMIT;
        return THE_NON_VALUE;
    }

    used_size_in_bits = erts_bin_offset + build_size_in_bits;

    /* Make sure that no one else can append to the incoming bitstring. */
    erl_sub_bits_clear_writable(sb);

    update_wb_overhead(c_p, br, sb->end, used_size_in_bits);
    binp->intern.flags |= BIN_FLAG_ACTIVE_WRITER;

    /* Reallocate the underlying binary if it is too small. */
    if (binp->orig_size < NBYTES(used_size_in_bits)) {
        Uint new_size = GROW_PROC_BIN_SIZE(NBYTES(used_size_in_bits));

        binp = erts_bin_realloc(binp, new_size);
        br->val = binp;

        BUMP_REDS(c_p, erts_bin_offset / BITS_PER_REDUCTION);
    }

    binp->intern.apparent_size = NBYTES(used_size_in_bits);
    erts_current_bin = (byte*)binp->orig_bytes;

    /* Allocate heap space and build a new sub binary. */
    reg[live] = sb->orig;

    heap_need = ERL_SUB_BITS_SIZE + extra_words;
    if (HeapWordsLeft(c_p) < heap_need) {
        (void)erts_garbage_collect(c_p, heap_need, reg, live + 1);
        if (ERTS_PROC_IS_EXITING(c_p)) {
            return THE_NON_VALUE;
        }
    }

    sb = (ErlSubBits*)c_p->htop;
    c_p->htop += ERL_SUB_BITS_SIZE;

    erl_sub_bits_init(sb,
                      ERL_SUB_BITS_FLAGS_WRITABLE,
                      reg[live],
                      erts_current_bin,
                      0,
                      used_size_in_bits);

    return make_bitstring(sb);

    /*
     * The binary is not writable. We must create a new writable binary and
     * copy the old contents of the binary.
     */
 not_writable:
    {
        Uint src_offset, src_size;
        byte* src_bytes;
        Uint alloc_size;
        ErlSubBits *sb;
        BinRef *br;

        heap_need = ERL_REFC_BITS_SIZE + extra_words;
        if (HeapWordsLeft(c_p) < heap_need) {
            (void) erts_garbage_collect(c_p, heap_need, reg, live+1);
            if (ERTS_PROC_IS_EXITING(c_p)) {
                return THE_NON_VALUE;
            }
            bin = reg[live];
        }

        /* Calculate sizes. The size of the new binary is the sum of the
         * build size and the size of the old binary. Allow some room
         * for growing. */
        ERTS_GET_BITSTRING(bin, src_bytes, src_offset, src_size);

        if (unit > 1 && (src_size % unit) != 0) {
            c_p->fvalue = am_unit;
            goto badarg;
        }

        if (build_size_in_bits == 0) {
            return bin;
        }

        if((ERTS_UINT_MAX - build_size_in_bits) < src_size) {
            c_p->fvalue = am_size;
            c_p->freason = SYSTEM_LIMIT;
            return THE_NON_VALUE;
        }

        used_size_in_bits = src_size + build_size_in_bits;

        if(used_size_in_bits < (ERTS_UINT_MAX / 2)) {
            alloc_size = GROW_PROC_BIN_SIZE(NBYTES(used_size_in_bits));
        } else {
            alloc_size = NBYTES(ERTS_UINT_MAX);
        }

        ASSERT(HeapWordsLeft(c_p) >= ERL_REFC_BITS_SIZE);
        build_writable_bitstring(c_p,
                                 &c_p->htop,
                                 erts_bin_nrml_alloc(MAX(alloc_size, 256)),
                                 used_size_in_bits,
                                 used_size_in_bits,
                                 &br,
                                 &sb);

        erts_current_bin = (byte*)(br->val)->orig_bytes;
        erts_bin_offset = src_size;

        copy_binary_to_buffer(erts_current_bin,
                              0,
                              src_bytes,
                              src_offset,
                              src_size);
        BUMP_REDS(c_p, src_size / BITS_PER_REDUCTION);

        return make_bitstring(sb);
    }
}

Eterm
erts_bs_private_append(Process* p, Eterm bin, Eterm build_size_term, Uint unit)
{
    Uint unsigned_bits;
    Uint build_size_in_bits;

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
    return erts_bs_private_append_checked(p, bin, build_size_in_bits, unit);
}

Eterm
erts_bs_private_append_checked(Process* p, Eterm bin, Uint build_size_in_bits, Uint unit)
{
    Uint new_position, new_size, used_size;
    Binary *refc_binary;
    ErlSubBits *sb;
    BinRef *br;

    ERL_BITS_DEFINE_STATEP(p);

    sb = (ErlSubBits*)bitstring_val(bin);
    ASSERT(sb->thing_word == HEADER_SUB_BITS);

    br = (BinRef*)boxed_val(sb->orig);
    ASSERT(br->thing_word == HEADER_BIN_REF);

    /* Calculate new size in bits. */
    ASSERT(sb->start == 0);
    erts_bin_offset = sb->end;

    if((ERTS_UINT_MAX - build_size_in_bits) < erts_bin_offset) {
        p->fvalue = am_size;
        p->freason = SYSTEM_LIMIT;
        return THE_NON_VALUE;
    }

    refc_binary = br->val;

    new_position = erts_bin_offset + build_size_in_bits;
    update_wb_overhead(p, br, sb->end, new_position);

    used_size = NBYTES(new_position);
    new_size = GROW_PROC_BIN_SIZE(used_size);

    if (refc_binary->intern.flags & BIN_FLAG_WRITABLE) {
        /* This is the normal case - the binary is writable. There are no other
         * references to the binary, so it is safe to reallocate it when it's
         * too small. */
        ASSERT(erl_sub_bits_is_writable(sb));
        ASSERT(erts_refc_read(&refc_binary->intern.refc, 1) == 1);
        if (refc_binary->orig_size < used_size) {
            refc_binary = erts_bin_realloc(refc_binary, new_size);
            br->val = refc_binary;

            BUMP_REDS(p, erts_bin_offset / BITS_PER_REDUCTION);
        }

        ASSERT(sb->start == 0);
        sb->end = new_position;

        refc_binary->intern.flags |= BIN_FLAG_ACTIVE_WRITER;
        refc_binary->intern.apparent_size = used_size;
    } else {
        /* The binary is NOT writable. The only way that this can happen is
         * when call tracing is turned on, which means that a trace process now
         * has (or have had) a reference to underlying binary. We are therefore
         * unable to reallocate the binary, and must instead allocate a new
         * binary and make a copy of the data.
         *
         * We'll also make a new BinRef as the old one may have been moved from
         * the `wrt_bins` list to the regular `off_heap` list by the GC. To
         * To move it back would mean traversing the `off_heap` list from the
         * start, so we'll create a new BinRef instead for this (hopefully)
         * rare case. */
        Binary *new_binary = erts_bin_nrml_alloc(new_size);
        Eterm *hp = HeapFragOnlyAlloc(p, ERL_REFC_BITS_SIZE);

        build_writable_bitstring(p,
                                 &hp,
                                 new_binary,
                                 new_position,
                                 new_position,
                                 &br,
                                 &sb);

        sys_memcpy(new_binary->orig_bytes,
                   refc_binary->orig_bytes,
                   MIN(refc_binary->orig_size, new_size));

        BUMP_REDS(p, erts_bin_offset / BITS_PER_REDUCTION);
        refc_binary = new_binary;
    }

    ASSERT(refc_binary->intern.flags & BIN_FLAG_WRITABLE);
    erts_current_bin = (byte*)&refc_binary->orig_bytes[0];

    return make_bitstring(sb);
}

Eterm
erts_bs_init_writable(Process* p, Eterm sz)
{
    Uint bin_size = 1024;
    Binary *refc_binary;
    ErlSubBits *sb;
    BinRef *br;

    if (is_small(sz)) {
        Sint s = signed_val(sz);
        if (s >= 0) {
            bin_size = (Uint) s;
        }
    }

    if (HeapWordsLeft(p) < ERL_REFC_BITS_SIZE) {
        (void)erts_garbage_collect(p, ERL_REFC_BITS_SIZE, NULL, 0);
    }

    refc_binary = erts_bin_nrml_alloc(bin_size);
    build_writable_bitstring(p,
                             &p->htop,
                             refc_binary,
                             0,
                             bin_size * 8,
                             &br, &sb);
    (void)br;

    return make_bitstring(sb);
}

void erts_pin_writable_binary(ErlSubBits *sb, BinRef *br) {
    if (erl_sub_bits_was_writable(sb)) {
        enum binary_flags flags;
        Binary *refc_binary;

        refc_binary = br->val;
        flags = refc_binary->intern.flags;

        if (flags & (BIN_FLAG_WRITABLE | BIN_FLAG_ACTIVE_WRITER)) {
            Uint apparent_size = refc_binary->intern.apparent_size;

            ASSERT(refc_binary->orig_size >= apparent_size);
            ASSERT(!(flags & ~(BIN_FLAG_WRITABLE | BIN_FLAG_ACTIVE_WRITER)));
            ASSERT(erts_refc_read(&refc_binary->intern.refc, 1) == 1);

            refc_binary->intern.flags = 0;

            /* Our allocators are 8 byte aligned, i.e., shrinking with less
             * than 8 bytes will have no real effect */
            if (refc_binary->orig_size - apparent_size >= 8) {
                refc_binary = erts_bin_realloc(refc_binary, apparent_size);
                br->val = refc_binary;
            }
        }

        sb->base_flags = (UWord)refc_binary->orig_bytes;
    }

    ASSERT(erl_sub_bits_is_normal(sb));
}

Uint32
erts_bs_get_unaligned_uint32(ErlSubBits* sb)
{
    Uint bytes;
    Uint offs;
    byte bigbuf[4];
    byte* LSB;
    byte* MSB;
	
    CHECK_MATCH_BUFFER(sb);
    ASSERT((sb->start & 7) != 0);
    ASSERT(sb->end - sb->start >= 32);

    bytes = 4;
    offs = 0;

    LSB = bigbuf;
    MSB = LSB + bytes - 1;

    *MSB = 0;
    erts_copy_bits(erl_sub_bits_get_base(sb), sb->start, 1, MSB, offs, -1, 32);
    return LSB[0] | (LSB[1]<<8) | (LSB[2]<<16) | (LSB[3]<<24);
}

static void
erts_align_utf8_bytes(ErlSubBits *sb, byte* buf)
{
    Uint bits = sb->end - sb->start;

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
    erts_copy_bits(erl_sub_bits_get_base(sb), sb->start, 1, buf, 0, 1, bits);
}

Eterm
erts_bs_get_utf8(ErlSubBits *sb)
{
    Eterm result;
    Uint remaining_bits;
    const byte *pos;
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

    CHECK_MATCH_BUFFER(sb);

    if ((remaining_bits = sb->end - sb->start) < 8) {
	return THE_NON_VALUE;
    }
    if (BIT_OFFSET(sb->start) == 0) {
	pos = erl_sub_bits_get_base(sb) + BYTE_OFFSET(sb->start);
    } else {
	erts_align_utf8_bytes(sb, tmp_buf);
	pos = tmp_buf;
    }
    result = pos[0];
    switch (erts_trailing_bytes_for_utf8[result]) {
      case 0:
	/* One byte only */
	sb->start += 8;
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
	sb->start += 16;
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
	sb->start += 24;
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
	sb->start += 32;
	break;
      default:
	return THE_NON_VALUE;
    }
    return make_small(result);
}

Eterm
erts_bs_get_utf16(ErlSubBits *sb, Uint flags)
{
    Uint bit_offset;
    Uint num_bits = sb->end - sb->start;
    byte* src;
    byte tmp_buf[4];
    Uint16 w1;
    Uint16 w2;

    if (num_bits < 16) {
	return THE_NON_VALUE;
    }

    CHECK_MATCH_BUFFER(sb);
    /*
     * Set up the pointer to the source bytes.
     */
    if ((bit_offset = BIT_OFFSET(sb->start)) == 0) {
	/* We can access the binary directly because the bytes are aligned. */
	src = erl_sub_bits_get_base(sb) + BYTE_OFFSET(sb->start);
    } else {
	/*
	 * We must copy the data to a temporary buffer. If possible,
	 * get 4 bytes, otherwise two bytes.
	 */
	Uint n = num_bits < 32 ? 16 : 32;
	erts_copy_bits(erl_sub_bits_get_base(sb), sb->start, 1,
                       tmp_buf, 0, 1, n);
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
	sb->start += 16;
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
    sb->start += 32;
    return make_small((((w1 & 0x3FF) << 10) | (w2 & 0x3FF)) + 0x10000UL);
}

static byte
get_bit(byte b, size_t offs) 
{
    return (b >> (7-offs)) & 1;
}

int erts_cmp_bits__(const byte *a_ptr,
                    Uint a_offs,
                    const byte *b_ptr,
                    Uint b_offs,
                    Uint size)
{
    Uint lshift, rshift;
    byte a_bit, b_bit;
    byte a, b;
    int cmp;

    /* The inlined wrapper should take care of these cases. */
    ASSERT(((a_offs | b_offs | size) & 7) != 0);
    ASSERT(((a_offs | b_offs) & ~7) == 0);
    ASSERT(size > 0);

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
erts_copy_bits(const byte* src, /* Base pointer to source. */
               size_t soffs,    /* Bit offset for source relative to src. */
               int sdir,        /* Direction: 1 (forward) or -1 (backward). */
               byte* dst,       /* Base pointer to destination. */
               size_t doffs,    /* Bit offset for destination relative to dst. */
               int ddir,        /* Direction: 1 (forward) or -1 (backward). */
               size_t n)        /* Number of bits to copy. */
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

Eterm erts_build_sub_bitstring(Eterm **hp,
                               Eterm br_flags,
                               const BinRef *br,
                               const byte *base,
                               Uint offset, Uint size)
{
    ERTS_CT_ASSERT(sizeof(ErlSubBits) <= ERL_ONHEAP_BINARY_LIMIT);

    if (size <= ERL_ONHEAP_BITS_LIMIT) {
        Eterm result = HEAP_BITSTRING(*hp, base, offset, size);
        *hp += heap_bits_size(size);
        return result;
    } else {
        ErlSubBits *sb = (ErlSubBits*)*hp;
        UWord flags = 0;

        ASSERT(br && ((br_flags & _TAG_PRIMARY_MASK) == TAG_PRIMARY_BOXED));

        /* If the underlying binary is writable, we have to mark the result as
         * volatile. We can skip this indirection once the flags move into
         * br_flags. */
        if ((br->val)->intern.flags & BIN_FLAG_WRITABLE) {
            flags |= ERL_SUB_BITS_FLAG_VOLATILE;
        }

        erl_sub_bits_init(sb,
                          flags,
                          ((Eterm)br) | br_flags,
                          base,
                          offset,
                          size);

        *hp += ERL_SUB_BITS_SIZE;
        return make_bitstring(sb);
    }
}

Eterm erts_wrap_refc_bitstring(struct erl_off_heap_header **oh,
                               Uint64 *overhead,
                               Eterm **hpp,
                               Binary *bin,
                               byte *bytes,
                               Uint offset,
                               Uint size)
{
    ErlSubBits *sb = (ErlSubBits*)&(*hpp)[ERL_BIN_REF_SIZE];
    BinRef *br = (BinRef*)*hpp;
    UWord flags = 0;

    ASSERT(bin != NULL);

    br->thing_word = HEADER_BIN_REF;
    br->next = (*oh);
    br->val = bin;

    /* If the underlying binary is writable, we have to mark the result as
     * volatile. */
    if (bin->intern.flags & BIN_FLAG_WRITABLE) {
        flags |= ERL_SUB_BITS_FLAG_VOLATILE;
    }

    erl_sub_bits_init(sb,
                      flags,
                      make_boxed((Eterm*)br),
                      bytes,
                      offset,
                      size);

    /* Note that the overhead must be the actual allocated size of the off-heap
     * `Binary`, not the apparent size of the binary, in order for virtual heap
     * sizes to be accounted correctly. */
    *overhead += bin->orig_size / sizeof(Eterm);

    *oh = (struct erl_off_heap_header*)br;
    *hpp += ERL_REFC_BITS_SIZE;

    return make_bitstring(sb);
}

Eterm erts_make_sub_bitstring(Process *p,
                              Eterm bitstring,
                              Uint offset,
                              Uint size)
{
    Uint inner_offset, inner_size;
    const byte *base;
    Eterm br_flags;
    BinRef *br;
    Eterm *hp;

    ERTS_GET_BITSTRING_REF(bitstring,
                           br_flags,
                           br,
                           base,
                           inner_offset,
                           inner_size);

    ASSERT((offset + size) <= inner_size);
    (void)inner_size;

    hp = HAlloc(p, erts_extracted_bitstring_size(size));
    return erts_build_sub_bitstring(&hp,
                                    br_flags,
                                    br,
                                    base,
                                    inner_offset + offset,
                                    size);
}

Eterm erts_make_sub_binary(Process *p,
                           Eterm bitstring,
                           Uint offset,
                           Uint size)
{
    ASSERT(offset < (ERTS_UWORD_MAX - size));
    ASSERT(IS_BINARY_SIZE_OK(offset + size));
    return erts_make_sub_bitstring(p,
                                   bitstring,
                                   NBITS(offset),
                                   NBITS(size));
}

Eterm
erts_hfact_new_bitstring(ErtsHeapFactory *hfact, Uint reserve_size,
                         Uint size, byte **datap)
{
    if (size <= ERL_ONHEAP_BITS_LIMIT) {
        ErlHeapBits *hb = (ErlHeapBits*)erts_produce_heap(hfact,
                                                          heap_bits_size(size),
                                                          reserve_size);

        hb->thing_word = header_heap_bits(size);
        hb->size = size;

        *datap = (byte*)hb->data;

        return make_bitstring(hb);
    } else {
        Binary *refc_binary = erts_bin_nrml_alloc(NBYTES(size));
        Eterm *hp = erts_produce_heap(hfact,
                                      ERL_REFC_BITS_SIZE,
                                      reserve_size);

        *datap = (byte*)refc_binary->orig_bytes;

        return erts_wrap_refc_bitstring(&(hfact->off_heap)->first,
                                        &(hfact->off_heap)->overhead,
                                        &hp,
                                        refc_binary,
                                        (byte*)refc_binary->orig_bytes,
                                        0,
                                        size);
    }
}

Eterm
erts_hfact_new_binary_from_data(ErtsHeapFactory *hfact, Uint reserve_size,
                                Uint size, const byte *data)
{
    Eterm result;
    byte *base;

    ASSERT(IS_BINARY_SIZE_OK(size));
    result = erts_hfact_new_bitstring(hfact,
                                      reserve_size,
                                      NBITS(size),
                                      &base);

    if (size > 0) {
        sys_memcpy(base, data, size);
    }

    return result;
}

Eterm
erts_new_bitstring_refc(Process *p, Uint size, Binary **binp, byte **datap)
{
    if (size <= ERL_ONHEAP_BITS_LIMIT) {
        ErlHeapBits *hb;

        hb = (ErlHeapBits *)HAlloc(p, heap_bits_size(size));
        hb->thing_word = header_heap_bits(size);
        hb->size = size;

        *datap = (byte*)hb->data;

        return make_bitstring(hb);
    } else {
        Binary *refc_binary = erts_bin_nrml_alloc(NBYTES(size));
        Eterm *hp = HAlloc(p, ERL_REFC_BITS_SIZE);

        *datap = (byte*)refc_binary->orig_bytes;
        *binp = refc_binary;

        return erts_wrap_refc_bitstring(&MSO(p).first,
                                        &MSO(p).overhead,
                                        &hp,
                                        refc_binary,
                                        (byte*)refc_binary->orig_bytes,
                                        0,
                                        size);
    }
}

Eterm
erts_new_bitstring(Process *p, Uint size, byte **datap)
{
    Binary *unused;
    return erts_new_bitstring_refc(p, size, &unused, datap);
}

Eterm erts_new_bitstring_from_data(Process *p, Uint size, const byte *data) {
    Eterm result;
    byte *bytes;

    result = erts_new_bitstring(p, size, &bytes);

    if (size > 0) {
        sys_memcpy(bytes, data, NBYTES(size));
    }

    return result;
}

Eterm
erts_new_binary_refc(Process *p, Uint size, Binary **binp, byte **datap)
{
    ASSERT(IS_BINARY_SIZE_OK(size));
    return erts_new_bitstring_refc(p, NBITS(size), binp, datap);
}

Eterm
erts_new_binary(Process *p, Uint size, byte **datap)
{
    ASSERT(IS_BINARY_SIZE_OK(size));
    return erts_new_bitstring(p, NBITS(size), datap);
}

Eterm erts_new_binary_from_data(Process *p, Uint size, const byte *data)
{
    ASSERT(IS_BINARY_SIZE_OK(size));
    return erts_new_bitstring_from_data(p, NBITS(size), data);
}

Eterm
erts_shrink_binary_term(Eterm binary, size_t size)
{
    Eterm* ptr = bitstring_val(binary);

    if (thing_subtag(*ptr) == HEAP_BITS_SUBTAG) {
        ErlHeapBits *hb = (ErlHeapBits*)ptr;
        ASSERT(TAIL_BITS(hb->size) == 0 && hb->size >= NBITS(size));
        hb->size = NBITS(size);
    } else {
        ErlSubBits *sb = (ErlSubBits*)ptr;
        BinRef *br = (BinRef*)boxed_val(sb->orig);

        ASSERT(erl_sub_bits_is_normal(sb));
        ASSERT(TAIL_BITS(sb->end) == 0 && sb->end >= NBITS(size));
        ASSERT(sb->start == 0);
        sb->end = NBITS(size);

        /* Our allocators are 8-byte aligned, so don't bother reallocating for
         * differences smaller than that. */
        if (size < (NBYTES(sb->end) + 8)) {
            br->val = erts_bin_realloc(br->val, size);
            sb->base_flags = (UWord)&(br->val)->orig_bytes[0];
        }
    }

    return binary;
}
