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

#ifndef __BIG_H__
#define __BIG_H__

#include "sys.h"
#include "global.h"

typedef Uint     ErtsDigit;

#if (SIZEOF_VOID_P == 4) && defined(SIZEOF_LONG_LONG) && (SIZEOF_LONG_LONG == 8)
/* Assume 32-bit machine with long long support */
typedef Uint64   ErtsDoubleDigit;
typedef Uint16   ErtsHalfDigit;
#define BIG_HAVE_DOUBLE_DIGIT 1

#elif (SIZEOF_VOID_P == 4)
/* Assume 32-bit machine with no long support */
#undef  BIG_HAVE_DOUBLE_DIGIT
typedef Uint16   ErtsHalfDigit;

#elif (SIZEOF_VOID_P == 8)
/* Assume 64-bit machine, does it exist 128 bit long long long ? */
#undef  BIG_HAVE_DOUBLE_DIGIT
typedef Uint32   ErtsHalfDigit;
#else
#error "cannot determine machine size"
#endif

typedef Uint  dsize_t;	 /* Vector size type */

#define D_EXP (ERTS_SIZEOF_ETERM*8)
#define D_MASK     ((ErtsDigit)(-1))      /* D_BASE-1 */

/* macros for bignum objects */
#define big_v(x)       BIG_V(big_val(x))
#define big_sign(x)    BIG_SIGN(big_val(x))
#define big_arity(x)   BIG_ARITY(big_val(x))
#define big_digit(x,i) BIG_DIGIT(big_val(x),i)
#define big_size(x)    BIG_SIZE(big_val(x))


/* macros for thing pointers */

#define BIG_V(xp)        ((ErtsDigit*)((xp)+1))
#define BIG_SIGN(xp)     (!!bignum_header_is_neg(*xp))
#define BIG_ARITY(xp)    ((Uint)bignum_header_arity(*(xp)))
#define BIG_DIGIT(xp,i)  *(BIG_V(xp)+(i))
#define BIG_DIGITS_PER_WORD (sizeof(Uint)/sizeof(ErtsDigit))

#define BIG_SIZE(xp)  BIG_ARITY(xp)

/* Check for small */
#define IS_USMALL(sgn,x)  ((sgn) ? ((x) <= MAX_SMALL+1) : ((x) <= MAX_SMALL))

/*
 * It seems that both clang and gcc will generate sub-optimal code
 * for the more obvious way to write the range check:
 *
 *    #define IS_SSMALL(x)  (((x) >= MIN_SMALL) && ((x) <= MAX_SMALL))
 *
 * Note that IS_SSMALL() may be used in the 32-bit emulator with
 * a Uint64 argument. Therefore, we must test the size of the argument
 * to ensure that the cast does not discard the high-order 32 bits.
 */
#if defined(ARCH_32)
#  define _IS_SSMALL32(x) (((Uint32) ((((x)) >> (SMALL_BITS-1)) + 1)) < 2)
#else
#  define _IS_SSMALL32(x) (1)
#endif
#define _IS_SSMALL64(x) (((Uint64) ((((x)) >> (SMALL_BITS-1)) + 1)) < 2)
#define IS_SSMALL(x) (sizeof(x) == sizeof(Uint32) ? _IS_SSMALL32(x) : _IS_SSMALL64(x))

/* The heap size needed for a bignum */
#define BIG_NEED_SIZE(x)  ((x) + 1)
#define BIG_NEED_FOR_BITS(bits) BIG_NEED_SIZE(((bits)-1)/D_EXP + 1)

#define BIG_UINT_HEAP_SIZE (1 + 1)	/* always, since sizeof(Uint) <= sizeof(Eterm) */

#define BIG_UWORD_HEAP_SIZE(UW) BIG_UINT_HEAP_SIZE

#if defined(ARCH_32)

#define ERTS_UINT64_BIG_HEAP_SIZE__(X) \
  ((X) >= (((Uint64) 1) << 32) ? (1 + 2) : (1 + 1))
#define ERTS_SINT64_HEAP_SIZE(X)				\
  (IS_SSMALL((X))						\
   ? 0								\
   : ERTS_UINT64_BIG_HEAP_SIZE__((X) >= 0 ? (X) : -(Uint64)(X)))
#define ERTS_UINT64_HEAP_SIZE(X)				\
  (IS_USMALL(0, (X)) ? 0 : ERTS_UINT64_BIG_HEAP_SIZE__((X)))
#define ERTS_MAX_SINT64_HEAP_SIZE (1 + 2)
#define ERTS_MAX_UINT64_HEAP_SIZE (1 + 2)
#define ERTS_UINT64_ARRAY_TO_BIG_MAX_HEAP_SZ(LEN) (2*(LEN)+1)

#else

#define ERTS_SINT64_HEAP_SIZE(X)				\
  (IS_SSMALL((X)) ? 0 : (1 + 1))
#define ERTS_UINT64_HEAP_SIZE(X)				\
  (IS_USMALL(0, (X)) ? 0 : (1 + 1))
#define ERTS_MAX_SINT64_HEAP_SIZE (1 + 1)
#define ERTS_MAX_UINT64_HEAP_SIZE (1 + 1)
#define ERTS_UINT64_ARRAY_TO_BIG_MAX_HEAP_SZ(LEN) ((LEN)+1)

#endif

int big_integer_estimate(Wterm, Uint base);
Eterm erts_big_to_list(Eterm, int base, Eterm**);
char *erts_big_to_string(Wterm x, int base, char *buf, Uint buf_sz);
Uint erts_big_to_binary_bytes(Eterm x, int base, char *buf, Uint buf_sz);

Eterm small_times(Sint, Sint, Eterm*);

Eterm big_plus(Wterm, Wterm, Eterm*);
Eterm big_minus(Eterm, Eterm, Eterm*);
Eterm big_times(Eterm, Eterm, Eterm*);
Eterm big_div(Eterm, Eterm, Eterm*);
Eterm big_rem(Eterm, Eterm, Eterm*);

Eterm big_plus_small(Eterm, Uint, Eterm*);
Eterm big_times_small(Eterm, Uint, Eterm*);

Eterm big_band(Eterm, Eterm, Eterm*);
Eterm big_bor(Eterm, Eterm, Eterm*);
Eterm big_bxor(Eterm, Eterm, Eterm*);
Eterm big_bnot(Eterm, Eterm*);

Eterm big_lshift(Eterm, Sint, Eterm*);
int big_comp (Wterm, Wterm);
int big_ucomp (Eterm, Eterm);
int big_to_double(Wterm x, double* resp);
Eterm double_to_big(double, Eterm*, Uint hsz);
Eterm small_to_big(Sint, Eterm*);
Eterm uint_to_big(Uint, Eterm*);
Eterm uword_to_big(UWord, Eterm*);
Eterm erts_make_integer(Uint, Process *);
Eterm erts_make_integer_from_uword(UWord x, Process *p);

dsize_t big_bytes(Eterm);
Eterm bytes_to_big(byte*, dsize_t, int, Eterm*);
byte* big_to_bytes(Eterm, byte*);

int term_to_Uint(Eterm, Uint*);
int term_to_Uint_mask(Eterm, Uint*);
int term_to_UWord(Eterm, UWord*);
int term_to_Sint(Eterm, Sint*);
#if HAVE_INT64
Eterm erts_uint64_array_to_big(Uint **, int, int, Uint64 *);
int term_to_Uint64(Eterm, Uint64*);
int term_to_Sint64(Eterm, Sint64*);
#endif

Uint32 big_to_uint32(Eterm b);
int term_equals_2pow32(Eterm);

Eterm erts_uint64_to_big(Uint64, Eterm **);
Eterm erts_sint64_to_big(Sint64, Eterm **);

Eterm erts_chars_to_integer(Process *, char*, Uint, const int);

/* How list_to_integer classifies the input, was it even a string? */
typedef enum {
    LTI_BAD_STRUCTURE = 0,
    LTI_NO_INTEGER    = 1,
    LTI_SOME_INTEGER  = 2,
    LTI_ALL_INTEGER   = 3
} LTI_result_t;

LTI_result_t erts_list_to_integer(Process *BIF_P, Eterm orig_list,
                                  const Uint base,
                                  Eterm *integer_out, Eterm *tail_out);
#endif
