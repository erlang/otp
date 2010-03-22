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

#ifndef __BIG_H__
#define __BIG_H__

#ifndef __SYS_H__
#include "sys.h"
#endif

#ifndef __CONFIG_H__
#include "erl_vm.h"
#endif

#ifndef __GLOBAL_H__
#include "global.h"
#endif

typedef Uint     ErtsDigit;

#if ((SIZEOF_VOID_P == 4) || HALFWORD_HEAP) && defined(SIZEOF_LONG_LONG) && (SIZEOF_LONG_LONG == 8)
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
#error "can not determine machine size"
#endif

#define D_DECIMAL_EXP	9
#define D_DECIMAL_BASE	1000000000

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
#define IS_SSMALL(x)      (((x) >= MIN_SMALL) && ((x) <= MAX_SMALL))

/* The heap size needed for a bignum */
#define BIG_NEED_SIZE(x)  ((x) + 1)

#define BIG_UINT_HEAP_SIZE (1 + 1)	/* always, since sizeof(Uint) <= sizeof(Eterm) */

#if HALFWORD_HEAP
#define BIG_UWORD_HEAP_SIZE(UW) (((UW) >> (sizeof(Uint) * 8)) ? 3 : 2)
#else
#define BIG_UWORD_HEAP_SIZE(UW) BIG_UINT_HEAP_SIZE
#endif

#if defined(ARCH_32) || HALFWORD_HEAP

#define ERTS_UINT64_BIG_HEAP_SIZE__(X) \
  ((X) >= (((Uint64) 1) << 32) ? (1 + 2) : (1 + 1))
#define ERTS_SINT64_HEAP_SIZE(X)				\
  (IS_SSMALL((X))						\
   ? 0								\
   : ERTS_UINT64_BIG_HEAP_SIZE__((X) >= 0 ? (X) : -(X)))
#define ERTS_UINT64_HEAP_SIZE(X)				\
  (IS_USMALL(0, (X)) ? 0 : ERTS_UINT64_BIG_HEAP_SIZE__((X)))

#else

#define ERTS_SINT64_HEAP_SIZE(X)				\
  (IS_SSMALL((X)) ? 0 : (1 + 1))
#define ERTS_UINT64_HEAP_SIZE(X)				\
  (IS_USMALL(0, (X)) ? 0 : (1 + 1))

#endif

int big_decimal_estimate(Eterm);
Eterm erts_big_to_list(Eterm, Eterm**);
char *erts_big_to_string(Eterm x, char *buf, Uint buf_sz);

Eterm small_times(Sint, Sint, Eterm*);

Eterm big_plus(Eterm, Eterm, Eterm*);
Eterm big_minus(Eterm, Eterm, Eterm*);
Eterm big_times(Eterm, Eterm, Eterm*);
Eterm big_div(Eterm, Eterm, Eterm*);
Eterm big_rem(Eterm, Eterm, Eterm*);
Eterm big_neg(Eterm, Eterm*);

Eterm big_minus_small(Eterm, Uint, Eterm*);
Eterm big_plus_small(Eterm, Uint, Eterm*);
Eterm big_times_small(Eterm, Uint, Eterm*);

Eterm big_band(Eterm, Eterm, Eterm*);
Eterm big_bor(Eterm, Eterm, Eterm*);
Eterm big_bxor(Eterm, Eterm, Eterm*);
Eterm big_bnot(Eterm, Eterm*);

Eterm big_lshift(Eterm, Sint, Eterm*);
int big_comp (Eterm, Eterm);
int big_ucomp (Eterm, Eterm);
int big_to_double(Eterm x, double* resp);
Eterm small_to_big(Sint, Eterm*);
Eterm uint_to_big(Uint, Eterm*);
Eterm uword_to_big(UWord, Eterm*);
Eterm erts_make_integer(Uint, Process *);

dsize_t big_bytes(Eterm);
Eterm bytes_to_big(byte*, dsize_t, int, Eterm*);
byte* big_to_bytes(Eterm, byte*);

int term_to_Uint(Eterm, Uint*);
int term_to_UWord(Eterm, UWord*);
int term_to_Sint(Eterm, Sint*);

Uint32 big_to_uint32(Eterm b);
int term_equals_2pow32(Eterm);

Eterm erts_uint64_to_big(Uint64, Eterm **);
Eterm erts_sint64_to_big(Sint64, Eterm **);

#endif

