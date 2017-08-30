/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2006-2016. All Rights Reserved.
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
 * Numeric guard BIFs.
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
#include "erl_map.h"

static Eterm gc_double_to_integer(Process* p, double x, Eterm* reg, Uint live);

static Eterm double_to_integer(Process* p, double x);

/*
 * Guard BIFs called using apply/3 and guard BIFs that never build
 * anything on the heap.
 */

BIF_RETTYPE abs_1(BIF_ALIST_1)
{
    Eterm res;
    Sint i0, i;
    Eterm* hp;

    /* integer arguments */
    if (is_small(BIF_ARG_1)) {
	i0 = signed_val(BIF_ARG_1);
	i = ERTS_SMALL_ABS(i0);
	if (i0 == MIN_SMALL) {
	    hp = HAlloc(BIF_P, BIG_UINT_HEAP_SIZE);
	    BIF_RET(uint_to_big(i, hp));
	} else {
	    BIF_RET(make_small(i));
	}
    } else if (is_big(BIF_ARG_1)) {
	if (!big_sign(BIF_ARG_1)) {
	    BIF_RET(BIF_ARG_1);
	} else {
	    int sz = big_arity(BIF_ARG_1) + 1;
	    Uint* x;

	    hp = HAlloc(BIF_P, sz);	/* See note at beginning of file */
	    sz--;
	    res = make_big(hp);
	    x = big_val(BIF_ARG_1);
	    *hp++ = make_pos_bignum_header(sz);
	    x++;                          /* skip thing */
	    while(sz--)
		*hp++ = *x++;
	    BIF_RET(res);
	}
    } else if (is_float(BIF_ARG_1)) {
	FloatDef f;

	GET_DOUBLE(BIF_ARG_1, f);
	if (f.fd < 0.0) {
	    hp = HAlloc(BIF_P, FLOAT_SIZE_OBJECT);
	    f.fd = fabs(f.fd);
	    res = make_float(hp);
	    PUT_DOUBLE(f, hp);
	    BIF_RET(res);
	}
	else
	    BIF_RET(BIF_ARG_1);
    }
    BIF_ERROR(BIF_P, BADARG);
}

BIF_RETTYPE float_1(BIF_ALIST_1)
{
    Eterm res;
    Eterm* hp;
    FloatDef f;
     
    /* check args */
    if (is_not_integer(BIF_ARG_1)) {
	if (is_float(BIF_ARG_1))  {
	    BIF_RET(BIF_ARG_1);
	} else {
	badarg:
	    BIF_ERROR(BIF_P, BADARG);
	}
    }
    if (is_small(BIF_ARG_1)) {
	Sint i = signed_val(BIF_ARG_1);
	f.fd = i;		/* use "C"'s auto casting */
    } else if (big_to_double(BIF_ARG_1, &f.fd) < 0) {
	goto badarg;
    }
    hp = HAlloc(BIF_P, FLOAT_SIZE_OBJECT);
    res = make_float(hp);
    PUT_DOUBLE(f, hp);
    BIF_RET(res);
}

BIF_RETTYPE trunc_1(BIF_ALIST_1)
{
    Eterm res;
    FloatDef f;
     
    /* check arg */
    if (is_not_float(BIF_ARG_1)) {
	if (is_integer(BIF_ARG_1)) 
	    BIF_RET(BIF_ARG_1);
	BIF_ERROR(BIF_P, BADARG);
    }
    /* get the float */
    GET_DOUBLE(BIF_ARG_1, f);

    /* truncate it and return the resultant integer */
    res = double_to_integer(BIF_P, (f.fd >= 0.0) ? floor(f.fd) : ceil(f.fd));
    BIF_RET(res);
}

BIF_RETTYPE round_1(BIF_ALIST_1)
{
    Eterm res;
    FloatDef f;
     
    /* check arg */ 
    if (is_not_float(BIF_ARG_1)) {
	if (is_integer(BIF_ARG_1)) 
	    BIF_RET(BIF_ARG_1);
	BIF_ERROR(BIF_P, BADARG);
    }
     
    /* get the float */
    GET_DOUBLE(BIF_ARG_1, f);

    /* round it and return the resultant integer */
    res = double_to_integer(BIF_P, (f.fd > 0.0) ? f.fd + 0.5 : f.fd - 0.5);
    BIF_RET(res);
}

BIF_RETTYPE length_1(BIF_ALIST_1)
{
    Eterm list;
    Uint i;
     
    if (is_nil(BIF_ARG_1)) 
	BIF_RET(SMALL_ZERO);
    if (is_not_list(BIF_ARG_1)) {
	BIF_ERROR(BIF_P, BADARG);
    }
    list = BIF_ARG_1;
    i = 0;
    while (is_list(list)) {
	i++;
	list = CDR(list_val(list));
    }
    if (is_not_nil(list))  {
	BIF_ERROR(BIF_P, BADARG);
    }
    BIF_RET(make_small(i));
}

/* returns the size of a tuple or a binary */

BIF_RETTYPE size_1(BIF_ALIST_1)
{
    if (is_tuple(BIF_ARG_1)) {
	Eterm* tupleptr = tuple_val(BIF_ARG_1);

	BIF_RET(make_small(arityval(*tupleptr)));
    } else if (is_binary(BIF_ARG_1)) {
	Uint sz = binary_size(BIF_ARG_1);
	if (IS_USMALL(0, sz)) {
	    return make_small(sz);
	} else {
	    Eterm* hp = HAlloc(BIF_P, BIG_UINT_HEAP_SIZE);
	    BIF_RET(uint_to_big(sz, hp));
	}
    }
    BIF_ERROR(BIF_P, BADARG);
}

/**********************************************************************/
/* returns the bitsize of a bitstring */

BIF_RETTYPE bit_size_1(BIF_ALIST_1)
{
    Uint low_bits;
    Uint bytesize;
    Uint high_bits;
    if (is_binary(BIF_ARG_1)) {
	bytesize = binary_size(BIF_ARG_1);
	high_bits = bytesize >>  ((sizeof(Uint) * 8)-3);
	low_bits = (bytesize << 3) + binary_bitsize(BIF_ARG_1);
	if (high_bits == 0) {
	    if (IS_USMALL(0,low_bits)) {
		BIF_RET(make_small(low_bits));
	    } else {
		Eterm* hp = HAlloc(BIF_P, BIG_UINT_HEAP_SIZE);
		BIF_RET(uint_to_big(low_bits, hp));
	    }
	} else {
	    Uint sz = BIG_UINT_HEAP_SIZE+1;
	    Eterm* hp = HAlloc(BIF_P, sz);
	    hp[0] = make_pos_bignum_header(sz-1);
	    BIG_DIGIT(hp,0) = low_bits;
	    BIG_DIGIT(hp,1) = high_bits;
	    BIF_RET(make_big(hp));
	}
    } else {
	BIF_ERROR(BIF_P, BADARG);
    }
}

/**********************************************************************/
/* returns the number of bytes need to store a bitstring */

BIF_RETTYPE byte_size_1(BIF_ALIST_1)
{
    if (is_binary(BIF_ARG_1)) {
	Uint bytesize = binary_size(BIF_ARG_1);
	if (binary_bitsize(BIF_ARG_1) > 0) {
	    bytesize++;
	}
	if (IS_USMALL(0, bytesize)) {
	    BIF_RET(make_small(bytesize));
	} else {
	    Eterm* hp = HAlloc(BIF_P, BIG_UINT_HEAP_SIZE);
	    BIF_RET(uint_to_big(bytesize, hp));
	}
    } else {
	BIF_ERROR(BIF_P, BADARG);
    }
}

/*
 * Generate the integer part from a double.
 */
static Eterm
double_to_integer(Process* p, double x)
{
    int is_negative;
    int ds;
    ErtsDigit* xp;
    int i;
    Eterm res;
    size_t sz;
    Eterm* hp;
    double dbase;

    if ((x < (double) (MAX_SMALL+1)) && (x > (double) (MIN_SMALL-1))) {
	Sint xi = x;
	return make_small(xi);
    }

    if (x >= 0) {
	is_negative = 0;
    } else {
	is_negative = 1;
	x = -x;
    }

    /* Unscale & (calculate exponent) */
    ds = 0;
    dbase = ((double)(D_MASK)+1);
    while(x >= 1.0) {
	x /= dbase;         /* "shift" right */
	ds++;
    }
    sz = BIG_NEED_SIZE(ds);          /* number of words including arity */

    hp = HAlloc(p, sz);
    res = make_big(hp);
    xp = (ErtsDigit*) (hp + 1);

    for (i = ds-1; i >= 0; i--) {
	ErtsDigit d;

	x *= dbase;      /* "shift" left */
	d = x;            /* trunc */
	xp[i] = d;        /* store digit */
	x -= d;           /* remove integer part */
    }
    while ((ds & (BIG_DIGITS_PER_WORD-1)) != 0) {
	xp[ds++] = 0;
    }

    if (is_negative) {
	*hp = make_neg_bignum_header(sz-1);
    } else {
	*hp = make_pos_bignum_header(sz-1);
    }
    return res;
}

/********************************************************************************
 * binary_part guards. The actual implementation is in erl_bif_binary.c
 ********************************************************************************/
BIF_RETTYPE binary_part_3(BIF_ALIST_3)
{
    return erts_binary_part(BIF_P,BIF_ARG_1,BIF_ARG_2, BIF_ARG_3);
}

BIF_RETTYPE binary_part_2(BIF_ALIST_2)
{
    Eterm *tp;
    if (is_not_tuple(BIF_ARG_2)) {
	goto badarg;
    }
    tp = tuple_val(BIF_ARG_2);
    if (arityval(*tp) != 2) {
	goto badarg;
    }
    return erts_binary_part(BIF_P,BIF_ARG_1,tp[1], tp[2]);
 badarg:
   BIF_ERROR(BIF_P,BADARG);
}


/*
 * The following code is used when a guard that may build on the
 * heap is called directly. They must not use HAlloc(), but must
 * do a garbage collection if there is insufficient heap space.
 *
 * Important note: All error checking MUST be done before doing
 * a garbage collection. The compiler assumes that all registers
 * are still valid if a guard BIF generates an exception.
 */

#define ERTS_NEED_GC(p, need) ((HEAP_LIMIT((p)) - HEAP_TOP((p))) <= (need))

Eterm erts_gc_length_1(Process* p, Eterm* reg, Uint live)
{
    Eterm list = reg[live];
    int i;

    if (is_nil(list)) 
	return SMALL_ZERO;
    i = 0;
    while (is_list(list)) {
	i++;
	list = CDR(list_val(list));
    }
    if (is_not_nil(list))  {
	BIF_ERROR(p, BADARG);
    }
    return make_small(i);
}

Eterm erts_gc_size_1(Process* p, Eterm* reg, Uint live)
{
    Eterm arg = reg[live];
    if (is_tuple(arg)) {
	Eterm* tupleptr = tuple_val(arg);
	return make_small(arityval(*tupleptr));
    } else if (is_binary(arg)) {
	Uint sz = binary_size(arg);
	if (IS_USMALL(0, sz)) {
	    return make_small(sz);
	} else {
	    Eterm* hp;
	    if (ERTS_NEED_GC(p, BIG_UINT_HEAP_SIZE)) {
		erts_garbage_collect(p, BIG_UINT_HEAP_SIZE, reg, live);
	    }
	    hp = p->htop;
	    p->htop += BIG_UINT_HEAP_SIZE;
	    return uint_to_big(sz, hp);
	}
    }
    BIF_ERROR(p, BADARG);
}

Eterm erts_gc_bit_size_1(Process* p, Eterm* reg, Uint live)
{
    Eterm arg = reg[live];
    if (is_binary(arg)) {
	Uint low_bits;
	Uint bytesize;
	Uint high_bits;
	bytesize = binary_size(arg);
	high_bits = bytesize >>  ((sizeof(Uint) * 8)-3);
	low_bits = (bytesize << 3) + binary_bitsize(arg);
	if (high_bits == 0) {
	    if (IS_USMALL(0,low_bits)) {
		return make_small(low_bits);
	    } else {
		Eterm* hp; 
		if (ERTS_NEED_GC(p, BIG_UINT_HEAP_SIZE)) {
		    erts_garbage_collect(p, BIG_UINT_HEAP_SIZE, reg, live);
		}
		hp = p->htop;
		p->htop += BIG_UINT_HEAP_SIZE;
		return uint_to_big(low_bits, hp);
	    }
	} else {
	    Uint sz = BIG_UINT_HEAP_SIZE+1;
	    Eterm* hp;
	    if (ERTS_NEED_GC(p, sz)) {
		erts_garbage_collect(p, sz, reg, live);
	    }
	    hp = p->htop;
	    p->htop += sz;
	    hp[0] = make_pos_bignum_header(sz-1);
	    BIG_DIGIT(hp,0) = low_bits;
	    BIG_DIGIT(hp,1) = high_bits;
	    return make_big(hp);
	}
    } else {
	BIF_ERROR(p, BADARG);
    }
}

Eterm erts_gc_byte_size_1(Process* p, Eterm* reg, Uint live)
{
    Eterm arg = reg[live];
    if (is_binary(arg)) {
	Uint bytesize = binary_size(arg);
	if (binary_bitsize(arg) > 0) {
	    bytesize++;
	}
	if (IS_USMALL(0, bytesize)) {
	    return make_small(bytesize);
	} else {
	    Eterm* hp;
	    if (ERTS_NEED_GC(p, BIG_UINT_HEAP_SIZE)) {
		erts_garbage_collect(p, BIG_UINT_HEAP_SIZE, reg, live);
	    }
	    hp = p->htop;
	    p->htop += BIG_UINT_HEAP_SIZE;
	    return uint_to_big(bytesize, hp);
	}
    } else {
	BIF_ERROR(p, BADARG);
    }
}

Eterm erts_gc_map_size_1(Process* p, Eterm* reg, Uint live)
{
    Eterm arg = reg[live];
    if (is_flatmap(arg)) {
	flatmap_t *mp = (flatmap_t*)flatmap_val(arg);
        return make_small(flatmap_get_size(mp));
    } else if (is_hashmap(arg)) {
        Eterm* hp;
        Uint size;
	size = hashmap_size(arg);
        if (IS_USMALL(0, size)) {
            return make_small(size);
        }
        if (ERTS_NEED_GC(p, BIG_UINT_HEAP_SIZE)) {
            erts_garbage_collect(p, BIG_UINT_HEAP_SIZE, reg, live);
        }
        hp = p->htop;
        p->htop += BIG_UINT_HEAP_SIZE;
        return uint_to_big(size, hp);
    }
    p->fvalue = arg;
    BIF_ERROR(p, BADMAP);
}

Eterm erts_gc_abs_1(Process* p, Eterm* reg, Uint live)
{
    Eterm arg;
    Eterm res;
    Sint i0, i;
    Eterm* hp;

    arg = reg[live];

    /* integer arguments */
    if (is_small(arg)) {
	i0 = signed_val(arg);
	i = ERTS_SMALL_ABS(i0);
	if (i0 == MIN_SMALL) {
	    if (ERTS_NEED_GC(p, BIG_UINT_HEAP_SIZE)) {
		erts_garbage_collect(p, BIG_UINT_HEAP_SIZE, reg, live+1);
		arg = reg[live];
	    }
	    hp = p->htop;
	    p->htop += BIG_UINT_HEAP_SIZE;
	    return uint_to_big(i, hp);
	} else {
	    return make_small(i);
	}
    } else if (is_big(arg)) {
	if (!big_sign(arg)) {
	    return arg;
	} else {
	    int sz = big_arity(arg) + 1;
	    Uint* x;

	    if (ERTS_NEED_GC(p, sz)) {
		erts_garbage_collect(p, sz, reg, live+1);
		arg = reg[live];
	    }
	    hp = p->htop;
	    p->htop += sz;
	    sz--;
	    res = make_big(hp);
	    x = big_val(arg);
	    *hp++ = make_pos_bignum_header(sz);
	    x++;                          /* skip thing */
	    while(sz--)
		*hp++ = *x++;
	    return res;
	}
    } else if (is_float(arg)) {
	FloatDef f;

	GET_DOUBLE(arg, f);
	if (f.fd < 0.0) {
	    if (ERTS_NEED_GC(p, FLOAT_SIZE_OBJECT)) {
		erts_garbage_collect(p, FLOAT_SIZE_OBJECT, reg, live+1);
		arg = reg[live];
	    }
	    hp = p->htop;
	    p->htop += FLOAT_SIZE_OBJECT;
	    f.fd = fabs(f.fd);
	    res = make_float(hp);
	    PUT_DOUBLE(f, hp);
	    return res;
	}
	else
	    return arg;
    }
    BIF_ERROR(p, BADARG);
}

Eterm erts_gc_float_1(Process* p, Eterm* reg, Uint live)
{
    Eterm arg;
    Eterm res;
    Eterm* hp;
    FloatDef f;
     
    /* check args */
    arg = reg[live];
    if (is_not_integer(arg)) {
	if (is_float(arg))  {
	    return arg;
	} else {
	badarg:
	    BIF_ERROR(p, BADARG);
	}
    }
    if (is_small(arg)) {
	Sint i = signed_val(arg);
	f.fd = i;		/* use "C"'s auto casting */
    } else if (big_to_double(arg, &f.fd) < 0) {
	goto badarg;
    }
    if (ERTS_NEED_GC(p, FLOAT_SIZE_OBJECT)) {
	erts_garbage_collect(p, FLOAT_SIZE_OBJECT, reg, live+1);
	arg = reg[live];
    }
    hp = p->htop;
    p->htop += FLOAT_SIZE_OBJECT;
    res = make_float(hp);
    PUT_DOUBLE(f, hp);
    return res;
}

Eterm erts_gc_round_1(Process* p, Eterm* reg, Uint live)
{
    Eterm arg;
    FloatDef f;
     
    arg = reg[live];
    if (is_not_float(arg)) {
	if (is_integer(arg))  {
	    return arg;
	}
	BIF_ERROR(p, BADARG);
    }
    GET_DOUBLE(arg, f);

    return gc_double_to_integer(p, (f.fd > 0.0) ? f.fd + 0.5 : f.fd - 0.5,
				reg, live);
}

Eterm erts_gc_trunc_1(Process* p, Eterm* reg, Uint live)
{
    Eterm arg;
    FloatDef f;
     
    arg = reg[live];
    if (is_not_float(arg)) {
	if (is_integer(arg))  {
	    return arg;
	}
	BIF_ERROR(p, BADARG);
    }
    /* get the float */
    GET_DOUBLE(arg, f);

    /* truncate it and return the resultant integer */
    return gc_double_to_integer(p, (f.fd >= 0.0) ? floor(f.fd) : ceil(f.fd),
				reg, live);
}

static Eterm
gc_double_to_integer(Process* p, double x, Eterm* reg, Uint live)
{
    int is_negative;
    int ds;
    ErtsDigit* xp;
    int i;
    Eterm res;
    size_t sz;
    Eterm* hp;
    double dbase;

    if ((x < (double) (MAX_SMALL+1)) && (x > (double) (MIN_SMALL-1))) {
	Sint xi = x;
	return make_small(xi);
    }

    if (x >= 0) {
	is_negative = 0;
    } else {
	is_negative = 1;
	x = -x;
    }

    /* Unscale & (calculate exponent) */
    ds = 0;
    dbase = ((double)(D_MASK)+1);
    while(x >= 1.0) {
	x /= dbase;         /* "shift" right */
	ds++;
    }
    sz = BIG_NEED_SIZE(ds);          /* number of words including arity */
    if (ERTS_NEED_GC(p, sz)) {
	erts_garbage_collect(p, sz, reg, live);
    }
    hp = p->htop;
    p->htop += sz;
    res = make_big(hp);
    xp = (ErtsDigit*) (hp + 1);

    for (i = ds-1; i >= 0; i--) {
	ErtsDigit d;

	x *= dbase;      /* "shift" left */
	d = x;            /* trunc */
	xp[i] = d;        /* store digit */
	x -= d;           /* remove integer part */
    }
    while ((ds & (BIG_DIGITS_PER_WORD-1)) != 0) {
	xp[ds++] = 0;
    }

    if (is_negative) {
	*hp = make_neg_bignum_header(sz-1);
    } else {
	*hp = make_pos_bignum_header(sz-1);
    }
    return res;
}

/********************************************************************************
 * binary_part guards. The actual implementation is in erl_bif_binary.c
 ********************************************************************************/
Eterm erts_gc_binary_part_3(Process* p, Eterm* reg, Uint live)
{
    return erts_gc_binary_part(p,reg,live,0);
}

Eterm erts_gc_binary_part_2(Process* p, Eterm* reg, Uint live)
{
    return erts_gc_binary_part(p,reg,live,1);
}
