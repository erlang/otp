/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2006-2017. All Rights Reserved.
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
 * This file implements the former GC BIFs. They used to do a GC when
 * they needed heap space. Because of changes to the implementation of
 * literals, those BIFs are now allowed to allocate heap fragments
 * (using HeapFragOnlyAlloc()). Note that they must NOT call HAlloc(),
 * because the caller does not do any SWAPIN / SWAPOUT (that is,
 * HEAP_TOP(p) and HEAP_LIMIT(p) contain stale values).
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

static Eterm double_to_integer(Process* p, double x);
static BIF_RETTYPE erlang_length_trap(BIF_ALIST_3);
static Export erlang_length_export;

void erts_init_bif_guard(void)
{
    erts_init_trap_export(&erlang_length_export,
			  am_erlang, am_length, 3,
			  &erlang_length_trap);
}

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
	    hp = HeapFragOnlyAlloc(BIF_P, BIG_UINT_HEAP_SIZE);
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

	    hp = HeapFragOnlyAlloc(BIF_P, sz);	/* See note at beginning of file */
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
	    hp = HeapFragOnlyAlloc(BIF_P, FLOAT_SIZE_OBJECT);
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
    hp = HeapFragOnlyAlloc(BIF_P, FLOAT_SIZE_OBJECT);
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

BIF_RETTYPE floor_1(BIF_ALIST_1)
{
    Eterm res;
    FloatDef f;

    if (is_not_float(BIF_ARG_1)) {
	if (is_integer(BIF_ARG_1))
	    BIF_RET(BIF_ARG_1);
	BIF_ERROR(BIF_P, BADARG);
    }
    GET_DOUBLE(BIF_ARG_1, f);
    res = double_to_integer(BIF_P, floor(f.fd));
    BIF_RET(res);
}

BIF_RETTYPE ceil_1(BIF_ALIST_1)
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

    res = double_to_integer(BIF_P, ceil(f.fd));
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
    res = double_to_integer(BIF_P, round(f.fd));
    BIF_RET(res);
}

/*
 * This version of length/1 is called from native code and apply/3.
 */

BIF_RETTYPE length_1(BIF_ALIST_1)
{
    Eterm args[3];

    /*
     * Arrange argument registers the way expected by
     * erts_trapping_length_1(). We save the original argument in
     * args[2] in case an error should signaled.
     */

    args[0] = BIF_ARG_1;
    args[1] = make_small(0);
    args[2] = BIF_ARG_1;
    return erlang_length_trap(BIF_P, args, A__I);
}

static BIF_RETTYPE erlang_length_trap(BIF_ALIST_3)
{
    Eterm res;

    res = erts_trapping_length_1(BIF_P, BIF__ARGS);
    if (is_value(res)) {        /* Success. */
        BIF_RET(res);
    } else {                    /* Trap or error. */
        if (BIF_P->freason == TRAP) {
            /*
             * The available reductions were exceeded. Trap.
             */
            BIF_TRAP3(&erlang_length_export, BIF_P, BIF_ARG_1, BIF_ARG_2, BIF_ARG_3);
        } else {
            /*
             * Signal an error. The original argument was tucked away in BIF_ARG_3.
             */
            ERTS_BIF_ERROR_TRAPPED1(BIF_P, BIF_P->freason,
                                    bif_export[BIF_length_1], BIF_ARG_3);
        }
    }
}

/*
 * Trappable helper function for calculating length/1.
 *
 * When calling this function, entries in args[] should be set up as
 * follows:
 *
 *   args[0] = List to calculate length for.
 *   args[1] = Length accumulator (tagged integer).
 *
 * If the return value is a tagged integer, the length was calculated
 * successfully.
 *
 * Otherwise, if return value is THE_NON_VALUE and p->freason is TRAP,
 * the available reductions were exceeded and this function must be called
 * again after rescheduling. args[0] and args[1] have been updated to
 * contain the next part of the list and length so far, respectively.
 *
 * Otherwise, if return value is THE_NON_VALUE, the list did not end
 * in an empty list (and p->freason is BADARG).
 */

Eterm erts_trapping_length_1(Process* p, Eterm* args)
{
    Eterm list;
    Uint i;
    Uint max_iter;
    Uint saved_max_iter;

#if defined(DEBUG) || defined(VALGRIND)
    max_iter = 50;
#else
    max_iter = ERTS_BIF_REDS_LEFT(p) * 16;
#endif
    saved_max_iter = max_iter;
    ASSERT(max_iter > 0);

    list = args[0];
    i = unsigned_val(args[1]);
    while (is_list(list) && max_iter != 0) {
	list = CDR(list_val(list));
	i++, max_iter--;
    }

    if (is_list(list)) {
        /*
         * We have exceeded the alloted number of iterations.
         * Save the result so far and signal a trap.
         */
        args[0] = list;
        args[1] = make_small(i);
        p->freason = TRAP;
        BUMP_ALL_REDS(p);
        return THE_NON_VALUE;
    } else if (is_not_nil(list))  {
        /* Error. Should be NIL. */
	BIF_ERROR(p, BADARG);
    }

    /*
     * We reached the end of the list successfully. Bump reductions
     * and return result.
     */
    BUMP_REDS(p, (saved_max_iter - max_iter) / 16);
    return make_small(i);
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
	    Eterm* hp = HeapFragOnlyAlloc(BIF_P, BIG_UINT_HEAP_SIZE);
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
		Eterm* hp = HeapFragOnlyAlloc(BIF_P, BIG_UINT_HEAP_SIZE);
		BIF_RET(uint_to_big(low_bits, hp));
	    }
	} else {
	    Uint sz = BIG_UINT_HEAP_SIZE+1;
	    Eterm* hp = HeapFragOnlyAlloc(BIF_P, sz);
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
	    Eterm* hp = HeapFragOnlyAlloc(BIF_P, BIG_UINT_HEAP_SIZE);
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

    hp = HeapFragOnlyAlloc(p, sz);
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
