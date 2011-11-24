/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1999-2010. All Rights Reserved.
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

/*
 * Arithmetic functions formerly found in beam_emu.c
 * now available as bifs as erl_db_util and db_match_compile needs
 * them.
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
#include "atom.h"

#ifndef MAX
#  define MAX(x, y) (((x) > (y)) ? (x) : (y))
#endif

#if !HEAP_ON_C_STACK
#  define DECLARE_TMP(VariableName,N,P)  \
     Eterm *VariableName = ((ERTS_PROC_GET_SCHDATA(P)->erl_arith_tmp_heap) + (2 * N))
#else
#  define DECLARE_TMP(VariableName,N,P) \
     Eterm VariableName[2]
#endif
#  define ARG_IS_NOT_TMP(Arg,Tmp) ((Arg) != make_big((Tmp)))


static Eterm shift(Process* p, Eterm arg1, Eterm arg2, int right);

static ERTS_INLINE void maybe_shrink(Process* p, Eterm* hp, Eterm res, Uint alloc)
{
    Uint actual;

    if (is_immed(res)) {
	if (p->heap <= hp && hp < p->htop) {
	    p->htop = hp;
	}
	else {
	    erts_heap_frag_shrink(p, hp);
	}
    } else if ((actual = bignum_header_arity(*hp)+1) < alloc) {
	if (p->heap <= hp && hp < p->htop) {
	    p->htop = hp+actual;
	}
	else {
	    erts_heap_frag_shrink(p, hp+actual);
	}
    }
}

/*
 * BIF interfaces. They will only be from match specs and
 * when a BIF is applied.
 */

BIF_RETTYPE splus_1(BIF_ALIST_1)
{
    if (is_number(BIF_ARG_1)) {
	BIF_RET(BIF_ARG_1);
    } else {
	BIF_ERROR(BIF_P, BADARITH);
    }
} 

BIF_RETTYPE splus_2(BIF_ALIST_2)
{
    BIF_RET(erts_mixed_plus(BIF_P, BIF_ARG_1, BIF_ARG_2));
} 

BIF_RETTYPE sminus_1(BIF_ALIST_1)
{
    BIF_RET(erts_mixed_minus(BIF_P, make_small(0), BIF_ARG_1));
} 

BIF_RETTYPE sminus_2(BIF_ALIST_2)
{
    BIF_RET(erts_mixed_minus(BIF_P, BIF_ARG_1, BIF_ARG_2));
} 

BIF_RETTYPE stimes_2(BIF_ALIST_2)
{
    BIF_RET(erts_mixed_times(BIF_P, BIF_ARG_1, BIF_ARG_2));
} 

BIF_RETTYPE div_2(BIF_ALIST_2)
{
    BIF_RET(erts_mixed_div(BIF_P, BIF_ARG_1, BIF_ARG_2));
} 

BIF_RETTYPE intdiv_2(BIF_ALIST_2)
{
    if (BIF_ARG_2 == SMALL_ZERO) {
	BIF_ERROR(BIF_P, BADARITH);
    }
    if (is_both_small(BIF_ARG_1,BIF_ARG_2)){
	Sint ires = signed_val(BIF_ARG_1) / signed_val(BIF_ARG_2);
	if (MY_IS_SSMALL(ires))
	    BIF_RET(make_small(ires));
    } 
    BIF_RET(erts_int_div(BIF_P, BIF_ARG_1, BIF_ARG_2));
} 

BIF_RETTYPE rem_2(BIF_ALIST_2)
{
    if (BIF_ARG_2 == SMALL_ZERO) {
	BIF_ERROR(BIF_P, BADARITH);
    }
    if (is_both_small(BIF_ARG_1,BIF_ARG_2)){
	/* Is this really correct? Isn't there a difference between 
	   remainder and modulo that is not defined in C? Well, I don't
	   remember, this is the way it's done in beam_emu anyway... */
	BIF_RET(make_small(signed_val(BIF_ARG_1) % signed_val(BIF_ARG_2)));
    } 
    BIF_RET(erts_int_rem(BIF_P, BIF_ARG_1, BIF_ARG_2));
} 

BIF_RETTYPE band_2(BIF_ALIST_2)
{
    if (is_both_small(BIF_ARG_1,BIF_ARG_2)){
	BIF_RET(BIF_ARG_1 & BIF_ARG_2);
    } 
    BIF_RET(erts_band(BIF_P, BIF_ARG_1, BIF_ARG_2));
} 

BIF_RETTYPE bor_2(BIF_ALIST_2)
{
    if (is_both_small(BIF_ARG_1,BIF_ARG_2)){
	BIF_RET(BIF_ARG_1 | BIF_ARG_2);
    } 
    BIF_RET(erts_bor(BIF_P, BIF_ARG_1, BIF_ARG_2));
} 

BIF_RETTYPE bxor_2(BIF_ALIST_2)
{
    if (is_both_small(BIF_ARG_1,BIF_ARG_2)){
	BIF_RET(make_small(signed_val(BIF_ARG_1) ^ signed_val(BIF_ARG_2)));
    } 
    BIF_RET(erts_bxor(BIF_P, BIF_ARG_1, BIF_ARG_2));
} 

BIF_RETTYPE bsl_2(BIF_ALIST_2)
{
    BIF_RET(shift(BIF_P, BIF_ARG_1, BIF_ARG_2, 0));
} 

BIF_RETTYPE bsr_2(BIF_ALIST_2)
{
    BIF_RET(shift(BIF_P, BIF_ARG_1, BIF_ARG_2, 1));
} 

static Eterm
shift(Process* p, Eterm arg1, Eterm arg2, int right)
{
    Sint i;
    Sint ires;
    DECLARE_TMP(tmp_big1,0,p);
    Eterm* bigp;
    Uint need;

    if (right) {
	if (is_small(arg2)) {
	    i = -signed_val(arg2);
	    if (is_small(arg1)) {
		goto small_shift;
	    } else if (is_big(arg1)) {
		if (i == 0) {
		    BIF_RET(arg1);
		}
		goto big_shift;
	    }
	} else if (is_big(arg2)) {
	    /*
	     * N bsr NegativeBigNum == N bsl MAX_SMALL
	     * N bsr PositiveBigNum == N bsl MIN_SMALL
	     */
	    arg2 = make_small(bignum_header_is_neg(*big_val(arg2)) ?
			      MAX_SMALL : MIN_SMALL);
	    goto do_bsl;
	}
    } else {
    do_bsl:
	if (is_small(arg2)) {
	    i = signed_val(arg2);

	    if (is_small(arg1)) {
	    small_shift:
		ires = signed_val(arg1);
	     
		if (i == 0 || ires == 0) {
		    BIF_RET(arg1);
		} else if (i < 0)  { /* Right shift */
		    i = -i;
		    if (i >= SMALL_BITS-1) {
			arg1 = (ires < 0) ? SMALL_MINUS_ONE : SMALL_ZERO;
		    } else {
			arg1 = make_small(ires >> i);
		    }
		    BIF_RET(arg1);
		} else if (i < SMALL_BITS-1) { /* Left shift */
		    if ((ires > 0 && ((~(Uint)0 << ((SMALL_BITS-1)-i)) & ires) == 0) ||
			((~(Uint)0 << ((SMALL_BITS-1)-i)) & ~ires) == 0) {
			arg1 = make_small(ires << i);
			BIF_RET(arg1);
		    }
		}
		arg1 = small_to_big(ires, tmp_big1);

	    big_shift:
		if (i > 0) {	/* Left shift. */
		    ires = big_size(arg1) + (i / D_EXP);
		} else {	/* Right shift. */
		    ires = big_size(arg1);
		    if (ires <= (-i / D_EXP))
			ires = 3;
		    else
			ires -= (-i / D_EXP);
		}

		/*
		 * Slightly conservative check the size to avoid
		 * allocating huge amounts of memory for bignums that 
		 * clearly would overflow the arity in the header
		 * word.
		 */
		if (ires-8 > BIG_ARITY_MAX) {
		    BIF_ERROR(p, SYSTEM_LIMIT);
		}
		need = BIG_NEED_SIZE(ires+1);
		bigp = HAlloc(p, need);
		arg1 = big_lshift(arg1, i, bigp);
		maybe_shrink(p, bigp, arg1, need);
		if (is_nil(arg1)) {
		    /*
		     * This result must have been only slight larger
		     * than allowed since it wasn't caught by the
		     * previous test.
		     */
		    BIF_ERROR(p, SYSTEM_LIMIT);
		}
		BIF_RET(arg1);
	    } else if (is_big(arg1)) {
		if (i == 0) {
		    BIF_RET(arg1);
		}
		goto big_shift;
	    }
	} else if (is_big(arg2)) {
	    if (bignum_header_is_neg(*big_val(arg2))) {
		/*
		 * N bsl NegativeBigNum is either 0 or -1, depending on
		 * the sign of N. Since we don't believe this case
		 * is common, do the calculation with the minimum
		 * amount of code.
		 */
		arg2 = make_small(MIN_SMALL);
		goto do_bsl;
	    } else if (is_small(arg1) || is_big(arg1)) {
		/*
		 * N bsl PositiveBigNum is too large to represent.
		 */
		BIF_ERROR(p, SYSTEM_LIMIT);
	    }
	     /* Fall through if the left argument is not an integer. */
	}
    }
    BIF_ERROR(p, BADARITH);
}

BIF_RETTYPE bnot_1(BIF_ALIST_1)
{
    Eterm ret;

    if (is_small(BIF_ARG_1)) {
	ret = make_small(~signed_val(BIF_ARG_1));
    } else if (is_big(BIF_ARG_1)) {
	Uint need = BIG_NEED_SIZE(big_size(BIF_ARG_1)+1);
	Eterm* bigp = HAlloc(BIF_P, need);

	ret = big_bnot(BIF_ARG_1, bigp);
	maybe_shrink(BIF_P, bigp, ret, need);
	if (is_nil(ret)) {
	    BIF_ERROR(BIF_P, SYSTEM_LIMIT);
	}
    } else {
	BIF_ERROR(BIF_P, BADARITH);
    }
    BIF_RET(ret);
} 

/*
 * Implementation and interfaces for the rest of the runtime system.
 * The functions that follow are only used in match specs and when
 * arithmetic functions are applied.
 */

Eterm
erts_mixed_plus(Process* p, Eterm arg1, Eterm arg2)
{
    DECLARE_TMP(tmp_big1,0,p);
    DECLARE_TMP(tmp_big2,1,p);
    Eterm res;
    Eterm hdr;
    FloatDef f1, f2;
    dsize_t sz1, sz2, sz;
    int need_heap;
    Eterm* hp;
    Sint ires;

    ERTS_FP_CHECK_INIT(p);
    switch (arg1 & _TAG_PRIMARY_MASK) {
    case TAG_PRIMARY_IMMED1:
	switch ((arg1 & _TAG_IMMED1_MASK) >> _TAG_PRIMARY_SIZE) {
	case (_TAG_IMMED1_SMALL >> _TAG_PRIMARY_SIZE):
	    switch (arg2 & _TAG_PRIMARY_MASK) {
	    case TAG_PRIMARY_IMMED1:
		switch ((arg2 & _TAG_IMMED1_MASK) >> _TAG_PRIMARY_SIZE) {
		case (_TAG_IMMED1_SMALL >> _TAG_PRIMARY_SIZE):
		    ires = signed_val(arg1) + signed_val(arg2);
		    ASSERT(MY_IS_SSMALL(ires) == IS_SSMALL(ires));
		    if (MY_IS_SSMALL(ires)) {
			return make_small(ires);
		    } else {
			hp = HAlloc(p, 2);
			res = small_to_big(ires, hp);
			return res;
		    }
		default:
		badarith:
		    p->freason = BADARITH;
		    return THE_NON_VALUE;
		}
	    case TAG_PRIMARY_BOXED:
		hdr = *boxed_val(arg2);
		switch ((hdr & _TAG_HEADER_MASK) >> _TAG_PRIMARY_SIZE) {
		case (_TAG_HEADER_POS_BIG >> _TAG_PRIMARY_SIZE):
		case (_TAG_HEADER_NEG_BIG >> _TAG_PRIMARY_SIZE):
		    if (arg1 == SMALL_ZERO) {
			return arg2;
		    }
		    arg1 = small_to_big(signed_val(arg1), tmp_big1);
		    goto do_big;
		case (_TAG_HEADER_FLOAT >> _TAG_PRIMARY_SIZE):
		    f1.fd = signed_val(arg1);
		    GET_DOUBLE(arg2, f2);
		    goto do_float;
		default:
		    goto badarith;
		}
	    }
	default:
	    goto badarith;
	}
    case TAG_PRIMARY_BOXED:
	hdr = *boxed_val(arg1);
	switch ((hdr & _TAG_HEADER_MASK) >> _TAG_PRIMARY_SIZE) {
	case (_TAG_HEADER_POS_BIG >> _TAG_PRIMARY_SIZE):
	case (_TAG_HEADER_NEG_BIG >> _TAG_PRIMARY_SIZE):
	    switch (arg2 & _TAG_PRIMARY_MASK) {
	    case (_TAG_IMMED1_SMALL >> _TAG_PRIMARY_SIZE):
		switch ((arg2 & _TAG_IMMED1_MASK) >> _TAG_PRIMARY_SIZE) {
		case (_TAG_IMMED1_SMALL >> _TAG_PRIMARY_SIZE):
		    if (arg2 == SMALL_ZERO) {
			return arg1;
		    }
		    arg2 = small_to_big(signed_val(arg2), tmp_big2);
		    goto do_big;
		default:
		    goto badarith;
		}
	    case TAG_PRIMARY_BOXED:
		hdr = *boxed_val(arg2);
		switch ((hdr & _TAG_HEADER_MASK) >> _TAG_PRIMARY_SIZE) {
		case (_TAG_HEADER_POS_BIG >> _TAG_PRIMARY_SIZE):
		case (_TAG_HEADER_NEG_BIG >> _TAG_PRIMARY_SIZE):
		do_big:
		    sz1 = big_size(arg1);
		    sz2 = big_size(arg2);
		    sz = MAX(sz1, sz2)+1;
		    need_heap = BIG_NEED_SIZE(sz);
		    hp = HAlloc(p, need_heap);
		    res = big_plus(arg1, arg2, hp);
		    maybe_shrink(p, hp, res, need_heap);
		    if (is_nil(res)) {
			p->freason = SYSTEM_LIMIT;
			return THE_NON_VALUE;
		    }
		    return res;
		case (_TAG_HEADER_FLOAT >> _TAG_PRIMARY_SIZE):
		    if (big_to_double(arg1, &f1.fd) < 0) {
			goto badarith;
		    }
		    GET_DOUBLE(arg2, f2);
		    goto do_float;
		default:
		    goto badarith;
		}
	    }
	case (_TAG_HEADER_FLOAT >> _TAG_PRIMARY_SIZE):
	    switch (arg2 & _TAG_PRIMARY_MASK) {
	    case TAG_PRIMARY_IMMED1:
		switch ((arg2 & _TAG_IMMED1_MASK) >> _TAG_PRIMARY_SIZE) {
		case (_TAG_IMMED1_SMALL >> _TAG_PRIMARY_SIZE):
		    GET_DOUBLE(arg1, f1);
		    f2.fd = signed_val(arg2);
		    goto do_float;
		default:
		    goto badarith;
		}
	    case TAG_PRIMARY_BOXED:
		hdr = *boxed_val(arg2);
		switch ((hdr & _TAG_HEADER_MASK) >> _TAG_PRIMARY_SIZE) {
		case (_TAG_HEADER_POS_BIG >> _TAG_PRIMARY_SIZE):
		case (_TAG_HEADER_NEG_BIG >> _TAG_PRIMARY_SIZE):
		    GET_DOUBLE(arg1, f1);
		    if (big_to_double(arg2, &f2.fd) < 0) {
			goto badarith;
		    }
		    goto do_float;
		case (_TAG_HEADER_FLOAT >> _TAG_PRIMARY_SIZE):
		    GET_DOUBLE(arg1, f1);
		    GET_DOUBLE(arg2, f2);

		do_float:
		    f1.fd = f1.fd + f2.fd;
		    ERTS_FP_ERROR(p, f1.fd, goto badarith);
		    hp = HAlloc(p, FLOAT_SIZE_OBJECT);
		    res = make_float(hp);
		    PUT_DOUBLE(f1, hp);
		    return res;
		default:
		    goto badarith;
		}
	    default:
		goto badarith;
	    }
	}
    default:
	goto badarith;
    }
}

Eterm
erts_mixed_minus(Process* p, Eterm arg1, Eterm arg2)
{
    DECLARE_TMP(tmp_big1,0,p);
    DECLARE_TMP(tmp_big2,1,p);
    Eterm hdr;
    Eterm res;
    FloatDef f1, f2;
    dsize_t sz1, sz2, sz;
    int need_heap;
    Eterm* hp;
    Sint ires;

    ERTS_FP_CHECK_INIT(p);
    switch (arg1 & _TAG_PRIMARY_MASK) {
    case TAG_PRIMARY_IMMED1:
	switch ((arg1 & _TAG_IMMED1_MASK) >> _TAG_PRIMARY_SIZE) {
	case (_TAG_IMMED1_SMALL >> _TAG_PRIMARY_SIZE):
	    switch (arg2 & _TAG_PRIMARY_MASK) {
	    case TAG_PRIMARY_IMMED1:
		switch ((arg2 & _TAG_IMMED1_MASK) >> _TAG_PRIMARY_SIZE) {
		case (_TAG_IMMED1_SMALL >> _TAG_PRIMARY_SIZE):
		    ires = signed_val(arg1) - signed_val(arg2);
		    ASSERT(MY_IS_SSMALL(ires) == IS_SSMALL(ires));
		    if (MY_IS_SSMALL(ires)) {
			return make_small(ires);
		    } else {
			hp = HAlloc(p, 2);
			res = small_to_big(ires, hp);
			return res;
		    }
		default:
		badarith:
		    p->freason = BADARITH;
		    return THE_NON_VALUE;
		}
	    case TAG_PRIMARY_BOXED:
		hdr = *boxed_val(arg2);
		switch ((hdr & _TAG_HEADER_MASK) >> _TAG_PRIMARY_SIZE) {
		case (_TAG_HEADER_POS_BIG >> _TAG_PRIMARY_SIZE):
		case (_TAG_HEADER_NEG_BIG >> _TAG_PRIMARY_SIZE):
		    arg1 = small_to_big(signed_val(arg1), tmp_big1);
		    goto do_big;
		case (_TAG_HEADER_FLOAT >> _TAG_PRIMARY_SIZE):
		    f1.fd = signed_val(arg1);
		    GET_DOUBLE(arg2, f2);
		    goto do_float;
		default:
		    goto badarith;
		}
	    }
	default:
	    goto badarith;
	}
    case TAG_PRIMARY_BOXED:
	hdr = *boxed_val(arg1);
	switch ((hdr & _TAG_HEADER_MASK) >> _TAG_PRIMARY_SIZE) {
	case (_TAG_HEADER_POS_BIG >> _TAG_PRIMARY_SIZE):
	case (_TAG_HEADER_NEG_BIG >> _TAG_PRIMARY_SIZE):
	    switch (arg2 & _TAG_PRIMARY_MASK) {
	    case (_TAG_IMMED1_SMALL >> _TAG_PRIMARY_SIZE):
		switch ((arg2 & _TAG_IMMED1_MASK) >> _TAG_PRIMARY_SIZE) {
		case (_TAG_IMMED1_SMALL >> _TAG_PRIMARY_SIZE):
		    if (arg2 == SMALL_ZERO) {
			return arg1;
		    }
		    arg2 = small_to_big(signed_val(arg2), tmp_big2);

		do_big:
		    sz1 = big_size(arg1);
		    sz2 = big_size(arg2);
		    sz = MAX(sz1, sz2)+1;
		    need_heap = BIG_NEED_SIZE(sz);
		    hp = HAlloc(p, need_heap);
		    res = big_minus(arg1, arg2, hp);
                    maybe_shrink(p, hp, res, need_heap);
		    if (is_nil(res)) {
			p->freason = SYSTEM_LIMIT;
			return THE_NON_VALUE;
		    }
		    return res;
		default:
		    goto badarith;
		}
	    case TAG_PRIMARY_BOXED:
		hdr = *boxed_val(arg2);
		switch ((hdr & _TAG_HEADER_MASK) >> _TAG_PRIMARY_SIZE) {
		case (_TAG_HEADER_POS_BIG >> _TAG_PRIMARY_SIZE):
		case (_TAG_HEADER_NEG_BIG >> _TAG_PRIMARY_SIZE):
		    goto do_big;
		case (_TAG_HEADER_FLOAT >> _TAG_PRIMARY_SIZE):
		    if (big_to_double(arg1, &f1.fd) < 0) {
			goto badarith;
		    }
		    GET_DOUBLE(arg2, f2);
		    goto do_float;
		default:
		    goto badarith;
		}
	    }
	case (_TAG_HEADER_FLOAT >> _TAG_PRIMARY_SIZE):
	    switch (arg2 & _TAG_PRIMARY_MASK) {
	    case TAG_PRIMARY_IMMED1:
		switch ((arg2 & _TAG_IMMED1_MASK) >> _TAG_PRIMARY_SIZE) {
		case (_TAG_IMMED1_SMALL >> _TAG_PRIMARY_SIZE):
		    GET_DOUBLE(arg1, f1);
		    f2.fd = signed_val(arg2);
		    goto do_float;
		default:
		    goto badarith;
		}
	    case TAG_PRIMARY_BOXED:
		hdr = *boxed_val(arg2);
		switch ((hdr & _TAG_HEADER_MASK) >> _TAG_PRIMARY_SIZE) {
		case (_TAG_HEADER_POS_BIG >> _TAG_PRIMARY_SIZE):
		case (_TAG_HEADER_NEG_BIG >> _TAG_PRIMARY_SIZE):
		    GET_DOUBLE(arg1, f1);
		    if (big_to_double(arg2, &f2.fd) < 0) {
			goto badarith;
		    }
		    goto do_float;
		case (_TAG_HEADER_FLOAT >> _TAG_PRIMARY_SIZE):
		    GET_DOUBLE(arg1, f1);
		    GET_DOUBLE(arg2, f2);

		do_float:
		    f1.fd = f1.fd - f2.fd;
		    ERTS_FP_ERROR(p, f1.fd, goto badarith);
		    hp = HAlloc(p, FLOAT_SIZE_OBJECT);
		    res = make_float(hp);
		    PUT_DOUBLE(f1, hp);
		    return res;
		default:
		    goto badarith;
		}
	    default:
		goto badarith;
	    }
	}
    default:
	goto badarith;
    }
}

Eterm
erts_mixed_times(Process* p, Eterm arg1, Eterm arg2)
{
    DECLARE_TMP(tmp_big1,0,p);
    DECLARE_TMP(tmp_big2,1,p);
    Eterm hdr;
    Eterm res;
    FloatDef f1, f2;
    dsize_t sz1, sz2, sz;
    int need_heap;
    Eterm* hp;

    ERTS_FP_CHECK_INIT(p);
    switch (arg1 & _TAG_PRIMARY_MASK) {
    case TAG_PRIMARY_IMMED1:
	switch ((arg1 & _TAG_IMMED1_MASK) >> _TAG_PRIMARY_SIZE) {
	case (_TAG_IMMED1_SMALL >> _TAG_PRIMARY_SIZE):
	    switch (arg2 & _TAG_PRIMARY_MASK) {
	    case TAG_PRIMARY_IMMED1:
		switch ((arg2 & _TAG_IMMED1_MASK) >> _TAG_PRIMARY_SIZE) {
		case (_TAG_IMMED1_SMALL >> _TAG_PRIMARY_SIZE):
		    if ((arg1 == SMALL_ZERO) || (arg2 == SMALL_ZERO)) {
			return(SMALL_ZERO);
		    } else if (arg1 == SMALL_ONE) {
			return(arg2);
		    } else if (arg2 == SMALL_ONE) {
			return(arg1);
		    } else {
			DeclareTmpHeap(big_res,3,p);
			UseTmpHeap(3,p);
			/*
			 * The following code is optimized for the case that
			 * result is small (which should be the most common case
			 * in practice).
			 */
			res = small_times(signed_val(arg1), signed_val(arg2), big_res);
			if (is_small(res)) {
			    UnUseTmpHeap(3,p);
			    return res;
			} else {
			    /*
			     * The result is a a big number.
			     * Allocate a heap fragment and copy the result.
			     * Be careful to allocate exactly what we need
			     * to not leave any holes.
			     */
			    Uint arity;
			    
			    ASSERT(is_big(res));
			    hdr = big_res[0];
			    arity = bignum_header_arity(hdr);
			    ASSERT(arity == 1 || arity == 2);
			    hp = HAlloc(p, arity+1);
			    res = make_big(hp);
			    *hp++ = hdr;
			    *hp++ = big_res[1];
			    if (arity > 1) {
				*hp = big_res[2];
			    }
			    UnUseTmpHeap(3,p);
			    return res;
			}
		    }
		default:
		badarith:
		    p->freason = BADARITH;
		    return THE_NON_VALUE;
		}
	    case TAG_PRIMARY_BOXED:
		hdr = *boxed_val(arg2);
		switch ((hdr & _TAG_HEADER_MASK) >> _TAG_PRIMARY_SIZE) {
		case (_TAG_HEADER_POS_BIG >> _TAG_PRIMARY_SIZE):
		case (_TAG_HEADER_NEG_BIG >> _TAG_PRIMARY_SIZE):
		    if (arg1 == SMALL_ZERO)
			return(SMALL_ZERO);
		    if (arg1 == SMALL_ONE)
			return(arg2);
		    arg1 = small_to_big(signed_val(arg1), tmp_big1);
		    sz = 2 + big_size(arg2);
		    goto do_big;
		case (_TAG_HEADER_FLOAT >> _TAG_PRIMARY_SIZE):
		    f1.fd = signed_val(arg1);
		    GET_DOUBLE(arg2, f2);
		    goto do_float;
		default:
		    goto badarith;
		}
	    }
	default:
	    goto badarith;
	}
    case TAG_PRIMARY_BOXED:
	hdr = *boxed_val(arg1);
	switch ((hdr & _TAG_HEADER_MASK) >> _TAG_PRIMARY_SIZE) {
	case (_TAG_HEADER_POS_BIG >> _TAG_PRIMARY_SIZE):
	case (_TAG_HEADER_NEG_BIG >> _TAG_PRIMARY_SIZE):
	    switch (arg2 & _TAG_PRIMARY_MASK) {
	    case (_TAG_IMMED1_SMALL >> _TAG_PRIMARY_SIZE):
		switch ((arg2 & _TAG_IMMED1_MASK) >> _TAG_PRIMARY_SIZE) {
		case (_TAG_IMMED1_SMALL >> _TAG_PRIMARY_SIZE):
		    if (arg2 == SMALL_ZERO)
			return(SMALL_ZERO);
		    if (arg2 == SMALL_ONE)
			return(arg1);
		    arg2 = small_to_big(signed_val(arg2), tmp_big2);
		    sz = 2 + big_size(arg1);
		    goto do_big;
		default:
		    goto badarith;
		}
	    case TAG_PRIMARY_BOXED:
		hdr = *boxed_val(arg2);
		switch ((hdr & _TAG_HEADER_MASK) >> _TAG_PRIMARY_SIZE) {
		case (_TAG_HEADER_POS_BIG >> _TAG_PRIMARY_SIZE):
		case (_TAG_HEADER_NEG_BIG >> _TAG_PRIMARY_SIZE):
		    sz1 = big_size(arg1);
		    sz2 = big_size(arg2);
		    sz = sz1 + sz2;

		do_big:
		    need_heap = BIG_NEED_SIZE(sz);
                    hp = HAlloc(p, need_heap);
		    res = big_times(arg1, arg2, hp);

		    /*
		     * Note that the result must be big in this case, since
		     * at least one operand was big to begin with, and
		     * the absolute value of the other is > 1.
		     */

                    maybe_shrink(p, hp, res, need_heap);
		    if (is_nil(res)) {
			p->freason = SYSTEM_LIMIT;
			return THE_NON_VALUE;
		    }		    
		    return res;
		case (_TAG_HEADER_FLOAT >> _TAG_PRIMARY_SIZE):
		    if (big_to_double(arg1, &f1.fd) < 0) {
			goto badarith;
		    }
		    GET_DOUBLE(arg2, f2);
		    goto do_float;
		default:
		    goto badarith;
		}
	    }
	case (_TAG_HEADER_FLOAT >> _TAG_PRIMARY_SIZE):
	    switch (arg2 & _TAG_PRIMARY_MASK) {
	    case TAG_PRIMARY_IMMED1:
		switch ((arg2 & _TAG_IMMED1_MASK) >> _TAG_PRIMARY_SIZE) {
		case (_TAG_IMMED1_SMALL >> _TAG_PRIMARY_SIZE):
		    GET_DOUBLE(arg1, f1);
		    f2.fd = signed_val(arg2);
		    goto do_float;
		default:
		    goto badarith;
		}
	    case TAG_PRIMARY_BOXED:
		hdr = *boxed_val(arg2);
		switch ((hdr & _TAG_HEADER_MASK) >> _TAG_PRIMARY_SIZE) {
		case (_TAG_HEADER_POS_BIG >> _TAG_PRIMARY_SIZE):
		case (_TAG_HEADER_NEG_BIG >> _TAG_PRIMARY_SIZE):
		    GET_DOUBLE(arg1, f1);
		    if (big_to_double(arg2, &f2.fd) < 0) {
			goto badarith;
		    }
		    goto do_float;
		case (_TAG_HEADER_FLOAT >> _TAG_PRIMARY_SIZE):
		    GET_DOUBLE(arg1, f1);
		    GET_DOUBLE(arg2, f2);

		do_float:
		    f1.fd = f1.fd * f2.fd;
		    ERTS_FP_ERROR(p, f1.fd, goto badarith);
		    hp = HAlloc(p, FLOAT_SIZE_OBJECT);
		    res = make_float(hp);
		    PUT_DOUBLE(f1, hp);
		    return res;
		default:
		    goto badarith;
		}
	    default:
		goto badarith;
	    }
	}
    default:
	goto badarith;
    }
}

Eterm
erts_mixed_div(Process* p, Eterm arg1, Eterm arg2)
{
    FloatDef f1, f2;
    Eterm* hp;
    Eterm hdr;

    ERTS_FP_CHECK_INIT(p);
    switch (arg1 & _TAG_PRIMARY_MASK) {
    case TAG_PRIMARY_IMMED1:
	switch ((arg1 & _TAG_IMMED1_MASK) >> _TAG_PRIMARY_SIZE) {
	case (_TAG_IMMED1_SMALL >> _TAG_PRIMARY_SIZE):
	    switch (arg2 & _TAG_PRIMARY_MASK) {
	    case TAG_PRIMARY_IMMED1:
		switch ((arg2 & _TAG_IMMED1_MASK) >> _TAG_PRIMARY_SIZE) {
		case (_TAG_IMMED1_SMALL >> _TAG_PRIMARY_SIZE):
		    f1.fd = signed_val(arg1);
		    f2.fd = signed_val(arg2);
		    goto do_float;
		default:
		badarith:
		    p->freason = BADARITH;
		    return THE_NON_VALUE;
		}
	    case TAG_PRIMARY_BOXED:
		hdr = *boxed_val(arg2);
		switch ((hdr & _TAG_HEADER_MASK) >> _TAG_PRIMARY_SIZE) {
		case (_TAG_HEADER_POS_BIG >> _TAG_PRIMARY_SIZE):
		case (_TAG_HEADER_NEG_BIG >> _TAG_PRIMARY_SIZE):
		    f1.fd = signed_val(arg1);
		    if (big_to_double(arg2, &f2.fd) < 0) {
			goto badarith;
		    }
		    goto do_float;
		case (_TAG_HEADER_FLOAT >> _TAG_PRIMARY_SIZE):
		    f1.fd = signed_val(arg1);
		    GET_DOUBLE(arg2, f2);
		    goto do_float;
		default:
		    goto badarith;
		}
	    }
	default:
	    goto badarith;
	}
    case TAG_PRIMARY_BOXED:
	hdr = *boxed_val(arg1);
	switch ((hdr & _TAG_HEADER_MASK) >> _TAG_PRIMARY_SIZE) {
	case (_TAG_HEADER_POS_BIG >> _TAG_PRIMARY_SIZE):
	case (_TAG_HEADER_NEG_BIG >> _TAG_PRIMARY_SIZE):
	    switch (arg2 & _TAG_PRIMARY_MASK) {
	    case (_TAG_IMMED1_SMALL >> _TAG_PRIMARY_SIZE):
		switch ((arg2 & _TAG_IMMED1_MASK) >> _TAG_PRIMARY_SIZE) {
		case (_TAG_IMMED1_SMALL >> _TAG_PRIMARY_SIZE):
		    if (big_to_double(arg1, &f1.fd) < 0) {
			goto badarith;
		    }
		    f2.fd = signed_val(arg2);
		    goto do_float;
		default:
		    goto badarith;
		}
	    case TAG_PRIMARY_BOXED:
		hdr = *boxed_val(arg2);
		switch ((hdr & _TAG_HEADER_MASK) >> _TAG_PRIMARY_SIZE) {
		case (_TAG_HEADER_POS_BIG >> _TAG_PRIMARY_SIZE):
		case (_TAG_HEADER_NEG_BIG >> _TAG_PRIMARY_SIZE):
		    if (big_to_double(arg1, &f1.fd) < 0 ||
			big_to_double(arg2, &f2.fd) < 0) {
			goto badarith;
		    }
		    goto do_float;
		case (_TAG_HEADER_FLOAT >> _TAG_PRIMARY_SIZE):
		    if (big_to_double(arg1, &f1.fd) < 0) {
			goto badarith;
		    }
		    GET_DOUBLE(arg2, f2);
		    goto do_float;
		default:
		    goto badarith;
		}
	    }
	case (_TAG_HEADER_FLOAT >> _TAG_PRIMARY_SIZE):
	    switch (arg2 & _TAG_PRIMARY_MASK) {
	    case TAG_PRIMARY_IMMED1:
		switch ((arg2 & _TAG_IMMED1_MASK) >> _TAG_PRIMARY_SIZE) {
		case (_TAG_IMMED1_SMALL >> _TAG_PRIMARY_SIZE):
		    GET_DOUBLE(arg1, f1);
		    f2.fd = signed_val(arg2);
		    goto do_float;
		default:
		    goto badarith;
		}
	    case TAG_PRIMARY_BOXED:
		hdr = *boxed_val(arg2);
		switch ((hdr & _TAG_HEADER_MASK) >> _TAG_PRIMARY_SIZE) {
		case (_TAG_HEADER_POS_BIG >> _TAG_PRIMARY_SIZE):
		case (_TAG_HEADER_NEG_BIG >> _TAG_PRIMARY_SIZE):
		    GET_DOUBLE(arg1, f1);
		    if (big_to_double(arg2, &f2.fd) < 0) {
			goto badarith;
		    }
		    goto do_float;
		case (_TAG_HEADER_FLOAT >> _TAG_PRIMARY_SIZE):
		    GET_DOUBLE(arg1, f1);
		    GET_DOUBLE(arg2, f2);

		do_float:
		    f1.fd = f1.fd / f2.fd;
		    ERTS_FP_ERROR(p, f1.fd, goto badarith);
		    hp = HAlloc(p, FLOAT_SIZE_OBJECT);
		    PUT_DOUBLE(f1, hp);
		    return make_float(hp);
		default:
		    goto badarith;
		}
	    default:
		goto badarith;
	    }
	}
    default:
	goto badarith;
    }
}

Eterm
erts_int_div(Process* p, Eterm arg1, Eterm arg2)
{
    DECLARE_TMP(tmp_big1,0,p);
    DECLARE_TMP(tmp_big2,1,p);
    int ires;

    switch (NUMBER_CODE(arg1, arg2)) {
    case SMALL_SMALL:
	/* This case occurs if the most negative fixnum is divided by -1. */
	ASSERT(arg2 == make_small(-1));
	arg1 = small_to_big(signed_val(arg1), tmp_big1);
	/*FALLTHROUGH*/
    case BIG_SMALL:
	arg2 = small_to_big(signed_val(arg2), tmp_big2);
	goto L_big_div;
    case SMALL_BIG:
	if (arg1 != make_small(MIN_SMALL)) {
	    return SMALL_ZERO;
	}
	arg1 = small_to_big(signed_val(arg1), tmp_big1);
	/*FALLTHROUGH*/
    case BIG_BIG:
    L_big_div:
	ires = big_ucomp(arg1, arg2);
	if (ires < 0) {
	    arg1 = SMALL_ZERO;
	} else if (ires == 0) {
	    arg1 = (big_sign(arg1) == big_sign(arg2)) ?
		SMALL_ONE : SMALL_MINUS_ONE;
	} else {
	    Eterm* hp;
	    int i = big_size(arg1);
	    Uint need;

	    ires = big_size(arg2);
	    need = BIG_NEED_SIZE(i-ires+1) + BIG_NEED_SIZE(i);
	    hp = HAlloc(p, need);
	    arg1 = big_div(arg1, arg2, hp);
	    maybe_shrink(p, hp, arg1, need);
	    if (is_nil(arg1)) {
		p->freason = SYSTEM_LIMIT;
		return THE_NON_VALUE;
	    }
	}
	return arg1;
    default:
	p->freason = BADARITH;
	return THE_NON_VALUE;
    }
}

Eterm
erts_int_rem(Process* p, Eterm arg1, Eterm arg2)
{
    DECLARE_TMP(tmp_big1,0,p);
    DECLARE_TMP(tmp_big2,1,p);
    int ires;

    switch (NUMBER_CODE(arg1, arg2)) {
    case BIG_SMALL:
	arg2 = small_to_big(signed_val(arg2), tmp_big2);
	goto L_big_rem;
    case SMALL_BIG:
	if (arg1 != make_small(MIN_SMALL)) {
	    return arg1;
	} else {
	    Eterm tmp;
	    tmp = small_to_big(signed_val(arg1), tmp_big1);
	    if ((ires = big_ucomp(tmp, arg2)) == 0) {
		return SMALL_ZERO;
	    } else {
		ASSERT(ires < 0);
		return arg1;
	    }
	}
	/* All paths returned */
    case BIG_BIG:
    L_big_rem:
	ires = big_ucomp(arg1, arg2);
	if (ires == 0) {
	    arg1 = SMALL_ZERO;
	} else if (ires > 0) {
	    Uint need = BIG_NEED_SIZE(big_size(arg1));
	    Eterm* hp = HAlloc(p, need);

	    arg1 = big_rem(arg1, arg2, hp);
	    maybe_shrink(p, hp, arg1, need);
	    if (is_nil(arg1)) {
		p->freason = SYSTEM_LIMIT;
		return THE_NON_VALUE;
	    }
	}
	return arg1;
    default:
	p->freason = BADARITH;
	return THE_NON_VALUE;
    }
}

Eterm erts_band(Process* p, Eterm arg1, Eterm arg2)
{
    DECLARE_TMP(tmp_big1,0,p);
    DECLARE_TMP(tmp_big2,1,p);
    Eterm* hp;
    int need;

    switch (NUMBER_CODE(arg1, arg2)) {
    case SMALL_BIG:
	arg1 = small_to_big(signed_val(arg1), tmp_big1);
	break;
    case BIG_SMALL:
	arg2 = small_to_big(signed_val(arg2), tmp_big2);
	break;
    case BIG_BIG:
	break;
    default:
	p->freason = BADARITH;
	return THE_NON_VALUE;
    }
    need = BIG_NEED_SIZE(MAX(big_size(arg1), big_size(arg2)) + 1);
    hp = HAlloc(p, need);
    arg1 = big_band(arg1, arg2, hp);
    ASSERT(is_not_nil(arg1));
    maybe_shrink(p, hp, arg1, need);
    return arg1;
}

Eterm erts_bor(Process* p, Eterm arg1, Eterm arg2)
{
    DECLARE_TMP(tmp_big1,0,p);
    DECLARE_TMP(tmp_big2,1,p);
    Eterm* hp;
    int need;

    switch (NUMBER_CODE(arg1, arg2)) {
    case SMALL_BIG:
	arg1 = small_to_big(signed_val(arg1), tmp_big1);
	break;
    case BIG_SMALL:
	arg2 = small_to_big(signed_val(arg2), tmp_big2);
	break;
    case BIG_BIG:
	break;
    default:
	p->freason = BADARITH;
	return THE_NON_VALUE;
    }
    need = BIG_NEED_SIZE(MAX(big_size(arg1), big_size(arg2)) + 1);
    hp = HAlloc(p, need);
    arg1 = big_bor(arg1, arg2, hp);
    ASSERT(is_not_nil(arg1));
    maybe_shrink(p, hp, arg1, need);
    return arg1;
}

Eterm erts_bxor(Process* p, Eterm arg1, Eterm arg2)
{
    DECLARE_TMP(tmp_big1,0,p);
    DECLARE_TMP(tmp_big2,1,p);
    Eterm* hp;
    int need;

    switch (NUMBER_CODE(arg1, arg2)) {
    case SMALL_BIG:
	arg1 = small_to_big(signed_val(arg1), tmp_big1);
	break;
    case BIG_SMALL:
	arg2 = small_to_big(signed_val(arg2), tmp_big2);
	break;
    case BIG_BIG:
	break;
    default:
	p->freason = BADARITH;
	return THE_NON_VALUE;
    }
    need = BIG_NEED_SIZE(MAX(big_size(arg1), big_size(arg2)) + 1);
    hp = HAlloc(p, need);
    arg1 = big_bxor(arg1, arg2, hp);
    ASSERT(is_not_nil(arg1));
    maybe_shrink(p, hp, arg1, need);
    return arg1;
}

Eterm erts_bnot(Process* p, Eterm arg)
{
    Eterm ret;

    if (is_big(arg)) {
	Uint need = BIG_NEED_SIZE(big_size(arg)+1);
	Eterm* bigp = HAlloc(p, need);

	ret = big_bnot(arg, bigp);
	maybe_shrink(p, bigp, ret, need);
	if (is_nil(ret)) {
	    p->freason = SYSTEM_LIMIT;
	    return NIL;
	}
    } else {
	p->freason = BADARITH;
	return NIL;
    }
    return ret;
} 

#define ERTS_NEED_GC(p, need) ((HEAP_LIMIT((p)) - HEAP_TOP((p))) <= (need))

static ERTS_INLINE void
trim_heap(Process* p, Eterm* hp, Eterm res)
{
    if (is_immed(res)) {
	ASSERT(p->heap <= hp && hp <= p->htop);
	p->htop = hp;
    } else {
	Eterm* new_htop;
	ASSERT(is_big(res));
	new_htop = hp + bignum_header_arity(*hp) + 1;
	ASSERT(p->heap <= new_htop && new_htop <= p->htop);
	p->htop = new_htop;
    }
    ASSERT(p->heap <= p->htop && p->htop <= p->stop);
}

/*
 * The functions that follow are called from the emulator loop.
 * They are not allowed to allocate heap fragments, but must do
 * a garbage collection if there is insufficient heap space.
 */

#define erts_heap_frag_shrink horrible error
#define maybe_shrink horrible error

Eterm
erts_gc_mixed_plus(Process* p, Eterm* reg, Uint live)
{
    Eterm arg1;
    Eterm arg2;
    DECLARE_TMP(tmp_big1,0,p);
    DECLARE_TMP(tmp_big2,1,p);
    Eterm res;
    Eterm hdr;
    FloatDef f1, f2;
    dsize_t sz1, sz2, sz;
    int need_heap;
    Eterm* hp;
    Sint ires;

    arg1 = reg[live];
    arg2 = reg[live+1];
    ERTS_FP_CHECK_INIT(p);
    switch (arg1 & _TAG_PRIMARY_MASK) {
    case TAG_PRIMARY_IMMED1:
	switch ((arg1 & _TAG_IMMED1_MASK) >> _TAG_PRIMARY_SIZE) {
	case (_TAG_IMMED1_SMALL >> _TAG_PRIMARY_SIZE):
	    switch (arg2 & _TAG_PRIMARY_MASK) {
	    case TAG_PRIMARY_IMMED1:
		switch ((arg2 & _TAG_IMMED1_MASK) >> _TAG_PRIMARY_SIZE) {
		case (_TAG_IMMED1_SMALL >> _TAG_PRIMARY_SIZE):
		    ires = signed_val(arg1) + signed_val(arg2);
		    ASSERT(MY_IS_SSMALL(ires) == IS_SSMALL(ires));
		    if (MY_IS_SSMALL(ires)) {
			return make_small(ires);
		    } else {
			if (ERTS_NEED_GC(p, 2)) {
			    erts_garbage_collect(p, 2, reg, live);
			}
			hp = p->htop;
			p->htop += 2;
			res = small_to_big(ires, hp);
			return res;
		    }
		default:
		badarith:
		    p->freason = BADARITH;
		    return THE_NON_VALUE;
		}
	    case TAG_PRIMARY_BOXED:
		hdr = *boxed_val(arg2);
		switch ((hdr & _TAG_HEADER_MASK) >> _TAG_PRIMARY_SIZE) {
		case (_TAG_HEADER_POS_BIG >> _TAG_PRIMARY_SIZE):
		case (_TAG_HEADER_NEG_BIG >> _TAG_PRIMARY_SIZE):
		    if (arg1 == SMALL_ZERO) {
			return arg2;
		    }
		    arg1 = small_to_big(signed_val(arg1), tmp_big1);
		    goto do_big;
		case (_TAG_HEADER_FLOAT >> _TAG_PRIMARY_SIZE):
		    f1.fd = signed_val(arg1);
		    GET_DOUBLE(arg2, f2);
		    goto do_float;
		default:
		    goto badarith;
		}
	    }
	default:
	    goto badarith;
	}
    case TAG_PRIMARY_BOXED:
	hdr = *boxed_val(arg1);
	switch ((hdr & _TAG_HEADER_MASK) >> _TAG_PRIMARY_SIZE) {
	case (_TAG_HEADER_POS_BIG >> _TAG_PRIMARY_SIZE):
	case (_TAG_HEADER_NEG_BIG >> _TAG_PRIMARY_SIZE):
	    switch (arg2 & _TAG_PRIMARY_MASK) {
	    case (_TAG_IMMED1_SMALL >> _TAG_PRIMARY_SIZE):
		switch ((arg2 & _TAG_IMMED1_MASK) >> _TAG_PRIMARY_SIZE) {
		case (_TAG_IMMED1_SMALL >> _TAG_PRIMARY_SIZE):
		    if (arg2 == SMALL_ZERO) {
			return arg1;
		    }
		    arg2 = small_to_big(signed_val(arg2), tmp_big2);
		    goto do_big;
		default:
		    goto badarith;
		}
	    case TAG_PRIMARY_BOXED:
		hdr = *boxed_val(arg2);
		switch ((hdr & _TAG_HEADER_MASK) >> _TAG_PRIMARY_SIZE) {
		case (_TAG_HEADER_POS_BIG >> _TAG_PRIMARY_SIZE):
		case (_TAG_HEADER_NEG_BIG >> _TAG_PRIMARY_SIZE):
		do_big:
		    sz1 = big_size(arg1);
		    sz2 = big_size(arg2);
		    sz = MAX(sz1, sz2)+1;
		    need_heap = BIG_NEED_SIZE(sz);
		    if (ERTS_NEED_GC(p, need_heap)) {
			erts_garbage_collect(p, need_heap, reg, live+2);
			if (ARG_IS_NOT_TMP(arg1,tmp_big1)) {
			    arg1 = reg[live];
			}
			if (ARG_IS_NOT_TMP(arg2,tmp_big2)) {
			    arg2 = reg[live+1];
			}
		    }
		    hp = p->htop;
		    p->htop += need_heap;
		    res = big_plus(arg1, arg2, hp);
		    trim_heap(p, hp, res);
		    if (is_nil(res)) {
			p->freason = SYSTEM_LIMIT;
			return THE_NON_VALUE;
		    }
		    return res;
		case (_TAG_HEADER_FLOAT >> _TAG_PRIMARY_SIZE):
		    if (big_to_double(arg1, &f1.fd) < 0) {
			goto badarith;
		    }
		    GET_DOUBLE(arg2, f2);
		    goto do_float;
		default:
		    goto badarith;
		}
	    }
	case (_TAG_HEADER_FLOAT >> _TAG_PRIMARY_SIZE):
	    switch (arg2 & _TAG_PRIMARY_MASK) {
	    case TAG_PRIMARY_IMMED1:
		switch ((arg2 & _TAG_IMMED1_MASK) >> _TAG_PRIMARY_SIZE) {
		case (_TAG_IMMED1_SMALL >> _TAG_PRIMARY_SIZE):
		    GET_DOUBLE(arg1, f1);
		    f2.fd = signed_val(arg2);
		    goto do_float;
		default:
		    goto badarith;
		}
	    case TAG_PRIMARY_BOXED:
		hdr = *boxed_val(arg2);
		switch ((hdr & _TAG_HEADER_MASK) >> _TAG_PRIMARY_SIZE) {
		case (_TAG_HEADER_POS_BIG >> _TAG_PRIMARY_SIZE):
		case (_TAG_HEADER_NEG_BIG >> _TAG_PRIMARY_SIZE):
		    GET_DOUBLE(arg1, f1);
		    if (big_to_double(arg2, &f2.fd) < 0) {
			goto badarith;
		    }
		    goto do_float;
		case (_TAG_HEADER_FLOAT >> _TAG_PRIMARY_SIZE):
		    GET_DOUBLE(arg1, f1);
		    GET_DOUBLE(arg2, f2);

		do_float:
		    f1.fd = f1.fd + f2.fd;
		    ERTS_FP_ERROR(p, f1.fd, goto badarith);
		    if (ERTS_NEED_GC(p, FLOAT_SIZE_OBJECT)) {
			erts_garbage_collect(p, FLOAT_SIZE_OBJECT, reg, live);
		    }
		    hp = p->htop;
		    p->htop += FLOAT_SIZE_OBJECT;
		    res = make_float(hp);
		    PUT_DOUBLE(f1, hp);
		    return res;
		default:
		    goto badarith;
		}
	    default:
		goto badarith;
	    }
	}
    default:
	goto badarith;
    }
}

Eterm
erts_gc_mixed_minus(Process* p, Eterm* reg, Uint live)
{
    Eterm arg1;
    Eterm arg2;
    DECLARE_TMP(tmp_big1,0,p);
    DECLARE_TMP(tmp_big2,1,p);
    Eterm hdr;
    Eterm res;
    FloatDef f1, f2;
    dsize_t sz1, sz2, sz;
    int need_heap;
    Eterm* hp;
    Sint ires;

    arg1 = reg[live];
    arg2 = reg[live+1];
    ERTS_FP_CHECK_INIT(p);
    switch (arg1 & _TAG_PRIMARY_MASK) {
    case TAG_PRIMARY_IMMED1:
	switch ((arg1 & _TAG_IMMED1_MASK) >> _TAG_PRIMARY_SIZE) {
	case (_TAG_IMMED1_SMALL >> _TAG_PRIMARY_SIZE):
	    switch (arg2 & _TAG_PRIMARY_MASK) {
	    case TAG_PRIMARY_IMMED1:
		switch ((arg2 & _TAG_IMMED1_MASK) >> _TAG_PRIMARY_SIZE) {
		case (_TAG_IMMED1_SMALL >> _TAG_PRIMARY_SIZE):
		    ires = signed_val(arg1) - signed_val(arg2);
		    ASSERT(MY_IS_SSMALL(ires) == IS_SSMALL(ires));
		    if (MY_IS_SSMALL(ires)) {
			return make_small(ires);
		    } else {
			if (ERTS_NEED_GC(p, 2)) {
			    erts_garbage_collect(p, 2, reg, live);
			}
			hp = p->htop;
			p->htop += 2;
			res = small_to_big(ires, hp);
			return res;
		    }
		default:
		badarith:
		    p->freason = BADARITH;
		    return THE_NON_VALUE;
		}
	    case TAG_PRIMARY_BOXED:
		hdr = *boxed_val(arg2);
		switch ((hdr & _TAG_HEADER_MASK) >> _TAG_PRIMARY_SIZE) {
		case (_TAG_HEADER_POS_BIG >> _TAG_PRIMARY_SIZE):
		case (_TAG_HEADER_NEG_BIG >> _TAG_PRIMARY_SIZE):
		    arg1 = small_to_big(signed_val(arg1), tmp_big1);
		    goto do_big;
		case (_TAG_HEADER_FLOAT >> _TAG_PRIMARY_SIZE):
		    f1.fd = signed_val(arg1);
		    GET_DOUBLE(arg2, f2);
		    goto do_float;
		default:
		    goto badarith;
		}
	    }
	default:
	    goto badarith;
	}
    case TAG_PRIMARY_BOXED:
	hdr = *boxed_val(arg1);
	switch ((hdr & _TAG_HEADER_MASK) >> _TAG_PRIMARY_SIZE) {
	case (_TAG_HEADER_POS_BIG >> _TAG_PRIMARY_SIZE):
	case (_TAG_HEADER_NEG_BIG >> _TAG_PRIMARY_SIZE):
	    switch (arg2 & _TAG_PRIMARY_MASK) {
	    case (_TAG_IMMED1_SMALL >> _TAG_PRIMARY_SIZE):
		switch ((arg2 & _TAG_IMMED1_MASK) >> _TAG_PRIMARY_SIZE) {
		case (_TAG_IMMED1_SMALL >> _TAG_PRIMARY_SIZE):
		    if (arg2 == SMALL_ZERO) {
			return arg1;
		    }
		    arg2 = small_to_big(signed_val(arg2), tmp_big2);

		do_big:
		    sz1 = big_size(arg1);
		    sz2 = big_size(arg2);
		    sz = MAX(sz1, sz2)+1;
		    need_heap = BIG_NEED_SIZE(sz);
		    if (ERTS_NEED_GC(p, need_heap)) {
			erts_garbage_collect(p, need_heap, reg, live+2);
			if (ARG_IS_NOT_TMP(arg1,tmp_big1)) {
			    arg1 = reg[live];
			}
			if (ARG_IS_NOT_TMP(arg2,tmp_big2)) {
			    arg2 = reg[live+1];
			}
		    }
		    hp = p->htop;
		    p->htop += need_heap;
		    res = big_minus(arg1, arg2, hp);
                    trim_heap(p, hp, res);
		    if (is_nil(res)) {
			p->freason = SYSTEM_LIMIT;
			return THE_NON_VALUE;
		    }
		    return res;
		default:
		    goto badarith;
		}
	    case TAG_PRIMARY_BOXED:
		hdr = *boxed_val(arg2);
		switch ((hdr & _TAG_HEADER_MASK) >> _TAG_PRIMARY_SIZE) {
		case (_TAG_HEADER_POS_BIG >> _TAG_PRIMARY_SIZE):
		case (_TAG_HEADER_NEG_BIG >> _TAG_PRIMARY_SIZE):
		    goto do_big;
		case (_TAG_HEADER_FLOAT >> _TAG_PRIMARY_SIZE):
		    if (big_to_double(arg1, &f1.fd) < 0) {
			goto badarith;
		    }
		    GET_DOUBLE(arg2, f2);
		    goto do_float;
		default:
		    goto badarith;
		}
	    }
	case (_TAG_HEADER_FLOAT >> _TAG_PRIMARY_SIZE):
	    switch (arg2 & _TAG_PRIMARY_MASK) {
	    case TAG_PRIMARY_IMMED1:
		switch ((arg2 & _TAG_IMMED1_MASK) >> _TAG_PRIMARY_SIZE) {
		case (_TAG_IMMED1_SMALL >> _TAG_PRIMARY_SIZE):
		    GET_DOUBLE(arg1, f1);
		    f2.fd = signed_val(arg2);
		    goto do_float;
		default:
		    goto badarith;
		}
	    case TAG_PRIMARY_BOXED:
		hdr = *boxed_val(arg2);
		switch ((hdr & _TAG_HEADER_MASK) >> _TAG_PRIMARY_SIZE) {
		case (_TAG_HEADER_POS_BIG >> _TAG_PRIMARY_SIZE):
		case (_TAG_HEADER_NEG_BIG >> _TAG_PRIMARY_SIZE):
		    GET_DOUBLE(arg1, f1);
		    if (big_to_double(arg2, &f2.fd) < 0) {
			goto badarith;
		    }
		    goto do_float;
		case (_TAG_HEADER_FLOAT >> _TAG_PRIMARY_SIZE):
		    GET_DOUBLE(arg1, f1);
		    GET_DOUBLE(arg2, f2);

		do_float:
		    f1.fd = f1.fd - f2.fd;
		    ERTS_FP_ERROR(p, f1.fd, goto badarith);
		    if (ERTS_NEED_GC(p, FLOAT_SIZE_OBJECT)) {
			erts_garbage_collect(p, FLOAT_SIZE_OBJECT, reg, live);
		    }
		    hp = p->htop;
		    p->htop += FLOAT_SIZE_OBJECT;
		    res = make_float(hp);
		    PUT_DOUBLE(f1, hp);
		    return res;
		default:
		    goto badarith;
		}
	    default:
		goto badarith;
	    }
	}
    default:
	goto badarith;
    }
}

Eterm
erts_gc_mixed_times(Process* p, Eterm* reg, Uint live)
{
    Eterm arg1;
    Eterm arg2;
    DECLARE_TMP(tmp_big1,0,p);
    DECLARE_TMP(tmp_big2,1,p);
    Eterm hdr;
    Eterm res;
    FloatDef f1, f2;
    dsize_t sz1, sz2, sz;
    int need_heap;
    Eterm* hp;

    arg1 = reg[live];
    arg2 = reg[live+1];
    ERTS_FP_CHECK_INIT(p);
    switch (arg1 & _TAG_PRIMARY_MASK) {
    case TAG_PRIMARY_IMMED1:
	switch ((arg1 & _TAG_IMMED1_MASK) >> _TAG_PRIMARY_SIZE) {
	case (_TAG_IMMED1_SMALL >> _TAG_PRIMARY_SIZE):
	    switch (arg2 & _TAG_PRIMARY_MASK) {
	    case TAG_PRIMARY_IMMED1:
		switch ((arg2 & _TAG_IMMED1_MASK) >> _TAG_PRIMARY_SIZE) {
		case (_TAG_IMMED1_SMALL >> _TAG_PRIMARY_SIZE):
		    if ((arg1 == SMALL_ZERO) || (arg2 == SMALL_ZERO)) {
			return(SMALL_ZERO);
		    } else if (arg1 == SMALL_ONE) {
			return(arg2);
		    } else if (arg2 == SMALL_ONE) {
			return(arg1);
		    } else {
			DeclareTmpHeap(big_res,3,p);
			UseTmpHeap(3,p);

			/*
			 * The following code is optimized for the case that
			 * result is small (which should be the most common case
			 * in practice).
			 */
			res = small_times(signed_val(arg1), signed_val(arg2),
					  big_res);
			if (is_small(res)) {
			    UnUseTmpHeap(3,p);
			    return res;
			} else {
			    /*
			     * The result is a a big number.
			     * Allocate a heap fragment and copy the result.
			     * Be careful to allocate exactly what we need
			     * to not leave any holes.
			     */
			    Uint arity;
			    Uint need;
			    
			    ASSERT(is_big(res));
			    hdr = big_res[0];
			    arity = bignum_header_arity(hdr);
			    ASSERT(arity == 1 || arity == 2);
			    need = arity + 1;
			    if (ERTS_NEED_GC(p, need)) {
				erts_garbage_collect(p, need, reg, live);
			    }
			    hp = p->htop;
			    p->htop += need;
			    res = make_big(hp);
			    *hp++ = hdr;
			    *hp++ = big_res[1];
			    if (arity > 1) {
				*hp = big_res[2];
			    }
			    UnUseTmpHeap(3,p);
			    return res;
			}
		    }
		default:
		badarith:
		    p->freason = BADARITH;
		    return THE_NON_VALUE;
		}
	    case TAG_PRIMARY_BOXED:
		hdr = *boxed_val(arg2);
		switch ((hdr & _TAG_HEADER_MASK) >> _TAG_PRIMARY_SIZE) {
		case (_TAG_HEADER_POS_BIG >> _TAG_PRIMARY_SIZE):
		case (_TAG_HEADER_NEG_BIG >> _TAG_PRIMARY_SIZE):
		    if (arg1 == SMALL_ZERO)
			return(SMALL_ZERO);
		    if (arg1 == SMALL_ONE)
			return(arg2);
		    arg1 = small_to_big(signed_val(arg1), tmp_big1);
		    sz = 2 + big_size(arg2);
		    goto do_big;
		case (_TAG_HEADER_FLOAT >> _TAG_PRIMARY_SIZE):
		    f1.fd = signed_val(arg1);
		    GET_DOUBLE(arg2, f2);
		    goto do_float;
		default:
		    goto badarith;
		}
	    }
	default:
	    goto badarith;
	}
    case TAG_PRIMARY_BOXED:
	hdr = *boxed_val(arg1);
	switch ((hdr & _TAG_HEADER_MASK) >> _TAG_PRIMARY_SIZE) {
	case (_TAG_HEADER_POS_BIG >> _TAG_PRIMARY_SIZE):
	case (_TAG_HEADER_NEG_BIG >> _TAG_PRIMARY_SIZE):
	    switch (arg2 & _TAG_PRIMARY_MASK) {
	    case (_TAG_IMMED1_SMALL >> _TAG_PRIMARY_SIZE):
		switch ((arg2 & _TAG_IMMED1_MASK) >> _TAG_PRIMARY_SIZE) {
		case (_TAG_IMMED1_SMALL >> _TAG_PRIMARY_SIZE):
		    if (arg2 == SMALL_ZERO)
			return(SMALL_ZERO);
		    if (arg2 == SMALL_ONE)
			return(arg1);
		    arg2 = small_to_big(signed_val(arg2), tmp_big2);
		    sz = 2 + big_size(arg1);
		    goto do_big;
		default:
		    goto badarith;
		}
	    case TAG_PRIMARY_BOXED:
		hdr = *boxed_val(arg2);
		switch ((hdr & _TAG_HEADER_MASK) >> _TAG_PRIMARY_SIZE) {
		case (_TAG_HEADER_POS_BIG >> _TAG_PRIMARY_SIZE):
		case (_TAG_HEADER_NEG_BIG >> _TAG_PRIMARY_SIZE):
		    sz1 = big_size(arg1);
		    sz2 = big_size(arg2);
		    sz = sz1 + sz2;

		do_big:
		    need_heap = BIG_NEED_SIZE(sz);
		    if (ERTS_NEED_GC(p, need_heap)) {
			erts_garbage_collect(p, need_heap, reg, live+2);
			if (ARG_IS_NOT_TMP(arg1,tmp_big1)) {
			    arg1 = reg[live];
			}
			if (ARG_IS_NOT_TMP(arg2,tmp_big2)) {
			    arg2 = reg[live+1];
			}
		    }
		    hp = p->htop;
		    p->htop += need_heap;
		    res = big_times(arg1, arg2, hp);
		    trim_heap(p, hp, res);

		    /*
		     * Note that the result must be big in this case, since
		     * at least one operand was big to begin with, and
		     * the absolute value of the other is > 1.
		     */

		    if (is_nil(res)) {
			p->freason = SYSTEM_LIMIT;
			return THE_NON_VALUE;
		    }
		    return res;
		case (_TAG_HEADER_FLOAT >> _TAG_PRIMARY_SIZE):
		    if (big_to_double(arg1, &f1.fd) < 0) {
			goto badarith;
		    }
		    GET_DOUBLE(arg2, f2);
		    goto do_float;
		default:
		    goto badarith;
		}
	    }
	case (_TAG_HEADER_FLOAT >> _TAG_PRIMARY_SIZE):
	    switch (arg2 & _TAG_PRIMARY_MASK) {
	    case TAG_PRIMARY_IMMED1:
		switch ((arg2 & _TAG_IMMED1_MASK) >> _TAG_PRIMARY_SIZE) {
		case (_TAG_IMMED1_SMALL >> _TAG_PRIMARY_SIZE):
		    GET_DOUBLE(arg1, f1);
		    f2.fd = signed_val(arg2);
		    goto do_float;
		default:
		    goto badarith;
		}
	    case TAG_PRIMARY_BOXED:
		hdr = *boxed_val(arg2);
		switch ((hdr & _TAG_HEADER_MASK) >> _TAG_PRIMARY_SIZE) {
		case (_TAG_HEADER_POS_BIG >> _TAG_PRIMARY_SIZE):
		case (_TAG_HEADER_NEG_BIG >> _TAG_PRIMARY_SIZE):
		    GET_DOUBLE(arg1, f1);
		    if (big_to_double(arg2, &f2.fd) < 0) {
			goto badarith;
		    }
		    goto do_float;
		case (_TAG_HEADER_FLOAT >> _TAG_PRIMARY_SIZE):
		    GET_DOUBLE(arg1, f1);
		    GET_DOUBLE(arg2, f2);

		do_float:
		    f1.fd = f1.fd * f2.fd;
		    ERTS_FP_ERROR(p, f1.fd, goto badarith);
		    if (ERTS_NEED_GC(p, FLOAT_SIZE_OBJECT)) {
			erts_garbage_collect(p, FLOAT_SIZE_OBJECT, reg, live);
		    }
		    hp = p->htop;
		    p->htop += FLOAT_SIZE_OBJECT;
		    res = make_float(hp);
		    PUT_DOUBLE(f1, hp);
		    return res;
		default:
		    goto badarith;
		}
	    default:
		goto badarith;
	    }
	}
    default:
	goto badarith;
    }
}

Eterm
erts_gc_mixed_div(Process* p, Eterm* reg, Uint live)
{
    Eterm arg1;
    Eterm arg2;
    FloatDef f1, f2;
    Eterm* hp;
    Eterm hdr;

    arg1 = reg[live];
    arg2 = reg[live+1];
    ERTS_FP_CHECK_INIT(p);
    switch (arg1 & _TAG_PRIMARY_MASK) {
    case TAG_PRIMARY_IMMED1:
	switch ((arg1 & _TAG_IMMED1_MASK) >> _TAG_PRIMARY_SIZE) {
	case (_TAG_IMMED1_SMALL >> _TAG_PRIMARY_SIZE):
	    switch (arg2 & _TAG_PRIMARY_MASK) {
	    case TAG_PRIMARY_IMMED1:
		switch ((arg2 & _TAG_IMMED1_MASK) >> _TAG_PRIMARY_SIZE) {
		case (_TAG_IMMED1_SMALL >> _TAG_PRIMARY_SIZE):
		    f1.fd = signed_val(arg1);
		    f2.fd = signed_val(arg2);
		    goto do_float;
		default:
		badarith:
		    p->freason = BADARITH;
		    return THE_NON_VALUE;
		}
	    case TAG_PRIMARY_BOXED:
		hdr = *boxed_val(arg2);
		switch ((hdr & _TAG_HEADER_MASK) >> _TAG_PRIMARY_SIZE) {
		case (_TAG_HEADER_POS_BIG >> _TAG_PRIMARY_SIZE):
		case (_TAG_HEADER_NEG_BIG >> _TAG_PRIMARY_SIZE):
		    f1.fd = signed_val(arg1);
		    if (big_to_double(arg2, &f2.fd) < 0) {
			goto badarith;
		    }
		    goto do_float;
		case (_TAG_HEADER_FLOAT >> _TAG_PRIMARY_SIZE):
		    f1.fd = signed_val(arg1);
		    GET_DOUBLE(arg2, f2);
		    goto do_float;
		default:
		    goto badarith;
		}
	    }
	default:
	    goto badarith;
	}
    case TAG_PRIMARY_BOXED:
	hdr = *boxed_val(arg1);
	switch ((hdr & _TAG_HEADER_MASK) >> _TAG_PRIMARY_SIZE) {
	case (_TAG_HEADER_POS_BIG >> _TAG_PRIMARY_SIZE):
	case (_TAG_HEADER_NEG_BIG >> _TAG_PRIMARY_SIZE):
	    switch (arg2 & _TAG_PRIMARY_MASK) {
	    case (_TAG_IMMED1_SMALL >> _TAG_PRIMARY_SIZE):
		switch ((arg2 & _TAG_IMMED1_MASK) >> _TAG_PRIMARY_SIZE) {
		case (_TAG_IMMED1_SMALL >> _TAG_PRIMARY_SIZE):
		    if (big_to_double(arg1, &f1.fd) < 0) {
			goto badarith;
		    }
		    f2.fd = signed_val(arg2);
		    goto do_float;
		default:
		    goto badarith;
		}
	    case TAG_PRIMARY_BOXED:
		hdr = *boxed_val(arg2);
		switch ((hdr & _TAG_HEADER_MASK) >> _TAG_PRIMARY_SIZE) {
		case (_TAG_HEADER_POS_BIG >> _TAG_PRIMARY_SIZE):
		case (_TAG_HEADER_NEG_BIG >> _TAG_PRIMARY_SIZE):
		    if (big_to_double(arg1, &f1.fd) < 0 ||
			big_to_double(arg2, &f2.fd) < 0) {
			goto badarith;
		    }
		    goto do_float;
		case (_TAG_HEADER_FLOAT >> _TAG_PRIMARY_SIZE):
		    if (big_to_double(arg1, &f1.fd) < 0) {
			goto badarith;
		    }
		    GET_DOUBLE(arg2, f2);
		    goto do_float;
		default:
		    goto badarith;
		}
	    }
	case (_TAG_HEADER_FLOAT >> _TAG_PRIMARY_SIZE):
	    switch (arg2 & _TAG_PRIMARY_MASK) {
	    case TAG_PRIMARY_IMMED1:
		switch ((arg2 & _TAG_IMMED1_MASK) >> _TAG_PRIMARY_SIZE) {
		case (_TAG_IMMED1_SMALL >> _TAG_PRIMARY_SIZE):
		    GET_DOUBLE(arg1, f1);
		    f2.fd = signed_val(arg2);
		    goto do_float;
		default:
		    goto badarith;
		}
	    case TAG_PRIMARY_BOXED:
		hdr = *boxed_val(arg2);
		switch ((hdr & _TAG_HEADER_MASK) >> _TAG_PRIMARY_SIZE) {
		case (_TAG_HEADER_POS_BIG >> _TAG_PRIMARY_SIZE):
		case (_TAG_HEADER_NEG_BIG >> _TAG_PRIMARY_SIZE):
		    GET_DOUBLE(arg1, f1);
		    if (big_to_double(arg2, &f2.fd) < 0) {
			goto badarith;
		    }
		    goto do_float;
		case (_TAG_HEADER_FLOAT >> _TAG_PRIMARY_SIZE):
		    GET_DOUBLE(arg1, f1);
		    GET_DOUBLE(arg2, f2);

		do_float:
		    f1.fd = f1.fd / f2.fd;
		    ERTS_FP_ERROR(p, f1.fd, goto badarith);
		    if (ERTS_NEED_GC(p, FLOAT_SIZE_OBJECT)) {
			erts_garbage_collect(p, FLOAT_SIZE_OBJECT, reg, live);
		    }
		    hp = p->htop;
		    p->htop += FLOAT_SIZE_OBJECT;
		    PUT_DOUBLE(f1, hp);
		    return make_float(hp);
		default:
		    goto badarith;
		}
	    default:
		goto badarith;
	    }
	}
    default:
	goto badarith;
    }
}

Eterm
erts_gc_int_div(Process* p, Eterm* reg, Uint live)
{
    Eterm arg1;
    Eterm arg2;
    DECLARE_TMP(tmp_big1,0,p);
    DECLARE_TMP(tmp_big2,1,p);
    int ires;

    arg1 = reg[live];
    arg2 = reg[live+1];
    switch (NUMBER_CODE(arg1, arg2)) {
    case SMALL_SMALL:
	/* This case occurs if the most negative fixnum is divided by -1. */
	ASSERT(arg2 == make_small(-1));
	arg1 = small_to_big(signed_val(arg1), tmp_big1);
	/*FALLTHROUGH*/
    case BIG_SMALL:
	arg2 = small_to_big(signed_val(arg2), tmp_big2);
	goto L_big_div;
    case SMALL_BIG:
	if (arg1 != make_small(MIN_SMALL)) {
	    return SMALL_ZERO;
	}
	arg1 = small_to_big(signed_val(arg1), tmp_big1);
	/*FALLTHROUGH*/
    case BIG_BIG:
    L_big_div:
	ires = big_ucomp(arg1, arg2);
	if (ires < 0) {
	    arg1 = SMALL_ZERO;
	} else if (ires == 0) {
	    arg1 = (big_sign(arg1) == big_sign(arg2)) ?
		SMALL_ONE : SMALL_MINUS_ONE;
	} else {
	    Eterm* hp;
	    int i = big_size(arg1);
	    Uint need;

	    ires = big_size(arg2);
	    need = BIG_NEED_SIZE(i-ires+1) + BIG_NEED_SIZE(i);
	    if (ERTS_NEED_GC(p, need)) {
		erts_garbage_collect(p, need, reg, live+2);
		if (ARG_IS_NOT_TMP(arg1,tmp_big1)) {
		    arg1 = reg[live];
		}
		if (ARG_IS_NOT_TMP(arg2,tmp_big2)) {
		    arg2 = reg[live+1];
		}
	    }
	    hp = p->htop;
	    p->htop += need;
	    arg1 = big_div(arg1, arg2, hp);
	    trim_heap(p, hp, arg1);
	    if (is_nil(arg1)) {
		p->freason = SYSTEM_LIMIT;
		return THE_NON_VALUE;
	    }
	}
	return arg1;
    default:
	p->freason = BADARITH;
	return THE_NON_VALUE;
    }
}

Eterm
erts_gc_int_rem(Process* p, Eterm* reg, Uint live)
{
    Eterm arg1;
    Eterm arg2;
    DECLARE_TMP(tmp_big1,0,p);
    DECLARE_TMP(tmp_big2,1,p);
    int ires;

    arg1 = reg[live];
    arg2 = reg[live+1];
    switch (NUMBER_CODE(arg1, arg2)) {
    case BIG_SMALL:
	arg2 = small_to_big(signed_val(arg2), tmp_big2);
	goto L_big_rem;
    case SMALL_BIG:
	if (arg1 != make_small(MIN_SMALL)) {
	    return arg1;
	} else {
	    Eterm tmp;
	    tmp = small_to_big(signed_val(arg1), tmp_big1);
	    if ((ires = big_ucomp(tmp, arg2)) == 0) {
		return SMALL_ZERO;
	    } else {
		ASSERT(ires < 0);
		return arg1;
	    }
	}
	/* All paths returned */
    case BIG_BIG:
    L_big_rem:
	ires = big_ucomp(arg1, arg2);
	if (ires == 0) {
	    arg1 = SMALL_ZERO;
	} else if (ires > 0) {
	    Eterm* hp;
	    Uint need = BIG_NEED_SIZE(big_size(arg1));

	    if (ERTS_NEED_GC(p, need)) {
		erts_garbage_collect(p, need, reg, live+2);
		if (ARG_IS_NOT_TMP(arg1,tmp_big1)) {
		    arg1 = reg[live];
		}
		if (ARG_IS_NOT_TMP(arg2,tmp_big2)) {
		    arg2 = reg[live+1];
		}
	    }
	    hp = p->htop;
	    p->htop += need;
	    arg1 = big_rem(arg1, arg2, hp);
	    trim_heap(p, hp, arg1);
	    if (is_nil(arg1)) {
		p->freason = SYSTEM_LIMIT;
		return THE_NON_VALUE;
	    }
	}
	return arg1;
    default:
	p->freason = BADARITH;
	return THE_NON_VALUE;
    }
}

#define DEFINE_GC_LOGIC_FUNC(func)						\
Eterm erts_gc_##func(Process* p, Eterm* reg, Uint live)				\
{										\
    Eterm arg1;									\
    Eterm arg2;									\
    DECLARE_TMP(tmp_big1,0,p);							\
    DECLARE_TMP(tmp_big2,1,p);							\
    Eterm* hp;									\
    int need;									\
										\
    arg1 = reg[live];								\
    arg2 = reg[live+1];								\
    switch (NUMBER_CODE(arg1, arg2)) {						\
    case SMALL_BIG:								\
	arg1 = small_to_big(signed_val(arg1), tmp_big1);			\
	need = BIG_NEED_SIZE(big_size(arg2) + 1);				\
	if (ERTS_NEED_GC(p, need)) {						\
	    erts_garbage_collect(p, need, reg, live+2);				\
	    arg2 = reg[live+1];							\
	}									\
	break;									\
    case BIG_SMALL:								\
	arg2 = small_to_big(signed_val(arg2), tmp_big2);			\
	need = BIG_NEED_SIZE(big_size(arg1) + 1);				\
	if (ERTS_NEED_GC(p, need)) {						\
	    erts_garbage_collect(p, need, reg, live+2);				\
	    arg1 = reg[live];							\
	}									\
	break;									\
    case BIG_BIG:								\
	need = BIG_NEED_SIZE(MAX(big_size(arg1), big_size(arg2)) + 1);		\
	if (ERTS_NEED_GC(p, need)) {						\
	    erts_garbage_collect(p, need, reg, live+2);				\
	    arg1 = reg[live];							\
	    arg2 = reg[live+1];							\
	}									\
	break;									\
    default:									\
	p->freason = BADARITH;							\
	return THE_NON_VALUE;							\
    }										\
    hp = p->htop;								\
    p->htop += need;								\
    arg1 = big_##func(arg1, arg2, hp);						\
    trim_heap(p, hp, arg1);							\
    return arg1;								\
}

DEFINE_GC_LOGIC_FUNC(band)
DEFINE_GC_LOGIC_FUNC(bor)
DEFINE_GC_LOGIC_FUNC(bxor)

Eterm erts_gc_bnot(Process* p, Eterm* reg, Uint live)
{
    Eterm result;
    Eterm arg;
    Uint need;
    Eterm* bigp;

    arg = reg[live];
    if (is_not_big(arg)) {
	p->freason = BADARITH;
	return NIL;
    } else {
	need = BIG_NEED_SIZE(big_size(arg)+1);
	if (ERTS_NEED_GC(p, need)) {
	    erts_garbage_collect(p, need, reg, live+1);
	    arg = reg[live];
	}
	bigp = p->htop;
	p->htop += need;
	result = big_bnot(arg, bigp);
	trim_heap(p, bigp, result);
	if (is_nil(result)) {
	    p->freason = SYSTEM_LIMIT;
	    return NIL;
	}
    }
    return result;
} 
