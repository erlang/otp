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

/*
 * BIFs logically belonging to the lists module.
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

static Eterm keyfind(int Bif, Process* p, Eterm Key, Eterm Pos, Eterm List);

static BIF_RETTYPE append(Process* p, Eterm A, Eterm B)
{
    Eterm list;
    Eterm copy;
    Eterm last;
    Eterm* hp = NULL;
    Sint i;

    list = A;

    if (is_nil(list)) {
        BIF_RET(B);
    }

    if (is_not_list(list)) {
        BIF_ERROR(p, BADARG);
    }

    /* optimistic append on heap first */

    if ((i = HeapWordsLeft(p) / 2) < 4) {
        goto list_tail;
    }

    hp   = HEAP_TOP(p);
    copy = last = CONS(hp, CAR(list_val(list)), make_list(hp+2));
    list = CDR(list_val(list));
    hp  += 2;
    i   -= 2; /* don't use the last 2 words (extra i--;) */

    while(i-- && is_list(list)) {
        Eterm* listp = list_val(list);
        last = CONS(hp, CAR(listp), make_list(hp+2));
        list = CDR(listp);
        hp += 2;
    }

    /* A is proper and B is NIL return A as-is, don't update HTOP */

    if (is_nil(list) && is_nil(B)) {
        BIF_RET(A);
    }

    if (is_nil(list)) {
        HEAP_TOP(p) = hp;
        CDR(list_val(last)) = B;
        BIF_RET(copy);
    }

list_tail:

    if ((i = erts_list_length(list)) < 0) {
        BIF_ERROR(p, BADARG);
    }

    /* remaining list was proper and B is NIL */
    if (is_nil(B)) {
        BIF_RET(A);
    }

    if (hp) {
        /* Note: fall through case, already written
         * on the heap.
         * The last 2 words of the heap is not written yet
         */
        Eterm *hp_save = hp;
        ASSERT(i != 0);
        HEAP_TOP(p) = hp + 2;
        if (i == 1) {
            hp[0] = CAR(list_val(list));
            hp[1] = B;
            BIF_RET(copy);
        }
        hp   = HAlloc(p, 2*(i - 1));
        last = CONS(hp_save, CAR(list_val(list)), make_list(hp));
    } else {
        hp   = HAlloc(p, 2*i);
        copy = last = CONS(hp, CAR(list_val(list)), make_list(hp+2));
        hp  += 2;
    }

    list = CDR(list_val(list));
    i--;

    ASSERT(i > -1);
    while(i--) {
        Eterm* listp = list_val(list);
        last = CONS(hp, CAR(listp), make_list(hp+2));
        list = CDR(listp);
        hp  += 2;
    }

    CDR(list_val(last)) = B;
    BIF_RET(copy);
}

/*
 * erlang:'++'/2
 */

Eterm
ebif_plusplus_2(BIF_ALIST_2)
{
    return append(BIF_P, BIF_ARG_1, BIF_ARG_2);
}

BIF_RETTYPE append_2(BIF_ALIST_2)
{
    return append(BIF_P, BIF_ARG_1, BIF_ARG_2);
}

/*
 * erlang:'--'/2
 */

#define SMALL_VEC_SIZE 10
static Eterm subtract(Process* p, Eterm A, Eterm B)
{
    Eterm  list;
    Eterm* hp;
    Uint  need;
    Eterm  res;
    Eterm small_vec[SMALL_VEC_SIZE];	/* Preallocated memory for small lists */
    Eterm* vec_p;
    Eterm* vp;
    Sint i;
    Sint n;
    Sint m;
    
    if ((n = erts_list_length(A)) < 0) {
	BIF_ERROR(p, BADARG);
    }
    if ((m = erts_list_length(B)) < 0) {
	BIF_ERROR(p, BADARG);
    }
    
    if (n == 0)
	BIF_RET(NIL);
    if (m == 0)
	BIF_RET(A);
    
    /* allocate element vector */
    if (n <= SMALL_VEC_SIZE)
	vec_p = small_vec;
    else
	vec_p = (Eterm*) erts_alloc(ERTS_ALC_T_TMP, n * sizeof(Eterm));
    
    /* PUT ALL ELEMENTS IN VP */
    vp = vec_p;
    list = A;
    i = n;
    while(i--) {
	Eterm* listp = list_val(list);
	*vp++ = CAR(listp);
	list = CDR(listp);
    }
    
    /* UNMARK ALL DELETED CELLS */
    list = B;
    m = 0;  /* number of deleted elements */
    while(is_list(list)) {
	Eterm* listp = list_val(list);
	Eterm  elem = CAR(listp);
	i = n;
	vp = vec_p;
	while(i--) {
	    if (is_value(*vp) && eq(*vp, elem)) {
		*vp = THE_NON_VALUE;
		m++;
		break;
	    }
	    vp++;
	}
	list = CDR(listp);
    }
    
    if (m == n)      /* All deleted ? */
	res = NIL;
    else if (m == 0)  /* None deleted ? */
	res = A;
    else {			/* REBUILD LIST */
	res = NIL;
	need = 2*(n - m);
	hp = HAlloc(p, need);
	vp = vec_p + n - 1;
	while(vp >= vec_p) {
	    if (is_value(*vp)) {
		res = CONS(hp, *vp, res);
		hp += 2;
	    }
	    vp--;
	}
    }
    if (vec_p != small_vec)
	erts_free(ERTS_ALC_T_TMP, (void *) vec_p);
    BIF_RET(res);
}

BIF_RETTYPE ebif_minusminus_2(BIF_ALIST_2)
{
    return subtract(BIF_P, BIF_ARG_1, BIF_ARG_2);
}

BIF_RETTYPE subtract_2(BIF_ALIST_2)
{
    return subtract(BIF_P, BIF_ARG_1, BIF_ARG_2);
}

BIF_RETTYPE lists_member_2(BIF_ALIST_2)
{
    Eterm term;
    Eterm list;
    Eterm item;
    int non_immed_key;
    int max_iter = 10 * CONTEXT_REDS;

    if (is_nil(BIF_ARG_2)) {
	BIF_RET(am_false);
    } else if (is_not_list(BIF_ARG_2)) {
	BIF_ERROR(BIF_P, BADARG);
    }
    
    term = BIF_ARG_1;
    non_immed_key = is_not_immed(term);
    list = BIF_ARG_2;
    while (is_list(list)) {
	if (--max_iter < 0) {
	    BUMP_ALL_REDS(BIF_P);
	    BIF_TRAP2(bif_export[BIF_lists_member_2], BIF_P, term, list);
	}
	item = CAR(list_val(list));
	if ((item == term) || (non_immed_key && eq(item, term))) {
	    BIF_RET2(am_true, CONTEXT_REDS - max_iter/10);
	}
	list = CDR(list_val(list));
    }
    if (is_not_nil(list))  {
	BIF_ERROR(BIF_P, BADARG);
    }
    BIF_RET2(am_false, CONTEXT_REDS - max_iter/10);
}

BIF_RETTYPE lists_reverse_2(BIF_ALIST_2)
{
    Eterm list;
    Eterm tmp_list;
    Eterm result;
    Eterm* hp;
    Uint n;
    int max_iter;
    
    /*
     * Handle legal and illegal non-lists quickly.
     */
    if (is_nil(BIF_ARG_1)) {
	BIF_RET(BIF_ARG_2);
    } else if (is_not_list(BIF_ARG_1)) {
    error:
	BIF_ERROR(BIF_P, BADARG);
    }

    /*
     * First use the rest of the remaning heap space.
     */
    list = BIF_ARG_1;
    result = BIF_ARG_2;
    hp = HEAP_TOP(BIF_P);
    n = HeapWordsLeft(BIF_P) / 2;
    while (n != 0 && is_list(list)) {
	Eterm* pair = list_val(list);
	result = CONS(hp, CAR(pair), result);
	list = CDR(pair);
	hp += 2;
	n--;
    }
    HEAP_TOP(BIF_P) = hp;
    if (is_nil(list)) {
	BIF_RET(result);
    }

    /*
     * Calculate length of remaining list (up to a suitable limit).
     */
    max_iter = CONTEXT_REDS * 40;
    n = 0;
    tmp_list = list;
    while (max_iter-- > 0 && is_list(tmp_list)) {
	tmp_list = CDR(list_val(tmp_list));
	n++;
    }
    if (is_not_nil(tmp_list) && is_not_list(tmp_list)) {
	goto error;
    }

    /*
     * Now do one HAlloc() and continue reversing.
     */
    hp = HAlloc(BIF_P, 2*n);
    while (n != 0 && is_list(list)) {
	Eterm* pair = list_val(list);
	result = CONS(hp, CAR(pair), result);
	list = CDR(pair);
	hp += 2;
	n--;
    }
    if (is_nil(list)) {
	BIF_RET(result);
    } else {
	BUMP_ALL_REDS(BIF_P);
	BIF_TRAP2(bif_export[BIF_lists_reverse_2], BIF_P, list, result);
    }
}

BIF_RETTYPE
lists_keymember_3(BIF_ALIST_3)
{
    Eterm res;

    res = keyfind(BIF_lists_keymember_3, BIF_P,
		  BIF_ARG_1, BIF_ARG_2, BIF_ARG_3);
    if (is_value(res) && is_tuple(res)) {
	return am_true;
    } else {
	return res;
    }
}

BIF_RETTYPE
lists_keysearch_3(BIF_ALIST_3)
{
    Eterm res;
    
    res = keyfind(BIF_lists_keysearch_3, BIF_P,
		  BIF_ARG_1, BIF_ARG_2, BIF_ARG_3);
    if (is_non_value(res) || is_not_tuple(res)) {
	return res;
    } else {			/* Tuple */
	Eterm* hp = HAlloc(BIF_P, 3);
	return TUPLE2(hp, am_value, res);
    }
}

BIF_RETTYPE
lists_keyfind_3(BIF_ALIST_3)
{
    return keyfind(BIF_lists_keyfind_3, BIF_P,
		   BIF_ARG_1, BIF_ARG_2, BIF_ARG_3);
}

static Eterm
keyfind(int Bif, Process* p, Eterm Key, Eterm Pos, Eterm List)
{
    int max_iter = 10 * CONTEXT_REDS;
    Sint pos;
    Eterm term;

    if (!is_small(Pos) || (pos = signed_val(Pos)) < 1) {
	BIF_ERROR(p, BADARG);
    }

    if (is_small(Key)) {
	double float_key = (double) signed_val(Key);

	while (is_list(List)) {
	    if (--max_iter < 0) {
		BUMP_ALL_REDS(p);
		BIF_TRAP3(bif_export[Bif], p, Key, Pos, List);
	    }
	    term = CAR(list_val(List));
	    List = CDR(list_val(List));
	    if (is_tuple(term)) {
		Eterm *tuple_ptr = tuple_val(term);
		if (pos <= arityval(*tuple_ptr)) {
		    Eterm element = tuple_ptr[pos];
		    if (Key == element) {
			return term;
		    } else if (is_float(element)) {
			FloatDef f;

			GET_DOUBLE(element, f);
			if (f.fd == float_key) {
			    return term;
			}
		    }
		}
	    }
	}
    } else if (is_immed(Key)) {
	while (is_list(List)) {
	    if (--max_iter < 0) {
		BUMP_ALL_REDS(p);
		BIF_TRAP3(bif_export[Bif], p, Key, Pos, List);
	    }
	    term = CAR(list_val(List));
	    List = CDR(list_val(List));
	    if (is_tuple(term)) {
		Eterm *tuple_ptr = tuple_val(term);
		if (pos <= arityval(*tuple_ptr)) {
		    Eterm element = tuple_ptr[pos];
		    if (Key == element) {
			return term;
		    }
		}
	    }
	}
    } else {
	while (is_list(List)) {
	    if (--max_iter < 0) {
		BUMP_ALL_REDS(p);
		BIF_TRAP3(bif_export[Bif], p, Key, Pos, List);
	    }
	    term = CAR(list_val(List));
	    List = CDR(list_val(List));
	    if (is_tuple(term)) {
		Eterm *tuple_ptr = tuple_val(term);
		if (pos <= arityval(*tuple_ptr)) {
		    Eterm element = tuple_ptr[pos];
		    if (CMP_EQ(Key, element)) {
			return term;
		    }
		}
	    }
	}
    }

    if (is_not_nil(List))  {
	BIF_ERROR(p, BADARG);
    }
    return am_false;
}
