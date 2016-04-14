/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1999-2013. All Rights Reserved.
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
 * Code for process dictionaries.
 *
 */
#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "erl_vm.h"
#include "global.h"
#include "erl_process.h" /* Will include erl_process_dict.h */
#include "error.h"
#include "erl_driver.h"
#include "bif.h"
#include "big.h"
#include "dist.h"
#include "erl_version.h"

/* #define HARDDEBUG */

/*
** Utility macros
*/

/* Flags to pd_get_hash */
#define PD_GET_OTHER_PROCESS 1UL

/* Hash constant macros */
#define MAX_HASH            1342177280UL
#define INITIAL_SIZE        (erts_pd_initial_size)

/* Hash utility macros  */
#define HASH_RANGE(PDict) ((PDict)->homeSize + (PDict)->splitPosition)

#define MAKE_HASH(Term) 				\
((is_small(Term)) ? unsigned_val(Term) :		\
 ((is_atom(Term)) ? 					\
  (atom_tab(atom_val(term))->slot.bucket.hvalue) :	\
  make_hash2(Term)))

#define PD_SZ2BYTES(Sz) (sizeof(ProcDict) + ((Sz) - 1)*sizeof(Eterm))

/* Memory allocation macros */
#define PD_ALLOC(Sz)				\
     erts_alloc(ERTS_ALC_T_PROC_DICT, (Sz))
#define PD_FREE(P, Sz)				\
     erts_free(ERTS_ALC_T_PROC_DICT, (P))
#define PD_REALLOC(P, OSz, NSz) 		\
     erts_realloc(ERTS_ALC_T_PROC_DICT, (P), (NSz))


#define TCAR(Term) CAR(list_val(Term))
#define TCDR(Term) CDR(list_val(Term))

/* Array access macro */ 
#define ARRAY_GET(PDict, Index) (((PDict)->size > (Index)) ? \
				 (PDict)->data[Index] : NIL)

/*
 * Forward decalarations
 */
static void pd_hash_erase(Process *p, Eterm id, Eterm *ret);
static void pd_hash_erase_all(Process *p);
static Eterm pd_hash_get_keys(Process *p, Eterm value);
static Eterm pd_hash_get_all_keys(Process *p, ProcDict *pd);
static Eterm pd_hash_get_all(Process *p, ProcDict *pd);
static Eterm pd_hash_put(Process *p, Eterm id, Eterm value);

static void shrink(Process *p, Eterm* ret); 
static void grow(Process *p);

static void array_shrink(ProcDict **ppd, unsigned int need);
static Eterm array_put(ProcDict **ppdict, unsigned int ndx, Eterm term);

static unsigned int pd_hash_value(ProcDict *pdict, Eterm term);
static unsigned int next_array_size(unsigned int need);

/*
** Debugging prototypes and macros
*/
#ifdef HARDDEBUG

#include <stdarg.h>

static int hdebugf(char *format, ...);
static char *hdebugf_file = "";
static int hdebugf_line;
#define HDEBUGF(X) ((int) hdebugf_file = __FILE__, hdebugf_line = __LINE__, \
		    hdebugf X)
#ifndef DEBUG
#define DEBUG 1
#endif

#else /* !HARDDEBUG */

#define HDEBUGF(X) /* Nothing */

#endif /* HARDDEBUG (else) */

#ifdef DEBUG 

static void pd_check(ProcDict *pd);

#define PD_CHECK(PD) pd_check(PD)

#else /* !DEBUG */

#define PD_CHECK(PD) /* Nothing */

#endif /* DEBUG (else) */

/*
** External interface
*/

/*
 * Called from break handler
 */
void
erts_dictionary_dump(int to, void *to_arg, ProcDict *pd)
{
    unsigned int i;
#ifdef DEBUG

    /*PD_CHECK(pd);*/
    if (pd == NULL)
	return;
    erts_print(to, to_arg, "(size = %d, used = %d, homeSize = %d, "
	       "splitPosition = %d, numElements = %d)\n",
	       pd->size, pd->used, pd->homeSize, 
	       pd->splitPosition, (unsigned int) pd->numElements);
    for (i = 0; i < HASH_RANGE(pd); ++i) {
	erts_print(to, to_arg, "%d: %T\n", i, ARRAY_GET(pd, i));
    }

#else /* !DEBUG */

    int written = 0;
    Eterm t;

    erts_print(to, to_arg, "[");
    if (pd != NULL) {
	for (i = 0; i < HASH_RANGE(pd); ++i) {
	    t = ARRAY_GET(pd, i);
	    if (is_list(t)) {
		for (; t != NIL; t = TCDR(t)) {
		    erts_print(to, to_arg, written++ ? ",%T" : "%T", TCAR(t));
		}
	    } else if (is_tuple(t)) {
		erts_print(to, to_arg, written++ ? ",%T" : "%T", t);
	    }
	}
    }
    erts_print(to, to_arg, "]");

#endif /* DEBUG (else) */
}

void
erts_deep_dictionary_dump(int to, void *to_arg,
			  ProcDict* pd, void (*cb)(int, void *, Eterm))
{
    unsigned int i;
    Eterm t;

    if (pd != NULL) {
	for (i = 0; i < HASH_RANGE(pd); ++i) {
	    t = ARRAY_GET(pd, i);
	    if (is_list(t)) {
		for (; t != NIL; t = TCDR(t)) {
		    (*cb)(to, to_arg, TCAR(t));
		}
	    } else if (is_tuple(t)) {
		(*cb)(to, to_arg, t);
	    }
	}
    }
}

Uint
erts_dicts_mem_size(Process *p)
{
    Uint size = 0;
    if (p->dictionary)
	size += PD_SZ2BYTES(p->dictionary->size);
    return size;
}

void
erts_erase_dicts(Process *p)
{
    if (p->dictionary) {
	pd_hash_erase_all(p);
	p->dictionary = NULL;
    }
}

/* 
 * Called from process_info/1,2.
 */
Eterm erts_dictionary_copy(Process *p, ProcDict *pd) 
{
    Eterm* hp;
    Eterm* heap_start;
    Eterm res = NIL;
    Eterm tmp, tmp2;
    unsigned int i, num;

    if (pd == NULL) {
	return res;
    }

    PD_CHECK(pd);
    num = HASH_RANGE(pd);
    heap_start = hp = (Eterm *) erts_alloc(ERTS_ALC_T_TMP,
					   sizeof(Eterm) * pd->numElements * 2);
    for (i = 0; i < num; ++i) {
	tmp = ARRAY_GET(pd, i);
	if (is_boxed(tmp)) {
	    ASSERT(is_tuple(tmp));
	    res = CONS(hp, tmp, res);
	    hp += 2;
	} else if (is_list(tmp)) {
	    while (tmp != NIL) {
		tmp2 = TCAR(tmp);
		res = CONS(hp, tmp2, res);
		hp += 2;
		tmp = TCDR(tmp);
	    }
	}
    }
    res = copy_object(res, p);
    erts_free(ERTS_ALC_T_TMP, (void *) heap_start);
    return res;
}


/*
** BIF interface
*/
BIF_RETTYPE get_0(BIF_ALIST_0)
{
    Eterm ret;
    PD_CHECK(BIF_P->dictionary);
    ret = pd_hash_get_all(BIF_P, BIF_P->dictionary);
    PD_CHECK(BIF_P->dictionary);
    BIF_RET(ret);
}

BIF_RETTYPE get_1(BIF_ALIST_1)
{
    Eterm ret;
    PD_CHECK(BIF_P->dictionary);
    ret = erts_pd_hash_get(BIF_P, BIF_ARG_1);
    PD_CHECK(BIF_P->dictionary);
    BIF_RET(ret);
}

BIF_RETTYPE get_keys_0(BIF_ALIST_0)
{
    Eterm ret;

    PD_CHECK(BIF_P->dictionary);
    ret = pd_hash_get_all_keys(BIF_P,BIF_P->dictionary);
    PD_CHECK(BIF_P->dictionary);
    BIF_RET(ret);
}

BIF_RETTYPE get_keys_1(BIF_ALIST_1)
{
    Eterm ret;

    PD_CHECK(BIF_P->dictionary);
    ret = pd_hash_get_keys(BIF_P, BIF_ARG_1);
    PD_CHECK(BIF_P->dictionary);
    BIF_RET(ret);
}

BIF_RETTYPE put_2(BIF_ALIST_2)
{
    Eterm ret;

    PD_CHECK(BIF_P->dictionary);
    ret = pd_hash_put(BIF_P, BIF_ARG_1, BIF_ARG_2);
    PD_CHECK(BIF_P->dictionary);
    BIF_RET(ret);
}

BIF_RETTYPE erase_0(BIF_ALIST_0)
{
    Eterm ret;
    PD_CHECK(BIF_P->dictionary);
    ret = pd_hash_get_all(BIF_P, BIF_P->dictionary);
    pd_hash_erase_all(BIF_P);
    PD_CHECK(BIF_P->dictionary);
    BIF_RET(ret);
}

BIF_RETTYPE erase_1(BIF_ALIST_1)
{
    Eterm ret;
    PD_CHECK(BIF_P->dictionary);
    pd_hash_erase(BIF_P, BIF_ARG_1, &ret);
    PD_CHECK(BIF_P->dictionary);
    BIF_RET(ret);
}

/*
 * BIF implementations
 */
static void pd_hash_erase(Process *p, Eterm id, Eterm *ret)
{
    unsigned int hval;
    Eterm old;
    Eterm tmp;
    unsigned int range;

    *ret = am_undefined;
    if (p->dictionary == NULL) {
	return;
    }
    hval = pd_hash_value(p->dictionary, id);
    old = ARRAY_GET(p->dictionary, hval);
    if (is_boxed(old)) {	/* Tuple */
	ASSERT(is_tuple(old));
	if (EQ(tuple_val(old)[1], id)) {
	    array_put(&(p->dictionary), hval, NIL);
	    --(p->dictionary->numElements);
	    *ret = tuple_val(old)[2];
	}
    } else if (is_list(old)) {
	/* Find cons cell for identical value */
	Eterm* prev = &p->dictionary->data[hval];

	for (tmp = *prev; tmp != NIL; prev = &TCDR(tmp), tmp = *prev) {
	    if (EQ(tuple_val(TCAR(tmp))[1], id)) {
		*prev = TCDR(tmp);
		*ret = tuple_val(TCAR(tmp))[2];
		--(p->dictionary->numElements);
	    }
	}

	/* If there is only one element left in the list we must remove the list. */
	old = ARRAY_GET(p->dictionary, hval);
	ASSERT(is_list(old));
	if (is_nil(TCDR(old))) {
	    array_put(&p->dictionary, hval, TCAR(old));
	}
    } else if (is_not_nil(old)) {
#ifdef DEBUG
	erts_fprintf(stderr,
		     "Process dictionary for process %T is broken, trying to "
		     "display term found in line %d:\n"
		     "%T\n", p->common.id, __LINE__, old);
#endif
	erts_exit(ERTS_ERROR_EXIT, "Damaged process dictionary found during erase/1.");
    }
    if ((range = HASH_RANGE(p->dictionary)) > INITIAL_SIZE && 
	range / 2  > (p->dictionary->numElements)) {
	shrink(p, ret);
    }
}

static void pd_hash_erase_all(Process *p)
{
    if (p->dictionary != NULL) {
	PD_FREE(p->dictionary, PD_SZ2BYTES(p->dictionary->size));
	p->dictionary = NULL;
    }
}

Eterm erts_pd_hash_get(Process *p, Eterm id) 
{
    unsigned int hval;
    Eterm tmp;
    ProcDict *pd = p->dictionary;

    if (pd == NULL)
	return am_undefined;
    hval = pd_hash_value(pd, id);
    tmp = ARRAY_GET(pd, hval);
    if (is_boxed(tmp)) {	/* Tuple */
	ASSERT(is_tuple(tmp));
	if (EQ(tuple_val(tmp)[1], id)) {
	    return tuple_val(tmp)[2];
	}
    } else if (is_list(tmp)) {
	for (; tmp != NIL && !EQ(tuple_val(TCAR(tmp))[1], id); tmp = TCDR(tmp)) {
	    ;
	}
	if (tmp != NIL) {
	    return tuple_val(TCAR(tmp))[2];
	}
    } else if (is_not_nil(tmp)) {
#ifdef DEBUG
	erts_fprintf(stderr,
		     "Process dictionary for process %T is broken, trying to "
		     "display term found in line %d:\n"
		     "%T\n", p->common.id, __LINE__, tmp);
#endif
	erts_exit(ERTS_ERROR_EXIT, "Damaged process dictionary found during get/1.");
    }
    return am_undefined;
}

#define PD_GET_TKEY(Dst,Src)                            \
do {                                                    \
    ASSERT(is_tuple((Src)));                            \
    ASSERT(arityval(*((Eterm*)tuple_val((Src)))) == 2); \
    (Dst) = ((Eterm*)tuple_val((Src)))[1];              \
} while(0)

static Eterm pd_hash_get_all_keys(Process *p, ProcDict *pd) {
    Eterm* hp;
    Eterm res = NIL;
    Eterm tmp, tmp2;
    unsigned int i;
    unsigned int num;

    if (pd == NULL) {
	return res;
    }

    num = HASH_RANGE(pd);
    hp = HAlloc(p, pd->numElements * 2);

    for (i = 0; i < num; ++i) {
	tmp = ARRAY_GET(pd, i);
	if (is_boxed(tmp)) {
	    PD_GET_TKEY(tmp,tmp);
	    res = CONS(hp, tmp, res);
	    hp += 2;
	} else if (is_list(tmp)) {
	    while (tmp != NIL) {
		tmp2 = TCAR(tmp);
		PD_GET_TKEY(tmp2,tmp2);
		res = CONS(hp, tmp2, res);
		hp += 2;
		tmp = TCDR(tmp);
	    }
	}
    }
    return res;
}
#undef PD_GET_TKEY

static Eterm pd_hash_get_keys(Process *p, Eterm value) 
{
    Eterm *hp;
    Eterm res = NIL;
    ProcDict *pd = p->dictionary;
    unsigned int i, num;
    Eterm tmp, tmp2;

    if (pd == NULL) {
	return res;
    }

    num = HASH_RANGE(pd);
    for (i = 0; i < num; ++i) {
	tmp = ARRAY_GET(pd, i);
	if (is_boxed(tmp)) {
	    ASSERT(is_tuple(tmp));
	    if (EQ(tuple_val(tmp)[2], value)) {
		hp = HAlloc(p, 2);
		res = CONS(hp, tuple_val(tmp)[1], res);
	    }
	} else if (is_list(tmp)) {
	    while (tmp != NIL) {
		tmp2 = TCAR(tmp);
		if (EQ(tuple_val(tmp2)[2], value)) {
		    hp = HAlloc(p, 2);
		    res = CONS(hp, tuple_val(tmp2)[1], res);
		}
		tmp = TCDR(tmp);
	    }
	}
    }
    return res;
}
	

static Eterm
pd_hash_get_all(Process *p, ProcDict *pd)
{
    Eterm* hp;
    Eterm res = NIL;
    Eterm tmp, tmp2;
    unsigned int i;
    unsigned int num;

    if (pd == NULL) {
	return res;
    }
    num = HASH_RANGE(pd);
    hp = HAlloc(p, pd->numElements * 2);
    
    for (i = 0; i < num; ++i) {
	tmp = ARRAY_GET(pd, i);
	if (is_boxed(tmp)) {
	    ASSERT(is_tuple(tmp));
	    res = CONS(hp, tmp, res);
	    hp += 2;
	} else if (is_list(tmp)) {
	    while (tmp != NIL) {
		tmp2 = TCAR(tmp);
		res = CONS(hp, tmp2, res);
		hp += 2;
		tmp = TCDR(tmp);
	    }
	}
    }
    return res;
}

static Eterm pd_hash_put(Process *p, Eterm id, Eterm value)
{ 
    unsigned int hval;
    Eterm *hp;
    Eterm tpl;
    Eterm old;
    Eterm tmp;
    int needed;
    int i = 0;
#ifdef DEBUG
    Eterm *hp_limit;
#endif

    if (p->dictionary == NULL) {
	/* Create it */
	array_put(&(p->dictionary), INITIAL_SIZE - 1, NIL);
	p->dictionary->homeSize = INITIAL_SIZE;
    }	
    hval = pd_hash_value(p->dictionary, id);
    old = ARRAY_GET(p->dictionary, hval);

    /*
     * Calculate the number of heap words needed and garbage
     * collect if necessary. (Might be a slight overestimation.)
     */
    needed = 3;			/* {Key,Value} tuple */
    if (is_boxed(old)) {
	/*
	 * We don't want to compare keys twice, so we'll always
	 * reserve the space for two CONS cells.
	 */
	needed += 2+2;
    } else if (is_list(old)) {
	i = 0;
	for (tmp = old; tmp != NIL && !EQ(tuple_val(TCAR(tmp))[1], id); tmp = TCDR(tmp)) {
	    ++i;
	}
	if (is_nil(tmp)) {
	    i = -1;
	    needed += 2;
	} else {
	    needed += 2*(i+1);
	}
    }
    if (HeapWordsLeft(p) < needed) {
	Eterm root[3];
	root[0] = id;
	root[1] = value;
	root[2] = old;
	BUMP_REDS(p, erts_garbage_collect(p, needed, root, 3));
	id = root[0];
	value = root[1];
	old = root[2];
    }
#ifdef DEBUG
    hp_limit = p->htop + needed;
#endif

    /*
     * Create the {Key,Value} tuple.
     */
    hp = HeapOnlyAlloc(p, 3);
    tpl = TUPLE2(hp, id, value);

    /*
     * Update the dictionary.
     */
    if (is_nil(old)) {
	array_put(&(p->dictionary), hval, tpl);
	++(p->dictionary->numElements);
    } else if (is_boxed(old)) {
	ASSERT(is_tuple(old));
	if (EQ(tuple_val(old)[1],id)) {
	    array_put(&(p->dictionary), hval, tpl);
	    return tuple_val(old)[2];
	} else {
	    hp = HeapOnlyAlloc(p, 4);
	    tmp = CONS(hp, old, NIL);
	    hp += 2;
	    ++(p->dictionary->numElements);
	    array_put(&(p->dictionary), hval, CONS(hp, tpl, tmp));
	    hp += 2;
	    ASSERT(hp <= hp_limit);
	}
    } else if (is_list(old)) {
	if (i == -1) {
	    /*
	     * New key. Simply prepend the tuple to the beginning of the list.
	     */
	    hp = HeapOnlyAlloc(p, 2);
	    array_put(&(p->dictionary), hval, CONS(hp, tpl, old));
	    hp += 2;
	    ASSERT(hp <= hp_limit);
	    ++(p->dictionary->numElements);
	} else {
	    /*
	     * i = Number of CDRs to skip to reach the changed element in the list.
	     *
	     * Replace old value in list. To avoid pointers from the old generation
	     * to the new, we must rebuild the list from the beginning up to and
	     * including the changed element.
	     */
	    Eterm nlist;
	    int j;

	    hp = HeapOnlyAlloc(p, (i+1)*2);
	    
	    /* Find the list element to change. */
	    for (j = 0, nlist = old; j < i; j++, nlist = TCDR(nlist)) {
		;
	    }
	    ASSERT(EQ(tuple_val(TCAR(nlist))[1], id));
	    nlist = TCDR(nlist); /* Unchanged part of list. */

	    /* Rebuild list before the updated element. */
	    for (tmp = old; i-- > 0; tmp = TCDR(tmp)) {
		nlist = CONS(hp, TCAR(tmp), nlist);
		hp += 2;
	    }
	    ASSERT(EQ(tuple_val(TCAR(tmp))[1], id));

	    /* Put the updated element first in the new list. */
	    nlist = CONS(hp, tpl, nlist);
	    hp += 2;
	    ASSERT(hp <= hp_limit);
	    array_put(&(p->dictionary), hval, nlist);
	    return tuple_val(TCAR(tmp))[2];
	}
    } else {
#ifdef DEBUG
	erts_fprintf(stderr,
		     "Process dictionary for process %T is broken, trying to "
		     "display term found in line %d:\n"
		     "%T\n", p->common.id, __LINE__, old);
#endif

	erts_exit(ERTS_ERROR_EXIT, "Damaged process dictionary found during put/2.");
    }
    if (HASH_RANGE(p->dictionary) <= p->dictionary->numElements) {
	grow(p);
    }
    return am_undefined;
}

/*
 * Hash table utilities, rehashing
 */

static void shrink(Process *p, Eterm* ret) 
{
    unsigned int range = HASH_RANGE(p->dictionary);
    unsigned int steps = (range*3) / 10;
    Eterm hi, lo, tmp;
    unsigned int i;
    Eterm *hp;
#ifdef DEBUG
    Eterm *hp_limit;
#endif

    if (range - steps < INITIAL_SIZE) {
	steps = range - INITIAL_SIZE; 
    }

    for (i = 0; i < steps; ++i) {
	ProcDict *pd = p->dictionary;
	if (pd->splitPosition == 0) {
	    pd->homeSize /= 2;
	    pd->splitPosition = pd->homeSize;
	}
	--(pd->splitPosition);
	hi = ARRAY_GET(pd, (pd->splitPosition + pd->homeSize));
	lo = ARRAY_GET(pd, pd->splitPosition);
	if (hi != NIL) {
	    if (lo == NIL) {
		array_put(&(p->dictionary), pd->splitPosition, hi);
	    } else {
		int needed = 4;
		if (is_list(hi) && is_list(lo)) {
		    needed = 2*erts_list_length(hi);
		}
		if (HeapWordsLeft(p) < needed) {
		    BUMP_REDS(p, erts_garbage_collect(p, needed, ret, 1));
		    hi = pd->data[(pd->splitPosition + pd->homeSize)];
		    lo = pd->data[pd->splitPosition];
		}
#ifdef DEBUG
		hp_limit = p->htop + needed;
#endif
		if (is_tuple(lo)) {
		    if (is_tuple(hi)) {
			hp = HeapOnlyAlloc(p, 4);
			tmp = CONS(hp, hi, NIL);
			hp += 2;
			array_put(&(p->dictionary), pd->splitPosition, 
				  CONS(hp,lo,tmp));
			hp += 2;
			ASSERT(hp <= hp_limit);
		    } else { /* hi is a list */
			hp = HeapOnlyAlloc(p, 2);
			array_put(&(p->dictionary), pd->splitPosition, 
				  CONS(hp, lo, hi));
			hp += 2;
			ASSERT(hp <= hp_limit);
		    }
		} else { /* lo is a list */
		    if (is_tuple(hi)) {
			hp = HeapOnlyAlloc(p, 2);
			array_put(&(p->dictionary), pd->splitPosition, 
				  CONS(hp, hi, lo));
			hp += 2;
			ASSERT(hp <= hp_limit);

		    } else { /* Two lists */
			hp = HeapOnlyAlloc(p, needed);
			for (tmp = hi; tmp != NIL; tmp = TCDR(tmp)) {
			    lo = CONS(hp, TCAR(tmp), lo);
			    hp += 2;
			}
			ASSERT(hp <= hp_limit);
			array_put(&(p->dictionary), pd->splitPosition, lo);
		    }
		}
	    }
	}
	array_put(&(p->dictionary), (pd->splitPosition + pd->homeSize), NIL);
    }
    if (HASH_RANGE(p->dictionary) <= (p->dictionary->size / 4)) {
	array_shrink(&(p->dictionary), (HASH_RANGE(p->dictionary) * 3) / 2);
    }
}

static void grow(Process *p)
{
    unsigned int i,j;
    unsigned int steps = p->dictionary->homeSize / 5;
    Eterm l1,l2;
    Eterm l;
    Eterm *hp;
    unsigned int pos;
    unsigned int homeSize;
    int needed = 0;
    ProcDict *pd;
#ifdef DEBUG
    Eterm *hp_limit;
#endif

    HDEBUGF(("grow: steps = %d", steps));
    if (steps == 0)
	steps = 1;
    /* Dont grow over MAX_HASH */
    if ((MAX_HASH - steps) <= HASH_RANGE(p->dictionary)) {
	return;
    }

    /*
     * Calculate total number of heap words needed, and garbage collect
     * if necessary.
     */

    pd = p->dictionary;
    pos = pd->splitPosition;
    homeSize = pd->homeSize;
    for (i = 0; i < steps; ++i) {
	if (pos == homeSize) {
	    homeSize *= 2;
	    pos = 0;
	}
	l = ARRAY_GET(pd, pos);
	pos++;
	if (is_not_tuple(l)) {
	    while (l != NIL) {
		needed += 2;
		l = TCDR(l);
	    }
	}
    }
    if (HeapWordsLeft(p) < needed) {
	BUMP_REDS(p, erts_garbage_collect(p, needed, 0, 0));
    }
#ifdef DEBUG
    hp_limit = p->htop + needed;
#endif

    /*
     * Now grow.
     */

    for (i = 0; i < steps; ++i) {
	ProcDict *pd = p->dictionary;
	if (pd->splitPosition == pd->homeSize) {
	    pd->homeSize *= 2;
	    pd->splitPosition = 0;
	}
	pos = pd->splitPosition;
	++pd->splitPosition; /* For the hashes */
	l = ARRAY_GET(pd, pos);
	if (is_tuple(l)) {
	    if (pd_hash_value(pd, tuple_val(l)[1]) != pos) {
		array_put(&(p->dictionary), pos + 
			  p->dictionary->homeSize, l);
		array_put(&(p->dictionary), pos, NIL);
	    }
	} else {
	    l2 = NIL;
	    l1 = l;
	    for (j = 0; l1 != NIL; l1 = TCDR(l1))
		j += 2;
	    hp = HeapOnlyAlloc(p, j);
	
	    while (l != NIL) {
		if (pd_hash_value(pd, tuple_val(TCAR(l))[1]) == pos) 
		    l1 = CONS(hp, TCAR(l), l1);
		else
		    l2 = CONS(hp, TCAR(l), l2);
		hp += 2;
		l = TCDR(l);
	    }
	    if (l1 != NIL && TCDR(l1) == NIL)
		l1 = TCAR(l1);
	    if (l2 != NIL && TCDR(l2) == NIL)
		l2 = TCAR(l2);
	    ASSERT(hp <= hp_limit);
	    /* After array_put pd is no longer valid */
	    array_put(&(p->dictionary), pos, l1);
	    array_put(&(p->dictionary), pos + 
		      p->dictionary->homeSize, l2);
	}
    }

#ifdef HARDDEBUG
    dictionary_dump(p->dictionary,CERR);
#endif
}

/*
** Array oriented operations
*/

static void array_shrink(ProcDict **ppd, unsigned int need) 
{
    unsigned int siz = next_array_size(need);

    HDEBUGF(("array_shrink: size = %d, used = %d, need = %d",
	     (*ppd)->size, (*ppd)->used, need));

    if (siz > (*ppd)->size)
	return; /* Only shrink */

    *ppd = PD_REALLOC(((void *) *ppd),
		      PD_SZ2BYTES((*ppd)->size),
		      PD_SZ2BYTES(siz));

    (*ppd)->size = siz;
    if ((*ppd)->size < (*ppd)->used)
	(*ppd)->used = (*ppd)->size;
}
    
			
static Eterm array_put(ProcDict **ppdict, unsigned int ndx, Eterm term)
{
    unsigned int i;
    Eterm ret;
    if (*ppdict == NULL) {
	Uint siz = next_array_size(ndx+1);
	ProcDict *p;

        p = PD_ALLOC(PD_SZ2BYTES(siz));
	for (i = 0; i < siz; ++i) 
	    p->data[i] = NIL;
	p->size = siz;
	p->homeSize = p->splitPosition = p->numElements = p->used = 0;
	*ppdict = p;
    } else if (ndx >= (*ppdict)->size) {
	Uint osize = (*ppdict)->size;
	Uint nsize = next_array_size(ndx+1);
	*ppdict = PD_REALLOC(((void *) *ppdict),
			     PD_SZ2BYTES(osize),
			     PD_SZ2BYTES(nsize));
	for (i = osize; i < nsize; ++i)
	    (*ppdict)->data[i] = NIL;
	(*ppdict)->size = nsize;
    }
    ret = (*ppdict)->data[ndx];
    (*ppdict)->data[ndx] = term;
    if ((ndx + 1) > (*ppdict)->used)
	(*ppdict)->used = ndx + 1;
#ifdef HARDDEBUG
    HDEBUGF(("array_put: (*ppdict)->size = %d, (*ppdict)->used = %d, ndx = %d",
	     (*ppdict)->size, (*ppdict)->used, ndx));
    erts_fprintf(stderr, "%T", term);
#endif /* HARDDEBUG */
    return ret;
}

/*
** Basic utilities
*/

static unsigned int pd_hash_value(ProcDict *pdict, Eterm term) 
{ 
    Uint hash, high;

    hash = MAKE_HASH(term);
    high = hash % (pdict->homeSize*2);
    if (high >= HASH_RANGE(pdict))
	return hash % pdict->homeSize;
    return high;
}

static unsigned int next_array_size(unsigned int need)
{
    static unsigned int tab[] =
    {
	10UL,
	20UL,
	40UL,
	80UL,
	160UL,
	320UL,
	640UL,
	1280UL,
	2560UL,
	5120UL,
	10240UL,
	20480UL,
	40960UL,
	81920UL,
	163840UL,
	327680UL,
	655360UL,
	1310720UL,
	2621440UL,
	5242880UL,
	10485760UL,
	20971520UL,
	41943040UL,
	83886080UL,
	167772160UL,
	335544320UL,
	671088640UL,
	1342177280UL,
	2684354560UL
    };
    int hi = sizeof(tab) / sizeof(Uint) - 1;
    int lo = 1;
    int cur = 4;

    while (hi >= lo) {
	if (tab[cur] >= need && tab[cur - 1] < need)
	    return tab[cur];
	if (tab[cur] > need)
	    hi = cur - 1;
	else
	    lo = cur + 1;
	cur = (hi + lo) / 2;
    }
    return need;
}


/*
** Debug functions 
*/	    
#ifdef DEBUG

static void pd_check(ProcDict *pd) 
{
    unsigned int i;
    Uint num;
    if (pd == NULL)
	return;
    ASSERT(pd->size >= pd->used);
    ASSERT(HASH_RANGE(pd) <= MAX_HASH);
    for (i = 0, num = 0; i < pd->used; ++i) {
	Eterm t = pd->data[i];
	if (is_nil(t)) {
	    continue;
	} else if (is_tuple(t)) {
	    ++num;
	    ASSERT(arityval(*tuple_val(t)) == 2);
	    continue;
	} else if (is_list(t)) {
	    while (t != NIL) {
		++num;
		ASSERT(is_tuple(TCAR(t)));
		ASSERT(arityval(*(tuple_val(TCAR(t)))) == 2);
		t = TCDR(t);
	    }
	    continue;
	} else {
	    erts_exit(ERTS_ERROR_EXIT,
		     "Found tag 0x%08x in process dictionary at position %d",
		     (unsigned long) t, (int) i);
	}
    }
    ASSERT(num == pd->numElements);
    ASSERT(pd->splitPosition <= pd->homeSize);
}

#endif /* DEBUG */


#ifdef HARDDEBUG

static int hdebugf(char *format, ...)
{
    va_list ap;

    erts_fprintf(stderr, "DEBUG: %s:%d :", hdebugf_file, hdebugf_line);
    va_start(ap, format);
    erts_vfprintf(stderr, format, ap);
    va_end(ap);
    erts_fprintf(stderr, "\n");
    return 0;
} 

#endif /* HARDDEBUG */

