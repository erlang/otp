/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1999-2018. All Rights Reserved.
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
#include "bif.h"
#include "erl_binary.h"

static Export subtract_trap_export;

static Eterm keyfind(int Bif, Process* p, Eterm Key, Eterm Pos, Eterm List);

void erts_init_bif_lists() {
    erts_init_trap_export(&subtract_trap_export, am_lists, am_subtract, 2,
                          &subtract_2);
}

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


/* If list size (precalculated outside of this fun) fits into static_array, then
 * use it and copy elements from list to it. Otherwise allocate a new array and
 * copy elements into it. */
static Eterm *
subtract_maybe_alloc(Sint src_size, Eterm *static_array, Sint static_size) {
    Eterm *dst;
    if (src_size > static_size) {
        dst = (Eterm *) erts_alloc(ERTS_ALC_T_TMP, src_size * sizeof(Eterm));
    } else {
        dst = static_array;
    }
    return dst;
}


/* Copy elements from list 'src' into 'dst' */
static ERTS_INLINE void
subtract_copy_list(Eterm src, Sint src_size, Eterm *dst) {
    Eterm *dst_p = dst;
    for (Sint i = src_size - 1; i >= 0; i--) {
        const Eterm *src_p = list_val(src);
        *dst_p++ = CAR(src_p);
        src = CDR(src_p);
    }
}


/* Naive search of val in array of array_sz elements.
 * Returns index where found or -1 if not found */
static ERTS_INLINE Sint
subtract_find_in_array(Eterm val, const Eterm *array_p, Sint array_sz) {
    for (Sint i = array_sz - 1; i >= 0; i--) {
        if (eq(val, array_p[i])) {
            return i;
        }
    }
    return -1;
}


enum { SUBTRACT_SMALL_VEC_SIZE = 10 };
#define SUBTRACT_SPECIAL_VALUE am_bif_return_trap

typedef enum {
    SUBTRACT_STEP_LEN_A = 1, /* calculate length of A */
    SUBTRACT_STEP_LEN_B = 2, /* calculate length of B */
    SUBTRACT_STEP_SCAN = 3   /* scan A and copy elements not in B to result */
} ErtsSubtractFsmState;

/* Context used for trapping length calculation */
typedef struct {
    Eterm iter;
    Sint count;
} ErtsSubtractLengthContext;

/* Context used for trapping scanning of inputs */
typedef struct {
    /* Preallocated memory for small lists */
    Eterm small_vec_a[SUBTRACT_SMALL_VEC_SIZE];
    Eterm small_vec_b[SUBTRACT_SMALL_VEC_SIZE];

    /* Dynamically allocated copy of B, for shrinking it when elements are
     * found and removed */
    Eterm *copy_of_b;

    /* iterator over A when subtracting and moving remaining elts */
    Eterm iter_a;

    /* Dynamically allocated result output array (on TMP heap) */
    Eterm *result_array;
    Sint result_size;
} ErtsSubtractScanContext;

typedef struct {
    /* Allows doing work in stages possibly interrupting on large inputs */
    ErtsSubtractFsmState fsm_state;
    Sint len_a;
    Sint len_b;
    union {
        ErtsSubtractLengthContext len;
        ErtsSubtractScanContext scan;
    } s;
} ErtsSubtractContext;


/*
 * Return either NIL, A or build new result list from state.result_array
 */
static ERTS_INLINE Eterm
subtract_build_result(Process *p, Eterm A, ErtsSubtractContext *context) {
    const ErtsSubtractScanContext *s = &context->s.scan;
    const Sint result_size = s->result_size;
    Eterm *write_p;
    const Eterm *read_p;
    Eterm res;

    if (result_size == 0) { /* All deleted? Return a [] */
        return NIL;
    } else if (result_size == context->len_a) {
        /* None deleted? Return the original A */
        return A;
    }

    /* Deleted some elements and A still not empty - Rebuild the new A */
    write_p = HAlloc(p, (Uint) (2 * result_size));
    read_p = s->result_array + result_size - 1;
    res = NIL;
    while (read_p >= s->result_array) {
        if (is_value(*read_p)) {
            res = CONS(write_p, *read_p, res);
            write_p += 2;
        }
        read_p--;
    }
    return res;
}


/* Constructor for state struct, called on creation */
static ERTS_INLINE void
subtractcontext_ctor(ErtsSubtractContext *context) {
    memset((void *)context, 0, sizeof(ErtsSubtractContext));
    context->fsm_state = SUBTRACT_STEP_LEN_A;
    /* Some platforms have different idea of NULL, remember to set fields 
     * when transitioning to the SCAN step */
}


static int
subtractcontext_dtor(ErtsSubtractContext *context) {
    ErtsSubtractScanContext *s;
    if (context->fsm_state != SUBTRACT_STEP_SCAN) {
        /* nothing to do if we didn't allocate any memory yet */
        return 1;
    }

    s = &context->s.scan;
    if (s->result_array && s->result_array != s->small_vec_a) {
        erts_free(ERTS_ALC_T_TMP, (void *) s->result_array);
        s->result_array = NULL;
    }
    if (s->copy_of_b && s->copy_of_b != s->small_vec_b) {
        erts_free(ERTS_ALC_T_TMP, (void *) s->copy_of_b);
        s->copy_of_b = NULL;
    }
    return 1;
}


/* Called when magic binary with ErtsSubtractContext is destroyed */
static int
subtractcontext_binary_dtor(Binary *context_bin) {
    ErtsSubtractContext *ctx = ERTS_MAGIC_BIN_DATA(context_bin);
    return subtractcontext_dtor(ctx);
}


/* Prepare magic ref to magic binary with our state.
 * The magic binary with context in it should already be created.
 * Return a triple {A, B, MagicRef}
 */
static ERTS_INLINE Eterm
subtract_create_returnstate(Process *p, Eterm A, Eterm B, Binary *context_b) {
    const static int TUPLE3_SIZE = 3 + 1;
    Eterm *hp = HAlloc(p, ERTS_MAGIC_REF_THING_SIZE + TUPLE3_SIZE);
    Eterm state_binref = erts_mk_magic_ref(&hp, &MSO(p), context_b);
    BUMP_ALL_REDS(p);
    return TUPLE3(hp, A, B, state_binref);
}


/* Iterate over A and for each element that is present in B, skip it
 * in A and remove it from B (by moving last element of B in its place and
 * making B shorter), otherwise that element is copied to the result.
 * Repeat until either end of A is reached or B is empty.
 *
 * Result is built in s->result_array of Eterm[result_size]
 */
static Eterm
subtract_fsm_scan(Process *p, Eterm A, ErtsSubtractContext *context) {
    ErtsSubtractScanContext *s = &context->s.scan;
    Eterm iter_a = s->iter_a; /* load iterator from state */

    /* A is guaranteed to be proper list, otherwise erts_list_length() call at
     * start will fail */
    while (is_list(iter_a)) {
        const Eterm *ptr_a = list_val(iter_a);
        Sint found_at = subtract_find_in_array(CAR(ptr_a), s->copy_of_b,
                                               context->len_b);
        if (found_at >= 0) {
            /* Remove elem from B and skip; shrink B by 1 */
            context->len_b--;
            s->copy_of_b[found_at] = s->copy_of_b[context->len_b];

            if (context->len_b == 0) {
                iter_a = CDR(ptr_a);

                /* B is empty at this point, so copy remaining A elements */
                while (is_list(iter_a)) {
                    ptr_a = list_val(iter_a);
                    s->result_array[s->result_size] = CAR(ptr_a);
                    s->result_size++;
                    iter_a = CDR(ptr_a);
                }
                break; /* nothing left in B, end search here */
            }
        } else {
            s->result_array[s->result_size] = CAR(ptr_a);
            s->result_size++;
        }
        iter_a = CDR(ptr_a);
    }

    s->iter_a = iter_a; /* save iterator */
    return subtract_build_result(p, A, context);
}


/*
 * Stage 0 creates binary state for subsequent stages
 */
static Binary *
subtract_fsm_construct_context(Process *_p, Eterm A, Eterm B) {
    ErtsSubtractContext *context;

    /* Context stored in magic binary. If this is not null, we will be trapping
     * periodically otherwise we will run operation till the end & return. */
    Binary *state_bin = NULL;

    /* Allocate tmp binary and use it for state */
    state_bin = erts_create_magic_binary(sizeof(ErtsSubtractContext),
                                         subtractcontext_binary_dtor);

    context = ERTS_MAGIC_BIN_DATA(state_bin);
    subtractcontext_ctor(context);

    return state_bin;
}


/*
 * Setup variables for entering length calculation state.
 */
static ERTS_INLINE void
subtract_enter_state_len_of(Eterm T, ErtsSubtractContext *context) {
    ErtsSubtractLengthContext *lc = &context->s.len;
    lc->count = 0;
    lc->iter = T;
}


/*
 * Stages 1 and 2 perform length calculation on lists A and B respectively.
 * Possibly trapping if the lists are too long.
 * Reentering this stage
 */
static Eterm
subtract_fsm_len(Process *p, ErtsSubtractContext *context) {
    context->s.len.count = erts_list_length(context->s.len.iter);
    return am_true;
}


/* Stage 3 creates binary or on-stack state and performs the subtraction
 * also possibly trapping if the inputs are too long. */
static ERTS_INLINE void
subtract_enter_state_scan(Process *p, Eterm A, Eterm B,
                          ErtsSubtractContext *context) {
    ErtsSubtractScanContext *s = &context->s.scan;
    s->result_array = subtract_maybe_alloc(context->len_a, s->small_vec_a,
                                           SUBTRACT_SMALL_VEC_SIZE);
    s->copy_of_b = subtract_maybe_alloc(context->len_b, s->small_vec_b,
                                        SUBTRACT_SMALL_VEC_SIZE);
    subtract_copy_list(B, context->len_b, s->copy_of_b);

    /* Initialise loop counters and run the loop */
    s->result_size = 0;
    s->iter_a = A;
}


/* Unpack the state triple and based on step field do something.
 * To enter here, A must be NON VALUE and B must be {OrigA, OrigB, MagicBin}
 */
static ERTS_INLINE Eterm
subtract_switch(Process *p, Eterm special_arg, Eterm a_b_state) {
    Binary *magic_bin;
    ErtsSubtractContext *context;
    Eterm *a_b_state_ptr;
    Eterm orig_A, orig_B;

    ERTS_ASSERT(special_arg == SUBTRACT_SPECIAL_VALUE);
    ERTS_ASSERT(is_tuple(a_b_state));
    ERTS_ASSERT(is_tuple_arity(a_b_state, 3));
    a_b_state_ptr = tuple_val(a_b_state);
    orig_A = a_b_state_ptr[1];
    orig_B = a_b_state_ptr[2];

    magic_bin = erts_magic_ref2bin(a_b_state_ptr[3]);
    context = ERTS_MAGIC_BIN_DATA(magic_bin);

    switch (context->fsm_state) {
        case SUBTRACT_STEP_LEN_A: {
            Eterm res = subtract_fsm_len(p, context);
            Sint count;
            if (res != am_true) {
                BIF_TRAP2(&subtract_trap_export, p,
                          SUBTRACT_SPECIAL_VALUE, a_b_state);
            }
            count = context->s.len.count;
            /* Returning true means length is done, result in s.len.count */
            if (count < 0) {
                BIF_ERROR(p, BADARG);
            }
            if (count == 0) {
                BIF_RET(NIL);
            }
            context->len_a = count;
            context->fsm_state = SUBTRACT_STEP_LEN_B;
            subtract_enter_state_len_of(orig_B, context);
            /* FALL THROUGH */
        }

        case SUBTRACT_STEP_LEN_B: {
            Eterm res = subtract_fsm_len(p, context);
            Sint count;
            if (res != am_true) {
                BIF_TRAP2(&subtract_trap_export, p,
                          SUBTRACT_SPECIAL_VALUE, a_b_state);
            }
            if (NULL != 0) {
                /* Some platforms have different idea of NULL, set fields like this */
                context->s.scan.result_array = NULL;
                context->s.scan.copy_of_b = NULL;
            }
            count = context->s.len.count;
            /* Returning true means length is done, result in s.len.count */
            if (count < 0) {
                BIF_ERROR(p, BADARG);
            }
            if (count == 0) {
                BIF_RET(orig_A);
            }
            context->len_b = count;
            context->fsm_state = SUBTRACT_STEP_SCAN;
            subtract_enter_state_scan(p, orig_A, orig_B, context);
            /* FALL THROUGH */
        }

        case SUBTRACT_STEP_SCAN: {
            Eterm res = subtract_fsm_scan(p, orig_A, context);
            if (res == SUBTRACT_SPECIAL_VALUE) {
                BIF_TRAP2(&subtract_trap_export, p,
                          SUBTRACT_SPECIAL_VALUE, a_b_state);
            }
            subtractcontext_dtor(context);
            BIF_RET(res);
        }
    }
    BIF_ERROR(p, EXC_BADFUN); /* bloop. Unreachable, should not be here */
}


/*
 * erlang:'--'/2
 * Calculates A -- B for two lists; complexity proportional to len(A)*len(B)
 *
 * B is copied into a flat array and gets shrunk every time a value from B is
 * found in A, this speeds up the search as B gets shorter.
 *
 * Args: A :: proper_list(), B :: proper_list() - create state and enter stage0
 * Args: A :: nonvalue(), B :: {list(), list(), magic_binary()} - continue
 *  The binary contains ErtsSubtractContext struct.
 *  The step field in state will define next action.
 */
static Eterm
subtract(Process *p, Eterm A, Eterm B) {
    if (A != SUBTRACT_SPECIAL_VALUE && ! is_tuple(B)) {
        /* This is initial call entry from Erlang, both args are lists, no ctx
         * is yet created. So create state here. */
        Binary *state_bin = subtract_fsm_construct_context(p, A, B);

        /* Enter now with state */
        ErtsSubtractContext *context = ERTS_MAGIC_BIN_DATA(state_bin);
        subtract_enter_state_len_of(A, context);
        return subtract_switch(
                p, SUBTRACT_SPECIAL_VALUE,
                subtract_create_returnstate(p, A, B, state_bin));
    }

    /* Continue. There A must be non value and B must be triple
     * {OrigA, OrigB, StateMagicBin} */
    return subtract_switch(p, A, B);
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

static BIF_RETTYPE lists_reverse_alloc(Process *c_p,
                                       Eterm list_in,
                                       Eterm tail_in)
{
    static const Uint CELLS_PER_RED = 40;

    Eterm *heap_top, *heap_end;
    Uint cells_left, max_cells;
    Eterm list, tail;
    Eterm lookahead;

    list = list_in;
    tail = tail_in;

    cells_left = max_cells = CELLS_PER_RED * (1 + ERTS_BIF_REDS_LEFT(c_p));
    lookahead = list;

    while (cells_left != 0 && is_list(lookahead)) {
        lookahead = CDR(list_val(lookahead));
        cells_left--;
    }

    BUMP_REDS(c_p, (max_cells - cells_left) / CELLS_PER_RED);

    if (is_not_list(lookahead) && is_not_nil(lookahead)) {
        BIF_ERROR(c_p, BADARG);
    }

    heap_top = HAlloc(c_p, 2 * (max_cells - cells_left));
    heap_end = heap_top + 2 * (max_cells - cells_left);

    while (heap_top < heap_end) {
        Eterm *pair = list_val(list);

        tail = CONS(heap_top, CAR(pair), tail);
        list = CDR(pair);

        ASSERT(is_list(list) || is_nil(list));

        heap_top += 2;
    }

    if (is_nil(list)) {
        BIF_RET(tail);
    }

    ASSERT(is_list(tail) && cells_left == 0);
    BIF_TRAP2(bif_export[BIF_lists_reverse_2], c_p, list, tail);
}

static BIF_RETTYPE lists_reverse_onheap(Process *c_p,
                                        Eterm list_in,
                                        Eterm tail_in)
{
    static const Uint CELLS_PER_RED = 60;

    Eterm *heap_top, *heap_end;
    Uint cells_left, max_cells;
    Eterm list, tail;

    list = list_in;
    tail = tail_in;

    cells_left = max_cells = CELLS_PER_RED * (1 + ERTS_BIF_REDS_LEFT(c_p));

    ASSERT(HEAP_LIMIT(c_p) >= HEAP_TOP(c_p) + 2);
    heap_end = HEAP_LIMIT(c_p) - 2;
    heap_top = HEAP_TOP(c_p);

    while (heap_top < heap_end && is_list(list)) {
        Eterm *pair = list_val(list);

        tail = CONS(heap_top, CAR(pair), tail);
        list = CDR(pair);

        heap_top += 2;
    }

    cells_left -= (heap_top - heap_end) / 2;
    BUMP_REDS(c_p, (max_cells - cells_left) / CELLS_PER_RED);
    HEAP_TOP(c_p) = heap_top;

    if (is_nil(list)) {
        BIF_RET(tail);
    } else if (is_list(list)) {
        ASSERT(is_list(tail));

        if (cells_left > CELLS_PER_RED) {
            return lists_reverse_alloc(c_p, list, tail);
        }

        BUMP_ALL_REDS(c_p);
        BIF_TRAP2(bif_export[BIF_lists_reverse_2], c_p, list, tail);
    }

    BIF_ERROR(c_p, BADARG);
}

BIF_RETTYPE lists_reverse_2(BIF_ALIST_2)
{
    /* Handle legal and illegal non-lists quickly. */
    if (is_nil(BIF_ARG_1)) {
        BIF_RET(BIF_ARG_2);
    } else if (is_not_list(BIF_ARG_1)) {
        BIF_ERROR(BIF_P, BADARG);
    }

    /* We build the reversal on the unused part of the heap if possible to save
     * us the trouble of having to figure out the list size. We fall back to
     * lists_reverse_alloc when we run out of space. */
    if (HeapWordsLeft(BIF_P) > 8) {
        return lists_reverse_onheap(BIF_P, BIF_ARG_1, BIF_ARG_2);
    }

    return lists_reverse_alloc(BIF_P, BIF_ARG_1, BIF_ARG_2);
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
