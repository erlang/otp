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

#if defined(DEBUG)
    #define IF_DEBUG(VAL_DEBUG, IGNORE) (VAL_DEBUG)
#else
    #define IF_DEBUG(IGNORE, VAL_RELEASE) (VAL_RELEASE)
#endif


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


typedef struct {
    Eterm src;
    Eterm *dst;
} ErtsCopyListToArrayContext;


static ERTS_INLINE void
erts_trappable_copy_list_to_array_setup(Eterm src, Eterm *dst,
                                        ErtsCopyListToArrayContext *context) {
    context->src = src;
    context->dst = dst;
}


/*
 * Copy elements from list 'src' into array 'dst' of src_size elements
 *
 * Before start: call erts_trappable_copy_list_to_array_setup on the context
 * Returns: 1 if finished, 0 if not finished, please call again.
 */
static int
erts_trappable_copy_list_to_array(Process *p,
                                  ErtsCopyListToArrayContext *context) {
    /* Step through so many elements of list per available reduction */
    const static Sint COPIES_PER_REDUC = 30;
    const Sint budget = COPIES_PER_REDUC * IF_DEBUG(p->fcalls / 10 + 1,
                                                    p->fcalls);
    register Sint reds = budget;

    Eterm src = context->src;
    Eterm *dst = context->dst;

    while (reds && is_list(src)) {
        const Eterm *src_p = list_val(src);
        *dst++ = CAR(src_p);
        src = CDR(src_p);
        reds--;
    }

    BUMP_REDS(p, budget - reds);
    if (reds) {
        /* Work is finished */
        return 1;
    }
    context->src = src;
    context->dst = dst;
    return 0;
}


/* Naive search of val in array of array_sz elements.
 * Returns index where found or -1 if not found */
static ERTS_INLINE Sint
subtract_find_in_array(Eterm val, const Eterm *array_p, Sint array_sz) {
    Sint i;
    for (i = array_sz - 1; i >= 0; i--) {
        if (eq(val, array_p[i])) {
            return i;
        }
    }
    return -1;
}


typedef struct {
    const Eterm *src; /* Read position starting end of src and counting back */
    const Eterm *src_start; /* Stop position (beginning of src) */
    Sint src_size;
    Eterm *dst;
    Eterm *dst_start; /* allocated memory on heap */
    Eterm result;
} ErtsCopyArrayToListContext;


static ERTS_INLINE void
erts_trappable_copy_array_to_list_setup(Process *p,
                                        const Eterm *src, Sint src_size,
                                        ErtsCopyArrayToListContext *context) {
    context->dst = context->dst_start = HAlloc(p, (Uint) (2 * src_size));

    context->src_size = src_size;
    context->src_start = src;
    context->src = src + src_size - 1; /* point to last element counting back */

    context->result = NIL;
}


/*
 * Build list from term array.
 *
 * Return: Eterm(list) when result is built
 * Return: am_false if result is not ready and another call is required
 */
static ERTS_INLINE Eterm
erts_trappable_copy_array_to_list(Process *p,
                                  ErtsCopyArrayToListContext *context) {
    /* Step through so many elements of list per available reduction */
    const static Sint COPIES_PER_REDUC = 30;
    const Sint budget = COPIES_PER_REDUC * IF_DEBUG(p->fcalls / 10 + 1,
                                                    p->fcalls);
    register Sint reds = budget;

    Eterm result = context->result;
    Eterm *dst = context->dst;
    const Eterm *src = context->src;
    const Eterm *src_start = context->src_start;

    while (src >= src_start && reds > 0) {
        result = CONS(dst, *src, result);
        dst += 2;
        src--;
        reds--;
    }

    BUMP_REDS(p, budget - reds);
    if (reds > 0) {
        /* Work complete, reds non-zero */
        //return make_list(context->dst_start);
        return result;
    }

    context->result = result;
    context->dst = dst;
    context->src = src;
    return am_false;
}



enum { SUBTRACT_SMALL_VEC_SIZE = 10 };
/* This value comes as 1st argument to list:subtract to indicate trapping.
 * In this case second arg must be {A, B, StateMagicBin} */
#define SUBTRACT_SPECIAL_VALUE am_bif_return_trap

typedef enum {
    SUBTRACT_STEP_LEN_A,    /* calculate length of A */
    SUBTRACT_STEP_LEN_B,    /* calculate length of B */
    SUBTRACT_STEP_COPY_B,   /* copy B to temporary array */
    SUBTRACT_STEP_SCAN,     /* scan A and copy elements not in B to result */
} ErtsSubtractFsmState;

/*
 * Context used for trapping length calculation.
 * The algorithm is reusable and might be moved elsewhere for where trappable
 * length is needed.
 */
typedef struct {
    Eterm iter;         /* points to next element of the list */
    Sint count;         /* stores the result */
} ErtsLengthContext;

/* Context used for trapping scanning of inputs */
typedef struct {
    /* iterator over A when subtracting and moving remaining elts */
    Eterm iter_a;
} ErtsSubtractScanContext;

typedef struct {
    /* Allows doing work in stages interrupting on large inputs */
    ErtsSubtractFsmState fsm_state;
    Uint len_a;
    Uint len_b;

    /* 1 if heap of p had enough space for result, otherwise 0 if heap fragment
     * was allocated. This is used to later judge whether we can free some of
     * result memory or we have to occupy it with NILs or a fake tuple. */
    int is_result_on_heap;
    Eterm *result_p; /* Start of allocated result mem */
    Eterm *result_write_p;

    /* Dynamically allocated copy of B, for shrinking it when elements are
     * found and removed */
    Eterm small_vec_b[SUBTRACT_SMALL_VEC_SIZE];
    Eterm *copy_of_b;

    union {
        ErtsLengthContext len;
        ErtsCopyListToArrayContext copy_b;
        ErtsSubtractScanContext scan;
        ErtsCopyArrayToListContext build_res;
    } s;
} ErtsSubtractContext;


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
    if (context->fsm_state != SUBTRACT_STEP_SCAN) {
        /* nothing to do if we didn't allocate any memory yet */
        return 1;
    }

    if (context->copy_of_b && context->copy_of_b != context->small_vec_b) {
        erts_free(ERTS_ALC_T_TMP, (void *) context->copy_of_b);
        context->copy_of_b = NULL;
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
subtract_create_state_tuple3(Process *p, Eterm A, Eterm B, Binary *context_b) {
    const static int TUPLE3_SIZE = 3 + 1;
    Eterm *hp = HAlloc(p, ERTS_MAGIC_REF_THING_SIZE + TUPLE3_SIZE);
    Eterm state_binref = erts_mk_magic_ref(&hp, &MSO(p), context_b);
    return TUPLE3(hp, A, B, state_binref);
}


/* Given a pointer to list cell, write value into it. Link this cell to the
 * future uninitialised cell located next in memory.
 * Eterm **ptr_p is assumed to point to large enough memory array.
 */
static ERTS_INLINE void
build_list_forward(Eterm **ptr_p, Eterm val) {
    CAR(*ptr_p) = val;
    CDR(*ptr_p) = make_list(*ptr_p + 2);
    *ptr_p += 2;
}


/* Iterate over A and for each element that is present in B, skip it
 * in A and remove it from B (by moving last element of B in its place and
 * making B shorter), otherwise that element is copied to the result.
 * Repeat until either end of A is reached or B is empty.
 *
 * Result is built in s->result_array of Eterm[result_size]
 * Returns: 0 if the work is not done and must call again
 * Returns: 1 if the work is done and must copy result to a new list
 */
static int
subtract_trappable_scan(Process *p, ErtsSubtractContext *context) {
    /* Step through so many elements of list per available reduction.
     * Scanning B each time takes 'remaining_in_B' reductions. */
    const static Sint CHECKS_PER_REDUC = 2;
    const Sint budget = CHECKS_PER_REDUC * IF_DEBUG(p->fcalls / 10 + 1,
                                                    p->fcalls);
    register Sint reds = budget;

    ErtsSubtractScanContext *s = &context->s.scan;
    Eterm iter_a = s->iter_a; /* load iterator from state */
    Eterm *b = context->copy_of_b;
    Uint len_b = context->len_b;
    Eterm *result_write_p = context->result_write_p;

    /* A is guaranteed to be proper list, otherwise erts_list_length() call at
     * start will fail */
    while (reds > 0 && is_list(iter_a)) {
        const Eterm *ptr_a  = list_val(iter_a);
        Sint found_at = subtract_find_in_array(CAR(ptr_a), b, len_b);
        reds -= len_b;
        if (found_at >= 0) {
            /* Remove elem from B and skip; shrink B by 1 */
            len_b--;
            b[found_at] = b[len_b];

            if (len_b == 0) {
                /* for reds calculation after the copy */
                iter_a = CDR(ptr_a);

                /* B is empty at this point, so copy remaining A elements
                 * TODO: This could be done via trappable copy list to array
                 */
                while (is_list(iter_a)) {
                    ptr_a = list_val(iter_a);

                    /* Write next element, and fill previous element's tail */
                    build_list_forward(&result_write_p, CAR(ptr_a));

                    reds--;
                    iter_a = CDR(ptr_a);
                }
                /* Trim reds to 1 so that reds>0 below does not trigger */
                reds = MAX(reds, 1);

                break; /* nothing left in B, end search here */
            }
        } else {
            build_list_forward(&result_write_p, CAR(ptr_a));
        }
        iter_a = CDR(ptr_a);
    }

    BUMP_REDS(p, budget - reds);
    context->len_b = len_b;

    if (reds > 0) {
        /* Reds not 0, work seems to be completed */
        return 1;
    }

    s->iter_a = iter_a; /* save iterator, only needed when we trap */
    return 0;
}


/*
 * Creates a magic binary and moves state there to survive subsequent stages
 */
static Binary *
subtract_move_context_to_magicbin(ErtsSubtractContext *context) {
    /* Context stored in magic binary. If this is not null, we will be trapping
     * periodically otherwise we will run operation till the end & return. */
    Binary *state_bin = erts_create_magic_binary(sizeof(ErtsSubtractContext),
                                                 subtractcontext_binary_dtor);

    /* Detect whether context was using local small vector for storage */
    int local_b = context->copy_of_b == context->small_vec_b;

    ErtsSubtractContext *new_context = ERTS_MAGIC_BIN_DATA(state_bin);
    memmove((void *)new_context, (void *)context, sizeof(ErtsSubtractContext));

    /* Restore pointer to local array of b */
    if (local_b) {
        new_context->copy_of_b = new_context->small_vec_b;
    }

    return state_bin;
}


/*
 * Setup variables for entering trappable length function
 */
static ERTS_INLINE void
erts_trappable_list_length_setup(Eterm T, ErtsSubtractContext *context) {
    ErtsLengthContext *lc = &context->s.len;
    lc->count = 0;
    lc->iter = T;
}


/*
 * Perform length calculation on a list. Will break early if the list is too
 * long and return atom 'false', you can reenter it with the same
 * ErtsLengthContext and continue.
 *
 * Before start: call erts_trappable_list_length_setup on the context
 * Returns: smallint(-1) - list did not end with a NIL.
 * Returns: smallint(Length) - if result is ready.
 * Returns: am_false - if you need to call it again.
 */
static Eterm
erts_trappable_list_length(Process *p, ErtsLengthContext *context) {
    Eterm iter = context->iter;
    register Sint count = context->count;

    /* Step through so many elements of list per available reduction */
    static const Sint ELEMENTS_PER_REDUC = 30;
    const Sint budget = ELEMENTS_PER_REDUC * IF_DEBUG(p->fcalls / 10 + 1,
                                                      p->fcalls);
    register Sint reds = budget;

    while (reds && is_list(iter)) {
        count++;
        iter = CDR(list_val(iter));
        reds--;
    }

    BUMP_REDS(p, budget - reds);
    if (reds) {
        /* if reds is non-zero means the work is finished, result is ready */
        if (is_not_nil(iter)) {
            return make_small(-1);
        }
        return make_small(count);
    } else {
        /* reds==0 means there's more work, save intermediate count */
        context->count = count;
        context->iter = iter;
        return am_false;
    }
}


/* Stage SCAN creates binary or on-stack state and performs the subtraction
 * also possibly trapping if the inputs are too long. */
static ERTS_INLINE void
subtract_enter_state_scan(Process *p, Eterm A, ErtsSubtractContext *context) {
    /* Having is_result_on_heap=1 later will allow us to free its unused part */
    context->is_result_on_heap = HeapWordsLeft(p) >= context->len_a;
    context->result_write_p = context->result_p = HAllocX(p, context->len_a, 0);
    /* Initialise loop counters and run the loop */
    context->s.scan.iter_a = A;
}


static ERTS_INLINE void
subtract_enter_state_copy_B(Eterm B, ErtsSubtractContext *context) {
    context->copy_of_b = subtract_maybe_alloc(
            context->len_b, context->small_vec_b, SUBTRACT_SMALL_VEC_SIZE);

    erts_trappable_copy_list_to_array_setup(B, context->copy_of_b,
                                            &context->s.copy_b);
}


static ERTS_INLINE void
subtract_abandon_result_memory(Process *p, ErtsSubtractContext *context) {
    Uint remaining_words;
    Uint used_words;

    if (context->is_result_on_heap) {
        /* drop remaining words of the allocated block */
        p->htop = context->result_write_p;
        return;
    }
    /* Fill heap fragment with an unused value */
    used_words = context->result_write_p - context->result_p;
    remaining_words = context->len_a * 2 - used_words;
    if (remaining_words > 1) {
        /* If a binary header will fit in there */
        *context->result_p = header_heap_bin(remaining_words * sizeof(Eterm));
    }
    ERTS_ASSERT(!"need to fill unused words here");
}


/* Do another part of work based on the current state.
 *
 * Returns: Value if result is found
 * Returns: am_badarg, if caller must BIF_ERROR(p, BADARG)
 * Returns: NON_VALUE if caller must BIF_TRAP2()
 */
static ERTS_INLINE Eterm
subtract_switch(Process *p, Eterm *bif_args_p,
                Eterm orig_A, Eterm orig_B, ErtsSubtractContext *context) {
    switch (context->fsm_state) {
        case SUBTRACT_STEP_LEN_A: {
            Eterm res = erts_trappable_list_length(p, &context->s.len);
            Sint count;
            if (res == am_false) {
                return THE_NON_VALUE; /* the caller fun will trap */
            }
            /* Returning smallint means that length result is ready */
            count = signed_val(res);
            if (count < 0) {
                p->flags &= ~F_DISABLE_GC;
                bif_args_p[0] = orig_A;
                bif_args_p[1] = orig_B;
                return am_badarg; /* exception created by the caller function */
            }
            if (count == 0) {
                p->flags &= ~F_DISABLE_GC;
                BIF_RET(NIL);
            }
            context->len_a = (Uint) count;
            context->fsm_state = SUBTRACT_STEP_LEN_B;
            erts_trappable_list_length_setup(orig_B, context);
            /* FALL THROUGH */
        }

        case SUBTRACT_STEP_LEN_B: {
            Eterm res = erts_trappable_list_length(p, &context->s.len);
            Sint count;
            if (res == am_false) {
                return THE_NON_VALUE; /* the caller fun will trap */
            }
            /* Returning smallint means that length result is ready */
            count = signed_val(res);
            if (count < 0) {
                p->flags &= ~F_DISABLE_GC;
                bif_args_p[0] = orig_A;
                bif_args_p[1] = orig_B;
                return am_badarg;
            }
            if (count == 0) {
                p->flags &= ~F_DISABLE_GC;
                BIF_RET(orig_A);
            }
            context->len_b = (Uint) count;
            context->fsm_state = SUBTRACT_STEP_COPY_B;
            subtract_enter_state_copy_B(orig_B, context);
            /* FALL THROUGH */
        }

        case SUBTRACT_STEP_COPY_B: {
            if (!erts_trappable_copy_list_to_array(p, &context->s.copy_b)) {
                return THE_NON_VALUE; /* the caller fun will trap */
            }
            context->fsm_state = SUBTRACT_STEP_SCAN;
            subtract_enter_state_scan(p, orig_A, context);
            /* FALL THROUGH */
        }

        case SUBTRACT_STEP_SCAN: {
            int res = subtract_trappable_scan(p, context);
            Uint result_len;
            if (!res) {
                return THE_NON_VALUE; /* the caller fun will trap */
            }
            if (context->result_write_p == context->result_p) {
                /* No values went into result? Free and return a [] */
                p->flags &= ~F_DISABLE_GC;
                BIF_RET(NIL);
            }
            result_len = (context->result_write_p - context->result_p) / 2;
            subtract_abandon_result_memory(p, context);
            if (result_len == context->len_a) {
                /* All elements of A survived? Return the original A */
                p->flags &= ~F_DISABLE_GC;
                BIF_RET(orig_A);
            }
            CDR(context->result_write_p - 2) = NIL; /* terminate last element */
            BIF_RET(make_list(context->result_p));
        }

//        case SUBTRACT_STEP_BUILD_RESULT: {
//            Eterm res = erts_trappable_copy_array_to_list(p, &context->s.build_res);
//            if (res == am_false) {
//                return THE_NON_VALUE; /* the caller fun will trap */
//            }
//            subtractcontext_dtor(context);
//            p->flags &= ~F_DISABLE_GC;
//            BIF_RET(res);
//        }
    }
    ERTS_ASSERT(!"unreachable"); /* bloop. Unreachable, should not be here */
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
subtract(Process *p, Eterm *bif_args_p, Eterm A, Eterm B) {
    if (A != SUBTRACT_SPECIAL_VALUE && ! is_tuple(B)) {
        Eterm result;

        /* This is initial call entry from Erlang, both args are lists, no ctx
         * is yet created. So create state here. */
        ErtsSubtractContext onstack_context;
        subtractcontext_ctor(&onstack_context);

        /* Enter now with state */
        erts_trappable_list_length_setup(A, &onstack_context);

        result = subtract_switch(p, bif_args_p, A, B, &onstack_context);

        if (result == am_badarg) {
            BIF_ERROR(p, BADARG);
        }
        /* Check if result was produced, and then do not create tmp binary */
        if (result != THE_NON_VALUE) {
            /* We got result in one go, do not allocate tmp binary */
            BIF_RET(result);
        }

        /* From here, a trap return is requested */

        /* We got a trap return, means we must allocate a tmp binary now */
        Binary *state_bin = subtract_move_context_to_magicbin(&onstack_context);
        p->flags |= F_DISABLE_GC;
        BIF_TRAP2(&subtract_trap_export, p,
                  SUBTRACT_SPECIAL_VALUE,
                  subtract_create_state_tuple3(p, A, B, state_bin));
    }

    /* Continue. There A must be non value and B must be triple
     * {OrigA, OrigB, StateMagicBin} */
    do {
        Eterm *a_b_state_ptr;
        Binary *magic_bin;
        ErtsSubtractContext *context;
        Eterm result;
        Eterm state_tuple = B;

        ERTS_ASSERT(A == SUBTRACT_SPECIAL_VALUE);
        ERTS_ASSERT(is_tuple(B));
        ERTS_ASSERT(is_tuple_arity(B, 3));
        a_b_state_ptr = tuple_val(B);
        A = a_b_state_ptr[1];
        B = a_b_state_ptr[2];

        magic_bin = erts_magic_ref2bin(a_b_state_ptr[3]);
        context = ERTS_MAGIC_BIN_DATA(magic_bin);
        result = subtract_switch(p, bif_args_p, A, B, context);

        if (result == am_badarg) {
            BIF_ERROR(p, BADARG);
        }
        /* Check if result was produced, and then do not create tmp binary */
        if (result != THE_NON_VALUE) {
            /* We got result in one go, do not allocate tmp binary */
            BIF_RET(result);
        }
        BIF_TRAP2(&subtract_trap_export, p, SUBTRACT_SPECIAL_VALUE, state_tuple);
    } while (0);
}


BIF_RETTYPE ebif_minusminus_2(BIF_ALIST_2) {
    /* Passing also &BIF_ARG_1 to modify BIF__ARGS for nice badarg reporting */
    return subtract(BIF_P, &BIF_ARG_1, BIF_ARG_1, BIF_ARG_2);
}


BIF_RETTYPE subtract_2(BIF_ALIST_2) {
    /* Passing also &BIF_ARG_1 to modify BIF__ARGS for nice badarg reporting */
    return subtract(BIF_P, &BIF_ARG_1, BIF_ARG_1, BIF_ARG_2);
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
