/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2023. All Rights Reserved.
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

#include "erl_iolist.h"

#include "bif.h"
#include "erl_bits.h"
#include "erl_binary.h"
#include "erl_process.h"

#define IOL2B_MIN_EXEC_REDS (CONTEXT_REDS / 4)
#define IOL2B_RESCHED_REDS (CONTEXT_REDS / 40)

static Export list_to_bitstring_continue_export;
static Export iolist_size_continue_export;

static BIF_RETTYPE list_to_bitstring_continue(BIF_ALIST_1);
static BIF_RETTYPE iolist_size_continue(BIF_ALIST_1);

void
erts_init_iolist(void)
{
    erts_init_trap_export(&list_to_bitstring_continue_export,
                          am_erts_internal, am_list_to_bitstring_continue, 1,
                          &list_to_bitstring_continue);

    erts_init_trap_export(&iolist_size_continue_export,
                          am_erts_internal, am_iolist_size_continue, 1,
                          &iolist_size_continue);
}

/* ************************************************************************* */

#define ERTS_IOLIST_STATE_INITER(C_P, OBJ)                                    \
    {{NULL, NULL, NULL, ERTS_ALC_T_INVALID}, /* Stack */                      \
     (C_P),                                                                   \
     (OBJ),                                                                   \
     0,                                      /* Size */                       \
     0,                                      /* Size valid */                 \
     0}                                      /* Reductions left */

#define ERTS_IOLIST_SIZE_YIELDS_COUNT_PER_RED 8

typedef struct {
    ErtsEStack estack;
    Process *c_p;
    Eterm obj;
    ErlDrvSizeT size;
    int have_size;
    int reds_left;
} ErtsIOListState;

enum iolist_action {
    ERTS_IOLIST_OK,
    ERTS_IOLIST_OVERFLOW,
    ERTS_IOLIST_TYPE,
    ERTS_IOLIST_YIELD
};

/* This MUST report type and unit errors in exactly the same way as
 * `generic_iol2b` so that the latter can elect to skip error checking.
 *
 * Note that overflow errors are reported differently: the size routine reports
 * overflow when the size in bits is larger than that which can be represented
 * in a word, whereas the conversion routine reports overflow when the provided
 * buffer is too small. (The latter iff one hasn't guaranteed that it will fit
 * beforehand by calling the size routine.) */
static ERTS_FORCE_INLINE
enum iolist_action generic_iolist_size(ErtsIOListState *state,
                                       const int Unit,
                                       const int YieldSupport)
{
    enum iolist_action res = ERTS_IOLIST_OK;
    int init_yield_count, yield_count;
    DECLARE_ESTACK(s);
    Uint size;
    Eterm obj;

    ASSERT((YieldSupport) == !!(YieldSupport));
    ASSERT((Unit) == 1 || (Unit) == 8);
    ASSERT(state->size == 0 || ((YieldSupport) && state->estack.start));

    size = state->size;
    obj = state->obj;

    if (YieldSupport) {
        if (state->reds_left <= 0) {
            return ERTS_IOLIST_YIELD;
        }

        ESTACK_CHANGE_ALLOCATOR(s, ERTS_ALC_T_SAVED_ESTACK);
        init_yield_count = ERTS_IOLIST_SIZE_YIELDS_COUNT_PER_RED;
        init_yield_count *= state->reds_left;
        yield_count = init_yield_count;

        if (state->estack.start) {
            ESTACK_RESTORE(s, &state->estack);
        }
    } else {
        init_yield_count = yield_count = 0;
    }

    goto L_start;

    while (!ESTACK_ISEMPTY(s)) {
        obj = ESTACK_POP(s);

    L_start:
        while (is_list(obj)) {
            Eterm *cell = list_val(obj);
            Eterm head, tail;

            if ((YieldSupport) && --yield_count <= 0) {
                goto L_yield;
            }

            /* Note that we don't update `obj` straight away: if we have to
             * bail on a size overflow, we don't want to return into the main
             * loop with the raw `head` as that may be a type error. */
            head = CAR(cell);
            tail = CDR(cell);

            if (is_byte(head)) {
                if (ERTS_UNLIKELY(size > (ERTS_UWORD_MAX - 8))) {
                    goto L_overflow;
                }
                size += 8;
            } else  if (is_bitstring(head)) {
                Uint head_size = bitstring_size(head);
                if ((head_size % (Unit)) != 0) {
                    goto L_type_error;
                }
                if (ERTS_UNLIKELY(size > (ERTS_UWORD_MAX - head_size))) {
                    goto L_overflow;
                }
                size += head_size;
            } else if (is_list(head)) {
                /* Deal with the inner list in `head` first, handling our tail
                 * later */
                ESTACK_PUSH(s, tail);
                obj = head;
                continue;
            } else if (is_not_nil(head)) {
                goto L_type_error;
            }

            obj = tail;
        }

        if ((YieldSupport) && --yield_count <= 0) {
            goto L_yield;
        } else if (is_bitstring(obj)) {
            Uint obj_size = bitstring_size(obj);
            if ((obj_size % (Unit)) != 0) {
                goto L_type_error;
            }
            if (ERTS_UNLIKELY(size > (ERTS_UWORD_MAX - obj_size))) {
                goto L_overflow;
            }
            size += obj_size;
        } else if (is_not_nil(obj)) {
            goto L_type_error;
        }
    }

    ASSERT((size % (Unit)) == 0);
    state->have_size = 1;
    state->size = size;

    res = ERTS_IOLIST_OK;

L_finished:
    DESTROY_ESTACK(s);
    CLEAR_SAVED_ESTACK(&state->estack);

L_return:
    if (YieldSupport) {
        int reds, yc;

        yc = init_yield_count - yield_count;
        reds = (yc - 1) / ERTS_IOLIST_SIZE_YIELDS_COUNT_PER_RED + 1;

        BUMP_REDS(state->c_p, reds);
        state->reds_left -= reds;
    }

    return res;

L_overflow:
    /* This is not necessarily an error as we may be allowed to calculate
     * sizes that can't fit into a word, for example erlang:iolist_size/1.
     *
     * Hence, we save our current state to allow resuming after adding the
     * current size to a larger accumulator. Note that the size referred
     * here is prior to adding the size of the current object. */
    if (YieldSupport) {
        ESTACK_SAVE(s, &state->estack);
    }

    state->size = size;
    state->obj = obj;

    res = ERTS_IOLIST_OVERFLOW;
    goto L_return;

L_type_error:
    res = ERTS_IOLIST_TYPE;
    goto L_finished;

L_yield:
    ASSERT(YieldSupport);

    BUMP_ALL_REDS(state->c_p);
    state->size = size;
    state->reds_left = 0;
    state->obj = obj;

    ESTACK_SAVE(s, &state->estack);
    return ERTS_IOLIST_YIELD;
}

/* ************************************************************************* */

#define ERTS_IOLIST2BUF_STATE_INITER(C_P, OBJ)                                \
    {ERTS_IOLIST_STATE_INITER((C_P), (OBJ)),                                  \
     {NULL, 0, 0},                              /* Bitstring copy state */    \
     NULL,                                      /* Base */                    \
     0}                                         /* Offset */

#define ERTS_IOLIST_TO_BUF_BYTES_PER_YIELD_COUNT 32
#define ERTS_IOLIST_TO_BUF_BITS_PER_YIELD_COUNT                               \
    (ERTS_IOLIST_TO_BUF_BYTES_PER_YIELD_COUNT / CHAR_BIT)
#define ERTS_IOLIST_TO_BUF_YIELD_COUNT_PER_RED 8
#define ERTS_IOLIST_TO_BUF_BYTES_PER_RED \
    (ERTS_IOLIST_TO_BUF_YIELD_COUNT_PER_RED *                                 \
     ERTS_IOLIST_TO_BUF_BYTES_PER_YIELD_COUNT)

typedef struct {
    ErtsIOListState iolist;

    struct {
        byte *base;
        Uint offset;
        Uint size;
    } bcopy;

    byte *base;
    ErlDrvSizeT offset;
} ErtsIOList2BufState;

static ERTS_INLINE
enum iolist_action generic_iol2b_copy_bitstring(ErtsIOList2BufState *state,
                                                Eterm obj,
                                                int *yield_countp,
                                                const int YieldSupport)
{
    Uint offset, size;
    Uint copy_size;
    byte* base;
    int yield_count;

    if (state->bcopy.base) {
        ASSERT(YieldSupport);

        base = state->bcopy.base;
        offset = state->bcopy.offset;
        size = state->bcopy.size;

        state->bcopy.base = NULL;
    } else {
        ASSERT(is_bitstring(obj));
        ERTS_GET_BITSTRING(obj, base, offset, size);
    }

    ASSERT(size <= (state->iolist.size - state->offset));

    ERTS_CT_ASSERT((CONTEXT_REDS * ERTS_IOLIST_SIZE_YIELDS_COUNT_PER_RED) <
                   (ERTS_UINT_MAX / ERTS_IOLIST_TO_BUF_BITS_PER_YIELD_COUNT));

    if (YieldSupport) {
        Uint copy_limit;

        yield_count = *yield_countp;

        ASSERT(yield_count >= 0);
        copy_limit = (Uint)yield_count + 1;
        copy_limit *= ERTS_IOLIST_TO_BUF_BITS_PER_YIELD_COUNT;

        copy_size = MIN(size, copy_limit);
    } else {
        copy_size = size;
    }

    copy_binary_to_buffer(state->base, state->offset, base, offset, copy_size);
    state->offset += copy_size;

    if (YieldSupport) {
        if (copy_size < size) {
            state->bcopy.base = base;
            state->bcopy.offset = offset + copy_size;
            state->bcopy.size = size - copy_size;

            *yield_countp = 0;

            return 1;
        }

        yield_count -= copy_size / ERTS_IOLIST_TO_BUF_BITS_PER_YIELD_COUNT;
        *yield_countp = yield_count;
    }

    return 0;
}

/* This MUST report type and unit errors in exactly the same way as
 * `generic_iolist_size`, so that the former can be used to pre-check errors
 * for this routine, letting us skip error checking here if we wish. */
static ERTS_FORCE_INLINE
int generic_iol2b(ErtsIOList2BufState *copy_state,
                  const int Unit,
                  const int ErrorSupport,
                  const int YieldSupport)
{
    enum iolist_action res = ERTS_IOLIST_OK;
    int init_yield_count, yield_count;
    ErtsIOListState *list_state;
    DECLARE_ESTACK(s);
    byte *base;
    Eterm obj;

    ASSERT((YieldSupport) == !!(YieldSupport));
    ASSERT((ErrorSupport) == !!(ErrorSupport));
    ASSERT((Unit) == 1 || (Unit) == 8);

    list_state = &copy_state->iolist;
    ASSERT(list_state->have_size && copy_state->offset < list_state->size);

    base = copy_state->base;
    obj = list_state->obj;

    if (YieldSupport) {
        if (list_state->reds_left <= 0) {
            return ERTS_IOLIST_YIELD;
        }

        ESTACK_CHANGE_ALLOCATOR(s, ERTS_ALC_T_SAVED_ESTACK);
        init_yield_count = ERTS_IOLIST_SIZE_YIELDS_COUNT_PER_RED;
        init_yield_count *= list_state->reds_left;
        yield_count = init_yield_count;

        if (list_state->estack.start) {
            ESTACK_RESTORE(s, &list_state->estack);
        }
    } else {
        init_yield_count = yield_count = 0;
    }

    goto L_start;

    while (!ESTACK_ISEMPTY(s)) {
        obj = ESTACK_POP(s);

    L_start:
        while (is_list(obj)) {
            Eterm *cell = list_val(obj);

            if ((YieldSupport) && --yield_count <= 0) {
                goto L_yield;
            }

            obj = CAR(cell);

            if (is_byte(obj)) {
                Uint shift, byte_offset;

                if (ErrorSupport) {
                    if ((list_state->size - copy_state->offset) < 8) {
                        goto L_overflow_error;
                    }
                }

                byte_offset = copy_state->offset / 8;
                shift = copy_state->offset % 8;

                if ((Unit) == 8 || (shift == 0)) {
                    ASSERT(shift == 0);
                    base[byte_offset] = unsigned_val(obj);
                } else if ((Unit) == 1) {
                    byte leading_bits = base[byte_offset];

                    base[byte_offset] =
                        (byte)(unsigned_val(obj) >> shift) |
                        ((leading_bits >> (8 - shift)) << (8 - shift));
                    base[byte_offset + 1] = (unsigned_val(obj) << (8 - shift));
                }

                copy_state->offset += 8;
            } else if (is_bitstring(obj)) {
                if (ErrorSupport) {
                    Uint size = bitstring_size(obj);

                    if ((size % (Unit)) != 0) {
                        goto L_type_error;
                    } else if ((list_state->size - copy_state->offset) < size) {
                        goto L_overflow_error;
                    }
                }

                if (generic_iol2b_copy_bitstring(copy_state,
                                                 obj,
                                                 &yield_count,
                                                 YieldSupport)) {
                    list_state->obj = obj;
                    ESTACK_PUSH(s, CDR(cell));
                    goto L_yield;
                }
            } else if (is_list(obj)) {
                /* Deal with the inner list in `obj` first, handling our tail
                 * later */
                ESTACK_PUSH(s, CDR(cell));
                continue;
            } else if ((ErrorSupport) && is_not_nil(obj)) {
                goto L_type_error;
            }

            obj = CDR(cell);
        }

        if ((YieldSupport) && --yield_count <= 0) {
            goto L_yield;
        } else if (is_bitstring(obj)) {
            if (ErrorSupport) {
                Uint size = bitstring_size(obj);

                if ((size % (Unit)) != 0) {
                    goto L_type_error;
                } else if ((list_state->size - copy_state->offset) < size) {
                    goto L_overflow_error;
                }
            }

            if (generic_iol2b_copy_bitstring(copy_state,
                                             obj,
                                             &yield_count,
                                             YieldSupport)) {
                list_state->obj = obj;
                goto L_yield;
            }
        } else if ((ErrorSupport) && is_not_nil(obj)) {
            goto L_type_error;
        }
    }

    res = ERTS_IOLIST_OK;

L_return:
    if (YieldSupport) {
        int reds, yc;

        yc = init_yield_count - yield_count;
        reds = (yc - 1) / ERTS_IOLIST_SIZE_YIELDS_COUNT_PER_RED + 1;
        BUMP_REDS(list_state->c_p, reds);
        list_state->reds_left -= reds;
    }

    DESTROY_ESTACK(s);
    CLEAR_SAVED_ESTACK(&list_state->estack);

    return res;

L_overflow_error:
    res = ERTS_IOLIST_OVERFLOW;
    goto L_return;

L_type_error:
    res = ERTS_IOLIST_TYPE;
    goto L_return;

L_yield:
    ASSERT(YieldSupport);

    /* As the yielding variants have already checked the list for errors, there
     * is no need to continue after having copied everything of substance: just
     * return that everything went well.
     *
     * While we could do this in the main loop, it makes it more cluttered and
     * probably just costs performance in the average case. Doing it here
     * simplifies trapping as we can assume that there's always work to be done
     * on entry. */
    ASSERT(copy_state->offset <= list_state->size);
    if (copy_state->offset == list_state->size) {
        res = ERTS_IOLIST_OK;
        goto L_return;
    }

    BUMP_ALL_REDS(list_state->c_p);
    list_state->reds_left = 0;
    list_state->obj = obj;

    ESTACK_SAVE(s, &list_state->estack);
    return ERTS_IOLIST_YIELD;
}

/* ************************************************************************* */

ErlDrvSizeT erts_iolist_to_buf(Eterm obj, char *data, ErlDrvSizeT size)
{
    ErtsIOList2BufState state = { .iolist.have_size = 1,
                                  .iolist.size = size * 8,
                                  .iolist.obj = obj,
                                  .base = (byte*)data };

    if (size > 0) {
        switch (generic_iol2b(&state, 8, 1, 0)) {
        case ERTS_IOLIST_OVERFLOW:
            return ERTS_IOLIST_TO_BUF_OVERFLOW;
        case ERTS_IOLIST_TYPE:
            return ERTS_IOLIST_TO_BUF_TYPE_ERROR;
        case ERTS_IOLIST_OK:
            ASSERT(ERTS_IOLIST_TO_BUF_SUCCEEDED(state.offset));
            return size - (state.offset / 8);
        case ERTS_IOLIST_YIELD:
            ERTS_INTERNAL_ERROR("unreachable");
        }
    }

    return 0;
}

int erts_iolist_size(Eterm obj, ErlDrvSizeT *sizep)
{
    ErtsIOListState state = { .obj = obj };
    int res;

    res = generic_iolist_size(&state, 8, 0);
    *sizep = state.size / 8;

    return res != ERTS_IOLIST_OK;
}

/* ************************************************************************* */

/* Turn a possibly deep list of ints (and binaries) into one large bitstring */

#define ERTS_L2B_STATE_INITER(C_P, ARG, BIF, SZFunc, TBufFunc)                \
    {ERTS_IOLIST2BUF_STATE_INITER((C_P), (ARG)),                              \
     THE_NON_VALUE, (ARG), (BIF), (SZFunc), (TBufFunc)}

typedef struct ErtsL2BState_ ErtsL2BState;

struct ErtsL2BState_ {
    ErtsIOList2BufState buf;

    Eterm result;
    Eterm arg;

    Export *bif;

    int (*iolist_to_buf_size)(ErtsIOListState *);
    int (*iolist_to_buf)(ErtsIOList2BufState *);
};

static ERTS_INLINE
enum iolist_action list_to_bitstring_engine(ErtsL2BState *sp)
{
    ErtsIOList2BufState *copy_state;
    ErtsIOListState *list_state;
    Process *c_p;

    copy_state = &sp->buf;
    list_state = &copy_state->iolist;
    c_p = list_state->c_p;

    /* have_size == 0 until sp->iolist_to_buf_size() finishes */
    if (!list_state->have_size) {
        int res = sp->iolist_to_buf_size(list_state);

        switch (res) {
        case ERTS_IOLIST_YIELD:
        case ERTS_IOLIST_OVERFLOW:
        case ERTS_IOLIST_TYPE:
            return res;
        case ERTS_IOLIST_OK:
            ASSERT(list_state->have_size);
            break;
        }

        sp->result = erts_new_bitstring(c_p,
                                        list_state->size,
                                        (byte**)&copy_state->base);

        if (list_state->size == 0) {
            return ERTS_IOLIST_OK;
        }

        list_state->obj = sp->arg;

        if (list_state->reds_left <= 0) {
            BUMP_ALL_REDS(c_p);
            return ERTS_IOLIST_YIELD;
        }
    }

    ASSERT(list_state->size > 0 && is_value(sp->arg) && copy_state->base);
    return sp->iolist_to_buf(copy_state);
}

static int
l2b_state_destructor(Binary *mbp)
{
    ErtsL2BState *sp = ERTS_MAGIC_BIN_DATA(mbp);
    ASSERT(ERTS_MAGIC_BIN_DESTRUCTOR(mbp) == l2b_state_destructor);
    DESTROY_SAVED_ESTACK(&sp->buf.iolist.estack);
    return 1;
}

static BIF_RETTYPE
list_to_bitstring_dispatch(Eterm mb_eterm, ErtsL2BState* sp, int gc_disabled)
{
    Process *c_p = sp->buf.iolist.c_p;
    enum iolist_action action;

    ASSERT(is_non_value(mb_eterm) ^ gc_disabled);
    ASSERT(gc_disabled == !!(c_p->flags & F_DISABLE_GC));

    action = list_to_bitstring_engine(sp);
    switch (action) {
    case ERTS_IOLIST_OK:
        if (gc_disabled) {
            erts_set_gc_state(c_p, 1);
        }

        ASSERT(!(c_p->flags & F_DISABLE_GC));
        BIF_RET(sp->result);
        break;
    case ERTS_IOLIST_YIELD:
        if (!gc_disabled) {
            ErtsL2BState *new_sp;
            Binary *mbp;
            Eterm *hp;

            mbp = erts_create_magic_binary(sizeof(ErtsL2BState),
                                        l2b_state_destructor);
            new_sp = ERTS_MAGIC_BIN_DATA(mbp);
            *new_sp = *sp;

            hp = HAlloc(c_p, ERTS_MAGIC_REF_THING_SIZE);
            mb_eterm = erts_mk_magic_ref(&hp, &MSO(c_p), mbp);

            erts_set_gc_state(c_p, 0);
        }

        BIF_TRAP1(&list_to_bitstring_continue_export, c_p, mb_eterm);
        break;
    default:
        {
            Uint reason;

            if (gc_disabled && erts_set_gc_state(c_p, 1)) {
                ERTS_VBUMP_ALL_REDS(c_p);
            }

            if (action == ERTS_IOLIST_OVERFLOW) {
                reason = SYSTEM_LIMIT;
            } else {
                reason = BADARG;
            }

            ASSERT(!(c_p->flags & F_DISABLE_GC));
            ERTS_BIF_ERROR_TRAPPED1(c_p, reason, sp->bif, sp->arg);
        }
        break;
    }
}

static BIF_RETTYPE list_to_bitstring_continue(BIF_ALIST_1)
{
    Binary *mbp = erts_magic_ref2bin(BIF_ARG_1);
    ErtsL2BState *state;

    ASSERT(ERTS_MAGIC_BIN_DESTRUCTOR(mbp) == l2b_state_destructor);
    ASSERT(BIF_P->flags & F_DISABLE_GC);

    state = (ErtsL2BState*)ERTS_MAGIC_BIN_DATA(mbp);
    state->buf.iolist.reds_left = ERTS_BIF_REDS_LEFT(BIF_P);

    return list_to_bitstring_dispatch(BIF_ARG_1, state, 1);
}

static int list_to_binary_size(ErtsIOListState *state)
{
    return generic_iolist_size(state, 8, 1);
}

static int list_to_binary_copy(ErtsIOList2BufState *state)
{
    return generic_iol2b(state, 8, 0, 1);
}

static int list_to_bitstring_size(ErtsIOListState *state)
{
    return generic_iolist_size(state, 1, 1);
}

static int list_to_bitstring_copy(ErtsIOList2BufState *state)
{
    return generic_iol2b(state, 1, 0, 1);
}

static BIF_RETTYPE list_to_bitstring_bif(Process *c_p,
                                         Eterm arg,
                                         Export *bif,
                                         const int Unit)
{
    int reds_left = ERTS_BIF_REDS_LEFT(c_p);

    ASSERT((Unit) == 1 || (Unit) == 8);

    if (reds_left < IOL2B_MIN_EXEC_REDS) {
        if (reds_left <= IOL2B_RESCHED_REDS) {
            /* Yield and do it with full context reds... */
            ERTS_BIF_YIELD1(bif, c_p, arg);
        }

        /* Allow a bit more reductions... */
        reds_left = IOL2B_MIN_EXEC_REDS;
    }

    if (is_nil(arg)) {
        BIF_RET(erts_new_bitstring_from_data(c_p, 0, (byte*)""));
    } else if (is_not_list(arg)) {
        BIF_ERROR(c_p, BADARG);
    } else {
        /* Check for [bitstring()] case */
        Eterm h = CAR(list_val(arg)), t = CDR(list_val(arg));

        if (is_bitstring(h) && is_nil(t) && (bitstring_size(h) % (Unit)) == 0) {
            BIF_RET(h);
        } else {
            int (*size_func)(ErtsIOListState *) = ((Unit) == 8) ?
                list_to_binary_size :
                list_to_bitstring_size;
            int (*copy_func)(ErtsIOList2BufState *) = ((Unit) == 8) ?
                list_to_binary_copy :
                list_to_bitstring_copy;
            ErtsL2BState state = ERTS_L2B_STATE_INITER(c_p,
                                                       arg,
                                                       bif,
                                                       size_func,
                                                       copy_func);
            state.buf.iolist.reds_left = reds_left;
            return list_to_bitstring_dispatch(THE_NON_VALUE, &state, 0);
        }
    }
}

BIF_RETTYPE binary_list_to_bin_1(BIF_ALIST_1)
{
    return list_to_bitstring_bif(BIF_P, BIF_ARG_1,
                                 BIF_TRAP_EXPORT(BIF_binary_list_to_bin_1), 8);
}

BIF_RETTYPE list_to_binary_1(BIF_ALIST_1)
{
    return list_to_bitstring_bif(BIF_P, BIF_ARG_1,
                                 BIF_TRAP_EXPORT(BIF_list_to_binary_1), 8);
}

BIF_RETTYPE iolist_to_binary_1(BIF_ALIST_1)
{
    if (is_bitstring(BIF_ARG_1)) {
        if ((bitstring_size(BIF_ARG_1) % 8) == 0) {
            BIF_RET(BIF_ARG_1);
        }

        BIF_ERROR(BIF_P, BADARG);
    }

    return list_to_bitstring_bif(BIF_P, BIF_ARG_1,
                                 BIF_TRAP_EXPORT(BIF_iolist_to_binary_1), 8);
}

BIF_RETTYPE list_to_bitstring_1(BIF_ALIST_1)
{
    return list_to_bitstring_bif(BIF_P, BIF_ARG_1,
                                 BIF_TRAP_EXPORT(BIF_list_to_bitstring_1), 1);
}

/* ************************************************************************* */

/* iolist_size/1 has historically allowed sizes without any upper bound, not
 * even honoring BIG_ARITY_MAX. This is silly as the only way to reach it is to
 * create a large binary and reference it in absurdum in the same iolist, but
 * I'm hesitant to break compatibility here as we released a gray patch for a
 * bug where the size was truncated when it went beyond the capacity of a word.
 *
 * While allowing sizes up to BIG_ARITY_MAX would be trivial, the intermediate
 * results would make us run out of memory long before that since the GC is
 * disabled, so we'll compromise by raising a system limit exception once the
 * size becomes obscene. */
#define IOLIST_SIZE_MAX_BIGNUM_ARITY 64

typedef struct {
    ErtsIOListState iolist;
    Eterm accumulator;
    Eterm arg;
} IOListSizeState;

static int iolist_size_destructor(Binary *mbp)
{
    IOListSizeState *sp = ERTS_MAGIC_BIN_DATA(mbp);
    ASSERT(ERTS_MAGIC_BIN_DESTRUCTOR(mbp) == iolist_size_destructor);
    DESTROY_SAVED_ESTACK(&sp->iolist.estack);
    return 1;
}

static int iolist_size_accumulate(IOListSizeState *sp, Process *c_p)
{
    Uint size_in_bytes;

    ASSERT((sp->iolist.size % 8) == 0);
    size_in_bytes = sp->iolist.size / 8;
    sp->iolist.size = 0;

    if (is_immed(sp->accumulator)) {
        ERTS_CT_ASSERT((ERTS_UINT_MAX / 8) <= (ERTS_UINT_MAX - MAX_SMALL));
        size_in_bytes += unsigned_val(sp->accumulator);
    } else if (is_value(sp->accumulator)) {
        Uint actual_size, guessed_size;
        Eterm result;
        Eterm *hp;

        guessed_size = BIG_NEED_SIZE(big_size(sp->accumulator) + 1);
        if (guessed_size > IOLIST_SIZE_MAX_BIGNUM_ARITY) {
            return 0;
        }

        hp = HAlloc(c_p, guessed_size);

        result = big_plus_small(sp->accumulator,
                                size_in_bytes,
                                hp);

        actual_size = BIG_NEED_SIZE(big_size(result));
        ASSERT(actual_size <= guessed_size);

        HRelease(c_p, &hp[guessed_size], &hp[actual_size]);

        sp->accumulator = result;
        return 1;
    }

    sp->accumulator = erts_make_integer(size_in_bytes, c_p);
    return 1;
}

static BIF_RETTYPE
iolist_size_dispatch(Eterm mb_eterm, IOListSizeState *sp, int gc_disabled)
{
    Process *c_p = sp->iolist.c_p;
    Uint reason;

    ASSERT(is_non_value(mb_eterm) ^ gc_disabled);
    ASSERT(gc_disabled == !!(c_p->flags & F_DISABLE_GC));

    switch (generic_iolist_size(&sp->iolist, 8, 1)) {
    case ERTS_IOLIST_YIELD:
        if (!gc_disabled) {
            IOListSizeState *new_sp;
            Binary *mbp;
            Eterm *hp;

            mbp = erts_create_magic_binary(sizeof(IOListSizeState),
                                           iolist_size_destructor);
            new_sp = (IOListSizeState*)ERTS_MAGIC_BIN_DATA(mbp);
            *new_sp = *sp;

            hp = HAlloc(c_p, ERTS_MAGIC_REF_THING_SIZE);
            mb_eterm = erts_mk_magic_ref(&hp, &MSO(c_p), mbp);

            erts_set_gc_state(c_p, 0);
        }

        BIF_TRAP1(&iolist_size_continue_export, c_p, mb_eterm);
    case ERTS_IOLIST_OK:
        if (iolist_size_accumulate(sp, c_p)) {
            if (gc_disabled) {
                erts_set_gc_state(c_p, 1);
            }

            ASSERT(!(c_p->flags & F_DISABLE_GC));
            return sp->accumulator;
        }

        reason = SYSTEM_LIMIT;
        break;
    case ERTS_IOLIST_OVERFLOW:
        /* Result too large to fit into sp->iolist.size, accumulate current
         * size in an integer term and start over where we left off. */
        if (iolist_size_accumulate(sp, c_p)) {
            return iolist_size_dispatch(mb_eterm, sp, gc_disabled);
        }

        reason = SYSTEM_LIMIT;
        break;
    case ERTS_IOLIST_TYPE:
    default:
        reason = BADARG;
        break;
    }

    if (gc_disabled && erts_set_gc_state(c_p, 1)) {
        ERTS_VBUMP_ALL_REDS(c_p);
    }

    ASSERT(!(c_p->flags & F_DISABLE_GC));
    ERTS_BIF_ERROR_TRAPPED1(c_p,
                            reason,
                            BIF_TRAP_EXPORT(BIF_iolist_size_1),
                            sp->arg);
}

static BIF_RETTYPE iolist_size_continue(BIF_ALIST_1)
{
    Binary *mbp = erts_magic_ref2bin(BIF_ARG_1);
    IOListSizeState *state;

    ASSERT(ERTS_MAGIC_BIN_DESTRUCTOR(mbp) == iolist_size_destructor);
    ASSERT(BIF_P->flags & F_DISABLE_GC);

    state = (IOListSizeState*)ERTS_MAGIC_BIN_DATA(mbp);
    state->iolist.reds_left = ERTS_BIF_REDS_LEFT(BIF_P);

    return iolist_size_dispatch(BIF_ARG_1, state, 1);
}

BIF_RETTYPE iolist_size_1(BIF_ALIST_1)
{
    IOListSizeState state =
        {ERTS_IOLIST_STATE_INITER(BIF_P, BIF_ARG_1),
         THE_NON_VALUE,
         BIF_ARG_1};

    state.iolist.reds_left = ERTS_BIF_REDS_LEFT(BIF_P);

    return iolist_size_dispatch(THE_NON_VALUE, &state, 0);
}
