/*
 * %CopyrightBegin%
 *
 * Copyright 2019, OpenX Technologies. All Rights Reserved.
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

/* Conversion between Erlang terms and JSON format.
 *
 * This code is modeled after the external term format conversion code in
 * external.c
 */

// #define EXTREME_TTB_TRAPPING 1

#define DECODE_ALL 0
#define ENCODE_ALL 0

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "erl_vm.h"
#include "global.h"
#include "erl_process.h"
#include "error.h"
#include "atom.h"
#define ERL_WANT_HIPE_BIF_WRAPPER__
#include "bif.h"
#undef ERL_WANT_HIPE_BIF_WRAPPER__
#include "big.h"
#include "dist.h"
#include "erl_binary.h"
#include "erl_bits.h"
#include "erl_map.h"

#ifndef likely
#  if defined(__GNUC__) || defined(__clang__)
#    define likely(x)     __builtin_expect((x), 1)
#    define unlikely(x)   __builtin_expect((x), 0)
#  else
#    define likely(x)     (x)
#    define unlikely(x)   (x)
#  endif
#endif // ! defined likely

typedef struct T2JContext_struct {
    int alive;
    Uint32 flags;
    byte *ep;
    Eterm obj;
    ErtsWStack wstack;
    Binary *result_bin;
} T2JContext;

typedef struct J2TContext_struct {
    int alive;
    Uint32 flags;
    Eterm null_atom;
    int state;
    const byte *ep;
    const byte *vp;
    const byte *endp;
    Eterm term;
    int strdiff;
    ErtsWStack wstack;
} J2TContext;


static Export term_to_json_trap_export;
static Export json_to_term_trap_export;

static BIF_RETTYPE term_to_json_trap_1(BIF_ALIST_1);
static BIF_RETTYPE json_to_term_trap_1(BIF_ALIST_1);

static Eterm erts_term_to_json_int(Process *p, Eterm Term, Sint initial_buf_size, Uint32 flags, Binary *context_b);
static Eterm erts_json_to_term_int(Process *p, Eterm Json, Uint32 flags, Binary *context_b);

static int enc_json_int(T2JContext *ctx, Sint *reds_arg, Binary **result_bin_arg);
int dec_json_int(Process *p, J2TContext *ctx, Sint *reds_arg, Eterm *result_term_arg);

unsigned i64ToAsciiTable(char *dst, int64_t value);
Sint json_enc_unicode(byte *d, byte *s, byte *send);


void erts_init_json(void)
{
    erts_init_trap_export(&term_to_json_trap_export,
                          am_erlang, am_term_to_json_trap,
                          1, // Arity of term_to_json_trap_1 function.
                          &term_to_json_trap_1);

    erts_init_trap_export(&json_to_term_trap_export,
                          am_erlang, am_json_to_term_trap,
                          1, // Arity of json_to_term_trap_1 function.
                          &json_to_term_trap_1);
}

/* Define EXTREME_TTB_TRAPPING for testing in dist.h */
#ifndef EXTREME_TTB_TRAPPING
#define TERM_TO_JSON_DEFAULT_INITIAL_SIZE 4096
#else
#define TERM_TO_JSON_DEFAULT_INITIAL_SIZE 20
#endif
#define TERM_TO_JSON_MAX_INITIAL_SIZE (20 * 1024 * 1024)

#define JSON_USE_NIL		0x1
#define JSON_RETURN_MAPS	0x2

/**********************************************************************/

/* Continue work after either term_to_json_1 or term_to_json_2 returns via
   BIF_TRAP1. */
static BIF_RETTYPE term_to_json_trap_1(BIF_ALIST_1)
{
    Eterm *tp = tuple_val(BIF_ARG_1);
    Eterm Term = tp[1];
    Eterm bt = tp[2];
    Binary *context = erts_magic_ref2bin(bt);

    return erts_term_to_json_int(BIF_P, Term, /* buf_size */ 0, /* flags */ 0, context);
}

HIPE_WRAPPER_BIF_DISABLE_GC(term_to_json, 1)

/* erlang:term_to_json/1 entry point. */
BIF_RETTYPE term_to_json_1(BIF_ALIST_1)
{
    return erts_term_to_json_int(BIF_P, BIF_ARG_1, TERM_TO_JSON_DEFAULT_INITIAL_SIZE, 0, NULL);
}

HIPE_WRAPPER_BIF_DISABLE_GC(term_to_json, 2)

/* erlang:term_to_json/2 entry point. */
BIF_RETTYPE term_to_json_2(BIF_ALIST_2)
{
    Process *p = BIF_P;
    Eterm Options = BIF_ARG_2;
    Sint buf_size = TERM_TO_JSON_DEFAULT_INITIAL_SIZE;
    Uint32 flags = 0;

    while (is_list(Options)) {
        Eterm arg = CAR(list_val(Options));
        Eterm *tp;
        if (is_tuple(arg) && *(tp = tuple_val(arg)) == make_arityval(2)) {
            if (ERTS_IS_ATOM_STR("min_buf_size", tp[1]) && tag_val_def(tp[2]) == SMALL_DEF) {
                buf_size = signed_val(tp[2]);
                if (buf_size < 1) { goto error; }
                if (buf_size > TERM_TO_JSON_MAX_INITIAL_SIZE) {
                    buf_size = TERM_TO_JSON_MAX_INITIAL_SIZE;
                }
            } else {
                goto error;
            }
        } else if (ERTS_IS_ATOM_STR("use_nil", arg)) {
            flags |= JSON_USE_NIL;
        } else {
        error:
            BIF_ERROR(p, EXC_BADARG);
        }
        Options = CDR(list_val(Options));
    }
    if (is_not_nil(Options)) {
        goto error;
    }

    return erts_term_to_json_int(p, BIF_ARG_1, buf_size, flags, NULL);
}

#define TERM_TO_JSON_LOOP_FACTOR TERM_TO_BINARY_LOOP_FACTOR
#define TERM_TO_JSON_MEMCPY_FACTOR 8

static int t2j_context_destructor(Binary *context_bin)
{
    T2JContext *context = ERTS_MAGIC_BIN_DATA(context_bin);
    if (context->alive) {
        context->alive = 0;
        DESTROY_SAVED_WSTACK(&context->wstack);
        if (context->result_bin != NULL) { /* Set to NULL if ever made alive! */
            ASSERT(erts_refc_read(&(context->result_bin->intern.refc), 1));
            erts_bin_free(context->result_bin);
            context->result_bin = NULL;
        }
    }
    return 1;
}

#define JSON_YIELD	0
#define JSON_BADARG	1
#define JSON_DONE	2

static BIF_RETTYPE
erts_term_to_json_int(Process *p, Eterm Term, Sint initial_buf_size, Uint32 flags, Binary *context_b)
{
#ifndef EXTREME_TTB_TRAPPING
    Sint reds = (Sint) (ERTS_BIF_REDS_LEFT(p) * TERM_TO_JSON_LOOP_FACTOR);
#else
    Sint reds = 4; /* For testing */
#endif
    Sint initial_reds = reds;
    int is_first_call;
    T2JContext context_buf;
    T2JContext *context;

    if (context_b == NULL) {
        // First call; initialize context.
        is_first_call = 1;
        context_buf.alive = 1;
        context_buf.flags = flags;
        context_buf.obj = Term;
        context_buf.wstack.wstart = NULL;
        context_buf.result_bin = erts_bin_nrml_alloc(initial_buf_size);
        context_buf.ep = (byte *) context_buf.result_bin->orig_bytes;
        context = &context_buf;
    } else {
        is_first_call = 0;
        context = ERTS_MAGIC_BIN_DATA(context_b);
    }

    flags = context->flags;
    switch (enc_json_int(context, &reds, &context->result_bin)) {
    case JSON_YIELD: {
        // Ran out of reductions; yield.
        Eterm *hp;
        Eterm ctx_term;

        if (context_b == NULL) {
            context_b = erts_create_magic_binary(sizeof context_buf, t2j_context_destructor);
            context = ERTS_MAGIC_BIN_DATA(context_b);
            sys_memcpy(context, &context_buf, sizeof context_buf);
        }
        if (is_first_call) {
            erts_set_gc_state(p, 0);
        }

        // I'm not sure why this creates a tuple for BIF_TRAP1 instead of using BIF_TRAP2.
        hp = HAlloc(p, ERTS_MAGIC_REF_THING_SIZE + 3); // +3 for 2-tuple.
        ctx_term = erts_mk_magic_ref(&hp, &MSO(p), context_b);
        BUMP_ALL_REDS(p);
        BIF_TRAP1(&term_to_json_trap_export, p, TUPLE2(hp, Term, ctx_term));
        /*NOTREACHED*/
    }

    case JSON_BADARG:
        // Found some bad input data; return a badarg error.
        ASSERT(erts_refc_read(&context->result_bin->intern.refc, 1) == 1);
        erts_bin_free(context->result_bin);
        if (context_b && erts_refc_read(&context_b->intern.refc, 0) == 0) {
            erts_bin_free(context_b);
        }

        context->result_bin = NULL;
        if (! is_first_call) {
            erts_set_gc_state(p, 1);
            ERTS_BIF_ERROR_TRAPPED1(p, EXC_BADARG, bif_export[BIF_term_to_json_1], Term);
        } else {
            BIF_ERROR(p, EXC_BADARG);
        }
        /*NOTREACHED*/

    case JSON_DONE: {
        // Finished; create return value.
        Binary *result_bin = context->result_bin;

        BUMP_REDS(p, (initial_reds - reds) / TERM_TO_JSON_LOOP_FACTOR);
        context->result_bin = NULL;
        context->alive = 0;
        ASSERT(erts_refc_read(&result_bin->intern.refc, 1));
        if (context_b && erts_refc_read(&context_b->intern.refc, 0) == 0) {
            erts_bin_free(context_b);
        }
        if (! is_first_call) {
            erts_set_gc_state(p, 1);
        }
        BIF_RET(erts_build_proc_bin(&MSO(p), HAlloc(p, PROC_BIN_SIZE), result_bin));
        /*NOTREACHED*/
    }
    } // switch enc_json_int
    abort();
}

#define ENC_TERM		((Eterm) 0)
#define ENC_ARRAY_ELEMENT	((Eterm) 1)
#define ENC_OBJECT_ELEMENT	((Eterm) 2) // Used for proplist object encoding.
#define ENC_FLATMAP_PAIR	((Eterm) 3) // Used for flatmap object encoding.
#define ENC_HASHMAP_NODE	((Eterm) 4) // Used for HAMT object encoding.
#define ENC_MAP_ATOM_KEY	((Eterm) 5)
#define ENC_MAP_VALUE		((Eterm) 6) // Flags value for object encoding.
#define ENC_MAP_LAST		((Eterm) 7) // Flags end of object encoding.
#define ENC_BIN_COPY		((Eterm) 8) // Copy long binary.
#define ENC_JSON_COPY		((Eterm) 9) // Copy preencoded JSON.

// Max number of output bytes one Unicode character can expand to: \uXXXX.
#define MAX_UTF8_EXPANSION 6

#define IS_UTF8_CONTINUATION_BYTE(byte) (((byte) & 0xC0) == 0x80)

/* Interruptable JSON encoder.  Returns 0 when term is completely encoded, or
   -1 when out of reductions. */

static int
enc_json_int(T2JContext *ctx, Sint *reds_arg, Binary **result_bin_arg)
{
    WSTACK_DECLARE(s);

    Sint reds = *reds_arg;

    const Uint32 flags = ctx->flags;
    byte *ep  = ctx->ep;
    Eterm obj = ctx->obj;
    const byte *endp = (byte *) (*result_bin_arg)->orig_bytes + (*result_bin_arg)->orig_size;

    WSTACK_CHANGE_ALLOCATOR(s, ERTS_ALC_T_SAVED_ESTACK);

    if (ctx->wstack.wstart) { /* restore saved stacks and byte pointer */
        WSTACK_RESTORE(s, &ctx->wstack);
    }

    if (! is_non_value(obj)) {
        goto encode_term_no_reduction_check;
    }

#define ENSURE_BUFFER(n)						\
    do {								\
        Uint needed_bytes = (n);					\
        if (ep + needed_bytes > endp) {					\
            Sint offset = ep - (byte *) (*result_bin_arg)->orig_bytes;	\
            Uint needed_size = offset + needed_bytes;			\
            Uint new_size = (*result_bin_arg)->orig_size;		\
            do { new_size *= 2; } while (new_size < needed_size);	\
            *result_bin_arg = erts_bin_realloc(*result_bin_arg, new_size); \
            ep = (byte *) (*result_bin_arg)->orig_bytes + offset;	\
            endp = (byte *) (*result_bin_arg)->orig_bytes + (*result_bin_arg)->orig_size; \
        }								\
    } while (0)


 outer_loop:
    while (! WSTACK_ISEMPTY(s)) {
        obj = WSTACK_POP(s);

        switch (WSTACK_POP(s)) {
        case ENC_TERM:
            break;

        case ENC_ARRAY_ELEMENT: {
            Eterm *cons;
            Eterm tail;
          enc_array_element:
            switch (tag_val_def(obj)) {
            case NIL_DEF:
                ENSURE_BUFFER(1);
                *ep++ = ']';
                goto outer_loop;
            case LIST_DEF:
                if (ep[-1] != '[') {
                    ENSURE_BUFFER(1);
                    *ep++ = ',';
                }
                cons = list_val(obj);
                tail = CDR(cons);
                obj = CAR(cons);
                WSTACK_PUSH2(s, ENC_ARRAY_ELEMENT, tail);
                goto encode_term;
            }
            goto fail; // Not a proper list.
        }

        case ENC_OBJECT_ELEMENT: {
            // Encodes the proplist JSON object representation.
            Eterm *cons;
            Eterm tail;
            Eterm *tuple;
            Uint tuple_len;
          enc_object_element:
            switch (tag_val_def(obj)) {
            case NIL_DEF:
                ENSURE_BUFFER(1);
                *ep++ = '}';
                goto outer_loop;
            case LIST_DEF:
                if (ep[-1] != '{') {
                    ENSURE_BUFFER(1);
                    *ep++ = ',';
                }
                cons = list_val(obj);
                tail = CDR(cons);
                obj = CAR(cons);
                if (unlikely(tag_val_def(obj) != TUPLE_DEF)) { goto fail; }
                tuple = tuple_val(obj);
                tuple_len = arityval(*tuple);
                if (unlikely(tuple_len != 2)) { goto fail; }
                obj = tuple[1];
                if (obj == am_json) {
                    // Preencoded JSON.
                    WSTACK_PUSH2(s, ENC_OBJECT_ELEMENT, tail);
                    obj = tuple[2];
                    goto enc_json_start;
                } else {
                    WSTACK_PUSH4(s, ENC_OBJECT_ELEMENT, tail, ENC_MAP_VALUE, tuple[2]);
                    // Encode object key.
                    switch (tag_val_def(obj)) {
                    case BINARY_DEF: goto encode_term;
                    case ATOM_DEF: goto enc_map_atom_key;
                    default: goto fail;
                    }
                }
            }
            goto fail; // Not a proper list.
        }

        case ENC_FLATMAP_PAIR: {
            // Encodes the flatmap map implementation as a JSON object.
            Uint pairs_left = obj;
            Eterm *vptr = (Eterm *) WSTACK_POP(s);
            Eterm *kptr = (Eterm *) WSTACK_POP(s);
            ENSURE_BUFFER(1);
            if (ep[-1] != '{') {
                *ep++ = ',';
            }
            obj = *kptr;
            if (--pairs_left > 0) {
                WSTACK_PUSH4(s, (UWord)(kptr+1), (UWord)(vptr+1), ENC_FLATMAP_PAIR, pairs_left);
            } else {
                WSTACK_PUSH2(s, ENC_MAP_LAST, THE_NON_VALUE);
            }
            if (obj == am_json) {
                // Preencoded JSON.
                obj = *vptr;
                goto enc_json_start;
            } else {
                WSTACK_PUSH2(s, ENC_MAP_VALUE, *vptr);
                // Encode object key.
                switch (tag_val_def(obj)) {
                case BINARY_DEF: goto encode_term;
                case ATOM_DEF: goto enc_map_atom_key;
                default: goto fail;
                }
            }
            break;
        }

        case ENC_HASHMAP_NODE: {
            if (is_list(obj)) { /* leaf node [K|V] */
                Eterm *cons = list_val(obj);
                obj = CAR(cons);
                if (ep[-1] != '{') {
                    ENSURE_BUFFER(1);
                    *ep++ = ',';
                }
                if (obj == am_json) {
                    // Preencoded JSON.
                    obj = CDR(cons);
                    goto enc_json_start;
                } else {
                    WSTACK_PUSH2(s, ENC_MAP_VALUE, CDR(cons));
                    // Encode object key.
                    switch (tag_val_def(obj)) {
                    case BINARY_DEF: goto encode_term;
                    case ATOM_DEF: goto enc_map_atom_key;
                    default: goto fail;
                    }
                }
            }
            break;
        }

        case ENC_MAP_ATOM_KEY: {
            // Encode an object key that is an atom.
            // This case key is never matched; it exists to hold the common
            // code for converting an atom to a binary.
            Atom *a;
            Sint strlen;
          enc_map_atom_key:
            a = atom_tab(atom_val(obj));
            ENSURE_BUFFER(a->len * MAX_UTF8_EXPANSION + 2);
            *ep++ = '"';
            strlen = json_enc_unicode(ep, a->name, a->name + a->len);
            if (unlikely(strlen < 0)) { goto fail; }
            ep += strlen;
            *ep++ = '"';
            obj = THE_NON_VALUE;
            goto outer_loop;
        }

        case ENC_MAP_VALUE:
            ENSURE_BUFFER(1);
            *ep++ = ':';
            break;

        case ENC_MAP_LAST:
            ENSURE_BUFFER(1);
            *ep++ = '}';
            goto outer_loop;

        case ENC_BIN_COPY: {
            // Encode large binaries in parts.
            Uint len = (Uint) obj;
            byte *aligned_alloc = (byte *) WSTACK_POP(s);
            byte *bytes = (byte *) WSTACK_POP(s);
            Sint strlen;
            if (len > reds * TERM_TO_JSON_MEMCPY_FACTOR) {
                Uint n = reds * TERM_TO_JSON_MEMCPY_FACTOR;
                // Move n forward to the end of the multi-byte UTF-8 character.
                while (n < len && IS_UTF8_CONTINUATION_BYTE(bytes[n])) { n++; }
                WSTACK_PUSH4(s, (UWord) (bytes + n), (UWord) aligned_alloc, ENC_BIN_COPY, len - n);

                ENSURE_BUFFER(MAX_UTF8_EXPANSION * n);
                strlen = json_enc_unicode(ep, bytes, bytes + n);
                if (strlen < 0) {
                    if (aligned_alloc != NULL) {
                        erts_free_aligned_binary_bytes_extra(aligned_alloc, ERTS_ALC_T_BINARY_BUFFER);
                    }
                    goto fail;
                }
                ep += strlen;

                obj = THE_NON_VALUE;
                reds = 0; // Yield.
            } else {
                ENSURE_BUFFER(MAX_UTF8_EXPANSION * len + 1);
                strlen = json_enc_unicode(ep, bytes, bytes + len);
                if (aligned_alloc != NULL) {
                    erts_free_aligned_binary_bytes_extra(aligned_alloc, ERTS_ALC_T_BINARY_BUFFER);
                }
                if (unlikely(strlen < 0)) { goto fail; }
                ep += strlen;
                *ep++ = '"';
                reds -= len / TERM_TO_JSON_MEMCPY_FACTOR;
                goto outer_loop;
            }
            break;
        }

        case ENC_JSON_COPY: {
            Uint len = (Uint) obj;
            byte *bytes = (byte *) WSTACK_POP(s);
            if (0) {
                Uint bitoffs;
                Uint bitsize;
            enc_json_start:
                if (unlikely(tag_val_def(obj) != BINARY_DEF)) { goto fail; }
                ERTS_GET_BINARY_BYTES(obj, bytes, bitoffs, bitsize);
                if (unlikely(bitsize != 0)) { goto fail; }
                if (unlikely(bitoffs % 8 != 0)) { goto fail; }
                bytes += bitoffs / 8;
                len = binary_size(obj);
            }

            if (ctx != NULL && len > reds * TERM_TO_JSON_MEMCPY_FACTOR) {
                // Copy partial binary.
                Uint n = reds * TERM_TO_JSON_MEMCPY_FACTOR;
                WSTACK_PUSH3(s, (UWord) (bytes + n), ENC_JSON_COPY, len - n);
                ENSURE_BUFFER(n);
                sys_memcpy(ep, bytes, n);
                ep += n;
                obj = THE_NON_VALUE;
                reds = 0; // Yield.
            } else {
                // Copy remaining.
                ENSURE_BUFFER(len);
                sys_memcpy(ep, bytes, len);
                ep += len;
                reds -= len / TERM_TO_JSON_MEMCPY_FACTOR;
                goto outer_loop;
            }
            break;
        }

        default:
            goto fail;
        }

    encode_term:
        if (ctx && --reds <= 0) {
            *reds_arg = 0;
            ctx->obj = obj;
            ctx->ep = ep;
            WSTACK_SAVE(s, &ctx->wstack);
            return JSON_YIELD;
        }

        // obj contains the next thing to encode.
    encode_term_no_reduction_check:
        switch (tag_val_def(obj)) {
        case NIL_DEF:
            ENSURE_BUFFER(2);
            *ep++ = '['; *ep++ = ']';
            break;

        case ATOM_DEF:
            switch (obj) {
            case am_true:
                ENSURE_BUFFER(4); ep[0] = 't'; ep[1] = 'r'; ep[2] = 'u'; ep[3] = 'e'; ep += 4;
                break;
            case am_false:
                ENSURE_BUFFER(5); ep[0] = 'f'; ep[1] = 'a'; ep[2] = 'l'; ep[3] = 's'; ep[4] = 'e'; ep += 5;
                break;
            case am_null:
              encode_null:
                ENSURE_BUFFER(4); ep[0] = 'n'; ep[1] = 'u'; ep[2] = 'l'; ep[3] = 'l'; ep += 4;
                break;
            default:
                if (flags & JSON_USE_NIL && ERTS_IS_ATOM_STR("nil", obj)) {
                    goto encode_null;
                }
                goto fail;
            }
            break;

        case SMALL_DEF: {
            // Emit a small integer.
            Sint val = signed_val(obj);
            ENSURE_BUFFER(22); // 20 chars is enough for -(2^63)-1 == -9223372036854775807.
            ep += i64ToAsciiTable((char *) ep, (long long) val);
            break;
        }

        case BIG_DEF: {
            // Emit a big integer.
            // Each byte turns into at most 3 decimal digits + 1 for sign.
            Uint big_bufsize = big_bytes(obj) * 3 + 1;
            Uint n;
            ENSURE_BUFFER(big_bufsize);
            n = erts_big_to_binary_bytes(obj, 10, (char *) ep, big_bufsize);
            // erts_big_to_binary writes the bytes at the end of the buffer,
            // so shift them to the beginning.
            if (n < big_bufsize) { memmove(ep, ep + big_bufsize - n, n); }
            ep += n;
            break;
        }

        case LIST_DEF:
            ENSURE_BUFFER(1);
            *ep++ = '[';
            goto enc_array_element;

        case TUPLE_DEF: {
            Eterm *tuple = tuple_val(obj);
            switch (arityval(*tuple)) {
            case 1:
                // A single-element tuple containing a list represents a JSON object.
                switch (tag_val_def(tuple[1])) {
                case NIL_DEF:
                    ENSURE_BUFFER(2);
                    *ep++ = '{'; *ep++ = '}';
                    goto outer_loop;
                case LIST_DEF:
                    ENSURE_BUFFER(1);
                    *ep++ = '{';
                    obj = tuple[1];
                    goto enc_object_element;
                }
                goto fail;
            case 2:
                // A two-element tuple whose first element is 'json' is preencoded JSON.
                if (unlikely(tuple[1] != am_json)) { goto fail; };
                obj = tuple[2];
                goto enc_json_start;
            }
            goto fail;
        }

        case MAP_DEF:
            // An erlang map is converted to a JSON object.
            if (is_flatmap(obj)) {
                flatmap_t *mp = (flatmap_t *) flatmap_val(obj);
                Uint size = flatmap_get_size(mp);

                ENSURE_BUFFER(2); // Enough for '{', and '}' if map is empty.
                *ep++ = '{';
                if (size > 0) {
                    Eterm *kptr = flatmap_get_keys(mp);
                    Eterm *vptr = flatmap_get_values(mp);
                    WSTACK_PUSH4(s, (UWord) kptr, (UWord) vptr, ENC_FLATMAP_PAIR, size);
                } else {
                    *ep++ = '}';
                }
            } else {
                Uint node_sz;
                Uint *ptr = boxed_val(obj);
                Eterm hdr = *ptr++;
                ASSERT(is_header(hdr));
                ENSURE_BUFFER(1);
                switch (hdr & _HEADER_MAP_SUBTAG_MASK) {
                case HAMT_SUBTAG_HEAD_ARRAY:
                    *ep++ = '{';
                    ptr++; // Skip arity on map HEAD.
                    node_sz = 16;
                    WSTACK_PUSH2(s, ENC_MAP_LAST, THE_NON_VALUE);
                    break;
                case HAMT_SUBTAG_HEAD_BITMAP:
                    *ep++ = '{';
                    ptr++; // Skip arity on map HEAD.
                    WSTACK_PUSH2(s, ENC_MAP_LAST, THE_NON_VALUE);
                    // FALL THROUGH
                case HAMT_SUBTAG_NODE_BITMAP:
                    node_sz = hashmap_bitcount(MAP_HEADER_VAL(hdr));
                    ASSERT(node_sz < 17);
                    break;
                default:
                    erts_exit(ERTS_ERROR_EXIT, "bad header\r\n");
                }
                WSTACK_RESERVE(s, node_sz * 2);
                while (node_sz--) {
                    WSTACK_FAST_PUSH(s, ENC_HASHMAP_NODE);
                    WSTACK_FAST_PUSH(s, *ptr++);
                }
            }
            break;

        case FLOAT_DEF: {
            FloatDef f;
            byte *epp;
            GET_DOUBLE(obj, f);
            ENSURE_BUFFER(24);
            epp = ep;
            ep += sprintf((char *) ep, "%.15g", f.fd);
            // Ensure that a double always contains a decimal point.
            while (epp < ep) {
                const byte c = *epp++;
                if (c == '.' || c == 'e') { goto no_period_needed; }
            }
            *ep++ = '.'; *ep++ = '0';
          no_period_needed:
            break;
        }

        case BINARY_DEF: {
            byte *aligned_alloc = NULL;
            int chunked_conversion;
            byte *bytes;
            Uint bitoffs;
            Uint bitsize;
            Uint len;
            Sint strlen;

            ERTS_GET_BINARY_BYTES(obj, bytes, bitoffs, bitsize);

            // We don't handle bitstrings.
            if (unlikely(bitsize != 0)) { goto fail; }

            len = binary_size(obj);
            chunked_conversion = (! ENCODE_ALL) && ctx != NULL && len > reds * TERM_TO_JSON_MEMCPY_FACTOR;
            if (bitoffs % 8 != 0) {
                bytes = erts_get_aligned_binary_bytes_extra(
                    obj, &aligned_alloc,
                    (chunked_conversion ? ERTS_ALC_T_EXT_TERM_DATA : ERTS_ALC_T_TMP),
                    0);
                if (unlikely(bytes == NULL)) { goto fail; }
            }

            if (chunked_conversion) {
                ENSURE_BUFFER(1);
                *ep++ = '"';
            	WSTACK_PUSH4(s, (UWord) bytes, (UWord) aligned_alloc, ENC_BIN_COPY, len);
            } else {
                ENSURE_BUFFER(MAX_UTF8_EXPANSION * len + 2);
                *ep++ = '"';
                strlen = json_enc_unicode(ep, bytes, bytes + len);
                if (aligned_alloc != NULL) {
                    erts_free_aligned_binary_bytes_extra(aligned_alloc, ERTS_ALC_T_TMP);
                }
                if (unlikely(strlen < 0)) { goto fail; }
                ep += strlen;
                *ep++ = '"';
                reds -= len / TERM_TO_JSON_MEMCPY_FACTOR;
            }
            break;
        }

#if 0
        case PID_DEF:
        case EXTERNAL_PID_DEF:
        case REF_DEF:
        case EXTERNAL_REF_DEF:
        case PORT_DEF:
        case EXTERNAL_PORT_DEF:
        case EXPORT_DEF:
        case FUN_DEF:
#endif
        default:
            goto fail;
        }
    }

    DESTROY_WSTACK(s);
    if (ctx) {
        ASSERT(ctx->wstack.wstart == NULL);
        *reds_arg = reds;
    }
    *result_bin_arg = erts_bin_realloc(*result_bin_arg, ep - (byte *) (*result_bin_arg)->orig_bytes);
    return JSON_DONE;

fail:
    DESTROY_WSTACK(s);
    if (ctx) {
        ASSERT(ctx->wstack.wstart == NULL);
        *reds_arg = reds;
    }
    // If we were going to return the partially-encoded JSON we would realloc
    // the buffer here.
    // *result_bin_arg = erts_bin_realloc(*result_bin_arg, ep - (byte *) (*result_bin_arg)->orig_bytes);
    return JSON_BADARG;
}



// From https://www.slideshare.net/andreialexandrescu1/three-optimization-tips-for-c-15708507

#define P01 10
#define P02 100
#define P03 1000
#define P04 10000
#define P05 100000
#define P06 1000000
#define P07 10000000
#define P08 100000000
#define P09 1000000000
#define P10 10000000000
#define P11 100000000000L
#define P12 1000000000000L

u_int32_t digits10(u_int64_t v);
unsigned int u64ToAsciiTable( char *dst, u_int64_t value);

u_int32_t
digits10(u_int64_t v)
{
    if (v < P01) return 1;
    if (v < P02) return 2;
    if (v < P03) return 3;
    if (v < P12) {
        if (v < P08) {
            if (v < P06) {
                if (v < P04) return 4;
                return 5 + (v >= P05);
            }
            return 7 + (v >= P07);
        }
        if (v < P10) {
            return 9 + (v >= P09);
        }
        return 11 + (v >= P11);
    }
    return 12 + digits10(v / P12);
}

unsigned int
u64ToAsciiTable( char *dst, u_int64_t value)
{
    static const char digits[201] =
        "0001020304050607080910111213141516171819"
        "2021222324252627282930313233343536373839"
        "4041424344454647484950515253545556575859"
        "6061626364656667686970717273747576777879"
        "8081828384858687888990919293949596979899";
    u_int32_t const length = digits10(value);
    u_int32_t next = length - 1;
    while (value >= 100) {
        const int i = (value % 100) * 2;
        value /= 100;
        dst[next] = digits[i + 1];
        dst[next - 1] = digits[i];
        next -= 2;
    }
    // Handle last 1-2 digits.
    if (value < 10) {
        dst[next] = '0' + (u_int32_t) value;
    } else {
        const int i = (u_int32_t) value * 2;
        dst[next] = digits[i + 1];
        dst[next - 1] = digits[i];
    }
    return length;
}

unsigned
i64ToAsciiTable(char *dst, int64_t value)
{
    if (value < 0) {
        *dst++ = '-';
        return 1 + u64ToAsciiTable(dst, -value);
    } else {
        return u64ToAsciiTable(dst, value);
    }
}

#define U4 0	 		// Four-byte Unicode.
#define U3 1			// Three-byte Unicode.
#define U2 2			// Two-byte Unicode.
#define A  3			// Safe ASCII.
// #define E2 0			// Escape with backslash.
#define E6 4			// Escape as "\uHHHH".
#define C  5			// Unicode continuation.
#define B  6			// Bad character.

static const byte unicode_enc_map[] = {
//   0    1    2    3    4    5    6    7     8    9    A    B    C    D    E    F
    E6,	 E6,  E6,  E6,  E6,  E6,  E6,  E6,  'b', 't', 'n',  E6,	 E6, 'r',  E6,  E6, // 0_
    E6,	 E6,  E6,  E6,  E6,  E6,  E6,  E6,   E6,  E6,  E6,  E6,	 E6,  E6,  E6,  E6, // 1_
     A,	  A, '"',   A,   A,   A,   A,   A,    A,   A,   A,   A,	  A,   A,   A,   A, // 2_
     A,	  A,   A,   A,   A,   A,   A,   A,    A,   A,   A,   A,	  A,   A,   A,   A, // 3_

     A,	  A,   A,   A,   A,   A,   A,   A,    A,   A,   A,   A,	  A,   A,   A,   A, // 4_
     A,	  A,   A,   A,   A,   A,   A,   A,    A,   A,   A,   A,'\\',   A,   A,   A, // 5_
     A,	  A,   A,   A,   A,   A,   A,   A,    A,   A,   A,   A,	  A,   A,   A,   A, // 6_
     A,	  A,   A,   A,   A,   A,   A,   A,    A,   A,   A,   A,	  A,   A,   A,  E6, // 7_

     B,	  B,   C,   C,   C,   C,   C,   C,    C,   C,   C,   C,	  C,   C,   C,   C, // 8_
     C,	  C,   C,   C,   C,   C,   C,   C,    C,   C,   C,   C,	  C,   C,   C,   C, // 9_
     C,	  C,   C,   C,   C,   C,   C,   C,    C,   C,   C,   C,	  C,   C,   C,   C, // A_
     C,	  C,   C,   C,   C,   C,   C,   C,    C,   C,   C,   C,	  C,   C,   C,   C, // B_

    U2,	 U2,  U2,  U2,  U2,  U2,  U2,  U2,   U2,  U2,  U2,  U2,	 U2,  U2,  U2,  U2, // C_
    U2,	 U2,  U2,  U2,  U2,  U2,  U2,  U2,   U2,  U2,  U2,  U2,	 U2,  U2,  U2,  U2, // D_
    U3,	 U3,  U3,  U3,  U3,  U3,  U3,  U3,   U3,  U3,  U3,  U3,	 U3,  U3,  U3,  U3, // E_
    U4,	 U4,  U4,  U4,  U4,   B,   B,   B,    B,   B,   B,   B,	  B,   B,   B,   B, // F_
};

byte *json_encode_byte(byte *d, int ucs);

byte *
json_encode_byte(byte *d, int ucs)
{
    const int low = (ucs >> 4) & 0xf;
    const int high = ucs & 0xf;
    *d++ = '0';
    *d++ = '0';
    *d++ = (low  < 10 ? '0' : ('A' - 10)) + low;
    *d++ = (high < 10 ? '0' : ('A' - 10)) + high;
    return d;
}

Sint
json_enc_unicode(byte *d, byte *s, byte *send)
{
    const byte *dstart = d;
    while (s < send) {
        const byte code = unicode_enc_map[*s];
        switch (code) {
        case U4: *d++ = *s++; // FALL THROUGH
        case U3: *d++ = *s++; // FALL THROUGH
        case U2: *d++ = *s++; // FALL THROUGH
        case A:  *d++ = *s++; continue;
        case E6: *d++ = '\\'; *d++ = 'u'; d = json_encode_byte(d, *s++); continue;
        case B: return -1;
        case C: return -1;
        default: *d++ = '\\'; *d++ = code; s++; continue;
        }
    }
    return d - dstart;
}

/**********************************************************************/

/* Continue work after either json_to_term_1 or json_to_term__2 returns via
   BIF_TRAP1. */
static BIF_RETTYPE json_to_term_trap_1(BIF_ALIST_1)
{
    Eterm *tp = tuple_val(BIF_ARG_1);
    Eterm Json = tp[1];
    Eterm bt = tp[2];
    Binary *context = erts_magic_ref2bin(bt);

    return erts_json_to_term_int(BIF_P, Json, /* flags */ 0, context);
}


HIPE_WRAPPER_BIF_DISABLE_GC(json_to_term, 1)

/* erlang:json_to_term/1 entry point. */
BIF_RETTYPE json_to_term_1(BIF_ALIST_1)
{
    return erts_json_to_term_int(BIF_P, BIF_ARG_1, 0, NULL);
}

HIPE_WRAPPER_BIF_DISABLE_GC(json_to_term, 2)

/* erlang:json_to_term/2 entry point. */
BIF_RETTYPE json_to_term_2(BIF_ALIST_2)
{
    Process *p = BIF_P;
    Eterm Options = BIF_ARG_2;
    Uint32 flags = 0;

    while (is_list(Options)) {
        Eterm arg = CAR(list_val(Options));
        if (ERTS_IS_ATOM_STR("return_maps", arg)) {
            flags |= JSON_RETURN_MAPS;
        } else if (ERTS_IS_ATOM_STR("use_nil", arg)) {
            flags |= JSON_USE_NIL;
        } else {
            error:
            BIF_ERROR(p, EXC_BADARG);
        }
        Options = CDR(list_val(Options));
    }
    if (is_not_nil(Options)) {
        goto error;
    }

    return erts_json_to_term_int(BIF_P, BIF_ARG_1, flags, NULL);
}

static int j2t_context_destructor(Binary *context_bin)
{
    J2TContext *context = ERTS_MAGIC_BIN_DATA(context_bin);
    if (context->alive) {
        context->alive = 0;
        DESTROY_SAVED_WSTACK(&context->wstack);
    }
    return 1;
}

#define ST_INIT 0

static Eterm
erts_json_to_term_int(Process *p, Eterm Json, Uint32 flags, Binary *context_b)
{
#ifndef EXTREME_TTB_TRAPPING
    Sint reds = (Sint) (ERTS_BIF_REDS_LEFT(p) * TERM_TO_JSON_LOOP_FACTOR);
#else
    Sint reds = 4; /* For testing */
#endif
    Sint initial_reds = reds;
    int is_first_call;
    J2TContext context_buf;
    J2TContext *context;
    Eterm result_term;

    if (! is_binary(Json)) {
        BIF_ERROR(p, EXC_BADARG);
    }

    if (context_b == NULL) {
        byte *bytes;
        Uint bitoffs, bitsize, len;

        ERTS_GET_BINARY_BYTES(Json, bytes, bitoffs, bitsize);

        if (bitoffs != 0 || bitsize % 8 != 0) {
            BIF_ERROR(p, EXC_BADARG);
        }

        len = binary_size(Json);

        is_first_call = 1;
        context_buf.alive = 1;
        context_buf.flags = flags;
        context_buf.null_atom = (flags & JSON_USE_NIL) ? ERTS_MAKE_AM("nil") : am_null;
        context_buf.state = ST_INIT;
        context_buf.ep = bytes;
        context_buf.vp = NULL;
        context_buf.endp = bytes + len;
        context_buf.term = THE_NON_VALUE;
        context_buf.strdiff = 0;
        context_buf.wstack.wstart = NULL;
        context = &context_buf;
    } else {
        is_first_call = 0;
        context = ERTS_MAGIC_BIN_DATA(context_b);
    }

    switch (dec_json_int(p, context, &reds, &result_term)) {
    case JSON_YIELD: {
        // Ran out of reductions; yield.
        Eterm *hp;
        Eterm ctx_term;

        if (context_b == NULL) {
            context_b = erts_create_magic_binary(sizeof context_buf, j2t_context_destructor);
            context = ERTS_MAGIC_BIN_DATA(context_b);
            sys_memcpy(context, &context_buf, sizeof context_buf);
        }
        if (is_first_call) {
            erts_set_gc_state(p, 0);
        }

        // I'm not sure why this creates a tuple for BIF_TRAP1 instead of using BIF_TRAP2.
        hp = HAlloc(p, ERTS_MAGIC_REF_THING_SIZE + 3); // +3 for 2-tuple.
        ctx_term = erts_mk_magic_ref(&hp, &MSO(p), context_b);
        BUMP_ALL_REDS(p);
        BIF_TRAP1(&json_to_term_trap_export, p, TUPLE2(hp, Json, ctx_term));
        /*NOTREACHED*/
    }

    case JSON_BADARG:
        if (context_b && erts_refc_read(&context_b->intern.refc, 0) == 0) {
            erts_bin_free(context_b);
        }
        if (! is_first_call) {
            erts_set_gc_state(p, 1);
            ERTS_BIF_ERROR_TRAPPED1(p, EXC_BADARG, bif_export[BIF_json_to_term_1], Json);
        } else {
            BIF_ERROR(p, EXC_BADARG);
        }
        /*NOTREACHED*/

    case JSON_DONE:
        BUMP_REDS(p, (initial_reds - reds) / TERM_TO_JSON_LOOP_FACTOR);
        context->alive = 0;
        if (context_b && erts_refc_read(&context_b->intern.refc, 0) == 0) {
            erts_bin_free(context_b);
        }
        if (! is_first_call) {
            erts_set_gc_state(p, 1);
        }
        BIF_RET(result_term);
        /*NOTREACHED*/
    }
    abort();
}


#include <alloca.h>
#include <string.h>

#define DEC_ARRAY       ((Eterm) 0xFFFFFFF0)
#define DEC_OBJECT      ((Eterm) 0xFFFFFFF1)

enum {
    st_init = ST_INIT,
    st_lst0,
    st_intm,
    st_int0,
    st_int1,
    st_flt0,
    st_flt1,
    st_flt2,
    st_flt3,
    st_flt4,
    st_str0,
    st_str1,
    st_obj0,
    st_obj1,
    st_true0,
    st_false0,
    st_null0,
    st_end,
    st_str_decode,
} state;

#define WSTACK_PEEK(s)          (s.wsp[-1])
#define WSTACK_PEEKN(s, n)      (s.wsp[-1 - (n)])

#define IS_CONTINUATION(c) (((c) & 0xC0) == 0x80)

#define CC_WHITESPACE   ' ': case '\t': case '\n': case '\r'
#define CC_DIGIT19      '1': case '2': case '3': case '4': case '5': case '6': case '7': case '8': case '9'
#define CC_DIGIT09      '0': case CC_DIGIT19
#define CC_E            'E': case 'e'

static inline int
__attribute__ ((const))
hexdec(byte c)
{
    if (c >= '0' && c <= '9') { return c - '0'; }
    c &= ~ ('a' - 'A'); // Mask off 0x20 bit to map lowercase to uppercase.
    if (c >= 'A' && c <= 'F') { return c - 'A' + 0xA; }
    return -1;
}

static Eterm
chars_to_float(Process *p, const byte * const s, int slen)
{
    Eterm *hp = HAlloc(p, FLOAT_SIZE_OBJECT);
    char *dbuf = alloca(slen);
    FloatDef f;
    memcpy(dbuf, s, slen);
    dbuf[slen] = '\0';
    f.fd = atof((char *) dbuf);
    PUT_DOUBLE(f, hp);
    return make_float(hp);
}

// Decodes a JSON string from s to a UTF-8 string d, stopping at se.  May copy
// a few bytes past se if se is in the middle of a JSON-encoded character,
// which can happen when copying a JSON string in parts.  Returns a pointer
// just after the last input byte decoded.
static byte const *
chars_to_utf8(byte *d, const byte *s, const byte * const se, int strdiff, const byte ** const de)
{
    if (strdiff == 0) {
        // If strdiff == 0 we know that the JSON string can be doesn't contain
        // any Unicode escapes and thus can be copied directly into the
        // destination without any decoding.
        const size_t len = se - s;
        sys_memcpy(d, s, len);
        s = se;
        if (de != NULL) { *de = d + len; }
    } else {
        while (s < se) {
            const int c = *s;
            if (c == '\\') {
                switch (s[1]) {
                case '"':  *d++ = '"';  s += 2; break;
                case '\\': *d++ = '\\'; s += 2; break;
                case '/':  *d++ = '/';  s += 2; break;
                case 'b':  *d++ = '\b'; s += 2; break;
                case 'f':  *d++ = '\f'; s += 2; break;
                case 'n':  *d++ = '\n'; s += 2; break;
                case 'r':  *d++ = '\r'; s += 2; break;
                case 't':  *d++ = '\t'; s += 2; break;
                case 'u': {
                    int h0 = hexdec(s[2]);
                    int h1 = hexdec(s[3]);
                    int h2 = hexdec(s[4]);
                    int h3 = hexdec(s[5]);
                    int unicode = (h0 << 12) + (h1 << 8) + (h2 << 4) + h3;
                    s += 6;
                    if        (unicode <     0x80) {
                        *d++ =         unicode;
                    } else if (unicode <    0x800) { // 2-byte UTF-8.
                        d[0] = 0xC0 | ((unicode >>  6)       );
                        d[1] = 0x80 | ((unicode      ) & 0x3F);
                        d += 2;
                    } else if (unicode < 0xD800 || unicode >= 0xE000) { // 3-byte UTF-8.
                        d[0] = 0xE0 | ((unicode >> 12)       );
                        d[1] = 0x80 | ((unicode >>  6) & 0x3F);
                        d[2] = 0x80 | ((unicode      ) & 0x3F);
                        d += 3;
                    } else { // unicode < 0xDC00 == 4-byte UTF-8
                        int h4, h5, h6, h7, ulow;
                        ASSERT(unicode >= 0xD800 && unicode < 0xDC00);
                        ASSERT(s[0] == '\\');
                        ASSERT(s[1] == 'u');
                        h4 = hexdec(s[2]);
                        h5 = hexdec(s[3]);
                        h6 = hexdec(s[4]);
                        h7 = hexdec(s[5]);
                        ulow = (h4 << 12) + (h5 << 8) + (h6 << 4) + h7;
                        ASSERT(ulow >= 0xDC00 && ulow < 0xE000);
                        unicode = 0x10000 + ((unicode & 0x03FF) << 10) + (ulow & 0x03FF);
                        s += 6;
                        d[0] = 0xF0 | ((unicode >> 18) & 0x07);
                        d[1] = 0x80 | ((unicode >> 12) & 0x3F);
                        d[2] = 0x80 | ((unicode >>  6) & 0x3F);
                        d[3] = 0x80 | ((unicode      ) & 0x3F);
                        d += 4;
                    }
                    break;
                } // case 'u'
                } // switch s[1]
            } else if (c < 0x80) {
                *d++ = c; s++;
            } else if (c < 0xE0) {
                memcpy(d, s, 2); d += 2; s += 2;
            } else if (c < 0xF0) {
                memcpy(d, s, 3); d += 3; s += 3;
            } else if (c < 0xF5) {
                memcpy(d, s, 4); d += 4; s += 4;
            } else {
                abort();
            }
        }

        if (de != NULL) { *de = d; }
    }

    return s;
}

int
dec_json_int(Process *p, J2TContext *ctx, Sint *reds_arg, Eterm *result_term_arg)
{
    // The s WSTACK is used to store lists and object in the process of being
    // parsed.  For a list we have:
    //   DEC_ARRAY 0
    //   DEC_ARRAY elt1 1
    //   DEC_ARRAY elt1 elt2 2
    //   ...
    // For an object we have:
    //   DEC_OBJECT 0
    //   DEC_OBJECT key1 1
    //   DEC_OBJECT key1 val1 2
    //   DEC_OBJECT key1 val1 key2 3
    //   ...
    WSTACK_DECLARE(s);

    Sint reds = *reds_arg;

    const Uint32 flags = ctx->flags;
    int state = ctx->state;
    const byte *ep = ctx->ep;
    const byte *vp = ctx->vp; // Pointer to start of value currently being decoded.
    const byte * const endp = ctx->endp;
    Eterm term = ctx->term;
    int strdiff = ctx->strdiff;

    WSTACK_CHANGE_ALLOCATOR(s, ERTS_ALC_T_SAVED_ESTACK);

    if (ctx->wstack.wstart) {
        WSTACK_RESTORE(s, &ctx->wstack);
    }

    if (state == st_str_decode) {
        goto decode_string;
    }

#define PARSE_INT(s, se) erts_chars_to_integer(p, (char *) s, se - s, 10)
#define PARSE_FLOAT(s, se) chars_to_float(p, s, se - s)
#define SKIP_WHITESPACE()						\
    do {								\
        byte cc;							\
        while (likely((ep < endp) &&					\
                      ((cc = *ep) == ' ' || cc == '\t' || cc == '\n' || cc == '\r'))) { \
            ep++;							\
        }								\
    } while (0)


    while (1) {
        int c;
    next_char:
        if (ep < endp) {
            c = *ep++;
        } else {
            c = EOF;
        }

        switch (state) {
        case st_init:
        case st_lst0:
            switch (c) {
            case '0':           state = st_int0; vp = ep - 1; break;
            case CC_DIGIT19:    state = st_int1; vp = ep - 1; break;
            case '-':           state = st_intm; vp = ep - 1; break;
            case '"':           state = st_str0; vp = ep; strdiff = 0; break;
            case 't':           state = st_true0; break;
            case 'f':           state = st_false0; break;
            case 'n':           state = st_null0; break;
            case '[':           state = st_lst0; WSTACK_PUSH2(s, DEC_ARRAY, 0); break;
            case '{':           state = st_obj0; WSTACK_PUSH2(s, DEC_OBJECT, 0); break;
            case ']':           if (state != st_lst0) goto fail;
                                term = THE_NON_VALUE; goto end_list;
            case CC_WHITESPACE: SKIP_WHITESPACE(); break;
            default:            goto fail;
            }
            break;
        case st_intm:
            switch (c) {
            case '0':		state = st_int0; break;
            case CC_DIGIT19:    state = st_int1; break;
            default:            goto fail;
            }
            break;
        case st_int0:
            switch (c) {
            case '.':           state = st_flt0; break;
            case CC_E:          state = st_flt2; break;
            case ',':           term = PARSE_INT(vp, ep - 1); goto handle_comma;
            case ']':           term = PARSE_INT(vp, ep - 1); goto end_list;
            case '}':           term = PARSE_INT(vp, ep - 1); goto end_object;
            case CC_WHITESPACE: term = PARSE_INT(vp, ep - 1); state = st_end; SKIP_WHITESPACE(); break;
            case EOF:		term = PARSE_INT(vp, ep);     goto done;
            default :           goto fail;
            }
            break;
        case st_int1:
            switch (c) {
            case CC_DIGIT09:    state = st_int1; break;
            case '.':           state = st_flt0; break;
            case CC_E:          state = st_flt2; break;
            case ',':           term = PARSE_INT(vp, ep - 1); goto handle_comma;
            case ']':           term = PARSE_INT(vp, ep - 1); goto end_list;
            case '}':           term = PARSE_INT(vp, ep - 1); goto end_object;
            case CC_WHITESPACE: term = PARSE_INT(vp, ep - 1); state = st_end; SKIP_WHITESPACE(); break;
            case EOF:		term = PARSE_INT(vp, ep);     goto done;
            default:            goto fail;
            }
            break;
        case st_flt0:
            switch (c) {
            case CC_DIGIT09:    state = st_flt1; break;
            default:            goto fail;
            }
            break;
        case st_flt1:
            switch (c) {
            case CC_DIGIT09:    break;
            case CC_E:          state = st_flt2; break;
            case ',':           term = PARSE_FLOAT(vp, ep - 1); goto handle_comma;
            case ']':           term = PARSE_FLOAT(vp, ep - 1); goto end_list;
            case '}':           term = PARSE_FLOAT(vp, ep - 1); goto end_object;
            case CC_WHITESPACE: term = PARSE_FLOAT(vp, ep - 1); state = st_end; SKIP_WHITESPACE(); break;
            case EOF:		term = PARSE_FLOAT(vp, ep);     goto done;
            default:            goto fail;
            }
            break;
        case st_flt2:
            switch (c) {
            case '+': case '-': state = st_flt3; break;
            case CC_DIGIT09:    state = st_flt4; break;
            default:            goto fail;
            }
            break;
        case st_flt3:
            switch (c) {
            case CC_DIGIT09:    state = st_flt4; break;
            default:            goto fail;
            }
            break;
        case st_flt4:
            switch (c) {
            case CC_DIGIT09:    break;
            case ',':           term = PARSE_FLOAT(vp, ep - 1); goto handle_comma;
            case ']':           term = PARSE_FLOAT(vp, ep - 1); goto end_list;
            case '}':           term = PARSE_FLOAT(vp, ep - 1); goto end_object;
            case CC_WHITESPACE: term = PARSE_FLOAT(vp, ep - 1); state = st_end; SKIP_WHITESPACE(); break;
            case EOF:		term = PARSE_FLOAT(vp, ep);     goto done;
            default:            goto fail;
            }
            break;
        case st_str0:
            switch (c) {
            case '\\':          state = st_str1; break;
            case '"':
                {
                    // We've scanned to the end of a string.  vp is pointing
                    // to the start of the string and ep is pointing just
                    // after the '"' that ends the string, so we have to
                    // subtract 1.
                    Uint ilen = ep - vp - 1;
                    Uint olen = ilen - strdiff;
                    if (olen < ERL_ONHEAP_BIN_LIMIT) {
                        // If the JSON string will be a heap-binary, convert immediately.
                        ErlHeapBin *hb = (ErlHeapBin *) HAlloc(p, heap_bin_size(olen));
                        hb->thing_word = header_heap_bin(olen);
                        hb->size = olen;
                        (void) chars_to_utf8((byte *) hb->data, vp, ep - 1, strdiff, NULL);
                        term = make_binary(hb);
                        reds -= ilen / TERM_TO_JSON_MEMCPY_FACTOR;
                        state = st_end;
                    } else {
                        // Otherwise the JSON string will be a ref-counted binary.
                        Uint n = reds * TERM_TO_JSON_MEMCPY_FACTOR;
                        Binary *result_bin = erts_bin_nrml_alloc(olen);
                        if (DECODE_ALL || ilen < n) {
                            // JSON string is small enough to convert immediately.
                            (void) chars_to_utf8((byte *) result_bin->orig_bytes, vp, ep - 1, strdiff, NULL);
                            term = erts_build_proc_bin(&MSO(p), HAlloc(p, PROC_BIN_SIZE), result_bin);
                            reds -= ilen / TERM_TO_JSON_MEMCPY_FACTOR;
                            state = st_end;
                        } else {
                            // Start decoding long JSON string in parts.
                            const byte *d2;
                            vp = chars_to_utf8((byte *) result_bin->orig_bytes, vp, vp + n, strdiff, &d2);
                            WSTACK_PUSH(s, (Eterm) d2);
                            term = erts_build_proc_bin(&MSO(p), HAlloc(p, PROC_BIN_SIZE), result_bin);
                            reds = 0; // Yield.
                            state = st_str_decode;
                        }
                    }
                }
                break;
            default:
                if (unlikely(c < 0x20)) {
                    goto fail;
                } else if (c < 0x80) {
                    ;
                } else if (unlikely(c < 0xC0)) {
                    goto fail;
                } else if (c < 0xE0) {
                    if (unlikely(ep + 1 >= endp)) goto fail;
                    if (unlikely(! IS_CONTINUATION(ep[0]))) goto fail;
                    // Reject overlong encoding (from Jiffy by Paul J. Davis).
                    if (unlikely((c & 0x1E) == 0))          goto fail;
                    ep += 1;
                } else if (c < 0xF0) {
                    int u;
                    if (unlikely(ep + 2 >= endp)) goto fail;
                    if (unlikely(! IS_CONTINUATION(ep[0]))) goto fail;
                    if (unlikely(! IS_CONTINUATION(ep[1]))) goto fail;
                    // Reject overlong encoding.
                    if (unlikely((c & 0x0F) + (ep[0] & 0x20) == 0)) goto fail;
                    // Reject surrogate pairs in the range 0xD800 to 0xDFFF.
                    u = ((c     & 0x0F) << 12) +
                        ((ep[0] & 0x3F) <<  6) +
                        ((ep[1] & 0x3F)      );
                    if (unlikely(u >= 0xD800 && u < 0xE000)) goto fail;
                    ep += 2;
                } else if (c < 0xF5) {
                    int u;
                    if (unlikely(ep + 3 >= endp)) goto fail;
                    if (unlikely(! IS_CONTINUATION(ep[0]))) goto fail;
                    if (unlikely(! IS_CONTINUATION(ep[1]))) goto fail;
                    if (unlikely(! IS_CONTINUATION(ep[2]))) goto fail;
                    // Reject overlong encoding.
                    if (unlikely((c & 0x07) + (ep[0] & 0x30) == 0)) goto fail;
                    // Reject code points above 0x10FFFF.
                    u = ((c     & 0x07) << 18) +
                        ((ep[0] & 0x3F) << 12) +
                        ((ep[1] & 0x3F) <<  6) +
                        ((ep[2] & 0x3F)      );
                    if (unlikely(u >= 0x110000)) goto fail;
                    ep += 3;
                } else {
                    goto fail;
                }
                break;
            }
            break;
        case st_str1:
            switch (c) {
            case '"': case '\\': case '/': case 'b': case 'f': case 'n': case 'r': case 't':
                state = st_str0; strdiff++; break;
            case 'u': {
                int h0, h1, h2, h3, unicode;
                if (unlikely(ep + 4 >= endp)) { goto fail; }
                h0 = hexdec(ep[0]);
                h1 = hexdec(ep[1]);
                h2 = hexdec(ep[2]);
                h3 = hexdec(ep[3]);
                if (unlikely(h0 < 0 || h1 < 0 || h2 < 0 || h3 < 0)) { goto fail; }
                unicode = (h0 << 12) + (h1 << 8) + (h2 << 4) + h3;
                ep += 4;
                state = st_str0;
                if      (unicode <   0x80) { strdiff += (6 - 1); } // 1-byte UTF-8.
                else if (unicode <  0x800) { strdiff += (6 - 2); } // 2-byte UTF-8.
                else if (unicode < 0xD800 || unicode >= 0xE000) {
                    strdiff += (6 - 3); }                          // 3-byte UTF-8.
                else if (unicode < 0xDC00) {
                    // Check for an escaped UTF-16 surrogate pair.  If the
                    // value we just decoded is in the range 0xD800 to 0xDBFF
                    // it is the first half of a surrogate pair.  In that
                    // case, validate that the second half is another escape
                    // sequence that decodes to a value in the range 0xDC00 to
                    // 0xDFFF.
                    int h4, h5, h6, h7, ulow;
                    if (unlikely(ep + 6 >= endp)) { goto fail; }
                    if (unlikely(ep[0] != '\\'))  { goto fail; }
                    if (unlikely(ep[1] != 'u'))   { goto fail; }
                    h4 = hexdec(ep[2]);
                    h5 = hexdec(ep[3]);
                    h6 = hexdec(ep[4]);
                    h7 = hexdec(ep[5]);
                    if (unlikely(h4 < 0 || h5 < 0 || h6 < 0 || h7 < 0)) { goto fail; }
                    ulow = (h4 << 12) + (h5 << 8) + (h6 << 4) + h7;
                    if (unlikely(ulow < 0xDC00 || ulow >= 0xE000)) { goto fail; }
                    ep += 6;
                    strdiff += (12 - 4);                           // 4-byte UTF-8.
                } else { goto fail; }
                break;
            } // case 'u'
            default:
                goto fail;
            }
            break;
        case st_obj0:
            switch (c) {
            case '"':           state = st_str0; vp = ep; strdiff = 0; break;
            case '}':           goto empty_object;
            case CC_WHITESPACE: SKIP_WHITESPACE(); break;
            default:            goto fail;
            }
            break;
        case st_obj1:
            switch (c) {
            case '"':           state = st_str0; vp = ep; strdiff = 0; break;
            case CC_WHITESPACE: SKIP_WHITESPACE(); break;
            default:            goto fail;
            }
            break;
        case st_true0:
            if (likely(ep + 1 < endp && c == 'r' && ep[0] == 'u' && ep[1] == 'e')) {
                ep += 2;
                term = am_true;
                state = st_end;
            } else {
                goto fail;
            }
            break;
        case st_false0:
            if (likely(ep + 2 < endp && c == 'a' && ep[0] == 'l' && ep[1] == 's' && ep[2] == 'e')) {
                ep += 3;
                term = am_false;
                state = st_end;
            } else {
                goto fail;
            }
            break;
        case st_null0:
            if (likely(ep + 1 < endp && c == 'u' && ep[0] == 'l' && ep[1] == 'l')) {
                ep += 2;
                term = ctx->null_atom;
                state = st_end;
            } else {
                goto fail;
            }
            break;
        case st_end:
            // Skip whitespace after term.
            switch (c) {
            case ',':           goto handle_comma;
            case ':':           goto handle_colon;
            case ']':           goto end_list;
            case '}':           goto end_object;
            case CC_WHITESPACE: SKIP_WHITESPACE(); break;
            case EOF:		goto done;
            default:            goto fail;
            }
            break;
        } // switch (state)

        if (0) {
        handle_comma: {
                Sint count;
                if (WSTACK_ISEMPTY(s)) goto fail;
                count = (Sint) WSTACK_POP(s);
                ASSERT(count >= 0);
                switch (WSTACK_PEEKN(s, count)) {
                case DEC_ARRAY:
                    WSTACK_PUSH2(s, term, count + 1);
                    state = st_init;
                    goto next_char;
                case DEC_OBJECT:
                    if (count % 2 != 1) goto fail;
                    WSTACK_PUSH2(s, term, count + 1);
                    state = st_obj1;
                    goto next_char;
                }
                goto fail;
            }
        handle_colon: {
                Uint count;
                if (WSTACK_ISEMPTY(s)) goto fail;
                count = (Uint) WSTACK_POP(s);
                ASSERT(count >= 0);
                if (WSTACK_PEEKN(s, count) == DEC_OBJECT) {
                    if (count % 2 != 0) goto fail;
                    WSTACK_PUSH2(s, term, count + 1);
                    state = st_init;
                    goto next_char;
                }
                goto fail;
            }
        end_list: {
                Eterm tail = NIL;
                Eterm *hp;
                Sint count;
                if (WSTACK_ISEMPTY(s)) goto fail;
                count = (Sint) WSTACK_POP(s);
                ASSERT(count >= 0);
                if (WSTACK_PEEKN(s, count) != DEC_ARRAY) goto fail;
                if (term != THE_NON_VALUE) {
                    WSTACK_PUSH(s, term);
                    count++;
                }
                // Create a list from the stack elements.
                hp = HAlloc(p, count * 2); // 2 for CONS for each element
                while (--count >= 0) {
                    tail = CONS(hp, WSTACK_POP(s), tail);
                    hp += 2;
                }
                (void) WSTACK_POP(s); // Pop the DEC_ARRAY marker.
                term = tail;
                state = st_end;
                goto next_char;
            }
        end_object: {
                Sint count;
                if (WSTACK_ISEMPTY(s)) goto fail;
                count = WSTACK_POP(s);
                ASSERT(count >= 0);
                if (count % 2 != 1)  goto fail;
                if (WSTACK_PEEKN(s, count) != DEC_OBJECT) goto fail;
                ASSERT(term != THE_NON_VALUE);
                WSTACK_PUSH(s, term);
                count++;
                count /= 2;
                {
                    // Create a proplist from the stack elements.
                    Eterm *hp = HAlloc(p, count * 5); // 3 for TUPLE2 + 2 for CONS for each element + 2.
                    Eterm tail = NIL;
                    int cc = count;
                    while (--cc >= 0) {
                        Eterm value = WSTACK_POP(s);
                        Eterm key = WSTACK_POP(s);
                        Eterm tuple = TUPLE2(hp, key, value);
                        tail = CONS(hp + 3, tuple, tail);
                        hp += 5;
                    }
                    if (flags & JSON_RETURN_MAPS) {
                        term = erts_map_from_validated_list(p, tail, (Uint) count);
                    } else {
                        // Make tuple wrapper for proplist.
                        hp = HAlloc(p, 2);
                        term = TUPLE1(hp, tail);
                    }
                }
                (void) WSTACK_POP(s); // Pop the DEC_OBJECT marker.
                state = st_end;
                goto next_char;
            }
        empty_object: {
                Eterm *hp;
                Sint count;
                if (WSTACK_ISEMPTY(s)) goto fail;
                count = WSTACK_POP(s);
                ASSERT(count == 0);
                if (WSTACK_PEEKN(s, 0) != DEC_OBJECT) goto fail;
                if (flags & JSON_RETURN_MAPS) {
                    term = erts_map_from_validated_list(p, NIL, 0U);
                } else {
                    hp = HAlloc(p, 2); // 2 for TUPLE1 wrapper.
                    term = TUPLE1(hp, NIL);
                }
                (void) WSTACK_POP(s); // Pop the DEC_OBJECT marker.
                state = st_end;
                goto next_char;
            }
        decode_string: {
                Uint ilen = ep - vp - 1;
                Uint n = reds * TERM_TO_JSON_MEMCPY_FACTOR;
                byte *d1 = (byte *) WSTACK_POP(s);
                if (ilen < n) {
                    // Finish decoding long JSON string.
                    vp = chars_to_utf8(d1, vp, ep - 1, strdiff, NULL);
                    reds -= ilen / TERM_TO_JSON_MEMCPY_FACTOR;
                    state = st_end;
                } else {
                    // Continue decoding long JSON string.
                    const byte *d2;
                    vp = chars_to_utf8(d1, vp, vp + n, strdiff, &d2);
                    WSTACK_PUSH(s, (Eterm) d2);
                    reds = 0; // Yield.
                    // state remains st_str_decode
                }
                goto check_reductions;
            }
        } // if (0)

    check_reductions:
        if (--reds <= 0) {
            *reds_arg = 0;
            ctx->state = state;
            ctx->ep = ep;
            ctx->vp = vp;
            ctx->term = term;
            ctx->strdiff = strdiff;
            WSTACK_SAVE(s, &ctx->wstack);
            return JSON_YIELD;
        };
    } // while (1)

done:
    if (! WSTACK_ISEMPTY(s)) goto fail;
    DESTROY_WSTACK(s);

    *result_term_arg = term;

    return JSON_DONE;

fail:
    DESTROY_WSTACK(s);
    return JSON_BADARG;
}

// Local Variables:
// comment-start:"// "
// comment-end:""
// End:

