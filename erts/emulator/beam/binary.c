/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1996-2023. All Rights Reserved.
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

#include "sys.h"
#include "erl_vm.h"
#include "global.h"
#include "erl_process.h"
#include "error.h"
#include "bif.h"
#include "big.h"
#include "erl_binary.h"
#include "erl_bits.h"
#include "erl_iolist.h"

#define B2L_MIN_EXEC_REDS (CONTEXT_REDS/4)
#define B2L_RESCHED_REDS (CONTEXT_REDS/40)

static BIF_RETTYPE binary_to_list_continue(BIF_ALIST_1);
static Export binary_to_list_continue_export;

void
erts_init_binary(void)
{
    /* Verify Binary alignment... */
    ERTS_CT_ASSERT((offsetof(Binary,orig_bytes) % 8) == 0);
    ERTS_CT_ASSERT((offsetof(ErtsMagicBinary,u.aligned.data) % 8) == 0);

    erts_init_trap_export(&binary_to_list_continue_export,
			  am_erts_internal, am_binary_to_list_continue, 1,
			  &binary_to_list_continue);

}

Eterm
erts_bin_bytes_to_list(Eterm previous, Eterm* hp, const byte* bytes, Uint size, Uint bitoffs)
{
    if (bitoffs == 0) {
	while (size) {
	    previous = CONS(hp, make_small(bytes[--size]), previous);
	    hp += 2;
	}
    } else {
	byte present;
	byte next;
	next = bytes[size];
	while (size) {
	    present = next;
	    next = bytes[--size];
	    previous = CONS(hp, make_small(((present >> (8-bitoffs)) |
					    (next << bitoffs)) & 255), previous);
	    hp += 2;
	}
    }
    return previous;
}

static Eterm integer_to_binary(Process *c_p, Eterm num, int base)
{
    Eterm res;

    if (is_small(num)) {
        char s[128];
        char *c = s;
        Uint digits;

        digits = Sint_to_buf(signed_val(num), base, &c, sizeof(s));
        res = erts_new_binary_from_data(c_p, digits, (byte*)c);
    } else {
        const int DIGITS_PER_RED = 16;
        Uint digits, n;
        byte *bytes;

        digits = big_integer_estimate(num, base);

        if ((digits / DIGITS_PER_RED) > ERTS_BIF_REDS_LEFT(c_p)) {
            ErtsSchedulerData *esdp = erts_get_scheduler_data();

            /* This could take a very long time, tell the caller to reschedule
             * us to a dirty CPU scheduler if we aren't already on one. */
            if (esdp->type == ERTS_SCHED_NORMAL) {
                return THE_NON_VALUE;
            }
        } else {
            BUMP_REDS(c_p, digits / DIGITS_PER_RED);
        }

        bytes = (byte*)erts_alloc(ERTS_ALC_T_TMP, sizeof(byte) * digits);
        n = erts_big_to_binary_bytes(num, base, (char*)bytes, digits);
        res = erts_new_binary_from_data(c_p, n, &bytes[digits - n]);
        erts_free(ERTS_ALC_T_TMP, (void*)bytes);
    }

    return res;
}

BIF_RETTYPE integer_to_binary_1(BIF_ALIST_1)
{
    Eterm res;

    if (is_not_integer(BIF_ARG_1)) {
        BIF_ERROR(BIF_P, BADARG);
    }

    res = integer_to_binary(BIF_P, BIF_ARG_1, 10);

    if (is_non_value(res)) {
        ErtsCodeMFA *mfa = &BIF_TRAP_EXPORT(BIF_integer_to_binary_1)->info.mfa;
        Eterm args[1];
        args[0] = BIF_ARG_1;
        return erts_schedule_bif(BIF_P,
                                 args,
                                 BIF_I,
                                 mfa,
                                 integer_to_binary_1,
                                 ERTS_SCHED_DIRTY_CPU,
                                 am_erlang,
                                 am_integer_to_binary,
                                 1);
    }

    return res;
}

BIF_RETTYPE integer_to_binary_2(BIF_ALIST_2)
{
    Eterm res;
    SWord base;

    if (is_not_integer(BIF_ARG_1) || is_not_small(BIF_ARG_2)) {
        BIF_ERROR(BIF_P, BADARG);
    }

    base = signed_val(BIF_ARG_2);
    if (base < 2 || base > 36) {
        BIF_ERROR(BIF_P, BADARG);
    }

    res = integer_to_binary(BIF_P, BIF_ARG_1, base);

    if (is_non_value(res)) {
        ErtsCodeMFA *mfa = &BIF_TRAP_EXPORT(BIF_integer_to_binary_2)->info.mfa;
        Eterm args[2];
        args[0] = BIF_ARG_1;
        args[1] = BIF_ARG_2;
        return erts_schedule_bif(BIF_P,
                                 args,
                                 BIF_I,
                                 mfa,
                                 integer_to_binary_2,
                                 ERTS_SCHED_DIRTY_CPU,
                                 am_erlang,
                                 am_integer_to_binary,
                                 2);
    }

    return res;
}

#define ERTS_B2L_BYTES_PER_REDUCTION 256

typedef struct {
    Eterm res;
    Eterm *hp;
#ifdef DEBUG
    Eterm *hp_end;
#endif
    byte *bytes;
    Uint size;
    Uint bitoffs;
} ErtsB2LState;

static int b2l_state_destructor(Binary *mbp)
{
    ASSERT(ERTS_MAGIC_BIN_DESTRUCTOR(mbp) == b2l_state_destructor);
    return 1;
}

static BIF_RETTYPE
binary_to_list_chunk(Process *c_p,
		     Eterm mb_eterm,
		     ErtsB2LState* sp,
		     int reds_left,
		     int gc_disabled)
{
    BIF_RETTYPE ret;
    int bump_reds;
    Uint size;
    byte *bytes;

    size = (reds_left + 1)*ERTS_B2L_BYTES_PER_REDUCTION;
    if (size > sp->size)
	size = sp->size;
    bytes = sp->bytes + (sp->size - size);

    bump_reds = (size - 1)/ERTS_B2L_BYTES_PER_REDUCTION + 1;
    BUMP_REDS(c_p, bump_reds);

    ASSERT(is_list(sp->res) || is_nil(sp->res));

    sp->res = erts_bin_bytes_to_list(sp->res,
				     sp->hp,
				     bytes,
				     size,
				     sp->bitoffs);
    sp->size -= size;
    sp->hp += 2*size;

    if (sp->size > 0) {

	if (!gc_disabled)
	    erts_set_gc_state(c_p, 0);

	ASSERT(c_p->flags & F_DISABLE_GC);
	ASSERT(is_value(mb_eterm));
	ERTS_BIF_PREP_TRAP1(ret,
			    &binary_to_list_continue_export,
			    c_p,
			    mb_eterm);
    }
    else {

	ASSERT(sp->hp == sp->hp_end);
	ASSERT(sp->size == 0);

	if (!gc_disabled || !erts_set_gc_state(c_p, 1))
	    ERTS_BIF_PREP_RET(ret, sp->res);
	else
	    ERTS_BIF_PREP_YIELD_RETURN(ret, c_p, sp->res);
	ASSERT(!(c_p->flags & F_DISABLE_GC));
    }

    return ret;
}

static ERTS_INLINE BIF_RETTYPE
binary_to_list(Process *c_p, Eterm *hp, Eterm tail, byte *bytes,
	       Uint size, Uint bitoffs, int reds_left, int one_chunk)
{
    if (one_chunk) {
	Eterm res;
	BIF_RETTYPE ret;
	int bump_reds = (size - 1)/ERTS_B2L_BYTES_PER_REDUCTION + 1;
	BUMP_REDS(c_p, bump_reds);
	res = erts_bin_bytes_to_list(tail, hp, bytes, size, bitoffs);
	ERTS_BIF_PREP_RET(ret, res);
	return ret;
    }
    else {
	Binary *mbp = erts_create_magic_binary(sizeof(ErtsB2LState),
					       b2l_state_destructor);
	ErtsB2LState *sp = ERTS_MAGIC_BIN_DATA(mbp);
	Eterm mb;

	sp->res = tail;
	sp->hp = hp;
#ifdef DEBUG
	sp->hp_end = sp->hp + 2*size;
#endif
	sp->bytes = bytes;
	sp->size = size;
	sp->bitoffs = bitoffs;

	hp = HAlloc(c_p, ERTS_MAGIC_REF_THING_SIZE);
	mb = erts_mk_magic_ref(&hp, &MSO(c_p), mbp);
	return binary_to_list_chunk(c_p, mb, sp, reds_left, 0);
    }
}

static BIF_RETTYPE binary_to_list_continue(BIF_ALIST_1)
{
    Binary *mbp = erts_magic_ref2bin(BIF_ARG_1);

    ASSERT(ERTS_MAGIC_BIN_DESTRUCTOR(mbp) == b2l_state_destructor);
    ASSERT(BIF_P->flags & F_DISABLE_GC);

    return binary_to_list_chunk(BIF_P,
				BIF_ARG_1,
				(ErtsB2LState*) ERTS_MAGIC_BIN_DATA(mbp),
				ERTS_BIF_REDS_LEFT(BIF_P),
				1);
}

BIF_RETTYPE binary_to_list_1(BIF_ALIST_1)
{
    Uint offset, size;
    byte *base;
    int reds_left;
    int one_chunk;

    if (is_not_bitstring(BIF_ARG_1)) {
        BIF_ERROR(BIF_P, BADARG);
    }

    ERTS_GET_BITSTRING(BIF_ARG_1, base, offset, size);

    if (size == 0) {
        BIF_RET(NIL);
    } else if (TAIL_BITS(size) != 0) {
        BIF_ERROR(BIF_P, BADARG);
    }

    base = &base[BYTE_OFFSET(offset)];
    offset = BIT_OFFSET(offset);
    size = BYTE_SIZE(size);

    reds_left = ERTS_BIF_REDS_LEFT(BIF_P);
    one_chunk = size < reds_left*ERTS_B2L_BYTES_PER_REDUCTION;
    if (!one_chunk) {
        if (size < B2L_MIN_EXEC_REDS*ERTS_B2L_BYTES_PER_REDUCTION) {
            if (reds_left <= B2L_RESCHED_REDS) {
                /* Yield and do it with full context reds... */
                ERTS_BIF_YIELD1(BIF_TRAP_EXPORT(BIF_binary_to_list_1),
                                BIF_P, BIF_ARG_1);
            }
            /* Allow a bit more reductions... */
            one_chunk = 1;
            reds_left = B2L_MIN_EXEC_REDS;
        }
    }

    return binary_to_list(BIF_P,
                          HAlloc(BIF_P, 2 * size),
                          NIL,
                          base,
                          size,
                          offset,
                          reds_left,
                          one_chunk);
}

BIF_RETTYPE binary_to_list_3(BIF_ALIST_3)
{
    Uint start, stop, i;
    Uint offset, size;
    byte *base;
    int reds_left;
    int one_chunk;

    if (is_not_bitstring(BIF_ARG_1)) {
        BIF_ERROR(BIF_P, BADARG);
    }
    if (!term_to_Uint(BIF_ARG_2, &start) || !term_to_Uint(BIF_ARG_3, &stop)) {
        BIF_ERROR(BIF_P, BADARG);
    }

    ERTS_GET_BITSTRING(BIF_ARG_1, base, offset, size);

    if (TAIL_BITS(size) != 0) {
        BIF_ERROR(BIF_P, BADARG);
    }

    size = BYTE_SIZE(size);

    if (start < 1 || start > size || stop < 1 || stop > size || stop < start) {
        BIF_ERROR(BIF_P, BADARG);
    }

    if (size == 0) {
        BIF_RET(NIL);
    }

    reds_left = ERTS_BIF_REDS_LEFT(BIF_P);
    one_chunk = size < reds_left*ERTS_B2L_BYTES_PER_REDUCTION;
    if (!one_chunk) {
        if (size < B2L_MIN_EXEC_REDS*ERTS_B2L_BYTES_PER_REDUCTION) {
            if (reds_left <= B2L_RESCHED_REDS) {
                /* Yield and do it with full context reds... */
                ERTS_BIF_YIELD3(BIF_TRAP_EXPORT(BIF_binary_to_list_3),
                                BIF_P, BIF_ARG_1, BIF_ARG_2, BIF_ARG_3);
            }
            /* Allow a bit more reductions... */
            one_chunk = 1;
            reds_left = B2L_MIN_EXEC_REDS;
        }
    }

    i = stop - start + 1;

    base = &base[BYTE_OFFSET(offset)];
    return binary_to_list(BIF_P,
                          HAlloc(BIF_P, 2 * i),
                          NIL,
                          &base[start - 1],
                          i,
                          BIT_OFFSET(offset),
                          reds_left,
                          one_chunk);
}

BIF_RETTYPE bitstring_to_list_1(BIF_ALIST_1)
{
    Uint offset, size;
    byte* bytes;
    Eterm tail;
    Eterm* hp;
    int reds_left;
    int one_chunk;
    Uint cells;

    if (is_not_bitstring(BIF_ARG_1)) {
        BIF_ERROR(BIF_P, BADARG);
    }

    ERTS_GET_BITSTRING(BIF_ARG_1, bytes, offset, size);

    if (size == 0) {
        BIF_RET(NIL);
    }

    /* One cell per byte, plus one more for the trailing bits if any. */
    cells = NBYTES(size);

    reds_left = ERTS_BIF_REDS_LEFT(BIF_P);
    one_chunk = cells < (reds_left * ERTS_B2L_BYTES_PER_REDUCTION);
    if (!one_chunk) {
        if (cells < (B2L_MIN_EXEC_REDS * ERTS_B2L_BYTES_PER_REDUCTION)) {
            if (reds_left <= B2L_RESCHED_REDS) {
                /* Yield and do it with full context reds... */
                ERTS_BIF_YIELD1(BIF_TRAP_EXPORT(BIF_bitstring_to_list_1),
                                BIF_P, BIF_ARG_1);
            }

            /* Allow a bit more reductions... */
            one_chunk = 1;
            reds_left = B2L_MIN_EXEC_REDS;
        }
    }

    if (TAIL_BITS(size) == 0) {
        hp = HAlloc(BIF_P, 2 * cells);
        tail = NIL;
    } else {
        const Uint last_size = TAIL_BITS(size);
        Eterm last;

        hp = HAlloc(BIF_P, heap_bits_size(last_size) + 2 * cells);
        last = erts_build_sub_bitstring(&hp,
                                        0,
                                        NULL,
                                        bytes,
                                        offset + size - last_size,
                                        last_size);

        tail = CONS(hp, last, NIL);
        hp += 2;
    }

    return binary_to_list(BIF_P, hp, tail, &bytes[BYTE_OFFSET(offset)],
                          BYTE_SIZE(size), BIT_OFFSET(offset),
                          reds_left, one_chunk);
}

BIF_RETTYPE split_binary_2(BIF_ALIST_2)
{
    Uint split_at;

    if (is_bitstring(BIF_ARG_1) && term_to_Uint(BIF_ARG_2, &split_at)) {
        Uint offset, size;
        Eterm br_flags;
        BinRef *br;
        byte *base;

        ERTS_GET_BITSTRING_REF(BIF_ARG_1, br_flags, br, base, offset, size);

        if (split_at <= (size / 8)) {
            Eterm left, right;
            Uint heap_size;
            Eterm *hp;

            split_at *= 8;

            heap_size = erts_extracted_bitstring_size(split_at) +
                        erts_extracted_bitstring_size(size - split_at) +
                        3;

            hp = HAlloc(BIF_P, heap_size);
            left = erts_build_sub_bitstring(&hp,
                                            br_flags,
                                            br,
                                            base,
                                            offset,
                                            split_at);
            right = erts_build_sub_bitstring(&hp,
                                             br_flags,
                                             br,
                                             base,
                                             offset + split_at,
                                             size - split_at);
            return TUPLE2(hp, left, right);
        }
    }

    BIF_ERROR(BIF_P, BADARG);
}
