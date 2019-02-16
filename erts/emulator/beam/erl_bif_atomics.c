/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2018. All Rights Reserved.
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
 * Purpose:  High performance atomics.
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include <stddef.h> /* offsetof */

#include "sys.h"
#include "export.h"
#include "bif.h"
#include "erl_threads.h"
#include "big.h"
#include "erl_binary.h"
#include "erl_bif_unique.h"
#include "erl_map.h"

typedef struct
{
    int is_signed;
    UWord vlen;
    erts_atomic64_t v[1];
}AtomicsRef;

static int atomics_destructor(Binary *unused)
{
    return 1;
}

#define OPT_SIGNED (1 << 0)

BIF_RETTYPE erts_internal_atomics_new_2(BIF_ALIST_2)
{
    AtomicsRef* p;
    Binary* mbin;
    UWord i, cnt, opts;
    Uint bytes;
    Eterm* hp;

    if (!term_to_UWord(BIF_ARG_1, &cnt)
        || cnt == 0
        || !term_to_UWord(BIF_ARG_2, &opts)) {

        BIF_ERROR(BIF_P, BADARG);
    }

    if (cnt > (ERTS_UWORD_MAX / sizeof(p->v[0])))
        BIF_ERROR(BIF_P, SYSTEM_LIMIT);

    bytes = offsetof(AtomicsRef, v) + cnt*sizeof(p->v[0]);
    mbin = erts_create_magic_binary_x(bytes,
                                      atomics_destructor,
                                      ERTS_ALC_T_ATOMICS,
                                      0);
    p = ERTS_MAGIC_BIN_DATA(mbin);
    p->is_signed = opts & OPT_SIGNED;
    p->vlen = cnt;
    for (i=0; i < cnt; i++)
        erts_atomic64_init_nob(&p->v[i], 0);
    hp = HAlloc(BIF_P, ERTS_MAGIC_REF_THING_SIZE);
    return erts_mk_magic_ref(&hp, &MSO(BIF_P), mbin);
}

static ERTS_INLINE int get_ref(Eterm ref, AtomicsRef** pp)
{
    Binary* mbin;
    if (!is_internal_magic_ref(ref))
        return 0;

    mbin = erts_magic_ref2bin(ref);
    if (ERTS_MAGIC_BIN_DESTRUCTOR(mbin) != atomics_destructor)
        return 0;
    *pp = ERTS_MAGIC_BIN_DATA(mbin);
    return 1;
}

static ERTS_INLINE int get_ref_ix(Eterm ref, Eterm ix,
                                  AtomicsRef** pp, UWord* ixp)
{
    return (get_ref(ref, pp)
            && term_to_UWord(ix, ixp)
            && --(*ixp) < (*pp)->vlen);
}

static ERTS_INLINE int get_value(AtomicsRef* p, Eterm term, erts_aint64_t *valp)
{
    return (p->is_signed ?
            term_to_Sint64(term, (Sint64*)valp) :
            term_to_Uint64(term, (Uint64*)valp));
}

static ERTS_INLINE int get_incr(AtomicsRef* p, Eterm term, erts_aint64_t *valp)
{
    return (term_to_Sint64(term, (Sint64*)valp)
            || term_to_Uint64(term, (Uint64*)valp));
}

static ERTS_INLINE Eterm bld_atomic(Process* proc, AtomicsRef* p,
                                    erts_aint64_t val)
{
    if (p->is_signed) {
        if (IS_SSMALL(val))
            return make_small((Sint) val);
        else {
            Uint hsz = ERTS_SINT64_HEAP_SIZE(val);
            Eterm* hp = HAlloc(proc, hsz);
            return erts_sint64_to_big(val, &hp);
        }
    }
    else {
        if ((Uint64)val <= MAX_SMALL)
            return make_small((Sint) val);
        else {
            Uint hsz = ERTS_UINT64_HEAP_SIZE((Uint64)val);
            Eterm* hp = HAlloc(proc, hsz);
            return erts_uint64_to_big(val, &hp);
        }
    }
}

BIF_RETTYPE atomics_put_3(BIF_ALIST_3)
{
    AtomicsRef* p;
    UWord ix;
    erts_aint64_t val;

    if (!get_ref_ix(BIF_ARG_1, BIF_ARG_2, &p, &ix)
        || !get_value(p, BIF_ARG_3, &val)) {
        BIF_ERROR(BIF_P, BADARG);
    }
    erts_atomic64_set_mb(&p->v[ix], val);
    return am_ok;
}

BIF_RETTYPE atomics_get_2(BIF_ALIST_2)
{
    AtomicsRef* p;
    UWord ix;

    if (!get_ref_ix(BIF_ARG_1, BIF_ARG_2, &p, &ix)) {
        BIF_ERROR(BIF_P, BADARG);
    }
    return bld_atomic(BIF_P, p, erts_atomic64_read_mb(&p->v[ix]));
}

BIF_RETTYPE atomics_add_3(BIF_ALIST_3)
{
    AtomicsRef* p;
    UWord ix;
    erts_aint64_t incr;

    if (!get_ref_ix(BIF_ARG_1, BIF_ARG_2, &p, &ix)
        || !get_incr(p, BIF_ARG_3, &incr)) {
        BIF_ERROR(BIF_P, BADARG);
    }
    erts_atomic64_add_mb(&p->v[ix], incr);
    return am_ok;
}

BIF_RETTYPE atomics_add_get_3(BIF_ALIST_3)
{
    AtomicsRef* p;
    UWord ix;
    erts_aint64_t incr;

    if (!get_ref_ix(BIF_ARG_1, BIF_ARG_2, &p, &ix)
        || !get_incr(p, BIF_ARG_3, &incr)) {
        BIF_ERROR(BIF_P, BADARG);
    }
    return bld_atomic(BIF_P, p, erts_atomic64_add_read_mb(&p->v[ix], incr));
}

BIF_RETTYPE atomics_exchange_3(BIF_ALIST_3)
{
    AtomicsRef* p;
    UWord ix;
    erts_aint64_t desired, was;

    if (!get_ref_ix(BIF_ARG_1, BIF_ARG_2, &p, &ix)
        || !get_value(p, BIF_ARG_3, &desired)) {
        BIF_ERROR(BIF_P, BADARG);
    }
    was = erts_atomic64_xchg_mb(&p->v[ix], desired);
    return bld_atomic(BIF_P, p, was);
}

BIF_RETTYPE atomics_compare_exchange_4(BIF_ALIST_4)
{
    AtomicsRef* p;
    UWord ix;
    erts_aint64_t expected, desired, was;

    if (!get_ref_ix(BIF_ARG_1, BIF_ARG_2, &p, &ix)
        || !get_value(p, BIF_ARG_3, &expected)
        || !get_value(p, BIF_ARG_4, &desired)) {
        BIF_ERROR(BIF_P, BADARG);
    }
    was = erts_atomic64_cmpxchg_mb(&p->v[ix], desired, expected);
    return was == expected ? am_ok : bld_atomic(BIF_P, p, was);
}

BIF_RETTYPE atomics_info_1(BIF_ALIST_1)
{
    AtomicsRef* p;
    Uint hsz = MAP4_SZ;
    Eterm *hp;
    Uint64 max;
    Sint64 min;
    UWord memory;
    Eterm max_val, min_val, sz_val, mem_val;

    if (!get_ref(BIF_ARG_1, &p))
        BIF_ERROR(BIF_P, BADARG);

    max = p->is_signed ? ERTS_SINT64_MAX : ERTS_UINT64_MAX;
    min = p->is_signed ? ERTS_SINT64_MIN : 0;
    memory = erts_magic_ref2bin(BIF_ARG_1)->orig_size;

    erts_bld_uint64(NULL, &hsz, max);
    erts_bld_sint64(NULL, &hsz, min);
    erts_bld_uword(NULL, &hsz, p->vlen);
    erts_bld_uword(NULL, &hsz, memory);

    hp = HAlloc(BIF_P, hsz);
    max_val = erts_bld_uint64(&hp, NULL, max);
    min_val = erts_bld_sint64(&hp, NULL, min);
    sz_val  = erts_bld_uword(&hp, NULL, p->vlen);
    mem_val = erts_bld_uword(&hp, NULL, memory);

    return MAP4(hp, am_max, max_val,
                am_memory, mem_val,
                am_min, min_val,
                am_size, sz_val);
}
