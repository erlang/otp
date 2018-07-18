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
 * Purpose:  High performance counters.
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

typedef struct
{
    UWord vlen;
    erts_atomic_t v[1];
}CountersRef;

static int counters_destructor(Binary *unused)
{
    return 1;
}

BIF_RETTYPE counters_new_2(BIF_ALIST_2)
{
    CountersRef* p;
    Binary* mbin;
    UWord i, cnt;
    Uint bytes;
    Eterm* hp;

    if (!term_to_UWord(BIF_ARG_1, &cnt)
        || cnt == 0
        || BIF_ARG_2 != NIL) {

        BIF_ERROR(BIF_P, BADARG);
    }

    if (cnt > (ERTS_UWORD_MAX / sizeof(p->v[0])))
        BIF_ERROR(BIF_P, SYSTEM_LIMIT);

    bytes = offsetof(CountersRef, v) + cnt*sizeof(p->v[0]),
    mbin = erts_create_magic_binary_x(bytes,
                                      counters_destructor,
                                      ERTS_ALC_T_COUNTERS,
                                      1);
    p = ERTS_MAGIC_BIN_UNALIGNED_DATA(mbin);
    p->vlen = cnt;
    for (i=0; i < cnt; i++) {
        erts_atomic_init_nob(&p->v[i], 0);
    }
    hp = HAlloc(BIF_P, ERTS_MAGIC_REF_THING_SIZE);
    return erts_mk_magic_ref(&hp, &MSO(BIF_P), mbin);
}

static ERTS_INLINE int get_ref(Eterm ref, CountersRef** pp)
{
    Binary* mbin;
    if (!is_internal_magic_ref(ref))
        return 0;

    mbin = erts_magic_ref2bin(ref);
    if (ERTS_MAGIC_BIN_DESTRUCTOR(mbin) != counters_destructor)
        return 0;
    *pp = ERTS_MAGIC_BIN_UNALIGNED_DATA(mbin);
    return 1;
}

static ERTS_INLINE int get_ref_ix(Eterm ref, Eterm ix,
                                  CountersRef** pp, UWord* ixp)
{
    return (get_ref(ref, pp)
            && term_to_UWord(ix, ixp)
            && *ixp < (*pp)->vlen);
}

static ERTS_INLINE Eterm bld_counter(Process* p, SWord val)
{
    if (IS_SSMALL(val))
        return make_small((Sint) val);
    else {
        Eterm* hp = HAlloc(p, BIG_UWORD_HEAP_SIZE(si64));
        return small_to_big(val, hp);
    }
}

BIF_RETTYPE counters_put_3(BIF_ALIST_3)
{
    CountersRef* p;
    UWord ix;
    SWord val;

    if (!get_ref_ix(BIF_ARG_1, BIF_ARG_2, &p, &ix)
        || !term_to_Sint(BIF_ARG_3, &val)) {
        BIF_ERROR(BIF_P, BADARG);
    }
    erts_atomic_set_mb(&p->v[ix], val);
    return am_ok;
}

BIF_RETTYPE counters_get_2(BIF_ALIST_2)
{
    CountersRef* p;
    UWord ix;

    if (!get_ref_ix(BIF_ARG_1, BIF_ARG_2, &p, &ix)) {
        BIF_ERROR(BIF_P, BADARG);
    }
    return bld_counter(BIF_P, erts_atomic_read_mb(&p->v[ix]));
}

BIF_RETTYPE counters_add_3(BIF_ALIST_3)
{
    CountersRef* p;
    UWord ix;
    SWord incr;

    if (!get_ref_ix(BIF_ARG_1, BIF_ARG_2, &p, &ix)
        || !term_to_Sint(BIF_ARG_3, &incr)) {
        BIF_ERROR(BIF_P, BADARG);
    }
    erts_atomic_add_mb(&p->v[ix], incr);
    return am_ok;
}

BIF_RETTYPE counters_add_get_3(BIF_ALIST_3)
{
    CountersRef* p;
    UWord ix;
    SWord incr;

    if (!get_ref_ix(BIF_ARG_1, BIF_ARG_2, &p, &ix)
        || !term_to_Sint(BIF_ARG_3, &incr)) {
        BIF_ERROR(BIF_P, BADARG);
    }
    return bld_counter(BIF_P, erts_atomic_add_read_mb(&p->v[ix], incr));
}

BIF_RETTYPE counters_size_1(BIF_ALIST_1)
{
    CountersRef* p;

    if (!get_ref(BIF_ARG_1, &p))
        BIF_ERROR(BIF_P, BADARG);
    return bld_counter(BIF_P, p->vlen);
}

BIF_RETTYPE counters_max_1(BIF_ALIST_1)
{
    CountersRef* p;

    if (!get_ref(BIF_ARG_1, &p))
        BIF_ERROR(BIF_P, BADARG);
    return bld_counter(BIF_P, ERTS_SWORD_MAX);
}

BIF_RETTYPE counters_min_1(BIF_ALIST_1)
{
    CountersRef* p;

    if (!get_ref(BIF_ARG_1, &p))
        BIF_ERROR(BIF_P, BADARG);
    return bld_counter(BIF_P, ERTS_SWORD_MIN);
}
