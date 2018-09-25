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


#define COUNTERS_PER_CACHE_LINE (ERTS_CACHE_LINE_SIZE / sizeof(erts_atomic64_t))

typedef struct
{
    UWord arity;
#ifdef DEBUG
    UWord ulen;
#endif
    union {
        erts_atomic64_t v[COUNTERS_PER_CACHE_LINE];
        byte cache_line__[ERTS_CACHE_LINE_SIZE];
    } u[1];
}CountersRef;

static int counters_destructor(Binary *unused)
{
    return 1;
}


static UWord ERTS_INLINE div_ceil(UWord dividend, UWord divisor)
{
    return (dividend + divisor - 1) / divisor;
}

BIF_RETTYPE erts_internal_counters_new_1(BIF_ALIST_1)
{
    CountersRef* p;
    Binary* mbin;
    UWord ui, vi, cnt;
    Uint bytes, cache_lines;
    Eterm* hp;

    if (!term_to_UWord(BIF_ARG_1, &cnt)
        || cnt == 0) {
        BIF_ERROR(BIF_P, BADARG);
    }

    if (cnt > (ERTS_UWORD_MAX / (sizeof(erts_atomic64_t)*2*erts_no_schedulers)))
        BIF_ERROR(BIF_P, SYSTEM_LIMIT);

    cache_lines = erts_no_schedulers * div_ceil(cnt, COUNTERS_PER_CACHE_LINE);
    bytes = offsetof(CountersRef, u) + cache_lines * ERTS_CACHE_LINE_SIZE;
    mbin = erts_create_magic_binary_x(bytes,
                                      counters_destructor,
                                      ERTS_ALC_T_ATOMICS,
                                      0);
    p = ERTS_MAGIC_BIN_DATA(mbin);
    p->arity = cnt;
#ifdef DEBUG
    p->ulen = cache_lines;
#endif
    ASSERT((byte*)&p->u[cache_lines] <= ((byte*)p + bytes));
    for (ui=0; ui < cache_lines; ui++)
        for (vi=0; vi < COUNTERS_PER_CACHE_LINE; vi++)
            erts_atomic64_init_nob(&p->u[ui].v[vi], 0);
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
    *pp = ERTS_MAGIC_BIN_DATA(mbin);
    return 1;
}

static ERTS_INLINE int get_ref_cnt(Eterm ref, Eterm index,
                                   CountersRef** pp,
                                   erts_atomic64_t** app,
                                   UWord sched_ix)
{
    CountersRef* p;
    UWord ix, ui, vi;
    if (!get_ref(ref, &p) || !term_to_UWord(index, &ix) || --ix >= p->arity)
        return 0;
    ui = (ix / COUNTERS_PER_CACHE_LINE) * erts_no_schedulers + sched_ix;
    vi = ix % COUNTERS_PER_CACHE_LINE;
    ASSERT(ui < p->ulen);
    *pp = p;
    *app = &p->u[ui].v[vi];
    return 1;
}

static ERTS_INLINE int get_ref_my_cnt(Eterm ref, Eterm index,
                                      CountersRef** pp,
                                      erts_atomic64_t** app)
{
    ErtsSchedulerData *esdp = erts_get_scheduler_data();
    ASSERT(esdp && !ERTS_SCHEDULER_IS_DIRTY(esdp));
    return get_ref_cnt(ref, index, pp, app, esdp->no - 1);
}

static ERTS_INLINE int get_ref_first_cnt(Eterm ref, Eterm index,
                                         CountersRef** pp,
                                         erts_atomic64_t** app)
{
    return get_ref_cnt(ref, index, pp, app, 0);
}

static ERTS_INLINE int get_incr(CountersRef* p, Eterm term, erts_aint64_t *valp)
{
    return (term_to_Sint64(term, (Sint64*)valp)
            || term_to_Uint64(term, (Uint64*)valp));
}

static ERTS_INLINE Eterm bld_counter(Process* proc, CountersRef* p,
                                     erts_aint64_t val)
{
    if (IS_SSMALL(val))
        return make_small((Sint) val);
    else {
        Uint hsz = ERTS_SINT64_HEAP_SIZE(val);
        Eterm* hp = HAlloc(proc, hsz);
        return erts_sint64_to_big(val, &hp);
    }
}

BIF_RETTYPE erts_internal_counters_get_2(BIF_ALIST_2)
{
    CountersRef* p;
    erts_atomic64_t* ap;
    erts_aint64_t acc = 0;
    int j;

    if (!get_ref_first_cnt(BIF_ARG_1, BIF_ARG_2, &p, &ap)) {
        BIF_ERROR(BIF_P, BADARG);
    }
    for (j = erts_no_schedulers; j ; --j) {
        acc += erts_atomic64_read_nob(ap);
        ap = (erts_atomic64_t*) ((byte*)ap + ERTS_CACHE_LINE_SIZE);
    }
    return bld_counter(BIF_P, p, acc);
}

BIF_RETTYPE erts_internal_counters_add_3(BIF_ALIST_3)
{
    CountersRef* p;
    erts_atomic64_t* ap;
    erts_aint64_t incr, sum;

    if (!get_ref_my_cnt(BIF_ARG_1, BIF_ARG_2, &p, &ap)
        || !get_incr(p, BIF_ARG_3, &incr)) {
        BIF_ERROR(BIF_P, BADARG);
    }
    sum = incr + erts_atomic64_read_nob(ap);
    erts_atomic64_set_nob(ap, sum);
    return am_ok;
}


BIF_RETTYPE erts_internal_counters_info_1(BIF_ALIST_1)
{
    CountersRef* p;
    Uint hsz = MAP2_SZ;
    Eterm *hp;
    UWord memory;
    Eterm sz_val, mem_val;

    if (!get_ref(BIF_ARG_1, &p))
        BIF_ERROR(BIF_P, BADARG);

    memory = erts_magic_ref2bin(BIF_ARG_1)->orig_size;
    erts_bld_uword(NULL, &hsz, p->arity);
    erts_bld_uword(NULL, &hsz, memory);

    hp = HAlloc(BIF_P, hsz);
    sz_val  = erts_bld_uword(&hp, NULL, p->arity);
    mem_val = erts_bld_uword(&hp, NULL, memory);

    return MAP2(hp, am_memory, mem_val,
                am_size, sz_val);
}
