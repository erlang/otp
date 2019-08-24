/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2014-2017. All Rights Reserved.
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

#define ERL_BIF_UNIQUE_C__
#include "sys.h"
#include "erl_vm.h"
#include "erl_alloc.h"
#include "export.h"
#include "bif.h"
#include "erl_bif_unique.h"
#include "hash.h"
#include "erl_binary.h"

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * Reference                                                         *
\*                                                                   */

static union {
    erts_atomic64_t count;
    char align__[ERTS_CACHE_LINE_SIZE];
} global_reference erts_align_attribute(ERTS_CACHE_LINE_SIZE);


/*
 * ref[0] indicate thread creating reference as follows:
 *
 * - ref[0] == 0 => Non-scheduler thread;
 * - else; ref[0] <= erts_no_schedulers =>
 *      ordinary scheduler with id == ref[0];
 * - else; ref[0] <= erts_no_schedulers
 *                   + erts_no_dirty_cpu_schedulers =>
 *      dirty cpu scheduler with id == 'ref[0] - erts_no_schedulers';
 * - else =>
 *      dirty io scheduler with id == 'ref[0]
 *                                     - erts_no_schedulers
 *                                     - erts_no_dirty_cpu_schedulers'
 */

#ifdef DEBUG
static Uint32 max_thr_id;
#endif

static void init_magic_ref_tables(void);

static Uint64 ref_init_value;

static void
init_reference(void)
{
    SysTimeval tv;
    sys_gettimeofday(&tv);
    ref_init_value = 0;
    ref_init_value |= (Uint64) tv.tv_sec;
    ref_init_value |= ((Uint64) tv.tv_usec) << 32;
    ref_init_value *= (Uint64) 268438039;
    ref_init_value += (Uint64) tv.tv_usec;
#ifdef DEBUG
    max_thr_id = (Uint32) erts_no_schedulers;
    max_thr_id += (Uint32) erts_no_dirty_cpu_schedulers;
    max_thr_id += (Uint32) erts_no_dirty_io_schedulers;
#endif
    erts_atomic64_init_nob(&global_reference.count,
			   (erts_aint64_t) ref_init_value);
    init_magic_ref_tables();
}

static ERTS_INLINE void
global_make_ref_in_array(Uint32 thr_id, Uint32 ref[ERTS_REF_NUMBERS])
{
    Uint64 value;

    value = (Uint64) erts_atomic64_inc_read_mb(&global_reference.count);
    
    erts_set_ref_numbers(ref, thr_id, value);
}

static ERTS_INLINE void
make_ref_in_array(Uint32 ref[ERTS_REF_NUMBERS])
{
    ErtsSchedulerData *esdp = erts_get_scheduler_data();
    if (esdp)
	erts_sched_make_ref_in_array(esdp, ref);
    else
	global_make_ref_in_array(0, ref);
}

void
erts_make_ref_in_array(Uint32 ref[ERTS_REF_NUMBERS])
{
    make_ref_in_array(ref);
}

void
erts_make_magic_ref_in_array(Uint32 ref[ERTS_REF_NUMBERS])
{
    make_ref_in_array(ref);
    ASSERT(!(ref[1] & ERTS_REF1_MAGIC_MARKER_BIT__));
    ref[1] |= ERTS_REF1_MAGIC_MARKER_BIT__;
}

Eterm erts_make_ref_in_buffer(Eterm buffer[ERTS_REF_THING_SIZE])
{
    Eterm* hp = buffer;
    Uint32 ref[ERTS_REF_NUMBERS];

    make_ref_in_array(ref);
    write_ref_thing(hp, ref[0], ref[1], ref[2]);
    return make_internal_ref(hp);
}

Eterm erts_make_ref(Process *c_p)
{
    Eterm* hp;
    Uint32 ref[ERTS_REF_NUMBERS];

    ERTS_LC_ASSERT(ERTS_PROC_LOCK_MAIN & erts_proc_lc_my_proc_locks(c_p));

    hp = HAlloc(c_p, ERTS_REF_THING_SIZE);

    make_ref_in_array(ref);
    write_ref_thing(hp, ref[0], ref[1], ref[2]);

    return make_internal_ref(hp);
}

/*
 * Magic reference tables
 */

typedef struct {
    HashBucket hash;
    ErtsMagicBinary *mb;
    Uint64 value;
    Uint32 thr_id;
} ErtsMagicRefTableEntry;

typedef struct {
    erts_rwmtx_t rwmtx;
    Hash hash;
    char name[32];
} ErtsMagicRefTable;

typedef struct {
    union {
	ErtsMagicRefTable table;
	char align__[ERTS_ALC_CACHE_LINE_ALIGN_SIZE(sizeof(ErtsMagicRefTable))];
    } u;
} ErtsAlignedMagicRefTable;

ErtsAlignedMagicRefTable *magic_ref_table;

ErtsMagicBinary *
erts_magic_ref_lookup_bin__(Uint32 refn[ERTS_REF_NUMBERS])
{
    ErtsMagicRefTableEntry tmpl;
    ErtsMagicRefTableEntry *tep;
    ErtsMagicBinary *mb;
    ErtsMagicRefTable *tblp;

    ASSERT(erts_is_ref_numbers_magic(refn));

    tmpl.value = erts_get_ref_numbers_value(refn);
    tmpl.thr_id = erts_get_ref_numbers_thr_id(refn);
    if (tmpl.thr_id > erts_no_schedulers)
	tblp = &magic_ref_table[0].u.table;
    else
	tblp = &magic_ref_table[tmpl.thr_id].u.table;

    erts_rwmtx_rlock(&tblp->rwmtx);

    tep = (ErtsMagicRefTableEntry *) hash_get(&tblp->hash, &tmpl);
    if (!tep)
	mb = NULL;
    else {
	erts_aint_t refc;
	mb = tep->mb;
        refc = erts_refc_inc_unless(&mb->intern.refc, 0, 0);
        if (refc == 0)
            mb = NULL;
    }

    erts_rwmtx_runlock(&tblp->rwmtx);

    return mb;
}

void
erts_magic_ref_save_bin__(Eterm ref)
{
    ErtsMagicRefTableEntry tmpl;
    ErtsMagicRefTableEntry *tep;
    ErtsMRefThing *mrtp;
    ErtsMagicRefTable *tblp;
    Uint32 *refn;

    ASSERT(is_internal_magic_ref(ref));

    mrtp = (ErtsMRefThing *) internal_ref_val(ref);
    refn = mrtp->mb->refn;

    tmpl.value = erts_get_ref_numbers_value(refn);
    tmpl.thr_id = erts_get_ref_numbers_thr_id(refn);

    if (tmpl.thr_id > erts_no_schedulers)
	tblp = &magic_ref_table[0].u.table;
    else
	tblp = &magic_ref_table[tmpl.thr_id].u.table;

    erts_rwmtx_rlock(&tblp->rwmtx);

    tep = (ErtsMagicRefTableEntry *) hash_get(&tblp->hash, &tmpl);

    erts_rwmtx_runlock(&tblp->rwmtx);

    if (!tep) {
	ErtsMagicRefTableEntry *used_tep;
	
	ASSERT(tmpl.value == erts_get_ref_numbers_value(refn));
	ASSERT(tmpl.thr_id == erts_get_ref_numbers_thr_id(refn));

	if (tblp != &magic_ref_table[0].u.table) {
	    tep = erts_alloc(ERTS_ALC_T_MREF_NSCHED_ENT,
			     sizeof(ErtsNSchedMagicRefTableEntry));
	}
	else {
	    tep = erts_alloc(ERTS_ALC_T_MREF_ENT,
			     sizeof(ErtsMagicRefTableEntry));
	    tep->thr_id = tmpl.thr_id;
	}

	tep->value = tmpl.value;
	tep->mb = mrtp->mb;

	erts_rwmtx_rwlock(&tblp->rwmtx);

	used_tep = hash_put(&tblp->hash, tep);

	erts_rwmtx_rwunlock(&tblp->rwmtx);

	if (used_tep != tep) {
	    if (tblp != &magic_ref_table[0].u.table)
		erts_free(ERTS_ALC_T_MREF_NSCHED_ENT, (void *) tep);
	    else
		erts_free(ERTS_ALC_T_MREF_ENT, (void *) tep);
	}
    }
}

void
erts_magic_ref_remove_bin(Uint32 refn[ERTS_REF_NUMBERS])
{
    ErtsMagicRefTableEntry tmpl;
    ErtsMagicRefTableEntry *tep;
    ErtsMagicRefTable *tblp;

    tmpl.value = erts_get_ref_numbers_value(refn);
    tmpl.thr_id = erts_get_ref_numbers_thr_id(refn);

    if (tmpl.thr_id > erts_no_schedulers)
	tblp = &magic_ref_table[0].u.table;
    else
	tblp = &magic_ref_table[tmpl.thr_id].u.table;

    erts_rwmtx_rlock(&tblp->rwmtx);

    tep = (ErtsMagicRefTableEntry *) hash_get(&tblp->hash, &tmpl);

    erts_rwmtx_runlock(&tblp->rwmtx);

    if (tep) {

	ASSERT(tmpl.value == erts_get_ref_numbers_value(refn));
	ASSERT(tmpl.thr_id == erts_get_ref_numbers_thr_id(refn));

	erts_rwmtx_rwlock(&tblp->rwmtx);

	tep = hash_remove(&tblp->hash, &tmpl);
	ASSERT(tep);

	erts_rwmtx_rwunlock(&tblp->rwmtx);

	if (tblp != &magic_ref_table[0].u.table)
	    erts_free(ERTS_ALC_T_MREF_NSCHED_ENT, (void *) tep);
	else
	    erts_free(ERTS_ALC_T_MREF_ENT, (void *) tep);
    }
}

static int nsched_mreft_cmp(void *ve1, void *ve2)
{
    ErtsNSchedMagicRefTableEntry *e1 = ve1;
    ErtsNSchedMagicRefTableEntry *e2 = ve2;
    return e1->value != e2->value;
}

static int non_nsched_mreft_cmp(void *ve1, void *ve2)
{
    ErtsMagicRefTableEntry *e1 = ve1;
    ErtsMagicRefTableEntry *e2 = ve2;
    return e1->value != e2->value || e1->thr_id != e2->thr_id;
}

static HashValue nsched_mreft_hash(void *ve)
{
    ErtsNSchedMagicRefTableEntry *e = ve;
    return (HashValue) e->value;
}

static HashValue non_nsched_mreft_hash(void *ve)
{
    ErtsMagicRefTableEntry *e = ve;
    HashValue h;
    h = (HashValue) e->thr_id;
    h *= 268440163;
    h += (HashValue) e->value;
    return h;
}

static void *mreft_alloc(void *ve)
{
    /*
     * We allocate the element before
     * hash_put() and pass it as
     * template which we get as
     * input...
     */
    return ve;
}

static void mreft_free(void *ve)
{
    /*
     * We free the element ourselves
     * after hash_remove()...
     */
}

static void *mreft_meta_alloc(int i, size_t size)
{
    return erts_alloc(ERTS_ALC_T_MREF_TAB_BKTS, size);
}

static void mreft_meta_free(int i, void *ptr)
{
    erts_free(ERTS_ALC_T_MREF_TAB_BKTS, ptr);
}

static void
init_magic_ref_tables(void)
{
    HashFunctions hash_funcs;
    int i;
    ErtsMagicRefTable *tblp;

    magic_ref_table = erts_alloc_permanent_cache_aligned(ERTS_ALC_T_MREF_TAB,
							 (sizeof(ErtsAlignedMagicRefTable)
							  * (erts_no_schedulers + 1)));

    hash_funcs.hash = non_nsched_mreft_hash;
    hash_funcs.cmp = non_nsched_mreft_cmp;

    hash_funcs.alloc = mreft_alloc;
    hash_funcs.free = mreft_free;
    hash_funcs.meta_alloc = mreft_meta_alloc;
    hash_funcs.meta_free = mreft_meta_free;
    hash_funcs.meta_print = erts_print;

    tblp = &magic_ref_table[0].u.table;
    erts_snprintf(&tblp->name[0], sizeof(tblp->name),
		  "magic_ref_table_0");
    hash_init(0, &tblp->hash, &tblp->name[0], 1, hash_funcs);
    erts_rwmtx_init(&tblp->rwmtx, "magic_ref_table", NIL,
        ERTS_LOCK_FLAGS_PROPERTY_STATIC | ERTS_LOCK_FLAGS_CATEGORY_GENERIC);

    hash_funcs.hash = nsched_mreft_hash;
    hash_funcs.cmp = nsched_mreft_cmp;

    for (i = 1; i <= erts_no_schedulers; i++) {
	ErtsMagicRefTable *tblp = &magic_ref_table[i].u.table;
	erts_snprintf(&tblp->name[0], sizeof(tblp->name),
		      "magic_ref_table_%d", i);
	hash_init(0, &tblp->hash, &tblp->name[0], 1, hash_funcs);
	erts_rwmtx_init(&tblp->rwmtx, "magic_ref_table", NIL,
        ERTS_LOCK_FLAGS_PROPERTY_STATIC | ERTS_LOCK_FLAGS_CATEGORY_GENERIC);
    }
}

void erts_ref_bin_free(ErtsMagicBinary *mb)
{
    erts_bin_free((Binary *) mb);
}


/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * Unique Integer                                                    *
\*                                                                   */

static struct {
    union {
	struct {
	    int left_shift;
	    int right_shift;
	    Uint64 mask;
	    Uint64 val0_max;
	} o;
	char align__[ERTS_CACHE_LINE_SIZE];
    } r;
    union {
	erts_atomic64_t val1;
	char align__[ERTS_CACHE_LINE_SIZE];
    } w;
} unique_data erts_align_attribute(ERTS_CACHE_LINE_SIZE);

static void
init_unique_integer(void)
{
    int bits;
    unique_data.r.o.val0_max = (Uint64) erts_no_schedulers;
    unique_data.r.o.val0_max += (Uint64) erts_no_dirty_cpu_schedulers;
    unique_data.r.o.val0_max += (Uint64) erts_no_dirty_io_schedulers;
    bits = erts_fit_in_bits_int64(unique_data.r.o.val0_max);
    unique_data.r.o.left_shift = bits;
    unique_data.r.o.right_shift = 64 - bits;
    unique_data.r.o.mask = (((Uint64) 1) << bits) - 1;
    erts_atomic64_init_nob(&unique_data.w.val1, -1);
}

#define ERTS_MAX_UNIQUE_INT_HEAP_SIZE ERTS_UINT64_ARRAY_TO_BIG_MAX_HEAP_SZ(2)

static ERTS_INLINE Eterm
bld_unique_integer_term(Eterm **hpp, Uint *szp,
			Uint64 val0, Uint64 val1,
			int positive)
{
    Uint hsz;
    Uint64 unique_val[2];

    unique_val[0] = ((Uint64) val0);
    unique_val[0] |= ((Uint64) val1) << unique_data.r.o.left_shift;
    unique_val[1] = ((Uint64) val1) >> unique_data.r.o.right_shift;
    unique_val[1] &= unique_data.r.o.mask;

    if (positive) {
	unique_val[0]++;
	if (unique_val[0] == 0)
	    unique_val[1]++;
    }
    else {
	ASSERT(MIN_SMALL < 0);
	if (unique_val[1] == 0
	    && unique_val[0] < ((Uint64) -1*((Sint64) MIN_SMALL))) {
	    Sint64 s_unique_val = (Sint64) unique_val[0];
	    s_unique_val += MIN_SMALL;
	    ASSERT(MIN_SMALL <= s_unique_val && s_unique_val < 0);
	    if (szp)
		*szp = 0;
	    if (!hpp)
		return THE_NON_VALUE;
	    return make_small((Sint) s_unique_val);
	}
	if (unique_val[0] < ((Uint64) -1*((Sint64) MIN_SMALL))) {
	    ASSERT(unique_val[1] != 0);
	    unique_val[1] -= 1;
	}
	unique_val[0] += MIN_SMALL;
    }

    if (!unique_val[1]) {
	if (unique_val[0] <= MAX_SMALL) {
	    if (szp)
		*szp = 0;
	    if (!hpp)
		return THE_NON_VALUE;
	    return make_small((Uint) unique_val[0]);
	}

	if (szp)
	    *szp = ERTS_UINT64_HEAP_SIZE(unique_val[0]);
	if (!hpp)
	    return THE_NON_VALUE;
	return erts_uint64_to_big(unique_val[0], hpp);
    }
    else {
	Eterm tmp, *tmp_hp, res;
	DeclareTmpHeapNoproc(local_heap, 2*ERTS_MAX_UNIQUE_INT_HEAP_SIZE);

	UseTmpHeapNoproc(2*ERTS_MAX_UNIQUE_INT_HEAP_SIZE);

	tmp_hp = local_heap;

	tmp = erts_uint64_array_to_big(&tmp_hp, 0, 2, unique_val);
	ASSERT(is_big(tmp));

	hsz = big_arity(tmp) + 1;

	ASSERT(hsz <= ERTS_MAX_UNIQUE_INT_HEAP_SIZE);

	if (szp)
	    *szp = hsz;

	if (!hpp)
	    res = THE_NON_VALUE;
	else {
	    int hix;
	    Eterm *hp = *hpp;
	    tmp_hp = big_val(tmp);
	    for (hix = 0; hix < hsz; hix++)
		hp[hix] = tmp_hp[hix];

	    *hpp = hp + hsz;
	    res = make_big(hp);
	}

	UnUseTmpHeapNoproc(2*ERTS_MAX_UNIQUE_INT_HEAP_SIZE);

	return res;
    }
}

static ERTS_INLINE Eterm unique_integer_bif(Process *c_p, int positive)
{
    ErtsSchedulerData *esdp;
    Uint64 thr_id, unique;
    Uint hsz;
    Eterm *hp;

    esdp = erts_proc_sched_data(c_p);
    thr_id = (Uint64) esdp->thr_id;
    unique = esdp->unique++;
    bld_unique_integer_term(NULL, &hsz, thr_id, unique, positive);
    hp = hsz ? HAlloc(c_p, hsz) : NULL;
    return bld_unique_integer_term(&hp, NULL, thr_id, unique, positive);
}

Uint
erts_raw_unique_integer_heap_size(Uint64 val[ERTS_UNIQUE_INT_RAW_VALUES],
                                  int positive)
{
    Uint sz;
    bld_unique_integer_term(NULL, &sz, val[0], val[1], positive);
    return sz;
}

Eterm
erts_raw_make_unique_integer(Eterm **hpp, Uint64 val[ERTS_UNIQUE_INT_RAW_VALUES],
    int positive)
{
    return bld_unique_integer_term(hpp, NULL, val[0], val[1], positive);
}

void
erts_raw_get_unique_integer(Uint64 val[ERTS_UNIQUE_INT_RAW_VALUES])
{
    ErtsSchedulerData *esdp = erts_get_scheduler_data();
    if (esdp) {
	val[0] = (Uint64) esdp->thr_id;
	val[1] = esdp->unique++;
    }
    else {
	val[0] = (Uint64) 0;
	val[1] = (Uint64) erts_atomic64_inc_read_nob(&unique_data.w.val1);
    }
}


Sint64
erts_get_min_unique_integer(void)
{
    return (Sint64) MIN_SMALL;
}

/* --- Debug --- */

Eterm
erts_debug_make_unique_integer(Process *c_p, Eterm etval0, Eterm etval1)
{
    Uint64 val0, val1;
    Uint hsz;
    Eterm res, *hp, *end_hp;

    if (!term_to_Uint64(etval0, &val0))
	return THE_NON_VALUE;

    if (!term_to_Uint64(etval1, &val1))
	return THE_NON_VALUE;

    bld_unique_integer_term(NULL, &hsz, val0, val1, 0);

    hp = HAlloc(c_p, hsz);
    end_hp = hp + hsz;

    res = bld_unique_integer_term(&hp, NULL, val0, val1, 0);
    if (hp != end_hp)
	ERTS_INTERNAL_ERROR("Heap allocation error");

    return res;
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * Strict Monotonic Counter                                          *
\*                                                                   */

static struct {
    union {
	erts_atomic64_t value;
	char align__[ERTS_CACHE_LINE_SIZE];
    } w;
} raw_unique_monotonic_integer erts_align_attribute(ERTS_CACHE_LINE_SIZE);

#if defined(ARCH_32)
#  define ERTS_UNIQUE_MONOTONIC_OFFSET ERTS_SINT64_MIN
#else
#  define ERTS_UNIQUE_MONOTONIC_OFFSET MIN_SMALL
#endif

static void
init_unique_monotonic_integer(void)
{
    erts_atomic64_init_nob(&raw_unique_monotonic_integer.w.value,
			   (erts_aint64_t) -1);
}

static ERTS_INLINE Uint64
get_raw_unique_monotonic_integer(void)
{
    return (Uint64) erts_atomic64_inc_read_mb(&raw_unique_monotonic_integer.w.value);
}

static ERTS_INLINE Uint
get_unique_monotonic_integer_heap_size(Uint64 raw, int positive)
{
    if (positive) {
	Uint64 value = raw+1;
	return ERTS_UINT64_HEAP_SIZE(value);
    }
    else {
	Sint64 value = ((Sint64) raw) + ERTS_UNIQUE_MONOTONIC_OFFSET;
	if (IS_SSMALL(value))
	    return 0;
#if defined(ARCH_32)
	return ERTS_SINT64_HEAP_SIZE(value);
#else
	return ERTS_UINT64_HEAP_SIZE((Uint64) value);
#endif
    }
}

static ERTS_INLINE Eterm
make_unique_monotonic_integer_value(Eterm *hp, Uint hsz, Uint64 raw, int positive)
{
    Eterm res;
#ifdef DEBUG
    Eterm *end_hp = hp + hsz;
#endif

    if (positive) {
	Uint64 value = raw+1;
	res = hsz ? erts_uint64_to_big(value, &hp) : make_small(value);
    }
    else {
	Sint64 value = ((Sint64) raw) + ERTS_UNIQUE_MONOTONIC_OFFSET;
	if (hsz == 0)
	    res = make_small(value);
	else {
#if defined(ARCH_32)
	    res = erts_sint64_to_big(value, &hp);
#else 
	    res = erts_uint64_to_big((Uint64) value, &hp);
#endif
	}
    }

    ASSERT(end_hp == hp);

    return res;
}

static ERTS_INLINE Eterm
unique_monotonic_integer_bif(Process *c_p, int positive)
{
    Uint64 raw;
    Uint hsz;
    Eterm *hp;

    raw = get_raw_unique_monotonic_integer();
    hsz = get_unique_monotonic_integer_heap_size(raw, positive);
    hp = hsz ? HAlloc(c_p, hsz) : NULL;
    return make_unique_monotonic_integer_value(hp, hsz, raw, positive);
}

Sint64
erts_raw_get_unique_monotonic_integer(void)
{
    return get_raw_unique_monotonic_integer();
}

Uint
erts_raw_unique_monotonic_integer_heap_size(Sint64 raw, int positive)
{
    return get_unique_monotonic_integer_heap_size(raw, positive);
}

Eterm
erts_raw_make_unique_monotonic_integer_value(Eterm **hpp, Sint64 raw, int positive)
{
    Uint hsz = get_unique_monotonic_integer_heap_size(raw, positive);
    Eterm res = make_unique_monotonic_integer_value(*hpp, hsz, raw, positive);
    *hpp += hsz;
    return res;
}

Sint64
erts_get_min_unique_monotonic_integer(void)
{
    return ERTS_UNIQUE_MONOTONIC_OFFSET;
}

/* --- Debug --- */

int
erts_debug_set_unique_monotonic_integer_state(Eterm et_value)
{
    Sint64 value;

    if (!term_to_Sint64(et_value, &value)) {
	Uint64 uvalue;
	if (!term_to_Uint64(et_value, &uvalue))
	    return 0;
	value = (Sint64) uvalue;
    }

    erts_atomic64_set_mb(&raw_unique_monotonic_integer.w.value,
			 (erts_aint64_t) value);
    return 1;
}

Eterm
erts_debug_get_unique_monotonic_integer_state(Process *c_p)
{
    Uint64 value;
    Eterm hsz, *hp;

    value = (Uint64) erts_atomic64_read_mb(&raw_unique_monotonic_integer.w.value);

    if (IS_USMALL(0, value))
	return make_small(value);
    hsz = ERTS_UINT64_HEAP_SIZE(value);
    hp = HAlloc(c_p, hsz);
    return erts_uint64_to_big(value, &hp);
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * Initilazation                                                     *
\*                                                                   */

void
erts_bif_unique_init(void)
{
    init_reference();
    init_unique_monotonic_integer();
    init_unique_integer();
}

void
erts_sched_bif_unique_init(ErtsSchedulerData *esdp)
{
    esdp->unique = (Uint64) 0;
    esdp->ref = (Uint64) ref_init_value;
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * The BIFs                                                          *
\*                                                                   */


BIF_RETTYPE make_ref_0(BIF_ALIST_0)
{
    BIF_RETTYPE res;
    Eterm* hp;

    ERTS_LC_ASSERT(ERTS_PROC_LOCK_MAIN & erts_proc_lc_my_proc_locks(BIF_P));

    hp = HAlloc(BIF_P, ERTS_REF_THING_SIZE);

    res = erts_sched_make_ref_in_buffer(erts_proc_sched_data(BIF_P), hp);

    BIF_RET(res);
}

BIF_RETTYPE unique_integer_0(BIF_ALIST_0)
{
    BIF_RET(unique_integer_bif(BIF_P, 0));
}

BIF_RETTYPE unique_integer_1(BIF_ALIST_1)
{
    Eterm modlist = BIF_ARG_1;
    int monotonic = 0;
    int positive = 0;
    BIF_RETTYPE res;

    while (is_list(modlist)) {
	Eterm *consp = list_val(modlist);
	switch (CAR(consp)) {
	case am_monotonic:
	    monotonic = 1;
	    break;
	case am_positive:
	    positive = 1;
	    break;
	default:
	    BIF_ERROR(BIF_P, BADARG);
	}
	modlist = CDR(consp);
    }

    if (is_not_nil(modlist))
	BIF_ERROR(BIF_P, BADARG);

    if (monotonic)
	res = unique_monotonic_integer_bif(BIF_P, positive);
    else
	res = unique_integer_bif(BIF_P, positive);

    BIF_RET(res);
}
