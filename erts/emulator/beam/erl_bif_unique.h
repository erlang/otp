/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2014-2016. All Rights Reserved.
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

#ifndef ERTS_BIF_UNIQUE_H__
#define ERTS_BIF_UNIQUE_H__

#include "erl_term.h"
#include "erl_process.h"
#include "big.h"
#define ERTS_BINARY_TYPES_ONLY__
#include "erl_binary.h"
#undef ERTS_BINARY_TYPES_ONLY__

void erts_bif_unique_init(void);
void erts_sched_bif_unique_init(ErtsSchedulerData *esdp);

/* reference */
Eterm erts_make_ref(Process *);
Eterm erts_make_ref_in_buffer(Eterm buffer[ERTS_REF_THING_SIZE]);
void erts_make_ref_in_array(Uint32 ref[ERTS_REF_NUMBERS]);
void erts_make_magic_ref_in_array(Uint32 ref[ERTS_REF_NUMBERS]);
void erts_magic_ref_remove_bin(Uint32 refn[ERTS_REF_NUMBERS]);
void erts_magic_ref_save_bin__(Eterm ref);
ErtsMagicBinary *erts_magic_ref_lookup_bin__(Uint32 refn[ERTS_REF_NUMBERS]);


/* strict monotonic counter */

#define ERTS_MAX_UNIQUE_MONOTONIC_INTEGER_HEAP_SIZE ERTS_MAX_UINT64_HEAP_SIZE

/*
 * Note that a raw value is an intermediate value that 
 * not necessarily correspond to the end result.
 */
Sint64 erts_raw_get_unique_monotonic_integer(void);
Uint erts_raw_unique_monotonic_integer_heap_size(Sint64 raw, int positive);
Eterm erts_raw_make_unique_monotonic_integer_value(Eterm **hpp, Sint64 raw,
                                                   int positive);

Sint64 erts_get_min_unique_monotonic_integer(void);

int erts_debug_set_unique_monotonic_integer_state(Eterm et_value);
Eterm erts_debug_get_unique_monotonic_integer_state(Process *c_p);

/* unique integer */
#define ERTS_UNIQUE_INT_RAW_VALUES 2
#define ERTS_MAX_UNIQUE_INT_HEAP_SIZE ERTS_UINT64_ARRAY_TO_BIG_MAX_HEAP_SZ(2)

Uint erts_raw_unique_integer_heap_size(Uint64 val[ERTS_UNIQUE_INT_RAW_VALUES],
                                       int positive);
Eterm erts_raw_make_unique_integer(Eterm **hpp,
                                   Uint64 val[ERTS_UNIQUE_INT_RAW_VALUES],
                                   int postive);
void erts_raw_get_unique_integer(Uint64 val[ERTS_UNIQUE_INT_RAW_VALUES]);
Sint64 erts_get_min_unique_integer(void);

Eterm erts_debug_make_unique_integer(Process *c_p,
				     Eterm etval0,
				     Eterm etval1);


ERTS_GLB_INLINE void erts_set_ref_numbers(Uint32 ref[ERTS_REF_NUMBERS],
					  Uint32 thr_id, Uint64 value);
ERTS_GLB_INLINE Uint32 erts_get_ref_numbers_thr_id(Uint32 ref[ERTS_REF_NUMBERS]);
ERTS_GLB_INLINE int erts_is_ref_numbers_magic(Uint32 ref[ERTS_REF_NUMBERS]);
ERTS_GLB_INLINE Uint64 erts_get_ref_numbers_value(Uint32 ref[ERTS_REF_NUMBERS]);
ERTS_GLB_INLINE void erts_sched_make_ref_in_array(ErtsSchedulerData *esdp,
						  Uint32 ref[ERTS_REF_NUMBERS]);
ERTS_GLB_INLINE void erts_sched_make_magic_ref_in_array(ErtsSchedulerData *esdp,
							Uint32 ref[ERTS_REF_NUMBERS]);
ERTS_GLB_INLINE Eterm erts_sched_make_ref_in_buffer(ErtsSchedulerData *esdp,
						    Eterm buffer[ERTS_REF_THING_SIZE]);
ERTS_GLB_INLINE Eterm erts_mk_magic_ref(Eterm **hpp, ErlOffHeap *ohp, Binary *mbp);
ERTS_GLB_INLINE Binary *erts_magic_ref2bin(Eterm mref);
ERTS_GLB_INLINE void erts_magic_ref_save_bin(Eterm ref);
ERTS_GLB_INLINE ErtsMagicBinary *erts_magic_ref_lookup_bin(Uint32 ref[ERTS_REF_NUMBERS]);

#define ERTS_REF1_MAGIC_MARKER_BIT_NO__ \
    (_REF_NUM_SIZE-1)
#define ERTS_REF1_MAGIC_MARKER_BIT__ \
    (((Uint32) 1) << ERTS_REF1_MAGIC_MARKER_BIT_NO__)
#define ERTS_REF1_THR_ID_MASK__ \
    (ERTS_REF1_MAGIC_MARKER_BIT__-1)
#define ERTS_REF1_NUM_MASK__ \
    (~(ERTS_REF1_THR_ID_MASK__|ERTS_REF1_MAGIC_MARKER_BIT__))

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

ERTS_GLB_INLINE void
erts_set_ref_numbers(Uint32 ref[ERTS_REF_NUMBERS], Uint32 thr_id, Uint64 value)
{
    /*
     * We cannot use thread id in the first 18-bit word since
     * the hash/phash/phash2 BIFs only hash on this word. If
     * we did, we would get really poor hash values. Instead
     * we have to shuffle the bits a bit.
     */
    ASSERT(thr_id == (thr_id & ((Uint32) ERTS_REF1_THR_ID_MASK__)));
    ref[0] = (Uint32) (value & ((Uint64) REF_MASK));
    ref[1] = (((Uint32) (value & ((Uint64) ERTS_REF1_NUM_MASK__)))
	      | (thr_id & ((Uint32) ERTS_REF1_THR_ID_MASK__)));
    ref[2] = (Uint32) ((value >> 32) & ((Uint64) 0xffffffff));
}

ERTS_GLB_INLINE Uint32
erts_get_ref_numbers_thr_id(Uint32 ref[ERTS_REF_NUMBERS])
{
    return ref[1] & ((Uint32) ERTS_REF1_THR_ID_MASK__);
}

ERTS_GLB_INLINE int
erts_is_ref_numbers_magic(Uint32 ref[ERTS_REF_NUMBERS])
{
    return !!(ref[1] & ERTS_REF1_MAGIC_MARKER_BIT__);
}

ERTS_GLB_INLINE Uint64
erts_get_ref_numbers_value(Uint32 ref[ERTS_REF_NUMBERS])
{
    ERTS_CT_ASSERT((ERTS_REF1_NUM_MASK__ | REF_MASK) == 0xffffffff);
    ERTS_CT_ASSERT((ERTS_REF1_NUM_MASK__ & REF_MASK) == 0);

    return (((((Uint64) ref[2]) & ((Uint64) 0xffffffff)) << 32)
	    | (((Uint64) ref[1]) & ((Uint64) ERTS_REF1_NUM_MASK__))
	    | (((Uint64) ref[0]) & ((Uint64) REF_MASK)));
}

ERTS_GLB_INLINE void
erts_sched_make_ref_in_array(ErtsSchedulerData *esdp,
			     Uint32 ref[ERTS_REF_NUMBERS])
{
    Uint64 value;

    ASSERT(esdp);
    value = esdp->ref++;
    erts_set_ref_numbers(ref, (Uint32) esdp->thr_id, value);
}

ERTS_GLB_INLINE void
erts_sched_make_magic_ref_in_array(ErtsSchedulerData *esdp,
				   Uint32 ref[ERTS_REF_NUMBERS])
{
    erts_sched_make_ref_in_array(esdp, ref);
    ASSERT(!(ref[1] & ERTS_REF1_MAGIC_MARKER_BIT__));
    ref[1] |= ERTS_REF1_MAGIC_MARKER_BIT__;
}

ERTS_GLB_INLINE Eterm
erts_sched_make_ref_in_buffer(ErtsSchedulerData *esdp,
			      Eterm buffer[ERTS_REF_THING_SIZE])
{
    Eterm* hp = buffer;
    Uint32 ref[ERTS_REF_NUMBERS];

    erts_sched_make_ref_in_array(esdp, ref);
    write_ref_thing(hp, ref[0], ref[1], ref[2]);
    return make_internal_ref(hp);
}

ERTS_GLB_INLINE Eterm
erts_mk_magic_ref(Eterm **hpp, ErlOffHeap *ohp, Binary *bp)
{
    Eterm *hp = *hpp;
    ASSERT(bp->intern.flags & BIN_FLAG_MAGIC);
    write_magic_ref_thing(hp, ohp, (ErtsMagicBinary *) bp);
    *hpp += ERTS_MAGIC_REF_THING_SIZE;
    erts_refc_inc(&bp->intern.refc, 1);
    OH_OVERHEAD(ohp, bp->orig_size / sizeof(Eterm));
    return make_internal_ref(hp);
}

ERTS_GLB_INLINE Binary *
erts_magic_ref2bin(Eterm mref)
{
    ErtsMRefThing *mrtp;
    ASSERT(is_internal_magic_ref(mref));
    mrtp = (ErtsMRefThing *) internal_ref_val(mref);
    return (Binary *) mrtp->mb;
}

/*
 * Save the magic binary of a ref when the
 * ref is exposed to the outside world...
 */
ERTS_GLB_INLINE void
erts_magic_ref_save_bin(Eterm ref)
{
    if (is_internal_magic_ref(ref))
	erts_magic_ref_save_bin__(ref);
}

/*
 * Look up the magic binary of a magic ref
 * when the ref comes from the outside world...
 */
ERTS_GLB_INLINE ErtsMagicBinary *
erts_magic_ref_lookup_bin(Uint32 ref[ERTS_REF_NUMBERS])
{
    if (!erts_is_ref_numbers_magic(ref))
	return NULL;
    return erts_magic_ref_lookup_bin__(ref);
}


#endif /* ERTS_GLB_INLINE_INCL_FUNC_DEF */


/*
 * Storage of internal refs in misc structures...
 */

#include "erl_message.h"

#if ERTS_REF_NUMBERS != 3
#  error fix this...
#endif

ERTS_GLB_INLINE int erts_internal_ref_number_cmp(Uint32 num1[ERTS_REF_NUMBERS],
						 Uint32 num2[ERTS_REF_NUMBERS]);

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

ERTS_GLB_INLINE int
erts_internal_ref_number_cmp(Uint32 num1[ERTS_REF_NUMBERS],
			     Uint32 num2[ERTS_REF_NUMBERS])
{
    if (num1[2] != num2[2])
	return (int) ((Sint64) num1[2] - (Sint64) num2[2]);
    if (num1[1] != num2[1])
	return (int) ((Sint64) num1[1] - (Sint64) num2[1]);
    if (num1[0] != num2[0])
	return (int) ((Sint64) num1[0] - (Sint64) num2[0]);
    return 0;
}

#endif /* #if ERTS_GLB_INLINE_INCL_FUNC_DEF */

/* Iref storage for all internal references... */ 
typedef struct {
    Uint32 is_magic;
    union {
	ErtsMagicBinary *mb;
	Uint32 num[ERTS_REF_NUMBERS];
    } u;
} ErtsIRefStorage;

void erts_ref_bin_free(ErtsMagicBinary *mb);

ERTS_GLB_INLINE void erts_iref_storage_save(ErtsIRefStorage *iref, Eterm ref);
ERTS_GLB_INLINE void erts_iref_storage_clean(ErtsIRefStorage *iref);
ERTS_GLB_INLINE Uint erts_iref_storage_heap_size(ErtsIRefStorage *iref);
ERTS_GLB_INLINE Eterm erts_iref_storage_make_ref(ErtsIRefStorage *iref,
						 Eterm **hpp, ErlOffHeap *ohp,
						 int clean_storage);
ERTS_GLB_INLINE int erts_iref_storage_cmp(ErtsIRefStorage *iref1,
					  ErtsIRefStorage *iref2);


#if ERTS_GLB_INLINE_INCL_FUNC_DEF

ERTS_GLB_INLINE void
erts_iref_storage_save(ErtsIRefStorage *iref, Eterm ref)
{
    Eterm *hp;

    ERTS_CT_ASSERT(ERTS_REF_NUMBERS == 3);
    ASSERT(is_internal_ref(ref));

    hp = boxed_val(ref);

    if (is_ordinary_ref_thing(hp)) {
	ErtsORefThing *rtp = (ErtsORefThing *) hp;
	iref->is_magic = 0;
	iref->u.num[0] = rtp->num[0];
	iref->u.num[1] = rtp->num[1];
	iref->u.num[2] = rtp->num[2];
    }
    else {
	ErtsMRefThing *mrtp = (ErtsMRefThing *) hp;
	ASSERT(is_magic_ref_thing(hp));
	iref->is_magic = 1;
	iref->u.mb = mrtp->mb;
	erts_refc_inc(&mrtp->mb->intern.refc, 1);
    }
}

ERTS_GLB_INLINE void
erts_iref_storage_clean(ErtsIRefStorage *iref)
{
    if (iref->is_magic && erts_refc_dectest(&iref->u.mb->intern.refc, 0) == 0)
	erts_ref_bin_free(iref->u.mb);
#ifdef DEBUG
    memset((void *) iref, 0xf, sizeof(ErtsIRefStorage));
#endif
}

ERTS_GLB_INLINE Uint
erts_iref_storage_heap_size(ErtsIRefStorage *iref)
{
    return iref->is_magic ? ERTS_MAGIC_REF_THING_SIZE : ERTS_REF_THING_SIZE;
}

ERTS_GLB_INLINE Eterm
erts_iref_storage_make_ref(ErtsIRefStorage *iref,
			   Eterm **hpp, ErlOffHeap *ohp,
			   int clean_storage)
{
    Eterm *hp = *hpp;
    if (!iref->is_magic) {
	write_ref_thing(hp, iref->u.num[0], iref->u.num[1],
			iref->u.num[2]);
	*hpp += ERTS_REF_THING_SIZE;
    }
    else {
	write_magic_ref_thing(hp, ohp, iref->u.mb);
        OH_OVERHEAD(ohp, iref->u.mb->orig_size / sizeof(Eterm));
	*hpp += ERTS_MAGIC_REF_THING_SIZE;
	/*
	 * If we clean storage, the term inherits the
	 * refc increment of the cleaned storage...
	 */
	if (!clean_storage)
	    erts_refc_inc(&iref->u.mb->intern.refc, 1);
    }

#ifdef DEBUG
    if (clean_storage)
	memset((void *) iref, 0xf, sizeof(ErtsIRefStorage));
#endif

    return make_internal_ref(hp);
}

ERTS_GLB_INLINE int
erts_iref_storage_cmp(ErtsIRefStorage *iref1,
		      ErtsIRefStorage *iref2)
{
    Uint32 *num1 = iref1->is_magic ? iref1->u.mb->refn : iref1->u.num;
    Uint32 *num2 = iref2->is_magic ? iref2->u.mb->refn : iref2->u.num;
    return erts_internal_ref_number_cmp(num1, num2);
}

#endif /* #if ERTS_GLB_INLINE_INCL_FUNC_DEF */


/* OIref storage for ordinary internal references only... */ 
typedef struct {
    Uint32 num[ERTS_REF_NUMBERS];
} ErtsOIRefStorage;

ERTS_GLB_INLINE void erts_oiref_storage_save(ErtsOIRefStorage *oiref,
					     Eterm ref);
ERTS_GLB_INLINE Eterm erts_oiref_storage_make_ref(ErtsOIRefStorage *oiref,
						  Eterm **hpp);
ERTS_GLB_INLINE int erts_oiref_storage_cmp(ErtsOIRefStorage *oiref1,
					   ErtsOIRefStorage *oiref2);

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

ERTS_GLB_INLINE void
erts_oiref_storage_save(ErtsOIRefStorage *oiref, Eterm ref)
{
    ErtsORefThing *rtp;
    ERTS_CT_ASSERT(ERTS_REF_NUMBERS == 3);
    ASSERT(is_internal_ordinary_ref(ref));

    rtp = (ErtsORefThing *) internal_ref_val(ref);

    oiref->num[0] = rtp->num[0];
    oiref->num[1] = rtp->num[1];
    oiref->num[2] = rtp->num[2];
}

ERTS_GLB_INLINE Eterm
erts_oiref_storage_make_ref(ErtsOIRefStorage *oiref, Eterm **hpp)
{
    Eterm *hp = *hpp;
    ERTS_CT_ASSERT(ERTS_REF_NUMBERS == 3);
    write_ref_thing(hp, oiref->num[0], oiref->num[1], oiref->num[2]);
    *hpp += ERTS_REF_THING_SIZE;
    return make_internal_ref(hp);
}

ERTS_GLB_INLINE int
erts_oiref_storage_cmp(ErtsOIRefStorage *oiref1,
		       ErtsOIRefStorage *oiref2)
{
    return erts_internal_ref_number_cmp(oiref1->num, oiref2->num);
}

#endif /* #if ERTS_GLB_INLINE_INCL_FUNC_DEF */


ERTS_GLB_INLINE Eterm
erts_proc_store_ref(Process *c_p, Uint32 ref[ERTS_MAX_REF_NUMBERS]);

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

ERTS_GLB_INLINE Eterm
erts_proc_store_ref(Process *c_p, Uint32 ref[ERTS_MAX_REF_NUMBERS])
{
    Eterm *hp = HAlloc(c_p, ERTS_REF_THING_SIZE);
    write_ref_thing(hp, ref[0], ref[1], ref[2]);
    return make_internal_ref(hp);
}

#endif

#endif /*  ERTS_BIF_UNIQUE_H__ */

#if (defined(ERTS_ALLOC_C__) || defined(ERL_BIF_UNIQUE_C__)) \
    && !defined(ERTS_BIF_UNIQUE_H__FIX_ALLOC_TYPES__)
#define ERTS_BIF_UNIQUE_H__FIX_ALLOC_TYPES__

#include "hash.h"

typedef struct {
    HashBucket hash;
    ErtsMagicBinary *mb;
    Uint64 value;
} ErtsNSchedMagicRefTableEntry;

#endif
