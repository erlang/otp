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

#include "erl_process.h"
#include "big.h"

void erts_bif_unique_init(void);
void erts_sched_bif_unique_init(ErtsSchedulerData *esdp);

/* reference */
Eterm erts_make_ref(Process *);
Eterm erts_make_ref_in_buffer(Eterm buffer[REF_THING_SIZE]);
void erts_make_ref_in_array(Uint32 ref[ERTS_MAX_REF_NUMBERS]);

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


ERTS_GLB_INLINE void erts_set_ref_numbers(Uint32 ref[ERTS_MAX_REF_NUMBERS],
					  Uint32 thr_id, Uint64 value);
ERTS_GLB_INLINE Uint32 erts_get_ref_numbers_thr_id(Uint32 ref[ERTS_MAX_REF_NUMBERS]);
ERTS_GLB_INLINE Uint64 erts_get_ref_numbers_value(Uint32 ref[ERTS_MAX_REF_NUMBERS]);
ERTS_GLB_INLINE void erts_sched_make_ref_in_array(ErtsSchedulerData *esdp,
						  Uint32 ref[ERTS_MAX_REF_NUMBERS]);
ERTS_GLB_INLINE Eterm erts_sched_make_ref_in_buffer(ErtsSchedulerData *esdp,
						    Eterm buffer[REF_THING_SIZE]);

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

ERTS_GLB_INLINE void
erts_set_ref_numbers(Uint32 ref[ERTS_MAX_REF_NUMBERS], Uint32 thr_id, Uint64 value)
{
    /*
     * We cannot use thread id in the first 18-bit word since
     * the hash/phash/phash2 BIFs only hash on this word. If
     * we did, we would get really poor hash values. Instead
     * we have to shuffle the bits a bit.
     */
    ASSERT(thr_id == (thr_id & ((Uint32) 0x3ffff)));
    ref[0] = (Uint32) (value & ((Uint64) 0x3ffff));
    ref[1] = (((Uint32) (value & ((Uint64) 0xfffc0000)))
	      | (thr_id & ((Uint32) 0x3ffff)));
    ref[2] = (Uint32) ((value >> 32) & ((Uint64) 0xffffffff));
}

ERTS_GLB_INLINE Uint32
erts_get_ref_numbers_thr_id(Uint32 ref[ERTS_MAX_REF_NUMBERS])
{
    return ref[1] & ((Uint32) 0x3ffff);
}

ERTS_GLB_INLINE Uint64
erts_get_ref_numbers_value(Uint32 ref[ERTS_MAX_REF_NUMBERS])
{
    return (((((Uint64) ref[2]) & ((Uint64) 0xffffffff)) << 32)
	    | (((Uint64) ref[1]) & ((Uint64) 0xfffc0000))
	    | (((Uint64) ref[0]) & ((Uint64) 0x3ffff)));
}

ERTS_GLB_INLINE void
erts_sched_make_ref_in_array(ErtsSchedulerData *esdp,
			     Uint32 ref[ERTS_MAX_REF_NUMBERS])
{
    Uint64 value;

    ASSERT(esdp);
    value = esdp->ref++;
    erts_set_ref_numbers(ref, (Uint32) esdp->thr_id, value);
}

ERTS_GLB_INLINE Eterm
erts_sched_make_ref_in_buffer(ErtsSchedulerData *esdp,
			      Eterm buffer[REF_THING_SIZE])
{
    Eterm* hp = buffer;
    Uint32 ref[ERTS_MAX_REF_NUMBERS];

    erts_sched_make_ref_in_array(esdp, ref);
    write_ref_thing(hp, ref[0], ref[1], ref[2]);
    return make_internal_ref(hp);
}

#endif /* ERTS_GLB_INLINE_INCL_FUNC_DEF */

#endif /*  ERTS_BIF_UNIQUE_H__ */
