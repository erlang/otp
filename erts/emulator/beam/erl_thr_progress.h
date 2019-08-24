/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2011-2018. All Rights Reserved.
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
 * Description: Thread progress information. Used by lock free algorithms
 *              to determine when all involved threads are guaranteed to
 *              have passed a specific point of execution.
 *
 *              Usage instructions can be found in ert_thr_progress.c
 *
 * Author: 	Rickard Green
 */

#ifndef ERL_THR_PROGRESS_H__TSD_TYPE__
#define ERL_THR_PROGRESS_H__TSD_TYPE__

#include "sys.h"

void erts_thr_progress_block(void);
void erts_thr_progress_unblock(void);
int erts_thr_progress_is_blocking(void);

typedef Uint64 ErtsThrPrgrVal;

#define ERTS_THR_PRGR_WAKEUP_DATA_SIZE 4 /* Need to be an even power of 2. */

typedef struct {
    ErtsThrPrgrVal next;
    ErtsThrPrgrVal current;
    int chk_next_ix;
    struct {
	int current;
	int waiting;
    } umrefc_ix;
} ErtsThrPrgrLeaderState;

typedef struct {
    int id;
    int is_managed;
    int is_blocking;
#ifdef ERTS_ENABLE_LOCK_CHECK
    int is_delaying; /* managed is always delaying */
#endif
    int is_temporary;

    /* --- Part below only for registered threads --- */

    ErtsThrPrgrVal wakeup_request[ERTS_THR_PRGR_WAKEUP_DATA_SIZE];

    /* --- Part below only for managed threads --- */

    int leader; /* Needs to be first in the managed threads part */
    int active;
    ErtsThrPrgrVal confirmed;
    ErtsThrPrgrLeaderState leader_state;
} ErtsThrPrgrData;

int erts_thr_progress_fatal_error_block(ErtsThrPrgrData *tmp_tpd_bufp);
void erts_thr_progress_fatal_error_wait(SWord timeout);


typedef struct ErtsThrPrgrLaterOp_ ErtsThrPrgrLaterOp;
struct ErtsThrPrgrLaterOp_ {
    ErtsThrPrgrVal later;
    void (*func)(void *);
    void *data;
    ErtsThrPrgrLaterOp *next;
};

#endif

#if !defined(ERL_THR_PROGRESS_H__) && !defined(ERL_THR_PROGRESS_TSD_TYPE_ONLY)
#define ERL_THR_PROGRESS_H__

#include "erl_threads.h"
#include "erl_process.h"


/* ERTS_THR_PRGR_VAL_FIRST should only be used when initializing... */
#define ERTS_THR_PRGR_VAL_FIRST ((ErtsThrPrgrVal) 0)
#define ERTS_THR_PRGR_VAL_WAITING (~((ErtsThrPrgrVal) 0))
#define ERTS_THR_PRGR_INVALID (~((ErtsThrPrgrVal) 0))

extern erts_tsd_key_t erts_thr_prgr_data_key__;

#define ERTS_THR_PRGR_ATOMIC erts_atomic64_t

typedef struct {
    void *arg;
    void (*wakeup)(void *);
    void (*prepare_wait)(void *);
    void (*wait)(void *);
    void (*finalize_wait)(void *);
} ErtsThrPrgrCallbacks;

typedef struct {
    ERTS_THR_PRGR_ATOMIC current;
} ErtsThrPrgr;

typedef int ErtsThrPrgrDelayHandle;
#define ERTS_THR_PRGR_DHANDLE_MANAGED ((ErtsThrPrgrDelayHandle) -1)
/* ERTS_THR_PRGR_DHANDLE_MANAGED implies managed thread */
#define ERTS_THR_PRGR_DHANDLE_INVALID ((ErtsThrPrgrDelayHandle) -2)

extern ErtsThrPrgr erts_thr_prgr__;

void erts_thr_progress_pre_init(void);
void erts_thr_progress_init(int no_schedulers, int managed, int unmanaged);
ErtsThrPrgrData *erts_thr_progress_register_managed_thread(
    ErtsSchedulerData *esdp, ErtsThrPrgrCallbacks *, int);
void erts_thr_progress_register_unmanaged_thread(ErtsThrPrgrCallbacks *);
void erts_thr_progress_active(ErtsThrPrgrData *, int on);
void erts_thr_progress_wakeup(ErtsThrPrgrData *,
			      ErtsThrPrgrVal value);
int erts_thr_progress_update(ErtsThrPrgrData *);
int erts_thr_progress_leader_update(ErtsThrPrgrData *);
void erts_thr_progress_prepare_wait(ErtsThrPrgrData *);
void erts_thr_progress_finalize_wait(ErtsThrPrgrData *);
ErtsThrPrgrDelayHandle erts_thr_progress_unmanaged_delay__(void);
void erts_thr_progress_unmanaged_continue__(int umrefc_ix);
ErtsThrPrgrData *erts_thr_progress_data(void);

void erts_thr_progress_dbg_print_state(void);

ERTS_GLB_INLINE ErtsThrPrgrData *erts_thr_prgr_data(ErtsSchedulerData *esdp);

ERTS_GLB_INLINE ErtsThrPrgrVal erts_thr_prgr_read_nob__(ERTS_THR_PRGR_ATOMIC *atmc);
ERTS_GLB_INLINE ErtsThrPrgrVal erts_thr_prgr_read_acqb__(ERTS_THR_PRGR_ATOMIC *atmc);
ERTS_GLB_INLINE ErtsThrPrgrVal erts_thr_prgr_read_mb__(ERTS_THR_PRGR_ATOMIC *atmc);

ERTS_GLB_INLINE int erts_thr_progress_is_managed_thread(void);
ERTS_GLB_INLINE ErtsThrPrgrDelayHandle erts_thr_progress_unmanaged_delay(void);
ERTS_GLB_INLINE void erts_thr_progress_unmanaged_continue(ErtsThrPrgrDelayHandle handle);
#ifdef ERTS_ENABLE_LOCK_CHECK
ERTS_GLB_INLINE int erts_thr_progress_lc_is_delaying(void);
#endif
ERTS_GLB_INLINE ErtsThrPrgrVal erts_thr_progress_current_to_later__(ErtsThrPrgrVal val);
ERTS_GLB_INLINE ErtsThrPrgrVal erts_thr_progress_later(ErtsSchedulerData *);
ERTS_GLB_INLINE ErtsThrPrgrVal erts_thr_progress_current(void);
ERTS_GLB_INLINE int erts_thr_progress_has_passed__(ErtsThrPrgrVal val1, ErtsThrPrgrVal val2);
ERTS_GLB_INLINE int erts_thr_progress_has_reached_this(ErtsThrPrgrVal this, ErtsThrPrgrVal val);
ERTS_GLB_INLINE int erts_thr_progress_equal(ErtsThrPrgrVal val1,
					    ErtsThrPrgrVal val2);
ERTS_GLB_INLINE int erts_thr_progress_cmp(ErtsThrPrgrVal val1, ErtsThrPrgrVal val2);
ERTS_GLB_INLINE int erts_thr_progress_has_reached(ErtsThrPrgrVal val);

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

ERTS_GLB_INLINE ErtsThrPrgrData *
erts_thr_prgr_data(ErtsSchedulerData *esdp) {
    if (esdp) {
        return &esdp->thr_progress_data;
    } else {
        return erts_thr_progress_data();
    }
}

ERTS_GLB_INLINE ErtsThrPrgrVal
erts_thr_prgr_read_nob__(ERTS_THR_PRGR_ATOMIC *atmc)
{
    return (ErtsThrPrgrVal) erts_atomic64_read_nob(atmc);
}

ERTS_GLB_INLINE ErtsThrPrgrVal
erts_thr_prgr_read_acqb__(ERTS_THR_PRGR_ATOMIC *atmc)
{
    return (ErtsThrPrgrVal) erts_atomic64_read_acqb(atmc);
}

ERTS_GLB_INLINE ErtsThrPrgrVal
erts_thr_prgr_read_mb__(ERTS_THR_PRGR_ATOMIC *atmc)
{
    return (ErtsThrPrgrVal) erts_atomic64_read_mb(atmc);
}

ERTS_GLB_INLINE int
erts_thr_progress_is_managed_thread(void)
{
    ErtsThrPrgrData *tpd = erts_tsd_get(erts_thr_prgr_data_key__);
    return tpd && tpd->is_managed;
}

ERTS_GLB_INLINE ErtsThrPrgrDelayHandle
erts_thr_progress_unmanaged_delay(void)
{
    if (erts_thr_progress_is_managed_thread())
	return ERTS_THR_PRGR_DHANDLE_MANAGED; /* Nothing to do */
    else
	return erts_thr_progress_unmanaged_delay__();
}

ERTS_GLB_INLINE void
erts_thr_progress_unmanaged_continue(ErtsThrPrgrDelayHandle handle)
{
    ASSERT(handle != ERTS_THR_PRGR_DHANDLE_MANAGED
	   || erts_thr_progress_is_managed_thread());
    if (handle != ERTS_THR_PRGR_DHANDLE_MANAGED)
	erts_thr_progress_unmanaged_continue__(handle);
}

#ifdef ERTS_ENABLE_LOCK_CHECK

ERTS_GLB_INLINE int
erts_thr_progress_lc_is_delaying(void)
{
    ErtsThrPrgrData *tpd = erts_tsd_get(erts_thr_prgr_data_key__);
    return tpd && tpd->is_delaying;
}

#endif

ERTS_GLB_INLINE ErtsThrPrgrVal
erts_thr_progress_current_to_later__(ErtsThrPrgrVal val)
{
    if (val == (ERTS_THR_PRGR_VAL_WAITING-((ErtsThrPrgrVal)2)))
	return ((ErtsThrPrgrVal) 0);
    else if (val == (ERTS_THR_PRGR_VAL_WAITING-((ErtsThrPrgrVal)1)))
	return ((ErtsThrPrgrVal) 1);
    else
	return val + ((ErtsThrPrgrVal) 2);
}

ERTS_GLB_INLINE ErtsThrPrgrVal
erts_thr_progress_later(ErtsSchedulerData *esdp)
{
    ErtsThrPrgrData *tpd;
    ErtsThrPrgrVal val;
    if (esdp) {
	tpd = &esdp->thr_progress_data;
    managed_thread:
	val = tpd->confirmed;
	ERTS_THR_MEMORY_BARRIER;
    }
    else {
	tpd = erts_tsd_get(erts_thr_prgr_data_key__);
	if (tpd && tpd->is_managed)
	    goto managed_thread;
	val = erts_thr_prgr_read_mb__(&erts_thr_prgr__.current);
    }
    ASSERT(val != ERTS_THR_PRGR_VAL_WAITING);
    return erts_thr_progress_current_to_later__(val);
}

ERTS_GLB_INLINE ErtsThrPrgrVal
erts_thr_progress_current(void)
{
    if (erts_thr_progress_is_managed_thread())
	return erts_thr_prgr_read_nob__(&erts_thr_prgr__.current);
    else
	return erts_thr_prgr_read_acqb__(&erts_thr_prgr__.current);
}

ERTS_GLB_INLINE int
erts_thr_progress_has_passed__(ErtsThrPrgrVal val1, ErtsThrPrgrVal val0)
{
    if ((((((ErtsThrPrgrVal) 1) << 63) & val1)
	 ^ ((((ErtsThrPrgrVal) 1) << 63) & val0)) != 0) {
	/* May have wrapped... */
	if (val1 < (((ErtsThrPrgrVal) 1) << 62)
	    && val0 > (((ErtsThrPrgrVal) 3) << 62)) {
	    /*
	     * 'val1' has wrapped but 'val0' has not yet wrapped. While in
	     * these ranges 'current' is considered later than 'val0'.
	     */
	    return 1;
	}
    }
    return val1 > val0;
}

ERTS_GLB_INLINE int
erts_thr_progress_has_reached_this(ErtsThrPrgrVal this, ErtsThrPrgrVal val)
{
    if (this == val)
	return 1;
    return erts_thr_progress_has_passed__(this, val);
}

ERTS_GLB_INLINE int
erts_thr_progress_equal(ErtsThrPrgrVal val1, ErtsThrPrgrVal val2)
{
    return val1 == val2 && val1 != ERTS_THR_PRGR_INVALID;
}

ERTS_GLB_INLINE int
erts_thr_progress_cmp(ErtsThrPrgrVal val1, ErtsThrPrgrVal val2)
{
    if (val1 == val2)
	return 0;
    if (erts_thr_progress_has_passed__(val1, val2))
	return 1;
    else
	return -1;
}	

ERTS_GLB_INLINE int
erts_thr_progress_has_reached(ErtsThrPrgrVal val)
{
    ErtsThrPrgrVal current = erts_thr_progress_current();
    return erts_thr_progress_has_reached_this(current, val);
}

#endif


#endif
