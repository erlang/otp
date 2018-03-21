/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1999-2018. All Rights Reserved.
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
 * Support routines for the time
 */

/* #define ERTS_TIME_CORRECTION_PRINT */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "erl_vm.h"
#include "global.h"
#define ERTS_WANT_TIMER_WHEEL_API
#include "erl_time.h"
#include "erl_driver.h"
#include "erl_nif.h"
#include "erl_proc_sig_queue.h"
 
static erts_mtx_t erts_get_time_mtx;

 /* used by erts_runtime_elapsed_both */
typedef struct {
    erts_mtx_t mtx;
    ErtsMonotonicTime user;
    ErtsMonotonicTime sys;
} ErtsRunTimePrevData;

static union {
    ErtsRunTimePrevData data;
    char align__[ERTS_ALC_CACHE_LINE_ALIGN_SIZE(sizeof(ErtsRunTimePrevData))];
} runtime_prev erts_align_attribute(ERTS_CACHE_LINE_SIZE);

static union {
    erts_atomic64_t time;
    char align__[ERTS_ALC_CACHE_LINE_ALIGN_SIZE(sizeof(erts_atomic64_t))];
} wall_clock_prev erts_align_attribute(ERTS_CACHE_LINE_SIZE);

static union {
    erts_atomic64_t time;
    char align__[ERTS_ALC_CACHE_LINE_ALIGN_SIZE(sizeof(erts_atomic64_t))];
} now_prev erts_align_attribute(ERTS_CACHE_LINE_SIZE);

static ErtsMonitor *time_offset_monitors = NULL;
static Uint no_time_offset_monitors = 0;

#ifdef DEBUG
static int time_sup_initialized = 0;
#endif

#define ERTS_MONOTONIC_TIME_KILO \
    ((ErtsMonotonicTime) 1000)
#define ERTS_MONOTONIC_TIME_MEGA \
    (ERTS_MONOTONIC_TIME_KILO*ERTS_MONOTONIC_TIME_KILO)
#define ERTS_MONOTONIC_TIME_GIGA \
    (ERTS_MONOTONIC_TIME_MEGA*ERTS_MONOTONIC_TIME_KILO)
#define ERTS_MONOTONIC_TIME_TERA \
    (ERTS_MONOTONIC_TIME_GIGA*ERTS_MONOTONIC_TIME_KILO)

static void init_time_napi(void);
static void
schedule_send_time_offset_changed_notifications(ErtsMonotonicTime new_offset);

struct time_sup_read_only__ {
    ErtsMonotonicTime (*get_time)(void);
    int correction;
    ErtsTimeWarpMode warp_mode;
#ifdef ERTS_HAVE_OS_MONOTONIC_TIME_SUPPORT
    ErtsMonotonicTime moffset;
    int os_corrected_monotonic_time;
    int os_monotonic_time_disable;
    char *os_monotonic_time_func;
    char *os_monotonic_time_clock_id;
    int os_monotonic_time_locked;
    Uint64 os_monotonic_time_resolution;
    Uint64 os_monotonic_time_extended;
#endif
    char *os_system_time_func;
    char *os_system_time_clock_id;
    int os_system_time_locked;
    Uint64 os_system_time_resolution;
    Uint64 os_system_time_extended;
    struct {
	ErtsMonotonicTime large_diff;
	ErtsMonotonicTime small_diff;
    } adj;
    struct {
	ErtsMonotonicTime error;
	ErtsMonotonicTime resolution;
	int intervals;
	int use_avg;
    } drift_adj;
};

typedef struct {
    ErtsMonotonicTime drift; /* Correction for os monotonic drift */
    ErtsMonotonicTime error; /* Correction for error between system times */
} ErtsMonotonicCorrection;

typedef struct {
    ErtsMonotonicTime erl_mtime;
    ErtsMonotonicTime os_mtime;
    ErtsMonotonicCorrection correction;
} ErtsMonotonicCorrectionInstance;

#define ERTS_MAX_DRIFT_INTERVALS 50
typedef struct {
    struct {
	struct {
	    ErtsMonotonicTime sys;
	    ErtsMonotonicTime mon;
	} diff;
	struct {
	    ErtsMonotonicTime sys;
	    ErtsMonotonicTime mon;
	} time;
    } intervals[ERTS_MAX_DRIFT_INTERVALS];
    struct {
	ErtsMonotonicTime sys;
	ErtsMonotonicTime mon;
    } acc;
    int ix;
    int dirty_counter;
} ErtsMonotonicDriftData;

typedef struct {
    ErtsMonotonicCorrectionInstance prev;
    ErtsMonotonicCorrectionInstance curr;
} ErtsMonotonicCorrectionInstances;

typedef struct {
    ErtsMonotonicCorrectionInstances insts;
    ErtsMonotonicDriftData drift;
    ErtsMonotonicTime last_check;
    int short_check_interval;
} ErtsMonotonicCorrectionData;

struct time_sup_infrequently_changed__ {
#ifdef ERTS_HAVE_OS_MONOTONIC_TIME_SUPPORT
    struct {
	erts_rwmtx_t rwmtx;
	ErtsTWheelTimer timer;
	ErtsMonotonicCorrectionData cdata;
    } parmon;
    ErtsMonotonicTime minit;
#endif
    ErtsSystemTime sinit;
    ErtsMonotonicTime not_corrected_moffset;
    erts_atomic64_t offset;
    ErtsMonotonicTime shadow_offset;
    erts_atomic32_t preliminary_offset;
};

struct time_sup_frequently_changed__ {
    ErtsMonotonicTime last_not_corrected_time;
};

static struct {
    union {
	struct time_sup_read_only__ o;
	char align__[ERTS_ALC_CACHE_LINE_ALIGN_SIZE(sizeof(struct time_sup_read_only__))];
    } r;
    union {
	struct time_sup_infrequently_changed__ c;
	char align__[ERTS_ALC_CACHE_LINE_ALIGN_SIZE(sizeof(struct time_sup_infrequently_changed__))];
    } inf;
    union {
	struct time_sup_frequently_changed__ c;
	char align__[ERTS_ALC_CACHE_LINE_ALIGN_SIZE(sizeof(struct time_sup_frequently_changed__))];
    } f;
} time_sup erts_align_attribute(ERTS_CACHE_LINE_SIZE);

ErtsTimeSupData erts_time_sup__ erts_align_attribute(ERTS_CACHE_LINE_SIZE);

static ERTS_INLINE void
init_time_offset(ErtsMonotonicTime offset)
{
    erts_atomic64_init_nob(&time_sup.inf.c.offset, (erts_aint64_t) offset);
}

static ERTS_INLINE void
set_time_offset(ErtsMonotonicTime offset)
{
    erts_atomic64_set_relb(&time_sup.inf.c.offset, (erts_aint64_t) offset);
}

static ERTS_INLINE ErtsMonotonicTime
get_time_offset(void)
{
    return (ErtsMonotonicTime) erts_atomic64_read_acqb(&time_sup.inf.c.offset);
}

static ERTS_INLINE void
update_last_mtime(ErtsSchedulerData *esdp, ErtsMonotonicTime mtime)
{
    if (!esdp)
	esdp = erts_get_scheduler_data();
    if (esdp) {
	ASSERT(mtime >= esdp->last_monotonic_time);
	esdp->last_monotonic_time = mtime;
	esdp->check_time_reds = 0;
    }
}

#ifdef ERTS_HAVE_OS_MONOTONIC_TIME_SUPPORT

/*
 * Time correction adjustments made due to
 * error between Erlang system time and OS
 * system time:
 * - Large adjustment ~1%
 * - Small adjustment ~0.05%
 */
#define ERTS_TCORR_ERR_UNIT 2048
#define ERTS_TCORR_ERR_LARGE_ADJ 20
#define ERTS_TCORR_ERR_SMALL_ADJ 1

#define ERTS_INIT_SHORT_INTERVAL_COUNTER 10
#define ERTS_LONG_TIME_CORRECTION_CHECK ERTS_SEC_TO_MONOTONIC(60)
#define ERTS_SHORT_TIME_CORRECTION_CHECK ERTS_SEC_TO_MONOTONIC(15)

#define ERTS_TIME_DRIFT_MAX_ADJ_DIFF ERTS_USEC_TO_MONOTONIC(50)
#define ERTS_TIME_DRIFT_MIN_ADJ_DIFF ERTS_USEC_TO_MONOTONIC(5)

/*
 * Maximum drift of the OS monotonic clock expected.
 *
 * We use 1 milli second per second. If the monotonic
 * clock drifts more than this we will fail to adjust for
 * drift, and error correction will kick in instead.
 * If it is larger than this, one could argue that the
 * primitive is to poor to be used...
 */
#define ERTS_MAX_MONOTONIC_DRIFT ERTS_MSEC_TO_MONOTONIC(1)

/*
 * We assume that precision is 32 times worse than the
 * resolution. This is a wild guess, but there are no
 * practical way to determine actual precision.
 */
#define ERTS_ASSUMED_PRECISION_DROP 32

#define ERTS_MIN_MONOTONIC_DRIFT_MEASUREMENT \
    (ERTS_SHORT_TIME_CORRECTION_CHECK - 2*ERTS_MAX_MONOTONIC_DRIFT)


static ERTS_INLINE ErtsMonotonicTime
calc_corrected_erl_mtime(ErtsMonotonicTime os_mtime,
			 ErtsMonotonicCorrectionInstance *cip,
			 ErtsMonotonicTime *os_mdiff_p,
			 int os_drift_corrected)
{
    ErtsMonotonicTime erl_mtime, diff = os_mtime - cip->os_mtime;
    ERTS_TIME_ASSERT(diff >= 0);
    if (!os_drift_corrected)
	diff += (cip->correction.drift*diff)/ERTS_MONOTONIC_TIME_UNIT;
    erl_mtime = cip->erl_mtime;
    erl_mtime += diff;
    erl_mtime += cip->correction.error*(diff/ERTS_TCORR_ERR_UNIT);
    if (os_mdiff_p)
	*os_mdiff_p = diff;
    return erl_mtime;
}

static ERTS_INLINE ErtsMonotonicTime
read_corrected_time(int os_drift_corrected)
{
    ErtsMonotonicTime os_mtime;
    ErtsMonotonicCorrectionInstance ci;

    erts_rwmtx_rlock(&time_sup.inf.c.parmon.rwmtx);

    os_mtime = erts_os_monotonic_time();

    if (os_mtime >= time_sup.inf.c.parmon.cdata.insts.curr.os_mtime)
	ci = time_sup.inf.c.parmon.cdata.insts.curr;
    else {
	if (os_mtime < time_sup.inf.c.parmon.cdata.insts.prev.os_mtime)
	    erts_exit(ERTS_ABORT_EXIT,
		     "OS monotonic time stepped backwards\n");
	ci = time_sup.inf.c.parmon.cdata.insts.prev;
    }

    erts_rwmtx_runlock(&time_sup.inf.c.parmon.rwmtx);

    return calc_corrected_erl_mtime(os_mtime, &ci, NULL,
				    os_drift_corrected);
}

static ErtsMonotonicTime get_os_drift_corrected_time(void)
{
    return read_corrected_time(!0);
}

static ErtsMonotonicTime get_corrected_time(void)
{
    return read_corrected_time(0);
}

#ifdef ERTS_TIME_CORRECTION_PRINT

static ERTS_INLINE void
print_correction(int change,
		 ErtsMonotonicTime sdiff,
		 ErtsMonotonicTime old_ecorr,
		 ErtsMonotonicTime old_dcorr,
		 ErtsMonotonicTime new_ecorr,
		 ErtsMonotonicTime new_dcorr,
		 Uint tmo)
{
    ErtsMonotonicTime usec_sdiff;
    if (sdiff < 0)
	usec_sdiff = -1*ERTS_MONOTONIC_TO_USEC(-1*sdiff);
    else
	usec_sdiff = ERTS_MONOTONIC_TO_USEC(sdiff);

    if (!change)
	erts_fprintf(stderr,
		     "sdiff = %b64d usec : [ec=%b64d ppm, dc=%b64d ppb] : "
		     "tmo = %bpu msec\r\n",
		     usec_sdiff,
		     (1000000*old_ecorr) / ERTS_TCORR_ERR_UNIT,
		     (1000000000*old_dcorr) / ERTS_MONOTONIC_TIME_UNIT,
		     tmo);
    else
	erts_fprintf(stderr,
		     "sdiff = %b64d usec : [ec=%b64d ppm, dc=%b64d ppb] "
		     "-> [ec=%b64d ppm, dc=%b64d ppb] : tmo = %bpu msec\r\n",
		     usec_sdiff,
		     (1000000*old_ecorr) / ERTS_TCORR_ERR_UNIT,
		     (1000000000*old_dcorr) / ERTS_MONOTONIC_TIME_UNIT,
		     (1000000*new_ecorr) / ERTS_TCORR_ERR_UNIT,
		     (1000000000*new_dcorr) / ERTS_MONOTONIC_TIME_UNIT,
		     tmo);
}

#endif

static ERTS_INLINE ErtsMonotonicTime
get_timeout_pos(ErtsMonotonicTime now, ErtsMonotonicTime tmo)
{
    ErtsMonotonicTime tpos;
    tpos = ERTS_MONOTONIC_TO_CLKTCKS(now - 1);
    tpos += ERTS_MSEC_TO_CLKTCKS(tmo);
    tpos += 1;
    return tpos;
}

static void
check_time_correction(void *vesdp)
{
    int init_drift_adj = !vesdp;
    ErtsSchedulerData *esdp = (ErtsSchedulerData *) vesdp;
    ErtsMonotonicCorrection new_correction;
    ErtsMonotonicCorrectionInstance ci;
    ErtsMonotonicTime mdiff, sdiff, os_mtime, erl_mtime, os_stime,
	erl_stime, time_offset, timeout_pos;
    Uint timeout;
    int os_drift_corrected = time_sup.r.o.os_corrected_monotonic_time;
    int set_new_correction = 0, begin_short_intervals = 0;

    erts_rwmtx_rlock(&time_sup.inf.c.parmon.rwmtx);

    erts_os_times(&os_mtime, &os_stime);

    ci = time_sup.inf.c.parmon.cdata.insts.curr;

    erts_rwmtx_runlock(&time_sup.inf.c.parmon.rwmtx);

    if (os_mtime < ci.os_mtime)
	erts_exit(ERTS_ABORT_EXIT,
		 "OS monotonic time stepped backwards\n");

    erl_mtime = calc_corrected_erl_mtime(os_mtime, &ci, &mdiff,
					 os_drift_corrected);
    time_offset = get_time_offset();
    erl_stime = erl_mtime + time_offset;

    sdiff = erl_stime - os_stime;

    if (time_sup.inf.c.shadow_offset) {
	ERTS_TIME_ASSERT(time_sup.r.o.warp_mode == ERTS_SINGLE_TIME_WARP_MODE);
	if (erts_atomic32_read_nob(&time_sup.inf.c.preliminary_offset))
	    sdiff += time_sup.inf.c.shadow_offset;
	else
	    time_sup.inf.c.shadow_offset = 0;
    }

    new_correction = ci.correction;

    if (time_sup.r.o.warp_mode == ERTS_MULTI_TIME_WARP_MODE
	&& (sdiff < -2*time_sup.r.o.adj.small_diff
	    || 2*time_sup.r.o.adj.small_diff < sdiff)) {
	/* System time diff exeeded limits; change time offset... */
	time_offset -= sdiff;
	sdiff = 0;
	set_time_offset(time_offset);
	schedule_send_time_offset_changed_notifications(time_offset);
	begin_short_intervals = 1;
	if (ci.correction.error != 0) {
	    set_new_correction = 1;
	    new_correction.error = 0;
	}
    }
    else if ((time_sup.r.o.warp_mode == ERTS_SINGLE_TIME_WARP_MODE
	      && erts_atomic32_read_nob(&time_sup.inf.c.preliminary_offset))
	     && (sdiff < -2*time_sup.r.o.adj.small_diff
		 || 2*time_sup.r.o.adj.small_diff < sdiff)) {
	/*
	 * System time diff exeeded limits; change shadow offset
	 * and let OS system time leap away from Erlang system
	 * time.
	 */
	time_sup.inf.c.shadow_offset -= sdiff;
	sdiff = 0;
	begin_short_intervals = 1;
	if (ci.correction.error != 0) {
	    set_new_correction = 1;
	    new_correction.error = 0;
	}
    }
    else if (ci.correction.error == 0) {
	if (sdiff < -time_sup.r.o.adj.small_diff) {
	    set_new_correction = 1;
	    if (sdiff < -time_sup.r.o.adj.large_diff)
		new_correction.error = ERTS_TCORR_ERR_LARGE_ADJ;
	    else
		new_correction.error = ERTS_TCORR_ERR_SMALL_ADJ;
	}
	else if (sdiff > time_sup.r.o.adj.small_diff) {
	    set_new_correction = 1;
	    if (sdiff > time_sup.r.o.adj.large_diff)
		new_correction.error = -ERTS_TCORR_ERR_LARGE_ADJ;
	    else
		new_correction.error = -ERTS_TCORR_ERR_SMALL_ADJ;
	}
    }
    else if (ci.correction.error > 0) {
	if (sdiff < 0) {
	    if (ci.correction.error != ERTS_TCORR_ERR_LARGE_ADJ
		&& sdiff < -time_sup.r.o.adj.large_diff) {
		new_correction.error = ERTS_TCORR_ERR_LARGE_ADJ;
		set_new_correction = 1;
	    }
	}
	else if (sdiff > time_sup.r.o.adj.small_diff) {
	    set_new_correction = 1;
	    if (sdiff > time_sup.r.o.adj.large_diff)
		new_correction.error = -ERTS_TCORR_ERR_LARGE_ADJ;
	    else
		new_correction.error = -ERTS_TCORR_ERR_SMALL_ADJ;
	}
	else {
	    set_new_correction = 1;
	    new_correction.error = 0;
	}
    }
    else /* if (ci.correction.error < 0) */ { 
	if (0 < sdiff) {
	    if (ci.correction.error != -ERTS_TCORR_ERR_LARGE_ADJ
		&& time_sup.r.o.adj.large_diff < sdiff) {
		new_correction.error = -ERTS_TCORR_ERR_LARGE_ADJ;
		set_new_correction = 1;
	    }
	}
	else if (sdiff < -time_sup.r.o.adj.small_diff) {
	    set_new_correction = 1;
	    if (sdiff < -time_sup.r.o.adj.large_diff)
		new_correction.error = ERTS_TCORR_ERR_LARGE_ADJ;
	    else
		new_correction.error = ERTS_TCORR_ERR_SMALL_ADJ;
	}
	else {
	    set_new_correction = 1;
	    new_correction.error = 0;
	}
    }

    if (!os_drift_corrected) {
	ErtsMonotonicDriftData *ddp = &time_sup.inf.c.parmon.cdata.drift;
	int ix = ddp->ix;
	ErtsMonotonicTime mtime_diff, old_os_mtime;

	old_os_mtime = ddp->intervals[ix].time.mon;
	mtime_diff = os_mtime - old_os_mtime;

	if ((mtime_diff >= ERTS_MIN_MONOTONIC_DRIFT_MEASUREMENT)
	    | init_drift_adj) {
	    ErtsMonotonicTime drift_adj, drift_adj_diff, old_os_stime,
		smtime_diff, stime_diff, mtime_acc, stime_acc,
		avg_drift_adj, max_drift;

	    old_os_stime = ddp->intervals[ix].time.sys;

	    mtime_acc = ddp->acc.mon;
	    stime_acc = ddp->acc.sys;

	    avg_drift_adj = (((stime_acc - mtime_acc)
			      * ERTS_MONOTONIC_TIME_UNIT)
			     / mtime_acc);

	    mtime_diff = os_mtime - old_os_mtime;
	    stime_diff = os_stime - old_os_stime;
	    smtime_diff = stime_diff - mtime_diff;
	    ix++;
	    if (ix >= time_sup.r.o.drift_adj.intervals)
		ix = 0;
	    mtime_acc -= ddp->intervals[ix].diff.mon;
	    mtime_acc += mtime_diff;
	    stime_acc -= ddp->intervals[ix].diff.sys;
	    stime_acc += stime_diff;

	    ddp->intervals[ix].diff.mon = mtime_diff;
	    ddp->intervals[ix].diff.sys = stime_diff;
	    ddp->intervals[ix].time.mon = os_mtime;
	    ddp->intervals[ix].time.sys = os_stime;

	    ddp->ix = ix;
	    ddp->acc.mon = mtime_acc;
	    ddp->acc.sys = stime_acc;

	    max_drift = ERTS_MAX_MONOTONIC_DRIFT;
	    max_drift *= ERTS_MONOTONIC_TO_SEC(mtime_diff);

	    if (smtime_diff > time_sup.r.o.drift_adj.error + max_drift
		|| smtime_diff < -1*time_sup.r.o.drift_adj.error - max_drift) {
	    dirty_intervals:
		/*
		 * We had a leap in system time. Mark array as
		 * dirty to ensure that dirty values are rotated
		 * out before we use it again...
		 */
		ddp->dirty_counter = time_sup.r.o.drift_adj.intervals;
		begin_short_intervals = 1;
	    }
	    else if (ddp->dirty_counter > 0) {
		if (init_drift_adj) {
		    new_correction.drift = ((smtime_diff
					     * ERTS_MONOTONIC_TIME_UNIT)
					    / mtime_diff);
		    set_new_correction = 1;
		}
		ddp->dirty_counter--;
	    }
	    else {
		if (ddp->dirty_counter == 0) {
		    /* Force set new drift correction... */
		    set_new_correction = 1;
		    ddp->dirty_counter--;
		}

		if (time_sup.r.o.drift_adj.use_avg)
		    drift_adj = (((stime_acc - mtime_acc)
				  * ERTS_MONOTONIC_TIME_UNIT)
				 / mtime_acc);
		else
		    drift_adj = ((smtime_diff
				  * ERTS_MONOTONIC_TIME_UNIT)
				 / mtime_diff);

		drift_adj_diff = avg_drift_adj - drift_adj;
		if (drift_adj_diff < -ERTS_TIME_DRIFT_MAX_ADJ_DIFF
		    || ERTS_TIME_DRIFT_MAX_ADJ_DIFF < drift_adj_diff)
		    goto dirty_intervals;

		drift_adj_diff = drift_adj - new_correction.drift;
		if (drift_adj_diff) {
		    if (drift_adj_diff > ERTS_TIME_DRIFT_MAX_ADJ_DIFF)
			drift_adj_diff = ERTS_TIME_DRIFT_MAX_ADJ_DIFF;
		    else if (drift_adj_diff < -ERTS_TIME_DRIFT_MAX_ADJ_DIFF)
			drift_adj_diff = -ERTS_TIME_DRIFT_MAX_ADJ_DIFF;
		    new_correction.drift += drift_adj_diff;
		    if (drift_adj_diff < -ERTS_TIME_DRIFT_MIN_ADJ_DIFF
			|| ERTS_TIME_DRIFT_MIN_ADJ_DIFF < drift_adj_diff) {
			set_new_correction = 1;
		    }
		}
	    }
	}
    }

    begin_short_intervals |= set_new_correction;
    
    if (begin_short_intervals) {
	time_sup.inf.c.parmon.cdata.short_check_interval
	    = ERTS_INIT_SHORT_INTERVAL_COUNTER;
    }
    else if ((os_mtime - time_sup.inf.c.parmon.cdata.last_check
	      >= ERTS_SHORT_TIME_CORRECTION_CHECK - ERTS_MONOTONIC_TIME_UNIT)
	     && time_sup.inf.c.parmon.cdata.short_check_interval > 0) {
	time_sup.inf.c.parmon.cdata.short_check_interval--;
    }
    time_sup.inf.c.parmon.cdata.last_check = os_mtime;

    if (new_correction.error == 0)
	timeout = ERTS_MONOTONIC_TO_MSEC(ERTS_LONG_TIME_CORRECTION_CHECK);
    else {
	ErtsMonotonicTime ecorr = new_correction.error;
	ErtsMonotonicTime abs_sdiff;
	abs_sdiff = (sdiff < 0) ? -1*sdiff : sdiff;
	if (ecorr < 0)
	    ecorr = -1*ecorr;
	if (abs_sdiff > ecorr*(ERTS_LONG_TIME_CORRECTION_CHECK/ERTS_TCORR_ERR_UNIT))
	    timeout = ERTS_MONOTONIC_TO_MSEC(ERTS_LONG_TIME_CORRECTION_CHECK);
	else {
	    timeout = ERTS_MONOTONIC_TO_MSEC((ERTS_TCORR_ERR_UNIT*abs_sdiff)/ecorr);
	    if (timeout < 10)
		timeout = 10;
	}
    }

    if (timeout > ERTS_MONOTONIC_TO_MSEC(ERTS_SHORT_TIME_CORRECTION_CHECK)
	&& (time_sup.inf.c.parmon.cdata.short_check_interval
	    || time_sup.inf.c.parmon.cdata.drift.dirty_counter >= 0)) {
	timeout = ERTS_MONOTONIC_TO_MSEC(ERTS_SHORT_TIME_CORRECTION_CHECK);
    }

    timeout_pos = get_timeout_pos(erl_mtime, timeout);

#ifdef ERTS_TIME_CORRECTION_PRINT
    print_correction(set_new_correction,
		     sdiff,
		     ci.correction.error,
		     ci.correction.drift,
		     new_correction.error,
		     new_correction.drift,
		     timeout);
#endif

    if (set_new_correction) {
	erts_rwmtx_rwlock(&time_sup.inf.c.parmon.rwmtx);

	os_mtime = erts_os_monotonic_time();

	/* Save previous correction instance */
	time_sup.inf.c.parmon.cdata.insts.prev = ci;

	/*
	 * Current correction instance begin when
	 * OS monotonic time has increased two units.
	 */
	os_mtime += 2;

	/*
	 * Erlang monotonic time corresponding to
	 * next OS monotonic time using previous
	 * correction.
	 */
	erl_mtime = calc_corrected_erl_mtime(os_mtime, &ci, NULL,
					     os_drift_corrected);

	/*
	 * Save new current correction instance.
	 */
	time_sup.inf.c.parmon.cdata.insts.curr.erl_mtime = erl_mtime;
	time_sup.inf.c.parmon.cdata.insts.curr.os_mtime = os_mtime;
	time_sup.inf.c.parmon.cdata.insts.curr.correction = new_correction;

	erts_rwmtx_rwunlock(&time_sup.inf.c.parmon.rwmtx);
    }

    if (!esdp)
	esdp = erts_get_scheduler_data();

    erts_twheel_set_timer(esdp->timer_wheel,
			  &time_sup.inf.c.parmon.timer,
			  check_time_correction,
			  (void *) esdp,
			  timeout_pos);
}

static ErtsMonotonicTime get_os_corrected_time(void)
{
    ASSERT(time_sup.r.o.warp_mode == ERTS_MULTI_TIME_WARP_MODE);
    return erts_os_monotonic_time() + time_sup.r.o.moffset;
}

static void
check_time_offset(void *vesdp)
{
    ErtsSchedulerData *esdp = (ErtsSchedulerData *) vesdp;
    ErtsMonotonicTime sdiff, os_mtime, erl_mtime, os_stime,
	erl_stime, time_offset, timeout, timeout_pos;

    ASSERT(time_sup.r.o.warp_mode == ERTS_MULTI_TIME_WARP_MODE);

    erts_os_times(&os_mtime, &os_stime);

    erl_mtime =  os_mtime + time_sup.r.o.moffset;
    time_offset = get_time_offset();
    erl_stime = erl_mtime + time_offset;

    sdiff = erl_stime - os_stime;

    if ((sdiff < -2*time_sup.r.o.adj.small_diff
	 || 2*time_sup.r.o.adj.small_diff < sdiff)) {
	/* System time diff exeeded limits; change time offset... */
#ifdef ERTS_TIME_CORRECTION_PRINT
	erts_fprintf(stderr, "sdiff = %b64d nsec -> 0 nsec\n",
		     ERTS_MONOTONIC_TO_NSEC(sdiff));
#endif
	time_offset -= sdiff;
	sdiff = 0;
	set_time_offset(time_offset);
	schedule_send_time_offset_changed_notifications(time_offset);
    }
#ifdef ERTS_TIME_CORRECTION_PRINT
    else erts_fprintf(stderr, "sdiff = %b64d nsec\n",
		      ERTS_MONOTONIC_TO_NSEC(sdiff));
#endif

    timeout = ERTS_MONOTONIC_TO_MSEC(ERTS_LONG_TIME_CORRECTION_CHECK);
    timeout_pos = get_timeout_pos(erl_mtime, timeout);

    erts_twheel_set_timer(esdp->timer_wheel,
			  &time_sup.inf.c.parmon.timer,
			  check_time_offset,
			  vesdp,
			  timeout_pos);
}

static void
init_check_time_correction(void *vesdp)
{
    ErtsMonotonicDriftData *ddp;
    ErtsMonotonicTime old_mtime, old_stime, mtime, stime, mtime_diff,
	stime_diff, smtime_diff, max_drift;
    int ix;

    ddp = &time_sup.inf.c.parmon.cdata.drift;
    ix = ddp->ix;
    old_mtime = ddp->intervals[0].time.mon;
    old_stime = ddp->intervals[0].time.sys;

    erts_os_times(&mtime, &stime);

    mtime_diff = mtime - old_mtime;
    stime_diff = stime - old_stime;
    smtime_diff = stime_diff - mtime_diff;

    max_drift = ERTS_MAX_MONOTONIC_DRIFT;
    max_drift *= ERTS_MONOTONIC_TO_SEC(mtime_diff);

    if (smtime_diff > time_sup.r.o.drift_adj.error + max_drift
	|| smtime_diff < -1*time_sup.r.o.drift_adj.error - max_drift) {
	/* Had a system time leap... pretend no drift... */
	stime_diff = mtime_diff;
    }
    
    /*
     * We use old time values in order to trigger
     * a drift adjustment, and repeat this interval
     * in all slots...
     */
    for (ix = 0; ix < time_sup.r.o.drift_adj.intervals; ix++) {
	ddp->intervals[ix].diff.mon = mtime_diff;
	ddp->intervals[ix].diff.sys = stime_diff;
	ddp->intervals[ix].time.mon = old_mtime;
	ddp->intervals[ix].time.sys = old_stime;
    }

    ddp->acc.sys = stime_diff*time_sup.r.o.drift_adj.intervals;
    ddp->acc.mon = mtime_diff*time_sup.r.o.drift_adj.intervals;
    ddp->ix = 0;
    ddp->dirty_counter = time_sup.r.o.drift_adj.intervals;

    check_time_correction(vesdp);
}

static ErtsMonotonicTime
finalize_corrected_time_offset(ErtsSystemTime *stimep)
{
    ErtsMonotonicTime os_mtime;
    ErtsMonotonicCorrectionInstance ci;
    int os_drift_corrected = time_sup.r.o.os_corrected_monotonic_time;

    erts_rwmtx_rlock(&time_sup.inf.c.parmon.rwmtx);

    erts_os_times(&os_mtime, stimep);

    ci = time_sup.inf.c.parmon.cdata.insts.curr;

    erts_rwmtx_runlock(&time_sup.inf.c.parmon.rwmtx);

    if (os_mtime < ci.os_mtime)
	erts_exit(ERTS_ABORT_EXIT,
		 "OS monotonic time stepped backwards\n");

    return calc_corrected_erl_mtime(os_mtime, &ci, NULL,
				    os_drift_corrected);
}

static void
late_init_time_correction(ErtsSchedulerData *esdp)
{
    int quick_init_drift_adj;
    void (*check_func)(void *);
    ErtsMonotonicTime timeout, timeout_pos;

    quick_init_drift_adj =
	ERTS_MONOTONIC_TO_USEC(time_sup.r.o.drift_adj.error) == 0;

    if (quick_init_drift_adj)
	timeout = ERTS_MONOTONIC_TO_MSEC(ERTS_SHORT_TIME_CORRECTION_CHECK/10);
    else
	timeout = ERTS_MONOTONIC_TO_MSEC(ERTS_SHORT_TIME_CORRECTION_CHECK);

    if (!time_sup.r.o.os_corrected_monotonic_time)
	check_func = init_check_time_correction;
    else if (time_sup.r.o.get_time == get_os_corrected_time) {
	quick_init_drift_adj = 0;
	check_func = check_time_offset;
    }
    else
	check_func = check_time_correction;

    timeout_pos = get_timeout_pos(erts_get_monotonic_time(esdp),
				  timeout);

    erts_twheel_init_timer(&time_sup.inf.c.parmon.timer);
    erts_twheel_set_timer(esdp->timer_wheel,
			  &time_sup.inf.c.parmon.timer,
			  check_func,
			  (quick_init_drift_adj
			   ? NULL
			   : esdp),
			  timeout_pos);
}

#endif /* ERTS_HAVE_OS_MONOTONIC_TIME_SUPPORT */

static ErtsMonotonicTime get_not_corrected_time(void)
{
    ErtsMonotonicTime stime, mtime;

    erts_mtx_lock(&erts_get_time_mtx);

    stime = erts_os_system_time();

    mtime = stime - time_sup.inf.c.not_corrected_moffset;

    if (mtime >= time_sup.f.c.last_not_corrected_time)
	time_sup.f.c.last_not_corrected_time = mtime;
    else {
	mtime = time_sup.f.c.last_not_corrected_time;

	if (time_sup.r.o.warp_mode == ERTS_MULTI_TIME_WARP_MODE) {
	    ErtsMonotonicTime new_offset = stime - mtime;
	    new_offset = ERTS_MONOTONIC_TO_USEC(new_offset);
	    new_offset = ERTS_USEC_TO_MONOTONIC(new_offset);
	    if (time_sup.inf.c.not_corrected_moffset != new_offset) {
		time_sup.inf.c.not_corrected_moffset = new_offset;
		set_time_offset(new_offset);
		schedule_send_time_offset_changed_notifications(new_offset);
	    }
	}

    }

    ASSERT(stime == mtime + time_sup.inf.c.not_corrected_moffset);

    erts_mtx_unlock(&erts_get_time_mtx);

    return mtime;
}

int erts_check_time_adj_support(int time_correction,
				ErtsTimeWarpMode time_warp_mode)
{
    if (!time_correction)
	return 1;

    /* User wants time correction */

#ifdef ERTS_HAVE_OS_MONOTONIC_TIME_SUPPORT
    return !time_sup.r.o.os_monotonic_time_disable;
#else
    return 0;
#endif
}

int
erts_has_time_correction(void)
{
    return time_sup.r.o.correction;
}

void erts_init_sys_time_sup(void)
{
    ErtsSysInitTimeResult sys_init_time_res
	= ERTS_SYS_INIT_TIME_RESULT_INITER;

    sys_init_time(&sys_init_time_res);

    erts_time_sup__.r.o.monotonic_time_unit
	= sys_init_time_res.os_monotonic_time_unit;

#ifndef SYS_CLOCK_RESOLUTION
    erts_time_sup__.r.o.clktck_resolution
	= sys_init_time_res.sys_clock_resolution;
    erts_time_sup__.r.o.clktck_resolution *= 1000;
#endif

#ifdef ERTS_HAVE_OS_MONOTONIC_TIME_SUPPORT
    time_sup.r.o.os_monotonic_time_disable
	= !sys_init_time_res.have_os_monotonic_time;
    time_sup.r.o.os_corrected_monotonic_time =
	sys_init_time_res.have_corrected_os_monotonic_time;
    time_sup.r.o.os_monotonic_time_func
	= sys_init_time_res.os_monotonic_time_info.func;
    time_sup.r.o.os_monotonic_time_clock_id
	= sys_init_time_res.os_monotonic_time_info.clock_id;
    time_sup.r.o.os_monotonic_time_locked
	= sys_init_time_res.os_monotonic_time_info.locked_use;
    time_sup.r.o.os_monotonic_time_resolution
	= sys_init_time_res.os_monotonic_time_info.resolution;
    time_sup.r.o.os_monotonic_time_extended
	= sys_init_time_res.os_monotonic_time_info.extended;
#endif
    time_sup.r.o.os_system_time_func
	= sys_init_time_res.os_system_time_info.func;
    time_sup.r.o.os_system_time_clock_id
	= sys_init_time_res.os_system_time_info.clock_id;
    time_sup.r.o.os_system_time_locked
	= sys_init_time_res.os_system_time_info.locked_use;
    time_sup.r.o.os_system_time_resolution
	= sys_init_time_res.os_system_time_info.resolution;
}

int 
erts_init_time_sup(int time_correction, ErtsTimeWarpMode time_warp_mode)
{
    ErtsMonotonicTime resolution, ilength, intervals, short_isecs;
#if !ERTS_COMPILE_TIME_MONOTONIC_TIME_UNIT
    ErtsMonotonicTime abs_native_offset, native_offset;
#endif

    init_time_napi();

    erts_hl_timer_init();

    ASSERT(ERTS_MONOTONIC_TIME_MIN < ERTS_MONOTONIC_TIME_MAX);

    erts_mtx_init(&erts_get_time_mtx, "get_time", NIL,
        ERTS_LOCK_FLAGS_PROPERTY_STATIC | ERTS_LOCK_FLAGS_CATEGORY_GENERIC);
    erts_mtx_init(&runtime_prev.data.mtx, "runtime", NIL,
        ERTS_LOCK_FLAGS_PROPERTY_STATIC | ERTS_LOCK_FLAGS_CATEGORY_GENERIC);
    runtime_prev.data.user = 0;
    runtime_prev.data.sys = 0;

    time_sup.r.o.correction = time_correction;
    time_sup.r.o.warp_mode = time_warp_mode;
 
    if (time_warp_mode == ERTS_SINGLE_TIME_WARP_MODE)
	erts_atomic32_init_nob(&time_sup.inf.c.preliminary_offset, 1);
    else
	erts_atomic32_init_nob(&time_sup.inf.c.preliminary_offset, 0);
    time_sup.inf.c.shadow_offset = 0;

#if !ERTS_COMPILE_TIME_MONOTONIC_TIME_UNIT

    /*
     * NOTE! erts_time_sup__.r.o.start *need* to be a multiple
     *       of ERTS_MONOTONIC_TIME_UNIT.
     */

#ifdef ARCH_32
    erts_time_sup__.r.o.start = ((((ErtsMonotonicTime) 1) << 32)-1);
    erts_time_sup__.r.o.start /= ERTS_MONOTONIC_TIME_UNIT;
    erts_time_sup__.r.o.start *= ERTS_MONOTONIC_TIME_UNIT;
    erts_time_sup__.r.o.start += ERTS_MONOTONIC_TIME_UNIT;
    native_offset = erts_time_sup__.r.o.start - ERTS_MONOTONIC_BEGIN;
    abs_native_offset = native_offset;
#else /* ARCH_64 */
    if (ERTS_MONOTONIC_TIME_UNIT <= 10*1000*1000) {
	erts_time_sup__.r.o.start = 0;
	native_offset = -ERTS_MONOTONIC_BEGIN;
	abs_native_offset = ERTS_MONOTONIC_BEGIN;
    }
    else {
	erts_time_sup__.r.o.start = ((ErtsMonotonicTime) MIN_SMALL);
	erts_time_sup__.r.o.start /= ERTS_MONOTONIC_TIME_UNIT;
	erts_time_sup__.r.o.start *= ERTS_MONOTONIC_TIME_UNIT;
	native_offset = erts_time_sup__.r.o.start - ERTS_MONOTONIC_BEGIN;
	abs_native_offset = -1*native_offset;
    }
#endif

    erts_time_sup__.r.o.start_offset.native = native_offset;
    erts_time_sup__.r.o.start_offset.nsec = (ErtsMonotonicTime)
	erts_time_unit_conversion((Uint64) abs_native_offset,
				  (Uint32) ERTS_MONOTONIC_TIME_UNIT,
				  (Uint32) 1000*1000*1000);
    erts_time_sup__.r.o.start_offset.usec = (ErtsMonotonicTime)
	erts_time_unit_conversion((Uint64) abs_native_offset,
				  (Uint32) ERTS_MONOTONIC_TIME_UNIT,
				  (Uint32) 1000*1000);
    erts_time_sup__.r.o.start_offset.msec = (ErtsMonotonicTime)
	erts_time_unit_conversion((Uint64) abs_native_offset,
				  (Uint32) ERTS_MONOTONIC_TIME_UNIT,
				  (Uint32) 1000);
    erts_time_sup__.r.o.start_offset.sec = (ErtsMonotonicTime)
	erts_time_unit_conversion((Uint64) abs_native_offset,
				  (Uint32) ERTS_MONOTONIC_TIME_UNIT,
				  (Uint32) 1);
    if (native_offset < 0) {
	erts_time_sup__.r.o.start_offset.nsec *= -1;
	erts_time_sup__.r.o.start_offset.usec *= -1;
	erts_time_sup__.r.o.start_offset.msec *= -1;
	erts_time_sup__.r.o.start_offset.sec *= -1;
    }

#endif

    resolution = time_sup.r.o.os_system_time_resolution;
#ifdef ERTS_HAVE_OS_MONOTONIC_TIME_SUPPORT
    if (resolution > time_sup.r.o.os_monotonic_time_resolution)
	resolution = time_sup.r.o.os_monotonic_time_resolution;
#endif

    time_sup.r.o.adj.large_diff = erts_time_sup__.r.o.monotonic_time_unit;
    time_sup.r.o.adj.large_diff *= 50;
    time_sup.r.o.adj.large_diff /= resolution;
    if (time_sup.r.o.adj.large_diff < ERTS_USEC_TO_MONOTONIC(500))
	time_sup.r.o.adj.large_diff = ERTS_USEC_TO_MONOTONIC(500);
    time_sup.r.o.adj.small_diff = time_sup.r.o.adj.large_diff/10;

    time_sup.r.o.drift_adj.resolution = resolution;

    if (time_sup.r.o.os_corrected_monotonic_time) {
	time_sup.r.o.drift_adj.use_avg = 0;
	time_sup.r.o.drift_adj.intervals = 0;
	time_sup.r.o.drift_adj.error = 0;
	time_sup.inf.c.parmon.cdata.drift.dirty_counter = -1;
    }
    else {
	/*
	 * Calculate length of the interval in seconds needed
	 * in order to get an error that is at most 1 micro second.
	 * If this interval is longer than the short time correction
	 * check interval we use the average of all values instead
	 * of the latest value.
	 */
	short_isecs = ERTS_MONOTONIC_TO_SEC(ERTS_SHORT_TIME_CORRECTION_CHECK);
	ilength = ERTS_ASSUMED_PRECISION_DROP * ERTS_MONOTONIC_TIME_UNIT;
	ilength /= (resolution * ERTS_USEC_TO_MONOTONIC(1));
	time_sup.r.o.drift_adj.use_avg = ilength > short_isecs;

	if (ilength == 0)
	    intervals = 5;
	else {
	    intervals = ilength / short_isecs;
	    if (intervals > ERTS_MAX_DRIFT_INTERVALS)
		intervals = ERTS_MAX_DRIFT_INTERVALS;
	    else if (intervals < 5)
		intervals = 5;
	}
	time_sup.r.o.drift_adj.intervals = (int) intervals;

	/*
	 * drift_adj.error equals maximum assumed error
	 * over a short time interval. We use this value also
	 * when examining a large interval. In this case the
	 * error will be smaller, but we do not want to
	 * recalculate this over and over again.
	 */

	time_sup.r.o.drift_adj.error = ERTS_MONOTONIC_TIME_UNIT;
	time_sup.r.o.drift_adj.error *= ERTS_ASSUMED_PRECISION_DROP;
	time_sup.r.o.drift_adj.error /= resolution * short_isecs;
    }
#ifdef ERTS_TIME_CORRECTION_PRINT
    erts_fprintf(stderr, "resolution           = %b64d\n", resolution);
    erts_fprintf(stderr, "adj large diff       = %b64d usec\n",
		 ERTS_MONOTONIC_TO_USEC(time_sup.r.o.adj.large_diff));
    erts_fprintf(stderr, "adj small diff       = %b64d usec\n",
		 ERTS_MONOTONIC_TO_USEC(time_sup.r.o.adj.small_diff));
    if (!time_sup.r.o.os_corrected_monotonic_time) {
	erts_fprintf(stderr, "drift intervals      = %d\n",
		     time_sup.r.o.drift_adj.intervals);
	erts_fprintf(stderr, "drift adj error      = %b64d usec\n",
		     ERTS_MONOTONIC_TO_USEC(time_sup.r.o.drift_adj.error));
	erts_fprintf(stderr, "drift adj max diff   = %b64d nsec\n",
		     ERTS_MONOTONIC_TO_NSEC(ERTS_TIME_DRIFT_MAX_ADJ_DIFF));
	erts_fprintf(stderr, "drift adj min diff   = %b64d nsec\n",
		     ERTS_MONOTONIC_TO_NSEC(ERTS_TIME_DRIFT_MIN_ADJ_DIFF));
    }
#endif

    if (ERTS_MONOTONIC_TIME_UNIT < ERTS_CLKTCK_RESOLUTION)
	ERTS_INTERNAL_ERROR("Too small monotonic time time unit");

#ifndef ERTS_HAVE_OS_MONOTONIC_TIME_SUPPORT
    time_sup.r.o.correction = 0;
#else
    if (time_sup.r.o.os_monotonic_time_disable)
	time_sup.r.o.correction = 0;

    if (time_sup.r.o.correction) {
	ErtsMonotonicCorrectionData *cdatap;
	erts_rwmtx_opt_t rwmtx_opts = ERTS_RWMTX_OPT_DEFAULT_INITER;
	ErtsMonotonicTime offset;
	erts_os_times(&time_sup.inf.c.minit,
		      &time_sup.inf.c.sinit);
	time_sup.r.o.moffset = -1*time_sup.inf.c.minit;
	time_sup.r.o.moffset += ERTS_MONOTONIC_BEGIN;
	offset = time_sup.inf.c.sinit;
	offset -= ERTS_MONOTONIC_BEGIN;
	init_time_offset(offset);

	rwmtx_opts.type = ERTS_RWMTX_TYPE_EXTREMELY_FREQUENT_READ;
	rwmtx_opts.lived = ERTS_RWMTX_LONG_LIVED;

        erts_rwmtx_init_opt(&time_sup.inf.c.parmon.rwmtx, &rwmtx_opts,
            "get_corrected_time", NIL,
            ERTS_LOCK_FLAGS_PROPERTY_STATIC | ERTS_LOCK_FLAGS_CATEGORY_GENERIC);

	cdatap = &time_sup.inf.c.parmon.cdata;
    
	cdatap->drift.intervals[0].time.sys = time_sup.inf.c.sinit;
	cdatap->drift.intervals[0].time.mon = time_sup.inf.c.minit;
	cdatap->insts.curr.correction.drift = 0;
	cdatap->insts.curr.correction.error = 0;
	cdatap->insts.curr.erl_mtime = ERTS_MONOTONIC_BEGIN;
	cdatap->insts.curr.os_mtime = time_sup.inf.c.minit;
	cdatap->last_check = time_sup.inf.c.minit;
	cdatap->short_check_interval = ERTS_INIT_SHORT_INTERVAL_COUNTER;
	cdatap->insts.prev = cdatap->insts.curr;

	if (!time_sup.r.o.os_corrected_monotonic_time)
	    time_sup.r.o.get_time = get_corrected_time;
	else if (time_sup.r.o.warp_mode == ERTS_MULTI_TIME_WARP_MODE)
	    time_sup.r.o.get_time = get_os_corrected_time;
	else
	    time_sup.r.o.get_time = get_os_drift_corrected_time;
    }
    else
#endif
    {
	ErtsMonotonicTime stime, offset;
	time_sup.r.o.get_time = get_not_corrected_time;
	stime = time_sup.inf.c.sinit = erts_os_system_time();
	offset = stime - ERTS_MONOTONIC_BEGIN;
	time_sup.inf.c.not_corrected_moffset = offset;
	init_time_offset(offset);
	time_sup.f.c.last_not_corrected_time = 0;
    }

    erts_atomic64_init_nob(&wall_clock_prev.time,
                           (erts_aint64_t) 0);

    erts_atomic64_init_nob(
        &now_prev.time,
        (erts_aint64_t) ERTS_MONOTONIC_TO_USEC(get_time_offset()));


#ifdef DEBUG
    time_sup_initialized = 1;
#endif

    return ERTS_CLKTCK_RESOLUTION/1000;
}    

void
erts_late_init_time_sup(void)
{
    erts_late_sys_init_time();
}

void
erts_sched_init_time_sup(ErtsSchedulerData *esdp)
{
    esdp->timer_wheel = erts_create_timer_wheel(esdp);
    esdp->next_tmo_ref = erts_get_next_timeout_reference(esdp->timer_wheel);
    esdp->timer_service = erts_create_timer_service();
#ifdef ERTS_HAVE_OS_MONOTONIC_TIME_SUPPORT
    if (esdp->no == 1) {
	/* A timer wheel to use must have beeen initialized */
	if (time_sup.r.o.get_time != get_not_corrected_time)
	    late_init_time_correction(esdp);
    }
#endif	
}

ErtsTimeWarpMode erts_time_warp_mode(void)
{
    return time_sup.r.o.warp_mode;
}

ErtsTimeOffsetState erts_time_offset_state(void)
{
    switch (time_sup.r.o.warp_mode) {
    case ERTS_NO_TIME_WARP_MODE:
	return ERTS_TIME_OFFSET_FINAL;
    case ERTS_SINGLE_TIME_WARP_MODE:
	if (erts_atomic32_read_nob(&time_sup.inf.c.preliminary_offset))
	    return ERTS_TIME_OFFSET_PRELIMINARY;
	return ERTS_TIME_OFFSET_FINAL;
    case ERTS_MULTI_TIME_WARP_MODE:
	return ERTS_TIME_OFFSET_VOLATILE;
    default:
	ERTS_INTERNAL_ERROR("Invalid time warp mode");
	return ERTS_TIME_OFFSET_VOLATILE;
    }
}

/*
 * erts_finalize_time_offset() will only change time offset
 * the first time it is called when the emulator has been
 * started in "single time warp" mode. Returns previous
 * state:
 * * ERTS_TIME_OFFSET_PRELIMINARY - Finalization performed
 * * ERTS_TIME_OFFSET_FINAL - Already finialized; nothing changed
 * * ERTS_TIME_OFFSET_VOLATILE - Not supported, either in
 * * no correction mode (or multi time warp mode; not yet implemented).
 */

ErtsTimeOffsetState
erts_finalize_time_offset(void)
{
    switch (time_sup.r.o.warp_mode) {
    case ERTS_NO_TIME_WARP_MODE:
	return ERTS_TIME_OFFSET_FINAL;
    case ERTS_MULTI_TIME_WARP_MODE:
	return ERTS_TIME_OFFSET_VOLATILE;
    case ERTS_SINGLE_TIME_WARP_MODE: {
	ErtsTimeOffsetState res = ERTS_TIME_OFFSET_FINAL;

	erts_mtx_lock(&erts_get_time_mtx);

	if (erts_atomic32_read_nob(&time_sup.inf.c.preliminary_offset)) {
	    ErtsMonotonicTime mtime, new_offset;

#ifdef ERTS_HAVE_OS_MONOTONIC_TIME_SUPPORT
	    if (!time_sup.r.o.correction)
#endif
	    {
		ErtsMonotonicTime stime = erts_os_system_time();

		mtime = stime - time_sup.inf.c.not_corrected_moffset;

		if (mtime >= time_sup.f.c.last_not_corrected_time) {
		    time_sup.f.c.last_not_corrected_time = mtime;
		    new_offset = time_sup.inf.c.not_corrected_moffset;
		}
		else {
		    mtime = time_sup.f.c.last_not_corrected_time;

		    ASSERT(time_sup.inf.c.not_corrected_moffset != stime - mtime);
		    new_offset = stime - mtime;
		    time_sup.inf.c.not_corrected_moffset = new_offset;
		}

	    }
#ifdef ERTS_HAVE_OS_MONOTONIC_TIME_SUPPORT
	    else {
		ErtsSystemTime stime;
		mtime = finalize_corrected_time_offset(&stime);
		new_offset = stime - mtime;
	    }
#endif
	    new_offset = ERTS_MONOTONIC_TO_USEC(new_offset);
	    new_offset = ERTS_USEC_TO_MONOTONIC(new_offset);

	    set_time_offset(new_offset);
	    schedule_send_time_offset_changed_notifications(new_offset);

	    erts_atomic32_set_nob(&time_sup.inf.c.preliminary_offset, 0);
	    res = ERTS_TIME_OFFSET_PRELIMINARY;
	}

	erts_mtx_unlock(&erts_get_time_mtx);

	return res;
    }
    default:
	ERTS_INTERNAL_ERROR("Invalid time warp mode");
	return ERTS_TIME_OFFSET_VOLATILE;
    }
}

/* info functions */

void 
erts_runtime_elapsed_both(ErtsMonotonicTime *ms_user, ErtsMonotonicTime *ms_sys, 
                          ErtsMonotonicTime *ms_user_diff, ErtsMonotonicTime *ms_sys_diff)
{
    ErtsMonotonicTime prev_user, prev_sys, user, sys;

#ifdef HAVE_GETRUSAGE

    struct rusage now;

    if (getrusage(RUSAGE_SELF, &now) != 0) {
        erts_exit(ERTS_ABORT_EXIT, "getrusage(RUSAGE_SELF, _) failed: %d\n", errno);
	return;
    }

    user = (ErtsMonotonicTime) now.ru_utime.tv_sec;
    user *= (ErtsMonotonicTime) 1000000;
    user += (ErtsMonotonicTime) now.ru_utime.tv_usec;
    user /= (ErtsMonotonicTime) 1000;

    sys = (ErtsMonotonicTime) now.ru_stime.tv_sec;
    sys *= (ErtsMonotonicTime) 1000000;
    sys += (ErtsMonotonicTime) now.ru_stime.tv_usec;
    sys /= (ErtsMonotonicTime) 1000;

#else

    SysTimes now;

    sys_times(&now);
    user = (ErtsMonotonicTime) now.tms_utime;
    user *= (ErtsMonotonicTime) 1000;
    user /= (ErtsMonotonicTime) SYS_CLK_TCK;

    sys = (ErtsMonotonicTime) now.tms_stime;
    sys *= (ErtsMonotonicTime) 1000;
    sys /= (ErtsMonotonicTime) SYS_CLK_TCK;

#endif

    if (ms_user)
	*ms_user = user;
    if (ms_sys)
	*ms_sys = sys;

    if (ms_user_diff || ms_sys_diff) {
 
        erts_mtx_lock(&runtime_prev.data.mtx);

        prev_user = runtime_prev.data.user;
        prev_sys = runtime_prev.data.sys;
        runtime_prev.data.user = user;
        runtime_prev.data.sys = sys;

        erts_mtx_unlock(&runtime_prev.data.mtx);

        if (ms_user_diff)
            *ms_user_diff = user - prev_user;
        if (ms_sys_diff)
            *ms_sys_diff = sys - prev_sys;
    }
}


/* wall clock routines */

void 
erts_wall_clock_elapsed_both(ErtsMonotonicTime *ms_total, ErtsMonotonicTime *ms_diff)
{
    ErtsMonotonicTime now, elapsed;

    now = time_sup.r.o.get_time();
    update_last_mtime(NULL, now);

    elapsed = ERTS_MONOTONIC_TO_MSEC(now);
    elapsed -= ERTS_MONOTONIC_TO_MSEC(ERTS_MONOTONIC_BEGIN);

    *ms_total = elapsed;

    if (ms_diff) {
        ErtsMonotonicTime prev;

        prev = ((ErtsMonotonicTime)
                erts_atomic64_xchg_mb(&wall_clock_prev.time,
                                      (erts_aint64_t) elapsed));

        *ms_diff = elapsed - prev;
    }
}

/* get current time */
void 
get_time(int *hour, int *minute, int *second)
{
    time_t the_clock;
    struct tm *tm;
#ifdef HAVE_LOCALTIME_R
    struct tm tmbuf;
#endif
    
    the_clock = time((time_t *)0);
#ifdef HAVE_LOCALTIME_R
    tm = localtime_r(&the_clock, &tmbuf);
#else
    tm = localtime(&the_clock);
#endif
    *hour = tm->tm_hour;
    *minute = tm->tm_min;
    *second = tm->tm_sec;
}

/* get current date */
void 
get_date(int *year, int *month, int *day)
{
    time_t the_clock;
    struct tm *tm;
#ifdef HAVE_LOCALTIME_R
    struct tm tmbuf;
#endif


    the_clock = time((time_t *)0);
#ifdef HAVE_LOCALTIME_R
    tm = localtime_r(&the_clock, &tmbuf);
#else
    tm = localtime(&the_clock);
#endif
    *year = tm->tm_year + 1900;
    *month = tm->tm_mon +1;
    *day = tm->tm_mday;
}

/* get localtime */
void 
get_localtime(int *year, int *month, int *day, 
	      int *hour, int *minute, int *second)
{
    time_t the_clock;
    struct tm *tm;
#ifdef HAVE_LOCALTIME_R
    struct tm tmbuf;
#endif

    the_clock = time((time_t *)0);
#ifdef HAVE_LOCALTIME_R
    localtime_r(&the_clock, (tm = &tmbuf));
#else
    tm = localtime(&the_clock);
#endif
    *year = tm->tm_year + 1900;
    *month = tm->tm_mon +1;
    *day = tm->tm_mday;
    *hour = tm->tm_hour;
    *minute = tm->tm_min;
    *second = tm->tm_sec;
}


/* get universaltime */
void 
get_universaltime(int *year, int *month, int *day, 
		  int *hour, int *minute, int *second)
{
    time_t the_clock;
    struct tm *tm;
#ifdef HAVE_GMTIME_R
    struct tm tmbuf;
#endif

    the_clock = time((time_t *)0);
#ifdef HAVE_GMTIME_R
    gmtime_r(&the_clock, (tm = &tmbuf));
#else
    tm = gmtime(&the_clock);
#endif
    *year = tm->tm_year + 1900;
    *month = tm->tm_mon +1;
    *day = tm->tm_mday;
    *hour = tm->tm_hour;
    *minute = tm->tm_min;
    *second = tm->tm_sec;
}


/* days in month = 1, 2, ..., 12 */
static const int mdays[14] = {0, 31, 28, 31, 30, 31, 30,
                                 31, 31, 30, 31, 30, 31};

#define  IN_RANGE(a,x,b)  (((a) <= (x)) && ((x) <= (b)))
#define  is_leap_year(y)  (((((y) % 4) == 0) && \
                            (((y) % 100) != 0)) || \
                           (((y) % 400) == 0))

/* This is the earliest year we are sure to be able to handle
   on all platforms w/o problems */
#define  BASEYEAR       1902 

/* A more "clever" mktime
 * return  1, if successful
 * return -1, if not successful
 */

static int erl_mktime(time_t *c, struct tm *tm) {
    time_t clock;

    clock = mktime(tm);

    if (clock != -1) {
	*c = clock;
	return 1;
    }

    /* in rare occasions mktime returns -1
     * when a correct value has been entered
     *
     * decrease seconds with one second
     * if the result is -2, epochs should be -1
     */

    tm->tm_sec = tm->tm_sec - 1;
    clock = mktime(tm);
    tm->tm_sec = tm->tm_sec + 1;

    *c = -1;

    if (clock == -2) {
	return 1;
    }

    return -1;
}

/*
 * gregday
 *
 * Returns the number of days since Jan 1, 1600, if year is
 * greater of equal to 1600 , and month [1-12] and day [1-31] 
 * are within range. Otherwise it returns -1.
 */
static time_t gregday(int year, int month, int day)
{
  Sint ndays = 0;
  Sint gyear, pyear, m;
  
  /* number of days in previous years */
  gyear = year - 1600;
  if (gyear > 0) {
    pyear = gyear - 1;
    ndays = (pyear/4) - (pyear/100) + (pyear/400) + pyear*365 + 366;
  }
  /* number of days in all months preceding month */
  for (m = 1; m < month; m++)
    ndays += mdays[m];
  /* Extra day if leap year and March or later */
  if (is_leap_year(year) && (month > 2))
    ndays++;
  ndays += day - 1;
  return (time_t) (ndays - 135140);        /* 135140 = Jan 1, 1970 */
}

#define SECONDS_PER_MINUTE  (60)
#define SECONDS_PER_HOUR    (60 * SECONDS_PER_MINUTE)
#define SECONDS_PER_DAY     (24 * SECONDS_PER_HOUR)

int seconds_to_univ(Sint64 time, Sint *year, Sint *month, Sint *day, 
	Sint *hour, Sint *minute, Sint *second) {

    Sint y,mi;
    Sint days = time / SECONDS_PER_DAY;
    Sint secs = time % SECONDS_PER_DAY;
    Sint tmp;

    if (secs < 0) {
	days--;
	secs += SECONDS_PER_DAY;
    }
    
    tmp     = secs % SECONDS_PER_HOUR;

    *hour   = secs / SECONDS_PER_HOUR;
    *minute = tmp  / SECONDS_PER_MINUTE;
    *second = tmp  % SECONDS_PER_MINUTE;

    days   += 719468;
    y       = (10000*((Sint64)days) + 14780) / 3652425; 
    tmp     = days - (365 * y + y/4 - y/100 + y/400);

    if (tmp < 0) {
	y--;
	tmp = days - (365*y + y/4 - y/100 + y/400);
    }
    mi = (100 * tmp + 52)/3060;
    *month = (mi + 2) % 12 + 1;
    *year  = y + (mi + 2) / 12;
    *day   = tmp - (mi * 306 + 5)/10 + 1;

    return 1;
}

int univ_to_seconds(Sint year, Sint month, Sint day, Sint hour, Sint minute, Sint second, Sint64 *time) {
    Sint days;

    if (!(IN_RANGE(1600, year, INT_MAX - 1) &&
          IN_RANGE(1, month, 12) &&
          IN_RANGE(1, day, (mdays[month] + 
                             (month == 2 
                              && (year % 4 == 0) 
                              && (year % 100 != 0 || year % 400 == 0)))) &&
          IN_RANGE(0, hour, 23) &&
          IN_RANGE(0, minute, 59) &&
          IN_RANGE(0, second, 59))) {
      return 0;
    }
 
    days   = gregday(year, month, day);
    *time  = SECONDS_PER_DAY;
    *time *= days;             /* don't try overflow it, it hurts */
    *time += SECONDS_PER_HOUR * hour;
    *time += SECONDS_PER_MINUTE * minute;
    *time += second;

    return 1;
}

#if defined(HAVE_TIME2POSIX) && defined(HAVE_DECL_TIME2POSIX) && \
    !HAVE_DECL_TIME2POSIX
extern time_t time2posix(time_t);
#endif

int 
local_to_univ(Sint *year, Sint *month, Sint *day, 
	      Sint *hour, Sint *minute, Sint *second, int isdst)
{
    time_t the_clock;
    struct tm *tm, t;
#ifdef HAVE_GMTIME_R
    struct tm tmbuf;
#endif
    
    if (!(IN_RANGE(BASEYEAR, *year, INT_MAX - 1) &&
          IN_RANGE(1, *month, 12) &&
          IN_RANGE(1, *day, (mdays[*month] + 
                             (*month == 2 
                              && (*year % 4 == 0) 
                              && (*year % 100 != 0 || *year % 400 == 0)))) &&
          IN_RANGE(0, *hour, 23) &&
          IN_RANGE(0, *minute, 59) &&
          IN_RANGE(0, *second, 59))) {
      return 0;
    }
    
    t.tm_year = *year - 1900;
    t.tm_mon = *month - 1;
    t.tm_mday = *day;
    t.tm_hour = *hour;
    t.tm_min = *minute;
    t.tm_sec = *second;
    t.tm_isdst = isdst;

    /* the nature of mktime makes this a bit interesting,
     * up to four mktime calls could happen here
     */

    if (erl_mktime(&the_clock, &t) < 0) {
	if (isdst) {
	    /* If this is a timezone without DST and the OS (correctly)
	       refuses to give us a DST time, we simulate the Linux/Solaris
	       behaviour of giving the same data as if is_dst was not set. */
	    t.tm_isdst = 0;
	    if (erl_mktime(&the_clock, &t) < 0) {
		/* Failed anyway, something else is bad - will be a badarg */
		return 0;
	    }
	} else {
	    /* Something else is the matter, badarg. */
	    return 0;
	}
    }

#ifdef HAVE_TIME2POSIX
    the_clock = time2posix(the_clock);
#endif

#ifdef HAVE_GMTIME_R
    tm = gmtime_r(&the_clock, &tmbuf);
#else
    tm = gmtime(&the_clock);
#endif
    if (!tm) {
      return 0;
    }
    *year = tm->tm_year + 1900;
    *month = tm->tm_mon +1;
    *day = tm->tm_mday;
    *hour = tm->tm_hour;
    *minute = tm->tm_min;
    *second = tm->tm_sec;
    return 1;
}
#if defined(HAVE_POSIX2TIME) && defined(HAVE_DECL_POSIX2TIME) && \
    !HAVE_DECL_POSIX2TIME
extern time_t posix2time(time_t);
#endif

int 
univ_to_local(Sint *year, Sint *month, Sint *day, 
	      Sint *hour, Sint *minute, Sint *second)
{
    time_t the_clock;
    struct tm *tm;
#ifdef HAVE_LOCALTIME_R
    struct tm tmbuf;
#endif
    
    if (!(IN_RANGE(BASEYEAR, *year, INT_MAX - 1) &&
          IN_RANGE(1, *month, 12) &&
          IN_RANGE(1, *day, (mdays[*month] + 
                             (*month == 2 
                              && (*year % 4 == 0) 
                              && (*year % 100 != 0 || *year % 400 == 0)))) &&
          IN_RANGE(0, *hour, 23) &&
          IN_RANGE(0, *minute, 59) &&
          IN_RANGE(0, *second, 59))) {
      return 0;
    }
    
    the_clock = *second + 60 * (*minute + 60 * (*hour + 24 *
                                            gregday(*year, *month, *day)));
#ifdef HAVE_POSIX2TIME
    /*
     * Addition from OpenSource - affects FreeBSD.
     * No valid test case /PaN
     *
     * leap-second correction performed
     * if system is configured so;
     * do nothing if not
     * See FreeBSD 6.x and 7.x
     * /usr/src/lib/libc/stdtime/localtime.c
     * for the details
     */
    the_clock = posix2time(the_clock);
#endif

#ifdef HAVE_LOCALTIME_R
    tm = localtime_r(&the_clock, &tmbuf);
#else
    tm = localtime(&the_clock);
#endif
    if (tm) {
	*year   = tm->tm_year + 1900;
	*month  = tm->tm_mon +1;
	*day    = tm->tm_mday;
	*hour   = tm->tm_hour;
	*minute = tm->tm_min;
	*second = tm->tm_sec;
	return 1;
    }
    return 0;
}

/* get a timestamp */
void
get_now(Uint* megasec, Uint* sec, Uint* microsec)
{
    ErtsMonotonicTime now_megasec, now_sec, now, prev, mtime, time_offset;
    
    mtime = time_sup.r.o.get_time();
    time_offset = get_time_offset();
    update_last_mtime(NULL, mtime);
    now = ERTS_MONOTONIC_TO_USEC(mtime + time_offset);

    /* Make sure now time is later than last time */
    prev = erts_atomic64_read_nob(&now_prev.time);
    while (1) {
        ErtsMonotonicTime act;
        if (now <= prev)
            now = prev + 1;
        act = ((ErtsMonotonicTime)
               erts_atomic64_cmpxchg_mb(&now_prev.time,
                                        (erts_aint64_t) now,
                                        (erts_aint64_t) prev));
        if (act == prev)
            break;
        prev = act;
    }

    now_megasec = now / ERTS_MONOTONIC_TIME_TERA;
    now_sec = now / ERTS_MONOTONIC_TIME_MEGA;
    *megasec = (Uint) now_megasec;
    *sec = (Uint) (now_sec - now_megasec*ERTS_MONOTONIC_TIME_MEGA);
    *microsec = (Uint) (now - now_sec*ERTS_MONOTONIC_TIME_MEGA);

    ASSERT(((ErtsMonotonicTime) *megasec)*ERTS_MONOTONIC_TIME_TERA
	   + ((ErtsMonotonicTime) *sec)*ERTS_MONOTONIC_TIME_MEGA
	   + ((ErtsMonotonicTime) *microsec) == now);
}

ErtsMonotonicTime
erts_get_monotonic_time(ErtsSchedulerData *esdp)
{
    ErtsMonotonicTime mtime = time_sup.r.o.get_time();
    update_last_mtime(esdp, mtime);
    return mtime;
}

ErtsMonotonicTime
erts_get_time_offset(void)
{
    return get_time_offset();
}

static ERTS_INLINE void
make_timestamp_value(Uint* megasec, Uint* sec, Uint* microsec,
		     ErtsMonotonicTime mtime, ErtsMonotonicTime offset)
{
    ErtsMonotonicTime stime, as;
    Uint ms;

    stime = ERTS_MONOTONIC_TO_USEC(mtime + offset);

    as = stime / ERTS_MONOTONIC_TIME_MEGA;
    *megasec = ms = (Uint) (stime / ERTS_MONOTONIC_TIME_TERA);
    *sec = (Uint) (as - (((ErtsMonotonicTime) ms)
			 * ERTS_MONOTONIC_TIME_MEGA));
    *microsec = (Uint) (stime - as*ERTS_MONOTONIC_TIME_MEGA);

    ASSERT(((ErtsMonotonicTime) ms)*ERTS_MONOTONIC_TIME_TERA
	   + ((ErtsMonotonicTime) *sec)*ERTS_MONOTONIC_TIME_MEGA
	   + *microsec == stime);
}

void
erts_make_timestamp_value(Uint* megasec, Uint* sec, Uint* microsec,
			  ErtsMonotonicTime mtime, ErtsMonotonicTime offset)
{
    make_timestamp_value(megasec, sec, microsec, mtime, offset);
}

void
get_sys_now(Uint* megasec, Uint* sec, Uint* microsec)
{
    ErtsSystemTime stime = erts_os_system_time();
    ErtsSystemTime ms, s, us;

    us = ERTS_MONOTONIC_TO_USEC(stime);
    s = us / (1000*1000);
    ms = s / (1000*1000);

    *megasec = (Uint) ms;
    *sec = (Uint) (s - ms*(1000*1000));
    *microsec = (Uint) (us - s*(1000*1000));
}

#ifdef HAVE_ERTS_NOW_CPU
void erts_get_now_cpu(Uint* megasec, Uint* sec, Uint* microsec) {
  SysCpuTime t;
  SysTimespec tp;

  sys_get_proc_cputime(t, tp);
  *microsec = (Uint)(tp.tv_nsec / 1000);
  t = (tp.tv_sec / 1000000);
  *megasec = (Uint)(t % 1000000);
  *sec = (Uint)(tp.tv_sec % 1000000);
}
#endif

#include "big.h"

void
erts_monitor_time_offset(ErtsMonitor *mon)
{
    erts_mtx_lock(&erts_get_time_mtx);
    erts_monitor_list_insert(&time_offset_monitors, mon);
    no_time_offset_monitors++;
    erts_mtx_unlock(&erts_get_time_mtx);
}

void
erts_demonitor_time_offset(ErtsMonitor *mon)
{
    ErtsMonitorData *mdp = erts_monitor_to_data(mon);
    ASSERT(erts_monitor_is_origin(mon));
    ASSERT(mon->type == ERTS_MON_TYPE_TIME_OFFSET);

    erts_mtx_lock(&erts_get_time_mtx);

    ASSERT(erts_monitor_is_in_table(&mdp->target));

    erts_monitor_list_delete(&time_offset_monitors, &mdp->target);

    ASSERT(no_time_offset_monitors > 0);
    no_time_offset_monitors--;

    erts_mtx_unlock(&erts_get_time_mtx);

    erts_monitor_release_both(mdp);
}

typedef struct {
    Eterm pid;
    Eterm ref;
    Eterm heap[ERTS_REF_THING_SIZE];
} ErtsTimeOffsetMonitorInfo;

typedef struct {
    Uint ix;
    ErtsTimeOffsetMonitorInfo *to_mon_info;
} ErtsTimeOffsetMonitorContext;

static void
save_time_offset_monitor(ErtsMonitor *mon, void *vcntxt)
{
    ErtsTimeOffsetMonitorContext *cntxt;
    ErtsMonitorData *mdp = erts_monitor_to_data(mon);
    Eterm *from_hp, *to_hp;
    Uint mix;
    int hix;

    cntxt = (ErtsTimeOffsetMonitorContext *) vcntxt;
    mix = (cntxt->ix)++;
    ASSERT(is_internal_pid(mon->other.item));
    cntxt->to_mon_info[mix].pid = mon->other.item;
    to_hp = &cntxt->to_mon_info[mix].heap[0];

    ASSERT(is_internal_ordinary_ref(mdp->ref));
    from_hp = internal_ref_val(mdp->ref);
    ASSERT(thing_arityval(*from_hp) + 1 == ERTS_REF_THING_SIZE);

    for (hix = 0; hix < ERTS_REF_THING_SIZE; hix++)
	to_hp[hix] = from_hp[hix];

    cntxt->to_mon_info[mix].ref
	= make_internal_ref(&cntxt->to_mon_info[mix].heap[0]);

}

static void
send_time_offset_changed_notifications(void *new_offsetp)
{
    ErtsMonotonicTime new_offset;
    ErtsTimeOffsetMonitorInfo *to_mon_info = NULL; /* Shut up faulty warning */
    Uint no_monitors;
    char *tmp = NULL;

#ifdef ARCH_64
    new_offset = (ErtsMonotonicTime) new_offsetp;
#else
    new_offset = *((ErtsMonotonicTime *) new_offsetp);
    erts_free(ERTS_ALC_T_NEW_TIME_OFFSET, new_offsetp);
#endif
    new_offset -= ERTS_MONOTONIC_OFFSET_NATIVE;

    erts_mtx_lock(&erts_get_time_mtx);

    no_monitors = no_time_offset_monitors;
    if (no_monitors) {
	ErtsTimeOffsetMonitorContext cntxt;
	Uint alloc_sz;
	
	/* Monitor info array size */
	alloc_sz = no_monitors*sizeof(ErtsTimeOffsetMonitorInfo);
	/* + template max size */
	alloc_sz += 6*sizeof(Eterm); /* 5-tuple */
	alloc_sz += ERTS_MAX_SINT64_HEAP_SIZE*sizeof(Eterm); /* max offset size */
	tmp = erts_alloc(ERTS_ALC_T_TMP, alloc_sz);

	to_mon_info = (ErtsTimeOffsetMonitorInfo *) tmp;
	cntxt.ix = 0;
	cntxt.to_mon_info = to_mon_info;

        erts_monitor_list_foreach(time_offset_monitors,
                                  save_time_offset_monitor,
                                  &cntxt);

	ASSERT(cntxt.ix == no_monitors);
    }

    erts_mtx_unlock(&erts_get_time_mtx);

    if (no_monitors) {
	Eterm *hp, *patch_refp, new_offset_term, message_template;
	Uint mix, hsz;

	/* Make message template */

	hp = (Eterm *) (tmp + no_monitors*sizeof(ErtsTimeOffsetMonitorInfo));

	hsz = 6; /* 5-tuple */
	hsz += ERTS_REF_THING_SIZE;
	hsz += ERTS_SINT64_HEAP_SIZE(new_offset);

	if (IS_SSMALL(new_offset))
	    new_offset_term = make_small(new_offset);
	else
	    new_offset_term = erts_sint64_to_big(new_offset, &hp);
	message_template = TUPLE5(hp,
				  am_CHANGE,
				  THE_NON_VALUE, /* Patch point for ref */
				  am_time_offset,
				  am_clock_service,
				  new_offset_term);
	patch_refp = &hp[2];

	ASSERT(*patch_refp == THE_NON_VALUE);

	for (mix = 0; mix < no_monitors; mix++) {
            *patch_refp = to_mon_info[mix].ref;
            erts_proc_sig_send_persistent_monitor_msg(ERTS_MON_TYPE_TIME_OFFSET,
                                                      *patch_refp,
                                                      am_clock_service,
                                                      to_mon_info[mix].pid,
                                                      message_template,
                                                      hsz);
        }

	erts_free(ERTS_ALC_T_TMP, tmp);
    }
}

static void
schedule_send_time_offset_changed_notifications(ErtsMonotonicTime new_offset)
{
#ifdef ARCH_64
    void *new_offsetp = (void *) new_offset;
    ASSERT(sizeof(void *) == sizeof(ErtsMonotonicTime));
#else
    void *new_offsetp = erts_alloc(ERTS_ALC_T_NEW_TIME_OFFSET,
				   sizeof(ErtsMonotonicTime));
    *((ErtsMonotonicTime *) new_offsetp) = new_offset;
#endif
    erts_schedule_misc_aux_work(1,
				send_time_offset_changed_notifications,
				new_offsetp);
}

static ERTS_INLINE Eterm
make_time_val(Process *c_p, ErtsMonotonicTime time_val)
{
    Sint64 val = (Sint64) time_val;
    Eterm *hp;
    Uint sz;

    if (IS_SSMALL(val))
	return make_small(val);

    sz = ERTS_SINT64_HEAP_SIZE(val);
    hp = HAlloc(c_p, sz);
    return erts_sint64_to_big(val, &hp);
}

Eterm
erts_get_monotonic_start_time(struct process *c_p)
{
    return make_time_val(c_p, ERTS_MONOTONIC_TIME_START_EXTERNAL);
}

Eterm
erts_get_monotonic_end_time(struct process *c_p)
{
    return make_time_val(c_p, ERTS_MONOTONIC_TIME_END_EXTERNAL);
}

static Eterm
bld_monotonic_time_source(Uint **hpp, Uint *szp, Sint64 os_mtime)
{
#ifndef ERTS_HAVE_OS_MONOTONIC_TIME_SUPPORT
    return NIL;
#else
    int i = 0;
    Eterm k[6];
    Eterm v[6];

    if (time_sup.r.o.os_monotonic_time_disable)
	return NIL;

    k[i] = erts_bld_atom(hpp, szp, "function");
    v[i++] = erts_bld_atom(hpp, szp,
			   time_sup.r.o.os_monotonic_time_func);

    if (time_sup.r.o.os_monotonic_time_clock_id) {
	k[i] = erts_bld_atom(hpp, szp, "clock_id");
	v[i++] = erts_bld_atom(hpp, szp,
			       time_sup.r.o.os_monotonic_time_clock_id);
    }

    k[i] = erts_bld_atom(hpp, szp, "resolution");
    v[i++] = erts_bld_uint64(hpp, szp,
			     time_sup.r.o.os_monotonic_time_resolution);

    k[i] = erts_bld_atom(hpp, szp, "extended");
    v[i++] = time_sup.r.o.os_monotonic_time_extended ? am_yes : am_no;

    k[i] = erts_bld_atom(hpp, szp, "parallel");
    v[i++] = time_sup.r.o.os_monotonic_time_locked ? am_no : am_yes;

    k[i] = erts_bld_atom(hpp, szp, "time");
    v[i++] = erts_bld_sint64(hpp, szp, os_mtime);

    return erts_bld_2tup_list(hpp, szp, (Sint) i, k, v);
#endif
}

Eterm
erts_monotonic_time_source(struct process *c_p)
{
    Uint hsz = 0;
    Eterm *hp = NULL;
    Sint64 os_mtime = 0;
#ifdef ERTS_HAVE_OS_MONOTONIC_TIME_SUPPORT
    if (!time_sup.r.o.os_monotonic_time_disable)
	os_mtime = (Sint64) erts_os_monotonic_time();
#endif

    bld_monotonic_time_source(NULL, &hsz, os_mtime);
    if (hsz)
	hp = HAlloc(c_p, hsz);
    return bld_monotonic_time_source(&hp, NULL, os_mtime);
}

static Eterm
bld_system_time_source(Uint **hpp, Uint *szp, Sint64 os_stime)
{
    int i = 0;
    Eterm k[5];
    Eterm v[5];

    k[i] = erts_bld_atom(hpp, szp, "function");
    v[i++] = erts_bld_atom(hpp, szp,
			   time_sup.r.o.os_system_time_func);

    if (time_sup.r.o.os_system_time_clock_id) {
	k[i] = erts_bld_atom(hpp, szp, "clock_id");
	v[i++] = erts_bld_atom(hpp, szp,
			       time_sup.r.o.os_system_time_clock_id);
    }

    k[i] = erts_bld_atom(hpp, szp, "resolution");
    v[i++] = erts_bld_uint64(hpp, szp,
			     time_sup.r.o.os_system_time_resolution);

    k[i] = erts_bld_atom(hpp, szp, "parallel");
    v[i++] = am_yes;

    k[i] = erts_bld_atom(hpp, szp, "time");
    v[i++] = erts_bld_sint64(hpp, szp, os_stime);

    return erts_bld_2tup_list(hpp, szp, (Sint) i, k, v);
}

Eterm
erts_system_time_source(struct process *c_p)
{
    Uint hsz = 0;
    Eterm *hp = NULL;
    Sint64 os_stime = (Sint64) erts_os_system_time();

    bld_system_time_source(NULL, &hsz, os_stime);
    if (hsz)
	hp = HAlloc(c_p, hsz);
    return bld_system_time_source(&hp, NULL, os_stime);
}


#include "bif.h"

static ERTS_INLINE Eterm
time_unit_conversion(Process *c_p, Eterm term, ErtsMonotonicTime val, ErtsMonotonicTime muloff)
{
    ErtsMonotonicTime result;
    BIF_RETTYPE ret;

    if (val < 0)
	goto trap_to_erlang_code;

    /* Convert to common user specified time units */
    switch (term) {
    case am_second:
    case am_seconds:
    case make_small(1):
	result = ERTS_MONOTONIC_TO_SEC(val) + muloff*ERTS_MONOTONIC_OFFSET_SEC;
	ERTS_BIF_PREP_RET(ret, make_time_val(c_p, result));
	break;
    case am_millisecond:
    case am_milli_seconds:
    case make_small(1000):
	result = ERTS_MONOTONIC_TO_MSEC(val) + muloff*ERTS_MONOTONIC_OFFSET_MSEC;
	ERTS_BIF_PREP_RET(ret, make_time_val(c_p, result));
	break;
    case am_microsecond:
    case am_micro_seconds:
    case make_small(1000*1000):
	result = ERTS_MONOTONIC_TO_USEC(val) + muloff*ERTS_MONOTONIC_OFFSET_USEC;
	ERTS_BIF_PREP_RET(ret, make_time_val(c_p, result));
	break;
#ifdef ARCH_64
    case am_nanosecond:
    case am_nano_seconds:
    case make_small(1000*1000*1000):
	result = ERTS_MONOTONIC_TO_NSEC(val) + muloff*ERTS_MONOTONIC_OFFSET_NSEC;
	ERTS_BIF_PREP_RET(ret, make_time_val(c_p, result));
	break;
#endif
    default: {
	Eterm value, native_res;
#ifndef ARCH_64
	Sint user_res;
	if (term == am_nanosecond || term == am_nano_seconds)
	    goto to_nano_seconds;
	if (term_to_Sint(term, &user_res)) {
	    if (user_res == 1000*1000*1000) {
	    to_nano_seconds:
		result = (ERTS_MONOTONIC_TO_NSEC(val)
			  + muloff*ERTS_MONOTONIC_OFFSET_NSEC);
		ERTS_BIF_PREP_RET(ret, make_time_val(c_p, result));
		break;
	    }
	    if (user_res <= 0)
		goto badarg;
	}
#else
	if (is_small(term)) {
	    if (signed_val(term) <= 0)
		goto badarg;
	}
#endif
	else if (is_big(term)) {
	    if (big_sign(term))
		goto badarg;
	}
	else {
	badarg:
	    ERTS_BIF_PREP_ERROR(ret, c_p, BADARG);
	    break;
	}

	trap_to_erlang_code:
	/* Do it in erlang code instead; pass along values to use... */
	value = make_time_val(c_p, val + muloff*ERTS_MONOTONIC_OFFSET_NATIVE);
	native_res = make_time_val(c_p, ERTS_MONOTONIC_TIME_UNIT);

	ERTS_BIF_PREP_TRAP3(ret, erts_convert_time_unit_trap, c_p,
			    value, native_res, term);

	break;
    }
    }

    return ret;
}


/*
 * Time Native API (drivers and NIFs)
 */

#define ERTS_NAPI_TIME_ERROR ((ErtsMonotonicTime) ERTS_NAPI_TIME_ERROR__)

static void
init_time_napi(void)
{
    /* Verify that time native api constants are as expected... */

    ASSERT(sizeof(ErtsMonotonicTime) == sizeof(ErlDrvTime));
    ASSERT(ERL_DRV_TIME_ERROR == (ErlDrvTime) ERTS_NAPI_TIME_ERROR);
    ASSERT(ERL_DRV_TIME_ERROR < (ErlDrvTime) 0);
    ASSERT(ERTS_NAPI_SEC__ == (int) ERL_DRV_SEC);
    ASSERT(ERTS_NAPI_MSEC__ == (int) ERL_DRV_MSEC);
    ASSERT(ERTS_NAPI_USEC__ == (int) ERL_DRV_USEC);
    ASSERT(ERTS_NAPI_NSEC__ == (int) ERL_DRV_NSEC);

    ASSERT(sizeof(ErtsMonotonicTime) == sizeof(ErlNifTime));
    ASSERT(ERL_NIF_TIME_ERROR == (ErlNifTime) ERTS_NAPI_TIME_ERROR);
    ASSERT(ERL_NIF_TIME_ERROR < (ErlNifTime) 0);
    ASSERT(ERTS_NAPI_SEC__ == (int) ERL_NIF_SEC);
    ASSERT(ERTS_NAPI_MSEC__ == (int) ERL_NIF_MSEC);
    ASSERT(ERTS_NAPI_USEC__ == (int) ERL_NIF_USEC);
    ASSERT(ERTS_NAPI_NSEC__ == (int) ERL_NIF_NSEC);
}

ErtsMonotonicTime
erts_napi_monotonic_time(int time_unit)
{
    ErtsSchedulerData *esdp;
    ErtsMonotonicTime mtime;

    /* At least for now only allow schedulers to do this... */
    esdp = erts_get_scheduler_data();
    if (!esdp)
	return ERTS_NAPI_TIME_ERROR;

    mtime = time_sup.r.o.get_time();
    update_last_mtime(esdp, mtime);

    switch (time_unit) {
    case ERTS_NAPI_SEC__:
	mtime = ERTS_MONOTONIC_TO_SEC(mtime);
	mtime += ERTS_MONOTONIC_OFFSET_SEC;
	break;
    case ERTS_NAPI_MSEC__:
	mtime = ERTS_MONOTONIC_TO_MSEC(mtime);
	mtime += ERTS_MONOTONIC_OFFSET_MSEC;
	break;
    case ERTS_NAPI_USEC__:
	mtime = ERTS_MONOTONIC_TO_USEC(mtime);
	mtime += ERTS_MONOTONIC_OFFSET_USEC;
	break;
    case ERTS_NAPI_NSEC__:
	mtime = ERTS_MONOTONIC_TO_NSEC(mtime);
	mtime += ERTS_MONOTONIC_OFFSET_NSEC;
	break;
    default:
	return ERTS_NAPI_TIME_ERROR;
    }

    return mtime;
}

ErtsMonotonicTime
erts_napi_time_offset(int time_unit)
{
    ErtsSchedulerData *esdp;
    ErtsSystemTime offs;

    /* At least for now only allow schedulers to do this... */
    esdp = erts_get_scheduler_data();
    if (!esdp)
	return ERTS_NAPI_TIME_ERROR;

    offs = get_time_offset();
    switch (time_unit) {
    case ERTS_NAPI_SEC__:
	offs = ERTS_MONOTONIC_TO_SEC(offs);
	offs -= ERTS_MONOTONIC_OFFSET_SEC;
	break;
    case ERTS_NAPI_MSEC__:
	offs = ERTS_MONOTONIC_TO_MSEC(offs);
	offs -= ERTS_MONOTONIC_OFFSET_MSEC;
	break;
    case ERTS_NAPI_USEC__:
	offs = ERTS_MONOTONIC_TO_USEC(offs);
	offs -= ERTS_MONOTONIC_OFFSET_USEC;
	break;
    case ERTS_NAPI_NSEC__:
	offs = ERTS_MONOTONIC_TO_NSEC(offs);
	offs -= ERTS_MONOTONIC_OFFSET_NSEC;
	break;
    default:
	return ERTS_NAPI_TIME_ERROR;
    }
    return offs;
}

ErtsMonotonicTime
erts_napi_convert_time_unit(ErtsMonotonicTime val, int from, int to)
{
    ErtsMonotonicTime ffreq, tfreq, denom;
    /*
     * Convertion between time units using floor function.
     *
     * Note that this needs to work also for negative
     * values. Ordinary integer division on a negative
     * value will give ceiling...
     */

    switch ((int) from) {
    case ERTS_NAPI_SEC__: ffreq = 1; break;
    case ERTS_NAPI_MSEC__: ffreq = 1000; break;
    case ERTS_NAPI_USEC__: ffreq = 1000*1000; break;
    case ERTS_NAPI_NSEC__: ffreq = 1000*1000*1000; break;
    default: return ERTS_NAPI_TIME_ERROR;
    }

    switch ((int) to) {
    case ERTS_NAPI_SEC__: tfreq = 1; break;
    case ERTS_NAPI_MSEC__: tfreq = 1000; break;
    case ERTS_NAPI_USEC__: tfreq = 1000*1000; break;
    case ERTS_NAPI_NSEC__: tfreq = 1000*1000*1000; break;
    default: return ERTS_NAPI_TIME_ERROR;
    }

    if (tfreq >= ffreq)
	return val * (tfreq / ffreq);

    denom = ffreq / tfreq;
    if (val >= 0)
	return val / denom;

    return (val - (denom - 1)) / denom;
}

/* Built in functions */

BIF_RETTYPE monotonic_time_0(BIF_ALIST_0)
{
    ErtsMonotonicTime mtime = time_sup.r.o.get_time();
    update_last_mtime(erts_proc_sched_data(BIF_P), mtime);
    mtime += ERTS_MONOTONIC_OFFSET_NATIVE;
    BIF_RET(make_time_val(BIF_P, mtime));
}

BIF_RETTYPE monotonic_time_1(BIF_ALIST_1)
{
    ErtsMonotonicTime mtime = time_sup.r.o.get_time();
    update_last_mtime(erts_proc_sched_data(BIF_P), mtime);
    BIF_RET(time_unit_conversion(BIF_P, BIF_ARG_1, mtime, 1));
}

BIF_RETTYPE system_time_0(BIF_ALIST_0)
{
    ErtsMonotonicTime mtime, offset;
    mtime = time_sup.r.o.get_time();
    offset = get_time_offset();
    update_last_mtime(erts_proc_sched_data(BIF_P), mtime);
    BIF_RET(make_time_val(BIF_P, mtime + offset));
}

BIF_RETTYPE system_time_1(BIF_ALIST_0)
{
    ErtsMonotonicTime mtime, offset;
    mtime = time_sup.r.o.get_time();
    offset = get_time_offset();
    update_last_mtime(erts_proc_sched_data(BIF_P), mtime);
    BIF_RET(time_unit_conversion(BIF_P, BIF_ARG_1, mtime + offset, 0));
}

BIF_RETTYPE erts_internal_time_unit_0(BIF_ALIST_0)
{
    BIF_RET(make_time_val(BIF_P, ERTS_MONOTONIC_TIME_UNIT));
}

BIF_RETTYPE time_offset_0(BIF_ALIST_0)
{
    ErtsMonotonicTime time_offset = get_time_offset();
    time_offset -= ERTS_MONOTONIC_OFFSET_NATIVE;
    BIF_RET(make_time_val(BIF_P, time_offset));
}

BIF_RETTYPE time_offset_1(BIF_ALIST_1)
{
    BIF_RET(time_unit_conversion(BIF_P, BIF_ARG_1, get_time_offset(), -1));
}


BIF_RETTYPE timestamp_0(BIF_ALIST_0)
{
    Eterm *hp, res;
    ErtsMonotonicTime mtime, offset;
    Uint mega_sec, sec, micro_sec;

    mtime = time_sup.r.o.get_time();
    offset = get_time_offset();
    update_last_mtime(erts_proc_sched_data(BIF_P), mtime);

    make_timestamp_value(&mega_sec, &sec, &micro_sec, mtime, offset);

    /*
     * Mega seconds is the only value that potentially
     * ever could be a bignum. However, that wont happen
     * during at least the next 4 million years...
     *
     * (System time will also have wrapped in the
     * 64-bit integer before we get there...)
     */

    ASSERT(IS_USMALL(0, mega_sec));
    ASSERT(IS_USMALL(0, sec));
    ASSERT(IS_USMALL(0, micro_sec));

    hp = HAlloc(BIF_P, 4);
    res = TUPLE3(hp,
		 make_small(mega_sec),
		 make_small(sec),
		 make_small(micro_sec));
    BIF_RET(res);
}

BIF_RETTYPE os_system_time_0(BIF_ALIST_0)
{
    ErtsSystemTime stime = erts_os_system_time();
    BIF_RET(make_time_val(BIF_P, stime));
}

BIF_RETTYPE os_system_time_1(BIF_ALIST_1)
{
    ErtsSystemTime stime = erts_os_system_time();
    BIF_RET(time_unit_conversion(BIF_P, BIF_ARG_1, stime, 0));
}

BIF_RETTYPE
os_perf_counter_0(BIF_ALIST_0)
{
    BIF_RET(make_time_val(BIF_P, erts_sys_perf_counter()));
}

BIF_RETTYPE erts_internal_perf_counter_unit_0(BIF_ALIST_0)
{
    BIF_RET(make_time_val(BIF_P, erts_sys_perf_counter_unit()));
}
