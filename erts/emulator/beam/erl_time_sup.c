/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1999-2012. All Rights Reserved.
 *
 * The contents of this file are subject to the Erlang Public License,
 * Version 1.1, (the "License"); you may not use this file except in
 * compliance with the License. You should have received a copy of the
 * Erlang Public License along with this software. If not, it can be
 * retrieved online at http://www.erlang.org/.
 *
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
 * the License for the specific language governing rights and limitations
 * under the License.
 *
 * %CopyrightEnd%
 */

/*
 * Support routines for the time
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "erl_vm.h"
#include "global.h"
 
static erts_smp_mtx_t erts_timeofday_mtx;
static erts_smp_mtx_t erts_get_time_mtx;

static SysTimes t_start; /* Used in elapsed_time_both */
static ErtsMonotonicTime prev_wall_clock_elapsed; /* Used in wall_clock_elapsed_time_both */
static ErtsMonotonicTime previous_now; /* Used in get_now */

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

static void
schedule_send_time_offset_changed_notifications(ErtsMonotonicTime new_offset);

/*
 * NOTE! ERTS_MONOTONIC_TIME_START *need* to be a multiple
 *       of ERTS_MONOTONIC_TIME_UNIT.
 */

#if ERTS_COMPILE_TIME_MONOTONIC_TIME_UNIT

#ifdef ARCH_32
/*
 * Want to use a big-num of arity 2 as long as possible (584 years
 * in the nano-second time unit case).
 */
#define ERTS_MONOTONIC_TIME_START		\
    (((((((ErtsMonotonicTime) 1) << 32)-1)	\
       / ERTS_MONOTONIC_TIME_UNIT)		\
      * ERTS_MONOTONIC_TIME_UNIT)		\
     + ERTS_MONOTONIC_TIME_UNIT)

#else /* ARCH_64 */

#if ERTS_COMPILE_TIME_MONOTONIC_TIME_UNIT <= 1000*1000

/*
 * Using micro second time unit or lower. Start at zero since
 * time will remain an immediate for a very long time anyway
 * (18279 years in the micro second case)...
 */
#define ERTS_MONOTONIC_TIME_START ((ErtsMonotonicTime) 0)

#else /* ERTS_COMPILE_TIME_MONOTONIC_TIME_UNIT > 1000*1000 */

/*
 * Want to use an immediate as long as possible (36 years in the
 * nano-second time unit case).
*/
#define ERTS_MONOTONIC_TIME_START 		\
    ((((ErtsMonotonicTime) MIN_SMALL)		\
      / ERTS_MONOTONIC_TIME_UNIT)		\
     * ERTS_MONOTONIC_TIME_UNIT)

#endif /* ERTS_COMPILE_TIME_MONOTONIC_TIME_UNIT > 1000*1000 */

#endif /* ARCH_64 */

#define ERTS_MONOTONIC_OFFSET_NATIVE ERTS_MONOTONIC_TIME_START
#define ERTS_MONOTONIC_OFFSET_NSEC ERTS_MONOTONIC_TO_NSEC__(ERTS_MONOTONIC_TIME_START)
#define ERTS_MONOTONIC_OFFSET_USEC ERTS_MONOTONIC_TO_USEC__(ERTS_MONOTONIC_TIME_START)
#define ERTS_MONOTONIC_OFFSET_MSEC ERTS_MONOTONIC_TO_MSEC__(ERTS_MONOTONIC_TIME_START)
#define ERTS_MONOTONIC_OFFSET_SEC ERTS_MONOTONIC_TO_SEC__(ERTS_MONOTONIC_TIME_START)

#else /* ERTS_COMPILE_TIME_MONOTONIC_TIME_UNIT */

/*
 * Initialized in erts_init_time_sup()...
 */

#define ERTS_MONOTONIC_TIME_START (time_sup.r.o.start)
#define ERTS_MONOTONIC_OFFSET_NATIVE (time_sup.r.o.start_offset.native)
#define ERTS_MONOTONIC_OFFSET_NSEC (time_sup.r.o.start_offset.nsec)
#define ERTS_MONOTONIC_OFFSET_USEC (time_sup.r.o.start_offset.usec)
#define ERTS_MONOTONIC_OFFSET_MSEC (time_sup.r.o.start_offset.msec)
#define ERTS_MONOTONIC_OFFSET_SEC (time_sup.r.o.start_offset.sec)

#endif /* ERTS_COMPILE_TIME_MONOTONIC_TIME_UNIT */

#define ERTS_MONOTONIC_TO_SYS_TIME_VAL(TVP, MT)				\
    do {								\
	ErtsMonotonicTime sec__, usec__;				\
	sec__ = ERTS_MONOTONIC_TO_SEC((MT));				\
	usec__ = ERTS_MONOTONIC_TO_USEC((MT)) - sec__*1000000;		\
	ASSERT(usec__ < 1000000);					\
	(TVP)->tv_sec = sec__;						\
	(TVP)->tv_usec = usec__;					\
    } while (0)

#define ERTS_MAX_SYSTEM_TIME_DIFF ERTS_MSEC_TO_MONOTONIC(10)
#define ERTS_SYSTEM_TIME_DIFF_EXCEED_LIMIT(ESYSTIME, OSSYSTIME)		\
    (((Uint64) (ESYSTIME)) - (((Uint64) (OSSYSTIME))			\
			      - ERTS_MAX_SYSTEM_TIME_DIFF)		\
     > 2*ERTS_MAX_SYSTEM_TIME_DIFF)

#define ERTS_TIME_CORRECTION_LARGE_ADJ_DIFF (ERTS_MAX_SYSTEM_TIME_DIFF/2)
#define ERTS_TIME_CORRECTION_SMALL_ADJ_DIFF ERTS_USEC_TO_MONOTONIC(500)

struct time_sup_read_only__ {
    ErtsMonotonicTime (*get_time)(void);
    int correction;
    ErtsTimeWarpMode warp_mode;
#ifdef ERTS_HAVE_OS_MONOTONIC_TIME_SUPPORT
    ErtsMonotonicTime moffset;
    int os_monotonic_disable;
    char *os_monotonic_func;
    char *os_monotonic_clock_id;
    int os_monotonic_locked;
    Uint64 os_monotonic_resolution;
#endif
#if !ERTS_COMPILE_TIME_MONOTONIC_TIME_UNIT
    ErtsMonotonicTime start;
    struct {
	ErtsMonotonicTime native;
	ErtsMonotonicTime nsec;
	ErtsMonotonicTime usec;
	ErtsMonotonicTime msec;
	ErtsMonotonicTime sec;
    } start_offset;
#endif
};

typedef struct {
#ifndef ERTS_HAVE_CORRECTED_OS_MONOTONIC
    ErtsMonotonicTime drift; /* Correction for os monotonic drift */
#endif
    ErtsMonotonicTime error; /* Correction for error between system times */
} ErtsMonotonicCorrection;

typedef struct {
    ErtsMonotonicTime erl_mtime;
    ErtsMonotonicTime os_mtime;
    ErtsMonotonicCorrection correction;
} ErtsMonotonicCorrectionInstance;

#define ERTS_DRIFT_INTERVALS 5
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
    } intervals[ERTS_DRIFT_INTERVALS];
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
#ifndef ERTS_HAVE_CORRECTED_OS_MONOTONIC
    ErtsMonotonicDriftData drift;
#endif
    ErtsMonotonicTime last_check;
    int short_check_interval;
} ErtsMonotonicCorrectionData;

struct time_sup_infrequently_changed__ {
#ifdef ERTS_HAVE_OS_MONOTONIC_TIME_SUPPORT
    struct {
	erts_smp_rwmtx_t rwmtx;
	ErlTimer timer;
	ErtsMonotonicCorrectionData cdata;
    } parmon;
    ErtsMonotonicTime minit;
#endif
    int finalized_offset;
    SysTimeval inittv; /* Used everywhere, the initial time-of-day */
    ErtsMonotonicTime not_corrected_moffset;
    erts_atomic64_t offset;
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

/*
 * erts_get_approx_time() returns an *approximate* time
 * in seconds. NOTE that this time may jump backwards!!!
 */
erts_approx_time_t
erts_get_approx_time(void)
{
    SysTimeval tv;
    sys_gettimeofday(&tv);

    return (erts_approx_time_t) tv.tv_sec;
}

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

#define ERTS_TIME_DRIFT_MAX_ADJ_DIFF ERTS_USEC_TO_MONOTONIC(100)
#define ERTS_TIME_DRIFT_MIN_ADJ_DIFF ERTS_USEC_TO_MONOTONIC(5)

static ERTS_INLINE ErtsMonotonicTime
calc_corrected_erl_mtime(ErtsMonotonicTime os_mtime,
			 ErtsMonotonicCorrectionInstance *cip,
			 ErtsMonotonicTime *os_mdiff_p)
{
    ErtsMonotonicTime erl_mtime, diff = os_mtime - cip->os_mtime;
    ERTS_TIME_ASSERT(diff >= 0);
#ifndef ERTS_HAVE_CORRECTED_OS_MONOTONIC
    diff += (cip->correction.drift*diff)/ERTS_MONOTONIC_TIME_UNIT;
#endif
    erl_mtime = cip->erl_mtime;
    erl_mtime += diff;
    erl_mtime += cip->correction.error*(diff/ERTS_TCORR_ERR_UNIT);
    if (os_mdiff_p)
	*os_mdiff_p = diff;
    return erl_mtime;
}

static ErtsMonotonicTime get_corrected_time(void)
{
    ErtsMonotonicTime os_mtime;
    ErtsMonotonicCorrectionData cdata;
    ErtsMonotonicCorrectionInstance *cip;

    erts_smp_rwmtx_rlock(&time_sup.inf.c.parmon.rwmtx);

    os_mtime = erts_os_monotonic_time();

    cdata = time_sup.inf.c.parmon.cdata;

    erts_smp_rwmtx_runlock(&time_sup.inf.c.parmon.rwmtx);

    if (os_mtime >= cdata.curr.os_mtime)
	cip = &cdata.curr;
    else {
	if (os_mtime < cdata.prev.os_mtime)
	    erl_exit(ERTS_ABORT_EXIT,
		     "OS monotonic time stepped backwards\n");
	cip = &cdata.prev;
    }

    return calc_corrected_erl_mtime(os_mtime, cip, NULL);
}

static void
check_time_correction(void *unused)
{
    ErtsMonotonicCorrectionData cdata;
    ErtsMonotonicCorrection new_correction;
    ErtsMonotonicCorrectionInstance *cip;
    ErtsMonotonicTime mdiff, sdiff, os_mtime, erl_mtime, os_stime, erl_stime, time_offset;
    Uint timeout;
    SysTimeval tod;
    int set_new_correction, begin_short_intervals = 0;

    erts_smp_rwmtx_rlock(&time_sup.inf.c.parmon.rwmtx);

    ASSERT(time_sup.inf.c.finalized_offset);

    os_mtime = erts_os_monotonic_time();
    sys_gettimeofday(&tod);

    cdata = time_sup.inf.c.parmon.cdata;

    erts_smp_rwmtx_runlock(&time_sup.inf.c.parmon.rwmtx);

    if (os_mtime < cdata.curr.os_mtime)
	erl_exit(ERTS_ABORT_EXIT,
		 "OS monotonic time stepped backwards\n");
    cip = &cdata.curr;

    erl_mtime = calc_corrected_erl_mtime(os_mtime, cip, &mdiff);
    time_offset = get_time_offset();
    erl_stime = erl_mtime + time_offset;

    os_stime = ERTS_SEC_TO_MONOTONIC(tod.tv_sec);
    os_stime += ERTS_USEC_TO_MONOTONIC(tod.tv_usec);

    sdiff = erl_stime - os_stime;

    new_correction = cip->correction;
    
    if (time_sup.r.o.warp_mode == ERTS_MULTI_TIME_WARP_MODE
	&& (sdiff < -2*ERTS_TIME_CORRECTION_SMALL_ADJ_DIFF
	    || 2*ERTS_TIME_CORRECTION_SMALL_ADJ_DIFF < sdiff)) {
	/* System time diff exeeded limits; change time offset... */
	time_offset -= sdiff;
	sdiff = 0;
	set_time_offset(time_offset);
	schedule_send_time_offset_changed_notifications(time_offset);
	begin_short_intervals = 1;
	if (cdata.curr.correction.error == 0)
	    set_new_correction = 0;
	else {
	    set_new_correction = 1;
	    new_correction.error = 0;
	}
    }
    else if (cdata.curr.correction.error == 0) {
	if (sdiff < -ERTS_TIME_CORRECTION_SMALL_ADJ_DIFF) {
	    set_new_correction = 1;
	    if (sdiff < -ERTS_TIME_CORRECTION_LARGE_ADJ_DIFF)
		new_correction.error = ERTS_TCORR_ERR_LARGE_ADJ;
	    else
		new_correction.error = ERTS_TCORR_ERR_SMALL_ADJ;
	}
	else if (sdiff > ERTS_TIME_CORRECTION_SMALL_ADJ_DIFF) {
	    set_new_correction = 1;
	    if (sdiff > ERTS_TIME_CORRECTION_LARGE_ADJ_DIFF)
		new_correction.error = -ERTS_TCORR_ERR_LARGE_ADJ;
	    else
		new_correction.error = -ERTS_TCORR_ERR_SMALL_ADJ;
	}
	else {
	    set_new_correction = 0;
	}
    }
    else if (cdata.curr.correction.error > 0) {
	if (sdiff < 0) {
	    if (cdata.curr.correction.error == ERTS_TCORR_ERR_LARGE_ADJ
		|| -ERTS_TIME_CORRECTION_LARGE_ADJ_DIFF <= sdiff)
		set_new_correction = 0;
	    else {
		new_correction.error = ERTS_TCORR_ERR_LARGE_ADJ;
		set_new_correction = 1;
	    }
	}
	else if (sdiff > ERTS_TIME_CORRECTION_SMALL_ADJ_DIFF) {
	    set_new_correction = 1;
	    if (sdiff > ERTS_TIME_CORRECTION_LARGE_ADJ_DIFF)
		new_correction.error = -ERTS_TCORR_ERR_LARGE_ADJ;
	    else
		new_correction.error = -ERTS_TCORR_ERR_SMALL_ADJ;
	}
	else {
	    set_new_correction = 1;
	    new_correction.error = 0;
	}
    }
    else /* if (cdata.curr.correction.error < 0) */ { 
	if (0 < sdiff) {
	    if (cdata.curr.correction.error == -ERTS_TCORR_ERR_LARGE_ADJ
		|| sdiff <= ERTS_TIME_CORRECTION_LARGE_ADJ_DIFF)
		set_new_correction = 0;
	    else {
		new_correction.error = -ERTS_TCORR_ERR_LARGE_ADJ;
		set_new_correction = 1;
	    }
	    set_new_correction = 0;
	}
	else if (sdiff < -ERTS_TIME_CORRECTION_SMALL_ADJ_DIFF) {
	    set_new_correction = 1;
	    if (sdiff < -ERTS_TIME_CORRECTION_LARGE_ADJ_DIFF)
		new_correction.error = ERTS_TCORR_ERR_LARGE_ADJ;
	    else
		new_correction.error = ERTS_TCORR_ERR_SMALL_ADJ;
	}
	else {
	    set_new_correction = 1;
	    new_correction.error = 0;
	}
    }

#ifndef ERTS_HAVE_CORRECTED_OS_MONOTONIC
    {
	ErtsMonotonicDriftData *ddp = &time_sup.inf.c.parmon.cdata.drift;
	int ix = ddp->ix;
	ErtsMonotonicTime mtime_diff, old_os_mtime;

	old_os_mtime = ddp->intervals[ix].time.mon;
	mtime_diff = os_mtime - old_os_mtime;

	if (mtime_diff >= ERTS_SEC_TO_MONOTONIC(10)) {
	    ErtsMonotonicTime drift_adj, drift_adj_diff, old_os_stime,
		stime_diff, mtime_acc, stime_acc, avg_drift_adj;

	    old_os_stime = ddp->intervals[ix].time.sys;

	    mtime_acc = ddp->acc.mon;
	    stime_acc = ddp->acc.sys;

	    avg_drift_adj = ((stime_acc - mtime_acc)*ERTS_MONOTONIC_TIME_UNIT) / mtime_acc;

	    mtime_diff = os_mtime - old_os_mtime;
	    stime_diff = os_stime - old_os_stime;
	    drift_adj = ((stime_diff - mtime_diff)*ERTS_MONOTONIC_TIME_UNIT) / mtime_diff;
	    
	    ix++;
	    if (ix >= ERTS_DRIFT_INTERVALS)
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

	    /*
	     * If calculated drift adjustment is if off by more than 20% from the
	     * average drift we interpret this as a discontinous leap in system
	     * time and ignore it. If it actually is a change in drift we will
	     * later detect this when the average drift change.
	     */
	    drift_adj_diff = avg_drift_adj - drift_adj;
	    if (drift_adj_diff < -ERTS_TIME_DRIFT_MAX_ADJ_DIFF
		|| ERTS_TIME_DRIFT_MAX_ADJ_DIFF < drift_adj_diff) {
		ddp->dirty_counter = ERTS_DRIFT_INTERVALS;
		begin_short_intervals = 1;
	    }
	    else {
		if (ddp->dirty_counter <= 0) {
		    drift_adj = ((stime_acc - mtime_acc)*ERTS_MONOTONIC_TIME_UNIT) / mtime_acc;
		}
		if (ddp->dirty_counter >= 0) {
		    if (ddp->dirty_counter == 0) {
			/* Force set new drift correction... */
			set_new_correction = 1;
		    }
		    ddp->dirty_counter--;
		}
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
#endif

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
	if (sdiff < 0)
	    sdiff = -1*sdiff;
	if (ecorr < 0)
	    ecorr = -1*ecorr;
	if (sdiff > ecorr*(ERTS_LONG_TIME_CORRECTION_CHECK/ERTS_TCORR_ERR_UNIT))
	    timeout = ERTS_MONOTONIC_TO_MSEC(ERTS_LONG_TIME_CORRECTION_CHECK);
	else {
	    timeout = ERTS_MONOTONIC_TO_MSEC((ERTS_TCORR_ERR_UNIT*sdiff)/ecorr);
	    if (timeout < 10)
		timeout = 10;
	}
    }

    if (timeout > ERTS_MONOTONIC_TO_MSEC(ERTS_SHORT_TIME_CORRECTION_CHECK)
	&& time_sup.inf.c.parmon.cdata.short_check_interval) {
	timeout = ERTS_MONOTONIC_TO_MSEC(ERTS_SHORT_TIME_CORRECTION_CHECK);
    }
    
    if (set_new_correction) {
	erts_smp_rwmtx_rwlock(&time_sup.inf.c.parmon.rwmtx);

	os_mtime = erts_os_monotonic_time();

	/* Save previous correction instance */
	time_sup.inf.c.parmon.cdata.prev = *cip;

	/*
	 * Current correction instance begin when
	 * OS monotonic time has increased one unit.
	 */
	os_mtime++;

	/*
	 * Erlang monotonic time corresponding to
	 * next OS monotonic time using previous
	 * correction.
	 */
	erl_mtime = calc_corrected_erl_mtime(os_mtime, cip, NULL);

	/*
	 * Save new current correction instance.
	 */
	time_sup.inf.c.parmon.cdata.curr.erl_mtime = erl_mtime;
	time_sup.inf.c.parmon.cdata.curr.os_mtime = os_mtime;
	time_sup.inf.c.parmon.cdata.curr.correction = new_correction;

	erts_smp_rwmtx_rwunlock(&time_sup.inf.c.parmon.rwmtx);
    }

    erts_set_timer(&time_sup.inf.c.parmon.timer,
		   check_time_correction,
		   NULL,
		   NULL,
		   timeout);
}

#ifndef ERTS_HAVE_CORRECTED_OS_MONOTONIC

static void
init_check_time_correction(void *unused)
{
    ErtsMonotonicDriftData *ddp;
    ErtsMonotonicTime old_mtime, old_stime, mtime, stime, mtime_diff, stime_diff;
    int ix;
    SysTimeval tod;

    ddp = &time_sup.inf.c.parmon.cdata.drift;
    ix = ddp->ix;
    old_mtime = ddp->intervals[0].time.mon;
    old_stime = ddp->intervals[0].time.sys;

    mtime = erts_os_monotonic_time();
    sys_gettimeofday(&tod);

    stime = ERTS_SEC_TO_MONOTONIC(tod.tv_sec);
    stime += ERTS_USEC_TO_MONOTONIC(tod.tv_usec);

    mtime_diff = mtime - old_mtime;
    stime_diff = stime - old_stime;
    if (100*stime_diff  < 80*mtime_diff || 120*mtime_diff < 100*stime_diff ) {
	/* Had a system time leap... pretend no drift... */
	stime_diff = mtime_diff;
    }
    
    /*
     * We use old time values in order to trigger
     * a drift adjustment, and repeat this interval
     * in all slots...
     */
    for (ix = 0; ix < ERTS_DRIFT_INTERVALS; ix++) {
	ddp->intervals[ix].diff.mon = mtime_diff;
	ddp->intervals[ix].diff.sys = stime_diff;
	ddp->intervals[ix].time.mon = old_mtime;
	ddp->intervals[ix].time.sys = old_stime;
    }

    ddp->acc.sys = stime_diff*ERTS_DRIFT_INTERVALS;
    ddp->acc.mon = mtime_diff*ERTS_DRIFT_INTERVALS;
    ddp->ix = 0;
    ddp->dirty_counter = ERTS_DRIFT_INTERVALS;

    check_time_correction(NULL);
}

#endif

static ErtsMonotonicTime
finalize_corrected_time_offset(SysTimeval *todp)
{
    ErtsMonotonicTime os_mtime;
    ErtsMonotonicCorrectionData cdata;
    ErtsMonotonicCorrectionInstance *cip;

    erts_smp_rwmtx_rlock(&time_sup.inf.c.parmon.rwmtx);

    os_mtime = erts_os_monotonic_time();
    sys_gettimeofday(todp);

    cdata = time_sup.inf.c.parmon.cdata;

    erts_smp_rwmtx_runlock(&time_sup.inf.c.parmon.rwmtx);

    if (os_mtime < cdata.curr.os_mtime)
	erl_exit(ERTS_ABORT_EXIT,
		 "OS monotonic time stepped backwards\n");
    cip = &cdata.curr;

    return calc_corrected_erl_mtime(os_mtime, cip, NULL);
}

static void
late_init_time_correction(void)
{
    if (time_sup.inf.c.finalized_offset) {
	erts_set_timer(&time_sup.inf.c.parmon.timer,
#ifndef ERTS_HAVE_CORRECTED_OS_MONOTONIC
		       init_check_time_correction,
#else
		       check_time_correction,
#endif
		       NULL,
		       NULL,
		       ERTS_MONOTONIC_TO_MSEC(ERTS_SHORT_TIME_CORRECTION_CHECK));
    }
}

#endif /* ERTS_HAVE_OS_MONOTONIC_TIME_SUPPORT */

static ErtsMonotonicTime get_not_corrected_time(void)
{
    SysTimeval tmp_tv;
    ErtsMonotonicTime stime, mtime;

    erts_smp_mtx_lock(&erts_get_time_mtx);

    sys_gettimeofday(&tmp_tv);

    stime = ERTS_SEC_TO_MONOTONIC(tmp_tv.tv_sec);
    stime += ERTS_USEC_TO_MONOTONIC(tmp_tv.tv_usec);

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

    erts_smp_mtx_unlock(&erts_get_time_mtx);

    return mtime;
}

int erts_check_time_adj_support(int time_correction,
				ErtsTimeWarpMode time_warp_mode)
{
    if (!time_correction)
	return 1;

    /* User wants time correction */

#ifdef ERTS_HAVE_OS_MONOTONIC_TIME_SUPPORT
    return !time_sup.r.o.os_monotonic_disable;
#else
    return 0;
#endif
}

int
erts_has_time_correction(void)
{
    return time_sup.r.o.correction;
}

void
erts_early_init_time_sup(void)
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
    time_sup.r.o.os_monotonic_disable
	= !sys_init_time_res.have_os_monotonic;
    time_sup.r.o.os_monotonic_func
	= sys_init_time_res.os_monotonic_info.func;
    time_sup.r.o.os_monotonic_clock_id
	= sys_init_time_res.os_monotonic_info.clock_id;
    time_sup.r.o.os_monotonic_locked
	= sys_init_time_res.os_monotonic_info.locked_use;
    time_sup.r.o.os_monotonic_resolution
	= sys_init_time_res.os_monotonic_info.resolution;
#endif
}

int 
erts_init_time_sup(int time_correction, ErtsTimeWarpMode time_warp_mode)
{
#if !ERTS_COMPILE_TIME_MONOTONIC_TIME_UNIT
    ErtsMonotonicTime abs_start;
#endif

    ASSERT(ERTS_MONOTONIC_TIME_MIN < ERTS_MONOTONIC_TIME_MAX);

    erts_smp_mtx_init(&erts_timeofday_mtx, "timeofday");
    erts_smp_mtx_init(&erts_get_time_mtx, "get_time");

    time_sup.r.o.correction = time_correction;
    time_sup.r.o.warp_mode = time_warp_mode;
 
    if (time_warp_mode == ERTS_SINGLE_TIME_WARP_MODE)
	time_sup.inf.c.finalized_offset = 0;
    else
	time_sup.inf.c.finalized_offset = ~0;

#if !ERTS_COMPILE_TIME_MONOTONIC_TIME_UNIT

#ifdef ARCH_32
    time_sup.r.o.start = ((((ErtsMonotonicTime) 1) << 32)-1);
    time_sup.r.o.start /= ERTS_MONOTONIC_TIME_UNIT;
    time_sup.r.o.start *= ERTS_MONOTONIC_TIME_UNIT;
    time_sup.r.o.start += ERTS_MONOTONIC_TIME_UNIT;
    abs_start = time_sup.r.o.start;
#else /* ARCH_64 */
    if (ERTS_MONOTONIC_TIME_UNIT <= 1000*1000)
	abs_start = time_sup.r.o.start = 0;
    else {
	time_sup.r.o.start = ((ErtsMonotonicTime) MIN_SMALL);
	time_sup.r.o.start /= ERTS_MONOTONIC_TIME_UNIT;
	time_sup.r.o.start *= ERTS_MONOTONIC_TIME_UNIT;
	abs_start = -1*time_sup.r.o.start;
    }
#endif

    time_sup.r.o.start_offset.native = time_sup.r.o.start;
    time_sup.r.o.start_offset.nsec = (ErtsMonotonicTime)
	erts_time_unit_conversion((Uint64) abs_start,
				  (Uint32) ERTS_MONOTONIC_TIME_UNIT,
				  (Uint32) 1000*1000*1000);
    time_sup.r.o.start_offset.usec = (ErtsMonotonicTime)
	erts_time_unit_conversion((Uint64) abs_start,
				  (Uint32) ERTS_MONOTONIC_TIME_UNIT,
				  (Uint32) 1000*1000);
    time_sup.r.o.start_offset.msec = (ErtsMonotonicTime)
	erts_time_unit_conversion((Uint64) abs_start,
				  (Uint32) ERTS_MONOTONIC_TIME_UNIT,
				  (Uint32) 1000);
    time_sup.r.o.start_offset.sec = (ErtsMonotonicTime)
	erts_time_unit_conversion((Uint64) abs_start,
				  (Uint32) ERTS_MONOTONIC_TIME_UNIT,
				  (Uint32) 1);
    if (time_sup.r.o.start < 0) {
	time_sup.r.o.start_offset.nsec *= -1;
	time_sup.r.o.start_offset.usec *= -1;
	time_sup.r.o.start_offset.msec *= -1;
	time_sup.r.o.start_offset.sec *= -1;
    }

#endif

    if (ERTS_MONOTONIC_TIME_UNIT < ERTS_CLKTCK_RESOLUTION)
	ERTS_INTERNAL_ERROR("Too small monotonic time time unit");

#ifndef ERTS_HAVE_OS_MONOTONIC_TIME_SUPPORT
    time_sup.r.o.correction = 0;
#else
    if (time_sup.r.o.os_monotonic_disable)
	time_sup.r.o.correction = 0;

    if (time_sup.r.o.correction) {
	ErtsMonotonicCorrectionData *cdatap;
	erts_smp_rwmtx_opt_t rwmtx_opts = ERTS_SMP_RWMTX_OPT_DEFAULT_INITER;
	ErtsMonotonicTime offset;
	time_sup.inf.c.minit = erts_os_monotonic_time();
	sys_gettimeofday(&time_sup.inf.c.inittv);
	time_sup.r.o.moffset = -1*time_sup.inf.c.minit;
	offset = ERTS_SEC_TO_MONOTONIC(time_sup.inf.c.inittv.tv_sec);
	offset += ERTS_USEC_TO_MONOTONIC(time_sup.inf.c.inittv.tv_usec);
	init_time_offset(offset);

	rwmtx_opts.type = ERTS_SMP_RWMTX_TYPE_EXTREMELY_FREQUENT_READ;
	rwmtx_opts.lived = ERTS_SMP_RWMTX_LONG_LIVED;

	erts_smp_rwmtx_init_opt(&time_sup.inf.c.parmon.rwmtx,
				&rwmtx_opts, "get_corrected_time");

	cdatap = &time_sup.inf.c.parmon.cdata;
    
#ifndef ERTS_HAVE_CORRECTED_OS_MONOTONIC
	cdatap->drift.intervals[0].time.sys
	    = ERTS_SEC_TO_MONOTONIC(time_sup.inf.c.inittv.tv_sec);
	cdatap->drift.intervals[0].time.sys
	    += ERTS_USEC_TO_MONOTONIC(time_sup.inf.c.inittv.tv_usec);
	cdatap->drift.intervals[0].time.mon = time_sup.inf.c.minit;
	cdatap->curr.correction.drift = 0;
#endif
	cdatap->curr.correction.error = 0;
	cdatap->curr.erl_mtime = 0;
	cdatap->curr.os_mtime = time_sup.inf.c.minit;
	cdatap->last_check = time_sup.inf.c.minit;
	cdatap->short_check_interval = ERTS_INIT_SHORT_INTERVAL_COUNTER;
	cdatap->prev = cdatap->curr;

	time_sup.r.o.get_time = get_corrected_time;
    }
    else
#endif
    {
	ErtsMonotonicTime stime, offset;
	time_sup.r.o.get_time = get_not_corrected_time;
	stime = ERTS_SEC_TO_MONOTONIC(time_sup.inf.c.inittv.tv_sec);
	stime += ERTS_USEC_TO_MONOTONIC(time_sup.inf.c.inittv.tv_usec);
	offset = stime;
	time_sup.inf.c.not_corrected_moffset = offset;
	init_time_offset(offset);
	time_sup.f.c.last_not_corrected_time = 0;
    }

    prev_wall_clock_elapsed = 0;

    previous_now = ERTS_MONOTONIC_TO_USEC(get_time_offset());

#ifdef DEBUG
    time_sup_initialized = 1;
#endif

    return ERTS_CLKTCK_RESOLUTION/1000;
}    

void
erts_late_init_time_sup(void)
{
#ifdef ERTS_HAVE_OS_MONOTONIC_TIME_SUPPORT
    /* Timer wheel must be initialized */
    if (time_sup.r.o.get_time == get_corrected_time)
	late_init_time_correction();
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
	if (time_sup.inf.c.finalized_offset)
	    return ERTS_TIME_OFFSET_FINAL;
	return ERTS_TIME_OFFSET_PRELIMINARY;
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

	erts_smp_mtx_lock(&erts_get_time_mtx);

	if (!time_sup.inf.c.finalized_offset) {
	    ErtsMonotonicTime mtime, new_offset;
	    SysTimeval tv;

#ifdef ERTS_HAVE_OS_MONOTONIC_TIME_SUPPORT
	    if (!time_sup.r.o.correction)
#endif
	    {
		ErtsMonotonicTime stime;
		sys_gettimeofday(&tv);

		stime = ERTS_SEC_TO_MONOTONIC(tv.tv_sec);
		stime += ERTS_USEC_TO_MONOTONIC(tv.tv_usec);

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
		mtime = finalize_corrected_time_offset(&tv);
		new_offset = ERTS_SEC_TO_MONOTONIC(tv.tv_sec);
		new_offset += ERTS_USEC_TO_MONOTONIC(tv.tv_usec);
		new_offset -= mtime;

	    }
#endif
	    new_offset = ERTS_MONOTONIC_TO_USEC(new_offset);
	    new_offset = ERTS_USEC_TO_MONOTONIC(new_offset);

	    set_time_offset(new_offset);
	    schedule_send_time_offset_changed_notifications(new_offset);

	    time_sup.inf.c.finalized_offset = ~0;
	    res = ERTS_TIME_OFFSET_PRELIMINARY;
	}

	erts_smp_mtx_unlock(&erts_get_time_mtx);

#ifdef ERTS_HAVE_OS_MONOTONIC_TIME_SUPPORT
	if (res == ERTS_TIME_OFFSET_PRELIMINARY
	    && time_sup.r.o.get_time == get_corrected_time) {
	    late_init_time_correction();
	}
#endif

	return res;
    }
    default:
	ERTS_INTERNAL_ERROR("Invalid time warp mode");
	return ERTS_TIME_OFFSET_VOLATILE;
    }
}

/* info functions */

void 
elapsed_time_both(UWord *ms_user, UWord *ms_sys, 
		  UWord *ms_user_diff, UWord *ms_sys_diff)
{
    UWord prev_total_user, prev_total_sys;
    UWord total_user, total_sys;
    SysTimes now;

    sys_times(&now);
    total_user = (now.tms_utime * 1000) / SYS_CLK_TCK;
    total_sys = (now.tms_stime * 1000) / SYS_CLK_TCK;

    if (ms_user != NULL)
	*ms_user = total_user;
    if (ms_sys != NULL)
	*ms_sys = total_sys;

    erts_smp_mtx_lock(&erts_timeofday_mtx);
    
    prev_total_user = (t_start.tms_utime * 1000) / SYS_CLK_TCK;
    prev_total_sys = (t_start.tms_stime * 1000) / SYS_CLK_TCK;
    t_start = now;
    
    erts_smp_mtx_unlock(&erts_timeofday_mtx);

    if (ms_user_diff != NULL)
	*ms_user_diff = total_user - prev_total_user;
	  
    if (ms_sys_diff != NULL)
	*ms_sys_diff = total_sys - prev_total_sys;
}


/* wall clock routines */

void 
wall_clock_elapsed_time_both(UWord *ms_total, UWord *ms_diff)
{
    ErtsMonotonicTime now, elapsed;

    erts_smp_mtx_lock(&erts_timeofday_mtx);

    now = time_sup.r.o.get_time();

    elapsed = ERTS_MONOTONIC_TO_MSEC(now);
    *ms_total = (UWord) elapsed;
    *ms_diff = (UWord) (elapsed - prev_wall_clock_elapsed);
    prev_wall_clock_elapsed = elapsed;

    erts_smp_mtx_unlock(&erts_timeofday_mtx);
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
  /* number of days in all months preceeding month */
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
    ErtsMonotonicTime now_megasec, now_sec, now, mtime, time_offset;
    
    mtime = time_sup.r.o.get_time();
    time_offset = get_time_offset();
    now = ERTS_MONOTONIC_TO_USEC(mtime + time_offset);

    erts_smp_mtx_lock(&erts_timeofday_mtx);

    /* Make sure now time is later than last time */
    if (now <= previous_now)
	now = previous_now + 1;

    previous_now = now;
    
    erts_smp_mtx_unlock(&erts_timeofday_mtx);

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
erts_get_monotonic_time(void)
{
    return time_sup.r.o.get_time();
}

void
get_sys_now(Uint* megasec, Uint* sec, Uint* microsec)
{
    SysTimeval now;
    
    sys_gettimeofday(&now);
    
    *megasec = (Uint) (now.tv_sec / 1000000);
    *sec = (Uint) (now.tv_sec % 1000000);
    *microsec = (Uint) (now.tv_usec);
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
erts_monitor_time_offset(Eterm id, Eterm ref)
{
    erts_smp_mtx_lock(&erts_get_time_mtx);
    erts_add_monitor(&time_offset_monitors, MON_TIME_OFFSET, ref, id, NIL);
    no_time_offset_monitors++;
    erts_smp_mtx_unlock(&erts_get_time_mtx);
}

int
erts_demonitor_time_offset(Eterm ref)
{
    int res;
    ErtsMonitor *mon;
    ASSERT(is_internal_ref(ref));
    erts_smp_mtx_lock(&erts_get_time_mtx);
    mon = erts_remove_monitor(&time_offset_monitors, ref);
    if (!mon)
	res = 0;
    else {
	ASSERT(no_time_offset_monitors > 0);
	no_time_offset_monitors--;
	res = 1;
    }
    erts_smp_mtx_unlock(&erts_get_time_mtx);
    if (res)
	erts_destroy_monitor(mon);
    return res;
}

typedef struct {
    Eterm pid;
    Eterm ref;
    Eterm heap[REF_THING_SIZE];
} ErtsTimeOffsetMonitorInfo;

typedef struct {
    Uint ix;
    ErtsTimeOffsetMonitorInfo *to_mon_info;
} ErtsTimeOffsetMonitorContext;

static void
save_time_offset_monitor(ErtsMonitor *mon, void *vcntxt)
{
    ErtsTimeOffsetMonitorContext *cntxt;
    Eterm *from_hp, *to_hp;
    Uint mix;
    int hix;

    cntxt = (ErtsTimeOffsetMonitorContext *) vcntxt;
    mix = (cntxt->ix)++;
    cntxt->to_mon_info[mix].pid = mon->pid;
    to_hp = &cntxt->to_mon_info[mix].heap[0];

    ASSERT(is_internal_ref(mon->ref));
    from_hp = internal_ref_val(mon->ref);
    ASSERT(thing_arityval(*from_hp) + 1 == REF_THING_SIZE);

    for (hix = 0; hix < REF_THING_SIZE; hix++)
	to_hp[hix] = from_hp[hix];

    cntxt->to_mon_info[mix].ref
	= make_internal_ref(&cntxt->to_mon_info[mix].heap[0]);

}

static void
send_time_offset_changed_notifications(void *new_offsetp)
{
    ErtsMonotonicTime new_offset;
    ErtsTimeOffsetMonitorInfo *to_mon_info;
    Uint no_monitors;
    char *tmp = NULL;

#ifdef ARCH_64
    new_offset = (ErtsMonotonicTime) new_offsetp;
#else
    new_offset = *((ErtsMonotonicTime *) new_offsetp);
    erts_free(ERTS_ALC_T_NEW_TIME_OFFSET, new_offsetp);
#endif
    new_offset -= ERTS_MONOTONIC_OFFSET_NATIVE;

    erts_smp_mtx_lock(&erts_get_time_mtx);

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

	erts_doforall_monitors(time_offset_monitors,
			       save_time_offset_monitor,
			       &cntxt);

	ASSERT(cntxt.ix == no_monitors);
    }

    erts_smp_mtx_unlock(&erts_get_time_mtx);

    if (no_monitors) {
	Eterm *hp, *patch_refp, new_offset_term, message_template;
	Uint mix, hsz;

	/* Make message template */

	hp = (Eterm *) (tmp + no_monitors*sizeof(ErtsTimeOffsetMonitorInfo));

	hsz = 6; /* 5-tuple */
	hsz += REF_THING_SIZE;
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
	    Process *rp = erts_proc_lookup(to_mon_info[mix].pid);
	    if (rp) {
		Eterm ref = to_mon_info[mix].ref;
		ErtsProcLocks rp_locks = ERTS_PROC_LOCK_LINK;
		erts_smp_proc_lock(rp, ERTS_PROC_LOCK_LINK);
		if (erts_lookup_monitor(ERTS_P_MONITORS(rp), ref)) {
		    ErlHeapFragment *bp;
		    ErlOffHeap *ohp;
		    Eterm message;

		    hp = erts_alloc_message_heap(hsz, &bp, &ohp, rp, &rp_locks);
		    *patch_refp = ref;
		    ASSERT(hsz == size_object(message_template));
		    message = copy_struct(message_template, hsz, &hp, ohp);
		    erts_queue_message(rp, &rp_locks, bp, message, NIL
#ifdef USE_VM_PROBES
				       , NIL
#endif
			);
		}
		erts_smp_proc_unlock(rp, rp_locks);
	    }
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
    return make_time_val(c_p, ERTS_MONOTONIC_OFFSET_NATIVE);
}

static Eterm
bld_monotonic_time_source(Uint **hpp, Uint *szp, Sint64 os_mtime)
{
#ifndef ERTS_HAVE_OS_MONOTONIC_TIME_SUPPORT
    return NIL;
#else
    int i = 0;
    Eterm k[5];
    Eterm v[5];

    if (time_sup.r.o.os_monotonic_disable)
	return NIL;

    k[i] = erts_bld_atom(hpp, szp, "function");
    v[i++] = erts_bld_atom(hpp, szp, time_sup.r.o.os_monotonic_func);

    if (time_sup.r.o.os_monotonic_clock_id) {
	k[i] = erts_bld_atom(hpp, szp, "clock_id");
	v[i++] = erts_bld_atom(hpp, szp, time_sup.r.o.os_monotonic_clock_id);
    }

    if (time_sup.r.o.os_monotonic_resolution) {
	k[i] = erts_bld_atom(hpp, szp, "resolution");
	v[i++] = erts_bld_uint64(hpp, szp, time_sup.r.o.os_monotonic_resolution);
    }

    k[i] = erts_bld_atom(hpp, szp, "parallel");
    v[i++] = time_sup.r.o.os_monotonic_locked ? am_no : am_yes;

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
    if (!time_sup.r.o.os_monotonic_disable)
	os_mtime = (Sint64) erts_os_monotonic_time();
#endif

    bld_monotonic_time_source(NULL, &hsz, os_mtime);
    if (hsz)
	hp = HAlloc(c_p, hsz);
    return bld_monotonic_time_source(&hp, NULL, os_mtime);
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
    case am_seconds:
    case make_small(1):
	result = ERTS_MONOTONIC_TO_SEC(val) + muloff*ERTS_MONOTONIC_OFFSET_SEC;
	ERTS_BIF_PREP_RET(ret, make_time_val(c_p, result));
	break;
    case am_milli_seconds:
    case make_small(1000):
	result = ERTS_MONOTONIC_TO_MSEC(val) + muloff*ERTS_MONOTONIC_OFFSET_MSEC;
	ERTS_BIF_PREP_RET(ret, make_time_val(c_p, result));
	break;
    case am_micro_seconds:
    case make_small(1000*1000):
	result = ERTS_MONOTONIC_TO_USEC(val) + muloff*ERTS_MONOTONIC_OFFSET_USEC;
	ERTS_BIF_PREP_RET(ret, make_time_val(c_p, result));
	break;
#ifdef ARCH_64
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
	if (term == am_nano_seconds)
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

/* Built in functions */

BIF_RETTYPE monotonic_time_0(BIF_ALIST_0)
{
    ErtsMonotonicTime mtime = time_sup.r.o.get_time();
    mtime += ERTS_MONOTONIC_OFFSET_NATIVE;
    BIF_RET(make_time_val(BIF_P, mtime));
}

BIF_RETTYPE monotonic_time_1(BIF_ALIST_1)
{
    BIF_RET(time_unit_conversion(BIF_P, BIF_ARG_1, time_sup.r.o.get_time(), 1));
}

BIF_RETTYPE system_time_0(BIF_ALIST_0)
{
    ErtsMonotonicTime mtime, offset;
    mtime = time_sup.r.o.get_time();
    offset = get_time_offset();
    BIF_RET(make_time_val(BIF_P, mtime + offset));
}

BIF_RETTYPE system_time_1(BIF_ALIST_0)
{
    ErtsMonotonicTime mtime, offset;
    mtime = time_sup.r.o.get_time();
    offset = get_time_offset();
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
    ErtsMonotonicTime stime, mtime, all_sec, offset;
    Uint mega_sec, sec, micro_sec;

    mtime = time_sup.r.o.get_time();
    offset = get_time_offset();
    stime = ERTS_MONOTONIC_TO_USEC(mtime + offset);
    all_sec = stime / ERTS_MONOTONIC_TIME_MEGA;
    mega_sec = (Uint) (stime / ERTS_MONOTONIC_TIME_TERA);
    sec = (Uint) (all_sec - (((ErtsMonotonicTime) mega_sec)
			     * ERTS_MONOTONIC_TIME_MEGA));
    micro_sec = (Uint) (stime - all_sec*ERTS_MONOTONIC_TIME_MEGA);

    ASSERT(((ErtsMonotonicTime) mega_sec)*ERTS_MONOTONIC_TIME_TERA
	   + ((ErtsMonotonicTime) sec)*ERTS_MONOTONIC_TIME_MEGA
	   + micro_sec == stime);

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
    ErtsMonotonicTime stime;
    SysTimeval tod;
    sys_gettimeofday(&tod);
    stime = ERTS_SEC_TO_MONOTONIC(tod.tv_sec);
    stime += ERTS_USEC_TO_MONOTONIC(tod.tv_usec);
    BIF_RET(make_time_val(BIF_P, stime));
}

BIF_RETTYPE os_system_time_1(BIF_ALIST_0)
{
    ErtsMonotonicTime stime;
    SysTimeval tod;
    sys_gettimeofday(&tod);
    stime = ERTS_SEC_TO_MONOTONIC(tod.tv_sec);
    stime += ERTS_USEC_TO_MONOTONIC(tod.tv_usec);
    BIF_RET(time_unit_conversion(BIF_P, BIF_ARG_1, stime, 0));
}

