/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2005-2009. All Rights Reserved.
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

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

/* These need to be undef:ed to not break activation of
 * micro level process accounting on /proc/self 
 */
#ifdef _LARGEFILE_SOURCE
#  undef _LARGEFILE_SOURCE
#endif
#ifdef _FILE_OFFSET_BITS
#  undef _FILE_OFFSET_BITS
#endif

#include "sys.h"
#include "global.h"

#ifdef NO_SYSCONF
#  define TICKS_PER_SEC()	HZ
#else
#define TICKS_PER_SEC()	sysconf(_SC_CLK_TCK)
#endif

#ifdef HAVE_GETHRVTIME_PROCFS_IOCTL
#  include <unistd.h>
#  include <sys/types.h>
#  include <sys/stat.h>
#  include <sys/signal.h>
#  include <sys/fault.h>
#  include <sys/syscall.h>
#  include <sys/procfs.h>
#  include <fcntl.h>
#endif

/******************* Routines for time measurement *********************/

#undef ERTS_SYS_TIME_INTERNAL_STATE_WRITE_FREQ__
#undef ERTS_SYS_TIME_INTERNAL_STATE_READ_ONLY__

#if defined(OS_MONOTONIC_TIME_USING_TIMES)

#define ERTS_WRAP_SYS_TIMES 1
#define ERTS_SYS_TIME_INTERNAL_STATE_WRITE_FREQ__
#define ERTS_SYS_TIME_INTERNAL_STATE_READ_ONLY__

/*
 * Not sure there is a need to use times() anymore, perhaps drop
 * support for this soon...
 *
 * sys_times() might need to be wrapped and the values shifted (right)
 * a bit to cope with faster ticks, this has to be taken care 
 * of dynamically to start with, a special version that uses
 * the times() return value as a high resolution timer can be made
 * to fully utilize the faster ticks, like on windows, but for now, we'll
 * settle with this silly workaround
 */
#ifdef ERTS_WRAP_SYS_TIMES
static clock_t sys_times_wrap(void);
#define KERNEL_TICKS() (sys_times_wrap() &  \
			((1UL << ((sizeof(clock_t) * 8) - 1)) - 1)) 
#define ERTS_KERNEL_TICK_TO_USEC(TCKS) (((TCKS)*(1000*1000)) \
					/ internal_state.r.o.ticks_per_sec_wrap)
#else

#define KERNEL_TICKS() (sys_times(&internal_state.w.f.dummy_tms) &  \
			((1UL << ((sizeof(clock_t) * 8) - 1)) - 1)) 
#define ERTS_KERNEL_TICK_TO_USEC(TCKS) (((TCKS)*(1000*1000))/SYS_CLK_TCK)
#endif

#endif

/* 
 * init timers, chose a tick length, and return it.
 * Unix is priviliged when it comes to time, as erl_time_sup.c 
 * does almost everything. Other platforms have to
 * emulate Unix in this sense.
 */

ErtsSysTimeData__ erts_sys_time_data__ erts_align_attribute(ERTS_CACHE_LINE_SIZE);

#if defined(__linux__) && defined(OS_MONOTONIC_TIME_USING_CLOCK_GETTIME)

#define ERTS_SYS_TIME_INTERNAL_STATE_WRITE_FREQ__

ErtsMonotonicTime clock_gettime_monotonic_raw(void);
ErtsMonotonicTime clock_gettime_monotonic_verified(void);

#endif /* defined(__linux__) && defined(OS_MONOTONIC_TIME_USING_CLOCK_GETTIME) */

#ifdef ERTS_SYS_TIME_INTERNAL_STATE_READ_ONLY__
struct sys_time_internal_state_read_only__ {
#if defined(OS_MONOTONIC_TIME_USING_TIMES)
    int ticks_bsr;
    int ticks_per_sec_wrap;
#endif
};
#endif

#ifdef ERTS_SYS_TIME_INTERNAL_STATE_WRITE_FREQ__
struct sys_time_internal_state_write_freq__ {
    erts_smp_mtx_t mtx;
#if defined(__linux__) && defined(OS_MONOTONIC_TIME_USING_CLOCK_GETTIME)
    ErtsMonotonicTime last_delivered;
#endif
#if defined(OS_MONOTONIC_TIME_USING_TIMES)
    ErtsMonotonicTime last_tick_count;
    ErtsMonotonicTime last_tick_wrap_count;
    ErtsMonotonicTime last_tick_monotonic_time;
    ErtsMonotonicTime last_timeofday_usec;
#ifndef ERTS_WRAP_SYS_TIMES 
    SysTimes dummy_tms;
#endif
#endif
};
#endif

#if defined(ERTS_SYS_TIME_INTERNAL_STATE_READ_ONLY__)	\
    || defined(ERTS_SYS_TIME_INTERNAL_STATE_WRITE_FREQ__)
static struct {
#ifdef ERTS_SYS_TIME_INTERNAL_STATE_READ_ONLY__
    union {
	struct sys_time_internal_state_read_only__ o;
	char align__[(((sizeof(struct sys_time_internal_state_read_only__) - 1)
		       / ASSUMED_CACHE_LINE_SIZE) + 1)
		     * ASSUMED_CACHE_LINE_SIZE];
    } r;
#endif
#ifdef ERTS_SYS_TIME_INTERNAL_STATE_WRITE_FREQ__
    union {
	struct sys_time_internal_state_write_freq__ f;
	char align__[(((sizeof(struct sys_time_internal_state_write_freq__) - 1)
		       / ASSUMED_CACHE_LINE_SIZE) + 1)
		     * ASSUMED_CACHE_LINE_SIZE];
    } w;
#endif
} internal_state erts_align_attribute(ERTS_CACHE_LINE_SIZE);
#endif

void
sys_init_time(ErtsSysInitTimeResult *init_resp)
{
#if !defined(ERTS_HAVE_OS_MONOTONIC_TIME_SUPPORT)

    init_resp->have_os_monotonic = 0;

#else /* defined(ERTS_HAVE_OS_MONOTONIC_TIME_SUPPORT) */

    int major, minor, build, vsn;

    init_resp->os_monotonic_info.resolution = (Uint64) 1000*1000*1000;
#if defined(HAVE_CLOCK_GETRES) && defined(MONOTONIC_CLOCK_ID)
    {
	struct timespec ts;
	if (clock_getres(MONOTONIC_CLOCK_ID, &ts) == 0
	    && ts.tv_sec == 0 && ts.tv_nsec != 0) {
	    init_resp->os_monotonic_info.resolution /= ts.tv_nsec;
	}
    }
#endif

#ifdef MONOTONIC_CLOCK_ID_STR
    init_resp->os_monotonic_info.clock_id = MONOTONIC_CLOCK_ID_STR;
#else
    init_resp->os_monotonic_info.clock_id = NULL;
#endif

    init_resp->os_monotonic_info.locked_use = 0;

#if defined(OS_MONOTONIC_TIME_USING_CLOCK_GETTIME)
    init_resp->os_monotonic_info.func = "clock_gettime";
#elif defined(OS_MONOTONIC_TIME_USING_MACH_CLOCK_GET_TIME)
    init_resp->os_monotonic_info.func = "clock_get_time";
#elif defined(OS_MONOTONIC_TIME_USING_GETHRTIME)
    init_resp->os_monotonic_info.func = "gethrtime";
#elif defined(OS_MONOTONIC_TIME_USING_TIMES)
    init_resp->os_monotonic_info.func = "times";
    init_resp->os_monotonic_info.locked_use = 1;
    init_resp->os_monotonic_info.resolution = TICKS_PER_SEC();
#else
# error Unknown erts_os_monotonic_time() implementation
#endif

    init_resp->have_os_monotonic = 1;

    os_version(&major, &minor, &build);

    vsn = ERTS_MK_VSN_INT(major, minor, build);


#if defined(__linux__) && defined(OS_MONOTONIC_TIME_USING_CLOCK_GETTIME)
    if (vsn >= ERTS_MK_VSN_INT(2, 6, 33))
	erts_sys_time_data__.r.o.os_monotonic_time =
	    clock_gettime_monotonic_raw;
    else {
	/*
	 * Linux versions prior to 2.6.33 have a
	 * known bug that sometimes cause monotonic
	 * time to take small steps backwards.
	 */
	erts_sys_time_data__.r.o.os_monotonic_time =
	    clock_gettime_monotonic_verified;
	erts_smp_mtx_init(&internal_state.w.f.mtx,
			  "os_monotonic_time");
	internal_state.w.f.last_delivered
	    = clock_gettime_monotonic_raw();
	init_resp->os_monotonic_info.locked_use = 1;
    }
#else /* !(defined(__linux__) && defined(OS_MONOTONIC_TIME_USING_CLOCK_GETTIME)) */
    {
	char flavor[1024];

	os_flavor(flavor, sizeof(flavor));

	if (sys_strcmp(flavor, "sunos") == 0) {
	    /*
	     * Don't trust hrtime on multi processors
	     * on SunOS prior to SunOS 5.8
	     */
	    if (vsn < ERTS_MK_VSN_INT(5, 8, 0)) {
#if defined(HAVE_SYSCONF) && defined(_SC_NPROCESSORS_CONF)
		if (sysconf(_SC_NPROCESSORS_CONF) > 1)
#endif
		    init_resp->have_os_monotonic = 0;
	    }
	}
    }
#endif /* !(defined(__linux__) && defined(OS_MONOTONIC_TIME_USING_CLOCK_GETTIME)) */

#endif /* defined(ERTS_HAVE_OS_MONOTONIC_TIME_SUPPORT) */

    init_resp->os_monotonic_time_unit = ERTS_COMPILE_TIME_MONOTONIC_TIME_UNIT;
    init_resp->sys_clock_resolution = SYS_CLOCK_RESOLUTION;

    /* 
     * This (erts_sys_time_data__.r.o.ticks_per_sec) is only for
     * times() (CLK_TCK), the resolution is always one millisecond..
     */
    if ((erts_sys_time_data__.r.o.ticks_per_sec = TICKS_PER_SEC()) < 0)
	erl_exit(ERTS_ABORT_EXIT, "Can't get clock ticks/sec\n");
    
#if defined(OS_MONOTONIC_TIME_USING_TIMES)

    if (erts_sys_time_data__.r.o.ticks_per_sec >= 1000) {
	/* Workaround for beta linux kernels, need to be done in runtime
	   to make erlang run on both 2.4 and 2.5 kernels. In the future, 
	   the kernel ticks might as 
	   well be used as a high res timer instead, but that's for when the 
	   majority uses kernels with HZ == 1024 */
	internal_state.r.o.ticks_bsr = 3;
    } else {
	internal_state.r.o.ticks_bsr = 0;
    }

    internal_state.r.o.ticks_per_sec_wrap
	= (erts_sys_time_data__.r.o.ticks_per_sec
	   >> internal_state.r.o.ticks_bsr);

    erts_smp_mtx_init(&internal_state.w.f.mtx, "os_monotonic_time");
    internal_state.w.f.last_tick_count = KERNEL_TICKS();
    internal_state.w.f.last_tick_wrap_count = 0;    
    internal_state.w.f.last_tick_monotonic_time
	= ERTS_KERNEL_TICK_TO_USEC(internal_state.w.f.last_tick_count);
    {
	SysTimeval tv;
	sys_gettimeofday(&tv);
	internal_state.w.f.last_timeofday_usec = tv.tv_sec*(1000*1000);
	internal_state.w.f.last_timeofday_usec += tv.tv_usec;
    }

#endif /* defined(OS_MONOTONIC_TIME_USING_TIMES) */

}

#if defined(OS_MONOTONIC_TIME_USING_CLOCK_GETTIME)

static ERTS_INLINE ErtsMonotonicTime
clock_gettime_monotonic(void)
{
    ErtsMonotonicTime mtime;
    struct timespec ts;

    if (clock_gettime(MONOTONIC_CLOCK_ID,&ts) != 0) {
	int err = errno;
	char *errstr = err ? strerror(err) : "unknown";
	erl_exit(ERTS_ABORT_EXIT,
		 "clock_gettime(%s, _) failed: %s (%d)\n",
		 MONOTONIC_CLOCK_ID_STR, errstr, err);

    }
    mtime = (ErtsMonotonicTime) ts.tv_sec;
    mtime *= (ErtsMonotonicTime) 1000*1000*1000;
    mtime += (ErtsMonotonicTime) ts.tv_nsec;
    return mtime;
}

#if defined(__linux__)

ErtsMonotonicTime clock_gettime_monotonic_verified(void)
{
    ErtsMonotonicTime mtime;

    mtime = clock_gettime_monotonic();

    erts_smp_mtx_lock(&internal_state.w.f.mtx);
    if (mtime < internal_state.w.f.last_delivered)
	mtime = internal_state.w.f.last_delivered;
    else
	internal_state.w.f.last_delivered = mtime;
    erts_smp_mtx_unlock(&internal_state.w.f.mtx);

    return mtime;
}

ErtsMonotonicTime clock_gettime_monotonic_raw(void)
{
    return clock_gettime_monotonic();
}

#else /* !defined(__linux__) */

ErtsMonotonicTime erts_os_monotonic_time(void)
{
    return clock_gettime_monotonic();
}

#endif /* !defined(__linux__) */

#elif defined(OS_MONOTONIC_TIME_USING_MACH_CLOCK_GET_TIME)

#include <mach/clock.h>
#include <mach/mach.h>

ErtsMonotonicTime erts_os_monotonic_time(void)
{
    ErtsMonotonicTime mtime;
    kern_return_t res;
    clock_serv_t clk_srv;
    mach_timespec_t time_spec;
    int err;

    host_get_clock_service(mach_host_self(),
			   MONOTONIC_CLOCK_ID,
			   &clk_srv);
    errno = 0;
    res = clock_get_time(clk_srv, &time_spec);
    err = errno;
    mach_port_deallocate(mach_task_self(), clk_srv);
    if (res != KERN_SUCCESS) {
	char *errstr = err ? strerror(err) : "unknown";
	erl_exit(ERTS_ABORT_EXIT,
		 "clock_get_time(%s, _) failed: %s (%d)\n",
		 MONOTONIC_CLOCK_ID_STR, errstr, err);
    }

    mtime = (ErtsMonotonicTime) time_spec.tv_sec;
    mtime *= (ErtsMonotonicTime) 1000*1000*1000;
    mtime += (ErtsMonotonicTime) time_spec.tv_nsec;
    return mtime;
}

#elif defined(OS_MONOTONIC_TIME_USING_TIMES)

static clock_t sys_times_wrap(void)
{
    SysTimes dummy;
    clock_t result = (sys_times(&dummy) >> internal_state.r.o.ticks_bsr);
    return result;
}

void
erts_os_time_offset_finalize(void)
{
    erts_smp_mtx_lock(&internal_state.w.f.mtx);
    internal_state.w.f.last_tick_wrap_count = 0;
    erts_smp_mtx_unlock(&internal_state.w.f.mtx);
}

#define ERTS_TIME_EXCEED_TICK_LIMIT(SYS_TIME, TCK_TIME)			\
    (((Uint64) (SYS_TIME)) - (((Uint64) (TCK_TIME))			\
			      - ERTS_KERNEL_TICK_TO_USEC(1))		\
     > ERTS_KERNEL_TICK_TO_USEC(2))

/* Returns monotonic time in micro seconds */
ErtsMonotonicTime
erts_os_monotonic_time(void)
{
    SysTimeval tv;
    ErtsMonotonicTime res;
    ErtsMonotonicTime tick_count;
    ErtsMonotonicTime tick_count_usec;
    ErtsMonotonicTime tick_monotonic_time;
    ErtsMonotonicTime timeofday_usec;
    ErtsMonotonicTime timeofday_diff_usec;

    erts_smp_mtx_lock(&internal_state.w.f.mtx);

    tick_count = (ErtsMonotonicTime) KERNEL_TICKS();
    sys_gettimeofday(&tv);

    if (internal_state.w.f.last_tick_count > tick_count) {
	internal_state.w.f.last_tick_wrap_count
	    += (((ErtsMonotonicTime) 1) << ((sizeof(clock_t) * 8) - 1));
    }
    internal_state.w.f.last_tick_count = tick_count;
    tick_count += internal_state.w.f.last_tick_wrap_count;

    tick_count_usec = ERTS_KERNEL_TICK_TO_USEC(tick_count);

    timeofday_usec = (ErtsMonotonicTime) tv.tv_sec*(1000*1000);
    timeofday_usec += (ErtsMonotonicTime) tv.tv_usec;
    timeofday_diff_usec = timeofday_usec;
    timeofday_diff_usec -= internal_state.w.f.last_timeofday_usec;
    internal_state.w.f.last_timeofday_usec = timeofday_usec;

    if (timeofday_diff_usec < 0) {
	/* timeofday jumped backwards use tick count only... */
	tick_monotonic_time = tick_count_usec;
    }
    else {
	/* Use time diff from of timeofday if not off by too much... */
	tick_monotonic_time = internal_state.w.f.last_tick_monotonic_time;
	tick_monotonic_time += timeofday_diff_usec;

	if (ERTS_TIME_EXCEED_TICK_LIMIT(tick_monotonic_time, tick_count_usec)) {
	    /*
	     * Value off by more than one tick from tick_count, i.e.
	     * timofday leaped one way or the other. We use
	     * tick_count_usec as is instead and unfortunately
	     * get lousy precision.
	     */
	    tick_monotonic_time = tick_count_usec;
	}
    }

    if (internal_state.w.f.last_tick_monotonic_time < tick_monotonic_time)
	internal_state.w.f.last_tick_monotonic_time = tick_monotonic_time;

    res = internal_state.w.f.last_tick_monotonic_time;

    erts_smp_mtx_unlock(&internal_state.w.f.mtx);

    return res;
}

#endif  /* !defined(OS_MONOTONIC_TIME_USING_TIMES) */

#ifdef HAVE_GETHRVTIME_PROCFS_IOCTL

int sys_start_hrvtime(void)
{
    long msacct = PR_MSACCT;
    int fd;

    if ( (fd = open("/proc/self", O_WRONLY)) == -1) {
	return -1;
    }
    if (ioctl(fd, PIOCSET, &msacct) < 0) {
	close(fd);
	return -2;
    }
    close(fd);
    return 0;
}

int sys_stop_hrvtime(void)
{
    long msacct = PR_MSACCT;
    int fd;

    if ( (fd = open("/proc/self", O_WRONLY)) == -1) {
	return -1;
    }
    if (ioctl(fd, PIOCRESET, &msacct) < 0) {
	close(fd);
	return -2;
    }
    close(fd);
    return 0;
}

#endif /* HAVE_GETHRVTIME_PROCFS_IOCTL */


