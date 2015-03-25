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
#include "erl_os_monotonic_time_extender.h"

#undef ERTS_HAVE_ERTS_OS_TIMES_IMPL__
#undef ERTS_HAVE_ERTS_SYS_HRTIME_IMPL__

#if defined(OS_MONOTONIC_TIME_USING_MACH_CLOCK_GET_TIME) \
    || defined(OS_SYSTEM_TIME_USING_MACH_CLOCK_GET_TIME)
# include <mach/clock.h>
# include <mach/mach.h>
#  ifdef HAVE_CLOCK_GET_ATTRIBUTES
#  define ERTS_HAVE_MACH_CLOCK_GETRES
static Sint64
mach_clock_getres(clock_id_t clkid, char *clkid_str);
#  endif
#endif

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
#undef ERTS_SYS_TIME_INTERNAL_STATE_READ_MOSTLY__

#if defined(OS_MONOTONIC_TIME_USING_TIMES)

static Uint32
get_tick_count(void)
{
    struct tms unused;
    return (Uint32) times(&unused);
}

#define ERTS_SYS_TIME_INTERNAL_STATE_READ_ONLY__
#define ERTS_SYS_TIME_INTERNAL_STATE_READ_MOSTLY__

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

static ErtsMonotonicTime clock_gettime_monotonic_raw(void);
static ErtsMonotonicTime clock_gettime_monotonic_verified(void);
#if defined(OS_SYSTEM_TIME_USING_CLOCK_GETTIME)
static void clock_gettime_times_raw(ErtsMonotonicTime *, ErtsSystemTime *);
static void clock_gettime_times_verified(ErtsMonotonicTime *, ErtsSystemTime *);
#endif

#endif /* defined(__linux__) && defined(OS_MONOTONIC_TIME_USING_CLOCK_GETTIME) */

#ifdef ERTS_SYS_TIME_INTERNAL_STATE_READ_ONLY__
struct sys_time_internal_state_read_only__ {
#if defined(OS_MONOTONIC_TIME_USING_TIMES)
    int times_shift;
#endif
};
#endif

#ifdef ERTS_SYS_TIME_INTERNAL_STATE_READ_MOSTLY__
struct sys_time_internal_state_read_mostly__ {
#if defined(OS_MONOTONIC_TIME_USING_TIMES)
    ErtsOsMonotonicTimeExtendState os_mtime_xtnd;
#endif
};
#endif

#ifdef ERTS_SYS_TIME_INTERNAL_STATE_WRITE_FREQ__
struct sys_time_internal_state_write_freq__ {
    erts_smp_mtx_t mtx;
#if defined(__linux__) && defined(OS_MONOTONIC_TIME_USING_CLOCK_GETTIME)
    ErtsMonotonicTime last_delivered;
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
#ifdef ERTS_SYS_TIME_INTERNAL_STATE_READ_MOSTLY__
    union {
	struct sys_time_internal_state_read_mostly__ m;
	char align__[(((sizeof(struct sys_time_internal_state_read_mostly__) - 1)
		       / ASSUMED_CACHE_LINE_SIZE) + 1)
		     * ASSUMED_CACHE_LINE_SIZE];
    } wr;
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

    init_resp->have_os_monotonic_time = 0;

#else /* defined(ERTS_HAVE_OS_MONOTONIC_TIME_SUPPORT) */

    int major, minor, build, vsn;

    init_resp->os_monotonic_time_info.resolution = (Uint64) 1000*1000*1000;
#if defined(HAVE_CLOCK_GETRES) && defined(MONOTONIC_CLOCK_ID)
    {
	struct timespec ts;
	if (clock_getres(MONOTONIC_CLOCK_ID, &ts) == 0) {
	    if (ts.tv_sec == 0 && ts.tv_nsec != 0)
		init_resp->os_monotonic_time_info.resolution /= ts.tv_nsec;
	    else if (ts.tv_sec >= 1)
		init_resp->os_monotonic_time_info.resolution = 1;
	}
    }
#elif defined(ERTS_HAVE_MACH_CLOCK_GETRES) && defined(MONOTONIC_CLOCK_ID)
    init_resp->os_monotonic_time_info.resolution
	= mach_clock_getres(MONOTONIC_CLOCK_ID, MONOTONIC_CLOCK_ID_STR);
#endif

#ifdef MONOTONIC_CLOCK_ID_STR
    init_resp->os_monotonic_time_info.clock_id = MONOTONIC_CLOCK_ID_STR;
#else
    init_resp->os_monotonic_time_info.clock_id = NULL;
#endif

    init_resp->os_monotonic_time_info.locked_use = 0;

#if defined(OS_MONOTONIC_TIME_USING_CLOCK_GETTIME)
    init_resp->os_monotonic_time_info.func = "clock_gettime";
#elif defined(OS_MONOTONIC_TIME_USING_MACH_CLOCK_GET_TIME)
    init_resp->os_monotonic_time_info.func = "clock_get_time";
#elif defined(OS_MONOTONIC_TIME_USING_GETHRTIME)
    init_resp->os_monotonic_time_info.func = "gethrtime";
#elif defined(OS_MONOTONIC_TIME_USING_TIMES)
    init_resp->os_monotonic_time_info.func = "times";
#else
# error Unknown erts_os_monotonic_time() implementation
#endif

    init_resp->have_os_monotonic_time = 1;

    os_version(&major, &minor, &build);

    vsn = ERTS_MK_VSN_INT(major, minor, build);


#if defined(__linux__) && defined(OS_MONOTONIC_TIME_USING_CLOCK_GETTIME)
    if (vsn >= ERTS_MK_VSN_INT(2, 6, 33)) {
	erts_sys_time_data__.r.o.os_monotonic_time =
	    clock_gettime_monotonic_raw;
#if defined(OS_SYSTEM_TIME_USING_CLOCK_GETTIME)
	erts_sys_time_data__.r.o.os_times =
	    clock_gettime_times_raw;
#endif
    }
    else {
	/*
	 * Linux versions prior to 2.6.33 have a
	 * known bug that sometimes cause monotonic
	 * time to take small steps backwards.
	 */
	erts_sys_time_data__.r.o.os_monotonic_time =
	    clock_gettime_monotonic_verified;
#if defined(OS_SYSTEM_TIME_USING_CLOCK_GETTIME)
	erts_sys_time_data__.r.o.os_times =
	    clock_gettime_times_verified;
#endif
	erts_smp_mtx_init(&internal_state.w.f.mtx,
			  "os_monotonic_time");
	internal_state.w.f.last_delivered
	    = clock_gettime_monotonic_raw();
	init_resp->os_monotonic_time_info.locked_use = 1;
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
		    init_resp->have_os_monotonic_time = 0;
	    }
	}
    }
#endif /* !(defined(__linux__) && defined(OS_MONOTONIC_TIME_USING_CLOCK_GETTIME)) */

#endif /* defined(ERTS_HAVE_OS_MONOTONIC_TIME_SUPPORT) */

#ifdef ERTS_COMPILE_TIME_MONOTONIC_TIME_UNIT
    init_resp->os_monotonic_time_unit = ERTS_COMPILE_TIME_MONOTONIC_TIME_UNIT;
#endif
    init_resp->sys_clock_resolution = SYS_CLOCK_RESOLUTION;

    /* 
     * This (erts_sys_time_data__.r.o.ticks_per_sec) is only for
     * times() (CLK_TCK), the resolution is always one millisecond..
     */
    if ((erts_sys_time_data__.r.o.ticks_per_sec = TICKS_PER_SEC()) < 0)
	erl_exit(ERTS_ABORT_EXIT, "Can't get clock ticks/sec\n");
    
#if defined(OS_MONOTONIC_TIME_USING_TIMES)
#if ERTS_COMPILE_TIME_MONOTONIC_TIME_UNIT
#  error Time unit is supposed to be determined at runtime...
#endif
    {
	ErtsMonotonicTime resolution = erts_sys_time_data__.r.o.ticks_per_sec;
	ErtsMonotonicTime time_unit = resolution;
	int shift = 0;

	while (time_unit < 1000*1000) {
	    time_unit <<= 1;
	    shift++;
	}

	init_resp->os_monotonic_time_info.resolution = resolution;
	init_resp->os_monotonic_time_unit = time_unit;
	init_resp->os_monotonic_time_info.extended = 1;
	internal_state.r.o.times_shift = shift;

	erts_init_os_monotonic_time_extender(&internal_state.wr.m.os_mtime_xtnd,
					     get_tick_count,
					     (1 << 29) / resolution);
    }
#endif /* defined(OS_MONOTONIC_TIME_USING_TIMES) */

#ifdef WALL_CLOCK_ID_STR
    init_resp->os_system_time_info.clock_id = WALL_CLOCK_ID_STR;
#else
    init_resp->os_system_time_info.clock_id = NULL;
#endif

    init_resp->os_system_time_info.locked_use = 0;
    init_resp->os_system_time_info.resolution = (Uint64) 1000*1000*1000;
#if defined(HAVE_CLOCK_GETRES) && defined(WALL_CLOCK_ID)
    {
	struct timespec ts;
	if (clock_getres(WALL_CLOCK_ID, &ts) == 0) {
	    if (ts.tv_sec == 0 && ts.tv_nsec != 0)
		init_resp->os_system_time_info.resolution /= ts.tv_nsec;
	    else if (ts.tv_sec >= 1)
		init_resp->os_system_time_info.resolution = 1;
	}
    }
#elif defined(ERTS_HAVE_MACH_CLOCK_GETRES) && defined(WALL_CLOCK_ID)
    init_resp->os_system_time_info.resolution
	= mach_clock_getres(WALL_CLOCK_ID, WALL_CLOCK_ID_STR);
#endif

#if defined(OS_SYSTEM_TIME_USING_CLOCK_GETTIME)
    init_resp->os_system_time_info.func = "clock_gettime";
#elif defined(OS_SYSTEM_TIME_USING_MACH_CLOCK_GET_TIME)
    init_resp->os_system_time_info.func = "clock_get_time";
#elif defined(OS_SYSTEM_TIME_GETTIMEOFDAY)
    init_resp->os_system_time_info.func = "gettimeofday";    
    init_resp->os_system_time_info.resolution = 1000*1000;
    init_resp->os_system_time_info.clock_id = NULL;
#else
#  error Missing erts_os_system_time() implementation
#endif

}

void
erts_late_sys_init_time(void)
{
#if defined(OS_MONOTONIC_TIME_USING_TIMES)
    erts_late_init_os_monotonic_time_extender(&internal_state.wr.m.os_mtime_xtnd);
#endif
}

static ERTS_INLINE ErtsSystemTime
adj_stime_time_unit(ErtsSystemTime stime, Uint32 res)
{
    if (res == ERTS_COMPILE_TIME_MONOTONIC_TIME_UNIT)
	return stime;
    if (res == (Uint32) 1000*1000*1000
	&& ERTS_COMPILE_TIME_MONOTONIC_TIME_UNIT == 1000*1000)
	return stime/1000;
    if (res == (Uint32) 1000*1000
	&& ERTS_COMPILE_TIME_MONOTONIC_TIME_UNIT == 1000*1000*1000)
	return stime*1000;
    return ((ErtsSystemTime)
	    erts_time_unit_conversion(stime,
				      (Uint32) res,
				      (Uint32) ERTS_MONOTONIC_TIME_UNIT));
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * POSIX clock_gettime()                                                     *
\*                                                                           */

#if defined(OS_MONOTONIC_TIME_USING_CLOCK_GETTIME) \
    || defined(OS_SYSTEM_TIME_USING_CLOCK_GETTIME)

static ERTS_INLINE ErtsMonotonicTime
timespec2montime(struct timespec *ts)
{
    ErtsMonotonicTime time;
    time = (ErtsMonotonicTime) ts->tv_sec;
    time *= (ErtsMonotonicTime) 1000*1000*1000;
    time += (ErtsMonotonicTime) ts->tv_nsec;
    return time;
}

static ERTS_INLINE ErtsMonotonicTime
posix_clock_gettime(clockid_t id, char *name)
{
    struct timespec ts;

    if (clock_gettime(id, &ts) != 0) {
	int err = errno;
	char *errstr = err ? strerror(err) : "unknown";
	erl_exit(ERTS_ABORT_EXIT,
		 "clock_gettime(%s, _) failed: %s (%d)\n",
		 name, errstr, err);
    }
    return timespec2montime(&ts);
}

#endif /* defined(OS_MONOTONIC_TIME_USING_CLOCK_GETTIME) \
	  || defined(OS_SYSTEM_TIME_USING_CLOCK_GETTIME) */

#if defined(OS_SYSTEM_TIME_USING_CLOCK_GETTIME)

ErtsSystemTime
erts_os_system_time(void)
{
    ErtsSystemTime stime;

    stime = (ErtsSystemTime) posix_clock_gettime(WALL_CLOCK_ID,
						 WALL_CLOCK_ID_STR);
#if defined(OS_MONOTONIC_TIME_USING_CLOCK_GETTIME)
    return stime;
#else
    return adj_stime_time_unit(stime, (Uint32) 1000*1000*1000);
#endif
}

#endif /* defined(OS_SYSTEM_TIME_USING_CLOCK_GETTIME) */

#if defined(OS_MONOTONIC_TIME_USING_CLOCK_GETTIME)

#if defined(OS_SYSTEM_TIME_USING_CLOCK_GETTIME)

#define ERTS_HAVE_ERTS_OS_TIMES_IMPL__

static ERTS_INLINE void
posix_clock_gettime_times(ErtsMonotonicTime *mtimep,
			  ErtsSystemTime *stimep)
{
    struct timespec mts, sts;
    int mres, sres, merr, serr;

    mres = clock_gettime(MONOTONIC_CLOCK_ID, &mts);
    merr = errno;
    sres = clock_gettime(WALL_CLOCK_ID, &sts);
    serr = errno;
    
    if (mres != 0) {
	char *errstr = merr ? strerror(merr) : "unknown";
	erl_exit(ERTS_ABORT_EXIT,
		 "clock_gettime(%s, _) failed: %s (%d)\n",
		 MONOTONIC_CLOCK_ID_STR, errstr, merr);
    }
    if (sres != 0) {
	char *errstr = serr ? strerror(serr) : "unknown";
	erl_exit(ERTS_ABORT_EXIT,
		 "clock_gettime(%s, _) failed: %s (%d)\n",
		 WALL_CLOCK_ID_STR, errstr, serr);
    }

    *mtimep = timespec2montime(&mts);
    *stimep = (ErtsSystemTime) timespec2montime(&sts);
}

#endif /* defined(OS_SYSTEM_TIME_USING_CLOCK_GETTIME) */

#if defined(__linux__)

static ErtsMonotonicTime clock_gettime_monotonic_verified(void)
{
    ErtsMonotonicTime mtime = posix_clock_gettime(MONOTONIC_CLOCK_ID,
						  MONOTONIC_CLOCK_ID_STR);

    erts_smp_mtx_lock(&internal_state.w.f.mtx);
    if (mtime < internal_state.w.f.last_delivered)
	mtime = internal_state.w.f.last_delivered;
    else
	internal_state.w.f.last_delivered = mtime;
    erts_smp_mtx_unlock(&internal_state.w.f.mtx);

    return mtime;
}

#if defined(OS_SYSTEM_TIME_USING_CLOCK_GETTIME)

static void clock_gettime_times_verified(ErtsMonotonicTime *mtimep,
					 ErtsSystemTime *stimep)
{
    posix_clock_gettime_times(mtimep, stimep);

    erts_smp_mtx_lock(&internal_state.w.f.mtx);
    if (*mtimep < internal_state.w.f.last_delivered)
	*mtimep = internal_state.w.f.last_delivered;
    else
	internal_state.w.f.last_delivered = *mtimep;
    erts_smp_mtx_unlock(&internal_state.w.f.mtx);
}

#endif /* defined(OS_SYSTEM_TIME_USING_CLOCK_GETTIME) */

static ErtsMonotonicTime clock_gettime_monotonic_raw(void)
{
    return posix_clock_gettime(MONOTONIC_CLOCK_ID,
			       MONOTONIC_CLOCK_ID_STR);
}

#if defined(OS_SYSTEM_TIME_USING_CLOCK_GETTIME)

static void clock_gettime_times_raw(ErtsMonotonicTime *mtimep,
				    ErtsSystemTime *stimep)
{
    posix_clock_gettime_times(mtimep, stimep);
}

#endif /* defined(OS_SYSTEM_TIME_USING_CLOCK_GETTIME) */

#else /* !defined(__linux__) */

ErtsMonotonicTime erts_os_monotonic_time(void)
{
    return posix_clock_gettime(MONOTONIC_CLOCK_ID,
			       MONOTONIC_CLOCK_ID_STR);
}

#if defined(OS_SYSTEM_TIME_USING_CLOCK_GETTIME)

static void erts_os_times(ErtsMonotonicTime *mtimep,
			  ErtsSystemTime *stimep)
{
    posix_clock_gettime_times(mtimep, stimep);
}

#endif /* defined(OS_SYSTEM_TIME_USING_CLOCK_GETTIME) */

#endif /* !defined(__linux__) */

#define ERTS_HAVE_ERTS_SYS_HRTIME_IMPL__

ErtsSysHrTime
erts_sys_hrtime(void)
{
    return (ErtsSysHrTime) posix_clock_gettime(MONOTONIC_CLOCK_ID,
					       MONOTONIC_CLOCK_ID_STR);
}

#endif /* defined(OS_MONOTONIC_TIME_USING_CLOCK_GETTIME) */

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * MACH clock_get_time()                                                     *
\*                                                                           */

#if defined(OS_MONOTONIC_TIME_USING_MACH_CLOCK_GET_TIME) \
    || defined(OS_SYSTEM_TIME_USING_MACH_CLOCK_GET_TIME)

#ifdef ERTS_HAVE_MACH_CLOCK_GETRES

static Sint64
mach_clock_getres(clock_id_t clkid, char *clkid_str)
{
    mach_port_t task;
    host_name_port_t host;
    natural_t attr[1];
    kern_return_t kret;
    clock_serv_t clk_srv;
    mach_msg_type_number_t cnt;

    host = mach_host_self();
    kret = host_get_clock_service(host, clkid, &clk_srv);
    if (kret != KERN_SUCCESS) {
	erl_exit(ERTS_ABORT_EXIT,
		 "host_get_clock_service(_, %s, _) failed\n",
		 clkid_str);
    }

    cnt = sizeof(attr);
    kret = clock_get_attributes(clk_srv, CLOCK_GET_TIME_RES, (clock_attr_t) attr, &cnt);
    if (kret != KERN_SUCCESS) {
	erl_exit(ERTS_ABORT_EXIT,	
		 "clock_get_attributes(%s, _) failed\n",
		 clkid_str);
    }
    task = mach_task_self();
    mach_port_deallocate(task, host);
    mach_port_deallocate(task, clk_srv);

    return (Sint64) attr[0];
}

#endif /* ERTS_HAVE_MACH_CLOCK_GETRES */

static ERTS_INLINE Sint64
mach_clock_gettime(clock_id_t clkid, char *clkid_str)
{
    Sint64 time;
    mach_port_t task;
    host_name_port_t host;
    kern_return_t kret;
    clock_serv_t clk_srv;
    mach_timespec_t time_spec;

    host = mach_host_self();
    kret = host_get_clock_service(host, clkid, &clk_srv);
    if (kret != KERN_SUCCESS) {
	erl_exit(ERTS_ABORT_EXIT,
		 "host_get_clock_service(_, %s, _) failed\n",
		 clkid_str);
    }
    errno = 0;
    kret = clock_get_time(clk_srv, &time_spec);
    if (kret != KERN_SUCCESS) {
	erl_exit(ERTS_ABORT_EXIT,
		 "clock_get_time(%s, _) failed\n",
		 clkid_str);
    }
    task = mach_task_self();
    mach_port_deallocate(task, host);
    mach_port_deallocate(task, clk_srv);

    time = (Sint64) time_spec.tv_sec;
    time *= (Sint64) 1000*1000*1000;
    time += (Sint64) time_spec.tv_nsec;
    return time;
}

#endif /* defined(OS_MONOTONIC_TIME_USING_MACH_CLOCK_GET_TIME) \
	  || defined(OS_SYSTEM_TIME_USING_MACH_CLOCK_GET_TIME) */

#if defined(OS_SYSTEM_TIME_USING_MACH_CLOCK_GET_TIME)

#define ERTS_HAVE_ERTS_OS_TIMES_IMPL__

ErtsSystemTime
erts_os_system_time(void)
{
    ErtsSystemTime stime;
    stime = (ErtsSystemTime) mach_clock_gettime(WALL_CLOCK_ID,
						WALL_CLOCK_ID_STR);
#if defined(OS_MONOTONIC_TIME_USING_MACH_CLOCK_GET_TIME)
    return stime;
#else
    return adj_stime_time_unit(stime, (Uint32) 1000*1000*1000);
#endif
}

#endif /* defined(OS_SYSTEM_TIME_USING_MACH_CLOCK_GET_TIME) */

#if defined(OS_MONOTONIC_TIME_USING_MACH_CLOCK_GET_TIME)

ErtsMonotonicTime
erts_os_monotonic_time(void)
{
    return (ErtsMonotonicTime) mach_clock_gettime(MONOTONIC_CLOCK_ID,
						  MONOTONIC_CLOCK_ID_STR);
}

#define ERTS_HAVE_ERTS_SYS_HRTIME_IMPL__

ErtsSysHrTime
erts_sys_hrtime(void)
{
    return (ErtsMonotonicTime) mach_clock_gettime(MONOTONIC_CLOCK_ID,
						  MONOTONIC_CLOCK_ID_STR);
}

#if defined(OS_SYSTEM_TIME_USING_MACH_CLOCK_GET_TIME)

#define ERTS_HAVE_ERTS_OS_TIMES_IMPL__

void
erts_os_times(ErtsMonotonicTime *mtimep, ErtsSystemTime *stimep)
{
    ErtsMonotonicTime mtime;
    ErtsSystemTime stime;
    mach_port_t task;
    host_name_port_t host;
    kern_return_t mkret, skret;
    clock_serv_t mclk_srv, sclk_srv;
    mach_timespec_t mon_time_spec, sys_time_spec;

    host = mach_host_self();
    mkret = host_get_clock_service(host, MONOTONIC_CLOCK_ID, &mclk_srv);
    skret = host_get_clock_service(host, WALL_CLOCK_ID, &sclk_srv);
    if (mkret != KERN_SUCCESS) {
	erl_exit(ERTS_ABORT_EXIT,
		 "host_get_clock_service(_, %s, _) failed\n",
		 MONOTONIC_CLOCK_ID);
    }
    if (skret != KERN_SUCCESS) {
	erl_exit(ERTS_ABORT_EXIT,
		 "host_get_clock_service(_, %s, _) failed\n",
		 WALL_CLOCK_ID);
    }
    mkret = clock_get_time(mclk_srv, &mon_time_spec);
    skret = clock_get_time(sclk_srv, &sys_time_spec);
    if (mkret != KERN_SUCCESS) {
	erl_exit(ERTS_ABORT_EXIT,
		 "clock_get_time(%s, _) failed\n",
		 MONOTONIC_CLOCK_ID);
    }
    if (skret != KERN_SUCCESS) {
	erl_exit(ERTS_ABORT_EXIT,
		 "clock_get_time(%s, _) failed\n",
		 WALL_CLOCK_ID);
    }
    task = mach_task_self();
    mach_port_deallocate(task, host);
    mach_port_deallocate(task, mclk_srv);
    mach_port_deallocate(task, sclk_srv);

    mtime = (ErtsMonotonicTime) mon_time_spec.tv_sec;
    mtime *= (ErtsMonotonicTime) 1000*1000*1000;
    mtime += (ErtsMonotonicTime) mon_time_spec.tv_nsec;
    stime = (ErtsSystemTime) sys_time_spec.tv_sec;
    stime *= (ErtsSystemTime) 1000*1000*1000;
    stime += (ErtsSystemTime) sys_time_spec.tv_nsec;
    *mtimep = mtime;
    *stimep = stime;
}

#endif /* defined(OS_SYSTEM_TIME_USING_MACH_CLOCK_GET_TIME) */

#endif /* defined(OS_MONOTONIC_TIME_USING_MACH_CLOCK_GET_TIME) */

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * Solaris gethrtime() - OS monotonic time                                   *
\*                                                                           */

#if defined(OS_MONOTONIC_TIME_USING_GETHRTIME)

ErtsMonotonicTime erts_os_monotonic_time(void)
{
    return (ErtsMonotonicTime) gethrtime();
}

#define ERTS_HAVE_ERTS_SYS_HRTIME_IMPL__

ErtsSysHrTime
erts_sys_hrtime(void)
{
    return (ErtsSysHrTime) gethrtime();
}

#endif /* defined(OS_MONOTONIC_TIME_USING_GETHRTIME) */

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * gettimeofday() - OS system time                                           *
\*                                                                           */

#if defined(OS_SYSTEM_TIME_GETTIMEOFDAY)

ErtsSystemTime
erts_os_system_time(void)
{
    ErtsSystemTime stime;
    struct timeval tv;

    if (gettimeofday(&tv, NULL) != 0) {
	int err = errno;
	char *errstr = err ? strerror(err) : "unknown";
	erl_exit(ERTS_ABORT_EXIT,
		 "gettimeofday(_, NULL) failed: %s (%d)\n",
		 errstr, err);
    }

    stime = (ErtsSystemTime) tv.tv_sec;
    stime *= (ErtsSystemTime) 1000*1000;
    stime += (ErtsSystemTime) tv.tv_usec;

    return adj_stime_time_unit(stime, (Uint32) 1000*1000);
}

#endif /* defined(OS_SYSTEM_TIME_GETTIMEOFDAY) */

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * times() - OS monotonic time                                               *
\*                                                                           */

#if defined(OS_MONOTONIC_TIME_USING_TIMES)

ErtsMonotonicTime
erts_os_monotonic_time(void)
{
    Uint32 ticks = get_tick_count();
    ERTS_CHK_EXTEND_OS_MONOTONIC_TIME(&internal_state.wr.m.os_mtime_xtnd,
				      ticks);
    return ERTS_EXTEND_OS_MONOTONIC_TIME(&internal_state.wr.m.os_mtime_xtnd,
					 ticks) << internal_state.r.o.times_shift;
}

#endif

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * Fallbacks                                                                 *
\*                                                                           */

#ifndef ERTS_HAVE_ERTS_SYS_HRTIME_IMPL__

ErtsSysHrTime
erts_sys_hrtime(void)
{
    return (ErtsSysHrTime) erts_os_system_time();
}

#endif

#if !defined(ERTS_HAVE_ERTS_OS_TIMES_IMPL__) \
    && defined(ERTS_HAVE_OS_MONOTONIC_TIME_SUPPORT)

void
erts_os_times(ErtsMonotonicTime *mtimep, ErtsSystemTime *stimep)
{
    *mtimep = erts_os_monotonic_time();
    *stimep = erts_os_system_time();
}

#endif

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

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


