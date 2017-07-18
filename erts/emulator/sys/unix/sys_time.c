/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2005-2016. All Rights Reserved.
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

/* These need to be undef:ed to not break activation of
 * micro level process accounting on /proc/self 
 */
#ifdef _LARGEFILE_SOURCE
#  undef _LARGEFILE_SOURCE
#endif
#ifdef _FILE_OFFSET_BITS
#  undef _FILE_OFFSET_BITS
#endif

#include <stdlib.h>
#include "sys.h"
#include "global.h"
#include "erl_os_monotonic_time_extender.h"

#undef ERTS_HAVE_ERTS_OS_TIMES_IMPL__
#undef ERTS_HAVE_ERTS_SYS_HRTIME_IMPL__

#if defined(OS_MONOTONIC_TIME_USING_MACH_CLOCK_GET_TIME) \
    || defined(OS_SYSTEM_TIME_USING_MACH_CLOCK_GET_TIME) \
    || defined(SYS_HRTIME_USING_MACH_CLOCK_GET_TIME)
#  include <mach/clock.h>
#  include <mach/mach.h>
#  define ERTS_MACH_CLOCKS
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

static void init_perf_counter(void);

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

static ErtsMonotonicTime clock_gettime_monotonic(void);
static ErtsMonotonicTime clock_gettime_monotonic_verified(void);
#if defined(HAVE_CLOCK_GETTIME_MONOTONIC_RAW)
static ErtsMonotonicTime clock_gettime_monotonic_raw(void);
#endif
#if defined(OS_SYSTEM_TIME_USING_CLOCK_GETTIME)
static void clock_gettime_times(ErtsMonotonicTime *, ErtsSystemTime *);
static void clock_gettime_times_verified(ErtsMonotonicTime *, ErtsSystemTime *);
#if defined(HAVE_CLOCK_GETTIME_MONOTONIC_RAW)
static void clock_gettime_times_raw(ErtsMonotonicTime *, ErtsSystemTime *);
#endif
#endif

#endif /* defined(__linux__) && defined(OS_MONOTONIC_TIME_USING_CLOCK_GETTIME) */

#ifdef ERTS_MACH_CLOCKS
#  define ERTS_SYS_TIME_INTERNAL_STATE_READ_ONLY__
typedef struct {
    clock_id_t id;
    clock_serv_t srv;
    char *name;
} ErtsMachClock;

typedef struct {
    host_name_port_t host;
    struct {
	ErtsMachClock monotonic;
	ErtsMachClock wall;
    } clock;
} ErtsMachClocks;
static void mach_clocks_init(void);
static void mach_clocks_fini(void);
#  ifdef HAVE_CLOCK_GET_ATTRIBUTES
#    define ERTS_HAVE_MACH_CLOCK_GETRES
static Sint64
mach_clock_getres(ErtsMachClock *clk);
#  endif    
#endif /* ERTS_MACH_CLOCKS */

#ifdef ERTS_SYS_TIME_INTERNAL_STATE_READ_ONLY__
struct sys_time_internal_state_read_only__ {
#if defined(OS_MONOTONIC_TIME_USING_TIMES)
    int times_shift;
#endif
#ifdef ERTS_MACH_CLOCKS
    ErtsMachClocks mach;
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
    erts_mtx_t mtx;
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
#if defined(ERTS_HAVE_OS_MONOTONIC_TIME_SUPPORT)
    int major, minor, build, vsn;
#endif
#if defined(ERTS_MACH_CLOCKS)
    mach_clocks_init();
#endif
#if !defined(ERTS_HAVE_OS_MONOTONIC_TIME_SUPPORT)

    init_resp->have_os_monotonic_time = 0;

#else /* defined(ERTS_HAVE_OS_MONOTONIC_TIME_SUPPORT) */

#ifdef ERTS_HAVE_CORRECTED_OS_MONOTONIC_TIME
    init_resp->have_corrected_os_monotonic_time = 1;
#else
    init_resp->have_corrected_os_monotonic_time = 0;
#endif

    init_resp->os_monotonic_time_info.resolution = (Uint64) 1000*1000*1000;
#if defined(ERTS_HAVE_MACH_CLOCK_GETRES) && defined(MONOTONIC_CLOCK_ID)
    init_resp->os_monotonic_time_info.resolution
	= mach_clock_getres(&internal_state.r.o.mach.clock.monotonic);
#elif defined(HAVE_CLOCK_GETRES) && defined(MONOTONIC_CLOCK_ID)
    {
	struct timespec ts;
	if (clock_getres(MONOTONIC_CLOCK_ID, &ts) == 0) {
	    if (ts.tv_sec == 0 && ts.tv_nsec != 0)
		init_resp->os_monotonic_time_info.resolution /= ts.tv_nsec;
	    else if (ts.tv_sec >= 1)
		init_resp->os_monotonic_time_info.resolution = 1;
	}
    }
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
	    clock_gettime_monotonic;
#if defined(OS_SYSTEM_TIME_USING_CLOCK_GETTIME)
	erts_sys_time_data__.r.o.os_times =
	    clock_gettime_times;
#endif
    }
    else {
	/*
	 * Linux versions prior to 2.6.33 have a
	 * known bug that sometimes cause the NTP
	 * adjusted monotonic clock to take small
	 * steps backwards. Use raw monotonic clock
	 * if it is present; otherwise, fall back
	 * on locked verification of values.
	 */
	init_resp->have_corrected_os_monotonic_time = 0;
#if defined(HAVE_CLOCK_GETTIME_MONOTONIC_RAW)
	/* We know that CLOCK_MONOTONIC_RAW is defined,
	   but we don't know if we got a kernel that
	   supports it. Support for CLOCK_MONOTONIC_RAW
	   appeared in kernel 2.6.28... */
	if (vsn >= ERTS_MK_VSN_INT(2, 6, 28)) {
	    erts_sys_time_data__.r.o.os_monotonic_time =
		clock_gettime_monotonic_raw;
#if defined(OS_SYSTEM_TIME_USING_CLOCK_GETTIME)
	    erts_sys_time_data__.r.o.os_times =
		clock_gettime_times_raw;
#endif
	    init_resp->os_monotonic_time_info.clock_id =
		"CLOCK_MONOTONIC_RAW";
	}
	else
#endif /* defined(HAVE_CLOCK_GETTIME_MONOTONIC_RAW) */
	{
	    erts_sys_time_data__.r.o.os_monotonic_time =
		clock_gettime_monotonic_verified;
#if defined(OS_SYSTEM_TIME_USING_CLOCK_GETTIME)
	    erts_sys_time_data__.r.o.os_times =
		clock_gettime_times_verified;
#endif
            erts_mtx_init(&internal_state.w.f.mtx, "os_monotonic_time", NIL,
                ERTS_LOCK_FLAGS_PROPERTY_STATIC | ERTS_LOCK_FLAGS_CATEGORY_IO);
	    internal_state.w.f.last_delivered
		= clock_gettime_monotonic();
	    init_resp->os_monotonic_time_info.locked_use = 1;
	}
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
	erts_exit(ERTS_ABORT_EXIT, "Can't get clock ticks/sec\n");
    
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
#if defined(ERTS_HAVE_MACH_CLOCK_GETRES) && defined(WALL_CLOCK_ID)
    init_resp->os_system_time_info.resolution
	= mach_clock_getres(&internal_state.r.o.mach.clock.wall);
#elif defined(HAVE_CLOCK_GETRES) && defined(WALL_CLOCK_ID)
    {
	struct timespec ts;
	if (clock_getres(WALL_CLOCK_ID, &ts) == 0) {
	    if (ts.tv_sec == 0 && ts.tv_nsec != 0)
		init_resp->os_system_time_info.resolution /= ts.tv_nsec;
	    else if (ts.tv_sec >= 1)
		init_resp->os_system_time_info.resolution = 1;
	}
    }
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

    init_perf_counter();

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

#define ERTS_TimeSpec2Sint64(TS)				\
    ((((Sint64) (TS)->tv_sec) * ((Sint64) 1000*1000*1000))	\
     + ((Sint64) (TS)->tv_nsec))

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * POSIX clock_gettime()                                                     *
\*                                                                           */

#if defined(OS_MONOTONIC_TIME_USING_CLOCK_GETTIME) \
    || defined(OS_SYSTEM_TIME_USING_CLOCK_GETTIME)

static ERTS_INLINE Sint64
posix_clock_gettime(clockid_t id, char *name)
{
    struct timespec ts;

    if (clock_gettime(id, &ts) != 0) {
	int err = errno;
	char *errstr = err ? strerror(err) : "unknown";
	erts_exit(ERTS_ABORT_EXIT,
		 "clock_gettime(%s, _) failed: %s (%d)\n",
		 name, errstr, err);
    }
    return ERTS_TimeSpec2Sint64(&ts);
}

#endif /* defined(OS_MONOTONIC_TIME_USING_CLOCK_GETTIME) \
	  || defined(OS_SYSTEM_TIME_USING_CLOCK_GETTIME) */

#if defined(OS_SYSTEM_TIME_USING_CLOCK_GETTIME)

ErtsSystemTime
erts_os_system_time(void)
{
    Sint64 stime = posix_clock_gettime(WALL_CLOCK_ID,
				       WALL_CLOCK_ID_STR);
    return adj_stime_time_unit((ErtsSystemTime) stime,
			       (Uint32) 1000*1000*1000);
}

#endif /* defined(OS_SYSTEM_TIME_USING_CLOCK_GETTIME) */

#if defined(OS_MONOTONIC_TIME_USING_CLOCK_GETTIME)

#if defined(OS_SYSTEM_TIME_USING_CLOCK_GETTIME)

#define ERTS_HAVE_ERTS_OS_TIMES_IMPL__

static ERTS_INLINE void
posix_clock_gettime_times(clockid_t mid, char *mname,
			  ErtsMonotonicTime *mtimep,
			  clockid_t sid, char *sname,
			  ErtsSystemTime *stimep)
{
    struct timespec mts, sts;
    int mres, sres, merr, serr;

    mres = clock_gettime(mid, &mts);
    merr = errno;
    sres = clock_gettime(sid, &sts);
    serr = errno;
    
    if (mres != 0) {
	char *errstr = merr ? strerror(merr) : "unknown";
	erts_exit(ERTS_ABORT_EXIT,
		 "clock_gettime(%s, _) failed: %s (%d)\n",
		 mname, errstr, merr);
    }
    if (sres != 0) {
	char *errstr = serr ? strerror(serr) : "unknown";
	erts_exit(ERTS_ABORT_EXIT,
		 "clock_gettime(%s, _) failed: %s (%d)\n",
		 sname, errstr, serr);
    }

    *mtimep = (ErtsMonotonicTime) ERTS_TimeSpec2Sint64(&mts);
    *stimep = (ErtsSystemTime) ERTS_TimeSpec2Sint64(&sts);
}

#endif /* defined(OS_SYSTEM_TIME_USING_CLOCK_GETTIME) */

#if defined(__linux__)

static ErtsMonotonicTime clock_gettime_monotonic_verified(void)
{
    ErtsMonotonicTime mtime;

    mtime = (ErtsMonotonicTime) posix_clock_gettime(MONOTONIC_CLOCK_ID,
						    MONOTONIC_CLOCK_ID_STR);

    erts_mtx_lock(&internal_state.w.f.mtx);
    if (mtime < internal_state.w.f.last_delivered)
	mtime = internal_state.w.f.last_delivered;
    else
	internal_state.w.f.last_delivered = mtime;
    erts_mtx_unlock(&internal_state.w.f.mtx);

    return mtime;
}

#if defined(OS_SYSTEM_TIME_USING_CLOCK_GETTIME)

static void clock_gettime_times_verified(ErtsMonotonicTime *mtimep,
					 ErtsSystemTime *stimep)
{
    posix_clock_gettime_times(MONOTONIC_CLOCK_ID,
			      MONOTONIC_CLOCK_ID_STR,
			      mtimep,
			      WALL_CLOCK_ID,
			      WALL_CLOCK_ID_STR,
			      stimep);

    erts_mtx_lock(&internal_state.w.f.mtx);
    if (*mtimep < internal_state.w.f.last_delivered)
	*mtimep = internal_state.w.f.last_delivered;
    else
	internal_state.w.f.last_delivered = *mtimep;
    erts_mtx_unlock(&internal_state.w.f.mtx);
}

#endif /* defined(OS_SYSTEM_TIME_USING_CLOCK_GETTIME) */

static ErtsMonotonicTime clock_gettime_monotonic(void)
{
    return (ErtsMonotonicTime) posix_clock_gettime(MONOTONIC_CLOCK_ID,
						   MONOTONIC_CLOCK_ID_STR);
}

#if defined(HAVE_CLOCK_GETTIME_MONOTONIC_RAW)

static ErtsMonotonicTime clock_gettime_monotonic_raw(void)
{
    return (ErtsMonotonicTime) posix_clock_gettime(CLOCK_MONOTONIC_RAW,
						   "CLOCK_MONOTONIC_RAW");
}

#endif /* defined(HAVE_CLOCK_GETTIME_MONOTONIC_RAW) */

#if defined(OS_SYSTEM_TIME_USING_CLOCK_GETTIME)

static void clock_gettime_times(ErtsMonotonicTime *mtimep,
				ErtsSystemTime *stimep)
{
    posix_clock_gettime_times(MONOTONIC_CLOCK_ID,
			      MONOTONIC_CLOCK_ID_STR,
			      mtimep,
			      WALL_CLOCK_ID,
			      WALL_CLOCK_ID_STR,
			      stimep);
}

#if defined(HAVE_CLOCK_GETTIME_MONOTONIC_RAW)

static void clock_gettime_times_raw(ErtsMonotonicTime *mtimep,
				    ErtsSystemTime *stimep)
{
    posix_clock_gettime_times(CLOCK_MONOTONIC_RAW,
			      "CLOCK_MONOTONIC_RAW",
			      mtimep,
			      WALL_CLOCK_ID,
			      WALL_CLOCK_ID_STR,
			      stimep);
}

#endif /* defined(HAVE_CLOCK_GETTIME_MONOTONIC_RAW) */

#endif /* defined(OS_SYSTEM_TIME_USING_CLOCK_GETTIME) */

#else /* !defined(__linux__) */

ErtsMonotonicTime erts_os_monotonic_time(void)
{
    return posix_clock_gettime(MONOTONIC_CLOCK_ID,
			       MONOTONIC_CLOCK_ID_STR);
}

#if defined(OS_SYSTEM_TIME_USING_CLOCK_GETTIME)

void erts_os_times(ErtsMonotonicTime *mtimep, ErtsSystemTime *stimep)
{
   posix_clock_gettime_times(MONOTONIC_CLOCK_ID,
			     MONOTONIC_CLOCK_ID_STR,
			     mtimep,
			     WALL_CLOCK_ID,
			     WALL_CLOCK_ID_STR,
			     stimep);
}

#endif /* defined(OS_SYSTEM_TIME_USING_CLOCK_GETTIME) */

#endif /* !defined(__linux__) */

#endif /* defined(OS_MONOTONIC_TIME_USING_CLOCK_GETTIME) */

#if defined(SYS_HRTIME_USING_CLOCK_GETTIME)
#  define ERTS_HAVE_ERTS_SYS_HRTIME_IMPL__

ErtsSysHrTime
erts_sys_hrtime(void)
{
    return (ErtsSysHrTime) posix_clock_gettime(HRTIME_CLOCK_ID,
					       HRTIME_CLOCK_ID_STR);
}

#endif /* defined(SYS_HRTIME_USING_CLOCK_GETTIME) */

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * MACH clock_get_time()                                                     *
\*                                                                           */

#if defined(ERTS_MACH_CLOCKS)

static void
mach_clocks_fini(void)
{
    mach_port_t task = mach_task_self();
    mach_port_deallocate(task, internal_state.r.o.mach.host);
#if defined(OS_MONOTONIC_TIME_USING_MACH_CLOCK_GET_TIME)
    mach_port_deallocate(task, internal_state.r.o.mach.clock.monotonic.srv);
#endif
#if defined(OS_SYSTEM_TIME_USING_MACH_CLOCK_GET_TIME)
    mach_port_deallocate(task, internal_state.r.o.mach.clock.wall.srv);
#endif
}

static void
mach_clocks_init(void)
{
    kern_return_t kret;
    host_name_port_t host;
    clock_id_t id;
    clock_serv_t *clck_srv_p;
    char *name;

    host = internal_state.r.o.mach.host = mach_host_self();

#if defined(OS_MONOTONIC_TIME_USING_MACH_CLOCK_GET_TIME) \
    ||  defined(SYS_HRTIME_USING_MACH_CLOCK_GET_TIME)
    id = internal_state.r.o.mach.clock.monotonic.id = MONOTONIC_CLOCK_ID;
    name = internal_state.r.o.mach.clock.monotonic.name = MONOTONIC_CLOCK_ID_STR;
    clck_srv_p = &internal_state.r.o.mach.clock.monotonic.srv;
    kret = host_get_clock_service(host, id, clck_srv_p);
    if (kret != KERN_SUCCESS) {
	erts_exit(ERTS_ABORT_EXIT,
		 "host_get_clock_service(_, %s, _) failed\n",
		 name);
    }
#endif

#if defined(OS_SYSTEM_TIME_USING_MACH_CLOCK_GET_TIME)
    id = internal_state.r.o.mach.clock.wall.id = WALL_CLOCK_ID;
    name = internal_state.r.o.mach.clock.wall.name = WALL_CLOCK_ID_STR;
    clck_srv_p = &internal_state.r.o.mach.clock.wall.srv;
    kret = host_get_clock_service(host, id, clck_srv_p);
    if (kret != KERN_SUCCESS) {
	erts_exit(ERTS_ABORT_EXIT,
		 "host_get_clock_service(_, %s, _) failed\n",
		 name);
    }
#endif

    if (atexit(mach_clocks_fini) != 0) {
	int err = errno;
	char *errstr = err ? strerror(err) : "unknown";
	erts_exit(ERTS_ABORT_EXIT,
		 "Failed to register mach_clocks_fini() "
		 "for call at exit: %s (%d)\n",
		 errstr, err);
    }
}

#ifdef ERTS_HAVE_MACH_CLOCK_GETRES

static Sint64
mach_clock_getres(ErtsMachClock *clk)
{
    kern_return_t kret;
    natural_t attr[1];
    mach_msg_type_number_t cnt;

    cnt = sizeof(attr);
    kret = clock_get_attributes(clk->srv,
				CLOCK_GET_TIME_RES,
				(clock_attr_t) attr,
				&cnt);
    if (kret != KERN_SUCCESS || cnt != 1) {
	erts_exit(ERTS_ABORT_EXIT,
		 "clock_get_attributes(%s, _) failed\n",
		 clk->name);
    }

    return (Sint64) attr[0];
}

#endif /* ERTS_HAVE_MACH_CLOCK_GETRES */

static ERTS_INLINE Sint64
mach_clock_get_time(ErtsMachClock *clk)
{
    kern_return_t kret;
    mach_timespec_t time_spec;

    kret = clock_get_time(clk->srv, &time_spec);
    if (kret != KERN_SUCCESS)
	erts_exit(ERTS_ABORT_EXIT, "clock_get_time(%s, _) failed\n", clk->name);

    return ERTS_TimeSpec2Sint64(&time_spec);
}

#endif /* defined(ERTS_MACH_CLOCKS) */

#if defined(OS_SYSTEM_TIME_USING_MACH_CLOCK_GET_TIME)

#define ERTS_HAVE_ERTS_OS_TIMES_IMPL__

ErtsSystemTime
erts_os_system_time(void)
{
    Sint64 stime = mach_clock_get_time(&internal_state.r.o.mach.clock.wall);
    return adj_stime_time_unit((ErtsSystemTime) stime,
			       (Uint32) 1000*1000*1000);
}

#endif /* defined(OS_SYSTEM_TIME_USING_MACH_CLOCK_GET_TIME) */

#if defined(OS_MONOTONIC_TIME_USING_MACH_CLOCK_GET_TIME)

ErtsMonotonicTime
erts_os_monotonic_time(void)
{
    return (ErtsMonotonicTime)
	mach_clock_get_time(&internal_state.r.o.mach.clock.monotonic);
}

#if defined(OS_SYSTEM_TIME_USING_MACH_CLOCK_GET_TIME)

#define ERTS_HAVE_ERTS_OS_TIMES_IMPL__

void
erts_os_times(ErtsMonotonicTime *mtimep, ErtsSystemTime *stimep)
{
    kern_return_t mkret, skret;
    mach_timespec_t mon_time_spec, sys_time_spec;

    mkret = clock_get_time(internal_state.r.o.mach.clock.monotonic.srv,
			   &mon_time_spec);
    skret = clock_get_time(internal_state.r.o.mach.clock.wall.srv,
			   &sys_time_spec);

    if (mkret != KERN_SUCCESS)
	erts_exit(ERTS_ABORT_EXIT,
		 "clock_get_time(%s, _) failed\n",
		 internal_state.r.o.mach.clock.monotonic.name);
    if (skret != KERN_SUCCESS)
	erts_exit(ERTS_ABORT_EXIT,
		 "clock_get_time(%s, _) failed\n",
		 internal_state.r.o.mach.clock.wall.name);

    *mtimep = (ErtsMonotonicTime) ERTS_TimeSpec2Sint64(&mon_time_spec);
    *stimep = (ErtsSystemTime) ERTS_TimeSpec2Sint64(&sys_time_spec);
}

#endif /* defined(OS_SYSTEM_TIME_USING_MACH_CLOCK_GET_TIME) */

#endif /* defined(OS_MONOTONIC_TIME_USING_MACH_CLOCK_GET_TIME) */

#if defined(SYS_HRTIME_USING_MACH_CLOCK_GET_TIME)

#define ERTS_HAVE_ERTS_SYS_HRTIME_IMPL__

ErtsSysHrTime
erts_sys_hrtime(void)
{
    return (ErtsSysHrTime)
	mach_clock_get_time(&internal_state.r.o.mach.clock.monotonic);
}

#endif /* defined(SYS_HRTIME_USING_MACH_CLOCK_GET_TIME) */

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * Solaris gethrtime() - OS monotonic time                                   *
\*                                                                           */

#if defined(OS_MONOTONIC_TIME_USING_GETHRTIME)

ErtsMonotonicTime erts_os_monotonic_time(void)
{
    return (ErtsMonotonicTime) gethrtime();
}

#endif /* defined(OS_MONOTONIC_TIME_USING_GETHRTIME) */

#if defined(SYS_HRTIME_USING_GETHRTIME)

#define ERTS_HAVE_ERTS_SYS_HRTIME_IMPL__

ErtsSysHrTime
erts_sys_hrtime(void)
{
    return (ErtsSysHrTime) gethrtime();
}

#endif /* defined(SYS_HRTIME_USING_GETHRTIME) */

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
	erts_exit(ERTS_ABORT_EXIT,
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
    return (ErtsSysHrTime) ERTS_MONOTONIC_TO_NSEC(erts_os_system_time());
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

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * Performance counter functions                                             *
\*                                                                           */


/* What resolution to spin to in micro seconds */
#define RESOLUTION 100
/* How many iterations to spin */
#define ITERATIONS 1
/* How many significant figures to round to */
#define SIGFIGS 3

static ErtsSysPerfCounter calculate_perf_counter_unit(void) {
    int i;
    ErtsSysPerfCounter pre, post;
    double value = 0;
    double round_factor;
#if defined(HAVE_GETHRTIME) && defined(GETHRTIME_WITH_CLOCK_GETTIME)
    struct timespec basetime,comparetime;
#define __GETTIME(arg) clock_gettime(CLOCK_MONOTONIC,arg)
#define __GETUSEC(arg) (arg.tv_nsec / 1000)
#else
    SysTimeval basetime,comparetime;
#define __GETTIME(arg) sys_gettimeofday(arg)
#define __GETUSEC(arg) arg.tv_usec
#endif

    for (i = 0; i < ITERATIONS; i++) {
        /* Make sure usec just flipped over at current resolution */
        __GETTIME(&basetime);
        do {
            __GETTIME(&comparetime);
        } while ((__GETUSEC(basetime) / RESOLUTION) == (__GETUSEC(comparetime) / RESOLUTION));

        pre = erts_sys_perf_counter();

        __GETTIME(&basetime);
        do {
            __GETTIME(&comparetime);
        } while ((__GETUSEC(basetime) / RESOLUTION) == (__GETUSEC(comparetime) / RESOLUTION));

        post = erts_sys_perf_counter();

        value += post - pre;
    }
    /* After this value is ticks per us */
    value /= (RESOLUTION*ITERATIONS);

    /* We round to 3 significant figures */
    round_factor = pow(10.0, SIGFIGS - ceil(log10(value)));
    value = ((ErtsSysPerfCounter)(value * round_factor + 0.5)) / round_factor;

    /* convert to ticks per second */
    return 1000000 * value;
}

static int have_rdtscp(void)
{
#if defined(ETHR_X86_RUNTIME_CONF__)
    /* On early x86 cpu's the tsc varies with the current speed of the cpu,
       which means that the time per tick vary depending on the current
       load of the cpu. We do not want this as it would give very scewed
       numbers when the cpu is mostly idle.
       The linux kernel seems to think that checking for constant and
       reliable is enough to trust the counter so we do the same.

       If this test is not good enough, I don't know what we'll do.
       Maybe fallback on erts_sys_hrtime always, but that would be a shame as
       rdtsc is about 3 times faster than hrtime... */
    return ETHR_X86_RUNTIME_CONF_HAVE_CONSTANT_TSC__ &&
        ETHR_X86_RUNTIME_CONF_HAVE_TSC_RELIABLE__;
#else
    return 0;
#endif
}

static ErtsSysPerfCounter rdtsc(void)
{
 /*  It may have been a good idea to put the cpuid instruction before
     the rdtsc, but I decided against it because it is not really
     needed for msacc, and it slows it down by quite a bit (5-7 times slower).
     As a result though, this timestamp becomes much less
     accurate as it might be re-ordered to be executed way before or after this
     function is called.
 */
    ErtsSysPerfCounter ts;
#if defined(__x86_64__)
    __asm__ __volatile__ ("rdtsc\n\t"
                          "shl $32, %%rdx\n\t"
                          "or %%rdx, %0" : "=a" (ts) : : "rdx");
#elif defined(__i386__)
    __asm__ __volatile__ ("rdtsc\n\t"
                           : "=A" (ts) );
#endif
    return ts;
}

static void init_perf_counter(void)
{
    if (have_rdtscp()) {
        erts_sys_time_data__.r.o.perf_counter = rdtsc;
        erts_sys_time_data__.r.o.perf_counter_unit = calculate_perf_counter_unit();
    } else {
        erts_sys_time_data__.r.o.perf_counter = erts_sys_hrtime;
        erts_sys_time_data__.r.o.perf_counter_unit = ERTS_HRTIME_UNIT;
    }
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#ifdef HAVE_GETHRVTIME_PROCFS_IOCTL

/* The code below only has effect on solaris < 10,
   needed in order for gehhrvtime to work properly */
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


