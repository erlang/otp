/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1997-2018. All Rights Reserved.
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
 * Purpose: System-dependent time functions.
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif
#include "sys.h"
#include "assert.h"
#include "erl_os_monotonic_time_extender.h"
#include "erl_time.h"

/* Need to look more closely at qpc before use... */
#define ERTS_DISABLE_USE_OF_QPC_FOR_MONOTONIC_TIME 1

#define LL_LITERAL(X) ERTS_I64_LITERAL(X)

/******************* Routines for time measurement *********************/

#define EPOCH_JULIAN_DIFF LL_LITERAL(11644473600)
#define TICKS_PER_SECOND LL_LITERAL(10000000)
#define SECONDS_PER_DAY LL_LITERAL(86400)

#define ULI_TO_FILETIME(ft,ull)			\
 do {						\
     (ft).dwLowDateTime = (ull).LowPart;	\
     (ft).dwHighDateTime = (ull).HighPart;	\
 } while (0)

#define FILETIME_TO_ULI(ull,ft)			\
 do {						\
     (ull).LowPart = (ft).dwLowDateTime;	\
     (ull).HighPart = (ft).dwHighDateTime;	\
 } while (0)


#define EPOCH_TO_FILETIME(ft, epoch) \
    do { \
	ULARGE_INTEGER ull; \
	ull.QuadPart = (((epoch) + EPOCH_JULIAN_DIFF) * TICKS_PER_SECOND); \
	ULI_TO_FILETIME(ft,ull); \
    } while(0)

#define FILETIME_TO_EPOCH(epoch, ft) \
    do { \
	ULARGE_INTEGER ull; \
	FILETIME_TO_ULI(ull,ft); \
	(epoch) = ((ull.QuadPart / TICKS_PER_SECOND) - EPOCH_JULIAN_DIFF); \
    } while(0)
 
/* Getting timezone information is a heavy operation, so we want to do this 
   only once */

static TIME_ZONE_INFORMATION static_tzi;
static int have_static_tzi = 0;

static int days_in_month[2][13] = {
    {0,31,28,31,30,31,30,31,31,30,31,30,31},
    {0,31,29,31,30,31,30,31,31,30,31,30,31}};

#define ERTS_GET_TICK_COUNT_TIME_UNIT_SHIFT 10

/*
 * erts_os_monotonic_time()
 */

struct sys_time_internal_state_read_only__ {
    ULONGLONG (WINAPI *pGetTickCount64)(void);
    BOOL (WINAPI *pQueryPerformanceCounter)(LARGE_INTEGER *);
    Sint32 pcf;
    int using_get_tick_count_time_unit;
};

struct sys_time_internal_state_read_mostly__ {
    ErtsOsMonotonicTimeExtendState os_mtime_xtnd;
};

struct sys_time_internal_state_write_freq__ {
    erts_mtx_t mtime_mtx;
    ULONGLONG wrap;
    ULONGLONG last_tick_count;
};

__declspec(align(ASSUMED_CACHE_LINE_SIZE)) struct {
    union {
	struct sys_time_internal_state_read_only__ o;
	char align__[(((sizeof(struct sys_time_internal_state_read_only__) - 1)
		       / ASSUMED_CACHE_LINE_SIZE) + 1)
		     * ASSUMED_CACHE_LINE_SIZE];
    } r;
    union {
	struct sys_time_internal_state_read_mostly__ m;
	char align__[(((sizeof(struct sys_time_internal_state_read_mostly__) - 1)
		       / ASSUMED_CACHE_LINE_SIZE) + 1)
		     * ASSUMED_CACHE_LINE_SIZE];
    } wr;
    union {
	struct sys_time_internal_state_write_freq__ f;
	char align__[(((sizeof(struct sys_time_internal_state_write_freq__) - 1)
		       / ASSUMED_CACHE_LINE_SIZE) + 1)
		     * ASSUMED_CACHE_LINE_SIZE];
    } w;
} internal_state;

__declspec(align(ASSUMED_CACHE_LINE_SIZE)) ErtsSysTimeData__ erts_sys_time_data__;


static ERTS_INLINE ErtsSystemTime
SystemTime2MilliSec(SYSTEMTIME *stp)
{
    ErtsSystemTime stime;
    FILETIME ft;
    ULARGE_INTEGER ull;

    SystemTimeToFileTime(stp, &ft);
    FILETIME_TO_ULI(ull,ft);
    /* now in 100 ns units */
    stime = (ErtsSystemTime) ull.QuadPart;
    stime -= (((ErtsSystemTime) EPOCH_JULIAN_DIFF)
	      * ((ErtsSystemTime) (10*1000*1000)));
    stime /= (ErtsSystemTime) (10*1000); /* ms */
    return stime;
}

static ErtsMonotonicTime
os_monotonic_time_qpc(void)
{
    LARGE_INTEGER pc;

    if (!(*internal_state.r.o.pQueryPerformanceCounter)(&pc))
	erts_exit(ERTS_ABORT_EXIT, "QueryPerformanceCounter() failed\n");

    return (ErtsMonotonicTime) pc.QuadPart;
}

static void
os_times_qpc(ErtsMonotonicTime *mtimep, ErtsSystemTime *stimep)
{
    LARGE_INTEGER pc;
    SYSTEMTIME st;
    ErtsSystemTime stime;
    BOOL qpcr;

    qpcr = (*internal_state.r.o.pQueryPerformanceCounter)(&pc);
    GetSystemTime(&st);

    if (!qpcr)
	erts_exit(ERTS_ABORT_EXIT, "QueryPerformanceCounter() failed\n");

    *mtimep = (ErtsMonotonicTime) pc.QuadPart;

    stime = SystemTime2MilliSec(&st);

    *stimep = ((ErtsSystemTime)
	       erts_time_unit_conversion((Uint64) stime,
					 (Uint32) 1000,
					 internal_state.r.o.pcf));
}

static Uint32
get_tick_count(void)
{
    return (Uint32) GetTickCount();
}

static ErtsMonotonicTime
os_monotonic_time_gtc32(void) 
{
    ErtsMonotonicTime mtime;
    Uint32 ticks = (Uint32) GetTickCount();
    mtime = ERTS_EXTEND_OS_MONOTONIC_TIME(&internal_state.wr.m.os_mtime_xtnd,
					  ticks);
    mtime <<= ERTS_GET_TICK_COUNT_TIME_UNIT_SHIFT;
    return mtime;
}

static void
os_times_gtc32(ErtsMonotonicTime *mtimep, ErtsSystemTime *stimep)
{
    SYSTEMTIME st;
    ErtsSystemTime stime, mtime;
    Uint32 ticks;

    ticks = (Uint32) GetTickCount();
    GetSystemTime(&st);

    mtime = ERTS_EXTEND_OS_MONOTONIC_TIME(&internal_state.wr.m.os_mtime_xtnd,
					  ticks);
    mtime <<= ERTS_GET_TICK_COUNT_TIME_UNIT_SHIFT;
    *mtimep = mtime;

    stime = SystemTime2MilliSec(&st);
    stime <<= ERTS_GET_TICK_COUNT_TIME_UNIT_SHIFT;
    *stimep = stime;

}

static ErtsMonotonicTime
os_monotonic_time_gtc64(void) 
{
    ULONGLONG ticks = (*internal_state.r.o.pGetTickCount64)();
    ErtsMonotonicTime mtime = (ErtsMonotonicTime) ticks;
    return mtime << ERTS_GET_TICK_COUNT_TIME_UNIT_SHIFT;
}

static void
os_times_gtc64(ErtsMonotonicTime *mtimep, ErtsSystemTime *stimep)
{
    SYSTEMTIME st;
    ErtsSystemTime stime, mtime;
    ULONGLONG ticks;

    ticks = (*internal_state.r.o.pGetTickCount64)();
    GetSystemTime(&st);

    mtime = (ErtsMonotonicTime) ticks;
    mtime <<= ERTS_GET_TICK_COUNT_TIME_UNIT_SHIFT;
    *mtimep = mtime;

    stime = SystemTime2MilliSec(&st);
    stime <<= ERTS_GET_TICK_COUNT_TIME_UNIT_SHIFT;
    *stimep = stime;
}

static ErtsSysHrTime
sys_hrtime_qpc(void)
{
    LARGE_INTEGER pc;

    if (!(*internal_state.r.o.pQueryPerformanceCounter)(&pc))
	erts_exit(ERTS_ABORT_EXIT, "QueryPerformanceCounter() failed\n");

    ASSERT(pc.QuadPart > 0);

    return (ErtsSysHrTime) erts_time_unit_conversion((Uint64) pc.QuadPart,
						     internal_state.r.o.pcf,
						     (Uint32) 1000*1000*1000);
}

static ErtsSysHrTime
sys_hrtime_gtc32(void)
{
    ErtsSysHrTime time;
    Uint32 ticks = (Uint32) GetTickCount();
    time = (ErtsSysHrTime) ERTS_EXTEND_OS_MONOTONIC_TIME(&internal_state.wr.m.os_mtime_xtnd,
							 ticks);
    time *= (ErtsSysHrTime) (1000 * 1000);
    return time;
}

static ErtsSysHrTime
sys_hrtime_gtc64(void)
{
    ErtsSysHrTime time = (*internal_state.r.o.pGetTickCount64)();
    time *= (ErtsSysHrTime) (1000*1000);
    return time;
}

/*
 * Init
 */

void
sys_init_time(ErtsSysInitTimeResult *init_resp)
{
    ErtsMonotonicTime (*os_mtime_func)(void);
    void (*os_times_func)(ErtsMonotonicTime *, ErtsSystemTime *);
    ErtsSysHrTime (*sys_hrtime_func)(void) = NULL;
    ErtsMonotonicTime time_unit;
    char kernel_dll_name[] = "kernel32";
    HMODULE module;

    init_resp->os_monotonic_time_info.clock_id = NULL;

    module = GetModuleHandle(kernel_dll_name);
    if (!module) {
    get_tick_count:
        erts_mtx_init(&internal_state.w.f.mtime_mtx, "os_monotonic_time", NIL,
            ERTS_LOCK_FLAGS_PROPERTY_STATIC | ERTS_LOCK_FLAGS_CATEGORY_GENERIC);
	internal_state.w.f.wrap = 0;
	internal_state.w.f.last_tick_count = 0;

	init_resp->os_monotonic_time_info.func = "GetTickCount";
	init_resp->os_monotonic_time_info.locked_use = 0;
	/* 10-16 ms resolution according to MicroSoft documentation */
	init_resp->os_monotonic_time_info.resolution = 100; /* 10 ms */
	time_unit = (ErtsMonotonicTime) 1000;
	time_unit <<= ERTS_GET_TICK_COUNT_TIME_UNIT_SHIFT;
	internal_state.r.o.using_get_tick_count_time_unit = 1;
	os_mtime_func = os_monotonic_time_gtc32;
	os_times_func = os_times_gtc32;
	init_resp->os_monotonic_time_info.extended = 1;
	erts_init_os_monotonic_time_extender(&internal_state.wr.m.os_mtime_xtnd,
					     get_tick_count,
					     60*60*24*7); /* Check once a week */
	if (!sys_hrtime_func)
	    sys_hrtime_func = sys_hrtime_gtc32;
    }
    else {
	int major, minor, build;

	os_version(&major, &minor, &build);

	if (major < 6) {

	get_tick_count64:

	    internal_state.r.o.pGetTickCount64
		= ((ULONGLONG (WINAPI *)(void))
		   GetProcAddress(module, "GetTickCount64"));
	    if (!internal_state.r.o.pGetTickCount64)
		goto get_tick_count;

	    init_resp->os_monotonic_time_info.func = "GetTickCount64";
	    init_resp->os_monotonic_time_info.locked_use = 0;
	    /* 10-16 ms resolution according to MicroSoft documentation */
	    init_resp->os_monotonic_time_info.resolution = 100; /* 10 ms */
	    time_unit = (ErtsMonotonicTime) 1000;
	    time_unit <<= ERTS_GET_TICK_COUNT_TIME_UNIT_SHIFT;
	    internal_state.r.o.using_get_tick_count_time_unit = 1;
	    os_mtime_func = os_monotonic_time_gtc64;
	    os_times_func = os_times_gtc64;
	    if (!sys_hrtime_func)
		sys_hrtime_func = sys_hrtime_gtc64;
	}
	else { /* Vista or newer... */

	    LARGE_INTEGER pf;
	    BOOL (WINAPI *QPF)(LARGE_INTEGER *);

	    QPF = ((BOOL (WINAPI *)(LARGE_INTEGER *))
		   GetProcAddress(module, "QueryPerformanceFrequency"));
	    if (!QPF)
		goto get_tick_count64;
	    if (!(*QPF)(&pf))
		goto get_tick_count64;

	    internal_state.r.o.pQueryPerformanceCounter
		= ((BOOL (WINAPI *)(LARGE_INTEGER *))
		   GetProcAddress(module, "QueryPerformanceCounter"));
	    if (!internal_state.r.o.pQueryPerformanceCounter)
		goto get_tick_count64;

	    if (pf.QuadPart > (((LONGLONG) 1) << 32))
		goto get_tick_count64;

	    internal_state.r.o.pcf = (Uint32) pf.QuadPart;
	    sys_hrtime_func = sys_hrtime_qpc;
	    
	    /*
	     * We only use QueryPerformanceCounter() for
	     * os-monotonic-time if its frequency is equal
	     * to, or larger than GHz in order to ensure
	     * that the user wont be able to observe faulty
	     * order between values retrieved on different threads.
	     */
	    if (pf.QuadPart < (LONGLONG) 1000*1000*1000)
		goto get_tick_count64;

	    if (ERTS_DISABLE_USE_OF_QPC_FOR_MONOTONIC_TIME)
		goto get_tick_count64;

	    init_resp->os_monotonic_time_info.func = "QueryPerformanceCounter";
	    init_resp->os_monotonic_time_info.locked_use = 0;
	    time_unit = (ErtsMonotonicTime) pf.QuadPart;
	    internal_state.r.o.using_get_tick_count_time_unit = 0;
	    init_resp->os_monotonic_time_info.resolution = time_unit;
	    os_mtime_func = os_monotonic_time_qpc;
	    os_times_func = os_times_qpc;
	}
    }

    erts_sys_time_data__.r.o.os_monotonic_time = os_mtime_func;
    erts_sys_time_data__.r.o.os_times = os_times_func;
    erts_sys_time_data__.r.o.sys_hrtime = sys_hrtime_func;
    init_resp->os_monotonic_time_unit = time_unit;
    init_resp->have_os_monotonic_time = 1;
    init_resp->have_corrected_os_monotonic_time = 0;
    init_resp->sys_clock_resolution = 1;

    init_resp->os_system_time_info.func = "GetSystemTime";    
    init_resp->os_system_time_info.clock_id = NULL;
    init_resp->os_system_time_info.resolution = 100;
    init_resp->os_system_time_info.locked_use = 0;

    if(GetTimeZoneInformation(&static_tzi) && 
       static_tzi.StandardDate.wMonth != 0 &&
       static_tzi.DaylightDate.wMonth != 0) {
	have_static_tzi = 1;
    }
}

void
erts_late_sys_init_time(void)
{
    if (erts_sys_time_data__.r.o.os_monotonic_time == os_monotonic_time_gtc32)
	erts_late_init_os_monotonic_time_extender(&internal_state.wr.m.os_mtime_xtnd);
}

/* Returns a switchtimes for DST as UTC filetimes given data from a 
   TIME_ZONE_INFORMATION, see sys_localtime_r for usage. */ 
static void 
get_dst_switchtime(DWORD year, 
		   SYSTEMTIME dstinfo, LONG bias,
		   FILETIME *utc_switchtime) 
{
    DWORD occu;
    DWORD weekday,wday_1st;
    DWORD day, days_in;
    FILETIME tmp,tmp2;
    ULARGE_INTEGER ull;
    int leap_year = 0;
    if (dstinfo.wYear != 0) {
	/* A year specific transition, in which case the data in the structure
	   is already properly set for a specific year. Compare year
	   with parameter and see if they correspond, in that case generate a 
	   filetime directly, otherwise set the filetime to 0 */
	if (year != dstinfo.wYear) {
	    utc_switchtime->dwLowDateTime = utc_switchtime->dwHighDateTime = 0;
	    return;
	} 
    } else {
	occu = dstinfo.wDay;
	weekday = dstinfo.wDayOfWeek;

	dstinfo.wDayOfWeek = 0;
	dstinfo.wDay = 1;
	dstinfo.wYear = year;
	
	SystemTimeToFileTime(&dstinfo,&tmp);
	ull.LowPart = tmp.dwLowDateTime;
	ull.HighPart = tmp.dwHighDateTime;
    
	ull.QuadPart /= (TICKS_PER_SECOND*SECONDS_PER_DAY); /* Julian Day */
	wday_1st = (DWORD) ((ull.QuadPart + LL_LITERAL(1)) % LL_LITERAL(7));
	day = (weekday >= wday_1st) ? 
	    weekday - wday_1st + 1 :
	    weekday - wday_1st + 8;
	--occu;
	if (((dstinfo.wYear % 4) == 0 && (dstinfo.wYear % 100) > 0) ||
	    ((dstinfo.wYear % 400) == 0)) {
	    leap_year = 1;
	}
	days_in = days_in_month[leap_year][dstinfo.wMonth];
	while (occu > 0 && (day + 7 <= days_in)) {
	    --occu;
	    day += 7;
	}
	dstinfo.wDay = day;
    }
    SystemTimeToFileTime(&dstinfo,&tmp);
    /* correct for bias */
    ull.LowPart = tmp.dwLowDateTime;
    ull.HighPart = tmp.dwHighDateTime;
    ull.QuadPart += (((LONGLONG) bias) * LL_LITERAL(60) * TICKS_PER_SECOND);
    utc_switchtime->dwLowDateTime = ull.LowPart;
    utc_switchtime->dwHighDateTime = ull.HighPart;
    return;
}

/* This function gives approximately the correct year from a FILETIME
   Around the actual new year, it may return the wrong value, but that's OK
   as DST never switches around new year. */
static DWORD 
approx_year(FILETIME ft)
{
    ULARGE_INTEGER ull;
    FILETIME_TO_ULI(ull,ft);
    ull.QuadPart /= LL_LITERAL(1000);
    ull.QuadPart /= SECONDS_PER_DAY;
    ull.QuadPart /= LL_LITERAL(3652425);
    ull.QuadPart += 1601;
    return (DWORD) ull.QuadPart;
}

struct tm *
sys_localtime_r(time_t *epochs, struct tm *ptm)
{
    FILETIME ft,lft;
    SYSTEMTIME st;
    
    if ((((*epochs) + EPOCH_JULIAN_DIFF) * TICKS_PER_SECOND) < 0LL) {
	fprintf(stderr,"1\r\n"); fflush(stderr);
	return NULL;
    }
    
    EPOCH_TO_FILETIME(ft,*epochs);
    ptm->tm_isdst = 0;
    if (have_static_tzi) {
	FILETIME dst_start, dst_stop;
	ULARGE_INTEGER ull;
	DWORD year = approx_year(ft);
	get_dst_switchtime(year,static_tzi.DaylightDate,
			   static_tzi.Bias+static_tzi.StandardBias,&dst_start);
	get_dst_switchtime(year,static_tzi.StandardDate,
			   static_tzi.Bias+static_tzi.StandardBias+
			   static_tzi.DaylightBias,
			   &dst_stop);
	FILETIME_TO_ULI(ull,ft);

	if (CompareFileTime(&ft,&dst_start) >= 0 && 
	    CompareFileTime(&ft,&dst_stop) < 0) {
	    ull.QuadPart -= 
		((LONGLONG) static_tzi.Bias+static_tzi.StandardBias+
		 static_tzi.DaylightBias) * 
		LL_LITERAL(60) * TICKS_PER_SECOND;
	    ptm->tm_isdst = 1;
	} else {
	    ull.QuadPart -= 
		((LONGLONG) static_tzi.Bias+static_tzi.StandardBias) 
		* LL_LITERAL(60) * TICKS_PER_SECOND;
	}
	ULI_TO_FILETIME(ft,ull);
    } else {
	if (!FileTimeToLocalFileTime(&ft,&lft)) {
	    return NULL;
	}
	ft = lft;
    }
    
    if (!FileTimeToSystemTime(&ft,&st)) {
	return NULL;
    }
    
    ptm->tm_year = (int) st.wYear - 1900;
    ptm->tm_mon  = (int) st.wMonth - 1;
    ptm->tm_mday = (int) st.wDay;
    ptm->tm_hour = (int) st.wHour;
    ptm->tm_min  = (int) st.wMinute;
    ptm->tm_sec  = (int) st.wSecond;
    ptm->tm_wday  = (int) st.wDayOfWeek;
    {
	int yday = ptm->tm_mday - 1;
	int m =  ptm->tm_mon;
	int leap_year = 0;
	if (((st.wYear % 4) == 0 && (st.wYear % 100) > 0) ||
	    ((st.wYear % 400) == 0)) {
	    leap_year = 1;
	}
	while (m > 0) {
	    yday +=days_in_month[leap_year][m];
	    --m;
	}
	ptm->tm_yday = yday;
    }
    return ptm;
}

struct tm * 
sys_gmtime_r(time_t *epochs, struct tm *ptm)
{
    FILETIME ft;
    SYSTEMTIME st;
    
    if ((((*epochs) + EPOCH_JULIAN_DIFF) * TICKS_PER_SECOND) < 0LL) {
	return NULL;
    }
    
    EPOCH_TO_FILETIME(ft,*epochs);
    
    if (!FileTimeToSystemTime(&ft,&st)) {
	return NULL;
    }
    
    ptm->tm_year = (int) st.wYear - 1900;
    ptm->tm_mon  = (int) st.wMonth - 1;
    ptm->tm_mday = (int) st.wDay;
    ptm->tm_hour = (int) st.wHour;
    ptm->tm_min  = (int) st.wMinute;
    ptm->tm_sec  = (int) st.wSecond;
    ptm->tm_wday  = (int) st.wDayOfWeek;
    ptm->tm_isdst  = 0;
    {
	int yday = ptm->tm_mday - 1;
	int m =  ptm->tm_mon;
	int leap_year = 0;
	if (((st.wYear % 4) == 0 && (st.wYear % 100) > 0) ||
	    ((st.wYear % 400) == 0)) {
	    leap_year = 1;
	}
	while (m > 0) {
	    yday +=days_in_month[leap_year][m];
	    --m;
	}
	ptm->tm_yday = yday;
    }

    return ptm;
}

time_t 
sys_mktime(struct tm *ptm) 
{
    FILETIME ft;
    SYSTEMTIME st;
    int dst = 0;
    time_t epochs;

    memset(&st,0,sizeof(st));
    /* Convert relevant parts of truct tm to SYSTEMTIME */
    st.wYear =  (USHORT) (ptm->tm_year + 1900);
    st.wMonth =  (USHORT) (ptm->tm_mon + 1);
    st.wDay =  (USHORT) ptm->tm_mday;
    st.wHour =  (USHORT) ptm->tm_hour;
    st.wMinute =  (USHORT) ptm->tm_min;
    st.wSecond = (USHORT) ptm->tm_sec;

    SystemTimeToFileTime(&st,&ft);
    
    /* ft is now some kind of local file time, but it may be wrong depending 
       on what is in the tm_dst field. We need to manually convert it to
       UTC before turning it into epochs */

    if (have_static_tzi) {
	FILETIME dst_start, dst_stop;
	ULARGE_INTEGER ull_start,ull_stop,ull_ft;

	FILETIME_TO_ULI(ull_ft,ft);

	/* Correct everything except DST */
	ull_ft.QuadPart += (static_tzi.Bias+static_tzi.StandardBias) 
	    * LL_LITERAL(60) * TICKS_PER_SECOND;

	/* Determine if DST is active */
	if (ptm->tm_isdst >= 0) {
	    dst = ptm->tm_isdst;
	} else if (static_tzi.DaylightDate.wMonth != 0){
	    /* This is how windows mktime does it, meaning it does not
	       take nonexisting local times into account */
	    get_dst_switchtime(st.wYear,static_tzi.DaylightDate,
			       static_tzi.Bias+static_tzi.StandardBias,
			       &dst_start);
	    get_dst_switchtime(st.wYear,static_tzi.StandardDate,
			       static_tzi.Bias+static_tzi.StandardBias+
			       static_tzi.DaylightBias,
			       &dst_stop);
	    FILETIME_TO_ULI(ull_start,dst_start);
	    FILETIME_TO_ULI(ull_stop,dst_stop);
	    if ((ull_ft.QuadPart >= ull_start.QuadPart) &&
		(ull_ft.QuadPart < ull_stop.QuadPart)) {
		/* We are in DST */
		dst = 1;
	    }
	}
	/* Correct for DST */
	if (dst) {
	    ull_ft.QuadPart += static_tzi.DaylightBias * 
		LL_LITERAL(60) * TICKS_PER_SECOND;
	}
	epochs = ((ull_ft.QuadPart / TICKS_PER_SECOND) - EPOCH_JULIAN_DIFF);
    } else {
	/* No DST, life is easy... */
	FILETIME lft;
	LocalFileTimeToFileTime(&ft,&lft);
	FILETIME_TO_EPOCH(epochs,lft);
    }
    /* Normalize the struct tm */
    sys_localtime_r(&epochs,ptm);
    return epochs;
}

void 
sys_gettimeofday(SysTimeval *tv)
{
    SYSTEMTIME t;
    FILETIME ft;
    ULARGE_INTEGER ull;

    GetSystemTime(&t);
    SystemTimeToFileTime(&t, &ft);
    FILETIME_TO_ULI(ull,ft);
    tv->tv_usec = (long) ((ull.QuadPart / LL_LITERAL(10)) % 
			  LL_LITERAL(1000000));
    tv->tv_sec = (long) ((ull.QuadPart / LL_LITERAL(10000000)) - 
			 EPOCH_JULIAN_DIFF);
}

ErtsSystemTime
erts_os_system_time(void)
{
    SYSTEMTIME st;
    ErtsSystemTime stime;

    GetSystemTime(&st);
    stime = SystemTime2MilliSec(&st);

    if (internal_state.r.o.using_get_tick_count_time_unit) {
	stime <<= ERTS_GET_TICK_COUNT_TIME_UNIT_SHIFT;
	return stime;
    }

    return ((ErtsSystemTime)
	    erts_time_unit_conversion((Uint64) stime,
				      (Uint32) 1000,
				      internal_state.r.o.pcf));
}


clock_t 
sys_times(SysTimes *buffer) {
    clock_t kernel_ticks = (GetTickCount() / 
			    (1000 / SYS_CLK_TCK)) & 0x7FFFFFFF;
    FILETIME dummy;
    LONGLONG user;
    LONGLONG system;
    
    buffer->tms_utime = buffer->tms_stime = buffer->tms_cutime = 
	buffer->tms_cstime = 0;

    if (GetProcessTimes(GetCurrentProcess(), &dummy, &dummy,
			(FILETIME *) &system, (FILETIME *) &user) == 0)
	return kernel_ticks;
    system /= (LONGLONG)(10000000 / SYS_CLK_TCK);
    user /= (LONGLONG)(10000000 / SYS_CLK_TCK);
    
    buffer->tms_utime = (clock_t) (user & LL_LITERAL(0x7FFFFFFF));
    buffer->tms_stime = (clock_t) (system & LL_LITERAL(0x7FFFFFFF));
    return kernel_ticks;
}
