/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1997-2013. All Rights Reserved.
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
 * Purpose: System-dependent time functions.
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif
#include "sys.h"
#include "assert.h"

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
 
static SysHrTime wrap = 0;
static DWORD last_tick_count = 0;
static erts_smp_mtx_t wrap_lock;
static ULONGLONG (WINAPI *pGetTickCount64)(void) = NULL;

/* Getting timezone information is a heavy operation, so we want to do this 
   only once */

static TIME_ZONE_INFORMATION static_tzi;
static int have_static_tzi = 0;

static int days_in_month[2][13] = {
    {0,31,28,31,30,31,30,31,31,30,31,30,31},
    {0,31,29,31,30,31,30,31,31,30,31,30,31}};

int 
sys_init_time(void)
{
    char kernel_dll_name[] = "kernel32";
    HMODULE module;

    module = GetModuleHandle(kernel_dll_name);
    pGetTickCount64 = (module != NULL) ? 
	(ULONGLONG (WINAPI *)(void)) 
	GetProcAddress(module,"GetTickCount64") : 
	NULL;

    if(GetTimeZoneInformation(&static_tzi) && 
       static_tzi.StandardDate.wMonth != 0 &&
       static_tzi.DaylightDate.wMonth != 0) {
	have_static_tzi = 1;
    }

    erts_smp_mtx_init(&wrap_lock, "sys_gethrtime");

    return 1;
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

extern int erts_initialized;
SysHrTime 
sys_gethrtime(void) 
{
    if (pGetTickCount64 != NULL) {
	return ((SysHrTime) pGetTickCount64()) * LL_LITERAL(1000000);
    } else {
	DWORD ticks;
	SysHrTime res;
	erts_smp_mtx_lock(&wrap_lock);
	ticks = (SysHrTime) (GetTickCount() & 0x7FFFFFFF);
	if (ticks < (SysHrTime) last_tick_count) {
	    /* Detect a race that should no longer be here... */
	    if ((((SysHrTime) last_tick_count) - ((SysHrTime) ticks)) > 1000) {
		wrap += LL_LITERAL(1) << 31;
	    } else {
		/* 
		 * XXX Debug: Violates locking order, remove all this,
		 * after testing!
		 */
		erts_dsprintf_buf_t *dsbufp = erts_create_logger_dsbuf();
		erts_dsprintf(dsbufp, "Did not wrap when last_tick %d "
			      "and tick %d", 
			      last_tick_count, ticks);
		erts_send_error_to_logger_nogl(dsbufp);
		ticks = last_tick_count;
	    }
	}
	last_tick_count = ticks;
	res = ((((LONGLONG) ticks) + wrap) * LL_LITERAL(1000000));
	erts_smp_mtx_unlock(&wrap_lock);
	return res;
    }
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
