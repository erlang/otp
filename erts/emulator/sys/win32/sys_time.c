/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1997-2009. All Rights Reserved.
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

#ifdef __GNUC__
#define LL_LITERAL(X) X##LL
#else
#define LL_LITERAL(X) X##i64
#endif

/******************* Routines for time measurement *********************/

#define EPOCH_JULIAN_DIFF LL_LITERAL(11644473600)
#define TICKS_PER_SECOND (10000000LL)

#define EPOCH_TO_FILETIME(ft, epoch) \
    do { \
	ULARGE_INTEGER ull; \
	ull.QuadPart = (((epoch) + EPOCH_JULIAN_DIFF) * TICKS_PER_SECOND); \
	(ft).dwLowDateTime = ull.LowPart; \
	(ft).dwHighDateTime = ull.HighPart; \
    } while(0)

#define FILETIME_TO_EPOCH(epoch, ft) \
    do { \
	ULARGE_INTEGER ull; \
	ull.LowPart  = (ft).dwLowDateTime; \
	ull.HighPart = (ft).dwHighDateTime; \
	(epoch) = ((ull.QuadPart / TICKS_PER_SECOND) - EPOCH_JULIAN_DIFF); \
    } while(0)
 
static SysHrTime wrap = 0;
static DWORD last_tick_count = 0;

int 
sys_init_time(void)
{
    return 1;
}

struct tm * sys_localtime_r(time_t *epochs, struct tm *ptm)
{
    FILETIME ft,lft;
    SYSTEMTIME st;
    
    if ((((*epochs) + EPOCH_JULIAN_DIFF) * TICKS_PER_SECOND) < 0LL) {
	return NULL;
    }
    
    EPOCH_TO_FILETIME(ft,*epochs);
    
    if (!FileTimeToLocalFileTime(&ft,&lft)) {
	return NULL;
    }
    
    if (!FileTimeToSystemTime(&lft,&st)) {
	return NULL;
    }
    
    ptm->tm_year = (int) st.wYear - 1900;
    ptm->tm_mon  = (int) st.wMonth - 1;
    ptm->tm_mday = (int) st.wDay;
    ptm->tm_hour = (int) st.wHour;
    ptm->tm_min  = (int) st.wMinute;
    ptm->tm_sec  = (int) st.wSecond;

    return ptm;
}

void 
sys_gettimeofday(SysTimeval *tv)
{
    SYSTEMTIME t;
    FILETIME ft;
    LONGLONG lft;

    GetSystemTime(&t);
    SystemTimeToFileTime(&t, &ft);
    memcpy(&lft, &ft, sizeof(lft));
    tv->tv_usec = (long) ((lft / LL_LITERAL(10)) % LL_LITERAL(1000000));
    tv->tv_sec = (long) ((lft / LL_LITERAL(10000000)) - EPOCH_JULIAN_DIFF);
}

SysHrTime 
sys_gethrtime(void) 
{
    DWORD ticks = (SysHrTime) (GetTickCount() & 0x7FFFFFFF);
    if (ticks < (SysHrTime) last_tick_count) {
	wrap += LL_LITERAL(1) << 31;
    }
    last_tick_count = ticks;
    return ((((LONGLONG) ticks) + wrap) * LL_LITERAL(1000000));
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
