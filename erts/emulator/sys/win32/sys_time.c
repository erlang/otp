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

#include "sys.h"
#include "assert.h"

#ifdef __GNUC__
#define LL_LITERAL(X) X##LL
#else
#define LL_LITERAL(X) X##i64
#endif

/******************* Routines for time measurement *********************/

#define EPOCH_JULIAN_DIFF LL_LITERAL(11644473600)

static SysHrTime wrap = 0;
static DWORD last_tick_count = 0;

int 
sys_init_time(void)
{
    return 1;
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






