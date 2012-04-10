/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1999-2011. All Rights Reserved.
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
** Support routines for the timer wheel
**
** This code contains two strategies for dealing with 
** date/time changes in the system. 
** If the system has some kind of high resolution timer (HAVE_GETHRTIME),
** the high resolution timer is used to correct the time-of-day and the
** timeouts, the base source is the hrtimer, but at certain intervals the 
** OS time-of-day is checked and if it is not within certain bounds, the 
** delivered time gets slowly adjusted for each call until
** it corresponds to the system time (built-in adjtime...). 
** The call gethrtime() is detected by autoconf on Unix, but other 
** platforms may define it in erl_*_sys.h and implement 
** their own high resolution timer. The high resolution timer
** strategy is (probably) best on all systems where the timer have 
** a resolution higher or equal to gettimeofday (or what's implemented
** is sys_gettimeofday()). The actual resolution is the interesting thing,
** not the unit's thats used (i.e. on VxWorks, nanoseconds can be
** retrieved in terms of units, but the actual resolution is the same as 
** for the clock ticks).
** If the systems best timer routine is kernel ticks returned from 
** sys_times(), and the actual resolution of sys_gettimeofday() is
** better (like most unixes that does not have any realtime extensions), 
** another strategy is used. The tolerant gettimeofday() corrects 
** the value with respect to uptime (sys_times() return value) and checks 
** for correction both when delivering timeticks and delivering nowtime.
** this strategy is slower, but accurate on systems without better timer 
** routines. The kernel tick resolution is not enough to implement
** a gethrtime routine. On Linux and other non solaris unix-boxes the second 
** strategy is used, on all other platforms we use the first.
** 
** The following is expected (from sys.[ch] and erl_*_sys.h):
**
** 64 bit integers. So it is, and so it will be.
**
** sys_init_time(), will return the clock resolution in MS and
** that's about it. More could be added of course
** If the clock-rate is constant (i.e. 1 ms) one can define 
** SYS_CLOCK_RESOLUTION (to 1),
** which makes erts_deliver_time/erts_time_remaining a bit faster.
**
** if HAVE_GETHRTIME is defined:
**    sys_gethrtime() will return a SysHrTime (long long) representing 
**    nanoseconds, sys_init_hrtime() will do any initialization.
** else
**    a long (64bit) integer type called Sint64 should be defined.
**
** sys_times() will return clock_ticks since start and 
**    fill in a SysTimes structure (struct tms). Instead of CLK_TCK, 
**    SYS_CLK_TCK is used to determine the resolution of kernel ticks.
**
** sys_gettimeofday() will take a SysTimeval (a struct timeval) as parameter
**    and fill it in as gettimeofday(X,NULL).
**
*/

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "erl_vm.h"
#include "global.h"

static erts_smp_mtx_t erts_timeofday_mtx;

static SysTimeval inittv; /* Used everywhere, the initial time-of-day */

static SysTimes t_start; /* Used in elapsed_time_both */
static SysTimeval gtv; /* Used in wall_clock_elapsed_time_both */
static SysTimeval then; /* Used in get_now */
static SysTimeval last_emu_time; /* Used in erts_get_emu_time() */
SysTimeval erts_first_emu_time; /* Used in erts_get_emu_time() */


#ifdef HAVE_GETHRTIME

int erts_disable_tolerant_timeofday;

static SysHrTime hr_init_time, hr_last_correction_check, 
    hr_correction, hr_last_time;

static void init_tolerant_timeofday(void)
{
    /* Should be in sys.c */
#if defined(HAVE_SYSCONF) && defined(_SC_NPROCESSORS_CONF)
    if (sysconf(_SC_NPROCESSORS_CONF) > 1) {
	char b[1024];
	int maj,min,build;
	os_flavor(b,1024);
	os_version(&maj,&min,&build);
	if (!strcmp(b,"sunos") && maj <= 5 && min <= 7) {
	    erts_disable_tolerant_timeofday = 1;
	}
    }
#endif
    hr_init_time = sys_gethrtime();
    hr_last_correction_check = hr_last_time = hr_init_time;
    hr_correction = 0;
}

static void get_tolerant_timeofday(SysTimeval *tv)
{
    SysHrTime diff_time, curr;

    if (erts_disable_tolerant_timeofday) {
	sys_gettimeofday(tv);
	return;
    }
    *tv = inittv;
    diff_time = ((curr = sys_gethrtime()) + hr_correction - hr_init_time) / 1000; 

    if (curr < hr_init_time) {
	erl_exit(1,"Unexpected behaviour from operating system high "
		 "resolution timer");
    }

    if ((curr - hr_last_correction_check) / 1000 > 1000000) {
	/* Check the correction need */
	SysHrTime tv_diff, diffdiff;
	SysTimeval tmp;
	int done = 0;

	sys_gettimeofday(&tmp);
	tv_diff = ((SysHrTime) tmp.tv_sec) * 1000000 + tmp.tv_usec;
	tv_diff -= ((SysHrTime) inittv.tv_sec) * 1000000 + inittv.tv_usec;
	diffdiff = diff_time - tv_diff;
	if (diffdiff > 10000) {
	    SysHrTime corr = (curr - hr_last_time) / 100;
	    if (corr / 1000 >= diffdiff) {
		++done;
		hr_correction -= ((SysHrTime)diffdiff) * 1000;
	    } else {
		hr_correction -= corr;
	    }
	    diff_time = (curr + hr_correction - hr_init_time) / 1000; 
	} else if (diffdiff < -10000) {
	    SysHrTime corr = (curr - hr_last_time) / 100;
	    if (corr / 1000 >= -diffdiff) {
		++done;
		hr_correction -= ((SysHrTime)diffdiff) * 1000;
	    } else {
		hr_correction += corr;
	    }
	    diff_time = (curr + hr_correction - hr_init_time) / 1000; 
	} else {
	    ++done;
	}
	if (done) {
	    hr_last_correction_check = curr;
	}
    }
    tv->tv_sec += (int) (diff_time / ((SysHrTime) 1000000));
    tv->tv_usec += (int) (diff_time % ((SysHrTime) 1000000));
    if (tv->tv_usec >= 1000000) {
	tv->tv_usec -= 1000000;
	tv->tv_sec += 1;
    }
    hr_last_time = curr;
}

#define correction (hr_correction/1000000)

#else /* !HAVE_GETHRTIME */
#if !defined(CORRECT_USING_TIMES) 
#define init_tolerant_timeofday() 
#define get_tolerant_timeofday(tvp) sys_gettimeofday(tvp)
#else

typedef Sint64 Milli;

static clock_t init_ct;
static Sint64 ct_wrap;
static Milli init_tv_m;
static Milli correction_supress; 
static Milli last_ct_diff;
static Milli last_cc; 
static clock_t last_ct;

/* sys_times() might need to be wrapped and the values shifted (right)
   a bit to cope with newer linux (2.5.*) kernels, this has to be taken care 
   of dynamically to start with, a special version that uses
   the times() return value as a high resolution timer can be made
   to fully utilize the faster ticks, like on windows, but for now, we'll
   settle with this silly workaround */
#ifdef ERTS_WRAP_SYS_TIMES 
#define KERNEL_TICKS() (sys_times_wrap() &  \
			((1UL << ((sizeof(clock_t) * 8) - 1)) - 1)) 
#else
SysTimes dummy_tms;

#define KERNEL_TICKS() (sys_times(&dummy_tms) &  \
			((1UL << ((sizeof(clock_t) * 8) - 1)) - 1)) 

#endif

static void init_tolerant_timeofday(void)
{
    last_ct = init_ct = KERNEL_TICKS();
    last_cc = 0;
    init_tv_m = (((Milli) inittv.tv_sec) * 1000) + 
	(inittv.tv_usec / 1000);
    ct_wrap = 0;
    correction_supress = 0;
}


static void get_tolerant_timeofday(SysTimeval *tvp)
{
    clock_t current_ct;
    SysTimeval current_tv;
    Milli ct_diff;
    Milli tv_diff;
    Milli current_correction;
    Milli act_correction;	/* long shown to be too small */
    Milli max_adjust;

    if (erts_disable_tolerant_timeofday) {
	sys_gettimeofday(tvp);
	return;
    }

#ifdef ERTS_WRAP_SYS_TIMES 
#define TICK_MS (1000 / SYS_CLK_TCK_WRAP)
#else
#define TICK_MS (1000 / SYS_CLK_TCK)
#endif
    current_ct = KERNEL_TICKS();
    sys_gettimeofday(&current_tv);

    /* I dont know if uptime can move some units backwards
       on some systems, but I allow for small backward 
       jumps to avoid such problems if they exist...*/
    if (last_ct > 100 && current_ct < (last_ct - 100)) {
	ct_wrap += ((Sint64) 1) << ((sizeof(clock_t) * 8) - 1);
    }
    last_ct = current_ct;
    ct_diff = ((ct_wrap + current_ct) - init_ct) * TICK_MS;

    /*
     * We will adjust the time in milliseconds and we allow for 1% 
     * adjustments, but if this function is called more often then every 100 
     * millisecond (which is obviously possible), we will never adjust, so 
     * we accumulate small times by setting last_ct_diff iff max_adjust > 0
     */
    if ((max_adjust = (ct_diff - last_ct_diff)/100) > 0)
	last_ct_diff = ct_diff;

    tv_diff = ((((Milli) current_tv.tv_sec) * 1000) + 
	       (current_tv.tv_usec / 1000)) - init_tv_m;

    current_correction = ((ct_diff - tv_diff) / TICK_MS) * TICK_MS; /* trunc */

    /* 
     * We allow the current_correction value to wobble a little, as it
     * suffers from the low resolution of the kernel ticks. 
     * if it hasn't changed more than one tick in either direction, 
     * we will keep the old value.
     */
    if ((last_cc > current_correction + TICK_MS) ||
	(last_cc < current_correction - TICK_MS)) {
	last_cc = current_correction;
    } else {
	current_correction = last_cc;
    }
    
    /*
     * As time goes, we try to get the actual correction to 0, 
     * that is, make erlangs time correspond to the systems dito.
     * The act correction is what we seem to need (current_correction)
     * minus the correction suppression. The correction supression
     * will change slowly (max 1% of elapsed time) but in millisecond steps.
     */
    act_correction = current_correction - correction_supress;
    if (max_adjust > 0) {
	/*
	 * Here we slowly adjust erlangs time to correspond with the 
	 * system time by changing the correction_supress variable.
	 * It can change max_adjust milliseconds which is 1% of elapsed time
	 */
	if (act_correction > 0) {
	    if (current_correction - correction_supress > max_adjust) {
		correction_supress += max_adjust;
	    } else {
		correction_supress = current_correction;
	    }
	    act_correction = current_correction - correction_supress;
	} else if (act_correction < 0) {
	    if (correction_supress - current_correction > max_adjust) {
		correction_supress -= max_adjust;
	    } else {
		correction_supress = current_correction;
	    }
	    act_correction = current_correction - correction_supress;
	}
    }
    /*
     * The actual correction will correct the timeval so that system 
     * time warps gets smothed down.
     */
    current_tv.tv_sec += act_correction / 1000;
    current_tv.tv_usec += (act_correction % 1000) * 1000;

    if (current_tv.tv_usec >= 1000000) {
	++current_tv.tv_sec ;
	current_tv.tv_usec -= 1000000;
    } else if (current_tv.tv_usec < 0) {
	--current_tv.tv_sec;
	current_tv.tv_usec += 1000000;
    }
    *tvp = current_tv;
#undef TICK_MS
}

#endif /* CORRECT_USING_TIMES */
#endif /* !HAVE_GETHRTIME */

/*
** Why this? Well, most platforms have a constant clock resolution of 1,
** we dont want the deliver_time/time_remaining routines to waste 
** time dividing and multiplying by/with a variable that's always one.
** so the return value of sys_init_time is ignored on those platforms.
*/
 
#ifndef SYS_CLOCK_RESOLUTION
static int clock_resolution;
#define CLOCK_RESOLUTION clock_resolution
#else
#define CLOCK_RESOLUTION SYS_CLOCK_RESOLUTION
#endif

/*
** The clock resolution should really be the resolution of the 
** time function in use, which on most platforms 
** is 1. On VxWorks the resolution should be
** the number of ticks per second (or 1, which would work nicely to).
**
** Setting lower resolutions is mostly interesting when timers are used
** instead of something like select.
*/

static SysTimeval last_delivered; 

static void init_erts_deliver_time(const SysTimeval *inittv)
{
    /* We set the initial values for deliver_time here */
    last_delivered = *inittv;
    last_delivered.tv_usec = 1000 * (last_delivered.tv_usec / 1000); 
                                                   /* ms resolution */
}

static void do_erts_deliver_time(const SysTimeval *current)
{
    SysTimeval cur_time;
    erts_time_t elapsed;
    
    /* calculate and deliver appropriate number of ticks */
    cur_time = *current;
    cur_time.tv_usec = 1000 * (cur_time.tv_usec / 1000); /* ms resolution */
    elapsed = (1000 * (cur_time.tv_sec - last_delivered.tv_sec) +
	       (cur_time.tv_usec - last_delivered.tv_usec) / 1000) / 
	CLOCK_RESOLUTION;

    /* Sometimes the time jump backwards,
       resulting in a negative elapsed time. We compensate for
       this by simply pretend as if the time stood still. :) */

    if (elapsed > 0) {

	ASSERT(elapsed < ((erts_time_t) ERTS_SHORT_TIME_T_MAX));

	erts_do_time_add((erts_short_time_t) elapsed);
	last_delivered = cur_time;
    }
}

int 
erts_init_time_sup(void)
{
    erts_smp_mtx_init(&erts_timeofday_mtx, "timeofday");

    last_emu_time.tv_sec = 0;
    last_emu_time.tv_usec = 0;

#ifndef SYS_CLOCK_RESOLUTION
    clock_resolution = sys_init_time();
#else
    (void) sys_init_time();
#endif
    sys_gettimeofday(&inittv);
    
#ifdef HAVE_GETHRTIME
    sys_init_hrtime();
#endif
    init_tolerant_timeofday();

    init_erts_deliver_time(&inittv);
    gtv = inittv;
    then.tv_sec = then.tv_usec = 0;

    erts_get_emu_time(&erts_first_emu_time);

    return CLOCK_RESOLUTION;
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
    UWord prev_total;
    SysTimeval tv;

    erts_smp_mtx_lock(&erts_timeofday_mtx);

    get_tolerant_timeofday(&tv);

    *ms_total = 1000 * (tv.tv_sec - inittv.tv_sec) +
	(tv.tv_usec - inittv.tv_usec) / 1000;

    prev_total = 1000 * (gtv.tv_sec - inittv.tv_sec) +
	(gtv.tv_usec - inittv.tv_usec) / 1000;
    *ms_diff = *ms_total - prev_total;
    gtv = tv;

    /* must sync the machine's idea of time here */
    do_erts_deliver_time(&tv);

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
	    if (erl_mktime(&the_clock, &t)) {
		/* Failed anyway, something else is bad - will be a badarg */
		return 0;
	    }
	} else {
	    /* Something else is the matter, badarg. */
	    return 0;
	}
    }
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
    SysTimeval now;
    
    erts_smp_mtx_lock(&erts_timeofday_mtx);
    
    get_tolerant_timeofday(&now);
    do_erts_deliver_time(&now);

    /* Make sure time is later than last */
    if (then.tv_sec > now.tv_sec ||
	(then.tv_sec == now.tv_sec && then.tv_usec >= now.tv_usec)) {
	now = then;
	now.tv_usec++;
    }
    /* Check for carry from above + general reasonability */
    if (now.tv_usec >= 1000000) {
	now.tv_usec = 0;
	now.tv_sec++;
    }
    then = now;
    
    erts_smp_mtx_unlock(&erts_timeofday_mtx);
    
    *megasec = (Uint) (now.tv_sec / 1000000);
    *sec = (Uint) (now.tv_sec % 1000000);
    *microsec = (Uint) (now.tv_usec);
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


/* deliver elapsed *ticks* to the machine - takes a pointer
   to a struct timeval representing current time (to save
   a gettimeofday() where possible) or NULL */

void erts_deliver_time(void) {
    SysTimeval now;
    
    erts_smp_mtx_lock(&erts_timeofday_mtx);
    
    get_tolerant_timeofday(&now);
    do_erts_deliver_time(&now);
    
    erts_smp_mtx_unlock(&erts_timeofday_mtx);
}

/* get *real* time (not ticks) remaining until next timeout - if there
   isn't one, give a "long" time, that is guaranteed
   to not cause overflow when we report elapsed time later on */

void erts_time_remaining(SysTimeval *rem_time)
{
    erts_time_t ticks;
    SysTimeval cur_time;
    erts_time_t elapsed;

    /* erts_next_time() returns no of ticks to next timeout or -1 if none */

    ticks = (erts_time_t) erts_next_time();
    if (ticks == (erts_time_t) -1) {
	/* timer queue empty */
	/* this will cause at most 100000000 ticks */
	rem_time->tv_sec = 100000;
	rem_time->tv_usec = 0;
    } else {
	/* next timeout after ticks ticks */
	ticks *= CLOCK_RESOLUTION;
	
	erts_smp_mtx_lock(&erts_timeofday_mtx);
	
	get_tolerant_timeofday(&cur_time);
	cur_time.tv_usec = 1000 * 
	    (cur_time.tv_usec / 1000);/* ms resolution*/
	elapsed = 1000 * (cur_time.tv_sec - last_delivered.tv_sec) +
	    (cur_time.tv_usec - last_delivered.tv_usec) / 1000;
	
	erts_smp_mtx_unlock(&erts_timeofday_mtx);
	
	if (ticks <= elapsed) { /* Ooops, better hurry */
	    rem_time->tv_sec = rem_time->tv_usec = 0;
	    return;
	}
	rem_time->tv_sec = (ticks - elapsed) / 1000;
	rem_time->tv_usec = 1000 * ((ticks - elapsed) % 1000);
    }
}

void erts_get_timeval(SysTimeval *tv)
{
    erts_smp_mtx_lock(&erts_timeofday_mtx);
    get_tolerant_timeofday(tv);
    erts_smp_mtx_unlock(&erts_timeofday_mtx);
}

erts_time_t
erts_get_time(void)
{
    SysTimeval sys_tv;
    
    erts_smp_mtx_lock(&erts_timeofday_mtx);
    
    get_tolerant_timeofday(&sys_tv);
    
    erts_smp_mtx_unlock(&erts_timeofday_mtx);
    
    return sys_tv.tv_sec;
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


/*
 * erts_get_emu_time() is similar to get_now(). You will
 * always get different times from erts_get_emu_time(), but they
 * may equal a time from get_now().
 *
 * erts_get_emu_time() is only used internally in the emulator in
 * order to order emulator internal events.
 */

void
erts_get_emu_time(SysTimeval *this_emu_time_p)
{
    erts_smp_mtx_lock(&erts_timeofday_mtx);
    
    get_tolerant_timeofday(this_emu_time_p);

    /* Make sure time is later than last */
    if (last_emu_time.tv_sec > this_emu_time_p->tv_sec ||
	(last_emu_time.tv_sec == this_emu_time_p->tv_sec
	 && last_emu_time.tv_usec >= this_emu_time_p->tv_usec)) {
	*this_emu_time_p = last_emu_time;
	this_emu_time_p->tv_usec++;
    }
    /* Check for carry from above + general reasonability */
    if (this_emu_time_p->tv_usec >= 1000000) {
	this_emu_time_p->tv_usec = 0;
	this_emu_time_p->tv_sec++;
    }

    last_emu_time = *this_emu_time_p;
    
    erts_smp_mtx_unlock(&erts_timeofday_mtx);
}
