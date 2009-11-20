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

int erts_ticks_per_sec = 0; /* Will be SYS_CLK_TCK in erl_unix_sys.h */
int erts_ticks_per_sec_wrap = 0; /* Will be SYS_CLK_TCK_WRAP */
static int ticks_bsr = 0; /* Shift wrapped tick value this much to the right */

/* 
 * init timers, chose a tick length, and return it.
 * Unix is priviliged when it comes to time, as erl_time_sup.c 
 * does almost everything. Other platforms have to
 * emulate Unix in this sense.
 */
int sys_init_time(void)
{
    /* 
     * This (erts_ticks_per_sec) is only for times() (CLK_TCK), 
     * the resolution is always one millisecond..
     */
    if ((erts_ticks_per_sec = TICKS_PER_SEC()) < 0)
	erl_exit(1, "Can't get clock ticks/sec\n");
    if (erts_ticks_per_sec >= 1000) {
	/* Workaround for beta linux kernels, need to be done in runtime
	   to make erlang run on both 2.4 and 2.5 kernels. In the future, 
	   the kernel ticks might as 
	   well be used as a high res timer instead, but that's for when the 
	   majority uses kernels with HZ == 1024 */
	ticks_bsr = 3;
    } else {
	ticks_bsr = 0;
    }
    erts_ticks_per_sec_wrap = (erts_ticks_per_sec >> ticks_bsr);
    return SYS_CLOCK_RESOLUTION;
}

clock_t sys_times_wrap(void)
{
    SysTimes dummy;
    clock_t result = (sys_times(&dummy) >> ticks_bsr);
    return result;
}




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


