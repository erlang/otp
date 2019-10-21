/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2006-2017. All Rights Reserved.
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
#include "config.h"
#endif

#if defined(__WIN32__)
#  include <windows.h>
#endif

#include "ethread_inline.h"
#include "erl_misc_utils.h"

#if defined(__WIN32__)
#elif defined(VXWORKS)
#  include <selectLib.h>
#else /* UNIX */
#  include <stdio.h>
#  include <sys/types.h>
#  include <sys/param.h>
#  include <limits.h>
#  include <dirent.h>
#  include <sys/stat.h>
#  include <fcntl.h>
#  ifdef SYS_SELECT_H
#    include <sys/select.h>
#  endif
#  if TIME_WITH_SYS_TIME
#     include <sys/time.h>
#     include <time.h>
#  else
#     if HAVE_SYS_TIME_H
#         include <sys/time.h>
#     else
#         include <time.h>
#     endif
#  endif
#  include <string.h>
#  ifdef HAVE_UNISTD_H
#    include <unistd.h>
#  endif
#  if defined(_SC_NPROC_CONF) && !defined(_SC_NPROCESSORS_CONF)
#    define _SC_NPROCESSORS_CONF _SC_NPROC_CONF
#  endif
#  if defined(_SC_NPROC_ONLN) && !defined(_SC_NPROCESSORS_ONLN)
#    define _SC_NPROCESSORS_ONLN _SC_NPROC_ONLN
#  endif
#  if (defined(NO_SYSCONF) || !defined(_SC_NPROCESSORS_CONF))
#    ifdef HAVE_SYS_SYSCTL_H
#      include <sys/sysctl.h>
#    endif
#  endif
#endif

#if defined(HAVE_SCHED_xETAFFINITY)
#  include <sched.h>
#  define ERTS_HAVE_MISC_UTIL_AFFINITY_MASK__
#define ERTS_MU_GET_PROC_AFFINITY__(CPUINFOP, CPUSET)			\
     (sched_getaffinity((CPUINFOP)->pid,				\
			sizeof(cpu_set_t),				\
			(CPUSET)) != 0 ? -errno : 0)
#define ERTS_MU_SET_THR_AFFINITY__(SETP)				\
     (sched_setaffinity(0, sizeof(cpu_set_t), (SETP)) != 0 ? -errno : 0)
#elif defined(HAVE_CPUSET_xETAFFINITY)
#  include <sys/param.h>
#  include <sys/cpuset.h>
#  define ERTS_HAVE_MISC_UTIL_AFFINITY_MASK__
#define ERTS_MU_GET_PROC_AFFINITY__(CPUINFOP, CPUSET)			\
     (cpuset_getaffinity(CPU_LEVEL_WHICH, CPU_WHICH_PID, -1, 	    \
			sizeof(cpuset_t),				\
			(CPUSET)) != 0 ? -errno : 0)
#define ERTS_MU_SET_THR_AFFINITY__(CPUSETP)				\
     (cpuset_setaffinity(CPU_LEVEL_WHICH, CPU_WHICH_TID, -1,    \
            sizeof(cpuset_t),               \
            (CPUSETP)) != 0 ? -errno : 0)
#  define cpu_set_t cpuset_t
#elif defined(__WIN32__)
#  define ERTS_HAVE_MISC_UTIL_AFFINITY_MASK__
#  define cpu_set_t DWORD
#  define CPU_SETSIZE (sizeof(DWORD)*8)
#  define CPU_ZERO(SETP) (*(SETP) = (DWORD) 0)
#  define CPU_SET(CPU, SETP) (*(SETP) |= (((DWORD) 1) << (CPU)))
#  define CPU_CLR(CPU, SETP) (*(SETP) &= ~(((DWORD) 1) << (CPU)))
#  define CPU_ISSET(CPU, SETP) ((*(SETP) & (((DWORD) 1) << (CPU))) != (DWORD) 0)
#define ERTS_MU_GET_PROC_AFFINITY__ get_proc_affinity
#define ERTS_MU_SET_THR_AFFINITY__ set_thr_affinity
#endif
#ifdef HAVE_PSET_INFO
#  include <sys/pset.h>
#endif
#ifdef HAVE_PROCESSOR_BIND
#  include <sys/processor.h>
#  include <sys/procset.h>
#endif

#include <stdlib.h>
#ifdef HAVE_LIMITS_H
#include <limits.h>
#endif

#ifdef __linux__
#  define ERTS_SYS_NODE_PATH	"/sys/devices/system/node"
#  define ERTS_SYS_CPU_PATH	"/sys/devices/system/cpu"
#endif

#ifdef __FreeBSD__
#include <sys/types.h>
#include <sys/sysctl.h>
#endif

/* Simplify include for static functions */

#if defined(__linux__) || defined(HAVE_KSTAT) || defined(__WIN32__) || defined(__FreeBSD__)
#  define ERTS_CPU_TOPOLOGY_ENABLED (1)
#endif

static int read_topology(erts_cpu_info_t *cpuinfo);

#if defined(ERTS_HAVE_MISC_UTIL_AFFINITY_MASK__)
static int
cpu_sets_are_eq(cpu_set_t *x, cpu_set_t *y)
{
    int i;
    for (i = 0; i < CPU_SETSIZE; i++) {
	if (CPU_ISSET(i, x)) {
	    if (!CPU_ISSET(i, y))
		return 0;
	}
	else {
	    if (CPU_ISSET(i, y))
		return 0;
	}
    }
    return 1;
}

#endif

int
erts_milli_sleep(long ms)
{
    if (ms > 0) {
#ifdef __WIN32__
	Sleep((DWORD) ms);
#else
	struct timeval tv;
	tv.tv_sec = ms / 1000;
	tv.tv_usec = (ms % 1000) * 1000;
	if (select(0, NULL, NULL, NULL, &tv) < 0)
	    return errno == EINTR ? 1 : -1;
#endif
    }
    return 0;
}

struct erts_cpu_info_t_ {
    int configured;
    int online;
    int available;
    int topology_size;
    erts_cpu_topology_t *topology;
#if defined(ERTS_HAVE_MISC_UTIL_AFFINITY_MASK__)
    char *affinity_str;
    char affinity_str_buf[CPU_SETSIZE/4+2];
    cpu_set_t cpuset;
#if defined(HAVE_SCHED_xETAFFINITY)
    pid_t pid;
#endif
#elif defined(HAVE_PSET_INFO)
    processorid_t *cpuids;
#endif
};

#if defined(__WIN32__)

static ETHR_FORCE_INLINE int
get_proc_affinity(erts_cpu_info_t *cpuinfo, cpu_set_t *cpuset)
{
    DWORD_PTR pamask;
    DWORD_PTR samask;
    if (GetProcessAffinityMask(GetCurrentProcess(), &pamask, &samask)) {
	*cpuset = (cpu_set_t) pamask;
	return 0;
    }
    else {
	*cpuset = (cpu_set_t) 0;
	return -erts_get_last_win_errno();
    }
}

static ETHR_FORCE_INLINE int
set_thr_affinity(cpu_set_t *set)
{
    if (*set == (cpu_set_t) 0)
	return -ENOTSUP;
    if (SetThreadAffinityMask(GetCurrentThread(), *set) == 0)
	return -erts_get_last_win_errno();
    else
	return 0;
}

#endif

erts_cpu_info_t *
erts_cpu_info_create(void)
{
    erts_cpu_info_t *cpuinfo = malloc(sizeof(erts_cpu_info_t));
    if (!cpuinfo)
	return NULL;
#if defined(ERTS_HAVE_MISC_UTIL_AFFINITY_MASK__)
    cpuinfo->affinity_str = NULL;
#if defined(HAVE_SCHED_xETAFFINITY)
    cpuinfo->pid = getpid();
#endif
#elif defined(HAVE_PSET_INFO)
    cpuinfo->cpuids = NULL;
#endif
    cpuinfo->topology_size = 0;
    cpuinfo->topology = NULL;
    cpuinfo->configured = -1;
    cpuinfo->online = -1;
    cpuinfo->available = -1;
    erts_cpu_info_update(cpuinfo);
    return cpuinfo;
}

void
erts_cpu_info_destroy(erts_cpu_info_t *cpuinfo)
{
    if (cpuinfo) {
	cpuinfo->configured = 0;
	cpuinfo->online = 0;
	cpuinfo->available = 0;
#ifdef HAVE_PSET_INFO
	if (cpuinfo->cpuids)
	    free(cpuinfo->cpuids);
#endif
	cpuinfo->topology_size = 0;
	if (cpuinfo->topology) {
	    cpuinfo->topology = NULL;
	    free(cpuinfo->topology);
	}
	free(cpuinfo);
    }
}

int
erts_cpu_info_update(erts_cpu_info_t *cpuinfo)
{
    int changed = 0;
    int configured = 0;
    int online = 0;
    int available = 0;
    erts_cpu_topology_t *old_topology;
    int old_topology_size;
#if defined(ERTS_HAVE_MISC_UTIL_AFFINITY_MASK__)
    cpu_set_t cpuset;
#endif

#ifdef __WIN32__
    {
	int i;
	SYSTEM_INFO sys_info;
	GetSystemInfo(&sys_info);
	configured = (int) sys_info.dwNumberOfProcessors;
	for (i = 0; i < sizeof(DWORD)*8; i++)
	    if (sys_info.dwActiveProcessorMask & (((DWORD) 1) << i))
		online++;
    }
#elif !defined(NO_SYSCONF) && (defined(_SC_NPROCESSORS_CONF) \
			       || defined(_SC_NPROCESSORS_ONLN))
#ifdef _SC_NPROCESSORS_CONF
    configured = (int) sysconf(_SC_NPROCESSORS_CONF);
    if (configured < 0)
	configured = 0;
#endif
#ifdef _SC_NPROCESSORS_ONLN
    online = (int) sysconf(_SC_NPROCESSORS_ONLN);
    if (online < 0)
	online = 0;
#endif
#elif defined(HAVE_SYS_SYSCTL_H) && defined(CTL_HW) && (defined(HW_NCPU) \
							|| defined(HW_AVAILCPU))
    {
	int mib[2];
	size_t len;

#ifdef HW_NCPU
	len = sizeof(int);
	mib[0] = CTL_HW;
	mib[1] = HW_NCPU;
	if (sysctl(&mib[0], 2, &configured, &len, NULL, 0) < 0)
	    configured = 0;
#endif
#ifdef HW_AVAILCPU
	len = sizeof(int);
	mib[0] = CTL_HW;
	mib[1] = HW_AVAILCPU;
	if (sysctl(&mib[0], 2, &online, &len, NULL, 0) < 0)
	    online = 0;
#endif
    }
#endif

    if (online > configured)
	online = configured;

    if (cpuinfo->configured != configured)
	changed = 1;
    if (cpuinfo->online != online)
	changed = 1;

#if defined(ERTS_HAVE_MISC_UTIL_AFFINITY_MASK__)
    if (ERTS_MU_GET_PROC_AFFINITY__(cpuinfo, &cpuset) == 0) {
	if (!changed && !cpu_sets_are_eq(&cpuset, &cpuinfo->cpuset))
	    changed = 1;

	if (!changed)
	    available = cpuinfo->available;
	else {
	    int i, c, cn, si;

	    memcpy((void *) &cpuinfo->cpuset,
		   (void *) &cpuset,
		   sizeof(cpu_set_t));

	    c = cn = 0;
	    si = sizeof(cpuinfo->affinity_str_buf) - 1;
	    cpuinfo->affinity_str_buf[si] = '\0';
	    for (i = 0; i < CPU_SETSIZE; i++) {
		if (CPU_ISSET(i, &cpuinfo->cpuset)) {
		    c |= 1 << cn;
		    available++;
		}
		cn++;
		if (cn == 4) {
		    cpuinfo->affinity_str_buf[--si] = (c < 10
						       ? '0' + c
						       : 'A' + c - 10);
		    c = cn = 0;
		}
	    }
	    if (c)
		cpuinfo->affinity_str_buf[--si] = (c < 10
						   ? '0' + c
						   : 'A' + c - 10);
	    while (cpuinfo->affinity_str_buf[si] == '0')
		si++;
	    cpuinfo->affinity_str = &cpuinfo->affinity_str_buf[si];
	}
    }
#elif defined(HAVE_PSET_INFO)
    {
	processorid_t *cpuids;
	uint_t numcpus = configured;
	cpuids = malloc(sizeof(processorid_t)*numcpus);
	if (cpuids) {
	    if (pset_info(PS_MYID, NULL, &numcpus, &cpuids) == 0)
		available = (int) numcpus;
	    if (available < 0) {
		free(cpuids);
		cpuids = NULL;
		available = 0;
	    }
	}
	if (!cpuids) {
	    if (cpuinfo->cpuids)
		changed = 1;
	}
	else {
	    if (cpuinfo->cpuids)
		changed = 1;
	    if (memcmp((void *) cpuinfo->cpuids,
		       (void *) cpuids,
		       sizeof(processorid_t)*numcpus) != 0)
		changed = 1;

	}
	if (!changed) {
	    if (cpuids)
		free(cpuids);
	}
	else {
	    if (cpuinfo->cpuids)
		free(cpuinfo->cpuids);
	    cpuinfo->cpuids = cpuids;
	}
    }
#endif

    if (available > online)
	available = online;

    if (cpuinfo->available != available)
	changed = 1;

    cpuinfo->configured = configured;
    cpuinfo->online = online;
    cpuinfo->available = available;

    old_topology = cpuinfo->topology;
    old_topology_size = cpuinfo->topology_size;
    cpuinfo->topology = NULL;

    read_topology(cpuinfo);

    if (cpuinfo->topology_size != old_topology_size
	|| (old_topology_size != 0
	    && memcmp((void *) cpuinfo->topology,
		      (void *) old_topology,
		      (sizeof(erts_cpu_topology_t)
		       * old_topology_size)) != 0)) {
	changed = 1;
	if (old_topology)
	    free(old_topology);
    }
    else {
	if (cpuinfo->topology)
	    free(cpuinfo->topology);
	cpuinfo->topology = old_topology;
    }

    return changed;
}

int
erts_get_cpu_configured(erts_cpu_info_t *cpuinfo)
{
    if (!cpuinfo)
	return -EINVAL;
    if (cpuinfo->configured <= 0)
	return -ENOTSUP;
    return cpuinfo->configured;
}

int
erts_get_cpu_online(erts_cpu_info_t *cpuinfo)
{
    if (!cpuinfo)
	return -EINVAL;
    if (cpuinfo->online <= 0)
	return -ENOTSUP;
    return cpuinfo->online;
}

int
erts_get_cpu_available(erts_cpu_info_t *cpuinfo)
{
    if (!cpuinfo)
	return -EINVAL;
    if (cpuinfo->available <= 0)
	return -ENOTSUP;
    return cpuinfo->available;
}

char *
erts_get_unbind_from_cpu_str(erts_cpu_info_t *cpuinfo)
{
#if defined(ERTS_HAVE_MISC_UTIL_AFFINITY_MASK__)
    if (!cpuinfo)
	return "false";
    return cpuinfo->affinity_str;
#else
    return "true";
#endif
}

int
erts_get_available_cpu(erts_cpu_info_t *cpuinfo, int no)
{
    if (!cpuinfo || no < 1 || cpuinfo->available < no)
	return -EINVAL;
#if defined(ERTS_HAVE_MISC_UTIL_AFFINITY_MASK__)
    {
	cpu_set_t *allowed = &cpuinfo->cpuset;
	int ix, n;
	for (ix = 0, n = 1; ix < CPU_SETSIZE; ix++) {
	    if (CPU_ISSET(ix, allowed)) {
		if (no == n)
		    return ix;
		n++;
	    }
	}
    }
    return -EINVAL;
#elif defined(HAVE_PROCESSOR_BIND)
#if defined(HAVE_PSET_INFO)
    return (int) cpuinfo->cpuids[no-1];
#elif defined(HAVE_KSTAT)
    if (cpuinfo->topology && cpuinfo->online <= no) {
	/* May not be available, but this is the best we can do */
	return cpuinfo->topology[no-1].logical;
    }
    return -EINVAL;
#endif
#else
    return -ENOTSUP;
#endif
}

int
erts_is_cpu_available(erts_cpu_info_t *cpuinfo, int id)
{
    if (cpuinfo && 0 <= id) {
#if defined(ERTS_HAVE_MISC_UTIL_AFFINITY_MASK__)
	if (id < CPU_SETSIZE)
	    return CPU_ISSET(id, &cpuinfo->cpuset);
#elif defined(HAVE_PROCESSOR_BIND)
	int no;
#if defined(HAVE_PSET_INFO)
	for (no = 0; no < cpuinfo->available; no++)
	    if (id == (int) cpuinfo->cpuids[no])
		return 1;
#elif defined(HAVE_KSTAT)
	if (cpuinfo->topology) {
	    for (no = 0; no < cpuinfo->online; no++) {
		if (id == (int) cpuinfo->topology[no].logical) {
		    /* May not be available, but this is the best we can do... */
		    return 1;
		}
	    }
	}
#endif
#endif
    }
    return 0;
}

int
erts_get_cpu_topology_size(erts_cpu_info_t *cpuinfo)
{
    return cpuinfo->topology_size;
}

int
erts_get_cpu_topology(erts_cpu_info_t *cpuinfo,
		      erts_cpu_topology_t *topology)
{
    if (!cpuinfo->topology)
	return 0;
    memcpy((void *) topology,
	   (void *) cpuinfo->topology,
	   cpuinfo->topology_size*sizeof(erts_cpu_topology_t));
    return cpuinfo->topology_size;
}

int
erts_bind_to_cpu(erts_cpu_info_t *cpuinfo, int cpu)
{
    /*
     * Caller can test for available functionality by
     * passing a negative cpu id. If functionality is
     * available -EINVAL is returned; otherwise,
     * -ENOTSUP.
     */
    if (!cpuinfo)
	return -EINVAL;
#if defined(ERTS_HAVE_MISC_UTIL_AFFINITY_MASK__)
    {
	cpu_set_t bind_set;
	if (cpu < 0)
	    return -EINVAL;
	if (!CPU_ISSET(cpu, &cpuinfo->cpuset))
	    return -EINVAL;

	CPU_ZERO(&bind_set);
	CPU_SET(cpu, &bind_set);
	return ERTS_MU_SET_THR_AFFINITY__(&bind_set);
    }
#elif defined(HAVE_PROCESSOR_BIND)
    if (cpu < 0)
	return -EINVAL;
    if (processor_bind(P_LWPID, P_MYID, (processorid_t) cpu, NULL) != 0)
	return -errno;
    return 0;
#else
    return -ENOTSUP;
#endif
}

int
erts_unbind_from_cpu(erts_cpu_info_t *cpuinfo)
{
    if (!cpuinfo)
	return -EINVAL;
#if defined(ERTS_HAVE_MISC_UTIL_AFFINITY_MASK__)
    return ERTS_MU_SET_THR_AFFINITY__(&cpuinfo->cpuset);
#elif defined(HAVE_PROCESSOR_BIND)
    if (processor_bind(P_LWPID, P_MYID, PBIND_NONE, NULL) != 0)
	return -errno;
    return 0;
#else
    return -ENOTSUP;
#endif
}

int
erts_unbind_from_cpu_str(char *str)
{
#if defined(ERTS_HAVE_MISC_UTIL_AFFINITY_MASK__)
    char *c = str;
    int cpus = 0;
    int shft = 0;
    cpu_set_t cpuset;

    CPU_ZERO(&cpuset);

    if (!c)
	return -EINVAL;

    while (*c)
	c++;

    while (c != str) {
	int shft2;
	int mask = 0;
	c--;
	switch (*c) {
	case '0': mask = 0; break;
	case '1': mask = 1; break;
	case '2': mask = 2; break;
	case '3': mask = 3; break;
	case '4': mask = 4; break;
	case '5': mask = 5; break;
	case '6': mask = 6; break;
	case '7': mask = 7; break;
	case '8': mask = 8; break;
	case '9': mask = 9; break;
	case 'A': case 'a': mask = 10; break;
	case 'B': case 'b': mask = 11; break;
	case 'C': case 'c': mask = 12; break;
	case 'D': case 'd': mask = 13; break;
	case 'E': case 'e': mask = 14; break;
	case 'F': case 'f': mask = 15; break;
	default: return -EINVAL;
	}
	for (shft2 = 0; shft2 < 4; shft2++) {
	    if (mask & (1 << shft2)) {
		int cpu = shft + shft2;
		if (cpu >= CPU_SETSIZE)
		    return -EINVAL;
		cpus++;
		CPU_SET(cpu, &cpuset);
	    }
	}
	shft += 4;
    }

    if (!cpus)
	return -EINVAL;

    return ERTS_MU_SET_THR_AFFINITY__(&cpuset);
#elif defined(HAVE_PROCESSOR_BIND)
    if (processor_bind(P_LWPID, P_MYID, PBIND_NONE, NULL) != 0)
	return -errno;
    return 0;
#else
    return -ENOTSUP;
#endif
}


#if defined(ERTS_CPU_TOPOLOGY_ENABLED)
static int
pn_cmp(const void *vx, const void *vy)
{
    erts_cpu_topology_t *x = (erts_cpu_topology_t *) vx;
    erts_cpu_topology_t *y = (erts_cpu_topology_t *) vy;

    if (x->processor != y->processor)
	return x->processor - y->processor;
    if (x->node != y->node)
	return x->node - y->node;
    if (x->processor_node != y->processor_node)
	return x->processor_node - y->processor_node;
    if (x->core != y->core)
	return x->core - y->core;
    if (x->thread != y->thread)
	return x->thread - y->thread;
    if (x->logical != y->logical)
	return x->logical - y->logical;
    return 0;
}

static int
cpu_cmp(const void *vx, const void *vy)
{
    erts_cpu_topology_t *x = (erts_cpu_topology_t *) vx;
    erts_cpu_topology_t *y = (erts_cpu_topology_t *) vy;

    if (x->node != y->node)
	return x->node - y->node;
    if (x->processor != y->processor)
	return x->processor - y->processor;
    if (x->processor_node != y->processor_node)
	return x->processor_node - y->processor_node;
    if (x->core != y->core)
	return x->core - y->core;
    if (x->thread != y->thread)
	return x->thread - y->thread;
    if (x->logical != y->logical)
	return x->logical - y->logical;
    return 0;
}

static void
adjust_processor_nodes(erts_cpu_info_t *cpuinfo, int no_nodes)
{
    erts_cpu_topology_t *prev, *this, *last;
    if (no_nodes > 1) {
	int processor = -1;
	int processor_node = 0;
	int node = -1;

	qsort(cpuinfo->topology,
	      cpuinfo->topology_size,
	      sizeof(erts_cpu_topology_t),
	      pn_cmp);

	prev = NULL;
	this = &cpuinfo->topology[0];
	last = &cpuinfo->topology[cpuinfo->topology_size-1];
	while (1) {
	    if (processor == this->processor) {
		if (node != this->node)
		    processor_node = 1;
	    }
	    else {
		if (processor_node) {
		make_processor_node:
		    while (prev->processor == processor) {
			prev->processor_node = prev->node;
			prev->node = -1;
			if (prev == &cpuinfo->topology[0])
			    break;
			prev--;
		    }
		    processor_node = 0;
		}
		processor = this->processor;
		node = this->node;
	    }
	    if (this == last) {
		if (processor_node) {
		    prev = this;
		    goto make_processor_node;
		}
		break;
	    }
	    prev = this++;
	}
    }
}
#endif


#ifdef __linux__

static int
read_file(char *path, char *buf, int size)
{
    int ix = 0;
    ssize_t sz = size-1;
    int fd = open(path, O_RDONLY);
    if (fd < 0)
	goto error;
    while (size > ix) {
	sz = read(fd, &buf[ix], size - ix);
	if (sz <= 0) {
	    if (sz == 0)
		break;
	    if (errno == EINTR)
		continue;
	    goto error;
	}
	ix += sz;
    }
    buf[ix] = '\0';
    close(fd);
    return ix;

 error: {
	int saved_errno = errno;
	if (fd >= 0)
	    close(fd);
	if (saved_errno)
	    return -saved_errno;
	else
	    return -EINVAL;
    }
}

/* Macro to convert in int to a string */
#define STR_INDIR(x) #x
#define STR(x) STR_INDIR(x)

static int
read_topology(erts_cpu_info_t *cpuinfo)
{
    /* Need to fit all of the path in these buffers... */
    char npath[MAXPATHLEN];
    char cpath[MAXPATHLEN];
    char tpath[MAXPATHLEN+5+30];
    char fpath[MAXPATHLEN];
    DIR *ndir = NULL;
    DIR *cdir = NULL;
    struct dirent *nde;
    int ix;
    int res = 0;
    int got_nodes = 0;
    int no_nodes = 0;

    errno = 0;

    if (cpuinfo->configured < 1)
	goto error;

    cpuinfo->topology = malloc(sizeof(erts_cpu_topology_t)
			       * cpuinfo->configured);
    if (!cpuinfo->topology)
	goto error;

    for (ix = 0; ix < cpuinfo->configured; ix++) {
	cpuinfo->topology[ix].node = -1;
	cpuinfo->topology[ix].processor = -1;
	cpuinfo->topology[ix].processor_node = -1;
	cpuinfo->topology[ix].core = -1;
	cpuinfo->topology[ix].thread = -1;
	cpuinfo->topology[ix].logical = -1;
    }

    ix = 0;

    if (realpath(ERTS_SYS_NODE_PATH, npath)) {
	ndir = opendir(npath);
	got_nodes = (ndir != NULL);
    }

    do {
	int node_id = -1;

	if (!got_nodes) {
	    if (!realpath(ERTS_SYS_CPU_PATH, cpath))
		goto error;
	}
	else {

	    nde = readdir(ndir);

	    if (!nde)
		break;

	    if (sscanf(nde->d_name, "node%d", &node_id) != 1)
		continue;

	    no_nodes++;

	    sprintf(tpath, "%." STR(MAXPATHLEN) "s/node%d", npath, node_id);

	    if (!realpath(tpath, cpath))
		goto error;
	}

	cdir = opendir(cpath);
	if (!cdir)
	    goto error;

	while (1) {
	    int cpu_id;
	    struct dirent *cde = readdir(cdir);
	    if (!cde) {
		closedir(cdir);
		cdir = NULL;
		break;
	    }

	    if (sscanf(cde->d_name, "cpu%d", &cpu_id) == 1) {
		char buf[50]; /* Much more than enough for an integer */
		int processor_id, core_id;
		sprintf(tpath, "%." STR(MAXPATHLEN) "s/cpu%d/topology/physical_package_id",
			cpath, cpu_id);
		if (!realpath(tpath, fpath))
		    continue;
		if (read_file(fpath, buf, sizeof(buf)) <= 0)
		    continue;
		if (sscanf(buf, "%d", &processor_id) != 1)
		    continue;
		sprintf(tpath, "%." STR(MAXPATHLEN) "s/cpu%d/topology/core_id",
			cpath, cpu_id);
		if (!realpath(tpath, fpath))
		    continue;
		if (read_file(fpath, buf, sizeof(buf)) <= 0)
		    continue;
		if (sscanf(buf, "%d", &core_id) != 1)
		    continue;

                /*
                 * The number of CPUs that proc fs presents is greater
                 * then the number of CPUs configured in sysconf.
                 * This has been known to happen in docker. When this
                 * happens we refuse to give a CPU topology.
                 */
                if (ix >= cpuinfo->configured)
                    goto error;

		/*
		 * We now know node id, processor id, and
		 * core id of the logical processor with
		 * the cpu id 'cpu_id'.
		 */
		cpuinfo->topology[ix].node	= node_id;
		cpuinfo->topology[ix].processor	= processor_id;
		cpuinfo->topology[ix].processor_node = -1; /* Fixed later */
		cpuinfo->topology[ix].core	= core_id;
		cpuinfo->topology[ix].thread	= 0; /* we'll numerate later */
		cpuinfo->topology[ix].logical	= cpu_id;
		ix++;

	    }
	}
    } while (got_nodes);

    res = ix;

    if (!res || res < cpuinfo->online)
	res = 0;
    else {
	erts_cpu_topology_t *prev, *this, *last;

	cpuinfo->topology_size = res;

	if (cpuinfo->topology_size != cpuinfo->configured) {
	    void *t = realloc(cpuinfo->topology, (sizeof(erts_cpu_topology_t)
						  * cpuinfo->topology_size));
	    if (t)
		cpuinfo->topology = t;
	}

	adjust_processor_nodes(cpuinfo, no_nodes);

	qsort(cpuinfo->topology,
	      cpuinfo->topology_size,
	      sizeof(erts_cpu_topology_t),
	      cpu_cmp);

	this = &cpuinfo->topology[0];
	this->thread = 0;

	if (res > 1) {
	    prev = this++;
	    last = &cpuinfo->topology[cpuinfo->topology_size-1];

	    while (1) {
		this->thread = ((this->node == prev->node
				 && this->processor == prev->processor
				 && this->processor_node == prev->processor_node
				 && this->core == prev->core)
				? prev->thread + 1
				: 0);
		if (this == last)
		    break;
		prev = this++;
	    }
	}
    }

 error:

    if (res == 0) {
	cpuinfo->topology_size = 0;
	if (cpuinfo->topology) {
	    free(cpuinfo->topology);
	    cpuinfo->topology = NULL;
	}
	if (errno)
	    res = -errno;
	else
	    res = -EINVAL;
    }

    if (ndir)
	closedir(ndir);
    if (cdir)
	closedir(cdir);

    return res;
}

#elif defined(HAVE_KSTAT) /* SunOS kstat */

#include <kstat.h>

static int
data_lookup_int(kstat_t *ks, char *what)
{
    int res;
    kstat_named_t *ks_n;

    ks_n = kstat_data_lookup(ks, what);
    if (!ks_n)
	return 0;

    switch (ks_n->data_type) {
    case KSTAT_DATA_CHAR:
	res = atoi(ks_n->value.c);
	break;
    case KSTAT_DATA_INT32:
	res = (int) ks_n->value.i32;
	break;
    case KSTAT_DATA_UINT32:
	res = (int) ks_n->value.ui32;
	break;
    case KSTAT_DATA_INT64:
	res = (int) ks_n->value.i64;
	break;
    case KSTAT_DATA_UINT64:
	res = (int) ks_n->value.ui64;
	break;
    default:
	res = 0;
	break;
    }
    return res;
}

static int
read_topology(erts_cpu_info_t *cpuinfo)
{
    int res = 0;
    int ix;
    kstat_ctl_t *ks_ctl;
    kstat_t *ks;

    errno = 0;

    if (cpuinfo->configured < 1)
	goto error;

    cpuinfo->topology = malloc(sizeof(erts_cpu_topology_t)
			       * cpuinfo->configured);
    if (!cpuinfo->topology)
	goto error;

    for (ix = 0; ix < cpuinfo->configured; ix++) {
	cpuinfo->topology[ix].node = -1;
	cpuinfo->topology[ix].processor = -1;
	cpuinfo->topology[ix].processor_node = -1;
	cpuinfo->topology[ix].core = -1;
	cpuinfo->topology[ix].thread = -1;
	cpuinfo->topology[ix].logical = -1;
    }

    ks_ctl = kstat_open();
    if (!ks_ctl)
	goto error;

    ix = 0;
    for (ks = ks_ctl->kc_chain; ks; ks = ks->ks_next) {
	if (strcmp("cpu_info", ks->ks_module) == 0) {
	    kstat_read(ks_ctl, ks, NULL);
	    if (ks->ks_type == KSTAT_TYPE_NAMED) {
		/*
		 * Don't know how to figure numa nodes out;
		 * hope there is none...
		 */
		cpuinfo->topology[ix].node = -1;
		cpuinfo->topology[ix].processor = data_lookup_int(ks,"chip_id");
		cpuinfo->topology[ix].processor_node = -1;
		cpuinfo->topology[ix].core = data_lookup_int(ks, "core_id");
		cpuinfo->topology[ix].thread = 0; /* we'll numerate later */
		cpuinfo->topology[ix].logical = ks->ks_instance;
		if (++ix == cpuinfo->configured)
		    break;
	    }
	}
    }

    kstat_close(ks_ctl);

    res = ix;

    if (!res || res < cpuinfo->online)
	res = 0;
    else {
	erts_cpu_topology_t *prev, *this, *last;

	cpuinfo->topology_size = res;

	if (cpuinfo->topology_size != cpuinfo->configured) {
	    void *t = realloc(cpuinfo->topology, (sizeof(erts_cpu_topology_t)
						  * cpuinfo->topology_size));
	    if (t)
		cpuinfo->topology = t;
	}

	qsort(cpuinfo->topology,
	      cpuinfo->topology_size,
	      sizeof(erts_cpu_topology_t),
	      cpu_cmp);

	this = &cpuinfo->topology[0];
	this->thread = 0;

	if (res > 1) {
	    prev = this++;
	    last = &cpuinfo->topology[cpuinfo->topology_size-1];

	    while (1) {
		this->thread = ((this->node == prev->node
				 && this->processor == prev->processor
				 && this->processor_node == prev->processor_node
				 && this->core == prev->core)
				? prev->thread + 1
				: 0);
		if (this == last)
		    break;
		prev = this++;
	    }
	}
    }

    adjust_processor_nodes(cpuinfo, 1);

 error:

    if (res == 0) {
	cpuinfo->topology_size = 0;
	if (cpuinfo->topology) {
	    free(cpuinfo->topology);
	    cpuinfo->topology = NULL;
	}
	if (errno)
	    res = -errno;
	else
	    res = -EINVAL;
    }

    return res;

}

#elif defined(__WIN32__)

/*
 * We cannot use Relation* out of the box since all of them are not
 * always part of the LOGICAL_PROCESSOR_RELATIONSHIP enum. They are
 * however documented as follows...
 */
#define ERTS_MU_RELATION_PROCESSOR_CORE       0 /* RelationProcessorCore */
#define ERTS_MU_RELATION_NUMA_NODE            1 /* RelationNumaNode */
#define ERTS_MU_RELATION_CACHE                2 /* RelationCache */
#define ERTS_MU_RELATION_PROCESSOR_PACKAGE    3 /* RelationProcessorPackage */

static ETHR_FORCE_INLINE int
rel_cmp_val(int r)
{
    switch (r) {
    case ERTS_MU_RELATION_NUMA_NODE:         return 0;
    case ERTS_MU_RELATION_PROCESSOR_PACKAGE: return 1;
    case ERTS_MU_RELATION_PROCESSOR_CORE:    return 2;
    default: /* currently not used */        return 3;
    }
}

static int
slpi_cmp(const void *vx, const void *vy)
{
    PSYSTEM_LOGICAL_PROCESSOR_INFORMATION x, y;
    x = (PSYSTEM_LOGICAL_PROCESSOR_INFORMATION) vx;
    y = (PSYSTEM_LOGICAL_PROCESSOR_INFORMATION) vy;

    if ((int) x->Relationship != (int) y->Relationship)
	return (rel_cmp_val((int) x->Relationship)
		- rel_cmp_val((int) y->Relationship));

    switch ((int) x->Relationship) {
    case ERTS_MU_RELATION_NUMA_NODE:
	if (x->NumaNode.NodeNumber == y->NumaNode.NodeNumber)
	    break;
	return ((int) x->NumaNode.NodeNumber) - ((int) y->NumaNode.NodeNumber);
    case ERTS_MU_RELATION_PROCESSOR_CORE:
    case ERTS_MU_RELATION_PROCESSOR_PACKAGE:
    default:
	break;
    }

    if (x->ProcessorMask == y->ProcessorMask)
	return 0;
    return x->ProcessorMask < y->ProcessorMask ? -1 : 1;
}

typedef BOOL (WINAPI *glpi_t)(PSYSTEM_LOGICAL_PROCESSOR_INFORMATION, PDWORD);

static int
read_topology(erts_cpu_info_t *cpuinfo)
{
    int res = 0;
    glpi_t glpi;
    int *core_id = NULL;
    PSYSTEM_LOGICAL_PROCESSOR_INFORMATION slpip = NULL;
    int wix, rix, max_l, l, packages, nodes, no_slpi;
    DWORD slpi_size = 0;


    glpi = (glpi_t) GetProcAddress(GetModuleHandle("kernel32"),
				   "GetLogicalProcessorInformation");
    if (!glpi)
	return -ENOTSUP;

    cpuinfo->topology = NULL;

    if (cpuinfo->configured < 1 || sizeof(ULONG_PTR)*8 < cpuinfo->configured)
	goto error;

    while (1) {
	DWORD werr;
	if (TRUE == glpi(slpip, &slpi_size))
	    break;
	werr = GetLastError();
	if (werr != ERROR_INSUFFICIENT_BUFFER) {
	    res = -erts_map_win_error_to_errno(werr);
	    goto error;
	}
	if (slpip)
	    free(slpip);
	slpip = malloc(slpi_size);
	if (!slpip) {
	    res = -ENOMEM;
	    goto error;
	}
    }

    no_slpi = (int) slpi_size/sizeof(SYSTEM_LOGICAL_PROCESSOR_INFORMATION);

    qsort(slpip,
	  no_slpi,
	  sizeof(SYSTEM_LOGICAL_PROCESSOR_INFORMATION),
	  slpi_cmp);

    /*
     * Now numa node relations appear before package relations which
     * appear before core relations which appear before relations
     * we aren't interested in...
     */

    max_l = 0;
    packages = 0;
    nodes = 0;
    for (rix = 0; rix < no_slpi; rix++) {
	PSYSTEM_LOGICAL_PROCESSOR_INFORMATION this = &slpip[rix];
	for (l = sizeof(ULONG_PTR)*8 - 1; l > 0; l--) {
	    if (slpip[rix].ProcessorMask & (((ULONG_PTR) 1) << l)) {
		if (max_l < l)
		    max_l = l;
		break;
	    }
	}
	if ((int) slpip[rix].Relationship == ERTS_MU_RELATION_PROCESSOR_PACKAGE)
	    packages++;
	if ((int) slpip[rix].Relationship == ERTS_MU_RELATION_NUMA_NODE)
	    nodes++;
    }

    if (!packages) {
      packages = 1;
    }
    core_id = malloc(sizeof(int)*packages);
    if (!core_id) {
	res = -ENOMEM;
	goto error;
    }

    for (rix = 0; rix < packages; rix++)
	core_id[rix] = 0;

    cpuinfo->topology_size = max_l + 1;
    cpuinfo->topology = malloc(sizeof(erts_cpu_topology_t)
			       * cpuinfo->topology_size);
    if (!cpuinfo->topology) {
	res = -ENOMEM;
	goto error;
    }

    for (wix = 0; wix < cpuinfo->topology_size; wix++) {
	cpuinfo->topology[wix].node = -1;
	cpuinfo->topology[wix].processor = -1;
	cpuinfo->topology[wix].processor_node = -1;
	cpuinfo->topology[wix].core = -1;
	cpuinfo->topology[wix].thread = -1;
	cpuinfo->topology[wix].logical = -1;
    }

    nodes = 0;
    packages = 0;

    for (rix = 0; rix < no_slpi; rix++) {

        switch ((int) slpip[rix].Relationship) {
        case ERTS_MU_RELATION_NUMA_NODE:
	    for (l = 0; l < sizeof(ULONG_PTR)*8; l++) {
		if (slpip[rix].ProcessorMask & (((ULONG_PTR) 1) << l)) {
		    cpuinfo->topology[l].logical = l;
		    cpuinfo->topology[l].node = slpip[rix].NumaNode.NodeNumber;
		}
	    }
	    nodes++;
            break;
        case ERTS_MU_RELATION_PROCESSOR_PACKAGE:
	    for (l = 0; l < sizeof(ULONG_PTR)*8; l++) {
		if (slpip[rix].ProcessorMask & (((ULONG_PTR) 1) << l)) {
		    cpuinfo->topology[l].logical = l;
		    cpuinfo->topology[l].processor = packages;
		}
	    }
	    packages++;
            break;
        case ERTS_MU_RELATION_PROCESSOR_CORE: {
	    int thread = 0;
	    int processor = -1;
	    for (l = 0; l < sizeof(ULONG_PTR)*8; l++) {
		/*
		 * Nodes and packages may not be supported; pretend
		 * that there are one if this is the case...
		 */
		if (slpip[rix].ProcessorMask & (((ULONG_PTR) 1) << l)) {
		    if (!nodes) {
		      cpuinfo->topology[l].node = 0;
		    }
		    if (!packages) {
		      cpuinfo->topology[l].processor = 0;
		    }
		    if (processor < 0) {
			processor = cpuinfo->topology[l].processor;
			if (processor < 0) {
			    res = -EINVAL;
			    goto error;
			}
		    }
		    else if (processor != cpuinfo->topology[l].processor) {
			res = -EINVAL;
			goto error;
		    }
		    cpuinfo->topology[l].logical = l;
		    cpuinfo->topology[l].thread = thread;
		    cpuinfo->topology[l].core = core_id[processor];
		    thread++;
		}
	    }
	    core_id[processor]++;
            break;
	}
        default:
	    /*
	     * We have reached the end of the relationships
	     * that we (currently) are interested in...
	     */
	    goto relationships_done;
        }
    }

 relationships_done:

    /*
     * There may be unused entries; remove them...
     */
    for (rix = wix = 0; rix < cpuinfo->topology_size; rix++) {
	if (cpuinfo->topology[rix].logical >= 0) {
	    if (wix != rix)
		cpuinfo->topology[wix] = cpuinfo->topology[rix];
	    wix++;
	}
    }

    if (cpuinfo->topology_size != wix) {
	erts_cpu_topology_t *new = cpuinfo->topology;
	new = realloc(cpuinfo->topology,
		      sizeof(erts_cpu_topology_t)*wix);
	if (!new) {
	    res = -ENOMEM;
	    goto error;
	}
	cpuinfo->topology = new;
	cpuinfo->topology_size = wix;
    }

    res = wix;

    adjust_processor_nodes(cpuinfo, nodes);

    qsort(cpuinfo->topology,
	  cpuinfo->topology_size,
	  sizeof(erts_cpu_topology_t),
	  cpu_cmp);

    if (res < cpuinfo->online)
	res = -EINVAL;

 error:

    if (res <= 0) {
	cpuinfo->topology_size = 0;
	if (cpuinfo->topology) {
	    free(cpuinfo->topology);
	    cpuinfo->topology = NULL;
	}
    }

    if (slpip)
	free(slpip);
    if (core_id)
	free(core_id);

    return res;
}

#elif defined(__FreeBSD__)

/**
 * FreeBSD topology detection is based on kern.sched.topology_spec XML as
 * exposed by the ULE scheduler and described in SMP(4). It is available in
 * 8.0 and higher.
 *
 * Threads are identified in this XML chunk with a THREAD flag. The function
 * (simplistically) distinguishes cores and processors by the amount of cache
 * they share (0 => processor, otherwise => core). Nodes are not identified
 * (ULE doesn't handle NUMA yet, I believe).
 */

/**
 * Recursively parse a topology_spec <group> tag.
 */
static
const char* parse_topology_spec_group(erts_cpu_info_t *cpuinfo, const char* xml, int parentCacheLevel, int* processor_p, int* core_p, int* index_procs_p) {
    int error = 0;
    int cacheLevel = parentCacheLevel;
    const char* next_group_start = strstr(xml + 1, "<group");
    int is_thread_group = 0;
    const char* next_cache_level;
    const char* next_thread_flag;
    const char* next_group_end;
    const char* next_children;
    const char* next_children_end;

    /* parse the cache level */
    next_cache_level = strstr(xml, "cache-level=\"");
    if (next_cache_level && (next_group_start == NULL || next_cache_level < next_group_start)) {
	sscanf(next_cache_level, "cache-level=\"%i\"", &cacheLevel);
    }

    /* parse the threads flag */
    next_thread_flag = strstr(xml, "THREAD");
    if (next_thread_flag && (next_group_start == NULL || next_thread_flag < next_group_start))
	is_thread_group = 1;

    /* Determine if it's a leaf with the position of the next children tag */
    next_group_end = strstr(xml, "</group>");
    next_children = strstr(xml, "<children>");
    next_children_end = strstr(xml, "</children>");
    if (next_children == NULL || next_group_end < next_children) {
	do {
	    const char* next_cpu_start;
	    const char* next_cpu_cdata;
	    const char* next_cpu_end;
	    int cpu_str_size;
	    char* cpu_str;
	    char* cpu_crsr;
	    char* brkb;
	    int thread = 0;
	    int index_procs = *index_procs_p;

	    next_cpu_start = strstr(xml, "<cpu");
	    if (!next_cpu_start) {
		error = 1;
		break;
	    }
	    next_cpu_cdata = strstr(next_cpu_start, ">") + 1;
	    if (!next_cpu_cdata) {
		error = 1;
		break;
	    }
	    next_cpu_end = strstr(next_cpu_cdata, "</cpu>");
	    if (!next_cpu_end) {
		error = 1;
		break;
	    }
	    cpu_str_size = next_cpu_end - next_cpu_cdata;
	    cpu_str = (char*) malloc(cpu_str_size + 1);
	    memcpy(cpu_str, (const char*) next_cpu_cdata, cpu_str_size);
	    cpu_str[cpu_str_size] = 0;
	    for (cpu_crsr = strtok_r(cpu_str, " \t,", &brkb); cpu_crsr; cpu_crsr = strtok_r(NULL, " \t,", &brkb)) {
		int cpu_id;
		if (index_procs >= cpuinfo->configured) {
		    void* t = realloc(cpuinfo->topology, (sizeof(erts_cpu_topology_t) * (index_procs + 1)));
		    if (t) {
			cpuinfo->topology = t;
		    } else {
			error = 1;
			break;
		    }
		}
		cpu_id = atoi(cpu_crsr);
		cpuinfo->topology[index_procs].node = -1;
		cpuinfo->topology[index_procs].processor = *processor_p;
		cpuinfo->topology[index_procs].processor_node = -1;
		cpuinfo->topology[index_procs].core = *core_p;
		cpuinfo->topology[index_procs].thread = thread;
		cpuinfo->topology[index_procs].logical = cpu_id;
		if (is_thread_group) {
		    thread++;
		} else {
		    *core_p = (*core_p) + 1;
		}
		index_procs++;
	    }
	    *index_procs_p = index_procs;
	    free(cpu_str);
	} while (0);
	xml = next_group_end;
    } else {
	while (next_group_start != NULL && next_group_start < next_children_end) {
	    xml = parse_topology_spec_group(cpuinfo, next_group_start, cacheLevel, processor_p, core_p, index_procs_p);
	    if (!xml)
		break;
	    next_group_start = strstr(xml, "<group");
	    next_children_end = strstr(xml, "</children>");
	}
    }

    if (parentCacheLevel == 0) {
	*core_p = 0;
	*processor_p = (*processor_p) + 1;
    } else {
	*core_p = (*core_p) + 1;
    }

    if (error)
	xml = NULL;

    return xml;
}

/**
 * Parse the topology_spec. Return the number of CPUs or 0 if parsing failed.
 */
static
int parse_topology_spec(erts_cpu_info_t *cpuinfo, const char* xml) {
    int res = 1;
    int index_procs = 0;
    int core = 0;
    int processor = 0;
    xml = strstr(xml, "<groups");
    if (!xml)
	return -1;

    xml += 7;
    xml = strstr(xml, "<group");
    while (xml) {
	xml = parse_topology_spec_group(cpuinfo, xml, 0, &processor, &core, &index_procs);
	if (!xml) {
	    res = 0;
	    break;
	}
	xml = strstr(xml, "<group");
    }

    if (res)
	res = index_procs;

    return res;
}

static int
read_topology(erts_cpu_info_t *cpuinfo)
{
    int ix;
    int res = 0;
    size_t topology_spec_size = 0;
    void* topology_spec = NULL;

    errno = 0;

    if (cpuinfo->configured < 1)
	goto error;

    cpuinfo->topology_size = cpuinfo->configured;
    cpuinfo->topology = malloc(sizeof(erts_cpu_topology_t)
			       * cpuinfo->configured);
    if (!cpuinfo->topology) {
	res = -ENOMEM;
	goto error;
    }

    for (ix = 0; ix < cpuinfo->configured; ix++) {
	cpuinfo->topology[ix].node = -1;
	cpuinfo->topology[ix].processor = -1;
	cpuinfo->topology[ix].processor_node = -1;
	cpuinfo->topology[ix].core = -1;
	cpuinfo->topology[ix].thread = -1;
	cpuinfo->topology[ix].logical = -1;
    }

    if (!sysctlbyname("kern.sched.topology_spec", NULL, &topology_spec_size, NULL, 0)) {
	topology_spec = malloc(topology_spec_size);
	if (!topology_spec) {
	    res = -ENOMEM;
	    goto error;
	}

	if (sysctlbyname("kern.sched.topology_spec", topology_spec, &topology_spec_size, NULL, 0)) {
	    goto error;
	}

	res = parse_topology_spec(cpuinfo, topology_spec);
	if (!res || res < cpuinfo->online)
	    res = 0;
	else {
	    cpuinfo->topology_size = res;

	    if (cpuinfo->topology_size != cpuinfo->configured) {
		void *t = realloc(cpuinfo->topology, (sizeof(erts_cpu_topology_t)
						  * cpuinfo->topology_size));
		if (t)
		    cpuinfo->topology = t;
	    }

	    adjust_processor_nodes(cpuinfo, 1);

	    qsort(cpuinfo->topology,
	        cpuinfo->topology_size,
	        sizeof(erts_cpu_topology_t),
	        cpu_cmp);
	}
    }

error:

    if (res == 0) {
	cpuinfo->topology_size = 0;
	if (cpuinfo->topology) {
	    free(cpuinfo->topology);
	    cpuinfo->topology = NULL;
	}
	if (errno)
	    res = -errno;
	else
	    res = -EINVAL;
    }

    if (topology_spec)
	free(topology_spec);

    return res;
}

#else

static int
read_topology(erts_cpu_info_t *cpuinfo)
{
    return -ENOTSUP;
}

#endif

#if defined(__WIN32__)

int
erts_map_win_error_to_errno(DWORD win_error)
{
    switch (win_error) {
    case ERROR_INVALID_FUNCTION:		return EINVAL;	/* 1	*/
    case ERROR_FILE_NOT_FOUND:			return ENOENT;	/* 2	*/
    case ERROR_PATH_NOT_FOUND:			return ENOENT;	/* 3	*/
    case ERROR_TOO_MANY_OPEN_FILES:		return EMFILE;	/* 4	*/
    case ERROR_ACCESS_DENIED:			return EACCES;	/* 5	*/
    case ERROR_INVALID_HANDLE:			return EBADF;	/* 6	*/
    case ERROR_ARENA_TRASHED:			return ENOMEM;	/* 7	*/
    case ERROR_NOT_ENOUGH_MEMORY:		return ENOMEM;	/* 8	*/
    case ERROR_INVALID_BLOCK:			return ENOMEM;	/* 9	*/
    case ERROR_BAD_ENVIRONMENT:			return E2BIG;	/* 10	*/
    case ERROR_BAD_FORMAT:			return ENOEXEC;	/* 11	*/
    case ERROR_INVALID_ACCESS:			return EINVAL;	/* 12	*/
    case ERROR_INVALID_DATA:			return EINVAL;	/* 13	*/
    case ERROR_OUTOFMEMORY:			return ENOMEM;	/* 14	*/
    case ERROR_INVALID_DRIVE:			return ENOENT;	/* 15	*/
    case ERROR_CURRENT_DIRECTORY:		return EACCES;	/* 16	*/
    case ERROR_NOT_SAME_DEVICE:			return EXDEV;	/* 17	*/
    case ERROR_NO_MORE_FILES:			return ENOENT;	/* 18	*/
    case ERROR_WRITE_PROTECT:			return EACCES;	/* 19	*/
    case ERROR_BAD_UNIT:			return EACCES;	/* 20	*/
    case ERROR_NOT_READY:			return EACCES;	/* 21	*/
    case ERROR_BAD_COMMAND:			return EACCES;	/* 22	*/
    case ERROR_CRC:				return EACCES;	/* 23	*/
    case ERROR_BAD_LENGTH:			return EACCES;	/* 24	*/
    case ERROR_SEEK:				return EACCES;	/* 25	*/
    case ERROR_NOT_DOS_DISK:			return EACCES;	/* 26	*/
    case ERROR_SECTOR_NOT_FOUND:		return EACCES;	/* 27	*/
    case ERROR_OUT_OF_PAPER:			return EACCES;	/* 28	*/
    case ERROR_WRITE_FAULT:			return EACCES;	/* 29	*/
    case ERROR_READ_FAULT:			return EACCES;	/* 30	*/
    case ERROR_GEN_FAILURE:			return EACCES;	/* 31	*/
    case ERROR_SHARING_VIOLATION:		return EACCES;	/* 32	*/
    case ERROR_LOCK_VIOLATION:			return EACCES;	/* 33	*/
    case ERROR_WRONG_DISK:			return EACCES;	/* 34	*/
    case ERROR_SHARING_BUFFER_EXCEEDED:		return EACCES;	/* 36	*/
    case ERROR_BAD_NETPATH:			return ENOENT;	/* 53	*/
    case ERROR_NETWORK_ACCESS_DENIED:		return EACCES;	/* 65	*/
    case ERROR_BAD_NET_NAME:			return ENOENT;	/* 67	*/
    case ERROR_FILE_EXISTS:			return EEXIST;	/* 80	*/
    case ERROR_CANNOT_MAKE:			return EACCES;	/* 82	*/
    case ERROR_FAIL_I24:			return EACCES;	/* 83	*/
    case ERROR_INVALID_PARAMETER:		return EINVAL;	/* 87	*/
    case ERROR_NO_PROC_SLOTS:			return EAGAIN;	/* 89	*/
    case ERROR_DRIVE_LOCKED:			return EACCES;	/* 108	*/
    case ERROR_BROKEN_PIPE:			return EPIPE;	/* 109	*/
    case ERROR_DISK_FULL:			return ENOSPC;	/* 112	*/
    case ERROR_INVALID_TARGET_HANDLE:		return EBADF;	/* 114	*/
    case ERROR_WAIT_NO_CHILDREN:		return ECHILD;	/* 128	*/
    case ERROR_CHILD_NOT_COMPLETE:		return ECHILD;	/* 129	*/
    case ERROR_DIRECT_ACCESS_HANDLE:		return EBADF;	/* 130	*/
    case ERROR_NEGATIVE_SEEK:			return EINVAL;	/* 131	*/
    case ERROR_SEEK_ON_DEVICE:			return EACCES;	/* 132	*/
    case ERROR_DIR_NOT_EMPTY:			return ENOTEMPTY;/* 145	*/
    case ERROR_NOT_LOCKED:			return EACCES;	/* 158	*/
    case ERROR_BAD_PATHNAME:			return ENOENT;	/* 161	*/
    case ERROR_MAX_THRDS_REACHED:		return EAGAIN;	/* 164	*/
    case ERROR_LOCK_FAILED:			return EACCES;	/* 167	*/
    case ERROR_ALREADY_EXISTS:			return EEXIST;	/* 183	*/
    case ERROR_INVALID_STARTING_CODESEG:	return ENOEXEC;	/* 188	*/
    case ERROR_INVALID_STACKSEG:		return ENOEXEC;	/* 189	*/
    case ERROR_INVALID_MODULETYPE:		return ENOEXEC;	/* 190	*/
    case ERROR_INVALID_EXE_SIGNATURE:		return ENOEXEC;	/* 191	*/
    case ERROR_EXE_MARKED_INVALID:		return ENOEXEC;	/* 192	*/
    case ERROR_BAD_EXE_FORMAT:			return ENOEXEC;	/* 193	*/
    case ERROR_ITERATED_DATA_EXCEEDS_64k:	return ENOEXEC;	/* 194	*/
    case ERROR_INVALID_MINALLOCSIZE:		return ENOEXEC;	/* 195	*/
    case ERROR_DYNLINK_FROM_INVALID_RING:	return ENOEXEC;	/* 196	*/
    case ERROR_IOPL_NOT_ENABLED:		return ENOEXEC;	/* 197	*/
    case ERROR_INVALID_SEGDPL:			return ENOEXEC;	/* 198	*/
    case ERROR_AUTODATASEG_EXCEEDS_64k:		return ENOEXEC;	/* 199	*/
    case ERROR_RING2SEG_MUST_BE_MOVABLE:	return ENOEXEC;	/* 200	*/
    case ERROR_RELOC_CHAIN_XEEDS_SEGLIM:	return ENOEXEC;	/* 201	*/
    case ERROR_INFLOOP_IN_RELOC_CHAIN:		return ENOEXEC;	/* 202	*/
    case ERROR_FILENAME_EXCED_RANGE:		return ENOENT;	/* 206	*/
    case ERROR_NESTING_NOT_ALLOWED:		return EAGAIN;	/* 215	*/
    case ERROR_NOT_ENOUGH_QUOTA:		return ENOMEM;	/* 1816	*/
    default:					return EINVAL;
    }
}

int
erts_get_last_win_errno(void)
{
    return erts_map_win_error_to_errno(GetLastError());
}


#endif
