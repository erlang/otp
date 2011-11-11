/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2002-2009. All Rights Reserved.
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

#include "sys.h"
#include "global.h"
#include "benchmark.h"

#ifdef BM_COUNTERS
unsigned long long processes_spawned;
unsigned long long messages_sent;
unsigned long long messages_copied;
unsigned long long messages_ego;
unsigned long long minor_gc;
unsigned long long major_gc;
#ifdef HYBRID
unsigned long long minor_global_gc;
unsigned long long major_global_gc;
unsigned long long gc_in_copy;
#ifdef INCREMENTAL
unsigned long long minor_gc_cycles;
unsigned long long major_gc_cycles;
unsigned long long minor_gc_stages;
unsigned long long major_gc_stages;
#endif
#endif
#endif /* BM_COUNTERS */

#ifdef BM_TIMERS

#if (defined(__i386__) || defined(__x86_64__)) && USE_PERFCTR

#include "libperfctr.h"
struct vperfctr *system_clock;
double cpu_khz;
BM_NEW_TIMER(start);

static double get_hrvtime(void)
{
    unsigned long long ticks;
    double milli_seconds;

    ticks = vperfctr_read_tsc(system_clock);
    milli_seconds = (double)ticks / cpu_khz;
    return milli_seconds;
}

static void stop_hrvtime(void)
{
    if(system_clock)
    {
	vperfctr_stop(system_clock);
	vperfctr_close(system_clock);
	system_clock = NULL;
    }
}

#else /* not perfctr, asuming Solaris */
#include <time.h>
BM_TIMER_T system_clock;
#endif

unsigned long local_pause_times[MAX_PAUSE_TIME];
unsigned long pause_times[MAX_PAUSE_TIME];
unsigned long pause_times_old[MAX_PAUSE_TIME];

BM_TIMER_T mmu;
BM_TIMER_T mmu_counter;

BM_NEW_TIMER(timer);
BM_NEW_TIMER(system);
BM_NEW_TIMER(gc);
BM_NEW_TIMER(minor_gc);
BM_NEW_TIMER(major_gc);
BM_NEW_TIMER(minor_global_gc);
BM_NEW_TIMER(major_global_gc);
BM_NEW_TIMER(send);
BM_NEW_TIMER(copy);
BM_NEW_TIMER(size);
BM_NEW_TIMER(max_minor);
BM_NEW_TIMER(max_major);
BM_NEW_TIMER(max_global_minor);
BM_NEW_TIMER(max_global_major);
BM_NEW_TIMER(misc0);
BM_NEW_TIMER(misc1);
BM_NEW_TIMER(misc2);
#endif /* BM_TIMERS */

#ifdef BM_HEAP_SIZES
unsigned long long max_used_heap;
unsigned long long max_allocated_heap;
unsigned long long max_used_global_heap;
unsigned long long max_allocated_global_heap;
#endif /* BM_HEAP_SIZES */

#ifdef BM_MESSAGE_SIZES
unsigned long long words_sent;
unsigned long long words_copied;
unsigned long long words_prealloc;
unsigned long long message_sizes[1000];
#endif /* BM_MESSAGE_SIZES */

/*****
 * The following functions have to be defined, but they only have contents
 * if certain keywords are defined.
 */

void init_benchmarking()
{
#ifdef BM_TIMERS
#if (defined(__i386__) || defined(__x86_64__)) && USE_PERFCTR
    /* pass `--with-perfctr=/path/to/perfctr' when configuring */
    struct perfctr_info info;
    struct vperfctr_control control;
    int i;

    system_clock = vperfctr_open();
    if (system_clock != NULL)
    {
        if (vperfctr_info(system_clock,&info) >= 0)
        {
            cpu_khz = (double)info.cpu_khz;
            if (info.cpu_features & PERFCTR_FEATURE_RDTSC)
            {
                memset(&control,0,sizeof control);
                control.cpu_control.tsc_on = 1;
            }
        }
        if (vperfctr_control(system_clock,&control) < 0)
        {
            vperfctr_close(system_clock);
            system_clock = NULL;
        }
    }

    for (i = 0; i < 1000; i++)
    {
        BM_START_TIMER(system);
        BM_STOP_TIMER(system);
    }

    timer_time = system_time / 1000;
    start_time = 0;
#else
    int i;
    for (i = 0; i < 1000; i++)
    {
        BM_START_TIMER(system);
        BM_STOP_TIMER(system);
    }
    timer_time = system_time / 1000;
#endif

    for (i = 0; i < MAX_PAUSE_TIME; i++) {
        local_pause_times[i] = 0;
        pause_times[i] = 0;
        pause_times_old[i] = 0;
    }

    mmu = 0;
    mmu_counter = 0;

    BM_MMU_INIT();
#endif /* BM_TIMERS */

#ifdef BM_COUNTERS
    processes_spawned  = 0;
    messages_sent      = 0;
    messages_copied    = 0;
    messages_ego       = 0;
    minor_gc           = 0;
    major_gc           = 0;
#ifdef HYBRID
    minor_global_gc    = 0;
    major_global_gc    = 0;
    gc_in_copy         = 0;
#ifdef INCREMENTAL
    minor_gc_cycles    = 0;
    major_gc_cycles    = 0;
    minor_gc_stages    = 0;
    major_gc_stages    = 0;
#endif
#endif
#endif /* BM_COUNTERS */

#ifdef BM_HEAP_SIZES
    max_used_heap             = 0;
    max_allocated_heap        = 0;
    max_used_global_heap      = 0;
    max_allocated_global_heap = 0;
#endif /* BM_HEAP_SIZES */

#ifdef BM_MESSAGE_SIZES
    words_sent   = 0;
    words_copied = 0;
    words_prealloc = 0;
    {
        int i;
        for (i = 0; i < 1000; i++)
            message_sizes[i] = 0;
    }
#endif /* BM_MESSAGE_SIZES */
}

void save_statistics()
{
#ifdef BM_STATISTICS
    FILE *file = fopen(BM_STATISTICS_FILE,"a");
    long i = 0;

    if (file)
    {
        erts_fprintf(file,"-------------------------------------------------------------------------\n");
        erts_fprintf(file,"The counters are reset at system start and are sums over the entire node.\n");
        erts_fprintf(file,"You may reset them manually using the BIFs in the module hipe_bifs.\n");
        erts_fprintf(file,"All times are given in milliseconds.\n");
        erts_fprintf(file,"-------------------------------------------------------------------------\n");

	erts_fprintf(file,"Node: %T\n",erts_this_node->sysname);

#ifdef BM_COUNTERS
        erts_fprintf(file,"Number of processes spawned: %lld\n",processes_spawned);
        erts_fprintf(file,"Number of local minor GCs: %lld\n",minor_gc);
        erts_fprintf(file,"Number of local major GCs: %lld\n",major_gc);
#ifdef HYBRID
        erts_fprintf(file,"Number of global minor GCs: %lld\n",minor_global_gc);
        erts_fprintf(file,"Number of global major GCs: %lld\n",major_global_gc);
#ifdef INCREMENTAL
        erts_fprintf(file,"Number of minor GC-cycles: %lld\n",minor_gc_cycles);
        erts_fprintf(file,"Number of major GC-cycles: %lld\n",major_gc_cycles);
        erts_fprintf(file,"Number of minor GC-stages: %lld\n",minor_gc_stages);
        erts_fprintf(file,"Number of major GC-stages: %lld\n",major_gc_stages);
#endif
#endif
        erts_fprintf(file,"Number of messages sent: %lld\n",messages_sent);
        erts_fprintf(file,"Number of messages copied: %lld\n",messages_copied);
        erts_fprintf(file,"Number of messages sent to self: %lld\n",messages_ego);
#endif /* BM_COUNTERS */

#ifdef BM_MESSAGE_SIZES
        erts_fprintf(file,"Number of words sent: %lld\n",words_sent);
        erts_fprintf(file,"Number of words copied: %lld\n",words_copied);
        erts_fprintf(file,"Number of words preallocated: %lld\n",words_prealloc);
#endif /* BM_MESSAGE_SIZES */

#ifdef BM_HEAP_SIZES
        erts_fprintf(file,"Biggest local heap used (in words): %lld\n",max_used_heap);
        erts_fprintf(file,"Biggest local heap allocated (in words): %lld\n",max_allocated_heap);
        erts_fprintf(file,"Biggest global heap used (in words): %lld\n",max_used_global_heap);
        erts_fprintf(file,"Biggest global heap allocated (in words): %lld\n",max_allocated_global_heap);
#endif /* BM_HEAP_SIZES */

#ifdef BM_TIMERS
        erts_fprintf(file,"--- The total active system time is the sum of all times below ---\n");
        BM_TIME_PRINTER("Mutator time",system_time);
        BM_TIME_PRINTER("Time spent in send (excluding size & copy)",send_time);
        BM_TIME_PRINTER("Time spent in size",size_time);
        BM_TIME_PRINTER("Time spent in copy",copy_time);
        BM_TIME_PRINTER("Time spent in local minor GC",minor_gc_time);
        BM_TIME_PRINTER("Time spent in local major GC",major_gc_time);
        BM_TIME_PRINTER("Time spent in global minor GC",minor_global_gc_time);
        BM_TIME_PRINTER("Time spent in global major GC",major_global_gc_time);
        erts_fprintf(file,"---\n");
        BM_TIME_PRINTER("Maximum time spent in one separate local minor GC",max_minor_time);
        BM_TIME_PRINTER("Maximum time spent in one separate local major GC",max_major_time);
        BM_TIME_PRINTER("Maximum time spent in one separate global minor GC",max_global_minor_time);
        BM_TIME_PRINTER("Maximum time spent in one separate global major GC",max_global_major_time);
#endif /* BM_TIMERS */

#if 0
        /* Save a log file for import into excel */

        long long total_time, n;
        long left, right, mid;

#ifdef BM_COUNTERS
        erts_fprintf(file,"Spawns\tLocalGC\tMAGC\tMessages\tMutator_t\tLocalGC_t\tMAGC_t\tLocMaxP\tLocMeanP\tLocGeoMP\tMAMaxP\tMAMeanP\tMAGeoMP\t\tCMAGC\tCMAGC_t\n");
        erts_fprintf(file,"%lld\t%lld\t%lld\t%lld\t",
                processes_spawned,
                minor_garbage_cols + major_garbage_cols,
                minor_global_garbage_cols + major_global_garbage_cols,
                messages_sent);
#endif /* BM_COUNTERS */

#ifdef BM_TIMERS
        erts_fprintf(file,"%lld\t%lld\t%lld\t",
                (long long)(system_time + send_time + size_time + copy_time),
                (long long)(minor_gc_time + major_gc_time),
                (long long)(minor_global_gc_time + major_global_gc_time));

        total_time = 0; n = 0;
        left = 0; right = 0; mid = 0;
        for (i = 0; i < MAX_PAUSE_TIME; i++) {
            total_time += local_pause_times[i] * i;
            n += local_pause_times[i];
            if (i > mid)
                right += local_pause_times[i];
            while(right > left) {
                left += local_pause_times[mid++];
                right -= local_pause_times[mid];
            }
        }
        erts_fprintf(file,"%lld\t%lld\t%ld\t",
                (long long)((max_minor_time > max_major_time ?
                             max_minor_time :
                             max_major_time)*1000),
                total_time / n,
                mid);

        total_time = 0; n = 0;
        left = 0; right = 0; mid = 0;
        for (i = 0; i < MAX_PAUSE_TIME; i++) {
            if (pause_times[i] > 0) {
                total_time += pause_times[i] * i;
                n += pause_times[i];
                if (i > mid)
                    right += pause_times[i];
                while(right > left) {
                    left += pause_times[mid++];
                    right -= pause_times[mid];
                }
            }
        }
        erts_fprintf(file,"%lld\t%lld\t%ld\t",
                (long long)((max_global_minor_time > max_global_major_time ?
                             max_global_minor_time :
                             max_global_major_time)*1000),
                (n > 0 ? total_time / n : 0),
                mid);

        erts_fprintf(file,"\t%lld\t%lld\n",n,total_time);

        erts_fprintf(file,"\nMinor:\n");
        for (i = 0; i < MAX_PAUSE_TIME; i++) {
            if (i < 1000 || pause_times[i] > 0) {
                erts_fprintf(file,"%d\t%ld\n",i,pause_times[i]);
            }
        }

        fprintf(file,"Major:\n");
        for (i = 0; i < MAX_PAUSE_TIME; i++) {
            if (pause_times_old[i] > 0) {
                fprintf(file,"%d\t%ld\n",i,pause_times_old[i]);
            }
        }
#endif /* BM_TIMERS */

#ifdef BM_TIMERS
        total_time = 0; n = 0;
        left = 0; right = 0; mid = 0;
        fprintf(file,"\nLocal:\n");
        for (i = 0; i < MAX_PAUSE_TIME; i++) {
            if (local_pause_times[i] > 0) {
                erts_fprintf(file,"%d\t%ld\n",i,local_pause_times[i]);
                total_time += local_pause_times[i] * i;
                n += local_pause_times[i];
                if (i > mid)
                    right += local_pause_times[i];
                while(right > left) {
                    left += local_pause_times[mid++];
                    right -= local_pause_times[mid];
                }
            }
        }
        erts_fprintf(file,"Mid: %ld  Mean: %ld\n",(long)mid,
                (long)(n > 0 ? total_time / n : 0));
#endif
#endif /* 0 */
        fclose(file);
    }
    else
        fprintf(stderr,"Sorry... Can not write to %s!\n\r",BM_STATISTICS_FILE);
#endif /* BM_STATISTICS */
}
