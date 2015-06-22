/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2002-2012. All Rights Reserved.
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

#include "sys.h"
#include "global.h"
#include "benchmark.h"

#ifdef BM_COUNTERS
unsigned long long processes_busy;
unsigned long long processes_spawned;
unsigned long long messages_sent;
unsigned long long messages_copied;
unsigned long long messages_ego;
unsigned long long minor_gc;
unsigned long long major_gc;
#endif /* BM_COUNTERS */

#ifdef BM_TIMERS

/* assuming Solaris */
#include <time.h>
BM_TIMER_T system_clock;

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
    int i;
    for (i = 0; i < 1000; i++)
    {
        BM_START_TIMER(system);
        BM_STOP_TIMER(system);
    }
    timer_time = system_time / 1000;

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
    processes_busy     = 0;
    processes_spawned  = 0;
    messages_sent      = 0;
    messages_copied    = 0;
    messages_ego       = 0;
    minor_gc           = 0;
    major_gc           = 0;
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
