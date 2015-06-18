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

#ifndef __BENCHMARK_H__
#define __BENCHMARK_H__

/* The define __BENCHMARK__ is the master switch to turn on and off
 * benchmarking. This will enable the benchmark-BIFs in hipe_bif1.c.
 * Documentation for the BIFs is in hipe_bif1.c, and that is where you
 * will find the information about how to accually get some data out
 * from these timers and counters.
 */
/* #define __BENCHMARK__ */

#ifdef __BENCHMARK__
/*
 * The defines below enables different parts of the benchmaring.
 * Counters and timers that are disabled, always report zero in
 * the BIFs.
 */

/* BM_TIMERS keeps track of the time spent in diferent parts of the
 * system. It only measures accual active time, not time spent in idle
 * mode. Currently, the Solaris hrtime_t will be used.
 * To add new timers look below.
 */
#define BM_TIMERS

/* BM_COUNTERS count all kinds of events that occurs in the system.
 * Among other things it counts the number of messages, then number of
 * garbage collections, the number of processes spawned etc.
 * To add new counters look below.
 */
#define BM_COUNTERS

/* BM_MESSAGE_SIZES keeps a log of the size of all messages sent in
 * the system. This introduce an overhead in time for the shared heap
 * system since all message sizes have to be calculated at send.
 */
/* #define BM_MESSAGE_SIZES */

/* BM_HEAP_SIZES goes through all processes at garbage collection time
 * to sum their allocated and used heap sizes. In anything else than a
 * shared heap system, this will cost.
 */
/* #define BM_HEAP_SIZES */

/* BM_STATISTICS saves an entry in the file BM_STATISTICS_FILE. This
 * is done for each erlang node at exit time.
 */
/* #define BM_STATISTICS */

#endif /* __BENCHMARK__ */


#ifdef BM_STATISTICS
#  define BM_STATISTICS_FILE "/tmp/erlang_statistics.joppe.log"
#endif /* BM_STATISTICS */


/************ There are no more settings below this line *************/

/*
 * Maintenance and how to add new stuff is documented by the code
 * below ;-)
 */

#ifdef BM_COUNTERS
/*********************************************************************
 * To add new counters:
 *
 * Add the variable here AND in benchmark.c. Use the macro
 * BM_COUNT(var) in the code where you want to increase it.
 *
 */
extern unsigned long long processes_busy;
extern unsigned long long processes_spawned;
extern unsigned long long messages_sent;
extern unsigned long long messages_copied;
extern unsigned long long messages_ego;
extern unsigned long long minor_gc;
extern unsigned long long major_gc;

#define BM_COUNT(var) (var)++;

#define BM_EGO_COUNT(send,rec) {     \
    if ((send) == (rec))             \
        BM_COUNT(messages_ego);      }

#define BM_LAZY_COPY_START long long gcs = minor_global_gc + major_global_gc;
#define BM_LAZY_COPY_STOP { gcs = (minor_global_gc + major_global_gc) - gcs; \
    if (gcs > gc_in_copy) gc_in_copy = gcs; }

#else /* !BM_COUNTERS */
#  define BM_COUNT(var)
#  define BM_EGO_COUNT(send,rec)
#  define BM_LAZY_COPY_START
#  define BM_LAZY_COPY_STOP
#endif /* BM_COUNTERS */


#ifdef BM_TIMERS
/*********************************************************************
 * To add new timers:
 *
 * Add the variable below using the form extern BM_TIMER_T blah_time.
 * Also add them in benchmark.c using the macro NEW_TIMER(blah).  Use
 * the macro BM_SWAP_TIMER(from,blah) ... BM_SWAP_TIMER(blah,to) to
 * start and stop the new timer. Note, that you have to know what
 * timer is running at the place where you want to insert your new
 * timer to be able to stop and start (from,to) it.
 *
 * You can use the macros BM_STOP_TIMER(blah) and BM_START_TIMER(blah)
 * around code that should not be timed at all. As above, you have to
 * know what timer to start and stop. The system timer is running at
 * most places in the emulator. Only the garbage collector and the
 * message sending has its own timers at the moment.
 *
 * The timer_time used when stopping timers is the time it takes to
 * start and stop the timers, calculated in init_benchmarking(). If it
 * is not there, the time it takes to do this will accually be
 * substantial compared to some small times in the system we want to
 * meassure (send time in shared heap for instance).
 */

/* (Assuming Solaris) */

#define BM_TIMER_T ErtsMonotonicTime
#define BM_START_TIMER(t) system_clock = ERTS_MONOTONIC_TO_NSEC(erts_os_monotonic_time())
#define BM_STOP_TIMER(t) do {                                        \
    BM_TIMER_T tmp = (ERTS_MONOTONIC_TO_NSEC(erts_os_monotonic_time()) - system_clock) - timer_time;  \
    t##_time += (tmp > 0 ? tmp : 0);                                 \
} while(0)

#define BM_TIME_PRINTER(str,time) do {                               \
    int min,sec,milli,micro;                                         \
    BM_TIMER_T tmp;                                                  \
    tmp = (time) / 1000;                                             \
    micro = tmp % 1000;                                              \
    tmp /= 1000;                                                     \
    milli = tmp % 1000;                                              \
    tmp /= 1000;                                                     \
    sec = tmp % 60;                                                  \
    min = tmp / 60;                                                  \
    erts_fprintf(file,str": %d:%02d.%03d %03d\n",min,sec,milli,micro);  \
} while(0)

extern BM_TIMER_T system_clock;

extern BM_TIMER_T timer_time;
extern BM_TIMER_T system_time;
extern BM_TIMER_T gc_time;
extern BM_TIMER_T minor_gc_time;
extern BM_TIMER_T major_gc_time;
extern BM_TIMER_T minor_global_gc_time;
extern BM_TIMER_T major_global_gc_time;
extern BM_TIMER_T send_time;
extern BM_TIMER_T copy_time;
extern BM_TIMER_T size_time;
extern BM_TIMER_T max_minor_time;
extern BM_TIMER_T max_major_time;
extern BM_TIMER_T max_global_minor_time;
extern BM_TIMER_T max_global_major_time;
extern BM_TIMER_T misc0_time;
extern BM_TIMER_T misc1_time;
extern BM_TIMER_T misc2_time;

#define MAX_PAUSE_TIME 500000
extern unsigned long local_pause_times[MAX_PAUSE_TIME];
extern unsigned long pause_times[MAX_PAUSE_TIME];
extern unsigned long pause_times_old[MAX_PAUSE_TIME];

#define MMU_INTERVAL 5  /* milli seconds */
extern BM_TIMER_T mmu_counter;
extern BM_TIMER_T mmu;

#define BM_NEW_TIMER(t) BM_TIMER_T t##_time = 0;
#define BM_RESET_TIMER(t) t##_time = 0;
#define BM_SWAP_TIMER(t1,t2) do { BM_STOP_TIMER(t1); BM_START_TIMER(t2); } while(0)
#define BM_MMU_INIT() do {                                              \
    BM_TIMER_T gc = gc_time;                                            \
    while (gc > 0) {                                                    \
        if (gc > MMU_INTERVAL) {                                        \
            gc -= MMU_INTERVAL - mmu_counter;                           \
            erts_printf("%d\n",(int)((mmu / MMU_INTERVAL) * 100));      \
            mmu_counter = 0; mmu = 0;                                   \
        } else {                                                        \
            mmu_counter += gc;                                          \
            if (mmu_counter >= MMU_INTERVAL) {                          \
                mmu_counter -= MMU_INTERVAL;                            \
                erts_printf("%d\n",(int)((mmu / MMU_INTERVAL) * 100));  \
                mmu = 0;                                                \
            }                                                           \
            gc = 0;                                                     \
        }                                                               \
    }                                                                   \
    BM_RESET_TIMER(system);                                             \
    BM_RESET_TIMER(send);                                               \
    BM_RESET_TIMER(copy);                                               \
    BM_RESET_TIMER(size);                                               \
} while(0)

#define BM_MMU_READ() do {                                              \
    BM_TIMER_T mut = system_time + send_time + copy_time + size_time;   \
    while (mut > 0) {                                                   \
        if (mut > MMU_INTERVAL) {                                       \
            BM_TIMER_T tmp = MMU_INTERVAL - mmu_counter;                \
            mmu += tmp; mut -= tmp;                                     \
            erts_printf("%d\n",(int)((mmu / MMU_INTERVAL) * 100));      \
            mmu_counter = 0; mmu = 0;                                   \
        } else {                                                        \
            mmu_counter += mut; mmu += mut;                             \
            if (mmu_counter >= MMU_INTERVAL) {                          \
                mmu_counter -= MMU_INTERVAL;                            \
                mmu -= mmu_counter;                                     \
                erts_printf("%d\n",(int)((mmu / MMU_INTERVAL) * 100));  \
                mmu = mmu_counter;                                      \
            }                                                           \
            mut = 0;                                                    \
        }                                                               \
    }                                                                   \
} while(0)

#else /* !BM_TIMERS */
#  define BM_NEW_TIMER(t)
#  define BM_START_TIMER(t)
#  define BM_STOP_TIMER(t)
#  define BM_RESET_TIMER(t)
#  define BM_SWAP_TIMER(t1,t2)
#  define BM_TIME_PRINTER(str,time)
#  define BM_MMU_INIT()
#  define BM_MMU_READ()
#endif /* BM_TIMERS */

#ifdef BM_HEAP_SIZES
extern unsigned long long max_used_heap;
extern unsigned long long max_allocated_heap;
extern unsigned long long max_used_global_heap;
extern unsigned long long max_allocated_global_heap;
#endif /* BM_HEAP_SIZES */

#ifdef BM_MESSAGE_SIZES
extern unsigned long long words_sent;
extern unsigned long long words_copied;
extern unsigned long long words_prealloc;
extern unsigned long long message_sizes[1000];

#define BM_MESSAGE_COPIED(size) { \
    words_copied += size;         \
    BM_COUNT(messages_copied);    }

#define BM_PREALLOC_DATA(size) { \
    words_prealloc += size;      }

#define BM_MESSAGE(mess,send,rec) {  \
    Uint msize = size_object(mess);  \
    words_sent += msize;             \
    if (msize < 1000)                \
        message_sizes[msize]++;      \
    else                             \
        message_sizes[999]++;        \
    BM_EGO_COUNT(send,rec);          \
    BM_COUNT(messages_sent);         }

#else /* !BM_MESSAGE_SIZES */

#define BM_MESSAGE_COPIED(size) BM_COUNT(messages_copied);
#define BM_PREALLOC_DATA(size)
#define BM_MESSAGE(mess,send,rec) {  \
    BM_EGO_COUNT(send,rec);          \
    BM_COUNT(messages_sent);         }

#endif /* BM_MESSAGE_SIZES */

void init_benchmarking(void);
void save_statistics(void);

#endif /* _BENCHMARK_H_ */
