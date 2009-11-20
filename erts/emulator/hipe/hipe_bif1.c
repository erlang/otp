/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2001-2009. All Rights Reserved.
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
/* $Id$
 * hipe_bif1.c
 *
 * Performance analysis support.
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
#include "sys.h"
#include "global.h"
#include "bif.h"
#include "big.h"
#include "error.h"
#include "beam_load.h"
#include "hipe_bif0.h"
#include "hipe_bif1.h"

#define BeamOpCode(Op)	((Uint)BeamOp(Op))

BIF_RETTYPE hipe_bifs_call_count_on_1(BIF_ALIST_1)
{
    Eterm *pc;
    struct hipe_call_count *hcc;

    pc = hipe_bifs_find_pc_from_mfa(BIF_ARG_1);
    if (!pc)
	BIF_ERROR(BIF_P, BADARG);
    ASSERT(pc[-5] == BeamOpCode(op_i_func_info_IaaI));
    if (pc[0] == BeamOpCode(op_hipe_trap_call))
	BIF_ERROR(BIF_P, BADARG);
    if (pc[0] == BeamOpCode(op_hipe_call_count))
	BIF_RET(NIL);
    hcc = erts_alloc(ERTS_ALC_T_HIPE, sizeof(*hcc));
    hcc->count = 0;
    hcc->opcode = pc[0];
    pc[-4] = (Eterm)hcc;
    pc[0] = BeamOpCode(op_hipe_call_count);
    BIF_RET(am_true);
}

BIF_RETTYPE hipe_bifs_call_count_off_1(BIF_ALIST_1)
{
    Eterm *pc;
    struct hipe_call_count *hcc;
    unsigned count;

    pc = hipe_bifs_find_pc_from_mfa(BIF_ARG_1);
    if (!pc)
	BIF_ERROR(BIF_P, BADARG);
    ASSERT(pc[-5] == BeamOpCode(op_i_func_info_IaaI));
    if (pc[0] != BeamOpCode(op_hipe_call_count))
	BIF_RET(am_false);
    hcc = (struct hipe_call_count*)pc[-4];
    count = hcc->count;
    pc[0] = hcc->opcode;
    pc[-4] = (Eterm)NULL;
    erts_free(ERTS_ALC_T_HIPE, hcc);
    BIF_RET(make_small(count));
}

BIF_RETTYPE hipe_bifs_call_count_get_1(BIF_ALIST_1)
{
    Eterm *pc;
    struct hipe_call_count *hcc;

    pc = hipe_bifs_find_pc_from_mfa(BIF_ARG_1);
    if (!pc)
	BIF_ERROR(BIF_P, BADARG);
    ASSERT(pc[-5] == BeamOpCode(op_i_func_info_IaaI));
    if (pc[0] != BeamOpCode(op_hipe_call_count))
	BIF_RET(am_false);
    hcc = (struct hipe_call_count*)pc[-4];
    BIF_RET(make_small(hcc->count));
}

BIF_RETTYPE hipe_bifs_call_count_clear_1(BIF_ALIST_1)
{
    Eterm *pc;
    struct hipe_call_count *hcc;
    unsigned count;

    pc = hipe_bifs_find_pc_from_mfa(BIF_ARG_1);
    if (!pc)
	BIF_ERROR(BIF_P, BADARG);
    ASSERT(pc[-5] == BeamOpCode(op_i_func_info_IaaI));
    if (pc[0] != BeamOpCode(op_hipe_call_count))
	BIF_RET(am_false);
    hcc = (struct hipe_call_count*)pc[-4];
    count = hcc->count;
    hcc->count = 0;
    BIF_RET(make_small(count));
}

unsigned int hipe_trap_count;

BIF_RETTYPE hipe_bifs_trap_count_get_0(BIF_ALIST_0)
{
    BIF_RET(make_small(hipe_trap_count));
}

BIF_RETTYPE hipe_bifs_trap_count_clear_0(BIF_ALIST_0)
{
    unsigned int count = hipe_trap_count;
    hipe_trap_count = 0;
    BIF_RET(make_small(count));
}

/*****************************************************************************
 * BIFs for benchmarking. These only do useful things if
 * __BENCHMARK__ is defined in beam/benchmark.h. For documentation
 * about how to add new counters or maintain the existing counters,
 * see benchmark.h.
 *
 * If benchmarking is not enabled all BIFs will return false. If the
 * required benchmark feature is not enabled, the counter will remain
 * zero.
 *
 * process_info/0 -> { Number of live processes,
 *		       Processes spawned in total }
 *
 *   Live processes are increased when a new process is created, and
 *   decreased when a process dies. Processes spawned is increased
 *   when a process is created.
 *
 *
 * process_info_clear/0 -> true
 *
 *   Will reset the processes spawned-counters to zero. If this is
 *   done at some improper time, live processes may become a negative
 *   value. This is not a problem in itself, just as long as you know
 *   about it.
 *
 *
 * message_info/0 -> { Messages sent,
 *		       Messages copied,
 *		       Ego messages (sender = receiver),
 *		       Words sent,
 *		       Words copied,
 *		       Words preallocated }
 *
 *   Counting the words sent in a shared heap system will affect
 *   runtime performance since it means that we have to calculate the
 *   size of the mesage. With private heaps, this is done anyway and
 *   will not affect performance.
 *
 *
 * message_info_clear/0 -> true
 *
 *   Reset the message counters to zero.
 *
 *
 * message_sizes/0 -> true
 *
 *   Displays a text-mode bar diagram with message sizes. There are no
 *   guaranties that this is printed in a way the Erlang system is
 *   supposed to print things.
 *
 *
 * gc_info/0 -> { Minor collections,
 *		  Major collections,
 *		  Used heap,
 *		  Allocated heap,
 *		  Max used heap,
 *		  Max allocated heap }
 *
 *   Information about private heap garbage collections. Number of
 *   minor and major collections, how much heap is used and allocated
 *   and how much heap has been in use and allocated at most since the
 *   counters were reset.
 *
 *
 * shared_gc_info/0 -> { Minor collections of the shared heap,
 *			 Major collections of the shared heap,
 *			 Used shared heap,
 *			 Allocated shared heap,
 *			 Max used shared heap,
 *			 Max allocated shared heap }
 *
 *   The same as above, but for the shared heap / message area. Note,
 *   that in a shared heap system the max used heap and max allocated
 *   heap are mostly the same, since the heap allways is filled before
 *   a garbage collection, and most garbage collections do not enlarge
 *   the heap. The private heap numbers are much more interesting.
 *
 *
 * incremental_gc_info/0 -> { Complete minor GC cycles,
 *			      Complete major GC cycles,
 *			      Minor GC stages,
 *			      Major GC stages }
 *
 *
 * gc_info_clear/0 -> true
 *
 *   Reset counters for both private and shared garbage collection.
 *
 *
 * BM Timers
 * ---------
 *
 * All timers returns tuples of the kind: { Minutes, Seconds, Milliseconds }
 * except for the max times in garbage collection where times are normally
 * small. The tuple is therefor: { Seconds, Milliseconds, Microseconds }
 *
 * system_timer/0 -> Mutator time
 *
 *   This timer is not a real-time clock, it only runs when a process
 *   is scheduled to run. You can not find out the accual time a
 *   program has taken to run using this timer.
 *
 *
 * system_timer_clear/0 -> true
 *
 *   Reset system timer to zero.
 *
 *
 * send_timer/0 -> { Send time,
 *		     Copy time,
 *		     Size time }
 *
 *   Time spent in sending messages. The copy time and size time are
 *   only active if the copying is needed in send. Copying of data
 *   into ETS-tables etc is not timed with this timer.
 *
 *
 * send_timer_clear/0 -> true
 *
 *   Reset send timers to zero.
 *
 *
 * gc_timer/0 -> { Time in minor collection,
 *		   Time in major collection,
 *		   Max time in minor collection (탎),
 *		   Max time in major collection (탎) }
 *
 *   Total time spent in garbage collection of the private heaps. The
 *   max times are for one separate collection.
 *
 *
 * shared_gc_timer/0 -> { Time in minor collection,
 *			  Time in major collection,
 *			  Max time in minor collection (탎),
 *			  Max time in major collection (탎) }
 *
 *   Total time spent in garbage collection of the shared heap /
 *   message area. The max times are for one separate collection.
 *
 *
 * gc_timer_clear/0 -> true
 *
 *   Reset private and shared garbage collection timers to zero. Note,
 *   that the max-times are also reset.
 *
 *
 * misc_timer/0 -> { Misc 0, Misc 1, Misc 2 }
 *
 *   Timers for debug purposes. In a normal system, these timers are
 *   never used. Add these timers at places where you want to time
 *   something not covered here. Use BM_SWAP_TIMER(from,to) to start
 *   one of the misc timers.
 *
 *   ... code timed by the system timer ...
 *   BM_SWAP_TIMER(system,misc1);
 *   ... code we want to time ...
 *   BM_SWAP_TIMER(misc1,system);
 *   ... back on system time ...
 *
 *
 * misc_timer_clear/0 -> true
 *
 *   Reset misc timers to zero.
 */

BIF_RETTYPE hipe_bifs_process_info_0(BIF_ALIST_0)
{
#ifdef __BENCHMARK__
#ifndef BM_COUNTERS
    Uint processes_busy	   = 0;
    Uint processes_spawned = 0;
#endif
    Eterm *hp;

    hp = HAlloc(BIF_P, 3);
    BIF_RET(TUPLE2(hp,
		   make_small(processes_busy),
		   make_small(processes_spawned)));
#else
    BIF_RET(am_false);
#endif
}

BIF_RETTYPE hipe_bifs_process_info_clear_0(BIF_ALIST_0)
{
#ifdef __BENCHMARK__
#ifdef BM_COUNTERS
    processes_spawned = 0;
#endif
    BIF_RET(am_true);
#else
    BIF_RET(am_false);
#endif
}

BIF_RETTYPE hipe_bifs_message_info_0(BIF_ALIST_0)
{
#ifdef __BENCHMARK__
    Eterm *hp;
#ifndef BM_COUNTERS
    unsigned long messages_sent	  = 0;
    unsigned long messages_copied = 0;
    unsigned long messages_ego	  = 0;
#endif
#ifndef BM_MESSAGE_SIZES
    unsigned long words_sent   = 0;
    unsigned long words_copied = 0;
    unsigned long words_prealloc = 0;
#endif

    hp = HAlloc(BIF_P, 7);
    BIF_RET(TUPLE6(hp,
		   make_small(messages_sent),
		   make_small(messages_copied),
		   make_small(messages_ego),
		   make_small(words_sent),
		   make_small(words_copied),
		   make_small(words_prealloc)));
#else
    BIF_RET(am_false);
#endif
}

BIF_RETTYPE hipe_bifs_message_info_clear_0(BIF_ALIST_0)
{
#ifdef __BENCHMARK__
#ifdef BM_COUNTERS
    messages_sent   = 0;
    messages_copied = 0;
    messages_ego    = 0;
#endif
#ifdef BM_MESSAGE_SIZES
    words_sent	 = 0;
    words_copied = 0;
    words_prealloc = 0;
    {
	int i;
	for (i = 0; i < 1000; i++)
	    message_sizes[i] = 0;
    }
#endif
    BIF_RET(am_true);
#else
    BIF_RET(am_false);
#endif
}

BIF_RETTYPE hipe_bifs_message_sizes_0(BIF_ALIST_0)
{
#ifdef BM_MESSAGE_SIZES
    int i, j, max = 0;
    int tmp[12] = {0,0,0,0,0,0,0,0,0,0,0,0};

    for (i = 0; i < 65; i++) {
	tmp[0] += message_sizes[i];
	if (tmp[0] > max)
	    max = tmp[0];
    }
    for (i = 65; i < 999; i++) {
	tmp[i / 100 + 1] += message_sizes[i];
	if (tmp[i / 100 + 1] > max)
	    max = tmp[i / 100 + 1];
    }
    tmp[11] = message_sizes[999];
    if (tmp[11] > max)
	max = tmp[11];
    for (i = -1; i < 11; i++) {
	int num = (tmp[i + 1] * 50) / max;
	if (i == -1)
	    printf("\n\r  0 -  64: (%6d) |", tmp[0]);
	else if (i == 0)
	    printf("\n\r 65 -  99: (%6d) |", tmp[1]);
	else if (i == 10)
	    printf("\n\r  >= 1000: (%6d) |", tmp[11]);
	else
	    printf("\n\r%3d - %3d: (%6d) |", i * 100, i * 100 + 99,
		   tmp[i + 1]);

	for (j = 0; j < num; j++)
	    printf(".");
    }
    printf("\n\r");

    BIF_RET(am_true);
#else
    BIF_RET(am_false);
#endif
}

BIF_RETTYPE hipe_bifs_gc_info_0(BIF_ALIST_0)
{
#ifdef __BENCHMARK__
#ifndef BM_COUNTERS
    Uint minor_gc = 0;
    Uint major_gc = 0;
#endif
#ifndef BM_HEAP_SIZES
    Uint max_used_heap	    = 0;
    Uint max_allocated_heap = 0;
#endif
    Eterm *hp;
    Uint used_heap = (BIF_P->htop - BIF_P->heap) +
		     (OLD_HTOP(BIF_P) - OLD_HEAP(BIF_P)) +
		     MBUF_SIZE(BIF_P);

    Uint alloc_heap = (BIF_P->hend - BIF_P->heap) +
		      (OLD_HEND(BIF_P) - OLD_HEAP(BIF_P)) +
		      MBUF_SIZE(BIF_P);

    hp = HAlloc(BIF_P, 7);
    BIF_RET(TUPLE6(hp,
		   make_small((Uint)minor_gc),
		   make_small((Uint)major_gc),
		   make_small((Uint)used_heap),
		   make_small((Uint)alloc_heap),
		   make_small(max_used_heap),
		   make_small(max_allocated_heap)));
#else
    BIF_RET(am_false);
#endif
}

BIF_RETTYPE hipe_bifs_shared_gc_info_0(BIF_ALIST_0)
{
#ifdef __BENCHMARK__
#if !(defined(BM_COUNTERS) && defined(HYBRID))
    Uint minor_global_gc = 0;
    Uint major_global_gc = 0;
#endif
#ifndef BM_HEAP_SIZES
    Uint max_used_global_heap	   = 0;
    Uint max_allocated_global_heap = 0;
#endif
    Eterm *hp;

#if defined(HYBRID)
    Uint tmp_used_heap = (Uint)((BIF_P->htop - BIF_P->heap) +
				(OLD_HTOP(BIF_P) - OLD_HEAP(BIF_P)) +
				MBUF_SIZE(BIF_P));
    Uint tmp_allocated_heap = (Uint)((BIF_P->hend - BIF_P->heap) +
				     (OLD_HEND(BIF_P) - OLD_HEAP(BIF_P)) +
				     MBUF_SIZE(BIF_P));
#else
    Uint tmp_used_heap = 0;
    Uint tmp_allocated_heap = 0;
#endif
    hp = HAlloc(BIF_P, 7);
    BIF_RET(TUPLE6(hp,
		   make_small((uint)minor_global_gc),
		   make_small((uint)major_global_gc),
		   make_small(tmp_used_heap),
		   make_small(tmp_allocated_heap),
		   make_small(max_used_global_heap),
		   make_small(max_allocated_global_heap)));
#else
    BIF_RET(am_false);
#endif
}

BIF_RETTYPE hipe_bifs_incremental_gc_info_0(BIF_ALIST_0)
{
#ifdef __BENCHMARK__
#if !(defined(BM_COUNTERS) && defined(INCREMENTAL))
    Uint minor_gc_cycles = 0;
    Uint major_gc_cycles = 0;
    Uint minor_gc_stages = 0;
    Uint major_gc_stages = 0;
#endif
    Eterm *hp;

    hp = HAlloc(BIF_P, 5);
    BIF_RET(TUPLE4(hp,
		   make_small(minor_gc_cycles),
		   make_small(major_gc_cycles),
		   make_small(minor_gc_stages),
		   make_small(major_gc_stages)));
#else
    BIF_RET(am_false);
#endif
}

BIF_RETTYPE hipe_bifs_gc_info_clear_0(BIF_ALIST_0)
{
#ifdef __BENCHMARK__

#ifdef BM_COUNTERS
    minor_gc	    = 0;
    major_gc	    = 0;
#ifdef HYBRID
    minor_global_gc = 0;
    major_global_gc = 0;
    gc_in_copy	    = 0;
#ifdef INCREMENTAL
    minor_gc_cycles = 0;
    major_gc_cycles = 0;
    minor_gc_stages = 0;
    major_gc_stages = 0;
#endif
#endif
#endif

#ifdef BM_HEAP_SIZES
    max_used_heap	      = 0;
    max_allocated_heap	      = 0;
    max_used_global_heap      = 0;
    max_allocated_global_heap = 0;
#endif

    BIF_RET(am_true);
#else
    BIF_RET(am_false);
#endif
}

BIF_RETTYPE hipe_bifs_pause_times_0(BIF_ALIST_0)
{
#ifdef BM_TIMERS
    int i;
    int total_time = 0, n = 0;
    int left = 0, right = 0, mid = 0;

    printf("Pause times in minor collection:\r\n");
    for (i = 0; i < MAX_PAUSE_TIME; i++) {
	if (pause_times[i] > 0) {
	    printf("%d: %ld\r\n", i, pause_times[i]);
	    total_time += pause_times[i] * i;
	    n += pause_times[i];

	    if (i > mid)
		right += pause_times[i];

	    while (right > left) {
		left += pause_times[mid++];
		right -= pause_times[mid];
	    }
	}
    }

    printf("Number of collections: %d\r\n", n);
    printf("Total collection time: %d\r\n", total_time);
    if (n > 0)
	printf("Mean pause time: %d\r\n", total_time / n);

    printf("Geometrical mean: %d\r\n", mid);

    total_time = 0; n = 0;
    left = 0; right = 0; mid = 0;
    printf("Pause times in major collection:\r\n");
    for (i = 0; i < MAX_PAUSE_TIME; i++) {
	if (pause_times_old[i] > 0) {
	    printf("%d: %ld\r\n", i, pause_times_old[i]);
	    total_time += pause_times_old[i] * i;
	    n += pause_times_old[i];
	}
    }

    printf("Number of collections: %d\r\n", n);
    printf("Total collection time: %d\r\n", total_time);
    if (n > 0)
	printf("Mean pause time: %d\r\n", total_time / n);

    BIF_RET(am_true);
#else
    BIF_RET(am_false);
#endif
}

/* XXX: these macros have free variables */
#ifdef BM_TIMERS
#if USE_PERFCTR
#define MAKE_TIME(_timer_) {			      \
    BM_TIMER_T tmp = _timer_##_time;		      \
    milli = (uint)(tmp - ((int)(tmp / 1000)) * 1000); \
    tmp /= 1000;				      \
    sec = (uint)(tmp - ((int)(tmp / 60)) * 60);	      \
    min = (uint)tmp / 60;			      }

#define MAKE_MICRO_TIME(_timer_) {		      \
    BM_TIMER_T tmp = _timer_##_time * 1000;	      \
    micro = (uint)(tmp - ((int)(tmp / 1000)) * 1000); \
    tmp /= 1000;				      \
    milli = (uint)(tmp - ((int)(tmp / 1000)) * 1000); \
    sec = (uint)tmp / 1000;			      }

#else
#define MAKE_TIME(_timer_) {			      \
    BM_TIMER_T tmp = _timer_##_time / 1000000;	      \
    milli = tmp % 1000;				      \
    tmp /= 1000;				      \
    sec = tmp % 60;				      \
    min = tmp / 60;				      }

#define MAKE_MICRO_TIME(_timer_) {		      \
    BM_TIMER_T tmp = _timer_##_time / 1000;	      \
    micro = tmp % 1000;				      \
    tmp /= 1000;				      \
    milli = tmp % 1000;				      \
    sec = tmp / 1000;				      }

#endif
#else
#define MAKE_TIME(_timer_)
#define MAKE_MICRO_TIME(_timer_)
#endif

BIF_RETTYPE hipe_bifs_system_timer_0(BIF_ALIST_0)
{
#ifdef __BENCHMARK__
    uint min = 0;
    uint sec = 0;
    uint milli = 0;
    Eterm *hp;

    hp = HAlloc(BIF_P, 4);
    MAKE_TIME(system);
    BIF_RET(TUPLE3(hp,
		   make_small(min),
		   make_small(sec),
		   make_small(milli)));
#else
    BIF_RET(am_false);
#endif
}

BIF_RETTYPE hipe_bifs_system_timer_clear_0(BIF_ALIST_0)
{
#ifdef BM_TIMERS
    system_time = 0;
    BIF_RET(am_true);
#else
    BIF_RET(am_false);
#endif
}

BIF_RETTYPE hipe_bifs_send_timer_0(BIF_ALIST_0)
{
#ifdef __BENCHMARK__
    uint min   = 0;
    uint sec   = 0;
    uint milli = 0;
    Eterm *hp;
    Eterm sendtime, copytime, sizetime;

    hp = HAlloc(BIF_P, 4 * 4);

    MAKE_TIME(send);
    sendtime = TUPLE3(hp,
		      make_small(min),
		      make_small(sec),
		      make_small(milli));
    hp += 4;

    MAKE_TIME(copy);
    copytime = TUPLE3(hp,
		      make_small(min),
		      make_small(sec),
		      make_small(milli));
    hp += 4;

    MAKE_TIME(size);
    sizetime = TUPLE3(hp,
		      make_small(min),
		      make_small(sec),
		      make_small(milli));
    hp += 4;
    BIF_RET(TUPLE3(hp, sendtime, copytime, sizetime));
#else
    BIF_RET(am_false);
#endif
}

BIF_RETTYPE hipe_bifs_send_timer_clear_0(BIF_ALIST_0)
{
#ifdef BM_TIMERS
    send_time = 0;
    copy_time = 0;
    size_time = 0;
    BIF_RET(am_true);
#else
    BIF_RET(am_false);
#endif
}

BIF_RETTYPE hipe_bifs_gc_timer_0(BIF_ALIST_0)
{
#ifdef __BENCHMARK__
    Eterm *hp;
    uint min = 0;
    uint sec = 0;
    uint milli = 0;
    uint micro = 0;
    Eterm minor, major, max_min, max_maj;

    hp = HAlloc(BIF_P, 4 * 4 + 5);

    MAKE_TIME(minor_gc);
    minor = TUPLE3(hp,
		   make_small(min),
		   make_small(sec),
		   make_small(milli));
    hp += 4;

    MAKE_TIME(major_gc);
    major = TUPLE3(hp,
		   make_small(min),
		   make_small(sec),
		   make_small(milli));
    hp += 4;

    MAKE_MICRO_TIME(max_minor);
    max_min = TUPLE3(hp,
		     make_small(sec),
		     make_small(milli),
		     make_small(micro));
    hp += 4;

    MAKE_MICRO_TIME(max_major);
    max_maj = TUPLE3(hp,
		     make_small(sec),
		     make_small(milli),
		     make_small(micro));
    hp += 4;

    BIF_RET(TUPLE4(hp, minor, major, max_min, max_maj));
#else
    BIF_RET(am_false);
#endif
}

BIF_RETTYPE hipe_bifs_shared_gc_timer_0(BIF_ALIST_0)
{
#ifdef __BENCHMARK__
    Eterm *hp;
    uint min = 0;
    uint sec = 0;
    uint milli = 0;
    uint micro = 0;
    Eterm minor, major, max_min, max_maj;

    hp = HAlloc(BIF_P, 4 * 4 + 5);

    MAKE_TIME(minor_global_gc);
    minor = TUPLE3(hp,
		   make_small(min),
		   make_small(sec),
		   make_small(milli));
    hp += 4;

    MAKE_TIME(major_global_gc);
    major = TUPLE3(hp,
		   make_small(min),
		   make_small(sec),
		   make_small(milli));
    hp += 4;

    MAKE_MICRO_TIME(max_global_minor);
    max_min = TUPLE3(hp,
		     make_small(sec),
		     make_small(milli),
		     make_small(micro));
    hp += 4;

    MAKE_MICRO_TIME(max_global_major);
    max_maj = TUPLE3(hp,
		     make_small(sec),
		     make_small(milli),
		     make_small(micro));
    hp += 4;

    BIF_RET(TUPLE4(hp, minor, major, max_min, max_maj));
#else
    BIF_RET(am_false);
#endif
}

BIF_RETTYPE hipe_bifs_gc_timer_clear_0(BIF_ALIST_0)
{
#ifdef BM_TIMERS
    minor_gc_time	  = 0;
    major_gc_time	  = 0;
    max_minor_time	  = 0;
    max_major_time	  = 0;
    minor_global_gc_time  = 0;
    major_global_gc_time  = 0;
    max_global_minor_time = 0;
    max_global_major_time = 0;
    BIF_RET(am_true);
#else
    BIF_RET(am_false);
#endif
}

BIF_RETTYPE hipe_bifs_misc_timer_0(BIF_ALIST_0)
{
#ifdef __BENCHMARK__
    uint min   = 0;
    uint sec   = 0;
    uint milli = 0;
    Eterm *hp;
    Eterm misctime1, misctime2, misctime3;

    hp = HAlloc(BIF_P, 4 * 4);

    MAKE_TIME(misc0);
    misctime1 = TUPLE3(hp,
		       make_small(min),
		       make_small(sec),
		       make_small(milli));
    hp += 4;

    MAKE_TIME(misc1);
    misctime2 = TUPLE3(hp,
		       make_small(min),
		       make_small(sec),
		       make_small(milli));
    hp += 4;

    MAKE_TIME(misc2);
    misctime3 = TUPLE3(hp,
		       make_small(min),
		       make_small(sec),
		       make_small(milli));
    hp += 4;
    BIF_RET(TUPLE3(hp, misctime1, misctime2, misctime3));
#else
    BIF_RET(am_false);
#endif
}

BIF_RETTYPE hipe_bifs_misc_timer_clear_0(BIF_ALIST_0)
{
#ifdef BM_TIMERS
    misc0_time = 0;
    misc1_time = 0;
    misc2_time = 0;
    BIF_RET(am_true);
#else
    BIF_RET(am_false);
#endif
}

#undef MAKE_TIME
#undef MAKE_MICRO_TIME

/*
 * HiPE hrvtime().
 * These implementations are currently available:
 * + On Linux with the perfctr extension we can use the process'
 *   virtualised time-stamp counter. To enable this mode you must
 *   pass `--with-perfctr=/path/to/perfctr' when configuring.
 * + The fallback, which is the same as {X,_} = runtime(statistics).
 */

#if USE_PERFCTR

#include "hipe_perfctr.h"
static int hrvtime_is_open;
#define hrvtime_is_started()	hrvtime_is_open

static void start_hrvtime(void)
{
    if (hipe_perfctr_hrvtime_open() >= 0)
	hrvtime_is_open = 1;
}

#define get_hrvtime()		hipe_perfctr_hrvtime_get()
#define stop_hrvtime()		hipe_perfctr_hrvtime_close()

#else

/*
 * Fallback, if nothing better exists.
 * This is the same as {X,_} = statistics(runtime), which uses
 * times(2) on Unix systems.
 */

#define hrvtime_is_started()	1
#define start_hrvtime()		do{}while(0)
#define stop_hrvtime()		do{}while(0)

static double get_hrvtime(void)
{
    unsigned long ms_user;
    elapsed_time_both(&ms_user, NULL, NULL, NULL);
    return (double)ms_user;
}

#endif	/* hrvtime support */

BIF_RETTYPE hipe_bifs_get_hrvtime_0(BIF_ALIST_0)
{
    Eterm *hp;
    Eterm res;
    FloatDef f;

    if (!hrvtime_is_started()) {
	start_hrvtime();
	if (!hrvtime_is_started())
	    BIF_RET(NIL); /* arity 0 BIFs may not fail */
    }
    f.fd = get_hrvtime();
    hp = HAlloc(BIF_P, FLOAT_SIZE_OBJECT);
    res = make_float(hp);
    PUT_DOUBLE(f, hp);
    BIF_RET(res);
}

BIF_RETTYPE hipe_bifs_stop_hrvtime_0(BIF_ALIST_0)
{
    stop_hrvtime();
    BIF_RET(am_true);
}
