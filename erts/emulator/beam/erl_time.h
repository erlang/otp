/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2006-2018. All Rights Reserved.
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

#ifndef ERL_TIME_H__
#define ERL_TIME_H__

#include "erl_monitor_link.h"

#if 0
#  define ERTS_TW_DEBUG
#endif
#if defined(DEBUG) && !defined(ERTS_TW_DEBUG)
#  define ERTS_TW_DEBUG
#endif

#if defined(ERTS_TW_DEBUG)
#define ERTS_TIME_ASSERT(B) ERTS_ASSERT(B)
#else
#define ERTS_TIME_ASSERT(B) ((void) 1)
#endif

#ifdef ERTS_TW_DEBUG
/*
 * Soon wheel will handle about 1 seconds
 * Later wheel will handle about 8 minutes
 */
#  define ERTS_TW_SOON_WHEEL_BITS 10
#  define ERTS_TW_LATER_WHEEL_BITS 10
#else
#  ifdef SMALL_MEMORY
/*
 * Soon wheel will handle about 4 seconds
 * Later wheel will handle about 2 hours and 19 minutes
 */
#    define ERTS_TW_SOON_WHEEL_BITS 12
#    define ERTS_TW_LATER_WHEEL_BITS 12
#  else
/*
 * Soon wheel will handle about 16 seconds
 * Later wheel will handle about 37 hours and 16 minutes
 */
#    define ERTS_TW_SOON_WHEEL_BITS 14
#    define ERTS_TW_LATER_WHEEL_BITS 14
#  endif
#endif

/*
 * Number of slots in each timer wheel...
 *
 * These *need* to be a power of 2
 */
#define ERTS_TW_SOON_WHEEL_SIZE (1 << ERTS_TW_SOON_WHEEL_BITS)
#define ERTS_TW_LATER_WHEEL_SIZE (1 << ERTS_TW_LATER_WHEEL_BITS)

typedef enum {
    ERTS_NO_TIME_WARP_MODE,
    ERTS_SINGLE_TIME_WARP_MODE,
    ERTS_MULTI_TIME_WARP_MODE
} ErtsTimeWarpMode;

typedef struct ErtsTimerWheel_ ErtsTimerWheel;
typedef ErtsMonotonicTime * ErtsNextTimeoutRef;

extern SysTimeval erts_first_emu_time;


void erts_monitor_time_offset(ErtsMonitor *mon);
void erts_demonitor_time_offset(ErtsMonitor *mon);

int erts_init_time_sup(int, ErtsTimeWarpMode);
void erts_late_init_time_sup(void);

ErtsNextTimeoutRef erts_get_next_timeout_reference(ErtsTimerWheel *);
void erts_init_time(int time_correction, ErtsTimeWarpMode time_warp_mode);
void erts_bump_timers(ErtsTimerWheel *, ErtsMonotonicTime);
Uint erts_timer_wheel_memory_size(void);

#ifdef DEBUG
void erts_p_slpq(void);
#endif

/* time_sup */

#if (defined(HAVE_GETHRVTIME) || defined(HAVE_CLOCK_GETTIME_CPU_TIME))
#  ifndef HAVE_ERTS_NOW_CPU
#    define HAVE_ERTS_NOW_CPU
#    ifdef HAVE_GETHRVTIME
#      define erts_start_now_cpu() sys_start_hrvtime()
#      define erts_stop_now_cpu()  sys_stop_hrvtime()
#    endif
#  endif
void erts_get_now_cpu(Uint* megasec, Uint* sec, Uint* microsec);
#endif

int erts_has_time_correction(void);
int erts_check_time_adj_support(int time_correction,
				ErtsTimeWarpMode time_warp_mode);

ErtsTimeWarpMode erts_time_warp_mode(void);

typedef enum {
    ERTS_TIME_OFFSET_PRELIMINARY,
    ERTS_TIME_OFFSET_FINAL,
    ERTS_TIME_OFFSET_VOLATILE
} ErtsTimeOffsetState;

ErtsTimeOffsetState erts_time_offset_state(void); 
ErtsTimeOffsetState erts_finalize_time_offset(void); 
struct process;
Eterm erts_get_monotonic_start_time(struct process *c_p);
Eterm erts_get_monotonic_end_time(struct process *c_p);
Eterm erts_monotonic_time_source(struct process*c_p);
Eterm erts_system_time_source(struct process*c_p);

void erts_runtime_elapsed_both(ErtsMonotonicTime *ms_user,
                               ErtsMonotonicTime *ms_sys, 
                               ErtsMonotonicTime *ms_user_diff,
                               ErtsMonotonicTime *ms_sys_diff);
void erts_wall_clock_elapsed_both(ErtsMonotonicTime *total,
                                  ErtsMonotonicTime *diff);

#ifdef SYS_CLOCK_RESOLUTION
#define ERTS_CLKTCK_RESOLUTION ((ErtsMonotonicTime) (SYS_CLOCK_RESOLUTION*1000))
#else
#define ERTS_CLKTCK_RESOLUTION (erts_time_sup__.r.o.clktck_resolution)
#endif

#define ERTS_TW_SOON_WHEEL_MSEC (ERTS_TW_SOON_WHEEL_SIZE/(ERTS_CLKTCK_RESOLUTION/1000))
#define ERTS_TW_LATER_WHEEL_MSEC (ERTS_TW_LATER_WHEEL_SIZE*ERTS_TW_SOON_WHEEL_MSEC/2)

#define ERTS_TIMER_WHEEL_MSEC ERTS_TW_LATER_WHEEL_MSEC

struct erts_time_sup_read_only__ {
    ErtsMonotonicTime monotonic_time_unit;
#if !ERTS_COMPILE_TIME_MONOTONIC_TIME_UNIT
    ErtsMonotonicTime start;
    struct {
	ErtsMonotonicTime native;
	ErtsMonotonicTime nsec;
	ErtsMonotonicTime usec;
	ErtsMonotonicTime msec;
	ErtsMonotonicTime sec;
    } start_offset;
#endif
#ifndef SYS_CLOCK_RESOLUTION
    ErtsMonotonicTime clktck_resolution;
#endif
};

typedef struct {
    union {
	struct erts_time_sup_read_only__ o;
	char align__[(((sizeof(struct erts_time_sup_read_only__) - 1)
		       / ASSUMED_CACHE_LINE_SIZE) + 1)
		     * ASSUMED_CACHE_LINE_SIZE];
    } r;
} ErtsTimeSupData;

extern ErtsTimeSupData erts_time_sup__;

ErtsMonotonicTime erts_napi_monotonic_time(int time_unit);
ErtsMonotonicTime erts_napi_time_offset(int time_unit);
ErtsMonotonicTime erts_napi_convert_time_unit(ErtsMonotonicTime val, int from, int to);

ERTS_GLB_INLINE Uint64
erts_time_unit_conversion(Uint64 value,
			  Uint32 from_time_unit,
			  Uint32 to_time_unit);

ErtsSysPerfCounter erts_perf_counter_unit(void);

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

ERTS_GLB_INLINE Uint64
erts_time_unit_conversion(Uint64 value,
			  Uint32 from_time_unit,
			  Uint32 to_time_unit)
{
    Uint64 high, low, result;
    if (value <= ~((Uint64) 0)/to_time_unit)
	return (value*to_time_unit)/from_time_unit;

    low = value & ((Uint64) 0xffffffff);
    high = (value >> 32) & ((Uint64) 0xffffffff);

    low *= to_time_unit;
    high *= to_time_unit;

    high += (low >> 32) & ((Uint64) 0xffffffff);
    low &= ((Uint64) 0xffffffff);

    result = high % from_time_unit;
    high /= from_time_unit;
    high <<= 32;

    result <<= 32;
    result += low;
    result /= from_time_unit;
    result += high;

    return result;
}

#endif /* ERTS_GLB_INLINE_INCL_FUNC_DEF */

/*
 * Range of monotonic time internally
 */

#define ERTS_MONOTONIC_BEGIN						\
    ERTS_MONOTONIC_TIME_UNIT
#define ERTS_MONOTONIC_END						\
    ((ERTS_MONOTONIC_TIME_MAX / ERTS_MONOTONIC_TIME_UNIT)		\
     * ERTS_MONOTONIC_TIME_UNIT)

#if ERTS_COMPILE_TIME_MONOTONIC_TIME_UNIT

/*
 * If the monotonic time unit is a compile time constant,
 * it is assumed (and need) to be a power of 10.
 */

#if ERTS_COMPILE_TIME_MONOTONIC_TIME_UNIT < 1000*1000
#  error Compile time time unit needs to be at least 1000000
#endif

#if ERTS_COMPILE_TIME_MONOTONIC_TIME_UNIT == 1000*1000*1000
/* Nano-second time unit */

#define ERTS_MONOTONIC_TO_SEC__(NSEC) ((NSEC) / (1000*1000*1000))
#define ERTS_MONOTONIC_TO_MSEC__(NSEC) ((NSEC) / (1000*1000))
#define ERTS_MONOTONIC_TO_USEC__(NSEC) ((NSEC) / 1000)
#define ERTS_MONOTONIC_TO_NSEC__(NSEC) (NSEC)

#define ERTS_SEC_TO_MONOTONIC__(SEC) (((ErtsMonotonicTime) (SEC))*(1000*1000*1000))
#define ERTS_MSEC_TO_MONOTONIC__(MSEC) (((ErtsMonotonicTime) (MSEC))*(1000*1000))
#define ERTS_USEC_TO_MONOTONIC__(USEC) (((ErtsMonotonicTime) (USEC))*1000)
#define ERTS_NSEC_TO_MONOTONIC__(NSEC) ((ErtsMonotonicTime) (NSEC))

#elif ERTS_COMPILE_TIME_MONOTONIC_TIME_UNIT == 1000*1000
/* Micro-second time unit */

#define ERTS_MONOTONIC_TO_SEC__(USEC) ((USEC) / (1000*1000))
#define ERTS_MONOTONIC_TO_MSEC__(USEC) ((USEC) / 1000)
#define ERTS_MONOTONIC_TO_USEC__(USEC) (USEC)
#define ERTS_MONOTONIC_TO_NSEC__(USEC) ((USEC)*1000)

#define ERTS_SEC_TO_MONOTONIC__(SEC) (((ErtsMonotonicTime) (SEC))*(1000*1000))
#define ERTS_MSEC_TO_MONOTONIC__(MSEC) (((ErtsMonotonicTime) (MSEC))*1000)
#define ERTS_USEC_TO_MONOTONIC__(USEC) ((ErtsMonotonicTime) (USEC))
#define ERTS_NSEC_TO_MONOTONIC__(NSEC) (((ErtsMonotonicTime) (NSEC))/1000)

#else
#error Missing implementation for monotonic time unit
#endif

#define ERTS_MONOTONIC_TIME_UNIT \
    ((ErtsMonotonicTime) ERTS_COMPILE_TIME_MONOTONIC_TIME_UNIT)

/*
 * NOTE! ERTS_MONOTONIC_TIME_START_EXTERNAL *need* to be a multiple
 *       of ERTS_MONOTONIC_TIME_UNIT.
 */

#ifdef ARCH_32
/*
 * Want to use a big-num of arity 2 as long as possible (584 years
 * in the nano-second time unit case).
 */
#define ERTS_MONOTONIC_TIME_START_EXTERNAL		\
    (((((((ErtsMonotonicTime) 1) << 32)-1)	\
       / ERTS_MONOTONIC_TIME_UNIT)		\
      * ERTS_MONOTONIC_TIME_UNIT)		\
     + ERTS_MONOTONIC_TIME_UNIT)

#else /* ARCH_64 */

#if ERTS_COMPILE_TIME_MONOTONIC_TIME_UNIT <= 10*1000*1000

/*
 * Using micro second time unit or lower. Start at zero since
 * time will remain an immediate for a very long time anyway
 * (1827 years in the 10 micro second case)...
 */
#define ERTS_MONOTONIC_TIME_START_EXTERNAL ((ErtsMonotonicTime) 0)

#else /* ERTS_COMPILE_TIME_MONOTONIC_TIME_UNIT > 10*1000*1000 */

/*
 * Want to use an immediate as long as possible (36 years in the
 * nano-second time unit case).
*/
#define ERTS_MONOTONIC_TIME_START_EXTERNAL 		\
    ((((ErtsMonotonicTime) MIN_SMALL)		\
      / ERTS_MONOTONIC_TIME_UNIT)		\
     * ERTS_MONOTONIC_TIME_UNIT)

#endif /* ERTS_COMPILE_TIME_MONOTONIC_TIME_UNIT > 1000*1000 */

#endif /* ARCH_64 */

/*
 * Offsets from internal monotonic time to external monotonic time
 */

#define ERTS_MONOTONIC_OFFSET_NATIVE \
    (ERTS_MONOTONIC_TIME_START_EXTERNAL - ERTS_MONOTONIC_BEGIN)
#define ERTS_MONOTONIC_OFFSET_NSEC					\
    ERTS_MONOTONIC_TO_NSEC__(ERTS_MONOTONIC_OFFSET_NATIVE)
#define ERTS_MONOTONIC_OFFSET_USEC					\
    ERTS_MONOTONIC_TO_USEC__(ERTS_MONOTONIC_OFFSET_NATIVE)
#define ERTS_MONOTONIC_OFFSET_MSEC					\
    ERTS_MONOTONIC_TO_MSEC__(ERTS_MONOTONIC_OFFSET_NATIVE)
#define ERTS_MONOTONIC_OFFSET_SEC					\
    ERTS_MONOTONIC_TO_SEC__(ERTS_MONOTONIC_OFFSET_NATIVE)

#define ERTS_MONOTONIC_TO_CLKTCKS__(MON) \
    ((MON) / (ERTS_MONOTONIC_TIME_UNIT/ERTS_CLKTCK_RESOLUTION))
#define ERTS_CLKTCKS_TO_MONOTONIC__(TCKS) \
    ((TCKS) * (ERTS_MONOTONIC_TIME_UNIT/ERTS_CLKTCK_RESOLUTION))

#else /* !ERTS_COMPILE_TIME_MONOTONIC_TIME_UNIT */

/*
 * Initialized in erts_init_sys_time_sup()
 */
#define ERTS_MONOTONIC_TIME_UNIT (erts_time_sup__.r.o.monotonic_time_unit)

/*
 * Offsets from internal monotonic time to external monotonic time
 *
 * Initialized in erts_init_time_sup()...
 */
#define ERTS_MONOTONIC_TIME_START_EXTERNAL (erts_time_sup__.r.o.start)
#define ERTS_MONOTONIC_OFFSET_NATIVE (erts_time_sup__.r.o.start_offset.native)
#define ERTS_MONOTONIC_OFFSET_NSEC (erts_time_sup__.r.o.start_offset.nsec)
#define ERTS_MONOTONIC_OFFSET_USEC (erts_time_sup__.r.o.start_offset.usec)
#define ERTS_MONOTONIC_OFFSET_MSEC (erts_time_sup__.r.o.start_offset.msec)
#define ERTS_MONOTONIC_OFFSET_SEC (erts_time_sup__.r.o.start_offset.sec)

#define ERTS_CONV_FROM_MON_UNIT___(M, TO)				\
    ((ErtsMonotonicTime)						\
     erts_time_unit_conversion((Uint64) (M),				\
			       (Uint32) ERTS_MONOTONIC_TIME_UNIT,	\
			       (Uint32) (TO)))

#define ERTS_CONV_TO_MON_UNIT___(M, FROM)				\
    ((ErtsMonotonicTime)						\
     erts_time_unit_conversion((Uint64) (M),				\
			       (Uint32) (FROM),				\
			       (Uint32) ERTS_MONOTONIC_TIME_UNIT))	\

#define ERTS_MONOTONIC_TO_SEC__(M) \
    ERTS_CONV_FROM_MON_UNIT___((M), 1)
#define ERTS_MONOTONIC_TO_MSEC__(M) \
    ERTS_CONV_FROM_MON_UNIT___((M), 1000)
#define ERTS_MONOTONIC_TO_USEC__(M) \
    ERTS_CONV_FROM_MON_UNIT___((M), 1000*1000)
#define ERTS_MONOTONIC_TO_NSEC__(M) \
    ERTS_CONV_FROM_MON_UNIT___((M), 1000*1000*1000)

#define ERTS_SEC_TO_MONOTONIC__(SEC) \
    ERTS_CONV_TO_MON_UNIT___((SEC), 1)
#define ERTS_MSEC_TO_MONOTONIC__(MSEC) \
    ERTS_CONV_TO_MON_UNIT___((MSEC), 1000)
#define ERTS_USEC_TO_MONOTONIC__(USEC) \
    ERTS_CONV_TO_MON_UNIT___((USEC), 1000*1000)
#define ERTS_NSEC_TO_MONOTONIC__(NSEC) \
    ERTS_CONV_TO_MON_UNIT___((NSEC), 1000*1000*1000)

#define ERTS_MONOTONIC_TO_CLKTCKS__(MON) \
    ERTS_CONV_FROM_MON_UNIT___((MON), ERTS_CLKTCK_RESOLUTION)
#define ERTS_CLKTCKS_TO_MONOTONIC__(TCKS) \
    ERTS_CONV_TO_MON_UNIT___((TCKS), ERTS_CLKTCK_RESOLUTION)

#endif /* !ERTS_COMPILE_TIME_MONOTONIC_TIME_UNIT */

#define ERTS_MONOTONIC_TIME_END_EXTERNAL				\
    (ERTS_MONOTONIC_TIME_START_EXTERNAL < 0				\
     ? (ERTS_MONOTONIC_TIME_START_EXTERNAL				\
	+ (ERTS_MONOTONIC_END - ERTS_MONOTONIC_BEGIN))			\
     : (ERTS_MONOTONIC_END - ERTS_MONOTONIC_TIME_START_EXTERNAL))

#define ERTS_MSEC_TO_CLKTCKS__(MON) \
    ((MON) * (ERTS_CLKTCK_RESOLUTION/1000))
#define ERTS_CLKTCKS_TO_MSEC__(TCKS) \
    ((TCKS) / (ERTS_CLKTCK_RESOLUTION/1000))

#define ERTS_MONOTONIC_TO_SEC(X)	\
    (ERTS_TIME_ASSERT((X) >= 0),	\
     ERTS_MONOTONIC_TO_SEC__((X)))
#define ERTS_MONOTONIC_TO_MSEC(X)	\
    (ERTS_TIME_ASSERT((X) >= 0),	\
     ERTS_MONOTONIC_TO_MSEC__((X)))
#define ERTS_MONOTONIC_TO_USEC(X)	\
    (ERTS_TIME_ASSERT((X) >= 0),	\
     ERTS_MONOTONIC_TO_USEC__((X)))
#define ERTS_MONOTONIC_TO_NSEC(X)	\
    (ERTS_TIME_ASSERT((X) >= 0),	\
     ERTS_MONOTONIC_TO_NSEC__((X)))
#define ERTS_SEC_TO_MONOTONIC(X)	\
    (ERTS_TIME_ASSERT((X) >= 0),	\
     ERTS_SEC_TO_MONOTONIC__((X)))
#define ERTS_MSEC_TO_MONOTONIC(X)	\
    (ERTS_TIME_ASSERT((X) >= 0),	\
     ERTS_MSEC_TO_MONOTONIC__((X)))
#define ERTS_USEC_TO_MONOTONIC(X)	\
    (ERTS_TIME_ASSERT((X) >= 0),	\
     ERTS_USEC_TO_MONOTONIC__((X)))
#define ERTS_NSEC_TO_MONOTONIC(X)	\
    (ERTS_TIME_ASSERT((X) >= 0),	\
     ERTS_NSEC_TO_MONOTONIC__((X)))

#define ERTS_MONOTONIC_TO_CLKTCKS(X) \
    (ERTS_TIME_ASSERT((X) >= 0),		\
     ERTS_MONOTONIC_TO_CLKTCKS__((X)))
#define ERTS_CLKTCKS_TO_MONOTONIC(X) \
    (ERTS_TIME_ASSERT((X) >= 0),		\
     ERTS_CLKTCKS_TO_MONOTONIC__((X)))

#define ERTS_MSEC_TO_CLKTCKS(X) \
    (ERTS_TIME_ASSERT((X) >= 0),		\
     ERTS_MSEC_TO_CLKTCKS__((X)))
#define ERTS_CLKTCKS_TO_MSEC(X) \
    (ERTS_TIME_ASSERT((X) >= 0),		\
     ERTS_CLKTCKS_TO_MSEC__((X)))

#endif /* ERL_TIME_H__ */

/* timer-wheel api */
#if defined(ERTS_WANT_TIMER_WHEEL_API) && !defined(ERTS_GOT_TIMER_WHEEL_API)
#define ERTS_GOT_TIMER_WHEEL_API

#include "erl_thr_progress.h"
#include "erl_process.h"

void erts_sched_init_time_sup(ErtsSchedulerData *esdp);


#define ERTS_TW_SLOT_INACTIVE (-2)

/*
** Timer entry:
*/
typedef struct erl_timer {
    ErtsMonotonicTime timeout_pos; /* Timeout in absolute clock ticks */
    struct erl_timer* next;     /* next entry tiw slot or chain */
    struct erl_timer* prev;	/* prev entry tiw slot or chain */
    void (*timeout)(void*); /* called when timeout */
    void* arg;              /* argument to timeout/cancel procs */
    int slot;
} ErtsTWheelTimer;

typedef void (*ErlTimeoutProc)(void*);

void erts_twheel_set_timer(ErtsTimerWheel *tiw,
			   ErtsTWheelTimer *p, ErlTimeoutProc timeout,
			   void *arg, ErtsMonotonicTime timeout_pos);
void erts_twheel_cancel_timer(ErtsTimerWheel *tiw, ErtsTWheelTimer *p);
ErtsTimerWheel *erts_create_timer_wheel(ErtsSchedulerData *esdp);

ErtsMonotonicTime erts_check_next_timeout_time(ErtsSchedulerData *);

ERTS_GLB_INLINE void erts_twheel_init_timer(ErtsTWheelTimer *p);
ERTS_GLB_INLINE ErtsMonotonicTime erts_next_timeout_time(ErtsNextTimeoutRef);
ERTS_GLB_INLINE ErtsMonotonicTime erts_tweel_read_timeout(ErtsTWheelTimer *twt);

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

ERTS_GLB_INLINE void erts_twheel_init_timer(ErtsTWheelTimer *p)
{
    p->slot = ERTS_TW_SLOT_INACTIVE;
}

ERTS_GLB_INLINE ErtsMonotonicTime erts_next_timeout_time(ErtsNextTimeoutRef nxt_tmo_ref)
{
    return *((ErtsMonotonicTime *) nxt_tmo_ref);
}

ERTS_GLB_INLINE ErtsMonotonicTime
erts_tweel_read_timeout(ErtsTWheelTimer *twt)
{
    return twt->timeout_pos;
}

#endif /* ERTS_GLB_INLINE_INCL_FUNC_DEF */

void
erts_twheel_debug_foreach(ErtsTimerWheel *tiw,
			  void (*tclbk)(void *),
			  void (*func)(void *,
				       ErtsMonotonicTime,
				       void *),
			  void *arg);

#endif /* timer wheel api */
