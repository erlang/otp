/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2006-2011. All Rights Reserved.
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

#ifndef ERL_TIME_H__
#define ERL_TIME_H__

#if defined(DEBUG) || 0
#define ERTS_TIME_ASSERT(B) ERTS_ASSERT(B)
#else
#define ERTS_TIME_ASSERT(B) ((void) 1)
#endif

typedef struct ErtsTimerWheel_ ErtsTimerWheel;
typedef erts_atomic64_t * ErtsNextTimeoutRef;
extern ErtsTimerWheel *erts_default_timer_wheel;

extern SysTimeval erts_first_emu_time;

/*
** Timer entry:
*/
typedef struct erl_timer {
    struct erl_timer* next;	/* next entry tiw slot or chain */
    struct erl_timer* prev;	/* prev entry tiw slot or chain */
    Uint slot;			/* slot in timer wheel */
    erts_smp_atomic_t wheel;
    ErtsMonotonicTime timeout_pos; /* Timeout in absolute clock ticks */
    /* called when timeout */
    void (*timeout)(void*);
    /* called when cancel (may be NULL) */
    void (*cancel)(void*);
    void* arg;        /* argument to timeout/cancel procs */
} ErlTimer;

typedef void (*ErlTimeoutProc)(void*);
typedef void (*ErlCancelProc)(void*);

#ifdef ERTS_SMP
/*
 * Process and port timer
 */
typedef union ErtsSmpPTimer_ ErtsSmpPTimer;
union ErtsSmpPTimer_ {
    struct {
	ErlTimer tm;
	Eterm id;
	void (*timeout_func)(void*);
	ErtsSmpPTimer **timer_ref;
	Uint32 flags;
    } timer;
    ErtsSmpPTimer *next;
};

void erts_create_smp_ptimer(ErtsSmpPTimer **timer_ref,
			    Eterm id,
			    ErlTimeoutProc timeout_func,
			    Uint timeout);
void erts_cancel_smp_ptimer(ErtsSmpPTimer *ptimer);
#endif

void erts_monitor_time_offset(Eterm id, Eterm ref);
int erts_demonitor_time_offset(Eterm ref);

void erts_early_init_time_sup(void);
void erts_late_init_time_sup(void);

/* timer-wheel api */

ErtsTimerWheel *erts_create_timer_wheel(int);
ErtsNextTimeoutRef erts_get_next_timeout_reference(ErtsTimerWheel *);
void erts_init_time(int time_correction, ErtsTimeWarpMode time_warp_mode);
void erts_set_timer(ErlTimer*, ErlTimeoutProc, ErlCancelProc, void*, Uint);
void erts_cancel_timer(ErlTimer*);
Uint erts_time_left(ErlTimer *);
void erts_bump_timers(ErtsTimerWheel *, ErtsMonotonicTime);
Uint erts_timer_wheel_memory_size(void);

#ifdef DEBUG
void erts_p_slpq(void);
#endif

ErtsMonotonicTime erts_check_next_timeout_time(ErtsTimerWheel *,
					       ErtsMonotonicTime);

ERTS_GLB_INLINE void erts_init_timer(ErlTimer *p);
ERTS_GLB_INLINE ErtsMonotonicTime erts_next_timeout_time(ErtsNextTimeoutRef);

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

ERTS_GLB_INLINE void erts_init_timer(ErlTimer *p)
{
    erts_smp_atomic_init_nob(&p->wheel, (erts_aint_t) NULL);
}

ERTS_GLB_INLINE ErtsMonotonicTime erts_next_timeout_time(ErtsNextTimeoutRef nxt_tmo_ref)
{
    return (ErtsMonotonicTime) erts_atomic64_read_acqb((erts_atomic64_t *) nxt_tmo_ref);
}

#endif /* #if ERTS_GLB_INLINE_INCL_FUNC_DEF */


/* time_sup */

#if (defined(HAVE_GETHRVTIME) || defined(HAVE_CLOCK_GETTIME))
#  ifndef HAVE_ERTS_NOW_CPU
#    define HAVE_ERTS_NOW_CPU
#    ifdef HAVE_GETHRVTIME
#      define erts_start_now_cpu() sys_start_hrvtime()
#      define erts_stop_now_cpu()  sys_stop_hrvtime()
#    endif
#  endif
void erts_get_now_cpu(Uint* megasec, Uint* sec, Uint* microsec);
#endif

typedef UWord erts_approx_time_t;
erts_approx_time_t erts_get_approx_time(void);

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
Eterm erts_monotonic_time_source(struct process*c_p);

#ifdef SYS_CLOCK_RESOLUTION
#define ERTS_CLKTCK_RESOLUTION ((ErtsMonotonicTime) (SYS_CLOCK_RESOLUTION*1000))
#else
#define ERTS_CLKTCK_RESOLUTION (erts_time_sup__.r.o.clktck_resolution)
#endif

struct erts_time_sup_read_only__ {
    ErtsMonotonicTime monotonic_time_unit;
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

ERTS_GLB_INLINE Uint64
erts_time_unit_conversion(Uint64 value,
			  Uint32 from_time_unit,
			  Uint32 to_time_unit);

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

#if ERTS_COMPILE_TIME_MONOTONIC_TIME_UNIT

/*
 * If the monotonic time unit is a compile time constant,
 * it is assumed (and need) to be a power of 10.
 */

#define ERTS_MONOTONIC_TIME_UNIT \
    ((ErtsMonotonicTime) ERTS_COMPILE_TIME_MONOTONIC_TIME_UNIT)

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

#elif ERTS_COMPILE_TIME_MONOTONIC_TIME_UNIT == 1000
/* Milli-second time unit */

#define ERTS_MONOTONIC_TO_SEC__(MSEC) ((USEC)/(1000))
#define ERTS_MONOTONIC_TO_MSEC__(MSEC) (MSEC)
#define ERTS_MONOTONIC_TO_USEC__(MSEC) ((MSEC)*1000)
#define ERTS_MONOTONIC_TO_NSEC__(MSEC) ((MSEC)*(1000*1000))

#define ERTS_SEC_TO_MONOTONIC__(SEC) (((ErtsMonotonicTime) (SEC))*1000)
#define ERTS_MSEC_TO_MONOTONIC__(MSEC) ((ErtsMonotonicTime) (MSEC))
#define ERTS_USEC_TO_MONOTONIC__(USEC) (((ErtsMonotonicTime) (USEC))/1000)
#define ERTS_NSEC_TO_MONOTONIC__(NSEC) (((ErtsMonotonicTime) (NSEC))/(1000*1000))

#else
#error Missing implementation for monotonic time unit
#endif

#define ERTS_MONOTONIC_TO_CLKTCKS__(MON) \
    ((MON) / (ERTS_MONOTONIC_TIME_UNIT/ERTS_CLKTCK_RESOLUTION))
#define ERTS_CLKTCKS_TO_MONOTONIC__(TCKS) \
    ((TCKS) * (ERTS_MONOTONIC_TIME_UNIT/ERTS_CLKTCK_RESOLUTION))

#else /* !ERTS_COMPILE_TIME_MONOTONIC_TIME_UNIT */

#define ERTS_MONOTONIC_TIME_UNIT (erts_time_sup__.r.o.monotonic_time_unit)

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
