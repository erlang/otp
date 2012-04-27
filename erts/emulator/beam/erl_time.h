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

#define ERTS_SHORT_TIME_T_MAX ERTS_AINT32_T_MAX
#define ERTS_SHORT_TIME_T_MIN ERTS_AINT32_T_MIN
typedef erts_aint32_t erts_short_time_t;

extern erts_smp_atomic32_t do_time;	/* set at clock interrupt */
extern SysTimeval erts_first_emu_time;

/*
** Timer entry:
*/
typedef struct erl_timer {
    struct erl_timer* next;	/* next entry tiw slot or chain */
    struct erl_timer* prev;	/* prev entry tiw slot or chain */
    Uint slot;			/* slot in timer wheel */
    Uint count;			/* number of loops remaining */
    int    active;		/* 1=activated, 0=deactivated */
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

/* timer-wheel api */

void erts_init_time(void);
void erts_set_timer(ErlTimer*, ErlTimeoutProc, ErlCancelProc, void*, Uint);
void erts_cancel_timer(ErlTimer*);
void erts_bump_timer(erts_short_time_t);
Uint erts_timer_wheel_memory_size(void);
Uint erts_time_left(ErlTimer *);
erts_short_time_t erts_next_time(void);

#ifdef DEBUG
void erts_p_slpq(void);
#endif

ERTS_GLB_INLINE erts_short_time_t erts_do_time_read_and_reset(void);
ERTS_GLB_INLINE void erts_do_time_add(erts_short_time_t);

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

ERTS_GLB_INLINE erts_short_time_t erts_do_time_read_and_reset(void)
{
    erts_short_time_t time = erts_smp_atomic32_xchg_acqb(&do_time, 0);
    if (time < 0)
	erl_exit(ERTS_ABORT_EXIT, "Internal time management error\n");
    return time;
}

ERTS_GLB_INLINE void erts_do_time_add(erts_short_time_t elapsed)
{
    erts_smp_atomic32_add_relb(&do_time, elapsed);
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

void erts_get_timeval(SysTimeval *tv);
erts_time_t erts_get_time(void);

ERTS_GLB_INLINE int erts_cmp_timeval(SysTimeval *t1p, SysTimeval *t2p);

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

ERTS_GLB_INLINE int
erts_cmp_timeval(SysTimeval *t1p, SysTimeval *t2p)
{
    if (t1p->tv_sec == t2p->tv_sec) {
	if (t1p->tv_usec < t2p->tv_usec)
	    return -1;
	else if (t1p->tv_usec > t2p->tv_usec)
	    return 1;
	return 0;
    }
    return t1p->tv_sec < t2p->tv_sec ? -1 : 1;
}

#endif /* #if ERTS_GLB_INLINE_INCL_FUNC_DEF */
#endif /* ERL_TIME_H__ */
