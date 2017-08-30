/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2004-2016. All Rights Reserved.
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
#include "erl_mtrace.h"

#ifdef ERTS_CAN_TRACK_MALLOC
#if defined(HAVE_END_SYMBOL)
extern char end;
#elif defined(HAVE__END_SYMBOL)
extern char _end;
#endif

static int inited = 0;
static int init(void);

static volatile char *heap_start = NULL;
static volatile char *heap_end = NULL;

#if defined(ERTS___AFTER_MORECORE_HOOK_CAN_TRACK_MALLOC) /* ----------------- */

#ifdef HAVE_MALLOC_H
#  include <malloc.h>
#endif

#undef SBRK_0
#define SBRK_0 sbrk(0)

static void
init_hook(void)
{
    __after_morecore_hook = erts_mtrace_update_heap_size;
    if (inited)
	return;
    heap_end = NULL;
#if defined(HAVE_END_SYMBOL)
    heap_start = &end;
#elif defined(HAVE__END_SYMBOL)
    heap_start = &_end;
#else
    heap_start = SBRK_0;
    if (heap_start == (SBRK_RET_TYPE) -1) {
	heap_start = NULL;
	return;
    }
#endif
    inited = 1;
}

static int
init(void)
{
    init_hook();
    return inited;
}

void (*__malloc_initialize_hook)(void) = init_hook;

#elif defined(ERTS_BRK_WRAPPERS_CAN_TRACK_MALLOC) /* ------------------------ */
#ifdef HAVE_DLFCN_H
#  include <dlfcn.h>
#endif

#undef SBRK_0
#define SBRK_0 (*real_sbrk)(0)

#ifndef HAVE_SBRK
#  error no sbrk()
#endif
#if !defined(HAVE_END_SYMBOL) && !defined(HAVE__END_SYMBOL)
#  error no 'end' nor '_end'
#endif

static void update_heap_size(char *new_end);

#define SBRK_IMPL(RET_TYPE, FUNC, ARG_TYPE)			\
RET_TYPE FUNC (ARG_TYPE);					\
static RET_TYPE (*real_ ## FUNC)(ARG_TYPE) = NULL;		\
RET_TYPE FUNC (ARG_TYPE arg)					\
{								\
    RET_TYPE res;						\
    if (!inited && !init())					\
	return (RET_TYPE) -1;					\
    res = (*real_ ## FUNC)(arg);				\
    if (erts_mtrace_enabled && res != ((RET_TYPE) -1))		\
	update_heap_size((char *) (*real_ ## FUNC)(0));		\
    return res;							\
}

#define BRK_IMPL(RET_TYPE, FUNC, ARG_TYPE)			\
RET_TYPE FUNC (ARG_TYPE);					\
static RET_TYPE (*real_ ## FUNC)(ARG_TYPE) = NULL;		\
RET_TYPE FUNC (ARG_TYPE arg)					\
{								\
    RET_TYPE res;						\
    if (!inited && !init())					\
	return (RET_TYPE) -1;					\
    res = (*real_ ## FUNC)(arg);				\
    if (erts_mtrace_enabled && res != ((RET_TYPE) -1))		\
	update_heap_size((char *) arg);				\
    return res;							\
}

SBRK_IMPL(SBRK_RET_TYPE, sbrk, SBRK_ARG_TYPE)
#ifdef HAVE_BRK
   BRK_IMPL(BRK_RET_TYPE, brk, BRK_ARG_TYPE)
#endif

#ifdef HAVE__SBRK
   SBRK_IMPL(SBRK_RET_TYPE, _sbrk, SBRK_ARG_TYPE)
#endif
#ifdef HAVE__BRK
   BRK_IMPL(BRK_RET_TYPE, _brk, BRK_ARG_TYPE)
#endif

#ifdef HAVE___SBRK
   SBRK_IMPL(SBRK_RET_TYPE, __sbrk, SBRK_ARG_TYPE)
#endif
#ifdef HAVE___BRK
   BRK_IMPL(BRK_RET_TYPE, __brk, BRK_ARG_TYPE)
#endif

static int
init(void)
{
    if (inited)
	return 1;

#define INIT_XBRK_SYM(SYM)			\
do {						\
    if (!real_ ## SYM) {			\
	real_ ## SYM = dlsym(RTLD_NEXT, #SYM);	\
	if (!real_ ## SYM) {			\
	    errno = ENOMEM;			\
	    return 0;				\
	}					\
    }						\
} while (0)

    heap_end = NULL;
#if defined(HAVE_END_SYMBOL)
    heap_start = &end;
#elif defined(HAVE__END_SYMBOL)
    heap_start = &_end;
#endif

    INIT_XBRK_SYM(sbrk);
#ifdef HAVE_BRK
    INIT_XBRK_SYM(brk);
#endif
#ifdef HAVE__SBRK
    INIT_XBRK_SYM(_sbrk);
#endif
#ifdef HAVE__BRK
    INIT_XBRK_SYM(_brk);
#endif
#ifdef HAVE___SBRK
    INIT_XBRK_SYM(__sbrk);
#endif
#ifdef HAVE___BRK
    INIT_XBRK_SYM(__brk);
#endif

    return inited = 1;
#undef INIT_XBRK_SYM
}

#endif /* #elif defined(ERTS_BRK_WRAPPERS_CAN_TRACK_MALLOC) */ /* ----------- */

static void
update_heap_size(char *new_end)
{
    volatile char *new_start, *old_start, *old_end;
    Uint size;

    if (new_end == ((char *) -1))
	return;

    new_start = (old_start = heap_start);
    old_end = heap_end;
    heap_end = new_end;
    if (new_end < old_start || !old_start)
	heap_start = (new_start = new_end);

    size = (Uint) (new_end - new_start);

    if (!old_end) {
	if (size)
	    erts_mtrace_crr_alloc((void *) new_start,
				  ERTS_ALC_A_SYSTEM,
				  ERTS_MTRACE_SEGMENT_ID,
				  size);
	else
	    heap_end = NULL;
    }
    else {
	if (old_end != new_end || old_start != new_start) {

	    if (size)
		erts_mtrace_crr_realloc((void *) new_start,
					ERTS_ALC_A_SYSTEM,
					ERTS_MTRACE_SEGMENT_ID,
					(void *) old_start,
					size);
	    else {
		if (old_start)
		    erts_mtrace_crr_free(ERTS_ALC_A_SYSTEM,
					 ERTS_MTRACE_SEGMENT_ID,
					 (void *) old_start);
		heap_end = NULL;
	    }
	}
    }
}

#endif /* #ifdef ERTS_CAN_TRACK_MALLOC */

void
erts_mtrace_update_heap_size(void)
{
#ifdef ERTS_CAN_TRACK_MALLOC
    if (erts_mtrace_enabled && (inited || init()))
	update_heap_size((char *) SBRK_0);
#endif
}

