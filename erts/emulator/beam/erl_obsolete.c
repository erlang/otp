/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2004-2009. All Rights Reserved.
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
#include "erl_driver.h"

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 *                                                                           *
 * ------------------------- OBSOLETE! DO NOT USE! ------------------------- *
 *                                                                           *
\*                                                                           */

/* cut from ../obsolete/driver.h (since it doesn't mix well with other
 * headers from the emulator).
 */
#ifdef __WIN32__
#ifdef CONST
#  undef CONST
#endif
#endif

#if ((defined(__STDC__) || defined(SABER)) && !defined(NO_PROTOTYPE)) || defined(__cplusplus) || defined(USE_PROTOTYPE)
#   define _USING_PROTOTYPES_ 1
#   define _ANSI_ARGS_(x)	x
#   define CONST const
#else
#   define _ANSI_ARGS_(x)	()
#   define CONST
#endif

typedef void* erl_mutex_t;
typedef void* erl_cond_t;
typedef void* erl_thread_t;

EXTERN erl_mutex_t erts_mutex_create _ANSI_ARGS_((void));
EXTERN int erts_mutex_destroy _ANSI_ARGS_((erl_mutex_t));
EXTERN int erts_mutex_lock _ANSI_ARGS_((erl_mutex_t));
EXTERN int erts_mutex_unlock _ANSI_ARGS_((erl_mutex_t));

EXTERN erl_cond_t erts_cond_create _ANSI_ARGS_((void));
EXTERN int erts_cond_destroy _ANSI_ARGS_((erl_cond_t));
EXTERN int erts_cond_signal _ANSI_ARGS_((erl_cond_t));
EXTERN int erts_cond_broadcast _ANSI_ARGS_((erl_cond_t));
EXTERN int erts_cond_wait _ANSI_ARGS_((erl_cond_t, erl_mutex_t));
EXTERN int erts_cond_timedwait _ANSI_ARGS_((erl_cond_t, erl_mutex_t, long));

EXTERN int erts_thread_create _ANSI_ARGS_((erl_thread_t*,
					 void* (*func)(void*),
					 void* arg,
					 int detached));
EXTERN erl_thread_t erts_thread_self _ANSI_ARGS_((void));
EXTERN void erts_thread_exit _ANSI_ARGS_((void*));
EXTERN int  erts_thread_join _ANSI_ARGS_((erl_thread_t, void**));
EXTERN int  erts_thread_kill _ANSI_ARGS_((erl_thread_t));

/*
 * These functions implement the thread interface in ../obsolete/driver.h.
 * Do *not* use this interface! Within the emulator, use the erl_threads.h,
 * erl_smp.h, or ethread.h interface. From a driver use the thread interface
 * in erl_driver.h.
 */

erl_mutex_t
erts_mutex_create(void)
{
    return (erl_mutex_t) erl_drv_mutex_create(NULL);
}

int
erts_mutex_destroy(erl_mutex_t mtx)
{
    erl_drv_mutex_destroy((ErlDrvMutex *) mtx);
    return 0;
}

int
erts_mutex_lock(erl_mutex_t mtx)
{
    erl_drv_mutex_lock((ErlDrvMutex *) mtx);
    return 0;
}

int
erts_mutex_unlock(erl_mutex_t mtx)
{
    erl_drv_mutex_unlock((ErlDrvMutex *) mtx);
    return 0;
}

erl_cond_t
erts_cond_create(void)
{
    return (erl_cond_t) erl_drv_cond_create(NULL);
}

int
erts_cond_destroy(erl_cond_t cnd)
{
    erl_drv_cond_destroy((ErlDrvCond *) cnd);
    return 0;
}


int
erts_cond_signal(erl_cond_t cnd)
{
    erl_drv_cond_signal((ErlDrvCond *) cnd);
    return 0;
}

int
erts_cond_broadcast(erl_cond_t cnd)
{
    erl_drv_cond_broadcast((ErlDrvCond *) cnd);
    return 0;
}


int
erts_cond_wait(erl_cond_t cnd, erl_mutex_t mtx)
{
    erl_drv_cond_wait((ErlDrvCond *) cnd, (ErlDrvMutex *) mtx);
    return 0;
}

int
erts_cond_timedwait(erl_cond_t cnd, erl_mutex_t mtx, long ms)
{
    return ENOTSUP;
}

int
erts_thread_create(erl_thread_t *tid,
		   void* (*func)(void*),
		   void* arg,
		   int detached)
{
    if (detached)
	return ENOTSUP;
    return erl_drv_thread_create(NULL, (ErlDrvTid *) tid, func, arg, NULL);
}

erl_thread_t
erts_thread_self(void)
{
    return (erl_thread_t) erl_drv_thread_self();
}

void
erts_thread_exit(void *res)
{
    erl_drv_thread_exit(res);
}

int
erts_thread_join(erl_thread_t tid, void **respp)
{
    return erl_drv_thread_join((ErlDrvTid) tid, respp);
}

int
erts_thread_kill(erl_thread_t tid)
{
    return ENOTSUP;
}

