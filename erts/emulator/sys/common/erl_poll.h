/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2006-2016. All Rights Reserved.
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

/**
 * @description: Poll interface suitable for ERTS with SMP support.
 *
 * @author: 	Rickard Green
 * @author: 	Lukas Larsson
 *
 * This header file exports macros and functions that are used to
 * react to I/O polling events from file descriptors or wait-able
 * objects. The API exported is the following:
 *
 * defines:
 *   ERTS_POLL_EV_NONE - No events have been set. This is not the same as 0.
 *   ERTS_POLL_EV_IN   - Represent an IN event
 *   ERTS_POLL_EV_OUT  - Represent an OUT event
 *   ERTS_POLL_EV_ERR  - Represent an error event
 *   ERTS_POLL_EV_NVAL - Represent an invalid event
 *
 * macro functions:
 *   ErtsSysFdType ERTS_POLL_RES_GET_FD(ErtsPollResFd *evt);
 *   void ERTS_POLL_RES_SET_FD(ErtsPollResFd *evt, ErtsSysFdType fd);
 *   ErtsPollEvents ERTS_POLL_RES_GET_EVTS(ErtsPollResFd *evt)
 *   void ERTS_POLL_RES_SET_EVTS(ErtsPollResFd *evt, ErtsPollEvents fd);
 *
 * functions:
 *   See erl_poll_api.h
 */

#ifndef ERL_POLL_H__
#define ERL_POLL_H__

#include "sys.h"

#define ERTS_POLL_NO_TIMEOUT ERTS_MONOTONIC_TIME_MIN

#ifdef ERTS_ENABLE_KERNEL_POLL
#  undef ERTS_ENABLE_KERNEL_POLL
#  define ERTS_ENABLE_KERNEL_POLL 1
#  if defined(ERTS_NO_KERNEL_POLL_VERSION)
#    define ERTS_POLL_EXPORT(FUNC) FUNC ## _flbk
#    undef ERTS_NO_KERNEL_POLL_VERSION
#    define ERTS_NO_KERNEL_POLL_VERSION 1
#    define ERTS_KERNEL_POLL_VERSION 0
#  else
#    undef ERTS_KERNEL_POLL_VERSION
#    define ERTS_KERNEL_POLL_VERSION 1
#    define ERTS_NO_KERNEL_POLL_VERSION 0
#    define ERTS_POLL_EXPORT(FUNC) FUNC
#  endif
#else
#    define ERTS_POLL_EXPORT(FUNC) FUNC
#    define ERTS_ENABLE_KERNEL_POLL 0
#    define ERTS_NO_KERNEL_POLL_VERSION 1
#    define ERTS_KERNEL_POLL_VERSION 0
#endif

#undef ERTS_POLL_USE_KQUEUE
#define ERTS_POLL_USE_KQUEUE 0
#undef ERTS_POLL_USE_EPOLL
#define ERTS_POLL_USE_EPOLL 0
#undef ERTS_POLL_USE_DEVPOLL
#define ERTS_POLL_USE_DEVPOLL 0
#undef ERTS_POLL_USE_POLL
#define ERTS_POLL_USE_POLL 0
#undef ERTS_POLL_USE_SELECT
#define ERTS_POLL_USE_SELECT 0

/* Defines which structure that erts_poll_wait should use to wait with
   and how events should be represented */
#define ERTS_POLL_USE_EPOLL_EVS 0
#define ERTS_POLL_USE_KQUEUE_EVS 0
#define ERTS_POLL_USE_DEVPOLL_EVS 0
#define ERTS_POLL_USE_POLL_EVS 0
#define ERTS_POLL_USE_SELECT_EVS 0

#define ERTS_POLL_USE_KERNEL_POLL ERTS_KERNEL_POLL_VERSION

#if ERTS_ENABLE_KERNEL_POLL
#  if defined(HAVE_SYS_EVENT_H)
#    undef ERTS_POLL_USE_KQUEUE_EVS
#    define ERTS_POLL_USE_KQUEUE_EVS 1
#    undef ERTS_POLL_USE_KQUEUE
#    define ERTS_POLL_USE_KQUEUE ERTS_KERNEL_POLL_VERSION
#  elif defined(HAVE_SYS_EPOLL_H)
#    undef ERTS_POLL_USE_EPOLL_EVS
#    define ERTS_POLL_USE_EPOLL_EVS 1
#    undef ERTS_POLL_USE_EPOLL
#    define ERTS_POLL_USE_EPOLL ERTS_KERNEL_POLL_VERSION
#  elif defined(HAVE_SYS_DEVPOLL_H)
#    undef ERTS_POLL_USE_DEVPOLL_EVS
#    define ERTS_POLL_USE_DEVPOLL_EVS 1
#    undef ERTS_POLL_USE_DEVPOLL
#    define ERTS_POLL_USE_DEVPOLL ERTS_KERNEL_POLL_VERSION
#  else
#    error "Missing kernel poll implementation of erts_poll()"
#  endif
#endif

#if ERTS_NO_KERNEL_POLL_VERSION
#  if defined(ERTS_USE_POLL)
#    undef ERTS_POLL_USE_POLL_EVS
#    define ERTS_POLL_USE_POLL_EVS 1
#    undef ERTS_POLL_USE_POLL
#    define ERTS_POLL_USE_POLL 1
#  elif !defined(__WIN32__)
#    undef ERTS_POLL_USE_SELECT_EVS
#    define ERTS_POLL_USE_SELECT_EVS 1
#    undef ERTS_POLL_USE_SELECT
#    define ERTS_POLL_USE_SELECT 1
#  endif
#endif

#define ERTS_POLL_USE_FALLBACK (ERTS_POLL_USE_KQUEUE || ERTS_POLL_USE_EPOLL)

typedef Uint32 ErtsPollEvents;

typedef enum {
    ERTS_POLL_OP_ADD = 0,          /* Add the FD to the pollset */
    ERTS_POLL_OP_MOD = 1,          /* Modify the FD in the pollset */
    ERTS_POLL_OP_DEL = 2           /* Delete the FD from the pollset */
} ErtsPollOp;

#define op2str(op) (op == ERTS_POLL_OP_ADD ? "add" :            \
                    (op == ERTS_POLL_OP_MOD ? "mod" : "del"))

#if defined(__WIN32__)	/* --- win32  --------------------------------------- */

#define ERTS_POLL_EV_IN    1
#define ERTS_POLL_EV_OUT   2
#define ERTS_POLL_EV_ERR   4
#define ERTS_POLL_EV_NVAL  8

#define ERTS_POLL_EV_E2N(EV) 		(EV)
#define ERTS_POLL_EV_N2E(EV) 		(EV)

#elif ERTS_POLL_USE_EPOLL_EVS	/* --- epoll ------------------------------- */

#include <sys/epoll.h>

#define ERTS_POLL_EV_E2N(EV) \
  ((uint32_t) (EV))
#define ERTS_POLL_EV_N2E(EV) \
  ((ErtsPollEvents) (EV) & ~EPOLLONESHOT)

#define ERTS_POLL_EV_IN			ERTS_POLL_EV_N2E(EPOLLIN)
#define ERTS_POLL_EV_OUT		ERTS_POLL_EV_N2E(EPOLLOUT)
#define ERTS_POLL_EV_NVAL		ERTS_POLL_EV_N2E(EPOLLET)
#define ERTS_POLL_EV_ERR		ERTS_POLL_EV_N2E(EPOLLERR|EPOLLHUP)

typedef struct epoll_event ErtsPollResFd;

#define ERTS_POLL_RES_GET_FD(evt) ((ErtsSysFdType)((evt)->data.fd))
#define ERTS_POLL_RES_SET_FD(evt, ident) (evt)->data.fd = ident
#define ERTS_POLL_RES_GET_EVTS(evt) ERTS_POLL_EV_N2E((evt)->events)
#define ERTS_POLL_RES_SET_EVTS(evt, evts) (evt)->events = ERTS_POLL_EV_E2N(evts)

#elif ERTS_POLL_USE_DEVPOLL_EVS	/* --- devpoll ----------------------------- */

#include <sys/devpoll.h>

#define ERTS_POLL_EV_E2N(EV) \
  ((short) ((EV) & ~((~((ErtsPollEvents) 0)) << 8*SIZEOF_SHORT)))
#define ERTS_POLL_EV_N2E(EV) \
  ((ErtsPollEvents) ((unsigned short) (EV)))

#define ERTS_POLL_EV_IN			ERTS_POLL_EV_N2E(POLLIN)
#define ERTS_POLL_EV_OUT		ERTS_POLL_EV_N2E(POLLOUT)
#define ERTS_POLL_EV_NVAL		ERTS_POLL_EV_N2E(POLLNVAL)
#define ERTS_POLL_EV_ERR		ERTS_POLL_EV_N2E(POLLERR|POLLHUP)

typedef struct pollfd ErtsPollResFd;

#define ERTS_POLL_RES_GET_FD(evt) ((ErtsSysFdType)((evt)->fd))
#define ERTS_POLL_RES_SET_FD(evt, ident) (evt)->fd = ident
#define ERTS_POLL_RES_GET_EVTS(evt) ERTS_POLL_EV_N2E((evt)->revents)
#define ERTS_POLL_RES_SET_EVTS(evt, evts) (evt)->revents = ERTS_POLL_EV_E2N(evts)

#elif ERTS_POLL_USE_KQUEUE_EVS	/* --- kqueue ------------------------------ */
/* Kqueue use fallback defines (poll() or select()) */

#include <sys/event.h>

#ifdef ERTS_USE_POLL
#  undef ERTS_POLL_USE_POLL_EVS
#  define ERTS_POLL_USE_POLL_EVS   1
#elif !defined(__WIN32__)
#  undef ERTS_POLL_USE_SELECT_EVS
#  define ERTS_POLL_USE_SELECT_EVS 1
#endif

typedef struct kevent ErtsPollResFd;

#define ERTS_POLL_RES_GET_FD(evt) ((ErtsSysFdType)((evt)->ident))
#define ERTS_POLL_RES_SET_FD(evt, fd) (evt)->ident = fd
#define ERTS_POLL_RES_GET_EVTS(evt) ERTS_POLL_EV_N2E((ErtsPollEvents)(evt)->udata)
#define ERTS_POLL_RES_SET_EVTS(evt, evts) (evt)->udata = (void*)(UWord)(ERTS_POLL_EV_E2N(evts))

#endif

#if ERTS_POLL_USE_POLL_EVS
				/* --- poll -------------------------------- */
#include <poll.h>

#define ERTS_POLL_EV_NKP_E2N(EV) \
  ((short) ((EV) & ~((~((ErtsPollEvents) 0)) << 8*SIZEOF_SHORT)))
#define ERTS_POLL_EV_NKP_N2E(EV) \
  ((ErtsPollEvents) ((unsigned short) (EV)))

/* At least on FreeBSD, we need POLLRDNORM for normal files, not POLLIN. */
/* Whether this is a bug in FreeBSD, I don't know. */
#ifdef POLLRDNORM
#define ERTS_POLL_EV_NKP_IN		ERTS_POLL_EV_N2E(POLLIN|POLLRDNORM)
#else
#define ERTS_POLL_EV_NKP_IN		ERTS_POLL_EV_N2E(POLLIN)
#endif
#define ERTS_POLL_EV_NKP_OUT		ERTS_POLL_EV_N2E(POLLOUT)
#define ERTS_POLL_EV_NKP_NVAL		ERTS_POLL_EV_N2E(POLLNVAL)
#define ERTS_POLL_EV_NKP_ERR		ERTS_POLL_EV_N2E(POLLERR|POLLHUP)

#elif ERTS_POLL_USE_SELECT_EVS	/* --- select ------------------------------ */

#define ERTS_POLL_EV_NKP_E2N(EV) (EV)
#define ERTS_POLL_EV_NKP_N2E(EV) (EV)

#define ERTS_POLL_EV_NKP_IN		(((ErtsPollEvents) 1) << 0)
#define ERTS_POLL_EV_NKP_OUT		(((ErtsPollEvents) 1) << 1)
#define ERTS_POLL_EV_NKP_NVAL		(((ErtsPollEvents) 1) << 2)
#define ERTS_POLL_EV_NKP_ERR		(((ErtsPollEvents) 1) << 3)

#endif				/* ----------------------------------------- */


#if !defined(ERTS_POLL_EV_E2N) && defined(ERTS_POLL_EV_NKP_E2N)
/* poll(), select(), and kqueue() */

#define ERTS_POLL_EV_E2N(EV) 		ERTS_POLL_EV_NKP_E2N((EV))
#define ERTS_POLL_EV_N2E(EV) 		ERTS_POLL_EV_NKP_N2E((EV))

#define ERTS_POLL_EV_IN			ERTS_POLL_EV_NKP_IN
#define ERTS_POLL_EV_OUT		ERTS_POLL_EV_NKP_OUT
#define ERTS_POLL_EV_NVAL		ERTS_POLL_EV_NKP_NVAL
#define ERTS_POLL_EV_ERR		ERTS_POLL_EV_NKP_ERR

#endif

#if !ERTS_ENABLE_KERNEL_POLL

typedef struct _ErtsPollResFd {
    ErtsSysFdType fd;
    ErtsPollEvents events;
} ErtsPollResFd;

#define ERTS_POLL_RES_GET_FD(evt) (evt)->fd
#define ERTS_POLL_RES_SET_FD(evt, ident) (evt)->fd = (ident)
#define ERTS_POLL_RES_GET_EVTS(evt) ERTS_POLL_EV_N2E((evt)->events)
#define ERTS_POLL_RES_SET_EVTS(evt, evts) (evt)->events = ERTS_POLL_EV_E2N(evts)

#endif

#define ERTS_POLL_EV_NONE (UINT_MAX & ~(ERTS_POLL_EV_IN|ERTS_POLL_EV_OUT|ERTS_POLL_EV_NVAL|ERTS_POLL_EV_ERR))

#define ev2str(ev)                                                     \
    (((ev) == 0 || (ev) == ERTS_POLL_EV_NONE) ? "NONE" :               \
     ((ev) == ERTS_POLL_EV_IN ? "IN" :                                 \
      ((ev) == ERTS_POLL_EV_OUT ? "OUT" :                              \
       ((ev) == (ERTS_POLL_EV_IN|ERTS_POLL_EV_OUT) ? "IN|OUT" :        \
        ((ev) & ERTS_POLL_EV_ERR ? "ERR" :                             \
         ((ev) & ERTS_POLL_EV_NVAL ? "NVAL" : "OTHER"))))))


typedef struct ERTS_POLL_EXPORT(erts_pollset) ErtsPollSet;

typedef struct {
    char *primary;
    char *kernel_poll;
    Uint memory_size;
    Uint poll_set_size;
    int lazy_updates;
    Uint pending_updates;
    int batch_updates;
    int concurrent_updates;
    int is_fallback;
    Uint max_fds;
    Uint active_fds;
    Uint poll_threads;
} ErtsPollInfo;

#if defined(ERTS_POLL_USE_FALLBACK) && ERTS_KERNEL_POLL_VERSION
#  undef ERTS_POLL_EXPORT
#  define ERTS_POLL_EXPORT(FUNC) FUNC ## _flbk
#  include "erl_poll_api.h"
#  undef ERTS_POLL_EXPORT
#  define ERTS_POLL_EXPORT(FUNC) FUNC
#elif !defined(ERTS_POLL_USE_FALLBACK)
#  define ERTS_POLL_USE_FALLBACK 0
#endif

#include "erl_poll_api.h"

/**
 * Get the next size of the array that holds the file descriptors.
 * This function is used in order for the check io array and the
 * pollset array to be of the same size.
 */
int erts_poll_new_table_len(int old_len, int need_len);

#endif /* #ifndef ERL_POLL_H__ */
