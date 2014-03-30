/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2006-2013. All Rights Reserved.
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

/*
 * Description:	Poll interface suitable for ERTS with or without
 *              SMP support.
 *
 * Author: 	Rickard Green
 */

#ifndef ERL_POLL_H__
#define ERL_POLL_H__

#include "sys.h"

#if 0
#define ERTS_POLL_COUNT_AVOIDED_WAKEUPS
#endif

#ifdef ERTS_ENABLE_KERNEL_POLL
#  if defined(ERTS_KERNEL_POLL_VERSION)
#    define ERTS_POLL_EXPORT(FUNC) FUNC ## _kp
#  else
#    define ERTS_POLL_EXPORT(FUNC) FUNC ## _nkp
#    undef ERTS_POLL_DISABLE_KERNEL_POLL
#    define ERTS_POLL_DISABLE_KERNEL_POLL
#  endif
#else
#    define ERTS_POLL_EXPORT(FUNC) FUNC
#    undef ERTS_POLL_DISABLE_KERNEL_POLL
#    define ERTS_POLL_DISABLE_KERNEL_POLL
#endif

#ifdef ERTS_POLL_DISABLE_KERNEL_POLL
#  undef HAVE_SYS_EPOLL_H
#  undef HAVE_SYS_EVENT_H
#  undef HAVE_SYS_DEVPOLL_H
#endif

#undef ERTS_POLL_USE_KERNEL_POLL
#define ERTS_POLL_USE_KERNEL_POLL 0

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

#if defined(HAVE_SYS_EVENT_H)
#  undef ERTS_POLL_USE_KQUEUE
#  define ERTS_POLL_USE_KQUEUE 1
#  undef ERTS_POLL_USE_KERNEL_POLL
#  define ERTS_POLL_USE_KERNEL_POLL 1
#elif defined(HAVE_SYS_EPOLL_H)
#  undef ERTS_POLL_USE_EPOLL
#  define ERTS_POLL_USE_EPOLL 1
#  undef ERTS_POLL_USE_KERNEL_POLL
#  define ERTS_POLL_USE_KERNEL_POLL 1
#elif defined(HAVE_SYS_DEVPOLL_H)
#  undef ERTS_POLL_USE_DEVPOLL
#  define ERTS_POLL_USE_DEVPOLL 1
#  undef ERTS_POLL_USE_KERNEL_POLL
#  define ERTS_POLL_USE_KERNEL_POLL 1
#endif

#define ERTS_POLL_USE_FALLBACK (ERTS_POLL_USE_KQUEUE || ERTS_POLL_USE_EPOLL)

#if !ERTS_POLL_USE_KERNEL_POLL || ERTS_POLL_USE_FALLBACK
#  if defined(ERTS_USE_POLL)
#    undef ERTS_POLL_USE_POLL
#    define ERTS_POLL_USE_POLL 1
#  elif !defined(__WIN32__) && !defined(__OSE__)
#    undef ERTS_POLL_USE_SELECT
#    define ERTS_POLL_USE_SELECT 1
#  endif
#endif

typedef Uint32 ErtsPollEvents;
#undef ERTS_POLL_EV_E2N

#if defined(__WIN32__) || defined(__OSE__)	/* --- win32 or ose -------- */

#define ERTS_POLL_EV_IN   1
#define ERTS_POLL_EV_OUT  2
#define ERTS_POLL_EV_ERR  4
#define ERTS_POLL_EV_NVAL 8

#ifdef __OSE__

typedef struct ErtsPollOseMsgList_ {
  struct ErtsPollOseMsgList_ *next;
  union SIGNAL *data;
} ErtsPollOseMsgList;

struct erts_sys_fd_type {
    SIGSELECT signo;
    ErlDrvOseEventId id;
    ErtsPollOseMsgList *msgs;
    ErlDrvOseEventId (*resolve_signal)(union SIGNAL *sig);
    ethr_mutex mtx;
    void *extra;
};

#endif

#elif ERTS_POLL_USE_EPOLL	/* --- epoll ------------------------------- */

#include <sys/epoll.h>

#define ERTS_POLL_EV_E2N(EV) \
  ((__uint32_t) (EV))
#define ERTS_POLL_EV_N2E(EV) \
  ((ErtsPollEvents) (EV))

#define ERTS_POLL_EV_IN			ERTS_POLL_EV_N2E(EPOLLIN)
#define ERTS_POLL_EV_OUT		ERTS_POLL_EV_N2E(EPOLLOUT)
#define ERTS_POLL_EV_NVAL		ERTS_POLL_EV_N2E(EPOLLET)
#define ERTS_POLL_EV_ERR		ERTS_POLL_EV_N2E(EPOLLERR|EPOLLHUP)

#elif ERTS_POLL_USE_DEVPOLL	/* --- devpoll ----------------------------- */

#include <sys/devpoll.h>

#define ERTS_POLL_EV_E2N(EV) \
  ((short) ((EV) & ~((~((ErtsPollEvents) 0)) << 8*SIZEOF_SHORT)))
#define ERTS_POLL_EV_N2E(EV) \
  ((ErtsPollEvents) ((unsigned short) (EV)))

#define ERTS_POLL_EV_IN			ERTS_POLL_EV_N2E(POLLIN)
#define ERTS_POLL_EV_OUT		ERTS_POLL_EV_N2E(POLLOUT)
#define ERTS_POLL_EV_NVAL		ERTS_POLL_EV_N2E(POLLNVAL)
#define ERTS_POLL_EV_ERR		ERTS_POLL_EV_N2E(POLLERR|POLLHUP)

#elif ERTS_POLL_USE_KQUEUE	/* --- kqueue ------------------------------ */
/* Kqueue use fallback defines (poll() or select()) */
#endif

#if ERTS_POLL_USE_POLL	/* --- poll -------------------------------- */

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

#elif ERTS_POLL_USE_SELECT	/* --- select ------------------------------ */

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

typedef struct ErtsPollSet_ *ErtsPollSet;

typedef struct {
    ErtsSysFdType fd;
    ErtsPollEvents events;
    int on;
} ErtsPollControlEntry;

typedef struct {
    ErtsSysFdType fd;
    ErtsPollEvents events;
} ErtsPollResFd;

typedef struct {
    char *primary;
    char *fallback;
    char *kernel_poll;
    Uint memory_size;
    int poll_set_size;
    int fallback_poll_set_size;
    int lazy_updates;
    int pending_updates;
    int batch_updates;
    int concurrent_updates;
    int max_fds;
#ifdef ERTS_POLL_COUNT_AVOIDED_WAKEUPS
    long no_avoided_wakeups;
    long no_avoided_interrupts;
    long no_interrupt_timed;
#endif
} ErtsPollInfo;

#ifdef ERTS_POLL_NEED_ASYNC_INTERRUPT_SUPPORT
void		ERTS_POLL_EXPORT(erts_poll_async_sig_interrupt)(ErtsPollSet);
#endif
void		ERTS_POLL_EXPORT(erts_poll_interrupt)(ErtsPollSet,
						      int);
void		ERTS_POLL_EXPORT(erts_poll_interrupt_timed)(ErtsPollSet,
							    int,
							    erts_short_time_t);
ErtsPollEvents	ERTS_POLL_EXPORT(erts_poll_control)(ErtsPollSet,
						    ErtsSysFdType,
						    ErtsPollEvents,
						    int on,
						    int* wake_poller
						    );
void		ERTS_POLL_EXPORT(erts_poll_controlv)(ErtsPollSet,
						     ErtsPollControlEntry [],
						     int on);
int		ERTS_POLL_EXPORT(erts_poll_wait)(ErtsPollSet,
						 ErtsPollResFd [],
						 int *,
						 SysTimeval *);
int		ERTS_POLL_EXPORT(erts_poll_max_fds)(void);
void		ERTS_POLL_EXPORT(erts_poll_info)(ErtsPollSet,
						 ErtsPollInfo *);
ErtsPollSet	ERTS_POLL_EXPORT(erts_poll_create_pollset)(void);
void		ERTS_POLL_EXPORT(erts_poll_destroy_pollset)(ErtsPollSet);
void		ERTS_POLL_EXPORT(erts_poll_init)(void);
void		ERTS_POLL_EXPORT(erts_poll_get_selected_events)(ErtsPollSet,
								ErtsPollEvents [],
								int);

int		ERTS_POLL_EXPORT(erts_poll_get_table_len)(int);

#endif /* #ifndef ERL_POLL_H__ */
