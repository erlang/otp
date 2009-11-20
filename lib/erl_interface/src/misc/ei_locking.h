/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1997-2009. All Rights Reserved.
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
 *

 */
#ifndef _EI_LOCKING_H
#define _EI_LOCKING_H

#include "config.h"

#if defined(VXWORKS)
#include <taskLib.h>
#include <taskVarLib.h>
#endif

#ifdef __WIN32__
#include <winsock2.h>
#include <windows.h>
#include <winbase.h>
#endif

#ifdef HAVE_MIT_PTHREAD_H
#include <pthread/mit/pthread.h>
#elif HAVE_PTHREAD_H 
#include <pthread.h>
#endif


typedef struct ei_mutex_s {
#ifdef __WIN32__
  HANDLE lock;
#elif VXWORKS
  SEM_ID lock;
#else /* unix */
#if defined(HAVE_MIT_PTHREAD_H) || defined(HAVE_PTHREAD_H)
  pthread_mutex_t *lock;
#else /* ! (HAVE_MIT_PTHREAD_H || HAVE_PTHREAD_H) */
  void *dummy;   /* Actually never used */
#endif /* ! (HAVE_MIT_PTHREAD_H || HAVE_PTHREAD_H) */
#endif /* unix */
} ei_mutex_t;

extern ei_mutex_t* ei_sockets_lock; /* FIXME global variable! */

ei_mutex_t *ei_mutex_create(void);
int ei_mutex_free(ei_mutex_t *l, int nblock);
int ei_mutex_lock(ei_mutex_t *l, int nblock);
int ei_mutex_unlock(ei_mutex_t *l);


#if defined(_REENTRANT) && !defined(VXWORKS) && !defined(__WIN32__)

void *ei_m_create(void);
int ei_m_destroy(void *l);
int ei_m_lock(void *l);
int ei_m_trylock(void *l);
int ei_m_unlock(void *l);

#endif /* _REENTRANT && !VXWORKS && !__WIN32__ */

#endif /* _EI_LOCKING_H */
