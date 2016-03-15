/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1997-2016. All Rights Reserved.
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
 *

 */
/* 
 * common interface to some simple synchronisation primitives for
 * internal use by ei.
 */

/* Note that these locks are NOT recursive on Win32 or Solaris,
 * i.e. self-deadlock will occur if a thread tries to obtain a lock it
 * is already holding. The primitives used on VxWorks are recursive however.
 */

#include "eidef.h"

#ifdef __WIN32__
#include <winsock2.h>
#include <windows.h>
#include <winbase.h>

#elif VXWORKS
#include <vxWorks.h>
#include <semLib.h>

#else /* unix */
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#endif /* platforms */

#include "ei_malloc.h"
#include "ei_locking.h"

#ifdef _REENTRANT

/* 
 * Create a new mutex object. 
 * Returns a pointer to the mutex if successful, NULL otherwise.
 */
ei_mutex_t *ei_mutex_create(void)
{
    ei_mutex_t *l;

    if ((l = ei_malloc(sizeof(*l))) == NULL) return NULL;

#ifdef __WIN32__
  l->lock = CreateMutex(NULL,FALSE,NULL);

#elif VXWORKS
  if (!(l->lock = semMCreate(SEM_DELETE_SAFE))) {
    ei_free(l);
    return NULL;
  }
#else /* unix */
  l->lock = ei_m_create();
#endif

  return l;
}

/* 
 * Free a mutex and the structure asociated with it.
 *
 * This function attempts to obtain the mutex before releasing it;
 * If nblock == 1 and the mutex was unavailable, the function will
 * return failure and the mutex will not have been removed.
 *
 * If nblock == 0 the function will block until the mutex becomes
 * available, at which time it will be removed and the function will
 * succeed.
 *
 * returns 0 if the mutex is removed, -1 on failure (busy) 
 */
int ei_mutex_free(ei_mutex_t *l, int nblock)
{
  /* attempt to lock it first, to make sure it's really free */
  if (ei_mutex_lock(l,nblock)) return -1; /* attempt failed */
    
  /* we are now holding the lock */
#ifdef __WIN32__
  CloseHandle(l->lock);

#elif VXWORKS
  if (semDelete(l->lock) == ERROR) return -1;

#else /* unix */
  ei_m_destroy(l->lock);
#endif

  ei_free(l);

  return 0;
}

/* Grab a mutex. If the mutex is not held by any other process the
 * function returns so that the caller may enter a critical section.
 * Processes subsequently wishing to obtain the lock will block 
 * until this process releases it.
 *
 * If the mutex is busy (held by some other process) and nblock == 0,
 * the function will block until the mutex is freed by the process
 * holding it, returning only when the mutex has been grabbed.
 *
 * If the mutex is busy and nblock != 0, the function will not block.
 * Instead it will return -1 immediately, indicating that the
 * operation failed. 

 * Returns 0 on success, -1 on failure.
 */
int ei_mutex_lock(ei_mutex_t *l, int nblock)
{
#ifdef __WIN32__
  /* check valid values for timeout: is 0 ok? */
  if (WaitForSingleObject(l->lock,(nblock? 0 : INFINITE)) != WAIT_OBJECT_0) 
    return -1; 

#elif VXWORKS
  if (semTake(l->lock,(nblock? NO_WAIT : WAIT_FOREVER)) == ERROR)
    return -1;

#else /* unix */
  if (nblock) {
    if (ei_m_trylock(l->lock) < 0) return -1;
  }
  else ei_m_lock(l->lock);
#endif

  return 0;
}

/* Release a mutex */
int ei_mutex_unlock(ei_mutex_t *l)
{
#ifdef __WIN32__
  ReleaseMutex(l->lock);

#elif VXWORKS
  semGive(l->lock);

#else /* unix */
  ei_m_unlock(l->lock);
#endif

  return 0;
}

#endif /* _REENTRANT */
