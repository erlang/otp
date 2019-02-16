/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2001-2018. All Rights Reserved.
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

/* FIXME why not use ei_malloc here? */

#include "eidef.h"

#include <stdlib.h>
#include "ei.h"
#include "ei_locking.h"

#ifdef __WIN32__
#ifdef USE_DECLSPEC_THREAD
/* Define (and initialize) the variable __erl_errno */
volatile __declspec(thread) int __erl_errno = 0;
#else
static volatile DWORD errno_tls_index = TLS_OUT_OF_INDEXES;
static LONG volatile tls_init_mutex = 0;
#endif
#endif

#if defined(VXWORKS)

/* 
   Moved to each of the erl_*threads.c files, as they seem to know how
   to get thread-safety. 
*/
static volatile int __erl_errno;
volatile int *__erl_errno_place(void)
{
    /* This check is somewhat insufficient, double task var entries will occur
       if __erl_errno is actually -1, which on the other hand is an invalid 
       error code. */
    if (taskVarGet(taskIdSelf(), &__erl_errno) == ERROR) {
	taskVarAdd(taskIdSelf(), &__erl_errno);
    }
    return &__erl_errno;
}
#endif /* VXWORKS */

#if defined(__WIN32__)

#ifdef USE_DECLSPEC_THREAD

volatile int *__erl_errno_place(void)
{
    return &__erl_errno;
}

#else
static void tls_init_once(void)
{

    if (errno_tls_index != TLS_OUT_OF_INDEXES) {
	return;
    }
    if (InterlockedExchange((LPLONG) &tls_init_mutex,1L) == 0) {
	/* I was first */
	errno_tls_index = TlsAlloc();
	if (errno_tls_index == TLS_OUT_OF_INDEXES) {
	    fprintf(stderr, 
		    "FATAL ERROR: cannot allocate TLS index for "
		    "erl_errno (error code = %d)!\n",GetLastError());
	    exit(1);
	}
    } else {
	while (errno_tls_index == TLS_OUT_OF_INDEXES) {
	    SwitchToThread();
	}
    }
}

volatile int *__erl_errno_place(void)
{
    volatile int *ptr;
    tls_init_once();
    ptr = TlsGetValue(errno_tls_index);
    if (ptr == NULL) {
	ptr = malloc(sizeof(int));
	*ptr = 0;
	TlsSetValue(errno_tls_index, (PVOID) ptr);
    }
    return ptr;
}

#endif /* USE_DECLSPEC_THREAD */

#endif /* __WIN32__ */

#if defined(_REENTRANT) && !defined(VXWORKS) && !defined(__WIN32__)

#if defined(HAVE_PTHREAD_H) || defined(HAVE_MIT_PTHREAD_H)

void *ei_m_create(void)  
{ 
  pthread_mutex_t *l;
 
  if ((l = malloc(sizeof(*l)))) { /* FIXME get memory or abort */
    pthread_mutex_init(l,NULL);
  }

  return l;
}

int ei_m_destroy(void *l) 
{ 
  int r = pthread_mutex_destroy(l);
  free(l);
  
  return r;
}

int ei_m_lock(void *l)    
{ 
  return pthread_mutex_lock(l);
}

int ei_m_trylock(void *l) 
{ 
  return pthread_mutex_trylock(l);
}

int ei_m_unlock(void *l)  
{ 
  return pthread_mutex_unlock(l);
} 


/*
 * Thread-specific erl_errno variable.
 *
 * The second line below will give a "missing braces around initializer"
 * on Solaris but the code will work.
 */

static pthread_key_t erl_errno_key;
static pthread_once_t erl_errno_key_once = PTHREAD_ONCE_INIT;

/*
 * Destroy per-thread erl_errno locus
 */
static void erl_errno_destroy(void * ptr)
{
    free(ptr);
}

/*
 * Allocate erl_errno key.
 * This will be done once for all threads
 */
static void erl_errno_key_alloc(void)
{
    pthread_key_create(&erl_errno_key, erl_errno_destroy);
}

/*
 * Return a pointer to the erl_errno locus.
 * If pthread functions fail we fall back to using fallback_errno
 * so that the main thread (actually not a thread in all ascpects)
 * still will set and get an erl_errno value.
 * Actually this is a bit to nice, it would be preferrable to exit fatal
 * as we do on windows, but we might break some code with one thread
 * but still compiled with -D_REENTRANT, so we'll leave it here.
 */
volatile int *__erl_errno_place(void)
{
    int *erl_errno_p;
    static volatile int use_fallback = 0;
    static volatile int fallback_errno = 0;

    if (use_fallback) {
	return &fallback_errno;
    }	

    /* This will create the key once for all threads */
    if (pthread_once(&erl_errno_key_once, erl_errno_key_alloc) != 0) {
	use_fallback = 1;
	return &fallback_errno;
    }

    /* This is the normal case, return the pointer to the data */
    if ((erl_errno_p = pthread_getspecific(erl_errno_key)) != NULL) {
	return erl_errno_p;
    }

    if ((erl_errno_p = malloc(sizeof(int))) == NULL) {
	use_fallback = 1;
	return &fallback_errno;
    }
    *erl_errno_p = 0;

    if (pthread_setspecific(erl_errno_key, erl_errno_p) != 0 ||
	(erl_errno_p = pthread_getspecific(erl_errno_key)) == NULL) {
	free(erl_errno_p);
	return &fallback_errno;
    }

    return erl_errno_p;
}

#endif /* HAVE_PTHREAD_H || HAVE_MIT_PTHREAD_H */

#endif /* _REENTRANT && !VXWORKS && !__WIN32__ */

#if !defined(_REENTRANT) && !defined(VXWORKS) && !defined(__WIN32__)

volatile int __erl_errno;

#endif
