/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1996-2016. All Rights Reserved.
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
/*
 * Function: General purpose Memory allocator for fixed block 
 *    size objects. This allocater is at least an order of 
 *    magnitude faster than malloc().
 */
#include "eidef.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "ei_locking.h"
#include "erl_interface.h"
#include "erl_error.h"
#include "erl_malloc.h"
#include "erl_fix_alloc.h"
#include "erl_eterm.h"

#define WIPE_CHAR ((char)0xaa) /* 10101010 */

/* the freelist is a singly linked list of these */
/* i.e. the user structure and a link pointer */
struct fix_block {
  ETERM term;
  struct fix_block *next;
  int free;
};

/* this is a struct just to keep namespace pollution low on VxWorks */
struct eterm_stateinfo {
  struct fix_block *freelist;
  unsigned long freed;
  unsigned long allocated;
#ifdef _REENTRANT
  ei_mutex_t *lock;
#endif /* _REENTRANT */
};
/* FIXME problem for threaded ? */
static struct eterm_stateinfo *erl_eterm_state=NULL;


int erl_init_eterm_alloc (void)
{
#if defined(PURIFY) && defined (DEBUG)
    fprintf(stderr,"erl_fix_alloc() compiled for Purify - using \"real\" malloc()");
#endif
  
    erl_eterm_state = malloc(sizeof(*erl_eterm_state));
    if (erl_eterm_state == NULL) goto err1;

    erl_eterm_state->freelist = NULL;
    erl_eterm_state->freed = 0;
    erl_eterm_state->allocated = 0;
#ifdef _REENTRANT
    erl_eterm_state->lock = ei_mutex_create();    
    if (erl_eterm_state->lock == NULL) goto err2;
#endif /* _REENTRANT */

    return 1;

    /* Error cleanup */
#ifdef _REENTRANT
 err2:
    /* FIXME ENOMEM is not what went wrong... */
    free(erl_eterm_state);
#endif /* _REENTRANT */
 err1:
    erl_errno = ENOMEM;
    return 0;
}

/* get an eterm, from the freelist if possible or from malloc() */
void *erl_eterm_alloc (void)
{
#ifdef PURIFY
    ETERM *p;
  
    if ((p = malloc(sizeof(*p)))) {
	memset(p, WIPE_CHAR, sizeof(*p));
    }
    return p;
#else
    struct fix_block *b;

#ifdef _REENTRANT
    ei_mutex_lock(erl_eterm_state->lock, 0);
#endif /* _REENTRANT */

    /* try to pop block from head of freelist */
    if ((b = erl_eterm_state->freelist) != NULL) {
	erl_eterm_state->freelist = b->next;
	erl_eterm_state->freed--;      
    } else if ((b = malloc(sizeof(*b))) == NULL) {
	erl_errno = ENOMEM;
#ifdef _REENTRANT
	ei_mutex_unlock(erl_eterm_state->lock);
#endif /* _REENTRANT */
	return NULL;
    }
    erl_eterm_state->allocated++;
    b->free = 0;
    b->next = NULL;
#ifdef _REENTRANT
    ei_mutex_unlock(erl_eterm_state->lock);
#endif /* _REENTRANT */
    return (void *) &b->term;
#endif /* !PURIFY */
}

/* free an eterm back to the freelist */
void erl_eterm_free(void *p)
{
#ifdef PURIFY
  if (p) {
      memset(p, WIPE_CHAR, sizeof(ETERM));
  }
  free(p);
#else
  struct fix_block *b = p;
  
  if (b) {
      if (b->free) {
#ifdef DEBUG
	  fprintf(stderr,"erl_eterm_free: attempt to free already freed block %p\n",b);
#endif
	  return;
      }

#ifdef _REENTRANT
      ei_mutex_lock(erl_eterm_state->lock,0);
#endif /* _REENTRANT */
      b->free = 1;
      b->next = erl_eterm_state->freelist;
      erl_eterm_state->freelist = b;
      erl_eterm_state->freed++;
      erl_eterm_state->allocated--;
#ifdef _REENTRANT
      ei_mutex_unlock(erl_eterm_state->lock);
#endif /* _REENTRANT */
  }
#endif /* !PURIFY */
}

/* really free the freelist */
void erl_eterm_release (void)
{
#if !defined(PURIFY)
    struct fix_block *b;

#ifdef _REENTRANT
    ei_mutex_lock(erl_eterm_state->lock,0);
#endif /* _REENTRANT */
    {
	while (erl_eterm_state->freelist != NULL) {
	    b = erl_eterm_state->freelist;
	    erl_eterm_state->freelist = b->next;
	    free(b);
	    erl_eterm_state->freed--;
	}
    }
#ifdef _REENTRANT
    ei_mutex_unlock(erl_eterm_state->lock);
#endif /* _REENTRANT */
#endif /* !PURIFY */
}

void erl_eterm_statistics (unsigned long *allocd, unsigned long *freed)
{
  if (allocd) *allocd = erl_eterm_state->allocated;
  if (freed) *freed = erl_eterm_state->freed;

  return;
}


/*
 * Local Variables:
 * compile-command: "cd ..; ERL_TOP=/clearcase/otp/erts make -k"
 * End:
 */
