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
 */
/* timeout.c 
 * 
 * todo: use posix timers (timer_create etc) instead of setitimer.
 * 
 */
#if !defined(__WIN32__) && !defined(VXWORKS)

/* FIXME: well, at least I can compile now... */

#include "eidef.h"

#include <stdio.h>
#include <stdlib.h>
#include <setjmp.h>
#include <signal.h>

#if TIME_WITH_SYS_TIME
# include <sys/time.h>
# include <time.h>
#else
# if HAVE_SYS_TIME_H
#  include <sys/time.h>
# else
#  include <time.h>
# endif
#endif

#include "erl_interface.h"
#include "erl_timeout.h"

typedef struct jmp_s {
  jmp_buf jmpbuf;
  struct itimerval timerinfo;
  void *siginfo;
  struct jmp_s *next;
} *jmp_t;

static jmp_t push(jmp_t j);
static jmp_t pop(void);
static void timeout_handler(int dummy);

jmp_buf *timeout_setup(int ms)
{
  struct itimerval t;
  jmp_t j;
  void *s;

#ifdef DEBUG
  fprintf(stderr,"timeout setup\n");
#endif
  s=signal(SIGALRM,timeout_handler);

  /* set the timer */
  t.it_interval.tv_sec = 0;
  t.it_interval.tv_usec = 0;
  t.it_value.tv_sec = ms / 1000;
  t.it_value.tv_usec = (ms % 1000) * 1000;

  /* get a jump buffer and save it */
  j = erl_malloc(sizeof(*j));
  j->siginfo = s;
  push(j);

  setitimer(ITIMER_REAL,&t,&(j->timerinfo));

  return &(j->jmpbuf);
}


int timeout_cancel(void)
{
  jmp_t j;

#ifdef DEBUG
  fprintf(stderr,"timeout cancel\n");
#endif
  /* retrieve the jump buffer */
  j=pop();
  /* restore the timer and signal disposition */
  setitimer(ITIMER_REAL,&(j->timerinfo),NULL);
  signal(SIGALRM,j->siginfo);

  free(j);

  return 0;
}

void timeout_handler(int dummy)
{
  jmp_t j;

#ifdef DEBUG
  fprintf(stderr,"timeout handler\n");
#endif

  /* retrieve the jump buffer */
  j=pop();

  /* restore the timer and signal disposition */
  setitimer(ITIMER_REAL,&(j->timerinfo),NULL);
  signal(SIGALRM,j->siginfo);

  free(j);
  longjmp(j->jmpbuf,JMPVAL);
  return; /* not reached */
}


/* a simple stack for saving the jump buffer allows us to pass a
 * variable between functions that don't call each other, in a way
 * that will survive the longjmp(). 
 */

/* FIXME problem for threaded ? */
static jmp_t jmp_head=NULL;
#ifdef DEBUG
static int depth = 0;
static int maxdepth = 0;
#endif

static jmp_t push(jmp_t j)
{
  j->next = jmp_head;
  jmp_head = j;

#ifdef DEBUG
  depth++;
  if (depth > maxdepth) maxdepth = depth;
#endif

  return j;
}

static jmp_t pop(void)
{
  jmp_t j = jmp_head;
  if (j) jmp_head = j->next;
#ifdef DEBUG
  depth--;
#endif
  return j;
}

#endif /* platform */
