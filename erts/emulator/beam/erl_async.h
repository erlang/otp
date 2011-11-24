/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2011. All Rights Reserved.
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

#ifndef ERL_ASYNC_H__
#define ERL_ASYNC_H__

#define ERTS_MAX_NO_OF_ASYNC_THREADS 1024
extern int erts_async_max_threads;
#define ERTS_ASYNC_THREAD_MIN_STACK_SIZE 16	/* Kilo words */
#define ERTS_ASYNC_THREAD_MAX_STACK_SIZE 8192	/* Kilo words */
extern int erts_async_thread_suggested_stack_size;

#ifdef USE_THREADS

#ifdef ERTS_SMP
/*
 * With smp support we can choose to have, or not to
 * have an async ready queue.
 */
#define ERTS_USE_ASYNC_READY_Q 1
#endif

#ifndef ERTS_SMP
/* In non-smp case we *need* the async ready queue */
#  undef ERTS_USE_ASYNC_READY_Q
#  define ERTS_USE_ASYNC_READY_Q 1
#endif

#ifndef ERTS_USE_ASYNC_READY_Q
#  define ERTS_USE_ASYNC_READY_Q 0
#endif

#if ERTS_USE_ASYNC_READY_Q
int erts_check_async_ready(void *);
int erts_async_ready_clean(void *, void *);
void *erts_get_async_ready_queue(Uint sched_id);
#define ERTS_ASYNC_READY_CLEAN 0
#define ERTS_ASYNC_READY_DIRTY 1
#ifdef ERTS_SMP
#define ERTS_ASYNC_READY_NEED_THR_PRGR 2
#endif
#endif /* ERTS_USE_ASYNC_READY_Q */

#endif /* USE_THREADS */

void erts_init_async(void);
void erts_exit_flush_async(void);


#endif /* ERL_ASYNC_H__ */
