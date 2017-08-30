/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2013-2016. All Rights Reserved.
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

#ifndef ERL_UTIL_QUEUE_H_
#define ERL_UTIL_QUEUE_H_

#define erts_circleq_head(Q)     ((Q)->next)
#define erts_circleq_tail(Q)     ((Q)->prev)
#define erts_circleq_next(Q)     ((Q)->next)
#define erts_circleq_prev(Q)     ((Q)->prev)
#define erts_circleq_is_empty(Q) ((Q)->next == (void *)(Q))

#define erts_circleq_remove(N)        \
    do {                              \
        (N)->next->prev = (N)->prev;  \
        (N)->prev->next = (N)->next;  \
        (N)->next = (N);              \
        (N)->prev = (N);              \
    } while(0)

#define erts_circleq_pop_head(Q, N)   \
    do {                              \
	(N) = (Q)->next;              \
        (N)->next->prev = (N)->prev;  \
        (N)->prev->next = (N)->next;  \
        (N)->next = (N);              \
        (N)->prev = (N);              \
    } while(0)

#define erts_circleq_pop_tail(Q, N)   \
    do {                              \
	(N) = (Q)->prev;              \
        (N)->next->prev = (N)->prev;  \
        (N)->prev->next = (N)->next;  \
        (N)->next = (N);              \
        (N)->prev = (N);              \
    } while(0)

#define erts_circleq_push_head(Q, N)  \
    do {                              \
        (N)->next = (Q)->next;        \
        (N)->prev = (void *)(Q);      \
        (Q)->next->prev = (N);        \
        (Q)->next = (N);              \
    } while(0)

#define erts_circleq_push_tail(Q, N)  \
    do {                              \
        (N)->prev = (Q)->prev;        \
        (N)->next = (void *)(Q);      \
        (Q)->prev->next = (N);        \
        (Q)->prev = (N);              \
    } while(0)

#define erts_circleq_foreach(V, Q) \
    for ((V) = (Q)->next; (V) != (const void *)(Q); (V) = (V)->next)

#define erts_circleq_foreach_reverse(V, Q) \
    for ((V) = (Q)->prev; (V) != (const void *)(Q); (V) = (V)->prev)

#endif
