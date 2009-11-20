/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2006-2009. All Rights Reserved.
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

#ifndef ERL_TIME_H__
#define ERL_TIME_H__

/*
** Timer entry:
*/
typedef struct erl_timer {
    struct erl_timer* next;	/* next entry tiw slot or chain */
    Uint slot;			/* slot in timer wheel */
    Uint count;			/* number of loops remaining */
    int    active;		/* 1=activated, 0=deactivated */
    /* called when timeout */
    void (*timeout)(void*);
    /* called when cancel (may be NULL) */
    void (*cancel)(void*);
    void* arg;        /* argument to timeout/cancel procs */
} ErlTimer;

typedef void (*ErlTimeoutProc)(void*);
typedef void (*ErlCancelProc)(void*);

#ifdef ERTS_SMP

/*
 * Process and port timer
 */
typedef union ErtsSmpPTimer_ ErtsSmpPTimer;
union ErtsSmpPTimer_ {
    struct {
	ErlTimer tm;
	Eterm id;
	void (*timeout_func)(void*);
	ErtsSmpPTimer **timer_ref;
	Uint32 flags;
    } timer;
    ErtsSmpPTimer *next;
};


void erts_create_smp_ptimer(ErtsSmpPTimer **timer_ref,
			    Eterm id,
			    ErlTimeoutProc timeout_func,
			    Uint timeout);
void erts_cancel_smp_ptimer(ErtsSmpPTimer *ptimer);

#endif

#endif
