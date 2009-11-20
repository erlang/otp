/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2005-2009. All Rights Reserved.
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


#ifndef ERL_BIF_TIMER_H__
#define ERL_BIF_TIMER_H__

typedef struct ErtsBifTimer_ ErtsBifTimer;

#include "sys.h"
#include "erl_process.h"
#include "erl_message.h"

Uint erts_bif_timer_memory_size(void);
void erts_print_bif_timer_info(int to, void *to_arg);
void erts_cancel_bif_timers(Process *p, ErtsProcLocks plocks);
void erts_bif_timer_init(void);
void erts_bif_timer_foreach(void (*func)(Eterm,Eterm,ErlHeapFragment *,void *),
			    void *arg);
#endif
