/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2002-2011. All Rights Reserved.
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
 * hipe_signal.h
 *
 * Architecture-specific initialisation of Unix signals.
 */
#ifndef HIPE_SIGNAL_H
#define HIPE_SIGNAL_H

#if defined(__i386__) || defined(__x86_64__)
extern void hipe_signal_init(void);
#else
static __inline__ void hipe_signal_init(void) { }
#endif

#if defined(ERTS_SMP) && (defined(__i386__) || defined(__x86_64__))
extern void hipe_thread_signal_init(void);
#else
static __inline__ void hipe_thread_signal_init(void) { }
#endif

#endif /* HIPE_SIGNAL_H */
