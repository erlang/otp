/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2002-2016. All Rights Reserved.
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
