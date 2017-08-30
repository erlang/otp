/* 
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2005-2016. All Rights Reserved.
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
 * Low-level ethread support on SPARC V9.
 * Author: Mikael Pettersson.
 */
#ifndef ETHREAD_SPARC32_ETHREAD_H
#define ETHREAD_SPARC32_ETHREAD_H

#include "ethr_membar.h"
#define ETHR_ATOMIC_WANT_32BIT_IMPL__
#include "atomic.h"
#if ETHR_SIZEOF_PTR == 8
#  define ETHR_ATOMIC_WANT_64BIT_IMPL__
#  include "atomic.h"
#endif
#include "spinlock.h"
#include "rwlock.h"

#endif /* ETHREAD_SPARC32_ETHREAD_H */
