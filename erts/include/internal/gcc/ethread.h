/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2010-2011. All Rights Reserved.
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
 * Description: Native atomic ethread support when using gcc
 * Author: Rickard Green
 */

#ifndef ETHREAD_GCC_H__
#define ETHREAD_GCC_H__

#if defined(ETHR_HAVE___SYNC_VAL_COMPARE_AND_SWAP32) \
     || defined(ETHR_HAVE___SYNC_VAL_COMPARE_AND_SWAP64)

#ifndef ETHR_MEMBAR
#  include "ethr_membar.h"
#endif

#if !defined(ETHR_HAVE_NATIVE_ATOMIC32)
#  define ETHR_ATOMIC_WANT_32BIT_IMPL__
#  include "ethr_atomic.h"
#endif

#if ETHR_SIZEOF_PTR == 8 && !defined(ETHR_HAVE_NATIVE_ATOMIC64)
#  define ETHR_ATOMIC_WANT_64BIT_IMPL__
#  include "ethr_atomic.h"
#endif

#if (!defined(ETHR_HAVE_NATIVE_DW_ATOMIC) \
     && !(ETHR_SIZEOF_PTR == 4 && defined(ETHR_HAVE_NATIVE_ATOMIC64)) \
     && !(ETHR_SIZEOF_PTR == 8 && defined(ETHR_HAVE_NATIVE_ATOMIC128)))
#  include "ethr_dw_atomic.h"
#endif

#endif

#endif
