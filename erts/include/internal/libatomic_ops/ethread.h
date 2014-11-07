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
 * Description: Native atomics ethread support using libatomic_ops
 * Author: Rickard Green
 */

#ifndef ETHREAD_LIBATOMIC_OPS_H__
#define ETHREAD_LIBATOMIC_OPS_H__

#if (defined(ETHR_HAVE_LIBATOMIC_OPS) \
     && ((ETHR_SIZEOF_AO_T == 4 && !defined(ETHR_HAVE_NATIVE_ATOMIC32)) \
	 || (ETHR_SIZEOF_AO_T == 8 && !defined(ETHR_HAVE_NATIVE_ATOMIC64))))

#if defined(__x86_64__)
#define AO_USE_PENTIUM4_INSTRS
#endif

#define ETHR_NATIVE_IMPL__ "libatomic_ops"

#include "atomic_ops.h"
#include "ethr_membar.h"
#include "ethr_atomic.h"
#include "ethr_dw_atomic.h"

#endif

#endif
