/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2004-2011. All Rights Reserved.
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


#ifndef HIPE_GC_H
#define HIPE_GC_H

#if defined(__sparc__)
#include "hipe_sparc_gc.h"
#endif
#if defined(__i386__)
#include "hipe_x86_gc.h"
#endif
#if defined(__x86_64__)
#include "hipe_amd64_gc.h"
#endif
#if defined(__powerpc__) || defined(__ppc__) || defined(__powerpc64__)
#include "hipe_ppc_gc.h"
#endif
#if defined(__arm__)
#include "hipe_arm_gc.h"
#endif

#endif /* HIPE_GC_H */
