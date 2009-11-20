/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2003-2009. All Rights Reserved.
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
/* $Id$
 */
#ifndef HIPE_ARCH_H
#define HIPE_ARCH_H

extern const void *hipe_arch_primop_address(Eterm key);

/* used by beam_load.c:patch(). patchtype == am_load_fe, Value is an ErlFunEntry* */
extern void hipe_patch_address(Uint *address, Eterm patchtype, Uint value);
extern void hipe_patch_load_fe(Uint *address, Uint value);
extern int hipe_patch_insn(void *address, Uint value, Eterm type);
extern int hipe_patch_call(void *callAddress, void *destAddress, void *trampoline);

extern void *hipe_make_native_stub(void *beamAddress, unsigned int beamArity);

#if defined(__sparc__)
#include "hipe_sparc.h"
#endif
#if defined(__i386__)
#include "hipe_x86.h"
#endif
#if defined(__x86_64__)
#include "hipe_amd64.h"
#endif
#if defined(__powerpc__) || defined(__ppc__) || defined(__powerpc64__)
#include "hipe_ppc.h"
#endif
#if defined(__arm__)
#include "hipe_arm.h"
#endif

#if !defined(AEXTERN)
#define AEXTERN(RET,NAME,PROTO)	extern RET NAME PROTO
#endif

#endif /* HIPE_ARCH_H */
