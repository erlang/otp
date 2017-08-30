/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2003-2016. All Rights Reserved.
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


#ifndef HIPE_ARCH_H
#define HIPE_ARCH_H

extern const void *hipe_arch_primop_address(Eterm key);

/* used by beam_load.c:patch(). patchtype == am_load_fe, Value is an ErlFunEntry* */
extern void hipe_patch_address(Uint *address, Eterm patchtype, Uint value);
extern void hipe_patch_load_fe(Uint *address, Uint value);
extern int hipe_patch_insn(void *address, Uint value, Eterm type);
extern int hipe_patch_call(void *callAddress, void *destAddress, void *trampoline);

extern void *hipe_alloc_code(Uint nrbytes, Eterm callees, Eterm *trampolines, Process *p);
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
