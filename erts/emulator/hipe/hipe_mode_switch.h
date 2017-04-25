/*
 * %CopyrightBegin%

 *
 * Copyright Ericsson AB 2001-2016. All Rights Reserved.
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
 * hipe_mode_switch.h
 */
#ifndef HIPE_MODE_SWITCH_H
#define HIPE_MODE_SWITCH_H

/* command codes for beam_emu -> hipe_mode_switch() call */
#define HIPE_MODE_SWITCH_CMD_CALL		0
#define HIPE_MODE_SWITCH_CMD_RETURN		1
#define HIPE_MODE_SWITCH_CMD_THROW		2
#define HIPE_MODE_SWITCH_CMD_RESUME		3

/* result codes for beam_emu <- hipe_mode_switch() return */
#define HIPE_MODE_SWITCH_RES_RETURN		4
#define HIPE_MODE_SWITCH_RES_CALL_EXPORTED	5
#define HIPE_MODE_SWITCH_RES_THROW		6

/* additional result codes for hipe_mode_switch() <- native return */
#define HIPE_MODE_SWITCH_RES_SUSPEND		7
#define HIPE_MODE_SWITCH_RES_WAIT		8
#define HIPE_MODE_SWITCH_RES_WAIT_TIMEOUT	9
#define HIPE_MODE_SWITCH_RES_TRAP		10

#define HIPE_MODE_SWITCH_CMD_CALL_CLOSURE	11 /* BEAM -> mode_switch */
#define HIPE_MODE_SWITCH_RES_CALL_CLOSURE	12 /* mode_switch <- native */

#define HIPE_MODE_SWITCH_RES_APPLY		13 /* mode_switch <- native */

#define HIPE_MODE_SWITCH_RES_CALL_BEAM	        14

#ifndef ASM

#include "error.h"

extern int hipe_modeswitch_debug;

void hipe_mode_switch_init(void);
void hipe_set_call_trap(ErtsCodeInfo*, void *nfun, int is_closure);
Process *hipe_mode_switch(Process*, unsigned, Eterm*);
void hipe_inc_nstack(Process *p);
void hipe_empty_nstack(Process *p);
void hipe_set_closure_stub(ErlFunEntry *fe);
Eterm hipe_build_stacktrace(Process *p, struct StackTrace *s);

ERTS_GLB_INLINE void hipe_reserve_beam_trap_frame(Process*, Eterm reg[], unsigned arity);
ERTS_GLB_INLINE void hipe_unreserve_beam_trap_frame(Process*);

extern Uint hipe_beam_pc_return[];
extern Uint hipe_beam_pc_throw[];
extern Uint hipe_beam_pc_resume[];

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

#include "erl_gc.h"
#include "hipe_stack.h"

#if defined(__sparc__)
#include "hipe_sparc_glue.h"
#elif defined(__i386__)
#include "hipe_x86_glue.h"
#elif defined(__x86_64__)
#include "hipe_amd64_glue.h"
#elif defined(__powerpc__) || defined(__ppc__) || defined(__powerpc64__)
#include "hipe_ppc_glue.h"
#elif defined(__arm__)
#include "hipe_arm_glue.h"
#endif

extern Eterm hipe_beam_catch_throw;

ERTS_GLB_INLINE void hipe_reserve_beam_trap_frame(Process *p, Eterm reg[], unsigned arity)
{
    if (!hipe_bifcall_from_native_is_recursive(p))
	return;

    /* ensure that at least 2 words are available on the BEAM stack */
    if ((p->stop - 2) < p->htop) {
	erts_garbage_collect(p, 2, reg, arity);
	ASSERT(!((p->stop - 2) < p->htop));
    }
    p->stop -= 2;
    p->stop[0] = NIL;
    p->stop[1] = hipe_beam_catch_throw;
}

ERTS_GLB_INLINE void hipe_unreserve_beam_trap_frame(Process *p)
{
    if (!hipe_bifcall_from_native_is_recursive(p))
	return;

    ASSERT(p->stop[0] == NIL && p->stop[1] == hipe_beam_catch_throw);
    p->stop += 2;
}

#endif /* ERTS_GLB_INLINE_INCL_FUNC_DEF */

#endif	/* ASM */

#endif /* HIPE_MODE_SWITCH_H */
