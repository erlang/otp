/*
 * %CopyrightBegin%

 *
 * Copyright Ericsson AB 2001-2011. All Rights Reserved.
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
#define HIPE_MODE_SWITCH_RES_CALL		5
#define HIPE_MODE_SWITCH_RES_THROW		6

/* additional result codes for hipe_mode_switch() <- native return */
#define HIPE_MODE_SWITCH_RES_SUSPEND		7
#define HIPE_MODE_SWITCH_RES_WAIT		8
#define HIPE_MODE_SWITCH_RES_WAIT_TIMEOUT	9
#define HIPE_MODE_SWITCH_RES_TRAP		10

#define HIPE_MODE_SWITCH_CMD_CALL_CLOSURE	11 /* BEAM -> mode_switch */
#define HIPE_MODE_SWITCH_RES_CALL_CLOSURE	12 /* mode_switch <- native */

#define HIPE_MODE_SWITCH_RES_APPLY		13 /* mode_switch <- native */

#ifndef ASM

#include "error.h"

extern int hipe_modeswitch_debug;

void hipe_mode_switch_init(void);
void hipe_set_call_trap(Uint *bfun, void *nfun, int is_closure);
Process *hipe_mode_switch(Process*, unsigned, Eterm*);
void hipe_inc_nstack(Process *p);
void hipe_empty_nstack(Process *p);
void hipe_set_closure_stub(ErlFunEntry *fe, unsigned num_free);
Eterm hipe_build_stacktrace(Process *p, struct StackTrace *s);

void hipe_reserve_beam_trap_frame(Process*, Eterm reg[], unsigned arity);
void hipe_unreserve_beam_trap_frame(Process*);

extern Uint hipe_beam_pc_return[];
extern Uint hipe_beam_pc_throw[];
extern Uint hipe_beam_pc_resume[];

#endif	/* ASM */

#endif /* HIPE_MODE_SWITCH_H */
