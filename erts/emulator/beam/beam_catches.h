/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2000-2013. All Rights Reserved.
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

#ifndef __BEAM_CATCHES_H
#define __BEAM_CATCHES_H

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif
#include "sys.h"
#include "code_ix.h"

#define BEAM_CATCHES_NIL	(-1)

void beam_catches_init(void);
void beam_catches_start_staging(void);
void beam_catches_end_staging(int commit);
unsigned beam_catches_cons(BeamInstr* cp, unsigned cdr);
BeamInstr *beam_catches_car(unsigned i);
void beam_catches_delmod(unsigned head, BeamInstr* code, unsigned code_bytes,
			 ErtsCodeIndex);

#define catch_pc(x)	beam_catches_car(catch_val((x)))

#endif	/* __BEAM_CATCHES_H */
