/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2004-2009. All Rights Reserved.
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
 *

 */
#include "ei.h"
#include "ei_internal.h"

#define EI_COMPAT_NO_REL (~((unsigned) 0))

static unsigned compat_rel = EI_COMPAT_NO_REL;

void
ei_set_compat_rel(unsigned rel)
{
    if (compat_rel == EI_COMPAT_NO_REL)
	compat_rel = rel;
}

int
ei_internal_use_r9_pids_ports(void)
{
    return compat_rel < 10;
}
