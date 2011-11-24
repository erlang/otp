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


#ifndef HIPE_AMD64_H
#define HIPE_AMD64_H

#include "hipe_x86.h"
#undef hipe_arch_name

/* for hipe_bifs_{read,write}_{s,u}64 */
static __inline__ int hipe_word64_address_ok(void *address)
{
    return 1;
}

#define hipe_arch_name	am_amd64

extern const Uint sse2_fnegate_mask[];

#endif /* HIPE_AMD64_H */
