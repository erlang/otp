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
/* hipe_bif_64.c
 *
 * Compiler and linker support. 64-bit specific.
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
#include "global.h"
#include "error.h"
#include "bif.h"
#include "big.h"	/* term_to_Sint() */
#include "hipe_arch.h"
#include "hipe_bif0.h"
#include "hipe_bif64.h"

#if 0 /* unused */
static int term_to_Sint64(Eterm term, Sint64 *sp)
{
    return term_to_Sint(term, sp);
}

BIF_RETTYPE hipe_bifs_write_s64_2(BIF_ALIST_2)
{
    Sint64 *address;
    Sint64 value;

    address = term_to_address(BIF_ARG_1);
    if (!address || !hipe_word64_address_ok(address))
	BIF_ERROR(BIF_P, BADARG);
    if (!term_to_Sint64(BIF_ARG_2, &value))
	BIF_ERROR(BIF_P, BADARG);
    *address = value;
    BIF_RET(NIL);
}
#endif

BIF_RETTYPE hipe_bifs_write_u64_2(BIF_ALIST_2)
{
    Uint64 *address;
    Uint64 value;

    address = term_to_address(BIF_ARG_1);
    if (!address || !hipe_word64_address_ok(address))
	BIF_ERROR(BIF_P, BADARG);
    if (!term_to_Uint(BIF_ARG_2, &value))
	BIF_ERROR(BIF_P, BADARG);
    *address = value;
    hipe_flush_icache_word(address);
    BIF_RET(NIL);
}
