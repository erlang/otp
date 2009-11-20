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
#ifndef HIPE_X86_H
#define HIPE_X86_H

static __inline__ void hipe_flush_icache_word(void *address)
{
    /* Do nothing. This works as long as compiled code is
       executed by a single CPU thread. */
}

static __inline__ void
hipe_flush_icache_range(void *address, unsigned int nbytes)
{
    /* Do nothing. This works as long as compiled code is
       executed by a single CPU thread. */
}

/* for stack descriptor hash lookup */
#define HIPE_RA_LSR_COUNT	0	/* all bits are significant */

/* for hipe_bifs_{read,write}_{s,u}32 */
static __inline__ int hipe_word32_address_ok(void *address)
{
    return 1;
}

/* Native stack growth direction. */
#define HIPE_NSTACK_GROWS_DOWN

#define hipe_arch_name	am_x86

extern void nbif_inc_stack_0(void);
extern void nbif_handle_fp_exception(void);

/* for hipe_bifs_enter_code_2 */
extern void *hipe_alloc_code(Uint nrbytes, Eterm callees, Eterm *trampolines, Process *p);
#define HIPE_ALLOC_CODE(n,c,t,p) hipe_alloc_code((n),(c),(t),(p))

#endif /* HIPE_X86_H */
