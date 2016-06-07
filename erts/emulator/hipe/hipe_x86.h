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
#ifndef NO_FPE_SIGNALS
extern void nbif_handle_fp_exception(void);
#endif

#endif /* HIPE_X86_H */
