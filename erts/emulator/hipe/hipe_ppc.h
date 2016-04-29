/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2004-2016. All Rights Reserved.
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


#ifndef HIPE_PPC_H
#define HIPE_PPC_H

static __inline__ void hipe_flush_icache_word(void *address)
{
    asm volatile("dcbst 0,%0\n"
		 "\tsync\n"
		 "\ticbi 0,%0\n"
		 "\tsync\n"
		 "\tisync"
		 :
		 : "r"(address)
		 : "memory");
}

extern void hipe_flush_icache_range(void *address, unsigned int nbytes);

/* for stack descriptor hash lookup */
#define HIPE_RA_LSR_COUNT	2	/* low 2 bits are always zero */

/* for hipe_bifs_{read,write}_{s,u}32 */
static __inline__ int hipe_word32_address_ok(void *address)
{
    return ((unsigned long)address & 0x3) == 0;
}

#if defined(__powerpc64__)
/* for hipe_bifs_{read,write}_{s,u}64 */
static __inline__ int hipe_word64_address_ok(void *address)
{
    return ((unsigned long)address & 0x7) == 0;
}
#endif

/* Native stack growth direction. */
#define HIPE_NSTACK_GROWS_DOWN

#if defined(__powerpc64__)
#define hipe_arch_name	am_ppc64
#define AEXTERN(RET,NAME,PROTO)	extern const int NAME[]
AEXTERN(void,hipe_ppc_inc_stack,(void));
#else
#define hipe_arch_name	am_powerpc
extern void hipe_ppc_inc_stack(void); /* we don't have the AEXTERN() fallback :-( */
#endif

#if !defined(__powerpc64__)
extern const unsigned int fconv_constant[];
#endif

#endif /* HIPE_PPC_H */
