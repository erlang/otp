/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2011. All Rights Reserved.
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
 * Description: Memory barriers for PowerPC
 * Author: Rickard Green
 */

#ifndef ETHR_PPC_MEMBAR_H__
#define ETHR_PPC_MEMBAR_H__

#define ETHR_LoadLoad	(1 << 0)
#define ETHR_LoadStore	(1 << 1)
#define ETHR_StoreLoad	(1 << 2)
#define ETHR_StoreStore	(1 << 3)

static __inline__ void
ethr_lwsync__(void)
{
#ifdef ETHR_PPC_HAVE_NO_LWSYNC
    __asm__ __volatile__ ("sync\n\t" : : : "memory");
#else
#ifndef ETHR_PPC_HAVE_LWSYNC
    if (ETHR_PPC_RUNTIME_CONF_HAVE_NO_LWSYNC__)
	__asm__ __volatile__ ("sync\n\t" : : : "memory");
    else
#endif
	__asm__ __volatile__ ("lwsync\n\t" : : : "memory");
#endif
}

static __inline__ void
ethr_sync__(void)
{
    __asm__ __volatile__ ("sync\n\t" : : : "memory");
}

/*
 * According to the "memory barrier intstructions" section of
 * http://www.ibm.com/developerworks/systems/articles/powerpc.html
 * we want to use sync when a StoreLoad is needed and lwsync for
 * everything else.
 */
#define ETHR_MEMBAR(B) \
  ETHR_CHOOSE_EXPR((B) & ETHR_StoreLoad, ethr_sync__(), ethr_lwsync__())

#endif
