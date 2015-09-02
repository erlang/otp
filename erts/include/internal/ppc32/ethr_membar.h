/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2011. All Rights Reserved.
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
