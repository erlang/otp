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
 * Description: Memory barriers for sparc-v9
 * Author: Rickard Green
 */

#ifndef ETHR_SPARC_V9_MEMBAR_H__
#define ETHR_SPARC_V9_MEMBAR_H__

#if defined(ETHR_SPARC_TSO)
/* --- Total Store Order ------------------------------------------------ */

#define ETHR_LoadLoad	0
#define ETHR_LoadStore	0
#define ETHR_StoreLoad	1
#define ETHR_StoreStore	0

static __inline__ void
ethr_cb__(void)
{
    __asm__ __volatile__ ("" : : : "memory");
}

static __inline__ void
ethr_StoreLoad__(void)
{
    __asm__ __volatile__ ("membar #StoreLoad\n\t" : : : "memory");
}


#define ETHR_MEMBAR(B) \
  ETHR_CHOOSE_EXPR((B), ethr_StoreLoad__(), ethr_cb__())

#elif defined(ETHR_SPARC_PSO)
/* --- Partial Store Order ---------------------------------------------- */

#define ETHR_LoadLoad	0
#define ETHR_LoadStore	0
#define ETHR_StoreLoad	(1 << 0)
#define ETHR_StoreStore	(1 << 1)

static __inline__ void
ethr_cb__(void)
{
    __asm__ __volatile__ ("" : : : "memory");
}

static __inline__ void
ethr_StoreLoad__(void)
{
    __asm__ __volatile__ ("membar #StoreLoad\n\t" : : : "memory");
}

static __inline__ void
ethr_StoreStore__(void)
{
    __asm__ __volatile__ ("membar #StoreStore\n\t" : : : "memory");
}

static __inline__ void
ethr_StoreLoad_StoreStore__(void)
{
    __asm__ __volatile__ ("membar #StoreLoad|StoreStore\n\t" : : : "memory");
}

#define ETHR_MEMBAR(B)					\
  ETHR_CHOOSE_EXPR(					\
      (B) == ETHR_StoreLoad,				\
      ethr_StoreLoad__(),				\
      ETHR_CHOOSE_EXPR(					\
	  (B) == ETHR_StoreStore,			\
	  ethr_StoreStore__(),				\
	  ETHR_CHOOSE_EXPR(				\
	      (B) == (ETHR_StoreLoad|ETHR_StoreStore),	\
	      ethr_StoreLoad_StoreStore__(),		\
	      ethr_cb__())))

#elif defined(ETHR_SPARC_RMO)
/* --- Relaxed Memory Order --------------------------------------------- */

#  define ETHR_LoadLoad #LoadLoad
#  define ETHR_LoadStore #LoadStore
#  define ETHR_StoreLoad #StoreLoad
#  define ETHR_StoreStore #StoreStore

#  define ETHR_MEMBAR_AUX__(B) \
     __asm__ __volatile__("membar " #B "\n\t" : : : "memory")

#  define ETHR_MEMBAR(B) ETHR_MEMBAR_AUX__(B)

#else

#  error "No memory order defined"

#endif

#endif
