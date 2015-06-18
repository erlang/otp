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
 * Description: Memory barriers when using libatomic_ops
 * Author: Rickard Green
 */

#ifndef ETHR_LIBATOMIC_OPS_MEMBAR_H__
#define ETHR_LIBATOMIC_OPS_MEMBAR_H__

#define ETHR_LoadLoad	(1 << 0)
#define ETHR_LoadStore	(1 << 1)
#define ETHR_StoreLoad	(1 << 2)
#define ETHR_StoreStore	(1 << 3)

#ifndef AO_HAVE_nop_full
#  error "No AO_nop_full()"
#endif

static __inline__ void
ethr_mb__(void)
{
    AO_nop_full();
}

static __inline__ void
ethr_rb__(void)
{
#ifdef AO_HAVE_nop_read
    AO_nop_read();
#else
    AO_nop_full();
#endif
}

static __inline__ void
ethr_wb__(void)
{
#ifdef AO_HAVE_nop_write
    AO_nop_write();
#else
    AO_nop_full();
#endif
}

#define ETHR_MEMBAR(B)						\
  ETHR_CHOOSE_EXPR((B) == ETHR_StoreStore,			\
		   ethr_wb__(),					\
		   ETHR_CHOOSE_EXPR((B) == ETHR_LoadLoad,	\
				    ethr_rb__(),		\
				    ethr_mb__()))

#define ETHR_COMPILER_BARRIER AO_compiler_barrier()
#ifdef AO_NO_DD_ORDERING
#  define ETHR_READ_DEPEND_MEMORY_BARRIER ETHR_READ_MEMORY_BARRIER
#endif

#endif /* ETHR_LIBATOMIC_OPS_MEMBAR_H__ */
