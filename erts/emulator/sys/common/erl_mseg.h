/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2002-2018. All Rights Reserved.
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

#ifndef ERL_MSEG_H_
#define ERL_MSEG_H_

#include "sys.h"
#include "erl_alloc_types.h"
#include "erl_mmap.h"

/*
 * We currently only enable mseg_alloc if we got
 * a genuine mmap()/munmap() primitive. It is possible
 * to utilize erts_mmap() withiout a mmap support but
 * alloc_util needs to be prepared before we can do
 * that.
 */
#ifdef ERTS_HAVE_GENUINE_OS_MMAP
#  define HAVE_ERTS_MSEG 1
#  define ERTS_HAVE_MSEG_SUPER_ALIGNED 1
#else
#  define HAVE_ERTS_MSEG 0
#  define ERTS_HAVE_MSEG_SUPER_ALIGNED 0
#endif

#if ERTS_HAVE_MSEG_SUPER_ALIGNED
#  define MSEG_ALIGN_BITS ERTS_MMAP_SUPERALIGNED_BITS
#endif

#if HAVE_ERTS_MSEG

#define MSEG_ALIGNED_SIZE     (1 << MSEG_ALIGN_BITS)

#define ERTS_MSEG_FLG_NONE    ((Uint)(0))
#define ERTS_MSEG_FLG_2POW    ((Uint)(1 << 0))


#define ERTS_MSEG_VSN_STR "0.9"

typedef struct {
    Uint amcbf;
    Uint rmcbf;
    Uint mcs;
    Uint nos;
    ErtsMMapInit dflt_mmap;
    ErtsMMapInit literal_mmap;
} ErtsMsegInit_t;

#define ERTS_MSEG_INIT_DEFAULT_INITIALIZER				\
{									\
    4*1024*1024,	/* amcbf: Absolute max cache bad fit	*/	\
    20,			/* rmcbf: Relative max cache bad fit	*/	\
    10,			/* mcs:   Max cache size		*/	\
    1000,		/* cci:   Cache check interval		*/	\
    ERTS_MMAP_INIT_DEFAULT_INITER,					\
    ERTS_MMAP_INIT_LITERAL_INITER,                                      \
}

typedef struct {
    int  cache;
    int  preserv;
    UWord abs_shrink_th;
    UWord rel_shrink_th;
    int sched_spec;
} ErtsMsegOpt_t;

extern const ErtsMsegOpt_t erts_mseg_default_opt;

void *erts_mseg_alloc(ErtsAlcType_t, UWord *, Uint);
void *erts_mseg_alloc_opt(ErtsAlcType_t, UWord *, Uint, const ErtsMsegOpt_t *);
void  erts_mseg_dealloc(ErtsAlcType_t, void *, UWord, Uint);
void  erts_mseg_dealloc_opt(ErtsAlcType_t, void *, UWord, Uint, const ErtsMsegOpt_t *);
void *erts_mseg_realloc(ErtsAlcType_t, void *, UWord, UWord *, Uint);
void *erts_mseg_realloc_opt(ErtsAlcType_t, void *, UWord, UWord *, Uint, const ErtsMsegOpt_t *);
void  erts_mseg_clear_cache(void);
void  erts_mseg_cache_check(void);
Uint  erts_mseg_no( const ErtsMsegOpt_t *);
Uint  erts_mseg_unit_size(void);
void  erts_mseg_init(ErtsMsegInit_t *init);
void  erts_mseg_late_init(void); /* Have to be called after all allocators,
				   threads and timers have been initialized. */
Eterm erts_mseg_info_options(int, fmtfn_t*, void*, Uint **, Uint *);
Eterm erts_mseg_info(int, fmtfn_t *, void*, int, int, Uint **, Uint *);

#endif /* #if HAVE_ERTS_MSEG */

UWord erts_mseg_test(UWord, UWord, UWord, UWord);

#endif /* #ifndef ERL_MSEG_H_ */
