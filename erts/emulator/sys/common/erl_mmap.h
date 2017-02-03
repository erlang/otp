/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2013-2016. All Rights Reserved.
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

#ifndef ERL_MMAP_H__
#define ERL_MMAP_H__

#include "sys.h"
#include "erl_printf.h"

#define ERTS_MMAP_SUPERALIGNED_BITS (18)
/* Affects hard limits for sbct and lmbcs documented in erts_alloc.xml */

#ifndef HAVE_MMAP
#  define HAVE_MMAP 0
#endif
#ifndef HAVE_MREMAP
#  define HAVE_MREMAP 0
#endif
#if HAVE_MMAP
#  define ERTS_HAVE_OS_MMAP 1
#  define ERTS_HAVE_GENUINE_OS_MMAP 1
#  if HAVE_MREMAP
#    define ERTS_HAVE_OS_MREMAP 1
#  endif
/*
 * MAP_NORESERVE is undefined in FreeBSD 10.x and later.
 * This is to enable 64bit HiPE experimentally on FreeBSD.
 * Note that on FreeBSD MAP_NORESERVE was "never implemented"
 * even before 11.x (and the flag does not exist in /usr/src/sys/vm/mmap.c
 * of 10.3-STABLE r301478 either), and HiPE was working on OTP 18.3.3,
 * so mandating MAP_NORESERVE on FreeBSD might not be needed.
 * See the following message on how MAP_NORESERVE was treated on FreeBSD:
 * <http://lists.llvm.org/pipermail/cfe-commits/Week-of-Mon-20150202/122958.html>
 */
#  if defined(MAP_FIXED) && (defined(MAP_NORESERVE) || defined(__FreeBSD__))
#    define ERTS_HAVE_OS_PHYSICAL_MEMORY_RESERVATION 1
#  endif
#endif

#ifndef HAVE_VIRTUALALLOC
#  define HAVE_VIRTUALALLOC 0
#endif
#if HAVE_VIRTUALALLOC
#  define ERTS_HAVE_OS_MMAP 1
#endif

#ifdef ERTS_HAVE_GENUINE_OS_MMAP
#  define HAVE_ERTS_MMAP 1
#else
#  define HAVE_ERTS_MMAP 0
#endif


extern UWord erts_page_inv_mask;

typedef struct {
    struct {
	char *start;
	char *end;
    } virtual_range;
    struct {
	char *start;
	char *end;
    } predefined_area;
    UWord scs;  /* super carrier size */
    int sco;    /* super carrier only? */
    UWord scrfsd; /* super carrier reserved free segment descriptors */
    int scrpm; /* super carrier reserve physical memory */
}ErtsMMapInit;

#define ERTS_MMAP_INIT_DEFAULT_INITER \
    {{NULL, NULL}, {NULL, NULL}, 0, 1, (1 << 16), 1}

#define ERTS_LITERAL_VIRTUAL_AREA_SIZE (UWORD_CONSTANT(1)*1024*1024*1024)

#define ERTS_MMAP_INIT_LITERAL_INITER \
    {{NULL, NULL}, {NULL, NULL}, ERTS_LITERAL_VIRTUAL_AREA_SIZE, 1, (1 << 10), 0}

#define ERTS_HIPE_EXEC_VIRTUAL_AREA_SIZE (UWORD_CONSTANT(512)*1024*1024)

#define ERTS_MMAP_INIT_HIPE_EXEC_INITER \
    {{NULL, NULL}, {NULL, NULL}, ERTS_HIPE_EXEC_VIRTUAL_AREA_SIZE, 1, (1 << 10), 0}


#define ERTS_SUPERALIGNED_SIZE \
    (1 << ERTS_MMAP_SUPERALIGNED_BITS)
#define ERTS_INV_SUPERALIGNED_MASK \
    ((UWord) (ERTS_SUPERALIGNED_SIZE - 1))
#define ERTS_SUPERALIGNED_MASK \
    (~ERTS_INV_SUPERALIGNED_MASK)
#define ERTS_SUPERALIGNED_FLOOR(X) \
    (((UWord) (X)) & ERTS_SUPERALIGNED_MASK)
#define ERTS_SUPERALIGNED_CEILING(X) \
    ERTS_SUPERALIGNED_FLOOR((X) + ERTS_INV_SUPERALIGNED_MASK)
#define ERTS_IS_SUPERALIGNED(X) \
    (((UWord) (X) & ERTS_INV_SUPERALIGNED_MASK) == 0)

#define ERTS_INV_PAGEALIGNED_MASK \
    (erts_page_inv_mask)
#define ERTS_PAGEALIGNED_MASK \
    (~ERTS_INV_PAGEALIGNED_MASK)
#define ERTS_PAGEALIGNED_FLOOR(X) \
    (((UWord) (X)) & ERTS_PAGEALIGNED_MASK)
#define ERTS_PAGEALIGNED_CEILING(X) \
    ERTS_PAGEALIGNED_FLOOR((X) + ERTS_INV_PAGEALIGNED_MASK)
#define ERTS_IS_PAGEALIGNED(X) \
    (((UWord) (X) & ERTS_INV_PAGEALIGNED_MASK) == 0)
#define ERTS_PAGEALIGNED_SIZE \
    (ERTS_INV_PAGEALIGNED_MASK + 1)

struct process;
Eterm erts_mmap_debug_info(struct process*);

#if HAVE_ERTS_MMAP

typedef struct ErtsMemMapper_ ErtsMemMapper;

#define ERTS_MMAPFLG_OS_ONLY			(((Uint32) 1) << 0)
#define ERTS_MMAPFLG_SUPERCARRIER_ONLY		(((Uint32) 1) << 1)
#define ERTS_MMAPFLG_SUPERALIGNED		(((Uint32) 1) << 2)

void *erts_mmap(ErtsMemMapper*, Uint32 flags, UWord *sizep);
void erts_munmap(ErtsMemMapper*, Uint32 flags, void *ptr, UWord size);
void *erts_mremap(ErtsMemMapper*, Uint32 flags, void *ptr, UWord old_size, UWord *sizep);
int erts_mmap_in_supercarrier(ErtsMemMapper*, void *ptr);
void erts_mmap_init(ErtsMemMapper*, ErtsMMapInit*, int executable);
struct erts_mmap_info_struct
{
    UWord sizes[6];
    UWord segs[6];
    UWord os_used;
};
Eterm erts_mmap_info(ErtsMemMapper*, fmtfn_t *print_to_p, void *print_to_arg,
                     Eterm** hpp, Uint* szp, struct erts_mmap_info_struct*);
Eterm erts_mmap_info_options(ErtsMemMapper*,
                             char *prefix, fmtfn_t *print_to_p, void *print_to_arg,
                             Uint **hpp, Uint *szp);


#ifdef ERTS_WANT_MEM_MAPPERS
#  include "erl_alloc_types.h"

extern ErtsMemMapper erts_dflt_mmapper;

# if defined(ERTS_HAVE_OS_PHYSICAL_MEMORY_RESERVATION)

#  if defined(ARCH_64)
extern ErtsMemMapper erts_literal_mmapper;
#  endif

#  if defined(ERTS_ALC_A_EXEC) && defined(__x86_64__)
   /*
    * On x86_64, exec_alloc employs its own super carrier 'erts_exec_mmaper'
    * to ensure low memory for HiPE AMD64 small code model.
    */
#   define ERTS_HAVE_EXEC_MMAPPER
extern ErtsMemMapper erts_exec_mmapper;
#  endif

# endif /* ERTS_HAVE_OS_PHYSICAL_MEMORY_RESERVATION */
#endif /* ERTS_WANT_MEM_MAPPERS */

/*#define HARD_DEBUG_MSEG*/
#ifdef HARD_DEBUG_MSEG
#  define HARD_DBG_INSERT_MSEG hard_dbg_insert_mseg
#  define HARD_DBG_REMOVE_MSEG hard_dbg_remove_mseg
void hard_dbg_insert_mseg(void* seg, UWord sz);
void hard_dbg_remove_mseg(void* seg, UWord sz);
#else
#  define HARD_DBG_INSERT_MSEG(SEG,SZ)
#  define HARD_DBG_REMOVE_MSEG(SEG,SZ)
#endif

#endif /* HAVE_ERTS_MMAP */

#endif /* ERL_MMAP_H__ */
