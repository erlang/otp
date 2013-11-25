/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2013. All Rights Reserved.
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

#ifndef ERL_MMAP_H__
#define ERL_MMAP_H__

#include "sys.h"

#define ERTS_MMAP_SUPERALIGNED_BITS (18)
/* Affects hard limits for sbct and lmbcs documented in erts_alloc.xml */

#define ERTS_MMAPFLG_OS_ONLY			(((Uint32) 1) << 0)
#define ERTS_MMAPFLG_SUPERCARRIER_ONLY		(((Uint32) 1) << 1)
#define ERTS_MMAPFLG_SUPERALIGNED		(((Uint32) 1) << 2)

#define ERTS_HAVE_ERTS_OS_MMAP			(1 << 0)
#define ERTS_HAVE_ERTS_SUPERCARRIER_MMAP	(1 << 1)
extern int erts_have_erts_mmap;
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

void *erts_mmap(Uint32 flags, UWord *sizep);
void erts_munmap(Uint32 flags, void *ptr, UWord size);
void *erts_mremap(Uint32 flags, void *ptr, UWord old_size, UWord *sizep);
int erts_mmap_in_supercarrier(void *ptr);
void erts_mmap_init(ErtsMMapInit*);
struct erts_mmap_info_struct
{
    UWord sizes[6];
    UWord segs[6];
    UWord os_used;
};
Eterm erts_mmap_info(int *print_to_p, void *print_to_arg,
                     Eterm** hpp, Uint* szp, struct erts_mmap_info_struct*);
Eterm erts_mmap_info_options(char *prefix, int *print_to_p, void *print_to_arg,
                             Uint **hpp, Uint *szp);
struct process;
Eterm erts_mmap_debug_info(struct process*);

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
#  if defined(MAP_FIXED) && defined(MAP_NORESERVE)
#    define ERTS_HAVE_OS_PHYSICAL_MEMORY_RESERVATION 1
#  endif
#endif

#ifndef HAVE_VIRTUALALLOC
#  define HAVE_VIRTUALALLOC 0
#endif
#if HAVE_VIRTUALALLOC
#  define ERTS_HAVE_OS_MMAP 1
#endif

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

#endif /* ERL_MMAP_H__ */
