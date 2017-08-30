/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1996-2016. All Rights Reserved.
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

#ifndef __ERL_VM_H__
#define __ERL_VM_H__

/* FORCE_HEAP_FRAGS:
 * Debug provocation to make HAlloc always create heap fragments (if allowed)
 * even if there is room on heap.
 */
/* #define FORCE_HEAP_FRAGS */


#if defined(DEBUG) && !defined(CHECK_FOR_HOLES) && !defined(__WIN32__)
# define CHECK_FOR_HOLES
#endif

#define BEAM 1
#define EMULATOR "BEAM"
#define SEQ_TRACE 1

#define CONTEXT_REDS 2000	/* Swap process out after this number */
#define MAX_ARG 255	        /* Max number of arguments allowed */
#define MAX_REG 1024            /* Max number of x(N) registers used */

/*
 * The new arithmetic operations need some extra X registers in the register array.
 * so does the gc_bif's (i_gc_bif3 need 3 extra).
 */
#define ERTS_X_REGS_ALLOCATED (MAX_REG+3)

#define INPUT_REDUCTIONS (2 * CONTEXT_REDS)

#define H_DEFAULT_SIZE  233        /* default (heap + stack) min size */
#define VH_DEFAULT_SIZE  32768     /* default virtual (bin) heap min size (words) */
#define H_DEFAULT_MAX_SIZE 0       /* default max heap size is off */

#define CP_SIZE 1

#define ErtsHAllocLockCheck(P) \
  ERTS_SMP_LC_ASSERT(erts_dbg_check_halloc_lock((P)))


#ifdef DEBUG
/*
 * Debug HAlloc that initialize all memory to bad things.
 *
 * To get information about where memory is allocated, insert the two
 * lines below directly after the memset line and use the flag +va.
 *
         VERBOSE(DEBUG_ALLOCATION,("HAlloc @ 0x%08lx (%d) %s:%d\n",     \
                 (unsigned long)HEAP_TOP(p),(sz),__FILE__,__LINE__)),   \
 */
#  ifdef CHECK_FOR_HOLES
#    define INIT_HEAP_MEM(p,sz) erts_set_hole_marker(HEAP_TOP(p), (sz))
#  else
#    define INIT_HEAP_MEM(p,sz) memset(HEAP_TOP(p),0x01,(sz)*sizeof(Eterm*))
#  endif
#else
#  define INIT_HEAP_MEM(p,sz) ((void)0)
#endif /* DEBUG */


#ifdef FORCE_HEAP_FRAGS
#  define IS_FORCE_HEAP_FRAGS 1
#else
#  define IS_FORCE_HEAP_FRAGS 0
#endif

/*
 * Allocate heap memory, first on the ordinary heap;
 * failing that, in a heap fragment.
 */
#define HAllocX(p, sz, xtra)		                              \
  (ASSERT((sz) >= 0),					              \
     ErtsHAllocLockCheck(p),					      \
     (IS_FORCE_HEAP_FRAGS || (((HEAP_LIMIT(p) - HEAP_TOP(p)) < (sz))) \
      ? erts_heap_alloc((p),(sz),(xtra))                              \
      : (INIT_HEAP_MEM(p,sz),		                              \
         HEAP_TOP(p) = HEAP_TOP(p) + (sz), HEAP_TOP(p) - (sz))))

#define HAlloc(P, SZ) HAllocX(P,SZ,0)

#define HRelease(p, endp, ptr)					\
  if ((ptr) == (endp)) {					\
     ;								\
  } else if (HEAP_START(p) <= (ptr) && (ptr) < HEAP_TOP(p)) {	\
     HEAP_TOP(p) = (ptr);					\
  } else {							\
     erts_heap_frag_shrink(p, ptr);					\
  }

#define HeapWordsLeft(p) (HEAP_LIMIT(p) - HEAP_TOP(p))

#if defined(DEBUG) || defined(CHECK_FOR_HOLES)
# define ERTS_HOLE_MARKER (((0xdeadbeef << 24) << 8) | 0xdeadbeef)
#endif /* egil: 32-bit ? */

/*
 * Allocate heap memory on the ordinary heap, NEVER in a heap
 * segment. The caller must ensure that there is enough words
 * left on the heap before calling HeapOnlyAlloc() (for instance,
 * by testing HeapWordsLeft() and calling the garbage collector
 * if not enough).
 */
#ifdef CHECK_FOR_HOLES
# define HeapOnlyAlloc(p, sz)					\
    (ASSERT((sz) >= 0),					        \
     (ASSERT(((HEAP_LIMIT(p) - HEAP_TOP(p)) >= (sz))),	        \
      (erts_set_hole_marker(HEAP_TOP(p), (sz)),			\
       (HEAP_TOP(p) = HEAP_TOP(p) + (sz), HEAP_TOP(p) - (sz)))))
#else
# define HeapOnlyAlloc(p, sz)					\
    (ASSERT((sz) >= 0),					        \
     (ASSERT(((HEAP_LIMIT(p) - HEAP_TOP(p)) >= (sz))),	        \
      (HEAP_TOP(p) = HEAP_TOP(p) + (sz), HEAP_TOP(p) - (sz))))
#endif


/*
 * Description for each instruction (defined here because the name and
 * count fields are interesting outside the emulator proper).
 */

typedef struct op_entry {
   char* name;			/* Name of instruction. */
   Uint32 mask[3];		/* Signature mask. */
   unsigned involves_r;		/* Needs special attention when matching. */
   int sz;			/* Number of loaded words. */
   char* pack;			/* Instructions for packing engine. */
   char* sign;			/* Signature string. */
   unsigned count;		/* Number of times executed. */
} OpEntry;

extern OpEntry opc[];		/* Description of all instructions. */
extern int num_instructions;	/* Number of instruction in opc[]. */

/* some constants for various table sizes etc */

#define ATOM_TEXT_SIZE  32768	/* Increment for allocating atom text space */

#define ITIME 100		/* Number of milliseconds per clock tick    */
#define MAX_PORT_LINK 8		/* Maximum number of links to a port        */

extern int H_MIN_SIZE;		/* minimum (heap + stack) */
extern int BIN_VH_MIN_SIZE;	/* minimum virtual (bin) heap */
extern int H_MAX_SIZE;          /* maximum (heap + stack) */
extern int H_MAX_FLAGS;         /* maximum heap flags  */

extern int erts_atom_table_size;/* Atom table size */
extern int erts_pd_initial_size;/* Initial Process dictionary table size */

#define ORIG_CREATION 0

/* macros for extracting bytes from uint16's */

#define hi_byte(a) ((a) >> 8) 
#define lo_byte(a) ((a) & 255) 

/* macros for combining bytes */

#define make_16(x, y) (((x) << 8) | (y))
#define make_24(x,y,z) (((x) << 16) | ((y) << 8) | (z))
#define make_32(x3,x2,x1,x0) (((x3)<<24) | ((x2)<<16) | ((x1)<<8) | (x0))

#define make_signed_24(x,y,z) ((sint32) (((x) << 24) | ((y) << 16) | ((z) << 8)) >> 8)
#define make_signed_32(x3,x2,x1,x0) ((sint32) (((x3) << 24) | ((x2) << 16) | ((x1) << 8) | (x0)))

#include "erl_term.h"

#endif	/* __ERL_VM_H__ */
