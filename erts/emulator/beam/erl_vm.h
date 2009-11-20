/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1996-2009. All Rights Reserved.
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

#ifndef __ERL_VM_H__
#define __ERL_VM_H__

/* #define ERTS_OPCODE_COUNTER_SUPPORT */

#if defined(HYBRID)
/* # define CHECK_FOR_HOLES */
#endif

#if defined(DEBUG) && !defined(CHECK_FOR_HOLES) && !defined(__WIN32__)
# define CHECK_FOR_HOLES
#endif

#if defined(HYBRID)
/* #  define INCREMENTAL 1    */ /* Incremental garbage collection */
/* #  define INC_TIME_BASED 1 */ /* Time-based incremental GC (vs Work-based) */
#endif

#define BEAM 1
#define EMULATOR "BEAM"
#define SEQ_TRACE 1

#define CONTEXT_REDS 2000	/* Swap process out after this number */
#define MAX_ARG 256	        /* Max number of arguments allowed */
#define MAX_REG 1024            /* Max number of x(N) registers used */

/*
 * The new arithmetic operations need some extra X registers in the register array.
 */
#define ERTS_X_REGS_ALLOCATED (MAX_REG+2)

#define INPUT_REDUCTIONS (2 * CONTEXT_REDS)

#define H_DEFAULT_SIZE  233     /* default (heap + stack) min size */

#ifdef HYBRID
#  define SH_DEFAULT_SIZE  2629425 /* default message area min size */
#endif

#ifdef INCREMENTAL
#  define INC_NoPAGES       256   /* Number of pages in the old generation */
#  define INC_PAGESIZE      32768 /* The size of each page */
#  define INC_STORAGE_SIZE  1024  /* The size of gray stack and similar */
#endif

#define CP_SIZE 1

#define ErtsHAllocLockCheck(P) \
  ERTS_SMP_LC_ASSERT((ERTS_PROC_LOCK_MAIN & erts_proc_lc_my_proc_locks((P))) \
		     || ((P)->scheduler_data \
			 && (P) == (P)->scheduler_data->match_pseudo_process) \
		     || erts_is_system_blocked(0))

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
#ifdef CHECK_FOR_HOLES
#define HAlloc(p, sz)						\
    (ASSERT_EXPR((sz) >= 0),					\
     ErtsHAllocLockCheck(p),					\
     ((((HEAP_LIMIT(p) - HEAP_TOP(p)) < (sz)))			\
      ? erts_heap_alloc((p),(sz))				\
      : (erts_set_hole_marker(HEAP_TOP(p), (sz)),		\
         HEAP_TOP(p) = HEAP_TOP(p) + (sz), HEAP_TOP(p) - (sz))))
#else
#define HAlloc(p, sz)                                                   \
    (ASSERT_EXPR((sz) >= 0),                                            \
     ErtsHAllocLockCheck(p),						\
     ((((HEAP_LIMIT(p) - HEAP_TOP(p)) < (sz)))                          \
      ? erts_heap_alloc((p),(sz))                                       \
      : (memset(HEAP_TOP(p),DEBUG_BAD_BYTE,(sz)*sizeof(Eterm*)),        \
         HEAP_TOP(p) = HEAP_TOP(p) + (sz), HEAP_TOP(p) - (sz))))
#endif
#else

/*
 * Allocate heap memory, first on the ordinary heap;
 * failing that, in a heap fragment.
 */
#define HAlloc(p, sz)                                                   \
    (ASSERT_EXPR((sz) >= 0),                                            \
     ErtsHAllocLockCheck(p),						\
     ((((HEAP_LIMIT(p) - HEAP_TOP(p)) < (sz)))                          \
      ? erts_heap_alloc((p),(sz))                                       \
      : (HEAP_TOP(p) = HEAP_TOP(p) + (sz), HEAP_TOP(p) - (sz))))

#endif /* DEBUG */

#if defined(CHECK_FOR_HOLES)
# define HRelease(p, endp, ptr)					\
  if ((ptr) == (endp)) {					\
     ;								\
  } else if (HEAP_START(p) <= (ptr) && (ptr) < HEAP_TOP(p)) {	\
     HEAP_TOP(p) = (ptr);					\
  } else {							\
     erts_arith_shrink(p, ptr);					\
  }
#else
# define HRelease(p, endp, ptr)					\
  if ((ptr) == (endp)) {					\
     ;								\
  } else if (HEAP_START(p) <= (ptr) && (ptr) < HEAP_TOP(p)) {	\
     HEAP_TOP(p) = (ptr);					\
  }
#endif

#define HeapWordsLeft(p) (HEAP_LIMIT(p) - HEAP_TOP(p))

#if defined(DEBUG) || defined(CHECK_FOR_HOLES)
# define ERTS_HOLE_MARKER (((0xaf5e78ccUL << 24) << 8) | 0xaf5e78ccUL)
#endif

/*
 * Allocate heap memory on the ordinary heap, NEVER in a heap
 * segment. The caller must ensure that there is enough words
 * left on the heap before calling HeapOnlyAlloc() (for instance,
 * by testing HeapWordsLeft() and calling the garbage collector
 * if not enough).
 */
#ifdef CHECK_FOR_HOLES
# define HeapOnlyAlloc(p, sz)					\
    (ASSERT_EXPR((sz) >= 0),					\
     (ASSERT_EXPR(((HEAP_LIMIT(p) - HEAP_TOP(p)) >= (sz))),	\
      (erts_set_hole_marker(HEAP_TOP(p), (sz)),			\
       (HEAP_TOP(p) = HEAP_TOP(p) + (sz), HEAP_TOP(p) - (sz)))))
#else
# define HeapOnlyAlloc(p, sz)					\
    (ASSERT_EXPR((sz) >= 0),					\
     (ASSERT_EXPR(((HEAP_LIMIT(p) - HEAP_TOP(p)) >= (sz))),	\
      (HEAP_TOP(p) = HEAP_TOP(p) + (sz), HEAP_TOP(p) - (sz))))
#endif


/*
 * Description for each instruction (defined here because the name and
 * count fields are interesting outside the emulator proper).
 */

typedef struct op_entry {
   char* name;			/* Name of instruction. */
   Uint32 mask[3];		/* Signature mask. */
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
