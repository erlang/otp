/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1996-2010. All Rights Reserved.
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

/* FORCE_HEAP_FRAGS:
 * Debug provocation to make HAlloc always create heap fragments (if allowed)
 * even if there is room on heap.
 */
/* #define FORCE_HEAP_FRAGS */


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

/* Scheduler stores data for temporary heaps if
   !HEAP_ON_C_STACK. Macros (*TmpHeap*) in global.h selects if we put temporary
   heap data on the C stack or if we use the buffers in the scheduler data. */
#define TMP_HEAP_SIZE 128            /* Number of Eterm in the schedulers
				        small heap for transient heap data */
#define CMP_TMP_HEAP_SIZE       2    /* cmp wants its own tmp-heap... */
#define ERL_ARITH_TMP_HEAP_SIZE 4    /* as does erl_arith... */
#define BEAM_EMU_TMP_HEAP_SIZE  2    /* and beam_emu... */

/*
 * The new arithmetic operations need some extra X registers in the register array.
 * so does the gc_bif's (i_gc_bif3 need 3 extra).
 */
#define ERTS_X_REGS_ALLOCATED (MAX_REG+3)

#define INPUT_REDUCTIONS (2 * CONTEXT_REDS)

#define H_DEFAULT_SIZE  233        /* default (heap + stack) min size */
#define VH_DEFAULT_SIZE  32768     /* default virtual (bin) heap min size (words) */

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
      	             || ((P)->id == ERTS_INVALID_PID) \
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
#  ifdef CHECK_FOR_HOLES
#    define INIT_HEAP_MEM(p,sz) erts_set_hole_marker(HEAP_TOP(p), (sz))
#  else
#    define INIT_HEAP_MEM(p,sz) memset(HEAP_TOP(p),DEBUG_BAD_BYTE,(sz)*sizeof(Eterm*))
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
#define HAlloc(p, sz)			                              \
    (ASSERT_EXPR((sz) >= 0),					      \
     ErtsHAllocLockCheck(p),					      \
     (IS_FORCE_HEAP_FRAGS || (((HEAP_LIMIT(p) - HEAP_TOP(p)) < (sz))) \
      ? erts_heap_alloc((p),(sz))                                     \
      : (INIT_HEAP_MEM(p,sz),		                              \
         HEAP_TOP(p) = HEAP_TOP(p) + (sz), HEAP_TOP(p) - (sz))))


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
#if HALFWORD_HEAP
# define ERTS_HOLE_MARKER (0xaf5e78ccU)
#else
# define ERTS_HOLE_MARKER (((0xaf5e78ccUL << 24) << 8) | 0xaf5e78ccUL)
#endif
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
extern int BIN_VH_MIN_SIZE;	/* minimum virtual (bin) heap */

extern int erts_atom_table_size;/* Atom table size */

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
