/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1996-2023. All Rights Reserved.
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

/* `valgrind` can't handle stack switching, so we will turn off native
 * stack. */
#ifdef VALGRIND
#undef NATIVE_ERLANG_STACK
#undef ERLANG_FRAME_POINTERS
#endif

/* Frame pointer support costs an extra word per process even when unused, so
 * it's worth disabling for compact builds. */
#ifdef CODE_MODEL_SMALL
#undef ERLANG_FRAME_POINTERS
#endif

#if defined(DEBUG) && !defined(CHECK_FOR_HOLES) && !defined(__WIN32__)
# define CHECK_FOR_HOLES
#endif

#define BEAM 1
#define EMULATOR "BEAM"
#define SEQ_TRACE 1

#define CONTEXT_REDS 4000            /* Swap process out after this number */
#define MAX_ARG      255             /* Max number of arguments allowed */
#define MAX_REG      1024            /* Max number of x(N) registers used */
#define REG_MASK     (MAX_REG - 1)

#define MAX_BIF_ARITY 4 /* Maximum allowed arguments for C-based BIFs. */

/*
 * Guard BIFs and the new trapping length/1 implementation need 3 extra
 * registers in the register array.
 */
#define ERTS_X_REGS_ALLOCATED (MAX_REG+3)

#define H_DEFAULT_SIZE  233        /* default (heap + stack) min size */
#define VH_DEFAULT_SIZE  32768     /* default virtual (bin) heap min size (words) */
#define H_DEFAULT_MAX_SIZE 0       /* default max heap size is off */

typedef enum {
    /* Return address only */
    ERTS_FRAME_LAYOUT_RA,
    /* Frame pointer, return address */
    ERTS_FRAME_LAYOUT_FP_RA
} ErtsFrameLayout;

ERTS_GLB_INLINE
int erts_cp_size(void);

#if defined(BEAMASM) && defined(ERLANG_FRAME_POINTERS)
extern ErtsFrameLayout ERTS_WRITE_UNLIKELY(erts_frame_layout);
#   define CP_SIZE erts_cp_size()
#else
#   define erts_frame_layout ERTS_FRAME_LAYOUT_RA
#   define CP_SIZE 1
#endif

/* In the JIT we're not guaranteed to have allocated a word for the CP when
 * allocating a stack frame (it's still reserved however), as the `call` and
 * `ret` instructions bump the stack pointer for us. Consider the following
 * code:
 *
 *    {call_ext, 1, {extfunc,foo,bar,1}.
 *    {test_heap, 2, 1}.
 *    {put_list, {y,0}, {x,0}, {x,0}}.
 *    {call_ext, 1, {extfunc,bar,qux,1}.
 *
 * Since the CP is not reflected in the stack use, the test_heap instruction
 * will not GC when there's 2 words left on the heap, overwriting the space for
 * the CP and crashing after the call to `bar:qux/1`.
 *
 * To get around this, we maintain a minimum amount (S_RESERVED) of free space
 * on the stack that can be freely used by the JIT or interpreter for whatever
 * purpose. */

#if defined(BEAMASM) && defined(NATIVE_ERLANG_STACK)
#define S_REDZONE (CP_SIZE * 3)
#elif defined(BEAMASM) && defined(__aarch64__)
#define S_REDZONE (CP_SIZE * 3)
#elif defined(DEBUG)
/* Ensure that a redzone won't cause problems in the interpreter. */
#define S_REDZONE CP_SIZE
#else
#define S_REDZONE 0
#endif

#define S_RESERVED (CP_SIZE + S_REDZONE)

#define ErtsHAllocLockCheck(P) \
  ERTS_LC_ASSERT(erts_dbg_check_halloc_lock((P)))


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
Eterm* erts_set_hole_marker(Eterm* ptr, Uint sz);
#    define INIT_HEAP_MEM(p,sz) erts_set_hole_marker(p, (sz))
#  else
#    define INIT_HEAP_MEM(p,sz) sys_memset(p,0x01,(sz)*sizeof(Eterm*))
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
#define HAllocX(p, sz, xtra)                                                \
  (ASSERT((sz) >= 0),                                                       \
     ErtsHAllocLockCheck(p),                                                \
     ((IS_FORCE_HEAP_FRAGS || (!HEAP_START(p) || HeapWordsLeft(p) < (sz)))  \
      ? erts_heap_alloc((p),(sz),(xtra))                                    \
      : (INIT_HEAP_MEM(HEAP_TOP(p), sz),                                    \
         HEAP_TOP(p) = HEAP_TOP(p) + (sz), HEAP_TOP(p) - (sz))))

#define HAlloc(P, SZ) HAllocX(P,SZ,0)

#define HRelease(p, endp, ptr)					\
  if ((ptr) == (endp)) {					\
     ;								\
  } else if (HEAP_START(p) <= (ptr) && (ptr) < HEAP_TOP(p)) {	\
     ASSERT(HEAP_TOP(p) == (endp));                             \
     HEAP_TOP(p) = (ptr);					\
  } else {							\
     ASSERT(MBUF(p)->mem + MBUF(p)->used_size == (endp));       \
     erts_heap_frag_shrink(p, ptr);                             \
  }

#define HeapWordsLeft(p) (HEAP_LIMIT(p) - HEAP_TOP(p))

#if defined(DEBUG) || defined(CHECK_FOR_HOLES)

/*
 * ERTS_HOLE_MARKER must *not* be mistaken for a valid term
 * on the heap...
 */
#  ifdef ARCH_64
#    define ERTS_HOLE_MARKER \
    make_catch(UWORD_CONSTANT(0xdeadbeaf00000000) >> _TAG_IMMED2_SIZE)
/* Will (at the time of writing) appear as 0xdeadbeaf0000001b */
#  else
#    define ERTS_HOLE_MARKER \
    make_catch(UWORD_CONSTANT(0xdead0000) >> _TAG_IMMED2_SIZE)
/* Will (at the time of writing) appear as 0xdead001b */
#  endif
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
 * Always allocate in a heap fragment, never on the heap.
 */
#if defined(VALGRIND)
/* Running under valgrind, allocate exactly as much as needed.*/
#  define HeapFragOnlyAlloc(p, sz)              \
  (ASSERT((sz) >= 0),                           \
   ErtsHAllocLockCheck(p),                      \
   erts_heap_alloc((p),(sz),0))
#else
#  define HeapFragOnlyAlloc(p, sz)              \
  (ASSERT((sz) >= 0),                           \
   ErtsHAllocLockCheck(p),                      \
   erts_heap_alloc((p),(sz),512))
#endif

/*
 * Description for each instruction (defined here because the name and
 * count fields are interesting outside the emulator proper).
 */

typedef struct op_entry {
   char* name;			/* Name of instruction. */
   Uint32 mask[3];		/* Signature mask. */
#ifndef BEAMASM
   unsigned involves_r;		/* Needs special attention when matching. */
   int sz;			/* Number of loaded words. */
   int adjust;                  /* Adjustment for start of instruction. */
   char* pack;			/* Instructions for packing engine. */
#endif
   char* sign;			/* Signature string. */
} OpEntry;

extern const OpEntry opc[];	/* Description of all instructions. */
extern const int num_instructions; /* Number of instruction in opc[]. */

extern Uint erts_instr_count[];

/* some constants for various table sizes etc */

#define ATOM_TEXT_SIZE  32768	/* Increment for allocating atom text space */

#define ITIME 100		/* Number of milliseconds per clock tick    */
#define MAX_PORT_LINK 8		/* Maximum number of links to a port        */

extern int H_MIN_SIZE;		/* minimum (heap + stack) */
extern int BIN_VH_MIN_SIZE;	/* minimum virtual (bin) heap */
extern Uint H_MAX_SIZE;          /* maximum (heap + stack) */
extern int H_MAX_FLAGS;         /* maximum heap flags  */

extern int erts_atom_table_size;/* Atom table size */
extern int erts_pd_initial_size;/* Initial Process dictionary table size */

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

#if defined(NO_JUMP_TABLE) || defined(BEAMASM)
#  define BeamOpsAreInitialized() (1)
#  define BeamOpCodeAddr(OpCode) ((BeamInstr)(OpCode))
#else
extern void** beam_ops;
#  define BeamOpsAreInitialized() (beam_ops != 0)
#  define BeamOpCodeAddr(OpCode) ((BeamInstr)beam_ops[(OpCode)])
#endif

#if defined(ARCH_64) && defined(CODE_MODEL_SMALL) && !defined(BEAMASM)
#  define BeamCodeAddr(InstrWord) ((BeamInstr)(Uint32)(InstrWord))
#  define BeamSetCodeAddr(InstrWord, Addr) (((InstrWord) & ~((1ull << 32)-1)) | (Addr))
#  define BeamExtraData(InstrWord) ((InstrWord) >> 32)
#else
#  define BeamCodeAddr(InstrWord) ((BeamInstr)(InstrWord))
#  define BeamSetCodeAddr(InstrWord, Addr) (Addr)
#endif

#define BeamIsOpCode(InstrWord, OpCode) (BeamCodeAddr(InstrWord) == BeamOpCodeAddr(OpCode))

#ifndef BEAMASM

#define BeamIsReturnCallAccTrace(w) \
    BeamIsOpCode(*(const BeamInstr*)(w), op_i_call_trace_return)
#define BeamIsReturnToTrace(w) \
    BeamIsOpCode(*(const BeamInstr*)(w), op_i_return_to_trace)
#define BeamIsReturnTrace(w) \
    BeamIsOpCode(*(const BeamInstr*)(w), op_return_trace)

#else /* BEAMASM */

#define BeamIsReturnCallAccTrace(w) \
    ((w) == beam_call_trace_return)
#define BeamIsReturnToTrace(w) \
    ((w) == beam_return_to_trace)
#define BeamIsReturnTrace(w) \
    ((w) == beam_return_trace || (w) == beam_exception_trace)

#endif /* BEAMASM */

/* Stack frame sizes (not including CP_SIZE) */
#define BEAM_RETURN_CALL_ACC_TRACE_FRAME_SZ 3
#define BEAM_RETURN_TO_TRACE_FRAME_SZ   1
#define BEAM_RETURN_TRACE_FRAME_SZ      3

#if ERTS_GLB_INLINE_INCL_FUNC_DEF
ERTS_GLB_INLINE
int erts_cp_size(void)
{
    if (erts_frame_layout == ERTS_FRAME_LAYOUT_RA) {
        return 1;
    }

    ASSERT(erts_frame_layout == ERTS_FRAME_LAYOUT_FP_RA);
    return 2;
}
#endif

#endif	/* __ERL_VM_H__ */
